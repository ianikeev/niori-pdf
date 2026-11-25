;;; niori-pdf-images.el --- JPEG and PNG image embedding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev.
;;
;; Original Emacs Lisp implementation of JPEG and PNG image embedding.
;; Similar functionality in CL-PDF is not a claim of code derivation.
;; See LICENSE for the license applicable to this file.

;;; Commentary:

;; JPEG images are passed through as DCTDecode XObjects (dimensions and
;; colorspace parsed from markers).  PNG images are fully decoded in
;; Elisp: chunk parsing, zlib inflation via Emacs built-ins, row
;; un-filtering (None/Sub/Up/Average/Paeth), color types 0/2/3/4/6 at
;; bit depths 1/2/4/8 (16-bit is downsampled to 8-bit), non-interlaced.
;; Alpha (tRNS or RGBA/GA) is embedded as a /SMask image.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-content)

;;;; Reading files as bytes

(defun niori-pdf--file-bytes (path)
  "Return the unibyte contents of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun niori-pdf--detect-type (bytes)
  "Return `jpeg', `png' or nil from magic bytes."
  (cond
   ((and (>= (length bytes) 2) (= (aref bytes 0) 255) (= (aref bytes 1) 216))
    'jpeg)
   ((and (>= (length bytes) 8)
         (= (aref bytes 0) 137) (= (aref bytes 1) 80)
         (= (aref bytes 2) 78) (= (aref bytes 3) 71))
    'png)
   (t nil)))

(defun niori-pdf-image-from-file (spec)
  "Create an `niori-pdf-image' from SPEC.
SPEC is either a file path or a plist (:type jpeg|png :data BYTES)."
  (let* ((bytes (if (stringp spec)
                    (niori-pdf--file-bytes spec)
                  (plist-get spec :data)))
         (type (or (if (stringp spec)
                       (niori-pdf--detect-type bytes)
                     (plist-get spec :type))
                   (niori-pdf--detect-type bytes))))
    (pcase type
      (`jpeg (niori-pdf-make-jpeg-image bytes))
      (`png (niori-pdf-make-png-image bytes))
      (_ (error "niori-pdf: unsupported image format (only JPEG and PNG)")))))

;;;; JPEG

(defun niori-pdf--jpeg-parse (bytes)
  "Parse JPEG markers; return plist (:width :height :components)."
  (unless (and (= (aref bytes 0) 255) (= (aref bytes 1) 216))
    (error "niori-pdf: not a JPEG file"))
  (let ((i 2) (n (length bytes))
        width height comps)
    (while (and (< i (- n 1)) (not comps))
      (when (= (aref bytes i) 255)
        (let* ((marker (aref bytes (1+ i)))
               (len (if (or (= marker 216)
                            (and (>= marker 208) (<= marker 217)))
                        0
                      (+ (* (aref bytes (+ i 2)) 256)
                         (aref bytes (+ i 3))))))
          (cond
           ((and (>= marker 192) (<= marker 207)
                 (/= marker 196) (/= marker 200) (/= marker 204))
            (setq height (+ (* (aref bytes (+ i 5)) 256) (aref bytes (+ i 6)))
                  width (+ (* (aref bytes (+ i 7)) 256) (aref bytes (+ i 8)))
                  comps (aref bytes (+ i 9))))
           ((= marker 218) ; SOS: header scanning ends here
            (setq comps (or comps 3))))
          (setq i (+ i 2 len)))
        (setq i (1+ i))))
    (unless width
      (error "niori-pdf: no SOF frame in JPEG"))
    (list :width width :height height :components comps)))

(defun niori-pdf-make-jpeg-image (bytes)
  "Create an `niori-pdf-image' embedding raw JPEG BYTES (DCTDecode)."
  (let* ((info (niori-pdf--jpeg-parse bytes))
         (comps (plist-get info :components))
         (cs (pcase comps
               (1 'DeviceGray)
               (3 'DeviceRGB)
               (4 'DeviceCMYK)
               (_ (error "niori-pdf: unexpected JPEG component count %d" comps))))
         (dict (niori-pdf-dict-from-alist
                (list (cons 'Type 'XObject)
                      (cons 'Subtype 'Image)
                      (cons 'Width (plist-get info :width))
                      (cons 'Height (plist-get info :height))
                      (cons 'ColorSpace cs)
                      (cons 'BitsPerComponent 8)
                      (cons 'Filter 'DCTDecode))))
         (img (niori-pdf-make-image :dict dict :data bytes
                                    :width (plist-get info :width)
                                    :height (plist-get info :height)
                                    :kind 'jpeg)))
    (when (eq cs 'DeviceCMYK)
      ;; Adobe CMYK JPEGs store inverted values
      (niori-pdf-dict-set dict 'Decode (niori-pdf-array 1 0 1 0 1 0 1 0)))
    img))

;;;; PNG

(defun niori-pdf--png-chunks (bytes)
  "Split PNG BYTES into a list of (TYPE-STRING . DATA) chunks."
  (let ((i 8) (n (length bytes)) (chunks nil))
    (while (< i n)
      (let* ((len (+ (* (aref bytes i) 16777216) (* (aref bytes (+ i 1)) 65536)
                     (* (aref bytes (+ i 2)) 256) (aref bytes (+ i 3))))
             (type (substring bytes (+ i 4) (+ i 8)))
             (data (substring bytes (+ i 8) (+ i 8 len))))
        (push (cons type data) chunks)
        (setq i (+ i 12 len))))
    (nreverse chunks)))

(defun niori-pdf--png-parse (bytes)
  "Parse a PNG; return a plist of its structural fields."
  (let ((chunks (niori-pdf--png-chunks bytes))
        ihdr palette trns idat)
    (dolist (c chunks)
      (pcase (car c)
        ("IHDR" (setq ihdr (cdr c)))
        ("PLTE" (setq palette (cdr c)))
        ("tRNS" (setq trns (cdr c)))
        ("IDAT" (push (cdr c) idat))))
    (unless ihdr
      (error "niori-pdf: PNG without IHDR"))
    (let* ((width (+ (* (aref ihdr 0) 16777216) (* (aref ihdr 1) 65536)
                     (* (aref ihdr 2) 256) (aref ihdr 3)))
           (height (+ (* (aref ihdr 4) 16777216) (* (aref ihdr 5) 65536)
                      (* (aref ihdr 6) 256) (aref ihdr 7)))
           (depth (aref ihdr 8))
           (color-type (aref ihdr 9))
           (interlace (aref ihdr 12)))
      (when (/= interlace 0)
        (error "niori-pdf: interlaced PNG images are not supported"))
      (unless (memq color-type '(0 2 3 4 6))
        (error "niori-pdf: unsupported PNG color type %d" color-type))
      (list :width width :height height :depth depth
            :color-type color-type
            :palette palette :trns trns
            :idat (apply #'concat (nreverse idat))))))

(defconst niori-pdf--png-channels
  '((0 . 1) (2 . 3) (3 . 1) (4 . 2) (6 . 4))
  "PNG color type -> channel count.")

(defun niori-pdf--png-unfilter (raw width height channels bytes-per-sample)
  "Undo PNG row filtering of RAW scanline data.
BYTES-PER-SAMPLE is 1 (8-bit) or 2 (16-bit).
Returns (SAMPLES-STRING ROW-BYTES)."
  (let* ((samples-per-pixel (* channels bytes-per-sample))
         (row-bytes (* width samples-per-pixel))
         (out (make-string (* row-bytes height) 0))
         (prev (make-vector row-bytes 0))
         (p 0))
    (dotimes (yy height)
      (let ((filter (aref raw p))
            (cur (make-vector row-bytes 0)))
        (setq p (1+ p))
        (dotimes (x row-bytes)
          (let* ((b (aref raw (+ p x)))
                 (a (if (>= x samples-per-pixel) (aref cur (- x samples-per-pixel)) 0))
                 (c (aref prev x))
                 (d (if (>= x samples-per-pixel) (aref prev (- x samples-per-pixel)) 0))
                 (v (pcase filter
                      (0 b)
                      (1 (+ b a))
                      (2 (+ b c))
                      (3 (+ b (/ (+ a c) 2)))
                      (4 (let* ((pp (+ a c (- d)))
                                (pa (abs (- pp a)))
                                (pb (abs (- pp c)))
                                (pc (abs (- pp d))))
                           (+ b (cond ((and (<= pa pb) (<= pa pc)) a)
                                      ((<= pb pc) c)
                                      (t d)))))
                      (_ (error "niori-pdf: bad PNG filter %d" filter)))))
            (aset cur x (logand v 255))))
        (dotimes (x row-bytes)
          (aset out (+ (* yy row-bytes) x) (aref cur x)))
        (setq prev cur))
      (setq p (+ p row-bytes)))
    (list out row-bytes)))

(defun niori-pdf--png-unpack-subbyte (data width height channels depth)
  "Expand sub-byte samples of DATA to one byte per sample (scaled to 0-255)."
  (let* ((row-bytes (* channels (ceiling (/ (* width depth) 8.0))))
         (per-byte (/ 8 depth))
         (mask (1- (ash 1 depth)))
         (scale (/ 255 mask))
         (out (make-string (* width height channels) 0)))
    (dotimes (y height)
      (dotimes (x width)
        (dotimes (ch channels)
          (let* ((pos (+ (* x channels) ch))
                 (byte (aref data (+ (* y row-bytes) (/ pos per-byte))))
                 (shift (* (- (1- per-byte) (% pos per-byte)) depth))
                 (v (* (logand (ash byte (- shift)) mask) scale)))
            (aset out (+ (* (+ (* y width) x) channels) ch) v)))))
    out))

(defun niori-pdf--png-16-to-8 (data width height channels)
  "Downscale 16-bit big-endian samples of DATA to 8-bit (high byte)."
  (let* ((row-bytes (* channels width 2))
         (out (make-string (* width height channels) 0)))
    (dotimes (y height)
      (dotimes (x (* channels width))
        (aset out (+ (* y (* channels width)) x)
              (aref data (+ (* y row-bytes) (* 2 x))))))
    out))

(defun niori-pdf-make-png-image (bytes)
  "Decode PNG BYTES into an `niori-pdf-image'."
  (let* ((info (niori-pdf--png-parse bytes))
         (width (plist-get info :width))
         (height (plist-get info :height))
         (depth (plist-get info :depth))
         (color-type (plist-get info :color-type))
         (channels (cdr (assq color-type niori-pdf--png-channels)))
         (raw (plist-get info :idat))
         (inflated (or (niori-pdf-inflate raw)
                       (error "niori-pdf: PNG needs zlib support (Emacs built without zlib)")))
         (un (niori-pdf--png-unfilter inflated width height channels
                                      (if (eq depth 16) 2 1)))
         (data (car un)))
    (when (eq depth 16)
      (setq data (niori-pdf--png-16-to-8 data width height channels)))
    (when (and (memq depth '(1 2 4)) (/= color-type 3))
      (setq data (niori-pdf--png-unpack-subbyte data width height channels depth)))
    (pcase color-type
      (3 (niori-pdf--png-palette-image info data width height depth))
      (0 (niori-pdf--png-gray-image info data width height depth))
      (2 (niori-pdf--png-rgb-image data width height))
      (4 (niori-pdf--png-gray-alpha-image data width height))
      (6 (niori-pdf--png-rgba-image data width height)))))

(defun niori-pdf--png-palette-image (info data width height depth)
  "Build an /Indexed image (alpha from tRNS applied at save time)."
  (let* ((palette (plist-get info :palette))
         (trns (plist-get info :trns))
         (n-entries (/ (length palette) 3))
         (dict (niori-pdf-dict-from-alist
                (list (cons 'Type 'XObject)
                      (cons 'Subtype 'Image)
                      (cons 'Width width)
                      (cons 'Height height)
                      (cons 'ColorSpace
                            (niori-pdf-array "Indexed" 'DeviceRGB
                                             (1- n-entries) nil))
                      (cons 'BitsPerComponent depth)
                      (cons 'Filter 'FlateDecode))))
         (img (niori-pdf-make-image :dict dict :data data
                                    :width width :height height :kind 'png)))
    (setf (niori-pdf-image-props img)
          (list :indexed-palette palette :trns trns :depth depth))
    img))

(defun niori-pdf--png-gray-image (info data width height _depth)
  "Build a DeviceGray image (tRNS key color -> SMask)."
  (let* ((trns (plist-get info :trns))
         (dict (niori-pdf-dict-from-alist
                (list (cons 'Type 'XObject)
                      (cons 'Subtype 'Image)
                      (cons 'Width width)
                      (cons 'Height height)
                      (cons 'ColorSpace 'DeviceGray)
                      (cons 'BitsPerComponent 8)
                      (cons 'Filter 'FlateDecode))))
         (img (niori-pdf-make-image :dict dict :data data
                                    :width width :height height :kind 'png)))
    (when trns
      (let* ((v (if (> (length trns) 1)
                    (+ (* (aref trns 0) 256) (aref trns 1))
                  (aref trns 0)))
             (n (length data))
             (alpha (make-string n 0)))
        (dotimes (i n)
          (aset alpha i (if (= (aref data i) v) 0 255)))
        (setf (niori-pdf-image-props img) (list :smask-gray alpha))))
    img))

(defun niori-pdf--png-rgb-image (data width height)
  (let ((dict (niori-pdf-dict-from-alist
               (list (cons 'Type 'XObject)
                     (cons 'Subtype 'Image)
                     (cons 'Width width)
                     (cons 'Height height)
                     (cons 'ColorSpace 'DeviceRGB)
                     (cons 'BitsPerComponent 8)
                     (cons 'Filter 'FlateDecode)))))
    (niori-pdf-make-image :dict dict :data data
                          :width width :height height :kind 'png)))

(defun niori-pdf--png-gray-alpha-image (data width height)
  (let* ((n (* width height))
         (g (make-string n 0))
         (a (make-string n 0)))
    (dotimes (i n)
      (aset g i (aref data (* 2 i)))
      (aset a i (aref data (+ 1 (* 2 i)))))
    (let* ((dict (niori-pdf-dict-from-alist
                  (list (cons 'Type 'XObject)
                        (cons 'Subtype 'Image)
                        (cons 'Width width)
                        (cons 'Height height)
                        (cons 'ColorSpace 'DeviceGray)
                        (cons 'BitsPerComponent 8)
                        (cons 'Filter 'FlateDecode))))
           (img (niori-pdf-make-image :dict dict :data g
                                      :width width :height height :kind 'png)))
      (setf (niori-pdf-image-props img) (list :smask-gray a))
      img)))

(defun niori-pdf--png-rgba-image (data width height)
  (let* ((n (* width height))
         (rgb (make-string (* 3 n) 0))
         (a (make-string n 0)))
    (dotimes (i n)
      (aset rgb (* 3 i) (aref data (* 4 i)))
      (aset rgb (+ 1 (* 3 i)) (aref data (+ 1 (* 4 i))))
      (aset rgb (+ 2 (* 3 i)) (aref data (+ 2 (* 4 i))))
      (aset a i (aref data (+ 3 (* 4 i)))))
    (let* ((dict (niori-pdf-dict-from-alist
                  (list (cons 'Type 'XObject)
                        (cons 'Subtype 'Image)
                        (cons 'Width width)
                        (cons 'Height height)
                        (cons 'ColorSpace 'DeviceRGB)
                        (cons 'BitsPerComponent 8)
                        (cons 'Filter 'FlateDecode))))
           (img (niori-pdf-make-image :dict dict :data rgb
                                      :width width :height height :kind 'png)))
      (setf (niori-pdf-image-props img) (list :smask-gray a))
      img)))

;;;; At save time: finalize image XObjects (SMask, Indexed lookups)

(defun niori-pdf-images--finalize (doc)
  "Save hook: attach image stream objects, SMask and Indexed refs for DOC."
  (dolist (kv (niori-pdf-document-images doc))
    (let* ((image (cdr kv))
           (props (niori-pdf-image-props image))
           (dict (niori-pdf-image-dict image)))
      (unless (niori-pdf-image-ref image)
        (let* ((palette (plist-get props :indexed-palette))
               (trns (plist-get props :trns))
               (data (niori-pdf-image-data image))
               (smask-ref
                ;; priority 1: explicit gray alpha from RGBA/GA decoding
                (or (when (plist-get props :smask-gray)
                      (niori-pdf--emit-smask doc image
                                             (plist-get props :smask-gray)))
                    ;; priority 2: palette tRNS -> per-pixel alpha
                    (when (and palette trns)
                      (let* ((w (niori-pdf-image-width image))
                             (h (niori-pdf-image-height image))
                             (n (* w h))
                             (depth (or (plist-get props :depth) 8))
                             (per-byte (/ 8 depth))
                             (mask (1- (ash 1 depth)))
                             (row-bytes (ceiling (/ (* w depth) 8.0)))
                             (alpha (make-string n 0)))
                        (dotimes (i n)
                          (let* ((byte (aref data (+ (* (/ i w) row-bytes)
                                                     (/ i per-byte))))
                                 (shift (* (- (1- per-byte) (% i per-byte)) depth))
                                 (idx (logand (ash byte (- shift)) mask))
                                 (ta (if (< idx (length trns))
                                         (aref trns idx)
                                       255)))
                            (aset alpha i ta)))
                        (niori-pdf--emit-smask doc image alpha))))))
          ;; palette image: replace nil Lookup with the palette stream ref
          (when palette
            (let* ((cs (niori-pdf-dict-get dict 'ColorSpace))
                   (elems (niori-pdf-array-elements cs))
                   (lookup-ref (niori-pdf--palette-object doc palette)))
              (setf (niori-pdf-array-elements cs)
                    (list "Indexed" 'DeviceRGB (nth 2 elems) lookup-ref))))
          ;; main image stream
          (let ((cc (niori-pdf-compress-stream-data doc data)))
            (niori-pdf-dict-set dict 'Length (length (car cc)))
            (when (cdr cc) (niori-pdf-dict-set dict 'Filter (cdr cc)))
            (when smask-ref (niori-pdf-dict-set dict 'SMask smask-ref))
            (setf (niori-pdf-image-ref image)
                  (niori-pdf-add-object
                   doc (niori-pdf-stream :dict dict :data (car cc))))))))))

(defun niori-pdf--emit-smask (doc image gray)
  "Add a DeviceGray SMask object for IMAGE with GRAY alpha bytes."
  (let* ((sd (niori-pdf-dict-from-alist
              (list (cons 'Type 'XObject)
                    (cons 'Subtype 'Image)
                    (cons 'Width (niori-pdf-image-width image))
                    (cons 'Height (niori-pdf-image-height image))
                    (cons 'ColorSpace 'DeviceGray)
                    (cons 'BitsPerComponent 8))))
         (cc (niori-pdf-compress-stream-data doc gray)))
    (niori-pdf-dict-set sd 'Length (length (car cc)))
    (when (cdr cc) (niori-pdf-dict-set sd 'Filter (cdr cc)))
    (niori-pdf-add-object doc (niori-pdf-stream :dict sd :data (car cc)))))

(defun niori-pdf--palette-object (doc palette)
  "Add the Indexed color lookup stream for PALETTE bytes to DOC."
  (let* ((dict (niori-pdf-dict-from-alist
                (list (cons 'Length (length palette)))))
         (stream (niori-pdf-stream :dict dict :data palette)))
    (niori-pdf-add-object doc stream)))

(provide 'niori-pdf-images)
;;; niori-pdf-images.el ends here
