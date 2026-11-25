;;; niori-pdf-ttf.el --- TrueType font loading, subsetting, embedding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev.
;;
;; Original Emacs Lisp implementation of TrueType/OpenType font
;; loading, subsetting and embedding.  See LICENSE for the license
;; applicable to this file.

;;; Commentary:

;; Loads TrueType (.ttf) font files, builds glyph subsets and embeds them
;; as Type0 / CIDFontType2 fonts with Identity-H encoding so that any
;; Unicode text can be drawn.  A ToUnicode CMap keeps text extraction
;; working.  OpenType/CFF (.otf) fonts are subsetted at the CFF level
;; (see the "CFF (Type 1C) subsetting" section at the end) and embedded
;; via FontFile3 with the OpenType subtype.  TrueType collections (.ttc)
;; are parsed at the member-font offset given by an index (default 0);
;; use `niori-pdf-ttf-collection-fonts' to list the members.
;; Type 1 fonts (.pfb/.pfa) are handled by niori-pdf-type1.el.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)

;;;; Low-level big-endian readers over unibyte strings

(defun niori-pdf-ttf--u8 (b i) (aref b i))

(defun niori-pdf-ttf--u16 (b i)
  (+ (* (aref b i) 256) (aref b (1+ i))))

(defun niori-pdf-ttf--u32 (b i)
  (+ (* (aref b i) 16777216) (* (aref b (1+ i)) 65536)
     (* (aref b (+ i 2)) 256) (aref b (+ i 3))))

(defun niori-pdf-ttf--s16 (b i)
  (let ((v (niori-pdf-ttf--u16 b i)))
    (if (>= v 32768) (- v 65536) v)))

(defun niori-pdf-ttf--s32 (b i)
  (let ((v (niori-pdf-ttf--u32 b i)))
    (if (>= v 2147483648) (- v 4294967296) v)))

(defun niori-pdf-ttf--tag (b i)
  (substring b i (+ i 4)))

;;;; Font file structure

(cl-defstruct niori-pdf-ttf-file
  bytes num-glyphs units-per-em index-to-loc-format
  cmap widths glyf-off glyf-len loca
  ps-name italic-angle bbox ascender descender
  cap-height weight-class fixed-pitch
  cff-p base tables)

(defun niori-pdf-ttf-collection-p (path)
  "Return t when the font file at PATH is a TrueType collection (.ttc)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 4)
    (= (niori-pdf-ttf--u32 (buffer-string) 0) #x74746366)))

(defun niori-pdf-ttf-collection-fonts (path)
  "List the fonts of the TrueType collection at PATH.
Returns a list of (INDEX PS-NAME FAMILY); INDEX is the 0-based
member index to pass to `niori-pdf-ttf-read-file' or a
\(:file PATH :index INDEX) font spec."
  (let* ((bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally path)
                  (buffer-string))))
    (unless (= (niori-pdf-ttf--u32 bytes 0) #x74746366)
      (error "niori-pdf: not a TrueType collection: %s" path))
    (let ((n (niori-pdf-ttf--u32 bytes 8))
          (out nil))
      (dotimes (idx n)
        (let* ((base (niori-pdf-ttf--u32 bytes (+ 12 (* 4 idx))))
               (num-tables (niori-pdf-ttf--u16 bytes (+ base 4)))
               (namet nil))
          (dotimes (i num-tables)
            (let* ((rec (+ base 12 (* i 16)))
                   (tag (niori-pdf-ttf--tag bytes rec)))
              (when (string= tag "name")
                (setq namet
                      (list (niori-pdf-ttf--u32 bytes (+ rec 8))
                            (niori-pdf-ttf--u32 bytes (+ rec 12)))))))
          (push (list idx
                      (niori-pdf-ttf--name-for-id bytes namet 6)
                      (niori-pdf-ttf--name-for-id bytes namet 1))
                out)))
      (nreverse out))))

(defun niori-pdf-ttf--collection-base (bytes path index)
  "Return the sfnt base offset of member INDEX of collection BYTES.
PATH is only used for error messages.  A plain .ttf/.otf has a base
of 0; a .ttc TrueType collection stores one sfnt offset per member
after the \"ttcf\" header."
  (let ((magic (niori-pdf-ttf--u32 bytes 0)))
    (cond
     ((= magic #x74746366) ; "ttcf"
      (let* ((n (niori-pdf-ttf--u32 bytes 8))
             (idx (or index 0)))
        (when (>= idx n)
          (error "niori-pdf: collection %s has %d font%s (index %d out of range)"
                 path n (if (= n 1) "" "s") idx))
        (niori-pdf-ttf--u32 bytes (+ 12 (* 4 idx)))))
     ((and (/= magic #x00010000) (/= magic #x74727565)
           (/= magic #x4f54544f)) ; not TTF/"true"/OTTO
      (error "niori-pdf: not a TrueType/OpenType font: %s" path))
     (t
      (when (and index (/= index 0))
        (error "niori-pdf: %s is not a collection, a font index is meaningless"
               path))
      0))))

(defun niori-pdf-ttf-read-file (path &optional index)
  "Read and parse the TrueType/OpenType font at PATH.
For a TrueType collection (.ttc), INDEX selects the member font
(0-based, default 0); use `niori-pdf-ttf-collection-fonts' to see
the members."
  (let* ((bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally path)
                  (buffer-string)))
         (base (niori-pdf-ttf--collection-base bytes path index))
         (magic (niori-pdf-ttf--u32 bytes base)))
    (unless (or (= magic #x00010000) (= magic #x74727565)
                (= magic #x4f54544f))
      (error "niori-pdf: not a TrueType/OpenType font: %s" path))
    (let* ((num-tables (niori-pdf-ttf--u16 bytes (+ base 4)))
           (tables (make-hash-table :test #'equal))
           (i 0))
      (while (< i num-tables)
        (let* ((rec (+ base 12 (* i 16)))
               (tag (niori-pdf-ttf--tag bytes rec))
               (off (niori-pdf-ttf--u32 bytes (+ rec 8)))
               (len (niori-pdf-ttf--u32 bytes (+ rec 12))))
          (puthash tag (list off len) tables))
        (setq i (1+ i)))
      (let* ((head (gethash "head" tables))
             (hhea (gethash "hhea" tables))
             (maxp (gethash "maxp" tables))
             (cmap (gethash "cmap" tables))
             (hmtx (gethash "hmtx" tables))
             (locat (gethash "loca" tables))
             (glyf (gethash "glyf" tables))
             (namet (gethash "name" tables))
             (postt (gethash "post" tables))
             (os2 (gethash "OS/2" tables))
             (cffp (gethash "CFF " tables)))
        (if cffp
            ;; OpenType/CFF font: same sfnt metadata tables, outlines in
            ;; the CFF table; glyf/loca absent
            (unless (and head hhea maxp cmap hmtx)
              (error "niori-pdf: font %s lacks required tables" path))
          (unless (and head hhea maxp cmap hmtx locat glyf)
            (error "niori-pdf: font %s lacks required tables" path)))
        (let* ((head-off (car head))
               (upm (niori-pdf-ttf--u16 bytes (+ head-off 18)))
               (locfmt (niori-pdf-ttf--s16 bytes (+ head-off 50)))
               (bbox (list (niori-pdf-ttf--s16 bytes (+ head-off 36))
                           (niori-pdf-ttf--s16 bytes (+ head-off 38))
                           (niori-pdf-ttf--s16 bytes (+ head-off 40))
                           (niori-pdf-ttf--s16 bytes (+ head-off 42))))
               (maxp-off (car maxp))
               (num-glyphs (niori-pdf-ttf--u16 bytes (+ maxp-off 4)))
               (hhea-off (car hhea))
               (ascender (niori-pdf-ttf--s16 bytes (+ hhea-off 4)))
               (descender (niori-pdf-ttf--s16 bytes (+ hhea-off 6)))
               (num-h (niori-pdf-ttf--u16 bytes (+ hhea-off 34)))
               (hmtx-off (car hmtx))
               (widths (make-vector num-glyphs 0)))
          ;; advance widths (monotone table with a shared last entry)
          (dotimes (g num-glyphs)
            (aset widths g
                  (if (< g num-h)
                      (niori-pdf-ttf--u16 bytes (+ hmtx-off (* 4 g)))
                    (niori-pdf-ttf--u16 bytes
                                        (+ hmtx-off (* 4 (1- num-h)))))))
          ;; loca (TrueType outlines only; nil for OpenType/CFF)
          (let* ((loca-off (and locat (car locat)))
                 (loca (and loca-off
                            (let ((v (make-vector (1+ num-glyphs) 0)))
                              (dotimes (k (1+ num-glyphs))
                                (aset v k
                                      (if (= locfmt 0)
                                          (* 2 (niori-pdf-ttf--u16
                                                bytes (+ loca-off (* 2 k))))
                                        (niori-pdf-ttf--u32
                                         bytes (+ loca-off (* 4 k))))))
                              v))))
            (let ((parsed-cmap (niori-pdf-ttf--parse-cmap
                                bytes cmap num-glyphs)))
            (make-niori-pdf-ttf-file
             :bytes bytes :num-glyphs num-glyphs :units-per-em upm
             :index-to-loc-format locfmt
             :cmap parsed-cmap
             :widths widths
             :glyf-off (and glyf (car glyf))
             :glyf-len (and glyf (car (cdr glyf)))
             :loca loca
             :ps-name (niori-pdf-ttf--ps-name bytes namet path)
             :italic-angle (if postt
                               (/ (niori-pdf-ttf--s32 bytes (+ (car postt) 4))
                                  65536.0)
                             0.0)
             :bbox bbox
             :ascender ascender :descender descender
             :cap-height (if cffp
                             (niori-pdf-ttf--cap-height-os2 bytes os2)
                           (niori-pdf-ttf--cap-height
                            bytes tables parsed-cmap loca glyf))
             :weight-class (if os2
                               (niori-pdf-ttf--u16 bytes (+ (car os2) 4))
                             400)
             :fixed-pitch (and postt
                               (/= 0 (niori-pdf-ttf--u32
                                      bytes (+ (car postt) 12))))
             :cff-p (and cffp t)
             :base base
             :tables tables))))))))

(defun niori-pdf-ttf--parse-cmap (bytes cmap &optional num-glyphs)
  "Parse the cmap table; return a hash codepoint -> gid.
When NUM-GLYPHS is given, entries pointing beyond the last glyph are
dropped (defensive against unusual segments)."
  (let* ((off (car cmap))
         (n (niori-pdf-ttf--u16 bytes (+ off 2)))
         (best -1)
         (best-off nil))
    ;; pick the best available subtable
    (dotimes (k n)
      (let* ((rec (+ off 4 (* k 8)))
             (pid (niori-pdf-ttf--u16 bytes rec))
             (eid (niori-pdf-ttf--u16 bytes (+ rec 2)))
             (soff (niori-pdf-ttf--u32 bytes (+ rec 4)))
             (score (cond
                     ((and (= pid 3) (= eid 10)) 100)
                     ((and (= pid 0) (= eid 4)) 90)
                     ((and (= pid 0) (= eid 6)) 88)
                     ((and (= pid 0) (= eid 3)) 85)
                     ((and (= pid 3) (= eid 1)) 80)
                     ((and (= pid 0) (= eid 2)) 75)
                     ((and (= pid 0) (= eid 1)) 70)
                     ((and (= pid 0) (= eid 0)) 60)
                     ((and (= pid 3) (= eid 0)) 50)
                     ((and (= pid 1) (= eid 0)) 30)
                     (t 1))))
        (when (> score best)
          (setq best score best-off (+ off soff)))))
    (unless best-off
      (error "niori-pdf: no usable cmap subtable"))
    (let ((fmt (niori-pdf-ttf--u16 bytes best-off))
          (h (make-hash-table :test #'eql)))
      (cond
       ;; format 0: single-byte Macintosh table
       ((= fmt 0)
        (dotimes (c 256)
          (let ((g (aref bytes (+ best-off 6 c))))
            (when (> g 0) (puthash c g h)))))
       ;; format 4: BMP two-level segments
       ((= fmt 4)
        (let* ((segx2 (niori-pdf-ttf--u16 bytes (+ best-off 6)))
               (segc (/ segx2 2))
               (end-off (+ best-off 14))
               (start-off (+ end-off segx2 2)) ; +2 for reservedPad
               (delta-off (+ start-off segx2))
               (range-off (+ delta-off segx2)))
          (dotimes (s segc)
            (let ((end (niori-pdf-ttf--u16 bytes (+ end-off (* 2 s))))
                  (start (niori-pdf-ttf--u16 bytes (+ start-off (* 2 s))))
                  (delta (niori-pdf-ttf--s16 bytes (+ delta-off (* 2 s))))
                  (ro (niori-pdf-ttf--u16 bytes (+ range-off (* 2 s)))))
              (unless (= start 65535)
                (if (= ro 0)
                    (let ((c start))
                      (while (<= c end)
                        (let ((g (logand (+ c delta) 65535)))
                          (when (> g 0) (puthash c g h)))
                        (setq c (1+ c))))
                  (let ((c start))
                    (while (<= c end)
                      (let* ((idx (+ range-off (* 2 s) ro
                                     (* 2 (- c start))))
                             (g (if (<= (+ idx 1) (1- (length bytes)))
                                    (niori-pdf-ttf--u16 bytes idx)
                                  0)))
                        (when (> g 0) (puthash c g h)))
                      (setq c (1+ c))))))))))
       ;; format 6: trimmed contiguous table
       ((= fmt 6)
        (let ((first (niori-pdf-ttf--u16 bytes (+ best-off 6)))
              (count (niori-pdf-ttf--u16 bytes (+ best-off 8))))
          (dotimes (j count)
            (let ((g (niori-pdf-ttf--u16 bytes (+ best-off 10 (* 2 j)))))
              (when (> g 0) (puthash (+ first j) g h))))))
       ;; format 12: full 32-bit segments
       ((= fmt 12)
        (let ((ng (niori-pdf-ttf--u32 bytes (+ best-off 12))))
          (dotimes (g ng)
            (let* ((rec (+ best-off 16 (* g 12)))
                   (start (niori-pdf-ttf--u32 bytes rec))
                   (end (niori-pdf-ttf--u32 bytes (+ rec 4)))
                   (sgid (niori-pdf-ttf--u32 bytes (+ rec 8))))
              (when (<= (- end start) 65535)
                (let ((c start))
                  (while (<= c end)
                    (puthash c (logand (+ sgid (- c start)) 65535) h)
                    (setq c (1+ c)))))))))
       (t
        (error "niori-pdf: unsupported cmap format %d" fmt)))
      (when num-glyphs
        (maphash (lambda (c g)
                   (when (>= g num-glyphs)
                     (remhash c h)))
                 h))
      h)))

(defun niori-pdf-ttf--name-for-id (bytes namet nid)
  "Extract name record NID from the name table NAMET, or nil."
  (when namet
    (let* ((off (car namet))
           (count (niori-pdf-ttf--u16 bytes (+ off 2)))
           (stroff (niori-pdf-ttf--u16 bytes (+ off 4)))
           result)
      (dotimes (i count)
        (when (not result)
          (let* ((rec (+ off 6 (* i 12)))
                 (pid (niori-pdf-ttf--u16 bytes rec))
                 (id (niori-pdf-ttf--u16 bytes (+ rec 6)))
                 (len (niori-pdf-ttf--u16 bytes (+ rec 8)))
                 (soff (niori-pdf-ttf--u16 bytes (+ rec 10))))
            (when (and (= id nid) (> len 0))
              (let* ((raw (substring bytes (+ off stroff soff)
                                     (+ off stroff soff len)))
                     (txt (if (= pid 3)
                              (decode-coding-string raw 'utf-16be)
                            (decode-coding-string raw 'mac-roman))))
                (setq result txt))))))
      result)))

(defun niori-pdf-ttf--ps-name (bytes namet path)
  "Extract the PostScript name (nameID 6) from the name table."
  (or (niori-pdf-ttf--name-for-id bytes namet 6)
      (file-name-nondirectory path)))

(defun niori-pdf-ttf--cap-height-os2 (bytes os2)
  "Cap height for OpenType/CFF fonts: OS/2 sCapHeight or a default."
  (if (and os2 (>= (niori-pdf-ttf--u16 bytes (car os2)) 2))
      (niori-pdf-ttf--s16 bytes (+ (car os2) 88))
    700))

(defun niori-pdf-ttf--cap-height (bytes tables cmap loca glyf)
  "Cap height from OS/2 sCapHeight (v2+) or the yMax of glyph H."
  (let ((os2 (gethash "OS/2" tables)))
    (if (and os2 (>= (niori-pdf-ttf--u16 bytes (car os2)) 2))
        (niori-pdf-ttf--s16 bytes (+ (car os2) 88))
      (let ((h (gethash 72 cmap 0)))
        (if (> h 0)
            (let* ((start (aref loca h))
                   (end (aref loca (1+ h))))
              (if (> end start)
                  (niori-pdf-ttf--s16 bytes (+ (car glyf) start 8))
                700))
          700)))))

(defun niori-pdf-ttf-glyph-bytes (ff gid)
  "Return the raw glyf bytes of GID in font file FF (nil if empty)."
  (let ((start (aref (niori-pdf-ttf-file-loca ff) gid))
        (end (aref (niori-pdf-ttf-file-loca ff) (1+ gid))))
    (when (> end start)
      (substring (niori-pdf-ttf-file-bytes ff)
                 (+ (niori-pdf-ttf-file-glyf-off ff) start)
                 (+ (niori-pdf-ttf-file-glyf-off ff) end)))))

(defun niori-pdf-ttf--component-gids (gb)
  "Return the referenced glyph ids of composite glyph bytes GB."
  (let ((gids nil)
        (i 10)                          ; after the glyph header
        (more t))
    (while (and more (< (+ i 4) (length gb)))
      (let* ((flags (niori-pdf-ttf--u16 gb i))
             (gidx (niori-pdf-ttf--u16 gb (+ i 2))))
        (push gidx gids)
        (setq i (+ i 4))
        (if (/= 0 (logand flags 1))     ; ARG_1_AND_2_ARE_WORDS
            (setq i (+ i 4))            ; two s16 args
          (setq i (+ i 2)))             ; two s8 args
        (cond
         ((/= 0 (logand flags 128)) (setq i (+ i 8)))  ; TWO_BY_TWO
         ((/= 0 (logand flags 64)) (setq i (+ i 4)))   ; X_AND_Y_SCALE
         ((/= 0 (logand flags 8)) (setq i (+ i 2))))   ; WE_HAVE_A_SCALE
        (when (= 0 (logand flags 32))   ; MORE_COMPONENTS
          (setq more nil))))
    (nreverse gids)))

(defun niori-pdf-ttf--glyph-references (ff gid)
  "All glyph ids transitively referenced by GID in FF (composite closure)."
  (let ((seen (make-hash-table :test #'eql))
        (stack (list gid))
        (out nil))
    (while stack
      (let ((g (pop stack)))
        (unless (gethash g seen)
          (puthash g t seen)
          (push g out)
          (let ((gb (niori-pdf-ttf-glyph-bytes ff g)))
            (when (and gb (>= (length gb) 2)
                       (< (niori-pdf-ttf--s16 gb 0) 0)) ; composite glyph
              (dolist (c (niori-pdf-ttf--component-gids gb))
                (when (and (>= c 0)
                           (< c (niori-pdf-ttf-file-num-glyphs ff))
                           (not (gethash c seen)))
                  (push c stack))))))))
    (nreverse out)))

;;;; Document font wrapper and metric accessors

(declare-function niori-pdf--register-font-resource "niori-pdf-fonts" (doc font))

(defun niori-pdf-ttf--font-props (ff path)
  "Metric plist for a parsed TrueType/OpenType font file FF from PATH.
Shared by `niori-pdf-ttf-make-font' (which registers the font in a
document) and the document-less measurement fonts."
  (let ((upm (niori-pdf-ttf-file-units-per-em ff)))
    (list :ttf ff
          :file-path (expand-file-name path)
          :otf (niori-pdf-ttf-file-cff-p ff)
          :units-per-em upm
          :ascender (/ (* (niori-pdf-ttf-file-ascender ff) 1000.0) upm)
          :descender (/ (* (niori-pdf-ttf-file-descender ff) 1000.0) upm)
          :cap-height (/ (* (niori-pdf-ttf-file-cap-height ff) 1000.0)
                         upm)
          :used-gids (make-hash-table :test #'eql)
          :used-uni (make-hash-table :test #'eql))))

(defun niori-pdf-ttf--font-props (ff path &optional index)
  "Metric plist for a parsed TrueType/OpenType font file FF from PATH.
Shared by `niori-pdf-ttf-make-font' (which registers the font in a
document) and the document-less measurement fonts.  For a TrueType
collection member, INDEX is the member index recorded in the props."
  (let ((upm (niori-pdf-ttf-file-units-per-em ff)))
    (list :ttf ff
          :file-path (expand-file-name path)
          :ttc-index index
          :otf (niori-pdf-ttf-file-cff-p ff)
          :units-per-em upm
          :ascender (/ (* (niori-pdf-ttf-file-ascender ff) 1000.0) upm)
          :descender (/ (* (niori-pdf-ttf-file-descender ff) 1000.0) upm)
          :cap-height (/ (* (niori-pdf-ttf-file-cap-height ff) 1000.0)
                         upm)
          :used-gids (make-hash-table :test #'eql)
          :used-uni (make-hash-table :test #'eql))))

(defun niori-pdf-ttf-make-font (doc path &optional index)
  "Load a TrueType, OpenType/CFF or collection font PATH and register it in DOC.
INDEX selects the member of a TrueType collection (.ttc; default 0).
Returns a `niori-pdf-font' of kind `ttf' ready for drawing."
  (let* ((ff (niori-pdf-ttf-read-file (expand-file-name path) index))
         (font (niori-pdf-make-font :kind 'ttf :name path :doc doc)))
    (setf (niori-pdf-font-props font)
          (niori-pdf-ttf--font-props ff path index))
    (niori-pdf--register-font-resource doc font)
    (niori-pdf-ttf--register doc font)
    font))

(defun niori-pdf-ttf-encode (string font)
  "Encode STRING to a glyph id list using FONT's cmap; marks usage."
  (let* ((props (niori-pdf-font-props font))
         (ff (plist-get props :ttf))
         (cmap (niori-pdf-ttf-file-cmap ff))
         (used (plist-get props :used-gids))
         (used-uni (plist-get props :used-uni))
         (gids nil))
    (dotimes (i (length string))
      (let* ((c (aref string i))
             (g (gethash c cmap 0)))
        (puthash g t used)
        (unless (gethash g used-uni)
          (puthash g c used-uni))
        (push g gids)))
    (nreverse gids)))

(defun niori-pdf-ttf-encoded-width (gids font)
  "Width in units/1000 em of glyph id list GIDS drawn with FONT."
  (let* ((props (niori-pdf-font-props font))
         (ff (plist-get props :ttf))
         (upm (niori-pdf-ttf-file-units-per-em ff))
         (widths (niori-pdf-ttf-file-widths ff))
         (sum 0))
    (dolist (g gids)
      (setq sum (+ sum (aref widths g))))
    (/ (* sum 1000.0) upm)))

(defun niori-pdf-ttf-ascender (props)
  (plist-get props :ascender))

(defun niori-pdf-ttf-descender (props)
  (plist-get props :descender))

(defun niori-pdf-ttf-cap-height (props)
  (plist-get props :cap-height))

;;;; PDF object registration

(defun niori-pdf-ttf--register (doc font)
  "Create the Type0/CIDFontType2 object graph for FONT in DOC."
  (let* ((props (niori-pdf-font-props font))
         (ff (plist-get props :ttf))
         (upm (niori-pdf-ttf-file-units-per-em ff))
         (ps-name (niori-pdf-ttf-file-ps-name ff))
         (scale (lambda (v) (/ (* v 1000.0) upm)))
         (bbox (mapcar scale (niori-pdf-ttf-file-bbox ff)))
         (weight (niori-pdf-ttf-file-weight-class ff))
         (stemv (+ 10 (* 220 (/ (- weight 100) 900.0))))
         (flags (if (/= (niori-pdf-ttf-file-italic-angle ff) 0) 96 32))
         ;; FontFile stream: FontFile2 (TrueType subset) or FontFile3
         ;; with the OpenType subtype carrying the subsetted CFF table
         (otf-p (plist-get props :otf))
         (ff-dict (niori-pdf-dict))
         (ff-stream (niori-pdf-stream :dict ff-dict :data ""))
         (ff-ref (niori-pdf-add-object doc ff-stream))
         ;; FontDescriptor
         (fd (niori-pdf-dict))
         (fd-ref (niori-pdf-add-object doc fd))
         ;; CIDFontType2 (TrueType) or CIDFontType0 (CFF)
         (cid (niori-pdf-dict))
         (cid-ref (niori-pdf-add-object doc cid))
         ;; ToUnicode CMap stream (filled at save time)
         (tu-dict (niori-pdf-dict))
         (tu (niori-pdf-stream :dict tu-dict :data ""))
         (tu-ref (niori-pdf-add-object doc tu))
         ;; Type0
         (t0 (niori-pdf-dict))
         (ps-sym (intern ps-name)))
    (niori-pdf-dict-set ff-dict 'Length 0)
    (if otf-p
        (niori-pdf-dict-set ff-dict 'Subtype 'OpenType)
      (niori-pdf-dict-set ff-dict 'Length1 0))
    (niori-pdf-dict-set fd 'Type 'FontDescriptor)
    (niori-pdf-dict-set fd 'FontName ps-sym)
    (niori-pdf-dict-set fd 'Flags flags)
    (niori-pdf-dict-set fd 'FontBBox (apply #'niori-pdf-array bbox))
    (niori-pdf-dict-set fd 'ItalicAngle (niori-pdf-ttf-file-italic-angle ff))
    (niori-pdf-dict-set fd 'Ascent (funcall scale
                                            (niori-pdf-ttf-file-ascender ff)))
    (niori-pdf-dict-set fd 'Descent
                        (funcall scale (niori-pdf-ttf-file-descender ff)))
    (niori-pdf-dict-set fd 'CapHeight
                        (funcall scale (niori-pdf-ttf-file-cap-height ff)))
    (niori-pdf-dict-set fd 'StemV stemv)
    (if otf-p
        (niori-pdf-dict-set fd 'FontFile3 ff-ref)
      (niori-pdf-dict-set fd 'FontFile2 ff-ref))
    (niori-pdf-dict-set
     cid 'CIDSystemInfo
     (niori-pdf-dict-from-alist
      (list (cons 'Registry "Adobe") (cons 'Ordering "Identity")
            (cons 'Supplement 0))))
    (niori-pdf-dict-set cid 'Type 'Font)
    (niori-pdf-dict-set cid 'Subtype (if otf-p 'CIDFontType0 'CIDFontType2))
    (niori-pdf-dict-set cid 'BaseFont ps-sym)
    (niori-pdf-dict-set cid 'FontDescriptor fd-ref)
    (niori-pdf-dict-set cid 'CIDToGIDMap 'Identity)
    (niori-pdf-dict-set tu-dict 'Length 0)
    (niori-pdf-dict-set t0 'Type 'Font)
    (niori-pdf-dict-set t0 'Subtype 'Type0)
    (niori-pdf-dict-set t0 'BaseFont ps-sym)
    (niori-pdf-dict-set t0 'Encoding 'Identity-H)
    (niori-pdf-dict-set t0 'DescendantFonts (niori-pdf-array cid-ref))
    (niori-pdf-dict-set t0 'ToUnicode tu-ref)
    (setf (niori-pdf-font-ref font) (niori-pdf-add-object doc t0))
    (setf (niori-pdf-font-props font)
          (plist-put props :objs
                     (list :ff ff-stream :ff-dict ff-dict
                           :cid cid :tu tu :tu-dict tu-dict
                           :ps-name ps-sym)))
    (unless (member #'niori-pdf-ttf--finalize
                    (niori-pdf-document-save-hooks doc))
      (push #'niori-pdf-ttf--finalize (niori-pdf-document-save-hooks doc)))
    font))

(defun niori-pdf-ttf--keys (hash)
  "Return a list of all keys in HASH."
  (let (keys)
    (maphash (lambda (k _v) (push k keys)) hash)
    keys))

(defun niori-pdf-ttf--finalize (doc)
  "Save hook: build subsets, W arrays and ToUnicode CMaps for DOC's fonts."
  (dolist (kv (niori-pdf-document-fonts doc))
    (let ((font (cdr kv)))
      (when (eq (niori-pdf-font-kind font) 'ttf)
        (let* ((props (niori-pdf-font-props font))
               (ff-file (plist-get props :ttf))
               (objs (plist-get props :objs))
               (used (plist-get props :used-gids))
               (used-uni (plist-get props :used-uni))
               (upm (niori-pdf-ttf-file-units-per-em ff-file))
               (widths (niori-pdf-ttf-file-widths ff-file))
               (otf-p (plist-get props :otf))
               ;; composite closure over the used glyph ids (TrueType
               ;; subsetting); CFF subsetting handles seac closure itself
               (closed (make-hash-table :test #'eql))
               (gids
                (if otf-p
                    (sort (niori-pdf-ttf--keys used) #'<)
                  (progn
                    (puthash 0 t closed)
                    (dolist (g (niori-pdf-ttf--keys used))
                      (dolist (r (niori-pdf-ttf--glyph-references ff-file g))
                        (puthash r t closed)))
                    (sort (niori-pdf-ttf--keys closed) #'<))))
               (ff-stream (or (plist-get objs :ff) (plist-get objs :ff2)))
               (ff-dict (plist-get objs :ff-dict))
               (cid (plist-get objs :cid)))
          ;; FontFile stream: FontFile2 (TrueType subset) or FontFile3
          ;; with the OpenType subtype carrying a subsetted CFF table
          (if otf-p
              (let ((subset (niori-pdf-ttf--build-otf-subset ff-file gids)))
                (setf (niori-pdf-stream-data ff-stream) subset)
                (niori-pdf-dict-set ff-dict 'Length (length subset)))
            (let ((subset (niori-pdf-ttf--build-subset ff-file gids)))
              (setf (niori-pdf-stream-data ff-stream) subset)
              (niori-pdf-dict-set ff-dict 'Length (length subset))
              (niori-pdf-dict-set ff-dict 'Length1 (length subset))))
          ;; W array (CID ranges with equal widths collapsed)
          (let ((w-arr (niori-pdf-array))
                (k 0))
            (while (< k (length gids))
              (let* ((g (nth k gids))
                     (w (aref widths g))
                     (start g)
                     (end g))
                (while (and (< (1+ k) (length gids))
                            (= (aref widths (nth (1+ k) gids)) w)
                            (= (nth (1+ k) gids) (1+ end)))
                  (setq end (nth (1+ k) gids)
                        k (1+ k)))
                (push start (niori-pdf-array-elements w-arr))
                (push end (niori-pdf-array-elements w-arr))
                (push (round (/ (* w 1000.0) upm))
                      (niori-pdf-array-elements w-arr)))
              (setq k (1+ k)))
            (setf (niori-pdf-array-elements w-arr)
                  (nreverse (niori-pdf-array-elements w-arr)))
            (niori-pdf-dict-set cid 'W w-arr)
            (niori-pdf-dict-set
             cid 'DW (round (/ (* (aref widths 0) 1000.0) upm))))
          ;; ToUnicode
          (let ((tu (plist-get objs :tu))
                (tu-dict (plist-get objs :tu-dict)))
            (setf (niori-pdf-stream-data tu)
                  (encode-coding-string (niori-pdf-ttf--tounicode used-uni) 'binary))
            (niori-pdf-dict-set tu-dict 'Length
                                (length (niori-pdf-stream-data tu)))))))
    nil))

(defun niori-pdf-ttf--tounicode (used-uni)
  "Build a ToUnicode CMap stream body from USED-UNI (gid -> codepoint)."
  (let* ((pairs (sort (niori-pdf-ttf--keys used-uni) #'<))
         (out (concat "/CIDInit /ProcSet findresource begin\n"
                      "12 dict begin\nbegincmap\n"
                      "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS)"
                      " /Supplement 0 >> def\n"
                      "/CMapName /Adobe-Identity-UCS def\n"
                      "/CMapType 2 def\n"
                      "1 begincodespacerange\n<0000> <FFFF>\n"
                      "endcodespacerange\n"))
         (i 0))
    (while (< i (length pairs))
      (let ((chunk (min 100 (- (length pairs) i)))
            (j 0))
        (setq out (concat out (format "%d beginbfchar\n" chunk)))
        (while (< j chunk)
          (let* ((g (nth (+ i j) pairs))
                 (u (gethash g used-uni)))
            (setq out
                  (concat out
                          (if (< u 65536)
                              (format "<%04X> <%04X>\n" g u)
                            ;; surrogate pair for astral planes
                            (format "<%04X> <%04X%04X>\n" g
                                    (+ 55296 (/ (- u 65536) 1024))
                                    (+ 56320 (% (- u 65536) 1024)))))))
          (setq j (1+ j)))
        (setq out (concat out "endbfchar\n"))
        (setq i (+ i chunk))))
    (concat out "endcmap\nCMapName currentdict /CMap defineresource pop\n"
            "end\nend\n")))

;;;; Subset builder

(defun niori-pdf-ttf--build-subset (ff gids)
  "Build a subset TrueType font from FF containing GIDS (plus gid 0).
Glyph ids are retained (sparse glyf), so CIDToGIDMap /Identity stays
valid.  Returns the unibyte font file bytes."
  (let* ((bytes (niori-pdf-ttf-file-bytes ff))
         (num-glyphs (niori-pdf-ttf-file-num-glyphs ff))
         (used (if (memq 0 gids) gids (cons 0 gids)))
         (loca (niori-pdf-ttf-file-loca ff))
         (glyf-off (niori-pdf-ttf-file-glyf-off ff))
         (new-glyf nil)
         (new-loca (make-vector (1+ num-glyphs) 0))
         (pos 0))
    ;; rebuild glyf: copy only glyphs in USED, keep original gids
    (dotimes (g num-glyphs)
      (aset new-loca g pos)
      (when (memq g used)
        (let* ((start (aref loca g))
               (end (aref loca (1+ g))))
          (when (> end start)
            (push (substring bytes (+ glyf-off start) (+ glyf-off end))
                  new-glyf)
            (setq pos (+ pos (- end start)))))))
    (aset new-loca num-glyphs pos)
    (setq new-glyf (apply #'concat (nreverse new-glyf)))
    ;; loca in long format (4-byte entries)
    (let ((new-loca-str (make-string (* 4 (1+ num-glyphs)) 0)))
      (dotimes (g (1+ num-glyphs))
        (let ((v (aref new-loca g)))
          (aset new-loca-str (* 4 g) (logand (ash v -24) 255))
          (aset new-loca-str (+ 1 (* 4 g)) (logand (ash v -16) 255))
          (aset new-loca-str (+ 2 (* 4 g)) (logand (ash v -8) 255))
          (aset new-loca-str (+ 3 (* 4 g)) (logand v 255))))
      ;; prepare the remaining tables
      (let* ((head-src (car (niori-pdf-ttf--table-off
                             bytes "head"
                             (niori-pdf-ttf-file-base ff))))
             (head (copy-sequence
                    (substring bytes head-src (+ head-src 54)))))
        ;; head: checkSumAdjustment = 0, indexToLocFormat = long (1)
        (dotimes (j 4) (aset head (+ 8 j) 0))
        (aset head 50 0)
        (aset head 51 1)
        (niori-pdf-ttf--assemble
         (list
          (cons "cmap" (niori-pdf-ttf--minimal-cmap used))
          (cons "glyf" new-glyf)
          (cons "head" head)
          (cons "hhea" (niori-pdf-ttf--patched-hhea ff num-glyphs))
          (cons "hmtx" (niori-pdf-ttf--full-hmtx ff))
          (cons "loca" new-loca-str)
          (cons "maxp" (niori-pdf-ttf--patched-maxp ff num-glyphs))
          (cons "name" (niori-pdf-ttf--minimal-name
                        (niori-pdf-ttf-file-ps-name ff)))
          (cons "post" (niori-pdf-ttf--post3
                        (niori-pdf-ttf-file-italic-angle ff)
                        (niori-pdf-ttf-file-fixed-pitch ff)))))))))

(defun niori-pdf-ttf--table-off (bytes tag &optional base)
  "Return (OFFSET . LENGTH) of table TAG in original font BYTES.
BASE is the sfnt directory offset (non-zero inside a TrueType
collection)."
  (let* ((n (niori-pdf-ttf--u16 bytes (+ (or base 0) 4)))
         (res nil))
    (dotimes (i n)
      (when (not res)
        (let ((rec (+ (or base 0) 12 (* i 16))))
          (when (string= (niori-pdf-ttf--tag bytes rec) tag)
            (setq res (cons (niori-pdf-ttf--u32 bytes (+ rec 8))
                            (niori-pdf-ttf--u32 bytes (+ rec 12))))))))
    (or res (error "niori-pdf: table %s not found" tag))))

(defun niori-pdf-ttf--patched-hhea (ff num-glyphs)
  "Copy the 36-byte hhea of FF with numberOfHMetrics patched to NUM-GLYPHS."
  (let* ((bytes (niori-pdf-ttf-file-bytes ff))
         (off (car (niori-pdf-ttf--table-off bytes "hhea"
                                            (niori-pdf-ttf-file-base ff))))
         (hhea (copy-sequence (substring bytes off (+ off 36)))))
    (aset hhea 34 (logand (ash num-glyphs -8) 255))
    (aset hhea 35 (logand num-glyphs 255))
    hhea))

(defun niori-pdf-ttf--patched-maxp (ff num-glyphs)
  "Copy the full maxp table of FF with numGlyphs patched to NUM-GLYPHS."
  (let* ((bytes (niori-pdf-ttf-file-bytes ff))
         (base (niori-pdf-ttf-file-base ff))
         (off (car (niori-pdf-ttf--table-off bytes "maxp" base)))
         (len (cdr (niori-pdf-ttf--table-off bytes "maxp" base)))
         (maxp (copy-sequence (substring bytes off (+ off len)))))
    (aset maxp 4 (logand (ash num-glyphs -8) 255))
    (aset maxp 5 (logand num-glyphs 255))
    maxp))

(defun niori-pdf-ttf--full-hmtx (ff)
  "Build an hmtx table with one entry per glyph (lsb 0)."
  (let* ((n (niori-pdf-ttf-file-num-glyphs ff))
         (widths (niori-pdf-ttf-file-widths ff))
         (out (make-string (* 4 n) 0)))
    (dotimes (g n)
      (let ((w (aref widths g)))
        (aset out (* 4 g) (logand (ash w -8) 255))
        (aset out (+ 1 (* 4 g)) (logand w 255))))
    out))

(defun niori-pdf-ttf--minimal-cmap (used)
  "Build a minimal format-6 cmap identity-mapping used glyph ids.
CIDToGIDMap is Identity in PDF, so the font's cmap is not consulted by
the consumer; it exists to satisfy the sfnt table requirement.  The
returned table includes the cmap directory header plus one (1,0)
encoding record."
  (let* ((gids (sort (copy-sequence used) #'<))
         (min-g (car gids))
         (max-g (car (last gids)))
         (count (1+ (- max-g min-g)))
         (sub-len (+ 10 (* 2 count)))
         (len (+ 12 sub-len))
         (tbl (make-string len 0)))
    ;; cmap directory: version 0, 1 encoding record (platform 1 / mac, 0)
    (aset tbl 0 0) (aset tbl 1 0)          ; version
    (aset tbl 2 0) (aset tbl 3 1)          ; numTables = 1
    (aset tbl 4 0) (aset tbl 5 1)          ; platformID 1
    (aset tbl 6 0) (aset tbl 7 0)          ; encodingID 0
    (aset tbl 8 0) (aset tbl 9 0)
    (aset tbl 10 0) (aset tbl 11 12)       ; offset 12
    ;; format 6 subtable at offset 12
    (aset tbl 12 0) (aset tbl 13 6)        ; format 6
    (aset tbl 14 (logand (ash sub-len -8) 255))
    (aset tbl 15 (logand sub-len 255))
    (aset tbl 16 0) (aset tbl 17 0)        ; language 0
    (aset tbl 18 (logand (ash min-g -8) 255))
    (aset tbl 19 (logand min-g 255))
    (aset tbl 20 (logand (ash count -8) 255))
    (aset tbl 21 (logand count 255))
    (dotimes (j count)
      (let* ((g (+ min-g j))
             (v (if (memq g gids) g 0)))
        (aset tbl (+ 22 (* 2 j)) (logand (ash v -8) 255))
        (aset tbl (+ 23 (* 2 j)) (logand v 255))))
    tbl))

(defun niori-pdf-ttf--minimal-name (ps-name)
  "Build a minimal name table exposing the PostScript name (nameID 6)."
  (let* ((ps (encode-coding-string ps-name 'us-ascii))
         (len (length ps))
         (tbl (make-string (+ 6 12 len) 0)))
    (aset tbl 0 0) (aset tbl 1 0)          ; format 0
    (aset tbl 2 0) (aset tbl 3 1)          ; count = 1
    (aset tbl 4 0) (aset tbl 5 18)         ; stringOffset
    ;; record: platform 1, encoding 0, language 0, nameID 6, len, off 0
    (aset tbl 6 0) (aset tbl 7 1)
    (aset tbl 8 0) (aset tbl 9 0)
    (aset tbl 10 0) (aset tbl 11 0)
    (aset tbl 12 0) (aset tbl 13 6)
    (aset tbl 14 (logand (ash len -8) 255))
    (aset tbl 15 (logand len 255))
    (aset tbl 16 0) (aset tbl 17 0)
    (dotimes (j len)
      (aset tbl (+ 18 j) (aref ps j)))
    tbl))

(defun niori-pdf-ttf--post3 (italic-angle fixed-pitch)
  "Build a version-3.0 post table (no glyph names).
version is a Fixed 16.16 number: 3.0 = #x00030000 (a literal 3 makes
strict parsers reject the font)."
  (let* ((tbl (make-string 32 0))
         (ang (round (* italic-angle 65536)))
         (a (if (< ang 0) (+ ang 4294967296) ang)))
    (aset tbl 0 0) (aset tbl 1 3) (aset tbl 2 0) (aset tbl 3 0) ; v3.0
    (aset tbl 4 (logand (ash a -24) 255))
    (aset tbl 5 (logand (ash a -16) 255))
    (aset tbl 6 (logand (ash a -8) 255))
    (aset tbl 7 (logand a 255))
    (aset tbl 8 255) (aset tbl 9 255)   ; underlinePosition -1
    (aset tbl 10 0) (aset tbl 11 50)    ; underlineThickness 50
    (when fixed-pitch                   ; isFixedPitch uint32 at 12..15
      (aset tbl 15 1))
    tbl))

(defun niori-pdf-ttf--assemble (tables)
  "Assemble TABLES ((TAG . BYTES) ...) into a complete sfnt font file."
  (setq tables (sort tables (lambda (a b) (string< (car a) (car b)))))
  (let* ((n (length tables))
         (es (max 0 (floor (log n 2))))
         (search-range (* 16 (ash 1 es)))
         (range-shift (- (* 16 n) search-range))
         (dir-len (+ 12 (* 16 n)))
         (records nil)
         (blobs nil)
         (offset dir-len))
    ;; compute offsets, pad tables to 4-byte boundaries
    (dolist (tbl tables)
      (let* ((data (cdr tbl))
             (len (length data))
             (pad (% (- 4 (% len 4)) 4))
             (padded (if (> pad 0)
                         (concat data (make-string pad 0))
                       data)))
        (push (list (car tbl) len offset) records)
        (push padded blobs)
        (setq offset (+ offset len pad))))
    (let ((out (make-string offset 0)))
      ;; header: 'OTTO' when a CFF table is present (OpenType spec),
      ;; otherwise the TrueType version tag #x00010000
      (if (assoc "CFF " tables)
          (progn (aset out 0 #x4f) (aset out 1 #x54)
                 (aset out 2 #x54) (aset out 3 #x4f))
        (aset out 0 0) (aset out 1 1) (aset out 2 0) (aset out 3 0))
      (aset out 4 (logand (ash n -8) 255))
      (aset out 5 (logand n 255))
      (aset out 6 (logand (ash search-range -8) 255))
      (aset out 7 (logand search-range 255))
      (aset out 8 (logand (ash es -8) 255))
      (aset out 9 (logand es 255))
      (aset out 10 (logand (ash range-shift -8) 255))
      (aset out 11 (logand range-shift 255))
      ;; directory records (records list is newest-first, so reverse once)
      (setq records (nreverse records))
      (dotimes (i n)
        (let* ((base (+ 12 (* i 16)))
               (rec (nth i records))
               (tag (nth 0 rec))
               (len (nth 1 rec))
               (off (nth 2 rec))
               (cs (niori-pdf-ttf--table-checksum
                    (cdr (assoc tag tables)))))
          (dotimes (j 4) (aset out (+ base j) (aref tag j)))
          (aset out (+ base 4) (logand (ash cs -24) 255))
          (aset out (+ base 5) (logand (ash cs -16) 255))
          (aset out (+ base 6) (logand (ash cs -8) 255))
          (aset out (+ base 7) (logand cs 255))
          (aset out (+ base 8) (logand (ash off -24) 255))
          (aset out (+ base 9) (logand (ash off -16) 255))
          (aset out (+ base 10) (logand (ash off -8) 255))
          (aset out (+ base 11) (logand off 255))
          (aset out (+ base 12) (logand (ash len -24) 255))
          (aset out (+ base 13) (logand (ash len -16) 255))
          (aset out (+ base 14) (logand (ash len -8) 255))
          (aset out (+ base 15) (logand len 255))))
      ;; table data
      (let ((pos dir-len))
        (dolist (blob (nreverse blobs))
          (dotimes (j (length blob))
            (aset out (+ pos j) (aref blob j)))
          (setq pos (+ pos (length blob)))))
      ;; head checkSumAdjustment: #xB1B0AFBA - checksum(whole file)
      (let ((head-off (nth 2 (assoc "head" records)))
            (fontsum (niori-pdf-ttf--table-checksum out)))
        (let ((adj (logand (+ 2981146982 (- fontsum)) 4294967295)))
          (aset out (+ head-off 8) (logand (ash adj -24) 255))
          (aset out (+ head-off 9) (logand (ash adj -16) 255))
          (aset out (+ head-off 10) (logand (ash adj -8) 255))
          (aset out (+ head-off 11) (logand adj 255))))
      out)))

(defun niori-pdf-ttf--table-checksum (bytes)
  "Checksum of BYTES: sum of big-endian uint32s, mod 2^32."
  (let* ((n (length bytes))
         (len (+ n (% (- 4 (% n 4)) 4)))
         (sum 0))
    (dotimes (i (/ len 4))
      (let ((b0 (if (< (* i 4) n) (aref bytes (* i 4)) 0))
            (b1 (if (< (+ 1 (* i 4)) n) (aref bytes (+ 1 (* i 4))) 0))
            (b2 (if (< (+ 2 (* i 4)) n) (aref bytes (+ 2 (* i 4))) 0))
            (b3 (if (< (+ 3 (* i 4)) n) (aref bytes (+ 3 (* i 4))) 0)))
        (setq sum (% (+ sum (* b0 16777216) (* b1 65536) (* b2 256) b3)
                     4294967296))))
    sum))

;;;; CFF (Type 1C) subsetting -- OpenType/CFF fonts
;;
;; The subset keeps the original GID space (a sparse CharStrings INDEX
;; whose unused slots hold an empty `endchar' glyph), copies the
;; charset, FDSelect and String INDEX verbatim, rebuilds the Top DICT /
;; FDArray / Private DICTs with rewritten offsets, and includes only
;; the referenced global/local subroutines (bias-stable: unused subr
;; slots become empty entries so the charstrings' bias-corrected
;; indices stay valid).  seac components (endchar with 4 operands) are
;; pulled in through the CFF standard encoding.  DICT integers use the
;; compact CFF 1.0 encodings (op 29 is CFF2-only and rejected by strict
;; parsers), which makes block sizes depend on the offset values; the
;; layout is therefore computed with a fixed-point iteration.  When the
;; embedding declares CIDFontType0 the subset is forced CID-keyed: a
;; synthetic identity charset (CID == GID), a minimal FDSelect and a
;; one-entry FDArray are generated so strict viewers find a conforming
;; CID-keyed CFF.

(defun niori-pdf-ttf--cff-bias (count)
  "Subroutine bias for a subr INDEX with COUNT entries.
The Type 2 charstring spec (Adobe 5177) says 107 - the 139 printed in
the older CFF spec (5176) is a known erratum that every real-world
implementation ignores."
  (cond ((< count 1240) 107)
        ((< count 33900) 1131)
        (t 32768)))

(defconst niori-pdf-ttf--cff-std-encoding
  '(;; CFF Appendix B standard encoding: (CODE SID)
    (32 1) (33 2) (34 3) (35 4) (36 5) (37 6) (38 7) (39 8) (40 9)
    (41 10) (42 11) (43 12) (44 13) (45 14) (46 15) (47 16) (48 17)
    (49 18) (50 19) (51 20) (52 21) (53 22) (54 23) (55 24) (56 25)
    (57 26) (58 27) (59 28) (60 29) (61 30) (62 31) (63 32) (64 33)
    (65 34) (66 35) (67 36) (68 37) (69 38) (70 39) (71 40) (72 41)
    (73 42) (74 43) (75 44) (76 45) (77 46) (78 47) (79 48) (80 49)
    (81 50) (82 51) (83 52) (84 53) (85 54) (86 55) (87 56) (88 57)
    (89 58) (90 59) (91 60) (92 61) (93 62) (94 63) (95 64) (96 65)
    (97 66) (98 67) (99 68) (100 69) (101 70) (102 71) (103 72)
    (104 73) (105 74) (106 75) (107 76) (108 77) (109 78) (110 79)
    (111 80) (112 81) (113 82) (114 83) (115 84) (116 85) (117 86)
    (118 87) (119 88) (120 89) (121 90) (122 91) (123 92) (124 93)
    (125 94) (126 95) (161 96) (162 97) (163 98) (164 99) (165 100)
    (166 101) (167 102) (168 103) (169 104) (170 105) (171 106)
    (172 107) (173 108) (174 109) (175 110) (177 111) (178 112)
    (179 113) (180 114) (182 115) (183 116) (184 117) (185 118)
    (186 119) (187 120) (188 121) (189 122) (191 123) (193 124)
    (194 125) (195 126) (196 127) (197 128) (198 129) (199 130)
    (200 131) (202 132) (203 133) (205 134) (206 135) (207 136)
    (208 137) (225 138) (227 139) (232 140) (233 141) (234 142)
    (235 143) (241 144) (245 145) (248 146) (249 147) (250 148)
    (251 149))
  "CFF standard encoding: (CODE SID) pairs, for seac resolution.")

(defun niori-pdf-ttf--cff-read-index (bytes pos)
  "Read the INDEX starting at BYTES/POS; return (ITEMS . END-POS)."
  (let ((count (niori-pdf-ttf--u16 bytes pos)))
    (if (= count 0)
        (cons nil (+ pos 2))
      (let* ((off-size (aref bytes (+ pos 2)))
             (base (+ pos 3))
             (offs nil))
        (dotimes (i (1+ count))
          (let ((o 0) (j 0))
            (while (< j off-size)
              (setq o (+ (* o 256) (aref bytes (+ base (* i off-size) j)))
                    j (1+ j)))
            (push (1- o) offs)))
        (setq offs (nreverse offs))
        (let* ((data-base (+ base (* (1+ count) off-size)))
               (items nil))
          (dotimes (i count)
            (push (substring bytes (+ data-base (nth i offs))
                             (+ data-base (nth (1+ i) offs)))
                  items))
          (cons (nreverse items)
                (+ data-base (nth count offs))))))))

(defun niori-pdf-ttf--cff-index-bytes (items)
  "Serialize ITEMS (list of unibyte strings) as a CFF INDEX."
  (let ((count (length items)))
    (if (= count 0)
        (unibyte-string 0 0)
      (let* ((offs (list 0))
             (acc 0))
        (dolist (it items)
          (setq acc (+ acc (length it)))
          (push acc offs))
        (setq offs (nreverse offs))
        (let* ((off-size (cond ((< acc 256) 1)
                               ((< acc 65536) 2)
                               ((< acc 16777216) 3)
                               (t 4)))
               (out (make-string (+ 3 (* off-size (1+ count))) 0)))
          (aset out 0 (logand (ash count -8) 255))
          (aset out 1 (logand count 255))
          (aset out 2 off-size)
          (dotimes (i (1+ count))
            (let ((v (1+ (nth i offs))))
              (dotimes (k off-size)
                (aset out (+ 3 (* i off-size) k)
                      (logand (ash v (* -8 (- off-size 1 k))) 255)))))
          (concat out (apply #'concat items)))))))

(defun niori-pdf-ttf--cff-read-dict (bytes start end)
  "Parse the DICT in BYTES/START..END into ((OP . OPERANDS) ...)."
  (let ((out nil)
        (ops nil)
        (i start))
    (while (< i end)
      (let ((b (aref bytes i)))
        (cond
         ((= b 28)
          (push (niori-pdf-ttf--s16 bytes (1+ i)) ops)
          (setq i (+ i 3)))
         ((= b 29)
          (push (niori-pdf-ttf--s32 bytes (1+ i)) ops)
          (setq i (+ i 5)))
         ((= b 30)
          ;; real number in BCD
          (let ((nib nil) (done nil))
            (setq i (1+ i))
            (while (and (not done) (< i end))
              (let ((byte (aref bytes i)))
                (setq i (1+ i))
                (dolist (x (list (ash byte -4) (logand byte 15)))
                  (cond ((= x 15) (setq done t))
                        (t (push x nib))))))
            (let ((s (apply #'concat
                            (mapcar (lambda (x)
                                      (cond ((= x 12) "e-")
                                            ((= x 13) "e")
                                            ((= x 14) ".")
                                            (t (number-to-string x))))
                                    (nreverse nib)))))
              (push (string-to-number s) ops))))
         ((and (>= b 32) (<= b 246))
          (push (- b 139) ops)
          (setq i (1+ i)))
         ((and (>= b 247) (<= b 250))
          (push (+ (* (- b 247) 256) (aref bytes (1+ i)) 108) ops)
          (setq i (+ i 2)))
         ((and (>= b 251) (<= b 254))
          (push (- (- (* (- b 251) 256)) (aref bytes (1+ i)) 108) ops)
          (setq i (+ i 2)))
         ((= b 12)
          (push (cons (+ 1200 (aref bytes (1+ i))) (nreverse ops)) out)
          (setq ops nil i (+ i 2)))
         (t
          (push (cons b (nreverse ops)) out)
          (setq ops nil i (1+ i))))))
    (nreverse out)))

(defun niori-pdf-ttf--cff-dict-get (entries op)
  "Operands of operator OP in DICT ENTRIES, or nil."
  (let ((hit nil))
    (dolist (e entries)
      (when (and (not hit) (= (car e) op))
        (setq hit (cdr e))))
    hit))

(defun niori-pdf-ttf--cff-real-bytes (v)
  "BCD encoding of the real number V (operator 30).
Nibble codes per the CFF spec: 0-9 digits, 10 `.', 11 `E', 12 `E-',
13 reserved, 14 `-', 15 end."
  (let* ((s (format "%.7f" v))
         (s (replace-regexp-in-string "0+$" "" s))
         (s (replace-regexp-in-string "\\.$" "" s))
         (nib nil))
    (dotimes (k (length s))
      (let ((c (aref s k)))
        (cond ((= c ?.) (push 10 nib))
              ((= c ?e) (push 11 nib))
              ((= c ?-) (push 14 nib))
              (t (push (- c 48) nib)))))
    (when (= (% (length nib) 2) 1) (push 15 nib))
    (setq nib (nreverse nib))
    (let ((out nil))
      (while nib
        (let ((hi (pop nib))
              (lo (if nib (pop nib) 15)))
          (push (+ (* hi 16) lo) out)))
      (apply #'unibyte-string 30 (nreverse out)))))

(defun niori-pdf-ttf--cff-int-bytes (n)
  "CFF 1.0 DICT operand bytes for the integer N.
-107..107 one byte, ±108..1131 two bytes, ±32768..32767 via op 28,
anything else as a BCD real number.  (The 5-byte op-29 form is CFF2
only and is not valid in a CFF 1.0 DICT.)"
  (cond
   ((and (>= n -107) (<= n 107))
    (unibyte-string (+ n 139)))
   ((and (>= n 108) (<= n 1131))
    (let ((v (- n 108)))
      (unibyte-string (+ 247 (/ v 256)) (% v 256))))
   ((and (>= n -1131) (<= n -108))
    (let ((v (- (- n) 108)))
      (unibyte-string (+ 251 (/ v 256)) (% v 256))))
   ((and (>= n -32768) (<= n 32767))
    (let ((v (if (< n 0) (+ n 65536) n)))
      (unibyte-string 28 (ash v -8) (logand v 255))))
   (t (niori-pdf-ttf--cff-real-bytes (float n)))))

(defun niori-pdf-ttf--cff-dict-bytes (entries)
  "Serialize DICT ENTRIES with compact CFF 1.0 integers, reals BCD."
  (let ((out nil))
    (dolist (e entries)
      (dolist (v (cdr e))
        (cond
         ((and (floatp v) (/= v (float (truncate v))))
          (push (niori-pdf-ttf--cff-real-bytes v) out))
         ((floatp v)
          (push (niori-pdf-ttf--cff-int-bytes (truncate v)) out))
         (t (push (niori-pdf-ttf--cff-int-bytes v) out))))
      (let ((op (car e)))
        (if (>= op 1200)
            (push (unibyte-string 12 (- op 1200)) out)
          (push (unibyte-string op) out))))
    (apply #'concat (nreverse out))))

(defun niori-pdf-ttf--cff-scan-charstring (cs)
  "Scan a Type 2 charstring CS.
Returns (GSET LSET SEACS); GSET/LSET are hash tables (eql) of the RAW
subroutine operands (the bias must be added to obtain the slot), SEACS
a list of (BCHAR . ACHAR) seac code pairs.  Returns nil when the scan
cannot track the operand stack safely (the caller must then include
all subroutines)."
  (catch 'fallback
    (let ((gused (make-hash-table :test #'eql))
          (lused (make-hash-table :test #'eql))
          (seacs nil)
          (last-num 'none)             ; 'none = unknown
          (i 0)
          (n (length cs)))
      (while (< i n)
        (let ((b (aref cs i)))
          (cond
           ((and (>= b 32) (<= b 246))
            (setq last-num (- b 139) i (1+ i)))
           ((and (>= b 247) (<= b 250))
            (setq last-num (+ (* (- b 247) 256) (aref cs (1+ i)) 108)
                  i (+ i 2)))
           ((and (>= b 251) (<= b 254))
            (setq last-num (- (- (* (- b 251) 256)) (aref cs (1+ i)) 108)
                  i (+ i 2)))
           ((= b 28)
            (setq last-num (niori-pdf-ttf--s16 cs (1+ i)) i (+ i 3)))
           ((= b 255)
            (setq last-num 'none i (+ i 5)))
           ((= b 10)
            (if (eq last-num 'none) (throw 'fallback nil))
            (puthash last-num t lused)
            (setq last-num 'none i (1+ i)))
           ((= b 29)
            (if (eq last-num 'none) (throw 'fallback nil))
            (puthash last-num t gused)
            (setq last-num 'none i (1+ i)))
           ((memq b '(1 3 18 23))
            (setq last-num 'none i (1+ i)))
           ((memq b '(19 20))
            ;; hintmask/cntrmask: skip one mask byte (the stem count is
            ;; unknown across subroutine boundaries; masks are one byte
            ;; for the common stem counts, and a wrong guess only makes
            ;; the scan bail out into the safe fallback)
            (setq i (+ i 2) last-num 'none))
           ((= b 12)
            (setq last-num 'none i (+ i 2)))
           ((= b 14)
            ;; endchar: seac when the trailing run has >= 4 numbers
            (let ((j i) (cnt 0))
              (while (and (> j 0) (< cnt 5))
                (let ((bb (aref cs (1- j))))
                  (cond
                   ((and (>= bb 32) (<= bb 246))
                    (setq j (1- j) cnt (1+ cnt)))
                   ((and (>= bb 247) (<= bb 254) (> j 1))
                    (setq j (- j 2) cnt (1+ cnt)))
                   ((and (= bb 28) (> j 2)) (setq j (- j 3) cnt (1+ cnt)))
                   ((and (= bb 255) (> j 4)) (setq j (- j 5)))
                   (t (setq j 0)))))
              (when (>= cnt 4)
                (let ((toks nil) (jj j))
                  (while (< jj i)
                    (let ((bb (aref cs jj)))
                      (cond
                       ((and (>= bb 32) (<= bb 246))
                        (push (- bb 139) toks) (setq jj (1+ jj)))
                       ((and (>= bb 247) (<= bb 250))
                        (push (+ (* (- bb 247) 256) (aref cs (1+ jj)) 108)
                              toks)
                        (setq jj (+ jj 2)))
                       ((and (>= bb 251) (<= bb 254))
                        (push (- (- (* (- bb 251) 256))
                                 (aref cs (1+ jj)) 108) toks)
                        (setq jj (+ jj 2)))
                       ((= bb 28)
                        (push (niori-pdf-ttf--s16 cs (1+ jj)) toks)
                        (setq jj (+ jj 3)))
                       ((= bb 255) (push 'none toks) (setq jj (+ jj 5)))
                       (t (setq jj i)))))
                  (setq toks (nreverse toks))
                  (when (and (>= (length toks) 4)
                             (not (memq 'none (last toks 4))))
                    (let ((l4 (last toks 4)))
                      (push (cons (nth 2 l4) (nth 3 l4)) seacs)))))
              (setq last-num 'none i (1+ i))))
           (t
            (setq last-num 'none i (1+ i))))))
      (list gused lused (nreverse seacs)))))

(defun niori-pdf-ttf--cff-charset-sids (bytes off n)
  "SID per GID from the charset at BYTES/OFF for N glyphs (vector)."
  (let ((out (make-vector n 0)))
    (cond
     ((= off 0)
      (dotimes (g n) (aset out g (min g 228))))
     ((= off 1) (error "niori-pdf: expert charsets are not supported"))
     ((= off 2)
      (error "niori-pdf: expert subset charsets are not supported"))
     (t
      (let ((fmt (aref bytes off)))
        (cond
         ((= fmt 0)
          (dotimes (g n)
            (aset out g (if (= g 0) 0
                          (niori-pdf-ttf--u16 bytes
                                              (+ off 1 (* 2 (1- g))))))))
         ((= fmt 1)
          (let ((pos (+ off 1)) (count 1))
            (aset out 0 0)
            (while (< count n)
              (let* ((first (niori-pdf-ttf--u16 bytes pos))
                     (cnt (aref bytes (+ pos 2))))
                (setq pos (+ pos 3))
                (dotimes (k (1+ cnt))
                  (when (< count n)
                    (aset out count (+ first k))
                    (setq count (1+ count))))))))
         (t
          (let ((pos (+ off 1)) (count 1))
            (aset out 0 0)
            (while (< count n)
              (let* ((first (niori-pdf-ttf--u16 bytes pos))
                     (cnt (niori-pdf-ttf--u16 bytes (+ pos 2))))
                (setq pos (+ pos 4))
                (dotimes (k (1+ cnt))
                  (when (< count n)
                    (aset out count (+ first k))
                    (setq count (1+ count))))))))))))
    out))

(defun niori-pdf-ttf--cff-charset-len (bytes off n)
  "Length in bytes of the charset at BYTES/OFF covering N glyphs."
  (cond ((memq off '(0 1 2)) 0)
        ((= (aref bytes off) 0) (+ 1 (* 2 (1- n))))
        ((= (aref bytes off) 1)
         (let ((pos (+ off 1)) (count 1))
           (while (< count n)
             (let ((cnt (aref bytes (+ pos 2))))
               (setq count (+ count cnt 1) pos (+ pos 3))))
           (- pos off)))
        (t
         (let ((pos (+ off 1)) (count 1))
           (while (< count n)
             (let ((cnt (niori-pdf-ttf--u16 bytes (+ pos 2))))
               (setq count (+ count cnt 1) pos (+ pos 4))))
           (- pos off)))))

(defun niori-pdf-ttf--cff-encoding-len (bytes off)
  "Length of a custom Encoding table at BYTES/OFF."
  (if (= (aref bytes off) 0)
      (+ 2 (aref bytes (1+ off)))
    (let* ((n (aref bytes (1+ off)))
           (p (+ off 2 (* 2 n)))
           (ns (niori-pdf-ttf--u16 bytes p)))
      (+ (- p off) 2 (* 2 ns)))))

(defun niori-pdf-ttf--cff-fdselect-len (bytes off n)
  "Length of the FDSelect table at BYTES/OFF for N glyphs.
Format 3: format(1) nRanges(2) Range3(3 each) sentinel(2) = 5+3k;
format 4: format(1) nRanges(4) Range4(5 each) sentinel(4) = 9+5k;
format 1: format(1) one FD byte per glyph = 1+n."
  (let ((fmt (aref bytes off)))
    (cond ((= fmt 3)
           (+ 5 (* 3 (niori-pdf-ttf--u16 bytes (+ off 1)))))
          ((= fmt 4)
           (+ 9 (* 5 (niori-pdf-ttf--u32 bytes (+ off 1)))))
          (t (+ 1 n)))))

(defun niori-pdf-ttf--cff-parse (bytes)
  "Parse the CFF TABLE BYTES into a plist.
BYTES is the bare CFF table (all offsets in the table are relative to
its start).  Keys: :bytes :name-index :top :strings :gsubrs
:charstrings :charset-op :charset-off :enc-op :fdarray-op
:fdselect-op :is-cid :nglyphs."
  (let* ((hdr-size (aref bytes 2))
         (r (niori-pdf-ttf--cff-read-index bytes hdr-size))
         (name-index (car r))
         (r (niori-pdf-ttf--cff-read-index bytes (cdr r)))
         (top-raw (car (car r)))
         (r (niori-pdf-ttf--cff-read-index bytes (cdr r)))
         (strings (car r))
         (r (niori-pdf-ttf--cff-read-index bytes (cdr r)))
         (gsubrs (car r))
         (top (niori-pdf-ttf--cff-read-dict top-raw 0 (length top-raw)))
         (cs-off (car (niori-pdf-ttf--cff-dict-get top 17)))
         (r (niori-pdf-ttf--cff-read-index bytes cs-off))
         (charstrings (car r))
         (charset-op (niori-pdf-ttf--cff-dict-get top 15))
         (fdarray-op (niori-pdf-ttf--cff-dict-get top 1236)))
    (list :bytes bytes
          :name-index name-index
          :top top
          :strings strings
          :gsubrs gsubrs
          :charstrings charstrings
          :nglyphs (length charstrings)
          :charset-op charset-op
          :charset-off (or (and charset-op (car charset-op)) 0)
          :enc-op (niori-pdf-ttf--cff-dict-get top 16)
          :fdarray-op fdarray-op
          :fdselect-op (niori-pdf-ttf--cff-dict-get top 1237)
          :is-cid (and fdarray-op t))))

(defun niori-pdf-ttf--cff-privs-info (p)
  "Per-Private-DICT data for the parsed CFF P: a list of
(DICT-ENTRIES SUBR-OP LSUBRS BIAS), one entry per FD (CID fonts) or a
single one (name-keyed fonts)."
  (let* ((bytes (plist-get p :bytes))
         (top (plist-get p :top))
         (fdarray-op (plist-get p :fdarray-op))
         (privs nil))
    (if (not (plist-get p :is-cid))
        (let* ((po (niori-pdf-ttf--cff-dict-get top 18))
               (psize (car po))
               (poff (cadr po))
               (pents (niori-pdf-ttf--cff-read-dict
                       bytes poff (+ poff psize)))
               (subr-op (niori-pdf-ttf--cff-dict-get pents 19))
               (lsubrs (and subr-op
                            (car (niori-pdf-ttf--cff-read-index
                                  bytes (+ poff (car subr-op)))))))
          (setq privs
                (list (list pents subr-op lsubrs
                            (if lsubrs
                                (niori-pdf-ttf--cff-bias (length lsubrs))
                              107)))))
      (let ((r (niori-pdf-ttf--cff-read-index bytes (car fdarray-op))))
        (dolist (fdr (car r))
          (let* ((ents (niori-pdf-ttf--cff-read-dict fdr 0 (length fdr)))
                 (po (niori-pdf-ttf--cff-dict-get ents 18))
                 (psize (car po))
                 (poff (cadr po))
                 (pents (niori-pdf-ttf--cff-read-dict
                         bytes poff (+ poff psize)))
                 (subr-op (niori-pdf-ttf--cff-dict-get pents 19))
                 (lsubrs (and subr-op
                              (car (niori-pdf-ttf--cff-read-index
                                    bytes (+ poff (car subr-op)))))))
            (push (list pents subr-op lsubrs
                        (if lsubrs (niori-pdf-ttf--cff-bias
                                    (length lsubrs))
                          107))
                  privs))
          (setq privs (nreverse privs))))
      (setq privs (nreverse privs)))
    privs))

(defun niori-pdf-ttf--cff-close-glyphs (p used-gids)
  "Glyph closure over seac components; returns a hash of gid -> t."
  (let* ((bytes (plist-get p :bytes))
         (charstrings (plist-get p :charstrings))
         (nglyphs (plist-get p :nglyphs))
         (closed (make-hash-table :test #'eql))
         (queue nil)
         (sids (niori-pdf-ttf--cff-charset-sids
                bytes (plist-get p :charset-off) nglyphs))
         (gid-by-sid (make-hash-table :test #'eql)))
    (puthash 0 t closed)
    (dolist (g used-gids)
      (when (< g nglyphs) (puthash g t closed)))
    (maphash (lambda (k _v) (push k queue)) closed)
    (dotimes (g nglyphs)
      (let ((s (aref sids g)))
        (unless (gethash s gid-by-sid)
          (puthash s g gid-by-sid))))
    (let ((work (copy-sequence queue)))
      (while work
        (let ((g (pop work)))
          (when (< g nglyphs)
            (let ((scan (niori-pdf-ttf--cff-scan-charstring
                         (nth g charstrings))))
              (when scan
                (dolist (pair (nth 2 scan))
                  (dolist (code (list (car pair) (cdr pair)))
                    (let* ((sid (cdr (assq code
                                           niori-pdf-ttf--cff-std-encoding)))
                           (gid (and sid (gethash sid gid-by-sid))))
                      (when (and gid (not (gethash gid closed)))
                        (puthash gid t closed)
                        (push gid work)))))))))))
    closed))

(defun niori-pdf-ttf--cff-hash-keys (hash)
  "Keys of HASH as a list."
  (let ((out nil))
    (maphash (lambda (k _v) (push k out)) hash)
    out))

(defun niori-pdf-ttf--cff-collect-usage (content gset lset)
  "Add the RAW subroutine operands of the charstring CONTENT to the
hash tables GSET and LSET.  Returns t, or the symbol `failed' when the
static operand scan could not track the stack."
  (let ((scan (niori-pdf-ttf--cff-scan-charstring content)))
    (if (not scan)
        'failed
      (dolist (k (niori-pdf-ttf--cff-hash-keys (nth 0 scan)))
        (puthash k t gset))
      (dolist (k (niori-pdf-ttf--cff-hash-keys (nth 1 scan)))
        (puthash k t lset))
      t)))

(defun niori-pdf-ttf--cff-scan-subr-content (content gset lset)
  "Scan a subroutine body CONTENT, adding newly discovered RAW
operands to GSET/LSET.  Returns `failed' when the scan cannot track
the stack, t when new references appeared, nil otherwise."
  (let ((scan (niori-pdf-ttf--cff-scan-charstring content)))
    (if (not scan)
        'failed
      (let ((grow nil))
        (dolist (k (niori-pdf-ttf--cff-hash-keys (nth 0 scan)))
          (unless (gethash k gset)
            (puthash k t gset)
            (setq grow t)))
        (dolist (k (niori-pdf-ttf--cff-hash-keys (nth 1 scan)))
          (unless (gethash k lset)
            (puthash k t lset)
            (setq grow t)))
        grow))))

(defun niori-pdf-ttf--cff-subr-closure (p closed privs)
  "Subroutine usage for CLOSED glyphs; returns (OK GSET LSET).
GSET/LSET hold RAW subroutine operands; OK nil means the static scan
failed and the caller must keep all subroutines."
  (let* ((charstrings (plist-get p :charstrings))
         (nglyphs (plist-get p :nglyphs))
         (gsubrs (plist-get p :gsubrs))
         (gb (niori-pdf-ttf--cff-bias (length gsubrs)))
         (gset (make-hash-table :test #'eql))
         (lset (make-hash-table :test #'eql))
         (ok t))
    ;; usage from the closed glyphs
    (let ((gids (niori-pdf-ttf--cff-hash-keys closed)))
      (while (and ok gids)
        (let ((g (pop gids)))
          (when (< g nglyphs)
            (let ((res (niori-pdf-ttf--cff-collect-usage
                        (nth g charstrings) gset lset)))
              (when (eq res 'failed) (setq ok nil)))))))
    ;; nested subr references, iterate until stable
    (let ((done nil))
      (while (and ok (not done))
        (let ((grow nil))
          ;; global subrs
          (let ((refs (niori-pdf-ttf--cff-hash-keys gset)))
            (while (and ok refs)
              (let* ((raw (pop refs))
                     (slot (+ raw gb)))
                (when (and (>= slot 0) (< slot (length gsubrs)))
                  (let ((res (niori-pdf-ttf--cff-scan-subr-content
                              (nth slot gsubrs) gset lset)))
                    (cond ((eq res 'failed) (setq ok nil))
                          (res (setq grow t))))))))
          ;; local subrs (union over all private dicts)
          (when ok
            (dolist (pv privs)
              (let ((lsubrs (nth 2 pv)) (lb (nth 3 pv)))
                (when lsubrs
                  (let ((refs (niori-pdf-ttf--cff-hash-keys lset)))
                    (while (and ok refs)
                      (let* ((raw (pop refs))
                             (slot (+ raw lb)))
                        (when (and (>= slot 0) (< slot (length lsubrs)))
                          (let ((res (niori-pdf-ttf--cff-scan-subr-content
                                      (nth slot lsubrs) gset lset)))
                            (cond ((eq res 'failed) (setq ok nil))
                                  (res (setq grow t))))))))))))
          (setq done (or (not ok) (not grow)))))
      (cons ok (list gset lset)))))

(defun niori-pdf-ttf--cff-bias-stable-subset (subrs raw-set bias)
  "Subr INDEX items for SUBRS keeping the raw RAW-SET usage.
Unused slots become empty entries so the bias (and thus the raw
operands in the copied charstrings) stays valid.  RAW-SET nil keeps
all subrs."
  (if (null raw-set)
      subrs
    (let ((v (make-vector (length subrs) "")))
      (dotimes (i (length subrs))
        (when (gethash (- i bias) raw-set)
          (aset v i (nth i subrs))))
      (append v nil))))

(defun niori-pdf-ttf--cff-assemble (p closed subr-closure &optional force-cid)
  "Build the subset CFF table bytes.  P is the parse plist, CLOSED the
glyph hash and SUBR-CLOSURE the (OK GSET LSET) result.  FORCE-CID
converts a name-keyed subset into a minimal CID-keyed one (identity
charset, single-FD FDSelect/FDArray) so that it conforms to a PDF
CIDFontType0 embedding."
  (let* ((bytes (plist-get p :bytes))
         (nglyphs (plist-get p :nglyphs))
         (charstrings (plist-get p :charstrings))
         (gsubrs (plist-get p :gsubrs))
         (top (plist-get p :top))
         (gb (niori-pdf-ttf--cff-bias (length gsubrs)))
         (ok (car subr-closure))
         (gset (nth 0 (cdr subr-closure)))
         (lset (nth 1 (cdr subr-closure)))
         (privs (niori-pdf-ttf--cff-privs-info p))
         (charset-op (plist-get p :charset-op))
         (charset-off (plist-get p :charset-off))
         (enc-op (plist-get p :enc-op))
         (fdarray-op (plist-get p :fdarray-op))
         (fdselect-op (plist-get p :fdselect-op))
         (src-cid (plist-get p :is-cid))
         (cid-mode (or src-cid force-cid))
         ;; ROS strings for the synthesized CID-keying: "Adobe" and
         ;; "Identity" are not CFF standard strings, so append them to
         ;; the String INDEX (SIDs start at 391).
         (str-items (append (plist-get p :strings)
                            (when (and force-cid (not src-cid))
                              (list "Adobe" "Identity"))))
         ;; ---- fixed blocks -----------------------------------------
         (hdr (unibyte-string (aref bytes 0) (aref bytes 1) 4 1))
         (name-bytes (niori-pdf-ttf--cff-index-bytes
                      (plist-get p :name-index)))
         (strings-bytes (niori-pdf-ttf--cff-index-bytes str-items))
         (gitems (niori-pdf-ttf--cff-bias-stable-subset
                  gsubrs (and ok gset) gb))
         (gsubr-bytes (niori-pdf-ttf--cff-index-bytes gitems))
         (charset-bytes
          (cond
           ((and force-cid (not src-cid))
            ;; synthetic identity charset: glyph 0 (.notdef) is
            ;; implicit and owns CID 0; glyphs 1..n-1 map to CIDs
            ;; 1..n-1 (format 2, one range) so CID == GID, matching
            ;; /CIDToGIDMap /Identity
            (cond
             ((<= nglyphs 1) (unibyte-string 0))
             (t (let ((n (- nglyphs 2)))
                  (unibyte-string 2 0 1 (ash n -8) (logand n 255))))))
           ((and charset-op (not (memq charset-off '(0 1 2))))
            (substring bytes charset-off
                       (+ charset-off
                          (niori-pdf-ttf--cff-charset-len
                           bytes charset-off nglyphs))))
           (t "")))
         (enc-bytes
          (if (and enc-op (not cid-mode) (not (memq (car enc-op) '(0 1))))
              (let ((off (car enc-op)))
                (substring bytes off
                           (+ off (niori-pdf-ttf--cff-encoding-len
                                   bytes off))))
            ""))
         (cs-items (let ((v (make-vector nglyphs (unibyte-string 14))))
                     (maphash (lambda (g _v)
                                (when (< g nglyphs)
                                  (aset v g (nth g charstrings))))
                              closed)
                     (append v nil)))
         (cs-bytes (niori-pdf-ttf--cff-index-bytes cs-items))
         (fdselect-bytes
          (cond
           ((and force-cid (not src-cid))
            ;; FDSelect format 3, one range covering every glyph -> FD 0
            (unibyte-string 3 0 1 0 0 0
                            (ash nglyphs -8) (logand nglyphs 255)))
           (src-cid
            (let ((off (car fdselect-op)))
              (substring bytes off
                         (+ off (niori-pdf-ttf--cff-fdselect-len
                                 bytes off nglyphs)))))
           (t "")))
         ;; ---- private dicts (strip old Subrs op; subset local subrs)
         (priv-data
          (let ((blocks nil) (sizes nil))
            (dolist (pv privs)
              (let* ((pents (nth 0 pv))
                     (subr-op (nth 1 pv))
                     (lsubrs (nth 2 pv))
                     (lb (nth 3 pv))
                     (clean nil))
                (dolist (e pents)
                  (unless (= (car e) 19) (push e clean)))
                (setq clean (nreverse clean))
                (let* ((base (niori-pdf-ttf--cff-dict-bytes clean))
                       ;; the Subrs INDEX follows the DICT; its offset
                       ;; is relative to the Private DICT start, so it
                       ;; only depends on the DICT bytes -- iterate to
                       ;; the fixed point of the operand encoding
                       (dict-size (length base))
                       (entry nil))
                  (when subr-op
                    (setq entry (niori-pdf-ttf--cff-int-bytes dict-size))
                    (let ((n 0))
                      (while (/= (length entry) n)
                        (setq n (length entry)
                              dict-size (+ (length base) n)
                              entry (niori-pdf-ttf--cff-dict-bytes
                                     (list (cons 19 (list dict-size)))))))
                    (setq dict-size (+ (length base) (length entry))))
                  (push (if subr-op
                            (concat base entry
                                    (niori-pdf-ttf--cff-index-bytes
                                     (niori-pdf-ttf--cff-bias-stable-subset
                                      lsubrs (and ok lset) lb)))
                          base)
                        blocks)
                  (push dict-size sizes))))
            (setq blocks (nreverse blocks) sizes (nreverse sizes))
            (cons blocks sizes)))
         (priv-blocks (car priv-data))
         (priv-sizes (cdr priv-data))
         ;; ---- FDArray (CID fonts) ----------------------------------
         (fd-dict-ents
          (cond
           (src-cid
            (let ((out nil))
              ;; FDArray offsets are relative to the CFF table start
              ;; (P :bytes is the bare CFF table)
              (dolist (fdr (car
                            (niori-pdf-ttf--cff-read-index
                             bytes (car fdarray-op))))
                (let ((ents (niori-pdf-ttf--cff-read-dict
                             fdr 0 (length fdr)))
                      (clean nil))
                  (dolist (e ents)
                    (unless (= (car e) 18) (push e clean)))
                  (push (nreverse clean) out)))
              (nreverse out)))
           (cid-mode (list nil))       ; synthesized single FD, Private only
           (t nil)))
         (fd-index-fn
          (lambda (priv-off-fn)
            ;; PRIV-OFF-FN maps an FD index to its Private DICT offset
            (let ((items nil))
              (cl-mapc
               (lambda (ents sz)
                 (let ((idx (length items)))
                   (push (concat
                          (niori-pdf-ttf--cff-dict-bytes ents)
                          (niori-pdf-ttf--cff-dict-bytes
                           (list (cons 18 (list sz
                                                (funcall priv-off-fn
                                                         idx))))))
                         items)))
               fd-dict-ents priv-sizes)
              (niori-pdf-ttf--cff-index-bytes (nreverse items)))))
         ;; ---- Top DICT rebuild --------------------------------------
         (ros-sids (and force-cid (not src-cid)
                        (list (+ 391 (length (plist-get p :strings)))
                              (+ 392 (length (plist-get p :strings))))))
         (top-entries-fn
          (lambda (offs)
            (let ((out nil) (charset-done nil))
              (dolist (e top)
                (let ((op (car e)))
                  (cond
                   ((= op 17)
                    (push (cons 17 (list (plist-get offs :cs))) out))
                   ((= op 15)
                    (setq charset-done t)
                    (cond ((> (length charset-bytes) 0)
                           (push (cons 15
                                       (list (plist-get offs :charset)))
                                 out))
                          (charset-op (push e out))))
                   ((= op 16)
                    (unless cid-mode
                      (when enc-op
                        (push (cons 16
                                    (list (if (> (length enc-bytes) 0)
                                              (plist-get offs :encoding)
                                            (car enc-op))))
                              out))))
                   ((= op 18)
                    (unless cid-mode
                      (push (cons 18
                                  (list (car priv-sizes)
                                        (plist-get offs :priv0)))
                            out)))
                   ((= op 1236)
                    (when cid-mode
                      (push (cons 1236
                                  (list (plist-get offs :fdarray))) out)))
                   ((= op 1237)
                    (when cid-mode
                      (push (cons 1237
                                  (list (plist-get offs :fdselect))) out)))
                   ((and force-cid (not src-cid)
                         (memq op '(1230 1231 1232 1233 1234 1235)))
                    nil)                ; never keep stale CID ops
                   (t (push e out)))))
              (unless charset-done
                (when (> (length charset-bytes) 0)
                  (push (cons 15 (list (plist-get offs :charset))) out)))
              (when (and force-cid (not src-cid))
                (push (cons 1230 (append ros-sids (list 0))) out)
                (push (cons 1237 (list (plist-get offs :fdselect))) out)
                (push (cons 1236 (list (plist-get offs :fdarray))) out))
              (nreverse out))))
         (assemble-with
          (lambda (offs)
            ;; returns (TABLE . OFFS2): the full table bytes for the
            ;; offset set OFFS together with the offsets its layout
            ;; implies -- equal tables mean a stable fixed point
            (let* ((top-bytes (niori-pdf-ttf--cff-dict-bytes
                               (funcall top-entries-fn offs)))
                   (top-index (niori-pdf-ttf--cff-index-bytes
                               (list top-bytes)))
                   (fd-index-bytes
                    (if cid-mode
                        (funcall fd-index-fn
                                 (lambda (idx)
                                   (let ((pv (plist-get offs :priv0)))
                                     (dotimes (k idx)
                                       (setq pv (+ pv (nth k priv-sizes))))
                                     pv)))
                      ""))
                   (offs2 nil)
                   (pos (+ (length hdr) (length name-bytes)
                           (length top-index)
                           (length strings-bytes)
                           (length gsubr-bytes)))
                   (out nil))
              (when (> (length charset-bytes) 0)
                (setq offs2 (plist-put offs2 :charset pos))
                (push charset-bytes out)
                (setq pos (+ pos (length charset-bytes))))
              (when (> (length enc-bytes) 0)
                (setq offs2 (plist-put offs2 :encoding pos))
                (push enc-bytes out)
                (setq pos (+ pos (length enc-bytes))))
              (when (> (length fdselect-bytes) 0)
                (setq offs2 (plist-put offs2 :fdselect pos))
                (push fdselect-bytes out)
                (setq pos (+ pos (length fdselect-bytes))))
              (setq offs2 (plist-put offs2 :cs pos))
              (push cs-bytes out)
              (setq pos (+ pos (length cs-bytes)))
              (when cid-mode
                (setq offs2 (plist-put offs2 :fdarray pos))
                (push fd-index-bytes out)
                (setq pos (+ pos (length fd-index-bytes))))
              (setq offs2 (plist-put offs2 :priv0 pos))
              (setq out (append (nreverse out) priv-blocks))
              (cons (concat hdr name-bytes top-index strings-bytes
                            gsubr-bytes
                            (apply #'concat (nreverse out)))
                    offs2)))))
    ;; ---- fixed-point layout ----------------------------------------
    ;; compact DICT integers make block sizes depend on the offset
    ;; values, so iterate: encode dicts with the current offsets,
    ;; recompute the layout, repeat until the table is stable
    (let ((offs (list :cs 0 :charset 0 :encoding 0 :priv0 0
                      :fdarray 0 :fdselect 0))
          (table nil))
      (catch 'niori-pdf-cff-stable
        (dotimes (_ 12)
          (let ((res (funcall assemble-with offs)))
            (when (equal (car res) table)
              (throw 'niori-pdf-cff-stable table))
            (setq table (car res) offs (cdr res)))))
      (if table
          table
        (error "niori-pdf: CFF subset layout did not converge")))))

(defun niori-pdf-ttf--cff-subset (ff used-gids &optional force-cid)
  "Build the subset CFF table for FF with USED-GIDS kept in place.
Returns the unibyte CFF table bytes.  FORCE-CID converts the subset
to CID-keyed form (see `niori-pdf-ttf--cff-assemble')."
  (let* ((raw (niori-pdf-ttf-file-bytes ff))
         (base (niori-pdf-ttf-file-base ff))
         (cffo (car (niori-pdf-ttf--table-off raw "CFF " base)))
         (cff-len (cdr (niori-pdf-ttf--table-off raw "CFF " base)))
         (p (niori-pdf-ttf--cff-parse
             (substring raw cffo (+ cffo cff-len))))
         (closed (niori-pdf-ttf--cff-close-glyphs p used-gids))
         (privs (niori-pdf-ttf--cff-privs-info p))
         (closure (niori-pdf-ttf--cff-subr-closure p closed privs)))
    (niori-pdf-ttf--cff-assemble p closed closure force-cid)))

(defun niori-pdf-ttf--build-otf-subset (ff gids)
  "Build a subset OpenType/CFF sfnt for FF keeping GIDS in place.
The metadata tables (head/hhea/hmtx/maxp/cmap/name/post) come from the
original font; the CFF table is replaced by the subset built by
`niori-pdf-ttf--cff-subset'."
  (let* ((bytes (niori-pdf-ttf-file-bytes ff))
         (base (niori-pdf-ttf-file-base ff))
         (closed (make-hash-table :test #'eql))
         (used nil))
    (puthash 0 t closed)
    (dolist (g gids) (puthash g t closed))
    (maphash (lambda (k _v) (push k used)) closed)
    (let* ((head-src (car (niori-pdf-ttf--table-off bytes "head" base)))
           (head (copy-sequence (substring bytes head-src
                                           (+ head-src 54))))
           (hhea-off (car (niori-pdf-ttf--table-off bytes "hhea" base)))
           (maxp-off (car (niori-pdf-ttf--table-off bytes "maxp" base)))
           (maxp-len (cdr (niori-pdf-ttf--table-off bytes "maxp" base)))
           (cff-subset (niori-pdf-ttf--cff-subset ff used t)))
      ;; head: checkSumAdjustment = 0 (indexToLocFormat is irrelevant
      ;; without a loca table)
      (dotimes (j 4) (aset head (+ 8 j) 0))
      (niori-pdf-ttf--assemble
       (list
        (cons "CFF " cff-subset)
        (cons "cmap" (niori-pdf-ttf--minimal-cmap used))
        (cons "head" head)
        (cons "hhea" (substring bytes hhea-off (+ hhea-off 36)))
        (cons "hmtx" (niori-pdf-ttf--full-hmtx ff))
        (cons "maxp" (substring bytes maxp-off (+ maxp-off maxp-len)))
        (cons "name" (niori-pdf-ttf--minimal-name
                      (niori-pdf-ttf-file-ps-name ff)))
        (cons "post" (niori-pdf-ttf--post3
                      (niori-pdf-ttf-file-italic-angle ff)
                      (niori-pdf-ttf-file-fixed-pitch ff))))))))

(provide 'niori-pdf-ttf)
;;; niori-pdf-ttf.el ends here
