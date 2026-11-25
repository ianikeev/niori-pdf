;;; niori-pdf-qrcode.el --- QR Code model 2 encoder for niori-pdf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev.
;;
;; Original QR Code Model 2 encoder.  CL-PDF's separate barcode
;; module draws precomputed QR matrices; this file implements encoding.
;; See LICENSE for the license applicable to this file.

;;; Commentary:

;; QR Code (ISO/IEC 18004 model 2) encoder in pure Elisp.  cl-pdf only
;; DRAWS a pre-computed QR matrix (`niori-pdf-barcode-qr', ported in
;; niori-pdf-barcodes.el); this module supplies the missing encoder so
;; a string can be turned into the module matrix inside Emacs:
;;
;;   (niori-pdf-qr-encode "https://niori.example/download")
;;     => ((1 1 1 1 1 1 1 0 ...) ...)
;;
;;   (niori-pdf-barcode-qr-string "https://niori.example" 50 700
;;                                :point-size 2.5)
;;
;; Features:
;;   * byte mode data (UTF-8, no ECI segment - the de-facto standard
;;     assumption of current scanners), versions 1-10,
;;   * all four error correction levels L / M / Q / H,
;;   * Reed-Solomon error correction over GF(256) with the proper
;;     block structure and interleaving,
;;   * all eight mask patterns with the ISO penalty rules N1-N4,
;;   * format information (BCH 15,5) and version information
;;     (BCH 18,6) for versions 7-10.
;;
;; The matrix is a list of N lists of N integers (1 = dark, 0 = light)
;; with row 0 at the TOP, exactly the orientation `niori-pdf-barcode-qr'
;; expects (its y axis runs downward from the anchor).

;;; Code:

(require 'cl-lib)

(declare-function niori-pdf-barcode-qr "niori-pdf-barcodes" (matrix x y &rest keys))

;;;; GF(256) arithmetic (field polynomial x^8 + x^4 + x^3 + x^2 + 1)

(defconst niori-pdf-qr--gf-exp
  (let ((v (make-vector 512 0))
        (x 1))
    (dotimes (i 255)
      (aset v i x)
      (setq x (logxor (ash x 1) (if (>= x 128) #x11d 0))))
    (dotimes (i 255)
      (aset v (+ 255 i) (aref v i)))
    v)
  "Exponent table: alpha^i for i 0..509.")

(defconst niori-pdf-qr--gf-log
  (let ((v (make-vector 256 0)))
    (dotimes (i 255)
      (aset v (aref niori-pdf-qr--gf-exp i) i))
    v)
  "Logarithm table: log_alpha(value).")

(defun niori-pdf-qr--gf-mul (a b)
  "Multiply A and B in GF(256)."
  (if (or (zerop a) (zerop b))
      0
    (aref niori-pdf-qr--gf-exp
          (+ (aref niori-pdf-qr--gf-log a)
             (aref niori-pdf-qr--gf-log b)))))

;;;; Reed-Solomon error correction

(defun niori-pdf-qr--rs-generator (degree)
  "Generator polynomial of DEGREE, coefficients low->high degree."
  (let ((poly (list 1)))
    (dotimes (i degree poly)
      ;; multiply poly by (x + alpha^i)
      (let ((out (make-list (1+ (length poly)) 0)))
        (cl-loop for c in poly
                 for idx from 0
                 do (setcar (nthcdr idx out)
                            (logxor (nth idx out)
                                    (niori-pdf-qr--gf-mul
                                     c (aref niori-pdf-qr--gf-exp i))))
                    (setcar (nthcdr (1+ idx) out)
                            (logxor (nth (1+ idx) out) c)))
        (setq poly out)))))

(defun niori-pdf-qr--rs-remainder (data degree)
  "Reed-Solomon remainder (DEGREE codewords) of DATA.
DATA is a vector with the high-degree coefficient first."
  (let* ((gen (vconcat (reverse (niori-pdf-qr--rs-generator degree))))
         (glen (length gen))
         (buf (vconcat data (make-vector degree 0))))
    (cl-loop for i from 0 below (length data)
             do (let ((coef (aref buf i)))
                  (unless (zerop coef)
                    (cl-loop for j from 0 below glen
                             do (aset buf (+ i j)
                                      (logxor (aref buf (+ i j))
                                              (niori-pdf-qr--gf-mul
                                               (aref gen j) coef)))))))
    (cl-loop for i from (length data) below (length buf)
             collect (aref buf i))))

;;;; Version / ECL tables (versions 1-10)

;; Each entry: (EC-CODEWORDS-PER-BLOCK . GROUPS) where GROUPS is a
;; list of (COUNT . DATA-CODEWORDS-PER-BLOCK).
(defconst niori-pdf-qr--blocks
  '((((7  . ((1 . 19))))
     ((10 . ((1 . 16))))
     ((13 . ((1 . 13))))
     ((17 . ((1 . 9)))))
    (((10 . ((1 . 34))))
     ((16 . ((1 . 28))))
     ((22 . ((1 . 22))))
     ((28 . ((1 . 16)))))
    (((15 . ((1 . 55))))
     ((26 . ((1 . 44))))
     ((18 . ((2 . 17))))
     ((22 . ((2 . 13)))))
    (((20 . ((1 . 80))))
     ((18 . ((2 . 32))))
     ((26 . ((2 . 24))))
     ((16 . ((4 . 9)))))
    (((26 . ((1 . 108))))
     ((24 . ((2 . 43))))
     ((18 . ((2 . 15) (2 . 16))))
     ((22 . ((2 . 11) (2 . 12)))))
    (((18 . ((2 . 68))))
     ((16 . ((4 . 27))))
     ((24 . ((4 . 19))))
     ((28 . ((4 . 15)))))
    (((20 . ((2 . 78))))
     ((18 . ((4 . 31))))
     ((18 . ((2 . 14) (4 . 15))))
     ((26 . ((4 . 13) (1 . 14)))))
    (((24 . ((2 . 97))))
     ((22 . ((2 . 38) (2 . 39))))
     ((22 . ((4 . 18) (2 . 19))))
     ((26 . ((4 . 14) (2 . 15)))))
    (((30 . ((2 . 116))))
     ((22 . ((3 . 36) (2 . 37))))
     ((20 . ((4 . 16) (4 . 17))))
     ((30 . ((4 . 12) (4 . 13)))))
    (((18 . ((2 . 68) (2 . 69))))
     ((26 . ((4 . 43) (1 . 44))))
     ((24 . ((6 . 19) (2 . 20))))
     ((28 . ((6 . 15) (2 . 16))))))
  "Block structure table for versions 1-10.
Indexed [version-1][ecl] where ecl 0=L 1=M 2=Q 3=H; each entry is a
list of (EC-CODEWORDS-PER-BLOCK . GROUPS) records, one per group.")

(defconst niori-pdf-qr--alignment-centers
  [nil [6 18] [6 22] [6 26] [6 30] [6 34]
       [6 22 38] [6 24 42] [6 26 46] [6 28 50]]
  "Alignment pattern center coordinates per version 1-10 (v1 nil).")

(defconst niori-pdf-qr--remainder-bits
  [0 7 7 7 7 7 0 0 0 0]
  "Remainder bits added after the data codewords, versions 1-10.")

(defconst niori-pdf-qr--ecl-format-bits
  '((L . 1) (M . 0) (Q . 3) (H . 2))
  "Two-bit error correction level indicator used in format info.")

(defun niori-pdf-qr--version-size (version)
  "Module count of the side of VERSION (21 + 4 * (VERSION - 1))."
  (+ 21 (* 4 (1- version))))

(defun niori-pdf-qr--ecl-index (ecl)
  "Index 0..3 of ECL symbol `L' `M' `Q' `H'."
  (pcase ecl
    (`L 0) (`M 1) (`Q 2) (`H 3)
    (_ (error "niori-pdf-qrcode: unknown error correction level %S" ecl))))

(defun niori-pdf-qr--groups (version ecl)
  "Block groups for VERSION and ECL: list of (COUNT . DATA-WORDS)."
  (apply #'append
         (mapcar #'cdr
                 (nth (niori-pdf-qr--ecl-index ecl)
                      (nth (1- version) niori-pdf-qr--blocks)))))

(defun niori-pdf-qr--ec-per-block (version ecl)
  "Error correction codewords per block for VERSION and ECL."
  (car (car (nth (niori-pdf-qr--ecl-index ecl)
                 (nth (1- version) niori-pdf-qr--blocks)))))

(defun niori-pdf-qr--data-codewords (version ecl)
  "Total data codeword count for VERSION and ECL."
  (cl-reduce #'+ (niori-pdf-qr--groups version ecl)
             :key (lambda (g) (* (car g) (cdr g))) :initial-value 0))

(defun niori-pdf-qr--byte-capacity (version ecl)
  "Maximum number of bytes encodable in VERSION at ECL (byte mode)."
  (let ((count-bits (if (>= version 10) 16 8)))
    (/ (- (* 8 (niori-pdf-qr--data-codewords version ecl)) 4 count-bits)
       8)))

(defun niori-pdf-qr--auto-version (nbytes ecl)
  "Smallest version 1-10 holding NBYTES at ECL, or nil."
  (cl-loop for v from 1 to 10
           when (<= nbytes (niori-pdf-qr--byte-capacity v ecl))
           return v))

;;;; Data encoding (byte mode)

(defun niori-pdf-qr--byte-list (string)
  "UTF-8 bytes of STRING as a list of integers."
  (mapcar #'identity (encode-coding-string string 'utf-8)))

(defun niori-pdf-qr--bits-append (bits value nbits)
  "Append VALUE as NBITS big-endian bits to reversed bit list BITS."
  (let ((out bits))
    (cl-loop for i from (1- nbits) downto 0
             do (push (logand (ash value (- i)) 1) out))
    out))

(defun niori-pdf-qr--codewords (string version ecl)
  "Data codeword list (integers 0-255) for STRING in VERSION at ECL.
Byte-mode segment, terminator, byte alignment and 0xEC/0x11 padding."
  (let* ((bytes (niori-pdf-qr--byte-list string))
         (count-bits (if (>= version 10) 16 8))
         (total (niori-pdf-qr--data-codewords version ecl))
         (bits nil))
    (setq bits (niori-pdf-qr--bits-append bits 4 4)) ; byte mode 0100
    (setq bits (niori-pdf-qr--bits-append bits (length bytes) count-bits))
    (dolist (b bytes)
      (setq bits (niori-pdf-qr--bits-append bits b 8)))
    ;; terminator: up to 4 zero bits
    (let ((term (min 4 (- (* 8 total) (length bits)))))
      (setq bits (niori-pdf-qr--bits-append bits 0 term))
      ;; pad to a byte boundary
      (let ((rem (% (length bits) 8)))
        (unless (zerop rem)
          (setq bits (niori-pdf-qr--bits-append bits 0 (- 8 rem)))))
      ;; 0xEC / 0x11 alternating pad bytes
      (while (< (length bits) (* 8 total))
        (setq bits (niori-pdf-qr--bits-append bits #xec 8))
        (when (< (length bits) (* 8 total))
          (setq bits (niori-pdf-qr--bits-append bits #x11 8))))
      (let ((cws nil) (acc 0) (n 0))
        (dolist (b (nreverse bits))
          (setq acc (+ (ash acc 1) b) n (1+ n))
          (when (= n 8)
            (push acc cws)
            (setq acc 0 n 0)))
        (nreverse cws)))))

(defun niori-pdf-qr--interleave (string version ecl)
  "Full codeword sequence for STRING: data blocks interleaved, then
EC codewords interleaved (single list)."
  (let* ((ec-len (niori-pdf-qr--ec-per-block version ecl))
         (blocks nil)
         (ec-blocks nil))
    ;; split the data codewords into blocks
    (let ((stream (vconcat (niori-pdf-qr--codewords string version ecl)))
          (pos 0))
      (dolist (g (niori-pdf-qr--groups version ecl))
        (dotimes (_ (car g))
          (let* ((n (cdr g))
                 (blk (cl-subseq stream pos (+ pos n))))
            (setq pos (+ pos n))
            (push blk blocks)))))
    (setq blocks (nreverse blocks))
    ;; error correction per block
    (dolist (blk blocks)
      (push (vconcat (niori-pdf-qr--rs-remainder blk ec-len)) ec-blocks))
    (setq ec-blocks (nreverse ec-blocks))
    ;; interleave
    (let ((out nil)
          (max-data (cl-reduce #'max blocks :key #'length :initial-value 0))
          (max-ec (cl-reduce #'max ec-blocks :key #'length
                             :initial-value 0)))
      (cl-loop for i from 0 below max-data
               do (dolist (blk blocks)
                    (when (< i (length blk))
                      (push (aref blk i) out))))
      (cl-loop for i from 0 below max-ec
               do (dolist (ec ec-blocks)
                    (when (< i (length ec))
                      (push (aref ec i) out))))
      (nreverse out))))

;;;; Function patterns and the module grid

(defun niori-pdf-qr--make-grid (size)
  "Return (MODULES RESERVED) as SIZE x SIZE vectors of vectors."
  (let ((m (make-vector size nil))
        (r (make-vector size nil)))
    (dotimes (y size)
      (aset m y (make-vector size 0))
      (aset r y (make-vector size nil)))
    (list m r)))

(defun niori-pdf-qr--set-fn (reserved modules x y dark)
  "Mark module (column X row Y) as reserved with value DARK (0/1)."
  (aset (aref reserved y) x t)
  (aset (aref modules y) x (if dark 1 0)))

(defun niori-pdf-qr--draw-finder (reserved modules size cx cy)
  "Finder + separator centered at (CX CY)."
  (cl-loop for dy from -4 to 4
           do (cl-loop for dx from -4 to 4
                       do (let* ((x (+ cx dx)) (y (+ cy dy))
                                 (dist (max (abs dx) (abs dy))))
                            (when (and (>= x 0) (< x size)
                                       (>= y 0) (< y size))
                              (niori-pdf-qr--set-fn
                               reserved modules x y
                               (and (/= dist 2) (/= dist 4))))))))

(defun niori-pdf-qr--draw-alignment (reserved modules cx cy)
  "5x5 alignment pattern centered at (CX CY)."
  (cl-loop for dy from -2 to 2
           do (cl-loop for dx from -2 to 2
                       do (niori-pdf-qr--set-fn
                           reserved modules (+ cx dx) (+ cy dy)
                           (/= 1 (max (abs dx) (abs dy)))))))

(defun niori-pdf-qr--draw-function-patterns (reserved modules version)
  "Draw finders, alignment, timing; reserve format/version areas.
The format/version modules are reserved with dummy values; the final
bit values are written after masking."
  (let ((size (niori-pdf-qr--version-size version)))
    (niori-pdf-qr--draw-finder reserved modules size 3 3)
    (niori-pdf-qr--draw-finder reserved modules size (- size 4) 3)
    (niori-pdf-qr--draw-finder reserved modules size 3 (- size 4))
    ;; alignment patterns, skipping the three finder corners
    (let ((centers (aref niori-pdf-qr--alignment-centers (1- version))))
      (when centers
        (let ((last (aref centers (1- (length centers)))))
          (dolist (cy (append centers nil))
            (dolist (cx (append centers nil))
              (unless (or (and (= cx 6) (= cy 6))
                          (and (= cx 6) (= cy last))
                          (and (= cy 6) (= cx last)))
                (niori-pdf-qr--draw-alignment
                 reserved modules cx cy)))))))
    ;; timing patterns
    (cl-loop for i from 8 below (- size 8)
             do (let ((color (if (cl-evenp i) 1 0)))
                  (unless (aref (aref reserved 6) i)
                    (aset (aref reserved 6) i t)
                    (aset (aref modules 6) i color))
                  (unless (aref (aref reserved i) 6)
                    (aset (aref reserved i) 6 t)
                    (aset (aref modules i) 6 color))))
    ;; reserve the format information areas (two L-shaped copies)
    (cl-loop for i from 0 below 9
             do (unless (= i 6)
                  (aset (aref reserved i) 8 t)
                  (aset (aref reserved 8) i t)))
    (cl-loop for i from 0 below 8
             do (aset (aref reserved (- size 1 i)) 8 t)
                (aset (aref reserved 8) (- size 1 i) t))
    ;; the dark module (row size-8, column 8)
    (aset (aref reserved (- size 8)) 8 t)
    (aset (aref modules (- size 8)) 8 1)
    ;; version information areas (versions >= 7)
    (when (>= version 7)
      (cl-loop for i from 0 below 18
               do (let ((a (+ (- size 11) (% i 3)))
                        (b (/ i 3)))
                    (aset (aref reserved b) a t)
                    (aset (aref reserved a) b t))))))

(defun niori-pdf-qr--place-codewords (reserved modules size codewords)
  "Place CODEWORDS bit by bit in the standard zigzag.
Two-column pairs run from the right edge to the left, skipping the
vertical timing column; the direction alternates.  Bits beyond the
data (remainder bits) are left light."
  (let* ((total-bits (* 8 (length codewords)))
         (bit-index 0)
         (right (- size 1)))
    (while (>= right 1)
      (when (= right 6) (setq right 5))
      (cl-loop for vert from 0 below size
               do (cl-loop for j from 0 below 2
                           do (let* ((x (- right j))
                                     (upward (zerop (logand (+ right 1) 2)))
                                     (y (if upward (- size 1 vert) vert)))
                                (unless (aref (aref reserved y) x)
                                  (if (< bit-index total-bits)
                                      (progn
                                        (aset (aref modules y) x
                                              (logand (ash
                                                       (nth (/ bit-index 8)
                                                            codewords)
                                                       (- (% bit-index 8) 7))
                                                      1))
                                        (cl-incf bit-index))
                                    (aset (aref modules y) x 0))))))
      (setq right (- right 2)))))

;;;; Format and version information

(defun niori-pdf-qr--format-bits (ecl mask)
  "15-bit format information value for ECL and MASK.
BCH(15,5) remainder of (ECL BITS, MASK BITS) over the generator
x^10+x^8+x^5+x^4+x^2+x+1 (0x537), XORed with 0x5412."
  (let* ((data (+ (ash (cdr (assq ecl niori-pdf-qr--ecl-format-bits)) 3)
                  mask))
         (rem data))
    (cl-loop for i from 0 below 10
             do (setq rem (ash rem 1))
                (when (/= 0 (logand rem #x400))
                  (setq rem (logxor rem #x537)))
             finally return (logxor (logior (ash data 10)
                                           (logand rem #x3ff))
                                   #x5412))))

(defun niori-pdf-qr--version-bits (version)
  "18-bit version information value for VERSION (>= 7).
BCH(18,6) remainder over the generator 0x1f25."
  (let ((rem version))
    (cl-loop for i from 0 below 12
             do (setq rem (ash rem 1))
                (when (/= 0 (logand rem #x1000))
                  (setq rem (logxor rem #x1f25)))
             finally return (logior (ash version 12)
                                    (logand rem #xfff)))))

(defun niori-pdf-qr--write-format (modules size ecl mask)
  "Write both unmasked copies of the format information."
  (let* ((bits (niori-pdf-qr--format-bits ecl mask))
         (get (lambda (i) (logand (ash bits (- i)) 1))))
    ;; copy 1: around the top-left finder
    (cl-loop for i from 0 to 5
             do (aset (aref modules i) 8 (funcall get i)))
    (aset (aref modules 7) 8 (funcall get 6))
    (aset (aref modules 8) 8 (funcall get 7))
    (aset (aref modules 8) 7 (funcall get 8))
    (cl-loop for i from 9 below 15
             do (aset (aref modules 8) (- 14 i) (funcall get i)))
    ;; copy 2: below the top-right finder (row 8, right side) and
    ;; right of the bottom-left finder (column 8, bottom side)
    (cl-loop for i from 0 below 8
             do (aset (aref modules 8) (- size 1 i) (funcall get i)))
    (cl-loop for i from 8 below 15
             do (aset (aref modules (+ (- size 15) i)) 8 (funcall get i)))))

(defun niori-pdf-qr--write-version (modules size version)
  "Write the two version information blocks (VERSION >= 7)."
  (let ((bits (niori-pdf-qr--version-bits version)))
    (cl-loop for i from 0 below 18
             do (let ((bit (logand (ash bits (- i)) 1))
                      (a (+ (- size 11) (% i 3)))
                      (b (/ i 3)))
                  (aset (aref modules b) a bit)
                  (aset (aref modules a) b bit)))))

;;;; Masking and penalty evaluation

(defun niori-pdf-qr--mask-p (mask-id row col)
  "Return 1 when mask MASK-ID flips the module at (ROW COL)."
  (pcase mask-id
    (0 (if (zerop (% (+ row col) 2)) 1 0))
    (1 (if (zerop (% row 2)) 1 0))
    (2 (if (zerop (% col 3)) 1 0))
    (3 (if (zerop (% (+ row col) 3)) 1 0))
    (4 (if (zerop (% (+ (/ row 2) (/ col 3)) 2)) 1 0))
    (5 (if (zerop (+ (% (* row col) 2) (% (* row col) 3))) 1 0))
    (6 (if (zerop (% (+ (% (* row col) 2) (% (* row col) 3)) 2)) 1 0))
    (7 (if (zerop (% (+ (% (+ row col) 2) (% (* row col) 3)) 2)) 1 0))
    (_ (error "niori-pdf-qrcode: bad mask id %S" mask-id))))

(defun niori-pdf-qr--apply-mask (modules reserved size mask-id)
  "Flip every non-reserved module of MODULES where the mask is 1."
  (dotimes (y size)
    (dotimes (x size)
      (unless (aref (aref reserved y) x)
        (when (= 1 (niori-pdf-qr--mask-p mask-id y x))
          (aset (aref modules y) x
                (- 1 (aref (aref modules y) x))))))))

(defun niori-pdf-qr--penalty (modules size)
  "Total penalty score (rules N1-N4) of MODULES."
  (let ((pen 0)
        (dark 0))
    ;; N1: runs of >= 5 equal modules in every row and column
    (dotimes (y size)
      (let ((run 1) (prev (aref (aref modules y) 0)))
        (cl-loop for x from 1 below size
                 do (let ((v (aref (aref modules y) x)))
                      (if (= v prev)
                          (cl-incf run)
                        (when (>= run 5) (cl-incf pen (- run 2)))
                        (setq run 1 prev v))))
        (when (>= run 5) (cl-incf pen (- run 2)))))
    (dotimes (x size)
      (let ((run 1) (prev (aref (aref modules 0) x)))
        (cl-loop for y from 1 below size
                 do (let ((v (aref (aref modules y) x)))
                      (if (= v prev)
                          (cl-incf run)
                        (when (>= run 5) (cl-incf pen (- run 2)))
                        (setq run 1 prev v))))
        (when (>= run 5) (cl-incf pen (- run 2)))))
    ;; N2: 2x2 blocks of the same color
    (cl-loop for y from 0 below (1- size)
             do (cl-loop for x from 0 below (1- size)
                         do (let ((v (aref (aref modules y) x)))
                              (when (and (= v (aref (aref modules y)
                                                    (1+ x)))
                                         (= v (aref (aref modules
                                                     (1+ y)) x))
                                         (= v (aref (aref modules
                                                     (1+ y)) (1+ x))))
                                (cl-incf pen 3)))))
    ;; N3: finder-like 1:1:3:1:1 segments with >= 4 light outside
    (let ((row-bits (make-string size ?0))
          (col-bits (make-string size ?0))
          (pat-a "00001011101")
          (pat-b "10111010000"))
      (dotimes (y size)
        (cl-loop for x from 0 below size
                 do (aset row-bits x (if (= 1 (aref (aref modules y) x))
                                         ?1 ?0)))
        (cl-incf pen (+ (* 40 (niori-pdf-qr--count-pat row-bits pat-a))
                        (* 40 (niori-pdf-qr--count-pat row-bits pat-b)))))
      (dotimes (x size)
        (cl-loop for y from 0 below size
                 do (aset col-bits y (if (= 1 (aref (aref modules y) x))
                                         ?1 ?0)))
        (cl-incf pen (+ (* 40 (niori-pdf-qr--count-pat col-bits pat-a))
                        (* 40 (niori-pdf-qr--count-pat col-bits pat-b))))))
    ;; N4: deviation of the dark module proportion from 50%
    (dotimes (y size)
      (dotimes (x size)
        (when (= 1 (aref (aref modules y) x)) (cl-incf dark))))
    (let* ((total (* size size))
           (pct (* 100.0 (/ dark (float total))))
           (dev (floor (/ (abs (- (* pct 2.0) 100.0)) 5.0))))
      (+ pen (* 10 dev)))))

(defun niori-pdf-qr--count-pat (string pattern)
  "Count non-overlapping occurrences of PATTERN in STRING."
  (let ((n 0) (start 0))
    (while (setq start (string-search pattern string start))
      (cl-incf n)
      (setq start (1+ start)))
    n))

;;;; Public encoding entry point

(cl-defun niori-pdf-qr-encode (string &key (ecl 'M) version)
  "Encode STRING as a QR Code matrix; return (MATRIX . VERSION).
MATRIX is a list of N lists of N integers (1 dark, 0 light), row 0
at the top.  ECL is the symbol `L' `M' `Q' or `H' (default `M');
VERSION forces a version 1-10 (default: the smallest one that fits).
The matrix can be drawn with `niori-pdf-barcode-qr' or, more
conveniently, `niori-pdf-barcode-qr-string'."
  (let* ((bytes (niori-pdf-qr--byte-list string))
         (version (or version
                      (niori-pdf-qr--auto-version (length bytes) ecl)
                      (error "niori-pdf-qrcode: string too long (%d bytes) for versions 1-10 at level %S"
                             (length bytes) ecl)))
         (size (niori-pdf-qr--version-size version))
         (codewords (niori-pdf-qr--interleave string version ecl))
         (best-score nil)
         (best-modules nil))
    (cl-loop for mask-id from 0 below 8
             do (let* ((grids (niori-pdf-qr--make-grid size))
                       (modules (nth 0 grids))
                       (reserved (nth 1 grids)))
                  (niori-pdf-qr--draw-function-patterns
                   reserved modules version)
                  (niori-pdf-qr--place-codewords
                   reserved modules size codewords)
                  (niori-pdf-qr--apply-mask
                   modules reserved size mask-id)
                  (niori-pdf-qr--write-format modules size ecl mask-id)
                  (when (>= version 7)
                    (niori-pdf-qr--write-version modules size version))
                  (let ((score (niori-pdf-qr--penalty modules size)))
                    (when (or (null best-score) (< score best-score))
                      (setq best-score score
                            best-modules modules)))))
    (cons (mapcar (lambda (row) (append row nil)) best-modules)
          version)))

(defun niori-pdf-qr--matrix-rows (matrix)
  "Debug helper: print MATRIX as #/space rows."
  (mapconcat (lambda (row)
               (concat (mapconcat (lambda (v) (if (= v 1) "#" ".")) row "")
                       "\n"))
             matrix ""))

;;;; Drawing integration

(cl-defun niori-pdf-barcode-qr-string (string x y &rest keys)
  "Encode STRING as a QR code and draw it with its top-left module
corner at (X Y).

This is the string-level convenience front-end over
`niori-pdf-qr-encode' + `niori-pdf-barcode-qr' (which ports cl-pdf's
`draw-qr-code' for pre-computed matrices).  KEYS:

  :ecl         error correction level `L' `M' `Q' `H' (default `M')
  :version     force a QR version 1-10 (default: smallest that fits)
  :point-size  module size in points (default 2)
  :color       fill color: nil/black, a color name, or (R G B)

All other KEYS are ignored.  Returns (list WIDTH HEIGHT VERSION) of
the drawn symbol (VERSION = the QR version used); see
`niori-pdf-qr-encode' for the raw matrix."
  (let* ((matrix+ver (niori-pdf-qr-encode
                      string
                      :ecl (or (plist-get keys :ecl) 'M)
                      :version (plist-get keys :version)))
         (dims (apply #'niori-pdf-barcode-qr (car matrix+ver) x y keys)))
    (list (car dims) (cadr dims) (cdr matrix+ver))))

(provide 'niori-pdf-qrcode)
;;; niori-pdf-qrcode.el ends here
