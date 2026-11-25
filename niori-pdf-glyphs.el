;;; niori-pdf-glyphs.el --- Glyph outlines for niori-pdf (glyf + CFF) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev.
;;
;; Original Emacs Lisp implementation of TrueType/CFF glyph-outline
;; handling, informed by the corresponding capabilities in CL-PDF.
;; See LICENSE for the license applicable to this file.

;;; Commentary:

;; Extract vector outlines for drawing text as paths:
;;
;;   (niori-pdf-glyph-outline "/path/font.ttf" ?a)
;;     => (((:move 10 0) (:line 200 700) ... :close) ...)
;;
;; Contours are lists of ops in glyph units (y up, origin on the
;; baseline): (:move X Y), (:line X Y), (:curve X1 Y1 X2 Y2 X3 Y3)
;; and the symbol :close.  Quadratic TrueType curves are elevated to
;; cubics.
;;
;;   (niori-pdf-draw-glyph-outline font-or-path ?a x y size
;;                                 :fill '(0 0 0))
;;
;; draws the glyph with its baseline at X/Y, SIZE points per em.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-content)
(require 'niori-pdf-ttf)

;;;; sfnt helpers

(defun niori-pdf-glyphs--table (ff tag)
  "Return (OFFSET . LENGTH) of sfnt table TAG in FF, or nil."
  (let ((tables (niori-pdf-ttf-file-tables ff)))
    (and tables (gethash tag tables))))

(defun niori-pdf-glyphs--u16 (b i)
  (+ (* (aref b i) 256) (aref b (1+ i))))

(defun niori-pdf-glyphs--s16 (b i)
  (let ((v (+ (* (aref b i) 256) (aref b (1+ i)))))
    (if (>= v 32768) (- v 65536) v)))

(defun niori-pdf-glyphs--s32 (b i)
  (let ((v (+ (* (aref b i) 16777216) (* (aref b (1+ i)) 65536)
              (* (aref b (+ i 2)) 256) (aref b (+ i 3)))))
    (if (>= v 2147483648) (- v 4294967296) v)))

;;;; TrueType glyf outlines
;;
;; Intermediate representation: a contour is a list of (X Y ON-P)
;; triples, converted to op lists afterwards.

(defun niori-pdf-glyphs--simple-glyf (bytes start)
  "Parse a simple glyf outline at START; return (CONTOURS . BBOX)."
  (let ((nc (niori-pdf-glyphs--s16 bytes start))
        (bbox (list (niori-pdf-glyphs--s16 bytes (+ start 2))
                    (niori-pdf-glyphs--s16 bytes (+ start 4))
                    (niori-pdf-glyphs--s16 bytes (+ start 6))
                    (niori-pdf-glyphs--s16 bytes (+ start 8))))
        (pos (+ start 10))
        (endpts nil)
        (flags nil)
        (xs nil)
        (ys nil)
        (contours nil))
    (if (<= nc 0)
        (cons nil bbox)
      ;; end points of contours
      (dotimes (i nc)
        (push (niori-pdf-glyphs--u16 bytes (+ pos (* 2 i))) endpts))
      (setq endpts (nreverse endpts))
      (setq pos (+ pos (* 2 nc)))
      ;; skip instructions
      (setq pos (+ pos 2 (niori-pdf-glyphs--u16 bytes pos)))
      ;; flags
      (let ((npts (1+ (car (last endpts)))))
        (while (< (length flags) npts)
          (let ((f (aref bytes pos)))
            (setq pos (1+ pos))
            (push f flags)
            (when (/= 0 (logand f #x08))
              (let ((r (aref bytes pos)))
                (setq pos (1+ pos))
                (dotimes (_ r) (push f flags))))))
        (setq flags (nreverse flags))
        ;; x coordinates
        (let ((x 0))
          (dolist (f flags)
            (cond
             ((/= 0 (logand f #x02))
              (let ((d (aref bytes pos)))
                (setq pos (1+ pos))
                (setq x (+ x (if (/= 0 (logand f #x10)) d (- d))))
                (push x xs)))
             ((/= 0 (logand f #x10))
              (push x xs))
             (t
              (setq x (+ x (niori-pdf-glyphs--s16 bytes pos)))
              (setq pos (+ pos 2))
              (push x xs)))))
        (setq xs (nreverse xs))
        ;; y coordinates
        (let ((y 0))
          (dolist (f flags)
            (cond
             ((/= 0 (logand f #x04))
              (let ((d (aref bytes pos)))
                (setq pos (1+ pos))
                (setq y (+ y (if (/= 0 (logand f #x20)) d (- d))))
                (push y ys)))
             ((/= 0 (logand f #x20))
              (push y ys))
             (t
              (setq y (+ y (niori-pdf-glyphs--s16 bytes pos)))
              (setq pos (+ pos 2))
              (push y ys)))))
        (setq ys (nreverse ys))
        ;; slice into contours
        (let ((pts (cl-mapcar (lambda (x y f)
                                (list x y (/= 0 (logand f #x01))))
                              xs ys flags))
              (start-i 0))
          (dolist (ep endpts)
            (let ((n (1+ (- ep start-i))))
              (push (cl-subseq pts start-i (+ start-i n)) contours))
            (setq start-i (1+ ep)))
          (cons (nreverse contours) bbox))))))

(defun niori-pdf-glyphs--xform-op (dx dy sx sy op)
  "Transform outline OP by (DX DY) and scale (SX SY)."
  (if (eq op :close)
      :close
    (list (car op)
          (+ dx (* sx (nth 1 op)))
          (+ dy (* sy (nth 2 op))))))

(defun niori-pdf-glyphs--composite-glyf (ff bytes start)
  "Parse a composite glyf outline; return (CONTOURS . BBOX)."
  (let ((pos (+ start 10))
        (contours nil)
        (bbox nil))
    (catch 'done
      (while t
        (let* ((flags (niori-pdf-glyphs--u16 bytes pos))
               (gid (niori-pdf-glyphs--u16 bytes (+ pos 2)))
               (args (if (/= 0 (logand flags #x0001))
                         (list (niori-pdf-glyphs--s16 bytes (+ pos 4))
                               (niori-pdf-glyphs--s16 bytes (+ pos 6))
                               (+ pos 8))
                       (list (- (% (aref bytes (+ pos 4)) 256) 128)
                             (- (% (aref bytes (+ pos 5)) 256) 128)
                             (+ pos 6))))
               (dx (nth 0 args))
               (dy (nth 1 args))
               (pos2 (nth 2 args))
               (m00 1.0) (m11 1.0)
               (pos3
                (cond
                 ((/= 0 (logand flags #x0080))
                  (setq m00 (/ (niori-pdf-glyphs--s16 bytes pos2) 16384.0)
                        m11 (/ (niori-pdf-glyphs--s16 bytes (+ pos2 6))
                               16384.0))
                  (+ pos2 8))
                 ((/= 0 (logand flags #x0040))
                  (setq m00 (/ (niori-pdf-glyphs--s16 bytes pos2) 16384.0)
                        m11 m00)
                  (+ pos2 2))
                 ((/= 0 (logand flags #x0008))
                  (setq m00 (/ (niori-pdf-glyphs--s16 bytes pos2) 16384.0)
                        m11 m00)
                  (+ pos2 2))
                 (t pos2)))
               (sub (niori-pdf-glyphs--outline-gid ff gid))
               (xform
                (lambda (op) (niori-pdf-glyphs--xform-op dx dy m00 m11 op))))
          (dolist (contour (car sub))
            (push (mapcar xform contour) contours))
          (unless bbox
            (let ((b (cdr sub)))
              (setq bbox (list (+ dx (* m00 (nth 0 b)))
                               (+ dy (* m11 (nth 1 b)))
                               (+ dx (* m00 (nth 2 b)))
                               (+ dy (* m11 (nth 3 b)))))))
          (if (/= 0 (logand flags #x0020))
              (setq pos pos3)
            (throw 'done nil)))))
    (cons (nreverse contours) bbox)))

(defun niori-pdf-glyphs--outline-gid (ff gid)
  "Raw outline of glyph GID in FF: (CONTOURS-OF-PTS . BBOX)."
  (let* ((bytes (niori-pdf-ttf-file-bytes ff))
         (loca (niori-pdf-ttf-file-loca ff))
         (glyf (niori-pdf-ttf-file-glyf-off ff))
         (start (and loca glyf (+ glyf (aref loca gid))))
         (end (and loca glyf (+ glyf (aref loca (1+ gid))))))
    (if (or (null start) (<= end start))
        (cons nil nil)
      (let ((ncontours (niori-pdf-glyphs--s16 bytes start)))
        (if (< ncontours 0)
            (niori-pdf-glyphs--composite-glyf ff bytes start)
          (niori-pdf-glyphs--simple-glyf bytes start))))))

(defun niori-pdf-glyphs--pts-to-ops (pts)
  "Convert a contour of (X Y ON-P) points into op lists.
Quadratic Bézier segments are elevated to cubics; consecutive
off-curve points get implied on-curve midpoints."
  (let* ((n (length pts))
         (start (or (cl-position t pts :key #'caddr) 0))
         (seq (append (cl-subseq pts start n) (cl-subseq pts 0 start)))
         (ops nil)
         (cx 0.0)
         (cy 0.0)
         (pending nil)
         (started nil)
         (k2/3 (/ 2.0 3.0)))
    (dotimes (i n)
      (let* ((pt (nth i seq))
             (x (car pt))
             (y (cadr pt))
             (on (caddr pt)))
        (if on
            (if pending
                (let ((qx (car pending)) (qy (cadr pending)))
                  (push (list :curve
                              (+ cx (* k2/3 (- qx cx)))
                              (+ cy (* k2/3 (- qy cy)))
                              (+ x (* k2/3 (- qx x)))
                              (+ y (* k2/3 (- qy y)))
                              x y)
                        ops)
                  (setq cx x cy y pending nil))
              (if started
                  (push (list :line x y) ops)
                (push (list :move x y) ops)
                (setq started t))
              (setq cx x cy y))
          (when pending
            ;; implied on-curve midpoint between two off-curve points
            (let* ((qx (car pending)) (qy (cadr pending))
                   (mx (/ (+ cx qx) 2.0))
                   (my (/ (+ cy qy) 2.0)))
              (push (list :curve
                          (+ cx (* k2/3 (- qx cx)))
                          (+ cy (* k2/3 (- qy cy)))
                          (+ mx (* k2/3 (- qx mx)))
                          (+ my (* k2/3 (- qy my)))
                          mx my)
                    ops)
              (setq cx mx cy my)))
          (setq pending (list x y)))))
    ;; trailing off-curve closes back onto the contour start point
    (when pending
      (let ((qx (car pending)) (qy (cadr pending)))
        (push (list :curve
                    (+ cx (* k2/3 (- qx cx)))
                    (+ cy (* k2/3 (- qy cy)))
                    (+ cx (* k2/3 (- qx cx)))
                    (+ cy (* k2/3 (- qy cy)))
                    cx cy)
              ops)))
    (push :close ops)
    (nreverse ops)))

;;;; CFF (Type 2 charstrings)

(defconst niori-pdf-glyphs--c1 52845)
(defconst niori-pdf-glyphs--c2 22719)

(defun niori-pdf-glyphs--decipher (bytes r)
  "Type1/CFF eexec-style decryption of BYTES with key R."
  (let* ((n (length bytes))
         (out (make-string n 0))
         (rr r))
    (dotimes (i n)
      (let ((c (aref bytes i)))
        (aset out i (logxor c (/ rr 256)))
        (setq rr (% (+ (* (+ c rr) niori-pdf-glyphs--c1)
                       niori-pdf-glyphs--c2)
                    65536))))
    out))

(defun niori-pdf-glyphs--index-parse (bytes pos)
  "Parse a CFF INDEX at POS.
Returns (COUNT OFFSETS DATA-START NEXT-POS)."
  (let ((count (niori-pdf-glyphs--u16 bytes pos)))
    (if (= count 0)
        (list 0 nil nil (+ pos 2))
      (let* ((off-size (aref bytes (+ pos 2)))
             (off-start (+ pos 3))
             (offsets nil))
        (dotimes (i (1+ count))
          (let ((o 0))
            (dotimes (k off-size)
              (setq o (+ (* o 256)
                         (aref bytes (+ off-start (* i off-size) k)))))
            (push o offsets)))
        (setq offsets (nreverse offsets))
        (let* ((data-start (+ off-start (* (1+ count) off-size) -1))
               (end (+ data-start (car (last offsets)))))
          (list count offsets data-start end))))))

(defun niori-pdf-glyphs--index-entry (bytes idx i)
  "Byte string of entry I (0-based) of parsed CFF INDEX IDX."
  (let ((count (nth 0 idx))
        (offsets (nth 1 idx))
        (data-start (nth 2 idx)))
    (when (and offsets (< i count))
      (substring bytes
                 (+ data-start (nth i offsets))
                 (+ data-start (nth (1+ i) offsets))))))

(defun niori-pdf-glyphs--dict-parse (bytes start end)
  "Parse CFF DICT operators in [START END).
Returns a list of (OPCODE . OPERANDS); 12 xx operators become 1200+xx."
  (let ((pos start)
        (stack nil)
        (out nil))
    (while (< pos end)
      (let ((b0 (aref bytes pos)))
        (cond
         ((<= 32 b0 246)
          (push (- b0 139) stack)
          (setq pos (1+ pos)))
         ((<= 247 b0 250)
          (push (+ (* (- b0 247) 256) (aref bytes (1+ pos)) 108) stack)
          (setq pos (+ pos 2)))
         ((<= 251 b0 254)
          (push (- (* (- b0 251) 256) (aref bytes (1+ pos)) 108) stack)
          (setq pos (+ pos 2)))
         ((= b0 28)
          (push (niori-pdf-glyphs--s16 bytes (1+ pos)) stack)
          (setq pos (+ pos 3)))
         ((= b0 29)
          (push (niori-pdf-glyphs--s32 bytes (1+ pos)) stack)
          (setq pos (+ pos 5)))
         ((= b0 30)
          (let ((v "") done)
            (while (not done)
              (setq pos (1+ pos))
              (let* ((b (aref bytes pos))
                     (hi (/ b 16))
                     (lo (% b 16)))
                (dolist (nib (list hi lo))
                  (cond
                   ((<= nib 9) (setq v (concat v (number-to-string nib))))
                   ((= nib 10) (setq v (concat v ".")))
                   ((= nib 11) (setq v (concat v "E")))
                   ((= nib 12) (setq v (concat v "E-")))
                   ((= nib 15) (setq done t))))
                (when (> (length v) 40) (setq done t))))
            (push (string-to-number v) stack)
            (setq pos (1+ pos))))
         ((= b0 12)
          (push (cons (+ 1200 (aref bytes (1+ pos))) (nreverse stack))
                out)
          (setq stack nil)
          (setq pos (+ pos 2)))
         (t
          (push (cons b0 (nreverse stack)) out)
          (setq stack nil)
          (setq pos (1+ pos))))))
    (nreverse out)))

(defun niori-pdf-glyphs--cff-structures (ff)
  "Parse FF's CFF table into a plist with :charstrings :gsubrs :subrs."
  (let* ((bytes (niori-pdf-ttf-file-bytes ff))
         (tab (niori-pdf-glyphs--table ff "CFF "))
         (base (car tab))
         (hdr-size (aref bytes (+ base 2)))
         (pos (+ base hdr-size))
         (name-idx (niori-pdf-glyphs--index-parse bytes pos))
         (pos (nth 3 name-idx))
         (top-idx (niori-pdf-glyphs--index-parse bytes pos))
         (pos (nth 3 top-idx))
         (str-idx (niori-pdf-glyphs--index-parse bytes pos))
         (pos (nth 3 str-idx))
         (gsubr-idx (niori-pdf-glyphs--index-parse bytes pos))
         (top-dict
          (niori-pdf-glyphs--dict-parse bytes (nth 2 top-idx)
                                        (nth 3 top-idx)))
         (charstrings-off (car (cdr (assq 17 top-dict))))
         (private-entry (assq 18 top-dict))
         (private-size (and private-entry (car (cdr private-entry))))
         (private-off (and private-entry (cadr (cdr private-entry))))
         (cs-idx (and charstrings-off
                      (niori-pdf-glyphs--index-parse
                       bytes (+ base charstrings-off))))
         (local-subrs nil))
    (when private-off
      (let* ((enc (substring bytes (+ base private-off)
                             (+ base private-off private-size)))
             (priv (niori-pdf-glyphs--decipher enc 4330))
             (dict (niori-pdf-glyphs--dict-parse priv 0 (length priv)))
             (subrs-entry (assq 19 dict)))
        (when subrs-entry
          (let* ((sub-off (+ base private-off (car (cdr subrs-entry))))
                 (raw (substring bytes sub-off))
                 (dec (niori-pdf-glyphs--decipher raw 4330))
                 (idx (niori-pdf-glyphs--index-parse dec 0)))
            (if (and (> (nth 0 idx) 0)
                     (< (nth 0 idx) 65536)
                     (nth 1 idx))
                (setq local-subrs (list :bytes dec :idx idx))
              (setq local-subrs
                    (list :bytes raw
                          :idx (niori-pdf-glyphs--index-parse raw 0))))))))
    (list :bytes bytes :base base
          :charstrings cs-idx :gsubrs gsubr-idx :subrs local-subrs)))

(defun niori-pdf-glyphs--subr-bias (n)
  (cond ((< n 1240) 107)
        ((< n 33900) 1131)
        (t 32768)))

(defun niori-pdf-glyphs--flex-full-args (op a)
  "Normalize flex variant OP with stack args A to 12 explicit deltas."
  (pcase op
    (1235 a)                            ; flex: all 12 present
    (1234 (list (nth 0 a) 0             ; hflex
                (nth 1 a) (nth 2 a)
                (nth 3 a) 0
                (nth 4 a) 0
                (nth 5 a) 0
                (nth 6 a) 0))
    (1236 (list (nth 0 a) (nth 1 a)     ; hflex1
                (nth 2 a) (nth 3 a)
                (nth 4 a) 0
                (nth 5 a) 0
                (nth 6 a) (nth 7 a)
                (nth 8 a) 0))
    (1237 a)))                          ; flex1: 11 deltas + computed dy6

(defun niori-pdf-glyphs--type2 (code cff state)
  "Interpret a Type 2 charstring CODE within CFF structures STATE.
STATE is a plist with :x :y :stems :ops :stack; returns STATE."
  (let ((frames (list (cons code 0)))
        (done nil)
        (guard 0))
    (while (and frames (not done))
      (setq guard (1+ guard))
      (when (> guard 1000000)
        (error "niori-pdf-glyphs: charstring interpreter runaway"))
      (let* ((frame (car frames))
             (bytes (car frame))
             (instr (catch 'instr
                      (let ((pos (cdr frame)))
                        (while t
                          (let ((b0 (aref bytes pos)))
                            (cond
                             ((<= 32 b0 246)
                              (push (- b0 139) (plist-get state :stack))
                              (setq pos (1+ pos)))
                             ((<= 247 b0 250)
                              (push (+ (* (- b0 247) 256)
                                       (aref bytes (1+ pos)) 108)
                                    (plist-get state :stack))
                              (setq pos (+ pos 2)))
                             ((<= 251 b0 254)
                              (push (- (* (- b0 251) 256)
                                       (aref bytes (1+ pos)) 108)
                                    (plist-get state :stack))
                              (setq pos (+ pos 2)))
                             ((= b0 28)
                              (push (niori-pdf-glyphs--s16 bytes (1+ pos))
                                    (plist-get state :stack))
                              (setq pos (+ pos 3)))
                             ((= b0 29)
                              (push (niori-pdf-glyphs--s32 bytes (1+ pos))
                                    (plist-get state :stack))
                              (setq pos (+ pos 5)))
                             ((= b0 255)
                              (push (/ (niori-pdf-glyphs--s32 bytes
                                        (1+ pos))
                                       65536.0)
                                    (plist-get state :stack))
                              (setq pos (+ pos 5)))
                             (t
                              (setf (plist-get state :stack)
                                    (nreverse (plist-get state :stack)))
                              (throw 'instr
                                     (cons (if (= b0 12)
                                               (+ 1200
                                                  (aref bytes (1+ pos)))
                                             b0)
                                           (if (= b0 12)
                                               (+ pos 2)
                                             (1+ pos)))))))))))
             (op (car instr))
             (next (cdr instr))
             (sargs (plist-get state :stack))
             (nargs (length sargs))
             (x (plist-get state :x))
             (y (plist-get state :y))
             (out (plist-get state :ops)))
        (setcdr frame next)
        (setf (plist-get state :stack) nil)
        (pcase op
          ((or 1 3 18 23)
           (setf (plist-get state :stems)
                 (+ (plist-get state :stems) (/ nargs 2))))
          ((or 19 20)
           (setf (plist-get state :stems)
                 (+ (plist-get state :stems) (/ nargs 2)))
           (setcdr frame (+ (cdr frame)
                            (/ (+ (plist-get state :stems) 7) 8))))
          (21
           (let ((dx (if (= nargs 3) (nth 1 sargs) (nth 0 sargs)))
                 (dy (if (= nargs 3) (nth 2 sargs) (nth 1 sargs))))
             (when out (push :close out))
             (push (list :move (+ x dx) (+ y dy)) out)
             (setf (plist-get state :x) (+ x dx)
                   (plist-get state :y) (+ y dy))))
          (22
           (let ((dx (if (= nargs 2) (nth 1 sargs) (nth 0 sargs))))
             (when out (push :close out))
             (push (list :move (+ x dx) y) out)
             (setf (plist-get state :x) (+ x dx))))
          (4
           (let ((dy (if (= nargs 2) (nth 1 sargs) (nth 0 sargs))))
             (when out (push :close out))
             (push (list :move x (+ y dy)) out)
             (setf (plist-get state :y) (+ y dy))))
          (5
           (cl-loop for i from 0 below nargs by 2
                    do (setq x (+ x (nth i sargs))
                             y (+ y (nth (1+ i) sargs)))
                       (push (list :line x y) out))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (6
           (cl-loop for i from 0 below nargs
                    do (if (= 0 (% i 2))
                           (setq x (+ x (nth i sargs)))
                         (setq y (+ y (nth i sargs))))
                       (push (list :line x y) out))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (7
           (cl-loop for i from 0 below nargs
                    do (if (= 0 (% i 2))
                           (setq y (+ y (nth i sargs)))
                         (setq x (+ x (nth i sargs))))
                       (push (list :line x y) out))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (8
           (cl-loop for i from 0 below nargs by 6
                    do (push (list :curve
                              (+ x (nth i sargs))
                              (+ y (nth (1+ i) sargs))
                              (+ x (nth (+ i 2) sargs))
                              (+ y (nth (+ i 3) sargs))
                              (+ x (nth (+ i 4) sargs))
                              (+ y (nth (+ i 5) sargs)))
                            out)
                       (setq x (+ x (nth (+ i 4) sargs))
                             y (+ y (nth (+ i 5) sargs))))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (24
           (cl-loop for i from 0 below (- nargs 2) by 6
                    do (push (list :curve
                              (+ x (nth i sargs))
                              (+ y (nth (1+ i) sargs))
                              (+ x (nth (+ i 2) sargs))
                              (+ y (nth (+ i 3) sargs))
                              (+ x (nth (+ i 4) sargs))
                              (+ y (nth (+ i 5) sargs)))
                            out)
                       (setq x (+ x (nth (+ i 4) sargs))
                             y (+ y (nth (+ i 5) sargs))))
           (setq x (+ x (nth (- nargs 2) sargs))
                 y (+ y (nth (1- nargs) sargs)))
           (push (list :line x y) out)
           (setf (plist-get state :x) x (plist-get state :y) y))
          (25
           (cl-loop for i from 0 below (- nargs 6) by 2
                    do (setq x (+ x (nth i sargs))
                             y (+ y (nth (1+ i) sargs)))
                       (push (list :line x y) out))
           (let ((i (- nargs 6)))
             (push (list :curve
                         (+ x (nth i sargs))
                         (+ y (nth (1+ i) sargs))
                         (+ x (nth (+ i 2) sargs))
                         (+ y (nth (+ i 3) sargs))
                         (+ x (nth (+ i 4) sargs))
                         (+ y (nth (+ i 5) sargs)))
                   out)
             (setq x (+ x (nth (+ i 4) sargs))
                   y (+ y (nth (+ i 5) sargs))))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (26
           (let ((i 0) (d1 (if (= 1 (% nargs 2)) (car sargs) 0)))
             (when (= 1 (% nargs 2)) (setq i 1))
             (while (< i nargs)
               (push (list :curve
                           (+ x (nth i sargs)) (+ y d1)
                           (+ x (nth i sargs)) (+ y (nth (1+ i) sargs))
                           (+ x (nth i sargs)) (+ y (nth (1+ i) sargs)))
                     out)
               (setq y (+ y (nth (1+ i) sargs)))
               (setq i (+ i 2))))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (27
           (let ((i 0) (d1 (if (= 1 (% nargs 2)) (car sargs) 0)))
             (when (= 1 (% nargs 2)) (setq i 1))
             (while (< i nargs)
               (push (list :curve
                           (+ x d1) (+ y (nth i sargs))
                           (+ x (nth (+ i 1) sargs))
                           (+ y (nth i sargs))
                           (+ x (nth (+ i 1) sargs))
                           (+ y (nth i sargs)))
                     out)
               (setq x (+ x (nth (+ i 1) sargs)))
               (setq i (+ i 2))))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (30
           (let ((i 0))
             (while (< i nargs)
               (let ((extra (and (>= nargs (+ i 5))
                                 (= (- nargs i) 5))))
                 (push (list :curve
                             x (+ y (nth i sargs))
                             (+ x (nth (+ i 1) sargs))
                             (+ y (nth (+ i 2) sargs))
                             (+ x (nth (+ i 3) sargs))
                             (+ y (nth (+ i 3) sargs)))
                       out)
                 (setq x (+ x (nth (+ i 3) sargs))
                       y (+ y (nth (+ i 3) sargs)))
                 (when extra
                   (setq x (+ x (nth (+ i 4) sargs)))))
               (setq i (+ i 4))))
           (setf (plist-get state :x) x (plist-get state :y) y))
          (31
           (let ((i 0))
             (while (< i nargs)
               (let ((extra (and (>= nargs (+ i 5))
                                 (= (- nargs i) 5))))
                 (push (list :curve
                             (+ x (nth i sargs)) y
                             (+ x (nth i sargs))
                             (+ y (nth (+ i 1) sargs))
                             (+ x (nth (+ i 2) sargs))
                             (+ y (nth (+ i 2) sargs)))
                       out)
                 (setq x (+ x (nth (+ i 2) sargs))
                       y (+ y (nth (+ i 2) sargs)))
                 (when extra
                   (setq y (+ y (nth (+ i 4) sargs)))))
               (setq i (+ i 4))))
           (setf (plist-get state :x) x (plist-get state :y) y))
          ((or 1234 1235 1236 1237)
           (let* ((full (niori-pdf-glyphs--flex-full-args op sargs))
                  (dx1 (nth 0 full)) (dy1 (nth 1 full))
                  (dx2 (nth 2 full)) (dy2 (nth 3 full))
                  (dx3 (nth 4 full)) (dy3 (nth 5 full))
                  (dx4 (nth 6 full)) (dy4 (nth 7 full))
                  (dx5 (nth 8 full)) (dy5 (nth 9 full))
                  (dx6 (nth 10 full))
                  (dy6 (if (eq op 1237)
                           (- (sqrt (+ (expt (+ dx1 dx2 dx3 dx4 dx5 dx6) 2)
                                       (expt (+ dy1 dy2 dy3 dy4 dy5) 2)))
                              (abs (+ dx1 dx2 dx3 dx4 dx5 dx6)))
                         (nth 11 full))))
             (push (list :curve (+ x dx1) (+ y dy1)
                         (+ x dx1 dx2) (+ y dy1 dy2)
                         (+ x dx1 dx2 dx3) (+ y dy1 dy2 dy3))
                   out)
             (let ((x3 (+ x dx1 dx2 dx3))
                   (y3 (+ y dy1 dy2 dy3)))
               (push (list :curve (+ x3 dx4) (+ y3 dy4)
                           (+ x3 dx4 dx5) (+ y3 dy4 dy5)
                           (+ x3 dx4 dx5 dx6) (+ y3 dy4 dy5 dy6))
                     out)
               (setf (plist-get state :x) (+ x3 dx4 dx5 dx6)
                     (plist-get state :y) (+ y3 dy4 dy5 dy6)))))
          (10
           (let* ((ls (plist-get cff :subrs))
                  (bias (niori-pdf-glyphs--subr-bias
                         (nth 0 (plist-get ls :idx))))
                  (sub-code
                   (niori-pdf-glyphs--index-entry
                    (plist-get ls :bytes) (plist-get ls :idx)
                    (- (car sargs) bias))))
             (push (cons sub-code 0) frames)))
          (29
           (let* ((gs (plist-get cff :gsubrs))
                  (bias (niori-pdf-glyphs--subr-bias (nth 0 gs)))
                  (sub-code
                   (niori-pdf-glyphs--index-entry
                    (plist-get cff :bytes) gs
                    (- (car sargs) bias))))
             (push (cons sub-code 0) frames)))
          (11 (pop frames))
          (14
           (push :close out)
           (setq done t))
          (_ nil))
        (setf (plist-get state :ops) out)))
    state))

(defun niori-pdf-glyphs--cff-outline-gid (cff gid)
  "CFF outline of GID as a list of op contours."
  (let* ((cs (plist-get cff :charstrings))
         (code (niori-pdf-glyphs--index-entry
                (plist-get cff :bytes) cs gid))
         (state (niori-pdf-glyphs--type2
                 code cff
                 (list :x 0.0 :y 0.0 :stems 0 :ops nil :stack nil)))
         (ops (nreverse (plist-get state :ops)))
         (contours nil)
         (cur nil))
    (dolist (o ops)
      (if (eq o :close)
          (when cur
            (push (nreverse cur) contours)
            (setq cur nil))
        (push o cur)))
    (when cur (push (nreverse cur) contours))
    (nreverse contours)))

;;;; Public API

(defun niori-pdf-glyphs--font-file (font-or-path)
  "Return the parsed sfnt FILE for FONT-OR-PATH (path or ttf font)."
  (cond
   ((and (niori-pdf-font-p font-or-path)
         (eq (niori-pdf-font-kind font-or-path) 'ttf))
    (plist-get (niori-pdf-font-props font-or-path) :ttf))
   ((stringp font-or-path)
    (niori-pdf-ttf-read-file (expand-file-name font-or-path)))
   (t (error "niori-pdf-glyphs: need a font path or a ttf font"))))

(defun niori-pdf-glyph-outline (font-or-path char-or-gid)
  "Return the outline contours of CHAR-OR-GID in FONT-OR-PATH.
Contour format is described in `niori-pdf-glyphs' commentary."
  (let* ((ff (niori-pdf-glyphs--font-file font-or-path))
         ;; characters are mapped through the cmap; a raw gid falls
         ;; through when the cmap has no entry for that codepoint
         (gid (gethash char-or-gid (niori-pdf-ttf-file-cmap ff)
                       char-or-gid)))
    (if (niori-pdf-ttf-file-cff-p ff)
        (niori-pdf-glyphs--cff-outline-gid
         (niori-pdf-glyphs--cff-structures ff) gid)
      (let ((raw (car (niori-pdf-glyphs--outline-gid ff gid))))
        (mapcar #'niori-pdf-glyphs--pts-to-ops raw)))))

(cl-defun niori-pdf-draw-glyph-outline (font-or-path char x y size
                                                     &key fill stroke
                                                     (line-width 0.5)
                                                     (line-join 1))
  "Draw the outline of CHAR with baseline at X/Y scaled to SIZE.
FILL/STROKE are RGB lists; the outline uses content-layer primitives."
  (let* ((contours (niori-pdf-glyph-outline font-or-path char))
         (ff (niori-pdf-glyphs--font-file font-or-path))
         (s (/ size (float (niori-pdf-ttf-file-units-per-em ff)))))
    (niori-pdf-with-saved-state
      (when (and stroke (listp stroke))
        (niori-pdf-set-line-width line-width)
        (niori-pdf-set-line-join (or line-join 1))
        (apply #'niori-pdf-set-rgb-stroke stroke))
      (when (and fill (listp fill))
        (apply #'niori-pdf-set-rgb-fill fill))
      (dolist (contour contours)
        (dolist (op contour)
          (if (eq op :close)
              (niori-pdf-close-path)
            (let ((kind (car op)) (a (cdr op)))
              (pcase kind
                (:move
                 (niori-pdf-move-to (+ x (* s (nth 0 a)))
                                    (+ y (* s (nth 1 a)))))
                (:line
                 (niori-pdf-line-to (+ x (* s (nth 0 a)))
                                    (+ y (* s (nth 1 a)))))
                (:curve
                 (niori-pdf-curve-to
                  (+ x (* s (nth 0 a))) (+ y (* s (nth 1 a)))
                  (+ x (* s (nth 2 a))) (+ y (* s (nth 3 a)))
                  (+ x (* s (nth 4 a))) (+ y (* s (nth 5 a))))))))))
      (cond
       ((and fill stroke) (niori-pdf-fill-stroke))
       (fill (niori-pdf-fill t))
       (stroke (niori-pdf-stroke))))))

(provide 'niori-pdf-glyphs)
;;; niori-pdf-glyphs.el ends here
