;;; niori-pdf-content.el --- Text and vector graphics primitives -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2009 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; Portions of this file are derived from CL-PDF's pdf-base.lisp,
;; text.lisp and pdf-geom.lisp.  See LICENSE for details.

;;; Commentary:

;; Content-stream level API: this is the Elisp equivalent of cl-pdf's
;; pdf-base.lisp, text.lisp and pdf-geom.lisp.  Pages accumulate operator
;; strings; text uses BT/ET blocks with Tm positioning and TJ arrays for
;; justification (works for both base-14 and CID-keyed TTF fonts).

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-fonts)

;;;; Dynamic drawing context

(defvar niori-pdf--doc nil "Current document while building.")
(defvar niori-pdf--page nil "Current page while building.")
(defvar niori-pdf--font nil "Current font object (a `niori-pdf-font').")
(defvar niori-pdf--font-size nil "Current font size in points.")

(defun niori-pdf--emit (string)
  "Append operator STRING to the current page's content stream."
  (niori-pdf-page-emit niori-pdf--page string))

(defun niori-pdf--emitf (fmt &rest args)
  (niori-pdf-page-emit niori-pdf--page (apply #'format fmt args)))

;;;; Document/page macros

(defmacro niori-pdf-with-document (props &rest body)
  "Bind a fresh document context and run BODY.
PROPS is a literal plist: (:title :author :subject :keywords :creator
:producer :page-width :page-height :compression).  Returns the
finished document."
  (declare (indent 1))
  `(let ((niori-pdf--doc (niori-pdf-make-document))
         niori-pdf--page niori-pdf--font niori-pdf--font-size)
     ,@(when (plist-get props :page-width)
         `((setf (niori-pdf-document-page-width niori-pdf--doc)
                 ,(plist-get props :page-width))))
     ,@(when (plist-get props :page-height)
         `((setf (niori-pdf-document-page-height niori-pdf--doc)
                 ,(plist-get props :page-height))))
     ,@(when (plist-get props :compression)
         `((setf (niori-pdf-document-compression niori-pdf--doc)
                 ,(plist-get props :compression))))
     (setf (niori-pdf-document-info niori-pdf--doc)
           (append
            (list :producer
                  (concat "niori-pdf.el " niori-pdf-version " (port of cl-pdf)"))
            ',(apply #'append
                     (mapcar (lambda (k)
                               (when (plist-get props k)
                                 (list k (plist-get props k))))
                             '(:title :author :subject :keywords :creator)))))
     ,@body
     niori-pdf--doc))

(defmacro niori-pdf-with-page (&optional props &rest body)
  "Create a page in the current document and run BODY inside it.
PROPS (optional plist): :width :height.  Geometry is in PDF points with
the origin at the bottom-left of the page, as in cl-pdf."
  (declare (indent 1))
  `(progn
     (setq niori-pdf--page
           (niori-pdf-new-page niori-pdf--doc
                               ,(plist-get props :width)
                               ,(plist-get props :height)))
     ,@body))

(defmacro niori-pdf-with-saved-state (&rest body)
  "Save and restore the graphics state around BODY (q/Q operators)."
  (declare (indent 0))
  `(progn (niori-pdf--emit "q\n") ,@body (niori-pdf--emit "Q\n")))

(defmacro niori-pdf-in-text-mode (&rest body)
  "Run BODY inside a BT/ET text block."
  (declare (indent 0))
  `(progn (niori-pdf--emit "BT\n")
          (setq niori-pdf--font nil niori-pdf--font-size nil)
          ,@body
          (niori-pdf--emit "ET\n")))

;;;; Fonts and text state

(defun niori-pdf-set-font (name size)
  "Set the current font to NAME (base-14 name or .ttf path) at SIZE points."
  (let ((font (niori-pdf-get-font niori-pdf--doc name)))
    (setq niori-pdf--font font
          niori-pdf--font-size (float size))
    (niori-pdf--emitf "/%s %s Tf\n"
                      (niori-pdf-font-resource font)
                      (niori-pdf-num size))
    font))

(defun niori-pdf--encoded-to-tj-string (encoded font)
  "Return the PDF string token for ENCODED data of FONT."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1) (niori-pdf--escape-literal-string encoded))
    (`ttf
     (concat "<"
             (mapconcat (lambda (g) (format "%04X" g)) encoded "")
             ">"))))

(defun niori-pdf--encoded-tj-operator (encoded font &optional kerned)
  "Return the PDF operator string showing ENCODED glyphs of FONT.
With KERNED, apply the font's AFM kerning pairs as TJ adjustments."
  (if (and kerned (memq (niori-pdf-font-kind font) '(base14 type1)))
      (let ((parts nil)
            (start 0)
            (n (length encoded)))
        (dotimes (i n)
          (let ((dx (and (> i 0)
                         (niori-pdf-font-kern font (aref encoded (1- i))
                              (aref encoded i)))))
            (when (and dx (/= dx 0))
              (push (niori-pdf--escape-literal-string
                     (substring encoded start i))
                    parts)
              ;; positive TJ values move left; kern DX is negative
              (push (niori-pdf-num (- dx)) parts)
              (setq start i))))
        (push (niori-pdf--escape-literal-string
               (substring encoded start))
              parts)
        (setq parts (nreverse parts))
        (if (= (length parts) 1)
            (concat (car parts) " Tj\n")
          (concat "[ " (mapconcat #'identity parts " ") " ] TJ\n")))
    (concat (niori-pdf--encoded-to-tj-string encoded font) " Tj\n")))

(defun niori-pdf-show-text (string)
  "Show STRING at the current text position (Tj)."
  (let* ((encoded (niori-pdf-font-encode string niori-pdf--font)))
    (niori-pdf--emit
     (niori-pdf--encoded-tj-operator encoded niori-pdf--font))))

(defun niori-pdf-show-text-kerned (string)
  "Show STRING at the current text position with AFM kerning.
Applies the kerning pairs of base-14 fonts; identical to
`niori-pdf-show-text' for fonts without kerning data."
  (let* ((encoded (niori-pdf-font-encode string niori-pdf--font)))
    (niori-pdf--emit
     (niori-pdf--encoded-tj-operator encoded niori-pdf--font 'kerned))))

(defun niori-pdf-show-justified (segments)
  "Show SEGMENTS as a TJ array with inter-segment adjustments.
SEGMENTS is a list of strings and numbers: a string is drawn, a number
gives the displacement in thousandths of an em (negative moves right)."
  (let ((parts nil))
    (dolist (segment segments)
      (push
       (if (numberp segment)
           (niori-pdf-num segment)
         (let* ((encoded (niori-pdf-font-encode segment niori-pdf--font)))
           (niori-pdf--encoded-to-tj-string encoded niori-pdf--font)))
       parts))
    (niori-pdf--emit
     (concat "[ " (mapconcat #'identity (nreverse parts) " ") " ] TJ\n"))))

(defun niori-pdf-move-text (x &optional y)
  "Move the text position by (X Y) points (Td)."
  (niori-pdf--emitf "%s %s Td\n" (niori-pdf-num x) (niori-pdf-num (or y 0))))

(defun niori-pdf-set-text-matrix (a b c d e f)
  "Set the text matrix directly (Tm)."
  (niori-pdf--emitf "%s %s %s %s %s %s Tm\n"
                    (niori-pdf-num a) (niori-pdf-num b) (niori-pdf-num c)
                    (niori-pdf-num d) (niori-pdf-num e) (niori-pdf-num f)))

(defun niori-pdf-set-leading (leading)
  "Set the text leading in points (TL)."
  (niori-pdf--emitf "%s TL\n" (niori-pdf-num leading)))

(defun niori-pdf-next-line ()
  "Move to the start of the next line (T*)."
  (niori-pdf--emit "T*\n"))

(defun niori-pdf-show-text-on-next-line (string)
  "Move to the start of the next line and show STRING (' operator)."
  (niori-pdf--emit "'\n")
  (niori-pdf-show-text string))

(defun niori-pdf-set-text-rise (rise)
  "Set the text rise, the baseline offset in points, to RISE (Ts).
Complements `niori-pdf-set-text-matrix' for pure vertical shifts."
  (niori-pdf--emitf "%s Ts\n" (niori-pdf-num rise)))

(defun niori-pdf-set-char-spacing (s)
  "Set character spacing S points (Tc)."
  (niori-pdf--emitf "%s Tc\n" (niori-pdf-num s)))

(defun niori-pdf-set-word-spacing (w)
  "Set word spacing W points (Tw)."
  (niori-pdf--emitf "%s Tw\n" (niori-pdf-num w)))

(defun niori-pdf-set-text-scale (s)
  "Set horizontal text scale to S percent (Tz)."
  (niori-pdf--emitf "%s Tz\n" (niori-pdf-num s)))

(defun niori-pdf-set-text-render-mode (m)
  "Set text rendering mode M (Tr): 0 fill, 1 stroke, 3 invisible..."
  (niori-pdf--emitf "%d Tr\n" m))

;;;; Drawing text at absolute positions

(defun niori-pdf-draw-text-at (x y string &optional font size)
  "Draw STRING with its baseline at (X Y) using FONT at SIZE."
  (let ((font (or font niori-pdf--font))
        (size (or size niori-pdf--font-size)))
    (unless font
      (error "niori-pdf: no current font (call niori-pdf-set-font first)"))
    (niori-pdf-in-text-mode
     (niori-pdf-set-font (niori-pdf-font-name font) size)
     (niori-pdf-set-text-matrix 1 0 0 1 x y)
     (niori-pdf-show-text string))))

(defun niori-pdf-draw-text (x y string)
  "Draw STRING at (X Y) with the current font and size."
  (niori-pdf-draw-text-at x y string))

(defun niori-pdf-draw-centered-text (x y string &optional font size)
  "Draw STRING horizontally centered on X with baseline at Y."
  (let* ((size (or size niori-pdf--font-size 12))
         (font (or font niori-pdf--font))
         (w (niori-pdf-text-width string font size)))
    (niori-pdf-draw-text-at (- x (/ w 2.0)) y string font size)))

(defun niori-pdf-draw-right-text (x y string &optional font size)
  "Draw STRING right-aligned at X with baseline at Y."
  (let* ((size (or size niori-pdf--font-size 12))
         (font (or font niori-pdf--font))
         (w (niori-pdf-text-width string font size)))
    (niori-pdf-draw-text-at (- x w) y string font size)))

(defun niori-pdf-draw-left-text (x y string &optional font size)
  "Draw STRING left-aligned at X with baseline at Y (same as draw-text-at)."
  (niori-pdf-draw-text-at x y string font size))

;;;; Paths

(defun niori-pdf-move-to (x y)
  "Start a new subpath at (X Y) (m)."
  (niori-pdf--emitf "%s %s m\n" (niori-pdf-num x) (niori-pdf-num y)))

(defun niori-pdf-line-to (x y)
  "Append a straight line to (X Y) (l)."
  (niori-pdf--emitf "%s %s l\n" (niori-pdf-num x) (niori-pdf-num y)))

(defun niori-pdf-curve-to (x1 y1 x2 y2 x3 y3)
  "Append a cubic Bezier curve (c) with control points (X1 Y1) (X2 Y2)
ending at (X3 Y3)."
  (niori-pdf--emitf "%s %s %s %s %s %s c\n"
                    (niori-pdf-num x1) (niori-pdf-num y1)
                    (niori-pdf-num x2) (niori-pdf-num y2)
                    (niori-pdf-num x3) (niori-pdf-num y3)))

(defun niori-pdf-bezier2-to (x1 y1 x3 y3)
  "Append a cubic Bezier (v) whose FIRST control point coincides with
the current point; the explicit control is (X1 Y1), the end is (X3 Y3)."
  (niori-pdf--emitf "%s %s %s %s v\n"
                    (niori-pdf-num x1) (niori-pdf-num y1)
                    (niori-pdf-num x3) (niori-pdf-num y3)))

(defun niori-pdf-bezier3-to (x2 y2 x3 y3)
  "Append a cubic Bezier (y) whose SECOND control point coincides with
the end point; the explicit control is (X2 Y2), the end is (X3 Y3)."
  (niori-pdf--emitf "%s %s %s %s y\n"
                    (niori-pdf-num x2) (niori-pdf-num y2)
                    (niori-pdf-num x3) (niori-pdf-num y3)))

(defun niori-pdf-close-path ()
  "Close the current subpath (h)."
  (niori-pdf--emit "h\n"))

(defun niori-pdf-rectangle (x y dx dy)
  "Append a rectangle at (X Y) with size DX x DY (re)."
  (niori-pdf--emitf "%s %s %s %s re\n"
                    (niori-pdf-num x) (niori-pdf-num y)
                    (niori-pdf-num dx) (niori-pdf-num dy)))

(defun niori-pdf-arc (cx cy radius &optional start extent)
  "Append an arc around (CX CY) with RADIUS, from START degrees sweeping
EXTENT degrees (Bezier approximation).  With nil EXTENT draws a full circle."
  (let* ((start (or start 0))
         (extent (or extent 360))
         (steps (max 1 (ceiling (/ (abs extent) 45.0))))
         (seg (/ extent (float steps)))
         (k (* (/ 4.0 3.0)
               (tan (/ (* seg 3.141592653589793) 720.0))))
         (to-rad (lambda (d) (* d 3.141592653589793 (/ 1 180.0)))))
    (let ((a0 (funcall to-rad start)))
      (niori-pdf-move-to (+ cx (* radius (cos a0)))
                         (+ cy (* radius (sin a0)))))
    (dotimes (i steps)
      (let* ((a0 (funcall to-rad (+ start (* i seg))))
             (a1 (funcall to-rad (+ start (* (1+ i) seg)))))
        (niori-pdf-curve-to
         (+ cx (* radius (- (cos a0) (* k (sin a0)))))
         (+ cy (* radius (+ (sin a0) (* k (cos a0)))))
         (+ cx (* radius (+ (cos a1) (* k (sin a1)))))
         (+ cy (* radius (- (sin a1) (* k (cos a1)))))
         (+ cx (* radius (cos a1)))
         (+ cy (* radius (sin a1))))))))

(defun niori-pdf-circle (cx cy radius)
  "Append a circle around (CX CY) with RADIUS."
  (niori-pdf-arc cx cy radius 0 360))

(defun niori-pdf-ellipse (cx cy ra rb)
  "Append an ellipse around (CX CY) with radii RA (x) and RB (y).
The curve segments are emitted directly in target coordinates: a
path is transformed by the CTM in effect when it is PAINTED, so a
transform-and-restore implementation would be painted at the wrong
position if the caller strokes after the restore."
  (let* ((steps (max 1 (ceiling (/ 360.0 45.0))))
         (seg (/ 360.0 steps))
         (k (* (/ 4.0 3.0)
               (tan (/ (* seg 3.141592653589793) 720.0))))
         (to-rad (lambda (d) (* d 3.141592653589793 (/ 1 180.0)))))
    (let ((a0 0.0))
      (niori-pdf-move-to (+ cx (* ra (cos a0))) cy))
    (dotimes (i steps)
      (let* ((a0 (funcall to-rad (* i seg)))
             (a1 (funcall to-rad (* (1+ i) seg))))
        (niori-pdf-curve-to
         (+ cx (* ra (- (cos a0) (* k (sin a0)))))
         (+ cy (* rb (+ (sin a0) (* k (cos a0)))))
         (+ cx (* ra (+ (cos a1) (* k (sin a1)))))
         (+ cy (* rb (- (sin a1) (* k (cos a1)))))
         (+ cx (* ra (cos a1)))
         (+ cy (* rb (sin a1))))))))

(defun niori-pdf-polyline (points &optional closed)
  "Append lines through POINTS ((x y) ...); CLOSED closes the path."
  (when points
    (niori-pdf-move-to (car (car points)) (cdr (car points)))
    (dolist (p (cdr points))
      (niori-pdf-line-to (car p) (cdr p)))
    (when closed (niori-pdf-close-path))))

;;;; Painting operators

(defun niori-pdf-stroke () (niori-pdf--emit "S\n"))
(defun niori-pdf-close-stroke () (niori-pdf--emit "s\n"))
(defun niori-pdf-fill (&optional even-odd)
  "Fill the current path (f or f* when EVEN-ODD)."
  (niori-pdf--emit (if even-odd "f*\n" "f\n")))
(defun niori-pdf-fill-stroke () (niori-pdf--emit "B\n"))
(defun niori-pdf-close-fill-stroke () (niori-pdf--emit "b\n"))
(defun niori-pdf-even-odd-fill-stroke () (niori-pdf--emit "B*\n"))
(defun niori-pdf-even-odd-close-fill-stroke () (niori-pdf--emit "b*\n"))
(defun niori-pdf-end-path ()
  "End the current path without painting or clipping (n).
Useful to terminate a path that was only used for a W clip."
  (niori-pdf--emit "n\n"))
(defun niori-pdf-clip (&optional even-odd)
  "Clip to the current path (W n)."
  (niori-pdf--emit (if even-odd "W* n\n" "W n\n")))

;;;; Graphics state

(defun niori-pdf-set-line-width (w) (niori-pdf--emitf "%s w\n" (niori-pdf-num w)))
(defun niori-pdf-set-line-join (j) (niori-pdf--emitf "%d j\n" j))
(defun niori-pdf-set-line-cap (c) (niori-pdf--emitf "%d J\n" c))
(defun niori-pdf-set-miter-limit (m) (niori-pdf--emitf "%s M\n" (niori-pdf-num m)))
(defun niori-pdf-set-dash-pattern (dash phase)
  "Set line dash pattern: DASH is a list of lengths (or nil for solid)."
  (if dash
      (niori-pdf--emitf "[ %s ] %s d\n"
                        (mapconcat #'niori-pdf-num dash " ")
                        (niori-pdf-num phase))
    (niori-pdf--emitf "[] 0 d\n")))

;;;; Colors

(defun niori-pdf-set-rgb-fill (r g b)
  "Set the fill color (RGB, components in 0..1)."
  (niori-pdf--emitf "%s %s %s rg\n"
                    (niori-pdf-num r) (niori-pdf-num g) (niori-pdf-num b)))

(defun niori-pdf-set-rgb-stroke (r g b)
  "Set the stroke color (RGB, components in 0..1)."
  (niori-pdf--emitf "%s %s %s RG\n"
                    (niori-pdf-num r) (niori-pdf-num g) (niori-pdf-num b)))

(defun niori-pdf-set-gray-fill (g) (niori-pdf--emitf "%s g\n" (niori-pdf-num g)))
(defun niori-pdf-set-gray-stroke (g) (niori-pdf--emitf "%s G\n" (niori-pdf-num g)))

(defun niori-pdf-set-cmyk-fill (c m y k)
  "Set the fill color in DeviceCMYK.
DeviceCMYK requires a CMYK OutputIntent; PDF/A documents carry an
sRGB OutputIntent, so the call is rejected there."
  (niori-pdf--pdfa-no-cmyk)
  (niori-pdf--emitf "%s %s %s %s k\n"
                    (niori-pdf-num c) (niori-pdf-num m)
                    (niori-pdf-num y) (niori-pdf-num k)))

(defun niori-pdf-set-cmyk-stroke (c m y k)
  "Set the stroke color in DeviceCMYK (see `niori-pdf-set-cmyk-fill')."
  (niori-pdf--pdfa-no-cmyk)
  (niori-pdf--emitf "%s %s %s %s K\n"
                    (niori-pdf-num c) (niori-pdf-num m)
                    (niori-pdf-num y) (niori-pdf-num k)))

(defconst niori-pdf-colors
  '(("red" 1 0 0) ("green" 0 1 0) ("blue" 0 0 1)
    ("black" 0 0 0) ("white" 1 1 1) ("gray" 0.5 0.5 0.5)
    ("yellow" 1 1 0) ("magenta" 1 0 1) ("cyan" 0 1 1)
    ("orange" 1 0.549 0) ("purple" 0.627 0.125 0.941)
    ("brown" 0.647 0.165 0.165) ("pink" 1 0.753 0.796)
    ("darkgray" 0.66 0.66 0.66) ("lightgray" 0.83 0.83 0.83)
    ("darkblue" 0 0 0.545) ("darkgreen" 0 0.392 0)
    ("lightblue" 0.678 0.847 0.902) ("lightgreen" 0.741 0.718 0.42)
    ("navy" 0 0 0.502) ("maroon" 0.502 0 0.502))
  "A small named color table (subset of cl-pdf's x11-colors).")

(defun niori-pdf-color (spec)
  "Return the RGB triple (R G B) (0..1) for color SPEC.
SPEC may be
  - a name from `niori-pdf-colors' (case-insensitive),
  - a \"#RRGGBB\" (or \"#RGB\") hex string,
  - a 24-bit integer #xRRGGBB,
  - a list or vector of (R G B) components in 0..1 (returned normalized)."
  (cond
   ((stringp spec)
    (if (and (> (length spec) 1) (= (aref spec 0) ?#))
        (let ((v (string-to-number (substring spec 1) 16)))
          (if (> (length spec) 4)
              (list (/ (logand v #xFF0000) 16711680.0)
                    (/ (logand v #xFF00) 65280.0)
                    (/ (logand v #xFF) 255.0))
            (let* ((r (logand (ash v -8) #xF))
                   (g (logand (ash v -4) #xF))
                   (b (logand v #xF)))
              (list (/ (+ (* r 16) r) 255.0)
                    (/ (+ (* g 16) g) 255.0)
                    (/ (+ (* b 16) b) 255.0)))))
      (or (cdr (assoc (downcase spec) niori-pdf-colors))
          (error "niori-pdf: unknown color %S" spec))))
   ((integerp spec)
    (list (/ (logand spec #xFF0000) 16711680.0)
          (/ (logand spec #xFF00) 65280.0)
          (/ (logand spec #xFF) 255.0)))
   ((vectorp spec) (append spec nil))
   ((proper-list-p spec) spec)
   (t (error "niori-pdf: bad color spec %S" spec))))

(defun niori-pdf-set-color-fill (spec)
  "Set the fill color to SPEC (see `niori-pdf-color')."
  (apply #'niori-pdf-set-rgb-fill (niori-pdf-color spec)))

(defun niori-pdf-set-color-stroke (spec)
  "Set the stroke color to SPEC (see `niori-pdf-color')."
  (apply #'niori-pdf-set-rgb-stroke (niori-pdf-color spec)))

;;;; ExtGState transparency

(defconst niori-pdf--blend-modes
  '(Normal Multiply Screen Overlay Darken Lighten ColorDodge ColorBurn
    HardLight SoftLight Difference Exclusion Hue Saturation Color
    Luminosity)
  "The 16 PDF 1.4 blend modes, as in cl-pdf's set-transparency.")

(defun niori-pdf--extgstate (doc key dict)
  "Register an ExtGState DICT under resource KEY in DOC."
  (let ((ref (niori-pdf-add-object doc dict)))
    (push (cons key ref) (niori-pdf-document-extgstates doc))
    key))

(defun niori-pdf--alpha-gstate (doc fill-alpha stroke-alpha blend-mode)
  "Return the resource name of an ExtGState for DOC with FILL-ALPHA,
STROKE-ALPHA and optional BLEND-MODE.  Identical states are shared
within a document (cl-pdf's gstate-object cache)."
  (let* ((cache-key (list fill-alpha stroke-alpha blend-mode))
         (hit (cdr (assoc cache-key
                          (niori-pdf-document-gstate-cache doc)
                          #'equal))))
    (or hit
        (let* ((key (format "GSa%d"
                            (1+ (length (niori-pdf-document-extgstates doc)))))
               (alist (list (cons 'Type 'ExtGState)
                            (cons 'CA (niori-pdf-num stroke-alpha))
                            (cons 'ca (niori-pdf-num fill-alpha))))
               (alist (if blend-mode
                          (progn
                            (unless (memq blend-mode niori-pdf--blend-modes)
                              (error "niori-pdf: unknown blend mode %S"
                                     blend-mode))
                            (append alist (list (cons 'BM blend-mode))))
                        alist)))
          (niori-pdf--extgstate doc key (niori-pdf-dict-from-alist alist))
          (push (list cache-key key) (niori-pdf-document-gstate-cache doc))
          key))))

(defun niori-pdf-set-fill-alpha (alpha &optional blend-mode)
  "Set the fill opacity ALPHA (0..1), optionally with BLEND-MODE."
  (let ((key (niori-pdf--alpha-gstate niori-pdf--doc alpha 1.0 blend-mode)))
    (niori-pdf--emitf "/%s gs\n" key)))

(defun niori-pdf-set-stroke-alpha (alpha &optional blend-mode)
  "Set the stroke opacity ALPHA (0..1), optionally with BLEND-MODE."
  (let ((key (niori-pdf--alpha-gstate niori-pdf--doc 1.0 alpha blend-mode)))
    (niori-pdf--emitf "/%s gs\n" key)))

(defun niori-pdf-set-transparency (alpha &optional blend-mode fill-stroke)
  "Set the opacity of both fill and stroke to ALPHA (0..1) at once,
like cl-pdf's set-transparency.  BLEND-MODE is an optional symbol
from `niori-pdf--blend-modes'; FILL-STROKE may be :fill, :stroke or
:both (default) to restrict which alphas are set."
  (let ((fa (if (eq fill-stroke :stroke) 1.0 alpha))
        (sa (if (eq fill-stroke :fill) 1.0 alpha)))
    (niori-pdf--emitf "/%s gs\n"
                      (niori-pdf--alpha-gstate
                       niori-pdf--doc fa sa blend-mode))))

;;;; Transformations

(defun niori-pdf-transform (a b c d e f)
  "Concatenate matrix (A B C D E F) to the CTM (cm)."
  (niori-pdf--emitf "%s %s %s %s %s %s cm\n"
                    (niori-pdf-num a) (niori-pdf-num b) (niori-pdf-num c)
                    (niori-pdf-num d) (niori-pdf-num e) (niori-pdf-num f)))

(defun niori-pdf-translate (dx dy)
  (niori-pdf-transform 1 0 0 1 dx dy))

(defun niori-pdf-scale (sx sy)
  (niori-pdf-transform sx 0 0 sy 0 0))

(defun niori-pdf-rotate (degrees)
  (let ((c (cos (* degrees 3.141592653589793 (/ 1 180.0))))
        (s (sin (* degrees 3.141592653589793 (/ 1 180.0)))))
    (niori-pdf-transform c s (- s) c 0 0)))

(defun niori-pdf-skew (ax ay)
  (let ((tx (tan (* ax 3.141592653589793 (/ 1 180.0))))
        (ty (tan (* ay 3.141592653589793 (/ 1 180.0)))))
    (niori-pdf-transform 1 tx ty 1 0 0)))

(defun niori-pdf-rotate* (radians)
  "Like `niori-pdf-rotate' but takes RADIANS (cl-pdf's rotate*)."
  (niori-pdf-transform (cos radians) (sin radians)
                       (- (sin radians)) (cos radians) 0 0))

(defun niori-pdf-skew* (ax ay)
  "Like `niori-pdf-skew' but takes RADIANS (cl-pdf's skew*)."
  (niori-pdf-transform 1 (tan ax) (tan ay) 1 0 0))

;;;; Shapes (cl-pdf pdf-geom.lisp)

(defconst niori-pdf--pi 3.141592653589793)

(defun niori-pdf-arc-to (cx cy radius start extent)
  "Append an arc of RADIUS around (CX CY) from START radians sweeping
EXTENT radians, as Bezier segments (cl-pdf's arc-to, radians)."
  (unless (zerop extent)
    (if (<= (abs extent) (/ niori-pdf--pi 2.0))
        (let* ((seg extent)
               (k (* (/ 4.0 3.0) (tan (/ seg 4.0))))
               (a0 start)
               (a1 (+ start extent)))
          (niori-pdf-curve-to
           (+ cx (* radius (- (cos a0) (* k (sin a0)))))
           (+ cy (* radius (+ (sin a0) (* k (cos a0)))))
           (+ cx (* radius (+ (cos a1) (* k (sin a1)))))
           (+ cy (* radius (- (sin a1) (* k (cos a1)))))
           (+ cx (* radius (cos a1)))
           (+ cy (* radius (sin a1)))))
      (let ((half (/ extent 2.0)))
        (niori-pdf-arc-to cx cy radius start half)
        (niori-pdf-arc-to cx cy radius (+ start half) half)))))

(defun niori-pdf-pie (cx cy radius start extent)
  "Append a pie-slice path (arc closed back to the center) around
(CX CY) with RADIUS; START and EXTENT are radians.  Paint the path
afterwards with `niori-pdf-fill-stroke' or similar (cl-pdf's pie)."
  (niori-pdf-move-to cx cy)
  (niori-pdf-line-to (+ cx (* radius (cos start)))
                     (+ cy (* radius (sin start))))
  (niori-pdf-arc-to cx cy radius start extent)
  (niori-pdf-line-to cx cy)
  (niori-pdf-close-path))

(defun niori-pdf-regular-polygon (cx cy radius sides &optional angle)
  "Append a closed regular polygon with SIDES vertices around (CX CY)
with RADIUS (cl-pdf's regular-polygon).  ANGLE (radians) rotates the
first vertex; the default pi/2 puts a vertex straight up.  Paint the
path afterwards."
  (let ((step (/ (* 2 niori-pdf--pi) sides))
        (angle (or angle (/ niori-pdf--pi 2.0)))
        (pts nil))
    (dotimes (i sides)
      (push (list (+ cx (* radius (cos (+ angle (* i step)))))
                  (+ cy (* radius (sin (+ angle (* i step))))))
            pts))
    (niori-pdf-polyline (nreverse pts) t)))

(defun niori-pdf-star (cx cy ext-radius int-radius sides &optional angle)
  "Append a closed star path around (CX CY) with SIDES points, outer
EXT-RADIUS and inner INT-RADIUS (cl-pdf's star).  Paint the path
afterwards."
  (let ((step (/ (* 2 niori-pdf--pi) sides))
        (half (/ step 2.0))
        (angle (or angle (/ niori-pdf--pi 2.0)))
        (pts nil))
    (dotimes (i sides)
      (let ((a (+ angle (* i step))))
        (push (list (+ cx (* ext-radius (cos a)))
                    (+ cy (* ext-radius (sin a)))) pts)
        (push (list (+ cx (* int-radius (cos (+ a half))))
                    (+ cy (* int-radius (sin (+ a half))))) pts)))
    (niori-pdf-polyline (nreverse pts) t)))

(defun niori-pdf--pt-angle (p1 p2)
  "Polar angle of the vector P1 -> P2, radians (cl-pdf's angle)."
  (atan (- (nth 1 p2) (nth 1 p1)) (- (nth 0 p2) (nth 0 p1))))

(defun niori-pdf--pt-distance (p1 p2)
  "Euclidean distance between points P1 and P2 (cl-pdf's distance)."
  (sqrt (+ (expt (- (nth 0 p2) (nth 0 p1)) 2)
           (expt (- (nth 1 p2) (nth 1 p1)) 2))))

(defun niori-pdf--pt-midpoint (p1 p2 ratio)
  "Point at RATIO along the segment P1 -> P2 (cl-pdf's midpoint)."
  (list (+ (nth 0 p1) (* ratio (- (nth 0 p2) (nth 0 p1))))
        (+ (nth 1 p1) (* ratio (- (nth 1 p2) (nth 1 p1))))))

(defun niori-pdf-fillet (p1 p2 p3 radius)
  "Append the rounded-corner arc for the corner at P1 between the
segments P3-P1 and P1-P2, RADIUS points wide (cl-pdf's fillet).
Assumes the current point is somewhere on the P2 side before P1;
call it between `niori-pdf-line-to' calls along a polyline."
  (let* ((gamma (/ (abs (- (niori-pdf--pt-angle p1 p3)
                           (niori-pdf--pt-angle p1 p2))) 2))
         (dist-p1-t (/ radius (tan gamma)))
         (dist-p1-s (/ (sqrt (+ (* radius radius) (* dist-p1-t dist-p1-t)))
                       (cos gamma)))
         (dist-p1-p2 (niori-pdf--pt-distance p1 p2))
         (dist-p1-p3 (niori-pdf--pt-distance p1 p3)))
    (if (or (< dist-p1-p2 dist-p1-t) (< dist-p1-p3 dist-p1-t))
        ;; radius too large for the segments: just touch the corner
        (niori-pdf-line-to (nth 0 p1) (nth 1 p1))
      (let ((t2 (niori-pdf--pt-midpoint p1 p2 (/ dist-p1-t dist-p1-p2)))
            (t3 (niori-pdf--pt-midpoint p1 p3 (/ dist-p1-t dist-p1-p3)))
            (center (niori-pdf--pt-midpoint
                     (niori-pdf--pt-midpoint p1 p2 (/ dist-p1-s dist-p1-p2))
                     (niori-pdf--pt-midpoint p1 p3 (/ dist-p1-s dist-p1-p3))
                     0.5)))
        (niori-pdf-line-to (nth 0 t2) (nth 1 t2))
        (niori-pdf-arc-to (nth 0 center) (nth 1 center) radius
                          (niori-pdf--pt-angle center t2)
                          (- (niori-pdf--pt-angle center t3)
                             (niori-pdf--pt-angle center t2)))))))

(defun niori-pdf-rounded-rectangle (x y dx dy radius)
  "Append a rectangle at (X Y) size DX x DY with RADIUS-rounded
corners (cl-pdf's rectangle :radius).  Paint the path afterwards."
  (let ((r (min radius (/ dx 2.0) (/ dy 2.0))))
    (niori-pdf-move-to (+ x r) y)
    (niori-pdf-line-to (+ x dx (- r)) y)
    (niori-pdf-arc-to (+ x dx (- r)) (+ y r) r (* 1.5 niori-pdf--pi) (/ niori-pdf--pi 2.0))
    (niori-pdf-line-to (+ x dx) (+ y dy (- r)))
    (niori-pdf-arc-to (+ x dx (- r)) (+ y dy (- r)) r 0 (/ niori-pdf--pi 2.0))
    (niori-pdf-line-to (+ x r) (+ y dy))
    (niori-pdf-arc-to (+ x r) (+ y dy (- r)) r (* 0.5 niori-pdf--pi) (/ niori-pdf--pi 2.0))
    (niori-pdf-line-to x (+ y r))
    (niori-pdf-arc-to (+ x r) (+ y r) r niori-pdf--pi (/ niori-pdf--pi 2.0))
    (niori-pdf-close-path)))

;;;; Images

(declare-function niori-pdf-images--finalize "niori-pdf-images" (doc))

(defun niori-pdf-draw-image (image x y &optional dx dy rotation keep-aspect-ratio)
  "Draw IMAGE (an `niori-pdf-image') with lower-left corner at (X Y).
DX/DY default to the image's natural size in points (1 px = 1 pt).
ROTATION (degrees) rotates the image around its own center; with
KEEP-ASPECT-RATIO non-nil the image is scaled uniformly to fit the
DX x DY box and centered inside it (cl-pdf's draw-image options)."
  (let ((doc niori-pdf--doc)
        name)
    (unless (cl-assoc image (niori-pdf-document-images doc) :test #'eq)
      (let ((n (format "Im%d" (1+ (niori-pdf-document-image-count doc)))))
        (cl-incf (niori-pdf-document-image-count doc))
        (setf (niori-pdf-image-resource image) n)
        (push (cons image image) (niori-pdf-document-images doc))
        (unless (member #'niori-pdf-images--finalize
                        (niori-pdf-document-save-hooks doc))
          (push #'niori-pdf-images--finalize
                (niori-pdf-document-save-hooks doc)))))
    (setq name (niori-pdf-image-resource image))
    (let* ((dx (or dx (niori-pdf-image-width image)))
           (dy (or dy (niori-pdf-image-height image)))
           (iw (float (niori-pdf-image-width image)))
           (ih (float (niori-pdf-image-height image)))
           ;; with keep-aspect-ratio: uniform scale to fit, centered
           (s (if keep-aspect-ratio
                  (min (/ dx iw) (/ dy ih))
                1.0))
           (sx (* dx s))
           (sy (* dy s))
           (ox (if keep-aspect-ratio (/ (- dx sx) 2.0) 0.0))
           (oy (if keep-aspect-ratio (/ (- dy sy) 2.0) 0.0)))
      (niori-pdf-with-saved-state
       (niori-pdf-translate (+ x ox (/ sx 2.0)) (+ y oy (/ sy 2.0)))
       (when (and rotation (/= rotation 0))
         (niori-pdf-rotate rotation))
       (niori-pdf-translate (- (/ sx 2.0)) (- (/ sy 2.0)))
       (niori-pdf-scale sx sy)
       (niori-pdf--emitf "/%s Do\n" name)))))

;;;; Link annotations

(defun niori-pdf-add-uri-link (x y dx dy uri)
  "Add a rectangle hyperlink to URI."
  (let ((ann (niori-pdf-dict-from-alist
              (list (cons 'Type 'Annot)
                    (cons 'Subtype 'Link)
                    (cons 'Rect (niori-pdf-array x y (+ x dx) (+ y dy)))
                    (cons 'Border (niori-pdf-array 0 0 0))
                    (cons 'A
                          (niori-pdf-dict-from-alist
                           (list (cons 'Type 'Action)
                                 (cons 'S 'URI)
                                 (cons 'URI uri))))))))
    (push ann (niori-pdf-page-annots niori-pdf--page))
    ann))

(defun niori-pdf--escape-pdf-string (s)
  "Escape S for use inside a literal PDF string: backslash and parens."
  (let ((out (string-replace "\\" "\\\\" s)))
    (setq out (string-replace "(" "\\(" out))
    (setq out (string-replace ")" "\\)" out))
    out))

(defun niori-pdf-add-external-link (x y dx dy file page-number)
  "Add a rectangle GoToR link to remote FILE, 1-based PAGE-NUMBER
(cl-pdf's add-external-link).  PAGE-NUMBER nil or 0 means the whole
remote document (/Fit)."
  (let* ((dest (if (and page-number (>= page-number 1))
                   (niori-pdf-array page-number 'Fit)
                 (niori-pdf-array "Fit")))
         (ann (niori-pdf-dict-from-alist
               (list (cons 'Type 'Annot)
                     (cons 'Subtype 'Link)
                     (cons 'Rect (niori-pdf-array x y (+ x dx) (+ y dy)))
                     (cons 'Border (niori-pdf-array 0 0 0))
                     (cons 'A
                           (niori-pdf-dict-from-alist
                            (list (cons 'S 'GoToR)
                                  (cons 'F (niori-pdf-raw
                                            (format "(%s)"
                                                    (niori-pdf--escape-pdf-string
                                                     file))))
                                  (cons 'D dest))))))))
    (push ann (niori-pdf-page-annots niori-pdf--page))
    ann))

(defun niori-pdf-add-link (x y dx dy page)
  "Add a rectangle link to PAGE (a `niori-pdf-page')."
  (let* ((page-num (niori-pdf-page-obj-num page))
         (dest (if page-num
                   (niori-pdf-array (niori-pdf-ref page-num) 'XYZ 0 nil nil)
                 nil)))
    (unless dest
      (error "niori-pdf: link target page not yet created"))
    (push (niori-pdf-dict-from-alist
           (list (cons 'Type 'Annot)
                 (cons 'Subtype 'Link)
                 (cons 'Rect (niori-pdf-array x y (+ x dx) (+ y dy)))
                 (cons 'Border (niori-pdf-array 0 0 0))
                 (cons 'Dest dest)))
          (niori-pdf-page-annots niori-pdf--page))))

(defun niori-pdf-add-deferred-link (x y dx dy page-number &optional top)
  "Add a rectangle link annotation to 1-based PAGE-NUMBER.
The target page object may not exist yet (a table of contents links
to pages created later); the /Dest array is filled in when the
document is saved.  TOP is the destination y in PDF coordinates;
nil keeps the viewer's current position on the target page.  Requires
the dynamic `niori-pdf--doc' and `niori-pdf--page' context."
  (unless (and (boundp 'niori-pdf--doc) niori-pdf--doc
               (boundp 'niori-pdf--page) niori-pdf--page)
    (error "niori-pdf: deferred link outside a page context"))
  (let ((annot (niori-pdf-dict-from-alist
                (list (cons 'Type 'Annot)
                      (cons 'Subtype 'Link)
                      (cons 'Rect (niori-pdf-array x y (+ x dx) (+ y dy)))
                      (cons 'Border (niori-pdf-array 0 0 0))))))
    (push (list annot page-number top)
          (niori-pdf-document-pending-links niori-pdf--doc))
    (push annot (niori-pdf-page-annots niori-pdf--page))
    annot))

(provide 'niori-pdf-content)
;;; niori-pdf-content.el ends here
