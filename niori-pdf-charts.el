;;; niori-pdf-charts.el --- Basic charts for niori-pdf (port of cl-pdf chart.lisp) -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2009 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; This file contains code ported from CL-PDF's chart.lisp.  The
;; original source acknowledges Carlos Ungil for the plot-xy work.
;; See LICENSE and THIRD-PARTY-NOTICES.

;;; Commentary:

;; Basic charts for niori-pdf: histogram (grouped or stacked), pie chart
;; and xy plot, ported from the alpha-quality chart.lisp module of
;; cl-pdf (thanks to Carlos Ungil for the original plot-xy).
;;
;; The CLOS classes of chart.lisp (chart-item, axis, value-axis,
;; histo-axis, legend, histogram, pie-chart, plot-xy) are replaced by
;; plain functions taking keyword arguments; the algorithmic core --
;; `niori-pdf-chart-nice-number' scale computation, tick layout, axis
;; drawing, legend layout and the bar/pie/xy drawing -- is a faithful
;; translation.  All chart functions must be called inside
;; `niori-pdf-with-page'; they draw into the current page's content
;; stream using PDF coordinates (origin bottom-left, y up, like
;; cl-pdf) and return (list WIDTH HEIGHT), the size of the chart box
;; that was drawn.
;;
;; Example:
;;
;;   (niori-pdf-with-document (:title "Charts")
;;     (niori-pdf-with-page ()
;;       (niori-pdf-chart-histogram
;;        50 400 300 200 '((1 2 3) (2 3 1)) '("a" "b" "c")
;;        '(("one" "steelblue") ("two" "indianred"))
;;        :title "Grouped histogram")))
;;
;; Colors are RGB lists (components 0..1) or X11 color name strings;
;; LABELS-COLORS entries have the form (LABEL COLOR).
;;
;; Deviations from cl-pdf chart.lisp:
;; * Chart titles are actually rendered (chart.lisp stores title slots
;;   but never draws them), centered above the chart box in 12pt
;;   Helvetica-Bold.
;; * Axis tick labels that do not fit into their slot are truncated
;;   with "..." (see `niori-pdf-charts--fit-text'); cl-pdf wraps them
;;   onto several lines instead.
;; * plot-xy draws its series even when LABELS-COLORS is nil (cl-pdf
;;   silently skips all series in that case); the default palette is
;;   used then, and no legend is drawn.
;; * Pie slices start at 12 o'clock and proceed clockwise, instead of
;;   cl-pdf's 3 o'clock counter-clockwise convention.
;; * The five default palette colors are looked up via
;;   `niori-pdf-color', backed by a small local X11 table
;;   (`niori-pdf-charts--x11-colors') because `niori-pdf-colors' in
;;   niori-pdf-content.el does not know these names yet.
;; * The wedge path of a pie slice uses `niori-pdf-arc', which takes
;;   degrees (not radians as in cl-pdf); radians are converted here.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-content)
(require 'niori-pdf-fonts)

;;;; Defaults and colors

(defconst niori-pdf-charts--default-palette
  '("steelblue" "indianred" "seagreen" "darkorchid" "goldenrod")
  "Default series palette (X11 color names).")

(defconst niori-pdf-charts--x11-colors
  '(("steelblue" 0.2745 0.5098 0.7059)
    ("indianred" 0.8039 0.3608 0.3608)
    ("seagreen" 0.1804 0.5451 0.3412)
    ("darkorchid" 0.6 0.1961 0.8)
    ("goldenrod" 0.8549 0.6471 0.1255))
  "RGB triples (0..1) for the default palette names.
These X11 colors are missing from `niori-pdf-colors' in
niori-pdf-content.el (change reported to that module); once they are
added there this table becomes redundant.")

(defun niori-pdf-charts--color (name)
  "Return the RGB triple for X11 color NAME.
`niori-pdf-color' is tried first, then the local X11 fallback table."
  (or (ignore-errors (niori-pdf-color name))
      (cdr (assoc (downcase name) niori-pdf-charts--x11-colors))
      (error "niori-pdf-charts: unknown color %S" name)))

(defun niori-pdf-charts--as-rgb (color)
  "Return an RGB list for COLOR (an RGB list or a color name string)."
  (cond ((and (consp color) (numberp (car color))) color)
        ((stringp color) (niori-pdf-charts--color color))
        (t '(0 0 0))))

(defun niori-pdf-charts--font (font)
  "Resolve FONT (a `niori-pdf-font', a font name string, or nil) to a
font object of the current document.  nil means \"Helvetica\" (the
cl-pdf chart default)."
  (cond ((niori-pdf-font-p font) font)
        ((stringp font) (niori-pdf-get-font niori-pdf--doc font))
        (t (niori-pdf-get-font niori-pdf--doc "Helvetica"))))

(defun niori-pdf-charts--resolve-labels-colors (labels-colors n)
  "Normalize LABELS-COLORS into a list of N (LABEL RGB) entries.
Missing entries fall back to \"series I\" labels and the default
palette; entries may be (LABEL COLOR) with COLOR an RGB list or a
color name string."
  (let ((palette niori-pdf-charts--default-palette)
        (out nil))
    (dotimes (i n)
      (let* ((entry (nth i labels-colors))
             (label (cond ((and (consp entry) (car entry)))
                          ((stringp entry) entry)
                          (t (format "series %d" (1+ i)))))
             (color (and (consp entry) (cadr entry))))
        (push (list label
                    (cond ((and (consp color) (numberp (car color))) color)
                          ((stringp color) (niori-pdf-charts--color color))
                          (t (niori-pdf-charts--color
                              (nth (mod i (length palette)) palette)))))
              out)))
    (nreverse out)))

(defun niori-pdf-charts--fit-text (string font size max-width)
  "Truncate STRING with a trailing \"...\" so that it fits MAX-WIDTH
points when drawn with FONT at SIZE (empty string if nothing fits)."
  (if (<= (niori-pdf-text-width string font size) max-width)
      string
    (let ((n (1- (length string))))
      (while (and (> n 0)
                  (> (niori-pdf-text-width
                      (concat (substring string 0 n) "...") font size)
                     max-width))
        (setq n (1- n)))
      (if (> (niori-pdf-text-width "..." font size) max-width)
          ""
        (concat (substring string 0 n) "...")))))

;;;; Scale computation (port of nice-number / compute-scale)

(defun niori-pdf-chart-nice-number (n approx integer-p)
  "Port of cl-pdf's `nice-number': round N to a \"nice\" value.
When APPROX is non-nil the coarser 1/2/5/10 sequence is used, else the
exact 1/2/5/10 sequence of the cl-pdf algorithm.  INTEGER-P rounds the
result to an integer of at least 1."
  (let* ((n10 (expt 10 (floor (log n 10))))
         (nf (/ n n10))
         (value (cl-loop for (mx val) in
                         (if approx
                             '((1.5 1) (3 2) (7 5) (15 10))
                           '((1 1) (2 2) (5 5) (15 10)))
                         when (<= nf mx)
                         return (* val n10))))
    (if integer-p
        (max 1 (round value))
      value)))

(defun niori-pdf-chart-compute-scale (min max size nb-ticks &optional integer-p)
  "Port of cl-pdf's `compute-scale' for a value axis spanning data
range MIN..MAX mapped onto SIZE points with about NB-TICKS ticks.
Returns a plist:

  :tick-value        the step between two ticks (a \"nice\" number)
  :axis-min/:axis-max  the range rounded outward to tick multiples
  :nb-ticks          the actual number of ticks (>= the initial one)
  :scale             SIZE / (:axis-max - :axis-min)
  :ticks-separation  SIZE / (:nb-ticks - 1)
  :format-fn         formatter lambda (n) -> string; \"round to
                     integer\" style when INTEGER-P, else fixed
                     decimals according to :nfrac
  :nfrac             number of decimal digits to display
  :ticks-positions   list of the :nb-ticks tick offsets in 0..SIZE"
  (let* ((min (float min))
         (max (float max))
         (size (float size))
         nfrac d)
    (when (= min max)
      (if (zerop min)
          (setq max 1.0)
        (if (> max 0)
            (setq min 0.0 max (* max 1.01))
          (setq min (* min 1.01) max 0.0))))
    (setq d (niori-pdf-chart-nice-number
             (/ (niori-pdf-chart-nice-number (- max min) nil integer-p)
                (float nb-ticks))
             t integer-p))
    (setq nfrac (max (- (floor (log d 10))) 0))
    (let* ((axis-min (* d (floor min d)))
           (axis-max (* d (ceiling max d)))
           (nb (1+ (ceiling (- axis-max axis-min) d)))
           (axis-scale (/ size (- axis-max axis-min)))
           (ticks-sep (/ size (float (1- nb))))
           (format-fn (if integer-p
                          (lambda (n) (format "%d" (round n)))
                        (let ((fmt (format "%%.%df" nfrac)))
                          (lambda (n) (format fmt n)))))
           (ticks nil))
      (let ((pos 0.0) (i 0))
        (while (< i nb)
          (push pos ticks)
          (setq pos (+ pos ticks-sep)
                i (1+ i))))
      (list :tick-value d
            :axis-min axis-min
            :axis-max axis-max
            :nb-ticks nb
            :scale axis-scale
            :ticks-separation ticks-sep
            :format-fn format-fn
            :nfrac nfrac
            :ticks-positions (nreverse ticks)))))

;;;; Axis drawing

(cl-defun niori-pdf-charts--draw-vertical-value-axis
    (x y height scale &key label-font label-font-size label-color
     line-color line-width tick-length tick-width)
  "Port of cl-pdf's draw-object for `vertical-value-axis'.
Draws the axis line, left-pointing ticks and right-aligned tick
labels ending 1.25*tick-length left of the axis, at scale SCALE
(a `niori-pdf-chart-compute-scale' plist) over HEIGHT points."
  (let* ((fs (or label-font-size 10.0))
         (font-obj (niori-pdf-charts--font label-font))
         (l (- (or tick-length 6)))
         (tw (or tick-width 0.25))
         (sep (plist-get scale :ticks-separation))
         (format-fn (plist-get scale :format-fn))
         (tick-value (plist-get scale :tick-value))
         (nb (plist-get scale :nb-ticks)))
    (niori-pdf-with-saved-state
      (niori-pdf-set-line-width (or line-width 0.5))
      (apply #'niori-pdf-set-rgb-stroke (or line-color '(0 0 0)))
      (niori-pdf-translate x y)
      (niori-pdf-move-to 0 0)
      (niori-pdf-line-to 0 height)
      (niori-pdf-stroke)
      (niori-pdf-set-line-width tw)
      (niori-pdf-move-to 0 0)
      (niori-pdf-line-to l 0)
      (niori-pdf-stroke)
      (apply #'niori-pdf-set-rgb-fill (or label-color '(0 0 0)))
      (let ((tick-y 0.0)
            (text-y (* -0.35 fs))
            (text-x (* l 1.25))
            (value (float (plist-get scale :axis-min)))
            (i 0))
        (while (< i nb)
          (niori-pdf-move-to 0 tick-y)
          (niori-pdf-line-to l tick-y)
          (niori-pdf-stroke)
          (niori-pdf-draw-right-text
           text-x text-y (funcall format-fn value) font-obj fs)
          (setq tick-y (+ tick-y sep)
                text-y (+ text-y sep)
                value (+ value tick-value)
                i (1+ i)))))))

(cl-defun niori-pdf-charts--draw-horizontal-value-axis
    (x y width scale &key label-font label-font-size label-color
     line-color line-width tick-length tick-width)
  "Port of cl-pdf's draw-object for `horizontal-value-axis'.
Draws the baseline, downward ticks and centered tick labels below the
axis at scale SCALE (a `niori-pdf-chart-compute-scale' plist) over
WIDTH points."
  (let* ((fs (or label-font-size 10.0))
         (font-obj (niori-pdf-charts--font label-font))
         (l (- (or tick-length 6)))
         (tw (or tick-width 0.25))
         (sep (plist-get scale :ticks-separation))
         (format-fn (plist-get scale :format-fn))
         (tick-value (plist-get scale :tick-value))
         (nb (plist-get scale :nb-ticks))
         (text-y (+ (* l 1.25) (- fs))))
    (niori-pdf-with-saved-state
      (niori-pdf-set-line-width (or line-width 0.5))
      (apply #'niori-pdf-set-rgb-stroke (or line-color '(0 0 0)))
      (niori-pdf-translate x y)
      (niori-pdf-move-to 0 0)
      (niori-pdf-line-to width 0)
      (niori-pdf-stroke)
      (niori-pdf-set-line-width tw)
      (niori-pdf-move-to 0 0)
      (niori-pdf-line-to 0 l)
      (niori-pdf-stroke)
      (apply #'niori-pdf-set-rgb-fill (or label-color '(0 0 0)))
      (let ((tick-x 0.0)
            (text-x (* -0.35 fs))
            (value (float (plist-get scale :axis-min)))
            (i 0))
        (while (< i nb)
          (niori-pdf-move-to tick-x 0)
          (niori-pdf-line-to tick-x l)
          (niori-pdf-stroke)
          (niori-pdf-draw-centered-text
           text-x text-y
           (niori-pdf-charts--fit-text (funcall format-fn value)
                                       font-obj fs sep)
           font-obj fs)
          (setq tick-x (+ tick-x sep)
                text-x (+ text-x sep)
                value (+ value tick-value)
                i (1+ i)))))))

(cl-defun niori-pdf-charts--draw-horizontal-histo-axis
    (x y width label-names &key label-font label-font-size label-color
     line-color line-width tick-length tick-width)
  "Port of cl-pdf's draw-object for `horizontal-histo-axis'.
Draws the baseline, downward ticks at group boundaries and centered
LABEL-NAMES under each group slot (truncated to fit)."
  (let* ((fs (or label-font-size 10.0))
         (font-obj (niori-pdf-charts--font label-font))
         (l (- (or tick-length 6)))
         (tw (or tick-width 0.25))
         (nb (length label-names))
         (d (if (> nb 0) (/ (float width) nb) (float width)))
         (max-width (- d (* 0.6 fs)))
         (text-y (* -1.25 fs)))
    (niori-pdf-with-saved-state
      (niori-pdf-set-line-width (or line-width 0.5))
      (apply #'niori-pdf-set-rgb-stroke (or line-color '(0 0 0)))
      (niori-pdf-translate x y)
      (niori-pdf-move-to 0 0)
      (niori-pdf-line-to width 0)
      (niori-pdf-stroke)
      (niori-pdf-set-line-width tw)
      (niori-pdf-move-to 0 0)
      (niori-pdf-line-to 0 l)
      (niori-pdf-stroke)
      (apply #'niori-pdf-set-rgb-fill (or label-color '(0 0 0)))
      (when (> nb 0)
        (let ((tx d)
              (text-x (* 0.5 d))
              (name (car label-names))
              (rest (cdr label-names)))
          (while name
            (niori-pdf-move-to tx 0)
            (niori-pdf-line-to tx l)
            (niori-pdf-stroke)
            (niori-pdf-draw-centered-text
             text-x text-y
             (niori-pdf-charts--fit-text name font-obj fs max-width)
             font-obj fs)
            (setq tx (+ tx d)
                  text-x (+ text-x d)
                  name (car rest)
                  rest (cdr rest))))))))

;;;; Legend (port of the cl-pdf legend class)

(cl-defun niori-pdf-chart-legend (x y width height labels-colors
                                  &key font font-size)
  "Draw a legend box for LABELS-COLORS ((LABEL COLOR) ...) entries.
The box's lower-left corner is at (X Y) (X is the box corner like in
cl-pdf; WIDTH is accepted for API compatibility -- cl-pdf computes the
box width from the label texts); the box is vertically centered in the
HEIGHT band starting at Y.  Returns (list BOX-WIDTH BOX-HEIGHT)."
  (ignore width)
  (when labels-colors
    (let* ((fs (or font-size 10.0))
           (font-obj (niori-pdf-charts--font font))
           (space (* 0.2 fs))
           (line-height (+ space fs))
           (entries (mapcar (lambda (lc)
                              (list (car lc)
                                    (niori-pdf-charts--as-rgb (cadr lc))))
                            labels-colors))
           (max-w (apply #'max
                         (mapcar (lambda (e)
                                   (niori-pdf-text-width (car e) font-obj fs))
                                 entries)))
           (box-width (+ (* 3 space) fs max-w))
           (box-height (+ (* (length entries) line-height) space))
           (text-x (+ fs (* 2 space))))
      (niori-pdf-with-saved-state
        (niori-pdf-set-line-width 0.5)
        (niori-pdf-set-rgb-stroke 0 0 0)
        (niori-pdf-set-rgb-fill 1 1 1)
        (niori-pdf-translate x (+ y (* 0.5 (- height box-height))))
        (niori-pdf-rectangle 0 0 box-width box-height)
        (niori-pdf-fill-stroke)
        (niori-pdf-rectangle 0 0 box-width box-height)
        (niori-pdf-clip)
        (cl-loop for (name color) in entries
                 for ly downfrom (- box-height line-height) by line-height
                 do (apply #'niori-pdf-set-rgb-fill color)
                    (niori-pdf-rectangle space ly fs fs)
                    (niori-pdf-fill-stroke)
                    (niori-pdf-set-rgb-fill 0 0 0)
                    (niori-pdf-draw-left-text text-x ly name font-obj fs)))
      (list box-width box-height))))

;;;; Titles (chart.lisp keeps title slots but never draws them)

(defun niori-pdf-charts--draw-title (x y width height title
                                     &optional title-font-size)
  "Draw TITLE centered above the chart box at (X Y) WIDTH x HEIGHT."
  (when (and title (not (string= title "")))
    (let* ((fs (or title-font-size 12.0))
           (font-obj (niori-pdf-charts--font "Helvetica-Bold")))
      (niori-pdf-draw-centered-text
       (+ x (* 0.5 width)) (+ y height (* 0.75 fs)) title font-obj fs))))

;;;; Histogram (port of the cl-pdf histogram class)

(defun niori-pdf-charts--draw-stacked-bars (series resolved y-scale
                                            group-width spacing bar-width)
  "Port of the stacked branch of cl-pdf's histogram draw-object.
SERIES is the list of value lists, RESOLVED the (LABEL RGB) entries."
  (let ((bx (* 0.5 spacing)))
    (dolist (values (apply #'cl-mapcar #'list series))
      (let ((by 0.0)
            (ci 0))
        (while (< ci (length values))
          (let ((dy (* (nth ci values) y-scale)))
            (apply #'niori-pdf-set-rgb-fill
                   (niori-pdf-charts--as-rgb (cadr (nth ci resolved))))
            (niori-pdf-rectangle bx by bar-width dy)
            (niori-pdf-fill-stroke)
            (setq by (+ by dy)
                  ci (1+ ci))))
        (setq bx (+ bx group-width))))))

(defun niori-pdf-charts--draw-grouped-bars (series resolved axis-min
                                            y-scale group-width spacing
                                            bar-width)
  "Port of the grouped branch of cl-pdf's histogram draw-object.
Bars of series I start at 0.5*SPACING + I*BAR-WIDTH inside every
group; their height spans AXIS-MIN (the chart bottom) up to the
value."
  (let ((gx (* 0.5 spacing))
        (si 0)
        (rest series))
    (while rest
      (apply #'niori-pdf-set-rgb-fill
             (niori-pdf-charts--as-rgb (cadr (nth si resolved))))
      (let ((bx gx))
        (dolist (value (car rest))
          (niori-pdf-rectangle
           bx 0 bar-width (* (- value axis-min) y-scale))
          (niori-pdf-fill-stroke)
          (setq bx (+ bx group-width))))
      (setq gx (+ gx bar-width)
            si (1+ si)
            rest (cdr rest)))))

(cl-defun niori-pdf-chart-histogram (x y width height series label-names
                                     labels-colors
                                     &key stacked background-color line-color
                                     h-lines-color x-label-color y-label-color
                                     label-font label-font-size
                                     min-value max-value nb-ticks
                                     title title-font-size legend
                                     &allow-other-keys)
  "Draw a histogram with lower-left corner at (X Y), WIDTH x HEIGHT
points.  SERIES is a list of value lists (one per series);
LABEL-NAMES the names of the value groups; LABELS-COLORS a list of
(LABEL COLOR) per series (default palette when nil).  With STACKED
non-nil the series are stacked per group, else drawn side by side as
groups of bars with 20% group spacing.  MIN-VALUE/MAX-VALUE override
the automatically computed y data range; NB-TICKS (default 10) is
passed to `niori-pdf-chart-compute-scale'.  TITLE (Helvetica-Bold,
TITLE-FONT-SIZE, default 12) is centered above the chart.  A legend
box is drawn right of the chart when there is more than one series or
LEGEND is non-nil.  Returns (list WIDTH HEIGHT)."
  (unless (and (consp series) (cl-every #'consp series))
    (error "niori-pdf-charts: histogram SERIES must be a non-empty list of non-empty lists"))
  (let* ((nb-series (length series))
         (nb-values (length (car series)))
         (resolved (niori-pdf-charts--resolve-labels-colors
                    labels-colors (max nb-series (length labels-colors))))
         ;; y axis data range (cl-pdf initialize-instance :after).
         (data-min (if stacked 0.0
                     (apply #'min (mapcar (lambda (s) (apply #'min s)) series))))
         (data-max (if stacked
                       (apply #'max
                              (mapcar (lambda (group) (apply #'+ group))
                                      (apply #'cl-mapcar #'list series)))
                     (apply #'max (mapcar (lambda (s) (apply #'max s)) series))))
         (scale (niori-pdf-chart-compute-scale
                 (or min-value data-min) (or max-value data-max)
                 height (or nb-ticks 10)))
         (axis-min (plist-get scale :axis-min))
         (y-scale (plist-get scale :scale))
         (group-width (/ (float width) nb-values))
         (spacing (* 0.2 group-width))
         (bar-width (/ (- group-width spacing) (if stacked 1 nb-series)))
         (bg (or background-color '(1 1 1)))
         (lc (or line-color '(0 0 0)))
         (hlc (or h-lines-color '(0.5 0.5 0.5)))
         (font-obj (niori-pdf-charts--font label-font))
         (fs (or label-font-size 10.0)))
    ;; chart body: background, grid lines, bars.
    (niori-pdf-with-saved-state
      (niori-pdf-translate x y)
      (niori-pdf-set-line-width 0.5)
      (apply #'niori-pdf-set-rgb-stroke lc)
      (apply #'niori-pdf-set-rgb-fill bg)
      (niori-pdf-rectangle 0 0 width height)
      (niori-pdf-fill-stroke)
      (niori-pdf-set-line-width 0.2)
      (apply #'niori-pdf-set-rgb-stroke hlc)
      (dolist (tick-y (plist-get scale :ticks-positions))
        (niori-pdf-move-to 0 tick-y)
        (niori-pdf-line-to width tick-y)
        (niori-pdf-stroke))
      (niori-pdf-set-line-width 0.5)
      (apply #'niori-pdf-set-rgb-stroke lc)
      (if stacked
          (niori-pdf-charts--draw-stacked-bars
           series resolved y-scale group-width spacing bar-width)
        (niori-pdf-charts--draw-grouped-bars
         series resolved axis-min y-scale group-width spacing bar-width)))
    ;; axes, legend, title (cl-pdf draw order: x-axis, y-axis, legend).
    (niori-pdf-charts--draw-horizontal-histo-axis
     x y width label-names
     :label-font font-obj :label-font-size fs
     :label-color (or x-label-color '(0 0 0)) :line-color lc)
    (niori-pdf-charts--draw-vertical-value-axis
     x y height scale
     :label-font font-obj :label-font-size fs
     :label-color (or y-label-color '(0 0 0)) :line-color lc)
    (when (or legend (> nb-series 1))
      (niori-pdf-chart-legend (+ x width 10) y 60 height resolved
                              :font font-obj :font-size fs))
    (niori-pdf-charts--draw-title x y width height title title-font-size)
    (list width height)))

;;;; Pie chart (port of the cl-pdf pie-chart class)

(defun niori-pdf-charts--pie-path (cx cy radius start extent)
  "Append a pie-slice wedge path around (CX CY) (port of cl-pdf's
`pie' from pdf-geom.lisp).  START and EXTENT are in radians; the arc
segment is produced by `niori-pdf-arc', which takes degrees, so they
are converted here."
  (niori-pdf-move-to cx cy)
  (niori-pdf-line-to (+ cx (* radius (cos start)))
                     (+ cy (* radius (sin start))))
  (niori-pdf-arc cx cy radius
                 (* start (/ 180.0 float-pi))
                 (* extent (/ 180.0 float-pi)))
  (niori-pdf-line-to cx cy)
  (niori-pdf-close-path))

(cl-defun niori-pdf-chart-pie (x y width height serie labels-colors
                               &key no-legend background-color line-color
                               label-font-size title
                               &allow-other-keys)
  "Draw a pie chart in the box with lower-left corner at (X Y), WIDTH
x HEIGHT points.  SERIE is a list of numbers (one value per slice);
LABELS-COLORS a list of (LABEL COLOR) per slice.  Slices start at 12
o'clock and proceed clockwise.  NO-LEGEND suppresses the legend box
right of the chart; TITLE (Helvetica-Bold, default 12pt) is centered
above.  BACKGROUND-COLOR is accepted for API compatibility but never
painted (cl-pdf fills an empty path here, i.e. it draws nothing).
Returns (list WIDTH HEIGHT)."
  (ignore background-color)
  (unless (and (consp serie) (cl-every #'numberp serie))
    (error "niori-pdf-charts: pie SERIE must be a non-empty list of numbers"))
  (let* ((nb (length serie))
         (resolved (niori-pdf-charts--resolve-labels-colors
                    labels-colors (max nb (length labels-colors))))
         (sum (let ((s (apply #'+ serie))) (if (zerop s) 1 s)))
         (radius (* width 0.45))
         (lc (or line-color '(0 0 0)))
         (fs (or label-font-size 10.0)))
    (niori-pdf-with-saved-state
      (niori-pdf-translate (+ x (* 0.5 width)) (+ y (* 0.5 height)))
      (niori-pdf-set-line-width 0.5)
      (apply #'niori-pdf-set-rgb-stroke lc)
      (let ((start-acum 0.0)
            (si 0)
            (rest serie))
        (while rest
          (let ((angle (/ (* 2.0 float-pi (car rest)) sum)))
            (apply #'niori-pdf-set-rgb-fill
                   (niori-pdf-charts--as-rgb (cadr (nth si resolved))))
            (niori-pdf-charts--pie-path
             0 0 radius (- (/ float-pi 2.0) start-acum) (- angle))
            (niori-pdf-fill-stroke)
            (setq start-acum (+ start-acum angle)
                  si (1+ si)
                  rest (cdr rest))))))
    (unless no-legend
      (niori-pdf-chart-legend (+ x width 10) y 60 height resolved
                              :font-size fs))
    (niori-pdf-charts--draw-title x y width height title)
    (list width height)))

;;;; XY plot (port of the cl-pdf plot-xy class)

(defun niori-pdf-charts--xy-extremes (series idx)
  "Return (MIN MAX) of element IDX (0 = x, 1 = y) of all points in
SERIES.  Points are (X Y) lists/conses; nil elements or nil components
are skipped (data gaps)."
  (let ((vals nil))
    (dolist (serie series)
      (dolist (p serie)
        (let ((v (and (consp p) (nth idx p))))
          (when (numberp v) (push v vals)))))
    (if vals
        (list (apply #'min vals) (apply #'max vals))
      (error "niori-pdf-charts: plot-xy needs at least one numeric point"))))

(cl-defun niori-pdf-chart-plot-xy (x y width height series labels-colors
                                   &key background-color h-lines-color
                                   point-radius x-min x-max y-min y-max
                                   nb-ticks label-font label-font-size title
                                   &allow-other-keys)
  "Draw an xy plot in the box with lower-left corner at (X Y), WIDTH x
HEIGHT points.  SERIES is a list of point lists, each point an (X Y)
list or cons; a nil element or a nil X/Y component splits the polyline
(data gap).  LABELS-COLORS ((LABEL COLOR) per series) enables the
legend right of the chart; series colors come from it, else from the
default palette.  POINT-RADIUS (default 2) is the radius of the
markers; 0 suppresses them.  X-MIN/X-MAX/Y-MIN/Y-MAX override the
automatic data ranges; NB-TICKS (default 10) feeds
`niori-pdf-chart-compute-scale' for both axes.  TITLE is centered
above.  Returns (list WIDTH HEIGHT)."
  (unless (listp series)
    (error "niori-pdf-charts: plot-xy SERIES must be a list of point lists"))
  (let* ((nb-series (length series))
         (resolved (niori-pdf-charts--resolve-labels-colors
                    labels-colors (max nb-series (length labels-colors))))
         (point-radius (or point-radius 2))
         (x-range (niori-pdf-charts--xy-extremes series 0))
         (y-range (niori-pdf-charts--xy-extremes series 1))
         (x-scale (niori-pdf-chart-compute-scale
                   (or x-min (car x-range)) (or x-max (cadr x-range))
                   width (or nb-ticks 10)))
         (y-scale (niori-pdf-chart-compute-scale
                   (or y-min (car y-range)) (or y-max (cadr y-range))
                   height (or nb-ticks 10)))
         (x-min-a (plist-get x-scale :axis-min))
         (y-min-a (plist-get y-scale :axis-min))
         (sx (plist-get x-scale :scale))
         (sy (plist-get y-scale :scale))
         (bg (or background-color '(1 1 1)))
         (hlc (or h-lines-color '(0.5 0.5 0.5)))
         (font-obj (niori-pdf-charts--font label-font))
         (fs (or label-font-size 10.0)))
    (niori-pdf-with-saved-state
      (niori-pdf-translate x y)
      (niori-pdf-set-line-width 0.5)
      (niori-pdf-set-rgb-stroke 0 0 0)
      (apply #'niori-pdf-set-rgb-fill bg)
      (niori-pdf-rectangle 0 0 width height)
      (niori-pdf-fill-stroke)
      (niori-pdf-set-line-width 0.2)
      (apply #'niori-pdf-set-rgb-stroke hlc)
      (dolist (tick-y (plist-get y-scale :ticks-positions))
        (niori-pdf-move-to 0 tick-y)
        (niori-pdf-line-to width tick-y)
        (niori-pdf-stroke))
      (dolist (tick-x (plist-get x-scale :ticks-positions))
        (niori-pdf-move-to tick-x 0)
        (niori-pdf-line-to tick-x height)
        (niori-pdf-stroke))
      (niori-pdf-set-line-width 0.5)
      (niori-pdf-set-line-join 2)
      (let ((si 0)
            (rest series))
        (while rest
          (let ((color (niori-pdf-charts--as-rgb (cadr (nth si resolved)))))
            (apply #'niori-pdf-set-rgb-stroke color)
            (apply #'niori-pdf-set-rgb-fill color)
            (let ((points nil)
                  (all-points nil))
              (dolist (p (car rest))
                (let* ((xx (and (consp p) (car p)))
                       (yy (and (consp p) (cadr p))))
                  (if (and xx yy)
                      (let ((px (* (- xx x-min-a) sx))
                            (py (* (- yy y-min-a) sy)))
                        (setq points (cons (cons px py) points))
                        (unless (zerop point-radius)
                          (niori-pdf-circle px py point-radius)
                          (niori-pdf-fill-stroke)))
                    (when points
                      (setq all-points (cons (nreverse points) all-points)
                            points nil)))))
              (when points
                (setq all-points (cons (nreverse points) all-points)))
              (dolist (seg all-points)
                (niori-pdf-polyline seg))
              (niori-pdf-stroke)))
          (setq si (1+ si)
                rest (cdr rest)))))
    (niori-pdf-charts--draw-vertical-value-axis
     x y height y-scale
     :label-font font-obj :label-font-size fs)
    (niori-pdf-charts--draw-horizontal-value-axis
     x y width x-scale
     :label-font font-obj :label-font-size fs)
    (when labels-colors
      (niori-pdf-chart-legend (+ x width 10) y 60 height resolved
                              :font font-obj :font-size fs))
    (niori-pdf-charts--draw-title x y width height title)
    (list width height)))

(provide 'niori-pdf-charts)
;;; niori-pdf-charts.el ends here
