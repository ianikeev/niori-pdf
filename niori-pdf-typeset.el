;;; niori-pdf-typeset.el --- Box/glue/penalty model + Knuth-Plass breaking -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2021 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; This file ports the CL-TYPESETTING box/glue/penalty model and
;; paragraph-breaking machinery.  See LICENSE for details.

;;; Commentary:

;; Items are horizontal typesetting atoms:
;;
;;   `niori-pdf-ts-box'      (width content height depth) -- a word or
;;                           any fixed-width object; CONTENT is an
;;                           opaque plist understood by the renderer
;;   `niori-pdf-ts-glue'     (width stretch shrink) -- elastic space;
;;                           stretch/shrink orders 0=finite, 1=fil,
;;                           2=fill, 3=filll (see `niori-pdf-ts-hfill')
;;   `niori-pdf-ts-kern'     fixed space, discardable at line ends
;;   `niori-pdf-ts-penalty'  (amount flag width) -- break point; a
;;                           break adds WIDTH to the line (the hyphen)
;;
;; `niori-pdf-ts-inf-penalty' forbids a break, `niori-pdf-ts-forced-penalty'
;; forces one.  Entry points:
;;
;;   `niori-pdf-ts-break-paragraph' -- full TeX three-pass driver,
;;        returns the list of breaks (plists :start :end :ratio
;;        :fitness :badness :width :hyphen-p :demerits) or nil.
;;   `niori-pdf-ts-break-items' -- single-pass Knuth-Plass run.
;;   `niori-pdf-ts-line' -- (ITEMS . WIDTHS) for one break, with glue
;;        elasticity applied; fill glues have width nil ("absorb
;;        remaining space" -- the renderer distributes it).

;;; Code:

(require 'cl-lib)

(cl-defstruct (niori-pdf-ts-box (:constructor niori-pdf-ts-box
                                 (width &optional content height depth)))
  width content height depth)

(cl-defstruct (niori-pdf-ts-glue (:constructor niori-pdf-ts-glue
                                  (width &optional (stretch 0) (shrink 0))))
  width stretch shrink
  ;; stretch/shrink order: 0 = finite, 1 = fil, 2 = fill, 3 = filll
  (stretch-order 0) (shrink-order 0) flag)

(cl-defstruct (niori-pdf-ts-kern (:constructor niori-pdf-ts-kern (amount)))
  amount)

(cl-defstruct (niori-pdf-ts-penalty (:constructor niori-pdf-ts-penalty
                                     (amount &optional flag width)))
  amount flag width)

(defconst niori-pdf-ts-inf-penalty 10000
  "Penalty value meaning \"never break here\".")

(defconst niori-pdf-ts-forced-penalty -10000
  "Penalty value meaning \"forced break\".")

(defun niori-pdf-ts-hfill (&optional order)
  "Fill glue (infinite stretch); ORDER 1 = fil, 2 = fill, 3 = filll."
  (let ((g (niori-pdf-ts-glue 0 1 0)))
    (setf (niori-pdf-ts-glue-stretch-order g) (or order 1))
    g))

(defun niori-pdf-ts-hfil ()
  "First-order fill glue."
  (niori-pdf-ts-hfill 1))

(defun niori-pdf-ts-hfilll ()
  "Third-order fill glue (dominates fil glues)."
  (niori-pdf-ts-hfill 3))

(defun niori-pdf-ts-hspace (width)
  "Fixed, non-elastic horizontal space of WIDTH (not a break point)."
  (let ((g (niori-pdf-ts-glue width 0 0)))
    (setf (niori-pdf-ts-glue-flag g) 'spacing)
    g))

(defun niori-pdf-ts-item-width (item)
  "Natural width of ITEM."
  (pcase (type-of item)
    (`niori-pdf-ts-box (niori-pdf-ts-box-width item))
    (`niori-pdf-ts-glue (niori-pdf-ts-glue-width item))
    (`niori-pdf-ts-kern (niori-pdf-ts-kern-amount item))
    (_ 0)))

(defun niori-pdf-ts-item-stretch (item)
  (if (eq (type-of item) 'niori-pdf-ts-glue)
      (niori-pdf-ts-glue-stretch item)
    0))

(defun niori-pdf-ts-item-shrink (item)
  (if (eq (type-of item) 'niori-pdf-ts-glue)
      (niori-pdf-ts-glue-shrink item)
    0))

(defun niori-pdf-ts-item-penalty-p (item)
  "Whether ITEM is a legal break candidate (penalty < +inf)."
  (and (eq (type-of item) 'niori-pdf-ts-penalty)
       (< (niori-pdf-ts-penalty-amount item) niori-pdf-ts-inf-penalty)))

;;;; Badness and demerits

(defun niori-pdf-ts-badness (delta stretch shrink)
  "Badness of fitting DELTA of elastic space into STRETCH/SHRINK.
DELTA > 0 uses stretch, DELTA < 0 uses shrink.  Caps at 10000
(infeasible / infinitely bad)."
  (cond
   ((< delta (- shrink)) 10000)
   ((< delta 0)
    (if (= shrink 0) 10000
      (min 10000 (floor (* 100.0 (expt (/ (float (- delta)) shrink) 3))))))
   ((> delta 0)
    (if (= stretch 0) 10000
      (min 10000 (floor (* 100.0 (expt (/ (float delta) stretch) 3))))))
   (t 0)))

(defun niori-pdf-ts-fitness (ratio)
  "Fitness class of RATIO: 0 tight, 1 normal, 2 loose, 3 very loose."
  (cond ((< ratio -0.5) 0)
        ((<= ratio 0.5) 1)
        ((<= ratio 1.0) 2)
        (t 3)))

;;;; Knuth-Plass breaking

(cl-defun niori-pdf-ts-break-items
    (items width
           &key
           (tolerance 200)
           (line-penalty 10)
           (adj-demerits 10000)
           (flag-demerits 3000)
           (double-hyphen-demerits 10000)
           (final-hyphen-demerits 5000)
           (emergency-stretch 0)
           (ignore-flagged-breaks nil)
           (deactivate t))
  "Break ITEMS (ending in a forced penalty) into lines of WIDTH.
Returns the list of break plists (:start :pos :ratio :fitness :badness
:width :hyphen-p :demerits), or nil when infeasible within TOLERANCE.
IGNORE-FLAGGED-BREAKS treats flagged penalties (hyphenation points)
as non-breaks -- the first pass of the TeX scheme uses this to try a
paragraph without hyphenation while keeping item indexes identical.
DEACTIVATE nil keeps every active node alive for the whole run (the
last-resort pass uses this so a paragraph always gets some setting,
even when the only solutions contain over-full lines)."
  (when items
    (let* ((vec (vconcat items))
           (n (length vec))
           ;; prefix sums: pw[i] = natural width of items 0..i-1 etc.
           (pw (make-vector (1+ n) 0.0))
           (pstr (make-vector (1+ n) 0.0))
           (pshr (make-vector (1+ n) 0.0))
           ;; number of fill-order glues before i (stretch and shrink)
           (pfil-s (make-vector (1+ n) 0))
           (pfil-r (make-vector (1+ n) 0))
           (act nil))
      (cl-loop for i from 0 below n
               for itm across vec
               do (aset pw (1+ i) (+ (aref pw i) (niori-pdf-ts-item-width itm)))
                  (aset pstr (1+ i) (+ (aref pstr i)
                                       (niori-pdf-ts-item-stretch itm)))
                  (aset pshr (1+ i) (+ (aref pshr i)
                                       (niori-pdf-ts-item-shrink itm)))
                  (when (and (eq (type-of itm) 'niori-pdf-ts-glue)
                             (> (niori-pdf-ts-glue-stretch-order itm) 0))
                    (aset pfil-s (1+ i) (1+ (aref pfil-s i))))
                  (when (and (eq (type-of itm) 'niori-pdf-ts-glue)
                             (> (niori-pdf-ts-glue-shrink-order itm) 0))
                    (aset pfil-r (1+ i) (1+ (aref pfil-r i)))))
      ;; carry the fill counters forward for non-fill items too
      (cl-loop for i from 1 to n
               do (aset pfil-s i (max (aref pfil-s i) (aref pfil-s (1- i))))
                  (aset pfil-r i (max (aref pfil-r i) (aref pfil-r (1- i)))))
      (cl-flet ((try-break (b a)
                ;; try ending a line at index B starting from node A.
                ;; START is the first item of the new line: the item at
                ;; A's break position was the break itself and is
                ;; discarded, so the line begins at A's :ns.
                (let* ((start (plist-get a :ns))
                       (natural (- (aref pw b) (aref pw start)))
                       (stretch (- (aref pstr b) (aref pstr start)))
                       (shrink (- (aref pshr b) (aref pshr start)))
                       (has-fill-s (> (- (aref pfil-s b) (aref pfil-s start)) 0))
                       (has-fill-r (> (- (aref pfil-r b) (aref pfil-r start)) 0))
                       (brk-item (aref vec b))
                       (is-pen (eq (type-of brk-item) 'niori-pdf-ts-penalty))
                       (pen-w (if is-pen
                                  (or (niori-pdf-ts-penalty-width brk-item) 0)
                                0.0))
                       (flag-p (and is-pen
                                    (niori-pdf-ts-penalty-flag brk-item)))
                       (avail (- width pen-w))
                       (delta (- avail natural))
                       (has-fill (or has-fill-s has-fill-r))
                       ;; emergency stretch (pass 3) adds artificial
                       ;; elasticity so a paragraph can always be set
                       (eff-stretch (+ stretch emergency-stretch))
                       ;; A fill glue absorbs POSITIVE slack only
                       ;; (badness 0, TeX's "fil stretch" case); it
                       ;; cannot shrink, so an over-full line is always
                       ;; infinitely bad -- exactly like TeX's
                       ;; overfull boxes -- and must not be chosen by
                       ;; the optimizer (it used to be masked by the
                       ;; paragraph-final parfillskip, producing
                       ;; lines that stick out of the measure).
                       (badness (cond
                                 ((and has-fill (> delta 0)) 0)
                                 (t (niori-pdf-ts-badness
                                     delta eff-stretch shrink))))
                       (ratio (cond ((and has-fill (> delta 0)) 1.0)
                                    ((> delta 0)
                                     (if (> eff-stretch 0)
                                         (/ (float delta) eff-stretch)
                                       2.0))
                                    ((< delta 0)
                                     (if (> shrink 0)
                                         (/ (float delta) shrink)
                                       -2.0))
                                    (t 0.0)))
                       (fit (niori-pdf-ts-fitness ratio))
                       (pen-amount (if is-pen
                                       (niori-pdf-ts-penalty-amount brk-item)
                                     0))
                       (is-final (= b (1- n)))
                       (b2 (+ line-penalty (min badness 10000)))
                       (d (+ (* b2 b2)
                             (if (> pen-amount 0) (* pen-amount pen-amount) 0)
                             (cond (flag-p
                                    (if is-final
                                        final-hyphen-demerits
                                      (if (plist-get a :flagged-p)
                                          double-hyphen-demerits
                                        flag-demerits)))
                                   (t 0))
                             (if (>= (abs (- fit (plist-get a :fitness))) 2)
                                 adj-demerits
                               0))))
                  ;; TeX accepts a breakpoint when badness does not
                  ;; exceed the tolerance (badness is capped at 10000,
                  ;; so tolerance 10000 = "allow overfull boxes" for
                  ;; the emergency pass), or at a forced break.
                  (when (or (<= badness tolerance)
                            (<= pen-amount niori-pdf-ts-forced-penalty))
                    (list :pos b
                          ;; the item at B (the breaking glue or
                          ;; penalty) is consumed by the break; the
                          ;; next line starts after it.  :start is the
                          ;; first item of THIS line.
                          :start start
                          :ns (1+ b)
                          :avail avail
                          :line (1+ (plist-get a :line))
                          :fitness fit
                          :demerits (+ (plist-get a :demerits) d)
                          :ratio ratio
                          :prev a
                          :flagged-p flag-p
                          :hyphen-p (and flag-p (> pen-w 0))
                          :badness badness
                          :width (+ natural pen-w))))))
        ;; initial active node: before item 0
        (setq act (list (list :pos 0 :ns 0 :line 0 :fitness 1 :demerits 0
                              :ratio 0.0 :prev nil :flagged-p nil)))
        (cl-loop for b from 0 below n
                 do (let* ((itm (aref vec b))
                           (ty (type-of itm))
                           (legal
                            (pcase ty
                              (`niori-pdf-ts-glue
                               (and (> b 0)
                                    (not (memq (type-of (aref vec (1- b)))
                                               '(niori-pdf-ts-glue
                                                 niori-pdf-ts-kern)))
                                    (not (eq (niori-pdf-ts-glue-flag itm)
                                             'spacing))))
                              (`niori-pdf-ts-penalty
                               (and (< (niori-pdf-ts-penalty-amount itm)
                                       niori-pdf-ts-inf-penalty)
                                    (not (and ignore-flagged-breaks
                                              (niori-pdf-ts-penalty-flag
                                               itm)))))
                              (_ nil))))
                      (when legal
                        (let ((cands nil)
                              (newact nil))
                          (dolist (a act)
                            (let ((node (try-break b a)))
                              (when node (push node cands))
                              ;; deactivate: when even a fully shrunk
                              ;; line from A to here is too wide, no
                              ;; later break can be valid either
                              (let* ((start (plist-get a :ns))
                                     (nat (- (aref pw b) (aref pw start)))
                                     (shr (- (aref pshr b) (aref pshr start)))
                                     (pen-w (if (eq ty 'niori-pdf-ts-penalty)
                                                (or (niori-pdf-ts-penalty-width
                                                     itm)
                                                    0)
                                              0.0)))
                                (when (or (not deactivate)
                                          (<= (- (+ nat pen-w) shr
                                                 emergency-stretch)
                                              width))
                                  (push a newact)))))
                          (setq act (nreverse newact))
                          (when cands
                            (if (and (eq ty 'niori-pdf-ts-penalty)
                                     (<= (niori-pdf-ts-penalty-amount itm)
                                         niori-pdf-ts-forced-penalty))
                                ;; forced break: continue from the best
                                (setq act
                                      (list (cl-reduce
                                             (lambda (x y)
                                               (if (< (plist-get x :demerits)
                                                      (plist-get y :demerits))
                                                   x y))
                                             (nreverse cands))))
                              ;; keep the best node per fitness class
                              (let ((best (make-vector 4 nil)))
                                (dolist (c (nreverse cands))
                                  (let ((f (plist-get c :fitness)))
                                    (when (or (null (aref best f))
                                              (< (plist-get c :demerits)
                                                 (plist-get (aref best f)
                                                            :demerits)))
                                      (aset best f c))))
                                (setq act (append (delq nil (append best nil))
                                                  act))))
                            (setq act (cl-sort
                                       (copy-sequence act) #'<
                                       :key (lambda (x)
                                              (plist-get x :demerits)))))))))
        ;; the paragraph must end with a forced penalty; pick it up
        (when (and (> n 0)
                   (eq (type-of (aref vec (1- n))) 'niori-pdf-ts-penalty)
                   (<= (niori-pdf-ts-penalty-amount (aref vec (1- n)))
                       niori-pdf-ts-forced-penalty))
          (let ((final
                 (cl-find-if
                  (lambda (a)
                    (= (plist-get a :pos) (1- n)))
                  act)))
            (when final
              (let ((breaks nil)
                    (node final))
                ;; walking the prev chain already yields first-line
                ;; first order (final node pushed first)
                (while node
                  (when (plist-get node :prev)
                    (push node breaks))
                  (setq node (plist-get node :prev)))
                breaks))))))))

(cl-defun niori-pdf-ts-break-paragraph
    (items width &optional _plain-items
           &key
           (pretolerance 100) (tolerance 200)
           (emergency-stretch 12.0)
           (line-penalty 10) (adj-demerits 10000) (flag-demerits 3000)
           (double-hyphen-demerits 10000) (final-hyphen-demerits 5000))
  "TeX three-pass driver over `niori-pdf-ts-break-items'.
The first pass ignores hyphenation breaks (flagged penalties) at
PRETOLERANCE; if that fails, pass two hyphenates at TOLERANCE; the
last resort adds EMERGENCY-STRETCH and, if even that fails (the
paragraph can only be set with over-full lines), a final pass keeps
all active nodes alive so the paragraph always gets a setting.
Returns the break list or nil."
  (or (niori-pdf-ts-break-items items width
                                :tolerance pretolerance
                                :ignore-flagged-breaks t
                                :line-penalty line-penalty
                                :adj-demerits adj-demerits
                                :flag-demerits flag-demerits
                                :double-hyphen-demerits
                                double-hyphen-demerits
                                :final-hyphen-demerits
                                final-hyphen-demerits)
      (niori-pdf-ts-break-items items width
                                :tolerance tolerance
                                :line-penalty line-penalty
                                :adj-demerits adj-demerits
                                :flag-demerits flag-demerits
                                :double-hyphen-demerits
                                double-hyphen-demerits
                                :final-hyphen-demerits
                                final-hyphen-demerits)
      (niori-pdf-ts-break-items items width
                                :tolerance 10000
                                :emergency-stretch emergency-stretch
                                :line-penalty line-penalty
                                :adj-demerits adj-demerits
                                :flag-demerits flag-demerits
                                :double-hyphen-demerits
                                double-hyphen-demerits
                                :final-hyphen-demerits
                                final-hyphen-demerits)
      (niori-pdf-ts-break-items items width
                                :tolerance 10000
                                :deactivate nil
                                :line-penalty line-penalty
                                :adj-demerits adj-demerits
                                :flag-demerits flag-demerits
                                :double-hyphen-demerits
                                double-hyphen-demerits
                                :final-hyphen-demerits
                                final-hyphen-demerits)))

(cl-defun niori-pdf-ts-line (items brk &key (justify t))
  "Return the renderable line for ITEMS and break BRK.
Value: (ITEMS . WIDTHS): the line's atoms and per-atom widths with
glue elasticity applied.  Fill glues get width nil and the renderer
distributes the remaining space over them; penalties inside the line
have width 0.  With JUSTIFY non-nil (the default) finite glues are
stretched/shrunk so the line fills its target width exactly (the
break node's :avail, third-pass emergency stretch included); with nil
glues keep their natural width, except when the line is over-full,
which always shrinks -- centered and right-aligned lines are shifted
by the renderer instead.  Residual glue/kern/penalty items left at
the start of a continuation line by the break are discarded; the
paragraph's first line keeps its intentional leading glue."
  (let* ((vec (vconcat items))
         (n (length vec))
         (start (min (or (plist-get brk :start) 0) n))
         (end (min (or (plist-get brk :pos) n) n))
         (k0 (if (> start 0)
                 (+ start (or (cl-position-if
                               (lambda (itm)
                                 (eq (type-of itm) 'niori-pdf-ts-box))
                               (substring vec start end))
                              (- end start)))
               start))
         (line nil) (natural 0.0) (stretch 0.0) (shrink 0.0) (has-fill nil))
    (cl-loop for k from k0 below end
             for itm = (aref vec k)
             do (push itm line)
                (cl-incf natural (niori-pdf-ts-item-width itm))
                (when (eq (type-of itm) 'niori-pdf-ts-glue)
                  (if (> (niori-pdf-ts-glue-stretch-order itm) 0)
                      (setq has-fill t)
                    (cl-incf stretch (niori-pdf-ts-glue-stretch itm))
                    (cl-incf shrink (niori-pdf-ts-glue-shrink itm)))))
    (setq line (nreverse line))
    (let* ((avail (plist-get brk :avail))
           (delta (and (numberp avail) (- avail natural)))
           ;; an over-full line shrinks at most SHRINK; the rest sticks
           ;; out of the measure like a TeX overfull box
           (applied (and delta (< delta 0) (max delta (- shrink))))
           (ratio (plist-get brk :ratio))
           (out nil)
           (widths nil))
      (cl-loop for itm in line
               do (push itm out)
                  (push (pcase (type-of itm)
                          (`niori-pdf-ts-glue
                           (let ((w (niori-pdf-ts-glue-width itm)))
                             (cond
                              ;; fill glues: the renderer distributes
                              ;; the remaining space over them
                              ((> (niori-pdf-ts-glue-stretch-order itm) 0)
                               nil)
                              ;; exact shrink (both justify modes); this
                              ;; also covers over-full lines, which take
                              ;; at most their shrink and stick out like
                              ;; a TeX overfull box -- fill glue cannot
                              ;; shrink, so it must not mask the shrink
                              ((and delta (< delta 0) (> shrink 0))
                               (+ w (* (niori-pdf-ts-glue-shrink itm)
                                       (/ (float applied) shrink))))
                              ;; a fill glue in the line absorbs the
                              ;; whole excess: finite glues stay natural
                              (has-fill w)
                              ;; exact stretch (pass 1/2/3 alike)
                              ((and justify delta (> delta 0) (> stretch 0))
                               (+ w (* (niori-pdf-ts-glue-stretch itm)
                                       (/ (float delta) stretch))))
                              ;; break node without :avail: fall back
                              ;; to the recorded ratio
                              ((and (not delta) justify (> ratio 0))
                               (+ w (* ratio
                                       (niori-pdf-ts-glue-stretch itm))))
                              ((and (not delta) justify (< ratio 0))
                               (+ w (* ratio
                                       (niori-pdf-ts-glue-shrink itm))))
                              (t w))))
                          (_ (niori-pdf-ts-item-width itm)))
                        widths)
         finally (cl-return-from niori-pdf-ts-line
                   (cons (nreverse out) (nreverse widths)))))))

(provide 'niori-pdf-typeset)
;;; niori-pdf-typeset.el ends here
