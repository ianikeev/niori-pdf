;;; niori-pdf-flow.el --- Flow typesetting engine (cl-typesetting model) -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2021 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; This file ports the CL-TYPESETTING layout model.  Its flow engine
;; uses boxes, glue and penalties with Knuth-Plass paragraph breaking.
;; See LICENSE for the applicable upstream license.

;;; Commentary:

;; High-level document layout API.  A flow document is a vertical
;; sequence of blocks laid out between the page margins with automatic
;; page breaks.  Blocks:
;;
;;   `niori-pdf-flow-heading'    headings with keep-with-next
;;   `niori-pdf-flow-paragraph'  paragraphs; Knuth-Plass line breaking,
;;                               hyphenation, justified/center/right/
;;                               left alignment via fill glues
;;   `niori-pdf-flow-rule'       horizontal rules
;;   `niori-pdf-flow-spacer'     vertical space
;;   `niori-pdf-flow-page-break' forced page break
;;   `niori-pdf-flow-image'      scaled images with captions
;;   `niori-pdf-flow-box'        filled/stroked cards with padding
;;   `niori-pdf-flow-columns'    side-by-side blocks (KPI rows)
;;   `niori-pdf-flow-table'      grid tables with wrapping cells,
;;                               backgrounds, borders and header repeat
;;   `niori-pdf-flow-toc'        table of contents with page numbers
;;
;; Use `niori-pdf-flow-generate' (or `niori-pdf-flow-save') with a
;; builder thunk; when a TOC is requested the builder is re-run until
;; the collected page numbers are stable (at most 3 passes).

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-fonts)
(require 'niori-pdf-ttf)
(require 'niori-pdf-content)
(require 'niori-pdf-images)
(require 'niori-pdf-typeset)
(require 'niori-pdf-hyphen)

;;;; Flow context

(cl-defstruct niori-pdf-flow
  doc                        ; the underlying `niori-pdf-document'
  page-w page-h              ; page size in points
  margin-left margin-right margin-top margin-bottom
  x0 x1                      ; text area bounds
  y                          ; flow cursor: points from the TOP of the page
  header-fn footer-fn        ; functions of (flow page-number)
  page-number
  toc-entries                ; collected plist entries, newest first
  toc-needed                 ; t when a TOC block was placed
  refs                       ; alist (ID . PLIST) marked this pass
  floats                     ; active floats on the current page
  footnotes                  ; footnotes registered on the current page
  footnote-heights           ; alist (PAGE-NUMBER . height) this pass
  vjustify                   ; distribute leftover space over block boundaries
  segments                   ; buffered page: reversed list of (:chunks :leading)
  seg-chunks                 ; reversed operator chunks of the current segment
  seg-stretch                ; vertical stretch accumulated for this segment
  default-font default-size default-leading
  paragraph-font paragraph-size
  (language niori-pdf-hyphen-default-language) ; hyphenation language
  (hyphenate t))             ; hyphenate paragraphs by default

(defvar niori-pdf--flow nil "Current flow context.")
(defvar niori-pdf-flow--pass-entries nil
  "TOC entries prepared by a previous pass (non-nil during later passes).")

(defvar niori-pdf-flow--last nil
  "The most recent flow context; survives the builder's dynamic extent.")

(defvar niori-pdf-flow--pass-refs nil
  "Reference points collected by the previous pass (non-nil during
later passes), an alist (ID . PLIST) with :page :y :data.")

(defvar niori-pdf-flow--pass-footnotes nil
  "Per-page footnote block heights collected by the previous pass
(non-nil during later passes), an alist (PAGE-NUMBER . HEIGHT).
The text column of a page that carries footnotes is shortened by
its height so the note block fits between the text and the bottom
margin (see `niori-pdf-flow-footnote').")

(defun niori-pdf-flow--current ()
  "Return the most recent flow context (see `niori-pdf-flow-generate')."
  niori-pdf-flow--last)

(defun niori-pdf-flow--reset-page (flow &optional justify)
  "Start a fresh page in FLOW, running its template functions.
The buffered content of the previous page is flushed first; with
JUSTIFY non-nil (automatic page breaks) leftover vertical space is
distributed over the page's vertical glues."
  (niori-pdf-flow--flush-page flow justify)
  (niori-pdf-new-page (niori-pdf-flow-doc flow)
                      (niori-pdf-flow-page-w flow)
                      (niori-pdf-flow-page-h flow))
  (setf (niori-pdf-flow-page-number flow)
        (length (niori-pdf-document-pages (niori-pdf-flow-doc flow))))
  (setf (niori-pdf-flow-floats flow) nil)
  (setf (niori-pdf-flow-footnotes flow) nil)
  (let ((niori-pdf--doc (niori-pdf-flow-doc flow))
        (niori-pdf--font nil)
        (niori-pdf--font-size nil))
    (when (niori-pdf-flow-header-fn flow)
      (funcall (niori-pdf-flow-header-fn flow)
               flow (niori-pdf-flow-page-number flow)))
    (when (niori-pdf-flow-footer-fn flow)
      (funcall (niori-pdf-flow-footer-fn flow)
               flow (niori-pdf-flow-page-number flow))))
  (setf (niori-pdf-flow-y flow) (niori-pdf-flow-margin-top flow)))

(defun niori-pdf-flow--bottom (flow)
  "Y limit (from page top) below which blocks move to the next page.
A page that carries footnotes is shortened by its note block height
from the previous layout pass (see `niori-pdf-flow-footnote')."
  (- (niori-pdf-flow-page-h flow)
     (niori-pdf-flow-margin-bottom flow)
     (or (cdr (assq (niori-pdf-flow-page-number flow)
                    niori-pdf-flow--pass-footnotes))
         0)))

(defun niori-pdf-flow--width (flow)
  "Current text width in points."
  (- (niori-pdf-flow-x1 flow) (niori-pdf-flow-x0 flow)))

(defun niori-pdf-flow--page (flow)
  "Current page (topmost in doc.pages)."
  (car (niori-pdf-document-pages (niori-pdf-flow-doc flow))))

(defun niori-pdf-flow--ensure-space (flow h)
  "Page-break unless H points fit between cursor and bottom margin."
  (when (> (+ (niori-pdf-flow-y flow) h) (niori-pdf-flow--bottom flow))
    (niori-pdf-flow--reset-page flow t)))

(defun niori-pdf-flow--advance (flow h)
  "Move the flow cursor down by H points."
  (setf (niori-pdf-flow-y flow) (+ (niori-pdf-flow-y flow) h)))

(defun niori-pdf-flow--emit (flow string)
  "Buffer an operator string in the current page segment of FLOW.
Page content is buffered so that vertical glues can stretch it when
the page is complete (see `niori-pdf-flow-vglue')."
  (setf (niori-pdf-flow-seg-chunks flow)
        (cons string (niori-pdf-flow-seg-chunks flow))))

(defun niori-pdf-flow--emitf (flow fmt &rest args)
  (niori-pdf-flow--emit flow (apply #'format fmt args)))

(defun niori-pdf-flow--seg-close (flow)
  "Close the current page segment of FLOW, keeping its stretch.
The segment's :stretch is the glue accumulated immediately before
it (from page top), so `niori-pdf-flow--flush-page' can shift each
segment down by the share of the glues above it."
  (when (or (niori-pdf-flow-seg-chunks flow)
            (> (niori-pdf-flow-seg-stretch flow) 0)
            (niori-pdf-flow-segments flow))
    (push (list :chunks (nreverse (niori-pdf-flow-seg-chunks flow))
                :stretch (niori-pdf-flow-seg-stretch flow))
          (niori-pdf-flow-segments flow))
    (setf (niori-pdf-flow-seg-chunks flow) nil
          (niori-pdf-flow-seg-stretch flow) 0)))

(defun niori-pdf-flow--vgap (flow stretch)
  "Record a vertical glue of STRETCH at the current position.
The content emitted before the glue forms one segment, the content
after it another; the glue's stretch is shared proportionally at
flush time."
  (when (niori-pdf-flow-seg-chunks flow)
    (niori-pdf-flow--seg-close flow))
  (cl-incf (niori-pdf-flow-seg-stretch flow) (float stretch)))

(defun niori-pdf-flow--boundary (flow)
  "Record a block boundary; a weak glue when :vjustify is on."
  (when (niori-pdf-flow-vjustify flow)
    (niori-pdf-flow--vgap flow 1)))

(defun niori-pdf-flow--flush-page (flow &optional justify)
  "Flush the buffered page content of FLOW to the current page.
With JUSTIFY non-nil the leftover space above the bottom margin is
distributed over the page's vertical glues: every segment after a
glue is shifted down with a `cm' transform, so text, rules and
images all move consistently."
  (niori-pdf-flow--seg-close flow)
  (let* ((segs (nreverse (niori-pdf-flow-segments flow)))
         (total (cl-reduce #'+ segs :initial-value 0.0
                           :key (lambda (s) (plist-get s :stretch)))))
    (setf (niori-pdf-flow-segments flow) nil)
    (when (and segs (niori-pdf-flow--page flow))
      (let* ((deficit (if (and justify (> total 0))
                          (max 0.0 (- (niori-pdf-flow--bottom flow)
                                      (niori-pdf-flow-y flow)))
                        0.0))
             (out nil)
             (shift 0.0))
        (dolist (seg segs)
          (let ((stretch (plist-get seg :stretch))
                (chunks (plist-get seg :chunks)))
            ;; this segment's own leading glue shifts it down
            (cl-incf shift (if (> total 0)
                               (/ (* deficit stretch) total)
                             0.0))
            (when (> shift 0.001)
              (push "q\n" out)
              (push (format "1 0 0 1 0 %s cm\n"
                            (niori-pdf-num (- shift))) out))
            (setq out (append (nreverse chunks) out))
            (when (> shift 0.001)
              (push "Q\n" out))))
        (niori-pdf-page-emit
         (niori-pdf-flow--page flow)
         (apply #'concat (nreverse out))))))
  ;; footnotes registered on this page are drawn when the page is
  ;; flushed, into the space the text column reserved for them (see
  ;; `niori-pdf-flow-footnote')
  (niori-pdf-flow--draw-footnotes flow))

(defmacro niori-pdf-flow--with-graphics (flow &rest body)
  "Run BODY with the content-level dynamic context on FLOW's page."
  (declare (indent 1))
  `(let ((niori-pdf--doc (niori-pdf-flow-doc ,flow))
         (niori-pdf--page (niori-pdf-flow--page ,flow))
         (niori-pdf--font nil)
         (niori-pdf--font-size nil))
     ,@body))

(defun niori-pdf-flow--pdf-y (flow y-from-top)
  "Convert a y measured from the page top into PDF coordinates."
  (- (niori-pdf-flow-page-h flow) y-from-top))

;;;; Document macro

(defmacro niori-pdf-with-flow-document (props &rest body)
  "Create a flow document context and run BODY (block calls) inside it.
PROPS is a literal plist: :width :height :margins (left top right
bottom) :header-fn :footer-fn :title :author :subject :keywords
:creator :compression :default-font :default-size :language
:hyphenate :vjustify :pdfa.  After BODY the finished document is the
value of `niori-pdf--doc'."
  (declare (indent 1))
  `(let ((niori-pdf--doc (niori-pdf-make-document))
         (niori-pdf--flow
          (make-niori-pdf-flow
           :doc nil
           :page-w (or ,(plist-get props :width) 595.28)
           :page-h (or ,(plist-get props :height) 841.89)
           :margin-left 54 :margin-top 56
           :margin-right 54 :margin-bottom 56
           :toc-entries nil :toc-needed nil
           :refs nil :floats nil
           :segments nil :seg-chunks nil :seg-stretch 0
           :vjustify ,(or (plist-get props :vjustify) nil)
           :default-font (or ,(plist-get props :default-font) "Helvetica")
           :default-size (or ,(plist-get props :default-size) 10.5)
           :default-leading 1.38
           :paragraph-font (or ,(plist-get props :default-font) "Helvetica")
           :paragraph-size (or ,(plist-get props :default-size) 10.5)
           :language (or ,(plist-get props :language)
                         niori-pdf-hyphen-default-language)
           :hyphenate ,(or (plist-get props :hyphenate) t))))
     (setf (niori-pdf-flow-doc niori-pdf--flow) niori-pdf--doc)
     ,@(when (plist-get props :margins)
         `((cl-destructuring-bind (l . rest) ,(plist-get props :margins)
             (setf (niori-pdf-flow-margin-left niori-pdf--flow) l)
             (when rest
               (setf (niori-pdf-flow-margin-top niori-pdf--flow) (nth 0 rest))
               (when (nth 1 rest)
                 (setf (niori-pdf-flow-margin-right niori-pdf--flow)
                       (nth 1 rest))
                 (when (nth 2 rest)
                   (setf (niori-pdf-flow-margin-bottom niori-pdf--flow)
                         (nth 2 rest))))))))
     ,@(when (plist-get props :header-fn)
         `((setf (niori-pdf-flow-header-fn niori-pdf--flow)
                 ,(plist-get props :header-fn))))
     ,@(when (plist-get props :footer-fn)
         `((setf (niori-pdf-flow-footer-fn niori-pdf--flow)
                 ,(plist-get props :footer-fn))))
     ,@(when (plist-get props :compression)
         `((setf (niori-pdf-document-compression niori-pdf--doc)
                 ,(plist-get props :compression))))
     ;; :pdfa may be a literal t or a run-time expression (e.g. a
     ;; command-line flag); the value is embedded at expansion time
     ;; and checked at run time
     (when ,(plist-get props :pdfa)
       (require 'niori-pdf-pdfa)
       (niori-pdf-pdfa-enable niori-pdf--doc))
     (setf (niori-pdf-flow-x0 niori-pdf--flow)
           (niori-pdf-flow-margin-left niori-pdf--flow)
           (niori-pdf-flow-x1 niori-pdf--flow)
           (- (niori-pdf-flow-page-w niori-pdf--flow)
              (niori-pdf-flow-margin-right niori-pdf--flow)))
     (niori-pdf-flow--reset-page niori-pdf--flow)
     ;; route raw content-level operators (images, rules, charts, link
     ;; rectangles) into the page buffer as well, so that vertical glue
     ;; shifts move them together with the text
     (cl-letf (((symbol-function 'niori-pdf--emit)
                (lambda (str)
                  (niori-pdf-flow--emit niori-pdf--flow str)))
               ((symbol-function 'niori-pdf--emitf)
                (lambda (fmt &rest args)
                  (niori-pdf-flow--emit niori-pdf--flow
                                        (apply #'format fmt args)))))
       ,@body
       ;; flush the final page (vertically justified like auto breaks)
       (niori-pdf-flow--flush-page niori-pdf--flow t))
     (setf (niori-pdf-document-info niori-pdf--doc)
           (append
            (list :producer
                  (concat "niori-pdf.el " niori-pdf-version
                          " (port of cl-pdf)"))
            ',(apply #'append
                     (mapcar (lambda (k)
                               (when (plist-get props k)
                                 (list k (plist-get props k))))
                             '(:title :author :subject :keywords :creator)))))
     (setq niori-pdf-flow--last niori-pdf--flow)
     niori-pdf--doc))

(defun niori-pdf-flow-save (doc file)
  "Write flow document DOC to FILE (same as `niori-pdf-save')."
  (niori-pdf-save doc file))

(defun niori-pdf-flow-generate (builder)
  "Run BUILDER (a thunk of flow blocks) and return the document.
The builder is re-run until the layout is stable: the table of
contents entries and the reference points collected by
`niori-pdf-flow-mark-ref-point' must stop changing between passes
(at most six passes), so TOCs and cross-references show correct
page numbers."
  (setq niori-pdf-flow--pass-entries nil)
  (setq niori-pdf-flow--pass-refs nil)
  (setq niori-pdf-flow--pass-footnotes nil)
  (setq niori-pdf--doc (funcall builder))
  (let ((prev-toc nil) (prev-refs nil) (prev-notes nil) (pass 0)
        (stable nil))
    (while (and (not stable) (< pass 6))
      (let* ((flow (niori-pdf-flow--current))
             (cur-toc (reverse (niori-pdf-flow-toc-entries flow)))
             (cur-refs (niori-pdf-flow-refs flow))
             (cur-notes (niori-pdf-flow-footnote-heights flow)))
        (setq stable (and (equal cur-toc prev-toc)
                          (equal cur-refs prev-refs)
                          (equal cur-notes prev-notes)
                          (or (not (niori-pdf-flow-toc-needed flow))
                              (> pass 0))))
        (setq prev-toc cur-toc prev-refs cur-refs prev-notes cur-notes)
        (setq niori-pdf-flow--pass-entries cur-toc)
        (setq niori-pdf-flow--pass-refs cur-refs)
        (setq niori-pdf-flow--pass-footnotes cur-notes)
        (unless stable
          (setq niori-pdf--doc (funcall builder))
          (setq pass (1+ pass))))))
  niori-pdf--doc)

;;;; Paragraph items (boxes, glue, penalties)

(defun niori-pdf-flow--split-words (runs)
  "Split styled RUNS into words, preserving per-word styles.
RUNS is a string or a list whose elements are strings or (TEXT &key
font size color) forms.  Returns a list of words; each word is a list
of (TEXT . STYLE) conses (a single cons for an ordinary word).  A
word boundary exists only where the source has whitespace: adjacent
runs that touch without whitespace (e.g. the italic run \"you\" of
\"_you_.\" followed by the roman run \".\") are fragments of one
word, set later without any space between them."
  (let ((normalized (if (stringp runs) (list runs) runs))
        (words nil) (cur nil) (after-ws t))
    (dolist (run normalized)
      (let* ((run (if (stringp run) (list run) run))
             (text (car run))
             (style (cdr run))
             (len (length text))
             (i 0)
             (segs nil))
        ;; segment this run's text into word pieces and whitespace
        (while (< i len)
          (let ((m (string-match "[ \t\n]+" text i)))
            (if m
                (progn
                  (unless (= i m)
                    (push (cons 'word (substring text i m)) segs))
                  (push '(ws) segs)
                  (setq i (match-end 0)))
              (push (cons 'word (substring text i)) segs)
              (setq i len))))
        ;; run the state machine over the segments
        (dolist (seg (nreverse segs))
          (if (eq (car seg) 'ws)
              (progn
                (when cur
                  (push (nreverse cur) words)
                  (setq cur nil))
                (setq after-ws t))
            (cond ((and cur (not after-ws))
                   ;; touching continuation of the same word
                   (push (cons (cdr seg) style) cur))
                  (t
                   (when cur
                     (push (nreverse cur) words)
                     (setq cur nil))
                   (push (cons (cdr seg) style) cur)))
            (setq after-ws nil)))))
    (when cur (push (nreverse cur) words))
    (nreverse words)))

(defun niori-pdf-flow--deco-style (style base-size)
  "Apply run decorations to STYLE for BASE-SIZE text.
Superscript/subscript shrink the size and add a :raise baseline
offset; :underline/:strike/:background are handled by the line
renderer.  Returns the possibly extended style plist.  (:size is
replaced, not appended -- plist-get sees the first occurrence.)"
  (let ((base (or (plist-get style :size) base-size)))
    (cond
     ((plist-get style :superscript)
      (plist-put (plist-put (copy-sequence style)
                            :size (* base 0.66))
                 :raise (* base 0.33)))
     ((plist-get style :subscript)
      (plist-put (plist-put (copy-sequence style)
                            :size (* base 0.66))
                 :raise (- (* base 0.17))))
     (t style))))

(cl-defun niori-pdf-flow--word-box (flow word style
                                         &key (hyphenate t) (language nil)
                                         &allow-other-keys)
  "Build the KP items for WORD in FLOW with STYLE.
When HYPHENATE is non-nil the word's TeX hyphenation points become
flagged penalties carrying the hyphen width; LANGUAGE (default: the
flow's document language) selects the pattern set.  Returns (ITEMS .
PLAIN-ITEMS): the hyphenated item list and the plain variant used for
the first (pretolerance) pass.

Words that contain an explicit hyphen (compounds such as
\"niori-pdf-curve-to\") follow the TeX rule: pattern hyphenation is
disabled for the whole word, and the break opportunities sit AFTER
the existing hyphens instead.  Those breaks are flagged penalties of
width 0 (the hyphen glyph is part of the left fragment, so no extra
hyphen is drawn at the line end) and an ex-hyphen penalty of 50."
  (let* ((doc (niori-pdf-flow-doc flow))
         (para-font (niori-pdf-flow-paragraph-font flow))
         (para-size (niori-pdf-flow-paragraph-size flow))
         (style (niori-pdf-flow--deco-style style para-size))
         (fname (or (plist-get style :font) para-font))
         (size (or (plist-get style :size) para-size))
         (font (niori-pdf-get-font doc fname))
         (content (list :text word
                        :style style :font font :font-name fname
                        :size size))
         (width (niori-pdf-text-width-kerned word font size))
         (plain-box (niori-pdf-ts-box
                     (niori-pdf-text-width word font size)
                     (list :text word :style style :font font
                           :font-name fname :size size)))
         ;; fragments when the word has interior hyphens: each fragment
         ;; carries its trailing hyphen; nil when not applicable
         (hy-frags (when (and (> (length word) 1)
                              (string-match-p "-" word))
                     (let ((frags nil) (start 0) (pos -1))
                       (while (setq pos (string-match "-" word (1+ pos)))
                         (when (< pos (1- (length word)))
                           (push (substring word start (1+ pos)) frags)
                           (setq start (1+ pos))))
                       (when (< start (length word))
                         (push (substring word start) frags))
                       (setq frags (nreverse frags))
                       (when (cdr frags) frags)))))
    (cond
     ;; explicit hyphens: break after each interior hyphen, no
     ;; pattern hyphenation (TeX disables it for such words)
     (hy-frags
      (let ((items nil))
        (dolist (frag hy-frags)
          (push (niori-pdf-ts-box
                 (niori-pdf-text-width-kerned frag font size)
                 (list :text frag :style style :font font
                       :font-name fname :size size))
                items)
          (unless (eq frag (car (last hy-frags)))
            (push (niori-pdf-ts-penalty 50 t 0) items)))
        (cons (nreverse items) (list plain-box))))
     ((not hyphenate)
      (cons (list (niori-pdf-ts-box width content))
            (list plain-box)))
     (t
      (let* ((frags (niori-pdf-hyphen-word-splits
                     word (or language (niori-pdf-flow-language flow))))
             (hyphen-w (niori-pdf-text-width "-" font size)))
        (if (= (length frags) 1)
            (cons (list (niori-pdf-ts-box width content))
                  (list plain-box))
          (let ((items nil)
                (frags-left frags))
            (while frags-left
              (let ((frag (car frags-left)))
                (push (niori-pdf-ts-box
                       (niori-pdf-text-width-kerned frag font size)
                       (list :text frag :style style :font font
                             :font-name fname :size size))
                      items)
                (when (cdr frags-left)
                  (push (niori-pdf-ts-penalty 50 t hyphen-w) items))
                (setq frags-left (cdr frags-left))))
            (cons (nreverse items) (list plain-box)))))))))

(cl-defun niori-pdf-flow--paragraph-items (flow runs &key (hyphenate t)
                                                (align 'left)
                                                (language nil)
                                                (font nil) (size nil)
                                                &allow-other-keys)
  "Build the KP item sequence for a paragraph of styled RUNS.
FONT/SIZE are the paragraph-level defaults (the flow defaults are
used for nil).  Returns (ITEMS . PLAIN-ITEMS) with interword glue
and -- for justified paragraphs -- the parfillskip and the final
forced break.  The interword glue width is taken from the
paragraph's own font and size, not from the document defaults.
Alignment is not baked into the items: centered and right-aligned
lines are shifted by the renderer (see
`niori-pdf-flow--render-line'), which keeps the alignment correct on
every line of a multi-line paragraph."
  (let* ((words (niori-pdf-flow--split-words runs))
         (para-font (or font (niori-pdf-flow-paragraph-font flow)))
         (para-size (or size (niori-pdf-flow-paragraph-size flow)))
         (sw (niori-pdf-flow--space-width flow para-font para-size))
         (doc (niori-pdf-flow-doc flow))
         (items nil) (plain nil))
    ;; words with interword glue
    (let ((firstp t))
      (dolist (word words)
        (unless firstp
          ;; interword glue: stretch = sw/2, shrink = sw/3
          (push (niori-pdf-ts-glue sw (/ sw 2.0) (/ sw 3.0)) items)
          (push (niori-pdf-ts-glue sw (/ sw 2.0) (/ sw 3.0)) plain))
        (if (cdr word)
            ;; a multi-fragment word (a style change inside the word,
            ;; e.g. "_you_."): one box per fragment, glued together by
            ;; unbreakable penalties so the word never splits; no
            ;; pattern hyphenation (TeX does not hyphenate such words)
            (cl-labels ((frag-items (frags)
                          ;; fragments and the inter-fragment penalties,
                          ;; in REVERSE document order (the accumulator
                          ;; convention: nreverse(d) by the caller)
                          (let ((out nil))
                            (dolist (f frags)
                              (when out
                                (push (niori-pdf-ts-penalty
                                       niori-pdf-ts-inf-penalty)
                                      out))
                              (push f out))
                            out)))
              (let ((witems nil))
                (dolist (frag word)
                  (let* ((ftext (car frag))
                         (fstyle (niori-pdf-flow--deco-style
                                  (cdr frag) para-size))
                         (fname (or (plist-get fstyle :font) para-font))
                         (fsize (or (plist-get fstyle :size) para-size))
                         (ffont (niori-pdf-get-font doc fname)))
                    (push (niori-pdf-ts-box
                           (niori-pdf-text-width-kerned ftext ffont fsize)
                           (list :text ftext :style fstyle :font ffont
                                 :font-name fname :size fsize))
                          witems)))
                (setq witems (nreverse witems))
                (setq items (append (frag-items witems) items))
                (setq plain (append (frag-items witems) plain))))
          ;; single-fragment word: hyphenated item list
          (let* ((frag (car word))
                 (wb (niori-pdf-flow--word-box flow (car frag) (cdr frag)
                                               :hyphenate hyphenate
                                               :language language))
                 (wi (car wb)) (wp2 (cdr wb)))
            (setq items (append (nreverse wi) items))
            (setq plain (append (nreverse wp2) plain))))
        (setq firstp nil)))
    ;; every paragraph ends with a parfillskip (TeX's \parfillskip):
    ;; the last line must be able to be short without a badness
    ;; explosion, otherwise the breaker prefers a single over-full
    ;; line (this used to break table cells, which are not justified).
    ;; The forced break terminates the paragraph after it.
    ;; (push order reversed by nreverse: penalty ends up LAST)
    (push (niori-pdf-ts-hfil) items)
    (push (niori-pdf-ts-hfil) plain)
    (push (niori-pdf-ts-penalty niori-pdf-ts-forced-penalty) items)
    (push (niori-pdf-ts-penalty niori-pdf-ts-forced-penalty) plain)
    (cons (nreverse items) (nreverse plain))))

(defun niori-pdf-flow--space-width (flow font-name size)
  "Interword space width of FONT-NAME at SIZE.
FONT-NAME may be a base-14 name or a .ttf/.otf path (resolved
through the flow's document)."
  (let ((font (if (niori-pdf-flow-doc flow)
                  (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                      font-name)
                font-name)))
    (niori-pdf-char-width 32 font size)))

;;;; Line rendering

(cl-defun niori-pdf-flow--render-line (flow x y-top line widths brk
                                            &key (avail 0) align)
  "Render one broken line (LINE . WIDTHS from `niori-pdf-ts-line')
with its baseline at Y-TOP.  nil widths (fill glues) absorb the
remaining AVAIL space equally; whatever slack is left over shifts the
line origin for CENTER/RIGHT alignment (JUSTIFY nil, see
`niori-pdf-ts-line').  BRK supplies the hyphen flag.
Run decorations are honored: :background fills a rect behind the
run (before the text), :raise shifts the baseline (superscript/
subscript), :underline and :strike draw rules under/through the run
(after the text)."
  (niori-pdf-flow--with-graphics flow
    (let* ((wv (vconcat widths))
           (lv (vconcat line))
           (used (cl-reduce #'+ (delq nil (append widths nil))
                            :initial-value 0.0))
           (slack (max 0.0 (- avail used)))
           (x-origin
            (pcase align
              (`center (+ x (/ slack 2.0)))
              (`right (+ x slack))
              (_ x)))
           ;; fill glues: the highest stretch order absorbs all slack,
           ;; split equally among the glues of that order (TeX)
           (fill-orders
            (mapcar (lambda (i)
                      (cons i (niori-pdf-ts-glue-stretch-order
                               (aref lv i))))
                    (cl-loop for i below (length wv)
                             when (null (aref wv i)) collect i)))
           (max-order (if fill-orders
                          (cl-reduce #'max fill-orders
                                     :key #'cdr :initial-value 0)
                        0))
           (top-fills (cl-loop for (i . o) in fill-orders
                               when (= o max-order) collect i))
           (fill-share (if top-fills (/ slack (float (length top-fills)))
                         0.0))
           (resolved (mapcar
                      (lambda (w) (or w fill-share))
                      (append widths nil)))
           (y (niori-pdf-flow--pdf-y flow y-top))
           (bgs nil) (rules nil)
           (xcur x-origin))
      ;; backgrounds first (under the text)
      (cl-loop for item across (vconcat line)
               for w across (vconcat resolved)
               do (let ((style (and (niori-pdf-ts-box-p item)
                                    (plist-get
                                     (niori-pdf-ts-box-content item)
                                     :style))))
                  (when (and style (plist-get style :background))
                    (let* ((size (plist-get
                                  (niori-pdf-ts-box-content item) :size))
                           (dy (or (plist-get style :raise) 0)))
                      (push (list xcur w (+ y dy (* -0.22 size))
                                  (* 0.98 size)
                                  (plist-get style :background))
                            bgs)))
                  (cl-incf xcur (or w 0.0))))
      (dolist (bg (nreverse bgs))
        (niori-pdf-flow--emitf flow "%s %s %s rg\n"
                               (niori-pdf-num (nth 0 (nth 4 bg)))
                               (niori-pdf-num (nth 1 (nth 4 bg)))
                               (niori-pdf-num (nth 2 (nth 4 bg))))
        (niori-pdf-flow--emitf
         flow "%s %s %s %s re f\n"
         (niori-pdf-num (nth 0 bg)) (niori-pdf-num (nth 2 bg))
         (niori-pdf-num (nth 1 bg)) (niori-pdf-num (nth 3 bg))))
      (niori-pdf-flow--emitf flow "BT\n")
      (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                             (niori-pdf-num x-origin) (niori-pdf-num y))
      (let ((prev-fname :none) (prev-size nil) (prev-color :none)
            (last-font nil) (last-size nil) (encoded-hyphen nil)
            (xcur x-origin)
            ;; pending Td delta.  Advances (box widths, glues, kerns)
            ;; accumulate in PEND-X/PEND-Y and are flushed in a single
            ;; Td right before the next box paints, so that a box's own
            ;; :raise is already in effect while its glyphs are drawn
            ;; (emitting the raise after the text would draw the
            ;; superscript at the baseline and shift the rest of the
            ;; line up instead).  CUR-Y is the baseline offset the
            ;; cursor currently sits at.
            (pend-x 0.0) (cur-y 0.0))
        (cl-loop for item across (vconcat line)
                 for w across (vconcat resolved)
                 do (pcase (type-of item)
                      (`niori-pdf-ts-box
                       (let* ((c (niori-pdf-ts-box-content item))
                              (style (plist-get c :style))
                              (color (or (plist-get style :color)
                                         '(0.12 0.12 0.12)))
                              (fname (plist-get c :font-name))
                              (size (plist-get c :size))
                              (font (plist-get c :font))
                              (text (plist-get c :text))
                              (dy (or (plist-get style :raise) 0))
                              (encoded (niori-pdf-font-encode text font)))
                         ;; move to the box origin (advance + own raise)
                         (niori-pdf-flow--emitf
                          flow "%s %s Td\n"
                          (niori-pdf-num pend-x)
                          (niori-pdf-num (- dy cur-y)))
                         (setq pend-x 0.0 cur-y dy)
                         (when (and color (not (equal color prev-color)))
                           (niori-pdf-flow--emitf
                            flow "%s %s %s rg\n"
                            (niori-pdf-num (nth 0 color))
                            (niori-pdf-num (nth 1 color))
                            (niori-pdf-num (nth 2 color)))
                           (setq prev-color color))
                         (unless (and (equal fname prev-fname)
                                      (equal size prev-size))
                           (niori-pdf-flow--emitf
                            flow "/%s %s Tf\n"
                            (niori-pdf-font-resource
                             (niori-pdf-get-font
                              (niori-pdf-flow-doc flow) fname))
                            (niori-pdf-num size))
                           (setq prev-fname fname prev-size size))
                         (niori-pdf-flow--emit
                          flow (niori-pdf--encoded-tj-operator
                                encoded font 'kerned))
                         (setq last-font font last-size size
                               encoded-hyphen
                               (niori-pdf-font-encode "-" font))
                         ;; this box's width advances past its glyphs
                         (setq pend-x w)
                         ;; underline / strikethrough rules
                         (when (or (plist-get style :underline)
                                   (plist-get style :strike))
                           (let ((color (or (plist-get style :color)
                                            '(0.12 0.12 0.12))))
                             (when (plist-get style :underline)
                               (push (list xcur w (- dy (* 0.15 size))
                                           (* 0.07 size) color)
                                     rules))
                             (when (plist-get style :strike)
                               (push (list xcur w (+ dy (* 0.28 size))
                                           (* 0.07 size) color)
                                     rules))))))
                      (`niori-pdf-ts-glue
                       (setq pend-x (+ pend-x (or w 0.0))))
                      (`niori-pdf-ts-kern
                       (setq pend-x
                             (+ pend-x (niori-pdf-ts-kern-amount item))))
                      (`niori-pdf-ts-penalty nil)
                      (_ nil))
                 do (cl-incf xcur (or w 0.0)))
        ;; hyphen at the line end when the break was at a hyphen point
        (when (and (plist-get brk :hyphen-p) last-font)
          ;; first move past the last box (its width is still pending)
          (when (> pend-x 0.0)
            (niori-pdf-flow--emitf flow "%s %s Td\n"
                                   (niori-pdf-num pend-x)
                                   (niori-pdf-num 0.0)))
          (unless (and (equal (niori-pdf-font-name last-font) prev-fname)
                       (equal last-size prev-size))
            (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                                   (niori-pdf-font-resource last-font)
                                   (niori-pdf-num last-size))
            (setq prev-fname (niori-pdf-font-name last-font)
                  prev-size last-size))
          (niori-pdf-flow--emit
           flow (niori-pdf--encoded-tj-operator encoded-hyphen last-font)))
        (niori-pdf-flow--emitf flow "ET\n"))
      ;; rules last (over the text)
      (dolist (r rules)
        (niori-pdf-flow--emitf flow "%s %s %s RG\n"
                               (niori-pdf-num (nth 0 (nth 4 r)))
                               (niori-pdf-num (nth 1 (nth 4 r)))
                               (niori-pdf-num (nth 2 (nth 4 r))))
        (niori-pdf-flow--emitf flow "%s w\n" (niori-pdf-num (nth 3 r)))
        (niori-pdf-flow--emitf
         flow "%s %s m %s %s l S\n"
         (niori-pdf-num (nth 0 r))
         (niori-pdf-num (+ y (nth 2 r)))
         (niori-pdf-num (+ (nth 0 r) (nth 1 r)))
         (niori-pdf-num (+ y (nth 2 r))))))))

(defun niori-pdf-flow--float-intrusion (flow ya yb)
  "Horizontal intrusion of active floats on FLOW over [YA, YB].
Returns (LEFT-INSET RIGHT-INSET) in points."
  (let ((lin 0.0) (rin 0.0))
    (dolist (f (niori-pdf-flow-floats flow))
      ;; a float spans [y0, y1] from the page top; overlap when the
      ;; line band intersects it
      (when (and (< ya (plist-get f :y1)) (> yb (plist-get f :y0)))
        (if (eq (plist-get f :side) 'left)
            (setq lin (max lin (+ (- (plist-get f :x1)
                                   (niori-pdf-flow-x0 flow))
                                 (or (plist-get f :gap) 0))))
          (setq rin (max rin (+ (- (niori-pdf-flow-x1 flow)
                                   (plist-get f :x0))
                                 (or (plist-get f :gap) 0)))))))
    (list lin rin)))

(defun niori-pdf-flow--line-suffix (items pos)
  "Slice ITEMS after break position POS, dropping the leading
residual glue/kern/penalty items the break left behind."
  (let ((rest (nthcdr pos (append items nil))))
    (while (and rest
                (not (niori-pdf-ts-box-p (car rest))))
      (setq rest (cdr rest)))
    (or rest '(()))))

(cl-defun niori-pdf-flow--paragraph-block (runs &key (align 'left) font size
                                                leading color indent
                                                (hyphenate
                                                 (niori-pdf-flow-hyphenate
                                                  niori-pdf--flow))
                                                (space-before 0)
                                                (space-after 0)
                                                (keep-lines 2)
                                                (language nil))
  "Add a paragraph block with CONTENT (the core of `niori-pdf-flow-paragraph').
LANGUAGE overrides the hyphenation language for this paragraph
only (a `niori-pdf-hyphen' language symbol such as `german').
When floats are active on the current page (see
`niori-pdf-flow-float') the lines wrap around them: each line is
broken at the width left over by the floats."
  (let* ((flow niori-pdf--flow)
         (font-name (or font (niori-pdf-flow-paragraph-font flow)))
         (size (or size (niori-pdf-flow-paragraph-size flow)))
         (leading (or leading (* size (niori-pdf-flow-default-leading
                                       flow))))
         (indent (or indent 0))
         ;; normalize runs: a run is a string or a (TEXT &key font size
         ;; color) list; fill in the paragraph defaults and color
         (runs (cond ((stringp runs) (list runs))
                     (t (copy-sequence runs))))
         (runs (mapcar
                (lambda (r)
                  (let ((pl (if (stringp r) (list r) (copy-sequence r))))
                    (append pl
                            (unless (plist-get (cdr pl) :font)
                              (list :font font-name))
                            (unless (plist-get (cdr pl) :size)
                              (list :size size))
                            (when (and color
                                       (not (plist-get (cdr pl) :color)))
                              (list :color color)))))
                runs))
         (width (- (niori-pdf-flow--width flow) indent))
         (pb (niori-pdf-flow--paragraph-items flow runs
                                              :hyphenate hyphenate
                                              :align align
                                              :language language
                                              :font font-name
                                              :size size))
         (items (car pb)) (plain (cdr pb))
         (floats-p (niori-pdf-flow-floats flow)))
    (niori-pdf-flow--boundary flow)
    (niori-pdf-flow--advance flow space-before)
    (if (not floats-p)
        ;; fast path: one Knuth-Plass run over the whole paragraph
        (let* ((breaks (niori-pdf-ts-break-paragraph items width plain))
               (n-lines (length breaks)))
          (let ((i 0))
            (while (< i n-lines)
              (let ((brk (nth i breaks)))
                ;; widow/orphan control: never leave fewer than
                ;; KEEP-LINES lines alone at a page boundary
                (let ((remaining (- n-lines i))
                      (need (* leading (min keep-lines
                                            (if (= i 0) keep-lines 1)))))
                  (niori-pdf-flow--ensure-space
                   flow (+ leading
                           (if (and (< remaining (min keep-lines 2))
                                    (> remaining 0))
                               (* remaining leading)
                             need))))
                (let* ((tl (niori-pdf-ts-line
                            items brk :justify (eq align 'justified)))
                       (line (car tl)) (widths (cdr tl))
                       (y-base (+ (niori-pdf-flow-y flow) (* 0.82 size))))
                  (niori-pdf-flow--render-line
                   flow (+ (niori-pdf-flow-x0 flow) indent) y-base
                   line widths brk
                   :avail width :align align))
                (niori-pdf-flow--advance flow leading))
              (setq i (1+ i)))))
      ;; float path: wrap around the floats -- break each line at the
      ;; width left over by the floats covering its vertical band
      (let ((rest items)
            (done nil))
        (while (not done)
          (let* ((ya (niori-pdf-flow-y flow))
                 (yb (+ ya leading))
                 (intr (niori-pdf-flow--float-intrusion flow ya yb))
                 (lin (+ indent (car intr)))
                 (rin (cadr intr))
                 (avail (max 12.0 (- width (car intr) rin)))
                 (rest-vec (append rest nil))
                 (brks (niori-pdf-ts-break-paragraph rest avail))
                 (brk (car brks)))
            (cond
             ((null brk)
              ;; nothing fits: emit an empty line to make progress
              (niori-pdf-flow--ensure-space flow leading)
              (niori-pdf-flow--advance flow leading))
             (t
              ;; widow/orphan control on the first lines
              (niori-pdf-flow--ensure-space flow leading)
              (let* ((tl (niori-pdf-ts-line
                          rest-vec brk :justify (eq align 'justified)))
                     (line (car tl)) (widths (cdr tl))
                     (y-base (+ (niori-pdf-flow-y flow) (* 0.82 size))))
                (niori-pdf-flow--render-line
                 flow (+ (niori-pdf-flow-x0 flow) lin) y-base
                 line widths brk
                 :avail avail :align align))
              (niori-pdf-flow--advance flow leading)
              (setq rest (niori-pdf-flow--line-suffix
                          rest-vec (plist-get brk :pos)))))
            ;; done when only the trailing forced break/parfillskip
            ;; remains (no boxes left)
            (let ((probe (append (if (consp rest) rest (list rest)) nil)))
              (setq done (not (cl-some #'niori-pdf-ts-box-p probe))))))))
    (niori-pdf-flow--advance flow space-after)))

(defun niori-pdf-flow-paragraph (content &rest keys)
  "Add a paragraph block with CONTENT.
CONTENT is a string or a list of runs; a run is a string or a (TEXT
&key font size color) form.  Paragraph-level defaults in KEYS apply
to every run.
KEYS: :align left|center|right|justified, :font :size :leading
:color :indent (left inset applied to every line) :hyphenate
:space-before :space-after.  Lines are broken with
the Knuth-Plass optimizer and set with AFM kerning; hyphenation uses
the document language (see `niori-pdf-with-flow-document' :language)
and defaults to the document's :hyphenate setting."
  (unless (plist-get keys :hyphenate)
    (setq keys (plist-put keys :hyphenate
                          (niori-pdf-flow-hyphenate niori-pdf--flow))))
  (apply #'niori-pdf-flow--paragraph-block content keys))

;;;; Headings

(defconst niori-pdf-flow--heading-styles
  '((1 :font "Helvetica-Bold" :size 17 :space-before 12 :space-after 7)
    (2 :font "Helvetica-Bold" :size 13.5 :space-before 11 :space-after 5)
    (3 :font "Helvetica-Bold" :size 11.5 :space-before 9 :space-after 4)
    (4 :font "Helvetica-BoldOblique" :size 10.5 :space-before 7 :space-after 4))
  "Default heading styles per level.")

(cl-defun niori-pdf-flow-heading (text &key (level 1) font size color
                                       space-before space-after
                                       (toc t) (bookmark toc))
  "Add a heading TEXT at LEVEL; collects a TOC entry by default.
With BOOKMARK non-nil (the default when TOC is on) a document outline
entry is created as well (see `niori-pdf-outlines-add')."
  (let* ((style (cdr (assq level niori-pdf-flow--heading-styles)))
         (font-name (or font (plist-get style :font)))
         (size (or size (plist-get style :size)))
         (sb (or space-before (plist-get style :space-before)))
         (sa (or space-after (plist-get style :space-after)))
         (leading (* 1.2 size))
         (flow niori-pdf--flow))
    (niori-pdf-flow--boundary flow)
    ;; keep-with-next: heading plus two body lines must fit
    (niori-pdf-flow--ensure-space flow (+ sb leading sa 24))
    (niori-pdf-flow--advance flow sb)
    (let ((y-base (+ (niori-pdf-flow-y flow) (* 0.84 size))))
      (niori-pdf-flow--with-graphics flow
        (niori-pdf-flow--emitf flow "BT\n")
        (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                               (niori-pdf-num (niori-pdf-flow-x0 flow))
                               (niori-pdf-num
                                (niori-pdf-flow--pdf-y flow y-base)))
        (niori-pdf-flow--emitf flow "%s %s %s rg\n"
                               (niori-pdf-num (nth 0 (or color '(0.1 0.1 0.1))))
                               (niori-pdf-num (nth 1 (or color '(0.1 0.1 0.1))))
                               (niori-pdf-num (nth 2 (or color '(0.1 0.1 0.1)))))
        (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                               (niori-pdf-font-resource
                                (niori-pdf-get-font
                                 (niori-pdf-flow-doc flow) font-name))
                               (niori-pdf-num size))
        (let* ((fnt (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                        font-name))
               (encoded (niori-pdf-font-encode text fnt)))
          (niori-pdf-flow--emit
           flow (niori-pdf--encoded-tj-operator encoded fnt 'kerned)))
        (niori-pdf-flow--emitf flow "ET\n")))
    (niori-pdf-flow--advance flow leading)
    (when toc
      ;; record the heading's own y (points from page top) so the TOC
      ;; links and bookmarks can target the heading, not just the page
      (push (list :level level :title text
                  :page (niori-pdf-flow-page-number flow)
                  :y (- (niori-pdf-flow-y flow) leading))
            (niori-pdf-flow-toc-entries flow)))
    (when (and bookmark (fboundp 'niori-pdf-outlines-add))
      (niori-pdf-outlines-add
       (niori-pdf-flow-doc flow) text
       (niori-pdf-flow-page-number flow) :level level
       :y (niori-pdf-flow--pdf-y flow
                                 (- (niori-pdf-flow-y flow) leading))))
    (niori-pdf-flow--advance flow sa)))

;;;; Simple blocks

(defun niori-pdf-flow-spacer (h)
  "Add vertical space of H points."
  (niori-pdf-flow--advance niori-pdf--flow h))

(defun niori-pdf-flow-page-break ()
  "Force a new page.
The current page's vertical glues stretch into the leftover space
first (so `niori-pdf-flow-vfill' works for title pages)."
  (niori-pdf-flow--reset-page niori-pdf--flow t))

(cl-defun niori-pdf-flow-vglue (&optional (stretch 6))
  "Add stretchable vertical space of STRETCH units.
When the current page is finished the leftover space above the bottom
margin is distributed over its vertical glues proportionally to their
stretch (a page broken by `niori-pdf-flow-page-break' is not
stretched).  Blocks after the glue are shifted down accordingly."
  (niori-pdf-flow--vgap niori-pdf--flow (float stretch)))

(defun niori-pdf-flow-vfill ()
  "Add infinitely stretchable vertical space.
Dominates every finite `niori-pdf-flow-vglue' on the page (TeX's
\\vfill); use it to push subsequent blocks to the bottom of the page."
  (niori-pdf-flow--vgap niori-pdf--flow 1.0e6))

;;;; Lists

(defun niori-pdf-flow--list-item-marker (flow x y text font-name size color)
  "Draw the list marker TEXT at X/Y-BASE (y from page top)."
  (niori-pdf-flow--with-graphics flow
    (niori-pdf-flow--emitf flow "BT\n")
    (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                           (niori-pdf-num x)
                           (niori-pdf-num (niori-pdf-flow--pdf-y flow y)))
    (apply #'niori-pdf-flow--emitf flow "%s %s %s rg\n"
           (mapcar #'niori-pdf-num (or color '(0.12 0.12 0.12))))
    (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                           (niori-pdf-font-resource
                            (niori-pdf-get-font
                             (niori-pdf-flow-doc flow) font-name))
                           (niori-pdf-num size))
    (let ((encoded (niori-pdf-font-encode text
                                          (niori-pdf-get-font
                                           (niori-pdf-flow-doc flow)
                                           font-name))))
      (niori-pdf-flow--emit
       flow (niori-pdf--encoded-tj-operator encoded
                                            (niori-pdf-get-font
                                             (niori-pdf-flow-doc flow)
                                             font-name)
                                            'kerned)))
    (niori-pdf-flow--emitf flow "ET\n")))

(cl-defun niori-pdf-flow-itemize (items &key (bullet "\u2022")
                                        (bullet-color '(0.13 0.25 0.51))
                                        (indent 16) (gap 6)
                                        font size color align
                                        (space-between 3)
                                        (space-before 3) (space-after 5)
                                        hyphenate language)
  "Add a bulleted list.  ITEMS is a list of strings or lists of runs.
Each item is set as a paragraph with a hanging indent: every line is
indented by INDENT plus the marker width, and the BULLET is drawn GAP
points left of the text at the item's first baseline.  Bullet \"-\"
works with every font; \"\\u2022\" needs a font that has the glyph
(the base-14 fonts do)."
  (let* ((flow niori-pdf--flow)
         (font-name (or font (niori-pdf-flow-paragraph-font flow)))
         (size (or size (niori-pdf-flow-paragraph-size flow)))
         (fnt (niori-pdf-get-font (niori-pdf-flow-doc flow) font-name))
         (mark-w (+ (niori-pdf-text-width bullet fnt size) gap))
         (leading (* size (niori-pdf-flow-default-leading flow))))
    (niori-pdf-flow--boundary flow)
    (niori-pdf-flow--advance flow space-before)
    (dolist (item items)
      ;; reserve room for the item's first lines so that no page break
      ;; happens between the marker placement and the first line
      (niori-pdf-flow--ensure-space
       flow (+ (* 3.2 leading) space-between))
      (let ((marker-y (+ (niori-pdf-flow-y flow) (* 0.82 size))))
        (niori-pdf-flow--paragraph-block
         item :font font-name :size size :color color :align align
         :indent (+ indent mark-w) :space-after space-between
         :hyphenate (not (eq hyphenate nil)) :language language)
        (niori-pdf-flow--list-item-marker
         flow (+ (niori-pdf-flow-x0 flow) indent) marker-y
         bullet font-name size (or bullet-color '(0.12 0.12 0.12)))))
    (niori-pdf-flow--advance flow (- space-after space-between))))

(cl-defun niori-pdf-flow-enumerate (items &key (start 1) (format "%d.")
                                      (marker-color '(0.13 0.25 0.51))
                                      (indent 16) (gap 6)
                                      font size color align
                                      (space-between 3)
                                      (space-before 3) (space-after 5)
                                      hyphenate language)
  "Add a numbered list.  ITEMS is a list of strings or lists of runs;
numbers start at START (1) and are rendered with FORMAT (\"%d.\").
All markers are right-aligned in a common column so the text edges
line up."
  (let* ((flow niori-pdf--flow)
         (font-name (or font (niori-pdf-flow-paragraph-font flow)))
         (size (or size (niori-pdf-flow-paragraph-size flow)))
         (fnt (niori-pdf-get-font (niori-pdf-flow-doc flow) font-name))
         (marks (let ((n start) (out nil))
                  (dolist (_ items)
                    (push (format format n) out)
                    (cl-incf n))
                  (nreverse out)))
         (mark-w (+ (cl-reduce #'max marks
                               :key (lambda (m)
                                      (niori-pdf-text-width m fnt size))
                               :initial-value 0.0)
                    gap))
         (leading (* size (niori-pdf-flow-default-leading flow))))
    (niori-pdf-flow--boundary flow)
    (niori-pdf-flow--advance flow space-before)
    (cl-mapc
     (lambda (item mark)
       (niori-pdf-flow--ensure-space
        flow (+ (* 3.2 leading) space-between))
       (let ((marker-y (+ (niori-pdf-flow-y flow) (* 0.82 size)))
             (mw (niori-pdf-text-width mark fnt size)))
         (niori-pdf-flow--paragraph-block
          item :font font-name :size size :color color :align align
          :indent (+ indent mark-w) :space-after space-between
          :hyphenate (not (eq hyphenate nil)) :language language)
         ;; right-align the marker inside the marker column
         (niori-pdf-flow--list-item-marker
          flow (+ (niori-pdf-flow-x0 flow) indent (- mark-w mw gap))
          marker-y
          mark font-name size (or marker-color '(0.12 0.12 0.12)))))
     items marks)
    (niori-pdf-flow--advance flow (- space-after space-between))))

(cl-defun niori-pdf-flow-rule (&key (thickness 0.7)
                                    (color '(0.72 0.72 0.72))
                                    indent
                                    (space-before 4) (space-after 7))
  "Add a horizontal rule across the text area.
When floats are active the rule is shortened to the space left over
beside them."
  (niori-pdf-flow--boundary niori-pdf--flow)
  (niori-pdf-flow--ensure-space niori-pdf--flow
                                (+ space-before space-after thickness))
  (niori-pdf-flow--advance niori-pdf--flow space-before)
  (let* ((flow niori-pdf--flow)
         (intr (niori-pdf-flow--float-intrusion
                flow (niori-pdf-flow-y flow)
                (+ (niori-pdf-flow-y flow) thickness)))
         (x0 (+ (niori-pdf-flow-x0 flow) (car intr) (or indent 0)))
         (x1 (- (niori-pdf-flow-x1 flow) (cadr intr) (or indent 0)))
         (y (niori-pdf-flow--pdf-y flow
                                  (+ (niori-pdf-flow-y flow) 1))))
    (niori-pdf-flow--with-graphics flow
      (apply #'niori-pdf-set-rgb-stroke color)
      (niori-pdf-set-line-width thickness)
      (niori-pdf-move-to x0 y)
      (niori-pdf-line-to x1 y)
      (niori-pdf-stroke)))
  (niori-pdf-flow--advance niori-pdf--flow (+ thickness space-after)))

;;;; Images

(cl-defun niori-pdf-flow-image (path &key width max-height align caption)
  "Add an image block from PATH scaled to WIDTH points (fit MAX-HEIGHT)."
  (let* ((image (niori-pdf-image-from-file path))
         (iw (niori-pdf-image-width image))
         (ih (niori-pdf-image-height image))
         (w0 (or width (min iw (niori-pdf-flow--width niori-pdf--flow))))
         (h0 (* ih (/ w0 (float iw))))
         (h (if (and max-height (> h0 max-height)) max-height h0))
         (w (if (/= h h0) (* w0 (/ h (float h0))) w0)))
    (niori-pdf-flow--boundary niori-pdf--flow)
    (niori-pdf-flow--ensure-space niori-pdf--flow (+ h 4))
    (let ((x (pcase (or align 'center)
               (`center (+ (niori-pdf-flow-x0 niori-pdf--flow)
                           (/ (- (niori-pdf-flow--width niori-pdf--flow) w)
                              2.0)))
               (`right (- (niori-pdf-flow-x1 niori-pdf--flow) w))
               (_ (niori-pdf-flow-x0 niori-pdf--flow)))))
      (niori-pdf-flow--with-graphics niori-pdf--flow
        (niori-pdf-draw-image image x
                              (niori-pdf-flow--pdf-y
                               niori-pdf--flow
                               (+ (niori-pdf-flow-y niori-pdf--flow) h))
                              w h))
      (niori-pdf-flow--advance niori-pdf--flow h))
    (when caption
      (niori-pdf-flow-paragraph caption :align 'center :size 8.5
                                :space-before 3 :space-after 6))))

;;;; Floats

(cl-defun niori-pdf-flow-float (builder &key (width 160) (side 'left)
                                        (gap 10))
  "Float the blocks produced by BUILDER (a thunk) to SIDE.
The floated content is placed at the current position, WIDTH points
wide, and subsequent paragraphs on this page wrap around it (their
lines are shortened by `niori-pdf-flow--float-intrusion').  GAP
separates the float from the text.  Floats do not survive a page
break; rules also respect them, other block types use the full text
width."
  (let* ((flow niori-pdf--flow)
         (inner (copy-niori-pdf-flow flow))
         (fx (if (eq side 'right)
                 (- (niori-pdf-flow-x1 flow) width)
               (niori-pdf-flow-x0 flow))))
    (setf (niori-pdf-flow-x0 inner) fx
          (niori-pdf-flow-x1 inner) (+ fx width)
          (niori-pdf-flow-margin-top inner) 0
          (niori-pdf-flow-margin-bottom inner) 0)
    (let ((old-y (niori-pdf-flow-y flow)))
      (setf (niori-pdf-flow-y inner) old-y)
      (let ((cap (niori-pdf-flow--capture flow inner builder))
            (h 0.0))
        (setq h (- (cdr cap) old-y))
        ;; a float must fit on the page: break first, then re-run
        (niori-pdf-flow--ensure-space flow h)
        (when (/= (niori-pdf-flow-y flow) old-y)
          (setf (niori-pdf-flow-y inner)
                (niori-pdf-flow-y flow))
          (setq cap (niori-pdf-flow--capture flow inner builder)
                h (- (cdr cap) (niori-pdf-flow-y flow))))
        (dolist (chunk (car cap))
          (niori-pdf-flow--emit flow chunk))
        (push (list :side side :x0 fx :x1 (+ fx width)
                    :y0 (niori-pdf-flow-y flow)
                    :y1 (+ (niori-pdf-flow-y flow) h)
                    :gap gap)
              (niori-pdf-flow-floats flow))
        ;; the cursor stays: following blocks flow beside the float
        (niori-pdf-flow--boundary flow)))))

;;;; Boxes and column rows
;;
;; The content of a box/column is rendered by a nested flow whose x
;; bounds are narrowed and whose cursor starts at the real page
;; position; its operator output is captured, then drawn after the
;; background/border rectangles.

(defun niori-pdf-flow--capture (_flow inner builder)
  "Run BUILDER with niori-pdf--flow bound to INNER, capturing output.
Returns (CONTENT-CHUNKS . HEIGHT)."
  (let ((scratch nil))
    (cl-letf (((symbol-function 'niori-pdf-flow--emit)
               (lambda (_fl str) (push str scratch)))
              ((symbol-function 'niori-pdf-flow--emitf)
               (lambda (_fl fmt &rest args)
                 (push (apply #'format fmt args) scratch)))
              ;; also capture raw content-level operators (used by
              ;; charts drawn inside boxes/columns)
              ((symbol-function 'niori-pdf--emit)
               (lambda (str) (push str scratch)))
              ((symbol-function 'niori-pdf--emitf)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) scratch)))
              ((symbol-function 'niori-pdf-flow--ensure-space)
               (lambda (_fl _h) nil))
              ((symbol-function 'niori-pdf-flow--reset-page)
               (lambda (_fl) nil)))
      (let ((niori-pdf--flow inner))
        (niori-pdf-flow--with-graphics inner (funcall builder))))
    (cons (nreverse scratch) (niori-pdf-flow-y inner))))

(cl-defun niori-pdf-flow-box (builder &key background border
                                      (padding 8) width align)
  "Draw a box with the blocks produced by BUILDER (a thunk).
BACKGROUND/BORDER are RGB lists; WIDTH fixes the box width (default:
full text width); ALIGN centers a narrower box.  When the box does
not fit and triggers a page break, the builder is run again at the
new position so the captured operators carry fresh coordinates."
  (let* ((box-w (or width (niori-pdf-flow--width niori-pdf--flow)))
         (box-x (pcase (or align 'left)
                  (`center (+ (niori-pdf-flow-x0 niori-pdf--flow)
                              (/ (- (niori-pdf-flow--width niori-pdf--flow)
                                    box-w)
                                 2.0)))
                  (`right (- (niori-pdf-flow-x1 niori-pdf--flow) box-w))
                  (_ (niori-pdf-flow-x0 niori-pdf--flow))))
         (inner (copy-niori-pdf-flow niori-pdf--flow)))
    (setf (niori-pdf-flow-x0 inner) (+ box-x padding)
          (niori-pdf-flow-x1 inner) (+ box-x box-w (- padding))
          (niori-pdf-flow-margin-top inner) 0
          (niori-pdf-flow-margin-bottom inner) 0)
    (niori-pdf-flow--boundary niori-pdf--flow)
    (let* ((old-y (niori-pdf-flow-y niori-pdf--flow))
           (cap (progn
                  (setf (niori-pdf-flow-y inner) (+ old-y padding))
                  (niori-pdf-flow--capture niori-pdf--flow inner builder)))
           (h (+ (- (cdr cap) old-y) padding)))
      (niori-pdf-flow--ensure-space niori-pdf--flow h)
      (when (/= (niori-pdf-flow-y niori-pdf--flow) old-y)
        ;; a page break moved the origin: re-run the builder so text
        ;; and graphics land inside the box on the new page
        (setf (niori-pdf-flow-y inner)
              (+ (niori-pdf-flow-y niori-pdf--flow) padding))
        (setq cap (niori-pdf-flow--capture niori-pdf--flow inner builder)
              h (+ (- (cdr cap) (niori-pdf-flow-y niori-pdf--flow))
                   padding)))
      (let ((content (car cap))
            (y0 (niori-pdf-flow-y niori-pdf--flow)))
        (niori-pdf-flow--with-graphics niori-pdf--flow
          (when background
            (apply #'niori-pdf-set-rgb-fill background)
            (niori-pdf-rectangle box-x
                                 (niori-pdf-flow--pdf-y
                                  niori-pdf--flow (+ y0 h))
                                 box-w h)
            (niori-pdf-fill))
          (when border
            (apply #'niori-pdf-set-rgb-stroke border)
            (niori-pdf-set-line-width 0.8)
            (niori-pdf-rectangle box-x
                                 (niori-pdf-flow--pdf-y
                                  niori-pdf--flow (+ y0 h))
                                 box-w h)
            (niori-pdf-stroke)))
        (dolist (chunk content)
          (niori-pdf-flow--emit niori-pdf--flow chunk))
        (niori-pdf-flow--advance niori-pdf--flow h)))))

(cl-defun niori-pdf-flow-columns (builders &key widths (gap 10) row-height)
  "Lay out BUILDERS (list of thunks) side by side.
WIDTHS is a list of column widths (default: equal); the row height is
ROW-HEIGHT or the maximum column height.  When the row triggers a
page break, the builders are run again at the new position so the
captured operators carry fresh coordinates."
  (let* ((n (length builders))
         (total (niori-pdf-flow--width niori-pdf--flow))
         (widths (or widths
                     (make-list n (/ (- total (* gap (1- n)))
                                     (float n)))))
         ;; the given widths must fit the text area: scale them down
         ;; proportionally when widths + gaps would overflow the page
         ;; (a too-wide row would push the last columns off the page)
         (widths (let ((sum (+ (cl-reduce #'+ widths :initial-value 0.0)
                               (* gap (1- n)))))
                   (if (and (> sum total) (> sum 0))
                       (mapcar (lambda (w)
                                 (* w (/ (- total (* gap (1- n))) sum)))
                               widths)
                     widths)))
         (x-positions nil)
         (x (niori-pdf-flow-x0 niori-pdf--flow))
         (inners (make-vector n nil))
         (contents (make-vector n nil))
         (heights (make-vector n 0)))
    (dotimes (i n)
      (push x x-positions)
      (setq x (+ x (nth i widths) gap)))
    (setq x-positions (nreverse x-positions))
    (let ((row-h
           (progn
             (dotimes (i n)
               (let ((inner (copy-niori-pdf-flow niori-pdf--flow)))
                 (setf (niori-pdf-flow-x0 inner) (nth i x-positions)
                       (niori-pdf-flow-x1 inner)
                       (+ (nth i x-positions) (nth i widths))
                       (niori-pdf-flow-y inner)
                       (niori-pdf-flow-y niori-pdf--flow)
                       (niori-pdf-flow-margin-top inner) 0
                       (niori-pdf-flow-margin-bottom inner) 0)
                 (aset inners i inner)
                 (let ((cap (niori-pdf-flow--capture
                             niori-pdf--flow inner (nth i builders))))
                   (aset contents i (car cap))
                   (aset heights i (- (cdr cap)
                                      (niori-pdf-flow-y
                                       niori-pdf--flow))))))
             (or row-height
                 (cl-reduce #'max heights :initial-value 0)))))
      (let ((old-y (niori-pdf-flow-y niori-pdf--flow)))
        (niori-pdf-flow--ensure-space niori-pdf--flow row-h)
        (when (/= (niori-pdf-flow-y niori-pdf--flow) old-y)
          ;; page break: re-run the builders at the new position
          (dotimes (i n)
            (let ((inner (aref inners i)))
              (setf (niori-pdf-flow-y inner)
                    (niori-pdf-flow-y niori-pdf--flow))
              (let ((cap (niori-pdf-flow--capture
                          niori-pdf--flow inner (nth i builders))))
                (aset contents i (car cap)))))))
      (dotimes (i n)
        (dolist (chunk (aref contents i))
          (niori-pdf-flow--emit niori-pdf--flow chunk)))
      (niori-pdf-flow--advance niori-pdf--flow row-h))))

;;;; Tables

(cl-defstruct niori-pdf-flow--tbl
  flow col-w padding header-bg header-fg borders total leading
  default-aligns body-font body-size header)

(defun niori-pdf-flow--tbl-row-spec (row)
  "Normalize a table ROW.
A row is a list or vector of cells, or a plist with :cells,
:keep-with-next and/or :background.  Returns the plist form."
  (cond
   ((vectorp row)
    (list :cells (append row nil)))
   ((and (consp row) (keywordp (car row)))
    (let ((c (plist-get row :cells)))
      (plist-put (copy-sequence row) :cells
                 (if (vectorp c) (append c nil) c))))
   (t (list :cells row))))

(defun niori-pdf-flow--tbl-cell-lines (tbl cell col is-header)
  "Normalize one table CELL of TBL placed at column COL.
A cell may span columns with :colspan and rows with :rowspan, align
vertically with :valign top|center|bottom.  Returns a layout plist;
the cell width is the sum of the spanned column widths."
  (let* ((plist (and (listp cell) (not (stringp cell)) cell))
         (col-w (niori-pdf-flow--tbl-col-w tbl))
         (cspan (max 1 (or (and plist (plist-get plist :colspan)) 1)))
         (rspan (max 1 (or (and plist (plist-get plist :rowspan)) 1)))
         (text (if plist (plist-get plist :text)
                 (if (stringp cell) cell "")))
         (fname (or (and plist (plist-get plist :font))
                    (if is-header "Helvetica-Bold"
                      (niori-pdf-flow--tbl-body-font tbl))))
         (size (or (and plist (plist-get plist :size))
                   (niori-pdf-flow--tbl-body-size tbl)))
         (color (or (and plist (plist-get plist :color))
                    (if is-header (niori-pdf-flow--tbl-header-fg tbl)
                      '(0.12 0.12 0.12))))
         (align (or (and plist (plist-get plist :align))
                    (nth col (niori-pdf-flow--tbl-default-aligns tbl))
                    'left))
         (bg (and plist (plist-get plist :background)))
         (valign (or (and plist (plist-get plist :valign)) 'top))
         (w (cl-reduce #'+ (cl-subseq col-w col
                                      (min (length col-w)
                                           (+ col cspan)))
                       :initial-value 0.0))
         (flow (niori-pdf-flow--tbl-flow tbl))
         (inner-w (- w (* 2 (niori-pdf-flow--tbl-padding tbl))))
         (runs (list (list text :font fname :size size :color color)))
         (pb (niori-pdf-flow--paragraph-items
              flow runs :hyphenate nil :align align))
         (items (car pb)) (plain (cdr pb))
         (breaks (niori-pdf-ts-break-paragraph items inner-w plain)))
    (list :font-name fname :size size :color color
          :align align :bg bg :width w :valign valign
          :cspan cspan :rspan rspan :col col
          :items items :breaks breaks)))

(defun niori-pdf-flow--tbl-place (rows n-cols)
  "Place the cells of the normalized ROW SPECS into an N-COLS grid.
Returns (PLACEMENTS . OWNERS): placements is a vector with, per row,
the list of placed cells ((:col :cspan :rspan :cell :ci)); owners is
a vector of vectors mapping (row, col) to (ROW . CI) of the cell
covering it."
  (let* ((n (length rows))
         (placements (make-vector n nil))
         (owners (make-vector n nil)))
    (dotimes (r n)
      (aset owners r (make-vector n-cols nil)))
    (dotimes (r n)
      (let ((c 0) (ci -1))
        (dolist (cell (plist-get (elt rows r) :cells))
          (cl-incf ci)
          (while (and (< c n-cols) (aref (elt owners r) c))
            (setq c (1+ c)))
          (when (< c n-cols)
            (let* ((plist (and (consp cell) (not (stringp cell)) cell))
                   (cs (max 1 (min (or (and plist
                                            (plist-get plist :colspan))
                                       1)
                                   (- n-cols c))))
                   (rs (max 1 (min (or (and plist
                                            (plist-get plist :rowspan))
                                       1)
                                   (- n r)))))
              (push (list :col c :cspan cs :rspan rs :cell cell :ci ci)
                    (elt placements r))
              (dotimes (dr rs)
                (when (< (+ r dr) n)
                  (dotimes (dc cs)
                    (when (< (+ c dc) n-cols)
                      (aset (elt owners (+ r dr)) (+ c dc)
                            (cons r ci))))))
              (setq c (+ c cs)))))))
    (dotimes (r n)
      (aset placements r (nreverse (elt placements r))))
    (cons placements owners)))

(defun niori-pdf-flow--tbl-heights (tbl rows placements)
  "Per-row heights for the placed grid, honoring rowspans.
A cell spanning several rows contributes its required height to the
SUM of the covered rows; a deficit is spread equally over them."
  (let* ((n (length rows))
         (leading (niori-pdf-flow--tbl-leading tbl))
         (padding (niori-pdf-flow--tbl-padding tbl))
         (heights (make-vector n (niori-pdf-flow--tbl-leading tbl)))
         (spanned nil))
    (dotimes (r n)
      (let ((h (niori-pdf-flow--tbl-leading tbl)))
        (dolist (p (elt placements r))
          (let* ((lay (niori-pdf-flow--tbl-cell-lines
                       tbl (plist-get p :cell) (plist-get p :col)
                       (= r 0)))
                 (need (+ (* (length (plist-get lay :breaks)) leading)
                          (* 2 padding))))
            (if (= (plist-get p :rspan) 1)
                (setq h (max h need))
              (push (list r (plist-get p :rspan) need) spanned))))
        (aset heights r (max h (aref heights r)))))
    (dolist (sp spanned)
      (let* ((r (nth 0 sp)) (rs (nth 1 sp)) (need (nth 2 sp))
             (sum 0.0))
        (dotimes (k rs)
          (when (< (+ r k) n)
            (setq sum (+ sum (aref heights (+ r k))))))
        (when (< sum need)
          (let ((d (/ (- need sum) (float rs))))
            (dotimes (k rs)
              (when (< (+ r k) n)
                (cl-incf (aref heights (+ r k)) d)))))))
    heights))

(defun niori-pdf-flow--tbl-x-at (col col-w x0)
  "X coordinate of column boundary COL given COL-W and table start X0."
  (let ((x x0))
    (dotimes (k col)
      (setq x (+ x (nth k col-w))))
    x))

(cl-defun niori-pdf-flow--tbl-draw-row (tbl rows placements owners
                                            heights r prev-owners)
  "Draw row R of the placed table TBL.
PREV-OWNERS is the owner vector of the row above (nil at a page
start); horizontal and vertical grid lines are only drawn where the
cell ownership actually changes, so spanning cells stay open.
Returns the owner vector of the row just drawn."
  (let* ((flow (niori-pdf-flow--tbl-flow tbl))
         (leading (niori-pdf-flow--tbl-leading tbl))
         (padding (niori-pdf-flow--tbl-padding tbl))
         (col-w (niori-pdf-flow--tbl-col-w tbl))
         (n-cols (length col-w))
         (total (niori-pdf-flow--tbl-total tbl))
         (row-spec (elt rows r))
         (is-header (and (= r 0) (niori-pdf-flow--tbl-header tbl)))
         (h (aref heights r))
         (x0 (niori-pdf-flow-x0 flow))
         ;; a rowspan must not cross a page break, and a
         ;; :keep-with-next row travels with its successor; both raise
         ;; the height a break must leave room for (plus the header a
         ;; page break would repeat)
         (span-h
          (let ((s 0.0))
            (dolist (p (elt placements r))
              (when (> (plist-get p :rspan) 1)
                (let ((sh 0.0))
                  (dotimes (k (plist-get p :rspan))
                    (when (< (+ r k) (length rows))
                      (setq sh (+ sh (aref heights (+ r k))))))
                  (setq s (max s sh)))))
            s))
         (keep-next
          (and (< (1+ r) (length rows))
               (plist-get (elt rows r) :keep-with-next)
               (aref heights (1+ r))))
         (need (max h span-h
                    (if keep-next (+ h (or keep-next 0.0)) 0.0)))
         (repeat (and (not is-header)
                      (niori-pdf-flow--tbl-header tbl))))
    ;; page break: repeat the header row when rows overflow the page
    (when (> (+ (niori-pdf-flow-y flow) need
                (if repeat (aref heights 0) 0.0))
             (niori-pdf-flow--bottom flow))
      (niori-pdf-flow--reset-page flow t)
      (when repeat
        (setq prev-owners
              (niori-pdf-flow--tbl-draw-row
               tbl rows placements owners heights 0 nil))))
    (let* ((y0 (niori-pdf-flow-y flow))
           (own (elt owners r)))
      ;; backgrounds
      (niori-pdf-flow--with-graphics flow
        (when is-header
          (apply #'niori-pdf-set-rgb-fill
                 (niori-pdf-flow--tbl-header-bg tbl))
          (niori-pdf-rectangle x0 (niori-pdf-flow--pdf-y flow (+ y0 h))
                               total h)
          (niori-pdf-fill))
        (when (plist-get row-spec :background)
          (apply #'niori-pdf-set-rgb-fill
                 (plist-get row-spec :background))
          (niori-pdf-rectangle x0 (niori-pdf-flow--pdf-y flow (+ y0 h))
                               total h)
          (niori-pdf-fill))
        (dolist (p (elt placements r))
          (let* ((lay (niori-pdf-flow--tbl-cell-lines
                       tbl (plist-get p :cell) (plist-get p :col)
                       is-header))
                 (bg (plist-get lay :bg)))
            (when bg
              (let ((ch h)
                    (rs (plist-get p :rspan)))
                (when (> rs 1)
                  (setq ch 0.0)
                  (dotimes (k rs)
                    (when (< (+ r k) (length rows))
                      (setq ch (+ ch (aref heights (+ r k)))))))
                (apply #'niori-pdf-set-rgb-fill bg)
                (niori-pdf-rectangle
                 (niori-pdf-flow--tbl-x-at (plist-get p :col) col-w x0)
                 (niori-pdf-flow--pdf-y flow (+ y0 ch))
                 (plist-get lay :width) ch)
                (niori-pdf-fill))))))
      ;; grid lines: horizontal segments where ownership changes,
      ;; vertical boundaries where owners differ
      (when (niori-pdf-flow--tbl-borders tbl)
        (apply #'niori-pdf-set-rgb-stroke
               (niori-pdf-flow--tbl-borders tbl))
        (niori-pdf-set-line-width 0.6)
        (let ((c 0))
          (while (< c n-cols)
            (when (or (null prev-owners)
                      (not (equal (aref prev-owners c) (aref own c))))
              (niori-pdf-move-to
               (niori-pdf-flow--tbl-x-at c col-w x0)
               (niori-pdf-flow--pdf-y flow y0))
              (niori-pdf-line-to
               (+ x0 total) (niori-pdf-flow--pdf-y flow y0)))
            (setq c (1+ c))))
        (dotimes (b (1+ n-cols))
          (when (or (= b 0) (= b n-cols)
                    (not (equal (aref own (1- b)) (aref own b))))
            (let ((xb (niori-pdf-flow--tbl-x-at
                       (if (= b n-cols) n-cols b) col-w x0)))
              (niori-pdf-move-to xb (niori-pdf-flow--pdf-y flow y0))
              (niori-pdf-line-to xb
                                 (niori-pdf-flow--pdf-y flow (+ y0 h))))))
        (niori-pdf-stroke))
      ;; cell text: cells starting at this row only; vertical
      ;; alignment inside the spanned height
      (dolist (p (elt placements r))
        (let* ((lay (niori-pdf-flow--tbl-cell-lines
                     tbl (plist-get p :cell) (plist-get p :col)
                     is-header))
               (cw (plist-get lay :width))
               (size (plist-get lay :size))
               (align (plist-get lay :align))
               (valign (plist-get lay :valign))
               (items (plist-get lay :items))
               (inner-w (- cw (* 2 padding)))
               (nlines (length (plist-get lay :breaks)))
               (ch (if (> (plist-get p :rspan) 1)
                       (let ((s 0.0))
                         (dotimes (k (plist-get p :rspan))
                           (when (< (+ r k) (length rows))
                             (setq s (+ s (aref heights (+ r k))))))
                         s)
                     h))
               (free (max 0.0 (- ch (* 2 padding)
                                 (* nlines leading))))
               (voff (pcase valign
                       (`center (/ free 2.0))
                       (`bottom free)
                       (_ 0.0)))
               (ly (+ y0 padding voff))
               (cx (+ (niori-pdf-flow--tbl-x-at
                       (plist-get p :col) col-w x0)
                      padding)))
          (dolist (brk (plist-get lay :breaks))
            (let* ((tl (niori-pdf-ts-line
                        items brk :justify (eq align 'justified)))
                   (line (car tl))
                   (widths (cdr tl)))
              (niori-pdf-flow--render-line
               flow cx (+ ly (* 0.82 size)) line widths brk
               :avail inner-w :align align)
              (setq ly (+ ly leading))))))
      (niori-pdf-flow--advance flow h)
      own)))

(defun niori-pdf-flow--tbl-chunks (word)
  "Split WORD at its explicit hyphens into unbreakable chunks.
A line may break after a hyphen, so \"bar-codes\" yields the
chunks \"bar-\" and \"codes\"; \"Dependencies\" stays whole."
  (let ((chunks nil) (start 0))
    (dotimes (i (length word))
      (when (= (aref word i) ?-)
        (push (substring word start (1+ i)) chunks)
        (setq start (1+ i))))
    (push (substring word start) chunks)
    (nreverse chunks)))

(defun niori-pdf-flow--tbl-min-word-width (text font size)
  "Width of the widest unbreakable chunk of cell TEXT at FONT/SIZE."
  (let ((max-w 0.0))
    (dolist (word (split-string text "[ \t\n\r]+" t))
      (dolist (chunk (niori-pdf-flow--tbl-chunks word))
        (setq max-w (max max-w (niori-pdf-text-width chunk font size)))))
    max-w))

(cl-defun niori-pdf-flow--fit-col-widths (row-specs header-p
                                                    col-w padding
                                                    body-font body-size)
  "Adjust COL-W so that no unbreakable cell chunk overflows a column.
Every column whose minimum content width (widest chunk plus cell
padding) exceeds its width is widened; the required extra width is
taken from the columns with surplus, proportionally to their surplus,
so the table's total width is preserved exactly.  Cells spanning more
than one column are not measured (their width distributes over
several columns).  When the minimum widths exceed the total, they are
scaled down to it (the affected cells render as over-full boxes, like
TeX's \\hboxes)."
  (let* ((n (length col-w))
         (target (cl-reduce #'+ col-w :initial-value 0.0))
         (mins (make-vector n 0.0)))
    ;; per-column minimum: widest chunk of any single-column cell
    (dotimes (r (length row-specs))
      (let ((c 0))
        (dolist (cell (plist-get (elt row-specs r) :cells))
          (let* ((plist (and (consp cell) (not (stringp cell)) cell))
                 (cs (or (and plist (plist-get plist :colspan)) 1))
                 (text (cond ((stringp cell) cell)
                             (plist (or (plist-get plist :text) ""))
                             (t "")))
                 (fname (or (and plist (plist-get plist :font))
                            (if (and header-p (= r 0))
                                "Helvetica-Bold"
                              body-font)))
                 (fsize (or (and plist (plist-get plist :size))
                            body-size)))
            (when (= cs 1)
              (when (> (length text) 0)
                (aset mins c
                      (max (aref mins c)
                           (+ (niori-pdf-flow--tbl-min-word-width
                               text fname fsize)
                              (* 2.0 padding))))))
            (setq c (+ c cs))))))
    (let ((mins-lst (append mins nil))
          (sum-mins (cl-reduce #'+ (append mins nil) :initial-value 0.0)))
      (if (<= sum-mins target)
          (let* ((extra (- target sum-mins))
                 (surplus (cl-mapcar (lambda (w m) (max 0.0 (- w m)))
                                     (append col-w nil) mins-lst))
                 (sum-surplus (cl-reduce #'+ surplus :initial-value 0.0)))
            (if (> sum-surplus 0)
                ;; every column keeps its minimum, the surplus above it
                ;; shrinks proportionally to fund the deficient columns
                (cl-mapcar (lambda (m s)
                             (+ m (* extra (/ s sum-surplus))))
                           mins-lst surplus)
              ;; all columns sit at their minimum: spread evenly
              (mapcar (lambda (m) (+ m (/ extra (max 1 n)))) mins-lst)))
        ;; minimums exceed the total: scale them down to it
        (mapcar (lambda (m) (* m (/ target sum-mins))) mins-lst)))))

(cl-defun niori-pdf-flow-table (spec)
  "Add a table block.  SPEC plist keys:
:cols          list of column widths (points), or :auto to divide evenly
:header        header row (list of cells, or a row plist)
:rows          list of body rows; a row is a list of cells or a plist
               (:cells :keep-with-next :background)
:cell          cell = string, or plist (:text :font :size :color
               :align :background :colspan :rowspan :valign)
:padding       cell padding (default 4)
:header-bg     RGB list for the header background (default dark blue)
:header-fg     RGB list for header text (default white)
:borders       RGB list for grid lines (default light gray; nil = none)
:font :size    default body cell font
:align         default per-column alignment list
:space-after   vertical space between the closing rule and the
               next block (default 0)"
  (let* ((header (plist-get spec :header))
         (rows (or (plist-get spec :rows) nil))
         (row-specs (append (when header
                              (list (niori-pdf-flow--tbl-row-spec header)))
                            (mapcar #'niori-pdf-flow--tbl-row-spec rows)))
         (n-cols (length (or (plist-get (elt row-specs 0) :cells)
                             (car rows) '(""))))
         ;; resolve the column widths first: the table's total width is
         ;; the SUM of the columns (not necessarily the full text
         ;; width, so backgrounds and grid lines stay inside the table)
         (col-w (let* ((cols (plist-get spec :cols))
                       (total (niori-pdf-flow--width niori-pdf--flow))
                       (raw (cond
                             ((not cols)
                              (make-list n-cols (/ (float total)
                                                   (max 1 n-cols))))
                             (t
                              ;; expand per-column :auto entries:
                              ;; each gets an equal share of the
                              ;; remaining width
                              (let* ((n-auto (cl-count-if
                                              (lambda (c) (eq c :auto))
                                              cols))
                                     (fixed (cl-reduce #'+ cols
                                                       :initial-value 0.0
                                                       :key (lambda (c)
                                                              (if (eq c :auto)
                                                                  0 c)))))
                                (mapcar (lambda (c)
                                          (if (eq c :auto)
                                              (if (> n-auto 0)
                                                  (/ (- (float total) fixed)
                                                     n-auto)
                                                0.0)
                                            c))
                                        cols)))))
                       ;; a table wider than the text area would run
                       ;; off the page: scale all columns down
                       ;; proportionally (a narrower table keeps its
                       ;; specified widths)
                       (sum (cl-reduce #'+ raw :initial-value 0.0)))
                  (if (and (> sum total) (> sum 0))
                      (mapcar (lambda (w) (* w (/ total sum))) raw)
                    raw)))
         ;; no single word may be wider than its column: widen the
         ;; deficient columns (funded by the surplus ones)
         (tbl-padding (or (plist-get spec :padding) 4))
         (tbl-font (or (plist-get spec :font)
                       (niori-pdf-flow-default-font niori-pdf--flow)))
         (tbl-size (or (plist-get spec :size)
                       (niori-pdf-flow-default-size niori-pdf--flow)))
         (col-w (niori-pdf-flow--fit-col-widths
                 row-specs header col-w tbl-padding
                 tbl-font tbl-size))
         (tbl (make-niori-pdf-flow--tbl
               :flow niori-pdf--flow
               :col-w col-w
               :padding tbl-padding
               :header-bg (or (plist-get spec :header-bg)
                              '(0.13 0.25 0.51))
               :header-fg (or (plist-get spec :header-fg) '(1 1 1))
               :borders (or (plist-get spec :borders) '(0.78 0.78 0.78))
               :total (cl-reduce #'+ col-w :initial-value 0.0)
               :leading (* (or (plist-get spec :size)
                               (niori-pdf-flow-default-size niori-pdf--flow))
                           1.32)
               :default-aligns (plist-get spec :align)
               :body-font (or (plist-get spec :font)
                              (niori-pdf-flow-default-font niori-pdf--flow))
               :body-size (or (plist-get spec :size)
                              (niori-pdf-flow-default-size
                               niori-pdf--flow)))))
    (niori-pdf-flow--boundary niori-pdf--flow)
    (when header
      (setf (niori-pdf-flow--tbl-header tbl) header))
    (let* ((placed (niori-pdf-flow--tbl-place row-specs n-cols))
           (placements (car placed))
           (owners (cdr placed))
           (heights (niori-pdf-flow--tbl-heights
                     tbl row-specs placements))
           (n (length row-specs))
           (prev nil))
      (dotimes (r n)
        (setq prev (niori-pdf-flow--tbl-draw-row
                    tbl row-specs placements owners heights r prev)))
      ;; closing rule at the bottom of the table
      (when (niori-pdf-flow--tbl-borders tbl)
        (niori-pdf-flow--with-graphics
         (niori-pdf-flow--tbl-flow tbl)
         (apply #'niori-pdf-set-rgb-stroke
                (niori-pdf-flow--tbl-borders tbl))
         (niori-pdf-set-line-width 0.6)
         (niori-pdf-move-to
          (niori-pdf-flow-x0 (niori-pdf-flow--tbl-flow tbl))
          (niori-pdf-flow--pdf-y
           (niori-pdf-flow--tbl-flow tbl)
           (niori-pdf-flow-y (niori-pdf-flow--tbl-flow tbl))))
         (niori-pdf-line-to
          (+ (niori-pdf-flow-x0 (niori-pdf-flow--tbl-flow tbl))
             (niori-pdf-flow--tbl-total tbl))
          (niori-pdf-flow--pdf-y
           (niori-pdf-flow--tbl-flow tbl)
           (niori-pdf-flow-y (niori-pdf-flow--tbl-flow tbl))))
         (niori-pdf-stroke)))
      ;; vertical space between the closing rule and the next block
      (niori-pdf-flow--advance
       niori-pdf--flow
       (or (plist-get spec :space-after) 0)))))

;;;; Reference points (multi-pass cross references)

(defun niori-pdf-flow-mark-ref-point (id &optional data)
  "Mark a reference point ID at the current position.
DATA (any Lisp value) is carried along.  On the next pass of
`niori-pdf-flow-generate' the point can be resolved with
`niori-pdf-flow-ref-page-number' / `niori-pdf-flow-ref-data'."
  (let ((flow niori-pdf--flow))
    (setf (niori-pdf-flow-refs flow)
          (cons (cons id (list :page (niori-pdf-flow-page-number flow)
                               :y (niori-pdf-flow-y flow)
                               :data data))
                (cl-remove-if (lambda (e) (eq (car e) id))
                              (niori-pdf-flow-refs flow))))))

(defun niori-pdf-flow-ref (id)
  "Reference data for ID from the previous pass, or nil on the first
pass (an alist entry with :page :y :data)."
  (cdr (assq id niori-pdf-flow--pass-refs)))

(defun niori-pdf-flow-ref-page-number (id &optional default)
  "Page number of reference point ID (DEFAULT on the first pass)."
  (or (plist-get (niori-pdf-flow-ref id) :page) default))

(defun niori-pdf-flow-ref-data (id)
  "DATA recorded with reference point ID (nil on the first pass)."
  (plist-get (niori-pdf-flow-ref id) :data))

(cl-defun niori-pdf-flow-put-ref-page-number (id &key font size color
                                                 (align 'right)
                                                 (unresolved "\u2014"))
  "Typeset the page number of reference point ID, flush right (or
per ALIGN).
On the first pass UNRESOLVED is shown instead; the document is
regenerated until the number is correct.  The value is also exposed
to text content via `niori-pdf-flow-ref-page-number'."
  (let* ((flow niori-pdf--flow)
         (font-name (or font (niori-pdf-flow-paragraph-font flow)))
         (size (or size (niori-pdf-flow-paragraph-size flow)))
         (text (format "%s" (or (niori-pdf-flow-ref-page-number id)
                                 unresolved)))
         (fnt (niori-pdf-get-font (niori-pdf-flow-doc flow) font-name))
         (w (niori-pdf-text-width text fnt size))
         (x (pcase align
              (`right (- (niori-pdf-flow-x1 flow) w))
              (`center (+ (niori-pdf-flow-x0 flow)
                          (/ (- (niori-pdf-flow--width flow) w) 2.0)))
              (_ (niori-pdf-flow-x0 flow))))
         (y-base (+ (niori-pdf-flow-y flow) (* 0.82 size))))
    (niori-pdf-flow--ensure-space flow (* 1.5 size))
    (niori-pdf-flow--with-graphics flow
      (niori-pdf-flow--emitf flow "BT\n")
      (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                             (niori-pdf-num x)
                             (niori-pdf-num
                              (niori-pdf-flow--pdf-y flow y-base)))
      (apply #'niori-pdf-flow--emitf flow "%s %s %s rg\n"
             (mapcar #'niori-pdf-num (or color '(0.12 0.12 0.12))))
      (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                             (niori-pdf-font-resource fnt)
                             (niori-pdf-num size))
      (niori-pdf-flow--emit
       flow (niori-pdf--encoded-tj-operator
             (niori-pdf-font-encode text fnt) fnt 'kerned))
      (niori-pdf-flow--emitf flow "ET\n"))
    (niori-pdf-flow--advance flow (* 1.38 size))))

;;;; Footnotes

(defconst niori-pdf-flow-footnote-rule-width 0.4
  "Width fraction (of the text area) of the footnote separator rule,
like LaTeX's default \\footnoterule (0.4\\columnwidth).")

(defvar niori-pdf-flow-footnote-rule-color '(0.12 0.12 0.12)
  "Color of the footnote separator rule.")

(defun niori-pdf-flow--footnotes-height (notes)
  "Height of the drawn block for NOTES (newest first): the note
lines plus the separator rule zone and the inter-note gaps."
  (if (null notes)
      0
    (+ 7.5                       ; 3.5 above the rule + 0.5 rule + 3.5 below
       (apply #'+ (mapcar (lambda (e) (plist-get e :height)) notes))
       (* 2.5 (- (length notes) 1)))))

(cl-defun niori-pdf-flow-footnote (text &key marker font (size 8)
                                        (leading 1.24) color)
  "Register TEXT as a footnote for the current page; return MARKER.
The note is set in a block at the bottom of the page, below a short
separator rule, when the page is flushed.  MARKER defaults to the
note's number on the page (\"1\", \"2\", ...); embed it in the
citing paragraph as a superscript run:

  (let ((fn (niori-pdf-flow-footnote \"Studio notes.\")))
    (niori-pdf-flow-paragraph
     (list \"Text cites a source\" (list fn :superscript t) \".\") ...))

FONT, SIZE, LEADING and COLOR style the note text (defaults: the
paragraph font at 8 pt with 1.24 leading, body color).  Pages that
carry footnotes are shortened by the note block's height on the next
layout pass of `niori-pdf-flow-generate', so the notes always fit
between the text and the bottom margin.  Register the note adjacent
to the paragraph that cites it: a note belongs to the page that is
current when `niori-pdf-flow-footnote' runs."
  (let* ((flow niori-pdf--flow)
         (font-name (or font (niori-pdf-flow-paragraph-font flow)))
         (size (float size))
         (lead (* size (or leading 1.24)))
         (marker (format "%s" (or marker (1+ (length
                                              (niori-pdf-flow-footnotes
                                               flow))))))
         (runs (list (list marker :superscript t) (concat " " text)))
         ;; normalize the runs the way `niori-pdf-flow--paragraph-block'
         ;; does (the word boxes read the size from the run style, so
         ;; the note defaults must be injected per run)
         (runs (mapcar (lambda (r)
                         (let ((pl (if (stringp r) (list r)
                                     (copy-sequence r))))
                           (append pl
                                   (unless (plist-get (cdr pl) :font)
                                     (list :font font-name))
                                   (unless (plist-get (cdr pl) :size)
                                     (list :size size)))))
                       runs))
         (runs (if color
                   (mapcar (lambda (r)
                             (if (or (stringp r) (plist-get (cdr r) :color))
                                 (if (stringp r) (list r :color color) r)
                               (append r (list :color color))))
                           runs)
                 runs))
         (width (niori-pdf-flow--width flow))
         (pb (niori-pdf-flow--paragraph-items
              flow runs :font font-name :size size
              :hyphenate (niori-pdf-flow-hyphenate flow)))
         (breaks (niori-pdf-ts-break-paragraph (car pb) width (cdr pb)))
         (height (* lead (max 1 (length breaks)))))
    (push (list :marker marker :runs runs :font font-name :size size
                :leading lead :color color :height height)
          (niori-pdf-flow-footnotes flow))
    (let* ((page (niori-pdf-flow-page-number flow))
           (cell (assq page (niori-pdf-flow-footnote-heights flow)))
           (total (niori-pdf-flow--footnotes-height
                   (niori-pdf-flow-footnotes flow))))
      (if cell
          (setcdr cell total)
        (push (cons page total)
              (niori-pdf-flow-footnote-heights flow))))
    marker))

(defun niori-pdf-flow--draw-footnotes (flow)
  "Draw the footnotes registered on the page being flushed in FLOW.
The block is bottom-anchored at the text area's bottom edge; while
the layout has not reserved its height yet (first pass, or notes
that moved between passes) the block starts below the last content
line instead, so it never overlaps the text."
  (let* ((notes (reverse (niori-pdf-flow-footnotes flow)))
         (band (- (niori-pdf-flow-page-h flow)
                  (niori-pdf-flow-margin-bottom flow))))
    (when notes
      (let* ((total (niori-pdf-flow--footnotes-height notes))
             (top (max (- band total) (niori-pdf-flow-y flow)))
             (width (niori-pdf-flow--width flow))
             (rule-color (or niori-pdf-flow-footnote-rule-color
                             '(0.12 0.12 0.12))))
        (niori-pdf-flow--with-graphics flow
          ;; The page is being flushed: its segments were emitted
          ;; already and the next page's segment buffer must not
          ;; receive anything.  Route the footnote operators straight
          ;; to the flushed page, bypassing the segment buffer (the
          ;; macro's emit rerouting is active around this call).
          (cl-letf (((symbol-function 'niori-pdf-flow--emit)
                     (lambda (_flow str)
                       (niori-pdf-page-emit niori-pdf--page str)))
                    ((symbol-function 'niori-pdf-flow--emitf)
                     (lambda (_flow fmt &rest args)
                       (niori-pdf-page-emit niori-pdf--page
                                            (apply #'format fmt args)))))
            ;; separator rule (LaTeX's \footnoterule)
            (niori-pdf-flow--emitf flow "0.5 w\n%s %s %s RG\n"
                                   (niori-pdf-num (nth 0 rule-color))
                                   (niori-pdf-num (nth 1 rule-color))
                                   (niori-pdf-num (nth 2 rule-color)))
            (niori-pdf-flow--emitf
             flow "%s %s m\n%s %s l\nS\n"
             (niori-pdf-num (niori-pdf-flow-x0 flow))
             (niori-pdf-num (niori-pdf-flow--pdf-y flow (+ top 3.5)))
             (niori-pdf-num (+ (niori-pdf-flow-x0 flow)
                               (* niori-pdf-flow-footnote-rule-width
                                  width)))
             (niori-pdf-num (niori-pdf-flow--pdf-y flow (+ top 3.5))))
            ;; the notes themselves: marker superscript, then the
            ;; text, line-broken with the same Knuth-Plass machinery
            ;; as the body paragraphs
            (let ((y (+ top 7.5)))
              (dolist (note notes)
                (let* ((runs (plist-get note :runs))
                       (size (plist-get note :size))
                       (lead (plist-get note :leading))
                       (pb (niori-pdf-flow--paragraph-items
                            flow runs
                            :font (plist-get note :font)
                            :size size
                            :hyphenate (niori-pdf-flow-hyphenate flow)))
                       (breaks (niori-pdf-ts-break-paragraph
                                (car pb) width (cdr pb))))
                  (dolist (brk breaks)
                    (let* ((tl (niori-pdf-ts-line (car pb) brk
                                                  :justify nil))
                           (line (car tl)) (widths (cdr tl)))
                      (niori-pdf-flow--render-line
                       flow (niori-pdf-flow-x0 flow)
                       (+ y (* 0.82 size))
                       line widths brk
                       :avail width :align 'left)
                      (setq y (+ y lead))))
                  (setq y (+ y 2.5)))))))))))

;;;; Page furniture helpers

(defun niori-pdf-flow-page-footer (flow page-number)
  "Default page footer: centered \"- N -\" page number."
  (let* ((text (format "- %d -" page-number))
         (font (niori-pdf-get-font (niori-pdf-flow-doc flow) "Helvetica"))
         (w (niori-pdf-text-width text font 9))
         (x (/ (- (niori-pdf-flow-page-w flow) w) 2.0))
         (y 28))
    (niori-pdf-flow--emitf
     flow "BT\n/%s 9 Tf\n1 0 0 1 %s %s Tm\n(%s) Tj\nET\n"
     (niori-pdf-font-resource font)
     (niori-pdf-num x) (niori-pdf-num y)
     text)))

(defun niori-pdf-flow-page-header (flow page-number)
  "Default page header: document title left, page number right."
  (let* ((title (or (plist-get
                     (niori-pdf-document-info (niori-pdf-flow-doc flow))
                     :title)
                    ""))
         (font (niori-pdf-get-font (niori-pdf-flow-doc flow) "Helvetica"))
         (size 8)
         (y (- (niori-pdf-flow-page-h flow) 30)))
    (niori-pdf-flow--emitf
     flow "BT\n/%s %s Tf\n1 0 0 1 %s %s Tm\n(%s) Tj\nET\n"
     (niori-pdf-font-resource font) (niori-pdf-num size)
     (niori-pdf-num (niori-pdf-flow-x0 flow)) (niori-pdf-num y)
     title)
    (niori-pdf-flow--emitf
     flow "BT\n/%s %s Tf\n1 0 0 1 %s %s Tm\n(%d) Tj\nET\n"
     (niori-pdf-font-resource font) (niori-pdf-num size)
     (niori-pdf-num (- (niori-pdf-flow-x1 flow)
                       (niori-pdf-text-width
                        (number-to-string page-number) font size)))
     (niori-pdf-num y)
     page-number)))


(defun niori-pdf-flow-toc ()
  "Insert the table of contents block.
First pass: just records that a TOC is needed.  Later passes render
the entries collected by previous passes with page numbers.  Every
entry row carries a link annotation pointing at its heading (the
/Dest is resolved when the document is saved, because the target
page may be written after the TOC)."
  (setf (niori-pdf-flow-toc-needed niori-pdf--flow) t)
  (when niori-pdf-flow--pass-entries
    (niori-pdf-flow-heading "Contents" :level 1 :toc nil :bookmark nil)
    (dolist (e niori-pdf-flow--pass-entries)
      (let* ((level (plist-get e :level))
             (title (plist-get e :title))
             (page (plist-get e :page))
             (indent (* (1- level) 14))
             (font-name (if (= level 1) "Helvetica-Bold" "Helvetica"))
             (size (if (= level 1) 10.5 9.5))
             (leading (* size 1.55))
             (page-str (number-to-string page))
             (flow niori-pdf--flow)
             (font (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                       font-name))
             (dot-font (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                           "Helvetica"))
             (x0 (niori-pdf-flow-x0 flow))
             (x1 (niori-pdf-flow-x1 flow)))
        (niori-pdf-flow--ensure-space flow leading)
        (let* ((y-entry (niori-pdf-flow-y flow))
               (y-base (+ y-entry (* 0.82 size)))
               ;; page number flush right, title starts left
               (tw-page (niori-pdf-text-width page-str font size))
               (x-page (- x1 tw-page))
               (title-end (+ x0 indent
                             (niori-pdf-text-width title font size)))
               (dots-room (- x-page title-end 8))
               (dot-w (niori-pdf-text-width "." dot-font size))
               (n-dots (max 0 (floor (/ dots-room dot-w)))))
          (niori-pdf-flow--with-graphics flow
            ;; title
            (niori-pdf-flow--emitf flow "BT\n")
            (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                                   (niori-pdf-num (+ x0 indent))
                                   (niori-pdf-num
                                    (niori-pdf-flow--pdf-y flow y-base)))
            (niori-pdf-flow--emitf flow "0.12 0.12 0.12 rg\n")
            (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                                   (niori-pdf-font-resource font)
                                   (niori-pdf-num size))
            (let ((encoded (niori-pdf-font-encode title font)))
              (niori-pdf-flow--emit
               flow (niori-pdf--encoded-tj-operator encoded font)))
            ;; dotted leader
            (when (> n-dots 0)
              (niori-pdf-flow--emitf flow "0.6 0.6 0.6 rg\n")
              (niori-pdf-flow--emitf
               flow "1 0 0 1 %s %s Tm\n"
               (niori-pdf-num (+ title-end 4))
               (niori-pdf-num
                (niori-pdf-flow--pdf-y flow y-base)))
              (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                                     (niori-pdf-font-resource dot-font)
                                     (niori-pdf-num size))
              (let ((encoded (niori-pdf-font-encode
                              (make-string n-dots ?.) dot-font)))
                (niori-pdf-flow--emit
                 flow (niori-pdf--encoded-tj-operator encoded dot-font))))
            ;; page number, flush right
            (niori-pdf-flow--emitf flow "0.12 0.12 0.12 rg\n")
            (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                                   (niori-pdf-num x-page)
                                   (niori-pdf-num
                                    (niori-pdf-flow--pdf-y flow y-base)))
            (niori-pdf-flow--emitf flow "/%s %s Tf\n"
                                   (niori-pdf-font-resource font)
                                   (niori-pdf-num size))
            (let ((encoded (niori-pdf-font-encode page-str font)))
              (niori-pdf-flow--emit
               flow (niori-pdf--encoded-tj-operator encoded font)))
            (niori-pdf-flow--emitf flow "ET\n")
            ;; the whole row is clickable: a deferred link annotation
            ;; whose /Dest is resolved at save time (the target page may
            ;; not exist yet); TOP puts the heading at the viewport top
            ;; (inside --with-graphics: it binds niori-pdf--doc/--page)
            (niori-pdf-add-deferred-link
             (+ x0 indent -2)
             (niori-pdf-flow--pdf-y flow (+ y-entry leading))
             (+ (- x1 (+ x0 indent)) 4) leading page
             (- (niori-pdf-flow-page-h flow)
                (plist-get e :y))))
          (niori-pdf-flow--advance flow leading))))))

(provide 'niori-pdf-flow)
;;; niori-pdf-flow.el ends here
