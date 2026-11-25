;;; niori-pdf-acroforms.el --- AcroForm support for niori-pdf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev
;;
;; Original implementation based on PDF 32000-1:2008, section 12.7
;; (Interactive Forms).  This file is not derived from CL-PDF.
;; See LICENSE for the licenses applicable to this package and its
;; third-party components.

;;; Commentary:

;; Interactive AcroForm fields (PDF 32000-1 chapter 12.7):
;;
;;   (niori-pdf-with-document (:title "form")
;;     (niori-pdf-with-page ()
;;       (niori-pdf-acroforms-enable doc)
;;       (niori-pdf-acroforms-text-field doc "name" 72 720 220 22
;;           :value "Ada Lovelace" :tooltip "Your name")
;;       (niori-pdf-acroforms-checkbox doc "ok" 72 690 16 16 :checked t)
;;       (niori-pdf-acroforms-choice doc "color" 72 660 140 20
;;           '("Red" "Green" "Blue") :value "Green")
;;       (niori-pdf-acroforms-radio-group doc "size"
;;           '((72 630 16 "S") (100 630 16 "M") (128 630 16 "L"))
;;           :value "M")
;;       (niori-pdf-acroforms-push-button doc "go" 72 590 80 24
;;           :caption "Submit" :uri "https://example.com/submit"))
;;     (niori-pdf-save doc "form.pdf"))
;;
;; Coordinates are PDF page coordinates (origin bottom-left), like
;; `niori-pdf-add-link'.  Every terminal field gets a generated
;; appearance stream (/AP /N), so the form renders correctly in
;; viewers that ignore NeedAppearances.  Field values are kept in
;; sync by `niori-pdf-acroforms-set-value'.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-fonts)

;;;; Field flag constants (PDF 32000-1 table 226)

(defconst niori-pdf-ff-readonly 1)
(defconst niori-pdf-ff-required 2)
(defconst niori-pdf-ff-no-export 4)
(defconst niori-pdf-ff-multiline 4096)
(defconst niori-pdf-ff-password 8192)
(defconst niori-pdf-ff-no-toggle-to-off 16384)
(defconst niori-pdf-ff-radio 32768)
(defconst niori-pdf-ff-pushbutton 65536)
(defconst niori-pdf-ff-combo 131072)
(defconst niori-pdf-ff-editable 262144)

(defvar niori-pdf-acroforms--store nil
  "Alist of (DOC . STATE); STATE = plist :fields :fonts :need-ap.")

(declare-function niori-pdf-register-catalog-extension "niori-pdf-core" (doc fn))
(defvar niori-pdf--page)
(declare-function niori-pdf--utf16-string "niori-pdf-core" (s))
(declare-function niori-pdf--escape-literal-string "niori-pdf-core" (bytes))

(defun niori-pdf-acroforms--state (doc)
  "AcroForm accumulation state for DOC (created on demand)."
  (let ((kv (assq doc niori-pdf-acroforms--store)))
    (unless kv
      (setq kv (cons doc (list :fields nil :fonts nil :need-ap nil)))
      (push kv niori-pdf-acroforms--store))
    kv))

(defun niori-pdf-acroforms-fields (doc)
  "List of field state plists accumulated for DOC."
  (plist-get (cdr (assq doc niori-pdf-acroforms--store)) :fields))

(defun niori-pdf-acroforms-clear (doc)
  "Forget all AcroForm state of DOC."
  (setq niori-pdf-acroforms--store
        (assq-delete-all doc niori-pdf-acroforms--store))
  nil)

;;;; Appearance stream helpers

(defun niori-pdf-acroforms--xobject (doc w h content resources)
  "Create a Form XObject stream of size W x H with CONTENT.
RESOURCES is a resources dict or nil.  Returns the stream's ref."
  (let* ((cc (niori-pdf-compress-stream-data doc content))
         (sdict (niori-pdf-dict)))
    (niori-pdf-dict-set sdict 'Type 'XObject)
    (niori-pdf-dict-set sdict 'Subtype 'Form)
    (niori-pdf-dict-set sdict 'FormType 1)
    (niori-pdf-dict-set sdict 'BBox (niori-pdf-array 0 0 w h))
    (when resources
      (niori-pdf-dict-set sdict 'Resources resources))
    (niori-pdf-dict-set sdict 'Length (length (car cc)))
    (when (cdr cc) (niori-pdf-dict-set sdict 'Filter (cdr cc)))
    (niori-pdf-add-object doc (niori-pdf-stream :dict sdict :data (car cc)))))

(defun niori-pdf-acroforms--mk-colors (bc bg)
  "Build the /MK color entries for border BC and background BG."
  (let ((mk (niori-pdf-dict)))
    (when bc
      (niori-pdf-dict-set mk 'BC (apply #'niori-pdf-array bc)))
    (when bg
      (niori-pdf-dict-set mk 'BG (apply #'niori-pdf-array bg)))
    mk))

(defun niori-pdf-acroforms--num (v)
  "Format number V compactly for content streams."
  (niori-pdf-num v))

;;;; Text value helpers

(defun niori-pdf-acroforms--literal (value font)
  "Encode VALUE in FONT's encoding as a literal PDF string body."
  (niori-pdf--escape-literal-string
   (encode-coding-string (niori-pdf-font-encode value font) 'binary)))

(defun niori-pdf-acroforms--text-width (value font size)
  "Width in points of VALUE set in FONT at SIZE."
  (let ((enc (niori-pdf-font-encode value font)))
    (* (/ (niori-pdf-font-encoded-width-kerned enc font) 1000.0)
       size)))

(defun niori-pdf-acroforms--wrap (value font size w)
  "Naive word wrap of VALUE for width W; returns a list of lines."
  (let ((lines nil) (cur ""))
    (dolist (word (split-string value "[ \t\n]+" t))
      (let ((cand (if (string-empty-p cur) word (concat cur " " word))))
        (if (or (string-empty-p cur)
                (<= (niori-pdf-acroforms--text-width cand font size) w))
            (setq cur cand)
          (push cur lines)
          (setq cur word))))
    (setq lines (nreverse (if (string-empty-p cur) lines (cons cur lines))))
    (or lines (list ""))))

;;;; Fonts for /DR and /DA

(defun niori-pdf-acroforms--da-font (doc font-name)
  "Return (RESOURCE-NAME . FONT) for DA font FONT-NAME in DOC."
  (let* ((state (niori-pdf-acroforms--state doc))
         (pl (cdr state)))
    (or (assoc font-name (plist-get pl :fonts) #'equal)
        (let* ((font (niori-pdf-get-font doc font-name))
               (kv (cons font-name font)))
          (setf (cdr state)
                (plist-put pl :fonts
                           (append (plist-get pl :fonts) (list kv))))
          kv))))

(defun niori-pdf-acroforms--da-string (font size &optional color)
  "Default appearance string: resource name of FONT, SIZE, COLOR."
  (let ((col (or color '(0 0 0))))
    (format "/%s %.2f Tf %.3g %.3g %.3g rg"
            (niori-pdf-font-resource font) size
            (nth 0 col) (nth 1 col) (nth 2 col))))

;;;; Field creation (during page construction)

(defun niori-pdf-acroforms--require-page ()
  "Current page object (the widget's target page)."
  (or niori-pdf--page
      (error "niori-pdf-acroforms: no current page (use within niori-pdf-with-page)")))

(defun niori-pdf-acroforms--push-widget (doc dict)
  "Add widget DICT to DOC and its ref to the current page /Annots.
Returns the widget's object ref."
  (let ((ref (niori-pdf-add-object doc dict)))
    (push ref (niori-pdf-page-annots (niori-pdf-acroforms--require-page)))
    ref))

(defun niori-pdf-acroforms--update-entry (doc entry &rest kv)
  "Replace ENTRY in DOC's field list with a plist carrying KV."
  (let ((new (apply #'plist-put (copy-sequence entry) kv)))
    (setf (cdr (niori-pdf-acroforms--state doc))
          (plist-put (cdr (niori-pdf-acroforms--state doc)) :fields
                     (mapcar (lambda (e) (if (eq e entry) new e))
                             (niori-pdf-acroforms-fields doc))))
    new))

(defun niori-pdf-acroforms--register (doc entry)
  "Add ENTRY (a plist) to DOC's field list and enable the save hook.
For terminal widgets (everything except radio groups, whose kid
widgets are pushed individually) the field's own ref is added to
the current page /Annots so the widget renders on the page."
  (let ((state (niori-pdf-acroforms--state doc)))
    (setf (cdr state)
          (plist-put (cdr state) :fields
                     (append (plist-get (cdr state) :fields)
                             (list entry))))
    (unless (eq (plist-get entry :kind) 'radio)
      (push (plist-get entry :ref)
            (niori-pdf-page-annots (niori-pdf-acroforms--require-page)))))
  (niori-pdf-acroforms--ensure-hook doc)
  entry)

(cl-defun niori-pdf-acroforms-text-field
    (doc name x y w h &key value font font-size tooltip multiline password
         read-only align max-len (bg '(1 1 1)) (bc '(0.5 0.5 0.5)))
  "Add a text field NAME at X, Y (lower left), W x H points.
VALUE is the initial content; FONT (default \"Helvetica\") and
FONT-SIZE (default 10.5) style the text; TOOLTIP is the /TU entry.
MULTILINE and PASSWORD set the corresponding flags.  ALIGN is 0
(left), 1 (center) or 2 (right).  MAX-LEN limits the content.  BG
and BC are the background / border color lists."
  (let* ((page (niori-pdf-acroforms--require-page))
         (fname (or font "Helvetica"))
         (size (or font-size 10.5))
         (ff (+ (if multiline niori-pdf-ff-multiline 0)
                (if password niori-pdf-ff-password 0)
                (if read-only niori-pdf-ff-readonly 0)))
         (dakv (niori-pdf-acroforms--da-font doc fname))
         (da-font (cdr dakv))
         (da (niori-pdf-acroforms--da-string da-font size))
         (dict (niori-pdf-dict)))
    (niori-pdf-dict-set dict 'Type 'Annot)
    (niori-pdf-dict-set dict 'Subtype 'Widget)
    (niori-pdf-dict-set dict 'FT 'Tx)
    (niori-pdf-dict-set dict 'T name)
    (when tooltip
      (niori-pdf-dict-set dict 'TU (niori-pdf--utf16-string tooltip)))
    (when (> ff 0) (niori-pdf-dict-set dict 'Ff ff))
    (when max-len (niori-pdf-dict-set dict 'MaxLen max-len))
    (niori-pdf-dict-set dict 'F 4)
    (niori-pdf-dict-set dict 'Rect (niori-pdf-array x y (+ x w) (+ y h)))
    (niori-pdf-dict-set dict 'DA (niori-pdf-raw (format "(%s)" da)))
    (niori-pdf-dict-set dict 'Q (or align 0))
    (niori-pdf-dict-set dict 'MK
                        (niori-pdf-acroforms--mk-colors bc bg))
    (let ((shown (if password
                     (make-string (length value) ?*)
                   value))
          (ref (niori-pdf-add-object doc dict)))
      (niori-pdf-dict-set dict 'V (niori-pdf-raw
                                   (niori-pdf-acroforms--literal shown da-font)))
      (niori-pdf-dict-set
       dict 'AP
       (niori-pdf-dict-from-alist
        (list (cons 'N
                    (niori-pdf-acroforms--text-ap
                     doc shown da-font size w h bg bc align multiline)))))
      (niori-pdf-acroforms--register
       doc (list :kind 'text :name name :dict dict :ref ref :page page
                 :value value :font fname :size size :w w :h h
                 :multiline multiline :password password
                 :align align :bg bg :bc bc))
      ref)))

(defun niori-pdf-acroforms--text-ap (doc value font size w h bg bc align multiline)
  "Appearance stream ref for a text field VALUE in W x H."
  (let* ((pad 2.0)
         (lines (if multiline
                    (niori-pdf-acroforms--wrap value font size (- w (* 2 pad)))
                  (list (or value ""))))
         (lh (* size 1.25))
         (parts
          (list
           ;; background + border
           (format "%.3g %.3g %.3g rg 0 0 %.3g %.3g re f "
                   (nth 0 bg) (nth 1 bg) (nth 2 bg) w h)
           (format "%.3g w %.3g %.3g %.3g RG 0.5 0.5 %.3g %.3g re S "
                   0.75 (nth 0 bc) (nth 1 bc) (nth 2 bc) (- w 1) (- h 1))))
         (res (niori-pdf-dict))
         (fd (niori-pdf-dict))
         (y0 (if multiline
                 (- h pad size 1.0)
               (/ (- h (* 1.15 size)) 2.0))))
    (niori-pdf-dict-set fd (intern (niori-pdf-font-resource font))
                        (niori-pdf-font-ref font))
    (niori-pdf-dict-set res 'Font fd)
    (let ((buf (concat (apply #'concat parts) "q BT "))
          (yy y0))
      (dolist (ln lines)
        (let* ((tw (niori-pdf-acroforms--text-width ln font size))
               (tx (pcase (or align 0)
                     (1 (/ (- w tw) 2.0))
                     (2 (- w pad tw))
                     (_ pad))))
          (setq buf
                (concat buf
                        (format "/%s %.2f Tf 0 0 0 rg 1 0 0 1 %.3g %.3g Tm %s Tj "
                                (niori-pdf-font-resource font) size
                                tx yy
                                (niori-pdf-acroforms--literal ln font)))
                yy (- yy lh))
          (when (< yy pad)
            (setq yy pad))))
      (setq buf (concat buf "ET Q"))
      (niori-pdf-acroforms--xobject doc w h buf res))))

(cl-defun niori-pdf-acroforms-checkbox
    (doc name x y w h &key checked read-only tooltip
         (bg '(1 1 1)) (bc '(0.4 0.4 0.4)) (mark '(0 0 0)))
  "Add a check box NAME at X, Y, W x H points (CHECKED initial state)."
  (let* ((page (niori-pdf-acroforms--require-page))
         (ff (if read-only niori-pdf-ff-readonly 0))
         (dict (niori-pdf-dict))
         (on (niori-pdf-acroforms--check-ap doc w h bg bc mark t))
         (off (niori-pdf-acroforms--check-ap doc w h bg bc mark nil))
         (state (if checked 'On 'Off))
         (ref (niori-pdf-add-object doc dict)))
    (niori-pdf-dict-set dict 'Type 'Annot)
    (niori-pdf-dict-set dict 'Subtype 'Widget)
    (niori-pdf-dict-set dict 'FT 'Btn)
    (niori-pdf-dict-set dict 'T name)
    (when tooltip
      (niori-pdf-dict-set dict 'TU (niori-pdf--utf16-string tooltip)))
    (when (> ff 0) (niori-pdf-dict-set dict 'Ff ff))
    (niori-pdf-dict-set dict 'F 4)
    (niori-pdf-dict-set dict 'Rect (niori-pdf-array x y (+ x w) (+ y h)))
    (niori-pdf-dict-set dict 'V state)
    (niori-pdf-dict-set dict 'AS state)
    (niori-pdf-dict-set dict 'MK (niori-pdf-acroforms--mk-colors bc bg))
    (niori-pdf-dict-set
     dict 'AP
     (niori-pdf-dict-from-alist
      (list (cons 'N
                  (niori-pdf-dict-from-alist
                   (list (cons 'On on) (cons 'Off off)))))))
    (niori-pdf-acroforms--register
     doc (list :kind 'checkbox :name name :dict dict :ref ref :page page
               :value checked :bg bg :bc bc :mark mark))
    ref))

(defun niori-pdf-acroforms--check-ap (doc w h bg bc mark on-p)
  "Appearance ref for a W x H check box; ON-P decides the mark."
  (let* ((bg-str (format "%.3g %.3g %.3g rg 0 0 %.3g %.3g re f "
                         (nth 0 bg) (nth 1 bg) (nth 2 bg) w h))
         (bd-str (format "0.75 w %.3g %.3g %.3g RG 0.5 0.5 %.3g %.3g re S "
                         (nth 0 bc) (nth 1 bc) (nth 2 bc) (- w 1) (- h 1)))
         (body (if (not on-p)
                   (concat bg-str bd-str)
                 (concat bg-str bd-str
                         (format "q %.3g w %.3g %.3g %.3g RG %.3g %.3g m %.3g %.3g l %.3g %.3g l S Q "
                                 (* 0.14 w)
                                 (nth 0 mark) (nth 1 mark) (nth 2 mark)
                                 (* 0.20 w) (* 0.50 h)
                                 (* 0.42 w) (* 0.22 h)
                                 (* 0.80 w) (* 0.78 h))))))
    (niori-pdf-acroforms--xobject doc w h body nil)))

(cl-defun niori-pdf-acroforms-radio-group
    (doc name buttons &key value read-only tooltip
         (bg '(1 1 1)) (bc '(0.4 0.4 0.4)) (mark '(0 0 0)))
  "Add a radio group NAME; BUTTONS defines the kid widgets.
Each element is (X Y SIZE EXPORT-STRING) placed on the current
page.  VALUE is the initially selected export string (or nil).
TOOLTIP applies to the group field."
  (let* ((page (niori-pdf-acroforms--require-page))
         (ff (+ niori-pdf-ff-radio niori-pdf-ff-no-toggle-to-off
                (if read-only niori-pdf-ff-readonly 0)))
         (field (niori-pdf-dict))
         (field-ref (niori-pdf-add-object doc field))
         (kids nil))
    (niori-pdf-dict-set field 'FT 'Btn)
    (niori-pdf-dict-set field 'T name)
    (when tooltip
      (niori-pdf-dict-set field 'TU (niori-pdf--utf16-string tooltip)))
    (niori-pdf-dict-set field 'Ff ff)
    (niori-pdf-dict-set field 'F 4)
    (niori-pdf-dict-set field 'V (if value (intern (concat "ON-" value)) 'Off))
    (dolist (b buttons)
      (let* ((bx (nth 0 b)) (by (nth 1 b)) (bs (nth 2 b))
             (ex (nth 3 b))
             (kid (niori-pdf-dict))
             (on (niori-pdf-acroforms--radio-ap doc bs bg bc mark t))
             (off (niori-pdf-acroforms--radio-ap doc bs bg bc mark nil))
             (sel (and value (string= ex value))))
        (niori-pdf-dict-set kid 'Type 'Annot)
        (niori-pdf-dict-set kid 'Subtype 'Widget)
        (niori-pdf-dict-set kid 'Parent field-ref)
        (niori-pdf-dict-set kid 'F 4)
        (niori-pdf-dict-set kid 'Rect
                            (niori-pdf-array bx by (+ bx bs) (+ by bs)))
        (niori-pdf-dict-set kid 'AS (if sel
                                        (intern (concat "ON-" ex))
                                      'Off))
        (niori-pdf-dict-set kid 'MK (niori-pdf-acroforms--mk-colors bc bg))
        (niori-pdf-dict-set
         kid 'AP
         (niori-pdf-dict-from-alist
          (list (cons 'N
                      (niori-pdf-dict-from-alist
                       (list (cons (intern (concat "ON-" ex)) on)
                             (cons 'Off off)))))))
        (let ((kid-ref (niori-pdf-acroforms--push-widget doc kid)))
          (push (list :dict kid :ref kid-ref :page page :export ex
                      :selected sel :size bs :bg bg :bc bc :mark mark)
                kids))))
    (niori-pdf-dict-set
     field 'Kids
     (apply #'niori-pdf-array
            (nreverse (mapcar (lambda (k) (plist-get k :ref)) kids))))
    (niori-pdf-acroforms--register
     doc (list :kind 'radio :name name :dict field :ref field-ref
               :page page :kids (nreverse kids) :value value
               :tooltip tooltip :bg bg :bc bc :mark mark))
    field-ref))

(cl-defun niori-pdf-acroforms--radio-ap (doc size bg bc mark on-p)
  "Appearance ref for a SIZE x SIZE radio button; ON-P selects the dot."
  (let* ((bg-str (format "%.3g %.3g %.3g rg 0 0 %.3g %.3g re f "
                         (nth 0 bg) (nth 1 bg) (nth 2 bg) size size))
         (bd-str (format "0.75 w %.3g %.3g %.3g RG 0.5 0.5 %.3g %.3g re S "
                         (nth 0 bc) (nth 1 bc) (nth 2 bc) (- size 1) (- size 1)))
         (body (if (not on-p)
                   (concat bg-str bd-str)
                 (concat bg-str bd-str
                         (format "q %.3g %.3g %.3g rg %.3g %.3g %.3g %.3g re f Q "
                                 (nth 0 mark) (nth 1 mark) (nth 2 mark)
                                 (* 0.28 size) (* 0.28 size)
                                 (* 0.44 size) (* 0.44 size))))))
    (niori-pdf-acroforms--xobject doc size size body nil)))

(cl-defun niori-pdf-acroforms-choice
    (doc name x y w h options &key value editable tooltip
         read-only (bg '(1 1 1)) (bc '(0.4 0.4 0.4)) (mark '(0 0 0)))
  "Add a dropdown (combo) choice field NAME with OPTIONS (strings).
VALUE selects the initial option; EDITABLE adds an edit box."
  (let* ((page (niori-pdf-acroforms--require-page))
         (fname "Helvetica")
         (size 10.5)
         (ff (+ niori-pdf-ff-combo
                (if editable niori-pdf-ff-editable 0)
                (if read-only niori-pdf-ff-readonly 0)))
         (dakv (niori-pdf-acroforms--da-font doc fname))
         (da-font (cdr dakv))
         (shown (or value (car options)))
         (dict (niori-pdf-dict))
         (ref (niori-pdf-add-object doc dict)))
    (niori-pdf-dict-set dict 'Type 'Annot)
    (niori-pdf-dict-set dict 'Subtype 'Widget)
    (niori-pdf-dict-set dict 'FT 'Ch)
    (niori-pdf-dict-set dict 'T name)
    (when tooltip
      (niori-pdf-dict-set dict 'TU (niori-pdf--utf16-string tooltip)))
    (niori-pdf-dict-set dict 'Ff ff)
    (niori-pdf-dict-set dict 'F 4)
    (niori-pdf-dict-set dict 'Rect (niori-pdf-array x y (+ x w) (+ y h)))
    (niori-pdf-dict-set dict 'DA (niori-pdf-raw
                                  (format "(%s)"
                                          (niori-pdf-acroforms--da-string
                                           da-font size))))
    (niori-pdf-dict-set
     dict 'Opt
     (apply #'niori-pdf-array
            (mapcar (lambda (o)
                      (niori-pdf-raw
                       (niori-pdf-acroforms--literal o da-font)))
                    options)))
    (niori-pdf-dict-set dict 'V (niori-pdf-raw
                                 (niori-pdf-acroforms--literal
                                  (or shown "") da-font)))
    (niori-pdf-dict-set dict 'Q 0)
    (niori-pdf-dict-set dict 'MK (niori-pdf-acroforms--mk-colors bc bg))
    (niori-pdf-dict-set
     dict 'AP
     (niori-pdf-dict-from-alist
      (list (cons 'N (niori-pdf-acroforms--choice-ap
                      doc shown da-font size w h bg bc mark)))))
    (niori-pdf-acroforms--register
     doc (list :kind 'choice :name name :dict dict :ref ref :page page
               :options options :value shown :font fname :size size
               :w w :h h :bg bg :bc bc :mark mark))
    ref))

(defun niori-pdf-acroforms--choice-ap (doc value font size w h bg bc mark)
  "Appearance ref for a choice field showing VALUE."
  (let* ((pad 3.0)
         (res (niori-pdf-dict))
         (fd (niori-pdf-dict))
         (y0 (/ (- h (* 1.15 size)) 2.0))
         (tx (+ pad 2.0))
         (arrow-x (- w (* 1.6 h)))
         (body (concat
                (format "%.3g %.3g %.3g rg 0 0 %.3g %.3g re f "
                        (nth 0 bg) (nth 1 bg) (nth 2 bg) w h)
                (format "0.75 w %.3g %.3g %.3g RG 0.5 0.5 %.3g %.3g re S "
                        (nth 0 bc) (nth 1 bc) (nth 2 bc) (- w 1) (- h 1))
                (format "q BT /%s %.2f Tf 0 0 0 rg 1 0 0 1 %.3g %.3g Tm %s Tj ET Q "
                        (niori-pdf-font-resource font) size tx y0
                        (niori-pdf-acroforms--literal (or value "") font))
                (format "q %.3g %.3g %.3g rg %.3g %.3f m %.3g %.3f l %.3g %.3f l h f Q "
                        (nth 0 mark) (nth 1 mark) (nth 2 mark)
                        (+ arrow-x (* 0.30 h)) (* 0.60 h)
                        (+ arrow-x (* 0.60 h)) (* 0.60 h)
                        (+ arrow-x (* 0.45 h)) (* 0.35 h)))))
    (niori-pdf-dict-set fd (intern (niori-pdf-font-resource font))
                        (niori-pdf-font-ref font))
    (niori-pdf-dict-set res 'Font fd)
    (niori-pdf-acroforms--xobject doc w h body res)))

(cl-defun niori-pdf-acroforms-push-button
    (doc name x y w h &key caption tooltip uri font font-size
         read-only (bg '(0.92 0.92 0.95)) (bc '(0.3 0.3 0.4))
         (fg '(0 0 0)))
  "Add a push button NAME; CAPTION is drawn on it.
When URI is given the button triggers a URI action."
  (let* ((page (niori-pdf-acroforms--require-page))
         (fname (or font "Helvetica-Bold"))
         (size (or font-size 11.0))
         (ff (+ niori-pdf-ff-pushbutton
                (if read-only niori-pdf-ff-readonly 0)))
         (dakv (niori-pdf-acroforms--da-font doc fname))
         (da-font (cdr dakv))
         (dict (niori-pdf-dict))
         (ref (niori-pdf-add-object doc dict)))
    (niori-pdf-dict-set dict 'Type 'Annot)
    (niori-pdf-dict-set dict 'Subtype 'Widget)
    (niori-pdf-dict-set dict 'FT 'Btn)
    (niori-pdf-dict-set dict 'T name)
    (when tooltip
      (niori-pdf-dict-set dict 'TU (niori-pdf--utf16-string tooltip)))
    (niori-pdf-dict-set dict 'Ff ff)
    (niori-pdf-dict-set dict 'F 4)
    (niori-pdf-dict-set dict 'Rect (niori-pdf-array x y (+ x w) (+ y h)))
    (let ((mk (niori-pdf-acroforms--mk-colors bc bg)))
      (niori-pdf-dict-set mk 'CA (niori-pdf--utf16-string (or caption name)))
      (niori-pdf-dict-set dict 'MK mk))
    (when uri
      (niori-pdf-dict-set
       dict 'A
       (niori-pdf-dict-from-alist
        (list (cons 'Type 'Action) (cons 'S 'URI) (cons 'URI uri)))))
    (niori-pdf-dict-set
     dict 'AP
     (niori-pdf-dict-from-alist
      (list (cons 'N (niori-pdf-acroforms--button-ap
                      doc (or caption name) da-font size w h bg bc fg)))))
    (niori-pdf-acroforms--register
     doc (list :kind 'button :name name :dict dict :ref ref :page page
               :caption (or caption name) :font fname :size size
               :w w :h h :bg bg :bc bc :fg fg :uri uri))
    ref))

(defun niori-pdf-acroforms--button-ap (doc caption font size w h bg bc fg)
  "Appearance ref for a push button with CAPTION centered."
  (let* ((res (niori-pdf-dict))
         (fd (niori-pdf-dict))
         (tw (niori-pdf-acroforms--text-width caption font size))
         (body (concat
                (format "%.3g %.3g %.3g rg 0 0 %.3g %.3g re f "
                        (nth 0 bg) (nth 1 bg) (nth 2 bg) w h)
                (format "1 w %.3g %.3g %.3g RG 0.5 0.5 %.3g %.3g re S "
                        (nth 0 bc) (nth 1 bc) (nth 2 bc) (- w 1) (- h 1))
                (format "q BT /%s %.2f Tf %.3g %.3g %.3g rg 1 0 0 1 %.3g %.3g Tm %s Tj ET Q "
                        (niori-pdf-font-resource font) size
                        (nth 0 fg) (nth 1 fg) (nth 2 fg)
                        (/ (- w tw) 2.0) (/ (- h (* 1.15 size)) 2.0)
                        (niori-pdf-acroforms--literal caption font)))))
    (niori-pdf-dict-set fd (intern (niori-pdf-font-resource font))
                        (niori-pdf-font-ref font))
    (niori-pdf-dict-set res 'Font fd)
    (niori-pdf-acroforms--xobject doc w h body res)))

;;;; Value updates after creation

(defun niori-pdf-acroforms--set-text-value (doc entry value)
  "Set VALUE of the text field ENTRY in DOC."
  (let* ((dict (plist-get entry :dict))
         (font (niori-pdf-get-font doc (plist-get entry :font)))
         (shown (if (plist-get entry :password)
                    (make-string (length value) ?*)
                  value)))
    (niori-pdf-dict-set
     dict 'V (niori-pdf-raw (niori-pdf-acroforms--literal shown font)))
    (niori-pdf-dict-set
     dict 'AP
     (niori-pdf-dict-from-alist
      (list (cons 'N
                  (niori-pdf-acroforms--text-ap
                   doc shown font (plist-get entry :size)
                   (plist-get entry :w) (plist-get entry :h)
                   (plist-get entry :bg) (plist-get entry :bc)
                   (plist-get entry :align)
                   (plist-get entry :multiline))))))
    (niori-pdf-acroforms--update-entry doc entry :value value)))

(defun niori-pdf-acroforms--set-checkbox-value (doc entry value)
  "Set VALUE (boolean) of the check box ENTRY in DOC."
  (let ((dict (plist-get entry :dict))
        (state (if value 'On 'Off)))
    (niori-pdf-dict-set dict 'V state)
    (niori-pdf-dict-set dict 'AS state)
    (niori-pdf-acroforms--update-entry doc entry :value value)))

(defun niori-pdf-acroforms--set-radio-value (doc entry value)
  "Set VALUE (export string) of the radio group ENTRY in DOC."
  (niori-pdf-dict-set
   (plist-get entry :dict) 'V
   (if value (intern (concat "ON-" value)) 'Off))
  (dolist (kid (plist-get entry :kids))
    (niori-pdf-dict-set
     (plist-get kid :dict) 'AS
     (if (and value (string= (plist-get kid :export) value))
         (intern (concat "ON-" value))
       'Off)))
  (niori-pdf-acroforms--update-entry doc entry :value value))

(defun niori-pdf-acroforms--set-choice-value (doc entry value)
  "Set VALUE of the choice field ENTRY in DOC."
  (let ((dict (plist-get entry :dict))
        (font (niori-pdf-get-font doc (plist-get entry :font))))
    (niori-pdf-dict-set
     dict 'V (niori-pdf-raw (niori-pdf-acroforms--literal value font)))
    (niori-pdf-dict-set
     dict 'AP
     (niori-pdf-dict-from-alist
      (list (cons 'N
                  (niori-pdf-acroforms--choice-ap
                   doc value font (plist-get entry :size)
                   (plist-get entry :w) (plist-get entry :h)
                   (plist-get entry :bg) (plist-get entry :bc)
                   (plist-get entry :mark))))))
    (niori-pdf-acroforms--update-entry doc entry :value value)))

(defun niori-pdf-acroforms-set-value (doc name value)
  "Set the value of field NAME in DOC to VALUE.
Works for text fields (string), check boxes (boolean) and radio
groups (export string).  Regenerates the appearance."
  (let* ((entry (cl-find name (niori-pdf-acroforms-fields doc)
                         :key (lambda (e) (plist-get e :name))
                         :test #'equal))
         (kind (and entry (plist-get entry :kind))))
    (unless entry
      (error "niori-pdf-acroforms: unknown field %S" name))
    (cond
     ((eq kind 'text)
      (niori-pdf-acroforms--set-text-value doc entry value))
     ((eq kind 'checkbox)
      (niori-pdf-acroforms--set-checkbox-value doc entry value))
     ((eq kind 'radio)
      (niori-pdf-acroforms--set-radio-value doc entry value))
     ((eq kind 'choice)
      (niori-pdf-acroforms--set-choice-value doc entry value))))
  nil)

;;;; Save integration

(defun niori-pdf-acroforms--finalize (doc)
  "Save hook for DOC: register the catalog extension."
  (when (niori-pdf-acroforms-fields doc)
    (niori-pdf-register-catalog-extension
     doc #'niori-pdf-acroforms--catalog-ext))
  nil)

(defun niori-pdf-acroforms--catalog-ext (doc catalog-dict)
  "Build the /AcroForm entry in CATALOG-DICT for DOC."
  (let* ((state (cdr (assq doc niori-pdf-acroforms--store)))
         (fields (plist-get state :fields))
         (af (niori-pdf-dict))
         (field-refs nil))
    ;; /P page references (page obj numbers are known by now)
    (dolist (f fields)
      (when (plist-get f :page)
        (niori-pdf-dict-set (plist-get f :dict) 'P
                            (niori-pdf-ref
                             (niori-pdf-page-obj-num (plist-get f :page)))))
      (dolist (kid (plist-get f :kids))
        (when (plist-get kid :page)
          (niori-pdf-dict-set (plist-get kid :dict) 'P
                              (niori-pdf-ref
                               (niori-pdf-page-obj-num
                                (plist-get kid :page))))))
      (push (plist-get f :ref) field-refs))
    (niori-pdf-dict-set af 'Fields
                        (apply #'niori-pdf-array (nreverse field-refs)))
    (let ((fd (niori-pdf-dict)))
      (dolist (kv (plist-get state :fonts))
        (niori-pdf-dict-set
         fd (intern (niori-pdf-font-resource (cdr kv)))
         (niori-pdf-font-ref (cdr kv))))
      (niori-pdf-dict-set af 'DR
                          (niori-pdf-dict-from-alist
                           (list (cons 'Font fd)))))
    (niori-pdf-dict-set af 'DA (niori-pdf-raw
                                (format "(%s)"
                                        (niori-pdf-acroforms--da-default doc))))
    (when (plist-get state :need-ap)
      (niori-pdf-dict-set af 'NeedAppearances t))
    (niori-pdf-dict-set catalog-dict 'AcroForm af)))

(defun niori-pdf-acroforms--da-default (doc)
  "Default /DA string for DOC (first registered DA font)."
  (let* ((state (cdr (assq doc niori-pdf-acroforms--store)))
         (fonts (plist-get state :fonts)))
    (if fonts
        (format "/%s 0 Tf 0 g" (niori-pdf-font-resource (cdr (car fonts))))
      "/Helv 0 Tf 0 g")))

(defun niori-pdf-acroforms--ensure-hook (doc)
  (unless (member #'niori-pdf-acroforms--finalize
                  (niori-pdf-document-save-hooks doc))
    (push #'niori-pdf-acroforms--finalize
          (niori-pdf-document-save-hooks doc))))

(defun niori-pdf-acroforms-enable (doc &optional need-appearances)
  "Enable AcroForms for DOC; fields added afterwards are collected.
When NEED-APPEARANCES is non-nil the catalog gets /NeedAppearances
true (viewers regenerate all appearances; by default niori-pdf
provides its own appearance streams and omits the flag)."
  (let ((state (niori-pdf-acroforms--state doc)))
    (setf (cdr state) (plist-put (cdr state) :need-ap
                                 (and need-appearances t))))
  (niori-pdf-acroforms--ensure-hook doc)
  nil)

(provide 'niori-pdf-acroforms)
;;; niori-pdf-acroforms.el ends here
