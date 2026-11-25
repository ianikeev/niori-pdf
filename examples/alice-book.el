;;; alice-book.el --- Typeset "Alice's Adventures in Wonderland" as a complete PDF book with niori-pdf -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Igor Anikeev
;; See LICENSE and THIRD-PARTY-NOTICES for applicable terms.

;; Commentary:
;;
;; A typographic showcase for niori-pdf, the pure-Emacs-Lisp port of
;; cl-pdf.  Lewis Carroll's "Alice's Adventures in Wonderland"
;; (Project Gutenberg ebook #11, public domain) set as a compact
;; book:
;;
;;   * A5 page with classic book margins
;;   * body text in P052 (URW's Palatino revival), 9.7/13.6,
;;     Knuth-Plass justification with American English TeX
;;     hyphenation
;;   * italic runs (the text's _underscore markup_) in P052 Italic
;;   * chapter openers: letterspaced sans numerals, sans-serif titles
;;     (wrapped to fit), a diamond ornament, raised initials
;;   * verse set centered and unhyphenated, scene separators as
;;     diamond ornaments
;;   * running heads (book title on versos, chapter title on rectos,
;;     suppressed on openers and front matter), centered folios
;;   * front matter: half title, title page, colophon with a QR code,
;;     and a table of contents with dotted leaders, internal GoTo
;;     link annotations and PDF bookmarks
;;   * all four text faces are OpenType/CFF fonts, subsetted and
;;     embedded by niori-pdf's CFF subsetter
;;
;; Run:  emacs --batch -L . -L examples -l examples/alice-book.el

;;; Code:

(require 'niori-pdf-flow)
(require 'niori-pdf-barcodes)
(require 'niori-pdf-qrcode)
(require 'niori-pdf-outlines)

(defconst alice-book-source
  (expand-file-name "alice.txt"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory))))

;; Build the PDF/A-2b archival variant with `--pdfa' (after `--' on
;; the Emacs command line) or NIORI_PDFA=1; the output is then written
;; to alice-in-wonderland-pdfa.pdf.  All four text faces are embedded
;; OpenType/CFF fonts, so the archival build needs no base-14
;; substitutions.
(defvar alice-pdfa
  (or (member "--pdfa" command-line-args)
      (equal (getenv "NIORI_PDFA") "1"))
  "Build the PDF/A-2b archival variant of the book.")

;;; Fonts: four OpenType/CFF faces, all subsetted at the CFF level

(defconst alice-serif
  "/usr/share/fonts/opentype/urw-base35/P052-Roman.otf")
(defconst alice-serif-italic
  "/usr/share/fonts/opentype/urw-base35/P052-Italic.otf")
(defconst alice-sans
  "/usr/share/fonts/opentype/urw-base35/NimbusSans-Regular.otf")
(defconst alice-sans-bold
  "/usr/share/fonts/opentype/urw-base35/NimbusSans-Bold.otf")

;;; Geometry (points): A5 with classic book margins

(defconst alice-page-w 419.53)         ; A5 width
(defconst alice-page-h 595.28)         ; A5 height
(defconst alice-front-pages 4)         ; half title, title, colophon, contents

(defconst alice-body-size 9.7)
(defconst alice-body-leading 13.6)

;;; Two-pass state: TOC page numbers and running heads

(defvar alice--chapters nil)      ; ((NUM TITLE PDF-PAGE Y) ...) this pass
(defvar alice--prev-chapters nil) ; same, from the previous pass
(defvar alice--book nil)          ; parsed chapters
(defvar alice-debug-limit nil)    ; non-nil: render only N chapters

;;; ------------------------------------------------------------------
;;; Parsing the Project Gutenberg text

(defun alice--read-text ()
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents alice-book-source))
    (buffer-string)))

(defun alice--split-blocks (text)
  "Split TEXT into blocks of consecutive non-blank lines."
  (let ((blocks nil) (cur nil))
    (dolist (line (split-string text "\n"))
      (if (string-match-p "\\`[ \t]*\\'" line)
          (when cur
            (push (nreverse cur) blocks)
            (setq cur nil))
        (push line cur)))
    (when cur (push (nreverse cur) blocks))
    (nreverse blocks)))

(defun alice--classify (lines)
  "Classify LINES (one block) into a typed entry."
  (cond
   ;; chapter heading: "CHAPTER I." followed by the title line(s)
   ((and (string-match-p "\\`CHAPTER [IVXLC]+\\.?\\'" (car lines))
         (cdr lines))
    (list 'chapter
          (replace-regexp-in-string
           "\\`CHAPTER \\([IVXLC]+\\)\\.*.*" "\\1" (car lines))
          (string-join (cdr lines) " ")))
   ;; scene separator: asterisks and blanks only
   ((cl-every (lambda (l) (string-match-p "\\`[ \t*]+\\'" l)) lines)
    '(separator))
   ;; the end
   ((equal lines '("THE END")) '(end))
   ;; verse: at least one line indented in the source (Gutenberg poems
   ;; indent the even lines; paragraphs are always flush-left)
   ((cl-some (lambda (l) (string-match-p "\\`[ \t]+" l)) lines)
    (cons 'verse (mapcar #'string-trim lines)))
   (t (list 'paragraph
            (string-join (mapcar #'string-trim lines) " ")))))

(defun alice--parse-book ()
  "Parse alice.txt into typed entries (after the Gutenberg boilerplate)."
  (let* ((text (alice--read-text)))
    (string-match "\\*\\*\\* START OF THE PROJECT GUTENBERG[^\n]*\n" text)
    (setq text (substring text (match-end 0)))
    (when (string-match "\\*\\*\\* END OF THE PROJECT GUTENBERG" text)
      (setq text (substring text 0 (match-beginning 0))))
    (setq text (replace-regexp-in-string
                "^\\[Illustration\\][ \t]*\n?" "" text))
    (mapcar #'alice--classify (alice--split-blocks text))))

(defun alice--group-chapters (entries)
  "Group ENTRIES after their chapter markers into
(chapter NUM TITLE ENTRY...) structures.  Everything before the
first chapter (Gutenberg front matter) is dropped."
  (let ((chapters nil)
        (cur nil) (cur-num nil) (cur-title nil) (have-cur nil))
    (dolist (e entries)
      (pcase e
        (`(chapter ,num ,title)
         (when have-cur
           (push (append (list 'chapter cur-num cur-title) (nreverse cur))
                 chapters))
         (setq cur-num num cur-title title cur nil have-cur t))
        (_ (when have-cur (push e cur)))))
    (when have-cur
      (push (append (list 'chapter cur-num cur-title) (nreverse cur)) chapters))
    (nreverse chapters)))

;;; ------------------------------------------------------------------
;;; Typography helpers

(defun alice--runs (text)
  "Convert TEXT with _italic markup_ into styled runs."
  (if (not (string-match-p "_" text))
      (list text)
    (let ((parts (split-string text "_")) (i 0) out)
      (dolist (p parts)
        (unless (= (length p) 0)
          (push (if (= (% i 2) 1)
                    (list p :font alice-serif-italic)
                  p)
                out))
        (setq i (1+ i)))
      (nreverse out))))

(defun alice--raise-initial (runs)
  "Give the first character of RUNS a raised-initial style."
  (let ((r (car runs)))
    (if (and (stringp r) (> (length r) 1))
        (cons (list (substring r 0 1) :size 13.8)
              (cons (substring r 1) (cdr runs)))
      runs)))

(defun alice--spaced (s)
  "Letterspace S: one space between letters, three between words."
  (string-join
   (mapcar (lambda (w)
             (string-join (mapcar #'string (append w nil)) " "))
           (split-string s "[ \t]+" t))
   "   "))

(defun alice--ornament (flow &optional dy size gap)
  "Draw three small diamonds centered at the current flow position."
  (let* ((w (niori-pdf-flow--width flow))
         (cx (+ (niori-pdf-flow-x0 flow) (/ w 2.0)))
         (y (niori-pdf-flow--pdf-y flow
                                   (+ (niori-pdf-flow-y flow) (or dy 3))))
         (s (or size 2.4))
         (gap (or gap 16)))
    (niori-pdf-flow--with-graphics flow
      (niori-pdf-set-rgb-fill 0.42 0.40 0.36)
      (dolist (dx '(-1 0 1))
        (let ((x (+ cx (* dx gap))))
          (niori-pdf-move-to x (+ y s))
          (niori-pdf-line-to (+ x s) y)
          (niori-pdf-line-to x (- y s))
          (niori-pdf-line-to (- x s) y)
          (niori-pdf-close-path)
          (niori-pdf-fill))))))

(defun alice--draw-tracked (flow text font size
                                 &optional track color y-extra)
  "Draw TEXT centered with letter tracking at the flow cursor.
Letters get TRACK extra points of advance, spaces get three times
that, which reads as a classic letterspaced word gap.  Does not
advance the flow cursor."
  (let* ((f (niori-pdf-get-font (niori-pdf-flow-doc flow) font))
         (track (or track 1.0))
         (chars (append text nil))
         (widths (mapcar (lambda (c)
                           (+ (niori-pdf-char-width c f size)
                              (if (eq c 32) (* 3.0 track) track)))
                         chars))
         (total (apply #'+ widths))
         (x (+ (niori-pdf-flow-x0 flow)
               (/ (- (niori-pdf-flow--width flow) total) 2.0)))
         (y-base (+ (niori-pdf-flow-y flow)
                    (* 0.8 size) (or y-extra 0))))
    (niori-pdf-flow--with-graphics flow
      (apply #'niori-pdf-set-rgb-fill (or color '(0.1 0.1 0.1)))
      (niori-pdf-flow--emitf flow "BT\n/%s %s Tf\n"
                             (niori-pdf-font-resource f)
                             (niori-pdf-num size))
      (dolist (ch chars)
        (niori-pdf-flow--emitf
         flow "1 0 0 1 %s %s Tm\n"
         (niori-pdf-num x)
         (niori-pdf-num (niori-pdf-flow--pdf-y flow y-base)))
        (niori-pdf-flow--emit
         flow (niori-pdf--encoded-tj-operator
               (niori-pdf-font-encode (string ch) f) f))
        (setq x (+ x (car widths)))
        (setq widths (cdr widths)))
      (niori-pdf-flow--emitf flow "ET\n"))))

(defun alice--tracked-line (flow text font size leading
                                  &optional track color)
  "Draw tracked TEXT and advance by LEADING."
  (niori-pdf-flow--ensure-space flow leading)
  (alice--draw-tracked flow text font size track color)
  (niori-pdf-flow--advance flow leading))

(defun alice--wrap-title (title font size)
  "Return TITLE as one or two centered lines fitting the measure."
  (let* ((doc niori-pdf--doc)
         (f (niori-pdf-get-font doc font))
         (maxw (- (niori-pdf-flow--width niori-pdf--flow) 24)))
    (if (<= (niori-pdf-text-width title f size) maxw)
        (list title)
      (let ((words (split-string title " " t)) best bestd)
        (cl-loop for i from 1 to (1- (length words))
                 for l1 = (string-join (cl-subseq words 0 i) " ")
                 for l2 = (string-join (cl-subseq words i) " ")
                 for d = (abs (- (niori-pdf-text-width l1 f size)
                                 (niori-pdf-text-width l2 f size)))
                 when (or (null bestd) (< d bestd))
                 do (setq bestd d best (list l1 l2)))
        best))))

;;; ------------------------------------------------------------------
;;; Running heads and folios

(defun alice--rh-chapter-for (page)
  "Return (NUM TITLE PAGE Y) of the chapter open on PAGE, or nil.
Nil also when PAGE is itself a chapter opener (no running head)."
  (let ((best nil))
    (dolist (e alice--prev-chapters)
      (when (and (nth 2 e) (<= (nth 2 e) page)
                 (or (null best) (> (nth 2 e) (nth 2 best))))
        (setq best e)))
    (when (and best (< (nth 2 best) page))
      best)))

(defun alice--header (flow page)
  "Running head: book title on versos, chapter title on rectos;
no head on chapter opener pages."
  (when (> page alice-front-pages)
    (unless (cl-some (lambda (e) (= (nth 2 e) page))
                     alice--prev-chapters)
      (let* ((chapter (alice--rh-chapter-for page))
             (text (cond
                    ;; recto (book odd pages): chapter title when known
                    ((and chapter (= (% page 2) 1))
                     (upcase (nth 1 chapter)))
                    (t "ALICE'S ADVENTURES IN WONDERLAND"))))
        (let* ((font (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                         alice-sans))
               (size 6.8)
               (spaced (alice--spaced text))
               (w (niori-pdf-text-width spaced font size))
               (x (/ (- alice-page-w w) 2.0))
               (y (- alice-page-h 34)))
          (niori-pdf-flow--emitf
           flow "BT\n0.46 0.44 0.42 rg\n/%s %s Tf\n1 0 0 1 %s %s Tm\n"
           (niori-pdf-font-resource font)
           (niori-pdf-num size)
           (niori-pdf-num x)
           (niori-pdf-num y))
          (niori-pdf-flow--emit
           flow (niori-pdf--encoded-tj-operator
                 (niori-pdf-font-encode spaced font) font 'kerned))
          (niori-pdf-flow--emitf flow "ET\n"))))))

(defun alice--footer (flow page)
  "Centered folio, book numbering (front matter suppressed)."
  (when (> page alice-front-pages)
    (let* ((font (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                     alice-sans))
           (text (number-to-string (- page alice-front-pages)))
           (w (niori-pdf-text-width text font 8.8))
           (x (/ (- alice-page-w w) 2.0)))
      (niori-pdf-flow--emitf
       flow "BT\n0.34 0.34 0.34 rg\n/%s 8.8 Tf\n1 0 0 1 %s %s Tm\n"
       (niori-pdf-font-resource font)
       (niori-pdf-num x)
       (niori-pdf-num 30))
      (niori-pdf-flow--emit
       flow (niori-pdf--encoded-tj-operator
             (niori-pdf-font-encode text font) font))
      (niori-pdf-flow--emitf flow "ET\n"))))

;;; ------------------------------------------------------------------
;;; Front matter

(defun alice--front-matter ()
  ;; ---- page 1: half title
  (niori-pdf-flow-spacer 238)
  (alice--tracked-line niori-pdf--flow "ALICE'S ADVENTURES"
                       alice-sans 10.5 18 1.4 '(0.25 0.24 0.23))
  (alice--tracked-line niori-pdf--flow "IN WONDERLAND"
                       alice-sans 10.5 18 1.4 '(0.25 0.24 0.23))
  (niori-pdf-flow-page-break)
  ;; ---- page 2: title page
  (niori-pdf-flow-spacer 86)
  (alice--ornament niori-pdf--flow 2 2.7 18)
  (niori-pdf-flow-spacer 46)
  (niori-pdf-flow-paragraph "ALICE'S ADVENTURES"
                            :align 'center :font alice-sans-bold
                            :size 19.5 :space-after 8)
  (niori-pdf-flow-paragraph "IN WONDERLAND"
                            :align 'center :font alice-sans-bold
                            :size 19.5 :space-after 0)
  (niori-pdf-flow-spacer 30)
  (niori-pdf-flow-rule :thickness 0.6 :color '(0.5 0.48 0.45)
                       :indent (/ (- (niori-pdf-flow--width
                                      niori-pdf--flow)
                                     96)
                                  2.0)
                       :space-before 0 :space-after 0)
  (niori-pdf-flow-spacer 36)
  (niori-pdf-flow-paragraph "by Lewis Carroll"
                            :align 'center :font alice-serif-italic
                            :size 13)
  (niori-pdf-flow-spacer 16)
  (niori-pdf-flow-paragraph "The Millennium Fulcrum Edition 3.0"
                            :align 'center :font alice-sans :size 8
                            :color '(0.45 0.44 0.43))
  (niori-pdf-flow-spacer 122)
  (niori-pdf-flow-paragraph
   "Typeset with niori-pdf, the pure Emacs Lisp PDF engine"
   :align 'center :font alice-sans :size 7.5
   :color '(0.45 0.44 0.43) :space-after 3)
  (niori-pdf-flow-paragraph
   "Set in P052 and Nimbus Sans; all faces subsetted and embedded in Emacs"
   :align 'center :font alice-sans :size 7.5
   :color '(0.45 0.44 0.43) :space-after 6)
  (niori-pdf-flow-paragraph "MMXXVI"
                            :align 'center :font alice-serif :size 9.5)
  (niori-pdf-flow-page-break)
  ;; ---- page 3: colophon
  (alice--colophon)
  (niori-pdf-flow-page-break)
  ;; ---- page 4: contents (chapters start with their own page break)
  (alice--toc-page))

(defun alice--colophon ()
  (niori-pdf-flow-spacer 34)
  (alice--tracked-line niori-pdf--flow "COLOPHON"
                       alice-sans-bold 11 16 1.6 '(0.2 0.2 0.2))
  (niori-pdf-flow-spacer 14)
  (niori-pdf-flow-paragraph
   "This book was typeset entirely with niori-pdf, a pure Emacs Lisp
port of Marc Battyani's cl-pdf, running inside GNU Emacs.  The text
is set in P052, URW's Palatino revival, at 9.7/13.6 with Knuth-Plass
justification and American English TeX hyphenation; chapter headings
and running heads are set in Nimbus Sans.  All four faces are
OpenType/CFF fonts, subsetted to the glyphs actually used and
embedded by niori-pdf's own CFF subsetter.  The finished book is a
single PDF 1.4 file; no external typesetting tools were involved."
   :align 'justified :font alice-serif :size 8.8 :leading 12.6
   :space-after 10)
  (niori-pdf-flow-paragraph
   "Lewis Carroll's text is in the public domain.  This edition uses
the Project Gutenberg transcription (ebook number 11, the Millennium
Fulcrum Edition), converted to curly quotes on the way in.  The
contents list and every chapter carry working internal links, and a
bookmark outline is generated for the sidebar of your viewer."
   :align 'justified :font alice-serif :size 8.8 :leading 12.6
   :space-after 16)
  ;; QR code linking to the source text, centered
  (let* ((flow niori-pdf--flow)
         (enc (niori-pdf-qr-encode
               "https://www.gutenberg.org/ebooks/11" :ecl 'M))
         (ver (cdr enc))
         (n (+ 17 (* 4 ver)))          ; modules per side
         (ps 2.4)
         (side (* n ps))
         (cx (+ (niori-pdf-flow-x0 flow)
                (/ (niori-pdf-flow--width flow) 2.0)))
         (y-top (niori-pdf-flow--pdf-y flow
                                       (niori-pdf-flow-y flow))))
    (niori-pdf-flow--with-graphics flow
      (niori-pdf-barcode-qr-string
       "https://www.gutenberg.org/ebooks/11"
       (- cx (/ side 2.0)) y-top
       :point-size ps :ecl 'M))
    (niori-pdf-flow--advance flow (+ side 10))
    (niori-pdf-flow-paragraph
     "https://www.gutenberg.org/ebooks/11"
     :align 'center :font alice-sans :size 7.5
     :color '(0.42 0.42 0.42))))

(defun alice--toc-page ()
  (niori-pdf-flow-spacer 62)
  (alice--tracked-line niori-pdf--flow "CONTENTS"
                       alice-sans-bold 12.5 20 2.0 '(0.13 0.13 0.13))
  (niori-pdf-flow-spacer 22)
  (let* ((flow niori-pdf--flow)
         (doc niori-pdf--doc)
         (font (niori-pdf-get-font doc alice-sans))
         (size 9.6)
         (leading 20.5)
         (x0 (niori-pdf-flow-x0 flow))
         (x1 (niori-pdf-flow-x1 flow)))
    (dolist (e (or alice--prev-chapters
                   ;; first pass: placeholders keep pagination identical
                   (cl-loop for i from 1 to 12 collect (list "?" "" nil nil))))
      (let* ((num (nth 0 e))
             (title (nth 1 e))
             (page (nth 2 e))
             (page-str (if page
                           (number-to-string (- page alice-front-pages))
                         "0"))
             (row (if (= (length title) 0)
                      (alice--spaced num)
                    (concat (alice--spaced num) "  " (upcase title))))
             (y-entry (niori-pdf-flow-y flow))
             (y-base (+ y-entry (* 0.8 size)))
             (tw-page (niori-pdf-text-width page-str font size))
             (x-page (- x1 tw-page))
             (tw-row (niori-pdf-text-width row font size))
             (title-end (+ x0 tw-row))
             (dots-room (- x-page title-end 8))
             (dot-w (niori-pdf-text-width "." font size))
             (n-dots (max 0 (floor (/ dots-room dot-w)))))
        (niori-pdf-flow--ensure-space flow leading)
        (niori-pdf-flow--with-graphics flow
          (niori-pdf-flow--emitf flow "BT\n0.13 0.13 0.13 rg\n/%s %s Tf\n1 0 0 1 %s %s Tm\n"
                                 (niori-pdf-font-resource font)
                                 (niori-pdf-num size)
                                 (niori-pdf-num x0)
                                 (niori-pdf-num
                                  (niori-pdf-flow--pdf-y flow y-base)))
          (niori-pdf-flow--emit
           flow (niori-pdf--encoded-tj-operator
                 (niori-pdf-font-encode row font) font 'kerned))
          (when (> n-dots 0)
            (niori-pdf-flow--emitf
             flow "0.55 0.55 0.55 rg\n1 0 0 1 %s %s Tm\n"
             (niori-pdf-num (+ title-end 4))
             (niori-pdf-num (niori-pdf-flow--pdf-y flow y-base)))
            (niori-pdf-flow--emit
             flow (niori-pdf--encoded-tj-operator
                   (niori-pdf-font-encode
                    (make-string n-dots ?.) font) font)))
          (niori-pdf-flow--emitf
           flow "0.13 0.13 0.13 rg\n1 0 0 1 %s %s Tm\n"
           (niori-pdf-num x-page)
           (niori-pdf-num (niori-pdf-flow--pdf-y flow y-base)))
          (niori-pdf-flow--emit
           flow (niori-pdf--encoded-tj-operator
                 (niori-pdf-font-encode page-str font) font 'kerned))
          (niori-pdf-flow--emitf flow "ET\n")
          (when page
            (niori-pdf-add-deferred-link
             x0 (niori-pdf-flow--pdf-y flow (+ y-entry leading))
             (- x-page x0) leading page
             (- alice-page-h (nth 3 e)))))
        (niori-pdf-flow--advance flow leading))))
  (niori-pdf-outlines-add
   niori-pdf--doc "Contents"
   (niori-pdf-flow-page-number niori-pdf--flow)
   :level 1 :y (- alice-page-h 64)))

;;; ------------------------------------------------------------------
;;; The mouse's tail-tale: the famous spiral poem

;; The Gutenberg plain-text edition sets the tale as one indented
;; block, having lost the shape of the original 1865 woodcut (the
;; words winding around like a mouse's tail).  For the showcase the
;; original staircase is restored: one line per entry, indented by
;; INDENT-STEPS, with the wording and the _italic_ markup taken from
;; the source text.
(defconst alice--tail-poem
  '((0 . "Fury said to a")
    (1 . "mouse, That he")
    (2 . "met in the house,")
    (3 . "‘Let us both go to")
    (4 . "law: _I_ will prosecute")
    (5 . "_you_.—Come, I’ll take no")
    (6 . "denial; We must have a")
    (7 . "trial: For really this")
    (8 . "morning I’ve nothing")
    (9 . "to do.’")
    (8 . "Said the mouse to the cur,")
    (7 . "‘Such a trial, dear sir,")
    (6 . "With no jury or judge, would be")
    (5 . "wasting our breath.’")
    (4 . "‘I’ll be judge, I’ll be jury,’ Said")
    (3 . "cunning old Fury: ‘I’ll")
    (2 . "try the whole cause, and")
    (1 . "condemn you to death.’”")))

(defun alice--render-tail-poem ()
  "Set the mouse's spiral tail after the original layout.
Each line is its own unhyphenated left-set paragraph; the staircase
starts at an indent that centers the widest line of the tail in the
measure."
  (let* ((size 9.4)
         (unit 9.0)
         (font (niori-pdf-get-font niori-pdf--doc alice-serif))
         (widths
          (mapcar (lambda (e)
                    (niori-pdf-text-width
                     (replace-regexp-in-string "_" "" (cdr e))
                     font size))
                  alice--tail-poem))
         (ends (cl-mapcar (lambda (e w) (+ (* unit (car e)) w))
                          alice--tail-poem widths))
         (base (max 0.0 (/ (- (niori-pdf-flow--width niori-pdf--flow)
                              (apply #'max ends))
                           2.0))))
    (dolist (e alice--tail-poem)
      (niori-pdf-flow-paragraph
       (alice--runs (cdr e))
       :align 'left :font alice-serif :size size :leading 12.4
       :hyphenate nil
       :indent (+ base (* unit (car e)))
       :space-after 0))))

;;; ------------------------------------------------------------------
;;; Chapters

(defun alice--render-chapter (entry)
  (pcase entry
    (`(chapter ,num ,title . ,blocks)
     (let ((flow niori-pdf--flow))
       (niori-pdf-flow-page-break)
       (niori-pdf-flow-spacer 40)
       ;; record for the TOC / running heads / outline
       (let ((page (niori-pdf-flow-page-number flow))
             (y-top (niori-pdf-flow-y flow)))
         (push (list num title page y-top) alice--chapters)
         (niori-pdf-outlines-add
          niori-pdf--doc
          (concat "Chapter " num ".  " title)
          page :level 1
          :y (niori-pdf-flow--pdf-y flow (- y-top 4))))
       ;; opener: letterspaced numeral, sans title, ornament
       (alice--tracked-line flow (concat "CHAPTER " num)
                            alice-sans 10 20 1.5 '(0.36 0.35 0.33))
       (niori-pdf-flow-spacer 6)
       (let ((lines (alice--wrap-title
                     (upcase title) alice-sans-bold 15.5)))
         (dolist (l lines)
           (niori-pdf-flow-paragraph
            l :align 'center :font alice-sans-bold :size 15.5
            :leading 19.5
            :space-after (if (eq l (car (last lines))) 0 5))))
       (niori-pdf-flow-spacer 12)
       (alice--ornament niori-pdf--flow 2 2.2 16)
       (niori-pdf-flow-spacer 28)
       ;; body blocks
       (let ((first t) (prev-sep nil))
         (dolist (b blocks)
           (pcase b
             (`(paragraph ,text)
              (let ((runs (alice--runs text)))
                (when first
                  (setq runs (alice--raise-initial runs)))
                (niori-pdf-flow-paragraph
                 runs :align 'justified :font alice-serif
                 :size alice-body-size :leading alice-body-leading
                 :space-after 4.6)
                (setq first nil prev-sep nil)))
             (`(verse . ,lines)
              (niori-pdf-flow-spacer 7)
              (if (and (car lines)
                       (string-match-p "Fury said to a mouse"
                                       (car lines)))
                  ;; the mouse's tail-tale: restore the spiral layout
                  ;; of the original 1865 edition
                  (alice--render-tail-poem)
                (dolist (l lines)
                  (niori-pdf-flow-paragraph
                   (alice--runs l) :align 'center :font alice-serif
                   :size 9.4 :leading 12.4 :hyphenate nil
                   :space-after 0)))
              (niori-pdf-flow-spacer 7)
              (setq prev-sep nil))
             (`(separator)
              ;; consecutive separator blocks in the source collapse
              ;; into one ornament
              (unless prev-sep
                (niori-pdf-flow-spacer 10)
                (alice--ornament niori-pdf--flow 3 2.0 15)
                (niori-pdf-flow-spacer 10))
              (setq prev-sep t))
             (`(end)
              (niori-pdf-flow-spacer 34)
              (alice--ornament niori-pdf--flow 2 2.2 16)
              (niori-pdf-flow-spacer 20)
              (alice--tracked-line flow "THE END"
                                   alice-sans 10 20 1.8
                                   '(0.36 0.35 0.33))))))))))

;;; ------------------------------------------------------------------
;;; Build

(defun alice--pass ()
  "One builder pass; returns the finished document."
  (setq alice--chapters nil)
  (niori-pdf-with-flow-document
      (:width 419.53 :height 595.28
              :margins '(46 50 46 54)
              :header-fn #'alice--header
              :footer-fn #'alice--footer
              :default-font "/usr/share/fonts/opentype/urw-base35/P052-Roman.otf"
              :default-size 9.7
              :language 'american
              :pdfa alice-pdfa
              :title "Alice's Adventures in Wonderland"
              :author "Lewis Carroll"
              :subject "A niori-pdf typesetting showcase"
              :keywords "PDF, Emacs Lisp, niori-pdf, Lewis Carroll"
              :creator "niori-pdf.el 0.1.0")
    (alice--front-matter)
    (let ((count 0))
      (dolist (ch alice--book)
        (when (or (null alice-debug-limit)
                  (< count alice-debug-limit))
          (alice--render-chapter ch))
        (setq count (1+ count))))))

(defun alice-book (&optional out-file)
  "Build the book; two passes until TOC page numbers are stable."
  (let ((doc nil) (prev nil) (pass 0) (stable nil))
    (while (not stable)
      (setq doc (alice--pass))
      (setq stable (and prev (equal prev alice--chapters)))
      (setq alice--prev-chapters (reverse alice--chapters))
      (setq prev alice--chapters)
      (setq pass (1+ pass))
      (when (> pass 3) (setq stable t)))
    (message "alice: %d passes, %d pages, %d chapters"
             pass (length (niori-pdf-document-pages doc))
             (length alice--chapters))
    (when out-file
      (niori-pdf-flow-save doc out-file))
    doc))

;;; ------------------------------------------------------------------

(setq alice--book (alice--group-chapters (alice--parse-book)))

(unless (getenv "ALICE_NO_AUTORUN")
  (let* ((out (if alice-pdfa
                  "/tmp/niori-test/alice-in-wonderland-pdfa.pdf"
                "/tmp/niori-test/alice-in-wonderland.pdf"))
         (doc (alice-book out)))
    (princ (format "%s written: %d pages, %d objects (pdfa: %s)\n"
                   (file-name-nondirectory out)
                   (length (niori-pdf-document-pages doc))
                   (niori-pdf-document-n-objects doc)
                   (if alice-pdfa "2b" "no")))))

(provide 'alice-book)
;;; alice-book.el ends here
