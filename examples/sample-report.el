;;; sample-report.el --- Example: a complete data report with niori-pdf -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Igor Anikeev
;; See LICENSE and THIRD-PARTY-NOTICES for applicable terms.

;; Run with:  emacs --batch --load sample-report.el
;;   --no-hyphenation   build the comparison variant without global
;;                      hyphenation (also: NIORI_NO_HYPHEN=1)
;;   --pdfa             build the PDF/A-2b archival variant
;;                      (also: NIORI_PDFA=1)
;; Demonstrates the full niori-pdf feature set in one document:
;;   - flow typesetting engine: headings, justified paragraphs, KPI
;;     boxes in columns, a table with repeated headers, TOC, header/
;;     footer, automatic pagination
;;   - cl-typesetting block API: itemize/enumerate lists with hanging
;;     indents, floats with paragraph wrap-around, table cells with
;;     rowspan/colspan and vertical alignment, keep-with-next rows,
;;     run decorations (superscript, underline, strikethrough, run
;;     background), reference points resolved by the multi-pass
;;     layout, vertical glue
;;   - the chart module (niori-pdf-charts, port of cl-pdf chart.lisp):
;;     grouped histogram and pie chart, all vector
;;   - vector curves and arcs drawn with Bezier path operators
;;   - bar codes: Code 128, Code 39 and EAN-13
;;   - QR codes (niori-pdf-qrcode: full ISO 18004 encoder, verified by
;;     round-trip decoding) drawn with the cl-pdf matrix drawer
;;   - an interactive AcroForm: text fields, checkbox, radio group,
;;     dropdown choice and a URI push button, all with appearance streams
;;   - ZapfDingbats and Symbol native text encodings
;;   - embedded TrueType (subsetted, CIDFontType2), OpenType/CFF
;;     (CIDFontType0), TrueType collection (.ttc member) and Type 1
;;     (.pfb, FontFile with eexec-extracted widths) fonts with
;;     Unicode text
;;   - TeX-pattern hyphenation in ten shipped languages
;;   - with --pdfa: PDF/A-2b archival output (ICC sRGB OutputIntent,
;;     XMP metadata, embedded base-14 substitutions)

(add-to-list 'load-path
             (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'niori-pdf)
(require 'hyphenation-texts)
(require 'font-utils)

(defconst sample-ttf
  (niori-example-fontconfig-file "DejaVu Sans" "Book" ".ttf")
  "TrueType font used for the embedding demo.")
(defconst sample-ttf-bold
  (niori-example-fontconfig-file "DejaVu Sans" "Bold" ".ttf")
  "Bold TrueType font used for the embedding demo.")
(defconst sample-ttf-oblique
  (niori-example-fontconfig-file "DejaVu Sans" "Oblique" ".ttf")
  "Oblique TrueType font used for PDF/A substitution.")
(defconst sample-ttf-mono
  (niori-example-fontconfig-file "DejaVu Sans Mono" "Book" ".ttf")
  "Monospaced TrueType font used for PDF/A substitution.")
(defconst sample-otf
  (niori-example-fontconfig-file "Nimbus Sans" "Regular" ".otf")
  "OpenType/CFF font used for the embedding demo.")
(defconst sample-ttc
  (niori-example-fontconfig-file "WenQuanYi Zen Hei Mono" "Regular" ".ttc")
  "TrueType collection used for the .ttc demo.")
(defconst sample-ttc-index 1
  "Collection member drawn in the demo (WenQuanYi Zen Hei Mono).")
(defconst sample-pfb
  (niori-example-fontconfig-file "Nimbus Roman No9 L" "Regular" ".pfb")
  "Type 1 font used for the embedding demo (Times clone).")
(defconst sample-pfb-bold
  (niori-example-fontconfig-file "Nimbus Roman No9 L" "Medium" ".pfb")
  "Bold Type 1 font used for PDF/A substitution.")
(defconst sample-pfb-italic
  (niori-example-fontconfig-file "Nimbus Roman No9 L" "Regular Italic" ".pfb")
  "Italic Type 1 font used for PDF/A substitution.")
(defconst sample-pfb-sans
  (niori-example-fontconfig-file "Nimbus Sans L" "Regular" ".pfb")
  "Second Type 1 font for the demo (Helvetica clone).")

(defvar sample-pdfa
  (or (member "--pdfa" command-line-args)
      (equal (getenv "NIORI_PDFA") "1"))
  "Build the PDF/A-2b archival variant of this report.
Run the script with `--pdfa' (after `--' on the Emacs command line)
or with NIORI_PDFA=1 in the environment.  The variant is written to
sample-report-pdfa[-nohyphen].pdf and enables the archival writer:
PDF 1.7 header, /ID trailer, XMP metadata, sRGB OutputIntent with an
embedded ICC profile, and the base-14 fonts mapped to embedded
ttf/pfb files via `niori-pdf-pdfa-base14-substitutions' (ISO 19005
forbids unembedded fonts, including Symbol/ZapfDingbats).")

;; In PDF/A mode every font must be embedded: map the base-14 names
;; used by this report (paragraph defaults, KPI cards, headers,
;; footers, form labels) to embedded files.  Symbol and ZapfDingbats
;; runs are drawn from Unicode strings through the font's cmap, so
;; mapping them to DejaVu Sans keeps the same visible glyphs.  The
;; interactive AcroForm section is skipped in this variant (its
;; appearance streams need single-byte fonts).
(when sample-pdfa
  (setq niori-pdf-pdfa-base14-substitutions
        `(("Helvetica" . ,sample-ttf)
          ("Helvetica-Bold" . ,sample-ttf-bold)
          ("Helvetica-Oblique" . ,sample-ttf-oblique)
          ("Courier" . ,sample-ttf-mono)
          ("Times-Roman" . ,sample-pfb)
          ("Times-Bold" . ,sample-pfb-bold)
          ("Times-Italic" . ,sample-pfb-italic)
          ("Symbol" . ,sample-ttf)
          ("ZapfDingbats" . ,sample-ttf))))

(defvar sample-hyphenate
  (not (or (member "--no-hyphenation" command-line-args)
           (equal (getenv "NIORI_NO_HYPHEN") "1")))
  "Global hyphenation switch for this report.
Defaults to t.  Run the script with `--no-hyphenation' (after `--' on
the Emacs command line) or with NIORI_NO_HYPHEN=1 in the environment
to build the comparison variant set without global hyphenation; it is
written to sample-report-nohyphen.pdf so the two versions can be
juxtaposed.  The section 9 demo of explicit break points works in
both variants, because it calls the hyphenation API directly.")

(defvar sample-output-file
  (concat "sample-report"
          (unless sample-hyphenate "-nohyphen")
          (when sample-pdfa "-pdfa")
          ".pdf")
  "Name of the PDF this run writes.")

;; the footnote separator follows the report's light blue-gray rules
(setq niori-pdf-flow-footnote-rule-color '(0.62 0.72 0.86))

(defun sample-kpi-box (label value note)
  "One KPI card: LABEL on top, big VALUE, small NOTE."
  (niori-pdf-flow-box
   (lambda ()
     (niori-pdf-flow-paragraph label :size 8 :font "Helvetica-Bold"
                               :color '(0.45 0.5 0.58) :space-after 2)
     (niori-pdf-flow-paragraph value :size 22 :font "Helvetica-Bold"
                               :color '(0.13 0.25 0.51) :space-after 2)
     (niori-pdf-flow-paragraph note :size 7.5 :color '(0.45 0.5 0.58)))
   :background '(0.967 0.973 0.984)
   :border '(0.86 0.88 0.91)
   :padding 9))

(defun sample-figure-box (builder &optional height)
  "Framed figure box running BUILDER; the box is HEIGHT points tall."
  (niori-pdf-flow-box
   (lambda ()
     (funcall builder)
     (niori-pdf-flow--advance niori-pdf--flow (or height 0)))
   :border '(0.86 0.88 0.91)
   :background '(0.99 0.99 1.0)
   :padding 12
   :width 460 :align 'center))

(defun sample-charts-section ()
  "Section 3: the chart module (histogram + pie)."
  (niori-pdf-flow-heading "3.  Charts: histogram and pie" :level 1)
  (niori-pdf-flow-paragraph
   "The two charts below come from the chart module of niori-pdf, a
port of cl-pdf's chart.lisp.  Everything is vector graphics: axis
scale computation, tick layout, bars, wedges and legends are drawn
with PDF path operators, so the charts stay crisp at any zoom level."
   :align 'justified :space-after 10)
  ;; grouped histogram with legend
  (sample-figure-box
   (lambda ()
     (let* ((flow niori-pdf--flow)
            (x (niori-pdf-flow-x0 flow))
            (w (- (niori-pdf-flow-x1 flow) x))
            (y (niori-pdf-flow--pdf-y flow
                                     (+ (niori-pdf-flow-y flow) 185))))
       (niori-pdf-chart-histogram
        (+ x 28) y (- w 138) 170
        '((118 134 129 156) (74 81 79 95))
        '("Q1" "Q2" "Q3" "Q4")
        '(("revenue" "steelblue") ("cost" "indianred"))
        :title "Quarterly revenue and cost (k EUR)")))
   212)
  (niori-pdf-flow-spacer 12)
  ;; pie chart
  (sample-figure-box
   (lambda ()
     (let* ((flow niori-pdf--flow)
            (x (niori-pdf-flow-x0 flow))
            (y (niori-pdf-flow--pdf-y flow
                                     (+ (niori-pdf-flow-y flow) 165))))
       (niori-pdf-chart-pie
        (+ x 28) y 150 150
        '(34.1 19.0 9.9 4.3 32.7)
        '(("Core platform" "steelblue")
          ("Add-on modules" "indianred")
          ("Support contracts" "seagreen")
          ("Training" "darkorchid")
          ("Other" "goldenrod"))
        :title "Q4 product mix (%)")))
   192)
  (niori-pdf-flow-spacer 10))

(defun sample--wedge-path (cx cy r start extent)
  "Emit the path of a pie wedge: center, arc START..+EXTENT degrees,
back to the center (built from cubic Bezier segments)."
  (let* ((steps (max 1 (ceiling (/ (abs extent) 45.0))))
         (seg (/ extent (float steps)))
         (k (* (/ 4.0 3.0)
               (tan (/ (* seg 3.141592653589793) 720.0))))
         (to-rad (lambda (d) (* d 3.141592653589793 (/ 1 180.0)))))
    (niori-pdf-move-to cx cy)
    (let ((a0 (funcall to-rad start)))
      (niori-pdf-line-to (+ cx (* r (cos a0))) (+ cy (* r (sin a0)))))
    (dotimes (i steps)
      (let* ((a0 (funcall to-rad (+ start (* i seg))))
             (a1 (funcall to-rad (+ start (* (1+ i) seg)))))
        (niori-pdf-curve-to
         (+ cx (* r (- (cos a0) (* k (sin a0)))))
         (+ cy (* r (+ (sin a0) (* k (cos a0)))))
         (+ cx (* r (+ (cos a1) (* k (sin a1)))))
         (+ cy (* r (- (sin a1) (* k (cos a1)))))
         (+ cx (* r (cos a1)))
         (+ cy (* r (sin a1))))))
    (niori-pdf-close-path)))

(defun sample-curves-section ()
  "Section 5: vector curves and arcs (pure path operators)."
  (niori-pdf-flow-heading "5.  Vector curves and arcs" :level 1)
  (niori-pdf-flow-paragraph
   "PDF paths are not limited to straight lines.  The figure below is
drawn with cubic Bezier curves ('niori-pdf-curve-to'), arcs
('niori-pdf-arc'), circles and ellipses; the small dots mark the Bezier
control points of the wave."
   :align 'justified :space-after 10)
  (sample-figure-box
   (lambda ()
     (let* ((flow niori-pdf--flow)
            (x0 (niori-pdf-flow-x0 flow))
            (y0 (niori-pdf-flow-y flow))
            (top (lambda (d) (niori-pdf-flow--pdf-y flow (+ y0 d)))))
       ;; a sine-like wave built from four Bezier segments; the wave
       ;; baseline sits 60pt below the top of the box and swings ±50
       (let ((by (funcall top 60)))
         (niori-pdf-set-rgb-stroke 0.231 0.361 0.659)
         (niori-pdf-set-line-width 1.6)
         (niori-pdf-move-to x0 by)
         (niori-pdf-curve-to (+ x0 40) (- by 50) (+ x0 90) (- by 50)
                             (+ x0 130) by)
         (niori-pdf-curve-to (+ x0 170) (+ by 50) (+ x0 220) (+ by 50)
                             (+ x0 260) by)
         (niori-pdf-curve-to (+ x0 300) (- by 50) (+ x0 350) (- by 50)
                             (+ x0 390) by)
         (niori-pdf-stroke)
         ;; control points as small filled circles
         (niori-pdf-set-rgb-fill 0.8039 0.3608 0.3608)
         (dolist (p (list (list (+ x0 40) (- by 50))
                          (list (+ x0 90) (- by 50))
                          (list (+ x0 170) (+ by 50))
                          (list (+ x0 220) (+ by 50))
                          (list (+ x0 300) (- by 50))
                          (list (+ x0 350) (- by 50))))
           (niori-pdf-circle (nth 0 p) (nth 1 p) 2.4)
           (niori-pdf-fill)))
       ;; circle, ellipse and a filled arc wedge at 170pt from the top
       (let ((cy (funcall top 170)))
         (niori-pdf-set-line-width 1.2)
         (niori-pdf-set-rgb-stroke 0.1804 0.5451 0.3412)
         (niori-pdf-circle (+ x0 45) cy 28)
         (niori-pdf-stroke)
         (niori-pdf-set-rgb-stroke 0.6 0.1961 0.8)
         (niori-pdf-ellipse (+ x0 130) cy 40 24)
         (niori-pdf-stroke)
         (niori-pdf-set-rgb-fill 0.8549 0.6471 0.1255)
         (sample--wedge-path (+ x0 230) cy 30 15 125)
         (niori-pdf-fill)
         (niori-pdf-set-line-width 1.0)
         (niori-pdf-set-rgb-stroke 0.8549 0.6471 0.1255)
         (niori-pdf-circle (+ x0 230) cy 30)
         (niori-pdf-stroke))
       ;; labels at 215pt from the top
       (niori-pdf-set-font "Helvetica" 7)
       (niori-pdf-set-rgb-fill 0.25 0.25 0.25)
       (niori-pdf-draw-centered-text (+ x0 45) (funcall top 215) "circle")
       (niori-pdf-draw-centered-text (+ x0 130) (funcall top 215) "ellipse")
       (niori-pdf-draw-centered-text (+ x0 230) (funcall top 215)
                                     "arc wedge")
       (niori-pdf-draw-centered-text (+ x0 330) (funcall top 215)
                                     "bezier wave")))
   228)
  (niori-pdf-flow-spacer 10))

(defun sample-barcode-section ()
  "Section 6: bar codes (port of cl-pdf bar-codes.lisp + Code 39/EAN-13)."
  (niori-pdf-flow-heading "6.  Bar codes and QR codes" :level 1)
  (niori-pdf-flow-paragraph
   "The bar-code module ports cl-pdf's bar-codes.lisp: Code 128 with
the complete 107-entry symbology table (charset C is selected
automatically for digit pairs), plus Code 39 and EAN-13 with check
digit computation and human readable text.  Below the 1D symbols,
three QR codes are encoded by niori-pdf's own ISO 18004 encoder
(byte mode, versions 1-10, all four error correction levels,
Reed-Solomon over GF(256), all eight masks with the ISO penalty
rules) and drawn with the cl-pdf QR matrix port."
   :align 'justified :space-after 10)
  (sample-figure-box
   (lambda ()
     (let* ((flow niori-pdf--flow)
            (x0 (niori-pdf-flow-x0 flow))
            (y0 (niori-pdf-flow-y flow))
            (top (lambda (d) (niori-pdf-flow--pdf-y flow (+ y0 d)))))
       ;; Code 128: bars grow upward from the baseline; the taller
       ;; start/stop bars reach 12pt below the top of the box
       (niori-pdf-barcode-code128 "NIORI-PDF 2026" (+ x0 20)
                                  (funcall top 70)
                                  :height 44 :width 240 :show-string nil)
       (niori-pdf-set-font "Helvetica" 7)
       (niori-pdf-set-rgb-fill 0.25 0.25 0.25)
       (niori-pdf-draw-centered-text (+ x0 140) (funcall top 82)
                                     "Code 128 - \"NIORI-PDF 2026\"")
       ;; EAN-13 with check digit and human readable digits
       (niori-pdf-barcode-ean13 "4006381333931" (+ x0 20)
                                (funcall top 150)
                                :height 42 :width 170)
       (niori-pdf-draw-centered-text (+ x0 105) (funcall top 184)
                                     "EAN-13 (check digit computed)")
       ;; Code 3 of 9 next to the EAN symbol
       (niori-pdf-barcode-code39 "NIORI 2026" (+ x0 230)
                                 (funcall top 150)
                                 :height 42 :width 190 :show-text nil)
       (niori-pdf-draw-centered-text (+ x0 325) (funcall top 184)
                                     "Code 3 of 9 - \"NIORI 2026\"")))
   196)
  (niori-pdf-flow-spacer 10)
  ;; QR codes: the matrix comes from niori-pdf-qrcode (a full ISO 18004
  ;; encoder written for niori-pdf - cl-pdf only draws a pre-computed
  ;; matrix) and is rendered by `niori-pdf-barcode-qr-string', the
  ;; string-level front-end over the cl-pdf `draw-qr-code' port.
  (sample-figure-box
   (lambda ()
     (let* ((flow niori-pdf--flow)
            (x0 (niori-pdf-flow-x0 flow))
            (y0 (niori-pdf-flow-y flow))
            (top (lambda (d) (niori-pdf-flow--pdf-y flow (+ y0 d)))))
       (niori-pdf-set-font "Helvetica" 7)
       (niori-pdf-set-rgb-fill 0.25 0.25 0.25)
       (niori-pdf-barcode-qr-string
        "https://niori.example.com/download/niori-pdf" (+ x0 18)
        (funcall top 14)
        :point-size 2.36 :ecl 'M)
       (niori-pdf-draw-centered-text (+ x0 56) (funcall top 102)
                                     "URL, level M")
       (niori-pdf-barcode-qr-string
        "niori-report:Q4-2026;author=emacs;qr-level=H" (+ x0 150)
        (funcall top 14)
        :point-size 2.11 :ecl 'H)
       (niori-pdf-draw-centered-text (+ x0 188) (funcall top 102)
                                     "payload, level H")
       (niori-pdf-barcode-qr-string
        "Gr\374\337e aus M\374nchen - 100% Emacs Lisp" (+ x0 282)
        (funcall top 14)
        :point-size 2.36 :ecl 'M)
       (niori-pdf-draw-centered-text (+ x0 320) (funcall top 102)
                                     "UTF-8 bytes")))
   110)
  (niori-pdf-flow-spacer 10))

(defun sample-dingbats-section ()
  "Section 7: ZapfDingbats and Symbol native encodings."
  (niori-pdf-flow-heading "7.  Symbol and ZapfDingbats fonts" :level 1)
  (niori-pdf-flow-paragraph
   "The base-14 fonts include Symbol and ZapfDingbats.  They do not use
WinAnsiEncoding: niori-pdf maps Unicode characters to each font's
native byte codes, so a mixed-style paragraph can simply name the
font per run.  ZapfDingbats supplies the symbols"
   :align 'justified :space-after 6)
  (niori-pdf-flow-paragraph
   (list "scissors, pencil, telephone, envelope, check marks, star and arrow:  "
         (list "✂ ✎ ☎ ✉ ✓ ✔ ★ ➔" :font "ZapfDingbats" :size 11
               :color '(0.36 0.24 0.59)))
   :align 'center :size 10 :space-after 8)
  (niori-pdf-flow-paragraph
   "and the Symbol font supplies Greek letters and mathematical
operators directly from their Unicode code points:"
   :align 'justified :space-after 6)
  (niori-pdf-flow-paragraph
   (list (list "α β γ δ ε π σ Σ λ ω  •  ∞ ≈ ≠ ≤ ≥ ± √ ∫ →"
               :font "Symbol" :size 11 :color '(0.16 0.35 0.55)))
   :align 'center :size 10 :space-after 8)
  (niori-pdf-flow-paragraph
   (if sample-pdfa
       "This PDF/A variant maps Symbol and ZapfDingbats to an embedded
TrueType font (ISO 19005 forbids unembedded fonts), so the glyphs
above flow through the font's Unicode cmap instead of the native
byte encodings - the visible symbols are identical.  The non-archival
variant draws them with the real Adobe AFM metrics of the base-14
fonts."
     "Both fonts are drawn with real Adobe AFM metrics, so the surrounding
text flow measures them correctly and the Knuth-Plass line breaker
treats them like any other word.")
   :align 'justified :space-after 8))

(defun sample-embedded-fonts-section ()
  "Section 8: TrueType, OpenType/CFF, collection and Type 1 embedding."
  (niori-pdf-flow-heading
   "8.  Embedded fonts: TrueType, OpenType, collections, Type 1"
   :level 1)
  (niori-pdf-flow-paragraph
   "Any font file can be used where a base-14 name would go: .ttf,
.otf and .ttc paths are parsed and subsetted; .pfb and .pfa Type 1
programs are embedded whole with their widths read out of the
charstrings.  TrueType fonts are embedded as Type0/CIDFontType2 with
an Identity-H encoding and a ToUnicode map, so text extraction keeps
working.  The paragraph below is set in DejaVu Sans (regular and
bold runs), a TrueType font:"
   :align 'justified :space-after 6)
  (niori-pdf-flow-paragraph
   (list "DejaVu Sans regular, "
         (list "DejaVu Sans bold, " :font sample-ttf-bold)
         (list "and Unicode beyond WinAnsi: Ω ≈ ∞ ✓ é ü ß € 24"
               :font sample-ttf))
   :align 'justified :size 9.5 :space-after 8)
  (niori-pdf-flow-paragraph
   "OpenType fonts with CFF outlines are embedded as Type0/CIDFontType0
with a FontFile3/OpenType stream whose CFF table is subsetted like the
TrueType ones: the charstrings of the used glyphs keep their original
glyph ids, the charset is copied, the DICTs are rebuilt and the
subroutine indexes are reduced to what the retained glyphs call.  This
paragraph is set in Nimbus Sans, the URW clone of Helvetica:"
   :align 'justified :space-after 6)
  (niori-pdf-flow-paragraph
   "Nimbus Sans Regular (OpenType/CFF): the quick brown fox jumps over
the lazy dog while the typesetter subsets the CFF charstrings, encodes
and embeds the font - all inside Emacs."
   :font sample-otf
   :align 'justified :size 9.5 :space-after 8)
  (niori-pdf-flow-paragraph
   "TrueType collections (.ttc) pack several fonts into one file;
niori-pdf parses the collection header and reads the member you
select with a (:file PATH :index N) spec.  The line below is set in
member 1 of the WenQuanYi collection (Zen Hei Mono, one of three
fonts in the file), drawn together with CJK text:"
   :align 'justified :space-after 6)
  (niori-pdf-flow-paragraph
   (list "WenQuanYi Zen Hei Mono (ttc member 1): TrueType collections work."
         (list " 中文 TrueType 集合，成员一。" :font (list :file sample-ttc
                                                  :index sample-ttc-index)))
   :font (list :file sample-ttc :index sample-ttc-index)
   :align 'justified :size 9.5 :space-after 8)
  (niori-pdf-flow-paragraph
   "Type 1 fonts (.pfb binaries and .pfa ASCII programs) are embedded
whole with a /FontFile stream; niori-pdf decrypts the eexec section
and reads every advance width straight out of the CharStrings, so no
companion .afm file is needed.  This paragraph is set in URW Nimbus
Roman (the Times clone), with a Nimbus Sans run inside it:"
   :align 'justified :space-after 6)
  (niori-pdf-flow-paragraph
   (list "Nimbus Roman (Type 1, .pfb): the quick brown fox jumps over
the lazy dog, "
         (list "with a Nimbus Sans run" :font sample-pfb-sans)
         " - widths, StemV, FontBBox and ItalicAngle all come from the
font program itself."
         )
   :font sample-pfb
   :align 'justified :size 9.5 :space-after 8))

(defun sample--hyphen-marked (word lang)
  "Return WORD with a middle dot at every hyphenation point of LANG."
  (string-join (niori-pdf-hyphen-word-splits word lang) "\u00b7"))

(defun sample--break-point-words (paragraph lang &optional n)
  "Return the N longest hyphenatable words of PARAGRAPH marked with
middle dots at their LANG break points."
  (let* ((words (delete-dups
                 (split-string paragraph "[^[:alpha:]]+" t)))
         (scored
          (sort (delq nil
                      (mapcar (lambda (w)
                                (and (>= (length w) 9)
                                     (cons (length w) w)))
                              words))
                (lambda (a b) (> (car a) (car b)))))
         (picked (let ((acc nil))
                   (dolist (s scored)
                     (when (and (< (length acc) (or n 3))
                                (niori-pdf-hyphenate-word (cdr s) lang))
                       (push (cdr s) acc)))
                   (nreverse acc))))
    (mapconcat (lambda (w) (sample--hyphen-marked w lang))
               picked ", ")))

(defvar sample-hyphen-langs
  (list
   (list "American English" 'american "american" "Helvetica"
         "Henry David Thoreau, Walking (1862).")
   (list "British English" 'british "british" "Helvetica"
         "Jerome K. Jerome, Three Men in a Boat (1889).")
   (list "German" 'german "german" "Helvetica"
         "Theodor Storm, Der Schimmelreiter (1888).")
   (list "French" 'french "french" "Helvetica"
         "Guy de Maupassant, Le Horla (1887).")
   (list "Spanish" 'spanish "spanish" "Helvetica"
         "Benito Pérez Galdós, Fortunata y Jacinta (1887).")
   (list "Italian" 'italian "italian" "Helvetica"
         "Carlo Collodi, Le avventure di Pinocchio (1883).")
   (list "Portuguese" 'portuguese "portuguese" "Helvetica"
         "Machado de Assis, O Alienista (1882).")
   (list "Esperanto" 'esperanto "esperanto" sample-otf
         "L. L. Zamenhof, Dua Libro de l’ Lingvo Internacia (1888).")
   (list "Ukrainian" 'ukrainian "ukrainian" sample-otf
         "Mykhailo Kotsiubynsky, Intermezzo (1909).")
   (list "Russian" 'russian "russian" sample-otf
         "Anton Chekhov, Дама с собачкой (1899)."))
  "Language demo entries: (TITLE LANG TEXT-KEY FONT NOTE).")

(defun sample-hyphenation-section ()
  "Section 9: hyphenation in the ten shipped languages.
Literary excerpts in each language are set with that language's
patterns; the break points of the longest words are shown with
middle dots."
  (niori-pdf-flow-heading "9.  International hyphenation" :level 1)
  (niori-pdf-flow-paragraph
   (concat (if sample-hyphenate
               "Every paragraph is broken with the Knuth-Plass algorithm
and the Liang/TeX hyphenation patterns of its language.  "
             "This comparison variant was built with --no-hyphenation,
so every paragraph relies on interword glue alone to meet the right
margin.  Build the default report (without the flag) to see the same
text set with hyphenation and compare the two.  ")
           "niori-pdf ships
pattern sets for American English, British English, German, French,
Spanish, Italian, Portuguese, Esperanto, Ukrainian and Russian.
The document default is set with the :language key and each paragraph
can override it.  The examples below use literary excerpts in their
original languages; the dots in the Break points lines mark where the
patterns allow a hyphen (word, not paragraph, break points).")
   :align 'justified :space-after 10)
  (let ((idx 0))
    (dolist (entry sample-hyphen-langs)
      (let* ((title (nth 0 entry))
             (lang (nth 1 entry))
             (text-key (nth 2 entry))
             (font (nth 3 entry))
             (note (nth 4 entry))
             (paras (symbol-value
                     (intern (concat "hyphenation-texts--"
                                     text-key))))
             (heading
              (format "9.%d  %s" (cl-incf idx) title)))
        (niori-pdf-flow-heading heading :level 2)
        (niori-pdf-flow-paragraph note
				  :font font
                                  :size 9 :color '(0.35 0.4 0.47)
                                  :space-after 4)
        (dolist (para paras)
          (niori-pdf-flow-paragraph para :font font :size 9.5
                                    :align 'justified :language lang
                                    :space-after 6))
        (niori-pdf-flow-paragraph
         (concat "Break points: "
                 (sample--break-point-words (car paras) lang 4))
         :size 8 :font font
         :color '(0.45 0.5 0.58) :space-after 10))))
  (niori-pdf-flow-paragraph
   "The hyphenation points come from the pattern files shipped with
niori-pdf. Words shorter than the language's minima are never broken,
and the three-pass TeX scheme (pretolerance, tolerance, emergency
stretch) decides when hyphenation is used at all."
   :align 'justified :size 9 :color '(0.35 0.4 0.47) :space-after 8))


(defun sample--form-label (flow label x y &optional right-align)
  "Draw a small form LABEL with its baseline at (X Y) (PDF coords)."
  (niori-pdf-flow--with-graphics flow
    (niori-pdf-flow--emitf flow "BT\n")
    (niori-pdf-flow--emitf flow "%s %s %s rg\n" 0.3 0.34 0.4)
    (niori-pdf-flow--emitf flow "/%s 8.5 Tf\n"
                           (niori-pdf-font-resource
                            (niori-pdf-get-font (niori-pdf-flow-doc flow)
                                                "Helvetica")))
    (let ((xx (if right-align
                  (- x (niori-pdf-text-width label "Helvetica" 8.5))
                x)))
      (niori-pdf-flow--emitf flow "1 0 0 1 %s %s Tm\n"
                             (niori-pdf-num xx) (niori-pdf-num y)))
    (niori-pdf-flow--emit
     flow (niori-pdf--encoded-tj-operator
           (niori-pdf-font-encode label
                                  (niori-pdf-get-font
                                   (niori-pdf-flow-doc flow) "Helvetica"))
           (niori-pdf-get-font (niori-pdf-flow-doc flow) "Helvetica")))
    (niori-pdf-flow--emitf flow "ET\n")))


(defun sample-form-section ()
  "Section 10: interactive AcroForm (niori-pdf-acroforms)."
  (niori-pdf-flow-heading "10.  Interactive AcroForm" :level 1)
  (niori-pdf-flow-paragraph
   "The acroforms module builds a complete interactive form: every
field is a widget annotation with a real appearance stream (so the
initial values render everywhere) plus /DA strings, /MK colors and
the document-level /AcroForm dictionary with /DR resources.  The
form below declares a text field, a multiline text field, a
checkbox, a radio group, a dropdown choice and a URI push button -
all created at absolute page coordinates while the flow engine
reserves the vertical space."
   :align 'justified :space-after 10)
  (let* ((flow niori-pdf--flow)
         (doc (niori-pdf-flow-doc flow))
         (row-h 26)
         (label-x (niori-pdf-flow-x0 flow))
         (field-x (+ label-x 90))
         (field-w 240)
         (form-h (* 7 row-h)))
    ;; keep the whole form on one page, then place it at the cursor
    (niori-pdf-flow--ensure-space flow form-h)
    (let ((pdf-y (lambda (row h)
                   ;; PDF y of the BOTTOM of a field in ROW (0-based),
                   ;; H points tall, with a 4pt gap under the row
                   (- (niori-pdf-flow-page-h flow)
                      (+ (niori-pdf-flow-y flow)
                         (* row row-h) h 4)))))
      (niori-pdf-acroforms-enable doc)
      (niori-pdf-flow--with-graphics flow
        ;; row 0: name
        (sample--form-label flow "Name:" (+ label-x 62)
                            (+ (funcall pdf-y 0 16) 5) t)
        (niori-pdf-acroforms-text-field
         doc "name" field-x (funcall pdf-y 0 16) field-w 16
         :value "Noria Q. Hacker" :tooltip "Your full name"
         :bg '(0.97 0.98 1.0) :bc '(0.55 0.6 0.7))
        ;; row 1: email
        (sample--form-label flow "Email:" (+ label-x 62)
                            (+ (funcall pdf-y 1 16) 5) t)
        (niori-pdf-acroforms-text-field
         doc "email" field-x (funcall pdf-y 1 16) field-w 16
         :value "noria@niori.example.com" :tooltip "Work email"
         :bg '(0.97 0.98 1.0) :bc '(0.55 0.6 0.7))
        ;; rows 2-3: multiline comments
        (sample--form-label flow "Comments:" (+ label-x 62)
                            (+ (funcall pdf-y 2 44) 5) t)
        (niori-pdf-acroforms-text-field
         doc "comments" field-x (funcall pdf-y 2 44) field-w 44
         :value "Generated inside Emacs - no external tool was\ninvolved in producing this document."
         :tooltip "Anything worth noting" :multiline t
         :bg '(0.97 0.98 1.0) :bc '(0.55 0.6 0.7))
        ;; row 4: checkbox
        (sample--form-label flow "Report:" (+ label-x 62)
                            (+ (funcall pdf-y 4 14) 4.5) t)
        (niori-pdf-acroforms-checkbox
         doc "subscribe" field-x (funcall pdf-y 4 14) 14 14
         :checked t :tooltip "Subscribe to the quarterly report")
        (sample--form-label flow "subscribe to the quarterly report"
                            (+ field-x 22) (+ (funcall pdf-y 4 14) 4.5))
        ;; row 5: radio group
        (sample--form-label flow "Plan:" (+ label-x 62)
                            (+ (funcall pdf-y 5 14) 4.5) t)
        (niori-pdf-acroforms-radio-group
         doc "plan"
         (list (list field-x (funcall pdf-y 5 14) 14 "basic")
               (list (+ field-x 120) (funcall pdf-y 5 14) 14 "pro")
               (list (+ field-x 240) (funcall pdf-y 5 14) 14 "enterprise"))
         :value "pro" :tooltip "Subscription plan")
        (sample--form-label flow "basic" (+ field-x 20)
                            (+ (funcall pdf-y 5 14) 4.5))
        (sample--form-label flow "pro" (+ field-x 140)
                            (+ (funcall pdf-y 5 14) 4.5))
        (sample--form-label flow "enterprise" (+ field-x 260)
                            (+ (funcall pdf-y 5 14) 4.5))
        ;; row 6: choice + push button
        (sample--form-label flow "Country:" (+ label-x 62)
                            (+ (funcall pdf-y 6 16) 5) t)
        (niori-pdf-acroforms-choice
         doc "country" field-x (funcall pdf-y 6 16) 150 16
         (list "Spain" "Mexico" "Argentina" "Germany" "Other")
         :value "Spain" :tooltip "Country of residence"
         :bg '(0.97 0.98 1.0) :bc '(0.55 0.6 0.7))
        (niori-pdf-acroforms-push-button
         doc "download" (+ field-x 170) (funcall pdf-y 6 16) 90 16
         :caption "Get the report" :tooltip "Open the download page"
         :uri "https://niori.example.com/download"
         :bg '(0.13 0.25 0.51) :bc '(0.08 0.16 0.34)
         :fg '(1 1 1)))
      (niori-pdf-flow--advance flow form-h)
      (niori-pdf-flow-spacer 8)
      (niori-pdf-flow-paragraph
       "Try it in a viewer: the fields are editable, the radio group
keeps one selection, the dropdown opens the option list and the
button opens the URI action.  niori-pdf-acroforms-set-value can
update any field's value and appearance stream before the document
is written."
       :align 'justified :size 9 :color '(0.35 0.4 0.47) :space-after 8))))

(defun sample-flow-features-section ()
  "Section 11: lists, run decorations, floats, table spans and
cross-references -- the cl-typesetting feature set."
  (niori-pdf-flow-heading "11.  Lists, floats, spans and references" :level 1)
  (niori-pdf-flow-paragraph
   "This section exercises the block API that the earlier sections did
not use: bulleted and numbered lists with hanging indents, floats
with text wrap-around, table cells that span rows and columns, and
multi-pass cross-references."
   :align 'justified :space-after 8)
  ;; ---- lists
  (niori-pdf-flow-itemize
   (list (list "Bulleted lists (" (list "niori-pdf-flow-itemize"
                                        :font "Courier")
                ") keep a hanging indent across wrapped lines.")
         "Markers can be any string; the bullet glyph comes from the
item font."
         (list "Items may be styled runs, like this "
               (list "blue tail" :color '(0.13 0.25 0.51)) "."))
   :bullet "\u2022" :indent 14)
  (niori-pdf-flow-enumerate
   '("Numbered lists right-align their markers in a common column."
     "Numbers start at 1 by default; :start and :format customize them."
     "Both list forms are built on the same justified paragraphs.")
   :format "%d.")
  ;; ---- float + wrapping text
  (niori-pdf-flow-spacer 4)
  (niori-pdf-flow-float
   (lambda ()
     (niori-pdf-flow-box
      (lambda ()
        (niori-pdf-flow-paragraph "WRAP\nDEMO" :size 13 :align 'center
                                  :font "Helvetica-Bold"
                                  :color '(0.13 0.25 0.51)
                                  :space-after 4)
        (niori-pdf-flow-paragraph "a floated card" :size 8 :align 'center
                                  :color '(0.45 0.5 0.58)))
      :background '(0.967 0.973 0.984)
      :border '(0.62 0.72 0.86)
      :width 110 :padding 8))
   :width 110 :side 'left :gap 10)
  (niori-pdf-flow-paragraph
   "The card on the left is a float: the paragraph lines beside it are
shortened to the space left over, and once the float ends the text
returns to the full measure.  Floats are page-local: a page break
drops them, and only paragraphs and rules flow around them."
   :align 'justified :space-after 10)
  ;; ---- table with spans
  (niori-pdf-flow-heading "11.1  Spanned table cells" :level 2
                          :toc nil :bookmark nil)
  (niori-pdf-flow-table
   (list :header ["Release" "Area" "State"]
         :rows
         (list
          (list (list :text "niori-pdf 0.1" :rowspan 2
                      :background '(0.93 0.95 0.99))
                "writer, fonts, images" "shipped")
          (list "flow engine, TOC" "shipped")
          (list (list :text "this release" :colspan 2 :align 'center
                      :background '(1 0.95 0.85))
                (list :text "new" :align 'center
                      :background '(1 0.95 0.85)))
          (list (list :text "planned" :rowspan 2 :valign 'center)
                "GSUB/GPOS shaping" "roadmap")
          (list "vertical justification everywhere" "roadmap"))
         :cols '(120 190 90)
         :borders '(0.86 0.88 0.91)
         :padding 5
         :space-after 10))
  (niori-pdf-flow-paragraph
   "The release column spans two rows each (with a background or a
centered vertical alignment), and the \"this release\" row is a
colspan across the area column.  A row given as a plist can carry
:keep-with-next so it never strands above a page break."
   :align 'justified :size 9 :color '(0.35 0.4 0.47) :space-after 8)
  ;; ---- cross references
  (niori-pdf-flow-heading "11.2  Cross references" :level 2
                          :toc nil :bookmark nil)
  (niori-pdf-flow-paragraph
   (list "Reference points survive the multi-pass layout: the quarterly
figures table of section 2 sits on page "
         (format "%s" (or (niori-pdf-flow-ref-page-number
                           'quarterly-figures)
                          "?"))
         ", and the value at the right of the next line is resolved the
same way.")
   :align 'justified :space-after 6)
  (niori-pdf-flow-put-ref-page-number 'quarterly-figures
                                      :size 10
                                      :color '(0.13 0.25 0.51))
  (niori-pdf-flow-paragraph
   "On the first layout pass the number is unknown (an em dash is
printed); `niori-pdf-flow-generate' re-runs the builder until every
reference and the table of contents are stable, exactly like
cl-typesetting's multi-pass `draw-pages'."
   :align 'justified :size 9 :color '(0.35 0.4 0.47)))

(defun sample-report ()
  (niori-pdf-flow-generate
   (lambda ()
     (niori-pdf-with-flow-document
         (:title "niori quarterly business report"
                 :author "niori.el sample"
                 :hyphenate sample-hyphenate
                 :pdfa sample-pdfa
                 :footer-fn #'niori-pdf-flow-page-footer)
       ;; ----- title block
       (niori-pdf-flow-spacer 60)
       (niori-pdf-flow-heading "niori quarterly business report" :level 1
                               :size 26 :toc nil :space-before 0)
       (niori-pdf-flow-paragraph
        "Prepared entirely in Emacs Lisp with niori-pdf, the Elisp port of cl-pdf.
No LaTeX, no Ghostscript, no external processes."
        :size 11 :color '(0.42 0.47 0.55) :space-after 18)
       (niori-pdf-flow-rule :thickness 1.4
                            :color '(0.13 0.25 0.51) :space-after 14)
       ;; ----- KPI row
       (niori-pdf-flow-columns
        (list (lambda () (sample-kpi-box "REVENUE Q4" "156 k" "+20.3% vs Q3"))
              (lambda () (sample-kpi-box "ACTIVE USERS" "2,481" "+8.1% vs Q3"))
              (lambda () (sample-kpi-box "CHURN" "1.9%" "-0.4 pts vs Q3")))
        :widths '(172 172 123.28))
       (niori-pdf-flow-spacer 16)
       ;; ----- TOC
       (niori-pdf-flow-toc)
       (niori-pdf-flow-page-break)
       ;; ----- section 1
       (niori-pdf-flow-heading "1.  Executive summary" :level 1)
       (let ((fn1 (niori-pdf-flow-footnote
                   "External tools (pdfinfo, pdftotext, a rasterizer) are
used only to inspect the finished file during automated testing -
never to produce any part of it.")))
         (niori-pdf-flow-paragraph
          (list "This report is generated by niori.el using the "
                (list "niori-pdf library" :underline t)
                ", a pure Emacs Lisp port of Marc Battyani's cl-pdf.  Every
element you see here, from the justified text to the vector chart
and the embedded image, was produced by Emacs without invoking any
external program"
                (list fn1 :superscript t)
                ".  The output is a standard PDF file with a classic
cross-reference table that every viewer can read"
                (if sample-pdfa
                    " - this archival variant is PDF/A-2b: PDF 1.7 with an
/ID trailer, XMP metadata and an sRGB OutputIntent, and every font
embedded."
                  " (PDF 1.4)."))
          :align 'justified :space-after 8))
       (niori-pdf-flow-paragraph
        (list "Content streams are compressed with a DEFLATE implementation
written in Elisp, because Emacs ships zlib decompression only.  Base-14
fonts are drawn with the original Adobe AFM metrics, and arbitrary
TrueType fonts are subsetted and embedded as CIDFontType2 with a
ToUnicode map, so text extraction keeps working in every viewer.  The
typographic details matter too: "
              (list "real superscripts" :color '(0.13 0.25 0.51)
                    :superscript t)
              " like footnote marks, "
              (list "strikethrough" :strike t)
              " revisions and "
              (list "highlighted findings" :background '(1 0.92 0.62))
              " are all first-class citizens.")
        :align 'justified :space-after 10)
       ;; ----- section 2 with table (a reference point for section 11)
       (niori-pdf-flow-heading "2.  Quarterly figures" :level 1)
       (niori-pdf-flow-mark-ref-point 'quarterly-figures)
       (niori-pdf-flow-paragraph
        "The table below lists the quarterly figures by product line.  Long
tables break across pages automatically and repeat their header row."
        :align 'justified :space-after 8)
       (niori-pdf-flow-table
        (list :header ["Product line" "Q1" "Q2" "Q3" "Q4" "Share"]
              :rows (list
                     (list "Core platform" "62" "68" "71" "79" "34.1%")
                     (list "Add-on modules" "34" "38" "36" "44" "19.0%")
                     (list "Support contracts" "18" "21" "19" "23" "9.9%")
                     (list "Training" "4" "7" "3" "10" "4.3%")
                     (list "Total" "118" "134" "129" "156" "67.3%"))
              :cols '(120 50 50 50 50 60)
              :align '(left right right right right right)
              :borders '(0.86 0.88 0.91)
              :padding 5))
       (niori-pdf-flow-spacer 14)
       ;; ----- section 3: chart module
       (sample-charts-section)
       ;; ----- section 4 with image
       (niori-pdf-flow-heading "4.  Embedded raster image" :level 1)
       (niori-pdf-flow-paragraph
        "PNG images are decoded inside Emacs (row un-filtering included) and
embedded with an alpha SMask; JPEG files pass through untouched as
DCTDecode streams.  The chart below is a PNG produced by matplotlib,
embedded by niori-pdf."
        :align 'justified :space-after 8)
       (niori-pdf-flow-image
        (concat (file-name-directory load-file-name) "chart.png")
        :width 380 :caption "Figure 1: quarterly revenue (embedded PNG)")
       (niori-pdf-flow-spacer 6)
       ;; ----- section 5: curves
       (sample-curves-section)
       ;; ----- section 6: bar codes
       (sample-barcode-section)
       ;; ----- section 7: dingbats + symbol
       (sample-dingbats-section)
       ;; ----- section 8: embedded fonts
       (sample-embedded-fonts-section)
       ;; ----- section 9: hyphenation
       (sample-hyphenation-section)
       ;; ----- section 10: interactive AcroForm (skipped in the
       ;; PDF/A variant: widget appearance streams need single-byte
       ;; fonts, which conflicts with the embed-everything rule)
       (if sample-pdfa
           (progn
             (niori-pdf-flow-heading "10.  Interactive AcroForm" :level 1)
             (niori-pdf-flow-paragraph
              "The interactive AcroForm demo is omitted from this PDF/A
variant: form widget appearance streams need single-byte fonts, and
ISO 19005 requires every font to be embedded, which the form fields
of the non-archival build draw from the unembedded base-14 Helvetica.
See the standard variant for a complete interactive form (text
fields, checkbox, radio group, dropdown and URI button)."
              :align 'justified :space-after 8))
         (sample-form-section))
       ;; ----- section 11: lists, floats, spans, cross-references
       (sample-flow-features-section)
       ;; ----- section 12
       (niori-pdf-flow-heading "12.  Outlook" :level 1)
       (niori-pdf-flow-paragraph
        (concat "The niori export pipeline now writes reports directly from live
application data.  This version already covers the features the
evaluation identified as next steps: AFM kerning pairs, Symbol and
ZapfDingbats native text, CFF/OTF embedding, TrueType collections,
Type 1 font embedding and PDF/A-2b archival output"
                (if sample-pdfa
                    " (this build is the archival variant), document
outlines, interactive form fields, bar codes, QR codes and charts -
plus Knuth-Plass justification"
                  " (the archival variant is generated with the
--pdfa flag), document outlines, interactive form fields, bar codes,
QR codes and charts - plus Knuth-Plass justification")
                (if sample-hyphenate
                    " with TeX hyphenation in ten
languages, used by every paragraph above."
                  ".  This variant is the --no-hyphenation comparison
build; the default report sets every paragraph with TeX hyphenation
in ten languages."))
        :align 'justified :space-after 8)
       (niori-pdf-flow-rule :space-before 10 :space-after 6)
       (niori-pdf-flow-paragraph
        "Generated with niori-pdf 0.1.0 (port of cl-pdf) inside GNU Emacs."
        :size 8 :color '(0.5 0.5 0.55) :align 'center)))))

(let ((doc (sample-report)))
  (niori-pdf-flow-save doc (concat "/tmp/niori-test/" sample-output-file))
  (princ (format "%s written: %d pages, %d objects (hyphenation: %s, pdfa: %s)\n"
                 sample-output-file
                 (length (niori-pdf-document-pages doc))
                 (niori-pdf-document-n-objects doc)
                 (if sample-hyphenate "on" "off")
                 (if (niori-pdf-document-pdfa-level doc) "2b" "no"))))
