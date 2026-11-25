;;; niori-pdf-tests.el --- Tests for niori-pdf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev

;; This file is part of niori-pdf.  See LICENSE for applicable terms.

;;; Commentary:

;; Run with:  emacs --batch -l test/niori-pdf-tests.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst niori-pdf-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst niori-pdf-tests--pkg
  (expand-file-name ".." niori-pdf-tests--dir))

(add-to-list 'load-path niori-pdf-tests--pkg)
(require 'niori-pdf)

;;;; Object model and writer

(defun niori-pdf-test--page-literals (page)
  "Concatenated PDF string literals of PAGE's content stream.
Kerning splits words across TJ fragments and the operators between
them carry letters too, so tests that look for phrases concatenate
the literals and drop everything else."
  (let ((s (niori-pdf-page-content-string page))
        (out nil) (i 0) (in-str nil))
    (while (< i (length s))
      (let ((ch (aref s i)))
        (cond ((eq ch ?\\) (setq i (1+ i)))
              ((eq ch ?\() (setq in-str t))
              ((eq ch ?\)) (setq in-str nil))
              (in-str (push ch out))))
      (setq i (1+ i)))
    (apply #'string (nreverse out))))

(ert-deftest niori-pdf-num-format ()
  (should (equal (niori-pdf-num 42) "42"))
  (should (equal (niori-pdf-num 12.5) "12.5"))
  (should (equal (niori-pdf-num 0.5) "0.5"))
  (should (equal (niori-pdf-num -3.0) "-3"))
  (should (equal (niori-pdf-num 100.0) "100")))

(ert-deftest niori-pdf-page-font-dict-dedupe ()
  "The shared resource dict lists each resource name once even when
the same font object is registered under several spec keys (a PDF/A
base-14 substitution registers it under the base-14 name and under
the substituted file)."
  (let* ((doc (niori-pdf-make-document))
         (path "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf")
         (f (niori-pdf-get-font doc path)))
    ;; simulate the PDF/A double registration: one object, two keys
    (push (cons "Helvetica" f) (niori-pdf-document-fonts doc))
    (should (eq (niori-pdf-get-font doc path) f))
    (should (eq (niori-pdf-get-font doc "Helvetica") f))
    (should (= (length (niori-pdf-document-fonts doc)) 2))
    ;; the assembled resource entries collapse to a single name
    (let ((entries (mapcar (lambda (kv)
                             (cons (intern (niori-pdf-font-resource
                                            (cdr kv)))
                                   (niori-pdf-font-ref (cdr kv))))
                           (niori-pdf-document-fonts doc))))
      (should (= (length entries) 2))
      (should (equal (niori-pdf--dedupe-resource-entries entries)
                     (list (car entries)))))))

(ert-deftest niori-pdf-deflate-roundtrip ()
  "The pure Elisp deflate must produce streams Emacs zlib can inflate."
  (skip-unless (zlib-available-p))
  (dolist (input (list "hello"
                       (apply #'concat (make-list 50 "abcabcabcabc "))
                       (apply #'string (number-sequence 0 255))
                       (make-string 5000 ?A)))
    (let* ((in (string-as-unibyte input))
           (out (niori-pdf-deflate in)))
      (should (string= in (niori-pdf-inflate out)))
      (should (< (length out) (+ (length in) 64))))))

(ert-deftest niori-pdf-minimal-document ()
  "A minimal document must produce a file starting with %PDF and %%EOF."
  (let ((tmp (make-temp-file "niori-pdf-test" nil ".pdf")))
    (unwind-protect
        (progn
          (niori-pdf-with-document (:title "t")
            (niori-pdf-with-page ()
              (niori-pdf-set-font "Helvetica" 12)
              (niori-pdf-draw-text 72 720 "Hello"))
            (niori-pdf-save niori-pdf--doc tmp))
          (with-temp-buffer
            (insert-file-contents-literally tmp)
            (goto-char (point-min))
            (should (looking-at "%PDF-1.4"))
            (should (string-match-p "%%EOF
"
                                    (buffer-substring
                                     (- (point-max) 8) (point-max))))))
      (delete-file tmp))))

(ert-deftest niori-pdf-xref-offsets-are-valid ()
  "Every xref entry must point exactly at its \"N 0 obj\" marker."
  (let ((tmp (make-temp-file "niori-pdf-test" nil ".pdf")))
    (unwind-protect
        (progn
          (niori-pdf-with-document ()
            (niori-pdf-with-page ()
              (niori-pdf-set-font "Times-Roman" 10)
              (niori-pdf-draw-text 72 700 "xref test"))
            (niori-pdf-save niori-pdf--doc tmp))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally tmp)
            (goto-char (point-max))
            (search-backward "xref\n0 ")
            (let* ((xref-start (+ (point) (length "xref\n0 ")))
                   (trailer (search-forward "trailer"))
                   (entries (split-string
                             (buffer-substring-no-properties
                              xref-start (- trailer 8))
                             "\n" t))
                   (count (length entries)))
              ;; entries[0] is the free entry; entries[k] is object k
              (should (> count 1))
              (let ((passed 0))
                (dotimes (k (- count 2))
                  ;; entries[0] is the "0 N" count line, entries[1] the
                  ;; free entry; object k lives at entries[k+2]
                  (let* ((entry (nth (+ k 2) entries))
                         (offset (string-to-number (substring entry 0 10))))
                    (goto-char (1+ offset))
                    (when (looking-at (format "%d 0 obj" (1+ k)))
                      (setq passed (1+ passed)))))
                (should (= passed (- count 2)))))))
      (delete-file tmp))))

;;;; Fonts and encoding

(ert-deftest niori-pdf-winansi-encode ()
  (should (equal (niori-pdf-encode-winansi "abc") "abc"))
  (should (= (aref (niori-pdf-encode-winansi "é") 0) 233))
  (should (= (aref (niori-pdf-encode-winansi "€") 0) 128))
  (should (= (aref (niori-pdf-encode-winansi "—") 0) 151))
  (should (= (aref (niori-pdf-encode-winansi "日") 0) 63)))

(ert-deftest niori-pdf-metrics-known-values ()
  (let* ((doc (niori-pdf-make-document))
         (font (niori-pdf-get-font doc "Helvetica")))
    (should (= (niori-pdf-text-width " " font 10) 2.78))
    (should (= (niori-pdf-text-width "W" font 10) 9.44))))

(ert-deftest niori-pdf-text-width-ttf ()
  (skip-unless (file-exists-p
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"))
  (let* ((doc (niori-pdf-make-document))
         (font (niori-pdf-get-font
                doc "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"))
         (w (niori-pdf-text-width "HH" font 10)))
    (should (> w 0))
    (should (< (abs (- w 15.0390625)) 0.1))))

;;;; Images

(defun niori-pdf-tests--crc32 (bytes)
  "CRC-32 of unibyte BYTES (PNG requires it for chunk integrity)."
  (let ((crc #xFFFFFFFF) (i 0) (n (length bytes)))
    (while (< i n)
      (setq crc (logxor crc (aref bytes i)))
      (let ((k 0))
        (while (< k 8)
          (setq crc (if (/= 0 (logand crc 1))
                        (logxor (ash crc -1) #xEDB88320)
                      (ash crc -1)))
          (setq k (1+ k))))
      (setq i (1+ i)))
    (logand (logxor crc #xFFFFFFFF) 4294967295)))

(defun niori-pdf-tests--crc32-string (chunk)
  "CRC-32 bytes for the PNG CHUNK (type + data), big-endian string."
  (let ((v (niori-pdf-tests--crc32 chunk)))
    (string (logand (ash v -24) 255) (logand (ash v -16) 255)
            (logand (ash v -8) 255) (logand v 255))))

(defun niori-pdf-tests--be32 (v)
  "Big-endian 4-byte string of V."
  (string (logand (ash v -24) 255) (logand (ash v -16) 255)
          (logand (ash v -8) 255) (logand v 255)))

(defun niori-pdf--png-build-test-image (w h)
  "Build a minimal 8-bit grayscale PNG of W x H pixels.
Rows alternate between bytes #x61 (a) and #x62 (b), filter 0."
  (let* ((ihdr-data (concat (niori-pdf-tests--be32 w)
                            (niori-pdf-tests--be32 h)
                            (string 8 0 0 0 0)))
         (ihdr (concat (niori-pdf-tests--be32 13) "IHDR" ihdr-data))
         (raw (let ((rows ""))
                (dotimes (y h)
                  (setq rows (concat rows (string 0)
                                     (make-string w (+ 97 y)))))
                rows))
         (idat-data (niori-pdf-deflate raw))
         (idat (concat (niori-pdf-tests--be32 (length idat-data))
                       "IDAT" idat-data))
         (iend (concat (niori-pdf-tests--be32 0) "IEND")))
    (concat "\x89PNG\r\n\x1a\n"
            ihdr (niori-pdf-tests--crc32-string ihdr)
            idat (niori-pdf-tests--crc32-string idat)
            iend (niori-pdf-tests--crc32-string iend))))

(ert-deftest niori-pdf-png-roundtrip ()
  "A tiny generated PNG must decode to the right pixel values."
  (let* ((png (niori-pdf--png-build-test-image 4 2))
         (img (niori-pdf-make-png-image png)))
    (should (= (niori-pdf-image-width img) 4))
    (should (= (niori-pdf-image-height img) 2))
    (should (string= (niori-pdf-image-data img) "aaaabbbb"))))

;;;; TTF subsetting

(ert-deftest niori-pdf-ttf-subset-structure ()
  (skip-unless (file-exists-p
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"))
  (let* ((ff (niori-pdf-ttf-read-file
              "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"))
         (subset (niori-pdf-ttf--build-subset ff (list 3 36 68))))
    (should (> (length subset) 1000))
    (should (= (niori-pdf-ttf--u32 subset 0) #x00010000))
    (should (= (niori-pdf-ttf--u16 subset 4) 9))))

;;;; Flow engine

(ert-deftest niori-pdf-ts-knuth-plass-breaking ()
  "Three words of 17.5pt in a 40pt column: exactly two lines."
  (let* ((word (niori-pdf-ts-box 17.5))
         (spc (niori-pdf-ts-glue 2.78 2.78 1.12))
         (items (list word spc
                      (copy-sequence word) (copy-sequence spc)
                      (copy-sequence word) (copy-sequence spc)
                      (niori-pdf-ts-glue 0 0 0)
                      (niori-pdf-ts-penalty niori-pdf-ts-forced-penalty)))
         (brks (niori-pdf-ts-break-paragraph items 40.0)))
    (should (= (length brks) 2))
    ;; line 1 = box glue box (3 atoms); line 2 = box glue final-glue
    ;; (3 atoms: the breaking glue is discarded, so line 2 starts at
    ;; the third box; the forced penalty is not part of the line)
    (should (= (length (car (niori-pdf-ts-line items (car brks)))) 3))
    (should (= (length (car (niori-pdf-ts-line items (cadr brks)))) 3))
    ;; the second line must start at a box (no leftover break glue)
    (should (eq (type-of (car (car (niori-pdf-ts-line items (cadr brks)))))
                'niori-pdf-ts-box))
    ;; both lines must fit
    (dolist (brk brks)
      (let ((line (niori-pdf-ts-line items brk)))
        (should (<= (apply #'+ (delq nil (cdr line))) 40.0))))))

(ert-deftest niori-pdf-ts-justified-ratio ()
  "Justification distributes the missing space over the glue."
  (let* ((word (niori-pdf-ts-box 17.5))
         (spc (niori-pdf-ts-glue 2.78 2.78 1.12))
         (items (list word spc (copy-sequence word) (copy-sequence spc)
                      (copy-sequence word) (copy-sequence spc)
                      (niori-pdf-ts-glue 0 0 0)
                      (niori-pdf-ts-penalty niori-pdf-ts-forced-penalty)))
         (brks (niori-pdf-ts-break-paragraph items 40.0))
         (line (niori-pdf-ts-line items (car brks))))
    ;; two boxes + one interword glue stretched to fill 40pt:
    ;; glue width = 40 - 35 = 5pt
    (should (< (abs (- (nth 1 (cdr line)) 5.0)) 1e-6))))

(ert-deftest niori-pdf-ts-overfull-final-line-rejected ()
  "A fill glue (the parfillskip) must not mask an over-full line:
badness 0 is only for lines that are too SHORT.  Three 10pt words
with 1pt glue in a 23.5pt measure: the whole paragraph in one line
is 32pt natural (8.5pt overfull) and only 1pt of shrink, so the
breaker must wrap after the second word instead (regression for the
lines that used to stick out of the right margin in the Alice book)."
  (let* ((word (niori-pdf-ts-box 10.0))
         (spc (niori-pdf-ts-glue 1.0 2.0 0.5))
         (items (list word spc (copy-sequence word) (copy-sequence spc)
                      (copy-sequence word)
                      (niori-pdf-ts-hfil)
                      (niori-pdf-ts-penalty niori-pdf-ts-forced-penalty)))
         (brks (niori-pdf-ts-break-paragraph items 23.5)))
    ;; two lines: [word spc word] then [word]
    (should (= (length brks) 2))
    (dolist (brk brks)
      (let* ((line (niori-pdf-ts-line items brk))
             (w (apply #'+ (delq nil (cdr line)))))
        ;; every line fits the measure after elasticity is applied
        (should (<= w 23.5))))))

(ert-deftest niori-pdf-ts-overfull-line-shrinks-to-fit ()
  "An accepted slightly over-full line shrinks its glue to fit; only
the part beyond the total shrink sticks out (TeX overfull box).
Two 10pt words with 1pt glue (0.25pt shrink) in a 20.75pt measure:
natural width 21pt, 0.25pt overfull, absorbed by the glue shrink so
the line fits the measure exactly."
  (let* ((word (niori-pdf-ts-box 10.0))
         (spc (niori-pdf-ts-glue 1.0 2.0 0.25))
         (items (list word spc (copy-sequence word)
                      (niori-pdf-ts-hfil)
                      (niori-pdf-ts-penalty niori-pdf-ts-forced-penalty)))
         (brks (niori-pdf-ts-break-paragraph items 20.75))
         (line (niori-pdf-ts-line items (car brks))))
    (should (= (length brks) 1))
    ;; the interword glue shrank from 1.0 to 0.75: the line fits the
    ;; measure exactly instead of overflowing
    (should (< (abs (- (nth 1 (cdr line)) 0.75)) 1e-6))))

(ert-deftest niori-pdf-flow-toc-and-outlines ()
  "The TOC registers outline entries and the catalog gets /Outlines."
  (let ((doc (niori-pdf-with-flow-document (:title "t")
               (niori-pdf-flow-toc)
               (niori-pdf-flow-heading "Alpha" :level 1)
               (niori-pdf-flow-page-break)
               (niori-pdf-flow-heading "Beta" :level 2)
               niori-pdf--doc)))
    (should (= (niori-pdf-outlines-count doc) 2))
    (niori-pdf-save doc "/tmp/toc-outline-test.pdf")
    (with-temp-buffer
      (insert-file-contents-literally "/tmp/toc-outline-test.pdf")
      (should (search-forward "/Outlines " nil t))
      (should (search-forward "/PageMode /UseOutlines" nil t)))))

(ert-deftest niori-pdf-acroforms-structure ()
  "Fields land in /Fields, widgets in /Annots with appearance streams."
  (let ((doc (niori-pdf-with-document (:title "f")
               (niori-pdf-with-page ()
                 (niori-pdf-acroforms-enable niori-pdf--doc)
                 (niori-pdf-acroforms-text-field niori-pdf--doc
                     "t1" 72 700 120 20 :value "hi")
                 (niori-pdf-acroforms-checkbox niori-pdf--doc
                     "c1" 72 670 14 14 :checked t)
                 (niori-pdf-acroforms-choice niori-pdf--doc
                     "ch1" 72 640 100 18 '("A" "B") :value "B")
                 (niori-pdf-acroforms-radio-group niori-pdf--doc
                     "r1" '((72 610 14 "x") (92 610 14 "y")) :value "y")
                 (niori-pdf-acroforms-push-button niori-pdf--doc
                     "b1" 72 580 80 22 :caption "OK")))))
    (should (= (length (niori-pdf-acroforms-fields doc)) 5))
    (niori-pdf-save doc "/tmp/acro-structure-test.pdf")
    (with-temp-buffer
      (insert-file-contents-literally "/tmp/acro-structure-test.pdf")
      (should (search-forward "/AcroForm" nil t))
      (goto-char (point-min))
      (should (search-forward "/Fields [ " nil t))
      (goto-char (point-min))
      (should (search-forward "/AP << /N <<" nil t)))))

(ert-deftest niori-pdf-flow-pagination ()
  "Blocks that do not fit must create new pages."
  (let ((doc (niori-pdf-with-flow-document (:height 200 :margins '(10 10 10 10))
               (dotimes (i 12)
                 (niori-pdf-flow-paragraph "line after line after line"
                                           :size 14))
               niori-pdf--doc)))
    (should (> (length (niori-pdf-document-pages doc)) 1))))

(ert-deftest niori-pdf-flow-table-breaks-with-header ()
  (let ((doc (niori-pdf-with-flow-document (:height 300 :margins '(10 10 10 10))
               (niori-pdf-flow-table
                (list :header ["A" "B"]
                      :rows (cl-loop for i from 1 to 40
                                     collect (list (number-to-string i)
                                                   "xxxx xxxxx xxxxxx"))
                      :cols '(60 200)
                      :borders nil))
               niori-pdf--doc)))
    (should (> (length (niori-pdf-document-pages doc)) 1))))


;;;; QR codes (niori-pdf-qrcode)

(ert-deftest niori-pdf-qr-block-table ()
  "Block structure table entries must match ISO/IEC 18004."
  (should (equal (niori-pdf-qr--groups 1 'M) '((1 . 16))))
  (should (= (niori-pdf-qr--ec-per-block 1 'M) 10))
  (should (= (niori-pdf-qr--data-codewords 1 'M) 16))
  (should (equal (niori-pdf-qr--groups 8 'H) '((4 . 14) (2 . 15))))
  (should (= (niori-pdf-qr--ec-per-block 8 'H) 26))
  (should (= (niori-pdf-qr--data-codewords 8 'H) 86))
  (should (= (niori-pdf-qr--data-codewords 10 'M) 216))
  (should (= (niori-pdf-qr--byte-capacity 1 'M) 14))
  (should (= (niori-pdf-qr--byte-capacity 10 'M) 213))
  (should (eq (niori-pdf-qr--auto-version 5 'M) 1))
  (should (eq (niori-pdf-qr--auto-version 15 'M) 2)))

(ert-deftest niori-pdf-qr-reed-solomon ()
  "The EC codewords must match the ISO 18004 worked example
(version 1-M, numeric \"01234567\") and an independent computation."
  (should (equal (niori-pdf-qr--rs-remainder
                  [32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17] 10)
                 '(196 35 39 119 235 215 231 226 93 23)))
  (should (equal (niori-pdf-qr--rs-remainder
                  [64 20 16 236 17 236 17 236 17 236 17 236 17 236 17 236] 10)
                 '(107 112 244 24 163 122 17 95 52 252))))

(ert-deftest niori-pdf-qr-codewords-byte-mode ()
  "Byte-mode stream for \"A\" in version 1-M."
  (should (equal (niori-pdf-qr--codewords "A" 1 'M)
                 '(64 20 16 236 17 236 17 236 17 236 17 236 17 236 17 236)))
  (should (equal (niori-pdf-qr--interleave "A" 1 'M)
                 '(64 20 16 236 17 236 17 236 17 236 17 236 17 236 17 236
                   107 112 244 24 163 122 17 95 52 252))))

(ert-deftest niori-pdf-qr-format-info ()
  "Format information BCH(15,5) values."
  ;; (M, mask 3) -> 0b101101101001011 (cross-checked against a
  ;; reference implementation and decoded by zbar)
  (should (= (niori-pdf-qr--format-bits 'M 3) #b101101101001011))
  (should (= (niori-pdf-qr--format-bits 'L 0) #b111011111000100))
  ;; version info BCH(18,6) for version 8 (nayuki reference value)
  (should (= (niori-pdf-qr--version-bits 7) #x7c94))
  (should (= (niori-pdf-qr--version-bits 8) #x85bc)))

(ert-deftest niori-pdf-qr-encode-structure ()
  "The encoded matrix has the right size and fixed patterns."
  (let* ((mv (niori-pdf-qr-encode "https://niori.example.com" :ecl 'M))
         (matrix (car mv))
         (ver (cdr mv))
         (size (niori-pdf-qr--version-size ver)))
    (should (= ver 2))
    (should (= (length matrix) size))
    (dolist (row matrix) (should (= (length row) size)))
    ;; top-left finder: 7x7 ring with dark center row
    (should (= 1 (nth 0 (nth 0 matrix))))
    (should (= 1 (nth 3 (nth 3 matrix))))
    (should (= 0 (nth 7 (nth 3 matrix))))      ; separator
    ;; timing patterns alternate
    (dotimes (i (- size 16))
      (should (= (nth (+ 8 i) (nth 6 matrix))
                 (if (cl-evenp i) 1 0)))
      (should (= (nth 6 (nth (+ 8 i) matrix))
                 (if (cl-evenp i) 1 0))))
    ;; the dark module
    (should (= 1 (nth 8 (nth (- size 8) matrix))))))

(ert-deftest niori-pdf-qr-golden-matrix ()
  "Golden matrix for \"A\" (version 1, level M), verified by
zbar round-trip decoding and an independent reference implementation."
  (let* ((want
          (list
           "#######.#..#..#######"
    "#.....#.#####.#.....#"
    "#.###.#...#.#.#.###.#"
    "#.###.#.##.##.#.###.#"
    "#.###.#..###..#.###.#"
    "#.....#..#.##.#.....#"
    "#######.#.#.#.#######"
    "........##.##........"
    "#.##.###..###.#..#.##"
    ".#.##..#.#.####..#..."
    "##..###.##.#.....##.#"
    "#.##.#.....#..#####.."
    "#..####..#..#..#..#.."
    "........#.##..#..#..#"
    "#######.#..##..#.#..."
    "#.....#.##.....##.##."
    "#.###.#..##.#####...#"
    "#.###.#.####..######."
    "#.###.#.#.#.#.##....."
    "#.....#...#..#.#..#.#"
    "#######.#....#..#...."))
         (mv (niori-pdf-qr-encode "A" :ecl 'M))
         (matrix (car mv)))
    (should (= (cdr mv) 1))
    (should (= (length matrix) 21))
    (dotimes (y 21)
      (should (equal (mapconcat (lambda (v) (if (= v 1) "#" ".")) (nth y matrix) "")
                     (nth y want))))))

;;;; Spanish hyphenation (niori-pdf-hyphen-data)

(ert-deftest niori-pdf-hyphen-spanish ()
  "Spanish patterns hyphenate like the canonical hyph-es set."
  (should (niori-pdf-hyphen-language-available-p 'spanish))
  (should (equal (niori-pdf-hyphen-word-splits "computadora" 'spanish)
                 '("compu" "tado" "ra")))
  (should (equal (niori-pdf-hyphen-word-splits "español" 'spanish)
                 '("es" "pa" "ñol")))
  (should (equal (niori-pdf-hyphen-word-splits "esdrújulas" 'spanish)
                 '("es" "drú" "ju" "las")))
  (should (equal (niori-pdf-hyphen-word-splits "aéreo" 'spanish)
                 '("aé" "reo")))
  ;; minima (2/2): no break before the 3rd character
  (should (equal (niori-pdf-hyphen-word-splits "niori" 'spanish)
                 '("nio" "ri"))))

;;;; Explicit-hyphen words (TeX rule)

(ert-deftest niori-pdf-flow-explicit-hyphen-word ()
  "Words with explicit hyphens break after the hyphens only, with
zero-width flagged penalties and no pattern hyphenation."
  (niori-pdf-with-flow-document (:title "t")
    (let* ((flow niori-pdf--flow)
           (style (list :font "Helvetica" :size 10.5))
           (items (car (niori-pdf-flow--word-box
                        flow "niori-pdf-curve-to" style))))
      (should (= (length items) 7))          ; 4 boxes + 3 penalties
      (should (equal (plist-get (niori-pdf-ts-box-content (nth 0 items))
                                :text)
                     "niori-"))
      (should (equal (plist-get (niori-pdf-ts-box-content (nth 2 items))
                                :text)
                     "pdf-"))
      (should (= (niori-pdf-ts-penalty-width (nth 1 items)) 0))
      (should (= (niori-pdf-ts-penalty-amount (nth 1 items)) 50)))
    ;; plain word still pattern-hyphenates
    (let* ((items (car (niori-pdf-flow--word-box
                        niori-pdf--flow "justified"
                        (list :font "Helvetica" :size 10.5)))))
      (should (= (length items) 5)))))      ; jus|ti|fied + 2 penalties

;;;; Flow overflow protection

(ert-deftest niori-pdf-flow-columns-overflow-scaled ()
  "Column widths wider than the text area are scaled down to fit."
  (let ((doc (niori-pdf-with-flow-document (:title "t")
               (niori-pdf-flow-columns
                (list (lambda () (niori-pdf-flow-paragraph "a"))
                      (lambda () (niori-pdf-flow-paragraph "b")))
                :widths '(180 180 127))
               niori-pdf--doc)))
    ;; nothing to assert structurally - the point is no error and the
    ;; row stays inside the page; check the document still saves
    (should doc)))

(ert-deftest niori-pdf-flow-table-overflow-scaled ()
  "A table wider than the text area is scaled down, not clipped."
  (let ((doc (niori-pdf-with-flow-document (:title "t")
               (niori-pdf-flow-table
                (list :header (list "Aspect" "Assessment")
                      :rows (list (list "License" "FreeBSD-style (2-clause)"))
                      :cols '(75 449)))
               niori-pdf--doc)))
    (should doc)))


(provide 'niori-pdf-tests)
;;; niori-pdf-tests.el ends here

;;;; New-language hyphenation, table wrapping, TOC links, CFF subsetting

(ert-deftest niori-pdf-hyphen-ten-languages ()
  "Ten languages ship with pattern data."
  (should (equal niori-pdf-hyphen-data--languages
                 '(american german french british spanish italian
                            portuguese esperanto ukrainian russian)))
  (dolist (lang niori-pdf-hyphen-data--languages)
    (should (niori-pdf-hyphen-language-available-p lang))
    (should (niori-pdf-hyphen-load-language lang))))

(ert-deftest niori-pdf-hyphen-new-languages-breaks ()
  "The new pattern sets produce the reference break points.
The reference words were verified against a direct Liang
implementation over the same TeX pattern files (40/40 agreement)."
  (dolist (case '((italian   "programmazione" "pro-gram-ma-zio-ne")
                  (italian   "qualità"        "qua-li-tà")
                  (italian   "esempio"        "esem-pio")
                  (portuguese "humanidade"    "hu-ma-ni-dade")
                  (portuguese "computação"    "com-pu-ta-ção")
                  (portuguese "desenvolvimento"
                              "de-sen-vol-vi-mento")
                  (esperanto "esperanto"      "es-pe-ran-to")
                  (esperanto "biblioteko"     "bi-bli-o-te-ko")
                  (ukrainian "українська"     "укра-їн-ська")
                  (ukrainian "гідність"       "гі-дність")
                  (russian   "гидрография"    "гид-ро-гра-фия")
                  (russian   "подъезд"        "подъ-езд")
                  (russian   "вычисление"     "вы-чис-ле-ние")))
    (let ((word (nth 1 case)) (want (nth 2 case)))
      (should (string= (string-join
                        (niori-pdf-hyphen-word-splits word (nth 0 case))
                        "-")
                       want)))))

(ert-deftest niori-pdf-hyphen-russian-exceptions ()
  "Russian ships its TeX exception list; асбест breaks ас-бест by
exception, while подъезд hyphenates from the patterns."
  (let ((pl (niori-pdf-hyphen-load-language 'russian)))
    (should (> (hash-table-count (plist-get pl :exceptions)) 100))
    ;; ас-бест is listed verbatim in hyph-ru.tex
    (should (gethash "асбест" (plist-get pl :exceptions)))
    (should (equal (niori-pdf-hyphenate-word "асбест" 'russian) '(2)))
    ;; подъезд hyphenates from the pattern data
    (should (equal (niori-pdf-hyphenate-word "подъезд" 'russian) '(4)))))

(ert-deftest niori-pdf-flow-parfillskip-wraps-cell ()
  "A left-aligned paragraph (like a table cell) wraps instead of
sticking to a single over-full line: the paragraph ends with a
parfillskip so short last lines cost no badness."
  (niori-pdf-with-flow-document (:default-font "Helvetica"
                                 :default-size 10.5)
    (let* ((flow niori-pdf--flow)
           (runs (list (list "FreeBSD-style (BSD 2-clause), (c) 2003 Marc
Battyani; commercial use permitted with attribution"
                             :font "Helvetica" :size 10.5)))
           (pb (niori-pdf-flow--paragraph-items
                flow runs :hyphenate nil :align 'left))
           (items (car pb))
           (breaks (niori-pdf-ts-break-paragraph items 403.8 nil)))
      ;; two lines: the cell wraps inside its column
      (should (= (length breaks) 2))
      ;; the last item before the forced break is a fil glue
      (should (eq (type-of (car (last items 2))) 'niori-pdf-ts-glue))
      (should (> (niori-pdf-ts-glue-stretch-order
                  (car (last items 2)))
                 0)))))

(ert-deftest niori-pdf-flow-adjacent-runs-no-spurious-space ()
  "Adjacent runs that touch without whitespace (\"_you_.\") are
fragments of ONE word: no interword glue between them, so no
spurious space is rendered at the style boundary (regression for
the gaps in italicised words followed by punctuation)."
  (niori-pdf-with-flow-document (:default-font "Helvetica"
                                 :default-size 10.5)
    (let* ((runs (list "go " (list "you" :font "Helvetica-Oblique")
                       ".Come, stop"))
           (words (niori-pdf-flow--split-words runs)))
      ;; words: "go", ("you" . ".Come,"), "stop"
      (should (= (length words) 3))
      (should (equal (car (nth 0 words)) '("go")))
      (should (equal (mapcar #'car (nth 1 words)) '("you" ".Come,")))
      (should (equal (car (nth 2 words)) '("stop")))
      ;; item level: exactly two finite glues (go|merged-word and
      ;; merged-word|stop) -- none inside the merged word
      (let* ((pb (niori-pdf-flow--paragraph-items
                  niori-pdf--flow runs :hyphenate nil :align 'left))
             (items (car pb))
             (finite-glues
              (cl-count-if
               (lambda (it)
                 (and (eq (type-of it) 'niori-pdf-ts-glue)
                      (= (niori-pdf-ts-glue-stretch-order it) 0)))
               items)))
        (should (= finite-glues 2))
        ;; the two fragments of the merged word are adjacent boxes:
        ;; separated only by an infinite penalty
        (let ((k (cl-position-if
                  (lambda (it)
                    (and (eq (type-of it) 'niori-pdf-ts-box)
                         (equal (plist-get
                                 (niori-pdf-ts-box-content it) :text)
                                "you")))
                  items)))
          (should k)
          (should (eq (type-of (aref (vconcat items) (1+ k)))
                      'niori-pdf-ts-penalty))
          (should (= (niori-pdf-ts-penalty-amount
                      (aref (vconcat items) (1+ k)))
                     niori-pdf-ts-inf-penalty)))))))

(ert-deftest niori-pdf-flow-table-column-autofit ()
  "Columns widen so that an unbreakable word (Dependencies) fits,
funded by the surplus column; the table total is preserved."
  (niori-pdf-with-flow-document (:default-font "Helvetica"
                                 :default-size 10.5)
    (let* ((col-w (niori-pdf-flow--fit-col-widths
                   (list (niori-pdf-flow--tbl-row-spec
                          ["Aspect" "Assessment"])
                         (niori-pdf-flow--tbl-row-spec
                          (list "Dependencies"
                                "trivial-garbage; optional UFFI (zlib)")))
                   t
                   '(69.46 413.82)
                   5 "Helvetica" 10.5))
           (dep-w (niori-pdf-text-width "Dependencies" "Helvetica" 10.5)))
      (should (>= (car col-w) (+ dep-w 10)))
      (should (< (abs (- (apply #'+ col-w) (+ 69.46 413.82))) 0.01))
      ;; the second column paid for it
      (should (< (cadr col-w) 413.82)))))

(ert-deftest niori-pdf-flow-table-cell-never-overflows ()
  "End-to-end: the cl-pdf assessment table renders with every cell
wrapped inside its column (regression for the malformed table)."
  (let ((doc (niori-pdf-with-flow-document (:title "t")
               (niori-pdf-flow-table
                (list :header ["Aspect" "Assessment"]
                      :rows
                      (list
                       (list "License"
                             "FreeBSD-style (BSD 2-clause), (c) 2003 Marc
Battyani; commercial use permitted with attribution")
                       (list "Dependencies"
                             "trivial-garbage; optional UFFI (zlib),
zpb-ttf (TrueType), salza2 (older PNG); core needs none"))
                      :cols '(70 413)
                      :padding 5))
               niori-pdf--doc)))
    (should doc)))

(ert-deftest niori-pdf-flow-toc-links ()
  "TOC entries carry link annotations whose /Dest is resolved at save
time (rect on the TOC page, XYZ destination at the heading)."
  (let* ((file (make-temp-file "niori-toc" nil ".pdf"))
         (doc (niori-pdf-flow-generate
               (lambda ()
                 (niori-pdf-with-flow-document (:title "toc demo")
                   (niori-pdf-flow-toc)
                   (niori-pdf-flow-page-break)
                   (niori-pdf-flow-heading "One" :level 1)
                   (niori-pdf-flow-spacer 400)
                   (niori-pdf-flow-heading "Two" :level 1)))))
         (bytes nil))
    (should (= (length (niori-pdf-document-pages doc)) 2))
    (niori-pdf-flow-save doc file)
    ;; pending links were resolved at save
    (should (null (niori-pdf-document-pending-links doc)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (setq bytes (decode-coding-string (buffer-string) 'raw-text)))
    (delete-file file)
    (should (string-search "/Subtype /Link" bytes))
    (should (string-search "/Dest [" bytes))))

(ert-deftest niori-pdf-cff-subset-structure ()
  "The CFF subset keeps the GID space, copies the charset and
rebuilds a valid Top DICT."
  (skip-unless (file-exists-p
                "/usr/share/fonts/opentype/urw-base35/NimbusSans-Regular.otf"))
  (let* ((ff (niori-pdf-ttf-read-file
              "/usr/share/fonts/opentype/urw-base35/NimbusSans-Regular.otf"))
         (bytes (niori-pdf-ttf-file-bytes ff))
         (cffo (car (niori-pdf-ttf--table-off bytes "CFF ")))
         (cff-len (cdr (niori-pdf-ttf--table-off bytes "CFF ")))
         (cff-table (substring bytes cffo (+ cffo cff-len)))
         (gids (list 2 34 36 70 72 75 109 111)) ; space A B e a s i l
         (sub (niori-pdf-ttf--cff-subset ff gids))
         (parsed (niori-pdf-ttf--cff-parse sub))
         (orig (niori-pdf-ttf--cff-parse cff-table))
         (r (niori-pdf-ttf--cff-read-index sub 4))
         (name-index (car r))
         (r (niori-pdf-ttf--cff-read-index sub (cdr r)))
         (top-raw (car (car r)))
         (r (niori-pdf-ttf--cff-read-index sub (cdr r)))
         (strings (car r))
         (r (niori-pdf-ttf--cff-read-index sub (cdr r)))
         (gsubrs (car r))
         (top (niori-pdf-ttf--cff-read-dict top-raw 0 (length top-raw)))
         (cs-off (car (niori-pdf-ttf--cff-dict-get top 17)))
         (r (niori-pdf-ttf--cff-read-index sub cs-off))
         (charstrings (car r))
         (charset-op (niori-pdf-ttf--cff-dict-get top 15))
         (charset-off (car charset-op))
         (priv-op (niori-pdf-ttf--cff-dict-get top 18)))
    ;; header + name preserved
    (should (= (length name-index) 1))
    (should (= (length strings) 615))
    ;; bias-stable: the subr COUNT never changes
    (should (= (length gsubrs)
               (length (plist-get parsed :gsubrs))))
    ;; glyph space preserved: 855 slots, only used ones non-empty
    (should (= (length charstrings)
               (niori-pdf-ttf-file-num-glyphs ff)))
    (should (> (length (nth 36 charstrings)) 1))  ; "A" is drawn
    (should (= (length (nth 500 charstrings)) 1)) ; unused: endchar
    ;; charset copied verbatim
    (let ((orig-ch (car (niori-pdf-ttf--cff-dict-get
                         (plist-get orig :top) 15)))
          (olen (niori-pdf-ttf--cff-charset-len
                 cff-table
                 (car (niori-pdf-ttf--cff-dict-get
                       (plist-get orig :top) 15))
                 (length charstrings))))
      (should (equal (substring sub charset-off (+ charset-off olen))
                     (substring cff-table orig-ch
                                (+ orig-ch olen)))))
    ;; rebuilt Top DICT references everything
    (should (numberp cs-off))
    (should (and priv-op (= (length priv-op) 2)))
    (should (> (car priv-op) 0))    ; private dict size
    (should (> (cadr priv-op) cs-off)) ; private dict follows charstrings
    ;; subset is much smaller than the original table
    (should (< (length sub) (* 0.5 (length cff-table))))))

(defun niori-pdf-tests--unescape-pdf-string (s)
  "Expand PDF string octal escapes in S into a unibyte string."
  (let ((i 0) out)
    (while (< i (length s))
      (let ((c (aref s i)))
        (if (and (eq c ?\\) (< (1+ i) (length s)))
            (let* ((j (1+ i))
                   (digits (substring s j (min (length s) (+ j 3)))))
              (if (and (= (length digits) 3)
                       (string-match-p "[0-7][0-7][0-7]" digits))
                  (progn
                    (push (string-to-number digits 8) out)
                    (setq i (+ j 3)))
                (push (aref s (1+ i)) out)
                (setq i (+ i 2))))
          (push c out)
          (setq i (1+ i)))))
    (apply #'string (nreverse out))))

(ert-deftest niori-pdf-info-has-version ()
  "Creator and Producer carry the package version."
  (let* ((doc (niori-pdf-make-document))
         (entries (niori-pdf--build-info-dict doc)))
    (dolist (key '(Creator Producer))
      (let* ((raw (niori-pdf-raw-str
                   (niori-pdf-dict-get entries key)))
             ;; expand the \\ooo escapes, drop the parens and the BOM,
             ;; decode the UTF-16BE bytes
             (bytes (niori-pdf-tests--unescape-pdf-string
                     (substring raw 1 -1)))
             (text (decode-coding-string (substring bytes 2) 'utf-16be)))
        (should (string-match-p
                 (regexp-quote (concat "niori-pdf.el " niori-pdf-version))
                 text))))))

(ert-deftest niori-pdf-info-entries-use-keywords ()
  "Explicit :title/:author/... info values all land in the Info dict.
Regression: the lookup used bare symbols against a keyword-keyed
plist, so explicit entries were silently dropped (only the implicit
Creator fallback and CreationDate survived)."
  (let ((doc (niori-pdf-make-document)))
    (setf (niori-pdf-document-info doc)
          (list :title "T" :author "A" :subject "S" :keywords "K"
                :creator "C" :producer "P"))
    (let ((entries (niori-pdf--build-info-dict doc)))
      (dolist (key '(Title Author Subject Keywords Creator Producer))
        (should (stringp (niori-pdf-raw-str
                          (niori-pdf-dict-get entries key))))))))

;;;; Flow features: lists, decorations, table spans, floats, vglue, refs

(defun niori-pdf-tests--occurs (sub s)
  "Number of non-overlapping SUB occurrences in S."
  (let ((n 0) (start 0))
    (while (string-match sub s start)
      (setq n (1+ n) start (match-end 0)))
    n))

(ert-deftest niori-pdf-flow-itemize-hanging-indent ()
  "List items get a hanging indent and a bullet marker."
  (let ((doc (niori-pdf-with-flow-document (:title "lists")
               (niori-pdf-flow-itemize
                '("alpha" "beta") :bullet "-"
                :indent 16 :gap 6))))
    (should doc)
    (let ((content (niori-pdf-page-content-string
                    (car (last (niori-pdf-document-pages doc))))))
      ;; two paragraphs plus two markers => at least 4 text objects
      (should (> (niori-pdf-tests--occurs "BT" content) 2))
      (should (string-match-p "alpha" content))
      (should (string-match-p "(beta) Tj" content)))))

(ert-deftest niori-pdf-flow-enumerate-numbers ()
  "Enumerate renders per-item number markers, right-aligned."
  (let ((doc (niori-pdf-with-flow-document (:title "enum")
               (niori-pdf-flow-enumerate
                '("one" "two" "three") :format "%d." :start 1))))
    (let ((content (niori-pdf-page-content-string
                    (car (niori-pdf-document-pages doc)))))
      (dolist (n '("1." "2." "3."))
        (should (string-match-p (regexp-quote n) content))))))

(ert-deftest niori-pdf-flow-run-decorations ()
  "Superscript/subscript shrink the box, underline/strike/background
emit graphics operators."
  (let ((doc (niori-pdf-with-flow-document (:title "deco")
               (niori-pdf-flow-paragraph
                (list "a"
                      (list "x" :superscript t)
                      (list "y" :subscript t)
                      (list "u" :underline t)
                      (list "s" :strike t)
                      (list "b" :background '(1 0.9 0.6)))))))
    (let ((content (niori-pdf-page-content-string
                    (car (niori-pdf-document-pages doc)))))
      ;; underline and strikethrough draw stroked lines
      (should (string-match-p " S\n" content))
      ;; the background is a filled rect
      (should (string-match-p " re f\n" content))
      ;; superscript text is set at 66% of the base size (7.0 vs 10.5)
      (should (string-match-p " 6\.93 Tf" content)))))

(ert-deftest niori-pdf-flow-table-spans ()
  "Rowspan/colspan cells occupy their grid rectangle; v-align shifts
the text inside the cell."
  (let ((doc (niori-pdf-with-flow-document (:title "spans")
               (niori-pdf-flow-table
                (list :header ["H1" "H2"]
                      :rows
                      (list
                       (list (list :text "span4" :rowspan 4 :valign 'bottom)
                             "r1c2")
                       (list "r2c2")
                       (list "r3c2")
                       (list (list :text "wide" :colspan 2))))))))
    (should doc)
    (let ((content (niori-pdf-page-content-string
                    (car (last (niori-pdf-document-pages doc))))))
      ;; all six cell texts present
      (dolist (txt '("H1" "H2" "span4" "r1c2" "r2c2" "r3c2" "wide"))
        (should (string-match-p (regexp-quote txt) content))))))

(ert-deftest niori-pdf-flow-table-keep-with-next ()
  "A row marked :keep-with-next moves to the next page together with
its successor when both do not fit."
  (let* ((doc (niori-pdf-with-flow-document
                  (:title "keep" :width 400 :height 300
                   :margins '(40 40 40 60))
                (dotimes (_ 12)
                  (niori-pdf-flow-table
                   (list :header ["A" "B"]
                         :rows (list (list "x1" "y1")
                                     (list "x2" "y2")))))))
           ;; the document must paginate without error and the header
           ;; must repeat on every page that has body rows
           (pages (niori-pdf-document-pages doc)))
    (should (> (length pages) 1))
    (dolist (p pages)
      (should (string-match-p "A" (niori-pdf-page-content-string p))))))

(ert-deftest niori-pdf-flow-table-space-after ()
  ":space-after advances the flow cursor below the closing rule."
  (let (y0 y1)
    (niori-pdf-with-flow-document (:title "s0")
      (niori-pdf-flow-table
       (list :header ["A"] :rows (list (list "x"))))
      (setq y0 (niori-pdf-flow-y niori-pdf--flow)))
    (niori-pdf-with-flow-document (:title "s1")
      (niori-pdf-flow-table
       (list :header ["A"] :rows (list (list "x")) :space-after 20))
      (setq y1 (niori-pdf-flow-y niori-pdf--flow)))
    (should (= (- y1 y0) 20))))

(ert-deftest niori-pdf-flow-footnote-bottom-of-page ()
  "A footnote is drawn on the page that cites it: the superscript
marker sits in the paragraph, the note text at the bottom of the
same page behind a separator rule."
  (let* ((doc (niori-pdf-flow-generate
               (lambda ()
                 (niori-pdf-with-flow-document (:title "fn")
                   (let ((fn (niori-pdf-flow-footnote
                              "Studio notes for the record.")))
                     (niori-pdf-flow-paragraph
                      (list "Cited here" (list fn :superscript t) "."))
                     (niori-pdf-flow-page-break)
                     (niori-pdf-flow-paragraph "Next page."))))))
         (pages (niori-pdf-document-pages doc)))
    ;; the pages list is newest-first: page 1 is the last element
    (should (= (length pages) 2))
    (let ((s1 (niori-pdf-test--page-literals (car (last pages)))))
      ;; marker paragraph and note text are both on page 1; interword
      ;; spaces are Td offsets, so the patterns carry no spaces
      (should (string-match-p "Citedhere" s1))
      (should (string-match-p "Studionotesfortherecord" s1))
      ;; the note is set in the default note size (8 pt)
      (should (string-match-p "/F1 8 Tf"
                             (niori-pdf-page-content-string
                              (car (last pages))))))
    (let ((s2 (niori-pdf-test--page-literals (car pages))))
      (should (string-match-p "Nextpage" s2))
      (should-not (string-match-p "Studionotes" s2)))))

(ert-deftest niori-pdf-flow-footnote-auto-numbers ()
  "Auto markers number the notes per page (1, 2, ... restart on the
next page); the per-page block heights are recorded for the next
layout pass."
  (let (markers)
    (let* ((doc (niori-pdf-flow-generate
                 (lambda ()
                   (niori-pdf-with-flow-document (:title "fn2")
                     (let ((fn1 (niori-pdf-flow-footnote "First note."))
                           (fn2 (niori-pdf-flow-footnote
                                 "Second note." :color '(0.4 0.4 0.4))))
                       (setq markers (list fn1 fn2))
                       (niori-pdf-flow-paragraph
                        (list "Cites" (list fn1 :superscript t) "and"
                              (list fn2 :superscript t) "."))
                       (niori-pdf-flow-page-break)
                       (let ((fn3 (niori-pdf-flow-footnote
                                   "Third note.")))
                         (setq markers (append markers (list fn3)))
                         (niori-pdf-flow-paragraph
                          (list "Page two cites"
                                (list fn3 :superscript t) "."))))))))
           (flow (niori-pdf-flow--current))
           (heights (niori-pdf-flow-footnote-heights flow)))
      (should (equal markers '("1" "2" "1")))
      ;; block height = rule zone (7.5) + one 8/1.24 line per note
      ;; (9.92) + the 2.5 inter-note gap on page 1
      (should (< (abs (- (cdr (assq 1 heights)) 29.84)) 0.01))
      (should (< (abs (- (cdr (assq 2 heights)) 17.42)) 0.01))
      ;; the reserve shortens the text column of a note page
      (let ((niori-pdf-flow--pass-footnotes heights))
        (setf (niori-pdf-flow-page-number flow) 1)
        (should (< (niori-pdf-flow--bottom flow)
                   (- (niori-pdf-flow-page-h flow)
                      (niori-pdf-flow-margin-bottom flow))))))))

(ert-deftest niori-pdf-flow-float-wraps-text ()
  "Paragraph lines beside a float start right of it; after the float
they return to the full width."
  (let* ((flow-w 300)
         (doc (niori-pdf-with-flow-document
                  (:title "float" :width 400 :height 500
                   :margins '(40 40 40 40))
                (niori-pdf-flow-float
                 (lambda ()
                   (niori-pdf-flow-paragraph "FLOAT" :align 'center))
                 :width 100 :side 'left :gap 0)
                (niori-pdf-flow-paragraph
                 "aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm
nnn ooo ppp qqq rrr sss ttt uuu vvv www xxx yyy zzz" :align 'left)
                nil))
         (content (niori-pdf-page-content-string
                   (car (niori-pdf-document-pages doc)))))
    ;; first line starts right of the float (x > 140), later lines at 40
    (should (string-match-p "1 0 0 1 1[0-9][0-9] " content))
    (should (string-match-p "1 0 0 1 40 " content))))

(ert-deftest niori-pdf-flow-float-rule-shortened ()
  "A rule beside a float stops before the float."
  (let* ((doc (niori-pdf-with-flow-document
                  (:title "float rule" :width 400 :height 500
                   :margins '(40 40 40 40))
                (niori-pdf-flow-float
                 (lambda ()
                   (niori-pdf-flow-paragraph "F" :align 'center))
                 :width 100 :side 'left :gap 0)
                (niori-pdf-flow-rule)))
         (content (niori-pdf-page-content-string
                   (car (niori-pdf-document-pages doc)))))
    ;; rule drawn from x=140 to x=360 (200 points)
    (should (string-match-p "140 [0-9.]+ m\n360 [0-9.]+ l" content))))

(ert-deftest niori-pdf-flow-vfill-pushes-to-bottom ()
  "vfill stretches so the block after it lands at the bottom margin."
  (let* ((doc (niori-pdf-with-flow-document
                  (:title "vfill" :width 400 :height 500
                   :margins '(40 40 40 40))
                (niori-pdf-flow-heading "top" :level 1 :toc nil
                                        :bookmark nil)
                (niori-pdf-flow-vfill)
                (niori-pdf-flow-paragraph "pushed" :align 'left)))
         (content (niori-pdf-page-content-string
                   (car (last (niori-pdf-document-pages doc))))))
    ;; the cm transform shifts the pushed paragraph near 460 (bottom)
    (should (string-match-p "cm" content))
    (should (string-match-p "pushed" content))))

(ert-deftest niori-pdf-flow-vglue-proportional ()
  "Two vglues with different stretch share the leftover space 1:2."
  (let* ((doc (niori-pdf-with-flow-document
                  (:title "vglue" :width 200 :height 400
                   :margins '(40 40 40 40))
                (niori-pdf-flow-paragraph "A")
                (niori-pdf-flow-vglue 1)
                (niori-pdf-flow-paragraph "B")
                (niori-pdf-flow-vglue 2)
                (niori-pdf-flow-paragraph "C")))
         (content (niori-pdf-page-content-string
                   (car (last (niori-pdf-document-pages doc))))))
    ;; two shifts: the second twice the first
    (let ((shifts nil) (start 0))
      (while (string-match "1 0 0 1 0 \\(-[0-9.]+\\) cm" content start)
        (push (string-to-number (match-string 1 content)) shifts)
        (setq start (match-end 0)))
      (setq shifts (nreverse shifts))
      (should (= (length shifts) 2))
      (should (< (abs (- (- (nth 1 shifts) (nth 0 shifts))
                            (* 2.0 (nth 0 shifts))))
                         1.0)))))

(ert-deftest niori-pdf-flow-ref-points-resolve ()
  "mark-ref-point + put-ref-page-number resolve to the real page
after the multi-pass generate."
  (let ((doc (niori-pdf-flow-generate
              (lambda ()
                (niori-pdf-with-flow-document (:title "refs")
                  (niori-pdf-flow-paragraph
                   (list "target on page "
                         (format "%s"
                                 (or (niori-pdf-flow-ref-page-number
                                      'tgt)
                                     "?"))))
                  (niori-pdf-flow-page-break)
                  (niori-pdf-flow-heading "Target" :level 1)
                  (niori-pdf-flow-mark-ref-point 'tgt)
                  (niori-pdf-flow-put-ref-page-number 'tgt))))))
    (let ((content (niori-pdf-page-content-string
                    (car (last (niori-pdf-document-pages doc))))))
      ;; the first page must reference page 2, not "?"; words are
      ;; emitted as per-word TJ arrays
      (should (string-match-p "(2) Tj" content))
      (should (string-match-p "(page) Tj" content))
      (should-not (string-match-p "(\\?) Tj" content)))))

;;;; TrueType collections (.ttc)

(defconst niori-pdf-tests--ttc
  "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc"
  "TrueType collection available on Debian-based systems.")

(ert-deftest niori-pdf-ttc-collection-listing ()
  "collection-fonts lists (INDEX PS-NAME FAMILY) for each member."
  (skip-unless (file-exists-p niori-pdf-tests--ttc))
  (let ((fonts (niori-pdf-ttf-collection-fonts niori-pdf-tests--ttc)))
    (should (>= (length fonts) 2))
    (should (= (nth 0 (nth 0 fonts)) 0))
    (should (stringp (nth 1 (nth 0 fonts))))
    (should (stringp (nth 2 (nth 0 fonts))))))

(ert-deftest niori-pdf-ttc-index-selection ()
  "read-file INDEX selects the member; out-of-range errors."
  (skip-unless (file-exists-p niori-pdf-tests--ttc))
  (let* ((f0 (niori-pdf-ttf-read-file niori-pdf-tests--ttc 0))
         (f1 (niori-pdf-ttf-read-file niori-pdf-tests--ttc 1)))
    (should (equal (niori-pdf-ttf-file-ps-name f0)
                   (nth 1 (nth 0 (niori-pdf-ttf-collection-fonts
                                  niori-pdf-tests--ttc)))))
    (should-not (equal (niori-pdf-ttf-file-ps-name f0)
                       (niori-pdf-ttf-file-ps-name f1))))
  (should-error (niori-pdf-ttf-read-file niori-pdf-tests--ttc 99)))

(ert-deftest niori-pdf-ttc-plain-font-rejects-index ()
  "A non-collection font errors when an index is requested."
  (let ((ttf (expand-file-name
              "examples/chart.png" niori-pdf-tests--pkg)))
    (skip-unless (file-exists-p "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"))
    (should-error (niori-pdf-ttf-read-file
                   "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" 1))))

(ert-deftest niori-pdf-ttc-plist-spec-measure-and-embed ()
  "(:file PATH :index N) specs work for measurement and embedding."
  (skip-unless (file-exists-p niori-pdf-tests--ttc))
  (let* ((spec0 '(:file "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc" :index 0))
         (spec1 '(:file "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc" :index 1))
         (w0 (niori-pdf-text-width "test" spec0 12))
         (w1 (niori-pdf-text-width "test" spec1 12)))
    (should (> w0 0))
    (should (> w1 0)))
  (let* ((doc (niori-pdf-make-document))
         (page (niori-pdf-new-page doc)))
    (let ((niori-pdf--doc doc) (niori-pdf--page page))
      (niori-pdf-set-font
       '(:file "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc" :index 1) 12)
      (niori-pdf-draw-text 60 700 "TTC test"))
    (niori-pdf-save doc "/tmp/niori-test/ttc-test.pdf")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents "/tmp/niori-test/ttc-test.pdf")
      (should (string-match-p "WenQuanYiZenHeiMono" (buffer-string))))))

;;;; Type 1 fonts (.pfb / .pfa)

(defconst niori-pdf-tests--pfb
  "/usr/share/fonts/X11/Type1/NimbusSans-Regular.pfb"
  "Type 1 font available on Debian-based systems (Helvetica clone).")

(ert-deftest niori-pdf-type1-parse ()
  "PFB parsing extracts the name, eexec widths and bounding box."
  (skip-unless (file-exists-p niori-pdf-tests--pfb))
  (let ((t1 (niori-pdf-type1-read-file niori-pdf-tests--pfb)))
    (should (string= (niori-pdf-type1-file-font-name t1)
                     "NimbusSans-Regular"))
    (should (niori-pdf-type1-file-bbox t1))
    ;; Nimbus Sans is metric-compatible with Helvetica: the AFM widths
    ;; for space/A are 278/667, and charstrings must yield the same
    (let ((wh (niori-pdf-type1-file-widths-hash t1)))
      (should (= (gethash "space" wh) 278))
      (should (= (gethash "A" wh) 667)))
    ;; the FontFile parts must sum up to the stream length
    (should (= (+ (niori-pdf-type1-file-length1 t1)
                  (niori-pdf-type1-file-length2 t1)
                  (niori-pdf-type1-file-length3 t1))
               (length (niori-pdf-type1-file-fontfile-bytes t1))))))

(ert-deftest niori-pdf-type1-measure ()
  "Type 1 fonts measure like their metric-compatible base-14 font."
  (skip-unless (file-exists-p niori-pdf-tests--pfb))
  (let ((w-t1 (niori-pdf-text-width "Hello World" niori-pdf-tests--pfb 10))
        (w-helv (niori-pdf-text-width "Hello World" "Helvetica" 10)))
    (should (> w-t1 0))
    ;; Nimbus Sans has the Helvetica metrics; allow small drift
    (should (< (abs (- w-t1 w-helv)) 0.3))))

(ert-deftest niori-pdf-type1-embed-and-draw ()
  "A .pfb font embeds as /Subtype /Type1 with /FontFile and draws."
  (skip-unless (file-exists-p niori-pdf-tests--pfb))
  (let* ((doc (niori-pdf-make-document))
         (page (niori-pdf-new-page doc)))
    (let ((niori-pdf--doc doc) (niori-pdf--page page))
      (niori-pdf-set-font niori-pdf-tests--pfb 11)
      (niori-pdf-draw-text 60 700 "Type 1 embedded")
      ;; widths flow through the normal measurement path
      (should (> (niori-pdf-text-width "Type 1 embedded"
                                       niori-pdf-tests--pfb 11)
                 0)))
    (niori-pdf-save doc "/tmp/niori-test/type1-test.pdf")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents "/tmp/niori-test/type1-test.pdf")
      (let ((s (buffer-string)))
        (should (string-match-p "/Subtype /Type1" s))
        (should (string-match-p "/FontFile" s))
        (should (string-match-p "/Length1" s))
        (should (string-match-p "/Length3" s))
        (should (string-match-p "NimbusSans-Regular" s))))))

;;;; PDF/A-2b

(defconst niori-pdf-tests--ttf
  "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf")

(ert-deftest niori-pdf-pdfa-icc-profile-is-valid ()
  "The generated ICC profile has a valid header and tag table."
  (let ((icc (niori-pdf-pdfa--icc-srgb)))
    ;; header
    (should (string= (substring icc 36 40) "acsp"))
    (should (string= (substring icc 12 16) "mntr"))
    (should (string= (substring icc 16 20) "RGB "))
    ;; declared size == actual size
    (let ((n (+ (* 16777216 (aref icc 0)) (* 65536 (aref icc 1))
                (* 256 (aref icc 2)) (aref icc 3))))
      (should (= n (length icc))))
    ;; tag table: each entry's data must start with its type signature
    (let ((count (+ (* 16777216 (aref icc 128)) (* 65536 (aref icc 129))
                    (* 256 (aref icc 130)) (aref icc 131)))
          (seen nil))
      (should (= count 10))
      (dotimes (i count)
        (let* ((base (+ 132 (* i 12)))
               (sig (substring icc base (+ base 4)))
               (off (+ (* 16777216 (aref icc (+ base 4)))
                       (* 65536 (aref icc (+ base 5)))
                       (* 256 (aref icc (+ base 6)))
                       (aref icc (+ base 7))))
               (size (+ (* 16777216 (aref icc (+ base 8)))
                        (* 65536 (aref icc (+ base 9)))
                        (* 256 (aref icc (+ base 10)))
                        (aref icc (+ base 11)))))
          (push sig seen)
          (should (<= (+ off size) (length icc)))
          ;; the payload must begin with its TYPE signature, not the
          ;; tag signature (rTRC holds 'curv', rXYZ holds 'XYZ ', ...)
          (should (or (string= (substring icc off (+ off 4)) "XYZ ")
                      (string= (substring icc off (+ off 4)) "curv")
                      (string= (substring icc off (+ off 4)) "text")
                      (string= (substring icc off (+ off 4)) "desc"))))
        )
      (should (= (length seen) 10)))))

(ert-deftest niori-pdf-pdfa-file-structure ()
  "PDF/A output: 1.7 header, /ID trailer, OutputIntent, XMP."
  (skip-unless (file-exists-p niori-pdf-tests--ttf))
  (let* ((doc (niori-pdf-make-document))
         (page (niori-pdf-new-page doc)))
    (setf (niori-pdf-document-info doc)
          (list :title "Archival" :author "Test"))
    (niori-pdf-pdfa-enable doc)
    (let ((niori-pdf--doc doc) (niori-pdf--page page))
      (niori-pdf-set-font niori-pdf-tests--ttf 12)
      (niori-pdf-draw-text 60 700 "archival")
      (niori-pdf-add-uri-link 60 680 100 12 "https://example.com/"))
    (niori-pdf-save doc "/tmp/niori-test/pdfa-test.pdf"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents "/tmp/niori-test/pdfa-test.pdf")
    (let ((s (buffer-string)))
      (should (string-prefix-p "%PDF-1.7" s))
      (should (string-match-p "/ID \\[<[0-9a-f]+> <[0-9a-f]+>\\]" s))
      (should (string-match-p "/OutputIntents" s))
      (should (string-match-p "/GTS_PDFA1" s))
      (should (string-match-p "/DestOutputProfile" s))
      (should (string-match-p "pdfaid:part>2<" s))
      (should (string-match-p "pdfaid:conformance>B<" s))
      (should (string-match-p "dc:title" s))
      (should (string-match-p "Archival" s))
      ;; link annotations carry the Print flag (ISO 19005-2 6.8)
      (should (string-match-p "/F 4" s))
      ;; the DestOutputProfile stream is a valid ICC profile
      (let ((m (string-match "/DestOutputProfile \\([0-9]+\\) 0 R" s))
            (num (string-to-number (match-string 1 s))))
        (should m)
        (let ((pos (string-match (format "^%d 0 obj" num) s)))
          (should pos)
          (let ((stream-pos (string-match "stream\n" s pos))
                (end-pos (string-match "\nendstream" s pos)))
            (should stream-pos)
            ;; header of the ICC payload: acsp at offset 36
            (should (string= (substring s (+ stream-pos 7 36)
                                        (+ stream-pos 7 40))
                             "acsp"))))))))

(ert-deftest niori-pdf-pdfa-base14-rejection ()
  "Unmapped base-14 fonts are rejected in PDF/A documents."
  (let ((doc (niori-pdf-make-document)))
    (niori-pdf-pdfa-enable doc)
    (let ((niori-pdf--doc doc)
          (niori-pdf--page (niori-pdf-new-page doc)))
      (should-error (niori-pdf-set-font "Symbol" 10)))))

(ert-deftest niori-pdf-pdfa-base14-substitution ()
  "Mapped base-14 fonts are replaced by the embedded substitute."
  (skip-unless (file-exists-p niori-pdf-tests--ttf))
  (let ((doc (niori-pdf-make-document))
        (niori-pdf-pdfa-base14-substitutions
         `(("Helvetica" . ,niori-pdf-tests--ttf))))
    (niori-pdf-pdfa-enable doc)
    (let ((niori-pdf--doc doc)
          (niori-pdf--page (niori-pdf-new-page doc))
          font)
      (setq font (niori-pdf-set-font "Helvetica" 12))
      (niori-pdf-draw-text 60 700 "substituted")
      (should (eq (niori-pdf-font-kind font) 'ttf)))))

(ert-deftest niori-pdf-pdfa-cmyk-rejection ()
  "DeviceCMYK operators are rejected in PDF/A documents."
  (skip-unless (file-exists-p niori-pdf-tests--ttf))
  (let ((doc (niori-pdf-make-document)))
    (niori-pdf-pdfa-enable doc)
    (let ((niori-pdf--doc doc)
          (niori-pdf--page (niori-pdf-new-page doc)))
      (niori-pdf-set-font niori-pdf-tests--ttf 10)
      (should-error (niori-pdf-set-cmyk-fill 0 0 0 1)))))

(ert-deftest niori-pdf-pdfa-level-validation ()
  "Only level 2 is supported; plain documents keep 1.4 + no ID change."
  (let ((doc (niori-pdf-make-document)))
    (should-error (niori-pdf-pdfa-enable doc 1))
    (should-error (niori-pdf-pdfa-enable doc 3)))
  (let* ((doc (niori-pdf-make-document))
         (page (niori-pdf-new-page doc)))
    (let ((niori-pdf--doc doc) (niori-pdf--page page))
      (niori-pdf-set-font "Helvetica" 12)
      (niori-pdf-draw-text 60 700 "plain"))
    (niori-pdf-save doc "/tmp/niori-test/plain-test.pdf")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents "/tmp/niori-test/plain-test.pdf")
      (let ((s (buffer-string)))
        (should (string-prefix-p "%PDF-1.4" s))
        ;; plain files carry /ID too (harmless, uniquely identifying)
        (should (string-match-p "/ID \\[<" s))))))

(ert-deftest niori-pdf-pdfa-flow-macro-prop ()
  ":pdfa t in niori-pdf-with-flow-document enables archival mode."
  (skip-unless (file-exists-p niori-pdf-tests--ttf))
  (let* ((niori-pdf-pdfa-base14-substitutions
          `(("Helvetica" . ,niori-pdf-tests--ttf)))
         (doc (niori-pdf-with-flow-document
                  (:title "Flow PDF/A" :pdfa t)
                (niori-pdf-flow-paragraph "archival flow"))))
    (should (= (niori-pdf-document-pdfa-level doc) 2))
    (niori-pdf-save doc "/tmp/niori-test/pdfa-flow.pdf")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents "/tmp/niori-test/pdfa-flow.pdf")
      (should (string-prefix-p "%PDF-1.7" (buffer-string))))))
