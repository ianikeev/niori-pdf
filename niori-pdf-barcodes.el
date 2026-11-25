;;; niori-pdf-barcodes.el --- Bar codes for niori-pdf (port of cl-pdf bar-codes.lisp) -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2003 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; This file contains code ported from CL-PDF's bar-codes.lisp.
;; The Code 128 implementation is credited to Lars Rustemeier in the
;; original CL-PDF source.  See LICENSE and THIRD-PARTY-NOTICES.

;;; Commentary:

;; Barcode drawing for niori-pdf.  This module ports cl-pdf's
;; bar-codes.lisp:
;;
;;   - Code 128 (full port of `draw-bar-code128' by Lars Rustemeier,
;;     including the complete 107-entry symbology table with the code
;;     A / code B / code C columns and the signed bar/space patterns;
;;     positive pattern numbers are black bars, negative ones white
;;     spaces).  Charset C is auto-selected for even-length all-digit
;;     strings, charset B otherwise; charset A is implemented but never
;;     auto-selected (as in cl-pdf).
;;
;;   - QR matrix drawing (port of `draw-qr-code': dark modules drawn as
;;     squares with the y axis running downward from the anchor, with
;;     per-row run-length encoding to keep the content stream small).
;;
;; and adds two small new symbologies not present in cl-pdf (documented
;; as such):
;;
;;   - Code 3 of 9 (Code 39) with the standard 43-character alphabet,
;;   - EAN-13 with guard-bar descent and human readable digits.
;;
;; Calling convention: Elisp has no `&key', so the optional arguments
;; of the drawing functions are passed as a keyword plist after the
;; positional ones.  The call syntax is identical to Common Lisp:
;;
;;   (niori-pdf-barcode-code128 "CL-PDF" 50 50 :height 60 :width 300)
;;
;; All functions must be called inside `niori-pdf-with-document' and
;; `niori-pdf-with-page' (they emit operators into the current page).
;; All coordinates are PDF points with y increasing upward, exactly as
;; in cl-pdf.  The drawing functions return (list WIDTH HEIGHT).

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-content)
(require 'niori-pdf-fonts)

;;;; Color helper

(defun niori-pdf-barcodes--color (color)
  "Normalize a barcode COLOR spec to an RGB list.
COLOR may be nil (black), an X11 color name accepted by
`niori-pdf-color', or a list (R G B) of 0..1 components."
  (cond ((null color) '(0 0 0))
        ((stringp color) (niori-pdf-color color))
        ((and (consp color) (= (length color) 3)
              (cl-every #'numberp color))
         (mapcar #'float color))
        (t (user-error "niori-pdf-barcodes: invalid color %S" color))))

;;;; Code 128 logic layer (port of bar-codes.lisp)
;;
;; Table format (one list per value, vector indexed by value):
;;   (VALUE CODE-A CODE-B CODE-C PATTERN...)
;; where CODE-A/CODE-B are characters (or keyword symbols for the
;; function/start/stop codes), CODE-C is the two-digit pair string,
;; and PATTERN is the sequence of bar/space widths: positive numbers
;; are black bars, negative numbers white spaces.

(defconst niori-pdf-barcodes--code128-table
  [ (0 ?\s ?\s "00" 2 -1 2 -2 2 -2)
    (1 ?! ?! "01" 2 -2 2 -1 2 -2)
    (2 ?\" ?\" "02" 2 -2 2 -2 2 -1)
    (3 ?# ?# "03" 1 -2 1 -2 2 -3)
    (4 ?$ ?$ "04" 1 -2 1 -3 2 -2)
    (5 ?% ?% "05" 1 -3 1 -2 2 -2)
    (6 ?& ?& "06" 1 -2 2 -2 1 -3)
    (7 ?' ?' "07" 1 -2 2 -3 1 -2)
    (8 ?\( ?\( "08" 1 -3 2 -2 1 -2)
    (9 ?\) ?\) "09" 2 -2 1 -2 1 -3)
    (10 ?* ?* "10" 2 -2 1 -3 1 -2)
    (11 ?+ ?+ "11" 2 -3 1 -2 1 -2)
    (12 ?, ?, "12" 1 -1 2 -2 3 -2)
    (13 ?- ?- "13" 1 -2 2 -1 3 -2)
    (14 ?. ?. "14" 1 -2 2 -2 3 -1)
    (15 ?/ ?/ "15" 1 -1 3 -2 2 -2)
    (16 ?0 ?0 "16" 1 -2 3 -1 2 -2)
    (17 ?1 ?1 "17" 1 -2 3 -2 2 -1)
    (18 ?2 ?2 "18" 2 -2 3 -2 1 -1)
    (19 ?3 ?3 "19" 2 -2 1 -1 3 -2)
    (20 ?4 ?4 "20" 2 -2 1 -2 3 -1)
    (21 ?5 ?5 "21" 2 -1 3 -2 1 -2)
    (22 ?6 ?6 "22" 2 -2 3 -1 1 -2)
    (23 ?7 ?7 "23" 3 -1 2 -1 3 -1)
    (24 ?8 ?8 "24" 3 -1 1 -2 2 -2)
    (25 ?9 ?9 "25" 3 -2 1 -1 2 -2)
    (26 ?: ?: "26" 3 -2 1 -2 2 -1)
    (27 ?\; ?\; "27" 3 -1 2 -2 1 -2)
    (28 ?< ?< "28" 3 -2 2 -1 1 -2)
    (29 ?= ?= "29" 3 -2 2 -2 1 -1)
    (30 ?> ?> "30" 2 -1 2 -1 2 -3)
    (31 ?\? ?\? "31" 2 -1 2 -3 2 -1)
    (32 ?@ ?@ "32" 2 -3 2 -1 2 -1)
    (33 ?A ?A "33" 1 -1 1 -3 2 -3)
    (34 ?B ?B "34" 1 -3 1 -1 2 -3)
    (35 ?C ?C "35" 1 -3 1 -3 2 -1)
    (36 ?D ?D "36" 1 -1 2 -3 1 -3)
    (37 ?E ?E "37" 1 -3 2 -1 1 -3)
    (38 ?F ?F "38" 1 -3 2 -3 1 -1)
    (39 ?G ?G "39" 2 -1 1 -3 1 -3)
    (40 ?H ?H "40" 2 -3 1 -1 1 -3)
    (41 ?I ?I "41" 2 -3 1 -3 1 -1)
    (42 ?J ?J "42" 1 -1 2 -1 3 -3)
    (43 ?K ?K "43" 1 -1 2 -3 3 -1)
    (44 ?L ?L "44" 1 -3 2 -1 3 -1)
    (45 ?M ?M "45" 1 -1 3 -1 2 -3)
    (46 ?N ?N "46" 1 -1 3 -3 2 -1)
    (47 ?O ?O "47" 1 -3 3 -1 2 -1)
    (48 ?P ?P "48" 3 -1 3 -1 2 -1)
    (49 ?Q ?Q "49" 2 -1 1 -3 3 -1)
    (50 ?R ?R "50" 2 -3 1 -1 3 -1)
    (51 ?S ?S "51" 2 -1 3 -1 1 -3)
    (52 ?T ?T "52" 2 -1 3 -3 1 -1)
    (53 ?U ?U "53" 2 -1 3 -1 3 -1)
    (54 ?V ?V "54" 3 -1 1 -1 2 -3)
    (55 ?W ?W "55" 3 -1 1 -3 2 -1)
    (56 ?X ?X "56" 3 -3 1 -1 2 -1)
    (57 ?Y ?Y "57" 3 -1 2 -1 1 -3)
    (58 ?Z ?Z "58" 3 -1 2 -3 1 -1)
    (59 ?\[ ?\[ "59" 3 -3 2 -1 1 -1)
    (60 ?\\ ?\\ "60" 3 -1 4 -1 1 -1)
    (61 ?\] ?\] "61" 2 -2 1 -4 1 -1)
    (62 ?^ ?^ "62" 4 -3 1 -1 1 -1)
    (63 ?_ ?_ "63" 1 -1 1 -2 2 -4)
    (64 'NUL ?` "64" 1 -1 1 -4 2 -2)
    (65 'SOH ?a "65" 1 -2 1 -1 2 -4)
    (66 'STX ?b "66" 1 -2 1 -4 2 -1)
    (67 'ETX ?c "67" 1 -4 1 -1 2 -2)
    (68 'EOT ?d "68" 1 -4 1 -2 2 -1)
    (69 'ENQ ?e "69" 1 -1 2 -2 1 -4)
    (70 'ACK ?f "70" 1 -1 2 -4 1 -2)
    (71 'BEL ?g "71" 1 -2 2 -1 1 -4)
    (72 'BS ?h "72" 1 -2 2 -4 1 -1)
    (73 'HT ?i "73" 1 -4 2 -1 1 -2)
    (74 'LF ?j "74" 1 -4 2 -2 1 -1)
    (75 'VT ?k "75" 2 -4 1 -2 1 -1)
    (76 'FF ?l "76" 2 -2 1 -1 1 -4)
    (77 'CR ?m "77" 4 -1 3 -1 1 -1)
    (78 'SO ?n "78" 2 -4 1 -1 1 -2)
    (79 'SI ?o "79" 1 -3 4 -1 1 -1)
    (80 'DLE ?p "80" 1 -1 1 -2 4 -2)
    (81 'DC1 ?q "81" 1 -2 1 -1 4 -2)
    (82 'DC2 ?r "82" 1 -2 1 -2 4 -1)
    (83 'DC3 ?s "83" 1 -1 4 -2 1 -2)
    (84 'DC4 ?t "84" 1 -2 4 -1 1 -2)
    (85 'NAK ?u "85" 1 -2 4 -2 1 -1)
    (86 'SYN ?v "86" 4 -1 1 -2 1 -2)
    (87 'ETB ?w "87" 4 -2 1 -1 1 -2)
    (88 'CAN ?x "88" 4 -2 1 -2 1 -1)
    (89 'EM ?y "89" 2 -1 2 -1 4 -1)
    (90 'SUB ?z "90" 2 -1 4 -1 2 -1)
    (91 'ESC ?{ "91" 4 -1 2 -1 2 -1)
    (92 'FS ?| "92" 1 -1 1 -1 4 -3)
    (93 'GS ?} "93" 1 -1 1 -3 4 -1)
    (94 'RS ?~ "94" 1 -3 1 -1 4 -1)
    (95 'US 'DEL "95" 1 -1 4 -1 1 -3)
    (96 FNC-3 FNC-3 "96" 1 -1 4 -3 1 -1)
    (97 FNC-2 FNC-2 "97" 4 -1 1 -1 1 -3)
    (98 'SHIFT 'SHIFT "98" 4 -1 1 -3 1 -1)
    (99 CODE-C CODE-C "99" 1 -1 3 -1 4 -1)
    (100 CODE-B FNC-4 CODE-B 1 -1 4 -1 3 -1)
    (101 FNC-4 CODE-A CODE-A 3 -1 1 -1 4 -1)
    (102 FNC-1 FNC-1 FNC-1 4 -1 1 -1 3 -1)
    (103 START-A START-A START-A 2 -1 1 -4 1 -2)
    (104 START-B START-B START-B 2 -1 1 -2 1 -4)
    (105 START-C START-C START-C 2 -1 1 -2 3 -2)
    (106 STOP STOP STOP 2 -3 3 -1 1 -1 2)]
  "The complete Code 128 symbology table (107 entries).
Ported verbatim from cl-pdf bar-codes.lisp (*table*), which credits
Lars Rustemeier for the Code 128 code.")

(defun niori-pdf-barcodes--code128-index (column)
  "Build the lookup hash for COLUMN (1=code A, 2=code B, 3=code C) of
`niori-pdf-barcodes--code128-table': key -> (VALUE . PATTERN)."
  (let ((ht (make-hash-table :test 'equal)))
    (cl-loop for row across niori-pdf-barcodes--code128-table
             do (puthash (nth column row)
                         (cons (car row) (nthcdr 4 row))
                         ht))
    ht))

(defconst niori-pdf-barcodes--code128-a
  (niori-pdf-barcodes--code128-index 1)
  "Code 128-A lookup: character/symbol key -> (VALUE . PATTERN).")

(defconst niori-pdf-barcodes--code128-b
  (niori-pdf-barcodes--code128-index 2)
  "Code 128-B lookup: character/symbol key -> (VALUE . PATTERN).")

(defconst niori-pdf-barcodes--code128-c
  (niori-pdf-barcodes--code128-index 3)
  "Code 128-C lookup: pair-string/symbol key -> (VALUE . PATTERN).")

(defun niori-pdf-barcodes--code128-checksum (values)
  "Code 128 checksum of VALUES (start value first):
(START + sum(value_i * i)) mod 103, exactly as in cl-pdf."
  (let ((sum (car values))
        (i 1))
    (dolist (v (cdr values) (mod sum 103))
      (cl-incf sum (* v i))
      (cl-incf i))))

(defun niori-pdf-barcodes--group (string n)
  "Split STRING into chunks of N characters (port of cl-pdf `group')."
  (unless (> n 0) (error "niori-pdf-barcodes: zero length group"))
  (let ((chunks nil)
        (len (length string))
        (i 0))
    (while (< i len)
      (push (substring string i (min (+ i n) len)) chunks)
      (cl-incf i n))
    (nreverse chunks)))

(defun niori-pdf-barcodes--code128-check-b (string)
  "Signal `user-error' if STRING has characters outside Code 128-B."
  (let (bad)
    (dotimes (i (length string))
      (let ((ch (aref string i)))
        (unless (and (>= ch 32) (<= ch 126))
          (push ch bad))))
    (when bad
      (user-error
       "niori-pdf-barcodes: characters not encodable in Code 128-B (printable ASCII 0x20..0x7e): %s"
       (mapconcat (lambda (ch) (format "%c (0x%02X)" ch ch))
                  (nreverse bad) ", ")))))

(defun niori-pdf-barcodes--code128-values (string charset)
  "Return the list of Code 128 values (start value first) for STRING
in CHARSET (`a', `b' or `c').  Validation errors use `user-error'."
  (pcase charset
    ('a
     (mapcar (lambda (ch)
               (cond ((and (>= ch 0) (<= ch 31)) (+ ch 64))
                     ((and (>= ch 32) (<= ch 95)) (- ch 32))
                     (t (user-error
                         "niori-pdf-barcodes: character %c (0x%02X) not encodable in Code 128-A"
                         ch ch))))
             string))
    ('b
     (niori-pdf-barcodes--code128-check-b string)
     (mapcar (lambda (ch) (- ch 32)) string))
    ('c
     (unless (and (cl-evenp (length string))
                  (string-match-p "\\`[0-9]*\\'" string))
       (user-error
        "niori-pdf-barcodes: Code 128-C requires an even-length all-digit string: %S"
        string))
     (mapcar #'string-to-number
             (niori-pdf-barcodes--group string 2)))
    (_ (user-error "niori-pdf-barcodes: unknown Code 128 charset %S" charset))))

(defun niori-pdf-barcodes--code128-encode (string charset)
  "Encode STRING in Code 128 CHARSET (`a', `b' or `c').
Return (PATTERNS CHECKSUM) where PATTERNS is the list of bar/space
patterns for start + data + checksum + stop (port of cl-pdf's
`code128-a' / `code128-b' / `code128-c')."
  (let* ((s-table (pcase charset
                    ('a niori-pdf-barcodes--code128-a)
                    ('b niori-pdf-barcodes--code128-b)
                    ('c niori-pdf-barcodes--code128-c)))
         (getter (pcase charset ('a 1) ('b 2) ('c 3)))
         (start-value (pcase charset ('a 103) ('b 104) ('c 105)))
         (start-key (pcase charset ('a 'START-A) ('b 'START-B) ('c 'START-C)))
         (values (niori-pdf-barcodes--code128-values string charset))
         ;; checksum includes the start value, exactly as in cl-pdf
         (checksum (niori-pdf-barcodes--code128-checksum
                    (cons start-value values)))
         (start-pattern (cdr (gethash start-key s-table)))
         (data-patterns
          (mapcar
           (lambda (v)
             ;; port of: (cdr (gethash (getter (gethash v *table-h*)) s-table))
             (cdr (gethash (nth getter (aref niori-pdf-barcodes--code128-table v))
                           s-table)))
           values))
         ;; the checksum value indexes the table directly (row = code)
         (checksum-pattern
          (nthcdr 4 (aref niori-pdf-barcodes--code128-table checksum)))
         (stop (cdr (gethash 'STOP s-table))))
    (list (append (list start-pattern) data-patterns
                  (list checksum-pattern stop))
          checksum)))

(defun niori-pdf-barcodes--code128-charset (string)
  "Auto-select the Code 128 charset for STRING: `c' for even-length
all-digit strings, `b' otherwise (as in cl-pdf `draw-bar-code128')."
  (if (and (cl-evenp (length string))
           (string-match-p "\\`[0-9]*\\'" string))
      'c
    'b))

(defun niori-pdf-barcodes--code128-unit-width (patterns)
  "Total width of PATTERNS in units (sum of absolute pattern numbers)."
  (let ((sum 0))
    (dolist (p patterns sum)
      (dolist (e p)
        (cl-incf sum (abs e))))))

;;;; Code 128 drawing

(defun niori-pdf-barcode-code128 (string x y &rest keys)
  "Draw a Code 128 barcode of STRING with its baseline at (X Y).

KEYS is a plist of keyword arguments (Elisp emulation of cl-pdf's
`draw-bar-code128' &key arguments):

  :height            bar height in points (default 100)
  :width             total symbol width in points (default 400)
  :font              font name for the human readable text
                     (default \"Helvetica\")
  :font-size         its size in points (default 5)
  :start-stop-factor extra height fraction of the start/stop symbols
                     (default 0.3; their bars are drawn
                     HEIGHT * (1 + FACTOR) tall, extending upward)
  :show-string       draw the human readable text below the baseline
                     (default t)
  :color             fill color: nil/black, a color name, or (R G B)

The bars are filled rectangles rising upward from the baseline Y (the
anchor Y stays the bottom edge of the symbol); the charset is selected
automatically: code C for even-length all-digit strings, code B
otherwise.  Returns (list WIDTH HEIGHT) with WIDTH = the :width used.

Note: cl-pdf's original draws the bars downward from the anchor; this
port follows the niori-pdf convention of keeping the baseline at Y
with the bars extending upward, and the human readable text below."
  (let* ((height (or (plist-get keys :height) 100))
         (width (or (plist-get keys :width) 400))
         (font (or (plist-get keys :font) "Helvetica"))
         (font-size (or (plist-get keys :font-size) 5))
         (start-stop-factor (or (plist-get keys :start-stop-factor) 0.3))
         (show-string (if (plist-member keys :show-string)
                          (plist-get keys :show-string)
                        t))
         (color (niori-pdf-barcodes--color (plist-get keys :color)))
         (charset (niori-pdf-barcodes--code128-charset string))
         ;; 11 segments per code-B symbol, 5.5 per digit in code C
         ;; (2 digits share an 11-unit symbol), exactly as in cl-pdf.
         (segs-per-char (if (eq charset 'c) 5.5 11))
         (bars (car (niori-pdf-barcodes--code128-encode string charset)))
         (unit-w (niori-pdf-barcodes--code128-unit-width bars))
         (line-width (/ (float width) unit-w)))
    (unless niori-pdf--page
      (error "niori-pdf-barcodes: no current page (draw inside niori-pdf-with-page)"))
    (niori-pdf-with-saved-state
      (apply #'niori-pdf-set-rgb-fill color)
      (niori-pdf-set-line-width 0)
      ;; Bars: filled rectangles scaled so the symbol spans WIDTH.
      ;; The first and last pattern (start/stop symbols) are taller by
      ;; (1 + start-stop-factor).  cl-pdf advances with `translate'
      ;; per segment; we compute the same geometry with absolute
      ;; positions (identical result, fewer operators, no drift).
      (let ((n (length bars))
            (xcur (float x))
            (i 0))
        (dolist (bar bars)
          (let ((h (if (or (= i 0) (= i (1- n)))
                       (* height (+ 1 start-stop-factor))
                     height)))
            (dolist (seg bar)
              (let ((w (* seg line-width)))
                (when (> seg 0)
                  (niori-pdf-rectangle xcur y w h))
                (cl-incf xcur (abs w)))))
          (cl-incf i)))
      ;; paint the accumulated bar path (the `re' rectangles only
      ;; build the path; without `f' nothing would be drawn)
      (niori-pdf-fill)
      ;; Human readable characters spread evenly under the bars, the
      ;; first one 11 units in, one SEGS-PER-CHAR step per character.
      (when show-string
        (niori-pdf-in-text-mode
          (niori-pdf-set-font font font-size)
          (cl-loop for ch across string
                   for i from 0
                   do (niori-pdf-set-text-matrix
                       1 0 0 1
                       (+ x (* line-width (+ 11.0 (* i segs-per-char))))
                       (- y (float font-size)))
                      (niori-pdf-show-text (char-to-string ch))))))
    (list width height)))

(defun niori-pdf-barcode-code128-widths (string)
  "Return planning data for the Code 128 symbol of STRING as a plist:
  :width-units  total width of the symbol in Code 128 units
                (start + data + checksum + stop patterns)
  :charset      the auto-selected charset, `c' or `b'
  :checksum     the Code 128 check value (0..102)
Signals the same validation errors as `niori-pdf-barcode-code128'."
  (let* ((charset (niori-pdf-barcodes--code128-charset string))
         (enc (niori-pdf-barcodes--code128-encode string charset)))
    (list :width-units (niori-pdf-barcodes--code128-unit-width (car enc))
          :charset charset
          :checksum (cadr enc))))

;;;; Code 39 (new, small extension not present in cl-pdf)
;;
;; Standard Code 3 of 9: each character is 9 alternating elements
;; starting and ending with a bar (odd positions are bars, even ones
;; spaces); exactly 3 of the 9 elements are wide.  Characters are
;; separated by one narrow space.  The table below uses the classic
;; narrow/wide notation and is the standard published encoding (the
;; systematic structure - digits, A-J, K-T and U-Z+,*, space, -, .
;; mirroring the ten 2-of-5 bar pairs per space pattern - was verified
;; by decoding generated symbols with a zbar decoder).

(defconst niori-pdf-barcodes--code39-table
  '((?0 . "nnnwwnwnn") (?1 . "wnnwnnnnw") (?2 . "nnwwnnnnw")
    (?3 . "wnwwnnnnn") (?4 . "nnnwwnnnw") (?5 . "wnnwwnnnn")
    (?6 . "nnwwwnnnn") (?7 . "nnnwnnwnw") (?8 . "wnnwnnwnn")
    (?9 . "nnwwnnwnn") (?A . "wnnnnwnnw") (?B . "nnwnnwnnw")
    (?C . "wnwnnwnnn") (?D . "nnnnwwnnw") (?E . "wnnnwwnnn")
    (?F . "nnwnwwnnn") (?G . "nnnnnwwnw") (?H . "wnnnnwwnn")
    (?I . "nnwnnwwnn") (?J . "nnnnwwwnn") (?K . "wnnnnnnww")
    (?L . "nnwnnnnww") (?M . "wnwnnnnwn") (?N . "nnnnwnnww")
    (?O . "wnnnwnnwn") (?P . "nnwnwnnwn") (?Q . "nnnnnnwww")
    (?R . "wnnnnnwwn") (?S . "nnwnnnwwn") (?T . "nnnnwnwwn")
    (?U . "wwnnnnnnw") (?V . "nwwnnnnnw") (?W . "wwwnnnnnn")
    (?X . "nwnnwnnnw") (?Y . "wwnnwnnnn") (?Z . "nwwnwnnnn")
    (?- . "nwnnnnwnw") (?. . "wwnnnnwnn") (?  . "nwwnnnwnn")
    (?$ . "nwnwnwnnn") (?/ . "nwnwnnwnn") (?+ . "nwnnnwnwn")
    (?% . "nnnwnwnwn") (?* . "nwnnwnwnn"))
  "Code 39 encoding table: character -> 9-element narrow/wide pattern.
Element 1, 3, 5, 7, 9 are bars, 2, 4, 6, 8 are spaces; `n' is narrow,
`w' is wide (3 wide elements per character).  The `*' character is the
start/stop symbol and wraps every message.")

(defconst niori-pdf-barcodes--code39-wide-ratio 2.5
  "Ratio of wide to narrow Code 39 elements (spec allows 2.2..3.0).")

(defun niori-pdf-barcodes--code39-validate (string)
  "Signal `user-error' listing the characters of STRING that are not
part of the Code 39 alphabet."
  (let (bad)
    (dotimes (i (length string))
      (let ((ch (aref string i)))
        (unless (assq ch niori-pdf-barcodes--code39-table)
          (push ch bad))))
    (when bad
      (user-error
       "niori-pdf-barcodes: characters not encodable in Code 39 (alphabet 0-9 A-Z - . space $ / + %%): %s"
       (mapconcat (lambda (ch) (format "%c (0x%02X)" ch ch))
                  (nreverse bad) ", ")))))

(defun niori-pdf-barcodes--code39-units (string)
  "Return the signed unit widths of the Code 39 symbol for STRING
(wrapped in `*' start/stop characters).  Each element is (SIGN . UNIT)
with positive SIGN for bars and negative for spaces; UNIT is 1 for a
narrow element and `niori-pdf-barcodes--code39-wide-ratio' for a wide
one.  Characters are separated by one narrow gap (none after the last
stop character)."
  (niori-pdf-barcodes--code39-validate string)
  (let* ((msg (concat "*" string "*"))
         (units nil))
    (dotimes (ci (length msg))
      (let ((pat (cdr (assq (aref msg ci) niori-pdf-barcodes--code39-table))))
        (dotimes (ei 9)
          (push (cons (if (= (% ei 2) 0) 1 -1) ; odd positions are bars
                      (if (= (aref pat ei) ?w)
                          niori-pdf-barcodes--code39-wide-ratio
                        1))
                units))
        (unless (= ci (1- (length msg)))
          (push (cons -1 1) units))))        ; inter-character gap
    (nreverse units)))

(defun niori-pdf-barcodes--units-total (units)
  "Total (unsigned) width of the signed unit list UNITS."
  (let ((sum 0))
    (dolist (u units sum)
      (cl-incf sum (abs (cdr u))))))

(defun niori-pdf-barcode-code39 (string x y &rest keys)
  "Draw a Code 3 of 9 barcode of STRING with its baseline at (X Y).

This symbology is a small niori-pdf extension; it is not part of
cl-pdf's bar-codes.lisp.

KEYS is a plist:

  :height    bar height in points (default 50)
  :width     scale the whole symbol (including the surrounding
             `*...*' and inter-character gaps) to this width in
             points (default 200)
  :narrow    when non-nil, the narrow element width in points; the
             symbol is then NOT scaled to :width (a size of
             WIDTH = narrow * total-units results).  Takes
             precedence over :width when both are given.
  :show-text draw the message (without the `*' guards) centered
             under each character (default t)
  :font      font name (default \"Helvetica\")
  :font-size font size in points (default 8)
  :color     fill color: nil/black, a color name, or (R G B)

STRING is validated against the standard Code 39 alphabet
(\"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ- . $/+%\"); the `*' start/stop
characters are added automatically.  Returns (list WIDTH HEIGHT) with
WIDTH the width actually used."
  (let* ((height (or (plist-get keys :height) 50))
         (width (plist-get keys :width))
         (narrow (plist-get keys :narrow))
         (show-text (if (plist-member keys :show-text)
                        (plist-get keys :show-text)
                      t))
         (font (or (plist-get keys :font) "Helvetica"))
         (font-size (or (plist-get keys :font-size) 8))
         (color (niori-pdf-barcodes--color (plist-get keys :color)))
         (units (niori-pdf-barcodes--code39-units string))
         (total (niori-pdf-barcodes--units-total units))
         (unit (cond (narrow (float narrow))
                     (t (/ (float (or width 200)) total))))
         (used-width (* total unit)))
    (unless niori-pdf--page
      (error "niori-pdf-barcodes: no current page (draw inside niori-pdf-with-page)"))
    (niori-pdf-with-saved-state
      (apply #'niori-pdf-set-rgb-fill color)
      (let ((xcur (float x)))
        (dolist (u units)
          (let ((w (* (cdr u) unit)))
            (when (> (car u) 0)
              (niori-pdf-rectangle xcur y w height))
            (cl-incf xcur w))))
      ;; paint the accumulated bar path (`re' alone draws nothing)
      (niori-pdf-fill)
      (when show-text
        ;; each message character is centered in its cell: a character
        ;; is 6 narrow + 3 wide = 6 + 3*ratio units, followed by a
        ;; 1-unit gap (cell 0 is the leading `*')
        (niori-pdf-set-font font font-size)
        (let* ((cell-units (+ 6.0 (* 3.0 niori-pdf-barcodes--code39-wide-ratio)))
               (cell (* unit cell-units))
               (gap unit))
          (cl-loop for ch across string
                   for i from 0
                   do (niori-pdf-draw-centered-text
                       (+ x (* (+ cell gap) (1+ i)) (/ cell 2.0))
                       (- y (float font-size))
                       (char-to-string ch))))))
    (list used-width height)))

;;;; EAN-13 (new, small extension not present in cl-pdf)
;;
;; Standard GS1 EAN-13: 95 modules = 3 (left guard 101) + 6 * 7 + 5
;; (center 01010) + 6 * 7 + 3 (right guard 101).  The first digit is
;; encoded by the odd/even (L/G) parity pattern of the left half; the
;; right half always uses R codes.  The L/G/R digit codes below are
;; the standard GS1 tables; the first-digit parity table was verified
;; against the reportlab implementation and by decoding generated
;; symbols with a zbar decoder.

(defconst niori-pdf-barcodes--ean13-l
  ["0001101" "0011001" "0010011" "0111101" "0100011"
   "0110001" "0101111" "0111011" "0110111" "0001011"]
  "EAN-13 odd-parity (L) 7-module codes for digits 0-9 (left half).")

(defconst niori-pdf-barcodes--ean13-g
  ["0100111" "0110011" "0011011" "0100001" "0011101"
   "0111001" "0000101" "0010001" "0001001" "0010111"]
  "EAN-13 even-parity (G) 7-module codes for digits 0-9 (left half).")

(defconst niori-pdf-barcodes--ean13-r
  ["1110010" "1100110" "1101100" "1000010" "1011100"
   "1001110" "1010000" "1000100" "1001000" "1110100"]
  "EAN-13 right-half (R) 7-module codes for digits 0-9.")

(defconst niori-pdf-barcodes--ean13-parity
  ["LLLLLL" "LLGLGG" "LLGGLG" "LLGGGL" "LGLLGG"
   "LGGLLG" "LGGGLL" "LGLGLG" "LGLGGL" "LGGLGL"]
  "EAN-13 first-digit parity patterns (L=odd, G=even), digits 0-9.")

(defun niori-pdf-barcodes--ean13-check-digit (digits12)
  "Check digit of the 12-digit string DIGITS12 (weights 1,3 alternating;
equivalently GS1's odd/even positions from the right)."
  (let ((sum 0))
    (cl-loop for i from 0
             for ch across digits12
             do (cl-incf sum (* (- ch ?0) (if (cl-evenp i) 1 3))))
    (mod (- 10 (mod sum 10)) 10)))

(defun niori-pdf-barcodes--ean13-normalize (string)
  "Validate STRING as 12 or 13 digits and return the full 13-digit
number (computing the check digit when 12 digits are given)."
  (unless (and (memq (length string) '(12 13))
               (string-match-p "\\`[0-9]+\\'" string))
    (user-error "niori-pdf-barcodes: EAN-13 requires a 12- or 13-digit string: %S"
                string))
  (if (= (length string) 13)
      string
    (concat string (number-to-string
                    (niori-pdf-barcodes--ean13-check-digit string)))))

(defun niori-pdf-barcodes--ean13-modules (s13)
  "Return the 95-module 0/1 string for the 13-digit number S13."
  (let* ((par (aref niori-pdf-barcodes--ean13-parity (- (aref s13 0) ?0)))
         (out "101"))
    (cl-loop for i from 1 to 6
             do (let* ((d (- (aref s13 i) ?0))
                       (code (if (= (aref par (1- i)) ?L)
                                 (aref niori-pdf-barcodes--ean13-l d)
                               (aref niori-pdf-barcodes--ean13-g d))))
                  (setq out (concat out code))))
    (setq out (concat out "01010"))
    (cl-loop for i from 7 to 12
             do (setq out (concat out
                                  (aref niori-pdf-barcodes--ean13-r
                                        (- (aref s13 i) ?0)))))
    (concat out "101")))

(defun niori-pdf-barcodes--ean13-guard-p (i)
  "Non-nil when module I of an EAN-13 symbol belongs to a guard bar
pattern (left guard 0-2, center 45-49, right guard 92-94)."
  (or (<= i 2)
      (and (>= i 45) (<= i 49))
      (>= i 92)))

(defun niori-pdf-barcode-ean13 (string x y &rest keys)
  "Draw an EAN-13 barcode with its baseline at (X Y).

This symbology is a small niori-pdf extension; it is not part of
cl-pdf's bar-codes.lisp.

STRING is a 12- or 13-digit string; when 12 digits are given the
check digit is computed automatically (weights 1,3 alternating).
When all 13 digits are supplied the first digit is also printed
left of the bars.

KEYS is a plist:

  :height    bar height in points above the baseline (default 60)
  :width     total symbol width in points (95 modules; default 190)
  :show-text draw the human readable digits (default t): groups of 6
             centered under the left and right halves, in the space
             the guard bars descend into, plus the first digit left
             of the bars for 13-digit input
  :font      font name (default \"Helvetica\"); the size is scaled
             to 7 modules
  :color     fill color: nil/black, a color name, or (R G B)

The three guard patterns (101, 01010, 101) extend 5 modules below the
baseline (5 * WIDTH/95 points), as in standard EAN artwork.  Returns
\(list WIDTH HEIGHT)."
  (let* ((height (or (plist-get keys :height) 60))
         (width (or (plist-get keys :width) 190))
         (show-text (if (plist-member keys :show-text)
                        (plist-get keys :show-text)
                      t))
         (font (or (plist-get keys :font) "Helvetica"))
         (color (niori-pdf-barcodes--color (plist-get keys :color)))
         (full (niori-pdf-barcodes--ean13-normalize string))
         (module (/ (float width) 95))
         (descent (* 5 module))
         (font-size (* 7 module))
         (modules (niori-pdf-barcodes--ean13-modules full)))
    (unless niori-pdf--page
      (error "niori-pdf-barcodes: no current page (draw inside niori-pdf-with-page)"))
    (niori-pdf-with-saved-state
      (apply #'niori-pdf-set-rgb-fill color)
      ;; Dark runs of one guard kind are emitted as one rectangle per
      ;; run, then a single fill for the whole symbol.
      (let ((i 0))
        (while (< i 95)
          (if (= (aref modules i) ?1)
              (let* ((run-start i)
                     (run-guard (niori-pdf-barcodes--ean13-guard-p i)))
                (while (and (< i 95)
                            (= (aref modules i) ?1)
                            (eq (niori-pdf-barcodes--ean13-guard-p i) run-guard))
                  (cl-incf i))
                (let ((x0 (+ x (* run-start module)))
                      (w (* (- i run-start) module)))
                  (if run-guard
                      (niori-pdf-rectangle
                       x0 (- y descent) w (+ height descent))
                    (niori-pdf-rectangle x0 y w height))))
            (cl-incf i)))
        (niori-pdf-fill))
      (when show-text
        (niori-pdf-set-font font font-size)
        (let ((ty (- y descent))
              (first-digit (= (length string) 13)))
          (when first-digit
            (niori-pdf-draw-right-text (- x (* 3 module)) ty
                                       (substring full 0 1)))
          (let ((s (if first-digit 1 0)))
            (niori-pdf-draw-centered-text
             (+ x (* 24 module)) ty (substring full s (+ s 6)))
            (niori-pdf-draw-centered-text
             (+ x (* 71 module)) ty (substring full 7 13))))))
    (list width height)))

;;;; QR matrix drawing (port of cl-pdf `draw-qr-code')

(defun niori-pdf-barcodes--qr-dark-p (v)
  "Non-nil when matrix element V counts as a dark module.
Accepts 1, t, :dark and :fdark (the latter two for compatibility with
cl-qrencode symbol data, as in cl-pdf)."
  (or (eq v t)
      (eq v :dark)
      (eq v :fdark)
      (and (numberp v) (= v 1))))

(defun niori-pdf-barcode-qr (matrix x y &rest keys)
  "Draw a square 2D MATRIX at (X Y) as a QR-style module symbol.

Direct port of cl-pdf's `draw-qr-code'.  MATRIX is a list of lists
\(square, N x N) whose elements are dark/light; 1, t, :dark and :fdark
count as dark, everything else (0, nil, :light, ...) as light.
KEYS is a plist:

  :point-size size of one module in points (default 2)
  :color      fill color: nil/black, a color name, or (R G B)

The y axis runs downward from Y, exactly as in cl-pdf (row 0 is drawn
just below the anchor).  Dark runs are run-length encoded per row to
keep the content stream small.  Returns (list WIDTH HEIGHT), each
N * POINT-SIZE.

Note: cl-pdf advances one module to the right before drawing the
first row, shifting the whole symbol one module to the right; this
port draws column 0 at X so the drawing spans exactly the size the
return value announces."
  (let* ((point-size (or (plist-get keys :point-size) 2))
         (color (niori-pdf-barcodes--color (plist-get keys :color)))
         (size (length matrix)))
    (dolist (row matrix)
      (unless (= (length row) size)
        (user-error "niori-pdf-barcodes: QR matrix is not square (%d rows of %d columns)"
                    size (length row))))
    (unless niori-pdf--page
      (error "niori-pdf-barcodes: no current page (draw inside niori-pdf-with-page)"))
    (niori-pdf-with-saved-state
      (apply #'niori-pdf-set-rgb-fill color)
      (niori-pdf-translate x y)
      ;; Axis goes down!
      (niori-pdf-scale point-size (- point-size))
      (cl-loop for r from 0
               for row in matrix
               do (let ((c 0))
                    (while (< c size)
                      (if (niori-pdf-barcodes--qr-dark-p (elt row c))
                          (let ((start c))
                            (while (and (< c size)
                                        (niori-pdf-barcodes--qr-dark-p
                                         (elt row c)))
                              (cl-incf c))
                            ;; one rectangle per dark run, one fill per
                            ;; row; x = the column, y = the row so the
                            ;; matrix is drawn untransposed (row 0 at
                            ;; the anchor, running downward)
                            (niori-pdf-rectangle start r (- c start) 1))
                        (cl-incf c)))
                    (niori-pdf-fill))))
    (list (* size point-size) (* size point-size))))

(provide 'niori-pdf-barcodes)
;;; niori-pdf-barcodes.el ends here
