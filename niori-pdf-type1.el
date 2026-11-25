;;; niori-pdf-type1.el --- Type 1 font (.pfb/.pfa) parsing and embedding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev.
;;
;; Original Emacs Lisp implementation of Type 1 font parsing and
;; embedding.  See LICENSE for the license applicable to this file.

;;; Commentary:

;; Parses Adobe Type 1 font programs (.pfb binaries and .pfa ASCII
;; files) in pure Elisp and embeds them as PDF /Subtype /Type1 fonts
;; with a /FontFile stream, WinAnsiEncoding and a /Widths array.
;;
;; The parser follows the Adobe Type 1 Font Format specification:
;;  * .pfb segment structure (0x80 0x01 ASCII / 0x80 0x02 binary /
;;    0x80 0x03 EOF markers)
;;  * .pfa: clear text up to the "eexec" line, then hex-encoded binary
;;  * eexec decryption (key 55665) of the private dictionary
;;  * charstring decryption (key 4330) and advance-width extraction
;;    from the first two operands of each /hsbw charstring
;;
;; The extracted advance widths become the PDF /Widths array (mapped
;; through WinAnsiEncoding glyph names) and the measurement metrics,
;; so no companion .afm file is required.  Fonts are embedded whole
;; (no subsetting -- Type 1 fonts are small and subsetting a Type 1
;; program means rewriting the private dictionary, which is out of
;; scope).

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)

;;;; Byte helpers

(defun niori-pdf-type1--u16le (b i)
  (+ (aref b i) (* 256 (aref b (1+ i)))))

(defun niori-pdf-type1--u32le (b i)
  (+ (aref b i) (* 256 (aref b (1+ i)))
     (* 65536 (aref b (+ i 2))) (* 16777216 (aref b (+ i 3)))))

(defun niori-pdf-type1--s32be (b i)
  "Signed big-endian 32-bit integer at I in B."
  (let ((v (+ (* (aref b i) 16777216) (* (aref b (1+ i)) 65536)
              (* (aref b (+ i 2)) 256) (aref b (+ i 3)))))
    (if (>= v 2147483648) (- v 4294967296) v)))

;;;; Type 1 decryption (Adobe Type 1 spec, section 7.1)

(defconst niori-pdf-type1--c1 52845)
(defconst niori-pdf-type1--c2 22719)

(defun niori-pdf-type1--decrypt (cipher r &optional skip)
  "Decrypt the eexec/charstring-encrypted CIPHER bytes with key R.
The first SKIP decrypted bytes (the random leading bytes mandated by
the Type 1 format) are dropped from the result."
  (let* ((n (length cipher))
         (out (make-string n 0))
         (kept 0))
    (dotimes (i n)
      (let* ((c (aref cipher i))
             (plain (logxor c (% (/ r 256) 256))))
        (setq r (% (+ (* (+ c r) niori-pdf-type1--c1)
                      niori-pdf-type1--c2)
                   65536))
        (when (>= i (or skip 0))
          (aset out kept (logand plain 255))
          (setq kept (1+ kept)))))
    (substring out 0 kept)))

;;;; PFB / PFA container parsing

(cl-defstruct niori-pdf-type1-file
  font-name bbox italic-angle fixed-pitch stemv
  widths-hash ascender descender cap-height
  fontfile-bytes length1 length2 length3)

(defun niori-pdf-type1--read-pfb (bytes)
  "Split PFB segment BYTES into (CLEAR ENCRYPTED TRAILER).
PFB layout: one ASCII segment holding the clear text up to the
\"eexec\" line, one or more binary segments holding the eexec-
encrypted private dictionary and charstrings, and a final short
ASCII segment with the 512-byte hex trailer (\"cleartomark\").  For
the PDF /FontFile stream the parts map to Length1/Length2/Length3,
so the trailer is kept separately -- merging it into the clear part
would break every Type 1 parser."
  (let ((pos 0) (n (length bytes))
        clear enc trailer)
    (while (< pos n)
      (unless (and (<= (+ pos 2) n) (= (aref bytes pos) 128))
        (error "niori-pdf: corrupt .pfb segment header"))
      (let ((seg-type (aref bytes (1+ pos))))
        (cond
         ((= seg-type 3)           ; EOF marker
          (setq pos n))
         ((or (= seg-type 1) (= seg-type 2))
          (let* ((len (niori-pdf-type1--u32le bytes (+ pos 2)))
                 (start (+ pos 6))
                 (end (+ start len)))
            (when (> end n)
              (error "niori-pdf: truncated .pfb segment"))
            (cond
             ((= seg-type 1)
              (if clear
                  ;; a second ASCII segment: the post-eexec trailer
                  (setq trailer (append trailer
                                        (list (substring bytes start end))))
                (setq clear (substring bytes start end))))
             (t
              (setq enc (append enc (list (substring bytes start end))))))
            (setq pos end)))
         (t
          (error "niori-pdf: unknown .pfb segment type %d" seg-type)))))
    (list (or clear "")
          (apply #'concat (or enc '("")))
          (apply #'concat (or trailer '(""))))))

(defun niori-pdf-type1--read-pfa (bytes)
  "Split PFA (ASCII hex) BYTES into (CLEAR ENCRYPTED TRAILER).
The clear part ends after the \"eexec\" keyword line; the binary
part is the hex-encoded section that follows.  The PFA trailer
(lines after the last hex line) is returned as the third element; a
PFA already includes its 512-hex-zero trailer inside the hex
section, so Length3 is conventionally 0 for PFA sources."
  (let* ((text bytes)
         (m (string-match "\neexec[ \t\r]*\n" text)))
    (unless m
      (error "niori-pdf: .pfa file has no eexec section"))
    (let* ((clear (substring text 0 (match-end 0)))
           (rest (substring text (match-end 0)))
           (hex (niori-pdf-type1--pfa-hex rest)))
      (list clear (niori-pdf-type1--hex-to-bytes hex) ""))))

(defun niori-pdf-type1--pfa-hex (text)
  "Collect the hex digits of the eexec section starting at TEXT.
Stops at the first line that is not pure hex (the \\\"0000\\\" end
marker is consumed when it is the terminating short line, the
standard PFA writer's convention)."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((hex (make-string 0 0)))
      (while (not (eobp))
        (let ((line (buffer-substring
                     (line-beginning-position) (line-end-position))))
          (setq line (replace-regexp-in-string "[ \t\r]" "" line))
          (if (and (> (length line) 0)
                   (string-match-p "\\`[0-9A-Fa-f]+\\'" line))
              (progn
                (setq hex (concat hex line))
                (forward-line 1))
            ;; a non-hex line (cleartomark, end, ...) terminates the
            ;; section -- unless it is the leading "0000" sentinel,
            ;; which is hex and already handled above
            (goto-char (point-max)))))
      hex)))

(defun niori-pdf-type1--hex-digit (c)
  "Hex value of character C, or nil."
  (cond
   ((and (>= c ?0) (<= c ?9)) (- c 48))
   ((and (>= c ?A) (<= c ?F)) (- c 55))
   ((and (>= c ?a) (<= c ?f)) (- c 87))
   (t nil)))

(defun niori-pdf-type1--hex-to-bytes (hex)
  "Decode the hex string HEX to a unibyte byte string."
  (let* ((n (/ (length hex) 2))
         (out (make-string n 0)))
    (dotimes (i n)
      (let ((hi (niori-pdf-type1--hex-digit (aref hex (* 2 i))))
            (lo (niori-pdf-type1--hex-digit (aref hex (1+ (* 2 i))))))
        (unless (and hi lo)
          (error "niori-pdf: invalid hex in .pfa"))
        (aset out i (+ (* 16 hi) lo))))
    out))

;;;; Clear-text and private dictionary extraction

(defun niori-pdf-type1--parse-number (s)
  "Parse the PostScript number string S (integer or real)."
  (if (string-match-p "\\`-?[0-9]+\\'" s)
      (string-to-number s)
    (string-to-number s)))

(defun niori-pdf-type1--find-number (text key)
  "Return the number following KEY in the clear text TEXT, or nil.
Handles both `/Key 109 def' and `/Key [109]' array forms."
  (let ((m (string-match
            (concat (regexp-quote key) "[ \t\n\r]+\\[?"
                    "[ \t\n\r]*\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)")
            text)))
    (when m
      (niori-pdf-type1--parse-number (match-string 1 text)))))

(defun niori-pdf-type1--find-name (text key)
  "Return the name following KEY in the clear text TEXT, or nil."
  (let ((m (string-match
            (concat (regexp-quote key) "[ \t\n\r]+/\\([^ \t\n\r/(){}<>*]+\\)")
            text)))
    (when m
      (match-string 1 text))))

(defun niori-pdf-type1--find-bbox (text)
  "Return the /FontBBox of clear text TEXT as a 4-element list."
  (let ((m (string-match
            "/FontBBox[ \t\n\r]*{[ \t\n\r]*\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)[ \t\n\r]+\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)[ \t\n\r]+\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)[ \t\n\r]+\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)"
            text)))
    (when m
      (mapcar (lambda (k) (niori-pdf-type1--parse-number
                           (match-string k text)))
              '(1 2 3 4)))))

(defun niori-pdf-type1--charstring-numbers (bytes pos limit)
  "Decode Type 1 charstring number operands starting at POS in BYTES.
Returns (VALUES POS) where VALUES are the numbers decoded before the
first operator byte (0-31) or until LIMIT operands were read."
  (let ((vals '()))
    (while (and (< (length vals) (or limit 999)) (< pos (length bytes)))
      (let ((b (aref bytes pos)))
        (cond
         ((<= 32 b 246)
          (push (- b 139) vals)
          (setq pos (1+ pos)))
         ((<= 247 b 250)
          (push (+ (* (- b 247) 256) (aref bytes (1+ pos)) 108) vals)
          (setq pos (+ pos 2)))
         ((<= 251 b 254)
          (push (- (- (* (- b 251) 256) (aref bytes (1+ pos))) 108) vals)
          (setq pos (+ pos 2)))
         ((= b 255)
          (push (/ (niori-pdf-type1--s32be bytes (1+ pos)) 65536.0) vals)
          (setq pos (+ pos 5)))
         (t                        ; operator byte: stop
          (setq pos (length bytes))))))
    (list (nreverse vals) pos)))

(defun niori-pdf-type1--scan-charstrings (private)
  "Extract glyph name -> advance width from PRIVATE dict bytes.
PRIVATE is the decrypted eexec section.  Each /CharStrings entry is
/name LEN RD <binary> NP/ND; decrypting the binary charstring with
key 4330 yields <sbx wx hsbw> operands, so the advance width is the
second operand of the first operator's argument list."
  (let ((map (make-hash-table :test #'equal))
        (pos 0)
        (rx "/\\([!-~]+\\)[ \t\n\r]+\\([0-9]+\\)[ \t\n\r]+\\(?:RD\\|-|\\)[ \t\n\r]"))
    ;; restrict scanning to the region after /CharStrings when present
    ;; (scanning to the very end is safe: entry payloads are skipped,
    ;; so no binary byte sequence can produce a false match)
    (let ((cs (string-match "/CharStrings" private)))
      (when cs
        (setq pos (match-end 0))))
    (while (setq pos (string-match rx private pos))
      (let* ((name (match-string 1 private))
             (len (string-to-number (match-string 2 private)))
             (start (match-end 0))
             (payload (and (>= (length private) (+ start len))
                           (substring private start (+ start len)))))
        (setq pos (+ start len))
        (when payload
          (let* ((plain (niori-pdf-type1--decrypt payload 4330 4))
                 (nums (car (niori-pdf-type1--charstring-numbers plain 0 3))))
            (when (>= (length nums) 2)
              ;; hsbw takes (sbx wx): the advance width is the 2nd operand
              (puthash name
                       (round (nth 1 nums))
                       map))))))
    map))

;;;; Font file reading

(defun niori-pdf-type1-read-file (path)
  "Read and parse the Type 1 font program at PATH (.pfb or .pfa)."
  (let* ((bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally path)
                  (buffer-string)))
         (parts (if (and (>= (length bytes) 2)
                         (= (aref bytes 0) 128)
                         (memq (aref bytes 1) '(1 2)))
                    (niori-pdf-type1--read-pfb bytes)
                  (niori-pdf-type1--read-pfa bytes)))
         (clear (nth 0 parts))
         (enc (nth 1 parts))
         (trailer (nth 2 parts))
         (private (niori-pdf-type1--decrypt enc 55665 4))
         (name (or (niori-pdf-type1--find-name clear "/FontName")
                   (file-name-nondirectory path)))
         (bbox (niori-pdf-type1--find-bbox clear))
         (italic (or (niori-pdf-type1--find-number clear "/ItalicAngle") 0))
         (fixed-pitch (string-match-p "isFixedPitch[ \t\n\r]+true" private))
         (widths (niori-pdf-type1--scan-charstrings private))
         (stemv (or (niori-pdf-type1--find-number private "/StdVW") 80))
         (cap (or (niori-pdf-type1--find-number private "/CapHeight") nil))
         (ascent (niori-pdf-type1--afm-number path "Ascender"))
         (descent (niori-pdf-type1--afm-number path "Descender"))
         (afm-cap (niori-pdf-type1--afm-number path "CapHeight"))
         (ymax (if bbox (nth 3 bbox) 700))
         (ymin (if bbox (nth 1 bbox) -200)))
    (make-niori-pdf-type1-file
     :font-name name
     :bbox bbox
     :italic-angle italic
     :fixed-pitch (and fixed-pitch t)
     :stemv stemv
     :widths-hash widths
     :ascender (or ascent ymax)
     :descender (or descent ymin)
     :cap-height (or cap afm-cap 700)
     :fontfile-bytes (concat clear enc trailer)
     :length1 (length clear)
     :length2 (length enc)
     :length3 (length trailer))))

(defun niori-pdf-type1--afm-number (path key)
  "Read the global font info KEY from the .afm file next to PATH."
  (condition-case nil
      (let* ((base (file-name-sans-extension path))
             (afm (cond
                   ((file-exists-p (concat base ".afm"))
                    (concat base ".afm"))
                   ((file-exists-p (concat base ".AFM"))
                    (concat base ".AFM")))))
        (when afm
          (with-temp-buffer
            (insert-file-contents afm)
            (let ((m (string-match
                      (concat "^" (regexp-quote key)
                              "[ \t]+\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)")
                      (buffer-string))))
              (when m
                (niori-pdf-type1--parse-number
                 (match-string 1 (buffer-string))))))))
    (error nil)))

;;;; WinAnsiEncoding byte -> glyph names (PDF 32000-1 Annex D)

(defconst niori-pdf-type1--winansi-glyphs
  (concat
   "space exclam quotedbl numbersign dollar percent ampersand quoteright "
   "parenleft parenright asterisk plus comma hyphen period slash zero one "
   "two three four five six seven eight nine colon semicolon less equal "
   "greater question at A B C D E F G H I J K L M N O P Q R S T U V W X Y "
   "Z bracketleft backslash bracketright asciicircum underscore quoteleft "
   "a b c d e f g h i j k l m n o p q r s t u v w x y z braceleft bar "
   "braceright asciitilde "
   ;; 127-159: WinAnsi specials (undefined bytes -> nil placeholder)
   "Euro nil quotesinglbase florin quotedblbase ellipsis dagger daggerdbl "
   "circumflex perthousand Scaron guilsinglleft OE nil Zcaron nil nil "
   "quoteleft quoteright quotedblleft quotedblright bullet endash emdash "
   "tilde trademark scaron guilsinglright oe nil zcaron Ydieresis "
   ;; 160-255
   "space exclamdown cent sterling currency yen brokenbar section "
   "dieresis copyright ordfeminine guillemotleft logicalnot hyphen "
   "registered macron degree plusminus twosuperior threesuperior acute "
   "mu paragraph periodcentered cedilla onesuperior ordmasculine "
   "guillemotright onequarter onehalf threequarters questiondown "
   "Agrave Aacute Acircumflex Atilde Adieresis Aring AE Ccedilla Egrave "
   "Eacute Ecircumflex Edieresis Igrave Iacute Icircumflex Idieresis "
   "Eth Ntilde Ograve Oacute Ocircumflex Otilde Odieresis multiply "
   "Oslash Ugrave Uacute Ucircumflex Udieresis Yacute Thorn germandbls "
   "agrave aacute acircumflex atilde adieresis aring ae ccedilla egrave "
   "eacute ecircumflex edieresis igrave iacute icircumflex idieresis "
   "eth ntilde ograve oacute ocircumflex otilde odieresis divide "
   "oslash ugrave uacute ucircumflex udieresis yacute thorn ydieresis")
  "Glyph names of WinAnsiEncoding codes 32..255, space separated.")

(defconst niori-pdf-type1--winansi-vector
  (let ((v (make-vector 256 nil)))
    (let ((code 32)
          (names (split-string niori-pdf-type1--winansi-glyphs)))
      (dolist (nm names)
        (unless (string= nm "nil")
          (aset v code nm))
        (setq code (1+ code))))
    v)
  "Vector mapping byte codes to WinAnsiEncoding glyph names.")

;;;; Document font wrapper

(declare-function niori-pdf--register-font-resource "niori-pdf-fonts" (doc font))

(defun niori-pdf-type1--metrics (t1)
  "Build the measurement metrics plist for parsed Type 1 font T1.
Same layout as the base-14 metrics so the shared width code in
`niori-pdf-fonts' can be reused."
  (let* ((wh (niori-pdf-type1-file-widths-hash t1))
         (w (make-vector 224 0))
         (space (or (gethash "space" wh) 250)))
    (dotimes (b 224)
      (let* ((code (+ b 32))
             (nm (aref niori-pdf-type1--winansi-vector code)))
        (when nm
          (aset w b (or (gethash nm wh) 0)))))
    (list :widths w
          :extra-widths (list (cons 9 space) (cons 10 space)
                              (cons 13 space))
          :ascender (niori-pdf-type1-file-ascender t1)
          :descender (niori-pdf-type1-file-descender t1)
          :capheight (niori-pdf-type1-file-cap-height t1))))

(defun niori-pdf-type1--font-props (t1 path)
  "Props plist for parsed Type 1 font T1 loaded from PATH."
  (list :type1 t1
        :file-path (expand-file-name path)
        :metrics (niori-pdf-type1--metrics t1)))

(defun niori-pdf-type1-make-font (doc path)
  "Load the Type 1 font PATH and register it in DOC.
Creates the /Type1 font dictionary with /Widths, /Encoding
/WinAnsiEncoding, a /FontDescriptor and an uncompressed /FontFile
stream (Length1/Length2/Length3), per ISO 32000-1 9.6.2.4."
  (let* ((t1 (niori-pdf-type1-read-file (expand-file-name path)))
         (name (niori-pdf-type1-file-font-name t1))
         (font (niori-pdf-make-font :kind 'type1 :name path :doc doc))
         (ff-dict (niori-pdf-dict))
         (ff-stream (niori-pdf-stream :dict ff-dict
                                      :data (niori-pdf-type1-file-fontfile-bytes
                                             t1)))
         (ff-ref (niori-pdf-add-object doc ff-stream))
         (flags (+ 32                   ; nonsymbolic
                   (if (niori-pdf-type1-file-fixed-pitch t1) 1 0)
                   (if (/= (niori-pdf-type1-file-italic-angle t1) 0) 64 0)))
         (bbox (niori-pdf-type1-file-bbox t1))
         (fd (niori-pdf-dict))
         (fd-ref (niori-pdf-add-object doc fd))
         (dict (niori-pdf-dict))
         (wh (niori-pdf-type1-file-widths-hash t1)))
    ;; FontFile stream (uncompressed; Length3 is always 0 for raw Type 1)
    (niori-pdf-dict-set ff-dict 'Length1
                        (niori-pdf-type1-file-length1 t1))
    (niori-pdf-dict-set ff-dict 'Length2
                        (niori-pdf-type1-file-length2 t1))
    (niori-pdf-dict-set ff-dict 'Length3
                        (niori-pdf-type1-file-length3 t1))
    (niori-pdf-dict-set ff-dict 'Length
                        (length (niori-pdf-type1-file-fontfile-bytes t1)))
    ;; FontDescriptor
    (niori-pdf-dict-set fd 'Type 'FontDescriptor)
    (niori-pdf-dict-set fd 'FontName (intern name))
    (niori-pdf-dict-set fd 'Flags flags)
    (niori-pdf-dict-set fd 'FontBBox
                        (apply #'niori-pdf-array
                               (or bbox '(0 -200 1000 900))))
    (niori-pdf-dict-set fd 'ItalicAngle
                        (niori-pdf-type1-file-italic-angle t1))
    (niori-pdf-dict-set fd 'Ascent (niori-pdf-type1-file-ascender t1))
    (niori-pdf-dict-set fd 'Descent (niori-pdf-type1-file-descender t1))
    (niori-pdf-dict-set fd 'CapHeight (niori-pdf-type1-file-cap-height t1))
    (niori-pdf-dict-set fd 'StemV (niori-pdf-type1-file-stemv t1))
    (niori-pdf-dict-set fd 'FontFile ff-ref)
    ;; Font dictionary
    (niori-pdf-dict-set dict 'Type 'Font)
    (niori-pdf-dict-set dict 'Subtype 'Type1)
    (niori-pdf-dict-set dict 'BaseFont (intern name))
    (niori-pdf-dict-set dict 'FirstChar 32)
    (niori-pdf-dict-set dict 'LastChar 255)
    (let ((metrics (niori-pdf-type1--metrics t1)))
      (niori-pdf-dict-set
       dict 'Widths
       (apply #'niori-pdf-array
              (mapcar (lambda (w) (round w))
                      (append (plist-get metrics :widths) nil))))
      (setf (niori-pdf-font-props font)
            (niori-pdf-type1--font-props t1 path))
      ;; keep the widths computed once
      (setf (plist-get (niori-pdf-font-props font) :metrics) metrics))
    (niori-pdf-dict-set dict 'Encoding 'WinAnsiEncoding)
    (niori-pdf-dict-set dict 'FontDescriptor fd-ref)
    (niori-pdf--register-font-resource doc font)
    (setf (niori-pdf-font-ref font) (niori-pdf-add-object doc dict))
    font))

(provide 'niori-pdf-type1)
;;; niori-pdf-type1.el ends here
