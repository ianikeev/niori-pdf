;;; niori-pdf-fonts.el --- Base-14 fonts, WinAnsi encoding, text metrics -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2009 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; Portions of this file are derived from CL-PDF font handling.
;; See LICENSE for details.

;;; Commentary:

;; Base-14 standard fonts (Helvetica, Times, Courier families, Symbol,
;; ZapfDingbats) with real AFM metrics, WinAnsiEncoding / native Symbol
;; and ZapfDingbats text encoding, AFM kerning pairs and text
;; measurement.  TrueType/OpenType/collection fonts are handled by
;; niori-pdf-ttf.el; Type 1 fonts (.pfb/.pfa) by niori-pdf-type1.el.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-metrics)
(require 'niori-pdf-ttf)
(require 'niori-pdf-type1)

(defconst niori-pdf-fonts--winansi-special-hash
  (let ((h (make-hash-table :test #'eql)))
    (dolist (kv niori-pdf-metrics--winansi-specials h)
      (puthash (car kv) (cdr kv) h)))
  "Unicode codepoint -> WinAnsi byte for the non-Latin-1 range.")

(defconst niori-pdf-fonts--metrics-hash
  (let ((h (make-hash-table :test #'equal)))
    (dolist (kv niori-pdf-metrics--fonts h)
      (puthash (car kv) (cdr kv) h)))
  "Font name -> metrics plist.")

(defconst niori-pdf-fonts--native-uni-hash
  (let ((h (make-hash-table :test #'equal)))
    (dolist (kv niori-pdf-metrics--native-uni h)
      (let ((fh (make-hash-table :test #'eql)))
        (dolist (pair (cdr kv))
          (puthash (car pair) (cdr pair) fh))
        (puthash (car kv) fh h))))
  "Font name -> hash Unicode codepoint -> native code (Symbol/ZapfDingbats).")

(defconst niori-pdf-fonts--kerning-data
  (let ((h (make-hash-table :test #'equal)))
    (dolist (kv niori-pdf-metrics--kerning h)
      (puthash (car kv) (cdr kv) h)))
  "Font name -> list of (C1 (C2 . DX) ...) kern groups, units/1000 em.")

(defun niori-pdf-fonts-metrics (name)
  "Return the metrics plist for base-14 font NAME, or nil."
  (gethash name niori-pdf-fonts--metrics-hash))

(defun niori-pdf-fonts-known-p (name)
  "Return t when NAME is one of the base-14 fonts."
  (and (niori-pdf-fonts-metrics name) t))

;;;; WinAnsi encoding

(defun niori-pdf-encode-winansi (string)
  "Encode multibyte STRING as a WinAnsiEncoding byte string (unibyte).
Characters without a WinAnsi mapping become `?'."
  (let* ((n (length string)) (out (make-string n 0)))
    (dotimes (i n)
      (let* ((c (aref string i))
             (b (cond
                 ((and (>= c 32) (<= c 126)) c)
                 ((and (>= c 160) (<= c 255)) c)
                 ((= c 9) 9) ((= c 10) 10) ((= c 13) 13)
                 (t (gethash c niori-pdf-fonts--winansi-special-hash)))))
        (aset out i (or b 63))))
    (encode-coding-string out 'binary)))

;;;; Symbol / ZapfDingbats native encoding

(defun niori-pdf-encode-native (string font)
  "Encode STRING in FONT's native Symbol/ZapfDingbats encoding.
Characters without a mapping in the font become `?'.  Note that the
Symbol encoding has no Latin letters (codes 65..90 are the Greek
capitals); only characters listed in the font's Unicode map survive."
  (let* ((uni-map (gethash (niori-pdf-font-name font)
                           niori-pdf-fonts--native-uni-hash))
         (n (length string)) (out (make-string n 0)))
    (dotimes (i n)
      (let ((c (aref string i)))
        (aset out i (or (and uni-map (gethash c uni-map)) 63))))
    (encode-coding-string out 'binary)))

;;;; Kerning (AFM KPX pairs)

(defun niori-pdf-fonts--kern-hash (font-name)
  "Hash of the kern pairs of base-14 font FONT-NAME, or nil.
Key is (+ (* C1 256) C2), value is DX in units/1000 em."
  (let ((groups (gethash font-name niori-pdf-fonts--kerning-data)))
    (when groups
      (let ((h (make-hash-table :test #'eql)))
        (dolist (g groups h)
          (let ((c1 (car g)))
            (dolist (pair (cdr g))
              (puthash (+ (* c1 256) (car pair)) (cdr pair) h))))))))

(defun niori-pdf-font-kern (font c1 c2)
  "Kerning adjustment between chars C1 and C2 of FONT, units/1000 em.
Returns 0 when the font has no kerning data for the pair."
  (pcase (niori-pdf-font-kind font)
    (`base14
     (let ((h (or (plist-get (niori-pdf-font-props font) :kern-hash)
                  (let ((h (niori-pdf-fonts--kern-hash
                            (niori-pdf-font-name font))))
                    (setf (plist-get (niori-pdf-font-props font) :kern-hash)
                          h)
                    h))))
       (or (and h (gethash (+ (* c1 256) c2) h)) 0)))
    (_ 0)))

;;;; Font creation and registration

(declare-function niori-pdf-pdfa--base14-for-pdfa "niori-pdf-pdfa" (doc name))

(defun niori-pdf-get-font (doc name)
  "Get or create font NAME for document DOC.
NAME is a base-14 font name (e.g. \"Helvetica-Bold\"), a path to a
font file (.ttf, .otf or .ttc), a Type 1 font path (.pfb/.pfa), or a
font spec plist \(:file PATH :index N) which selects member N of a
TrueType collection.  Returns a `niori-pdf-font'; identical NAMEs
share one object.  In PDF/A documents base-14 names are replaced via
`niori-pdf-pdfa-base14-substitutions' (see niori-pdf-pdfa.el)."
  (or (cdr (assoc name (niori-pdf-document-fonts doc) #'equal))
      (let ((font
             (cond
              ((niori-pdf-fonts--ttf-path-p name)
               (niori-pdf-ttf-make-font doc (niori-pdf-fonts--file-of name)
                                        (niori-pdf-fonts--index-of name)))
              ((niori-pdf-fonts--type1-path-p name)
               (niori-pdf-type1-make-font doc (niori-pdf-fonts--file-of name)))
              ((niori-pdf-fonts--path-p name)
               (error "niori-pdf: unsupported font file type: %S" name))
              ((and (niori-pdf-document-pdfa-level doc)
                    (niori-pdf-fonts-known-p name))
               ;; PDF/A forbids unembedded fonts: substitute or fail
               (niori-pdf-get-font
                doc (niori-pdf-pdfa--base14-for-pdfa doc name)))
              (t (niori-pdf--base14-font doc name)))))
        (push (cons name font) (niori-pdf-document-fonts doc))
        font)))

(defun niori-pdf-fonts--spec-file (name)
  "Return the file path part of font spec NAME (string or plist)."
  (if (stringp name) name (plist-get name :file)))

(defalias 'niori-pdf-fonts--file-of 'niori-pdf-fonts--spec-file)

(defun niori-pdf-fonts--index-of (name)
  "Return the collection member index of font spec NAME, or nil."
  (unless (stringp name) (plist-get name :index)))

(defun niori-pdf-fonts--path-p (name)
  "Return t when NAME is a font file path or a file spec plist."
  (and (not (stringp name)) (plist-get name :file)))

(defun niori-pdf-fonts--ttf-path-p (name)
  "Return t when NAME is a TrueType/OpenType/collection font spec."
  (let ((file (niori-pdf-fonts--spec-file name)))
    (and (stringp file)
         (file-name-extension file)
         (member (downcase (file-name-extension file))
                 '("ttf" "otf" "ttc")))))

(defun niori-pdf-fonts--type1-path-p (name)
  "Return t when NAME is a Type 1 (.pfb/.pfa) font spec."
  (let ((file (niori-pdf-fonts--spec-file name)))
    (and (stringp file)
         (file-name-extension file)
         (member (downcase (file-name-extension file)) '("pfb" "pfa")))))

(defun niori-pdf--register-font-resource (doc font)
  "Assign FONT its resource name /F<n> in DOC."
  (let ((idx (1+ (niori-pdf-document-font-count doc))))
    (setf (niori-pdf-document-font-count doc) idx)
    (setf (niori-pdf-font-resource font) (format "F%d" idx))))

(defun niori-pdf--base14-font (doc name)
  "Create and register a base-14 font object for NAME in DOC."
  (let ((m (niori-pdf-fonts-metrics name)))
    (unless m
      (error "niori-pdf: unknown font %S (use a base-14 name or a .ttf path)"
             name))
    (let* ((font (niori-pdf-make-font :kind 'base14 :name name :doc doc))
           (dict (niori-pdf-dict)))
      (setf (niori-pdf-font-props font) (list :metrics m))
      (niori-pdf--register-font-resource doc font)
      (niori-pdf-dict-set dict 'Type 'Font)
      (niori-pdf-dict-set dict 'Subtype 'Type1)
      (niori-pdf-dict-set dict 'BaseFont (intern name))
      (unless (eq (plist-get m :encoding) 'native)
        (niori-pdf-dict-set dict 'Encoding 'WinAnsiEncoding))
      (setf (niori-pdf-font-ref font) (niori-pdf-add-object doc dict))
      font)))

;;;; Text encoding and measurement

(defun niori-pdf-font-metrics (font)
  "Return the base-14/Type-1 metrics plist of FONT (nil for TTF fonts)."
  (plist-get (niori-pdf-font-props font) :metrics))

(defun niori-pdf-font-encode (string font)
  "Encode STRING for drawing with FONT.
For base-14 and Type 1 fonts returns the WinAnsi (or native
Symbol/ZapfDingbats) unibyte byte string; for TTF fonts returns the
list of glyph ids (recording usage in FONT)."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1)
     (if (eq (plist-get (niori-pdf-font-metrics font) :encoding) 'native)
         (niori-pdf-encode-native string font)
       (niori-pdf-encode-winansi string)))
    (`ttf (niori-pdf-ttf-encode string font))))

(defun niori-pdf-font-encoded-width (encoded font)
  "Width in units/1000 em of ENCODED (result of `niori-pdf-font-encode')."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1)
     (let ((m (niori-pdf-font-metrics font))
           (sum 0.0))
       (let ((w (plist-get m :widths))
             (extra (plist-get m :extra-widths))
             (n (length encoded))
             (idx 0))
         (while (< idx n)
           (let* ((b (aref encoded idx))
                  (cw (if (and (>= b 32) (<= b 255))
                          (or (aref w (- b 32)) 0)
                        (or (cdr (assq b extra)) 0))))
             (setq sum (+ sum cw)))
           (setq idx (1+ idx))))
       sum))
    (`ttf (niori-pdf-ttf-encoded-width encoded font))))

(defun niori-pdf-font-encoded-width-kerned (encoded font)
  "Width of ENCODED in FONT including AFM kerning, units/1000 em.
Kerning is only applied for base-14 fonts with KPX data."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1)
     (let ((m (niori-pdf-font-metrics font))
           (sum 0.0))
       (let ((w (plist-get m :widths))
             (extra (plist-get m :extra-widths))
             (n (length encoded))
             (idx 0)
             prev)
         (while (< idx n)
           (let* ((b (aref encoded idx))
                  (cw (if (and (>= b 32) (<= b 255))
                          (or (aref w (- b 32)) 0)
                        (or (cdr (assq b extra)) 0))))
             (setq sum (+ sum cw))
             (when prev
               (setq sum (+ sum (niori-pdf-font-kern font prev b))))
             (setq prev b idx (1+ idx))))
         sum)))
    (`ttf (niori-pdf-ttf-encoded-width encoded font))))

(defun niori-pdf-text-width (string font &optional size)
  "Width of STRING drawn with FONT at SIZE points (SIZE defaults to 12).
FONT is a `niori-pdf-font' object, a base-14 font name, a font file
path (.ttf/.otf/.ttc/.pfb/.pfa) or a \(:file :index) spec."
  (let ((size (or size 12))
        (font (if (and (not (niori-pdf-font-p font))
                       (or (stringp font) (niori-pdf-fonts--path-p font)))
                  (niori-pdf--measurement-font font)
                font)))
    (/ (* (niori-pdf-font-encoded-width
           (niori-pdf-font-encode string font) font)
          size)
       1000.0)))

(defun niori-pdf-text-width-kerned (string font &optional size)
  "Width of STRING with FONT at SIZE including AFM kerning.
Same as `niori-pdf-text-width' for fonts without kerning data."
  (let ((size (or size 12))
        (font (if (and (not (niori-pdf-font-p font))
                       (or (stringp font) (niori-pdf-fonts--path-p font)))
                  (niori-pdf--measurement-font font)
                font)))
    (/ (* (niori-pdf-font-encoded-width-kerned
           (niori-pdf-font-encode string font) font)
          size)
       1000.0)))

(defconst niori-pdf--measurement-ttf-cache
  (make-hash-table :test #'equal)
  "Document-less TTF/OTF/TTC fonts parsed for width measurement only,
keyed by the font spec (path string or plist).  Measurement fonts
never create PDF objects and are never embedded.")

(defun niori-pdf--measurement-font (name)
  "Build a font object for measuring with NAME outside a document.
NAME is a base-14 font name, a font file path, or a \(:file :index)
spec; font files are parsed once and cached."
  (cond
   ((niori-pdf-fonts--ttf-path-p name)
    (or (gethash name niori-pdf--measurement-ttf-cache)
        (let* ((file (expand-file-name (niori-pdf-fonts--file-of name)))
               (idx (niori-pdf-fonts--index-of name))
               (font (niori-pdf-make-font :kind 'ttf :name name)))
          (setf (niori-pdf-font-props font)
                (niori-pdf-ttf--font-props
                 (niori-pdf-ttf-read-file file idx)
                 file idx))
          (puthash name font niori-pdf--measurement-ttf-cache)
          font)))
   ((niori-pdf-fonts--type1-path-p name)
    (or (gethash name niori-pdf--measurement-ttf-cache)
        (let* ((file (expand-file-name (niori-pdf-fonts--file-of name)))
               (font (niori-pdf-make-font :kind 'type1 :name name)))
          (setf (niori-pdf-font-props font)
                (niori-pdf-type1--font-props
                 (niori-pdf-type1-read-file file) file))
          (puthash name font niori-pdf--measurement-ttf-cache)
          font)))
   (t
    (let ((m (niori-pdf-fonts-metrics name)))
      (unless m
        (error "niori-pdf: unknown font %S" name))
      (niori-pdf-make-font :kind 'base14 :name name
                           :props (list :metrics m))))))

(defun niori-pdf-char-width (char font &optional size)
  "Width of character CHAR with FONT at SIZE."
  (niori-pdf-text-width (string char) font size))

(defun niori-pdf-font-ascender (font)
  "Ascender of FONT in units/1000 em."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1)
     (plist-get (niori-pdf-font-metrics font) :ascender))
    (`ttf (niori-pdf-ttf-ascender (niori-pdf-font-props font)))))

(defun niori-pdf-font-descender (font)
  "Descender of FONT in units/1000 em (a negative number)."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1)
     (plist-get (niori-pdf-font-metrics font) :descender))
    (`ttf (niori-pdf-ttf-descender (niori-pdf-font-props font)))))

(defun niori-pdf-font-cap-height (font)
  "Cap height of FONT in units/1000 em."
  (pcase (niori-pdf-font-kind font)
    ((or `base14 `type1)
     (plist-get (niori-pdf-font-metrics font) :capheight))
    (`ttf (niori-pdf-ttf-cap-height (niori-pdf-font-props font)))))

(provide 'niori-pdf-fonts)
;;; niori-pdf-fonts.el ends here
