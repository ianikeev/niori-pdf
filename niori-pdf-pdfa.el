;;; niori-pdf-pdfa.el --- PDF/A-2b archival output for niori-pdf -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev.
;;
;; Original Emacs Lisp implementation of PDF/A support.
;; See LICENSE for the license applicable to this file.

;;; Commentary:

;; PDF/A-2b (ISO 19005-2, level B) archival output.
;;
;; Enable with (niori-pdf-pdfa-enable doc) after creating the document
;; (or pass :pdfa t to `niori-pdf-with-flow-document').  The writer
;; then produces, in addition to the normal file structure:
;;
;;  * a %PDF-1.7 header (PDF/A-2 is based on ISO 32000-1),
;;  * a trailer /ID array (MD5 of the file body; also written for
;;    plain documents),
;;  * a document catalog /Metadata stream holding an XMP packet with
;;    pdfaid:part / pdfaid:conformance and values mirroring the Info
;;    dictionary byte for byte (ISO 19005-2 6.6.2),
;;  * a catalog /OutputIntents entry with a GTS_PDFA1 OutputIntent and
;;    an embedded sRGB ICC profile (DestOutputProfile, ISO 19005-2
;;    6.2.2), so DeviceRGB and DeviceGray colors are valid,
;;  * /F 4 (Print) flags on link annotations (6.8),
;;  * embedded fonts: the unembedded base-14 fonts are rejected unless
;;    `niori-pdf-pdfa-base14-substitutions' maps them to a font file
;;    (ISO 19005-1 6.3.4 / 19005-2 6.2.5: every font must be embedded),
;;  * no DeviceCMYK color operators (there is no CMYK OutputIntent;
;;    `niori-pdf-set-cmyk-fill' signals an error).
;;
;; The ICC profile is generated in pure Elisp: a minimal, deterministic
;; ICC v2 matrix/TRC profile describing the sRGB primaries with a
;; gamma 2.2 tone reproduction curve, D50 media white point, and the
;; required desc/cprt/wtpt/bkpt tags.  At roughly 0.5 KB it is much
;; smaller than shipping a full profile, and it is a structurally
;; valid ICC profile for archival rendering.
;;
;; Transparency (ExtGState alpha) and URI/GoTo link actions are legal
;; in PDF/A-2 (they were forbidden in PDF/A-1), so the regular feature
;; set keeps working; level 1 is deliberately not supported.

;;; Code:

(require 'cl-lib)
(require 'niori-pdf-core)
(require 'niori-pdf-fonts)

(defgroup niori-pdf-pdfa nil
  "PDF/A-2b archival output for niori-pdf."
  :group 'niori-pdf
  :prefix "niori-pdf-pdfa-")

(defcustom niori-pdf-pdfa-base14-substitutions nil
  "Alist of base-14 font names mapped to embedded font file specs.
PDF/A forbids unembedded fonts (ISO 19005-1 6.3.4), and the base-14
fonts (Helvetica & friends, Symbol, ZapfDingbats) are by definition
not embedded -- so in PDF/A documents they must be replaced by real
font files, e.g.:

  (setq niori-pdf-pdfa-base14-substitutions
        \\='((\"Helvetica\" . \"/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf\")
          (\"ZapfDingbats\" . (:file \"/path/to/symbol-capable.ttf\"))))

The replacement can be any font spec accepted by `niori-pdf-set-font'
(a .ttf/.otf/.ttc path, a Type 1 .pfb/.pfa path, or a \\(:file :index)
plist).  A base-14 font without an entry raises an error in PDF/A
mode.  Only applies to PDF/A documents; plain documents keep using
the unembedded base-14 fonts."
  :type '(alist :key-type string
                :value-type (choice string plist))
  :group 'niori-pdf-pdfa)

;;;; Time memoization (Info dictionary and XMP must agree)

(defun niori-pdf-pdfa--doc-time (doc)
  "Return DOC's memoized creation time, storing it in the info plist."
  (let ((info (niori-pdf-document-info doc)))
    (or (plist-get info :niori-pdf--time)
        (let ((n (current-time)))
          (setf (niori-pdf-document-info doc)
                (plist-put info :niori-pdf--time n))
          n))))

;;;; Minimal ICC v2 sRGB profile (matrix/TRC)

(defconst niori-pdf-pdfa--srgb-primaries
  ;; D50-adapted sRGB colorant XYZ values, as used by the standard
  ;; sRGB IEC61966-2.1 profile (PCS encoding: s15Fixed16).
  '((r 0.4360747 0.2225045 0.0139322)
    (g 0.3850680 0.7168786 0.0971045)
    (b 0.1430804 0.0606241 0.7141733))
  "sRGB colorant matrices adapted to D50.")

(defconst niori-pdf-pdfa--d50 '(0.9642 1.0 0.8249)
  "ICC D50 PCS illuminant (media white point).")

(defun niori-pdf-pdfa--s15f16 (v)
  "Format number V as a 4-byte s15Fixed16 big-endian string."
  (let ((n (round (* v 65536))))
    (string
     (logand (ash n -24) 255)
     (logand (ash n -16) 255)
     (logand (ash n -8) 255)
     (logand n 255))))

(defun niori-pdf-pdfa--icc-u32 (n)
  "Format integer N as a 4-byte big-endian string."
  (string (logand (ash n -24) 255)
          (logand (ash n -16) 255)
          (logand (ash n -8) 255)
          (logand n 255)))

(defun niori-pdf-pdfa--icc-xyz-tag (x y z)
  "XYZType ('XYZ ') payload with coordinates X Y Z.
The payload's first four bytes are the TYPE signature 'XYZ ',
followed by the 4-byte reserved field and three s15Fixed16 numbers."
  (concat "XYZ \0\0\0\0"
          (niori-pdf-pdfa--s15f16 x)
          (niori-pdf-pdfa--s15f16 y)
          (niori-pdf-pdfa--s15f16 z)))

(defun niori-pdf-pdfa--icc-curv-tag (gamma)
  "curveType ('curv') payload with exponent GAMMA.
Type signature + reserved field + entry count; with a single entry
the value is a u8Fixed8 gamma (256 * GAMMA)."
  (let ((g (round (* gamma 256))))
    (concat "curv\0\0\0\0"
            (niori-pdf-pdfa--icc-u32 1)  ; one entry: simple gamma
            (string (logand (ash g -8) 255) (logand g 255)))))

(defun niori-pdf-pdfa--icc-text-tag (s)
  "textType ('text') payload holding ASCII string S."
  (concat "text\0\0\0\0" s "\0"))

(defun niori-pdf-pdfa--icc-desc-tag (name)
  "textDescriptionType ('desc') payload describing the profile as NAME."
  (let* ((ascii (concat name "\0"))
         (n (length ascii)))
    (concat "desc\0\0\0\0"
            (niori-pdf-pdfa--icc-u32 n)
            ascii
            ;; unicode language code (0), unicode count (0),
            ;; scriptcode (0), mac count (0) + 67 reserved bytes
            "\0\0\0\0\0\0\0\0\0\0\0"
            (make-string 67 0))))

(defun niori-pdf-pdfa--icc-srgb ()
  "Build a minimal, deterministic ICC v2 sRGB matrix/TRC profile.
Returns the unibyte profile bytes.  Header + tag table (sorted) +
tag data; rendering intent perceptual; D50 media white point; the
sRGB primaries with a gamma 2.2 TRC."
  (let* ((prim (lambda (c)
                 (assq c niori-pdf-pdfa--srgb-primaries)))
         (xyz (lambda (c)
                (let ((kv (funcall prim c)))
                  (list (nth 1 kv) (nth 2 kv) (nth 3 kv)))))
         (rxyz (funcall xyz 'r))
         (gxyz (funcall xyz 'g))
         (bxyz (funcall xyz 'b))
         (wtpt niori-pdf-pdfa--d50)
         (gamma 2.2)
         (desc (niori-pdf-pdfa--icc-desc-tag
                "sRGB (niori-pdf.el minimal matrix/TRC profile)"))
         (cprt (niori-pdf-pdfa--icc-text-tag
                "niori-pdf.el generated ICC profile; CC0"))
         (wtpt-t (apply #'niori-pdf-pdfa--icc-xyz-tag wtpt))
         (bkpt-t (niori-pdf-pdfa--icc-xyz-tag 0.0 0.0 0.0))
         (rxyz-t (apply #'niori-pdf-pdfa--icc-xyz-tag rxyz))
         (gxyz-t (apply #'niori-pdf-pdfa--icc-xyz-tag gxyz))
         (bxyz-t (apply #'niori-pdf-pdfa--icc-xyz-tag bxyz))
         (rtrc-t (niori-pdf-pdfa--icc-curv-tag gamma))
         (gtrc-t (niori-pdf-pdfa--icc-curv-tag gamma))
         (btrc-t (niori-pdf-pdfa--icc-curv-tag gamma))
         ;; sorted by signature ascending (ICC v2 recommends)
         (tags (list (cons "bTRC" btrc-t)
                     (cons "bXYZ" bxyz-t)
                     (cons "bkpt" bkpt-t)
                     (cons "cprt" cprt)
                     (cons "desc" desc)
                     (cons "gTRC" gtrc-t)
                     (cons "gXYZ" gxyz-t)
                     (cons "rTRC" rtrc-t)
                     (cons "rXYZ" rxyz-t)
                     (cons "wtpt" wtpt-t)))
         (n-tags (length tags))
         ;; tag table: 4-byte count + 12 bytes per tag
         (data-offset (+ 128 4 (* 12 n-tags))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; ---- header (128 bytes) ----
      (insert "\0\0\0\0")                 ; profile size (patched below)
      (insert "\0\0\0\0")                 ; preferred CMM: none
      (insert "\x02\x10\0\0")             ; version 2.1.0 (0x02100000)
      (insert "mntr")                     ; device class: display
      (insert "RGB ")                     ; data color space
      (insert "XYZ ")                     ; PCS
      (insert "\x0A\xEA\0\1\0\1\0\0\0\0\0\0") ; creation date
                                        ; 2026-01-01 00:00:00 (fixed,
                                        ; keeps output deterministic)
      (insert "acsp")                     ; signature
      (insert "\0\0\0\0")                 ; platform
      (insert "\0\0\0\0")                 ; profile flags
      (insert "\0\0\0\0")                 ; device manufacturer
      (insert "\0\0\0\0")                 ; device model
      (insert "\0\0\0\0\0\0\0\0")         ; device attributes
      (insert "\0\0\0\0")                 ; rendering intent: perceptual
      (insert (niori-pdf-pdfa--s15f16 0.9642)
              (niori-pdf-pdfa--s15f16 1.0)
              (niori-pdf-pdfa--s15f16 0.8249)) ; D50 illuminant
      (insert "\0\0\0\0")                 ; profile creator
      (insert (make-string 16 0))         ; profile ID (v2: optional)
      (insert (make-string 28 0))         ; reserved
      ;; ---- tag table ----
      (insert (niori-pdf-pdfa--icc-u32 n-tags))
      (let ((pos data-offset))
        (dolist (tag tags)
          (insert (car tag))
          (insert (niori-pdf-pdfa--icc-u32 pos))
          (insert (niori-pdf-pdfa--icc-u32 (length (cdr tag))))
          (setq pos (+ pos (length (cdr tag))))
          ;; 4-byte alignment padding between tag data
          (setq pos (+ pos (% (- 4 (% pos 4)) 4)))))
      ;; ---- tag data ----
      (dolist (tag tags)
        (insert (cdr tag))
        (let ((pad (% (- 4 (% (length (cdr tag)) 4)) 4)))
          (when (> pad 0) (insert (make-string pad 0)))))
      ;; patch the profile size at offset 0
      (let ((size (buffer-size)))
        (goto-char 1)
        (delete-char 4)
        (insert (niori-pdf-pdfa--icc-u32 size)))
      (buffer-string))))

;;;; XMP metadata packet

(defun niori-pdf-pdfa--xml-escape (s)
  "Escape string S for use as XML text content."
  (let ((out (string-replace "&" "&amp;" s)))
    (setq out (string-replace "<" "&lt;" out))
    (setq out (string-replace ">" "&gt;" out))
    out))

(defun niori-pdf-pdfa--xmp-date (time)
  "Format TIME as an XMP ISO-8601 date with timezone offset."
  (let ((s (format-time-string "%Y-%m-%dT%H:%M:%S" time))
        (tz (format-time-string "%z" time)))
    (concat s (substring tz 0 3) ":" (substring tz 3 5))))

(defun niori-pdf-pdfa--producer-string (doc)
  "The Producer string the Info dictionary will use for DOC."
  (or (plist-get (niori-pdf-document-info doc) :producer)
      (concat "niori-pdf.el " niori-pdf-version " (port of cl-pdf)")))

(defun niori-pdf-pdfa--creator-string (doc)
  "The Creator (creating application) string for DOC."
  (or (plist-get (niori-pdf-document-info doc) :creator)
      (concat "niori-pdf.el " niori-pdf-version)))

(defun niori-pdf-pdfa--xmp-packet (doc)
  "Build the XMP metadata packet for PDF/A document DOC.
Mirrors the Info dictionary entries (ISO 19005-2 6.6.2) and carries
the pdfaid identification."
  (let* ((level (niori-pdf-document-pdfa-level doc))
         (info (niori-pdf-document-info doc))
         (time (niori-pdf-pdfa--doc-time doc))
         (title (plist-get info :title))
         (author (plist-get info :author))
         (subject (plist-get info :subject))
         (keywords (plist-get info :keywords))
         (producer (niori-pdf-pdfa--producer-string doc))
         (creator (niori-pdf-pdfa--creator-string doc))
         (date (niori-pdf-pdfa--xmp-date time)))
    (concat
     "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n"
     "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmptk=\"niori-pdf.el "
     niori-pdf-version "\">\n"
     " <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n"
     "  <rdf:Description rdf:about=\"\"\n"
     "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n"
     "    xmlns:pdf=\"http://ns.adobe.com/pdf/1.3/\"\n"
     "    xmlns:xmp=\"http://ns.adobe.com/xap/1.0/\"\n"
     "    xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">\n"
     "   <dc:format>application/pdf</dc:format>\n"
     "   <pdfaid:part>" (number-to-string level) "</pdfaid:part>\n"
     "   <pdfaid:conformance>B</pdfaid:conformance>\n"
     "   <pdf:Producer>" (niori-pdf-pdfa--xml-escape producer)
     "</pdf:Producer>\n"
     "   <xmp:CreatorTool>" (niori-pdf-pdfa--xml-escape creator)
     "</xmp:CreatorTool>\n"
     "   <xmp:CreateDate>" date "</xmp:CreateDate>\n"
     "   <xmp:ModifyDate>" date "</xmp:ModifyDate>\n"
     (when title
       (concat "   <dc:title><rdf:Alt><rdf:li xml:lang=\"x-default\">"
               (niori-pdf-pdfa--xml-escape title)
               "</rdf:li></rdf:Alt></dc:title>\n"))
     (when author
       (concat "   <dc:creator><rdf:Seq><rdf:li>"
               (niori-pdf-pdfa--xml-escape author)
               "</rdf:li></rdf:Seq></dc:creator>\n"))
     (when subject
       (concat "   <dc:description><rdf:Alt><rdf:li xml:lang=\"x-default\">"
               (niori-pdf-pdfa--xml-escape subject)
               "</rdf:li></rdf:Alt></dc:description>\n"))
     (when keywords
       (concat "   <pdf:Keywords>"
               (niori-pdf-pdfa--xml-escape keywords)
               "</pdf:Keywords>\n"))
     "  </rdf:Description>\n"
     " </rdf:RDF>\n"
     "</x:xmpmeta>\n"
     "<?xpacket end=\"w\"?>")))

;;;; Save hook and catalog extension

(defun niori-pdf-pdfa--save-hook (doc)
  "Save hook for DOC: PDF/A annotation requirements.
Link annotations must carry the Print flag (/F bit 2) and must not
carry Hidden, Invisible or ToggleNoView (ISO 19005-2 6.8)."
  (dolist (page (niori-pdf-document-pages doc))
    (dolist (annot (niori-pdf-page-annots page))
      (when (eq (niori-pdf-dict-get annot 'Subtype) 'Link)
        (niori-pdf-dict-set annot 'F 4)))))

(defun niori-pdf-pdfa--catalog-extension (doc catalog)
  "Catalog extension: add /Metadata (XMP) and /OutputIntents to CATALOG."
  (let* ((xmp (niori-pdf-pdfa--xmp-packet doc))
         (xmp-dict (niori-pdf-dict))
         (xmp-data (encode-coding-string xmp 'utf-8))
         (xmp-stream (niori-pdf-stream :dict xmp-dict :data xmp-data))
         (xmp-ref (niori-pdf-add-object doc xmp-stream))
         (icc (niori-pdf-pdfa--icc-srgb))
         (icc-dict (niori-pdf-dict))
         (icc-stream (niori-pdf-stream :dict icc-dict :data icc))
         (icc-ref (niori-pdf-add-object doc icc-stream))
         (oi (niori-pdf-dict)))
    (niori-pdf-dict-set xmp-dict 'Type 'Metadata)
    (niori-pdf-dict-set xmp-dict 'Subtype 'XML)
    (niori-pdf-dict-set xmp-dict 'Length (length
                                          (niori-pdf-stream-data
                                           xmp-stream)))
    (niori-pdf-dict-set icc-dict 'N 3)
    (niori-pdf-dict-set icc-dict 'Length (length icc))
    (niori-pdf-dict-set oi 'Type 'OutputIntent)
    (niori-pdf-dict-set oi 'S 'GTS_PDFA1)
    (niori-pdf-dict-set oi 'OutputConditionIdentifier "sRGB")
    (niori-pdf-dict-set oi 'OutputCondition
                        "sRGB (niori-pdf.el ICC profile)")
    (niori-pdf-dict-set oi 'RegistryName "http://www.color.org")
    (niori-pdf-dict-set oi 'DestOutputProfile icc-ref)
    (niori-pdf-dict-set catalog 'Metadata xmp-ref)
    (niori-pdf-dict-set catalog 'OutputIntents (niori-pdf-array oi))
    nil))

;;;; Font policy

(defun niori-pdf-pdfa--base14-for-pdfa (doc name)
  "Return the font spec replacing base-14 font NAME in PDF/A DOC.
Consults `niori-pdf-pdfa-base14-substitutions'; signals an error
when there is no entry, pointing at the required fix."
  (or (cdr (assoc name niori-pdf-pdfa-base14-substitutions))
      (error
       "niori-pdf: font %S cannot be used unembedded in PDF/A (ISO 19005); \
add it to `niori-pdf-pdfa-base14-substitutions' or use an embedded \
.ttf/.otf/.ttc/.pfb font" name)))

;;;; Enabling

(defun niori-pdf-pdfa-enable (doc &optional level)
  "Enable PDF/A-2b archival output for document DOC.
LEVEL is the PDF/A part (only 2 = PDF/A-2b is supported, the most
pragmatic archival flavor: transparency, JPEG and URI actions stay
legal).  Registers the metadata/output-intent catalog extension and
the annotation save hook; `niori-pdf-save' writes a %PDF-1.7 header
and an /ID trailer once the level is set.  Returns DOC.

Fonts used in DOC must all be embedded: base-14 names require an
entry in `niori-pdf-pdfa-base14-substitutions'."
  (let ((level (or level 2)))
    (unless (= level 2)
      (error "niori-pdf: only PDF/A-2b (level 2) is supported; got %S"
             level))
    (setf (niori-pdf-document-pdfa-level doc) level)
    (unless (member #'niori-pdf-pdfa--save-hook
                    (niori-pdf-document-save-hooks doc))
      (push #'niori-pdf-pdfa--save-hook
            (niori-pdf-document-save-hooks doc)))
    (niori-pdf-register-catalog-extension
     doc #'niori-pdf-pdfa--catalog-extension)
    doc))

(provide 'niori-pdf-pdfa)
;;; niori-pdf-pdfa.el ends here
