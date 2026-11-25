;;; niori-pdf-core.el --- PDF object model, compression and file writer -*- lexical-binding: t; -*-

;; Copyright (c) 2002-2009 Marc Battyani.
;; Copyright (C) 2026 Igor Anikeev.
;;
;; Portions of this file are derived from CL-PDF.  See LICENSE
;; for the applicable upstream license and disclaimer.

;;; Commentary:

;; This module implements the low-level PDF object model (dictionaries,
;; arrays, streams, indirect references), the document/file writer with
;; classic xref table, and stream compression.
;;
;; Compression: GNU Emacs ships zlib support for *decompression* only
;; (`zlib-available-p', `zlib-decompress-region'); there is no built-in
;; compression primitive (there is no `zlib-insert').  niori-pdf therefore
;; provides a small pure Elisp DEFLATE implementation (RFC 1950/1951,
;; fixed Huffman codes + LZ77) that produces standard FlateDecode streams
;; with no external dependency.  Modes, via `niori-pdf-compression':
;;   `deflate  -- pure Elisp deflate (default, always available)
;;   `raw      -- no compression
;;   `gzip     -- external gzip binary (opt-in; its raw deflate payload is
;;                used as the FlateDecode stream, falling back to the Elisp
;;                deflate when the binary is unavailable)

;;; Code:

(require 'cl-lib)

(defconst niori-pdf-version "0.1.0"
  "Current version of niori-pdf (keep in sync with the package header).")

;;;; Customization

(defgroup niori-pdf nil
  "Generate PDF files in pure Emacs Lisp (port of cl-pdf)."
  :group 'applications
  :prefix "niori-pdf-")

(defcustom niori-pdf-compression 'deflate
  "Stream compression method.
`deflate' uses the built-in pure Elisp deflate implementation,
`raw' writes uncompressed streams, `gzip' uses the external program
`niori-pdf-gzip-program' (falling back to `deflate' when unavailable)."
  :type '(choice (const :tag "Pure Elisp deflate" deflate)
                 (const :tag "No compression" raw)
                 (const :tag "External gzip (opt-in)" gzip))
  :group 'niori-pdf)

(defcustom niori-pdf-gzip-program "gzip"
  "Path to the gzip executable used when `niori-pdf-compression' is `gzip'."
  :type 'string
  :group 'niori-pdf)

(defcustom niori-pdf-deflate-min-size 100
  "Streams shorter than this many bytes are written uncompressed."
  :type 'integer
  :group 'niori-pdf)

(defcustom niori-pdf-deflate-window 32768
  "LZ77 sliding window size used by the Elisp deflate (max 32768)."
  :type 'integer
  :group 'niori-pdf)

(defcustom niori-pdf-deflate-max-chain 16
  "Maximum hash chain length searched for LZ77 matches."
  :type 'integer
  :group 'niori-pdf)

;;;; Numbers and names

(defun niori-pdf-num (n)
  "Format number N as a PDF real or integer."
  (cond
   ((integerp n) (number-to-string n))
   ((and (floatp n) (<= (abs n) 1.0e+06))
    (let ((s (format "%.4f" n)))
      (setq s (replace-regexp-in-string "0+$" "" s))
      (setq s (replace-regexp-in-string "\\.$" "" s))
      (cond ((string= s "-") (setq s "0"))
            ((string-prefix-p "." s) (setq s (concat "0" s)))
            ((string-prefix-p "-." s) (setq s (concat "-0" (substring s 1)))))
      s))
   (t (error "niori-pdf: number out of range: %S" n))))

(defun niori-pdf--name-string (sym)
  "Return the print form of SYM as a PDF name token (no leading slash)."
  (let ((name (if (symbolp sym) (symbol-name sym) sym)))
    (replace-regexp-in-string
     "[^A-Za-z0-9+.:_/()-]"
     (lambda (c) (format "#%02X" (aref (encode-coding-string c 'binary) 0)))
     name)))

;;;; Object model

(cl-defstruct (niori-pdf-dict
               (:constructor niori-pdf-dict)
               (:constructor niori-pdf-dict-from-alist (entries)))
  "PDF dictionary.  ENTRIES is an alist of (SYMBOL . VALUE) preserving order."
  entries)

(cl-defstruct (niori-pdf-array
               (:constructor niori-pdf--make-array (elements)))
  "PDF array object; ELEMENTS is a list of child objects."
  elements)

(defun niori-pdf-array (&rest elements)
  "Construct a PDF array holding ELEMENTS."
  (niori-pdf--make-array elements))

(cl-defstruct (niori-pdf-stream
               (:constructor niori-pdf-stream (&key dict data)))
  "PDF stream: DICT (a `niori-pdf-dict') plus DATA (unibyte string)."
  dict data)

(cl-defstruct (niori-pdf-ref (:constructor niori-pdf-ref (num)))
  num)

(cl-defstruct (niori-pdf-raw (:constructor niori-pdf-raw (str)))
  str)

(defun niori-pdf-dict-set (dict key value)
  "Set KEY to VALUE in `niori-pdf-dict' DICT, replacing any previous entry."
  (let ((cell (assq key (niori-pdf-dict-entries dict))))
    (if cell
        (setcdr cell value)
      (push (cons key value) (niori-pdf-dict-entries dict)))
    value))

(defun niori-pdf-dict-get (dict key)
  "Return the value for KEY in DICT or nil."
  (alist-get key (niori-pdf-dict-entries dict)))

(defun niori-pdf--serialize (obj)
  "Serialize direct object OBJ to its PDF syntax representation."
  (cond
   ((niori-pdf-ref-p obj)
    (format "%d 0 R" (niori-pdf-ref-num obj)))
   ((niori-pdf-dict-p obj)
    (concat "<< "
            (mapconcat
             (lambda (kv)
               (concat "/" (niori-pdf--name-string (car kv))
                       " " (niori-pdf--serialize (cdr kv))))
             (niori-pdf-dict-entries obj) " ")
            " >>"))
   ((niori-pdf-array-p obj)
    (concat "[ " (mapconcat #'niori-pdf--serialize
                            (niori-pdf-array-elements obj) " ") " ]"))
   ((niori-pdf-stream-p obj)
    (error "niori-pdf: stream must be written as an indirect object"))
   ((niori-pdf-raw-p obj) (niori-pdf-raw-str obj))
   ((eq obj t) "true")
   ((null obj) "null")
   ((symbolp obj) (concat "/" (niori-pdf--name-string obj)))
   ((stringp obj) (niori-pdf--escape-literal-string obj))
   ((floatp obj) (niori-pdf-num obj))
   ((integerp obj) (number-to-string obj))
   (t (error "niori-pdf: cannot serialize %S" obj))))

(defun niori-pdf--escape-literal-string (bytes)
  "Escape unibyte BYTES as a PDF literal string."
  (let ((n (length bytes)) (i 0) (out nil))
    (while (< i n)
      (let ((c (aref bytes i)))
        (push
         (cond ((or (= c 41) (= c 40) (= c 92)) (string 92 c))
               ((and (>= c 32) (<= c 126)) (string c))
               (t (format "\\%03o" c)))
         out)
        (setq i (1+ i))))
    (concat "(" (apply #'concat (nreverse out)) ")")))

;;;; Document

(cl-defstruct (niori-pdf-document (:constructor niori-pdf-make-document))
  "A PDF document under construction."
  (objects nil)            ; object bodies, newest first (reverse chronological)
  (n-objects 0)            ; number of objects = current highest object number
  (pages nil)              ; list of `niori-pdf-page', newest first
  (fonts nil)              ; alist (NAME-KEY . niori-pdf-font)
  (font-count 0)
  (images nil)             ; alist (NAME-KEY . niori-pdf-image)
  (image-count 0)
  (extgstates nil)         ; alist (name-string . ref)
  (gstate-cache nil)       ; dedupe of (fill-alpha stroke-alpha blend-mode)
  (templates nil)          ; alist (NAME-KEY . niori-pdf-template)
  (template-count 0)
  (page-mode nil)          ; catalog /PageMode symbol, e.g. 'UseOutlines
  (page-layout nil)        ; catalog /PageLayout symbol, e.g. 'OneColumn
  (info nil)               ; plist :title :author :subject :keywords :creator :producer
  (save-hooks nil)         ; functions called with DOC at save time, before page tree
  (pending-links nil)      ; (ANNOT-DICT PAGE-NUM TOP) links resolved at save time
  (page-width 595.28) (page-height 841.89) ; A4 portrait by default
  (compression niori-pdf-compression)
  (pdfa-level nil))        ; non-nil = PDF/A output (2 = PDF/A-2b, see
                           ; niori-pdf-pdfa.el); affects the header
                           ; version, /ID trailer and the font policy

(cl-defstruct (niori-pdf-font (:constructor niori-pdf-make-font))
  "Font registered in a document (kind-specific data lives in PROPS)."
  kind                       ; 'base14, 'type1 (embedded Type 1) or 'ttf
  name                       ; base font name or font file path
  resource                   ; resource name string, e.g. "F1"
  ref                        ; indirect ref of the top-level font dict
  doc                        ; owning document
  (props nil))               ; kind-specific plist

(cl-defstruct (niori-pdf-image (:constructor niori-pdf-make-image))
  "Image XObject registered in a document."
  resource                   ; resource name string, e.g. "Im1"
  ref                        ; indirect ref of the image XObject
  dict                       ; XObject dictionary (niori-pdf-dict)
  data                       ; unibyte stream data
  width height               ; pixel dimensions
  kind                       ; 'jpeg or 'png
  (props nil))               ; kind-specific plist

(cl-defstruct (niori-pdf-page (:constructor niori-pdf-make-page))
  "One page of a document."
  (content nil)            ; list of content chunks, newest first
  (media-box nil)          ; (x0 y0 x1 y1)
  (annots nil)             ; list of annotation dicts, newest first
  (rotate nil)             ; page /Rotate (multiples of 90), or nil
  (obj-num nil))

(cl-defstruct (niori-pdf-template (:constructor niori-pdf-make-template))
  "Form XObject recorded with `niori-pdf-with-template'
and painted with `niori-pdf-draw-template'."
  resource                 ; resource name string, e.g. "Tx1"
  ref                      ; indirect ref of the form XObject
  dict                     ; XObject dictionary (niori-pdf-dict)
  data                     ; unibyte (well, raw) stream content
  width height             ; bounding box size in points
  (bbox-origin nil))       ; (x0 y0) of the /BBox, default (0 0)

(defun niori-pdf-add-object (doc body)
  "Add BODY as a new indirect object of document DOC; return its ref."
  (let ((num (1+ (niori-pdf-document-n-objects doc))))
    (push body (niori-pdf-document-objects doc))
    (setf (niori-pdf-document-n-objects doc) num)
    (niori-pdf-ref num)))

(defun niori-pdf-new-page (doc &optional width height rotate)
  "Create, register and return a new page in DOC (W x H in points).
ROTATE is an optional /Rotate angle in multiples of 90 degrees."
  (let* ((w (or width (niori-pdf-document-page-width doc)))
         (h (or height (niori-pdf-document-page-height doc)))
         (page (niori-pdf-make-page :media-box (list 0 0 w h)
                                    :rotate rotate)))
    (push page (niori-pdf-document-pages doc))
    page))

(defun niori-pdf--pdfa-no-cmyk ()
  "Reject DeviceCMYK color operators in PDF/A documents.
Called by `niori-pdf-set-cmyk-fill' / `niori-pdf-set-cmyk-stroke':
a PDF/A document carries an RGB (sRGB) OutputIntent, and ISO
19005-2 6.2.3 forbids DeviceCMYK without a matching CMYK
OutputIntent."
  (when (and (boundp 'niori-pdf--doc) niori-pdf--doc
             (niori-pdf-document-pdfa-level niori-pdf--doc))
    (error "niori-pdf: DeviceCMYK color is not allowed in a PDF/A document (use RGB)")))

;;;; Content accumulation

(defun niori-pdf-page-emit (page string)
  "Append raw operator STRING to PAGE's content stream."
  (push string (niori-pdf-page-content page)))

(defun niori-pdf-page-content-string (page)
  "Concatenate PAGE's content chunks in order.
Uses the non-destructive `reverse' so that inspecting the content
(e.g. for a table of contents pass or tests) does not corrupt the
page's chunk list before `niori-pdf-save'."
  (apply #'concat (reverse (niori-pdf-page-content page))))

;;;; Pure Elisp DEFLATE (RFC 1950/1951, fixed Huffman codes)
;;
;; Verified against Python's zlib: canonical fixed literal/length codes
;; written MSB-first, 5-bit distance codes (value = code) written
;; MSB-first, all extra bits LSB-first.

(defconst niori-pdf--length-base
  [3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258])

(defconst niori-pdf--length-extra
  [0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0])

(defconst niori-pdf--dist-base
  [1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577])

(defconst niori-pdf--dist-extra
  [0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13])

(defun niori-pdf--adler32 (bytes)
  "Compute the Adler-32 checksum of unibyte string BYTES."
  (let ((n (length bytes)) (i 0) (a 1) (b 0))
    (while (< i n)
      (let ((chunk (min 5552 (- n i))) (j 0))
        (while (< j chunk)
          (setq a (+ a (aref bytes (+ i j)))
                b (+ b a)
                j (1+ j)))
        (setq a (% a 65521) b (% b 65521) i (+ i chunk))))
    (+ (* b 65536) a)))

(cl-defstruct (niori-pdf--bitsink (:constructor niori-pdf--make-bitsink))
  buf (acc 0) (nbits 0))

(defmacro niori-pdf--with-bitsink-buffer (bs &rest body)
  "Run BODY with the buffer of bitsink BS current."
  `(with-current-buffer (niori-pdf--bitsink-buf ,bs) ,@body))

(defun niori-pdf--bs-put (bs byte)
  "Emit one raw BYTE at the current position of bitsink BS."
  (niori-pdf--with-bitsink-buffer bs (insert byte)))

(defun niori-pdf--bits-put (bs value nbits)
  "Put the low NBITS of VALUE into the bit stream of BS (LSB first)."
  (when (> nbits 0)
    (setf (niori-pdf--bitsink-acc bs)
          (logior (niori-pdf--bitsink-acc bs)
                  (ash (logand value (1- (ash 1 nbits)))
                       (niori-pdf--bitsink-nbits bs))))
    (setf (niori-pdf--bitsink-nbits bs)
          (+ (niori-pdf--bitsink-nbits bs) nbits))
    (while (>= (niori-pdf--bitsink-nbits bs) 8)
      (niori-pdf--bs-put bs (logand (niori-pdf--bitsink-acc bs) 255))
      (setf (niori-pdf--bitsink-acc bs) (ash (niori-pdf--bitsink-acc bs) -8))
      (setf (niori-pdf--bitsink-nbits bs)
            (- (niori-pdf--bitsink-nbits bs) 8)))))

(defun niori-pdf--code-put (bs code len)
  "Put canonical Huffman CODE of bit length LEN into BS, MSB first."
  (let ((shift (1- len)))
    (while (>= shift 0)
      (niori-pdf--bits-put bs (logand (ash code (- shift)) 1) 1)
      (setq shift (1- shift)))))

(defun niori-pdf--fixed-lit-code (lit)
  "Return (CODE . LEN) for literal/length LIT under fixed Huffman."
  (cond
   ((< lit 144) (cons (+ lit 48) 8))
   ((< lit 256) (cons (+ (- lit 144) 400) 9))
   ((< lit 280) (cons (- lit 256) 7))
   (t (cons (+ (- lit 280) 192) 8))))

(defun niori-pdf--len-code (len)
  "Return (CODE EXTRA EXTRA-BITS) for match length LEN (3..258)."
  (let ((i 28))
    (while (and (> i 0) (< len (aref niori-pdf--length-base i)))
      (setq i (1- i)))
    (list (+ 257 i) (- len (aref niori-pdf--length-base i))
          (aref niori-pdf--length-extra i))))

(defun niori-pdf--dist-code (dist)
  "Return (CODE EXTRA EXTRA-BITS) for match distance DIST (1..32768)."
  (let ((i 29))
    (while (and (> i 0) (< dist (aref niori-pdf--dist-base i)))
      (setq i (1- i)))
    (list i (- dist (aref niori-pdf--dist-base i))
          (aref niori-pdf--dist-extra i))))

(defun niori-pdf-deflate (str)
  "Compress unibyte string STR with the pure Elisp deflate.
Returns a zlib (RFC 1950) stream using fixed Huffman codes and LZ77."
  (setq str (encode-coding-string str 'binary))
  (let* ((n (length str))
         (win (min niori-pdf-deflate-window 32768))
         (chain niori-pdf-deflate-max-chain)
         (bs (niori-pdf--make-bitsink
              :buf (generate-new-buffer " *niori-pdf deflate*"))))
    (unwind-protect
        (progn
          (with-current-buffer (niori-pdf--bitsink-buf bs)
            (set-buffer-multibyte nil))
          (niori-pdf--bs-put bs 120)          ; CMF: CM=8, CINFO=7
          (niori-pdf--bs-put bs 156)          ; FLG: FCHECK ok, FDICT=0
          (niori-pdf--bits-put bs 1 1)        ; BFINAL
          (niori-pdf--bits-put bs 1 2)        ; BTYPE = 01 (fixed Huffman)
          (let ((hash (make-hash-table :test #'eql
                                       :size (max 64 (min 65536 (/ (max n 1) 4)))))
                (i 0))
            (while (< i n)
              (let* ((c0 (aref str i))
                     (c1 (if (< (1+ i) n) (aref str (1+ i)) 0))
                     (c2 (if (< (+ i 2) n) (aref str (+ i 2)) 0))
                     (key (+ (* c0 65536) (* c1 256) c2))
                     (cands (gethash key hash))
                     (best-len 0) (best-dist 0) (tries 0)
                     done)
                (while (and (not done) cands (< tries chain))
                  (let ((pos (car cands)))
                    (setq cands (cdr cands)
                          tries (1+ tries))
                    (when (and pos (<= (- i pos) win) (>= (- i pos) 1))
                      (let ((len 0) (maxl (min 258 (- n i))))
                        (while (and (< len maxl)
                                    (= (aref str (+ pos len)) (aref str (+ i len))))
                          (setq len (1+ len)))
                        (when (> len best-len)
                          (setq best-len len best-dist (- i pos)))
                        (when (= best-len maxl) (setq done t))))))
                ;; register current position (and covered positions) in chains
                (let ((k 0))
                  (while (and (< k best-len) (< (+ i k) (- n 2)))
                    (let* ((p (+ i k))
                           (a (aref str p)) (bb (aref str (1+ p)))
                           (cc (aref str (+ p 2)))
                           (kk (+ (* a 65536) (* bb 256) cc)))
                      (puthash kk (cons p (gethash kk hash)) hash))
                    (setq k (1+ k))))
                (if (>= best-len 3)
                    (let ((lc (niori-pdf--len-code best-len))
                          (dc (niori-pdf--dist-code best-dist)))
                      (let ((cc (niori-pdf--fixed-lit-code (nth 0 lc))))
                        (niori-pdf--code-put bs (car cc) (cdr cc)))
                      (niori-pdf--bits-put bs (nth 1 lc) (nth 2 lc))
                      (niori-pdf--code-put bs (nth 0 dc) 5)
                      (niori-pdf--bits-put bs (nth 1 dc) (nth 2 dc))
                      (setq i (+ i best-len)))
                  (let ((cc (niori-pdf--fixed-lit-code c0)))
                    (niori-pdf--code-put bs (car cc) (cdr cc)))
                  (puthash key (cons i (gethash key hash)) hash)
                  (setq i (1+ i)))))
            (let ((cc (niori-pdf--fixed-lit-code 256))) ; end of block
              (niori-pdf--code-put bs (car cc) (cdr cc)))
            (niori-pdf--bits-put bs 0 (% (- 8 (niori-pdf--bitsink-nbits bs)) 8))
            (let ((adler (niori-pdf--adler32 str)))
              (niori-pdf--bs-put bs (logand (ash adler -24) 255))
              (niori-pdf--bs-put bs (logand (ash adler -16) 255))
              (niori-pdf--bs-put bs (logand (ash adler -8) 255))
              (niori-pdf--bs-put bs (logand adler 255)))
            (niori-pdf--with-bitsink-buffer bs (buffer-string))))
      (kill-buffer (niori-pdf--bitsink-buf bs)))))

(defun niori-pdf-inflate (zlib-str)
  "Decompress ZLIB-STR with Emacs built-in zlib (used by tests and PNG).
Returns nil when Emacs was built without zlib support."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert zlib-str)
    (when (and (zlib-available-p)
               (zlib-decompress-region (point-min) (point-max)))
      (buffer-string))))

(defun niori-pdf-gzip-external (str)
  "Compress STR with external `niori-pdf-gzip-program'.
Return the raw DEFLATE payload (gzip container stripped), or nil on failure."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert str)
    (let ((ok (zerop (call-process-region (point-min) (point-max)
                                          niori-pdf-gzip-program
                                          t t nil "-c" "-9" "-n"))))
      (when (and ok (> (buffer-size) 18))
        (buffer-substring 11 (- (point-max) 7))))))

(defun niori-pdf-compress-stream-data (doc str)
  "Compress STR according to DOC's compression setting.
Return (DATA-STRING . FILTER-NAME-OR-NIL)."
  (setq str (encode-coding-string str 'binary))
  (if (< (length str) niori-pdf-deflate-min-size)
      (cons str nil)
    (pcase (niori-pdf-document-compression doc)
      (`raw (cons str nil))
      (`gzip (let ((gz (niori-pdf-gzip-external str)))
               (if gz
                   (cons gz 'FlateDecode)
                 (cons (niori-pdf-deflate str) 'FlateDecode))))
      (_ (cons (niori-pdf-deflate str) 'FlateDecode)))))

;;;; Info dictionary helpers

(defun niori-pdf--utf16-string (s)
  "Encode multibyte string S as a PDF UTF-16BE text string (with BOM)."
  (let* ((bytes (encode-coding-string (encode-coding-string s 'utf-16be) 'binary))
         (all (vconcat (concat "\xFE\xFF" bytes))))
    (niori-pdf-raw
     (concat "("
             (mapconcat (lambda (c) (format "\\%03o" c)) all "")
             ")"))))

(defun niori-pdf--pdf-date (&optional time)
  "Format TIME as a PDF date string D:YYYYMMDDHHmmSS with timezone."
  (let* ((t0 (or time (current-time)))
         (s (format-time-string "D:%Y%m%d%H%M%S" t0))
         (tz (format-time-string "%z" t0)))
    (concat "(" s (substring tz 0 3) "'" (substring tz 3 5) "')")))

(defun niori-pdf--build-info-dict (doc)
  "Build the document information dictionary for DOC.
The info plist is keyed by keywords (:title :author ...); both the
entry lookup and the Producer/Creator fallbacks below use the same
convention.  The creation time is memoized on the info plist under
:niori-pdf--time so the XMP packet built by niori-pdf-pdfa.el carries
byte-identical dates (ISO 19005 requires the Info dictionary and the
XMP metadata to agree)."
  (let* ((entries (niori-pdf-dict))
         (info (niori-pdf-document-info doc))
         (now (or (plist-get info :niori-pdf--time)
                  (let ((n (current-time)))
                    (setf (niori-pdf-document-info doc)
                          (plist-put info :niori-pdf--time n))
                    n))))
    (dolist (key '(:title :author :subject :keywords :creator :producer))
      (let ((v (plist-get info key)))
        (when (stringp v)
          (niori-pdf-dict-set
           entries (intern (capitalize (substring (symbol-name key) 1)))
           (niori-pdf--utf16-string v)))))
    (unless (plist-get info :producer)
      (niori-pdf-dict-set entries 'Producer
                          (niori-pdf--utf16-string
                           (concat "niori-pdf.el " niori-pdf-version
                                   " (port of cl-pdf)"))))
    (unless (plist-get info :creator)
      (niori-pdf-dict-set entries 'Creator
                          (niori-pdf--utf16-string
                           (concat "niori-pdf.el " niori-pdf-version))))
    (niori-pdf-dict-set entries 'CreationDate (niori-pdf--pdf-date now))
    (niori-pdf-dict-set entries 'ModDate (niori-pdf--pdf-date now))
    entries))

;;;; Page tree, resources and file assembly

(defvar niori-pdf--catalog-extensions nil
  "Alist of (DOC . FN).  FN is called with (DOC CATALOG-DICT) while
the catalog is assembled during `niori-pdf-save'; modules such as
document outlines or AcroForms use it to add their catalog entries.")

(defun niori-pdf-register-catalog-extension (doc fn)
  "Register FN as a catalog extension for DOC (see
`niori-pdf--catalog-extensions').  Later registrations replace
earlier ones per module symbol; FN receives (DOC CATALOG-DICT)."
  (let ((kv (assq fn niori-pdf--catalog-extensions)))
    (if (and kv (eq (car (cdr kv)) doc))
        (setcdr (cdr kv) nil)
      nil))
  (push (list fn doc) niori-pdf--catalog-extensions)
  nil)

(defun niori-pdf--dedupe-resource-entries (entries)
  "Return ENTRIES with duplicate resource names removed (first wins).
The document font alist can carry the same font object under several
spec keys -- a PDF/A base-14 substitution registers it both under the
base-14 name and under the substituted file -- and a resource
dictionary must list each name exactly once."
  (let ((seen nil) (out nil))
    (dolist (e entries)
      (unless (member (car e) seen)
        (push (car e) seen)
        (push e out)))
    (nreverse out)))

(defun niori-pdf--build-page-tree (doc)
  "Build catalog, page tree, shared resources and info objects for DOC.
Returns a plist (:catalog REF :info REF)."
  (let* ((font-entries
          (niori-pdf--dedupe-resource-entries
           (mapcar (lambda (kv)
                     (cons (intern (niori-pdf-font-resource (cdr kv)))
                           (niori-pdf-font-ref (cdr kv))))
                   (niori-pdf-document-fonts doc))))
         (image-entries
          (niori-pdf--dedupe-resource-entries
           (mapcar (lambda (kv)
                     (cons (intern (niori-pdf-image-resource (cdr kv)))
                           (niori-pdf-image-ref (cdr kv))))
                   (niori-pdf-document-images doc))))
         (eg-entries
          (mapcar (lambda (kv) (cons (intern (car kv)) (cdr kv)))
                  (niori-pdf-document-extgstates doc)))
         (font-dict-ref (if font-entries
                            (niori-pdf-add-object
                             doc (niori-pdf-dict-from-alist font-entries))))
         (xobj-dict-ref (if image-entries
                            (niori-pdf-add-object
                             doc (niori-pdf-dict-from-alist image-entries))))
         (eg-dict-ref (if eg-entries
                          (niori-pdf-add-object
                           doc (niori-pdf-dict-from-alist eg-entries))))
         (res (niori-pdf-dict)))
    (when font-dict-ref (niori-pdf-dict-set res 'Font font-dict-ref))
    (when xobj-dict-ref (niori-pdf-dict-set res 'XObject xobj-dict-ref))
    (when eg-dict-ref (niori-pdf-dict-set res 'ExtGState eg-dict-ref))
    (niori-pdf-dict-set res 'ProcSet
                        (niori-pdf-array "PDF" "Text" "ImageB" "ImageC" "ImageI"))
    (let ((res-ref (niori-pdf-add-object doc res))
          page-bodies)
      ;; page objects (chronological order); `reverse' because
      ;; `nreverse' would corrupt the document's page list
      (dolist (page (reverse (niori-pdf-document-pages doc)))
        (let* ((cc (niori-pdf-compress-stream-data
                    doc (niori-pdf-page-content-string page)))
               (sdict (niori-pdf-dict)))
          (niori-pdf-dict-set sdict 'Length (length (car cc)))
          (when (cdr cc) (niori-pdf-dict-set sdict 'Filter (cdr cc)))
          (let* ((stream (niori-pdf-stream :dict sdict :data (car cc)))
                 (content-ref (niori-pdf-add-object doc stream))
                 (annots-ref
                  (when (niori-pdf-page-annots page)
                    (niori-pdf-add-object
                     doc (apply #'niori-pdf-array
                                (reverse (niori-pdf-page-annots page))))))
                 (pd (niori-pdf-dict)))
            (niori-pdf-dict-set pd 'Type 'Page)
            (niori-pdf-dict-set pd 'MediaBox
                                (apply #'niori-pdf-array
                                       (niori-pdf-page-media-box page)))
            (niori-pdf-dict-set pd 'Resources res-ref)
            (niori-pdf-dict-set pd 'Contents content-ref)
            (when annots-ref (niori-pdf-dict-set pd 'Annots annots-ref))
            (when (niori-pdf-page-rotate page)
              (niori-pdf-dict-set pd 'Rotate (niori-pdf-page-rotate page)))
            (setf (niori-pdf-page-obj-num page)
                  (niori-pdf-ref-num (niori-pdf-add-object doc pd)))
            (push (cons page pd) page-bodies)
            ;; release content memory
            (setf (niori-pdf-page-content page) nil)
            (setf (niori-pdf-page-annots page) nil))))
      (setq page-bodies (nreverse page-bodies))
      ;; all page object numbers are known: resolve deferred links
      ;; (e.g. table-of-contents entries targeting later pages)
      (niori-pdf--resolve-pending-links doc)
      (let ((pages-dict (niori-pdf-dict))
            (pages-ref)
            (catalog-dict (niori-pdf-dict))
            (catalog-ref)
            (info-ref))
        (niori-pdf-dict-set pages-dict 'Type 'Pages)
        (niori-pdf-dict-set pages-dict 'Count (length page-bodies))
        (niori-pdf-dict-set
         pages-dict 'Kids
         (apply #'niori-pdf-array
                (mapcar (lambda (pb)
                          (niori-pdf-ref
                           (niori-pdf-page-obj-num (car pb))))
                        page-bodies)))
        (setq pages-ref (niori-pdf-add-object doc pages-dict))
        (dolist (pb page-bodies)
          (niori-pdf-dict-set (cdr pb) 'Parent pages-ref))
        (niori-pdf-dict-set catalog-dict 'Type 'Catalog)
        (niori-pdf-dict-set catalog-dict 'Pages pages-ref)
        (when (niori-pdf-document-page-mode doc)
          (niori-pdf-dict-set catalog-dict 'PageMode
                              (niori-pdf-document-page-mode doc)))
        (when (niori-pdf-document-page-layout doc)
          (niori-pdf-dict-set catalog-dict 'PageLayout
                              (niori-pdf-document-page-layout doc)))
        (dolist (ext niori-pdf--catalog-extensions)
          (when (eq (nth 1 ext) doc)
            (funcall (car ext) doc catalog-dict)))
        (setq catalog-ref (niori-pdf-add-object doc catalog-dict))
        (setq info-ref (niori-pdf-add-object doc (niori-pdf--build-info-dict doc)))
        (list :catalog catalog-ref :info info-ref)))))

(defun niori-pdf--resolve-pending-links (doc)
  "Fill in the /Dest arrays of DOC's deferred link annotations.
Each pending link is (ANNOT-DICT PAGE-NUM TOP): PAGE-NUM is the
1-based target page and TOP the destination y in PDF coordinates
(nil = keep the viewer's current position on that page)."
  (let* ((pages (niori-pdf-document-pages doc))
         (n (length pages)))
    (dolist (pl (niori-pdf-document-pending-links doc))
      (let* ((p (max 1 (min (nth 1 pl) n)))
             (page (nth (- n p) pages))
             (obj (niori-pdf-page-obj-num page)))
        (when obj
          (niori-pdf-dict-set
           (nth 0 pl) 'Dest
           (niori-pdf-array (niori-pdf-ref obj) 'XYZ 0 (nth 2 pl) nil)))))
    (setf (niori-pdf-document-pending-links doc) nil))
  nil)

(defun niori-pdf-save (doc file)
  "Write finished document DOC to FILE.  Return FILE.
When DOC is in PDF/A mode (`niori-pdf-document-pdfa-level') the file
header is %PDF-1.7 and the trailer carries the /ID array mandated by
ISO 19005; the /ID (an MD5 of the file body) is written for plain
documents too, which is harmless and makes every file uniquely
identifiable."
  (unless (niori-pdf-document-pages doc)
    (error "niori-pdf: document has no pages"))
  (dolist (fn (niori-pdf-document-save-hooks doc))
    (funcall fn doc))
  (let ((built (niori-pdf--build-page-tree doc))
        (objects (reverse (niori-pdf-document-objects doc)))
        (pdfa-p (niori-pdf-document-pdfa-level doc)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (if pdfa-p "%PDF-1.7\n" "%PDF-1.4\n")
              "%\xE2\xE3\xCF\xD3\n")
      (let ((offsets (make-vector (niori-pdf-document-n-objects doc) 0))
            (num 1))
        (dolist (body objects)
          (let ((start (1- (point))))
            (insert (format "%d 0 obj\n" num))
            (if (niori-pdf-stream-p body)
                (progn
                  (insert (niori-pdf--serialize (niori-pdf-stream-dict body)))
                  (insert "\nstream\n")
                  (insert (niori-pdf-stream-data body))
                  (insert "\nendstream\n"))
              (insert (niori-pdf--serialize body))
              (insert "\n"))
            (insert "endobj\n")
            (aset offsets (1- num) start)
            (setq num (1+ num))))
        (let* ((xref-pos (1- (point)))
               (count (1+ (length objects)))
               (id (md5 (buffer-substring (point-min) (point-max))
                        nil nil 'binary)))
          (insert (format "xref\n0 %d\n" count))
          (insert "0000000000 65535 f \n")
          (dotimes (i (length offsets))
            (insert (format "%010d 00000 n \n" (aref offsets i))))
          (insert (format "trailer\n<< /Size %d /Root %s /Info %s /ID [<%s> <%s>] >>\nstartxref\n%d\n%%%%EOF\n"
                          count
                          (niori-pdf--serialize (plist-get built :catalog))
                          (niori-pdf--serialize (plist-get built :info))
                          id id
                          xref-pos)))
        (let ((coding-system-for-write 'binary))
          (write-region (point-min) (point-max) file nil nil nil))
        file))))

(provide 'niori-pdf-core)
;;; niori-pdf-core.el ends here
