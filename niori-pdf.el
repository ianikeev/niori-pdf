;;; niori-pdf.el --- Generate PDF files in pure Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Igor Anikeev

;; Author: Igor Anikeev
;; Maintainer: Igor Anikeev
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: pdf, files, data, report
;; URL: https://github.com/ianikeev/niori-pdf

;; This file is part of niori-pdf.
;; The project combines original code with portions derived from
;; CL-PDF and CL-TYPESETTING.  See LICENSE and THIRD-PARTY-NOTICES
;; for the applicable notices and license terms.

;;; Commentary:

;; niori-pdf generates PDF files entirely in Emacs Lisp, with no external
;; programs (no LaTeX, no ghostscript, no headless browsers).  It is a
;; port of the core of Marc Battyani's Common Lisp library cl-pdf
;; (https://github.com/mbattyani/cl-pdf) together with a practical subset
;; of its cl-typesetting layout engine, adapted to idiomatic Elisp.
;;
;; It was written for niori.el so that a standalone Emacs-based
;; application can export data reports without any external toolchain.
;;
;; Features:
;;  * PDF object model + file writer with classic xref tables
;;  * Stream compression: pure Elisp DEFLATE (Emacs ships zlib
;;    decompression only), optional external gzip, or raw streams
;;  * Base-14 fonts (Helvetica, Times, Courier families, Symbol,
;;    ZapfDingbats) with real Adobe AFM metrics and WinAnsiEncoding
;;  * TrueType font embedding with glyph subsetting (Type0/CIDFontType2,
;;    Identity-H, ToUnicode maps) for arbitrary Unicode
;;  * JPEG pass-through (DCTDecode) and full PNG decoding
;;    (gray/RGB/RGBA/palette + alpha SMask)
;;  * Vector graphics: paths, Beziers, arcs, transforms, colors,
;;    transparency, clipping, link annotations
;;  * Flow typesetting engine: Knuth-Plass justified paragraphs with
;;    TeX-pattern hyphenation (English, German, French, Spanish),
;;    headings with keep-with-next, tables with
;;    repeated headers, images, boxes, multi-column rows, auto
;;    pagination, headers/footers, table of contents with page numbers
;;    and internal links, document outlines (bookmarks)
;;  * Bar codes: Code 128 (full cl-pdf port), Code 39 and EAN-13,
;;    plus a complete QR Code encoder (ISO 18004, versions 1-10,
;;    Reed-Solomon error correction, mask optimization)
;;  * Pie & bar charts, glyph outline drawing from TrueType and CFF
;;    (OTF) fonts
;;
;; Quick start:
;;
;;   (require 'niori-pdf)
;;   (niori-pdf-with-document (:title "Hello")
;;     (niori-pdf-with-page ()
;;       (niori-pdf-set-font "Helvetica-Bold" 24)
;;       (niori-pdf-draw-text 72 720 "Hello, PDF world"))
;;     (niori-pdf-save doc "/tmp/hello.pdf"))
;;
;; See the Info manual (niori-pdf.info) for the complete documentation,
;; and examples/ for working report generators.

;;; Code:

(require 'niori-pdf-core)
(require 'niori-pdf-metrics)
(require 'niori-pdf-type1)
(require 'niori-pdf-fonts)
(require 'niori-pdf-ttf)
(require 'niori-pdf-content)
(require 'niori-pdf-images)
(require 'niori-pdf-typeset)
(require 'niori-pdf-hyphen)
(require 'niori-pdf-flow)
(require 'niori-pdf-outlines)
(require 'niori-pdf-acroforms)
(require 'niori-pdf-barcodes)
(require 'niori-pdf-qrcode)
(require 'niori-pdf-charts)
(require 'niori-pdf-glyphs)
(require 'niori-pdf-pdfa)

;;;; Compatibility aliases for cl-pdf naming (optional sugar)

(defalias 'niori-pdf-set-rgb-stroke-color #'niori-pdf-set-rgb-stroke)
(defalias 'niori-pdf-set-rgb-fill-color #'niori-pdf-set-rgb-fill)

(provide 'niori-pdf)
;;; niori-pdf.el ends here
