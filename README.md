# niori-pdf

**Generate PDF files entirely in Emacs Lisp — no LaTeX, no Ghostscript, no
external processes.**

`niori-pdf` is an Emacs Lisp port of the core of
[cl-pdf](https://github.com/mbattyani/cl-pdf) (Marc Battyani's Common Lisp
PDF generator) together with a practical subset of its companion layout
engine [cl-typesetting](https://github.com/mbattyani/cl-typesetting).  It
was written so that [niori.el](…) — and any other standalone Emacs-based
application — can export data reports as PDF files without an external
toolchain.

Everything happens inside Emacs: the PDF object model, the file writer
with classic xref tables, stream compression, font metrics, TrueType
glyph subsetting, PNG decoding and the block layout engine are all
implemented in Elisp using built-in functions only.  The sole dependency
is Emacs itself (`emacs "27.1"`).

## Features

| Area | What you get |
|---|---|
| Writer | PDF 1.4 object model, dictionaries/arrays/streams, classic xref + trailer |
| Compression | pure-Elisp DEFLATE (RFC 1950/1951) producing `FlateDecode` streams; `raw` and external-`gzip` modes; built-in zlib used for *de*compression (PNG) |
| Fonts | base-14 (Helvetica, Times, Courier families, Symbol, ZapfDingbats) with real Adobe AFM metrics and `WinAnsiEncoding`; TrueType, OpenType/CFF and TrueType-collection (`.ttc`) embedding with glyph subsetting (`Type0`/`CIDFontType2` resp. `CIDFontType0`, `Identity-H`, `ToUnicode`; `.otf` CFF tables are subsetted at the charstring level); Type 1 `.pfb`/`.pfa` embedding (`/FontFile`, widths extracted from the eexec-decrypted CharStrings, no `.afm` needed) |
| Graphics | paths, Béziers, arcs/circles/ellipses, transforms, RGB/gray/CMYK colors, dash patterns, transparency (ExtGState), clipping, URI and internal link annotations |
| Images | JPEG pass-through (`DCTDecode`) and full PNG decoding (color types 0/2/3/4/6, bit depths 1/2/4/8/16→8, alpha as `/SMask`) |
| Layout (`niori-pdf-flow`) | Knuth-Plass justified paragraphs with TeX-pattern hyphenation in ten languages (American & British English, German, French, Spanish, Italian, Portuguese - one set covers pt-PT and pt-BR, Esperanto, Ukrainian, Russian), explicit-hyphen compound words per the TeX rule, styled runs with superscript/subscript, underline, strikethrough and run backgrounds, headings with keep-with-next, itemize/enumerate lists with hanging indents, footnotes (per-page auto numbering, LaTeX-style separator rule, text column shortened on the next multi-pass layout), floats with paragraph wrap-around, tables with wrapping cells, row/col spans, cell vertical alignment, row backgrounds, repeated header rows and `:space-after` (columns and tables wider than the text area are scaled to fit), images with captions, boxes/cards, multi-column rows, vertical glue (`vglue`/`vfill`) with page-level vertical justification, reference points resolved by multi-pass layout, automatic pagination, page header/footer templates, table of contents with page numbers and internal links, document bookmarks |
| Extras | document outlines (bookmarks), interactive AcroForm fields (text/check/choice/radio/button with generated appearance streams), Code 128 / Code 39 / EAN-13 bar codes, QR codes (full ISO 18004 encoder: versions 1-10, all four EC levels, Reed-Solomon over GF(256), eight masks with ISO penalty scoring - round-trip verified against an independent decoder), pie & bar charts, glyph outline drawing from TrueType and CFF (OTF) fonts, AFM kerning pairs, Symbol/ZapfDingbats native text encoding |
| Archival | PDF/A-2b output (ISO 19005-2): `%PDF-1.7` header, `/ID` trailer, XMP metadata mirroring the Info dictionary, `GTS_PDFA1` OutputIntent with an embedded pure-Elisp ICC sRGB profile (validated against lcms2), Print-flagged link annotations, all-fonts-embedded policy with configurable base-14 substitutions (`:pdfa t` in flow documents) |

## Installation

Until it is on MELPA, install from a checkout:

```elisp
(add-to-list 'load-path "/path/to/niori-pdf")
(require 'niori-pdf)
```

MELPA recipe (once the repository is public — adjust the owner):

```elisp
(niori-pdf :fetcher github :repo "ianikeev/niori-pdf")
```

The `Makefile` byte-compiles the library and builds the Info manual:

```sh
make            # compile + niori-pdf.info (requires makeinfo)
make test       # ERT test suite
make dist       # niori-pdf-0.1.0.tar.gz
```

## Quick start — primitives

```elisp
(require 'niori-pdf)

(niori-pdf-with-document (:title "Hello")
  (niori-pdf-with-page ()
    (niori-pdf-set-font "Helvetica-Bold" 24)
    (niori-pdf-draw-text 72 720 "Hello, PDF world")
    (niori-pdf-set-font "Helvetica" 12)
    (niori-pdf-draw-text 72 690 "Written in pure Emacs Lisp."))
  (niori-pdf-save niori-pdf--doc "/tmp/hello.pdf"))
```

## Quick start — reports

```elisp
(niori-pdf-flow-generate
 (lambda ()
   (niori-pdf-with-flow-document (:title "Q4 report"
                                  :footer-fn #'niori-pdf-flow-page-footer)
     (niori-pdf-flow-heading "Q4 report" :level 1 :size 24)
     (niori-pdf-flow-paragraph
      "Justified text with *real* font metrics, wrapped and paginated
automatically.  Tables, images and boxes flow around it."
      :align 'justified)
     (niori-pdf-flow-itemize '("first point" "second point"))
     (niori-pdf-flow-table
      (list :header ["Region" "Revenue"]
            :rows '(("North" "156") ("South" "134"))
            :cols '(200 120)))
     (niori-pdf-flow-toc)))
 (niori-pdf-flow-save niori-pdf--doc "/tmp/report.pdf"))
```

When `niori-pdf-flow-toc` or reference points are used,
`niori-pdf-flow-generate` runs the builder again (at most six passes)
until every page number in the output is stable.

## Quick start — QR codes

```elisp
(require 'niori-pdf)

(niori-pdf-with-document (:title "QR demo")
  (niori-pdf-with-page ()
    ;; encode + draw in one call (top-left module corner at 72,700)
    (niori-pdf-barcode-qr-string "https://niori.example.com"
                                 72 700 :point-size 2.5 :ecl 'M))
  (niori-pdf-save niori-pdf--doc "/tmp/qr.pdf"))

;; or work with the raw matrix:
(niori-pdf-qr-encode "Hello" :ecl 'H)   ; => ((1 1 1 ...) ...) . 1
```

## Quick start — PDF/A archival documents

```elisp
(require 'niori-pdf)

;; the base-14 fonts cannot be embedded (ISO 19005), so map them to
;; real font files for archival documents:
(setq niori-pdf-pdfa-base14-substitutions
      '(("Helvetica"      . "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf")
        ("Helvetica-Bold" . "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf")
        ("Courier"        . "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")))

(niori-pdf-flow-generate
 (lambda ()
   (niori-pdf-with-flow-document (:title "Archival report" :pdfa t)
     (niori-pdf-flow-heading "Archival report" :level 1)
     (niori-pdf-flow-paragraph "Every font embedded, ICC output intent,
XMP metadata, PDF 1.7 with an /ID trailer.  Renders identically for
decades.")))
 (niori-pdf-flow-save niori-pdf--doc "/tmp/archival.pdf"))
```

Or at the primitive level: call `(niori-pdf-pdfa-enable doc)` before
drawing and save as usual.  DeviceCMYK color is rejected in PDF/A
documents (the built-in OutputIntent is sRGB); RGB, grayscale,
transparency and link annotations are all fine in PDF/A-2b.

## Compression and zlib — how it works

This is the one surprising area, so it is documented in detail.

* GNU Emacs bundles zlib **for decompression only**.  The Elisp API
  exposes `zlib-available-p` and `zlib-decompress-region`; there is no
  built-in compression primitive (you will not find `zlib-insert`).
* niori-pdf therefore ships a small pure-Elisp DEFLATE implementation
  (`niori-pdf-deflate`, ~200 lines in `niori-pdf-core.el`): fixed-Huffman
  codes + LZ77 hash chains, producing standard RFC 1950 zlib streams that
  every PDF reader accepts as `/Filter /FlateDecode`.  It is verified
  against Emacs's own zlib decompressor in the test suite.
* `niori-pdf-compression` selects the strategy:
  - `'deflate` — the Elisp implementation (default; always available)
  - `'raw` — no compression
  - `'gzip` — pipe streams through an external `gzip` binary (opt-in;
    its raw DEFLATE payload is used so the result is still a valid
    `FlateDecode` stream).  This breaks the standalone-Emacs guarantee
    and is only interesting for very large documents.
* Decompression (PNG IDAT inflation, test round-trips) uses Emacs
  built-in `zlib-decompress-region` when available and falls back to an
  error message otherwise.

Because the DEFLATE implementation is pure Elisp, no Emacs rebuild or
extra library is needed — the standalone application stays standalone.

## Status, scope and roadmap

Ported and working: document/page writer, xref, compression, base-14
fonts with AFM metrics + kerning pairs + WinAnsi and native
Symbol/ZapfDingbats encodings, TrueType subsetting/embedding, CFF/OTF
embedding (`FontFile3`/`OpenType`) *with* CFF subsetting (charstrings,
charset, DICTs and subroutines reduced to the glyphs actually used;
subsets are emitted with compact CFF 1.0 DICT integers and, for
CIDFontType0 embedding, forced CID-keyed with a synthetic identity
charset, FDSelect and FDArray, so strict parsers accept them),
TrueType collections (`.ttc` with member-index selection), Type 1
font embedding (`.pfb`/`.pfa`: segment/eexec/hex parsing, charstring
width extraction, `FontFile` with Length1/2/3), JPEG and
PNG images, vector graphics primitives, link annotations, document
outlines, AcroForms, bar codes, QR codes, charts, glyph outline
drawing, PDF/A-2b archival output, and the flow engine (Knuth-Plass
paragraphs, hyphenation in ten languages, run decorations, lists,
footnotes, floats with wrap-around, tables with spans and vertical
alignment, vertical glue and justification, reference points with
multi-pass resolution, headings, images, boxes, columns, TOC,
pagination, templates).

Not yet ported from cl-pdf / cl-typesetting (roadmap):

* Font shaping tables (GSUB/GPOS) — base-14 + Identity-H coverage only
* Math module (`display-formula`, `fraction`)
* Floats that survive page breaks (current floats are page-local)

As a full-scale showcase, `examples/alice-book.el` typesets Lewis
Carroll's *Alice's Adventures in Wonderland* as a complete A5 book
(87 pages): serif body in URW P052 at 9.7/13.6 with Knuth-Plass
justification and hyphenation, sans-serif chapter openers and running
heads, raised initials, centered verse, the mouse's tail-tale restored
to the spiral layout of the original 1865 edition, a linked table of
contents, PDF bookmarks and a QR-linked colophon — all four text faces
are CFF-subsetted OpenType fonts:

```sh
emacs --batch --load examples/alice-book.el
```

See `niori-pdf-evaluation.pdf` for the analysis that led to the
cl-pdf port (itself generated by `examples/cl-pdf-evaluation.el`),
and the Info manual (`docs/niori-pdf.texi` → `niori-pdf.info`) for
the complete reference.  `examples/sample-report.el` generates a complete
nine-page business report using every major feature — its output is a
good smoke test:

```sh
emacs --batch --load examples/sample-report.el
```

## Credits

`niori-pdf` combines original Emacs Lisp implementations with portions
ported from [CL-PDF](https://github.com/mbattyani/cl-pdf) and
[CL-TYPESETTING](https://github.com/mbattyani/cl-typesetting), both by
Marc Battyani.  The CL-PDF barcode source credits Lars Rustemeier for
Code 128 and Carlos Ungil for the `plot-xy` contribution.  Base-14 font
metrics are derived from Adobe Core 14 AFM data.  See
`THIRD-PARTY-NOTICES` for the applicable acknowledgments and notices.

## License

Original code written for `niori-pdf` by Igor Anikeev is distributed under
the BSD 2-Clause License, unless a file states otherwise.  Portions derived
from CL-PDF and CL-TYPESETTING retain their respective upstream licenses
and copyright notices.  Some bundled data has separate terms.  See
`LICENSE` and `THIRD-PARTY-NOTICES`; the latter also identifies material
whose licensing review remains outstanding.
