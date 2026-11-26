# Hyphenation pattern sources

Upstream pattern files from which `niori-pdf-hyphen-data.el` is generated
by `../gen_hyphen.py`.  Each file is kept **verbatim** as obtained
upstream; the six `hyph-*.tex` files carry their own license header
(hyph-utf8 YAML metadata) inside the file itself.  This README records
author, version, license and origin of every file, as required by those
licenses.

## The two source families

1. **Classic TeX sets (`.txt`, latin-1/latin-9 encoded)** --
   `ushyph.txt`, `dehyphn.txt`, `frhyph.txt`, `gbhyph.txt` were taken
   from the `hyphen-patterns/` directory of the cl-typesetting
   distribution (Marc Battyani's Common Lisp project, the upstream this
   whole package ports).  Those files are bare pattern lists without
   license headers; they are the long-standing freely redistributable
   TeX pattern sets whose authorship is documented below.  Modern,
   maintained equivalents with explicit licenses live in the hyph-utf8
   project (see the "Modern equivalent" column).

2. **hyph-utf8 sets (`.tex`, UTF-8)** -- the six `hyph-*.tex` files come
   from the TeX Live hyph-utf8 project,
   <https://github.com/hyphenation/tex-hyphen>
   (path:
   `hyph-utf8/tex/generic/hyph-utf8/patterns/tex/`).  Each file embeds
   its copyright/author/license metadata; nothing here is asserted from
   memory.

## Per-language credits

| Language (elisp name) | File | Author(s) | Version | License | Modern equivalent |
|---|---|---|---|---|---|
| american | `ushyph.txt` | Frank M. Liang (Stanford dissertation, 1983) | the set TeX has shipped since 1983 | freely redistributable classic TeX pattern set | `hyph-en-us.tex` |
| german | `dehyphn.txt` | German hyphenation team: Walter A. Schmidt, Bernd Raichle et al. | classic "dehyphn" (1996 orthography) | freely redistributable classic TeX pattern set | `hyph-de-1996.tex` (MIT, Deutschsprachige Trennmustermannschaft) |
| french | `frhyph.txt` | Raymond Seroul lineage, maintained by Bernard Gaulle | classic French set | freely redistributable classic TeX pattern set | `hyph-fr.tex` (MIT, Flipo/Gaulle/Reutenauer) |
| british | `gbhyph.txt` | Peter Abbott, maintained by Dominik Wujastyk | classic British set | freely redistributable classic TeX pattern set | `hyph-en-gb.tex` (MIT, Wujastyk/Toal) |
| spanish | `hyph-es.tex` | Javier Bezos, CervanTeX | 5.0 (2019-09-24) | MIT/X11 | same |
| italian | `hyph-it.tex` | Claudio Beccari | 4.9 (2014-04-22) | LPPL 1.3+, or MIT | same |
| portuguese | `hyph-pt.tex` | Pedro J. de Rezende, J. Joao Dias Almeida, Leonardo Araujo, Aline Benevides | 1.4 (2024-07-13) | BSD 3-clause | same |
| esperanto | `hyph-eo.tex` | Sergio (Sergei B.) Pokrovskij | 1.0 (1999-08-10) | LPPL 1+ | same |
| ukrainian | `hyph-uk.tex` | Maksym Polyakov | 2001-05-10 | MIT, or LPPL | same |
| russian | `hyph-ru.tex` | Alexander I. Lebedev | 2003-03-10 | LPPL 1.2+ | same |

Notes on individual sets:

- **spanish**: one pattern set covers all Spanish variants
  (es-ES, es-MX, es-AR, ...): the RAE/ASALE orthography is common, so a
  single `spanish` language is shipped.
- **portuguese**: one set covers both European and Brazilian
  Portuguese (pt-PT and pt-BR).
- **british**: the hyph-utf8 `hyph-en-gb.tex` header notes that the
  *source word list* used to create the patterns is copyright the
  Oxford University Press; the pattern file itself is MIT-licensed.
  The same provenance caveat is inherited by the classic `gbhyph.txt`
  lineage.
- **russian**: ships 184 `\hyphenation{}` exceptions in addition to
  patterns -- they are part of the upstream file and reproduced in the
  generated data.

## License compliance

All of these licenses (MIT/X11, MIT, BSD-3, LPPL, and the unrestricted
classic TeX pattern sets) permit redistribution with attribution.
This repository satisfies attribution by (a) shipping the upstream
files verbatim with their embedded headers, (b) the table above, and
(c) the per-language credits emitted into the header of the generated
`niori-pdf-hyphen-data.el`.  LPPL-licensed files (italian, esperanto,
russian) are distributed as *independent pattern files* -- separate
documents, not modified LaTeX code -- which LPPL expressly allows; if
you modify a pattern file, rename it per LPPL requirements.

## Where the runtime hooks in

`niori-pdf-hyphen.el` consumes `(LEFTMIN RIGHTMIN PATTERNS EXCEPTIONS)`
per language; see `../gen_hyphen.py` for the exact on-disk-to-Elisp
transformation and `../README.md` ("Extending") for adding a language.
