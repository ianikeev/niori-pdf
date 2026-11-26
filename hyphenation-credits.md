Hyphenation pattern credits, licenses and conversion pipeline for niori-pdf
===========================================================================

Every statement below was verified against the actual source files on
2026-11-26 (headers quoted verbatim where they matter).  The four classic
files bundled with cl-typesetting ship WITHOUT their license headers
(cl-typesetting's own license.txt is silent about them), so the
attribution comes from the upstream TeX sources.

The upstream reference copies used for verification live in
scripts/hyphen-sources/ (tex-hyphen shallow clone + downloaded legacy
files); the converter is scripts/gen_hyphen.py.

1. VERIFIED PROVENANCE TABLE
----------------------------

| language   | pattern file   | upstream source                    | author(s)                                                                 | license            | lmin/rmin |
|------------|----------------|------------------------------------|---------------------------------------------------------------------------|--------------------|-----------|
| american   | ushyph.txt     | plain TeX hyphen.tex + Kuiken additions | Frank M. Liang (classic set); Gerard D.C. Kuiken (1990 exception-log additions) | see 2.1            | 2/3       |
| british    | gbhyph.txt     | ukhyphen.tex (tex-hyphen old/hyphen) | Dominik Wujastyk, Graham Toal (patterns); OUP word list (not redistributed) | see 2.2            | 2/3       |
| german     | dehyphn.txt    | dehyphn.tex rev 31                 | Rechenzentrum Ruhr-Univ. Bochum 1988,1991; Bernd Raichle/DANTE 1993-1999; Walter Schmidt 1998-2001 | LPPL 1+            | 2/2       |
| french     | frhyph.txt     | frhyph.tex v2.12 (2002/12/11)      | Jacques Desarmenien; Norman Buckle; Michael Ferguson; Justin Bur; GUTenberg; Daniel Flipo; Bernard Gaulle | see 2.3            | 2/3       |
| spanish    | hyph-es.tex    | hyph-utf8 v5.0 2019-09-24          | Javier Bezos, CervanTeX (C) 1993,1997,2001-2019                            | MIT/X11            | 2/2       |
| italian    | hyph-it.tex    | hyph-utf8 v4.9 2014/04/22          | Claudio Beccari (C) 2008-2011                                              | LPPL 1.3+ OR MIT   | 2/2       |
| portuguese | hyph-pt.tex    | hyph-utf8 v1.4 2024-07-13          | Pedro J. de Rezende, J. Joao Dias Almeida, Leonardo Araujo, Aline Benevides | BSD 3-clause       | 2/3       |
| esperanto  | hyph-eo.tex    | hyph-utf8 v1.0 1999-08-10          | Sergei B. Pokrovsky (Sergio Pokrovskij) (C) 1999                           | LPPL 1+            | 2/2       |
| russian    | hyph-ru.tex    | hyph-utf8 v2003/03/10              | Alexander I. Lebedev (C) 1999-2003                                         | LPPL 1.2+          | 2/2       |
| ukrainian  | hyph-uk.tex    | hyph-utf8 v2001/05/10              | Maksym Polyakov (C) 1998-2001                                              | MIT OR LPPL        | 2/2       |

Content verification performed (pattern token sets, cl-typesetting copy vs
upstream):

- gbhyph.txt == ukhyphen.tex exactly: 8527 patterns identical, plus its 8
  \hyphenation exceptions.  Both cl-typesetting copies (gbhyph.txt and
  ukhyph.txt) are the same set; the "gbhyph" NAME is what satisfies
  ukhyphen.tex's renaming clause (see 2.2).
- dehyphn.txt == dehyphn.tex in its 8-bit (EC) branch: 5801 patterns, ZERO
  unique patterns after expanding TeX macros (\n{...} identity wrapper,
  \"o-accents, german.sty "a shorthands, \3 = ss).  The 72 remaining
  upstream-only tokens are all \c{...\9...} wrappers of a different
  encoding branch, dropped in the 8-bit rendering.
- frhyph.txt == frhyph.tex: 1146 patterns both sides; 11 tokens differ only
  in the oe ligature encoding (\oe -> ^^bd), and one token has a letter
  difference: the cl-typesetting copy reads .mono1ï2dé where upstream
  frhyph.tex reads .mono1i2dé.
- ushyph.txt is a historical snapshot of the Kuiken-extended Liang set:
  classic hyphen.tex (4447 patterns) + 377 additional patterns from the
  Hyphenation Exception Log lineage (4564 of its 4824 patterns also appear
  in the Kuiken-copyrighted modern hyph-en-us.tex / ushyphmax.tex sets).
  It is not byte-identical to any single current upstream file.

The six hyph-utf8 languages (es/it/pt/eo/ru/uk) come from
https://github.com/hyphenation/tex-hyphen (hyph-utf8 package,
see also http://www.hyphenation.org/tex).

2. LICENSE TEXTS TO REPRODUCE
-----------------------------

2.1 american (ushyph.txt)
    Two layers, both permissive, both require the notice to be preserved:

    - classic part (hyphen.tex, Liang's patterns, no copyright holder named
      in the file):
        % The Plain TeX hyphenation tables [NOT TO BE CHANGED IN ANY WAY!]
        % Unlimited copying and redistribution of this file are permitted as long
        % as this file is not modified. Modifications are permitted, but only if
        % the resulting file is not named hyphen.tex.
    - additions (Kuiken lineage; the notice carried by ushyphmax.tex and the
      modern hyph-en-us.tex):
        Copyright (C) 1990, 2004, 2005 Gerard D.C. Kuiken.
        Copying and distribution of this file, with or without modification,
        are permitted in any medium without royalty provided the copyright
        notice and this notice are preserved.

2.2 british (gbhyph.txt == ukhyphen.tex)
    The original conditions must be reproduced for any redistributed
    modified copy ("must contain conditions identical to these"); the
    original file may be redistributed unmodified freely:

        Unlimited copying and redistribution of this file
        is permitted so long as the file is not modified
        in any way.
        Modifications may be made for private purposes (though
        this is discouraged, as it could result in documents
        hyphenating differently on different systems) but if
        such modifications are re-distributed, the modified
        file must not be capable of being confused with the
        original.  In particular, this means
        (a) the filename (the portion before the extension, if any)
            must not match any of :
                UKHYPH      UK-HYPH
                UKHYPHEN    UK-HYPHEN
                UKHYPHENS   UK-HYPHENS
                UKHYPHENATION UK-HYPHENISATION UK-HYPHENIZATION
        (b) the file must contain conditions identical to these.

    Attribution header of ukhyphen.tex: "Created by Dominik Wujastyk and
    Graham Toal using Frank Liang's PATGEN 1.0", based on an Oxford
    University Press list of 114925 hyphenated words; "This file of
    hyphenation patterns may be freely distributed."  (The OUP word list
    itself is copyrighted and NOT redistributed; only the derived patterns
    are.)

2.3 french (frhyph.tex)
        This file is available for free and can used and redistributed
        asis for free. Modified versions should have another name.

3. SUGGESTED REPO TEXTS (paste-ready)
-------------------------------------

3.1 For the data file header (niori-pdf-hyphen-data.el), replace the
    provenance comment with:

;; Hyphenation pattern data for niori-pdf (Liang/TeX algorithm).
;;
;; Sources and licenses (original files ship in patterns/):
;;   american   ushyph.txt  - plain TeX hyphenation tables (F.M. Liang's
;;              classic set) extended with patterns from the Hyphenation
;;              Exception Log work of G.D.C. Kuiken (1990).  Unlimited
;;              redistribution permitted; modified copies must not be
;;              named hyphen.tex (hyphen.tex notice); the additions carry
;;              "Copyright (C) 1990, 2004, 2005 Gerard D.C. Kuiken" and a
;;              royalty-free redistribution notice.
;;   british    gbhyph.txt  - ukhyphen.tex by Dominik Wujastyk and Graham
;;              Toal (patterns freely distributed; renamed per the
;;              original's own conditions; see patterns/README).
;;   german     dehyphn.txt  - dehyphn.tex rev 31 (new German orthography);
;;              (C) 1988,1991 Rechenzentrum der Ruhr-Universitaet Bochum;
;;              (C) 1993,1994,1999 Bernd Raichle/DANTE e.V.;
;;              (C) 1998-2001 Walter Schmidt.  LPPL 1+.
;;   french     frhyph.txt  - frhyph.tex v2.12; Desarmenien/Buckle/
;;              Ferguson/Bur/Flipo/Gaulle (GUTenberg).  Free use and
;;              redistribution as-is; modified versions renamed.
;;   spanish    hyph-es.tex  v5.0 - (C) 1993,1997,2001-2019 Javier Bezos,
;;              CervanTeX.  MIT/X11.
;;   italian    hyph-it.tex  v4.9 - (C) 2008-2011 Claudio Beccari.
;;              LPPL 1.3+ or MIT.
;;   portuguese hyph-pt.tex  v1.4 - (C) 1987,1994,1996,2015 Pedro J. de
;;              Rezende, 1996,2015 J. Joao Dias Almeida, 2024 Leonardo
;;              Araujo and Aline Benevides.  BSD 3-clause.
;;   esperanto  hyph-eo.tex  v1.0 - (C) 1999 Sergei B. Pokrovsky.  LPPL 1+.
;;   russian    hyph-ru.tex  v2003/03/10 - (C) 1999-2003 Alexander I.
;;              Lebedev.  LPPL 1.2+.
;;   ukrainian  hyph-uk.tex  v2001/05/10 - (C) 1998-2001 Maksym Polyakov.
;;              MIT or LPPL.
;;
;; es/it/pt/eo/ru/uk are from the TeX Live hyph-utf8 project
;; (https://github.com/hyphenation/tex-hyphen, http://hyphenation.org).

3.2 For LICENSE (add after the Adobe AFM paragraph):

  Hyphenation pattern data in niori-pdf-hyphen-data.el is derived from
  the TeX hyphenation pattern files listed in the header of that file;
  the original, unmodified pattern files are distributed in patterns/
  together with their own copyright and license statements (MIT/X11,
  BSD 3-clause, LPPL, and permissive redistribution notices; see
  patterns/README for details and full texts).

3.3 README section (extend the hyphenation part):

### Hyphenation

Paragraph hyphenation uses the classic Knuth/Liang pattern algorithm with
TeX pattern sets for american/british/german/french/spanish/italian/
portuguese/esperanto/russian/ukrainian.  The original TeX pattern files
and the generator that converts them into niori-pdf-hyphen-data.el are
shipped in patterns/ (see patterns/README for provenance, licenses and
the conversion pipeline), so the data file can be regenerated or extended
with further languages offline.

4. THE CONVERSION PIPELINE (what gen_hyphen.py does)
----------------------------------------------------

Input formats, two kinds:

  a) classic plain-TeX files (ushyph.txt, dehyphn.txt, frhyph.txt,
     gbhyph.txt): one pattern per line, possibly prefixed by a lone
     "%" comment line; \patterns{...}/\hyphenation{...} introduced by
     bare "patterns"/"exceptions" keyword lines in the .txt rendering.
     Encodings are latin-1 (umlauts are literal bytes).
  b) hyph-utf8 .tex files (hyph-es/it/pt/eo/ru/uk.tex): UTF-8, metadata
     in % comments, one big \patterns{ ... } block, \hyphenation{ ... }
     for exceptions.  Comments (and inline comments) are stripped; a TeX
     pattern never contains a literal %.

Token to data mapping (Liang semantics):

  - Each pattern token interleaves letters and digits, e.g. ".ach4",
    "hy3ph", "o2n1cy".  A digit belongs to the GAP AFTER the letters seen
    so far (gap 0 = before the first letter, gap N = after the N-th).
  - The generator emits a cons (LETTERS . DIGITS) where LETTERS is the
    letter sequence and DIGITS is a (len+1)-character string of the gap
    values, "." marking "no value".  Example: ".ach4" -> (".ach" . "....4").
  - Values >= 10 (written as letters a..p by TeX's \patterns) do not occur
    in the shipped sets, so single ASCII digits suffice; a pattern whose
    trailing gap exceeds its letter count is rejected with a warning.
  - Exceptions are kept as the original hyphenated words ("as-so-ciate");
    the runtime converts them to break indices.
  - Leftmin/rightmin come from each file's own % hyphenmins metadata
    (verified: es/it/eo/ru/uk) or the babel conventions for the classic
    sets (american 2/3, british 2/3, french 2/3, german 2/2).

Output: one defconst per language, niori-pdf-hyphen-data--LANG =
  (LEFTMIN RIGHTMIN ( (LETTERS . DIGITS) ... ) ( EXCEPTION ... ))
plus niori-pdf-hyphen-data--languages.  Languages are generated in
sorted order, so regeneration is byte-stable (diff-able).

5. RUNTIME INCORPORATION (how the Elisp side consumes it)
---------------------------------------------------------

niori-pdf-hyphen.el (require 'niori-pdf-hyphen-data):

  1. niori-pdf-hyphen-load-language compiles a language once:
     - patterns go into a hash bucketed by FIRST character (bucket key
       is the first letter of LETTERS, "." included), so matching only
       scans candidate patterns per position;
     - exceptions go into a hash word -> list of break indices (computed
       from the "as-so-ciate" form) or the marker 'none for bare words
       (meaning "never hyphenate").
  2. niori-pdf-hyphenate-word (memoized per LANG+lowercased word):
     - exceptions win outright;
     - words shorter than min-l + min-r are skipped;
     - otherwise Liang matching runs over "." + word + "." : for every
       position i, each pattern in the bucket of padded[i] whose LETTERS
       equal substring(padded, i, i+len) contributes its DIGITS values;
       per-gap values merge by MAXIMUM.
  3. Break points are the gaps with ODD maximal value, restricted to
     >= min-l characters before and >= min-r characters after the break.
  4. niori-pdf-hyphen-word-splits cuts the word into fragments;
     niori-pdf-hyphenate-string applies the same to every alphabetic
     run of a longer string.

niori-pdf-flow.el integration (paragraph building):

  - each word becomes a list of fragment boxes joined by flagged
    penalties of value 50, penalty width = width of "-" in the current
    font (niori-pdf-hyphen-word-splits at flow.el:492);
  - words containing explicit hyphens break only at the hyphens
    (patterns disabled, as in TeX);
  - the Knuth-Plass breaker (niori-pdf-typeset.el) runs a first pass
    ignoring flagged penalties, then a second pass with them, charging
    final-hyphen-demerits 5000 / double-hyphen-demerits 10000;
  - when a line actually ends on a hyphenated fragment, the renderer
    emits the trailing "-" glyph (flow.el:756).

The per-paragraph language comes from the flow's :language (default
niori-pdf-hyphen-default-language, 'american).

6. ADDING A NEW LANGUAGE
------------------------

  1. Put the TeX pattern file (UTF-8 hyph-utf8 style preferred) in
     patterns/, read its % hyphenmins metadata for leftmin/rightmin.
  2. Add a LANGS entry in patterns/gen-hyphen.py (file, lmin, rmin).
  3. Run: python3 patterns/gen-hyphen.py   (regenerates
     niori-pdf-hyphen-data.el; sorted output keeps diffs small).
  4. In niori-pdf-hyphen.el: add a pcase branch mapping the language
     symbol to the new defconst.
  5. Sanity test: ert for hyphenation + hyphenate a known word
     (e.g. niori-pdf-hyphenate-word "hyphenation" => (2 6 8) american).
  6. Add the author/license line to the data-file header and patterns/
     README - keep the original file unmodified in patterns/ so that
     "unmodified redistribution" style licenses stay satisfied.

7. WHERE THINGS LIVE (this sandbox)
-----------------------------------

  scripts/gen_hyphen.py                    the converter (Portuguese
                                           license line corrected 2026-11-26:
                                           it is BSD 3-clause, not LPPL)
  scripts/hyphen-sources/hyph-*.tex        the 6 hyph-utf8 originals
  scripts/hyphen-sources/tex-hyphen/       shallow clone of
                                           github.com/hyphenation/tex-hyphen
  scripts/hyphen-sources/tex-hyphen/old/hyphen/
                                           hyphen.tex, ukhyphen.tex,
                                           dehyphn.tex, frhyph.tex
  research/cl-typesetting/hyphen-patterns/ the copies bundled with
                                           cl-typesetting (headerless)
  scripts/cmp_patterns.py                  the verification script used
                                           for the equivalence checks above
