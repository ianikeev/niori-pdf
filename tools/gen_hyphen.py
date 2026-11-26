#!/usr/bin/env python3
"""gen_hyphen.py -- regenerate niori-pdf-hyphen-data.el from upstream TeX
hyphenation pattern files.

This is the reproducibility pipeline for the pattern data baked into
niori-pdf-hyphen-data.el.  Running it parses the upstream files under
tools/hyphen-sources/ and rewrites niori-pdf-hyphen-data.el in the
repository root, so the shipped Elisp data can always be rebuilt from
first sources -- no binary blobs, no unverifiable provenance.

Usage:
  python3 tools/gen_hyphen.py             # rewrite niori-pdf-hyphen-data.el
  python3 tools/gen_hyphen.py --check     # verify shipped file is current;
                                          # exit 1 + diff summary if not
  python3 tools/gen_hyphen.py --lang french   # print one language to stdout

Reproducibility check (byte-identical output expected):
  python3 tools/gen_hyphen.py --check

Adding a new language: see tools/README.md, section "Adding a language".

Pattern semantics (Liang/TeX):
  A pattern like "ach4" carries letter sequence "ach" and a value 4 at
  the gap after 3 letters (gap index counts every letter of the pattern,
  including a leading '.').  For each pattern we emit a cons
  (LETTERS . DIGITS) where DIGITS is a string of digits padded to
  len(LETTERS)+1 positions, using '.' for "no value" positions.  Digits
  10+ (only in some pattern sets) map to letters a..p as in TeX's
  \\patterns; the sets shipped here use single digits only.

Output format: one defconst per language
  (LEFTMIN RIGHTMIN PATTERNS EXCEPTIONS)
consumed by niori-pdf-hyphen.el, which compiles it into per-first-letter
buckets at load time.
"""
import re, os, sys, argparse

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(REPO, "tools", "hyphen-sources")
OUT = os.path.join(REPO, "niori-pdf-hyphen-data.el")

# language -> metadata.  file is relative to tools/hyphen-sources/.
# left/right are the TeX \lefthyphenmin/\righthyphenmin (shipped data
# uses cl-typesetting's values for the four legacy sets and the
# hyph-utf8 \hyphenmins for the others).
LANGS = {
    "american": dict(
        file="ushyph.txt", parser="legacy", left=2, right=3,
        origin="classic TeX set (Frank M. Liang, Stanford dissertation 1983), "
               "as shipped in cl-typesetting's hyphen-patterns/",
        authors="Frank M. Liang",
        license="freely redistributable TeX pattern set (see "
                "tools/hyphen-sources/README.md)"),
    "german": dict(
        file="dehyphn.txt", parser="legacy", left=2, right=2,
        origin="classic 'dehyphn' set for the 1996 German orthography, "
               "as shipped in cl-typesetting's hyphen-patterns/",
        authors="German hyphenation team (Walter A. Schmidt, Bernd Raichle et al.)",
        license="freely redistributable TeX pattern set (see "
                "tools/hyphen-sources/README.md)"),
    "french": dict(
        file="frhyph.txt", parser="legacy", left=2, right=3,
        origin="classic French set (Raymond Seroul lineage, maintained by "
               "Bernard Gaulle), as shipped in cl-typesetting's hyphen-patterns/",
        authors="Raymond Seroul, Bernard Gaulle",
        license="freely redistributable TeX pattern set (see "
                "tools/hyphen-sources/README.md)"),
    "british": dict(
        file="gbhyph.txt", parser="legacy", left=2, right=3,
        origin="classic British set (Peter Abbott, maintained by Dominik "
               "Wujastyk), as shipped in cl-typesetting's hyphen-patterns/",
        authors="Peter Abbott, Dominik Wujastyk",
        license="freely redistributable TeX pattern set (see "
                "tools/hyphen-sources/README.md)"),
    "spanish": dict(
        file="hyph-es.tex", parser="hyph-utf8", left=2, right=2,
        origin="hyph-utf8 hyph-es.tex v5.0 (2019-09-24); one pattern set "
               "covers all Spanish variants (RAE/ASALE orthography is common "
               "to es-ES, es-MX, es-AR, ...)",
        authors="Javier Bezos, CervanTeX",
        license="MIT/X11",
        url="https://github.com/hyphenation/tex-hyphen"),
    "italian": dict(
        file="hyph-it.tex", parser="hyph-utf8", left=2, right=2,
        origin="hyph-utf8 hyph-it.tex v4.9 (2014-04-22)",
        authors="Claudio Beccari",
        license="LPPL 1.3+ or MIT",
        url="https://github.com/hyphenation/tex-hyphen"),
    "portuguese": dict(
        file="hyph-pt.tex", parser="hyph-utf8", left=2, right=3,
        origin="hyph-utf8 hyph-pt.tex v1.4 (2024-07-13); one set for pt-PT "
               "and pt-BR",
        authors="Pedro J. de Rezende, J. Joao Dias Almeida, Leonardo Araujo, "
                "Aline Benevides",
        license="BSD 3-clause",
        url="https://github.com/hyphenation/tex-hyphen"),
    "esperanto": dict(
        file="hyph-eo.tex", parser="hyph-utf8", left=2, right=2,
        origin="hyph-utf8 hyph-eo.tex v1.0 (1999-08-10)",
        authors="Sergio (Sergei B.) Pokrovskij",
        license="LPPL 1+",
        url="https://github.com/hyphenation/tex-hyphen"),
    "ukrainian": dict(
        file="hyph-uk.tex", parser="hyph-utf8", left=2, right=2,
        origin="hyph-utf8 hyph-uk.tex (2001-05-10)",
        authors="Maksym Polyakov",
        license="MIT or LPPL",
        url="https://github.com/hyphenation/tex-hyphen"),
    "russian": dict(
        file="hyph-ru.tex", parser="hyph-utf8", left=2, right=2,
        origin="hyph-utf8 hyph-ru.tex (2003-03-10)",
        authors="Alexander I. Lebedev",
        license="LPPL 1.2+",
        url="https://github.com/hyphenation/tex-hyphen"),
}


def parse_legacy(path):
    """Parse a classic TeX .txt pattern file (bare "% patterns" format,
    latin-1/latin-9 encoded).  Return (patterns, exceptions); patterns is
    a list of (letters, digits-str) cons pairs."""
    patterns, exceptions = [], []
    mode = None
    for line in open(path, encoding="latin-1"):
        line = line.strip()
        if not line:
            continue
        low = line.lower()
        if low.startswith("%") or low.startswith("\\"):
            if "pattern" in low and "nopattern" not in low: mode = "p"
            elif "exception" in low: mode = "e"
            elif "\\endinput" in low: mode = None
            continue
        if low.startswith("patterns"):
            mode = "p"; continue
        if low.startswith("exceptions"):
            mode = "e"; continue
        if low.startswith("\\end"):
            mode = None; continue
        if mode == "p":
            pat = _compile_pattern(line)
            if pat:
                patterns.append(pat)
        elif mode == "e":
            word = line.replace("-", "")
            if word:
                exceptions.append(line)
    return patterns, exceptions


def parse_hyph_utf8(path):
    """Parse a hyph-utf8 style .tex file (UTF-8, \\patterns{ ... } block,
    % metadata header).  Return (patterns, exceptions)."""
    txt = open(path, encoding="utf-8").read()
    # strip comments (incl. inline comments inside the block):
    # a TeX pattern never contains a literal %
    txt = re.sub(r"(?m)%.*$", "", txt)
    patterns, exceptions = [], []
    m = re.search(r"\\patterns\{(.*?)\n\}", txt, re.S)
    if not m:
        raise SystemExit(f"no \\patterns block in {path}")
    for tok in m.group(1).split():
        pat = _compile_pattern(tok)
        if pat:
            patterns.append(pat)
    m = re.search(r"\\hyphenation\{(.*?)\}", txt, re.S)
    if m:
        for tok in m.group(1).split():
            if tok:
                exceptions.append(tok)
    return patterns, exceptions


def _compile_pattern(token):
    """Turn "ach4" into ("ach", "....4"): the letter sequence plus one
    value slot per inter-letter gap (len(letters)+1 slots), '.' where
    the pattern carries no value."""
    letters, gap_vals, lcount = [], {}, 0
    for ch in token:
        if ch.isdigit():
            # digit applies to the gap AFTER the letters seen so far
            gap_vals[lcount] = ch
        else:
            letters.append(ch)
            lcount += 1
    if not letters or any(g > len(letters) for g in gap_vals):
        print(f"  !! skipped pattern {token!r}")
        return None
    d = ["."] * (len(letters) + 1)
    for gap, ch in gap_vals.items():
        d[gap] = ch
    return ("".join(letters), "".join(d))


def parse_language(lang):
    meta = LANGS[lang]
    path = os.path.join(SRC, meta["file"])
    if meta["parser"] == "hyph-utf8":
        return parse_hyph_utf8(path)
    return parse_legacy(path)


def elip(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'


def build_header():
    return [
        ";;; niori-pdf-hyphen-data.el --- TeX hyphenation patterns for niori-pdf -*- lexical-binding: t; -*-",
        "",
        ";; Data-only file generated by tools/gen_hyphen.py.  Do not edit by hand;",
        ";; regenerate with:  python3 tools/gen_hyphen.py",
        ";;",
        ";; TeX hyphenation patterns (Liang's algorithm).  Sources live in",
        ";; tools/hyphen-sources/ with full provenance in",
        ";; tools/hyphen-sources/README.md.  Per-language credits:",
        ";;",
        ";;   american   Frank M. Liang's classic TeX set (via cl-typesetting)",
        ";;   german     classic 'dehyphn' set, 1996 orthography (via cl-typesetting)",
        ";;   french     classic French set, Seroul/Gaulle (via cl-typesetting)",
        ";;   british    classic British set, Abbott/Wujastyk (via cl-typesetting)",
        ";;   spanish    hyph-es 5.0 -- Javier Bezos, CervanTeX (MIT/X11)",
        ";;   italian    hyph-it 4.9 -- Claudio Beccari (LPPL 1.3+ / MIT)",
        ";;   portuguese hyph-pt 1.4 -- Rezende, Almeida, Araujo, Benevides (BSD-3)",
        ";;   esperanto  hyph-eo 1.0 -- Sergio Pokrovskij (LPPL 1+)",
        ";;   ukrainian  hyph-uk -- Maksym Polyakov (MIT / LPPL)",
        ";;   russian    hyph-ru -- Alexander I. Lebedev (LPPL 1.2+)",
        ";;",
        ";; The hyph-* sources are from the TeX Live hyph-utf8 project:",
        ";; https://github.com/hyphenation/tex-hyphen -- each upstream file",
        ";; in tools/hyphen-sources/ carries its license header verbatim.",
        ";;",
        ";; Each language constant is (LEFTMIN RIGHTMIN PATTERNS EXCEPTIONS):",
        ";;   PATTERNS   list of (LETTERS . DIGITS); LETTERS is the pattern's",
        ";;              letter sequence, DIGITS is a same-length+1 string of",
        ";;              break-point values, '.' meaning no value.  A DIGITS",
        ";;              position i applies to the gap before LETTERS index i",
        ";;              when the pattern matches a word at that offset.",
        ";;   EXCEPTIONS list of hyphenated words (\"as-so-ciate\") that",
        ";;              override the patterns.",
        "",
        ";; Copyright (C) 2026 niori-pdf authors",
        "",
        ";;; Code:",
        "",
    ]


def build_body(only=None):
    lines = []
    for lang, meta in sorted(LANGS.items()):
        if only and lang != only:
            continue
        pats, excs = parse_language(lang)
        print(f"{lang}: {len(pats)} patterns, {len(excs)} exceptions")
        lines.append(f"(defconst niori-pdf-hyphen-data--{lang}")
        lines.append(f"  '({meta['left']} {meta['right']}")
        lines.append("    (")
        for letters, digits in pats:
            lines.append(f"    ({elip(letters)} . {elip(digits)})")
        lines.append("    )")
        lines.append("    (")
        for e in excs:
            lines.append(f"    {elip(e)}")
        lines.append("    ))")
        lines.append(f"  \"Hyphenation data: {lang} (TeX patterns).\")")
        lines.append("")
    if not only:
        lines.append("(defconst niori-pdf-hyphen-data--languages")
        lines.append("  '(american german french british spanish italian portuguese")
        lines.append("    esperanto ukrainian russian)")
        lines.append("  \"Languages shipped in this file.\")")
        lines.append("")
        lines.append("(provide 'niori-pdf-hyphen-data)")
        lines.append(";;; niori-pdf-hyphen-data.el ends here")
    return lines


def main():
    ap = argparse.ArgumentParser(
        description="Regenerate niori-pdf-hyphen-data.el from TeX pattern sources")
    ap.add_argument("--check", action="store_true",
                    help="verify the shipped data file matches; exit 1 if not")
    ap.add_argument("--lang", metavar="LANG", choices=sorted(LANGS),
                    help="print only this language's defconst to stdout")
    args = ap.parse_args()

    if args.lang:
        sys.stdout.write("\n".join(build_body(only=args.lang)) + "\n")
        return

    text = "\n".join(build_header() + build_body()) + "\n"

    if args.check:
        try:
            with open(OUT, encoding="utf-8") as fh:
                current = fh.read()
        except FileNotFoundError:
            print(f"FAIL: {OUT} does not exist")
            sys.exit(1)
        if current == text:
            print(f"OK: {OUT} is up to date "
                  f"({os.path.getsize(OUT)} bytes, byte-identical)")
        else:
            cur, new = current.splitlines(), text.splitlines()
            diffs = sum(1 for a, b in zip(cur, new) if a != b) + abs(len(cur) - len(new))
            print(f"FAIL: {OUT} differs from regenerated output "
                  f"({diffs} differing lines)")
            shown = 0
            for i, (a, b) in enumerate(zip(cur, new), 1):
                if a != b and shown < 5:
                    print(f"  line {i}:\n    shipped: {a}\n    built:   {b}")
                    shown += 1
            sys.exit(1)
        return

    open(OUT, "w", encoding="utf-8").write(text)
    print(f"wrote {OUT} ({os.path.getsize(OUT)} bytes)")


if __name__ == "__main__":
    main()
