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
import re, os, sys, argparse, hashlib

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(REPO, "tools", "hyphen-sources")
OUT = os.path.join(REPO, "niori-pdf-hyphen-data.el")
LOCK = os.path.join(SRC, "SOURCES.lock")

UPSTREAM_REPOSITORY = "https://github.com/hyphenation/tex-hyphen.git"
PINNED_REVISION = "5684c0f51c0b81133db2efbe60a408b4155a3ff5"

PINNED_SOURCES = {
    "american": "hyph-en-us.tex",
    "german": "hyph-de-1996.tex",
    "french": "hyph-fr.tex",
    "british": "hyph-en-gb.tex",
}

# language -> metadata.  file is relative to tools/hyphen-sources/.
# left/right are the TeX \lefthyphenmin/\righthyphenmin (shipped data
# uses cl-typesetting's values for the four legacy sets and the
# hyph-utf8 \hyphenmins for the others).
LANGS = {
    "american": dict(
        file="hyph-en-us.tex", parser="hyph-utf8",
        left=2, right=3,
        origin="hyph-utf8 hyph-en-us.tex (upstream revision 5684c0f51c0b81133db2efbe60a408b4155a3ff5)",
        authors='Gerard D.C. Kuiken',
        license='Royalty-free; preserve copyright and permission notice',
        url="https://github.com/hyphenation/tex-hyphen"),
    "german": dict(
        file="hyph-de-1996.tex", parser="hyph-utf8",
        left=2, right=2,
        origin="hyph-utf8 hyph-de-1996.tex (upstream revision 5684c0f51c0b81133db2efbe60a408b4155a3ff5)",
        authors='Stephan Hennig, Werner Lemberg, Günter Milde, Sander van Geloven, Georg Pfeiffer, Gisbert W. Selke, Tobias Wendorf, Keno Wehr',
        license='MIT',
        url="https://github.com/hyphenation/tex-hyphen"),
    "french": dict(
        file="hyph-fr.tex", parser="hyph-utf8",
        left=2, right=2,
        origin="hyph-utf8 hyph-fr.tex (upstream revision 5684c0f51c0b81133db2efbe60a408b4155a3ff5)",
        authors='Daniel Flipo, Bernard Gaulle, Arthur Reutenauer',
        license='MIT',
        url="https://github.com/hyphenation/tex-hyphen"),
    "british": dict(
        file="hyph-en-gb.tex", parser="hyph-utf8",
        left=2, right=3,
        origin="hyph-utf8 hyph-en-gb.tex (upstream revision 5684c0f51c0b81133db2efbe60a408b4155a3ff5)",
        authors='Dominik Wujastyk, Graham Toal',
        license='MIT',
        url="https://github.com/hyphenation/tex-hyphen"),
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


def verify_source_lock():
    """Verify the four migrated sources against SOURCES.lock."""
    try:
        with open(LOCK, encoding="utf-8") as fh:
            lines = fh.readlines()
    except OSError as exc:
        raise SystemExit(f"Cannot read source lock {LOCK}: {exc}")

    metadata = {}
    checksums = {}

    for lineno, raw_line in enumerate(lines, 1):
        line = raw_line.strip()

        if not line or line.startswith("#"):
            continue

        fields = line.split()

        if len(fields) == 2 and fields[0] in ("repository", "revision"):
            key, value = fields
            if key in metadata:
                raise SystemExit(
                    f"{LOCK}:{lineno}: duplicate {key} entry"
                )
            metadata[key] = value

        elif (
            len(fields) == 2
            and re.fullmatch(r"[0-9a-f]{64}", fields[0])
        ):
            digest, filename = fields
            if filename in checksums:
                raise SystemExit(
                    f"{LOCK}:{lineno}: duplicate checksum for {filename}"
                )
            checksums[filename] = digest

        else:
            raise SystemExit(
                f"{LOCK}:{lineno}: unrecognized lock-file entry"
            )

    if metadata.get("repository") != UPSTREAM_REPOSITORY:
        raise SystemExit(
            f"{LOCK}: unexpected upstream repository: "
            f"{metadata.get('repository')!r}"
        )

    if metadata.get("revision") != PINNED_REVISION:
        raise SystemExit(
            f"{LOCK}: expected revision {PINNED_REVISION}, "
            f"found {metadata.get('revision')!r}"
        )

    expected_files = set(PINNED_SOURCES.values())

    if set(checksums) != expected_files:
        missing = expected_files - set(checksums)
        unexpected = set(checksums) - expected_files
        raise SystemExit(
            f"{LOCK}: source list mismatch; "
            f"missing={sorted(missing)}, unexpected={sorted(unexpected)}"
        )

    for language, filename in PINNED_SOURCES.items():
        meta = LANGS[language]

        if meta["file"] != filename:
            raise SystemExit(
                f"LANGS[{language!r}] references {meta['file']!r}; "
                f"expected {filename!r}"
            )

        if PINNED_REVISION not in meta.get("origin", ""):
            raise SystemExit(
                f"LANGS[{language!r}] does not identify "
                f"the pinned upstream revision"
            )

        source_path = os.path.join(SRC, filename)

        try:
            with open(source_path, "rb") as fh:
                actual = hashlib.sha256(fh.read()).hexdigest()
        except OSError as exc:
            raise SystemExit(f"Cannot read source {filename}: {exc}")

        expected = checksums[filename]

        if actual != expected:
            raise SystemExit(
                f"SHA-256 mismatch for {filename}\n"
                f"  expected: {expected}\n"
                f"  actual:   {actual}\n"
                "The source file may have changed. Check SOURCES.lock "
                "and the pinned upstream revision."
            )


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
    lines = [
        ";;; niori-pdf-hyphen-data.el --- TeX hyphenation patterns for niori-pdf -*- lexical-binding: t; -*-",
        "",
        ";; Data-only file generated by tools/gen_hyphen.py.  Do not edit by hand;",
        ";; regenerate with:  python3 tools/gen_hyphen.py",
        ";;",
        ";; TeX hyphenation patterns (Liang's algorithm). Sources and their",
        ";; upstream headers are kept in tools/hyphen-sources/; see its README",
        ";; and THIRD-PARTY-NOTICES for provenance and license information.",
        ";; Per-language credits (license details are in the source headers):",
        ";;",
    ]
    for lang, meta in LANGS.items():
        authors = meta.get("authors", "see upstream source header")
        license_name = meta.get("license", "see upstream source header")
        lines.append(f";;   {lang:<11} {authors} ({license_name})")
    lines += [
        ";;",
        ";; Individual pattern copyrights and licenses remain with their",
        ";; respective authors and copyright holders. The niori-pdf project",
        ";; does not claim exclusive copyright in this generated collection.",
        ";;",
        ";; Each language constant is (LEFTMIN RIGHTMIN PATTERNS EXCEPTIONS):",
        ";;   PATTERNS   list of (LETTERS . DIGITS); DIGITS has one slot per gap.",
        ";;   EXCEPTIONS list of hyphenated words that override the patterns.",
        "",
        ";;; Code:",
        "",
    ]
    return lines

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

    verify_source_lock()

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
