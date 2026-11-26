# tools/ -- development pipeline for niori-pdf

Currently one tool:

## gen_hyphen.py -- hyphenation pattern pipeline

`niori-pdf-hyphen-data.el` (the 950 KB pattern database used by
`niori-pdf-hyphen.el`) is **generated code**.  It is produced from the
upstream TeX pattern files in `tools/hyphen-sources/` by this script.
Nothing in the repository is a hand-edited binary blob: every break
point the hyphenator produces can be traced back to a plain-text
pattern file kept verbatim in the repo.

### How the conversion works

A TeX pattern like `ach4` means: the letter sequence `a c h` with the
value 4 attached to the *gap after* `h`.  Liang's algorithm overlays
thousands of these patterns on a word; where the sum of overlaid odd
values lands, the word may be hyphenated.

The conversion is a pure textual re-encoding, one pattern at a time:

```
TeX input token:  a c h 4            (letters interleaved with digits)
                  | | | |
letter sequence:  "ach"              (digits stripped)
gap values:        . . .  4           one slot per gap, len+1 slots
output cons:      ("ach" . "....4")  '.' marks a gap with no value
```

* The leading `.` of boundary patterns (`.ach4`) is kept as part of the
  letter sequence -- at runtime it matches the word-boundary sentinel
  that `niori-pdf-hyphen--breaks` adds around every word.
* `\hyphenation{...}` exception words (`as-so-ciate`) are stored as-is;
  the Elisp side converts the hyphens into break indexes when the
  language is loaded.
* Two input formats are recognized by the `parser` field in `LANGS`:
  - `legacy` -- classic bare `.txt` files (`% patterns` sections,
    latin-1/latin-9 encoded): ushyph, dehyphn, frhyph, gbhyph.
  - `hyph-utf8` -- modern `.tex` files with a `\patterns{...}` block
    and YAML-ish `%` metadata headers (UTF-8).
* Output is one `defconst` per language, shaped
  `(LEFTMIN RIGHTMIN PATTERNS EXCEPTIONS)`, plus a `--languages` index
  consumed by `niori-pdf-hyphen-load-language`.

### Reproducing the shipped data

From the repository root:

```sh
python3 tools/gen_hyphen.py --check   # verify shipped file is current
```

prints `OK: niori-pdf-hyphen-data.el is up to date (..., byte-identical)`
when the shipped file matches what the sources regenerate, and exits 1
with a diff summary otherwise.  To rebuild after editing a source file
or the generator:

```sh
python3 tools/gen_hyphen.py           # rewrite niori-pdf-hyphen-data.el
make compile test                     # 66 ERT tests must stay green
```

There is no hidden state: the only inputs are the ten files in
`tools/hyphen-sources/` (Python >= 3.6, stdlib only).

### Extending -- adding a language

Example: add Catalan from a hyph-utf8 download.  Four steps:

1. **Fetch the upstream file** and copy it verbatim into
   `tools/hyphen-sources/`:

   ```sh
   curl -LO https://raw.githubusercontent.com/hyphenation/tex-hyphen/master/hyph-utf8/tex/generic/hyph-utf8/patterns/tex/hyph-ca.tex
   mv hyph-ca.tex tools/hyphen-sources/
   ```

   The file's own `%` header will tell you the author, license and the
   `\lefthyphenmin`/`\righthyphenmin` values (`hyphenmins:` block).

2. **Register it** in `LANGS` inside `tools/gen_hyphen.py`:

   ```python
   "catalan": dict(
       file="hyph-ca.tex", parser="hyph-utf8", left=2, right=2,
       origin="hyph-utf8 hyph-ca.tex v<version> (<date>)",
       authors="<author>",
       license="<license>",
       url="https://github.com/hyphenation/tex-hyphen"),
   ```

   Use `parser="legacy"` only for classic bare `.txt` files; for
   anything from hyph-utf8 (or any file with a `\patterns{...}` block)
   use `parser="hyph-utf8"`.

3. **Wire it into the runtime** -- two small edits in Elisp:

   - `niori-pdf-hyphen.el`, `niori-pdf-hyphen-load-language`: add a
     `(`catalan niori-pdf-hyphen-data--catalan)` branch to the
     `pcase`.
   - the generator emits `niori-pdf-hyphen-data--languages` from a
     fixed list; add the new symbol to that list in
     `tools/gen_hyphen.py` (`build_body`) so
     `niori-pdf-hyphen-language-available-p` recognizes it.

   (A refactor hooking this dispatch off the data list itself is
   welcome; today the list is intentionally explicit.)

4. **Regenerate, credit, test, commit**:

   ```sh
   python3 tools/gen_hyphen.py          # now emits the catalan defconst
   # also add the catalan credit line to build_header() in the generator
   python3 tools/gen_hyphen.py          # re-run so the header credits it
   ```

   Then add an ERT test next to `niori-pdf-hyphen-spanish` in
   `test/niori-pdf-tests.el` with two or three canonical break points
   for the language, and record the new upstream file + license in
   `tools/hyphen-sources/README.md` (per-language table).  Run
   `make compile test`; everything green, commit the pattern file, the
   regenerated data, the README rows and the test in one commit.

### Pattern-level debugging

`--lang LANG` prints just that language's defconst to stdout, which is
handy when you need to check whether a specific pattern survived the
conversion:

```sh
python3 tools/gen_hyphen.py --lang french | rg '"fran'
```

And in Elisp, `(niori-pdf-hyphenate-word "word" 'lang)` returns break
indexes *before* the 0-based character N; the ERT suite in
`test/niori-pdf-tests.el` shows the canonical expected values per
language.
