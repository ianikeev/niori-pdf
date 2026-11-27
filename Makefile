EMACS   ?= emacs
MAKEINFO ?= makeinfo
ELS     := $(wildcard *.el)
ELC     := $(ELS:.el=.elc)
VERSION := 0.1.0
DIST    := niori-pdf-$(VERSION)

.PHONY: all compile info test check clean dist examples

all: compile info

compile: $(ELC)

%.elc: %.el
	$(EMACS) --batch -L . -L examples -f batch-byte-compile $<

# ----- example PDFs (generated into examples/, written via /tmp first)
# Every document has a hyphenated default and a --no-hyphenation
# comparison build (same script, opposite flag); sample-report and the
# alice book additionally have a --pdfa PDF/A-2b archival variant (see
# the .el headers).

EXAMPLES := examples/sample-report.pdf examples/sample-report-nohyphen.pdf \
	examples/sample-report-pdfa.pdf examples/sample-report-nohyphen-pdfa.pdf \
	examples/niori-pdf-evaluation.pdf examples/niori-pdf-evaluation-nohyphen.pdf \
	examples/alice-in-wonderland.pdf examples/alice-in-wonderland-pdfa.pdf

examples: $(EXAMPLES) | /tmp/niori-test

# the example scripts buffer their PDFs here before the rules copy them
/tmp/niori-test:
	mkdir -p $@

examples/sample-report.pdf: examples/sample-report.el examples/udhr-texts.el $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $<
	cp /tmp/niori-test/$(notdir $@) $@

examples/sample-report-nohyphen.pdf: examples/sample-report.el examples/udhr-texts.el $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $< -- --no-hyphenation
	cp /tmp/niori-test/$(notdir $@) $@

examples/sample-report-pdfa.pdf: examples/sample-report.el examples/udhr-texts.el $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $< -- --pdfa
	cp /tmp/niori-test/$(notdir $@) $@

examples/sample-report-nohyphen-pdfa.pdf: examples/sample-report.el examples/udhr-texts.el $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $< -- --pdfa --no-hyphenation
	cp /tmp/niori-test/$(notdir $@) $@

examples/niori-pdf-evaluation.pdf: examples/cl-pdf-evaluation.el $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $<
	cp /tmp/niori-test/$(notdir $@) $@

examples/niori-pdf-evaluation-nohyphen.pdf: examples/cl-pdf-evaluation.el $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $< -- --no-hyphenation
	cp /tmp/niori-test/$(notdir $@) $@

examples/alice-in-wonderland.pdf: examples/alice-book.el examples/alice.txt $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $<
	cp /tmp/niori-test/$(notdir $@) $@

examples/alice-in-wonderland-pdfa.pdf: examples/alice-book.el examples/alice.txt $(ELS) $(ELC) | /tmp/niori-test
	$(EMACS) --batch -L . -L examples -l $< -- --pdfa
	cp /tmp/niori-test/$(notdir $@) $@

info: niori-pdf.info

niori-pdf.info: docs/niori-pdf.texi
	if command -v makeinfo >/dev/null 2>&1; then \
	  $(MAKEINFO) -o $@ $<; \
	else \
	  $(EMACS) --batch -l docs/build-info.el $< $@; \
	fi

pdf: docs/niori-pdf.texi
	$(MAKEINFO) --pdf -o docs/niori-pdf.pdf $<

test:
	$(EMACS) --batch -L . -l test/niori-pdf-tests.el \
	-f ert-run-tests-batch-and-exit

# Hyphenation pattern pipeline (tools/gen_hyphen.py): verify that the
# shipped niori-pdf-hyphen-data.el matches the upstream sources under
# tools/hyphen-sources/.  Run `make patterns-regen` after editing a
# source or the generator, then `make check`.
patterns:
	python3 tools/gen_hyphen.py --check

patterns-regen:
	python3 tools/gen_hyphen.py

check: compile test patterns

clean:
	rm -f $(ELC) niori-pdf.info docs/niori-pdf.pdf
	rm -rf $(DIST) $(DIST).tar.gz

# Release tarball: compiled info included, .elc excluded (MELPA rebuilds).
dist: clean all
	mkdir -p $(DIST)
	cp -r $(ELS) docs examples test tools LICENSE README.md Makefile niori-pdf.info $(DIST)/
	tar czf $(DIST).tar.gz $(DIST)
	rm -rf $(DIST)
	@echo "$(DIST).tar.gz ready"
