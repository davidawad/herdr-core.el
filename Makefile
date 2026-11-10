EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -L test -l test/test-herdr-core-features.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t byte-compile-docstring-max-column 100)' -f batch-byte-compile herdr-core.el

clean:
	rm -f *.elc test/*.elc
