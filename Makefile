EMACS ?= emacs

# vui.el and vuistack-table are fetched into DEPS by `make deps' (not
# vendored).  Point VUI or VUISTACK_TABLE at a checkout to use your own.
DEPS ?= .deps
VUI_URL ?= https://github.com/d12frosted/vui.el
VUISTACK_URL ?= git@gitlab.com:davidawad/dotfiles.git
VUISTACK_PATH ?= config/terminal/emacs/packages/vuistack-table
VUI ?= $(DEPS)/vui.el
VUISTACK_TABLE ?= $(DEPS)/vuistack/$(VUISTACK_PATH)
LOAD_PATH = -L . -L $(VUI) -L $(VUISTACK_TABLE)

.PHONY: test compile deps clean

deps: $(VUI)/vui.el $(VUISTACK_TABLE)/vuistack-table.el

$(VUI)/vui.el:
	git clone --depth 1 $(VUI_URL) $(VUI)

$(VUISTACK_TABLE)/vuistack-table.el:
	git clone --depth 1 --filter=blob:none --sparse $(VUISTACK_URL) $(DEPS)/vuistack
	git -C $(DEPS)/vuistack sparse-checkout set $(VUISTACK_PATH)

test: deps
	$(EMACS) -Q --batch $(LOAD_PATH) -L test -l test/test-herdr-core-features.el -f ert-run-tests-batch-and-exit

compile: deps
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t byte-compile-docstring-max-column 100)' -f batch-byte-compile herdr-core.el

clean:
	rm -f *.elc test/*.elc
