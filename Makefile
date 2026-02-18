EMACS ?= emacs
MAKEINFO ?= makeinfo
INSTALL_INFO ?= install-info

LOAD = -L .
TEST_FILES = \
  test/bilk-mode-tests.el \
  test/bilk-protocol-tests.el \
  test/bilk-repl-tests.el \
  test/bilk-lsp-tests.el \
  test/bilk-debug-tests.el \
  test/bilk-project-tests.el

LOAD_TESTS = $(foreach f,$(TEST_FILES),-l $(f))

.PHONY: test info clean

test: ## Run all tests
	$(EMACS) -Q -batch $(LOAD) -l ert $(LOAD_TESTS) -f ert-run-tests-batch-and-exit

info: bilk-mode.info dir ## Build info manual

bilk-mode.info: bilk-mode.texi
	$(MAKEINFO) --no-split $< -o $@

dir: bilk-mode.info
	$(INSTALL_INFO) --dir-file=dir bilk-mode.info

clean: ## Remove build artifacts
	rm -f *.elc bilk-mode.info dir
