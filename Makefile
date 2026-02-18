EMACS ?= emacs

LOAD = -L .
TEST_FILES = \
  test/bilk-mode-tests.el \
  test/bilk-protocol-tests.el \
  test/bilk-repl-tests.el \
  test/bilk-lsp-tests.el \
  test/bilk-debug-tests.el \
  test/bilk-project-tests.el

LOAD_TESTS = $(foreach f,$(TEST_FILES),-l $(f))

.PHONY: test
test: ## Run all tests
	$(EMACS) -Q -batch $(LOAD) -l ert $(LOAD_TESTS) -f ert-run-tests-batch-and-exit
