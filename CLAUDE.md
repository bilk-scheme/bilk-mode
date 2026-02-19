# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Bilk
The `bilk` executable is available from the the following path:
`/home/sbj/Sandbox/bilk-scheme/bilk/_opam/bin/bilk`

Information about `bilk` is available from it's website https://bilk-scheme.github.io/

## Project

bilk-mode is an Emacs major mode for the Bilk Scheme language. It provides syntax highlighting (3 font-lock levels), REPL integration via a binary TCP protocol, LSP support (eglot), DAP debugging (dape), project management (project.el), and library navigation. Requires Emacs 29.1+.



## Commands

```bash
# Run all tests
make test

# Run a single test file
emacs -Q -batch -L . -l ert -l test/bilk-protocol-tests.el -f ert-run-tests-batch-and-exit

# Run a single named test
emacs -Q -batch -L . -l ert -l test/bilk-mode-tests.el --eval '(ert-run-tests-batch-and-exit "bilk-mode-font-lock-keywords")'

# Build info manual
make info

# Byte-compile a file
emacs -Q -batch -L . -f batch-byte-compile bilk-mode.el
```

## Architecture

### Module Dependency Graph

```
bilk-custom.el          ← Leaf: customization variables only
    ↓
bilk-mode.el            ← Core mode (derives from scheme-mode)
    ├→ bilk-repl.el     ← REPL client, process mgmt, eval commands
    │   └→ bilk-protocol.el  ← Binary frame codec (pure functions, no I/O)
    ├→ bilk-lsp.el      ← Thin eglot registration wrapper
    ├→ bilk-debug.el    ← Dape configuration adapter
    └→ bilk-project.el  ← project.el backend, build/test/profile commands
```

Every module depends only on `bilk-custom` and `bilk-mode`. Optional dependencies (dape, eglot) are handled via `with-eval-after-load` and `fboundp` guards.

### Key Design Decisions

- **bilk-protocol.el is purely functional** — encode/decode functions with no side effects or global state. Frame format: `[u32 length][u8 tag][payload]` big-endian. Test it with round-trip property-style tests.
- **bilk-repl.el manages connection state** — connection states are `disconnected`, `ready`, `busy`. REPL interaction states are `idle`, `eval-wait`, `read-wait`. The process filter accumulates partial TCP frames before dispatching complete messages. The REPL buffer has an interactive prompt (`bilk> `) with marker-based prompt/input tracking and per-text-property read-only protection.
- **Font-lock levels** — Level 1: 39 Bilk keywords; Level 2: adds booleans + character literals; Level 3: adds 200+ R7RS builtins.
- **Datum comment highlighting** (`#;`) uses a custom matcher function that calls `forward-sexp` to determine extent.
- **Library resolution order**: `$BILK_PATH` → project root (nearest `package.scm`) → `~/.bilk/lib/` → `bilk-library-search-paths`.

### Testing

Tests use ERT. All source files declare `lexical-binding: t`. Test helpers are defined at the top of `test/bilk-mode-tests.el` (`bilk-test-with-buffer`, `bilk-test-face-at`, `bilk-test-indent`, `bilk-test-imenu-find`) and `test/bilk-repl-tests.el` (`bilk-test-with-source-buffer`). Protocol tests in `test/bilk-protocol-tests.el` use round-trip encode/decode assertions. Dape tests are skipped in batch mode since dape is not installed.

### Conventions

- Private functions use `--` prefix (e.g., `bilk-repl--send-client-msg`).
- All files use `lexical-binding: t`.
- Keybindings follow Emacs Scheme-mode conventions: `C-x C-e` eval-last-sexp, `C-M-x` eval-defun, `C-c C-z` switch-to-repl.
- Error regexp for bilk compiler output: `^Error: ([^:]+):([0-9]+):([0-9]+): `.
