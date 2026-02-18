# bilk-mode

An Emacs major mode for [Bilk Scheme](https://bilk-scheme.github.io), providing
syntax highlighting, REPL integration, LSP support, debugging, and project
management.

## Features

- Derives from `scheme-mode` for R7RS syntax, indentation, and font-lock
- Three levels of font-lock: keywords, booleans/characters, R7RS builtins
- Datum comment (`#;`) highlighting
- Imenu support for `define`, `define-syntax`, `define-library`,
  `define-record-type`, and `define-values`
- Interactive REPL via `bilk serve` with eval, completion, and auto-reload
- LSP integration via eglot (built-in since Emacs 29)
- DAP debugging via dape
- `project.el` backend recognizing `package.scm` project roots
- Library navigation with search path resolution

## Requirements

- Emacs 29 or later
- `bilk` compiler on `$PATH` (or set `bilk-program`)
- **Optional:** [dape](https://github.com/svaante/dape) for DAP debugging

Eglot ships with Emacs 29+ and is used for LSP — no extra package needed.

## Installation

### Manual

Clone this repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/bilk-mode")
(require 'bilk-mode)
(require 'bilk-repl)
(require 'bilk-lsp)
(require 'bilk-project)
;; Optional — only if dape is installed
(require 'bilk-debug)
```

### use-package

```elisp
(use-package bilk-mode
  :load-path "/path/to/bilk-mode"
  :mode ("\\.scm\\'" "\\.sld\\'")
  :config
  (require 'bilk-repl)
  (require 'bilk-lsp)
  (require 'bilk-project)
  ;; Optional — only if dape is installed
  (with-eval-after-load 'dape
    (require 'bilk-debug)))
```

## Keybindings

| Key           | Command              | Description                        |
|---------------|----------------------|------------------------------------|
| `C-x C-e`    | `bilk-eval-last-sexp`| Evaluate sexp before point         |
| `C-M-x`      | `bilk-eval-defun`    | Evaluate top-level form at point   |
| `C-c C-r`    | `bilk-eval-region`   | Evaluate active region             |
| `C-c C-b`    | `bilk-eval-buffer`   | Evaluate entire buffer             |
| `C-c C-l`    | `bilk-load-file`     | Load a file into the REPL          |
| `C-c C-z`    | `bilk-switch-to-repl`| Switch to the REPL buffer          |
| `C-c C-c`    | `bilk-interrupt`     | Interrupt REPL evaluation          |
| `C-c C-f`    | `bilk-find-library`  | Find and visit a library by name   |
| `C-c C-p b`  | `bilk-build`         | Build the current project          |
| `C-c C-p t`  | `bilk-test`          | Run tests for the current project  |
| `C-c C-p p`  | `bilk-profile`       | Profile a file                     |

## REPL Integration

Start the REPL with `M-x bilk-repl-start`. This launches `bilk serve` and
connects over TCP (default port 7890).

### Eval commands

Use `C-x C-e`, `C-M-x`, `C-c C-r`, or `C-c C-b` to send code from any
`bilk-mode` buffer to the running REPL. Results appear in the `*bilk-repl*`
buffer and in the echo area.

### Comma-commands

The REPL supports comma-commands, exposed as interactive functions:

| Function          | Comma-command        | Description                       |
|-------------------|----------------------|-----------------------------------|
| `bilk-checkpoint` | `,checkpoint NAME`   | Save a named REPL state snapshot  |
| `bilk-revert`     | `,revert NAME`       | Restore a named snapshot          |
| `bilk-reload`     | `,reload (lib name)` | Reload an .sld library            |
| `bilk-exports`    | `,exports (lib name)`| List library exports              |
| `bilk-deps`       | `,deps (lib name)`   | List library dependencies         |

### Auto-reload

When `bilk-auto-reload` is non-nil (the default), saving an `.sld` file
automatically reloads that library in the connected REPL.

### Completion

With an active REPL connection, `completion-at-point` (`C-M-i` or `TAB` in many
configurations) queries the REPL for symbol completions.

### Error navigation

The REPL buffer enables `compilation-minor-mode` with a regexp matching Bilk
error locations (`file:line:col`), so `M-g M-n` / `M-g M-p` jump to error
sites.

## LSP Integration

When `bilk-lsp-enabled` is non-nil (the default), opening a `.scm` or `.sld`
file automatically starts eglot with `bilk lsp` as the language server. This
provides diagnostics, completion, hover, and go-to-definition.

Disable auto-start:

```elisp
(setq bilk-lsp-enabled nil)
```

## DAP Integration

Requires the [dape](https://github.com/svaante/dape) package.

A `bilk-debug` configuration is registered with dape automatically when `dape`
loads and `bilk-debug` has been required.

| Command                | Description                          |
|------------------------|--------------------------------------|
| `bilk-debug-file`      | Start debugging the current file    |
| `bilk-toggle-breakpoint`| Toggle a breakpoint at point       |

## Project Integration

bilk-mode registers a `project.el` backend that recognizes any directory
containing a `package.scm` file as a project root. This enables standard
`project-find-file`, `project-switch-project`, etc.

Project commands (under the `C-c C-p` prefix):

| Key         | Command        | Runs            |
|-------------|----------------|-----------------|
| `C-c C-p b` | `bilk-build`  | `bilk build`    |
| `C-c C-p t` | `bilk-test`   | `bilk test`     |
| `C-c C-p p` | `bilk-profile`| `bilk profile FILE` |

Build output goes to a `*compilation*` buffer with error navigation support.

## Library Navigation

`bilk-find-library` (`C-c C-f`) resolves a library name like `(scheme base)` to
its `.sld` file and visits it. The search path resolution order:

1. `$BILK_PATH` environment variable (colon-separated directories)
2. Project root (nearest ancestor containing `package.scm`)
3. `~/.bilk/lib/` if it exists
4. Entries in `bilk-library-search-paths`

## Customization

All options are in the `bilk` customization group (`M-x customize-group bilk`).

| Variable                    | Type           | Default  | Description                                 |
|-----------------------------|----------------|----------|---------------------------------------------|
| `bilk-program`              | string         | `"bilk"` | Path to the bilk executable                 |
| `bilk-repl-port`            | integer        | `7890`   | TCP port for the REPL server                |
| `bilk-lsp-enabled`          | boolean        | `t`      | Auto-start eglot in bilk-mode buffers       |
| `bilk-auto-reload`          | boolean        | `t`      | Auto-reload .sld libraries on save          |
| `bilk-library-search-paths` | list of dirs   | `nil`    | Additional directories for library lookup   |

## Module Architecture

```
bilk-custom          (leaf: defgroup + defcustom, no deps)
    |
bilk-mode            (major mode, derives from scheme-mode)
    |
    +-- bilk-repl     (REPL client: eval, completion, auto-reload)
    |     |
    |     +-- bilk-protocol  (binary frame codec, pure functions)
    |
    +-- bilk-lsp      (eglot registration + auto-start hook)
    |
    +-- bilk-debug    (dape configuration + debug commands)
    |
    +-- bilk-project  (project.el backend + build/test/profile)
```

Each module has a single responsibility and depends only on `bilk-custom` and
`bilk-mode`. `bilk-protocol` is a pure-data module with no side effects.

## Running Tests

The test suite uses ERT (Emacs Lisp Regression Testing):

```sh
make test
```

Or directly:

```sh
emacs -Q -batch -L . -l ert \
  -l test/bilk-mode-tests.el \
  -l test/bilk-protocol-tests.el \
  -l test/bilk-repl-tests.el \
  -l test/bilk-lsp-tests.el \
  -l test/bilk-debug-tests.el \
  -l test/bilk-project-tests.el \
  -f ert-run-tests-batch-and-exit
```
