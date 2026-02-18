;;; bilk-mode.el --- Major mode for Bilk Scheme  -*- lexical-binding: t; -*-

;;; Commentary:

;; Derives from `scheme-mode' for R7RS syntax table, indentation, and
;; font-lock foundation.  Extends with bilk-specific keywords, builtins,
;; and imenu patterns.

;;; Code:

(require 'scheme)
(require 'bilk-custom)

;; ---------------------------------------------------------------------------
;;; Keywords
;; ---------------------------------------------------------------------------

;; 39 from tokenizer.ml + 3 from expander.ml (delay, delay-force, case-lambda)
(defconst bilk-keywords
  '("define" "define-syntax" "define-record-type" "define-library"
    "define-values"
    "lambda" "if" "cond" "case" "else" "=>" "and" "or"
    "when" "unless" "begin" "do"
    "let" "let*" "letrec" "letrec*" "let-values" "let*-values"
    "let-syntax" "letrec-syntax"
    "set!" "quote" "quasiquote" "unquote" "unquote-splicing"
    "syntax-rules" "syntax-error"
    "import" "export" "library" "include" "include-ci"
    "cond-expand" "guard"
    ;; expander.ml additions
    "delay" "delay-force" "case-lambda")
  "Bilk Scheme keywords (tokenizer.ml + expander.ml core forms).")

;; ---------------------------------------------------------------------------
;;; R7RS builtins — extracted from instance.ml register calls.
;;  Internal helpers (%-prefixed) are excluded.
;; ---------------------------------------------------------------------------

(defconst bilk-builtins
  '("+" "-" "*" "/" "=" "<" ">" "<=" ">="
    "cons" "car" "cdr" "null?" "pair?" "not"
    "display" "write" "write-shared" "write-simple" "newline"
    "eqv?" "eq?" "list"
    "apply" "call/cc" "call-with-current-continuation"
    "call-with-values" "dynamic-wind" "values"
    ;; numeric
    "abs" "min" "max" "quotient" "remainder" "modulo"
    "floor" "ceiling" "truncate" "round"
    "floor-quotient" "floor-remainder"
    "truncate-quotient" "truncate-remainder"
    "gcd" "lcm" "exact->inexact" "inexact->exact" "exact" "inexact"
    "expt" "sqrt" "exact-integer-sqrt" "number->string"
    "numerator" "denominator"
    ;; inexact math
    "exp" "log" "sin" "cos" "tan" "asin" "acos" "atan"
    "finite?" "infinite?" "nan?"
    ;; complex
    "real-part" "imag-part" "magnitude" "angle"
    "make-rectangular" "make-polar"
    ;; lazy
    "make-promise" "promise?"
    ;; process
    "exit" "emergency-exit"
    "get-environment-variable" "get-environment-variables"
    ;; time
    "current-second" "current-jiffy" "jiffies-per-second"
    "string->number"
    ;; pair & list
    "set-car!" "set-cdr!"
    "caar" "cadr" "cdar" "cddr"
    "make-list" "length" "append" "reverse"
    "list-tail" "list-ref" "list-set!" "list-copy"
    "memq" "memv" "member" "assq" "assv" "assoc"
    ;; character
    "char=?" "char<?" "char>?" "char<=?" "char>=?"
    "char-ci=?" "char-ci<?" "char-ci>?" "char-ci<=?" "char-ci>=?"
    "char->integer" "integer->char"
    "char-upcase" "char-downcase" "char-foldcase"
    "char-alphabetic?" "char-numeric?" "char-whitespace?"
    "char-upper-case?" "char-lower-case?" "digit-value"
    ;; string
    "make-string" "string" "string-length" "string-ref" "string-set!"
    "string=?" "string<?" "string>?" "string<=?" "string>=?"
    "string-ci=?" "string-ci<?" "string-ci>?" "string-ci<=?" "string-ci>=?"
    "substring" "string-append" "string->list" "list->string"
    "string-copy" "string-copy!" "string-fill!"
    "string-upcase" "string-downcase" "string-foldcase"
    ;; vector
    "make-vector" "vector" "vector-length" "vector-ref" "vector-set!"
    "vector->list" "list->vector"
    "vector-copy" "vector-copy!" "vector-append" "vector-fill!"
    "vector->string" "string->vector"
    ;; bytevector
    "make-bytevector" "bytevector" "bytevector-length"
    "bytevector-u8-ref" "bytevector-u8-set!"
    "bytevector-copy" "bytevector-copy!" "bytevector-append"
    "utf8->string" "string->utf8"
    ;; type predicates
    "equal?" "boolean?" "boolean=?"
    "number?" "complex?" "real?" "rational?" "integer?"
    "exact?" "inexact?" "exact-integer?"
    "zero?" "positive?" "negative?" "odd?" "even?"
    "symbol?" "symbol=?" "symbol->string" "string->symbol"
    "char?" "string?" "vector?" "bytevector?"
    "procedure?" "list?"
    "eof-object?" "eof-object"
    ;; exception (public)
    "error-object?" "error-object-message" "error-object-irritants"
    "read-error?" "file-error?" "error-object-type?"
    ;; port predicates
    "port?" "input-port?" "output-port?"
    "input-port-open?" "output-port-open?"
    "textual-port?" "binary-port?"
    ;; current ports
    "current-input-port" "current-output-port" "current-error-port"
    ;; string ports
    "open-input-string" "open-output-string" "get-output-string"
    ;; write primitives
    "write-char" "write-string" "write-u8" "write-bytevector"
    "flush-output-port"
    ;; read primitives
    "read-char" "peek-char" "read-line" "read-string"
    "read-u8" "peek-u8" "read-bytevector" "char-ready?"
    ;; file I/O
    "open-input-file" "open-output-file"
    "close-input-port" "close-output-port" "close-port"
    ;; file system
    "file-exists?" "delete-file")
  "R7RS standard library procedures registered by Bilk (instance.ml).")

;; ---------------------------------------------------------------------------
;;; Font-lock
;; ---------------------------------------------------------------------------

(defconst bilk-font-lock-keywords-1
  `((,(concat "(" (regexp-opt bilk-keywords t) "\\_>")
     1 font-lock-keyword-face))
  "Level 1: Bilk keywords.")

(defconst bilk-font-lock-keywords-2
  `(,@bilk-font-lock-keywords-1
    ;; Booleans: #t, #f, #true, #false
    ("\\_<#\\(?:t\\(?:rue\\)?\\|f\\(?:alse\\)?\\)\\_>"
     . font-lock-constant-face)
    ;; Character literals: #\x, #\space, #\newline, etc.
    ("#\\\\\\(?:[[:alpha:]][[:alpha:]]*\\|.\\)"
     . font-lock-string-face))
  "Level 2: keywords + booleans + character literals.")

(defconst bilk-font-lock-keywords-3
  `(,@bilk-font-lock-keywords-2
    (,(concat "(" (regexp-opt bilk-builtins t) "\\_>")
     1 font-lock-builtin-face))
  "Level 3: keywords + booleans + chars + R7RS builtins.")

;; ---------------------------------------------------------------------------
;;; Datum comment (#;) font-lock
;; ---------------------------------------------------------------------------

(defun bilk--datum-comment-matcher (limit)
  "Search for #; datum comments up to LIMIT and highlight them."
  (when (re-search-forward "#;" limit t)
    (let ((start (match-beginning 0)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (condition-case nil
          (forward-sexp 1)
        (scan-error (goto-char limit)))
      (set-match-data (list start (point)))
      t)))

;; ---------------------------------------------------------------------------
;;; Indentation — scheme-indent-function properties
;; ---------------------------------------------------------------------------

;; Forms already in scheme-mode: define, lambda, let, let*, letrec, etc.
;; We add bilk-specific forms not covered by scheme-mode.
(dolist (form '((guard . 1)
                (cond-expand . 0)
                (case-lambda . 0)
                (delay-force . 0)
                (export . 0)
                (import . 0)
                (include . 0)
                (include-ci . 0)
                (define-library . 1)
                (define-values . 1)))
  (put (car form) 'scheme-indent-function (cdr form)))

;; ---------------------------------------------------------------------------
;;; Imenu
;; ---------------------------------------------------------------------------

(defconst bilk-imenu-generic-expression
  `(("Libraries"
     ,(rx bol (* space) "("
          "define-library"
          (+ space) "(" (group (+ (not (any ")")))) ")")
     1)
    ("Record Types"
     ,(rx bol (* space) "("
          "define-record-type"
          (+ space) (group (+ (not (any " \t\n)")))))
     1)
    ("Values"
     ,(rx bol (* space) "("
          "define-values"
          (+ space) "(" (group (+ (not (any ")")))) ")")
     1)
    (nil
     ,(rx bol (* space) "("
          (or "define" "define-syntax")
          (+ space) (? "(") (group (+ (not (any " \t\n()")))))
     1))
  "Imenu patterns for Bilk Scheme: libraries, record types, values, defines.")

;; ---------------------------------------------------------------------------
;;; Keymap
;; ---------------------------------------------------------------------------

(defvar bilk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'bilk-eval-last-sexp)
    (define-key map (kbd "C-M-x")   #'bilk-eval-defun)
    (define-key map (kbd "C-c C-r") #'bilk-eval-region)
    (define-key map (kbd "C-c C-b") #'bilk-eval-buffer)
    (define-key map (kbd "C-c C-l") #'bilk-load-file)
    (define-key map (kbd "C-c C-z") #'bilk-switch-to-repl)
    (define-key map (kbd "C-c C-c") #'bilk-interrupt)
    map)
  "Keymap for `bilk-mode'.")

;; Placeholder commands — real implementations live in bilk-repl.el
(defun bilk-eval-last-sexp ()
  "Evaluate the sexp before point in the Bilk REPL." (interactive)
  (message "bilk-repl not loaded"))

(defun bilk-eval-defun ()
  "Evaluate the top-level form at point in the Bilk REPL." (interactive)
  (message "bilk-repl not loaded"))

(defun bilk-eval-region (start end)
  "Evaluate the region from START to END in the Bilk REPL." (interactive "r")
  (message "bilk-repl not loaded"))

(defun bilk-eval-buffer ()
  "Evaluate the entire buffer in the Bilk REPL." (interactive)
  (message "bilk-repl not loaded"))

(defun bilk-load-file (file)
  "Load FILE into the Bilk REPL." (interactive "fLoad file: ")
  (message "bilk-repl not loaded"))

(defun bilk-switch-to-repl ()
  "Switch to the Bilk REPL buffer." (interactive)
  (message "bilk-repl not loaded"))

(defun bilk-interrupt ()
  "Send interrupt to the Bilk REPL." (interactive)
  (message "bilk-repl not loaded"))

;; ---------------------------------------------------------------------------
;;; Mode definition
;; ---------------------------------------------------------------------------

;;;###autoload
(define-derived-mode bilk-mode scheme-mode "Bilk"
  "Major mode for editing Bilk Scheme source files.

Derives from `scheme-mode' for R7RS syntax table, indentation,
and font-lock.  Extends with bilk-specific keywords, R7RS
builtins, and imenu patterns."
  :group 'bilk
  (setq font-lock-defaults
        '((bilk-font-lock-keywords-1
           bilk-font-lock-keywords-2
           bilk-font-lock-keywords-3)
          nil t
          (("+-*/.<>=!?$%_&~^:" . "w")
           (?# . "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  ;; Datum comment highlighting via font-lock
  (font-lock-add-keywords nil
    '((bilk--datum-comment-matcher 0 font-lock-comment-face prepend)))
  ;; Imenu
  (setq imenu-generic-expression bilk-imenu-generic-expression))

;; ---------------------------------------------------------------------------
;;; Auto-mode associations
;; ---------------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scm\\'" . bilk-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sld\\'" . bilk-mode))

(provide 'bilk-mode)
;;; bilk-mode.el ends here
