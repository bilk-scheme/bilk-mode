;;; bilk-mode-tests.el --- Tests for bilk-custom.el and bilk-mode.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'imenu)
(require 'bilk-custom)
(require 'bilk-mode)

;; ---------------------------------------------------------------------------
;;; Test helpers
;; ---------------------------------------------------------------------------

(defmacro bilk-test-with-buffer (content &rest body)
  "Insert CONTENT into a temp buffer in `bilk-mode', then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (bilk-mode)
     (insert ,content)
     (font-lock-ensure)
     ,@body))

(defun bilk-test-face-at (pos)
  "Return the face at POS in the current buffer."
  (get-text-property pos 'face))

(defun bilk-test-has-face-p (pos face)
  "Return non-nil if POS has FACE (handles both single face and face list)."
  (let ((f (bilk-test-face-at pos)))
    (or (eq f face)
        (and (listp f) (memq face f)))))

;; ---------------------------------------------------------------------------
;;; bilk-custom tests
;; ---------------------------------------------------------------------------

(ert-deftest bilk-custom/group-exists ()
  "The `bilk' customization group should exist."
  (should (get 'bilk 'custom-group)))

(ert-deftest bilk-custom/bilk-program-default ()
  "`bilk-program' should default to \"bilk\"."
  (should (equal (default-value 'bilk-program) "bilk")))

(ert-deftest bilk-custom/bilk-repl-port-default ()
  "`bilk-repl-port' should default to 7890."
  (should (equal (default-value 'bilk-repl-port) 7890)))

(ert-deftest bilk-custom/bilk-lsp-enabled-default ()
  "`bilk-lsp-enabled' should default to t."
  (should (equal (default-value 'bilk-lsp-enabled) t)))

;; ---------------------------------------------------------------------------
;;; Mode activation
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/derives-from-scheme-mode ()
  "bilk-mode should derive from scheme-mode."
  (with-temp-buffer
    (bilk-mode)
    (should (derived-mode-p 'scheme-mode))))

(ert-deftest bilk-mode/mode-name ()
  "Mode name should be \"Bilk\"."
  (with-temp-buffer
    (bilk-mode)
    (should (equal mode-name "Bilk"))))

;; ---------------------------------------------------------------------------
;;; auto-mode-alist
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/auto-mode-scm ()
  ".scm files should associate with bilk-mode."
  (should (eq (cdr (assoc "\\.scm\\'" auto-mode-alist)) 'bilk-mode)))

(ert-deftest bilk-mode/auto-mode-sld ()
  ".sld files should associate with bilk-mode."
  (should (eq (cdr (assoc "\\.sld\\'" auto-mode-alist)) 'bilk-mode)))

;; ---------------------------------------------------------------------------
;;; Font-lock: keywords
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/font-lock-define ()
  "`define' should be highlighted as a keyword."
  (bilk-test-with-buffer "(define x 1)"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-lambda ()
  "`lambda' should be highlighted as a keyword."
  (bilk-test-with-buffer "(lambda (x) x)"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-guard ()
  "`guard' should be highlighted as a keyword."
  (bilk-test-with-buffer "(guard (exn (#t 'err)) body)"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-case-lambda ()
  "`case-lambda' should be highlighted (from expander.ml)."
  (bilk-test-with-buffer "(case-lambda ((x) x) ((x y) y))"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-delay-force ()
  "`delay-force' should be highlighted (from expander.ml)."
  (bilk-test-with-buffer "(delay-force expr)"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-cond-expand ()
  "`cond-expand' should be highlighted."
  (bilk-test-with-buffer "(cond-expand ((library (scheme base)) body))"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-import ()
  "`import' should be highlighted."
  (bilk-test-with-buffer "(import (scheme base))"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-define-library ()
  "`define-library' should be highlighted."
  (bilk-test-with-buffer "(define-library (foo bar))"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

(ert-deftest bilk-mode/font-lock-define-record-type ()
  "`define-record-type' should be highlighted."
  (bilk-test-with-buffer "(define-record-type <point>)"
    (should (eq (bilk-test-face-at 2) 'font-lock-keyword-face))))

;; ---------------------------------------------------------------------------
;;; Font-lock: booleans
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/font-lock-true ()
  "`#t' should be highlighted as a constant."
  (bilk-test-with-buffer "#t"
    (should (eq (bilk-test-face-at 1) 'font-lock-constant-face))))

(ert-deftest bilk-mode/font-lock-false ()
  "`#f' should be highlighted as a constant."
  (bilk-test-with-buffer "#f"
    (should (eq (bilk-test-face-at 1) 'font-lock-constant-face))))

(ert-deftest bilk-mode/font-lock-true-long ()
  "`#true' should be highlighted as a constant."
  (bilk-test-with-buffer "#true"
    (should (eq (bilk-test-face-at 1) 'font-lock-constant-face))))

(ert-deftest bilk-mode/font-lock-false-long ()
  "`#false' should be highlighted as a constant."
  (bilk-test-with-buffer "#false"
    (should (eq (bilk-test-face-at 1) 'font-lock-constant-face))))

;; ---------------------------------------------------------------------------
;;; Font-lock: character literals
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/font-lock-char-literal ()
  "`#\\a' should be highlighted as a string/char."
  (bilk-test-with-buffer "#\\a"
    (should (eq (bilk-test-face-at 1) 'font-lock-string-face))))

(ert-deftest bilk-mode/font-lock-char-space ()
  "`#\\space' should be highlighted."
  (bilk-test-with-buffer "#\\space"
    (should (eq (bilk-test-face-at 1) 'font-lock-string-face))))

(ert-deftest bilk-mode/font-lock-char-newline ()
  "`#\\newline' should be highlighted."
  (bilk-test-with-buffer "#\\newline"
    (should (eq (bilk-test-face-at 1) 'font-lock-string-face))))

;; ---------------------------------------------------------------------------
;;; Font-lock: R7RS builtins (level 3)
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/font-lock-builtin-car ()
  "`car' should be highlighted as a builtin at font-lock level 3."
  (bilk-test-with-buffer "(car xs)"
    (should (eq (bilk-test-face-at 2) 'font-lock-builtin-face))))

(ert-deftest bilk-mode/font-lock-builtin-display ()
  "`display' should be highlighted as a builtin."
  (bilk-test-with-buffer "(display x)"
    (should (eq (bilk-test-face-at 2) 'font-lock-builtin-face))))

(ert-deftest bilk-mode/font-lock-builtin-call/cc ()
  "`call/cc' should be highlighted as a builtin."
  (bilk-test-with-buffer "(call/cc k)"
    (should (eq (bilk-test-face-at 2) 'font-lock-builtin-face))))

(ert-deftest bilk-mode/font-lock-builtin-vector-ref ()
  "`vector-ref' should be highlighted as a builtin."
  (bilk-test-with-buffer "(vector-ref v 0)"
    (should (eq (bilk-test-face-at 2) 'font-lock-builtin-face))))

;; ---------------------------------------------------------------------------
;;; Indentation
;; ---------------------------------------------------------------------------

(defun bilk-test-indent (input expected)
  "Assert that indenting INPUT yields EXPECTED in `bilk-mode'."
  (with-temp-buffer
    (bilk-mode)
    (setq indent-tabs-mode nil)
    (insert input)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) expected))))

(ert-deftest bilk-mode/indent-guard ()
  "`guard' clauses align under first element, body at +2."
  (bilk-test-indent
   "(guard (exn\n(#t 'err))\nbody)"
   "(guard (exn\n        (#t 'err))\n  body)"))

(ert-deftest bilk-mode/indent-case-lambda ()
  "`case-lambda' body clauses indent at +2."
  (bilk-test-indent
   "(case-lambda\n((x) x)\n((x y) y))"
   "(case-lambda\n  ((x) x)\n  ((x y) y))"))

(ert-deftest bilk-mode/indent-define-library ()
  "`define-library' body indent at +2."
  (bilk-test-indent
   "(define-library (foo bar)\n(export baz))"
   "(define-library (foo bar)\n  (export baz))"))

(ert-deftest bilk-mode/indent-cond-expand ()
  "`cond-expand' clauses indent at +2."
  (bilk-test-indent
   "(cond-expand\n(r7rs (import (scheme base)))\n(else #f))"
   "(cond-expand\n  (r7rs (import (scheme base)))\n  (else #f))"))

(ert-deftest bilk-mode/indent-delay-force ()
  "`delay-force' body at +2."
  (bilk-test-indent
   "(delay-force\nexpr)"
   "(delay-force\n  expr)"))

;; ---------------------------------------------------------------------------
;;; Imenu
;; ---------------------------------------------------------------------------

(defun bilk-test-imenu-find (index pattern)
  "Search INDEX (possibly nested) for an entry whose name matches PATTERN."
  (cl-find-if
   (lambda (entry)
     (or (and (stringp (car entry))
              (string-match pattern (car entry)))
         ;; Recurse into subcategory alists
         (and (consp (cdr entry))
              (consp (cadr entry))
              (bilk-test-imenu-find (cdr entry) pattern))))
   index))

(ert-deftest bilk-mode/imenu-define-library ()
  "Imenu should index `define-library' forms."
  (bilk-test-with-buffer "(define-library (foo bar)\n  (export baz))\n"
    (let ((index (funcall imenu-create-index-function)))
      (should (bilk-test-imenu-find index "foo bar")))))

(ert-deftest bilk-mode/imenu-define-record-type ()
  "Imenu should index `define-record-type' forms."
  (bilk-test-with-buffer "(define-record-type <point>\n  (make-point x y)\n  point?\n  (x point-x)\n  (y point-y))\n"
    (let ((index (funcall imenu-create-index-function)))
      (should (bilk-test-imenu-find index "<point>")))))

(ert-deftest bilk-mode/imenu-define-values ()
  "Imenu should index `define-values' forms."
  (bilk-test-with-buffer "(define-values (a b) (values 1 2))\n"
    (let ((index (funcall imenu-create-index-function)))
      (should (bilk-test-imenu-find index "a b")))))

;; ---------------------------------------------------------------------------
;;; Keymap
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/keymap-exists ()
  "`bilk-mode-map' should be a keymap."
  (should (keymapp bilk-mode-map)))

(ert-deftest bilk-mode/keymap-eval-last-sexp ()
  "C-x C-e should be bound in `bilk-mode-map'."
  (should (lookup-key bilk-mode-map (kbd "C-x C-e"))))

(ert-deftest bilk-mode/keymap-eval-defun ()
  "C-M-x should be bound in `bilk-mode-map'."
  (should (lookup-key bilk-mode-map (kbd "C-M-x"))))

(ert-deftest bilk-mode/keymap-switch-to-repl ()
  "C-c C-z should be bound in `bilk-mode-map'."
  (should (lookup-key bilk-mode-map (kbd "C-c C-z"))))

;; ---------------------------------------------------------------------------
;;; Comment syntax
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/block-comment-syntax ()
  "Block comments #|...|# should be recognized."
  (bilk-test-with-buffer "#| block comment |# visible"
    (should (eq (bilk-test-face-at 1) 'font-lock-comment-face))
    ;; "visible" should not be in comment face
    (should-not (eq (bilk-test-face-at 22) 'font-lock-comment-face))))

(ert-deftest bilk-mode/datum-comment-syntax ()
  "#; should comment out the next datum."
  (bilk-test-with-buffer "#;(foo bar) visible"
    (should (bilk-test-has-face-p 1 'font-lock-comment-face))))

;; ---------------------------------------------------------------------------
;;; Pipe identifiers (|...|)
;; ---------------------------------------------------------------------------

(ert-deftest bilk-mode/pipe-identifier-not-comment ()
  "|foo bar| should not be treated as a comment."
  (bilk-test-with-buffer "(define |foo bar| 42)"
    ;; The 42 should not be commented out
    (goto-char (point-max))
    (search-backward "42")
    (should-not (eq (bilk-test-face-at (point)) 'font-lock-comment-face))))

(provide 'bilk-mode-tests)
;;; bilk-mode-tests.el ends here
