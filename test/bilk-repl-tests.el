;;; bilk-repl-tests.el --- Tests for bilk-repl.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bilk-protocol)
(require 'bilk-repl)

;; ---------------------------------------------------------------------------
;;; Test helpers
;; ---------------------------------------------------------------------------

(defvar bilk-test-sent-messages nil
  "Accumulator for messages sent during tests.")

(defmacro bilk-test-with-source-buffer (content &rest body)
  "Set up a bilk-mode buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (bilk-mode)
     (insert ,content)
     ,@body))

;; ---------------------------------------------------------------------------
;;; Text extraction for eval commands
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/extract-last-sexp ()
  "bilk-repl--last-sexp should return the sexp before point."
  (bilk-test-with-source-buffer "(+ 1 2)"
    (goto-char (point-max))
    (should (equal (bilk-repl--last-sexp) "(+ 1 2)"))))

(ert-deftest bilk-repl/extract-last-sexp-nested ()
  "bilk-repl--last-sexp handles nested forms."
  (bilk-test-with-source-buffer "(define (f x) (+ x 1))"
    (goto-char (point-max))
    (should (equal (bilk-repl--last-sexp) "(define (f x) (+ x 1))"))))

(ert-deftest bilk-repl/extract-defun ()
  "bilk-repl--defun-at-point returns the top-level form."
  (bilk-test-with-source-buffer "(define (f x)\n  (+ x 1))\n\n(f 42)"
    (goto-char 20) ;; inside the define
    (should (equal (bilk-repl--defun-at-point) "(define (f x)\n  (+ x 1))"))))

;; ---------------------------------------------------------------------------
;;; Process filter: frame accumulation and dispatch
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/filter-accumulates-partial ()
  "The process filter accumulates partial data without dispatching."
  (let ((bilk-repl--receive-buffer "")
        (dispatched nil))
    (cl-letf (((symbol-function 'bilk-repl--dispatch) (lambda (_msg) (push _msg dispatched))))
      ;; Send just the first 3 bytes of a frame header
      (bilk-repl--filter-data "\x00\x00\x05")
      (should (equal bilk-repl--receive-buffer "\x00\x00\x05"))
      (should (null dispatched)))))

(ert-deftest bilk-repl/filter-dispatches-complete ()
  "The process filter dispatches a complete frame."
  (let ((bilk-repl--receive-buffer "")
        (dispatched nil))
    (cl-letf (((symbol-function 'bilk-repl--dispatch) (lambda (msg) (push msg dispatched))))
      (let ((frame (bilk-protocol-encode-server '(result . "42"))))
        (bilk-repl--filter-data frame)
        (should (equal (length dispatched) 1))
        (should (equal (car dispatched) '(result . "42")))
        (should (equal bilk-repl--receive-buffer ""))))))

(ert-deftest bilk-repl/filter-handles-multiple-frames ()
  "The process filter handles multiple frames arriving at once."
  (let ((bilk-repl--receive-buffer "")
        (dispatched nil))
    (cl-letf (((symbol-function 'bilk-repl--dispatch) (lambda (msg) (push msg dispatched))))
      (let ((data (concat (bilk-protocol-encode-server '(output . "hi\n"))
                          (bilk-protocol-encode-server '(result . "42")))))
        (bilk-repl--filter-data data)
        (should (equal (length dispatched) 2))
        ;; dispatched is in reverse order (push)
        (should (equal (cadr dispatched) '(output . "hi\n")))
        (should (equal (car dispatched) '(result . "42")))
        (should (equal bilk-repl--receive-buffer ""))))))

(ert-deftest bilk-repl/filter-handles-split-frame ()
  "The process filter handles a frame split across two calls."
  (let ((bilk-repl--receive-buffer "")
        (dispatched nil))
    (cl-letf (((symbol-function 'bilk-repl--dispatch) (lambda (msg) (push msg dispatched))))
      (let* ((frame (bilk-protocol-encode-server '(result . "hello")))
             (mid (/ (length frame) 2)))
        (bilk-repl--filter-data (substring frame 0 mid))
        (should (null dispatched))
        (bilk-repl--filter-data (substring frame mid))
        (should (equal (length dispatched) 1))
        (should (equal (car dispatched) '(result . "hello")))))))

;; ---------------------------------------------------------------------------
;;; Eval commands produce correct protocol messages
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/eval-last-sexp-sends-eval ()
  "bilk-eval-last-sexp should send an eval message with the sexp text."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-test-with-source-buffer "(+ 1 2)"
        (goto-char (point-max))
        (bilk-eval-last-sexp)
        (should (equal (car sent) '(eval . "(+ 1 2)")))))))

(ert-deftest bilk-repl/eval-region-sends-eval ()
  "bilk-eval-region should send an eval message with the region text."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-test-with-source-buffer "(+ 1 2)\n(* 3 4)"
        (bilk-eval-region (point-min) (point-max))
        (should (equal (car sent) '(eval . "(+ 1 2)\n(* 3 4)")))))))

;; ---------------------------------------------------------------------------
;;; Completion bridge
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/completion-at-point-prefix ()
  "bilk-completion-at-point returns correct bounds."
  (bilk-test-with-source-buffer "(vec"
    (goto-char (point-max))
    (let ((result (bilk-repl--completion-bounds)))
      ;; bounds should be (2 . 5) — the symbol "vec"
      (should (equal (car result) 2))
      (should (equal (cdr result) 5)))))

;; ---------------------------------------------------------------------------
;;; Mode-line status
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/mode-line-disconnected ()
  "When not connected, mode-line indicator should show disconnected."
  (let ((bilk-repl--connection-status 'disconnected))
    (should (string-match "disconnected"
                          (bilk-repl--mode-line-string)))))

(ert-deftest bilk-repl/mode-line-ready ()
  "When connected and ready, mode-line should show ready."
  (let ((bilk-repl--connection-status 'ready))
    (should (string-match "ready"
                          (bilk-repl--mode-line-string)))))

(ert-deftest bilk-repl/mode-line-busy ()
  "When evaluating, mode-line should show busy."
  (let ((bilk-repl--connection-status 'busy))
    (should (string-match "busy"
                          (bilk-repl--mode-line-string)))))

(provide 'bilk-repl-tests)
;;; bilk-repl-tests.el ends here
