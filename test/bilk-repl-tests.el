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

;; ---------------------------------------------------------------------------
;;; Error regexp matching
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/error-regexp-matches-located-error ()
  "bilk-error-regexp should match a Bilk error with file:line:col."
  (let ((err "Error: foo.scm:10:5: unbound variable x"))
    (should (string-match bilk-error-regexp err))))

(ert-deftest bilk-repl/error-regexp-extracts-file-line-col ()
  "bilk-error-regexp should capture file, line, and column."
  (let ((err "Error: lib/util.scm:42:13: syntax error"))
    (string-match bilk-error-regexp err)
    (should (equal (match-string 1 err) "lib/util.scm"))
    (should (equal (match-string 2 err) "42"))
    (should (equal (match-string 3 err) "13"))))

(ert-deftest bilk-repl/error-regexp-rejects-plain-error ()
  "bilk-error-regexp should not match an error without location."
  (let ((err "Error: something went wrong"))
    (should-not (string-match bilk-error-regexp err))))

;; ---------------------------------------------------------------------------
;;; Auto-reload on save
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/buffer-library-name-from-sld ()
  "bilk-repl--buffer-library-name extracts library name from .sld buffer."
  (with-temp-buffer
    (insert "(define-library (mylib utils)\n  (export foo))\n")
    (setq buffer-file-name "/tmp/utils.sld")
    (should (equal (bilk-repl--buffer-library-name) "(mylib utils)"))))

(ert-deftest bilk-repl/buffer-library-name-nil-for-scm ()
  "bilk-repl--buffer-library-name returns nil for .scm files."
  (with-temp-buffer
    (insert "(define (foo) 42)\n")
    (setq buffer-file-name "/tmp/foo.scm")
    (should-not (bilk-repl--buffer-library-name))))

(ert-deftest bilk-repl/auto-reload-sends-when-connected ()
  "bilk-repl--maybe-reload-on-save sends reload when connected."
  (let ((sent nil)
        (bilk-repl--connection-status 'ready)
        (bilk-auto-reload t))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent)))
              ((symbol-function 'bilk-repl--buffer-library-name)
               (lambda () "(mylib utils)")))
      (with-temp-buffer
        (rename-buffer "utils.sld" t)
        (bilk-repl--maybe-reload-on-save)
        (should (equal (car sent) '(eval . ",reload (mylib utils)")))))))

(ert-deftest bilk-repl/auto-reload-skipped-when-disconnected ()
  "bilk-repl--maybe-reload-on-save does nothing when disconnected."
  (let ((sent nil)
        (bilk-repl--connection-status 'disconnected)
        (bilk-auto-reload t))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent)))
              ((symbol-function 'bilk-repl--buffer-library-name)
               (lambda () "(mylib utils)")))
      (with-temp-buffer
        (rename-buffer "utils.sld" t)
        (bilk-repl--maybe-reload-on-save)
        (should (null sent))))))

(ert-deftest bilk-repl/auto-reload-skipped-when-disabled ()
  "bilk-repl--maybe-reload-on-save does nothing when bilk-auto-reload is nil."
  (let ((sent nil)
        (bilk-repl--connection-status 'ready)
        (bilk-auto-reload nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent)))
              ((symbol-function 'bilk-repl--buffer-library-name)
               (lambda () "(mylib utils)")))
      (with-temp-buffer
        (rename-buffer "utils.sld" t)
        (bilk-repl--maybe-reload-on-save)
        (should (null sent))))))

;; ---------------------------------------------------------------------------
;;; Comma-command wrappers
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/checkpoint-sends-eval ()
  "bilk-checkpoint should send eval with ,checkpoint NAME."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-checkpoint "snap1")
      (should (equal (car sent) '(eval . ",checkpoint snap1"))))))

(ert-deftest bilk-repl/revert-sends-eval ()
  "bilk-revert should send eval with ,revert NAME."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-revert "snap1")
      (should (equal (car sent) '(eval . ",revert snap1"))))))

(ert-deftest bilk-repl/reload-sends-eval ()
  "bilk-reload should send eval with ,reload LIBRARY."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-reload "(scheme base)")
      (should (equal (car sent) '(eval . ",reload (scheme base)"))))))

(ert-deftest bilk-repl/exports-sends-eval ()
  "bilk-exports should send eval with ,exports LIBRARY."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-exports "(scheme base)")
      (should (equal (car sent) '(eval . ",exports (scheme base)"))))))

(ert-deftest bilk-repl/deps-sends-eval ()
  "bilk-deps should send eval with ,deps LIBRARY."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (bilk-deps "(scheme base)")
      (should (equal (car sent) '(eval . ",deps (scheme base)"))))))

;; ---------------------------------------------------------------------------
;;; REPL prompt defcustom
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/prompt-default-value ()
  "bilk-repl-prompt should default to \"bilk> \"."
  (should (equal bilk-repl-prompt "bilk> ")))

;; ---------------------------------------------------------------------------
;;; Mode derivation
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/mode-not-special ()
  "bilk-repl-mode should not derive from special-mode."
  (with-temp-buffer
    (bilk-repl-mode)
    (should-not (derived-mode-p 'special-mode))))

(ert-deftest bilk-repl/compilation-minor-mode-active ()
  "bilk-repl-mode should have compilation-minor-mode active."
  (with-temp-buffer
    (bilk-repl-mode)
    (should (bound-and-true-p compilation-minor-mode))))

(ert-deftest bilk-repl/ret-bound-to-send-input ()
  "RET should be bound to bilk-repl-send-input in bilk-repl-mode."
  (with-temp-buffer
    (bilk-repl-mode)
    (should (eq (key-binding (kbd "RET")) 'bilk-repl-send-input))))

;; ---------------------------------------------------------------------------
;;; Prompt insertion
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/insert-prompt-text ()
  "After insert-prompt, buffer should contain the prompt string."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (should (equal (buffer-string) "bilk> "))))

(ert-deftest bilk-repl/prompt-marker-position ()
  "prompt-marker should point to the start of the prompt."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (should (= (marker-position bilk-repl--prompt-marker) 1))))

(ert-deftest bilk-repl/input-marker-position ()
  "input-marker should point to the end of the prompt (start of input area)."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (should (= (marker-position bilk-repl--input-marker) 7))))

(ert-deftest bilk-repl/prompt-text-is-read-only ()
  "Prompt text should have the read-only text property."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (should (get-text-property 1 'read-only))))

(ert-deftest bilk-repl/text-after-prompt-is-editable ()
  "Text typed after the prompt should not be read-only."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert "hello"))
    (should-not (get-text-property 7 'read-only))))

;; ---------------------------------------------------------------------------
;;; Input submission
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/send-input-sends-eval ()
  "In idle state, send-input should send (eval . \"user input\")."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (with-temp-buffer
        (bilk-repl-mode)
        (bilk-repl--insert-prompt bilk-repl-prompt)
        (goto-char (point-max))
        (insert "(+ 1 2)")
        (bilk-repl-send-input)
        (should (equal (car sent) '(eval . "(+ 1 2)")))))))

(ert-deftest bilk-repl/send-input-transitions-to-eval-wait ()
  "After send-input, state should be eval-wait."
  (cl-letf (((symbol-function 'bilk-repl--send-client-msg) #'ignore))
    (with-temp-buffer
      (bilk-repl-mode)
      (bilk-repl--insert-prompt bilk-repl-prompt)
      (goto-char (point-max))
      (insert "(+ 1 2)")
      (bilk-repl-send-input)
      (should (eq bilk-repl--repl-state 'eval-wait)))))

(ert-deftest bilk-repl/send-input-commits-read-only-history ()
  "After send-input, prompt+input should be read-only history."
  (cl-letf (((symbol-function 'bilk-repl--send-client-msg) #'ignore))
    (with-temp-buffer
      (bilk-repl-mode)
      (bilk-repl--insert-prompt bilk-repl-prompt)
      (goto-char (point-max))
      (insert "(+ 1 2)")
      (bilk-repl-send-input)
      ;; The committed line "bilk> (+ 1 2)\n" should be read-only
      (should (get-text-property 1 'read-only))
      ;; The user input portion should now also be read-only
      (should (get-text-property 7 'read-only)))))

(ert-deftest bilk-repl/send-input-empty-does-nothing ()
  "Empty input in idle state should not send any message."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (with-temp-buffer
        (bilk-repl-mode)
        (bilk-repl--insert-prompt bilk-repl-prompt)
        (bilk-repl-send-input)
        (should (null sent))))))

;; ---------------------------------------------------------------------------
;;; Status dispatch re-prompts
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/status-ready-in-eval-wait-inserts-prompt ()
  "Receiving (status . ready) in eval-wait should insert a new prompt.
Dispatch is called from outside the REPL buffer (as in the process filter)."
  (let ((repl-buf (generate-new-buffer " *test-repl*")))
    (unwind-protect
        (progn
          (with-current-buffer repl-buf
            (bilk-repl-mode)
            (setq bilk-repl--repl-state 'eval-wait))
          (setq bilk-repl--repl-buffer repl-buf)
          ;; Dispatch from a different buffer, as the process filter would
          (with-temp-buffer
            (bilk-repl--dispatch '(status . ready)))
          (with-current-buffer repl-buf
            (should (string-match-p "bilk> " (buffer-string)))
            (should (eq bilk-repl--repl-state 'idle))))
      (kill-buffer repl-buf))))

(ert-deftest bilk-repl/status-ready-in-idle-no-duplicate ()
  "Receiving (status . ready) in idle should NOT insert a duplicate prompt."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (setq bilk-repl--repl-buffer (current-buffer))
    (let ((content-before (buffer-string)))
      (bilk-repl--dispatch '(status . ready))
      (should (equal (buffer-string) content-before)))))

;; ---------------------------------------------------------------------------
;;; Prompt-aware output insertion
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/output-inserts-before-prompt-in-idle ()
  "In idle state, output should appear before the prompt."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (setq bilk-repl--repl-buffer (current-buffer))
    (bilk-repl--insert-output "hello\n")
    (should (string-match-p "hello\nbilk> " (buffer-string)))))

(ert-deftest bilk-repl/result-inserts-before-prompt-in-idle ()
  "In idle state, result should appear before the prompt."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (setq bilk-repl--repl-buffer (current-buffer))
    (bilk-repl--insert-result "42")
    (should (string-match-p "42\nbilk> " (buffer-string)))))

(ert-deftest bilk-repl/output-inserts-at-end-in-eval-wait ()
  "In eval-wait state, output should appear at point-max."
  (with-temp-buffer
    (bilk-repl-mode)
    (setq bilk-repl--repl-state 'eval-wait)
    (setq bilk-repl--repl-buffer (current-buffer))
    (bilk-repl--insert-output "hello\n")
    (should (equal (buffer-string) "hello\n"))))

(ert-deftest bilk-repl/output-preserves-user-input ()
  "Output inserted while user is typing should preserve typed input."
  (with-temp-buffer
    (bilk-repl-mode)
    (bilk-repl--insert-prompt bilk-repl-prompt)
    (setq bilk-repl--repl-buffer (current-buffer))
    (goto-char (point-max))
    (insert "(+ 1")
    (bilk-repl--insert-output "debug info\n")
    (should (string-match-p "debug info\nbilk> (\\+ 1" (buffer-string)))))

;; ---------------------------------------------------------------------------
;;; Read-request handling
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/read-request-inserts-server-prompt ()
  "A read-request should insert the server's prompt text in the buffer."
  (with-temp-buffer
    (bilk-repl-mode)
    (setq bilk-repl--repl-state 'eval-wait)
    (setq bilk-repl--repl-buffer (current-buffer))
    (bilk-repl--handle-read-request "Enter: ")
    (should (string-match-p "Enter: " (buffer-string)))))

(ert-deftest bilk-repl/read-request-transitions-to-read-wait ()
  "After read-request, state should be read-wait."
  (with-temp-buffer
    (bilk-repl-mode)
    (setq bilk-repl--repl-state 'eval-wait)
    (setq bilk-repl--repl-buffer (current-buffer))
    (bilk-repl--handle-read-request "Enter: ")
    (should (eq bilk-repl--repl-state 'read-wait))))

(ert-deftest bilk-repl/read-wait-send-input-sends-input-msg ()
  "RET in read-wait should send (input . \"text\\n\")."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (with-temp-buffer
        (bilk-repl-mode)
        (setq bilk-repl--repl-state 'eval-wait)
        (setq bilk-repl--repl-buffer (current-buffer))
        (bilk-repl--handle-read-request "Enter: ")
        (goto-char (point-max))
        (insert "42")
        (bilk-repl-send-input)
        (should (equal (car sent) '(input . "42\n")))))))

(ert-deftest bilk-repl/read-wait-send-transitions-to-eval-wait ()
  "After read-wait submit, state should be eval-wait."
  (cl-letf (((symbol-function 'bilk-repl--send-client-msg) #'ignore))
    (with-temp-buffer
      (bilk-repl-mode)
      (setq bilk-repl--repl-state 'eval-wait)
      (setq bilk-repl--repl-buffer (current-buffer))
      (bilk-repl--handle-read-request "Enter: ")
      (goto-char (point-max))
      (insert "42")
      (bilk-repl-send-input)
      (should (eq bilk-repl--repl-state 'eval-wait)))))

;; ---------------------------------------------------------------------------
;;; Bug fix: self-insert must work at prompt
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/self-insert-not-suppressed ()
  "self-insert-command should not be remapped to undefined in bilk-repl-mode."
  (with-temp-buffer
    (bilk-repl-mode)
    ;; If compilation-minor-mode's special-mode-map parent leaks through,
    ;; self-insert-command is remapped to 'undefined'.
    (should-not (eq (key-binding [remap self-insert-command]) 'undefined))))

;; ---------------------------------------------------------------------------
;;; Bug fix: source-buffer eval commits prompt
;; ---------------------------------------------------------------------------

(ert-deftest bilk-repl/source-eval-commits-prompt ()
  "Source-buffer eval should commit the current prompt and transition to eval-wait."
  (let ((sent nil))
    (cl-letf (((symbol-function 'bilk-repl--send-client-msg)
               (lambda (msg) (push msg sent))))
      (with-temp-buffer
        (bilk-repl-mode)
        (bilk-repl--insert-prompt bilk-repl-prompt)
        (setq bilk-repl--repl-buffer (current-buffer))
        (bilk-repl--submit-eval "(+ 1 2)")
        (should (equal (car sent) '(eval . "(+ 1 2)")))
        (should (eq bilk-repl--repl-state 'eval-wait))))))

(ert-deftest bilk-repl/source-eval-output-after-prompt ()
  "After source eval, output should appear after the committed prompt."
  (cl-letf (((symbol-function 'bilk-repl--send-client-msg) #'ignore))
    (with-temp-buffer
      (bilk-repl-mode)
      (bilk-repl--insert-prompt bilk-repl-prompt)
      (setq bilk-repl--repl-buffer (current-buffer))
      (bilk-repl--submit-eval "(+ 1 2)")
      ;; Now in eval-wait; output goes at point-max
      (bilk-repl--insert-output "hello\n")
      (should (string-match-p "bilk> \nhello\n" (buffer-string))))))

(provide 'bilk-repl-tests)
;;; bilk-repl-tests.el ends here
