;;; bilk-lsp-tests.el --- Tests for bilk-lsp.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bilk-lsp)

;; ---------------------------------------------------------------------------
;;; Eglot registration
;; ---------------------------------------------------------------------------

(ert-deftest bilk-lsp/eglot-registration ()
  "bilk-mode should be registered in `eglot-server-programs'."
  (require 'eglot)
  (let ((entry (assoc 'bilk-mode eglot-server-programs)))
    (should entry)
    (should (equal (cdr entry) '("bilk" "lsp")))))

(ert-deftest bilk-lsp/hook-registered ()
  "When `bilk-lsp-enabled' is t, `bilk-mode-hook' should contain the eglot hook."
  (let ((bilk-lsp-enabled t))
    ;; bilk-lsp.el adds a function to bilk-mode-hook that calls eglot-ensure
    (should (memq 'bilk-lsp--maybe-enable bilk-mode-hook))))

(provide 'bilk-lsp-tests)
;;; bilk-lsp-tests.el ends here
