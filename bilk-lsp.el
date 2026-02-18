;;; bilk-lsp.el --- Eglot integration for Bilk Scheme  -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin layer: registers `bilk-mode' with eglot so that `bilk lsp'
;; provides diagnostics, completion, hover, go-to-definition, etc.

;;; Code:

(require 'bilk-custom)
(require 'bilk-mode)

(defun bilk-lsp--maybe-enable ()
  "Start eglot in the current buffer when `bilk-lsp-enabled' is non-nil."
  (when bilk-lsp-enabled
    (when (fboundp 'eglot-ensure)
      (eglot-ensure))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(bilk-mode . ("bilk" "lsp"))))

(add-hook 'bilk-mode-hook #'bilk-lsp--maybe-enable)

(provide 'bilk-lsp)
;;; bilk-lsp.el ends here
