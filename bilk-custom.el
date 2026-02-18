;;; bilk-custom.el --- Customization variables for Bilk Scheme  -*- lexical-binding: t; -*-

;;; Commentary:

;; Leaf module: defines the `bilk' customization group and all
;; user-facing options.  No dependencies beyond Emacs builtins.

;;; Code:

(defgroup bilk nil
  "Bilk Scheme development environment."
  :group 'scheme
  :prefix "bilk-")

(defcustom bilk-program "bilk"
  "Path to the bilk executable."
  :type 'string
  :group 'bilk)

(defcustom bilk-repl-port 7890
  "Default TCP port for the Bilk REPL server."
  :type 'integer
  :group 'bilk)

(defcustom bilk-lsp-enabled t
  "When non-nil, start eglot automatically in `bilk-mode' buffers."
  :type 'boolean
  :group 'bilk)

(provide 'bilk-custom)
;;; bilk-custom.el ends here
