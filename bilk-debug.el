;;; bilk-debug.el --- DAP integration for Bilk Scheme via dape  -*- lexical-binding: t; -*-

;;; Commentary:

;; Optional module: registers a dape configuration for `bilk debug'.
;; Dape handles all DAP communication; this module only configures it.

;;; Code:

(require 'bilk-custom)
(require 'bilk-mode)

;; ---------------------------------------------------------------------------
;;; Launch args builder (no dape dependency)
;; ---------------------------------------------------------------------------

(defun bilk-debug--launch-args (file)
  "Build a dape-compatible plist for debugging FILE."
  (list 'command bilk-program
        'command-args (list "debug" file)
        'modes '(bilk-mode)))

;; ---------------------------------------------------------------------------
;;; Dape configuration (deferred until dape loads)
;; ---------------------------------------------------------------------------

;;;###autoload
(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(bilk-debug
                 modes (bilk-mode)
                 command ,bilk-program
                 command-args ("debug" ,(or (buffer-file-name) ""))
                 :type "bilk"
                 :request "launch")))

;; ---------------------------------------------------------------------------
;;; Interactive commands
;; ---------------------------------------------------------------------------

(defun bilk-debug-file ()
  "Start debugging the current file with dape."
  (interactive)
  (unless (fboundp 'dape)
    (user-error "dape is not installed"))
  (let ((file (or (buffer-file-name)
                  (user-error "Buffer is not visiting a file"))))
    (dape (bilk-debug--launch-args file))))

(defun bilk-toggle-breakpoint ()
  "Toggle a breakpoint at point (convenience wrapper for dape)."
  (interactive)
  (if (fboundp 'dape-breakpoint-toggle)
      (call-interactively #'dape-breakpoint-toggle)
    (user-error "dape is not installed")))

(provide 'bilk-debug)
;;; bilk-debug.el ends here
