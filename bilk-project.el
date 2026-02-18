;;; bilk-project.el --- Project.el integration for Bilk Scheme  -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrates Bilk Scheme projects with Emacs project.el.
;; Recognizes directories containing package.scm as project roots
;; and provides build, test, and profile commands.

;;; Code:

(require 'project)
(require 'compile)
(require 'bilk-custom)
(require 'bilk-mode)

;; ---------------------------------------------------------------------------
;;; Project backend
;; ---------------------------------------------------------------------------

;;;###autoload
(defun bilk-project-try (dir)
  "Return a Bilk project for DIR if `package.scm' is found, or nil."
  (when-let ((root (bilk--find-package-root dir)))
    (cons 'bilk root)))

;;;###autoload
(cl-defmethod project-root ((project (head bilk)))
  "Return the root directory for a Bilk PROJECT."
  (file-name-as-directory (cdr project)))

;;;###autoload
(add-hook 'project-find-functions #'bilk-project-try)

;; ---------------------------------------------------------------------------
;;; Build commands
;; ---------------------------------------------------------------------------

(defun bilk-build ()
  "Build the current Bilk project."
  (interactive)
  (let ((default-directory (or (bilk--find-package-root default-directory)
                               default-directory)))
    (compile "bilk build")))

(defun bilk-test ()
  "Run tests for the current Bilk project."
  (interactive)
  (let ((default-directory (or (bilk--find-package-root default-directory)
                               default-directory)))
    (compile "bilk test")))

(defun bilk-profile (file)
  "Profile FILE in the current Bilk project."
  (interactive "fProfile file: ")
  (let ((default-directory (or (bilk--find-package-root default-directory)
                               default-directory)))
    (compile (format "bilk profile %s" file))))

;; ---------------------------------------------------------------------------
;;; Register error regexp with compilation-mode
;; ---------------------------------------------------------------------------

;;;###autoload
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               (list 'bilk-error bilk-error-regexp 1 2 3 2))
  (add-to-list 'compilation-error-regexp-alist 'bilk-error))

(provide 'bilk-project)
;;; bilk-project.el ends here
