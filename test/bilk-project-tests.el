;;; bilk-project-tests.el --- Tests for bilk-project.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bilk-project)

;; ---------------------------------------------------------------------------
;;; project-try
;; ---------------------------------------------------------------------------

(ert-deftest bilk-project/try-with-package-scm ()
  "bilk-project-try returns a project when package.scm exists."
  (let ((dir (make-temp-file "bilk-proj" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "package.scm" dir))
          (let ((proj (bilk-project-try dir)))
            (should proj)
            (should (eq (car proj) 'bilk))))
      (delete-directory dir t))))

(ert-deftest bilk-project/try-without-package-scm ()
  "bilk-project-try returns nil when no package.scm exists."
  (let ((dir (make-temp-file "bilk-proj" t)))
    (unwind-protect
        (should-not (bilk-project-try dir))
      (delete-directory dir t))))

(ert-deftest bilk-project/project-root-method ()
  "project-root should return the root for a bilk project."
  (let ((dir (make-temp-file "bilk-proj" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "package.scm" dir))
          (let ((proj (bilk-project-try dir)))
            (should (equal (project-root proj)
                           (file-name-as-directory dir)))))
      (delete-directory dir t))))

;; ---------------------------------------------------------------------------
;;; Build commands
;; ---------------------------------------------------------------------------

(ert-deftest bilk-project/build-compile-command ()
  "bilk-build should set compile-command to 'bilk build'."
  (let ((dir (make-temp-file "bilk-proj" t))
        (compile-command nil)
        (called-with nil))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "package.scm" dir))
          (cl-letf (((symbol-function 'compile)
                     (lambda (cmd) (setq called-with cmd)))
                    ((symbol-function 'bilk--find-package-root)
                     (lambda (_) dir)))
            (let ((default-directory dir))
              (bilk-build)
              (should (equal called-with "bilk build")))))
      (delete-directory dir t))))

(ert-deftest bilk-project/test-compile-command ()
  "bilk-test should set compile-command to 'bilk test'."
  (let ((dir (make-temp-file "bilk-proj" t))
        (called-with nil))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "package.scm" dir))
          (cl-letf (((symbol-function 'compile)
                     (lambda (cmd) (setq called-with cmd)))
                    ((symbol-function 'bilk--find-package-root)
                     (lambda (_) dir)))
            (let ((default-directory dir))
              (bilk-test)
              (should (equal called-with "bilk test")))))
      (delete-directory dir t))))

(ert-deftest bilk-project/profile-compile-command ()
  "bilk-profile should set compile-command to 'bilk profile FILE'."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq called-with cmd)))
              ((symbol-function 'bilk--find-package-root)
               (lambda (_) "/proj")))
      (let ((default-directory "/proj/"))
        (bilk-profile "main.scm")
        (should (equal called-with "bilk profile main.scm"))))))

(ert-deftest bilk-project/error-regexp-in-compilation-alist ()
  "bilk-error should be registered in compilation-error-regexp-alist-alist."
  (require 'compile)
  (should (assq 'bilk-error compilation-error-regexp-alist-alist)))

(provide 'bilk-project-tests)
;;; bilk-project-tests.el ends here
