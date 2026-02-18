;;; bilk-debug-tests.el --- Tests for bilk-debug.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bilk-debug)

;; ---------------------------------------------------------------------------
;;; Dape configuration (skipped when dape is not installed)
;; ---------------------------------------------------------------------------

(defun bilk-test-dape-available-p ()
  "Return non-nil when `dape' can be loaded."
  (condition-case nil (require 'dape) (error nil)))

(ert-deftest bilk-debug/dape-config-registered ()
  "bilk-debug should be registered in `dape-configs'."
  (skip-unless (bilk-test-dape-available-p))
  (should (assoc 'bilk-debug dape-configs)))

(ert-deftest bilk-debug/dape-config-command ()
  "The dape config should use the bilk executable."
  (skip-unless (bilk-test-dape-available-p))
  (let ((config (cdr (assoc 'bilk-debug dape-configs))))
    (should (equal (plist-get config 'command) "bilk"))))

(ert-deftest bilk-debug/dape-config-args ()
  "The dape config should pass 'debug' as the first arg."
  (skip-unless (bilk-test-dape-available-p))
  (let ((config (cdr (assoc 'bilk-debug dape-configs))))
    (should (member "debug" (plist-get config 'command-args)))))

;; ---------------------------------------------------------------------------
;;; Launch args (does not require dape)
;; ---------------------------------------------------------------------------

(ert-deftest bilk-debug/launch-args-construction ()
  "bilk-debug--launch-args should build correct dape args for a file."
  (let ((args (bilk-debug--launch-args "/tmp/test.scm")))
    (should (member "debug" (plist-get args 'command-args)))
    (should (member "/tmp/test.scm" (plist-get args 'command-args)))))

(provide 'bilk-debug-tests)
;;; bilk-debug-tests.el ends here
