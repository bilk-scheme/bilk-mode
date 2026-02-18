;;; bilk-protocol-tests.el --- Tests for bilk-protocol.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'bilk-protocol)

;; ---------------------------------------------------------------------------
;;; Helpers
;; ---------------------------------------------------------------------------

(defun bilk-test-round-trip-client (msg)
  "Encode client MSG, then decode and return the result."
  (let ((frame (bilk-protocol-encode-client msg)))
    (bilk-protocol-decode-server-or-client frame 0 'client)))

(defun bilk-test-round-trip-server (msg)
  "Encode server MSG, then decode and return the result."
  (let ((frame (bilk-protocol-encode-server msg)))
    (bilk-protocol-decode-server frame 0)))

;; ---------------------------------------------------------------------------
;;; Client message encoding / decoding round-trips
;; ---------------------------------------------------------------------------

(ert-deftest bilk-protocol/round-trip-eval ()
  "Eval message round-trips correctly."
  (let ((result (bilk-test-round-trip-client '(eval . "hello"))))
    (should (equal (car result) '(eval . "hello")))))

(ert-deftest bilk-protocol/round-trip-complete ()
  "Complete message round-trips correctly."
  (let ((result (bilk-test-round-trip-client '(complete . "(vec"))))
    (should (equal (car result) '(complete . "(vec")))))

(ert-deftest bilk-protocol/round-trip-interrupt ()
  "Interrupt message round-trips correctly."
  (let ((result (bilk-test-round-trip-client '(interrupt))))
    (should (equal (car result) '(interrupt)))))

(ert-deftest bilk-protocol/round-trip-input ()
  "Input message round-trips correctly."
  (let ((result (bilk-test-round-trip-client '(input . "42\n"))))
    (should (equal (car result) '(input . "42\n")))))

(ert-deftest bilk-protocol/round-trip-resume ()
  "Resume message round-trips correctly."
  (let ((result (bilk-test-round-trip-client '(resume . "next"))))
    (should (equal (car result) '(resume . "next")))))

(ert-deftest bilk-protocol/round-trip-disconnect ()
  "Disconnect message round-trips correctly."
  (let ((result (bilk-test-round-trip-client '(disconnect))))
    (should (equal (car result) '(disconnect)))))

;; ---------------------------------------------------------------------------
;;; Server message encoding / decoding round-trips
;; ---------------------------------------------------------------------------

(ert-deftest bilk-protocol/round-trip-output ()
  "Output message round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(output . "hello world\n"))))
    (should (equal (car result) '(output . "hello world\n")))))

(ert-deftest bilk-protocol/round-trip-result ()
  "Result message round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(result . "42"))))
    (should (equal (car result) '(result . "42")))))

(ert-deftest bilk-protocol/round-trip-error ()
  "Error message round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(error . "unbound variable"))))
    (should (equal (car result) '(error . "unbound variable")))))

(ert-deftest bilk-protocol/round-trip-completions ()
  "Completions message round-trips correctly."
  (let ((result (bilk-test-round-trip-server
                 '(completions "vector-ref" "vector-set!" "vector-copy"))))
    (should (equal (car result)
                   '(completions "vector-ref" "vector-set!" "vector-copy")))))

(ert-deftest bilk-protocol/round-trip-completions-empty ()
  "Empty completions list round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(completions))))
    (should (equal (car result) '(completions)))))

(ert-deftest bilk-protocol/round-trip-read-request ()
  "Read-request message round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(read-request . "> "))))
    (should (equal (car result) '(read-request . "> ")))))

(ert-deftest bilk-protocol/round-trip-status-ready ()
  "Status Ready round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(status . ready))))
    (should (equal (car result) '(status . ready)))))

(ert-deftest bilk-protocol/round-trip-status-busy ()
  "Status Busy round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(status . busy))))
    (should (equal (car result) '(status . busy)))))

(ert-deftest bilk-protocol/round-trip-session-ok ()
  "Session-ok round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(session-ok))))
    (should (equal (car result) '(session-ok)))))

(ert-deftest bilk-protocol/round-trip-session-deny ()
  "Session-deny round-trips correctly."
  (let ((result (bilk-test-round-trip-server '(session-deny))))
    (should (equal (car result) '(session-deny)))))

;; ---------------------------------------------------------------------------
;;; Frame completeness detection
;; ---------------------------------------------------------------------------

(ert-deftest bilk-protocol/frame-complete-full ()
  "A complete frame should be detected."
  (let ((frame (bilk-protocol-encode-client '(eval . "x"))))
    (should (bilk-protocol-frame-complete-p frame 0))))

(ert-deftest bilk-protocol/frame-complete-partial-header ()
  "3 bytes is not enough for a frame header."
  (should-not (bilk-protocol-frame-complete-p "\x00\x00\x05" 0)))

(ert-deftest bilk-protocol/frame-complete-partial-payload ()
  "Header present but payload truncated."
  ;; Frame says 10 bytes of payload but only 5 available
  (should-not (bilk-protocol-frame-complete-p
               "\x00\x00\x00\x0a\x01hello" 0)))

(ert-deftest bilk-protocol/frame-complete-empty-buffer ()
  "Empty string has no complete frame."
  (should-not (bilk-protocol-frame-complete-p "" 0)))

;; ---------------------------------------------------------------------------
;;; UTF-8 payloads
;; ---------------------------------------------------------------------------

(ert-deftest bilk-protocol/utf8-eval ()
  "UTF-8 strings survive encode/decode."
  (let* ((text "(display \"λ α β γ\")")
         (result (bilk-test-round-trip-client (cons 'eval text))))
    (should (equal (cdar result) text))))

(ert-deftest bilk-protocol/utf8-output ()
  "UTF-8 server output survives encode/decode."
  (let* ((text "λ = 42\n")
         (result (bilk-test-round-trip-server (cons 'output text))))
    (should (equal (cdar result) text))))

;; ---------------------------------------------------------------------------
;;; Offset handling (multiple frames in buffer)
;; ---------------------------------------------------------------------------

(ert-deftest bilk-protocol/decode-at-offset ()
  "Decoding at a non-zero offset works correctly."
  (let* ((frame1 (bilk-protocol-encode-server '(output . "one")))
         (frame2 (bilk-protocol-encode-server '(result . "two")))
         (buf (concat frame1 frame2))
         (result (bilk-protocol-decode-server buf (length frame1))))
    (should (equal (car result) '(result . "two")))))

;; ---------------------------------------------------------------------------
;;; Constants
;; ---------------------------------------------------------------------------

(ert-deftest bilk-protocol/max-frame-size ()
  "Max frame size should be 16 MiB."
  (should (= bilk-protocol-max-frame-size (* 16 1024 1024))))

(provide 'bilk-protocol-tests)
;;; bilk-protocol-tests.el ends here
