;;; bilk-repl.el --- REPL client for Bilk Scheme  -*- lexical-binding: t; -*-

;;; Commentary:

;; Manages the connection to `bilk serve', sends eval/complete/interrupt
;; messages via bilk-protocol, and displays results in a REPL buffer.
;;
;; Architecture:
;;   bilk-repl--send-client-msg   — encode & transmit a client message
;;   bilk-repl--filter-data       — accumulate bytes, decode frames, dispatch
;;   bilk-repl--dispatch          — route decoded server messages to handlers

;;; Code:

(require 'bilk-custom)
(require 'bilk-protocol)
(require 'bilk-mode)

;; ---------------------------------------------------------------------------
;;; State
;; ---------------------------------------------------------------------------

(defvar bilk-repl--process nil
  "The network process connected to `bilk serve'.")

(defvar bilk-repl--server-process nil
  "The subprocess running `bilk serve'.")

(defvar bilk-repl--receive-buffer ""
  "Unibyte string accumulating incoming data from the server.")

(defvar bilk-repl--connection-status 'disconnected
  "Current connection status: `disconnected', `ready', or `busy'.")

(defvar bilk-repl--repl-buffer nil
  "The REPL output buffer.")

(defvar bilk-repl--pending-completions nil
  "Callback for in-flight completion request, or nil.")

;; ---------------------------------------------------------------------------
;;; Mode-line
;; ---------------------------------------------------------------------------

(defun bilk-repl--mode-line-string ()
  "Return a mode-line string describing the REPL connection status."
  (format " [bilk: %s]" (symbol-name bilk-repl--connection-status)))

;; ---------------------------------------------------------------------------
;;; Text extraction helpers
;; ---------------------------------------------------------------------------

(defun bilk-repl--last-sexp ()
  "Return the sexp ending at point as a string."
  (save-excursion
    (let ((end (point)))
      (backward-sexp 1)
      (buffer-substring-no-properties (point) end))))

(defun bilk-repl--defun-at-point ()
  "Return the top-level form at point as a string."
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n")
    (let ((end (point)))
      (beginning-of-defun)
      (buffer-substring-no-properties (point) end))))

(defun bilk-repl--completion-bounds ()
  "Return (START . END) of the symbol at point, or nil."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (cons (car bounds) (cdr bounds)))))

;; ---------------------------------------------------------------------------
;;; Sending messages
;; ---------------------------------------------------------------------------

(defun bilk-repl--send-client-msg (msg)
  "Encode and send client MSG to the REPL server."
  (when bilk-repl--process
    (let ((frame (bilk-protocol-encode-client msg)))
      (process-send-string bilk-repl--process frame))))

;; ---------------------------------------------------------------------------
;;; Process filter: accumulate and decode
;; ---------------------------------------------------------------------------

(defun bilk-repl--filter-data (data)
  "Accumulate DATA into the receive buffer and dispatch complete frames."
  (setq bilk-repl--receive-buffer (concat bilk-repl--receive-buffer data))
  (let ((offset 0))
    (while (bilk-protocol-frame-complete-p bilk-repl--receive-buffer offset)
      (let ((result (bilk-protocol-decode-server bilk-repl--receive-buffer offset)))
        (bilk-repl--dispatch (car result))
        (setq offset (cdr result))))
    (setq bilk-repl--receive-buffer
          (substring bilk-repl--receive-buffer offset))))

(defun bilk-repl--process-filter (_proc data)
  "Network process filter: delegate to `bilk-repl--filter-data'."
  (bilk-repl--filter-data data))

;; ---------------------------------------------------------------------------
;;; Dispatch: route decoded server messages
;; ---------------------------------------------------------------------------

(defun bilk-repl--dispatch (msg)
  "Handle a decoded server MSG."
  (pcase msg
    (`(output . ,text)
     (bilk-repl--insert-output text))
    (`(result . ,text)
     (bilk-repl--insert-result text))
    (`(error . ,text)
     (bilk-repl--insert-error text))
    (`(completions . ,lst)
     (when bilk-repl--pending-completions
       (funcall bilk-repl--pending-completions lst)
       (setq bilk-repl--pending-completions nil)))
    (`(read-request . ,_prompt)
     (bilk-repl--handle-read-request _prompt))
    (`(status . ,st)
     (setq bilk-repl--connection-status st)
     (force-mode-line-update t))
    (`(session-ok)
     (setq bilk-repl--connection-status 'ready)
     (force-mode-line-update t))
    (`(session-deny)
     (message "Bilk REPL: session denied")
     (setq bilk-repl--connection-status 'disconnected))
    (_ nil)))

;; ---------------------------------------------------------------------------
;;; REPL buffer display
;; ---------------------------------------------------------------------------

(defun bilk-repl--ensure-buffer ()
  "Ensure the REPL buffer exists and return it."
  (or (and (buffer-live-p bilk-repl--repl-buffer) bilk-repl--repl-buffer)
      (setq bilk-repl--repl-buffer
            (let ((buf (get-buffer-create "*bilk-repl*")))
              (with-current-buffer buf
                (bilk-repl-mode))
              buf))))

(defun bilk-repl--insert-output (text)
  "Insert server output TEXT into the REPL buffer."
  (with-current-buffer (bilk-repl--ensure-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))))

(defun bilk-repl--insert-result (text)
  "Insert evaluation result TEXT into the REPL buffer and echo area."
  (with-current-buffer (bilk-repl--ensure-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize text 'face 'font-lock-type-face) "\n")))
  (message "%s" text))

(defun bilk-repl--insert-error (text)
  "Insert error TEXT into the REPL buffer."
  (with-current-buffer (bilk-repl--ensure-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize text 'face 'font-lock-warning-face) "\n")))
  (message "Error: %s" text))

(defun bilk-repl--handle-read-request (prompt)
  "Handle a read-request from the server with PROMPT."
  (let ((input (read-string prompt)))
    (bilk-repl--send-client-msg (cons 'input (concat input "\n")))))

;; ---------------------------------------------------------------------------
;;; REPL mode (for the output buffer)
;; ---------------------------------------------------------------------------

(define-derived-mode bilk-repl-mode special-mode "Bilk REPL"
  "Major mode for the Bilk Scheme REPL output buffer."
  :group 'bilk
  (setq-local bilk-repl--receive-buffer "")
  (setq-local compilation-error-regexp-alist '(bilk-error))
  (setq-local compilation-error-regexp-alist-alist
              (list (list 'bilk-error bilk-error-regexp 1 2 3 2)))
  (compilation-minor-mode 1))

;; ---------------------------------------------------------------------------
;;; Interactive commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun bilk-eval-last-sexp ()
  "Evaluate the sexp before point in the Bilk REPL."
  (interactive)
  (bilk-repl--send-client-msg (cons 'eval (bilk-repl--last-sexp))))

;;;###autoload
(defun bilk-eval-defun ()
  "Evaluate the top-level form at point in the Bilk REPL."
  (interactive)
  (bilk-repl--send-client-msg (cons 'eval (bilk-repl--defun-at-point))))

;;;###autoload
(defun bilk-eval-region (start end)
  "Evaluate the region from START to END in the Bilk REPL."
  (interactive "r")
  (bilk-repl--send-client-msg
   (cons 'eval (buffer-substring-no-properties start end))))

;;;###autoload
(defun bilk-eval-buffer ()
  "Evaluate the entire buffer in the Bilk REPL."
  (interactive)
  (bilk-repl--send-client-msg
   (cons 'eval (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun bilk-load-file (file)
  "Load FILE into the Bilk REPL."
  (interactive "fLoad file: ")
  (bilk-repl--send-client-msg
   (cons 'eval (format "(load \"%s\")" (expand-file-name file)))))

;;;###autoload
(defun bilk-switch-to-repl ()
  "Switch to the Bilk REPL buffer."
  (interactive)
  (pop-to-buffer (bilk-repl--ensure-buffer)))

;;;###autoload
(defun bilk-interrupt ()
  "Send interrupt to the Bilk REPL."
  (interactive)
  (bilk-repl--send-client-msg '(interrupt)))

;; ---------------------------------------------------------------------------
;;; Completion at point
;; ---------------------------------------------------------------------------

(defun bilk-completion-at-point ()
  "Completion-at-point function for Bilk Scheme via the REPL."
  (when bilk-repl--process
    (let ((bounds (bilk-repl--completion-bounds)))
      (when bounds
        (let ((prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (list (car bounds)
                (cdr bounds)
                (completion-table-dynamic
                 (lambda (_)
                   (let ((result nil)
                         (done nil))
                     (setq bilk-repl--pending-completions
                           (lambda (completions)
                             (setq result completions
                                   done t)))
                     (bilk-repl--send-client-msg (cons 'complete prefix))
                     ;; Wait briefly for response
                     (with-timeout (1 nil)
                       (while (not done)
                         (accept-process-output bilk-repl--process 0.1)))
                     (or result nil))))))))))

;; ---------------------------------------------------------------------------
;;; Auto-reload on save
;; ---------------------------------------------------------------------------

(defun bilk-repl--buffer-library-name ()
  "Extract the library name from the current buffer if it is an .sld file.
Returns a string like \"(mylib utils)\" or nil."
  (when (and buffer-file-name
             (string-match-p "\\.sld\\'" buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "(define-library\\s-+(\\([^)]+\\))" nil t)
        (concat "(" (match-string 1) ")")))))

(defun bilk-repl--maybe-reload-on-save ()
  "Reload the current .sld library in the REPL after save, if connected."
  (when (and bilk-auto-reload
             (not (eq bilk-repl--connection-status 'disconnected)))
    (when-let ((lib (bilk-repl--buffer-library-name)))
      (bilk-reload lib))))

;; ---------------------------------------------------------------------------
;;; Comma-command wrappers
;; ---------------------------------------------------------------------------

;;;###autoload
(defun bilk-checkpoint (name)
  "Send a checkpoint command with NAME to the Bilk REPL."
  (interactive "sCheckpoint name: ")
  (bilk-repl--send-client-msg (cons 'eval (concat ",checkpoint " name))))

;;;###autoload
(defun bilk-revert (name)
  "Send a revert command with NAME to the Bilk REPL."
  (interactive "sRevert to checkpoint: ")
  (bilk-repl--send-client-msg (cons 'eval (concat ",revert " name))))

;;;###autoload
(defun bilk-reload (library)
  "Send a reload command for LIBRARY to the Bilk REPL.
LIBRARY should be in parenthesized form, e.g. \"(scheme base)\"."
  (interactive "sReload library: ")
  (bilk-repl--send-client-msg (cons 'eval (concat ",reload " library))))

;;;###autoload
(defun bilk-exports (library)
  "Send an exports command for LIBRARY to the Bilk REPL.
LIBRARY should be in parenthesized form, e.g. \"(scheme base)\"."
  (interactive "sShow exports for library: ")
  (bilk-repl--send-client-msg (cons 'eval (concat ",exports " library))))

;;;###autoload
(defun bilk-deps (library)
  "Send a deps command for LIBRARY to the Bilk REPL.
LIBRARY should be in parenthesized form, e.g. \"(scheme base)\"."
  (interactive "sShow deps for library: ")
  (bilk-repl--send-client-msg (cons 'eval (concat ",deps " library))))

;; ---------------------------------------------------------------------------
;;; Connection management
;; ---------------------------------------------------------------------------

;;;###autoload
(defun bilk-repl-start ()
  "Start `bilk serve' and connect to it."
  (interactive)
  (when (and bilk-repl--process (process-live-p bilk-repl--process))
    (user-error "Already connected to a Bilk REPL"))
  (let* ((port bilk-repl-port)
         (buf (generate-new-buffer " *bilk-serve*"))
         (proc (start-process "bilk-serve" buf
                              bilk-program "serve"
                              "--port" (number-to-string port)
                              "--insecure")))
    (setq bilk-repl--server-process proc)
    (set-process-query-on-exit-flag proc nil)
    ;; Wait for BILK CONNECT line
    (set-process-filter proc
                        (lambda (_p output)
                          (with-current-buffer buf (insert output))
                          (when (string-match "BILK CONNECT" output)
                            ;; Server is ready — connect via TCP
                            (bilk-repl--tcp-connect "127.0.0.1" port)
                            ;; Restore normal filter
                            (set-process-filter _p nil))))))

(defun bilk-repl--tcp-connect (host port)
  "Connect to the Bilk REPL at HOST:PORT."
  (let ((proc (make-network-process
               :name "bilk-repl"
               :host host
               :service port
               :coding 'binary
               :filter #'bilk-repl--process-filter
               :sentinel #'bilk-repl--sentinel)))
    (setq bilk-repl--process proc
          bilk-repl--connection-status 'ready
          bilk-repl--receive-buffer "")
    (set-process-query-on-exit-flag proc nil)
    ;; Add completion to bilk-mode buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'bilk-mode)
          (add-hook 'completion-at-point-functions
                    #'bilk-completion-at-point nil t))))
    (force-mode-line-update t)
    (message "Connected to Bilk REPL on %s:%d" host port)))

(defun bilk-repl--sentinel (_proc event)
  "Handle connection events."
  (when (string-match "\\(closed\\|broken\\|finished\\)" event)
    (setq bilk-repl--connection-status 'disconnected
          bilk-repl--process nil)
    (force-mode-line-update t)
    (message "Bilk REPL: %s" (string-trim event))))

;;;###autoload
(defun bilk-repl-disconnect ()
  "Disconnect from the Bilk REPL."
  (interactive)
  (when bilk-repl--process
    (bilk-repl--send-client-msg '(disconnect))
    (delete-process bilk-repl--process)
    (setq bilk-repl--process nil
          bilk-repl--connection-status 'disconnected)
    (force-mode-line-update t)
    (when (and bilk-repl--server-process (process-live-p bilk-repl--server-process))
      (kill-process bilk-repl--server-process))
    (message "Disconnected from Bilk REPL")))

(provide 'bilk-repl)
;;; bilk-repl.el ends here
