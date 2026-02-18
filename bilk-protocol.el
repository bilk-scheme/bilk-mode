;;; bilk-protocol.el --- Binary REPL protocol codec for Bilk Scheme  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure data module: encodes and decodes the binary frame protocol
;; defined in repl_protocol.ml.  No I/O, no side effects — only
;; unibyte string manipulation.
;;
;; Frame format:  [u32 payload-length] [u8 tag] [payload ...]
;;   payload-length includes the tag byte.
;;
;; Client message tags:
;;   0x01 Eval        0x02 Complete    0x03 Interrupt
;;   0x04 Input       0x05 Resume      0x06 Disconnect
;;   0x07 Auth-response
;;
;; Server message tags:
;;   0x81 Output      0x82 Result      0x83 Error
;;   0x84 Completions 0x85 Read-request 0x86 Status
;;   0x87 Session-ok  0x88 Session-deny
;;   0x89 Auth-challenge 0x8a Auth-ok  0x8b Auth-deny

;;; Code:

(defconst bilk-protocol-max-frame-size (* 16 1024 1024)
  "Maximum frame payload size (16 MiB), matching repl_protocol.ml.")

;; ---------------------------------------------------------------------------
;;; Wire helpers — all operate on unibyte strings
;; ---------------------------------------------------------------------------

(defun bilk-protocol--write-u32 (n)
  "Encode N as a 4-byte big-endian unibyte string."
  (unibyte-string
   (logand (ash n -24) #xff)
   (logand (ash n -16) #xff)
   (logand (ash n  -8) #xff)
   (logand n #xff)))

(defun bilk-protocol--read-u32 (data offset)
  "Read a big-endian u32 from DATA at OFFSET."
  (logior
   (ash (aref data offset) 24)
   (ash (aref data (+ offset 1)) 16)
   (ash (aref data (+ offset 2)) 8)
   (aref data (+ offset 3))))

(defun bilk-protocol--write-u16 (n)
  "Encode N as a 2-byte big-endian unibyte string."
  (unibyte-string
   (logand (ash n -8) #xff)
   (logand n #xff)))

(defun bilk-protocol--read-u16 (data offset)
  "Read a big-endian u16 from DATA at OFFSET."
  (logior
   (ash (aref data offset) 8)
   (aref data (+ offset 1))))

(defun bilk-protocol--write-bytes (s)
  "Encode string S as [u32 length][bytes]."
  (let ((raw (encode-coding-string s 'utf-8)))
    (concat (bilk-protocol--write-u32 (length raw)) raw)))

(defun bilk-protocol--read-bytes (data offset)
  "Read a length-prefixed byte string from DATA at OFFSET.
Return (STRING . NEXT-OFFSET)."
  (let* ((len (bilk-protocol--read-u32 data offset))
         (start (+ offset 4))
         (raw (substring data start (+ start len))))
    (cons (decode-coding-string raw 'utf-8) (+ start len))))

;; ---------------------------------------------------------------------------
;;; Frame construction
;; ---------------------------------------------------------------------------

(defun bilk-protocol--make-frame (tag payload)
  "Build a frame: [u32 len] [u8 TAG] PAYLOAD."
  (let ((payload-len (+ 1 (length payload))))
    (concat (bilk-protocol--write-u32 payload-len)
            (unibyte-string tag)
            payload)))

;; ---------------------------------------------------------------------------
;;; Client message encoding
;; ---------------------------------------------------------------------------

(defun bilk-protocol-encode-client (msg)
  "Encode a client MSG cons cell into a unibyte frame string.
MSG forms:
  (eval . STRING)  (complete . STRING)  (interrupt)
  (input . STRING) (resume . STRING)    (disconnect)"
  (pcase msg
    (`(eval . ,s)       (bilk-protocol--make-frame #x01 (bilk-protocol--write-bytes s)))
    (`(complete . ,s)   (bilk-protocol--make-frame #x02 (bilk-protocol--write-bytes s)))
    (`(interrupt)       (bilk-protocol--make-frame #x03 ""))
    (`(input . ,s)      (bilk-protocol--make-frame #x04 (bilk-protocol--write-bytes s)))
    (`(resume . ,s)     (bilk-protocol--make-frame #x05 (bilk-protocol--write-bytes s)))
    (`(disconnect)      (bilk-protocol--make-frame #x06 ""))
    (_ (error "bilk-protocol: unknown client message %S" msg))))

;; ---------------------------------------------------------------------------
;;; Server message encoding (for testing)
;; ---------------------------------------------------------------------------

(defun bilk-protocol-encode-server (msg)
  "Encode a server MSG into a unibyte frame string.
Used in tests to create frames the client-side decoder can parse."
  (pcase msg
    (`(output . ,s)     (bilk-protocol--make-frame #x81 (bilk-protocol--write-bytes s)))
    (`(result . ,s)     (bilk-protocol--make-frame #x82 (bilk-protocol--write-bytes s)))
    (`(error . ,s)      (bilk-protocol--make-frame #x83 (bilk-protocol--write-bytes s)))
    (`(completions . ,lst)
     (let ((payload (concat (bilk-protocol--write-u16 (length lst))
                            (mapconcat #'bilk-protocol--write-bytes lst ""))))
       (bilk-protocol--make-frame #x84 payload)))
    (`(read-request . ,s) (bilk-protocol--make-frame #x85 (bilk-protocol--write-bytes s)))
    (`(status . ready)  (bilk-protocol--make-frame #x86 (unibyte-string #x00)))
    (`(status . busy)   (bilk-protocol--make-frame #x86 (unibyte-string #x01)))
    (`(session-ok)      (bilk-protocol--make-frame #x87 ""))
    (`(session-deny)    (bilk-protocol--make-frame #x88 ""))
    (_ (error "bilk-protocol: unknown server message %S" msg))))

;; ---------------------------------------------------------------------------
;;; Frame completeness check
;; ---------------------------------------------------------------------------

(defun bilk-protocol-frame-complete-p (data offset)
  "Return non-nil if a complete frame starts at OFFSET in DATA."
  (and (>= (- (length data) offset) 4)
       (let ((payload-len (bilk-protocol--read-u32 data offset)))
         (<= (+ offset 4 payload-len) (length data)))))

;; ---------------------------------------------------------------------------
;;; Server message decoding (what the Emacs client actually uses)
;; ---------------------------------------------------------------------------

(defun bilk-protocol-decode-server (data offset)
  "Decode one server message from DATA starting at OFFSET.
Return (MSG . NEXT-OFFSET) or nil if the frame is incomplete."
  (when (bilk-protocol-frame-complete-p data offset)
    (let* ((payload-len (bilk-protocol--read-u32 data offset))
           (tag-off (+ offset 4))
           (tag (aref data tag-off))
           (body-off (+ tag-off 1))
           (next (+ offset 4 payload-len))
           (msg
            (pcase tag
              (#x81 (cons 'output (car (bilk-protocol--read-bytes data body-off))))
              (#x82 (cons 'result (car (bilk-protocol--read-bytes data body-off))))
              (#x83 (cons 'error  (car (bilk-protocol--read-bytes data body-off))))
              (#x84
               (let* ((count (bilk-protocol--read-u16 data body-off))
                      (off (+ body-off 2))
                      (lst nil))
                 (dotimes (_ count)
                   (let ((pair (bilk-protocol--read-bytes data off)))
                     (push (car pair) lst)
                     (setq off (cdr pair))))
                 (cons 'completions (nreverse lst))))
              (#x85 (cons 'read-request (car (bilk-protocol--read-bytes data body-off))))
              (#x86
               (let ((b (aref data body-off)))
                 (cons 'status (if (= b 0) 'ready 'busy))))
              (#x87 '(session-ok))
              (#x88 '(session-deny))
              (#x89 (cons 'auth-challenge (substring data body-off (+ body-off 32))))
              (#x8a (cons 'auth-ok (substring data body-off (+ body-off 32))))
              (#x8b '(auth-deny))
              (_ (error "bilk-protocol: unknown server tag 0x%02x" tag)))))
      (cons msg next))))

;; ---------------------------------------------------------------------------
;;; Client message decoding (for test round-trips)
;; ---------------------------------------------------------------------------

(defun bilk-protocol-decode-server-or-client (data offset side)
  "Decode one message from DATA at OFFSET.
SIDE is `server' or `client'; `client' decodes client-sent messages."
  (if (eq side 'server)
      (bilk-protocol-decode-server data offset)
    ;; Client message decoding (mirror of server-side read_client_msg)
    (when (bilk-protocol-frame-complete-p data offset)
      (let* ((payload-len (bilk-protocol--read-u32 data offset))
             (tag-off (+ offset 4))
             (tag (aref data tag-off))
             (body-off (+ tag-off 1))
             (next (+ offset 4 payload-len))
             (msg
              (pcase tag
                (#x01 (cons 'eval     (car (bilk-protocol--read-bytes data body-off))))
                (#x02 (cons 'complete (car (bilk-protocol--read-bytes data body-off))))
                (#x03 '(interrupt))
                (#x04 (cons 'input    (car (bilk-protocol--read-bytes data body-off))))
                (#x05 (cons 'resume   (car (bilk-protocol--read-bytes data body-off))))
                (#x06 '(disconnect))
                (_ (error "bilk-protocol: unknown client tag 0x%02x" tag)))))
        (cons msg next)))))

(provide 'bilk-protocol)
;;; bilk-protocol.el ends here
