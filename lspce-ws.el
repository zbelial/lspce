;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

;;; Require
(eval-when-compile
  (require 'cl-lib))
(require 'json)
(require 'websocket)

(defcustom lspce-ws-server-port 38456
  "Websocket server port."
  :group 'lspce
  :type 'integer)

(cl-defstruct lspce--server-key
  (project-root)
  (language-id)
  (server-name))

(defun lspce--server-key-test-fn (k1 k2)
  (and (string-equal (lspce--server-key-project-root k1)
                     (lspce--server-key-project-root k2))
       (string-equal (lspce--server-key-language-id k1)
                     (lspce--server-key-language-id k2))
       (string-equal (lspce--server-key-server-name k1)
                     (lspce--server-key-server-name k2))))

(defun lspce--server-key-hash-fn (k)
  (let ((project-root (lspce--server-key-project-root k))
        (language-id (lspce--server-key-language-id k))
        (server-name (lspce--server-key-server-name k))
        (hash 0))
    (seq-do (lambda (c)
              (setq hash (+ (* 31 hash) c))
              (setq hash (% hash (max-char))))
            (concat project-root language-id server-name))
    hash))

(define-hash-table-test 'lspce--server-key-equal 'lspce--server-key-test-fn 'lspce--server-key-hash-fn)

(defvar lspce--ws-clients (make-hash-table :test 'lspce--server-key-equal))
;; (setq lspce--ws-clients (make-hash-table :test 'lspce--server-key-equal))

;; (defun lspce--ws-handler (ws frame)
;;   (let ((msg (websocket-frame-text frame))
;;         (server-url (websocket-url ws))
;;         (client-key (websocket-client-data ws)))
;;     (message "received data %s from %s" msg server-url)
;;     ;; TODO lspce--ws-handler
;;     )
;;   )

;; (defun lspce--ws-connect (project-root language-id server-name ws-port)
;;   (let ((ws-client-key (make-lspce--server-key :project-root project-root
;;                                                :language-id language-id
;;                                                :server-name server-name))
;;         (server-url (format "ws://127.0.0.1:%d" ws-port))
;;         ws-client)
;;     (setq ws-client (websocket-open server-url
;;                                     :on-open (lambda (ws)
;;                                                (message "connected to %s" server-url)
;;                                                (setf (websocket-client-data ws) ws-client-key)
;;                                                (puthash ws-client-key ws lspce--ws-clients))
;;                                     :on-close (lambda (ws)
;;                                                 (message "disconnecting from %s" server-url)
;;                                                 (let ((key (websocket-client-data ws)))
;;                                                   (when key
;;                                                     (remhash key lspce--ws-clients))))
;;                                     :on-message #'lspce--ws-handler))
;;     ws-client))

;; (defun lspce--ws-close (ws-client)
;;   (unless (eq (websocket-ready-state 'closed))
;;     (websocket-close ws-client)))

;; ;; (setq ws-client (lspce--ws-connect "/mnt/Personal/Sources/lspce" "python" "pylsp" lspce-ws-server-port))
;; ;; (websocket-send-text ws-client (json-encode (list :event_type -1 :event_data "test")))
;; ;; (websocket-send-text ws-client (json-encode (list :event_type 7 :event_data "test")))
