;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

;;; Require
(eval-when-compile
  (require 'cl-lib))
(require 'json)
(require 'websocket)
(require 'lspce-structs)

(defcustom lspce-ws-server-port 38456
  "Websocket server port."
  :group 'lspce
  :type 'integer)

(defun lspce--ws-handler (ws frame)
  (let ((msg (websocket-frame-text frame))
        (server-url (websocket-url ws))
        (client-key (websocket-client-data ws)))
    (message "received data %s from %s" msg server-url)
    ;; TODO lspce--ws-handler
    )
  )

(defun lspce--ws-connect (project-root language-id server-name ws-port)
  (let ((ws-client-key (make-lspce--server-key :project-root project-root
                                               :language-id language-id
                                               :server-name server-name))
        (server-url (format "ws://127.0.0.1:%d" ws-port))
        ws-client)
    (setq ws-client (websocket-open server-url
                                    :on-open (lambda (ws)
                                               (message "connected to %s" server-url)
                                               (setf (websocket-client-data ws) ws-client-key)
                                               (puthash ws-client-key ws lspce--ws-clients)
                                               )
                                    :on-close (lambda (ws)
                                                (message "disconnecting from %s" server-url)
                                                (let ((key (websocket-client-data ws)))
                                                  (when key
                                                    (remhash key lspce--ws-clients))))
                                    :on-message #'lspce--ws-handler))
    ws-client))

;; (defun lspce--ws-close (ws-client)
;;   (unless (eq (websocket-ready-state 'closed))
;;     (websocket-close ws-client)))

;; (setq ws-client (lspce--ws-connect "/mnt/Personal/Sources/lspce" "python" "pylsp" lspce-ws-server-port))
;; ;; (websocket-send-text ws-client (json-encode (list :event_type -1 :event_data "test")))
;; ;; (websocket-send-text ws-client (json-encode (list :event_type 7 :event_data "test")))
(let ((count 10000)
      (ws-client (lspce--ws-connect "/mnt/Personal/Sources/lspce" "python" "pylsp" 8765))
      (content (make-string 2000 ?t))
      (idx 0))
  (message "start time: %s" (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))
  (while (< idx count)
    (websocket-send-text ws-client content)
    (setq idx (1+ idx)))
  (message "end time: %s" (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))
  (websocket-close ws-client)
  nil)

(let ((server (websocket-server 8765
                                :host 'local
                                :on-message (lambda (ws frame) (message "%s" frame))
                                :on-close (lambda (ws))))))

(setq ws-server (websocket-server 8765
                                  :host 'local
                                  :on-message (lambda (ws frame)
                                                ;; (message "%s" frame)
                                                )
                                  :on-close (lambda (ws))))
(websocket-server-close ws-server)
