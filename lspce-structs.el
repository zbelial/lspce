;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)

(cl-defstruct documentChange
  (kind) ;; change, documentChange, create, rename or delete
  (change) ;; 
  )

(cl-defstruct lsp-response
  (request-id)
  (response-data)
  (latest-request-id)
  )

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

(defun lspce--event (event-type event-data)
  (list :event_type event-type :event_data event-data))

(defun lspce--event-start-server (project-root language-id server-name cmd)
  (list :event_type event-type
        :event_data (list :project_root project-root
                          :language_id language-id
                          :server_name server-name
                          :cmd cmd)))

(provide 'lspce-structs)
