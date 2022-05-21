;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-


(add-to-list 'load-path "/mnt/Personal/VCS/bitbucket/lspce/rs/target/debug")
(add-to-list 'load-path "/mnt/Personal/VCS/bitbucket/lspce/lisp")

(require 'lspce-module)
(require 'lspce)

(defun lspce--initialize-request () (list 
                                     :id (lspce--jsonrpc-id)
                                     :method "initialize"
                                     :params (lspce--initializeParams (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce")
                                                                      (lspce--clientCapabilities)
                                                                      nil
                                                                      (lspce--clientInfo))))

(defun lspce--shutdown-request () (list
                                   :id (lspce--jsonrpc-id)
                                   :method "shutdown"))

(defun lspce--file-content (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(lspce-module-server "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust-mode")
(lspce-module-server "/home/lucency/lsp-bridge" "python-mode")

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" "rust-analyzer" "--log-file /tmp/ra.log -v" (json-encode (lspce--initialize-request)))
;; (lspce-module-shutdown "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--shutdown-request)))

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce" "rust-mode" "rust-analyzer" "--log-file /tmp/ra.log -v" (json-encode (lspce--initialize-request)))
;; (lspce-module-shutdown "/mnt/Personal/VCS/bitbucket/lspce" "rust-mode" (json-encode (lspce--shutdown-request)))

;; (lspce-module-notify "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-notification "textDocument/didOpen" (lspce--didOpenTextDocumentParams (lspce--textDocumentItem (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs") "rust" 1 (lspce--file-content "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs"))))))

;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-request "textDocument/declaration" nil (lspce--declarationParams (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19)))))
;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-request "textDocument/definition" nil (lspce--definitionParams (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19)))))
;; (message "%S" (json-parse-string (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-request "textDocument/definition" nil (lspce--definitionParams (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19)))))))
;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-request "textDocument/references" nil (lspce--referencesParams (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19) (lspce--referenceContext)))))

;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-request "textDocument/completion" nil (lspce--completionParams (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19) (lspce--completionContext)))))
;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--make-request "textDocument/completion" nil (lspce--completionParams (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 146 27) (lspce--completionContext)))))

;; (lspce-module-connect "/home/lucency/lsp-bridge" "python" "pyright-langserver" "--stdio" (json-encode (lspce--initialize-request)))

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" "rust-analyzer" "proc-macro" (json-encode (lspce--initialize-request)))

