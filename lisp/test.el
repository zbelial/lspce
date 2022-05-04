;;; lspce.el --- LSP client for Emacs -*- lexical-binding: t; -*-


(add-to-list 'load-path "/mnt/Personal/VCS/bitbucket/lspce/rs/target/debug")
(add-to-list 'load-path "/mnt/Personal/VCS/bitbucket/lspce/lisp")

(require 'lspce-module)
(require 'lspce)

(defun lspce--initialize-request () (list 
                                     :id (lspce--jsonrpc-id)
                                     :method "initialize"
                                     :params (lspce--initialize-params (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs")
                                                                       (lspce--client-capabilities)
                                                                       nil
                                                                       (lspce--client-info))))

(defun lspce--shutdown-request () (list
                                   :id (lspce--jsonrpc-id)
                                   :method "shutdown"))

(defun lspce--file-content (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(lspce-module-server-running "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust")
(lspce-module-server-running "/home/lucency/lsp-bridge" "python")

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" "rust-analyzer" "--log-file /tmp/ra.log -v" (json-encode (lspce--initialize-request)))
;; (lspce-module-shutdown "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--shutdown-request)))

;; (lspce-module-notify "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--notification "textDocument/didOpen" (lspce--didOpen-params (lspce--textDocumentItem (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs") "rust" 1 (lspce--file-content "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs"))))))

;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--request "textDocument/declaration" nil (lspce--declaration-params (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19)))))
;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--request "textDocument/definition" nil (lspce--definition-params (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19)))))
;; (message "%S" (json-parse-string (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--request "textDocument/definition" nil (lspce--definition-params (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19)))))))
;; (lspce-module-request "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" (json-encode (lspce--request "textDocument/references" nil (lspce--references-params (lspce--textDocumentIdenfitier (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs/src/lib.rs")) (lspce--position 127 19) (lspce--referenceContext)))))

;; (lspce-module-connect "/home/lucency/lsp-bridge" "python" "pyright-langserver" "--stdio" (json-encode (lspce--initialize-request)))

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" "rust-analyzer" "proc-macro" (json-encode (lspce--initialize-request)))

