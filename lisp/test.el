;;; lspce.el --- LSP client for Emacs -*- lexical-binding: t; -*-


(add-to-list 'load-path "/mnt/Personal/VCS/bitbucket/lspce/rs/target/debug")

(require 'lspce-module)

(defun lspce--path-to-uri (path)
  "URIfy PATH."
  (url-hexify-string
   (concat "file://" (if (eq system-type 'windows-nt) "/") (file-truename path))
   url-path-allowed-chars))
(defvar lspce--jsonrpc-id 0)

(defun lspce--jsonrpc-id ()
  (let ((id lspce--jsonrpc-id))
    (setq lspce--jsonrpc-id (1+ lspce--jsonrpc-id))
    id))

(setq lspce--initialize-params (list :processId (emacs-pid)
                                     :rootUri (lspce--path-to-uri "/mnt/Personal/VCS/bitbucket/lspce/rs")
                                     :initializationOptions (make-hash-table)
                                     :capabilities (make-hash-table)
                                     ))
(setq lspce--initialize-request (list 
                                 :id (lspce--jsonrpc-id)
                                 :method "initialize"
                                 :params lspce--initialize-params))

(lspce-module-server-running "/mnt/Personal/VCS/bitbucket/lspce" "rust")
(lspce-module-server-running "/mnt/Personal/VCS/bitbucket/lspce" "python")

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" "rust-analyzer" "--log-file /tmp/ra.log -v" (json-encode lspce--initialize-request))
;; (lspce-module-connect "/home/lucency/lsp-bridge" "python" "pyright-langserver" "--stdio" (json-encode lspce--initialize-request))

;; (lspce-module-connect "/mnt/Personal/VCS/bitbucket/lspce/rs" "rust" "rust-analyzer" "proc-macro" (json-encode lspce--initialize-request))

