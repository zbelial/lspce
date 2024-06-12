;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)

(cl-defstruct lspce-documentChange
  (kind) ;; change, documentChange, create, rename or delete
  (change) ;; 
  )

(cl-defstruct lspce-completionCache
  (prefix-start)
  (candidates)
  (lsp-items)
  (markers)
  (prefix))

(provide 'lspce-structs)
