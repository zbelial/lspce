;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)

(cl-defstruct documentChange
  (kind) ;; change, documentChange, create, rename or delete
  (change) ;; 
  )

(provide 'lspce-structs)
