;;; lspce.el --- LSP client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)


(cl-defun lspce--request (id method &optional params)
  "jsonrpc is added in the module."
  (let ((request (make-hash-table)))
    (puthash :id id request)
    (puthash :method method request)
    (when params
      (puthash :params params request))
    request))

(cl-defun lspce--notification (method &optional params)
  "jsonrpc is added in the module."
  (let ((notification (make-hash-table)))
    (puthash :method method notification)
    (when params
      (puthash :params params notification))
    notification))

(cl-defun lspce--client-capabilities ()
  )

(cl-defun lspce--initialize-params (processId rootUri capabilities &optional clientInfo locale rootPath trace workspaceFolder)
  "初始化参数"
  )

(provide 'lspce-types)
