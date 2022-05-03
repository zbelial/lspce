;;; lspce.el --- LSP client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'lspce-core)


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

;; (cl-defun lspce--client-synchronization ()
;;   (let ((params (make-hash-table)))
;;     params))

(cl-defun lspce--client-completion ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    params))

;; (cl-defun lspce--client-hover ()
;;   (let ((params (make-hash-table)))
;;     params))

;; (cl-defun lspce--client-signatureHelp()
;;   (let ((params (make-hash-table)))
;;     params))

(cl-defun lspce--client-declaration ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--client-definition ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--client-typeDefinition ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--client-implementation ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--client-references ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    params))

(cl-defun lspce--client-codeAction ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--client-rename ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--client-publishDiagnostics ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--text-document-client-capabilities ()
  (let ((capabilities (make-hash-table)))
    ;; (puthash :completion (lspce--client-completion) capabilities)
    (puthash :declaration (lspce--client-declaration) capabilities)
    (puthash :definition (lspce--client-definition) capabilities)
    (puthash :typeDefinition (lspce--client-typeDefinition) capabilities)
    (puthash :implementation (lspce--client-implementation) capabilities)
    (puthash :references (lspce--client-references) capabilities)
    ;; (puthash :codeAction (lspce--client-codeAction) capabilities)
    ;; (puthash :rename (lspce--client-rename) capabilities)
    ;; (puthash :publishDiagnostics (lspce--client-publishDiagnostics) capabilities)
    capabilities))

(cl-defun lspce--client-info ()
  (let ((params (make-hash-table)))
    (puthash :name LSPCE-NAME params)
    (puthash :version LSPCE-VERSION params)
    params))

(cl-defun lspce--file-operations ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--workspace ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--window ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--client-capabilities ()
  (let ((capabilities (make-hash-table)))
    ;; (puthash :workspace (lspce--workspace) capabilities)    
    (puthash :textDocument (lspce--text-document-client-capabilities) capabilities)
    ;; (puthash :window (lspce--window) capabilities)
    capabilities))

(cl-defun lspce--initialize-params (rootUri capabilities &optional initializationOptions clientInfo locale rootPath trace workspaceFolders)
  "初始化参数"
  (let ((params (make-hash-table)))
    (puthash :processId (emacs-pid) params)
    (puthash :rootUri rootUri params)
    (puthash :capabilities capabilities params)
    (when initializationOptions
      (puthash :initializationOptions initializationOptions params))
    (when clientInfo
      (puthash :clientInfo clientInfo params))
    (when locale
      (puthash :locale locale params))
    (when rootPath
      (puthash :rootPath rootPath params))
    (when trace
      (puthash :trace trace params))
    (when workspaceFolders
      (puthash :workspaceFolders workspaceFolders params))
    
    params))

(provide 'lspce-types)
