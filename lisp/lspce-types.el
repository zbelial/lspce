;;; lspce.el --- LSP client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'lspce-core)


(cl-defun lspce--make-request (method &optional id params)
  "jsonrpc is added in the module."
  (let ((request (make-hash-table)))
    (puthash :id (or id (lspce--jsonrpc-id)) request)
    (puthash :method method request)
    (when params
      (puthash :params params request))
    request))

(cl-defun lspce--make-notification (method &optional params)
  "jsonrpc is added in the module."
  (let ((notification (make-hash-table)))
    (puthash :method method notification)
    (when params
      (puthash :params params notification))
    notification))

(cl-defun lspce--synchronizationClientCapabilities ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--completionItem ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--completionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :completionItem (lspce--completionItem) params)
    params))

(cl-defun lspce--hoverClientCapabilities ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--signatureHelpClientCapabilities()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--declarationClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--definitionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--typeDefinitionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--implementationClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(cl-defun lspce--referencesClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    params))

(cl-defun lspce--codeActionClientCapabilities ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--renameClientCapabitlities ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--publishDiagnosticsClientCapabilities ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--textDocumentClientCapabilities ()
  (let ((capabilities (make-hash-table)))
    (puthash :completion (lspce--completionClientCapabilities) capabilities)
    (puthash :declaration (lspce--declarationClientCapabilities) capabilities)
    (puthash :definition (lspce--definitionClientCapabilities) capabilities)
    (puthash :typeDefinition (lspce--typeDefinitionClientCapabilities) capabilities)
    (puthash :implementation (lspce--implementationClientCapabilities) capabilities)
    (puthash :references (lspce--referencesClientCapabilities) capabilities)
    ;; (puthash :codeAction (lspce--codeActionClientCapabilities) capabilities)
    ;; (puthash :rename (lspce--renameClientCapabitlities) capabilities)
    (puthash :publishDiagnostics (lspce--publishDiagnosticsClientCapabilities) capabilities)
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
    (puthash :textDocument (lspce--textDocumentClientCapabilities) capabilities)
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

;; (cl-defun lspce--position (&optional pos)
;;   (let ((params (make-hash-table))
;;         line character)
;;     (setq line (1- (line-number-at-pos pos t)))
;;     (setq character (progn (when pos (goto-char pos))
;;                            (funcall lspce-current-column-function)))
;;     (puthash :line line params)
;;     (puthash :character character params)
;;     params))

(cl-defun lspce--textDocumentItem (uri languageId version text)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    (puthash :languageId languageId params)
    (puthash :version version params)
    (puthash :text text params)
    params))

(cl-defun lspce--didOpen-params (textDocument)
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    params))

(cl-defun lspce--position (line character)
  (let ((params (make-hash-table)))
    (puthash :line line params)
    (puthash :character character params)
    params))

(cl-defun lspce--range (start end)
  (let ((params (make-hash-table)))
    (puthash :start start params)
    (puthash :end end params)
    params))

(cl-defun lspce--textDocumentIdenfitier (uri)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    params))

(cl-defun lspce--referenceContext ()
  (let ((params (make-hash-table)))
    (puthash :includeDeclaration t params)
    params))

(cl-defun lspce--declaration-params (textDocument position &optional context)
  "context is only used when finding references."
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :position position params)
    (when context
      (puthash :context context params))
    params))
(defalias 'lspce--definition-params 'lspce--declaration-params "lspce--definition-params")
(defalias 'lspce--references-params 'lspce--declaration-params "lspce--references-params")
(defalias 'lspce--implementation-params 'lspce--declaration-params "lspce--implementation-params")


(provide 'lspce-types)
