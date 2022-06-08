;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'lspce-util)

(cl-defun lspce--make-request (method &optional params)
  "jsonrpc is added in the module."
  (let ((request (make-hash-table)))
    (puthash :id (lspce--next-jsonrpc-id) request)
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

(cl-defun lspce--documentationFormat ()
  (let (format)
    (setq format (vector "plaintext"))
    (when (fboundp 'markdown-mode)
      (setq format (vector "markdown" "plaintext")))
    format))

(cl-defun lspce--resolveSupport ()
  (let ((params (make-hash-table)))
    (puthash :properties (vector "documentation" "detail") params)
    params))

(cl-defun lspce--completionItem ()
  (let ((params (make-hash-table)))
    ;; (puthash :snippetSupport :json-false params)
    (puthash :snippetSupport t params)
    (puthash :commitCharactersSupport :json-false params)
    (puthash :documentationFormat (lspce--documentationFormat) params)
    (puthash :deprecatedSupport :json-false params)
    (puthash :contextSupport t params)
    ;; TODO https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion
    (puthash :preselectSupoort :json-false params)
    (puthash :insertReplaceSupport :json-false params)
    (puthash :resolveSupport (lspce--resolveSupport) params)
    params))

(cl-defun lspce--completionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :completionItem (lspce--completionItem) params)
    params))

(cl-defun lspce--hoverClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :contentFormat (lspce--documentationFormat) params)
    params))

(cl-defun lspce--signatureHelpClientCapabilities()
  (let ((params (make-hash-table))
        (signatureInformation (make-hash-table))
        (parameterInformation (make-hash-table)))
    (puthash :labelOffsetSupport :json-false parameterInformation)

    (puthash :documentationFormat (lspce--documentationFormat) signatureInformation)
    (puthash :parameterInformation parameterInformation signatureInformation)
    (puthash :activeParameterSupport :json-false signatureInformation)

    (puthash :dynamicRegistration :json-false params)
    (puthash :signatureInformation signatureInformation params)
    (puthash :contextSupport :json-false params)
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
    (puthash :relatedInformation :json-false params)
    (puthash :versionSupport :json-false params)
    (puthash :codeDescriptionSupport :json-false params)
    (puthash :dataSupport :json-false params)
    (puthash :tagSupport (list :valueSet (vector 1 2)) params)
    params))

(cl-defun lspce--textDocumentSyncClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :didSave :json-false params)
    params))

(cl-defun lspce--textDocumentClientCapabilities ()
  (let ((capabilities (make-hash-table)))
    (puthash :synchronization (lspce--textDocumentSyncClientCapabilities) capabilities)
    (puthash :completion (lspce--completionClientCapabilities) capabilities)
    (puthash :hover (lspce--hoverClientCapabilities) capabilities)
    (puthash :signatureHelp (lspce--signatureHelpClientCapabilities) capabilities)
    (puthash :declaration (lspce--declarationClientCapabilities) capabilities)
    (puthash :definition (lspce--definitionClientCapabilities) capabilities)
    (puthash :typeDefinition (lspce--typeDefinitionClientCapabilities) capabilities)
    (puthash :implementation (lspce--implementationClientCapabilities) capabilities)
    (puthash :references (lspce--referencesClientCapabilities) capabilities)
    ;; (puthash :codeAction (lspce--codeActionClientCapabilities) capabilities)
    ;; (puthash :rename (lspce--renameClientCapabitlities) capabilities)
    (puthash :publishDiagnostics (lspce--publishDiagnosticsClientCapabilities) capabilities)
    capabilities))

(cl-defun lspce--clientInfo ()
  (let ((params (make-hash-table)))
    (puthash :name LSPCE-NAME params)
    (puthash :version LSPCE-VERSION params)
    params))

(cl-defun lspce--fileOperations ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--workspace ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--window ()
  (let ((params (make-hash-table)))
    params))

(cl-defun lspce--clientCapabilities ()
  (let ((capabilities (make-hash-table)))
    ;; (puthash :workspace (lspce--workspace) capabilities)    
    (puthash :textDocument (lspce--textDocumentClientCapabilities) capabilities)
    ;; (puthash :window (lspce--window) capabilities)
    capabilities))

(cl-defun lspce--initializeParams (rootUri capabilities &optional initializationOptions trace workspaceFolders)
  "初始化参数"
  (let ((params (make-hash-table)))
    (puthash :processId (emacs-pid) params)
    (puthash :rootUri rootUri params)
    (puthash :capabilities capabilities params)
    (when initializationOptions
      (puthash :initializationOptions initializationOptions params))
    (puthash :clientInfo (lspce--clientInfo) params)
    (when trace
      (puthash :trace trace params))
    (when workspaceFolders
      (puthash :workspaceFolders workspaceFolders params))
    
    params))

(cl-defun lspce--textDocumentItem (uri languageId version text)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    (puthash :languageId languageId params)
    (puthash :version version params)
    (puthash :text text params)
    params))

(cl-defun lspce--versionedTextDocumentIdenfitier (uri version)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    (puthash :version version params)
    params))


(cl-defun lspce--didOpenTextDocumentParams (textDocument)
  "For didOpen, textDocument is a TextDocumentItem, but for didClose, it's TextDocumentIdentifier."
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    params))
(defalias 'lspce--didCloseTextDocumentParams 'lspce--didOpenTextDocumentParams "lspce--didCloseTextDocumentParams")

(cl-defun lspce--textDocumentContentChangeEvent (range rangeLength text)
  (let ((params (make-hash-table)))
    (puthash :range range params)
    (puthash :rangeLength rangeLength params)
    (puthash :text text params)
    params))

(cl-defun lspce--didChangeTextDocumentParams (textDocument contentChanges)
  "textDocument is a VersionedTextDocumentIdentifier."
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :contentChanges contentChanges params)
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

(cl-defun lspce--textDocumentPositionParams (textDocument position &optional context)
  "context is only used when finding references and completion."
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :position position params)
    (when context
      (puthash :context context params))
    params))

(defalias 'lspce--declarationParams 'lspce--textDocumentPositionParams "lspce--definitionParams")
(defalias 'lspce--definitionParams 'lspce--textDocumentPositionParams "lspce--definitionParams")
(defalias 'lspce--referencesParams 'lspce--textDocumentPositionParams "lspce--referencesParams")
(defalias 'lspce--implementationParams 'lspce--textDocumentPositionParams "lspce--implementationParams")
(defalias 'lspce--hoverParams 'lspce--textDocumentPositionParams "lspce--hoverParams")

(defconst LSPCE-Invoked 1 "Completion was triggered by typing an identifier or via API")
(defconst LSPCE-TriggerCharacter 2 "Completion was triggered by a trigger character")
(defconst LSPCE-TriggerForIncompleteCompletions 3 "Completion was re-triggered as the current completion list is incomplete")
(cl-defun lspce--completionContext (trigger-kind trigger-character)
  (let ((params (make-hash-table)))
    (when trigger-kind
      (puthash :triggerKind trigger-kind params))
    (when trigger-character
      (puthash :triggerCharacter trigger-character params))
    params))
(defalias 'lspce--completionParams 'lspce--textDocumentPositionParams "lspce--definitionParams")

(provide 'lspce-types)
