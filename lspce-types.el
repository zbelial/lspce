;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'lspce-util)

(defun lspce--make-request (method &optional params)
  "jsonrpc is added in the module."
  (let ((request (make-hash-table)))
    (puthash :id (lspce--next-jsonrpc-id) request)
    (puthash :method method request)
    (when params
      (puthash :params params request))
    request))

(defun lspce--make-notification (method &optional params)
  "jsonrpc is added in the module."
  (let ((notification (make-hash-table)))
    (puthash :method method notification)
    (when params
      (puthash :params params notification))
    notification))

(defun lspce--synchronizationClientCapabilities ()
  (let ((params (make-hash-table)))
    params))

(defun lspce--documentationFormat ()
  (let (format)
    (setq format (vector "plaintext"))
    (when (fboundp 'markdown-mode)
      (setq format (vector "markdown" "plaintext")))
    format))

(defun lspce--resolveSupport ()
  (let ((params (make-hash-table)))
    (puthash :properties (vector "documentation" "detail") params)
    params))

(defun lspce--completionItem ()
  (let ((params (make-hash-table)))
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

(defun lspce--completionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :completionItem (lspce--completionItem) params)
    params))

(defun lspce--hoverClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :contentFormat (lspce--documentationFormat) params)
    params))

(defun lspce--signatureHelpClientCapabilities()
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

(defun lspce--declarationClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(defun lspce--definitionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(defun lspce--typeDefinitionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(defun lspce--implementationClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :linkSupport :json-false params)
    params))

(defun lspce--referencesClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    params))

(defun lspce--codeActionClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :dynamicRegistration :json-false params)
    (puthash :codeActionLiteralSupport (list :codeActionKind
                                             (list :valueSet ["quickfix"
                                                              "refactor" "refactor.extract"
                                                              "refactor.inline" "refactor.rewrite"
                                                              "source" "source.organizeImports"]))
             params)
    (puthash :isPreferredSupport t params)
    (puthash :disabledSupport :json-false params)
    (puthash :dataSupport :json-false params)
    params))

(defun lspce--renameClientCapabitlities ()
  (let ((params (make-hash-table)))
    params))

(defun lspce--publishDiagnosticsClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :relatedInformation :json-false params)
    (puthash :versionSupport :json-false params)
    (puthash :codeDescriptionSupport :json-false params)
    (puthash :dataSupport :json-false params)
    (puthash :tagSupport (list :valueSet (vector 1 2)) params)
    params))

(defun lspce--textDocumentSyncClientCapabilities ()
  (let ((params (make-hash-table)))
    (puthash :didSave :json-false params)
    params))

(defun lspce--textDocumentClientCapabilities ()
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
    (puthash :codeAction (lspce--codeActionClientCapabilities) capabilities)
    ;; (puthash :rename (lspce--renameClientCapabitlities) capabilities)
    (puthash :publishDiagnostics (lspce--publishDiagnosticsClientCapabilities) capabilities)
    capabilities))

(defun lspce--clientInfo ()
  (let ((params (make-hash-table)))
    (puthash :name LSPCE-NAME params)
    (puthash :version LSPCE-VERSION params)
    params))

(defun lspce--fileOperations ()
  (let ((params (make-hash-table)))
    params))

(defun lspce--workspace ()
  (let ((params (make-hash-table)))
    (puthash :applyEdit :json-false params)
    (puthash :workspaceEdit (list :documentChanges t) params)
    (puthash :workspaceFolders :json-false params)
    (puthash :configuration :json-false params)
    params))

(defun lspce--window ()
  (let ((params (make-hash-table)))
    params))

(defun lspce--clientCapabilities ()
  (let ((capabilities (make-hash-table)))
    (puthash :workspace (lspce--workspace) capabilities)    
    (puthash :textDocument (lspce--textDocumentClientCapabilities) capabilities)
    ;; (puthash :window (lspce--window) capabilities)
    capabilities))

(defun lspce--initializeParams (rootUri capabilities &optional initializationOptions trace workspaceFolders)
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

(defun lspce--textDocumentItem (uri languageId version text)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    (puthash :languageId languageId params)
    (puthash :version version params)
    (puthash :text text params)
    params))

(defun lspce--versionedTextDocumentIdenfitier (uri version)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    (puthash :version version params)
    params))


(defun lspce--didOpenTextDocumentParams (textDocument)
  "For didOpen, textDocument is a TextDocumentItem, but for didClose, it's TextDocumentIdentifier."
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    params))
(defalias 'lspce--didCloseTextDocumentParams 'lspce--didOpenTextDocumentParams "lspce--didCloseTextDocumentParams")

(defun lspce--textDocumentContentChangeEvent (range rangeLength text)
  (let ((params (make-hash-table)))
    (puthash :range range params)
    (puthash :rangeLength rangeLength params)
    (puthash :text text params)
    params))

(defun lspce--didChangeTextDocumentParams (textDocument contentChanges)
  "textDocument is a VersionedTextDocumentIdentifier."
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :contentChanges contentChanges params)
    params))

(defun lspce--position (line character)
  (let ((params (make-hash-table)))
    (puthash :line line params)
    (puthash :character character params)
    params))

(defun lspce--range (start end)
  (let ((params (make-hash-table)))
    (puthash :start start params)
    (puthash :end end params)
    params))

(defun lspce--textDocumentIdenfitier (uri)
  (let ((params (make-hash-table)))
    (puthash :uri uri params)
    params))

(defun lspce--referenceContext ()
  (let ((params (make-hash-table)))
    (puthash :includeDeclaration t params)
    params))

(defun lspce--textDocumentPositionParams (textDocument position &optional context)
  "context is only used for references, signature help and completion."
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
(defalias 'lspce--signatureHelpParams 'lspce--textDocumentPositionParams "lspce--signatureHelpParams")

(defconst LSPCE-Invoked 1 "Completion was triggered by typing an identifier or via API")
(defconst LSPCE-TriggerCharacter 2 "Completion was triggered by a trigger character")
(defconst LSPCE-TriggerForIncompleteCompletions 3 "Completion was re-triggered as the current completion list is incomplete")
(defun lspce--completionContext (trigger-kind trigger-character)
  (let ((params (make-hash-table)))
    (when trigger-kind
      (puthash :triggerKind trigger-kind params))
    (when trigger-character
      (puthash :triggerCharacter trigger-character params))
    params))
(defalias 'lspce--completionParams 'lspce--textDocumentPositionParams "lspce--definitionParams")

(defun lspce--codeActionContext (diagnostics &optional only)
  (let ((params (make-hash-table)))
    (puthash :diagnostics diagnostics params)
    (when only
      (puthash :only only params))
    params))

(defun lspce--codeActionParams (textDocument range context)
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :range range params)
    (puthash :context context params)
    params))

(provide 'lspce-types)
