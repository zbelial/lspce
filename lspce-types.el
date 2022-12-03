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

(defun lspce--documentationFormat ()
  (let (format)
    (setq format (vector "plaintext"))
    (when (fboundp 'markdown-mode)
      (setq format (vector "markdown" "plaintext")))
    format))

(defun lspce--clientInfo ()
  (let ((params (make-hash-table)))
    (puthash :name LSPCE-NAME params)
    (puthash :version LSPCE-VERSION params)
    params))

(defun lspce--clientCapabilities ()
  (list
   :workspace (list
               :applyEdit :json-false
               :executeCommand (list
                                :dynamicRegistration :json-false)
               :workspaceEdit (list
                               :documentChanges t
                               :resourceOperations (vector "create" "rename" "delete"))
               :symbol (list
                        :dynamicRegistration :json-false)
               :configuration :json-false
               :workspaceFolders :json-false)
   :textDocument (list
                  :synchronization (list
                                    :dynamicRegistration :json-false
                                    :willSave t
                                    :didSave t)
                  :completion      (list
                                    :dynamicRegistration :json-false
                                    :completionItem (list
                                                     :snippetSupport (if (lspce--snippet-expansion-fn)
                                                                         t
                                                                       :json-false)
                                                     :commitCharactersSupport :json-false
                                                     :documentationFormat (if (fboundp 'gfm-view-mode)
                                                                              ["markdown" "plaintext"]
                                                                            ["plaintext"])
                                                     :deprecatedSupport :json-false
                                                     :preselectSupoort :json-false
                                                     :insertReplaceSupport :json-false
                                                     :resolveSupport (list :properties (vector "documentation" "detail" "additionalTextEdits"))
                                                     )
                                    :contextSupport t)
                  :hover              (list
                                       :dynamicRegistration :json-false
                                       :contentFormat (if (fboundp 'gfm-view-mode)
                                                          ["markdown" "plaintext"]
                                                        ["plaintext"]))
                  :signatureHelp      (list :dynamicRegistration :json-false
                                            :signatureInformation (list
                                                                   :documentationFormat (if (fboundp 'gfm-view-mode)
                                                                                            ["markdown" "plaintext"]
                                                                                          ["plaintext"])
                                                                   :parameterInformation (list
                                                                                          :labelOffsetSupport :json-false)
                                                                   :activeParameterSupport :json-false)
                                            :contextSupport :json-false)
                  :references         (list
                                       :dynamicRegistration :json-false)
                  :definition         (list
                                       :dynamicRegistration :json-false
                                       :linkSupport :json-false)
                  :declaration        (list
                                       :dynamicRegistration :json-false
                                       :linkSupport :json-false)
                  :implementation     (list
                                       :dynamicRegistration :json-false
                                       :linkSupport :json-false)
                  :typeDefinition     (list
                                       :dynamicRegistration :json-false
                                       :linkSupport :json-false)
                  :documentHighlight  (list
                                       :dynamicRegistration :json-false)
                  :codeAction         (list
                                       :dynamicRegistration :json-false
                                       :codeActionLiteralSupport (list
                                                                  :codeActionKind (list :valueSet
                                                                                        ["quickfix"
                                                                                         "refactor" "refactor.extract"
                                                                                         "refactor.inline" "refactor.rewrite"
                                                                                         "source" "source.organizeImports"]))
                                       :isPreferredSupport t
                                       :disabledSupport :json-false
                                       :dataSupport :json-false)
                  :formatting         (list
                                       :dynamicRegistration :json-false)
                  :rangeFormatting    (list
                                       :dynamicRegistration :json-false)
                  :rename             (list
                                       :dynamicRegistration :json-false
                                       :prepareSupport :json-false
                                       :honorsChangeAnnotations :json-false)
                  :publishDiagnostics (list
                                       :relatedInformation :json-false
                                       :codeDescriptionSupport :json-false
                                       :codeDescriptionSupport :json-false
                                       :dataSupport :json-false
                                       :versionSupport :json-false
                                       :tagSupport (list :valueSet [1 2]))
                  :callHierarchy (list
                                  :dynamicRegistration :json-false))
   :experimental lspce--{})
  )

(defun lspce--initializeParams (rootUri &optional initializationOptions trace workspaceFolders)
  "初始化参数"
  (let ((params (make-hash-table)))
    (puthash :processId (emacs-pid) params)
    (puthash :rootUri rootUri params)
    (puthash :capabilities (lspce--clientCapabilities) params)
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

(defun lspce--renameParams (textDocument position newName)
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :position position params)
    (puthash :newName newName params)
    params))

(provide 'lspce-types)
