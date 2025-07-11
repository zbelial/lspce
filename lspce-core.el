;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-


;;; Require
(require 'json)
(eval-when-compile
  (require 'cl-lib))
(require 'project)
(require 'url-util)
(require 'compile)                      ; for some faces
(require 'warnings)
(require 'markdown-mode)
(require 'yasnippet)
(require 'flymake)

(require 'lspce-module)
(eval-and-compile
  (require 'lspce-util))
(require 'lspce-snippet)


;;; User tweakable stuff
(defgroup lspce nil
  "Interaction with Language Server Protocol servers"
  :prefix "lspce-"
  :group 'applications)

(defcustom lspce-connect-server-timeout 60
  "The timeout of connecting to lsp server, in seconds."
  :group 'lspce
  :type 'integer)

(defcustom lspce-modes-enable-single-file-root '(python-mode python-ts-mode)
  "Major modes where lspce enables even for a single file (IOW no project)."
  :group 'lspce
  :type 'list)

(defcustom lspce-inherit-exec-path nil
  "If non-nil, pass `exec-path' as PATH to rust code to
create the lsp server subprocesses."
  :group 'lspce
  :type 'boolean)

;;; Constants
;;;
(defconst lspce--symbol-kind-names
  `((1 . "File") (2 . "Module")
    (3 . "Namespace") (4 . "Package") (5 . "Class")
    (6 . "Method") (7 . "Property") (8 . "Field")
    (9 . "Constructor") (10 . "Enum") (11 . "Interface")
    (12 . "Function") (13 . "Variable") (14 . "Constant")
    (15 . "String") (16 . "Number") (17 . "Boolean")
    (18 . "Array") (19 . "Object") (20 . "Key")
    (21 . "Null") (22 . "EnumMember") (23 . "Struct")
    (24 . "Event") (25 . "Operator") (26 . "TypeParameter")))

(defconst lspce--kind-names
  `((1 . "Text") (2 . "Method") (3 . "Function") (4 . "Constructor")
    (5 . "Field") (6 . "Variable") (7 . "Class") (8 . "Interface")
    (9 . "Module") (10 . "Property") (11 . "Unit") (12 . "Value")
    (13 . "Enum") (14 . "Keyword") (15 . "Snippet") (16 . "Color")
    (17 . "File") (18 . "Reference") (19 . "Folder") (20 . "EnumMember")
    (21 . "Constant") (22 . "Struct") (23 . "Event") (24 . "Operator")
    (25 . "TypeParameter")))


;;; Variables and custom
(defvar lspce-envs-pass-to-subprocess '()
  "Environment variables that should be passed to rust to start lsp server
subprocesses. Should be a list of string, e.g. '(\"PATH\")")

(defvar lspce-current-column-function #'lspce-current-column
  "Function to calculate the current column.

This is the inverse operation of
`lspce-move-to-column-function' (which see).  It is a function of
no arguments returning a column number.  For buffers managed by
fully LSP-compliant servers, this should be set to
`lspce-lsp-abiding-column', and `lspce-current-column' (the default)
for all others.")

(defvar lspce-move-to-column-function #'lspce-move-to-column
  "Function to move to a column reported by the LSP server.

According to the standard, LSP column/character offsets are based
on a count of UTF-16 code units, not actual visual columns.  So
when LSP says position 3 of a line containing just \"aXbc\",
where X is a multi-byte character, it actually means `b', not
`c'. However, many servers don't follow the spec this closely.

For buffers managed by fully LSP-compliant servers, this should
be set to `lspce-move-to-lsp-abiding-column', and
`lspce-move-to-column' (the default) for all others.")

(defvar lspce--lsp-type-map (make-hash-table :test #'equal)
  "Use file name extension to determine lsp type.")

(defun lspce--lsp-type-default ()
  "The return value is also used as language-id."
  (let ((suffix "")
        (mm (symbol-name major-mode))
        lsp-type)
    (when buffer-file-name
      (setq suffix (file-name-extension buffer-file-name)))
    (setq lsp-type (gethash suffix lspce--lsp-type-map))
    (unless lsp-type
      (setq lsp-type
            (cond
             ((member suffix '("js"))
              "javascript")
             ((member suffix '("ts"))
              "typescript")
             ((member suffix '("tsx"))
              "typescriptreact")
             ((member suffix '("c" "c++" "cpp" "h" "hpp" "cxx" "cc"))
              "C")
             ((member mm '("go-mode" "go-ts-mode"))
              "go")
             ((string-suffix-p "-ts-mode" mm)
              (string-remove-suffix "-ts-mode" mm))
             (t
              (string-remove-suffix "-mode" (symbol-name major-mode))))))
    lsp-type))

(defalias 'lspce--buffer-language-id
  'lspce--lsp-type-default "lspce--buffer-language-id")

(defvar lspce-lsp-type-function #'lspce--lsp-type-default
  "Function to figure out the lsp type of current buffer.")

;;; Lsp types

(cl-defstruct lspce-documentChange
  (kind) ;; change, documentChange, create, rename or delete
  (change)) ;; 
  

(cl-defstruct lspce-completionCache
  (prefix-start)
  (candidates)
  (lsp-items)
  (markers)
  (prefix))

(defun lspce--make-request (method &optional params request-tick)
  "jsonrpc is added in the module."
  (let ((request (make-hash-table)))
    (puthash :id (lspce--next-jsonrpc-id) request)
    (puthash :method method request)
    (puthash :request_tick request-tick request)
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
                                    :willSaveWaitUntil t
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
                                                     :deprecatedSupport t
                                                     :tagSupport (list :valueSet [1])
                                                     :preselectSupoort :json-false
                                                     :insertReplaceSupport :json-false
                                                     :resolveSupport (list :properties (vector "documentation" "details" "additionalTextEdits")))
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
                  :documentSymbol     (list
                                       :dynamicRegistration :json-false
                                       :hierarchicalDocumentSymbolSupport t
                                       :symbolKind `(:valueSet
                                                     [,@(mapcar
                                                         #'car lspce--symbol-kind-names)]))
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
                  :inlayHint          (list
                                       :dynamicRegistration :json-false)
                  :publishDiagnostics (list
                                       :relatedInformation :json-false
                                       :codeDescriptionSupport :json-false
                                       :codeDescriptionSupport :json-false
                                       :dataSupport :json-false
                                       :versionSupport :json-false
                                       :tagSupport (list :valueSet [1 2]))
                  :callHierarchy (list
                                  :dynamicRegistration :json-false))
   :experimental lspce--{}))
  

(defun lspce--initializeParams (rootUri &optional initializationOptions trace workspaceFolders)
  "初始化参数"
  (let ((params (make-hash-table)))
    (puthash :processId (emacs-pid) params)
    ;; some lsp servers don't support rootUri well
    (puthash :rootPath rootUri params)
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
(defalias 'lspce--typeDefinitionParams 'lspce--textDocumentPositionParams "lspce--typeDefinitionParams")
(defalias 'lspce--implementationParams 'lspce--textDocumentPositionParams "lspce--implementationParams")
(defalias 'lspce--hoverParams 'lspce--textDocumentPositionParams "lspce--hoverParams")
(defalias 'lspce--signatureHelpParams 'lspce--textDocumentPositionParams "lspce--signatureHelpParams")

(defun lspce--workspaceSymbolParams (query)
  (let ((params (make-hash-table)))
    (puthash :query query params)
    params))

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

(defun lspce--inlayHintsParams (textDocument range)
  (let ((params (make-hash-table)))
    (puthash :textDocument textDocument params)
    (puthash :range range params)
    params))

;;; Create LSP params
(defun lspce--make-didOpenTextDocumentParams ()
  (let ((uri (lspce--path-to-uri buffer-file-name))
        (language-id (lspce--buffer-language-id))
        (version lspce--identifier-version)
        (text (lspce--widening (buffer-substring-no-properties
                                (point-min) (point-max)))))
    (lspce--didOpenTextDocumentParams
     (lspce--textDocumentItem uri language-id version text))))

(defun lspce--make-didCloseTextDocumentParams ()
  (let ((uri (lspce--path-to-uri buffer-file-name)))
    (lspce--didCloseTextDocumentParams (lspce--textDocumentIdenfitier uri))))

(defun lspce--make-didChangeTextDocumentParams (&optional full)
  (let ((uri (lspce--path-to-uri buffer-file-name))
        (version lspce--identifier-version)
        beg end len text changes)
    (if full
        (setq changes (vector `(:text ,(lspce--widening
                                        (buffer-substring-no-properties
                                         (point-min)
                                         (point-max))))))
      (dolist (change lspce--recent-changes)
        (setq beg (nth 0 change)
              end (nth 1 change)
              len (nth 2 change)
              text (nth 3 change))
        (push (lspce--textDocumentContentChangeEvent
               (lspce--range beg end)
               len
               text)
              changes))
      (setq changes (vconcat changes)))
    (lspce--didChangeTextDocumentParams
     (lspce--versionedTextDocumentIdenfitier uri version)
     changes)))

(defun lspce--make-position (&optional pos)
  (save-excursion
    (let (line character)
      (setq line (1- (line-number-at-pos pos t)))
      (setq character (progn (when pos (goto-char pos))
                             (funcall lspce-current-column-function)))
      (lspce--position line character))))

(defun lspce--make-range (start end)
  (list :start (lspce--make-position start) :end (lspce--make-position end)))

(defun lspce--make-declarationParams ()
  (lspce--declarationParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-definitionParams (&optional context)
  (lspce--definitionParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-implementationParams ()
  (lspce--implementationParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-referenceParams ()
  (lspce--referencesParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)
   (lspce--referenceContext)))

(defun lspce--make-typeDefinitionParams ()
  (lspce--typeDefinitionParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-hoverParams (&optional context)
  (lspce--hoverParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-textDocumentPositionParams (&optional context)
  (lspce--textDocumentPositionParams
   (lspce--textDocumentIdenfitier (lspce--uri))
   (lspce--make-position)
   context))
(defalias 'lspce--make-signatureHelpParams
  'lspce--make-textDocumentPositionParams
  "lspce--make-signatureHelpParams")

(defun lspce--make-workspaceSymbolParams (pattern)
  (lspce--workspaceSymbolParams pattern))

(defun lspce--make-initializeParams (root-uri initializationOptions)
  (lspce--initializeParams root-uri initializationOptions))

(defun lspce--make-inlayHintsParams (start end)
  (lspce--inlayHintsParams
   (lspce--textDocumentIdenfitier (lspce--uri))
   (lspce--make-range start end)))

(defun lspce--make-completionParams()
  (lspce--completionParams (lspce--textDocumentIdenfitier (lspce--uri))
                           (lspce--make-position)
                           (lspce--make-completionContext)))

(defun lspce--make-codeActionContext (begin end &optional action-kind)
  (let ((flymake-diags (flymake-diagnostics begin end))
        (diagnostics (vector))
        diag)
    (dolist (d flymake-diags)
      (setq diag (cdr (assoc 'lspce-lsp-diag (flymake-diagnostic-data d))))
      (when diag
        (setq diagnostics (vconcat diagnostics (list diag)))))
    (lspce--codeActionContext diagnostics (when action-kind (vector action-kind)))))

(defun lspce--make-codeActionParams (begin end &optional action-kind)
  (lspce--codeActionParams
   (lspce--textDocumentIdenfitier (lspce--uri))
   (lspce--make-range begin end)
   (lspce--make-codeActionContext begin end action-kind)))


;;; Macros
(defmacro lspce--while-no-input (&rest body)
  "Wrap BODY in `while-no-input' and respecting `non-essential'.
Return value of `body', or nil if interrupted."
  (declare (debug t) (indent 0))
  `(if non-essential
       (let ((res (while-no-input ,@body)))
         (cond
          ;; interrupted or input arriving
          ((booleanp res)
           nil)
          ;; execute normally
          (t
           res)))
     ,@body))


;;; LSP functions
(defun lspce--server-capable (capability)
  (gethash capability lspce--server-capabilities))

(defun lspce--server-capable-chain (&rest cs)
  (let ((capabilities lspce--server-capabilities))
    (dolist (c cs)
      (when (and capabilities
                 (hash-table-p capabilities))
        (setq capabilities (gethash c capabilities))))
    capabilities))

(defun lspce--project-root ()
  (let ((proj (project-current))
        proj-root)
    (setq proj-root
          (if proj
              (project-root proj)
            (when (and
                   buffer-file-name
                   (member major-mode lspce-modes-enable-single-file-root))
              buffer-file-name)))
    proj-root))

(defun lspce--root-uri ()
  (let ((proj-root (lspce--project-root)))
    (when proj-root
      (lspce--path-to-uri proj-root))))

(defun lspce--project-root-dir ()
  (let ((proj-root (lspce--project-root)))
    (when proj-root
      (if (file-directory-p proj-root)
          proj-root
        (file-name-directory proj-root)))))

(defun lspce--lsp-type ()
  (funcall lspce-lsp-type-function))

(defun lspce--uri ()
  (lspce--path-to-uri (buffer-file-name)))

(defvar lspce--request-ticks (make-hash-table :test #'equal)) ;; key: request id
(defvar lspce--latest-tick nil)

(defun lspce--current-tick ()
  (format "%d_%d_%s" (buffer-chars-modified-tick) (point) (buffer-file-name)))

(defvar lspce--default-sit-for-interval 0.02)
(defvar lspce-sit-for-interval-alist
  '(("java" . 0.05))
  "Map language id to a sit-for interval used to retrieve response.")
(defun lspce--sit-for-interval (language-id)
  (let ((interval (or (assoc-default language-id lspce-sit-for-interval-alist)
                      lspce--default-sit-for-interval)))
    interval))

(cl-defun lspce--get-response
    (request-id method &optional timeout root-uri lsp-type)
  (let ((trying t)
        (lspce--root-uri (or root-uri lspce--root-uri))
        (lspce--lsp-type (or lsp-type lspce--lsp-type))
        (start-time (float-time))
        (request-tick (gethash request-id lspce--request-ticks))
        response-tick
        response msg response-error response-data)
    (lspce--debug "lspce--get-response for request-id %s" request-id)
    (when (member method '("textDocument/completion"))
      (lspce--log-perf
       "before getting response ===== request-id %s, method %s, start-time %s"
       request-id method start-time))
    (while (and trying
                (or (null timeout)
                    (> (+ start-time timeout) (float-time))))
      (lspce--debug "request-tick: %s, lspce--latest-tick: %s"
                    request-tick lspce--latest-tick)
      (unless (string-equal request-tick lspce--latest-tick)
        (lspce--debug
         "lspce--get-response request outdated, request-tick: %s, lspce--latest-tick: %s"
         request-tick lspce--latest-tick)
        (cl-return-from lspce--get-response nil))
      (if (sit-for (lspce--sit-for-interval lspce--lsp-type) t)
          (progn
            (setq response-tick (lspce-module-read-latest-response-tick
                                 lspce--root-uri lspce--lsp-type))
            (lspce--debug "response-tick %S" response-tick)
            (lspce--debug "response-tick %S" response-tick)

            (when (string-equal response-tick request-tick)
              (lspce--debug "start to read response %s" request-id)
              (setq response
                    (lspce-module-read-response-exact
                     lspce--root-uri lspce--lsp-type request-id method))
              (lspce--debug "response %s" response)

              (when response
                (setq trying nil)
                (setq msg (lspce--json-deserialize response))
                (setq response-error (gethash "error" msg))
                (if response-error
                    (lspce--warn "LSP error %s"
                                 (gethash "message" response-error))
                  (setq response-data (gethash "result" msg))))))
        (lspce--debug "sit-for is interrupted.")
        (setq trying nil)))
    (when (member method '("textDocument/completion"))
      (lspce--log-perf
       "after getting response ===== request-id %s, method %s, end-time %s"
       request-id method (float-time)))
    response-data))

(defun lspce--request (method &optional params timeout root-uri lsp-type)
  (lspce--while-no-input
    (let (response)
      (when-let (request-id
                 (lspce--request-async method params root-uri lsp-type))
        (setq response
              (lspce--get-response request-id method timeout root-uri lsp-type))
        (when (string-equal method "textDocument/completion")
          (lspce--log-perf
           "response ===== request_id: %s, method: %s" request-id method)))
      response)))

(cl-defun lspce--notify (method &optional params root-uri lsp-type)
  (let ((notification (lspce--make-notification method params))
        (root-uri (or root-uri lspce--root-uri))
        (lsp-type (or lsp-type lspce--lsp-type)))
    (unless (and root-uri lsp-type)
      (user-error
       "lspce--notify: Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--notify nil))
    (lspce-module-notify root-uri lsp-type (json-encode notification))))

(defun lspce--notify-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when lspce--recent-changes
    (let* ((sync-capability (lspce--server-capable "textDocumentSync"))
           (sync-kind (if (numberp sync-capability) sync-capability
                        (or (and (hash-table-p sync-capability)
                                 (gethash "change" sync-capability))
                            2)))
           (full-sync-p (or (eq sync-kind 1)
                            (eq :emacs-messup lspce--recent-changes))))
      (when (not (eq sync-kind 0))
        (lspce--notify "textDocument/didChange"
                       (lspce--make-didChangeTextDocumentParams full-sync-p)))
      (setq lspce--recent-changes nil))))

(defun lspce--notify-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (lspce--notify
   "textDocument/didOpen" (lspce--make-didOpenTextDocumentParams)))

(defun lspce--notify-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[lspce] error sending textDocument/didClose: %s"
    (lspce--notify
     "textDocument/didClose" (lspce--make-didCloseTextDocumentParams))))

(defun lspce--make-willSaveTextDocumentParams ()
  (list :textDocument (lspce--textDocumentIdenfitier (lspce--uri))
        :reason 1))

(defun lspce--notify-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (when (lspce--server-capable-chain "textDocumentSync" "willSave")
    (lspce--notify
     "textDocument/willSave"
     (lspce--make-willSaveTextDocumentParams))))

(defun lspce--request-textDocument/willSaveWaitUntil ()
  "Request textDocument/willSaveWaitUntil."
  (when (lspce--server-capable-chain "textDocumentSync" "willSaveWaitUntil")
    (let ((response (lspce--request "textDocument/willSaveWaitUntil" (lspce--make-willSaveTextDocumentParams) 1)))
      (when response (lspce--apply-text-edits response)))))

(defun lspce--notify-textDocument/didSave ()
  "Send textDocument/willSave to server."
  (let ((capability (lspce--server-capable-chain "textDocumentSync" "save"))
        (includeText nil))
    (when capability
      (when (hash-table-p capability)
        (setq includeText (gethash "includeText" capability)))
      (if includeText
          (lspce--notify
           "textDocument/didSave"
           (list :textDocument
                 (lspce--textDocumentIdenfitier (lspce--uri))
                 :text
                 (lspce--widening
                  (buffer-substring-no-properties (point-min) (point-max)))))
        (lspce--notify
         "textDocument/didSave"
         (list :textDocument (lspce--textDocumentIdenfitier (lspce--uri))))))))

(cl-defun lspce--request-async (method &optional params root-uri lsp-type)
  ;; (unless (and buffer-file-name
  ;;              (file-exists-p buffer-file-name))
  ;;   (lspce--warn "lspce--request-async: current buffer has no disk file.")
  ;;   (cl-return-from lspce--request-async nil))
  (setq lspce--latest-tick (lspce--current-tick))
  (let* ((request (lspce--make-request method params lspce--latest-tick))
         (root-uri (or root-uri lspce--root-uri))
         (lsp-type (or lsp-type lspce--lsp-type))
         (request-id (gethash :id request)))
    (unless (and root-uri lsp-type)
      (user-error
       "lspce--request-async: Can not get root-uri or lsp-type.")
      (cl-return-from lspce--request-async nil))

    (lspce--notify-textDocument/didChange)

    (when (string-equal method "textDocument/completion")
      (lspce--log-perf "request ===== request_id: %s, method: %s"
                       request-id method))
    (lspce--debug "lspce--request-async request: %s" (json-encode request))
    (when (lspce-module-request-async root-uri lsp-type (json-encode request))
      (puthash request-id lspce--latest-tick lspce--request-ticks)
      request-id)))

(defun lspce--process-environment ()
  (let ((ht (make-hash-table :test #'equal))
        key value)
    (dolist (env process-environment)
      (when-let (pos (string-search "=" env))
        (setq key (substring env 0 pos)
              value (substring env (1+ pos)))
        (puthash key value ht)))
    ht))

(defun lspce--envs-pass-to-subprocess ()
  (let ((ht (make-hash-table :test #'equal))
        (process-envs (lspce--process-environment)))
    (dolist (env lspce-envs-pass-to-subprocess)
      (when-let (value (gethash env process-envs))
        (if (string-equal env "PATH")
            (puthash "PATH" (mapconcat 'identity exec-path ":") ht)
          (puthash env value ht))))
    (when lspce-inherit-exec-path
      (puthash "PATH" (mapconcat 'identity exec-path ":") ht))
    (lspce--json-serialize ht)))


;;;###autoload
(defvar-local lspce-workspace-configuration nil
  "Configure LSP servers specifically for a given project.")

;;;###autoload
(put 'lspce-workspace-configuration 'safe-local-variable 'listp)

(defun lspce--workspace-configuration-plist (&optional current-buffer)
  "Returns workspace configuration of current server as a plist."
  (with-current-buffer (or current-buffer (current-buffer))
    lspce-workspace-configuration))

(defun lspce--notify-workspace/didChangeConfiguration (configuration)
  (let ((settings (make-hash-table :test #'equal)))
    (lspce--add-options-internal settings configuration)
    (lspce--notify
     "workspace/didChangeConfiguration" (list :settings (or settings lspce--{})))))

;; 返回server info.
(defun lspce--connect-existing-server (root-uri lsp-type)
  (let (lsp-server)
    (setq lsp-server (lspce-module-server root-uri lsp-type))
    (when lsp-server
      (lspce--info
       "lspce--connect: Server for (%s %s) is running." root-uri lsp-type))
    lsp-server))

(cl-defun lspce--connect (root-uri lsp-type server-cmd server-args initialize-options)
  (let ((initialize-params nil)
        lsp-server
        response-str)
    
    (setq lsp-server (lspce-module-server root-uri lsp-type))
    (when lsp-server
      (lspce--info
       "lspce--connect: Server for (%s %s) is running." root-uri lsp-type)
      (cl-return-from lspce--connect lsp-server))

    (setq initialize-params
          (lspce--make-initializeParams root-uri initialize-options))

    (lspce--debug "lspce--connect initialize-params: %s"
                  (json-encode initialize-params))

    (setq lspce--latest-tick (lspce--current-tick))
    (setq response-str
          (lspce-module-connect root-uri
                                lsp-type
                                server-cmd
                                server-args
                                (json-encode
                                 (lspce--make-request "initialize"
                                                      initialize-params
                                                      lspce--latest-tick))
                                lspce-connect-server-timeout
                                (lspce--envs-pass-to-subprocess)))
    (lspce--debug "lspce--connect response: %s" response-str)

    (when response-str
      (let ((configuration (lspce--workspace-configuration-plist (current-buffer))))
        (lspce--notify-workspace/didChangeConfiguration configuration)))

    response-str))

(defvar-local lspce--uri nil)
(defvar-local lspce--root-uri nil)
(defvar-local lspce--lsp-type nil)
(defvar-local lspce--server-info nil)
(defvar-local lspce--server-capabilities nil)

(defvar lspce--notification-idle-timer nil)

;;;###autoload
(defun lspce-enable-notification-handler ()
  (interactive)
  (when lspce--notification-idle-timer
    (cancel-timer lspce--notification-idle-timer))
  (setq lspce--notification-idle-timer
        (run-with-idle-timer 1 t #'lspce--notification-handler)))

;;;###autoload
(defun lspce-disable-notification-handler ()
  (interactive)
  (when lspce--notification-idle-timer
    (cancel-timer lspce--notification-idle-timer)))

(defun lspce--notification-handler ()
  (let (notification
        method params
        message-type
        message)
    (with-current-buffer (current-buffer)
      (when (and lspce-mode
                 lspce--root-uri
                 lspce--lsp-type)
        (setq notification
              (lspce-module-read-notification lspce--root-uri lspce--lsp-type))
        (lspce--debug "notification: %s" notification)
        (when notification
          (setq notification (lspce--json-deserialize notification))
          (setq method (gethash "method" notification)
                params (gethash "params" notification))
          (cond
           ((or (string-equal method "window/showMessage")
                (string-equal method "window/logMessage"))
            (setq message-type (gethash "type" params)
                  message (gethash "message" params))
            (cond
             ((eq message-type 1)
              (lspce--error "Server message: %s" message))
             ((eq message-type 2)
              (lspce--warn "Server message: %s" message))
             ((eq message-type 3)
              (lspce--info "Server message: %s" message))
             ((eq message-type 4)
              (lspce--debug "Server message: %s" message))))
           (t)))))))
            


(provide 'lspce-core)
