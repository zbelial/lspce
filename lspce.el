;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(unless (json-available-p)
  (user-error "LSPCE needs JSON support in Emacs;
 please rebuild it using `--with-json'"))


;;; Require
(require 'json)
(eval-when-compile
  (require 'cl-lib))
(require 'project)
(require 'url-util)
(require 'compile)                      ; for some faces
(require 'warnings)
(ignore-errors
  (require 'posframe-plus))
(require 'markdown-mode)
(require 'yasnippet)
(require 'flymake)

(require 'lspce-core)
(eval-and-compile
  (require 'lspce-util))
(require 'lspce-langs)

;;; Code
(defface lspce-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in LSPCE's mode line.")

(defcustom lspce-send-changes-idle-time 0.5
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :group 'lspce
  :type 'number)

(defcustom lspce-doc-tooltip-border-width 1
  "The border width of lspce tooltip, default is 1 px."
  :group 'lspce
  :type 'integer)

(defcustom lspce-doc-tooltip-timeout 30
  "The timeout of lspce tooltip show time, in seconds."
  :group 'lspce
  :type 'integer)

(defcustom lspce-completion-ignore-case t
  "If non-nil, ignore case when completing."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-enable-eldoc t
  "If non-nil, enable eldoc."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-eldoc-enable-hover t
  "If non-nil, enable hover in eldoc."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-eldoc-enable-signature t
  "If non-nil, enable signature in eldoc."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-enable-flymake t
  "If non-nil, enable flymake."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-enable-logging nil
  "If non-nil, enable logging to file."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-auto-enable-within-project t
  "If non-nil, enable lspce when a file is opened
if lspce is running in current project."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-completion-no-annotation nil
  "If non-nil, do not display completion item's annotation."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-show-log-level-in-modeline t
  "If non-nil, show log level in modeline."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-xref-append-implementations-to-definitions nil
  "If non-nil, when finding definitions, also query implementations
 and append them to definitions."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-enable-imenu-index-function nil
  "If non-nil, set `lspce-imenu-create' as `imenu-create-index-function'."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-after-text-edit-hook '()
  "Functions called after finishing text edits in a buffer.
When running hooks, current buffer is set to the buffer being edited."
  :type 'hook)

(defcustom lspce-after-each-text-edit-hook '()
  "Functions called after finishing each text edit in a buffer.
When running hooks, current buffer is set to the buffer being edited."
  :type 'hook)

(defcustom lspce-enable-symbol-highlight nil
  "Whether enable lsp symbol highlighting."
  :group 'lspce
  :type 'boolean)

(defcustom lspce-on-idle-hook nil
  "on idle hook"
  :type 'hook
  :group 'lspce)

(defcustom lspce-idle-delay 0.5
  "idle time."
  :type 'number
  :group 'lspce)

(defface lspce-face-highlight
  '((t :inherit highlight))
  "Base face used for highlighting occurrences of symbols."
  :group 'lspce-mode)

(defface lspce-face-highlight-textual
  '((t :inherit lspce-face-highlight))
  "Face used for textual occurrences of symbols."
  :group 'lspce-mode)

(defface lspce-face-highlight-read
  '((t :inherit lspce-face-highlight))
  "Face used for highlighting symbols being read."
  :group 'lspce-mode)

(defface lspce-face-highlight-write
  '((t :inherit lspce-face-highlight :weight bold))
  "Face used for highlighting symbols being written to."
  :group 'lspce-mode)

;;; Logging
(defvar lspce--log-level-text nil)
(defun lspce-get-log-level ()
  "Get current log level."
  (interactive)
  (let ((ll (lspce-module-get-log-level)))
    ll))

(defconst lspce--log-level-plist
  '(0 "DISABLED" 1 "ERROR" 2 "INFO" 3 "TRACE" 4 "DEBUG"))

(defun lspce--refresh-log-level ()
  (when lspce-show-log-level-in-modeline
    (let* ((ll (lspce-module-get-log-level))
           (ltext (plist-get lspce--log-level-plist ll)))
      (setq lspce--log-level-text ltext)
      nil)))

(defun lspce-disable-logging ()
  "Disable logging"
  (interactive)
  (lspce-module-disable-logging)
  (lspce--refresh-log-level))

(defun lspce-enable-logging ()
  "Enable logging"
  (interactive)
  (lspce-module-enable-logging)
  (lspce--refresh-log-level))

(defun lspce-set-log-level-error ()
  "Set log level to error on rust code side."
  (interactive)
  (lspce-module-set-log-level 1)
  (lspce--refresh-log-level))

(defun lspce-set-log-level-info ()
  "Set log level to info on rust code side."
  (interactive)
  (lspce-module-set-log-level 2)
  (lspce--refresh-log-level))

(defun lspce-set-log-level-trace ()
  "Set log level to trace on rust code side."
  (interactive)
  (lspce-module-set-log-level 3)
  (lspce--refresh-log-level))

(defun lspce-set-log-level-debug ()
  "Set log level to debug on rust code side."
  (interactive)
  (lspce-module-set-log-level 4)
  (lspce--refresh-log-level))

(defun lspce-set-log-file (filename)
  (let ((dirname (f-dirname filename)))
    (unless (file-exists-p dirname)
      (f-mkdir-full-path dirname))
    (lspce-module-set-log-file filename)))

(defun lspce--remove-overlays ()
  (remove-overlays nil nil 'lspce--overlay t))

(defvar-local lspce--saved-bindings nil
  "Bindings saved by `lspce--save-and-setq'.")

(defmacro lspce--save-and-rebind (symbol binding)
  `(unless (not (boundp ',symbol))
     (push (cons ',symbol (symbol-value ',symbol)) lspce--saved-bindings)
     (setq-local ,symbol ,binding)))

(defun lspce--restore-bindings ()
  (dolist (binding lspce--saved-bindings)
    (set (make-local-variable (car binding)) (cdr binding))))

;;; Xref
(defun lspce-xref-backend () 'xref-lspce)

(cl-defstruct lspce--xref-item
  (filename)
  (start-pos)
  (start-line)
  (start-column)
  (end-pos)
  (end-line)
  (end-column))

(defun lspce--location-before-p (left right)
  "Sort first by file, then by line, then by column."
  (let ((left-uri (lspce--xref-item-filename left))
        (right-uri (lspce--xref-item-filename right))
        (left-start-line (lspce--xref-item-start-line left))
        (right-start-line (lspce--xref-item-start-line right))
        (left-start-column (lspce--xref-item-start-column left))
        (right-start-column (lspce--xref-item-start-column right)))
    (if (not (string= left-uri right-uri))
        (string< left-uri right-uri)
      (if (= left-start-line right-start-line)
          (< left-start-column right-start-column)
        (< left-start-line right-start-line)))))

(defun lspce--lsp-position-to-point (pos &optional markers)
  "Convert LSP position to Emacs point."
  (let ((line (gethash "line" pos))
        (character (gethash "character" pos)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line line)
        (unless (eobp) ;; if line was excessive leave point at eob
          (let ((tab-width 1)
                (col character))
            (unless (wholenump col)
              (lspce--warn
               "LSP server sent invalid character position %s. Using 0 instead."
               col)
              (setq col 0))
            (funcall lspce-move-to-column-function col)))
        (if markers (copy-marker (point-marker)) (point))))))

(defun lspce--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((start (gethash "start" range))
         (end (gethash "end" range))
         (beg (lspce--lsp-position-to-point start markers))
         (end (lspce--lsp-position-to-point end markers)))
    (cons beg end)))

(defun lspce--locations-to-xref (locations)
  (let (xrefs uri range start
              end filename start-line start-column
              end-line end-column items xref-items groups)
    (cond
     ((arrayp locations)
      (setq items locations)
      )
     ((hash-table-p locations)
      (setq items (list locations)))
     ((listp locations)
      (setq items locations))
     (t
      (setq items nil)))
    (condition-case err
        (progn
          (dolist (item items)
            (setq uri (or (gethash "uri" item)
                          (gethash "targetUri" item)))
            (setq range (or (gethash "range" item)
                            (gethash "targetSelectionRange" item)))
            (setq start (gethash "start" range))
            (setq end (gethash "end" range))
            (setq start-line (gethash "line" start))
            (setq start-column (gethash "character" start))
            (setq end-line (gethash "line" end))
            (setq end-column (gethash "character" end))
            (if (string-prefix-p "jdt:\/\/" uri)
                (setq filename (lspce--jdtls-open-jdt-link uri))
              (setq filename (lspce--uri-to-path uri)))

            (when filename
              (cl-pushnew (make-lspce--xref-item :filename filename
                                                 :start-pos start
                                                 :start-line start-line
                                                 :start-column start-column
                                                 :end-pos end
                                                 :end-line end-line
                                                 :end-column end-column)
                          xref-items)))

          (setq groups (seq-group-by
                        (lambda (x) (lspce--xref-item-filename x))
                        (seq-sort #'lspce--location-before-p xref-items)))
          (dolist (group groups)
            (let* ((filename (car group))
                   (items (cdr group))
                   (visiting (find-buffer-visiting filename))
                   (collect
                    (lambda (item)
                      (lspce--widening
                       (let* ((beg (lspce--lsp-position-to-point
                                    (lspce--xref-item-start-pos item)))
                              (end (lspce--lsp-position-to-point
                                    (lspce--xref-item-end-pos item)))
                              (bol (progn
                                     (goto-char beg) (point-at-bol)))
                              (substring
                               (buffer-substring bol (point-at-eol)))
                              (hi-beg (- beg bol))
                              (hi-end (- (min (point-at-eol) end) bol)))
                         (add-face-text-property hi-beg hi-end
                                                 'xref-match t substring)
                         (cl-pushnew
                          (xref-make substring
                                     (xref-make-file-location
                                      filename
                                      (+ (lspce--xref-item-start-line item) 1)
                                      (lspce--xref-item-start-column item)))
                          xrefs))))))
              (if visiting
                  (with-current-buffer visiting
                    (seq-map collect items))
                (when (file-readable-p filename)
                  (with-temp-buffer
                    (insert-file-contents-literally filename)
                    (seq-map collect items)))))))
      
      (error (lspce--warn "Failed to process xref entry for filename '%s': %s"
                          filename (error-message-string err)))
      (file-error (lspce--warn "Failed to process xref entry, '%s': %s"
                               filename (error-message-string err))))
    (nreverse xrefs)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lspce)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(defun lspce--xref-dup-test (x1 x2)
  (let* ((x1-location (xref-item-location x1))
         (x1-file (xref-file-location-file x1-location))
         (x1-line (xref-file-location-line x1-location))
         (x1-column (xref-file-location-column x1-location))
         (x2-location (xref-item-location x2))
         (x2-file (xref-file-location-file x2-location))
         (x2-line (xref-file-location-line x2-location))
         (x2-column (xref-file-location-column x2-location)))
    (and (string-equal x1-file x2-file)
         (= x1-line x2-line)
         (= x1-column x2-column))))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lspce)) identifier)
  (let (defs
        impls)
    (save-excursion
      (when (lspce--server-capable "definitionProvider")
        (let* ((method "textDocument/definition")
               (response (lspce--request method
                                         (lspce--make-definitionParams))))
          (when response
            (setq defs (lspce--locations-to-xref response)))))
      (when (and lspce-xref-append-implementations-to-definitions
                 (lspce--server-capable "implementationProvider"))
        (let* ((method "textDocument/implementation")
               (response (lspce--request method
                                         (lspce--make-implementationParams))))
          (when response
            (setq impls (lspce--locations-to-xref response))))))
    (cl-remove-duplicates (append defs impls) :test #'lspce--xref-dup-test)))

;; NOTE if you use `ivy-xref-show-xrefs' as the `xref-show-xrefs-function',
;; you will find `xref-backend-references' is called twice.
;; See https://github.com/alexmurray/ivy-xref/issues/2
(cl-defmethod xref-backend-references ((_backend (eql xref-lspce)) identifier)
  (when (lspce--server-capable "referencesProvider")
    (save-excursion
      (let* ((method "textDocument/references")
             (response (lspce--request method (lspce--make-referenceParams))))
        (when response
          (lspce--locations-to-xref response))))))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lspce)) pattern)
  (when (lspce--server-capable "workspaceSymbolProvider")
    (let* ((method "workspace/symbol")
           (response (lspce--request
                      method
                      (lspce--make-workspaceSymbolParams pattern)))
           location locations)
      (when response
        (dolist (symbol response)
          (when-let ((location (gethash "location" symbol)))
            (cl-pushnew location locations)))
        (lspce--locations-to-xref locations)))))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql xref-lspce)))
  (list (propertize (or (thing-at-point 'symbol) "")
                    'identifier-at-point t)))

(when (not (fboundp 'xref-backend-implementations))
  (cl-defgeneric xref-backend-implementations (backend identifier)
    "Find implementations of IDENTIFIER.

The result must be a list of xref objects. If there are multiple possible
implementations, return all of them.  If no implementations can be found,
return nil.

IDENTIFIER can be any string returned by
`xref-backend-identifier-at-point', or from the table returned by
`xref-backend-identifier-completion-table'.

To create an xref object, call `xref-make'.")
  
  )

(cl-defmethod xref-backend-implementations
  ((_backend (eql xref-lspce)) identifier)
  (when (lspce--server-capable "implementationProvider")
    (save-excursion
      (let* ((method "textDocument/implementation")
             (response (lspce--request method
                                       (lspce--make-implementationParams))))
        (when response
          (lspce--locations-to-xref response))))))

;;;###autoload
(defun xref-find-implementations (identifier)
  "Find implementations to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point."
  (interactive (list (xref--read-identifier "Find implementations of: ")))
  (xref--find-xrefs identifier 'implementations identifier nil))

;; type definition
(when (not (fboundp 'xref-backend-type-definition))
  (cl-defgeneric xref-backend-type-definition (backend identifier)
    "Find type definition of IDENTIFIER.

The result must be a list of xref objects.
If no type definition can be found, return nil.

IDENTIFIER can be any string returned by
`xref-backend-identifier-at-point', or from the table returned by
`xref-backend-identifier-completion-table'.

To create an xref object, call `xref-make'.")
  )

(cl-defmethod xref-backend-type-definition
  ((_backend (eql xref-lspce)) identifier)
  (when (lspce--server-capable "typeDefinitionProvider")
    (save-excursion
      (let* ((method "textDocument/typeDefinition")
             (response (lspce--request
                        method
                        (lspce--make-typeDefinitionParams))))
        (when response
          (lspce--locations-to-xref response))))))

;;;###autoload
(defun xref-find-type-definition (identifier)
  "Find type definition to the IDENTIFIER at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point."
  (interactive (list (xref--read-identifier "Find Type Definition of: ")))
  (xref--show-defs
   (xref--create-fetcher identifier 'type-definition identifier)
   nil))

;; declaration
(when (not (fboundp 'xref-backend-declaration))
  (cl-defgeneric xref-backend-declaration (backend identifier)
    "Find declaration of IDENTIFIER.

The result must be a list of xref objects. If there are multiple possible
declarations, return all of them.  If no declaration can be found,
return nil.

IDENTIFIER can be any string returned by
`xref-backend-identifier-at-point', or from the table returned by
`xref-backend-identifier-completion-table'.

To create an xref object, call `xref-make'.")
  )

(cl-defmethod xref-backend-declaration ((_backend (eql xref-lspce)) identifier)
  (when (lspce--server-capable "declarationProvider")
    (save-excursion
      (let* ((method "textDocument/declaration")
             (response (lspce--request method (lspce--make-declarationParams))))
        (when response
          (lspce--locations-to-xref response))))))

;;;###autoload
(defun xref-find-declaration (identifier)
  "Find declaration to the IDENTIFIER at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point."
  (interactive (list (xref--read-identifier "Find Definition of: ")))
  (xref--show-defs
   (xref--create-fetcher identifier 'declaration identifier)
   nil))

;;; Capf
(defvar-local lspce--completion-complete? nil) ;; 1 incomplete 2 complete
(defvar-local lspce--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defvar lspce--completion-cache nil
  "Cached candidates for completion at point function.
In the form of plist (prefix-start-pos items :lsp-items :markers :prefix ...).
When the completion is incomplete, `items' contains value of :incomplete.")

(defvar lspce--completion-last-result nil
  "Last completion result.")

(defun lspce--completion-clear-cache (&optional keep-last-result)
  "Clear completion caches, and last result if KEEP-LAST-RESULT not specified."
  (let ((markers (and lspce--completion-cache
                      (lspce-completionCache-markers lspce--completion-cache))))
    (when markers
      (setq markers (list (cl-first markers)
                          (set-marker (cl-second markers) nil)))
      (setf (lspce-completionCache-markers lspce--completion-cache) markers)))
  (setq lspce--completion-cache nil)
  (unless keep-last-result
    (setq lspce--completion-last-result nil)))

(defun lspce--post-self-insert-hook ()
  "Set `lspce--last-inserted-char'."
  (setq lspce--last-inserted-char last-input-event))

(defun lspce--pre-command-hook ()
  "Reset `lspce--last-inserted-char'."
  (setq lspce--last-inserted-char nil))


(defun lspce--make-completionContext ()
  (let ((trigger (and (characterp lspce--last-inserted-char)
                      (cl-find lspce--last-inserted-char
                               (lspce--server-capable-chain "completionProvider"
                                                            "triggerCharacters")
                               :key (lambda (str) (aref str 0))
                               :test #'char-equal))))
    (if trigger
        (lspce--completionContext LSPCE-TriggerCharacter trigger)
      (if (and lspce--completion-complete? (= lspce--completion-complete? 1))
          (lspce--completionContext LSPCE-TriggerForIncompleteCompletions nil)
        (lspce--completionContext LSPCE-Invoked nil)))))

(defvar lspce--in-completion-p nil)
(cl-defun lspce--request-completion ()
  (setq lspce--in-completion-p t)
  (condition-case err
      (let* ((method "textDocument/completion")
             (params (lspce--make-completionParams))
             (response (lspce--request method params))
             items complete?)
        (setq lspce--in-completion-p nil)
        (unless response
          (lspce--debug "lspce--request-completion failed to getting response")
          (cl-return-from lspce--request-completion nil))

        (lspce--debug "lspce--request-completion response: %S" response)
        (lspce--debug "lspce--request-completion response type-of: %s"
                      (type-of response))
        (cond
         ((listp response)
          (setq complete? t
                items response))
         ((hash-table-p response)
          (setq complete? (not (gethash "isIncomplete" response))
                items (gethash "items" response)))
         (t
          (lspce--warn "Unknown response type: %s" (type-of response))
          (cl-return-from lspce--request-completion nil)))
        (list complete? items))
    ((error quit)
     (lspce--error "lspce--request-completion error: [%s]" err)
     (setq lspce--in-completion-p nil)
     nil)))

(defun lspce--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'lspce--expand-snippet))

(defun lspce--completion-resolve (item)
  (when (and
         (lspce--server-capable-chain "completionProvider" "resolveProvider")
         (gethash "data" item))
    (lspce--request "completionItem/resolve" item)))

(defun lspce--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list markup 'gfm-view-mode)
                 (list (gethash "value" markup)
                       (pcase (gethash "kind" markup)
                         ("markdown" 'gfm-view-mode)
                         ("plaintext" 'text-mode)
                         (_ major-mode))))))
    (with-temp-buffer
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert string)
      (let ((inhibit-message t)
            (message-log-max nil))
        (ignore-errors (delay-mode-hooks (funcall mode))))
      (font-lock-ensure)
      (string-trim (buffer-string)))))

(defun lspce--completion-looking-back-trigger (trigger-characters)
  "Return trigger character if text before point
matches any of the TRIGGER-CHARACTERS."
  (unless (= (point) (point-at-bol))
    (seq-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties
                    (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

(defun lspce--completion-bounds-start (start trigger-chars)
  (let ((bounds-start (point)))
    (when start
      (setq bounds-start (save-excursion
                           (ignore-errors
                             (goto-char (+ start 1))
                             (while (lspce--completion-looking-back-trigger
                                     trigger-chars)
                               (cl-incf start)
                               (forward-char))
                             start))))
    bounds-start))

(defun lspce-completion-at-point()
  (when-let (completion-capability (lspce--server-capable "completionProvider"))
    (let* ((trigger-chars (lspce--server-capable-chain "completionProvider"
                                                       "triggerCharacters"))
           (bounds (bounds-of-thing-at-point 'symbol))
           (bounds-start (lspce--completion-bounds-start
                          (cl-first bounds) trigger-chars))
           (sort-completions
            (lambda (completions)
              (cl-sort completions
                       #'string-lessp
                       :key (lambda (c)
                              (or (gethash
                                   "sortText"
                                   (get-text-property 0 'lspce--lsp-item c))
                                  "")))))
           done?
           (cached-proxies :none)
           (proxies
            (lambda ()
              (let* ((same-session?
                      (and lspce--completion-cache
                           ;; Special case for empty prefix and empty result
                           (or (lspce-completionCache-candidates lspce--completion-cache)
                               (not (string-empty-p
                                     (lspce-completionCache-prefix lspce--completion-cache))))
                           (equal
                            (lspce-completionCache-prefix-start lspce--completion-cache) bounds-start)
                           (string-prefix-p
                            (lspce-completionCache-prefix lspce--completion-cache)
                            (buffer-substring-no-properties bounds-start (point)))))
                     (old-cached-proxies
                      (and same-session?
                           (lspce-completionCache-candidates lspce--completion-cache))))
                (if same-session?
                    (lspce--debug "same-session")
                  (lspce--debug "not same-session"))
                (lspce--debug "last insert char: " lspce--last-inserted-char)
                (cond
                 ((or done? (listp cached-proxies))
                  (lspce--debug "use cached-proxies")
                  cached-proxies)
                 ((and same-session?
                       old-cached-proxies
                       (listp old-cached-proxies))
                  (lspce--debug "use old cached-proxies")
                  (setq cached-proxies old-cached-proxies))
                 (t
                  (lspce--debug "new completion request")
                  (let* ((completions (lspce--request-completion))
                         (complete? (nth 0 completions))
                         (items (nth 1 completions))
                         (markers (list bounds-start (copy-marker (point) t)))
                         (prefix (buffer-substring-no-properties bounds-start (point))))
                    (lspce--debug "completion result: %s" (json-encode completions))
                    (if completions
                        (progn
                          (if complete?
                              (setq-local lspce--completion-complete? 2)
                            (setq-local lspce--completion-complete? 1))
                          (setq cached-proxies
                                (mapcar
                                 (lambda (item)
                                   (let* ((label (gethash "label" item))
                                          (insertTextFormat (or (gethash "insertTextFormat" item) 1))
                                          (insertText (gethash "insertText" item))
                                          (proxy
                                           (cond 
                                            ((and (eql insertTextFormat 2)
                                                  (lspce--snippet-expansion-fn))
                                             (string-trim-left label))
                                            ((and insertText
                                                  (not (string-empty-p insertText)))
                                             insertText)
                                            (t
                                             (string-trim-left label)))))
                                     (unless (zerop (length proxy))
                                       (put-text-property 0 1 'lspce--lsp-item item proxy)
                                       (put-text-property 0 1 'lspce--lsp-markers markers proxy)
                                       (put-text-property 0 1 'lspce--lsp-start bounds-start proxy)
                                       (put-text-property 0 1 'lspce--lsp-prefix prefix proxy))
                                     proxy))
                                 items))
                          (lspce--completion-clear-cache same-session?)
                          (setq done? complete?)
                          (setq lspce--completion-cache
                                (make-lspce-completionCache :prefix-start bounds-start
                                                            :candidates (cond
                                                                         ((and done?
                                                                               (not (seq-empty-p cached-proxies)))
                                                                          cached-proxies)
                                                                         ((not done?)
                                                                          :incomplete))
                                                            :lsp-items nil
                                                            :markers markers
                                                            :prefix prefix))
                          (setq lspce--completion-last-result cached-proxies))
                      (when same-session?
                        lspce--completion-last-result))))))))
           (resolved (make-hash-table))
           (resolve-maybe (lambda (lsp-item)
                            (or (gethash lsp-item resolved)
                                (setf (gethash lsp-item resolved)
                                      (or (lspce--completion-resolve lsp-item) lsp-item))))))
      (list
       bounds-start
       (point)
       (lambda (probe pred action)
         (let (collection)
           (lspce--debug "action: %s" action)
           (cond
            ((eq action 'metadata) `(metadata (category . lspce-capf)
                                              (display-sort-function . ,sort-completions)
                                              (cycle-sort-function . identity)))               ; metadata
            ((eq (car-safe action) 'boundaries) nil)       ; boundaries
            ;; test-completion: not return exact match so that the selection will
            ;; always be shown
            ((eq action 'lambda)                           ; test-completion
             (setq collection (funcall proxies))
             (lspce--debug "lambda collection: %s" collection)
             (test-completion pattern collection))
            ((null action)                                 ; try-completion
             (setq collection (funcall proxies))
             (lspce--debug "null collection: %s" collection)
             (try-completion probe collection))
            ((eq action t)                                 ; all-completions
             (setq collection (funcall proxies))
             (lspce--debug "t collection: %s" collection)
             (all-completions
              probe
              collection
              (lambda (proxy)
                (let* ((item (get-text-property 0 'lspce--lsp-item proxy))
                       (filterText (and item (gethash "filterText" item))))
                  (lspce--debug "item: %s" item)
                  (lspce--debug "filterText: %s" filterText)
                  (lspce--debug "probe: %s" probe)
                  ;; FIXME typescript-language-server returns filterText and newText with a "." prefix,
                  ;; it will make the following string-prefix-p fail since probe does not have a "."
                  ;; NOTE https://github.com/typescript-language-server/typescript-language-server/issues/631
                  ;; There is useful information about filterText in the above link.
                  (and (or (null pred) (funcall pred proxy))
                       (string-prefix-p
                        probe (or filterText proxy) lspce-completion-ignore-case)))))))))
       :annotation-function
       (lambda (proxy)
         (unless lspce-completion-no-annotation
           (let* ((item (get-text-property 0 'lspce--lsp-item proxy))
                  (detail (gethash "detail" item))
                  (kind (gethash "kind" item))
                  annotation)
             (setq detail (and (stringp detail)
                               (not (string-empty-p detail))
                               detail))
             (setq annotation (or detail
                                  (cdr (assoc kind lspce--kind-names))))
             (when annotation
               (concat " "
                       (propertize annotation
                                   'face 'font-lock-function-name-face))))))
       :company-require-match 'never
       :company-kind
       ;; Associate each lsp-item with a lsp-kind symbol.
       (lambda (proxy)
         (when-let* ((lsp-item (get-text-property 0 'lspce--lsp-item proxy))
                     (kind (alist-get (gethash "kind" lsp-item)
                                      lspce--kind-names)))
           (intern (downcase kind))))
       :company-deprecated
       (lambda (proxy)
         (let ((lsp-item (get-text-property 0 'lspce--lsp-item proxy))
               tags deprecated)
           (setq tags (gethash "tags" lsp-item)
                 deprecated (gethash "deprecated" lsp-item))
           (or (seq-contains-p tags 1)
               (eq t deprecated))))
       :company-doc-buffer
       (lambda (proxy)
         (let* ((documentation
                 (let* ((lsp-item (get-text-property 0 'lspce--lsp-item proxy))
                        (resolve (funcall resolve-maybe lsp-item)))
                   (when resolve
                     (gethash "documentation" resolve))))
                (formatted (and documentation
                                (lspce--format-markup documentation))))
           (when formatted
             (with-current-buffer (get-buffer-create " *lspce doc*")
               (erase-buffer)
               (insert formatted)
               (current-buffer)))))       
       :company-prefix-length
       (save-excursion
         (when (car bounds)
           (goto-char (car bounds)))
         (when (hash-table-p completion-capability)
           (looking-back
            (regexp-opt
             (cl-coerce (gethash "triggerCharacters" completion-capability) 'list))
            (line-beginning-position))))
       :exit-function
       (lambda (proxy status)
         (when (memq status '(finished exact))
           (let ((inhibit-redisplay t))
             (with-current-buffer (if (minibufferp)
                                      (window-buffer (minibuffer-selected-window))
                                    (current-buffer))
               (setq-local lspce--completion-complete? nil)
               (let* ((proxy (if (plist-member (text-properties-at 0 proxy) 'lspce--lsp-item)
                                 proxy
                               (cl-find proxy (funcall proxies) :test #'equal)))
                      (lsp-item (funcall resolve-maybe (get-text-property 0 'lspce--lsp-item proxy)))
                      (lsp-markers (get-text-property 0 'lspce--lsp-markers proxy))
                      (lsp-prefix (get-text-property 0 'lspce--lsp-prefix proxy))
                      (lsp-start (get-text-property 0 'lspce--lsp-start proxy))
                      (insertTextFormat (or (gethash "insertTextFormat" lsp-item) 1))
                      (insertText (gethash "insertText" lsp-item))
                      (label (gethash "label" lsp-item))
                      (textEdit (gethash "textEdit" lsp-item))
                      (additionalTextEdits (gethash "additionalTextEdits" lsp-item))
                      (snippet-fn (and (eql insertTextFormat 2)
                                       (lspce--snippet-expansion-fn))))
                 (lspce--debug "lsp-item %S" (json-encode lsp-item))
                 (cond (textEdit
                        (let* ((range (gethash "range" textEdit))
                               (newText (gethash "newText" textEdit))
                               (old-text (apply #'buffer-substring-no-properties lsp-markers))
                               (region (lspce--range-region range)))
                          (if (and (length> old-text 0)
                                   (string-prefix-p old-text newText))
                              (progn
                                (funcall (or snippet-fn #'insert) (substring newText (length old-text))))
                            (progn
                              (lspce--debug "lsp-markers: %s, markers: %s"
                                            lsp-markers
                                            (apply #'buffer-substring-no-properties lsp-markers))
                              (lspce--debug "lsp-prefix: %s" lsp-prefix)
                              (lspce--debug "region: %s, region: %s"
                                            region
                                            (buffer-substring-no-properties (car region) (cdr region)))
                              (apply #'delete-region lsp-markers)
                              (insert lsp-prefix)
                              (delete-region (car region) (cdr region))
                              (goto-char (car region))
                              (funcall (or snippet-fn #'insert) newText))))
                        (when (cl-plusp (length additionalTextEdits))
                          (lspce--apply-text-edits additionalTextEdits)))
                       (snippet-fn
                        ;; A snippet should be inserted, but using plain
                        ;; `insertText' or `label'.  This requires us to delete the
                        ;; whole completion, since `insertText' or `label' is the full
                        ;; completion's text.
                        (let* ((newText (or insertText label))
                               (old-text (apply #'buffer-substring-no-properties lsp-markers)))
                          (if (and (string-prefix-p old-text newText))
                              (progn
                                (funcall snippet-fn (substring newText (length old-text))))
                            (apply #'delete-region lsp-markers)
                            (insert lsp-prefix)
                            (delete-region lsp-start (point))
                            (funcall snippet-fn (or insertText label)))))))
               (lspce--completion-clear-cache)
               (lspce--notify-textDocument/didChange)))))))))

;;; imenu
(defun lspce-imenu-create ()
  (cl-labels
      ((unfurl (obj)
         (if-let ((children (gethash "children" obj))
                  (name (gethash "name" obj)))
             (cons obj
                   (mapcar (lambda (c)
                             (puthash
                              "containerName"
                              (let ((existing (gethash "containerName" c)))
                                (if existing (format "%s::%s" name existing)
                                  name)) c) c)
                           (mapcan #'unfurl children)))
           (list obj))))
    (when (lspce--server-capable-chain "documentSymbolProvider")
      (mapcar
       (lambda (obj)
         (cons
          (cdr (assoc (car obj) lspce--symbol-kind-names))
          (mapcar
           (lambda (obj)
             (let ((content
                    (cons (gethash "name" obj)
                          (lspce--lsp-position-to-point
                           (gethash "start"
                                    (if-let ((range (gethash "selectionRange" obj)))
                                        range
                                      (gethash "range" (gethash "location" obj)))))))
                   (container (gethash "containerName" obj)))
               (if container (list container content)
                 content)))
           (cdr obj))))
       (seq-group-by
        (lambda (obj) (gethash "kind" obj))
        (mapcan #'unfurl
                (lspce--request "textDocument/documentSymbol" (list :textDocument (lspce--textDocumentIdenfitier (lspce--uri))))))))))

;;; hover
(defvar lspce--doc-buffer-name "*lspce-hover*")
(defvar lspce--doc-max-width 100)
(defvar lspce--doc-max-height 30)
(defun lspce--display-help (kind content)
  (let* ((theme-mode (format "%s" (frame-parameter nil 'background-mode)))
         (background-color (if (string-equal theme-mode "dark")
                               "#191a1b"
                             "#f0f0f0"))
         (height 1)
         (lines 1))
    (with-current-buffer (get-buffer-create lspce--doc-buffer-name)
      (read-only-mode -1)
      (erase-buffer)
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert content)
      (if (fboundp 'gfm-view-mode)
          (let ((view-inhibit-help-message t))
            (gfm-view-mode))
        (gfm-mode))
      (font-lock-ensure)
      (save-excursion
        (goto-char (point-max))
        (setq lines (line-number-at-pos))
        (if (> lines height)
            (setq height (min lines lspce--doc-max-height)))))
    (if (and (fboundp #'posframe-workable-p)
             (posframe-workable-p)
             (fboundp #'posframe-plus-show))
        (posframe-plus-show lspce--doc-buffer-name t t
                            :position (point)
                            :timeout lspce-doc-tooltip-timeout
                            :internal-border-color "orange"
                            :internal-border-width lspce-doc-tooltip-border-width
                            :background-color background-color
                            :accept-focus nil
                            :width lspce--doc-max-width
                            :height height
                            :left-fringe 10
                            :right-fringe 10)
      (switch-to-buffer-other-window lspce--doc-buffer-name))))

(defun lspce-help-at-point ()
  "Show document of the symbol at the point using LSP's hover."
  (interactive)
  (if lspce-mode
      (let ((hover-info (lspce--hover-at-point)))
        (when hover-info
          (lspce--display-help (nth 0 hover-info) (nth 1 hover-info))))
    (user-error "Lspce mode is not enabled yet.")))

(defun lspce--line-character-to-point (line character)
  (lspce--widening
   (goto-char (point-min))
   (forward-line line)
   (-let [line-end (line-end-position)]
            (if (> character (- line-end (point)))
                line-end
              (forward-char character)
              (point)))))


;;; textDocument/documentHighlight

(defconst lspce--highlight-kind-face
  '((1 . lspce-face-highlight-textual)
    (2 . lspce-face-highlight-read)
    (3 . lspce-face-highlight-write)))

(defalias 'lspce--textDocumentHighlightParams 'lspce--textDocumentPositionParams "lspce--textDocumentHighlightParams")

(defun lspce--make-textDocumentHighlightParams ()
  (lspce--textDocumentHighlightParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--update-highlight ()
  (interactive)
  (let* ((wins-visible-pos (mapcar (lambda (win)
                                   (cons (1- (window-start win))
                                         (1+ (min (window-end win)
                                                  (with-current-buffer (window-buffer win)
                                                    (buffer-end +1))))))
                                 (get-buffer-window-list nil nil 'visible))))
    (mapcar (lambda (pos) (lspce--update-highlights-region (car pos) (cdr pos))) wins-visible-pos)))

(defun lspce--update-highlights-region (start end)
  (when (lspce--server-capable "documentHighlightProvider")
    (let ((resp (lspce--request
                 "textDocument/documentHighlight"
                 (lspce--make-textDocumentHighlightParams)))
          ;; TODO this needs correction and some optimize
          (predicate (lambda (sym-start sym-end)
                       (and (> sym-start start) (< sym-end end)))))
      (when resp
        (lspce--remove-highlight-overlays start end)
        (lspce--highlight-symbols resp predicate)))))

(defun lspce--remove-highlight-overlays (start end)
  (lspce--debug "lspce--remove-highlight-overlays from %s to %s" start end)
  (let ((ovs (overlays-in (1- start) (1+ end))))
    (dolist (o ovs)
      (when (overlay-get o 'lspce--highlight)
        (lspce--debug "old overlay: %s" o)
        (delete-overlay o)))))


(defun lspce--highlight-symbols (highlights predicate)
  (let ((items (cond ((or (arrayp highlights) (listp highlights)) highlights)
                     ((hash-table-p highlights) (list highlights)))))
    (condition-case err
        (progn
          (dolist (item items)
            (let* ((range (or (gethash "range" item) (gethash "targetSelectionRange" item)))
                   (kind? (gethash "kind" item))
                   (kind (cond ((stringp kind?) (string-to-number kind?))
                               ((numberp kind?) kind?)
                               (t 1)))
                   (start (gethash "start" range))
                   (end (gethash "end" range))
                   (start-line (gethash "line" start))
                   (start-column (gethash "character" start))
                   (end-line (gethash "line" end))
                   (end-column (gethash "character" end))
                   (start-point (lspce--line-character-to-point start-line start-column))
                   (end-point (lspce--line-character-to-point end-line end-column)))
              (when (funcall predicate start-point end-point)
                (let ((ov (make-overlay start-point end-point)))
                  (overlay-put ov 'face (cdr (assq kind lspce--highlight-kind-face)))
                  (overlay-put ov 'lspce--highlight t)
                  (overlay-put ov 'lspce--overlay t)))))))))


(defun lspce--post-command ()
  (lspce--cleanup-highlights-if-needed)
  (lspce--idle-reschedule (current-buffer)))

(defvar lspce--on-idle-timer nil)

(defun lspce--idle-reschedule (buffer)
  (when lspce--on-idle-timer
    (cancel-timer lsp--on-idle-timer))
  (setq lsp--on-idle-timer
        (run-with-idle-timer lspce-idle-delay nil #'lspce--on-idle buffer)))

(defun lspce--on-idle (buffer)
  "Start post command loop."
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer)))
    (run-hooks 'lspce-on-idle-hook)))

(defun lspce--point-on-highlight? ()
  (-some? (lambda (overlay)
            (overlay-get overlay 'lspce--highlight))
          (overlays-at (point))))

(defun lspce--cleanup-highlights-if-needed ()
  (when (and lspce-enable-symbol-highlight (not (lspce--point-on-highlight?)))
    (remove-overlays nil nil 'lspce--highlight t)))

(defun lspce--hover-at-point ()
  "Show document of the symbol at the point using LSP's hover."
  (when (and
         (not lspce--in-completion-p)
         (lspce--server-capable-chain "hoverProvider"))
    (let* ((method "textDocument/hover")
           (params (lspce--make-hoverParams))
           (response (lspce--request method params))
           contents kind content language hover-info)
      (when response
        (setq contents (gethash "contents" response))
        (cond
         ((hash-table-p contents)
          (setq kind (gethash "kind" contents))
          (setq language (gethash "language" contents))
          (setq content (gethash "value" contents))
          (when (and (null kind)
                     language)
            (setq kind "markdown"
                  content (concat "```" language "\n" content "\n```")))
          (when (and kind content)
            (setq hover-info (list kind content))))
         ((listp contents)
          (setq kind "markdown")
          (setq content nil)
          (dolist (c contents)
            (cond
             ((stringp c)
              (setq content (concat content "\n" c)))
             ((hash-table-p c)
              (setq content (concat "```" (gethash "language" c) "\n" (gethash "value" c) "\n```")))
             (t
              )))
          (when (and kind content)
            (setq hover-info (list kind content))))
         (t
          ;; nothing
          )))
      hover-info)))

(defun lspce--eldoc-render-markup (content)
  (let (mode)
    (with-temp-buffer
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert content)
      (if (fboundp 'gfm-view-mode)
          (setq mode 'gfm-view-mode)
        (setq mode 'gfm-mode))
      (let ((inhibit-message t)
            (message-log-max nil))
        (ignore-errors (delay-mode-hooks (funcall mode))))
      (font-lock-ensure)
      (mapconcat #'identity (seq-filter (lambda (s) (not (string-match-p "^```" s)))
                                        (split-string (string-trim (buffer-string)) "\n"))
                 "\n"))))

(defun lspce-eldoc-hover-function (callback)
  (when lspce-mode
    (let ((hover-info (lspce--hover-at-point))
          content)
      (when hover-info
        (setq content (lspce--eldoc-render-markup (nth 1 hover-info)))
        (funcall callback content)))))

;;; signature help

(defun lspce-signature-at-point ()
  (interactive)
  (lspce--signature-at-point))

(defun lspce--signature-at-point ()
  (let (signature-at-point
        label
        signatures signature parameters parameter
        active-signature active-parameter param-label param-start param-end)
    (when (and
           (not lspce--in-completion-p)
           (lspce--server-capable "signatureHelpProvider"))
      (let ((params (lspce--make-signatureHelpParams)))
        (setq signature-at-point (lspce--request "textDocument/signatureHelp" params 2.0))))
    (when signature-at-point
      (setq active-signature (gethash "activeSignature" signature-at-point))
      (setq signatures (gethash "signatures" signature-at-point))
      (setq active-parameter (gethash "activeParameter" signature-at-point))
      (cond
       ((and active-signature signatures)
        (setq signature (nth active-signature signatures)))
       (signatures
        (setq signature (nth 0 signatures)))
       (t))
      (when signature
        (setq label (gethash "label" signature))
        (setq parameters (gethash "parameters" signature))
        (setq active-parameter (or (gethash "activeParameter" signature) active-parameter))
        (when (and active-parameter parameters)
          (setq parameter (nth active-parameter parameters)))
        (when parameter
          (setq param-label (gethash "label" parameter))
          (when (not (stringp param-label))
            (setq param-start (nth 0 param-label))
            (setq param-end (nth 1 param-label))
            (setq param-label (substring-no-properties label param-start param-end)))
          (setq label (string-replace param-label
                                      (propertize param-label 'face 'font-lock-type-face)
                                      label)))))
    label))

(defun lspce-eldoc-signature-function (callback)
  (when lspce-mode
    (let ((signature (lspce--signature-at-point)))
      (funcall callback signature))))

(defface lspce-eldoc-backend-face
  '((((background light)) :foreground "#7F7F7F")
    (t :foreground "#9B9B9B"))
  "Face used to highlight backend name."
  :group 'lspce)

(defun lspce-eldoc-function (callback)
  (when lspce-mode
    (let ((hover-info (and lspce-eldoc-enable-hover (lspce--hover-at-point)))
          (signature (and lspce-eldoc-enable-signature (lspce--signature-at-point)))
          backend
          content
          document)
      (when hover-info
        (setq content (lspce--eldoc-render-markup (nth 1 hover-info))))
      (cond
       ((and signature content)
        (setq document (concat signature "\n\n" content)))
       ((or signature content)
        (setq document (concat signature content))))
      (when document
        (setq backend (propertize "[lspce]\n" 'face 'lspce-eldoc-backend-face))
        (funcall callback (concat backend document))))))

;;; diagnostics
(put 'lspce-note 'flymake-category 'flymake-note)
(put 'lspce-warning 'flymake-category 'flymake-warning)
(put 'lspce-error 'flymake-category 'flymake-error)

(defalias 'lspce--make-diag 'flymake-make-diagnostic)
(defalias 'lspce--diag-data 'flymake-diagnostic-data)

(defvar-local lspce--diagnostics nil
  "Flymake diagnostics for this buffer.")
(defvar-local lspce--current-flymake-report-fn nil
  "Current flymake report function for this buffer.")

(defun lspce-current-max-diagnostics-count ()
  "Read current MAX_DIAGNOSTIC_COUNT."
  (interactive)
  (lspce-module-read-max-diagnostics-count))

(defun lspce-change-max-diagnostics-count (count)
  "Change current MAX_DIAGNOSTIC_COUNT to COUND."
  (interactive (list (read-number "Max diagnostics count: ")))
  (if (integerp count)
      (lspce-module-change-max-diagnostics-count count)
    (lspce--error "Invalid count: %s" count)))

(defun lspce--diag-type (sev)
  (cond ((null sev) 'lspce-error)
        ((<= sev 1) 'lspce-error)
        ((= sev 2)  'lspce-warning)
        (t          'lspce-note)))

(defun lspce--read-diagnostics ()
  (if lspce-mode
      (let (diagnostics
            flymake-diags range start end severity msg)
        (setq diagnostics (lspce-module-read-file-diagnostics lspce--root-uri lspce--lsp-type (lspce--uri)))
        (lspce--debug "diagnostics: %S" diagnostics)
        (when diagnostics
          ;; FIXME sort according to diag-type and positon.
          (dolist (d (lspce--json-deserialize diagnostics))
            (lspce--debug "d %S" d)
            (setq range (gethash "range" d)
                  severity (gethash "severity" d)
                  msg (gethash "message" d))
            (setq start (gethash "start" range)
                  end (gethash "end" range))
            (push (flymake-make-diagnostic (current-buffer)
                                           (lspce--lsp-position-to-point start)
                                           (lspce--lsp-position-to-point end)
                                           (lspce--diag-type severity)
                                           msg `((lspce-lsp-diag . ,d))) flymake-diags)))
        flymake-diags)))

(defun lspce-flymake-backend (report-fn &rest _more)
  "A Flymake backend for Lspce."
  (let ((diagnostics (lspce--read-diagnostics)))
    (cond (lspce-mode
           (setq lspce--current-flymake-report-fn report-fn)
           (lspce--report-to-flymake diagnostics))
          (t
           (funcall report-fn nil)))))

(defun lspce--report-to-flymake (diags)
  "Internal helper for `lspce-flymake-backend'."
  (save-restriction
    (widen)
    (funcall lspce--current-flymake-report-fn diags
             ;; If the buffer hasn't changed since last
             ;; call to the report function, flymake won't
             ;; delete old diagnostics.  Using :region
             ;; keyword forces flymake to delete
             ;; them (github#159).
             :region (cons (point-min) (point-max))))
  (setq lspce--diagnostics diags))

;;; code action
(defun lspce--region-bounds ()
  "Region bounds if active, else bounds of things at point."
  (if (use-region-p) `(,(region-beginning) ,(region-end))
    (let ((boftap (bounds-of-thing-at-point 'sexp)))
      (list (car boftap) (cdr boftap)))))

(defun lspce--execute-command (command arguments)
  (if (string-equal command "java.apply.workspaceEdit")
      (progn
        (mapc #'lspce--apply-workspace-edit arguments))
    (let ((params (list :command command :arguments arguments)))
      (lspce--request "workspace/executeCommand" params))))

(defun lspce--apply-file-edits (change)
  (lspce--debug "lspce--apply-file-edits change %s" change)
  (let ((kind (gethash "kind" change))
        (options (gethash "options" change))
        uri
        filename
        new-uri new-filename
        overwrite ignoreIfExists recursive ignoreIfNotExists)
    (cond
     ((equal kind "create")
      (setq uri (gethash "uri" change))
      (setq filename (lspce--uri-to-path uri))
      (when options
        (setq overwrite (gethash "overwrite" options)
              ignoreIfExists (gethash "ignoreIfExists" options)))
      (if (file-exists-p filename)
          (if (or overwrite
                  (not ignoreIfExists))
              (progn
                (when (find-buffer-visiting filename)
                  (with-current-buffer (find-buffer-visiting filename)
                    (save-buffer)
                    (kill-buffer)))
                (delete-file filename t)
                (with-current-buffer (find-file-noselect filename)
                  (save-buffer)))
            (lspce--warn "Cannot create file %s." filename))
        (when (find-buffer-visiting filename)
          (with-current-buffer (find-buffer-visiting filename)
            (save-buffer)
            (kill-buffer)))
        (delete-file filename t)
        (with-current-buffer (find-file-noselect new-filename)
          (save-buffer))))
     ((equal kind "rename")
      (setq uri (gethash "oldUri" change))
      (setq filename (lspce--uri-to-path uri))
      (setq new-uri (gethash "newUri" change))
      (setq new-filename (lspce--uri-to-path new-uri))
      (when options
        (setq overwrite (gethash "overwrite" options)
              ignoreIfExists (gethash "ignoreIfExists" options)))
      (if (file-exists-p new-filename)
          (if (or overwrite
                  (not ignoreIfExists))
              (progn
                (when (find-buffer-visiting filename)
                  (with-current-buffer (find-buffer-visiting filename)
                    (save-buffer)
                    (kill-buffer)))
                (when (find-buffer-visiting new-filename)
                  (with-current-buffer (find-buffer-visiting new-filename)
                    (save-buffer)
                    (kill-buffer)))
                (rename-file filename new-filename t))
            (lspce--warn "Cannot rename %s to %s" filename new-filename))
        (if (find-buffer-visiting filename) ;; new filename not existing
            (progn
              (with-current-buffer (find-buffer-visiting filename)
                (save-buffer)
                (kill-buffer))
              (rename-file filename new-filename t)
              (find-file new-filename))
          (rename-file filename new-filename t))))
     ((equal kind "delete")
      (setq uri (gethash "uri" change))
      (setq filename (lspce--uri-to-path uri))
      (when options
        (setq recursive (gethash "recursive" options)
              ignoreIfNotExists (gethash "ignoreIfNotExists" options)))
      (when (file-exists-p filename)
        (if (file-directory-p filename)
            (progn
              (if recursive
                  (progn
                    (dolist (buf (buffer-list))
                      (with-current-buffer buf
                        (when (and buffer-file-name
                                   (f-parent-of-p filename buffer-file-name))
                          (save-buffer)
                          (kill-buffer))))
                    (delete-directory filename t t))
                (lspce--warn "Cannot delete directory %s" filename)))
          (if (find-buffer-visiting filename)
              (with-current-buffer (find-buffer-visiting filename)
                (save-buffer)
                (kill-buffer)))
          (delete-file filename t)))))))

(defun lspce--apply-text-edits (edits &optional version)
  (lspce--debug "buffer %s, version %s, edits: %s" (buffer-name) version (json-encode edits))
  (when (or (not version)
            (equal version :null)
            (equal version lspce--identifier-version))
    (atomic-change-group
      (let* ((change-group (prepare-change-group)))
        (mapc (lambda (edit)
                (let* ((source (current-buffer))
                       (newText (car edit))
                       (range (cdr edit))
                       (start (car range))
                       (end (cdr range)))
                  (with-temp-buffer
                    (insert newText)
                    (let ((temp (current-buffer)))
                      (with-current-buffer source
                        (save-excursion
                          (save-restriction
                            (narrow-to-region start end)
                            (let ((inhibit-modification-hooks t)
                                  (length (- end start))
                                  (start (marker-position start))
                                  (end (marker-position end)))
                              (run-hook-with-args 'before-change-functions
                                                  start end)
                              (replace-buffer-contents temp)
                              (run-hook-with-args 'after-change-functions
                                                  start (+ start (length newText))
                                                  length))
                            (run-hook-with-args 'lspce-after-each-text-edit-hook
                                                (marker-position start)
                                                (+ (marker-position start) (length newText))))))))))
              (mapcar (lambda (edit)
                        (let* ((newText (gethash "newText" edit))
                               (range (lspce--range-region (gethash "range" edit) t)))
                          (cons newText range)))
                      (nreverse edits)))
        (undo-amalgamate-change-group change-group)))
    (run-hooks 'lspce-after-text-edit-hook)))

(defun lspce--apply-workspace-edit (wedit &optional confirm)
  (let ((changes (gethash "changes" wedit))
        (documentChanges (gethash "documentChanges" wedit))
        (confirmed t)
        kind edits all-edits)
    (if documentChanges
        (progn
          (dolist (dc documentChanges)
            (setq kind (gethash "kind" dc))
            (if kind
                (cl-pushnew (make-lspce-documentChange :kind kind :change dc) all-edits)
              (cl-pushnew (make-lspce-documentChange :kind "documentChange" :change dc) all-edits))))
      (when changes
        (let (change)
          (dolist (filename (hash-table-keys changes))
            (setq edits (gethash filename changes))
            (setq change (make-hash-table :test #'equal))
            (puthash "uri" filename change)
            (puthash "edits" edits change)
            (cl-pushnew (make-lspce-documentChange :kind "change" :change change) all-edits)))))
    (setq all-edits (reverse all-edits))
    (when confirm
      (if (length> all-edits 0)
          (unless (y-or-n-p
                   (format "[lspce] Server wants to:\n %s\n Proceed? "
                           (mapconcat #'identity
                                      (mapcar
                                       (lambda (edit)
                                         (let ((kind (lspce-documentChange-kind edit))
                                               (change (lspce-documentChange-change edit)))
                                           (cond
                                            ((equal kind "change")
                                             (format "edit %s" (gethash "uri" change)))
                                            ((equal kind "documentChange")
                                             (format "edit %s" (gethash "uri" (gethash "textDocument" change))))
                                            ((equal kind "rename")
                                             (format "rename %s to %s"
                                                     (gethash "oldUri" change)
                                                     (gethash "newUri" change)))
                                            ((equal kind "delete")
                                             (format "delete %s" (gethash "uri" change)))
                                            ((equal kind "create")
                                             (format "create %s" (gethash "uri" change))))))
                                       all-edits)
                                      "\n ")))
            (setq confirmed nil)
            (lspce--info "User cancelled server edit"))
        (lspce--info "No edits to apply")
        (setq confirmed nil)))
    (when (and confirmed
               (length> all-edits 0))
      (let (change
            kind
            textDocument filename edits version)
        (dolist (aedits all-edits)
          (setq change (lspce-documentChange-change aedits))
          (setq kind (lspce-documentChange-kind aedits))
          (cond
           ((equal kind "change")
            (setq filename (lspce--uri-to-path (gethash "uri" change))
                  edits (gethash "edits" change)
                  version nil)
            (with-current-buffer (find-file-noselect filename)
              (lspce--debug "lspce--apply-text-edit filename %s" filename)
              (lspce--apply-text-edits edits version)))
           ((equal kind "documentChange")
            (setq textDocument (gethash "textDocument" change)
                  edits (gethash "edits" change))
            (setq filename (lspce--uri-to-path (gethash "uri" textDocument))
                  version (gethash "version" textDocument))
            (with-current-buffer (find-file-noselect filename)
              (lspce--debug "lspce--apply-text-edit filename %s" filename)
              (lspce--apply-text-edits edits version)))
           (t
            (lspce--debug "lspce--apply-file-edits filename %s" filename)
            (lspce--apply-file-edits change))))))))

(cl-defun lspce-code-actions (beg &optional end action-kind)
  "Offer to execute actions of ACTION-KIND between BEG and END.
If ACTION-KIND is nil, consider all kinds of actions.
Interactively, default BEG and END to region's bounds else BEG is
point and END is nil, which results in a request for code actions
at point.  With prefix argument, prompt for ACTION-KIND."
  (interactive
   `(,@(lspce--region-bounds)
     ,(and current-prefix-arg
           (completing-read "[lspce] Action kind: "
                            '("quickfix" "refactor.extract" "refactor.inline"
                              "refactor.rewrite" "source.organizeImports")))))
  (unless (lspce--server-capable "codeActionProvider")
    (lspce--warn "Server can't execute code actions!")
    (cl-return-from lspce-code-actions nil))

  (let ((actions (lspce--request "textDocument/codeAction" (lspce--make-codeActionParams beg end action-kind)))
        candidates preferred-action default-action selected-action
        kind title preferred)
    (if actions
        (progn
          (dolist (action actions)
            (setq kind (gethash "kind" action)
                  title (gethash "title" action)
                  preferred (gethash "isPreferred" action))
            (when (or (not action-kind)
                      (string-equal action-kind kind))
              (cl-pushnew (cons title action) candidates))
            (when preferred
              (setq preferred-action (cons title action))))
          (setq default-action (or preferred-action (car candidates)))
          (setq selected-action (cdr (assoc (completing-read
                                             (format "[lspce] Pick an action (default %s): "
                                                     (car default-action))
                                             candidates nil t nil nil default-action)
                                            candidates)))
          (when selected-action
            (let* ((command (gethash "command" selected-action))
                   (edit (gethash "edit" selected-action)))
              (when edit
                (lspce--apply-workspace-edit edit))
              (when command
                (lspce--execute-command (gethash "command" command) (gethash "arguments" command))))))
      (lspce--info "No code actions here."))))

;;; rename
(defun lspce--make-renameParams (newname)
  (lspce--renameParams (lspce--textDocumentIdenfitier (lspce--uri))
                       (lspce--make-position)
                       newname))

(defun lspce-rename (newname)
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (if (lspce--server-capable-chain "renameProvider")
      (let ((response (lspce--request "textDocument/rename" (lspce--make-renameParams newname))))
        (when response
          (lspce--apply-workspace-edit response t)))
    (lspce--warn "Server does not support rename.")
    nil))

(defun lspce--format-lsp-position (position)
  (let ((line (gethash "line" position))
        (character (gethash "character" position)))
    (format "line: %d, character %d" line character)))

(defvar-local lspce--flymake-already-enabled nil
  "Whether flymake is enabled before lspce starting.")
(defvar-local lspce--eldoc-already-enabled nil
  "Whether eldoc is enabled before lspce starting.")

;;; lspce mode

(defvar lspce-mode-map (make-sparse-keymap))

(defvar lspce-server-programs
  `(("rust"  "rust-analyzer" "")
    ("python" "jedi-language-server" "")
    ("python" "pylsp" "")
    ("C" "clangd" "")
    ("sh" "bash-language-server" "start")
    ("go" "gopls" "")
    ("typescript" "typescript-language-server" "--stdio")
    ("js" "typescript-language-server" "--stdio"))
  "How the command `lspce' gets the server to start. For a given buffer,
if there are multiple servers available, you can choose one interactively.

A list of (LSP-TYPE SERVER-COMMAND SERVER-PARAMS initializationOptions).

LSP-TYPE identifies the buffers that are to be managed by a specific
language server, it is returned by `lspce-lsp-type-function'.

SERVER-COMMAND specifies which server is used for those buffers.

SERVER-PARAMS can be:

* nil, in this case, no param is required to start the lsp server;

* In the most common case, a string such as --stdio;

* A function that returns the params;

initializationOptions can be:
* nil, means no options;

* A function that returns the options used to in initialize request;
")

(defun lspce--server-program (lsp-type)
  (let ((programs nil))
    (cl-dolist (p lspce-server-programs)
      (when (string-equal (car p) lsp-type)
        (cl-pushnew (cdr p) programs)))
    programs))

(defun lspce--choose-server (servers)
  (let ((server-ht (make-hash-table :test #'equal))
        chosen)
    (dolist (s servers)
      (puthash (format "%s" s) s server-ht))
    (setq chosen (completing-read "Choose one server: "
                                  (hash-table-keys server-ht)))
    (when chosen
      (gethash chosen server-ht))))

(cl-defun lspce--buffer-enable-lsp ()
  (let ((root-uri (lspce--root-uri))
        (lsp-type (lspce--lsp-type))
        servers server server-cmd server-args initialize-options
        server-info server-key server-managed-buffers)
    (unless (and root-uri lsp-type)
      (lspce--warn "Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--buffer-enable-lsp nil))

    (setq-local lspce--root-uri root-uri)
    (setq-local lspce--lsp-type lsp-type)
    (setq-local lspce--recent-changes nil
                lspce--identifier-version 0
                lspce--uri (lspce--path-to-uri buffer-file-name))

    (setq server-info (lspce--connect-existing-server root-uri lsp-type))
    (unless server-info
      (setq servers (lspce--server-program lsp-type))
      (lspce--debug "servers: %s" servers)
      (unless servers
        (user-error "lspce--buffer-enable-lsp: Server not found for type %s." lsp-type)
        (cl-return-from lspce--buffer-enable-lsp nil))

      (if (length= servers 1)
          (setq server (nth 0 servers))
        (setq server (lspce--choose-server servers)))
      (unless server
        (user-error "lspce--buffer-enable-lsp: No valid server.")
        (cl-return-from lspce--buffer-enable-lsp nil))

      (lspce--debug "server %S" server)
      (setq server-cmd (nth 0 server)
            server-args (nth 1 server)
            initialize-options (nth 2 server))
      (when (functionp server-cmd)
        (setq server-cmd (funcall server-cmd)))
      (unless server-cmd
        (user-error "lspce--buffer-enable-lsp: Can not find lsp server progrom.")
        (cl-return-from lspce--buffer-enable-lsp nil))
      (setq server-cmd (or (executable-find server-cmd)
                           (user-error "Cannot find `%s' using `executable-find'"
                                       server-cmd)))
      (when (functionp server-args)
        (setq server-args (funcall server-args)))
      (unless server-args
        (setq server-args ""))
      (when (and initialize-options
                 (functionp initialize-options))
        (setq initialize-options (funcall initialize-options)))

      (lspce--debug "server-cmd: %s" server-cmd)
      (lspce--debug "server-args: %s" server-args)
      (lspce--debug "initialize-options: %s" initialize-options)

      (setq server-info (lspce--connect root-uri lsp-type server-cmd server-args initialize-options)))
    (lspce--debug "server-info: %s" server-info)
    (unless server-info
      (cl-return-from lspce--buffer-enable-lsp nil))

    (when (lspce--notify-textDocument/didOpen)
      (setq-local lspce--server-info (lspce--json-deserialize server-info))
      (if-let (capabilities (gethash "capabilities" lspce--server-info))
          (progn
            (setq lspce--server-capabilities
                  (lspce--json-deserialize capabilities)))
        (setq lspce--server-capabilities (make-hash-table :test #'equal)))
      (setq server-key
            (make-lspce--hash-key :root-uri root-uri :lsp-type lsp-type))
      (setq server-managed-buffers (gethash server-key lspce--managed-buffers))
      (unless server-managed-buffers
        (setq server-managed-buffers (make-hash-table :test 'equal)))
      (puthash lspce--uri t server-managed-buffers)
      (puthash server-key server-managed-buffers lspce--managed-buffers))))

(cl-defun lspce--buffer-disable-lsp ()
  (when (and lspce--root-uri
             lspce--lsp-type)
    (let (server-key server-managed-buffers)
      (setq server-key
            (make-lspce--hash-key :root-uri lspce--root-uri
                                  :lsp-type lspce--lsp-type))
      (setq server-managed-buffers (gethash server-key lspce--managed-buffers))
      (when server-managed-buffers
        (remhash (lspce--uri) server-managed-buffers)
        (when (= (hash-table-count server-managed-buffers) 0)
          (lspce--shutdown))
        (puthash server-key server-managed-buffers lspce--managed-buffers)
        
        (setq-local lspce--server-info nil)
        (setq-local lspce--root-uri nil)
        (setq-local lspce--lsp-type nil)
        (setq-local lspce--recent-changes nil
                    lspce--identifier-version 0
                    lspce--uri nil)))))

(defun lspce--server-id (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when (and
           lspce--server-info
           (hash-table-p lspce--server-info))
      (gethash "id" lspce--server-info))))

(defun lspce--kill-buffer-hook ()
  (when (and buffer-file-name
             lspce-mode)
    (lspce--notify-textDocument/didClose)
    (lspce--buffer-disable-lsp)))

(cl-defstruct lspce--hash-key
  (root-uri)
  (lsp-type))

(defun lspce--hash-key-test-fn (k1 k2)
  (and (string-equal (lspce--hash-key-root-uri k1)
                     (lspce--hash-key-root-uri k2))
       (string-equal (lspce--hash-key-lsp-type k1)
                     (lspce--hash-key-lsp-type k2))))

(defun lspce--hash-key-hash-fn (k)
  (let ((root-uri (lspce--hash-key-root-uri k))
        (lsp-type (lspce--hash-key-lsp-type k))
        (hash 0))
    (seq-do (lambda (c)
              (setq hash (+ (* 31 hash) c))
              (setq hash (% hash (max-char))))
            (concat root-uri lsp-type))
    hash))

(define-hash-table-test 'lspce--hash-equal
                        'lspce--hash-key-test-fn
                        'lspce--hash-key-hash-fn)

(defvar lspce--managed-buffers (make-hash-table :test 'lspce--hash-equal)
  "All files that enable lspce-mode. The key is `lspce--hash-key'.
The value is also a hash table, with uri as the key and the value is just t.")

;;; Hooks
(defvar-local lspce--recent-changes nil
  "Recent buffer changes as collected by `lspce--before-change'.")

(defvar-local lspce--change-idle-timer nil "Idle timer for didChange signals.")

(defvar-local lspce--identifier-version 0
  "The version number of this document
(it will increase after each change, including undo/redo).")

(defun lspce--before-change (beg end)
  "Hook onto `before-change-functions' with BEG and END."
  (when (listp lspce--recent-changes)
    ;; Records BEG and END, crucially convert them into LSP
    ;; (line/char) positions before that information is lost (because
    ;; the after-change thingy doesn't know if newlines were
    ;; deleted/added).  Also record markers of BEG and END
    ;; (github#259)
    (push `(,(lspce--pos-to-lsp-position beg)
            ,(lspce--pos-to-lsp-position end)
            (,beg . ,(copy-marker beg nil))
            (,end . ,(copy-marker end t)))
          lspce--recent-changes)))

(defun lspce--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf lspce--identifier-version)
  (pcase (and (listp lspce--recent-changes)
              (car lspce--recent-changes))
    (`(,lsp-beg ,lsp-end
                (,b-beg . ,b-beg-marker)
                (,b-end . ,b-end-marker))
     (if (and (= b-end b-end-marker) (= b-beg b-beg-marker)
              (or (/= beg b-beg) (/= end b-end)))
         (setcar lspce--recent-changes
                 `(,lsp-beg ,lsp-end ,(- b-end-marker b-beg-marker)
                            ,(buffer-substring-no-properties b-beg-marker
                                                             b-end-marker)))
       (setcar lspce--recent-changes
               `(,lsp-beg ,lsp-end ,pre-change-length
                          ,(buffer-substring-no-properties beg end)))))
    (_ (setf lspce--recent-changes :emacs-messup)))
  (when lspce--change-idle-timer (cancel-timer lspce--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq lspce--change-idle-timer
          (run-with-idle-timer
           lspce-send-changes-idle-time
           nil (lambda () (lspce--when-live-buffer buf
                            (when lspce-mode
                              (lspce--notify-textDocument/didChange)
                              (setq lspce--change-idle-timer nil))))))))

(defun lspce--before-revert-hook ()
  (lspce--notify-textDocument/didClose))

(defun lspce--after-revert-hook ()
  "Lspce's `after-revert-hook'."
  (lspce--notify-textDocument/didOpen))


;; TODO add kill-emacs-hook to kill all lsp servers.
;;;###autoload
(define-minor-mode lspce-mode
  "Mode for source buffers managed by some LSPCE project."
  :init-value nil :lighter nil :keymap lspce-mode-map
  (cond
   (lspce-mode
    (cond
     ((not buffer-file-name)
      (lspce--warn "Lspce can not be used in non-file buffers.")
      (setq lspce-mode nil))
     (t
      (add-hook 'after-change-functions 'lspce--after-change nil t)
      (add-hook 'before-change-functions 'lspce--before-change nil t)
      (add-hook 'kill-buffer-hook 'lspce--kill-buffer-hook nil t)
      (add-hook 'before-revert-hook 'lspce--before-revert-hook nil t)
      (add-hook 'after-revert-hook 'lspce--after-revert-hook nil t)
      (add-hook 'xref-backend-functions 'lspce-xref-backend nil t)
      (add-hook 'completion-at-point-functions 'lspce-completion-at-point nil t)
      (add-hook 'pre-command-hook 'lspce--pre-command-hook nil t)
      (add-hook 'post-self-insert-hook 'lspce--post-self-insert-hook nil t)
      (add-hook 'before-save-hook 'lspce--notify-textDocument/willSave nil t)
      (add-hook 'before-save-hook 'lspce--request-textDocument/willSaveWaitUntil nil t)
      (add-hook 'after-save-hook 'lspce--notify-textDocument/didSave nil t)
      (add-hook 'post-command-hook #'lspce--post-command nil t)
      (when lspce-enable-symbol-highlight
        (add-hook 'lspce-on-idle-hook #'lspce--update-highlight))
      (when lspce-enable-eldoc
        (when eldoc-mode
          (setq-local lspce--eldoc-already-enabled t))
        (add-hook 'eldoc-documentation-functions #'lspce-eldoc-function nil t)
        (eldoc-mode 1))
      (when lspce-enable-flymake
        (when flymake-mode
          (setq-local lspce--flymake-already-enabled t))
        (lspce--save-and-rebind flymake-diagnostic-functions
                                '(lspce-flymake-backend))
        ;; postpone flymake check since there is no diagnostic yet
        (setq-local flymake-start-on-flymake-mode nil)
        (flymake-mode 1))
      (when lspce-enable-imenu-index-function
        (lspce--save-and-rebind imenu-create-index-function
                                'lspce-imenu-create))
      (condition-case err
          (progn
            (lspce--refresh-log-level)
            (lspce--buffer-enable-lsp)
            (if lspce--server-info
                (lspce--info "Connected to lsp server.")
              (lspce--warn "Failed to connect to lsp server.")
              (lspce-mode -1)))
        ((error user-error quit)
         (lspce--error "lspce-mode enable: error [%s]" err)
         (lspce-mode -1)))

      (when lspce-enable-flymake
        ;; if diagnostics configured, then server will send `publishDiagnostics'
        ;; notification after receiving a `didOpen' one. So, wait a bit more to
        ;; let it arrive and be checked synchroniously by flymake. 
        (run-with-timer lspce-idle-delay nil #'flymake-start t)))))
   (t
    (remove-hook 'after-change-functions 'lspce--after-change t)
    (remove-hook 'before-change-functions 'lspce--before-change t)
    (remove-hook 'kill-buffer-hook 'lspce--kill-buffer-hook t)
    (remove-hook 'before-revert-hook 'lspce--before-revert-hook t)
    (remove-hook 'after-revert-hook 'lspce--after-revert-hook t)
    (remove-hook 'xref-backend-functions 'lspce-xref-backend t)
    (remove-hook 'completion-at-point-functions #'lspce-completion-at-point t)
    (remove-hook 'pre-command-hook 'lspce--pre-command-hook t)
    (remove-hook 'post-self-insert-hook 'lspce--post-self-insert-hook t)
    (remove-hook 'before-save-hook 'lspce--notify-textDocument/willSave t)
    (remove-hook 'before-save-hook 'lspce--request-textDocument/willSaveWaitUntil t)
    (remove-hook 'after-save-hook 'lspce--notify-textDocument/didSave t)
    (remove-hook 'flymake-diagnostic-functions 'lspce-flymake-backend t)
    (remove-hook 'eldoc-documentation-functions #'lspce-eldoc-function t)
    (remove-hook 'post-command-hook #'lspce--post-command t)
    (when lspce-enable-symbol-highlight
      (remove-hook 'lspce-on-idle-hook #'lspce--update-highlight))
    (when lspce--server-info
      (lspce--notify-textDocument/didClose))
    (when lspce-enable-flymake
      (if (not lspce--flymake-already-enabled)
          (flymake-mode -1)
        ;; Extract the overlay from each diagnostic before deletion
        ;; This prevents "wrong-type-argument overlayp" errors
        (mapc (lambda (diag)
                (when-let ((overlay (flymake--diag-overlay diag)))
                  (delete-overlay overlay)))
              (flymake-diagnostics))))
    (when (and lspce-enable-eldoc
               (not lspce--eldoc-already-enabled))
      (eldoc-mode -1))
    (lspce-inlay-hints-mode -1)
    (lspce--remove-overlays)
    (lspce--buffer-disable-lsp)
    (lspce--restore-bindings))))

;; auto enable lspce-mode for files
;; when some files in its project has enabled lspce-mode.
(cl-defun lspce-enable-within-project ()
  (when (and lspce-auto-enable-within-project
             buffer-file-name
             (file-exists-p buffer-file-name))
    (let ((root-uri (lspce--root-uri))
          (lsp-type (lspce--lsp-type))
          lsp-server)
      (unless (and root-uri lsp-type)
        (cl-return-from lspce-enable-within-project nil))
      
      (setq lsp-server (lspce-module-server root-uri lsp-type))
      (when lsp-server
        (lspce--info
         "lspce-enable-after-save: Server for (%s %s) is running."
         root-uri lsp-type)
        (lspce-mode t)))))
(add-hook 'find-file-hook #'lspce-enable-within-project)

;; auto enable lspce-mode for files newly created
;; when some files in its project has enabled lspce-mode.
(cl-defun lspce-enable-after-save ()
  (when (and
         (not lspce-mode)
         buffer-file-name
         (file-exists-p buffer-file-name))
    (let ((root-uri (lspce--root-uri))
          (lsp-type (lspce--lsp-type))
          lsp-server)
      (unless (and root-uri lsp-type)
        (cl-return-from lspce-enable-after-save nil))
      
      (setq lsp-server (lspce-module-server root-uri lsp-type))
      (when lsp-server
        (lspce--info
         "lspce-enable-after-save: Server for (%s %s) is running."
         root-uri lsp-type)
        (lspce-mode t)))))
(add-hook 'after-save-hook #'lspce-enable-after-save)

;;; workspace server
(defun lspce-server-info ()
  (interactive)
  lspce--server-info)

(defun lspce-shutdown-server ()
  "Shutdown server running in current buffer."
  (interactive)
  (let (server-id
        server-buffers)
    (setq server-id (lspce--server-id (current-buffer)))
    (lspce--debug "server-id %S" server-id)
    (if server-id
        (progn
          (cl-dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (string-equal server-id (lspce--server-id buf))
                (cl-pushnew buf server-buffers))))
          (cl-dolist (buf server-buffers)
            (with-current-buffer buf
              (lspce-mode -1))))
      (lspce--warn "No server running in current buffer"))))

(defun lspce-restart-server ()
  "Restart server running in current buffer."
  (interactive)
  (let (server-id
        server-buffers)
    (setq server-id (lspce--server-id (current-buffer)))
    (lspce--debug "server-id %S" server-id)
    (if server-id
        (progn
          (cl-dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (string-equal server-id (lspce--server-id buf))
                (cl-pushnew buf server-buffers))))
          (cl-dolist (buf server-buffers)
            (lspce--info "lspce-restart-server disable lspce for buf %s" (buffer-name buf))
            (with-current-buffer buf
              (lspce-mode -1)))
          (while (= lspce--shutdown-status 1)
            (sleep-for 0.005))
          (cl-dolist (buf server-buffers)
            (lspce--info "lspce-restart-server enable lspce for buf %s" (buffer-name buf))
            (with-current-buffer buf
              (lspce-mode 1))))
      (lspce--warn "No server running in current buffer"))))

(defun lspce-restart-workspace ()
  "Restart all server in current project."
  (interactive)
  (let (server-id
        root-uri
        server-buffers)
    (setq server-id (lspce--server-id (current-buffer)))
    (setq root-uri (lspce--root-uri))
    (if server-id
        (progn
          (cl-dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (and lspce--server-info
                         (string-equal root-uri (lspce--root-uri)))
                (cl-pushnew buf server-buffers))))
          (cl-dolist (buf server-buffers)
            (with-current-buffer buf
              (lspce-mode -1)))
          (while (= lspce--shutdown-status 1)
            (sleep-for 0.005))
          (cl-dolist (buf server-buffers)
            (with-current-buffer buf
              (lspce-mode 1))))
      (lspce--info "No sever running in current buffer"))))

(defvar lspce--shutdown-status 0)
(defun lspce--shutdown ()
  (let ((root-uri lspce--root-uri)
        (lsp-type lspce--lsp-type))
    (unless (and root-uri lsp-type)
      (user-error "lspce--shutdown: Can not get root-uri or lsp-type.")
      (cl-return-from lspce--shutdown nil))

    (setq lspce--shutdown-status 1)
    (setq lspce--latest-tick (lspce--current-tick))
    (make-thread (lambda ()
                   (ignore-errors
                     (lspce--info "shutdown server %s %s" root-uri lsp-type)
                     (lspce-module-shutdown
                      root-uri
                      lsp-type
                      (json-encode
                       (lspce--make-request "shutdown" nil lspce--latest-tick))))
                   (setq lspce--shutdown-status 0)))))

;;; Specific features
;;;; Java jdtls
(defun lspce--jdtls-update-project-configuration ()
  "Updates the Java configuration, refreshing settings from build artifacts"
  (when (and 
         buffer-file-name
         (file-exists-p buffer-file-name))
    (let ((file-name (file-name-nondirectory (buffer-file-name)))
          (root-uri (lspce--root-uri))
          ;; TODO use a more general mothod
          (lsp-type "java"))
      (when (and root-uri
                 lsp-type
                 (or (string-equal file-name "pom.xml")
                     (string-match-p "\\.gradle" file-name))
                 (lspce-module-server root-uri lsp-type))
        (lspce--info "send java/projectConfigurationUpdate")
        (lspce--notify
         "java/projectConfigurationUpdate"
         (list :textDocument (lspce--textDocumentIdenfitier (lspce--uri))) root-uri lsp-type)))))
(add-hook 'after-save-hook #'lspce--jdtls-update-project-configuration)

;;;###autoload
(defun lspce-jdtls-update-project-configuration ()
  "Updates the Java configuration, refreshing settings from build artifacts"
  (interactive)
  (lspce--jdtls-update-project-configuration))

;;;###autoload
(defun lspce-jdtls-reset-project ()
  "Updates the Java configuration, refreshing settings from build artifacts"
  (interactive)
  (let* ((project-current (project-current))
         (jdtls-flag nil)
         project-root)
    (when project-current
      (setq project-root (project-root project-current))
      (let ((default-directory project-root))
        (cl-dolist (f '(".project" ".classpath" ".factorypath"))
          (when (file-exists-p f)
            (setq jdtls-flag t))))
      (when jdtls-flag
        (let ((root-uri (lspce--root-uri))
              (lsp-type (lspce--lsp-type)))
          (if (and root-uri lsp-type
                   (lspce-module-server root-uri lsp-type))
              (progn
                (let ((default-directory project-root))
                  (cl-dolist (f '(".project" ".classpath" ".factorypath"))
                    (when (file-exists-p f)
                      (delete-file f t))))            
                (call-interactively #'lspce-restart-server))
            (lspce--warn "lspce-jdtls-reset-project - No server running in current server.")))))))

;;; Inlay hints mode
(defface lspce-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays.")

(defvar-local lspce--inlay-hints-timer nil)

(defun lspce--jit-lock-update-inlay-hints (start end)
  (when lspce--inlay-hints-timer
    (cancel-timer lspce--inlay-hints-timer))
  (setq lspce--inlay-hints-timer
        (run-at-time 0 nil
                     (lambda ()
                       (lspce--when-live-buffer (current-buffer)
                         (lspce--jit-lock-do-update-hints start end)))))
  (setq lspce--inlay-hints-timer nil))

(defun lspce--inlay-hint-label-text (label)
  (cond
   ((stringp label)
    label)
   ((listp label)
    (mapconcat (lambda (l)
                 (gethash "value" l))
               label ""))
   (t "")))

(defun lspce--remove-inlay-hint-overlays (start end)
  (lspce--debug "lspce--remove-inlay-hint-overlays from %s to %s" start end)
  (let ((ovs (overlays-in (1- start) (1+ end))))
    (dolist (o ovs)
      (when (overlay-get o 'lspce--inlay-hint)
        (lspce--debug "old overlay: %s" o)
        (delete-overlay o)))))

(defun lspce--jit-lock-do-update-hints (start end)
  (let* ((buf (current-buffer))
         (hint-index 0)
         (hints))
    (lspce--widening
     (lspce--when-live-buffer buf
       (lspce--debug
        "lspce--jit-lock-do-update-hints start %s, end %s" start end)
       (setq hints (lspce--request-inlay-hints start end))
       (when hints
         (lspce--remove-inlay-hint-overlays start end)
         (dolist (hint hints)
           (let* ((pos (gethash "position" hint))
                  (label (gethash "label" hint))
                  (kind (gethash "kind" hint))
                  (paddingLeft (gethash "paddingLeft" hint))
                  (paddingRight (gethash "paddingRight" hint))
                  (hint-after-p (eql kind 1))
                  (hint-point (lspce--lsp-position-to-point pos))
                  (left-pad )
                  (right-pad )
                  ov text)
             (when (and (>= hint-point start) (<= hint-point end))
               (goto-char hint-point)
               (setq left-pad (and paddingLeft
                                   (not (eq paddingLeft :json-false))
                                   (not (memq (char-before) '(32 9)))
                                   " "))
               (setq right-pad (and paddingRight
                                    (not (eq paddingRight :json-false))
                                    (not (memq (char-after) '(32 9)))
                                    " "))
               (setq ov
                     (if hint-after-p
                         (make-overlay (point) (1+ (point)) nil t)
                       (make-overlay (1- (point)) (point) nil nil nil)))
               (lspce--debug "new overlay %s" ov)
               (setq text (concat left-pad
                                  (lspce--inlay-hint-label-text label)
                                  right-pad))
               (when (and hint-after-p
                          (= hint-index 0)
                          (length> text 0))
                 (put-text-property 0 1 'cursor 1 text))
               (overlay-put ov (if hint-after-p 'before-string 'after-string)
                            (propertize
                             text
                             'face 'lspce-inlay-hint-face))
               (overlay-put ov 'lspce--inlay-hint t)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'lspce--overlay t)

               (setq hint-index (1+ hint-index))))))))))

(cl-defun lspce--request-inlay-hints (start end)
  (let* ((method "textDocument/inlayHint")
         (params (lspce--make-inlayHintsParams start end))
         (response (lspce--request method params))
         inlayHints)
    (unless response
      (lspce--debug "lspce--request-inlay-hints failed to get response")
      (cl-return-from lspce--request-inlay-hints nil))

    (lspce--debug "lspce--request-inlay-hints response: %S" response)
    (lspce--debug "lspce--request-inlay-hints response type-of: %s"
                  (type-of response))
    (cond
     ((listp response)
      (setq inlayHints response))
     (t
      (cl-return-from lspce--request-inlay-hints nil)))
    inlayHints))

(define-minor-mode lspce-inlay-hints-mode
  "Minor mode to show inlay hints."
  :global nil
  (cond
   (lspce-inlay-hints-mode
    (if (and lspce-mode
             (or (lspce--server-capable "inlayHintProvider")
                 (lspce--server-capable-chain "inlayHintProvider"
                                              "resolveProvider")))
        (jit-lock-register #'lspce--jit-lock-update-inlay-hints t)
      (lspce-inlay-hints-mode -1)))
   (t
    (jit-lock-unregister #'lspce--jit-lock-update-inlay-hints)
    (remove-overlays nil nil 'lspce--inlay-hint t))))

;;; Mode-line
;;;
(defvar lspce--mode-line-format `(:eval (lspce--mode-line-format)))

(put 'lspce--mode-line-format 'risky-local-variable t)

(defun lspce--mode-line-format ()
  "Compose the LSPCE's mode-line."
  (let* ((server lspce--server-info)
         id)
    (when server
      (setq id (gethash "id" server))
      (propertize (concat "lspce:"
                          id
                          (when lspce-show-log-level-in-modeline
                            (concat ":" lspce--log-level-text)))
                  'face 'lspce-mode-line))))

(add-to-list 'mode-line-misc-info
             `(lspce-mode (" [" lspce--mode-line-format "] ")))


(provide 'lspce)
