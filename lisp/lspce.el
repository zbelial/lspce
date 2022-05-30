;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-


;;; Require
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-util)
(require 'compile)                      ; for some faces
(require 'warnings)
(require 'posframe-plus)
(require 'markdown-mode)
(require 'flymake)

(require 'lspce-util)
(require 'lspce-types)
(require 'lspce-lang-options)

;;; User tweakable stuff
(defgroup lspce nil
  "Interaction with Language Server Protocol servers"
  :prefix "lspce-"
  :group 'applications)

(defface lspce-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in LSPCE's mode line.")


(defcustom lspce-send-changes-idle-time 0.5
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :type 'number)

(defcustom lspce-doc-tooltip-border-width 1
  "The border width of lspce tooltip, default is 1 px."
  :type 'integer)

(defcustom lspce-doc-tooltip-timeout 30
  "The timeout of lspce tooltip show time, in seconds."
  :type 'integer)

(defcustom lspce-doc-name "*lspce doc*"
  "The name of lspce tooltip name."
  :type 'string)

(defcustom lspce-completion-ignore-case t
  "If non-nil, ignore case when completing."
  :type 'boolean)

;; Customizable via `completion-category-overrides'.
(when (assoc 'flex completion-styles-alist)
  (add-to-list 'completion-category-defaults '(lspce-capf (styles flex basic))))

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
(defvar lspce-server-programs `(("rust-mode"  "rust-analyzer" "--log-file /tmp/ra.log -v" lspce-ra-initializationOptions)
                                ("python-mode" "pyright-langserver" "--stdio" lspce-pyright-initializationOptions)
                                ("C" "clangd" "")
                                ("go-mode"  "gopls" "-logfile=/tmp/gopls.log -v")
                                ("java-mode"  "jdtls" lspce-jdtls-cmd-args lspce-jdtls-initializationOptions))
  "How the command `lspce' gets the server to start.
A list of (LSP-TYPE SERVER-COMMAND SERVER-PARAMS).  LSP-TYPE
identifies the buffers that are to be managed by a specific
language server, it is returned by `lspce-lsp-type-function'.
The SERVER-COMMAND specifies which server is used for those buffers.

SERVER-PARAMS can be:

* nil, in this case, no param is required to start the lsp server;

* In the most common case, a string such as --stdio;

* A function that returns the params;
")

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


;;; Create LSP params
(defun lspce--make-didOpenTextDocumentParams ()
  (let ((uri (lspce--path-to-uri buffer-file-name))
        (language-id (lspce--buffer-language-id))
        (version lspce--identifier-version)
        (text (lspce--widening (buffer-substring-no-properties (point-min) (point-max)))))
    (lspce--didOpenTextDocumentParams (lspce--textDocumentItem uri language-id version text))))

(defun lspce--make-didCloseTextDocumentParams ()
  (let ((uri (lspce--path-to-uri buffer-file-name)))
    (lspce--didCloseTextDocumentParams (lspce--textDocumentIdenfitier uri)))
  )

(defun lspce--make-didChangeTextDocumentParams ()
  (let ((uri (lspce--path-to-uri buffer-file-name))
        (version lspce--identifier-version)
        beg end len text changes)
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
    (lspce--didChangeTextDocumentParams (lspce--versionedTextDocumentIdenfitier uri version)
                                        (vconcat changes))))

(defun lspce--make-position (&optional pos)
  (let (line character)
    (setq line (1- (line-number-at-pos pos t)))
    (setq character (progn (when pos (goto-char pos))
                           (funcall lspce-current-column-function)))
    (lspce--position line character)))

(defun lspce--make-definitionParams (&optional context)
  (lspce--definitionParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-referenceParams ()
  (lspce--referencesParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)
   (lspce--referenceContext)))

(defun lspce--make-hoverParams (&optional context)
  (lspce--hoverParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))

(defun lspce--make-initializeParams (root-uri initializationOptions)
  (lspce--initializeParams root-uri (lspce--clientCapabilities) initializationOptions))


;;; LSP functions
(defun lspce--server-capable (capability)
  (gethash capability lspce--server-capabilities))

(defun lspce--server-capable-chain (&rest cs)
  (let ((capabilities lspce--server-capabilities))
    (dolist (c cs)
      (when capabilities
        (setq capabilities (gethash c capabilities))))
    capabilities))

(defun lspce--root-uri ()
  (let ((proj (project-current))
        root-uri)
    (setq root-uri (if proj
                       (project-root proj)
                     buffer-file-name))
    (when root-uri
      (lspce--path-to-uri root-uri))))

(defun lspce--lsp-type-default ()
  "The return value is also used as language-id."
  (let ((suffix ""))
    (when buffer-file-name
      (setq suffix (file-name-extension buffer-file-name)))
    (cond
     ((member suffix '("c" "c++" "cpp" "h" "hpp" "cxx"))
      "C")
     (t
      ;; (string-remove-suffix "-mode" (symbol-name major-mode))
      (symbol-name major-mode)
      ))))
(defalias 'lspce--buffer-language-id 'lspce--lsp-type-default "lspce--buffer-language-id")

(defvar lspce-lsp-type-function #'lspce--lsp-type-default
  "Function to the lsp type of current buffer.")

(cl-defun lspce--request (method &optional params timeout)
  (let ((request (lspce--make-request method params))
        (root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        (timeout (max (or timeout 3) 3))
        response-str response response-error response-data)
    (unless (and root-uri lsp-type)
      (user-error "lspce--request: Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--request nil))

    (lspce--notify-textDocument/didChange)

    (setq response-str (lspce-module-request root-uri lsp-type (json-encode request) timeout))
    (when response-str
      (progn
        (setq response (json-parse-string response-str :array-type 'list))
        (setq response-error (gethash "error" response))
        (if response-error
            (message "LSP error %s" (gethash "message" response-error))
          (setq response-data (gethash "result" response)))))
    response-data))

(cl-defun lspce--notify (method &optional params)
  (let ((notification (lspce--make-notification method params))
        (root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        )
    (unless (and root-uri lsp-type)
      (user-error "lspce--notify: Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--notify nil))

    (lspce-module-notify root-uri lsp-type (json-encode notification))))


(defun lspce--notify-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when lspce--recent-changes
    (lspce--notify "textDocument/didChange" (lspce--make-didChangeTextDocumentParams))
    (setq lspce--recent-changes nil)))

(defun lspce--notify-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (lspce--notify
   "textDocument/didOpen" (lspce--make-didOpenTextDocumentParams)))

(defun lspce--notify-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[lspce] error sending textDocument/didClose: %s"
    (lspce--notify
     "textDocument/didClose" (lspce--make-didCloseTextDocumentParams)))
  )

(defun lspce--server-program (lsp-type)
  (let ((server (assoc-default lsp-type lspce-server-programs)))
    server))

;; 返回server info.
(cl-defun lspce--connect ()
  (let ((root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        (initialize-params nil)
        lsp-server
        server server-cmd server-args initialize-options
        response-str response response-error response-result)
    (setq lsp-server (lspce-module-server root-uri lsp-type))
    (when lsp-server
      (message "lspce--connect: Server for (%s %s) is running." root-uri lsp-type)
      (cl-return-from lspce--connect lsp-server))

    (unless (and root-uri lsp-type)
      (user-error "lspce--connect: Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--connect nil))

    (setq server (lspce--server-program lsp-type))
    (unless server
      (user-error "lspce--connect: Do not support current buffer.")
      (cl-return-from lspce--connect nil))

    ;; (message "server %S" server)
    (setq server-cmd (nth 0 server)
          server-args (nth 1 server)
          initialize-options (nth 2 server))
    (when (functionp server-cmd)
      (setq server-cmd (funcall server-cmd)))
    (unless server-cmd
      (user-error "lspce--connect: Can not find lsp server progrom.")
      (cl-return-from lspce--connect nil))
    (when (functionp server-args)
      (setq server-args (funcall server-args)))
    (unless server-args
      (setq server-args ""))
    (when (functionp initialize-options)
      (setq initialize-options (funcall initialize-options)))

    ;; (message "server-args: %s" server-args)
    ;; (message "initialize-options: %s" initialize-options)

    (setq initialize-params (lspce--make-initializeParams root-uri initialize-options))

    (setq response-str (lspce-module-connect root-uri lsp-type server-cmd server-args (json-encode (lspce--make-request "initialize" initialize-params))))

    response-str))

(defun lspce--shutdown ()
  (let ((root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        response-str)
    (unless (and root-uri lsp-type)
      (user-error "lspce--shutdown: Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--shutdown nil))

    (setq response-str (lspce-module-shutdown root-uri lsp-type (json-encode (lspce--make-request "shutdown"))))

    response-str))

;;; Minor modes
;;;
(cl-defstruct lspce--hash-key
  (root-uri)
  (lsp-type)
  )

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

(define-hash-table-test 'lspce--hash-equal 'lspce--hash-key-test-fn 'lspce--hash-key-hash-fn)

(defvar lspce--managed-buffers (make-hash-table :test 'lspce--hash-equal)
  "All files that enable lspce-mode. The key is `lspce--hash-key'.
The value is also a hash table, with uri as the key and the value is just t.")

(defvar-local lspce--uri nil)
(defvar-local lspce--root-uri nil)
(defvar-local lspce--lsp-type nil)
(defvar-local lspce--server-info nil)
(defvar-local lspce--server-capabilities nil)

(defvar lspce-mode-map (make-sparse-keymap))

(cl-defun lspce--buffer-enable-lsp ()
  (let ((root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        server-info server-key server-managed-buffers)
    (unless (and root-uri lsp-type)
      (message "Can not get root-uri or lsp-type of current buffer.")
      (cl-return-from lspce--buffer-enable-lsp nil))

    (setq-local lspce--root-uri root-uri)
    (setq-local lspce--lsp-type lsp-type)
    (setq-local lspce--recent-changes nil
                lspce--identifier-version 0
                lspce--uri (lspce--path-to-uri buffer-file-name))

    (setq server-info (lspce--connect))
    (unless server-info
      (cl-return-from lspce--buffer-enable-lsp nil))
    ;; (message "server-info: %s" server-info)

    (when (lspce--notify-textDocument/didOpen)
      (setq-local lspce--server-info (json-parse-string server-info :array-type 'list))
      (if-let (capabilities (gethash "capabilities" lspce--server-info))
          (progn
            (setq lspce--server-capabilities (json-parse-string capabilities :array-type 'list)))
        (setq lspce--server-capabilities (make-hash-table :test #'equal)))
      (setq server-key (make-lspce--hash-key :root-uri root-uri :lsp-type lsp-type))
      (setq server-managed-buffers (gethash server-key lspce--managed-buffers))
      (unless server-managed-buffers
        (setq server-managed-buffers (make-hash-table :test 'equal)))
      (puthash lspce--uri t server-managed-buffers)
      (puthash server-key server-managed-buffers lspce--managed-buffers))))

(cl-defun lspce--buffer-disable-lsp ()
  (let (server-key server-managed-buffers)
    (setq server-key (make-lspce--hash-key :root-uri lspce--root-uri :lsp-type lspce--lsp-type))
    (setq server-managed-buffers (gethash server-key lspce--managed-buffers))
    (when server-managed-buffers
      (remhash lspce--uri server-managed-buffers)
      (when (= (hash-table-count server-managed-buffers) 0)
        (lspce--shutdown))
      (puthash server-key server-managed-buffers lspce--managed-buffers)
      
      (setq-local lspce--server-info nil))))

(defun lspce--kill-buffer-hook ()
  (lspce--notify-textDocument/didClose)
  (lspce--buffer-disable-lsp)
  )

;; TODO add kill-emacs-hook to kill all lsp servers.
(define-minor-mode lspce-mode
  "Mode for source buffers managed by some LSPCE project."
  :init-value nil :lighter nil :keymap lspce-mode-map
  (cond
   (lspce-mode
    (cond
     ((not buffer-file-name)
      (message "Lspce can not be used in non-file buffers.")
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
      (add-hook 'flymake-diagnostic-functions 'lspce-flymake-backend nil t)
      (flymake-mode 1)
      (lspce--buffer-enable-lsp)
      (if lspce--server-info
          (message "Connected to lsp server.")
        (message "Failed to connect to lsp server.")
        (setq lspce-mode nil)))))
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
    (remove-hook 'flymake-diagnostic-functions 'lspce-flymake-backend t)
    (lspce--notify-textDocument/didClose)
    (flymake-mode -1)
    (lspce--buffer-disable-lsp)
    )))

;;; Hooks
(defvar-local lspce--recent-changes nil
  "Recent buffer changes as collected by `lspce--before-change'.")

(defvar-local lspce--change-idle-timer nil "Idle timer for didChange signals.")

(defvar-local lspce--identifier-version 0 "The version number of this document (it will increase after each change, including undo/redo).")

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
     ;; github#259 and github#367: With `capitalize-word' or somesuch,
     ;; `before-change-functions' always records the whole word's
     ;; `b-beg' and `b-end'.  Similarly, when coalescing two lines
     ;; into one, `fill-paragraph' they mark the end of the first line
     ;; up to the end of the second line.  In both situations, args
     ;; received here contradict that information: `beg' and `end'
     ;; will differ by 1 and will likely only encompass the letter
     ;; that was capitalized or, in the sentence-joining situation,
     ;; the replacement of the newline with a space.  That's we keep
     ;; markers _and_ positions so we're able to detect and correct
     ;; this.  We ignore `beg', `len' and `pre-change-len' and send
     ;; "fuller" information about the region from the markers.  I've
     ;; also experimented with doing this unconditionally but it seems
     ;; to break when newlines are added.
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
  (lspce--notify-textDocument/didClose)
  )

(defun lspce--after-revert-hook ()
  "Lspce's `after-revert-hook'."
  (lspce--notify-textDocument/didOpen))


;;; Xref
(defun lspce-xref-backend () 'xref-lspce)

(cl-defstruct lspce--xref-item
  (filename)
  (start-line)
  (start-column)
  (end-line)
  (end-column)
  )

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

(defun lspce--extract-line-from-buffer (pos)
  "Return the line pointed to by POS (a Position object) in the current buffer."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (buffer-substring (line-beginning-position) (line-end-position)))))

(defun lspce--lsp-position-to-point (line character)
  "Convert LSP position to Emacs point."
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
             "Caution: LSP server sent invalid character position %s. Using 0 instead."
             col)
            (setq col 0))
          (funcall lspce-move-to-column-function col)))
      (point))))

(defun lspce--lsp-position-to-point2 (pos)
  (let ((line (gethash "line" pos))
        (character (gethash "character" pos)))
    (lspce--lsp-position-to-point line character)))

(defun lspce--range-region (range)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((start (gethash "start" range))
         (end (gethash "end" range))
         (beg (lspce--lsp-position-to-point2 start))
         (end (lspce--lsp-position-to-point2 end)))
    (cons beg end)))

(defun lspce--locations-to-xref (locations)
  (let (xrefs uri range start end filename start-line start-column end-line end-column items xref-items groups)
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
            (setq uri (gethash "uri" item))
            (setq range (gethash "range" item))
            (setq start (gethash "start" range))
            (setq end (gethash "end" range))
            (setq start-line (gethash "line" start))
            (setq start-column (gethash "character" start))
            (setq end-line (gethash "line" end))
            (setq end-column (gethash "character" end))
            (setq filename (lspce--uri-to-path uri))

            (cl-pushnew (make-lspce--xref-item :filename filename :start-line start-line :start-column start-column :end-line end-line :end-column end-column) xref-items)
            )

          (setq groups (seq-group-by (lambda (x) (lspce--xref-item-filename x))
                                     (seq-sort #'lspce--location-before-p xref-items)))
          (dolist (group groups)
            (let* ((filename (car group))
                   (items (cdr group))
                   (visiting (find-buffer-visiting filename))
                   (collect (lambda (item)
                              (lspce--widening
                               (let* ((beg (lspce--lsp-position-to-point (lspce--xref-item-start-line item)
                                                                         (lspce--xref-item-start-column item)))
                                      (end (lspce--lsp-position-to-point (lspce--xref-item-end-line item)
                                                                         (lspce--xref-item-end-column item)))
                                      (bol (progn (goto-char beg) (point-at-bol)))
                                      (substring (buffer-substring bol (point-at-eol)))
                                      (hi-beg (- beg bol))
                                      (hi-end (- (min (point-at-eol) end) bol)))
                                 (add-face-text-property hi-beg hi-end 'xref-match t substring)
                                 (cl-pushnew (xref-make substring (xref-make-file-location filename (+ (lspce--xref-item-start-line item) 1) (lspce--xref-item-start-column item))) xrefs))))))
              (if visiting
                  (with-current-buffer visiting
                    (seq-map collect items))
                (when (file-readable-p filename)
                  (with-temp-buffer
                    (insert-file-contents-literally filename)
                    (seq-map collect items)))))))
      
      (error (lspce-warn "Failed to process xref entry for filename '%s': %s"
                         filename (error-message-string err)))
      (file-error (lspce-warn "Failed to process xref entry, file-error, '%s': %s"
                              filename (error-message-string err)))
      )
    (nreverse xrefs)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lspce)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lspce)) identifier)
  (save-excursion
    (lspce--locations-to-xref (lspce--request "textDocument/definition" (lspce--make-definitionParams)))
    ))

;; NOTE if you use `ivy-xref-show-xrefs' as the `xref-show-xrefs-function',
;; you will find `xref-backend-references' is called twice.
;; See https://github.com/alexmurray/ivy-xref/issues/2
(cl-defmethod xref-backend-references ((_backend (eql xref-lspce)) identifier)
  (save-excursion
    (lspce--locations-to-xref (lspce--request "textDocument/references" (lspce--make-referenceParams)))
    ))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lspce)))
  (list (propertize (or (thing-at-point 'symbol) "")
                    'identifier-at-point t)))


;;; capf
(defvar-local lspce--completion-complete? nil) ;; 1 incomplete 2 complete
(defvar-local lspce--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

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

(defun lspce--make-completionParams()
  (let (context)
    (lspce--completionParams (lspce--textDocumentIdenfitier lspce--uri)
                             (lspce--make-position)
                             (lspce--make-completionContext))))

(cl-defun lspce--request-completion ()
  (let ((params (lspce--make-completionParams))
        response items complete?)
    (setq response (lspce--request "textDocument/completion" params))
    (unless response
      (message "lspce--request-completion response-str is null")
      (cl-return-from lspce--request-completion nil))

    ;; (message "lspce--request-completion response: %S" response)
    ;; (message "lspce--request-completion response type-of: %s" (type-of response))
    (cond
     ((listp response)
      (setq complete? t
            items response))
     ((hash-table-p response)
      (setq complete? (not (gethash "isIncomplete" response))
            items (gethash "items" response))
      )
     (t
      (message "Unknown response type: %s" (type-of response))
      (cl-return-from lspce--request-completion nil)))
    (list complete? items)))

(defun lspce--completions (items)
  (let (completions)
    (dolist (item items)
      (cl-pushnew (gethash "label" item) completions))
    completions))

(defun lspce--test-completions ()
  (let (completions (lspce--completions (nth 1 (lspce--request-completion))))
    (message "completions: %S" completions)))

(defun lspce--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'yas-expand-snippet))

(defun lspce-completion-at-point()
  (when-let (completion-capability (lspce--server-capable "completionProvider"))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           items
           completions
           complete?
           (cached-proxies :none)
           (proxies
            (lambda ()
              (if (and
                   complete?
                   (listp cached-proxies))
                  cached-proxies
                (setq completions (lspce--request-completion))
                (setq complete? (nth 0 completions)
                      items (nth 1 completions))
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
                             (put-text-property 0 1 'lspce--lsp-item item proxy))
                           proxy))
                       items))
                cached-proxies))))
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (lambda (probe pred action)
         (let (collection)
           (cond
            ((eq action 'metadata) `(metadata (category . lspce-capf)
                                              (display-sort-function . identity)
                                              (cycle-sort-function . identity)))               ; metadata
            ((eq (car-safe action) 'boundaries) nil)       ; boundaries
            ((eq action 'lambda)                           ; test-completion
             (setq collection (funcall proxies))
             (test-completion probe collection))
            ((null action)                                 ; try-completion
             (setq collection (funcall proxies))
             (try-completion probe collection))
            ((eq action t)                                 ; all-completions
             (setq collection (funcall proxies))
             (all-completions
              probe
              collection
              (lambda (proxy)
                (let* ((item (get-text-property 0 'lspce--lsp-item proxy))
                       (filterText (gethash "filterText" item)))
                  (and (or (null pred) (funcall pred proxy))
                       (string-prefix-p
                        probe (or filterText proxy) lspce-completion-ignore-case))))
              ))
            ))
         )
       :annotation-function
       (lambda (proxy)
         (let* ((item (get-text-property 0 'lspce--lsp-item proxy))
                (detail (gethash "detail" item))
                (kind (gethash "kind" item))
                annotation)
           (setq detail (and (stringp detail)
                             (not (string-equal detail ""))
                             detail))
           (setq annotation (or detail
                                (cdr (assoc kind lspce--kind-names))))
           (when annotation
             (concat " "
                     (propertize annotation
                                 'face 'font-lock-function-name-face)))))       
       :company-require-match 'never
       :company-kind
       ;; Associate each lsp-item with a lsp-kind symbol.
       (lambda (proxy)
         (when-let* ((lsp-item (get-text-property 0 'lspce--lsp-item proxy))
                     (kind (alist-get (gethash "kind" lsp-item)
                                      lspce--kind-names)))
           (intern (downcase kind))))
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
           (with-current-buffer (if (minibufferp)
                                    (window-buffer (minibuffer-selected-window))
                                  (current-buffer))
             (setq-local lspce--completion-complete? nil)
             (let* ((lsp-item (get-text-property 0 'lspce--lsp-item proxy))
                    (insertTextFormat (or (gethash "insertTextFormat" lsp-item) 1))
                    (insertText (gethash "insertText" lsp-item))
                    (textEdit (gethash "textEdit" lsp-item))
                    (snippet-fn (and (eql insertTextFormat 2)
                                     (lspce--snippet-expansion-fn))))
               ;; (message "lsp-item %S" (json-encode lsp-item))
               (cond (textEdit
                      ;; Undo (yes, undo) the newly inserted completion.
                      ;; If before completion the buffer was "foo.b" and
                      ;; now is "foo.bar", `proxy' will be "bar".  We
                      ;; want to delete only "ar" (`proxy' minus the
                      ;; symbol whose bounds we've calculated before)
                      ;; (github#160).
                      (delete-region (+ (- (point) (length proxy))
                                        (if bounds
                                            (- (cdr bounds) (car bounds))
                                          0))
                                     (point))
                      (let ((range (gethash "range" textEdit))
                            (newText (gethash "newText" textEdit)))
                        ;; (message "range %S" range)
                        (pcase-let ((`(,beg . ,end)
                                     (lspce--range-region range)))
                          ;; (message "beg %s, end %s" beg end)
                          (delete-region beg end)
                          (goto-char beg)
                          (funcall (or snippet-fn #'insert) newText))
                        )
                      )
                     (snippet-fn
                      ;; A snippet should be inserted, but using plain
                      ;; `insertText'.  This requires us to delete the
                      ;; whole completion, since `insertText' is the full
                      ;; completion's text.
                      (delete-region (- (point) (length proxy)) (point))
                      (funcall snippet-fn (or insertText label))))
               )
             (lspce--notify-textDocument/didChange)
             )
           )
         )
       )))
  )

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
    (if (and (fboundp #'posframe-workable-p) (posframe-workable-p))
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
      (let* ((params (lspce--make-hoverParams))
             (response (lspce--request "textDocument/hover" params))
             contents kind content language)
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
              (lspce--display-help kind content))
            )
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
              (lspce--display-help kind content)))
           (t
            ;; nothing
            ))))
    (user-error "Lspce mode is not enabled.")))

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

(defun lspce--diag-type (sev)
  (cond ((null sev) 'lspce-error)
        ((<= sev 1) 'lspce-error)
        ((= sev 2)  'lspce-warning)
        (t          'lspce-note)))

(defun lspce--read-diagnostics ()
  (if lspce-mode
      (let (diagnostics
            flymake-diags range start end severity message)
        (setq diagnostics (lspce-module-read-file-diagnostics lspce--root-uri lspce--lsp-type lspce--uri))
        ;; (message "diagnostics: %S" diagnostics)
        (when diagnostics
          ;; FIXME 根据diag-type和位置排序。
          (dolist (d (json-parse-string diagnostics :array-type 'list))
            ;; (message "d %S" d)
            (setq range (gethash "range" d)
                  severity (gethash "severity" d)
                  message (gethash "message" d))
            (setq start (gethash "start" range)
                  end (gethash "end" range))
            (push (flymake-make-diagnostic (current-buffer)
                                           (lspce--lsp-position-to-point (gethash "line" start)
                                                                         (gethash "character" start))
                                           (lspce--lsp-position-to-point (gethash "line" end)
                                                                         (gethash "character" end))
                                           (lspce--diag-type severity)
                                           message) flymake-diags)))
        flymake-diags)))

(defun lspce-flymake-backend (report-fn &rest _more)
  "A Flymake backend for Lspce."
  (let ((diagnostics (lspce--read-diagnostics)))
    (cond (lspce-mode
           (setq lspce--current-flymake-report-fn report-fn)
           (lspce--report-to-flymake diagnostics))
          (t
           (funcall report-fn nil))))
  )

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

;; (flymake-make-diagnostic)


;;; Mode-line
;;;
(defvar lspce--mode-line-format `(:eval (lspce--mode-line-format)))

(put 'lspce--mode-line-format 'risky-local-variable t)

(defun lspce--mode-line-format ()
  "Compose the LSPCE's mode-line."
  (let* ((server lspce--server-info)
         name id)
    (when server
      (setq id (gethash "id" server))
      (propertize (concat "lspce:" id) 'face 'lspce-mode-line))))

(add-to-list 'mode-line-misc-info
             `(lspce-mode (" [" lspce--mode-line-format "] ")))


(provide 'lspce)
