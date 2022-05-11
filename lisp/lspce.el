;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-


;;; Require
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-util)
(require 'compile)                      ; for some faces
(require 'warnings)

(require 'lspce-util)
(require 'lspce-types)

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

(defcustom lspce-doc-tooltip-font nil
  "The font of documentation tooltip.

Font format follow rule: fontname-fontsize."
  :type 'string)

(defcustom lspce-doc-tooltip-border-width 15
  "The border width of lspce tooltip, default is 15 px."
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

(defun lspce--make-textDocumentPositionParams ()
  (lspce--definitionParams
   (lspce--textDocumentIdenfitier (lspce--path-to-uri buffer-file-name))
   (lspce--make-position)))


;;; LSP functions
(defun lspce--root-uri ()
  (let ((proj (project-current)))
    (if proj
        (project-root proj)
      buffer-file-name)))

(defun lspce--lsp-type-default ()
  (let ((suffix ""))
    (when buffer-file-name
      (setq suffix (file-name-extension buffer-file-name)))
    (cond
     ((member suffix '("c" "c++" "cpp" "h" "hpp" "cxx"))
      "C")
     (t
      (symbol-name major-mode)))))

(defvar lspce-lsp-type-function #'lspce--lsp-type-default
  "Function to the lsp type of current buffer.")

(cl-defun lspce--request (method &optional params)
  (let ((request (lspce--make-request method params))
        (root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        response-str response response-error response-result)
    (unless (and root-uri lsp-type)
      (user-error "lspce--request: Can not get root-uri of lsp-type of current buffer.")
      (cl-return-from lspce--request nil))

    (setq response-str (lspce-module-request root-uri lsp-type (json-encode request)))
    (when response-str
      (progn
        (setq response (json-parse-string response-str))
        (setq response-error (gethash "error" response))
        (if response-error
            (message "LSP error %s" (gethash "message" response-error))
          (setq response-result (gethash "result" response)))))
    response-result))

(cl-defun lspce--notify (method &optional params)
  (let ((notification (lspce--make-notification method params))
        (root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        )
    (unless (and root-uri lsp-type)
      (user-error "lspce--notify: Can not get root-uri of lsp-type of current buffer.")
      (cl-return-from lspce--notify nil))

    (lspce-module-notify root-uri lsp-type (json-encode notification))))


(defun lspce--notify-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when lspce--recent-changes
    (lspce--notify "textDocument/didChange" (lspce--make-didChangeTextDocumentParams))
    (setq lspce--recent-changes nil)))

(defun lspce--notify-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq lspce--recent-changes nil lspce--identifier-version 0)
  (lspce--notify
   "textDocument/didOpen" (lspce--make-didOpenTextDocumentParams)))

(defun lspce--notify-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[lspce] error sending textDocument/didClose: %s"
    (lspce--notify
     "textDocument/didClose" (lspce--make-didCloseTextDocumentParams))))


(defun lspce--connect ()
  (let ((request (lspce--make-request method params))
        (root-uri (lspce--root-uri))
        (lsp-type (funcall lspce-lsp-type-function))
        response-str response response-error response-result)
    (unless (and root-uri lsp-type)
      (user-error "lspce--request: Can not get root-uri of lsp-type of current buffer.")
      (cl-return-from lspce--request nil))

    (setq response-str (lspce-module-request root-uri lsp-type (json-encode request)))
    (when response-str
      (progn
        (setq response (json-parse-string response-str))
        (setq response-error (gethash "error" response))
        (if response-error
            (message "LSP error %s" (gethash "message" response-error))
          (setq response-result (gethash "result" response)))))
    response-result)
  )

;;; Minor modes
;;;
(defvar lspce-mode-map (make-sparse-keymap))

(define-minor-mode lspce-mode
  "Mode for source buffers managed by some LSPCE project."
  :init-value nil :lighter nil :keymap lspce-mode-map
  (cond
   (lspce-mode
    (cond
     ((not buffer-file-name)
      (user-error "Lspce can not be used in non-file buffers.")
      (setq lspce-mode nil))
     (t
      (add-hook 'after-change-functions 'lspce--after-change nil t)
      (add-hook 'before-change-functions 'lspce--before-change nil t)
      (add-hook 'kill-buffer-hook 'lspce--notify-textDocument/didClose nil t)
      (add-hook 'before-revert-hook 'lspce--notify-textDocument/didClose nil t)
      (add-hook 'after-revert-hook 'lspce--after-revert-hook nil t)
      (add-hook 'xref-backend-functions 'lspce-xref-backend nil t))))
   (t
    (remove-hook 'after-change-functions 'lspce--after-change t)
    (remove-hook 'before-change-functions 'lspce--before-change t)
    (remove-hook 'kill-buffer-hook 'lspce--notify-textDocument/didClose t)
    (remove-hook 'before-revert-hook 'lspce--notify-textDocument/didClose t)
    (remove-hook 'after-revert-hook 'lspce--after-revert-hook t)
    (remove-hook 'xref-backend-functions 'lspce-xref-backend t))))

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

(defun lspce--after-revert-hook ()
  "Lspce's `after-revert-hook'."
  (lspce--notify-textDocument/didOpen))


;;; Xref
(defun lspce-xref-backend () 'xref-lspce)

(defun lspce--locations-to-xref (locations)
  (let (xref-items uri range start filename line column items)
    (cond
     ((arrayp locations)
      (setq items locations)
      )
     ((hash-table-p locations)
      (setq items (list locations)))
     (t
      (setq items nil)))
    (condition-case err
        (dolist (item items)
          (setq uri (gethash "targetUri" item))
          (setq range (gethash "targetRange" item))
          (setq start (gethash "start" range))
          (setq line (1+ (gethash "line" start)))
          (setq column (gethash "character" start))
          (setq filename (lspce--uri-to-path uri))

          (cl-pushnew (xref-make filename (xref-make-file-location filename line column)) xref-items)

          (let ((visiting (find-buffer-visiting filename)))
            )
          )
      (error (lspce-warn "Failed to process xref entry for filename '%s': %s"
                         filename (error-message-string err)))
      (file-error (lspce-warn "Failed to process xref entry, file-error, '%s': %s"
                              filename (error-message-string err)))
      )
    xref-items
    )
  )

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lspce)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lspce)) identifier)
  (save-excursion
    (lspce--locations-to-xref (lspce--request "textDocument/definition" nil (lspce--make-textDocumentPositionParams)))
    ))

(cl-defmethod xref-backend-references ((_backend (eql xref-lspce)) identifier)
  (save-excursion
    (lspce--locations-to-xref (lspce--request "textDocument/references" nil (lspce--make-textDocumentPositionParams)))
    ))



(provide 'lspce)
