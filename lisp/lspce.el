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


(defcustom lspce-connect-timeout 30
  "Number of seconds before timing out LSP connection attempts.
If nil, never time out."
  :type 'number)

(defcustom lspce-autoshutdown nil
  "If non-nil, shut down server after killing last managed buffer."
  :type 'boolean)

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



;;; Minor modes
;;;
(defvar lspce-mode-map (make-sparse-keymap))

(defvar lspce--managed-mode)              ; forward decl

(defun lspce-managed-p ()
  "Tell if current buffer is managed by LSPCE."
  lspce--managed-mode)

(defvar lspce-managed-mode-hook nil
  "A hook run by LSPCE after it started/stopped managing a buffer.
Use `lspce-managed-p' to determine if current buffer is managed.")


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
      (user-error "Can not get root-uri of lsp-type of current buffer.")
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

;;; Xref
(defun lspce--xref-backend () 'xref-lspce)

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
