;;; lspce.el --- LSP client for Emacs -*- lexical-binding: t; -*-


;;; Require
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-util)
(require 'compile)                      ; for some faces
(require 'warnings)

(require 'lspce-core)
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


(defun lspce--request (method &optional id params)
  (let ((request (lspce--make-request method id params))
        response)
    )
  )

(defun lspce--xref-backend () 'xref-lspce)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lspce)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lspce)) identifier)
  (save-excursion
    ))

(cl-defmethod xref-backend-references ((_backend (eql xref-lspce)) identifier)
  (save-excursion
    ))



(provide 'lspce)
