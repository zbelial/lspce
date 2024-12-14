;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'lspce-core)
(require 'lspce-util)
(require 'cl-lib)
(require 'compile) ;; for compile face
(require 'hierarchy)

(declare-function lspce--uri-to-path "lspce-util")
(declare-function lspce--widening "lspce-util")


(defun lspce--incoming-calls (item)
  (let ((response (lspce--request "callHierarchy/incomingCalls" (list :item item)))
        children
        from result)
    (when response
      (dolist (incoming response)
        (message "incoming: %s" (json-encode incoming))
        (setq from (gethash "from" incoming))
        (cl-pushnew (list from (lspce--incoming-calls from)) children)))
    (setq result (reverse children))
    result))

(defun lspce--query-incoming-calls ()
  (if (lspce--server-capable-chain "callHierarchyProvider")
      (let ((response (lspce--request "textDocument/prepareCallHierarchy" (lspce--make-textDocumentPositionParams)))
            tree)
        (when response
          (dolist (item response)
            (message "item: %s" (json-encode item))
            (cl-pushnew (list item (lspce--incoming-calls item)) tree))
          tree))
    (lspce--warn "Server does not support call hierarchy.")
    nil))


(defvar lspce--incoming-call-indent 0)
(defvar lspce--incoming-call-items nil)

(defun lspce--call-hierarchy-item-collector (walker-fn)
  (setq lspce--incoming-call-indent 0)
  (setq lspce--incoming-call-items nil)

  (when-let ((tree (lspce--query-incoming-calls)))
    (message "tree: %s" tree)
    (funcall walker-fn tree)))

(defvar lspce--incoming-call-org-buffer "*Incoming Calls in Org*")
(defun lspce--call-hierarchy-org-item-transfer (item indent)
  "org transfer"
  (let (label filename range start start-line)
    (setq name (gethash "name" item))
    (setq name (gethash "name" item)
          filename (lspce--uri-to-path (gethash "uri" item))
          range (gethash "range" item))
    (setq start (gethash "start" range))
    (setq start-line (1+ (gethash "line" start)))
    
    (setq label (format "%s %s\n%s" (make-string indent ?*) name
                        (format "%s[[%s::%d][%s:%d]]" (make-string (1+ indent) ?\s) filename start-line (f-filename filename) start-line)))

    label))

(defun lspce--call-hierarchy-org-walker (item-list)
  (dolist (item item-list)
    (cond
     ((listp item)
      (setq lspce--incoming-call-indent (1+ lspce--incoming-call-indent))
      (lspce--call-hierarchy-org-walker item)
      (setq lspce--incoming-call-indent (- lspce--incoming-call-indent 1)))
     (t
      (cl-pushnew (lspce--call-hierarchy-org-item-transfer item lspce--incoming-call-indent) lspce--incoming-call-items)))))

;;;###autoload
(defun lspce-incoming-calls-to-org ()
  "Fetch incoming calls to current symbol, render the result in orgmode format."
  (interactive)
  (let (call-items
        buffer)
    (lspce--call-hierarchy-item-collector #'lspce--call-hierarchy-org-walker)
    (setq call-items (reverse lspce--incoming-call-items))
    (if call-items
        (progn
          (setq buffer (get-buffer-create lspce--incoming-call-org-buffer))
          (with-current-buffer buffer
            (erase-buffer)
            (dolist (ci call-items)
              (insert ci "\n"))
            (org-mode))
          (switch-to-buffer buffer))
      (message "No incoming calls."))))

(provide 'lspce-calltree)
