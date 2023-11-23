;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile) ;; for compile face

(declare-function lspce--query-call-hierarchy "lspce")
(declare-function lspce--uri-to-path "lspce-util")
(declare-function lspce--widening "lspce-util")

(defvar lspce--incoming-call-indent 0)
(defvar lspce--incoming-call-items nil)

(defun lspce--call-hierarchy-item-collector (walker-fn)
  (setq lspce--incoming-call-indent 0)
  (setq lspce--incoming-call-items nil)

  (when-let ((tree (lspce--query-incoming-calls)))
    (funcall walker-fn tree)))

(when (featurep 'ivy)
  (defun lspce--call-hierarchy-ivy-item-transfer (item indent)
    "ivy transfer"
    (let (result
          label
          name kind detail uri range start end
          start-line start-column info)
      (setq name (gethash "name" item)
            uri (gethash "uri" item)
            range (gethash "range" item))
      (setq start (gethash "start" range))
      (setq start-line (1+ (gethash "line" start)))
      (setq start-column (gethash "character" start))
      (setq info (list :uri uri :start-line start-line))

      (setq label (format "%30s:%s%s%s"
                          (propertize (truncate-string-to-width (f-filename (lspce--uri-to-path uri)) 30) 'face 'compilation-info)
                          (propertize (format "%4d" start-line) 'face 'compilation-line-number)
                          (make-string (* indent 4) ?\s) name ))
      (cons label info)))

  (defun lspce--call-hierarchy-ivy-walker (item-list)
    (dolist (item item-list)
      (cond
       ((listp item)
        (setq lspce--incoming-call-indent (1+ lspce--incoming-call-indent))
        (lspce--call-hierarchy-ivy-walker item)
        (setq lspce--incoming-call-indent (- lspce--incoming-call-indent 1)))
       (t
        (cl-pushnew (lspce--call-hierarchy-ivy-item-transfer item lspce--incoming-call-indent) lspce--incoming-call-items)))))

;;;###autoload
  (defun lspce-incoming-calls-to-ivy ()
    "Fetch incoming calls to current symbol, render the result with ivy."
    (interactive)
    (lspce--call-hierarchy-item-collector #'lspce--call-hierarchy-ivy-walker)
    (ivy-read "调用链: " (reverse lspce--incoming-call-items)
              :action '(1
                        ("v" (lambda (item)
                               (let* ((info (cdr item))
                                      (uri (plist-get info :uri))
                                      (start-line (plist-get info :start-line))
                                      (start-column (plist-get info :start-column))
                                      (filename (lspce--uri-to-path uri))
                                      (buffer (find-file-noselect filename)))
                                 (switch-to-buffer buffer)
                                 (with-current-buffer buffer
                                   (widen)
                                   (goto-char (point-min))
                                   (forward-line start-line)
                                   (forward-char start-column)))
                               )
                         ))))

  )

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
