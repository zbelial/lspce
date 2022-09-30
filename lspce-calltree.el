;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'compile) ;; for compile face

(declare-function lspce--query-call-hierarchy "lspce")
(declare-function lspce--uri-to-path "lspce-util")
(declare-function lspce--lsp-position-to-point2 "lspce")

(defcustom lspce-call-hierarchy-display-method 'ivy
  "How to display call hierarchy. Only support ivy ATM."
  :group 'lspce
  :type 'symbol)

(when (featurep 'ivy)
  (defvar lspce--incoming-call-indent 0)
  (defvar lspce--incoming-call-items nil)

  (defun lspce--call-hierarchy-item-transfer (item indent)
    "ivy transfer"
    (let (result
          label
          name kind detail uri range start end
          start-pos info)
      (setq name (gethash "name" item)
            uri (gethash "uri" item)
            range (gethash "range" item))
      (setq start (gethash "start" range))
      (setq start-pos (lspce--lsp-position-to-point2 start))
      (setq info (list :uri uri :start-pos start-pos))

      (setq label (format "%30s:%s%s%s"
                          (propertize (truncate-string-to-width (f-filename (lspce--uri-to-path uri)) 30) 'face 'compilation-info)
                          (propertize (format "%4d" (1+ (gethash "line" start))) 'face 'compilation-line-number)
                          (make-string (* indent 4) ?\s) name ))
      (cons label info)))

  (defun lspce--call-hierarchy-walker (item-list)
    (dolist (item item-list)
      (cond
       ((listp item)
        (setq lspce--incoming-call-indent (1+ lspce--incoming-call-indent))
        (lspce--call-hierarchy-walker item)
        (setq lspce--incoming-call-indent (- lspce--incoming-call-indent 1)))
       (t
        (cl-pushnew (lspce--call-hierarchy-item-transfer item lspce--incoming-call-indent) lspce--incoming-call-items)))))

  (defun lspce--call-hierarchy-item-collector ()
    (let ((tree (lspce--query-incoming-calls)))
      (setq lspce--incoming-call-indent 0)
      (setq lspce--incoming-call-items nil)
      (when tree
        (lspce--call-hierarchy-walker tree))))

  (defun lspce-incoming-calls ()
    (interactive)
    (lspce--call-hierarchy-item-collector)
    (ivy-read "调用链: " (reverse lspce--incoming-call-items)
              :action '(1
                        ("v" (lambda (item)
                               (let* ((info (cdr item))
                                      (uri (plist-get info :uri))
                                      (start-pos (plist-get info :start-pos))
                                      (filename (lspce--uri-to-path uri))
                                      (buffer (find-buffer-visiting filename)))
                                 (find-file filename)
                                 (goto-char start-pos)))
                         ))))

  )

(provide 'lspce-calltree)
