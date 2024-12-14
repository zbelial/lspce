;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'lspce-core)
(require 'lspce-util)
(require 'seq)
(require 'hierarchy)
(require 'button)

(defcustom lspce-call-hierarchy-call-site nil
  "If t, jump to the first call site instead of the start
of the surrounding function when clicking."
  :type 'boolean)

(defvar lspce--incoming-call-buffer-name "*Lspce incoming call*")
(defvar lspce--outgoing-call-buffer-name "*Lspce outgoing call*")

(defun lspce--prepare-call-hierarchy ()
  (if (lspce--server-capable-chain "callHierarchyProvider")
      (lspce--request "textDocument/prepareCallHierarchy" (lspce--make-textDocumentPositionParams))
    (lspce--warn "Server does not support call hierarchy.")
    nil))

(define-button-type 'lspce-call-hierarchy-button
  'follow-link t                        ; Click via mouse
  'face 'default)

(defun lspce--call-hierarchy-open-file (file)
  (select-window (get-mru-window (selected-frame) nil :not-selected))
  (find-file file))

(defun lspce--char-after-point-is-alpha ()
  "Check if the character before point is an alphabetic character."
  (let ((char (char-after (point))))
    (and char (or (and (>= char ?a) (<= char ?z))
                  (and (>= char ?A) (<= char ?Z))
                  (= char ?_)))))

(defun lspce--call-hierarchy-next-line ()
  (interactive)
  (when (= (forward-line 1) 1)
    (goto-char (point-min)))
  (goto-char (line-beginning-position))
  (skip-chars-forward "^a-zA-Z_"))

(defun lspce--call-hierarchy-previous-line ()
  (interactive)
  (when (= (forward-line -1) -1)
    (goto-char (point-max)))
  (goto-char (line-beginning-position))
  (skip-chars-forward "^a-zA-Z_"))

;; learn some skills from https://github.com/dolmens/eglot-hierarchy
(defun lspce--hierarchy-calls (direction)
  "Fetch incoming calls to current symbol.
DIRECTION should be 'incoming or 'outgoing."
  (let* ((root (lspce--prepare-call-hierarchy))
         (root-nodes (seq-map (lambda (node) `(:item ,node)) root))
         (tree (hierarchy-new))
         (root-uri lspce--root-uri)
         (lsp-type lspce--lsp-type)
         (method (if (eq direction 'incoming)
                     "callHierarchy/incomingCalls"
                   "callHierarchy/outgoingCalls"))
         (tag (if (eq direction 'incoming)
                  "from"
                "to"))
         (buffer-name (if (eq direction 'incoming)
                          lspce--incoming-call-buffer-name
                        lspce--outgoing-call-buffer-name)))
    (if (length> root 0)
        (progn
          (hierarchy-add-trees
           tree
           root-nodes
           nil
           (lambda (node)
             (let* ((item (plist-get node :item)))
               (condition-case err
                   (let* ((response (lspce--request method (list :item item) nil root-uri lsp-type))
                          children)
                     (setq children (seq-map (lambda (item)
                                               `(:item ,(gethash tag item)
                                                       :fromRanges ,(gethash "fromRanges" item)))
                                             response))
                     children)
                 ((error user-error)
                  (lspce--error "Failed to invoke %s, %s" method err)))))
           nil
           t)
          (pop-to-buffer
           (hierarchy-tree-display
            tree
            (lambda (node _)
              (let* ((item (plist-get node :item))
                     (fromRanges (plist-get node :fromRanges))
                     name range filename selectionRange)
                (setq name (gethash "name" item)
                      filename (lspce--uri-to-path (gethash "uri" item))
                      range (gethash "range" item)
                      selectionRange (gethash "selectionRange" item))
                (insert-text-button name
                                    :type 'lspce-call-hierarchy-button
                                    'action (lambda (btn)
                                              ;; FIXME select-window may fail
                                              (let ((w (get-buffer-window (marker-buffer btn))))
	                                        (when w
                                                  (select-window w)))
                                              (lspce--call-hierarchy-open-file filename)
                                              (let (jump-range)
                                                (if (and lspce-call-hierarchy-call-site)
                                                    (setq jump-range selectionRange)
                                                  (setq jump-range range))
                                                (goto-char (lspce--lsp-position-to-point
                                                            (gethash "start" jump-range))))))))
            (get-buffer-create buffer-name)))
          (with-current-buffer buffer-name
            (keymap-local-set (kbd "h") #'backward-char)
            (keymap-local-set (kbd "l") #'forward-char)
            (keymap-local-set (kbd "b") #'backward-char)
            (keymap-local-set (kbd "f") #'forward-char)
            (keymap-local-set (kbd "n") #'lspce--call-hierarchy-next-line)
            (keymap-local-set (kbd "p") #'lspce--call-hierarchy-previous-line)
            (keymap-local-set (kbd "j") #'lspce--call-hierarchy-next-line)
            (keymap-local-set (kbd "k") #'lspce--call-hierarchy-previous-line)
            (goto-char (point-min))
            (widget-button-press (point))))
      (lspce--warn "No incoming call hierachy under point."))))

;;;###autoload
(defun lspce-incoming-calls ()
  "Fetch incoming calls to current symbol."
  (interactive)
  (lspce--hierarchy-calls 'incoming))

;;;###autoload
(defun lspce-outgoing-calls ()
  "Fetch outgoing calls from current symbol."
  (interactive)
  (lspce--hierarchy-calls 'outgoing))

(provide 'lspce-calltree)
