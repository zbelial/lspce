;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs))
(require 'yasnippet)

(defconst LSPCE-VERSION "1.1.0")
(defconst LSPCE-NAME "lspce")

(defconst LSPCE-LOG-ERROR 4)
(defconst LSPCE-LOG-WARN 3)
(defconst LSPCE-LOG-INFO 2)
(defconst LSPCE-LOG-DEBUG 1)

(defconst lspce--{} (make-hash-table) "The empty JSON object.")

(defcustom lspce-log-level LSPCE-LOG-WARN
  "lspce log level."
  :type 'integer
  :group 'stock)


(defun lspce-current-column () (- (point) (point-at-bol)))

(defun lspce-lsp-abiding-column ()
  "Calculate current COLUMN as defined by the LSP spec."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (point) 'utf-16 t))
        2)
     2))

(cl-defmacro lspce--widening (&rest body)
  "Save excursion and restriction.  Widen.  Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))

(cl-defmacro lspce--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))


(defun lspce--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (lspce--widening
   (list :line (1- (line-number-at-pos pos t)) ; F!@&#$CKING OFF-BY-ONE
         :character (progn (when pos (goto-char pos))
                           (funcall lspce-current-column-function)))))

(defun lspce-move-to-column (column)
  "Move to COLUMN without closely following the LSP spec."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP columns in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (line-beginning-position) column)
                  (line-end-position))))

(defun lspce-move-to-lsp-abiding-column (column)
  "Move to COLUMN abiding by the LSP spec."
  (cl-loop
   initially (move-to-column column)
   with lbp = (line-beginning-position)
   for diff = (- column
                 (/ (- (length (encode-coding-region lbp (point) 'utf-16 t))
                       2)
                    2))
   until (zerop diff)
   do (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))))

;; copied from eglot
(defconst lspce--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil)
    vec)
  "Like `url-path-allows-chars' but more restrictive.")

(defun lspce--path-to-uri (path)
  "URIfy PATH."
  (let* ((truepath (file-truename path))
         (full-name (directory-file-name (file-local-name truepath))))
    (if (eq system-type 'windows-nt)
        (let ((label (url-type (url-generic-parse-url path)))
              prefix)
          (setq prefix (concat label ":"))
          (concat "file:///"
                  prefix
                  (url-hexify-string
                   (substring full-name (length prefix))
                   lspce--uri-path-allowed-chars)))
      (concat "file://"
              (url-hexify-string
               ;; Again watch out for trampy paths.
               (directory-file-name (file-local-name truepath))
               lspce--uri-path-allowed-chars)))))

(defun lspce--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri)
    (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-unhex-string (url-filename (url-generic-parse-url uri)))))
    (if (eq system-type 'windows-nt)
        (substring retval 1)
      retval)))

(defvar lspce--jsonrpc-id 1000000000)
(defsubst lspce--next-jsonrpc-id ()
  (setq lspce--jsonrpc-id (1+ lspce--jsonrpc-id))
  (format "%d" lspce--jsonrpc-id))

(defun lspce--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path t)))

(defun lspce--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[lspce] %s %s" (format-time-string "%Y-%m-%d %H:%M:%S.%3N") (apply #'format format args)))

(defun lspce--log (level format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[lspce] [%s] %s %s" level (format-time-string "%Y-%m-%d %H:%M:%S.%3N") (apply #'format format args)))

(defmacro lspce--error (format &rest args)
  `(when (<= lspce-log-level LSPCE-LOG-ERROR)
     (apply #'lspce--log "ERROR" ,format (list ,@args))))

(defmacro lspce--info (format &rest args)
  `(when (<= lspce-log-level LSPCE-LOG-INFO)
     (apply #'lspce--log "INFO" ,format (list ,@args))))

(defmacro lspce--warn (format &rest args)
  `(when (<= lspce-log-level LSPCE-LOG-WARN)
     (apply #'lspce--log "WARN" ,format (list ,@args))))

(defmacro lspce--debug (format &rest args)
  `(when (<= lspce-log-level LSPCE-LOG-DEBUG)
     (apply #'lspce--log "DEBUG" ,format (list ,@args))))

(defvar lspce--log-perf-enabled nil)
(defmacro lspce--log-perf (format &rest args)
  `(when lspce--log-perf-enabled
     (apply #'lspce--log "PERF" ,format (list ,@args))))

(defvar lspce--log-temp-enabled nil)
(defmacro lspce--log-temp (format &rest args)
  `(when lspce--log-temp-enabled
     (apply #'lspce--log "TEMP" ,format (list ,@args))))

(defun lspce--download-file (source-url dest-location)
  "Download a file from a URL at SOURCE-URL and save it to file at DEST-LOCATION."
  (let* ((dest-dir     (file-name-directory dest-location))
         (dest-abspath (expand-file-name dest-location)))
    (unless (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (lspce--message "Downloading %s\n to %s." source-url dest-abspath)
    (url-copy-file  source-url dest-abspath t)))

(defun lspce--yas-expand-snippet (snippet &optional start end expand-env)
  "Expand SNIPPET at current point.

Text between START and END will be deleted before inserting
template.  EXPAND-ENV is a list of (SYM VALUE) let-style dynamic
bindings considered when expanding the snippet.  If omitted, use
SNIPPET's expand-env field.

SNIPPET may be a snippet structure (e.g., as returned by
`yas-lookup-snippet'), or just a snippet body (which is a string
for normal snippets, and a list for command snippets)."
  ;; If not a snippet, no need to invoke the expensive snippet expanding.
  (if (not (string-search "$" snippet))
      (insert snippet)
    (cl-assert (and yas-minor-mode
                    (memq 'yas--post-command-handler post-command-hook))
               nil
               "[yas] `yas-expand-snippet' needs properly setup `yas-minor-mode'")
    (run-hooks 'yas-before-expand-snippet-hook)

    (let* ((clear-field
            (let ((field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field))))
              (and field (yas--skip-and-clear-field-p
                          field (point) (point) 0)
                   field)))
           (start (point))
           (end (point))
           (to-delete (and (> end start)
                           (buffer-substring-no-properties start end)))
           (yas-selected-text
            (cond (yas-selected-text)
                  ((and (region-active-p)
                        (not clear-field))
                   to-delete))))
      (goto-char start)
      (setq yas--indent-original-column (current-column))
      ;; Delete the region to delete, this *does* get undo-recorded.
      (when to-delete
        (delete-region start end))

      (let ((content snippet))
        (cond ((listp content)
               ;; x) This is a snippet-command.
               (yas--eval-for-effect content))
              (t
               ;; x) This is a snippet-snippet :-)
               (setq yas--start-column (current-column))
               ;; Stacked expansion: also shoosh the overlay modification hooks.
               (let ((yas--inhibit-overlay-hooks t))
                 (setq snippet
                       (yas--snippet-create content expand-env start (point))))

               ;; Stacked-expansion: This checks for stacked expansion, save the
               ;; `yas--previous-active-field' and advance its boundary.
               (let ((existing-field (and yas--active-field-overlay
                                          (overlay-buffer yas--active-field-overlay)
                                          (overlay-get yas--active-field-overlay 'yas--field))))
                 (when existing-field
                   (setf (yas--snippet-previous-active-field snippet) existing-field)
                   (yas--advance-end-maybe-previous-fields
                    existing-field (overlay-end yas--active-field-overlay)
                    (cdr yas--active-snippets))))

               ;; Exit the snippet immediately if no fields.
               (unless (yas--snippet-fields snippet)
                 (yas-exit-snippet snippet))

               ;; Now, schedule a move to the first field.
               (let ((first-field (car (yas--snippet-fields snippet))))
                 (when first-field
                   (sit-for 0) ;; fix issue 125
                   (yas--letenv (yas--snippet-expand-env snippet)
                     (yas--move-to-field snippet first-field))
                   (when (and (eq (yas--field-number first-field) 0)
                              (> (length (yas--field-text-for-display
                                          first-field))
                                 0))
                     ;; Keep region for ${0:exit text}.
                     (setq deactivate-mark nil))))
               (yas--message 4 "snippet %d expanded." (yas--snippet-id snippet))
               t))))))

(defun lspce--json-deserialize (str)
  "Parse the JSON STRING into a Lisp object."
  (json-parse-string str :array-type 'list :null-object nil :false-object nil))

(defalias 'lspce--json-serialize #'json-encode)

(provide 'lspce-util)
