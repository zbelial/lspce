;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs))

(defconst LSPCE-VERSION "0.3")
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

(defconst lspce--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil) ;; see github#639
    vec)
  "Like `url-path-allows-chars' but more restrictive.")

(defun lspce--path-to-uri (path)
  "URIfy PATH."
  (let ((truepath (file-truename path)))
    (concat "file://"
            ;; Add a leading "/" for local MS Windows-style paths.
            (if (and (eq system-type 'windows-nt)
                     (not (file-remote-p truepath)))
                "/")
            (url-hexify-string
             ;; Again watch out for trampy paths.
             (directory-file-name (file-local-name truepath))
             lspce--uri-path-allowed-chars))))

(defun lspce--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri)
    (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-unhex-string (url-filename (url-generic-parse-url uri)))))
    (if (eq system-type 'windows-nt)
        (substring retval 1)
      retval)))

(defvar lspce--jsonrpc-id 0)
(defsubst lspce--next-jsonrpc-id ()
  (setq lspce--jsonrpc-id (1+ lspce--jsonrpc-id))
  lspce--jsonrpc-id)
(defsubst lspce--current-jsonrpc-id ()
  lspce--jsonrpc-id)

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

(defun lspce--error (format &rest args)
  (when (<= lspce-log-level LSPCE-LOG-ERROR)
    (apply #'lspce--log "ERROR" format args)
    (display-warning 'lspce-mode (apply #'format-message format args) :error)))

(defun lspce--warn (format &rest args)
  (when (<= lspce-log-level LSPCE-LOG-WARN)
    (apply #'lspce--log "WARN" format args)))

(defun lspce--info (format &rest args)
  (when (<= lspce-log-level LSPCE-LOG-INFO)
    (apply #'lspce--log "INFO" format args)))

(defun lspce--debug (format &rest args)
  (when (<= lspce-log-level LSPCE-LOG-DEBUG)
    (apply #'lspce--log "DEBUG" format args)))

(defvar lspce--log-perf-enabled nil)
(defun lspce--log-perf (format &rest args)
  (when lspce--log-perf-enabled
    (apply #'lspce--log "PERF" format args)))

(defun lspce--download-file (source-url dest-location)
  "Download a file from a URL at SOURCE-URL and save it to file at DEST-LOCATION."
  (let* ((dest-dir     (file-name-directory dest-location))
         (dest-abspath (expand-file-name dest-location)))
    (unless (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (lspce--message "Downloading %s\n to %s." source-url dest-abspath)
    (url-copy-file  source-url dest-abspath t)))

(provide 'lspce-util)
