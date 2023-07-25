;;; lspce-snippet.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

;; A simplified snippet expanding implementation.
(defun lspce--expand-snippet (snippet &optional expand-env)
  "Expand SNIPPET at current point.

EXPAND-ENV is a list of (SYM VALUE) let-style dynamic
bindings considered when expanding the snippet.  If omitted, use
SNIPPET's expand-env field.

SNIPPET is just a snippet body (which is a string
for normal snippets)."
  ;; If not a snippet, no need to invoke the expensive snippet expanding.
  (if (not (string-search "$" snippet))
      (insert snippet)
    (cl-assert (and yas-minor-mode
                    (memq 'yas--post-command-handler post-command-hook))
               nil
               "[yas] `yas-expand-snippet' needs properly setup `yas-minor-mode'")

    (let* ((start (point))
           (end (point)))
      (goto-char start)
      (setq yas--indent-original-column (current-column))

      (let ((content snippet))
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
        t))))

(provide 'lspce-snippet)
