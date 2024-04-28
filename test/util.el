;;; -*- lexical-binding: t -*-

(require 'lspce)
(require 'ert)


(defvar lspce--test-dir nil)
(setf lspce--test-dir (file-name-directory (or load-file-name (buffer-file-name))))


(defmacro lspce-def-test (name args &rest body)
  (declare (debug (symbol sexp body)) (indent 2))
  `(ert-deftest ,name ,args
     ,@body))

(defmacro lspce-test-in-buffer (filename &rest body)
  (declare (debug (form body)) (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,filename lspce--test-dir) t)
     (normal-mode)
     (lspce-mode 1)
     ,@body))


(provide 'test/util)
