;;; -*- lexical-binding: t -*-

(require 'test/util)


(defvar java-mode-hook)
(defvar java-ts-mode-hook)

(defmacro lspce-def-jdtls-test (name args &rest body)
  (declare (debug (symbol sexp body)) (indent 2))
  `(lspce-def-test ,name ,args
     (unless (executable-find "jdtls")
       (ert-skip "JDTLS is not installed"))
     (let ((lspce-server-programs `(("java" "jdtls")))
           ;; Disable customizations while the test is running.
           (java-mode-hook        nil)
           (java-ts-mode-hook     nil))
       ,@body)))


(lspce-def-jdtls-test lspce-jdtls-hello-world ()
  (lspce-test-in-buffer "java/hello-world/my/HelloWorld.java"
    ;; Just assert that it has started up.
    (should lspce-mode)))

(lspce-def-jdtls-test lspce-jdtls-hello-world-xref ()
  (lspce-test-in-buffer "java/hello-world/my/HelloWorld.java"
    (search-forward "sayHelloTo")  ; FIXME: Fails without this line; is it supposed to be like that or is it a bug?
    (should-not (looking-at (rx "sayHelloTo (String whom)")))
    (xref-find-definitions "sayHelloTo")
    (should (looking-at (rx "sayHelloTo (String whom)")))))


(provide 'test/jdtls)
