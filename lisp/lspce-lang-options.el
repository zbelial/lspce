;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-


(defun lspce--add-option (option value options)
  (when (not (hash-table-p options))
    (error "lspce--add-option: internal error."))
  (lspce--add-option-internal option value options))

(defun lspce--add-option-internal (option value options)
  (let ((sep (string-search "." option))
        left remain ht)
    (if (null sep)
        (progn
          (when (null options)
            (setq options (make-hash-table :test #'equal)))
          (puthash option value options))
      (progn
        (setq left (substring option 0 sep)
              remain (substring option (+ sep 1)))
        (when (null options)
          (setq options (make-hash-table :test #'equal)))
        (setq ht (gethash left options))
        (puthash left (lspce--add-option-internal remain value ht) options)
        )
      )
    options))

;;; rust rust-analyzer
(defun lspce-ra-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    (setq options (lspce--add-option "diagnostics.enable" t options))
    (setq options (lspce--add-option "enableExperimental" :json-false options))
    (setq options (lspce--add-option "cargo.allFeatures" :json-false options))
    (setq options (lspce--add-option "cargo.noDefaultFeatures" :json-false options))
    (setq options (lspce--add-option "cargo.runBuildScripts" t options))
    (setq options (lspce--add-option "cargo.loadOutDirsFromCheck" t options))
    (setq options (lspce--add-option "cargo.autoreload" t options))
    (setq options (lspce--add-option "cargo.useRustcWrapperForBuildScripts" t options))
    (setq options (lspce--add-option "completion.addCallParenthesis" t options))
    (setq options (lspce--add-option "completion.addCallArgumentSnippets" :json-false options))
    (setq options (lspce--add-option "completion.postfix.enable" :json-false options))
    (setq options (lspce--add-option "procMacro" t options))
    options
    )
  )


;;; java 
(defvar lspce--jdtls-workspace-dir "/home/lucency/.emacs.d/data/unsync/lspce-jdtls/workspace/")
(defcustom lspce-java-vmargs '("--jvm-arg=-XX:+UseParallelGC" "--jvm-arg=-XX:GCTimeRatio=4" "--jvm-arg=-XX:AdaptiveSizePolicyWeight=90" "--jvm-arg=-Dsun.zip.disableMemoryMapping=true" "--jvm-arg=-Xmx1536m" )
  "Specifies extra VM arguments used to launch the Java Language Server."
  :group 'lsp-java
  :risky t
  :type '(repeat string))

(defun lspce-jdtls-cmd-args ()
  (let ((args ""))
    (unless (file-exists-p lspce--jdtls-workspace-dir)
      (make-directory lspce--jdtls-workspace-dir t))
    (setq args (concat args
                       (mapconcat #'identity lspce-java-vmargs " ")
                       " -data " lspce--jdtls-workspace-dir))
    args
    )
  )


(defun lspce-jdtls-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    (setq options (lspce--add-option "settings.java.server.launchMode" "Hybrid" options))
    (setq options (lspce--add-option "settings.java.completion.enabled" t options))
    (setq options (lspce--add-option "settings.java.maxConcurrentBuilds" 1 options))
    (setq options (lspce--add-option "settings.java.autobuild.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.maven.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.enabled" t options))
    (setq options (lspce--add-option "settings.java.configuration.updateBuildConfiguration" "automatic" options))
    (setq options (lspce--add-option "settings.java.configuration.checkProjectSettingsExclusions" t options))
    options
    )
  )


(provide 'lspce-lang-options)
