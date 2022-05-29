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
    ;; (setq options (lspce--add-option "settings.java.server.launchMode" "LightWeight" options))
    (setq options (lspce--add-option "settings.java.completion.enabled" t options))
    (setq options (lspce--add-option "settings.java.completion.maxResults" 30 options))
    (setq options (lspce--add-option "settings.java.completion.importOrder" (vector "java" "javax" "com" "org") options))
    (setq options (lspce--add-option "settings.java.completion.guessMethodArguments" :json-false options))
    (setq options (lspce--add-option "settings.java.progressReports.enabled" t options))
    (setq options (lspce--add-option "settings.java.foldingRange.enabled" :json-false options))
    (setq options (lspce--add-option "settings.java.maxConcurrentBuilds" 2 options))
    (setq options (lspce--add-option "settings.java.autobuild.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.maven.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.wrapper.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.offline.enabled" t options))
    (setq options (lspce--add-option "settings.java.maven.downloadSources" :json-false options))
    (setq options (lspce--add-option "settings.java.maven.updateSnapshots" :json-false options))
    (setq options (lspce--add-option "settings.java.trace.server" "off" options))
    (setq options (lspce--add-option "settings.java.configuration.updateBuildConfiguration" "automatic" options))
    (setq options (lspce--add-option "settings.java.configuration.checkProjectSettingsExclusions" t options))
    (setq options (lspce--add-option "settings.java.configuration.runtimes" (vector (list :name "JavaSE-1.8" :path "/usr/lib/jvm/java-8-openjdk/" :default t)
                                                                                    (list :name "JavaSE-11" :path "/usr/lib/jvm/java-11-openjdk/"))
                                     options))
    (setq options (lspce--add-option "settings.java.showBuildStatusOnStart.enabled" t options))
    options
    )
  )

(defun lspce-pyright-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    ;; (setq options (lspce--add-option "python.analysis.autoImportCompletions" t options))
    (setq options (lspce--add-option "python.analysis.useLibraryCodeForTypes" t options))
    (setq options (lspce--add-option "python.analysis.typeCheckingMode" "basic" options))
    (setq options (lspce--add-option "python.analysis.stubPath" "" options))
    (setq options (lspce--add-option "python.analysis.autoSearchPaths" t options))
    (setq options (lspce--add-option "python.analysis.typeshedPaths" (vector) options))
    (setq options (lspce--add-option "python.analysis.extraPaths" (vector) options))
    options
    )
  )


(provide 'lspce-lang-options)
