;;; lspce.el --- LSP Client for Emacs -*- lexical-binding: t; -*-

(require 'f)

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
(defcustom lspce-jdtls-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "jdtls workspace directory."
  :group 'lspce
  :type 'directory)

(defcustom lspce-java-vmargs '("--add-modules=ALL-SYSTEM" "--add-opens java.base/java.util=ALL-UNNAMED" "--add-opens java.base/java.lang=ALL-UNNAMED" "-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx1536m" )
  "Specifies extra VM arguments used to launch the Java Language Server."
  :group 'lspce
  :risky t
  :type '(repeat string))

(defcustom lspce-java-path "java"
  "Path of the java executable."
  :group 'lspce
  :type 'string)

(defcustom lspce-jdtls-install-dir nil
  "Install directory for eclipse.jdt.ls-server."
  :group 'lspce
  :type 'directory)

(defun lspce--jdtls-workspace-dir ()
  (let ((proj (project-current))
        workspace)
    (unless (file-exists-p lspce-jdtls-workspace-dir)
      (make-directory lspce-jdtls-workspace-dir t))
    (setq workspace (if proj
                        (file-truename (project-root proj))
                      "DEFAULT"))
    (f-join lspce-jdtls-workspace-dir (string-replace "/" "!" workspace))))

(defun lspce--jdtls-workspace-cache-dir ()
  (let ((cache-dir (f-join lspce-jdtls-workspace-dir ".cache/")))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    cache-dir))

(defun lspce--jdtls-locate-server-jar ()
  "Return the jar file location of the language server.
The entry point of the language server is in `lspce-jdtls-install-dir'/plugins/org.eclipse.equinox.launcher_`version'.jar."
  (pcase (f-glob "org.eclipse.equinox.launcher_*.jar" (expand-file-name "plugins" lspce-jdtls-install-dir))
    (`(,single-entry) single-entry)
    (`nil nil)
    (server-jar-filenames
     (error "Unable to find single point of entry %s" server-jar-filenames))))

(defun lspce--jdtls-locate-server-config ()
  "Return the server config based on OS."
  (let ((config (cond
                 ((string-equal system-type "windows-nt") ; Microsoft Windows
                  "config_win")
                 ((string-equal system-type "darwin") ; Mac OS X
                  "config_mac")
                 (t "config_linux"))))
    (let ((inhibit-message t))
      (message (format "using config for %s" config)))
    (expand-file-name config lspce-jdtls-install-dir)))            

(defun lspce-jdtls-cmd-args ()
  (let ((server-jar (lspce--jdtls-locate-server-jar))
        (server-config (lspce--jdtls-locate-server-config))
        (data (lspce--jdtls-workspace-dir)))
    (mapconcat #'identity `("-Declipse.application=org.eclipse.jdt.ls.core.id1"
                            "-Dosgi.bundles.defaultStartLevel=4"
                            "-Declipse.product=org.eclipse.jdt.ls.core.product"
                            "-Dlog.protocol=true"
                            "-Dlog.level=ALL"
                            ,@lspce-java-vmargs
                            "-jar"
                            ,server-jar
                            "-configuration"
                            ,server-config
                            "-data"
                            ,data
                            ) " ")))

(defun lspce-jdtls-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    ;; (setq options (lspce--add-option "settings.java.server.launchMode" "Hybrid" options))
    (setq options (lspce--add-option "settings.java.server.launchMode" "LightWeight" options))
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
