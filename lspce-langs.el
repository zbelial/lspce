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
        (puthash left (lspce--add-option-internal remain value ht) options)))
    options))

;;; rust rust-analyzer
(defun lspce-ra-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    (setq options (lspce--add-option "diagnostics.enable" t options))
    (setq options (lspce--add-option "enableExperimental" :json-false options))
    (setq options (lspce--add-option "cargo.features" "all" options))
    (setq options (lspce--add-option "cargo.noDefaultFeatures" :json-false options))
    (setq options (lspce--add-option "cargo.runBuildScripts" t options))
    (setq options (lspce--add-option "cargo.loadOutDirsFromCheck" t options))
    (setq options (lspce--add-option "cargo.autoreload" t options))
    (setq options (lspce--add-option "cargo.useRustcWrapperForBuildScripts" t options))
    (setq options (lspce--add-option "completion.addCallParenthesis" t options))
    (setq options (lspce--add-option "completion.addCallArgumentSnippets" :json-false options))
    (setq options (lspce--add-option "completion.postfix.enable" :json-false options))
    (setq options (lspce--add-option "completion.autoimport.enable" t options))
    (setq options (lspce--add-option "procMacro.enable" t options))
    (setq options (lspce--add-option "lens.enable" :json-false options))
    options
    )
  )


;;; java 
(defcustom lspce-jdtls-workspace-dir (expand-file-name "~/.jdtls/workspace/")
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

(defcustom lspce-jdtls-download-url
  "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  "URL to download the Eclipse JDT language server."
  :type 'string
  :group 'lspce)

(defcustom lspce-jdtls-install-dir nil
  "Install directory for eclipse.jdt.ls-server."
  :group 'lspce
  :type 'directory)

(defcustom lspce-jdtls-launch-mode "Hybrid"
  "The launch mode for the Java extension"
  :group 'lspce
  :type '(choice (:tag "Standard" "LightWeight" "Hybrid")))

(declare-function tar-untar-buffer "tar-mode" ())
;;;###autoload
(defun lspce-install-jdtls-server ()
  "Install the Eclipse JDT LSP server."
  (interactive)
  (let* ((dest-dir (expand-file-name lspce-jdtls-install-dir))
         (dest-dir-bak (concat (string-trim-right dest-dir (f-path-separator)) ".bak" (format ".%s" (format-time-string "%Y%m%d%H%M%S"))))
         (download-url lspce-jdtls-download-url)
         (dest-filename (file-name-nondirectory download-url))
         (dest-abspath (expand-file-name dest-filename dest-dir))
         (large-file-warning-threshold nil))
    (when (file-exists-p dest-dir)
      (f-move dest-dir dest-dir-bak))
    (f-mkdir-full-path dest-dir)
    (lspce--message "Installing Eclipse JDT LSP server, please wait...")
    (lspce--download-file download-url dest-abspath)
    (lspce--message "Extracting Eclipse JDT LSP archive, please wait...")
    (with-temp-buffer
      (let ((temporary-buffer (find-file dest-abspath)))
        (goto-char (point-min))
        (tar-untar-buffer)
        (kill-buffer temporary-buffer)))
    (lspce--message "Eclipse JDT LSP server installed in folder \n\"%s\"." dest-dir)))

(defun lspce--jdtls-workspace-dir ()
  (let ((proj (project-current))
        workspace)
    (lspce--ensure-dir lspce-jdtls-workspace-dir)
    (setq workspace (if proj
                        (file-truename (project-root proj))
                      "DEFAULT"))
    (f-join lspce-jdtls-workspace-dir (string-replace "/" "!" workspace))))

(defun lspce--jdtls-workspace-cache-dir ()
  (let ((cache-dir (f-join (lspce--jdtls-workspace-dir) ".cache/")))
    (lspce--ensure-dir cache-dir)
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

(defvar lspce--jdt-link-pattern "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?")
(defun lspce--jdtls-get-jdt-filename (uri)
  "Get the name of the buffer calculating it based on URL."
  (or (save-match-data
        (when (string-match lspce--jdt-link-pattern uri)
          (format "%s.java"
                  (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
      (save-match-data
        (when (string-match
               "jdt://.*?/\\(.*?\\)\\?=\\(.*?\\)/.*/\\(.*\\)"
               (url-unhex-string uri))
          (format "%s(%s)" (match-string 2 uri) (string-replace "\\" "" (string-replace "/" "" (match-string 4 uri))))))
      (save-match-data
        (when (string-match "chelib://\\(.*\\)" uri)
          (let ((matched (match-string 1 uri)))
            (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
      (error "Unable to match %s" uri)))

(defun lspce--jdtls-open-jdt-link (uri)
  (let ((filename (lspce--jdtls-get-jdt-filename uri))
        fullname content)
    (when filename 
      (setq fullname (f-join (lspce--jdtls-workspace-cache-dir) filename))
      ;; FIXME use a cache to reduce requesting
      (lspce--info "jdt link uri %s" uri)
      (setq content (lspce--request "java/classFileContents" (list :uri uri)))
      (when content
        (with-temp-file fullname
          (erase-buffer)
          (insert content))))
    fullname))

(defun lspce-jdtls-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    (setq options (lspce--add-option "settings.java.server.launchMode" lspce-jdtls-launch-mode options))
    (setq options (lspce--add-option "settings.java.completion.enabled" t options))
    (setq options (lspce--add-option "settings.java.completion.maxResults" 30 options))
    (setq options (lspce--add-option "settings.java.completion.importOrder" (vector "java" "javax" "com" "org") options))
    (setq options (lspce--add-option "settings.java.completion.guessMethodArguments" t options))
    (setq options (lspce--add-option "settings.java.signatureHelp.enabled" t options))
    (setq options (lspce--add-option "settings.java.progressReports.enabled" t options))
    (setq options (lspce--add-option "settings.java.foldingRange.enabled" :json-false options))
    (setq options (lspce--add-option "settings.java.maxConcurrentBuilds" 1 options))
    (setq options (lspce--add-option "settings.java.autobuild.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.maven.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.wrapper.enabled" t options))
    (setq options (lspce--add-option "settings.java.import.gradle.offline.enabled" t options))
    (setq options (lspce--add-option "settings.java.maven.downloadSources" :json-false options))
    (setq options (lspce--add-option "settings.java.maven.updateSnapshots" :json-false options))
    (setq options (lspce--add-option "settings.java.project.importHint" t options))
    (setq options (lspce--add-option "settings.java.project.importOnFirstTimeStartup" "automatic" options))
    (setq options (lspce--add-option "settings.java.project.referecedLibraries" (vector "lib/**/*.jar") options))
    (setq options (lspce--add-option "settings.java.trace.server" "off" options))
    (setq options (lspce--add-option "settings.java.configuration.updateBuildConfiguration" "automatic" options))
    (setq options (lspce--add-option "settings.java.configuration.checkProjectSettingsExclusions" t options))
    (setq options (lspce--add-option "settings.java.showBuildStatusOnStart.enabled" t options))
    (setq options (lspce--add-option "extendedClientCapabilities.classFileContentsSupport" t options))
    options
    )
  )

;;; python pyright
(defun lspce-pyright-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    (setq options (lspce--add-option "python.analysis.autoImportCompletions" t options))
    (setq options (lspce--add-option "python.analysis.useLibraryCodeForTypes" t options))
    (setq options (lspce--add-option "python.analysis.typeCheckingMode" "basic" options))
    (setq options (lspce--add-option "python.analysis.diagnosticMode" "openFilesOnly" options))
    (setq options (lspce--add-option "python.analysis.stubPath" "" options))
    (setq options (lspce--add-option "python.analysis.autoSearchPaths" t options))
    (setq options (lspce--add-option "python.analysis.typeshedPaths" (vector) options))
    (setq options (lspce--add-option "python.analysis.extraPaths" (vector) options))
    options
    )
  )


;;; go gopls
(defun lspce-gopls-initializationOptions ()
  (let ((options (make-hash-table :test #'equal)))
    (setq options (lspce--add-option "settings.gopls.usePlaceholders" t options))
    options
    )
  )

(provide 'lspce-langs)
