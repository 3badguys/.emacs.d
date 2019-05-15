;;; init.el --- Load the full configuration

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Check the emacs version
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(setq emacs-load-start-time (current-time))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let* ((file-name-handler-alist nil))
  (require 'init-utils)
  ;; Call (package-initialize)
  (require 'init-elpa)      ;; Install required packages
  (require 'init-exec-path) ;; Set up $PATH

  ;; Allow users to provide an optional "init-preload-local.el"
  (require 'init-preload-local nil t)

  ;; Load configs for specific features and modes
  (require 'init-gui-frames)
  (require 'init-windows)
  (require 'init-sessions)
  (require 'init-edit-utils)
  (require 'init-company)
  (require 'init-cnfonts)
  (require 'init-search) ;; Search file, content, command, etc.
  (require 'init-abbrev)
  (require 'init-hippie-expand)
  (require 'init-recentf)
  (require 'init-dired)
  (require 'init-paredit)
  (require 'init-which-key)
  (require 'init-iedit)
  (require 'init-popwin)
  (require 'init-org)
  (require 'init-lisp)
  (require 'init-c)
  (require 'init-go)
  (require 'init-perl)

  ;; Variables configured via the interactive 'customize' interface
  (load (setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory)) t t)

  ;; Allow users to provide an option "init-local" containing presonal settings
  (require 'init-local nil t))

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

(provide 'init)
