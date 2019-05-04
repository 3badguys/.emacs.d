;;; init.el --- Load the full configuration

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Check the emacs version
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; Variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Allow users to provide an option "init-local" containing presonal settings
(require 'init-local nil t)

(provide 'init)
