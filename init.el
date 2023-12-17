;;  ____  _                _
;; |__ / | |__   __ _   __| |  __ _   _  _   _  _   ___
;;  |_ \ | '_ \ / _` | / _` | / _` | | || | | || | (_-<
;; |___/ |_.__/ \__,_| \__,_| \__, |  \_,_|  \_, | /__/
;;                            |___/          |__/

;;; init.el --- Load the full configuration

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Check the emacs version
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(defvar best-gc-cons-threshold
  4000000
  "Best default gc threshold value.  Should NOT be too big!")

;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Personal information
(setq user-full-name "3badguys")
(setq user-mail-address "chuiC456@163.com")

;; Add site-lisp's subdirs to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "site-lisp" user-emacs-directory))

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
  (require 'init-xah-fly-keys)
  (require 'init-gui-frames)
  (require 'init-sessions)
  (require 'init-edit-utils)
  (require 'init-search)
  (require 'init-abbrev)
  (require 'init-hippie-expand)
  (require 'init-dired)
  (require 'init-tags)
  (require 'init-company)
  (require 'init-org)
  (require 'init-lisp)
  (require 'init-c)
  (require 'init-perl)
  (require 'init-python)

  ;; Variables configured via the interactive 'customize' interface
  (load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)) t t)

  ;; Allow users to provide an option "init-local" containing presonal settings
  (require 'init-local nil t)

  ;; Load personal tbg-xxx packages
  (require 'tbg-cloc)
  (require 'tbg-find-replace)
  (require 'tbg-header)
  ;;
  )

(defalias 'yes-or-no-p 'y-or-n-p)

(setq gc-cons-threshold best-gc-cons-threshold)

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

(provide 'init)
