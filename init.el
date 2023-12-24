;;  ____  _                _
;; |__ / | |__   __ _   __| |  __ _   _  _   _  _   ___
;;  |_ \ | '_ \ / _` | / _` | / _` | | || | | || | (_-<
;; |___/ |_.__/ \__,_| \__,_| \__, |  \_,_|  \_, | /__/
;;                            |___/          |__/

;;; init.el --- Load the full configuration -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; This configuration file is divided into a number of other files.

;;; Code:


;; Produce backtraces when errors occur
(setq debug-on-error t)

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
  (require 'init-org)
  (require 'init-lisp)

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

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

(provide 'init)

;;; init.el ends here
