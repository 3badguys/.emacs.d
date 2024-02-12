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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Firstly set package.el
(require 'init-elpa)

;; To disable collection of benchmark data after init is done.
(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Add site-lisp's subdirs to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Bootstrap config
(require 'init-xah-fly-keys)
(require 'init-gui-frames)
(require 'init-sessions)
(require 'init-edit-utils)
(require 'init-search)
(require 'init-hippie-expand)
(require 'init-dired)
(require 'init-tags)
(require 'init-org)
(require 'init-program)

;; Variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file "NOERROR" "NOMESSAGE")

;; Load personal tbg-xxx packages
(require 'tbg-cloc)
(require 'tbg-find-replace)
(require 'tbg-header)

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init)

;;; init.el ends here
