;;; init-elpa.el --- Settings and helpers for package.el

(package-initialize)

(require 'cl)

(when (>= emacs-major-version 24)
  (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			               ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; Add Packages
(defvar 3badguys-emacs/packages '(
                                  exec-path-from-shell
                                  smex
                                  magit
                                  diminish
                                  which-key
                                  command-log-mode
                                  rainbow-delimiters
                                  highlight-numbers
                                  symbol-overlay
                                  highlight-indent-guides
                                  goto-line-preview
                                  company
                                  info-colors
                                  haskell-mode
                                  anaconda-mode
                                  company-anaconda
                                  yaml-mode
                                  htmlize
                                  ) "Default packages")

(setq package-selected-packages 3badguys-emacs/packages)

(defun 3badguys-emacs/packages-installed-p ()
  (loop for pkg in 3badguys-emacs/packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (3badguys-emacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg 3badguys-emacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-elpa)
