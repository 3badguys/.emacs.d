;;; init-elpa.el --- Settings and helpers for package.el

(package-initialize)

(require 'cl)

(when (>= emacs-major-version 24)
  (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			               ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; Add Packages
(defvar chuic456_emacs/packages '(
                                  exec-path-from-shell
                                  smex
                                  magit
                                  diminish
                                  which-key
                                  command-log-mode
                                  cnfonts
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
                                  ) "Default packages")

(setq package-selected-packages chuic456_emacs/packages)

(defun chuic456_emacs/packages-installed-p ()
  (loop for pkg in chuic456_emacs/packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (chuic456_emacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg chuic456_emacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-elpa)
