;;; init-elpa.el --- Settings and helpers for package.el

(package-initialize)

(require 'cl)

(when (>= emacs-major-version 24)
  (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			               ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; Add Packages
(defvar chuic456_emacs/packages '(
                                  exec-path-from-shell
                                  magit
                                  company
                                  hungry-delete
                                  swiper
                                  counsel
                                  smartparens
                                  popwin
                                  expand-region
                                  iedit
                                  geiser
                                  slime
                                  paredit
                                  windresize
                                  window-numbering
                                  which-key
                                  ;; ggtags
                                  ace-jump-mode
                                  symbol-overlay
                                  cnfonts
                                  session
                                  htmlize
                                  auto-complete
                                  go-mode
                                  go-autocomplete
                                  emms
                                  smooth-scrolling
                                  tabbar
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
