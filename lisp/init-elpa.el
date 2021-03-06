;;; init-elpa.el --- Settings and helpers for package.el

(package-initialize)

(require 'cl)

(setq package-archives
      '(
        ;; uncomment below line if you need use GNU ELPA.
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")

        ;; Option: emacs-china repository
        ;; ("gnu" . "http://elpa.emacs-china.org/gnu/")
        ;; ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ;;
))

;; Add Packages
(defvar 3badguys-emacs/packages '(
                                  exec-path-from-shell
                                  smex
                                  magit
                                  which-key
                                  command-log-mode
                                  company
                                  pyim
                                  pyim-basedict
                                  haskell-mode
                                  anaconda-mode
                                  company-anaconda
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
