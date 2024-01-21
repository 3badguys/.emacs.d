;;; init-elpa.el --- Settings and helpers for package.el

(package-initialize)

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

(provide 'init-elpa)
