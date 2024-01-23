;;; init-elpa.el --- Settings and helpers for package.el

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(provide 'init-elpa)
