;;; init-elpa.el --- Settings and helpers for package.el

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(setq load-prefer-newer t)

(package-initialize)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'init-elpa)
