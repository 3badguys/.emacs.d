;;; early-init.el --- Emacs 27+ pre-initialisation config -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads early-init.el before init.el.

;;; Code:


(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024))))

(provide 'early-init)

;;; early-init.el ends here
