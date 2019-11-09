;;; init-c.el --- Configurations for C programming language.

(defun 3badguys-c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq 3badguys-c-mode-common-hook '3badguys-c-mode-common-defaults)

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks '3badguys-c-mode-common-hook)))

(defun 3badguys-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(setq 3badguys-makefile-mode-hook '3badguys-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks '3badguys-makefile-mode-hook)))

(provide 'init-c)
