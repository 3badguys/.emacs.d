;;; init-program.el --- Config for program

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; emacs lisp
(require 'xah-elisp-mode)

;; dockerfile
(use-package dockerfile-mode :ensure t)

;; ocaml
(use-package tuareg :ensure t)

(provide 'init-program)
