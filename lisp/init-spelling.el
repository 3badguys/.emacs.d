;;; init-seplling.el --- Config for spelling checkout

(require 'ispell)

;; Spell check through aspell
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(provide 'init-spelling)