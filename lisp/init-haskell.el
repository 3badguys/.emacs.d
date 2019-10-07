;;; init-haskell.el --- Config for haskell

(require 'haskell-mode)

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'init-haskell)