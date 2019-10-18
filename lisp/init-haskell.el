;;; init-haskell.el --- Config for haskell

(require 'haskell-mode)
(require 'haskell-indentation)

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)) ;; hoogle is a haskell API search engine

(provide 'init-haskell)
