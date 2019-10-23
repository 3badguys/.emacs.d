;;; init-highlight.el --- Initialize highlight configurations.

;; Highlight the current line
(when (display-graphic-p)
  (global-hl-line-mode t))

;; symbol-overlay mode for highlighting the symbol
(require 'symbol-overlay)
(dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(after-load 'symbol-overlay
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-put)
  (global-set-key (kbd "<f9>") 'symbol-overlay-remove-all)
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev))

;; Highlight indentation, only use in graphic environment
(when (display-graphic-p)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top))

(provide 'init-highlight)
