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
        highlight-indent-guides-responsive 'top)

  ;; Don't display first level of indentation
  (defun my-indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function
        #'my-indent-guides-for-all-but-first-column)

  ;; Disable `highlight-indent-guides-mode' in `swiper'
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (after-load 'ivy
    (defadvice ivy-cleanup-string (after my-ivy-cleanup-hig activate)
      (let ((pos 0) (next 0) (limit (length str)) (prop 'highlight-indent-guides-prop))
        (while (and pos next)
          (setq next (text-property-not-all pos limit prop nil str))
          (when next
            (setq pos (text-property-any next limit prop nil str))
            (remove-text-properties next pos '(display nil face nil) str))))))
  )

;; highlight-numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; rainbow-delimiters-mode
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; beacon-mode
(require 'beacon)
(setq-default beacon-blink-delay '0.2)
(setq-default beacon-blink-when-focused 't)
(setq-default beacon-dont-blink-commands 'nil)
(setq-default beacon-push-mark '1)
(add-hook 'after-init-hook 'beacon-mode)

;; info-colors
(require 'info-colors)
(after-load 'info
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(provide 'init-highlight)
