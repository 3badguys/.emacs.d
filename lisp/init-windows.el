;;; init-windows.el --- Working with windows within frames

;; Show the number of window
(require 'window-numbering)
(window-numbering-mode 1)

;; Resize window
(require 'windresize)
(global-set-key (kbd "C-c r") 'windresize)

;; Switch window
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(provide 'init-windows)

