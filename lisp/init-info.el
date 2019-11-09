;;; init-info.el --- Configurations for info mode.

;; info-colors
(require 'info-colors)
(after-load 'info
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; keybindings
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(provide 'init-info)
