;;; tabbar.el --- Settings fors tabbar

(require 'tabbar)
(tabbar-mode 1)

(global-set-key (kbd "<M-up>") 'tabbar-backward-group)
(global-set-key (kbd "<M-down>") 'tabbar-forward-group)
(global-set-key (kbd "<M-left>") 'tabbar-backward)
(global-set-key (kbd "<M-right>") 'tabbar-forward)

(set-face-attribute
 'tabbar-default nil
 :family "DejaVu Sans Mono"
 :background "gray80"
 :foreground "gray30"
 :height 1.0
 )
(set-face-attribute
 'tabbar-button nil
 :inherit 'tabbar-default
 :box '(:line-width 1 :color "yellow70")
 )
(set-face-attribute
 'tabbar-selected nil
 :inherit 'tabbar-default
 :foreground "DarkGreen"
 :background "LightGoldenrod"
 :box '(:line-width 2 :color "DarkGoldenrod")
 :overline "black"
 :underline "black"
 :weight 'bold
 )
(set-face-attribute
 'tabbar-unselected nil
 :inherit 'tabbar-default
 :box '(:line-width 2 :color "#00B2BF")
 )

(provide 'init-tabbar)
