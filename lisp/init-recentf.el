;;; init-recentf.el --- Settings for tracking recent files

(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-menu-items 25)

(provide 'init-recentf)
