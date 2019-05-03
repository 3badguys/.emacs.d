;;; init-sessions.el --- Save and restore editor sessions between restarts

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'desktop)
(desktop-save-mode t)

(provide 'init-sessions)
