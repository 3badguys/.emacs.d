;;; init-sessions.el --- Save and restore editor sessions between restarts

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'desktop)
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode t)
;; The default desktop is loaded anyway if it is locked
(setq desktop-load-locked-desktop t)

;; Copy from https://www.emacswiki.org/emacs/Desktop
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(provide 'init-sessions)
