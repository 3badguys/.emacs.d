;;; init-gui-frames.el --- Behaviour specific to non-TTY frames

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode -1))

(setq inhibit-splash-screen t)

(setq-default cursor-type 'box)  ;; bar or box

(setq initial-frame-alist (quote ((fullscreen . maximized))))

(provide 'init-gui-frames)
