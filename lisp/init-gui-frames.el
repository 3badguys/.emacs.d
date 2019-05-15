;;; init-gui-frames.el --- Behaviour specific to non-TTY frames

;; suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)

;; show a marker in the left fringe for lines not in the buffer
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; full screen after init emacs
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; no tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; no scroll bar
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode -1))
;; no menu bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; set cursor type, bar or box
(setq-default cursor-type 'box)

(provide 'init-gui-frames)
