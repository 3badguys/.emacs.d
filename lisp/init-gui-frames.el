;;; init-gui-frames.el --- Behaviour specific to non-TTY frames

;; suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)

;; show a marker in the left fringe for lines not in the buffer
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Note: Use tool-bar-lines faster than setting `(tool-bar-mode -1)`
;; set initial-frame-alist and default-frame-alist
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (background-color . "honeydew")
              ;; If you want to set full screen, use: (fullscreen . maximized)
              (width . 100)
              (height . 45)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (background-color . "honeydew")
              (width . 100)
              (height . 45)))
      (when (equal window-system 'x)
        (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9")))
      )
  (progn
    (setq initial-frame-alist '((tool-bar-lines . 0)))
    (setq default-frame-alist '((tool-bar-lines . 0)))
    ))

;; cursor
;; Not set cursor-type to avoid the conflict with xah-fly-keys.
;; (setq-default cursor-type 'box)
(add-hook 'window-setup-hook
          '(lambda ()
             (set-cursor-color "red")))
;; disable cursor blink
(blink-cursor-mode 0)

;; Time management
(setq display-time-24hr-format t) ; the date in modeline is English too, magic
(setq display-time-day-and-date t)
(display-time-mode) ; show date in modeline

;; whitespace-mode
(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          ))
  )

;; display file size in mode line
(size-indication-mode t)

;; Use diminish to remove the minor-mode in mode line
(require 'diminish)
(add-hook 'window-setup-hook
          (lambda ()
            (dolist (mm minor-mode-alist)
              (diminish (car mm)))))

(provide 'init-gui-frames)
