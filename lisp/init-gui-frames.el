;;; init-gui-frames.el --- Behaviour specific to non-TTY frames

;; suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)

;; initial window and default window
(setq default-frame-alist
      (if (display-graphic-p)
          ;; Use tool-bar-lines faster than setting `(tool-bar-mode -1)`
          '((tool-bar-lines . 0)
            (background-color . "honeydew"))))

;; set default font
(set-frame-font
 (cond
  ((string-equal system-type "gnu/linux")
   (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-8" nil))
  ((string-equal system-type "windows-nt")
   (if (member "Consolas" (font-family-list)) "Consolas-9" nil))
  ((string-equal system-type "darwin")
   (if (member "Monaco" (font-family-list)) "Monaco-10" nil))
  (t nil))
 t t)

;; some base preferences
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; highlight the current line
(global-hl-line-mode 1)

;; show cursor position within line
(column-number-mode 1)

;; disable cursor blink
(blink-cursor-mode 0)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; set tab-line face attribute
(if (display-graphic-p)
    (progn
      (global-tab-line-mode)
      (set-face-attribute 'tab-line nil :height 1.1))
  (global-tab-line-mode -1))

;; scroll up screen
(setq scroll-conservatively 101
      scroll-margin 2)

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

(provide 'init-gui-frames)
