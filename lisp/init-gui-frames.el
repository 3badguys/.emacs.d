;;; init-gui-frames.el --- Behaviour specific to non-TTY frames

;; suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)

;; initial window and default window
(setq default-frame-alist
      (if (display-graphic-p)
          (cond
           ((string-equal (upcase (system-name)) "CHUIC456-DEBIAN10")
            '(
              (tool-bar-lines . 0)
              (background-color . "honeydew")
              (width . 100)
              (height . 60)))
           ((string-equal (upcase (system-name)) "CCHHIT-HP")
            '(
              (tool-bar-lines . 0)
              (background-color . "honeydew")
              (width . 110)
              (height . 63)))
           (t
            '(
              (tool-bar-lines . 0)
              (background-color . "honeydew")
              (width . 95)
              (height . 45))))
        ;; Use tool-bar-lines faster than setting `(tool-bar-mode -1)`
        '((tool-bar-lines . 0))))

;; set default font
(set-frame-font
 (cond
  ((string-equal system-type "gnu/linux")
   (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-8" nil))
  ((string-equal system-type "windows-nt")
   (if (member "Consolas" (font-family-list)) "Consolas-9" nil))
  (t nil))
 t t)

(defun tbg-cycle-frame-size ()
  "Cycle frame size among certain sizes.
Version 2020-10-26"
  (interactive)
  (let (
        ($widthHeightPairs [[75 45] [55 45] [100 60] [55 60]])
        $pairIdx $width $height)

    (when (not (get this-command 'pairIdx))
      (put this-command 'pairIdx 0))
    (setq $pairIdx (get this-command 'pairIdx))

    (setq $width (elt (elt $widthHeightPairs $pairIdx) 0))
    (setq $height (elt (elt $widthHeightPairs $pairIdx) 1))
    (set-frame-size (selected-frame) $width $height)
    (put this-command 'pairIdx
         (mod (1+ $pairIdx) (length $widthHeightPairs)))))

;; some base preferences
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; show cursor position within line
(column-number-mode 1)

;; disable cursor blink
(blink-cursor-mode 0)

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
