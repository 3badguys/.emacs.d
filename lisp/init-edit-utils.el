;;; init-edit-utils.el --- Day-to-day editing helpers

;; some base preferences
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))
;; (global-hl-line-mode t)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 make-backup-files nil
 auto-save-default nil
 delete-selection-mode t)

;; auto revert
(global-auto-revert-mode)

;; set mark
(global-set-key (kbd "C-<return>") 'set-mark-command)

;; hungry-delete mode
(global-hungry-delete-mode)

;; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; indent the text content
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
	    (progn
	      (indent-region (region-beginning) (region-end))
	      (message "Indent selected region."))
      (progn
	    (indent-buffer)
	    (message "Indent buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; handle eol
(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; comment like Eclipse
(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "C-c C-c") 'comment-eclipse)

;; multiple-cursors mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; undo-tree mode
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-.") 'undo-tree-redo)
(global-set-key (kbd "C-,") 'undo-tree-undo)

;; symbol-overlay mode for highlighting the symbol
(require 'symbol-overlay)
(after-load 'symbol-overlay
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-put)
  (global-set-key (kbd "<f9>") 'symbol-overlay-remove-all)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward))

;; Copy from: https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(provide 'init-edit-utils)
