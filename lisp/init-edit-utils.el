;;; init-edit-utils.el --- Day-to-day editing helpers

;; set default file encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; some base preferences
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; show cursor position within line
(column-number-mode 1)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 make-backup-files nil
 auto-save-default nil)

;; delete-selection-mode
(add-hook 'after-init-hook 'delete-selection-mode)

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

;; Copy from: https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(progn
  ;; Don't disable case-change functions
  (put 'upcase-region 'disabled nil)   ;; C-x C-u
  (put 'downcase-region 'disabled nil) ;; C-x C-l

  ;; Don't disable narrowing commands
  (put 'narrow-to-region 'disabled nil) ;; C-x n n
  (put 'narrow-to-page 'disabled nil)   ;; C-x n d
  (put 'narrow-to-defun 'disabled nil)  ;; C-x n p

  ;; Erase buffer
  (put 'erase-buffer 'disabled nil)

  ;; Scroll to the left
  (put 'scroll-left 'disabled nil) ;; C-x <
  )

(provide 'init-edit-utils)
