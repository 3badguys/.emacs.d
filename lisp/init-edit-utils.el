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
 backup-by-copying t
 create-lockfiles nil
 auto-save-default nil
)

;; delete-selection-mode
(add-hook 'after-init-hook 'delete-selection-mode)

;; auto revert
(global-auto-revert-mode)

;; hungry-delete mode
(global-hungry-delete-mode)

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

;; Move to beginning of line
;; Copied from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun 3badguys-smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                '3badguys-smarter-move-beginning-of-line)

(provide 'init-edit-utils)
