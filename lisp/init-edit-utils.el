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

;; spell checking
(require 'ispell)
;; Spell check through aspell
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (define-key flyspell-mode-map (kbd "C-,") nil)
    (define-key flyspell-mode-map (kbd "C-.") nil)))

;; delete-selection-mode
(add-hook 'after-init-hook 'delete-selection-mode)

;; auto revert
(global-auto-revert-mode)

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
