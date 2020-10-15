;;; init-edit-utils.el --- Day-to-day editing helpers

;; set default file encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

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
