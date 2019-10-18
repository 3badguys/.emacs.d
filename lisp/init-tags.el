;;; init-tags.el --- Config for tags

;; Create TAGS file through ctags asyn.
(defun create-tags-async (dir-name)
  "Create tags file asyn."
  (interactive "DTAG-Root: ")
  (start-process-shell-command
   ""
   nil
   (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name))))

;; Create TAGS file through ctags sync.
;; In order to debug when creating tags file failed.
(defun create-tags-sync (dir-name)
  "Create tags file sync."
  (interactive "DTAG-Root: ")
  (shell-command
   (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name))))

;; Define keybindings for create-tags-funcs.
(global-set-key (kbd "C-c c a") 'create-tags-async)
(global-set-key (kbd "C-c c s") 'create-tags-sync)

;; ggtags
;; (add-hook 'c-mode-common-hook
;; 	      (lambda ()
;; 	        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

(provide 'init-tags)
