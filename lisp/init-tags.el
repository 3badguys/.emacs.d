;;; init-tags.el --- Config for tags

;; Create TAGS file through ctags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DTAG-Root: ")
  (shell-command
   (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name))))

;; ggtags
;; (add-hook 'c-mode-common-hook
;; 	      (lambda ()
;; 	        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

(provide 'init-tags)
