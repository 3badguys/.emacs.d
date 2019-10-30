;;; init-tags.el --- Config for tags

(setq tags-revert-without-query t)
(setq tags-case-fold-search nil)
(setq large-file-warning-threshold nil)

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

;; Search for TAGS file in the directory tree
;; Copy from https://pages.sachachua.com/.emacs.d/Sacha.html
(defun my/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my/recursive-find-file file (expand-file-name ".." directory)))))

(defun my/find-tags ()
  "Set the TAGS files."
  (interactive)
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-table-list)
       (my/recursive-find-file "TAGS")))

;; TODO: How to find tags automatically
;; (after-load 'prog-mode
;;   '(progn
;;      (add-hook 'prog-mode-hook 'my/find-tags)))

;; ggtags
;; (add-hook 'c-mode-common-hook
;; 	      (lambda ()
;; 	        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

(provide 'init-tags)
