;;; init-site-lisp.el --- Config for site-lisp

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Add site-lisp's subdirs to load-path
(add-subdirs-to-load-path (expand-file-name "site-lisp" user-emacs-directory))

(provide 'init-site-lisp)
