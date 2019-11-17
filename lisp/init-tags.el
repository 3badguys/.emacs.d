;;; init-tags.el --- Config for tags

(setq tags-revert-without-query t)
(setq tags-case-fold-search nil)
(setq large-file-warning-threshold nil)

;; Create TAGS file through ctags async.
(defun 3badguys-create-tags-async (dir-name)
  "Create tags file async."
  (interactive "DTAG-Root: ")
  (start-process-shell-command
   ""
   nil
   (format "ctags -f %s -e -R %s"
           (expand-file-name "TAGS" (directory-file-name dir-name))
           (directory-file-name dir-name)))
  (message "created tags async through start-process-shell-command."))

;; Create TAGS file through ctags sync.
;; In order to debug when creating tags file failed.
(defun 3badguys-create-tags-sync (dir-name)
  "Create tags file sync."
  (interactive "DTAG-Root: ")
  (shell-command
   (format "ctags -f %s -e -R %s"
           (expand-file-name "TAGS" (directory-file-name dir-name))
           (directory-file-name dir-name)))
  (message "created tags sync through shell-command."))

;; Define keybindings for create-tags-funcs.
(global-set-key (kbd "C-c c a") '3badguys-create-tags-async)
(global-set-key (kbd "C-c c s") '3badguys-create-tags-sync)

;; How to use ctags in Emacs effectively
;; Copy from http://blog.binchen.org/posts/how-to-use-ctags-in-emacs-effectively-3.html
(defun 3badguys-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))

(defun 3badguys-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
        file)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s." dir)
      (shell-command
       (format "ctags -f %s -e -R %s" file dir))
      )
    file
    ))

(defvar 3badguys-tags-updated-time nil)

(defun 3badguys-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (3badguys-create-tags-if-needed (file-name-directory tag) t)
    ))

(defun 3badguys-auto-update-tags-when-save ()
  (interactive)
  (cond
   ((not 3badguys-tags-updated-time)
    (setq 3badguys-tags-updated-time (current-time)))
   ((< (- (float-time (current-time)) (float-time 3badguys-tags-updated-time)) 180)
    ;; < 180 seconds
    (message "no need to updated the tags")
    )
   (t
    (setq 3badguys-tags-updated-time (current-time))
    (3badguys-update-tags)
    (message "updated tags after %d seconds." (- (float-time (current-time))  (float-time 3badguys-tags-updated-time)))
    )
   ))

(defun 3badguys-setup-develop-environment ()
  (when (3badguys-project-name-contains-substring "/home/chuic456")
    (cond
     ((3badguys-project-name-contains-substring "code_proj")
      (setq tags-table-list (list
                             (3badguys-create-tags-if-needed "~/code_proj/TrainingGround")
                             (3badguys-create-tags-if-needed "~/code_proj/remote_toy")))))))

(add-hook 'after-save-hook '3badguys-auto-update-tags-when-save)
(add-hook 'c-mode-hook '3badguys-setup-develop-environment)
(add-hook 'c++-mode-hook '3badguys-setup-develop-environment)
(add-hook 'python-mode-hook '3badguys-setup-develop-environment)
(add-hook 'cperl-mode-hook '3badguys-setup-develop-environment)
(add-hook 'haskell-mode-hook '3badguys-setup-develop-environment)

(defun 3badguys-load-ctags-conf (CONF_FILE)
  (interactive)
  "Load .ctags to CONF_FILE."
  (when (not (file-exists-p CONF_FILE))
    (shell-command (format "mkdir -p %s; ln -s %s %s"
                           (file-name-directory CONF_FILE)
                           (expand-file-name (file-name-nondirectory CONF_FILE) user-emacs-directory)
                           CONF_FILE))
    (message "load .ctags success.")))

(add-hook 'after-init-hook
          (lambda ()
            (3badguys-load-ctags-conf "~/.ctags.d/.ctags")))

(provide 'init-tags)
