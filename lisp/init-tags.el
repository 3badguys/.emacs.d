;;; init-tags.el --- Config for tags

(setq tags-revert-without-query t)
(setq tags-case-fold-search nil)
(setq large-file-warning-threshold nil)

(require 'tbg-jump)
;; In windows system, use the ctags program in ~/.emacs.d/bin/ctags/ctags.exe
(when (string-equal system-type "windows-nt")
  (setq tbg-jump-ctags-program-path
        (file-truename (concat user-emacs-directory
                               "bin/ctags/ctags.exe"))))

(provide 'init-tags)
