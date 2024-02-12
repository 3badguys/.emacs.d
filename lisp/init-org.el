;;; init-org.el --- Org-mode config

(use-package org
  :ensure nil
  :bind (("C-c a"  . org-agenda))
  :config
  (setq
   ;; when opening a org file, don't collapse headings
   org-startup-folded nil
   ;; indent the org file
   org-startup-indented t
   ;; wrap long lines. don't let it disappear to the right
   org-startup-truncated nil
   ;; when in a url link, enter key should open it
   org-return-follows-link t
   ;; make org-mode” syntax color embedded source code
   org-src-fontify-natively t
   )

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((C . t)
     (emacs-lisp . t)
     (python . t)
     (perl . t)
     (shell . t)
     (js . t)
     (lisp . t)
     (latex . t))))

(provide 'init-org)
