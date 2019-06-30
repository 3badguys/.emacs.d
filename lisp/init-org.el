;;; init-org.el --- Org-mode config

(require 'org)

(setq org-agenda-files '("~/org"))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-startup-indented t)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setq org-src-fontify-natively t)

(setq org-ditaa-jar-path "/usr/bin/ditaa")

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((C . t)
     (org . t)
     (emacs-lisp . t)
     (latex . t)
     (python . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (ditaa . t)
     (plantuml . t))))

(provide 'init-org)
