;;; init-org.el --- Org-mode config

(require 'org)

;; org-mode setup
(progn
  ;; when opening a org file, don't collapse headings
  (setq org-startup-folded nil)
  ;; indent the org file
  (setq org-startup-indented t)
  ;; wrap long lines. don't let it disappear to the right
  (setq org-startup-truncated nil)
  ;; when in a url link, enter key should open it
  (setq org-return-follows-link t)
  ;; make org-mode” syntax color embedded source code
  (setq org-src-fontify-natively t))

(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)

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
     (lisp . t)
     (ditaa . t)
     (plantuml . t))))

(provide 'init-org)
