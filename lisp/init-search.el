;;; init-search.el --- Config for search tools

;; ivy
(add-hook 'after-init-hook 'ivy-mode)
(after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t))

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

(defun xah-toggle-search-whitespace ()
  "Set `search-whitespace-regexp' to nil or includes hyphen lowline tab newline.
Explanation: When in isearch (M-x `isearch-forward'), space key can also stand for other chars such as hyphen lowline tab newline. It depend on a regex. It's convenient. But sometimes you want literal. This command makes it easy to toggle.
Emacs Isearch Space Toggle
http://ergoemacs.org/emacs/emacs_isearch_space.html
Version 2019-02-22"
  (interactive)
  (if (string-equal search-whitespace-regexp nil)
      (progn
        (setq search-whitespace-regexp "[-_ \t\n]+")
        (message "Space set to hyphen lowline tab newline space"))
    (progn
      (setq search-whitespace-regexp nil)
      (message "Space set to literal."))))

;; swiper
(global-set-key "\C-s" 'swiper)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(setq ivy-initial-inputs-alist nil)

;; counsel
(add-hook 'after-init-hook 'counsel-mode)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key "\C-x\ \C-r" 'counsel-recentf)
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c p f") 'counsel-git)
(global-set-key (kbd "C-c p s") 'counsel-rg)
(global-set-key (kbd "M-s i") 'counsel-semantic-or-imenu)

;; ace-jump
(autoload
  'ace-jump-mode
  "ace-jump-mode" t)
(after-load 'ace-jump-mode
  '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; goto-line-preview
(require 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)

;; command-log-mode
(require 'command-log-mode)
(global-command-log-mode)
(global-set-key (kbd "C-c h l") 'clm/open-command-log-buffer)

(provide 'init-search)
