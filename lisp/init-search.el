;;; init-search.el --- Config for search tools

;; ivy
(add-hook 'after-init-hook 'ivy-mode)
(after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t))

(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; counsel
(add-hook 'after-init-hook 'counsel-mode)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c p f") 'counsel-git)

;; swiper
(global-set-key "\C-s" 'swiper)

;; helm
(add-hook 'after-init-hook 'helm-mode)
(global-set-key (kbd "M-s i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key "\C-x\ \C-r" 'helm-recentf)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(require 'helm-ag)
(global-set-key (kbd "C-c p s") 'helm-do-ag-project-root)

;; ace-jump
(autoload
  'ace-jump-mode
  "ace-jump-mode" t)
(after-load 'ace-jump-mode
  '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(provide 'init-search)
