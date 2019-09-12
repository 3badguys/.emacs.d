;;; init-search.el --- Config for search tools

;; ivy
(add-hook 'after-init-hook 'ivy-mode)
(after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t))

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
(global-set-key (kbd "C-c p s") 'counsel-ag)
(global-set-key (kbd "M-s i") 'counsel-semantic-or-imenu)

;; ace-jump
(autoload
  'ace-jump-mode
  "ace-jump-mode" t)
(after-load 'ace-jump-mode
  '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(provide 'init-search)
