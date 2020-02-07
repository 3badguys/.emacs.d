;;; init-company.el --- Config for company mode

(add-hook 'after-init-hook 'global-company-mode)

;; Use C-n C-p to select
(after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; Anaconda
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; Trigger completion immediately
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(provide 'init-company)
