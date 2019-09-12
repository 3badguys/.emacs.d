;;; init-company.el --- Config for company mode

(add-hook 'after-init-hook 'global-company-mode)

;; Use C-n C-p to select
(after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(provide 'init-company)
