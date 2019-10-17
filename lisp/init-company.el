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

;; TabNine
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
;; Copy from manateelazycat, thanks.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))

;; Trigger completion immediately
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(provide 'init-company)
