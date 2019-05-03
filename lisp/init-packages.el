(require 'cl)

(when (>= emacs-major-version 24)
  (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			               ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; Add Packages
(defvar chuic456_emacs/packages '(
				                  company
				                  ;; monokai-theme
				                  ;; solarized-theme
				                  hungry-delete
				                  swiper
				                  counsel
				                  smartparens
				                  popwin
				                  expand-region
				                  iedit
				                  geiser
				                  paredit
				                  windresize
				                  window-numbering
				                  which-key
				                  helm-ag
				                  ggtags
				                  ace-jump-mode
				                  symbol-overlay
				                  cnfonts
				                  session
				                  ;; ecb
				                  w3m
				                  htmlize
				                  auto-complete
				                  go-mode
				                  go-autocomplete
				                  ) "Default packages")

(setq package-selected-packages chuic456_emacs/packages)

(defun chuic456_emacs/packages-installed-p ()
  (loop for pkg in chuic456_emacs/packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (chuic456_emacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg chuic456_emacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(global-hungry-delete-mode)

;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; config auto-mode-alist
(setq auto-mode-alist
      (append
       '(("\\.go\\'" . go-mode)
	     )
       auto-mode-alist))

(global-company-mode t)

;; (global-set-key (kbd "M-s i") 'counsel-imenu)
(global-set-key (kbd "M-s i") 'helm-semantic-or-imenu)

;; (load-theme 'monokai t)
;; (load-theme 'solarized-dark t)

(require 'popwin)
(popwin-mode t)

(global-set-key (kbd "C-=") 'er/expand-region)

;; Default is C-; for iedit-mode
(global-set-key (kbd "M-s e") 'iedit-mode)

(setq geiser-active-implementations '(chez))

(require 'w3m)
(setq w3m-home-page "http://www.google.com.hk")
                                        ;(require 'mime-w3m)
(setq w3m-default-display-inline-images t)
(setq w3m-default-toggle-inline-images t)
(setq w3m-show-graphic-icons-in-header-line t)
(setq w3m-show-graphic-icons-in-mode-line t)
(setq w3m-use-cookies t)
(setq w3m-command-arguments '("-cookie" "-F"))

(add-hook 'before-save-hook 'gofmt-before-save)

(add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
(require 'go-flymake)

;; auto complete for go
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; config for tab key
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default c-basic-offset 4)
(setq c-default-style "linux")

(provide 'init-packages)
