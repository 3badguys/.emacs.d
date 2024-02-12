;;; init-search.el --- Config for search tools

(progn
  (setq enable-recursive-minibuffers t)

  ;; Save minibuffer history
  (savehist-mode 1)

  ;; big minibuffer height, for ido to show choices vertically
  (setq max-mini-window-height 0.5)

  ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
  ;; minibuffer, stop cursor going into prompt
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt))))

(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (require 'ido)
  (ido-mode 1))

;; recentf, open recently opened file
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-menu-items 50)

;; ripgrep
(when (executable-find "rg")
  (use-package rg :ensure t))

;; vertical completion UI
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

;; set orderless completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

;; marginalia in the minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; embark
(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; consulting completing-read
(use-package consult :ensure t)
(use-package embark-consult :ensure t)

;; enable indentation+completion using the TAB key
(setq tab-always-indent 'complete)

;; auto complete through corfu or company mode
(if (display-graphic-p)
    (use-package corfu
      :ensure t
      :custom
      (corfu-auto t)
      (corfu-cycle t)
      ;; (corfu-separator ?\s)
      (corfu-quit-at-boundary nil)
      (corfu-quit-no-match t)
      (corfu-preview-current nil)
      (corfu-preselect 'prompt)
      (corfu-auto-delay 0.1)
      (corfu-auto-prefix 2)
      (corfu-on-exact-match nil)
      (corfu-scroll-margin 1)
      :hook (after-init . global-corfu-mode))
  (use-package company
    :ensure t
    :hook (after-init . global-company-mode)
    :config
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2)))

;; everytime bookmark is changed, automatically save it
(setq bookmark-save-flag 1)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

;; display the current command through keycast
(use-package keycast
  :ensure t
  :config
  (keycast-header-line-mode))

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

(provide 'init-search)
