;;; init-search.el --- Config for search tools

;; smex, a M-x enhancement for Emacs
(when (fboundp 'smex)
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; which-key
(require 'which-key)
(which-key-mode 1)

;; recentf, open recently opened file
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-menu-items 25)

;; everytime bookmark is changed, automatically save it
(setq bookmark-save-flag 1)

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

;; goto-line-preview
(require 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)

;; command-log-mode
(require 'command-log-mode)
(global-command-log-mode)
(global-set-key (kbd "C-c h l") 'clm/open-command-log-buffer)

(provide 'init-search)
