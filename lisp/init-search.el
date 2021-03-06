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

;; http://ergoemacs.org/emacs/emacs_icomplete_mode.html
(progn
  ;; minibuffer enhanced completion
  (require 'icomplete)
  (icomplete-mode 1)
  ;; show choices vertically
  (setq icomplete-separator "\n")
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions))

;; http://ergoemacs.org/emacs/emacs_ido_mode.html
(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (require 'ido)
  (ido-mode 1)

  ;; show choices vertically
  (if (version< emacs-version "25")
      (progn
        (make-local-variable 'ido-separator)
        (setq ido-separator "\n"))
    (progn
      (make-local-variable 'ido-decorations)
      (setf (nth 2 ido-decorations) "\n")))

  ;; show any name that has the chars you typed
  (setq ido-enable-flex-matching t)
  ;; use current pane for newly opened file
  (setq ido-default-file-method 'selected-window)
  ;; use current pane for newly switched buffer
  (setq ido-default-buffer-method 'selected-window)
  ;; stop ido from suggesting when naming new file
  (when (boundp 'ido-minor-mode-map-entry)
    (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)))

;; which-key
(require 'which-key)
(which-key-mode 1)

;; recentf, open recently opened file
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-menu-items 50)

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

(defun tbg-start-command-log ()
  "Start the `command-log-mode' globally and
make a new buffer as log buffer. Change from
 `xah-start-command-log'.
Version 2020-10-26"
  (interactive)
  (setq $current-fr (selected-frame))
  (setq $fr-height (frame-parameter $current-fr 'height))
  (setq $new-fr (make-frame))
  (select-frame-set-input-focus $new-fr)
  (set-frame-size $new-fr 55 $fr-height)
  (command-log-mode)
  (global-command-log-mode)
  (clm/open-command-log-buffer)
  (delete-window)
  (text-scale-set 0)
  (select-frame-set-input-focus $current-fr))

(provide 'init-search)
