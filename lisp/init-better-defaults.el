(global-linum-mode t)

(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					                        ;; signature
					                        ("SHIT" "T_ACT3_T")
					                        ))

(setq make-backup-files nil)

(setq auto-save-default nil)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defadvice show-paren-function (around fix-show-paren-function activate)
  (cond ((looking-at-p "\\s(") ad-do-it)
	    (t (save-excursion
	         (ignore-errors (backward-up-list))
	         ad-do-it))))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(delete-selection-mode t)

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
	    (progn
	      (indent-region (region-beginning) (region-end))
	      (message "Indent selected region."))
      (progn
	    (indent-buffer)
	    (message "Indent buffer.")))))

(setq hippie-expand-try-function-list '(try-expand-debbrev
					                    try-expand-debbrev-all-buffers
					                    try-expand-debbrev-from-kill
					                    try-complete-file-name-partially
					                    try-complete-file-name
					                    try-expand-all-abbrevs
					                    try-expand-list
					                    try-expand-line
					                    try-complete-lisp-symbol-partially
					                    try-complete-lisp-symbol))

(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

(setq dired-dwim-target t)

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; dwin = do what i mean.
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	        (buffer-substring-no-properties
	         (region-beginning)
	         (region-end))
	      (let ((sym (thing-at-point 'symbol)))
	        (when (stringp sym)
	          (regexp-quote sym))))
	    regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

(require 'window-numbering)
(window-numbering-mode 1)

(require 'which-key)
(which-key-mode 1)

(add-hook 'c-mode-common-hook
	      (lambda ()
	        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(require 'cmuscheme)
(setq scheme-program-name "scheme")

(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
	      (lambda ()
	        (paredit-mode 1)
	        (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
	        (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

(require 'symbol-overlay)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-put)
(global-set-key (kbd "<f9>") 'symbol-overlay-remove-all)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)

(require 'cnfonts)
(cnfonts-enable)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'desktop)
(desktop-save-mode t)

;; (require 'cedet)
(add-to-list 'load-path "~/.emacs.d/git_repo/ecb")
(when (require 'ecb nil 'noerror)
  (setq ecb-tip-of-the-day nil)
  (setq ecb-auto-compatibility-check nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1))

(provide 'init-better-defaults)
