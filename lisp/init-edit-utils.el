;;; init-edit-utils.el --- Day-to-day editing helpers

;; indent the text content
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

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; handle eol
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

;; hungry-delete mode
(global-hungry-delete-mode)

;; config for tab key
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; show line number
(global-linum-mode t)

;; highlight the current line
(global-hl-line-mode t)

;; symbol-overlay mode for highlighting the symbol
(require 'symbol-overlay)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-put)
(global-set-key (kbd "<f9>") 'symbol-overlay-remove-all)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)

;; no backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; delete the selection
(delete-selection-mode t)

;; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-edit-utils)
