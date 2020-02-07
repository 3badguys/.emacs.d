;;; init-dired.el --- Dired customisations

(require 'dired-x)

;; always allow dired to delete or copy dir
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; let the other dir in the split pane to be default destination
(setq dired-dwim-target t)

;; http://ergoemacs.org/emacs/emacs_dired_tips.html
;; open the file/directory without creating a new buffer
(put 'dired-find-alternate-file 'disabled nil)
(after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

;; TODO: what's the target of dwin?
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

(provide 'init-dired)
