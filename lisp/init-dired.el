;;; init-dired.el --- Dired customisations

(require 'dired-x)

;; always allow dired to delete or copy dir
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; let the other dir in the split pane to be default destination
(setq dired-dwim-target t)

;; open the file/directory without creating a new buffer
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

(provide 'init-dired)
