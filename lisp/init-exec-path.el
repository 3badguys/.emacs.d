;;; init-exec-path.el --- Set up exec-path to help Emacs find programs

(require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
