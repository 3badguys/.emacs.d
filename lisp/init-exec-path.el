;;; init-exec-path.el --- Set up exec-path to help Emacs find programs

(require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; TODO: This is ugly!
;; Add the golang exec path
(setq gopaths '("/usr/local/go/bin"
		        ;;		(expand-file-name "bin" (substitute-in-file-name "$HOME/gocode/"))
		        "/home/chuic456/gocode/bin"
		        ))
(setenv "PATH" (concat (getenv "PATH")
		               (mapconcat 'identity gopaths ":")
		               ))
(setq exec-path (append gopaths exec-path))

(provide 'init-exec-path)
