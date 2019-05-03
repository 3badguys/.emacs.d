;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Add the golang exec path
(setq gopaths '("/usr/local/go/bin"
		        ;;		(expand-file-name "bin" (substitute-in-file-name "$HOME/gocode/"))
		        "/home/chuic456/gocode/bin"
		        ))
(setenv "PATH" (concat (getenv "PATH")
		               (mapconcat 'identity gopaths ":")
		               ))
(setq exec-path (append gopaths exec-path))

(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-keybindings)
(require 'init-org)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

(load-file custom-file)

