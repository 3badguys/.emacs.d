;;; init-eshell.el --- Config for eshell

;; Eshell Alias
;; https://www.emacswiki.org/emacs/EshellAlias
;; Examples through alias command(save to ~/.emacs.d/eshell/alias) in eshell:
;; 1. ~ $ alias ll 'ls -l $*'
;; 2. ~ $ alias alias emacs 'find-file $1'

;; permanent aliases through Eshell Functions
;; https://www.emacswiki.org/emacs/EshellFunctions
(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
    \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))
(defalias 'eshell/vim 'eshell/vi)

(provide 'init-eshell)
