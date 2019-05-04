;;; init-hippie-expand.el --- Settings for hippie-expand

(global-set-key (kbd "s-/") 'hippie-expand)

(setq hippie-expand-try-function-list
      '(try-expand-debbrev
		try-expand-debbrev-all-buffers
		try-expand-debbrev-from-kill
		try-complete-file-name-partially
		try-complete-file-name
		try-expand-all-abbrevs
		try-expand-list
		try-expand-line
		try-complete-lisp-symbol-partially
		try-complete-lisp-symbol))

(provide 'init-hippie-expand)
