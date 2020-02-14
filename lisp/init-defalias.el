;;; init-defalias.el --- Use alias to shortend command

(defalias 'yes-or-no-p 'y-or-n-p) ;; y or n is enough

(defalias 'rs 'replace-string)

;; make frequently used commands short
(defalias 'dc 'desktop-clear)

(defalias 'lcd 'list-colors-display)

(when (fboundp 'magit-status)
  (defalias 'ms 'magit-status))

(provide 'init-defalias)
