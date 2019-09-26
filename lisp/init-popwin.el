;;; init-popwin.el --- Config for popwin mode

(require 'popwin)
(popwin-mode t)

;; xref
(push "*xref*" popwin:special-display-config)

(provide 'init-popwin)
