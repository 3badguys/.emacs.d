;;; init-chinese.el --- Configurations for Chinese.

(require 'pyim)
(require 'pyim-basedict)

(pyim-basedict-enable)
(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)
(setq pyim-default-scheme 'quanpin)

;; When mode line displays PYIM-AU and can't input chinese, that is annoying.
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))

(pyim-isearch-mode 1)

(setq pyim-page-tooltip 'minibuffer)

(setq pyim-page-length 5)

(setq pyim-enable-shortcode nil)

(provide 'init-chinese)
