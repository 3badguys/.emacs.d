;;; init-yaml.el --- Support Yaml files

(require 'yaml-mode)
(add-auto-mode 'yaml-mode "\\.yml\\'")

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide 'init-yaml)
