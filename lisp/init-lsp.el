;;; init-lsp.el--- Config for lsp mode

(require 'lsp-mode)
(require 'company-lsp)
(require 'eglot)

(push 'company-lsp company-backends)

(provide 'init-lsp)
