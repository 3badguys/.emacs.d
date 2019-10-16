;;; init-lsp.el--- Config for lsp mode

(require 'lsp-mode)
(require 'lsp-ui)
(require 'company-lsp)
(require 'eglot)

(push 'company-lsp company-backends)

(setq lsp-prefer-flymake nil)
;; (setq lsp-auto-guess-root t)
(setq lsp-enable-snippet nil)
(setq lsp-enable-eldoc nil)
(setq lsp-message-project-root-warning t)
(setq create-lockfiles nil)

(add-hook 'python-mode-hook #'lsp)

(provide 'init-lsp)
