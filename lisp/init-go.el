;;; init-go.el --- Config for golang

;; add go-mode to auto-mode-alist
(add-auto-mode 'go-mode "\\.go\\'")

;; run gofmt before save the file
(add-hook 'before-save-hook 'gofmt-before-save)

;; config go-flymake
(add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
(require 'go-flymake)

;; auto complete for go
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(provide 'init-go)
