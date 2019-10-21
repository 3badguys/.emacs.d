;;; init-prog.el --- Initialize programming configurations.

(setq-default prettify-symbols-alist
              '(("lambda" . ?λ)
                ("<-" . ?←)
                ("->" . ?→)
                ("->>" . ?↠)
                ("=>" . ?⇒)
                ("map" . ?↦)
                ("/=" . ?≠)
                ("!=" . ?≠)
                ("==" . ?≡)
                ("<=" . ?≤)
                (">=" . ?≥)
                ("&&" . ?∧)
                ("||" . ?∨)
                ("not" . ?¬)))

(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Not open prettify-symbols-mode by default, otherwise `M-x prettify-symbols-mode`
;; (add-hook 'prog-mode-hook 'prettify-symbols-mode)

(provide 'init-prog)
