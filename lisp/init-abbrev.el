;;; init-abbrev.el --- Config for abbrev mode

(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					                        ;; signature
					                        ("SHIT" "T_ACT3_T")
					                        ))

(provide 'init-abbrev)
