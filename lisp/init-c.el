;;; init-c.el --- Config for c

;; ggtags
(add-hook 'c-mode-common-hook
	      (lambda ()
	        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(setq-default c-basic-offset 4)
(setq c-default-style "linux")

(provide 'init-c)
