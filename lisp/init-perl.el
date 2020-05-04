;;; init-perl.org --- Config for perl

;; Use cperl-mode instead of the default perl-mode
(add-auto-mode 'cperl-mode "\\.\\([pP][Llm]\\|al\\)\\'")
(add-interpreter-mode 'cperl-mode "perl" "perl5" "miniperl")

;; Change default indentations
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (cperl-set-style "PerlStyle"))

(provide 'init-perl)
