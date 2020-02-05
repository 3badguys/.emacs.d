;;; init-abbrev.el --- Config for abbrev mode

;; Note:
;;      1. Press Ctrl+q before typing space or punctuation, can stop abbrev from expansion.
;;      2. M+x list-abbrevs → Display a list of defined abbrevs.

(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ("3bg" "3badguys")

    ;; english word abbrev
    ("u" "you")
    ("r" "are")
    ("ur" "you are")
    ("bg" "background")
    ("thx" "thanks")
    ("cnt" "can't")
    ("dnt" "don't")
    ("wnt" "won't")

    ;; computing
    ("cs" "computer science")

    ;; programming
    ("db" "databases")

    ;;
    ))

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
;; won't save to the file named abbrev_defs
(setq save-abbrevs nil)

(provide 'init-abbrev)
