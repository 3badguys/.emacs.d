;;; tbg-cloc.el --- ▮ -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2027, by 3badguys

;; Author: 3badguys
;; Version: ▮
;; Created: ▮
;; Package-Requires: ▮
;; Keywords: ▮
;; License: GPL v3

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


;;;###autoload
(defun tbg-cloc-file (@input_file)
  "Cloc for specific file."
  (interactive)
  (with-temp-buffer
    (insert-file-contents @input_file)
    ;; remove empty lines
    (beginning-of-buffer)
    (while (re-search-forward "\n\n+" nil "NOERROR")
      (replace-match "\n"))
    (message "Lines of %s: %d" @input_file (count-lines (point-min) (point-max)))))
;;; Test case:
;; (tbg-cloc-file "~/.emacs.d/init.el")

;;;###autoload
(defun tbg-cloc-directory (@input_dir)
  "Cloc for specific directory."
  (interactive)
  (mapc 'tbg-cloc-file
        (directory-files-recursively @input_dir "\.el$" nil)))
;;; Test case:
;; (tbg-cloc-directory "~/.emacs.d/")

(provide 'tbg-cloc)

;;; tbg-cloc.el ends here
