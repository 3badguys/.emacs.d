;;; init-utils.el --- Elisp helpers functions and commands

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist` to use `MODE` for all given file `PATTERNS`."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Handier way to add modes to interpreter-mode-alist
(defun add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist` to use `MODE` for all given file `PATTERNS`."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

(provide 'init-utils)
