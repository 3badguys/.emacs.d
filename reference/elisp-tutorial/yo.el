;;;;; This file snarfed from ttn's personal elisp library,
;;;;; with RCS tags frozen.  -ttn, 1999/05/02 04:20:26

;;; ID: yo.el,v 1.11 1998/10/08 08:57:26 ttn Rel
;;;
;;; Copyright (C) 1997, 1998 Thien-Thi Nguyen
;;; This file is part of ttn's personal elisp library, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Description: Alternative notification.

(require 'cl)

;;;###autoload
(defun starfield-string (len)
  (let* ((r (random))
	 (s (make-string 16 ?*))
	 (hmm (do ((i 15 (1- i)))
		  ((< i 0) s)
		(aset s
		      (- 15 i)
		      (if (= 0 (logand r (lsh 1 i))) ?* ?-)))))
    (substring (concat hmm hmm hmm hmm hmm) 0 (if (< len 0) 0 len))))

;;;###autoload
(defun yo! (msg &optional style)
  "Reworks mode line to display MSG in high STYLE.
Styles are: simple-message, message, funky, funky-flash, bounce, t."
  (let ((standard-message (concat ">>> " msg " <<<")))
    (case style
      (simple-message
       (message msg)
       (sit-for 1))
      (message
       (message standard-message)
       (sit-for 1))
      (funky
       (let ((mode-line-format (concat "YO! " standard-message)))
	 (force-mode-line-update)
	 (sit-for 1))
       (force-mode-line-update))
      (funky-flash
       (let ((i 10))
	 (while (and (> i 0))
	   (yo! msg 'funky)
	   (sit-for 0.5)
	   (decf i))))
      (bounce
       (let* ((len-msg (length msg))
	      (fr-width (frame-width))
	      (m (/ (- len-msg fr-width) 2)) ; magnitude
	      (o (abs m))		; offset
	      done)
	 (do ((x 0.0 (+ x (* 0.01 pi))))
	     (done nil)
	   (let* ((ns (truncate (+ o (* m (sin x))))) ; num spaces
		  (mode-line-format
		   (concat
		    (starfield-string (1- ns))
		    " "
		    msg
		    " "
		    (starfield-string (- fr-width len-msg (1+ ns))))))
	     (setq done (not (sit-for 0.02)))
	     (force-mode-line-update)))))
      ;; Add styles (don't forget doscstring) here.
      (t
       (let ((mode-line-format standard-message))
	 (force-mode-line-update)
	 (sit-for 1))
       (force-mode-line-update)))))

(provide 'yo)

;;; yo.el,v1.11 ends here
