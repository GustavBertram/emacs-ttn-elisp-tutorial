;;; tutorial-magic.el
;;;
;;; Copyright (C) 1999, 2004, 2005, 2009 Thien-Thi Nguyen
;;; This file is part of ttn's Emacs Lisp tutorial, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(or (get 'tutorial-magic 'tutorial-home-dir)
    (error "Do not load this file manually, infidel!"))

(defmacro with-tutorial-load-path (&rest body)
  `(let ((load-path (cons (get 'tutorial-magic 'tutorial-home-dir)
			  load-path)))
     (progn
       ,@body)))

(defun sanity-check (sym &optional)
  (with-tutorial-load-path
   (let ((tutorial-pov sym))
     (load "sanity-check" t t))))

(defun tutorial-functionp (&rest syms)
  (catch 'sorry
    (mapcar '(lambda (sym)
	       (unless (fboundp sym)
		 (throw 'sorry nil)))
	    syms)))

(defun fancy-message (&rest args)
  "Display formatted ARGS on the mode-line."
  (with-tutorial-load-path (require 'yo))
  (yo! (apply 'format args) 'funky)
  t)

(defun dawdle (dawdle-time)
  "Delay DAWDLE-TIME seconds, updating mode-line each second."
  (cond ((< dawdle-time 0)
	 (message "done dawdling!"))
	((< dawdle-time 1)
	 (sit-for dawdle-time))
	(t
	 (while (<= 1 dawdle-time)
	   (fancy-message "dawdling %s %d"
			  (make-string (% (truncate dawdle-time) 5) ?.)
			  dawdle-time)
	   (setq dawdle-time (- dawdle-time 1.0)))
	 (dawdle dawdle-time))))

(defun tutorial-bonus (sel)
  (cond ((eq sel 'lesson02-end)
	 (or (featurep 'cl)
	     (error "Hey, you didn't really go through lesson02!"))
	 (require 'tetris)
	 (describe-function 'tetris)
	 (delete-window)
	 (if (y-or-n-p "Play tetris? ")
	     (tetris)
	   (kill-buffer (current-buffer))
	   (message "OK, maybe next time...")))))

(provide 'tutorial-magic)

;;; tutorial-magic.el ends here
