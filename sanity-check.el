;;; sanity-check.el
;;;
;;; Copyright (C) 1999, 2004, 2009 Thien-Thi Nguyen
;;; This file is part of ttn's Emacs Lisp tutorial, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; When loaded, this file considers `tutorial-pov' and judges if the
;; environment is sane with respect to that.  To make things personal,
;; it calls the environment "you".

;;; Code:

(cond

 ((or (not (boundp 'tutorial-pov))
	   (not tutorial-pov))
  ;; Improper usage should be treated with humor.  Unfortunately, humor
  ;; is a complex subject not easily codified.  Often one just has to be
  ;; obtuse instead and hope there is some overlap.
  (message "You may or may not be insane, what is your `tutorial-pov'?.")
  (ding) (sit-for 1) (ding)
  (sit-for 2))

 ((eq tutorial-pov 'basic)
  ;; We try to be affirmative here, but have to stick to standards you know.
  (message (if (and (file-exists-p "./sanity-check.el")
		    (file-exists-p "./lesson01.el")
		    (put 'tutorial-magic			; sneaky
			 'tutorial-home-dir default-directory))
	       "Congratulations, you are basically sane."
	     "Hmmm, you seem to be a bit insane.  Where are you again?"))
  (when (get 'tutorial-magic 'tutorial-home-dir)
    (let ((load-path (cons (get 'tutorial-magic 'tutorial-home-dir)
			   load-path)))
      (require 'tutorial-magic))))

 ((eq tutorial-pov 'lesson02-chore-defuns-ok)
  (message
   (if (tutorial-functionp 'buy-milk 'chat-with-neighbor 'do-weekly-chores)
       "You are still sane."
     "Hmmm, did you remember to evaluate the above defuns?")))

 ((eq tutorial-pov 'lesson03-global-vars-ok)
  (message
   (if (and (boundp 'spiffy-variable)     (= 448 spiffy-variable)
	    (boundp 'new-spiffy-variable) (= 448 new-spiffy-variable)
	    (boundp 'days-in-year)        (= 365 days-in-year)
	    (boundp 'my-friend)
	    (boundp 'minutes-in-year)     (= 525600 minutes-in-year)
	    (get 'minutes-in-year 'variable-documentation))
       "You are still sane, good job."
     "Hmmm, did you remember to evaluate the above sexps?")))

 ;; Add new points of view here.
 )

;;; sanity-check.el ends here
