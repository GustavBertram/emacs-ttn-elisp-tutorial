;;; lesson01.el
;;;
;;; Copyright (C) 1999, 2004, 2009 Thien-Thi Nguyen
;;; This file is part of ttn's Emacs Lisp tutorial, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Audience
;;; ========
;;; This tutorial presumes you have some familiarity with Emacs as a user.
;;; In other words, instructions like "Type `C-x i'" make sense to you.
;;; If this is not the case, ask a local Emacs-head for help and get back
;;; here later.

;;; Methods
;;; =======
;;; The tutorial is presented as comments and code in Emacs Lisp (.el) files.
;;; Some browsers may know what to do with these, others may not.  In any
;;; case, the best way to proceed is to download the file (or save to disk
;;; if you are viewing this already), consulting the first line for the
;;; filename.  For example, you should save this file in some directory
;;; (doesn't matter where) as `lesson01.el', omitting the quotes, of course.
;;;
;;; The way to interact with the tutorial is through Emacs.  This ain't
;;; no web-based approach (unless you are already using Emacs for web
;;; browsing).  Instead, we encourage hands-on tinkering.  You will
;;; notice that at the bottom of the file are local variables directing
;;; Emacs to put the buffer into Lisp-interaction mode.  If this doesn't
;;; work for you, type
;;;
;;;	M-x lisp-interaction-mode RET
;;;
;;; to do it manually.  In this mode, `C-j' does `eval-print-last-sexp',
;;; which modifies the current buffer by inserting the value of the last
;;; structured expression evaluated.  Do not be alarmed!  This is natural
;;; and part of the fun.
;;;
;;; We might as well get started...


;;; Evaluation
;;; ==========

;; These comments (starting with ";" and going to end of line) don't
;; have any "value", but other things usually do.

;; >> Place the cursor after the "3" in the next line and type `C-j'.
3

;; You should see another 3 appear.  This is because "3" has the value of
;; 3, in other words, the representation of 3 "evaluates" to 3.  This is
;; pretty obvious.  Try replacing the 3 above with 42 and see what happens.
;; Remember to place the cursor *AFTER* the number.  What happens when you
;; type `C-j' with the cursor in front of the number?

;; >> Place the cursor at the end of the next line and then type `C-j'
;; (henceforth, we will just say "evaluate the next sexp"):
"Emacs Lisp rules"

;; You should see the string (delimited by double-quotes) inserted again.
;; This is because strings are another type of expression that evaluates
;; to itself.  In general, such expressions are called literals.

;; >> Now type `M-x describe-key RET C-j'.

;; This should show a description of the command that `C-j' runs, namely
;; `eval-print-last-sexp'.  In some versions of Emacs, the message:
;;
;;	You can run the command `describe-key' with C-h k
;;
;; may appear in the echo area at the bottom of the screen after a few
;; seconds.  This means that you could have used `C-h k C-j' to do the
;; same thing.  (Try it.)  You can use `C-x 1' to remove the help window.

;; A "sexp" is a structured expression, which can be very simple as
;; we've seen so far.  Let's try something more complex.

;; >> Evaluate this sexp.
(+ 3 42)

;; You should see the number 45 appear.  So, `(+ 3 42)' has the value
;; 45.  Note that parentheses delimit this expression and there are both
;; numbers and symbols in between.  Unlike other languages, parentheses
;; in Emacs Lisp have significant meaning -- you cannot arbitrarily omit
;; or include them.

;; >> Evaluate this sexp.
+ 3 42

;; You should see 42, because in actuality, there are three sexps on
;; that line, the symbol `+', and two numbers `3' and `42'.  When you
;; type `C-j', the last sexp is evaluated, rather than the last line.
;; (So yeah, the instruction "evaluate this sexp" is misleading -- that
;; ought to teach you to always mistrust authority!)

;; >> Evaluate this sexp.
(+
 3
 42)

;; What you will see depends on where you place the cursor.  If placed
;; after the right-parens, the expected 45 shows up.  What happens when
;; you place the cursor elsewhere?

;; >> Evaluate this sexp.
((+ 3 42))

;; This should not print anything (except maybe a newline), and you
;; should see the error in the minibuffer "Invalid function: (+ 3 42)".
;; Try it again if you missed it the first time.

;; The reason for the error is because the way that Emacs evaluates
;; sexps is to either return a value (as we've seen with `3' and
;; `"Emacs Lisp rules"'), or to look for the first item after the left
;; parenthesis and consider it a function to apply to the remaining
;; items before the closing right parenthesis.  In this case, the first
;; item was actually another sexp, namely `(+ 3 42)', and because Emacs
;; doesn't know of a function with that name it threw the error you saw.
;; How about other functions?

;; >> Evaluate these sexps.
(- 3 42)
(* 3 42)
(/ 3 42)
(% 3 42)

;; You should see integer (non-fractional) results.  This is because the
;; arguments `3' and `42' to the respective functions are integers.

;; >> Evaluate these sexps.
(- 3.0 42.0)
(* 3.0 42.0)
(/ 3.0 42.0)
(% 3.0 42.0)

;; All but the last one should result in floating-point numbers.  The
;; last one results in a "Wrong type argument" error because the modulo
;; function `%' expects integer arguments only.  Functions are picky
;; like that.

;; >> Evaluate this sexp.
(+ "Emacs Lisp rules" " forever")

;; Same deal, i.e., you should see a "Wrong type argument" error because
;; the function `+' expects integer arguments only.  Try replacing the
;; "+" with "concat" and see what happens.  We will cover `concat' and
;; friends starting in a lesson or two.  OK, now to get more complex.
;; Rather than using 42, we could use another sexp that is itself a
;; function application.

;; >> Evaluate this sexp.
(+ 3 (* 21 2))

;; When faced with complexity, Emacs evaluates the innermost sexp and
;; moves outward, substituting partial results until the top-level sexp
;; is evaluated.  Another thing to note is that some functions (such as
;; some of the arithmetic ones) can take more than one argument.  And
;; some can take only one (or even zero) argument(s) and still do the
;; right thing.

;; >> Evaluate this sexp.
(+ 1 1 1 (* (+ 1 1 1) (+ 1 1 1 (* 2 2)) (* 2)))

;; >> Evaluate these sexps.
(*)
(* 3)
(+)
(+ 42)

;; Tips for editing sexps: When you type a right paren, Emacs normally
;; moves the cursor to the matching left paren.  This is useful to see
;; what you're closing off.

;; >> Evaluate this sexp to make sure the feature is enabled.
(setq blink-matching-paren t)

;; >> Type ")" to complete the following sexp.  Watch the cursor.
(blah blah blah

;; >> Type the appropriate number of right parens to complete the
;; >> following sexp.  How many are required?  Try this at different
;; >> speeds to see how Emacs behaves.  For the adventurous, copy the
;; >> entire line several times (say 5) and repeat the experiment.
(blah (blah (blah blah) blah) (blah blah (blah blah

;; But what if you don't want to add new text (or if you can't, i.e.,
;; a read-only buffer)?  In that case, the following key bindings (in
;; lisp-interaction-mode or emacs-lisp-mode) can be very useful:
;;
;;	M-C-f	forward-sexp		M-C-u	backward-up-list
;;	M-C-b	backward-sexp		M-C-d	down-list
;;	M-C-n	forward-list		M-C-SPC	mark-sexp
;;	M-C-p	backward-list
;;
;; You can use these to jump around sexps and lists with ultimate
;; confidence because Emacs is doing the job of keeping track of things
;; for you.

;; >> Put the cursor on the second `+' in the following sexp.
;; >> Type the command sequence `M-C-f M-C-u M-C-n M-C-f M-C-n'.
;; >> (You can hold down Meta and Control and type `f u n f n'.)
;;
;; this one--+
;;           |
;;           v
(+ 1 1 1 (* (+ 1 1 1) (+ 1 1 1 (* 2 2)) (* 2)))

;; What column do you end up on?  (Use `C-x =' to find out.)

;; Evaluation can also be done in the minibuffer using `M-:', which runs
;; the command `eval-expression'.  If you have this command disabled,
;; the first time you run it you may be asked if you want to enable it,
;; and if so, permanently.  Probably a good idea if you want to become a
;; serious Emacs Lisp hacker.

;; >> Put the cursor at the *BEGINNING* of the sexp in the above example.
;; >> Type the command sequence `M-C-SPC M-w M-: C-y RET'.
;; >> What do you see?

;;; Lesson 01 Wrap Up
;;; =================

;; Well, that's it for the first lesson.  You should be able to use
;; Emacs as simple calculator now.  For example, a standard 40 hr/wk
;; minimum wage (USD) with no vacation would be:
;;
;;	(* 52 40 5.25)
;;
;; If this is your situation, I urge you to take up the next Emacs Lisp
;; lesson, where we learn more about functions.  Even if this is not
;; your situation, do drop in.


;;; Local variables:
;;; mode: lisp-interaction
;;; End:

;;; lesson01.el ends here
