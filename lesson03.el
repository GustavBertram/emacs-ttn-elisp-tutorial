;;; lesson03.el
;;;
;;; Copyright (C) 1999, 2004, 2005, 2009 Thien-Thi Nguyen
;;; This file is part of ttn's Emacs Lisp tutorial, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; If you are asking yourself "what is this file?", you should work
;;; through lesson01.el and lesson02.el in this directory, then come
;;; back here.

;;; Variables and SETQ
;;; ==================

;; In previous lessons we looked at evaluation, which (1) is recursive;
;; and (2) results in a return value and possibly the execution of side
;; effects.  We saw that functions are basically expressions that have
;; been named so that they can be evaluated at a later time.  This
;; convenient ability to store expressions is analogously available for
;; values.  The container is a "variable", named so because over its
;; lifetime, you can place different values into it.

;; In Emacs Lisp, symbols can be used to name variables.  As you know,
;; symbols can also be used as function names.  In fact, each Lisp
;; symbol can have two independent meanings: a meaning as a function,
;; and a meaning as a variable.  In Lisp, the context always makes it
;; clear whether you're using the symbol as a function or as a variable,
;; so Emacs never gets confused about the two meanings.

;; When we say "the variable BLAH" we really mean "BLAH's meaning
;; as a variable".

;; Introducing SETQ
;; ----------------
;; To set a symbol's meaning as a variable to some value, use `setq'.
;; This is a special form that (1) does not evaluate its first argument,
;; normally a symbol; (2) does evaluate its second argument; (3) sets
;; the variable-meaning of the symbol to the value of the second
;; argument.

;; >> Eval this sexp.
(setq spiffy-variable 42)

;; >> Eval this symbol.
spiffy-variable

;; You should see 42 as the result of evaluation for both cases.  In the
;; first one, however, the side effect was to set `spiffy-variable' to
;; the value 42.  Let's see what happens when we have a sexp for the
;; second argument.

;; >> Eval this sexp.
(setq spiffy-variable (* 2 4 7 8))

;; You should see 448 as the value of the expression.  If you do `C-h v
;; spiffy-variable RET' now you will also see 448.  Evaluating a symbol
;; that has not been set in some manner results in an error.

;; >> Eval this symbol.
unset-variable

;; You should get a "Symbol's value as variable is void" error.  The
;; `setq' special form is able to take multiple pairs of arguments.
;; Each pair is processed before doing the next, so that it is ok to
;; chain dependencies.  The last value is the return value for the
;; entire `setq' form.

;; >> Eval this sexp.
(setq
 new-spiffy-variable
 spiffy-variable

 days-in-year
 (- new-spiffy-variable (- (* 42 2) 1))

 my-friend
 (format "%s %d days/year" (upcase "emacs") days-in-year))

;; You should see a friendly string inserted into the buffer.  Note that
;; `days-in-year' depends on the newly defined `new-spiffy-variable' on
;; the preceding line.  Also note that for both `days-in-year' and
;; `my-friend', the functions `-', `upcase' and `format' are called as
;; part of the evaluation of the expression.

;; Variable Names
;; --------------
;; This is all easy to say, but you should check for yourself that
;; `setq' did the right thing:

;; >> For each symbol, do two things:
;; >> (1) Place the cursor somewhere on the symbol (or immediately
;; >>     following it and type `C-h v RET'.  Use `C-x 1' to return to
;; >>     one window.
;; >> (2) Place the cursor immediately following the symbol and eval it
;; >>     by typing `C-j' as usual.

new-spiffy-variable
days-in-year
my-friend

;; You should see the same info being presented in two different ways.
;; The key sequence `C-h v' runs the command `describe-variable', which
;; is analogous to `describe-function' which we saw in lesson02.el.  If
;; a symbol has meaning as a variable, the default will show that symbol;
;; otherwise, there will be no default.

;; >> Place the cursor somewhere on the following symbol and invoke
;; >> `describe-variable'.  Type `C-g' to cancel.

unset-variable

;; You should not see a default.  Also similar to `describe-function',
;; `describe-variable' shows a completion list for ambiguous input.  You
;; can type in the first few letters of the variable name and then `TAB'
;; to show the list of possible completions.

;; Variable Documentation
;; ----------------------
;; It is then, no surprise, that `describe-variable' also displays
;; documentation on a particular variable (or a message saying no
;; documentation is available).  We saw how to include a docstring in
;; a function definition, but what about variables?  The answer is the
;; `defvar' form, which takes a name, a value and a string.  `defvar'
;; can only be used for one variable at a time, and is different from
;; `setq' in that it returns the variable name instead of its value.

;; >> Eval this sexp.
(defvar minutes-in-year (* days-in-year 24 60)
  "Number of minutes in a year (we think).")

;; >> Type `C-h v minutes-in-year RET'.
;; >> Delete the " (we think)" and re-eval the sexp.
;; >> Type `C-h v minutes-in-year RET'.

;; You should see the docstring included in the display, and it should
;; reflect changes to the form (as long as you re-eval).

;; Note how we can use `days-in-year' in the definition of
;; `minutes-in-year'.  In a similar way, we could write a function that
;; uses these global variables.  Variables used in this way are called
;; "free variables" since they are defined outside of the function and
;; are not captured by the function's "closure".  (More on this later.)

;; Although there is a school of programming that derides global
;; variables (and their usage in this way), Emacs Lisp programs typically
;; use them quite a bit for configuration (i.e., customization) and for
;; storing data tables.  In these cases, documentation by `defvar' is
;; very important.

;; >> Look at the docstrings for these variables (move the cursor
;; >> somewhere on each line and type `C-h v').
major-mode
fill-column
system-name
system-type
emacs-version
tab-width

;; >> Eval this sexp using `C-j'.
(let ((load-path (cons "." load-path))
      (tutorial-pov 'basic))
  (load "sanity-check" t t)
  (sanity-check 'lesson03-global-vars-ok))

;; Make sure you feel comfortable with the answer before continuing.

;;; Localizing Variables with LET
;;; =============================

;; Often (perhaps even most of the time), the coordination cost of using
;; a global variable outweighs its benefits.  Global variables do not
;; help you encapsulate state, and thus give rise to subtle bugs where
;; some other piece of code you never even saw before trashes your state
;; unmercifully.  Yuk!

;; So the solution is to localize the variable by introducing a "scope"
;; using the `let' special form.  Inside of this scope, the variable
;; initialization is completely in your control.  As an added bonus,
;; once the control flow completes evaluation of the `let' form, the
;; variable is no longer even visible; it goes "out of scope".  Thus,
;; its so-called "lifetime" is defined completely by the `let' form.
;; Here is a template:
;;
;;   (let ((VAR1 VAL1)
;;         (VAR2 VAL2)
;;         ...)
;;     BODY1
;;     BODY2
;;     ...)
;;
;; Here, VAR1 and VAL1 are similar to `(setq VAR1 VAL1)', and similarly
;; VAR2 and VAL2 are similar to `(setq VAR2 VAL2)'.  BODY1 and so on are
;; other expressions that presumably use VAR1 and VAR2.  When the `let'
;; form is evaluated, VAR1 is "bound" to VAL1, VAR2 is bound to VAL2,
;; and the BODY forms are evaluated in sequence.  The return value of
;; the entire `let' form is the return value of the last BODY form.

;; Note that all the parens shown are required: one pair around each
;; binding, one around all the bindings, and of course, one around the
;; entire `let' form.  The indentation is provided automatically by
;; Emacs.  When you are editing programs in Lisp Interaction mode or
;; Emacs Lisp mode, typing `TAB' indents the line automatically.
;; Another convenient command is `indent-sexp' (normally bound to
;; `M-C-q'), which reindents the entire sexp, recursively.

;; Life, the Universe and Everything
;; ---------------------------------
;; Let's work through some examples.

;; >> Eval this sexp.
(let ((x 3)
      (y 7)
      (z 2))
  (* x y z))

;; You should see 42.  The single body form here uses only local
;; variables, but that doesn't have to be the case.  In the following,
;; `x', `y' and `z' are completely ignored.

;; >> Eval this sexp.
(let ((x 3) (y 7) (z 2))
  (message "%s" my-friend))

;; So far we've used numbers in the VAL1, VAL2, etc. locations.  We can
;; actually use any expression there (similar to `setq'), even another
;; `let' form.

;; >> Eval this sexp.
(let ((x 3)
      (y (let ((xx 1)			; "VAL2" is a `let' form
	       (yy 2)
	       (zz 3))
	   (yo! (format "xx=%d yy=%d zz=%d" xx yy zz) 'funky-flash)
	   (+ xx xx yy zz)))
      (z 2))
  (* x y z))

;; In the internal `let', there are two BODY forms, the first of which
;; we use purely for its side-effects.  Note also that `xx' is used
;; twice by the call to `+'.  This is fine.  But wait!  Why set `zz' to
;; 3 when we've just, prior to that, set `x' to 3 -- why not just use
;; `x' instead?  It seems like a good way to tighten up the code, right?

;; >> Eval this sexp.
(let ((x 3)
      (y (let ((xx 1)			; "VAL2" is a `let' form
	       (yy 2))
	   (yo! (format "xx=%d yy=%d x=%d" xx yy x) 'funky-flash)
	   ;;          v------ now use `x' ------^
	   (+ xx xx yy x)))
      (z 2))
  (* x y z))

;; You should see a "Symbol's value as variable is void" error.  Which
;; symbol?  Why is this?

;; The answer is that the binding of the VARs to the VALs happens in
;; parallel, implying that no VAL form can refer to any of its sibling
;; VARs.  Since the body forms must be able to refer to these new VARs,
;; we can get around this restriction by nesting another `let' form
;; inside the first one (the inner `let' is the body of the outer
;; `let').

;; >> Eval this sexp.
(let ((x 3)
      (z 2))
  (let ((y (let ((xx 1)
		 (yy 2))
	     (yo! (format "xx=%d yy=%d x=%d" xx yy x) 'funky-flash)
	     ;;          v------ now use `x' ------^
	     (+ xx xx yy x))))
    (* x y z)))


;; Serial Let
;; ----------
;; Because this nesting of `let' forms is quite common, the related
;; `let*' form was introduced to act basically the same as `let', but
;; without the reference restriction.  Some people call this "serial
;; let" because bindings happen serially, with each binding immediately
;; available for use by the next one.

;; >> Eval this sexp.
(let* ((x 3)
       (y (let ((xx 1)			; "VAL2" is a `let' form
		(yy 2))
	    (yo! (format "xx=%d yy=%d x=%d" xx yy x) 'funky-flash)
	    ;;          v------ now use `x' ------^
	    (+ xx xx yy x)))
       (z 2))
  (* x y z))

;; So why is parallel `let' the default and not serial?  That's a good
;; question better asked of wiser folks than the author -- still, it's
;; interesting to note the similarities between a `let' form and a
;; function definition:
;;
;;   (defun life-the-universe-and-everything (x y z)
;;     (* x y z))
;;   (life-the-universe-and-everything 3 7 2)
;;
;; looks and behaves rather like
;;
;;   (let ((x 3) (y 7) (z 2))
;;     (* x y z))
;;
;; The only difference is that the binding for the function is given an
;; associated name (the function name) that can be referenced later.  In
;; both cases, the bindings are done in parallel.

;; Shadowing
;; ---------
;; In the `let' and `let*' examples so far, we've taken care to always
;; use different variable names.  But what happens if you use the same
;; names?  How does Emacs resolve this textual ambiguity?  It turns out
;; that a variable bound with `let' or `let*' "shadows" same-named
;; variables in outer scopes.  This means that reference to a variable
;; resolves from the innermost scope value outwards.

;; >> Eval this sexp.
(let ((a 1))
  (yo! (format "in the outer scope, a=%d" a))
  (let ((a 2))
    (yo! (format "in the inner scope, a=%d" a))
    a))

;; You should see the values of `a' in first the outer scope, then the
;; inner scope.  The sexp returns `a' from the inner scope.  Does this
;; mean that `a' is changed in the outer scope?

;; >> Eval this sexp.
(let ((a 1))
  (yo! (format "in the outer scope, a=%d" a))
  (let ((a 2))
    (yo! (format "in the inner scope, a=%d" a)))
  (yo! (format "back in the outer scope, a=%d" a))
  a)

;; You should see the value of the outer `a' remaining unchanged, even
;; though, as before, the inner scope `a' has a different value.

;;; Lesson 03 Wrap Up
;;; =================

;; With what we know at this point, let's see if we can advance our mini
;; project a bit (recall that we are trying to edit SGF files).  So far
;; we have the following:

(defun edit-sgf-file (filename edits)
  "Read in FILENAME (SGF[4] format), apply EDITS and write it out."
  (interactive "fFile (SGF[4] format): ")
  (sgf-write-file (sgf-edit (sgf-read-file filename) edits)))

;; Looks nice, but in actuality this simple code is already full of bugs!
;; The "obvious" bug -- that functions `sgf-write-file', `sgf-edit' and
;; `sgf-read-file' are not yet defined -- we won't bother to comment on.
;; More interesting are:
;;
;; (1) the function `edit-sgf-file' "takes" two "arguments" but the
;;     `interactive' "special form" only queries for one; and
;;
;; (2) the function's name does not begin with "sgf-" like the others.
;;
;; The many quoted words -- indicating concepts to be defined -- in (1)
;; tell us that we have overreached our understanding a bit.  A standard
;; fix for this kind of bug is to put the idea on hold for a bit, simplify
;; the code, and come back to it when we have more understanding.  So now:

;; >> Eval this sexp.
(defun edit-sgf-file (filename edits)
  "Read in FILENAME (SGF[4] format), apply EDITS and write it out."
  ;; TODO: (interactive "fFile (SGF[4] format): ")
  (sgf-write-file (sgf-edit (sgf-read-file filename) edits)))

;; Better, of course, would be to avoid bug (1) in the first place, but
;; then you would have missed a chance to see Authority Botch Another One.
;;
;; With this new definition, a call to the function produces the
;; expected error about `sgf-write-file' not being defined.

;; >> Eval this sexp.
(edit-sgf-file "/tmp/random.sgf" nil)

;; Bug (2) is actually a bit of planning in play: all functions and
;; variables in Emacs lisp share a namespace, so to avoid clobbering
;; things (perhaps in the future) we need to adopt conventions early on.
;; Since "sgf-" is used for the other functions it makes sense to adhere
;; to it even for the top-level.  Thus, we arrive at:

;; >> Eval this sexp.
(defun sgf-edit-file (filename edits)
  "Read in FILENAME (SGF[4] format), apply EDITS and write it out."
  ;; TODO: (interactive "fFile (SGF[4] format): ")
  (sgf-write-file (sgf-edit (sgf-read-file filename) edits)))

;; Consistent use of the "sgf-" prefix makes it easy to find related
;; items in the sea of other functions and variables available, as well.

;; >> Eval these sexps.
(defvar sgf-version "0.0.0" "Version of sgf.el.")
(apropos "^sgf-")

;; OK, bugfixes done.  Now we can continue with the things we've touched
;; upon in this lesson -- variables and `let' -- and deepen the design a
;; bit (but only the very smallest of bits) in the process.

(defun sgf-edit-file (filename edits)
  "Read in FILENAME (SGF[4] format), apply EDITS and write it out.
If EDITS is nil, skip the write and instead return ~~~SOMETHING~~~."
  ;; TODO: (interactive "fFile (SGF[4] format): ")
  (let ((~~~something~~~ (sgf-read-file filename)))
    (if edits
        (sgf-write-file (sgf-edit ~~~something~~~ edits))
      ~~~something~~~)))

;; Here we use `if' -- to be explained fully in lesson04.el -- to select
;; the behavior of `sgf-edit-file' depending on the value of `edits'.
;; We update the docstring but remind ourselves that the design is still
;; incomplete with the tildes (they look kind of wavy, or "unsure") and
;; the nondescript word "something" as part of the variable name.


;;; Local variables:
;;; mode: lisp-interaction
;;; End:

;;; lesson03.el ends here
