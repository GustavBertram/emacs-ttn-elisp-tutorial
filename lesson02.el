;;; lesson02.el
;;;
;;; Copyright (C) 1999, 2004, 2005, 2009 Thien-Thi Nguyen
;;; This file is part of ttn's Emacs Lisp tutorial, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; If you are asking yourself "what is this file?", you should look at
;;; lesson01.el in this directory, then come back here.

;;; A Project?
;;; ==========
;;; Probably the best way to learn is to practice, and it's usually more
;;; fun to practice on a project than on small exercises (although the
;;; exercises certainly help, too).  So, this Emacs Lisp tutorial will
;;; adopt a project, but which one?
;;;
;;; Most groovy things that are done with Emacs Lisp are just that: done.
;;; This includes everything from web browsing (with graphics in buffer) to
;;; games and diversions (try `M-0 M-x life').  People have written Emacs
;;; Lisp packages for processing mouse "strokes", for implemeting IRC
;;; frontends (and bots), and for having buffers be speech-synthesized.
;;; And so on.  Basically, there has been so much hacking on Emacs Lisp
;;; that one could argue that there's not much left to hack.  All of this
;;; is in addition to the usual kinds of customizations that occur in one's
;;; ~/.emacs file.
;;;
;;; Because we don't want to re-invent the wheel, we may be faced with
;;; the prospect of having nothing to work on.  But still, there's quite
;;; a lot of room for play.
;;;
;;; A particularly interesting but still (2005-01-26) nascent area of
;;; Emacs Lisp hacking is playing the game of Go, itself a very involving
;;; topic even outside of Emacs.  To date there is Emacs Lisp to play a
;;; game against the GNU Go computer program inside an Emacs buffer:
;;;
;;;  http://www.gnu.org/software/gnugo/
;;;  http://www.gnuvola.org/software/j/gnugo/
;;;
;;; However, still lacking is a way to read in a "game tree" stored in
;;; SGF[4] format.  The full spec of Smart Game Format Revision 4 is at:
;;;
;;;  http://www.red-bean.com/sgf/
;;;
;;; We will tackle this problem in this tutorial, namely the ability to
;;; edit (read in, change in a syntactically correct way, and write out
;;; to a file) a game tree or two.  With some planning and a bit of luck,
;;; the result can be used elsewhere.


;;; Sanity check
;;; ============

;; Obviously you're somewhat sane for choosing to muck with this tutorial,
;; but how can you be sure?

;; >> Evaluate this sexp.
(format "default-directory is %s" default-directory)

;; So you know where you are, and that is the first step of sanity.

;; >> Evaluate this sexp.
(file-exists-p (expand-file-name "sanity-check.el" default-directory))

;; What do you see?  If "t" you are certainly on the right track.  If
;; you see "nil", it's probably a good idea to go download the full
;; tutorial and then come back here when ready.

;; >> Evaluate this sexp.
(let ((load-path (cons "." load-path))
      (tutorial-pov 'basic))
  (load "sanity-check" t t))

;; Note that in the process of evaluating this complicated sexp, Emacs
;; also managed to present judgement about you.  Do you like that?  If I
;; were you I'd be quite querulous of this pronouncement (if you missed
;; it, just redo the evaluation).  The result of the evaulation should
;; be `t', but in this case that is not as interesting as what occurs as
;; a "side effect" of the evaluation.  We will go into this in detail in
;; the following sections.

;;; Doing Chores
;;; ============

;; Functions like `+' and `format' are useful for their "return value",
;; but other functions are useful for their "side effects".  And of
;; course, there are some that are useful for both.  It's like going to
;; the store for milk, but also chatting with the neighbor along the way;
;; one could either get milk (return value), chat with the neighbor (side
;; effect), or do both, in some sequence.

;; Let's define some functions so that we can play with them.

;; >> Evaluate this defun.
(defun buy-milk ()
  "Buy milk."
  (fancy-message "Now buying milk.")
  "milk")

;; >> And this one.
(defun chat-with-neighbor ()
  "Chat with neighbor (could be considered a chore at times)."
  (fancy-message "Now chatting with neighbor."))

;; >> And finally this one.
(defun do-weekly-chores (dawdle-time)
  "Do weekly chores, dawdling DAWDLE-TIME seconds after each chore."
  (interactive "nDawdle-time: ")
  (buy-milk)
  (dawdle dawdle-time)
  (chat-with-neighbor)
  (dawdle dawdle-time)
  (message "OK, done."))

;; >> Eval this sexp.
(sanity-check 'lesson02-chore-defuns-ok)

;; If everything is ok at this point you should have several functions
;; defined.  The last one can be invoked via `M-x'.  The exact meaning
;; of all this is explained below.  But first, you should feel free to
;; experiment.  If you change the above definitions, don't forget to
;; evaluate the sexp using `C-j' to inform Emacs of the change.

;; >> Eval this sexp.
(do-weekly-chores 3)

;; >> Type `M-x do-weekly-chores RET'.  When asked the dawdle-time, try
;; >> `3' first (for three seconds).  Then try other values.  If you
;; >> asked Emacs to dawdle too long and want to interrupt, type `C-g'.
;; >>
;; >> HINT: After the first invocation, you can use `M-x M-p RET' to
;; >> save yourself some typing.

;; OK, so what was going on there (besides dawdling)?  In lesson01.el,
;; the sexps you were evaluating all had the form: (FUNC ARG1 ARG2...)
;; with the result being the application of FUNC to its args.  So, one
;; has to wonder, what is this "defun" func?  Why does it return its
;; first arg?  Well as it turns out, there are some forms (aka sexps)
;; that are special -- these are appropriately termed "special forms",
;; and their behavior on evaluation must be discussed case-by-case.

;;; Building Functions with DEFUN
;;; =============================

;; For now let's look at `defun', which is one of the most important
;; special forms in Emacs Lisp.  It is the one that defines functions and
;; thus serves as a way to extend Emacs' capabilities (which are just a
;; bunch of functions defined by the Emacs maintainers).

;; A function has basically four parts: a name; a list of arguments; a
;; documentation string; and everything else, commonly called the "body"
;; of the function.  The general form for a defun looks like this:
;;
;;	(defun NAME (ARG1 ARG2...)
;;        DOCSTRING
;;        BODY-FORM1
;;        BODY-FORM2...)
;;
;; Each of these parts is described in turn below.

;; Function Names
;; --------------

;; The name is a "symbol" useful when you want to call the function.  In
;; the above example, we introduced three functions named `buy-milk',
;; `chat-with-neighbor' and `do-weekly-chores', respectively.  The
;; characters that make up a symbol are usually letters and numbers plus
;; the hyphen "-".  It is rare to see lisp symbols with underscore "_",
;; but that's not disallowed.  Other characters such as the asterisk
;; or slash can also be used.  But stay away from parentheses and:
;;   , (comma)
;;   ' (single quote)
;;   " (double quote)
;;   # (hash or pound or octothorpe or ..., depending on your mood)
;; as these have other syntactic roles in Emacs Lisp.

;; When choosing a function name, it's a good idea to make sure you're
;; not clobbering a function that already exists; `describe-function',
;; normally bound to `C-h f', comes in handy for this check.

;; >> Type `C-h f' and then `TAB'.
;; >> Type `TAB' repeatedly to scroll.
;; >> Type `C-g' to cancel.

;; You should see the names of thousands of functions that are part of
;; Emacs, including the three new ones just added.  If at the "Describe
;; function:" prompt you start typing a few keys, Emacs will narrow the
;; list down to those functions whose names begin with those letters the
;; next time you type `TAB'.  This is called "completion".

;; >> Type `C-h f'.
;; >> Type `b', then `u' and then `TAB' several times.
;; >> Type `C-g' to cancel.

;; Emacs can also recognize function names from the current buffer (when
;; in lisp-interaction mode, such as we are now).  This is an even
;; faster way of seeing if a symbol is already taken as a function name.

;; >> Move the cursor anywhere on each of the following lines and
;; >> type `C-h f'.  Type `C-g' to cancel.
buy-milk
chat-with-neighbor
do-weekly-chores
describe-function
+
random-unlikely-to-be-a-function-symbol

;; You should see Emacs supply a "default" for the first five and no
;; default for the last one.  From this you can tell that the first five
;; are defined functions and that the last is not.

;; Function Arguments
;; ------------------

;; If you consider the standard picture of programs:
;;
;;		--INPUT--> [PROGRAM] --OUTPUT-->
;;
;; then, the analogous picture for functions is:
;;
;;		<--RETURN-VALUE-- [FUNCTION] <--ARGUMENTS--
;;
;; The direction of the arrows is reversed to emphasize the usage in
;; Emacs Lisp (and other languages).  Arguments are symbols that only come
;; into play within the function.

;; In the above example, the arg `dawdle-time' is the single argument in
;; the list `(dawdle-time)' to the function `do-weekly-chores'.  We've
;; seen that it's used to change the function's side effects, but does
;; it exist outside the function?

;; >> Eval this symbol.
dawdle-time

;; You should get a "value as variable is void" error.  Officially, we
;; say that the symbol `dawdle-time' is "bound" to a value only within
;; the scope of the function.  By the `C-h f' test we also see that it's
;; not a function either.

;; Note that the functions `buy-milk' and `chat-with-neighbor' do not
;; have any arguments, and so their arglists are empty: `()'.

;; >> Eval this function definition.
(defun simple-+ (a b)
  "Return the addition of A and B."
  (+ a b))

;; >> Eval these sexps.
(fancy-message "%d" (simple-+ 3 5))
(fancy-message "%d" (simple-+ 35 7))

;; You should see the arithmetic results displayed in a fancy way.
;; First Emacs evaluates `simple-+', passing to it the args `3' and `5'.
;; During evaluation, `a' is bound to `3' and `b' is bound to `5'.  The
;; result of this evaluation, the number `8', is then passed, along with
;; the format string "%d", to `fancy-message'.  We are interested in the
;; side effects of `fancy-message' rather than its return value, but for
;; the record, it is `t'.

;; Function Documentation
;; ----------------------

;; An old myth tells us about some early-day hero having the foresight
;; to tie a string to the entrance of a labyrinth so that he could find
;; his way out again (after having vanquished the monster within).  In a
;; similar way, you can tie a string to the beginning of your function
;; so that you can find your way out again (after having vanquished the
;; algorithms within).

;; These strings, delimited by double-quotes, are called "docstrings"
;; because supposedly they document the function.  Like the old hero,
;; you need to figure out how heavy to make the docstring: too light,
;; and it may break, too heavy and you may tire prematurely.

;; >> Type `C-h f RET' on the following lines to browse the docstrings.
fancy-message
do-weekly-chores
simple-+
describe-function
lisp-interaction-mode
interactive

;; Function Body
;; -------------

;; The function's body is a "sequence" of forms that are evaluated.  The
;; last form's return value is then returned from the function as its
;; return value.  The sequence is the other major relationship between
;; forms (the first being recursion, which we covered in lesson01.el).
;; In a `defun' special form, the sequence is "inlined" so that you
;; don't have to specify explicitly.  We will look at different cases
;; later.

;; >> Eval this function definition.
(defun do-weekly-chores-backwards-quickly (dawdle-time)
  "Do weekly chores really quickly, not taking into account DAWDLE-TIME."
  (interactive "nDawdle-time: ")
  (chat-with-neighbor)
  (buy-milk))

;; >> Eval this sexp.
(do-weekly-chores-backwards-quickly 1e+9)

;; >> Type `M-x do-weekly-chores-backwards-quickly RET'.  Enter a large
;; >> number when prompted for "Dawdle time:".

;; You should see `chat-with-neighbor' called before `buy-milk' without
;; any dawdling.  This is because that's exactly what we defined the body
;; of this new function to do.
;;
;; Note also that `do-weekly-chores-backwards-quickly' returns the string
;; "milk" because that is what `buy-milk' returns.  And that is because
;; the last form in the function `buy-milk' is a simple string "milk".

;;; Lesson 02 Wrap Up
;;; =================

;; OK, now that we know a few things about functions, how is this going
;; to help us with our SGF file editing project?

;; Even if we don't know all the details, at this point we can begin
;; thinking of the problem in a top-down fashion, and write some code
;; that helps organize our thoughts.  Here is the seed of our SGF file
;; editing idea, as a function.

(defun edit-sgf-file (filename edits)
  "Read in FILENAME (SGF[4] format), apply EDITS and write it out."
  (interactive "fFile (SGF[4] format): ")
  (sgf-write-file (sgf-edit (sgf-read-file filename) edits)))

;; In the next Emacs Lisp lesson we will look at variables, including the
;; special form `let'.  For now, take a peek at the SGF[4] spec and try
;; and keep playing with functions.  Use `C-h f' often enough and you
;; might not even need to complete this tutorial!

;; By the way, you can save function definitions in files with the ".el"
;; extension.  See tutorial-magic.el in this directory for an example.
;; Also, there is a more convenient way to load definitions into Emacs
;; than to visit the file and eval each defun: use `load-file'.

;; >> Bonus for the dedicated reader: Eval the following sexp.
(tutorial-bonus 'lesson02-end)


;;; Local variables:
;;; mode: lisp-interaction
;;; End:

;;; lesson02.el ends here
