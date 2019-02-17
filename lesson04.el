;;; lesson04.el
;;;
;;; Copyright (C) 2005, 2009 Thien-Thi Nguyen
;;; This file is part of ttn's Emacs Lisp tutorial, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; If you are asking yourself "what is this file?", you should work
;;; through lesson01.el, lesson02.el and lesson03.el in this directory,
;;; then come back here.

;;; Choosing Paths with IF
;;; ======================

;; Like most things in life, the ability to make a decision is
;; important for both its psychological as well as its practical
;; effects.  Historically humans have used this power rather abusively,
;; most of the time to aid and comfort those within some social group
;; while contemporaneously suppressing, oppressing and distressing
;; those outside that group.  Although you may wish your programs to
;; never become like the MCP in the movie Tron, as their creator you
;; still need to embue them with the ability to make decisions; hopefully
;; they can serve their users and improve their lot without resorting to
;; unwarranted coercion and other such nastiness.  Of course, if your
;; goal is actually to write MCP or an MCP-like program, then Emacs Lisp
;; makes a fine implementation language for that endeavor as well...

;; During evaluation, the most basic way to make a decision is with
;; the `if' special form.  But before we get into `if', we need to be
;; comfortable with what is considered "true" and what is considered
;; "not true".  In Emacs Lisp, the latter is easy to answer:

;; >> Eval this symbol.
nil

;; How utterly boring (but predictable) the answer!  The symbol `nil'
;; evaluates to itself (its value is also `nil') and this value is the
;; only value in Emacs Lisp considered "not true".  Every other value,
;; no matter how outlandishly, incomprehensibly, or even misdirectedly
;; expressed, is considered "true".  A politician's paradise, but we
;; digress...

;; >> Eval this symbol.
t

;; You should see the initial characters of the author's given name.
;; (What narcisism!)  Like `nil', the symbol `t' evaluates to itself,
;; but unlike `nil', it is merely one of the many "true" values
;; available for your program to ponder, although many consider it the
;; ultimate Truth.  In any case it is a concise way to express "true"
;; when the particular details of the value are not important.  Emacs
;; Lisp functions that take this attitude with their return value are
;; called "predicates" for some reason.

;; >> Eval this sexp.
(setq some-truths
      (list (not nil) (not t)
            (not (+ 1 2))
            (not "how") (not :very) (not 'kind-of-you)))

;; Here we introduce the function `list', which returns a list composed
;; of its arguments, and the predicate `not', which inverts the truth
;; value of its argument.  It is said: "List not what your Emacs can do
;; for you, list what you can do for your Emacs...".  We use `list' here
;; primarily to save you some time evaluating individual sexps.  Here is
;; the alternative:

;; >> Eval these sexps.
(not nil)
(not t)
(not (+ 1 2))
(not "how")
(not :very)
(not 'kind-of-you)

;; Most lists are not circular and thus can be traversed with a finite
;; amount of effort.  Perhaps we will talk about circular lists later,
;; or perhaps it is enough that you remember reading this sentence to
;; form a model of a circular list in your brain much clearer than what
;; the tutorial writer can ever hope to accomplish.  In any case, we
;; can use `length' to see exactly how finite a non-circular list is.

;; >> Eval this sexp.
(list (length some-truths)
      (length (list some-truths))
      (length (list some-truths some-truths))
      (length (list)))

;; >> Eval this sexp.
(not (list))

;; At first this result seems to fly in the face of the explanations
;; above.  But we can work out a hypothesis by process of deduction:
;; Given that `not' only ever returns `t' for an argument of `nil', and
;; that `list' returns a list of its arguments (even if there are none),
;; that must mean that a list of zero length is "equivalent" to `nil'.
;; Really?

;; >> Eval this sexp.
(list)

;; It turns out our deductive reasoning is sound, in the sense that the
;; empirical data confirms our hypothesis.  Still the question remains:
;; Why?  And why the quotes around "equivalent"?  And why has the author
;; waited five years between writing the previous lesson and this one?
;; Why, why, why?

;; The answer to the first question is easiest: To make it easier to
;; write Emacs Lisp programs that operate on lists.  Think about a to-do
;; list and how beautiful it is to be able to say either:
;;
;;  - "there are no elements in this list"
;;     (it has a length of zero);
;;
;;  - "this whole idea of having to do things is invalid"
;;     (it has nil value).
;;
;; The beauty lies in two places:
;;
;;  - the resulting behavior: you have nothing to do
;;    (ahhh, free time and a cool drink on a hot day...);
;;
;;  - the latitude in attitude required to decide on the resulting
;;    behavior: you can choose to be a persnickety accountant or a
;;    cavalier loaf-about (accountants enjoy free time, too, you know).
;;
;; Similar to (most) Emacs Lisp programmers, Emacs Lisp programs have a
;; mutual understanding when it comes to passing lists around -- processing
;; an empty list is just like doing no processing at all!  Or in other
;; words: counting to zero (from zero) is a waste; let's not even bother.
;; We will explore this in detail a little further on in the lesson.

;; >> Eval these sexps.
(list :no-surprise
      (eq t t)
      (eq nil nil)
      (eq (list) (list))
      (eq t (not nil))
      (eq nil (not t))
      (eq 42 (1+ (1- 42))))

(list :also-no-surprise
      (eq t nil)
      (eq nil t)
      (eq (list) (list nil))
      (eq "ignorance" "bliss"))

(list :surprise!
      (eq (list t) (list t))
      (eq (list nil) (list nil)))

;; By the way, the symbols that start with colon (":") are called
;; "keywords" -- we will play with them later.  For now, we use them
;; because, like `t' and `nil', they evaluate to themselves, and more
;; importantly for this tutorial, Emacs displays them in a highlighted
;; way if you have Font-Lock mode enabled.  If they are not highlighted,
;; eval this sexp:
(global-font-lock-mode 1)

;; Anyway, in the three lists produced above (prior to the Font-Lock
;; blurb), the `eq' function compares its two args and returns t if they
;; are "equivalent" and nil otherwise.  You should see a bunch of t, a
;; bunch of nil, and a pair of (perhaps unexpected) nil.  Or perhaps you
;; wisely mistrusted authority and held no expectations whatsoever, and
;; are thus now ready to consider the second question: Why the quotes
;; around "equivalent"?

;; Surely t is t, nil is nil, and the empty list is "equivalent" to nil.
;; This is the bedrock of our experience.  It appears that according to
;; Emacs, 42 is 42, too -- well, that's somewhat reassuring.  But what
;; about the last sexp?  And what about:

;; >> Eval this sexp.
(let ((me          "me")
      (myself      "myself")
      (i           "i")
      (me/myself/i "me/myself/i"))
  (list :new-strings-not-eq
        me/myself/i
        (concat me "/" myself "/" i)
        (eq me/myself/i
            (concat me "/" myself "/" i))))

;; The rest of the assurance comes from understanding the difference
;; between an object and the container in which the object resides.  If
;; you are Bilbo Baggins giving away gifts for a birthday party but have
;; run out of new boxes, you might rummage around your vast collection
;; of old boxes and use one of those, instead.  Your guests may grumble
;; when they recognize the box (it is "equivalent" to what they have
;; seen before), but perhaps the contents will delight them anyway.

;; Unlike Bilbo, however, `list' always manages to find a new container
;; when given at least one argument (no argument => empty list => nil),
;; even for the same objects -- don't take the analogy too seriously!
;; Likewise, a new string, when written literally, such as "foo", or
;; when formed through `concat' behaves like a container of characters.
;; And unlike Bilbo's guests, when `eq' sees a container, it doesn't
;; look inside.

;; >> Eval this sexp.
(list :obviously-enough
      (eq (list t) t)
      (eq (list nil) nil)
      (eq (list some-truths) some-truths)
      (eq (list some-truths)
          (list some-truths))
      :numbers-are-not-containers
      (eq (length some-truths)
          (length some-truths))
      (eq 42 (+ (length some-truths)
                (* (length some-truths)
                   (length some-truths)))))

;; >> Eval these sexps.
(list (listp some-truths)
      (listp 42)
      (listp t)
      (listp nil))
(setq shared-truths some-truths)
(listp shared-truths)

;; Here we introduce `listp' which is a predicate (you can recognize
;; some predicates because they are named with a trailing "p" or "-p")
;; that returns non-`nil' if its argument is a list.  Note that `nil' is
;; indeed a list and that 42 is indeed not.

;; So, now that we have initialized `shared-truths', what is its value
;; as a variable?  And how is that value related to `some-truths'?

;; >> Eval this sexp.
(list (eq some-truths some-truths)
      (eq shared-truths shared-truths)
      (eq shared-truths some-truths))

;; You should see `(t t t)'.  You can also verify this with `C-h v'.
;; If we start thinking:
;;
;;   But wait a minute!  "When `eq' sees a container, it doesn't look
;;   inside", right?  Doesn't this result contradict that statement?  We
;;   gave `some-truths' the value returned by a call to `list', did we
;;   not?  Shouldn't `eq' return `nil' as for the `:surprise!' result
;;   above?
;;
;; Then that means we have forgotten an important detail: We used `setq'
;; to initialize `shared-truths' and NOT `list'.  For containers, `setq'
;; does not make a new container and consequently does not copy contents
;; either.  Strings are a form of container.  More precisely, it is kind
;; of "sequence".

;; >> There is much more to be said about containers (and sequences)
;; >> than this tutorial covers.  For complete info, eval the following
;; >> sexps.  If Emacs cannot find the referenced info pages, you need
;; >> to install the "GNU Emacs Lisp Reference Manual" and try again.
(info "(elisp) Lists")
(info "(elisp) Sequences Arrays Vectors")

;; When you want to check equivalence of contents as well as container,
;; use `equal'.  This function recurses into its arguments as necessary.

;; >> Eval these sexps.
(list :no-surprise
      (equal t t)
      (equal nil nil)
      (equal (list) (list))
      (equal t (not nil))
      (equal nil (not t))
      (equal 42 (1+ (1- 42))))

(list :also-no-surprise
      (equal t nil)
      (equal nil t)
      (equal (list) (list nil))
      (equal "ignorance" "bliss"))

(list :surprise!-but-not-really
      (equal (list t) (list t))
      (equal (list nil) (list nil))
      (equal (concat "me" "/" "myself" "/" "i") "me/myself/i"))

;; Even though `equal' handles most objects, it doesn't handle
;; everything with the same aplomb.

;; >> Eval this sexp.
(let ((a 42)
      (b 42.0)
      (c (let ((six (* 2   3  ))) (+ six (* six six))))
      (d (let ((six (* 2.0 3.0))) (+ six (* six six)))))
  (list :floats-require-care
        ;; (list (type-of a) 'a a
        ;;       (type-of b) 'b b
        ;;       (type-of c) 'c c
        ;;       (type-of d) 'd d)
        (list :eq
              (eq a b) (eq a c) (eq a d)
              (eq b c) (eq b d) (eq c d))
        ;; (list :eql
        ;;       (eql a b) (eql a c) (eql a d)
        ;;       (eql b c) (eql b d) (eql c d))
        (list :equal
              (equal a b) (equal a c) (equal a d)
              (equal b c) (equal b d) (equal c d))))

;; While we let this sink in, with the help of:
(info "(elisp) Equality Predicates")
;; let's take a break from the mental stuff and do some finger exercises.
;; Some people complain that Emacs causes RSI and other problems, so be
;; sure to consult a certified health-care professional or Doctor of
;; Journalism (RIP HST 2005) before proceeding with this section.
;;
;; Did you, out of curiosity perhaps, modify the above sexp to comment-in
;; those parts originally commented-out?  If so, here is a fresh copy:

(let ((a 42)
      (b 42.0)
      (c (let ((six (* 2   3  ))) (+ six (* six six))))
      (d (let ((six (* 2.0 3.0))) (+ six (* six six)))))
  (list :floats-require-care
        ;; (list (type-of a) 'a a
        ;;       (type-of b) 'b b
        ;;       (type-of c) 'c c
        ;;       (type-of d) 'd d)
        (list :eq
              (eq a b) (eq a c) (eq a d)
              (eq b c) (eq b d) (eq c d))
        ;; (list :eql
        ;;       (eql a b) (eql a c) (eql a d)
        ;;       (eql b c) (eql b d) (eql c d))
        (list :equal
              (equal a b) (equal a c) (equal a d)
              (equal b c) (equal b d) (equal c d))))

;; >> Eval this sexp.
(setq transient-mark-mode (not (local-set-key "\C-\M-y" [?\C-y ?\C-\M-k]))
      comment-style 'indent
      comment-add 1)

;; >> For the following instructions, hold down the control and meta keys.
;; >> Type `a' until you are at the left-paren that opens the `let' form
;; >> (perhaps two or three times, depending on the buffer's contents).
;; >> Then `f' a few times to place point *between* these truth values:
nil t
;; >> Then `k' to kill the "t", in the process adding it to the kill ring.
;; >> Then `b' two or three times to return to the `setq' form.
;; >> Then `d' to "enter" the `setq' and `f' twice to find the `(not ...)'.
;; >> Then `y' to run a command comprising a `yank' followed by a `kill-sexp'.
;; >> Then `x' to eval the sexp (you should see 1 in the echo area).
;; >> Then `a' until you are at the `let' form.
;; >> Then `d f f', `d f f', `f b'.  (Where are you?)

;; >> For the following instructions, hold down control and meta keys
;; >> only when necessary, minimizing key motion where possible.
;; >> Move point (if neeed) to the open-paren of the `(list :eq ...)' form.
;; >> Type `C-M-x' (you should see something in the echo area).
;; >> Type `C-M-k'.
;; >> Type `C-u C-p', `C-SPC' `C-u C-n', `C-u C-n', `M-;' (meta-colon).
;; >> Type `C-M-o', `C-u C-y'.
;; >> Type `C-M-x'.
;; >> Type `M-2 C-M-SPC', `M-;'.
;; >> Type `C-M-x'.

;; OK, enough finger flinging!  Maybe you can put on some Chopin or Rage
;; Against the Machine for further (aural, background) study.  We now pop the
;; stack, introducing one last predicate, before finally returning to the `if'
;; special form.

;; >> Eval this sexp.
(string= "to be" (substring "or not to be" (/ 42 (/ (- 42 4 4 -2) (+ 4 2)))))

;; >> Eval this sexp a few times, responding with different input.
(if (string= "" (read-string "What do you think of Emacs? "))
    (error "No opinion!")
  "OK, Emacs can handle it.")

;; This example shows how `if' evaluates its first argument, using the
;; value to select which sub-expression to further evaluate.  The
;; unselected sub-expression is NOT evaluated.  We call `if' a "special
;; form" precisely because this selective evaluation order is not the
;; normal one used for functions like `list'.

;; >> Eval this sexp, responding with different input.
(list (string= "" (read-string "What do you think of Emacs? "))
      (error "No opinion!")
      "OK, Emacs can handle it.")

;; Note that unlike the prior sexp, this one always results in an error
;; after the input, no matter what you type in response to the question.
;; Another thing the observant reader will note is the indentation differs
;; between the two forms.  Was that a typo?

;; >> Go the beginning of each `error' and "OK..." line (two in all) and
;; >> type `M-SPC' (meta-space) to run the command `just-one-space'.
;; >> Then type `C-M-a' and `C-M-q'.  Repeat but insert spaces instead
;; >> of deleting them.  Repeat, but first replace "list" with "if" (or
;; >> alternatively, experiment on the prior sexp).

;; You should see the forms return to their original indentation.  In
;; other words, Emacs has very specific ideas about the "proper way" to
;; indent forms based on their initial symbol, and in the case of the
;; `if' special form, the second sub-expression is indented a bit extra
;; to help distinguish it from the third and following sub-expressions.
;;
;; Hey, what's this "and following" business?  Indeed, the Emacs Lisp `if'
;; is somewhat peculiar in that handles exactly one "then" sub-expression
;; followed by zero or more "else" sub-expressions (most programming
;; languages expect at most one "else" sub-expression).

;; >> Reindent this sexp using `C-M-q'.
(if (string= "" (read-string "What do you think of Emacs? "))
 (error "No opinion!")
:ok?
  ;; (fancy-message "Well, that's interesting!")
 ;; (fancy-message "Actually, Emacs is rather indifferent.")
:ok!)

;; >> Reindent and eval this sexp.
(list :wrinkly-voiced-puppet-advised-me-to
 (if (string= "ignorance" "strength")
"stick my head in the sand"
 ;; (fancy-message "ignorance leads to fear")
 ;; (fancy-message "fear leads to anger")
 (if (string= "war" "peace")
"pre-emptively hang the meddling wise"
 ;; (fancy-message "anger leads to hate")
 (if (string= "freedom" "slavery")
"imprison those who share ideas"
 ;; (fancy-message "hate leads to suffering")
"write free software"))))

;; Two other special forms related to (and in fact built upon) the `if'
;; special form are `when' and `unless'.  You can think of them as:
;;
;; (if CONDITION THEN nil)    same as (when CONDITION THEN...)
;; (if CONDITION nil ELSE...) same as (unless CONDITION ELSE...)
;;
;; Note that `when' allows multiple THEN sub-expressions.  As usual, the
;; value of these forms is the value of the last THEN or ELSE evaluated.

;; >> Eval this sexp.
(let ((opinion (read-string "What do you think of Emacs? "))
      (intrepid (format "You (%s)" (user-login-name))))
  (cond ((string= "" opinion)
         (list "You didn't say anything."
               "OK, whatever..."))
        ((= ?? (aref opinion (1- (length opinion))))
         (list (format "%s asked: %S" intrepid opinion)
               "How should I know?"))
        (t
         (list (format "%s said: %S" intrepid opinion)
               "No worries."))))

;; Here we introduce the `cond' special form, which is a generalization
;; of `if'.  Each of the three branches in this example has the form:
;; (CONDITION THEN...).  CONDITION expressions are evaluated in order
;; until one results in a non-nil value, at which point its associated
;; THEN... expressions are evaluated.  The value of the `cond' expression
;; is the value of the last expression evaluated, or nil, if all of the
;; CONDITIONs were nil.
;;
;; Also notable: `length' takes a string as well as a list; `aref'
;; likewise can handle strings; characters from strings can be referenced
;; by index (from zero, inclusive, through the string length, exclusive);
;; the first arg to both `format' and `fancy-message' is similar to that
;; for the C (programming language) library function `printf'; and lastly,
;; the literal representation of the question-mark character is `??'.

;; >> Type `M-:' (meta-colon), `??' (two question marks), and RET.
;; >> Then eval this sexp.
(list :some-characters
      (list ?? ?a ?b ?c)
      (list ?\C-j ?\C-k ?\C-l)
      (list ?\M-x ?\M-y ?\M-z))

;; We will look at characters more closely throughout the rest of this
;; tutorial, since they play a central role in the mini-project and,
;; indeed, in all aspects of Emacs operation and programming that deal
;; with text and/or command sequences.

;;; Iteration with WHILE
;;; ====================

;; Now we finally arrive at the answer of the third question, or more
;; precisely, at a way to frame the question so that the answer becomes
;; self-evident.

;; >> Eval this sexp.
(let ((todo (list "Chat with neighbor."
                  "Buy milk.")))
  (while (not (null todo))
    (fancy-message (car todo))
    (dawdle 3)
    (setq todo (cdr todo)))
  (fancy-message "OK, done. (Relax!)"))

;; You should see Emacs behave (almost) identically as it did when you
;; invoked `do-weekly-chores' (defined in lesson02.el), with a dawdle
;; time of 3 seconds.  Although we have converted the grammar of the
;; displayed messages and added an exhortation to relax, the overall
;; behavior remains the same: sequential display of weekly chores from
;; the todo list with dawdling between successive chores.
;;
;; More relevant to this lesson than these superficial changes is the
;; use of the `while' special form.  The approach has changed from
;; explicit sequencing (via calls to separate functions) in the body of
;; the `do-weekly-chores' defun, to iteration over a data set controlled
;; by an iteration-termination condition.
;;
;; But didn't we establish that an empty list is equivalent to nil?
;; Why not put that into practice?  Indeed!

;; >> Type these commands to change the iteration-termination condition.
;; >> `C-M-a', `M-3 C-M-d', `C-SPC', `M-w', `M-2 C-M-u', `C-M-f'
;; >> `C-M-d', `C-M-f', `SPC', `C-M-y', `C-M-a', `C-M-f'.
;; >> Eval the sexp to check that the behavior is the same.
;; >>
;; >> (Bonus: Figure out a more concise sequence of commands and send
;; >> it to ttn for inclusion (and credit) in a future tutorial release,
;; >> where conciseness takes into account movement of the control and
;; >> meta keys -- less movement (down/up) is more concise.)

;; We can do this because in the body of the `while', we reference part
;; of the data set with `car' and mutate the data set after using it.
;; If we omit the latter part (mutation) by removing `(setq todo ...)',
;; the iteration-termination condition is never satisfied and Emacs
;; loops forever, operating only on the first element of `todo' and
;; never advancing.  (Note: any similarity to Real Life handling of
;; (Real Life) todo lists is purely coincidental. :-)

;; Now we are at a crossroads for this fragment of code: by introducing
;; iteration of a (mostly) data-agnostic algorithm over a data set, we
;; have started to generalize the fragment.  The end of this process is
;; typically one of the following: (a) use the code as-is and forget
;; about it; or (b) "complete" the generalization, use the code, and
;; then save the algorithm part for application to another data set at
;; a later time.

;; Ugh, more quotes and parenthetical qualifiers -- tutorial author
;; is really weasling around this time!  Well, "complete" is easy to
;; justify for quoting -- a truly complete generalization is truly
;; useless:
;;
;;         OUTPUT <-- FUNCTION <-- INPUT
;;
;; [To be fair, we note that usefulness to humans reading the code is
;;  different than usefulness to Emacs -- that's the basis of this
;;  tutorial format (comments interspersed with code) after all!  But
;;  usefulness to humans is partially determined by the human (perhaps
;;  you are a know-it-all), so henceforth we say "useful" to mean "useful
;;  to Emacs", and you can judge for yourself its other meaning(s)...]
;;
;; So, to be useful we have to allow some specifics in the algorithm.
;; The parenthesized word alludes to the last message displayed, i.e.,
;; the "OK, done. (Relax!)".  That string is passed to `fancy-message'
;; like the other items in the `todo' -- why not flip it over the fence
;; that divides algorithm from data?

;; >> Eval this sexp to check that the behavior is the same.
(let ((todo (list "Chat with neighbor."
                  "Buy milk."
                  "OK, done. (Relax!)")))
  (while todo
    (fancy-message (car todo))
    (dawdle 3)
    (setq todo (cdr todo))))

;; You should have seen similar behavior but not exactly the same;
;; there is more dawdling now than before.

;; Thus, this was NOT an iso-functional change.  Let's be hypercritical
;; and call it a bug (so that we can fix it).  Besides, it's usually
;; easier, later, to adopt a less stringent view than it is to add (and
;; react accordingly to) a more stringent view.  How about:

;; >> Eval this sexp to check that the behavior is the same.
(let ((todo (list "Chat with neighbor."
                  "Buy milk."
                  "OK, done. (Relax!)")))
  (while todo
    (fancy-message (car todo))
    (setq todo (cdr todo))
    ;; Only dawdle *between* chores.
    (when todo
      (dawdle 3)))
  ;; Consistent value.
  t)

;; Now we make the dawdle time a parameter that can be disabled, define
;; a new function, and give the function a name.

;; >> Eval these sexps to check that the behavior is the same.
(let ((dawdle-time 3)
      (todo (list "Chat with neighbor."
                  "Buy milk."
                  "OK, done. (Relax!)")))
  (while todo
    (fancy-message (car todo))
    (setq todo (cdr todo))
    ;; Only dawdle *between* chores, if at all.
    (when (and dawdle-time todo)
      (dawdle dawdle-time)))
  ;; Consistent value.
  t)

(defun fancy-todo-list-display (todo &optional dawdle-time)
  (while todo
    (fancy-message (car todo))
    (setq todo (cdr todo))
    ;; Only dawdle *between* chores, if at all.
    (when (and dawdle-time todo)
      (dawdle dawdle-time)))
  t)

(fancy-todo-list-display (list "Chat with neighbor."
                               "Buy milk."
                               "OK, done. (Relax!)")
                         3)

;; But will `fancy-todo-list-display' actually be useful for the
;; mini-project?  How does displaying a todo list in a fancy way help us
;; edit SGF files?  And what about the five-years-no-release question?!

(fancy-todo-list-display (list "Learn of Emacs."
                               "Learn to use Emacs."
                               "Learn to write Emacs Lisp."
                               "Learn to understand learning."
                               "Learn to understand {un,re}learning."
                               "{Un,Re}learn to write Emacs Lisp."
                               "{Un,Re}learn to understand learning."
                               "{Un,Re}learn to understand {un,re}learning."
                               "Write Emacs Lisp tutorial lesson."))

;;; Lesson 04 Wrap Up
;;; =================

;; Although the five-years-no-release question is answered, the other
;; two deserve some attention.  "Sequentially processing the bytes
;; comprising an SGF file to yield a collection of game trees in memory"
;; is a pretty good description for `sgf-read-file' behavior.  Let's
;; fill in more of the sketch we started in lesson 03, first replacing
;; the ~~~something~~~ in `sgf-edit-file' and then adding an outline of
;; `sgf-read-file'.

(defun sgf-edit-file (filename edits)
  "Read in FILENAME (SGF[4] format), apply EDITS and write it out.
If EDITS is nil, skip the write and behave exactly like `sgf-read-file'."
  ;; TODO: (interactive "fFile (SGF[4] format): ")
  (let ((collection (sgf-read-file filename)))
    (if edits
        (sgf-write-file (sgf-edit collection edits))
      collection)))

(defun sgf-read-file (filename)
  "Parse FILENAME as SGF[4] and return a collection of game-trees."
  (let ((collection nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (~~~process-a-character~~~ (char-after (point)))
        (forward-char 1)))
    collection))

;; We see one similarity between the innermost part of `sgf-read-file'
;; and `fancy-todo-list-display' (they both use `while'), as well as
;; five important differences: the iteration-termination condition is
;; different, the data set is different, the reference method into the
;; data set is different, the handling of the referenced data set
;; element is different, and the mutation operation is different.  All
;; this leads to the conclusion that no, `fancy-todo-list-display' is
;; not useful per se for parsing data files, even though yes, its core
;; behavior of iterating over a data set is shared with `sgf-read-file'.
;;
;; Does this mean time spent generalizing `fancy-todo-list-display' was
;; time wasted?  Perhaps the next lesson will find a way to answer this.


;;; Local variables:
;;; mode: lisp-interaction
;;; End:

;;; lesson04.el ends here
