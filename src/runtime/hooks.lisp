;;;; src/runtime/hooks.lisp --- Running hooks: lists of functions called at a moment.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A hook is a list of functions the editor calls at one moment: after a file
;;;; is saved, as a buffer is killed, for the parts of the mode line. Hooks are
;;;; run here, so that a function that fails is dealt with the same way for
;;;; each kind of hook. RUN-HOOK calls every function for what it does, and
;;;; RUN-HOOK-UNTIL-SUCCESS asks a question as the screen is drawn or a key is
;;;; typed: one that fails is said once, in the debug log every time, and the
;;;; others are still called. RUN-HOOK-UNTIL-DONE offers a command's work, and
;;;; a function that fails fails the command. A hook whose functions' values
;;;; are gathered, or whose failure the caller says in its own words (a
;;;; refusal, a question), calls each through CALL-HOOK-FUNCTION. Not run here:
;;;; the hooks whose functions refuse what is about to happen by signalling an
;;;; error (before an edit, before a save), and those of the command loop, each
;;;; function guarded as a command is (before and after a command, the ticks,
;;;; at exit).

(in-package #:tuatara.runtime)

(define-condition quit (error) ()
  (:documentation "Signalled by keyboard-quit (\\`C-g') to abandon the current command or minibuffer.")
  (:report (lambda (c s) (declare (ignore c)) (format s "Quit"))))

(defgeneric host-say-hook-failure (host text)
  (:documentation "Say TEXT, that a hook function failed, where it is seen (the editor's echo
area); without a command loop, it is in the debug log alone.")
  (:method ((host host) text)
    (declare (ignore text))
    nil))

(defvar *hook-failures-said* t
  "True for a hook function's failure to be said where it is seen
\(HOST-SAY-HOOK-FAILURE); NIL while the caller says it in words of its own
\(a question, a refusal), the debug log still told.")

(defvar *hook-failures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The hook functions that have failed, each said once.")

(defun hook-function-name (function)
  "FUNCTION's name in lower case, for saying it failed: the symbol it is named
by, how a function made without a name is known where its source says so
\(lambda (buffer) :in save-buffer), else \"a function without a name\"."
  (let ((name (if (symbolp function) function (nth-value 2 (function-lambda-expression function)))))
    (if name (format nil "~(~a~)" name) "a function without a name")))

(defun hook-function-failed (function condition)
  "Note that FUNCTION, called from a hook, failed with CONDITION: in the debug
log every time, while debug mode is on, the calls under way the first time;
and the first time where it is seen (HOST-SAY-HOOK-FAILURE), while
*HOOK-FAILURES-SAID*. A function named by a symbol and defined again counts
as failing for the first time. Called before the calls under way are left."
  (let* ((key (if (and (symbolp function) (fboundp function)) (fdefinition function) function))
         (first (not (gethash key *hook-failures*)))
         (text (format nil "~a failed and was left out: ~a" (hook-function-name function) (failure-words condition))))
    (setf (gethash key *hook-failures*) t)
    (debug-report :error "~a~{~%    ~a~}" text (and first (not *secret-at-hand*) (backtrace-lines)))
    (when (and first *hook-failures-said*)
      (host-say-hook-failure *host* (format nil "~a failed and was left out: ~a"
                                            (hook-function-name function) (failure-words condition :said t))))))

(defmacro with-failure-said ((function condition) form &body failed)
  "The values of FORM, a call of FUNCTION from a hook; when it fails (signals
an error), the failure is noted (HOOK-FUNCTION-FAILED) and FAILED is
evaluated in its place, CONDITION bound to the error. \\`C-g' (QUIT) is no
failure and goes on to the caller, as does running out of memory."
  (let ((done (gensym "DONE")) (failure (gensym "FAILURE")) (c (gensym "CONDITION")))
    `(block ,done
       (let ((,condition
               (block ,failure
                 (handler-bind ((error (lambda (,c)
                                         (unless (typep ,c 'quit)
                                           (hook-function-failed ,function ,c)
                                           (return-from ,failure ,c)))))
                   (return-from ,done ,form)))))
         (declare (ignorable ,condition))
         ,@failed))))

(defun call-hook-function (function &rest arguments)
  "Call FUNCTION, one of a hook's, with ARGUMENTS and return its values. When
it fails, NIL and the error, as two values; the failure is noted
\(HOOK-FUNCTION-FAILED): in the debug log, and said once. \\`C-g' (QUIT) is
no failure and goes on to the caller, as does running out of memory."
  (declare (dynamic-extent arguments))
  (with-failure-said (function condition) (apply function arguments)
    (values nil condition)))

(defun run-hook (functions &rest arguments)
  "Call each of FUNCTIONS, a hook's, with ARGUMENTS, for what they do, in
order. One that fails is noted and said once (CALL-HOOK-FUNCTION) and the
others are still called. Returns NIL, or the first error met, for a caller
that is to give up once they have all run."
  (declare (dynamic-extent arguments))
  (let ((first nil))
    (dolist (function functions first)
      (with-failure-said (function condition) (apply function arguments)
        (unless first (setf first condition))))))

(defun run-hook-until-success (functions &rest arguments)
  "Ask each of FUNCTIONS, a hook's, with ARGUMENTS, in order, until one answers
\(returns other than NIL), and return that answer; NIL when none does. One
that fails is noted and said once (CALL-HOOK-FUNCTION) and counts as no
answer: for questions asked as the screen is drawn or a key typed."
  (declare (dynamic-extent arguments))
  (dolist (function functions nil)
    (let ((answer (apply #'call-hook-function function arguments)))
      (when answer (return answer)))))

(defun run-hook-until-done (functions &rest arguments)
  "Offer a command's work to each of FUNCTIONS, a hook's, with ARGUMENTS, in
order, until one does it (returns other than NIL), and return what it
returned; NIL when none does. One that fails fails the command: its error
goes on to the caller, which says it as it says any command's, the debug log
told with the calls under way (REPORTING-ERRORS)."
  (declare (dynamic-extent arguments))
  (dolist (function functions nil)
    (let ((done (reporting-errors ("~(~a~)" (hook-function-name function)) (apply function arguments))))
      (when done (return done)))))
