;;;; lisp/editing/questions.lisp --- Asking for a number or a pattern, and the histories of answers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The questions many commands ask in the minibuffer beside a line of text:
;;;; a whole number, a decimal or a fraction, and a pattern (a regular
;;;; expression or literal text, switched by a key) with its history.

(in-package #:tuatara.defaults)

(defparameter *history-kept* 200
  "How many of the newest entries a history kept while the editor runs holds:
the commands run by name, the patterns typed, the places definitions were
looked up from.")

(defun pushed-and-kept (item history)
  "HISTORY, a list, with ITEM pushed on it, no more than its newest
*HISTORY-KEPT* entries."
  (let ((history (cons item history)))
    (when (> (length history) *history-kept*)
      (setf (cdr (nthcdr (1- *history-kept*) history)) nil))
    history))

(defun read-number-as (parse prompt default valid refusal)
  "Ask for a number in the minibuffer, read from what is typed by PARSE, a
function of the text giving the number or NIL; PROMPT a format string given
DEFAULT, the number offered: the number typed, or DEFAULT when nothing is.
Asks again, saying REFUSAL first, while PARSE gives no number that VALID, a
function of it, is true for."
  (loop for asking = (format nil prompt default) then (format nil "~a ~?" refusal prompt (list default))
        do (let* ((text (string-trim " " (read-string asking)))
                  (n (and (plusp (length text)) (funcall parse text))))
             (cond ((zerop (length text)) (return default))
                   ((and n (funcall valid n)) (return n))))))

(defun read-whole-number (prompt default &key (valid #'plusp) (refusal "Not a whole number above 0."))
  "Ask for a whole number in the minibuffer, PROMPT a format string given
DEFAULT, the number offered: the number typed, or DEFAULT when nothing is.
Asks again, saying REFUSAL first, while what is typed is no whole number that
VALID, a function of it, is true for (as standard, more than 0)."
  (read-number-as (lambda (text) (and (every #'digit-char-p text) (parse-integer text)))
                  prompt default valid refusal))

(defun read-decimal-number (prompt default &key (valid #'plusp) (refusal "Not a number above 0."))
  "Ask for a number in the minibuffer, digits with perhaps a point and more
digits after (0.8), PROMPT a format string given DEFAULT, the number offered:
the number typed, exact, or DEFAULT when nothing is. Asks again, saying
REFUSAL first, while what is typed is no such number that VALID, a function
of it, is true for (as standard, more than 0)."
  (read-number-as #'tuatara.text:parse-decimal prompt default valid refusal))

(defun fraction-p (value)
  "True when VALUE is a number above 0 and at most 1, as a share or a
confidence is."
  (and (realp value) (< 0 value) (<= value 1)))

(defun read-fraction (prompt default)
  "Ask for a number above 0 and at most 1 (FRACTION-P) in the minibuffer, as
READ-DECIMAL-NUMBER asks, PROMPT a format string given DEFAULT, the number
offered."
  (read-decimal-number prompt default :valid #'fraction-p :refusal "Not a number above 0 and at most 1."))

(defvar *pattern-history* '()
  "The patterns typed in the questions that take a regular expression, most
recent first, one list for them all: a search (\\[regex-search-forward]) and
its replacements, a search filter, keep-lines and flush-lines, a split,
sort-regexp-fields. M-p and M-n bring them back as the next is typed. Kept
while the editor runs, never written to a file.")

(defun keep-typed-pattern (text)
  "Put TEXT, typed in a question that takes a regular expression, first in
*PATTERN-HISTORY*, once; nothing for empty TEXT."
  (when (plusp (length text))
    (setf *pattern-history* (pushed-and-kept text (remove text *pattern-history* :test #'string=)))))

(defvar *last-typed* 0
  "The internal real time at which the text of a question taking a pattern
last changed.")

(defvar *toggle-pattern-kind* nil
  "While a question other than a search's asks for a pattern (READ-PATTERN,
\\[search-in-files]): a function of no arguments that switches it between a
regular expression and literal text, for \\[search-toggle-regex].")
(pushnew '*toggle-pattern-kind* *question-variables*)

(defvar *pattern-finish* nil
  "While READ-PATTERN asks: a function of no arguments that ends its question
when what is typed can be used, and otherwise says why in the prompt.")
(pushnew '*pattern-finish* *question-variables*)

(defvar *pattern-keymap* (make-keymap :name "pattern")
  "The keys of every question that takes a pattern, before the minibuffer's:
\\<pattern>\\[search-toggle-regex] switches between a regular expression and literal text.")

(define-keys *pattern-keymap* '(("M-r" search-toggle-regex)))

(defvar *pattern-answer-keymap* (make-keymap :name "pattern-answer" :parent *pattern-keymap*)
  "The keys of a question that takes a pattern as its answer (READ-PATTERN):
those of *PATTERN-KEYMAP*, and \\<pattern-answer>\\[pattern-finish], which waits for a
pattern that can be used.")

(define-command pattern-finish ()
  "In a question that takes a pattern: take what is typed, or when it cannot
be used, say why in the prompt and wait for it to be mended
\(\\<minibuffer>\\[minibuffer-cancel] gives up)."
  (if *pattern-finish* (funcall *pattern-finish*) (finish-minibuffer)))

(define-keys *pattern-answer-keymap* '(("RET" pattern-finish) ("C-j" pattern-finish)))

(defun pattern-kind-words (regex)
  "How a question that takes a pattern names it, first in its prompt: literal
for literal text, else regex, or thorough regex with *REGEX-THOROUGH*; REGEX
tells which."
  (cond ((not regex) "literal")
        (tuatara.syntax:*regex-thorough* "thorough regex")
        (t "regex")))

(defun pattern-prompt (regex words &optional note)
  "The prompt of a question that takes a pattern, its kind first, as REGEX
says (PATTERN-KIND-WORDS), then WORDS saying what is done with it, and NOTE
in brackets when given, why the pattern cannot be used, as in Regex keep lines
[incomplete]: ."
  (format nil "~a ~a~@[ [~a]~]: " (string-capitalize (pattern-kind-words regex) :end 1) words note))

(defun pattern-problem (text regex)
  "Why TEXT cannot be used as a pattern, a regular expression when REGEX is
true, else literal text; NIL when it can, or is empty."
  (and (plusp (length text)) regex
       (nth-value 1 (make-pattern-matcher text :regex t))))

(defun refused-pattern-words (problem abort-keys)
  "What a prompt says when a pattern was refused for PROBLEM (PATTERN-PROBLEM):
the problem, to finish the pattern when it is incomplete or else to mend it,
or ABORT-KEYS, a key description, to give up."
  (format nil "~a; ~:[mend~;finish~] it, or ~a gives up" problem (string= problem "incomplete") abort-keys))

(defun refusable-pattern-prompt (regex words problem refused)
  "The prompt of a question that takes a pattern (PATTERN-PROMPT) for what
WORDS says is done with it, PROBLEM why what is typed cannot be used
\(PATTERN-PROBLEM) or NIL: once REFUSED, with what to do about it
\(REFUSED-PATTERN-WORDS)."
  (pattern-prompt regex words
                  (and problem
                       (if refused
                           (refused-pattern-words problem
                                                  (substitute-command-keys "\\<minibuffer>\\[minibuffer-cancel]"))
                           problem))))

(defun pattern-regex (text regex)
  "TEXT, a pattern READ-PATTERN read, as a regular expression: as it is when
REGEX is true, else literal text with every special character escaped."
  (if regex text (cl-ppcre:quote-meta-chars text)))

(defun read-pattern (words &key (initial "") count)
  "Ask for a pattern for what WORDS says is done with it (\"keep lines\"), the
prompt naming its kind first as a search's does (Regex keep lines: ).
\\<pattern>\\[search-toggle-regex] switches between a regular expression and
literal text. \\<minibuffer>\\[minibuffer-previous-history] and
\\[minibuffer-next-history] bring back the patterns typed before
\(*PATTERN-HISTORY*). A regular expression that cannot be used says why in the
prompt, \\<pattern-answer>\\[pattern-finish] waiting for it to be mended.
INITIAL is offered to begin with. COUNT, a function of what is typed and
whether it is a regular expression, keeps a count up to date as it is typed
and returns two values, as PATTERN-COUNT-UPDATE does: true while it counts,
and what to show before the prompt. Returns what was typed, kept among the
patterns typed before, and as a second value whether it is a regular
expression."
  (let* ((editor *editor*) (regex t) (refused nil)
         ;; the last text and kind looked at, and why it cannot be used: worked out once
         (checked (list nil)))
    (labels ((typed () (editor-minibuffer-text editor))
             (problem (text)
               (let ((key (cons text regex)))
                 (if (equal (car checked) key)
                     (cdr checked)
                     (cdr (setf checked (cons key (pattern-problem text regex)))))))
             (prompt (text) (refusable-pattern-prompt regex words (problem text) refused)))
      (let ((text (let ((*toggle-pattern-kind* (lambda () (setf regex (not regex) refused nil)))
                        (*pattern-finish* (lambda ()
                                          (if (problem (typed))
                                              (setf refused t)
                                              (finish-minibuffer)))))
                    (read-string (prompt initial)
                                 :initial initial :history *pattern-history* :keymap *pattern-answer-keymap*
                                 :changed (lambda (text)
                                            (declare (ignore text))
                                            (setf *last-typed* (get-internal-real-time) refused nil))
                                 :update (lambda ()
                                           (multiple-value-bind (going shown)
                                               (if count (funcall count (typed) regex) (values nil nil))
                                             (values going (prompt (typed)) shown)))))))
        (keep-typed-pattern text)
        (values text regex)))))
