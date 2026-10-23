;;;; lisp/editing/extended-command.lisp --- Commands by place, and running a command by name.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Which buffers a command made for one kind of buffer runs in, the
;;;; candidates M-x offers, and completion at point.

(in-package #:tuatara.defaults)

(defvar *command-history* '()
  "Names of commands run through \\[execute-extended-command], most recent
first, the newest *HISTORY-KEPT* of them.")

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

(defvar *pattern-done* nil
  "While READ-PATTERN asks: a function of no arguments that ends its question
when what is typed can be used, and otherwise says why in the prompt.")

(defvar *pattern-keymap* (make-keymap :name "pattern")
  "The keys of every question that takes a pattern, before the minibuffer's:
\\<pattern>\\[search-toggle-regex] switches between a regular expression and literal text.")

(define-keys *pattern-keymap* '(("M-r" search-toggle-regex)))

(defvar *pattern-answer-keymap* (make-keymap :name "pattern-answer" :parent *pattern-keymap*)
  "The keys of a question that takes a pattern as its answer (READ-PATTERN):
those of *PATTERN-KEYMAP*, and \\<pattern-answer>\\[pattern-done], which waits for a
pattern that can be used.")

(define-command pattern-done ()
  "In a question that takes a pattern: take what is typed, or when it cannot
be used, say why in the prompt and wait for it to be mended
\(\\<minibuffer>\\[minibuffer-abort] gives up)."
  (if *pattern-done* (funcall *pattern-done*) (finish-minibuffer)))

(define-keys *pattern-answer-keymap* '(("RET" pattern-done) ("C-j" pattern-done)))

(defun pattern-kind-words (regex)
  "How a question that takes a pattern names it, first in its prompt: literal
for literal text, else regex, or thorough regex with *REGEX-THOROUGH*; REGEX
tells which."
  (cond ((not regex) "literal")
        (tuatara.syntax:*regex-thorough* "thorough regex")
        (t "regex")))

(defun pattern-prompt (regex words &optional note)
  "The prompt of a question that takes a pattern: its kind first, as REGEX
says (PATTERN-KIND-WORDS), then WORDS saying what is done with it, and NOTE
in brackets when given, why the pattern cannot be used: Regex keep lines
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
                           (refused-pattern-words problem (substitute-command-keys "\\<minibuffer>\\[minibuffer-abort]"))
                           problem))))

(defun pattern-regex (text regex)
  "TEXT, a pattern READ-PATTERN read, as a regular expression: as it is when
REGEX is true, else literal text with every special character escaped."
  (if regex text (cl-ppcre:quote-meta-chars text)))

(defun read-pattern (words &key (initial "") count)
  "Ask for a pattern for what WORDS says is done with it (\"keep lines\"), the
prompt naming its kind first as a search's does (Regex keep lines: ):
\\<pattern>\\[search-toggle-regex] switches between a regular expression and literal text,
\\<minibuffer>\\[minibuffer-previous-history] and \\[minibuffer-next-history] bring back the patterns typed before
\(*PATTERN-HISTORY*), and a regular expression that cannot be used says why
in the prompt, \\<pattern-answer>\\[pattern-done] waiting for it to be mended. INITIAL is offered to
begin with. COUNT, a function of what is typed and whether it is a regular
expression, keeps a count up to date as it is typed and returns two values,
as PATTERN-COUNT-UPDATE does: true while it counts, and what to show before
the prompt. Returns what was typed, kept among the patterns typed before,
and as a second value whether it is a regular expression."
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
                        (*pattern-done* (lambda ()
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

(defvar *mode-commands* (make-hash-table :test 'eq)
  "The commands made for one kind of buffer only, such as a list's own keys: for
each, a list of conses of where it runs, a major mode's name or a function of
a buffer (COMMANDS-FOR-BUFFERS), and what to call its buffers.")

(defun commands-for-mode (mode where &rest commands)
  "Let COMMANDS run in buffers of major mode MODE only, and of the other modes
they are made for by calls of their own; elsewhere they say that they work in
WHERE (\"the buffer list (\\[buffers])\") and do nothing, and
\\[execute-extended-command] does not offer them."
  (dolist (command commands)
    (setf (gethash command *mode-commands*)
          (append (remove mode (gethash command *mode-commands*) :key #'car) (list (cons mode where))))))

(defun commands-for-buffers (test where &rest commands)
  "Let COMMANDS run only in the buffers for which TEST, the name of a function
of a buffer, is true, and in those COMMANDS-FOR-MODE gave them; elsewhere
they say that they work in WHERE (\"a quick edit\") and do nothing, and
\\[execute-extended-command] does not offer them."
  (apply #'commands-for-mode test where commands))

(defun command-place-p (place buffer)
  "True when BUFFER is a place a command made for PLACE runs in: PLACE the
name of a major mode, BUFFER in that mode or one made from it; else the name
of a function of a buffer (COMMANDS-FOR-BUFFERS), true for BUFFER."
  (if (find-mode place)
      (typep (buffer-major-mode buffer) place)
      (funcall place buffer)))

(defun minibuffer-active-p (buffer)
  "True while the minibuffer reads something, whatever BUFFER is."
  (declare (ignore buffer))
  (and *editor* (editor-minibuffer-active *editor*) t))

(apply #'commands-for-buffers 'minibuffer-active-p "the minibuffer, while it asks something"
       (mapcar (lambda (name) (find-symbol name '#:tuatara.command))
               '("MINIBUFFER-NEXT-CANDIDATE" "MINIBUFFER-PREVIOUS-CANDIDATE" "MINIBUFFER-COMPLETE" "MINIBUFFER-SUBMIT-INPUT"
                 "MINIBUFFER-DONE" "MINIBUFFER-ABORT" "MINIBUFFER-BACKWARD-DELETE-CHAR" "MINIBUFFER-BACKWARD-CHAR"
                 "MINIBUFFER-FORWARD-CHAR" "MINIBUFFER-BEGINNING" "MINIBUFFER-END" "MINIBUFFER-KILL-LINE"
                 "MINIBUFFER-CLEAR" "MINIBUFFER-BACKWARD-KILL-WORD")))

(apply #'commands-for-buffers 'menu-open-p "an open menu"
       (mapcar (lambda (name) (find-symbol name '#:tuatara.command))
               '("MENU-TOGGLE-SWITCH" "MENU-SET-OPTION" "MENU-ENTRY-LEFT-OUT" "MENU-DESCRIBE")))

(defvar *file-changing-commands* '()
  "Commands that change files, refused in a buffer opened safely (a rimu list
of a folder opened so among them); a package adds its own that do.")

(defvar *command-refusal-functions* '()
  "Functions of a command and the current buffer, each giving what to say
instead of running the command there, or NIL when it has nothing against
it: a rimu list of a folder as a commit left it refuses what works on the
files as they are now.")

(defun mode-command-refusal (command)
  "What to say instead of running COMMAND, made for buffers of another major mode
than the current buffer's, or for other buffers (COMMANDS-FOR-BUFFERS), the keys
it names written in (SUBSTITUTE-COMMAND-KEYS), or one that changes files
\(*FILE-CHANGING-COMMANDS*) in a buffer opened safely, or what one of
*COMMAND-REFUSAL-FUNCTIONS* says; NIL when it may run."
  (let ((entries (gethash command *mode-commands*)))
    (cond ((and entries *editor*
                (notany (lambda (entry) (command-place-p (car entry) (current-buffer))) entries))
           (format nil "~(~a~) works in ~{~a~^, ~} only" command
                   (mapcar (lambda (entry) (substitute-command-keys (cdr entry) :marks t)) entries)))
          ((and *editor* (member command *file-changing-commands*) (tuatara.io:safe-visit-p (current-buffer)))
           (format nil "~(~a~) changes files, and ~a is opened safely" command
                   (tuatara.buffer:buffer-name (current-buffer))))
          (*editor*
           (some (lambda (function) (funcall function command (current-buffer))) *command-refusal-functions*)))))

(defun command-candidates (&key everywhere)
  "Completion candidates for every command that can run here (not those made
for another kind of buffer), or with EVERYWHERE for every command there is:
its key bindings in brackets right after its name, and the first sentence of
its docstring at the right."
  (let ((keymaps (active-keymaps *editor*)))
    (loop for symbol being the hash-keys of *commands*
          unless (and (not everywhere) (mode-command-refusal symbol))
          collect (let ((name (string-downcase (symbol-name symbol)))
                        (keys (where-is symbol keymaps)))
                    (make-candidate name
                                    :suffix (and keys (list (list (format nil "(~{~a~^, ~})" keys) :completion-key)))
                                    :annotation (list (documentation-field (documentation symbol 'function) 1/2))
                                    :value symbol)))))

(defgeneric mode-completion-at-point (mode buffer position)
  (:documentation "What completing at POSITION of BUFFER in MODE offers, as six values: the
start and end of the word there, the candidates for it (a list of CANDIDATE),
and the functions giving what is said beside a candidate, its documentation,
and whether a character can be part of a word (see COMPLETE-IN-TEXT); NIL when
MODE completes nothing.")
  (:method (mode buffer position) (declare (ignore mode buffer position)) nil))

(define-command completion-at-point ()
  "Complete the word at point from a list beside it: in Lisp, the operators
after an open bracket, the variables, operators and classes elsewhere, the
keywords after a colon and a package's names after its name and a colon; in a
buffer open in a language server, what the server offers; with what each is
beside it and the chosen one's parameters and documentation beside the list.
Typing narrows the list, words in any order;
\\<popup>\\[popup-next] and \\[popup-previous] choose, \\[popup-insert] or
\\`TAB' put the chosen one in, \\[popup-quit] closes it. When one name alone
fits, it goes in at once."
  (let ((b (current-buffer)))
    (cond ((hex-view-here-p) (message "Nothing to complete in the hex view"))
          ((tuatara.buffer:buffer-local b :read-only) (message "Buffer is read-only: ~a" (tuatara.buffer:buffer-name b)))
          (t (complete-at-point-in b)))))

(defun complete-at-point-in (b &key quiet)
  "Complete the word at point of B as its major mode offers (MODE-COMPLETION-AT-POINT);
where it offers nothing, say so, unless QUIET."
  (multiple-value-bind (start end candidates annotate document constituent)
      (mode-completion-at-point (buffer-major-mode b) b (point))
    (if (null start)
        (unless quiet (message "Nothing to complete here"))
        (complete-in-text b start end candidates :annotate annotate :document document
                                                 :constituent (or constituent #'graphic-char-p)))))

(define-command execute-extended-command ()
  "Run a command chosen by name, with completion over every command: the
candidates show each command's keys and the first line of its documentation."
  (let ((chosen (completing-read "M-x " (command-candidates) :require-match t :history *command-history*)))
    (when (symbolp chosen)
      (setf *command-history* (pushed-and-kept (string-downcase (symbol-name chosen)) *command-history*))
      (setf (editor-this-command *editor*) chosen)
      ;; repeat runs the command chosen again, not the asking
      (unless (member chosen *not-repeatable*)
        (setf (editor-repeatable *editor*) (list* chosen (prefix-argument) (editor-this-key *editor*))))
      (tuatara.runtime:debug-report :command "~(~a~) (M-x)" chosen)
      ;; a command made for another place says so, as it does from its keys
      (let ((refusal (and *command-refusal-function* (funcall *command-refusal-function* chosen))))
        (if refusal
            (message "~a" refusal)
            (funcall (find-command chosen)))))))
