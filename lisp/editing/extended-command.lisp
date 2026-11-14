;;;; lisp/editing/extended-command.lisp --- Commands by place, and running a command by name.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Which buffers a command made for one kind of buffer runs in, the
;;;; candidates M-x offers, and completion at point.

(in-package #:tuatara.defaults)

(defvar *command-history* '()
  "Names of commands run through \\[execute-extended-command], most recent
first, the newest *HISTORY-KEPT* of them.")

(defvar *mode-commands* (make-hash-table :test 'eq)
  "The commands made for one kind of buffer only, such as a list's own keys: for
each, a list of conses of where it runs, a major mode's name or a function of
a buffer (COMMANDS-FOR-MODE), and what to call its buffers.")

(defun commands-for-mode (mode where &rest commands)
  "Let COMMANDS run in buffers of major mode MODE only, and of the other modes
they are made for by calls of their own; elsewhere they say that they work in
WHERE (\"the buffer list (\\[buffers])\") and do nothing, and
\\[execute-extended-command] does not offer them."
  (dolist (command commands)
    (setf (gethash command *mode-commands*)
          (append (remove mode (gethash command *mode-commands*) :key #'car) (list (cons mode where))))))

(defvar *broken-command-places* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The places of *MODE-COMMANDS* that are no major mode and whose function is
not there or failed (COMMAND-PLACE-P): no buffer is one of them.")

(defun command-place-p (place buffer)
  "True when BUFFER is a place a command made for PLACE runs in: PLACE the
name of a major mode, BUFFER in that mode or one made from it; else the name
of a function of a buffer (COMMANDS-FOR-MODE), true for BUFFER. A PLACE
that is neither, or whose function fails, is no place at all from then on,
said once in the debug log: the commands made for it run nowhere, and the
others are not hindered."
  (cond ((find-mode place) (typep (buffer-major-mode buffer) place))
        ((gethash place *broken-command-places*) nil)
        (t (multiple-value-bind (answer failure)
               (let ((tuatara.runtime:*hook-failures-said* nil))
                 (call-hook-function place buffer))
             (when failure (setf (gethash place *broken-command-places*) t))
             answer))))

(defun minibuffer-active-p (buffer)
  "True while the minibuffer reads something, whatever BUFFER is."
  (declare (ignore buffer))
  (and *editor* (editor-minibuffer-active *editor*) t))

(apply #'commands-for-mode 'minibuffer-active-p "the minibuffer, while it asks something"
       (mapcar (lambda (name) (find-symbol name '#:tuatara.command))
               '("MINIBUFFER-NEXT-CANDIDATE" "MINIBUFFER-PREVIOUS-CANDIDATE"
                 "MINIBUFFER-COMPLETE"
                 "MINIBUFFER-SUBMIT-INPUT"
                 "MINIBUFFER-FINISH" "MINIBUFFER-CANCEL" "MINIBUFFER-BACKWARD-DELETE-CHAR" "MINIBUFFER-BACKWARD-CHAR"
                 "MINIBUFFER-FORWARD-CHAR" "MINIBUFFER-BEGINNING" "MINIBUFFER-END" "MINIBUFFER-KILL-LINE"
                 "MINIBUFFER-CLEAR" "MINIBUFFER-BACKWARD-KILL-WORD")))

(apply #'commands-for-mode 'menu-open-p "an open menu"
       (mapcar (lambda (name) (find-symbol name '#:tuatara.command))
               '("MENU-TOGGLE-SWITCH" "MENU-SET-OPTION" "MENU-ENTRY-LEFT-OUT" "MENU-ENTRY-REFUSED" "MENU-DESCRIBE")))

(defvar *file-changing-commands* '()
  "Commands that change files, refused in a buffer opened safely (a rimu list
of a directory opened so among them); a package adds its own that do.")

(defvar *command-refusal-functions* '()
  "Functions of a command and the current buffer, each giving what to say
instead of running the command there, or NIL when it has nothing against
it: a rimu list of a directory as a commit left it refuses what works on the
files as they are now.")

(defun command-refusal-reason (command)
  "Why COMMAND may not run in the current buffer: :ELSEWHERE when it is made
for buffers of another major mode, or for other buffers
\(COMMANDS-FOR-MODE); :SAFE when it changes files (*FILE-CHANGING-COMMANDS*)
and the buffer is opened safely; else what one of *COMMAND-REFUSAL-FUNCTIONS*
says, the first that fails refusing it too, saying it could not be checked.
NIL when it may run."
  (when *editor*
    (let ((entries (gethash command *mode-commands*))
          (buffer (current-buffer)))
      (cond ((and entries (notany (lambda (entry) (command-place-p (car entry) buffer)) entries)) :elsewhere)
            ((and (member command *file-changing-commands*) (tuatara.io:safe-visit-p buffer)) :safe)
            (t (let ((tuatara.runtime:*hook-failures-said* nil))
                 (dolist (function *command-refusal-functions*)
                   (multiple-value-bind (refusal failure) (call-hook-function function command buffer)
                     (cond ((typep failure 'error)
                            (return (format nil "~(~a~) could not be checked (~a): not run"
                                            command (tuatara.runtime:failure-words failure :said t))))
                           (refusal (return refusal)))))))))))

(defun mode-command-refusal (command)
  "What to say instead of running COMMAND, made for buffers of another major mode
than the current buffer's, or for other buffers (COMMANDS-FOR-MODE), the keys
it names written in (SUBSTITUTE-COMMAND-KEYS), or one that changes files
\(*FILE-CHANGING-COMMANDS*) in a buffer opened safely, or what one of
*COMMAND-REFUSAL-FUNCTIONS* says; NIL when it may run."
  (let ((reason (command-refusal-reason command)))
    (case reason
      (:elsewhere
       (format nil "~(~a~) works in ~{~a~^, ~} only" command
               (mapcar (lambda (entry) (substitute-command-keys (cdr entry) :marks t))
                       (gethash command *mode-commands*))))
      (:safe
       (format nil "~(~a~) changes files, and ~a is opened safely" command
               (tuatara.buffer:buffer-name (current-buffer))))
      (t reason))))

(defmethod host-command-refusal ((host defaults-host) command)
  "Refuse a command made for one kind of buffer anywhere else, or one that
changes files in a buffer opened safely (MODE-COMMAND-REFUSAL)."
  (mode-command-refusal command))

(defun command-candidates (&key everywhere)
  "Completion candidates for every command that can run here (not those made for
another kind of buffer or for keys alone), or with EVERYWHERE for every command
there is: its key bindings in brackets right after its name, and the first
sentence of its docstring at the right."
  (let ((keymaps (active-keymaps *editor*)))
    (loop for symbol being the hash-keys of *commands*
          unless (and (not everywhere) (or (key-only-command-p symbol) (command-refusal-reason symbol)))
          collect (let ((name (string-downcase (symbol-name symbol)))
                        (keys (where-is symbol keymaps)))
                    (make-candidate name
                                    :suffix (and keys (list (list (format nil "(~{~a~^, ~})" keys) :completion-key)))
                                    :annotation (list (documentation-field (documentation symbol 'function) 1/2))
                                    :value symbol)))))

(defgeneric mode-completion-at-point (mode buffer position)
  (:documentation "What completing at POSITION of BUFFER in MODE offers, as
seven values. They are the start and end of the word there, the candidates for
it (a list of CANDIDATE), and the functions giving what is said beside a
candidate, its documentation, and whether a character can be part of a word
(see COMPLETE-IN-TEXT). The last is true for the candidates to be kept in the
order given. NIL when MODE completes nothing. A LATER gives the same values
once they come, for a mode that asks a program.")
  (:method (mode buffer position) (declare (ignore mode buffer position)) nil))

(define-command completion-at-point ()
  "Complete the word at point from a list beside it. In Lisp, the list holds
the operators after an open bracket, the variables, operators and classes
elsewhere, the keywords after a colon and a package's names after its name and
a colon. In a buffer open in a language server, it holds what the server
offers. What each is stands beside it, and the chosen one's parameters and
documentation stand beside the list. Typing narrows the list, words in any
order; \\<popup>\\[popup-next] and \\[popup-previous] choose, \\[popup-insert]
or \\`TAB' put the chosen one in, \\[popup-quit] closes it. When one name
alone fits, it goes in at once. Where the mode offers nothing, the words of
the buffers that start with the text before point, as \\[complete-word] lists
them."
  (let ((b (current-buffer)))
    (when (hex-view-here-p) (refuse "Nothing to complete in the hex view"))
    (check-writable b)
    (complete-at-point-in b :words t)))

(defun complete-at-point-in (b &key quiet words)
  "Complete the word at point of B as its major mode offers (MODE-COMPLETION-AT-POINT);
where it offers nothing, with WORDS the words of the buffers (WORD-COMPLETION-AT-POINT),
or with WORDS :ONLY those alone; where nothing is offered, say so, unless QUIET."
  (flet ((complete (&optional start end candidates annotate document constituent keep-order)
           (if (null start)
               (unless quiet (message "Nothing to complete here"))
               (complete-in-text b start end candidates :annotate annotate :document document :keep-order keep-order
                                                        :constituent (or constituent #'graphic-char-p)))))
    (if (eq words :only)
        (multiple-value-call #'complete (word-completion-at-point b (point)))
        (multiple-value-call #'take-answer b
          (lambda (&optional start &rest more)
            (if (or start (not words))
                (apply #'complete start more)
                (multiple-value-call #'complete (word-completion-at-point b (point)))))
          (mode-completion-at-point (buffer-major-mode b) b (point))))))

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
      (let ((refusal (mode-command-refusal chosen)))
        (if refusal
            (message "~a" refusal)
            (funcall (find-command chosen)))))))
