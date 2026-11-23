;;;; src/command/command.lisp --- Defining and finding commands.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defvar *commands* (make-hash-table :test 'eq)
  "All commands, keyed by symbol.")

(defvar *key-only-commands* (make-hash-table :test 'eq)
  "The commands made with :INTERACTIVE NIL, keyed by symbol.")

(defun key-only-command-p (name)
  "True when the command NAME is made for a key to run, not to be named: it
is not offered by \\[execute-extended-command] and is outside the stable surface.
An alias of such a command (DEFINE-COMMAND-ALIAS) is key-only too."
  (or (gethash name *key-only-commands*)
      (let ((target (get name 'alias-of)))
        (and target (not (eq target name)) (key-only-command-p (or (command-symbol target) target))))))

(defmacro define-command (name (&key (interactive t)) doc &body body)
  "Define NAME as an interactive command with docstring DOC (required) whose
BODY runs with the editor context bound: (current-buffer), (current-window),
*editor*. Commands take no arguments; they read what they need from the context
and the prefix argument. Registered for key bindings,
\\[execute-extended-command] and help. With :INTERACTIVE NIL the command is for
keys alone: \\[execute-extended-command] does not offer it. NAME is no
longer marked as made while the editor runs (INSTALL-COMMAND)."
  (check-type doc string "a docstring; every command documents itself")
  `(progn
     (defun ,name () ,doc ,@body)
     (setf (gethash ',name *commands*) #',name)
     (remprop ',name 'made-while-running)
     ,(if interactive
          `(remhash ',name *key-only-commands*)
          `(setf (gethash ',name *key-only-commands*) t))
     ',name))

(defun install-command (name function &optional documentation)
  "Make NAME a command that runs FUNCTION, as DEFINE-COMMAND makes one, for a
command made while the editor runs (a name given to recorded keys, a key of an
input method, a package's command before the package loads); DOCUMENTATION,
when given, is its docstring. NAME's property MADE-WHILE-RUNNING is true
after, telling it from a command a source file defines. Returns NAME."
  (setf (symbol-function name) function
        (gethash name *commands*) function
        (get name 'made-while-running) t)
  (when documentation
    (setf (documentation name 'function) documentation))
  name)

(defun define-command-alias (name command &key obsolete)
  "Make NAME another name for the command COMMAND: running NAME runs COMMAND,
which may be defined later (a package's command before the package loads).
Both are symbols; COMMAND is matched by name, so a symbol read in an init file
names the command whatever its package. NAME is offered by
\\[execute-extended-command], keys can be bound to it, and its help says it is
another name for COMMAND. An alias of a key-only command is key-only. Quiet
unless OBSOLETE is a version string, as \"1.0\": then NAME is a rename kept until
that version, its help says so, and the first time it runs in a session it says
which name to use. Refused when COMMAND is NAME, or an alias leading back to
it. Returns NAME."
  (loop for target = command then (get target 'alias-of)
        while target
        do (setf target (or (command-symbol target) target))
           (when (string-equal (symbol-name target) (symbol-name name))
             (refuse "~(~a~) cannot be another name for ~(~a~), which leads back to it" name command)))
  (let ((told nil))
    (install-command
     name
     (lambda ()
       (when (and obsolete (not told))
         (setf told t)
         (message "~(~a~) is now ~(~a~); use that name" name command))
       (let ((function (find-command (or (command-symbol command) command))))
         (if function
             (funcall function)
             (message "~(~a~) is another name for ~(~a~), which is not a command" name command))))
     (if obsolete
         (format nil "Obsolete: the old name of ~(~a~), kept until Tuatara ~a. Runs it, saying so the first time.~
                      ~%~%See ~(~a~)."
                 command obsolete command)
         (format nil "Another name for ~(~a~); runs it.~%~%See ~(~a~)." command command)))
    (setf (get name 'alias-of) command)
    name))

(defvar *command-names* (cons -1 nil)
  "The commands by name, as COMMAND-SYMBOL finds them: (COUNT . TABLE), TABLE
from each command's name to its symbol, made when *COMMANDS* held COUNT.")

(defun find-command (name)
  "The command function for NAME (a symbol or a string naming one), or NIL."
  (let ((symbol (if (symbolp name) name (command-symbol name))))
    (and symbol (gethash symbol *commands*))))

(defun command-symbol (name)
  "The registered symbol of the command called NAME (a symbol from any package,
or a string, in any case), matched by name; NIL when there is no such
command. Complexity: O(1), once a table of the names is made after commands
are defined."
  (let ((kept *command-names*))
    (unless (eql (car kept) (hash-table-count *commands*))
      (let ((table (make-hash-table :test 'equal)))
        (loop for s being the hash-keys of *commands*
              do (unless (gethash (string-upcase (symbol-name s)) table)
                   (setf (gethash (string-upcase (symbol-name s)) table) s)))
        (setf kept (cons (hash-table-count *commands*) table)
              *command-names* kept)))
    (values (gethash (string-upcase (if (symbolp name) (symbol-name name) name)) (cdr kept)))))

(defun command-names ()
  "All command names as downcased strings, sorted, each once (two packages'
commands of one name are one command by name)."
  (sort (remove-duplicates (loop for s being the hash-keys of *commands* collect (string-downcase (symbol-name s)))
                           :test #'string=)
        #'string<))

(defun forget-command (name)
  "Make the symbol NAME no longer a command: keys bound to it, \\[execute-extended-command]
and help no longer find it, nor is it marked as made while the editor runs
\(INSTALL-COMMAND). Its function stays."
  (remprop name 'made-while-running)
  (when (remhash name *commands*)
    (setf *command-names* (cons -1 nil))))
