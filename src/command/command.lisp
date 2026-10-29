;;;; src/command/command.lisp --- Defining and finding commands.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defvar *commands* (make-hash-table :test 'eq)
  "All commands, keyed by symbol.")

(defmacro define-command (name (&key) doc &body body)
  "Define NAME as an interactive command with docstring DOC (required) whose BODY
runs with the editor context bound: (current-buffer), (current-window),
*editor*. Commands take no arguments; they read what they need from the
context and the prefix argument. Registered for key bindings, \\[execute-extended-command] and help."
  (check-type doc string "a docstring; every command documents itself")
  `(progn
     (defun ,name () ,doc ,@body)
     (setf (gethash ',name *commands*) #',name)
     ',name))

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
and help no longer find it. Its function stays."
  (when (remhash name *commands*)
    (setf *command-names* (cons -1 nil))))
