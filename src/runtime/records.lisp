;;;; src/runtime/records.lisp --- Structures whose slots carry their own documentation.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A record is a structure written with DEFINE-RECORD instead of DEFSTRUCT:
;;;; each slot says what it holds where it is written, and that text becomes
;;;; the documentation of the slot's reader, which help shows for it. A slot
;;;; without it is refused when the record is compiled.

(in-package #:tuatara.runtime)

(defun record-option-key (option)
  "The keyword naming OPTION, one of a structure's options as DEFSTRUCT takes
them: the option itself when it is a keyword alone, else its first element."
  (if (consp option) (first option) option))

(defun record-option (options key)
  "The first of OPTIONS whose keyword is KEY, or NIL."
  (find key options :key #'record-option-key))

(defun record-conc-name (name options)
  "The prefix of the slot readers of the structure NAME with OPTIONS, a string:
NAME and a hyphen, the one :CONC-NAME gives, or \"\" for :CONC-NAME NIL."
  (let ((option (record-option options :conc-name)))
    (cond ((null option) (format nil "~a-" (symbol-name name)))
          ((or (atom option) (null (second option))) "")
          (t (string (second option))))))

(defun record-default-name (option default)
  "The name OPTION, a :PREDICATE, :COPIER or :CONSTRUCTOR option or NIL for none
given, makes: DEFAULT when it is absent or names none, NIL for NIL, else the
name it gives."
  (cond ((or (null option) (atom option) (null (rest option))) default)
        (t (second option))))

(defun record-article (name)
  "\"an\" when the name of the symbol NAME begins with a vowel, else \"a\"."
  (if (find (char (symbol-name name) 0) "AEIOU") "an" "a"))

(defun record-slot-documentation (record slot)
  "The documentation SLOT, a slot as DEFINE-RECORD takes it, gives its reader.
Signals an error naming the slot and RECORD when it gives none, so that a
record compiles only with every slot documented."
  (let ((documentation (and (consp slot) (consp (rest slot)) (getf (cddr slot) :documentation))))
    (unless (stringp documentation)
      (error "The slot ~s of the record ~s has no documentation: write it as (~(~a~) INITIAL-VALUE :DOCUMENTATION \"...\")"
             (if (consp slot) (first slot) slot) record (if (consp slot) (first slot) slot)))
    documentation))

(defun record-slot-without-documentation (slot)
  "SLOT, a slot as DEFINE-RECORD takes it, as DEFSTRUCT takes it: its
:DOCUMENTATION left out."
  (destructuring-bind (name initial-value &rest options) slot
    `(,name ,initial-value ,@(loop for (key value) on options by #'cddr
                                   unless (eq key :documentation) append (list key value)))))

(defun record-inherited-readers (parent conc-name)
  "For each slot of the structure PARENT, as (READER PARENT-READER): the reader
a structure including PARENT with CONC-NAME, a string, has for it, and
PARENT's own. A slot whose reader is PARENT's own is left out."
  (let ((description (sb-kernel:find-defstruct-description parent nil)))
    (unless description
      (error "The record includes ~s, which is no structure defined before it" parent))
    (loop for slot in (sb-kernel:dd-slots description)
          for parent-reader = (sb-kernel:dsd-accessor-name slot)
          for reader = (intern (format nil "~a~a" conc-name (symbol-name (sb-kernel:dsd-name slot))))
          when (and parent-reader (not (eq reader parent-reader)))
            collect (list reader parent-reader))))

(defmacro define-record (name-and-options documentation &rest slots)
  "Define a structure as DEFSTRUCT does, with every slot documented where it is
written. NAME-AND-OPTIONS and DOCUMENTATION are DEFSTRUCT's, the
documentation required. Each slot is a list, (NAME INITIAL-VALUE :TYPE TYPE
:READ-ONLY READ-ONLY :DOCUMENTATION TEXT), :TYPE and :READ-ONLY optional and
TEXT required: it becomes the documentation of the slot's reader, so that
\(DOCUMENTATION 'READER 'FUNCTION) and help give it. A slot without it is an
error when the record is compiled, naming the slot.

The predicate, copier and constructors are documented as what they do, the
slots they fill pointing to the readers; a SETF of DOCUMENTATION after the
record says more of one. The readers of the slots taken from a structure
:INCLUDE names have that structure's documentation of them. Returns the
name."
  (let* ((name (if (consp name-and-options) (first name-and-options) name-and-options))
         (options (if (consp name-and-options) (rest name-and-options) '()))
         (conc-name (record-conc-name name options))
         (typed (and (record-option options :type) (not (record-option options :named))))
         (predicate (record-default-name (record-option options :predicate)
                                         (and (not typed) (symbolicate name '#:-p))))
         (copier (record-default-name (record-option options :copier) (symbolicate '#:copy- name)))
         (constructors (or (loop for option in options
                                 when (eq (record-option-key option) :constructor)
                                   collect (record-default-name option (symbolicate '#:make- name)))
                           (list (symbolicate '#:make- name))))
         (include (record-option options :include))
         (thing (format nil "~a ~a" (record-article name) (symbol-name name))))
    (unless (stringp documentation)
      (error "The record ~s has no documentation: it comes after the name and options" name))
    `(progn
       (defstruct ,name-and-options
         ,documentation
         ,@(mapcar #'record-slot-without-documentation
                   (loop for slot in slots do (record-slot-documentation name slot) collect slot)))
       (setf ,@(loop for slot in slots
                     append `((documentation ',(intern (format nil "~a~a" conc-name (symbol-name (first slot))))
                                             'function)
                              ,(record-slot-documentation name slot)))
             ,@(loop for (reader parent-reader) in (and include (record-inherited-readers (second include) conc-name))
                     append `((documentation ',reader 'function) (documentation ',parent-reader 'function)))
             ,@(when predicate
                 `((documentation ',predicate 'function) ,(format nil "True when OBJECT is ~a." thing)))
             ,@(when copier
                 `((documentation ',copier 'function)
                   ,(format nil "A new ~a holding what the one given holds, slot for slot." (symbol-name name))))
             ,@(loop for constructor in (remove nil constructors)
                     append `((documentation ',constructor 'function)
                              ,(format nil "Make ~a, each argument the slot of its name, whose reader's ~
documentation says what it holds." thing))))
       ',name)))
