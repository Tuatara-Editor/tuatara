;;;; src/runtime/settings.lisp --- Settings, and the values each takes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A setting says once, where it is defined, which values it takes, in a
;;;; small language of descriptions. Help shows them in words, the manual's
;;;; tables list them, and the health check finds a value outside them.

(in-package #:tuatara.runtime)

(defun values-kind (description)
  "The kind of DESCRIPTION, a description of a setting's values, as a keyword:
:PHRASE for words, else the name of the symbol or of the list's first symbol
\(:BOOLEAN, :MEMBER, :INTEGER, :LIST-OF ...). Symbols count by their names, so
a description reads the same in any Lisp package."
  (cond ((stringp description) :phrase)
        ((and description (symbolp description)) (intern (symbol-name description) :keyword))
        ((and (consp description) (symbolp (first description)))
         (intern (symbol-name (first description)) :keyword))
        (t (error "~s describes no values" description))))

(defun check-values-description (description)
  "Signal an error unless DESCRIPTION is a description of a setting's values
that VALUES-WORDS and VALUE-FITS understand: words (a string); BOOLEAN,
NUMBER, INTEGER, STRING, FILE, DIRECTORY or FUNCTION; (MEMBER VALUE ...);
\(INTEGER LOW HIGH) or (NUMBER LOW HIGH), either end a number or * for open;
\(LIST-OF DESCRIPTION); (OR DESCRIPTION ...)."
  (flet ((bad () (error "~s describes no values a setting takes" description)))
    (let ((kind (values-kind description)))
      (if (atom description)
          (unless (member kind '(:phrase :boolean :number :integer :string :file :directory :function)) (bad))
          (case kind
            (:member (unless (rest description) (bad)))
            ((:integer :number)
             (unless (and (= (length description) 3)
                          (every (lambda (end) (or (eq end '*) (realp end))) (rest description)))
               (bad)))
            (:list-of (if (= (length description) 2) (check-values-description (second description)) (bad)))
            (:or (if (rest description) (mapc #'check-values-description (rest description)) (bad)))
            (t (bad))))))
  description)

(defmacro define-setting (name value documentation &key values)
  "Define NAME, a setting an init file sets, with VALUE unless it has a value
already, DOCUMENTATION saying what it changes and what each of its values does,
and VALUES the values it takes, a description in words for people
\(SETTING-VALUES-WORDS) and checked by \\[check-health] (SETTING-VALUE-FAULT):
  boolean                t or nil
  (member :a :b nil)     one of those
  (integer 0 *)          a whole number, 0 or more; * leaves an end open
  (number 0.5 10)        any number in that range; plain number for any
  string                 text
  file, directory           the name of a file or of a directory
  function               a function, or the name of one
  (list-of DESCRIPTION)  a list, each element as DESCRIPTION says
  (or DESCRIPTION ...)   any of them, as (or (member nil) string)
  \"a list of (NAME . PATTERN)\"  words, for anything else; never checked
Its name is exported from TUATARA-USER, where an init file names it."
  (unless (stringp documentation)
    (error "The setting ~s has no documentation" name))
  (when values (check-values-description values))
  `(progn
     (defvar ,name ,value ,documentation)
     (setf (get ',name 'setting-values) ',values)
     ',name))

(defun setting-values (setting)
  "The description of the values SETTING, a symbol, takes, as its
DEFINE-SETTING gave it; NIL when it gave none."
  (get setting 'setting-values))

(defun range-words (noun low high)
  "NOUN, \"a whole number\" or \"a number\", from LOW to HIGH in words, each
end a number or * for none."
  (let ((low (and (realp low) low)) (high (and (realp high) high)))
    (cond ((and low high) (format nil "~a from ~a to ~a" noun low high))
          (low (format nil "~a, ~a or more" noun low))
          (high (format nil "~a, ~a or less" noun high))
          (t noun))))

(defun values-words (description)
  "DESCRIPTION, of the values a setting takes, in words for people:
\"t, nil\", \":absolute, :relative, nil\", \"a whole number, 0 or more\",
\"text\", \"nil, or text\"."
  (let ((*print-case* :downcase) (*print-pretty* nil) (*package* (find-package '#:tuatara-user)))
    (ecase (values-kind description)
      (:phrase description)
      (:boolean "t, nil")
      (:string "text")
      (:file "a file's name")
      (:directory "a directory's name")
      (:function "a function")
      (:member (format nil "~{~s~^, ~}" (rest description)))
      (:integer (if (atom description) "a whole number" (apply #'range-words "a whole number" (rest description))))
      (:number (if (atom description) "a number" (apply #'range-words "a number" (rest description))))
      (:list-of (format nil "a list, each ~a" (values-words (second description))))
      (:or (format nil "~{~a~^, or ~}" (mapcar #'values-words (rest description)))))))

(defun value-fits (value description)
  "Whether VALUE is one of the values DESCRIPTION describes: :YES, :NO, or
:UNKNOWN where words decide it, which are not checked."
  (flet ((yes-if (test) (if test :yes :no))
         (in-range (low high)
           (and (or (not (realp low)) (>= value low)) (or (not (realp high)) (<= value high)))))
    (ecase (values-kind description)
      (:phrase :unknown)
      (:boolean (yes-if (member value '(t nil))))
      (:string (yes-if (stringp value)))
      ((:file :directory) (yes-if (or (stringp value) (pathnamep value))))
      (:function (yes-if (or (functionp value) (and value (symbolp value)))))
      (:member (yes-if (member value (rest description) :test #'equalp)))
      (:integer (yes-if (and (integerp value) (or (atom description) (apply #'in-range (rest description))))))
      (:number (yes-if (and (realp value) (or (atom description) (apply #'in-range (rest description))))))
      (:list-of (if (and (listp value) (handler-case (list-length value) (type-error () nil)))
                    (let ((fits (mapcar (lambda (element) (value-fits element (second description))) value)))
                      (cond ((member :no fits) :no)
                            ((member :unknown fits) :unknown)
                            (t :yes)))
                    :no))
      (:or (let ((fits (mapcar (lambda (each) (value-fits value each)) (rest description))))
             (cond ((member :yes fits) :yes)
                   ((member :unknown fits) :unknown)
                   (t :no)))))))

(defun setting-values-words (setting)
  "The values SETTING, a symbol, takes, in words for people (VALUES-WORDS);
NIL when its definition does not say."
  (let ((description (setting-values setting)))
    (and description (values-words description))))

(defun setting-value-fault (setting)
  "What is wrong with the value SETTING, a symbol, holds, in words naming the
setting, its value and the values it takes; NIL when the value is one of those
its definition declares, or when they are not declared or are given in words,
which are not checked."
  (let ((description (setting-values setting)))
    (when (and description (boundp setting) (eq :no (value-fits (symbol-value setting) description)))
      (let* ((*print-case* :downcase) (*print-pretty* nil) (*print-length* 8) (*print-level* 3)
             (*package* (find-package '#:tuatara-user))
             (value (prin1-to-string (symbol-value setting))))
        (format nil "~a is ~a, but it takes ~a"
                (string-downcase (symbol-name setting))
                (if (> (length value) 60) (format nil "~a..." (subseq value 0 60)) value)
                (values-words description))))))
