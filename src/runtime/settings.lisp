;;;; src/runtime/settings.lisp --- Settings, and the values each takes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A setting says once, where it is defined, which values it takes, in a
;;;; small language of descriptions. Help shows them in words, the manual's
;;;; tables list them, and the health check finds a value outside them. The
;;;; editor's own ways of setting one (SET-SETTING) refuse a value outside
;;;; them; a value set otherwise, by a form of the init file, a package's
;;;; :init or :config or an evaluation, is looked at once the form has run
;;;; and the setting put back (PUT-BACK-MISFIT-SETTINGS); one given before
;;;; the setting was defined is looked at when it is (NOTE-SETTING).
;;;;
;;;; A setting holding octets or a time may be given it in words, as "20G"
;;;; or "90s": the same three places turn the words into the number the
;;;; setting holds (CONVERTED-SETTING-VALUE), so that the code reading a
;;;; setting only ever finds a number there.

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
NUMBER, INTEGER, OCTETS, SECONDS, MINUTES, STRING, CHARACTER, SYMBOL, KEYWORD,
LIST, FILE, DIRECTORY or FUNCTION; (MEMBER VALUE ...); (INTEGER LOW HIGH),
\(NUMBER LOW HIGH) or (OCTETS LOW HIGH), either end a number or * for open;
\(SECONDS LOW HIGH) or (MINUTES LOW HIGH), with :WHOLE after them for whole
numbers alone; (STRING LENGTH); (CONS DESCRIPTION DESCRIPTION); (LIST-OF
DESCRIPTION); (OR DESCRIPTION ...)."
  (flet ((bad () (error "~s describes no values a setting takes" description))
         (ends-p (ends) (every (lambda (end) (or (eq end '*) (realp end))) ends)))
    (let ((kind (values-kind description)))
      (if (atom description)
          (unless (member kind '(:phrase :boolean :number :integer :octets :seconds :minutes :string :character
                                 :symbol :keyword :list :file :directory :function))
            (bad))
          (case kind
            (:member (unless (rest description) (bad)))
            ((:integer :number :octets)
             (unless (and (= (length description) 3) (ends-p (rest description)))
               (bad)))
            ((:seconds :minutes)
             (unless (and (<= 3 (length description) 4) (ends-p (subseq description 1 3))
                          (or (= (length description) 3) (whole-time-p description)))
               (bad)))
            (:string (unless (and (= (length description) 2) (typep (second description) '(integer 1))) (bad)))
            (:cons (if (= (length description) 3) (mapc #'check-values-description (rest description)) (bad)))
            (:list-of (if (= (length description) 2) (check-values-description (second description)) (bad)))
            (:or (if (rest description) (mapc #'check-values-description (rest description)) (bad)))
            (t (bad))))))
  description)

(sb-ext:defglobal **no-value** (make-symbol "NO-VALUE")
  "What SETTING-VALUE-NOW gives for a setting that has no value.")

(declaim (inline setting-value-now))
(defun setting-value-now (setting)
  "The value SETTING, a symbol, holds, or **NO-VALUE** when it holds none."
  (if (boundp setting) (symbol-value setting) **no-value**))

(defvar *settings* (make-array 400 :adjustable t :fill-pointer 0)
  "Every setting defined (DEFINE-SETTING), a symbol each, in the order they
were defined.")

(defun setting-values (setting)
  "The description of the values SETTING, a symbol, takes, as its
DEFINE-SETTING gave it; NIL when it gave none."
  (get setting 'setting-values))

(defparameter *largest-size* (expt 2 62)
  "The most octets a setting holding a size takes (OCTETS), 4 EiB: more than
any drive or memory holds, so that a size mistyped is refused.")

(defparameter *longest-time* (* 100 365 24 60 60)
  "The most seconds a setting holding a time takes (SECONDS, and the same in
minutes for MINUTES), a hundred years, so that a time mistyped is refused.")

(defun whole-time-p (description)
  "True when DESCRIPTION, (SECONDS LOW HIGH :WHOLE) or (MINUTES LOW HIGH
:WHOLE), takes whole numbers alone."
  (and (consp description) (= (length description) 4) (string= (symbol-name (fourth description)) "WHOLE")))

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
      (:string (if (atom description) "text" (format nil "text of exactly ~r character~:p" (second description))))
      (:character "a character")
      (:symbol "a symbol")
      (:keyword "a keyword")
      (:list "a list")
      (:cons (format nil "a pair (A . B), A ~a and B ~a"
                     (values-words (second description)) (values-words (third description))))
      (:file "a file's name")
      (:directory "a directory's name")
      (:function "a function")
      (:member (format nil "~{~s~^, ~}" (rest description)))
      (:integer (if (atom description) "a whole number" (apply #'range-words "a whole number" (rest description))))
      (:number (if (atom description) "a number" (apply #'range-words "a number" (rest description))))
      (:octets (format nil "~a, or a size in words such as 512M, 1.5G or 20 GB (in 1024s: 1K is 1024 octets), ~
                            at most 4E"
                       (if (atom description)
                           "a whole number of octets"
                           (range-words "a whole number of octets" (second description) (third description)))))
      ((:seconds :minutes)
       (let ((noun (format nil "~:[a number~;a whole number~] of ~(~a~)"
                           (whole-time-p description) (values-kind description))))
         (format nil "~a, or a time in words such as 500ms, 90s, 30m or 2h, at most a hundred years"
                 (if (atom description) noun (range-words noun (second description) (third description))))))
      (:list-of (format nil "a list, each ~a" (values-words (second description))))
      (:or (format nil "~{~a~^, or ~}" (mapcar #'values-words (rest description)))))))

(defun worse-fit (fit other)
  "The worse of FIT and OTHER, each an answer of VALUE-FITS: :NO before
:UNKNOWN before :YES."
  (cond ((or (eq fit :no) (eq other :no)) :no)
        ((or (eq fit :unknown) (eq other :unknown)) :unknown)
        (t :yes)))

(defun value-fits (value description)
  "Whether VALUE is one of the values DESCRIPTION describes: :YES, :NO, or
:UNKNOWN where words decide it, which are not checked. Makes nothing, so
that a setting may be checked each time it is given a value."
  (flet ((yes-if (test) (if test :yes :no))
         (in-range (low high)
           (and (or (not (realp low)) (>= value low)) (or (not (realp high)) (<= value high))))
         (whole-list-p (value) (and (listp value) (handler-case (list-length value) (type-error () nil)))))
    (ecase (values-kind description)
      (:phrase :unknown)
      (:boolean (yes-if (member value '(t nil))))
      (:string (yes-if (and (stringp value) (or (atom description) (= (length value) (second description))))))
      (:character (yes-if (characterp value)))
      (:symbol (yes-if (symbolp value)))
      (:keyword (yes-if (keywordp value)))
      (:list (yes-if (whole-list-p value)))
      (:cons (if (consp value)
                 (worse-fit (value-fits (car value) (second description))
                            (value-fits (cdr value) (third description)))
                 :no))
      ((:file :directory) (yes-if (or (stringp value) (pathnamep value))))
      (:function (yes-if (or (functionp value) (and value (symbolp value)))))
      (:member (yes-if (member value (rest description) :test #'equalp)))
      (:integer (yes-if (and (integerp value)
                             (or (atom description) (in-range (second description) (third description))))))
      (:number (yes-if (and (realp value)
                            (or (atom description) (in-range (second description) (third description))))))
      ((:seconds :minutes)
       (yes-if (and (if (whole-time-p description) (integerp value) (realp value))
                    (<= value (if (eq (values-kind description) :minutes) (/ *longest-time* 60) *longest-time*))
                    (or (atom description) (in-range (second description) (third description))))))
      (:octets (yes-if (and (integerp value) (<= value *largest-size*)
                            (or (atom description) (in-range (second description) (third description))))))
      (:list-of (if (whole-list-p value)
                    (loop with fit = :yes
                          for element in value
                          do (setf fit (worse-fit fit (value-fits element (second description))))
                          until (eq fit :no)
                          finally (return fit))
                    :no))
      (:or (loop with fit = :no
                 for each in (rest description)
                 for each-fit = (value-fits value each)
                 when (eq each-fit :yes)
                   return :yes
                 when (eq each-fit :unknown)
                   do (setf fit :unknown)
                 finally (return fit))))))

(defparameter *size-units* "KMGTPE"
  "The letters of the units a size in words may have after its number, each
1024 times the one before it, K being 1024 octets.")

(defun octets-in-words (words)
  "The whole number of octets WORDS, a string, gives: a number, perhaps with
a fraction, then perhaps a unit of *SIZE-UNITS*, with or without B or iB
after it, any letter in either case and spaces around: \"20G\", \"1.5 GB\",
\"512MiB\", \"300\". Units count in 1024s, whichever is written: 1 GB is
1073741824 octets. A fraction of an octet left is rounded to the nearest. NIL
when WORDS is not such a size."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^\\s*([0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)\\s*(?:([KMGTPEkmgtpe])(?:[iI]?[bB])?|[bB])?\\s*$"
                                words)
    (when match
      (let ((unit (aref groups 1)))
        (round (* (decimal-in-words (aref groups 0))
                  (if unit (expt 1024 (1+ (position (char-upcase (char unit 0)) *size-units*))) 1)))))))

(defun decimal-in-words (digits)
  "The exact rational number DIGITS, a string of digits with perhaps one
point among them (\"1.5\", \".25\", \"20\"), writes."
  (let* ((point (position #\. digits))
         (whole (subseq digits 0 point))
         (fraction (if point (subseq digits (1+ point)) "")))
    (+ (if (plusp (length whole)) (parse-integer whole) 0)
       (if (plusp (length fraction)) (/ (parse-integer fraction) (expt 10 (length fraction))) 0))))

(defparameter *time-units* '(("ms" . 1/1000) ("s" . 1) ("sec" . 1) ("m" . 60) ("min" . 60) ("h" . 3600)
                             ("d" . 86400))
  "The units a time in words may have after its number, each with the seconds
it stands for: milliseconds, seconds, minutes, hours and days.")

(defun seconds-in-words (words)
  "The seconds WORDS, a string, gives, as an exact rational number: a number,
perhaps with a fraction, then perhaps a unit of *TIME-UNITS*, in either case,
spaces allowed around: \"500ms\", \"90s\", \"1.5h\", \"30 m\", \"14d\". A
second value is true when WORDS has a unit. NIL when WORDS is not such a time."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^\\s*([0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)\\s*([A-Za-z]*)\\s*$" words)
    (when match
      (let* ((unit (aref groups 1))
             (scale (if (zerop (length unit)) 1 (cdr (assoc unit *time-units* :test #'string-equal)))))
        (and scale (values (* (decimal-in-words (aref groups 0)) scale) (plusp (length unit))))))))

(defun plain-number (number)
  "NUMBER, a rational, as a whole number when it is one, else as a float, as
an init file would write it: 3/2 as 1.5."
  (if (integerp number) number (float number 1.0)))

(defun words-number (description value)
  "The number VALUE, a size or a time in words, gives in the unit of
DESCRIPTION, an OCTETS, SECONDS or MINUTES description or an OR of them
\(CONVERTED-SETTING-VALUE), whether or not it fits it; VALUE itself when it
gives none."
  (case (values-kind description)
    (:octets (or (octets-in-words value) value))
    ((:seconds :minutes)
     (multiple-value-bind (seconds unit) (seconds-in-words value)
       (cond ((null seconds) value)
             ((and (eq (values-kind description) :minutes) unit) (plain-number (/ seconds 60)))
             (t (plain-number seconds)))))
    (:or (loop for each in (rest description)
               for converted = (converted-setting-value each value)
               unless (eq converted value)
                 return converted
               finally (return value)))
    (t value)))

(defun converted-setting-value (description value)
  "VALUE as a setting DESCRIPTION describes holds it: a string giving a size
in words (OCTETS-IN-WORDS) for an OCTETS description, or a time in words
\(SECONDS-IN-WORDS) for a SECONDS or MINUTES one, turned into the number of
octets, seconds or minutes it gives, when that number is one DESCRIPTION
takes; any other VALUE, and words giving none it takes, as they are, for the
fault to name them. A number without a unit counts in the setting's own unit:
\"5\" is 5 minutes for a MINUTES one. Makes nothing unless VALUE is a string."
  (if (not (and (stringp value) description (not (stringp description))
                (not (eq :yes (value-fits value description)))))
      value
      (let ((converted (words-number description value)))
        (if (eq :yes (value-fits converted description)) converted value))))

(defun convert-setting (setting)
  "Give SETTING, a symbol, the number its value in words gives, when it holds
words for a size or a time it takes (CONVERTED-SETTING-VALUE). Returns its
value."
  (let ((value (setting-value-now setting)))
    (if (stringp value)
        (setf (symbol-value setting) (converted-setting-value (setting-values setting) value))
        value)))

(defun setting-values-words (setting)
  "The values SETTING, a symbol, takes, in words for people (VALUES-WORDS);
NIL when its definition does not say."
  (let ((description (setting-values setting)))
    (and description (values-words description))))

(defun shown-value (value)
  "VALUE as Lisp writes it, as an init file would, cut short past 60 characters."
  (let* ((*print-case* :downcase) (*print-pretty* nil) (*print-length* 8) (*print-level* 3)
         (*package* (find-package '#:tuatara-user))
         (text (prin1-to-string value)))
    (if (> (length text) 60) (format nil "~a..." (subseq text 0 60)) text)))

(defun setting-value-fault (setting &optional (value (setting-value-now setting)))
  "What is wrong with VALUE, the value SETTING, a symbol, holds unless another
is given, in words naming the setting, the value and the values it takes; NIL
when the value is one of those its definition declares, or when they are given
in words, which are not checked."
  (let ((description (setting-values setting)))
    (when (and description (not (eq value **no-value**)) (eq :no (value-fits value description)))
      (format nil "~(~a~) is ~a, but it takes ~a"
              (symbol-name setting) (shown-value value) (values-words description)))))

(defun setting-standard-value (setting)
  "The value SETTING, a symbol, has when nothing sets it: the one its
DEFINE-SETTING gives, worked out again."
  (funcall (get setting 'setting-standard)))

(defun setting-in-force (setting)
  "The value SETTING, a symbol, holds when it is one the setting takes, else
its standard value (SETTING-STANDARD-VALUE): for code that reads a setting
on each key or each paint, which a value given where it was not looked at
\(Lisp a package runs on its own, a LET) must not stop. Makes nothing when
the value fits. Complexity: O(size of the value)."
  (let ((value (symbol-value setting)) (description (setting-values setting)))
    (if (and description (eq :no (value-fits value description)))
        (setting-standard-value setting)
        value)))

(defun set-setting (setting value)
  "Give SETTING, a symbol, VALUE when it is one of the values its definition
declares (SETTING-VALUE-FAULT); else refuse, saying what it takes, and keep
the value it has. The way the editor's own commands set a setting. Returns
VALUE, or the number it gives when it is a size or a time in words
\(CONVERTED-SETTING-VALUE)."
  (let* ((value (converted-setting-value (setting-values setting) value))
         (fault (setting-value-fault setting value)))
    (when fault
      (if (boundp setting)
          (refuse "~a: it keeps ~a" fault (shown-value (symbol-value setting)))
          (refuse "~a: it has no value, and is given none" fault)))
    (setf (symbol-value setting) value)))

(defun note-setting (name standard)
  "Count NAME among *SETTINGS*, once, STANDARD a function of no arguments
giving its standard value (SETTING-STANDARD-VALUE). A value NAME was given
before it was a setting, by an init file that names a package's setting
before the package loads, is kept when it is one the setting takes, a size
or a time in words turned into its number (CONVERT-SETTING); else
the standard value is given in its place, and that said
\(HOST-SAY-PROBLEM)."
  (setf (get name 'setting-standard) standard)
  (unless (get name 'setting-index)
    (setf (get name 'setting-index) (vector-push-extend name *settings*)))
  (convert-setting name)
  (let ((fault (setting-value-fault name)))
    (when fault
      (setf (symbol-value name) (funcall standard))
      (let ((words (format nil "~a: it is given its standard value, ~a" fault (shown-value (symbol-value name)))))
        (debug-report :error "~a" words)
        (host-say-problem *host* words))))
  name)

(defmacro define-setting (name value documentation &key values)
  "Define NAME, a setting an init file sets, with VALUE unless it has a value
already, DOCUMENTATION saying what it changes and what each of its values does,
and VALUES the values it takes, a description in words for people
\(SETTING-VALUES-WORDS), checked by \\[check-health] (SETTING-VALUE-FAULT),
whenever the editor sets it (SET-SETTING), after each form of the init file,
of a package's :init and :config and of each evaluation
\(PUT-BACK-MISFIT-SETTINGS), and when it is defined (NOTE-SETTING). A value
it does not take is refused, in words naming the setting and what it takes:
the setting keeps the value it had, or, when it is defined, takes its
standard value. Reading it costs nothing more. The
descriptions:
  boolean                t or nil
  (member :a :b nil)     one of those
  (integer 0 *)          a whole number, 0 or more; * leaves an end open
  (number 0.5 10)        any number in that range; plain number for any
  (octets 1 *)           a whole number of octets in that range, which may be
                         given as a size in words, \"20G\", \"1.5 GB\", \"512MiB\",
                         units in 1024s whichever is written
  (seconds 0 *)          a number of seconds, which may be given as a time in
                         words, \"500ms\", \"90s\", \"30m\", \"2h\", \"14d\"
  (minutes 0 * :whole)   the same in minutes; :WHOLE takes whole numbers alone
                         (\"90s\", 1.5 minutes, is then refused)
  string                 text
  (string 2)             text of exactly two characters
  (cons CAR CDR)         a pair, (CAR . CDR), each as its description says
  character, symbol, keyword, list    one of those
  file, directory        the name of a file or of a directory
  function               a function, or the name of one
  (list-of DESCRIPTION)  a list, each element as DESCRIPTION says
  (or DESCRIPTION ...)   any of them, as (or (member nil) string)
  \"a list of (NAME . PATTERN)\"  words, for anything else; never checked
Words are best kept within a description, as (list-of \"(NAME . PATTERN)\"),
so that the rest of it is checked. A setting without VALUES, or with words
alone, takes any value. Its name is exported from TUATARA-USER, where an init
file names it."
  (unless (stringp documentation)
    (error "The setting ~s has no documentation" name))
  (when values (check-values-description values))
  `(progn
     (defvar ,name ,value ,documentation)
     (setf (get ',name 'setting-values) ',values)
     (note-setting ',name (lambda () ,value))))

(defun settings-snapshot (&optional before)
  "The value of each of *SETTINGS* now, in its order, for
PUT-BACK-MISFIT-SETTINGS. With BEFORE, an earlier snapshot, a setting holding
a value it does not take is counted as it was in BEFORE, or as its standard
value when it was defined since, so that a later look still finds it.
Complexity: O(settings)."
  (let ((now (map 'simple-vector (lambda (setting) (setting-value-now setting)) *settings*)))
    (when before
      (dotimes (i (length now))
        (let* ((setting (aref *settings* i))
               (description (setting-values setting))
               (value (svref now i)))
          (when (and description (not (eq value **no-value**)) (eq :no (value-fits value description)))
            (setf (svref now i) (if (< i (length before)) (svref before i) (setting-standard-value setting)))))))
    now))

(define-record (settings-watch (:constructor make-settings-watch ()) (:copier nil))
  "What the settings held when SETTINGS-MOVED-P last looked at them for one
holder, an editor say. Each holder keeps a watch of its own and looks from
its own thread, so a value another thread binds for itself is not seen as a
change, and one holder's look never hides a change from another's."
  (seen #() :type simple-vector :documentation "The value of each of *SETTINGS*, in its order, when
the watch last looked."))

(defun settings-moved-p (watch)
  "True when a setting (*SETTINGS*) holds another value than when WATCH last
looked, or WATCH has never looked; WATCH then keeps the values now. Values
are read as the calling thread has them, its own bindings included, however
they were given: the init file, \\[eval-expression], a command. A value
changed in place, as a list edited where it is, is not seen: only a setting
given another value. Nothing is made unless a setting was defined since the
last look. Complexity: O(settings)."
  (let* ((settings *settings*) (count (length settings)) (seen (settings-watch-seen watch)) (moved nil))
    (declare (simple-vector seen) (fixnum count))
    (unless (= count (length seen))
      (setf seen (replace (make-array count :initial-element **no-value**) seen)
            (settings-watch-seen watch) seen
            moved t))
    (dotimes (i count moved)
      (let ((value (setting-value-now (aref settings i))))
        (unless (eql value (svref seen i))
          (setf (svref seen i) value
                moved t))))))

(defun put-back-misfit-settings (snapshot)
  "Put back each setting that was given a value it does not take since
SNAPSHOT (SETTINGS-SNAPSHOT) was made: to the value it had then, or for a
setting defined since, its standard value. A list of what was put back, in
words naming the setting, the value refused, what it takes and the value it
keeps. A size or a time given in words is first turned into the number it
gives (CONVERT-SETTING). Only the settings given another value are looked at closely, so that
it costs next to nothing when none was; a value is another when it is not
the same object (EQ), so a list changed where it stands is not looked at.
Complexity: O(settings)."
  (let ((said '()))
    (loop for i from 0 below (length *settings*)
          for setting = (aref *settings* i)
          for value = (setting-value-now setting)
          unless (and (< i (length snapshot)) (eq value (svref snapshot i)))
            do (let ((fault (setting-value-fault setting (convert-setting setting))))
                 (when fault
                   (let ((was (if (< i (length snapshot)) (svref snapshot i) (setting-standard-value setting))))
                     (if (eq was **no-value**) (makunbound setting) (setf (symbol-value setting) was))
                     (push (if (eq was **no-value**)
                               (format nil "~a: it is left with no value" fault)
                               (format nil "~a: it keeps ~a" fault (shown-value was)))
                           said)))))
    (nreverse said)))
