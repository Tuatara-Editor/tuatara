;;;; lisp/editing/records.lisp --- The fields of a file of records, asked for and tried.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file of records, a line each, cuts its records into fields as its mode
;;;; says (MODE-RECORD-FIELDS): a CSV by its separator, a log and the hosts
;;;; file by spaces, the password file by colons, JSON Lines by its keys,
;;;; todo.txt by its parts. A filter (keep-lines, C-x / f and the others)
;;;; asks for one field here and tries its pattern on that alone.

(in-package #:tuatara.defaults)

(defparameter *record-fields-offered* 256
  "The most fields of a record the question for one offers; one past them is
still taken by its number.")

(defparameter *record-keys-looked-at* 64
  "How many records are read for the names their fields go by (JSON Lines'
keys), to offer them.")

(defun record-ends (buffer)
  "How the records of BUFFER, a file of records, end: NIL when each is a line;
for a CSV or TSV, whose quoted fields may hold line breaks, CSV-RECORD-END, a
function of a text and a record's start giving the end of its last line and
the start of the line after it."
  (and (typep (buffer-major-mode buffer) 'csv-mode) #'csv-record-end))

(defun record-bounds (rope start ends)
  "Two values for the record of ROPE beginning at START, a line's start: the
end of its text and the start of the line after it, its records ending as
ENDS says (RECORD-ENDS), a line each when ENDS is NIL."
  (if ends
      (funcall ends rope start)
      (let ((line-end (tuatara.text:rope-line-end rope start)))
        (values line-end (min (tuatara.text:rope-bytes rope) (1+ line-end))))))

(defun make-record-memo ()
  "What LINE-RECORD keeps of the last record one thread found."
  (vector nil 0 0 0 :unknown))

(defun line-record (memo rope line next ends)
  "The record of ROPE holding its line from LINE to NEXT (the start of the
line after it), records ending as ENDS says (RECORD-ENDS): three values,
where it starts, where the text of its last line ends and where the line
after it starts. MEMO (MAKE-RECORD-MEMO), kept by one thread, holds the last
record found and what was decided of it in its fifth place, so that the
record's other lines, and the record after it, are known at once."
  (declare (type simple-vector memo) (type fixnum line next))
  (unless (and (eq rope (svref memo 0)) (<= (svref memo 1) line) (< line (svref memo 3)))
    (let ((start (if (and (eq rope (svref memo 0)) (= line (svref memo 3))) line (rope-record-start rope line))))
      (multiple-value-bind (text-end after) (funcall ends rope start)
        (setf (svref memo 0) rope (svref memo 1) start
              (svref memo 2) text-end (svref memo 3) (max after next)
              (svref memo 4) :unknown))))
  (values (svref memo 1) (svref memo 2) (svref memo 3)))

(defun record-text-end (rope text-end crlf)
  "TEXT-END, where a record's last line of ROPE ends, before the carriage
return there when its lines end in CRLF."
  (if (and crlf (plusp text-end) (= 13 (tuatara.text:rope-octet rope (1- text-end)))) (1- text-end) text-end))

(defun record-verdict (memo rope start text-end crlf limit holder decide)
  "What DECIDE, a function of a string and how many of its first characters
are the text, says of the record of ROPE from START, the text of its last
line ending at TEXT-END (its line ends CRLF when CRLF is true): its text
read up to LIMIT octets and decoded into the string in HOLDER's car
\(DECODE-INTO), true or NIL. Decided once a record: MEMO, LINE-RECORD's for
that record, keeps it for its other lines."
  (if (eq (svref memo 4) :unknown)
      (setf (svref memo 4)
            (let ((octets (tuatara.text:rope-octets rope :start start :end (min (record-text-end rope text-end crlf) (+ start limit)))))
              (and (funcall decide (car holder) (decode-into octets 0 (length octets) holder)) t)))
      (svref memo 4)))

(defun record-line (buffer start)
  "The text of BUFFER's record from START, a line start, its last line end
left out (RECORD-ENDS: a CSV's record may hold line breaks in quoted fields):
its first megabyte at most, a byte-order mark the file begins with no part of
it."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (end (min (record-bounds rope start (record-ends buffer)) (+ start +csv-scan-limit+)))
         (text (tuatara.text:lenient-string (tuatara.text:rope-octets rope :start start :end end))))
    (if (and (zerop start) (plusp (length text)) (char= (char text 0) (code-char #xfeff)))
        (subseq text 1)
        text)))

(defun first-records (buffer)
  "The text of BUFFER's first records, *RECORD-KEYS-LOOKED-AT* at most, its
header left out (LIST-HEADER-P)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (ends (record-ends buffer)))
    (loop repeat *record-keys-looked-at*
          for at = (records-start rope (if (list-header-p buffer) 1 0)) then (nth-value 1 (record-bounds rope at ends))
          while (< at size)
          collect (record-line buffer at))))

(defun record-field-value (fields text field)
  "The first value of FIELD in the record TEXT holds, cut as FIELDS has it,
as a string; NIL when it has none."
  (funcall (record-fields-each fields) text (length text) field
           (lambda (string start end) (subseq string start end))))

(defun offered-fields (buffer fields)
  "The fields of BUFFER's records, cut as FIELDS has them, as a question
offers them: a list of (NAME FIELD SAMPLE), NAME NIL for one that goes by its
number, SAMPLE the first record's value. Named by FIELDS, else by the header
when the first line is one (LIST-HEADER-P)."
  (let* ((header (list-header-p buffer))
         (top (record-line buffer 0))
         (sample (if header (record-line buffer (records-start (tuatara.buffer:buffer-text buffer) 1)) top))
         (keys (record-fields-keys fields)))
    (flet ((value (text field) (let ((v (record-field-value fields text field))) (and v (plusp (length v)) v))))
      (if keys
          (mapcar (lambda (key) (list key key (value sample key))) (funcall keys buffer))
          (let ((names (record-fields-names fields)))
            (loop for n from 0 below *record-fields-offered*
                  for name = (or (nth n names) (and header (value top n)))
                  while (or (< n (length names)) (value sample n) (and header (value top n)))
                  collect (list name n (value sample n))))))))

(defun answered-field (answer offered keyed)
  "The field ANSWER names among OFFERED (as OFFERED-FIELDS gives them): a
name (in any case when none is written so); for fields that go by their
number, a number, or $ and a number ($0 the first); for KEYED fields, any
other name as typed. NIL for the whole record (an empty answer, \"whole
record\" or $*); :UNKNOWN for anything else."
  (let ((answer (string-trim " " answer)))
    (cond ((or (string= answer "") (string-equal answer "whole record") (string= answer "$*")) nil)
          ((second (find answer offered :key #'first :test #'equal)))
          ((second (find answer offered :key #'first :test (lambda (a b) (and b (string-equal a b))))))
          (keyed answer)
          (t (let ((number (ignore-errors (parse-integer (string-left-trim "$" answer)))))
               (if (and number (>= number 0)) number :unknown))))))

(defun read-offered-field (offered keyed prompt &key (words (lambda (field) (if (integerp field) (format nil "$~d" field) field))))
  "Ask with PROMPT for one of OFFERED, fields as OFFERED-FIELDS gives them,
offering the whole record first, then each by its name, or by its number
from 0 when it has none, the first record's value beside it; with KEYED, any
name typed is taken. Two values: the field, NIL for the whole record; and
what WORDS, a function of the field, names it by (\"$2\"), NIL for the whole
record. An error for no such field."
  (let* ((candidates (cons (make-candidate "whole record" :annotation "the line as it is")
                           (loop for (name field sample) in offered
                                 for shown = (and sample (tuatara.display:shorten-string sample 40 :one-line t))
                                 collect (if name
                                             (make-candidate name :annotation (format nil "~:[~*~;$~d ~]~@[~a~]" (integerp field) field shown))
                                             (make-candidate (format nil "$~d" field) :annotation (or shown ""))))))
         (answer (completing-read prompt candidates :keep-order t))
         (field (answered-field answer offered keyed)))
    (when (eq field :unknown)
      (error "No such column: ~a (~:[~;a name, or ~]a number from 0)" (string-trim " " answer) (some #'first offered)))
    (values field (and field (funcall words field)))))

(defun read-record-field (buffer fields prompt)
  "Ask with PROMPT for a field of BUFFER's records, cut as FIELDS has them
\(READ-OFFERED-FIELD): two values, the field, NIL for the whole record, and
words naming it, \"$2\" or its name."
  (read-offered-field (offered-fields buffer fields) (and (record-fields-keys fields) t) prompt))

(defun read-list-column (list prompt)
  "Ask with PROMPT for a column of LIST, a table made by RENDER-TABLE-LIST,
by its title (READ-OFFERED-FIELD): two values, its number from 0, NIL for
the whole line, and its title."
  (let ((offered (list-table-columns list)))
    (read-offered-field offered nil prompt
                        :words (lambda (column) (or (first (find column offered :key #'second)) (format nil "$~d" column))))))

(defun filter-line-matcher (buffer)
  "What a filter of BUFFER's lines tries its pattern on: in a file of records
cut into fields (MODE-RECORD-FIELDS), the field asked for (READ-RECORD-FIELD),
else the whole line. Two values: a function of the compiled pattern, the text
of a line and its length, true when the pattern matches; and words naming the
field (\"$2\"), NIL for the whole line."
  (let ((fields (mode-record-fields (buffer-major-mode buffer) buffer)))
    (multiple-value-bind (field what) (and fields (read-record-field buffer fields "Filter by the column: "))
      (values (if field
                  (let ((each (record-fields-each fields)))
                    (lambda (scanner text length)
                      (and (funcall each text length field
                                    (lambda (string start end)
                                      (tuatara.runtime:stoppable (cl-ppcre:scan scanner string :start start :end end))))
                           t)))
                  (lambda (scanner text length)
                    (and (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start 0 :end length)) t)))
              what))))

;;; Fields apart by spaces, or by one character

(defun blank-separated-each (text length field fn)
  "Call FN with TEXT and where field FIELD (from 0) of its first LENGTH
characters stands, fields being apart by spaces and tabs (TEXT-FIELD-BOUNDS)."
  (multiple-value-bind (start end) (text-field-bounds text 0 length (1+ field))
    (and start (funcall fn text start end))))

(defun separated-each (separator)
  "A RECORD-FIELDS-EACH for fields apart by the character SEPARATOR, none
quoted."
  (lambda (text length field fn)
    (block separated-each-field
     (let ((start 0))
      (dotimes (i field)
        (let ((at (position separator text :start start :end length)))
          (if at (setf start (1+ at)) (return-from separated-each-field nil))))
      (funcall fn text start (or (position separator text :start start :end length) length))))))

(defmethod mode-record-fields ((mode log-mode) buffer)
  "A log's words, apart by spaces, named by its header when the first line is one."
  (declare (ignore buffer))
  (make-record-fields :each #'blank-separated-each))

(defmethod mode-record-fields ((mode hosts-generic-mode) buffer)
  "The hosts file's address, its name, then its other names."
  (declare (ignore buffer))
  (make-record-fields :each #'blank-separated-each :names '("address" "name")))

(defmethod mode-record-fields ((mode etc-passwd-generic-mode) buffer)
  "The password file's seven fields, apart by colons."
  (declare (ignore buffer))
  (make-record-fields :each (separated-each #\:) :names '("user" "password" "uid" "gid" "info" "home" "shell")))

;;; Fields of their own: .env, requirements, todo.txt and JSON Lines

(defun trimmed-end (text start end)
  "END moved back over the spaces and tabs of TEXT before it, no further than START."
  (loop while (and (> end start) (space-or-tab-char-p (char text (1- end)))) do (decf end))
  end)

(defun dotenv-each (text length field fn)
  "The name (field 0) or value (field 1) of an environment file's line, export
before it left out, the value's quotes taken off, a comment after an unquoted
one left out."
  (let* ((start (or (position-if-not #'space-or-tab-char-p text :end length) length))
         (start (if (and (<= (+ start 7) length) (string= "export " text :start2 start :end2 (+ start 7))) (+ start 7) start))
         (equals (position #\= text :start start :end length)))
    (when equals
      (case field
        (0 (funcall fn text start (trimmed-end text start equals)))
        (1 (let* ((from (or (position-if-not #'space-or-tab-char-p text :start (1+ equals) :end length) length))
                  (quote (and (< from length) (find (char text from) "\"'")))
                  (close (and quote (position quote text :start (1+ from) :end length))))
             (if close
                 (funcall fn text (1+ from) close)
                 (funcall fn text from (trimmed-end text from (or (search " #" text :start2 from :end2 length) length))))))))))

(defmethod mode-record-fields ((mode dotenv-mode) buffer)
  "An environment file's name and value."
  (declare (ignore buffer))
  (make-record-fields :each #'dotenv-each :names '("name" "value")))

(defun requirement-each (text length field fn)
  "The name (field 0, with its extras in brackets), the versions asked for
\(field 1) or the markers after ; (field 2) of a line of a Python
requirements file, a comment after # left out."
  (let* ((end (trimmed-end text 0 (or (search " #" text :end2 length) (and (plusp length) (char= (char text 0) #\#) 0) length)))
         (start (or (position-if-not #'space-or-tab-char-p text :end end) end))
         (name-end (or (position-if-not (lambda (c) (or (alphanumericp c) (find c "._-[],"))) text :start start :end end) end))
         (semicolon (or (position #\; text :start name-end :end end) end)))
    (flet ((give (from to)
             (let ((from (or (position-if-not #'space-or-tab-char-p text :start from :end to) to)))
               (funcall fn text from (trimmed-end text from to)))))
      (case field
        (0 (and (< start name-end) (funcall fn text start name-end)))
        (1 (give name-end semicolon))
        (2 (and (< semicolon end) (give (1+ semicolon) end)))))))

(defmethod mode-record-fields ((mode pip-requirements-mode) buffer)
  "A requirement's name, the versions asked for and its markers."
  (declare (ignore buffer))
  (make-record-fields :each #'requirement-each :names '("name" "version" "markers")))

(defparameter *todotxt-fields* '("done" "priority" "completed" "created" "text" "project" "context")
  "The parts of a todo.txt task a filter may try, in order, before the keys
of its key:value tags.")

(defun todotxt-date-p (text start length)
  "True when TEXT holds a date, 2026-10-19, from START, a space or the end
after it."
  (and (<= (+ start 10) length)
       (every (lambda (i) (if (member i '(4 7)) (char= (char text (+ start i)) #\-) (digit-char-p (char text (+ start i)))))
              '(0 1 2 3 4 5 6 7 8 9))
       (or (= (+ start 10) length) (space-or-tab-char-p (char text (+ start 10))))))

(defun todotxt-each (text length field fn)
  "A part of a todo.txt task, FIELD one of *TODOTXT-FIELDS* or the key of a
key:value tag: done (its x), priority (the letter), completed and created
\(the dates), text (the rest), each project after +, each context after @,
each value of the tag."
  (let ((p 0) done priority completed created)
    (flet ((skip () (loop while (and (< p length) (space-or-tab-char-p (char text p))) do (incf p))))
      (when (and (< (1+ p) length) (char= (char text 0) #\x) (space-or-tab-char-p (char text 1)))
        (setf done (cons 0 1) p 1) (skip)
        (when (todotxt-date-p text p length) (setf completed (cons p (+ p 10)) p (+ p 10)) (skip)))
      (when (and (<= (+ p 3) length) (char= (char text p) #\() (upper-case-p (char text (1+ p))) (char= (char text (+ p 2)) #\))
                 (or (= (+ p 3) length) (space-or-tab-char-p (char text (+ p 3)))))
        (setf priority (cons (1+ p) (+ p 2)) p (+ p 3)) (skip))
      (when (todotxt-date-p text p length) (setf created (cons p (+ p 10)) p (+ p 10)) (skip)))
    (flet ((give (span) (and span (funcall fn text (car span) (cdr span))))
           (each-word (test)
             (let ((n 1))
               (loop (multiple-value-bind (start end) (text-field-bounds text p length n)
                       (unless start (return nil))
                       (let ((from (funcall test start end)))
                         (when from
                           (let ((it (funcall fn text from end))) (when it (return it)))))
                       (incf n))))))
      (cond ((equal field "done") (give done))
            ((equal field "priority") (give priority))
            ((equal field "completed") (give completed))
            ((equal field "created") (give created))
            ((equal field "text") (and (< p length) (funcall fn text p (trimmed-end text p length))))
            ((member field '("project" "context") :test #'equal)
             (let ((sign (if (equal field "project") #\+ #\@)))
               (each-word (lambda (start end) (and (> end (1+ start)) (char= (char text start) sign) (1+ start))))))
            (t (let ((key (length field)))
                 (each-word (lambda (start end)
                              (and (> end (+ start key 1)) (char= (char text (+ start key)) #\:)
                                   (string= field text :start2 start :end2 (+ start key))
                                   (+ start key 1))))))))))

(defun todotxt-keys (buffer)
  "The parts of a todo.txt task (*TODOTXT-FIELDS*), then the keys of the
key:value tags of BUFFER's first tasks (FIRST-RECORDS)."
  (let ((keys (reverse *todotxt-fields*)))
    (dolist (text (first-records buffer) (nreverse keys))
      (cl-ppcre:do-register-groups (key) ("(?:^|\\s)([^\\s:+@]+):\\S" text)
        (pushnew key keys :test #'string=)))))

(defmethod mode-record-fields ((mode todotxt-mode) buffer)
  "A task's parts and the keys of its tags."
  (declare (ignore buffer))
  (make-record-fields :each #'todotxt-each :keys #'todotxt-keys))

(defun json-line-each (text length field fn)
  "The value of key FIELD, or of a path of keys apart by dots (user.id), of
the JSON object a line of JSON Lines holds: a string as it reads, escapes
undone; anything else as JSON. NIL for a line that is no JSON, or a key it
lacks or that holds null."
  (let* ((object (handler-case (tuatara.text:json-read text :end length)
                   (tuatara.text:json-error () nil)))
         (value (and (hash-table-p object)
                     (multiple-value-bind (whole found) (gethash field object)
                       (if found
                           (tuatara.text:json-get whole)
                           (apply #'tuatara.text:json-get object (uiop:split-string field :separator ".")))))))
    (when value
      (let ((string (if (stringp value) value (tuatara.text:json-write value))))
        (funcall fn string 0 (length string))))))

(defun json-line-keys (buffer)
  "The keys of the objects BUFFER's first lines hold (FIRST-RECORDS), in the
order they first come."
  (let ((keys '()))
    (dolist (text (first-records buffer) (nreverse keys))
      (let ((object (handler-case (tuatara.text:json-read text) (tuatara.text:json-error () nil))))
        (when (hash-table-p object)
          (loop for key being the hash-keys of object
                do (pushnew key keys :test #'string=)))))))

(defmethod mode-record-fields ((mode jsonl-mode) buffer)
  "A JSON Lines record's keys."
  (declare (ignore buffer))
  (make-record-fields :each #'json-line-each :keys #'json-line-keys))
