;;;; src/text/json.lisp --- JSON read and written: the language servers' messages, and any other.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A JSON object is read as a hash table of its keys (strings), an array as a
;;;; vector, true as T, false as :FALSE and null as :NULL; numbers as integers,
;;;; or as double floats when they have a fraction or an exponent. Written back
;;;; the same way, NIL standing for null. What comes from another program is
;;;; never trusted: nesting is bounded, a number's exponent too, and anything
;;;; that is not JSON is refused with a JSON-ERROR saying where.

(in-package #:tuatara.text)

(define-condition json-error (error)
  ((text :initarg :text :reader json-error-text)
   (position :initarg :position :reader json-error-position))
  (:report (lambda (condition stream)
             (format stream "Not JSON: ~a, at character ~d" (json-error-text condition)
                     (json-error-position condition))))
  (:documentation "Text read as JSON that is not: what was wrong, and where."))

(setf (documentation 'json-error-text 'function) "What was wrong with the text read as JSON."
      (documentation 'json-error-position 'function) "Where in the text read as JSON it went wrong:
a character's index.")

(defparameter *json-deepest* 512
  "The deepest JSON read: objects and arrays within each other past this are refused.")

(defparameter *json-longest-number* 1000
  "The most characters of a number in JSON read: a longer one is refused.")

(defun json-object (&rest keys-and-values)
  "A JSON object of KEYS-AND-VALUES, each key a string followed by its value;
a key whose value is :OMIT is left out."
  (let ((object (make-hash-table :test 'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          unless (eq value :omit)
            do (setf (gethash key object) value))
    object))

(defun json-get (value &rest path)
  "What PATH leads to in VALUE, read JSON: each step a key of an object (a
string) or an index of an array (an integer); NIL where there is nothing, or
where it is null."
  (dolist (step path (if (eq value :null) nil value))
    (setf value (typecase value
                  (hash-table (and (stringp step) (gethash step value)))
                  (vector (and (integerp step) (< -1 step (length value)) (aref value step)))
                  (t nil)))))

(defun json-true-p (value)
  "True when VALUE, read JSON, is true."
  (eq value t))

;;; Reading
;;;
;;; One walk over the grammar serves both readers: JSON-READ makes the value,
;;; JSON-PATH-BOUNDS only finds where the value a list of keys leads to lies.

(declaim (inline make-json-scan))
(define-record (json-scan (:constructor make-json-scan (text i end)) (:copier nil))
  "A walk over the JSON in TEXT: I is where it has got to, END where the text
read ends; FOUND-START and FOUND-END where the value looked for lies, once
found (JSON-PATH-BOUNDS)."
  (text "" :type string :read-only t :documentation "The text a JSON-SCAN walks over.")
  (i 0 :type fixnum :documentation "Where in its text a JSON-SCAN has got to.")
  (end 0 :type fixnum :read-only t :documentation "Where the text a JSON-SCAN reads ends.")
  (found-start nil :documentation "Where the value a JSON-SCAN looks for begins, or NIL.")
  (found-end nil :documentation "Where the value a JSON-SCAN looks for ends, or NIL."))

(setf (documentation 'make-json-scan 'function) "Return a JSON-SCAN of TEXT from I below END.")

(defmacro with-json-scan ((scan) &body body)
  "Run BODY with TEXT, I and END standing for those of the JSON-SCAN SCAN."
  `(symbol-macrolet ((text (json-scan-text ,scan)) (i (json-scan-i ,scan)) (end (json-scan-end ,scan)))
     ,@body))

(defun json-fail (scan what)
  "Signal a JSON-ERROR saying WHAT was wrong where SCAN has got to."
  (error 'json-error :text what :position (json-scan-i scan)))

(declaim (inline json-decimal))
(defun json-decimal (c)
  "The weight of C, an ASCII digit, or NIL: digits of other scripts are not JSON."
  (and (char<= #\0 c #\9) (- (char-code c) 48)))

(declaim (inline json-at-p json-digit-at))
(defun json-at-p (scan c &optional (offset 0))
  "True when the character OFFSET past where SCAN has got to is C."
  (with-json-scan (scan)
    (let ((j (+ i offset)))
      (and (< j end) (char= (char text j) c)))))

(defun json-digit-at (scan &optional (offset 0))
  "The weight of the ASCII digit OFFSET past where SCAN has got to, or NIL."
  (with-json-scan (scan)
    (let ((j (+ i offset)))
      (and (< j end) (json-decimal (char text j))))))

(defun json-blank (scan)
  "Move SCAN past white space."
  (with-json-scan (scan)
    (loop while (and (< i end) (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
          do (incf i))))

(defun json-expect (scan c)
  "Move SCAN past the character C, a JSON-ERROR when it is not there."
  (unless (json-at-p scan c)
    (json-fail scan (format nil "~c expected" c)))
  (incf (json-scan-i scan)))

(defun json-word (scan word value)
  "Move SCAN past WORD, true, false or null, and return VALUE."
  (with-json-scan (scan)
    (unless (and (<= (+ i (length word)) end) (string= word text :start2 i :end2 (+ i (length word))))
      (json-fail scan "a word other than true, false or null"))
    (incf i (length word))
    value))

(defun json-hex-at (scan at)
  "The code the four hexadecimal digits at AT of SCAN's text write, or NIL."
  (with-json-scan (scan)
    (and (<= (+ at 4) end)
         (loop for j from at below (+ at 4)
               always (and (char< (char text j) #\Rubout) (digit-char-p (char text j) 16)))
         (parse-integer text :start at :end (+ at 4) :radix 16))))

(defun json-escape (scan out)
  "Move SCAN past the escape after a backslash in a string, writing the
character it stands for to OUT when OUT is a stream. A pair of escaped
surrogates is one character; one alone stands as U+FFFD."
  (with-json-scan (scan)
    (when (>= i end) (json-fail scan "a string not ended"))
    (let ((e (char text i)))
      (incf i)
      (flet ((put (c) (when out (write-char c out))))
        (case e
          ((#\" #\\ #\/) (put e))
          (#\b (put (code-char 8)))
          (#\f (put (code-char 12)))
          (#\n (put #\Newline))
          (#\r (put #\Return))
          (#\t (put #\Tab))
          (#\u (let ((code (json-hex-at scan i)))
                 (unless code (json-fail scan "\\u without four hexadecimal digits"))
                 (incf i 4)
                 (when out
                   (let ((low (and (<= #xD800 code #xDBFF) (json-at-p scan #\\) (json-at-p scan #\u 1)
                                   (json-hex-at scan (+ i 2)))))
                     (put (code-char (cond ((and low (<= #xDC00 low #xDFFF))
                                            (incf i 6)
                                            (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00)))
                                           ((<= #xD800 code #xDFFF) #xFFFD)
                                           (t code))))))))
          (t (json-fail scan "an unknown escape")))))))

(defun json-scan-string (scan read)
  "Move SCAN past a string: with READ, return the string; else two values,
the index after its opening mark and that of its closing one."
  (json-expect scan #\")
  (with-json-scan (scan)
    (let ((out (and read (make-string-output-stream))) (from i))
      (loop
        (when (>= i end) (json-fail scan "a string not ended"))
        (let ((c (char text i)))
          (incf i)
          (case c
            (#\" (return (if read (get-output-stream-string out) (values from (1- i)))))
            (#\\ (json-escape scan out))
            (t (when (< (char-code c) 32) (json-fail scan "a control character in a string"))
               (when out (write-char c out)))))))))

(defun json-digits (scan from read)
  "Move SCAN past a run of digits of the number begun at FROM, refused past
*JSON-LONGEST-NUMBER* characters: two values, the number they write (0
without READ) and how many there were."
  (with-json-scan (scan)
    (let ((value 0) (start i))
      (loop for d = (json-digit-at scan)
            while d
            do (when (> (- i from) *json-longest-number*) (json-fail scan "a number too long"))
               (when read (setf value (+ (* value 10) d)))
               (incf i))
      (values value (- i start)))))

(defun json-number-value (sign whole fraction scale exponent float)
  "The number read in parts: an integer, or a double float with FLOAT; past
what a double holds, the largest it holds, or nought."
  (if float
      (let ((value (* sign (+ whole (/ fraction (expt 10 scale))) (expt 10 exponent))))
        (cond ((> (abs value) most-positive-double-float) (* sign most-positive-double-float))
              ((and (/= value 0) (< (abs value) least-positive-normalized-double-float)) 0d0)
              (t (coerce value 'double-float))))
      (* sign whole)))

(defun json-scan-number (scan read)
  "Move SCAN past a number, and with READ return it (JSON-NUMBER-VALUE)."
  (with-json-scan (scan)
    (let ((from i) (sign 1) (whole 0) (fraction 0) (scale 0) (exponent 0) (float nil))
      (when (json-at-p scan #\-) (setf sign -1) (incf i))
      (unless (json-digit-at scan) (json-fail scan "a number without digits"))
      (when (and (json-at-p scan #\0) (json-digit-at scan 1)) (json-fail scan "a number beginning with 0"))
      (setf whole (json-digits scan from read))
      (when (json-at-p scan #\.)
        (setf float t)
        (incf i)
        (unless (json-digit-at scan) (json-fail scan "a number without digits after its point"))
        (setf (values fraction scale) (json-digits scan from read)))
      (when (or (json-at-p scan #\e) (json-at-p scan #\E))
        (setf float t)
        (incf i)
        (let ((negative (json-at-p scan #\-)))
          (when (or negative (json-at-p scan #\+)) (incf i))
          (unless (json-digit-at scan) (json-fail scan "a number without digits in its exponent"))
          (setf exponent (min 1000 (json-digits scan from read)))
          (when negative (setf exponent (- exponent)))))
      (and read (json-number-value sign whole fraction scale exponent float)))))

(defun json-scan-object (scan depth path)
  "Move SCAN past an object, after its opening brace, PATH as for
JSON-SCAN-VALUE: with :READ, return it as a hash table."
  (let ((object (and (eq path :read) (make-hash-table :test 'equal))))
    (json-blank scan)
    (if (json-at-p scan #\})
        (incf (json-scan-i scan))
        (loop (json-blank scan)
              (if object
                  (let ((key (json-scan-string scan t)))
                    (json-blank scan)
                    (json-expect scan #\:)
                    (setf (gethash key object) (json-scan-value scan (1+ depth) :read)))
                  (multiple-value-bind (key-start key-end) (json-scan-string scan nil)
                    (json-blank scan)
                    (json-expect scan #\:)
                    (json-scan-member scan (1+ depth) path key-start key-end)))
              (json-blank scan)
              (cond ((json-at-p scan #\,) (incf (json-scan-i scan)))
                    ((json-at-p scan #\}) (incf (json-scan-i scan)) (return))
                    ((>= (json-scan-i scan) (json-scan-end scan)) (json-fail scan "an object not ended"))
                    (t (json-fail scan ", or } expected")))))
    object))

(defun json-scan-member (scan depth path key-start key-end)
  "Move SCAN past the value of the key between KEY-START and KEY-END of an
object, noting where it lies when it is the one PATH, a list of keys or
:NONE, leads to. A later key of the same name stands for an earlier one."
  (with-json-scan (scan)
    (cond ((not (and (consp path) (json-key-equal (first path) text key-start key-end)))
           (json-scan-value scan depth :none))
          ((rest path)
           (setf (json-scan-found-start scan) nil (json-scan-found-end scan) nil)
           (json-scan-value scan depth (rest path)))
          (t (json-blank scan)
             (let ((from i))
               (json-scan-value scan depth :none)
               (setf (json-scan-found-start scan) from (json-scan-found-end scan) i))))))

(defun json-scan-array (scan depth read)
  "Move SCAN past an array, after its opening bracket: with READ, return it
as a simple vector."
  (let ((items '()))
    (json-blank scan)
    (if (json-at-p scan #\])
        (incf (json-scan-i scan))
        (loop (let ((item (json-scan-value scan (1+ depth) (if read :read :none))))
                (when read (push item items)))
              (json-blank scan)
              (cond ((json-at-p scan #\,) (incf (json-scan-i scan)))
                    ((json-at-p scan #\]) (incf (json-scan-i scan)) (return))
                    ((>= (json-scan-i scan) (json-scan-end scan)) (json-fail scan "an array not ended"))
                    (t (json-fail scan ", or ] expected")))))
    (and read (coerce (nreverse items) 'simple-vector))))

(defun json-scan-value (scan depth path)
  "Move SCAN past a value DEPTH objects and arrays deep, refused past
*JSON-DEEPEST*. With PATH :READ, return the value; otherwise PATH is the keys
still to follow inside it (JSON-SCAN-MEMBER), or :NONE when it is not on the
way."
  (when (> depth *json-deepest*) (json-fail scan "nesting too deep"))
  (json-blank scan)
  (with-json-scan (scan)
    (when (>= i end) (json-fail scan "a value expected"))
    (let ((c (char text i)) (read (eq path :read)))
      (cond ((char= c #\{) (incf i) (json-scan-object scan depth path))
            ((char= c #\[) (incf i) (json-scan-array scan depth read))
            ((char= c #\") (json-scan-string scan read))
            ((char= c #\t) (json-word scan "true" t))
            ((char= c #\f) (json-word scan "false" :false))
            ((char= c #\n) (json-word scan "null" :null))
            ((or (char= c #\-) (json-decimal c)) (json-scan-number scan read))
            (t (json-fail scan "a value expected"))))))

(defun scan-json (text start end path)
  "Walk the JSON value TEXT holds from START below END, nothing but white space
around it, signalling a JSON-ERROR saying where when it holds anything else.
With PATH :READ, return the value read. Otherwise PATH is a list of keys, each
of an object inside the one before, the first of the object TEXT holds, and
no value is made: return where the value the keys lead to lies, as two
values, the index of its first character and the index after its last; for
no keys, that of the whole object; NIL when the keys lead to nothing."
  (unless (<= 0 start end (length text))
    (error 'json-error :text "a start or end outside the text" :position start))
  (let ((scan (make-json-scan text start end)))
    (declare (dynamic-extent scan))
    (json-blank scan)
    (let* ((from (json-scan-i scan))
           (value (json-scan-value scan 0 (or path :none))))
      (when (and (null path) (char= (char text from) #\{))
        (setf (json-scan-found-start scan) from (json-scan-found-end scan) (json-scan-i scan)))
      (json-blank scan)
      (when (< (json-scan-i scan) end) (json-fail scan "more after the value"))
      (cond ((eq path :read) value)
            ((json-scan-found-start scan) (values (json-scan-found-start scan) (json-scan-found-end scan)))))))

(defun json-read (text &key (start 0) (end (length text)))
  "The JSON value TEXT holds from START below END, nothing but white space
around it; a JSON-ERROR when it holds anything else."
  (scan-json text start end :read))

;;; Finding a value without reading the rest

(defun json-key-equal (key text start end)
  "True when KEY, a string, is the key whose text between its quotation
marks is TEXT from START below END: compared where it is, a string made only
when the key holds an escape."
  (if (find #\\ text :start start :end end)
      (string= key (json-read text :start (1- start) :end (1+ end)))
      (string= key text :start2 start :end2 end)))

(defun json-path-bounds (text path &key (start 0) (end (length text)))
  "Where the value PATH leads to lies in the JSON TEXT holds from START below
END, as two values, the index of its first character and the index after its
last: PATH is a list of keys, each of an object inside the one before, the
first of the object TEXT holds; no keys, that whole object. NIL when TEXT
does not hold JSON throughout, as JSON-READ would refuse it, or PATH leads to
nothing. When an object holds a key twice, the later one counts, as when it
is read. No value is made, only a key that holds an escape."
  (handler-case (scan-json text start end path)
    (json-error () nil)))

;;; Writing

(defun json-write (value &optional stream)
  "Write VALUE as JSON to STREAM, or return it as a string when STREAM is NIL.
A hash table is written as an object (its keys strings, or symbols written in
lower case), a vector or a list as an array, a string as a string, a number
as a number, T and :TRUE as true, :FALSE as false, NIL and :NULL as null. A
raw byte in a string (an octet of text that is not UTF-8) is written as
U+FFFD, so that what is written is always UTF-8 and takes the same room in
UTF-16."
  (if (null stream)
      (with-output-to-string (s) (json-write value s))
      (labels ((text (string)
                 (write-char #\" stream)
                 (loop for c across string
                       for code = (char-code c)
                       do (case c
                            (#\" (write-string "\\\"" stream))
                            (#\\ (write-string "\\\\" stream))
                            (#\Newline (write-string "\\n" stream))
                            (#\Return (write-string "\\r" stream))
                            (#\Tab (write-string "\\t" stream))
                            (t (cond ((or (< code 32) (= code 127)) (format stream "\\u~4,'0x" code))
                                     ((raw-byte-code-p code) (write-char (code-char #xFFFD) stream))
                                     (t (write-char c stream))))))
                 (write-char #\" stream))
               (out (value)
                 (typecase value
                   ((member nil :null) (write-string "null" stream))
                   ((member t :true) (write-string "true" stream))
                   ((eql :false) (write-string "false" stream))
                   (string (text value))
                   (integer (format stream "~d" value))
                   (real (let ((*read-default-float-format* 'double-float))
                           (format stream "~f" (coerce value 'double-float))))
                   (hash-table
                    (write-char #\{ stream)
                    (let ((first t))
                      (maphash (lambda (key item)
                                 (unless first (write-char #\, stream))
                                 (setf first nil)
                                 (text (if (stringp key) key (string-downcase (string key))))
                                 (write-char #\: stream)
                                 (out item))
                               value))
                    (write-char #\} stream))
                   (sequence
                    (write-char #\[ stream)
                    (let ((first t))
                      (map nil (lambda (item)
                                 (unless first (write-char #\, stream))
                                 (setf first nil)
                                 (out item))
                           value))
                    (write-char #\] stream))
                   (t (error "~s cannot be written as JSON" value)))))
        (out value)
        nil)))

(defun json-integer (value &rest path)
  "The integer PATH leads to in VALUE, read JSON (JSON-GET), or
NIL when it is anything else."
  (let ((found (apply #'json-get value path)))
    (and (integerp found) found)))

(defun json-text (value &rest path)
  "The string PATH leads to in VALUE, read JSON (JSON-GET), or
NIL when it is anything else."
  (let ((found (apply #'json-get value path)))
    (and (stringp found) found)))

(defun json-items (value &rest path)
  "The array PATH leads to in VALUE, read JSON (JSON-GET), as a
vector; an empty one when it is anything else."
  (let ((found (apply #'json-get value path)))
    (if (and (vectorp found) (not (stringp found))) found #())))
