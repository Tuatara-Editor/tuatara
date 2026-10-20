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
             (format stream "Not JSON: ~a, at character ~d" (json-error-text condition) (json-error-position condition))))
  (:documentation "Text read as JSON that is not: what was wrong, and where."))

(setf (documentation 'json-error-text 'function) "What was wrong with the text read as JSON."
      (documentation 'json-error-position 'function) "Where in the text read as JSON it went wrong: a character's index.")

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

(defun json-read (text &key (start 0) (end (length text)))
  "The JSON value TEXT holds from START below END, nothing but white space
around it; a JSON-ERROR when it holds anything else."
  (let ((i start))
    (unless (<= 0 start end (length text))
      (error 'json-error :text "a start or end outside the text" :position start))
    (labels ((fail (what) (error 'json-error :text what :position i))
             (decimal (c)
               ;; an ASCII digit's weight, or NIL: other scripts' digits are no JSON
               (and (char<= #\0 c #\9) (- (char-code c) 48)))
             (blank ()
               (loop while (and (< i end) (member (char text i) '(#\Space #\Tab #\Newline #\Return))) do (incf i)))
             (expect (c)
               (unless (and (< i end) (char= (char text i) c)) (fail (format nil "~c expected" c)))
               (incf i))
             (word (word value)
               (unless (and (<= (+ i (length word)) end) (string= word text :start2 i :end2 (+ i (length word))))
                 (fail "a word other than true, false or null"))
               (incf i (length word))
               value)
             (hex-at (at)
               ;; the code of the four hexadecimal digits at AT, or NIL
               (and (<= (+ at 4) end)
                    (loop for j from at below (+ at 4) always (and (char< (char text j) #\Rubout) (digit-char-p (char text j) 16)))
                    (parse-integer text :start at :end (+ at 4) :radix 16)))
             (hex4 ()
               (let ((code (hex-at i)))
                 (unless code (fail "\\u without four hexadecimal digits"))
                 (incf i 4)
                 code))
             (json-string ()
               (expect #\")
               (let ((out (make-string-output-stream)))
                 (loop
                   (when (>= i end) (fail "a string not ended"))
                   (let ((c (char text i)))
                     (incf i)
                     (case c
                       (#\" (return (get-output-stream-string out)))
                       (#\\
                        (when (>= i end) (fail "a string not ended"))
                        (let ((e (char text i)))
                          (incf i)
                          (case e
                            ((#\" #\\ #\/) (write-char e out))
                            (#\b (write-char (code-char 8) out)) (#\f (write-char (code-char 12) out))
                            (#\n (write-char #\Newline out)) (#\r (write-char #\Return out)) (#\t (write-char #\Tab out))
                            (#\u (let* ((code (hex4))
                                        (low (and (<= #xD800 code #xDBFF) (< (1+ i) end)
                                                  (char= (char text i) #\\) (char= (char text (1+ i)) #\u)
                                                  (hex-at (+ i 2)))))
                                   ;; a pair of surrogates is one character; one alone stands as
                                   ;; U+FFFD, what follows it read as itself
                                   (write-char
                                    (code-char
                                     (cond ((and low (<= #xDC00 low #xDFFF))
                                            (incf i 6)
                                            (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00)))
                                           ((<= #xD800 code #xDFFF) #xFFFD)
                                           (t code)))
                                    out)))
                            (t (fail "an unknown escape")))))
                       (t (when (< (char-code c) 32) (fail "a control character in a string"))
                          (write-char c out)))))))
             (digits (from)
               ;; the digits from I as a number, and how many there were
               (let ((value 0) (start i))
                 (loop while (and (< i end) (decimal (char text i)))
                       do (when (> (- i from) *json-longest-number*) (fail "a number too long"))
                          (setf value (+ (* value 10) (decimal (char text i))))
                          (incf i))
                 (values value (- i start))))
             (json-number ()
               (let ((from i) (sign 1) (whole 0) (fraction 0) (scale 0) (exponent 0) (float nil))
                 (when (char= (char text i) #\-) (setf sign -1) (incf i))
                 (unless (and (< i end) (decimal (char text i))) (fail "a number without digits"))
                 (when (and (char= (char text i) #\0) (< (1+ i) end) (decimal (char text (1+ i))))
                   (fail "a number beginning with 0"))
                 (setf whole (digits from))
                 (when (and (< i end) (char= (char text i) #\.))
                   (setf float t) (incf i)
                   (unless (and (< i end) (decimal (char text i))) (fail "a number without digits after its point"))
                   (setf (values fraction scale) (digits from)))
                 (when (and (< i end) (char-equal (char text i) #\e))
                   (setf float t) (incf i)
                   (let ((negative (and (< i end) (char= (char text i) #\-))))
                     (when (and (< i end) (member (char text i) '(#\+ #\-))) (incf i))
                     (unless (and (< i end) (decimal (char text i))) (fail "a number without digits in its exponent"))
                     (setf exponent (min 1000 (digits from)))
                     (when negative (setf exponent (- exponent)))))
                 (if float
                     (let ((value (* sign (+ whole (/ fraction (expt 10 scale))) (expt 10 exponent))))
                       ;; past what a double holds: the largest it holds, or nought
                       (cond ((> (abs value) most-positive-double-float) (* sign most-positive-double-float))
                             ((and (/= value 0) (< (abs value) least-positive-normalized-double-float)) 0d0)
                             (t (coerce value 'double-float))))
                     (* sign whole))))
             (value (depth)
               (when (> depth *json-deepest*) (fail "nesting too deep"))
               (blank)
               (when (>= i end) (fail "a value expected"))
               (let ((c (char text i)))
                 (cond ((char= c #\{)
                        (incf i)
                        (let ((object (make-hash-table :test 'equal)))
                          (blank)
                          (if (and (< i end) (char= (char text i) #\}))
                              (incf i)
                              (loop (blank)
                                    (let ((key (json-string)))
                                      (blank) (expect #\:)
                                      (setf (gethash key object) (value (1+ depth))))
                                    (blank)
                                    (when (>= i end) (fail "an object not ended"))
                                    (case (char text i)
                                      (#\, (incf i))
                                      (#\} (incf i) (return))
                                      (t (fail ", or } expected")))))
                          object))
                       ((char= c #\[)
                        (incf i)
                        (let ((items '()))
                          (blank)
                          (if (and (< i end) (char= (char text i) #\]))
                              (incf i)
                              (loop (push (value (1+ depth)) items)
                                    (blank)
                                    (when (>= i end) (fail "an array not ended"))
                                    (case (char text i)
                                      (#\, (incf i))
                                      (#\] (incf i) (return))
                                      (t (fail ", or ] expected")))))
                          (coerce (nreverse items) 'simple-vector)))
                       ((char= c #\") (json-string))
                       ((char= c #\t) (word "true" t))
                       ((char= c #\f) (word "false" :false))
                       ((char= c #\n) (word "null" :null))
                       ((or (char= c #\-) (decimal c)) (json-number))
                       (t (fail "a value expected"))))))
      (prog1 (value 0)
        (blank)
        (when (< i end) (fail "more after the value"))))))

;;; Writing

(defun json-write (value &optional stream)
  "Write VALUE as JSON to STREAM, or return it as a string when STREAM is NIL:
a hash table as an object (its keys strings, or symbols written in lower
case), a vector or a list as an array, a string as a string, a number as a
number, T and :TRUE as true, :FALSE as false, NIL and :NULL as null. A raw
byte in a string (an octet of text that is not UTF-8) is written as U+FFFD,
so that what is written is always UTF-8 and takes the same room in UTF-16."
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
