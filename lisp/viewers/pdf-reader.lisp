;;;; lisp/viewers/pdf-reader.lisp --- Tuatara's PDF reader: objects, streams, saves and content, from octets alone.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A PDF read by Tuatara alone, nothing in it run: its values (numbers,
;;;; strings, names, arrays, dictionaries, references), its objects where the
;;;; file writes them, its streams decoded by every filter and predictor, the
;;;; objects packed in object streams, the %%EOF lines and cross-reference
;;;; tables of its saves, and the operators of its pages' content. M-x
;;;; file-metadata reads a PDF's account of itself with it, and packages
;;;; examining PDFs use it too. Every offset and length a file gives is
;;;; checked against its octets, every structure held to a count, and a file
;;;; that names objects it does not have costs a bounded time
;;;; (*PDF-OBJECTS-FOUND*).

(in-package #:tuatara.defaults)

;;; Octets as text, and zlib data

(defun octets-text (octets start end)
  "OCTETS from START below END as text: UTF-8 when it is, else as Latin-1
\(each octet its character); a null (NUL) and what follows left out."
  (let* ((end (or (position 0 octets :start start :end end) end))
         (part (subseq octets start end)))
    (or (handler-case (sb-ext:octets-to-string part :external-format :utf-8) (sb-int:character-decoding-error () nil))
        (tuatara.text:octets-latin-1 part))))

(defun zlib-octets (octets start end &key (limit (* 4 1024 1024)) errors)
  "The octets the zlib data in OCTETS from START below END stands for (its
two-octet header passed over, when it has one), at most LIMIT of them; NIL
when it is damaged or stands for more, or with ERRORS, a ZIP-ERROR saying
why."
  (let ((start (if (and (>= (- end start) 2) (= 8 (logand 15 (aref octets start)))
                        (zerop (mod (+ (* 256 (aref octets start)) (aref octets (1+ start))) 31)))
                   (+ start 2)
                   start)))
    (if errors
        (inflate octets :start start :end end :limit limit)
        (ignoring-errors-reported ("inflating the zlib data at ~:d" start)
          (handler-case (inflate octets :start start :end end :limit limit) (zip-error () nil))))))

;;; Values

(defun pdf-string-text (octets)
  "OCTETS, a PDF text string, as text: UTF-16 with its mark, UTF-8 with its,
else each octet its character."
  (cond ((and (>= (length octets) 2) (= (aref octets 0) #xFE) (= (aref octets 1) #xFF)) (utf-16-text octets :big-endian t))
        ((and (>= (length octets) 3) (= (aref octets 0) #xEF) (= (aref octets 1) #xBB) (= (aref octets 2) #xBF))
         (octets-text octets 3 (length octets)))
        (t (tuatara.text:octets-latin-1 octets))))

(defun pdf-white-p (octet)
  "True when OCTET is white space in a PDF: null (NUL), tab, line feed, form
feed, carriage return or space."
  (member octet '(0 9 10 12 13 32)))

(defun pdf-skip-space (octets i)
  "Where in OCTETS the first thing from I that is neither white space nor a
comment begins; the length of OCTETS when nothing does."
  (let ((n (length octets)))
    (loop while (< i n)
          do (let ((c (aref octets i)))
               (cond ((pdf-white-p c) (incf i))
                     ((= c 37) (setf i (or (position-if (lambda (x) (member x '(10 13))) octets :start i) n)))
                     (t (return)))))
    i))

(defun pdf-delimiter-p (c)
  "True when octet C ends a name or a word in a PDF: white space, a bracket, / or %."
  (member c '(32 9 10 13 12 0 40 41 60 62 91 93 123 125 47 37)))

(defun pdf-line-end-after (octets i)
  "Where the line end at I in OCTETS (carriage return and line feed (CR LF),
line feed (LF) or carriage return (CR)) ends; I when there is none."
  (let ((n (length octets)))
    (cond ((and (< (1+ i) n) (= (aref octets i) 13) (= (aref octets (1+ i)) 10)) (+ i 2))
          ((and (< i n) (member (aref octets i) '(10 13))) (1+ i))
          (t i))))

(defun pdf-integer-at (octets i &optional (end (length octets)))
  "The whole number, unsigned, written in OCTETS at I (white space before it
passed over), and where it ends, below END; NIL when there is none, or it
has more than 15 digits."
  (let ((i (loop while (and (< i end) (pdf-white-p (aref octets i))) do (incf i) finally (return i))))
    (when (and (< i end) (<= 48 (aref octets i) 57))
      (let ((value 0) (j i))
        (loop while (and (< j end) (<= 48 (aref octets j) 57) (< (- j i) 16))
              do (setf value (+ (* value 10) (- (aref octets j) 48))) (incf j))
        (and (< (- j i) 16) (values value j))))))

(defun walk-pdf-dictionary (function octets i depth)
  "Call FUNCTION with each entry of the << dictionary >> in OCTETS from I,
nested DEPTH deep, in the order written: its key, its value, where the key
ends and where the value ends. Return where the reading ended (past the >>,
or where an entry could not be read) and where the >> is, NIL when it was
not reached, as two values."
  (let ((n (length octets)) (j (+ i 2)))
    (loop
      (setf j (pdf-skip-space octets j))
      (when (>= j n) (return (values j nil)))
      (when (and (= (aref octets j) 62) (< (1+ j) n) (= (aref octets (1+ j)) 62)) (return (values (+ j 2) j)))
      (multiple-value-bind (key after) (pdf-object-at octets j (1+ depth))
        (unless (and (stringp key) (plusp (length key)) (char= (char key 0) #\/) (> after j)) (return (values j nil)))
        (multiple-value-bind (value next) (pdf-object-at octets after (1+ depth))
          (when (<= next after) (return (values j nil)))
          (funcall function key value after next)
          (setf j next))))))

(defun pdf-dictionary-at (octets i depth)
  "The << dictionary >> in OCTETS from I, nested DEPTH deep, as an alist of
(NAME . VALUE), and where it ends."
  (let ((pairs '()))
    (flet ((pair (key value after next)
             (declare (ignore after next))
             (push (cons key value) pairs)))
      (declare (dynamic-extent #'pair))
      (let ((end (walk-pdf-dictionary #'pair octets i depth)))
        (values (nreverse pairs) end)))))

(defun map-pdf-dictionary (function octets at)
  "Call FUNCTION with each entry of the << dictionary >> written in OCTETS
from AT (white space and comments before it passed over), in the order
written: its key (a name with its /), its value as PDF-OBJECT-AT reads it,
where the key ends and where the value ends. Return where the dictionary
begins and where its closing >> is, as two values; NIL when no dictionary
begins there, or it cannot be read to its >>."
  (let ((n (length octets)) (start (pdf-skip-space octets at)))
    (when (and (< (1+ start) n) (= (aref octets start) 60) (= (aref octets (1+ start)) 60))
      (let ((close (nth-value 1 (walk-pdf-dictionary function octets start 0))))
        (and close (values start close))))))

(defun pdf-hex-string-at (octets i &optional raw)
  "The <hex string> in OCTETS from I, as text (with RAW, as the octets it
stands for), and where it ends. No more than 64 KB of it is read."
  (let* ((n (length octets))
         (close (or (position 62 octets :start i) n))
         (string (pdf-ascii-hex-decoded (subseq octets (1+ i) (min close (+ i 1 131072))) 65536)))
    (values (if raw string (pdf-string-text string)) (min n (1+ close)))))

(defun pdf-literal-string-at (octets i &optional raw)
  "The (literal string) in OCTETS from I, its escapes and balanced brackets
read, as text (with RAW, as its octets), and where it ends. No more than
64 KB of it is read."
  (let ((n (length octets)) (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (level 1) (j (1+ i)))
    (loop while (and (< j n) (plusp level) (< (length out) 65536))
          do (let ((x (aref octets j)))
               (cond ((= x 92)
                      (incf j)
                      (when (< j n)
                        (let ((e (aref octets j)))
                          (case (code-char e)
                            (#\n (vector-push-extend 10 out)) (#\r (vector-push-extend 13 out)) (#\t (vector-push-extend 9 out))
                            (#\b (vector-push-extend 8 out)) (#\f (vector-push-extend 12 out))
                            ((#\Newline #\Return))
                            (t (if (<= 48 e 55)
                                   (let ((value 0) (k 0))
                                     (loop while (and (< k 3) (< j n) (<= 48 (aref octets j) 55))
                                           do (setf value (+ (* value 8) (- (aref octets j) 48))) (incf j) (incf k))
                                     (decf j)
                                     (vector-push-extend (logand value 255) out))
                                   (vector-push-extend e out)))))))
                     ((= x 40) (incf level) (vector-push-extend x out))
                     ((= x 41) (decf level) (when (plusp level) (vector-push-extend x out)))
                     (t (vector-push-extend x out)))
               (incf j)))
    (values (if raw (coerce out '(simple-array (unsigned-byte 8) (*))) (pdf-string-text out)) j)))

(defparameter *pdf-most-items* 1000
  "The most items of a PDF array read (PDF-OBJECT-AT): the rest of a longer
one is passed over.")

(defun pdf-array-at (octets i depth)
  "The [array] in OCTETS from I, nested DEPTH deep, as a list of no more than
*PDF-MOST-ITEMS* items, and where it ends: past the closing bracket, or
where the items stopped being read."
  (let ((n (length octets)) (items '()) (count 0) (j (1+ i)))
    (loop
      (setf j (pdf-skip-space octets j))
      (when (or (>= j n) (= (aref octets j) 93) (> count *pdf-most-items*)) (setf j (min n (1+ j))) (return))
      (multiple-value-bind (item next) (pdf-object-at octets j (1+ depth))
        (when (<= next j) (return))
        (push item items) (incf count) (setf j next)))
    (values (nreverse items) j)))

(defun pdf-name-at (octets i)
  "The /Name in OCTETS from I, its #xx escapes read, as a string with its /,
and where it ends."
  (let ((n (length octets)) (j (1+ i)))
    (loop while (and (< j n) (not (pdf-delimiter-p (aref octets j)))) do (incf j))
    (values (concatenate 'string "/" (cl-ppcre:regex-replace-all "#([0-9A-Fa-f]{2})" (tuatara.text:octets-latin-1 octets :start (1+ i) :end j)
                                                                 (lambda (m g) (declare (ignore m)) (string (code-char (parse-integer g :radix 16))))
                                                                 :simple-calls t))
            j)))

(defun pdf-number-at (octets i &optional (end (length octets)))
  "The number written in OCTETS from I, below END, and where it ends: an
integer, or a double-float when it has a point; (:REF N G) when it begins a
reference, N G R. A sign within it is passed over, as PDF readers do. NIL
for a lone sign or point, and for a number of more than 32 characters,
which no PDF writes."
  (let ((j i) (sign 1) (whole 0) (fraction 0) (scale 1) (digits 0) (point nil))
    (when (and (< j end) (member (aref octets j) '(43 45)))
      (when (= (aref octets j) 45) (setf sign -1))
      (incf j))
    (loop while (< j end)
          do (let ((c (aref octets j)))
               (cond ((<= 48 c 57)
                      (incf digits)
                      (when (<= (- j i) 32)
                        (if point
                            (setf fraction (+ (* fraction 10) (- c 48)) scale (* scale 10))
                            (setf whole (+ (* whole 10) (- c 48))))))
                     ((and (= c 46) (not point)) (setf point t))
                     ((member c '(43 45)))
                     (t (return))))
             (incf j))
    (let ((number (and (plusp digits) (<= (- j i) 32)
                       (if point (* sign (coerce (+ whole (/ fraction scale)) 'double-float)) (* sign whole)))))
      ;; N G R, white space between each
      (multiple-value-bind (generation after)
          (and (integerp number) (< j end) (pdf-white-p (aref octets j)) (pdf-integer-at octets j end))
        (let ((r (and generation (loop with k = after while (and (< k end) (pdf-white-p (aref octets k))) do (incf k) finally (return k)))))
          (if (and r (> r after) (< r end) (= (aref octets r) 82)
                   (or (= (1+ r) end) (pdf-delimiter-p (aref octets (1+ r)))))
              (values (list :ref number generation) (1+ r))
              (values number j)))))))

(defun pdf-word-at (octets i)
  "The word in OCTETS from I (true, false, null or another), as a string, and
where it ends; at least one octet long."
  (let ((n (length octets)) (j i))
    (loop while (and (< j n) (not (pdf-delimiter-p (aref octets j)))) do (incf j))
    (values (tuatara.text:octets-latin-1 octets :start i :end (max (1+ i) j)) (max (1+ i) j))))

(defun pdf-object-at (octets at &optional (depth 0))
  "The PDF object in OCTETS from AT, and where it ends: a dictionary as an
alist of (NAME . VALUE), an array as a list, a string as text, a name as
:NAME's string with a /, a number, (:REF N G) for a reference; NIL for what
is not one. Nested no deeper than 32."
  (let ((n (length octets))
        (i (pdf-skip-space octets at)))
    (when (or (>= i n) (> depth 32)) (return-from pdf-object-at (values nil i)))
    (let ((c (aref octets i)))
      (cond ((and (= c 60) (< (1+ i) n) (= (aref octets (1+ i)) 60)) (pdf-dictionary-at octets i depth))
            ((= c 60) (pdf-hex-string-at octets i))
            ((= c 40) (pdf-literal-string-at octets i))
            ((= c 91) (pdf-array-at octets i depth))
            ((= c 47) (pdf-name-at octets i))
            ((or (<= 48 c 57) (member c '(43 45 46))) (pdf-number-at octets i))
            (t (pdf-word-at octets i))))))

;;; PDF values, as PDF-OBJECT-AT reads them

(defun pdf-ref-p (value)
  "The number of the object VALUE refers to when it is a reference, (:REF
NUMBER GENERATION); NIL when it is none."
  (and (consp value) (eq (first value) :ref) (integerp (second value)) (second value)))

(defun pdf-dictionary-p (value)
  "True when VALUE is a dictionary: a list of (NAME . VALUE), each NAME
beginning with a /."
  (and (consp value) (not (eq (first value) :ref))
       (every (lambda (pair) (and (consp pair) (stringp (car pair)) (plusp (length (car pair))) (char= #\/ (char (car pair) 0))))
              value)))

(defun pdf-get (dictionary key)
  "The value of KEY (a name with its /) in DICTIONARY, or NIL; NIL too when
DICTIONARY is none."
  (and (pdf-dictionary-p dictionary) (cdr (assoc key dictionary :test #'string=))))

(defun pdf-array-items (value)
  "VALUE as a list: its items when it is an array, else a list of it alone;
NIL for NIL."
  (cond ((null value) nil)
        ((and (listp value) (not (pdf-ref-p value)) (not (pdf-dictionary-p value))) value)
        (t (list value))))

;;; Objects as the file writes them

(defstruct (pdf-indirect (:copier nil))
  "An object of a PDF as the file writes it (NUMBER GENERATION obj ...
endobj): its number, generation and value, where it and its stream lie, and
the object stream holding it, each read by PDF-INDIRECT-GET; CONFIRMED, true
when its stream's /Length reaches an endstream, so that where the stream
ends is known, not guessed."
  number generation value at end stream-start stream-end length packed-in confirmed)

(setf (documentation 'pdf-indirect-p 'function) "True when OBJECT is a PDF-INDIRECT.")

(declaim (inline pdf-indirect-get))
(defun pdf-indirect-get (object field)
  "FIELD of OBJECT, an object of a PDF as the file writes it (from
PDF-INDIRECT-OBJECT-AT, MAP-PDF-WRITTEN-OBJECTS or PDF-PACKED-OBJECT):
:NUMBER and :GENERATION, its object number and generation; :VALUE, its
value as PDF-OBJECT-AT reads it; :AT, where it begins (its number), and
:END, where it ends (after endobj); :STREAM-START and :STREAM-END, where the
octets of its stream lie in the file, or NIL for one with none; :LENGTH, the
length its dictionary gives its stream, as written; :PACKED-IN, the number
of the object stream holding it, :AT and :END then counting in that
stream's decoded octets, else NIL."
  (ecase field
    (:number (pdf-indirect-number object))
    (:generation (pdf-indirect-generation object))
    (:value (pdf-indirect-value object))
    (:at (pdf-indirect-at object))
    (:end (pdf-indirect-end object))
    (:stream-start (pdf-indirect-stream-start object))
    (:stream-end (pdf-indirect-stream-end object))
    (:length (pdf-indirect-length object))
    (:packed-in (pdf-indirect-packed-in object))))

(defun pdf-object-header-before (octets p)
  "Where the object whose obj keyword is at P in OCTETS begins (the first
digit of its number), its number and its generation, as three values; NIL
when no NUMBER GENERATION stands before P."
  (let ((i (1- p)))
    (flet ((back-white () (let ((from i)) (loop while (and (>= i 0) (pdf-white-p (aref octets i))) do (decf i)) (< i from)))
           (back-numerals () (let ((to i)) (loop while (and (>= i 0) (<= 48 (aref octets i) 57) (< (- to i) 12)) do (decf i)) (< i to))))
      (when (and (back-white) (back-numerals))
        (let ((generation-start (1+ i)))
          (when (and (back-white) (back-numerals))
            (let ((start (1+ i)))
              (when (or (< i 0) (pdf-white-p (aref octets i)) (pdf-delimiter-p (aref octets i)))
                (values start (pdf-integer-at octets start) (pdf-integer-at octets generation-start))))))))))

(defun pdf-indirect-object-at (octets at)
  "The object written in OCTETS from AT (its number: NUMBER GENERATION obj),
as a PDF-INDIRECT with where its stream lies; NIL when none begins there. A
stream's length is the one its dictionary gives when endstream follows it
there, else as far as endstream; its end is confirmed when that /Length,
written there or in an object written just after the stream, reaches an
endstream."
  (let ((n (length octets)))
    (multiple-value-bind (number after) (pdf-integer-at octets at)
      (multiple-value-bind (generation after) (and number (pdf-integer-at octets after))
        (let ((obj (and generation (pdf-skip-space octets after))))
          (when (and obj (tuatara.io:octets-at-p octets "obj" obj)
                     (or (>= (+ obj 3) n) (pdf-delimiter-p (aref octets (+ obj 3)))))
            (multiple-value-bind (value value-end) (pdf-object-at octets (+ obj 3))
              (let* ((next (pdf-skip-space octets value-end))
                     (object (make-pdf-indirect :number number :generation generation :value value :at at :end value-end)))
                (when (and (tuatara.io:octets-at-p octets "stream" next)
                           (or (>= (+ next 6) n) (member (aref octets (+ next 6)) '(10 13 32))))
                  (let* ((data (pdf-line-end-after octets (+ next 6)))
                         (length (pdf-get value "/Length"))
                         (exact (and (integerp length) (<= 0 length) (<= (+ data length) n)
                                     (tuatara.io:octets-at-p octets "endstream" (pdf-skip-space octets (+ data length)))
                                     (+ data length)))
                         (close (or (and exact (pdf-skip-space octets exact))
                                    (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "endstream"))
                                                              octets :start data)))
                         (data-end (or exact
                                       (and close (let ((e close))
                                                    (when (and (> e data) (= (aref octets (1- e)) 10)) (decf e))
                                                    (when (and (> e data) (= (aref octets (1- e)) 13)) (decf e))
                                                    e))
                                       n)))
                    (setf (pdf-indirect-stream-start object) data (pdf-indirect-stream-end object) data-end
                          (pdf-indirect-length object) length
                          (pdf-indirect-confirmed object)
                          (and (or exact (and close (pdf-length-after-p octets length data close))) t)
                          next (if close (pdf-skip-space octets (+ close 9)) n))))
                (setf (pdf-indirect-end object)
                      (if (tuatara.io:octets-at-p octets "endobj" next) (+ next 6) (max next (pdf-indirect-end object))))
                object))))))))

;;; The file's objects, saves and cross-reference tables

(defun pdf-length-after-p (octets length data close)
  "True when LENGTH, a stream's /Length that is a reference (:REF N G), names
an object written in OCTETS within 4 KB after CLOSE, where the stream's
endstream is, whose value is a number of octets from DATA that reaches an
endstream, as writers put a stream's length after it."
  (when (pdf-ref-p length)
    (let ((n (length octets))
          (mark (tuatara.text:string-octets (format nil "~d ~d obj" (second length) (third length)))))
      (loop for at = (tuatara.io:octets-search mark octets :start close :end (min n (+ close 4096)))
              then (tuatara.io:octets-search mark octets :start (1+ at) :end (min n (+ close 4096)))
            while at
            thereis (and (pdf-white-p (aref octets (1- at)))
                         (let ((value (pdf-object-at octets (+ at (length mark)))))
                           (and (integerp value) (<= 0 value) (<= (+ data value) n)
                                (tuatara.io:octets-at-p octets "endstream"
                                                        (pdf-skip-space octets (+ data value))))))))))


(defun map-pdf-written-objects (function octets)
  "Call FUNCTION with each object written in the PDF in OCTETS, as a
PDF-INDIRECT, in the order written: each found by its NUMBER GENERATION obj,
none in a comment, the octets of each not looked in again, its stream's
among them when its end is confirmed (PDF-INDIRECT-OBJECT-AT). Stop when
FUNCTION returns true, and return that. In a job, it ends when the job is
asked to stop."
  (let ((n (length octets)) (mark (load-time-value (tuatara.text:string-octets "obj"))) (i 0) (count 0)
        (cursor 0) (depth 0) (comment nil) (escape nil))
    (flet ((in-comment-p (to)
             ;; read on from the cursor to TO: in a % comment there, outside a (string)?
             (loop while (< cursor to)
                   do (let ((c (aref octets cursor)))
                        (cond (comment (when (or (= c 10) (= c 13)) (setf comment nil)))
                              ((plusp depth) (cond (escape (setf escape nil))
                                                   ((= c 92) (setf escape t))
                                                   ((= c 40) (incf depth))
                                                   ((= c 41) (decf depth))))
                              ((= c 37) (setf comment t))
                              ((= c 40) (setf depth 1))))
                      (incf cursor))
             comment)
           (read-afresh (from)
             (setf cursor from depth 0 comment nil escape nil)))
      (loop
        (let ((p (tuatara.io:octets-search mark octets :start i)))
          (unless p (return nil))
          (when (zerop (mod (incf count) 4096)) (tuatara.runtime:job-check-stop))
          (let ((at (pdf-object-header-before octets p))
                (next (+ p 3)))
            (when (and at (or (>= (+ p 3) n) (pdf-delimiter-p (aref octets (+ p 3)))) (not (in-comment-p at)))
              (let ((object (pdf-indirect-object-at octets at)))
                (when object
                  (let ((result (funcall function object)))
                    (when result (return result)))
                  (setf next (max next (let ((start (pdf-indirect-stream-start object)))
                                         (if (and start (not (pdf-indirect-confirmed object)))
                                             start
                                             (pdf-indirect-end object)))))
                  (read-afresh next))))
            (setf i next)))))))

(defparameter *pdf-most-eof-lines* 100000
  "The most %%EOF lines of one PDF read (PDF-EOF-LINES): a file holding more
has the rest passed over.")

(defun pdf-eof-lines (octets &optional (objects (pdf-objects-in-file octets)))
  "The %%EOF lines that end the saves of the PDF in OCTETS, as a list of
\(EOF . END), in order: where its %%EOF is, and where its line ends; at
most *PDF-MOST-EOF-LINES* outside confirmed streams are read, those within
one passed over at once. OBJECTS, the objects the file writes in the order
written (as MAP-PDF-WRITTEN-OBJECTS gives them, a vector), say where its
streams are; when they are not given, the file is read for them.
A %%EOF within a stream whose end is confirmed ends nothing (an attached
PDF's own), unless it is the first after a cross-reference section of the
file itself: one the file's chain of sections names (PDF-SECTIONS), or one
within the stream that a startxref there names at its place in the file,
or whose entries name the file's own objects where the file writes them
\(PDF-SECTIONS-WITHIN). Two more values: those kept though they lie within
a stream whose end is not confirmed, each (EOF . NUMBER), NUMBER the
stream's object, a stream that may hide a save or a save cut into a
stream; the streams whose confirmed end such a section gives the lie to,
the section or the save it ends lying within them, each (OFFSET . NUMBER);
and the other confirmed streams, no file kept within the PDF, whose data
holds a %%EOF or object headers (PDF-HOLDS-SAVE-SIGNS-P), each (NUMBER .
EOF), EOF the first %%EOF in its data or NIL: a save may be hidden in
them, though nothing of the file names it."
  (let* ((mark (load-time-value (tuatara.text:string-octets "%%EOF")))
         (holding '()) (outside '()) (kept 0) (embedded nil))
    ;; the %%EOF lines outside confirmed streams, those within each passed over at once
    (loop with at = (tuatara.io:octets-search mark octets)
          for count from 1
          while (and at (< kept *pdf-most-eof-lines*))
          do (when (zerop (logand count 4095)) (tuatara.runtime:job-check-stop))
             (let ((stream (pdf-stream-holding objects at t)))
               (if stream
                   (progn (pushnew stream holding)
                          (setf at (tuatara.io:octets-search mark octets :start (pdf-indirect-stream-end stream))))
                   (progn (push at outside) (incf kept)
                          (setf at (tuatara.io:octets-search mark octets :start (+ at 5)))))))
    (flet ((embedded ()
             (or embedded (setf embedded (pdf-embedded-file-numbers octets objects)))))
      (let* ((sections (append (mapcar #'car (pdf-sections octets))
                               (loop for stream in holding append (pdf-sections-within octets stream (embedded)))))
             (named (remove nil (mapcar (lambda (at) (tuatara.io:octets-search mark octets :start at)) sections)))
             (eofs (sort (remove-duplicates
                          (append outside (remove-if-not (lambda (at) (pdf-stream-holding objects at t)) named)))
                         #'<))
             (unconfirmed (remove-if-not (lambda (o)
                                           (and (pdf-indirect-stream-start o) (not (pdf-indirect-confirmed o))))
                                         objects))
             (lines '()) (doubtful '()) (lying '()))
        (flet ((lie (at stream)
                 (unless (find (pdf-indirect-number stream) lying :key #'cdr)
                   (push (cons at (pdf-indirect-number stream)) lying))))
          (dolist (at sections)
            (let ((stream (pdf-stream-holding objects at t)))
              (when stream (lie at stream))))
          (dolist (at eofs)
            (let ((confirmed (pdf-stream-holding objects at t)))
              (push (cons at (pdf-line-end-after octets (+ at 5))) lines)
              (when confirmed (lie at confirmed))
              (let ((stream (pdf-stream-holding unconfirmed at nil)))
                (when stream (push (cons at (pdf-indirect-number stream)) doubtful))))))
        (values (nreverse lines) (nreverse doubtful) (nreverse lying)
                (loop for object across objects
                      for signs = (and (pdf-indirect-confirmed object)
                                       (not (find (pdf-indirect-number object) lying :key #'cdr))
                                       (pdf-holds-save-signs-p octets object (embedded)))
                      when signs collect (cons (pdf-indirect-number object) (and (integerp signs) signs))))))))

(defparameter *pdf-object-streams-read* (* 64 1024 1024)
  "The most octets of a PDF's object streams decoded while it is read through
\(WITH-PDF-OBJECTS-FOUND); the objects of those past them are not found.")

(defun pdf-embedded-file-numbers (octets objects)
  "The numbers of the streams the file specifications of the PDF in OCTETS
keep as files (their /EF), as a hash table: in the dictionaries of OBJECTS
\(a vector of PDF-INDIRECT) and of the objects their object streams hold,
those past *PDF-OBJECT-STREAMS-READ* octets not read."
  (let ((numbers (make-hash-table)) (read 0))
    (labels ((walk (value depth)
               (cond ((> depth 16))
                     ((pdf-dictionary-p value)
                      (let ((files (pdf-get value "/EF")))
                        (when (pdf-dictionary-p files)
                          (loop for (nil . file) in files
                                for number = (pdf-ref-p file)
                                when number do (setf (gethash number numbers) t))))
                      (loop for (nil . item) in value do (walk item (1+ depth))))
                     ((and (consp value) (not (pdf-ref-p value)))
                      (dolist (item value) (walk item (1+ depth)))))))
      (loop for object across objects
            do (walk (pdf-indirect-value object) 0)
               (when (and (pdf-indirect-stream-start object) (<= read *pdf-object-streams-read*)
                          (equal (pdf-get (pdf-indirect-value object) "/Type") "/ObjStm"))
                 (multiple-value-bind (data offsets) (pdf-object-stream-contents octets object)
                   (when (and data (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "/EF")) data))
                     (incf read (length data))
                     (loop for (nil . at) across offsets
                           do (walk (pdf-object-at data at) 0)))))))
    numbers))

(defun pdf-own-saves-p (octets stream)
  "True when each %%EOF in the data of STREAM, a PDF-INDIRECT of the PDF in
OCTETS, the first 64 of them, follows a startxref there whose number,
counted from where the data begins, names a cross-reference section: the
saves of a file kept within the PDF, not the PDF's own."
  (let ((start (pdf-indirect-stream-start stream)) (end (pdf-indirect-stream-end stream))
        (mark (load-time-value (tuatara.text:string-octets "%%EOF"))))
    (loop for eof = (tuatara.io:octets-search mark octets :start start :end end)
            then (tuatara.io:octets-search mark octets :start (+ eof 5) :end end)
          repeat 64
          while eof
          always (let ((named (pdf-startxref-written octets :start start :end eof)))
                   (and named (pdf-section-at-p octets (+ start named)))))))

(defun pdf-attachment-p (octets stream embedded)
  "True when STREAM, a PDF-INDIRECT of the PDF in OCTETS, is a file kept
within it: a file specification keeps it (its number in EMBEDDED, as
PDF-EMBEDDED-FILE-NUMBERS gives them), and each %%EOF in its data is that
file's own (PDF-OWN-SAVES-P). A stream merely beginning with %PDF- or
calling itself an /EmbeddedFile is none."
  (and (gethash (pdf-indirect-number stream) embedded) (pdf-own-saves-p octets stream)))

(defun pdf-table-names-objects-p (octets at shift)
  "True when the cross-reference table at AT in OCTETS lists two used
entries at least and its entries, at most 50 read, all name objects written
at their offsets moved by SHIFT."
  (let ((matched 0) (strange nil))
    (map-pdf-cross-reference-table
     (lambda (number offset generation kind)
       (declare (ignore generation))
       (when (eq kind :used)
         (let* ((place (+ offset shift))
                (object (and (< -1 place (length octets)) (pdf-indirect-object-at octets place))))
           (if (and object (eql (pdf-indirect-number object) number))
               (incf matched)
               (setf strange t))))
       (or strange (>= matched 50)))
     octets at)
    (and (>= matched 2) (not strange))))

(defun pdf-sections-within (octets stream embedded)
  "The cross-reference sections of the PDF in OCTETS itself written within
STREAM's octets (a PDF-INDIRECT): one a startxref there names at its place
in the file (PDF-OFFSET-BASE added), or a table whose entries, at most 50
read and two at least, all name the file's own objects where the file
writes them. Within a file kept in the PDF (PDF-ATTACHMENT-P, EMBEDDED the
numbers of the streams kept as files), whose own offsets count from where
its data begins, a section that stands where they put it is that file's
own, and none of these. A list of their offsets."
  (let* ((start (pdf-indirect-stream-start stream)) (end (pdf-indirect-stream-end stream))
         (base (pdf-offset-base octets)) (attachment (pdf-attachment-p octets stream embedded)) (found '()))
    (flet ((within-p (at) (and (integerp at) (<= start at) (< at end))))
      (loop for at = (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "startxref")) octets
                                               :start start :end end)
              then (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "startxref")) octets
                                             :start (+ at 9) :end end)
            while at
            do (let ((n (pdf-integer-at octets (+ at 9) end)))
                 (when (and n (within-p (+ n base)) (pdf-section-at-p octets (+ n base))
                            (not (and attachment (pdf-section-at-p octets (+ n start)))))
                   (pushnew (+ n base) found))))
      (loop for at = (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "xref")) octets
                                               :start start :end end)
              then (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "xref")) octets
                                             :start (+ at 4) :end end)
            while at
            do (unless (or (and (>= at 5) (tuatara.io:octets-at-p octets "start" (- at 5))) (member at found))
                 (when (and (pdf-table-names-objects-p octets at 0)
                            (not (and attachment (pdf-table-names-objects-p octets at (- start base)))))
                   (push at found)))))
    found))

(defun pdf-holds-save-signs-p (octets stream embedded)
  "Where the data of STREAM, a PDF-INDIRECT of the PDF in OCTETS that is no
file kept within it (PDF-ATTACHMENT-P, EMBEDDED the numbers of the streams
kept as files), holds a %%EOF, or T when it holds
an object header (N G obj) but no %%EOF; NIL when it holds neither, or is
such a file."
  (let ((start (pdf-indirect-stream-start stream)) (end (pdf-indirect-stream-end stream)))
    (unless (pdf-attachment-p octets stream embedded)
      (or (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "%%EOF")) octets :start start :end end)
          (loop for p = (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "obj")) octets
                                                  :start start :end end)
                  then (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "obj")) octets
                                                 :start (+ p 3) :end end)
                while p
                thereis (let ((at (pdf-object-header-before octets p)))
                          (and at (>= at start)
                               (or (>= (+ p 3) end) (pdf-delimiter-p (aref octets (+ p 3))))
                               t)))))))

(defun pdf-stream-holding (objects at confirmed)
  "The object of OBJECTS (a vector of PDF-INDIRECT in the order written)
whose stream holds AT, the last one beginning before it, when its end is
CONFIRMED or, with CONFIRMED NIL, when it is not; else NIL."
  (let ((low 0) (high (length objects)))
    (loop while (< low high)
          do (let ((middle (floor (+ low high) 2)))
               (if (<= (pdf-indirect-at (aref objects middle)) at) (setf low (1+ middle)) (setf high middle))))
    (let ((object (and (plusp low) (aref objects (1- low)))))
      (and object (pdf-indirect-stream-start object)
           (eq (and (pdf-indirect-confirmed object) t) (and confirmed t))
           (<= (pdf-indirect-stream-start object) at) (< at (pdf-indirect-stream-end object))
           object))))

(defun pdf-startxref-written (octets &key (start 0) (end (length octets)))
  "The number the last startxref from START below END in OCTETS is followed
by, as written, and where that startxref is, as two values (PDF-STARTXREF)."
  (let ((at (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "startxref")) octets
                                     :start start :end end :from-end t)))
    (values (and at (values (pdf-integer-at octets (+ at 9) end))) at)))

(defun pdf-section-at-p (octets at)
  "True when a cross-reference section of the PDF in OCTETS begins at AT: a
table (xref) or a cross-reference stream."
  (and (integerp at) (< -1 at (length octets))
       (or (tuatara.io:octets-at-p octets "xref" (pdf-skip-space octets at))
           (let ((object (pdf-indirect-object-at octets at)))
             (and object (equal (pdf-get (pdf-indirect-value object) "/Type") "/XRef"))))))

(defvar *pdf-offset-base-kept* (cons (sb-ext:make-weak-pointer nil) 0)
  "The PDF PDF-OFFSET-BASE last worked out for, held weakly, and its base.")

(defun pdf-offset-base (octets)
  "Where the offsets the PDF in OCTETS gives (its startxref, /Prev, its
cross-reference entries) are counted from: its %PDF- header, when octets
come before it and its last startxref names a cross-reference section only
counted from there, as readers take such a file; else 0, its first octet."
  (let ((kept *pdf-offset-base-kept*))
    (if (eq (sb-ext:weak-pointer-value (car kept)) octets)
        (cdr kept)
        (let* ((header (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "%PDF-")) octets))
               (named (and header (plusp header) (pdf-startxref-written octets)))
               (base (if (and named (not (pdf-section-at-p octets named)) (pdf-section-at-p octets (+ header named)))
                         header
                         0)))
          (setf *pdf-offset-base-kept* (cons (sb-ext:make-weak-pointer octets) base))
          base))))

(defun pdf-startxref (octets &key (start 0) (end (length octets)))
  "The offset the last startxref from START below END in the PDF in OCTETS
names (where a save's cross-reference section is said to be), counted from
the file's first octet (PDF-OFFSET-BASE), and where that startxref is, as
two values: the offset NIL when it names none that can be read, both NIL
when there is no startxref."
  (multiple-value-bind (named at) (pdf-startxref-written octets :start start :end end)
    (values (and named (+ named (pdf-offset-base octets))) at)))

(defun map-pdf-cross-reference-table (function octets at)
  "Call FUNCTION with each entry of the cross-reference table written in
OCTETS at AT (its xref keyword, white space and comments before it passed
over), in the order written: the object's number, its offset counted from
the file's first octet (PDF-OFFSET-BASE; for a free object, the number of
the next free one), its generation, and :USED or
:FREE. Stop when FUNCTION returns true. NIL when no table is at AT; else,
as three values, how its entries ended: :END, at its trailer or the file's
end; :STOPPED, by FUNCTION; :CUT-SHORT, where the first number and count of
a part of it could not be read; :UNREADABLE, where an entry could not be
read. Then the trailer dictionary written where they ended, or NIL; and
where they ended."
  (let* ((n (length octets)) (i (pdf-skip-space octets at)) (ending :end) (base 0))
    (when (tuatara.io:octets-at-p octets "xref" i)
      (setf base (pdf-offset-base octets))
      (setf i (+ i 4))
      (block sections
        (loop
          (setf i (pdf-skip-space octets i))
          (when (or (>= i n) (tuatara.io:octets-at-p octets "trailer" i)) (return))
          (multiple-value-bind (start after) (pdf-integer-at octets i)
            (multiple-value-bind (count after) (and start (pdf-integer-at octets after))
              (unless count (setf ending :cut-short) (return))
              (setf i after)
              (dotimes (k count)
                (when (zerop (logand k 4095)) (tuatara.runtime:job-check-stop))
                (multiple-value-bind (offset after) (pdf-integer-at octets i)
                  (multiple-value-bind (generation after) (and offset (pdf-integer-at octets after))
                    (let ((kind (and generation (pdf-skip-space octets after))))
                      (unless (and kind (< kind n) (member (aref octets kind) '(102 110)))
                        (setf ending :unreadable)
                        (return-from sections))
                      (when (if (= (aref octets kind) 110)
                                (funcall function (+ start k) (+ offset base) generation :used)
                                (funcall function (+ start k) offset generation :free))
                        (setf ending :stopped)
                        (return-from sections))
                      (setf i (1+ kind))))))))))
      (let ((trailer (and (tuatara.io:octets-at-p octets "trailer" i) (pdf-object-at octets (+ i 7)))))
        (values ending (and (pdf-dictionary-p trailer) trailer) i)))))

(defun map-pdf-cross-reference-stream (function dictionary data)
  "Call FUNCTION with each entry of a cross-reference stream, whose
DICTIONARY is given and whose decoded octets are DATA, in the order written:
the object's number, the entry's type (0 free, 1 written in the file, 2 in
an object stream; 1 when the stream gives no type) and its two fields (an
offset or object stream's number, and a generation or index; 0 when not
given), as written: an offset counts from the PDF's header, so that
PDF-OFFSET-BASE is added to have it in the file. Stop when FUNCTION returns
true. NIL when /W gives no field widths
it can be read by; else how its entries ended: :END, as its /Index (or
/Size) says; :STOPPED, by FUNCTION; :SHORT, its octets ending first."
  (let* ((widths (pdf-get dictionary "/W"))
         (size (pdf-get dictionary "/Size"))
         (index (or (pdf-get dictionary "/Index") (and (integerp size) (list 0 size)))))
    (when (and (listp widths) (= (length widths) 3) (every (lambda (w) (and (integerp w) (<= 0 w 8))) widths)
               (plusp (reduce #'+ widths)))
      (let ((row (reduce #'+ widths)) (at 0))
        (flet ((field (k standard)
                 (let ((w (nth k widths)))
                   (if (zerop w)
                       standard
                       (prog1 (loop with v = 0 for j from at below (+ at w) do (setf v (+ (* v 256) (aref data j)))
                                    finally (return v))
                         (incf at w))))))
          (loop for (start count) on (and (listp index) index) by #'cddr
                while (and (integerp start) (integerp count))
                do (dotimes (k count)
                     (when (zerop (logand k 4095)) (tuatara.runtime:job-check-stop))
                     (when (> (+ at row) (length data))
                       (return-from map-pdf-cross-reference-stream :short))
                     (let* ((type (field 0 1)) (second (field 1 0)) (third (field 2 0)))
                       (when (funcall function (+ start k) type second third)
                         (return-from map-pdf-cross-reference-stream :stopped)))))
          :end)))))

;;; Streams decoded

(defparameter *pdf-largest-stream* (* 16 1024 1024)
  "The most octets one stream of a PDF is decoded to (PDF-STREAM-OCTETS), as
standard: a stream that would decode to more is read no further.")

(defparameter *pdf-picture-filters*
  '("/DCTDecode" "/DCT" "/JPXDecode" "/JBIG2Decode" "/CCITTFaxDecode" "/CCF")
  "The filters of picture data, which PDF-STREAM-OCTETS never undoes: a
stream's decoding ends at one.")

(defun pdf-stream-filters (dictionary)
  "The filters a stream's DICTIONARY names, each (NAME . PARAMETERS), in the
order they are undone."
  (let ((filters (pdf-array-items (pdf-get dictionary "/Filter")))
        (parameters (let ((p (pdf-get dictionary "/DecodeParms"))) (if (pdf-dictionary-p p) (list p) (pdf-array-items p)))))
    (loop for filter in filters for k from 0
          collect (cons filter (let ((p (nth k parameters))) (and (pdf-dictionary-p p) p))))))

(defun pdf-predicted (data parameters)
  "DATA undone of the predictor PARAMETERS (a filter's /DecodeParms) name:
PNG's (10 to 15) row by row, or TIFF's (2); DATA itself for none."
  (let ((predictor (or (pdf-get parameters "/Predictor") 1)))
    (if (not (and (integerp predictor) (>= predictor 2)))
        data
        (let* ((colours (let ((c (pdf-get parameters "/Colors"))) (if (and (integerp c) (<= 1 c 32)) c 1)))
               (bits (let ((b (pdf-get parameters "/BitsPerComponent"))) (if (member b '(1 2 4 8 16)) b 8)))
               (columns (let ((c (pdf-get parameters "/Columns"))) (if (and (integerp c) (<= 1 c 1000000)) c 1)))
               (pixel (max 1 (ceiling (* colours bits) 8)))
               (row (ceiling (* colours bits columns) 8)))
          (if (= predictor 2)
              (let ((out (copy-seq data)))
                (when (= bits 8)
                  (loop for start from 0 below (length out) by row
                        do (loop for k from (+ start pixel) below (min (length out) (+ start row))
                                 do (setf (aref out k) (logand 255 (+ (aref out k) (aref out (- k pixel))))))))
                out)
              (let* ((rows (floor (length data) (1+ row)))
                     (out (make-array (* rows row) :element-type '(unsigned-byte 8) :initial-element 0)))
                (dotimes (r rows out)
                  (let ((kind (aref data (* r (1+ row))))
                        (in (1+ (* r (1+ row))))
                        (at (* r row)))
                    (dotimes (k row)
                      (let* ((x (aref data (+ in k)))
                             (left (if (>= k pixel) (aref out (+ at k (- pixel))) 0))
                             (up (if (plusp r) (aref out (+ at k (- row))) 0))
                             (corner (if (and (plusp r) (>= k pixel)) (aref out (+ at k (- row) (- pixel))) 0)))
                        (setf (aref out (+ at k))
                              (logand 255 (+ x (case kind
                                                 (1 left) (2 up) (3 (floor (+ left up) 2))
                                                 (4 (let* ((p (- (+ left up) corner)) (pa (abs (- p left))) (pb (abs (- p up))) (pc (abs (- p corner))))
                                                      (cond ((and (<= pa pb) (<= pa pc)) left) ((<= pb pc) up) (t corner))))
                                                 (t 0)))))))))))))))

(defun pdf-decoded-out (size limit)
  "An adjustable vector of octets for a stream being decoded, room for about
SIZE of them, at most LIMIT."
  (make-array (max 64 (min limit size)) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun pdf-decoded-put (out octet limit)
  "Put OCTET at the end of OUT (PDF-DECODED-OUT); an error past LIMIT octets."
  (when (>= (fill-pointer out) limit)
    (refuse "it decodes to more than ~:d octets" limit))
  (vector-push-extend octet out))

(defun pdf-ascii-hex-decoded (data limit)
  "DATA, ASCII hex digits ending at >, as the octets they stand for, at most LIMIT of them."
  (let ((out (pdf-decoded-out (floor (length data) 2) limit)) (high nil))
    (loop for c across data
          do (cond ((= c 62) (return))
                   ((digit-char-p (code-char c) 16)
                    (let ((v (digit-char-p (code-char c) 16)))
                      (if high (progn (pdf-decoded-put out (+ (* 16 high) v) limit) (setf high nil)) (setf high v))))))
    (when high (pdf-decoded-put out (* 16 high) limit))
    (coerce out 'tuatara.text:octets)))

(defun pdf-ascii-85-decoded (data limit)
  "DATA, ASCII base-85 ending at ~>, as the octets it stands for, at most LIMIT of them."
  (let ((out (pdf-decoded-out (length data) limit)) (group '()))
    (flet ((flush (count)
             (let ((value 0))
               (dolist (d (reverse group)) (setf value (+ (* value 85) d)))
               (loop repeat (- 5 count) do (setf value (+ (* value 85) 84)))
               (loop for k from 3 downto (- 4 (1- count)) do (pdf-decoded-put out (ldb (byte 8 (* 8 k)) value) limit)))))
      (loop for c across data
            do (cond ((= c 126) (return))
                     ((and (= c 122) (null group)) (dotimes (k 4) (pdf-decoded-put out 0 limit)))
                     ((<= 33 c 117)
                      (push (- c 33) group)
                      (when (= (length group) 5) (flush 5) (setf group '())))))
      (when (> (length group) 1) (flush (length group))))
    (coerce out 'tuatara.text:octets)))

(defun pdf-lzw-decoded (data limit early)
  "DATA, LZW-coded as a PDF codes it (EARLY 1 for the code length to grow one
code early, as standard), as the octets it stands for, at most LIMIT of them."
  (let ((out (pdf-decoded-out (* 3 (length data)) limit))
        (table (make-array 4096 :initial-element nil))
        (next 258) (width 9) (previous nil) (bit-buffer 0) (bit-count 0) (at 0))
    (dotimes (k 256) (setf (aref table k) (make-array 1 :element-type '(unsigned-byte 8) :initial-element k)))
    (loop
      (loop while (and (< bit-count width) (< at (length data)))
            do (setf bit-buffer (logior (ash bit-buffer 8) (aref data at)) bit-count (+ bit-count 8)) (incf at))
      (when (< bit-count width) (return))
      (let ((code (ldb (byte width (- bit-count width)) bit-buffer)))
        (decf bit-count width)
        (setf bit-buffer (ldb (byte bit-count 0) bit-buffer))
        (cond ((= code 256) (setf next 258 width 9 previous nil))
              ((= code 257) (return))
              (t (let ((entry (cond ((aref table code))
                                    ((and previous (= code next)) (concatenate 'tuatara.text:octets previous (subseq previous 0 1)))
                                    (t (return)))))
                   (loop for o across entry do (pdf-decoded-put out o limit))
                   (when (and previous (< next 4096))
                     (setf (aref table next) (concatenate 'tuatara.text:octets previous (subseq entry 0 1)))
                     (incf next))
                   (setf previous entry)
                   (setf width (cond ((>= (+ next early) 2048) 12) ((>= (+ next early) 1024) 11) ((>= (+ next early) 512) 10) (t 9))))))))
    (coerce out 'tuatara.text:octets)))

(defun pdf-run-length-decoded (data limit)
  "DATA, run-length coded, as the octets it stands for, at most LIMIT of them."
  (let ((out (pdf-decoded-out (* 2 (length data)) limit)) (at 0) (n (length data)))
    (loop while (< at n)
          do (let ((length (aref data at)))
               (incf at)
               (cond ((= length 128) (return))
                     ((< length 128) (loop repeat (1+ length) while (< at n) do (pdf-decoded-put out (aref data at) limit) (incf at)))
                     (t (when (< at n) (loop repeat (- 257 length) do (pdf-decoded-put out (aref data at) limit)) (incf at))))))
    (coerce out 'tuatara.text:octets)))

(defun pdf-stream-octets (dictionary raw &key (limit *pdf-largest-stream*))
  "The octets of the stream whose DICTIONARY is given and whose octets in the
file are RAW, its filters undone (Flate, LZW, ASCII hex and base-85, run
length, with their predictors), at most LIMIT of them, as two values: the
octets, and NIL when every filter was undone; the name of the picture's
filter it stopped at (*PDF-PICTURE-FILTERS*), the octets then the picture's
data; or why it could not be decoded, in words, the octets then NIL. A
stream whose data another file keeps is not read. Nothing in it is run."
  (if (pdf-get dictionary "/F")
      (values nil "its data is kept in another file, which is not read")
      (let ((data raw))
        (dolist (filter (pdf-stream-filters dictionary) (values data nil))
          (destructuring-bind (name . parameters) filter
            (when (member name *pdf-picture-filters* :test #'equal)
              (return (values data name)))
            (handler-case
                (reporting-errors ("decoding a stream's ~a" name)
                  (setf data
                        (cond ((member name '("/FlateDecode" "/Fl") :test #'equal)
                               (pdf-predicted (zlib-octets data 0 (length data) :limit limit :errors t) parameters))
                              ((member name '("/LZWDecode" "/LZW") :test #'equal)
                               (pdf-predicted (pdf-lzw-decoded data limit
                                                               (if (eql (pdf-get parameters "/EarlyChange") 0) 0 1))
                                              parameters))
                              ((member name '("/ASCIIHexDecode" "/AHx") :test #'equal)
                               (pdf-ascii-hex-decoded data limit))
                              ((member name '("/ASCII85Decode" "/A85") :test #'equal) (pdf-ascii-85-decoded data limit))
                              ((member name '("/RunLengthDecode" "/RL") :test #'equal)
                               (pdf-run-length-decoded data limit))
                              ((and (equal name "/Crypt")
                                    (member (pdf-get parameters "/Name") '(nil "/Identity") :test #'equal))
                               data)
                              (t (return (values data (format nil "its filter ~a is not one read here" name)))))))
              (error (e) (return (values nil (format nil "it could not be decoded (~a): ~a" name e))))))))))

;;; Object streams

(defun pdf-object-stream-offsets (dictionary data)
  "Where each object an object stream holds begins in DATA, its decoded
octets, as its DICTIONARY's /N and /First and the list before them say: a
vector of (NUMBER . OFFSET), in the order listed, at most 100,000; NIL when
they do not make sense."
  (let ((count (pdf-get dictionary "/N")) (first (pdf-get dictionary "/First")))
    (when (and (integerp count) (integerp first) (<= 0 count) (<= 0 first (length data)))
      (let ((pairs (make-array 0 :adjustable t :fill-pointer 0)) (i 0))
        (loop repeat (min count 100000)
              do (multiple-value-bind (number after) (pdf-integer-at data i first)
                   (multiple-value-bind (offset after) (and number (pdf-integer-at data after first))
                     (unless offset (return))
                     (vector-push-extend (cons number (+ first offset)) pairs)
                     (setf i after))))
        pairs))))

(defun pdf-packed-object (data offsets number &key index in)
  "The object NUMBER an object stream holds, as a PDF-INDIRECT read from
DATA, the stream's decoded octets, by its OFFSETS
(PDF-OBJECT-STREAM-OFFSETS): the one at INDEX among them when it is that
object, else wherever it is; IN is the object stream's own number. NIL when
it holds none."
  (let ((pair (or (and (integerp index) (< -1 index (length offsets)) (eql number (car (aref offsets index))) (aref offsets index))
                  (find number offsets :key #'car))))
    (when (and pair (< (cdr pair) (length data)))
      (multiple-value-bind (value end) (pdf-object-at data (cdr pair))
        (make-pdf-indirect :number number :generation 0 :value value :at (cdr pair) :end end :packed-in in)))))

(defun pdf-object-stream-contents (octets stream)
  "The decoded octets of STREAM, an object stream of the PDF in OCTETS (a
PDF-INDIRECT), and where each object it holds begins in them
\(PDF-OBJECT-STREAM-OFFSETS), as two values; NIL when it cannot be decoded
or does not say where its objects are."
  (let ((dictionary (pdf-indirect-value stream)))
    (multiple-value-bind (data why)
        (pdf-stream-octets dictionary
                           (subseq octets (pdf-indirect-stream-start stream) (pdf-indirect-stream-end stream)))
      (let ((offsets (and data (null why) (pdf-object-stream-offsets dictionary data))))
        (and offsets (values data offsets))))))

(defvar *pdf-objects-found* nil
  "While one PDF is read through (WITH-PDF-OBJECTS-FOUND), what PDF-FIND-OBJECT
knows of it, a hash table: under :WRITTEN, where each object is written, by
\(NUMBER . GENERATION), the last first; under :OBJECT-STREAMS, its object
streams; under :OBJECTS, every object it writes; under :PACKED, the object
stream holding each object, by number; under :ENTRIES and :TRAILER, what its
cross-reference says (PDF-CROSS-REFERENCE), under :DECODED, the object
streams it names, decoded; under :SCANNED, true once an object was found
by scanning the file instead; under :ASKED, how many objects were looked
for; and each object found, by (NUMBER . GENERATION). NIL elsewhere: each
object is then looked for through the whole file.")

(defparameter *pdf-most-objects-found* 200
  "The most objects of one PDF looked for while it is read through
\(WITH-PDF-OBJECTS-FOUND): a reference past them is not followed, so that a
file naming a great many is read in a bounded time.")

(defmacro with-pdf-objects-found (&body body)
  "Run BODY reading one PDF: where its objects are written and which object
streams hold them found once, at the first object looked for, and at most
*PDF-MOST-OBJECTS-FOUND* objects looked for (*PDF-OBJECTS-FOUND*)."
  `(let ((*pdf-objects-found* (make-hash-table :test 'equal)))
     ,@body))

(defun pdf-objects-written (octets)
  "Where each object of the PDF in OCTETS is written, as three values: a
hash table of (NUMBER . GENERATION) to the offsets of their headers, the
last first; its object streams, the first 500, in the order written; and
every object, a vector in the order written. The file is read
once, from its start, by MAP-PDF-WRITTEN-OBJECTS: what a stream's octets
hold is never taken for an object."
  (let ((table (make-hash-table :test 'equal)) (object-streams '()) (count 0)
        (objects (make-array 0 :adjustable t :fill-pointer 0)))
    (map-pdf-written-objects
     (lambda (object)
       (push (pdf-indirect-at object)
             (gethash (cons (pdf-indirect-number object) (pdf-indirect-generation object)) table))
       (vector-push-extend object objects)
       (when (and (< count 500) (pdf-indirect-stream-start object)
                  (equal (pdf-get (pdf-indirect-value object) "/Type") "/ObjStm"))
         (incf count)
         (push object object-streams))
       nil)
     octets)
    (values table (nreverse object-streams) objects)))

(defun pdf-objects-scanned (octets &optional (found (or *pdf-objects-found* (make-hash-table :test 'equal))))
  "FOUND, what is known of the PDF in OCTETS (*PDF-OBJECTS-FOUND* while it is
read through, else a table made now), with where its objects are written
found once (PDF-OBJECTS-WRITTEN): under :WRITTEN, :OBJECT-STREAMS and
:OBJECTS."
  (unless (gethash :written found)
    (multiple-value-bind (written object-streams objects) (pdf-objects-written octets)
      (setf (gethash :written found) written
            (gethash :object-streams found) object-streams
            (gethash :objects found) objects)))
  found)

(defun pdf-objects-in-file (octets)
  "Every object the PDF in OCTETS writes, a vector in the order written,
found once while it is read through (PDF-OBJECTS-SCANNED)."
  (gethash :objects (pdf-objects-scanned octets)))

(defun pdf-objects-packed (octets streams)
  "The object streams of the PDF in OCTETS holding each object, as a hash
table of an object's number to (STREAM DATA OFFSETS INDEX), the first
stream that holds it: those of STREAMS (PDF-OBJECTS-WRITTEN), until
*PDF-OBJECT-STREAMS-READ* octets of them are decoded, and at most a million
objects."
  (let ((table (make-hash-table)) (read 0))
    (dolist (stream streams table)
      (tuatara.runtime:job-check-stop)
      (multiple-value-bind (data offsets) (pdf-object-stream-contents octets stream)
        (when data
          (loop for (number) across offsets
                for index from 0
                while (< (hash-table-count table) 1000000)
                do (unless (gethash number table)
                     (setf (gethash number table) (list stream data offsets index))))
          (when (> (incf read (length data)) *pdf-object-streams-read*)
            (return table)))))))

(defparameter *pdf-most-sections* 64
  "The most cross-reference sections of one PDF followed by their /Prev
\(PDF-SECTIONS).")

(defparameter *pdf-most-entries* 1000000
  "The most cross-reference entries read from one PDF, all its sections
together (PDF-SECTIONS): a file listing more has the rest not read, and
that is said.")

(defun pdf-sections (octets &optional row)
  "The cross-reference sections of the PDF in OCTETS, from the one its last
startxref names back by each trailer's /Prev, at most *PDF-MOST-SECTIONS*,
each read once: a list of (OFFSET . TRAILER), the last first, OFFSET counted
from the file's first octet. With ROW, a function, the entries they list
are read too, cross-reference streams decoded and those a table names by
/XRefStm: ROW is called with each, the last section's first, as NUMBER and
:USED OFFSET GENERATION, :PACKED STREAM INDEX, or :FREE and two NILs. At
most *PDF-MOST-ENTRIES* entries are read; a second value is true when the
sections list more."
  (let ((base (pdf-offset-base octets)) (n (length octets)) (at (pdf-startxref octets))
        (sections '()) (count 0) (too-many nil))
    (labels ((entry (number kind first second)
               (cond ((> (incf count) *pdf-most-entries*) (setf too-many t) t)
                     (row (funcall row number kind first second) nil)))
             (stream-at (offset)
               (let ((object (and (integerp offset) (< -1 offset n) (pdf-indirect-object-at octets offset))))
                 (when (and object (pdf-indirect-stream-start object)
                            (equal (pdf-get (pdf-indirect-value object) "/Type") "/XRef"))
                   (let ((data (and row (pdf-stream-octets (pdf-indirect-value object)
                                                           (subseq octets (pdf-indirect-stream-start object)
                                                                   (pdf-indirect-stream-end object))))))
                     (when data
                       (map-pdf-cross-reference-stream
                        (lambda (number type second third)
                          (case type
                            (1 (entry number :used (+ base second) third))
                            (2 (entry number :packed second third))
                            (t (entry number :free nil nil))))
                        (pdf-indirect-value object) data)))
                   (pdf-indirect-value object)))))
      (loop repeat *pdf-most-sections*
            while (and (not too-many) (integerp at) (< -1 at n) (not (assoc at sections)))
            do (tuatara.runtime:job-check-stop)
               (multiple-value-bind (ending dictionary)
                   (map-pdf-cross-reference-table (lambda (number offset generation kind)
                                                    (if (eq kind :used)
                                                        (entry number :used offset generation)
                                                        (entry number :free nil nil)))
                                                  octets at)
                 (let ((dictionary (if ending
                                       (let ((hidden (pdf-get dictionary "/XRefStm")))
                                         (when (integerp hidden) (stream-at (+ base hidden)))
                                         dictionary)
                                       (stream-at at))))
                   (unless (or ending dictionary) (return))
                   (push (cons at dictionary) sections)
                   (setf at (let ((prev (pdf-get dictionary "/Prev"))) (and (integerp prev) (+ base prev))))))))
    (values (nreverse sections) too-many)))

(defun pdf-cross-reference (octets)
  "What the cross-reference sections of the PDF in OCTETS say (PDF-SECTIONS),
as three values: a hash table of each object's number to (:USED OFFSET
GENERATION), (:PACKED STREAM INDEX) or (:FREE), the last section's word on
it first; the last trailer, or the latest one giving /Info when that does
not; and true when they list more entries than are read. NIL when none of
them can be read."
  (let ((entries (make-hash-table)))
    (multiple-value-bind (sections too-many)
        (pdf-sections octets (lambda (number kind first second)
                               (unless (gethash number entries)
                                 (setf (gethash number entries)
                                       (if (eq kind :free) '(:free) (list kind first second))))))
      (let ((trailers (remove-if-not #'pdf-dictionary-p (mapcar #'cdr sections))))
        (and sections
             (values entries
                     (or (find-if (lambda (trailer) (pdf-get trailer "/Info")) trailers) (first trailers))
                     too-many))))))

(defun pdf-trailer (octets)
  "The PDF in OCTETS' last trailer (PDF-CROSS-REFERENCE), read once while it
is read through; NIL when its cross-reference cannot be read."
  (gethash :trailer (pdf-cross-reference-known octets)))

(defun pdf-cross-reference-known (octets &optional (found (or *pdf-objects-found* (make-hash-table :test 'equal))))
  "FOUND, what is known of the PDF in OCTETS, with its cross-reference read
\(PDF-CROSS-REFERENCE) once: under :ENTRIES, :TRAILER and :TOO-MANY."
  (unless (nth-value 1 (gethash :entries found))
    (multiple-value-bind (entries trailer too-many) (pdf-cross-reference octets)
      (setf (gethash :entries found) entries (gethash :trailer found) trailer (gethash :too-many found) too-many)))
  found)

(defun pdf-object-by-cross-reference (octets found number generation)
  "The object NUMBER GENERATION where the PDF in OCTETS' cross-reference
says it is, as a PDF-INDIRECT; :FREE when it says the object is free; NIL
when it says nothing of it, or it is not where it says."
  (let* ((entries (gethash :entries (pdf-cross-reference-known octets found)))
         (entry (and entries (gethash number entries))))
    (case (first entry)
      (:free :free)
      (:used (destructuring-bind (offset written) (rest entry)
               (let ((object (and (eql written generation) (< -1 offset (length octets))
                                  (pdf-indirect-object-at octets offset))))
                 (and object (eql (pdf-indirect-number object) number) object))))
      (:packed (destructuring-bind (stream-number index) (rest entry)
                 (multiple-value-bind (data offsets) (and (zerop generation)
                                                          (pdf-object-stream-named octets found stream-number))
                   (and data (pdf-packed-object data offsets number :index index :in stream-number))))))))

(defun pdf-object-stream-named (octets found number)
  "The decoded octets of the object stream NUMBER of the PDF in OCTETS and
where its objects begin (PDF-OBJECT-STREAM-CONTENTS), as two values, kept in
FOUND, what is known of the file, so that each is decoded once; NIL when it
is no object stream, cannot be decoded, or the object streams decoded so far
pass *PDF-OBJECT-STREAMS-READ* octets."
  (let ((decoded (or (gethash :decoded found) (setf (gethash :decoded found) (make-hash-table)))))
    (values-list
     (multiple-value-bind (kept known) (gethash number decoded)
       (if known
           kept
           (setf (gethash number decoded)
                 (let ((stream (pdf-find-object octets number 0)))
                   (and stream (null (pdf-indirect-packed-in stream)) (pdf-indirect-stream-start stream)
                        (<= (incf (gethash :decoded-octets found 0)
                                  (- (pdf-indirect-stream-end stream) (pdf-indirect-stream-start stream)))
                            *pdf-object-streams-read*)
                        (multiple-value-list (pdf-object-stream-contents octets stream))))))))))

(defun pdf-find-object (octets number generation)
  "The object NUMBER GENERATION of the PDF in OCTETS, as a PDF-INDIRECT, or
NIL: where its cross-reference sections say it is (PDF-CROSS-REFERENCE);
when they say nothing of it, or cannot be read, the last one of them
written in the file, else the one an object stream holds
\(PDF-OBJECTS-WRITTEN), and then :SCANNED is noted as true in what is
known of the file. While the PDF is read through (WITH-PDF-OBJECTS-FOUND),
where objects are is found once for all, and past *PDF-MOST-OBJECTS-FOUND*
objects looked for, none is found."
  (let ((found (or *pdf-objects-found* (make-hash-table :test 'equal))) (key (cons number generation)))
    (multiple-value-bind (object known) (gethash key found)
      (cond (known object)
            ((> (incf (gethash :asked found 0)) *pdf-most-objects-found*) nil)
            (t (setf (gethash key found)
                     (let ((listed (pdf-object-by-cross-reference octets found number generation)))
                       (cond ((eq listed :free) nil)
                             (listed)
                             (t (setf (gethash :scanned found) t)
                                (pdf-scanned-object octets found number generation))))))))))

(defun pdf-scanned-object (octets found number generation)
  "The object NUMBER GENERATION as the PDF in OCTETS writes it last, else as
an object stream holds it, found by scanning the file (PDF-OBJECTS-SCANNED,
FOUND what is known of it); NIL when it is in neither."
  (pdf-objects-scanned octets found)
  (or (loop for start in (gethash (cons number generation) (gethash :written found))
            thereis (pdf-indirect-object-at octets start))
      (let* ((packed (or (gethash :packed found)
                         (setf (gethash :packed found) (pdf-objects-packed octets (gethash :object-streams found)))))
             (place (gethash number packed)))
        (and place
             (destructuring-bind (stream data offsets index) place
               (pdf-packed-object data offsets number :index index :in (pdf-indirect-number stream)))))))

(defun pdf-resolve (octets value)
  "VALUE, an object of the PDF in OCTETS, with a reference followed once to
what it names (PDF-FIND-OBJECT)."
  (if (pdf-ref-p value)
      (let ((object (pdf-find-object octets (second value) (third value))))
        (and object (pdf-indirect-value object)))
      value))

;;; Content streams

(defparameter *pdf-most-operators* 2000000
  "The most operators of one content stream MAP-PDF-CONTENT reads, as
standard: past them the rest is not read.")

(defun map-pdf-content (function octets &key (limit *pdf-most-operators*))
  "Read OCTETS, a content stream (or a CMap), calling FUNCTION with each
operator met (a string), its operands (a list, in order: numbers, names as
strings with their /, strings as octets, arrays as lists, dictionaries as
alists, :TRUE, :FALSE and :NULL), where its first operand begins (or the
operator, when it has none) and where the operator ends. An inline picture
\(BI ... ID ... EI) is given as the operator BI with its dictionary as its
one operand, its data passed over. At most LIMIT operators; returns how many
there were. Nothing in it is run. In a job, it ends when the job is asked to
stop."
  (let ((n (length octets)) (i 0) (stack '()) (depth 0) (count 0) (from nil))
    (labels ((value (i depth)
               ;; one operand at I, and where it ends
               (let ((c (aref octets i)))
                 (cond ((= c 40) (pdf-literal-string-at octets i t))
                       ((and (= c 60) (< (1+ i) n) (= (aref octets (1+ i)) 60))
                        (pdf-object-at octets i))
                       ((= c 60) (pdf-hex-string-at octets i t))
                       ((= c 47) (multiple-value-bind (name end) (pdf-object-at octets i) (values name end)))
                       ((= c 91)
                        (let ((items '()) (j (1+ i)))
                          (loop
                            (setf j (pdf-skip-space octets j))
                            (when (or (>= j n) (> depth 16)) (return))
                            (when (= (aref octets j) 93) (incf j) (return))
                            (multiple-value-bind (item end) (value j (1+ depth))
                              (when (<= end j) (return))
                              (setf j end)
                              (unless (eq item :skip) (push item items))))
                          (values (nreverse items) j)))
                       ((or (<= 48 c 57) (member c '(43 45 46)))
                        (multiple-value-bind (number end) (pdf-number-at octets i n)
                          (values (if (numberp number) number :skip) (max end (1+ i)))))
                       ((pdf-delimiter-p c) (values :skip (1+ i)))
                       (t (let ((j i))
                            (loop while (and (< j n) (not (pdf-delimiter-p (aref octets j)))) do (incf j))
                            (values (tuatara.text:octets-latin-1 octets :start i :end j) j))))))
             (inline-image (i)
               ;; after BI: its keys and values to ID, then its data to EI
               (let ((pairs '()) (j i))
                 (loop
                   (setf j (pdf-skip-space octets j))
                   (when (>= j n) (return))
                   (when (and (tuatara.io:octets-at-p octets "ID" j)
                              (or (>= (+ j 2) n) (pdf-white-p (aref octets (+ j 2)))))
                     (incf j 3) (return))
                   (multiple-value-bind (key end) (value j 0)
                     (when (or (<= end j) (>= (pdf-skip-space octets end) n)) (return))
                     (multiple-value-bind (v after) (value (pdf-skip-space octets end) 0)
                       (when (<= after end) (return))
                       (when (stringp key) (push (cons key v) pairs))
                       (setf j after))))
                 (let ((close (loop with mark = (load-time-value (tuatara.text:string-octets "EI"))
                                    for at = (tuatara.io:octets-search mark octets :start j)
                                      then (tuatara.io:octets-search mark octets :start (+ at 2))
                                    while at
                                    when (and (> at 0) (pdf-white-p (aref octets (1- at)))
                                              (or (>= (+ at 2) n) (pdf-white-p (aref octets (+ at 2)))
                                                  (pdf-delimiter-p (aref octets (+ at 2)))))
                                      return at)))
                   (values (nreverse pairs) (if close (+ close 2) n))))))
      (loop
        (setf i (pdf-skip-space octets i))
        (when (or (>= i n) (>= count limit)) (return))
        (multiple-value-bind (item end) (value i 0)
          (setf end (max end (1+ i)))
          (unless (or from (eq item :skip)) (setf from i))
          (flet ((operand (x)
                   (push x stack)
                   (when (> (incf depth) 100000) (setf stack '() depth 0)))
                 (counted ()
                   (when (zerop (logand (incf count) 4095)) (tuatara.runtime:job-check-stop))))
            (cond ((eq item :skip))
                  ((and (stringp item) (plusp (length item)) (char/= #\/ (char item 0)))
                   (cond ((string= item "true") (operand :true))
                         ((string= item "false") (operand :false))
                         ((string= item "null") (operand :null))
                         ((string= item "BI")
                          (multiple-value-bind (dict after) (inline-image end)
                            (counted)
                            (funcall function "BI" (list dict) from after)
                            (setf stack '() depth 0 from nil end after)))
                         (t (counted)
                            (funcall function item (reverse stack) from end)
                            (setf stack '() depth 0 from nil))))
                  (t (operand item))))
          (setf i end))))
    count))
