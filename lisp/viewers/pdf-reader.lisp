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
the object stream holding it, each read by PDF-INDIRECT-GET."
  number generation value at end stream-start stream-end length packed-in)

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
there, else as far as endstream."
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
                          next (if close (pdf-skip-space octets (+ close 9)) n))))
                (setf (pdf-indirect-end object)
                      (if (tuatara.io:octets-at-p octets "endobj" next) (+ next 6) (max next (pdf-indirect-end object))))
                object))))))))

;;; The file's objects, saves and cross-reference tables

(defun map-pdf-written-objects (function octets)
  "Call FUNCTION with each object written in the PDF in OCTETS, as a
PDF-INDIRECT, in the order written: each found by its NUMBER GENERATION obj,
its own octets (its stream among them) not looked in again. Stop when
FUNCTION returns true, and return that. In a job, it ends when the job is
asked to stop."
  (let ((n (length octets)) (mark (load-time-value (tuatara.text:string-octets "obj"))) (i 0) (count 0))
    (loop
      (let ((p (tuatara.io:octets-search mark octets :start i)))
        (unless p (return nil))
        (when (zerop (mod (incf count) 4096)) (tuatara.runtime:job-check-stop))
        (let ((at (pdf-object-header-before octets p))
              (next (+ p 3)))
          (when (and at (or (>= (+ p 3) n) (pdf-delimiter-p (aref octets (+ p 3)))))
            (let ((object (pdf-indirect-object-at octets at)))
              (when object
                (let ((result (funcall function object)))
                  (when result (return result)))
                (setf next (max next (pdf-indirect-end object))))))
          (setf i next))))))

(defun pdf-eof-lines (octets)
  "The %%EOF lines of the PDF in OCTETS, each of which ends a save of it, as
a list of (EOF . END), in order: where its %%EOF is, and where its line
ends."
  (let ((mark (load-time-value (tuatara.text:string-octets "%%EOF"))))
    (loop for at = (tuatara.io:octets-search mark octets)
            then (tuatara.io:octets-search mark octets :start (+ at 5))
          while at
          collect (cons at (pdf-line-end-after octets (+ at 5))))))

(defun pdf-startxref (octets &key (start 0) (end (length octets)))
  "The offset the last startxref from START below END in the PDF in OCTETS
names (where a save's cross-reference section is said to be), and where
that startxref is, as two values: the offset NIL when it names none that can
be read, both NIL when there is no startxref."
  (let ((at (tuatara.io:octets-search (load-time-value (tuatara.text:string-octets "startxref")) octets
                                     :start start :end end :from-end t)))
    (values (and at (values (pdf-integer-at octets (+ at 9) end))) at)))

(defun map-pdf-cross-reference-table (function octets at)
  "Call FUNCTION with each entry of the cross-reference table written in
OCTETS at AT (its xref keyword, white space and comments before it passed
over), in the order written: the object's number, its offset (for a free
object, the number of the next free one), its generation, and :USED or
:FREE. Stop when FUNCTION returns true. NIL when no table is at AT; else,
as three values, how its entries ended: :END, at its trailer or the file's
end; :STOPPED, by FUNCTION; :CUT-SHORT, where the first number and count of
a part of it could not be read; :UNREADABLE, where an entry could not be
read. Then the trailer dictionary written where they ended, or NIL; and
where they ended."
  (let* ((n (length octets)) (i (pdf-skip-space octets at)) (ending :end))
    (when (tuatara.io:octets-at-p octets "xref" i)
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
                      (when (funcall function (+ start k) offset generation (if (= (aref octets kind) 110) :used :free))
                        (setf ending :stopped)
                        (return-from sections))
                      (setf i (1+ kind))))))))))
      (let ((trailer (and (tuatara.io:octets-at-p octets "trailer" i) (pdf-object-at octets (+ i 7)))))
        (values ending (and (pdf-dictionary-p trailer) trailer) i)))))

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
streams; under :PACKED, the object stream holding each object, by number;
under :ASKED, how many objects were looked for; and each object found, by
\(NUMBER . GENERATION). NIL elsewhere: each object is then looked for
through the whole file.")

(defparameter *pdf-most-objects-found* 200
  "The most objects of one PDF looked for while it is read through
\(WITH-PDF-OBJECTS-FOUND): a reference past them is not followed, so that a
file naming a great many is read in a bounded time.")

(defparameter *pdf-object-streams-read* (* 64 1024 1024)
  "The most octets of a PDF's object streams decoded while it is read through
\(WITH-PDF-OBJECTS-FOUND); the objects of those past them are not found.")

(defmacro with-pdf-objects-found (&body body)
  "Run BODY reading one PDF: where its objects are written and which object
streams hold them found once, at the first object looked for, and at most
*PDF-MOST-OBJECTS-FOUND* objects looked for (*PDF-OBJECTS-FOUND*)."
  `(let ((*pdf-objects-found* (make-hash-table :test 'equal)))
     ,@body))

(defun pdf-objects-written (octets)
  "Where each object of the PDF in OCTETS is written, as two values: a hash
table of (NUMBER . GENERATION) to the offsets of their headers, the last
first; and its object streams, the first 500, in the order written. The
file is read once, from its start, by MAP-PDF-WRITTEN-OBJECTS: what a
stream's octets hold is never taken for an object."
  (let ((table (make-hash-table :test 'equal)) (streams '()) (count 0))
    (map-pdf-written-objects
     (lambda (object)
       (push (pdf-indirect-at object)
             (gethash (cons (pdf-indirect-number object) (pdf-indirect-generation object)) table))
       (when (and (< count 500) (pdf-indirect-stream-start object)
                  (equal (pdf-get (pdf-indirect-value object) "/Type") "/ObjStm"))
         (incf count)
         (push object streams))
       nil)
     octets)
    (values table (nreverse streams))))

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

(defun pdf-find-object (octets number generation)
  "The object NUMBER GENERATION of the PDF in OCTETS, as a PDF-INDIRECT: the
last one of them written in the file, else the one an object stream holds;
NIL when it is in neither. The file's cross-reference sections are not
read: each object is found where it is written (PDF-OBJECTS-WRITTEN). While
the PDF is read through (WITH-PDF-OBJECTS-FOUND), where objects are is found
once for all, and past *PDF-MOST-OBJECTS-FOUND* objects looked for, none is
found."
  (let ((found (or *pdf-objects-found* (make-hash-table :test 'equal))) (key (cons number generation)))
    (multiple-value-bind (object known) (gethash key found)
      (cond (known object)
            ((> (incf (gethash :asked found 0)) *pdf-most-objects-found*) nil)
            (t (unless (gethash :written found)
                 (multiple-value-bind (written streams) (pdf-objects-written octets)
                   (setf (gethash :written found) written
                         (gethash :object-streams found) streams)))
               (setf (gethash key found)
                     (or (loop for start in (gethash key (gethash :written found))
                               thereis (pdf-indirect-object-at octets start))
                         (let* ((packed (or (gethash :packed found)
                                            (setf (gethash :packed found)
                                                  (pdf-objects-packed octets (gethash :object-streams found)))))
                                (place (gethash number packed)))
                           (and place
                                (destructuring-bind (stream data offsets index) place
                                  (pdf-packed-object data offsets number :index index
                                                                         :in (pdf-indirect-number stream))))))))))))

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
