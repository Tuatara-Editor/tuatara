;;;; src/text/octets.lisp --- Numbers, ASCII and base64 read from and written into vectors of octets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What reading a file's fields needs: integers of any width and order,
;;;; a prefix tested, ASCII and base64 read, and octets sought in octets,
;;;; quick on the simple vectors files are read into.

(in-package #:tuatara.text)

(defun octet-at (octets i)
  "The octet at I of OCTETS, or NIL past their end."
  (and (< -1 i (length octets)) (aref octets i)))

(defun octets-integer (octets at count &key big-endian)
  "The unsigned integer in COUNT octets of OCTETS from AT, least significant
first unless BIG-ENDIAN; NIL when they are not all there."
  (and (<= 0 at) (<= (+ at count) (length octets))
       (loop for k below count
             sum (ash (aref octets (+ at k)) (* 8 (if big-endian (- count k 1) k))))))

(defun octets-signed (value bits)
  "VALUE, an unsigned integer of BITS bits, as the signed one it stands for; NIL for NIL."
  (and value (if (logbitp (1- bits) value) (- value (ash 1 bits)) value)))

(declaim (inline octets-put-integer))
(defun octets-put-integer (octets value count &key at big-endian)
  "Write the unsigned integer VALUE as COUNT octets, least significant first
unless BIG-ENDIAN: into OCTETS from AT, or without AT at their end, OCTETS
then a vector with a fill pointer that grows. Returns OCTETS."
  (dotimes (k count octets)
    (let ((octet (ldb (byte 8 (* 8 (if big-endian (- count k 1) k))) value)))
      (if at
          (setf (aref octets (+ at k)) octet)
          (vector-push-extend octet octets)))))

(defun octets-at-p (octets prefix &optional (at 0))
  "True when OCTETS hold PREFIX, a string of ASCII or a list of octets, at AT."
  (let ((prefix (if (stringp prefix) (map 'list #'char-code prefix) prefix)))
    (and (<= 0 at) (<= (+ at (length prefix)) (length octets))
         (loop for o in prefix for i from at always (= o (aref octets i))))))

(defun base64-value (octet)
  "The six bits OCTET stands for in base64, or NIL for an octet that is not one of its letters."
  (cond ((<= 65 octet 90) (- octet 65)) ((<= 97 octet 122) (- octet 71)) ((<= 48 octet 57) (+ octet 4))
        ((= octet 43) 62) ((= octet 47) 63) (t nil)))

(defun base64-octets (octets &key (start 0) (end (length octets)))
  "The octets base64 in OCTETS from START below END stands for: what is not
of its letters (line ends, spaces) passed over, and a = ending it."
  (let ((out (make-array (floor (* 3 (- end start)) 4) :element-type '(unsigned-byte 8)))
        (filled 0) (bits 0) (count 0))
    (loop for i from start below end
          for octet = (aref octets i)
          until (= octet 61)
          do (let ((value (base64-value octet)))
               (when value
                 (setf bits (logior (ash bits 6) value))
                 (incf count)
                 (when (= count 4)
                   (setf (aref out filled) (ldb (byte 8 16) bits)
                         (aref out (+ filled 1)) (ldb (byte 8 8) bits)
                         (aref out (+ filled 2)) (ldb (byte 8 0) bits))
                   (incf filled 3)
                   (setf bits 0 count 0)))))
    ;; what a last group cut short holds
    (case count
      (2 (setf (aref out filled) (ldb (byte 8 4) bits)) (incf filled 1))
      (3 (setf (aref out filled) (ldb (byte 8 10) bits) (aref out (1+ filled)) (ldb (byte 8 2) bits)) (incf filled 2)))
    (subseq out 0 filled)))

(defparameter *base64-digits*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The 64 letters of base64, each standing for six bits, in order.")

(defun base64-string (octets &key (start 0) (end (length octets)))
  "The octets of OCTETS from START below END in base64, = filling its last
group: what BASE64-OCTETS reads back."
  (let* ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (count (- end start))
         (out (make-string (* 4 (ceiling count 3))))
         (digits *base64-digits*))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type simple-string digits out)
             (fixnum start end count))
    (loop for i fixnum from start below end by 3
          for o fixnum from 0 by 4
          do (let* ((left (- end i))
                    (n (logior (ash (aref octets i) 16)
                               (if (> left 1) (ash (aref octets (+ i 1)) 8) 0)
                               (if (> left 2) (aref octets (+ i 2)) 0))))
               (setf (schar out o) (schar digits (ldb (byte 6 18) n))
                     (schar out (+ o 1)) (schar digits (ldb (byte 6 12) n))
                     (schar out (+ o 2)) (if (> left 1) (schar digits (ldb (byte 6 6) n)) #\=)
                     (schar out (+ o 3)) (if (> left 2) (schar digits (ldb (byte 6 0) n)) #\=))))
    out))

(defun octets-ascii (octets start end)
  "The octets of OCTETS from START below END (or their end) as ASCII, a
character standing for each; NIL when START is past their end."
  (let ((end (min end (length octets))))
    (and (< start end) (octets-latin-1 octets :start start :end end))))

(defun octets-search (needle octets &key (start 0) (end (length octets)) from-end)
  "Where NEEDLE, a vector of octets, first stands in OCTETS from START below
END (with FROM-END, the last place it stands), or NIL. START below 0 counts
as 0 and END past the octets as their end; when END is then before START,
or too near it for NEEDLE, the answer is NIL, never an error. An empty
NEEDLE stands at START (at END, with FROM-END). Quick on simple vectors of
octets, as files are read, so that a file of hundreds of megabytes is
searched in well under a second."
  (let* ((end (min end (length octets))) (start (max 0 start)))
    (cond ((< end start) nil)
          ((zerop (length needle)) (if from-end end start))
          ((> (+ start (length needle)) end) nil)
          ((and (typep needle 'octets) (typep octets 'octets))
           (octets-search-simple needle octets start end from-end))
          (t (search needle octets :start2 start :end2 end :from-end from-end)))))

(defun octets-search-simple (needle octets start end from-end)
  "OCTETS-SEARCH's work on simple vectors of octets, NEEDLE not empty and
fitting between START and END."
  (declare (type octets needle octets) (type fixnum start end) (optimize speed (safety 0)))
  (let ((n (length needle)) (first (aref needle 0)))
    (declare (type fixnum n))
    (flet ((match-p (i)
             (declare (type fixnum i))
             (loop for k of-type fixnum from 1 below n always (= (aref octets (+ i k)) (aref needle k)))))
      (declare (inline match-p))
      (if from-end
          (loop for i of-type fixnum from (- end n) downto start
                when (and (= (aref octets i) first) (match-p i)) return i)
          (loop for i of-type fixnum from start to (- end n)
                when (and (= (aref octets i) first) (match-p i)) return i)))))

(defun octets-find (octets needle &key (start 0) (end (length octets)))
  "Where the ASCII string NEEDLE first stands in OCTETS from START below END, or NIL."
  (octets-search (string-octets needle) octets :start start :end end))
