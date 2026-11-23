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
  "True when OCTETS hold PREFIX, a string of ASCII or a list of octets, at AT.
Nothing is made: a file's kind is told by many such tests."
  (and (<= 0 at) (<= (+ at (length prefix)) (length octets))
       (if (stringp prefix)
           (loop for c across prefix for i from at always (= (char-code c) (aref octets i)))
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

(defun base64-string (octets &key (start 0) (end (length octets)) line-length)
  "The octets of OCTETS from START below END in base64, = filling its last
group: what BASE64-OCTETS reads back. With LINE-LENGTH, a number of
characters (76 for mail, RFC 2045 6.8), the text is broken into lines of that
many, each ended by a line feed, the last one too."
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
    (if line-length
        (with-output-to-string (lines)
          (loop for from from 0 below (length out) by line-length
                do (write-string out lines :start from :end (min (length out) (+ from line-length)))
                   (write-char #\Newline lines)))
        out)))

(defun octets-ascii (octets start end)
  "The octets of OCTETS from START below END (or their end) as text, a
character standing for each, as Latin-1 reads them (ASCII for octets below
128); NIL when START is not within them."
  (let ((end (min end (length octets))))
    (and (<= 0 start) (< start end) (octets-latin-1 octets :start start :end end))))

(sb-ext:defglobal *fold-table*
    (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (b 256 table)
        (setf (aref table b) (if (<= 65 b 90) (+ b 32) b))))
  "ASCII case folding for octets: upper-case letters mapped to lower case.")
(declaim (type (simple-array (unsigned-byte 8) (256)) *fold-table*))

(declaim (inline needle-at-p))
(defun needle-at-p (chunk i needle case-fold)
  "True when NEEDLE, its octets ASCII case folded when CASE-FOLD, stands in
CHUNK at index I, whose octet matches its first already."
  (declare (type octets chunk needle) (type fixnum i) (optimize speed (safety 0)))
  (let ((table *fold-table*))
    (loop for k of-type fixnum from 1 below (length needle)
          always (= (if case-fold (aref table (aref chunk (+ i k))) (aref chunk (+ i k)))
                    (aref needle k)))))

(defconstant +near-octets+ 32
  "How many octets on a search looks at one by one for where its needle's
first octet stands, before it hands the rest to POSITION: a needle that
begins with a common octet finds it nearby without a call, and a long
stretch without it is gone through many octets at a time.")

(defun scan-chunk (chunk needle case-fold from below &optional (filled (length chunk)) other-case)
  "Index of the first occurrence of NEEDLE in the first FILLED octets of CHUNK
starting at or after FROM and before BELOW (the start index limit), or NIL.
Both are octet vectors. The places its first octet stands are found one
octet at a time nearby (+NEAR-OCTETS+), else by POSITION, which looks at many
octets at a time. The rest of it is compared there. Ignoring case, a letter's
other case is looked for no further than the first case's next place, so the
first found is the first there is, and no octet is looked at twice.
OTHER-CASE, a cons kept by a caller that searches the same CHUNK again from
where it left off, holds where the other case stands next and how far it was
looked for, so that it is looked for once for the whole chunk."
  (declare (type octets chunk needle) (type fixnum from below filled) (optimize speed (safety 0)))
  (let ((n (length needle)))
    (declare (type fixnum n))
    (when (zerop n) (return-from scan-chunk from))
    (let* ((first (aref needle 0))
           (other (and case-fold (<= 97 first 122) (- first 32)))
           (limit (min below (- (1+ filled) n))))
      (declare (type (unsigned-byte 8) first) (type fixnum limit))
      (when (>= from limit) (return-from scan-chunk nil))
      (flet ((next (octet start end)
               (declare (type (unsigned-byte 8) octet) (type fixnum start end))
               (let ((near (min end (+ start +near-octets+))))
                 (declare (type fixnum near))
                 (loop for k of-type fixnum from start below near
                       when (= (aref chunk k) octet) do (return-from next k))
                 (and (< near end) (position octet chunk :start near :end end)))))
        ;; A, the first case's next place; B, the other case's, looked for only as far
        ;; as A (to LOOKED), so that no octet is looked at twice, nor further than needed
        (let* ((a (next first from limit))
               (kept (and other-case (cdr other-case) (>= (cdr other-case) from)))
               (b (and kept (car other-case) (>= (car other-case) from) (car other-case)))
               (looked (if kept (cdr other-case) from)))
          (declare (type fixnum looked))
          (flet ((done (at)
                   (when other-case (setf (car other-case) b (cdr other-case) looked))
                   (return-from scan-chunk at)))
            (loop
              ;; with OTHER-CASE kept, as far as the chunk: looked for once for all of it
              (let ((end (if other-case limit (or a limit))))
                (when (and other (null b) (< looked end))
                  (setf b (next other looked end)
                        looked (if b (1+ b) end))))
              (let ((at (if (and b (or (null a) (< b a))) b a)))
                (unless at (done nil))
                (when (needle-at-p chunk at needle case-fold) (done at))
                (if (eql at a)
                    (setf a (next first (1+ a) limit))
                    (setf b nil))))))))))

(defun scan-chunk-backward (chunk needle case-fold from &optional (filled (length chunk)) (floor 0))
  "Index of the last occurrence of NEEDLE in the first FILLED octets of CHUNK
starting at or before FROM and at or after FLOOR, or NIL. As SCAN-CHUNK, backwards: the places its
first octet stands found nearby one by one, else by POSITION from the end, a
letter's other case looked for no further back than the first case's next
place, the nearer to FROM taken first."
  (declare (type octets chunk needle) (type fixnum from filled floor) (optimize speed (safety 0)))
  (let ((n (length needle)))
    (declare (type fixnum n))
    (when (zerop n) (return-from scan-chunk-backward from))
    (let* ((first (aref needle 0))
           (other (and case-fold (<= 97 first 122) (- first 32)))
           (top (min from (- filled n))))
      (declare (type (unsigned-byte 8) first) (type fixnum top))
      (when (< top floor) (return-from scan-chunk-backward nil))
      (flet ((previous (octet below lowest)
               (declare (type (unsigned-byte 8) octet) (type fixnum below lowest))
               (let ((near (max lowest (- below +near-octets+))))
                 (declare (type fixnum near))
                 (loop for k of-type fixnum from (1- below) downto near
                       when (= (aref chunk k) octet) do (return-from previous k))
                 (and (> near lowest) (position octet chunk :start lowest :end near :from-end t)))))
        ;; as SCAN-CHUNK, backwards: B looked for only down to A (to LOOKED)
        (let ((a (previous first (1+ top) floor)) (b nil) (looked (1+ top)))
          (declare (type fixnum looked))
          (loop
            (let ((lowest (if a (1+ a) floor)))
              (when (and other (null b) (> looked lowest))
                (setf b (previous other looked lowest)
                      looked (or b lowest))))
            (let ((at (if (and b (or (null a) (> b a))) b a)))
              (unless at (return nil))
              (when (needle-at-p chunk at needle case-fold) (return at))
              (if (eql at a)
                  (setf a (previous first a floor))
                  (setf b nil)))))))))

(defun octets-search (needle octets &key (start 0) (end (length octets)) from-end)
  "Where NEEDLE, a vector of octets, first stands in OCTETS from START below
END (with FROM-END, the last place it stands), or NIL. START below 0 counts
as 0 and END past the octets as their end; when END is then before START,
or too near it for NEEDLE, the answer is NIL, never an error. An empty
NEEDLE stands at START (at END, with FROM-END). Quick on simple vectors of
octets, as files are read, so that a file of hundreds of megabytes is
searched in well under a second (SCAN-CHUNK)."
  (let* ((end (min end (length octets))) (start (max 0 start)))
    (cond ((< end start) nil)
          ((zerop (length needle)) (if from-end end start))
          ((> (+ start (length needle)) end) nil)
          ((and (typep needle 'octets) (typep octets 'octets))
           (if from-end
               (scan-chunk-backward octets needle nil (- end (length needle)) end start)
               (scan-chunk octets needle nil start end end)))
          (t (search needle octets :start2 start :end2 end :from-end from-end)))))

(defun octets-find (octets needle &key (start 0) (end (length octets)))
  "Where the ASCII string NEEDLE first stands in OCTETS from START below END, or NIL."
  (octets-search (string-octets needle) octets :start start :end end))
