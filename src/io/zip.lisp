;;;; src/io/zip.lisp --- Zip archives written and read, as a Word document (.docx) is one; CRC-32.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A .docx file is a zip archive of XML files and pictures. ZIP-OCTETS
;;;; writes one, each file stored as it is (zip's method 0, which every reader
;;;; takes), its name in UTF-8, its time the same always, so that the same
;;;; document gives the same octets. ZIP-ENTRIES and ZIP-ENTRY-OCTETS read
;;;; one, stored or deflated (INFLATE, RFC 1951), each file no larger
;;;; unpacked than *ZIP-LARGEST-ENTRY* nor *ZIP-LARGEST-RATIO* times its
;;;; packed size, its CRC checked; a locked or damaged archive is refused
;;;; with a ZIP-ERROR. INFLATE follows Mark Adler's puff.c.

(in-package #:tuatara.io)

(defvar *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c))))
  "The table of CRC-32, as zip's entries and PNG's chunks are checked by.")

(defun crc32 (octets &optional (start 0) (end (length octets)))
  "The CRC-32 of OCTETS from START below END."
  (let ((c #xFFFFFFFF) (table *crc-table*))
    (declare (type (unsigned-byte 32) c) (type (simple-array (unsigned-byte 32) (256)) table))
    (sb-kernel:with-array-data ((octets octets) (start start) (end end))
      (declare (type (simple-array (unsigned-byte 8) (*)) octets) (optimize speed))
      (loop for i of-type fixnum from start below end
            do (setf c (logxor (aref table (logand (logxor c (aref octets i)) #xFF)) (ash c -8)))))
    (logxor c #xFFFFFFFF)))

(defun zip-octets (entries)
  "A zip archive holding ENTRIES, each (NAME . OCTETS) or (NAME . STRING), a
string as UTF-8, in the order given, stored uncompressed, as octets."
  (let* ((entries (mapcar (lambda (entry)
                            (let ((data (cdr entry)))
                              (cons (tuatara.text:string-octets (car entry))
                                    (if (stringp data) (tuatara.text:string-octets data) data))))
                          entries))
         (out (make-array (+ 22 (loop for (name . data) in entries sum (+ 30 46 (* 2 (length name)) (length data))))
                          :element-type '(unsigned-byte 8)))
         (fill 0)
         (central '()))
    (labels ((put16 (n) (octets-put-integer out n 2 :at fill) (incf fill 2))
             (put32 (n) (octets-put-integer out n 4 :at fill) (incf fill 4))
             (put-octets (octets) (replace out octets :start1 fill) (incf fill (length octets))))
      (loop for (name . data) in entries
            do (let ((crc (crc32 data))
                     (offset fill))
                 ;; the local header: version 2.0, names in UTF-8, stored, 1980-01-01 00:00
                 (put32 #x04034b50) (put16 20) (put16 #x0800) (put16 0) (put16 0) (put16 33)
                 (put32 crc) (put32 (length data)) (put32 (length data)) (put16 (length name)) (put16 0)
                 (put-octets name) (put-octets data)
                 (push (list name crc (length data) offset) central)))
      (let ((start fill))
        (dolist (entry (reverse central))
          (destructuring-bind (name crc size offset) entry
            (put32 #x02014b50) (put16 20) (put16 20) (put16 #x0800) (put16 0) (put16 0) (put16 33)
            (put32 crc) (put32 size) (put32 size) (put16 (length name)) (put16 0) (put16 0)
            (put16 0) (put16 0) (put32 0) (put32 offset)
            (put-octets name)))
        (let ((size (- fill start)))
          (put32 #x06054b50) (put16 0) (put16 0) (put16 (length central)) (put16 (length central))
          (put32 size) (put32 start) (put16 0))))
    out))

;;; Reading: a zip archive's entries, stored or deflated

(defparameter *zip-largest-entry* (* 64 1024 1024)
  "The most octets one entry of a zip archive (a part of a Word document) is
unpacked to; a larger one is refused, so that an archive made to fill the
memory when unpacked cannot.")

(defparameter *zip-largest-ratio* 200
  "The most an entry may grow by when unpacked, as a multiple of what it takes
packed (beyond its first megabyte); more is taken for an archive made to
fill the memory, and refused.")

(define-condition zip-error (error)
  ((reason :initarg :reason :reader zip-error-reason :documentation "Why, in words."))
  (:report (lambda (c stream) (format stream "~a" (zip-error-reason c))))
  (:documentation "A zip archive that cannot be read: damaged, of a kind not read, or too large unpacked."))

(defun zip-fail (format &rest arguments)
  "Signal a ZIP-ERROR saying FORMAT with ARGUMENTS."
  (error 'zip-error :reason (apply #'format nil format arguments)))

(defun zip-entries (octets)
  "The entries of the zip archive OCTETS, from its central directory: each a
plist (:NAME :METHOD :CRC :PACKED :SIZE :DATA), DATA where its packed octets
begin. Signals ZIP-ERROR for what is no zip archive, or one in parts or
locked, which are not read."
  (let* ((n (length octets))
         (end (loop for at downfrom (- n 22) to (max 0 (- n 22 65535))
                    when (and (>= at 0) (= (octets-le octets at 4) #x06054b50)) return at)))
    (unless end (zip-fail "no zip archive: its directory is not at its end"))
    (let ((count (octets-le octets (+ end 10) 2))
          (at (octets-le octets (+ end 16) 4))
          (entries '()))
      (dotimes (i count (nreverse entries))
        (unless (and (<= (+ at 46) n) (= (octets-le octets at 4) #x02014b50))
          (zip-fail "a damaged zip archive: its directory's entry ~d" (1+ i)))
        (let* ((flags (octets-le octets (+ at 8) 2))
               (name-length (octets-le octets (+ at 28) 2))
               (extra (octets-le octets (+ at 30) 2))
               (comment (octets-le octets (+ at 32) 2))
               (local (octets-le octets (+ at 42) 4))
               (name (tuatara.text:octets-string (subseq octets (+ at 46) (min n (+ at 46 name-length))))))
          (when (logbitp 0 flags) (zip-fail "~a in the archive is locked with a password" name))
          (unless (and (<= (+ local 30) n) (= (octets-le octets local 4) #x04034b50))
            (zip-fail "a damaged zip archive: ~a's header" name))
          (push (list :name name :method (octets-le octets (+ at 10) 2) :crc (octets-le octets (+ at 16) 4)
                      :packed (octets-le octets (+ at 20) 4) :size (octets-le octets (+ at 24) 4)
                      :data (+ local 30 (octets-le octets (+ local 26) 2) (octets-le octets (+ local 28) 2)))
                entries)
          (setf at (+ at 46 name-length extra comment)))))))

;;; Inflating, as RFC 1951 says, after Mark Adler's puff

(defparameter *inflate-length-base* #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258)
  "The lengths the length codes 257 to 285 begin at.")
(defparameter *inflate-length-extra* #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
  "The extra bits after each length code.")
(defparameter *inflate-distance-base* #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577)
  "The distances the distance codes 0 to 29 begin at.")
(defparameter *inflate-distance-extra* #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13)
  "The extra bits after each distance code.")
(defparameter *inflate-code-order* #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
  "The order the code lengths of a dynamic block's code lengths come in.")

(defun inflate (octets &key (start 0) (end (length octets)) (limit *zip-largest-entry*) size)
  "The octets the deflated data OCTETS from START below END stand for, at most
LIMIT of them, SIZE of them when that is known. Signals ZIP-ERROR for data
that is damaged or would unpack to more."
  (let ((octets (coerce octets 'tuatara.text:octets))
        (out (tuatara.text:make-octets (min limit (or size (max 1024 (* 4 (- end start)))))))
        (fill 0) (at start) (bit-buffer 0) (bit-count 0))
    (declare (type tuatara.text:octets octets out) (type fixnum fill at end limit bit-count)
             (type (unsigned-byte 32) bit-buffer))
    (labels ((bits (need)
               (declare (type (integer 0 16) need))
               (loop while (< bit-count need)
                     do (when (>= at end) (zip-fail "damaged packed data: it ends too soon"))
                        (setf bit-buffer (logior bit-buffer (ash (aref octets at) bit-count)))
                        (incf at) (incf bit-count 8))
               (prog1 (ldb (byte need 0) bit-buffer)
                 (setf bit-buffer (ash bit-buffer (- need))) (decf bit-count need)))
             (put (o)
               (when (= fill (length out))
                 (when (>= fill limit) (zip-fail "a part unpacks to more than ~:d octets" limit))
                 (setf out (replace (tuatara.text:make-octets (min limit (* 2 (length out)))) out)))
               (setf (aref out fill) o)
               (incf fill))
             (table (lengths)
               ;; canonical Huffman: how many codes of each length, the symbols in code order
               (let ((count (make-array 16 :initial-element 0)) (offsets (make-array 16 :initial-element 0))
                     (symbols (make-array (length lengths) :initial-element 0)))
                 (loop for l across lengths do (incf (aref count l)))
                 (setf (aref count 0) 0)
                 (loop for l from 1 below 15 do (setf (aref offsets (1+ l)) (+ (aref offsets l) (aref count l))))
                 (loop for symbol from 0 for l across lengths
                       when (plusp l) do (setf (aref symbols (aref offsets l)) symbol) (incf (aref offsets l)))
                 (cons count symbols)))
             (decode (h)
               (let ((code 0) (first 0) (index 0) (count (car h)))
                 (loop for l from 1 to 15
                       do (setf code (logior code (bits 1)))
                          (let ((c (aref count l)))
                            (when (< (- code c) first)
                              (return-from decode (aref (cdr h) (+ index (- code first)))))
                            (incf index c) (incf first c)
                            (setf first (ash first 1) code (ash code 1))))
                 (zip-fail "damaged packed data: a code that is none")))
             (codes (lengths-table distances-table)
               (loop
                 (let ((symbol (decode lengths-table)))
                   (cond ((< symbol 256) (put symbol))
                         ((= symbol 256) (return))
                         (t (let ((s (- symbol 257)))
                              (when (>= s 29) (zip-fail "damaged packed data: a length that is none"))
                              (let* ((length (+ (aref *inflate-length-base* s) (bits (aref *inflate-length-extra* s))))
                                     (d (decode distances-table))
                                     (distance (progn (when (>= d 30) (zip-fail "damaged packed data: a distance that is none"))
                                                      (+ (aref *inflate-distance-base* d) (bits (aref *inflate-distance-extra* d))))))
                                (when (> distance fill) (zip-fail "damaged packed data: a distance too far back"))
                                (dotimes (i length) (put (aref out (- fill distance))))))))))))
      (loop
        (let ((last (bits 1)) (type (bits 2)))
          (case type
            (0 (setf bit-buffer 0 bit-count 0)
               (when (> (+ at 4) end) (zip-fail "damaged packed data: a stored block ends too soon"))
               (let ((n (octets-le octets at 2)))
                 (incf at 4)
                 (when (> (+ at n) end) (zip-fail "damaged packed data: a stored block ends too soon"))
                 (loop repeat n do (put (aref octets at)) (incf at))))
            (1 (let ((lengths (make-array 288 :initial-element 0)))
                 (loop for i below 288 do (setf (aref lengths i) (cond ((< i 144) 8) ((< i 256) 9) ((< i 280) 7) (t 8))))
                 (codes (table lengths) (table (make-array 30 :initial-element 5)))))
            (2 (let* ((nlen (+ 257 (bits 5))) (ndist (+ 1 (bits 5))) (ncode (+ 4 (bits 4)))
                      (lengths (make-array 19 :initial-element 0)))
                 (when (or (> nlen 286) (> ndist 30)) (zip-fail "damaged packed data: too many codes"))
                 (dotimes (i ncode) (setf (aref lengths (aref *inflate-code-order* i)) (bits 3)))
                 (let ((lencode (table lengths)) (all (make-array (+ nlen ndist) :initial-element 0)) (i 0))
                   (loop while (< i (+ nlen ndist))
                         do (let ((symbol (decode lencode)))
                              (if (< symbol 16)
                                  (progn (setf (aref all i) symbol) (incf i))
                                  (let* ((previous (if (= symbol 16)
                                                       (if (zerop i) (zip-fail "damaged packed data: a repeat with nothing before") (aref all (1- i)))
                                                       0))
                                         (times (case symbol (16 (+ 3 (bits 2))) (17 (+ 3 (bits 3))) (t (+ 11 (bits 7))))))
                                    (when (> (+ i times) (+ nlen ndist)) (zip-fail "damaged packed data: too many lengths"))
                                    (dotimes (k times) (setf (aref all i) previous) (incf i))))))
                   (codes (table (subseq all 0 nlen)) (table (subseq all nlen))))))
            (t (zip-fail "damaged packed data: a block of no kind")))
          (when (= last 1) (return)))))
    (if (= fill (length out)) out (subseq out 0 fill))))

(defun zip-entry-octets (octets entry)
  "The unpacked octets of ENTRY (ZIP-ENTRIES) of the zip archive OCTETS,
checked by its CRC; refused (ZIP-ERROR) when larger than *ZIP-LARGEST-ENTRY*,
growing by more than *ZIP-LARGEST-RATIO*, packed in a way not read, or damaged."
  (destructuring-bind (&key name method crc packed size data) entry
    (when (> size *zip-largest-entry*) (zip-fail "~a unpacks to ~:d octets, more than ~:d" name size *zip-largest-entry*))
    (when (and (> size (* 1024 1024)) (> size (* *zip-largest-ratio* (max 1 packed))))
      (zip-fail "~a grows ~d times when unpacked: an archive made to fill the memory" name (floor size (max 1 packed))))
    (when (> (+ data packed) (length octets)) (zip-fail "a damaged zip archive: ~a ends too soon" name))
    (let ((out (case method
                 (0 (subseq octets data (+ data packed)))
                 (8 (inflate octets :start data :end (+ data packed) :limit (max 1 size) :size (max 1 size)))
                 (t (zip-fail "~a is packed in a way not read (method ~d)" name method)))))
      (unless (and (= (length out) size) (= (crc32 out) crc))
        (zip-fail "a damaged zip archive: ~a is not what its check says" name))
      out)))
