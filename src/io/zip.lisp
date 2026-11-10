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
;;;; with a ZIP-ERROR. ZIP-LOCKED-P tells, from the directory at its end alone,
;;;; whether an archive of any size holds an entry locked with a password.
;;;; INFLATE follows Mark Adler's puff.c. DEFLATE packs what INFLATE reads,
;;;; for what is sent where octets cost.

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
                    when (and (>= at 0) (= (octets-integer octets at 4) #x06054b50)) return at)))
    (unless end (zip-fail "no zip archive: its directory is not at its end"))
    (let ((count (octets-integer octets (+ end 10) 2))
          (at (octets-integer octets (+ end 16) 4))
          (entries '()))
      (dotimes (i count (nreverse entries))
        (unless (and (<= (+ at 46) n) (= (octets-integer octets at 4) #x02014b50))
          (zip-fail "a damaged zip archive: its directory's entry ~d" (1+ i)))
        (let* ((flags (octets-integer octets (+ at 8) 2))
               (name-length (octets-integer octets (+ at 28) 2))
               (extra (octets-integer octets (+ at 30) 2))
               (comment (octets-integer octets (+ at 32) 2))
               (local (octets-integer octets (+ at 42) 4))
               (name (tuatara.text:octets-string (subseq octets (+ at 46) (min n (+ at 46 name-length))))))
          (when (logbitp 0 flags) (zip-fail "~a in the archive is locked with a password" name))
          (unless (and (<= (+ local 30) n) (= (octets-integer octets local 4) #x04034b50))
            (zip-fail "a damaged zip archive: ~a's header" name))
          (push (list :name name :method (octets-integer octets (+ at 10) 2) :crc (octets-integer octets (+ at 16) 4)
                      :packed (octets-integer octets (+ at 20) 4) :size (octets-integer octets (+ at 24) 4)
                      :data (+ local 30 (octets-integer octets (+ local 26) 2) (octets-integer octets (+ local 28) 2)))
                entries)
          (setf at (+ at 46 name-length extra comment)))))))

;;; Whether a zip archive is locked, read from its directory at its end

(defparameter *zip-largest-directory* (* 16 1024 1024)
  "The largest central directory of a zip archive ZIP-LOCKED-P reads (16 MB,
some 200,000 entries).")

(defun zip-locked-p (fd size)
  "True when an entry of the zip archive open as file descriptor (FD), of SIZE
octets, is locked with a password, as the directory at its end says (a zip64
one too); NIL when none is, and when it has no such directory, or one larger
than *ZIP-LARGEST-DIRECTORY*. Only the directory is read."
  (let* ((tail (make-array (min size (+ 22 65535 20)) :element-type '(unsigned-byte 8)))
         (n (read-fd-at fd tail (- size (length tail))))
         (end (loop for at downfrom (- n 22) to 0
                    when (= (octets-integer tail at 4) #x06054b50) return at)))
    (when end
      (let ((count (octets-integer tail (+ end 10) 2))
            (length (octets-integer tail (+ end 12) 4))
            (start (octets-integer tail (+ end 16) 4)))
        ;; zip64: where the directory is, in the record the locator just before the end points to
        (when (and (or (= count #xFFFF) (= length #xFFFFFFFF) (= start #xFFFFFFFF))
                   (>= end 20) (= (octets-integer tail (- end 20) 4) #x07064b50))
          (let ((record (make-array 56 :element-type '(unsigned-byte 8)))
                (at (octets-integer tail (- end 12) 8)))
            (when (and (<= (+ at 56) size)
                       (= 56 (read-fd-at fd record at))
                       (= (octets-integer record 0 4) #x06064b50))
              (setf length (octets-integer record 40 8) start (octets-integer record 48 8)))))
        (when (and (<= length *zip-largest-directory*) (<= (+ start length) size))
          (let ((directory (make-array length :element-type '(unsigned-byte 8))))
            (read-fd-at fd directory start)
            (loop for at = 0 then (+ at 46 (octets-integer directory (+ at 28) 2) (octets-integer directory (+ at 30) 2)
                                     (octets-integer directory (+ at 32) 2))
                  while (and (<= (+ at 46) length) (= (octets-integer directory at 4) #x02014b50))
                  thereis (logbitp 0 (octets-integer directory (+ at 8) 2)))))))))

;;; Inflating, as RFC 1951 says, after Mark Adler's puff

(defparameter *inflate-length-base*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258)
  "The lengths the length codes 257 to 285 begin at.")
(defparameter *inflate-length-extra* #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
  "The extra bits after each length code.")
(defparameter *inflate-distance-base*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577)
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
                 ;; more codes of a length than the codes shorter leave room for
                 (let ((left 1))
                   (loop for l from 1 to 15
                         do (setf left (- (* 2 left) (aref count l)))
                            (when (minusp left) (zip-fail "damaged packed data: more codes than their lengths hold"))))
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
                                     (distance (progn
                                                 (when (>= d 30)
                                                   (zip-fail "damaged packed data: a distance that is none"))
                                                 (+ (aref *inflate-distance-base* d)
                                                    (bits (aref *inflate-distance-extra* d))))))
                                (when (> distance fill) (zip-fail "damaged packed data: a distance too far back"))
                                (dotimes (i length) (put (aref out (- fill distance))))))))))))
      (loop
        (let ((last (bits 1)) (type (bits 2)))
          (case type
            (0 (setf bit-buffer 0 bit-count 0)
               (when (> (+ at 4) end) (zip-fail "damaged packed data: a stored block ends too soon"))
               (let ((n (octets-integer octets at 2)))
                 (incf at 4)
                 (when (> (+ at n) end) (zip-fail "damaged packed data: a stored block ends too soon"))
                 (loop repeat n do (put (aref octets at)) (incf at))))
            (1 (let ((lengths (make-array 288 :initial-element 0)))
                 (loop for i below 288 do (setf (aref lengths i)
                                                (cond ((< i 144) 8) ((< i 256) 9) ((< i 280) 7) (t 8))))
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
                                         (times (case symbol
                                                  (16 (+ 3 (bits 2)))
                                                  (17 (+ 3 (bits 3)))
                                                  (t (+ 11 (bits 7))))))
                                    (when (> (+ i times) (+ nlen ndist))
                                      (zip-fail "damaged packed data: too many lengths"))
                                    (dotimes (k times) (setf (aref all i) previous) (incf i))))))
                   (codes (table (subseq all 0 nlen)) (table (subseq all nlen))))))
            (t (zip-fail "damaged packed data: a block of no kind")))
          (when (= last 1) (return)))))
    (if (= fill (length out)) out (subseq out 0 fill))))

;;; Packing: deflated data (RFC 1951), one dynamic Huffman block per stretch

(defconstant +deflate-window+ 32768 "How far back in the text a match may reach.")
(defconstant +deflate-block-tokens+ 262144 "The most literals and matches of one block.")
(defconstant +deflate-chain+ 16 "How many earlier places with the same three octets a match is sought among.")

(defparameter *deflate-length-codes*
  (let ((codes (make-array 259 :element-type '(unsigned-byte 16) :initial-element 0)))
    (loop for code from 0 to 28
          do (loop for length from (aref *inflate-length-base* code)
                     to (min 258 (+ (aref *inflate-length-base* code) (1- (ash 1 (aref *inflate-length-extra* code)))))
                   do (setf (aref codes length) code)))
    codes)
  "For each length a match can have, its code (from 0, a symbol being this past 256).")

(defun deflate-distance-code (distance)
  "The code from 0 of a match going back DISTANCE octets."
  (declare (type fixnum distance))
  (let ((low 0) (high 29))
    (declare (type fixnum low high))
    (loop while (< low high)
          do (let ((middle (ceiling (+ low high) 2)))
               (if (<= (the fixnum (aref *inflate-distance-base* middle)) distance)
                   (setf low middle)
                   (setf high (1- middle)))))
    low))

(defun huffman-lengths (counts most)
  "For each symbol of COUNTS (how often it comes), the bits of a code for it
of at most MOST bits, 0 for one that never comes; those that come most have
the shortest. One symbol alone gets one bit."
  (let* ((lengths (make-array (length counts) :element-type '(unsigned-byte 8) :initial-element 0))
         (used (sort (loop for symbol from 0 below (length counts) when (plusp (aref counts symbol)) collect symbol)
                     #'> :key (lambda (symbol) (aref counts symbol)))))
    (cond ((null used))
          ((null (rest used)) (setf (aref lengths (first used)) 1))
          (t (let* ((depths (make-array (length counts) :initial-element 0))
                    (nodes (sort (loop for symbol in used collect (cons (aref counts symbol) (list symbol)))
                                 #'< :key #'car)))
               (loop while (rest nodes)
                     do (let* ((a (pop nodes)) (b (pop nodes)) (symbols (append (cdr a) (cdr b))))
                          (dolist (symbol symbols) (incf (aref depths symbol)))
                          (setf nodes (merge 'list (list (cons (+ (car a) (car b)) symbols)) nodes #'< :key #'car))))
               ;; the lengths that came out, the longest brought within MOST and paid for by others' being longer
               (let ((how-many (make-array (+ most 2) :initial-element 0)) (total 0))
                 (dolist (symbol used) (incf (aref how-many (min most (aref depths symbol)))))
                 (loop for length from 1 to most
                       do (incf total (ash (aref how-many length) (- most length))))
                 (loop while (> total (ash 1 most))
                       do (decf (aref how-many most))
                          (loop for length from (1- most) downto 1
                                do (when (plusp (aref how-many length))
                                     (decf (aref how-many length))
                                     (incf (aref how-many (1+ length)) 2)
                                     (return)))
                          (decf total))
                 (let ((symbols used))
                   (loop for length from 1 to most
                         do (loop repeat (aref how-many length)
                                  do (setf (aref lengths (pop symbols)) length))))))))
    lengths))

(defun huffman-codes (lengths)
  "The code of each symbol of LENGTHS (HUFFMAN-LENGTHS), as deflated data has
it, its first bit the lowest."
  (let ((how-many (make-array 16 :initial-element 0)) (next (make-array 16 :initial-element 0))
        (codes (make-array (length lengths) :element-type '(unsigned-byte 16) :initial-element 0)))
    (loop for length across lengths do (incf (aref how-many length)))
    (setf (aref how-many 0) 0)
    (loop for length from 1 to 15
          do (setf (aref next length) (ash (+ (aref next (1- length)) (aref how-many (1- length))) 1)))
    (loop for symbol from 0 for length across lengths
          when (plusp length)
            do (let ((code (aref next length)) (reversed 0))
                 (incf (aref next length))
                 (dotimes (bit length) (setf reversed (logior (ash reversed 1) (ldb (byte 1 bit) code))))
                 (setf (aref codes symbol) reversed)))
    codes))

(defstruct (bit-writer (:constructor make-bit-writer (size &aux (octets (tuatara.text:make-octets size))))
                       (:copier nil))
  "Deflated data being written: the first FILL of OCTETS, then the COUNT bits
of BUFFER not yet a whole octet."
  (octets nil :type tuatara.text:octets)
  (fill 0 :type fixnum)
  (buffer 0 :type (unsigned-byte 62))
  (count 0 :type (integer 0 62)))

(setf (documentation 'make-bit-writer 'function) "An empty BIT-WRITER with room for SIZE octets before it grows."
      (documentation 'bit-writer-p 'function) "True when OBJECT is a BIT-WRITER."
      (documentation 'bit-writer-octets 'function) "The octets WRITER has written."
      (documentation 'bit-writer-fill 'function) "How many of WRITER's octets are written."
      (documentation 'bit-writer-buffer 'function) "The bits WRITER holds short of a whole octet."
      (documentation 'bit-writer-count 'function) "How many bits WRITER holds short of a whole octet.")

(defun write-bits (writer value count)
  "Add the COUNT low bits of VALUE to WRITER, the lowest first."
  (declare (type bit-writer writer) (type (unsigned-byte 32) value) (type (integer 0 32) count))
  (setf (bit-writer-buffer writer) (logior (bit-writer-buffer writer) (ash value (bit-writer-count writer))))
  (incf (bit-writer-count writer) count)
  (loop while (>= (bit-writer-count writer) 8)
        do (when (= (bit-writer-fill writer) (length (bit-writer-octets writer)))
             (setf (bit-writer-octets writer)
                   (replace (tuatara.text:make-octets (* 2 (length (bit-writer-octets writer))))
                            (bit-writer-octets writer))))
           (setf (aref (bit-writer-octets writer) (bit-writer-fill writer)) (logand 255 (bit-writer-buffer writer)))
           (incf (bit-writer-fill writer))
           (setf (bit-writer-buffer writer) (ash (bit-writer-buffer writer) -8))
           (decf (bit-writer-count writer) 8)))

(defun code-length-runs (lengths)
  "LENGTHS, a list of a block's code lengths, run-length coded as deflated
data has them: a list of (SYMBOL . REPEATS), REPEATS NIL for a length as it is."
  (let ((runs '()) (all lengths))
    (loop while all
          do (let* ((length (first all))
                    (run (or (position-if-not (lambda (other) (= other length)) all) (length all))))
               (setf all (nthcdr run all))
               (loop while (plusp run)
                     do (cond ((and (zerop length) (>= run 11))
                               (let ((n (min run 138))) (push (cons 18 n) runs) (decf run n)))
                              ((and (zerop length) (>= run 3))
                               (let ((n (min run 10))) (push (cons 17 n) runs) (decf run n)))
                              (t (push (cons length nil) runs)
                                 (decf run)
                                 (let ((more (min run 6)))
                                   (when (and (plusp length) (>= more 3))
                                     (push (cons 16 more) runs)
                                     (decf run more))))))))
    (nreverse runs)))

(defun write-block-codes (writer literal-lengths distance-lengths final)
  "Begin a dynamic Huffman block in WRITER, the last when FINAL, saying the
code lengths LITERAL-LENGTHS and DISTANCE-LENGTHS."
  (let* ((hlit (max 257 (1+ (or (position-if #'plusp literal-lengths :from-end t) 0))))
         (hdist (max 1 (1+ (or (position-if #'plusp distance-lengths :from-end t) 0))))
         (runs (code-length-runs (concatenate 'list (subseq literal-lengths 0 hlit) (subseq distance-lengths 0 hdist))))
         (counts (make-array 19 :initial-element 0)))
    (dolist (run runs) (incf (aref counts (car run))))
    (let* ((lengths (huffman-lengths counts 7))
           (codes (huffman-codes lengths))
           (hclen (max 4 (1+ (or (position-if (lambda (symbol) (plusp (aref lengths symbol))) *inflate-code-order*
                                              :from-end t)
                                 0)))))
      (write-bits writer (if final 1 0) 1)
      (write-bits writer 2 2)
      (write-bits writer (- hlit 257) 5)
      (write-bits writer (- hdist 1) 5)
      (write-bits writer (- hclen 4) 4)
      (dotimes (i hclen) (write-bits writer (aref lengths (aref *inflate-code-order* i)) 3))
      (dolist (run runs)
        (write-bits writer (aref codes (car run)) (aref lengths (car run)))
        (case (car run)
          (16 (write-bits writer (- (cdr run) 3) 2))
          (17 (write-bits writer (- (cdr run) 3) 3))
          (18 (write-bits writer (- (cdr run) 11) 7)))))))

(defun write-deflate-block (writer tokens count final)
  "Write the first COUNT of TOKENS to WRITER as one dynamic Huffman block, the
last when FINAL. A token below 256 is that octet; any other a match, its
length in the bits above 16 and how far back it reaches in those below."
  (declare (type (simple-array (unsigned-byte 32) (*)) tokens) (type fixnum count))
  (let ((literal-counts (make-array 286 :initial-element 0))
        (distance-counts (make-array 30 :initial-element 0)))
    (setf (aref literal-counts 256) 1)
    (dotimes (i count)
      (let ((token (aref tokens i)))
        (if (< token 256)
            (incf (aref literal-counts token))
            (progn (incf (aref literal-counts (+ 257 (aref *deflate-length-codes* (ash token -16)))))
                   (incf (aref distance-counts (deflate-distance-code (logand token 65535))))))))
    (when (every #'zerop distance-counts) (setf (aref distance-counts 0) 1))
    (let* ((literal-lengths (huffman-lengths literal-counts 15)) (literal-codes (huffman-codes literal-lengths))
           (distance-lengths (huffman-lengths distance-counts 15)) (distance-codes (huffman-codes distance-lengths)))
      (write-block-codes writer literal-lengths distance-lengths final)
      (dotimes (i count)
        (let ((token (aref tokens i)))
          (if (< token 256)
              (write-bits writer (aref literal-codes token) (aref literal-lengths token))
              (let* ((length (ash token -16)) (distance (logand token 65535))
                     (lc (aref *deflate-length-codes* length)) (dc (deflate-distance-code distance)))
                (write-bits writer (aref literal-codes (+ 257 lc)) (aref literal-lengths (+ 257 lc)))
                (write-bits writer (- length (aref *inflate-length-base* lc)) (aref *inflate-length-extra* lc))
                (write-bits writer (aref distance-codes dc) (aref distance-lengths dc))
                (write-bits writer (- distance (aref *inflate-distance-base* dc))
                            (aref *inflate-distance-extra* dc))))))
      (write-bits writer (aref literal-codes 256) (aref literal-lengths 256)))))

(defun deflate-stored (octets start end)
  "OCTETS from START below END as deflated data of stored blocks, unpacked."
  (let* ((blocks (max 1 (ceiling (- end start) 65535)))
         (out (tuatara.text:make-octets (+ (- end start) (* 5 blocks))))
         (fill 0) (at start))
    (dotimes (block blocks out)
      (let ((count (min 65535 (- end at))))
        (setf (aref out fill) (if (= block (1- blocks)) 1 0)
              (aref out (+ fill 1)) (logand 255 count)
              (aref out (+ fill 2)) (ash count -8)
              (aref out (+ fill 3)) (logand 255 (lognot count))
              (aref out (+ fill 4)) (logand 255 (ash (lognot count) -8)))
        (replace out octets :start1 (+ fill 5) :start2 at :end2 (+ at count))
        (incf fill (+ 5 count))
        (incf at count)))))

(defun deflate (octets &key (start 0) (end (length octets)))
  "OCTETS from START below END as deflated data (RFC 1951), as INFLATE takes
it: in dynamic Huffman blocks, or stored when that would not be smaller."
  (let* ((octets (coerce octets 'tuatara.text:octets))
         (writer (make-bit-writer (max 1024 (ash (- end start) -2))))
         (head (make-array 32768 :element-type 'fixnum :initial-element -1))
         (previous (make-array 32768 :element-type 'fixnum :initial-element -1))
         (tokens (make-array +deflate-block-tokens+ :element-type '(unsigned-byte 32)))
         (token-count 0)
         (at start))
    (declare (type tuatara.text:octets octets) (type fixnum start end token-count at)
             (type (simple-array fixnum (*)) head previous)
             (type (simple-array (unsigned-byte 32) (*)) tokens))
    (labels ((hash (place)
               (declare (type fixnum place))
               (logand 32767 (logxor (ash (aref octets place) 10) (ash (aref octets (+ place 1)) 5)
                                     (aref octets (+ place 2)))))
             (remember (place)
               (declare (type fixnum place))
               (when (<= (+ place 3) end)
                 (let ((h (hash place)))
                   (setf (aref previous (logand place 32767)) (aref head h)
                         (aref head h) place))))
             (longest-match ()
               (let ((best 0) (distance 0) (limit (min 258 (- end at))))
                 (declare (type fixnum best distance limit))
                 (when (>= limit 3)
                   (loop with from of-type fixnum = (aref head (hash at))
                         repeat +deflate-chain+
                         while (and (>= from start) (> from (- at +deflate-window+)))
                         do (let ((length 0))
                              (declare (type fixnum length))
                              (loop while (and (< length limit)
                                               (= (aref octets (+ from length)) (aref octets (+ at length))))
                                    do (incf length))
                              (when (> length best)
                                (setf best length distance (- at from))
                                (when (= best limit) (return))))
                            (setf from (aref previous (logand from 32767)))))
                 (if (>= best 3) (values best distance) (values 0 0))))
             (token (value)
               (setf (aref tokens token-count) value)
               (incf token-count)
               (when (= token-count +deflate-block-tokens+)
                 (write-deflate-block writer tokens token-count nil)
                 (setf token-count 0))))
      (loop while (< at end)
            do (multiple-value-bind (length distance) (longest-match)
                 (declare (type fixnum length distance))
                 (cond ((plusp length)
                        (token (logior (ash length 16) distance))
                        (loop repeat length do (remember at) (incf at)))
                       (t (token (aref octets at))
                          (remember at)
                          (incf at)))))
      (write-deflate-block writer tokens token-count t)
      (write-bits writer 0 7)
      (if (>= (bit-writer-fill writer) (- end start))
          (deflate-stored octets start end)
          (subseq (bit-writer-octets writer) 0 (bit-writer-fill writer))))))

(defun zip-entry-octets (octets entry)
  "The unpacked octets of ENTRY (ZIP-ENTRIES) of the zip archive OCTETS,
checked by its CRC; refused (ZIP-ERROR) when larger than *ZIP-LARGEST-ENTRY*,
growing by more than *ZIP-LARGEST-RATIO*, packed in a way not read, or damaged."
  (destructuring-bind (&key name method crc packed size data) entry
    (when (> size *zip-largest-entry*)
      (zip-fail "~a unpacks to ~:d octets, more than ~:d" name size *zip-largest-entry*))
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
