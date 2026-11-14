;;;; src/io/zip.lisp --- Zip archives written and read, as a Word document (.docx) is one.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A .docx file is a zip archive of XML files and pictures. ZIP-OCTETS
;;;; writes one, each file stored as it is (zip's method 0, which every reader
;;;; takes), its name in UTF-8, its time the same always, so that the same
;;;; document gives the same octets. ZIP-ENTRIES and ZIP-ENTRY-OCTETS read
;;;; one, stored or deflated (INFLATE, deflate.lisp), each file no larger
;;;; unpacked than *ZIP-LARGEST-ENTRY* nor *ZIP-LARGEST-RATIO* times its
;;;; packed size, its CRC checked; a locked or damaged archive is refused
;;;; with a ZIP-ERROR. ZIP-LOCKED-P tells, from the directory at its end alone,
;;;; whether an archive of any size holds an entry locked with a password.

(in-package #:tuatara.io)

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

(defparameter *zip-largest-ratio* 200
  "The most an entry may grow by when unpacked, as a multiple of what it takes
packed (beyond its first megabyte); more is taken for an archive made to
fill the memory, and refused.")

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
