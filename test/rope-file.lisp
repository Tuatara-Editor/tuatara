;;;; test/rope-file.lisp --- File-backed ropes: mapping, indexing, editing spans.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun write-temp-file (octets)
  "Write OCTETS to a fresh temporary file and return its pathname."
  (let ((path (merge-pathnames (format nil "tuatara-test-~a-~a.txt" (get-universal-time) (random 1000000))
                               (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence octets out))
    path))

(defmacro with-temp-file ((path octets) &body body)
  "Run BODY with PATH bound to a temporary file holding OCTETS, deleting it afterwards."
  `(let ((,path (write-temp-file ,octets)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,path)))))

(define-test rope-file
  :description "File-backed ropes: open without reading, byte access before indexing, parallel indexing, edits that keep the file mapped, closing.")

(define-test (rope-file open-read-index)
  ;; big enough for many spans and several index chunks with a small chunk size
  (let* ((s (random-text 480000)) (octets (to-octets s)))
    (with-temp-file (path octets)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (true (tuatara.text:mapping-p mapping))
        (is = (length octets) (tuatara.text:rope-octet-length rope))
        (false (tuatara.text:rope-indexed-p rope))
        (fail (tuatara.text:rope-chars rope))
        (fail (tuatara.text:char->byte rope 0))
        ;; byte-based reading works before indexing, including across span cuts inside characters
        (is equalp octets (tuatara.text:rope-octets rope))
        (is string= s (tuatara.text:rope-string rope))
        (is = (aref octets 5) (tuatara.text:rope-octet rope 5))
        (let ((stats (tuatara.text:rope-statistics rope)))
          (is = 0 (getf stats :memory-bytes))
          (is = (length octets) (getf stats :file-bytes))
          (is = (ceiling (length octets) tuatara.text:+span-size+) (getf stats :spans))
          (is = (getf stats :spans) (getf stats :unindexed-spans)))
        ;; index on one thread and on several, with small chunks to exercise the boundary logic
        (dolist (threads '(1 4))
          (let ((indexed (tuatara.text:index-rope rope :threads threads :chunk 200000)))
            (true (tuatara.text:rope-indexed-p indexed))
            (check-rope-matches indexed s)
            (is = 0 (getf (tuatara.text:rope-statistics indexed) :unindexed-spans))
            (is = 0 (getf (tuatara.text:rope-statistics indexed) :memory-bytes))
            (let ((offsets (byte-offsets s)))
              (loop repeat 40
                    for c = (random (1+ (length s)))
                    do (is = (aref offsets c) (tuatara.text:char->byte indexed c))
                       (is = c (tuatara.text:byte->char indexed (aref offsets c)))))))
        ;; the original rope is still unindexed and still readable
        (false (tuatara.text:rope-indexed-p rope))
        (is equalp octets (tuatara.text:rope-octets rope))
        (tuatara.text:close-mapping mapping)
        (false (tuatara.text:mapping-live mapping))
        (fail (tuatara.text:rope-octets rope))))))

(define-test (rope-file edits-keep-the-file-mapped)
  (let* ((s (random-text 240000)) (octets (to-octets s)))
    (with-temp-file (path octets)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (let* ((indexed (tuatara.text:index-rope rope :threads 2 :chunk 280000))
               (offsets (byte-offsets s))
               (mid (aref offsets (floor (length s) 2)))
               (edited (tuatara.text:rope-insert indexed mid "INSERTED")))
          (check-rope-matches edited (concatenate 'string (subseq s 0 (floor (length s) 2)) "INSERTED"
                                                  (subseq s (floor (length s) 2))))
          ;; only the touched span (plus at most the small neighbours merged into it) was materialised
          (let ((stats (tuatara.text:rope-statistics edited)))
            (true (<= (getf stats :memory-bytes) (* 3 tuatara.text:+max-leaf+))
                  "memory bytes after one insertion: ~a" (getf stats :memory-bytes))
            (true (>= (getf stats :file-bytes) (- (length octets) (* 3 tuatara.text:+max-leaf+)))))
          ;; random batches against the oracle, on a file-backed rope
          (let ((rope indexed) (s s))
            (dotimes (step 15)
              (multiple-value-bind (edits expected expected-positions) (random-batch s)
                (multiple-value-bind (new positions) (tuatara.text:apply-edits rope edits)
                  (check-rope-matches new expected)
                  (is equalp expected-positions positions)
                  (setf rope new s expected)))))
          ;; an uncounted file takes edits too, its span cuts inside characters kept uncounted
          (let ((started (tuatara.text:rope-insert rope 0 "START")))
            (false (tuatara.text:rope-indexed-p started))
            (true (tuatara.text:check-rope started))
            (is string= (concatenate 'string "START" s)
                (tuatara.text:octets-string (tuatara.text:rope-octets (tuatara.text:index-rope started)))))
          ;; indexing an already indexed-and-edited rope is the identity
          (true (eq indexed (tuatara.text:index-rope indexed))))
        (tuatara.text:close-mapping mapping)))))

(define-test (rope-file few-objects-for-a-large-file)
  ;; every node of a rope is copied by each collection of garbage that reaches
  ;; it: a mapped gigabyte is a few thousand nodes, not tens of thousands
  (let ((path (merge-pathnames (format nil "tuatara-test-sparse-~d" (sb-posix:getpid)) (uiop:temporary-directory)))
        (size (expt 2 30)))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (file-position out (1- size))
      (write-byte 10 out))
    (unwind-protect
         (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
           (let ((nodes 0))
             (labels ((walk (node)
                        (incf nodes)
                        (when (tuatara.text::branch-p node)
                          (map nil #'walk (tuatara.text::branch-children node)))))
               (walk (tuatara.text::rope-root rope)))
             (is = size (tuatara.text:rope-octet-length rope))
             (true (< nodes 20000) "~:d nodes for a gigabyte" nodes))
           (tuatara.text:close-mapping mapping))
      (ignore-errors (delete-file path)))))

(define-test (rope-file empty-file)
  (with-temp-file (path (make-array 0 :element-type '(unsigned-byte 8)))
    (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
      (false mapping)
      (true (tuatara.text:rope-indexed-p rope))
      (is = 0 (tuatara.text:rope-octet-length rope))
      (check-rope-matches (tuatara.text:rope-insert rope 0 "x") "x"))))

(define-test (rope-file pipes-never-waited-on)
  ;; only a regular file is mapped: a named pipe is refused at once, not waited on for a writer
  (let ((fifo (format nil "/tmp/tuatara-test-fifo-~d" (sb-posix:getpid))))
    (sb-posix:mkfifo fifo #o600)
    (unwind-protect
         (let ((thread (sb-thread:make-thread
                        (lambda () (handler-case (progn (tuatara.text:map-file fifo) :mapped) (error () :refused)))
                        :name "mapping a pipe")))
           (is eq :refused (sb-thread:join-thread thread :timeout 5 :default :waiting)))
      (ignore-errors (sb-posix:unlink fifo)))))

(define-test (rope-file return-and-newline-join-once-after-an-edit)
  ;; a carriage return and the newline after it, inside one counted span of a mapped file, are
  ;; one line end: an x put between them and taken away again leaves two spans of the file that
  ;; follow each other, and still one line end, every line number after it as before
  (let ((text (with-output-to-string (out)
                (dotimes (i 20000)
                  (write-string "kea ruru" out)
                  (when (= i 1000) (write-char #\Return out))
                  (terpri out)))))
    (with-temp-file (path (to-octets text))
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (let* ((counted (tuatara.text:index-rope rope))
               (newline (1+ (position #\Return text)))
               (typed (tuatara.text:splice-rope counted newline newline (tuatara.text:string->rope "x")))
               (after (tuatara.text:splice-rope typed newline (1+ newline))))
          (is = 20000 (tuatara.text:rope-lines counted))
          (is = (tuatara.text:rope-octet-length counted) (tuatara.text:rope-octet-length after))
          (is = (tuatara.text:rope-lines counted) (tuatara.text:rope-lines after))
          (loop for byte in (list newline (+ newline 2) 60000 (1- (length text)))
                do (is = (tuatara.text:byte->line counted byte) (tuatara.text:byte->line after byte))))
        (tuatara.text:close-mapping mapping)))))

(define-test (rope-file return-and-newline-join-once-while-counting)
  ;; a carriage return and the newline after it, parted where the file is counted on one side
  ;; and not yet on the other, as an edit made while a large file is counted parts them, are
  ;; one line end once all of it is counted
  (let ((text (with-output-to-string (out)
                (dotimes (i 20000)
                  (write-string "kea ruru" out)
                  (when (= i 1000) (write-char #\Return out))
                  (terpri out)))))
    (with-temp-file (path (to-octets text))
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (let* ((newline (1+ (position #\Return text)))
               (left (tuatara.text:rope-slice rope 0 newline))
               (right (tuatara.text:index-rope (tuatara.text:rope-slice rope newline
                                                                        (tuatara.text:rope-octet-length rope))))
               (joined (tuatara.text:index-rope (tuatara.text:splice-rope left newline newline right))))
          (is = 20000 (tuatara.text:rope-lines joined))
          (loop for byte in (list newline (+ newline 2) 60000 (1- (length text)))
                do (is = (tuatara.text:byte->line (tuatara.text:index-rope rope) byte)
                       (tuatara.text:byte->line joined byte))))
        (tuatara.text:close-mapping mapping)))))
