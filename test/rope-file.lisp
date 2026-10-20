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
  (let* ((s (random-text 120000)) (octets (to-octets s)))
    (with-temp-file (path octets)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (true (tuatara.text:mapping-p mapping))
        (is = (length octets) (tuatara.text:rope-bytes rope))
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
          (let ((indexed (tuatara.text:index-rope rope :threads threads :chunk 50000)))
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
  (let* ((s (random-text 60000)) (octets (to-octets s)))
    (with-temp-file (path octets)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (let* ((indexed (tuatara.text:index-rope rope :threads 2 :chunk 70000))
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

(define-test (rope-file empty-file)
  (with-temp-file (path (make-array 0 :element-type '(unsigned-byte 8)))
    (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
      (false mapping)
      (true (tuatara.text:rope-indexed-p rope))
      (is = 0 (tuatara.text:rope-bytes rope))
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
