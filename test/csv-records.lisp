;;;; test/csv-records.lisp --- A CSV read where its text is kept: records over lines, fields, the header, the separator.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test csv-records
  :description "CSV records and fields read from the text where it is kept, nothing copied: record ends and
starts against a scan of the string, colouring without copying rows, record starts kept over edits, a
field read no further than asked, a quoted header over lines, the separator read as a filter matches.")

(defun csv-test-text (records)
  "RECORDS records of CSV, some fields quoted, some quotes doubled, some with
line breaks inside, the same each time."
  (let ((*random-state* (sb-ext:seed-random-state 360)))
    (with-output-to-string (o)
      (dotimes (i records)
        (dotimes (k (1+ (random 5)))
          (when (plusp k) (write-char #\, o))
          (case (random 6)
            (0 (format o "\"a,~d\"" i))
            (1 (format o "\"line~%break\""))
            (2 (format o "\"say \"\"hi\"\"\""))
            (t (format o "v~d" (random 1000)))))
        (terpri o)))))

(defun string-record-end (text start)
  "Where the record of TEXT from START ends, read from the string: the first
newline outside double quotes, or the end."
  (let ((quoted nil))
    (loop for i from start below (length text)
          do (case (char text i)
               (#\" (setf quoted (not quoted)))
               (#\Newline (unless quoted (return-from string-record-end i)))))
    (length text)))

(define-test (csv-records ends-and-starts-agree)
  ;; every record's end and start, read where the text is kept, as a scan of the string has them
  (let* ((text (csv-test-text 400))
         (rope (tuatara.text:string->rope text)))
    (loop for start = 0 then (1+ end)
          for end = (string-record-end text start)
          while (< start (length text))
          do (is = end (tuatara.defaults::csv-record-end rope start))
             ;; each line of the record: its record starts where the record does
             (loop for line = start then (1+ (position #\Newline text :start line))
                   while (< line end)
                   do (is = start (tuatara.defaults::rope-record-start rope line))))))

(defun least-bytes (function &optional (times 100))
  "The least of three runs' octets consed by calling FUNCTION TIMES times, a call's share."
  (loop repeat 3
        minimize (let ((before (sb-ext:get-bytes-consed)))
                   (dotimes (i times) (funcall function))
                   (/ (- (sb-ext:get-bytes-consed) before) times))))

(define-test (csv-records rows-coloured-without-copying)
  ;; a long row of few fields is coloured from the text where it is kept: what is made is its colours alone
  (let* ((row (format nil "~a,~a,\"~a\",~a~%" (make-string 600 :initial-element #\a) (make-string 600 :initial-element #\b)
                      (make-string 600 :initial-element #\c) (make-string 400 :initial-element #\d)))
         (rope (tuatara.text:string->rope row))
         (end (1- (length row))))
    (true (< (least-bytes (lambda () (tuatara.defaults::csv-scan rope 0 end 44 0 nil))) 1000))
    (true (< (least-bytes (lambda () (tuatara.defaults::csv-sparse-scan rope 0 end 44 nil))) 1000))
    (true (< (least-bytes (lambda () (tuatara.defaults::csv-record-end rope 0))) 200))))

(define-test (csv-records starts-kept-over-edits)
  ;; the record starts found are kept for the text before an edit: finding a record near
  ;; one found before reads a little, not the megabyte back, after typing further on
  (let* ((text (with-output-to-string (o) (dotimes (i 200000) (format o "~d,\"x~%y\",z~%" i))))
         (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "csv") :text text))
         (line (tuatara.buffer:buffer-line-start buffer (floor (length text) 2))))
    (unwind-protect
         (let ((fresh (least-seconds (lambda ()
                                       (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::csv-record-start) nil)
                                       (dotimes (i 10) (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::csv-record-start) nil)
                                         (tuatara.defaults::csv-record-start buffer line)))))
               (after-edit (least-seconds (lambda ()
                                            (dotimes (i 10)
                                              (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer) "w")
                                              (tuatara.defaults::csv-record-start buffer line))))))
           (is = (tuatara.defaults::rope-record-start (tuatara.buffer:buffer-text buffer) line)
               (tuatara.defaults::csv-record-start buffer line))
           (true (< after-edit (+ 0.002 (/ fresh 5))) "~,4fs after edits, ~,4fs from nothing kept" after-edit fresh))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (csv-records field-read-no-further)
  ;; a field near the start of a long record is found without reading the rest
  (let* ((text (format nil "~{~a~^,~}" (loop for i below 256 collect (make-string 40 :initial-element #\v))))
         (length (length text)))
    (is equal (list 41 81) (multiple-value-list (tuatara.defaults::csv-field-bounds text 0 length 44 1)))
    (is equal (list nil 256) (multiple-value-list (tuatara.defaults::csv-field-bounds text 0 length 44 300)))
    (let ((near (least-seconds (lambda () (dotimes (i 2000) (tuatara.defaults::csv-field-bounds text 0 length 44 1)))))
          (far (least-seconds (lambda () (dotimes (i 2000) (tuatara.defaults::csv-field-bounds text 0 length 44 255))))))
      (true (< near (+ 0.001 (/ far 10))) "~,4fs for field 1, ~,4fs for field 255" near far))))

(define-test (csv-records header-over-lines)
  ;; a header whose quoted name holds a line break is one record: its names whole, the
  ;; records after it
  (let* ((text (format nil "\"bird~%name\",place~%kiwi,Akaroa~%tui,Raglan~%"))
         (rope (tuatara.text:string->rope text)))
    (is = (search "kiwi" text) (tuatara.defaults::records-start rope 1 #'tuatara.defaults::csv-record-end))
    (with-records (f buffer path) ('() :text text)
      (is equal (list (format nil "bird~%name") "place") (tuatara.defaults::csv-header-names buffer)))
    (with-records (f buffer path) ('("C-x / s t u i RET") :text text)
      (is equal (list "\"bird" "name\",place" "tui,Raglan") (records-shown buffer)))))

(define-test (csv-records separator-read-as-matched)
  ;; a filter by a column kept before the separator is changed cuts records by the new one
  (with-records (f buffer path) ('() :text (format nil "a;b~%1;x~%2;y~%"))
    (setf (tuatara.buffer:buffer-local buffer :csv-delimiter) 44)
    (let ((fields (tuatara.defaults::mode-record-fields (tuatara.modes:buffer-major-mode buffer) buffer)))
      (setf (tuatara.buffer:buffer-local buffer :csv-delimiter) 59)
      (is string= "x" (tuatara.defaults::record-field-value fields "1;x" 1)))))
