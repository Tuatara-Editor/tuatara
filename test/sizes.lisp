;;;; test/sizes.lisp --- Long functions and long files, pinned as they are.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A function of more than 80 lines or a file of more than 3000 lines is
;;;; hard to read in one go. test/api/sizes.sexp lists those there are, with
;;;; their lengths; a new one fails the test, and so does one that grew by
;;;; more than a tenth. A shorter one passes; tools/pin-sizes.sh writes the
;;;; list again to take it down.

(in-package #:tuatara.tests)

(defparameter *sizes-file* (asdf:system-relative-pathname :tuatara "test/api/sizes.sexp")
  "The pinned sizes: a row (:file PATH LINES) for each long file, (:function PATH NAME LINES) for each long function.")

(defparameter *longest-function* 80
  "The most lines a function has without being pinned.")

(defparameter *longest-file* 3000
  "The most lines a source file has without being pinned.")

(defparameter *growth-allowed* 1/10
  "The part of its pinned length a long function or file may grow by.")

(defun function-lengths (file)
  "A list of (NAME LINES) for each function, macro, generic function, method and command the Lisp FILE defines,
LINES being the lines its form covers. A name defined twice (methods) is given once, with the longest."
  (let* ((text (coerce (uiop:read-file-string file :external-format :utf-8) 'simple-string))
         (lengths (make-hash-table :test 'equal)))
    (loop for (start end) in (top-level-forms text)
          do (multiple-value-bind (operator name) (form-head text start)
               (when (member operator '("defun" "defmacro" "defgeneric" "defmethod" "define-command") :test #'equal)
                 (let ((lines (1+ (count #\Newline text :start start :end (1- end)))))
                   (setf (gethash name lengths) (max lines (gethash name lengths 0)))))))
    (loop for name being the hash-keys of lengths using (hash-value lines) collect (list name lines))))

(defun long-things ()
  "The rows (:file PATH LINES) and (:function PATH NAME LINES) of what is over the limits in the core now, in order."
  (let ((rows '()))
    (dolist (file (core-source-files))
      (let ((path (tree-relative-name file))
            (lines (with-open-file (in file :external-format :utf-8)
                     (loop for line = (read-line in nil) while line count t))))
        (when (> lines *longest-file*) (push (list :file path lines) rows))
        (loop for (name length) in (function-lengths file)
              when (> length *longest-function*) do (push (list :function path name length) rows))))
    (sort rows #'string< :key (lambda (row) (format nil "~{~a~^ ~}" (butlast row))))))

(defun write-sizes (&optional (file *sizes-file*))
  "Write the long files and functions of the core as they are now to FILE."
  (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
    (write-line ";; The files over 3000 lines and the functions over 80, pinned by tools/pin-sizes.sh:" out)
    (write-line ";; (:file PATH LINES) and (:function PATH NAME LINES). A new one fails the test, and so does" out)
    (write-line ";; one grown by more than a tenth." out)
    (let ((*print-pretty* nil))
      (dolist (row (long-things)) (prin1 row out) (terpri out)))))

(defun read-sizes (&optional (file *sizes-file*))
  "The rows pinned in FILE."
  (with-open-file (in file :external-format :utf-8)
    (let ((*read-eval* nil))
      (loop for row = (read in nil) while row collect row))))

(defun size-problems (&optional (pinned (read-sizes)))
  "A list of lines saying what is longer than the limits and is not in PINNED, or grew by more than *GROWTH-ALLOWED*."
  (loop for row in (long-things)
        for old = (find (butlast row) pinned :key #'butlast :test #'equal)
        for length = (car (last row))
        unless (and old (<= length (* (car (last old)) (+ 1 *growth-allowed*))))
          collect (format nil "~{~a~^ ~}: ~d lines~@[, pinned at ~d~]" (butlast row) length
                          (and old (car (last old))))))

(define-test (definitions sizes-pinned)
  :description "No function over 80 lines or file over 3000 lines is new, or has grown by a tenth since it was pinned."
  (let ((problems (size-problems)))
    (false problems "Over the limits and not pinned (tools/pin-sizes.sh pins what is meant): ~{~%  ~a~}" problems)))

(defun sizes-report (&optional (stream *standard-output*))
  "Print to STREAM the functions and files of the core over the limits, longest first, with the length each is pinned
at, after a line that counts them and one that counts those not pinned or grown past what is allowed."
  (let ((rows (long-things)) (pinned (read-sizes)) (problems (size-problems)))
    (format stream "~d functions over ~d lines or files over ~d lines, ~d not pinned or grown past ~d%~%"
            (length rows) *longest-function* *longest-file* (length problems) (round (* 100 *growth-allowed*)))
    (dolist (row (sort (copy-list rows) #'> :key (lambda (row) (car (last row)))))
      (format stream "  ~5d lines  ~a  ~a  (pinned at ~a)~%" (car (last row))
              (if (eq (first row) :file) "(file)" (third row)) (second row)
              (or (car (last (find (butlast row) pinned :key #'butlast :test #'equal))) "nothing")))))
