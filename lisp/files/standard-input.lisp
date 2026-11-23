;;;; lisp/files/standard-input.lisp --- Text piped to the editor read into a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Started with - as a file, the editor reads its standard input into a file
;;;; with no name, in the background, and shows it in a buffer of its own.

(in-package #:tuatara.defaults)

(define-record (standard-input-job (:include tuatara.runtime:job)
                                   (:constructor make-standard-input-job (buffer descriptor)) (:copier nil))
  "Standard input being read for BUFFER, a job, from the open DESCRIPTOR into a
file with no name; SCRATCH, that file's descriptor, and SIZE, the octets in it."
  (descriptor nil :documentation "The open descriptor that STANDARD-INPUT-JOB reads standard input from.")
  (scratch nil
           :documentation "The descriptor of the file with no name that STANDARD-INPUT-JOB copies standard input into,
NIL until it is made.")
  (size nil :documentation "How many octets STANDARD-INPUT-JOB copied into that file, NIL until it is done."))

(tuatara.runtime:define-job-words standard-input-job
  :doing "Reading standard input"
  :description "Reading standard input"
  :verb "read"
  :left-words "standard input not read"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-take-in ((job standard-input-job))
  "Show what was read from standard input in its buffer, read only, or why it could not be read."
  (let ((scratch (standard-input-job-scratch job)))
    (if (and scratch (not (live-buffer-p (tuatara.runtime:job-buffer job))))
        (ignoring-errors-reported ("closing the copy of standard input") (sb-posix:close scratch))
        (show-reading-result job "standard input"
                             (lambda (buffer)
                               (multiple-value-bind (rope mapping)
                                   (tuatara.io:scratch-file-text scratch (standard-input-job-size job) "standard input")
                                 (set-made-text buffer rope)
                                 (setf (tuatara.buffer:buffer-mapping buffer) mapping
                                       (tuatara.buffer:buffer-point buffer) 0)))
                             "~a could not be read: ~a~%"))))

(defmethod host-visit-standard-input ((host defaults-host) descriptor)
  "Make a buffer of what is read from the open DESCRIPTOR, standard input, in
the background. The text goes into a file with no name in the cache directory
and is read from there as a large file is read, so that its size is bounded by
the disk. The buffer is called *stdin*, not a file's, read only and opened
safely, so that nothing in it is acted on. Returns the buffer at once."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*stdin*")
                                            :text (format nil "Reading standard input...~%"))))
    (setf (buffer-read-only-p buffer) t
          (tuatara.io:safe-visit-p buffer) "it came from standard input")
    (tuatara.runtime:start-job-thread
     (make-standard-input-job buffer descriptor)
     (lambda (job)
       (unwind-protect
            (multiple-value-bind (scratch size)
                (tuatara.io:copy-descriptor-to-scratch (standard-input-job-descriptor job))
              (setf (standard-input-job-scratch job) scratch
                    (standard-input-job-size job) size))
         (ignoring-errors-reported ("closing standard input") (sb-posix:close (standard-input-job-descriptor job)))))
     :name "tuatara standard input")
    buffer))
