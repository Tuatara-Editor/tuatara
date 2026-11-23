;;;; tools/fuzz-common.lisp --- What the Lisp fuzzers share: the tree, the limits, the fence, and how a run fails.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded first by tools/fuzz-lisp.sh, which sets the environment the limits are read from, and then the
;;;; fuzzer of the kind (tools/fuzz-pdf.lisp, fuzz-remote.lisp, fuzz-sftp.lisp).

(defpackage #:tuatara.fuzz
  (:use #:cl)
  (:export #:*root* #:*kind* #:*kept-type* #:*rounds* #:*seed* #:*deadline* #:*heap-cap* #:*random*
           #:environment-integer #:failed #:check-reaches)
  (:documentation "What the fuzzers of tools/ have in common."))

(in-package #:tuatara.fuzz)

(defvar *root* (pathname (concatenate 'string (string-right-trim "/" (sb-ext:posix-getenv "FUZZ_ROOT")) "/"))
  "The Tuatara source tree fuzzed, as tools/fuzz-lisp.sh gives it.")

(defvar *kind* (sb-ext:posix-getenv "FUZZ_KIND")
  "What is fuzzed (pdf, remote or sftp): the first word of the files a failure keeps.")

(defvar *kept-type* "bin"
  "The type of the file that keeps what made a run fail.")

(load (merge-pathnames "tools/quicklisp.lisp" *root*))
(push *root* asdf:*central-registry*)
(ql:quickload :tuatara/defaults :silent t)

;; nothing of the developer's own is read or written from here on, however the editor is started
(load (merge-pathnames "tools/fence.lisp" *root*))
(tuatara.fence:fence-person-data (sb-ext:posix-getenv "FUZZ_HOME"))

(defun environment-integer (name default)
  "The whole number the environment variable NAME holds, or DEFAULT."
  (let ((text (sb-ext:posix-getenv name)))
    (or (and text (parse-integer text :junk-allowed t)) default)))

(defparameter *rounds* (environment-integer "FUZZ_ROUNDS" 1000)
  "How many spoilt inputs are read.")

(defparameter *seed* (environment-integer "FUZZ_SEED" 1)
  "The seed of the run's random choices: the same seed spoils alike.")

(defparameter *deadline* (environment-integer "FUZZ_DEADLINE" 10)
  "The most seconds one input may take to be read before it counts as a hang.")

(defparameter *heap-cap* (* (environment-integer "FUZZ_HEAP_MB" 1024) 1024 1024)
  "The most octets of heap in use after an input is read, once collected.")

(defparameter *random* (sb-ext:seed-random-state *seed*)
  "The run's random state.")

(defun failed (round octets control &rest arguments)
  "Say the run FAILED at ROUND, as CONTROL and ARGUMENTS say, keep OCTETS under build/fuzz/ and end with exit 1."
  (let ((path (merge-pathnames (format nil "build/fuzz/~a-failed-~d-~d.~a" *kind* *seed* round *kept-type*) *root*)))
    (ensure-directories-exist path)
    (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
    (format t "~&FAILED at round ~:d: ~? (kept as ~a)~%" round control arguments path)
    (finish-output)
    (sb-ext:exit :code 1 :abort t)))

(defun check-reaches ()
  "Stop the run before its first round, saying which, when the fuzzer being
loaded names with two colons a name of the editor that is neither a function
nor a variable nor a class: one renamed or taken away since the fuzzer was
written, which would otherwise fail only in the round that reaches it."
  (let ((missing '()))
    (cl-ppcre:do-register-groups (package name)
        ("(?i)(?<![\\w.:-])(tuatara[a-z.-]*)::([^\\s()'`,\"]+)" (uiop:read-file-string *load-truename*))
      (let* ((found (find-package (string-upcase package)))
             (symbol (and found (find-symbol (string-upcase name) found))))
        (unless (and symbol (or (fboundp symbol) (boundp symbol) (find-class symbol nil)
                                (fboundp (list 'setf symbol))))
          (pushnew (format nil "~(~a::~a~)" package name) missing :test #'string=))))
    (when missing
      (format t "~&FAILED before round 0: ~{~a~^, ~} not defined in the editor~%" (reverse missing))
      (finish-output)
      (sb-ext:exit :code 1 :abort t))))
