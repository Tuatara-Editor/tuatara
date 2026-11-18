;;;; test/ended-tests.lisp --- A test that hangs fails after its time, and the tests after it run as usual.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defparameter *hanging-tests-package*
  (let ((package (or (find-package '#:tuatara.tests.hanging) (make-package '#:tuatara.tests.hanging :use '(#:cl)))))
    (setf (documentation package t) "The tests that hang on purpose, to see they are ended.")
    package)
  "The package of the tests that hang on purpose, kept from the suite's own.")

(defun run-tests-apart (names)
  "Run the tests called NAMES of *HANGING-TESTS-PACKAGE* apart from the suite,
reported plainly: a function of a test's name giving the statuses of its
results, and what the report printed, as two values."
  (let* ((parachute::*parent* nil)
         (parachute::*context* nil)
         (output (make-string-output-stream))
         (report (let ((*standard-output* output))
                   (parachute:test (loop for name in names collect (parachute:find-test name *hanging-tests-package*))
                                   :report 'parachute:plain))))
    (values (lambda (name)
              (loop for result across (parachute:results report)
                    when (and (typep (parachute:expression result) 'parachute:test)
                              (string= (string (parachute:name (parachute:expression result))) name))
                      collect (parachute:status result)))
            (get-output-stream-string output))))

(define-test ended-tests
  :description "A test that does not end fails after its time limit and leaves nothing running: its jobs and programs are ended, and the tests after it run.")

(define-test (ended-tests a-hanging-test-fails-and-the-next-runs)
  (let ((*package* *hanging-tests-package*)
        (process nil))
    (eval `(parachute:define-test hangs-in-a-job
             :time-limit 1
             (let ((job (tuatara.defaults::%make-hash-job :what "a sum")))
               (tuatara.runtime:start-job-thread job (lambda (job)
                                                       (declare (ignore job))
                                                       (loop (tuatara.runtime:job-check-stop) (sleep 0.01))))
               (setf (symbol-value 'tuatara.tests::*hung-process*)
                     (sb-ext:run-program "/bin/sleep" '("300") :wait nil))
               (loop (sleep 0.01)))))
    (eval `(parachute:define-test runs-after-it
             (parachute:true t)))
    ;; a program parsing text started before, by another test
    (true (tuatara.syntax:kept-helper :shared))
    (let ((tuatara.runtime:*jobs* '()))
      (multiple-value-bind (statuses output) (run-tests-apart '(hangs-in-a-job runs-after-it))
        (setf process *hung-process*)
        (true (member :failed (funcall statuses "HANGS-IN-A-JOB")))
        (false (member :failed (funcall statuses "RUNS-AFTER-IT")))
        (true (member :passed (funcall statuses "RUNS-AFTER-IT")))
        (true (search "was ended after 1 seconds" output))
        (false (some #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*))
        (false (sb-ext:process-alive-p process))))
    ;; the tests after it still have programs to parse with, started afresh
    (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "rust")
                                              (tuatara.syntax:kept-helper :shared))))
      (true parser)
      (tuatara.syntax:delete-parser parser))))

(define-test (ended-tests a-busy-editor-fails-the-test)
  ;; the hang watch's report that the editor was busy too long, written to the debug log while a test runs,
  ;; fails that test, named with what was under way; the test that makes it on purpose says so first
  (let ((*package* *hanging-tests-package*))
    (eval `(parachute:define-test busy-too-long
             (tuatara.runtime::queue-debug-report :error (format nil "busy for 10 s:~%    (SPIN)"))
             (parachute:true t)))
    (eval `(parachute:define-test busy-on-purpose
             (tuatara.tests::waiting-on-purpose
               (tuatara.runtime::queue-debug-report :error (format nil "busy for 10 s:~%    (SPIN)")))
             (parachute:true t)))
    (multiple-value-bind (statuses output) (run-tests-apart '(busy-too-long busy-on-purpose))
      (drain-debug-reports)
      (true (member :failed (funcall statuses "BUSY-TOO-LONG")))
      (true (member :passed (funcall statuses "BUSY-ON-PURPOSE")))
      (false (member :failed (funcall statuses "BUSY-ON-PURPOSE")))
      (true (search "BUSY-TOO-LONG>: the editor was busy for 10 s" output)))))
