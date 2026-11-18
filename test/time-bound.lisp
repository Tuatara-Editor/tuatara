;;;; test/time-bound.lisp --- What fails a test besides its checks: its time run out, faults noted while it ran.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defvar *test-time-limit* 300
  "The seconds each test may run, the tests it holds not counted. A test that
needs longer says so with its own :TIME-LIMIT, (define-test name :time-limit 900).")

(defvar *hung-process* nil
  "The program the test that hangs on purpose started.")

(defvar *test-faults* (list nil)
  "The faults noticed while a test ran that fail it although none of its checks
did, newest first, in the car: a kept list changed in place, the editor busy
past the time the hang watch allows.")

(defun note-test-fault (control &rest arguments)
  "Note a fault, said by the format CONTROL and ARGUMENTS, to fail the test
running now. Safe from any thread and from an interrupt."
  (let ((text (apply #'format nil control arguments)))
    (sb-ext:atomic-push text (car *test-faults*))
    text))

(defvar *busy-reports-expected* nil
  "True while a test makes a thread busy past *HANG-REPORT-SECONDS* on purpose,
so that the hang watch's report fails no test. Set, not bound: the report is
made in the busy thread.")

(defmacro waiting-on-purpose (&body body)
  "Run BODY, which keeps the editor waiting within one command longer than
the hang watch allows on purpose, with *BUSY-REPORTS-EXPECTED* set."
  (let ((was (gensym "WAS")))
    `(let ((,was *busy-reports-expected*))
       (setf *busy-reports-expected* t)
       (unwind-protect (progn ,@body)
         (setf *busy-reports-expected* ,was)))))

(defmacro with-health-waited (&body body)
  "Run BODY with \\[check-health] waiting for its whole check, however long it
takes under load, instead of going on with it in the background: a wait of
the tests alone (WAITING-ON-PURPOSE)."
  `(let ((tuatara.defaults::*health-wait* 60))
     (waiting-on-purpose ,@body)))

(defun busy-report-fails-the-test (function topic text)
  "Hand TOPIC and TEXT, a report for the debug log, to FUNCTION, the reporting
itself; the hang watch's report that the editor has been busy too long is
noted as a fault of the test running (NOTE-TEST-FAULT) first, with the
first of the calls under way, unless *BUSY-REPORTS-EXPECTED*."
  (when (and (eq topic :error) (stringp text) (eql 0 (search "busy for " text)) (not *busy-reports-expected*))
    (note-test-fault "the editor was ~a" (substitute #\Space #\Newline (subseq text 0 (min 300 (length text))))))
  (funcall function topic text))

(sb-int:unencapsulate 'tuatara.runtime::queue-debug-report 'busy-fails-the-test)
(sb-int:encapsulate 'tuatara.runtime::queue-debug-report 'busy-fails-the-test 'busy-report-fails-the-test)

(defun take-test-faults ()
  "The faults noted since they were last taken, oldest first, taken away:
each once, with how many times it was noted when more than once."
  (let ((faults (reverse (loop for faults = (car *test-faults*)
                               when (eq faults (sb-ext:compare-and-swap (car *test-faults*) faults nil))
                                 return faults))))
    (loop for fault in (remove-duplicates faults :test #'string= :from-end t)
          for times = (count fault faults :test #'string=)
          collect (if (> times 1) (format nil "~a (~d times)" fault times) fault))))

(defun end-test-leftovers ()
  "Put the session's state back after a test was ended in the middle: the
background jobs stopped and forgotten, the programs parsing text ended as the
editor ends them, so that the next test asking for one has one started
afresh, and the other programs the test started ended."
  (dolist (job (copy-list tuatara.runtime:*jobs*))
    (ignore-errors (tuatara.runtime:stop-and-settle job)))
  (setf tuatara.runtime:*jobs* '())
  (ignore-errors (tuatara.syntax:stop-helpers))
  (dolist (process (copy-list sb-impl::*active-processes*))
    (ignore-errors
     (when (sb-ext:process-alive-p process)
       (sb-ext:process-kill process 9)
       (sb-ext:process-wait process)))))

(defmethod parachute:eval-in-context ((report parachute:report) (test parachute:test))
  "Run TEST for at most its time limit (*TEST-TIME-LIMIT* when it has none): past it the test is ended
where it is, the state it left is put back (END-TEST-LEFTOVERS), and a failed check says so. Each
fault noted while it ran (NOTE-TEST-FAULT) fails it too, with a check saying what it was."
  (let* ((limit (or (parachute:time-limit test) *test-time-limit*))
         (tag (list 'timed-out))
         (active t)
         (timer (sb-ext:make-timer (lambda () (when active (throw tag :timed-out)))
                                   :thread sb-thread:*current-thread*))
         (ended nil))
    (setf ended (catch tag
                  (sb-ext:schedule-timer timer limit)
                  (unwind-protect (progn (call-next-method) :finished)
                    (setf active nil)
                    (sb-ext:unschedule-timer timer))))
    (when (eq ended :timed-out)
      (end-test-leftovers)
      (parachute:true nil "~a was ended after ~d seconds, the time it may take" test limit))
    (dolist (fault (take-test-faults))
      (parachute:true nil "~a: ~a" test fault))))
