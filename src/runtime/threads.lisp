;;;; src/runtime/threads.lisp --- Threads that never take the editor down, deadlines, and waiting for work to stop.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.runtime)

;;; Deadlines

(defun deadline-in (seconds)
  "The internal real time SECONDS from now: a deadline for SECONDS-LEFT."
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun seconds-left (deadline)
  "The seconds until DEADLINE (DEADLINE-IN), less than 0 once it has passed."
  (/ (- deadline (get-internal-real-time)) internal-time-units-per-second))

(defun deadline-passed-p (deadline)
  "True once DEADLINE (DEADLINE-IN), an internal real time, has come."
  (>= (get-internal-real-time) deadline))

(defun seconds-since (time)
  "The seconds since TIME, in internal time units, as a ratio."
  (/ (- (get-internal-real-time) time) internal-time-units-per-second))

(defun milliseconds-since (time)
  "The milliseconds since TIME, in internal time units, as a number with a fraction."
  (/ (* 1000.0 (- (get-internal-real-time) time)) internal-time-units-per-second))

;;; Threads

(defmacro with-thread-guard ((name) &body body)
  "Run BODY, the work of the thread called NAME: whatever goes wrong in it, a
lack of memory too, ends that work and is reported in the debug log, never
ending the editor with it. NIL then, else what BODY returns."
  `(handler-case (progn ,@body)
     (serious-condition (e)
       (ignore-errors (debug-report :thread "~a stopped: ~a" ,name e))
       nil)))

(defparameter *background-niceness* 5
  "How far below the editor's thread a thread of work in the background stands
for the processor (as nice counts it, to 19 at most): with every core busy,
the editor's thread and the window's are given it first, so that keys are
answered at once while a job runs. The programs such a thread starts stand as
low, as a process takes the standing of the thread that starts it.")

(defun stand-below-editor (niceness)
  "Have the calling thread stand NICENESS below the program's first thread
\(where the editor runs, or the window), for the processor (setpriority for this thread alone, 19 at
most), whichever thread started it; nothing where that cannot be done, as a
thread may not stand higher than the one that started it without leave."
  (ignore-errors
   (let* ((tid (sb-alien:alien-funcall (sb-alien:extern-alien "gettid" (function sb-alien:int))))
          (getpriority (sb-alien:extern-alien "getpriority" (function sb-alien:int sb-alien:int sb-alien:unsigned-int)))
          (setpriority (sb-alien:extern-alien "setpriority" (function sb-alien:int sb-alien:int sb-alien:unsigned-int sb-alien:int)))
          ;; PRIO_PROCESS by a thread id: that thread alone; the process's own id is its first thread's
          (wanted (min 19 (+ (sb-alien:alien-funcall getpriority 0 (sb-posix:getpid)) niceness))))
     (unless (= wanted (sb-alien:alien-funcall getpriority 0 tid))
       (sb-alien:alien-funcall setpriority 0 tid wanted)))))

(defun make-guarded-thread (function &rest keys &key name (niceness *background-niceness*) &allow-other-keys)
  "A thread running FUNCTION as SB-THREAD:MAKE-THREAD runs it, with KEYS (its
:NAME, its :ARGUMENTS), whatever goes wrong in it, a lack of memory too,
ending its work alone (WITH-THREAD-GUARD), never the editor. It stands
NICENESS below the editor's thread for the processor, whichever thread makes
it (*BACKGROUND-NICENESS*, STAND-BELOW-EDITOR): 0 for a thread whose work is
looked at as it comes, as a terminal's output is."
  (let ((keys (loop for (key value) on keys by #'cddr
                    unless (eq key :niceness) append (list key value))))
    (apply #'sb-thread:make-thread
           (lambda (&rest arguments)
             (stand-below-editor niceness)
             (with-thread-guard ((or name "a thread"))
               (apply function arguments)))
           keys)))

(defun carry-settings (settings function)
  "FUNCTION made ready to run on a thread of its own: the function returned
calls it with the same arguments, each of SETTINGS (a list of special
variables, a kind of work's carried settings) bound to the value it has here
and now. A new thread sees only the settings' global values, not what the
command that started it bound; work that starts a thread gives it its
settings this way, taken as it starts. Takes time in the number of SETTINGS."
  (let ((values (mapcar #'symbol-value settings)))
    (lambda (&rest arguments)
      (progv settings values (apply function arguments)))))

(defvar *stop-wait* 2
  "The seconds a buffer being killed waits for work in the background on its
text to stop, once told to: work that does not stop in that time (a pattern
that backtracks without end, a grammar that loops) is left to end on its own,
the buffer's file kept mapped for it until nothing reads it.")

(defun join-within (thread &optional (seconds *stop-wait*))
  "Wait at most SECONDS for THREAD to end; true when it has ended, or when
THREAD is NIL, work that never had one."
  (or (null thread)
      (not (eq :running (sb-thread:join-thread thread :timeout seconds :default :running)))))

(defun left-running (thread what)
  "Report in the debug log that THREAD, doing WHAT, was told to stop and did
not end in time, and is left to end on its own."
  (debug-report :job "~a did not stop within ~a s (thread ~a); left to end on its own"
                what *stop-wait* (sb-thread:thread-name thread)))
