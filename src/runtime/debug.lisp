;;;; src/runtime/debug.lisp --- Reports for debug mode: what the machinery says when asked.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; While debug mode is on, code anywhere says what it is doing with
;;;; DEBUG-REPORT: the report is queued, stamped with the time and the thread
;;;; that made it, and the editor shows it in *Debug*; it is written at once to
;;;; the debug log in the cache directory too, which a crash or a hang cannot
;;;; keep it from. While it is off a report costs one look at a variable, its
;;;; text not even made, so reports stay in the code for good.

(in-package #:tuatara.runtime)

(defvar *debug-on* nil
  "True while debug mode is on: what DEBUG-REPORT is given is kept, to be shown
in *Debug*.")

(defvar *debug-start* (get-internal-real-time)
  "When the reports' times count from, in internal time units: the start of the
editor.")

(defvar *debug-reports* (sb-concurrency:make-queue :name "debug reports")
  "The reports made and not yet shown in *Debug*, oldest first: each (TIME
THREAD TOPIC TEXT), TIME in internal time units, THREAD the name of the thread
that made it.")

(defvar *debug-wake-function* nil
  "A function called in the thread that made a report, once it is queued, to
have it shown soon; NIL for none.")

(defvar *debug-log-path* nil
  "The file the reports of debug mode are written to as they are made, the
debug log (debug.log in the cache directory); NIL for none.")

(defvar *debug-log-stream* nil
  "The debug log, open while debug mode is on; NIL otherwise.")

(defvar *debug-log-lock* (sb-thread:make-mutex :name "debug log")
  "Held while a report is written to the debug log.")

(defvar *debug-log-begun* nil
  "True once this session began its debug log, the ones before kept aside.")

(defmacro debug-report (topic control &rest arguments)
  "While debug mode is on, keep a report for *Debug*: TOPIC, a keyword saying
what it is about (:parse, :rules, :job, :file, :redisplay, :gc, :error), and
the text FORMAT makes of CONTROL and ARGUMENTS. While it is off nothing is
evaluated but the look at *DEBUG-ON*."
  `(when *debug-on*
     (queue-debug-report ,topic (format nil ,control ,@arguments))))

(defun debug-report-line (report)
  "REPORT, as DEBUG-REPORT queues it (TIME THREAD TOPIC TEXT), as a line of
*Debug* and the debug log: the seconds since the start, the thread, the topic
and the text."
  (destructuring-bind (time thread topic text) report
    (format nil "~10,3f  ~12a ~10a ~a"
            (/ (- time *debug-start*) internal-time-units-per-second)
            (let ((name (or thread "")))
              (if (and (> (length name) 8) (string= "tuatara " name :end2 8)) (subseq name 8) name))
            (string-downcase (symbol-name topic)) text)))

(defun queue-debug-report (topic text)
  "Queue TEXT about TOPIC to be shown in *Debug*, stamped with the time and the
thread (DEBUG-REPORT), write it to the debug log at once, and wake the editor
to show it."
  (let ((report (list (get-internal-real-time) (sb-thread:thread-name sb-thread:*current-thread*) topic text)))
    (sb-concurrency:enqueue report *debug-reports*)
    (let ((stream *debug-log-stream*))
      (when stream
        (ignore-errors
         (sb-thread:with-mutex (*debug-log-lock*)
           (write-line (debug-report-line report) stream)
           (finish-output stream))))))
  (let ((wake *debug-wake-function*))
    (when wake (ignore-errors (funcall wake))))
  nil)

(defun lock-fd (fd &key shared (wait 0))
  "Take a lock on the open file FD, shared with others that take it SHARED,
else held alone: true when it was had, waiting at most WAIT seconds for
another holder to let it go. The lock goes when FD is closed or the process
ends."
  (let ((deadline (deadline-in wait)))
    (loop
      ;; LOCK_SH 1 or LOCK_EX 2, with LOCK_NB 4: never blocked in the call itself
      (when (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
                    fd (logior (if shared 1 2) 4)))
        (return t))
      (when (deadline-passed-p deadline) (return nil))
      (sleep 0.005))))

(defun log-in-use-p (native)
  "True when another session has the log at NATIVE open (OPEN-ROTATED-LOG
holds a shared lock on it for as long as it writes there)."
  (let ((fd (ignore-errors (sb-posix:open native sb-posix:o-rdonly))))
    (and fd
         (unwind-protect (not (lock-fd fd))
           (sb-posix:close fd)))))

(defun open-rotated-log (native &key rotate (keep 3))
  "A descriptor open for writing at the end of the log at NATIVE, made
readable and writable by you alone from the start, holding a shared lock on
it while it is open. With ROTATE (once a session), when no other session is
writing to it, a new one is begun, the logs of the sessions before kept as
NATIVE.1, the one just before, to NATIVE.KEEP; while a session writes to the
log no other puts it aside: one beginning meanwhile writes after its lines.
The debug log and the log of standard error are kept so. An error when it
cannot be opened."
  (when (and rotate (not (log-in-use-p native)))
    (flet ((name (n) (if (zerop n) native (format nil "~a.~d" native n))))
      (loop for n from (1- keep) downto 0
            do (ignore-errors (sb-posix:rename (name n) (name (1+ n)))))))
  (let ((fd (sb-posix:open native (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append) #o600)))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-posix:close fd)))))
      (ignore-errors (sb-posix:fchmod fd #o600))
      (lock-fd fd :shared t)
      fd)))

(defun open-debug-log ()
  "Open the debug log for writing at its end (OPEN-ROTATED-LOG), for you
alone; the first time in a session, when no other session is writing to it,
beginning a new one, the logs of the sessions before kept as debug.log.1, the
one just before, to debug.log.3. NIL when there is no debug log to open."
  (when *debug-log-path*
    (ignore-errors
     (let* ((path (pathname *debug-log-path*))
            (native (sb-ext:native-namestring path)))
       (ensure-directories-exist path)
       (let ((fd (open-rotated-log native :rotate (not *debug-log-begun*))))
         (setf *debug-log-begun* t)
         (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-posix:close fd)))))
           (sb-sys:make-fd-stream fd :output t :external-format :utf-8 :buffering :full
                                     :auto-close t :file native)))))))

(defvar *secret-at-hand* nil
  "True while a secret is in the calls under way (a passphrase, a text to be
encrypted or just decrypted): the calls written to the debug log then show
their strings and octets by their length alone (BACKTRACE-LINES).")

(defun secret-free-call (frame)
  "The call FRAME makes, as a list of its function's name and its arguments,
each string or vector of octets in them written as its length alone."
  (multiple-value-bind (name arguments) (sb-debug::frame-call frame)
    (labels ((hide (value depth)
               (typecase value
                 (string (make-symbol (format nil "#<~d characters>" (length value))))
                 ((vector (unsigned-byte 8)) (make-symbol (format nil "#<~d octets>" (length value))))
                 (cons (if (> depth 3) (make-symbol "#<...>") (mapcar (lambda (v) (hide v (1+ depth))) value)))
                 (t value))))
      (cons name (mapcar (lambda (argument) (hide argument 0)) arguments)))))

(defun backtrace-lines (&key (count 60) stop (package *package*))
  "The calls under way now, innermost first, as a list of strings, each printed
short, names as PACKAGE sees them, at most COUNT; those of the signalling
itself left out, and those from the first whose call is to the function named
STOP, a symbol, or to a function made inside it, on. While a secret is at hand (*SECRET-AT-HAND*), strings and
octets show as their length."
  (let ((lines '()))
    (ignore-errors
     (let ((*print-length* 8) (*print-level* 3) (*print-circle* t) (*print-pretty* nil) (*print-readably* nil)
           (*package* package))
       (sb-debug:map-backtrace
        (lambda (frame)
          (push (handler-case
                    (let ((text (if *secret-at-hand*
                                    (prin1-to-string (secret-free-call frame))
                                    (with-output-to-string (s) (sb-debug::print-frame-call frame s :argument-limit 8)))))
                      ;; a call's line of the log: its first 300 characters
                      (substitute #\Space #\Newline (subseq text 0 (min (length text) 300))))
                  (error () "#<a call that cannot be printed>"))
                lines))
        :count (+ count 12))))
    (let* ((lines (nreverse lines))
           (signalled (position-if (lambda (line)
                                     (or (eql 0 (search "(SB-KERNEL::%SIGNAL " line)) (eql 0 (search "(SIGNAL " line))
                                         (eql 0 (search "(ERROR " line))))
                                   lines :end (min 12 (length lines))))
           (lines (if signalled (nthcdr (1+ signalled) lines) lines))
           (call (and stop (cl-ppcre:create-scanner
                            (let ((name (format nil "(?:[^\\s():]+::?)?~a" (cl-ppcre:quote-meta-chars (symbol-name stop)))))
                              ;; (STOP ...), or ((LAMBDA () :IN STOP)) and the like
                              (format nil "^\\((?:~a[\\s)]|\\((?:LAMBDA|FLET|LABELS) [^\\n]*:IN ~a\\))" name name)))))
           (end (or (and call (position-if (lambda (line) (cl-ppcre:scan call line)) lines)) (length lines))))
      (subseq lines 0 (min count end)))))

(defvar *editor-busy-since* nil
  "When the editor's thread began the work it is doing, an internal real time;
NIL while it waits for what comes next.")

(defparameter *hang-report-seconds* 10
  "The seconds the editor's thread may be busy with one piece of work before
the calls under way in it are written to the error log (STDERR.LOG), once.")

(defvar *editor-busy-wake* (sb-thread:make-semaphore :name "tuatara editor busy")
  "Signalled as the editor's thread starts work, to wake the hang watch, which
waits on it while the editor is at rest.")

(defun note-editor-busy (busy)
  "Note that the editor's thread starts work (BUSY true) or waits again."
  (setf *editor-busy-since* (and busy (get-internal-real-time)))
  (when (and busy (zerop (sb-thread:semaphore-count *editor-busy-wake*)))
    (sb-thread:signal-semaphore *editor-busy-wake*)))

(defun start-hang-watch (thread &key (while (constantly t)))
  "Watch THREAD, the editor's, from a thread of its own, as long as it lives and
WHILE, a function of no arguments, is true: once it has been busy with one
piece of work for *HANG-REPORT-SECONDS* (*EDITOR-BUSY-SINCE*), have it write
the calls under way in it to the error output, so that a hang leaves a trace
however it came about. At rest it sleeps until the editor starts work
\(*EDITOR-BUSY-WAKE*), looking once a minute whether THREAD still lives;
while the editor works, once a second. Returns the watching thread."
  (make-guarded-thread
   (lambda ()
     (let ((reported nil))
       (loop (unless *editor-busy-since*
               (sb-thread:wait-on-semaphore *editor-busy-wake* :timeout 60))
             (sleep 1)
             (unless (and (sb-thread:thread-alive-p thread) (funcall while)) (return))
             (let ((since *editor-busy-since*))
               (when (and since (not (eql since reported))
                          (> (seconds-since since) *hang-report-seconds*))
                 (setf reported since)
                 (ignore-errors
                  (sb-thread:interrupt-thread
                   thread
                   (lambda ()
                     (let ((lines (backtrace-lines)))
                       (format *error-output* "~&Tuatara: the editor has been busy for ~d s; the calls under way:~%~{  ~a~%~}"
                               (round (seconds-since since)) lines)
                       (finish-output *error-output*)
                       (queue-debug-report :error (format nil "busy for ~d s:~{~%    ~a~}" (round (seconds-since since)) lines)))))))))))
   :name "tuatara hang watch"))

(defmacro reporting-errors ((control &rest arguments) &body body)
  "Run BODY; when an error is signalled in it while debug mode is on, report it
with the calls under way, CONTROL and ARGUMENTS saying what was being done, and
let it go on to whatever handles it."
  `(handler-bind ((error (lambda (condition)
                           (when *debug-on*
                             (queue-debug-report :error (format nil "~? failed: ~a~{~%    ~a~}" ,control (list ,@arguments)
                                                                condition (backtrace-lines)))))))
     ,@body))

(defmacro ignoring-errors-reported ((control &rest arguments) &body body)
  "Run BODY as IGNORE-ERRORS does, NIL and the condition when an error is
signalled in it; while debug mode is on, the error is reported first, with the
calls under way and CONTROL and ARGUMENTS saying what was being done
\(REPORTING-ERRORS), so that work in the background that gives up quietly
still leaves a trace."
  `(handler-case (reporting-errors (,control ,@arguments) ,@body)
     (error (condition) (values nil condition))))

(defun set-debug (on)
  "Switch debug mode on when ON is true, else off: on, the debug log is opened
(OPEN-DEBUG-LOG), off, closed. Returns ON."
  (sb-thread:with-mutex (*debug-log-lock*)
    (cond ((and on (null *debug-log-stream*))
           (setf *debug-log-stream* (open-debug-log)))
          ((and (not on) *debug-log-stream*)
           (ignore-errors (close *debug-log-stream*))
           (setf *debug-log-stream* nil))))
  (setf *debug-on* (and on t)))


