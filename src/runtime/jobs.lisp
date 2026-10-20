;;;; src/runtime/jobs.lisp --- Jobs in the background: one runner for every kind of work.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Work that takes time runs as a job on a thread of its own, the editor free
;;;; meanwhile: a replacement, a sort, a sum, a save, files copied, an agenda
;;;; worked out. Every kind of job is a structure including JOB, started by
;;;; START-JOB-THREAD and kept in *JOBS* until the editor takes it in
;;;; (TAKE-IN-JOBS) once it has ended. However its work ends, an error, a lack
;;;; of memory or stack, or the thread cut short, the job leaves :RUNNING and
;;;; the editor is woken to take it in. What differs between kinds, how far one
;;;; has got, what it says of itself, how it stops and what is done with it at
;;;; the end, each kind says through the generic functions here.

(in-package #:tuatara.runtime)

(defstruct (job (:constructor nil) (:copier nil))
  "Work in the background for BUFFER, the buffer it belongs to (NIL for none),
running on THREAD. STATUS is :RUNNING, then :DONE, :STOPPED or :FAILED, with
ERROR saying why in words; the car of CANCEL turning true asks it to stop;
STARTED is the internal real time it began; WAKE, a function of no arguments
or NIL, wakes the editor for its thread, so that what it has to show shows at
once; PROCESS, the program it runs, if any (JOB-PROGRAM-STARTED). Each kind of job is a structure including this one, and says how it
differs through generic functions: JOB-PERCENT, JOB-DOING, JOB-DESCRIPTION,
JOB-VERB, JOB-LEFT-WORDS, JOB-CONSEQUENCE, JOB-STOP, JOB-HOLDS-QUEUE-P,
JOB-SHOWS-PROCESSING-P, JOB-AT-EXIT, JOB-GUARDED-SPANS and JOB-TAKE-IN."
  buffer (status :running) error (cancel (list nil)) thread (started (get-internal-real-time)) wake process)

(setf (documentation 'job-p 'function) "True when OBJECT is a JOB, of any kind."
      (documentation 'job-buffer 'function) "The buffer a job belongs to, or NIL."
      (documentation 'job-status 'function) "A job's status: :RUNNING, :DONE, :STOPPED or :FAILED."
      (documentation 'job-error 'function) "Why a job failed, in words, or NIL."
      (documentation 'job-cancel 'function) "A cons whose car turning true asks a job to stop."
      (documentation 'job-thread 'function) "The thread a job runs on."
      (documentation 'job-started 'function) "The internal real time a job began."
      (documentation 'job-wake 'function) "The function a job's thread calls to wake the editor, or NIL."
      (documentation 'job-process 'function) "The program a job runs, ended when it is stopped, or NIL.")

(defvar *jobs* '()
  "The jobs in the background, running or ended and not yet taken in, the
oldest first.")

(defvar *job* nil
  "On the thread of a job, the job its work is for (START-JOB-THREAD); NIL elsewhere.")

(defvar *job-taken-in* nil
  "On the editor's thread, while a job that has ended is taken in (TAKE-IN-JOBS),
that job; NIL otherwise.")

(defvar *job-waker* nil
  "A function of no arguments, called on the editor's thread as a job starts,
giving the function of no arguments its thread calls to wake the editor, or
NIL; the editor sets it.")

(defvar *job-buffer-name* (lambda (buffer) (if buffer (princ-to-string buffer) "no buffer"))
  "A function of a buffer, or NIL, giving its name, for what the jobs report;
the buffers' package sets it.")

(define-condition job-stopped (serious-condition) ()
  (:documentation "Signalled in a job's work where it finds it was asked to stop (JOB-CHECK-STOP): the job ends :STOPPED."))

(defun job-running-p (job)
  "True while JOB runs."
  (eq (job-status job) :running))

(defun jobs-of (buffer &key running)
  "The jobs in the background of BUFFER, the oldest first: running, or ended
and not yet taken in; with RUNNING those running alone."
  (remove-if-not (lambda (job) (and (eq (job-buffer job) buffer)
                                    (or (not running) (job-running-p job))))
                 *jobs*))

(defun job-check-stop ()
  "In a job's work, between two pieces of it: end the job there, :STOPPED, when
it has been asked to stop. Elsewhere, nothing."
  (when (and *job* (car (job-cancel *job*)))
    (error 'job-stopped)))

(defun job-buffer-name (job)
  "The name of the buffer JOB belongs to, for reports."
  (funcall *job-buffer-name* (job-buffer job)))

(defun condition-words (condition &key headline)
  "CONDITION in words on one line, as an error is said: its report, each line
end and run of spaces in it one space; a few words of its kind when it cannot
be printed (a lack of memory may leave none to print with). With HEADLINE,
only its first words, up to its first stop, colon or parenthesis, in lower
case: \"control stack exhausted\"."
  (let* ((text (or (ignore-errors (princ-to-string condition))
                   (string-downcase (type-of condition))))
         (blank nil)
         (words (string-trim " " (with-output-to-string (out)
                                   (loop for c across text
                                         do (if (member c '(#\Space #\Tab #\Newline #\Return))
                                                (setf blank t)
                                                (progn (when blank (write-char #\Space out) (setf blank nil))
                                                       (write-char c out))))))))
    (if headline
        (string-downcase (string-trim " " (subseq words 0 (or (position-if (lambda (c) (find c ".:(")) words) (length words)))))
        words)))

;;; What differs between kinds

(defgeneric job-percent (job)
  (:documentation "How far JOB has got, all it does counted, from 0 to 99; NIL when it cannot tell.")
  (:method ((job job)) nil))

(defgeneric job-doing (job)
  (:documentation "What JOB does, in a word or a few with a capital: Sorting,
Replacing, Working out the agenda. Its state in the list of jobs, and its
stop question, are made of it."))

(defgeneric job-state-words (job)
  (:documentation "How far JOB has got, for the list of jobs: what it does and
the per cent, Sorting 42%, or what it does alone when it cannot tell.")
  (:method ((job job)) (format nil "~a~@[ ~d%~]" (job-doing job) (job-percent job))))

(defgeneric job-description (job)
  (:documentation "What JOB does, in full, for the list of jobs: Replacing a with b in the buffer.")
  (:method ((job job)) (job-doing job)))

(defgeneric job-verb (job)
  (:documentation "What JOB does as a verb, for when it fails: Could not VERB: why.")
  (:method ((job job)) (string-downcase (job-doing job))))

(defgeneric job-left-words (job)
  (:documentation "What stopping JOB leaves, for when it has stopped: Stopped: WORDS.")
  (:method ((job job)) "nothing was changed"))

(defmacro define-job-words (class &key (doing nil doing-p) (description nil description-p) (verb nil verb-p)
                                        (left-words nil left-words-p) (at-exit nil at-exit-p))
  "Say how the jobs of CLASS are told of, each a form evaluated with JOB bound
to the job (a string, most often): DOING (JOB-DOING), DESCRIPTION
\(JOB-DESCRIPTION), VERB (JOB-VERB), LEFT-WORDS (JOB-LEFT-WORDS); AT-EXIT
\(JOB-AT-EXIT), :STOP, :QUIET or :WAIT. Those not given are as for any job.
JOB is the symbol of that name where the forms are written."
  (let ((job (intern "JOB" *package*)))
    `(progn
       ,@(when doing-p `((defmethod job-doing ((,job ,class)) (declare (ignorable ,job)) ,doing)))
       ,@(when description-p `((defmethod job-description ((,job ,class)) (declare (ignorable ,job)) ,description)))
       ,@(when verb-p `((defmethod job-verb ((,job ,class)) (declare (ignorable ,job)) ,verb)))
       ,@(when left-words-p `((defmethod job-left-words ((,job ,class)) (declare (ignorable ,job)) ,left-words)))
       ,@(when at-exit-p `((defmethod job-at-exit ((,job ,class)) (declare (ignorable ,job)) ,at-exit)))
       ',class)))

(defgeneric job-consequence (job)
  (:documentation "A sentence on what stopping JOB part of the way does, for the
question before it is stopped, or NIL: The file stays as it was.")
  (:method ((job job)) nil))

(defgeneric job-stop (job)
  (:documentation "Ask JOB to stop: it ends soon after, :STOPPED, and is taken
in as any job is. By standard its CANCEL's car is set, which its work looks
at between pieces, a call of it that does not look (a regular expression's
matcher) is cut short (STOP-WORK), and the program it runs (its PROCESS), if
any, is ended with every program that one started (END-PROGRAM).")
  (:method ((job job))
    (stop-work (job-cancel job))
    (let ((process (job-process job)))
      (when process (end-program process)))))

(defun end-program (process)
  "End PROCESS, a program a job runs, and every program it started (a hook, a
helper), so that none keeps the job waiting: the programs run in a process
group of their own."
  (ignore-errors (sb-ext:process-kill process sb-unix:sigterm :process-group)))

(defun job-program-started (job process)
  "Keep PROCESS as the program JOB runs, ended when JOB is stopped; ended at
once when JOB was stopped before it started. Returns PROCESS."
  (setf (job-process job) process)
  (when (car (job-cancel job)) (end-program process))
  process)

(defgeneric job-holds-queue-p (job)
  (:documentation "True when JOB has the jobs asked for in its buffer wait their
turn behind it, from when it starts until it is taken in. A save, a list's
files copied and the like let others start beside them.")
  (:method ((job job)) t))

(defgeneric job-shows-processing-p (job)
  (:documentation "True when the mode line of JOB's buffer lights its sign of
work (P) while JOB runs, and the list of jobs shows how far it has got; NIL
for work the mode line shows in words of its own (a save).")
  (:method ((job job)) t))

(defgeneric job-at-exit (job)
  (:documentation "What is done to JOB, running, when its buffer is killed or
the editor left: :STOP it, after asking; :QUIET, stop it without asking, for
work that only shows something and is quick to do again (a sum, a count of
words, an agenda worked out, a calendar's marks); or :WAIT for it to end, after
asking (a save, which stopping would leave not done).")
  (:method ((job job)) :stop))

(defgeneric job-guarded-spans (job)
  (:documentation "The stretches of JOB's buffer whose change while JOB runs
would spoil its work, each (START . END), offsets in the text as it is now:
an insertion at START or inside, or a deletion or replacement overlapping it
\(SPAN-TOUCHED-P). A sort or filter gives its lines, dynamic blocks being made
their blocks. NIL, by standard, for work that carries the edits made meanwhile
onto its result (a replacement) or that they do not touch (a sum or a count,
of the text as it was when it began; a save, of the version it took).")
  (:method ((job job)) nil))

(defgeneric job-take-in (job)
  (:documentation "Take in JOB, which has ended, on the editor's thread: put its
result where it goes, say how it went. Called once, after JOB has left
*JOBS*.")
  (:method ((job job)) nil))

;;; Words alike for every kind

(defun job-ending-words (job)
  "How JOB ended, when it did not end done, in the words every kind says it
with: Stopped: what that left, or Could not do it: why."
  (ecase (job-status job)
    (:stopped (format nil "Stopped: ~a" (job-left-words job)))
    (:failed (format nil "Could not ~a: ~a" (job-verb job) (or (job-error job) "it ended unexpectedly")))))

(defun job-stop-question (job)
  "The question asked before JOB is stopped: Stop sorting at 42%? and what
stopping does, when the job says (JOB-CONSEQUENCE)."
  (format nil "Stop ~(~a~)~@[ at ~d%~]?~@[ ~a~]" (job-doing job) (job-percent job) (job-consequence job)))

(defun job-guard-question (job)
  "The question asked before an edit that would spoil the work of JOB, running
over the text edited, is made: Sorting works on this text (42%); stop sorting
and change it? and what stopping does, when the job says (JOB-CONSEQUENCE)."
  (format nil "~a works on this text~@[ (~d%)~]; stop ~(~a~) and change it?~@[ ~a~]"
          (job-doing job) (job-percent job) (job-doing job) (job-consequence job)))

(defun job-guard-refusal (job)
  "What is said when an edit that would spoil the work of JOB is not made: Not
changed: sorting goes on."
  (format nil "Not changed: ~(~a~) goes on" (job-doing job)))

(defun running-percent (done total)
  "How far work that has DONE of TOTAL has got, from 0 to 99: never 100 while
it runs, 0 while there is nothing to do yet."
  (min 99 (floor (* 100 done) (max 1 total))))

(defun running-job (buffer type)
  "The job of TYPE (a job structure's name) running for BUFFER, or NIL."
  (find-if (lambda (job) (and (typep job type) (eq (job-buffer job) buffer) (job-running-p job))) *jobs*))

(defun part-percent (cell)
  "How far the work CELL follows has got, from 0 to 100: CELL a cons of what is
done and what there is to do, 0 while there is nothing to do."
  (let ((done (car cell)) (total (cdr cell)))
    (if (plusp total) (min 100 (floor (* 100 done) total)) 0)))

(defun two-phase-percent (first counting &key (halves t))
  "How far work in two phases has got, from 0 to 99: writing its result, FIRST
per cent of it done, the first half, then counting the lines of what it
wrote, COUNTING a cons of the octets counted and the octets to count once that
begins, the second. With HALVES NIL the work has its first phase alone, which
is then all of it."
  (min 99 (cond (counting (+ 50 (floor (part-percent counting) 2)))
                (halves (floor first 2))
                (t first))))

(defun two-phase-words (doing first counting)
  "The state of work in two phases (TWO-PHASE-PERCENT) for the list of jobs:
DOING and FIRST per cent, Sorting 42%, then, once COUNTING is a cons, counting
the lines of what it wrote: Sorting: counting lines 42%."
  (if counting
      (format nil "~a: counting lines ~d%" doing (part-percent counting))
      (format nil "~a ~d%" doing (min 100 first))))

;;; Text a job works on

(defun span-touched-p (span start end)
  "True when a change of the text from START below END, START = END for an
insertion there, touches SPAN, a stretch (FROM . TO) of it: an insertion at
FROM or inside it, or a deletion or replacement overlapping it. O(1)."
  (let ((from (car span)) (to (cdr span)))
    (if (= start end)
        (and (<= from start) (< start to))
        (and (< start to) (> end from)))))

(defun job-guards (buffer)
  "The jobs running for BUFFER, not asked to stop, whose work a change of some
of its text would spoil (JOB-GUARDED-SPANS), each as (JOB . SPANS), the oldest
first; NIL when there are none. O(jobs), and the spans of those that guard."
  (loop for job in *jobs*
        for spans = (and (eq (job-buffer job) buffer) (job-running-p job) (not (car (job-cancel job)))
                         (job-guarded-spans job))
        when spans collect (cons job spans)))

(defun guard-touched (guards start end)
  "The job of GUARDS (JOB-GUARDS) whose work a change of the text from START
below END would spoil (SPAN-TOUCHED-P), or NIL. O(spans)."
  (loop for (job . spans) in guards
        when (some (lambda (span) (span-touched-p span start end)) spans)
          return job))

;;; Reports

(defun report-job (job event &optional details)
  "Report in the debug log that JOB, what it does and the buffer it is in, has
reached EVENT (started, ended), DETAILS, a string or NIL, said after it."
  (debug-report :job "~a in ~a ~a~@[: ~a~]" (job-description job) (job-buffer-name job) event details))

(defun report-job-ended (job)
  "Report in the debug log that JOB has ended, how, and why when it failed."
  (report-job job "ended" (format nil "~(~a~)~@[, ~a~]" (job-status job)
                                  (and (eq (job-status job) :failed) (job-error job)))))

;;; Running

(defun job-wake-editor (job)
  "Wake the editor for JOB's thread, when JOB has a way to."
  (let ((wake (job-wake job)))
    (when wake (ignore-errors (funcall wake)))))

(defun start-background-result (function name wake &optional cancel)
  "Call FUNCTION on a thread of its own named NAME, and return a cell whose car
becomes what FUNCTION returns, or (:ERROR TEXT) when it fails, a lack of
memory or stack too, WAKE (a function, or NIL) called then to have the editor look; unless CANCEL, a cell,
has turned true meanwhile, when nothing is kept; STOP-WORK on CANCEL also cuts
short a call of FUNCTION's that does not look at it (STOPPABLE-WORK)."
  (let ((cell (list nil)))
    (make-guarded-thread
     (lambda ()
       (let ((value (handler-case (if cancel (stoppable-work (cancel) (funcall function)) (funcall function))
                      (serious-condition (condition) (list :error (condition-words condition))))))
         (unless (and cancel (car cancel))
           (setf (car cell) value)
           (when wake (funcall wake)))))
     :name name)
    cell))

(defun start-job-thread (job work &key (name "tuatara job") details)
  "Start JOB: keep it in *JOBS* and run WORK, a function of JOB, on a guarded
thread called NAME, *JOB* bound to JOB there, its errors reported in the debug
log; DETAILS, a string or NIL, are reported with its start. WORK sets JOB's
status as it ends, :DONE or :STOPPED, with what it made; a return leaving it
:RUNNING ends it :DONE, or :STOPPED when it was asked to stop. Whatever else
happens, the job never stays :RUNNING: a condition signalled in WORK, a lack
of memory or stack too, or no thread to be had for it, ends it :FAILED with
the condition's words, JOB-STOPPED or anything once it was asked to stop ends
it :STOPPED; and the editor is woken as it ends. WORK is the job's stoppable
work (STOPPABLE-WORK): JOB-STOP ends it even inside a call that never looks.
Returns JOB."
  (unless (job-wake job)
    (setf (job-wake job) (and *job-waker* (funcall *job-waker*))))
  (setf *jobs* (append *jobs* (list job)))
  (report-job job "started" details)
  (flet ((run ()
           (unwind-protect
                (handler-case
                    (let ((*job* job))
                      (reporting-errors ("~a in ~a" (job-description job) (job-buffer-name job))
                        ;; M-x stop-job reaches into a call that never looks (STOP-WORK)
                        (stoppable-work ((job-cancel job))
                          (funcall work job)))
                      (when (job-running-p job)
                        (setf (job-status job) (if (car (job-cancel job)) :stopped :done))))
                  (job-stopped () (setf (job-status job) :stopped))
                  (serious-condition (condition)
                    (if (car (job-cancel job))
                        (setf (job-status job) :stopped)
                        (setf (job-error job) (condition-words condition) (job-status job) :failed))))
             ;; cut short, the thread ended from outside: never left running
             (when (job-running-p job)
               (setf (job-status job) (if (car (job-cancel job)) :stopped :failed)))
             (job-wake-editor job))))
    (handler-case (setf (job-thread job) (make-guarded-thread #'run :name name))
      ;; no thread to be had (too many, no memory): the job ends at once, failed
      (serious-condition (condition)
        (setf (job-error job) (condition-words condition) (job-status job) :failed)
        (job-wake-editor job))))
  job)

(defun forget-job (job)
  "Take JOB out of *JOBS*, never to be taken in."
  (setf *jobs* (remove job *jobs*)))

(defun take-in-jobs (&optional (test (constantly t)))
  "Take in the jobs that have ended and pass TEST, a function of a job, on the
editor's thread, the oldest first: each leaves *JOBS*, its end is reported in
the debug log, and JOB-TAKE-IN takes it in. Returns the jobs taken in."
  (let ((taken '()))
    (dolist (job (copy-list *jobs*))
      (when (and (not (job-running-p job)) (member job *jobs*) (funcall test job))
        (forget-job job)
        (report-job-ended job)
        (push job taken)
        (let ((*job-taken-in* job))
          (job-take-in job))))
    (nreverse taken)))

(defun stop-and-settle (job)
  "Ask JOB to stop and wait for its thread to end, at most *STOP-WAIT*
seconds; true when it ended, else it is reported left to end on its own."
  (job-stop job)
  (or (null (job-thread job))
      (join-within (job-thread job))
      (progn (left-running (job-thread job) (job-description job)) nil)))
