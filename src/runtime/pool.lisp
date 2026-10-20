;;;; src/runtime/pool.lisp --- The worker pool: long-lived threads for parallel work.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Work that splits into independent pieces (indexing a mapped file, finding
;;;; every match, later parsing and scanning) runs on a pool of threads made
;;;; once and kept, instead of threads made and joined per call: creating and
;;;; joining 32 threads costs milliseconds and makes timings wander.
;;;;
;;;; A batch of tasks is offered to as many workers as it can use; the calling
;;;; thread works on the batch too and waits only for pieces already started
;;;; elsewhere. So a task may itself run a parallel batch without deadlock:
;;;; even with every worker busy, the caller finishes its own batch alone.
;;;; Workers never touch editor state.

(in-package #:tuatara.runtime)

(defun wait-on (waitqueue mutex seconds)
  "Wait on WAITQUEUE at most SECONDS, MUTEX held before and after, as
SB-THREAD:CONDITION-WAIT does, except that MUTEX is held again even when the
time runs out (CONDITION-WAIT then returns without it, and a loop around it
would wait again, or read and write, without the lock). True when woken."
  (or (sb-thread:condition-wait waitqueue mutex :timeout seconds)
      (progn (unless (sb-thread:holding-mutex-p mutex)
               (sb-thread:grab-mutex mutex))
             nil)))

(defun wait-on-until (waitqueue mutex done-p seconds)
  "Wait on WAITQUEUE, MUTEX held before and after (WAIT-ON), until DONE-P, a
function called with MUTEX held, returns true or SECONDS have passed (NIL: as
long as it takes). Returns what DONE-P last returned."
  (let ((deadline (and seconds (deadline-in seconds))))
    (loop
      (let ((done (funcall done-p)))
        (when done (return done))
        (let ((left (and deadline (seconds-left deadline))))
          (when (and left (<= left 0)) (return nil))
          (wait-on waitqueue mutex left))))))

(defun cpu-count ()
  "Number of online processors, or 4 if that cannot be determined."
  (handler-case
      (let ((n (sb-alien:alien-funcall
                (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
                84)))                   ; _SC_NPROCESSORS_ONLN on Linux
        (if (plusp n) n 4))
    (error () 4)))

(defun worker-count ()
  "How many worker threads the pool runs: nine in ten of the processors,
rounded down, at least one, so that work in the background (counting a
file's lines, searching) always leaves the editor's own thread room to
answer keys, on a machine with few processors as on one with many."
  (max 1 (floor (* 9 (cpu-count)) 10)))

(defstruct (batch (:constructor make-batch (tasks)) (:copier nil))
  "A set of TASKS (thunks) being run: RESULTS and ERRORS by task index, NEXT
the index the next worker takes, REMAINING the tasks not yet finished, and
DONE, signalled when REMAINING reaches zero."
  (tasks #() :type simple-vector :read-only t)
  (results #() :type simple-vector)
  (errors #() :type simple-vector)
  (next (list 0) :read-only t)
  (remaining (list 0) :read-only t)
  (done (sb-thread:make-semaphore :count 0) :read-only t))

(setf (documentation 'make-batch 'function) "Return a batch for TASKS, a simple-vector of thunks."
      (documentation 'batch-p 'function) "True when OBJECT is a BATCH."
      (documentation 'batch-tasks 'function) "The thunks of the batch."
      (documentation 'batch-results 'function) "The results of the batch's tasks, by index."
      (documentation 'batch-errors 'function) "The errors the batch's tasks signalled, by index, or NIL."
      (documentation 'batch-next 'function) "A cons whose car is the index of the next task to take."
      (documentation 'batch-remaining 'function) "A cons whose car counts the tasks not yet finished."
      (documentation 'batch-done 'function) "The semaphore signalled when the last task finishes.")

(defstruct (pool (:constructor %make-pool (size)) (:copier nil))
  "The worker pool: SIZE threads waiting on MAILBOX for batches to help with."
  (size 1 :type fixnum :read-only t)
  (mailbox (sb-concurrency:make-mailbox :name "tuatara pool") :read-only t)
  (threads '()))

(setf (documentation 'pool-p 'function) "True when OBJECT is a POOL."
      (documentation 'pool-size 'function) "How many worker threads the pool has."
      (documentation 'pool-mailbox 'function) "Where batches wait for a free worker."
      (documentation 'pool-threads 'function) "The pool's worker threads.")

(defvar *pool* nil
  "The worker pool, made on first use with as many workers as WORKER-COUNT says.")

(defvar *pool-lock* (sb-thread:make-mutex :name "tuatara pool creation")
  "Guards making the pool, so two threads asking at once make one.")

(defun work-on (batch)
  "Take tasks from BATCH until none are left, recording results and errors;
signal DONE when the last task of the batch finishes."
  (let* ((tasks (batch-tasks batch)) (n (length tasks)))
    ;; ATOMIC-INCF returns the value before the increment: the index this worker takes
    (loop for i = (sb-ext:atomic-incf (car (batch-next batch)))
          while (< i n)
          do (unwind-protect
                  ;; a lack of memory too: the batch must end, and say so where it is waited for
                  (handler-case (setf (aref (batch-results batch) i) (funcall (aref tasks i)))
                    (serious-condition (e) (setf (aref (batch-errors batch) i) e)))
               ;; ATOMIC-DECF also returns the value before: 1 means this was the last task
               (when (= 1 (sb-ext:atomic-decf (car (batch-remaining batch))))
                 (sb-thread:signal-semaphore (batch-done batch)))))))

(defun worker-loop (pool)
  "What a pool thread does: wait for a batch, help with it, repeat."
  (loop (work-on (sb-concurrency:receive-message (pool-mailbox pool)))))

(defun ensure-pool ()
  "The worker pool, made with WORKER-COUNT threads the first time it is needed."
  (or *pool*
      (sb-thread:with-mutex (*pool-lock*)
        (or *pool*
            (let ((pool (%make-pool (worker-count))))
              (setf (pool-threads pool)
                    (loop for i below (pool-size pool)
                          collect (tuatara.runtime:make-guarded-thread #'worker-loop :name (format nil "tuatara worker ~a" i)
                                                                       :arguments (list pool))))
              (setf *pool* pool))))))

(defun run-tasks (tasks &key (parallelism (cpu-count)) discard)
  "Run TASKS, a simple-vector of thunks, in parallel on the worker pool and the
calling thread, at most PARALLELISM at a time, and return a simple-vector of
their results in order. An error in a task is signalled here once all tasks
have finished, DISCARD, when given, called first with each result of the
others, so that what they hold (an open file) is let go. With one task, or
PARALLELISM 1, runs on the calling thread. The tasks are part of the work
the calling thread does (STOPPABLE-WORK): a stoppable call of theirs is cut
short when it is stopped, on every thread, and the calling thread then leaves
that work here."
  (let* ((n (length tasks))
         (cells *stop-cells*)
         (cut (list nil))
         (batch (make-batch (if cells
                                (map 'simple-vector
                                     (lambda (task)
                                       (lambda ()
                                         (let ((value nil))
                                           (unless (call-within-stop-cells cells (lambda () (setf value (funcall task))))
                                             (setf (car cut) t))
                                           value)))
                                     tasks)
                                tasks))))
    (setf (batch-results batch) (make-array n :initial-element nil)
          (batch-errors batch) (make-array n :initial-element nil)
          (car (batch-remaining batch)) n)
    (when (plusp n)
      (let ((helpers (min (1- n) (1- (max 1 parallelism)))))
        (when (plusp helpers)
          (let ((pool (ensure-pool)))
            (loop repeat (min helpers (pool-size pool))
                  do (sb-concurrency:send-message (pool-mailbox pool) batch))))
        (work-on batch)
        ;; pieces taken by workers may still be running
        (sb-thread:wait-on-semaphore (batch-done batch))))
    (let ((e (find-if #'identity (batch-errors batch))))
      (when e
        (when discard
          (loop for result across (batch-results batch) when result do (ignore-errors (funcall discard result))))
        (error e)))
    (when (car cut) (check-stopped))
    (batch-results batch)))
