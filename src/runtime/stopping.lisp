;;;; src/runtime/stopping.lisp --- Work that is stopped at once, even inside one long call.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Most work looks now and then whether it has been asked to stop: the car
;;;; of a cons, its cancel, turned true. One call that never looks, a regular
;;;; expression's matcher stuck on a line it can match in a great many ways,
;;;; would never hear it. So such a call runs as STOPPABLE, and the work it is
;;;; part of as STOPPABLE-WORK on its cancel: STOP-WORK then interrupts every
;;;; thread inside a stoppable call of that work, which leaves the work at
;;;; once, as though it had looked. Only the stoppable calls are cut short,
;;;; never code that holds a lock or is half-way through changing something,
;;;; so nothing is left behind. Work given a number of seconds is also cut
;;;; short, as slow, when one of its stoppable calls runs longer than that, so
;;;; that what the editor's thread does never holds it up; the caller then
;;;; carries the work on in the background. Batches on the worker pool are part
;;;; of the work that runs them (RUN-TASKS).

(in-package #:tuatara.runtime)

(defvar *stop-cells* '()
  "On a thread, the cancels of the work it does (STOPPABLE-WORK), the
innermost first.")

(defvar *stoppable-call* nil
  "On a thread inside a stoppable call (STOPPABLE), which of the thread's
calls it is, a number; NIL elsewhere.")

(defvar *stoppable-calls* nil
  "On a thread doing stoppable work, a cons made for that work whose car
counts its stoppable calls, so that each call is told from any other, of this
work or another, without reading the clock.")

(defvar *slow-call-seconds* 1/40
  "Seconds one stoppable call of work run on the editor's thread may take
\(STOPPABLE-WORK with :SLOW) before the work is cut short as slow and carried
on in the background: a regular expression stuck on one line answers no key
for longer than this.")

(defstruct (stop-entry (:constructor make-stop-entry (slow)) (:copier nil))
  "The threads doing the work of one cancel (STOPPABLE-WORK), one entry for
each time a thread began it, and SLOW, the seconds one stoppable call of it
may take, or NIL."
  (threads '()) slow)

(setf (documentation 'make-stop-entry 'function) "Return a stop entry for work whose calls may take SLOW seconds, or NIL for no limit."
      (documentation 'stop-entry-p 'function) "True when OBJECT is a STOP-ENTRY."
      (documentation 'stop-entry-threads 'function) "The threads doing a cancel's work."
      (documentation 'stop-entry-slow 'function) "Seconds one stoppable call of a cancel's work may take, or NIL.")

(sb-ext:defglobal **stop-entries** (make-hash-table :test 'eq)
  "For each cancel whose work runs now (STOPPABLE-WORK), its STOP-ENTRY.")

(sb-ext:defglobal **stop-lock** (sb-thread:make-mutex :name "tuatara stoppable work")
  "Guards **STOP-ENTRIES** and the watching thread.")

(sb-ext:defglobal **stop-watch** nil
  "The thread watching work that is slow or asked to stop (WATCH-STOPPABLE-WORK), once made.")

(sb-ext:defglobal **stop-watch-wake** (sb-thread:make-semaphore :name "tuatara stoppable work")
  "Signalled when there is work for the watching thread to watch.")

(defun check-stopped ()
  "Leave the work this thread does as soon as any of it has been asked to stop
\(STOP-WORK): to the outermost STOPPABLE-WORK whose cancel's car is true. Else
nothing."
  (let ((outermost nil))
    (dolist (cell *stop-cells*)
      (when (car cell) (setf outermost cell)))
    (when outermost (throw outermost (car outermost)))))

(defun stop-stoppable-call ()
  "What an interrupted thread does (STOP-WORK): leave its work when it is in a
stoppable call of work asked to stop."
  (when *stoppable-call* (check-stopped)))

(defun call-stoppable (function)
  "Call FUNCTION as STOPPABLE does, within work that can be stopped."
  (if *stoppable-call*
      (funcall function)
      (let ((*stoppable-call* (incf (the fixnum (car *stoppable-calls*)))))
        (check-stopped)
        (funcall function))))

(defmacro stoppable (&body body)
  "Run BODY, a call that may take long without looking whether it is to
stop (a regular expression's matcher), so that it is cut short as soon as the
work it is part of is stopped (STOPPABLE-WORK, STOP-WORK). BODY must change
nothing outside itself and hold no lock: it may be left at any point. Costs
nothing outside stoppable work."
  (let ((name (gensym "STOPPABLE")))
    `(flet ((,name () ,@body))
       (declare (dynamic-extent #',name))
       (if *stop-cells* (call-stoppable #',name) (,name)))))

(defun ensure-stop-watch ()
  "Start the thread watching stoppable work, unless it runs; **STOP-LOCK** held."
  (unless (and **stop-watch** (sb-thread:thread-alive-p **stop-watch**))
    (setf **stop-watch** (make-guarded-thread #'watch-stoppable-work :name "tuatara stoppable work"))))

(defun enter-stoppable-work (cancel slow)
  "Note that this thread begins the work of CANCEL, whose calls may take SLOW seconds (or NIL)."
  (sb-thread:with-mutex (**stop-lock**)
    (let ((entry (or (gethash cancel **stop-entries**)
                     (setf (gethash cancel **stop-entries**) (make-stop-entry slow)))))
      (push sb-thread:*current-thread* (stop-entry-threads entry))
      (when (stop-entry-slow entry)
        (ensure-stop-watch)
        (sb-thread:signal-semaphore **stop-watch-wake**)))))

(defun leave-stoppable-work (cancel)
  "Note that this thread has left the work of CANCEL."
  (sb-thread:with-mutex (**stop-lock**)
    (let ((entry (gethash cancel **stop-entries**)))
      (when entry
        (setf (stop-entry-threads entry) (remove sb-thread:*current-thread* (stop-entry-threads entry) :count 1))
        (unless (stop-entry-threads entry)
          (remhash cancel **stop-entries**))))))

(defun call-stoppable-work (cancel slow function)
  "Call FUNCTION as STOPPABLE-WORK does."
  (enter-stoppable-work cancel slow)
  (unwind-protect
       (let ((values nil) (done nil))
         (let ((reason (catch cancel
                         (let ((*stop-cells* (cons cancel *stop-cells*))
                               (*stoppable-calls* (or *stoppable-calls* (list 0))))
                           (setf values (multiple-value-list (funcall function)) done t)))))
           (if done (values-list values) (values nil reason))))
    (leave-stoppable-work cancel)))

(defmacro stoppable-work ((cancel &key slow) &body body)
  "Run BODY as the work CANCEL, a cons, stops: once STOP-WORK turns CANCEL's
car true, a stoppable call (STOPPABLE) of BODY's, under way or begun after,
on this thread or on the worker pool for it, ends BODY at once; what BODY
does between such calls goes on until it looks at CANCEL itself. Returns
BODY's values, or NIL and CANCEL's car when BODY was stopped so. With SLOW, seconds, BODY is also
stopped, CANCEL's car :SLOW, when one of its stoppable calls takes longer:
for work on the editor's thread, which then goes on with it in the
background."
  `(call-stoppable-work ,cancel ,slow (lambda () ,@body)))

(defun call-within-stop-cells (cells function)
  "Call FUNCTION as part of the work of each of CELLS, cancels the innermost
first (as *STOP-CELLS* holds them): on another thread, a piece of the work a
thread with those cells does. True when FUNCTION returned, NIL when it was cut
short."
  (if (null cells)
      (progn (funcall function) t)
      (call-stoppable-work (car (last cells)) nil
                           (lambda () (call-within-stop-cells (butlast cells) function)))))

(defun stop-work (cancel &optional (reason t))
  "Ask the work of CANCEL, a cons, to stop: its car turns REASON (when it is not
true already), which the work looks at between its pieces, and every thread in
a stoppable call of it (STOPPABLE-WORK) leaves the work at once. Returns
CANCEL."
  (unless (car cancel) (setf (car cancel) reason))
  (sb-thread:barrier (:memory))
  (interrupt-stoppable cancel)
  (sb-thread:with-mutex (**stop-lock**)
    (when (gethash cancel **stop-entries**)
      ;; the watch repeats it for a thread that was just entering a call
      (ensure-stop-watch)
      (sb-thread:signal-semaphore **stop-watch-wake**)))
  cancel)

(defun interrupt-stoppable (cancel)
  "Interrupt the threads doing CANCEL's work that are in a stoppable call, this
one aside, to leave it (STOP-STOPPABLE-CALL)."
  (let ((self sb-thread:*current-thread*))
    (dolist (thread (sb-thread:with-mutex (**stop-lock**)
                      (let ((entry (gethash cancel **stop-entries**)))
                        (and entry (copy-list (stop-entry-threads entry))))))
      (unless (or (eq thread self) (not (thread-in-stoppable-call thread)))
        (ignore-errors (sb-thread:interrupt-thread thread #'stop-stoppable-call))))))

(defun thread-in-stoppable-call (thread)
  "Which of THREAD's stoppable calls it is in (*STOPPABLE-CALL*), or NIL when it is in none."
  (ignore-errors (values (sb-thread:symbol-value-in-thread '*stoppable-call* thread nil))))

(sb-ext:defglobal **calls-seen** (make-hash-table :test 'eq)
  "For the watching thread: each thread seen in a stoppable call of work given
seconds, as (COUNTER CALL TIME): the call, told by its work's counter
\(*STOPPABLE-CALLS*) and number, and the internal real time it was first seen
in it.")

(defun stoppable-call-seconds (thread call now)
  "The seconds THREAD has been seen in its stoppable call CALL, up to NOW, an
internal real time, kept for the next look in **CALLS-SEEN**."
  (let ((seen (gethash thread **calls-seen**))
        (counter (ignore-errors (values (sb-thread:symbol-value-in-thread '*stoppable-calls* thread nil)))))
    (if (and seen (eq (first seen) counter) (eql (second seen) call))
        (/ (- now (third seen)) internal-time-units-per-second)
        (progn (setf (gethash thread **calls-seen**) (list counter call now)) 0))))

(defun innermost-slow-cancel (thread)
  "The innermost cancel of THREAD's work given seconds for its calls; **STOP-LOCK** held."
  (find-if (lambda (cancel)
             (let ((entry (gethash cancel **stop-entries**)))
               (and entry (stop-entry-slow entry))))
           (ignore-errors (values (sb-thread:symbol-value-in-thread '*stop-cells* thread nil)))))

(defun watch-stoppable-work ()
  "What the watching thread does, for as long as the editor runs: every few
milliseconds while a stoppable call runs (less often while there is work
to watch but none does), stop as slow the innermost work given seconds whose
stoppable call on some thread has been seen to run longer, and interrupt
again a thread still in a stoppable call of work asked to stop; asleep while
there is none."
  (loop
    (let ((slow '()) (stopped '()) (watching nil) (calling nil) (looked '()) (now (get-internal-real-time)))
      (sb-thread:with-mutex (**stop-lock**)
        (maphash (lambda (cancel entry)
                   (cond ((car cancel)
                          (when (some #'thread-in-stoppable-call (stop-entry-threads entry))
                            (setf watching t calling t)
                            (push cancel stopped)))
                         ((stop-entry-slow entry)
                          (setf watching t)
                          (dolist (thread (stop-entry-threads entry))
                            (let ((call (thread-in-stoppable-call thread)))
                              (when call
                                (setf calling t)
                                (push thread looked)
                                (when (and (> (stoppable-call-seconds thread call now) (stop-entry-slow entry))
                                           (eq (innermost-slow-cancel thread) cancel))
                                  (pushnew cancel slow))))))))
                 **stop-entries**))
      ;; a thread no longer in a call is forgotten
      (maphash (lambda (thread seen)
                 (declare (ignore seen))
                 (unless (member thread looked) (remhash thread **calls-seen**)))
               **calls-seen**)
      (dolist (cancel slow) (stop-work cancel :slow))
      (dolist (cancel stopped) (interrupt-stoppable cancel))
      (cond (calling (sleep 0.005))
            ;; work given seconds, between its calls: looked at less often
            (watching (sleep 0.02))
            (t (sb-thread:wait-on-semaphore **stop-watch-wake**))))))
