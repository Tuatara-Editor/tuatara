;;;; lisp/files/gathering.lisp --- Things found on a thread of its own while a question waits.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; START-GATHERING runs work on a thread of its own and hands what it finds
;;;; to the editor's thread as it comes: the files a question offers, the
;;;; text of files to search, the entries of a directory. The editor is woken
;;;; as things are found, and the work stops when it is no longer wanted.

(in-package #:tuatara.defaults)

(define-record (gathering (:constructor make-gathering ()) (:copier nil))
  "What a thread of its own finds while a question waits for it (a list of
files, the text of files to search). FOUND is what it gave since it was last
taken, the newest first. COUNT is how many it gave in all. DONE is true once it
has finished. CANCEL is a cons whose car, turned true, stops it. LOCK is held
while FOUND changes. WOKEN is when it last woke the editor. FINISHED is a
semaphore signalled once it has finished (WAIT-FOR-GATHERING)."
  (lock (sb-thread:make-mutex :name "gathering") :documentation "The lock held while a GATHERING's FOUND changes.")
  (found '() :documentation "What a GATHERING's thread gave since it was last taken, newest first.")
  (count 0 :documentation "How many things a GATHERING's thread gave in all.")
  (done nil :documentation "True once a GATHERING's thread has finished.")
  (cancel (list nil) :documentation "The cons whose car, turned true, stops a GATHERING's thread.")
  (woken 0 :documentation "When a GATHERING's thread last woke the editor, in internal time.")
  (finished (sb-thread:make-semaphore :name "gathering finished")
            :documentation "The semaphore signalled once a GATHERING's thread has finished."))

(defun start-gathering (name work)
  "Run WORK on a thread named NAME, called with a function that takes each
thing found and the cancel cons (car true: stop). Returns the GATHERING, from
which TAKE-GATHERED takes what was found, on the editor's thread; the editor
is woken as things are found, every twentieth of a second at most, and when
the work ends."
  (let ((gathering (make-gathering))
        (editor *editor*))
    (flet ((wake ()
             (setf (gathering-woken gathering) (get-internal-real-time))
             (when editor (wake-editor editor))))
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (unwind-protect
              (funcall work
                       (lambda (thing)
                         (sb-thread:with-mutex ((gathering-lock gathering))
                           (push thing (gathering-found gathering))
                           (incf (gathering-count gathering)))
                         (when (> (- (get-internal-real-time) (gathering-woken gathering))
                                  (floor internal-time-units-per-second 20))
                           (wake)))
                       (gathering-cancel gathering))
           (setf (gathering-done gathering) t)
           (sb-thread:signal-semaphore (gathering-finished gathering))
           (wake)))
       :name name))
    gathering))

(defun wait-for-gathering (gathering seconds)
  "Wait until GATHERING's thread has finished, SECONDS at most, asleep
meanwhile; true when it has."
  (or (gathering-done gathering)
      (and (sb-thread:wait-on-semaphore (gathering-finished gathering) :timeout seconds) t)))

(defun take-gathered (gathering)
  "What GATHERING's thread found since this was last asked, in the order it
was found, and as a second value true once it has finished, all taken."
  (let ((done (gathering-done gathering)))
    (values (reverse (sb-thread:with-mutex ((gathering-lock gathering))
                       (shiftf (gathering-found gathering) '())))
            done)))

(defun stop-gathering (gathering)
  "Stop GATHERING's thread at its next step: what it found is no longer wanted."
  (setf (car (gathering-cancel gathering)) t))

(defun gathering-source (gathering absorb)
  "A SOURCE for COMPLETING-READ (see there) taking in what GATHERING finds
while the question is open. ABSORB is called on the editor's thread with the
things found since, in the order found (none, at times), and returns how many
there are to choose from now, which the prompt tells while more come."
  (lambda ()
    (multiple-value-bind (new done) (take-gathered gathering)
      (values (and new t) (not done) (funcall absorb new)))))
