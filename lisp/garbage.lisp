;;;; lisp/garbage.lisp --- Collecting garbage: when, what room the heap has, and how long it stood still.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; SBCL collects garbage by stopping every thread while it copies what is
;;;; still in use out of the space being collected: typing, drawing and work
;;;; in the background all wait for it. What Tuatara decides of that is here:
;;;; how much new data makes a collection, collecting while nothing is typed
;;;; rather than between keys, the room left in the heap for work that plans
;;;; its memory, and how long each collection took, for debug mode.

(in-package #:tuatara.defaults)

;;; The heap

(defun heap-free ()
  "Octets of heap free now, garbage not yet collected counted as taken."
  (max 0 (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage))))

(defun heap-budget (setting)
  "The octets a piece of work may plan to use now: SETTING, a number of
octets, or when it is NIL a quarter of the heap free now (HEAP-FREE)."
  (or setting (floor (heap-free) 4)))

(defun heap-room-p (octets)
  "True when the heap has OCTETS free, its garbage collected first when it has not."
  (or (> (heap-free) octets)
      (progn (sb-ext:gc :full t) (> (heap-free) octets))))

;;; How often

(defparameter *nursery-octets* (* 256 1024 1024)
  "How many octets of new data are made between two collections of garbage:
more than SBCL's standard, so that collections come seldom; each takes as
long as what is still in use takes to copy, not as long as this.")

(defun set-collection-policy ()
  "Have garbage collected as Tuatara wants it (*NURSERY-OCTETS*), as the
editor starts."
  (setf (sb-ext:bytes-consed-between-gcs) *nursery-octets*))

;;; Collected while nothing is typed

(defvar *start-up-garbage-collected* nil
  "True once the garbage of starting the editor has been collected (START-UP-GARBAGE-TICK).")

(defvar *start-up-tick-time* nil
  "When START-UP-GARBAGE-TICK was last called, in internal time units; NIL before.")

(defun start-up-garbage-tick (editor)
  "Between commands, once a session: in the first second nothing is typed
after the editor starts, collect at once the garbage starting it made (the
init file read, packages compiled), so that this longer pause comes while
nothing is typed rather than later, between keys."
  (declare (ignore editor))
  (unless *start-up-garbage-collected*
    (let ((now (get-internal-real-time)))
      (cond ((and *start-up-tick-time*
                  (>= (- now *start-up-tick-time*) (* 9/10 internal-time-units-per-second)))
             (setf *start-up-garbage-collected* t)
             (sb-ext:gc :full t)
             nil)
            (t (setf *start-up-tick-time* now)
               1)))))

(defvar *idle-collection-seconds* 0.3
  "Seconds you leave the editor alone, no key typed, after which the garbage
made since the last collection is collected, when it is at least half of
what makes one (*NURSERY-OCTETS*): the pause comes while you think, not
between two keys. NIL to leave every collection to come when it comes.")

(defvar *consed-at-gc* 0
  "The octets of data made in all (SB-EXT:GET-BYTES-CONSED) when garbage was
last collected.")

(defun consed-since-gc ()
  "Octets of data made since garbage was last collected."
  (- (sb-ext:get-bytes-consed) *consed-at-gc*))

(defun idle-garbage-tick (editor)
  "Between commands: when you have left the editor alone for
*IDLE-COLLECTION-SECONDS* and half a nursery of new data (*NURSERY-OCTETS*)
waits to be collected, collect it now; while it waits and you are still
about, look again when that time is up."
  (declare (ignore editor))
  (when (and *idle-collection-seconds* (>= (consed-since-gc) (floor *nursery-octets* 2)))
    (let ((idle (/ (- (get-internal-real-time) tuatara.command:*last-input-time*) internal-time-units-per-second)))
      (if (>= idle *idle-collection-seconds*)
          (progn (sb-ext:gc) nil)
          (- *idle-collection-seconds* idle)))))

;;; How long each collection stood still

(defvar *gc-time-seen* 0
  "SBCL's time stood still for collections (SB-EXT:*GC-REAL-TIME*) as it was
after the last collection, in internal time units: what the last one took is
the difference.")

(defun report-gc ()
  "After each collection of garbage: note what it took and the data made so
far (*CONSED-AT-GC*), and while debug mode is on, report how long the editor
stood still for it and how much of the heap is in use."
  (let ((spent (- sb-ext:*gc-real-time* *gc-time-seen*)))
    (setf *gc-time-seen* sb-ext:*gc-real-time*
          *consed-at-gc* (sb-ext:get-bytes-consed))
    (when tuatara.runtime:*debug-on*
      (tuatara.runtime:queue-debug-report :gc (format nil "garbage collected in ~,1f ms; ~,1f MB of the heap in use; generations 0-5 hold ~{~,1f~^ ~} MB"
                                      (/ (* 1000 spent) internal-time-units-per-second)
                                      (/ (sb-kernel:dynamic-usage) 1048576.0)
                                      (generation-megabytes))))))

(defun generation-megabytes ()
  "How many megabytes each of SBCL's generations 0 to 5 holds now, the
youngest first: data that lives on is moved from one to the next, and a
collection of one copies what lives in it and every younger one, so that a
long pause shows as an older generation that has shrunk."
  (loop for generation from 0 to 5
        collect (/ (sb-ext:generation-bytes-allocated generation) 1048576.0)))

(pushnew 'report-gc sb-ext:*after-gc-hooks*)
