;;;; lisp/org/notify.lisp --- Notifications on the desktop of the agenda's entries tagged :notify:.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; While Tuatara runs, today's agenda is worked out on a thread of its own
;;;; about once a minute, from the agenda files as they are then, and each of
;;;; its entries tagged :notify: is told on the desktop by notify-send, some
;;;; minutes before its time and when the time comes, or, with a date alone,
;;;; once in the morning. Each is told once (NOTIFY, kept in *Notifications*
;;;; too); a task done is told no more.

(in-package #:tuatara.defaults)

(define-setting *org-notify-tag* "notify"
  "The tag of the agenda's entries told on the desktop as their time comes.
The tag is a heading's own or one it takes from those above it.
- text: the tag.
- nil: no entry is told."
  :values (or (member nil) string))

(define-setting *org-notify-before* 10
  "How many minutes before its time an entry tagged :notify: is told.
It is told when the time comes as well. An entry's NOTIFY_BEFORE property
says otherwise for it.
- a whole number: that many minutes before; 0 for only when the time comes.
- nil: only when the time comes."
  :values (or (member nil) (integer 0 *)))

(define-setting *org-notify-day-time* "08:00"
  "When an entry tagged :notify: with a date and no time of day is told.
- a time of day as text, \"08:00\": at that time of the day, or as Tuatara
  starts when that is later.
- nil: such entries are never told."
  :values (or (member nil) string))

(sb-ext:defglobal **org-notify-sent** (make-hash-table :test 'equal :synchronized t)
  "The notifications told so far, by what tells them apart: (FILE TITLE DAY
KIND TIME), KIND :BEFORE, :AT or :DAY, TIME the time of day (930), or NIL.")

(sb-ext:defglobal **org-notify-state** (list nil 0)
  "How the notifications stand, as a list: whether a thread is working them
out; when the tick looks again; what the agenda files open were the last time
they were worked out (ORG-NOTIFY-OPEN-KEY); when the next notification not yet
told is due; and when they were worked out last. The times are universal
times.")

(defparameter *org-notify-disk-check* 600
  "The most seconds between two workings out of the notifications while the
agenda files open stay as they were and none is due, so that a file another
program changed on disk is seen.")

(defparameter *org-notify-carried-settings*
  (append *org-agenda-carried-settings*
          '(*org-notify-tag* *org-notify-before* *org-notify-day-time* *notify-program*))
  "The settings the notifications are worked out with, carried to their thread.")

(defun org-notify-minutes-before (entry)
  "How many minutes before its time ENTRY is told: its NOTIFY_BEFORE
property, a whole number, else *ORG-NOTIFY-BEFORE*; 0 for none."
  (let ((own (agenda-entry-property-value entry "NOTIFY_BEFORE")))
    (or (and own (cl-ppcre:scan "^\\s*\\d{1,4}\\s*$" own) (parse-integer own))
        (and (integerp *org-notify-before*) (max 0 *org-notify-before*))
        0)))

(defun org-notify-due (items now)
  "The notifications due at the universal time NOW of ITEMS, today's agenda
lines, each (KEY TITLE WORDS) (KEY as **ORG-NOTIFY-SENT** keeps them). They are
those for a line of an entry tagged *ORG-NOTIFY-TAG*, not done, scheduled, a
deadline today, a timestamp or a range of days, or a repeating task past due,
with a time of day, from ORG-NOTIFY-MINUTES-BEFORE minutes before it until it
comes, and for five minutes from when it comes. With none, from
*ORG-NOTIFY-DAY-TIME* on that day. Those told already are left out. As a
second value, the universal time the next of the others is due, the start of
tomorrow at the latest."
  (let* ((today (org-today now))
         (next (day-universal-time (1+ today)))
         (due '()))
    (flet ((later (time) (when (< now time next) (setf next time))))
      (dolist (item items)
        (let ((entry (org-item-entry item)))
          (when (and *org-notify-tag*
                     (eql (org-item-day item) today)
                     (or (org-item-type-in item "scheduled" "deadline" "timestamp" "block")
                         ;; a repeating task left undone is due again at its time each day
                         (and (org-item-type-in item "past-scheduled")
                              (let ((facts (agenda-entry-facts entry)))
                                (or (third (getf facts :scheduled))
                                    (find :scheduled (getf facts :rules) :key #'first)))))
                     (member *org-notify-tag* (agenda-entry-tags entry) :test #'string=)
                     (not (org-done-p entry)))
            (let ((title (org-item-title item))
                  (kind (cond ((org-item-type-in item "deadline") "Deadline")
                              ((org-item-type-in item "scheduled" "past-scheduled") "Scheduled")
                              (t nil))))
              (flet ((add (which words)
                       (let ((key (list (agenda-entry-file entry) title today which (org-item-time item))))
                         (unless (gethash key **org-notify-sent**)
                           (push (list key title (format nil "~a~@[ · ~a~]~@[ · ~a~]" words kind
                                                         (agenda-entry-category entry)))
                                 due)))))
                (let ((time (or (org-item-time item)
                                ;; past due, the line has no time: its date's is the one
                                (and (org-item-type-in item "past-scheduled")
                                     (org-time-of-day (or (agenda-entry-scheduled entry) ""))))))
                  (if time
                      (multiple-value-bind (hour minute) (org-time-hour-minute time)
                        (let ((event (day-universal-time today hour minute))
                              (before (* 60 (org-notify-minutes-before entry)))
                              (clock (time-range-text hour minute)))
                          (cond ((and (plusp before) (<= (- event before) now) (< now event))
                                 (add :before (format nil "~a, in ~d min" clock (ceiling (- event now) 60))))
                                ((and (<= event now) (< now (+ event 300)))
                                 (add :at (format nil "~a, now" clock))))
                          (when (plusp before) (later (- event before)))
                          (later event)))
                      (multiple-value-bind (hour minute end-hour) (read-time-range (or *org-notify-day-time* ""))
                        (when (and hour (null end-hour))
                          (let ((at (day-universal-time today hour minute)))
                            (if (<= at now) (add :day "Today") (later at)))))))))))))
    (values (nreverse due) next)))

(defun org-notify-work (snapshot now)
  "On a thread of its own: today's agenda from SNAPSHOT (ORG-AGENDA-SNAPSHOT)
at the universal time NOW, and each notification due (ORG-NOTIFY-DUE) told
\(NOTIFY) and kept as told. Returns when the next is due, a universal time."
  (multiple-value-bind (due next)
      (let ((*org-agenda-snapshot* snapshot)
            (*org-agenda-ts-memo* (make-hash-table :test 'equal))
            (*org-agenda-time-memo* (make-hash-table :test 'equal))
            (*org-now* now))
        (org-notify-due (org-agenda-day (org-today now) :files (mapcar #'first snapshot) :log nil) now))
    ;; those of days gone are no longer needed
    (let ((today (org-today now)))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (when (< (third key) today) (remhash key **org-notify-sent**)))
               **org-notify-sent**))
    (dolist (notification due)
      (destructuring-bind (key title words) notification
        (setf (gethash key **org-notify-sent**) now)
        (notify title words :source "Agenda")))
    next))

(defun org-notify-open-key ()
  "What tells whether the Org files open changed: each Org buffer's file and
version."
  (loop for buffer in tuatara.buffer:*buffers*
        for file = (tuatara.buffer:buffer-file buffer)
        when (and file (typep (buffer-major-mode buffer) 'org-mode))
          collect (cons file (tuatara.buffer:buffer-version buffer))))

(defun org-notify-tick (editor)
  "Tick function: about once a minute, while *ORG-NOTIFY-TAG* is set, work out
on a thread of its own which entries tagged with it are due to be told
\(ORG-NOTIFY-WORK), the agenda files open taken here and those on disk
looked at there; only when an Org file open changed, a notification has come
due, or *ORG-NOTIFY-DISK-CHECK* seconds have gone by. The seconds until it
looks again; NIL with no tag set."
  (declare (ignore editor))
  (let ((now (get-universal-time)))
    (destructuring-bind (busy look &optional open-key (due 0) (last 0)) **org-notify-state**
      (when (and *org-notify-tag* (not busy) (>= now look))
        (let ((key (org-notify-open-key)))
          (if (or (not (equal key open-key)) (>= now due) (>= now (+ last *org-notify-disk-check*)))
              (progn
                (setf **org-notify-state** (list t (+ now 60) key due now))
                (org-agenda-in-background "tuatara notifications" *org-notify-carried-settings*
                                          (lambda (snapshot)
                                            (let ((next nil))
                                              (unwind-protect
                                                   (setf next (org-notify-work snapshot
                                                                               (or *org-now* (get-universal-time))))
                                                (setf (first **org-notify-state**) nil
                                                      (fourth **org-notify-state**) (or next 0)))))))
              (setf (second **org-notify-state**) (+ now 60))))))
    (and *org-notify-tag* (max 1 (- (second **org-notify-state**) now)))))

(defun install-org-notify ()
  "Tell the agenda's entries tagged :notify: on the desktop while Tuatara runs."
  (pushnew 'org-notify-tick *tick-functions*))
