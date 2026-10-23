;;;; lisp/org/notify.lisp --- Notifications on the desktop of the agenda's entries tagged :notify:.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; While Tuatara runs, today's agenda is worked out on a thread of its own
;;;; about once a minute, from the agenda files as they are then, and each of
;;;; its entries tagged :notify: is told on the desktop by notify-send: some
;;;; minutes before its time and when the time comes, or, with a date alone,
;;;; once in the morning. Each is told once; a task done is told no more.

(in-package #:tuatara.defaults)

(defvar *org-notify-tag* "notify"
  "The tag of the agenda's entries told on the desktop as their time comes
\(a heading's own or one it takes from those above it); NIL tells none.")

(defvar *org-notify-before* 10
  "How many minutes before its time an entry tagged :notify: is told, as
well as when the time comes; 0 or NIL for only then. An entry's NOTIFY_BEFORE
property says otherwise for it.")

(defvar *org-notify-day-time* "08:00"
  "When an entry tagged :notify: with a date and no time of day is told on
that day, or as Tuatara starts when that is later; NIL never tells them.")

(defvar *org-notify-program* "notify-send"
  "The program that shows a notification on the desktop: a name looked for
on the PATH, or a path. It is given the title and the words, as notify-send
takes them.")

(sb-ext:defglobal **org-notify-sent** (make-hash-table :test 'equal :synchronized t)
  "The notifications told so far, by what tells them apart: (FILE TITLE DAY
KIND TIME), KIND :BEFORE, :AT or :DAY, TIME the time of day (930), or NIL.")

(sb-ext:defglobal **org-notify-state** (list nil 0 nil nil)
  "How the notifications stand: whether a thread is working them out, when
the next is due as a universal time, the program found missing (its name),
and whether that was said.")

(defparameter *org-notify-carried-settings*
  (append *org-agenda-carried-settings*
          '(*org-notify-tag* *org-notify-before* *org-notify-day-time* *org-notify-program*))
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
lines, each (KEY TITLE WORDS) (KEY as **ORG-NOTIFY-SENT** keeps them): for a
line of an entry tagged *ORG-NOTIFY-TAG*, not done, scheduled, a deadline
today, a timestamp or a range of days, or a repeating task past due, with
a time of day, from
ORG-NOTIFY-MINUTES-BEFORE minutes before it until it comes, and for five
minutes from when it comes; with none, from *ORG-NOTIFY-DAY-TIME* on that
day. Those told already are left out."
  (let ((today (org-today now)) (due '()))
    (dolist (item items (nreverse due))
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
                         (push (list key title (format nil "~a~@[ · ~a~]~@[ · ~a~]" words kind (agenda-entry-category entry)))
                               due)))))
              (let ((time (or (org-item-time item)
                              ;; past due, the line has no time: its date's is the one
                              (and (org-item-type-in item "past-scheduled")
                                   (org-time-of-day (or (agenda-entry-scheduled entry) ""))))))
                (if time
                    (let* ((event (day-universal-time today (floor time 100) (mod time 100)))
                           (before (* 60 (org-notify-minutes-before entry)))
                           (clock (time-range-text (floor time 100) (mod time 100))))
                      (cond ((and (plusp before) (<= (- event before) now) (< now event))
                             (add :before (format nil "~a, in ~d min" clock (ceiling (- event now) 60))))
                            ((and (<= event now) (< now (+ event 300)))
                             (add :at (format nil "~a, now" clock)))))
                    (multiple-value-bind (hour minute end-hour) (read-time-range (or *org-notify-day-time* ""))
                      (when (and hour (null end-hour) (<= (day-universal-time today hour minute) now))
                        (add :day "Today"))))))))))))

(defun org-notify-arguments (title words)
  "The arguments *ORG-NOTIFY-PROGRAM* is given to show TITLE and WORDS: its
name as Tuatara's, and -- before them, so that a title beginning with - is
shown as it is."
  (list "--app-name=Tuatara" "--" title words))

(defun org-notify-work (snapshot now)
  "On a thread of its own: today's agenda from SNAPSHOT (ORG-AGENDA-SNAPSHOT)
at the universal time NOW, and each notification due (ORG-NOTIFY-DUE) shown by
*ORG-NOTIFY-PROGRAM* and kept as told; the program's name when it is not
installed."
  (let* ((*org-agenda-snapshot* snapshot)
         (*org-agenda-ts-memo* (make-hash-table :test 'equal))
         (*org-agenda-time-memo* (make-hash-table :test 'equal))
         (*org-now* now)
         (files (mapcar #'first snapshot))
         (due (org-notify-due (org-agenda-day (org-today now) :files files :log nil) now)))
    ;; those of days gone are no longer needed
    (let ((today (org-today now)))
      (maphash (lambda (key value) (declare (ignore value)) (when (< (third key) today) (remhash key **org-notify-sent**)))
               **org-notify-sent**))
    (when due
      (let ((program (tuatara.io:find-program *org-notify-program*)))
        (if (null program)
            *org-notify-program*
            (dolist (notification due)
              (destructuring-bind (key title words) notification
                (setf (gethash key **org-notify-sent**) now)
                (start-detached program (org-notify-arguments title words) :label "notification"))))))))

(defun org-notify-tick (editor)
  "Tick function: about once a minute, while *ORG-NOTIFY-TAG* is set, work out
on a thread of its own which entries tagged with it are due to be told
\(ORG-NOTIFY-WORK), and say once when the program that tells them is not
installed. The seconds until it looks again; NIL with no tag set."
  (declare (ignore editor))
  (destructuring-bind (running next missing said) **org-notify-state**
    (declare (ignore next))
    (when (and missing (not said))
      (setf (fourth **org-notify-state**) t)
      (message "~a is not installed: entries tagged :~a: are not told on the desktop (Fedora: libnotify)"
               missing *org-notify-tag*))
    (let ((now (get-universal-time)))
      (when (and *org-notify-tag* (not running) (>= now (second **org-notify-state**)))
        (let ((snapshot (org-agenda-snapshot)))
          (setf (first **org-notify-state**) t
                (second **org-notify-state**) (+ now 60))
          (tuatara.runtime:make-guarded-thread
           (tuatara.runtime:carry-settings
            *org-notify-carried-settings*
            (lambda ()
              (unwind-protect
                   (setf (third **org-notify-state**) (org-notify-work snapshot (or *org-now* (get-universal-time))))
                (setf (first **org-notify-state**) nil))))
           :name "tuatara notifications")))
      (and *org-notify-tag* (max 1 (- (second **org-notify-state**) now))))))

(defun org-notify-health-words ()
  "How entries tagged :notify: are told here, as M-x check-health says it."
  (program-health-words (format nil "agenda entries tagged :~a: told on the desktop" (or *org-notify-tag* "notify"))
                        (and *org-notify-program* (tuatara.io:find-program *org-notify-program*))
                        (format nil "~a not found (libnotify)" *org-notify-program*)
                        :how (format nil "~@[~d min before and ~]at their time" (and (integerp *org-notify-before*) (plusp *org-notify-before*) *org-notify-before*))))

(defun install-org-notify ()
  "Tell the agenda's entries tagged :notify: on the desktop while Tuatara runs."
  (pushnew 'org-notify-tick *tick-functions*))
