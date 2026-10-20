;;;; lisp/org/schedule.lisp --- Org tasks scheduled and given deadlines, dates read as Org reads them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-s schedules the task point is under, C-c C-d gives it a deadline:
;;;; the date is asked for as any date is (read-date: fri, +3d, 10-15,
;;;; sep 30, tomorrow, 2026-12-24 14:00, 3pm), the timestamp it reads shown as it is
;;;; typed, a date already past taken in the year or month to come. The
;;;; planning line under the headline gets it, as Org writes it, a repeater
;;;; and warning there before kept; with C-u it is taken away. With
;;;; *org-log-reschedule* and *org-log-redeadline* the date it had is logged.

(in-package #:tuatara.defaults)

(defvar *org-log-reschedule* nil
  "What Org notes when a task's scheduled date is changed or taken away: NIL
nothing, :TIME the date it had and when.")

(defvar *org-log-redeadline* nil
  "What Org notes when a task's deadline is changed or taken away: NIL
nothing, :TIME the deadline it had and when.")

(defun org-date-ts (day hour minute end-hour end-minute)
  "An active timestamp, an ORG-TS, of the day number DAY, at HOUR:MINUTE
when HOUR is given, ending at END-HOUR:END-MINUTE when that is given."
  (org-ts-set-day (make-org-ts :active t :hour hour :minute minute :end-hour end-hour :end-minute end-minute) day))

(defun org-read-date-text (input now)
  "The active timestamp INPUT, a date as typed, stands for at the universal
time NOW, an ORG-TS, or NIL when INPUT is no date: what READ-DATE-TEXT reads
\(fri, +3d, 10-15, sep 30, tomorrow, 2026-12-24 14:00, 3pm)."
  (multiple-value-bind (day hour minute end-hour end-minute) (read-date-text input (org-today now))
    (and day (org-date-ts day hour minute end-hour end-minute))))

(defvar *org-read-date-popup-calendar* t
  "True for Org's date prompt (\\[org-schedule], \\[org-deadline]) to show the
calendar below while it asks: the day
read from what is typed marked there, \\`S-<left>' and \\`S-<right>' a day
either way, \\`S-<up>' and \\`S-<down>' a week, \\`M-S-<left>' and
\\`M-S-<right>' a month, \\`M-S-<up>' and \\`M-S-<down>' a year, \\`C-.' today,
the date then written at the prompt; NIL to ask without it.")

(defvar *org-date-calendar* nil
  "While Org's date prompt shows the calendar: its buffer; NIL otherwise.")

(defun org-date-calendar-show (day)
  "Show the calendar below the window the date prompt was asked from, point on
the day DAY, that window staying selected; true when a window was made for it."
  (let* ((buffer (calendar-buffer))
         (selected (editor-window *editor*))
         (shown (buffer-window buffer)))
    (calendar-show-day buffer day :centre t)
    (let ((window (display-buffer buffer)))
      (select-window selected)
      (calendar-show-day buffer day)
      (and window (not shown) window))))

(defun org-date-calendar-follow (ts)
  "Put the calendar the date prompt shows on the day of TS, as it is read."
  (when (and *org-date-calendar* (live-buffer-p *org-date-calendar*))
    (calendar-show-day *org-date-calendar* (org-ts-day-number ts))))

(defun read-org-date (prompt)
  "Ask PROMPT for a date as any date is asked for (READ-DATE), what it reads
shown as it is typed as the timestamp it makes, and the calendar below, its
day marked (*ORG-READ-DATE-POPUP-CALENDAR*); a timestamp typed whole
\(<2026-10-05 Mon 09:00 +1w>) is taken as it is. The ORG-TS, or NIL when
what was typed is no date."
  (let* ((window (and *org-read-date-popup-calendar* (org-date-calendar-show (org-today))))
         (*org-date-calendar* (and *org-read-date-popup-calendar* (calendar-buffer))))
    (unwind-protect
         (multiple-value-bind (time day hour minute end-hour end-minute)
             (read-date prompt :now (org-now)
                               :shown (lambda (&rest parts) (org-ts-text (apply #'org-date-ts parts)))
                               :follow (lambda (&rest parts) (org-date-calendar-follow (apply #'org-date-ts parts)))
                               :none #'org-read-ts)
           (if (org-ts-p time) time (and time (org-date-ts day hour minute end-hour end-minute))))
      ;; the window made for the calendar goes with the question
      (when (and window (member window (editor-windows *editor*)))
        (leave-window window)))))

(defun org-date-calendar-step (days months)
  "In Org's date prompt with the calendar: move its day DAYS days and MONTHS
months on from the one marked, and write that date at the prompt, a time
typed kept after it, as the prompt reads it."
  (if (not (and *org-date-calendar* (live-buffer-p *org-date-calendar*)))
      (message "Only while a date is asked for with the calendar")
      (let* ((from (or (calendar-date-at *org-date-calendar*) (org-today)))
             (day (if (zerop months) (+ from days) (day-months-on from months)))
             (text (editor-minibuffer-text *editor*))
             (time (cl-ppcre:scan-to-strings "(?<![\\d-])\\d{1,2}:\\d{2}(?:-\\d{1,2}:\\d{2})?" text))
             (new (format nil "~a~@[ ~a~]" (format-day "%F" day) time)))
        (calendar-show-day *org-date-calendar* day)
        (setf (editor-minibuffer-text *editor*) new
              (editor-minibuffer-point *editor*) (length new)))))

(define-command org-calendar-forward-day ()
  "In Org's date prompt: the calendar's day a day on, written at the prompt."
  (org-date-calendar-step 1 0))

(define-command org-calendar-backward-day ()
  "In Org's date prompt: the calendar's day a day back, written at the prompt."
  (org-date-calendar-step -1 0))

(define-command org-calendar-forward-week ()
  "In Org's date prompt: the calendar's day a week on, written at the prompt."
  (org-date-calendar-step 7 0))

(define-command org-calendar-backward-week ()
  "In Org's date prompt: the calendar's day a week back, written at the prompt."
  (org-date-calendar-step -7 0))

(define-command org-calendar-forward-month ()
  "In Org's date prompt: the calendar's day a month on, written at the prompt."
  (org-date-calendar-step 0 1))

(define-command org-calendar-backward-month ()
  "In Org's date prompt: the calendar's day a month back, written at the prompt."
  (org-date-calendar-step 0 -1))

(define-command org-calendar-forward-year ()
  "In Org's date prompt: the calendar's day a year on, written at the prompt."
  (org-date-calendar-step 0 12))

(define-command org-calendar-backward-year ()
  "In Org's date prompt: the calendar's day a year back, written at the prompt."
  (org-date-calendar-step 0 -12))

(define-command org-calendar-goto-today ()
  "In Org's date prompt: the calendar on today, written at the prompt."
  (when (and *org-date-calendar* (live-buffer-p *org-date-calendar*))
    (org-date-calendar-step (- (org-today) (or (calendar-date-at *org-date-calendar*) (org-today))) 0)))

(defun org-planning-tokens (line)
  "The planning LINE's keywords and their timestamps, in order, as (KEYWORD
. TIMESTAMP), a range of two as one: <2026-10-05 Mon>--<2026-10-07 Wed>."
  (let ((tokens '()))
    (cl-ppcre:do-register-groups (keyword timestamp) (*org-planning-item-scanner* line)
      (push (cons keyword timestamp) tokens))
    (nreverse tokens)))

(defun org-inactive-text (timestamp)
  "TIMESTAMP's text with [ and ] about it, as a date is written in a log."
  (format nil "[~a]" (subseq timestamp 1 (1- (length timestamp)))))

(defun org-ts-keep-cycle (new old)
  "NEW, an ORG-TS, given the repeater and the warning of OLD (an ORG-TS or NIL)
where it has none of its own."
  (when old
    (unless (org-ts-repeater new)
      (setf (org-ts-repeater new) (org-ts-repeater old) (org-ts-repeat-value new) (org-ts-repeat-value old)
            (org-ts-repeat-unit new) (org-ts-repeat-unit old)))
    (unless (org-ts-warning new)
      (setf (org-ts-warning new) (org-ts-warning old) (org-ts-warning-value new) (org-ts-warning-value old)
            (org-ts-warning-unit new) (org-ts-warning-unit old))))
  new)

(defun org-no-date ()
  "Say that what was typed at a date prompt is no date, with the kinds it reads."
  (message "That is no date: fri, +3d, 10-15, sep 30 or 2026-12-24 14:00"))

(defun org-plan (keyword new)
  "Give the task point is under the planning KEYWORD (\"SCHEDULED\" or
\"DEADLINE\") with the ORG-TS NEW, or take it away when NEW is NIL, logging the
date it had as their settings say."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not under a headline")
          (let* ((begin (org-node-begin headline))
                 (line-end (string-line-end text begin))
                 (end (org-entry-end headline text))
                 (first-line (and (< line-end end) (org-line-at text (1+ line-end))))
                 (planning-p (and first-line (org-planning-line-p first-line)))
                 (tokens (and planning-p (org-planning-tokens first-line)))
                 (old (cdr (assoc keyword tokens :test #'string=)))
                 (new-text (and new (org-ts-text (org-ts-keep-cycle new (and old (org-read-ts old))))))
                 (tokens (cond ((and new old) (mapcar (lambda (tk) (if (string= (car tk) keyword) (cons keyword new-text) tk))
                                                      (remove "CLOSED" tokens :key #'car :test #'string=)))
                               (new (cons (cons keyword new-text) (remove "CLOSED" tokens :key #'car :test #'string=)))
                               (t (remove keyword tokens :key #'car :test #'string=))))
                 (planning (format nil "~{~a~^ ~}" (mapcar (lambda (tk) (format nil "~a: ~a" (car tk) (cdr tk))) tokens)))
                 (deadline-p (string= keyword "DEADLINE"))
                 (log (and old (if deadline-p *org-log-redeadline* *org-log-reschedule*)))
                 (note (and log (format nil "- ~a \"~a\" on ~a"
                                        (cond ((and deadline-p new) "New deadline from")
                                              (deadline-p "Removed deadline, was")
                                              (new "Rescheduled from")
                                              (t "Not scheduled, was"))
                                        (org-inactive-text old) (org-timestamp-text (org-now)))))
                 (entry (org-entry-with-log headline text (and note (list note)) *org-log-into-drawer*))
                 (entry-line-end (or (position #\Newline entry) (length entry)))
                 (body (if (< entry-line-end (length entry)) (subseq entry (1+ entry-line-end)) ""))
                 (lines (org-text-lines body))
                 (lines (append (and (plusp (length planning)) (list planning)) (if planning-p (rest lines) lines)))
                 (new-entry (org-entry-from-lines (subseq text begin line-end) lines (org-ends-with-newline-p entry))))
            (cond ((and (null new) (null old))
                   (message "~:[Not scheduled~;No deadline~]" deadline-p))
                  (t (org-replace-chars buffer begin end new-entry
                                        :point (cond ((<= position line-end) position)
                                                     ((>= position end) (+ position (- (length new-entry) (- end begin))))
                                                     (t begin)))
                     (message "~:[~a~;~*~a~]" new (if deadline-p "Deadline taken away" "Scheduling taken away")
                              (format nil "~a: ~a" keyword new-text)))))))))

(define-command org-schedule ()
  "Schedule the task point is under (\\`C-c C-s'): the
date is asked for (fri, +3d, 10-15, sep 30, 2026-12-24 14:00),
what it reads shown as it is typed, and SCHEDULED: goes on its planning line,
a repeater and warning it had kept. With \\[universal-argument], take it away."
  (if (prefix-argument)
      (org-plan "SCHEDULED" nil)
      (let ((ts (read-org-date "Schedule: ")))
        (if ts (org-plan "SCHEDULED" ts) (org-no-date)))))

(define-command org-deadline ()
  "Give the task point is under a deadline (\\`C-c C-d'):
the date asked for as \\[org-schedule] asks it, DEADLINE: on its planning
line, a repeater and warning it had kept. With \\[universal-argument], take it away."
  (if (prefix-argument)
      (org-plan "DEADLINE" nil)
      (let ((ts (read-org-date "Deadline: ")))
        (if ts (org-plan "DEADLINE" ts) (org-no-date)))))

(defun org-timestamp-around (text position)
  "Where the timestamp of TEXT that holds the character POSITION, or ends just
before it, begins and ends, on its line: in a heading, its text, or a
planning line (SCHEDULED: <...>); each date of a range alone. NIL when there
is none."
  (cl-ppcre:do-matches (start end *org-one-timestamp-scanner* text nil
                              :start (string-line-start text position) :end (string-line-end text position))
    (when (and (<= start position end) (org-read-ts (subseq text start end)))
      (return-from org-timestamp-around (values start end)))))

(defun org-insert-timestamp (active)
  "Ask for a date and put it in at point as a timestamp, ACTIVE <...> or
inactive [...]; on a timestamp or just after it (ORG-TIMESTAMP-AROUND), put it
in that one's place, its repeater and warning kept, and its time when none
was typed."
  (let ((ts (read-org-date (if active "Date: " "Inactive date: "))))
    (if (null ts)
        (org-no-date)
        (with-org-structure (buffer tree text position)
          (multiple-value-bind (from to) (org-timestamp-around text position)
            (let* ((old (and from (org-read-ts (subseq text from to))))
                   (new (progn (setf (org-ts-active ts) active)
                               (when (and old (org-ts-hour old) (not (org-ts-hour ts)))
                                 (setf (org-ts-hour ts) (org-ts-hour old) (org-ts-minute ts) (org-ts-minute old)
                                       (org-ts-end-hour ts) (org-ts-end-hour old) (org-ts-end-minute ts) (org-ts-end-minute old)))
                               (org-ts-text (org-ts-keep-cycle ts old))))
                   (from (or from position)))
              (org-replace-chars buffer from (or to position) new :point (+ from (length new)))))))))

(define-command org-time-stamp ()
  "Put in a timestamp at point (\\`C-c .'), the date asked for as
\\[org-schedule] asks it: <2026-10-13 Tue>, or with a time <2026-10-13 Tue
14:00>. An active timestamp shows its entry in the agenda on that day. On a
timestamp or just after it, change its date, its repeater and warning kept."
  (org-insert-timestamp t))

(define-command org-time-stamp-inactive ()
  "Put in an inactive timestamp at point (\\`C-c !'): as \\[org-time-stamp],
written [2026-10-13 Tue], which the agenda leaves out. On a timestamp, put
an inactive one in its place."
  (org-insert-timestamp nil))

(defun install-org-schedule ()
  "Bind \\`C-c C-s', \\`C-c C-d', \\`C-c .' and \\`C-c !' in Org buffers, and the keys
that move the calendar the date prompt shows."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-s" 'org-schedule)
    (define-key keymap "C-c C-d" 'org-deadline)
    (define-key keymap "C-c ." 'org-time-stamp)
    (define-key keymap "C-c !" 'org-time-stamp-inactive))
  (let ((minibuffer (editor-minibuffer-keymap *editor*)))
    (define-keys minibuffer '(("S-<right>" org-calendar-forward-day) ("S-<left>" org-calendar-backward-day)
                              ("S-<down>" org-calendar-forward-week) ("S-<up>" org-calendar-backward-week)
                              ("M-S-<right>" org-calendar-forward-month) ("M-S-<left>" org-calendar-backward-month)
                              ("M-S-<down>" org-calendar-forward-year) ("M-S-<up>" org-calendar-backward-year)
                              ("C-." org-calendar-goto-today)))))
