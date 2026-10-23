;;;; lisp/calendar/calendar.lisp --- The calendar: three months at a time, moved by day, week, month and year.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's M-x calendar: *Calendar* shows the month point is in with the
;;;; one before and the one after, laid out as Emacs lays them out, and point
;;;; on a day moves by days, weeks, months and years, the months scrolling as
;;;; it goes. Today is marked, and the days other parts of the editor name
;;;; (*CALENDAR-MARK-FUNCTIONS*: the days with agenda entries) are marked as
;;;; they are worked out in the background. Days are numbered as Org's agenda
;;;; numbers them, from 1900-01-01, any year at all.

(in-package #:tuatara.defaults)

(defvar *calendar-week-start-day* 1
  "The day a week begins on in the calendar: 0 Sunday, 1 Monday (as
standard), up to 6 Saturday.")

(defvar *calendar-months* 3
  "How many months the calendar shows: three as standard, the
month point is in among them, as many before it as after (one more after for
an even number); twelve for a year; NIL for as many as fit side by side in its
window (*CALENDAR-MONTHS-PER-ROW*, or the window's width).")

(defvar *calendar-months-per-row* nil
  "How many months the calendar shows side by side, the rest of
*CALENDAR-MONTHS* in rows of them below, a blank line between: NIL, as
standard, for as many as its window's width holds (three in 80 columns, four
in 105), a number for that many.")

(defvar *calendar-mark-functions* '()
  "What marks days in the calendar: functions called on the editor's thread
with the first and last day numbers shown, each returning a function of no
arguments that a thread of its own calls, returning the days to mark: day
numbers (the agenda's: the days with entries), shown in :calendar-marked, or
\(DAY . FACE) for a face of its own (the journal's days, :calendar-journal).")

(defvar *calendar-mark-days* t
  "True for the calendar to mark the days *CALENDAR-MARK-FUNCTIONS* name (the
days with agenda entries) as soon as it shows them; NIL to wait for \\<calendar-mode>\\[calendar-mark-days].")

(defparameter *calendar-weekend-days* '(0 6)
  "The days of the weekend, by WEEKDAY's numbers: Sunday and Saturday, shown
as such in the calendar's headers and the agenda's dates.")

(defparameter *calendar-left-margin* 5 "Columns left blank before the first month.")
(defparameter *calendar-month-width* 25 "Columns from one month's start to the next's: seven days of three columns and four between.")

(defun calendar-date-words (number)
  "The day NUMBER as the calendar names it: Sat 2026-10-03, week 40."
  (format-day "%a %F, week %-V" number))

;;; The layout, as Emacs's calendar-generate lays it out

(defun calendar-month-count ()
  "How many months the calendar shows: *CALENDAR-MONTHS*, at least one; when
it is NIL, as many as fit side by side (CALENDAR-ROW-COUNT)."
  (cond ((and (integerp *calendar-months*) (plusp *calendar-months*)) *calendar-months*)
        ((null *calendar-months*) (calendar-row-count))
        (t 3)))

(defvar *calendar-width* nil
  "While the calendar is laid out: the columns of the window it is for, or NIL.")

(defun calendar-row-count ()
  "How many months the calendar puts side by side: *CALENDAR-MONTHS-PER-ROW*,
else as many as *CALENDAR-WIDTH* holds (three when it is not known), at least one."
  (cond ((and (integerp *calendar-months-per-row*) (plusp *calendar-months-per-row*)) *calendar-months-per-row*)
        (*calendar-width* (max 1 (floor (- *calendar-width* *calendar-left-margin* -4) *calendar-month-width*)))
        (t 3)))

(defun calendar-months-before ()
  "How many of the months shown come before the one point is in: half of the
others, the rest after it (one before and one after with three)."
  (floor (1- (calendar-month-count)) 2))

(defun calendar-layout (year month)
  "The calendar of the months around MONTH of YEAR (*CALENDAR-MONTHS* of them,
*CALENDAR-MONTHS-PER-ROW* side by side, a blank line between rows of them),
as three values: its text, lines joined
by line ends and none after the last;
the place of each day shown, a list of (DAY START END) of characters (END
after its last digit); and the spans of its titles and day names, (START END
FACE) of characters."
  (let ((lines (make-array 0 :adjustable t :fill-pointer t))
        (places '()) (spans '()) (band 0))
    (labels ((line (index)
               (loop while (<= (length lines) index)
                     do (vector-push-extend (make-array 0 :element-type 'character :adjustable t :fill-pointer t) lines))
               (aref lines index))
             (put (index column text &optional face)
               (let ((line (line index)))
                 (loop while (< (length line) column) do (vector-push-extend #\Space line))
                 (setf (fill-pointer line) (max (length line) column))
                 (loop for c across text for i from column
                       do (if (< i (length line)) (setf (aref line i) c) (vector-push-extend c line)))
                 (when face (push (list index column (+ column (length text)) face) spans))
                 column)))
      (multiple-value-bind (first-year first-month) (add-months year month (- (calendar-months-before)))
        (dotimes (i (calendar-month-count))
          (multiple-value-bind (y m) (add-months first-year first-month i)
            (let* ((per-row (calendar-row-count))
                   (indent (+ *calendar-left-margin* (* *calendar-month-width* (mod i per-row))))
                   ;; a row of months below the last: a blank line after the lowest line it took
                   (base (if (and (plusp i) (zerop (mod i per-row))) (setf band (1+ (length lines))) band))
                   (title (format-day "%B %-Y" (day-number y m 1)))
                   (left (floor (- 20 (length title)) 2))
                   (first (day-number y m 1))
                   (blank (- first (week-start first *calendar-week-start-day*))))
              (put base indent (make-string 20 :initial-element #\Space))
              (put base (+ indent left) title :calendar-month-header)
              (dotimes (k 7)
                (let ((j (mod (+ *calendar-week-start-day* k) 7)))
                  (put (+ base 1) (+ indent (* 3 k)) (subseq (aref *short-weekday-names* j) 0 2)
                       (if (member j *calendar-weekend-days*) :calendar-weekend-header :calendar-weekday-header))
                  (put (+ base 1) (+ indent (* 3 k) 2) " ")))
              (dotimes (d (month-length y m))
                (let* ((cell (+ d blank))
                       (row (+ base 2 (floor cell 7)))
                       (column (+ indent (* 3 (mod cell 7)))))
                  (when (zerop (mod cell 7)) (put row indent ""))
                  (put row column (format nil "~2d " (1+ d)))
                  (push (list row (+ first d) column (+ column 2)) places))))))))
    ;; characters from the lines' starts
    (let ((starts (make-array (length lines))) (at 0))
      (dotimes (i (length lines))
        (setf (aref starts i) at)
        (incf at (1+ (length (aref lines i)))))
      (values (format nil "~{~a~^~%~}" (coerce lines 'list))
              (sort (loop for (row day start end) in places
                          collect (list day (+ (aref starts row) start) (+ (aref starts row) end)))
                    #'< :key #'first)
              (loop for (row start end face) in spans
                    collect (list (+ (aref starts row) start) (+ (aref starts row) end) face))))))

;;; The buffer

(define-major-mode calendar-mode special-mode
  "The calendar (\\[calendar]): three months, point on a day.
\\<calendar-mode>\\[calendar-forward-day] and \\[calendar-backward-day] move a day,
\\[calendar-forward-week] and \\[calendar-backward-week] a week, \\[calendar-forward-month] and \\[calendar-backward-month]
a month, \\[calendar-forward-year] and \\[calendar-backward-year] a year; \\[calendar-beginning-of-week], \\[calendar-end-of-week],
\\[calendar-beginning-of-month], \\[calendar-end-of-month], \\[calendar-beginning-of-year] and \\[calendar-end-of-year] to the
week's, month's or year's ends; \\[calendar-scroll-left] and \\[calendar-scroll-right] scroll a month,
\\[calendar-scroll-left-three-months] and \\[calendar-scroll-right-three-months] three; \\[calendar-goto-today] today, \\[calendar-goto-date] a date asked
for, \\[calendar-other-month] a month asked for; \\[calendar-print-day-of-year] and \\[calendar-iso-print-date] say
which day of the year and which ISO week it is; \\[calendar-mark-days] and
\\[calendar-unmark] mark and unmark the days with agenda entries; \\[quit-window] leaves; \\[revert-buffer] draws it
again."
  :lighter "Calendar" :interactive nil)

(defun calendar-buffer ()
  "The *Calendar* buffer, made in calendar mode when there is none."
  (find-or-make-buffer "*Calendar*" :mode 'calendar-mode
                                    :made (lambda (buffer)
                                            ;; a grid of days, no lines to number; in the modal editing, its own few keys
                                            (choose-display buffer 'line-numbers nil)
                                            (setf (tuatara.buffer:buffer-local buffer :tuatara-support-keymap)
                                                  *tuatara-calendar-keymap*))))

(defun calendar-days-of (buffer)
  "The places of the days BUFFER shows: a vector of (DAY START END), octets."
  (tuatara.buffer:buffer-local buffer 'calendar-days #()))

(defun calendar-render (buffer year month)
  "Show the months around MONTH of YEAR in BUFFER, a calendar, laid out for
the width of the window it is shown in (the selected one when none shows it)."
  (multiple-value-bind (text places spans count)
      (let ((*calendar-width* (let ((window (or (buffer-window buffer)
                                                (editor-window *editor*))))
                                (and window (tuatara.display:window-width window)))))
        (multiple-value-call #'values (calendar-layout year month) (calendar-month-count)))
    (let* ((octets (lambda (chars) (tuatara.text:string-octet-length text :end chars)))
           (days (map 'vector (lambda (place) (destructuring-bind (day start end) place
                                                (list day (funcall octets start) (funcall octets end))))
                      places)))
      (set-made-text buffer text :spans (loop for (start end face) in spans
                                              collect (list (funcall octets start) (funcall octets end) face))
                                 :refill 'calendar-refill)
      (setf (tuatara.buffer:buffer-local buffer 'calendar-shown) (list year month)
            (tuatara.buffer:buffer-local buffer 'calendar-count) count
            (tuatara.buffer:buffer-local buffer 'calendar-days) days)
      (calendar-start-marks buffer))))

(defun calendar-refill (buffer)
  "Draw the calendar BUFFER again, the same months for the window's width as
it is now, its days marked again, point on the same day (\\[revert-buffer])."
  (let ((day (calendar-date-at buffer)))
    (destructuring-bind (year month) (tuatara.buffer:buffer-local buffer 'calendar-shown)
      (calendar-render buffer year month))
    (when day (calendar-show-day buffer day))))

(defun calendar-shown-range (buffer)
  "The first and last day numbers BUFFER shows, as two values."
  (let ((days (calendar-days-of buffer)))
    (values (first (aref days 0)) (first (aref days (1- (length days)))))))

(defun calendar-visible-p (buffer day)
  "True when the calendar BUFFER shows the day DAY."
  (let ((days (calendar-days-of buffer)))
    (and (plusp (length days)) (<= (first (aref days 0)) day (first (aref days (1- (length days))))))))

(defun calendar-place (buffer day)
  "Where the calendar BUFFER shows the day DAY, as (DAY START END), or NIL."
  (find day (calendar-days-of buffer) :key #'first))

(defun calendar-date-at (buffer)
  "The day point is on in the calendar BUFFER, or the nearest before it; NIL before the first."
  (let ((point (tuatara.buffer:buffer-point buffer))
        (best nil))
    (loop for (day start end) across (calendar-days-of buffer)
          when (<= start point end) do (return-from calendar-date-at day)
          when (<= end point) do (setf best day))
    best))

(defun calendar-cursor-date ()
  "The day point is on in the current calendar, today when none."
  (or (calendar-date-at (current-buffer)) (org-today)))

(defun calendar-show-day (buffer day &key centre)
  "Put point on the day DAY in the calendar BUFFER, on its last digit, the
months shown moved so that DAY's is in the middle when it is not
shown, or with CENTRE. The day point leaves is kept for \\<*tuatara-calendar-keymap*>\\[calendar-jump-back]."
  (let ((from (and (plusp (length (calendar-days-of buffer))) (calendar-date-at buffer))))
    (when (and from (/= from day))
      (setf (tuatara.buffer:buffer-local buffer 'calendar-last-day) from)))
  (when (or centre (not (calendar-visible-p buffer day)))
    (multiple-value-bind (year month) (day-date day)
      (calendar-render buffer year month)))
  (let ((place (calendar-place buffer day)))
    (when place
      (setf (tuatara.buffer:buffer-point buffer) (1- (third place)))
      (dolist (window (buffer-windows buffer))
        (setf (tuatara.display:window-point window) (1- (third place)))))))

(define-command calendar ()
  "Show the calendar: the month of today and the months either side, below the window you are in, point on today. Its keys
are calendar mode's (\\[describe-bindings] there lists them): \\<calendar-mode>\\[calendar-forward-day] and \\[calendar-backward-day] a day,
\\[calendar-forward-week] and \\[calendar-backward-week] a week, \\[calendar-goto-today] today, \\[quit-window] leaves."
  (calendar-open (org-today)))

(define-command calendar-here ()
  "Show the calendar in the window you are in, in place of its buffer, point
on today; \\<calendar-mode>\\[quit-window] brings that buffer back. \\[calendar] shows it below instead."
  (let* ((buffer (calendar-buffer))
         (window (editor-window *editor*))
         (old (tuatara.display:window-buffer window))
         (today (org-today)))
    (calendar-show-day buffer today :centre t)
    (show-buffer buffer)
    (let ((shown (editor-window *editor*)))
      (when (and (eq shown window) (not (eq old buffer)) (null (tuatara.display:window-quit-restore window)))
        (setf (tuatara.display:window-quit-restore window) old)))
    (calendar-show-day buffer today)))

(defun calendar-open (day)
  "Show the calendar with point on the day DAY, its window selected."
  (let ((buffer (calendar-buffer)))
    (calendar-show-day buffer day :centre t)
    (pop-to-buffer buffer)
    (calendar-show-day buffer day)
    buffer))

(defun calendar-move (days)
  "Move point in the current calendar DAYS days on (back, when negative)."
  (calendar-show-day (current-buffer) (+ (calendar-cursor-date) days)))

(defun day-months-on (day months)
  "The day number MONTHS months on from the day number DAY (back, when
negative): the same day of the month, or the month's last when it has fewer."
  (multiple-value-bind (year month date) (day-date day)
    (multiple-value-bind (y m) (add-months year month months)
      (day-number y m (min date (month-length y m))))))

(defun calendar-move-months (months)
  "Move point in the current calendar MONTHS months on: to the same day of
the month, or the month's last when it has fewer."
  (calendar-show-day (current-buffer) (day-months-on (calendar-cursor-date) months)))

(define-command calendar-forward-day ()
  "Move point a day on in the calendar; with a count, that many."
  (calendar-move (prefix)))

(define-command calendar-backward-day ()
  "Move point a day back in the calendar; with a count, that many."
  (calendar-move (- (prefix))))

(define-command calendar-forward-week ()
  "Move point a week on in the calendar; with a count, that many."
  (calendar-move (* 7 (prefix))))

(define-command calendar-backward-week ()
  "Move point a week back in the calendar; with a count, that many."
  (calendar-move (* -7 (prefix))))

(define-command calendar-forward-month ()
  "Move point a month on in the calendar, to the same day of the month (or its last); with a count, that many."
  (calendar-move-months (prefix)))

(define-command calendar-backward-month ()
  "Move point a month back in the calendar, to the same day of the month (or its last); with a count, that many."
  (calendar-move-months (- (prefix))))

(define-command calendar-forward-year ()
  "Move point a year on in the calendar; with a count, that many."
  (calendar-move-months (* 12 (prefix))))

(define-command calendar-backward-year ()
  "Move point a year back in the calendar; with a count, that many."
  (calendar-move-months (* -12 (prefix))))

(define-command calendar-beginning-of-week ()
  "Move point to the start of the week it is in (*calendar-week-start-day*); on it, a week back."
  (let* ((day (calendar-cursor-date))
         (back (- day (week-start day *calendar-week-start-day*))))
    (calendar-show-day (current-buffer) (- day (if (zerop back) (* 7 (1- (prefix))) (+ back (* 7 (1- (prefix)))))))))

(define-command calendar-end-of-week ()
  "Move point to the end of the week it is in; on it, a week on."
  (let* ((day (calendar-cursor-date))
         (on (- (+ (week-start day *calendar-week-start-day*) 6) day)))
    (calendar-show-day (current-buffer) (+ day on (* 7 (1- (prefix)))))))

(define-command calendar-beginning-of-month ()
  "Move point to the first day of its month."
  (multiple-value-bind (year month) (day-date (calendar-cursor-date))
    (calendar-show-day (current-buffer) (day-number year month 1))))

(define-command calendar-end-of-month ()
  "Move point to the last day of its month."
  (multiple-value-bind (year month) (day-date (calendar-cursor-date))
    (calendar-show-day (current-buffer) (day-number year month (month-length year month)))))

(define-command calendar-beginning-of-year ()
  "Move point to the first day of its year."
  (calendar-show-day (current-buffer) (day-number (day-date (calendar-cursor-date)) 1 1)))

(define-command calendar-end-of-year ()
  "Move point to the last day of its year."
  (calendar-show-day (current-buffer) (day-number (day-date (calendar-cursor-date)) 12 31)))

(defun calendar-scroll (months)
  "Show the months MONTHS on from those shown (back, when negative).
Point stays on its day when it is still shown, else
goes to today when today is, else to the first day of the middle month."
  (let* ((buffer (current-buffer))
         (old (calendar-cursor-date))
         (today (org-today)))
    (destructuring-bind (year month) (tuatara.buffer:buffer-local buffer 'calendar-shown)
      (multiple-value-bind (y m) (add-months year month months)
        (calendar-render buffer y m)
        (calendar-show-day buffer (cond ((calendar-visible-p buffer old) old)
                                        ((calendar-visible-p buffer today) today)
                                        (t (day-number y m 1))))))))

(define-command calendar-scroll-left ()
  "Show the next month, the months moving left; with a count, that many."
  (calendar-scroll (prefix)))

(define-command calendar-scroll-right ()
  "Show the month before, the months moving right; with a count, that many."
  (calendar-scroll (- (prefix))))

(define-command calendar-scroll-left-three-months ()
  "Show the months after those shown, as many as are shown (*calendar-months*,
three as standard); with a count, that many times as many."
  (calendar-scroll (* (tuatara.buffer:buffer-local (current-buffer) 'calendar-count 3) (prefix))))

(define-command calendar-scroll-right-three-months ()
  "Show the months before those shown, as many as are shown (*calendar-months*,
three as standard); with a count, that many times as many."
  (calendar-scroll (* (- (tuatara.buffer:buffer-local (current-buffer) 'calendar-count 3)) (prefix))))

(define-command calendar-jump-back ()
  "Go back to the day point was on before it last moved; again, and back to
the day after once more."
  (let ((day (tuatara.buffer:buffer-local (current-buffer) 'calendar-last-day)))
    (if day
        (calendar-show-day (current-buffer) day)
        (message "Point has not moved in the calendar yet"))))

(define-command calendar-goto-today ()
  "Move point to today, the months around it shown."
  (calendar-show-day (current-buffer) (org-today) :centre t))

(defun calendar-read-day (text)
  "The day number TEXT names: 2026-10-03, 2026-10 (its first day), or what
a date asked for reads (READ-DATE-TEXT: fri, +3d, oct 3); NIL for none."
  (let ((text (string-trim " " text)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(\\d{1,4})-(\\d{1,2})(?:-(\\d{1,2}))?$" text)
      (if match
          (let ((y (parse-integer (aref groups 0))) (m (parse-integer (aref groups 1)))
                (d (if (aref groups 2) (parse-integer (aref groups 2)) 1)))
            (and (valid-date-p y m d) (day-number y m d)))
          (values (ignore-errors (read-date-text text (org-today))))))))

(define-command calendar-goto-date ()
  "Move point to a date asked for: 2026-12-24, 2026-12, or as any date asked
for is read (tomorrow, fri, +3d, dec 24)."
  (let ((day (calendar-read-day (read-string "Go to date: "))))
    (if day (calendar-show-day (current-buffer) day :centre t) (message "That is no date"))))

(defun calendar-two-digit-year (digits)
  "The year the two DIGITS of a year stand for, as '27: of the hundred years
around this one, the one ending in them (2027 in 2026, 1999 for '99)."
  (let* ((this (nth-value 0 (day-date (org-today))))
         (year (+ (* 100 (floor this 100)) (parse-integer digits))))
    (cond ((> (- year this) 50) (- year 100))
          ((> (- this year) 50) (+ year 100))
          (t year))))

(defun calendar-read-month (text)
  "The first day of the month TEXT names, as a day number: jan '27, jan 27,
january 2027 (a month's name, three letters at least, then its year: two
digits, with or without ', for the year of this century nearest to this one,
four as it is); jan alone for this year's; 2027-03; or a date as
CALENDAR-READ-DAY reads it, its month. NIL for none."
  (let ((text (string-downcase (string-trim " " text))))
    (or (cl-ppcre:register-groups-bind (name short long) ("^([a-z]+)\\.?(?:\\s+(?:'?(\\d{1,2})|(\\d{3,4})))?$" text)
          (let ((m (and (>= (length name) 3)
                        (position-if (lambda (full) (uiop:string-prefix-p name (string-downcase full))) *month-names*))))
            (and m (day-number (cond (long (parse-integer long))
                                              (short (calendar-two-digit-year short))
                                              (t (nth-value 0 (day-date (org-today)))))
                                        (1+ m) 1))))
        (let ((day (calendar-read-day text)))
          (and day (multiple-value-bind (year month) (day-date day)
                     (day-number year month 1)))))))

(define-command calendar-other-month ()
  "Show the months around a month asked for, point on its first day: jan '27
\(or jan 27, january 2027) for January 2027, jan for this year's, 2027-03,
or a date (2027-03-14, fri, +3m) for its month."
  (let ((day (calendar-read-month (read-string "Month (jan '27, 2027-03): "))))
    (if day (calendar-show-day (current-buffer) day :centre t) (message "That is no month: jan '27, january 2027 or 2027-03"))))

(define-command calendar-print-day-of-year ()
  "Say which day of its year the day at point is."
  (let* ((day (calendar-cursor-date))
         (year (day-date day))
         (of (year-day day))
         (left (- (day-number year 12 31) day)))
    (message "Day ~d of ~d; ~d day~:p remaining in the year" of year left)))

(define-command calendar-iso-print-date ()
  "Say the ISO date of the day at point: its day of the week, its week and its
year."
  (multiple-value-bind (year week weekday) (iso-week (calendar-cursor-date))
    (message "ISO date: Day ~d of week ~d of ~d" weekday week year)))

;;; A date asked for

(defun date-words (day hour minute end-hour end-minute)
  "The day number DAY written as a date asked for is shown: 2026-12-24 Thu,
with HOUR and MINUTE when given, 14:00, and END-HOUR and END-MINUTE for a
range, 14:00-15:30."
  (format nil "~a~@[-~a~]" (format-day (if hour "%F %a %R" "%F %a") day (or hour 0) (or minute 0))
          (and hour end-hour (format-day "%R" 0 end-hour end-minute))))

(defun read-date (prompt &key (now (get-universal-time)) (shown #'date-words) follow none also keymap)
  "Ask PROMPT for a date, read as READ-DATE-TEXT reads it (tomorrow, fri, +3d,
sep 30, 2026-12-24 14:00, 3pm), the date it reads shown below as it is
typed, as SHOWN, a function of its day number, hour, minute, end hour and
end minute, writes it (2026-12-24 Thu 14:00 unless given), \\`RET' taking it;
FOLLOW, when given, a function of the same, is called as each is read. NOW
is the universal time taken as now. ALSO, when given, reads what is typed
when it is no date: a function of it giving what it reads and the words
shown for it, or NIL. KEYMAP, when given, holds keys of the question's own.
Returns the universal time of the date in local time,
at midnight when no time is given, and as the values after it its day
number, hour, minute, end hour and end minute; when what was typed is no
date, what NONE, a function of it or of what ALSO read, gives (NIL unless
given)."
  (let* ((today (nth-value 0 (time-day now)))
         (chosen (completing-read prompt
                                  (lambda (input)
                                    (let ((parts (multiple-value-list (read-date-text input today))))
                                      (cond ((first parts)
                                             (when follow (apply follow parts))
                                             (list (make-candidate (apply shown parts) :value parts :annotation "RET takes it")))
                                            (also
                                             (multiple-value-bind (value words) (funcall also input)
                                               (and value (list (make-candidate words :value (list :also value)
                                                                                      :annotation "RET takes it"))))))))
                                  :filter nil :keymap keymap))
         (parts (cond ((and (consp chosen) (eq (first chosen) :also)) (list nil))
                      ((consp chosen) chosen)
                      (t (multiple-value-list (read-date-text chosen today))))))
    (if (first parts)
        (destructuring-bind (day hour minute end-hour end-minute) parts
          (values (day-universal-time day (or hour 0) (or minute 0)) day hour minute end-hour end-minute))
        (and none (funcall none (if (and (consp chosen) (eq (first chosen) :also)) (second chosen) chosen))))))

;;; Marks

(defun calendar-marking-p (buffer)
  "True when the calendar BUFFER marks its days: as \\<calendar-mode>\\[calendar-mark-days] or \\[calendar-unmark]
last said there, else as *CALENDAR-MARK-DAYS* says."
  (let ((said (tuatara.buffer:buffer-local buffer 'calendar-marking :unsaid)))
    (if (eq said :unsaid) *calendar-mark-days* said)))

(defstruct (calendar-marks (:include tuatara.runtime:job) (:constructor make-calendar-marks (buffer works))
                           (:copier nil))
  "The days a calendar BUFFER is to mark, worked out in the background, a job
\(TUATARA.RUNTIME:JOB): WORKS the functions that give them
\(*CALENDAR-MARK-FUNCTIONS*); MARKS, once done, the days they gave."
  works marks)

(setf (documentation 'calendar-marks-p 'function) "True when OBJECT is a CALENDAR-MARKS.")

(tuatara.runtime:define-job-words calendar-marks
  :doing "Marking the days"
  :verb "mark the days"
  :left-words "the days are not marked"
  :at-exit :quiet)
(defmethod tuatara.runtime:job-holds-queue-p ((job calendar-marks))
  "False: the calendar's marks, worked out in the background, keep no other job of its buffer waiting."
  nil)

(defun calendar-start-marks (buffer)
  "Work out in the background, as a job of BUFFER, a calendar (CALENDAR-MARKS),
the days it is to mark (*CALENDAR-MARK-FUNCTIONS*), unless marks are off
there; they are put in as the job is taken in. Those being worked out for it
before are given up."
  (when (and *calendar-mark-functions* (calendar-marking-p buffer)
             (plusp (length (calendar-days-of buffer))))
    (multiple-value-bind (first last) (calendar-shown-range buffer)
      (let* ((works (loop for f in *calendar-mark-functions*
                          for work = (ignore-errors (funcall f first last))
                          when work collect work))
             (job (make-calendar-marks buffer works))
             (before (tuatara.buffer:buffer-local buffer 'calendar-marks-job)))
        (when before (tuatara.runtime:job-stop before))
        (setf (tuatara.buffer:buffer-local buffer 'calendar-marks-job) job)
        (tuatara.runtime:start-job-thread
         job (lambda (job)
               (setf (calendar-marks-marks job)
                     (loop for work in (calendar-marks-works job)
                           do (tuatara.runtime:job-check-stop)
                           append (tuatara.runtime:ignoring-errors-reported ("marking the calendar's days")
                                    (funcall work)))))
         :name "tuatara calendar marks")))))

(defmethod tuatara.runtime:job-take-in ((job calendar-marks))
  "Put in the marked days of a calendar worked out in the background, unless
others are worked out for it since, or marks were taken off meanwhile."
  (let ((buffer (calendar-marks-buffer job)))
    (when (and (live-buffer-p buffer)
               (eq (tuatara.buffer:buffer-local buffer 'calendar-marks-job) job))
      (setf (tuatara.buffer:buffer-local buffer 'calendar-marks-job) nil)
      (cond ((not (eq (calendar-marks-status job) :done))
             (message "~a" (tuatara.runtime:job-ending-words job)))
            ;; marks taken off meanwhile (calendar-unmark) stay off
            ((calendar-marking-p buffer)
             (setf (tuatara.buffer:buffer-local buffer 'calendar-marked)
                   (let ((table (make-hash-table)))
                     (dolist (mark (calendar-marks-marks job) table)
                       (multiple-value-bind (day face) (if (consp mark) (values (car mark) (cdr mark)) (values mark :calendar-marked))
                         (pushnew face (gethash day table)))))))))))

(define-command calendar-mark-days ()
  "Mark the days with agenda entries in the calendar (as standard, with *calendar-mark-days*)."
  (setf (tuatara.buffer:buffer-local (current-buffer) 'calendar-marking) t)
  (calendar-start-marks (current-buffer))
  (message "Marking the days with entries"))

(define-command calendar-unmark ()
  "Take the marks off the calendar's days, until \\<calendar-mode>\\[calendar-mark-days] puts them back."
  (setf (tuatara.buffer:buffer-local (current-buffer) 'calendar-marking) nil
        (tuatara.buffer:buffer-local (current-buffer) 'calendar-marked) nil)
  (message "Marks taken off"))

(defun calendar-day-faces (faces)
  "The FACES a day is marked with (newest first, as the marks keep them), in
the order they are laid on it, each over the one before: as the marks came,
but the agenda's (:calendar-marked) last, so that a day with agenda entries
shows their colour whatever else marks it (a journal's day then shows its
bold and underline in that colour)."
  (stable-sort (reverse faces) #'< :key (lambda (face) (if (eq face :calendar-marked) 1 0))))

(defmethod mode-syntax-highlights ((mode calendar-mode) buffer rows)
  "The colours of the calendar in BUFFER's ROWS shown: those it was written with, the days marked and today."
  (when (plusp (length rows))
    (let* ((from (tuatara.display:row-start (aref rows 0)))
           (to (tuatara.display:row-shown-end (aref rows (1- (length rows)))))
           (today (org-today))
           (marked (tuatara.buffer:buffer-local buffer 'calendar-marked)))
      (append (made-spans-between buffer from to)
              (loop for (day start end) across (calendar-days-of buffer)
                    when (and (< start to) (> end from) marked)
                      append (loop for face in (calendar-day-faces (gethash day marked)) collect (list start end face)))
              (loop for (day start end) across (calendar-days-of buffer)
                    when (and (< start to) (> end from) (= day today))
                      collect (list start end :calendar-today))))))

(defun calendar-cursor-face (editor buffer)
  "Make :calendar-selected look as the cursor does in BUFFER, a calendar: its
ground the cursor's colour (the text's, when the cursor has none of its own),
its digits the text's ground, as a block cursor shows its character, both
given outright so that a marked day's colour does not show through. Changed
only when those colours have. In a theme without colours (plain), or one that
leaves them to the terminal, it is reverse video, bold and underlined instead."
  (when (or (let ((theme (tuatara.display:find-theme tuatara.display:*current-theme*)))
              (and theme (tuatara.display:theme-colourless theme)))
            (null (tuatara.display:face-attribute :default :bg))
            (null (or (let ((*current-buffer* buffer)) (nth-value 1 (cursor-look editor)))
                      (tuatara.display:face-attribute :default :fg))))
    (unless (and (tuatara.display:face-attribute :calendar-selected :reverse)
                 (null (tuatara.display:face-attribute :calendar-selected :bg)))
      (tuatara.display:set-face-attributes :calendar-selected :fg nil :bg nil :reverse t :bold t :underline t))
    (return-from calendar-cursor-face nil))
  (let* ((cursor (let ((*current-buffer* buffer)) (nth-value 1 (cursor-look editor))))
         (colour (or cursor (tuatara.display:face-attribute :default :fg)))
         (ground (tuatara.display:face-attribute :default :bg)))
    (unless (and (equal (tuatara.display:face-attribute :calendar-selected :bg) colour)
                 (equal (tuatara.display:face-attribute :calendar-selected :fg) ground)
                 (not (tuatara.display:face-attribute :calendar-selected :reverse)))
      (tuatara.display:set-face-attributes :calendar-selected :bg colour :fg ground :reverse nil :bold t :underline nil))))

(defun calendar-point-highlights (editor window rows)
  "Highlight function: in a window showing the calendar, both digits of the day
at the window's point on the cursor's colour (:calendar-selected, as
CALENDAR-CURSOR-FACE sets it)."
  (declare (ignore rows))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (typep (buffer-major-mode buffer) 'calendar-mode)
      (let ((point (tuatara.display:window-point window)))
        (loop for (nil start end) across (calendar-days-of buffer)
              when (<= start point end)
                return (progn (calendar-cursor-face editor buffer)
                              (list (list start end :calendar-selected))))))))

(defun calendar-mode-line (editor buffer)
  "Mode line function: in a calendar, the day point is on, its weekday and ISO week."
  (declare (ignore editor))
  (when (typep (buffer-major-mode buffer) 'calendar-mode)
    (let ((day (calendar-date-at buffer)))
      (and day (calendar-date-words day)))))

(defun install-calendar ()
  "Bind the calendar mode keys (the modal editing's are in
*TUATARA-CALENDAR-KEYMAP*); show it below the window it is asked from, as tall
as it is, the window kept for it and selected; mark its days as they are
worked out, today on a bright ground and the day at point on the cursor's
colour."
  (let ((keymap (mode-keymap 'calendar-mode)))
    (define-keys keymap '(("C-f" calendar-forward-day) ("<right>" calendar-forward-day)
                          ("C-b" calendar-backward-day) ("<left>" calendar-backward-day)
                          ("C-n" calendar-forward-week) ("<down>" calendar-forward-week)
                          ("C-p" calendar-backward-week) ("<up>" calendar-backward-week)
                          ("M-}" calendar-forward-month) ("M-{" calendar-backward-month)
                          ("C-x ]" calendar-forward-year) ("C-x [" calendar-backward-year)
                          ("C-a" calendar-beginning-of-week) ("C-e" calendar-end-of-week)
                          ("M-a" calendar-beginning-of-month) ("M-e" calendar-end-of-month)
                          ("M-<" calendar-beginning-of-year) ("M->" calendar-end-of-year)
                          (">" calendar-scroll-left) ("C-x >" calendar-scroll-left)
                          ("<" calendar-scroll-right) ("C-x <" calendar-scroll-right)
                          ("C-v" calendar-scroll-left-three-months) ("<next>" calendar-scroll-left-three-months)
                          ("M-v" calendar-scroll-right-three-months) ("<prior>" calendar-scroll-right-three-months)
                          ("." calendar-goto-today) ("g d" calendar-goto-date) ("o" calendar-other-month)
                          ("p d" calendar-print-day-of-year) ("p c" calendar-iso-print-date)
                          ("m" calendar-mark-days) ("u" calendar-unmark))))
  (commands-for-mode 'calendar-mode "the calendar (\\[calendar])"
                     'calendar-forward-day 'calendar-backward-day 'calendar-forward-week 'calendar-backward-week
                     'calendar-forward-month 'calendar-backward-month 'calendar-forward-year 'calendar-backward-year
                     'calendar-beginning-of-week 'calendar-end-of-week 'calendar-beginning-of-month 'calendar-end-of-month
                     'calendar-beginning-of-year 'calendar-end-of-year 'calendar-scroll-left 'calendar-scroll-right
                     'calendar-scroll-left-three-months 'calendar-scroll-right-three-months 'calendar-goto-today 'calendar-jump-back
                     'calendar-goto-date 'calendar-other-month 'calendar-print-day-of-year 'calendar-iso-print-date
                     'calendar-mark-days 'calendar-unmark)
  (place-buffers 'calendar-mode :below :size :fit :keep t :select t)
  (pushnew 'calendar-mode-line *mode-line-functions*)
  (pushnew 'calendar-point-highlights *highlight-functions*))
