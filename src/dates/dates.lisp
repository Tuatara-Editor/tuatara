;;;; src/dates/dates.lisp --- Dates and times: day numbers, weekdays, ISO weeks, local time, names, and times written in a pattern.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A day is a number: 0 for 1900-01-01, one more for each day after, below 0
;;;; for the days before, any year of the Gregorian calendar (the civil-days
;;;; algorithm of Howard Hinnant). Weekdays count from Sunday, 0, to Saturday,
;;;; 6. Local time reaches back before 1900: times before 1901 are counted in
;;;; the offset from universal time the time zone has at the start of 1901.
;;;; FORMAT-TIME and FORMAT-DAY write a time in a pattern of strftime's %
;;;; specifiers, padded as Emacs's format-time-string pads them.

(in-package #:tuatara.dates)

;;; Days as numbers

(defun civil-days (year month day)
  "Days from 1970-01-01 to DAY of MONTH (1 to 12) of YEAR, any year; DAY may
be past the month's end or below 1, counted on from its first. O(1)."
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defconstant +days-1900-to-1970+ 25567
  "Days from 1900-01-01, day 0, to 1970-01-01.")

(defun day-number (year month day)
  "The day DAY of MONTH of YEAR as a day number: 0 for 1900-01-01, one more
for each day after, below 0 before it, any year. A MONTH past 12 or below 1
counts on into the years after or before, and a DAY past its month's end or
below 1 into the months after or before: 2026-02-31 is 2026-03-03. O(1)."
  (multiple-value-bind (years month0) (floor (1- month) 12)
    (+ (civil-days (+ year years) (1+ month0) day) +days-1900-to-1970+)))

(defun day-date (number)
  "The day NUMBER (as DAY-NUMBER gives it) as three values: its year, its
month (1 to 12) and its day of the month. O(1)."
  (let* ((z (+ number (- +days-1900-to-1970+) 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (floor yoe 4)) (floor yoe 100))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
         (month (if (< mp 10) (+ mp 3) (- mp 9)))
         (year (+ yoe (* era 400) (if (<= month 2) 1 0))))
    (values year month day)))

(defun weekday (number)
  "The day of the week of the day NUMBER: 0 for Sunday, 1 Monday, up to 6 for
Saturday. O(1)."
  (mod (1+ number) 7))

(defun week-start (number first-weekday)
  "The day number of the first day of the week holding the day NUMBER, weeks
beginning on FIRST-WEEKDAY: 0 for Sunday (7 too), 1 Monday, up to 6 for
Saturday. O(1)."
  (- number (mod (- (weekday number) first-weekday) 7)))

(defun iso-week (number)
  "The ISO week of the day NUMBER as three values: its year (the year of that
week's Thursday, which differs from the day's own around New Year), the week
\(1 to 53, week 1 holding the year's first Thursday) and the day of the week,
1 for Monday to 7 for Sunday. O(1)."
  (let* ((weekday (let ((w (weekday number))) (if (zerop w) 7 w)))
         (thursday (+ number (- 4 weekday)))
         (year (nth-value 0 (day-date thursday)))
         (week (1+ (floor (- thursday (day-number year 1 1)) 7))))
    (values year week weekday)))

(defun iso-week-monday (year week)
  "The day number of the Monday that begins ISO WEEK of YEAR. O(1)."
  (let ((january-4 (day-number year 1 4)))
    (+ (- january-4 (1- (nth-value 2 (iso-week january-4)))) (* 7 (1- week)))))

(defun year-day (number)
  "Which day of its year the day NUMBER is: 1 for 1 January. O(1)."
  (1+ (- number (day-number (nth-value 0 (day-date number)) 1 1))))

(defun leap-year-p (year)
  "True when YEAR has a 29 February: every fourth year, except the hundredth
years that are not a four hundredth (1900 is not, 2000 is). O(1)."
  (and (zerop (mod year 4)) (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun month-length (year month)
  "How many days MONTH (1 to 12) of YEAR has. O(1)."
  (case month
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    (t 31)))

(defun valid-date-p (year month day)
  "True when YEAR, MONTH and DAY are whole numbers naming a day that is: MONTH
1 to 12, DAY 1 to the month's length. O(1)."
  (and (integerp year) (integerp month) (integerp day)
       (<= 1 month 12) (<= 1 day (month-length year month))))

(defun add-months (year month count)
  "The month COUNT months after MONTH of YEAR (before, for a negative COUNT),
as two values: its year and its month. O(1)."
  (let ((index (+ (* year 12) (1- month) count)))
    (values (floor index 12) (1+ (mod index 12)))))

;;; Local time

(defconstant +local-days-from+ 365
  "The day number of 1901-01-01: local times from it on are the time zone's
own, those before it counted back from it in the offset it has.")

(defconstant +local-time-from+ (* 366 86400)
  "The universal time of 1901-01-02 00:00 in universal time: a time from it on
is read in local time as the time zone has it, since it is 1901 by then in
every time zone; a time before it as DAY-UNIVERSAL-TIME counts it.")

(defun day-universal-time (day &optional (hour 0) (minute 0) (second 0))
  "The universal time of HOUR:MINUTE:SECOND in local time on the day number
DAY, any year: times before 1901 are counted back from the start of 1901 in
the offset from universal time it has, below 0 before 1900. An HOUR, MINUTE
or SECOND out of its range counts on into the days after or before."
  (multiple-value-bind (more seconds) (floor (+ (* 3600 hour) (* 60 minute) second) 86400)
    (let ((day (+ day more)))
      (if (>= day +local-days-from+)
          (multiple-value-bind (year month date) (day-date day)
            (multiple-value-bind (hour rest) (floor seconds 3600)
              (multiple-value-bind (minute second) (floor rest 60)
                (encode-universal-time second minute hour date month year))))
          (+ (day-universal-time +local-days-from+) (* 86400 (- day +local-days-from+)) seconds)))))

(defun time-day (time)
  "The universal TIME in local time as four values: its day number, hour,
minute and second; a time before 1901 as DAY-UNIVERSAL-TIME counts it."
  (if (>= time +local-time-from+)
      (multiple-value-bind (second minute hour date month year) (decode-universal-time time)
        (values (day-number year month date) hour minute second))
      (multiple-value-bind (days rest) (floor (- time (day-universal-time +local-days-from+)) 86400)
        (multiple-value-bind (hour rest) (floor rest 3600)
          (multiple-value-bind (minute second) (floor rest 60)
            (values (+ +local-days-from+ days) hour minute second))))))

(defun local-offset (time)
  "How many seconds local time is ahead of universal time at the universal
TIME (behind it, below 0), summer time counted; before 1901, the offset at
the start of 1901."
  (multiple-value-bind (second minute hour date month year weekday summer zone)
      (decode-universal-time (max time +local-time-from+))
    (declare (ignore second minute hour date month year weekday))
    (round (* 3600 (- (if summer 1 0) zone)))))

(defconstant +unix-epoch+ 2208988800
  "The universal time of the start of 1970, from which the system counts the times of files.")

(defun mtime-universal-time (mtime)
  "The universal time of MTIME, nanoseconds from the start of 1970 as the
system gives the times files were changed, to the second."
  (+ +unix-epoch+ (floor mtime 1000000000)))

;;; Names

(defparameter *weekday-names* #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "The days of the week in full, Sunday first, by WEEKDAY's numbers.")

(defparameter *short-weekday-names* #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "The days of the week in three letters, as Org's timestamps write them, Sunday first, by WEEKDAY's numbers.")

(defparameter *month-names* #("January" "February" "March" "April" "May" "June" "July" "August"
                               "September" "October" "November" "December")
  "The months in full, January first: month M is at M - 1.")

(defparameter *short-month-names* #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The months in three letters, January first: month M is at M - 1.")

;;; Times written in a pattern

(defun format-day (pattern day &optional (hour 0) (minute 0) (second 0) offset)
  "The day number DAY at HOUR:MINUTE:SECOND written as PATTERN says: its
characters as they are, except for these, which stand for parts of the date:
  %Y year (2026, four digits at least)   %y year in two digits   %G ISO week's year
  %m month (01-12)   %d day of the month (01-31)   %e day of the month, space-padded
  %j day of the year (001-366)   %V ISO week (01-53)   %u weekday, 1 Monday to 7 Sunday
  %w weekday, 0 Sunday to 6 Saturday   %A Monday   %a Mon   %B March   %b Mar
  %H hour (00-23)   %I hour (01-12)   %p AM or PM   %M minute   %S second
  %F the date as %Y-%m-%d   %T the time as %H:%M:%S   %R the time as %H:%M
  %z offset from universal time as +1300   %:z as +13:00   %% a %
A - after the % leaves out the padding (%-e, %-d: 5 for 05), a ^ makes the
part capitals (%^p, %^a). OFFSET is the seconds local time is ahead of
universal time for %z; NIL to work it out. O(length of PATTERN)."
  (multiple-value-bind (year month date) (day-date day)
    (with-output-to-string (out)
      (let ((i 0) (n (length pattern)))
        (loop while (< i n)
              do (let ((c (char pattern i)))
                   (if (or (char/= c #\%) (= (1+ i) n))
                       (progn (write-char c out) (incf i))
                       (let ((plain nil) (upper nil) (colon nil))
                         (incf i)
                         (loop while (and (< i n) (find (char pattern i) "-^:"))
                               do (case (char pattern i) (#\- (setf plain t)) (#\^ (setf upper t)) (t (setf colon t)))
                                  (incf i))
                         (when (< i n)
                           (let* ((spec (char pattern i))
                                  (two (lambda (v) (if plain (format nil "~d" v) (format nil "~2,'0d" v))))
                                  (four (lambda (v) (if (or plain (minusp v)) (format nil "~d" v) (format nil "~4,'0d" v))))
                                  (text (case spec
                                          (#\Y (funcall four year))
                                          (#\y (funcall two (mod year 100)))
                                          (#\G (funcall four (nth-value 0 (iso-week day))))
                                          (#\m (funcall two month))
                                          (#\d (funcall two date))
                                          (#\e (if plain (format nil "~d" date) (format nil "~2d" date)))
                                          (#\j (if plain (format nil "~d" (year-day day)) (format nil "~3,'0d" (year-day day))))
                                          (#\V (funcall two (nth-value 1 (iso-week day))))
                                          (#\u (format nil "~d" (nth-value 2 (iso-week day))))
                                          (#\w (format nil "~d" (weekday day)))
                                          (#\A (aref *weekday-names* (weekday day)))
                                          (#\a (aref *short-weekday-names* (weekday day)))
                                          (#\B (aref *month-names* (1- month)))
                                          (#\b (aref *short-month-names* (1- month)))
                                          (#\H (funcall two hour))
                                          (#\I (funcall two (let ((h (mod hour 12))) (if (zerop h) 12 h))))
                                          (#\p (if (< hour 12) "AM" "PM"))
                                          (#\M (funcall two minute))
                                          (#\S (funcall two second))
                                          (#\F (format nil "~a-~2,'0d-~2,'0d" (funcall four year) month date))
                                          (#\T (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))
                                          (#\R (format nil "~2,'0d:~2,'0d" hour minute))
                                          (#\z (let* ((east (or offset (local-offset (day-universal-time day hour minute second))))
                                                      (minutes (round (abs east) 60)))
                                                 (format nil "~:[+~;-~]~2,'0d~:[~;:~]~2,'0d"
                                                         (minusp east) (floor minutes 60) colon (mod minutes 60))))
                                          (#\% "%")
                                          (t (format nil "%~c" spec)))))
                             (write-string (if upper (string-upcase text) text) out)
                             (incf i)))))))))))

(defun format-time (pattern time)
  "The universal TIME in local time written as PATTERN says, its % specifiers
as FORMAT-DAY has them: (format-time \"[%F %a %R]\" time) writes
[2026-10-04 Sun 10:30]. Any year: times before 1901 as DAY-UNIVERSAL-TIME
counts them. O(length of PATTERN)."
  (multiple-value-bind (day hour minute second) (time-day time)
    (format-day pattern day hour minute second (and (search "z" pattern) (local-offset time)))))

(defun utc-time-words (time)
  "The universal TIME in universal time: 2026-10-06 20:15:00 UTC; NIL for
none, or one that cannot be written."
  (when time
    (ignore-errors (multiple-value-bind (s m h d mo y) (decode-universal-time time 0)
                     (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC" y mo d h m s)))))

(defun short-time-words (time)
  "The universal TIME in local time, short, as a list shows it: 2026-10-07
09:15; NIL for none."
  (and time (format-time "%F %R" time)))

(defun time-words (time)
  "The universal TIME in local time with its offset, then in universal time:
2026-10-07 Wed 09:15:00 +1300 (2026-10-06 20:15:00 UTC); NIL for none."
  (when time
    (format nil "~a~@[ (~a)~]" (format-time "%F %a %T %z" time) (utc-time-words time))))

;;; Dates as typed

(defun read-date-text (input today)
  "The day INPUT, a date as typed, names, TODAY being the day number of
today, as five values: its day number, the hour and minute when INPUT names
a time, and the hour and minute it ends at when it names a time range; NIL
when INPUT names no date. Read: nothing, . or today for today, tomorrow and
yesterday; fri, the Friday to come (a week on when today is one); +3d, -1w,
+1m, +1y from today, or from the date before it (2026-10-20 +3d); 5, the 5th
of this month or the next; 10-15 and sep 30 (or 30 sep), that day of this
year or the next; 2026-12-24; 2027, this day in that year; and a time,
14:00, 10:00-11:30, 3pm. Case does not count. A month or year on keeps the
day of the month, a day past the month's end counted on into the next: 31
January a month on is 3 March. A day or a time that does not exist (31 in a
month of 30 days, 13pm, 10:00-25:00) names no date. O(length of INPUT)."
  (multiple-value-bind (year month day) (day-date today)
    (let ((weekday (weekday today))
          (text (string-downcase (string-trim '(#\Space #\Tab) input)))
          (y year) (m month) (d day) hour minute end-hour end-minute (wrong nil))
      (labels ((take (pattern)
                 ;; the first match of PATTERN as its groups, taken out of TEXT
                 (multiple-value-bind (start end starts ends) (cl-ppcre:scan pattern text)
                   (when start
                     (prog1 (map 'list (lambda (s e) (and s (subseq text s e))) starts ends)
                       (setf text (concatenate 'string (subseq text 0 start) " " (subseq text end)))))))
               (date (new-y new-m new-d) (setf y new-y m new-m d new-d))
               (to-day (number) (multiple-value-bind (new-y new-m new-d) (day-date number) (date new-y new-m new-d)))
               (past-p (py pm pd) (< (+ (* 10000 py) (* 100 pm) pd) (+ (* 10000 year) (* 100 month) day))))
        ;; times
        (let ((range (take "(?<![\\d-])(\\d{1,2}):(\\d{2})(?:-(\\d{1,2}):(\\d{2}))?")))
          (when range
            (setf hour (parse-integer (first range)) minute (parse-integer (second range)))
            (when (third range)
              (setf end-hour (parse-integer (third range)) end-minute (parse-integer (fourth range))))))
        (let ((clock (take "\\b(\\d{1,2})(am|pm)\\b")))
          (when clock
            (let ((h (parse-integer (first clock))))
              (unless (<= 1 h 12) (setf wrong t))
              (setf hour (+ (mod h 12) (if (string= (second clock) "pm") 12 0))
                    minute 0))))
        ;; dates
        (let ((iso (take "\\b(\\d{4})-(\\d{1,2})-(\\d{1,2})\\b")))
          (when iso (date (parse-integer (first iso)) (parse-integer (second iso)) (parse-integer (third iso)))))
        (let ((month-day (take "\\b(\\d{1,2})-(\\d{1,2})\\b")))
          (when month-day
            (let ((md (parse-integer (first month-day))) (dd (parse-integer (second month-day))))
              (date (if (past-p year md dd) (1+ year) year) md dd))))
        (let ((named (or (take "\\b(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\\.? +(\\d{1,2})\\b(?: +(\\d{4})\\b)?")
                         (let ((other (take "\\b(\\d{1,2}) +(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\\b(?: +(\\d{4})\\b)?")))
                           (and other (list (second other) (first other) (third other)))))))
          (when named
            (let ((nm (1+ (position (first named) *short-month-names* :test #'string-equal))) (nd (parse-integer (second named))))
              (date (cond ((third named) (parse-integer (third named))) ((past-p year nm nd) (1+ year)) (t year)) nm nd))))
        (let ((word (take "\\b(today|tomorrow|yesterday)\\b")))
          (when word
            (to-day (+ (day-number y m d) (cond ((string= (first word) "tomorrow") 1) ((string= (first word) "yesterday") -1) (t 0))))))
        (let ((weekday-name (take "\\b(mon|tue|wed|thu|fri|sat|sun)[a-z]*\\b")))
          (when weekday-name
            (let ((ahead (mod (- (position (first weekday-name) *short-weekday-names* :test #'string-equal) weekday) 7)))
              (to-day (+ (day-number y m d) (if (zerop ahead) 7 ahead))))))
        (let ((relative (take "(?:^|\\s)([+-]{1,2})(\\d+)([dwmy]?)(?=\\s|$)")))
          (when relative
            (let ((value (* (parse-integer (second relative)) (if (char= (char (first relative) 0) #\-) -1 1)))
                  (unit (if (plusp (length (third relative))) (char (third relative) 0) #\d)))
              (to-day (case unit
                        (#\m (day-number y (+ m value) d))
                        (#\y (day-number (+ y value) m d))
                        (t (+ (day-number y m d) (* value (if (char= unit #\w) 7 1)))))))))
        (let ((year-only (take "\\b(\\d{4})\\b")))
          (when year-only
            (to-day (day-number (parse-integer (first year-only)) m d))))
        (let ((day-only (take "\\b(\\d{1,2})\\b")))
          (when day-only
            (let ((dd (parse-integer (first day-only))))
              (if (>= dd day)
                  (date year month dd)
                  (date (if (= month 12) (1+ year) year) (1+ (mod month 12)) dd)))))
        (take "^\\s*\\.\\s*$")
        (and (string= (string-trim '(#\Space #\Tab) text) "")
             (valid-date-p y m d)
             (not wrong)
             (or (null hour) (and (< hour 24) (< minute 60)))
             (or (null end-hour) (and (< end-hour 24) (< end-minute 60)))
             (values (day-number y m d) hour minute end-hour end-minute))))))

;;; Dates as files write them

(defun pdf-date-words (text)
  "TEXT, a date as a PDF writes it (D:20240301120000+13'00'), as words in its
own zone: 2024-03-01 12:00:00 +13:00; TEXT as it is when it is no such date.
Two more values: the universal time it names (its zone taken as UTC when it
gives none), NIL when it names none; and its zone in minutes east of UTC, NIL
when it gives none."
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings "^D?:?(\\d{4})(\\d{2})?(\\d{2})?(\\d{2})?(\\d{2})?(\\d{2})?([Z+-])?(\\d{2})?'?(\\d{2})?" (or text ""))
    (if (not match)
        text
        (let ((zone (and (aref parts 6) (* (if (string= (aref parts 6) "-") -1 1)
                                           (+ (* 60 (parse-integer (or (aref parts 7) "0"))) (parse-integer (or (aref parts 8) "0")))))))
          (flet ((part (k standard) (if (aref parts k) (parse-integer (aref parts k)) standard)))
            (values
             (format nil "~a-~a-~a ~a:~a:~a~@[ ~a~]" (aref parts 0) (or (aref parts 1) "01") (or (aref parts 2) "01")
                     (or (aref parts 3) "00") (or (aref parts 4) "00") (or (aref parts 5) "00")
                     (cond ((null zone) nil) ((string= (aref parts 6) "Z") "UTC")
                           (t (format nil "~a~a:~a" (aref parts 6) (or (aref parts 7) "00") (or (aref parts 8) "00")))))
             (let ((time (ignore-errors (encode-universal-time (part 5 0) (part 4 0) (part 3 0) (part 2 1) (part 1 1) (part 0 0) 0))))
               (and time (- time (* 60 (or zone 0)))))
             zone))))))

(defun iso-date-time (text)
  "TEXT, a date as XMP and ISO 8601 write it (2024-03-01T12:00:00+13:00), as
two values: the universal time it names (its zone taken as UTC when it gives
none), and its zone in minutes east of UTC, NIL when it gives none; NIL when
it names no date."
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings "^(\\d{4})-(\\d{2})(?:-(\\d{2}))?(?:T(\\d{2}):(\\d{2})(?::(\\d{2}))?(?:\\.\\d+)?)?(Z|[+-]\\d{2}:?\\d{2})?" (or text ""))
    (when match
      (flet ((part (k standard) (if (aref parts k) (parse-integer (aref parts k)) standard)))
        (let* ((zone-text (aref parts 6))
               (zone (cond ((null zone-text) nil) ((string= zone-text "Z") 0)
                           (t (let ((digits (remove #\: zone-text)))
                                (* (if (char= (char digits 0) #\-) -1 1)
                                   (+ (* 60 (parse-integer digits :start 1 :end 3)) (parse-integer digits :start 3 :end 5)))))))
               (time (ignore-errors (encode-universal-time (part 5 0) (part 4 0) (part 3 0) (part 2 1) (part 1 1) (part 0 0) 0))))
          (values (and time (- time (* 60 (or zone 0)))) zone))))))
