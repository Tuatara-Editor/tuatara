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

(defparameter *date-text-scanners*
  (let ((months (format nil "~{~a~^|~}" (map 'list #'string-downcase *short-month-names*)))
        (weekdays (format nil "~{~a~^|~}" (map 'list #'string-downcase *short-weekday-names*))))
    (flet ((scanner (pattern &rest arguments) (cl-ppcre:create-scanner (apply #'format nil pattern arguments))))
      (list :iso (scanner "\\b(\\d{4})-(\\d{1,2})-(\\d{1,2})\\b")
            :month-day (scanner "\\b(\\d{1,2})-(\\d{1,2})\\b")
            :month-name-day (scanner "\\b(~a)[a-z]*\\.? +(\\d{1,2})\\b(?: +(\\d{4})\\b)?" months)
            :day-month-name (scanner "\\b(\\d{1,2}) +(~a)[a-z]*\\b(?: +(\\d{4})\\b)?" months)
            :word (scanner "\\b(today|tomorrow|yesterday)\\b")
            :weekday (scanner "\\b(~a)[a-z]*\\b" weekdays)
            :relative (scanner "(?:^|\\s)([+-]{1,2})(\\d+)([dwmy]?)(?=\\s|$)")
            :year (scanner "\\b(\\d{4})\\b")
            :day (scanner "\\b(\\d{1,2})\\b")
            :today (scanner "^\\s*\\.\\s*$"))))
  "The patterns READ-DATE-TEXT reads a date as typed with, each made once, by
the part of a date it reads.")

(defun read-date-text (input today)
  "The day INPUT, a date as typed, names, TODAY being the day number of
today, as five values: its day number, the hour and minute when INPUT names
a time, and the hour and minute it ends at when it names a time range; NIL
when INPUT names no date. Read: nothing, . or today for today, tomorrow and
yesterday; fri, the Friday to come (a week on when today is one); +3d, -1w,
+1m, +1y from today, or from the date before it (2026-10-20 +3d); 5, the 5th
of this month or the next; 10-15 and sep 30 (or 30 sep), that day of this
year or the next; 2026-12-24; 2027, this day in that year; and a time or a
range of two as TIME-RANGE-AT reads them, 14:00, 10:00-11:30, 3pm, 3:30 pm.
Case does not count. A month or year on keeps the
day of the month, a day past the month's end counted on into the next: 31
January a month on is 3 March. A day or a time that does not exist (31 in a
month of 30 days, 13pm, 10:00-25:00) names no date. O(length of INPUT)."
  (multiple-value-bind (year month day) (day-date today)
    (let ((weekday (weekday today))
          (text (string-downcase (string-trim '(#\Space #\Tab) input)))
          (y year) (m month) (d day) hour minute end-hour end-minute)
      (labels ((take (part)
                 ;; the first match of PART's pattern as its groups, taken out of TEXT
                 (multiple-value-bind (start end starts ends) (cl-ppcre:scan (getf *date-text-scanners* part) text)
                   (when start
                     (prog1 (map 'list (lambda (s e) (and s (subseq text s e))) starts ends)
                       (setf text (concatenate 'string (subseq text 0 start) " " (subseq text end)))))))
               (date (new-y new-m new-d) (setf y new-y m new-m d new-d))
               (to-day (number) (multiple-value-bind (new-y new-m new-d) (day-date number) (date new-y new-m new-d)))
               (past-p (py pm pd) (< (+ (* 10000 py) (* 100 pm) pd) (+ (* 10000 year) (* 100 month) day))))
        ;; a time, or a range of two
        (multiple-value-bind (start end h mi eh em) (find-time-range text)
          (when start
            (setf hour h minute mi end-hour eh end-minute em
                  text (concatenate 'string (subseq text 0 start) " " (subseq text end)))))
        ;; dates
        (let ((iso (take :iso)))
          (when iso (date (parse-integer (first iso)) (parse-integer (second iso)) (parse-integer (third iso)))))
        (let ((month-day (take :month-day)))
          (when month-day
            (let ((md (parse-integer (first month-day))) (dd (parse-integer (second month-day))))
              (date (if (past-p year md dd) (1+ year) year) md dd))))
        (let ((named (or (take :month-name-day)
                         (let ((other (take :day-month-name)))
                           (and other (list (second other) (first other) (third other)))))))
          (when named
            (let ((nm (1+ (position (first named) *short-month-names* :test #'string-equal))) (nd (parse-integer (second named))))
              (date (cond ((third named) (parse-integer (third named))) ((past-p year nm nd) (1+ year)) (t year)) nm nd))))
        (let ((word (take :word)))
          (when word
            (to-day (+ (day-number y m d) (cond ((string= (first word) "tomorrow") 1) ((string= (first word) "yesterday") -1) (t 0))))))
        (let ((weekday-name (take :weekday)))
          (when weekday-name
            (let ((ahead (mod (- (position (first weekday-name) *short-weekday-names* :test #'string-equal) weekday) 7)))
              (to-day (+ (day-number y m d) (if (zerop ahead) 7 ahead))))))
        (let ((relative (take :relative)))
          (when relative
            (let ((value (* (parse-integer (second relative)) (if (char= (char (first relative) 0) #\-) -1 1)))
                  (unit (if (plusp (length (third relative))) (char (third relative) 0) #\d)))
              (to-day (case unit
                        (#\m (day-number y (+ m value) d))
                        (#\y (day-number (+ y value) m d))
                        (t (+ (day-number y m d) (* value (if (char= unit #\w) 7 1)))))))))
        (let ((year-only (take :year)))
          (when year-only
            (to-day (day-number (parse-integer (first year-only)) m d))))
        (let ((day-only (take :day)))
          (when day-only
            (let ((dd (parse-integer (first day-only))))
              (if (>= dd day)
                  (date year month dd)
                  (date (if (= month 12) (1+ year) year) (1+ (mod month 12)) dd)))))
        (take :today)
        (and (string= (string-trim '(#\Space #\Tab) text) "")
             (valid-date-p y m d)
             (values (day-number y m d) hour minute end-hour end-minute))))))

;;; Dates and times as text writes them, read where they stand without
;;; garbage: a search reads them at every place of a text.

(defconstant +day-nanoseconds+ (* 86400 1000000000)
  "Nanoseconds in a day.")

(declaim (inline ascii-digit))
(defun ascii-digit (text i end)
  "The value of the digit 0 to 9 at index I of TEXT, or NIL when there is
none before END."
  (and (< i end) (let ((c (char text i))) (and (char<= #\0 c #\9) (- (char-code c) 48)))))

(defun fixed-digits (text start count end)
  "The number the COUNT digits at START of TEXT write, or NIL when there are
not COUNT digits there before END."
  (let ((value 0))
    (dotimes (k count value)
      (let ((d (ascii-digit text (+ start k) end)))
        (if d (setf value (+ (* value 10) d)) (return nil))))))

(defun short-digits (text start end)
  "One or two digits at START of TEXT, as two values: the number they write
and the index after them; NIL when there is no digit."
  (let ((first (ascii-digit text start end)))
    (when first
      (let ((second (ascii-digit text (1+ start) end)))
        (if second
            (values (+ (* 10 first) second) (+ start 2))
            (values first (1+ start)))))))

(defun char-at-p (text i end chars)
  "True when the character at index I of TEXT, before END, is one of CHARS."
  (and (< i end) (find (char text i) chars)))

(defun hour-minute-at (text start end &key two-digit-hour (separators ":") (last 23))
  "An hour and minute of a day at START of TEXT, H:MM or HH:MM (two digits
of hour with TWO-DIGIT-HOUR), apart by one of SEPARATORS, the hour 0 to LAST
and the minute 0 to 59. Four values: the index after it, the hour, the
minute and the separator; NIL when none is there."
  (multiple-value-bind (hour at)
      (if two-digit-hour
          (let ((h (fixed-digits text start 2 end))) (and h (values h (+ start 2))))
          (short-digits text start end))
    (when (and hour (<= hour last) (char-at-p text at end separators))
      (let ((minute (fixed-digits text (1+ at) 2 end)))
        (when (and minute (<= minute 59))
          (values (+ at 3) hour minute (char text at)))))))

(defun meridiem-at (text start end)
  "AM or PM at START of TEXT, after one space or none, its two letters of one
case (am, PM), no letter right after. Two values: the index after it, and
:AM or :PM; NIL when neither is there."
  (let ((at (if (char-at-p text start end " ") (1+ start) start)))
    (when (and (char-at-p text at end "AaPp") (char-at-p text (1+ at) end "Mm")
               (eq (upper-case-p (char text at)) (upper-case-p (char text (1+ at))))
               (not (and (< (+ at 2) end) (alpha-char-p (char text (+ at 2))))))
      (values (+ at 2) (if (char-equal (char text at) #\a) :am :pm)))))

(defun meridiem-hour (hour half)
  "HOUR of a twelve-hour clock, 1 to 12, in HALF of the day, :AM or :PM, as
an hour of the day, 0 to 23: 12 AM is 0, 12 PM is 12; NIL when HOUR is not 1
to 12."
  (and (<= 1 hour 12) (+ (mod hour 12) (if (eq half :pm) 12 0))))

(defun time-at (text start end &key (last 23))
  "A time of day at START of TEXT: 9:30, 09:30, 9:30pm, 9:30 PM, 3pm or 3 PM,
AM or PM as MERIDIEM-AT reads it, the hour up to LAST (24 lets 24:00 end a
day). Three values: the index after it, the hour 0 to LAST and the minute;
NIL when none is there."
  (multiple-value-bind (at hour minute) (hour-minute-at text start end :last last)
    (if at
        (multiple-value-bind (after half) (meridiem-at text at end)
          (cond (half (let ((hour (meridiem-hour hour half))) (and hour (values after hour minute))))
                ((or (< hour 24) (zerop minute)) (values at hour minute))))
        (multiple-value-bind (hour at) (short-digits text start end)
          (when hour
            (multiple-value-bind (after half) (meridiem-at text at end)
              (let ((hour (and half (meridiem-hour hour half))))
                (and hour (values after hour 0)))))))))

(defun time-range-at (text start end)
  "A time of day or a range of two at START of TEXT, each as TIME-AT reads
it: 14:00, 3pm, 9:30-10:00, 10am-11:30am, the end 24:00 too; no digit or
colon right after. Five values: the index after it, the hour and minute it
begins at, and the hour and minute it ends at, NIL for no range; NIL when
no time of day is there, or a range's end is none (10:00-25:00)."
  (multiple-value-bind (at hour minute) (time-at text start end)
    (when at
      (multiple-value-bind (after end-hour end-minute)
          (and (char-at-p text at end "-") (ascii-digit text (1+ at) end) (time-at text (1+ at) end :last 24))
        (when (and (null after) (char-at-p text at end "-") (ascii-digit text (1+ at) end))
          (return-from time-range-at nil))
        (let ((at (or after at)))
          (unless (or (ascii-digit text at end) (char-at-p text at end ":"))
            (values at hour minute end-hour end-minute)))))))

(defun find-time-range (text &key (start 0) (end (length text)))
  "The first time of day or range of two in TEXT from START below END that
TIME-RANGE-AT reads, not right after a digit, colon or hyphen. Six values:
where it begins and ends, the hour and minute it begins at, and the hour and
minute it ends at, NIL for no range; NIL when there is none."
  (loop for i from start below end
        when (and (ascii-digit text i end)
                  (not (and (> i start) (find (char text (1- i)) "0123456789:-"))))
          do (multiple-value-bind (at hour minute end-hour end-minute) (time-range-at text i end)
               (when at (return (values i at hour minute end-hour end-minute))))))

(defun read-time-range (text)
  "TEXT, a time of day or a range of two and nothing else but spaces around
it (TIME-RANGE-AT), as four values: the hour and minute it begins at, and
the hour and minute it ends at, NIL for no range; NIL when it is none."
  (let* ((end (or (position #\Space text :from-end t :test-not #'char=) -1))
         (start (or (position #\Space text :test-not #'char=) 0)))
    (multiple-value-bind (at hour minute end-hour end-minute) (time-range-at text start (1+ end))
      (and at (= at (1+ end)) (values hour minute end-hour end-minute)))))

(defun time-range-text (hour minute &optional end-hour end-minute)
  "HOUR:MINUTE written as a timestamp writes a time, 09:30, and the range to
END-HOUR:END-MINUTE when it is given, 09:30-10:00; NIL when HOUR is NIL."
  (and hour (format nil "~2,'0d:~2,'0d~@[-~2,'0d~]~@[:~2,'0d~]" hour minute end-hour (and end-hour end-minute))))

(defun date-time-text (day &optional hour minute end-hour end-minute)
  "The day number DAY as a timestamp writes it between its brackets, its date
and weekday, 2026-12-24 Thu, and the time or range of times when HOUR is
given (TIME-RANGE-TEXT): 2026-12-24 Thu 14:00-15:30."
  (format nil "~a~@[ ~a~]" (format-day "%F %a" day) (time-range-text hour minute end-hour end-minute)))

(defun clock-at (text start end &key two-digit-hour (separators ":") (fraction-marks ".,"))
  "A time of day at START of TEXT, hours and minutes (HOUR-MINUTE-AT), then
seconds and parts of a second if written: H:MM, HH:MM:SS, HH:MM:SS.123 and
the like, the minutes and seconds apart by one of SEPARATORS, the same each
time, the parts of a second after one of FRACTION-MARKS other than that one.
Three values: the index after it, the nanoseconds since midnight, and the
nanoseconds the last figure written counts in (a minute, a second, a tenth
...); NIL when no time of day is there."
  (multiple-value-bind (at hour minute separator)
      (hour-minute-at text start end :two-digit-hour two-digit-hour :separators separators)
    (when at
      (let ((nanos (* (+ (* hour 3600) (* minute 60)) 1000000000))
            (unit (* 60 1000000000)))
        (when (and (< at end) (char= (char text at) separator))
          (let ((second (fixed-digits text (1+ at) 2 end)))
            ;; 60: a leap second, as RFC 3339 allows
            (when (and second (<= second 60))
              (setf at (+ at 3)
                    nanos (+ nanos (* second 1000000000))
                    unit 1000000000)
              (when (and (char-at-p text at end fraction-marks) (char/= (char text at) separator) (ascii-digit text (1+ at) end))
                (incf at)
                (let ((scale 100000000))
                  (loop for d = (ascii-digit text at end)
                        while d
                        do (when (plusp scale)
                             (incf nanos (* d scale))
                             (setf unit scale scale (floor scale 10)))
                           (incf at)))))))
        (values at nanos unit)))))

(defun zone-at (text start end)
  "An offset from UTC at START of TEXT: Z, +02:00, +0200 or +02 (or - for
west), after one space too, or a space and UTC or GMT. Two values: the
index after it and the offset in minutes east of UTC; NIL when there is
none."
  (let ((at (if (and (< start end) (char= (char text start) #\Space)) (1+ start) start)))
    (cond ((and (= at start) (char-at-p text at end "Zz")) (values (1+ at) 0))
          ((and (> at start) (<= (+ at 3) end)
                (or (string= "UTC" text :start2 at :end2 (+ at 3)) (string= "GMT" text :start2 at :end2 (+ at 3)))
                (not (and (< (+ at 3) end) (alpha-char-p (char text (+ at 3))))))
           (values (+ at 3) 0))
          ((char-at-p text at end "+-")
           (let ((sign (if (char= (char text at) #\-) -1 1))
                 (hours (fixed-digits text (1+ at) 2 end)))
             (when (and hours (<= hours 23))
               (let* ((colon (char-at-p text (+ at 3) end ":"))
                      (minutes (fixed-digits text (+ at 3 (if colon 1 0)) 2 end)))
                 (cond ((and minutes (<= minutes 59))
                        (values (+ at 5 (if colon 1 0)) (* sign (+ (* 60 hours) minutes))))
                       ((not colon) (values (+ at 3) (* sign 60 hours)))))))))))

(defun written-date-at (text start end format)
  "A date written in FORMAT, :ISO, :DMY or :MDY, at START of TEXT: 2026-06-30
for :ISO; 30.06.2026, 1.7.2026 or 30/06/2026 for :DMY; 06/30/2026 or 7/1/2026
for :MDY, the parts apart by . / or - (/ or - for :MDY), the same both
times. Four values: its year, month and day, and the index after it; NIL when
no date that is in the calendar is there."
  (multiple-value-bind (year month day at)
      (ecase format
        (:iso (let ((y (fixed-digits text start 4 end))
                    (m (and (char-at-p text (+ start 4) end "-") (fixed-digits text (+ start 5) 2 end)))
                    (d (and (char-at-p text (+ start 7) end "-") (fixed-digits text (+ start 8) 2 end))))
                (and y m d (values y m d (+ start 10)))))
        ((:dmy :mdy)
         (multiple-value-bind (a at) (short-digits text start end)
           (when (and a (char-at-p text at end (if (eq format :dmy) "./-" "/-")))
             (let ((mark (char text at)))
               (multiple-value-bind (b at) (short-digits text (1+ at) end)
                 (when (and b (< at end) (char= (char text at) mark))
                   (let ((y (fixed-digits text (1+ at) 4 end)))
                     (when y
                       (if (eq format :mdy)
                           (values y a b (+ at 5))
                           (values y b a (+ at 5))))))))))))
    (when (and at (valid-date-p year month day))
      (values year month day at))))

(defun date-time-at (text start end format)
  "A date written in FORMAT (WRITTEN-DATE-AT) at START of TEXT, and the time
of day and offset from UTC after it when written, no digit right after it.
Five values: the index after it; its day number (DAY-NUMBER);
the nanoseconds since that day's midnight; the offset in minutes east of
UTC, or NIL when none is written; and the nanoseconds its last figure counts
in, a day's for a date alone. NIL when no date is there."
  (multiple-value-bind (year month day at) (written-date-at text start end format)
    (when at
      (let ((nanos 0) (unit +day-nanoseconds+) (zone nil) (date-end at))
        (flet ((take-clock (from &key two-digit-hour (separators ":"))
                 (multiple-value-bind (after clock-nanos clock-unit)
                     (clock-at text from end :two-digit-hour two-digit-hour :separators separators)
                   (when after
                     (setf at after nanos clock-nanos unit clock-unit)
                     t))))
          (ecase format
            (:iso
             (cond ((char-at-p text at end "Tt")
                    ;; a T promises a time: without one it is no date-time
                    (unless (take-clock (1+ at) :two-digit-hour t)
                      (return-from date-time-at nil)))
                   ((char-at-p text at end " ")
                    (take-clock (1+ at) :two-digit-hour t))))
            (:dmy
             (when (char-at-p text at end " ")
               (take-clock (1+ at) :separators ":.")))
            (:mdy
             (let ((from (cond ((char-at-p text at end " ") (1+ at))
                               ((and (char-at-p text at end ",") (char-at-p text (1+ at) end " ")) (+ at 2)))))
               (when (and from (take-clock from))
                 (multiple-value-bind (after half) (meridiem-at text at end)
                   (when after
                     (let* ((hour (floor nanos (* 3600 1000000000)))
                            (day-hour (or (meridiem-hour hour half) (return-from date-time-at nil))))
                       (incf nanos (* 3600 1000000000 (- day-hour hour))))
                     (setf at after))))))))
        (when (< unit +day-nanoseconds+)
          (multiple-value-bind (after minutes) (zone-at text at end)
            (when after (setf at after zone minutes))))
        (cond ((not (ascii-digit text at end))
               (values at (day-number year month day) nanos zone unit))
              ;; a time after a space or comma that runs into more digits was none: the date alone
              ((and (> at date-end) (not (char-at-p text date-end end "Tt")))
               (values date-end (day-number year month day) 0 nil +day-nanoseconds+)))))))

(defun utc-moment (day nanos zone)
  "The moment DAY and NANOS (as DATE-TIME-AT gives them) name at ZONE minutes
east of UTC, as day and nanoseconds in UTC: two values."
  (multiple-value-bind (days rest) (floor (- nanos (* zone 60 1000000000)) +day-nanoseconds+)
    (values (+ day days) rest)))

(defun moment< (day nanos other-day other-nanos)
  "True when DAY and NANOS come before OTHER-DAY and OTHER-NANOS."
  (or (< day other-day) (and (= day other-day) (< nanos other-nanos))))

(defun moment-after (day nanos unit)
  "The moment UNIT nanoseconds after DAY and NANOS, as two values."
  (multiple-value-bind (days rest) (floor (+ nanos unit) +day-nanoseconds+)
    (values (+ day days) rest)))

;;; Dates as files write them

(defun pdf-date-words (text)
  "TEXT, a date as a PDF writes it (D:20240301120000+13'00'), as words in its
own zone: 2024-03-01 12:00:00 +13:00; TEXT as it is when it is no such date.
Two more values: the universal time it names (its zone taken as UTC when it
gives none), NIL when it names none; and its zone in minutes east of UTC, NIL
when it gives none."
  (let* ((text (or text ""))
         (end (length text))
         (at (if (char-at-p text 0 end "D") 1 0))
         (at (if (char-at-p text at end ":") (1+ at) at))
         (year (fixed-digits text at 4 end)))
    (if (null year)
        text
        ;; month, day, hour, minute and second, two digits each, as far as written
        (let ((parts (list 1 1 0 0 0)) (zone nil) (mark nil))
          (incf at 4)
          (loop for cell on parts
                for part = (fixed-digits text at 2 end)
                while part do (setf (car cell) part) (incf at 2))
          (when (char-at-p text at end "Z+-")
            (setf mark (char text at))
            (let* ((hours (fixed-digits text (1+ at) 2 end))
                   (after (if (char-at-p text (+ at 3) end "'") (+ at 4) (+ at 3)))
                   (minutes (and hours (fixed-digits text after 2 end))))
              (setf zone (* (if (char= mark #\-) -1 1) (+ (* 60 (or hours 0)) (or minutes 0))))))
          (destructuring-bind (month day hour minute second) parts
            (values
             (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]" year month day hour minute second
                     (cond ((null zone) nil) ((char= mark #\Z) "UTC")
                           (t (format nil "~c~2,'0d:~2,'0d" mark (floor (abs zone) 60) (mod (abs zone) 60)))))
             (and (valid-date-p year month day) (<= hour 23) (<= minute 59) (<= second 59)
                  (- (+ (* 86400 (day-number year month day)) (* 3600 hour) (* 60 minute) second) (* 60 (or zone 0))))
             zone))))))

(defun iso-date-time (text)
  "TEXT, a date as XMP and ISO 8601 write it (2024-03-01T12:00:00+13:00, or
2024-03 for a month), as two values: the universal time it names (its zone
taken as UTC when it gives none), and its zone in minutes east of UTC, NIL
when it gives none; NIL when it names no date."
  (let* ((text (or text "")) (end (length text)))
    (multiple-value-bind (after day nanos zone) (date-time-at text 0 end :iso)
      (cond (after
             (values (- (+ (* 86400 day) (floor nanos 1000000000)) (* 60 (or zone 0))) zone))
            ((written-date-at text 0 end :iso)
             (multiple-value-bind (year month day) (written-date-at text 0 end :iso)
               (values (* 86400 (day-number year month day)) nil)))
            ;; a month alone: its first day
            (t (let ((year (fixed-digits text 0 4 end))
                     (month (and (char-at-p text 4 end "-") (fixed-digits text 5 2 end))))
                 (when (and year month (<= 1 month 12) (not (ascii-digit text 7 end)) (not (char-at-p text 7 end "-")))
                   (values (* 86400 (day-number year month 1)) nil))))))))
