;;;; test/dates.lisp --- Dates and times: day numbers, weekdays, ISO weeks, local time and times written in a pattern.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test dates
  :description "Dates and times in one place: day numbers for any Gregorian date and back, weekdays from Sunday 0, ISO weeks, month lengths and leap years, local time of a day before 1900 and after, and times written in a pattern of % specifiers.")

(defun date-of (number)
  "The day NUMBER as a list (YEAR MONTH DAY)."
  (multiple-value-list (tuatara.dates:day-date number)))

(define-test (dates day-numbers)
  ;; 1900-01-01 is day 0, the days before it below 0, any year
  (is = 0 (tuatara.dates:day-number 1900 1 1))
  (is = -1 (tuatara.dates:day-number 1899 12 31))
  (is = -18200 (tuatara.dates:day-number 1850 3 4))
  (is = 58 (tuatara.dates:day-number 1900 2 28))
  (is = 59 (tuatara.dates:day-number 1900 3 1))
  (is = 36583 (tuatara.dates:day-number 2000 2 29))
  (is = 46297 (tuatara.dates:day-number 2026 10 4))
  (is = -693595 (tuatara.dates:day-number 1 1 1))
  (dolist (date '((1850 3 4) (1899 12 31) (1900 1 1) (1900 2 28) (1900 3 1) (2000 2 29) (2026 10 4)
                  (1 1 1) (1600 2 29) (2400 12 31) (1582 10 10) (-44 3 15)))
    (is equal date (date-of (apply #'tuatara.dates:day-number date)) "~a round trip" date))
  (is equal '() (loop for day from -800000 to 800000 by 997
                      unless (= day (apply #'tuatara.dates:day-number (date-of day))) collect day))
  ;; a month or day out of range counts on: 1900 has no 29 February
  (is equal '(1900 3 1) (date-of (tuatara.dates:day-number 1900 2 29)))
  (is equal '(2027 1 1) (date-of (tuatara.dates:day-number 2026 13 1)))
  (is equal '(2025 12 31) (date-of (tuatara.dates:day-number 2026 0 31)))
  (is equal '(2026 2 28) (date-of (tuatara.dates:day-number 2026 3 0)))
  (is equal '(2026 3 3) (date-of (tuatara.dates:day-number 2026 2 31)))
  (is equal '(2027 2) (multiple-value-list (tuatara.dates:add-months 2026 11 3)))
  (is equal '(2025 12) (multiple-value-list (tuatara.dates:add-months 2026 1 -1))))

(define-test (dates weekdays-and-weeks)
  ;; Sunday 0 to Saturday 6
  (is = 0 (tuatara.dates:weekday (tuatara.dates:day-number 2026 10 4)))
  (is = 6 (tuatara.dates:weekday (tuatara.dates:day-number 2026 10 3)))
  (is = 1 (tuatara.dates:weekday (tuatara.dates:day-number 1900 1 1)))
  (is = 0 (tuatara.dates:weekday (tuatara.dates:day-number 1899 12 31)))
  (is = 1 (tuatara.dates:weekday (tuatara.dates:day-number 1850 3 4)))
  (is = 2 (tuatara.dates:weekday (tuatara.dates:day-number 1600 2 29)))
  ;; the first day of a week, weeks from Monday, Sunday (0 or 7) or Saturday
  (flet ((start (y m d first) (date-of (tuatara.dates:week-start (tuatara.dates:day-number y m d) first))))
    (is equal '(2026 9 28) (start 2026 10 4 1))
    (is equal '(2026 10 4) (start 2026 10 4 0))
    (is equal '(2026 10 4) (start 2026 10 4 7))
    (is equal '(2026 10 3) (start 2026 10 4 6))
    (is equal '(1899 12 25) (start 1899 12 31 1))
    (is equal '(1900 1 1) (start 1900 1 1 1)))
  ;; ISO weeks at the years' ends
  (flet ((iso (y m d) (multiple-value-list (tuatara.dates:iso-week (tuatara.dates:day-number y m d)))))
    (is equal '(2020 53 4) (iso 2020 12 31))
    (is equal '(2020 53 7) (iso 2021 1 3))
    (is equal '(2021 1 1) (iso 2021 1 4))
    (is equal '(2026 1 4) (iso 2026 1 1))
    (is equal '(2026 53 5) (iso 2027 1 1))
    (is equal '(2009 1 1) (iso 2008 12 29))
    (is equal '(1899 52 7) (iso 1899 12 31))
    (is equal '(1900 1 1) (iso 1900 1 1))
    (is equal '(1850 10 1) (iso 1850 3 4)))
  (is equal '(2020 12 28) (date-of (tuatara.dates:iso-week-monday 2020 53)))
  (is equal '(2025 12 29) (date-of (tuatara.dates:iso-week-monday 2026 1)))
  (is equal '(2026 9 28) (date-of (tuatara.dates:iso-week-monday 2026 40)))
  (is equal '(1849 12 31) (date-of (tuatara.dates:iso-week-monday 1850 1)))
  ;; days of the year
  (is = 79 (tuatara.dates:year-day (tuatara.dates:day-number 2026 3 20)))
  (is = 366 (tuatara.dates:year-day (tuatara.dates:day-number 2024 12 31)))
  (is = 60 (tuatara.dates:year-day (tuatara.dates:day-number 1900 3 1))))

(define-test (dates months-and-years)
  (is = 28 (tuatara.dates:month-length 1900 2))
  (is = 29 (tuatara.dates:month-length 2000 2))
  (is = 29 (tuatara.dates:month-length 2024 2))
  (is = 28 (tuatara.dates:month-length 2100 2))
  (is = 30 (tuatara.dates:month-length 2026 4))
  (is = 31 (tuatara.dates:month-length 2026 12))
  (false (tuatara.dates:leap-year-p 1900))
  (true (tuatara.dates:leap-year-p 2000))
  (true (tuatara.dates:leap-year-p 1848))
  (false (tuatara.dates:leap-year-p 2026))
  (true (tuatara.dates:valid-date-p 2000 2 29))
  (false (tuatara.dates:valid-date-p 1900 2 29))
  (false (tuatara.dates:valid-date-p 2026 13 1))
  (false (tuatara.dates:valid-date-p 2026 4 31))
  (false (tuatara.dates:valid-date-p 2026 4 0))
  ;; the month lengths add up to the days between the years' first days
  (loop for year from 1890 to 2030
        do (is = (- (tuatara.dates:day-number (1+ year) 1 1) (tuatara.dates:day-number year 1 1))
               (loop for month from 1 to 12 sum (tuatara.dates:month-length year month)) "~d" year)))

(define-test (dates local-time)
  (flet ((at (y m d &optional (h 0) (mi 0) (s 0))
           (tuatara.dates:day-universal-time (tuatara.dates:day-number y m d) h mi s))
         (back (time) (multiple-value-list (tuatara.dates:time-day time))))
    ;; times count on below 1900, an hour 3600 seconds either side of it and of 1901
    (is = 7200 (- (at 1900 1 1 1) (at 1899 12 31 23)))
    (is = 3600 (- (at 1901 1 1 0 30) (at 1900 12 31 23 30)))
    (is = (* 18200 86400) (- (at 1900 1 1) (at 1850 3 4)))
    (true (minusp (at 1850 3 4)))
    ;; and read back as they were
    (dolist (moment '((1850 3 4 9 30 15) (1899 12 31 23 0 0) (1900 1 1 1 0 0) (1900 12 31 23 59 59)
                      (1901 1 1 0 30 0) (1970 1 1 0 0 0) (2026 10 4 10 30 15) (2400 2 29 12 0 0)))
      (destructuring-bind (y m d h mi s) moment
        (is equal (list (tuatara.dates:day-number y m d) h mi s) (back (at y m d h mi s)) "~a" moment)))
    ;; an hour, minute or second out of range counts on into the days either side
    (is = (at 2026 10 5) (at 2026 10 4 24))
    (is = (at 1850 3 5) (at 1850 3 4 24))
    (is = (at 2026 10 3 23 59) (at 2026 10 4 0 -1))
    ;; the offset from universal time, the same before 1901 as at its start
    (is = (tuatara.dates:local-offset (at 1901 1 1 12)) (tuatara.dates:local-offset (at 1850 3 4 12)))
    (is = (- (at 2026 10 4 12) (* 86400 (tuatara.dates:day-number 2026 10 4)) (* 12 3600))
        (- (tuatara.dates:local-offset (at 2026 10 4 12))))
    ;; the times of files
    (is = tuatara.dates:+unix-epoch+ (tuatara.dates:mtime-universal-time 0))
    (is = (1+ tuatara.dates:+unix-epoch+) (tuatara.dates:mtime-universal-time 1999999999))
    (is equal (list (tuatara.dates:day-number 1970 1 1) 0 0 0)
        (back (- tuatara.dates:+unix-epoch+ (tuatara.dates:local-offset tuatara.dates:+unix-epoch+))))))

(define-test (dates written-in-a-pattern)
  (let ((time (tuatara.dates:day-universal-time (tuatara.dates:day-number 2026 3 20) 13 39 58)))
    ;; what Emacs's format-time-string writes for the same moment
    (is string= "Friday 20 March 2026" (tuatara.dates:format-time "%A %-e %B %Y" time))
    (is string= "Fri Mar 20 20|079 12 2026 5 5 2026-03-20 13:39:58 13:39 01 PM PM 26 %"
        (tuatara.dates:format-time "%a %b %d %e|%j %V %G %u %w %F %T %R %I %p %^p %y %%" time))
    (is string= "79 3 20 1" (tuatara.dates:format-time "%-j %-m %-d %-I" time))
    (is string= "FRI MARCH" (tuatara.dates:format-time "%^a %^B" time))
    (is string= "20260320T133958" (tuatara.dates:format-time "%Y%m%dT%H%M%S" time))
    (is string= "[2026-03-20 Fri 13:39]" (tuatara.dates:format-time "[%F %a %R]" time))
    ;; the offset from universal time, with and without a colon
    (let ((plain (tuatara.dates:format-time "%z" time))
          (colon (tuatara.dates:format-time "%:z" time))
          (east (tuatara.dates:local-offset time)))
      (is string= (format nil "~:[+~;-~]~2,'0d~2,'0d" (minusp east) (floor (abs east) 3600) (mod (floor (abs east) 60) 60)) plain)
      (is string= (format nil "~a:~a" (subseq plain 0 3) (subseq plain 3)) colon)))
  (is string= "53 2026 001 Fri"
      (tuatara.dates:format-time "%V %G %j %a" (tuatara.dates:day-universal-time (tuatara.dates:day-number 2027 1 1) 12)))
  ;; midnight and noon on a twelve-hour clock
  (is string= "12 AM, 12 PM" (format nil "~a, ~a" (tuatara.dates:format-day "%I %p" 0 0) (tuatara.dates:format-day "%I %p" 0 12)))
  ;; before 1900, as on any other day
  (is string= "1850-03-04 Mon Monday March Mar 063 10 1850 1 1  4"
      (tuatara.dates:format-day "%F %a %A %B %b %j %V %G %u %w %e" (tuatara.dates:day-number 1850 3 4)))
  (is string= "1850-03-04 09:30:15"
      (tuatara.dates:format-time "%F %T" (tuatara.dates:day-universal-time (tuatara.dates:day-number 1850 3 4) 9 30 15)))
  (is string= "1899-12-31 Sun 23:59"
      (tuatara.dates:format-time "%F %a %R" (tuatara.dates:day-universal-time (tuatara.dates:day-number 1899 12 31) 23 59)))
  ;; years of fewer than four digits padded to four, as Org's timestamps have them
  (is string= "0999-01-01 Tue 999" (tuatara.dates:format-day "%F %a %-Y" (tuatara.dates:day-number 999 1 1)))
  ;; a % with no specifier after it, or one not known, written as it is
  (is string= "50% %Q" (tuatara.dates:format-day "50% %Q" 0))
  (is string= "100%" (tuatara.dates:format-day "100%" 0)))

(define-test (dates read-as-typed)
  ;; a date as typed, read the one way every question for a date reads it; today Saturday 2026-10-03
  (let ((today (tuatara.dates:day-number 2026 10 3)))
    (flet ((read-as (text) (multiple-value-list (tuatara.dates:read-date-text text today)))
           (day (y m d) (tuatara.dates:day-number y m d)))
      (is equal (list today nil nil nil nil) (read-as ""))
      (is equal (list today nil nil nil nil) (read-as "."))
      (is equal (list today nil nil nil nil) (read-as "Today"))
      (is equal (list (day 2026 10 4) nil nil nil nil) (read-as "tomorrow"))
      (is equal (list (day 2026 10 2) nil nil nil nil) (read-as "yesterday"))
      (is equal (list (day 2026 10 4) 15 0 nil nil) (read-as "tomorrow 3pm"))
      (is equal (list (day 2026 10 6) nil nil nil nil) (read-as "+3d"))
      (is equal (list (day 2026 9 26) nil nil nil nil) (read-as "-1w"))
      (is equal (list (day 2026 10 23) nil nil nil nil) (read-as "2026-10-20 +3d"))
      (is equal (list (day 2026 10 9) nil nil nil nil) (read-as "fri"))
      (is equal (list (day 2026 10 10) nil nil nil nil) (read-as "sat"))
      (is equal (list (day 2026 10 20) 10 0 11 30) (read-as "2026-10-20 10:00-11:30"))
      (is equal (list (day 2027 9 30) nil nil nil nil) (read-as "sep 30"))
      (is equal (list (day 2027 9 1) nil nil nil nil) (read-as "1 sep"))
      (is equal (list (day 2026 10 5) nil nil nil nil) (read-as "5"))
      (is equal (list (day 2026 11 1) nil nil nil nil) (read-as "1"))
      (is equal (list (day 2026 3 3) nil nil nil nil) (read-as "2026-01-31 +1m"))
      (is equal '(nil) (read-as "2026-02-30"))
      (is equal '(nil) (read-as "25:00"))
      (is equal '(nil) (read-as "not a date")))))

(define-test (dates days-and-times-that-do-not-exist)
  ;; a day or a time typed that does not exist names no date, never one rolled on
  (let ((today (tuatara.dates:day-number 2026 10 20)))
    (flet ((read-as (text) (multiple-value-list (tuatara.dates:read-date-text text today))))
      (is equal '(nil) (read-as "45"))
      (is equal '(nil) (read-as "0"))
      (is equal '(nil) (read-as "13pm"))
      (is equal '(nil) (read-as "0am"))
      (is equal '(nil) (read-as "2026-10-21 10:00-27:99"))
      (is equal (list (tuatara.dates:day-number 2026 10 31) nil nil nil nil) (read-as "31"))
      (is equal (list (tuatara.dates:day-number 2026 11 19) nil nil nil nil) (read-as "19"))
      (is equal (list today 12 0 nil nil) (read-as "12pm"))
      (is equal (list today 0 0 nil nil) (read-as "12am")))
    ;; the 31st asked on the 20th of a month of 30 days: no such day
    (is equal '(nil) (multiple-value-list (tuatara.dates:read-date-text "31" (tuatara.dates:day-number 2026 11 20))))
    ;; in December, a day before today is in January
    (is equal (list (tuatara.dates:day-number 2027 1 5) nil nil nil nil)
        (multiple-value-list (tuatara.dates:read-date-text "5" (tuatara.dates:day-number 2026 12 20))))))

(define-test (dates short-time-words-of-lists)
  ;; a time as the lists show it, in local time to the minute
  (is string= "2026-10-07 09:15" (tuatara.dates:short-time-words (encode-universal-time 30 15 9 7 10 2026)))
  (false (tuatara.dates:short-time-words nil)))

(define-test (dates read-as-typed-without-making-patterns)
  ;; each key at a date question reads what is typed: its patterns are made once, not at each read
  (let ((today (tuatara.dates:day-number 2026 10 3)))
    (flet ((consed (text)
             (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring (tuatara.dates:read-date-text text today))))))
      (dolist (text '("fri 10:00" "sep 30 3pm" "2026-10-20 +3d" "tomorrow"))
        (let ((bytes (consed text)))
          (true (< bytes 8000) "~d bytes to read ~s" bytes text))))))

(define-test (dates written-in-files-read-where-they-stand)
  ;; dates as XMP, ISO 8601 and PDF write them, read by the readers the search reads dates with
  (flet ((iso (text) (multiple-value-list (tuatara.dates:iso-date-time text))))
    (is equal (list (- (encode-universal-time 0 0 12 1 3 2024 0) (* 13 3600)) 780) (iso "2024-03-01T12:00:00+13:00"))
    (is equal (list (encode-universal-time 30 5 14 1 3 2024 0) 0) (iso "2024-03-01T14:05:30.25Z"))
    (is equal (list (encode-universal-time 0 0 0 1 3 2024 0) nil) (iso "2024-03-01"))
    (is equal (list (encode-universal-time 0 0 0 1 3 2024 0) nil) (iso "2024-03"))
    ;; any year: the day before 1900 began
    (is equal (list -86400 nil) (iso "1899-12-31"))
    (is equal '(nil) (iso "2024-13"))
    (is equal '(nil) (iso "no date")))
  (is equal (list "2024-03-01 12:00:00 +13:00" (- (encode-universal-time 0 0 12 1 3 2024 0) (* 13 3600)) 780)
      (multiple-value-list (tuatara.dates:pdf-date-words "D:20240301120000+13'00'")))
  (is equal (list "2024-03-01 00:00:00 UTC" (encode-universal-time 0 0 0 1 3 2024 0) 0)
      (multiple-value-list (tuatara.dates:pdf-date-words "D:20240301Z")))
  (is equal (list "1899-12-31 00:00:00" -86400 nil) (multiple-value-list (tuatara.dates:pdf-date-words "D:18991231")))
  (is equal (list "2024-02-30 00:00:00" nil nil) (multiple-value-list (tuatara.dates:pdf-date-words "D:20240230")))
  (is string= "not one" (tuatara.dates:pdf-date-words "not one"))
  ;; a template's date, the calendar's date question and a clock table's span read them alike
  (is equal '(2026 10 5) (multiple-value-list (tuatara.defaults::docx-date-parts "  [2026-10-05 Mon 10:00]")))
  (false (tuatara.defaults::docx-date-parts "2026-02-31"))
  (is = (tuatara.dates:day-number 2026 10 3) (tuatara.defaults::calendar-read-day "2026-10-03"))
  (is = (tuatara.dates:day-number 2026 10 1) (tuatara.defaults::calendar-read-day "2026-10"))
  (false (tuatara.defaults::calendar-read-day "2026-13"))
  (is equal (list (tuatara.dates:day-universal-time (tuatara.dates:day-number 2026 10 5))
                  (tuatara.dates:day-universal-time (tuatara.dates:day-number 2026 10 6)))
      (subseq (multiple-value-list (tuatara.defaults::clocktable-block "2026-10-05" 0)) 0 2))
  (false (tuatara.defaults::clocktable-block "2026-02-31" 0)))

(define-test (dates times-of-day-read-one-way)
  ;; a time of day, or a range of two, read and written the one way everywhere
  (flet ((range (text) (multiple-value-list (tuatara.dates:read-time-range text))))
    (is equal '(9 30 nil nil) (range "9:30"))
    (is equal '(15 30 nil nil) (range "3:30pm"))
    (is equal '(15 30 nil nil) (range "3:30 PM"))
    (is equal '(0 15 nil nil) (range "12:15am"))
    (is equal '(12 0 nil nil) (range "12pm"))
    (is equal '(10 0 11 30) (range "10am-11:30am"))
    (is equal '(22 0 24 0) (range "22:00-24:00"))
    (dolist (none '("25:00" "13pm" "0am" "9:60" "10:00-25:00" "24:00" "3:30pM" "3pmx" "10:00:30" "3"))
      (is equal '(nil) (range none) "~s is no time" none)))
  (is string= "09:05" (tuatara.dates:time-range-text 9 5))
  (is string= "09:05-10:00" (tuatara.dates:time-range-text 9 5 10 0))
  (is string= "2026-12-24 Thu 14:00-15:30" (tuatara.dates:date-time-text (tuatara.dates:day-number 2026 12 24) 14 0 15 30))
  ;; found in text: not inside a date, nor right after a digit
  (is equal '(5 11 15 30 nil nil) (multiple-value-list (tuatara.dates:find-time-range "Call 3:30pm Bob")))
  (is equal '(nil) (multiple-value-list (tuatara.dates:find-time-range "2026-10-20 and 25:00")))
  ;; typed at a date question
  (let ((today (tuatara.dates:day-number 2026 10 3)))
    (is equal (list today 15 30 nil nil) (multiple-value-list (tuatara.dates:read-date-text "3:30pm" today)))
    (is equal (list (1+ today) 17 0 nil nil) (multiple-value-list (tuatara.dates:read-date-text "tomorrow 5 pm" today)))
    (is equal '(nil) (multiple-value-list (tuatara.dates:read-date-text "10:00-25:00" today)))))

(define-test (dates names-of-days-and-months-as-words)
  ;; a weekday or month is the beginning of its name, three letters at least, never a longer word
  (is = 5 (tuatara.dates:weekday-of-word "fri"))
  (is = 4 (tuatara.dates:weekday-of-word "Thurs"))
  (is = 1 (tuatara.dates:weekday-of-word "mondays"))
  (false (tuatara.dates:weekday-of-word "su"))
  (false (tuatara.dates:weekday-of-word "sunshine"))
  (is = 9 (tuatara.dates:month-of-word "sept"))
  (is = 5 (tuatara.dates:month-of-word "May"))
  (false (tuatara.dates:month-of-word "marching"))
  (let ((today (tuatara.dates:day-number 2026 10 3)))
    (flet ((read-as (text) (multiple-value-list (tuatara.dates:read-date-text text today))))
      (is equal '(nil) (read-as "sunshine"))
      (is equal '(nil) (read-as "marching 3"))
      (is equal '(nil) (read-as "3 marching"))
      (is equal (list (tuatara.dates:day-number 2027 3 3) nil nil nil nil) (read-as "march 3"))
      (is equal (list (tuatara.dates:day-number 2027 9 30) nil nil nil nil) (read-as "30 sept"))
      (is equal (list (tuatara.dates:day-number 2026 10 8) nil nil nil nil) (read-as "thursday"))))
  ;; the calendar's month question and a repeat's days read them alike
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026)))
    (is = (tuatara.dates:day-number 2027 3 1) (tuatara.defaults::calendar-read-month "mar 2027"))
    (false (tuatara.defaults::calendar-read-month "marching 2027")))
  (is equal '(1 3) (tuatara.defaults::org-repeat-weekday-set '("mondays" "and" "wed"))))

(define-test (dates ordinals-as-words)
  ;; the first five as words, read and written one way
  (is equal '(1 2 3 4 5 1 2 3 5) (mapcar #'tuatara.dates:ordinal-of-word '("first" "Second" "third" "fourth" "fifth" "1st" "2nd" "3rd" "5th")))
  (false (tuatara.dates:ordinal-of-word "2st"))
  (false (tuatara.dates:ordinal-of-word "sixth"))
  (let ((now (encode-universal-time 0 0 12 23 10 2026)))
    (is equal "<%%(diary-float t 2 2)>" (tuatara.defaults::org-words-repeat "2nd tuesday every month" now))
    (is equal "Every month on the second Tuesday, from Tue 10 Nov 2026"
        (tuatara.defaults::repeat-words '("start=2026-11-10" "repeats=monthly-nth")))
    (is equal '(2 nil) (multiple-value-list (tuatara.defaults::repeat-nth (tuatara.dates:day-number 2026 11 10))))
    (is equal '(5 t) (multiple-value-list (tuatara.defaults::repeat-nth (tuatara.dates:day-number 2026 10 30))))
    (is equal '(4 t) (multiple-value-list (tuatara.defaults::repeat-nth (tuatara.dates:day-number 2026 2 28))))))

(define-test (dates utc-time-words-any-year)
  ;; a time in universal time written as any date is, any year
  (is string= "2026-10-06 20:15:00 UTC" (tuatara.dates:utc-time-words (encode-universal-time 0 15 20 6 10 2026 0)))
  (is string= "1899-12-31 23:59:59 UTC" (tuatara.dates:utc-time-words -1))
  (is string= "1066-10-14 09:00:00 UTC"
      (tuatara.dates:utc-time-words (+ (* 86400 (tuatara.dates:day-number 1066 10 14)) (* 9 3600))))
  (false (tuatara.dates:utc-time-words nil)))
