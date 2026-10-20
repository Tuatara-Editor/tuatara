;;;; src/dates/package.lisp --- Package definition for tuatara.dates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.dates
  (:use #:cl)
  (:documentation "Dates and times, in one place for the whole editor: days as numbers
for any date of the Gregorian calendar, back to their year, month and day,
weekdays, ISO weeks, month lengths and leap years, local time of a day and
back for any year, the names of the days and months, and a time written in a
pattern of % specifiers.")
  (:export #:time-words #:utc-time-words #:day-number #:day-date #:weekday #:week-start #:iso-week #:iso-week-monday #:year-day
           #:leap-year-p #:month-length #:valid-date-p #:add-months
           #:day-universal-time #:time-day #:local-offset #:+unix-epoch+ #:mtime-universal-time
           #:*weekday-names* #:*short-weekday-names* #:*month-names* #:*short-month-names*
           #:format-day #:format-time #:short-time-words #:read-date-text #:pdf-date-words #:iso-date-time))
