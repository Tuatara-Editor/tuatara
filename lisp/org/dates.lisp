;;;; lisp/org/dates.lisp --- Org's timestamps read, moved and written as Org moves and writes them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A timestamp <2026-10-05 Mon 09:00-10:00 +1w -3d> is read into its date,
;;;; its times, its repeater (+1w, ++1w, .+1w: again a week on, a week on from
;;;; the last that is still to come, a week from today) and its warning or
;;;; delay (-3d, --3d), and written back in Org's order. Moving one by months
;;;; or years rolls over as Emacs's dates do: 31 January a month on is 3 March.

(in-package #:tuatara.defaults)

(defvar *org-now* nil
  "The universal time Org's commands take as now; NIL for the clock's.")

(defun org-now ()
  "The universal time Org's commands take as now: *ORG-NOW*, else the clock's."
  (or *org-now* (get-universal-time)))

(defun org-today (&optional (now (org-now)))
  "Today as a day number, at the universal time NOW, any year."
  (nth-value 0 (time-day now)))

(defstruct (org-ts (:conc-name org-ts-))
  "A timestamp read: ACTIVE (<> rather than []), YEAR MONTH DAY, HOUR and
MINUTE (NIL without a time), END-HOUR and END-MINUTE (a time range, else NIL),
the REPEATER (:cumulate +, :catch-up ++, :restart .+) with REPEAT-VALUE and
REPEAT-UNIT (#\\h #\\d #\\w #\\m #\\y), and the WARNING (:all -, :first --) with
WARNING-VALUE and WARNING-UNIT."
  active year month day hour minute end-hour end-minute
  repeater repeat-value repeat-unit warning warning-value warning-unit)

(defparameter *org-timestamp-pattern*
  (format nil "^([<\\[])~a[]>]$" (org-timestamp-body t))
  "A timestamp of one date (ORG-TIMESTAMP-BODY): its kind, its date, its
times, and its repeaters and warnings, each a group.")

(defparameter *org-timestamp-scanner* (cl-ppcre:create-scanner *org-timestamp-pattern*)
  "*ORG-TIMESTAMP-PATTERN* made ready to match once, not again at each timestamp read.")

(defun org-read-ts (text)
  "TEXT, one timestamp (<2026-10-05 Mon 09:00 +1w -3d>), read as an ORG-TS, or NIL."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings *org-timestamp-scanner* text)
    (when match
      (flet ((int (i) (and (aref groups i) (parse-integer (aref groups i)))))
        (let ((ts (make-org-ts :active (string= (aref groups 0) "<")
                               :year (int 1) :month (int 2) :day (int 3)
                               :hour (int 4) :minute (int 5) :end-hour (int 6) :end-minute (int 7))))
          (cl-ppcre:do-register-groups (mark value unit) ("(\\.\\+|\\+\\+|\\+|--|-)(\\d+)([hdwmy])" (or (aref groups 8) ""))
            (let ((value (parse-integer value)) (unit (char unit 0)))
              (cond ((member mark '("+" "++" ".+") :test #'string=)
                     (setf (org-ts-repeater ts) (cond ((string= mark "+") :cumulate) ((string= mark "++") :catch-up) (t :restart))
                           (org-ts-repeat-value ts) value (org-ts-repeat-unit ts) unit))
                    (t (setf (org-ts-warning ts) (if (string= mark "--") :first :all)
                             (org-ts-warning-value ts) value (org-ts-warning-unit ts) unit)))))
          ts)))))

(defun org-ts-day-number (ts)
  "The day of the timestamp TS as a day number, a date past its month's end
counted on into the next (2026-02-31 is 2026-03-03)."
  (day-number (org-ts-year ts) (org-ts-month ts) (org-ts-day ts)))

(defun org-ts-set-day (ts day)
  "Make TS's date the day number DAY, its time kept; TS is returned."
  (multiple-value-bind (year month date) (day-date day)
    (setf (org-ts-year ts) year (org-ts-month ts) month (org-ts-day ts) date))
  ts)

(defun org-ts-time (ts)
  "The universal time TS begins at (midnight when it has no time), any year (DAY-UNIVERSAL-TIME)."
  (day-universal-time (org-ts-day-number ts) (or (org-ts-hour ts) 0) (or (org-ts-minute ts) 0)))

(defun org-ts-universal-time (text)
  "The universal time of TEXT, one timestamp with a time of day
\([2026-10-01 Thu 17:30]), or NIL when it is none or names a day or time
that is not (2026-02-31, 25:00)."
  (let ((ts (org-read-ts text)))
    (and ts (org-ts-hour ts)
         (valid-date-p (org-ts-year ts) (org-ts-month ts) (org-ts-day ts))
         (<= (org-ts-hour ts) 23) (<= (org-ts-minute ts) 59)
         (org-ts-time ts))))

(defun org-ts-set-time (ts time)
  "Make TS's date (and its time, when it has one) that of the universal TIME, its time range kept as long."
  (multiple-value-bind (day hour minute) (time-day time)
    (when (and (org-ts-hour ts) (org-ts-end-hour ts))
      (let ((length (- (+ (* 60 (org-ts-end-hour ts)) (org-ts-end-minute ts)) (+ (* 60 (org-ts-hour ts)) (org-ts-minute ts)))))
        (multiple-value-bind (h m) (floor (+ (* 60 hour) minute length) 60)
          (setf (org-ts-end-hour ts) (mod h 24) (org-ts-end-minute ts) m))))
    (org-ts-set-day ts day)
    (when (org-ts-hour ts) (setf (org-ts-hour ts) hour (org-ts-minute ts) minute))
    ts))

(defun org-ts-shift (ts value unit)
  "Move TS by VALUE UNITs (#\\h #\\d #\\w #\\m #\\y). Months and years keep
the day of the month, a date past the month's end counted on into the next:
31 January a month on is 3 March (2 March in a leap year)."
  (case unit
    ((#\m #\y)
     (org-ts-set-day ts (day-number (if (char= unit #\y) (+ (org-ts-year ts) value) (org-ts-year ts))
                                    (if (char= unit #\m) (+ (org-ts-month ts) value) (org-ts-month ts))
                                    (org-ts-day ts))))
    (#\h (org-ts-set-time ts (+ (org-ts-time ts) (* value 3600))))
    (t (org-ts-set-day ts (+ (org-ts-day-number ts) (* value (if (char= unit #\w) 7 1))))))
  ts)

(defun org-ts-text (ts)
  "TS written as a note writes a timestamp: <2026-10-05 Mon 09:00-10:00 +1w -3d>."
  (format nil "~:[[~;<~]~a~@[-~a~]~@[ ~a~]~@[ ~a~]~:[]~;>~]"
          (org-ts-active ts)
          (format-day (if (org-ts-hour ts) "%F %a %R" "%F %a") (org-ts-day-number ts) (or (org-ts-hour ts) 0) (or (org-ts-minute ts) 0))
          (and (org-ts-hour ts) (org-ts-end-hour ts) (format-day "%R" 0 (org-ts-end-hour ts) (org-ts-end-minute ts)))
          (and (org-ts-repeater ts)
               (format nil "~a~d~c" (ecase (org-ts-repeater ts) (:cumulate "+") (:catch-up "++") (:restart ".+"))
                       (org-ts-repeat-value ts) (org-ts-repeat-unit ts)))
          (and (org-ts-warning ts)
               (format nil "~a~d~c" (if (eq (org-ts-warning ts) :first) "--" "-") (org-ts-warning-value ts) (org-ts-warning-unit ts)))
          (org-ts-active ts)))

(defun org-timestamp-text (time &key (active nil))
  "The Org timestamp of the universal TIME to the minute, any year: [2026-10-01
Thu 17:30], or <2026-10-01 Thu 17:30> when ACTIVE."
  (format-time (if active "<%F %a %R>" "[%F %a %R]") time))

(defun org-ts-repeat (ts now)
  "Move TS, a timestamp with a repeater, to its next time after it is done at
the universal time NOW: +1w a week on from it, ++1w a week on until it is
past NOW, .+1w a week on from NOW; one repeating every 0 stays."
  (let ((value (org-ts-repeat-value ts)) (unit (org-ts-repeat-unit ts)))
    (unless (and value (plusp value)) (return-from org-ts-repeat ts))
    (ecase (org-ts-repeater ts)
      (:cumulate (org-ts-shift ts value unit))
      (:catch-up
       ;; hours, days and weeks: most of the way in one step, the rest one at a time
       (let ((seconds (* value (case unit (#\h 3600) (#\d 86400) (#\w 604800) (t 0)))))
         (when (plusp seconds)
           (let ((steps (1- (floor (- now (org-ts-time ts)) seconds))))
             (when (plusp steps) (org-ts-shift ts (* steps value) unit)))))
       (loop do (org-ts-shift ts value unit)
             while (<= (org-ts-time ts) now)))
      (:restart (if (char= unit #\h)
                    (org-ts-set-time ts (+ now (* value 3600)))
                    (org-ts-shift (org-ts-set-day ts (org-today now)) value unit)))))
  ts)

(defun org-repeating-ts-p (ts)
  "True when TS, an ORG-TS or NIL, repeats: it has a repeater, and not of 0 (+0d)."
  (and ts (org-ts-repeater ts) (plusp (org-ts-repeat-value ts))))

(defun org-repeating-timestamp-in-p (text start end)
  "True when TEXT holds an active timestamp that repeats (ORG-REPEATING-TS-P)
from START below END."
  (cl-ppcre:do-matches (s e *org-active-timestamp-scanner* text nil :start start :end end)
    (when (org-repeating-ts-p (org-read-ts (subseq text s e)))
      (return t))))

(defun org-repeat-timestamps (text now)
  "TEXT with each active timestamp in it that has a repeater moved to its next
time (ORG-TS-REPEAT), as when their task is done at the universal time NOW;
and how many were moved, as a second value. A repeater of 0 (+0d) repeats
nothing."
  (let ((count 0))
    (values (cl-ppcre:regex-replace-all *org-active-timestamp-scanner* text
                                        (lambda (match &rest groups)
                                          (declare (ignore groups))
                                          (let ((ts (org-read-ts match)))
                                            (if (org-repeating-ts-p ts)
                                                (progn (incf count) (org-ts-text (org-ts-repeat ts now)))
                                                match)))
                                        :simple-calls t)
            count)))
