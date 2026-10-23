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
  "TEXT, one timestamp (<2026-10-05 Mon 09:00 +1w -3d>), read as an ORG-TS, or
NIL; one whose time is none (25:00, a range ending 10:00-24:30) is none."
  (multiple-value-bind (match end starts ends) (cl-ppcre:scan *org-timestamp-scanner* text)
    (declare (ignore end))
    (when match
      (flet ((int (i) (and (aref starts i) (parse-integer text :start (aref starts i) :end (aref ends i)))))
        (multiple-value-bind (at hour minute end-hour end-minute)
            (and (aref starts 4) (time-range-at text (aref starts 4) (length text)))
          (when (or at (null (aref starts 4)))
            (let ((ts (make-org-ts :active (char= (char text 0) #\<)
                                   :year (int 1) :month (int 2) :day (int 3)
                                   :hour hour :minute minute :end-hour end-hour :end-minute end-minute)))
              (cl-ppcre:do-register-groups (mark value unit) ("(\\.\\+|\\+\\+|\\+|--|-)(\\d+)([hdwmy])" text nil :start (aref starts 8) :end (aref ends 8))
                (let ((value (parse-integer value)) (unit (char unit 0)))
                  (cond ((member mark '("+" "++" ".+") :test #'string=)
                         (setf (org-ts-repeater ts) (cond ((string= mark "+") :cumulate) ((string= mark "++") :catch-up) (t :restart))
                               (org-ts-repeat-value ts) value (org-ts-repeat-unit ts) unit))
                        (t (setf (org-ts-warning ts) (if (string= mark "--") :first :all)
                                 (org-ts-warning-value ts) value (org-ts-warning-unit ts) unit)))))
              ts)))))))

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

(defun org-ts-time-text (ts)
  "The time or range of times of TS as a timestamp writes it, 09:00 or
09:00-10:00 (TIME-RANGE-TEXT); NIL when it has none."
  (time-range-text (org-ts-hour ts) (org-ts-minute ts) (org-ts-end-hour ts) (org-ts-end-minute ts)))

(defun org-ts-text (ts)
  "TS written as a note writes a timestamp: <2026-10-05 Mon 09:00-10:00 +1w -3d>."
  (format nil "~:[[~;<~]~a~@[ ~a~]~@[ ~a~]~:[]~;>~]"
          (org-ts-active ts)
          (date-time-text (org-ts-day-number ts) (org-ts-hour ts) (org-ts-minute ts) (org-ts-end-hour ts) (org-ts-end-minute ts))
          (and (org-ts-repeater ts)
               (format nil "~a~d~c" (ecase (org-ts-repeater ts) (:cumulate "+") (:catch-up "++") (:restart ".+"))
                       (org-ts-repeat-value ts) (org-ts-repeat-unit ts)))
          (and (org-ts-warning ts)
               (format nil "~a~d~c" (if (eq (org-ts-warning ts) :first) "--" "-") (org-ts-warning-value ts) (org-ts-warning-unit ts)))
          (org-ts-active ts)))

(defun org-day-timestamp-text (day &key active hour minute)
  "The day number DAY as an Org timestamp, any year: [2026-10-01 Thu], or
<2026-10-01 Thu> when ACTIVE, with HOUR and MINUTE when HOUR is given:
[2026-10-01 Thu 17:30]."
  (format nil (if active "<~a>" "[~a]") (date-time-text day hour minute)))

(defun org-timestamp-text (time &key active (clock t))
  "The Org timestamp of the universal TIME to the minute, any year: [2026-10-01
Thu 17:30], or <2026-10-01 Thu 17:30> when ACTIVE; without CLOCK, its day
alone, [2026-10-01 Thu]."
  (multiple-value-bind (day hour minute) (time-day time)
    (org-day-timestamp-text day :active active :hour (and clock hour) :minute minute)))

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

;;; Dates by rule: <%%(diary-float t 1 1) 10:00>, the first Monday of every
;;; month, as Emacs's Org writes them. The rule between the brackets is read as
;;; data, a few forms of Emacs's diary that say which days, and never run.

(defstruct (org-rule (:conc-name org-rule-))
  "A date by rule, read from a timestamp <%%(...)>: KIND, one of :FLOAT
\(the Nth or last weekday of a month), :CYCLIC (every N days), :BLOCK (every
day from one date to another), :ANNIVERSARY (a day each year), :DATE (the
days whose year, month and day are as given), :WEEKDAYS (the days of the
week listed, 0 for Sunday to 6 for Saturday), :ALL (the days all its rules
take, from one date to another say) or :ANY (the days any of them takes); ARGUMENTS, what its form says,
as ORG-READ-RULE reads them; and HOUR, MINUTE, END-HOUR and END-MINUTE of
the time written after it, or NIL."
  kind arguments hour minute end-hour end-minute)

(defparameter *org-rule-scanner* (cl-ppcre:create-scanner (format nil "^<~a>$" (org-rule-timestamp-body t)))
  "A timestamp by rule, whole (ORG-RULE-TIMESTAMP-BODY): its form and its
time or range of times, each a group.")

(defun org-read-rule-data (text start end)
  "What TEXT holds from START below END read as data, never run: numbers,
t and nil, names (as strings), and lists of them in brackets, a quote before
one passed over. Two values: the list of what it holds, and the index after
it; NIL for anything else."
  (let ((i start) (stack (list '())))
    (loop
      (loop while (and (< i end) (member (char text i) '(#\Space #\Tab))) do (incf i))
      (when (>= i end)
        (return (and (null (rest stack)) (values (reverse (first stack)) i))))
      (let ((c (char text i)))
        (cond ((char= c #\') (incf i))
              ((char= c #\()
               (when (> (length stack) 3) (return nil))
               (push '() stack) (incf i))
              ((char= c #\))
               (when (null (rest stack)) (return nil))
               (let ((list (reverse (pop stack)))) (push list (first stack)))
               (incf i))
              (t (let* ((stop (or (position-if (lambda (ch) (member ch '(#\Space #\Tab #\( #\) #\'))) text :start i :end end) end))
                        (word (subseq text i stop))
                        (value (cond ((cl-ppcre:scan "^-?[0-9]{1,6}$" word) (parse-integer word))
                                     ((string= word "t") t)
                                     ((string= word "nil") nil)
                                     ((cl-ppcre:scan "^[a-z][a-z-]*$" word) word)
                                     (t (return nil)))))
                   (push value (first stack))
                   (setf i stop))))))))

(defun org-rule-date (a b c)
  "A date of a rule's form, its parts A B C read as year, month, day when A
is a year (more than 31), else as month, day, year, the order the diary forms
take by default. Three values: year, month and day, each a number, T for
any, or a list of numbers."
  (if (and (integerp a) (> a 31)) (values a b c) (values c a b)))

(defun org-rule-part-p (part)
  "True when PART of a rule's form is T, a number or a list of numbers."
  (or (eq part t) (integerp part) (and (consp part) (every #'integerp part))))

(defun org-rule-from-data (data)
  "DATA, a rule's form as ORG-READ-RULE-DATA reads it, as an ORG-RULE with no
time, or NIL when it is not one of those known: diary-float, diary-cyclic,
diary-block, diary-anniversary and diary-date, with numbers, t and lists of
numbers as their arguments; (memq (calendar-day-of-week date) '(1 2 3 4 5)),
the days of the week listed; (and FORM ...), the days all its forms take,
and (or FORM ...), the days any of them takes, of these."
  (let ((name (first data)) (args (rest data)))
    (flet ((date (a b c)
             (multiple-value-bind (y m d) (org-rule-date a b c)
               (and (integerp y) (integerp m) (integerp d) (valid-date-p y m d) (day-number y m d)))))
      (cond ((not (stringp name)) nil)
            ((string= name "diary-float")
             (destructuring-bind (&optional month dayname n day &rest more) args
               (and (null more) (org-rule-part-p month) (org-rule-part-p dayname)
                    (integerp n) (<= 1 (abs n) 5) (or (null day) (and (integerp day) (<= 1 day 31)))
                    (every (lambda (d) (<= 0 d 6)) (if (integerp dayname) (list dayname) (if (consp dayname) dayname '())))
                    (make-org-rule :kind :float :arguments (list month dayname n day)))))
            ((string= name "diary-cyclic")
             (destructuring-bind (&optional n a b c &rest more) args
               (let ((start (and (integerp n) (plusp n) (null more) (date a b c))))
                 (and start (make-org-rule :kind :cyclic :arguments (list n start))))))
            ((string= name "diary-block")
             (destructuring-bind (&optional a b c d e f &rest more) args
               (let ((from (and (null more) (date a b c))) (to (date d e f)))
                 (and from to (<= from to) (make-org-rule :kind :block :arguments (list from to))))))
            ((string= name "diary-anniversary")
             (destructuring-bind (&optional a b c &rest more) args
               (when (and (null more) (integerp a) (integerp b) (or (null c) (integerp c)))
                 (multiple-value-bind (y m d) (if c (org-rule-date a b c) (values nil a b))
                   (and (integerp m) (integerp d) (<= 1 m 12) (<= 1 d (month-length 2000 m))
                        (make-org-rule :kind :anniversary :arguments (list m d y)))))))
            ((string= name "memq")
             (destructuring-bind (&optional of days &rest more) args
               (and (null more) (equal of '("calendar-day-of-week" "date"))
                    (consp days) (every (lambda (d) (and (integerp d) (<= 0 d 6))) days)
                    (make-org-rule :kind :weekdays :arguments (list (remove-duplicates days))))))
            ((string= name "diary-date")
             (destructuring-bind (&optional a b c &rest more) args
               (when (and (null more) (org-rule-part-p a) (org-rule-part-p b) (org-rule-part-p c))
                 (multiple-value-bind (y m d) (org-rule-date a b c)
                   (make-org-rule :kind :date :arguments (list y m d))))))
            ((member name '("and" "or") :test #'string=)
             (let ((parts (mapcar (lambda (part) (and (consp part) (org-rule-from-data part))) args)))
               (and parts (every #'identity parts)
                    (make-org-rule :kind (if (string= name "and") :all :any) :arguments (list parts)))))))))

(defun org-read-rule (text)
  "TEXT, a timestamp by rule (<%%(diary-float t 1 1) 10:00>), read as an
ORG-RULE, or NIL when it is none or its form is not one of those
ORG-RULE-FROM-DATA knows."
  (multiple-value-bind (match groups)
      (and (> (length text) 4) (string= "<%%(" text :end2 4) (cl-ppcre:scan-to-strings *org-rule-scanner* text))
    (when match
      (let* ((form (aref groups 0))
             (rule (org-rule-from-data (org-read-rule-data form 0 (length form)))))
        (when rule
          (if (null (aref groups 1))
              rule
              (multiple-value-bind (hour minute end-hour end-minute) (read-time-range (aref groups 1))
                (when hour
                  (setf (org-rule-hour rule) hour (org-rule-minute rule) minute
                        (org-rule-end-hour rule) end-hour (org-rule-end-minute rule) end-minute)
                  rule))))))))

(defun org-rule-part-has-p (part value)
  "True when PART of a rule (T, a number or a list of numbers) takes VALUE."
  (or (eq part t) (eql part value) (and (consp part) (member value part))))

(defun org-float-day (year month dayname n day)
  "The day number of the Nth DAYNAME (0 Sunday to 6 Saturday) of MONTH of
YEAR, counted from its DAY (the first when NIL), or with N below 0 the -Nth
last, counted back from its DAY (its last day when NIL); NIL when that day is
not in MONTH and no DAY was given."
  (let* ((length (month-length year month))
         (occurrence (if (plusp n)
                         (let ((from (day-number year month (or day 1))))
                           (+ from (mod (- dayname (weekday from)) 7) (* 7 (1- n))))
                         (let ((to (day-number year month (or day length))))
                           (- to (mod (- (weekday to) dayname) 7) (* 7 (1- (- n))))))))
    (if (or day (multiple-value-bind (y m) (day-date occurrence) (and (= y year) (= m month))))
        occurrence
        nil)))

(defun org-rule-matches-p (rule day)
  "True when RULE (an ORG-RULE) takes the day number DAY."
  (destructuring-bind (a &optional b c d) (org-rule-arguments rule)
    (ecase (org-rule-kind rule)
      (:float
       (let ((dayname b) (n c) (from d))
         (and (org-rule-part-has-p dayname (weekday day))
              (multiple-value-bind (year month) (day-date day)
                ;; counted from a day of the month, the Nth may fall in the month before or after
                (loop for shift in (if from '(0 -1 1) '(0))
                      thereis (multiple-value-bind (y m) (floor (+ (* 12 year) (1- month) shift) 12)
                                (let ((m (1+ m)))
                                  (and (org-rule-part-has-p a m)
                                       (eql day (org-float-day y m (weekday day) n from))))))))))
      (:cyclic (and (>= day b) (zerop (mod (- day b) a))))
      (:weekdays (and (member (weekday day) a) t))
      (:all (every (lambda (part) (org-rule-matches-p part day)) a))
      (:any (some (lambda (part) (org-rule-matches-p part day)) a))
      (:block (<= a day b))
      (:anniversary (multiple-value-bind (y m dd) (day-date day)
                      (and (= m a) (= dd b) (or (null c) (>= y c)))))
      (:date (multiple-value-bind (y m dd) (day-date day)
               (and (org-rule-part-has-p a y) (org-rule-part-has-p b m) (org-rule-part-has-p c dd)))))))

(defparameter *org-rule-reach* (* 8 366)
  "The most days looked through for a rule's next or last day: a day each
eight years, 29 February in a year that leaves one out, is still found.")

(defun org-rule-span (rule)
  "The first and last day RULE can take, as two values, each NIL for none:
those of a range of days in it, of the years it lists or the year an
anniversary begins, every other day open."
  (destructuring-bind (&optional a b c &rest more) (org-rule-arguments rule)
    (declare (ignore more))
    (case (org-rule-kind rule)
      (:block (values a b))
      (:anniversary (values (and c (day-number c 1 1)) nil))
      (:date (let ((years (cond ((integerp a) (list a)) ((consp a) a))))
               (if years
                   (values (day-number (reduce #'min years) 1 1) (day-number (reduce #'max years) 12 31))
                   (values nil nil))))
      (:all (let ((first nil) (last nil))
              (dolist (part a (values first last))
                (multiple-value-bind (from to) (org-rule-span part)
                  (when from (setf first (if first (max first from) from)))
                  (when to (setf last (if last (min last to) to)))))))
      (t (values nil nil)))))

(defun org-rule-step (rule)
  "How the days RULE can take are looked through: :YEAR when a year holds one
at most (a day of a month), :MONTH when a month does (a day of every month,
the Nth weekday of a month), NIL day by day."
  (destructuring-bind (&optional a b c d) (org-rule-arguments rule)
    (declare (ignore a))
    (case (org-rule-kind rule)
      (:anniversary :year)
      (:date (cond ((and (integerp b) (integerp c)) :year) ((integerp c) :month)))
      (:float (and (integerp b) (null d) :month)))))

(defun org-rule-candidate (rule year month)
  "The one day RULE can take in YEAR, or in MONTH of YEAR when it is looked
through by month (ORG-RULE-STEP), or NIL."
  (destructuring-bind (a &optional b c d) (org-rule-arguments rule)
    (declare (ignore d))
    (ecase (org-rule-kind rule)
      (:anniversary (and (valid-date-p year a b) (day-number year a b)))
      (:date (let ((month (if (integerp b) b month)))
               (and (valid-date-p year month c) (day-number year month c))))
      (:float (org-float-day year month b c nil)))))

(defun org-rule-search (rule from to)
  "The first day RULE takes from the day FROM to the day TO, looking back
when TO is before FROM, or NIL: by year or by month where it can
\(ORG-RULE-STEP), else day by day."
  (let ((by (if (<= from to) 1 -1)))
    (flet ((taken (day)
             (and day (if (= by 1) (<= from day to) (<= to day from)) (org-rule-matches-p rule day) day))
           (index (day step)
             (multiple-value-bind (year month) (day-date day)
               (if (eq step :year) year (+ (* 12 year) (1- month))))))
      (let ((step (org-rule-step rule)))
        (if (null step)
            (loop for k = from then (+ k by)
                  until (if (= by 1) (> k to) (< k to))
                  when (org-rule-matches-p rule k) return k)
            (loop for i = (index from step) then (+ i by)
                  until (if (= by 1) (> i (index to step)) (< i (index to step)))
                  thereis (taken (if (eq step :year)
                                     (org-rule-candidate rule i nil)
                                     (org-rule-candidate rule (floor i 12) (1+ (mod i 12)))))))))))

(defun org-rule-next (rule day)
  "The first day number at or after DAY that RULE takes, or NIL when none
does within *ORG-RULE-REACH* days or its range of days (ORG-RULE-SPAN)."
  (case (org-rule-kind rule)
    (:cyclic (destructuring-bind (n start) (org-rule-arguments rule)
               (if (<= day start) start (+ start (* n (ceiling (- day start) n))))))
    (:block (destructuring-bind (from to) (org-rule-arguments rule)
              (cond ((<= day from) from) ((<= day to) day))))
    (t (multiple-value-bind (first last) (org-rule-span rule)
         (let ((from (max day (or first day)))
               (to (min (+ day *org-rule-reach*) (or last (+ day *org-rule-reach*)))))
           (and (<= from to) (org-rule-search rule from to)))))))

(defun org-rule-previous (rule day)
  "The last day number at or before DAY that RULE takes, or NIL when none
does within *ORG-RULE-REACH* days or its range of days (ORG-RULE-SPAN)."
  (case (org-rule-kind rule)
    (:cyclic (destructuring-bind (n start) (org-rule-arguments rule)
               (and (>= day start) (- day (mod (- day start) n)))))
    (:block (destructuring-bind (from to) (org-rule-arguments rule)
              (cond ((>= day to) to) ((>= day from) day))))
    (t (multiple-value-bind (first last) (org-rule-span rule)
         (let ((from (min day (or last day)))
               (to (max (- day *org-rule-reach*) (or first (- day *org-rule-reach*)))))
           (and (>= from to) (org-rule-search rule from to)))))))

(defun org-rule-time-text (rule)
  "The time RULE says, as a timestamp writes it (10:00, 10:00-11:00), or NIL."
  (time-range-text (org-rule-hour rule) (org-rule-minute rule) (org-rule-end-hour rule) (org-rule-end-minute rule)))

(defun org-rule-done-day (rule last-done today)
  "The day RULE's task is done for when it is marked done today, TODAY:
the first of its days after LAST-DONE (a day number, or NIL when it has
never been done), or the last up to today when that is later, missed days
passed over; never done before, the one of its days nearest today."
  (let ((before (org-rule-previous rule today))
        (after (org-rule-next rule (if last-done (1+ last-done) today))))
    (cond (last-done (if (and before after) (max before after) (or after before)))
          ((and before after) (if (<= (- today before) (- after today)) before after))
          (t (or before after)))))

(defun org-rule-timestamp-in (text start end)
  "The first timestamp by rule (ORG-READ-RULE) in TEXT from START below END:
its ORG-RULE, or NIL; its text as a second value."
  (cl-ppcre:do-matches (s e *org-rule-timestamp-scanner* text nil :start start :end end)
    (let ((rule (org-read-rule (subseq text s e))))
      (when rule (return (values rule (subseq text s e)))))))

(defun org-rule-done-text (rule last-repeat now)
  "What LAST_REPEAT says once a task by RULE is done at the universal time
NOW, LAST-REPEAT being what its property says now (when it was last done for,
or NIL): the day it is done for (ORG-RULE-DONE-DAY) as an inactive
timestamp, with the time RULE gives; NIL when RULE takes no day near."
  (let* ((last (let ((ts (and last-repeat (org-read-ts last-repeat)))) (and ts (org-ts-day-number ts))))
         (day (org-rule-done-day rule last (org-today now))))
    (and day (org-day-timestamp-text day :hour (org-rule-hour rule) :minute (org-rule-minute rule)))))
