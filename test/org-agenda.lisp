;;;; test/org-agenda.lisp --- Org's clock, scheduling, tags and agenda.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun org-masked-times (text)
  "TEXT with its timestamps written [T] and its clock durations D, to compare clock lines made at any time."
  (cl-ppcre:regex-replace-all "=> +\\d+:\\d\\d" (cl-ppcre:regex-replace-all "\\[\\d{4}-\\d\\d-\\d\\d [A-Za-z]+ \\d\\d:\\d\\d\\]" text "[T]") "=> D"))

(define-test (org clock)
  (flet ((clocked (keys text)
           (prog1 (org-masked-times (org-text-after keys text))
             (setf tuatara.defaults::*org-clock* nil))))
    ;; into a :LOGBOOK: drawer made after the headline, its planning line and properties, as Org puts it
    (is string= (format nil "* TODO Task~%:LOGBOOK:~%CLOCK: [T]--[T] => D~%:END:~%Body~%* Next~%")
        (clocked "C-c C-x C-i C-c C-x C-o" (format nil "* TODO Task~%Body~%* Next~%")))
    (is string= (format nil "* TODO Task~%SCHEDULED: <2026-10-02 Fri>~%:PROPERTIES:~%:ID: x~%:END:~%:LOGBOOK:~%CLOCK: [T]--[T] => D~%:END:~%Body~%")
        (clocked "C-c C-x C-i C-c C-x C-o" (format nil "* TODO Task~%SCHEDULED: <2026-10-02 Fri>~%:PROPERTIES:~%:ID: x~%:END:~%Body~%")))
    ;; into the drawer there, the newest first; from the body too
    (is string= (format nil "* Task~%:LOGBOOK:~%CLOCK: [T]--[T] => D~%CLOCK: [T]--[T] => D~%- Note~%:END:~%Body~%")
        (clocked "j j j C-c C-x C-i C-c C-x C-o"
                 (format nil "* Task~%:LOGBOOK:~%CLOCK: [2026-09-30 Wed 10:00]--[2026-09-30 Wed 11:15] =>  1:15~%- Note~%:END:~%Body~%")))
    ;; running: an open line; cancelled: gone, with the drawer it made
    (is string= (format nil "* Task~%:LOGBOOK:~%CLOCK: [T]~%:END:~%Body~%") (clocked "C-c C-x C-i" (format nil "* Task~%Body~%")))
    (is string= (format nil "* Task~%Body~%") (clocked "C-c C-x C-i C-c C-x C-q" (format nil "* Task~%Body~%")))
    ;; a headline at the end of the text, no newline after it
    (is string= (format nil "* Task~%:LOGBOOK:~%CLOCK: [T]~%:END:~%") (clocked "C-c C-x C-i" "* Task"))
    ;; clocking in elsewhere clocks out first
    (is string= (format nil "* A~%:LOGBOOK:~%CLOCK: [T]--[T] => D~%:END:~%* B~%:LOGBOOK:~%CLOCK: [T]~%:END:~%")
        (clocked "C-c C-x C-i g e C-c C-x C-i" (format nil "* A~%* B~%"))))
  ;; C-c C-c on a clock line writes its duration again, as Org does
  (is string= (format nil "* Task~%CLOCK: [2026-10-01 Thu 17:30]--[2026-10-01 Thu 19:00] =>  1:30~%")
      (org-text-after "j C-c C-c" (format nil "* Task~%CLOCK: [2026-10-01 Thu 17:30]--[2026-10-01 Thu 19:00] =>  0:12~%")))
  (is string= (format nil "* T~%CLOCK: [2026-10-01 Thu 17:30]--[2026-10-03 Sat 09:05] => 39:35~%")
      (org-text-after "j C-c C-c" (format nil "* T~%CLOCK: [2026-10-01 Thu 17:30]--[2026-10-03 Sat 09:05]~%")))
  ;; before the first headline: said
  (true (tuatara.frontend.fake:fake-frame-showing
         (run-keys "M-x o r g - m o d e RET C-c C-x C-i" :text (format nil "Text~%") :width 80 :height 8)
         "Not under a headline"))
  (setf tuatara.defaults::*org-clock* nil)
  ;; while it runs, the mode line shows it; SPC a O clocks out from anywhere
  (let ((f (run-keys "M-x o r g - m o d e RET C-c C-x C-i C-x b o t h e r C-j SPC a O" :text (format nil "* TODO Write the report~%") :width 80 :height 8)))
    (true (tuatara.frontend.fake:fake-frame-showing f "[0:00] (Write the report)"))
    (true (tuatara.frontend.fake:fake-frame-showing f "Clock stopped")))
  (is eq nil tuatara.defaults::*org-clock*)
  ;; quitting with a clock running asks first
  (let ((f (run-keys "M-x o r g - m o d e RET C-c C-x C-i C-x C-c" :text (format nil "* Task~%") :width 100 :height 8)))
    (true (tuatara.frontend.fake:fake-frame-showing f "clock out before quitting?")))
  (setf tuatara.defaults::*org-clock* nil))

(define-test (org repeating-tasks)
  ;; C-c C-t on a task done: as Org writes it, every case of test/org/repeats.sexp
  (let ((cases (with-open-file (in (asdf:system-relative-pathname :tuatara "test/org/repeats.sexp") :external-format :utf-8)
                 (let ((*read-eval* nil)) (loop for form = (read in nil in) until (eq form in) collect form))))
        (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        (tuatara.defaults::*org-log-done* :time)
        (tuatara.defaults::*org-log-into-drawer* t)
        (tuatara.defaults::*org-log-repeat* :time))
    (dolist (case cases)
      (destructuring-bind (name line text expected) case
        (is string= expected (org-text-after (format nil "~{~a ~}C-c C-t" (make-list line :initial-element "j")) text) "~a" name)))
    ;; one u undoes it
    (is string= (format nil "* TODO W~%SCHEDULED: <2026-09-28 Mon +1w>~%")
        (org-text-after "C-c C-t u" (format nil "* TODO W~%SCHEDULED: <2026-09-28 Mon +1w>~%"))))
  ;; Org's defaults: nothing noted when a task is done, the note of a repeat right under the planning line
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026)))
    (is string= (format nil "* DONE Once~%Body~%") (org-text-after "C-c C-t" (format nil "* TODO Once~%Body~%")))
    ;; a repeater of 0 repeats nothing (Org's org-auto-repeat-maybe): done, and never a loop
    (is string= (format nil "* DONE Z~%SCHEDULED: <2026-09-28 Mon ++0d>~%")
        (org-text-after "C-c C-t" (format nil "* TODO Z~%SCHEDULED: <2026-09-28 Mon ++0d>~%")))
    (is string= (format nil "* TODO W~%SCHEDULED: <2026-10-05 Mon +1w>~%:PROPERTIES:~%:LAST_REPEAT: [2026-10-01 Thu 18:00]~%:END:~%- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]~%Body~%")
        (org-text-after "C-c C-t" (format nil "* TODO W~%SCHEDULED: <2026-09-28 Mon +1w>~%Body~%")))))

(define-test (org scheduling)
  ;; dates read as Org's org-read-date reads them, on Thursday 2026-10-01 at 18:00 (Emacs's answers)
  (let ((now (encode-universal-time 0 0 18 1 10 2026)))
    (flet ((read-date (input) (let ((ts (tuatara.defaults::org-read-date-text input now)))
                                (and ts (tuatara.defaults::org-ts-text ts)))))
      (loop for (input expected) in '(("" "<2026-10-01 Thu>") ("." "<2026-10-01 Thu>") ("fri" "<2026-10-02 Fri>")
                                      ("mon" "<2026-10-05 Mon>") ("+3d" "<2026-10-04 Sun>") ("+2w" "<2026-10-15 Thu>")
                                      ("-1d" "<2026-09-30 Wed>") ("5" "<2026-10-05 Mon>") ("10-15" "<2026-10-15 Thu>")
                                      ("2026-12-24" "<2026-12-24 Thu>") ("14:00" "<2026-10-01 Thu 14:00>")
                                      ("fri 9am" "<2026-10-02 Fri 09:00>") ("3pm" "<2026-10-01 Thu 15:00>")
                                      ("+1m" "<2026-11-01 Sun>") ("2026-10-05 10:00-11:30" "<2026-10-05 Mon 10:00-11:30>")
                                      ("thu" "<2026-10-08 Thu>") ("sep 30" "<2027-09-30 Thu>") ("2027" "<2027-10-01 Fri>"))
            do (is equal expected (read-date input) "~s" input))
      (false (read-date "someday"))
      (false (read-date "2026-02-30"))))
  ;; C-c C-s and C-c C-d as Org writes them, with your Emacs's logging of changed dates
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        (tuatara.defaults::*org-log-into-drawer* t)
        (tuatara.defaults::*org-log-reschedule* :time)
        (tuatara.defaults::*org-log-redeadline* :time))
    (flet ((date-keys (date) (format nil "~{~a~^ ~}" (map 'list (lambda (c) (if (char= c #\Space) "SPC" (string c))) date))))
      (loop for (keys text expected)
              in `((,(format nil "C-c C-s ~a RET" (date-keys "2026-10-05")) "* TODO Task~%Body~%" "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%Body~%")
                   (,(format nil "C-c C-s ~a RET" (date-keys "2026-10-05 14:00")) "* TODO Task~%" "* TODO Task~%SCHEDULED: <2026-10-05 Mon 14:00>~%")
                   (,(format nil "C-c C-s ~a RET" (date-keys "2026-10-05")) "* TODO Task~%DEADLINE: <2026-10-09 Fri>~%" "* TODO Task~%SCHEDULED: <2026-10-05 Mon> DEADLINE: <2026-10-09 Fri>~%")
                   (,(format nil "C-c C-d ~a RET" (date-keys "2026-10-09")) "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%" "* TODO Task~%DEADLINE: <2026-10-09 Fri> SCHEDULED: <2026-10-05 Mon>~%")
                   (,(format nil "C-c C-s ~a RET" (date-keys "2026-10-05")) "* DONE Task~%CLOSED: [2026-09-30 Wed 10:00]~%" "* DONE Task~%SCHEDULED: <2026-10-05 Mon>~%")
                   (,(format nil "C-c C-s ~a RET" (date-keys "2026-10-07")) "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%"
                    "* TODO Task~%SCHEDULED: <2026-10-07 Wed>~%:LOGBOOK:~%- Rescheduled from \"[2026-10-05 Mon]\" on [2026-10-01 Thu 18:00]~%:END:~%")
                   (,(format nil "C-c C-s ~a RET" (date-keys "2026-10-07")) "* TODO Task~%SCHEDULED: <2026-10-05 Mon +1w>~%"
                    "* TODO Task~%SCHEDULED: <2026-10-07 Wed +1w>~%:LOGBOOK:~%- Rescheduled from \"[2026-10-05 Mon +1w]\" on [2026-10-01 Thu 18:00]~%:END:~%")
                   (,(format nil "C-c C-d ~a RET" (date-keys "2026-10-12")) "* TODO Task~%DEADLINE: <2026-10-09 Fri -3d>~%"
                    "* TODO Task~%DEADLINE: <2026-10-12 Mon -3d>~%:LOGBOOK:~%- New deadline from \"[2026-10-09 Fri -3d]\" on [2026-10-01 Thu 18:00]~%:END:~%")
                   ("C-u C-c C-s" "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%Body~%"
                    "* TODO Task~%:LOGBOOK:~%- Not scheduled, was \"[2026-10-05 Mon]\" on [2026-10-01 Thu 18:00]~%:END:~%Body~%")
                   ("C-u C-c C-d" "* TODO Task~%DEADLINE: <2026-10-09 Fri> SCHEDULED: <2026-10-05 Mon>~%"
                    "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%:LOGBOOK:~%- Removed deadline, was \"[2026-10-09 Fri]\" on [2026-10-01 Thu 18:00]~%:END:~%")
                   (,(format nil "C-c C-s ~a RET" (date-keys "2026-10-05")) "* TODO Task~%:PROPERTIES:~%:ID: x~%:END:~%" "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%:PROPERTIES:~%:ID: x~%:END:~%")
                   ;; C-c . and C-c ! put a timestamp in at point, or change the one point is on
                   (,(format nil "j C-c . ~a RET" (date-keys "2026-10-05")) "* Task~%~%" "* Task~%<2026-10-05 Mon>~%")
                   (,(format nil "j C-c ! ~a RET" (date-keys "2026-10-05 14:00")) "* Task~%~%" "* Task~%[2026-10-05 Mon 14:00]~%")
                   (,(format nil "j C-c . ~a RET" (date-keys "2026-10-07")) "* Task~%<2026-10-05 Mon +1w>~%" "* Task~%<2026-10-07 Wed +1w>~%")
                   (,(format nil "g l C-c . ~a RET" (date-keys "2026-10-07")) "* Meet <2026-10-05 Mon>~%" "* Meet <2026-10-07 Wed>~%")
                   ;; on a planning line's date: that date changed, no second put in
                   (,(format nil "j g l C-c . ~a RET" (date-keys "2026-10-07")) "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%"
                    "* TODO Task~%SCHEDULED: <2026-10-07 Wed>~%")
                   ;; its time kept when none is typed
                   (,(format nil "j g l C-c . ~a RET" (date-keys "2026-10-07")) "* Task~%<2026-10-05 Mon 09:00>~%" "* Task~%<2026-10-07 Wed 09:00>~%")
                   ;; one date of a range alone
                   (,(format nil "j C-c . ~a RET" (date-keys "2026-10-01")) "* Task~%<2026-10-05 Mon>--<2026-10-06 Tue>~%"
                    "* Task~%<2026-10-01 Thu>--<2026-10-06 Tue>~%"))
            do (is string= (format nil expected) (org-text-after keys (format nil text)) "~a on ~s" keys text))
      ;; what is typed read as it is typed; fri on Thursday is tomorrow
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "M-x o r g - m o d e RET C-c C-s f r i" :text (format nil "* Task~%") :width 80 :height 10)
             "<2026-10-02 Fri>"))
      ;; one u undoes it
      (is string= (format nil "* TODO Task~%") (org-text-after (format nil "C-c C-s ~a RET u" (date-keys "2026-10-05")) (format nil "* TODO Task~%"))))))

(defun org-agenda-test-rows (items)
  "ITEMS as the rows of test/org/agenda/agenda.expected: (DAY TYPE EXTRA TIME TODO TITLE TAGS CATEGORY)."
  (mapcar (lambda (item)
            (let ((entry (tuatara.defaults::org-item-entry item))
                  (type (tuatara.defaults::org-item-type item)))
              (list (and (tuatara.defaults::org-item-day item) (tuatara.dates:format-day "%F" (tuatara.defaults::org-item-day item)))
                    type
                    (string-trim " " (tuatara.defaults::org-item-extra item))
                    (tuatara.defaults::org-item-time item)
                    (and (not (member type '("closed" "clock") :test #'string=)) (tuatara.defaults::agenda-entry-todo entry))
                    (tuatara.defaults::org-item-title item)
                    (tuatara.defaults::agenda-entry-tags entry)
                    (tuatara.defaults::agenda-entry-category entry))))
          items))

(defun org-agenda-expected-views ()
  "The views of test/org/agenda/views.expected, what Org 9.7.11 shows in its agenda buffer for the test
files on 2026-10-01, 80 columns wide: an alist of (NAME . LINES), the time now and the TODO list's
help left out."
  (with-open-file (in (asdf:system-relative-pathname :tuatara "test/org/agenda/views.expected") :external-format :utf-8)
    (let ((views '()) (name nil) (lines '()))
      (loop for line = (read-line in nil) while line
            do (cond ((string= line "=== end") (push (cons name (nreverse lines)) views) (setf name nil lines '()))
                     ((uiop:string-prefix-p "=== " line) (setf name (subseq line 4)))
                     (name (push line lines))))
      (nreverse views))))

(define-test (org agenda-views)
  ;; the agenda's buffer, line for line as Org's, at 80 columns: the days, the TODO list, a match, a view of one's own
  (let* ((directory (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
         (tuatara.defaults::*org-agenda-files* (list (namestring (merge-pathnames "tasks.org" directory))
                                                     (namestring (merge-pathnames "notes.org" directory))))
         (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
         (tuatara.defaults::*org-agenda-time-grid* '((:daily :today :require-timed) (800 1000 1200 1400 1600 1800 2000)
                                                     "......" "----------------"))
         (tuatara.defaults::*org-agenda-current-time-string* nil)
         (monday (tuatara.dates:day-number 2026 9 28))
         (expected (org-agenda-expected-views)))
    (flet ((check (name view)
             (let* ((wanted (rest (assoc name expected :test #'string=)))
                    (lines (loop for (pieces) in (tuatara.defaults::org-agenda-view-lines view 80)
                                 for text = (string-right-trim " " (tuatara.defaults::org-agenda-pieces-text pieces))
                                 unless (or (uiop:string-prefix-p "Press " text) (cl-ppcre:scan "^ +\\(\\d+\\)" text))
                                   collect text)))
               (true wanted "~a is in views.expected" name)
               (is = (length wanted) (length lines) "~a: ~d lines, Org shows ~d" name (length lines) (length wanted))
               (loop for want in wanted for got in lines for i from 1
                     do (is string= want got "~a, line ~d" name i)))))
      (check "week-log" (list :kind :agenda :span :week :start monday :log t))
      (check "week" (list :kind :agenda :span :week :start monday :log nil))
      (check "day" (list :kind :agenda :span :day :start (+ monday 3)))
      (check "month" (list :kind :agenda :span :month :start (+ monday 3)))
      (check "todo" (list :kind :todo))
      (check "match work" (list :kind :match :match "work"))
      (check "untagged" (list :kind :match :match "-{.*}" :todo-only t :header "Untagged tasks")))))

(define-test (org timestamps-read-and-found-alike)
  ;; every way of finding a timestamp finds what ORG-READ-TS reads, whole, and nothing it does not
  (let ((stamps '("<2026-10-05 Mon>" "[2026-10-05 Mon]" "<2026-10-05>" "<2026-10-05 Mon 09:00>"
                  "<2026-10-05 Mon 9:05-10:30>" "<2026-10-05 Mon +1w>" "<2026-10-05 Mon .+1d/3d>"
                  "<2026-10-05 Mon ++2m -3d>" "<2026-10-05 Mon 09:00 +1w --2d>" "<2026-10-05 Mon -3d +1w>"
                  "<2026-10-05 lun.>" "<2026-10-05 Mon >"))
        (others '("<2026-1-05>" "<2026-10-05 Mon 25>" "<2026-10-05 Mon +1>" "<2026-10-05 Mon 9:00 junk>")))
    (dolist (stamp stamps)
      (let ((line (format nil "Meet ~a then" stamp)))
        (true (tuatara.defaults::org-read-ts stamp) "~a read" stamp)
        (is equal (list stamp) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-timestamp-re* line) "~a found" stamp)
        (is equal (list stamp) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-one-timestamp-scanner* line))
        (is equal (list stamp) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-timestamp-or-range-scanner* line))
        (is equal (and (char= (char stamp 0) #\<) (list stamp))
            (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-active-timestamp-scanner* line))))
    (dolist (other others)
      (false (tuatara.defaults::org-read-ts other) "~a not read" other)
      (false (cl-ppcre:scan tuatara.defaults::*org-one-timestamp-scanner* other) "~a not found" other)))
  ;; timestamps by rule likewise: found whole by each pattern that finds them, and read
  (dolist (stamp '("<%%(diary-float t 1 1)>" "<%%(diary-float t 1 1) 10:00>" "<%%(diary-cyclic 2 2026 10 1) 9:05-10:30>"
                   "<%%(diary-cyclic 2 2026 10 1) 22:00-24:00>" "<%%(memq (calendar-day-of-week date) '(1 2 3 4 5))>"
                   "<%%(and (diary-block 2026 11 1 2026 11 10) (memq (calendar-day-of-week date) '(1)))>"
                   "<%%(or (diary-cyclic 14 2026 11 2) (diary-cyclic 14 2026 11 4)) 18:00>"))
    (let ((line (format nil "Meet ~a then" stamp)))
      (true (tuatara.defaults::org-read-rule stamp) "~a read" stamp)
      (is equal (list stamp) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-timestamp-re* line) "~a found" stamp)
      (is equal (list stamp) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-rule-timestamp-scanner* line))
      (is equal (list stamp) (cl-ppcre:register-groups-bind (k text) (tuatara.defaults::*org-planning-item-scanner* (format nil "SCHEDULED: ~a" stamp))
                               (declare (ignore k)) (list text)))))
  (dolist (other '("<%%(diary-float t 1 1) 25:00>" "<%%(diary-float t 1 1) 10:00-25:00>" "<%%(diary-float t 1 1) 9:60>"))
    (false (tuatara.defaults::org-read-rule other) "~a not read" other)
    (false (cl-ppcre:scan tuatara.defaults::*org-timestamp-re* other) "~a not found" other)
    (false (cl-ppcre:scan tuatara.defaults::*org-rule-timestamp-scanner* other) "~a not found" other))
  ;; what is read of one
  (let ((ts (tuatara.defaults::org-read-ts "<2026-10-05 Mon 09:00-10:30 .+1d -3d>")))
    (is equal '(2026 10 5 9 0 10 30 :restart 1 #\d :all 3 #\d)
        (list (tuatara.defaults::org-ts-year ts) (tuatara.defaults::org-ts-month ts) (tuatara.defaults::org-ts-day ts)
              (tuatara.defaults::org-ts-hour ts) (tuatara.defaults::org-ts-minute ts)
              (tuatara.defaults::org-ts-end-hour ts) (tuatara.defaults::org-ts-end-minute ts)
              (tuatara.defaults::org-ts-repeater ts) (tuatara.defaults::org-ts-repeat-value ts) (tuatara.defaults::org-ts-repeat-unit ts)
              (tuatara.defaults::org-ts-warning ts) (tuatara.defaults::org-ts-warning-value ts) (tuatara.defaults::org-ts-warning-unit ts))))
  ;; a range is found whole, and each of its dates alone
  (let ((range "<2026-10-05 Mon>--<2026-10-07 Wed>"))
    (is equal (list range) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-timestamp-re* range))
    (is equal (list range) (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-active-range-scanner* range))
    (is equal '("<2026-10-05 Mon>" "<2026-10-07 Wed>") (cl-ppcre:all-matches-as-strings tuatara.defaults::*org-one-timestamp-scanner* range)))
  ;; a repeater of 0 repeats nothing; an inactive timestamp never repeats
  (true (tuatara.defaults::org-repeating-timestamp-in-p "SCHEDULED: <2026-10-05 Mon +1w>" 0 31))
  (false (tuatara.defaults::org-repeating-timestamp-in-p "SCHEDULED: <2026-10-05 Mon +0d>" 0 31))
  (false (tuatara.defaults::org-repeating-timestamp-in-p "[2026-10-05 Mon +1w]" 0 20))
  ;; a clock line: running, stopped without how long yet, and stopped with it
  (is equal '("[2026-10-01 Thu 17:30]" nil nil)
      (multiple-value-list (tuatara.defaults::org-read-clock-line "CLOCK: [2026-10-01 Thu 17:30]")))
  (is equal '("[2026-10-01 Thu 17:30]" "[2026-10-03 Sat 09:05]" nil)
      (multiple-value-list (tuatara.defaults::org-read-clock-line "CLOCK: [2026-10-01 Thu 17:30]--[2026-10-03 Sat 09:05]")))
  (is equal '("[2026-10-01 Thu 17:30]" "[2026-10-03 Sat 09:05]" "39:35")
      (multiple-value-list (tuatara.defaults::org-read-clock-line "CLOCK: [2026-10-01 Thu 17:30]--[2026-10-03 Sat 09:05] =>  39:35")))
  ;; the agenda's notes: when an entry became DONE, and a CLOSED: line
  (is string= "[2026-10-05 Mon 09:00]"
      (aref (nth-value 1 (cl-ppcre:scan-to-strings tuatara.defaults::*org-done-note-scanner*
                                                   "- State \"DONE\"       from \"TODO\"       [2026-10-05 Mon 09:00]"))
            0))
  (is string= "[2026-10-05 Mon 09:00]"
      (aref (nth-value 1 (cl-ppcre:scan-to-strings tuatara.defaults::*org-closed-or-clock-scanner* "CLOSED: [2026-10-05 Mon 09:00]")) 1)))

(define-test (org dates-before-1900)
  ;; timestamps before 1900 read, moved and written: 1900 has no 29 February
  (is string= "<1850-03-04 Mon>" (tuatara.defaults::org-ts-text (tuatara.defaults::org-read-ts "<1850-03-04>")))
  (is = (tuatara.dates:day-number 1850 3 4) (tuatara.defaults::org-ts-day-number (tuatara.defaults::org-read-ts "<1850-03-04 Mon>")))
  (is string= "<1900-03-01 Thu>" (tuatara.defaults::org-ts-text (tuatara.defaults::org-ts-shift (tuatara.defaults::org-read-ts "<1896-02-29 Sat>") 4 #\y)))
  (is string= "<1900-03-01 Thu>" (tuatara.defaults::org-ts-text (tuatara.defaults::org-ts-shift (tuatara.defaults::org-read-ts "<1900-02-28 Wed>") 1 #\d)))
  (is string= "<1899-12-31 Sun 23:30>" (tuatara.defaults::org-ts-text (tuatara.defaults::org-ts-shift (tuatara.defaults::org-read-ts "<1900-01-01 Mon 00:30>") -1 #\h)))
  ;; the times of timestamps count on below 1900, and are written back as they were
  (flet ((at (y m d &optional (h 0) (mi 0))
           (tuatara.dates:day-universal-time (tuatara.dates:day-number y m d) h mi)))
    (is = (at 1850 3 4 9 30) (tuatara.defaults::org-ts-time (tuatara.defaults::org-read-ts "<1850-03-04 Mon 09:30>")))
    (is = (at 1850 3 4 9 30) (tuatara.defaults::org-ts-universal-time "[1850-03-04 Mon 09:30]"))
    (false (tuatara.defaults::org-ts-universal-time "[2026-02-31 Tue 09:30]"))
    (false (tuatara.defaults::org-ts-universal-time "[2026-02-03 Tue]"))
    (is string= "[1850-03-04 Mon 09:30]" (tuatara.defaults::org-timestamp-text (at 1850 3 4 9 30)))
    (is string= "<1850-03-04 Mon 09:30>" (tuatara.defaults::org-timestamp-text (at 1850 3 4 9 30) :active t))
    (is = (tuatara.dates:day-number 1850 3 4) (tuatara.defaults::org-today (at 1850 3 4 23 59))))
  ;; Org's date prompt on a day before 1900
  (let ((now (tuatara.dates:day-universal-time (tuatara.dates:day-number 1899 12 30) 18)))
    (flet ((read-date (input) (tuatara.defaults::org-ts-text (tuatara.defaults::org-read-date-text input now))))
      (is string= "<1899-12-30 Sat>" (read-date ""))
      (is string= "<1900-01-01 Mon>" (read-date "mon"))
      (is string= "<1900-01-02 Tue>" (read-date "+3d"))
      (is string= "<1900-02-28 Wed>" (read-date "feb 28"))))
  ;; an agenda of a week in 1850, its entry on its day, and that entry overdue in this week's
  (with-test-directory (dir)
    (let ((file (merge-pathnames "voyage.org" dir)))
      (alexandria:write-string-into-file (format nil "* TODO Old voyage~%SCHEDULED: <1850-03-05 Tue>~%") file)
      (let ((tuatara.defaults::*org-agenda-files* (list (namestring file)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
            (tuatara.defaults::*org-agenda-time-grid* nil))
        (flet ((lines (view)
                 (loop for (pieces) in (tuatara.defaults::org-agenda-view-lines view 80)
                       collect (string-right-trim " " (tuatara.defaults::org-agenda-pieces-text pieces)))))
          ;; line for line as Emacs's Org shows it (org-read-date-force-compatible-dates nil)
          (is equal '("Week-agenda (W10):" "Monday      4 March 1850 W10" "Tuesday     5 March 1850"
                      "  voyage:     Scheduled:  TODO Old voyage" "Wednesday   6 March 1850" "Thursday    7 March 1850"
                      "Friday      8 March 1850" "Saturday    9 March 1850" "Sunday     10 March 1850")
              (lines (list :kind :agenda :span :week :start (tuatara.dates:day-number 1850 3 4))))
          (true (find-if (lambda (line) (search "Old voyage" line))
                         (lines (list :kind :agenda :span :day :start (tuatara.defaults::org-today)))))))))
  ;; a clock table of clocks before 1900 is in test/org/dblock/before-1900.org, made as Org makes it
  (true (probe-file (asdf:system-relative-pathname :tuatara "test/org/dblock/before-1900.expected"))))

(define-test (org agenda-buffer)
  ;; SPC a a and a key, the views, their keys, the entries opened from them, and the agenda kept current
  (with-test-directory (dir)
    (let* ((source (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
           (tasks (merge-pathnames "tasks.org" dir))
           (notes (merge-pathnames "notes.org" dir)))
      (uiop:copy-file (merge-pathnames "tasks.org" source) tasks)
      (uiop:copy-file (merge-pathnames "notes.org" source) notes)
      (let ((tuatara.defaults::*org-agenda-files* (list (namestring tasks) (namestring notes)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
            (tuatara.defaults::*org-agenda-log-mode* nil))
        (labels ((agenda (keys)
                   (multiple-value-bind (f editor) (run-keys keys :width 100 :height 60)
                     (values f editor (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
                 (point-line (b)
                   (let ((p (tuatara.buffer:buffer-point b)))
                     (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-line-start b p) :end (tuatara.buffer:buffer-line-end b p))))
                 (showing (keys &rest texts)
                   (multiple-value-bind (f editor b) (agenda keys)
                     (declare (ignore editor))
                     (dolist (text texts)
                       (true (or (tuatara.frontend.fake:fake-frame-showing f text) (search text (tuatara.buffer:buffer-string b)))
                             "~a shows ~s" keys text)))))
          ;; the week, from Monday, point on today's date, today's grid and the entries coloured as Org colours them
          (multiple-value-bind (f editor b) (agenda "SPC a a a")
            (declare (ignore editor))
            (is string= "*Org Agenda*" (tuatara.buffer:buffer-name b))
            (true (tuatara.frontend.fake:fake-frame-showing f "Week-agenda (W40):"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Monday     28 September 2026 W40"))
            (true (tuatara.frontend.fake:fake-frame-showing f "18:00 ┄┄┄┄┄ ← now"))
            (is string= "Thursday    1 October 2026" (point-line b))
            (dolist (face '(:org-agenda-structure :org-agenda-date-today :org-agenda-date-weekend :org-scheduled-today
                            :org-scheduled-previously :org-imminent-deadline :org-upcoming-deadline :org-agenda-done
                            :org-time-grid :org-agenda-current-time :org-todo))
              (true (find face (car (tuatara.buffer:buffer-local b 'tuatara.defaults::made-spans)) :key #'third) "~a used" face)))
          ;; the next week and back, the day, the log
          (showing "SPC a a a l" "Week-agenda (W41):" "Monday      5 October 2026 W41")
          (showing "SPC a a a l h h" "Week-agenda (W39):")
          (showing "SPC a a a D" "Day-agenda (W40):" "Thursday    1 October 2026")
          ;; a day's agenda of another day has its time grid too
          (showing "SPC a a a D l" "Friday      2 October 2026" "8:00 ┄┄┄┄┄ ┄┄┄")
          (showing "SPC a a a L" "Closed:     DONE Deadline done" "Log mode is on")
          (multiple-value-bind (f editor b) (agenda "SPC a a a l g h")
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "Week-agenda (W40):"))
            (is string= "Thursday    1 October 2026" (point-line b)))
          ;; a list of lines: j and k move, the keys that move by words or select are not bound
          (multiple-value-bind (f editor b) (agenda "SPC a a a j")
            (declare (ignore f editor))
            (true (search "8:00 ┄┄┄┄┄ ┄┄┄" (point-line b))))
          (dolist (key '("b" "v" "M" "%"))
            (showing (format nil "SPC a a a ~a" key) (format nil "~a is undefined" key)))
          ;; whole lines selected, grown and copied
          (multiple-value-bind (f editor b) (agenda "SPC a a a V J J y")
            (declare (ignore f b))
            (is string= (format nil "Thursday    1 October 2026~%               8:00 ┄┄┄┄┄ ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄~%              10:00 ┄┄┄┄┄ ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄~%")
                (first (tuatara.command:editor-kill-ring editor))))
          ;; g g the top, G the end
          (multiple-value-bind (f editor b) (agenda "SPC a a a g g")
            (declare (ignore f editor))
            (is = 0 (tuatara.buffer:buffer-point b)))
          (multiple-value-bind (f editor b) (agenda "SPC a a a g e")
            (declare (ignore f editor))
            (is = (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-size b))
                (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b))))
          ;; g l back to the view before, as cd -, on the line it was on; again, forth
          (flet ((header-and-line (keys)
                   (multiple-value-bind (f editor b) (agenda keys)
                     (declare (ignore f editor))
                     (list (subseq (tuatara.buffer:buffer-string b) 0 (position #\Newline (tuatara.buffer:buffer-string b)))
                           (point-line b)))))
            (is equal '("Week-agenda (W40):" "Thursday    1 October 2026") (header-and-line "SPC a a a l g l"))
            (is string= "Week-agenda (W41):" (first (header-and-line "SPC a a a l g l g l")))
            (let ((back (header-and-line "SPC a a a j j D g l")))
              (is string= "Week-agenda (W40):" (first back))
              (true (search "10:00 ┄┄┄┄┄ ┄┄┄" (second back))))
            (is string= "Global list of TODO items of type: ALL" (first (header-and-line "SPC a a a SPC a a t g l g l")))
            (let ((old (tuatara.buffer:find-buffer "*Org Agenda*")))
              (when old (tuatara.buffer:kill-buffer old)))
            (showing "SPC a a a g l" "No view before this one"))
          ;; a week, a month and a year, and the agenda drawn again, from anywhere
          (showing "SPC a w" "Week-agenda (W40):")
          (showing "SPC a m" "Month-agenda (W40-W44):" "Saturday   31 October 2026")
          (showing "SPC a y" "Year-agenda (W01-W53):")
          ;; and in the agenda, on their own: w m Y the spans, D the day, a the views to choose from
          (showing "SPC a a a m" "Month-agenda (W40-W44):")
          (showing "SPC a a a D w" "Week-agenda (W40):")
          (showing "SPC a a a Y" "Year-agenda (W01-W53):")
          (showing "SPC a a a a t" "Global list of TODO items of type: ALL")
          (showing "SPC a s" "SPC a s is undefined")
          ;; the TODO list, a match, the untagged tasks; C-u 5 r the tasks with the fifth keyword
          (multiple-value-bind (f editor b) (agenda "SPC a a t")
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "Global list of TODO items of type: ALL"))
            (true (search "TODO Plain scheduled" (point-line b))))
          (showing "SPC a a t C-u 4 r" "Global list of TODO items of type: NEXT" "NEXT Deadline this week")
          (showing "SPC a a u" "Untagged tasks" "errands:    TODO Second untagged")
          (showing "SPC a a m w o r k RET" "Headlines with TAGS match: work" "notes:      DONE Done work")
          (showing "SPC a a z" "No agenda view on z")
          ;; the question goes once answered; C-g leaves it
          (multiple-value-bind (f editor b) (agenda "SPC a a a")
            (declare (ignore b))
            (false (search "Agenda: a week" (tuatara.frontend.fake:fake-screen-line f 59)))
            (true (tuatara.command:editor-windows editor)))
          (multiple-value-bind (f editor b) (agenda "SPC a a C-g")
            (declare (ignore f editor))
            (is string= "*scratch*" (tuatara.buffer:buffer-name b)))
          ;; RET opens the entry on the line, TAB in the other window; q leaves
          (multiple-value-bind (f editor b) (agenda "SPC a a t RET")
            (declare (ignore f editor))
            (is string= "tasks.org" (tuatara.buffer:buffer-name b))
            (true (uiop:string-prefix-p "* TODO Plain scheduled" (point-line b))))
          (multiple-value-bind (f editor b) (agenda "SPC a a t TAB")
            (declare (ignore f))
            (is string= "tasks.org" (tuatara.buffer:buffer-name b))
            (is = 2 (length (tuatara.command:editor-windows editor))))
          (showing "SPC a a a RET" "No entry on this line")
          (multiple-value-bind (f editor b) (agenda "SPC a a a q")
            (declare (ignore f editor))
            (is string= "*scratch*" (tuatara.buffer:buffer-name b)))
          ;; a task added in its file's buffer appears in the agenda shown
          (multiple-value-bind (f editor b) (agenda "SPC a a t RET o * SPC T O D O SPC B r a n d SPC n e w C-g SPC b")
            (declare (ignore f editor))
            (is string= "*Org Agenda*" (tuatara.buffer:buffer-name b))
            (true (search "TODO Brand new" (tuatara.buffer:buffer-string b))))
          (dolist (name '("tasks.org" "notes.org" "*Org Agenda*"))
            (let ((buffer (tuatara.buffer:find-buffer name)))
              (when buffer
                (tuatara.buffer:buffer-mark-saved buffer)
                (tuatara.buffer:kill-buffer buffer)))))))))

(define-test (org agenda-edits)
  ;; the entry on the agenda's line changed from the agenda, in its file's buffer: keyword, dates, tags, note, clock
  (with-test-directory (dir)
    (let* ((tuatara-user:*org-tags-column* -77)
           (source (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
           (tasks (merge-pathnames "tasks.org" dir))
           (notes (merge-pathnames "notes.org" dir)))
      (uiop:copy-file (merge-pathnames "notes.org" source) notes)
      (let ((tuatara.defaults::*org-agenda-files* (list (namestring tasks) (namestring notes)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
            (tuatara.defaults::*org-agenda-log-mode* nil)
            (tuatara.defaults::*org-log-into-drawer* nil))
        (labels ((fresh ()
                   (dolist (name '("tasks.org" "notes.org" "*Org Agenda*"))
                     (let ((buffer (tuatara.buffer:find-buffer name)))
                       (when buffer (tuatara.buffer:buffer-mark-saved buffer) (tuatara.buffer:kill-buffer buffer))))
                   (setf tuatara.defaults::*org-clock* nil)
                   (uiop:copy-file (merge-pathnames "tasks.org" source) tasks))
                 (after (keys)
                   (fresh)
                   (multiple-value-bind (f editor) (run-keys keys :width 100 :height 60)
                     (values (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "tasks.org")) f editor))))
          ;; t: the next keyword; the agenda shows it at once, the file's buffer holds it, not yet saved
          (multiple-value-bind (text f) (after "SPC a a t T")
            (true (search (format nil "* NEXT Plain scheduled~%") text))
            (true (tuatara.frontend.fake:fake-frame-showing f "NEXT Plain scheduled"))
            (true (tuatara.buffer:buffer-modified-p (tuatara.buffer:find-buffer "tasks.org")))
            (false (search "NEXT Plain scheduled" (alexandria:read-file-into-string tasks))))
          ;; saved at once when the file is one of *org-agenda-autosave-files* (t: every agenda file); others not
          (dolist (setting (list t (list (namestring tasks)) (list (concatenate 'string (namestring dir) "*.org"))))
            (let ((tuatara-user:*org-agenda-autosave-files* setting))
              (after "SPC a a t T")
              (false (tuatara.buffer:buffer-modified-p (tuatara.buffer:find-buffer "tasks.org")))
              (true (search "NEXT Plain scheduled" (alexandria:read-file-into-string tasks)))))
          (let ((tuatara-user:*org-agenda-autosave-files* (list (namestring notes))))
            (after "SPC a a t T")
            (true (tuatara.buffer:buffer-modified-p (tuatara.buffer:find-buffer "tasks.org")))
            (false (search "NEXT Plain scheduled" (alexandria:read-file-into-string tasks))))
          ;; clocking out from the agenda saves the clock's file too
          (let ((tuatara-user:*org-agenda-autosave-files* t))
            (after "SPC a a t I O")
            (true (search "=>  0:00" (alexandria:read-file-into-string tasks))))
          ;; with large agenda files (here: any), the agenda is drawn in the background, at first and again
          ;; after a change from it
          (let ((tuatara.defaults::*org-agenda-at-once* 0))
            (fresh)
            (let ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 60))
                  (start (get-internal-real-time))
                  (shown nil))
              (labels ((wait-for (text then)
                         (if (and (not (tuatara.frontend.fake:fake-frame-showing f text))
                                  (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
                             (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f (lambda () (wait-for text then))))
                             (funcall then)))
                       (wait-for-next ()
                         (wait-for "NEXT Plain scheduled"
                                   (lambda () (setf shown (tuatara.frontend.fake:fake-frame-showing f "NEXT Plain scheduled"))))))
                (tuatara.frontend.fake:fake-queue-keys f "SPC a a t")
                (tuatara.frontend.fake:fake-queue-action
                 f (lambda ()
                     (wait-for "TODO Plain scheduled"
                               (lambda ()
                                 (tuatara.frontend.fake:fake-queue-keys f "T")
                                 (tuatara.frontend.fake:fake-queue-action f #'wait-for-next))))))
              (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (or (tuatara.buffer:find-buffer "*scratch*") (tuatara.buffer:make-buffer "*scratch*" :text "")))
              (true shown)
              (true (search "* NEXT Plain scheduled" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "tasks.org"))))))
          ;; scheduled, a deadline (C-c C-d), tags (C-c C-q), a note (C-c C-z)
          (true (search (format nil "* TODO Plain scheduled~%SCHEDULED: <2026-10-09 Fri>")
                        (after "SPC a a t C-c C-s 2 0 2 6 - 1 0 - 0 9 RET")))
          (true (search (format nil "* TODO Plain scheduled~%DEADLINE: <2026-10-20 Tue> SCHEDULED: <2026-09-30 Wed>")
                        (after "SPC a a t C-c C-d 2 0 2 6 - 1 0 - 2 0 RET")))
          (true (search (format nil "* TODO Plain scheduled~55@a~%" ":urgent:")
                        (after "SPC a a t C-c C-q u r g e n t RET")))
          (true (search (format nil "SCHEDULED: <2026-09-30 Wed>~%- Note taken on [2026-10-01 Thu 18:00] \\\\~%  Called back~%")
                        (after "SPC a a t C-c C-z C a l l e d SPC b a c k RET")))
          ;; the leader's SPC a keys on their own in the agenda: d a deadline, t tags, n a note
          (true (search (format nil "* TODO Plain scheduled~%DEADLINE: <2026-10-20 Tue> SCHEDULED: <2026-09-30 Wed>")
                        (after "SPC a a t d 2 0 2 6 - 1 0 - 2 0 RET")))
          (true (search (format nil "* TODO Plain scheduled~55@a~%" ":urgent:") (after "SPC a a t t u r g e n t RET")))
          (true (search (format nil "- Note taken on [2026-10-01 Thu 18:00] \\\\~%  Called back~%")
                        (after "SPC a a t n C a l l e d SPC b a c k RET")))
          ;; I clocks in on the entry, O clocks out
          (true (search (format nil "SCHEDULED: <2026-09-30 Wed>~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 18:00]--[2026-10-01 Thu 18:00] =>  0:00~%:END:")
                        (after "SPC a a t I O")))
          ;; s saves the agenda files changed, an Org buffer not on the agenda left unsaved
          (let* ((other-path (merge-pathnames "other.org" dir))
                 (other (progn (alexandria:write-string-into-file (format nil "* Other~%") other-path :if-exists :supersede)
                               (tuatara.buffer:make-buffer "other.org" :text (format nil "* Other~%")
                                                                        :file (namestring other-path)))))
            (unwind-protect
                 (progn
                   (tuatara.defaults::set-major-mode other 'tuatara.defaults::org-mode)
                   (tuatara.buffer:buffer-insert other 0 "x")
                   (multiple-value-bind (text f) (after "SPC a a t T s")
                     (declare (ignore text))
                     (true (tuatara.frontend.fake:fake-frame-showing f "Saved 1 agenda file"))
                     (true (search "* NEXT Plain scheduled" (alexandria:read-file-into-string tasks)))
                     (true (tuatara.buffer:buffer-modified-p other))
                     (is string= (format nil "* Other~%") (alexandria:read-file-into-string other-path)))
                   ;; M-x org-save-all-org-buffers saves it too
                   (true (tuatara.frontend.fake:fake-frame-showing
                          (run-keys "M-x o r g - s a v e - a l l - o r g - b u f f e r s RET" :width 100 :height 10)
                          "Saved 1 Org buffer"))
                   (is string= (format nil "x* Other~%") (alexandria:read-file-into-string other-path)))
              (tuatara.buffer:buffer-mark-saved other)
              (tuatara.buffer:kill-buffer other)))
          ;; a file that may not be written is not changed
          (fresh)
          (sb-posix:chmod (namestring tasks) #o444)
          (multiple-value-bind (f editor) (run-keys "SPC a a t T" :width 100 :height 60)
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "read-only"))
            (false (search "NEXT Plain" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "tasks.org")))))
          (sb-posix:chmod (namestring tasks) #o644)
          ;; not on an entry, and elsewhere
          (fresh)
          (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC a a a T" :width 100 :height 60) "No entry on this line"))
          (true (tuatara.frontend.fake:fake-frame-showing (run-keys "M-x o r g - a g e n d a - d e a d l i n e RET" :width 100 :height 10)
                                                          "org-deadline works on an entry of the agenda or an Org file"))
          (fresh))))))

(define-test (org agenda-views-worked-out-in-the-background)
  ;; with large agenda files (here: any), every view and span is worked out in the background: opening the
  ;; agenda, l and h, g h, L, g l, r, C-u N r; never on the editor's thread, point put where each asks
  (let* ((directory (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
         (tuatara.defaults::*org-agenda-files* (list (namestring (merge-pathnames "tasks.org" directory))
                                                     (namestring (merge-pathnames "notes.org" directory))))
         (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
         (tuatara.defaults::*org-agenda-log-mode* nil)
         (tuatara.defaults::*org-agenda-at-once* 0)
         (compose (fdefinition 'tuatara.defaults::org-agenda-compose))
         (editor-thread sb-thread:*current-thread*)
         (on-editor 0) (in-background 0)
         (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 60))
         (start (get-internal-real-time))
         (placeholder nil)
         (seen '()))
    (labels ((agenda () (tuatara.buffer:find-buffer "*Org Agenda*"))
             (point-line ()
               (let* ((b (agenda)) (p (tuatara.buffer:buffer-point b)))
                 (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-line-start b p) :end (tuatara.buffer:buffer-line-end b p))))
             (holds (text) (and (agenda) (search text (tuatara.buffer:buffer-string (agenda)))))
             (wait (text rest)
               (if (and (not (holds text)) (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f (lambda () (wait text rest))))
                   (progn (push (list text (and (holds text) t) (point-line)) seen)
                          (steps rest))))
             (steps (list)
               (when list
                 (destructuring-bind (keys text) (first list)
                   (tuatara.frontend.fake:fake-queue-keys f keys)
                   (tuatara.frontend.fake:fake-queue-action f (lambda () (wait text (rest list))))))))
      (let ((old (agenda))) (when old (tuatara.buffer:kill-buffer old)))
      (unwind-protect
           (progn
             (setf (fdefinition 'tuatara.defaults::org-agenda-compose)
                   (lambda (&rest arguments)
                     (if (eq sb-thread:*current-thread* editor-thread)
                         (incf on-editor)
                         (progn (incf in-background) (sleep 0.1)))
                     (apply compose arguments)))
             (tuatara.frontend.fake:fake-queue-keys f "SPC a a a")
             ;; while it is worked out, the agenda says so
             (tuatara.frontend.fake:fake-queue-action f (lambda () (setf placeholder (holds "Working out the agenda"))))
             (tuatara.frontend.fake:fake-queue-action f (lambda () (wait "Week-agenda (W40):"
                                                                         '(("l" "Week-agenda (W41):")
                                                                           ("g h" "Week-agenda (W40):")
                                                                           ("h" "Week-agenda (W39):")
                                                                           ("g l" "Week-agenda (W40):")
                                                                           ("L" "Closed:")
                                                                           ("SPC a a t" "type: ALL")
                                                                           ("C-u 4 r" "type: NEXT")
                                                                           ("r" "type: NEXT")))))
             (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                           :buffer (or (tuatara.buffer:find-buffer "*scratch*") (tuatara.buffer:make-buffer "*scratch*" :text ""))))
        (setf (fdefinition 'tuatara.defaults::org-agenda-compose) compose))
      (setf seen (reverse seen))
      (true placeholder)
      (is = 9 (length seen))
      (is = 0 on-editor "the agenda was worked out ~d times on the editor's thread" on-editor)
      (true (>= in-background 8))
      (loop for (text shown) in seen do (true shown "~a shown" text))
      (flet ((line-of (n) (third (nth n seen))))
        ;; today's date, the first date of another week, back on today, the line g l left
        (is string= "Thursday    1 October 2026" (line-of 0))
        (true (uiop:string-prefix-p "Monday      5 October 2026" (line-of 1)))
        (is string= "Thursday    1 October 2026" (line-of 2))
        (true (uiop:string-prefix-p "Monday     21 September 2026" (line-of 3)))
        (is string= "Thursday    1 October 2026" (line-of 4))
        ;; the TODO list on its first task
        (true (search "TODO Plain scheduled" (line-of 6)))
        (true (search "NEXT " (line-of 7))))
      (let ((old (agenda)))
        (when old (tuatara.buffer:kill-buffer old))))))

(define-test (org tags-and-notes)
  ;; C-c C-q sets a headline's tags at column 77, C-c C-z adds a note, as Org does
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        (tuatara.defaults::*org-log-into-drawer* nil)
        (tuatara-user:*org-tags-column* -77))
    (is string= (format nil "* TODO Task~66@a~%Body~%" ":work:urgent:")
        (org-text-after "C-c C-q w o r k SPC u r g e n t RET" (format nil "* TODO Task~%Body~%")))
    (is string= (format nil "* TODO Task~%Body~%")
        (org-text-after "C-c C-q C-u RET" (format nil "* TODO Task      :old:~%Body~%")))
    (is string= (format nil "* A very long headline that runs on and on past the column the tags go at :x:~%")
        (org-text-after "C-c C-q x RET" (format nil "* A very long headline that runs on and on past the column the tags go at~%")))
    (is string= (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%- Note taken on [2026-10-01 Thu 18:00] \\\\~%  Hello~%Body~%")
        (org-text-after "C-c C-z H e l l o RET" (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%Body~%")))
    (let ((tuatara.defaults::*org-log-into-drawer* t))
      (is string= (format nil "* Task~%:LOGBOOK:~%- Note taken on [2026-10-01 Thu 18:00] \\\\~%  Hi~%:END:~%")
          (org-text-after "C-c C-z H i RET" (format nil "* Task~%"))))
    (is string= (format nil "* Task~%") (org-text-after "C-c C-z RET" (format nil "* Task~%")))))

(define-test (org agenda-beside-typing)
  ;; typing in an agenda file of 3,000 tasks with the agenda shown beside it: no key waits for the
  ;; agenda, which is worked out in the background and shows what was typed
  (with-test-directory (dir)
    (let ((file (merge-pathnames "big.org" dir)))
      (alexandria:write-string-into-file
       (with-output-to-string (out)
         (dotimes (i 3000) (format out "* TODO Task ~d :work:~%SCHEDULED: <2026-10-~2,'0d Thu>~%" i (1+ (mod i 28)))))
       file)
      (let ((tuatara.defaults::*org-agenda-files* (list (namestring file)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
            (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 40))
            (times '()) (last nil))
        (flet ((stamp () (let ((now (get-internal-real-time))) (when last (push (- now last) times)) (setf last now))))
          (tuatara.frontend.fake:fake-queue-keys f (format nil "SPC a a a C-x 2 C-x o C-x C-f C-a C-k ~a RET g g o" (rimu-typed (namestring file))))
          (tuatara.frontend.fake:fake-queue-action f #'stamp)
          (dolist (key '("N" "e" "w" "T" "a" "s" "k"))
            (tuatara.frontend.fake:fake-queue-keys f key)
            (tuatara.frontend.fake:fake-queue-action f #'stamp))
          (tuatara.frontend.fake:fake-queue-keys f "C-g")
          (let ((start (get-internal-real-time)))
            (labels ((wait () (if (and (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second))
                                       (not (tuatara.frontend.fake:fake-frame-showing f "NewTask")))
                                  (progn (sleep 0.05) (tuatara.frontend.fake:fake-queue-action f #'wait))
                                  (tuatara.frontend.fake:fake-queue-keys f ""))))
              (tuatara.frontend.fake:fake-queue-action f #'wait)))
          (let ((old (tuatara.buffer:find-buffer "*scratch*"))) (when old (tuatara.buffer:kill-buffer old)))
          (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                        :buffer (tuatara.buffer:make-buffer "*scratch*" :text ""))
          (true (< (reduce #'max times) (floor internal-time-units-per-second 10)) "slowest key ~d ms"
                (floor (* 1000 (reduce #'max times)) internal-time-units-per-second))
          (true (tuatara.frontend.fake:fake-frame-showing f "NewTask") "the agenda shows what was typed")))
      (dolist (name '("big.org" "*Org Agenda*" "*scratch*"))
        (let ((buffer (tuatara.buffer:find-buffer name)))
          (when buffer (tuatara.buffer:buffer-mark-saved buffer) (tuatara.buffer:kill-buffer buffer)))))))

(define-test (org clock-and-subtrees-at-their-edges)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 9 2 10 2026)))
    ;; M-k on the first subtree says there is none before it, no error
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET M-k" :text (format nil "* A~%* B~%") :width 80 :height 10)
      (is string= (format nil "* A~%* B~%") (session-text editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "UNSIGNED-BYTE")))
    ;; the clock's messages show the time of day
    (true (tuatara.frontend.fake:fake-frame-showing
           (run-keys "M-x o r g - m o d e RET C-c C-x C-i" :text (format nil "* Task~%") :width 80 :height 10)
           "Clock starts at 09:00: Task"))
    (setf tuatara.defaults::*org-clock* nil)
    ;; clocking out in a buffer made read-only keeps the clock running
    (run-keys "M-x o r g - m o d e RET C-c C-x C-i M-x r e a d - o n l y - m o d e RET C-c C-x C-o" :text (format nil "* Task~%") :width 80 :height 10)
    (true tuatara.defaults::*org-clock*)
    (setf tuatara.defaults::*org-clock* nil)
    ;; killing the buffer of the clock stops it
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-c C-x C-i" :text (format nil "* Task~%") :width 120 :height 10)
      (declare (ignore f))
      (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (true tuatara.defaults::*org-clock*)
        (tuatara.buffer:buffer-mark-saved buffer)
        (tuatara.buffer:kill-buffer buffer)
        (false tuatara.defaults::*org-clock*)))
    (setf tuatara.defaults::*org-clock* nil)))

(define-test (org tags-one-space-after)
  ;; by default tags stand one space after the headline's text; C-u C-c C-q and M-x org-align-all-tags put them all so
  (let ((lined-up (format nil "* Top~%** TODO Garden shed roof repair H2 2026~27@a~%Body~%** DONE Install Doom [0/6] [0%] :projects:~%*** Wide~70@a~%"
                          ":projects:" ":a:b:"))
        (one-space (format nil "* Top~%** TODO Garden shed roof repair H2 2026 :projects:~%Body~%** DONE Install Doom [0/6] [0%] :projects:~%*** Wide :a:b:~%")))
    (is string= (format nil "* TODO Task :work:~%") (org-text-after "C-c C-q w o r k RET" (format nil "* TODO Task~%")))
    ;; the tags the file uses offered, as a capture template's %^g offers them, the next after a colon
    (is string= (format nil "* A :urgent:~%* B :urgent:~%") (org-text-after "j C-c C-q u r g RET" (format nil "* A :urgent:~%* B~%")))
    (is string= (format nil "* A :urgent:home:~%* B :home:urgent:~%")
        (org-text-after "j C-c C-q h o m e : u r g RET" (format nil "* A :urgent:home:~%* B~%")))
    (is string= one-space (org-text-after "C-u C-c C-q" lined-up))
    (is string= one-space (org-text-after "M-x o r g - a l i g n - a l l - t a g s RET" lined-up))
    ;; one headline: the one point is under
    (is string= (format nil "* Top~%** TODO Garden shed roof repair H2 2026 :projects:~%Body~%** DONE Install Doom [0/6] [0%] :projects:~%*** Wide~70@a~%" ":a:b:")
        (org-text-after "j j M-x o r g - a l i g n - t a g s RET" lined-up))
    ;; and back out to column 77, as Org's own
    (let ((tuatara-user:*org-tags-column* -77))
      (is string= (format nil "* Top~%** TODO Garden shed roof repair H2 2026~38@a~%Body~%** DONE Install Doom [0/6] [0%]~46@a~%*** Wide~69@a~%"
                          ":projects:" ":projects:" ":a:b:")
          (org-text-after "M-x o r g - a l i g n - a l l - t a g s RET" one-space)))
    ;; point stays on the tags it was on, and one undo takes it all back
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET l l l l l l l l l l l l M-x o r g - a l i g n - t a g s RET"
                                              :text (format nil "* Head      :tag:~%") :width 100 :height 10)
      (declare (ignore f))
      (is = 7 (session-point editor)))
    (is string= lined-up (org-text-after "M-x o r g - a l i g n - a l l - t a g s RET u" lined-up))))

(define-test (org tags-aligned-with-folds-and-clock)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026))
        (tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil)
        (lined-up (format nil "* One~63@a~%body one~%* Two~63@a~%body two~%* Three~61@a~%body three~%" ":a:" ":b:" ":c:")))
    ;; aligning every headline's tags is one edit a line: a fold between them stays folded
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET j j TAB g g M-x o r g - a l i g n - a l l - t a g s RET"
                                              :text lined-up :width 100 :height 12)
      (is string= (format nil "* One :a:~%body one~%* Two :b:~%body two~%* Three :c:~%body three~%") (session-text editor))
      (is string= "* Two :b:..." (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 2)))
      (is string= "* Three :c:" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 3))))
    ;; and the clock running on a headline between them keeps its line: clocking out finishes it
    (let ((editor (nth-value 1 (run-keys "M-x o r g - m o d e RET j j C-c C-x C-i g g M-x o r g - a l i g n - a l l - t a g s RET C-c C-x C-o"
                                         :text lined-up :width 100 :height 12))))
      (true (search "=>  0:00" (session-text editor)))
      (false tuatara.defaults::*org-clock*))
    ;; tags moved right, point on them: point stays on them, nothing past the text
    (let ((tuatara-user:*org-tags-column* -77))
      (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET M-x e n d - o f - l i n e RET M-x o r g - a l i g n - t a g s RET"
                                                :text (format nil "* Head :tag:~%next~%") :width 100 :height 12)
        (declare (ignore f))
        (let ((text (session-text editor)))
          (true (search (format nil "* Head~71@a~%" ":tag:") text))
          (is = (position #\Newline text) (session-point editor)))))))

(define-test (org agenda-and-tags-corners)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        (tuatara.defaults::*org-log-into-drawer* nil)
        (tuatara-user:*org-tags-column* -77))
    ;; the tags of a headline of stars alone taken away: still a headline; wide tags lined up by their columns
    (is string= (format nil "* ~%") (org-text-after "C-c C-q C-u RET" (format nil "* :old:~%")))
    (is string= (format nil "* T~72@a~%" ":日本:") (org-text-after "C-c C-q 日 本 RET" (format nil "* T~%")))
    ;; point stays by its text after C-c C-q and C-c C-z
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET j j C-c C-q x RET" :text (format nil "* Task~%one~%two~%") :width 100 :height 10)
      (declare (ignore f))
      (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is = 2 (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b))))))
  ;; a whole headline found again, never one that only begins the same; none, and nothing is changed
  (let ((b (tuatara.buffer:make-buffer "entry-start-test" :text (format nil "* New~%* Other~%* TODO Task two~%* TODO Task~%"))))
    (unwind-protect
         (let ((entry (tuatara.defaults::make-org-agenda-entry :level 1 :heading "TODO Task" :position 8
                                                               :text (format nil "* Other~%* TODO Task~%"))))
           (is = 3 (tuatara.buffer:buffer-line-number b (tuatara.defaults::org-agenda-entry-start b entry)))
           (setf (tuatara.defaults::agenda-entry-heading entry) "TODO Renamed")
           (false (tuatara.defaults::org-agenda-entry-start b entry)))
      (tuatara.buffer:kill-buffer b)))
  ;; a file's MIME type by its name's ending, whatever else is in the name
  (is string= "video/webm" (tuatara.defaults::file-mime-type :matroska "x\\.webm"))
  (is string= "video/mp4" (tuatara.defaults::file-mime-type :mp4 "clip\\")))

(define-test (org agenda-engine)
  ;; what Org's agenda lists for the test files on Thursday 2026-10-01 at 18:00, view by view, line by line
  (let* ((directory (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
         (files (list (namestring (merge-pathnames "tasks.org" directory)) (namestring (merge-pathnames "notes.org" directory))))
         (expected (with-open-file (in (merge-pathnames "agenda.expected" directory) :external-format :utf-8)
                     (let ((*read-eval* nil)) (loop for form = (read in nil in) until (eq form in) collect form))))
         (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
         (monday (tuatara.dates:day-number 2026 9 28))
         (today (tuatara.dates:day-number 2026 10 1)))
    (flet ((check (name rows)
             (let ((wanted (rest (assoc name expected :test #'equal))))
               (is = (length wanted) (length rows) "~a: ~d lines, Org lists ~d" name (length rows) (length wanted))
               (loop for want in wanted for got in rows for i from 1
                     do (is equal want got "~a, line ~d" name i)))))
      (check "week" (org-agenda-test-rows (tuatara.defaults::org-agenda-span monday 7 :files files :log nil)))
      (check "week-log" (org-agenda-test-rows (tuatara.defaults::org-agenda-span monday 7 :files files :log t)))
      (check "day" (org-agenda-test-rows (tuatara.defaults::org-agenda-day today :files files :log nil)))
      (check "todo" (org-agenda-test-rows (tuatara.defaults::org-agenda-todo-list :files files)))
      ;; the week after (repeats to come), a month, a week long past
      (check "next-week" (org-agenda-test-rows (tuatara.defaults::org-agenda-span (+ monday 7) 7 :files files :log nil)))
      (check "month" (org-agenda-test-rows (tuatara.defaults::org-agenda-span today 31 :files files :log nil)))
      (check "past-week" (org-agenda-test-rows (tuatara.defaults::org-agenda-span (- monday 14) 7 :files files :log nil)))
      (dolist (match '("work" "+home-work" "work|proj" "TODO=\"NEXT\"" "CATEGORY=\"errands\"" "LEVEL=2" "proj+TODO=\"TODO\""))
        (check (format nil "tags ~a" match) (org-agenda-test-rows (tuatara.defaults::org-agenda-match match :files files))))
      (dolist (match '("-{.*}" "work"))
        (check (format nil "tags-todo ~a" match) (org-agenda-test-rows (tuatara.defaults::org-agenda-match match :todo-only t :files files))))))
  ;; a file of 3,000 tasks: read once, then a year of days at once
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "many.org" dir)))
          (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026)))
      (with-open-file (out path :direction :output :external-format :utf-8)
        (dotimes (i 3000)
          (format out "* TODO Task ~d :t~d:~%~a: <2026-~2,'0d-~2,'0d Thu~@[ ~a~]>~%Body <2026-~2,'0d-~2,'0d Mon +1w>~%"
                  i (mod i 7) (if (evenp i) "SCHEDULED" "DEADLINE") (1+ (mod i 12)) (1+ (mod i 28)) (and (zerop (mod i 5)) "+1m")
                  (1+ (mod i 12)) (1+ (mod i 28)))))
      (tuatara.defaults::org-agenda-span (tuatara.defaults::org-today) 7 :files (list path))
      (let ((start (get-internal-real-time)))
        (tuatara.defaults::org-agenda-span (tuatara.defaults::org-today) 365 :files (list path))
        (true (< (- (get-internal-real-time) start) internal-time-units-per-second))))))

(define-test (org checkbox-tasks-in-agenda)
  ;; the agenda's TODO list holds checkbox items, their TODO in a colour of their own; T ticks one, logged; d is refused
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "errands.org" dir))
           (tuatara.defaults::*org-agenda-files* (list (namestring file)))
           (tuatara.defaults::*org-now* (encode-universal-time 0 5 14 2 10 2026)))
      (flet ((fresh ()
               (dolist (name '("errands.org" "*Org Agenda*"))
                 (let ((b (tuatara.buffer:find-buffer name)))
                   (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b))))
               (alexandria:write-string-into-file (format nil "* Errands~%- [ ] call Anna~%") file :if-exists :supersede)))
        (fresh)
        (let ((f (run-keys "SPC a a t" :width 100 :height 20)))
          (true (tuatara.frontend.fake:fake-frame-showing f "TODO call Anna"))
          (true (loop for row below 18 thereis (org-faces-include f row "TODO" :org-checkbox-todo))))
        (fresh)
        (run-keys "SPC a a t T" :width 100 :height 20)
        (true (search (format nil "- [X] call Anna~%  :LOGBOOK:~%  - State \"DONE\"")
                      (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "errands.org"))))
        (fresh)
        (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC a a t d" :width 100 :height 20) "checkbox has no deadline"))
        (fresh)
        ;; refiling from a checkbox's line is refused: its heading would go with it
        (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC a a t C-c C-w" :width 100 :height 20) "checkbox is not refiled"))
        (is string= (format nil "* Errands~%- [ ] call Anna~%") (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "errands.org")))
        (fresh)))))

(define-test (org agenda-g-e)
  ;; g e in the agenda goes to its end, g g to its top; G is not bound
  (run-keys "SPC a a w" :width 100 :height 20)
  (let ((keymap (tuatara.modes:mode-keymap 'tuatara.defaults::org-agenda-mode)))
    (flet ((bound (keys) (tuatara.command:lookup-key keymap (tuatara.frontend:parse-key-sequence keys))))
      (is eq 'tuatara.defaults::end-of-buffer (bound "g e"))
      (false (bound "G"))
      ;; t sets the tags, : does not
      (is eq 'tuatara.defaults::org-agenda-set-tags (bound "t"))
      (false (bound ":"))
      (is eq 'tuatara.defaults::beginning-of-buffer (bound "g g")))))

(define-test (org agenda-files-patterns)
  ;; agenda files written as a folder or a pattern: the .org files they stand for, in order, each once
  (with-test-directory (dir)
    (dolist (name '("b.org" "a.org" ".hidden.org" "notes.txt" "c.org"))
      (alexandria:write-string-into-file "* x" (merge-pathnames name dir)))
    (let ((folder (string-right-trim "/" (namestring dir))))
      (flet ((names (files) (mapcar #'file-namestring (tuatara.defaults::org-agenda-file-paths files))))
        (is equal '("a.org" "b.org" "c.org") (names (list (format nil "~a/" folder))))
        (is equal '("a.org" "b.org" "c.org") (names (list folder)))
        (is equal '("a.org" "b.org" "c.org") (names (list (format nil "~a/*.org" folder))))
        (is equal '("b.org") (names (list (format nil "~a/b*.org" folder))))
        (is equal '("notes.txt") (names (list (format nil "~a/*.txt" folder))))
        ;; a file named twice comes once; one not there stays, as Org keeps it
        (is equal '("a.org" "b.org" "c.org" "none.org")
            (names (list (format nil "~a/a.org" folder) (format nil "~a/*.org" folder) (format nil "~a/none.org" folder))))))))

(define-test (org agenda-search-filters)
  ;; C-x / s narrows the agenda as every list: its dates over the entries left, stacked, taken away; the agenda's
  ;; keys act on the entry on the line; drawn again it stays narrowed, another view or span is shown whole
  (with-test-directory (dir)
    (let* ((source (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
           (tasks (merge-pathnames "tasks.org" dir))
           (notes (merge-pathnames "notes.org" dir)))
      (uiop:copy-file (merge-pathnames "notes.org" source) notes)
      (let ((tuatara.defaults::*org-agenda-files* (list (namestring tasks) (namestring notes)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
            (tuatara.defaults::*org-agenda-log-mode* nil)
            (tuatara-user:*org-agenda-autosave-files* nil))
        (labels ((fresh ()
                   (dolist (name '("tasks.org" "notes.org" "*Org Agenda*"))
                     (let ((buffer (tuatara.buffer:find-buffer name)))
                       (when buffer (tuatara.buffer:buffer-mark-saved buffer) (tuatara.buffer:kill-buffer buffer))))
                   (setf tuatara.defaults::*org-clock* nil)
                   (uiop:copy-file (merge-pathnames "tasks.org" source) tasks))
                 (agenda (keys)
                   (fresh)
                   (multiple-value-bind (f editor) (run-keys keys :width 100 :height 60)
                     (declare (ignore editor))
                     (let ((b (tuatara.buffer:find-buffer "*Org Agenda*")))
                       (values (tuatara.buffer:buffer-string b) f b))))
                 (first-line (text) (subseq text 0 (position #\Newline text))))
          ;; a word: the entries whose lines hold it, under their dates; the dates with none and their grid go
          (let ((text (agenda "SPC a a a C-x / s m e e t i n g RET")))
            (true (uiop:string-prefix-p "Filtered by meeting: 1 of " (first-line text)) (first-line text))
            (true (search "Week-agenda (W40):" text))
            (true (search "Thursday    1 October 2026" text))
            (true (search "14:00-15:00 Meeting" text))
            (true (search "8:00 ┄┄┄┄┄" text) "the grid goes with its date")
            (false (search "Monday     28 September" text))
            (false (search "Plain scheduled" text))
            (false (search "Friday" text)))
          ;; by a column: NEXT in the keyword alone, not in a heading or a category
          (multiple-value-bind (text f b) (agenda "SPC a a a C-x / f k e y w o r d RET N E X T RET")
            (true (tuatara.frontend.fake:fake-frame-showing f "Filter by the column:"))
            (true (search "NEXT Deadline this week" text))
            (false (search "TODO Deadline soon" text))
            (is equal tuatara.defaults::*org-agenda-columns* (mapcar #'first (tuatara.defaults::list-table-columns b)))
            (let ((item (find-if #'tuatara.defaults::org-agenda-item-p (tuatara.buffer:buffer-local b 'tuatara.defaults::list-entries))))
              (true item)
              (when item
                (is equal '("NEXT" "Deadline this week" ":home::work:urgent:")
                    (let ((cells (tuatara.defaults::org-agenda-item-cells item)))
                      (list (aref cells 3) (aref cells 5) (aref cells 6)))))))
          ;; another day's grid goes with its date when that date has nothing left
          (let ((text (agenda "SPC a a a D l C-x / s n o t h i n g RET")))
            (true (uiop:string-prefix-p "Filtered by nothing: 0 of " (first-line text)))
            (false (search "┄┄┄" text))
            (false (search "October" text)))
          ;; stacked: the second narrows what the first left; C-x / r takes them away
          (let ((text (agenda "SPC a a a C-x / s t a s k RET C-x / s u n t a g g e d RET")))
            (true (uiop:string-prefix-p "Filtered by task, untagged: 1 of " (first-line text)) (first-line text))
            (true (search "TODO Untagged task" text))
            (false (search "Priority task" text)))
          (let ((text (agenda "SPC a a a C-x / s t a s k RET C-x / s u n t a g g e d RET C-x / r")))
            (false (search "Filtered by" text))
            (true (search "Monday     28 September 2026 W40" text))
            (true (search "Plain scheduled" text)))
          ;; a TODO change on an entry the filters leave: that entry's, and the agenda stays narrowed
          (multiple-value-bind (text f) (agenda "SPC a a t C-x / s d e a d l i n e SPC s o o n RET g e T")
            (declare (ignore f))
            (true (uiop:string-prefix-p "Filtered by deadline soon: 1 of " (first-line text)) (first-line text))
            (true (search "NEXT Deadline soon" text))
            (let ((file (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "tasks.org"))))
              (true (search "* NEXT Deadline soon" file))
              (true (search "* TODO Plain scheduled" file))
              (true (search "* TODO Deadline far" file))))
          ;; RET opens the entry on the line among those left
          (agenda "SPC a a t C-x / s f a r RET g e RET")
          (let ((b (tuatara.buffer:find-buffer "tasks.org")))
            (true (uiop:string-prefix-p "* TODO Deadline far"
                                        (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-point b)))))
          ;; drawn again (r, L) it stays narrowed
          (dolist (keys '("SPC a a a C-x / s m e e t i n g RET r" "SPC a a a C-x / s m e e t i n g RET g r"
                          "SPC a a a C-x / s m e e t i n g RET L"))
            (let ((text (agenda keys)))
              (true (uiop:string-prefix-p "Filtered by meeting: 1 of " (first-line text)) "~a: ~a" keys (first-line text))
              (false (search "Plain scheduled" text))))
          ;; another span or view is shown whole, the filters gone
          (dolist (keys '("SPC a a a C-x / s m e e t i n g RET l" "SPC a a a C-x / s m e e t i n g RET D"
                          "SPC a a a C-x / s m e e t i n g RET a t"))
            (let ((text (agenda keys)))
              (false (search "Filtered by" text) "~a: ~a" keys (first-line text))
              (true (null (tuatara:search-filters (tuatara.buffer:find-buffer "*Org Agenda*"))))))
          ;; the keys after C-x / s are its pattern: s a v e saves nothing
          (multiple-value-bind (text f) (agenda "SPC a a t T C-x / s s a v e RET")
            (true (uiop:string-prefix-p "Filtered by save: 0 of " (first-line text)) (first-line text))
            (false (tuatara.frontend.fake:fake-frame-showing f "Saved"))
            (true (tuatara.buffer:buffer-modified-p (tuatara.buffer:find-buffer "tasks.org")))
            (false (search "NEXT Plain scheduled" (alexandria:read-file-into-string tasks))))
          (fresh)
          ;; drawn again in the background after an agenda file changes (large files: here any), still narrowed
          (let ((tuatara.defaults::*org-agenda-at-once* 0)
                (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 60))
                (start (get-internal-real-time))
                (seen nil))
            (labels ((agenda-text ()
                       (let ((b (tuatara.buffer:find-buffer "*Org Agenda*"))) (if b (tuatara.buffer:buffer-string b) "")))
                     (wait-for (text then)
                       (if (and (not (search text (agenda-text)))
                                (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
                           (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f (lambda () (wait-for text then))))
                           (funcall then))))
              (tuatara.frontend.fake:fake-queue-keys f "SPC a a a")
              (tuatara.frontend.fake:fake-queue-action
               f (lambda ()
                   (wait-for "Week-agenda"
                             (lambda ()
                               (tuatara.frontend.fake:fake-queue-keys f "C-x / s m e e t i n g RET")
                               (tuatara.frontend.fake:fake-queue-action
                                f (lambda ()
                                    (let ((b (tuatara.io:visit-file (namestring tasks))))
                                      (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-size b)
                                                                    (format nil "* Meeting again~%<2026-10-02 Fri 11:00>~%")))
                                    (wait-for "Meeting again" (lambda () (setf seen (agenda-text))))))))))
              (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (or (tuatara.buffer:find-buffer "*scratch*") (tuatara.buffer:make-buffer "*scratch*" :text ""))))
            (true seen)
            (when seen
              (true (uiop:string-prefix-p "Filtered by meeting: 2 of " (first-line seen)) (first-line seen))
              (true (search "Friday      2 October 2026" seen))
              (false (search "Plain scheduled" seen))))
          (fresh))))))

(define-test (org catch-up-repeat-in-one-step)
  ;; ++ moves a timestamp on to just past now: from long ago at once, where a step at a time would take millions
  (let ((now (encode-universal-time 0 30 14 20 10 2026)))
    (flet ((stepped (text)
             ;; one step at a time, as the repeater means it
             (let* ((ts (tuatara.defaults::org-read-ts text))
                    (value (tuatara.defaults::org-ts-repeat-value ts)) (unit (tuatara.defaults::org-ts-repeat-unit ts)))
               (loop do (tuatara.defaults::org-ts-shift ts value unit)
                     while (<= (tuatara.defaults::org-ts-time ts) now))
               (tuatara.defaults::org-ts-text ts)))
           (repeated (text) (tuatara.defaults::org-ts-text (tuatara.defaults::org-ts-repeat (tuatara.defaults::org-read-ts text) now))))
      (dolist (text '("<2026-10-20 Tue 14:30 ++1h>" "<2026-10-20 Tue 14:29 ++1h>" "<2026-03-01 Sun 09:00 ++3h>"
                      "<2026-10-20 Tue ++1d>" "<2025-01-31 Fri 16:00 ++2d>" "<2024-02-29 Thu ++1w>" "<2026-11-01 Sun ++1w>"
                      "<2020-01-31 Fri ++1m>" "<2001-06-15 Fri ++1y>"))
        (is string= (stepped text) (repeated text) "~a" text))
      (let ((start (get-internal-real-time))
            (ts (tuatara.defaults::org-ts-repeat (tuatara.defaults::org-read-ts "<1900-01-01 Mon 00:00 ++1h>") now)))
        (true (< (- (get-internal-real-time) start) (* 1/10 internal-time-units-per-second)))
        (true (< now (tuatara.defaults::org-ts-time ts) (+ now 3601)) "~a" (tuatara.defaults::org-ts-text ts))))))

(define-test (org warning-of-any-length)
  ;; a warning too long to mean anything: read as days still, never an error that stops the agenda
  (flet ((warning (text) (tuatara.defaults::org-warning-days (tuatara.defaults::org-read-ts text))))
    (is = 3 (warning "<2026-10-20 Tue -3d>"))
    (is = 60 (warning "<2026-10-20 Tue -2m>"))
    (is = 1 (warning "<2026-10-20 Tue -36h>"))
    (false (warning "<2026-10-20 Tue +1w>"))
    (true (< 1000000 (warning (format nil "<2026-10-20 Tue -~am>" (make-string 400 :initial-element #\9)))))))

(define-test (org dates-by-rule)
  ;; <%%(diary-float t 1 1)>, the first Monday of every month, as Emacs's Org writes it:
  ;; read as data, never run, and listed, done and asked for in words as a repeat is
  (flet ((days (text from count)
           (let ((rule (tuatara.defaults::org-read-rule text)) (day from))
             (loop repeat count
                   for next = (tuatara.defaults::org-rule-next rule day)
                   while next collect (tuatara.dates:format-day "%F" next) do (setf day (1+ next)))))
         (at (y m d &optional (h 18)) (encode-universal-time 0 0 h d m y)))
    (let ((today (tuatara.dates:day-number 2026 10 23)))
      (is equal '("2026-11-02" "2026-12-07") (days "<%%(diary-float t 1 1) 10:00>" today 2))
      (is equal '("2026-10-30" "2027-01-29") (days "<%%(diary-float '(1 4 7 10) 5 -1) 14:00-15:00>" today 2))
      (is equal '("2026-10-29" "2026-11-12") (days "<%%(diary-cyclic 14 2026 10 1)>" today 2))
      ;; Emacs's own order, month, day, year, read too
      (is equal '("2026-10-29") (days "<%%(diary-cyclic 14 10 1 2026)>" today 1))
      (is equal '("2026-10-23" "2026-10-24" "2026-10-25") (days "<%%(diary-block 2026 10 20 2026 10 25)>" today 5))
      (is equal '("2027-06-30") (days "<%%(diary-anniversary 6 30 1990)>" today 1))
      (is equal '("2026-11-01" "2026-12-01") (days "<%%(diary-date t 1 t)>" today 2))
      ;; a fifth weekday only in the months that have one; counted from a day of the month
      (is equal '("2026-12-29" "2027-03-30") (days "<%%(diary-float t 2 5)>" today 2))
      (is equal '("2026-11-09") (days "<%%(diary-float t 1 1 8)>" today 1))
      ;; and: the days all its forms take, looked for within its range of days only
      (is equal '("2026-11-02" "2026-11-09") (days "<%%(and (diary-block 2026 11 1 2026 11 10) (memq (calendar-day-of-week date) '(1)))>" today 5))
      (is equal '() (days "<%%(and (diary-block 2020 1 1 2020 2 1) (diary-date t 1 t))>" today 3))
      ;; days of the week listed, in the one form of Lisp read as data
      (is equal '("2026-10-23" "2026-10-26" "2026-10-27" "2026-10-28" "2026-10-29" "2026-10-30" "2026-11-02")
          (days "<%%(memq (calendar-day-of-week date) '(1 2 3 4 5)) 09:00>" today 7)))
    ;; anything else is not read, and nothing of it is run
    (dolist (text '("<%%(shell-command \"touch /tmp/x\")>" "<%%((lambda () 1))>" "<%%(diary-float t 9 1)>"
                    "<%%(diary-float t 1 1) 25:00>" "<%%(diary-float #.(quit) 1 1)>" "<%%(diary-cyclic 0 2026 1 1)>"
                    "<%%(memq (calendar-day-of-week date) '(1 9))>" "<%%(memq (calendar-day-of-week now) '(1 2))>"
                    "<%%(memq (delete-file \"x\") '(1 2))>"))
      (false (tuatara.defaults::org-read-rule text) "~a" text))
    ;; a repeat in words: a repeater from today, or a rule, the months from the next time it comes
    (let ((now (at 2026 10 23 12)))
      (flet ((words (input) (let ((v (tuatara.defaults::org-words-repeat input now)))
                              (if (stringp v) v (and v (tuatara.defaults::org-ts-text v))))))
        (is equal "<%%(diary-float t 1 1) 10:00>" (words "first monday every month 10:00"))
        (is equal "<%%(diary-float '(1 4 7 10) 5 -1) 14:00-15:00>" (words "last friday of every 3 months, 14:00-15:00"))
        (is equal "<%%(diary-float '(1 3 5 7 9 11) 3 3)>" (words "third wed every 2 months"))
        (is equal "<2026-10-23 Fri 10:00 +1w>" (words "every week 10:00"))
        (is equal "<2026-10-23 Fri +2d>" (words "every second day"))
        (is equal "<2026-10-23 Fri 08:00 +1d>" (words "everyday 8:00"))
        (is equal "<2026-10-23 Fri 20:00 +1d>" (words "daily 20:00"))
        (is equal "<2026-10-23 Fri 09:30 +2w>" (words "every other friday 9:30"))
        (is equal "<2026-10-26 Mon +1w>" (words "every monday"))
        (is equal "<%%(memq (calendar-day-of-week date) '(1 2 3 4 5)) 09:00>" (words "every weekday 9:00"))
        (is equal "<%%(memq (calendar-day-of-week date) '(1 2 3 4 5)) 08:30-09:00>" (words "every monday to friday 8:30-9:00"))
        (is equal "<%%(memq (calendar-day-of-week date) '(1 3 5))>" (words "every mon, wed and fri"))
        (is equal "<%%(memq (calendar-day-of-week date) '(0 6))>" (words "every weekend"))
        (is equal "<%%(memq (calendar-day-of-week date) '(0 1 5 6))>" (words "every friday to monday"))
        ;; from and to: a repeat between two dates, the time wherever it is said
        (is equal "<%%(and (diary-block 2026 11 2 2026 12 21) (diary-cyclic 7 2026 11 2)) 10:00>"
            (words "every monday from nov 2 to dec 21 10:00"))
        (is equal "<%%(and (diary-block 2026 11 2 2026 11 13) (memq (calendar-day-of-week date) '(1 2 3 4 5))) 09:00>"
            (words "every weekday 9:00 from 2026-11-02 until 2026-11-13"))
        (is equal "<%%(and (diary-block 2026 10 23 2026 12 21) (diary-cyclic 7 2026 10 26))>" (words "every monday until dec 21"))
        (is equal "<2026-11-02 Mon +1w>" (words "every monday from nov 2"))
        (false (words "every monday from dec 21 to nov 2"))
        (false (words "first monday every month from jan 1"))
        (false (words "every other mon and fri"))
        (false (words "last friday every 5 months"))
        (false (words "monday"))))
    ;; done: the day it was for kept in LAST_REPEAT, the rule and the keyword as they were
    (let ((task (format nil "* TODO Team meeting~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%Notes~%")))
      (let ((tuatara.defaults::*org-now* (at 2026 11 2)))
        (setf task (org-text-after "C-c C-t" task))
        (is string= (format nil "* TODO Team meeting~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%:PROPERTIES:~%:LAST_REPEAT: [2026-11-02 Mon 10:00]~%:END:~%- State \"DONE\"       from \"TODO\"       [2026-11-02 Mon 18:00]~%Notes~%")
            task))
      ;; done before the next comes: that one; long after, the missed ones passed over
      (let ((tuatara.defaults::*org-now* (at 2026 11 3)))
        (setf task (org-text-after "C-c C-t" task))
        (true (search ":LAST_REPEAT: [2026-12-07 Mon 10:00]" task)))
      (let ((tuatara.defaults::*org-now* (at 2027 2 10)))
        (setf task (org-text-after "C-c C-t" task))
        (true (search ":LAST_REPEAT: [2027-02-01 Mon 10:00]" task))))
    ;; asked for in words, or typed whole
    (let ((tuatara.defaults::*org-now* (at 2026 10 23 12)))
      (is string= (format nil "* TODO Meet~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%")
          (org-text-after (format nil "C-c C-s ~a RET" (typed-keys "first monday every month 10:00")) (format nil "* TODO Meet~%")))
      (is string= (format nil "* TODO Run~%SCHEDULED: <2026-10-23 Fri 09:30 +2w>~%")
          (org-text-after (format nil "C-c C-s ~a RET" (typed-keys "every other friday 9:30")) (format nil "* TODO Run~%")))
      (is string= (format nil "* TODO Water~%SCHEDULED: <%%(diary-cyclic 2 2026 10 1)>~%")
          (org-text-after (format nil "C-c C-s ~a RET" (typed-keys "<%%(diary-cyclic 2 2026 10 1)>")) (format nil "* TODO Water~%")))
      ;; the calendar's day written after from, and after to, in place of what was typed there
      (is string= (format nil "* TODO Swim~%SCHEDULED: <%%(and (diary-block 2026 10 26 2026 12 14) (diary-cyclic 7 2026 10 26))>~%")
          (org-text-after (format nil "C-c C-s ~a S-<right> S-<right> S-<right> ~a S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> RET"
                                  (typed-keys "every monday from ") (typed-keys " to "))
                          (format nil "* TODO Swim~%")))
      (is equal 18 (tuatara.defaults::org-repeat-date-place "every monday from "))
      (false (tuatara.defaults::org-repeat-date-place "every monday to friday"))
      ;; the journal's date question takes dates only
      (true (null (tuatara.defaults::org-words-repeat "monday" (at 2026 10 23)))))
    ;; the agenda: a timestamp by rule on each of its days; a task by rule never done not past
    ;; due, done before past due from the first day after; a deadline warned of as it nears
    (uiop:with-temporary-file (:pathname path :type "org")
      (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
        (format o "* TODO Team meeting~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%~
                   * TODO Done before~%SCHEDULED: <%%(diary-float t 1 1)>~%:PROPERTIES:~%:LAST_REPEAT: [2026-09-07 Mon]~%:END:~%~
                   * TODO Quarterly review~%DEADLINE: <%%(diary-float '(1 4 7 10) 5 -1) 14:00>~%~
                   * Standup~%<%%(diary-cyclic 2 2026 10 1) 09:00>~%~
                   * DONE Finished~%SCHEDULED: <%%(diary-float t 1 1)>~%"))
      (let ((tuatara.defaults::*org-now* (at 2026 10 22 12))
            (files (list (namestring path))))
        (flet ((rows (day count)
                 (mapcar (lambda (row) (subseq row 0 6))
                         (org-agenda-test-rows (tuatara.defaults::org-agenda-span (tuatara.dates:day-number 2026 10 day) count :files files :log nil)))))
          (is equal '(("2026-10-05" "timestamp" "" 900 nil "Standup")
                      ("2026-10-05" "scheduled" "Scheduled:" 1000 "TODO" "Team meeting")
                      ("2026-10-05" "scheduled" "Scheduled:" nil "TODO" "Done before"))
              (rows 5 1))
          (is equal '(("2026-10-22" "upcoming-deadline" "In   8 d.:" 1400 "TODO" "Quarterly review")
                      ("2026-10-22" "past-scheduled" "Sched.17x:" nil "TODO" "Done before"))
              (rows 22 1))
          (is equal '(("2026-10-30" "deadline" "Deadline:" 1400 "TODO" "Quarterly review"))
              (remove "Standup" (rows 30 1) :key #'sixth :test #'string=)))))))

(define-test (org notify-tagged-entries)
  ;; entries tagged :notify: told on the desktop: before their time and at it, a date alone
  ;; in the morning, each once, a task done never; by the program, given -- before the title
  (uiop:with-temporary-file (:pathname path :type "org")
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o "* TODO Dentist :notify:~%SCHEDULED: <2026-10-23 Fri 14:00>~%~
                 * Team meeting :notify:~%:PROPERTIES:~%:NOTIFY_BEFORE: 30~%:END:~%<%%(diary-float t 5 4) 10:00>~%~
                 * TODO Pay rent :notify:~%DEADLINE: <2026-10-23 Fri>~%~
                 * DONE Done already :notify:~%SCHEDULED: <2026-10-23 Fri 14:00>~%~
                 * Not tagged~%<2026-10-23 Fri 14:00>~%~
                 * -rf / :notify:~%<2026-10-23 Fri 16:00>~%~
                 * Medication :notify:~%<2026-10-20 Tue 08:00 +1d>~%<2026-10-20 Tue 20:00 +1d>~%~
                 * TODO Tablet :notify:~%SCHEDULED: <2026-10-21 Wed 07:30 ++1d>~%~
                 * TODO Left undone :notify:~%SCHEDULED: <2026-10-21 Wed 07:30>~%"))
    (let ((files (list (namestring path))))
      (flet ((due (h m)
               (let* ((now (encode-universal-time 0 m h 23 10 2026))
                      (tuatara.defaults::*org-now* now))
                 (mapcar (lambda (d) (list (second d) (third d)))
                         (tuatara.defaults::org-notify-due
                          (tuatara.defaults::org-agenda-day (tuatara.defaults::org-today now) :files files :log nil) now)))))
        (clrhash tuatara.defaults::**org-notify-sent**)
        (let ((tuatara.defaults::*org-notify-before* 10) (tuatara.defaults::*org-notify-day-time* "08:00"))
          ;; before the morning: nothing
          (is equal '() (due 7 0))
          ;; the morning: the deadline of a date alone; the fourth Friday's meeting 30 minutes before
          ;; the words: when, what kind, and the category, here the file's name
          (true (assoc "Pay rent" (due 8 0) :test #'string=))
          (false (assoc "Pay rent" (due 7 59) :test #'string=))
          (true (uiop:string-prefix-p "Today · Deadline · " (second (assoc "Pay rent" (due 8 0) :test #'string=))))
          (true (uiop:string-prefix-p "10:00, in 25 min" (second (assoc "Team meeting" (due 9 35) :test #'string=))))
          ;; ten minutes before, and at the time, not the one done nor the one not tagged
          (is equal '("Dentist") (mapcar #'first (remove-if-not (lambda (d) (search "14:00" (second d))) (due 13 52))))
          (true (search "in 8 min" (second (assoc "Dentist" (due 13 52) :test #'string=))))
          (true (search "now" (second (assoc "Dentist" (due 14 2) :test #'string=))))
          (false (assoc "Dentist" (due 14 6) :test #'string=))
          ;; a repeating task not ticked off for days is told at its time all the same; one that
          ;; does not repeat, past due, is not
          (true (search "07:30, now" (second (assoc "Tablet" (due 7 31) :test #'string=))))
          (false (assoc "Left undone" (due 7 31) :test #'string=))
          ;; one entry, two times a day, every day: each told
          (true (search "08:00, now" (second (assoc "Medication" (due 8 1) :test #'string=))))
          (let ((first (tuatara.defaults::org-notify-due
                        (tuatara.defaults::org-agenda-day (tuatara.dates:day-number 2026 10 23) :files files :log nil)
                        (encode-universal-time 0 1 8 23 10 2026))))
            (dolist (d first) (setf (gethash (first d) tuatara.defaults::**org-notify-sent**) t))
            (true (search "20:00, now" (second (assoc "Medication" (due 20 1) :test #'string=)))))
          (clrhash tuatara.defaults::**org-notify-sent**)
          ;; told once: the program run by its thread, a stand-in writing what it was given
          (uiop:with-temporary-file (:pathname out)
            (uiop:with-temporary-file (:pathname program :type "sh")
              (with-open-file (o program :direction :output :if-exists :supersede)
                (format o "#!/bin/sh~%printf '%s|' \"$@\" >> '~a'~%printf '\\n' >> '~a'~%" (namestring out) (namestring out)))
              (sb-posix:chmod (namestring program) #o700)
              (let ((tuatara.defaults::*org-notify-program* (namestring program))
                    (snapshot (tuatara.defaults::org-agenda-snapshot files))
                    (now (encode-universal-time 0 0 16 23 10 2026)))
                (clrhash tuatara.defaults::**org-notify-sent**)
                (tuatara.defaults::org-notify-work snapshot now)
                (tuatara.defaults::org-notify-work snapshot (+ now 30))
                (loop repeat 100 until (search "-rf /" (uiop:read-file-string out)) do (sleep 0.05))
                (let ((lines (remove "" (uiop:split-string (uiop:read-file-string out) :separator '(#\Newline)) :test #'string=)))
                  (is = 1 (length lines) "told once: ~s" lines)
                  (true (uiop:string-prefix-p "--app-name=Tuatara|--|-rf /|16:00, now · " (first lines)) "~s" lines)))
              ;; a program not there is named, nothing told
              (let ((tuatara.defaults::*org-notify-program* "no-such-notifier-here"))
                (clrhash tuatara.defaults::**org-notify-sent**)
                (is equal "no-such-notifier-here"
                    (tuatara.defaults::org-notify-work (tuatara.defaults::org-agenda-snapshot files)
                                                       (encode-universal-time 0 0 16 23 10 2026))))))
          (clrhash tuatara.defaults::**org-notify-sent**))))))

(define-test (org repeat-menu)
  ;; C-r at the date prompt: the menu of a repeat, started from what was typed, RET writing
  ;; it where the prompt would have; what it comes to under it
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 23 10 2026)))
    (flet ((written (keys) (org-text-after keys (format nil "* TODO Thing~%"))))
      (is string= (format nil "* TODO Thing~%SCHEDULED: <2026-10-23 Fri +1w>~%")
          (written (format nil "C-c C-s C-r r ~a RET RET" (typed-keys "Weekly"))))
      ;; days of the week switched on, after six times: its sixth day the end
      (is string= (format nil "* TODO Thing~%SCHEDULED: <%%(and (diary-block 2026 10 23 2026 11 4) (memq (calendar-day-of-week date) '(1 3 5)))>~%")
          (written (format nil "C-c C-s C-r r ~a RET 1 3 5 e ~a RET ~a RET RET" (typed-keys "Weekly") (typed-keys "After") (typed-keys "6"))))
      ;; started from a day and time typed; every two weeks on two days
      (is string= (format nil "* TODO Thing~%SCHEDULED: <%%(or (diary-cyclic 14 2026 11 2) (diary-cyclic 14 2026 11 4)) 18:00>~%")
          (written (format nil "C-c C-s ~a C-r r ~a RET n ~a RET 1 3 RET" (typed-keys "nov 2 18:00") (typed-keys "Weekly") (typed-keys "2"))))
      ;; the choices named from the start: the last Friday, until a day
      (is string= (format nil "* TODO Thing~%SCHEDULED: <%%(and (diary-block 2026 10 30 2027 10 31) (diary-float t 5 -1))>~%")
          (written (format nil "C-c C-s ~a C-r r ~a RET e ~a RET ~a RET RET" (typed-keys "oct 30") (typed-keys "last") (typed-keys "On a") (typed-keys "2027-10-31"))))
      (is string= (format nil "* TODO Thing~%SCHEDULED: <2026-10-23 Fri 08:00 +1d>~%")
          (written (format nil "C-c C-s C-r t ~a RET r ~a RET RET" (typed-keys "08:00") (typed-keys "Daily"))))
      ;; C-g leaves it, nothing written
      (is string= (format nil "* TODO Thing~%") (written "C-c C-s C-r C-g")))
    (flet ((summary (&rest arguments) (tuatara.defaults::repeat-summary arguments)))
      (is equal '("Every 2 weeks on Monday and Wednesday, 18:00, from Mon 2 Nov 2026"
                  "Writes  <%%(or (diary-cyclic 14 2026 11 2) (diary-cyclic 14 2026 11 4)) 18:00>"
                  "Next    Mon 2 Nov · Wed 4 Nov · Mon 16 Nov · Wed 18 Nov · Mon 30 Nov")
          (summary "start=2026-11-02" "time=18:00" "repeats=weekly" "every=2" "mon" "wed"))
      (is equal "Every 3 months on the fourth Friday, from Fri 23 Oct 2026"
          (first (summary "start=2026-10-23" "repeats=monthly-nth" "every=3")))
      (is equal "Next    Fri 23 Oct · Mon 23 Oct 2028 · Wed 23 Oct 2030 · Sat 23 Oct 2032"
          (third (summary "start=2026-10-23" "repeats=yearly" "every=2" "ends=2032-12-31")))
      ;; what cannot be written says why
      (is equal '("Cannot be written: Every 5 months has no end date: 12 must be a multiple of it")
          (summary "start=2026-10-23" "repeats=monthly-day" "every=5" "ends=2027-10-31"))
      (is equal '("Cannot be written: It ends before it starts")
          (summary "start=2026-10-23" "repeats=daily" "ends=2026-10-01")))
    ;; the choices named from the day it starts on
    (is equal '("Does not repeat" "Daily" "Every weekday, Monday to Friday" "Weekly on Friday" "Monthly on day 30"
                "Monthly on the last Friday" "Yearly on 30 October")
        (mapcar #'second (tuatara.defaults::repeat-kinds (tuatara.dates:day-number 2026 10 30))))))

(define-test (org c-c-hints-line-up)
  ;; C-c on a heading: what acts on it under its title, the rest under it, the columns at the
  ;; same places in both; the prefixes under it named
  (let* ((f (run-keys-in-turns (list "M-x o r g - m o d e RET C-c" "") :text (format nil "* TODO Thing~%") :width 110 :height 30))
         (rows (loop for i below 30 collect (tuatara.frontend.fake:fake-screen-line f i)))
         (deadline (find-if (lambda (row) (search "C-d org-deadline" row)) rows))
         (formula (find-if (lambda (row) (search "= org-table-eval-formula" row)) rows)))
    (true (and deadline formula))
    (is = (search "C-d org-deadline" deadline) (search "= org-table-eval-formula" formula))
    (true (find-if (lambda (row) (search "C-x +clock, blocks and more" row)) rows))
    (true (find-if (lambda (row) (search "C-v +code files" row)) rows))))

(define-test (org rules-worked-out-once-per-agenda)
  ;; a task by rule: when it is open is worked out once for an agenda of many days, not each day
  (uiop:with-temporary-file (:pathname path :type "org")
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o "* TODO Meeting~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%~
                 * TODO Review~%DEADLINE: <%%(diary-float '(1 4 7 10) 5 -1)>~%~
                 * TODO Water~%SCHEDULED: <%%(diary-cyclic 3 2026 10 1)>~%"))
    (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 22 10 2026))
          (files (list (namestring path)))
          (calls 0))
      (sb-int:encapsulate 'tuatara.defaults::org-rule-next 'counted
                          (lambda (f &rest arguments) (incf calls) (apply f arguments)))
      (unwind-protect
           (let ((rows (org-agenda-test-rows (tuatara.defaults::org-agenda-span (tuatara.dates:day-number 2026 10 1) 60
                                                                                :files files :log nil))))
             (is equal '("2026-10-05" "scheduled" "Scheduled:" 1000 "TODO" "Meeting")
                 (subseq (find "Meeting" rows :key #'sixth :test #'string=) 0 6))
             (true (find "Review" rows :key #'sixth :test #'string=))
             (true (<= calls 3) "~d calls for 3 rules over 60 days" calls))
        (sb-int:unencapsulate 'tuatara.defaults::org-rule-next 'counted)))))

(defun least-consed (function &optional (runs 3))
  "The fewest bytes FUNCTION, of no arguments, conses in RUNS calls."
  (loop repeat runs minimize (nth-value 2 (tuatara.bench::measuring (funcall function)))))

(define-test (org rules-read-without-garbage)
  ;; a plain timestamp is no rule, seen at once; a rule's pattern is made once, not at each read
  (is = 0 (least-consed (lambda () (tuatara.defaults::org-read-rule "<2026-10-05 Mon 09:00>"))))
  (let ((consed (least-consed (lambda () (tuatara.defaults::org-read-rule "<%%(diary-float t 1 1) 10:00>")))))
    (true (< consed 4000) "~d bytes to read a rule" consed)))

(define-test (org agenda-times-of-day-read-one-way)
  ;; the agenda, timestamps and rules read a time of day as a date question does
  (is equal '(1530 "3:30pm" nil) (multiple-value-list (tuatara.defaults::org-time-of-day "Call 3:30pm")))
  (is equal '(930 "9:30-10:00" 1000) (multiple-value-list (tuatara.defaults::org-time-of-day "<2026-10-05 Mon 9:30-10:00>")))
  (false (tuatara.defaults::org-time-of-day "25:00"))
  (false (tuatara.defaults::org-time-of-day "13pm"))
  (false (tuatara.defaults::org-read-rule "<%%(diary-float t 1 1) 10:00-25:00>"))
  (false (tuatara.defaults::org-read-ts "<2026-10-05 Mon 25:00>"))
  (is string= "<2026-10-05 Mon 22:00-24:00>" (tuatara.defaults::org-ts-text (tuatara.defaults::org-read-ts "<2026-10-05 Mon 22:00-24:00>")))
  ;; a title's time in the afternoon sorts after a morning's
  (uiop:with-temporary-file (:pathname path :type "org")
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o "* Call 3:30pm~%<2026-10-05 Mon>~%* Standup~%<2026-10-05 Mon 09:00>~%"))
    (is equal '(("Standup" 900) ("Call" 1530))
        (mapcar (lambda (row) (list (sixth row) (fourth row)))
                (org-agenda-test-rows (tuatara.defaults::org-agenda-day (tuatara.dates:day-number 2026 10 5)
                                                                        :files (list (namestring path)) :log nil))))))

(define-test (org words-and-menu-write-repeats-alike)
  ;; a repeat in words with an end is written as the repeat menu writes it
  (let* ((now (encode-universal-time 0 0 12 23 10 2026))
         (tuatara.defaults::*org-now* now))
    (flet ((words (input) (let ((v (tuatara.defaults::org-words-repeat input now)))
                            (if (stringp v) v (and v (tuatara.defaults::org-ts-text v)))))
           (menu (&rest arguments) (let ((v (tuatara.defaults::repeat-value arguments)))
                                     (if (stringp v) v (and v (tuatara.defaults::org-ts-text v))))))
      (is equal "<%%(and (diary-block 2026 11 5 2027 3 1) (diary-date '(1 3 5 7 9 11) 5 t))>"
          (words "every 2 months from 2026-11-05 until 2027-03-01"))
      (is equal (menu "start=2026-11-05" "repeats=monthly-day" "every=2" "ends=2027-03-01")
          (words "every 2 months from 2026-11-05 until 2027-03-01"))
      (is equal (menu "start=2026-11-02" "time=10:00" "repeats=monthly-nth" "ends=2026-12-31")
          (words "first monday every month from 2026-11-01 to 2026-12-31 10:00"))
      (is equal (menu "start=2026-10-23" "repeats=weekly" "mon" "ends=2026-12-21") (words "every monday until dec 21"))
      ;; a weekly repeat on one day from a day before it: a repeater from that day, in words or the menu
      (is equal "<2026-10-26 Mon +1w>" (menu "start=2026-10-23" "repeats=weekly" "mon"))
      (is equal "<2026-10-26 Mon +1w>" (words "every monday")))))

(define-test (org repeat-days-long-after-the-start)
  ;; the days to come of a repeat that began long ago: found at once, not stepped to
  (let* ((today (tuatara.dates:day-number 2026 10 23))
         (daily (tuatara.defaults::org-read-ts "<2025-01-01 Wed +1d>"))
         (monthly (tuatara.defaults::org-read-ts "<2026-01-31 Sat +1m>")))
    (is equal (loop for k below 5 collect (+ today k)) (tuatara.defaults::repeat-days daily today 5))
    (is equal (mapcar (lambda (m) (tuatara.dates:day-number 2026 m 31)) '(10 11 12 13))
        (tuatara.defaults::repeat-days monthly today 4))
    (is equal (list (tuatara.dates:day-number 2025 1 1)) (tuatara.defaults::repeat-days daily 0 1))))

(define-test (org rule-days-found-by-year-and-month)
  ;; a rule's next and last days, looked for by year or month, are those a walk day by day finds
  (let ((rules (mapcar #'tuatara.defaults::org-read-rule
                       '("<%%(diary-anniversary 2 29 2000)>" "<%%(diary-anniversary 10 23)>" "<%%(diary-date 12 25 t)>"
                         "<%%(diary-date t 31 t)>" "<%%(diary-date '(1 4) 30 '(2027 2029))>" "<%%(diary-date 2 29 t)>"
                         "<%%(diary-float t 1 1)>" "<%%(diary-float '(1 4 7 10) 5 -1)>" "<%%(diary-float t 2 5)>"
                         "<%%(diary-float 2 0 5)>" "<%%(diary-float t 1 1 8)>" "<%%(diary-date 2026 t t)>"
                         "<%%(and (diary-block 2026 11 1 2027 3 1) (diary-date t 1 t))>")))
        (reach tuatara.defaults::*org-rule-reach*))
    (flet ((walk (rule day by)
             (multiple-value-bind (first last) (tuatara.defaults::org-rule-span rule)
               (if (= by 1)
                   (loop for k from (max day (or first day)) to (min (+ day reach) (or last (+ day reach)))
                         when (tuatara.defaults::org-rule-matches-p rule k) return k)
                   (loop for k downfrom (min day (or last day)) to (max (- day reach) (or first (- day reach)))
                         when (tuatara.defaults::org-rule-matches-p rule k) return k)))))
      (dolist (rule rules)
        (true rule)
        (loop for day from (tuatara.dates:day-number 2023 12 20) to (tuatara.dates:day-number 2032 3 10) by 37
              do (is eql (walk rule day 1) (tuatara.defaults::org-rule-next rule day) "next of ~a from ~d" rule day)
                 (is eql (walk rule day -1) (tuatara.defaults::org-rule-previous rule day) "last of ~a from ~d" rule day))))
    ;; a day that comes once in eight years found in few looks
    (let ((leap (tuatara.defaults::org-read-rule "<%%(diary-anniversary 2 29 2000)>")) (looks 0))
      (sb-int:encapsulate 'tuatara.defaults::org-rule-matches-p 'counted (lambda (f &rest arguments) (incf looks) (apply f arguments)))
      (unwind-protect (tuatara.defaults::org-rule-next leap (tuatara.dates:day-number 2097 3 1))
        (sb-int:unencapsulate 'tuatara.defaults::org-rule-matches-p 'counted))
      (true (< looks 20) "~d looks" looks))))

(define-test (org agenda-line-kinds-agree)
  ;; every kind of line the agenda makes is one of those named, and a kind not named is refused
  (let* ((directory (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
         (files (list (namestring (merge-pathnames "tasks.org" directory)) (namestring (merge-pathnames "notes.org" directory))))
         (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
         (items (append (tuatara.defaults::org-agenda-span (tuatara.dates:day-number 2026 9 20) 20 :files files :log t)
                        (tuatara.defaults::org-agenda-todo-list :files files)
                        (tuatara.defaults::org-agenda-match "work" :files files)))
         (kinds (remove-duplicates (mapcar #'tuatara.defaults::org-item-type items) :test #'string=)))
    (true (>= (length kinds) 9) "~s" kinds)
    (dolist (kind kinds)
      (true (member kind tuatara.defaults::*org-agenda-item-types* :test #'string=) "~s named" kind)))
  (fail (macroexpand-1 '(tuatara.defaults::org-item-type-in item "sheduled"))))

(define-test (org last-repeat-read-from-the-drawer)
  ;; when a task by rule was last done for is its property's, not a line like it in its text
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 2 11 2026)))
    (true (search ":LAST_REPEAT: [2026-11-02 Mon 10:00]"
                  (org-text-after "C-c C-t" (format nil "* TODO Team meeting~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%#+begin_example~%:LAST_REPEAT: [2027-01-04 Mon]~%#+end_example~%"))))
    (true (search ":LAST_REPEAT: [2026-12-07 Mon 10:00]"
                  (org-text-after "C-c C-t" (format nil "* TODO Team meeting~%SCHEDULED: <%%(diary-float t 1 1) 10:00>~%:PROPERTIES:~%:LAST_REPEAT: [2026-11-02 Mon 10:00]~%:END:~%"))))))

(define-test (org timestamps-written-one-way)
  ;; a timestamp of a time or a day, any year, with its time or without, written one way
  (let ((time (encode-universal-time 0 30 17 1 10 2026)) (day (tuatara.dates:day-number 2026 10 1)))
    (is string= "[2026-10-01 Thu 17:30]" (tuatara.defaults::org-timestamp-text time))
    (is string= "<2026-10-01 Thu 17:30>" (tuatara.defaults::org-timestamp-text time :active t))
    (is string= "[2026-10-01 Thu]" (tuatara.defaults::org-timestamp-text time :clock nil))
    (is string= "<2026-10-01 Thu>" (tuatara.defaults::org-timestamp-text time :active t :clock nil))
    (is string= "[2026-10-01 Thu]" (tuatara.defaults::org-day-timestamp-text day))
    (is string= "[2026-10-01 Thu 09:05]" (tuatara.defaults::org-day-timestamp-text day :hour 9 :minute 5))
    (is string= "[1899-12-31 Sun]" (tuatara.defaults::org-day-timestamp-text -1))))

(define-test (org agenda-times-from-timestamps-read)
  ;; the times, ranges and warnings of timestamps are those read with them, not looked for again in their text
  (uiop:with-temporary-file (:pathname path :type "org")
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o "* TODO Call~%DEADLINE: <2026-10-07 Wed 15:30 -3d>~%* Meet~%<2026-10-05 Mon 9:30-10:00>~%* Standup~%<%%(diary-cyclic 1 2026 10 1) 08:15>~%"))
    (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 5 10 2026)) (looks 0))
      (sb-int:encapsulate 'tuatara.dates:find-time-range 'counted (lambda (f &rest arguments) (incf looks) (apply f arguments)))
      (unwind-protect
           (let ((rows (org-agenda-test-rows (tuatara.defaults::org-agenda-span (tuatara.dates:day-number 2026 10 5) 3
                                                                                :files (list (namestring path)) :log nil))))
             (is equal '(("2026-10-05" 815 "Standup") ("2026-10-05" 930 "Meet") ("2026-10-05" nil "Call")
                         ("2026-10-06" 815 "Standup") ("2026-10-07" 815 "Standup") ("2026-10-07" 1530 "Call"))
                 (mapcar (lambda (row) (list (first row) (fourth row) (sixth row))) rows)))
        (sb-int:unencapsulate 'tuatara.dates:find-time-range 'counted))
      (is = 0 looks))))

(define-test (org entry-text-read-in-one-look)
  ;; an entry's ranges, timestamps and rules taken in one look along each line, its lines not copied
  (uiop:with-temporary-file (:pathname path :type "org")
    (with-open-file (o path :direction :output :if-exists :supersede)
      (format o "* Many~%")
      (dotimes (i 1000)
        (format o "Met <2026-10-05 Mon>--<2026-10-07 Wed> then <2026-10-~2,'0d Thu 10:00> and <%%(diary-float t 1 1)>~%" (1+ (mod i 28)))))
    (let* ((tuatara.defaults::*org-agenda-ts-memo* (make-hash-table :test 'equal))
           (entry (first (tuatara.defaults::org-agenda-entries (namestring path)))))
      (tuatara.defaults::agenda-entry-read-text entry)
      (is = 2000 (length (tuatara.defaults::agenda-entry-stamps entry)))
      (is equal '("<2026-10-05 Mon>" "<2026-10-07 Wed>") (first (tuatara.defaults::agenda-entry-ranges entry)))
      (is = 1000 (length (tuatara.defaults::agenda-entry-ranges entry)))
      (let ((per-line (floor (least-consed (lambda () (tuatara.defaults::agenda-entry-read-text entry))) 1000)))
        (true (< per-line 1700) "~d bytes a line" per-line)))))

(define-test (org keywords-and-properties-found-without-patterns-made)
  ;; a file's keyword, a drawer's property and a clock table's span read without a pattern made for each
  (let ((text (format nil "Some text~%  #+title:   A title  ~%#+FILETAGS:~%#+CATEGORYX: no~%#+category: work~%")))
    (is string= "A title" (tuatara.defaults::org-file-keyword text "TITLE"))
    (false (tuatara.defaults::org-file-keyword text "FILETAGS"))
    (is string= "work" (tuatara.defaults::org-file-keyword text "CATEGORY"))
    (false (tuatara.defaults::org-file-keyword text "ARCHIVE"))
    (true (< (least-consed (lambda () (tuatara.defaults::org-file-keyword text "CATEGORY"))) 400)))
  (let ((lines (list "SCHEDULED: <2026-10-05 Mon>" ":PROPERTIES:" ":ID: x" "  :last_repeat: [2026-10-01 Thu]" ":END:" "Body")))
    (is equal (list "SCHEDULED: <2026-10-05 Mon>" ":PROPERTIES:" ":ID: x" ":LAST_REPEAT: [2026-10-05 Mon]" ":END:" "Body")
        (tuatara.defaults::org-entry-lines-with lines "LAST_REPEAT" "[2026-10-05 Mon]"))
    (true (< (least-consed (lambda () (tuatara.defaults::org-entry-lines-with lines "LAST_REPEAT" "[2026-10-05 Mon]"))) 3000)))
  (let ((now (encode-universal-time 0 0 12 23 10 2026)))
    (is equal (list (tuatara.dates:day-universal-time (tuatara.dates:day-number 2026 10 21)))
        (subseq (multiple-value-list (tuatara.defaults::clocktable-block "today-2" now)) 0 1))
    (false (tuatara.defaults::clocktable-block "today-x" now))
    (true (< (least-consed (lambda () (tuatara.defaults::clocktable-block "thisweek" now))) 3000))))

(define-test (org notifications-look-at-disk-away-from-the-editor)
  ;; each minute's notifications take the open files on the editor's thread, and look at the disk on their own
  (uiop:with-temporary-file (:pathname path :type "org")
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o "* Nothing due :notify:~%<2030-01-01 Tue 10:00>~%"))
    (let* ((editor-thread sb-thread:*current-thread*)
           (here 0) (there 0)
           (folder (namestring (uiop:pathname-directory-pathname path)))
           (tuatara.defaults::*org-agenda-files* (list (format nil "~a*.org" folder)))
           (tuatara.defaults::*org-notify-tag* "notify")
           (kept (copy-list tuatara.defaults::**org-notify-state**)))
      (flet ((counted (f &rest arguments)
               (if (eq sb-thread:*current-thread* editor-thread) (incf here) (incf there))
               (apply f arguments)))
        (sb-int:encapsulate 'tuatara.io:path-stamp 'counted #'counted)
        (sb-int:encapsulate 'tuatara.io:directory-entries 'counted #'counted)
        (unwind-protect
             (progn
               (setf tuatara.defaults::**org-notify-state** (list nil 0 nil nil))
               (tuatara.defaults::org-notify-tick nil)
               (loop repeat 200 while (first tuatara.defaults::**org-notify-state**) do (sleep 0.02))
               (is = 0 here)
               (true (plusp there)))
          (sb-int:unencapsulate 'tuatara.io:path-stamp 'counted)
          (sb-int:unencapsulate 'tuatara.io:directory-entries 'counted)
          (setf tuatara.defaults::**org-notify-state** kept))))))

(define-test (org notify-health-said-as-others)
  ;; check-health says what is missing for desktop notices as for every outside program: what to install
  (let ((tuatara.defaults::*org-notify-program* "no-such-notify-program-here"))
    (is string= (format nil "agenda entries tagged :~a: told on the desktop by none: ~a"
                        (or tuatara.defaults::*org-notify-tag* "notify")
                        (tuatara.defaults::outside-program-install-words :notify))
        (tuatara.defaults::org-notify-health-words))))

(define-test (org agenda-files-looked-at-away)
  ;; the agenda files not open are looked at on the thread that works the agenda out, not on the
  ;; editor's: the calendar's marks and a refresh start without asking the disk anything
  (let* ((directory (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
         (tuatara.defaults::*org-agenda-files* (list (namestring directory)))
         (tuatara.defaults::*org-agenda-paths-kept* nil)
         (here sb-thread:*current-thread*)
         (looked 0))
    (sb-int:encapsulate 'tuatara.io:path-stamp 'looked
                        (lambda (function &rest arguments)
                          (when (eq sb-thread:*current-thread* here) (incf looked))
                          (apply function arguments)))
    (let ((marks (unwind-protect (tuatara.defaults::org-calendar-mark-source
                                  (tuatara.dates:day-number 2026 10 1) (tuatara.dates:day-number 2026 10 31))
                   (sb-int:unencapsulate 'tuatara.io:path-stamp 'looked))))
      (is = 0 looked)
      (true (functionp marks))
      (true (plusp (length (funcall marks)))))))
