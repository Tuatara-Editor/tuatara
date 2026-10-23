;;;; test/calendar.lisp --- The calendar: Emacs's layout, moving, marks, the agenda and Org's date prompt.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test calendar
  :description "The calendar: three months laid out as Emacs lays them out, point moved by days, weeks, months and years, the days with agenda entries marked, the agenda of a day, and the calendar Org's date prompt shows.")

(defun calendar-oracle-cases ()
  "The cases of test/calendar/layouts.txt, written by tools/calendar-oracle.el:
each (YEAR MONTH WEEK-START TEXT)."
  (let ((lines (uiop:read-file-lines (merge-pathnames "test/calendar/layouts.txt" (asdf:system-source-directory :tuatara))))
        (cases '()))
    (loop while lines
          do (let ((head (pop lines)))
               (when (uiop:string-prefix-p "== " head)
                 (destructuring-bind (year month start) (mapcar #'parse-integer (rest (uiop:split-string head)))
                   (let ((text (loop for line = (pop lines) until (string= line "==") collect line)))
                     (push (list year month start (format nil "~{~a~^~%~}" text)) cases))))))
    (nreverse cases)))

(define-test (calendar laid-out-as-emacs)
  (let ((cases (calendar-oracle-cases)))
    (is = 10 (length cases))
    (dolist (case cases)
      (destructuring-bind (year month start text) case
        (let ((tuatara-user:*calendar-week-start-day* start))
          (is string= text (values (tuatara.defaults::calendar-layout year month)) "~d-~d from ~d" year month start))))))

(define-test (calendar months-shown)
  ;; a year, three side by side: May 2026 to April 2027 around October, rows of months a blank line apart
  (let ((tuatara-user:*calendar-months* 12) (tuatara-user:*calendar-months-per-row* 3) (tuatara-user:*calendar-week-start-day* 1))
    (multiple-value-bind (text places) (tuatara.defaults::calendar-layout 2026 10)
      (let ((lines (uiop:split-string text :separator '(#\Newline))))
        (true (search "May 2026" (first lines)))
        (true (search "April 2027" text))
        (false (search "May 2027" text))
        (is = 4 (count-if (lambda (l) (search "Mo Tu We" l)) lines))
        (is = 3 (count "" lines :test #'string=))
        (is = 365 (length places)))))
  ;; one month alone; four around point: one before, two after
  (let ((tuatara-user:*calendar-months* 1))
    (multiple-value-bind (text places) (tuatara.defaults::calendar-layout 2026 10)
      (true (search "October 2026" text))
      (false (search "September" text))
      (is = 31 (length places))))
  (let ((tuatara-user:*calendar-months* 4) (tuatara-user:*calendar-months-per-row* 4))
    (let ((first (first (uiop:split-string (tuatara.defaults::calendar-layout 2026 10) :separator '(#\Newline)))))
      (true (search "September 2026" first))
      (true (search "December 2026" first))))
  ;; side by side as many as the width holds, unless set: three in 80 columns, four in 110
  (let ((tuatara-user:*calendar-months* 4) (tuatara-user:*calendar-months-per-row* nil))
    (flet ((first-line (width)
             (let ((tuatara.defaults::*calendar-width* width))
               (first (uiop:split-string (tuatara.defaults::calendar-layout 2026 10) :separator '(#\Newline))))))
      (false (search "December 2026" (first-line 80)))
      (true (search "December 2026" (first-line 110)))))
  ;; nil: as many as fit side by side, three in 80 columns, four in 110, or as many as a row is set to hold
  (let ((tuatara-user:*calendar-months* nil) (tuatara-user:*calendar-months-per-row* nil))
    (flet ((titles (width)
             (let ((tuatara.defaults::*calendar-width* width))
               (count-if (lambda (name) (search name (tuatara.defaults::calendar-layout 2026 10)))
                         '("August" "September" "October" "November" "December" "January")))))
      (is = 3 (titles 80))
      (is = 4 (titles 110))
      (let ((tuatara-user:*calendar-months-per-row* 5))
        (is = 5 (titles 80))))
    ;; C-v then scrolls four: September to December, then January to April, on February's first
    (is = (calendar-day 2027 2 1) (nth-value 2 (calendar-keys "C-v" :width 110))))
  (let ((tuatara-user:*calendar-months* 4))
    (multiple-value-bind (f editor) (calendar-keys "" :width 110)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "December 2026"))))
  ;; moving through a year's calendar: C-v scrolls as many months as are shown
  (let ((tuatara-user:*calendar-months* 12))
    (is = (calendar-day 2026 11 3) (nth-value 2 (calendar-keys "M-}")))
    (is = (calendar-day 2027 10 1) (nth-value 2 (calendar-keys "C-v")))))

(defun calendar-day (y m d)
  "The day number of Y-M-D."
  (tuatara.dates:day-number y m d))

(defun calendar-keys (keys &key (width 100) (height 24))
  "Run the calendar on 2026-10-03, KEYS typed in it (a string, or a list as
RUN-KEYS takes it); the frontend, the editor and the day point is on in the
calendar."
  (let ((old (tuatara.buffer:find-buffer "*Calendar*")))
    (when old (tuatara.buffer:kill-buffer old)))
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026)))
    (multiple-value-bind (f editor) (run-keys (if (listp keys) (cons "M-x c a l e n d a r RET" keys) (format nil "M-x c a l e n d a r RET ~a" keys))
                                                 :width width :height height)
      (let ((b (tuatara.buffer:find-buffer "*Calendar*")))
        (values f editor (and b (tuatara.defaults::calendar-date-at b)))))))

(defun wait-for-calendar-marks ()
  "Wait, a few seconds at most, till no calendar's days are being marked in the background."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (typep job 'tuatara.defaults::calendar-marks) (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(define-test (calendar calendar-moving)
  (flet ((at (keys) (nth-value 2 (calendar-keys keys))))
    (is = (calendar-day 2026 10 3) (at ""))
    (is = (calendar-day 2026 10 4) (at "C-f"))
    (is = (calendar-day 2026 10 2) (at "C-b"))
    (is = (calendar-day 2026 10 6) (at "C-u 3 C-f"))
    (is = (calendar-day 2026 10 10) (at "C-n"))
    (is = (calendar-day 2026 9 26) (at "C-p"))
    ;; the modal keys: h l a day, j k a week, J K a month, H L scroll, g h today, g l back as cd -
    (is = (calendar-day 2026 10 11) (at "l j"))
    (is = (calendar-day 2026 9 25) (at "h k"))
    (is = (calendar-day 2026 9 3) (at "J"))
    (is = (calendar-day 2026 11 3) (at "K"))
    (is = (calendar-day 2026 10 3) (at "L"))
    (is = (calendar-day 2026 12 1) (at "L L L"))
    (is = (calendar-day 2026 10 3) (at "K K g h"))
    (is = (calendar-day 2026 12 3) (at "K K g h g l"))
    (is = (calendar-day 2026 10 3) (at "K K g h g l g l"))
    (is = (calendar-day 2026 11 3) (at "M-}"))
    (is = (calendar-day 2026 9 3) (at "M-{"))
    (is = (calendar-day 2027 10 3) (at "C-x ]"))
    (is = (calendar-day 2025 10 3) (at "C-x ["))
    ;; the month's last day when the next is shorter
    (is = (calendar-day 2026 11 30) (at "M-e M-}"))
    (is = (calendar-day 2026 9 28) (at "C-a"))
    (is = (calendar-day 2026 10 4) (at "C-e"))
    (is = (calendar-day 2026 10 1) (at "M-a"))
    (is = (calendar-day 2026 10 31) (at "M-e"))
    (is = (calendar-day 2026 1 1) (at "M-<"))
    (is = (calendar-day 2026 12 31) (at "M->"))
    ;; scrolling keeps the day while it is shown, else today, else the middle month's first
    (is = (calendar-day 2026 10 3) (at ">"))
    (is = (calendar-day 2027 1 1) (at "C-v"))
    (is = (calendar-day 2026 10 3) (at "C-v M-v"))
    (is = (calendar-day 2026 10 3) (at "C-x ] ."))
    (is = (calendar-day 2026 12 24) (at "g d 2 0 2 6 - 1 2 - 2 4 RET"))
    (is = (calendar-day 2027 3 1) (at "o m a r SPC 2 0 2 7 RET"))
    ;; a month as you write it: jan '26, jan 27, january 2027, jan alone, 2027-03, a date's month
    (is = (calendar-day 2026 1 1) (at "o J a n SPC ' 2 6 RET"))
    (is = (calendar-day 2027 1 1) (at "o j a n SPC 2 7 RET"))
    (is = (calendar-day 2027 1 1) (at "o j a n u a r y SPC 2 0 2 7 RET"))
    (is = (calendar-day 2026 1 1) (at "o j a n RET"))
    (is = (calendar-day 2027 3 1) (at "o 2 0 2 7 - 0 3 RET"))
    (is = (calendar-day 2027 3 1) (at "o 2 0 2 7 - 0 3 - 1 4 RET")))
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026)))
    (is = 1999 (tuatara.defaults::calendar-two-digit-year "99"))
    (is = 2075 (tuatara.defaults::calendar-two-digit-year "75"))
    (is = 2000 (tuatara.defaults::calendar-two-digit-year "00")))
  ;; no other NORMAL keys: v selects nothing, x deletes nothing; y copies what % selects
  (multiple-value-bind (f editor) (calendar-keys "v l")
    (declare (ignore f editor))
    (false (tuatara.buffer:buffer-mark-active-p (tuatara.buffer:find-buffer "*Calendar*"))))
  (let ((editor (nth-value 1 (calendar-keys "% y"))))
    (true (search "October 2026" (or (first (tuatara.command:editor-kill-ring editor)) ""))))
  ;; with the modal editing off, h is not the calendar's (Emacs's calendar has it for holidays)
  (let ((tuatara-user:*modal-editing* nil))
    (is = (calendar-day 2026 10 3) (nth-value 2 (calendar-keys "h"))))
  ;; the week from Sunday
  (let ((tuatara-user:*calendar-week-start-day* 0))
    (is = (calendar-day 2026 9 27) (nth-value 2 (calendar-keys "C-a"))))
  ;; what it says, and its mode line
  (multiple-value-bind (f editor) (calendar-keys "p d")
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Day 276 of 2026; 89 days remaining in the year"))
    (true (tuatara.frontend.fake:fake-frame-showing f "Sat 2026-10-03, week 40")))
  (true (tuatara.frontend.fake:fake-frame-showing (calendar-keys "p c") "ISO date: Day 6 of week 40 of 2026"))
  ;; the day at point on its bright ground, and only it; after a move, the day moved to
  (flet ((selected (f)
           (loop for row below 24
                 append (loop for (text face) in (org-row-faces f row)
                              when (member :calendar-selected (if (listp face) face (list face)))
                                collect text))))
    (is equal '(" 3") (selected (calendar-keys "")))
    (is equal '(" 4") (selected (calendar-keys "C-f"))))
  ;; on the cursor's colour, as the support state's cursor has it; reversed when it has the text's
  (let ((tuatara-user:*tuatara-support-cursor-colour* "#ff0000"))
    (calendar-keys "")
    (is equal "#ff0000" (tuatara.display:face-attribute :calendar-selected :bg))
    (false (tuatara.display:face-attribute :calendar-selected :reverse)))
  ;; a cursor of the text's colour: the text's colour behind, the ground's for the digits, given outright
  ;; so that a marked day's blue does not show through half of it
  (let ((tuatara-user:*tuatara-support-cursor-colour* nil) (tuatara-user:*cursor-colour* nil))
    (calendar-keys "")
    (false (tuatara.display:face-attribute :calendar-selected :reverse))
    (is equal (or (tuatara.display:face-attribute :default :fg) :white) (tuatara.display:face-attribute :calendar-selected :bg))
    (is equal (or (tuatara.display:face-attribute :default :bg) :black) (tuatara.display:face-attribute :calendar-selected :fg)))
  ;; shown below, the months in it; q closes it
  (multiple-value-bind (f editor) (calendar-keys "")
    (true (tuatara.frontend.fake:fake-frame-showing f "September 2026            October 2026"))
    (is = 2 (length (tuatara.command:editor-windows editor)))
    ;; as tall as its eight lines, as your Emacs fits it
    (is = 8 (tuatara.display:window-height (find "*Calendar*" (tuatara.command:editor-windows editor)
                                                  :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w)))
                                                  :test #'string=))))
  (is = 1 (length (tuatara.command:editor-windows (nth-value 1 (calendar-keys "q")))))
  ;; SPC c below, SPC C in the window you are in, q bringing its buffer back
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026)))
    (let ((editor (nth-value 1 (run-keys "SPC c" :width 100 :height 24))))
      (is = 2 (length (tuatara.command:editor-windows editor))))
    (let ((editor (nth-value 1 (run-keys "SPC C" :width 100 :height 24))))
      (is = 1 (length (tuatara.command:editor-windows editor)))
      (is string= "*Calendar*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
    (let ((editor (nth-value 1 (run-keys "SPC C q" :width 100 :height 24))))
      (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))))

(define-test (calendar with-the-agenda)
  (with-test-directory (dir)
    (alexandria:write-string-into-file
     (format nil "* TODO Dentist~%SCHEDULED: <2026-10-05 Mon>~%* Meeting <2026-11-12 Thu>~%")
     (merge-pathnames "tasks.org" dir))
    (let ((tuatara-user:*org-agenda-files* (list (namestring dir))))
      ;; the days with entries marked, worked out in the background
      (calendar-keys "")
      (let ((marked (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*Calendar*") 'tuatara.defaults::calendar-marked)))
        (true marked)
        (true (and marked (gethash (calendar-day 2026 10 5) marked)))
        (true (and marked (gethash (calendar-day 2026 11 12) marked)))
        (false (and marked (gethash (calendar-day 2026 10 4) marked))))
      ;; u takes the marks off
      (calendar-keys "u")
      (false (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*Calendar*") 'tuatara.defaults::calendar-marked))
      ;; a shows the agenda of the day's week, point on the day
      (multiple-value-bind (f editor) (calendar-keys "C-f C-f a")
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Dentist"))
        (let ((agenda (tuatara.buffer:find-buffer "*Org Agenda*")))
          (true agenda)
          (when agenda
            (is = (calendar-day 2026 10 5) (tuatara.defaults::org-agenda-day-at-point agenda)))))
      ;; c in the agenda shows the calendar on the day at point
      (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026)))
        (run-keys "M-x o r g - a g e n d a - l i s t RET c" :width 100 :height 30)
        (is = (calendar-day 2026 10 3) (tuatara.defaults::calendar-date-at (tuatara.buffer:find-buffer "*Calendar*")))))))

(define-test (calendar in-the-date-prompt)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 3 10 2026)))
    (flet ((scheduled (keys)
             (multiple-value-bind (f editor)
                 (run-keys (format nil "M-x o r g - m o d e RET C-c C-s ~a" keys) :text (format nil "* Task~%") :width 100 :height 24)
               (values (session-text editor) f editor))))
      ;; S-<right> twice: two days on, written at the prompt
      (multiple-value-bind (text f editor) (scheduled "S-<right> S-<right> RET")
        (true (search "SCHEDULED: <2026-10-05 Mon>" text))
        ;; the calendar was shown while it asked, and goes with the question
        (true (tuatara.frontend.fake:fake-frame-showing f "October 2026"))
        (is = 1 (length (tuatara.command:editor-windows editor))))
      ;; as it asks, the day at point is marked by the calendar alone, one look across,
      ;; with no cursor of the window drawn over one of its digits
      (multiple-value-bind (cursor screen spans)
          (cursor-at-question '("M-x o r g - m o d e RET C-c C-s" :look "C-g") :text (format nil "* Task~%") :width 100 :height 24)
        (declare (ignore cursor))
        (flet ((with-face (row face)
                 (some (lambda (span) (let ((faces (third span))) (if (listp faces) (member face faces) (eq faces face)))) row)))
          (let ((row (position-if (lambda (row) (with-face row :calendar-selected)) spans)))
            (true row)
            (true (search " 3 " (aref screen row)))
            (false (with-face (aref spans row) :cursor-inactive)))))
      ;; a time typed stays; a week, a month and a year on
      (true (search "SCHEDULED: <2026-10-10 Sat 14:00>" (scheduled "1 4 : 0 0 S-<down> RET")))
      (true (search "SCHEDULED: <2026-11-03 Tue>" (scheduled "M-S-<right> RET")))
      (true (search "SCHEDULED: <2027-10-03 Sun>" (scheduled "M-S-<down> RET")))
      ;; what is typed moves the calendar, and the keys go on from there
      (true (search "SCHEDULED: <2026-10-10 Sat>" (scheduled "f r i S-<right> RET")))
      ;; C-. back to today
      (true (search "SCHEDULED: <2026-10-03 Sat>" (scheduled "S-<right> C-. RET")))
      ;; read as every date asked for is read
      (true (search "SCHEDULED: <2026-10-04 Sun 15:00>" (scheduled "t o m o r r o w SPC 3 p m RET")))
      ;; off: no calendar
      (let ((tuatara-user:*org-read-date-popup-calendar* nil))
        (multiple-value-bind (text f) (scheduled "+ 2 d RET")
          (true (search "SCHEDULED: <2026-10-05 Mon>" text))
          (false (tuatara.frontend.fake:fake-frame-showing f "October 2026")))))))
