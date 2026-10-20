;;; tools/calendar-oracle.el --- Emacs's own calendar layouts, written for Tuatara's tests.
;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;
;;; Usage: emacs -Q --batch -l tools/calendar-oracle.el OUTPUT
;;; Writes OUTPUT (test/calendar/layouts.txt): for each case below, a line
;;; "== YEAR MONTH WEEK-START" and the text calendar-generate makes of the
;;; three months around MONTH of YEAR, the week starting on WEEK-START (0
;;; Sunday, 1 Monday), then a line "==". Tuatara's test (test/calendar.lisp)
;;; lays out the same months and compares the text, character for character.

(require 'calendar)

(defvar calendar-oracle-cases
  '((2026 10 1) (2026 10 0) (2026 1 1) (2025 12 0) (2024 2 1) (2027 12 6) (1900 3 1) (2026 7 3) (1850 3 1) (1900 1 0))
  "The months laid out: (YEAR MONTH WEEK-START).")

(with-temp-file (car command-line-args-left)
  (dolist (case calendar-oracle-cases)
    (let ((calendar-week-start-day (nth 2 case)))
      (insert (format "== %d %d %d\n" (nth 0 case) (nth 1 case) (nth 2 case)))
      (insert (with-temp-buffer (setq indent-tabs-mode nil) (calendar-generate (nth 1 case) (nth 0 case)) (buffer-string)))
      (insert "\n==\n"))))

(setq command-line-args-left nil)
