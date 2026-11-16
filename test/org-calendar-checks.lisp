;;;; test/org-calendar-checks.lisp --- Week commands from a week's edge, captures that ask for a date.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-modal-test (calendar week-commands-from-the-weeks-edge)
  ;; on the week's first day C-a goes a week back, on its last C-e a week on; a count says how many
  (flet ((at (keys) (nth-value 2 (calendar-keys keys))))
    (is = (calendar-day 2026 9 21) (at "C-a C-a"))
    (is = (calendar-day 2026 9 14) (at "C-a C-a C-a"))
    (is = (calendar-day 2026 9 14) (at "C-a C-u 2 C-a"))
    (is = (calendar-day 2026 10 11) (at "C-e C-e"))
    (is = (calendar-day 2026 10 18) (at "C-e C-e C-e"))))

(define-modal-test (capture asked-date-on-a-day)
  ;; a template asking for a date with %^T gets no second date line when captured on a day
  (with-capture (dir agenda)
    (let ((directory (merge-pathnames "capture-templates/" dir)))
      (ensure-directories-exist directory)
      (alexandria:write-string-into-file
       (format nil "# name: Dated~%# key: p~%# file: ~a~%# heading: Inbox~%# --~%* Task %^T%?~%" agenda)
       (merge-pathnames "p-dated.org" directory))
      (run-keys "M-x c a l e n d a r RET l l l i p 2 0 2 6 - 0 4 - 0 1 RET C-c C-c" :text "x" :width 100 :height 30)
      (let ((text (file-text agenda)))
        (true (search "** Task <2026-04-01 Wed>" text) "~a" text)
        (false (search "<2026-03-23 Mon>" text) "~a" text)))))

(define-test (org-internals table-of-rules-only)
  ;; a table of rules alone has no columns, and the count is a number
  (is = 0 (tuatara.defaults::org-table-columns
           (tuatara.defaults::make-org-table-model :rows '(:rule :rule) :indent "" :row 0 :field 0 :offset 0)))
  (is = 3 (tuatara.defaults::org-table-columns
           (tuatara.defaults::make-org-table-model :rows '(:rule ("a" "b" "c")) :indent "" :row 0 :field 0
                                                   :offset 0))))

(define-test (org-internals indented-view-prefix-shared)
  ;; the indent of a row is the same string each time it is asked for
  (let ((buffer (tuatara.buffer:make-buffer "*prefix*" :text (format nil "* a~%body~%"))))
    (tuatara.defaults::set-major-mode buffer 'tuatara.defaults::org-mode)
    (setf (tuatara.buffer:buffer-local buffer 'org-indent) t)
    (is string= "  " (tuatara.defaults::org-line-prefix buffer 4 nil))
    (true (eq (tuatara.defaults::org-line-prefix buffer 4 nil) (tuatara.defaults::org-line-prefix buffer 4 nil)))))

(define-modal-test (calendar week-commands-counted-either-way)
  ;; a count says how many starts or ends of weeks to go past, back or on; a negative one the other way, zero
  ;; none; the same from a week's edge and from within it, and with weeks starting on another day
  (flet ((at (keys) (nth-value 2 (calendar-keys keys))))
    (is = (calendar-day 2026 10 5) (at "C-u - 1 C-a"))
    (is = (calendar-day 2026 10 12) (at "C-u - 2 C-a"))
    (is = (calendar-day 2026 10 5) (at "C-a C-u - 1 C-a"))
    (is = (calendar-day 2026 10 3) (at "C-u 0 C-a"))
    (is = (calendar-day 2026 10 11) (at "C-u 2 C-e"))
    (is = (calendar-day 2026 10 18) (at "C-e C-u 2 C-e"))
    (is = (calendar-day 2026 9 27) (at "C-u - 1 C-e"))
    (let ((tuatara-user:*calendar-week-start-day* 0))
      (is = (calendar-day 2026 9 27) (at "C-a"))
      (is = (calendar-day 2026 10 10) (at "C-e")))))
