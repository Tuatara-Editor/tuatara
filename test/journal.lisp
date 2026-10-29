;;;; test/journal.lisp --- The journal: entries opened or made as Denote's denote-journal makes them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test journal
  :description "The journal: SPC d j opens the day's entry or makes it, named, titled and headed as denote-journal makes it and saved at once; a week's, a month's or a year's entries; plain names instead; the calendar marks the days with entries and n there opens one.")

(defparameter *journal-moment* (encode-universal-time 58 39 13 20 3 2026)
  "Friday 20 March 2026, 13:39:58, when the journal's tests run.")

(defmacro with-journal ((directory) &body body)
  "Run BODY at *JOURNAL-MOMENT*, DIRECTORY a new empty directory the formatted
files' one, the journal in journal/ there, every setting as standard; the
buffers of its files killed after."
  `(with-test-directory (,directory)
     (let ((tuatara-user:*formatted-file-directory* (namestring ,directory))
           (tuatara-user:*journal-directory* nil) (tuatara-user:*journal-keyword* "journal")
           (tuatara-user:*journal-title-format* :day-date-month-year) (tuatara-user:*journal-interval* :daily)
           (tuatara-user:*journal-file-type* nil) (tuatara-user:*journal-file-name* nil)
           (tuatara-user:*org-agenda-files* '())
           (tuatara.defaults::*org-now* *journal-moment*))
       (unwind-protect (progn ,@body)
         (forget-file-buffers)))))

(defun journal-files (directory)
  "The names of the files in journal/ under DIRECTORY and below it, relative, sorted."
  (sort (mapcar #'cdr (tuatara.defaults::files-under (merge-pathnames "journal/" directory))) #'string<))

(defun journal-file-text (directory name)
  "The text of the file NAME in journal/ under DIRECTORY, as on disk."
  (alexandria:read-file-into-string (merge-pathnames (concatenate 'string "journal/" name) directory)))

(defun current-file-name (editor)
  "The name of the file the selected window of EDITOR shows, or NIL."
  (let ((file (tuatara.buffer:buffer-file (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
    (and file (tuatara.io:native-file-name file))))

(define-test (journal titles)
  ;; the titles of each style
  (let ((time *journal-moment*))
    (loop for (style title) in '((:day "Friday") (:day-date-month-year "Friday 20 March 2026")
                                 (:day-date-month-year-24h "Friday 20 March 2026 13:39")
                                 (:day-date-month-year-12h "Friday 20 March 2026 01:39 PM")
                                 ("Week %V, %A" "Week 12, Friday") ("" ""))
          do (let ((tuatara-user:*journal-title-format* style))
               (is string= title (tuatara.defaults::journal-title time) "~s" style)))))

(define-test (journal made-as-denote-journal-makes-it)
  (with-journal (dir)
    ;; made, named and headed as your entries are, and saved at once
    (let ((editor (nth-value 1 (run-keys "SPC d j" :width 100 :height 24))))
      (is equal '("20260320T133958--friday-20-march-2026__journal.org") (journal-files dir))
      (is string= (format nil "#+title:      Friday 20 March 2026~%#+date:       [2026-03-20 Fri 13:39]~%#+filetags:   :journal:~%#+identifier: 20260320T133958~%~%")
          (journal-file-text dir "20260320T133958--friday-20-march-2026__journal.org"))
      (is string= "20260320T133958--friday-20-march-2026__journal.org" (current-file-name editor)))
    (forget-file-buffers)
    ;; later the same day: that one opened, none made
    (let ((tuatara.defaults::*org-now* (+ *journal-moment* 3600)))
      (let ((editor (nth-value 1 (run-keys "SPC d j" :width 100 :height 24))))
        (is = 1 (length (journal-files dir)))
        (is string= "20260320T133958--friday-20-march-2026__journal.org" (current-file-name editor))))
    (forget-file-buffers)
    ;; another made anyway; then the day's two offered to choose from
    (let ((tuatara.defaults::*org-now* (+ *journal-moment* 3600)))
      (run-keys "M-x j o u r n a l - n e w - e n t r y RET" :width 100 :height 24)
      (is = 2 (length (journal-files dir)))
      (forget-file-buffers)
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC d j" :width 100 :height 24) "Journal entry:"))
      (let ((editor (nth-value 1 (run-keys "SPC d j 1 4 3 9 RET" :width 100 :height 24))))
        (is string= "20260320T143958--friday-20-march-2026__journal.org" (current-file-name editor))))
    (forget-file-buffers)
    ;; the next day: a new one
    (let ((tuatara.defaults::*org-now* (+ *journal-moment* 86400)))
      (run-keys "SPC d j" :width 100 :height 24)
      (true (member "20260321T133958--saturday-21-march-2026__journal.org" (journal-files dir) :test #'string=)))
    (forget-file-buffers)
    ;; with C-u, for a date asked for
    (let ((editor (nth-value 1 (run-keys "C-u SPC d j 2 0 2 6 - 0 3 - 0 1 RET" :width 100 :height 30))))
      (is string= "20260301T133958--sunday-1-march-2026__journal.org" (current-file-name editor)))))

(define-test (journal intervals-and-settings)
  (with-journal (dir)
    ;; a week's entry: made on Monday, opened again on Friday, a new one the Monday after
    (let ((tuatara-user:*journal-interval* :weekly))
      (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 9 16 3 2026)))
        (run-keys "SPC d j" :width 100 :height 24))
      (forget-file-buffers)
      (run-keys "SPC d j" :width 100 :height 24)
      (is equal '("20260316T090000--monday-16-march-2026__journal.org") (journal-files dir))
      (forget-file-buffers)
      (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 9 23 3 2026)))
        (run-keys "SPC d j" :width 100 :height 24))
      (is = 2 (length (journal-files dir)))
      (forget-file-buffers))
    ;; a month's: the 16th's is March's
    (let ((tuatara-user:*journal-interval* :monthly))
      (run-keys "SPC d j" :width 100 :height 24)
      (is = 2 (length (journal-files dir)))
      (forget-file-buffers)))
  (with-journal (dir)
    ;; a directory, keyword, title and kind of your own
    (let ((tuatara-user:*journal-directory* (namestring (merge-pathnames "journal/diary/" dir)))
          (tuatara-user:*journal-keyword* "diary")
          (tuatara-user:*journal-title-format* "Week %V, %A")
          (tuatara-user:*journal-file-type* :markdown))
      (run-keys "SPC d j" :width 100 :height 24)
      (is equal '("diary/20260320T133958--week-12-friday__diary.md") (journal-files dir))
      (true (search "title:      \"Week 12, Friday\"" (journal-file-text dir "diary/20260320T133958--week-12-friday__diary.md")))))
  (with-journal (dir)
    ;; the Org header of your other files instead of Denote's
    (let ((tuatara-user:*journal-org-header* nil)
          (tuatara.defaults::*formatted-file-types*
            (list (list :org ".org" :org (format nil "#+title: {title}~%#+options: toc:t~%~%")))))
      (run-keys "SPC d j" :width 100 :height 24)
      (is string= (format nil "#+title: Friday 20 March 2026~%#+options: toc:t~%~%")
          (journal-file-text dir "20260320T133958--friday-20-march-2026__journal.org")))))

(define-test (journal plain-names)
  (with-journal (dir)
    (let ((tuatara-user:*journal-file-name* "%Y/%m-%d.org"))
      ;; named by the date, the title alone at its top, saved at once
      (let ((editor (nth-value 1 (run-keys "SPC d j" :width 100 :height 24))))
        (is equal '("2026/03-20.org") (journal-files dir))
        (is string= (format nil "#+title: Friday 20 March 2026~%~%") (journal-file-text dir "2026/03-20.org"))
        (is string= "03-20.org" (current-file-name editor)))
      (forget-file-buffers)
      ;; opened again, written to, untouched by a second visit; one a day, even asked for a new one
      (alexandria:write-string-into-file "my day" (merge-pathnames "journal/2026/03-20.org" dir) :if-exists :supersede)
      (run-keys "SPC d j" :width 100 :height 24)
      (forget-file-buffers)
      (run-keys "M-x j o u r n a l - n e w - e n t r y RET" :width 100 :height 24)
      (forget-file-buffers)
      (is equal '("2026/03-20.org") (journal-files dir))
      (is string= "my day" (journal-file-text dir "2026/03-20.org"))
      ;; Markdown: a heading
      (let ((tuatara-user:*journal-file-name* "%F.md"))
        (run-keys "SPC d j" :width 100 :height 24)
        (is string= (format nil "# Friday 20 March 2026~%~%") (journal-file-text dir "2026-03-20.md"))))))

(define-test (journal before-1900)
  (with-journal (dir)
    (let ((tuatara.defaults::*org-now* (tuatara.dates:day-universal-time (tuatara.dates:day-number 1850 3 4) 13 39 58)))
      ;; named, titled and headed with its own day
      (run-keys "SPC d j" :width 100 :height 24)
      (is equal '("18500304T133958--monday-4-march-1850__journal.org") (journal-files dir))
      (true (search "#+date:       [1850-03-04 Mon 13:39]"
                    (journal-file-text dir "18500304T133958--monday-4-march-1850__journal.org")))
      (forget-file-buffers)
      ;; a plain name of its date
      (let ((tuatara-user:*journal-file-name* "%F-week-%V.org"))
        (run-keys "SPC d j" :width 100 :height 24)
        (true (member "1850-03-04-week-10.org" (journal-files dir) :test #'string=))))))

(define-test (journal names-of-no-day)
  ;; a file whose identifier names a day there is not (the 40th) is no entry, never taken for another day
  (with-journal (dir)
    (let ((folder (merge-pathnames "journal/" dir)))
      (ensure-directories-exist folder)
      (dolist (name '("20261340T090000--odd__journal.org" "20261001T090000--fine__journal.org"))
        (alexandria:write-string-into-file "x" (merge-pathnames name folder))))
    (is equal (list (calendar-day 2026 10 1)) (mapcar #'cdr (tuatara.defaults::journal-entries)))))

(define-test (journal in-the-calendar)
  (with-journal (dir)
    (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 1 10 2026)))
      (run-keys "SPC d j" :width 100 :height 24))
    (forget-file-buffers)
    ;; the day with an entry marked in its own face; another, none
    (calendar-keys "")
    (let ((marked (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*Calendar*") 'tuatara.defaults::calendar-marked)))
      (true (and marked (member :calendar-journal (gethash (calendar-day 2026 10 1) marked))))
      (false (and marked (gethash (calendar-day 2026 10 2) marked))))
    ;; a day with agenda entries too keeps their colour: theirs laid last, whichever mark came first
    (is equal '(:calendar-journal :calendar-marked) (tuatara.defaults::calendar-day-faces '(:calendar-journal :calendar-marked)))
    (is equal '(:calendar-journal :calendar-marked) (tuatara.defaults::calendar-day-faces '(:calendar-marked :calendar-journal)))
    ;; n on a day: its entry made, the calendar gone
    (multiple-value-bind (f editor) (calendar-keys "C-b n")
      (declare (ignore f))
      (is string= "20261002T120000--friday-2-october-2026__journal.org" (current-file-name editor))
      (is = 1 (length (tuatara.command:editor-windows editor))))
    (forget-file-buffers)
    ;; on a day with one: that one opened, with the modal key too
    (multiple-value-bind (f editor) (calendar-keys "h h n")
      (declare (ignore f))
      (is string= "20261001T120000--thursday-1-october-2026__journal.org" (current-file-name editor)))
    (is = 2 (length (journal-files dir)))))

(define-test (journal opened-from-a-marked-day)
  (with-journal (dir)
    (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 12 1 10 2026)))
      (run-keys "SPC d j" :width 100 :height 24))
    (forget-file-buffers)
    (let ((tasks (merge-pathnames "tasks.org" dir)))
      (alexandria:write-string-into-file (format nil "* Meeting <2026-10-01 Thu>~%* Dentist <2026-10-02 Fri>~%") tasks)
      (let ((tuatara-user:*org-agenda-files* (list (namestring tasks))))
        (flet ((on-day (keys)
                 (calendar-keys (list #'wait-for-calendar-marks :tick keys))))
          ;; RET on a day with agenda entries alone: the agenda, on that day
          (on-day "C-b RET")
          (let ((agenda (tuatara.buffer:find-buffer "*Org Agenda*")))
            (true agenda)
            (when agenda
              (is = (calendar-day 2026 10 2) (tuatara.defaults::org-agenda-day-at-point agenda))))
          ;; on a day with both: asked which, each by its own key; n the journal entry
          (multiple-value-bind (f editor) (on-day "C-b C-b RET n")
            (true (tuatara.frontend.fake:fake-frame-showing f "a agenda" "n journal entry"))
            (is string= "20261001T120000--thursday-1-october-2026__journal.org" (current-file-name editor)))
          (forget-file-buffers)
          ;; on a day with neither: said so, nothing opened
          (multiple-value-bind (f editor) (on-day "RET")
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "Nothing is marked on Sat 2026-10-03"))))))))
