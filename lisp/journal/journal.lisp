;;;; lisp/journal/journal.lisp --- The journal: an entry a day (or week, month, year), opened or made.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's denote-journal: journal-new-or-existing-entry (C-x n j) opens
;;;; today's entry, or makes it: a file with a formatted name in the journal's
;;;; directory, titled with the day (Friday 20 March 2026), keyword journal,
;;;; the header of its kind, saved at once. Several entries for the day are
;;;; offered to choose from. Entries can be a week's, a month's or a year's
;;;; instead. The calendar marks the days that have one, and n there opens or
;;;; makes the entry of the day at point. Files with plain names
;;;; (2026-03-20.org) can be the journal instead, for those who would rather.

(in-package #:tuatara.defaults)

(defvar *journal-directory* nil
  "The directory of your journal's entries: NIL, as standard, for journal/ in
*FORMATTED-FILE-DIRECTORY* (such as ~/notes/journal/), or a directory
of its own, such as \"~/journal/\".")

(defvar *journal-keyword* "journal"
  "The keyword in the formatted name of each journal entry, by which entries
are told from your other files: journal, as standard.")

(defvar *journal-title-format* :day-date-month-year
  "The title of a new journal entry: :day (Friday), :day-date-month-year
\(Friday 20 March 2026, as standard), :day-date-month-year-24h (Friday 20 March
2026 13:39), :day-date-month-year-12h (Friday 20 March 2026 01:39 PM), a
pattern in the % specifiers of `format-time' (\"Week %V, %A\"), \"\" for none,
or NIL to be asked each time.")

(defvar *journal-interval* :daily
  "How long a journal entry lasts: :daily, :weekly
\(the ISO week, from Monday), :monthly or :yearly. An entry is opened while
it lasts; after, a new one is made.")

(defvar *journal-file-type* nil
  "The kind of file a new journal entry is: :org, :text or :markdown; NIL for
*FORMATTED-FILE-TYPE*.")

(defvar *journal-file-name* nil
  "How journal entries are named: NIL, as standard, for formatted names as
Denote's (20260320T133958--friday-20-march-2026__journal.org), with the
header of their kind; or, for plain names, the entry's date in a pattern of
the % specifiers of `format-time', such as \"%Y-%m-%d.org\" (2026-03-20.org)
or \"%G/week-%V.md\" (%G the year the week counts in, so that no week is
split at New Year), the file then begun with its title alone (#+title: in
Org, # in Markdown).")

(defvar *journal-org-header* "#+title:      {title}
#+date:       {date}
#+filetags:   {keywords}
#+identifier: {identifier}

"
  "The header a new journal entry in Org begins with, its name formatted:
title, date, keywords and identifier lines, {title}, {date}, {keywords} and
{identifier} standing for the entry's; NIL for the Org header of your other
files with formatted names (*FORMATTED-FILE-TYPES*). Entries in Markdown or
plain text have the header of their kind there.")

(defun journal-directory ()
  "The directory of the journal's entries, as a native name ending in /."
  (directory-name (or *journal-directory* (concatenate 'string (directory-name *formatted-file-directory*) "journal/"))))

(defun journal-title (time)
  "The title of a new journal entry at TIME, as *JOURNAL-TITLE-FORMAT* says;
asked for when it is NIL."
  (let ((format *journal-title-format*))
    (cond ((null format) (read-string "New journal entry TITLE: " :initial (format-time "%F" time)))
          ((stringp format) (string-trim " " (format-time format time)))
          (t (format-time (ecase format
                                    (:day "%A")
                                    (:day-date-month-year "%A %-e %B %Y")
                                    (:day-date-month-year-24h "%A %-e %B %Y %H:%M")
                                    (:day-date-month-year-12h "%A %-e %B %Y %I:%M %^p"))
                                  time)))))

;;; Which entries a day has

(defun journal-same-interval-p (one other)
  "True when the day numbers ONE and OTHER fall in one journal entry's time (*JOURNAL-INTERVAL*)."
  (multiple-value-bind (y1 m1) (day-date one)
    (multiple-value-bind (y2 m2) (day-date other)
      (case *journal-interval*
        (:weekly (= (week-start one 1) (week-start other 1)))
        (:monthly (and (= y1 y2) (= m1 m2)))
        (:yearly (= y1 y2))
        (t (= one other))))))

(defun journal-entries ()
  "The journal's entries with formatted names, in its directory and under it:
a list of (PATH . DAY), DAY the day number of the identifier's date, the
latest first."
  (let ((entries '()))
    (dolist (file (ignore-errors (files-under (journal-directory))))
      (multiple-value-bind (identifier signature title keywords) (name-parts (tuatara.io:native-file-name (car file)))
        (declare (ignore signature title))
        (let ((time (and identifier (identifier-at-start-p identifier) (identifier-time identifier))))
          (when (and time (or (null *journal-keyword*) (member *journal-keyword* keywords :test #'string-equal)))
            (push (cons (car file) (nth-value 0 (time-day time))) entries)))))
    (sort entries #'> :key #'cdr)))

(defun journal-plain-path (day)
  "The path of the plain-named entry of the day DAY (*JOURNAL-FILE-NAME*)."
  (concatenate 'string (journal-directory) (format-day *journal-file-name* day 12)))

(defun journal-entries-of (day)
  "The paths of the journal's entries for the day DAY, as *JOURNAL-INTERVAL* reckons them."
  (if *journal-file-name*
      (let ((path (journal-plain-path day)))
        (and (ignore-errors (tuatara.io:path-stamp path)) (list path)))
      (loop for (path . entry-day) in (journal-entries)
            when (journal-same-interval-p day entry-day) collect path)))

;;; Opening and making entries

(defun journal-time-of (day)
  "A universal time on the day DAY at the time of day it is now."
  (multiple-value-bind (today hour minute second) (time-day (org-now))
    (declare (ignore today))
    (day-universal-time day hour minute second)))

(defun journal-make-entry (day)
  "Make the journal's entry for the day DAY, save it, and return its buffer:
with a formatted name and the header of its kind, or a plain name
\(*JOURNAL-FILE-NAME*) and its title alone. A plain name taken already is
visited as it is."
  (let ((plain (and *journal-file-name* (journal-plain-path day))))
    (if (and plain (ignore-errors (tuatara.io:path-stamp plain)))
        (visit-path plain)
        (let* ((time (journal-time-of day))
               (title (journal-title time))
               (directory (journal-directory))
               (buffer
                 (if plain
                     (let* ((buffer (progn (tuatara.io:ensure-directories plain) (tuatara.io:visit-file plain)))
                            (kind (first (formatted-file-type-for (name-extension (tuatara.io:native-file-name plain)))))
                            (head (cond ((string= title "") "")
                                        ((eq kind :org) (format nil "#+title: ~a~%~%" title))
                                        ((eq kind :markdown) (format nil "# ~a~%~%" title))
                                        (t (format nil "~a~%~%" title)))))
                       (when (plusp (length head))
                         (tuatara.buffer:buffer-insert buffer 0 head)
                         (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-size buffer)))
                       (show-buffer buffer)
                       buffer)
                     (let ((type (or (find (or *journal-file-type* *formatted-file-type*) *formatted-file-types* :key #'first)
                                     (first *formatted-file-types*))))
                       (tuatara.io:ensure-directories directory)
                       (let ((*formatted-file-types*
                               (if *journal-org-header*
                                   (mapcar (lambda (kind) (if (eq (first kind) :org)
                                                              (list (first kind) (second kind) (third kind) *journal-org-header*)
                                                              kind))
                                           *formatted-file-types*)
                                   *formatted-file-types*)))
                         (make-formatted-file directory title (and *journal-keyword* (list *journal-keyword*)) (second type)
                                              :time time))))))
          (when buffer
            (save-buffer-with-care buffer))
          buffer))))

(defun journal-open-entry (day &key new)
  "Open the journal's entry for the day DAY: the one there is, one chosen
among several, or a new one made when there is none, or with NEW (with plain
names, a day has one entry only)."
  (let ((paths (and (or (not new) *journal-file-name*) (journal-entries-of day))))
    (cond ((null paths) (journal-make-entry day))
          ((null (rest paths)) (visit-path (first paths)))
          (t (let* ((root (journal-directory))
                    (relative (mapcar (lambda (path) (if (uiop:string-prefix-p root path) (subseq path (length root)) path)) paths))
                    (chosen (completing-read "Journal entry: " relative :require-match t)))
               (when (member chosen relative :test #'string=)
                 (visit-path (concatenate 'string root chosen))))))))

(defun journal-read-day ()
  "Ask for a date as Org's date prompt reads one (READ-ORG-DATE), the calendar below; its day number, or NIL."
  (let ((ts (read-org-date "Journal entry for: ")))
    (and ts (org-ts-day-number ts))))

(define-command journal-new-or-existing-entry ()
  "Open today's journal entry, or make it when there is none: in *journal-directory*, named and
titled as *journal-file-name* and *journal-title-format* say, saved at
once; with several for today, the one chosen. *journal-interval* makes it a
week's, a month's or a year's entry instead. With \\[universal-argument], for a date asked for."
  (let ((day (if (prefix-argument) (journal-read-day) (org-today))))
    (if day (journal-open-entry day) (message "That is no date"))))

(define-command journal-new-entry ()
  "Make a new journal entry for today, whatever entries there are; with \\[universal-argument], for a date asked for.
With plain names (*journal-file-name*) a day has one entry, which is opened."
  (let ((day (if (prefix-argument) (journal-read-day) (org-today))))
    (if day (journal-open-entry day :new t) (message "That is no date"))))

(define-command calendar-journal-entry ()
  "Open the journal's entry for the calendar's day at point, or make it: the
calendar leaves its window, the entry shown where you were."
  (let ((day (calendar-cursor-date)))
    (leave-window (current-window))
    (journal-open-entry day)))

;;; Marked in the calendar

(defparameter *journal-carried-settings*
  '(*journal-directory* *formatted-file-directory* *journal-keyword* *journal-file-name*)
  "The settings that find the journal's entries, carried to the thread that
marks their days in the calendar as they are on the editor's when the marking
starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defun journal-calendar-marks (first last)
  "For the calendar (*CALENDAR-MARK-FUNCTIONS*): the function that, on a thread
of its own, gives the days from FIRST to LAST with a journal entry, each
\(DAY . :CALENDAR-JOURNAL)."
  (tuatara.runtime:carry-settings
   *journal-carried-settings*
   (lambda ()
     (if *journal-file-name*
         (loop for day from first to last
               when (ignore-errors (tuatara.io:path-stamp (journal-plain-path day)))
                 collect (cons day :calendar-journal))
         (loop for (nil . day) in (journal-entries)
               when (<= first day last) collect (cons day :calendar-journal))))))

(defun install-journal ()
  "Mark the journal's days in the calendar, and bind n there for the entry of
the day at point, which RET opens too on a day marked for it."
  (pushnew 'journal-calendar-marks *calendar-mark-functions*)
  (pushnew '(:calendar-journal calendar-journal-entry "journal entry") *calendar-day-openers* :test #'equal)
  (define-key (mode-keymap 'calendar-mode) "n" 'calendar-journal-entry)
  (commands-for-mode 'calendar-mode "the calendar (\\[calendar])" 'calendar-journal-entry))
