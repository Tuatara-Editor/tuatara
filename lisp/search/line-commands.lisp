;;;; lisp/search/line-commands.lisp --- Deleting the lines or records a pattern matches or misses, and the header line.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; flush-lines, keep-lines and the commands for records delete the lines a
;;;; pattern matches or misses, by a job over lines (START-LINE-JOB). The
;;;; header line of a file of records always stays, and can be locked in view
;;;; at the top of the window.

(in-package #:tuatara.defaults)

(defun delete-filtered-lines (buffer start end words filter keep report &key ends)
  "Delete each line of BUFFER from START below END, octets at line starts,
that FILTER leaves, or with KEEP each it does not (FILTERED-LINE), as
CHANGE-LINES does it, in the background when large, one step to undo.
REPORT, a function of how many lines went, gives what is said once done.
WORDS names the work (\"Filtering\"). Returns the job, :QUEUED
\(QUEUE-IF-BUSY), or NIL. With ENDS (RECORD-ENDS), records that may hold
line breaks are kept or deleted whole. A FILTER trying a column deletes
nothing when a line is longer than *REWRITE-LINE-LIMIT* octets, as its
column is then tried on the line's start alone, and says so."
  (let ((gone (list 0)) (cut (list 0)))
    (multiple-value-bind (per-line prepare) (filtered-line filter keep gone :ends ends :crlf (buffer-crlf-p buffer))
      (change-lines buffer start end words
                    (lambda (function slice)
                      (when (zerop (car cut)) (funcall per-line function slice)))
                    (lambda (done)
                      (if (plusp (car cut))
                          (format nil "Nothing deleted: ~:d line~:p longer than ~a, where a column is tried ~
on the start alone~@[ ~a~]"
                                  (car cut) (tuatara.text:octets-words *rewrite-line-limit* :binary t) done)
                          (format nil "~a~@[ ~a~]" (funcall report (car gone)) done)))
                    :prepare (lambda (&rest arguments)
                               (let ((*lines-cut* cut)) (apply prepare arguments)))))))

(defun read-line-filter (buffer words start end)
  "Ask how to filter the lines of BUFFER from START below END, WORDS (keep
lines) heading the question. It asks what of a line the pattern is tried on
\(FILTER-LINE-MATCHER, a column asked first in a file of records cut into
fields). Then it asks the pattern, as a search asks it, with how many lines it
matches counted before the prompt when it is tried on the whole line. Five
values: the pattern, true when it is a regular expression, the matcher, words
naming what of a line it tries or NIL, and the first line that may change, the
one after a file of records' header (LIST-HEADER-P), or NIL."
  (multiple-value-bind (matcher what) (filter-line-matcher buffer)
    (let ((first (and (records-file-p buffer)
                      (list-header-p buffer)
                      (records-start (tuatara.buffer:buffer-text buffer) 1 (record-ends buffer)))))
      (multiple-value-bind (pattern regex)
          (let ((counter (make-pattern-count))
                (lines (list (cons (max start (or first 0)) end))))
            (unwind-protect
                 (read-pattern (filter-question words what)
                               ;; how many lines the pattern matches, when it is tried on the whole line
                               :count (and (null what)
                                           (lambda (text regex)
                                             (stretch-pattern-count
                                              counter buffer lines text regex
                                              :ends (and (records-file-p buffer) (record-ends buffer))))))
              (tuatara.runtime:stop-background-work (pattern-count-job counter))))
        (values pattern regex matcher what first)))))

(defun filter-lines (command keep &key whole (noun "line") (words (if keep "keep lines" "flush lines")))
  "Ask for a pattern and delete the lines the selection touches that it matches,
or with KEEP those it does not, in the background when large; what of a line the
pattern is tried on is asked first in a file of records cut into fields
\(READ-LINE-FILTER, WORDS heading the question). With WHOLE, every line when
nothing is selected (of the part shown, in a narrowed buffer). COMMAND names the
command in messages, NOUN what a line is called in them. Refused at once in a
read-only buffer."
  (let ((b (current-buffer)))
    (check-writable b)
    (multiple-value-bind (start end) (selected-lines b)
      (when (and (null start) whole)
        (multiple-value-setq (start end) (tuatara.buffer:buffer-bounds b)))
      (if (null start)
          (say-not-run command)
          (multiple-value-bind (pattern regex matcher what first) (read-line-filter b words start end)
            (multiple-value-bind (filter why)
                (and (plusp (length pattern))
                     (make-search-filter pattern :regex regex :test (and what matcher)))
              (let ((start (max start (or first 0))))
                (cond ((zerop (length pattern)) (say-not-run command "No pattern"))
                      ((null filter) (say-not-run command (string-capitalize why :end 1)))
                      ((>= start end) (say-not-run command "No lines to filter but the header"))
                      (t (delete-filtered-lines b start end "Filtering" filter keep
                                                (lambda (gone)
                                                  (format nil "Deleted ~:d ~:[matching~;non-matching~] ~a~p"
                                                          gone keep noun gone))
                                                :ends (and (records-file-p b) (record-ends b))))))))))))

(define-command flush-lines ()
  "Delete the lines the selection touches in which a regular expression, asked
for in the minibuffer, finds a match. That is every line any part of a match
lies on, as one search of the text finds the matches, a match running over
several lines deleting them all. ^ and $ are a line's start and end, so that ^$
deletes the empty lines. Smart case, as in search: a pattern with a capital
letter, or naming a case ([[:upper:]], \\p{Lu}), heeds case. Says how many were
deleted. In a file of records cut into columns, a CSV, a log, JSON Lines and the
others \\[search-filter-show-records] names, a column is asked for first
\(nothing for the whole line), the pattern tried on its value alone, and a
header line always stays. A column of a line longer than *REWRITE-LINE-LIMIT*
octets (16 MiB as shipped) would be tried on its first part alone, so then
nothing is deleted, and that is said. Without a selection, does nothing and
says so; a large selection is handled in the background."
  (filter-lines 'flush-lines nil))

(define-command keep-lines ()
  "Delete the lines the selection touches in which a regular expression, asked
for in the minibuffer, finds no match, keeping those it matches. The pattern is
found as for flush-lines, in a file of records on the column asked for, nothing
deleted when a line is longer than *REWRITE-LINE-LIMIT* octets (16 MiB as
shipped). Says how many were deleted. Without a selection, does nothing and
says so; a large selection is handled in the background."
  (filter-lines 'keep-lines t))

(define-command list-keep-records ()
  "Keep the records whose line a regular expression matches and delete the rest,
in a file of records, a line each: a CSV or TSV, a log, a JSON Lines file,
todo.txt and the others *LINE-RECORD-MODES* names, the column asked for first as
\\[search-filter-show-records] asks. The lines the selection touches, or every
record; a header line always stays (\\[list-toggle-header]). Smart case, as in
search. One step to undo; a large file is filtered in the background.
\\[search-filter-show-records] hides the others instead, deleting nothing."
  (if (records-file-p (current-buffer))
      (filter-lines 'list-keep-records t :whole t :noun "record" :words "keep records")
      (say-not-records 'list-keep-records "keep-lines filters any lines")))

(define-command list-delete-records ()
  "Delete the records whose line a regular expression matches, keeping the
rest: asked for as \\[list-keep-records] asks, in the same files. A header
line always stays. \\[search-filter-hide-records] hides them instead,
deleting nothing."
  (if (records-file-p (current-buffer))
      (filter-lines 'list-delete-records nil :whole t :noun "record" :words "delete records")
      (say-not-records 'list-delete-records "flush-lines filters any lines")))

(defun list-header-line (window)
  "Header line function: the first line of WINDOW's buffer, its header, as the
window shows its text, scrolled as far to the side and coloured as it is below
\(by column in a CSV), its spans as the second value."
  (let* ((buffer (tuatara.display:window-buffer window))
         (margin (tuatara.display:window-margin window))
         (rows (tuatara.display:layout (tuatara.buffer:buffer-text buffer) 0
                                       (max 1 (- (tuatara.display:window-width window) margin)) 1
                                       :hscroll (if (tuatara.display:window-wraps-p window)
                                                    0
                                                    (tuatara.display:window-hscroll window))
                                       :tab-width (tuatara.display:tab-width-of buffer)
                                       :crlf (buffer-crlf-p buffer))))
    (if (zerop (length rows))
        (values "" nil)
        (let ((row (aref (apply #'tuatara.display:apply-highlights
                                rows (mode-syntax-highlights (buffer-major-mode buffer) buffer rows))
                         0)))
          (values (concatenate 'string (spaces margin) (tuatara.display:row-string row))
                  (mapcar (lambda (span) (list (+ margin (first span)) (+ margin (second span)) (third span)))
                          (tuatara.display:row-spans row)))))))

(defun set-header-lock (buffer on)
  "Keep BUFFER's first line at the top of its windows when ON is true, else
let it scroll away with the rest."
  (setf (tuatara.display:buffer-header-line buffer) (and on 'list-header-line)))

(define-command list-toggle-header-lock ()
  "Keep the first line of a file of records, its header, at the top of the
window as the rest scrolls under it, coloured as it is below; the same again
lets it scroll away; with a count, kept when it is positive, let go when it
is zero or less. A CSV's first line is its header; in a log and the other
files of records \\[list-toggle-header] makes it one first.
*LIST-HEADER-LOCK* keeps a header there wherever there is one."
  (let ((buffer (current-buffer)))
    (cond ((not (records-file-p buffer)) (say-not-records 'list-toggle-header-lock))
          ((not (list-header-p buffer))
           (message (substitute-command-keys "No header: \\[list-toggle-header] makes the first line one")))
          (t (let ((on (switched-by-count (tuatara.display:buffer-header-line buffer))))
               (set-header-lock buffer on)
               (message "Header ~:[scrolls with the rest~;kept at the top~]" on))))))

(defmethod mode-exit :before ((mode fundamental-mode) buffer)
  "A file of records leaving its mode: its search filters, its header lock and
what \\[list-toggle-header] said of its first line are let go."
  (when (filters-hide-lines-p mode buffer)
    (set-header-lock buffer nil)
    (drop-search-filters buffer)
    (setf (tuatara.buffer:buffer-local buffer :list-header) :mode)))

(define-command list-toggle-header ()
  "Take the first line of a file of records as its header, or as a record like
the rest; the same again switches back; with a count, a header when it is
positive, a record when it is zero or less. A header is always shown by search
filters and never deleted by \\[list-keep-records] and \\[list-delete-records],
and its words name the columns offered; a record is filtered with the rest, the
columns offered by their numbers but where the file's kind names them. A CSV's
first line is its header unless *CSV-HEADER* is NIL; a log's, a JSON Lines
file's and the others' is a record. Filters set are worked out again at once."
  (let ((buffer (current-buffer)))
    (if (not (records-file-p buffer))
        (say-not-records 'list-toggle-header)
        (let ((on (switched-by-count (list-header-p buffer)))
              (filtering (list-filtering buffer)))
          (setf (tuatara.buffer:buffer-local buffer :list-header) on)
          (set-header-lock buffer (and on (or *list-header-lock* (tuatara.display:buffer-header-line buffer))))
          (when (and filtering (list-filtering-lines filtering))
            (setf (list-filtering-lines filtering) (if on 1 0)
                  (list-filtering-source filtering) nil)
            (refilter buffer))
          (message "First line ~:[a record like the rest~;the header, always shown~]" on)))))
