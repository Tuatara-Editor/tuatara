;;;; lisp/search/filter-jobs.lisp --- The filters of a list or file worked out in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Matching runs in the background where a list gives more text, or has many
;;;; lines; each filter keeps what it left, so a filter added searches only
;;;; those, and one taken away has only the later ones worked out again.
;;;; Where the work is large, each key typed matches the lines from the top of
;;;; the window first, for as long as *SEARCH-FILTER-SCREEN-SECONDS*, and shows
;;;; what it found at once; the rest is worked out in the background on all but
;;;; two cores, and the next key typed starts again.

(in-package #:tuatara.defaults)

(define-record (search-filter-job (:include tuatara.runtime:job)
                                 (:constructor make-search-filter-job (buffer filtering source chain))
                                 (:copier nil))
  "Filters of a list or a file of records worked out in the background for
BUFFER. CHAIN is the filters it works out, FILTERING's at the start, for SOURCE,
its whole list or the version of its text. DONE of TOTAL entries are looked at
so far, or for records the octets looked at in PROGRESS's car of those in its
cdr, on filter PASS of PASSES. RESULTS is what each filter worked out leaves,
each (FILTER KEPT COUNT LOOKED), kept in the filters when the job is taken in
while they are still the list's. WHOLE is true when the first of them looked at
every record, so that its LOOKED is how many there are."
  (filtering nil :documentation "The filtering whose filters JOB works out, as it was at the start.")
  (source nil :documentation "The whole list or the version of the text that JOB works the filters out for.")
  (chain nil :documentation "The filters JOB works out.")
  (done 0 :documentation "How many entries JOB has looked at so far.")
  (total 1 :documentation "How many entries JOB has to look at.")
  (results '()
           :documentation "What each filter JOB has worked out leaves, latest first, each (FILTER KEPT COUNT LOOKED).")
  (progress (cons 0 1)
            :documentation "A cons whose car is the octets JOB has looked at of the octets in its cdr, for a file of
records.")
  (pass 0 :documentation "The number of the filter JOB is on, from 0.")
  (passes 1 :documentation "How many filters JOB works out.")
  (whole nil
         :documentation "True when the first filter of JOB looked at every record, so that its LOOKED is how many
there are."))

(defun search-filter-job-lines-p (job)
  "True when JOB filters the records of a file rather than a list."
  (list-filtering-lines (search-filter-job-filtering job)))

(tuatara.runtime:define-job-words search-filter-job
  :doing "Filtering"
  :description (format nil "Filtering the ~:[list~;records~] by ~{~a~^, ~}" (search-filter-job-lines-p job)
                       (mapcar #'search-filter-words (search-filter-job-chain job)))
  :verb (if (search-filter-job-lines-p job) "filter the records" "filter the list")
  :left-words "the filters not yet worked out are taken away"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-percent ((job search-filter-job))
  "How many of the entries, or of the octets of the records, to look at have been."
  (if (search-filter-job-lines-p job)
      (let ((progress (search-filter-job-progress job)))
        (tuatara.runtime:running-percent (+ (search-filter-job-pass job) (/ (car progress) (max 1 (cdr progress))))
                                         (search-filter-job-passes job)))
      (tuatara.runtime:running-percent (search-filter-job-done job) (search-filter-job-total job))))

(defmethod tuatara.runtime:job-holds-queue-p ((job search-filter-job))
  "Filtering a list lets other work on it start beside it."
  nil)

(defmethod tuatara.runtime:job-take-in ((job search-filter-job))
  "Show the list or the records narrowed, when the filters are still those
worked out; when the job was stopped, or failed, take away the filters it
did not work out, showing what those before them leave."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (filtering (search-filter-job-filtering job))
         (source (search-filter-job-source job))
         ;; any change to the filters or the list stops it and starts another
         (current (eq (list-filtering-job filtering) job)))
    (when current
      (setf (list-filtering-job filtering) nil))
    (when (and current (live-buffer-p buffer) (eq (list-filtering-source filtering) source))
      (cond ((eq (tuatara.runtime:job-status job) :done)
             (let ((results (reverse (search-filter-job-results job))))
               (loop for (filter kept count) in results
                     do (keep-worked-out filter source kept count))
               (when (and (search-filter-job-whole job) results)
                 (setf (list-filtering-total filtering) (fourth (first results))))))
            (t (setf (list-filtering-filters filtering)
                     (remove-if-not (lambda (filter) (eq (search-filter-source filter) source))
                                    (list-filtering-filters filtering)))
               (let ((typing (list-filtering-typing filtering)))
                 (when (and typing (not (eq (search-filter-source typing) source)))
                   (setf (list-filtering-typing filtering) nil)))))
      (shown-as-last-filter filtering)
      (show-filtered buffer))))

(defun filtering-slowly-p (buffer)
  "True while filters of BUFFER, a list or a file of records, are worked out in
the background because a pattern was too slow on its text for the editor's
thread."
  (let ((filtering (list-filtering buffer)))
    (and filtering (list-filtering-slow filtering) (list-filtering-job filtering) t)))

(defun take-in-filter-jobs (buffer)
  "Take in the jobs filtering BUFFER, a list or a file of records, that have
ended, shown narrowed, and start the one waiting for typing to pause once it
has (*TYPING-PAUSE*): true while one still runs, or the seconds still to
wait."
  (tuatara.runtime:take-in-jobs (lambda (job)
                                  (and (search-filter-job-p job) (eq (tuatara.runtime:job-buffer job) buffer))))
  (let ((filtering (list-filtering buffer)))
    (cond ((null filtering) nil)
          ((and (list-filtering-due filtering) (filter-pause-left filtering)))
          ((list-filtering-due filtering) (start-due-filter-job buffer) t)
          (t (and (list-filtering-job filtering) t)))))

(defun filter-pause-left (filtering)
  "Seconds still to wait before FILTERING's job starts (TYPING-PAUSE-LEFT for
the text it narrows), or NIL."
  (let ((source (list-filtering-source filtering)))
    (typing-pause-left (if (list-filtering-lines filtering)
                           (tuatara.text:rope-octet-length (tuatara.text:version-rope source))
                           (length (list-source-text source))))))

(defun start-due-filter-job (buffer)
  "Start the job of BUFFER's filters that waits for typing to pause, when
one waits."
  (let* ((filtering (list-filtering buffer))
         (due (and filtering (list-filtering-due filtering))))
    (when due
      (setf (list-filtering-due filtering) nil)
      (start-filter-job buffer filtering (first due) (second due)))))

(defun stop-filtering-job (filtering)
  "Stop the job working out FILTERING's filters, when one runs or waits."
  (setf (list-filtering-due filtering) nil)
  (let ((job (list-filtering-job filtering)))
    (when job
      (setf (list-filtering-job filtering) nil)
      (tuatara.runtime:job-stop job))))

(defun work-out-now (buffer filtering filter source previous)
  "Work out FILTER, of FILTERING, BUFFER's, for SOURCE on this thread, from what
PREVIOUS leaves: its entries' bits, or for records its runs of lines, kept
in it (KEEP-WORKED-OUT), and returned."
  (if (list-filtering-lines filtering)
      (let ((rope (tuatara.text:version-rope source)))
        (multiple-value-bind (runs count looked)
            (lines-matching (list filter) rope (or previous
                                                   (all-records rope
                                                                (list-filtering-lines filtering)
                                                                (record-ends buffer)))
                           (buffer-crlf-p buffer) :ends (record-ends buffer))
          (unless previous (setf (list-filtering-total filtering) looked))
          (keep-worked-out filter source runs count)))
      (multiple-value-call #'keep-worked-out filter source (work-out-filter filtering filter source previous))))

(defvar *filter-as-typed* nil
  "True while a filter is worked out for what is typed at its prompt, its job
then waiting for typing to pause.")
(pushnew '*filter-as-typed* *question-variables*)

(defun work-out-in-background (buffer filtering left previous)
  "Start the job working out the filters LEFT of FILTERING, BUFFER's, from
what PREVIOUS leaves, on all but two cores, while a filter is typed once
typing pauses (*FILTER-AS-TYPED*, *TYPING-PAUSE*); meanwhile what the whole
chain matches from the top of the window is shown (LIST-ON-SCREEN,
LINES-ON-SCREEN)."
  (let* ((source (list-filtering-source filtering))
         (chain (search-filter-chain filtering))
         (lines (list-filtering-lines filtering)))
    (multiple-value-bind (shown count)
        (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
          (if lines (lines-on-screen buffer chain) (list-on-screen buffer filtering source chain)))
      (when (eq count :slow)
        ;; a pattern slow on the lines from the window's top: nothing shown till the job has done
        (setf (list-filtering-slow filtering) t
              shown (if lines
                        (make-array 0 :element-type 'fixnum)
                        (make-array (length (list-source-entries source)) :element-type 'bit :initial-element 0))
              count 0))
      (setf (list-filtering-shown filtering) shown (list-filtering-shown-count filtering) count))
    (if (and *filter-as-typed* (filter-pause-left filtering))
        (setf (list-filtering-due filtering) (list left previous))
        (start-filter-job buffer filtering left previous))))

(defun start-filter-job (buffer filtering left previous)
  "Start the job working out the filters LEFT of FILTERING, BUFFER's, from
what PREVIOUS leaves, on all but two cores, the whole shown narrowed again
once it is done."
  (let* ((source (list-filtering-source filtering))
         (job (make-search-filter-job buffer filtering source (search-filter-chain filtering)))
         (lines (list-filtering-lines filtering)))
    (setf (list-filtering-job filtering) job)
    (if lines
        (let* ((rope (tuatara.text:version-rope source))
               (crlf (buffer-crlf-p buffer))
               (ends (record-ends buffer))
               (header (list-filtering-lines filtering))
               (hook *rewrite-piece-hook*))
          (setf (search-filter-job-passes job) (length left) (search-filter-job-whole job) (null previous))
          (tuatara.runtime:start-job-thread
           job (lambda (job)
                 (let ((ranges (or previous (all-records rope header ends)))
                       (cancel (tuatara.runtime:job-cancel job)))
                   (dolist (filter left)
                     (setf (search-filter-job-progress job) (cons 0 (max 1 (runs-octets ranges))))
                     (multiple-value-bind (runs count looked)
                         (lines-matching-everywhere filter rope ranges crlf (search-filter-job-progress job)
                                                   (lambda () (car cancel)) hook ends)
                       (tuatara.runtime:job-check-stop)
                       (push (list filter runs count looked) (search-filter-job-results job))
                       (incf (search-filter-job-pass job))
                       (setf ranges runs)))))
           :name "tuatara record filter"))
        (progn
          (list-source-entry-lines source)
          (setf (search-filter-job-total job)
                (* (length left) (max 1 (if previous (count 1 previous) (length (list-source-entries source))))))
          (tuatara.runtime:start-job-thread
           job (lambda (job)
                 (let ((bits previous))
                   (dolist (filter left)
                     (multiple-value-bind (kept count)
                         (work-out-filter filtering filter source bits
                                          (lambda ()
                                            (when (zerop (mod (incf (search-filter-job-done job)) 64))
                                              (tuatara.runtime:job-check-stop))))
                       (push (list filter kept count) (search-filter-job-results job))
                       (setf bits kept)))))
           :name "tuatara list filter")))))

(defun work-out-filters (buffer filtering)
  "Work out the filters of FILTERING, BUFFER's, not yet worked out for its
whole list or its text: at once when that is quick (FILTER-QUICKLY-P), else
in the background, what the filters match from the window's top shown
meanwhile, and the whole shown narrowed again once they are done; what is
shown is then what the last filter leaves. True when they are all worked
out now. Work on the editor's thread held up by one line longer than
TUATARA.RUNTIME:*SLOW-CALL-SECONDS*, a slow pattern, goes on in the
background (SLOW in FILTERING then)."
  (stop-filtering-job filtering)
  (setf (list-filtering-slow filtering) nil)
  (multiple-value-bind (left previous) (filters-to-work-out filtering)
    (let ((source (list-filtering-source filtering)))
      (let ((quick (filter-quickly-p filtering previous)))
        (cond ((null left))
              ((and quick
                    (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                      (dolist (filter left t)
                        (setf previous (work-out-now buffer filtering filter source previous))))))
              (t (when quick (setf (list-filtering-slow filtering) t))
                 ;; what a slow pattern left not worked out here goes on in the background
                 (multiple-value-bind (left previous) (filters-to-work-out filtering)
                   (work-out-in-background buffer filtering left previous))
                 (return-from work-out-filters nil))))
      (shown-as-last-filter filtering)
      t)))

(defun shown-as-last-filter (filtering)
  "Have the list FILTERING narrows show the entries its last filter leaves,
all of them when it has none."
  (let ((last (car (last (search-filter-chain filtering)))))
    (setf (list-filtering-shown filtering) (and last (search-filter-kept last))
          (list-filtering-shown-count filtering) (if last (search-filter-count last) 0))))
