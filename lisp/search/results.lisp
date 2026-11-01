;;;; lisp/search/results.lisp --- The lines a search of files found, changed in place and written back into their files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-s g's C-c C-l lists the lines found in a buffer of their own, path:line: text a line. C-x C-q there
;;;; makes the text after path:line: changeable (a guard on every edit keeps the rest as it is, and a
;;;; line a change would split or join), C-c C-c writes the lines changed into their files and C-c C-k
;;;; throws the changes away. The writing is a job (TUATARA.RUNTIME:JOB) with a thread of its own that
;;;; hands one file at a time to the editor's tick, where the file's buffer is changed, so that the
;;;; editor is free between two files, M-x stop-job stops it, and the mode line shows it. A file open in
;;;; a buffer is changed there, one step of its undo; any other is visited for the change and, when
;;;; saved, left again. A line is written only when the file's line still holds the text that was found.
;;;; A file that opens safely (evidence, a safe directory, a binary file), a project looked at only, a
;;;; file that may not be written and a buffer that is read-only are refused, each line with the reason.

(in-package #:tuatara.defaults)

(defvar *search-results-save-files* t
  "True to save the files that lines found by \\[search-in-files] are written back
into (\\[search-results-apply]), those that were not open too, closed again after;
NIL to leave each changed and not saved in a buffer of its own, for you to look at
and save (\\[save-buffer]). A file open with changes of yours not saved is never
saved by it.")

(defstruct (result-line (:constructor make-result-line (path line prefix text
                                                        &aux (prefix-octets (tuatara.text:string-octet-length prefix))))
                        (:copier nil))
  "A line a search of files found, as listed: the file's PATH, its LINE number from 1, the PREFIX
shown before it (path:line: ), and its TEXT as found without the white space before it, NIL for a
line that is not changed (cut short where it was listed, or holding a line break)."
  path line prefix text (prefix-octets 0 :type fixnum))

(setf (documentation 'result-line-p 'function) "True when OBJECT is a RESULT-LINE."
      (documentation 'result-line-path 'function) "The file a RESULT-LINE was found in."
      (documentation 'result-line-line 'function) "The number, from 1, of the line a RESULT-LINE stands for."
      (documentation 'result-line-prefix 'function) "The path:line: shown before a RESULT-LINE's text."
      (documentation 'result-line-text 'function) "A RESULT-LINE's text as found or last written, or NIL."
      (documentation 'result-line-prefix-octets 'function) "The octets of a RESULT-LINE's prefix.")

(define-major-mode search-results-mode special-mode
  "The lines a search of files found (\\[search-files-list]), one each as path:line: text.
\\<search-results-mode>\\[list-visit] opens the file at the line; \\[search-results-edit] makes the text after
path:line: changeable; the keys of the lines being changed are those of
\\<search-results-editing-mode>\\[search-results-apply] and \\[search-results-discard]."
  :lighter "Found" :interactive nil)

(define-major-mode search-results-editing-mode fundamental-mode
  "The lines a search of files found, being changed: type in the text after path:line: with
every key that changes text; the path and line number stay as they are, and a line is not split or
joined. \\<search-results-editing-mode>\\[search-results-apply] writes the lines changed into their files, each only
while it still holds what was found, \\[search-results-discard] throws the changes away, and
\\[list-visit] opens the file at the line."
  :lighter "Found*" :interactive nil)

(defvar *search-results-keymap* (mode-keymap 'search-results-mode)
  "Keys of the lines a search of files found (search-results-mode's keymap).")

(defvar *search-results-editing-keymap* (mode-keymap 'search-results-editing-mode)
  "Keys of the lines found while they are changed (search-results-editing-mode's keymap).")

(defun results-editing-p (buffer)
  "True when the text of BUFFER, lines a search found, can be changed."
  (tuatara.buffer:buffer-local buffer 'results-editing))

(defun results-header (buffer)
  "The words above BUFFER, lines a search found, on how to go on."
  (substitute-command-keys
   (if (results-editing-p buffer)
       (concatenate 'string " Changing: \\<search-results-editing-mode>\\[search-results-apply] writes,"
                    " \\[search-results-discard] throws away")
       " \\<search-results-mode>\\[search-results-edit] makes the lines changeable")))

(defun results-header-line (buffer)
  "Make the header of BUFFER, lines a search found, say how to go on as it is now."
  (setf (tuatara.display:buffer-header-line buffer)
        (lambda (window) (declare (ignore window)) (results-header buffer))))

(defun draw-results (buffer)
  "Make all the lines found that BUFFER lists, as they were found or last written, its text, read-only,
the lines no longer waiting to be written."
  (let ((entries (tuatara.buffer:buffer-local buffer 'results-all)))
    (setf (tuatara.buffer:buffer-local buffer 'results-editing) nil)
    (unless (typep (buffer-major-mode buffer) 'search-results-mode)
      (set-major-mode buffer 'search-results-mode))
    (render-pieces buffer (loop for entry across entries
                                collect (let ((text (or (result-line-text entry) "")))
                                          (concatenate 'string (result-line-prefix entry) text (string #\Newline))))
                   :entries entries :point :start :refill #'draw-results)
    (setf (buffer-read-only-p buffer) t)
    (results-header-line buffer)))

(defun show-search-results (found words)
  "List the lines FOUND, candidates of a search of files for WORDS, in a buffer of their own and show
it; the lines are also the places \\[next-place] goes through."
  (let* ((entries (coerce (loop for candidate in found
                                for (path line nil text) = (candidate-value candidate)
                                for string = (candidate-string candidate)
                                for changeable = (and text (notany (lambda (c) (member c '(#\Return #\Newline))) text))
                                collect (make-result-line
                                         path line
                                         (escape-controls (if changeable
                                                              (subseq string 0 (- (length string) (length text)))
                                                              string))
                                         (and changeable text)))
                          'simple-vector))
         (buffer (tuatara.buffer:make-buffer
                  (tuatara.buffer:unique-buffer-name
                   (format nil "*Found: ~a*"
                           (tuatara.display:shorten-string (escape-controls words) 40 :one-line t))))))
    (set-major-mode buffer 'search-results-mode)
    (setf (tuatara.buffer:buffer-local buffer 'results-all) entries)
    (draw-results buffer)
    (use-places (make-found-lines (map 'vector #'candidate-value found) 0 words))
    (show-buffer buffer)))

(defmethod visit-entry ((mode search-results-mode) list entry other)
  "Open the file ENTRY, a line found, is of, at its line."
  (declare (ignore list))
  (show-file-line (result-line-path entry) (result-line-line entry) :other other))

(defmethod visit-entry ((mode search-results-editing-mode) list entry other)
  "Open the file ENTRY, a line found, is of, at its line."
  (declare (ignore list))
  (show-file-line (result-line-path entry) (result-line-line entry) :other other))

;;; Changing the lines

(defun guard-result-lines (buffer edits)
  "Before-edit function (TUATARA.BUFFER:*BEFORE-EDIT-FUNCTIONS*): in a buffer of lines found that
can be changed, refuse EDITS reaching outside the text of one line that can be changed, and any that
puts a line break in it."
  (when (results-editing-p buffer)
    (loop for edit across edits
          for start = (tuatara.text:edit-start edit)
          for entry = (list-entry-at buffer start)
          do (unless (and entry (result-line-text entry)
                          (>= start (+ (tuatara.buffer:buffer-line-start buffer start)
                                       (result-line-prefix-octets entry)))
                          (<= (tuatara.text:edit-end edit) (tuatara.buffer:buffer-line-end buffer start))
                          (notany (lambda (octet) (member octet '(10 13))) (tuatara.text:edit-insert edit)))
               (error "Only the text after path:line: of a line that is not cut short changes here, ~
                       and a line stays one")))))

(defun end-results-editing (buffer)
  "Make BUFFER, lines found, read-only again, as they are now."
  (when (results-editing-p buffer)
    (let ((point (tuatara.buffer:buffer-point buffer)))
      (draw-results buffer)
      (setf (tuatara.buffer:buffer-point buffer) (min point (tuatara.buffer:buffer-size buffer))))))

(defun results-writing (buffer)
  "The job writing the lines of BUFFER, lines found, into their files, or NIL."
  (tuatara.runtime:running-job buffer 'write-back-job))

(define-command search-results-edit ()
  "Make the text after path:line: of the lines found changeable, with the keys
that change text: the path and line number stay as they are, and a line is
not split or joined. Then \\<search-results-editing-mode>\\[search-results-apply] writes the lines changed into
their files and \\[search-results-discard] throws the changes away; the keys that list,
revert or leave are typed text meanwhile."
  (let ((buffer (current-buffer)))
    (unless (results-editing-p buffer)
      (set-major-mode buffer 'search-results-editing-mode)
      (setf (buffer-read-only-p buffer) nil
            (tuatara.buffer:buffer-local buffer 'results-editing) t)
      (results-header-line buffer)
      (message "~a" (string-left-trim " " (results-header buffer))))))

(define-command search-results-discard ()
  "Throw away the changes made to the lines found, and make them read-only again; the files are not touched."
  (let ((buffer (current-buffer)))
    (cond ((results-writing buffer) (message "Lines are being written: \\[stop-job] stops it first"))
          ((not (results-editing-p buffer)) (message "No lines are being changed"))
          (t (draw-results buffer)
             (message "Changes thrown away")))))

(defun results-changes (buffer)
  "The lines of BUFFER, lines found, whose text was changed, each (ENTRY . NEW-TEXT), in the order
listed."
  (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries))
        (first (tuatara.buffer:buffer-local buffer 'list-first-line 0)))
    (loop for entry across entries
          for index from first
          for original = (result-line-text entry)
          for start = (and original (tuatara.buffer:buffer-goto-line buffer index))
          for now = (and start (tuatara.buffer:buffer-string
                                buffer :start (+ start (result-line-prefix-octets entry))
                                       :end (tuatara.buffer:buffer-line-end buffer start)))
          when (and now (string/= now original))
            collect (cons entry now))))

(defun changes-by-file (changes)
  "CHANGES, each (ENTRY . NEW-TEXT), as the files they are for in the order listed, each (PATH . LINES), the
lines each (NUMBER ENTRIES . TEXT) in the order of their numbers: a line listed twice is one, with both
entries, TEXT :CONFLICT when the two were changed to different text."
  (let ((files '()))
    (dolist (change changes)
      (let* ((entry (car change))
             (file (or (assoc (result-line-path entry) files :test #'equal)
                       (car (push (list (result-line-path entry)) files))))
             (line (find (result-line-line entry) (cdr file) :key #'first)))
        (cond ((null line) (push (list* (result-line-line entry) (list entry) (cdr change)) (cdr file)))
              (t (push entry (second line))
                 (unless (equal (cddr line) (cdr change)) (setf (cddr line) :conflict))))))
    (loop for file in (reverse files)
          collect (cons (car file) (sort (cdr file) #'< :key #'first)))))

;;; Writing them back

(defstruct (write-back-job (:include tuatara.runtime:job) (:constructor make-write-back-job (buffer files))
                           (:copier nil))
  "The lines changed in BUFFER, lines found, being written back into their files, a job
\(TUATARA.RUNTIME:JOB): FILES, each (PATH . LINES) not handed over yet (CHANGES-BY-FILE); TOTAL files
in all; REQUEST, the file handed to the editor and ANSWERED when it has dealt with it; ROOTS, what
is known of whether the projects are looked at only; SAVING, the buffers being saved, each (BUFFER
LINES MADE DONE); WRITTEN lines written in FILES-WRITTEN files, REFUSED the reasons the others were
not, and LEFT what else is to say of a file, each a sentence."
  files (total (length files)) request (answered (sb-thread:make-semaphore))
  (roots (make-hash-table :test 'equal)) (saving '()) (written 0) (files-written 0) (refused '()) (left '()))

(setf (documentation 'write-back-job-p 'function) "True when OBJECT is a WRITE-BACK-JOB.")

(tuatara.runtime:define-job-words write-back-job
  :doing "Writing lines"
  :description (format nil "Writing the lines found back into ~:d file~:p" (write-back-job-total job))
  :verb "write the lines found"
  :left-words "the files already written stay written, the rest as they were")

(defmethod tuatara.runtime:job-percent ((job write-back-job))
  "How many of the files are dealt with."
  (tuatara.runtime:running-percent (- (write-back-job-total job) (length (write-back-job-files job)))
                                   (write-back-job-total job)))

(defun look-only-reason-of (job root)
  "Why the project at ROOT is only looked at (PROJECT-LOOK-ONLY-REASON), asked once for JOB, waiting while
the folders under it are looked through for evidence folders; NIL when it is not."
  (multiple-value-bind (kept found) (gethash root (write-back-job-roots job))
    (if found
        (and (stringp kept) kept)
        (let ((reason (loop repeat 1200
                            for reason = (project-look-only-reason root)
                            while (and reason (search "is being looked through" reason))
                            do (tuatara.runtime:job-check-stop) (sleep 0.05)
                            finally (return reason))))
          (setf (gethash root (write-back-job-roots job)) (or reason :none))
          reason))))

(defun path-write-refusal (job path)
  "Why lines found in the file at PATH, where it really is, are not written back into it, a phrase, or NIL:
it is not there, it opens safely (evidence, a safe directory, a binary file), its project is only looked at,
or you may not write it."
  (let ((safe (visit-safely-reason path))
        (root (project-root (tuatara.io:parent-path path))))
    (cond ((not (eq (tuatara.io:path-kind path) :regular)) "the file is not there")
          (safe (format nil "it would open safely, ~a" safe))
          ((and root (look-only-reason-of job root))
           (format nil "its project is looked at only, ~a" (look-only-reason-of job root)))
          ((not (tuatara.io:writable-p path)) "you may not write it"))))

(defun locate-lines (path numbers)
  "Where the lines of the file at PATH numbered NUMBERS, ascending from 1, are, read a piece at a time on
the job's thread: each (NUMBER START . END), octets, the line end left out. A line ends with a newline,
a carriage return and a newline, or a carriage return alone, as a search of files counts them; a
line the file has not is left out."
  (let ((fd (tuatara.io:open-for-reading path t :accept '(:regular)))
        (piece (make-array (* 4 1024 1024) :element-type '(unsigned-byte 8)))
        (found '()) (wanted numbers) (line 1) (line-start 0) (offset 0) (pending nil))
    (declare (type fixnum line line-start offset) (type (simple-array (unsigned-byte 8) (*)) piece))
    (unwind-protect
         (loop while wanted
               do (tuatara.runtime:job-check-stop)
                  (let ((got (tuatara.io:read-fd-at fd piece offset :path path))
                        (from 0))
                    (declare (type fixnum got from))
                    (when (zerop got) (return))
                    ;; the last piece ended with a carriage return: a newline now is part of its line end
                    (when pending
                      (setf pending nil)
                      (if (= 10 (aref piece 0))
                          (setf from 1 line-start (1+ offset))
                          (setf line-start offset)))
                    (loop for at = (position-if (lambda (octet) (or (= octet 10) (= octet 13))) piece
                                                :start from :end got)
                          while (and at wanted)
                          do (let ((end (+ offset at)))
                               (when (= line (car wanted))
                                 (push (list* line line-start end) found)
                                 (pop wanted))
                               (incf line)
                               (cond ((/= 13 (aref piece at)) (setf line-start (1+ end) from (1+ at)))
                                     ((= (1+ at) got) (setf pending t from got))
                                     ((= 10 (aref piece (1+ at))) (setf line-start (+ end 2) from (+ at 2)))
                                     (t (setf line-start (1+ end) from (1+ at))))))
                    (incf offset got)))
      (ignoring-errors-reported ("closing ~a" path) (sb-posix:close fd)))
    (when pending (setf line-start offset))
    (when (and wanted (= line (car wanted)) (> offset line-start))
      (push (list* line line-start offset) found))
    (nreverse found)))

(defun write-back-thread (job)
  "The work of JOB, on its thread: for each file, ask whether it may be written, find its lines on disk, hand
it to the editor's tick (WRITE-BACK-TICK) and wait for it to be dealt with, looking at whether it was
asked to stop between the files; at the end wait for the saves to end."
  (flet ((refuse (path lines why)
           (dolist (line lines)
             (push (format nil "~a:~d not written: ~a" (abbreviated-path path) (first line) why)
                   (write-back-job-refused job)))))
    (loop while (write-back-job-files job)
          do (tuatara.runtime:job-check-stop)
             (destructuring-bind (given . lines) (pop (write-back-job-files job))
               (let* ((path (or (ignoring-errors-reported ("following the links of ~a" given)
                                       (tuatara.io:resolve-links given))
                                     given))
                      (why (path-write-refusal job path)))
                 (if why
                     (refuse given lines why)
                     (let ((where (handler-case (reporting-errors ("finding the lines of ~a" path)
                                                  (locate-lines path (mapcar #'first lines)))
                                    (error (condition) (refuse given lines (tuatara.runtime:condition-words condition))
                                      nil))))
                       (when where
                         (dolist (line (remove-if (lambda (line) (assoc (first line) where)) lines))
                           (refuse given (list line) "the file has fewer lines now"))
                         (setf (write-back-job-request job)
                               (list given path (loop for line in lines
                                                      for place = (assoc (first line) where)
                                                      when place collect (list line (second place) (cddr place)))))
                         (tuatara.runtime:job-wake-editor job)
                         (loop until (sb-thread:wait-on-semaphore (write-back-job-answered job) :timeout 0.1)
                               do (tuatara.runtime:job-check-stop))))))))
    (loop with idle = nil
          for open = (remove-if #'fourth (write-back-job-saving job))
          while open
          do (tuatara.runtime:job-check-stop)
             (if (some (lambda (record) (tuatara.io:save-of (first record))) open)
                 (setf idle nil)
                 (setf idle (or idle (get-internal-real-time))))
             (when (and idle (> (- (get-internal-real-time) idle) (* 3 internal-time-units-per-second)))
               (return))
             (sleep 0.05))))

(defun write-back-tick (editor)
  "Tick function: deal with the file a job writing lines back has handed over, on the editor's thread,
one for each time."
  (declare (ignore editor))
  (dolist (job tuatara.runtime:*jobs*)
    (when (write-back-job-p job)
      (let ((request (shiftf (write-back-job-request job) nil)))
        (when request
          (unwind-protect (apply #'write-back-file job request)
            (sb-thread:signal-semaphore (write-back-job-answered job))))))))

(defun buffer-write-refusal (buffer existing)
  "Why lines found are not written into BUFFER, a phrase, or NIL: it is opened safely or read-only, or the
file changed on disk since it was read (for a buffer EXISTING already)."
  (cond ((tuatara.io:safe-visit-p buffer) (format nil "it is opened safely, ~a" (tuatara.io:safe-visit-reason buffer)))
        ((buffer-read-only-p buffer) "its buffer is read-only")
        ((and existing (tuatara.io:file-disk-state buffer))
         "the file changed on disk since its buffer read it (\\[revert-buffer] reads it again)")))

(defun line-write-edit (buffer entry text &optional start end)
  "The edit that puts TEXT in the place of the line ENTRY was found on in BUFFER, keeping the white space
before it; or why the line is not changed, a phrase, as a second value, when it no longer holds the text found.
The line lies from the octet START to END when those are given, else it is found by its number."
  (handler-case
      (reporting-errors ("finding the line of ~a" (tuatara.buffer:buffer-name buffer))
        (unless start
          (let ((number (1- (result-line-line entry))))
            (when (>= number (tuatara.buffer:buffer-line-count buffer))
              (return-from line-write-edit (values nil "the file has fewer lines now")))
            (setf start (tuatara.buffer:buffer-goto-line buffer number)
                  end (tuatara.buffer:buffer-line-end buffer start))))
        (if (> end (tuatara.buffer:buffer-size buffer))
            (values nil "the file has fewer lines now")
            (let* ((now (tuatara.buffer:buffer-string buffer :start start :end end))
                   (indent (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) now) (length now))))
              (if (string= (subseq now indent) (result-line-text entry))
                  (tuatara.text:make-edit (+ start indent) end (tuatara.text:string-octets text))
                  (values nil "the line no longer holds the text found")))))
    (error () (values nil "the line no longer holds the text found"))))

(defun save-finished (job record)
  "Note that the buffer of RECORD, one of JOB's SAVING, is saved: its lines count as written, and a buffer
made for the purpose goes."
  (destructuring-bind (buffer lines made &rest done) record
    (declare (ignore done))
    (setf (fourth record) t)
    (incf (write-back-job-written job) lines)
    (incf (write-back-job-files-written job))
    (when (and made (tuatara.buffer:live-buffer-p buffer))
      (tuatara.buffer:kill-buffer buffer))))

(defun write-back-file (job given path located)
  "Write the lines LOCATED, each ((NUMBER ENTRIES . TEXT) START END), into the file at PATH, where it really
is (GIVEN as listed), for JOB: in its buffer, made for the purpose when none is open, as one step of its
undo. A buffer with no changes of yours is the file as it is on disk, the lines found by their octets; one
with changes is edited at its own line of that number and left unsaved, said. A line the file no longer
holds as found is not written; what is not written, and why, is kept in JOB's REFUSED. The buffer is
saved (*SEARCH-RESULTS-SAVE-FILES*), without a question: a save that would ask is refused, and the
lines count as written once the save has ended."
  (flet ((refuse (why &optional (lines (mapcar #'first located)))
           (dolist (line lines)
             (push (format nil "~a:~d not written: ~a" (abbreviated-path given) (first line) why)
                   (write-back-job-refused job)))))
    (multiple-value-bind (buffer existing)
        (handler-case (reporting-errors ("visiting ~a to write lines back" path)
                        (let ((tuatara.buffer:*find-file-functions* '())) (tuatara.io:visit-file path)))
          (error (condition) (return-from write-back-file (refuse (tuatara.runtime:condition-words condition)))))
      (flet ((give-up (why)
               (unless existing (tuatara.buffer:kill-buffer buffer))
               (return-from write-back-file (refuse why))))
        (let ((why (buffer-write-refusal buffer existing)))
          (when why (give-up why)))
        (let* ((kept-modified (and existing (tuatara.buffer:buffer-modified-p buffer)))
               (by-octets (not (tuatara.buffer:buffer-modified-p buffer)))
               (edits '()) (lines '()))
          (when (and (not by-octets) (lines-uncounted-p buffer))
            (give-up "its lines are still being counted"))
          (dolist (place located)
            (destructuring-bind ((number entries . text) start end) place
              (declare (ignore number))
              (if (eq text :conflict)
                  (refuse "the line is listed more than once and was changed to different text" (list (first place)))
                  (multiple-value-bind (edit why)
                      (if by-octets
                          (line-write-edit buffer (first entries) text start end)
                          (line-write-edit buffer (first entries) text))
                    (if edit
                        (progn (push edit edits) (push (first place) lines))
                        (refuse why (list (first place))))))))
          (when edits
            (setf edits (sort (coerce edits 'vector) #'< :key #'tuatara.text:edit-start))
            (handler-case (reporting-errors ("writing lines back to ~a" path)
                            (tuatara.buffer:with-narrowing-lifted (buffer) (tuatara.buffer:buffer-edit buffer edits)))
              (error (condition)
                (unless existing (tuatara.buffer:kill-buffer buffer))
                (return-from write-back-file (refuse (tuatara.runtime:condition-words condition) lines))))
            (dolist (line lines)
              (dolist (entry (second line))
                (setf (result-line-text entry) (cddr line))))
            (flet ((keep-open (note)
                     (unless existing (run-hook tuatara.buffer:*find-file-functions* buffer))
                     (incf (write-back-job-written job) (length lines))
                     (incf (write-back-job-files-written job))
                     (push (format nil "~a ~a" (abbreviated-path given) note) (write-back-job-left job))))
              (cond ((not *search-results-save-files*) (keep-open "is changed in its buffer, not saved"))
                    (kept-modified
                     (keep-open "has changes of yours not saved: the lines are in its buffer, unsaved"))
                    (t (let ((record (list buffer (length lines) (not existing) nil)))
                         (push record (write-back-job-saving job))
                         (unless (let ((*save-wait* 0))
                                   (save-buffer-with-care buffer :quiet t
                                                               :then (lambda (saved)
                                                                       (declare (ignore saved))
                                                                       (save-finished job record))))
                           (setf (write-back-job-saving job) (remove record (write-back-job-saving job)))
                           (unless existing (run-hook tuatara.buffer:*find-file-functions* buffer))
                           (push (format nil "~a changed but not saved" (abbreviated-path given))
                                 (write-back-job-left job))))))))
          (when (and (null edits) (not existing))
            (tuatara.buffer:kill-buffer buffer)))))))

(defmethod tuatara.runtime:job-take-in ((job write-back-job))
  "Say what was written and what was not, and make the lines read-only once none of them is
waiting to be written."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (refused (reverse (write-back-job-refused job)))
         (unsaved (loop for record in (write-back-job-saving job)
                        unless (fourth record)
                          collect (format nil "~a changed but not saved" (tuatara.buffer:buffer-name (first record))))))
    (loop for why in (append refused (reverse (write-back-job-left job)) unsaved)
          for count below 20
          do (message "~a" why))
    (when (and (live-buffer-p buffer) (null (results-changes buffer)))
      (end-results-editing buffer))
    (message (concatenate 'string "~@[~a; ~]Wrote ~:d line~:p in ~:d file~:p"
                          "~@[; ~:d not written (*Messages* says why)~]"
                          "~:[~;; the lines still changed here wait for "
                          "\\<search-results-editing-mode>\\[search-results-apply]~]")
             (and (not (eq (tuatara.runtime:job-status job) :done)) (tuatara.runtime:job-ending-words job))
             (write-back-job-written job) (write-back-job-files-written job)
             (and (or refused unsaved) (+ (length refused) (length unsaved)))
             (and (live-buffer-p buffer) (results-editing-p buffer)))))

(define-command search-results-apply ()
  "Write the lines changed here into their files. A line is written only while the file still holds
the text that was found there; any other is not, and the reason is said. A file open in a buffer is
changed there, so that undoing in it takes the change back; one with changes of yours not saved is
changed at its own line of that number and left unsaved, said. Any other file is read for the purpose
and closed again. The files are saved without a question (*SEARCH-RESULTS-SAVE-FILES*), with whatever
a save does in that buffer's mode, and a save that would ask (the file changed on disk, write-protected)
is refused. A file that opens safely, evidence among them, one reached through a link, a file in a
project looked at only, one that may not be written, and a read-only buffer are refused. It goes on in
the background, the editor free meanwhile, \\[stop-job] stopping it; lines written count as the text found,
and those not written stay changed here. A line listed twice and changed to the same text is written
once."
  (let ((buffer (current-buffer)))
    (cond ((results-writing buffer) (message "Lines are being written already"))
          ((not (results-editing-p buffer))
           (message "No lines are being changed: \\<search-results-mode>\\[search-results-edit] makes them changeable"))
          (t (let ((changes (results-changes buffer)))
               (if (null changes)
                   (progn (end-results-editing buffer) (message "No line changed"))
                   (tuatara.runtime:start-job-thread (make-write-back-job buffer (changes-by-file changes))
                                                     #'write-back-thread :name "tuatara writing lines found")))))))

(defun install-search-results ()
  "Give the lines found their keys, keep the text of the lines changeable apart from what a change must
leave, and deal with the files a job writing lines back hands over."
  (define-keys *search-results-keymap* '(("RET" list-visit) ("C-x C-q" search-results-edit)))
  (define-keys *search-results-editing-keymap* '(("RET" list-visit) ("C-c C-c" search-results-apply)
                                                 ("C-c C-k" search-results-discard)))
  (commands-for-mode 'search-results-mode "the lines a search of files found (\\[search-files-list])"
                     'search-results-edit)
  (commands-for-mode 'search-results-editing-mode
                     "the lines a search of files found being changed (\\[search-results-edit])"
                     'search-results-apply 'search-results-discard)
  (pushnew 'guard-result-lines tuatara.buffer:*before-edit-functions*)
  (pushnew 'write-back-tick *tick-functions*))
