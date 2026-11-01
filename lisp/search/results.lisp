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
path:line: changeable (the rest stays as it is, and a line is not split or joined),
\\[search-results-apply] writes the lines changed into their files, each only while it still holds
what was found, and \\[search-results-discard] throws the changes away."
  :lighter "Found" :interactive nil)

(defvar *search-results-keymap* (mode-keymap 'search-results-mode)
  "Keys of the lines a search of files found (search-results-mode's keymap).")

(defun results-editing-p (buffer)
  "True when the text of BUFFER, lines a search found, can be changed."
  (tuatara.buffer:buffer-local buffer 'results-editing))

(defun results-header (buffer)
  "The words above BUFFER, lines a search found, on how to go on."
  (substitute-command-keys
   (if (results-editing-p buffer)
       (concatenate 'string " Changing: \\<search-results-mode>\\[search-results-apply] writes the lines changed"
                    " into their files, \\[search-results-discard] throws the changes away")
       " \\<search-results-mode>\\[search-results-edit] makes the lines changeable")))

(defun draw-results (buffer entries)
  "Make ENTRIES, result lines, the text of BUFFER, read-only, nothing to be written."
  (setf (tuatara.buffer:buffer-local buffer 'results-editing) nil
        (tuatara.buffer:buffer-local buffer :modal-class) nil)
  (render-pieces buffer (loop for entry across entries
                              collect (let ((text (or (result-line-text entry) "")))
                                        (concatenate 'string (result-line-prefix entry) text (string #\Newline))))
                 :entries entries :point :start :refill (lambda (buffer) (draw-results buffer entries)))
  (setf (tuatara.buffer:buffer-local buffer :header-line)
        (let ((header (results-header buffer))) (lambda (window) (declare (ignore window)) header))))

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
    (draw-results buffer entries)
    (use-places (make-found-lines (map 'vector #'candidate-value found) 0 words))
    (show-buffer buffer)))

(defmethod visit-entry ((mode search-results-mode) list entry other)
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
  "Make BUFFER, lines found, read-only again, nothing in it changed."
  (setf (tuatara.buffer:buffer-local buffer 'results-editing) nil
        (tuatara.buffer:buffer-local buffer :read-only) t
        (tuatara.buffer:buffer-local buffer :modal-class) nil
        (tuatara.buffer:buffer-local buffer :header-line)
        (let ((header (results-header buffer))) (lambda (window) (declare (ignore window)) header)))
  (tuatara.buffer:buffer-mark-saved buffer))

(defun results-writing (buffer)
  "The job writing the lines of BUFFER, lines found, into their files, or NIL."
  (tuatara.runtime:running-job buffer 'write-back-job))

(define-command search-results-edit ()
  "Make the text after path:line: of the lines found changeable, with the keys that change text: the
path and line number stay as they are, and a line is not split or joined.
\\<search-results-mode>\\[search-results-apply] writes the lines changed into their files,
\\[search-results-discard] throws the changes away."
  (let ((buffer (current-buffer)))
    (cond ((results-editing-p buffer) (message "The lines can be changed already"))
          (t (setf (tuatara.buffer:buffer-local buffer :read-only) nil
                   (tuatara.buffer:buffer-local buffer :modal-class) :editing
                   (tuatara.buffer:buffer-local buffer 'results-editing) t
                   (tuatara.buffer:buffer-local buffer :header-line)
                   (let ((header (results-header buffer))) (lambda (window) (declare (ignore window)) header)))
             (message "~a" (string-left-trim " " (results-header buffer)))))))

(define-command search-results-discard ()
  "Throw away the changes made to the lines found, and make them read-only again; the files are not touched."
  (let ((buffer (current-buffer)))
    (cond ((results-writing buffer) (message "Lines are being written: \\[stop-job] stops it first"))
          ((not (results-editing-p buffer)) (message "No lines are being changed"))
          (t (draw-results buffer (tuatara.buffer:buffer-local buffer 'list-entries))
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

;;; Writing them back

(defstruct (write-back-job (:include tuatara.runtime:job) (:constructor make-write-back-job (buffer files))
                           (:copier nil))
  "The lines changed in BUFFER, lines found, being written back into their files, a job
\(TUATARA.RUNTIME:JOB): FILES, each (PATH . CHANGES) not written yet, CHANGES each (ENTRY . NEW-TEXT);
TOTAL files in all; REQUEST, the file handed to the editor and ANSWERED when it has dealt with it;
WRITTEN lines written in FILES-WRITTEN files, REFUSED the reasons the others were not, and LEFT what
else is to say of a file, each a sentence."
  files (total (length files)) request (answered (sb-thread:make-semaphore)) (written 0) (files-written 0) (refused '())
  (left '()))

(setf (documentation 'write-back-job-p 'function) "True when OBJECT is a WRITE-BACK-JOB.")

(tuatara.runtime:define-job-words write-back-job
  :doing "Writing lines"
  :description (format nil "Writing ~:d line~:p found back into ~:d file~:p"
                       (loop for (nil . changes) in (write-back-job-files job) sum (length changes))
                       (write-back-job-total job))
  :verb "write the lines found"
  :left-words "the files already written stay written, the rest as they were")

(defmethod tuatara.runtime:job-percent ((job write-back-job))
  "How many of the files are dealt with."
  (tuatara.runtime:running-percent (- (write-back-job-total job) (length (write-back-job-files job)))
                                   (write-back-job-total job)))

(defun write-back-thread (job)
  "The work of JOB, on its thread: hand each file to the editor's tick (WRITE-BACK-TICK) and wait for
it to be dealt with, looking at whether it was asked to stop between the files."
  (loop while (write-back-job-files job)
        do (tuatara.runtime:job-check-stop)
           (setf (write-back-job-request job) (pop (write-back-job-files job)))
           (tuatara.runtime:job-wake-editor job)
           (loop until (sb-thread:wait-on-semaphore (write-back-job-answered job) :timeout 0.1)
                 do (tuatara.runtime:job-check-stop))))

(defun write-back-tick (editor)
  "Tick function: deal with the file a job writing lines back has handed over, on the editor's thread,
one for each time."
  (declare (ignore editor))
  (dolist (job tuatara.runtime:*jobs*)
    (when (write-back-job-p job)
      (let ((request (shiftf (write-back-job-request job) nil)))
        (when request
          (unwind-protect (write-back-file job (car request) (cdr request))
            (sb-thread:signal-semaphore (write-back-job-answered job))))))))

(defun path-write-refusal (path)
  "Why lines found in the file at PATH are not written back into it, a phrase, or NIL: it is not
there, it opens safely (evidence, a safe directory, a binary file), its project is only looked at, or
you may not write it."
  (let ((safe (visit-safely-reason path))
        (root (project-root (tuatara.io:parent-path path))))
    (cond ((not (eq (tuatara.io:path-kind path) :regular)) "the file is not there")
          (safe (format nil "it would open safely, ~a" safe))
          ((and root (project-look-only-reason root))
           (format nil "its project is looked at only, ~a" (project-look-only-reason root)))
          ((not (tuatara.io:writable-p path)) "you may not write it"))))

(defun buffer-write-refusal (buffer existing)
  "Why lines found are not written into BUFFER, a phrase, or NIL: it is opened safely or read-only, the
file changed on disk since it was read (for a buffer EXISTING already), or its lines are still being
counted."
  (cond ((tuatara.io:safe-visit-p buffer) (format nil "it is opened safely, ~a" (tuatara.io:safe-visit-reason buffer)))
        ((tuatara.buffer:buffer-local buffer :read-only) "its buffer is read-only")
        ((and existing (tuatara.io:file-disk-state buffer))
         "the file changed on disk since its buffer read it (\\[revert-buffer] reads it again)")
        ((lines-uncounted-p buffer) "its lines are still being counted")))

(defun line-write-edit (buffer entry text)
  "The edit that puts TEXT in the place of the line ENTRY was found on in BUFFER, keeping the white space
before it; or why the line is not changed, a phrase, as a second value, when it no longer holds the text found."
  (let ((number (1- (result-line-line entry))))
    (if (>= number (tuatara.buffer:buffer-line-count buffer))
        (values nil "the file has fewer lines now")
        (let* ((start (tuatara.buffer:buffer-goto-line buffer number))
               (end (tuatara.buffer:buffer-line-end buffer start))
               (now (tuatara.buffer:buffer-string buffer :start start :end end))
               (indent (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) now) (length now))))
          (if (string= (subseq now indent) (result-line-text entry))
              (tuatara.text:make-edit (+ start indent) end (tuatara.text:string-octets text))
              (values nil "the line no longer holds the text found"))))))

(defun write-back-file (job path changes)
  "Write CHANGES, each (ENTRY . NEW-TEXT), into the file at PATH for JOB: in its buffer, made for the
purpose when none is open, as one step of its undo, and the buffer saved (*SEARCH-RESULTS-SAVE-FILES*).
A line the file no longer holds as found is not written; what is not written, and why, is kept in
JOB's REFUSED."
  (flet ((refuse (why &optional (lines changes))
           (dolist (change lines)
             (push (format nil "~a:~d not written: ~a" (abbreviated-path path) (result-line-line (car change)) why)
                   (write-back-job-refused job)))))
    (let ((why (path-write-refusal path)))
      (when why
        (return-from write-back-file (refuse why))))
    (multiple-value-bind (buffer existing)
        (handler-case (let ((tuatara.buffer:*find-file-functions* '())) (tuatara.io:visit-file path))
          (error (condition) (return-from write-back-file (refuse (tuatara.runtime:condition-words condition)))))
      (let ((why (buffer-write-refusal buffer existing)))
        (when why
          (unless existing (tuatara.buffer:kill-buffer buffer))
          (return-from write-back-file (refuse why))))
      (let ((edits '()) (lines '()) (kept-modified (and existing (tuatara.buffer:buffer-modified-p buffer))))
        (dolist (change changes)
          (multiple-value-bind (edit why) (line-write-edit buffer (car change) (cdr change))
            (if edit
                (progn (push edit edits) (push change lines))
                (refuse why (list change)))))
        (when edits
          (setf edits (sort (coerce edits 'vector) #'< :key #'tuatara.text:edit-start))
          (handler-case (tuatara.buffer:with-narrowing-lifted (buffer) (tuatara.buffer:buffer-edit buffer edits))
            (error (condition)
              (unless existing (tuatara.buffer:kill-buffer buffer))
              (return-from write-back-file (refuse (tuatara.runtime:condition-words condition) lines))))
          (dolist (change lines)
            (setf (result-line-text (car change)) (cdr change)))
          (incf (write-back-job-written job) (length lines))
          (incf (write-back-job-files-written job))
          (cond ((and *search-results-save-files* (not kept-modified))
                 (unless (save-buffer-with-care buffer :then (and (not existing)
                                                                  (lambda (saved) (tuatara.buffer:kill-buffer saved))))
                   (push (format nil "~a changed but not saved" (abbreviated-path path)) (write-back-job-left job))
                   (unless existing (run-hook tuatara.buffer:*find-file-functions* buffer))))
                (t (unless existing (run-hook tuatara.buffer:*find-file-functions* buffer))
                   (when kept-modified
                     (push (format nil "~a has changes of yours not saved: left unsaved" (abbreviated-path path))
                           (write-back-job-left job))))))
        (when (and (null edits) (not existing))
          (tuatara.buffer:kill-buffer buffer))))))

(defmethod tuatara.runtime:job-take-in ((job write-back-job))
  "Say what was written and what was not, and make the lines read-only once none of them is
waiting to be written."
  (let ((buffer (tuatara.runtime:job-buffer job))
        (refused (reverse (write-back-job-refused job))))
    (loop for why in (append refused (reverse (write-back-job-left job)))
          for count below 20
          do (message "~a" why))
    (when (and (live-buffer-p buffer) (null (results-changes buffer)))
      (end-results-editing buffer))
    (message (concatenate 'string "~@[~a; ~]Wrote ~:d line~:p in ~:d file~:p"
                          "~@[; ~:d not written (*Messages* says why)~]"
                          "~:[~;; the lines still changed here wait for \\[search-results-apply]~]")
             (and (not (eq (tuatara.runtime:job-status job) :done)) (tuatara.runtime:job-ending-words job))
             (write-back-job-written job) (write-back-job-files-written job)
             (and refused (length refused))
             (and (live-buffer-p buffer) (results-editing-p buffer)))))

(define-command search-results-apply ()
  "Write the lines changed here into their files. A line is written only while the file still holds
the text that was found there; any other is not, and the reason is said. A file open in a buffer is
changed there, so that undoing in it takes the change back; any other is read for the purpose. The files
are saved (*SEARCH-RESULTS-SAVE-FILES*). A file that opens safely, evidence among them, a file in a
project looked at only, one that may not be written, and a read-only buffer are refused. It goes on in
the background, the editor free meanwhile, \\[stop-job] stopping it; lines written count as the text found,
and those not written stay changed here."
  (let ((buffer (current-buffer)))
    (cond ((results-writing buffer) (message "Lines are being written already"))
          ((not (results-editing-p buffer))
           (message "No lines are being changed: \\[search-results-edit] makes them changeable"))
          (t (let ((changes (results-changes buffer)))
               (if (null changes)
                   (progn (end-results-editing buffer) (message "No line changed"))
                   (let ((files '()))
                     (dolist (change changes)
                       (let ((entry (assoc (result-line-path (car change)) files :test #'equal)))
                         (if entry
                             (setf (cdr entry) (append (cdr entry) (list change)))
                             (setf files (append files (list (list (result-line-path (car change)) change)))))))
                     (tuatara.runtime:start-job-thread (make-write-back-job buffer files) #'write-back-thread
                                                       :name "tuatara writing lines found"))))))))

(defun install-search-results ()
  "Give the lines found their keys, keep the text of the lines changeable apart from what a change must
leave, and deal with the files a job writing lines back hands over."
  (define-keys *search-results-keymap* '(("RET" list-visit) ("C-x C-q" search-results-edit)
                                         ("C-c C-c" search-results-apply) ("C-c C-k" search-results-discard)))
  (commands-for-mode 'search-results-mode "the lines a search of files found (\\[search-files-list])"
                     'search-results-edit 'search-results-apply 'search-results-discard)
  (pushnew 'guard-result-lines tuatara.buffer:*before-edit-functions*)
  (pushnew 'write-back-tick *tick-functions*))
