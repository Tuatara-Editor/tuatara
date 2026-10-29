;;;; lisp/org/agenda.lisp --- What Org's agenda lists: the entries of a day, the TODO list, a match.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The entries of the agenda files are read as Org's agenda reads them,
;;;; and listed as Org lists them: for a day, the deadlines (on their day, as
;;;; a warning before it, as overdue after it), the scheduled tasks (on their
;;;; day, carried to today until done, on the repeats to come), the
;;;; timestamps (on their day and their repeats), the ranges of days, and in
;;;; log mode what was closed and clocked; the tasks not done (the TODO list);
;;;; the entries a match of tags, properties and keywords selects. Each kind
;;;; is ordered as Org orders it. How they are shown is another matter.

(in-package #:tuatara.defaults)

(defparameter *org-done-note-scanner*
  (cl-ppcre:create-scanner (format nil "- State \"DONE\"[^[\\n]*(\\[~a\\])" (org-timestamp-body)))
  "A note of an entry's state becoming DONE, the time it was noted a group.")

(defparameter *org-boxes-ticked-scanner*
  (cl-ppcre:create-scanner "^[ \\t]+([0-9]+) of ([0-9]+) (?:checkboxes ticked|sub-tasks done)[ \\t]*$")
  "The line under a note of DONE saying how many checkboxes were ticked or
sub-tasks done then (*ORG-BOXES-TICKED-NOTE*), the two counts groups.")

(defparameter *org-agenda-stamp-scanner*
  (let ((stamp (org-timestamp-body)))
    (cl-ppcre:create-scanner (format nil "(<~a>--<~a>)|<~a>|<~a>" stamp stamp stamp (org-rule-timestamp-body))))
  "What the agenda takes from an entry's text, in one look: a range of two
active timestamps (a group), one active timestamp, or a timestamp by rule.")

(defparameter *org-closed-or-clock-scanner*
  (cl-ppcre:create-scanner (format nil "\\b(?:(CLOSED)|CLOCK): *(\\[~a\\])" (org-timestamp-body)))
  "A CLOSED: or CLOCK: and the inactive timestamp after it: CLOSED a group when
it is that, the timestamp the second.")

(defvar *org-agenda-files* '("~/org/agenda.org")
  "The files the agenda reads: each a file, a folder (every .org file in it,
those whose names begin with a dot left out) or a pattern of names in a
folder, * for any characters and ? for one (\"~/notes/*.org\"), names
beginning with a dot only for a pattern that does.")

(defvar *org-agenda-file-functions* '()
  "Functions of no arguments, each giving a list of more files the agenda
reads, written as *ORG-AGENDA-FILES* writes them: a package's own files join
the agenda this way, the setting left as you wrote it. What they give is
kept *ORG-AGENDA-FILES-ADDED-SECONDS*, the agenda asking for its files as
often as it is drawn. One that fails gives none, and debug mode reports
it.")

(defparameter *org-agenda-files-added-seconds* 2
  "How long the files *ORG-AGENDA-FILE-FUNCTIONS* gave are kept before they
are asked again.")

(defvar *org-agenda-files-added* nil
  "The files *ORG-AGENDA-FILE-FUNCTIONS* gave last: (FUNCTIONS TIME . FILES),
TIME in internal time units.")

(defun org-agenda-files-added ()
  "The files *ORG-AGENDA-FILE-FUNCTIONS* give, each function's in turn: those
given less than *ORG-AGENDA-FILES-ADDED-SECONDS* ago, by the same functions,
else asked again."
  (let ((kept *org-agenda-files-added*)
        (now (get-internal-real-time)))
    (if (and kept (equal (first kept) *org-agenda-file-functions*)
             (< (- now (second kept)) (* *org-agenda-files-added-seconds* internal-time-units-per-second)))
        (cddr kept)
        (let ((files (loop for function in *org-agenda-file-functions*
                           append (handler-case (copy-list (funcall function))
                                    (error (e)
                                      (tuatara.runtime:debug-report :error "Agenda files from ~a not had: ~a" function e)
                                      nil)))))
          (setf *org-agenda-files-added* (list* (copy-list *org-agenda-file-functions*) now files))
          files))))

(defun org-agenda-files-in-force ()
  "The files the agenda reads, written as *ORG-AGENDA-FILES* writes them: the
setting's, then those *ORG-AGENDA-FILE-FUNCTIONS* give (ORG-AGENDA-FILES-ADDED),
each once."
  (if (null *org-agenda-file-functions*)
      *org-agenda-files*
      (remove-duplicates (append *org-agenda-files* (org-agenda-files-added)) :test #'equal :from-end t)))

(defvar *org-agenda-date-in-title* nil
  "True to keep the dates written in a heading or a checkbox's line in its
title on the agenda's days, as Org shows them; NIL takes them out there,
the day and the time column saying them already (the TODO list and matches
keep them).")

(defvar *org-deadline-warning-days* 14
  "How many days before its deadline a task is listed on today; a deadline's
own -3d says otherwise.")

(defvar *org-agenda-log-mode* nil
  "True for the agenda to list what was closed and clocked on each day too.")

;;; Days

(defvar *org-agenda-ts-memo* nil
  "While an agenda is made, a table of the timestamps read so far, by their
text, and of the days each rule opens on (ORG-AGENDA-RULE-OPEN), by the rule.")

(defun memo-values (table key function)
  "The values FUNCTION, of no arguments, gives: kept in TABLE, a hash table,
under KEY the first time and given from there after; worked out each time
when TABLE is NIL."
  (if table
      (multiple-value-bind (kept found) (gethash key table)
        (values-list (if found kept (setf (gethash key table) (multiple-value-list (funcall function))))))
      (funcall function)))

(defun org-agenda-ts (text)
  "TEXT read as ORG-READ-TS reads it, once while an agenda is made. The
timestamp is shared: it is not to be changed."
  (and (not (org-rule-text-p text))
       (memo-values *org-agenda-ts-memo* text (lambda () (org-read-ts text)))))

(defun org-agenda-rule (text)
  "TEXT read as ORG-READ-RULE reads it, once while an agenda is made."
  (and (org-rule-text-p text)
       (memo-values *org-agenda-ts-memo* text (lambda () (org-read-rule text)))))

(defun org-repeater-of (text)
  "The repeater the agenda finds in the timestamp TEXT, read once: (TS VALUE
UNIT START), TS the timestamp read and START its day; NIL when it has none
(or repeats every 0)."
  (let* ((ts (org-agenda-ts text))
         (value (and ts (org-ts-repeat-value ts))))
    (and value (plusp value)
         (list ts value (org-ts-repeat-unit ts) (org-ts-day-number ts)))))

(defun org-repeat-day (repeater current prefer)
  "The day of the timestamp whose REPEATER (as ORG-REPEATER-OF gives it) is
nearest the day CURRENT: the repeat at or
before CURRENT (PREFER :past), at or after it (:future), or the nearer."
  (destructuring-bind (ts value unit start) repeater
    (if (<= current start)
        start
        (multiple-value-bind (n1 n2)
            (ecase unit
              (#\h (let ((missing (mod (- (* 24 (- current start)) (or (org-ts-hour ts) 0)) value)))
                     (values (if (zerop missing) current (- current (1+ (floor missing 24))))
                             (+ current (floor (- value missing) 24)))))
              ((#\d #\w) (let* ((step (if (char= unit #\w) (* 7 value) value))
                                (n1 (+ start (* step (floor (- current start) step)))))
                           (values n1 (+ n1 step))))
              (#\m (multiple-value-bind (ty tm td) (day-date current)
                     (let* ((by (org-ts-year ts)) (bm (org-ts-month ts)) (bd (org-ts-day ts))
                            (months (* value (truncate (+ (* 12 (- ty by)) (- tm bm) (if (> td bd) 0 -1)) value))))
                       (values (day-number by (+ bm months) bd) (day-number by (+ bm months value) bd)))))
              (#\y (multiple-value-bind (ty tm td) (day-date current)
                     (let* ((by (org-ts-year ts)) (bm (org-ts-month ts)) (bd (org-ts-day ts))
                            (years (* value (truncate (- ty by (if (or (> tm bm) (and (= tm bm) (> td bd))) 0 1)) value))))
                       (values (day-number (+ by years) bm bd) (day-number (+ by years value) bm bd))))))
          (case prefer
            (:past (if (= current n2) n2 n1))
            (:future (if (= current n1) n1 n2))
            (t (if (> (abs (- current n1)) (abs (- current n2))) n2 n1)))))))

(defun org-warning-days (ts)
  "The days of warning before the deadline TS (an ORG-TS) its -3d says, or
the days a scheduled TS waits; NIL when it says none."
  (and (org-ts-warning ts)
       (floor (* (org-ts-warning-value ts)
                 (ecase (org-ts-warning-unit ts) (#\d 1) (#\w 7) (#\m 152/5) (#\y 1461/4) (#\h 1/24))))))

;;; Entries of the files

(defstruct (org-agenda-entry (:conc-name agenda-entry-))
  "A headline of an agenda file, as the agenda reads it: its FILE (path) and
TEXT, the character it begins at (POSITION) and where its own text ends (END),
its LEVEL, TODO keyword and TODO-TYPE (TODO, DONE or NIL), PRIORITY (Org's
number: 2000 for [#A], 1000 for B and none, 0 for C), TITLE, TAGS (inherited
first) and LOCAL-TAGS (its own, on its headline), CATEGORY, its planning's SCHEDULED, DEADLINE and CLOSED timestamps
(texts), HEADING, its line after the stars, its PROPERTIES (an alist), its
plain active STAMPS (texts), its RANGES of days ((FROM TO) texts) and its LOGS
((KIND TIMESTAMP DURATION END) of its CLOSED: and CLOCK: lines, END the time a
clock stopped), and the FACTS
of its dates the agenda works with, read once (ORG-ENTRY-FACTS). Its KIND is
:HEADLINE, or :CHECKBOX for a list item with a checkbox kept as a task (an open
box TODO, a ticked one DONE); SKIP, the stretches of its text that are its
checkbox items' own, (START . END), read for them rather than for it."
  file text position end level todo todo-type priority title tags local-tags category scheduled deadline closed heading
  properties stamps ranges logs facts (kind :headline) skip)

(defun org-known-tags (&rest buffers)
  "The tags used in BUFFERS and in the agenda files: the headings' and the
#+FILETAGS:, each once, in the order first met; the agenda files' as the
agenda keeps them (ORG-AGENDA-ENTRIES), never opened for it."
  (let ((tags '()))
    (dolist (buffer buffers)
      (when buffer
        (multiple-value-bind (tree text) (org-tree-now buffer)
          (when tree
            (dolist (tag (org-file-tags text)) (pushnew tag tags :test #'string=))
            (dolist (headline (org-headlines tree))
              (dolist (tag (org-property headline :tags)) (pushnew tag tags :test #'string=)))))))
    (dolist (path (ignore-errors (org-agenda-file-paths)))
      (dolist (entry (ignore-errors (org-agenda-entries path)))
        (dolist (tag (agenda-entry-tags entry)) (pushnew tag tags :test #'string=))))
    (nreverse tags)))

(defun org-heading-priority (heading)
  "The priority Org gives HEADING's text: 1000 for each step above C, B with no cookie."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "\\[#([A-Z0-9])\\]" heading)
    (* 1000 (- (char-code #\C) (char-code (if match (char (aref groups 0) 0) #\B))))))

(defun org-file-keyword (text name)
  "The value of TEXT's first #+NAME: line, NAME in any case, blanks about it
left out; NIL when it has none or that line's is empty."
  (flet ((blank-p (c) (or (char= c #\Space) (char= c #\Tab))))
    (let ((length (length text)) (size (length name)))
      (loop for start = 0 then (1+ end)
            for end = (or (position #\Newline text :start start) length)
            do (let ((at (or (position-if-not #'blank-p text :start start :end end) end)))
                 (when (and (< (+ at 2 size) end)
                            (string= "#+" text :start2 at :end2 (+ at 2))
                            (string-equal name text :start2 (+ at 2) :end2 (+ at 2 size))
                            (char= (char text (+ at 2 size)) #\:))
                   (let* ((from (or (position-if-not #'blank-p text :start (+ at 3 size) :end end) end))
                          (to (if (< from end) (1+ (position-if-not #'blank-p text :start from :end end :from-end t)) from)))
                     (return (and (< from to) (subseq text from to))))))
            until (>= end length)))))

(defun org-file-tags (text)
  "The tags TEXT's #+FILETAGS: line gives every headline of it, in order."
  (let ((tags (org-file-keyword text "FILETAGS")))
    (and tags (tuatara.text:split-words tags ": "))))

(defun org-file-category (path text)
  "The category of the Org file at PATH, its TEXT: its #+CATEGORY:, else its
name without its last extension; NIL for no file and no #+CATEGORY:."
  (or (org-file-keyword text "CATEGORY")
      (and path (tuatara.io:native-file-stem path))))

(defun org-merge-tags (inherited own)
  "The tags of a headline whose own are OWN, under headlines and a file whose
tags are INHERITED: each once, where it comes last, so that its own stand at
the end."
  (remove-duplicates (append inherited own) :test #'string=))

(defun org-walk-entries (function path text tree &key agenda root)
  "Call FUNCTION on each headline of the Org file at PATH (TEXT its text, TREE
its parse), in order, with its ORG-AGENDA-ENTRY (its tags with those it
inherits, its category, planning and properties) and its node. With AGENDA,
the headlines the agenda leaves out are left out: those under a COMMENT
headline or a headline tagged ARCHIVE, the headline too, and all of a file
whose #+FILETAGS: has ARCHIVE. With ROOT, that headline and those under it.
True when the file was walked, NIL for one left out, and the file's tags and
category as two more values."
  (let ((*org-text* text)
        (file-tags (org-file-tags text))
        (file-category (org-file-category path text)))
    (labels ((walk (headline inherited category)
               (unless (and agenda (or (org-property headline :commentedp) (org-property headline :archivedp)))
                 (let* ((own (org-property headline :tags))
                        (tags (org-merge-tags inherited own))
                        (properties (org-headline-properties headline))
                        (category (or (cdr (assoc "CATEGORY" properties :test #'string-equal)) category))
                        (begin (org-node-begin headline))
                        (line-end (string-line-end text begin))
                        (heading (string-left-trim "* " (subseq text begin line-end)))
                        (end (org-entry-end headline text))
                        (planning (let ((line (and (< line-end end) (org-line-at text (1+ line-end)))))
                                    (and line (org-planning-line-p line)
                                         (org-planning-tokens line)))))
                   (funcall function
                            (make-org-agenda-entry
                             :file path :text text :position begin :end end :level (org-property headline :level)
                             :todo (org-property headline :todo-keyword) :todo-type (org-property headline :todo-type)
                             :priority (org-heading-priority heading) :title (or (org-property headline :raw-value) "")
                             :tags tags :local-tags own :category category :heading heading
                             :scheduled (cdr (assoc "SCHEDULED" planning :test #'string=))
                             :deadline (cdr (assoc "DEADLINE" planning :test #'string=))
                             :closed (cdr (assoc "CLOSED" planning :test #'string=))
                             :properties properties)
                            headline)
                   (dolist (child (org-node-children headline))
                     (when (eq (org-node-type child) 'headline) (walk child tags category)))))))
      (cond ((and agenda (member "ARCHIVE" file-tags :test #'string=))
             (return-from org-walk-entries nil))
            (root
             ;; the subtree's tags and category as the headlines above it give them
             (let ((above (loop for h = (org-node-parent root) then (org-node-parent h)
                                while (and h (eq (org-node-type h) 'headline)) collect h)))
               (walk root
                     (org-merge-tags file-tags (loop for h in (reverse above) append (org-property h :tags)))
                     (or (loop for h in above
                               for own = (org-headline-property h "CATEGORY")
                               when own return own)
                         file-category))))
            (t (dolist (child (org-node-children tree))
                 (when (eq (org-node-type child) 'headline) (walk child file-tags file-category))))))
    (values t file-tags file-category)))

(defun org-agenda-file-entries (path text)
  "The headlines of the agenda file at PATH, its TEXT, as ORG-AGENDA-ENTRY
objects in order, with the list items with a checkbox kept as tasks, those
the agenda leaves out left out (ORG-WALK-ENTRIES)."
  (let* ((tree (org-parse text))
         (*org-text* text)
         (entries '()))
    (multiple-value-bind (walked file-tags file-category)
        (org-walk-entries (lambda (entry headline)
                            (let ((items (org-agenda-checkbox-items headline)))
                              (setf (agenda-entry-skip entry) (mapcar #'cdr items))
                              (push (agenda-entry-read-text entry) entries)
                              (dolist (item items)
                                (push (org-agenda-checkbox-entry path text (car item) (cdr item) (1+ (agenda-entry-level entry))
                                                                 (agenda-entry-tags entry) (agenda-entry-category entry))
                                      entries))))
                          path text tree :agenda t)
      (setf entries (nreverse entries))
      ;; the items with checkboxes above the first headline, before them
      (when walked
        (setf entries (append (loop for item in (org-agenda-checkbox-items tree)
                                    collect (org-agenda-checkbox-entry path text (car item) (cdr item) 1 file-tags file-category))
                              entries)))
      entries)))

(defun org-agenda-checkbox-items (node)
  "The list items with a checkbox in NODE's own text (a headline's section, or
the text above the first headline), at any depth of their lists, each (ITEM
. (START . END)), START and END the stretch of the text that is the item's own
\(before its sub-items)."
  (let ((found '()))
    (labels ((walk (n)
               (dolist (child (org-node-children n))
                 (case (org-node-type child)
                   (headline nil)
                   (item (when (org-property child :checkbox)
                           (push (cons child (cons (org-node-begin child) (org-item-own-end child))) found))
                         (walk child))
                   (t (walk child))))))
      (let ((section (find 'section (org-node-children node) :key #'org-node-type)))
        (when section (walk section))))
    (nreverse found)))

(defun org-agenda-checkbox-entry (path text item own level tags category)
  "The agenda entry of the checkbox ITEM of the file at PATH (TEXT), its own
text OWN, (START . END), at LEVEL under the headline whose TAGS and CATEGORY
it takes: TODO while its box is open or half ticked, DONE once ticked, done
when its last note of a state says DONE."
  (let* ((title (org-checkbox-title item text))
         (done (eq (org-property item :checkbox) 'on))
         (keyword (if done "DONE" "TODO"))
         (entry (agenda-entry-read-text
                 (make-org-agenda-entry :file path :text text :position (car own) :end (cdr own) :level level
                                        :todo keyword :todo-type (if done 'done 'todo)
                                        :priority (org-heading-priority title) :title title
                                        :tags tags :local-tags '() :category category
                                        :heading (format nil "~a ~a" keyword title)
                                        :properties '() :kind :checkbox))))
    ;; when it was ticked: its latest note of DONE
    (when done
      (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings *org-done-note-scanner*
                                                            text :start (car own) :end (cdr own)))))
        (when groups
          (push (list :closed (aref groups 0) nil nil) (agenda-entry-logs entry)))))
    entry))

(defvar *org-agenda-file-cache* (make-hash-table :test 'equal :synchronized t)
  "The entries of each agenda file read, by its path: (STAMP-OR-VERSION . ENTRIES).")

(defvar *org-agenda-snapshot* nil
  "While an agenda is worked out away from the editor's thread, where each
agenda file is read from, as ORG-AGENDA-SNAPSHOT took it: a list of (PATH
SOURCE KEY).")

(defun org-agenda-snapshot (&optional (paths (org-agenda-file-paths)))
  "Where each of the agenda files PATHS is read from now, for working the
agenda out on another thread: a list of (PATH SOURCE KEY), SOURCE :DISK, the
text of a buffer as it is now (its rope, which never changes; a copy for a
file mapped from disk, whose mapping may go with its buffer) or NIL, KEY what
tells whether it changed."
  (loop for path in paths
        collect (multiple-value-bind (source key) (org-agenda-file-source path)
                  (list path
                        (cond ((or (null source) (eq source :disk)) source)
                              ((tuatara.buffer:buffer-mapping source) (tuatara.buffer:buffer-string source))
                              (t (tuatara.buffer:buffer-text source)))
                        key))))

(defun org-agenda-open-snapshot ()
  "On the editor's thread: the agenda files open now, as their buffers have
them (ORG-AGENDA-SNAPSHOT), for ORG-AGENDA-SNAPSHOT-WITH to finish on another
thread: the buffers visiting an Org file, or a file the agenda reads
\(ORG-AGENDA-FILES-IN-FORCE).
Nothing on disk is looked at."
  (let ((named (loop for file in (org-agenda-files-in-force)
                     for path = (ignore-errors (tuatara.io:native (tuatara.io:expand-path file)))
                     when path collect path)))
    (org-agenda-snapshot
     (loop for buffer in tuatara.buffer:*buffers*
           for file = (tuatara.buffer:buffer-file buffer)
           for path = (and file (tuatara.io:native file))
           when (and path (or (uiop:string-suffix-p path ".org") (member path named :test #'equal)))
             collect path))))

(defun org-agenda-in-background (name settings work)
  "Call WORK, a function, on a thread of its own named NAME with a snapshot of
the agenda files (ORG-AGENDA-SNAPSHOT-WITH), those open taken here on the
editor's thread, the settings named by SETTINGS, a list, carried to it as
they are here."
  (let ((open (org-agenda-open-snapshot)))
    (tuatara.runtime:make-guarded-thread
     (tuatara.runtime:carry-settings settings (lambda () (funcall work (org-agenda-snapshot-with open))))
     :name name)))

(defun org-agenda-snapshot-with (open &optional (paths (org-agenda-file-paths)))
  "Away from the editor's thread: a snapshot of the agenda files PATHS as
ORG-AGENDA-SNAPSHOT takes one, those OPEN has (ORG-AGENDA-OPEN-SNAPSHOT) as
it has them, the others as they are on disk now. The folders of the agenda
files are listed, and the files looked at, here."
  (loop for path in paths
        collect (or (assoc path open :test #'equal)
                    (multiple-value-bind (source key) (org-agenda-disk-source path)
                      (list path source key)))))

(defparameter *org-file-read-limit* (* 64 1024 1024)
  "The most octets of a file read from disk for the agenda or a clock table:
a larger one is left out, and said to be.")

(defun org-read-file-text (path)
  "The text of the file at PATH read from disk as the agenda reads it: through
a descriptor that waits on no pipe or device, a regular file of at most
*ORG-FILE-READ-LIMIT* octets, an octet that is not UTF-8 read as a
replacement character; NIL when it cannot be read so, said when too large."
  (handler-case
      (tuatara.io:file-text path :limit *org-file-read-limit* :replacement (code-char #xFFFD)
                                 :too-large (lambda (name size)
                                              (declare (ignore name size))
                                              (message "~a is not read: larger than ~a" (abbreviated-path path)
                                                       (octets-words *org-file-read-limit*))
                                              nil))
    (error () nil)))

(defun org-agenda-source-text (path source)
  "The text of the agenda file at PATH read from SOURCE (ORG-AGENDA-FILE-SOURCE
or a snapshot's), its lines ending in a newline alone; an octet that is not
UTF-8 read as a replacement character; NIL when it cannot be read."
  (let ((text (cond ((null source) nil)
                    ((eq source :disk) (org-read-file-text path))
                    ((stringp source) source)
                    ((typep source 'tuatara.buffer:buffer) (tuatara.buffer:buffer-string source))
                    (t (ignore-errors (tuatara.text:rope-string source))))))
    (and text (lf-newlines text))))

(defun org-agenda-file-source (path)
  "Where the agenda file at PATH is read from, as two values: a buffer visiting
it or :DISK, and what tells whether it changed (the buffer's version, the
file's time and size); NIL when it is neither open nor on disk. While the
agenda is worked out elsewhere, as *ORG-AGENDA-SNAPSHOT* has it."
  (let ((taken (and *org-agenda-snapshot* (assoc path *org-agenda-snapshot* :test #'equal))))
    (when taken
      (return-from org-agenda-file-source (values (second taken) (third taken)))))
  (let ((buffer (buffer-visiting path)))
    (if buffer
        (values buffer (list :buffer buffer (tuatara.buffer:buffer-version buffer)))
        (org-agenda-disk-source path))))

(defun org-agenda-disk-source (path)
  "Where the agenda file at PATH is read from on disk, as two values: :DISK
and what tells whether it changed (the file's time and size); NIL when it is
no regular file there (a pipe or a device is never waited on)."
  (let ((stamp (ignore-errors (tuatara.io:path-stamp path))))
    (and stamp (eq (tuatara.io:stamp-kind stamp) :regular)
         (values :disk (list :file (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-size stamp))))))

(defun org-agenda-entries (path)
  "The entries of the agenda file at PATH (native), read again only when it
changed; NIL when it cannot be read. An error reading its text names the
file. In a job in the background asked to stop, it ends the job instead
\(TUATARA.RUNTIME:JOB-CHECK-STOP)."
  (tuatara.runtime:job-check-stop)
  (multiple-value-bind (source key) (org-agenda-file-source path)
    (when source
      (let ((kept (gethash path *org-agenda-file-cache*)))
        (if (and kept (equal (car kept) key))
            (cdr kept)
            (let ((text (org-agenda-source-text path source)))
              (when text
                (let ((entries (handler-bind ((error (lambda (condition)
                                                       (error "~a could not be read: ~a" (file-namestring path)
                                                              (tuatara.runtime:condition-words condition)))))
                                 (org-agenda-file-entries path text))))
                  (setf (gethash path *org-agenda-file-cache*) (cons key entries))
                  entries))))))))

(defun org-clocks-left-open (&optional (now (org-now)))
  "The clocks left running in the agenda files, their CLOCK: lines open, as
the agenda reads them, as ORG-CLOCKs not yet held to their buffers
\(ORG-CLOCK-LIVE-BUFFER does), the one started last first; each one's task
the headline or checkbox item it is under. A line started after NOW, the
universal time, is passed over."
  (let ((found '()))
    (dolist (path (org-agenda-file-paths))
      (dolist (entry (org-agenda-entries path))
        (loop for (kind from nil nil open) in (agenda-entry-logs entry)
              for started = (and (eq kind :clock) open (org-ts-universal-time from))
              when (and started (<= started now))
                do (push (make-org-clock :path path :started started :task (agenda-entry-title entry)) found))))
    (stable-sort found #'> :key #'org-clock-started)))

(defvar *org-agenda-paths-kept* nil
  "The agenda files last worked out (ORG-AGENDA-FILE-PATHS): (FILES STAMPS
PATHS), FILES the setting they came from and STAMPS the times of the folders
listed for it, so that a folder is listed again only when it changed.")

(defun org-agenda-file-paths (&optional (files (org-agenda-files-in-force)))
  "FILES as native paths, the home directory written ~ taken as it is: a
folder as the .org files in it, a pattern (* ?) as the files it matches, each
in order of name, every file once. The folders are listed again only when
one of them changed (*ORG-AGENDA-PATHS-KEPT*)."
  (let* ((stamps (loop for file in files
                       for path = (ignore-errors (tuatara.io:native (tuatara.io:expand-path file)))
                       for (parent name) = (and path (multiple-value-list (tuatara.io:path-folder-and-name path)))
                       for folder = (and path (cond ((find-if (lambda (c) (find c "*?")) name) parent)
                                                    ((eq :directory (tuatara.io:path-kind path)) path)
                                                    (t nil)))
                       collect (and folder (let ((stamp (ignore-errors (tuatara.io:path-stamp folder))))
                                             (and stamp (tuatara.io:stamp-mtime stamp))))))
         (kept *org-agenda-paths-kept*))
    (if (and kept (equal (first kept) files) (equal (second kept) stamps))
        (third kept)
        (let ((paths (org-agenda-file-paths-listed files)))
          (setf *org-agenda-paths-kept* (list (copy-list files) stamps paths))
          paths))))

(defun org-agenda-file-paths-listed (files)
  "FILES as ORG-AGENDA-FILE-PATHS gives them, the folders listed now."
  (let ((paths '()) (seen (make-hash-table :test 'equal)))
    (dolist (file files)
      (let ((path (tuatara.io:native (tuatara.io:expand-path file))))
        (multiple-value-bind (folder name) (tuatara.io:path-folder-and-name path)
          (flet ((in-folder (directory test)
                   (loop for (entry kind) in (tuatara.io:directory-entries directory)
                         when (and (eq kind :regular) (funcall test entry))
                           collect (tuatara.io:path-join directory entry))))
            (cond ((find-if (lambda (c) (find c "*?")) name)
                   ;; as the shell has it: a name beginning with a dot only for a pattern that does
                   (dolist (match (in-folder folder (lambda (entry)
                                                      (and (or (char= (char name 0) #\.)
                                                               (zerop (length entry)) (char/= (char entry 0) #\.))
                                                           (glob-match name entry)))))
                     (unless (gethash match seen) (setf (gethash match seen) t) (push match paths))))
                  ((eq :directory (tuatara.io:path-kind path))
                   (dolist (match (in-folder path (lambda (entry) (and (plusp (length entry)) (char/= (char entry 0) #\.)
                                                                       (uiop:string-suffix-p entry ".org")))))
                     (unless (gethash match seen) (setf (gethash match seen) t) (push match paths))))
                  (t (unless (gethash path seen) (setf (gethash path seen) t) (push path paths))))))))
    (nreverse paths)))

;;; Items

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *org-agenda-item-types*
    '("scheduled" "past-scheduled" "deadline" "upcoming-deadline" "timestamp" "block"
      "closed" "clock" "repeat-done" "cancelled" "moved" "check" "todo" "tagsmatch" "grid" "now")
    "The kinds of line the agenda lists: a task scheduled for its day, one
scheduled before and not done, a deadline on its day or past, one to come, a
timestamp, a day of a range of days, what was closed and clocked (in log
mode), a repeating task done that day, a time of one cancelled or moved, an entry whose dates look wrong, a task of
the TODO list, an entry a match selects, and the time grid's
lines and the line of now."))

(defmacro org-item-type-in (item &rest types)
  "True when the agenda line ITEM is of one of TYPES, each one of
*ORG-AGENDA-ITEM-TYPES*; a type not among them is refused as this is
compiled."
  (dolist (type types)
    (unless (member type *org-agenda-item-types* :test #'equal)
      (error "~s is no kind of agenda line: ~{~a~^, ~}" type *org-agenda-item-types*)))
  `(member (org-item-type ,item) ',types :test #'string=))

(defstruct (org-agenda-item (:conc-name org-item-))
  "A line of the agenda: the DAY it is listed on (a day number, NIL in a list
of tasks), its TYPE (one of *ORG-AGENDA-ITEM-TYPES*), its EXTRA (Scheduled:,
Sched. 2x:, In   3 d.:, (1/3):), its TIME of day (930 for 09:30, or NIL) and
the END of its time range (1500 for 14:00-15:00, or NIL), its ENTRY and TITLE,
and its URGENCY, by which it is ordered (ORG-AGENDA-SORT)."
  day type extra time end entry title urgency)

(defun org-time-of-day (text)
  "The time of day TEXT holds first (09:30, 9:30-10:00, 3pm, as
FIND-TIME-RANGE finds it) as a number of hours and minutes: 930, or NIL; the
part it was found in, and the end of its range (1000) or NIL, as the second
and third values."
  (multiple-value-bind (start end hour minute end-hour end-minute) (find-time-range text)
    (when start
      (values (+ (* 100 hour) minute) (subseq text start end) (and end-hour (+ (* 100 end-hour) end-minute))))))

(defvar *org-agenda-time-memo* nil
  "While an agenda is made, a table of the times of day read so far, by the text they were read from.")

(defun org-agenda-time-of-day (text)
  "ORG-TIME-OF-DAY of TEXT, read once while an agenda is made."
  (memo-values *org-agenda-time-memo* text (lambda () (org-time-of-day text))))

(defun org-agenda-stamp-time (text)
  "The time of day of the timestamp TEXT, plain or by rule, as it was read
once while an agenda is made (ORG-AGENDA-TS, ORG-AGENDA-RULE): 930 for 09:30,
or NIL; and the end of its range, 1000, or NIL. For TEXT that is no
timestamp, the first time of day in it (ORG-TIME-OF-DAY)."
  (flet ((of-day (hour minute) (org-time-number hour minute)))
    (let ((ts (org-agenda-ts text)))
      (if ts
          (values (of-day (org-ts-hour ts) (org-ts-minute ts)) (of-day (org-ts-end-hour ts) (org-ts-end-minute ts)))
          (let ((rule (org-agenda-rule text)))
            (if rule
                (values (of-day (org-rule-hour rule) (org-rule-minute rule)) (of-day (org-rule-end-hour rule) (org-rule-end-minute rule)))
                (multiple-value-bind (of-day part end) (org-agenda-time-of-day text)
                  (declare (ignore part))
                  (values of-day end))))))))

(defun org-title-without-dates (title)
  "TITLE without the active dates written in it (*ORG-AGENDA-STAMP-SCANNER*:
<2026-10-27 Tue 10:00>, a range, one by rule), the spaces they leave made
one; TITLE as it is when it holds none, or nothing else."
  (if (not (search "<" title))
      title
      (let ((left (string-trim " " (cl-ppcre:regex-replace-all "[ \\t]{2,}" (cl-ppcre:regex-replace-all *org-agenda-stamp-scanner* title "") " "))))
        (if (plusp (length left)) left title))))

(defun org-agenda-item (entry day type extra time urgency &key (search-heading t) (title (agenda-entry-title entry)) keep-title end)
  "An agenda line for ENTRY: TIME the timestamp text its time of day is read
from (or :HEADING, its title's), NIL for none; with SEARCH-HEADING a title
holding a time gives it when the timestamp has none, the time taken out of
the title (unless KEEP-TITLE). On a day's line (DAY given) the dates written
in the title are taken out of it unless *ORG-AGENDA-DATE-IN-TITLE*
\(ORG-TITLE-WITHOUT-DATES). END, when given, is the end of its time range;
else the range's end where the time is one."
  (multiple-value-bind (of-day range-end found)
      (cond ((null time) nil)
            ((and (stringp time) (org-agenda-stamp-time time)) (org-agenda-stamp-time time))
            (search-heading (multiple-value-bind (value part) (org-agenda-time-of-day title) (values value nil part))))
    (let* ((title (if (and found (not keep-title))
                      (let ((at (search found title)))
                        (if at
                            (string-trim " " (concatenate 'string (string-right-trim " " (subseq title 0 at))
                                                          (subseq title (+ at (length found)))))
                            title))
                      title))
           (title (if (and day (not *org-agenda-date-in-title*)) (org-title-without-dates title) title)))
      (make-org-agenda-item :day day :type type :extra extra :time of-day :entry entry :title title :urgency urgency
                            :end (or end range-end)))))

(defun agenda-entry-read-text (entry)
  "Fill ENTRY's STAMPS, RANGES and LOGS from its own text, not its planning
line, a comment line's left out; ENTRY. A clock is read from its CLOCK: on
\(ORG-READ-CLOCK-LINE), how long it ran and when it stopped kept when its line
says both."
  (let* ((text (agenda-entry-text entry))
         (end (agenda-entry-end entry))
         (stamps '()) (ranges '()) (logs '()))
    (loop for line-start = (agenda-entry-position entry) then (1+ line-end)
          ;; past the last line, which may have no newline after it: no line left
          for line-end = (or (and (< line-start end) (position #\Newline text :start line-start :end end)) end)
          while (< line-start end)
          ;; the lines of its checkbox items are theirs
          unless (or (find-if (lambda (range) (and (<= (car range) line-start) (< line-start (cdr range)))) (agenda-entry-skip entry))
                     (cl-ppcre:scan "^[ \\t]*#(?: |$)" text :start line-start :end line-end))
          do (multiple-value-bind (match match-end starts ends)
                 (cl-ppcre:scan *org-closed-or-clock-scanner* text :start line-start :end line-end)
               (declare (ignore match-end))
               (cond ((null match)
                      ;; a note of DONE, as a repeating task leaves each time it is done (a
                      ;; checkbox's is its closing, read with the item)
                      (unless (eq (agenda-entry-kind entry) :checkbox)
                        (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings *org-done-note-scanner* text :start line-start :end line-end))))
                          (when groups
                            ;; the checkboxes ticked then, said on the line after
                            (let* ((next (min end (1+ line-end)))
                                   (ticked (nth-value 1 (cl-ppcre:scan-to-strings *org-boxes-ticked-scanner* text
                                                                                   :start next :end (or (position #\Newline text :start next :end end) end)))))
                              (push (list :done-note (aref groups 0) nil nil
                                          (and ticked (cons (parse-integer (aref ticked 0)) (parse-integer (aref ticked 1)))))
                                    logs))))))
                     ((aref starts 0) (push (list :closed (subseq text (aref starts 1) (aref ends 1)) nil nil) logs))
                     (t (multiple-value-bind (from to duration) (org-read-clock-line text match line-end)
                          (when from
                            (let ((stopped (and to duration (cl-ppcre:scan-to-strings "\\d{1,2}:\\d{2}(?=\\]$)" to))))
                              (push (list :clock from (and stopped duration) stopped (null to)) logs)))))))
             (unless (org-planning-line-p text :start line-start :end line-end)
               (cl-ppcre:do-scans (s e range-starts range-ends *org-agenda-stamp-scanner* text nil :start line-start :end line-end)
                 (if (aref range-starts 0)
                     (let ((dash (search ">--<" text :start2 s :end2 e)))
                       (push (list (subseq text s (1+ dash)) (subseq text (+ dash 3) e)) ranges))
                     (let ((stamp (subseq text s e)))
                       (when (or (org-agenda-ts stamp) (org-agenda-rule stamp)) (push stamp stamps)))))))
    (setf (agenda-entry-stamps entry) (nreverse stamps)
          (agenda-entry-ranges entry) (remove-if-not (lambda (r) (and (org-agenda-ts (first r)) (org-agenda-ts (second r)))) (nreverse ranges))
          (agenda-entry-logs entry) (nreverse logs))
    (setf (agenda-entry-facts entry) (org-entry-facts entry))
    entry))

(defun org-entry-facts (entry)
  "What the agenda needs of ENTRY's dates, worked out once, as a plist:
:DEADLINE (DAY WARNING REPEATS) and :SCHEDULED (DAY DELAY REPEATS) for its
active planning timestamps, WARNING its own -3d or NIL; :STAMPS, each (TEXT
DAY REPEATS), REPEATS its repeater (ORG-REPEATER-OF), or NIL; :RANGES, each
\(FROM TO FIRST-DAY LAST-DAY); and for its dates by rule (ORG-READ-RULE)
:RULES, each (KIND TEXT RULE), KIND :SCHEDULED, :DEADLINE or :STAMP, with
:LAST-DONE, the day of its LAST_REPEAT, the last day it was done for, or NIL;
:CANCELLED and :MOVED, the times cancelled and moved one at a time
\(ORG-OCCURRENCE-NOTES)."
  (flet ((planned (text)
           (let ((ts (and text (char= (char text 0) #\<) (org-agenda-ts text))))
             (and ts (list (org-ts-day-number ts) (org-warning-days ts) (org-repeater-of text))))))
    (list* :deadline (planned (agenda-entry-deadline entry))
          :scheduled (planned (agenda-entry-scheduled entry))
          :stamps (loop for text in (agenda-entry-stamps entry)
                        unless (org-agenda-rule text)
                        collect (list text (org-ts-day-number (org-agenda-ts text))
                                      (and (cl-ppcre:scan "^<\\d+-\\d+-\\d+[^>\\n]+?\\+\\d+[hdwmy]>$" text)
                                           (org-repeater-of text))))
          :ranges (loop for (from to) in (agenda-entry-ranges entry)
                        collect (list from to (org-ts-day-number (org-agenda-ts from)) (org-ts-day-number (org-agenda-ts to))))
          :rules (loop for (kind text) in (list* (list :scheduled (agenda-entry-scheduled entry))
                                                 (list :deadline (agenda-entry-deadline entry))
                                                 (loop for text in (agenda-entry-stamps entry) collect (list :stamp text)))
                       for rule = (and text (org-agenda-rule text))
                       when rule collect (list kind text rule))
          :last-done (let ((ts (org-agenda-ts (or (cdr (assoc "LAST_REPEAT" (agenda-entry-properties entry) :test #'string-equal)) ""))))
                       (and ts (org-ts-day-number ts)))
          (multiple-value-bind (cancelled moved)
              (if (eq (agenda-entry-kind entry) :checkbox)
                  (values nil nil)
                  (org-occurrence-notes (agenda-entry-text entry) (agenda-entry-position entry) (agenda-entry-end entry)))
            (list :cancelled cancelled :moved moved)))))

(defun org-done-p (entry)
  "True when ENTRY's TODO keyword is a done one."
  (eq (agenda-entry-todo-type entry) 'done))

(defun org-agenda-deadlines (entries current today)
  "The deadline lines of ENTRIES on the day CURRENT, today being TODAY."
  (loop for entry in entries
        for facts = (getf (agenda-entry-facts entry) :deadline)
        when facts
          append (destructuring-bind (deadline own-warning repeats) facts
                   (let* ((text (agenda-entry-deadline entry))
                          (repeat (if (or (<= current today) (not repeats)) deadline (org-repeat-day repeats current :future)))
                          (diff (- deadline current))
                          (warning (or own-warning *org-deadline-warning-days*))
                          (today-p (= current today)))
                     (unless (or (and (/= current deadline) (/= current repeat)
                                      (or (not today-p) (and (> deadline current) (> diff warning))))
                                 (and (org-done-p entry) (/= deadline current))
                                 ;; its time moved: listed where it went (ORG-AGENDA-NOTED-LINES)
                                 (and today-p (/= deadline current) (getf (agenda-entry-facts entry) :moved)
                                      (org-moved-from-p (getf (agenda-entry-facts entry) :moved) deadline (org-agenda-stamp-time text))))
                       (list (org-agenda-item entry current
                                              (if (and today-p (> deadline today)) "upcoming-deadline" "deadline")
                                              (cond ((and today-p (< deadline today)) (format nil "~2d d. ago: " (- diff)))
                                                    ((and today-p (> deadline today)) (format nil "In ~3d d.: " diff))
                                                    (t "Deadline:  "))
                                              (if (and (/= current deadline) (/= current repeat)) nil text)
                                              (+ (if today-p (- diff) 0) (agenda-entry-priority entry)))))))))

(defun org-agenda-scheduled (entries current today)
  "The scheduled lines of ENTRIES on the day CURRENT, today being TODAY."
  (loop for entry in entries
        for facts = (getf (agenda-entry-facts entry) :scheduled)
        when facts
          append (destructuring-bind (schedule delay repeats) facts
                   (let* ((text (agenda-entry-scheduled entry))
                          (repeat (if (or (<= current today) (not repeats)) schedule (org-repeat-day repeats current :future)))
                          (diff (- current schedule))
                          (delay (or delay 0))
                          (past (< schedule today)))
                     (unless (or (and (> delay 0) (< diff delay))
                                 (> schedule current)
                                 (and (/= current schedule) (/= current today) (/= current repeat))
                                 (and (org-done-p entry) (/= schedule current))
                                 ;; its time moved: listed where it went (ORG-AGENDA-NOTED-LINES)
                                 (and (= current today) past (getf (agenda-entry-facts entry) :moved)
                                      (org-moved-from-p (getf (agenda-entry-facts entry) :moved) schedule (org-agenda-stamp-time text))))
                       (list (org-agenda-item entry current (if past "past-scheduled" "scheduled")
                                              (if (and (= current today) past) (format nil "Sched.~2dx: " diff) "Scheduled: ")
                                              (if (and (/= current schedule) (/= current repeat)) nil text)
                                              (+ 99 diff (agenda-entry-priority entry)))))))))

(defun org-agenda-blocks (entries current)
  "The lines of the ranges of days of ENTRIES on the day CURRENT."
  (loop for entry in entries
        append (loop for (from to d1 d2) in (getf (agenda-entry-facts entry) :ranges)
                     when (<= d1 current d2)
                       collect (org-agenda-item entry current "block"
                                                (if (= d1 d2) "" (format nil "(~d/~d): " (1+ (- current d1)) (1+ (- d2 d1))))
                                                (cond ((= d1 current) from) ((= d2 current) to) (t nil))
                                                (agenda-entry-priority entry)))))

(defun org-agenda-timestamps (entries current today)
  "The lines of the plain timestamps of ENTRIES on the day CURRENT, today being
TODAY."
  (loop for entry in entries
        append (loop for (text day repeats) in (getf (agenda-entry-facts entry) :stamps)
                     when (if repeats
                              (let* ((past (org-repeat-day repeats (if (> current today) today current) :past))
                                     (future (if (<= current today) past (org-repeat-day repeats current :future))))
                                (or (= current past) (= current future)))
                              (= current day))
                       collect (org-agenda-item entry current "timestamp" "" text (agenda-entry-priority entry)))))

(defun org-agenda-rule-open (rule from)
  "The first day at or after the day FROM that RULE takes (ORG-RULE-NEXT),
worked out once for each rule and day while an agenda is made."
  (let ((table *org-agenda-ts-memo*))
    (if (null table)
        (org-rule-next rule from)
        (let ((known (assoc from (gethash rule table))))
          (if known
              (cdr known)
              (let ((open (org-rule-next rule from)))
                (push (cons from open) (gethash rule table))
                open))))))

(defun org-agenda-rule-open-past (rule from cancelled)
  "The first day at or after the day FROM that RULE takes (ORG-AGENDA-RULE-OPEN)
and CANCELLED (ORG-OCCURRENCE-NOTES) does not cancel, or NIL."
  (if cancelled
      (org-rule-next-open rule from cancelled (lambda (day) (org-agenda-rule-open rule day)))
      (org-agenda-rule-open rule from)))

(defun org-agenda-rules (entries current today)
  "The lines of the dates by rule of ENTRIES on the day CURRENT, today being
TODAY. A timestamp in the text is listed on each day its rule takes. A
scheduled or deadline task by rule is open from the first of its days after
the last it was done for (its LAST_REPEAT, ORG-RULE-DONE-DAY): today that one
is listed when it has come or is past, as a repeating task's is, a deadline's
too while it is near; on the days after today, each day the rule takes from
then; on the days before, those the rule takes after the last done for. Never
done, a task's days before today are not past due."
  (loop for entry in entries
        for facts = (agenda-entry-facts entry)
        for rules = (getf facts :rules)
        for last-done = (and rules (getf facts :last-done))
        for cancelled = (and rules (getf facts :cancelled))
        append (loop for (kind text rule) in rules
                     for priority = (agenda-entry-priority entry)
                     append
                     (if (eq kind :stamp)
                         (and (org-rule-matches-p rule current)
                              (list (org-agenda-item entry current "timestamp" "" text priority)))
                         (let* ((open (org-agenda-rule-open-past rule (if last-done (1+ last-done) today) cancelled))
                                (deadline (eq kind :deadline))
                                (warning *org-deadline-warning-days*))
                           (cond ((or (org-done-p entry) (null open)) '())
                                 ((and (= current today) (org-moved-from-p (getf facts :moved) open (org-rule-time-of-day rule))) '())
                                 ((= current today)
                                  (let ((diff (- today open)))
                                    (cond ((and (not deadline) (>= diff 0))
                                           (list (org-agenda-item entry current (if (plusp diff) "past-scheduled" "scheduled")
                                                                  (if (plusp diff) (format nil "Sched.~2dx: " diff) "Scheduled: ")
                                                                  text (+ 99 diff priority))))
                                          ((and deadline (or (>= diff 0) (<= (- diff) warning)))
                                           (list (org-agenda-item entry current (if (minusp diff) "upcoming-deadline" "deadline")
                                                                  (cond ((plusp diff) (format nil "~2d d. ago: " diff))
                                                                        ((minusp diff) (format nil "In ~3d d.: " (- diff)))
                                                                        (t "Deadline:  "))
                                                                  text (+ diff priority)))))))
                                 ((and (org-rule-matches-p rule current)
                                       (if (> current today) (>= current open) (or (null last-done) (> current last-done))))
                                  (list (org-agenda-item entry current (if deadline "deadline" "scheduled")
                                                         (if deadline "Deadline:  " "Scheduled: ")
                                                         text (+ 99 priority))))))))))

(defun org-agenda-progress (entries current)
  "In log mode, the lines of what ENTRIES closed and clocked on the day CURRENT;
a task cancelled (*ORG-CANCEL-KEYWORDS*) that has a date of its own has no
line of its closing, the line of its date saying it is cancelled already."
  (let ((day (format-day "%F" current)))
    (loop for entry in entries
          for title = (if (agenda-entry-todo entry) (format nil "~a ~a" (agenda-entry-todo entry) (agenda-entry-title entry)) (agenda-entry-title entry))
          for cancelled = (and (member (agenda-entry-todo entry) *org-cancel-keywords* :test #'equal)
                               (or (agenda-entry-stamps entry) (agenda-entry-ranges entry)
                                   (agenda-entry-scheduled entry) (agenda-entry-deadline entry)))
          append (loop for (kind timestamp clocked stopped) in (agenda-entry-logs entry)
                       when (and (member kind '(:closed :clock)) (not (and cancelled (eq kind :closed)))
                                 (uiop:string-prefix-p day (subseq timestamp 1)))
                         collect (org-agenda-item entry current (if (eq kind :closed) "closed" "clock")
                                                  (if (eq kind :closed) "Closed:    " (format nil "Clocked:   (~a)" (or clocked "-")))
                                                  timestamp 100000 :search-heading nil :title title
                                                  :end (and (eq kind :clock) stopped (org-time-of-day stopped)))))))

(defun org-entry-repeats-p (entry)
  "True when ENTRY is a task that repeats: scheduled or with a deadline that
repeats (+1w), or by rule."
  (let ((facts (agenda-entry-facts entry)))
    (or (third (getf facts :scheduled)) (third (getf facts :deadline))
        (find-if (lambda (rule) (member (first rule) '(:scheduled :deadline))) (getf facts :rules)))))

(defun org-agenda-repeats-done (entries current)
  "The lines of the times the repeating tasks of ENTRIES were done on the day
CURRENT, as their notes of DONE say, shown done: a repeating task done goes
on to its next time, and its day keeps what was done on it, its counts
saying the checkboxes ticked then when its note says (its own cleared for the
next time)."
  (let ((day (format-day "%F" current)))
    (loop for entry in entries
          when (org-entry-repeats-p entry)
            append (loop for (kind timestamp nil nil ticked) in (agenda-entry-logs entry)
                         when (and (eq kind :done-note) (uiop:string-prefix-p day (subseq timestamp 1)))
                           collect (org-agenda-item entry current "repeat-done" "Done:      " timestamp 100000
                                                    :search-heading nil
                                                    :title (format nil "DONE ~a"
                                                                   (if ticked
                                                                       (org-cookies-saying (agenda-entry-title entry) (car ticked) (cdr ticked))
                                                                       (agenda-entry-title entry))))))))

(defun org-agenda-sort (items key)
  "ITEMS ordered stably by KEY, a list of :TIME (by time of day, those without one last) and :URGENCY (the most urgent first)."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (loop for k in key
                       for order = (ecase k
                                     (:time (let ((ta (or (org-item-time a) 9901)) (tb (or (org-item-time b) 9901)))
                                              (cond ((< ta tb) -1) ((< tb ta) 1))))
                                     (:urgency (let ((ua (or (org-item-urgency a) 0)) (ub (or (org-item-urgency b) 0)))
                                                 (cond ((> ua ub) -1) ((> ub ua) 1)))))
                       when order return (minusp order)))))

(defvar *org-agenda-noted-memo* nil
  "While an agenda is made, the entries of each file with times cancelled or
moved one at a time (ORG-AGENDA-NOTED-ENTRIES), by its list of entries.")

(defun org-agenda-noted-entries (entries)
  "Those of ENTRIES with times cancelled or moved one at a time, found once
for each file while an agenda is made (*ORG-AGENDA-NOTED-MEMO*)."
  (flet ((noted ()
           (remove-if-not (lambda (entry)
                            (let ((facts (agenda-entry-facts entry)))
                              (or (getf facts :cancelled) (getf facts :moved))))
                          entries)))
    (let ((memo *org-agenda-noted-memo*))
      (if (null memo)
          (noted)
          (multiple-value-bind (known found) (gethash entries memo)
            (if found known (setf (gethash entries memo) (noted))))))))

(defun org-agenda-without-noted-times (items)
  "ITEMS without the lines of the times their entries cancelled or moved one
at a time (ORG-OCCURRENCE-NOTES): a time scheduled, due or of a timestamp on
the day of its line, at the time of day cancelled or moved, or any when all
that day is."
  (flet ((noted-p (item)
           (let ((facts (agenda-entry-facts (org-item-entry item))))
             (and (or (getf facts :cancelled) (getf facts :moved))
                  (org-item-type-in item "scheduled" "past-scheduled" "deadline" "timestamp")
                  (or (org-cancelled-p (getf facts :cancelled) (org-item-day item) (org-item-time item))
                      (org-moved-from-p (getf facts :moved) (org-item-day item) (org-item-time item)))))))
    (if (find-if #'noted-p items) (remove-if #'noted-p items) items)))

(defun org-moved-done-p (entry origin)
  "True when ENTRY, a task, is done for its time ORIGIN, an occurrence moved
elsewhere: by rule, its LAST_REPEAT that day or after; else its date that
repeats gone on past it."
  (let* ((facts (agenda-entry-facts entry))
         (last-done (getf facts :last-done)))
    (cond ((org-done-p entry) t)
          ((getf facts :rules) (and last-done (>= last-done (car origin))))
          (t (let ((planned (or (first (getf facts :scheduled)) (first (getf facts :deadline)))))
               (and planned (> planned (car origin))))))))

(defun org-agenda-noted-lines (entries current today)
  "The lines of the times of ENTRIES cancelled or moved one at a time on the
day CURRENT, today being TODAY. A time cancelled: Cancelled: on its day,
faint. A time moved: Moved: and where it went on the day it was, faint; on
the day it went to, the time itself, as scheduled for a task and as a
timestamp for an event, till a task is done for it; a task's moved to a day
gone and not done, today, as one scheduled before."
  (loop for entry in entries
        for facts = (agenda-entry-facts entry)
        for cancelled = (getf facts :cancelled)
        for task = (eq (agenda-entry-todo-type entry) 'todo)
        for priority = (agenda-entry-priority entry)
        nconc (loop for occurrence in cancelled
                    when (= (car occurrence) current)
                      collect (org-agenda-item entry current "cancelled" "Cancelled: " (org-occurrence-stamp occurrence)
                                               100000 :search-heading nil
                                               :title (format nil "CANCELLED ~a" (agenda-entry-title entry))))
        nconc (loop for (origin . target) in (getf facts :moved)
                    for listed = (and (not (eq (agenda-entry-todo-type entry) 'done))
                                      (not (and task (org-moved-done-p entry origin)))
                                      (not (org-cancelled-p cancelled (car target) (cdr target))))
                    when (= (car origin) current)
                      collect (org-agenda-item entry current "moved" "Moved:     " (org-occurrence-stamp origin) priority
                                               :search-heading nil
                                               :title (format nil "~@[~a ~]~a → ~a" (agenda-entry-todo entry) (agenda-entry-title entry)
                                                              (org-occurrence-words target)))
                    when (and listed (= (car target) current))
                      collect (if task
                                  (org-agenda-item entry current "scheduled" "Scheduled: " (org-occurrence-stamp target) (+ 99 priority))
                                  (org-agenda-item entry current "timestamp" "" (org-occurrence-stamp target) priority))
                    when (and listed task (= current today) (< (car target) today))
                      collect (org-agenda-item entry current "past-scheduled" (format nil "Sched.~2dx: " (- today (car target)))
                                               (org-occurrence-stamp target) (+ 99 (- today (car target)) priority)))))

(defun org-agenda-without-twice-listed (items)
  "ITEMS without the lines of plain timestamps of an entry listed already as
scheduled or with its deadline: its timestamp of that day says nothing more
when it has no time of day or the same as that line's."
  (flet ((twice-p (item)
           ;; only an entry with a planning line as well can be listed twice
           (and (let ((entry (org-item-entry item)))
                  (or (agenda-entry-scheduled entry) (agenda-entry-deadline entry)))
                (org-item-type-in item "timestamp")
                (find-if (lambda (other)
                           (and (eq (org-item-entry other) (org-item-entry item))
                                (org-item-type-in other "scheduled" "past-scheduled" "deadline" "upcoming-deadline")
                                (or (null (org-item-time item)) (eql (org-item-time item) (org-item-time other)))))
                         items))))
    (if (find-if #'twice-p items) (remove-if #'twice-p items) items)))

(defun org-agenda-day (day &key (files (org-agenda-file-paths)) (today (org-today)) (log *org-agenda-log-mode*) entries-by-file)
  "The agenda lines of the day DAY (a day number) from FILES: its deadlines,
what was done that day when LOG is true, the repeating tasks done that day,
what is scheduled, the ranges and the timestamps, an entry's timestamp
left out where it is listed already (ORG-AGENDA-WITHOUT-TWICE-LISTED), the
times cancelled or moved one at a time shown so instead
\(ORG-AGENDA-NOTED-LINES), ordered by time of day and then urgency
\(ORG-AGENDA-SORT); ENTRIES-BY-FILE, when given, the entries of each file,
read before."
  (org-agenda-sort
   (loop for path in files
         for entries in (or entries-by-file (mapcar #'org-agenda-entries files))
         for noted = (org-agenda-noted-entries entries)
         append (org-agenda-without-twice-listed
                 (let ((items (append (org-agenda-deadlines entries day today)
                                      (and log (org-agenda-progress entries day))
                                      (org-agenda-repeats-done entries day)
                                      (org-agenda-scheduled entries day today)
                                      (org-agenda-blocks entries day)
                                      (org-agenda-timestamps entries day today)
                                      (org-agenda-rules entries day today))))
                   (if noted
                       (nconc (org-agenda-without-noted-times items) (org-agenda-noted-lines noted day today))
                       items))))
   '(:time :urgency)))

(defun org-agenda-span (first count &rest arguments)
  "The agenda lines of COUNT days from the day FIRST, day by day, as ORG-AGENDA-DAY gives each (with its ARGUMENTS)."
  (let* ((*org-agenda-ts-memo* (or *org-agenda-ts-memo* (make-hash-table :test 'equal)))
         (*org-agenda-noted-memo* (or *org-agenda-noted-memo* (make-hash-table :test 'eq)))
         (*org-agenda-time-memo* (or *org-agenda-time-memo* (make-hash-table :test 'equal)))
         (files (getf arguments :files (org-agenda-file-paths)))
         (entries-by-file (mapcar #'org-agenda-entries files)))
    (loop for day from first below (+ first count)
          append (apply #'org-agenda-day day :files files :entries-by-file entries-by-file arguments))))

(defun org-agenda-todo-list (&key (files (org-agenda-file-paths)))
  "The tasks not done in FILES, the most urgent first (ORG-AGENDA-SORT)."
  (org-agenda-sort
   (loop for path in files
         append (loop for entry in (org-agenda-entries path)
                      when (eq (agenda-entry-todo-type entry) 'todo)
                        collect (org-agenda-item entry nil "todo" "" :heading (1+ (agenda-entry-priority entry)) :keep-title t)))
   '(:urgency)))

;;; Matches

(defun agenda-entry-property-value (entry name)
  "The value of ENTRY's property NAME, Org's special ones too (TODO, LEVEL, CATEGORY, PRIORITY, ITEM), or NIL."
  (cond ((string-equal name "TODO") (agenda-entry-todo entry))
        ((string-equal name "LEVEL") (princ-to-string (agenda-entry-level entry)))
        ((string-equal name "CATEGORY") (agenda-entry-category entry))
        ((string-equal name "PRIORITY") (multiple-value-bind (m g) (cl-ppcre:scan-to-strings "\\[#([A-Z0-9])\\]" (agenda-entry-heading entry))
                                          (if m (aref g 0) "B")))
        ((string-equal name "ITEM") (agenda-entry-title entry))
        ((string-equal name "SCHEDULED") (agenda-entry-scheduled entry))
        ((string-equal name "DEADLINE") (agenda-entry-deadline entry))
        (t (cdr (assoc name (agenda-entry-properties entry) :test #'string-equal)))))

(defun org-match-scanner (regex)
  "REGEX, of a match's {regex}, made ready to match as a search reads it, case
ignored; NIL when it is no regular expression."
  (ignore-errors (tuatara.syntax:compile-pattern regex :case-fold t)))

(defun org-match-compare (op left right)
  "LEFT OP RIGHT, as a match compares them: numbers as numbers, else texts, a {regex} by matching."
  (cond ((null left) (string= op "<>"))
        ((and (> (length right) 1) (char= (char right 0) #\{))
         (let ((found (ignore-errors (cl-ppcre:scan (org-match-scanner (subseq right 1 (1- (length right)))) left))))
           (if (string= op "<>") (not found) found)))
        ((and (> (length right) 1) (char= (char right 0) #\"))
         (let ((r (subseq right 1 (1- (length right)))))
           (cond ((string= op "=") (string= left r)) ((string= op "<>") (string/= left r))
                 ((string= op "<") (string< left r)) ((string= op ">") (string> left r))
                 ((string= op "<=") (string<= left r)) ((string= op ">=") (string>= left r)))))
        (t (let ((l (let ((n (ignore-errors (org-calc-read-number left)))) (and n (org-calc-value n))))
                 (r (let ((n (ignore-errors (org-calc-read-number right)))) (and n (org-calc-value n)))))
             (and (realp l) (realp r)
                  (cond ((string= op "=") (= l r)) ((string= op "<>") (/= l r))
                        ((string= op "<") (< l r)) ((string= op ">") (> l r))
                        ((string= op "<=") (<= l r)) ((string= op ">=") (>= l r))))))))

(defun org-match-predicate (match)
  "A function of an entry true when MATCH, an Org match (work+urgent-home,
work|proj, TODO=\"NEXT\", LEVEL>1, -{.*}, CATEGORY=\"x\", tags/NEXT|WAIT), selects it."
  (let* ((slash (let ((depth 0))
                  (loop for c across match for i from 0
                        do (case c (#\{ (incf depth)) (#\} (decf depth)))
                        when (and (char= c #\/) (zerop depth)) return i)))
         (tags-part (if slash (subseq match 0 slash) match))
         (todo-part (and slash (subseq match (1+ slash))))
         (groups (mapcar (lambda (group)
                           (let ((terms '()))
                             (cl-ppcre:do-register-groups (sign regex lop lvalue prop op value tag)
                                 ("([-+&]?)(?:\\{([^}]*)\\}|LEVEL(<>|<=|>=|=|<|>)(\\d+)|([\\w-]+)(<>|<=|>=|=|<|>)(\\{[^}]*\\}|\"[^\"]*\"|-?[.\\d]+)|([\\w@#%]+))" group)
                               (push (list (string= sign "-") regex lop lvalue prop op value tag) terms))
                             (nreverse terms)))
                         (cl-ppcre:split "\\|" tags-part)))
         (todo-only-not-done (and todo-part (uiop:string-prefix-p "!" todo-part)))
         (todo-words (and todo-part (cl-ppcre:split "\\|" (string-left-trim "!" todo-part)))))
    (lambda (entry)
      (and (or (every (lambda (group) (null group)) groups)
               (some (lambda (terms)
                       (and terms
                            (every (lambda (term)
                                     (destructuring-bind (negate regex lop lvalue prop op value tag) term
                                       (let ((hit (cond (regex (let ((scanner (org-match-scanner regex)))
                                                                 (and scanner (some (lambda (t1) (cl-ppcre:scan scanner t1)) (agenda-entry-tags entry)))))
                                                        (lop (org-match-compare lop (princ-to-string (agenda-entry-level entry)) lvalue))
                                                        (prop (org-match-compare op (agenda-entry-property-value entry prop) value))
                                                        (t (member tag (agenda-entry-tags entry) :test #'string=)))))
                                         (if negate (not hit) hit))))
                                   terms)))
                     groups))
           (or (null todo-part)
               (and (agenda-entry-todo entry)
                    (or (not todo-only-not-done) (eq (agenda-entry-todo-type entry) 'todo))
                    (or (null (remove "" todo-words :test #'string=))
                        (let ((positive (remove-if (lambda (w) (uiop:string-prefix-p "-" w)) (remove "" todo-words :test #'string=)))
                              (negative (mapcar (lambda (w) (subseq w 1)) (remove-if-not (lambda (w) (uiop:string-prefix-p "-" w)) todo-words))))
                          (and (or (null positive) (member (agenda-entry-todo entry) positive :test #'string=))
                               (not (member (agenda-entry-todo entry) negative :test #'string=)))))))))))

(defun org-entry-date-problem (entry)
  "What looks wrong with the dates of ENTRY, as two values, a short kind and
the words saying it; NIL when nothing does: a date in its text and a
SCHEDULED: or DEADLINE: too, when an event has the one and a task the other;
a task with times done, moved or cancelled one at a time whose dates no
longer repeat; a task with a date in its text alone, listed as an event, not
carried on as a scheduled one is until it is done."
  (let* ((facts (agenda-entry-facts entry))
         (rules (getf facts :rules))
         (planned (or (agenda-entry-scheduled entry) (agenda-entry-deadline entry)))
         (plain (or (getf facts :stamps) (find :stamp rules :key #'first) (agenda-entry-ranges entry)))
         (repeats (or (third (getf facts :scheduled)) (third (getf facts :deadline)) rules
                      (find-if #'third (getf facts :stamps))))
         (task (eq (agenda-entry-todo-type entry) 'todo)))
    (cond ((or (eq (agenda-entry-kind entry) :checkbox) (eq (agenda-entry-todo-type entry) 'done)) nil)
          ((and planned plain)
           (values "Both:" "a date in its text and a SCHEDULED: or DEADLINE: too: an event has the one, a task the other"))
          ((and task (not repeats) (or (getf facts :cancelled) (getf facts :moved) (getf facts :last-done)))
           (values "No repeat:" "times of it done, moved or cancelled one at a time, but none of its dates repeats any more"))
          ((and task plain (not planned))
           (values "Plain date:" "a task with a date in its text alone, listed as an event: SCHEDULED: would carry it on until done")))))

(defun org-agenda-date-check (&key (files (org-agenda-file-paths)))
  "The entries of FILES whose dates look wrong (ORG-ENTRY-DATE-PROBLEM), as
agenda lines, in the order of the files and of the entries in each: the kind
of what is wrong, and the title with the words saying it."
  (loop for path in files
        append (loop for entry in (org-agenda-entries path)
                     append (multiple-value-bind (kind words) (org-entry-date-problem entry)
                              (and kind
                                   (list (make-org-agenda-item :type "check" :extra (format nil "~11a" kind) :entry entry
                                                               :title (format nil "~@[~a ~]~a — ~a" (agenda-entry-todo entry)
                                                                              (agenda-entry-title entry) words)
                                                               :urgency nil)))))))

(defun org-agenda-match (match &key todo-only (files (org-agenda-file-paths)))
  "The entries of FILES MATCH (an Org match: work+urgent-home) selects, in the
order of the files and of the entries in each; with TODO-ONLY, only tasks
not done."
  (let ((predicate (org-match-predicate match)))
    (loop for path in files
          append (loop for entry in (org-agenda-entries path)
                       when (and (or (not todo-only) (eq (agenda-entry-todo-type entry) 'todo))
                                 (funcall predicate entry))
                         collect (make-org-agenda-item :type "tagsmatch" :extra "" :entry entry
                                                       :title (agenda-entry-title entry) :urgency nil)))))

(setf (documentation 'agenda-entry-file 'function) "The path of the file an agenda entry is in."
      (documentation 'agenda-entry-text 'function) "The whole text of the file an agenda entry is in, as it was read."
      (documentation 'agenda-entry-position 'function) "The character of its file's text an agenda entry's headline begins at."
      (documentation 'agenda-entry-end 'function) "The character of its file's text an agenda entry's own text ends at, its headlines below it left out."
      (documentation 'agenda-entry-level 'function) "How many stars an agenda entry's headline has."
      (documentation 'agenda-entry-todo 'function) "An agenda entry's TODO keyword, or NIL."
      (documentation 'agenda-entry-todo-type 'function) "TODO or DONE for an agenda entry's keyword, NIL for none."
      (documentation 'agenda-entry-title 'function) "An agenda entry's title: its headline without its stars, keyword, priority and tags."
      (documentation 'agenda-entry-heading 'function) "An agenda entry's headline after its stars, as written."
      (documentation 'agenda-entry-tags 'function) "An agenda entry's tags, those it inherits first."
      (documentation 'agenda-entry-local-tags 'function) "An agenda entry's own tags, on its headline."
      (documentation 'agenda-entry-scheduled 'function) "An agenda entry's SCHEDULED timestamp, its text, or NIL."
      (documentation 'agenda-entry-deadline 'function) "An agenda entry's DEADLINE timestamp, its text, or NIL."
      (documentation 'agenda-entry-closed 'function) "An agenda entry's CLOSED timestamp, its text, or NIL."
      (documentation 'agenda-entry-properties 'function) "An agenda entry's properties, an alist of names and values."
      (documentation 'agenda-entry-stamps 'function) "An agenda entry's active timestamps in its text, not its planning line's, as texts."
      (documentation 'agenda-entry-ranges 'function) "An agenda entry's ranges of days in its text, each (FROM TO), the two timestamps' texts.")
