;;;; lisp/editing/state.lisp --- The lists kept between sessions: recent files, places in files, PDF places, paths chosen.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each list is a file of lines in the state directory, shared by the
;;;; sessions side by side. A change made here is kept in memory at once, so
;;;; that the list shown next has it, and written by one thread in the
;;;; background: the file read again under its lock, the changes made since
;;;; put first, and written back whole, so that no session loses another's
;;;; entries and a slow disk never holds the editor up. The list in memory is
;;;; read again from the file once another session has changed it.
;;;;
;;;; A file begins with a line naming the list and the version of its
;;;; format (# tuatara bookmarks 1; a file without one is version 1). A later
;;;; release may add fields at the end of a line, or lines of a new kind: an
;;;; earlier one reads the fields it knows, and writes back as they were the
;;;; lines it leaves unchanged and those it cannot read, so that going from
;;;; one release to another and back loses nothing.

(in-package #:tuatara.defaults)

(defun escape-path (path)
  "PATH as a line of a list kept between sessions writes it: a backslash, a
newline, a carriage return and a tab in it written \\\\, \\n, \\r and \\t,
so that a list may part its fields by tabs."
  (with-output-to-string (out)
    (loop for c across path
          do (case c
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char c out))))))

(defun unescape-path (text)
  "The path a line of a list kept between sessions writes as TEXT (ESCAPE-PATH)."
  (with-output-to-string (out)
    (let ((i 0))
      (loop while (< i (length text))
            do (let ((c (char text i)))
                 (if (and (char= c #\\) (< (1+ i) (length text)))
                     (progn (write-char (case (char text (1+ i)) (#\n #\Newline) (#\r #\Return) (#\t #\Tab) (t (char text (1+ i)))) out)
                            (incf i 2))
                     (progn (write-char c out) (incf i))))))))

(defun state-line-fields (line)
  "The fields of LINE, a line of a list kept between sessions, a tab ending
each: a list of strings. A later release may add fields at the end, which a
list reading the first ones passes over."
  (uiop:split-string line :separator '(#\Tab)))

(defun state-line-path (line &optional (start 0))
  "The path LINE, a line of a list kept between sessions, writes from START to
its end or to a tab (ESCAPE-PATH writes none in a path), after which a later
release may add fields; an empty string when there is none."
  (unescape-path (subseq line start (position #\Tab line :start start))))

(defstruct (state-list (:constructor make-state-list (name parse print limit-variable
                                                      &key header (path-of #'identity) key (version 1) (class :remembered)))
                       (:copier nil))
  "A list kept between sessions in a file of the state directory: NAME, the
file's name there; PARSE, a function making an entry of a line (NIL for a
line to pass over); PRINT, one making the line of an entry; LIMIT-VARIABLE,
the variable saying how many entries are kept; HEADER, lines written first, or
NIL; PATH-OF, a function giving the path an entry is about; KEY, one giving
what makes two entries one (PATH-OF when NIL); VERSION, the version of the
format its lines are written in, said in the file's first line; CLASS, :KEPT
for a list you make yourself, whose loss would lose work, or :REMEMBERED for
one of what you did, kept for convenience. What is known of it: FILE, the
native name of the file read last, its STAMP then and the entries it had
then, DISK; PENDING, the changes made and not yet taken to be written, and
WRITING, those being written, the latest first, each (FILE . CHANGE); SHOWN,
the entries as STATE-LIST-ENTRIES gave them last; NOTED and TRIED, how many
changes were made, and how many there were when the writer last took them;
LOCK, held while these change."
  name parse print limit-variable header path-of key version class
  (file nil) (stamp nil) (disk '()) (pending '()) (writing '()) (shown nil)
  (noted 0) (tried 0)
  (lock (sb-thread:make-mutex :name "state list")))

(setf (documentation 'make-state-list 'function) "Return a STATE-LIST: a list kept between sessions in the state directory."
      (documentation 'state-list-p 'function) "True when OBJECT is a STATE-LIST."
      (documentation 'state-list-name 'function) "The name of a STATE-LIST's file in the state directory.")

(defvar *state-lists* '()
  "Every list kept between sessions (DEFINE-STATE-LIST), for the writer to look through.")

(defmacro define-state-list (variable name &rest arguments)
  "Define VARIABLE as a list kept between sessions in the file NAME of the
state directory, made by MAKE-STATE-LIST with ARGUMENTS (PARSE PRINT LIMIT
and the keys: :HEADER, :PATH-OF, :KEY, :VERSION and :CLASS), and known to the
writer (*STATE-LISTS*). Its file begins with the line # tuatara NAME VERSION;
lines beginning with # are comments, never given to PARSE."
  `(progn
     (defvar ,variable (make-state-list ,name ,@arguments)
       ,(format nil "The list kept between sessions in ~a, in the state directory (STATE-LIST): ~
                     ~:[what you did, kept for convenience~;made by you, kept by every later release~]."
                name (eq (getf (nthcdr 3 arguments) :class) :kept)))
     (setf *state-lists* (cons ,variable (remove ,name *state-lists* :key #'state-list-name :test #'string=)))
     ',variable))

(defun state-list-path (list)
  "The native name of LIST's file: NAME in the state directory."
  (tuatara.io:native (merge-pathnames (state-list-name list) (tuatara.io:state-directory))))

(defun state-file-stamp (file)
  "What FILE is like now, to tell whether it has changed: (INODE SIZE TIME),
or NIL when it is not there."
  (let ((stamp (ignore-errors (tuatara.io:path-stamp file))))
    (and stamp (list (tuatara.io:stamp-ino stamp) (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp)))))

(defun state-list-limit (list)
  "How many entries LIST keeps now: the value of its LIMIT-VARIABLE."
  (symbol-value (state-list-limit-variable list)))

(defun state-entry-key (list entry)
  "What makes ENTRY of LIST the same as another: its KEY, or its path."
  (funcall (or (state-list-key list) (state-list-path-of list)) entry))

(defun state-list-find (list key)
  "The entry of LIST that KEY makes (STATE-ENTRY-KEY): a bookmark by its name,
a place by its file; NIL when there is none."
  (find key (state-list-entries list) :key (lambda (entry) (state-entry-key list entry)) :test #'equal))

(defstruct (state-contents (:constructor make-state-contents (entries unread version lines))
                           (:copier nil) (:predicate nil))
  "What the file of a list kept between sessions holds: ENTRIES, the latest
first; UNREAD, the lines that are no entry this release reads, in their order;
VERSION, the version its first line says, 1 when it says none; LINES, a table
from each of ENTRIES to the line it was read from."
  entries unread version lines)

(defun state-version-line (list version)
  "The line beginning the file of LIST, a list kept between sessions, written
in the format of VERSION: # tuatara NAME VERSION."
  (format nil "# tuatara ~a ~d" (state-list-name list) version))

(defun state-line-version (list line)
  "The version of the format of LIST's file that LINE, a comment line of it,
says (STATE-VERSION-LINE), or NIL when it says none."
  (let ((name (state-list-name list)))
    (and (uiop:string-prefix-p "# tuatara " line)
         (string= name line :start2 10 :end2 (min (length line) (+ 10 (length name))))
         (> (length line) (+ 11 (length name)))
         (char= #\Space (char line (+ 10 (length name))))
         (let ((version (ignore-errors (parse-integer line :start (+ 11 (length name))))))
           (and version (plusp version) version)))))

(defun read-state-contents (list file)
  "What FILE, LIST's file, holds (STATE-CONTENTS): its entries, the latest
first, each once (the first of those with one key), at most as many as LIST
keeps; the lines that make no entry, at most as many again, to be written back
as they are; the version its first line says. Empty when there is no FILE, and
NIL when it is there and cannot be read. Lines beginning with # are comments."
  (handler-case
      (with-open-file (in (sb-ext:parse-native-namestring file) :if-does-not-exist nil :external-format '(:utf-8 :replacement #\?))
        (let ((entries '()) (count 0) (unread '()) (unread-count 0) (version 1) (most (state-list-limit list))
              (seen (make-hash-table :test 'equal)) (lines (make-hash-table :test 'eq)))
          (when in
            (loop for line = (read-line in nil)
                  while line
                  do (cond ((zerop (length line)))
                           ((char= (char line 0) #\#)
                            (let ((said (state-line-version list line)))
                              (when said (setf version (max version said)))))
                           (t (let ((entry (ignore-errors (funcall (state-list-parse list) line))))
                                (cond ((null entry)
                                       (when (< unread-count most)
                                         (push line unread)
                                         (incf unread-count)))
                                      ((< count most)
                                       (let ((key (state-entry-key list entry)))
                                         (unless (gethash key seen)
                                           (setf (gethash key seen) t
                                                 (gethash entry lines) line)
                                           (push entry entries)
                                           (incf count))))))))))
          (make-state-contents (nreverse entries) (nreverse unread) version lines)))
    (error () nil)))

(defun read-state-file (list file)
  "The entries of LIST as FILE has them, the latest first, each once (the
first of those with one key), at most as many as LIST keeps; empty when it
cannot be read. Lines that make no entry are passed over (READ-STATE-CONTENTS)."
  (let ((contents (read-state-contents list file)))
    (if contents (state-contents-entries contents) '())))

(defun state-file-text (list contents entries)
  "The text of LIST's file holding ENTRIES, CONTENTS what it held before
\(READ-STATE-CONTENTS): the version line, the newer of LIST's and the file's;
LIST's header; a line for each entry, as it was read for one read; then the
lines that made no entry, as they were."
  (with-output-to-string (out)
    (write-line (state-version-line list (max (state-list-version list) (state-contents-version contents))) out)
    (when (state-list-header list) (write-string (state-list-header list) out))
    (let ((lines (state-contents-lines contents)))
      (dolist (entry entries)
        (write-line (or (gethash entry lines) (funcall (state-list-print list) entry)) out)))
    (dolist (line (state-contents-unread contents))
      (write-line line out))))

(defun apply-state-changes (list entries changes)
  "ENTRIES of LIST with CHANGES made to them in turn, the oldest first: each
(:PUT NEW MOST), NEW put first, those of ENTRIES with one of their keys taken
out, and at most MOST kept; (:REPLACE NEW), each entry with the key of one
of NEW that one instead, where it stands, those not there left out; or (:DROP
PATHS), the entries about PATHS taken out."
  (dolist (change changes entries)
    (ecase (first change)
      (:put (destructuring-bind (new most) (rest change)
              (let ((keys (mapcar (lambda (entry) (state-entry-key list entry)) new)))
                (setf entries (append new (remove-if (lambda (entry) (member (state-entry-key list entry) keys :test #'equal))
                                                     entries)))
                (when (> (length entries) most)
                  (setf entries (subseq entries 0 most))))))
      (:replace (let ((new (second change)))
                  (setf entries (mapcar (lambda (entry)
                                          (or (find (state-entry-key list entry) new
                                                    :key (lambda (n) (state-entry-key list n)) :test #'equal)
                                              entry))
                                        entries))))
      (:drop (let ((paths (second change)))
               (setf entries (remove-if (lambda (entry) (member (funcall (state-list-path-of list) entry) paths :test #'equal))
                                        entries)))))))

(defun changes-for (file changes)
  "The changes among CHANGES (each (FILE . CHANGE), the latest first) made to
FILE, the oldest first."
  (loop for (to . change) in (reverse changes)
        when (string= to file) collect change))

(defun state-list-entries (list)
  "The entries of LIST, the latest first: as its file has them, read again
once another session has changed it, with the changes made here and not yet
written made to them; at most as many as it keeps. The same list is given
again while nothing changed; empty when there is no state directory."
  (let* ((file (or (ignore-errors (state-list-path list)) (return-from state-list-entries '())))
         (stamp (state-file-stamp file))
         (lock (state-list-lock list))
         (known nil))
    (sb-thread:with-mutex (lock)
      (when (and (equal file (state-list-file list)) (equal stamp (state-list-stamp list)))
        (when (state-list-shown list)
          (return-from state-list-entries (state-list-shown list)))
        (setf known t)))
    (let ((disk (if known (state-list-disk list) (read-state-file list file)))
          (most (state-list-limit list)))
      (sb-thread:with-mutex (lock)
        ;; what was read kept, unless the writer has put what it wrote in place meanwhile
        (unless (and (not known) (equal file (state-list-file list)) (not (equal stamp (state-list-stamp list)))
                     (equal (state-list-stamp list) (state-file-stamp file)))
          (setf (state-list-file list) file
                (state-list-stamp list) stamp
                (state-list-disk list) disk))
        (let ((entries (apply-state-changes list (state-list-disk list)
                                            (changes-for file (append (state-list-pending list) (state-list-writing list))))))
          (setf (state-list-shown list) (if (> (length entries) most) (subseq entries 0 most) entries)))))))

(defun note-state-change (list change)
  "Make CHANGE to LIST: at once in memory, and on disk in the background
\(START-STATE-WRITER); none when there is no state directory."
  (let ((file (ignore-errors (state-list-path list))))
    (when file
      (sb-thread:with-mutex ((state-list-lock list))
        (push (cons file change) (state-list-pending list))
        (incf (state-list-noted list))
        (setf (state-list-shown list) nil))
      (start-state-writer))))

(defun state-list-put (list entries)
  "Put ENTRIES first in LIST, in their order, those with the same keys taken
out, at most as many kept as LIST keeps; written in the background, with
what other sessions wrote meanwhile."
  (when entries
    (note-state-change list (list :put entries (state-list-limit list)))))

(defun state-list-replace (list entries)
  "Put ENTRIES in LIST where those with the same keys stand, the order kept;
one not there is not added. Written in the background, with what other
sessions wrote meanwhile."
  (when entries
    (note-state-change list (list :replace entries))))

(defun state-list-drop (list paths)
  "Take the entries about PATHS out of LIST; written in the background."
  (when paths
    (note-state-change list (list :drop paths))))

(defun write-state-file (list file changes)
  "Write CHANGES (the oldest first) to FILE, LIST's file, holding its lock: the
file read again, the changes made to what it has, and written back whole,
readable by you alone, the lines it holds that make no entry kept
\(STATE-FILE-TEXT). :WRITTEN, with the entries written as a second value;
:BUSY when another session held the lock for long, or the file could not be
read; NIL when it could not be written."
  (let ((written nil) (unread nil) (entries '()))
    (if (tuatara.io:call-with-state-lock
         file
         (lambda ()
           (let ((contents (read-state-contents list file)))
             (if contents
                 (progn
                   (setf entries (apply-state-changes list (state-contents-entries contents) changes))
                   (ignore-errors
                    (tuatara.io:write-private-file file (state-file-text list contents entries))
                    (setf written t)))
                 ;; never written over what could not be read
                 (setf unread t)))
           t)
         :wait 2)
        (cond (written (values :written entries))
              (unread :busy))
        ;; the lock not had: another session holds it, or the directory is gone
        (and (ignore-errors (tuatara.io:path-stamp (tuatara.io:parent-path file))) :busy))))

(defun write-state-list (list)
  "Write the changes made to LIST and not yet written, each to its file; those
another session kept from being written wait for the next time. True when
there were any."
  (let ((changes (sb-thread:with-mutex ((state-list-lock list))
                   (setf (state-list-tried list) (state-list-noted list))
                   (let ((pending (state-list-pending list)))
                     (setf (state-list-writing list) (append pending (state-list-writing list))
                           (state-list-pending list) '())
                     pending))))
    (when changes
      (dolist (file (remove-duplicates (mapcar #'car changes) :test #'string=))
        (multiple-value-bind (how entries)
            ;; one that cannot be written is let go of, never in the way of the others
            (tuatara.runtime:ignoring-errors-reported ("writing ~a" file)
              (write-state-file list file (changes-for file changes)))
          (let ((stamp (and (eq how :written) (state-file-stamp file))))
            (sb-thread:with-mutex ((state-list-lock list))
              (let ((mine (remove-if-not (lambda (change) (string= (car change) file)) changes)))
                (setf (state-list-writing list) (remove-if (lambda (change) (member change mine :test #'eq)) (state-list-writing list)))
                (when (eq how :busy)
                  (setf (state-list-pending list) (append (state-list-pending list) mine))))
              (when (and (eq how :written) (equal file (state-list-file list)))
                (setf (state-list-disk list) entries
                      (state-list-stamp list) stamp))
              (setf (state-list-shown list) nil)))))
      t)))

(sb-ext:defglobal **state-writer** nil
  "The thread writing the lists kept between sessions, while it runs.")

(sb-ext:defglobal **state-writer-lock** (sb-thread:make-mutex :name "state writer")
  "Held while the writer of the lists kept between sessions is started or ends.")

(defun state-writer-loop ()
  "Write the changes made to the lists kept between sessions, again and again
while more are made, then end."
  (loop
    (dolist (list *state-lists*) (write-state-list list))
    (sb-thread:with-mutex (**state-writer-lock**)
      (when (every (lambda (list) (= (state-list-noted list) (state-list-tried list))) *state-lists*)
        (setf **state-writer** nil)
        (return)))))

(defun start-state-writer ()
  "Start the thread writing the lists kept between sessions, unless it runs
already; one at a time, so that changes made together are written together."
  (sb-thread:with-mutex (**state-writer-lock**)
    (unless **state-writer**
      (setf **state-writer**
            (tuatara.runtime:make-guarded-thread
             (lambda ()
               (unwind-protect (state-writer-loop)
                 ;; however it ends, another can start
                 (sb-thread:with-mutex (**state-writer-lock**)
                   (when (eq **state-writer** sb-thread:*current-thread*)
                     (setf **state-writer** nil)))))
             :name "tuatara writing state")))))

(defun finish-state-writes (&optional editor)
  "Wait a moment (TUATARA.RUNTIME:*STOP-WAIT*) for the lists kept between
sessions to be written, those another session kept from being written tried
once more: as the editor stops. EDITOR is not used."
  (declare (ignore editor))
  (when (some (lambda (list) (state-list-pending list)) *state-lists*)
    (start-state-writer))
  (let ((thread (sb-thread:with-mutex (**state-writer-lock**) **state-writer**)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (tuatara.runtime:join-within thread))))

(defun install-state-lists ()
  "Write what is left of the lists kept between sessions as the editor stops,
after every other function then (*EXIT-FUNCTIONS*)."
  (setf *exit-functions* (append (remove 'finish-state-writes *exit-functions*) (list 'finish-state-writes))))
