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
;;;;
;;;; A file begins with a line naming the list and the version of its
;;;; format (# tuatara bookmarks 1; a file without one is version 1). A later
;;;; release may add fields at the end of a line, after every field of today
;;;; written even when empty, or lines of a new kind. An earlier release reads
;;;; the fields it knows; it writes back as they were the entries it does not
;;;; change, the lines it cannot read (as many again as the list keeps, all of
;;;; a list you make) and the comment lines of a later release's file. An
;;;; entry it changes is written as it writes one, without the later fields,
;;;; and a line it cannot read goes when an entry of the same name or path is
;;;; put, replaced or taken out. In a list of what you trusted, a line of a
;;;; later version with fields this release does not know is not read: what
;;;; it would not understand is never taken as trust.

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

(defun state-line-path-after (line spaces)
  "The path LINE, a line of a list kept between sessions, writes after its
first SPACES spaces (STATE-LINE-PATH); an empty string when it has fewer."
  (let ((start 0))
    (loop repeat spaces
          do (let ((space (position #\Space line :start start)))
               (if space (setf start (1+ space)) (return-from state-line-path-after ""))))
    (state-line-path line start)))

(defun parse-absolute-path-line (line)
  "The path LINE, a line of a list of paths kept between sessions, writes
\(STATE-LINE-PATH), or NIL for a line that writes none: a path begins with /."
  (let ((path (state-line-path line)))
    (and (plusp (length path)) (char= #\/ (char path 0)) path)))

(defstruct (state-list (:constructor make-state-list (name parse print limit-variable
                                                      &key header (path-of #'identity) key (version 1) (class :remembered)
                                                        (fields 1) trust line-key))
                       (:copier nil))
  "A list kept between sessions in a file of the state directory: NAME, the
file's name there; PARSE, a function making an entry of a line (NIL for a
line to pass over); PRINT, one making the line of an entry; LIMIT-VARIABLE,
the variable saying how many entries are kept; HEADER, lines written first, or
NIL; PATH-OF, a function giving the path an entry is about; KEY, one giving
what makes two entries one (PATH-OF when NIL); VERSION, the version of the
format its lines are written in, said in the file's first line; CLASS, :KEPT
for a list you make yourself, whose loss would lose work, or :REMEMBERED for
one of what you did, kept for convenience; FIELDS, how many fields parted by
tabs a line of today's format has at most; TRUST, true for a list of what you
trusted, whose lines of a later version with more fields are not read;
LINE-KEY, a function giving the name or path a line is about, read or not, to
match PATH-OF (the first field, STATE-LINE-PATH, when NIL). What is known of
it: FILE, the
native name of the file read last, its STAMP then and the entries it had
then, DISK; PENDING, the changes made and not yet taken to be written, and
WRITING, those being written, the latest first, each (FILE . CHANGE); SHOWN,
the entries as STATE-LIST-ENTRIES gave them last; NOTED and TRIED, how many
changes were made, and how many there were when the writer last took them;
LOCK, held while these change; UNREADABLE, the file last said could not be
read, until it is written."
  name parse print limit-variable header path-of key version class fields trust line-key
  (unreadable nil)
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
and the keys: :HEADER, :PATH-OF, :KEY, :VERSION, :CLASS, :FIELDS, :TRUST and
:LINE-KEY), and known to the
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
  "The value of LIST's LIMIT-VARIABLE: how many entries it keeps, or, for a
list you make (CLASS :KEPT), how many it may have before adding one says so."
  (symbol-value (state-list-limit-variable list)))

(defun state-list-most (list)
  "How many entries LIST keeps, those past them dropped: its limit, or NIL for
a list you make (CLASS :KEPT), of which no entry is ever dropped for its
number."
  (and (not (eq (state-list-class list) :kept)) (state-list-limit list)))

(defun state-list-past-limit (list)
  "How many entries LIST, a list you make (CLASS :KEPT), has, when that is
more than its limit; NIL when it is not."
  (let ((count (length (state-list-entries list))))
    (and (> count (state-list-limit list)) count)))

(defun state-entry-key (list entry)
  "What makes ENTRY of LIST the same as another: its KEY, or its path."
  (funcall (or (state-list-key list) (state-list-path-of list)) entry))

(defun state-list-find (list key)
  "The entry of LIST that KEY makes (STATE-ENTRY-KEY): a bookmark by its name,
a place by its file; NIL when there is none."
  (find key (state-list-entries list) :key (lambda (entry) (state-entry-key list entry)) :test #'equal))

(defstruct (state-contents (:constructor make-state-contents (entries unread version lines comments))
                           (:copier nil) (:predicate nil))
  "What the file of a list kept between sessions holds: ENTRIES, the latest
first; UNREAD, the lines that are no entry this release reads, in their order;
VERSION, the version its first line says, 1 when it says none; LINES, a table
from each of ENTRIES to the line it was read from; COMMENTS, the comment lines
of a file of a later version than the list's, but the version line."
  entries unread version lines comments)

(defun state-line-key (list line)
  "The name or path LINE of LIST, an entry or not, is about (LINE-KEY)."
  (ignore-errors (funcall (or (state-list-line-key list) #'state-line-path) line)))

(defun state-version-line (list version)
  "The line beginning the file of LIST, a list kept between sessions, written
in the format of VERSION: # tuatara NAME VERSION."
  (format nil "# tuatara ~a ~d" (state-list-name list) version))

(defun state-line-version (list line)
  "The version of the format of LIST's file that LINE, a comment line of it,
says (STATE-VERSION-LINE), or NIL when it says none."
  (let* ((name (state-list-name list)) (start (+ 11 (length name))))
    (and (uiop:string-prefix-p "# tuatara " line)
         (string= name line :start2 10 :end2 (min (length line) (+ 10 (length name))))
         (< start (length line) (+ start 7))
         (char= #\Space (char line (1- start)))
         (every #'digit-char-p (subseq line start))
         (let ((version (parse-integer line :start start)))
           (and (plusp version) version)))))

(defun read-state-contents (list file)
  "What FILE, LIST's file, holds (STATE-CONTENTS): its entries, the latest
first, each once (the first of those with one key), at most as many as LIST
keeps (STATE-LIST-MOST); the lines that make no entry, as many again at most
and every one for a list you make, to be written back as they are; the
version its first line says, and the comment lines of a later version. In a
list of what you trusted (TRUST), a line of a later version with more fields
than LIST's makes no entry. Empty when there is no FILE, and NIL when it is
there and cannot be read. Lines beginning with # are comments."
  (handler-case
      (with-open-file (in (sb-ext:parse-native-namestring file) :if-does-not-exist nil :external-format '(:utf-8 :replacement #\?))
        (let ((entries '()) (count 0) (unread '()) (unread-count 0) (version 1) (most (state-list-most list))
              (comments '()) (seen (make-hash-table :test 'equal)) (lines (make-hash-table :test 'eq)))
          (when in
            (loop for line = (read-line in nil)
                  while line
                  do (cond ((zerop (length line)))
                           ((char= (char line 0) #\#)
                            (let ((said (state-line-version list line)))
                              (if said
                                  (setf version (max version said))
                                  (push line comments))))
                           (t (let ((entry (and (not (and (state-list-trust list)
                                                          (> version (state-list-version list))
                                                          (> (1+ (count #\Tab line)) (state-list-fields list))))
                                                (ignore-errors (funcall (state-list-parse list) line)))))
                                (cond ((null entry)
                                       (when (or (null most) (< unread-count most))
                                         (push line unread)
                                         (incf unread-count)))
                                      ((or (null most) (< count most))
                                       (let ((key (state-entry-key list entry)))
                                         (unless (gethash key seen)
                                           (setf (gethash key seen) t
                                                 (gethash entry lines) line)
                                           (push entry entries)
                                           (incf count))))))))))
          (make-state-contents (nreverse entries) (nreverse unread) version lines
                               (and (> version (state-list-version list)) (nreverse comments)))))
    (error () nil)))

(defun read-state-file (list file)
  "The entries of LIST as FILE has them, the latest first, each once (the
first of those with one key), at most as many as LIST keeps (STATE-LIST-MOST); empty when it
cannot be read. Lines that make no entry are passed over (READ-STATE-CONTENTS)."
  (let ((contents (read-state-contents list file)))
    (if contents (state-contents-entries contents) '())))

(defun state-file-text (list contents entries)
  "The text of LIST's file holding ENTRIES, CONTENTS what it held before
\(READ-STATE-CONTENTS): the version line, the newer of LIST's and the file's;
LIST's header and the comment lines of a later version's file; a line for
each entry, as it was read for one read; then the lines that made no entry,
as they were."
  (with-output-to-string (out)
    (write-line (state-version-line list (max (state-list-version list) (state-contents-version contents))) out)
    (when (state-list-header list) (write-string (state-list-header list) out))
    (dolist (line (state-contents-comments contents))
      (write-line line out))
    (let ((lines (state-contents-lines contents)))
      (dolist (entry entries)
        (write-line (or (gethash entry lines) (funcall (state-list-print list) entry)) out)))
    (dolist (line (state-contents-unread contents))
      (write-line line out))))

(defun apply-state-changes (list entries changes &optional unread)
  "ENTRIES of LIST with CHANGES made to them in turn, the oldest first: each
\(:PUT NEW MOST), NEW put first, those of ENTRIES with one of their keys taken
out, and at most MOST kept (all when MOST is NIL); (:REPLACE NEW), each entry
with the key of one of NEW that one instead, where it stands, those not there
left out; or (:DROP PATHS), the entries about PATHS taken out. UNREAD, lines
of LIST's file that make no entry, lose those about the paths each change is
about (STATE-LINE-KEY); what is left of them is the second value."
  (flet ((forget-unread (paths)
           (when unread
             (setf unread (remove-if (lambda (line) (member (state-line-key list line) paths :test #'equal)) unread))))
         (paths-of (new) (mapcar (state-list-path-of list) new)))
    (dolist (change changes (values entries unread))
      (case (first change)
        ((:put :replace) (forget-unread (paths-of (second change))))
        (:drop (forget-unread (second change))))
      (setf entries (apply-state-change list entries change)))))

(defun apply-state-change (list entries change)
  "ENTRIES of LIST with CHANGE made to them (APPLY-STATE-CHANGES)."
  (flet ((key (entry) (state-entry-key list entry)))
    (ecase (first change)
      (:put (destructuring-bind (new most) (rest change)
              (let* ((keys (mapcar #'key new))
                     (all (append new (remove-if (lambda (entry) (member (key entry) keys :test #'equal)) entries))))
                (if (and most (> (length all) most)) (subseq all 0 most) all))))
      (:replace (let ((new (second change)))
                  (mapcar (lambda (entry) (or (find (key entry) new :key #'key :test #'equal) entry)) entries)))
      (:drop (let ((paths (second change)))
               (remove-if (lambda (entry) (member (funcall (state-list-path-of list) entry) paths :test #'equal))
                          entries))))))

(defun changes-for (file changes)
  "The changes among CHANGES (each (FILE . CHANGE), the latest first) made to
FILE, the oldest first."
  (loop for (to . change) in (reverse changes)
        when (string= to file) collect change))

(defun state-list-entries (list)
  "The entries of LIST, the latest first: as its file has them, read again
once another session has changed it, with the changes made here and not yet
written made to them; at most as many as it keeps (STATE-LIST-MOST). The same list is given
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
          (most (state-list-most list)))
      (sb-thread:with-mutex (lock)
        ;; what was read kept, unless the writer has put what it wrote in place meanwhile
        (unless (and (not known) (equal file (state-list-file list)) (not (equal stamp (state-list-stamp list)))
                     (equal (state-list-stamp list) (state-file-stamp file)))
          (setf (state-list-file list) file
                (state-list-stamp list) stamp
                (state-list-disk list) disk))
        (let ((entries (apply-state-changes list (state-list-disk list)
                                            (changes-for file (append (state-list-pending list) (state-list-writing list))))))
          (setf (state-list-shown list) (if (and most (> (length entries) most)) (subseq entries 0 most) entries)))))))

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
out, at most as many kept as LIST keeps (STATE-LIST-MOST); written in the background, with
what other sessions wrote meanwhile."
  (when entries
    (note-state-change list (list :put entries (state-list-most list)))))

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
:BUSY when another session held the lock for long; :UNREADABLE when the file
is there and could not be read, and so was not written over; NIL when it
could not be written."
  (let ((how nil) (entries '()))
    (if (tuatara.io:call-with-state-lock
         file
         (lambda ()
           (let ((contents (read-state-contents list file)))
             (if contents
                 (multiple-value-bind (now unread)
                     (apply-state-changes list (state-contents-entries contents) changes (state-contents-unread contents))
                   (setf entries now
                         (state-contents-unread contents) unread)
                   (ignore-errors
                    (tuatara.io:write-private-file file (state-file-text list contents entries))
                    (setf how :written)))
                 (setf how :unreadable)))
           t)
         :wait 2)
        (if (eq how :written) (values :written entries) how)
        ;; the lock not had: another session holds it, or the directory is gone
        (and (ignore-errors (tuatara.io:path-stamp (tuatara.io:parent-path file))) :busy))))

(defparameter *state-changes-waiting* 64
  "How many changes to a list kept between sessions wait at most for its file
to be read again, those of one kind one after another made one first.")

(defun merge-state-changes (list older newer)
  "One change to LIST doing what OLDER, then NEWER, of the same kind, do."
  (flet ((once (entries) (remove-duplicates entries :key (lambda (entry) (state-entry-key list entry))
                                                    :test #'equal :from-end t)))
    (ecase (first newer)
      (:put (let ((most (third newer)) (new (once (append (second newer) (second older)))))
              (list :put (if (and most (> (length new) most)) (subseq new 0 most) new) most)))
      (:replace (list :replace (once (append (second newer) (second older)))))
      (:drop (list :drop (union (second newer) (second older) :test #'equal))))))

(defun compact-state-changes (list changes)
  "CHANGES to one file of LIST, each (FILE . CHANGE), the latest first, made
fewer: those of one kind one after another made one (MERGE-STATE-CHANGES), and
the latest *STATE-CHANGES-WAITING* kept."
  (let ((out '()))
    (dolist (change (reverse changes))
      (let ((last (first out)))
        (if (and last (eq (first (cdr last)) (first (cdr change))))
            (setf (first out) (cons (car change) (merge-state-changes list (cdr last) (cdr change))))
            (push change out))))
    (if (> (length out) *state-changes-waiting*) (subseq out 0 *state-changes-waiting*) out)))

(defun write-state-list (list)
  "Write the changes made to LIST and not yet written, each to its file; those
another session kept from being written wait for the next time, and those to
a file that could not be read wait, made fewer (COMPACT-STATE-CHANGES), it said
once in the echo area and the debug log. True when there were any."
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
          (let ((stamp (and (eq how :written) (state-file-stamp file)))
                (say nil))
            (sb-thread:with-mutex ((state-list-lock list))
              (let ((mine (remove-if-not (lambda (change) (string= (car change) file)) changes)))
                (setf (state-list-writing list) (remove-if (lambda (change) (member change mine :test #'eq)) (state-list-writing list)))
                (case how
                  (:busy (setf (state-list-pending list) (append (state-list-pending list) mine)))
                  (:unreadable
                   (let ((waiting (append (remove-if-not (lambda (change) (string= (car change) file)) (state-list-pending list))
                                          mine)))
                     (setf (state-list-pending list)
                           (append (remove-if (lambda (change) (string= (car change) file)) (state-list-pending list))
                                   (compact-state-changes list waiting))))
                   (unless (equal file (state-list-unreadable list))
                     (setf (state-list-unreadable list) file say t)))
                  (:written (when (equal file (state-list-unreadable list))
                              (setf (state-list-unreadable list) nil)))))
              (when (and (eq how :written) (equal file (state-list-file list)))
                (setf (state-list-disk list) entries
                      (state-list-stamp list) stamp))
              (setf (state-list-shown list) nil))
            (when say
              (tuatara.runtime:debug-report :file "cannot read ~a: its changes wait" file)
              (message "Cannot read ~a: its changes wait until it can be" file)))))
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
