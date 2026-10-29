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

(defstruct (state-list (:constructor make-state-list (name parse print limit-variable &key header (path-of #'identity) key))
                       (:copier nil))
  "A list kept between sessions in a file of the state directory: NAME, the
file's name there; PARSE, a function making an entry of a line (NIL for a
line to pass over); PRINT, one making the line of an entry; LIMIT-VARIABLE,
the variable saying how many entries are kept; HEADER, lines written first, or
NIL; PATH-OF, a function giving the path an entry is about; KEY, one giving
what makes two entries one (PATH-OF when NIL). What is known of it: FILE, the
native name of the file read last, its STAMP then and the entries it had
then, DISK; PENDING, the changes made and not yet taken to be written, and
WRITING, those being written, the latest first, each (FILE . CHANGE); SHOWN,
the entries as STATE-LIST-ENTRIES gave them last; NOTED and TRIED, how many
changes were made, and how many there were when the writer last took them;
LOCK, held while these change."
  name parse print limit-variable header path-of key
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
and the keys), and known to the writer (*STATE-LISTS*)."
  `(progn
     (defvar ,variable (make-state-list ,name ,@arguments)
       ,(format nil "The list kept between sessions in ~a, in the state directory (STATE-LIST)." name))
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

(defun read-state-file (list file)
  "The entries of LIST as FILE has them, the latest first, each once (the
first of those with one key), at most as many as LIST keeps; empty when it
cannot be read. Lines that make no sense are passed over."
  (ignore-errors
   (with-open-file (in (sb-ext:parse-native-namestring file) :if-does-not-exist nil :external-format '(:utf-8 :replacement #\?))
     (when in
       (let ((entries '()) (seen (make-hash-table :test 'equal)) (count 0) (most (state-list-limit list)))
         (loop for line = (read-line in nil)
               while (and line (< count most))
               do (let ((entry (and (plusp (length line)) (funcall (state-list-parse list) line))))
                    (when entry
                      (let ((key (state-entry-key list entry)))
                        (unless (gethash key seen)
                          (setf (gethash key seen) t)
                          (push entry entries)
                          (incf count))))))
         (nreverse entries))))))

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
readable by you alone. :WRITTEN, with the entries written as a second value;
:BUSY when another session held the lock for long; NIL when it could not be
written."
  (let ((written nil) (entries '()))
    (if (tuatara.io:call-with-state-lock
         file
         (lambda ()
           (setf entries (apply-state-changes list (read-state-file list file) changes))
           (ignore-errors
            (tuatara.io:write-private-file
             file (with-output-to-string (out)
                    (when (state-list-header list) (write-string (state-list-header list) out))
                    (dolist (entry entries)
                      (write-line (funcall (state-list-print list) entry) out))))
            (setf written t))
           t)
         :wait 2)
        (values (and written :written) entries)
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
