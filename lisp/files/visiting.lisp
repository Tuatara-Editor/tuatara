;;;; lisp/files/visiting.lisp --- Asking for files and directories, visiting, saving and renaming files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The questions for a path (with the paths chosen before first), the views
;;;; a file may be shown in, and the commands that find, save, write and
;;;; rename the file of a buffer.

(in-package #:tuatara.defaults)

;;; Asking for files and directories

(defun buffer-visiting (path &key except)
  "The buffer visiting the file at PATH (a native name or a pathname), found
as \\[find-file] finds one, by the file and not the name it was opened by
\(TUATARA.IO:BUFFER-FOR-FILE), other than EXCEPT; NIL when there is none."
  (let ((expanded (ignoring-errors-reported ("expanding ~a" path) (tuatara.io:expand-path path))))
    (and expanded (tuatara.io:buffer-for-file expanded :except except))))

(define-setting *last-used-first* t
  "Whether the questions that choose a file or a directory (\\[find-file], \\[rimu]
and the rest) start where you chose last.
Either way the directories come first, then the files, each in order of name,
numbers by their value (file2 before file10), case not counted. A path opened
safely, or in one of *UNTRUSTED-DIRECTORIES*, is never remembered so.
- t: they start in the directory of the path chosen last in any of them, and list
  first, in each directory, what was chosen there last (or something in it), TAB
  then going the way last gone; the others follow in order of name. As you
  type, what was chosen there lately comes first among the names matching as
  closely.
- nil: they start in the directory of the current buffer, their lists in order of
  name alone."
  :values boolean)

(define-setting *path-delete-by-branches* t
  "What \\<path-question>\\[minibuffer-path-backward-delete] does in the questions
that choose a file or a directory (\\[find-file] and the rest), as the reverse
of \\`TAB'.
- t: after a whole name, a directory's just after its / or a file's as it is
  named, it paints the end of the name, past where it begins as another entry
  of its directory does, and deletes that next; that leaves the start both
  match by, and so back, branch by branch, to the directory alone. Typing adds
  to the path as ever. Within a name still being typed it deletes a character
  (the standard).
- :directories: so after a directory's / alone; after a file's name it deletes
  a character.
- nil: it deletes the character before the cursor, as everywhere."
  :values (member t :directories nil))

(defvar *chosen-paths-kept* 1000
  "How many files and directories chosen in the questions for them are kept,
to offer again first.")

(define-state-list *chosen-paths* "chosen-paths" 'parse-absolute-path-line 'escape-path '*chosen-paths-kept*
  :line-key 'parse-absolute-path-line)

(defun chosen-paths-file ()
  "Where the files and directories chosen lately are kept: in the state directory."
  (state-list-path *chosen-paths*))

(defun chosen-paths ()
  "The files and directories chosen lately in the questions for them (\\[find-file],
\\[rimu] and the rest), native names, the latest first, with what other
sessions chose (STATE-LIST-ENTRIES)."
  (state-list-entries *chosen-paths*))

(defun shown-safely-p (native)
  "True when a buffer shows the file or directory NATIVE (a native name) opened
safely: visiting it, or listing it with \\[rimu]."
  (let ((directory (tuatara.io:slash-ended native)))
    (some (lambda (buffer)
            (and (tuatara.io:safe-visit-p buffer)
                 (let ((file (tuatara.buffer:buffer-file buffer))
                       (listed (buffer-given-directory buffer)))
                   (or (and file (equal (tuatara.io:native file) native))
                       (and listed (equal (tuatara.io:slash-ended (tuatara.io:native listed)) directory))))))
          tuatara.buffer:*buffers*)))

(defun note-chosen-path (path &key directory safe)
  "Remember PATH, a file or a directory (with DIRECTORY, a directory whatever its
name ends in) just chosen in a question for one, as the latest: the next
question starts in its directory and that directory's list offers it first; kept
in the state directory, with what other sessions chose (STATE-LIST-PUT).
Nothing is remembered of a path opened safely (SAFE, or a buffer showing it
so: SHOWN-SAFELY-P) or in one of *UNTRUSTED-DIRECTORIES*; a later question
neither starts there nor offers it."
  (let ((native (ignoring-errors-reported ("expanding ~a" path) (tuatara.io:native (tuatara.io:expand-path path)))))
    (when (and native (plusp (length native)) (not safe)
               (not (untrusted-directory-of native)) (not (shown-safely-p native)))
      (state-list-put *chosen-paths*
                      (list (if directory (tuatara.io:slash-ended native) native))))))

(defun note-visited-path (path buffer &key directory)
  "Remember PATH, just chosen and shown in BUFFER (NIL when it could not be
shown), as NOTE-CHOSEN-PATH does, unless BUFFER shows it opened safely or
is a view of it as a file from outside (VISIT-PATH-ONCE)."
  (when buffer
    (note-chosen-path path :directory directory
                           :safe (or (tuatara.io:safe-visit-p buffer)
                                     (tuatara.buffer:buffer-local buffer :viewed-untrusted)))))

(defun named-safe-p (native)
  "True when NATIVE, a path remembered, is in one of *UNTRUSTED-DIRECTORIES* by
the names as written (PATH-WITHIN-P), or a buffer shows it opened safely; true
for every path when *UNTRUSTED-DIRECTORIES* cannot be read. The disk is not
asked."
  (or (shown-safely-p native)
      (handler-case (reporting-errors ("looking for ~a in the untrusted directories" native)
                      (some (lambda (directory) (tuatara.io:path-within-p native directory))
                            (untrusted-directory-names)))
        (error () t))))

(defun writing-chosen-p (native)
  "True when NATIVE, a path remembered, is never offered first in a question
whose answer is written to: in one of *UNTRUSTED-DIRECTORIES* (NAMED-SAFE-P), or
in an evidence directory (EVIDENCE-PATH-P)."
  (or (named-safe-p native) (evidence-path-p native)))

(defun last-chosen-native-directory (&key writing)
  "The directory, a native name ending in /, of the latest path chosen in a
question for a file or a directory (CHOSEN-PATHS) that is not of evidence
\(NAMED-SAFE-P; with WRITING, for a question whose answer is written to,
WRITING-CHOSEN-P). This is the path itself when it is a directory (its name
ends in /), else the directory it is in. The value is NIL when there is none or
*LAST-USED-FIRST* is NIL."
  (let ((last (and *last-used-first* (find-if-not (if writing #'writing-chosen-p #'named-safe-p) (chosen-paths)))))
    (and last (if (directory-input-p last) last (tuatara.io:parent-path last)))))

(defvar *slow-listings* '()
  "The directories whose listing in a question for a path had not ended within
*FILE-LISTING-WAIT*, still going (a drive that does not answer): each
\(DIRECTORY . GATHERING), DIRECTORY a native name ending in /. One is let go of
once its listing ends.")

(defun slow-listing-p (directory)
  "True while the listing of DIRECTORY (a native name) begun in an earlier
question has not ended (*SLOW-LISTINGS*); those ended are let go of."
  (setf *slow-listings* (remove-if (lambda (entry)
                                     (declare (notinline gathering-done))
                                     (gathering-done (cdr entry)))
                                   *slow-listings*))
  (and (assoc (tuatara.io:slash-ended directory) *slow-listings* :test #'string=) t))

(defun path-question-start (&key writing)
  "The text a question for a path starts with when it is given none: the
directory chosen last (LAST-CHOSEN-DIRECTORY), else, or while that directory's
listing has not ended (SLOW-LISTING-P), that of the current buffer
\(BUFFER-DIRECTORY). With WRITING, for a question whose answer is written
to, never a directory of evidence (*EVIDENCE-DIRECTORIES*): your home directory in its
place."
  (let ((directory (last-chosen-native-directory :writing writing)))
    (if (and directory (not (slow-listing-p directory)))
        (abbreviated-path directory)
        (let ((here (buffer-directory (current-buffer))))
          (if (and writing (evidence-path-p here)) "~/" (abbreviated-path here))))))

(defun writing-start (initial)
  "INITIAL, the text a question whose answer is written to starts with, or
in its place, when it names a place in an evidence directory, the directory
PATH-QUESTION-START offers for writing, with INITIAL's name after it when
INITIAL names a file."
  (if (and initial (plusp (length initial))
           (evidence-path-p (tuatara.io:native (tuatara.io:expand-path initial))))
      (concatenate 'string (path-question-start :writing t)
                   (if (directory-input-p initial) "" (tuatara.io:native-file-name initial)))
      initial))

(defun chosen-ranks (directory chosen &key writing)
  "How lately each entry of DIRECTORY (a native name ending in /) was chosen,
or something in it, by CHOSEN, the paths chosen lately (CHOSEN-PATHS). The
result is a table from its name, a directory's with a slash after it, to its
place among them, 0 the latest. Those in one of *UNTRUSTED-DIRECTORIES* are left out
\(SAFE-DIRECTORY-OF, which may ask the disk; called where the directory is read),
and with WRITING, for a question whose answer is written to, those in an
evidence directory (EVIDENCE-PATH-P): only the paths in DIRECTORY are asked."
  (let ((ranks (make-hash-table :test 'equal)) (length (length directory)))
    (loop for path in chosen
          for place from 0
          when (and (> (length path) length) (string= directory path :end2 length))
            do (let* ((slash (position #\/ path :start length))
                      (name (if slash (subseq path length (1+ slash)) (subseq path length))))
                 ;; what is in one of *untrusted-directories*, or evidence for a place to write, is offered first never
                 (unless (or (untrusted-directory-of path) (and writing (evidence-path-p path)))
                   (unless (gethash name ranks) (setf (gethash name ranks) place))
                   ;; a directory chosen itself, its name ending in / or not
                   (unless (or slash (gethash (concatenate 'string name "/") ranks))
                     (setf (gethash (concatenate 'string name "/") ranks) place)))))
    ranks))

(defvar *file-candidates-kept* nil
  "The candidates DIRECTORY-CANDIDATES made last, with what tells whether they
are still good: (DIRECTORY TIME CHOSEN WRITING CANDIDATES), the directory's
native name, its time of change, the paths chosen lately as they were, and
whether they were made for a question whose answer is written to.")

(defparameter *file-listing-wait* 1/10
  "Seconds a question for a file waits for a directory to be read before it
shows that it is still being read, the editor free meanwhile (READ-PATH).")

(defun input-directory-path (input)
  "The directory INPUT, a path being typed, names the entries of: its part up
to its last slash as an absolute pathname, the current directory when it has
none; a directory on another machine as its name, /ssh:HOST:/directory/."
  (let ((dir (input-directory input)))
    (if (remote-name-p dir)
        dir
        (tuatara.io:expand-path (if (string= dir "") "./" dir)))))

(defun directory-candidates (expanded chosen &key writing)
  "Completion candidates for the entries of the directory EXPANDED (an absolute
pathname, or the name of a directory on another machine), directories first
and marked by a trailing slash. Each has its permissions, its size (or what
it is, for a named pipe or a device) and when it last changed beside it.
Those chosen lately, by CHOSEN, the paths chosen lately (CHOSEN-PATHS) or
NIL, or with something in them chosen, are ranked the latest first
\(CHOSEN-RANKS, WRITING as it has it). Nothing but the directory is opened,
so a named pipe in it cannot hold the editor up. The candidates of a
directory are made once while it does not change, not again at each key
typed in it. The directory is read on the thread calling this, which in a
question for a file is one of its own (READ-PATH)."
  (when (remote-name-p expanded)
    (return-from directory-candidates (remote-candidates expanded)))
  (let* ((native (tuatara.io:native expanded))
         (time (let ((stamp (tuatara.io:path-stamp expanded :failure nil))) (and stamp (tuatara.io:stamp-mtime stamp))))
         (kept *file-candidates-kept*))
    (if (and kept time (equal (first kept) native) (eql (second kept) time) (eq (third kept) chosen)
             (eq (fourth kept) writing))
        (fifth kept)
        (let ((candidates (file-candidates-of expanded chosen writing)))
          (when time (setf *file-candidates-kept* (list native time chosen writing candidates)))
          candidates))))

(defun file-candidates-of (expanded chosen writing)
  "The completion candidates of the entries of the directory EXPANDED, as
DIRECTORY-CANDIDATES gives them, ranked by CHOSEN, the paths chosen lately,
with WRITING as CHOSEN-RANKS has it: the one chosen there last first, then
the directories and then the files, each in order of name
\(NATURAL-STRING-LESSP)."
  (let* ((entries (tuatara.io:directory-entries expanded :stamps t
                                                         :order #'tuatara.text:natural-string-lessp))
         (ranks (chosen-ranks (tuatara.io:slash-ended (tuatara.io:native expanded)) chosen :writing writing))
         (now (get-universal-time)))
    (flet ((fields (kind stamp)
             (if stamp (stamp-fields stamp now) (list (tuatara.io:kind-words kind)))))
      (last-chosen-first
       (append
        (loop for (name kind nil nil stamp) in entries
              when (eq kind :directory)
                collect (let ((shown (concatenate 'string name "/")))
                          (make-candidate shown :annotation (fields kind stamp) :rank (gethash shown ranks))))
        (loop for (name kind nil nil stamp) in entries
              unless (eq kind :directory)
                collect (make-candidate name :annotation (fields kind stamp) :rank (gethash name ranks))))))))

(defun last-chosen-first (candidates)
  "CANDIDATES, a fresh list, with the one chosen last (the lowest rank)
moved to the front, the others in their order."
  (let ((latest nil))
    (dolist (c candidates)
      (let ((rank (candidate-rank c)))
        (when (and rank (or (null latest) (< rank (candidate-rank latest))))
          (setf latest c))))
    (if latest (cons latest (delete latest candidates :count 1 :test #'eq)) candidates)))

(defun input-name-start (input)
  "Where the name being typed in INPUT, a path, begins: after its last slash,
or after the /ssh:HOST: of a file on another machine when that comes later."
  (max (1+ (or (position #\/ input :from-end t) -1)) (or (remote-prefix-length input) 0)))

(defun input-directory (input)
  "The directory part of INPUT, a path being typed: up to its last slash, or
the /ssh:HOST: naming the home directory on another machine."
  (subseq input 0 (input-name-start input)))

(defun substitute-file-input (input)
  "INPUT, a path being typed, as the file prompts read it: a path typed after
the directory offered starts again, from its last // on without the first
slash (~/notes//etc/hosts is /etc/hosts) and from its last /~/ on without what
is before the ~ (/etc/~/notes is ~/notes)."
  (let* ((double (search "//" input :from-end t))
         (home (or (search "/~/" input :from-end t)
                   (and (>= (length input) 2) (string= "/~" input :start2 (- (length input) 2))
                        (- (length input) 2))))
         (from (max (if double (1+ double) 0) (if home (1+ home) 0))))
    (if (plusp from) (subseq input from) input)))

(defun rewrite-file-input (input)
  "INPUT, a path being typed, started again once it ends in // or /~/
(SUBSTITUTE-FILE-INPUT): the text the line holds in place of what was typed."
  (let ((n (length input)))
    (if (or (and (>= n 2) (string= "//" input :start2 (- n 2)))
            (and (>= n 3) (string= "/~/" input :start2 (- n 3))))
        (substitute-file-input input)
        input)))

(defun resolve-file-input (input chosen)
  "The path to visit when CHOSEN (a candidate string or the raw input) was
chosen while INPUT was typed: the candidate is relative to INPUT's directory part."
  (let ((start (input-name-start input)))
    (if (and (plusp start) (not (find #\/ chosen :end (max 0 (1- (length chosen))))))
        (concatenate 'string (subseq input 0 start) chosen)
        chosen)))

(defun input-when-directory (candidates input)
  "The selection of a file or directory question, as INPUT changes: the line
typed itself when it names a directory (ends in /), so that RET takes that
directory; else the first of CANDIDATES."
  (declare (ignore candidates))
  (if (directory-input-p input) :input 0))

(defun directory-input-p (path)
  "True when PATH, a path typed or chosen, names a directory: it ends in /."
  (and (plusp (length path)) (char= #\/ (char path (1- (length path))))))

(defvar *path-question-keymap* (make-keymap :name "path-question")
  "The keys of a question for a path (READ-PATH), beside the minibuffer's:
\\<path-question>\\[minibuffer-home-directory] starts the path again from your
home directory.")

(defvar *asking-for-path* nil
  "The question for a path asking now (READ-PATH), a PATH-QUESTION, or NIL.")
(pushnew '*asking-for-path* *question-variables*)

(defun asking-for-path-p (buffer)
  "True while a question for a file or a directory asks, whatever BUFFER is."
  (declare (ignore buffer))
  (and *asking-for-path* *editor* (editor-minibuffer-active *editor*) t))

(define-command minibuffer-home-directory (:interactive nil)
  "In a question for a file or a directory, put ~/, your home directory, in place
of the path typed, listing what is in it."
  (replace-minibuffer-text 0 (length (editor-minibuffer-text *editor*)) "~/"))

(commands-for-mode 'asking-for-path-p "a question for a file or a directory" 'minibuffer-home-directory)

(defun shared-start (a b)
  "How many characters the strings A and B begin with alike."
  (or (mismatch a b) (length a)))

(defun branch-point (name names)
  "Where NAME, the last part of a path typed, last branches from the other
entries of its directory, NAMES: the longest start it shares with one of them
that does not begin with NAME whole, 0 when it shares none. Deleting the rest
leaves the start that matches them too."
  (let ((most 0))
    (dolist (other names most)
      (unless (uiop:string-prefix-p name other)
        (setf most (max most (shared-start name other)))))))

(defun path-question-names (text)
  "The names of the entries listed so far of the directory of TEXT, the line
of the question for a path asking now, a directory's without its /."
  (let ((question *asking-for-path*))
    (and question
         (mapcar (lambda (c) (string-right-trim "/" (candidate-string c)))
                 (path-question-entries question text)))))

(defun mark-to-branch-point ()
  "Mark for the next deletion the part of the last name of the path typed past
where it branches from the other entries listed of its directory
\(BRANCH-POINT), the whole name when it shares no start with one of them or
none are listed yet."
  (let* ((text (editor-minibuffer-text *editor*))
         (start (input-name-start text))
         (name (subseq text start)))
    (when (plusp (length name))
      (mark-minibuffer-deletion (+ start (branch-point name (path-question-names text)))))))

(define-command minibuffer-path-backward-delete (:interactive nil)
  "In a question for a file or a directory, delete back by the branches of the
path, the reverse of \\`TAB', as *PATH-DELETE-BY-BRANCHES* says. After a whole name,
just after a directory's / (which goes) or after a file's name as an entry is
named, paint the end of the name past where it last shares a start with
another entry of its directory; the next
\\<path-question>\\[minibuffer-path-backward-delete] deletes what is painted
and paints back to the branch before, until the directory alone is left, all
its entries listed. Typing adds to the path as ever, and any other key takes
the paint away. Within a name still being typed, or with the setting nil,
delete the character before the cursor."
  (let* ((text (editor-minibuffer-text *editor*))
         (point (editor-minibuffer-point *editor*))
         (by-branches (and tuatara-user:*path-delete-by-branches* (= point (length text))))
         (files (eq t tuatara-user:*path-delete-by-branches*))
         (start (input-name-start text)))
    (cond ((delete-marked-minibuffer-text)
           (when by-branches (mark-to-branch-point)))
          ((drop-offered-text))
          ((and by-branches (> point 1) (char= #\/ (char text (1- point))))
           (replace-minibuffer-text (1- point) point "")
           (mark-to-branch-point))
          ((and files by-branches (< start point)
                (member (subseq text start) (path-question-names text) :test #'string=))
           (mark-to-branch-point))
          ((plusp point)
           (replace-minibuffer-text (1- point) point "")))))

(commands-for-mode 'asking-for-path-p "a question for a file or a directory" 'minibuffer-path-backward-delete)

(define-keys *path-question-keymap* '(("S-TAB" minibuffer-home-directory)
                                      ("DEL" minibuffer-path-backward-delete)
                                      ("C-h" minibuffer-path-backward-delete)))

(define-record (path-question (:constructor make-path-question (&key writing keep)) (:copier nil))
  "A question for a path being asked (READ-PATH): WRITING and KEEP as it was
given them; LISTED, the entries of the directory typed as far as they are
read; LISTED-FOR, that directory as typed; LISTING, what reads it
\(START-GATHERING) until it is read."
  (writing nil :documentation "True for a question whose answer is written to.")
  (keep nil :documentation "What tells the entries offered, a function of an entry's name (a
directory's ending in /), or NIL to offer every entry.")
  (listed '() :documentation "The entries of the directory typed, as far as read.")
  (listed-for nil :documentation "The directory typed, whose entries are listed.")
  (listing nil :documentation "What reads the directory typed, until it is read."))

(defun path-question-take (question)
  "Take what the listing of QUESTION's directory found, once it has: true when
it came now."
  (multiple-value-bind (found done) (take-gathered (path-question-listing question))
    (when done (setf (path-question-listing question) nil))
    (when found (setf (path-question-listed question) (first found)))
    (and found t)))

(defun path-question-list (question path)
  "Start listing the directory of PATH, a path typed in QUESTION, on a thread of
its own, the paths chosen lately ranked first (those in evidence left out of
a question whose answer is written to); one read quickly is shown at once.
A directory whose listing has not ended yet is not listed again."
  (let* ((expanded (input-directory-path path))
         (directory (tuatara.io:slash-ended (tuatara.io:native expanded)))
         (chosen (and *last-used-first* (chosen-paths)))
         (writing (path-question-writing question))
         (keep (path-question-keep question)))
    (setf (path-question-listed question) '() (path-question-listing question) nil)
    (unless (slow-listing-p directory)
      (let ((listing (start-gathering
                      "tuatara file names"
                      (lambda (give cancel)
                        (declare (ignore cancel))
                        (let ((all (directory-candidates expanded chosen :writing writing)))
                          (funcall give (if keep
                                            (remove-if-not (lambda (c) (funcall keep (candidate-string c))) all)
                                            all)))))))
        (setf (path-question-listing question) listing)
        (unless (wait-for-gathering listing *file-listing-wait*)
          (push (cons directory listing) *slow-listings*))
        (path-question-take question)))))

(defun path-question-entries (question typed)
  "The entries QUESTION offers for TYPED, the line as typed: the directory is
listed again only when its part of the line changes."
  (let ((directory (input-directory (substitute-file-input typed))))
    (unless (equal directory (path-question-listed-for question))
      (path-question-stop question)
      (setf (path-question-listed-for question) directory)
      (path-question-list question (substitute-file-input typed)))
    (path-question-listed question)))

(defun path-question-source (question)
  "What QUESTION's listing has found since last asked, whether it is still
reading, and how many entries there are, as a question's SOURCE gives them."
  (if (path-question-listing question)
      (values (path-question-take question) (and (path-question-listing question) t)
              (length (path-question-listed question)))
      (values nil nil (length (path-question-listed question)))))

(defun path-question-stop (question)
  "Stop the listing of QUESTION's directory, when it is still under way."
  (let ((listing (path-question-listing question)))
    (when listing (stop-gathering listing))))

(defun read-path (prompt &key writing initial keep preview)
  "Ask with PROMPT for a path, as READ-FILE-PATH and READ-DIRECTORY-PATH do.
The question starts from INITIAL (as standard the directory of the path chosen
last in such a question, else the directory the current buffer stands in, by
PATH-QUESTION-START). With WRITING, for a question whose answer is written
to, it is never a place in an evidence directory, which is then neither where
the question starts (WRITING-START) nor ranked first among the entries. The
name is completed against the entries of the directory being typed
\(DIRECTORY-CANDIDATES; with KEEP, a function of an entry's name, those it is true
for, a directory's name ending in /), matched by any part of the name. While
nothing is typed after the directory, the entry chosen there last comes first,
the others in order of name; as you type, the best matches
first, those chosen lately before others. A / or ~/ typed after it starts the
path again (SUBSTITUTE-FILE-INPUT), the line holding just that
\(REWRITE-FILE-INPUT), and
\\<path-question>\\[minibuffer-home-directory] starts it from ~/. The line
typed is selected while it names a directory (ends in /), unpainted, as typing
adds to it, else the first entry matching.
\\[minibuffer-path-backward-delete] deletes back by the branches of the path.
A directory chosen is gone into, its entries offered in turn. With
PREVIEW, a function as CALL-WITH-WINDOW-PREVIEW gives, the file selected is
shown meanwhile. Returns the path, and as a second value true when it is the
line typed, taken as it is (\\<minibuffer>\\[minibuffer-finish] on it, or
\\[minibuffer-submit-input]), NIL when it is a file chosen."
  ;; the entries of a directory read afresh for each question
  (setf *file-candidates-kept* nil)
  (let* ((initial (or initial (path-question-start :writing writing)))
         (dir (if writing (writing-start initial) initial))
         (input ""))
    (call-with-window-preview
     (lambda (candidate made)
       (let ((chosen (candidate-result candidate)))
         (when (stringp chosen)
           (preview-file-buffer (tuatara.io:native (tuatara.io:expand-path
                                                    (substitute-file-input
                                                     (resolve-file-input (substitute-file-input input) chosen))))
                                made))))
     (lambda (show)
       (loop
         (setf input dir)
         (let* ((question (make-path-question :writing writing :keep keep))
                (chosen (unwind-protect
                             (let ((*asking-for-path* question))
                               (completing-read prompt
                                                (lambda (typed)
                                                  (setf input typed)
                                                  (path-question-entries question typed))
                                                :initial dir
                                                :keymap *path-question-keymap*
                                                :keep-order :until-typed
                                                :paint-input nil
                                                :rewrite #'rewrite-file-input
                                                :select #'input-when-directory
                                                :reselect t
                                                :preview (and preview show)
                                                :source (lambda () (path-question-source question))))
                          (path-question-stop question)))
                ;; the line typed taken (RET on it, or C-j), not a candidate
                (typed (and (stringp chosen) (string= chosen input)))
                (path (substitute-file-input (if typed chosen (resolve-file-input (substitute-file-input input) chosen)))))
           (if (and (not typed) (directory-input-p path))
               (setf dir path)
               (return (values path typed)))))))))

(defun read-file-path (prompt &key new-file preview (note t) writing keep)
  "Ask for a file with PROMPT, completing its name against the directory being
typed, from the directory of the path chosen last in such a question, else the
directory the current buffer stands in (BUFFER-DIRECTORY): that of its file,
the one it lists, or *DEFAULT-DIRECTORY*, your home directory written ~
\(READ-PATH, PATH-QUESTION-START). The file chosen is remembered as the
latest (NOTE-CHOSEN-PATH) unless NOTE is NIL. A / or ~/ typed after it starts the path
again (SUBSTITUTE-FILE-INPUT). The first file selected, or the line typed when
it names a directory or nothing matches; a directory candidate descends into
it. Returns the path typed or chosen, or NIL (said) when nothing was named; a
directory typed and taken comes with :DIRECTORY as a second value, or with
NEW-FILE :NEW, for a new file there. With PREVIEW, the file selected is
shown meanwhile (CALL-WITH-WINDOW-PREVIEW). WRITING, for a question whose
answer is written to, and KEEP, what tells the entries offered, as READ-PATH
has them."
  (let ((path (read-path prompt :preview preview :writing writing :keep keep)))
    (cond ((zerop (length path))
           (if new-file
               (values "./" :new)
               (progn (message "No file name") nil)))
          ((directory-input-p path) (values path (if new-file :new :directory)))
          (t (when note (note-chosen-path path)) path))))

(defun read-directory-path (prompt &key writing initial (note t))
  "Ask for a directory with PROMPT, from INITIAL (as standard the directory of the
path chosen last in such a question, else the directory the current buffer
stands in: PATH-QUESTION-START), as READ-FILE-PATH asks for a file. The
directories in the one being typed are offered, matched by any part of the name,
those chosen lately first. A / or ~/ typed after it starts the path again.
The line typed is selected while it names a directory (ends in /), else the
first directory matching. \\<minibuffer>\\[minibuffer-finish] or
\\[minibuffer-complete] on a directory goes into it. \\[minibuffer-finish] on
the line typed, one step above the first (\\[minibuffer-previous-candidate]),
takes it, as \\[minibuffer-submit-input] does at any time: a directory, or a
new name. Returns what was taken, ./ when nothing was typed. It is
remembered as chosen lately (NOTE-CHOSEN-PATH) unless NOTE is NIL. A caller
that shows the directory then remembers it itself, once shown
\(NOTE-VISITED-PATH). WRITING, for a question whose answer is written to, is
as READ-PATH has it."
  (let* ((path (read-path prompt :initial initial :keep #'directory-input-p :writing writing))
         (path (if (zerop (length path)) "./" path)))
    (when note (note-chosen-path path :directory t))
    path))

(defun read-native-path (prompt &key initial (kind :directory) (note t) writing)
  "Ask with PROMPT for a path of KIND, :DIRECTORY (or a new name) as
READ-DIRECTORY-PATH asks, :FILE as READ-FILE-PATH asks, or :ANY as READ-PATH
asks, from INITIAL where it takes one, and return what was taken as a whole
native name, ~ and the like expanded; NIL when nothing was typed. A directory
or file taken is remembered as chosen lately unless NOTE is NIL, for a
caller that opens it and remembers it once opened (NOTE-VISITED-PATH).
WRITING, for a question whose answer is written to (a place to save or copy
to), never starts in an evidence directory nor offers one first (READ-PATH)."
  (let ((typed (ecase kind
                 (:directory (read-directory-path prompt :initial initial :note note :writing writing))
                 (:file (read-file-path prompt :note note :writing writing))
                 (:any (read-path prompt :initial initial :writing writing)))))
    (and typed (plusp (length typed)) (tuatara.io:native (tuatara.io:expand-path typed)))))

(defun read-new-native-path (prompt &rest keys)
  "Ask with PROMPT for a path that is not there yet, as READ-NATIVE-PATH asks
with KEYS; NIL, said, when nothing was typed or something is there already
\(a link counting as there)."
  (let ((path (apply #'read-native-path prompt keys)))
    (cond ((null path) (message "No name given") nil)
          ((tuatara.io:path-kind path :follow nil) (message "~a is there already" (abbreviated-path path)) nil)
          (t path))))

;;; Views of files, and visiting them

(defvar *file-views* '()
  "Functions of a file's PATH and a function SHOW of a buffer that may show the
file visited as something other than its text (a PDF as its pages): each
returns the buffer it showed with SHOW, or NIL to leave the file to the next,
and at last to its text. A file asked to be opened safely shows its text
always; one from outside (UNTRUSTED-PATH-P) is shown only by the views of
*SANDBOXED-FILE-VIEWS*, and never when it is or may be evidence
\(MAY-BE-EVIDENCE-P).")

(defvar *sandboxed-file-views* '()
  "The views of *FILE-VIEWS* that show a file from outside (UNTRUSTED-PATH-P),
opened safely, when it is not evidence. A view named here promises, and
nothing checks it, that the file itself is read only by programs shut in by
the sandbox: never parsed by Lisp, run, decrypted or handed to a program
outside it; what those programs write may be read in Lisp, within limits.
It fetches nothing and writes nothing beside the file. Such a view is called
with *VISITING-SAFELY* true and leaves the file to its octets rather than
read it any other way. A package adds the name of its view here as well as
to *FILE-VIEWS* only when it keeps this promise.")

(defvar *file-view-stamp* nil
  "While a view of *SANDBOXED-FILE-VIEWS* is asked to show a file from
outside, the stamp of the file as its visit judged it (FILE-VIEW-STAMP).")

(defun file-view-stamp ()
  "While a view of *FILE-VIEWS* is asked to show a file from outside (one of
*SANDBOXED-FILE-VIEWS*), the stamp of the file as its visit judged it; NIL
otherwise. A view that reads the file later, on a thread of its own, takes it
now and gives it as :SAME-AS to TUATARA.IO:CALL-WITH-FILE or
TUATARA.IO:COPY-REGULAR-FILE, so that a file put in its place meanwhile is
refused. The buffers MAKE-FILE-VIEW-BUFFER makes keep it (:VIEWED-STAMP)."
  *file-view-stamp*)

(defvar *file-view-said* :none
  "Why a view of *FILE-VIEWS* left the file being visited to its octets, while
a visit is made (NIL until one says); :NONE outside a visit.")

(defun file-view-refuses (format &rest arguments)
  "Say why a view of *FILE-VIEWS* leaves the file to its octets (FORMAT with
ARGUMENTS): within a visit, kept to be said with how the file opened (a
binary one safely); elsewhere, at once."
  (let ((words (apply #'format nil format arguments)))
    (if (eq *file-view-said* :none)
        (message "~a" words)
        (setf *file-view-said* words))))

(defun file-view-path (path)
  "The native path of PATH when it is a regular file a view of *FILE-VIEWS*
may show; NIL otherwise."
  (let ((native (ignoring-errors-reported ("expanding ~a" path) (tuatara.io:native (tuatara.io:expand-path path)))))
    (and native (eq :regular (tuatara.io:path-kind native)) native)))

(defun file-view-can-draw-p (native decoder install)
  "True when a program shut in by the sandbox can draw the file NATIVE:
the sandbox is ready (or not known yet: the view says why when it cannot),
DECODER is true and the file is not too large to be given to it
\(TUATARA.IO:SANDBOX-INPUT-REFUSAL); otherwise said why (FILE-VIEW-REFUSES),
INSTALL the words naming what to install, and NIL."
  (let ((status (tuatara.io:sandbox-status-now))
        (refusal (tuatara.io:sandbox-input-refusal (tuatara.io:path-size native))))
    (cond ((not (member status '(:ready :unknown)))
           (file-view-refuses "~a opens as octets: ~a" (tuatara.io:native-file-name native) (no-sandbox-words status))
           nil)
          ((null decoder)
           (file-view-refuses "~a opens as octets: ~a" (tuatara.io:native-file-name native) install)
           nil)
          (refusal
           (file-view-refuses "~a opens as octets: ~a" (tuatara.io:native-file-name native) refusal)
           nil)
          (t t))))

(defun file-view-show (native views view-path make show)
  "Show the view of the file NATIVE with SHOW, a function of the buffer: the
one among VIEWS (buffers) whose VIEW-PATH (a function of a buffer) is NATIVE,
else the one MAKE (a function of no arguments) makes; the file remembered
among the recent ones. Returns the buffer."
  (let ((buffer (or (find native views :key view-path :test #'equal) (funcall make))))
    (funcall show buffer)
    (remember-file native)
    buffer))

(defun make-file-view-buffer (native mode text what)
  "A new buffer showing the file at NATIVE as WHAT (\"its pages\", \"its
picture\"), in major MODE: read only, shut in, its directory the file's,
TEXT in it but blank until the picture is drawn. \\[hex-mode] in it opens
the file's own octets (FILE-VIEW-HEX)."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name (tuatara.io:native-file-name native))
                                            :text text)))
    (set-major-mode buffer mode)
    (setf (buffer-read-only-p buffer) t
          (tuatara.buffer:buffer-local buffer :sandboxed) t
          ;; a picture in place of the text, no cursor, even before the first is drawn
          (tuatara.buffer:buffer-local buffer :picture-view) t
          ;; blank while a picture is on its way, never the text in its place
          (tuatara.buffer:buffer-local buffer :picture-pending) t
          (tuatara.buffer:buffer-local buffer :viewed-file) native
          (tuatara.buffer:buffer-local buffer :viewed-as) what
          (tuatara.buffer:buffer-local buffer :viewed-stamp) (file-view-stamp)
          (buffer-given-directory buffer) (tuatara.io:parent-path native))
    buffer))

(defun reading-buffer (registry view-path native name text mode start &key private-directory)
  "The buffer among those REGISTRY (the symbol of a variable holding a list of
buffers) holds whose VIEW-PATH (a function of a buffer) is NATIVE, the path of
a file; else a new one for that file. The new buffer is called NAME (made
unique), saying TEXT until the file is read, read only, in major MODE, its
directory the file's, added to REGISTRY. START is called with it, to say what
it shows and start reading the file in the background. With PRIVATE-DIRECTORY,
the name of a directory in the private cache directory (\"palimpsest/\"), the new
buffer is given a directory of its own in it before START (NEW-PRIVATE-DIRECTORY).
BUFFER-PRIVATE-DIRECTORY names that directory. It is deleted when the buffer is
killed, once the buffer's jobs have ended (FORGET-PRIVATE-DIRECTORY)."
  (or (find-if (lambda (b) (and (live-buffer-p b) (equal native (funcall view-path b)))) (symbol-value registry))
      (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name name) :text text)))
        (set-major-mode buffer mode)
        (setf (buffer-read-only-p buffer) t
              (buffer-given-directory buffer) (tuatara.io:parent-path native)
              (symbol-value registry) (cons buffer (remove-if-not #'live-buffer-p (symbol-value registry))))
        (when private-directory
          (setf (tuatara.buffer:buffer-local buffer 'private-directory)
                (new-private-directory (private-cache-directory private-directory))))
        (funcall start buffer)
        buffer)))

(defun buffer-private-directory (buffer)
  "The directory of BUFFER's own READING-BUFFER made for it in the private cache
directory, a native name ending in /; NIL when it has none."
  (tuatara.buffer:buffer-local buffer 'private-directory))

(defun private-directory-heir (buffer directory)
  "Another live buffer than BUFFER visiting a file in DIRECTORY, for whom it has
to stay; NIL when there is none."
  (find-if (lambda (other)
             (let ((file (and (not (eq other buffer)) (tuatara.buffer:buffer-file other))))
               (and file (uiop:string-prefix-p directory (tuatara.io:native file)))))
           tuatara.buffer:*buffers*))

(defun forget-private-directory (buffer)
  "Kill-buffer function: delete BUFFER's private directory (BUFFER-PRIVATE-DIRECTORY)
and all in it, in the background, once the jobs of BUFFER, stopped as it goes,
have ended; when another buffer visits a file in it, that buffer has it from
then on. The directory is still named while the other kill-buffer functions
run."
  (let* ((directory (buffer-private-directory buffer))
         (heir (and directory (private-directory-heir buffer directory)))
         (threads (remove nil (mapcar #'tuatara.runtime:job-thread (jobs-of buffer)))))
    (when heir
      (setf (tuatara.buffer:buffer-local heir 'private-directory) directory
            directory nil))
    (when directory
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (dolist (thread threads) (sb-thread:join-thread thread :default nil))
         (delete-private-directory directory))
       :name "tuatara private directory deleted"))))

(defun show-reading-result (job path shown &optional (failure "~a could not be read: ~a~%"))
  "Show in the buffer of JOB, a job reading the file at PATH, what was read,
when that buffer is still there. When JOB is done, SHOWN is the text to show
\(point put at its start) or a function of the buffer that shows it. Else the
buffer shows why the file could not be read, point at its start: FAILURE, a
function of the words for how JOB ended returning the text, or a format
control given the file's name and those words."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (live-buffer-p buffer)
      (cond ((not (eq (tuatara.runtime:job-status job) :done))
             (let ((words (tuatara.runtime:job-ending-words job)))
               (set-made-text buffer (if (functionp failure)
                                         (funcall failure words)
                                         (format nil failure (tuatara.io:native-file-name path) words))))
             (setf (tuatara.buffer:buffer-point buffer) 0))
            ((functionp shown) (funcall shown buffer))
            (t (set-made-text buffer shown)
               (setf (tuatara.buffer:buffer-point buffer) 0))))))

(defmethod host-visit-file ((host defaults-host) path &key safe hex)
  "Visit PATH, a file the editor is started with, as \\[find-file] does and
return its buffer, without showing it: with SAFE safely; with HEX as its
octets in the hex view, never shown as text or as a view of its kind first."
  (if hex
      (let ((buffer (tuatara.io:call-with-file-heads
                     (lambda () (nth-value 0 (visit-path-as-text path safe #'identity))))))
        (hex-view-on buffer)
        buffer)
      (nth-value 0 (visit-path path :safe safe :show #'identity))))

(defun hex-view-on (buffer &optional (at 0))
  "Show BUFFER in the hex view (\\[hex-mode]), its cursor on the octet AT."
  (setf (tuatara.display:hex-view-p buffer) t
        (tuatara.display:hex-point buffer) at))

(defun show-octets-in-hex (name octets &key (show #'pop-to-buffer))
  "Show OCTETS in a buffer of their own called NAME, read only, in the hex
view (\\[hex-mode]), with SHOW, a function of the buffer: in another window
as standard. A buffer of that name already there is replaced. Returns the
buffer."
  (let ((old (tuatara.buffer:find-buffer name)))
    (when old (tuatara.buffer:kill-buffer old)))
  (let ((buffer (tuatara.buffer:make-buffer name :text (coerce octets 'tuatara.text:octets))))
    (setf (buffer-read-only-p buffer) t)
    (hex-view-on buffer)
    (funcall show buffer)
    buffer))

(defun file-view-octets (path what)
  "Open the octets of the file at PATH, shown by a view as WHAT (its pages,
its picture), safely (read-only, nothing acted on), in the hex view, in the
window; the view stays as it was."
  (let ((octets (visit-path path :safe t)))
    (hex-view-on octets)
    (message "~a's octets, opened safely: \\[kill-buffer] or \\[switch-to-buffer] goes back to ~a"
             (tuatara.io:native-file-name path) what)))

(defun file-view-fall-to-octets (buffer words)
  "BUFFER, a file shown by a view, has nothing to show: open the file's own
octets, safely, in the hex view, in every window showing BUFFER, which goes,
saying WORDS for why. A copy of a file on another machine stays as long as
the octets do."
  (let ((native (tuatara.buffer:buffer-local buffer :viewed-file))
        (windows (buffer-windows buffer)))
    (when native
      (let ((octets (visit-path native :safe t :show #'identity)))
        (hex-view-on octets)
        (take-place-of octets buffer windows)
        (when (buffer-remote octets)
          (remote-octets-taken-over octets))
        (message "~a: its octets, opened safely" words)))))

(defun take-place-of (new old windows)
  "Have the buffer NEW take the place of OLD, another showing the same file:
shown in WINDOWS, those that showed OLD, holding OLD's private copy and file
on another machine (kept as long as NEW is), using the entries of the cache it
used, its directory when it was given one, and its name; OLD killed."
  (dolist (window windows)
    (tuatara.display:set-window-buffer window new :remember nil))
  (dolist (kept '(private-directory :remote-file))
    (setf (tuatara.buffer:buffer-local new kept) (shiftf (tuatara.buffer:buffer-local old kept) nil)))
  (pass-cache-uses old new)
  (when (buffer-given-directory old)
    (setf (buffer-given-directory new) (buffer-given-directory old)))
  (let ((name (tuatara.buffer:buffer-name old)))
    (tuatara.buffer:kill-buffer old)
    (setf (tuatara.buffer:buffer-name new) name)))

(defun file-view-hex (buffer)
  "For \\[hex-mode] in BUFFER, a file shown as its pages or its picture
\(MAKE-FILE-VIEW-BUFFER): open the file's own octets in the hex view
\(FILE-VIEW-OCTETS), the view staying. True when BUFFER is one."
  (let ((native (tuatara.buffer:buffer-local buffer :viewed-file))
        (what (tuatara.buffer:buffer-local buffer :viewed-as)))
    (when (and native what)
      (file-view-octets native what)
      t)))

(define-setting *hex-view-binary-files* t
  "Whether a file that looks binary is shown in the hex view (\\[hex-mode]) when
it is opened.
A file looks binary when it has a null (NUL) octet in its first 64 KB, as
executables, archives and disk images have and text has not. Pictures and PDFs
shown as such keep their views; \\[hex-mode] shows any file as text again.
- t: shown in the hex view.
- nil: shown as text."
  :values boolean)

(defun show-binary-in-hex (buffer)
  "Show BUFFER, a file just opened, in the hex view when its file looks binary
(TUATARA.IO:BINARY-HEAD-P of its TUATARA.IO:FILE-HEAD) and
*HEX-VIEW-BINARY-FILES* is true."
  (when (and *hex-view-binary-files* (tuatara.buffer:buffer-file buffer)
             (tuatara.io:binary-head-p (tuatara.io:file-head (tuatara.buffer:buffer-file buffer))))
    (hex-view-on buffer (tuatara.buffer:buffer-point buffer))))

(defun visit-path (path &key safe (show (lambda (buffer) (show-buffer buffer))))
  "Visit the file at PATH, as usual or with SAFE safely (TUATARA.IO:VISIT-FILE),
show its buffer with SHOW, a function of the buffer (in the selected window by
default), and put it first among the recent files. A name that does not exist
becomes a new file, except for a safe visit. Visited as usual, a file one of
*FILE-VIEWS* shows otherwise, a PDF, is shown so; one that looks binary, in
the hex view (*HEX-VIEW-BINARY-FILES*). Why a file opened safely unasked is
said, and a file opened safely goes back to where it was left. A file open
already that was left in INSERT state is shown in NORMAL state when it is
visited from another buffer (*SHOWN-AGAIN-FUNCTIONS*); visited from its own
buffer, going to another place in it, it stays as it is. The file's first
octets are read once for all of this (TUATARA.IO:CALL-WITH-FILE-HEADS).
Returns the buffer, and true as a second value when it was open already.
A file on another machine, /ssh:HOST:/path, is read from there first
\(VISIT-REMOTE)."
  (if (remote-name-p path)
      (visit-remote path safe show)
      (tuatara.io:call-with-file-heads (lambda () (visit-path-once path safe show)))))

(defvar *shown-again-functions* '()
  "Functions called with a buffer of a file open already, just shown by a
visit of its file from another buffer (VISIT-PATH). A buffer a command opens
without showing it (a capture's target, a journal's day) has none of them
called.")

(defvar *visiting-safely* nil
  "True while a file or directory is being visited safely, for the views that
show one (*FILE-VIEWS*): a directory's list is then opened safely too. True as
well while a view of *SANDBOXED-FILE-VIEWS* is asked to show a file from
outside.")

(defun run-sandboxed-file-views (path show &key (native path) missing)
  "Ask the views of *FILE-VIEWS* that are of *SANDBOXED-FILE-VIEWS* in turn to
show the file at PATH as such a view shows a file from outside, reading it shut
in by the sandbox, *VISITING-SAFELY* true and *FILE-VIEW-STAMP* the stamp of
NATIVE, PATH's native name: the first that does returns its buffer, shown with
SHOW, a function of the buffer, and marked as a view of a file from outside
\(:VIEWED-UNTRUSTED); NIL when none does. MISSING, words saying that part of
the file has not arrived, or NIL: given, no view is asked, as a decoder would
read what is missing as zeros, and why is said (FILE-VIEW-REFUSES)."
  (if missing
      (progn (file-view-refuses "~a" missing) nil)
      (let ((view (let ((*visiting-safely* t)
                        (*file-view-stamp* (tuatara.io:path-stamp native :failure nil)))
                    (run-hook-until-done (remove-if-not (lambda (view) (member view *sandboxed-file-views*))
                                                        *file-views*)
                                         path show))))
        (when view
          (setf (tuatara.buffer:buffer-local view :viewed-untrusted) t))
        view)))

(defun visit-path-once (path safe show)
  "Visit the file at PATH as VISIT-PATH does, SAFE and SHOW as it takes them,
its first octets read once meanwhile. A file asked to be opened safely is
shown by no view. One from outside (UNTRUSTED-PATH-P) is shown only by a view
of *SANDBOXED-FILE-VIEWS*, which reads it in the sandbox
\(RUN-SANDBOXED-FILE-VIEWS), and by none when it is or may be evidence
\(MAY-BE-EVIDENCE-P); such a view is not noted as the path chosen last. A
directory is listed (\\[rimu]) however it is opened, its access time kept and
nothing in it opened."
  (let* ((*file-view-said* nil)
         (*visiting-safely* (and safe t))
         (untrusted (and (not safe) (untrusted-path-p path)))
         (native (ignoring-errors-reported ("expanding ~a" path) (tuatara.io:native (tuatara.io:expand-path path))))
         (directory (and (or safe untrusted) native
                         (eq :directory (ignoring-errors-reported ("examining ~a" path)
                                          (tuatara.io:path-kind native))))))
    (or (cond ((or directory (not (or safe untrusted))) (run-hook-until-done *file-views* path show))
              ((and untrusted native (not (may-be-evidence-p native)))
               (run-sandboxed-file-views path show :native native)))
        (visit-path-as-text path safe show))))

(defun say-new-file-directory (buffer)
  "When BUFFER is a new file whose directory is not there yet: say so, and that
saving makes it, or why the buffer is read-only when that directory cannot be
made."
  (let* ((path (tuatara.buffer:buffer-file buffer))
         (directory (and path (tuatara.io:parent-path path))))
    (when (and directory (null (tuatara.io:path-stamp path)) (null (tuatara.io:path-stamp directory)))
      (let ((above (tuatara.io:nearest-directory path)))
        (if (tuatara.io:writable-p above)
            (message "(New file; its directory ~a is not there yet: saving asks to make it)"
                     (abbreviated-path directory))
            (message "(New file, read-only: its directory ~a is not there, and ~a may not be written)"
                     (abbreviated-path directory) (abbreviated-path above)))))))

(defun visit-path-as-text (path safe show)
  "Visit the file at PATH as its text (or octets), as VISIT-PATH-ONCE does
when no view of *FILE-VIEWS* shows it, saying why a view left it so
\(*FILE-VIEW-SAID*) with how it opened; called outside a visit, by a view of
its own, it says only how the file opened. A file open already, shown from
another buffer, has *SHOWN-AGAIN-FUNCTIONS* called with it. A file open
already, not safely, that changed on disk since its buffer read it is read
again when the buffer has no changes of its own; with changes, or deleted, it
is left as it is and that is said (REACT-TO-DISK). A file being saved, or on
a network file system, is left to the watcher, which never holds the editor
up."
  ;; none before the editor has a window (a file named as it starts)
  (let ((from (and *editor* (editor-window *editor*) (current-buffer))))
    (multiple-value-bind (buffer existing) (tuatara.io:visit-file path :safe safe)
      (unless existing (show-binary-in-hex buffer))
      (when (and existing (not safe) (not (tuatara.io:safe-visit-p buffer)) (not (tuatara.io:saving-p buffer))
                 (not (eq (tuatara.buffer:buffer-local buffer :file-class) :remote))
                 (member (tuatara.io:check-file buffer) '(:changed :stale :deleted)))
        (react-to-disk (list buffer) :visited t))
      (funcall show buffer)
      (when (and existing (not (eq buffer from)))
        (run-hook *shown-again-functions* buffer))
      (when (and safe existing (not (tuatara.io:safe-visit-p buffer)))
        (message "~a is open already, with its mode; kill that buffer to open the file safely"
                 (tuatara.buffer:buffer-name buffer)))
      (when (and (not safe) existing (tuatara.io:safe-visit-p buffer))
        (if (equal (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path path))
            (message "~a is open safely; ~a" (tuatara.buffer:buffer-name buffer) (end-safe-visit-words buffer))
            (message "~a is the same file as ~a, which is open safely~@[ (~a)~]: that buffer is shown"
                     (abbreviated-path path)
                     (abbreviated-path (tuatara.buffer:buffer-file buffer))
                     (tuatara.io:safe-visit-reason buffer))))
      (let ((said (and (not (eq *file-view-said* :none)) *file-view-said*)))
        (cond ((and (not existing) (tuatara.io:safe-visit-reason buffer)) (say-why-safe buffer said))
              (said (message "~a" said))
              ((and (not existing) (not safe)) (say-new-file-directory buffer))))
      ;; a file opened safely runs nothing after its visit, but goes back to where it was left
      (when (and (not existing) (tuatara.io:safe-visit-p buffer)) (restore-place buffer))
      ;; a new visit as usual was remembered as it was made (*find-file-functions*)
      (when (or existing (tuatara.io:safe-visit-p buffer)) (remember-buffer-file buffer))
      (values buffer existing))))

;;; Finding, saving and renaming files

(defvar *paths-changed-functions* '()
  "Functions called with a list of native paths just made, renamed, written or
removed by a command, so that the lists showing them read them again.")

(defun paths-changed (paths)
  "Tell each of *PATHS-CHANGED-FUNCTIONS* that the native PATHS were just
made, renamed, written or removed."
  (run-hook *paths-changed-functions* paths))

(defvar *file-renamed-functions* '()
  "Functions called with FROM and TO, native paths, once the file FROM is
renamed TO and the buffers visiting it follow it (FOLLOW-RENAMED-FILE).")

(defun follow-renamed-file (from to)
  "Have the buffers visiting FROM visit TO, the same file renamed, named after
it, and tell *FILE-RENAMED-FUNCTIONS*."
  (dolist (buffer tuatara.buffer:*buffers*)
    (when (equal (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path from))
      (let ((old-name (tuatara.io:native-file-name from)))
        (setf (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path to))
        (when (string= (tuatara.buffer:buffer-name buffer) old-name)
          (setf (tuatara.buffer:buffer-name buffer)
                (tuatara.buffer:unique-buffer-name (tuatara.io:native-file-name to)))))))
  (run-hook *file-renamed-functions* from to))

(defun follow-moved-paths (pairs)
  "Have the buffers visiting a file moved by PAIRS, each (FROM . TO) of native
names, a directory's files too, visit it where it went (FOLLOW-RENAMED-FILE)."
  (dolist (buffer tuatara.buffer:*buffers*)
    (let ((file (tuatara.buffer:buffer-file buffer)))
      (when file
        (let ((native (tuatara.io:native file)))
          (loop for (from . to) in pairs
                when (tuatara.io:path-within-p native from)
                  do (follow-renamed-file native (concatenate 'string to (subseq native (length (string-right-trim "/" from)))))
                     (return)))))))

(defun check-not-saving (paths)
  "An error when a buffer is being saved into one of PATHS (or a directory's
files): what it writes would land at the old name once moved."
  (dolist (buffer tuatara.buffer:*buffers*)
    (let ((file (tuatara.buffer:buffer-file buffer)))
      (when (and file (tuatara.io:save-of buffer)
                 (some (lambda (path) (tuatara.io:path-within-p (tuatara.io:native file) path)) paths))
        (refuse "~a is being saved: try again once it is" (tuatara.buffer:buffer-name buffer))))))

(defun rename-path-following (from to &key (on-disk t) (tell t))
  "Rename the file or directory FROM to TO, native names, the buffers visiting it
following it (FOLLOW-RENAMED-FILE), or those visiting a file in the directory
\(FOLLOW-MOVED-PATHS), and with TELL say so to the lists showing them
\(PATHS-CHANGED). An error, nothing renamed, while a buffer is being saved
into it. With ON-DISK NIL, only the buffers follow: a file not written yet,
or one another program renamed."
  (when on-disk
    (check-not-saving (list from))
    (tuatara.io:rename-path from to))
  (if (eq :directory (tuatara.io:path-kind to :follow nil))
      (follow-moved-paths (list (cons from to)))
      (follow-renamed-file from to))
  (when tell
    (paths-changed (list from to))))

(defun find-file-reading (safe &key (prompt (if safe "Find file safely: " "Find file: "))
                                    (show (lambda (buffer) (show-buffer buffer))))
  "Ask for a file with PROMPT (READ-FILE-PATH) and show it with SHOW, a
function of the buffer (in the selected window by default): visited as usual,
or with SAFE safely (VISIT-PATH), a directory listed so, its list shown by SHOW too. A name that does not
exist becomes a new file, except for a safe visit. The path is remembered
as the latest chosen (NOTE-CHOSEN-PATH) unless it opened safely."
  (multiple-value-bind (path kind) (read-file-path prompt :preview *preview-files* :note nil)
    (cond ((null path))
          ;; a directory taken as typed: listed in rimu, shown as SHOW shows
          ((eq kind :directory)
           (note-visited-path path (rimu-show-directory (tuatara.io:native (tuatara.io:expand-path path))
                                                        :safe safe :show show)
                              :directory t))
          ;; noted once shown: what opened safely, asked or not, leaves no trace
          (t (note-visited-path path (visit-path path :safe safe :show show))))))

(define-command find-file ()
  "Visit a file, completing its name against the directory being typed.
The question starts in the directory of the path chosen last in a
question for a file or a directory (*LAST-USED-FIRST*), the entry chosen
there last listed first, the others by name;
\\<path-question>\\[minibuffer-home-directory] starts again from ~/. A
directory candidate descends into it; a name that does not exist becomes
a new file. With a prefix argument (\\[universal-argument]
\\[find-file]), open it safely, as find-file-safely does."
  (find-file-reading (and (prefix-argument) t)))

(define-command find-file-safely ()
  "Open a file safely, for looking at a file from a source you do not trust:
read-only, with no major mode, no hooks and no version control, so that
nothing in the file is acted on; its file system left exactly as it was (no
copy beside it), and its access time too where the system allows. The mode
line says Safe, and the files visited lately remember it: it opens safely
from there again. \\[read-only-mode] allows editing it after all;
\\[end-safe-visit] opens it as usual."
  (find-file-reading t))

(define-command end-safe-visit ()
  "Open the file of this buffer, opened safely, as usual from now on, where it
is. The mode line no longer says Safe, and the buffer is editable when you
may write the file. It gets what \\[find-file] gives a file: its major mode
(or keeps the one chosen meanwhile, its colours now shown), version control,
and a place among the files visited lately that opens it as usual. Point,
changes not saved and undo stay as they are. A file one of *FILE-VIEWS*
shows otherwise, a PDF or a mailbox, is shown so in its place instead, as
\\[find-file] shows it, unless the buffer has changes not saved; one that
view decrypts is said to be. It stays opened safely, and says why
\(SAFE-VISIT-KEPT-REASON), in these cases: the file is evidence
\(*EVIDENCE-DIRECTORIES*) or was when it was opened, it has another name that
may be evidence, or its name leads to another file now than the one read.
A file from another machine still arriving, or stopped before all of it
came, is left as it is till all of it is here, how much has come said."
  (let* ((b (current-buffer))
         (arriving (remote-missing-words b)))
    (cond
      (arriving (message "~a; it can be opened as usual once all of it is here" arriving))
      ((not (tuatara.io:safe-visit-p b))
       (message "~a is not open safely" (tuatara.buffer:buffer-name b)))
      (t
       (let ((why (safe-visit-kept-reason b)))
         (if why
             (message "~a stays opened safely: ~a" (tuatara.buffer:buffer-name b) why)
             (let ((view (and (not (tuatara.buffer:buffer-modified-p b)) (view-in-place-of b))))
               (if view
                   (message "~a is open as usual now, shown as ~a"
                            (tuatara.buffer:buffer-name view)
                            (cond ((tuatara.buffer:buffer-local view :viewed-as))
                                  ((tuatara.buffer:buffer-local view :encrypted) "its text, decrypted by gpg")
                                  (t "its view")))
                   (progn
                     (visit-as-usual b)
                     (message "~a is open as usual now~:[~;; read-only, since you may not write it~]"
                              (tuatara.buffer:buffer-name b) (buffer-read-only-p b)))))))))))

(defun view-in-place-of (b)
  "The buffer of the view of *FILE-VIEWS* that shows B's file, which B visits
safely, having taken B's place (TAKE-PLACE-OF), or shown in the selected
window when B was shown in none; NIL, B left as it is, when no view shows
the file. A copy of a file on another machine is shown only by the views of
*SANDBOXED-FILE-VIEWS*, as when it is first read, and by none while part of
it has not arrived (RUN-SANDBOXED-FILE-VIEWS)."
  (let* ((windows (buffer-windows b))
         (view (let ((*file-view-said* nil))
                 (tuatara.io:call-with-file-heads
                  (lambda ()
                    (let ((native (tuatara.io:native (tuatara.buffer:buffer-file b)))
                          (show (lambda (view) (unless windows (show-buffer view)))))
                      (if (buffer-remote b)
                          (run-sandboxed-file-views native show :missing (remote-missing-words b))
                          (run-hook-until-done *file-views* native show))))))))
    (when view
      (take-place-of view b windows)
      view)))

(defun safe-visit-kept-reason (buffer)
  "Why BUFFER, visiting its file safely, stays so (\\[end-safe-visit]), a
phrase, or NIL. It opened safely for being evidence, or for having another
name that may be (its reason, TUATARA.IO:SAFE-VISIT-REASON, names
*EVIDENCE-DIRECTORIES*). It also stays safe when its name no longer leads to the
file it read, as a link changed meanwhile, when the file there is evidence
now, or when it has another name that may be in an evidence directory. Each is
asked anew, nothing kept used. The copy of a file from another machine stays
safe while that file is evidence, or could not be told not to be, as its
read found (REMOTE-FILE-EVIDENCE), whatever the reason's words."
  (let* ((file (tuatara.buffer:buffer-file buffer))
         (path (and file (tuatara.io:native file)))
         (why (tuatara.io:safe-visit-reason buffer))
         (read (tuatara.io:buffer-file-stamp buffer))
         (remote (buffer-remote buffer)))
    (cond ((and remote (remote-file-evidence remote)))
          ((and (not remote) why (search "*evidence-directories*" why)) why)
          ((and read path (not (tuatara.io:same-stamped-file-p read (tuatara.io:path-stamp path :failure nil))))
           "its name no longer leads to the file it read")
          ((and path (evidence-visit-reason path)))
          ((evidence-elsewhere-p buffer)
           "it has more than one name, and one may be in an evidence directory"))))

(defun visit-as-usual (b)
  "Have B, visiting its file safely, visit it as usual from now on: editable
when you may write the file, with what \\[find-file] gives a file (its
major mode, or the one chosen meanwhile, version control, a place among the
files visited lately)."
  (let* ((mode (buffer-major-mode b))
         (chosen (not (eq (type-of mode) 'fundamental-mode))))
    (tuatara.io:end-safe-visit b)
    (when chosen
      ;; the mode stays as it is set up, its language now parsed
      (let ((language (mode-language mode)))
        (when language (start-parsing b language))))
    (run-hook (if chosen
                  (remove 'set-auto-mode tuatara.buffer:*find-file-functions*)
                  tuatara.buffer:*find-file-functions*)
              b)))

(define-command read-only-mode ()
  "Make the current buffer read-only, or editable again if it is; with a
count, read-only when it is positive, editable when it is zero or less."
  (let* ((b (current-buffer)) (now (switched-by-count (buffer-read-only-p b))))
    (setf (buffer-read-only-p b) now)
    (message "Read-only mode ~:[off~;on~]" now)))

(defvar *after-save-functions* '()
  "Functions called with the buffer after it is saved.")

(defvar *before-save-functions* '()
  "Functions called with the buffer just before it is saved; what they change
is saved with it. One that returns :LATER stops the save: it saves the buffer
itself once ready (SAVE-BUFFER-WITH-CARE).")

(define-command save-buffer ()
  "Save the current buffer to its file, asking for one if it has none, as
\\[find-file] asks. The names of the directory being typed are completed, from
the directory of the path chosen last in a question for a path (else the
buffer's own directory, your home directory for the scratch buffers); a name that
does not exist is the new file. The file is written safely (a new copy
renamed over the old, keeping its owner, permissions and labels); a file
changed on disk since it was read is not overwritten without asking. Every
save is written on a thread of its own and waited for half a second. One that
takes longer, as a large buffer's may, goes on in the background while you
carry on, its progress in the mode line."
  (let ((b (current-buffer)))
    (if (tuatara.buffer:buffer-file b)
        (save-buffer-with-care b)
        (multiple-value-bind (path kind) (read-file-path "Save as: " :writing t)
          (cond ((null path) (message "Not saved"))
                (kind (message "~a is a directory: not saved; give the file's name after it" path))
                (t (save-buffer-with-care b :path path)))))))

(define-command write-file ()
  "Write the current buffer to another file, asked for as \\[find-file] asks
\(\\`C-x C-w'), from the directory of the path chosen last in a question for a
path, else the buffer's own directory. The buffer then visits that file, named
after it, in the major mode its name calls for; the file it visited before is
left as it was. A directory alone is the file's
own name in that directory. A file there already is overwritten only when you
say so."
  (let ((b (current-buffer)))
    (multiple-value-bind (path kind) (read-file-path "Write file: " :writing t)
      (let ((path (if (and path kind (tuatara.buffer:buffer-file b))
                      ;; into the directory, under the file's own name
                      (concatenate 'string path (tuatara.io:native-file-name (tuatara.buffer:buffer-file b)))
                      path)))
        (cond ((null path) (message "Not written"))
              ((char= (char path (1- (length path))) #\/)
               (message "~a is a directory: not written; give the file's name after it" path))
              (t (save-buffer-with-care b :path path)))))))

(define-command rename-visited-file ()
  "Give the file the current buffer visits a new name, asked for as any path
is, its own offered to change: a name alone keeps it in its directory, a path
\(~/notes/a.org, ../a.org) moves it there on the same drive. The file itself is renamed, not
written again, and the buffer follows it, named after it, in the mode its
new name calls for; changes not saved stay so. A name there already, or
one another buffer visits, is refused. A buffer with no file is saved under
the name asked for, as \\[write-file] does; a file visited safely, or on
another machine, is left as it is."
  (let* ((b (current-buffer))
         (file (tuatara.buffer:buffer-file b)))
    (cond ((null file) (write-file))
          ((buffer-remote b) (message "~a is on another machine: its file is left as it is" (buffer-remote-name b)))
          ((tuatara.io:safe-visit-p b) (message "~a is visited safely: its file is left as it is"
                                                (tuatara.buffer:buffer-name b)))
          ((tuatara.io:save-of b) (message "~a is being saved: try again once it is" (tuatara.buffer:buffer-name b)))
          (t
           (let* ((from (tuatara.io:native file))
                  (old (tuatara.io:native-file-name from))
                  ;; directories completed, the name typed taken as it is
                  (new (string-trim " " (read-path (format nil "Rename ~a to: " old)
                                                   :initial (abbreviated-path from) :writing t
                                                   :keep #'directory-input-p)))
                  (to (and (plusp (length new))
                           (tuatara.io:native (tuatara.io:expand-path new :directory (tuatara.io:parent-path from)))))
                  (taken (and to (buffer-visiting to))))
             (cond ((or (null to) (string= to from)) (message "Not renamed"))
                   ((char= (char new (1- (length new))) #\/)
                    (message "~a is a directory: not renamed; give the file's name after it" new))
                   ((tuatara.io:path-stamp to :follow nil) (message "~a is there already: not renamed"
                                                                    (abbreviated-path to)))
                   ((and taken (not (eq taken b)))
                    (message "~a is open in ~a: not renamed" (abbreviated-path to) (tuatara.buffer:buffer-name taken)))
                   ((and (encrypted-name-p (tuatara.io:native-file-name to)) (not (encrypted-name-p old)))
                    (message "~a is not renamed: its name would say encrypted, and its text would be there to read"
                             (tuatara.io:native-file-name to)))
                   (t
                    (handler-case
                        (reporting-errors ("renaming ~a" from)
                          (let ((written (tuatara.io:path-stamp from :follow nil)))
                            ;; a file not written yet has only its buffer to rename
                            (rename-path-following from to :on-disk written)
                            (take-mode-of-name b)
                            (if written
                                (message "Renamed to ~a" (abbreviated-path to))
                                (message "~a now visits ~a, not written yet: \\[save-buffer] writes it"
                                         (tuatara.buffer:buffer-name b) (abbreviated-path to)))))
                      (error (e)
                        (message "~a is not renamed: ~a~@[; \\[write-file] writes it there, the old file left as it is~]"
                                 old e (not (equal (tuatara.io:parent-path from) (tuatara.io:parent-path to)))))))))))))

(defun file-kind (path)
  "What the file at PATH is by its first octets (TUATARA.IO:FILE-HEAD), as two
values, the words and the keyword of TUATARA.IO:OCTETS-KIND, a TIFF's size
read where its first directory stands when that is past them
\(OCTETS-KIND-OF-FILE); NIL when they do not tell, or it cannot be read (a
third value, why)."
  (let ((head (tuatara.io:file-head path)))
    (cond ((null head)
           (handler-case (reporting-errors ("reading ~a" path)
                           (tuatara.io:call-with-file path (constantly nil))
                           (values nil nil "it cannot be read"))
             (error (e) (values nil nil (princ-to-string e)))))
          ((tuatara.io:tiff-octets-p head)
           (multiple-value-bind (words kind)
               (ignoring-errors-reported ("reading ~a" path)
                 (tuatara.io:call-with-file path (lambda (fd native size)
                                                   (declare (ignore native size))
                                                   (octets-kind-of-file fd head))))
             (if (keywordp kind) (values words kind) (tuatara.io:octets-kind head))))
          (t (tuatara.io:octets-kind head)))))
