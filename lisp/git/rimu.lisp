;;;; lisp/git/rimu.lisp --- Rimu and git: each entry's last commit, and a directory as a commit left it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; G in a rimu list shows, after each name, when the entry last changed in
;;;; git's history and the first line of that commit's message, as Emacs's
;;;; dired-git-info does; a directory's is the last commit of anything in it.
;;;; One git log is read on a thread of its own, the newest commit first, until
;;;; every entry has its commit, and the column fills in when it is done.
;;;; C-x G (M-x git-rewind) in a list lists the directory as a commit left it, in a
;;;; rimu list of its own read from git (git ls-tree), its preview reading
;;;; the files as they were then; p and n go a commit older and newer, RET
;;;; on a file goes through its history, C-x G again back to now.

(in-package #:tuatara.git)

(defun rimu-git-p (buffer)
  "True when the rimu list BUFFER shows the git column (\\<rimu-mode>\\[rimu-toggle-git]):
never in a list opened safely, nor in one of a directory as a commit left it."
  (rimu-column-allowed-p buffer 'rimu-git))

(defun rimu-git-commits (directory names &key cancel)
  "The last commit of each of NAMES, entries of DIRECTORY (a native name
ending in /), from git's history: a hash table from a name to (WHEN SUBJECT),
WHEN as git says it (2 days ago). One log is read (RUN-GIT), the newest
commit first, until each name has its commit or the history ends; a name git
does not know has none. NIL when DIRECTORY is not in a repository (no git
run at all) or git is not there. CANCEL, a cons, stops the reading when its
car turns true."
  (let ((wanted (make-hash-table :test 'equal))
        (found (make-hash-table :test 'equal))
        (left 0))
    ;; only those git keeps: one never committed would have the whole history read
    (multiple-value-bind (tree ok) (run-git directory (list "ls-tree" "--name-only" "-z" "HEAD" ".") :cancel cancel
                                                                                        :seconds *git-look-seconds*)
      (unless ok (return-from rimu-git-commits nil))
      (let ((tracked (make-hash-table :test 'equal)))
        (dolist (path (uiop:split-string (tuatara.text:lenient-string tree)
                                         :separator (string (code-char 0))))
          (when (plusp (length path))
            (setf (gethash (subseq path 0 (or (position #\/ path) (length path))) tracked) t)))
        (dolist (name names)
          (when (and (gethash name tracked) (not (gethash name wanted)))
            (setf (gethash name wanted) t)
            (incf left)))))
    (when (plusp left)
      (let ((commit nil))
        ;; names are NUL-ended (-z), so git quotes none; a line holds several pieces
        (run-git directory (list "log" "--relative" "--name-only" "-z" *git-commit-format* "--" ".")
                 :cancel cancel :seconds *git-look-seconds*
                 :line (lambda (line)
                         (dolist (piece (uiop:split-string line :separator (list (code-char 0))))
                           (cond ((zerop (length piece)))
                                 ((char= (char piece 0) (code-char #x1e))
                                  (let ((fields (parse-git-commit-fields (subseq piece 1))))
                                    (setf commit (and fields (list (git-commit-ago fields)
                                                                   (git-commit-subject fields))))))
                                 (commit
                                  ;; the entry it is under: the path's first part
                                  (let ((name (subseq piece 0 (or (position #\/ piece) (length piece)))))
                                    (when (and (gethash name wanted) (not (gethash name found)))
                                      (setf (gethash name found) commit)
                                      (decf left))))))
                         ;; every name has its commit: the rest of the history is not read
                         (zerop left)))
        ;; what was found shows even when the time limit ended the reading
        found))))

(defun rimu-git-cells (directory entries)
  "The git column's texts for ENTRIES of DIRECTORY: :PENDING, found on a
thread of its own (RIMU-GIT-COMMITS) and given to the lists by
RIMU-COLUMN-READY."
  (rimu-column-in-background
   :git entries "tuatara rimu git column"
   (lambda (names)
     (let* ((commits (rimu-git-commits directory names))
            ;; the messages lined up after the longest date
            (widest (if commits (loop for (when) being the hash-values of commits maximize (text-columns when)) 0)))
       (map 'vector
            (lambda (name)
              (let ((commit (and commits (gethash name commits))))
                (and commit
                     (list (cons (column-text (first commit) widest :elide nil) :list-date)
                           (cons "  " nil)
                           (cons (let ((subject (second commit)))
                                   (if (> (length subject) 72) (subseq subject 0 72) subject))
                                 :list-note)))))
            names)))))

(defun rimu-git-state (directory)
  "The stamps of git's own files that say which commit is checked out and
what is staged (GIT-STAMPS), for the repository or worktree DIRECTORY is in;
NIL when it is in none, or none of those files is there."
  (let* ((top (repository-top directory))
         (own (and top (git-real-directory top)))
         (stamps (and own (git-stamps own))))
    (and (some #'identity stamps) stamps)))

(define-command rimu-toggle-git ()
  "Show or hide, after each name in this list, the entry's last commit in
git's history: when it was made and the first line of its message.
*RIMU-GIT-INFO* says whether new lists show it."
  (rimu-toggle 'rimu-git "Last commits shown" "Last commits hidden"))

;;; A directory as a commit left it

(defparameter *rimu-link-limit* 4096
  "The longest target of a link in a directory as a commit left it that is read
from git; a longer one is shown with none.")

(defun rimu-link-targets (top objects)
  "Where each link of OBJECTS (git's names of blobs, as ls-tree gives them)
leads, read from the repository at TOP by a git cat-file for every 500 of
them: a hash table from an object to its text; an object git does not have,
or one longer than *RIMU-LINK-LIMIT*, is not in it."
  (let ((targets (make-hash-table :test 'equal)))
    ;; 500 names at most go to one git: they fit in its input at once, so it
    ;; never waits on us to read while we wait on it to take them
    (loop for start from 0 below (length objects) by 500
          for some = (subseq objects start (min (length objects) (+ start 500)))
          for octets = (run-git top (list "cat-file" "--batch") :input (format nil "~{~a~%~}" some)
                                :limit (* (length some) (+ *rimu-link-limit* 128)))
          ;; each: OBJECT TYPE SIZE on a line, then SIZE octets and a line end; OBJECT missing for one not there
          do (loop with at = 0
                   while (and octets (< at (length octets)))
                   do (let* ((end (or (position 10 octets :start at) (return)))
                             (header (uiop:split-string (tuatara.text:octets-string octets :start at :end end)
                                                        :separator '(#\Space)))
                             (size (and (= 3 (length header)) (parse-integer (third header) :junk-allowed t))))
                        (setf at (1+ end))
                        (when size
                          (when (<= size *rimu-link-limit*)
                            (setf (gethash (first header) targets)
                                  (tuatara.text:lenient-string (subseq octets at (min (length octets) (+ at size))))))
                          (incf at (1+ size))))))
    targets))

(define-record (git-rewound (:constructor make-git-rewound (top &optional hash)) (:copier nil))
  "A rimu list's directory as a commit left it: the TOP of its repository and
the HASH of the commit shown, NIL until its history is read."
  (top nil :documentation "The top directory of the repository of GIT-REWOUND.")
  (hash nil :documentation "The hash of the commit GIT-REWOUND shows, NIL until the history is read."))

(defmethod rimu-source-top ((source git-rewound))
  "The top of the repository."
  (git-rewound-top source))

(defmethod rimu-source-buffer-name ((source git-rewound) directory)
  "*git rewind: and the directory's name."
  (format nil "*git rewind: ~a*" (rimu-buffer-name directory)))

(defmethod rimu-source-taken ((source git-rewound) buffer)
  "BUFFER is of the repository's top, for git's commands."
  (setf (tuatara.buffer:buffer-local buffer :git-top) (git-rewound-top source)))

(defmethod rimu-source-listing ((source git-rewound) directory)
  "The directory as the commit left it (RIMU-READ-AT-COMMIT)."
  (rimu-read-at-commit (git-rewound-top source) (git-rewound-hash source) directory))

(defun rimu-read-at-commit (top hash directory)
  "A RIMU-LISTING of DIRECTORY, a native name ending in / within the repository
at TOP, as the commit HASH left it. It holds what the directory held as git keeps
it, each with its kind and permissions, a file its size and a link where it
leads, all changed when that commit was made and owned by you, with no inode. It
gives why not, when the commit held no such directory then. A name no file in a
directory can have (., .. or one holding a /) is left out."
  (let* ((dir (subseq directory (length top)))
         (made (* 1000000000
                  (or (parse-integer (git-job-try nil top (list "log" "-1" "--format=%ct" "--end-of-options" hash))
                                     :junk-allowed t)
                      0)))
         (uid (sb-posix:getuid))
         (gid (sb-posix:getgid))
         (rows '()))
    (flet ((entry (name mode size &optional target)
             (tuatara.io:make-listed name (tuatara.text:string-octets name)
                                     ;; no inode: it is not on the disk
                                     (tuatara.io:make-stamp 0 0 size made 1 mode uid gid)
                                     size nil target nil))
           (link-p (mode) (= (logand mode #o170000) #o120000)))
      (multiple-value-bind (tree ok) (git-job-try nil top (list "ls-tree" "-z" "-l" (git-object-name hash dir)))
        (when ok
          (dolist (line (uiop:split-string tree :separator (list (code-char 0))))
            (let ((tab (position #\Tab line)))
              (when tab
                (destructuring-bind (mode type object &optional size)
                    (remove "" (uiop:split-string (subseq line 0 tab) :separator '(#\Space)) :test #'string=)
                  (let ((name (subseq line (1+ tab))))
                    (when (and (plusp (length name))
                               (not (member name '("." "..") :test #'string=))
                               (not (find #\/ name)))
                      (push (list name (or (parse-integer mode :radix 8 :junk-allowed t) #o100644) type object
                                  (or (and size (parse-integer size :junk-allowed t)) 0))
                            rows))))))))
        (let ((targets (rimu-link-targets top (loop for (nil mode nil object) in rows
                                                    when (link-p mode) collect object))))
          (make-rimu-listing
           directory
           (coerce (list* (entry "." #o40755 0) (entry ".." #o40755 0)
                          (loop for (name mode type object size) in (nreverse rows)
                                collect (cond ((string= type "tree") (entry name #o40755 0))
                                              ;; a submodule, a repository of its own: a directory that lists nothing
                                              ((string= type "commit") (entry name #o40755 0))
                                              ;; git keeps no permissions for a link: a link's own
                                              ((link-p mode) (entry name #o120777 size (gethash object targets "")))
                                              (t (entry name mode size)))))
                   'simple-vector)
           nil
           (and (not ok) (format nil "it was not there as ~a left it" (subseq hash 0 (min 7 (length hash)))))))))))

(defmethod rimu-source-call-with-entry ((source git-rewound) directory octets function &key cancel)
  "The file as the commit left it, read from git into a file in memory."
  (declare (ignore cancel))
  (let* ((top (git-rewound-top source))
         (path (concatenate 'string directory (tuatara.text:octets-string octets))))
    (multiple-value-bind (blob ok too-large)
        (git-blob-octets top (git-rewound-hash source) (subseq path (length top)) :limit *git-shown-limit*)
      (cond (too-large (refuse "~a is larger than ~a" (tuatara.io:native-file-name path)
                               (tuatara.io:octets-words *git-shown-limit*)))
            ((not ok) (refuse "~a could not be read from git" (tuatara.io:native-file-name path)))
            (t (let ((fd (tuatara.io:memory-file (tuatara.io:native-file-name path) blob :kept nil)))
                 (unwind-protect (funcall function fd path (length blob))
                   (sb-posix:close fd))))))))

(defun rimu-rewind (list top)
  "Go through the history of the directory the rimu LIST lists, in the
repository at TOP: in a new rimu list shown at once in its place, the directory
as its latest commit left it, point on the entry it was on (\\[git-rewind])."
  (let* ((directory (tuatara.buffer:buffer-local list 'rimu-directory))
         (dir (subseq directory (length top)))
         (entry (list-entry-at list (tuatara.buffer:buffer-point list)))
         (point-on (and entry (tuatara.io:listed-octets entry)))
         (buffer (rimu-make-list directory :name (format nil "*git rewind: ~a*" (rimu-buffer-name directory))
                                           :like list)))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-rewound) (make-git-rewound top)
          (tuatara.buffer:buffer-local buffer :git-top) top
          (buffer-override-keymap buffer) *git-rewind-keymap*)
    (show-buffer buffer)
    (rimu-show-layout buffer (rimu-reading-layout directory "Reading its history...") nil :fresh t)
    (start-git-job "Reading the directory's history"
                   (lambda (job)
                     (let ((path (if (string= dir "") "." dir)))
                       (coerce (parse-git-commits (git-job-output job top (list "log" "--date=short" *git-commit-format*
                                                                                "--" path))
                                                  :path path)
                               'simple-vector)))
                   (lambda (revisions)
                     (if (zerop (length revisions))
                         (progn (rimu-rewind-leave buffer)
                                (message "~a has no history in git" (abbreviated-path directory)))
                         (progn
                           (setf (tuatara.buffer:buffer-local buffer 'git-rewind-revisions) revisions)
                           (rimu-rewind-show buffer 0 point-on))))
                   :quiet t :buffer buffer)))

(defun rimu-rewind-show (buffer index &optional point-on)
  "List in BUFFER, a rimu list of a directory as a commit left it, the directory as
the commit INDEX of its history (0 the latest) left it, point kept on its
entry, or on the one named by the octets POINT-ON."
  (let ((revision (svref (tuatara.buffer:buffer-local buffer 'git-rewind-revisions) index)))
    (setf (tuatara.buffer:buffer-local buffer 'git-rewind-wanted) index
          (git-rewound-hash (rimu-source buffer)) (git-commit-hash revision))
    (rimu-update buffer :reread t :point-on point-on :wait nil)))

(defmethod rimu-source-shown ((source git-rewound) buffer)
  "Once BUFFER shows what was read for the commit last asked for: make that
the one shown, and say which it is, when it was not already."
  (let ((wanted (tuatara.buffer:buffer-local buffer 'git-rewind-wanted)))
    (when (and wanted (not (eql wanted (tuatara.buffer:buffer-local buffer 'git-rewind-at))))
      (setf (tuatara.buffer:buffer-local buffer 'git-rewind-at) wanted)
      (message "~a" (git-rewind-words buffer wanted)))))

(defmethod rimu-source-open ((source git-rewound) buffer entry)
  "A directory listed in its place as the commit left it, a file gone through
from that commit back (GIT-REWIND-FILE), \\[quit-window] coming back to the list."
  (let ((top (git-rewound-top source)))
    (let ((path (rimu-path (tuatara.buffer:buffer-local buffer 'rimu-directory) entry)))
      (if (rimu-directory-p entry)
          (rimu-enter buffer (directory-name path :clean t))
          (git-rewind-file top (subseq path (length top)) :at (git-rewound-hash source) :list buffer)))))

(defun rimu-rewind-leave (buffer)
  "End BUFFER, a rimu list of a directory as a commit left it, and the histories
of files opened from it: list the directory as it is now, point on the entry it
was on (RIMU-SHOW-NEAREST)."
  (let* ((directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (entry (list-entry-at buffer (tuatara.buffer:buffer-point buffer)))
         (on (and entry (tuatara.io:listed-octets entry))))
    (dolist (opened (copy-list tuatara.buffer:*buffers*))
      (when (eq (tuatara.buffer:buffer-local opened 'git-rewind-list) buffer)
        (tuatara.buffer:kill-buffer opened)))
    (tuatara.buffer:kill-buffer buffer)
    (rimu-show-nearest directory :point-on on)))

(defmethod rimu-source-refusal-words ((source git-rewound) command)
  "COMMAND works on the files as they are now, which \\[git-rewind] goes back to."
  (format nil "~(~a~) works on the files as they are now: ~a goes back to them" command
          (substitute-command-keys "\\[git-rewind]")))

(defun install-rimu-git ()
  "Make the git column, kept for a list read again while the same commit is
checked out and the same staged, and bind G to show or hide it; refuse in a
list of a directory not on the disk the column and git's commands on the
files as they are now."
  (rimu-set-column :git 'rimu-git-cells :place :after :shown 'rimu-git-p :keep-while 'rimu-git-state
                   :documentation "The entry's last commit: when, and the first line of its message.")
  (define-key *rimu-keymap* "G" 'rimu-toggle-git)
  (dolist (command '(rimu-toggle-git git-file-menu git-blame git-log-file git-diff-file))
    (pushnew command *rimu-disk-commands*)))
