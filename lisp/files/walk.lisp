;;;; lisp/files/walk.lisp --- Walking the files under a directory as git leaves them out, and finding the directory a
;;;; project is in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; FILES-UNDER walks a directory on the thread calling it, leaving out what
;;;; the .gitignore files on the way, the repository's own excludes and yours
;;;; leave out (gitignore.lisp); MARKED-DIRECTORY finds the directory a project
;;;; is in by the files that mark its top (.git, Cargo.toml, *.sln);
;;;; MAP-PATH-PLACES goes along a path a name at a time, links followed by
;;;; hand. Searches, the questions that offer files and the language servers
;;;; use them.

(in-package #:tuatara.defaults)

;;; Files on disk

(defun backup-name-p (name)
  "True for the names of editors' backups and auto-save files: ending with ~,
or #name#."
  (or (char= (char name (1- (length name))) #\~)
      (and (> (length name) 1) (char= (char name 0) #\#) (char= (char name (1- (length name))) #\#))))

(defparameter *links-followed-most* 40
  "The most links followed on the way along one path, as the system follows no
more.")

(defun map-path-places (path function &key follow nowhere)
  "Go along PATH, a native name from the root, one name at a time, calling
FUNCTION with each place reached (a path from the root, \"/\" for the root
itself) and its stamp, a link's own (NIL when nothing is there), before a link
there is read; the first true value FUNCTION returns is returned. . and .. are
taken as the system takes them, and a link on the way is followed, its
target's names put before the rest, from the root again for a target from the
root, *LINKS-FOLLOWED-MOST* at most. Its last name is followed too with
FOLLOW. A link that cannot be read, or one past the most, calls NOWHERE, when
given, with its place, and its value is returned; else the link is taken as
the place it is. A name past the last that is there is taken as written."
  (let ((names (glob-components path))
        (directory "")
        (links 0))
    (if (null names)
        (funcall function "/" (tuatara.io:path-stamp "/" :follow nil :failure nil))
        (loop while names
              do (let ((name (pop names)))
                   (cond ((string= name "."))
                         ((string= name "..")
                          (setf directory (subseq directory 0 (or (position #\/ directory :from-end t) 0))))
                         (t (let* ((place (concatenate 'string directory "/" name))
                                   (stamp (tuatara.io:path-stamp place :follow nil :failure nil))
                                   (found (funcall function place stamp)))
                              (cond (found (return found))
                                    ((and stamp (eq (tuatara.io:stamp-kind stamp) :symlink) (or names follow))
                                     (let ((target (and (< links *links-followed-most*)
                                                        (ignoring-errors-reported ("reading the link ~a" place)
                                                          (tuatara.io:symlink-target place)))))
                                       (cond ((and target (plusp (length target)))
                                              (incf links)
                                              (setf names (append (glob-components target) names))
                                              (when (char= (char target 0) #\/) (setf directory "")))
                                             (nowhere (return (funcall nowhere place)))
                                             (t (setf directory place)))))
                                    (t (setf directory place)))))))))))

(defun marked-directory (directory markers &key below-home)
  "The nearest directory from DIRECTORY up holding a file or directory named
one of MARKERS, as a native name ending with a slash, or NIL; a marker with
* or ? in it stands for every name it matches (GLOB-MATCH: *.sln). With
BELOW-HOME, never the home directory (by any of its names), one above it or /."
  (let ((homes (and below-home (tuatara.io:home-directory-names))))
    (loop for dir = (directory-name directory) then (and (string/= dir "/") (tuatara.io:parent-path dir))
          while dir
          until (and below-home
                     (or (string= dir "/")
                         (some (lambda (home) (tuatara.io:path-within-p home dir)) homes)))
          when (let ((names :unread))
                 (some (lambda (marker)
                         (if (glob-pattern-p marker)
                             (progn
                               (when (eq names :unread)
                                 (setf names (mapcar #'first (tuatara.io:directory-entries dir))))
                               (some (lambda (name) (glob-match marker name)) names))
                             (tuatara.io:path-stamp (concatenate 'string dir marker) :follow nil :failure nil)))
                       markers))
            return dir)))

(define-setting *indexed-files-most* 100000
  "The most files of a directory whose text a search of it holds in memory, or
a directory kept read keeps (*INDEXED-DIRECTORIES*).
The files past them are searched from the disk, each as it comes, so that
none is left out; such a search is slower.
- a whole number of files: 100000 as standard."
  :values (integer 1 *))

(defun files-under (root &key (limit *indexed-files-most*) gitignore each past cancel directories hidden)
  "The regular files in the directory ROOT and its subdirectories, as a list of
\(PATH . RELATIVE), RELATIVE the path from ROOT, by name within each directory:
hidden files and directories (names starting with a dot) and backups left
out, links to directories not followed, and with GITIGNORE what git leaves
out, that is the .gitignore files there and, in a repository, those above
ROOT, the repository's .git/info/exclude and your excludes file
\(OUTER-GITIGNORE-RULES). At most LIMIT of them, *INDEXED-FILES-MOST* as
standard; a second value is true when there were more. PAST, a function, is
called with the path and the relative path of each found past LIMIT, and the
walk goes on; without it, the walk ends there. With DIRECTORIES, the
subdirectories in place of the files, each before what is in it, both its
paths ending in /; with DIRECTORIES :ALSO, both. With HIDDEN, names starting
with a dot are walked too, each directory then read from the disk. With EACH,
a function, it is called with the path and the relative path of each as it is
found, and none are listed; CANCEL, a cons, stops the walk when its car turns
true. Each directory is listed as DIRECTORY-LISTING lists it: from the index
of a directory kept read, else from the disk."
  (let ((found '()) (count 0) (more nil) (rules-of (make-hash-table :test 'equal)) (root (directory-name root)))
    (setf (gethash root rules-of) (and gitignore (outer-gitignore-rules root)))
    (flet ((give (path inside)
             (cond ((< count limit)
                    (incf count)
                    (if each
                        (funcall each path inside)
                        (push (cons path inside) found)))
                   (past (setf more t) (funcall past path inside))
                   (t (return-from files-under (values (nreverse found) t)))))
           (listing (directory)
             (let ((listed (directory-listing directory :rules gitignore :hidden hidden)))
               (when (and gitignore (index-directory-rules listed))
                 (setf (gethash directory rules-of) (append (gethash directory rules-of) (index-directory-rules listed))))
               (index-directory-entries listed))))
      (walk-directory root
                      (lambda (directory relative entry)
                        (when entry
                          (destructuring-bind (name kind &rest rest) entry
                            (declare (ignore rest))
                            (let ((path (concatenate 'string directory name))
                                  (inside (concatenate 'string relative name))
                                  (rules (gethash directory rules-of)))
                              (unless (and rules (gitignored-p rules name path (eq kind :directory)))
                                (if (eq kind :directory)
                                    (let ((path (concatenate 'string path "/")) (inside (concatenate 'string inside "/")))
                                      (when gitignore (setf (gethash path rules-of) rules))
                                      (when directories (give path inside))
                                      t)
                                    (progn (unless (eq directories t) (give path inside))
                                           nil)))))))
                      :list #'listing :cancel cancel))
    (values (nreverse found) more)))
