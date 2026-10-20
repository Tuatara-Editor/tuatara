;;;; lisp/files/walk.lisp --- Walking the files under a folder as git leaves them out, and finding the folder a project is in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; FILES-UNDER walks a folder on the thread calling it, leaving out what
;;;; the .gitignore files on the way, the repository's own excludes and
;;;; yours leave out; MARKED-DIRECTORY finds the folder a project is in by
;;;; the files that mark its top (.git, Cargo.toml, *.sln). Searches, the
;;;; questions that offer files and the language servers use them.

(in-package #:tuatara.defaults)

;;; Files on disk

(defun backup-name-p (name)
  "True for the names of editors' backups and auto-save files: ending with ~,
or #name#."
  (or (char= (char name (1- (length name))) #\~)
      (and (> (length name) 1) (char= (char name 0) #\#) (char= (char name (1- (length name))) #\#))))

;;; What .gitignore files leave out

(defun gitignore-rule (line)
  "The rule a line of a .gitignore file makes, as a list (GLOB
DIRECTORIES-ONLY NEGATED ANCHORED), GLOB read by GLOB-MATCH, or NIL for a blank line or a comment: a
pattern ending with / matches directories only, one beginning with ! takes
back what the lines before it left out, and one with a / before its end is of
paths from the .gitignore's directory rather than of names at any depth."
  (let ((text (string-right-trim " " line)))
    (unless (or (zerop (length text)) (char= (char text 0) #\#))
      (let* ((negated (char= (char text 0) #\!))
             (text (if negated (subseq text 1) text))
             (directories-only (and (plusp (length text)) (char= (char text (1- (length text))) #\/)))
             (text (string-right-trim "/" text))
             (anchored (find #\/ text))
             (text (if (and anchored (plusp (length text)) (char= (char text 0) #\/)) (subseq text 1) text)))
        (when (plusp (length text))
          (list text directories-only negated anchored))))))

(defun gitignore-rules (path base)
  "The rules of the file of git's patterns at PATH (a .gitignore), for the
paths under the directory BASE (a native name ending with a slash), each
(RULE . BASE); NIL when it cannot be read."
  (let ((octets (read-text-file path)))
    (when octets
      (loop for line in (uiop:split-string (tuatara.text:lenient-string octets)
                                           :separator '(#\Newline #\Return))
            for rule = (gitignore-rule line)
            when rule collect (cons rule base)))))

(defun gitignored-p (rules name path directory)
  "True when RULES (GITIGNORE-RULES, the outer files' first) leave out the entry
NAME at the native PATH, a DIRECTORY or not: the last rule that matches it
says."
  (let ((ignored nil))
    (dolist (entry rules ignored)
      (destructuring-bind ((glob directories-only negated anchored) . base) entry
        (when (and (or directory (not directories-only))
                   (if anchored
                       (and (> (length path) (length base)) (string= base path :end2 (length base))
                            (glob-match glob path :start (length base)))
                       (glob-match glob name)))
          (setf ignored (not negated)))))))

(defvar *git-excludes-file* :git
  "The file of patterns git leaves out of every repository, which a search in
one leaves out too: :GIT for the one your git configuration names
(core.excludesFile, else ~/.config/git/ignore), a path, or NIL for none.")

(defun git-excludes-file ()
  "The native name of the file *GIT-EXCLUDES-FILE* says, or NIL. Git's
configuration is only read, never run: core.excludesFile of its [core] section,
~/.gitconfig counting over $XDG_CONFIG_HOME/git/config as it does for git."
  (let ((setting *git-excludes-file*))
    (cond ((null setting) nil)
          ((not (eq setting :git)) (tuatara.io:native (tuatara.io:expand-path setting)))
          (t
           (let* ((xdg (let ((env (uiop:getenv "XDG_CONFIG_HOME")))
                         (if (and env (plusp (length env))) (directory-name env) (directory-name "~/.config/"))))
                  (named nil))
             (dolist (file (list (concatenate 'string xdg "git/config") (concatenate 'string (directory-name "~/") ".gitconfig")))
               (let ((octets (read-text-file file)))
                 (when octets
                   (let ((section nil))
                     (dolist (line (uiop:split-string (tuatara.text:lenient-string octets)
                                                      :separator '(#\Newline #\Return)))
                       (let ((text (string-trim '(#\Space #\Tab) line)))
                         (cond ((and (plusp (length text)) (char= (char text 0) #\[))
                                (setf section (string-downcase (string-trim "[] " text))))
                               ((and (equal section "core")
                                     (let ((equals (position #\= text)))
                                       (and equals (string-equal "excludesfile" (string-trim '(#\Space #\Tab) (subseq text 0 equals))))))
                                (setf named (string-trim '(#\Space #\Tab #\") (subseq text (1+ (position #\= text)))))))))))))
             (tuatara.io:native (tuatara.io:expand-path (or named (concatenate 'string xdg "git/ignore")))))))))

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
                         (if (find-if (lambda (c) (find c "*?")) marker)
                             (progn
                               (when (eq names :unread)
                                 (setf names (mapcar #'first (tuatara.io:directory-entries dir))))
                               (some (lambda (name) (glob-match marker name)) names))
                             (ignore-errors (tuatara.io:path-stamp (concatenate 'string dir marker) :follow nil))))
                       markers))
            return dir)))

(defun outer-gitignore-rules (root)
  "The rules git applies to the files under ROOT from outside it: when ROOT is
in a repository, those of your excludes file (GIT-EXCLUDES-FILE), of the
repository's .git/info/exclude and of the .gitignore files of the directories
from the repository's top down to ROOT's parent, the outer ones first."
  (let ((top (repository-top root)) (root (directory-name root)))
    (when top
      (let ((global (git-excludes-file))
            (above '()))
        ;; the directories from the top down to ROOT's parent: ROOT's own
        ;; .gitignore is read by the walk
        (unless (string= top root)
          (push top above)
          (loop for slash = (position #\/ root :start (length top)) then (position #\/ root :start (1+ slash))
                while (and slash (< (1+ slash) (length root)))
                do (push (subseq root 0 (1+ slash)) above)))
        (append (and global (gitignore-rules global top))
                (gitignore-rules (concatenate 'string top ".git/info/exclude") top)
                (loop for dir in (nreverse above)
                      append (gitignore-rules (concatenate 'string dir ".gitignore") dir)))))))

(defun walk-directory (root function &key list breadth-first max-depth same-device follow-links cancel limit)
  "Go through the folder ROOT (a native name ending in /) and the folders
under it, ROOT first, calling FUNCTION with three arguments: on entering a
folder, the folder (a native name ending in /), its path from ROOT (\"\" for
ROOT, else ending in /) and NIL; then, for each entry of it by name, the
folder, its path from ROOT and the entry, a list as TUATARA.IO:DIRECTORY-ENTRIES
gives it with stamps: (NAME KIND SIZE OWN STAMP). A folder among the entries
is entered when FUNCTION returns true for it: depth first, just after its
entry, or with BREADTH-FIRST once every folder nearer ROOT has been gone
through. A link to a folder is entered only with FOLLOW-LINKS; with
MAX-DEPTH, no folder more than MAX-DEPTH below ROOT is; with SAME-DEVICE,
no folder of another file system than ROOT's (a drive mounted below it,
/proc), nor one that could not be examined. LIST, a function of a folder,
gives its entries in place of TUATARA.IO:DIRECTORY-ENTRIES (NIL for a folder
that cannot be read); an entry of only (NAME KIND) counts as no link. CANCEL,
a cons, ends the walk when its car turns true, looked at as each folder is
entered; LIMIT ends it once that many folders were entered. True when every
folder wanted was gone through, NIL when CANCEL or LIMIT ended it."
  (let ((device (and same-device (let ((stamp (ignore-errors (tuatara.io:path-stamp root))))
                                   (and stamp (tuatara.io:stamp-dev stamp)))))
        (entered 0)
        (waiting '()) (last nil))
    (labels ((entries (directory)
               (if list
                   (funcall list directory)
                   (ignore-errors (tuatara.io:directory-entries directory :stamps t))))
             (enter (directory relative depth)
               (when (or (and cancel (car cancel)) (and limit (>= entered limit)))
                 (return-from walk-directory nil))
               (incf entered)
               (funcall function directory relative nil)
               (dolist (entry (entries directory))
                 (destructuring-bind (name kind &optional size (own kind) stamp) entry
                   (declare (ignore size))
                   (when (and (funcall function directory relative entry)
                              (eq kind :directory)
                              (or follow-links (eq own kind))
                              (or (null max-depth) (< depth max-depth))
                              (or (not same-device) (and stamp (eql device (tuatara.io:stamp-dev stamp)))))
                     (let ((folder (list (concatenate 'string directory name "/") (concatenate 'string relative name "/") (1+ depth))))
                       (cond ((not breadth-first) (apply #'enter folder))
                             (last (setf (cdr last) (list folder) last (cdr last)))
                             (t (setf waiting (list folder) last waiting)))))))))
      (enter (directory-name root) "" 0)
      (loop while waiting
            do (let ((folder (pop waiting)))
                 (unless waiting (setf last nil))
                 (apply #'enter folder))))
    t))

(defun files-under (root &key (limit 100000) gitignore each cancel directories hidden)
  "The regular files in the directory ROOT and its subdirectories, as a list of
(PATH . RELATIVE), RELATIVE the path from ROOT, by name within each directory:
hidden files and directories (names starting with a dot) and backups left
out, links to directories not followed, and with GITIGNORE what git leaves
out: the .gitignore files there and, in a repository, those above ROOT, the
repository's .git/info/exclude and your excludes file (OUTER-GITIGNORE-RULES). At most LIMIT of them; a second
value is true when there were more. With DIRECTORIES, the subdirectories in
place of the files, each before what is in it, both its paths ending in /;
with DIRECTORIES :ALSO, both. With HIDDEN, names starting with a dot are
walked too, each directory then read from the disk.
With EACH, a function, it is called with the path and the relative path of
each as it is found, and none are listed; CANCEL, a cons, stops the walk when
its car turns true. Each directory is listed as DIRECTORY-LISTING lists it:
from the index of a directory kept read, else from the disk."
  (let ((found '()) (count 0) (rules-of (make-hash-table :test 'equal)) (root (directory-name root)))
    (setf (gethash root rules-of) (and gitignore (outer-gitignore-rules root)))
    (flet ((give (path inside)
             (when (>= count limit)
               (return-from files-under (values (nreverse found) t)))
             (incf count)
             (if each
                 (funcall each path inside)
                 (push (cons path inside) found)))
           (listing (directory)
             (declare (notinline index-directory-entries index-directory-rules))
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
    (values (nreverse found) nil)))
