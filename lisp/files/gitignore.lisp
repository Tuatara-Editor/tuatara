;;;; lisp/files/gitignore.lisp --- What .gitignore files and git's own excludes leave out.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The rules of the .gitignore files on the way to a directory, of the
;;;; repository's .git/info/exclude and of your excludes file, read as text:
;;;; git's configuration is only read, never run.

(in-package #:tuatara.defaults)

(defun gitignore-rule (line)
  "The rule a line of a .gitignore file makes, as a list (GLOB
DIRECTORIES-ONLY NEGATED ANCHORED), GLOB read by GLOB-MATCH, or NIL for a blank line or a comment: a
pattern ending with / matches directories only, one beginning with ! takes
back what the lines before it left out, and one with a / before its end is of
paths from the .gitignore's directory rather than of names at any depth.
Spaces at the end are dropped, but one after a backslash, as git reads them."
  (let ((text (subseq line 0 (loop with end = (length line)
                                   while (and (plusp end) (char= (char line (1- end)) #\Space)
                                              ;; an odd run of backslashes before the space escapes it
                                              (evenp (loop for i downfrom (- end 2) to 0
                                                           while (char= (char line i) #\\) count t)))
                                   do (decf end)
                                   finally (return end)))))
    (unless (or (zerop (length text)) (char= (char text 0) #\#))
      (let* ((negated (char= (char text 0) #\!))
             (text (if negated (subseq text 1) text))
             (directories-only (and (plusp (length text)) (char= (char text (1- (length text))) #\/)))
             (text (string-right-trim "/" text))
             (anchored (find #\/ text))
             (text (if (and anchored (plusp (length text)) (char= (char text 0) #\/)) (subseq text 1) text)))
        (when (plusp (length text))
          (list text directories-only negated anchored))))))

(defparameter *gitignore-most-octets* (* 1024 1024)
  "The largest file of git's patterns (a .gitignore, an excludes file, a git
configuration) read: a larger one, as a tree from elsewhere may hold, is taken
as having no rules, said once.")

(defparameter *gitignore-most-rules* 10000
  "The most rules of one file of git's patterns kept: the rest are dropped,
which is said once, for each rule is tried on every name of a walk.")

(defparameter *gitignore-longest-line* 4096
  "The longest line of a .gitignore whose pattern is kept: a longer one is
passed over, for each pattern is tried on every name of a walk.")

(defvar *gitignore-too-large-said* (make-hash-table :test 'equal :synchronized t)
  "The files of git's patterns said to be too large to read (GITIGNORE-OCTETS)
or to hold too many rules (GITIGNORE-RULES), each said once.")

(defun gitignore-octets (path)
  "The octets of the file of git's patterns at PATH, read as text
\(READ-TEXT-FILE); NIL when it cannot be read, or is larger than
*GITIGNORE-MOST-OCTETS*, which is said once."
  (multiple-value-bind (octets why) (read-text-file path :most *gitignore-most-octets*)
    (when (and (eq why :too-large) (not (shiftf (gethash path *gitignore-too-large-said*) t)))
      (message "~a is larger than ~a: its patterns are not read (*gitignore-most-octets*)"
               (abbreviated-path path) (tuatara.io:octets-words *gitignore-most-octets*)))
    octets))

(defun gitignore-rules (path base)
  "The rules of the file of git's patterns at PATH (a .gitignore), for the
paths under the directory BASE (a native name ending with a slash), each
\(RULE . BASE), lines longer than *GITIGNORE-LONGEST-LINE* passed over, and
those after the first *GITIGNORE-MOST-RULES* rules dropped, which is said
once; NIL when it cannot be read or is too large (GITIGNORE-OCTETS)."
  (let ((octets (gitignore-octets path)) (rules '()) (count 0))
    (when octets
      (dolist (line (uiop:split-string (tuatara.text:lenient-string octets) :separator '(#\Newline #\Return)))
        (let ((rule (and (<= (length line) *gitignore-longest-line*) (gitignore-rule line))))
          (when rule
            (when (>= count *gitignore-most-rules*)
              (unless (shiftf (gethash path *gitignore-too-large-said*) t)
                (message "~a holds more than ~:d rules: the rest are not read (*gitignore-most-rules*)"
                         (abbreviated-path path) *gitignore-most-rules*))
              (return))
            (incf count)
            (push (cons rule base) rules))))
      (nreverse rules))))

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

(defun git-config-value (octets section key)
  "The value KEY has in SECTION of a git configuration file whose octets are
OCTETS, both names in any case, the last one given counting; NIL when it has
none. What follows a ; or a # outside double quotes is a comment. Lines are
only read, includes not followed."
  (let ((in nil) (value nil))
    (dolist (line (uiop:split-string (tuatara.text:lenient-string octets) :separator '(#\Newline #\Return)) value)
      (let* ((text (string-trim '(#\Space #\Tab)
                                (subseq line 0 (loop with quoted = nil and i = 0
                                                     while (< i (length line))
                                                     do (let ((c (char line i)))
                                                          (cond ((char= c #\\) (incf i))
                                                                ((char= c #\") (setf quoted (not quoted)))
                                                                ((and (not quoted) (find c ";#")) (return i))))
                                                        (incf i)
                                                     finally (return (length line))))))
             (equals (position #\= text)))
        (cond ((and (plusp (length text)) (char= (char text 0) #\[))
               (setf in (string-equal section (string-trim "[] " text))))
              ((and in equals (string-equal key (string-trim '(#\Space #\Tab) (subseq text 0 equals))))
               (setf value (string-trim '(#\Space #\Tab #\") (subseq text (1+ equals))))))))))

(defun git-excludes-file ()
  "The native name of the file *GIT-EXCLUDES-FILE* says, or NIL. Git's
configuration is only read, never run: core.excludesFile of its [core] section,
~/.gitconfig counting over $XDG_CONFIG_HOME/git/config as it does for git."
  (let ((setting *git-excludes-file*))
    (cond ((null setting) nil)
          ((not (eq setting :git)) (tuatara.io:native (tuatara.io:expand-path setting)))
          (t
           (let* ((xdg (tuatara.io:slash-ended (tuatara.io:native (tuatara.io:config-home))))
                  (named nil))
             (dolist (file (list (concatenate 'string xdg "git/config")
                                 (concatenate 'string (directory-name "~/") ".gitconfig")))
               (let ((octets (gitignore-octets file)))
                 (when octets
                   (setf named (or (git-config-value octets "core" "excludesfile") named)))))
             (tuatara.io:native (tuatara.io:expand-path (or named (concatenate 'string xdg "git/ignore")))))))))

(defun outer-gitignore-rules (root)
  "The rules git applies to the files under ROOT from outside it: when ROOT is
in a repository, those of your excludes file (GIT-EXCLUDES-FILE), of the
repository's .git/info/exclude and of the .gitignore files of the directories
from the repository's top down to ROOT's parent, the outer ones first."
  (let ((top (tuatara.git:repository-top root)) (root (directory-name root)))
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
