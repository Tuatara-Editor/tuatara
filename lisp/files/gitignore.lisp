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

(defun git-config-value (octets section key)
  "The value KEY has in SECTION of a git configuration file whose octets are
OCTETS, both names in any case, the last one given counting; NIL when it has
none. Lines are only read, includes not followed."
  (let ((in nil) (value nil))
    (dolist (line (uiop:split-string (tuatara.text:lenient-string octets) :separator '(#\Newline #\Return)) value)
      (let* ((text (string-trim '(#\Space #\Tab) line))
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
           (let* ((xdg (let ((env (tuatara.io:own-variable "XDG_CONFIG_HOME")))
                         (if (and env (plusp (length env))) (directory-name env) (directory-name "~/.config/"))))
                  (named nil))
             (dolist (file (list (concatenate 'string xdg "git/config")
                                 (concatenate 'string (directory-name "~/") ".gitconfig")))
               (let ((octets (read-text-file file)))
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
