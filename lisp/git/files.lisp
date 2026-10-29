;;;; lisp/git/files.lisp --- One file in git: who changed each line, and a menu for it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x git-blame lists a file a line each with the commit that last changed
;;;; it, its author and date, as git blame says; RET shows that commit.
;;;; M-x git-file-menu acts on the file you are in (the entry on the line in
;;;; rimu, the one on point's line of a status), as Magit's file menu does:
;;;; stage it, unstage it, its hunk at point, its changes, its log, its lines'
;;;; commits, its history, put it back, rename or delete it.

(in-package #:tuatara.defaults)

;;; Who changed each line

(define-major-mode git-blame-mode special-mode
  "A file a line each (\\[git-blame]): the commit that last changed the line,
its author and date, then the line. \\<git-blame-mode>\\[git-blame-show] shows the commit on the line; \\[quit-window] closes."
  :lighter "Git blame" :interactive nil)

(defun parse-git-blame (text)
  "TEXT, what git blame --porcelain printed, as a list of (HASH AUTHOR DATE
LINE), a line of the file each, HASH NIL for one not committed yet, DATE the
author's day as YYYY-MM-DD, worked out once for each commit."
  (let ((commits (make-hash-table :test 'equal)) (rows '()) (hash nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (cond ((and (plusp (length line)) (char= (char line 0) #\Tab))
             (let ((commit (gethash hash commits)))
               (destructuring-bind (&optional author time date) commit
                 (unless (or date (null time))
                   (setf date (tuatara.dates:format-time "%Y-%m-%d" (+ time +unix-epoch+))
                         (third commit) date))
                 (push (list (and (notevery (lambda (c) (char= c #\0)) hash) (subseq hash 0 (min 8 (length hash))))
                             (or author "")
                             (or date "")
                             (subseq line 1))
                       rows))))
            ((uiop:string-prefix-p "author " line)
             (setf (first (gethash hash commits)) (subseq line 7)))
            ((uiop:string-prefix-p "author-time " line)
             (setf (second (gethash hash commits)) (parse-integer line :start 12 :junk-allowed t)))
            ((let ((space (position #\Space line)))
               (and space (git-hash-p (subseq line 0 space))))
             (setf hash (subseq line 0 (position #\Space line)))
             (unless (gethash hash commits) (setf (gethash hash commits) (list nil nil nil))))))
    (nreverse rows)))

(defparameter *git-blame-columns* '(("Commit" :width 8 :elide nil) ("Author" :width 16) ("Date" :width 10) ("Line"))
  "The columns of a file's blame, as TABLE-HEADING-PIECES takes them.")

(define-command git-blame ()
  "List the current file (as saved), or the one on point's line of a status,
a line each with the commit that last changed it, its author and date; RET
on a line shows its commit."
  (with-git-repository (top)
    (call-with-git-file top "No file here to blame"
                        (lambda (path)
                          (git-show-table "Reading who changed each line" top (list "blame" "--porcelain" "--" path)
                                          (format nil "*git blame: ~a*" (tuatara.io:native-file-name path)) 'git-blame-mode
                                          #'parse-git-blame *git-blame-columns*
                                          (lambda (row)
                                            (destructuring-bind (hash author date line) row
                                              (list (or hash "--------") (escape-controls author) date
                                                    (escape-controls (substitute #\Space #\Tab line))))))))))

(define-command git-blame-show ()
  "Show the commit that last changed the line of the blame point is on."
  (let ((row (list-entry-at)))
    (cond ((null row) (message "No line here"))
          ((null (first row)) (message "Not committed yet"))
          (t (git-show-commit (git-here) (first row))))))

(commands-for-mode 'git-blame-mode "a file's blame (\\[git-blame])" 'git-blame-show)

;;; The file at hand

(defun git-act-on-file (doing arguments done)
  "Run git with ARGUMENTS and then the path of the file at hand (GIT-FILE-HERE)
for its repository, DOING said meanwhile and DONE, a format given the path, once done."
  (with-git-repository (top)
    (call-with-git-file top "No file here"
                        (lambda (path)
                          (git-run doing top (append arguments (list "--" path)) (format nil done (escape-controls path)))))))

(define-command git-stage-file ()
  "Stage the file you are in (as saved), or the one on point's line of a status."
  (git-act-on-file "Staging" (list "add") "Staged ~a"))

(define-command git-unstage-file ()
  "Unstage the file you are in, or the one on point's line of a status, in a
repository with no commit yet too."
  (with-git-repository (top)
    (call-with-git-file top "No file here"
                        (lambda (path)
                          (git-unstage-paths top (list path) (format nil "Unstaged ~a" (escape-controls path)))))))

(define-command git-diff-file ()
  "Show the changes of the file you are in not yet committed, staged or not."
  (with-git-repository (top switches)
    (call-with-git-file top "No file here"
                        (lambda (path)
                          (git-show-diff top (format nil "*git diff ~a*" (tuatara.io:native-file-name path)) (list switches (list path))
                                         :against-head t)))))

(define-command git-delete-file ()
  "Delete the file you are in, or the one on point's line of a status, from
the files and from git, after asking; the deletion is staged."
  (with-git-repository (top)
    (call-with-git-file top "No file here"
                        (lambda (path)
                          (when (yes-or-no-p* (format nil "Delete ~a from git and the files? Its changes not committed are lost" (escape-controls path)))
                            (git-run "Deleting a file" top (list "rm" "-f" "--" path) (format nil "Deleted ~a" (escape-controls path))
                                     :then (lambda (text)
                                             (declare (ignore text))
                                             (paths-changed (list (concatenate 'string top path))))))))))

(define-command git-rename-file ()
  "Rename the file you are in, or the one on point's line of a status, to a
path asked for within the repository (into a folder there, when it names
one); the rename is staged, and the buffers visiting the file follow it."
  (with-git-repository (top)
    (call-with-git-file top "No file here"
                        (lambda (path)
                          (let ((to (string-trim '(#\Space) (read-string (format nil "Rename ~a to: " (escape-controls path)) :initial path))))
                            (if (git-inside-path-p to)
                                (let* ((from (concatenate 'string top path))
                                       (there (concatenate 'string top to))
                                       (there (if (eq :directory (tuatara.io:path-kind there))
                                                  (tuatara.io:path-join (tuatara.io:slash-ended there) (tuatara.io:native-file-name from))
                                                  there)))
                                  (handler-case
                                      (progn
                                        (check-not-saving (list from))
                                        (git-run "Renaming a file" top (list "mv" "--" path to)
                                                 (format nil "Renamed ~a to ~a" (escape-controls path) (escape-controls to))
                                                 :then (lambda (text)
                                                         (declare (ignore text))
                                                         (rename-path-following from there :on-disk nil))))
                                    (error (e) (message "Not renamed: ~a" e))))
                                (message "Not a path within the repository: ~a" (escape-controls to))))))))

(define-menu git-file-menu (:title "This file" :show git-menu-shows-p :note git-menu-note)
  "Open the menu of the file you are in (or the one on point's line of a
status): stage or unstage it or its hunk at point, show its changes, its log
or who changed each line, go back through its history, put it back to a
revision, rename or delete it.
\\`C-h' there lists every action."
  ("Stage"
   ("s" "Stage it" git-stage-file)
   ("u" "Unstage it" git-unstage-file)
   ("S" "Stage the hunk at point" git-stage-hunk)
   ("U" "Unstage the hunk at point" git-unstage-hunk))
  ("Look"
   ("d" "Its changes" git-diff-file)
   ("L" "Its log" git-log-file)
   ("b" "Who changed each line" git-blame)
   ("r" "Rewind through its history" git-rewind))
  ("Change"
   ("f" "Put back to a revision" git-reset-file)
   ("R" "Rename" git-rename-file)
   ("D" "Delete" git-delete-file)))

(defun install-git-files ()
  "Bind RET, l and h in a blame, and refuse the commands that change the file
at hand, or what git keeps of it, in a buffer opened safely."
  (dolist (command '(git-stage-file git-unstage-file git-delete-file git-rename-file git-reset-file))
    (pushnew command *file-changing-commands*))
  (define-keys (mode-keymap 'git-blame-mode) '(("RET" git-blame-show) ("l" git-blame-show) ("h" quit-window))))
