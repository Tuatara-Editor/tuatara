;;;; lisp/git/log.lisp --- The log and the reflog, and differences, each from a menu of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status L opens the log menu, as Magit's l does: L the branch
;;;; checked out, o another branch or revision, b every branch, a every
;;;; reference, f a file's history, r the reflog; its switches draw the graph
;;;; or leave merges out, its options limit how many, by whom, saying what,
;;;; changing what and when. The log lists a commit a line, RET (or l)
;;;; showing it. d opens the diff menu: every change not committed, what is
;;;; staged, what is not, against a revision, between two, or a commit.

(in-package #:tuatara.defaults)

;;; Tables of what git printed

(defun git-show-table (doing top arguments name mode parse columns fields &key title (entry #'identity) empty)
  "Run git with ARGUMENTS for the repository at TOP as a job doing DOING and
show what it printed as a table in the buffer NAME in MODE, a row a line
\(RENDER-MADE-TABLE-LIST), a filter able to try a pattern on one column: on
the job's thread PARSE, a function of the text, gives the rows, COLUMNS (as
TABLE-HEADING-PIECES takes them, or a function of the rows giving them) head
them and FIELDS, a function of a row, gives its cells (TABLE-LIST-ROWS);
ENTRY, a function of a row, what its line is about (LIST-ENTRY-AT). TITLE,
when given, is a line above it all; EMPTY is said when there are no rows."
  (start-git-job doing
                 (lambda (job)
                   (let* ((rows (funcall parse (git-job-output job top arguments)))
                          (columns (if (functionp columns) (funcall columns rows) columns)))
                     (multiple-value-bind (pieces texts first) (table-list-rows columns rows fields :key entry)
                       (values columns pieces texts first (map 'vector entry rows)))))
                 (lambda (columns pieces texts first entries)
                   (let ((buffer (buffer-in-mode name mode)))
                     (git-set-top buffer top)
                     (render-made-table-list buffer columns pieces texts first
                                             :before (and title (list (cons (format nil "~a~%" title) :heading)))
                                             :point (if title 3 2) :entries entries)
                     (show-buffer buffer)
                     (when (and empty (zerop (length entries))) (message "~a" empty))))
                 :quiet t))

;;; The log

(defun git-log-columns (commits)
  "The columns of a log of COMMITS (PARSE-GIT-COMMITS), as TABLE-HEADING-PIECES
takes them: the commit, wide enough for the graph before its hash, its date,
author and subject."
  (let ((graph (reduce #'max commits :key (lambda (commit) (length (git-commit-graph commit))) :initial-value 0)))
    (list (list "Commit" :width (+ 9 graph) :elide nil) (list "Date" :width 10) (list "Author" :width 16) (list "Subject"))))

(defun git-log-row-fields (commit)
  "The fields of the log's line for COMMIT (PARSE-GIT-COMMITS): the graph and
hash, the date, the author, and the subject after the names at the commit."
  (if (git-commit-hash commit)
      (list (concatenate 'string (git-commit-graph commit) (git-commit-short commit)) (git-commit-date commit)
            (escape-controls (git-commit-author commit))
            (format nil "~@[(~a) ~]~a" (and (plusp (length (git-commit-names commit))) (escape-controls (git-commit-names commit)))
                    (escape-controls (git-commit-subject commit))))
      (list (git-commit-graph commit) "" "" "")))

(defun git-show-log (top title arguments)
  "List the commits git log gives with ARGUMENTS for the repository at TOP in
the log buffer, TITLE at its top, a line each, in the background."
  (git-show-table "Reading the log" top (append (list "log" *git-commit-format* "--date=short" "--no-color") arguments)
                  "*git log*" 'git-log-mode #'parse-git-commits #'git-log-columns #'git-log-row-fields
                  :title title :entry (lambda (commit) (and (git-commit-hash commit) commit)) :empty "No commits"))

(defun git-log-menu-arguments (switches)
  "SWITCHES, the arguments of git log the log menu had on, with how many
commits (*GIT-LOG-LENGTH* when not given)."
  (if (find-if (lambda (a) (uiop:string-prefix-p "--max-count=" a)) switches)
      switches
      (cons (format nil "--max-count=~d" *git-log-length*) switches)))

(define-command git-log ()
  "List the commits of the branch checked out, the latest first, a line
each (*GIT-LOG-LENGTH* of them); RET shows one. The log menu's switches and
options shape it."
  (with-git-repository (top switches)
    (git-show-log top "Log of HEAD" (git-log-menu-arguments switches))))

(define-command git-log-other ()
  "List the commits of a branch or revision asked for."
  (with-git-repository (top switches)
    (git-ask-branch "Log of: " top
                    (lambda (name current)
                      (declare (ignore current))
                      (git-show-log top (format nil "Log of ~a" name) (append (git-log-menu-arguments switches) (list name "--"))))
                    :remote t :default :current)))

(define-command git-log-branches ()
  "List the commits of every branch, the repository's own and its remotes'."
  (with-git-repository (top switches)
    (git-show-log top "Log of every branch" (append (git-log-menu-arguments switches) (list "--branches" "--remotes")))))

(define-command git-log-all ()
  "List the commits of every reference: branches, remotes' branches, tags, stashes."
  (with-git-repository (top switches)
    (git-show-log top "Log of every reference" (append (git-log-menu-arguments switches) (list "--all")))))

(define-command git-log-file ()
  "List the commits that changed the file on point's line of a status, or
the current file, following it across renames."
  (with-git-repository (top switches)
    (call-with-git-file top "No file here to list the log of"
                        (lambda (path)
                          (git-show-log top (format nil "Log of ~a" (escape-controls path))
                                        (append (git-log-menu-arguments switches) (list "--follow" "--" path)))))))

(define-command git-reflog ()
  "List where HEAD has been, the latest first: each commit it was at and what
moved it there (a commit, a checkout, a reset), as git's reflog keeps it."
  (with-git-repository (top)
    (git-show-table "Reading the reflog" top (list "reflog" "show" "--no-color" "--format=%h%x00%gd%x00%gs"
                                                   (format nil "--max-count=~d" *git-log-length*))
                    "*git log*" 'git-log-mode (lambda (text) (git-records text 3))
                    '(("Commit" :width 9 :elide nil) ("Where" :width 12) ("What moved it"))
                    (lambda (row) (destructuring-bind (hash where what) row (list hash where (escape-controls what))))
                    :title "Reflog of HEAD")))

(define-command git-log-show ()
  "Show the commit on the current line of the log."
  (let ((hash (git-commit-at-point)))
    (if hash
        (git-show-commit (git-here) hash)
        (message "No commit on this line"))))

(define-menu git-log-menu (:title "Log" :show git-menu-shows-p :note git-menu-note)
  "Open the log menu: list the commits of the branch checked out, another,
every branch or reference, or a file's, or the reflog; its switches and
options shape the list. \\`C-h' there lists every action."
  ("Log of"
   ("L" "This branch" git-log)
   ("o" "Another branch or revision" git-log-other)
   ("b" "Every branch" git-log-branches)
   ("a" "Every reference" git-log-all)
   ("f" "This file" git-log-file)
   ("r" "Reflog" git-reflog))
  ("Switches"
   ("- g" "Graph" :switch "--graph")
   ("- m" "No merges" :switch "--no-merges")
   ("- 1" "First parents only" :switch "--first-parent"))
  ("Options"
   ("= n" "How many" :option "--max-count=" :read read-whole-number-string)
   ("= a" "By author" :option "--author=")
   ("= s" "Message says" :option "--grep=")
   ("= S" "Changes the count of" :option "-S")
   ("= d" "Since" :option "--since=")
   ("= u" "Until" :option "--until=")))

(defun read-whole-number-string (prompt)
  "Ask with PROMPT for a whole number, as a string; empty when none is typed."
  (let ((number (read-whole-number prompt nil :valid (constantly t) :refusal "Not a whole number.")))
    (if number (princ-to-string number) "")))

;;; Differences

(defun git-head-or-empty (job top)
  "On JOB's thread: HEAD, or the empty tree in a repository at TOP with no commit yet, to compare with."
  (if (nth-value 1 (git-job-try job top (list "rev-parse" "--verify" "-q" "HEAD")))
      "HEAD"
      (git-output-line job top (list "hash-object" "-t" "tree" "/dev/null"))))

(defun git-show-diff (top name arguments &key (empty "No changes") against-head)
  "Show what git diff gives with ARGUMENTS (and the diff menu's switches)
for the repository at TOP in the buffer NAME (GIT-SHOW-OUTPUT); with
AGAINST-HEAD, compared with HEAD (GIT-HEAD-OR-EMPTY), ARGUMENTS then the
switches and the paths after --. EMPTY is said when it gives nothing."
  (git-show-output "Reading the changes" top
                   (if against-head
                       (lambda (job)
                         (destructuring-bind (switches paths) arguments
                           (append (list "diff" "--no-color" "--no-ext-diff") switches
                                   (list (git-head-or-empty job top) "--") paths)))
                       (append (list "diff" "--no-color" "--no-ext-diff") arguments))
                   name :empty empty))

(define-command git-diff ()
  "Show every change of the repository not yet committed, staged or not, in a
buffer of its own; the diff menu's switches shape it."
  (with-git-repository (top switches)
    (git-show-diff top "*git diff*" (list switches '()) :against-head t)))

(define-command git-diff-staged ()
  "Show what is staged: the changes the next commit makes."
  (with-git-repository (top switches)
    (git-show-diff top "*git diff staged*" (append switches (list "--cached" "--")) :empty "Nothing staged")))

(define-command git-diff-unstaged ()
  "Show the changes not staged: those in the files beyond what is staged."
  (with-git-repository (top switches)
    (git-show-diff top "*git diff unstaged*" (append switches (list "--")) :empty "No changes not staged")))

(define-command git-diff-revision ()
  "Show how the files differ from a revision asked for (a branch, a tag, a commit)."
  (with-git-repository (top switches)
    (git-ask-branch "Compare the files with: " top
                    (lambda (name current)
                      (declare (ignore current))
                      (git-show-diff top (format nil "*git diff ~a*" name) (append switches (list name "--"))))
                    :remote t)))

(define-command git-diff-range ()
  "Show what changed between two revisions asked for: from the first to the second."
  (with-git-repository (top switches)
    (git-ask "Reading the branches" top #'git-branches-and-head
             (lambda (names current)
               (let ((from (git-read-name "Changes from: " names)))
                 (when from
                   (let ((to (git-read-name (format nil "Changes from ~a to: " from) names :default current)))
                     (when to
                       (git-show-diff top (format nil "*git diff ~a..~a*" from to) (append switches (list from to "--")))))))))))

(define-command git-diff-commit ()
  "Show a commit asked for (the one on the line first): what it says and its changes."
  (with-git-repository (top)
    (git-ask-revision "Show commit: " top (lambda (hash) (git-show-commit top hash)) :commits-only t)))

(define-menu git-diff-menu (:title "Diff" :show git-menu-shows-p :note git-menu-note)
  "Open the diff menu: show the changes not committed, what is staged, what
is not, the files against a revision, between two revisions, or a commit;
its switches shape them. \\`C-h' there lists every action."
  ("Show"
   ("d" "Every change not committed" git-diff)
   ("s" "What is staged" git-diff-staged)
   ("u" "What is not staged" git-diff-unstaged)
   ("r" "Against a revision" git-diff-revision)
   ("R" "Between two revisions" git-diff-range)
   ("c" "A commit" git-diff-commit))
  ("Switches"
   ("- w" "Ignore all white space" :switch "--ignore-all-space")
   ("- b" "Ignore changes of white space" :switch "--ignore-space-change")
   ("- W" "Whole functions" :switch "--function-context")
   ("- s" "Files and counts" :switch "--stat"))
  ("Options"
   ("= U" "Lines round each change" :option "--unified=" :read read-whole-number-string)))

(defun install-git-log ()
  "Bind the log menu to L and the diff menu to d in a status, and RET, l and h in a log."
  (define-keys *git-status-keymap* '(("L" git-log-menu) ("d" git-diff-menu)))
  (define-keys *git-log-keymap* '(("RET" git-log-show) ("l" git-log-show) ("h" quit-window))))
