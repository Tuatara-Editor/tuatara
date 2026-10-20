;;;; lisp/git/commits.lisp --- Commits: written, amended, reworded, extended, fixed up, squashed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status c opens the commit menu, as Magit's does: c writes a commit
;;;; of what is staged in a buffer of its own (C-c C-c commits, C-c C-k gives
;;;; it up), a amends the last commit with what is staged and its message
;;;; written again, w rewords it alone, e extends it with what is staged as it
;;;; is, f and s make a commit fixing up or squashed into one asked for, for a
;;;; rebase to fold in later. Its switches go to git commit: -a every change of
;;;; a file git tracks, -e an empty commit, -s a Signed-off-by line, -n no
;;;; hooks, -R you as its author again, =A another author.

(in-package #:tuatara.defaults)

(defun git-staged-files (job top)
  "On JOB's thread: the files staged at TOP, each (PATH KIND), as git diff --cached --name-status lists them."
  (loop for line in (git-lines job top (list "diff" "--cached" "--name-status" "--no-renames" "-z"))
        append (let ((fields (uiop:split-string line :separator (list (code-char 0)))))
                 (loop for (code path) on fields by #'cddr
                       when (and path (plusp (length code)))
                         collect (list path (git-change-word (char code 0)))))))

(defun git-last-message (job top)
  "On JOB's thread: the message of the last commit at TOP, or NIL when there is none."
  (multiple-value-bind (text ok) (git-job-try job top (list "log" "-1" "--format=%B"))
    (and ok (string-right-trim '(#\Newline) text))))

(defun git-merge-message (job top)
  "On JOB's thread: the message git has made ready for a merge, or a
cherry-pick or revert, stopped half way at TOP, its comments left out; NIL
when there is none, or it is no plain file of a message's size."
  (let ((file (concatenate 'string (git-folder job top) "MERGE_MSG")))
    ;; a file of its own, never a link, a pipe or a device, and of a message's size
    (when (eq (tuatara.io:path-kind file :follow nil) :regular)
      (let ((text (tuatara.io:file-text file :limit (* 1024 1024) :too-large (constantly nil) :missing nil)))
        (when text
          (git-uncommented-lines (string-right-trim '(#\Newline) text)))))))

(defun git-commit-text (message staged doing)
  "The text a commit's message is written in: MESSAGE (empty for a new one),
then comments saying how it is committed, that DOING it (Committing what is
staged) is what C-c C-c does, and the files STAGED, each (PATH KIND)."
  (format nil "~a~%~%# Write the message above; lines beginning with # are left out.~%~
               # C-c C-c: ~a. C-c C-k gives it up.~%#~%~:[# Nothing staged.~%~;# Staged:~%~:*~{#   ~{~10a ~a~}~%~}~]"
          message doing
          (mapcar (lambda (f) (list (second f) (escape-controls (first f)))) staged)))

(defun git-write-commit (top message staged doing arguments)
  "Show a buffer to write the message of a commit of the repository at TOP
in, MESSAGE written in it to begin with, the files STAGED listed, DOING said
of C-c C-c; the commit then takes ARGUMENTS too. A message begun already for
the repository is shown as it is, with the arguments it was begun with."
  (let ((buffer (git-buffer-of top 'git-commit-mode)))
    (if buffer
        (progn (message "A commit's message is begun already: C-c C-c commits it, C-c C-k gives it up")
               (pop-to-buffer buffer :other t))
        (git-edit-buffer "*git commit: ~a*" top 'git-commit-mode (git-commit-text message staged doing) 0
                         'git-commit-arguments arguments))))

(defun git-begin-commit (top doing arguments &key amend)
  "Begin writing a commit of the repository at TOP, DOING said of C-c C-c,
with ARGUMENTS for git commit: the files staged read in the background, and
with AMEND the last commit's message, else the one git made ready for a
merge stopped half way, written in the buffer to begin with; a new commit
with nothing staged, and nothing in ARGUMENTS that commits without it, is
refused."
  (git-ask "Reading what is staged" top
           (lambda (job top) (values (git-staged-files job top) (if amend (git-last-message job top) (git-merge-message job top))))
           (lambda (staged last)
             (cond ((and amend (null last)) (message "No commit yet to amend"))
                   ((and (not amend) (null staged)
                         (notany (lambda (a) (member a '("--all" "--allow-empty") :test #'string=)) arguments))
                    (message "Nothing staged: s stages a file, -a in the commit menu commits every change"))
                   (t (git-write-commit top (or last "") staged doing arguments))))))

(define-command git-commit ()
  "Write a commit of what is staged: its message in a buffer of its own, what
is staged listed under it as comments; \\<git-commit-mode>\\[git-commit-finish] commits it, \\[git-commit-cancel] gives it up.
The commit menu's switches go with it."
  (with-git-repository (top switches)
    (git-begin-commit top "commit what is staged" switches)))

(define-command git-commit-amend ()
  "Amend the last commit: what is staged goes into it, and its message,
written in a buffer to begin with, is written again; \\<git-commit-mode>\\[git-commit-finish] amends it."
  (with-git-repository (top switches)
    (git-begin-commit top "amend the last commit with this message and what is staged" (cons "--amend" switches) :amend t)))

(define-command git-commit-reword ()
  "Reword the last commit: its message, written in a buffer to begin with, is
written again, nothing staged going into it; \\<git-commit-mode>\\[git-commit-finish] rewords it."
  (with-git-repository (top switches)
    (git-begin-commit top "reword the last commit, nothing staged going into it" (list* "--amend" "--only" switches) :amend t)))

(define-command git-commit-extend ()
  "Extend the last commit with what is staged, its message as it is."
  (with-git-repository (top switches)
    (git-run "Extending the last commit" top (list* "commit" "--amend" "--no-edit" switches) "Extended the last commit")))

(define-command git-commit-fixup ()
  "Make a commit of what is staged fixing up one asked for: a rebase with
autosquash folds it into that one, its message left as it was."
  (with-git-repository (top switches)
    (git-ask-revision "Fix up commit: " top
                      (lambda (hash)
                        (git-run "Committing a fixup" top (list* "commit" (format nil "--fixup=~a" hash) switches)
                                 (format nil "Committed a fixup of ~a" hash)))
                      :commits-only t)))

(define-command git-commit-squash ()
  "Make a commit of what is staged to be squashed into one asked for: a
rebase with autosquash folds it in, its message added to that one's."
  (with-git-repository (top switches)
    (git-ask-revision "Squash into commit: " top
                      (lambda (hash)
                        (git-run "Committing a squash" top (list* "commit" (format nil "--squash=~a" hash) "--no-edit" switches)
                                 (format nil "Committed a squash into ~a" hash)))
                      :commits-only t)))

(define-command git-commit-finish ()
  "Commit with the message of this buffer (lines beginning with # left out),
as git commit does with your settings (a signature among them) and the
arguments the commit was begun with (--amend for an amended one), and close it."
  (let* ((buffer (current-buffer))
         (top (git-here))
         (message (git-uncommented-lines (tuatara.buffer:buffer-string buffer))))
    (cond ((null top) (message "Not a commit's message: save it, and git takes it"))
          ((zerop (length (string-trim '(#\Space #\Newline #\Tab) message))) (message "No message: not committed"))
          (t (git-run "Committing" top (list* "commit" "--cleanup=strip" "-F" "-" (tuatara.buffer:buffer-local buffer 'git-commit-arguments))
                      (lambda (text)
                        (let ((first (first-line text)))
                          (if (plusp (length first)) first "Committed")))
                      :input message
                      :then (lambda (text)
                              (declare (ignore text))
                              (when (live-buffer-p buffer) (tuatara.buffer:kill-buffer buffer))))))))

(define-command git-commit-cancel ()
  "Give up this commit's message: nothing is committed."
  (git-give-up 'git-top "Not committed" "Not a commit's message written here"))

(define-menu git-commit-menu (:title "Commit" :show git-menu-shows-p :note git-menu-note)
  "Open the commit menu: write a commit, amend, reword or extend the last
one, or make one fixing up or squashed into another; its switches go to git
commit. \\`C-h' there lists every action."
  ("Make"
   ("c" "Commit" git-commit))
  ("Change the last one"
   ("a" "Amend" git-commit-amend)
   ("w" "Reword" git-commit-reword)
   ("e" "Extend" git-commit-extend))
  ("For a rebase to fold in"
   ("f" "Fix up" git-commit-fixup)
   ("s" "Squash" git-commit-squash))
  ("Switches"
   ("- a" "Every change" :switch "--all")
   ("- e" "Empty" :switch "--allow-empty")
   ("- s" "Sign off" :switch "--signoff")
   ("- n" "No hooks" :switch "--no-verify")
   ("- R" "Me as author" :switch "--reset-author")
   ("= A" "Author" :option "--author=")))

(commands-for-mode 'git-commit-mode "a commit's message" 'git-commit-finish 'git-commit-cancel)

(defun install-git-commits ()
  "Bind the commit menu to c in a status, and C-c C-c and C-c C-k in a commit's message."
  (define-keys *git-status-keymap* '(("c" git-commit-menu)))
  (define-keys (mode-keymap 'git-commit-mode) '(("C-c C-c" git-commit-finish) ("C-c C-k" git-commit-cancel))))
