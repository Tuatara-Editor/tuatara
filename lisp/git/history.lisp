;;;; lisp/git/history.lisp --- Merging, rebasing, cherry-picking, reverting and resetting.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status m opens the merge menu, R the rebase menu, A the cherry-pick
;;;; menu, _ the revert menu and X the reset menu, as Magit's do. Each that
;;;; can stop half way (files changed both sides) goes on (c), skips (s) or
;;;; stops (a) from its menu, and the status says what git is in the middle
;;;; of. Git keeps the messages it would ask to be edited as they are; what
;;;; would lose changes not committed is asked first.

(in-package #:tuatara.defaults)

(defparameter *git-keep-messages* (list "GIT_EDITOR=true")
  "What git is given where it would open an editor on a message (going on
with a rebase, a merge stopped half way): the message kept as it is.")

(defun git-commit-candidate (commit)
  "COMMIT, a GIT-COMMIT, as a candidate of a question: its short hash and
subject, its date beside them, its hash taken."
  (make-candidate (format nil "~a ~a" (git-commit-short commit) (escape-controls (git-commit-subject commit)))
                  :value (git-commit-short commit) :annotation (git-commit-date commit)))

(defun git-ask-revision (prompt top then &key commits-only)
  "Ask with PROMPT for a revision of the repository at TOP: a branch, a tag,
or one of the latest commits, offered with its subject (the commit on point's
line first, else HEAD); anything else git takes may be typed. With
COMMITS-ONLY one of the latest commits alone, as offered. Then call THEN
with it."
  (let ((at (git-commit-at-point)))
    (git-ask "Reading the revisions" top
             (lambda (job top)
               (values (and (not commits-only)
                            (git-lines job top (list "for-each-ref" "--format=%(refname:short)" "refs/heads" "refs/remotes" "refs/tags")))
                       (parse-git-commits (git-job-output job top (git-log-arguments *git-log-length*)))))
             (lambda (names commits)
               (if (and commits-only (null commits))
                   (message "No commit yet")
                   (let* ((candidates (append (mapcar #'make-candidate names) (mapcar #'git-commit-candidate commits)))
                          (default (or (and at (find at candidates :key #'candidate-result :test #'equal))
                                       (if commits-only (first candidates) (make-candidate "HEAD"))))
                          (revision (completing-read prompt (if commits-only candidates (cons (make-candidate "HEAD") candidates))
                                                     :keep-order t :require-match commits-only
                                                     :default (candidate-string default))))
                     (when (plusp (length revision))
                       (let ((name (if (find #\Space revision) (subseq revision 0 (position #\Space revision)) revision)))
                         (if (git-name-p name)
                             (funcall then name)
                             (message "Not a name git takes: ~a" (escape-controls name)))))))))))

(defun git-sequence-step (what noun step top &key ask)
  "Go on with, skip or stop (STEP :CONTINUE, :SKIP or :ABORT) what git is in
the middle of, WHAT a git command (rebase, merge, cherry-pick, revert, am)
and NOUN what it is called, at TOP; ASK, when given, is asked first."
  (when (or (null ask) (yes-or-no-p* ask))
    (git-run (ecase step (:continue "Going on") (:skip "Skipping") (:abort "Stopping"))
             top (list what (ecase step (:continue "--continue") (:skip "--skip") (:abort "--abort")))
             (ecase step
               (:continue (format nil "Went on with the ~a" noun))
               (:skip (format nil "Skipped a step of the ~a" noun))
               (:abort (format nil "Stopped the ~a: as before it began" noun)))
             :environment *git-keep-messages*)))

(defmacro define-git-sequence-commands (what noun &key (skip t))
  "Define the commands going on with, skipping a commit of (with SKIP), and
stopping the WHAT (a git command, a string) git is in the middle of, NOUN
saying what it is in their documentation."
  (flet ((name (step) (intern (format nil "GIT-~:@(~a~)-~a" what step) :tuatara.defaults)))
    `(progn
       (define-command ,(name "CONTINUE") ()
         ,(format nil "Go on with the ~a stopped half way, once the files git could not join are put right and staged." noun)
         (with-git-repository (top) (git-sequence-step ,what ,noun :continue top)))
       ,@(when skip
           `((define-command ,(name "SKIP") ()
               ,(format nil "Skip the commit the ~a stopped at and go on with the next." noun)
               (with-git-repository (top) (git-sequence-step ,what ,noun :skip top)))))
       (define-command ,(name "ABORT") ()
         ,(format nil "Stop the ~a, after asking: the branch and the files go back to where they were before it began." noun)
         (with-git-repository (top)
           (git-sequence-step ,what ,noun :abort top :ask ,(format nil "Stop the ~a? What was done for it is lost" noun)))))))

;;; Merging

(defun git-merge-with (doing done arguments)
  "Merge a branch asked for into the one checked out, git merge given
ARGUMENTS and the merge menu's switches, DOING said meanwhile and DONE, a
format given the branch, once done."
  (with-git-repository (top switches)
    (git-ask-branch "Merge: " top
                    (lambda (branch current)
                      (declare (ignore current))
                      (git-run doing top (append (list "merge") arguments switches (list branch))
                               (format nil done branch) :environment *git-keep-messages*))
                    :remote t)))

(define-command git-merge ()
  "Merge a branch, asked for, into the one checked out, committing it with
git's own message; files changed on both sides stop it half way, said so in
the status (c c commits once they are put right, m a stops it)."
  (git-merge-with "Merging" "Merged ~a" (list "--no-edit")))

(define-command git-merge-no-commit ()
  "Merge a branch, asked for, without committing: the merge is left staged,
to look at and commit (c c) or stop (m a)."
  (git-merge-with "Merging" "Merged ~a, not committed: c c commits it" (list "--no-commit" "--no-ff")))

(define-command git-merge-squash ()
  "Bring the changes of a branch, asked for, into the files and stage them as
one change, not a merge: c c commits them."
  (git-merge-with "Squashing" "~a's changes staged as one: c c commits them" (list "--squash")))

(define-git-sequence-commands "merge" "merge" :skip nil)

(define-menu git-merge-menu (:title "Merge" :show git-menu-shows-p :note git-menu-note)
  "Open the merge menu: merge a branch, committed or not, squash its changes
into one, or stop a merge stopped half way. \\`C-h' there lists every action."
  ("Merge a branch"
   ("m" "Merge" git-merge)
   ("n" "Without committing" git-merge-no-commit)
   ("s" "Squashed, staged" git-merge-squash))
  ("Stopped half way"
   ("c" "Commit it" git-merge-continue)
   ("a" "Stop the merge" git-merge-abort))
  ("Switches"
   ("- f" "Only if it can simply move on" :switch "--ff-only")
   ("- n" "Always a merge commit" :switch "--no-ff")))

;;; Rebasing

(define-command git-rebase-upstream ()
  "Rebase the branch checked out on its upstream: its own commits made again
after the upstream's."
  (with-git-repository (top switches)
    (git-run "Rebasing" top (list* "rebase" switches) "Rebased on the upstream" :environment *git-keep-messages*)))

(define-command git-rebase-onto ()
  "Rebase the branch checked out on a revision asked for: its commits since
they parted made again after it."
  (with-git-repository (top switches)
    (git-ask-revision "Rebase on: " top
                      (lambda (revision)
                        (git-run "Rebasing" top (append (list "rebase") switches (list revision))
                                 (format nil "Rebased on ~a" revision) :environment *git-keep-messages*)))))

(define-git-sequence-commands "rebase" "rebase")

(define-menu git-rebase-menu (:title "Rebase" :show git-menu-shows-p :note git-menu-note)
  "Open the rebase menu: rebase the branch on its upstream or a revision, or
go on with, skip a commit of, or stop a rebase stopped half way. \\`C-h'
there lists every action."
  ("Rebase on"
   ("u" "The upstream" git-rebase-upstream)
   ("e" "A revision asked for" git-rebase-onto))
  ("Change the commits"
   ("i" "As a list says" git-rebase-interactive))
  ("Stopped half way"
   ("c" "Go on" git-rebase-continue)
   ("s" "Skip this commit" git-rebase-skip)
   ("a" "Stop the rebase" git-rebase-abort))
  ("Switches"
   ("- A" "Put changes aside meanwhile" :switch "--autostash")
   ("- q" "Fold fixups and squashes in" :switch "--autosquash")))

;;; Rebasing as a list says

(defun shell-quoted (string)
  "STRING quoted for the shell, as one word whatever it holds."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for c across string
          do (if (char= c #\') (write-string "'\\''" out) (write-char c out)))
    (write-char #\' out)))

(defun git-sequence-editor (body)
  "The program git is told to edit its list of commits with: a shell function
of the list's path doing BODY, a shell command where $1 is that path."
  (format nil "f() { ~a; }; f" body))

(defun git-rebase-todo-lines (text)
  "The lines of a rebase's list TEXT that are not comments or empty."
  (remove-if (lambda (line) (let ((l (string-left-trim '(#\Space #\Tab) line))) (or (zerop (length l)) (char= (char l 0) #\#))))
             (uiop:split-string text :separator '(#\Newline))))

(defun git-reword-line-p (line)
  "The hash and subject of LINE of a rebase's list when it rewords a commit
\(reword or r), as two values; the # git writes before the subject, and the
# empty after an empty commit's, left out. NIL for a line whose hash git
would read as an option (beginning with -)."
  (let ((words (tuatara.text:split-words (string-trim '(#\Space #\Tab) line) '(#\Space #\Tab))))
    (when (and (member (first words) '("reword" "r") :test #'string=) (second words) (git-name-p (second words)))
      (let* ((subject (cddr words))
             (subject (if (equal (first subject) "#") (rest subject) subject))
             (subject (if (equal (last subject 2) '("#" "empty")) (butlast subject 2) subject)))
        (values (second words) (format nil "~{~a~^ ~}" subject))))))

(define-command git-rebase-interactive ()
  "Rebase the commits after a revision asked for (the commit on point's line
first) as a list says: git's own list of them is shown to change, a line a
commit, pick keeping one, reword giving it a new subject, edit stopping at
it, squash and fixup folding it into the one above, drop leaving it out,
the lines in the order wanted. \\<git-rebase-mode>\\[git-rebase-todo-finish] rebases as the list says, \\[git-rebase-todo-cancel] gives it up.
The rebase menu's switches go with it (--autosquash orders fixups first)."
  (with-git-repository (top switches)
    (git-ask-revision "Rebase the commits after: " top
                      (lambda (base)
                        (let ((folder (new-private-folder (private-cache-directory "git/")))
                              (shown nil))
                          (start-git-job "Reading the commits to rebase"
                                         (lambda (job)
                                           (let ((copy (concatenate 'string folder "todo-from-git")))
                                             ;; git writes its list, which is copied out, then the rebase
                                             ;; is given up: the editor fails, and git starts nothing
                                             (run-git top (append (list "rebase" "-i") switches (list base))
                                                      :environment (list* (format nil "GIT_SEQUENCE_EDITOR=~a"
                                                                                  (git-sequence-editor (format nil "cp \"$1\" ~a; exit 1" (shell-quoted copy))))
                                                                          *git-keep-messages*)
                                                      :cancel (tuatara.runtime:job-cancel job) :errors t)
                                             (and (eq :regular (tuatara.io:path-kind copy :follow nil))
                                                  (tuatara.io:file-text copy :limit tuatara.io:*memory-read-limit*))))
                                         (lambda (todo)
                                           (cond ((null todo) (message "git made no list of commits after ~a to rebase" base))
                                                 ((null (git-rebase-todo-lines todo)) (message "No commits after ~a to rebase" base))
                                                 (t (git-show-rebase-todo top base switches folder todo)
                                                    (setf shown t))))
                                         :after (lambda () (unless shown (delete-private-folder folder)))
                                         :quiet t))))))

(defun git-show-rebase-todo (top base switches folder todo)
  "Show git's list TODO of the commits of the repository at TOP after BASE to
change, the rebase to take SWITCHES and keep its files in FOLDER, which goes
with the list when it is killed or given up (FORGET-PRIVATE-FOLDER), and the
folder of a list it replaces with it."
  (let ((old (git-buffer-of top 'git-rebase-mode)))
    (when (and old (buffer-private-folder old))
      (delete-private-folder (buffer-private-folder old))))
  (git-edit-buffer "*git rebase: ~a*" top 'git-rebase-mode
                   (format nil "# C-c C-c rebases as these lines say, C-c C-k gives it up.~%~a" todo) 1
                   'git-rebase (list base switches folder)
                   'private-folder folder))

(define-command git-rebase-todo-finish ()
  "Rebase as this list says: each reword line asks the commit's new subject
first (the rest of its message kept), then git rebases in the background; the
list's folder, the new messages in it, is deleted once git is done."
  (let* ((buffer (current-buffer))
         (top (git-here))
         (rebase (tuatara.buffer:buffer-local buffer 'git-rebase)))
    (if (null rebase)
        (message "Not a rebase's list")
        (destructuring-bind (base switches folder) rebase
          (let* ((lines (uiop:split-string (tuatara.buffer:buffer-string buffer) :separator '(#\Newline)))
                 (rewords (loop for line in lines
                                for (hash subject) = (multiple-value-list (git-reword-line-p line))
                                when hash
                                  collect (list hash (read-string (format nil "New subject of ~a: " hash) :initial subject)))))
            ;; the folder goes when git is done, not with the list
            (setf (tuatara.buffer:buffer-local buffer 'private-folder) nil)
            (start-git-job "Rebasing"
                           (lambda (job)
                             (let ((todo (concatenate 'string folder "todo"))
                                   (n 0))
                               (tuatara.io:write-private-file
                                todo
                                (with-output-to-string (out)
                                  (dolist (line lines)
                                    (let ((hash (git-reword-line-p line)))
                                      (if (null hash)
                                          (format out "~a~%" line)
                                          (let* ((subject (second (find hash rewords :key #'first :test #'string=)))
                                                 (body (git-output-line job top (list "log" "-1" "--format=%b" "--end-of-options" hash)))
                                                 (file (format nil "~amessage-~d" folder (incf n))))
                                            (tuatara.io:write-private-file
                                             file (format nil "~a~@[~%~%~a~]~%" subject (and (plusp (length body)) body)))
                                            ;; the commit made as it was, then its message replaced, a line
                                            ;; beginning with # kept, an empty commit amended as it is
                                            (format out "pick ~a~%exec git commit --amend --only --allow-empty --cleanup=whitespace -F ~a~%"
                                                    hash (shell-quoted file))))))))
                               (git-job-output job top (append (list "rebase" "-i") switches (list base))
                                               :environment (list* (format nil "GIT_SEQUENCE_EDITOR=~a"
                                                                           (git-sequence-editor (format nil "cp ~a \"$1\"" (shell-quoted todo))))
                                                                   *git-keep-messages*))))
                           (lambda (text)
                             (when (live-buffer-p buffer) (tuatara.buffer:kill-buffer buffer))
                             (let ((stopped (search "Stopped at" text)))
                               (message (if stopped
                                            "Stopped at a commit to edit: c a amends it, R c goes on"
                                            "Rebased as the list said"))))
                           :after (lambda ()
                                    (delete-private-folder folder)
                                    (git-refresh-repository top))))))))

(define-command git-rebase-todo-cancel ()
  "Give up this rebase's list: nothing is rebased."
  (git-give-up 'git-rebase "Not rebased" "Not a rebase's list"))

;;; Cherry-picking and reverting

(defun git-apply-commit (what doing done arguments)
  "Apply a commit, asked for (the one on point's line first; a branch's name
for its newest commit, from any branch), as git WHAT (cherry-pick, revert)
does with ARGUMENTS and the menu's switches, DOING said meanwhile and DONE,
a format given the commit, once done."
  (with-git-repository (top switches)
    (git-ask-revision (format nil "~@(~a~) commit: " what) top
                      (lambda (revision)
                        (git-run doing top (append (list what) arguments switches (list revision))
                                 (format nil done revision) :environment *git-keep-messages*)))))

(define-command git-cherry-pick ()
  "Make a commit, asked for (the one on point's line of a status or a log
first, or another branch's newest by the branch's name), again on the branch
checked out, with its message."
  (git-apply-commit "cherry-pick" "Cherry-picking" "Cherry-picked ~a" '()))

(define-command git-cherry-pick-no-commit ()
  "Bring the changes of a commit, asked for, into the files and stage them, not committed."
  (git-apply-commit "cherry-pick" "Cherry-picking" "~a's changes staged: c c commits them" (list "--no-commit")))

(define-git-sequence-commands "cherry-pick" "cherry-pick")

(define-menu git-cherry-pick-menu (:title "Cherry-pick" :show git-menu-shows-p :note git-menu-note)
  "Open the cherry-pick menu: make a commit again on this branch, or bring its
changes in uncommitted; go on with, skip or stop one stopped half way.
\\`C-h' there lists every action."
  ("Pick a commit"
   ("A" "Pick" git-cherry-pick)
   ("n" "Its changes, not committed" git-cherry-pick-no-commit))
  ("Stopped half way"
   ("c" "Go on" git-cherry-pick-continue)
   ("s" "Skip this commit" git-cherry-pick-skip)
   ("a" "Stop" git-cherry-pick-abort))
  ("Switches"
   ("- x" "Say where it came from" :switch "-x")
   ("- s" "Sign off" :switch "--signoff")
   ("- f" "Simply move on when it can" :switch "--ff")))

(define-command git-revert ()
  "Make a commit undoing a commit asked for (the one on point's line of a
status or a log first), with git's message saying so."
  (git-apply-commit "revert" "Reverting" "Reverted ~a" (list "--no-edit")))

(define-command git-revert-no-commit ()
  "Undo the changes of a commit, asked for, in the files and stage that, not committed."
  (git-apply-commit "revert" "Reverting" "~a undone, staged: c c commits it" (list "--no-commit")))

(define-git-sequence-commands "revert" "revert")

(define-menu git-revert-menu (:title "Revert" :show git-menu-shows-p :note git-menu-note)
  "Open the revert menu: make a commit undoing another, or undo it in the
files uncommitted; go on with, skip or stop one stopped half way. \\`C-h'
there lists every action."
  ("Revert a commit"
   ("_" "Revert" git-revert)
   ("n" "Undone, not committed" git-revert-no-commit))
  ("Stopped half way"
   ("c" "Go on" git-revert-continue)
   ("s" "Skip this commit" git-revert-skip)
   ("a" "Stop" git-revert-abort))
  ("Switches"
   ("- s" "Sign off" :switch "--signoff")))

;;; Bisecting

(defun git-bisect-words (text)
  "What is said after a step of a bisection: git's line naming the first bad
commit, or the one saying how many are left to try, else its first line."
  (let ((lines (tuatara.text:split-words text '(#\Newline))))
    (escape-controls
     (or (find-if (lambda (l) (search "is the first" l)) lines)
         (find-if (lambda (l) (uiop:string-prefix-p "Bisecting:" l)) lines)
         (first lines)
         "Done"))))

(define-command git-bisect-start ()
  "Begin looking for the commit that brought a fault in: a revision that has
it (HEAD when nothing is typed) and one that has not are asked for, and git
checks out one between to try; B g, B b and B s then say it is good, bad, or
not to be tried."
  (with-git-repository (top)
    (git-ask-revision "Bad revision (has the fault): " top
                      (lambda (bad)
                        (git-ask-revision "Good revision (has not): " top
                                          (lambda (good)
                                            (git-run "Bisecting" top (list "bisect" "start" bad good) #'git-bisect-words)))))))

(defmacro define-git-bisect-step (name word doc)
  "Define the command NAME, documented by DOC, saying git bisect WORD of the commit checked out."
  `(define-command ,name ()
     ,doc
     (with-git-repository (top)
       (git-run "Bisecting" top (list "bisect" ,word) #'git-bisect-words))))

(define-git-bisect-step git-bisect-good "good" "Say the commit checked out has not the fault: git tries one after it.")
(define-git-bisect-step git-bisect-bad "bad" "Say the commit checked out has the fault: git tries one before it.")
(define-git-bisect-step git-bisect-skip "skip" "Say the commit checked out cannot be tried: git tries one beside it.")

(define-command git-bisect-reset ()
  "Stop looking: HEAD goes back to where it was when the bisection began."
  (with-git-repository (top)
    (git-run "Stopping the bisection" top (list "bisect" "reset") "Stopped bisecting: back where it began")))

(define-command git-bisect-run ()
  "Let a command asked for, run by the shell in the repository, say of each
commit tried whether it has the fault (it fails) or not (it succeeds, 125 for
one not to be tried), until the first bad commit is found."
  (with-git-repository (top)
    (let ((command (read-string "Command telling good (succeeds) from bad (fails): ")))
      (when (plusp (length command))
        (git-run "Bisecting" top (list "bisect" "run" "sh" "-c" command) #'git-bisect-words)))))

(define-menu git-bisect-menu (:title "Bisect" :show git-menu-shows-p :note git-menu-note)
  "Open the bisect menu: look for the commit that brought a fault in, saying
of each commit tried whether it is good or bad, or letting a command say.
\\`C-h' there lists every action."
  ("Look for the first bad commit"
   ("B" "Begin" git-bisect-start)
   ("x" "Let a command say" git-bisect-run))
  ("The commit tried"
   ("g" "Good" git-bisect-good)
   ("b" "Bad" git-bisect-bad)
   ("s" "Skip" git-bisect-skip))
  ("Done"
   ("r" "Stop" git-bisect-reset)))

;;; Resetting

(defun git-reset-to (how prompt done &key ask)
  "Move the branch checked out to a revision asked for with PROMPT, as git
reset HOW (--soft, --mixed, --keep, --hard) does, DONE a format given it,
saying once done; ASK, a format given it too, is asked first when given."
  (with-git-repository (top)
    (git-ask-revision prompt top
                      (lambda (revision)
                        (when (or (null ask) (yes-or-no-p* (format nil ask revision)))
                          (git-run "Resetting" top (list "reset" how revision "--") (format nil done revision)))))))

(define-command git-reset-mixed ()
  "Move the branch checked out to a revision asked for, what is staged put
back to it too, the files as they are (their changes shown as not staged)."
  (git-reset-to "--mixed" "Reset to: " "Reset to ~a, the files as they were"))

(define-command git-reset-soft ()
  "Move the branch checked out to a revision asked for, what is staged and
the files as they are (the commits' changes shown as staged)."
  (git-reset-to "--soft" "Reset to (soft): " "Reset to ~a, what is staged as it was"))

(define-command git-reset-keep ()
  "Move the branch checked out to a revision asked for, the files with it,
keeping changes not committed; git refuses when they would be lost."
  (git-reset-to "--keep" "Reset to (keep): " "Reset to ~a, changes not committed kept"))

(define-command git-reset-hard ()
  "Move the branch checked out to a revision asked for, what is staged and
the files with it, after asking: changes not committed are lost."
  (git-reset-to "--hard" "Reset to (hard): " "Reset to ~a, files and all"
                :ask "Reset everything to ~a? Changes not committed are lost and cannot be got back"))

(define-command git-reset-file ()
  "Put a file, the one on point's line of a status or the current one, back
to how a revision asked for has it, staged, after asking: its changes not
committed are lost."
  (with-git-repository (top)
    (call-with-git-file top "No file here to put back"
                        (lambda (path)
                          (git-ask-revision (format nil "Put ~a back to: " (escape-controls path)) top
                                            (lambda (revision)
                                              (when (yes-or-no-p* (format nil "Put ~a back to ~a? Its changes not committed are lost" (escape-controls path) revision))
                                                (git-run "Putting a file back" top (list "checkout" revision "--" path)
                                                         (format nil "Put ~a back to ~a" (escape-controls path) revision)))))))))

(commands-for-mode 'git-rebase-mode "a rebase's list" 'git-rebase-todo-finish 'git-rebase-todo-cancel)

(define-menu git-reset-menu (:title "Reset" :show git-menu-shows-p :note git-menu-note)
  "Open the reset menu: move the branch to a revision, keeping what is staged,
the files, or changes not committed, or nothing; or put a file back to a
revision. \\`C-h' there lists every action."
  ("Move the branch to a revision"
   ("m" "Keeping the files" git-reset-mixed)
   ("s" "Keeping the files and what is staged" git-reset-soft)
   ("K" "Keeping changes not committed" git-reset-keep)
   ("H" "Keeping nothing" git-reset-hard))
  ("A file"
   ("f" "Put back to a revision" git-reset-file)))

(defun install-git-history ()
  "Bind the merge menu to m, the rebase menu to R, the cherry-pick menu to A,
the revert menu to _, the reset menu to X and the bisect menu to B in a
status, and C-c C-c and C-c C-k in a rebase's list."
  (define-keys *git-status-keymap* '(("m" git-merge-menu) ("R" git-rebase-menu) ("A" git-cherry-pick-menu)
                                     ("_" git-revert-menu) ("X" git-reset-menu) ("B" git-bisect-menu)))
  (define-keys (mode-keymap 'git-rebase-mode) '(("C-c C-c" git-rebase-todo-finish) ("C-c C-k" git-rebase-todo-cancel))))
