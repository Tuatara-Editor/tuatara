;;;; lisp/git/stash.lisp --- Stashes: changes put aside, brought back, dropped, made a branch.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status z opens the stash menu, as Magit's does: z puts every change
;;;; aside (asking for a message), i only what is staged, x every change but
;;;; keeping what is staged in place; a applies a stash, p applies and drops
;;;; it, d drops it, b makes a branch of it, v shows it. A status lists the
;;;; stashes; on one, RET shows it and the menu's actions take it first.

(in-package #:tuatara.defaults)

(defun git-stash-at-point ()
  "The stash on point's line of a status, or NIL."
  (let ((entry (and (git-here) (list-entry-at))))
    (and (eq (first entry) :stash) (second entry))))

(defun git-ask-stash (prompt top then)
  "Ask with PROMPT for a stash of the repository at TOP, offered with their
subjects (the one on point's line first, else the latest), then call THEN
with its name (stash@{0}); said so when there is none."
  (let ((at (git-stash-at-point)))
    (git-ask "Reading the stashes" top #'git-stashes
             (lambda (stashes)
               (if (null stashes)
                   (message "No stash")
                   (let* ((candidates (mapcar (lambda (s) (make-candidate (format nil "~a ~a" (first s) (escape-controls (second s)))
                                                                          :value (first s)))
                                              stashes))
                          (default (find (or at (first (first stashes))) candidates :key #'candidate-result :test #'equal))
                          (name (completing-read prompt candidates :require-match t :keep-order t
                                                                   :default (and default (candidate-string default)))))
                     (when (git-name-p name) (funcall then name))))))))

(defun git-stash-push (doing done arguments)
  "Put changes aside as a stash with a message asked for (git's own, WIP on
the branch, when none is typed), git stash push given ARGUMENTS and the
stash menu's switches; DOING is said while it works, DONE once it has."
  (with-git-repository (top switches)
    (let ((message (read-string "Stash message (none for git's own): ")))
      (git-run doing top (append (list "stash" "push") arguments switches
                                 (and (plusp (length message)) (list "-m" message)))
               done))))

(define-command git-stash ()
  "Put every change aside as a stash, staged or not, with a message asked
for; the files go back to the last commit. The stash menu's switches take
files not tracked (-u), and ignored ones (-a), too."
  (git-stash-push "Stashing" "Put the changes aside" '()))

(define-command git-stash-index ()
  "Put only what is staged aside as a stash, with a message asked for; the
changes not staged stay."
  (git-stash-push "Stashing what is staged" "Put what is staged aside" (list "--staged")))

(define-command git-stash-keep-index ()
  "Put every change aside as a stash, with a message asked for, keeping what
is staged in place in the files and staged."
  (git-stash-push "Stashing" "Put the changes aside, what is staged kept" (list "--keep-index")))

(define-command git-stash-apply ()
  "Bring the changes of a stash, asked for, back into the files, the stash
kept; git says when they meet changes of their own."
  (with-git-repository (top)
    (git-ask-stash "Apply stash: " top
                   (lambda (name) (git-run "Applying a stash" top (list "stash" "apply" name) (format nil "Applied ~a" name))))))

(define-command git-stash-pop ()
  "Bring the changes of a stash, asked for, back into the files and drop it;
one whose changes meet others is kept, said so."
  (with-git-repository (top)
    (git-ask-stash "Pop stash: " top
                   (lambda (name) (git-run "Popping a stash" top (list "stash" "pop" name) (format nil "Popped ~a" name))))))

(define-command git-stash-drop ()
  "Drop a stash, asked for, after asking: its changes are gone."
  (with-git-repository (top)
    (git-ask-stash "Drop stash: " top
                   (lambda (name)
                     (when (yes-or-no-p* (format nil "Drop ~a? It cannot be undone" name))
                       (git-run "Dropping a stash" top (list "stash" "drop" name) (format nil "Dropped ~a" name)))))))

(define-command git-stash-branch ()
  "Make a new branch, asked for, of a stash, asked for: checked out at the
commit the stash was made on, its changes brought back, the stash dropped."
  (with-git-repository (top)
    (git-ask-stash "Branch from stash: " top
                   (lambda (name)
                     (let ((branch (git-read-name (format nil "Branch from ~a: " name) '())))
                       (when branch
                         (git-run "Making a branch of a stash" top (list "stash" "branch" branch name)
                                  (format nil "On the new branch ~a, ~a brought back" branch name))))))))

(defun git-show-stash (top name)
  "Show the stash NAME of the repository at TOP: the files it changes and its changes."
  (git-show-output "Reading the stash" top (list "stash" "show" "--no-color" "--no-ext-diff" "--stat" "--patch" name)
                   (format nil "*git ~a*" name)))

(define-command git-stash-show ()
  "Show a stash, asked for: the files it changes and its changes."
  (with-git-repository (top)
    (git-ask-stash "Show stash: " top (lambda (name) (git-show-stash top name)))))

(define-menu git-stash-menu (:title "Stash" :show git-menu-shows-p :note git-menu-note)
  "Open the stash menu: put changes aside (all, only what is staged, or
keeping it), apply, pop, drop or show a stash, or make a branch of one.
\\`C-h' there lists every action."
  ("Put aside"
   ("z" "Every change" git-stash)
   ("i" "What is staged" git-stash-index)
   ("x" "Keeping what is staged" git-stash-keep-index))
  ("Bring back"
   ("a" "Apply" git-stash-apply)
   ("p" "Pop" git-stash-pop)
   ("b" "As a branch" git-stash-branch))
  ("Other"
   ("v" "Show" git-stash-show)
   ("d" "Drop" git-stash-drop))
  ("Switches"
   ("- u" "Files not tracked too" :switch "--include-untracked")
   ("- a" "Ignored files too" :switch "--all")))

(defun install-git-stash ()
  "Bind the stash menu to z in a status."
  (define-keys *git-status-keymap* '(("z" git-stash-menu))))
