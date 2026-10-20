;;;; lisp/git/references.lisp --- Tags, remotes, and a list of every branch and tag.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status t opens the tag menu and M the remote menu, as Magit's do,
;;;; and y lists every branch and tag.
;;;; A remote's address is never one of git's ext:: kind, which names a
;;;; program to run, nor anything git would read as an option.

(in-package #:tuatara.defaults)

;;; Tags

(defun git-tag-names (job top)
  "On JOB's thread: the tags of the repository at TOP, the newest first."
  (git-lines job top (list "for-each-ref" "--format=%(refname:short)" "--sort=-creatordate" "refs/tags")))

(defun git-ask-tag (prompt top then)
  "Ask with PROMPT for a tag of the repository at TOP, the newest offered
first, then call THEN with its name; said so when there is none."
  (git-ask "Reading the tags" top #'git-tag-names
           (lambda (tags)
             (if (null tags)
                 (message "No tag")
                 (let ((tag (git-read-name prompt tags :require-match t)))
                   (when tag (funcall then tag)))))))

(define-command git-tag-create ()
  "Tag a revision asked for (the commit on point's line first, else HEAD)
with a name asked for; the tag menu's -a makes it annotated with a message
asked for, -s signed (with a message too), -f moves a tag of that name."
  (with-git-repository (top switches)
    (let ((name (git-read-name "Tag name: " '())))
      (when name
        (git-ask-revision (format nil "Tag ~a at: " name) top
                          (lambda (revision)
                            (let ((message (and (intersection switches '("--annotate" "--sign") :test #'string=)
                                                (read-string (format nil "Message of ~a: " name)))))
                              (if (and message (zerop (length message)))
                                  (message "An annotated tag needs a message: not tagged")
                                  (git-run "Tagging" top (append (list "tag") switches (and message (list "-m" message)) (list name revision))
                                           (format nil "Tagged ~a as ~a" revision name))))))))))

(define-command git-tag-delete ()
  "Delete a tag, asked for, here (not on any remote), after asking."
  (with-git-repository (top)
    (git-ask-tag "Delete tag: " top
                 (lambda (tag)
                   (when (yes-or-no-p* (format nil "Delete the tag ~a?" tag))
                     (git-run "Deleting a tag" top (list "tag" "-d" tag) (format nil "Deleted the tag ~a" tag)))))))

(define-menu git-tag-menu (:title "Tag" :show git-menu-shows-p :note git-menu-note)
  "Open the tag menu: tag a revision, or delete a tag; P t and P T in the
push menu send tags. \\`C-h' there lists every action."
  ("Tags"
   ("t" "Tag a revision" git-tag-create)
   ("d" "Delete a tag" git-tag-delete))
  ("Switches"
   ("- a" "Annotated, with a message" :switch "--annotate")
   ("- s" "Signed" :switch "--sign")
   ("- f" "Move one of that name" :switch "--force")))

;;; Remotes

(defun git-address-p (address)
  "True when ADDRESS may be given to git as a remote's: not empty, not
beginning with - (an option to git), not of the ext:: kind (a program to
run), no space or control character in it."
  (and (git-name-p address)
       (not (uiop:string-prefix-p "ext::" (string-downcase address)))))

(defun git-read-address (prompt &optional (initial ""))
  "Ask with PROMPT for a remote's address (INITIAL to begin with), a folder
on this computer written from ~/ made whole; NIL, said so, when it could not
be given to git (GIT-ADDRESS-P)."
  (let* ((address (string-trim '(#\Space #\Tab) (read-string prompt :initial initial)))
         (address (if (uiop:string-prefix-p "~/" address) (tuatara.io:native (tuatara.io:expand-path address)) address)))
    (if (git-address-p address)
        address
        (progn (message "Not an address git is given: ~a" (escape-controls address)) nil))))

(define-command git-remote-add ()
  "Add a remote: a name asked for (origin when nothing is typed and there is
none) and its address; its branches are fetched with f e."
  (with-git-repository (top)
    (git-ask "Reading the remotes" top (lambda (job top) (git-lines job top (list "remote")))
             (lambda (remotes)
               (let ((name (git-read-name "Remote name: " '() :default (unless (member "origin" remotes :test #'string=) "origin"))))
                 (when name
                   (let ((address (git-read-address (format nil "Address of ~a: " name))))
                     (when address
                       (git-run "Adding a remote" top (list "remote" "add" name address)
                                (format nil "Added the remote ~a" name))))))))))

(define-command git-remote-rename ()
  "Rename a remote, asked for, to a name asked for."
  (with-git-repository (top)
    (git-ask-remote "Rename remote: " top
                    (lambda (old)
                      (let ((new (git-read-name (format nil "Rename ~a to: " old) '())))
                        (when new
                          (git-run "Renaming a remote" top (list "remote" "rename" old new) (format nil "Renamed ~a to ~a" old new))))))))

(define-command git-remote-set-address ()
  "Give a remote, asked for, a new address, its own shown to begin with."
  (with-git-repository (top)
    (git-ask-remote "Address of remote: " top
                    (lambda (remote)
                      (git-ask "Reading the address" top
                               (lambda (job top) (git-output-line job top (list "remote" "get-url" remote)))
                               (lambda (old)
                                 (let ((address (git-read-address (format nil "Address of ~a: " remote) old)))
                                   (when address
                                     (git-run "Setting the address" top (list "remote" "set-url" remote address)
                                              (format nil "~a is at ~a now" remote (escape-controls address)))))))))))

(define-command git-remote-remove ()
  "Remove a remote, asked for, after asking: its branches here go with it."
  (with-git-repository (top)
    (git-ask-remote "Remove remote: " top
                    (lambda (remote)
                      (when (yes-or-no-p* (format nil "Remove the remote ~a and its branches here?" remote))
                        (git-run "Removing a remote" top (list "remote" "remove" remote) (format nil "Removed the remote ~a" remote)))))))

(define-command git-remote-prune ()
  "Forget the branches of a remote, asked for, that it no longer has."
  (with-git-repository (top)
    (git-ask-remote "Prune remote: " top
                    (lambda (remote)
                      (git-over-network "Pruning" top (list "remote" "prune" remote) (format nil "Pruned ~a" remote))))))

(define-menu git-remote-menu (:title "Remote" :show git-menu-shows-p :note git-menu-note)
  "Open the remote menu: add a remote, rename one, give it a new address,
remove it, or forget its branches gone. \\`C-h' there lists every action."
  ("Remotes"
   ("a" "Add" git-remote-add)
   ("r" "Rename" git-remote-rename)
   ("u" "New address" git-remote-set-address)
   ("p" "Forget branches gone" git-remote-prune)
   ("d" "Remove" git-remote-remove)))

;;; Every branch and tag

(define-major-mode git-refs-mode special-mode
  "A repository's branches, its remotes' branches and its tags, a line each
with the commit it is at (\\[git-show-refs]), in a section for each kind.
\\<git-refs-mode>\\[git-refs-log] lists the log of the one on the line, \\[list-toggle-fold] folds or unfolds the
section of the line, \\[list-toggle-all-folds] all of them; \\[quit-window] closes."
  :lighter "Git refs" :interactive nil)

(defparameter *git-refs-sections*
  '(("Branches" "refs/heads") ("Remote branches" "refs/remotes") ("Tags" "refs/tags"))
  "The sections of a list of references, each its title and where git keeps them.")

(defparameter *git-refs-format*
  (concatenate 'string "--format=%(HEAD)%00%(refname:short)"
               "%00%(if)%(*objectname)%(then)%(*objectname:short)%(else)%(objectname:short)%(end)"
               "%00%(upstream:short)"
               "%00%(if)%(*objectname)%(then)%(*contents:subject)%(else)%(contents:subject)%(end)"
               "%00%(symref)")
  "How git for-each-ref is asked to write a reference for the list of
references: whether it is checked out, its name, its commit (a tag's, not
the tag's own object), its upstream, the commit's subject, and what it
stands for when it only names another.")

(defun git-refs-line (head name hash upstream subject)
  "The line of the list of references for the reference NAME at the commit
HASH, whose subject is SUBJECT, HEAD * when it is checked out, UPSTREAM its
upstream or empty; the name in a column 24 wide on the screen."
  (format nil "~:[ ~;*~] ~a ~a ~@[~a  ~]~a" (equal head "*") (column-text (escape-controls name) 24 :elide nil)
          (column-text hash 8)
          (and (plusp (length upstream)) (format nil "-> ~a" (escape-controls upstream)))
          (escape-controls subject)))

(defun git-refs-render (buffer &optional (point :keep))
  "Draw BUFFER, a list of references, from the references last read (its
GIT-REFS-SECTIONS), a section for each kind that has some, folded as they
are (SECTIONED-PIECES), point as RENDER-PIECES puts it."
  (multiple-value-bind (entries pieces)
      (sectioned-pieces buffer (remove nil (tuatara.buffer:buffer-local buffer 'git-refs-sections) :key #'cdr)
                        #'identity
                        (lambda (ref section)
                          (declare (ignore section))
                          (destructuring-bind (head name hash upstream subject &rest more) ref
                            (declare (ignore more))
                            (list (git-refs-line head name hash upstream subject) (string #\Newline)))))
    (render-pieces buffer pieces :point point :entries entries :redraw 'git-refs-render)))

(define-command git-show-refs ()
  "List the repository's branches (the one checked out marked *, each with
its upstream), its remotes' branches and its tags, a line each with the
commit it is at and its subject, in sections folded as in every list in
sections; RET on one lists its log."
  (with-git-repository (top)
    (git-ask "Reading the references" top
             (lambda (job top)
               (loop for (title where) in *git-refs-sections*
                     collect (cons title (remove-if (lambda (ref) (plusp (length (sixth ref))))
                                                    (git-records (git-job-output job top (list "for-each-ref" *git-refs-format* where)) 6)))))
             (lambda (sections)
               (let ((buffer (buffer-in-mode (git-buffer-name "*git refs: ~a*" top) 'git-refs-mode)))
                 (git-set-top buffer top)
                 (setf (tuatara.buffer:buffer-local buffer 'git-refs-sections) sections)
                 (git-refs-render buffer 1)
                 (show-buffer buffer))))))

(define-command git-refs-log ()
  "List the log of the branch or tag on the line of the list of references;
on a section's heading, fold or unfold the section."
  (let* ((list (current-buffer))
         (entry (list-entry-at list))
         (ref (entry-member entry)))
    (cond (ref (let ((name (second ref)))
                 (git-show-log (git-here) (format nil "Log of ~a" name) (list (format nil "--max-count=~d" *git-log-length*) name "--"))))
          ((eq (first entry) :section) (toggle-list-section list (second entry)))
          (t (message "No branch or tag on this line")))))

(commands-for-mode 'git-refs-mode "a repository's references (\\[git-show-refs])"
                   'git-refs-log 'list-toggle-fold 'list-toggle-all-folds)

(defun install-git-references ()
  "Bind the tag menu to t, the remote menu to M and the list of references
to y in a status, and RET, l and h in that list, TAB and S-TAB folding its
sections."
  (define-keys *git-status-keymap* '(("t" git-tag-menu) ("M" git-remote-menu) ("y" git-show-refs)))
  (define-keys (mode-keymap 'git-refs-mode) '(("RET" git-refs-log) ("l" git-refs-log) ("h" quit-window)
                                               ("TAB" list-toggle-fold) ("S-TAB" list-toggle-all-folds))))
