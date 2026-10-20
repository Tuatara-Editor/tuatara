;;;; lisp/git/repository.lisp --- Submodules, ignored files, notes, subtrees and sparse checkouts.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status o opens the submodule menu, i the ignore menu, T the notes
;;;; menu, O the subtree menu and > the sparse checkout menu, as Magit's do.
;;;; A status lists the submodules a repository has; RET on one shows its own
;;;; status. A file of patterns git ignores is written here only when it is a
;;;; file of its own, never through a link.

(in-package #:tuatara.defaults)

;;; Submodules

(defun parse-git-submodules (text)
  "TEXT, what git submodule status printed, as a list of (PATH STATE),
STATE a word: checked out as recorded, not checked out, at another commit,
or clashing."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for words = (uiop:split-string (string-left-trim '(#\Space) (subseq line (min 1 (length line)))) :separator '(#\Space))
        when (and (plusp (length line)) (second words))
          collect (list (second words)
                        (case (char line 0)
                          (#\- "not checked out") (#\+ "at another commit") (#\U "clashing") (t "as recorded")))))

(defun git-submodules (job top)
  "On JOB's thread: the submodules of the repository at TOP (PARSE-GIT-SUBMODULES), none without a .gitmodules."
  (and (tuatara.io:path-kind (concatenate 'string top ".gitmodules") :follow nil)
       (parse-git-submodules (git-job-output job top (list "submodule" "status")))))

(defun git-ask-submodule (prompt top then)
  "Ask with PROMPT for a submodule of the repository at TOP (the one on
point's line of a status first), then call THEN with its path."
  (let ((at (let ((entry (and (git-here) (list-entry-at)))) (and (eq (first entry) :submodule) (second entry)))))
    (git-ask "Reading the submodules" top #'git-submodules
             (lambda (modules)
               (if (null modules)
                   (message "No submodule")
                   (let ((path (completing-read prompt (mapcar (lambda (m) (make-candidate (first m) :annotation (second m))) modules)
                                                :require-match t :default (or at (first (first modules))))))
                     (when (plusp (length path)) (funcall then path))))))))

(define-command git-submodule-add ()
  "Add a repository, its address asked for, as a submodule at a path asked
for (named after the address when nothing is typed), cloned there."
  (with-git-repository (top)
    (let ((address (git-read-address "Address of the submodule: ")))
      (when address
        (let* ((name (let ((base (car (last (tuatara.text:split-words address '(#\/ #\:))))))
                       (if (and base (uiop:string-suffix-p base ".git")) (subseq base 0 (- (length base) 4)) base)))
               (path (string-trim '(#\Space) (read-string (format nil "Put ~a at: " (escape-controls address)) :initial (or name "")))))
          (if (git-inside-path-p path)
              (git-over-network "Adding a submodule" top (list "submodule" "add" "--" address path)
                                (format nil "Added the submodule ~a" (escape-controls path)))
              (message "Not a path within the repository: ~a" (escape-controls path))))))))

(define-command git-submodule-update ()
  "Check every submodule out at the commit this repository records (cloning
those not checked out yet), and theirs in turn; with -r in the submodule menu
at the newest commit of the branch each follows instead."
  (with-git-repository (top switches)
    (git-over-network "Updating the submodules" top (append (list "submodule" "update" "--init" "--recursive") switches)
                      "Updated the submodules")))

(define-command git-submodule-sync ()
  "Take each submodule's address again from .gitmodules, after it changed there."
  (with-git-repository (top)
    (git-run "Syncing the submodules" top (list "submodule" "sync" "--recursive") "Synced the submodules' addresses")))

(define-command git-submodule-remove ()
  "Remove a submodule, asked for, after asking: its folder and its entry in
.gitmodules go, the removal staged."
  (with-git-repository (top)
    (git-ask-submodule "Remove submodule: " top
                       (lambda (path)
                         (when (yes-or-no-p* (format nil "Remove the submodule ~a and its folder? Changes in it are lost" (escape-controls path)))
                           (start-git-job "Removing a submodule"
                                          (lambda (job)
                                            (git-job-output job top (list "submodule" "deinit" "-f" "--" path))
                                            (git-job-output job top (list "rm" "-f" "--" path)))
                                          (lambda (text) (declare (ignore text)) (message "Removed the submodule ~a" (escape-controls path)))
                                          :after (lambda () (git-refresh-repository top))))))))

(define-command git-submodule-status ()
  "Show the status of a submodule, asked for (the one on point's line first), as \\[git] shows a repository's."
  (with-git-repository (top)
    (git-ask-submodule "Status of submodule: " top
                       (lambda (path) (git-show-status (concatenate 'string top (string-right-trim "/" path) "/"))))))

(define-menu git-submodule-menu (:title "Submodule" :show git-menu-shows-p :note git-menu-note)
  "Open the submodule menu: add a submodule, check them out, take their
addresses again, remove one or show its status. \\`C-h' there lists every action."
  ("Submodules"
   ("a" "Add" git-submodule-add)
   ("u" "Check them out" git-submodule-update)
   ("s" "Take their addresses again" git-submodule-sync)
   ("g" "Show one's status" git-submodule-status)
   ("d" "Remove one" git-submodule-remove))
  ("Switches"
   ("- r" "Newest of the branch each follows" :switch "--remote")))

;;; Ignoring files

(defun git-ignore-escaped (path)
  "PATH written as a pattern git ignores that file alone by: \\, [, ], * and
? taken as themselves, and spaces at its end kept."
  (let* ((end (length (string-right-trim " " path)))
         (escaped (with-output-to-string (out)
                    (loop for c across (subseq path 0 end)
                          do (when (find c "\\[]*?") (write-char #\\ out))
                             (write-char c out)))))
    (with-output-to-string (out)
      (write-string escaped out)
      (loop repeat (- (length path) end) do (write-string "\\ " out)))))

(defun git-ignore-pattern (top &optional folder)
  "The pattern offered for ignoring the file on point's line of a status, or
the current file: its path from TOP, or from FOLDER (the folder it is in,
ending in /), anchored with a /, its
characters git would read as a pattern's taken as themselves (GIT-IGNORE-ESCAPED)."
  (let ((path (git-file-here top)))
    (if path
        (let ((from (if folder (subseq (concatenate 'string top path) (length folder)) path)))
          (concatenate 'string "/" (git-ignore-escaped from)))
        "")))

(defun git-add-ignore-pattern (file pattern top)
  "Add PATTERN as a line of FILE, a file of patterns git ignores, made with
the folders it is in when it is not there, unless it is a line of it already;
refused, said so, when FILE is a link or not a file. The file is written whole
again in one step, a link put in its place meanwhile replaced, never followed.
The status of the repository at TOP is read again."
  (let ((kind (tuatara.io:path-kind file :follow nil)))
    (if (and kind (not (eq kind :regular)))
        (message "~a is not a file of its own: not written" (abbreviated-path file))
        (let ((old (if kind
                       (tuatara.io:file-octets file :limit (* 16 1024 1024))
                       (make-array 0 :element-type '(unsigned-byte 8)))))
          (if (member pattern (uiop:split-string (tuatara.text:lenient-string old) :separator '(#\Newline))
                      :test #'string=)
              (message "~a is in ~a already" (escape-controls pattern) (abbreviated-path file))
              (progn
                (tuatara.io:ensure-directories file)
                (tuatara.io:replace-file
                 file
                 (concatenate '(vector (unsigned-byte 8))
                              old
                              ;; a line of its own, even when the file does not end in one
                              (if (and (plusp (length old)) (/= 10 (aref old (1- (length old))))) #(10) #())
                              (tuatara.text:string-octets (format nil "~a~%" pattern)))
                 :mode (if kind (logand #o7777 (tuatara.io:stamp-mode (tuatara.io:path-stamp file :follow nil))) #o666))
                (message "~a ignored, in ~a" (escape-controls pattern) (abbreviated-path file))
                (git-refresh-repository top)))))))

(defun git-ignore-in (where)
  "Ask for a pattern for git to ignore and add it WHERE: :SHARED the
repository's own .gitignore at its top, :FOLDER the .gitignore of the
current file's folder, :PRIVATE git's own exclude file, which is never
committed."
  (with-git-repository (top)
    (let* ((folder (and (eq where :folder)
                        (let ((path (git-file-here top)))
                          (if path (tuatara.io:parent-path (concatenate 'string top path)) top))))
           (pattern (string-left-trim '(#\Space) (read-string "Ignore: " :initial (git-ignore-pattern top folder)))))
      (cond ((zerop (length (string-trim '(#\Space) pattern))) (message "No pattern given: nothing ignored"))
            ((find #\Newline pattern) (message "A pattern is one line"))
            ((eq where :private)
             (git-ask "Reading where git keeps its exclusions" top
                      (lambda (job top) (git-output-line job top (list "rev-parse" "--path-format=absolute" "--git-path" "info/exclude")))
                      (lambda (file) (git-add-ignore-pattern file pattern top))))
            (t (git-add-ignore-pattern (concatenate 'string (or folder top) ".gitignore") pattern top))))))

(define-command git-ignore-shared ()
  "Have git ignore a pattern asked for (the file on point's line, or the
current one, offered), in the repository's .gitignore, committed with it."
  (git-ignore-in :shared))

(define-command git-ignore-folder ()
  "Have git ignore a pattern asked for in the .gitignore of the current file's folder."
  (git-ignore-in :folder))

(define-command git-ignore-private ()
  "Have git ignore a pattern asked for here alone, in git's own exclude
file, which is never committed."
  (git-ignore-in :private))

(define-menu git-ignore-menu (:title "Ignore" :show git-menu-shows-p :note git-menu-note)
  "Open the ignore menu: have git ignore a file or a pattern, for everyone
\(.gitignore at the top or in the file's folder) or here alone. \\`C-h' there
lists every action."
  ("Ignore"
   ("t" "For everyone, at the top" git-ignore-shared)
   ("s" "For everyone, in this folder" git-ignore-folder)
   ("p" "Here alone" git-ignore-private)))

;;; Notes

(define-command git-notes-edit ()
  "Write a note on a commit asked for (the one on point's line first, else
HEAD), its note so far shown to begin with: a note says more of a commit
without changing it."
  (with-git-repository (top)
    (git-ask-revision "Note on: " top
                      (lambda (revision)
                        (git-ask "Reading the note" top
                                 (lambda (job top)
                                   (multiple-value-bind (text ok) (git-job-try job top (list "notes" "show" revision))
                                     (if ok (string-right-trim '(#\Newline) text) "")))
                                 (lambda (note)
                                   (let ((new (read-string (format nil "Note on ~a: " revision) :initial note)))
                                     (if (zerop (length new))
                                         (message "No note written: M-x git-notes-remove takes one away")
                                         (git-run "Writing a note" top (list "notes" "add" "-f" "-m" new revision)
                                                  (format nil "Wrote a note on ~a" revision))))))))))

(define-command git-notes-remove ()
  "Take away the note on a commit asked for."
  (with-git-repository (top)
    (git-ask-revision "Take the note away from: " top
                      (lambda (revision)
                        (git-run "Taking a note away" top (list "notes" "remove" revision)
                                 (format nil "Took the note away from ~a" revision))))))

(define-command git-notes-prune ()
  "Take away the notes on commits the repository no longer has."
  (with-git-repository (top)
    (git-run "Pruning the notes" top (list "notes" "prune") "Pruned the notes")))

(define-menu git-notes-menu (:title "Notes" :show git-menu-shows-p :note git-menu-note)
  "Open the notes menu: write a note on a commit, take one away, or prune
those on commits gone. \\`C-h' there lists every action."
  ("Notes"
   ("T" "Write a note on a commit" git-notes-edit)
   ("r" "Take a note away" git-notes-remove)
   ("p" "Prune the notes" git-notes-prune)))

;;; Subtrees

(defun git-read-prefix (prompt)
  "Ask with PROMPT for a folder within the repository for a subtree
\(GIT-INSIDE-PATH-P, a / at either end left out); NIL, said so, for one
that is not."
  (let ((prefix (string-trim '(#\Space #\/) (read-string prompt))))
    (if (git-inside-path-p prefix)
        prefix
        (progn (message "Not a folder within the repository: ~a" (escape-controls prefix)) nil))))

(defun git-subtree (verb doing done)
  "Run git subtree VERB (add, pull, push) over the network for a folder, a
repository (a remote's name or an address) and a branch or revision asked
for, with the subtree menu's switches, DOING said meanwhile and DONE, a
format given the folder, once done."
  (with-git-repository (top switches)
    (let ((prefix (git-read-prefix (format nil "Subtree folder to ~a: " verb))))
      (when prefix
        (let ((repository (git-read-address (format nil "Repository (a remote or an address) to ~a ~a: " verb prefix))))
          (when repository
            (let ((ref (git-read-name "Branch or revision: " '() :default "main")))
              (when ref
                (git-over-network doing top (append (list "subtree" verb (format nil "--prefix=~a" prefix)) switches (list repository ref))
                                  (format nil done prefix))))))))))

(define-command git-subtree-add ()
  "Bring another repository's branch into a new folder of this one as a
subtree, its history joined in (one commit with -s in the subtree menu)."
  (git-subtree "add" "Adding a subtree" "Added the subtree ~a"))

(define-command git-subtree-pull ()
  "Bring the newer commits of another repository's branch into the subtree folder."
  (git-subtree "pull" "Pulling into a subtree" "Pulled into the subtree ~a"))

(define-command git-subtree-push ()
  "Send the commits that changed the subtree folder to another repository's branch."
  (git-subtree "push" "Pushing a subtree" "Pushed the subtree ~a"))

(define-command git-subtree-split ()
  "Make a history of the commits that changed a folder asked for, as if it
were a repository of its own, and say the commit it ends at (a branch of it
with =b in the subtree menu)."
  (with-git-repository (top switches)
    (let ((prefix (git-read-prefix "Subtree folder to split: ")))
      (when prefix
        (git-run "Splitting a subtree" top (append (list "subtree" "split" (format nil "--prefix=~a" prefix)) switches)
                 (lambda (text) (format nil "~a split off, ending at ~a" (escape-controls prefix)
                                        (string-trim '(#\Newline #\Space) (car (last (tuatara.text:split-words text '(#\Newline))))))))))))

(define-menu git-subtree-menu (:title "Subtree" :show git-menu-shows-p :note git-menu-note)
  "Open the subtree menu: bring another repository into a folder, pull its
newer commits, push the folder's, or split the folder's history off.
\\`C-h' there lists every action."
  ("Bring in"
   ("a" "Add" git-subtree-add)
   ("f" "Pull" git-subtree-pull))
  ("Send out"
   ("p" "Push" git-subtree-push)
   ("s" "Split" git-subtree-split))
  ("Switches"
   ("- s" "One commit, not its history" :switch "--squash")
   ("= b" "A branch of the split" :option "--branch=")))

;;; Sparse checkouts

(defun git-read-folders (prompt)
  "Ask with PROMPT for folders of the repository, apart by spaces; NIL, said
so, when one is not within it (GIT-INSIDE-PATH-P) or none is given."
  (let ((folders (tuatara.text:split-words (read-string prompt) '(#\Space))))
    (cond ((null folders) (message "No folder given") nil)
          ((find-if-not #'git-inside-path-p folders)
           (message "Not a folder within the repository: ~a" (escape-controls (find-if-not #'git-inside-path-p folders))) nil)
          (t folders))))

(define-command git-sparse-set ()
  "Check out only folders asked for (and the files at the top), the rest of
the repository kept out of the files."
  (with-git-repository (top)
    (let ((folders (git-read-folders "Check out only the folders: ")))
      (when folders
        (git-run "Narrowing the checkout" top (list* "sparse-checkout" "set" "--cone" "--" folders)
                 (format nil "Checked out only ~{~a~^, ~}" folders))))))

(define-command git-sparse-add ()
  "Check out folders asked for too, in a sparse checkout."
  (with-git-repository (top)
    (let ((folders (git-read-folders "Check out the folders too: ")))
      (when folders
        (git-run "Widening the checkout" top (list* "sparse-checkout" "add" "--" folders)
                 (format nil "Checked out ~{~a~^, ~} too" folders))))))

(define-command git-sparse-disable ()
  "Check the whole repository out again."
  (with-git-repository (top)
    (git-run "Checking everything out" top (list "sparse-checkout" "disable") "The whole repository is checked out")))

(define-command git-sparse-reapply ()
  "Keep out of the files again what the sparse checkout leaves out, after a merge or a rebase brought some in."
  (with-git-repository (top)
    (git-run "Narrowing the checkout again" top (list "sparse-checkout" "reapply") "Checked out as the sparse checkout says")))

(define-menu git-sparse-menu (:title "Sparse checkout" :show git-menu-shows-p :note git-menu-note)
  "Open the sparse checkout menu: check out only some folders, more of them,
everything again, or narrow it again. \\`C-h' there lists every action."
  ("Folders"
   ("s" "Only these" git-sparse-set)
   ("a" "These too" git-sparse-add)
   ("r" "Narrow again" git-sparse-reapply)
   ("d" "Everything again" git-sparse-disable)))

(defun install-git-repository ()
  "Bind the submodule menu to o, the ignore menu to i, the notes menu to T,
the subtree menu to O and the sparse checkout menu to > in a status."
  (define-keys *git-status-keymap* '(("o" git-submodule-menu) ("i" git-ignore-menu) ("T" git-notes-menu)
                                     ("O" git-subtree-menu) (">" git-sparse-menu))))
