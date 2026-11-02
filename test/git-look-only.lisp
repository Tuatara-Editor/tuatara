;;;; test/git-look-only.lisp --- Git in a repository looked at only: reading alone, none of its programs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defmacro with-looked-at-only ((directory) &body body)
  "Run BODY with the repository at DIRECTORY answered look only, and
forgotten after."
  (let ((real (gensym)))
    `(let ((,real (tuatara.defaults::real-folder (namestring ,directory))))
       (unwind-protect (progn (tuatara.defaults::state-list-put tuatara.defaults::*look-only* (list ,real))
                              ,@body)
         (tuatara.defaults::state-list-drop tuatara.defaults::*look-only* (list ,real))))))

(defun hostile-repository (dir marker)
  "Make DIR a repository (SCRATCH-REPOSITORY) whose settings, the files they
take in, its attributes and its hooks name programs that each make a file
beginning with MARKER, a path, should they run; a.txt and b.dat changed
since their commit."
  (scratch-repository dir)
  (flet ((file (name text) (alexandria:write-string-into-file text (merge-pathnames name dir) :if-exists :supersede))
         (run (what) (format nil "touch ~a-~a" marker what)))
    (file "b.dat" (format nil "data~%"))
    (uiop:run-program (list "git" "-C" (namestring dir) "add" "b.dat") :ignore-error-status t)
    (uiop:run-program (list "git" "-C" (namestring dir) "commit" "-q" "-m" "Data") :ignore-error-status t)
    (with-open-file (out (merge-pathnames ".git/config" dir) :direction :output :if-exists :append)
      (format out "[filter \"evil\"]~%	clean = ~a; cat~%	smudge = ~a; cat~%	required = true~%" (run "clean") (run "smudge"))
      (format out "[filter \"pro\"]~%	process = ~a~%" (run "process"))
      (format out "[diff \"evil\"]~%	textconv = ~a; cat~%	command = ~a~%" (run "textconv") (run "command"))
      (format out "[diff]~%	external = ~a~%" (run "external"))
      (format out "[core]~%	fsmonitor = ~a~%	hooksPath = hooks~%	pager = ~a; cat~%" (run "fsmonitor") (run "pager"))
      (format out "[gpg]~%	program = ~a~%[log]~%	showSignature = true~%" (run "gpg"))
      (format out "[include]~%	path = more.cfg~%"))
    (file ".git/more.cfg" (format nil "[filter \"a=b\"]~%	clean = ~a; cat~%" (run "included")))
    (ensure-directories-exist (merge-pathnames ".git/info/" dir))
    (file ".git/info/attributes" (format nil "*.dat filter=a=b diff=evil~%"))
    (file ".gitattributes" (format nil "*.txt filter=evil diff=evil~%*.dat filter=pro~%"))
    (ensure-directories-exist (merge-pathnames "hooks/" dir))
    (dolist (hook '("post-index-change" "post-checkout" "reference-transaction"))
      (let ((path (merge-pathnames (format nil "hooks/~a" hook) dir)))
        (file (format nil "hooks/~a" hook) (format nil "#!/bin/sh~%~a~%" (run hook)))
        (sb-posix:chmod (namestring path) #o755)))
    (sleep 1.1)
    (file "a.txt" (format nil "one~%changed~%"))
    (file "b.dat" (format nil "data~%changed~%"))))

(defun look-only-entries ()
  "The entries of the list of projects looked at only."
  (tuatara.defaults::state-list-entries tuatara.defaults::*look-only*))

(defun markers (marker)
  "The files made by the programs of a HOSTILE-REPOSITORY whose MARKER was given."
  (let ((folder (directory-namestring marker)) (name (file-namestring marker)))
    (remove-if-not (lambda (path) (uiop:string-prefix-p name (file-namestring path)))
                   (uiop:directory-files folder))))

(define-test (git look-only-runs-no-program)
  ;; a repository looked at only: git reads it, and no program its settings, the files they take in,
  ;; its attributes or its hooks name runs, for status, log -p, diff, show or blame
  (with-test-directory (dir)
    (with-test-directory (out)
      (let ((marker (namestring (merge-pathnames "ran" out)))
            (top (namestring dir))
            (tuatara-user:*lsp-trusted-projects* '()))
        (hostile-repository dir marker)
        (with-looked-at-only (dir)
          (true (tuatara.defaults::project-look-only-reason top))
          (dolist (arguments '(("status" "--porcelain=v2") ("diff") ("diff" "--cached") ("log" "-p" "-2") ("show")
                               ("show" "HEAD:a.txt") ("blame" "a.txt") ("blame" "b.dat") ("stash" "list")
                               ("cat-file" "-p" "HEAD:b.dat") ("ls-files" "-m") ("describe" "--always" "--dirty")))
            (multiple-value-bind (octets ok) (tuatara.git:run-git top arguments)
              (true ok "~s" arguments)
              (true octets)))
          (multiple-value-bind (octets ok) (tuatara.git:run-git top '("diff"))
            (true ok)
            ;; the change itself, not a program's version of it
            (true (search "+changed" (tuatara.text:octets-string octets))))
          (is equal '() (markers marker)))
        ;; the same repository trusted: its programs do run, so the settings above were in force
        (let ((tuatara-user:*lsp-trusted-projects* (list top)))
          (tuatara.git:run-git top '("status" "--porcelain"))
          (tuatara.git:run-git top '("diff"))
          (true (markers marker)))))))

(define-test (git look-only-reads-only)
  ;; looked at only, git runs only what reads, given as it reads: what writes, reaches out or runs a program is refused
  (dolist (words '(("commit" "-m" "x") ("add" "a.txt") ("checkout" "--" ".") ("push") ("fetch") ("pull") ("clone" "x")
                   ("stash" "push") ("stash") ("branch" "new") ("branch" "-D" "main") ("branch" "--del" "main")
                   ("tag" "v1") ("tag" "-d" "v1") ("remote" "add" "o" "x") ("remote" "show" "origin")
                   ("config" "a.b" "c") ("reset" "--hard") ("gc") ("submodule" "update") ("submodule" "status")
                   ("notes" "add" "-m" "x") ("reflog" "expire") ("worktree" "add" "x") ("hash-object" "-w" "a.txt")
                   ("log" "--output=x") ("log" "--out=x") ("diff" "--ext-diff") ("log" "--textconv") ("cat-file" "--filters" "HEAD:a")
                   ("show" "--show-signature") ("log" "--format=%G?") ("for-each-ref" "--format=%(signature)")
                   ("grep" "-Ocat" "x") ("ls-files" "--recurse-submodules")
                   ("-c" "a.b=c" "log") ("--git-dir=/tmp/x" "log") ("--exec-path=/tmp" "log") ("archive" "HEAD")
                   ("format-patch" "-1") ("apply" "--cached" "-") ("a-command-not-known")))
    (true (tuatara.git::git-reading-refusal words) "~s" words))
  (dolist (words '(("status" "--porcelain=v2" "--branch" "-z") ("log" "-n10" "--format=%H") ("show" "HEAD")
                   ("diff" "--cached" "-M" "--" "a.txt") ("blame" "a.txt") ("ls-files" "-z") ("branch" "--show-current")
                   ("branch" "-a" "-v") ("tag" "-l") ("stash" "list") ("stash" "show" "-p" "stash@{0}") ("remote")
                   ("remote" "get-url" "origin") ("config" "--get" "user.name") ("rev-parse" "--absolute-git-dir")
                   ("for-each-ref" "--format=%(refname:short)") ("cat-file" "-p" "HEAD") ("reflog" "show")
                   ("worktree" "list" "--porcelain") ("--no-pager" "log" "-1") ("log" "--" "--output=x")))
    (false (tuatara.git::git-reading-refusal words) "~s" words))
  ;; and run-git refuses, in one sentence
  (with-test-directory (dir)
    (scratch-repository dir)
    (with-looked-at-only (dir)
      (let ((before (git-output dir "log" "--format=%H")))
        (handler-case (progn (tuatara.git:run-git (namestring dir) '("commit" "--allow-empty" "-m" "x")) (false t))
          (tuatara.git::git-refused (e)
            (let ((words (princ-to-string e)))
              (true (uiop:string-prefix-p "git commit is not run: " words) "~a" words)
              (true (search "is looked at only (it is not trusted)" words) "~a" words))))
        (is string= before (git-output dir "log" "--format=%H"))))))

(define-test (git look-only-answered-and-shown)
  ;; l to the trust question: the status opens, saying so at its top and in the mode line, what would
  ;; change the repository dimmed in its menus and refused when pressed, and the answer remembered
  (with-test-directory (dir)
    (scratch-repository dir :trusted nil)
    (alexandria:write-string-into-file (format nil "one~%two~%") (merge-pathnames "a.txt" dir) :if-exists :supersede)
    (let* ((tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*lsp-trusted-projects* '())
           (top (tuatara.git:repository-top (namestring dir)))
           (real (tuatara.defaults::real-folder top)))
      (flet ((status () (tuatara.git::git-buffer-of top 'tuatara.git::git-status-mode)))
        (unwind-protect
             (let ((f (auto-save-session
                       (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.txt" dir))))
                             "C-x g" (list :wait 0.1) "l"
                             (list :until (lambda () (and (status) (tuatara.buffer:buffer-local (status) 'tuatara.git::git-state))))
                             "M-< C-s m o d i f i e d RET s" (list :wait 0.1)
                             "c" (list :wait 0.1) "c" (list :wait 0.1) "C-g")
                       :width 160 :height 30)))
               (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "(y, l or n)"))
               (true (member real (look-only-entries) :test #'equal))
               (true (tuatara.frontend.fake:fake-frame-showing f "Look only: it is not trusted."))
               (true (tuatara.frontend.fake:fake-frame-showing f " Look only "))
               (true (tuatara.frontend.fake:fake-frame-showing f "git-stage is not run: "))
               (true (tuatara.frontend.fake:fake-frame-showing f "git-commit is not run: "))
               ;; nothing staged
               (is string= "" (git-output dir "diff" "--cached" "--name-only"))
               ;; the commit menu's actions dimmed, its title not
               (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Amend")
                 (let* ((row (position-if (lambda (r) (search "Amend" r)) screen))
                        (at (search "Amend" (aref screen row))))
                   (true (find-if (lambda (span) (and (<= (first span) at) (< at (second span))
                                                      (member :key-hint-unavailable (alexandria:ensure-list (third span)))))
                                  (aref spans row)))))
               ;; Look only in the mode line's own face
               (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f " Look only ")
                 (let* ((row (position-if (lambda (r) (search " Look only " r)) screen))
                        (at (search " Look only " (aref screen row))))
                   (true (find-if (lambda (span) (and (= (first span) at) (member :mode-line-look-only (alexandria:ensure-list (third span)))))
                                  (aref spans row))))))
          (tuatara.defaults::state-list-drop tuatara.defaults::*look-only* (list real)))))))

(define-test (git look-only-asked-not-again)
  ;; answered look only, the repository is not asked about again; trust-project asks again and y trusts it
  (with-test-directory (dir)
    (scratch-repository dir)
    (let* ((tuatara.defaults::*modal-editing* nil)
           (tuatara-user:*lsp-trusted-projects* '())
           (top (tuatara.git:repository-top (namestring dir)))
           (real (tuatara.defaults::real-folder top)))
      (unwind-protect
           (with-looked-at-only (dir)
             (let ((f (auto-save-session
                       (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.txt" dir))))
                             "C-x g" (list :wait 0.2))
                       :width 160 :height 30)))
               (false (tuatara.frontend.fake:fake-frame-showing-across-rows f "(y, l or n)"))
               (true (tuatara.frontend.fake:fake-frame-showing f "Look only: it is not trusted.")))
             (let ((f (auto-save-session
                       (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.txt" dir))))
                             "M-x t r u s t - p r o j e c t RET" (list :wait 0.1) "y" (list :wait 0.1) "y e s RET" (list :wait 0.1))
                       :width 160 :height 30)))
               (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "(y, l or n)"))
               (true (tuatara.defaults::project-trusted-p top))
               (false (tuatara.defaults::project-look-only-reason top))))
        (tuatara.defaults::state-list-drop tuatara.defaults::*lsp-trusted* (list real))))))

(define-test (git look-only-list-fails-closed)
  ;; the list of projects looked at only is kept as every list is; a line of a later release naming a folder,
  ;; whatever fields follow, is still read, so that the folder stays looked at only
  (is string= "look-only" (tuatara.defaults::state-list-name tuatara.defaults::*look-only*))
  (false (tuatara.defaults::state-list-trust tuatara.defaults::*look-only*))
  (is string= "/cases/seized/" (tuatara.defaults::parse-absolute-path-line (format nil "/cases/seized/~cfield of later" #\Tab)))
  (is string= "/cases/seized/" (funcall (tuatara.defaults::state-list-line-key tuatara.defaults::*look-only*)
                                        (format nil "/cases/seized/~cfield of later" #\Tab))))

(define-test (evidence seized-repository-read-untouched)
  ;; a repository in an evidence folder: read in look-only mode, and nothing of it changes, its index included
  (with-test-directory (dir)
    (let* ((ev (namestring (ensure-directories-exist (merge-pathnames "ev/" dir))))
           (seized (namestring (ensure-directories-exist (merge-pathnames "ev/seized/" dir)))))
      (scratch-repository seized)
      (alexandria:write-string-into-file (format nil "two~%") (merge-pathnames "b.txt" seized))
      (sleep 1.1)
      ;; a.txt's time changed, its text not: a diff would write the index again
      (let ((now (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0))))
        (sb-posix:utimes (concatenate 'string seized "a.txt") now now))
      (flet ((snapshot ()
               ;; every file and folder, .git's too, with its size, time and inode
               (uiop:run-program (list "find" seized "-printf" "%p %s %T@ %i\\n") :output :string)))
        (let ((before (snapshot)))
          (with-evidence-folders ((list ev))
            (true (tuatara.defaults::project-look-only-reason seized))
            (dolist (arguments '(("status" "--porcelain=v2" "--branch" "-z") ("diff") ("diff" "--cached") ("log" "-p")
                                 ("show") ("blame" "a.txt") ("describe" "--always" "--dirty") ("ls-files" "-m")))
              (true (nth-value 1 (tuatara.git:run-git seized arguments)) "~s" arguments))
            (true (refused-any (tuatara.git:run-git seized '("checkout" "--" "a.txt")))))
          (is equal before (snapshot)))))))

(defun repository-snapshot (folder)
  "Every file and folder within FOLDER, .git's too, with its kind, size, times, inode and mode, and the SHA-256 of each file."
  (uiop:run-program (list "sh" "-c" "cd \"$1\" && find . -printf '%p %y %s %T@ %C@ %i %m\\n' | sort && find . -type f -print0 | sort -z | xargs -0 sha256sum"
                          "snapshot" folder)
                    :output :string :ignore-error-status t))

(defparameter *hostile-commands*
  '(("status" "--porcelain=v2" "--branch" "-z" "--untracked-files=normal" "--ignored=traditional") ("diff") ("diff" "--cached")
    ("log" "-p" "-2") ("show") ("blame" "--porcelain" "--" "a.txt") ("hash-object" "a.txt") ("diff-files" "-p") ("grep" "one")
    ("ls-files" "-s") ("describe" "--always" "--dirty") ("cat-file" "-p" "HEAD:b.txt") ("for-each-ref") ("stash" "list")
    ("range-diff" "HEAD~1..HEAD" "HEAD~1..HEAD") ("rev-parse" "HEAD") ("count-objects" "-v") ("tag" "-l") ("reflog" "show"))
  "What git is asked of each hostile repository: the reading the editor does, and more.")

(define-test (git hostile-repositories-run-nothing)
  ;; repositories not trusted whose settings (driver names empty, of other octets than UTF-8, of another case), the files
  ;; they take in, their attributes, hooks, split index, grafts, replace refs, mailmap, promisor remote, FIFOs and
  ;; nesting try to run a program or change what is shown: nothing runs, nothing in them changes, git reads them as
  ;; they are, and a FIFO is never waited on
  (with-test-directory (dir)
    (let* ((repos (namestring (merge-pathnames "r/" dir)))
           (marks (namestring (ensure-directories-exist (merge-pathnames "m/" dir))))
           (tuatara-user:*lsp-trusted-projects* '()))
      (uiop:run-program (list "sh" (namestring (asdf:system-relative-pathname :tuatara "test/git-look-only/hostile-repositories.sh"))
                              repos marks)
                        :output nil :error-output nil)
      (flet ((repo (name) (concatenate 'string repos name "/"))
             (run (name arguments)
               (handler-case (multiple-value-bind (octets ok) (tuatara.git:run-git (concatenate 'string repos name "/") arguments :seconds 20)
                               (values (and octets (tuatara.text:lenient-string octets)) ok))
                 (tuatara.git::git-refused (e) (values (princ-to-string e) :refused)))))
        (dolist (name '("ascii" "bytes" "latin" "empty" "caseful" "incl" "tree" "fsmon" "hooks" "split" "gc" "lazy"
                        "idxlink" "idxfifo" "cfgfifo" "reft" "sha" "unknown" "grafts" "mailmap" "replaced"))
          (let ((before (repository-snapshot (repo name)))
                (start (get-internal-real-time)))
            (dolist (arguments *hostile-commands*)
              (run name arguments))
            (true (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)) "~a took too long" name)
            (is string= before (repository-snapshot (repo name)) "~a changed" name)))
        ;; read as they are
        (dolist (name '("ascii" "bytes" "latin" "empty" "reft" "sha" "split"))
          (multiple-value-bind (text ok) (run name '("status" "--porcelain"))
            (is eq t ok "~a: ~a" name text)
            (true (search "a.txt" text) "~a" name)))
        (multiple-value-bind (text ok) (run "reft" '("tag" "-l"))
          (true ok) (true (search "v1" text)))
        (is eq t (nth-value 1 (run "reft" '("stash" "list"))))
        (multiple-value-bind (text ok) (run "sha" '("log" "-1" "--format=%H"))
          (true ok) (is = 65 (length text)))
        (is = 3 (count #\Newline (run "grafts" '("log" "--oneline"))))
        (true (search "second" (run "replaced" '("log" "-1" "--format=%s"))))
        (is string= (format nil "Test~%") (run "mailmap" '("log" "-1" "--format=%an")))
        (false (search "Forged" (run "mailmap" '("shortlog" "-se" "HEAD"))))
        (false (search "Forged" (run "mailmap" '("blame" "--porcelain" "--" "a.txt"))))
        (true (search "Test" (run "mailmap" '("blame" "--porcelain" "--" "a.txt"))))
        (true (search "--contents" (format nil "~s" (tuatara.git::git-blame-raw-words '("blame" "--porcelain" "--" "a.txt") (repo "mailmap") (repo "mailmap")))))
        ;; refused in one sentence, never read through the repository's own folder
        (dolist (name '("idxfifo" "idxlink" "unknown"))
          (is eq :refused (nth-value 1 (run name '("status" "--porcelain"))) "~a" name))
        (true (search "index is not a file" (run "idxfifo" '("status" "--porcelain"))))
        (true (search "index is not a file" (run "idxlink" '("status" "--porcelain"))))
        (true (search "extension someday" (run "unknown" '("status" "--porcelain"))))
        ;; a folder with an empty .git of its own inside a repository is no way to its settings
        (is eq :refused (nth-value 1 (run "nest/X" '("status" "--porcelain"))))
        ;; a bare repository inside a trusted one is never taken for it
        (let ((tuatara-user:*lsp-trusted-projects* (list (repo "holder"))))
          (true (tuatara.defaults::project-trusted-p (repo "holder")))
          (run "holder/inner.git" '("status" "--porcelain"))
          (run "holder/inner.git" '("log")))
        (is equal '() (uiop:directory-files marks))))))

(define-test (git look-only-wins)
  ;; the answer that runs less wins: a folder answered look only inside a trusted one, or named both ways, is looked at only
  (with-test-directory (dir)
    (let* ((outer (namestring dir))
           (inner (namestring (ensure-directories-exist (merge-pathnames "case/" dir))))
           (deeper (namestring (ensure-directories-exist (merge-pathnames "case/sub/" dir))))
           (tuatara-user:*lsp-trusted-projects* (list outer)))
      (true (tuatara.defaults::project-trusted-p inner))
      (with-looked-at-only (inner)
        (false (tuatara.defaults::project-trusted-p inner))
        (false (tuatara.defaults::project-trusted-p deeper))
        (true (tuatara.defaults::project-look-only-reason deeper))
        (let ((tuatara-user:*lsp-trusted-projects* '()))
          (tuatara.defaults::state-list-put tuatara.defaults::*lsp-trusted* (list (tuatara.defaults::real-folder inner)))
          (unwind-protect (false (tuatara.defaults::project-trusted-p inner))
            (tuatara.defaults::state-list-drop tuatara.defaults::*lsp-trusted* (list (tuatara.defaults::real-folder inner)))))))))

(define-test (git reading-words-parsed)
  ;; options are read as git reads them: clusters, values in the next word, subcommands after options; anything not listed is refused
  (dolist (words '(("notes" "--ref" "list" "add" "-m" "pwn" "HEAD") ("hash-object" "-wt" "blob" "a.txt")
                   ("hash-object" "--stdin-paths" "-w") ("grep" "-iOtouch /tmp/x" "one") ("grep" "-O" "one")
                   ("for-each-ref" "--format" "%(signature)") ("tag" "-l" "--format" "%(signature)")
                   ("log" "--pretty" "format:%G?") ("log" "--format" "%aN") ("branch" "-l" "--set-upstream-to=x")
                   ("branch" "--list" "-Dr" "x") ("tag" "-l" "-dfoo") ("tag" "-l" "x" "-d") ("config" "--get" "core.bare" "--add" "x.y")
                   ("config" "-e") ("config" "--edi") ("config" "--get" "--file=/tmp/x" "a.b") ("config" "set" "a.b" "x")
                   ("worktree" "lock" "x") ("reflog" "expire" "--all") ("reflog" "--expire=now") ("remote" "-v" "set-url" "o" "x")
                   ("remote" "show" "origin") ("remote" "update") ("log" "--outp" "/tmp/x") ("diff" "--ext") ("cat-file" "--batch-command")
                   ("grep" "--open-files") ("blame" "--contents" "/etc/shadow" "a.txt") ("log" "--use-mailmap") ("shortlog" "--format=%aN")
                   ("branch" "--edit-desc") ("notes" "add") ("reflog" "-n1" "expire") ("worktree" "list" "lock")))
    (true (tuatara.git::git-reading-refusal words) "~s" words))
  (dolist (words '(("rev-parse" "--git-dir") ("rev-parse" "--absolute-git-dir") ("log" "--format" "%H %an") ("log" "-p" "-1")
                   ("tag") ("notes" "list") ("remote" "-v") ("check-attr" "-a" "a.txt") ("shortlog" "-s") ("worktree" "list" "--porcelain")
                   ("log" "--" "--output=x") ("log" "--end-of-options" "--output=x")))
    (false (tuatara.git::git-reading-refusal words) "~s" words)))

(define-test (git look-only-ignores-your-git-variables)
  ;; GIT_ variables of the editor's own environment never reach git in a repository not trusted
  (with-test-directory (dir)
    (with-test-directory (out)
      (scratch-repository dir :trusted nil)
      (let ((marker (namestring (merge-pathnames "ran" out)))
            (tuatara-user:*lsp-trusted-projects* '()))
        (unwind-protect
             (progn
               (sb-posix:setenv "GIT_CONFIG_PARAMETERS" (format nil "'core.fsmonitor'='touch ~a-fsmonitor'" marker) 1)
               (sb-posix:setenv "GIT_EXTERNAL_DIFF" (format nil "touch ~a-external" marker) 1)
               (sb-posix:setenv "GIT_DIR" "/nonexistent" 1)
               (alexandria:write-string-into-file (format nil "one~%changed~%") (merge-pathnames "a.txt" dir) :if-exists :supersede)
               (true (nth-value 1 (tuatara.git:run-git (namestring dir) '("status" "--porcelain"))))
               (true (nth-value 1 (tuatara.git:run-git (namestring dir) '("diff")))))
          (dolist (name '("GIT_CONFIG_PARAMETERS" "GIT_EXTERNAL_DIFF" "GIT_DIR")) (sb-posix:unsetenv name)))
        (is equal '() (uiop:directory-files out))))))

(define-test (git shadows-of-ended-sessions-deleted)
  ;; shadows a session left (it ended without deleting them, before it named its machine) are deleted an hour on
  (let ((left (tuatara.defaults::private-cache-directory "git-shadow/999999999/")))
    (ensure-directories-exist (merge-pathnames "x/index" left))
    (let ((then (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0) 7200)))
      (sb-posix:utimes left then then))
    (tuatara.git::forget-old-git-shadows)
    (false (tuatara.io:path-kind left))))

(defun shadows-size ()
  "The octets every shadow of git in the private cache holds on the disk."
  (parse-integer (string-trim '(#\Newline #\Space)
                              (uiop:run-program (list "sh" "-c" "du -sk \"$1\" | cut -f1" "size"
                                                      (tuatara.defaults::private-cache-directory "git-shadow/"))
                                                :output :string :ignore-error-status t))
                 :junk-allowed t))

(define-test (git hostile-repositories-second-round)
  ;; holes, links and named pipes among the references, alternates and packs, .git leading elsewhere, a trusted
  ;; worktree of a repository looked at only, blame of what is not a file: nothing runs, nothing changes, the cache
  ;; does not fill, nothing waits on a pipe, and what is read from elsewhere is said
  (with-test-directory (dir)
    (let* ((repos (namestring (merge-pathnames "r/" dir)))
           (marks (namestring (ensure-directories-exist (merge-pathnames "m/" dir))))
           (tuatara-user:*lsp-trusted-projects* '()))
      (uiop:run-program (list "sh" (namestring (asdf:system-relative-pathname :tuatara "test/git-look-only/hostile-repositories.sh"))
                              repos marks)
                        :output nil :error-output nil)
      (flet ((repo (name) (concatenate 'string repos name "/"))
             (run (name arguments)
               (handler-case (multiple-value-bind (octets ok) (tuatara.git:run-git (concatenate 'string repos name "/") arguments)
                               (values (and octets (tuatara.text:lenient-string octets)) ok))
                 (tuatara.git::git-refused (e) (values (princ-to-string e) :refused)))))
        (let ((before (shadows-size)))
          (dolist (name '("huge" "hugelog" "reflinks" "alts" "altfifo" "packfifo" "objlink" "gitlink" "gitfile" "common"
                          "blamez" "blamef" "blamel" "cfg" "cfg0"))
            (let ((snapshot (repository-snapshot (repo name)))
                  (start (get-internal-real-time)))
              (dolist (arguments *hostile-commands*)
                (run name arguments))
              (true (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)) "~a took too long" name)
              (is string= snapshot (repository-snapshot (repo name)) "~a changed" name)))
          ;; holes are never copied out: the cache took no gigabytes
          (true (< (- (shadows-size) before) (* 64 1024)) "the shadows grew by ~d KiB" (- (shadows-size) before)))
        (true (search "has holes" (run "huge" '("status" "--porcelain"))))
        (true (search "has holes" (run "hugelog" '("reflog"))))
        (is eq t (nth-value 1 (run "hugelog" '("status" "--porcelain"))) "a log is copied only for a command that reads it")
        (dolist (name '("altfifo" "alts" "packfifo"))
          (is eq :refused (nth-value 1 (run name '("rev-parse" "HEAD"))) "~a" name))
        (false (search "SECRET" (or (run "reflinks" '("for-each-ref")) "")))
        ;; read from elsewhere: said
        (is eq t (nth-value 1 (run "objlink" '("log" "-1"))))
        (true (search "objlink-objects" (format nil "~a" (tuatara.git::git-shadow-elsewhere-of (repo "objlink")))))
        (dolist (name '("gitlink" "gitfile" "common"))
          (true (tuatara.git::git-folder-elsewhere (repo name)) "~a" name))
        ;; blame of a link is of its own text; of a pipe refused; nothing outside shown as the file
        (is eq t (nth-value 1 (run "blamez" '("blame" "--porcelain" "--" "z.txt"))))
        (is eq :refused (nth-value 1 (run "blamef" '("blame" "--porcelain" "--" "f.txt"))))
        (false (search "SECRET" (or (run "blamel" '("blame" "--porcelain" "--" "s.txt")) "")))
        ;; a version 0 repository's extensions are not read, as git reads none
        (is eq t (nth-value 1 (run "cfg0" '("log" "-1"))))
        (is eq :refused (nth-value 1 (run "cfg" '("log" "-1"))))
        ;; a setting given twice: the last one counts, as git takes it
        (is eq :refused (nth-value 1 (run "cfgtwice" '("log" "-1"))))
        ;; a trusted folder whose .git leads into a repository looked at only reads it only
        (with-looked-at-only ((repo "lookedat"))
          (let ((snapshot (repository-snapshot (repo "lookedat")))
                (tuatara-user:*lsp-trusted-projects* (list (repo "trustedwt"))))
            (true (tuatara.git::git-reads-only-p (repo "trustedwt")))
            (run "trustedwt" '("status" "--porcelain"))
            (run "trustedwt" '("diff"))
            (is string= snapshot (repository-snapshot (repo "lookedat")))))
        (is equal '() (uiop:directory-files marks))))))

(define-test (git shadow-not-walked-again)
  ;; once a repository of 20,000 tags has its shadow, a command looks at a few folders, not at every reference
  (with-test-directory (dir)
    (scratch-repository dir :trusted nil)
    (let ((hash (string-trim '(#\Newline) (git-output dir "rev-parse" "HEAD")))
          (tags (ensure-directories-exist (merge-pathnames ".git/refs/tags/" dir)))
          (tuatara-user:*lsp-trusted-projects* '())
          (walks 0))
      (dotimes (i 20000)
        (with-open-file (out (merge-pathnames (format nil "t~d" i) tags) :direction :output)
          (write-line hash out)))
      (tuatara.git:run-git (namestring dir) '("rev-parse" "HEAD"))
      (sb-int:encapsulate 'tuatara.git::git-tree-entries 'count-walks
                          (lambda (function &rest arguments) (incf walks) (apply function arguments)))
      (unwind-protect
           (let ((start (get-internal-real-time)))
             (dotimes (i 10) (true (nth-value 1 (tuatara.git:run-git (namestring dir) '("rev-parse" "HEAD")))))
             (is = 0 walks)
             (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second))))
        (sb-int:unencapsulate 'tuatara.git::git-tree-entries 'count-walks)))))

(define-test (git blame-values-read)
  ;; blame -L 1,5 a.txt: 1,5 is the value of -L, not a revision; the file is blamed as it is now
  (with-test-directory (dir)
    (scratch-repository dir :trusted nil)
    (let ((words (tuatara.git::git-blame-raw-words '("blame" "-L" "1,5" "a.txt") (namestring dir) (namestring dir))))
      (true (member "--contents" words :test #'equal) "~s" words)
      (is equal "a.txt" (car (last words))))))

(define-test (git live-sessions-shadows-kept)
  ;; a folder of shadows whose owner.lock a running session holds is never taken for one left behind
  (let* ((folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "git-shadow/") :hold nil))
         (fd (progn (tuatara.io:write-private-file (tuatara.io:path-join folder "owner.lock") (machine-instance))
                    (sb-posix:open (tuatara.io:path-join folder "owner.lock") sb-posix:o-rdwr))))
    (unwind-protect
         (progn
           (true (tuatara.runtime:lock-fd fd))
           ;; another descriptor's lock: as another session's
           (let ((other (sb-posix:open (tuatara.io:path-join folder "owner.lock") sb-posix:o-rdwr)))
             (false (tuatara.runtime:lock-fd other))
             (sb-posix:close other))
           (tuatara.git::forget-old-git-shadows)
           (true (tuatara.io:path-kind folder)))
      (sb-posix:close fd))
    (tuatara.git::forget-old-git-shadows)
    (false (tuatara.io:path-kind folder))))

(define-test (git status-says-names-escaped)
  ;; a folder name holding a newline, ESC and a right-to-left mark reaches the status's top escaped: one line each, no forged line
  (with-test-directory (dir)
    (let* ((odd (concatenate 'string (namestring dir) (format nil "odd~%Look only: none~c31m~c/" (code-char 27) (code-char #x202E))))
           (repo (namestring (ensure-directories-exist (merge-pathnames "repo/" dir))))
           (tuatara-user:*lsp-trusted-projects* '()))
      (ensure-directories-exist odd)
      (scratch-repository odd :trusted nil)
      (scratch-repository repo :trusted nil)
      (uiop:delete-directory-tree (pathname (concatenate 'string repo ".git/")) :validate t)
      (alexandria:write-string-into-file (format nil "gitdir: ~a.git~%" odd) (concatenate 'string repo ".git"))
      (with-looked-at-only (repo)
        (let* ((top (tuatara.git:repository-top repo))
               (notes (tuatara.git::git-elsewhere-notes top))
               (buffer (tuatara.git::git-status-buffer top)))
          (unwind-protect
               (progn
                 (true (find #\Newline (first notes)) "the name as it is: ~s" notes)
                 (setf (tuatara.buffer:buffer-local buffer 'tuatara.git::git-elsewhere) notes
                       (tuatara.buffer:buffer-local buffer 'tuatara.git::git-look-only)
                       (format nil "it is not trusted~%Look only: none~c" (code-char #x202E)))
                 (tuatara.git::git-status-render buffer 0)
                 (let* ((text (tuatara.buffer:buffer-string buffer))
                        (lines (uiop:split-string text :separator '(#\Newline))))
                   (true (uiop:string-prefix-p "Look only: " (first lines)))
                   (true (uiop:string-prefix-p "Git's own folder: " (second lines)) "~s" (second lines))
                   (is = 1 (count-if (lambda (line) (uiop:string-prefix-p "Look only: " line)) lines))
                   (false (find (code-char 27) text))
                   (false (find (code-char #x202E) text))
                   (true (search "<202E>" text))))
            (tuatara.buffer:kill-buffer buffer)))))))

(define-test (git written-over-in-place-seen-when-asked)
  ;; a reference written over in place, its folder untouched, is seen once the status is read again when asked
  (with-test-directory (dir)
    (scratch-repository dir :trusted nil)
    (git-output dir "commit" "-q" "--allow-empty" "-m" "Second")
    (let ((top (namestring dir))
          (first (string-trim '(#\Newline) (git-output dir "rev-parse" "HEAD~1")))
          (tuatara-user:*lsp-trusted-projects* '()))
      (flet ((main () (string-trim '(#\Newline) (tuatara.text:octets-string (tuatara.git:run-git top '("rev-parse" "refs/heads/main"))))))
        (main)
        (with-open-file (out (merge-pathnames ".git/refs/heads/main" dir) :direction :output :if-exists :overwrite)
          (write-line first out))
        (tuatara.git::forget-git-shadow top)
        (is string= first (main))))))

(define-test (git holes-found-where-they-are)
  ;; holes are found by where the data is, not by the blocks a file takes: a file with a hole is told, one without is not
  (with-test-directory (dir)
    (let ((sparse (namestring (merge-pathnames "sparse" dir)))
          (plain (namestring (merge-pathnames "plain" dir))))
      (uiop:run-program (list "truncate" "-s" "8M" sparse))
      (alexandria:write-string-into-file (make-string 100000 :initial-element #\x) plain)
      (true (tuatara.io:file-holes-p sparse))
      (false (tuatara.io:file-holes-p plain))
      (is = (* 256 1024 1024) (tuatara.git::git-file-most "shallow")))))

(define-test (git timed-out-left-alone)
  ;; git ending past its time in a repository not trusted is not run there again this session, until asked
  (with-test-directory (dir)
    (scratch-repository dir :trusted nil)
    (let* ((top (namestring dir))
           (blob (string-trim '(#\Newline) (git-output dir "rev-parse" "HEAD:a.txt")))
           (object (merge-pathnames (format nil ".git/objects/~a/~a" (subseq blob 0 2) (subseq blob 2)) dir))
           (tuatara-user:*lsp-trusted-projects* '()))
      (unwind-protect
           (progn
             (delete-file object)
             (sb-posix:mkfifo (namestring object) #o600)
             (false (nth-value 1 (tuatara.git:run-git top (list "cat-file" "-p" (format nil "~a" blob)) :seconds 1)))
             (true (gethash (tuatara.git:repository-top top) tuatara.git::*git-timed-out*))
             (let ((start (get-internal-real-time)))
               (false (nth-value 1 (tuatara.git:run-git top '("rev-parse" "HEAD"))))
               (true (< (- (get-internal-real-time) start) (/ internal-time-units-per-second 2))))
             (tuatara.git::forget-git-shadow (tuatara.git:repository-top top))
             (true (nth-value 1 (tuatara.git:run-git top '("rev-parse" "HEAD")))))
        (remhash (tuatara.git:repository-top top) tuatara.git::*git-timed-out*)))))

(define-test (git other-machines-shadows-kept)
  ;; a folder of shadows another machine's session made is never deleted here; one of this machine's, ended, is
  (flet ((made (host)
           (let ((folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "git-shadow/") :hold nil)))
             (tuatara.io:write-private-file (tuatara.io:path-join folder "owner.lock") host)
             folder)))
    (let ((elsewhere (made "another-machine.example"))
          (here (made (machine-instance))))
      (unwind-protect
           (progn (tuatara.git::forget-old-git-shadows)
                  (true (tuatara.io:path-kind elsewhere))
                  (false (tuatara.io:path-kind here)))
        (ignore-errors (tuatara.io:delete-tree elsewhere))))))

(define-test (git evidence-refused-by-variables)
  ;; GIT_DIR, GIT_WORK_TREE or GIT_INDEX_FILE in the editor's own environment, the one git is given, naming an
  ;; evidence folder: the command that would write there is refused, never run
  (with-test-directory (dir)
    (let* ((ev (namestring (ensure-directories-exist (merge-pathnames "ev/" dir))))
           (work (namestring (ensure-directories-exist (merge-pathnames "work/" dir)))))
      (scratch-repository work)
      (scratch-repository ev)
      (with-evidence-folders ((list ev))
        (dolist (setting (list (cons "GIT_DIR" (concatenate 'string ev ".git"))
                               (cons "GIT_WORK_TREE" ev)
                               (cons "GIT_INDEX_FILE" (concatenate 'string ev ".git/index"))))
          (sb-posix:setenv (car setting) (cdr setting) 1)
          (unwind-protect
               (true (handler-case (progn (tuatara.git:run-git work '("add" "a.txt")) nil)
                       (tuatara.io:writing-refused () t))
                     "~a into evidence not refused" (car setting))
            (sb-posix:unsetenv (car setting))))))))
