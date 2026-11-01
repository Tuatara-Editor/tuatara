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
            (multiple-value-bind (octets ok) (tuatara.defaults::run-git top arguments)
              (true ok "~s" arguments)
              (true octets)))
          (multiple-value-bind (octets ok) (tuatara.defaults::run-git top '("diff"))
            (true ok)
            ;; the change itself, not a program's version of it
            (true (search "+changed" (tuatara.text:octets-string octets))))
          (is equal '() (markers marker)))
        ;; the same repository trusted: its programs do run, so the settings above were in force
        (let ((tuatara-user:*lsp-trusted-projects* (list top)))
          (tuatara.defaults::run-git top '("status" "--porcelain"))
          (tuatara.defaults::run-git top '("diff"))
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
    (true (tuatara.defaults::git-reading-refusal words) "~s" words))
  (dolist (words '(("status" "--porcelain=v2" "--branch" "-z") ("log" "-n10" "--format=%H") ("show" "HEAD")
                   ("diff" "--cached" "-M" "--" "a.txt") ("blame" "a.txt") ("ls-files" "-z") ("branch" "--show-current")
                   ("branch" "-a" "-v") ("tag" "-l") ("stash" "list") ("stash" "show" "-p" "stash@{0}") ("remote")
                   ("remote" "get-url" "origin") ("config" "--get" "user.name") ("rev-parse" "--absolute-git-dir")
                   ("for-each-ref" "--format=%(refname:short)") ("cat-file" "-p" "HEAD") ("reflog" "show")
                   ("worktree" "list" "--porcelain") ("--no-pager" "log" "-1") ("log" "--" "--output=x")))
    (false (tuatara.defaults::git-reading-refusal words) "~s" words))
  ;; and run-git refuses, in one sentence
  (with-test-directory (dir)
    (scratch-repository dir)
    (with-looked-at-only (dir)
      (let ((before (git-output dir "log" "--format=%H")))
        (handler-case (progn (tuatara.defaults::run-git (namestring dir) '("commit" "--allow-empty" "-m" "x")) (false t))
          (tuatara.defaults::git-refused (e)
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
           (top (tuatara.defaults::repository-top (namestring dir)))
           (real (tuatara.defaults::real-folder top)))
      (flet ((status () (tuatara.defaults::git-buffer-of top 'tuatara.defaults::git-status-mode)))
        (unwind-protect
             (let ((f (auto-save-session
                       (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.txt" dir))))
                             "C-x g" (list :wait 0.1) "l"
                             (list :until (lambda () (and (status) (tuatara.buffer:buffer-local (status) 'tuatara.defaults::git-state))))
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
           (top (tuatara.defaults::repository-top (namestring dir)))
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
              (true (nth-value 1 (tuatara.defaults::run-git seized arguments)) "~s" arguments))
            (true (refused-any (tuatara.defaults::run-git seized '("checkout" "--" "a.txt")))))
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
               (handler-case (multiple-value-bind (octets ok) (tuatara.defaults::run-git (concatenate 'string repos name "/") arguments :seconds 20)
                               (values (and octets (tuatara.text:lenient-string octets)) ok))
                 (tuatara.defaults::git-refused (e) (values (princ-to-string e) :refused)))))
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
        (true (search "--contents" (format nil "~s" (tuatara.defaults::git-blame-raw-words '("blame" "--porcelain" "--" "a.txt") "/r/" "/r/"))))
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
    (true (tuatara.defaults::git-reading-refusal words) "~s" words))
  (dolist (words '(("rev-parse" "--git-dir") ("rev-parse" "--absolute-git-dir") ("log" "--format" "%H %an") ("log" "-p" "-1")
                   ("tag") ("notes" "list") ("remote" "-v") ("check-attr" "-a" "a.txt") ("shortlog" "-s") ("worktree" "list" "--porcelain")
                   ("log" "--" "--output=x") ("log" "--end-of-options" "--output=x")))
    (false (tuatara.defaults::git-reading-refusal words) "~s" words)))

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
               (true (nth-value 1 (tuatara.defaults::run-git (namestring dir) '("status" "--porcelain"))))
               (true (nth-value 1 (tuatara.defaults::run-git (namestring dir) '("diff")))))
          (dolist (name '("GIT_CONFIG_PARAMETERS" "GIT_EXTERNAL_DIFF" "GIT_DIR")) (sb-posix:unsetenv name)))
        (is equal '() (uiop:directory-files out))))))

(define-test (git shadows-of-ended-sessions-deleted)
  ;; shadows a session left (it ended without deleting them) are deleted when the next one starts
  (let ((left (tuatara.defaults::private-cache-directory "git-shadow/999999999/")))
    (ensure-directories-exist (merge-pathnames "x/index" left))
    (tuatara.defaults::forget-old-git-shadows)
    (false (tuatara.io:path-kind left))))
