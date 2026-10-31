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
    (scratch-repository dir)
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
                             "M-x t r u s t - p r o j e c t RET" (list :wait 0.1) "y" (list :wait 0.1))
                       :width 160 :height 30)))
               (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "(y, l or n)"))
               (true (tuatara.defaults::project-trusted-p top))
               (false (tuatara.defaults::project-look-only-reason top))))
        (tuatara.defaults::state-list-drop tuatara.defaults::*lsp-trusted* (list real))))))

(define-test (git look-only-list-keeps-unknown-lines)
  ;; the list of projects looked at only says its version, and keeps a line it cannot read as it is
  (is equal '(:kept "later kind of line") (tuatara.defaults::parse-look-only-line "later kind of line"))
  (is string= "later kind of line" (tuatara.defaults::look-only-line '(:kept "later kind of line")))
  (false (tuatara.defaults::parse-look-only-line "# tuatara look-only 1"))
  (is string= "/a/b/" (tuatara.defaults::parse-look-only-line "/a/b/"))
  (true (search "# tuatara look-only 1" (tuatara.defaults::state-list-header tuatara.defaults::*look-only*))))

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
