;;;; test/tool-fences.lisp --- The fence the tools put round the developer's own files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (tools fence-sets-every-person-variable)
  ;; a Lisp of its own loads the fence and says where each variable then points
  (let* ((scratch (merge-pathnames (format nil "fence-~d/" (random 1000000)) *test-state-directory*))
         (variables '("HOME" "XDG_CONFIG_HOME" "XDG_STATE_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME" "GNUPGHOME"
                     "XDG_RUNTIME_DIR" "TMPDIR"))
         (lines (uiop:split-string
                 (uiop:run-program
                  (list (namestring sb-ext:*runtime-pathname*) "--non-interactive" "--no-userinit" "--no-sysinit"
                        "--load" (namestring (asdf:system-relative-pathname :tuatara "tools/fence.lisp"))
                        "--eval" (format nil "(tuatara.fence:fence-person-data ~s)" (namestring scratch))
                        "--eval" (format nil "(dolist (name '~s) (format t \"~~a=~~a~~%\" name (sb-posix:getenv name)))"
                                         variables))
                  :output :string)
                 :separator '(#\Newline))))
    (dolist (name variables)
      (let ((line (find-if (lambda (line) (uiop:string-prefix-p (format nil "~a=" name) line)) lines)))
        (true line "~a is said" name)
        (when line
          (true (uiop:subpathp (uiop:ensure-directory-pathname (subseq line (1+ (length name)))) scratch)
                "~a is under the fence: ~a" name line))))
    (is = #o700 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames "gnupg/" scratch))))))
    (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore)))

(define-test (tools fence-closes-the-person-agents)
  ;; a Lisp of its own given the variables of a person's agents, display, session bus and a repository of
  ;; theirs loads the fence and has none of them after, its Kerberos tickets and git settings in the fence; nor
  ;; has the tests' own run
  (let* ((scratch (merge-pathnames (format nil "fence-~d/" (random 1000000)) *test-state-directory*))
         (variables '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "DBUS_SESSION_BUS_ADDRESS" "DISPLAY"
                      "WAYLAND_DISPLAY" "XAUTHORITY" "TMUX" "GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE"
                      "GIT_OBJECT_DIRECTORY" "GIT_COMMON_DIR" "GIT_CONFIG_PARAMETERS"))
         (output (with-output-to-string (out)
                   (sb-ext:run-program
                    sb-ext:*runtime-pathname*
                    (list "--non-interactive" "--no-userinit" "--no-sysinit"
                          "--load" (namestring (asdf:system-relative-pathname :tuatara "tools/fence.lisp"))
                          "--eval" (format nil "(tuatara.fence:fence-person-data ~s)" (namestring scratch))
                          "--eval" (format nil "(dolist (name '~s) (format t \"~~a=~~a~~%\" name ~
                                                (sb-posix:getenv name)))"
                                           (list* "KRB5CCNAME" "GIT_CONFIG_GLOBAL" variables)))
                    :output out :error nil
                    :environment (append (mapcar (lambda (name) (format nil "~a=/run/owner/~(~a~)" name name))
                                                 (cons "KRB5CCNAME" variables))
                                         (remove-if (lambda (pair)
                                                      (find-if (lambda (name)
                                                                 (uiop:string-prefix-p (format nil "~a=" name) pair))
                                                               (list* "KRB5CCNAME" "GIT_CONFIG_GLOBAL" variables)))
                                                    (sb-ext:posix-environ)))))))
    (dolist (name variables)
      (true (search (format nil "~a=NIL" name) output) "~a is unset in the fence: ~a" name output)
      (false (sb-posix:getenv name) "~a is unset in the tests' run" name))
    ;; Kerberos tickets in a file of the fence, not the person's keyring
    (true (search (format nil "KRB5CCNAME=FILE:~akrb5cc" (namestring scratch)) output) "~a" output)
    (true (search (format nil "GIT_CONFIG_GLOBAL=~agitconfig" (namestring scratch)) output) "~a" output)
    (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore)))

(define-test (tools fence-commits-under-the-test-name)
  ;; a Lisp of its own given a person's name, address and programs for git loads the fence and commits in a
  ;; repository of its own: the commit is the fence's Test, and none of those programs is named after
  (let* ((scratch (merge-pathnames (format nil "fence-~d/" (random 1000000)) *test-state-directory*))
         (repository (namestring (merge-pathnames "repository/" scratch)))
         (variables '("GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_NAME" "GIT_COMMITTER_EMAIL" "EMAIL"
                      "GIT_SSH_COMMAND" "GIT_SSH" "GIT_ASKPASS" "SSH_ASKPASS" "GIT_EXEC_PATH" "GIT_TEMPLATE_DIR"))
         (output (with-output-to-string (out)
                   (sb-ext:run-program
                    sb-ext:*runtime-pathname*
                    (list "--non-interactive" "--no-userinit" "--no-sysinit"
                          "--load" (namestring (asdf:system-relative-pathname :tuatara "tools/fence.lisp"))
                          "--eval" (format nil "(tuatara.fence:fence-person-data ~s)" (namestring scratch))
                          "--eval" (format nil "(dolist (name '~s) (format t \"~~a=~~a~~%\" name ~
                                                (sb-posix:getenv name)))"
                                           variables)
                          "--eval" (format nil "(uiop:run-program '(\"git\" \"init\" \"-q\" ~s))" repository)
                          "--eval" (format nil "(uiop:run-program '(\"git\" \"-C\" ~s \"commit\" \"-q\" ~
                                                \"--allow-empty\" \"-m\" \"made in the fence\"))"
                                           repository)
                          "--eval" (format nil "(uiop:run-program '(\"git\" \"-C\" ~s \"log\" \"-1\" ~
                                                \"--format=commit %an <%ae> %cn <%ce>\") :output t)"
                                           repository))
                    :output out :error nil
                    :environment (append (mapcar (lambda (name)
                                                   (format nil "~a=~a" name
                                                           (cond ((search "NAME" name) "Outside")
                                                                 ((search "EMAIL" name) "owner@example.org")
                                                                 (t (format nil "/run/owner/~(~a~)" name)))))
                                                 variables)
                                         (remove-if (lambda (pair)
                                                      (find-if (lambda (name)
                                                                 (uiop:string-prefix-p (format nil "~a=" name) pair))
                                                               variables))
                                                    (sb-ext:posix-environ)))))))
    (true (search "commit Test <test@example.invalid> Test <test@example.invalid>" output) "~a" output)
    (dolist (name variables)
      (true (search (format nil "~a=NIL" name) output) "~a is unset in the fence: ~a" name output))
    (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore)))

(define-test (tools fence-moves-the-state-files)
  ;; a fence made while the editor is loaded, as a manual builder makes one inside the tests, has the
  ;; lists kept between sessions read and written in the fence from then on, never in the state
  ;; directory before it
  (let* ((scratch (merge-pathnames (format nil "fence-~d/" (random 1000000)) *test-state-directory*))
         (list tuatara.defaults::*saved-patterns*)
         (variables (cons "TUATARA_FENCE" (mapcar #'car tuatara.fence:*fenced-variables*)))
         (was (mapcar #'sb-posix:getenv variables))
         (config tuatara.defaults:*config-file*)
         (temporary uiop:*temporary-directory*))
    (tuatara.defaults::state-list-path list :known t)
    (unwind-protect
         (progn
           (tuatara.fence:fence-person-data scratch)
           (let ((path (tuatara.defaults::state-list-path list :known t)))
             (true (uiop:subpathp (uiop:parse-native-namestring path) scratch) "~a not in ~a" path scratch)))
      (loop for name in variables for value in was
            do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name)))
      (setf tuatara.defaults:*config-file* config uiop:*temporary-directory* temporary)
      (tuatara.defaults::forget-state-paths)
      (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore))))

(define-test (tools fence-gives-gnupg-an-agent-of-its-own)
  ;; GnuPG in the tests' run talks to an agent at a socket of its own, never the developer's in
  ;; /run/user/UID/gnupg/, which a fenced home's .gnupg would share; and a fence asked to put GNUPGHOME there, or
  ;; anywhere in the fenced home, however it is spelt, is refused
  (let ((gpgconf (ignore-errors (uiop:run-program '("gpgconf" "--list-dirs" "agent-socket")
                                                  :output '(:string :stripped t) :error-output nil))))
    (if (null gpgconf)
        (skip "no gpgconf here" (true nil))
        (true (search "/d." gpgconf) "the tests' agent socket is ~a" gpgconf)))
  (dolist (gnupg '("home/.gnupg/" "home/.gnupg" "home/../home/.gnupg/" "./home/keys/"))
    (true (search ".gnupg"
                  (handler-case (progn (tuatara.fence:fence-person-data
                                        (merge-pathnames (format nil "gnupg-~d/" (random 1000000))
                                                         *test-state-directory*)
                                        :places `(("HOME" . "home/") ("GNUPGHOME" . ,gnupg)))
                                       "")
                    (error (condition) (princ-to-string condition))))
          "GNUPGHOME ~a" gnupg)))

(define-test (tools tests-use-the-tools-fence)
  ;; the tests' own fence is the one the tools load, not a copy of it
  (true (search "tools/fence.lisp" (uiop:read-file-string (asdf:system-relative-pathname :tuatara "test/fences.lisp"))))
  (true (uiop:subpathp (uiop:ensure-directory-pathname (sb-posix:getenv "XDG_CONFIG_HOME")) *test-state-directory*))
  (true (uiop:subpathp (uiop:ensure-directory-pathname (sb-posix:getenv "GNUPGHOME")) *test-state-directory*))
  (false (probe-file tuatara.defaults:*config-file*)))

(define-test (tools real-home-guard-sees-every-route)
  ;; each way the editor opens, saves, examines, changes or lists a file is seen when it names one in the
  ;; developer's own home, and refused before the system is asked: the name is one no file system takes (a
  ;; directory name longer than any may be), so that nothing could be made there even were it asked
  (let* ((home (string-right-trim "/" (first *real-home-names*)))
         (directory (format nil "~a/~a/" home (make-string 300 :initial-element #\q)))
         (path (concatenate 'string directory "never.txt"))
         (elsewhere (namestring (merge-pathnames "guard-link" *test-state-directory*)))
         (octets (tuatara.text:string-octets path))
         (routes
           (list (cons "visit" (lambda () (tuatara.io:visit-file path)))
                 (cons "save" (lambda ()
                                (let ((buffer (tuatara.buffer:make-buffer "guard-probe" :text "text")))
                                  (unwind-protect (tuatara.io:save-buffer-file buffer :path path :force t)
                                    (tuatara.buffer:kill-buffer buffer)))))
                 (cons "open" (lambda () (with-open-file (in path) (read-line in))))
                 (cons "sb-posix open" (lambda () (sb-posix:open path sb-posix:o-rdonly)))
                 (cons "openat" (lambda () (tuatara.io::openat-octets tuatara.io::+at-fdcwd+ octets 0)))
                 (cons "stamp" (lambda () (tuatara.io:path-stamp path)))
                 (cons "times" (lambda () (tuatara.io::path-times path)))
                 (cons "real path" (lambda () (tuatara.io::real-path path)))
                 (cons "link" (lambda () (tuatara.io::symlink-target path)))
                 (cons "file system" (lambda () (tuatara.io::statvfs-fields path 0)))
                 (cons "mode" (lambda () (tuatara.io::set-path-mode path #o600)))
                 (cons "attribute" (lambda () (tuatara.io::path-xattr-p path "user.probe")))
                 (cons "watch" (lambda ()
                                 (let ((fd (tuatara.io::inotify-open)))
                                   (unwind-protect (tuatara.io::inotify-watch fd directory)
                                     (sb-posix:close fd)))))
                 (cons "hard link" (lambda () (tuatara.io::make-hard-link path elsewhere)))
                 (cons "make directory" (lambda () (tuatara.io::make-directory directory)))
                 (cons "remove" (lambda () (tuatara.io::remove-file path)))
                 (cons "rename" (lambda () (sb-posix:rename path elsewhere)))
                 (cons "unlink" (lambda () (sb-posix:unlink path)))
                 (cons "mkdir" (lambda () (sb-posix:mkdir directory #o700)))
                 (cons "stat" (lambda () (sb-posix:stat path)))
                 (cons "readlink" (lambda () (sb-posix:readlink path))))))
    (dolist (route routes)
      (let* ((before *real-home-reached*)
             (outcome (handler-case (progn (funcall (cdr route)) :done)
                        (error (condition) condition)))
             (reached (ldiff *real-home-reached* before)))
        ;; the records are this test's own, not a failure of the run
        (sb-thread:with-mutex (*real-home-lock*)
          (setf *real-home-reached* (set-difference *real-home-reached* reached)))
        ;; the file's own name, or the directory it is in, which is opened first
        (true (find-if (lambda (record) (uiop:string-prefix-p home (second record))) reached)
              "the guard did not see ~a" (car route))
        ;; refused as the system refuses one who may not look (EACCES), never the system's own answer
        (false (and (typep outcome 'sb-posix:syscall-error)
                    (eql (sb-posix:syscall-errno outcome) sb-posix:enametoolong))
               "~a reached the system" (car route))))
    (false (probe-file elsewhere))))

(define-test (tools real-home-guard-follows-dots)
  ;; a name that passes through the developer's home and climbs out of it with .. is judged where it leads, as
  ;; the system follows it: one out to the temporary directory is not theirs, from the home or from the
  ;; current directory
  (let* ((home (string-right-trim "/" (first *real-home-names*)))
         (climb (lambda (directory)
                  (format nil "~{~a~}tmp/tuatara-guard-probe"
                          (make-list (count #\/ (string-right-trim "/" directory)) :initial-element "../")))))
    (false (real-home-path-p (format nil "~a/~atmp/tuatara-guard-probe" home (funcall climb home))))
    (false (real-home-path-p (funcall climb (sb-posix:getcwd))))
    (true (real-home-path-p (format nil "/tmp/../~a/never" (subseq home 1))))))

(define-test (tools fences-load-again)
  ;; loaded again in the same Lisp, as from a session, the fence loads, and each call is watched once
  (load (asdf:system-relative-pathname :tuatara "test/fences.lisp"))
  (let* ((path (format nil "~a/~a"
                       (string-right-trim "/" (first *real-home-names*)) (make-string 300 :initial-element #\q)))
         (before *real-home-reached*)
         (outcome (handler-case (sb-posix:stat path) (sb-posix:syscall-error (condition) condition)))
         (reached (ldiff *real-home-reached* before)))
    (sb-thread:with-mutex (*real-home-lock*)
      (setf *real-home-reached* (set-difference *real-home-reached* reached)))
    (is = 1 (length reached))
    (is eql sb-posix:eacces (sb-posix:syscall-errno outcome))))

(define-test (tools fences-say-quicklisp-is-missing)
  ;; in a Lisp without Quicklisp the fence stops at once, saying what is missing
  (let ((setup (sb-posix:getenv "QUICKLISP_SETUP"))
        (home *quicklisp-home*))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "QUICKLISP_SETUP")
           (makunbound '*quicklisp-home*)
           (let ((refusal (progv (list (find-symbol "*QUICKLISP-HOME*" "QL")) '()
                            (handler-case (progn (load (asdf:system-relative-pathname :tuatara "test/fences.lisp")) nil)
                              (error (condition) condition)))))
             (true (typep refusal 'simple-error))
             (true (search "need Quicklisp" (princ-to-string refusal)))))
      (setf *quicklisp-home* home)
      (sb-posix:setenv "QUICKLISP_SETUP" setup 1))))

(define-test (tools tests-homes-are-their-own)
  ;; however the tests are run, the homes of settings, caches, data and state, and what the editor
  ;; keeps in them, are in the tests' own directories, never the developer's
  (dolist (home (list (tuatara.io:config-home) (tuatara.io:cache-home) (tuatara.io:data-home)
                      (tuatara.io::xdg-home "XDG_STATE_HOME" ".local/state/")
                      (tuatara.io:cache-directory) (tuatara.io:state-directory)))
    (true (or (uiop:subpathp home *test-state-directory*) (uiop:subpathp home *test-cache-directory*))
          "~a is not in the tests' own directories" home)))

(define-test (tools editor-running-tools-load-the-fence)
  ;; each fuzzer and manual builder fences before it runs the editor
  (let ((common (tools-text "fuzz-common.lisp")))
    (true (search "tools/fence.lisp" common))
    (true (search "(tuatara.fence:fence-person-data (sb-ext:posix-getenv \"FUZZ_HOME\"))" common)))
  ;; the launcher loads it before the fuzzer of the kind, which sets no init file of its own
  (true (search "--load \"$root/tools/fuzz-common.lisp\" --load \"$root/tools/fuzz-$kind.lisp\""
                (tools-text "fuzz-lisp-inside.sh")))
  (dolist (name '("fuzz-pdf.lisp" "fuzz-remote.lisp" "fuzz-sftp.lisp"))
    (false (search "*config-file*" (tools-text name)) "~a keeps the fence's init file" name))
  (true (search "(tuatara.fence:fence-person-data nil)" (tools-text "manual-tables.lisp")))
  (is = 2 (count-matches "(tuatara.fence:fence-person-data nil)" (tools-text "manual-html.lisp")))
  (true (search "--load tools/fence.lisp" (tools-text "inside.sh")))
  (dolist (name '("manual-tables.sh" "manual-html.sh" "manual-pdf.sh"))
    (true (search "in_tree " (tools-text name)) "~a loads the tree with the fence" name)))

(define-test (tools stale-fences-removed)
  ;; a fence a run left behind (killed, or ended at once, so no exit hook ran) is removed by the next
  ;; tool that makes one; a live run's fence stays, and a link named as a fence left behind is neither
  ;; followed nor removed; the new fence has a name of its own, for its owner alone
  (let* ((scratch (ensure-directories-exist
                   (merge-pathnames (format nil "fences-~d/" (random 1000000)) *test-state-directory*)))
         (dead (ensure-directories-exist (merge-pathnames "tuatara-fence-999999999-00000000000000aa/" scratch)))
         (live (ensure-directories-exist
                (merge-pathnames (format nil "tuatara-fence-~d-00000000000000bb/" (sb-posix:getpid)) scratch)))
         (kept (write-test-file scratch "elsewhere/kept" "kept"))
         (link (concatenate 'string (namestring scratch) "tuatara-fence-999999999-00000000000000cc"))
         (made (progn
                 (sb-posix:symlink (namestring (merge-pathnames "elsewhere/" scratch)) link)
                 (uiop:run-program
                  (list (namestring sb-ext:*runtime-pathname*) "--non-interactive" "--no-userinit" "--no-sysinit"
                        "--load" (namestring (asdf:system-relative-pathname :tuatara "tools/fence.lisp"))
                        "--eval" (format nil "(setf uiop:*temporary-directory* ~s)" (namestring scratch))
                        ;; a tool started on its own, not within the tests' fence
                        "--eval" "(sb-posix:unsetenv \"TUATARA_FENCE\")"
                        "--eval" "(let ((made (tuatara.fence:fence-person-data nil)))
                                    (format t \"~a ~o~%\" (namestring made)
                                            (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring made))))))")
                  :output :string))))
    (false (probe-file dead))
    (true (probe-file live))
    (true (sb-posix:lstat link))
    (true (probe-file kept))
    (true (cl-ppcre:scan (format nil "(?m)^~atuatara-fence-[0-9]+-[0-9A-Fa-f]{16}/ 700"
                                 (cl-ppcre:quote-meta-chars (namestring scratch)))
                         made)
          "~a" made)
    (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore)))

(define-test (tools fence-kept-within-a-fence)
  ;; a manual builder run inside the tests, which are fenced already, keeps their fence: it makes no
  ;; fence within it, so the tests after it find their directories where they were
  (let* ((variables (mapcar #'car tuatara.fence:*fenced-variables*))
         (was (mapcar #'sb-posix:getenv variables)))
    (is equal (uiop:ensure-directory-pathname (sb-posix:getenv "TUATARA_FENCE"))
        (tuatara.fence:fence-person-data nil))
    (is equal was (mapcar #'sb-posix:getenv variables)))
  ;; but a program that only inherited the fence's name, its directories elsewhere, is fenced anew
  (let* ((variables (cons "TUATARA_FENCE" (mapcar #'car tuatara.fence:*fenced-variables*)))
         (was (mapcar #'sb-posix:getenv variables))
         (config tuatara.defaults:*config-file*)
         (temporary uiop:*temporary-directory*)
         (made nil))
    (unwind-protect
         (progn
           (sb-posix:setenv "XDG_STATE_HOME" "/nowhere/state" 1)
           (setf made (tuatara.fence:fence-person-data nil))
           (false (uiop:pathname-equal made (uiop:ensure-directory-pathname (first was))))
           (true (uiop:subpathp (uiop:ensure-directory-pathname (sb-posix:getenv "XDG_STATE_HOME")) made)))
      (loop for name in variables for value in was
            do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name)))
      (setf tuatara.defaults:*config-file* config uiop:*temporary-directory* temporary)
      (tuatara.defaults::forget-state-paths)
      (when made (uiop:delete-directory-tree made :validate t :if-does-not-exist :ignore)))))

(define-test (tools manual-pdf-says-what-the-export-says)
  ;; the manual's LaTeX is made by the one export function, whose missing characters and notes the
  ;; builder then says; no second function that gave fewer of them is left
  (let ((source (tools-text "manual-html.lisp")))
    (true (search "(tuatara.defaults::org-latex-parts whole" source))
    (true (search "(getf parts :missing)" source)))
  (false (fboundp 'tuatara.defaults::org-latex-document)))

(define-test (tools generated-languages-carry-their-mends)
  ;; what was mended in the languages taken from Helix is mended in their generator, so that the next
  ;; run of it keeps it: DBML's comments begin //
  (let ((source (tools-text "languages/generate.py")))
    (true (search "'dbml': {'comment': '//'}" source))
    (true (search "'latex': {'definitions':" source)))
  (is equal '("//" "//") (multiple-value-list (tuatara.modes:mode-comment-syntax
                                                (make-instance 'tuatara.defaults::dbml-mode)))))

(define-test (tools test-keyrings-leave-no-socket-directory)
  ;; a keyring made in a test directory has its agent stopped and its socket directory under /run/user taken away
  (if (or (null (tuatara.defaults::gpg-path)) (null (tuatara.defaults::gpgconf-path)))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((home (namestring (ensure-directories-exist (merge-pathnames "keyring/" dir)))))
          (sb-posix:chmod home #o700)
          (flet ((gpgconf (&rest arguments)
                   (string-trim '(#\Newline #\Space)
                                (uiop:run-program (list* "gpgconf" "--homedir" home arguments)
                                                  :output :string :ignore-error-status t :error-output nil))))
            (uiop:run-program (list "gpg" "--homedir" home "--batch" "--passphrase" "" "--quick-gen-key"
                                    "Tui Haast <tui@example.org>" "ed25519" "sign" "never")
                              :ignore-error-status t :output nil :error-output nil)
            (let ((sockets (gpgconf "--list-dirs" "socketdir")))
              (true (uiop:directory-exists-p (uiop:ensure-directory-pathname sockets)) "~a" sockets)
              (stop-gnupg-homes-under dir)
              (false (uiop:directory-exists-p (uiop:ensure-directory-pathname sockets)) "~a" sockets)))))))
