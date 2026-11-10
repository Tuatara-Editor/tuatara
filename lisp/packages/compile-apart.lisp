;;;; lisp/packages/compile-apart.lisp --- Compiling a package in a process of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package's files are compiled by a child process started from a core made for
;;;; the purpose, with a request file saying what to compile, so that nothing a
;;;; package does while it compiles can reach the editor.

(in-package #:tuatara.packages)

(defparameter *package-compiling-line* ";;; tuatara: compiling"
  "What a process compiling a package apart prints on a line of its own as it
begins compiling a file of it (COMPILE-PACKAGE-CHILD).")

(defparameter *package-child-said-lines* 40
  "The most lines kept of what a process compiling a package apart said, the
last ones, to say why it failed.")

(defun note-package-compiling (file)
  "As FILE, of a package, is about to be compiled: an error while the editor
waits for a package to load (*PACKAGE-COMPILING* :REFUSED), for FILE changed
after the package was compiled apart; said on the output the first time in
a process compiling a package apart (:APART)."
  (case *package-compiling*
    (:refused (refuse "~a changed while the package was being compiled; it loads as changed the next time it is loaded"
                     (abbreviated-path (tuatara.io:native file))))
    (:apart (format t "~&~a~%" *package-compiling-line*)
     (finish-output)
     (setf *package-compiling* :apart-said))))

(defun package-request-text (folder name file token)
  "What a process compiling the package NAME in FOLDER apart is given to do
\(COMPILE-PACKAGE-CHILD), as text read back without evaluating anything: the
package, FILE its entry when named; the packages loaded so far, to load
first, oldest first; where this Tuatara keeps compiled packages; and TOKEN,
which the process is given apart too (PACKAGE-REQUEST-ALLOWED-P)."
  (with-standard-io-syntax
    (let ((*print-readably* t))
      (prin1-to-string
       (list :token token :name name :folder (tuatara.io:native folder) :file file
             :prelude (reverse (remove name *loaded-package-folders* :key #'first :test #'string=))
             :cache (tuatara.io:native (tuatara.io:cache-directory))
             :packages (packages-directory)
             :identity (tuatara-identity))))))

(defparameter *package-child-environment*
  '("PATH" "HOME" "USER" "LOGNAME" "LANG" "LC_ALL" "LC_CTYPE" "TZ" "TMPDIR"
    "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_STATE_HOME"
    "SBCL_HOME" "QUICKLISP_SETUP")
  "The variables of the editor's environment a process compiling a package
apart is given, and no others. This cuts down what it reaches by chance (the
desktop, the keyring's agent, the session's message bus, whose sockets
XDG_RUNTIME_DIR names); it is no sandbox: a package's code runs there with
your full rights, as in the editor.")

(defun package-child-environment (request token)
  "The environment of a process compiling a package apart, as REQUEST, a
file, and TOKEN say (COMPILE-PACKAGE-CHILD): those of
*PACKAGE-CHILD-ENVIRONMENT* the editor has, and, with REQUEST, what tells the
process what to do."
  (append (loop for name in *package-child-environment*
                for value = (tuatara.io:own-variable name)
                when value collect (format nil "~a=~a" name value))
          (list "TUATARA_HEAP_DECIDED=1")
          (and request
               (list (format nil "TUATARA_COMPILE_PACKAGE=~a" request)
                     (format nil "TUATARA_COMPILE_TOKEN=~a" token)
                     (format nil "TUATARA_COMPILE_CACHE=~a" (package-cache))))))

(defvar *compiler-core-lock* (sb-thread:make-mutex :name "tuatara compiler core")
  "Held while the core packages are compiled apart from, run from Tuatara's
sources, is made (COMPILER-CORE), so that it is made once.")

(defun compiler-core-toplevel ()
  "What the core COMPILER-CORE makes runs: compile a package as the editor
asks (COMPILE-PACKAGE-CHILD), then exit with its code."
  (sb-ext:disable-debugger)
  (uiop:call-image-restore-hook)
  (let ((code (compile-package-child)))
    (finish-output)
    (sb-ext:exit :code code :abort t)))

(defun compiler-core ()
  "Run from Tuatara's sources, the Steel Bank Common Lisp (SBCL) core a process
compiling a package apart starts from, so that it starts at once. It is SBCL
with Quicklisp and these sources loaded, saved once into the folder of compiled
packages. That folder is this Tuatara's alone (PACKAGE-CACHE: another source
tree, or the same one changed, has another). The core is made again only when it
is not there. It is made in a process of its own (save-lisp-and-die cannot run
where threads do), under a name of its own and renamed into place whole. Returns
its path, or NIL when it could not be made, why in *Messages*."
  (let ((core (format nil "~acompiler-~36r.core" (package-cache)
                      (sxhash (uiop:native-namestring (asdf:system-source-directory :tuatara))))))
    (sb-thread:with-mutex (*compiler-core-lock*)
      (or (and (probe-file core) core)
          (let ((temporary (format nil "~a.~d.~a.new" core (sb-posix:getpid) (tuatara.io:random-name))))
            (multiple-value-bind (said ok)
                (tuatara.io:program-output
                 (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                       "--non-interactive"
                       "--no-sysinit"
                       "--no-userinit"
                       "--load" (uiop:native-namestring (asdf:system-relative-pathname :tuatara "tools/quicklisp.lisp"))
                       "--eval" (format nil "(progn (push ~s asdf:*central-registry*) (funcall (find-symbol \"QUICKLOAD\" \"QL\") :tuatara/defaults :silent t))"
                                        (uiop:native-namestring (asdf:system-source-directory :tuatara)))
                       "--eval" (format nil "(sb-ext:save-lisp-and-die ~s :toplevel (find-symbol \"COMPILER-CORE-TOPLEVEL\" \"TUATARA.PACKAGES\"))"
                                        temporary))
                 :seconds 600 :environment (package-child-environment nil nil))
              (cond ((and ok (probe-file temporary))
                     (sb-posix:chmod temporary #o600)
                     (sb-posix:rename temporary core)
                     core)
                    (t (handler-case (delete-file temporary) (file-error () nil))
                       (let ((words (format nil "No core to compile packages apart from could be made: ~a"
                                            (tuatara.text:one-line (or said "it failed")))))
                         (defer-action (lambda () (log-message words))))
                       nil))))))))

(defun package-compiler-command ()
  "The command that runs this Tuatara again to compile a package apart, told
what to do by its environment (PACKAGE-CHILD-ENVIRONMENT): the saved
executable, or, run from its sources, Steel Bank Common Lisp (SBCL) started
from the core made for that (COMPILER-CORE); NIL when there is none."
  (if tuatara.io:*saved-program*
      (list tuatara.io:*saved-program*)
      (let ((core (compiler-core)))
        (and core (list (uiop:native-namestring sb-ext:*runtime-pathname*) "--core" core)))))

(defparameter *package-child-line-limit* 1024
  "The most characters kept of a line a process compiling a package apart
says; the rest of it is read and let go.")

(defparameter *package-child-said-limit* (* 64 1024)
  "The most characters read and kept in all of what a process compiling a
package apart says; the rest is read and let go, so that it never waits.")

(defun compile-package-apart (job folder name file)
  "Compile the package NAME in FOLDER, FILE its entry when named, in a process
of its own, this Tuatara run again (PACKAGE-COMPILER-COMMAND): it loads the
packages loaded here so far, then this one, compiling what changed, as a
load here would, so that its compile-time code never runs in the editor.
JOB is the loading's: stopping it ends that process; its COMPILED turns true
as a file of it begins compiling, said. True when it was all compiled so;
NIL when it failed there, as code of the package's that needs what only the
editor has does, the last of what the process said kept in *Messages*, or
when there is nothing to run it from (COMPILER-CORE). It
is refused for names this Tuatara lacks only as it loads in the editor
\(REFUSE-PACKAGE). What the process says is read a line of at most
*PACKAGE-CHILD-LINE-LIMIT* at a time, *PACKAGE-CHILD-SAID-LIMIT* in all."
  (let ((request (format nil "~acompile-~a.request" (package-cache) (tuatara.io:random-name)))
        (token (tuatara.io:random-name))
        (command (package-compiler-command))
        (said '()) (kept 0) (read 0))
    (unless command
      (return-from compile-package-apart nil))
    (tuatara.io:ensure-private-directory (package-cache))
    (tuatara.io:write-private-file request (package-request-text folder name file token))
    (unwind-protect
         (let ((code (tuatara.io:call-with-program
                      command
                      (lambda (process)
                        (loop for line = (tuatara.io:read-line-bounded (sb-ext:process-output process)
                                                                       *package-child-line-limit*)
                              while line
                              do (incf read (length line))
                                 (cond ((> read *package-child-said-limit*))
                                       ((string= line *package-compiling-line*)
                                        (unless (package-fetch-compiled job)
                                          (setf (package-fetch-compiled job) t)
                                          (message "Compiling the package ~a in the background..." name)))
                                       (t (push line said)
                                          (when (> (incf kept) *package-child-said-lines*)
                                            (setf said (subseq said 0 *package-child-said-lines*) kept *package-child-said-lines*)))))
                        (sb-ext:process-wait process)
                        (sb-ext:process-exit-code process))
                      :job job :output :stream :error :output
                      :environment (package-child-environment request token))))
           (tuatara.runtime:job-check-stop)
           (cond ((eql code 0) t)
                 (t (let ((words (format nil "The package ~a could not be compiled apart from the editor, and is compiled as it loads~@[: ~{~a~^ ~}~]"
                                         name
                                         (reverse (mapcar (lambda (l) (string-trim " ;" l))
                                                          (remove "" said :test #'string=))))))
                      ;; *Messages* is the editor's
                      (defer-action (lambda () (log-message words))))
                    nil)))
      (handler-case (delete-file request) (file-error () nil)))))

(defun package-request-allowed-p (request token cache)
  "True when REQUEST, a file, is one the editor wrote for a process compiling
a package apart (COMPILE-PACKAGE-APART): a regular file, not a link, in CACHE,
the folder of compiled packages the editor named, called compile-*.request,
yours and read and written by you alone; and TOKEN, given in the
environment, is the one it holds (checked by COMPILE-PACKAGE-CHILD)."
  (let ((stat (and request (plusp (length request)) token (plusp (length token)) cache
                   (ignoring-errors-reported ("examining ~a" request) (sb-posix:lstat request))))
        (name (and request (tuatara.io:native-file-name request))))
    (and stat
         (= (logand (sb-posix:stat-mode stat) #o170000) #o100000)
         (= (logand (sb-posix:stat-mode stat) #o777) #o600)
         (= (sb-posix:stat-uid stat) (sb-posix:getuid))
         (uiop:string-prefix-p "compile-" name)
         (uiop:string-suffix-p name ".request")
         (equal (tuatara.io:real-path (tuatara.io:parent-path request))
                (tuatara.io:real-path cache)))))

(defun compile-package-child ()
  "Compile a package as the editor asked in a process of its own: the
request it wrote (PACKAGE-REQUEST-TEXT), named in the environment as
TUATARA_COMPILE_PACKAGE, taken only when PACKAGE-REQUEST-ALLOWED-P and
holding the token TUATARA_COMPILE_TOKEN gives; never from the command line.
The packages loaded in the editor are loaded first, one that fails left out,
then the package, each file compiled that changed since, as a load in the
editor would, into the same place (PACKAGE-FASL), nothing refused
\(REFUSE-PACKAGE). Returns the exit code: 0 when it loaded, 1 when it
failed, why printed, 2 when the request was not taken."
  (let ((request (tuatara.io:own-variable "TUATARA_COMPILE_PACKAGE"))
        (token (tuatara.io:own-variable "TUATARA_COMPILE_TOKEN"))
        (cache (tuatara.io:own-variable "TUATARA_COMPILE_CACHE")))
    (dolist (name '("TUATARA_COMPILE_PACKAGE" "TUATARA_COMPILE_TOKEN" "TUATARA_COMPILE_CACHE"))
      (sb-posix:unsetenv name))
    (unless (package-request-allowed-p request token cache)
      (format t "~&Not a request of the editor's: nothing compiled~%")
      (return-from compile-package-child 2))
    (handler-case
        (reporting-errors ("compiling as ~a asks" request)
          (let ((plist (with-standard-io-syntax
                         (let ((*read-eval* nil)) (read-from-string (tuatara.io:file-text request))))))
            (unless (and (listp plist) (equal (getf plist :token) token))
              (format t "~&Not a request of the editor's: nothing compiled~%")
              (return-from compile-package-child 2))
            (setf *tuatara-identity* (getf plist :identity)
                  tuatara-user:*cache-directory* (getf plist :cache)
                  tuatara-user:*packages-directory* (getf plist :packages))
            (let ((*package-compiling* :apart))
              (loop for (name folder file) in (getf plist :prelude)
                    do (handler-case (reporting-errors ("loading the package ~a" name)
                                       (load-package-entry (pathname folder) name file))
                         (error (e) (format t "~&~a was left out: ~a~%" name e))))
              (load-package-entry (pathname (getf plist :folder)) (getf plist :name) (getf plist :file)))
            0))
      (serious-condition (e)
        (format t "~&~a~%" e)
        1))))

(defun start-package-fetch (name where version file key &key quiet)
  "Fetch the package NAME (FETCH-PACKAGE) in the background, and see who
signed it (PACKAGE-SIGNATURE), then ask and load it (PACKAGE-FETCHED). An
address of http:// or git:// not allowed (PLAIN-SOURCE-REFUSAL) is not
fetched from, said; nor is a package in hand already (START-PACKAGE-JOB),
said too unless QUIET. One whose folder package-remove-unused is deleting
waits for it (WAIT-FOR-SWEEP)."
  (let ((refusal (plain-source-refusal name where)))
    (when refusal
      (setf *package-failures* (acons name refusal (remove name *package-failures* :key #'car :test #'string=)))
      (message "~a" refusal)
      (return-from start-package-fetch nil)))
  (when (wait-for-sweep (package-kept-folder where name)
                        (lambda () (start-package-fetch name where version file key :quiet quiet)))
    (return-from start-package-fetch nil))
  (let ((plain (and (plain-transport-p where) t))
        (floor (let ((had (fetched-package key))) (and had (fourth had) (git-hash-p (second had) :short t) (second had)))))
    (when (start-package-job
           (make-package-fetch name where version file key)
           (lambda (job)
             (let ((*package-plain-transport* plain))
               (multiple-value-bind (folder commit tag passed signature)
                   (fetch-package name where version
                                  :file file :floor floor
                                  :walk-p (lambda (looked commit tag)
                                            (not (eq :refuse (package-trust-verdict where (package-signature looked commit tag where)))))
                                  :signature-of (lambda (looked commit tag) (package-signature looked commit tag where)))
                 (setf (package-fetch-folder job) folder (package-fetch-commit job) commit (package-fetch-tag job) tag
                       (package-fetch-passed job) passed (package-fetch-signature job) signature))))
           "tuatara package fetch" :quiet quiet)
      (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=))
      (log-message (format nil "Fetching the package ~a from ~a" name where)))))

(defun start-package-check (name where version file key entry &key quiet)
  "Check in the background who signed the version of the package NAME from
WHERE kept as ENTRY remembers it (PACKAGE-SIGNATURE), then take it as one
just fetched (PACKAGE-FETCHED): asked about with who signed it, loaded, or
refused, as the trust in force says. Nothing is fetched; nothing is done
while the package is in hand already (START-PACKAGE-JOB, QUIET as it takes
it)."
  (let ((job (make-package-fetch name where version file key)))
    (setf (package-fetch-folder job) (package-folder-for where name (second entry))
          (package-fetch-commit job) (second entry)
          (package-fetch-tag job) (third entry))
    (when (start-package-job
           job
           (lambda (job)
             (setf (package-fetch-signature job)
                   (package-signature (package-fetch-folder job) (package-fetch-commit job) (package-fetch-tag job) where)))
           "tuatara package check" :quiet quiet)
      (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=)))))

(defun signature-signer (signature)
  "Who SIGNATURE (PACKAGE-SIGNATURE) says signed a version, as an entry of
*FETCHED-PACKAGES* keeps it: the key's fingerprint, \"unsigned\", or NIL when
it could not be told."
  (case (getf signature :status)
    (:good (getf signature :fingerprint))
    (:none "unsigned")))

(defun kept-version-check-needed-p (where entry)
  "True when the version of the source WHERE that ENTRY of *FETCHED-PACKAGES*
remembers is to be checked again before it loads at a start. That is so when who
signed it, as git checked it when it was taken, is not known. It is also so when
the trust in force now would not take it (PACKAGE-TRUST-VERDICT: the source
trusted anew, its signer changed since, the init file's :signers naming others).
It is never so while *PACKAGE-CHECK-SIGNATURES* is NIL."
  (let ((signer (seventh entry)))
    (and *package-check-signatures*
         (or (null signer)
             (not (eq :take (package-trust-verdict where (if (string= signer "unsigned")
                                                             (list :status :none)
                                                             (list :status :good :fingerprint signer)))))))))

(defmethod tuatara.runtime:job-take-in ((job package-fetch))
  "Ask about and load the package fetched, or say why it was not; its package
held in hand until that is decided. For a package compiled and loaded in the
background, what follows its loading (PACKAGE-LOAD-TAKEN-IN)."
  (if (package-fetch-load job)
      (let ((job job))
        (defer-action (lambda () (package-load-taken-in job))))
      (progn
        (setf *fetching-packages* (remove (package-fetch-name job) *fetching-packages* :test #'string=))
        (case (package-fetch-status job)
          (:done (let ((job job))
                   (defer-action (lambda ()
                                   (let ((*package-job-deciding* job))
                                     (unwind-protect (package-fetched job)
                                       (let-package-go job)))))))
          (:failed (let-package-go job)
                   (let ((why (or (tuatara.runtime:job-error job) "it ended unexpectedly")))
                     (push (cons (package-fetch-name job) why) *package-failures*)
                     (message "The package ~a was not fetched: ~a" (package-fetch-name job) why)))
          (t (let-package-go job))))))

(defun package-fetched (job)
  "Take the version of a package JOB fetched, or refuse it. One that needs a
newer Tuatara than this was passed over for the newest that fits, said; when
none fits, the version loaded now is kept. One whose signer is not the one
trusted is refused (PACKAGE-TRUST-VERDICT). Else it is loaded: at once when
that commit was let load before and its source's signer is known, said to
be up to date when it is loaded already; else asked about first, who signed
it shown, and that signer remembered once it is let load. The version taken
before is kept to fall back on (LOAD-VERSION), older ones deleted
\(PRUNE-PACKAGE-VERSIONS)."
  (let* ((name (package-fetch-name job)) (key (package-fetch-key job)) (where (package-fetch-where job))
         (commit (package-fetch-commit job)) (tag (package-fetch-tag job))
         (passed (package-fetch-passed job))
         (had (fetched-package key)))
    (cond
      ((null (package-fetch-folder job))
       (let ((words (format nil "The package ~a ~a needs Tuatara ~a or later, and this is ~a; ~:[no older version tried fits~;~
                                 it is not signed as the versions taken before were, so no older version is looked for~]"
                            name (first passed) (second passed) (tuatara:version) (third passed)))
             (loaded (package-loaded-p name)))
         (unless loaded (push (cons name words) *package-failures*))
         (message "~a: ~:[it is not loaded~;the version loaded now is kept~]" words loaded)))
      (t
       (when passed
         (message "The package ~a ~a needs Tuatara ~a or later, and this is ~a: ~a, the newest version that fits, is taken"
                  name (first passed) (second passed) (tuatara:version) (or tag (subseq commit 0 7))))
       (multiple-value-bind (verdict words remember) (package-trust-verdict where (package-fetch-signature job))
         (if (eq verdict :refuse)
             (progn
               ;; a version taken before and refused now is asked about again, never loaded at a start unchecked
               (when (and had (fourth had) (string= (second had) commit))
                 (state-list-put *fetched-packages*
                                 (list (list key commit tag nil (fifth had) (sixth had) (seventh had)))))
               (refuse-package-version name where words remember))
             (let* ((folder (package-folder-in (package-fetch-folder job) name))
                    (same (and had (string= (second had) commit)))
                    (previous (if (and had (fourth had) (not same)) (second had) (fifth had)))
                    (previous-tag (if (and had (fourth had) (not same)) (third had) (sixth had)))
                    (signer (signature-signer (package-fetch-signature job)))
                    (trust (lambda ()
                             (when remember
                               (state-list-put *package-trust* (list remember)))))
                    (load (lambda ()
                            (state-list-put *fetched-packages*
                                            (list (list key commit tag t previous previous-tag signer)))
                            (load-version name where folder (package-fetch-file job)
                                          (cond ((and had (fourth had) (not same)) had)
                                                (previous (list key previous previous-tag t)))
                                          (lambda (outcome &optional loaded)
                                            (declare (ignore loaded))
                                            (when (eq outcome :taken)
                                              (funcall trust)
                                              (when (package-loaded-p name)
                                                (message "The package ~a~@[ ~a~] is loaded~@[ (~a needs Tuatara ~a or later)~]"
                                                         name
                                                         (or tag (and passed (subseq commit 0 7)))
                                                         (first passed)
                                                         (second passed))))
                                            (prune-package-versions name where))))))
               (cond ((and had (fourth had) same (package-loaded-p name))
                      (funcall trust)
                      (unless (equal (seventh had) signer)
                        (state-list-put *fetched-packages* (list (list key commit tag t (fifth had) (sixth had) signer))))
                      (message "The package ~a is up to date~@[ (~a)~]" name tag))
                     ((and had (fourth had) same (not (eq verdict :ask)))
                      (funcall load))
                     ((and had (fourth had) same)
                      ;; taken before its source's signer was remembered: asked again, who signed it shown
                      (package-question name where folder load words))
                     (t (state-list-put *fetched-packages* (list (list key commit tag nil previous previous-tag signer)))
                        (package-question name where folder load words))))))))))
