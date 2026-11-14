;;;; lisp/git/run.lisp --- How the editor runs git: never through a shell, never on the editor's thread.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every git command of the editor goes through RUN-GIT: the change gutter's
;;;; looks at a file, rimu's git column, the packages fetched and C-x g. Git is
;;;; never asked to fetch objects by itself or to run a file-system monitor, a
;;;; pager or a signature check: a repository's own settings can name programs
;;;; for those, and a repository from an archive or a download is not to be
;;;; trusted.

(in-package #:tuatara.git)

(defparameter *git-safety*
  '("--no-lazy-fetch" "--literal-pathspecs"
    "-c" "core.fsmonitor=false" "-c" "log.showSignature=false" "-c" "core.pager=cat")
  "Options every git command gets (RUN-GIT): no fetching of objects a partial
clone lacks, no file-system monitor, no signatures checked and no pager; and
the files named taken as they are, never as patterns (a file called * is
that file alone), but in the commands *GIT-OWN-PATHSPECS* names.")

(defparameter *git-english* (list "LC_ALL=C")
  "What git is given where the words it prints are looked for (a refused
delete, a stopped rebase, a bisection's end): its messages in English whatever
the language the person works in.")

(defparameter *git-own-pathspecs* '("stash")
  "The git commands given no paths that run git again on paths of their own
\(:/ for the whole tree), run without --literal-pathspecs, which would take
those as names of files.")

(defvar *git-lazy-fetch-option* :unknown
  "Whether the git found has the --no-lazy-fetch option (version 2.43 on):
:UNKNOWN until GIT-SAFETY first asks, then true or NIL.")

(defun git-version-numbers (git)
  "The numbers of the version of the program GIT, as git --version prints it
\(git version 2.43.0 is (2 43 0)); NIL when it says none."
  (multiple-value-bind (output said code) (tuatara.io:filter-through-program (list git "--version") nil)
    (declare (ignore said))
    (and (eql code 0)
         (let* ((text (tuatara.text:lenient-string output))
                (start (position-if #'digit-char-p text)))
           (and start
                (loop for from = start then (1+ end)
                      for end = (position-if-not #'digit-char-p text :start from)
                      for number = (parse-integer text :start from :end end :junk-allowed t)
                      while number
                      collect number
                      while (and end (char= (char text end) #\.) (< (1+ end) (length text)))))))))

(defun git-has-lazy-fetch-option-p ()
  "True when the git found takes --no-lazy-fetch (from version 2.43), asked
of it once."
  (when (eq *git-lazy-fetch-option* :unknown)
    (setf *git-lazy-fetch-option*
          (let* ((git (tuatara.io:find-program "git"))
                 (version (and git (ignoring-errors-reported ("asking ~a for its version" git)
                                     (git-version-numbers git)))))
            (and version (or (> (first version) 2) (and (= (first version) 2) (>= (or (second version) 0) 43)))))))
  *git-lazy-fetch-option*)

(defun git-safety (arguments)
  "The options of *GIT-SAFETY* git is given to run with ARGUMENTS, leaving out
what the git found does not know."
  (let ((options (if (member (first (git-command-words arguments)) *git-own-pathspecs* :test #'equal)
                     (remove "--literal-pathspecs" *git-safety* :test #'string=)
                     *git-safety*)))
    (if (and (member "--no-lazy-fetch" options :test #'string=) (not (git-has-lazy-fetch-option-p)))
        (remove "--no-lazy-fetch" options :test #'string=)
        options)))

(defparameter *git-commands-leaving-the-work-tree*
  '("status" "log" "diff" "show" "blame" "cat-file" "rev-parse" "ls-files" "ls-tree" "for-each-ref" "rev-list"
    "describe" "merge-base" "show-ref" "symbolic-ref" "config" "grep" "check-ignore" "check-attr" "shortlog"
    "name-rev" "var" "version" "help" "hash-object" "branch" "tag" "fetch" "push" "remote" "notes"
    "update-ref" "reflog" "count-objects" "fsck" "diff-tree" "diff-index" "diff-files" "cherry" "range-diff"
    "update-index" "write-tree" "commit-tree" "mktree" "ls-remote" "verify-commit" "verify-tag")
  "The git commands that write nothing in a repository's working files (its
.git and its index aside). Every other command is taken to write there
\(GIT-WRITES-WORK-TREE-P), so that a command not named here is held to it
too; add and commit among them, which run programs the repository names on
its working files (clean filters).")

(defparameter *git-options-taking-values*
  '("-c" "-C" "--git-dir" "--work-tree" "--namespace" "--super-prefix" "--config-env" "--attr-source")
  "The options before git's command that take the next word as their value
when it is not written after an = (-C DIRECTORY).")

(defun git-command-words (arguments)
  "ARGUMENTS of git with the options before the command (-c NAME=VALUE, -C
DIRECTORY, --git-dir DIR, --no-pager and the like) left out: the command, then
its own."
  (loop while (and arguments (uiop:string-prefix-p "-" (first arguments)))
        do (setf arguments (if (member (first arguments) *git-options-taking-values* :test #'string=)
                               (cddr arguments)
                               (rest arguments))))
  arguments)

(defun git-option-value (option words)
  "The values WORDS give OPTION (\"--output\"): written after an =, or as
the next word, each time it is given."
  (loop for (word next) on words
        when (string= word option) collect next
        when (uiop:string-prefix-p (concatenate 'string option "=") word)
          collect (subseq word (1+ (length option)))))

(defun git-places (directory arguments environment)
  "Where git run with ARGUMENTS in DIRECTORY and ENVIRONMENT (NAME=VALUE
strings, the first of a name the one in force, as a program reads them) works,
as four values, native names. The first is the directory it runs in, each -C taken
in turn. The second is the repository directory --git-dir or GIT_DIR names, or NIL.
The third is the working directory --work-tree or GIT_WORK_TREE names, or NIL. The
fourth is a list of the other places its variables have it write in
\(GIT_INDEX_FILE, GIT_OBJECT_DIRECTORY, GIT_COMMON_DIR)."
  (let ((here (tuatara.io:native directory)) (git-dir nil) (work-tree nil))
    (flet ((from-here (path)
             (tuatara.io:clean-path (if (uiop:string-prefix-p "/" path) path (tuatara.io:path-join here path)))))
      (loop for (name . more) on arguments
            while (uiop:string-prefix-p "-" name)
            do (cond ((and (string= name "-C") more) (setf here (from-here (first more))))
                     ((uiop:string-prefix-p "--git-dir=" name) (setf git-dir (subseq name 10)))
                     ((and (string= name "--git-dir") more) (setf git-dir (first more)))
                     ((uiop:string-prefix-p "--work-tree=" name) (setf work-tree (subseq name 12)))
                     ((and (string= name "--work-tree") more) (setf work-tree (first more)))))
      (flet ((variable (name)
               (let ((prefix (concatenate 'string name "=")))
                 (loop for setting in environment
                       when (uiop:string-prefix-p prefix setting) return (subseq setting (length prefix))))))
        ;; options name the places before the variables do
        (unless git-dir (setf git-dir (variable "GIT_DIR")))
        (unless work-tree (setf work-tree (variable "GIT_WORK_TREE")))
        (values here (and git-dir (from-here git-dir)) (and work-tree (from-here work-tree))
                (loop for name in '("GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY" "GIT_COMMON_DIR")
                      for value = (variable name)
                      when (and value (plusp (length value))) collect (from-here value)))))))

(defun git-output-paths (words)
  "The files and directories git's command WORDS (GIT-COMMAND-WORDS) writes
elsewhere than in the repository: --output and --output-directory, -o of
format-patch and archive, config's --file or -f, the bundle bundle create
makes, the directory worktree add, move and remove name, the directory clone,
init and submodule add make. As written: relative ones are from where git
runs."
  (destructuring-bind (&optional command &rest more) words
    (let ((plain (remove-if (lambda (word) (uiop:string-prefix-p "-" word)) more)))
      (append (git-option-value "--output" more)
              (git-option-value "--output-directory" more)
              (and (member command '("format-patch" "archive") :test #'equal)
                   (append (git-option-value "-o" more)
                           (loop for word in more
                                 when (and (> (length word) 2) (string= "-o" word :end2 2)) collect (subseq word 2))))
              (and (equal command "config") (append (git-option-value "--file" more) (git-option-value "-f" more)))
              (and (equal command "bundle") (equal (first plain) "create") (list (second plain)))
              (and (equal command "worktree") (member (first plain) '("add" "move" "remove") :test #'string=)
                   (rest plain))
              (and (equal command "clone") (rest plain) (last plain))
              (and (equal command "init") (or plain (list ".")))
              (and (equal command "submodule") (equal (first plain) "add") (cddr plain))))))

(defun git-writes-work-tree-p (arguments)
  "True when git run with ARGUMENTS may write in the repository's working files.
Every command does but those of *GIT-COMMANDS-LEAVING-THE-WORK-TREE*, and these
only as they are given:
apply with --cached or --check (not --index), reset without --hard, --merge,
--keep or --recurse-submodules, restore of --staged alone, rm --cached, stash
list and show, worktree list, submodule status and summary, bisect log,
sparse-checkout list, format-patch --stdout, bundle list-heads and verify, and
archive with no file to write."
  (destructuring-bind (&optional command &rest more) (git-command-words arguments)
    (flet ((given (&rest options) (some (lambda (option) (member option more :test #'string=)) options))
           (then (&rest words) (and more (member (first more) words :test #'string=))))
      (cond ((null command) nil)
            ((member command *git-commands-leaving-the-work-tree* :test #'string=) nil)
            ((string= command "apply") (not (and (given "--cached" "--check") (not (given "--index")))))
            ((string= command "reset") (given "--hard" "--merge" "--keep" "--recurse-submodules"))
            ((string= command "restore") (not (and (given "--staged" "-S") (not (given "--worktree" "-W")))))
            ((string= command "rm") (not (given "--cached")))
            ((string= command "stash") (not (then "list" "show")))
            ((string= command "worktree") (not (then "list")))
            ((string= command "submodule") (not (then "status" "summary")))
            ((string= command "bisect") (not (then "log")))
            ((string= command "sparse-checkout") (not (then "list")))
            ((string= command "format-patch") (not (given "--stdout")))
            ((string= command "bundle") (not (then "list-heads" "verify")))
            ((string= command "archive") (or (given "-o")
                                             (some (lambda (word) (uiop:string-prefix-p "--output" word)) more)))
            (t t)))))

(define-condition git-refused (tuatara.io:writing-refused) ()
  (:report (lambda (c stream)
             (format stream "git ~a is not run: ~a"
                     (tuatara.io:file-system-error-operation c) (tuatara.io:file-system-error-reason c))))
  (:documentation "Git not run, its command named as the operation, for it would write where nothing
is written (CHECK-GIT-WRITING)."))

(defun check-git-writing (directory arguments &optional environment)
  "Signal TUATARA.IO:WRITING-REFUSED, the directory named, when git run with
ARGUMENTS in DIRECTORY and ENVIRONMENT would write in an evidence directory.
It is refused in four cases.
The repository or working directory, found from where git runs (-C, --git-dir,
--work-tree, GIT_DIR, GIT_WORK_TREE: GIT-PLACES), is in one, for git writes its
.git there.
The command writes the repository's working files (GIT-WRITES-WORK-TREE-P) and
the repository holds one (TUATARA.IO:CHECK-TREE-WRITING).
A file or directory the command is told to write (GIT-OUTPUT-PATHS) is in one.
Any word given to it, taken as a path (CHECK-GIT-ARGUMENTS), is in one.
Whether the repository holds one is told from the setting
\(DIRECTORY-HOLDS-EVIDENCE-P); those words are returned then, for the hooks to
be left out (RUN-GIT-PROGRAM). Nothing is looked at while no place is refused."
  (when (tuatara.io:writing-refused-anywhere-p)
    (handler-case (check-git-places directory arguments environment)
      (git-refused (e) (error e))
      (tuatara.io:writing-refused (e)
        (error 'git-refused :operation (or (first (git-command-words arguments)) "")
                            :path (tuatara.io:file-system-error-path e)
                            :reason (tuatara.io:file-system-error-reason e))))))

(defun check-git-places (directory arguments environment)
  "CHECK-GIT-WRITING's looks, refusing in the words of a write refused."
  (let* ((words (git-command-words arguments))
         (doing (format nil "run git ~a in" (or (first words) ""))))
    (multiple-value-bind (here git-dir work-tree others) (git-places directory arguments environment)
      (let ((top (or work-tree (ignoring-errors-reported ("finding the repository of ~a" here)
                                 (repository-top here)))))
        (dolist (place (remove nil (list git-dir top)))
          (tuatara.io:check-writing place doing))
        (dolist (place others)
          (tuatara.io:check-tree-writing place doing))
        (dolist (path (git-output-paths words))
          (when (and path (plusp (length path)))
            (tuatara.io:check-tree-writing (if (uiop:string-prefix-p "/" path) path (tuatara.io:path-join here path))
                                           (format nil "have git ~a write" (first words)))))
        (check-git-arguments here arguments (format nil "have git ~a write" (first words)))
        (when top
          (let ((holding (directory-holds-evidence-p top)))
            (when (and holding (git-writes-work-tree-p arguments))
              (error 'tuatara.io:writing-refused :operation doing :path top :reason holding))
            holding))))))

(defun git-argument-places (word)
  "The paths a word given to git may name: the word itself, what follows its
first =, and what follows a short option stuck to it (-fPATH)."
  (remove-if (lambda (path) (zerop (length path)))
             (append (and (not (uiop:string-prefix-p "-" word)) (list word))
                     (let ((equals (position #\= word))) (and equals (list (subseq word (1+ equals)))))
                     (and (> (length word) 2) (char= (char word 0) #\-) (alpha-char-p (char word 1))
                          (list (subseq word 2))))))

(defparameter *git-pathspecs-read-only*
  '("log" "diff" "show" "blame" "grep" "ls-files" "ls-tree" "status" "check-attr" "check-ignore" "hash-object"
    "update-index" "diff-tree" "diff-index" "diff-files" "cat-file" "rev-list" "shortlog" "describe")
  "The git commands whose words after a standalone -- are paths of the
working files, only read, files of evidence among them. Named one by one:
for others (push, fetch, remote) the words after -- are repositories and
what to take or give them.")

(defun git-words-checked (arguments)
  "The words of ARGUMENTS that CHECK-GIT-ARGUMENTS looks at: all of them, but
for a command of *GIT-PATHSPECS-READ-ONLY* those after its first
standalone --, the paths it reads (git log -- evidence/report.pdf)."
  (let* ((words (git-command-words arguments))
         (end (and (member (first words) *git-pathspecs-read-only* :test #'equal)
                   (position "--" words :test #'string=))))
    (if end (ldiff arguments (nthcdr end words)) arguments)))

(defun check-git-arguments (here arguments doing)
  "Signal TUATARA.IO:WRITING-REFUSED, DOING said, when a word of ARGUMENTS,
taken as a path from HERE, where git runs (GIT-ARGUMENT-PLACES), lands in or
under an evidence directory, or names a link into one: no option of git's can
then write there, whatever it is called or however it is abbreviated. The
paths a reading command is given after -- are let be (GIT-WORDS-CHECKED)."
  (dolist (word (git-words-checked arguments))
    (dolist (place (git-argument-places word))
      (let ((path (if (uiop:string-prefix-p "/" place) place (tuatara.io:path-join here place))))
        (tuatara.io:check-writing path doing)
        (let ((real (ignoring-errors-reported ("finding where ~a leads" path) (tuatara.io:real-path path))))
          (when real (tuatara.io:check-writing real doing)))))))

(defun git-hash-p (text &key short)
  "True when TEXT is a commit's full hash as git writes it: 40 hexadecimal
digits, or 64 in a repository of SHA-256 hashes; with SHORT, a hash cut as
short as 7 digits too. What a commit says of itself (its subject, its
author) is never taken for one."
  (and (stringp text)
       (if short (<= 7 (length text) 64) (member (length text) '(40 64)))
       (every (lambda (c) (digit-char-p c 16)) text)))

(defparameter *git-look-seconds* 30
  "How long git may take to tell the change gutter and rimu's lists what it
has of a file or directory before it is ended.")

(defun repository-top (directory)
  "The nearest directory from DIRECTORY up holding .git (MARKED-DIRECTORY, as
PROJECT-ROOT finds a project), as a native name ending with a slash, or NIL."
  (marked-directory directory '(".git")))

(defparameter *git-reading-seconds* 120
  "How long git may read a repository not trusted, a command given no time
of its own, before it is ended: a file of its objects that never ends
reading (a named pipe) holds nothing up for longer.")

(defvar *git-timed-out* (make-hash-table :test 'equal :synchronized t)
  "The repositories not trusted, by their top, in which git ran too long this
session: not run there again, the change gutter and every look left out,
until \\[git] asks for it there (FORGET-GIT-SHADOW).")

(defun run-git (directory arguments &key cancel line environment errors input seconds limit)
  "Run git with ARGUMENTS (a list of strings, never through a shell) in
DIRECTORY and return what it printed, as octets, and true as a second value
when it succeeded. It runs on the thread calling it, which is never the
editor's, with *GIT-SAFETY*, and never asks for a password on a terminal.
NIL, and no process at all, when DIRECTORY is in no repository
\(REPOSITORY-TOP) or git is not found (FIND-PROGRAM).

With LINE, a function, each line git prints is given to it as text as it
comes, in place of the octets, until it returns true, which counts as success.
CANCEL, a cons, stops git when its car turns true; on the thread of a job,
stopping the job ends git at once. The process is ended however it stops.
ENVIRONMENT, NAME=VALUE strings, is added to git's. With ERRORS, what git says
on its error output (*GIT-SAID-LIMIT* octets of it at most) is read apart as
text, the fourth value. INPUT, a string, is given on its standard input. With
SECONDS, git is ended once it has run that long, as if stopped. Git printing
more octets than LIMIT (*PROGRAM-OUTPUT-LIMIT* when it is NIL), or with LINE a
line longer than that, is ended, NIL and NIL given, and true as a third value.

In a repository you trust, git looks for no repository above its top and takes
no bare one it finds on the way. In any other, git only reads, on a shadow of
the repository, and runs none of its programs: see GIT-READS-ONLY-P for which
those are, and CALL-GIT-READING for how. ENVIRONMENT is not given to it, it is
ended after *GIT-READING-SECONDS* when SECONDS gives no time, and a command that
would do more is refused, GIT-REFUSED signalled; the disk failing while its
shadow is made gives NIL, any other error reaching the caller. Once git has
run too long there, git is not run there again this session
\\(NOTE-GIT-TIMED-OUT) until \\[git] asks for it."
  (let* ((git (tuatara.io:find-program "git"))
         (top (ignoring-errors-reported ("finding the repository of ~a" directory) (repository-top directory))))
    (cond ((not (and git top)) (values nil nil))
          ((not (git-reads-only-p top))
           (run-git-program git directory arguments
                            :cancel cancel :line line :errors errors :input input :seconds seconds :limit limit
                            :options (list "-c" "safe.bareRepository=explicit")
                            :environment (cons (format nil "GIT_CEILING_DIRECTORIES=~a"
                                                       (string-right-trim "/" (tuatara.io:parent-path top)))
                                               environment)))
          ((gethash top *git-timed-out*) (values nil nil))
          (t
           (let ((timed-out (list nil)))
             (multiple-value-prog1
                 (handler-case
                     (reporting-errors ("reading ~a with git" top)
                       (call-git-reading top directory arguments
                                         (lambda (arguments looking-environment here)
                                           (run-git-program git here arguments
                                                            :cancel cancel :line line :errors errors :input input
                                                            :seconds (or seconds *git-reading-seconds*) :limit limit
                                                            :reading t :environment looking-environment
                                                            :timed-out timed-out))))
                   ;; refused: said by the caller; the disk failing under the shadow: no answer
                   (git-refused (e) (error e))
                   ((or tuatara.io:file-system-error file-error stream-error) () (values nil nil)))
               (when (car timed-out) (note-git-timed-out top))))))))

(defun git-object-name (revision path)
  "How git names the file or directory PATH of REVISION (a commit's hash, HEAD):
REVISION:PATH, PATH from the top of the repository, or from the directory git
runs in when it begins with ./."
  (format nil "~a:~a" revision path))

(defun git-blob-octets (top hash path &key job limit cancel seconds)
  "What the file PATH (from TOP, or from the directory TOP when it begins with ./)
held as the commit HASH (or HEAD) of the repository at TOP left it, as octets,
read by git cat-file. With PATH NIL, it is the text whose own id HASH is. With
JOB, it is read on its thread, as GIT-JOB-OCTETS reads it, JOB ending there when
git failed. Otherwise it is read on the thread calling it as RUN-GIT gives it,
CANCEL and SECONDS as it takes them. It returns true as a second value when it
was read and as a third when it was more than LIMIT octets."
  (let ((arguments (list "cat-file" "blob" (if path (git-object-name hash path) hash))))
    (if job
        (git-job-octets job top arguments :limit limit)
        (run-git top arguments :limit limit :cancel cancel :seconds seconds))))

(defparameter *git-said-limit* (* 64 1024)
  "The most octets of what git says on its error output kept, to say why it
failed (RUN-GIT with ERRORS); the rest is read and let go.")

(defun run-git-program
    (git directory arguments &key cancel line environment errors input seconds limit reading options timed-out)
  "Run the program GIT as RUN-GIT runs git, once it has found it. A command
that would write in the working files of a repository holding an evidence
directory is refused, never run (CHECK-GIT-WRITING); in a repository holding
one, git runs no hook of the repository's (core.hooksPath=/dev/null). With
READING, ARGUMENTS only read, made so by CALL-GIT-READING: nothing is looked
at, and git is given none of your GIT_ variables. TIMED-OUT, a cons, has its
car made true when git is ended for running past SECONDS. The options of
*GIT-SAFETY*, then OPTIONS, come after any of ARGUMENTS' own before its
command, so that they are the ones in force."
  (let* ((limit (or limit *program-output-limit*))
         (words (git-command-words arguments))
         (leading (ldiff arguments words))
         ;; all git is given, the editor's own variables and its client's among them: what it is checked against
         (environment (append environment (list "GIT_TERMINAL_PROMPT=0")
                              (if reading
                                  (remove-if (lambda (setting) (uiop:string-prefix-p "GIT_" setting))
                                             (tuatara.io:program-environment))
                                  (tuatara.io:program-environment))))
         ;; the editor's own options after yours, so that they are the ones in force
         (arguments (append leading (git-safety arguments) options
                            (and (not reading)
                                 (check-git-writing directory arguments environment)
                                 (list "-c" "core.hooksPath=/dev/null"))
                            words)))
    (handler-case
        (reporting-errors ("running git ~a in ~a" (or (first words) "") directory)
          (tuatara.io:call-with-program
           (list* git "-C" (tuatara.io:native directory) arguments)
           (lambda (process)
             (let ((stream (sb-ext:process-output process))
                   (listener (and errors
                                  (tuatara.runtime:make-guarded-thread
                                   (lambda ()
                                     (ignoring-errors-reported ("reading what git says")
                                       (tuatara.io:read-stream-octets (sb-ext:process-error process)
                                                                      :limit *git-said-limit* :drain t)))
                                   :name "tuatara git messages")))
                   (octets nil) (stopped nil) (cancelled nil) (too-large nil)
                   (timer (and seconds (sb-ext:make-timer (lambda () (when (sb-ext:process-alive-p process)
                                                                        (when timed-out (setf (car timed-out) t))
                                                                        (ignoring-errors-reported ("ending git")
                                                                          (sb-ext:process-kill process 9))))
                                                          :thread t))))
               (when timer (sb-ext:schedule-timer timer seconds))
               (when input
                 (let ((in (sb-ext:process-input process)))
                   (ignoring-errors-reported ("giving git its input") (write-string input in))
                   (ignoring-errors-reported ("closing git's input") (close in))))
               (unwind-protect
                    (if line
                        (loop (multiple-value-bind (text cut) (tuatara.io:read-line-bounded stream limit)
                                (cond ((null text) (return))
                                      (cut (setf too-large t) (return))
                                      ((and cancel (car cancel)) (setf cancelled t) (return))
                                      ((funcall line text) (setf stopped t) (return)))))
                        (setf (values octets too-large cancelled)
                              (tuatara.io:read-stream-octets stream :limit limit :cancel cancel)))
                 (when timer (sb-ext:unschedule-timer timer)))
               ;; read to its end: git is done, and how it ended is known once it is waited for
               (unless (or stopped cancelled too-large) (sb-ext:process-wait process))
               (when (and listener (or stopped cancelled too-large) (sb-ext:process-alive-p process))
                 (ignoring-errors-reported ("ending git") (sb-ext:process-kill process 15)))
               (let ((said (and listener
                                (let ((octets (sb-thread:join-thread listener :default nil)))
                                  (if octets (tuatara.text:lenient-string octets) ""))))
                     (ok (and (not too-large) (not cancelled) (or stopped (eql 0 (sb-ext:process-exit-code process))))))
                 (cond ((and too-large listener) (values nil nil t said))
                       (too-large (values nil nil t))
                       (listener (values (and (not line) (not cancelled) octets) ok nil said))
                       (t (values (and (not line) (not cancelled) octets) ok))))))
           :job tuatara.runtime:*job* :input (and input :stream) :output :stream :error (and errors :stream)
           :environment environment))
      (error () (values nil nil)))))
