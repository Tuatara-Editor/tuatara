;;;; src/io/programs.lisp --- Finding, starting, reading and ending the programs of the system.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A program is found on the PATH (FIND-PROGRAM), started with the
;;;; environment it is to have (START-PROGRAM, CALL-WITH-PROGRAM), its output
;;;; read within limits (PROGRAM-OUTPUT, READ-OUTPUTS), text filtered through
;;;; it (FILTER-THROUGH-PROGRAM), and ended with all it started when it is no
;;;; longer wanted (END-UNWANTED-PROGRAM). Programs that are to end with the
;;;; editor are started from one thread kept for that.

(in-package #:tuatara.io)

(defparameter *default-program-path* "/usr/bin:/bin"
  "Where programs are looked for when the editor was started with no PATH.")

(defun program-search-path ()
  "The directories programs are looked for in, as the PATH lists them:
the editor's own PATH, or *DEFAULT-PROGRAM-PATH* when it has none."
  (or (own-variable "PATH") *default-program-path*))

(defun find-program (name &optional (search (program-search-path)))
  "The full path of the program NAME in the directories SEARCH lists (the
PATH), or NAME itself when it is a path from the root (starting with /) and
can be run; NIL when there is no such program. Only directories named from
the root are looked in, and a path not from the root (./tool, bin/tool) is
never taken: either would mean wherever the editor happens to be, which may
be a directory of files not to be trusted."
  (flet ((runnable (path) (and (handler-case (sb-posix:access path sb-posix:x-ok) (sb-posix:syscall-error () nil))
                               (eq :regular (path-kind path))
                               path)))
    (if (find #\/ name)
        (and (char= #\/ (char name 0)) (runnable name))
        (loop for directory in (uiop:split-string search :separator ":")
              thereis (and (plusp (length directory)) (char= #\/ (char directory 0))
                           (runnable (path-join directory name)))))))

(defun program-missing (name &key install why)
  "Signal that the program NAME is not installed, saying how to install it
(INSTALL, \"Fedora: sudo dnf install gnupg2\") and what is not done without it
\(WHY), each when given, as in (or (find-program \"git\") (program-missing \"git\"))."
  (refuse "~a is not installed~@[ (~a)~]~@[: ~a~]" name install why))

(defun setsid-command (program arguments &rest setsid-flags)
  "How to start PROGRAM with ARGUMENTS in a session of its own (setsid), so
that it stays when the editor or its terminal goes, as two values: the full
path of the program to run and its arguments. That is setsid, given
SETSID-FLAGS (\"-w\" to wait for PROGRAM, \"-c\" to give it its terminal)
before PROGRAM and ARGUMENTS, when setsid is there; else PROGRAM itself.
Each is found as FIND-PROGRAM finds it, so the result is run as it is, never
looked for again; PROGRAM is left as it is when it is not found."
  (let ((setsid (find-program "setsid"))
        (program (or (find-program program) program)))
    (if setsid
        (values setsid (append setsid-flags (cons program arguments)))
        (values program arguments))))

(defun setpriv-command (program arguments)
  "How to start PROGRAM, by its full path, with ARGUMENTS so that it is
killed when the editor ends, killed or not (setpriv --pdeathsig KILL), as
a list of the program to run and its arguments; NIL when setpriv is not in
the system's own directories of programs (SANDBOX-FIND-PROGRAM)."
  (let ((setpriv (sandbox-find-program "setpriv")))
    (and setpriv (list* setpriv "--pdeathsig" "KILL" "--" program arguments))))

(define-setting *program-output-limit* (* 64 1024 1024)
  "The most octets PROGRAM-OUTPUT takes of what a program writes.
A program that writes more, a clipboard of a huge copy or a program gone
wrong, is stopped and its output not taken, so that it cannot fill memory.
Raise it to take more through such a program, at the cost of that memory;
lower it on a small machine.
- a whole number, 1 or more: the octets allowed; 64 MB as standard."
  :values (integer 1 *))

(defparameter *program-end-grace* 2
  "The seconds a program no longer wanted has to end once asked to (SIGTERM),
before it is killed (END-UNWANTED-PROGRAM).")

(defun end-unwanted-program (process &key (close t))
  "End PROCESS, a program started and no longer wanted, with every program in
its process group and in the sessions its children lead (as the program
SESSION-COMMAND starts leads one): asked to end (SIGTERM), then those not
ended within *PROGRAM-END-GRACE* seconds killed (SIGKILL), PROCESS waited for
and closed, or with CLOSE NIL left for whoever reads its pipes to close.
PROCESS itself is signalled too, for one started in the editor's own group
to keep its terminal (SB-EXT:RUN-PROGRAM's :INPUT T). Returns at once the
thread doing the waiting, which kills what is left if it is itself ended
first."
  (let* ((groups (list (sb-ext:process-pid process)))
         (buffer (make-array 1024 :element-type '(unsigned-byte 8)))
         (sessions (loop for child in (process-children process)
                         when (eql child (nth-value 1 (process-group-and-session child buffer)))
                           collect child)))
    ;; its group's number is its own while it is not reaped
    (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
      (sb-ext:process-kill process sb-unix:sigterm :process-group))
    (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
      (when (sb-ext:process-alive-p process)
        (sb-ext:process-kill process sb-unix:sigterm :pid)))
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
         (flet ((signal-all (signal)
                  (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
                    (when (sb-ext:process-alive-p process)
                      (sb-ext:process-kill process signal :pid)))
                  (dolist (pid (processes-in groups sessions buffer))
                    (signal-process-if pid (lambda ()
                                             (multiple-value-bind (group session) (process-group-and-session pid buffer)
                                               (and group (or (member group groups) (member session sessions)))))
                                       signal))))
           (unwind-protect
                (progn
                  (when sessions (signal-all sb-unix:sigterm))
                  (tuatara.runtime:wait-until (lambda ()
                                                (and (not (sb-ext:process-alive-p process))
                                                     (null (processes-in groups sessions buffer))))
                                              *program-end-grace* :interval 1/20))
             (signal-all sb-unix:sigkill)
             (tuatara.runtime:ignoring-errors-reported ("waiting for ~a to end" process) (sb-ext:process-wait process))
             (when close
               (tuatara.runtime:ignoring-errors-reported ("closing ~a" process) (sb-ext:process-close process)))))))
     :name "program ending")))

(defun done-with-program (process)
  "Let go of PROCESS, a program started: ended with what it started when it
still runs, without waiting for it (END-UNWANTED-PROGRAM), so that none is
left behind; else waited for and closed."
  (if (sb-ext:process-alive-p process)
      (end-unwanted-program process)
      (progn (tuatara.runtime:ignoring-errors-reported ("waiting for ~a to end" process)
               (sb-ext:process-wait process))
             (tuatara.runtime:ignoring-errors-reported ("closing ~a" process) (sb-ext:process-close process)))))

(defgeneric host-program-environment (host)
  (:documentation "The variables a program started now is given when it is given none of its
own, as NAME=VALUE strings: the editor's, with those of whom the work is for laid over
them. The core's own host gives the editor's.")
  (:method ((host tuatara.runtime:host))
    (sb-ext:posix-environ)))

(defvar *own-variables* '()
  "The names of the environment variables Tuatara set for itself alone, as
the window's GSK_RENDERER, kept for as long as it runs: no program it starts
is given them.")

(defun program-environment ()
  "The variables a program started now is given, as NAME=VALUE strings,
when it is given none of its own (HOST-PROGRAM-ENVIRONMENT), but those
Tuatara set for itself (*OWN-VARIABLES*)."
  (let ((environment (host-program-environment tuatara.runtime:*host*)))
    (if *own-variables*
        (remove-if (lambda (setting)
                     (let ((equals (position #\= setting)))
                       (and equals (member (subseq setting 0 equals) *own-variables* :test #'string=))))
                   environment)
        environment)))

(defun program-variable (name)
  "The value of the environment variable NAME as a program started now has
it (PROGRAM-ENVIRONMENT), or NIL when it has none. Working through a daemon,
that is as the client you work from has it: its display, its terminal's
variables. Read with it what belongs to where you are working; the editor's
own directories and settings with OWN-VARIABLE."
  (let ((prefix (concatenate 'string name "=")))
    (loop for setting in (program-environment)
          when (uiop:string-prefix-p prefix setting) return (subseq setting (length prefix)))))

(defun desktop-p ()
  "True when a program started now has a desktop to show a window on: its
DISPLAY or WAYLAND_DISPLAY names one (PROGRAM-VARIABLE)."
  (and (or (plusp (length (program-variable "DISPLAY")))
           (plusp (length (program-variable "WAYLAND_DISPLAY"))))
       t))

(defparameter *askpass-programs*
  '("/usr/libexec/openssh/ssh-askpass" "/usr/lib/ssh/ssh-askpass" "/usr/lib/openssh/ssh-askpass" "/usr/bin/ssh-askpass")
  "Where the program OpenSSH asks for a passphrase or a confirmation in a
window with is looked for when SSH_ASKPASS names none: where the systems
put the one OpenSSH comes with.")

(defun askpass-program ()
  "The program ssh and ssh-agent ask in a window with, by its full path: the
one SSH_ASKPASS names (PROGRAM-VARIABLE), as gnome-ssh-askpass or
ksshaskpass, else the first of *ASKPASS-PROGRAMS* there; NIL when there is
none."
  (let ((named (program-variable "SSH_ASKPASS")))
    (if (plusp (length named))
        (find-program named)
        (some #'find-program *askpass-programs*))))

(defun variable-listed-p (name names)
  "True when the environment variable NAME is one of NAMES, a * at the end of
one standing for any ending (LC_* for LC_ALL, LC_TIME and the rest)."
  (some (lambda (listed)
          (if (char= #\* (char listed (1- (length listed))))
              (uiop:string-prefix-p (subseq listed 0 (1- (length listed))) name)
              (string= listed name)))
        names))

(defun listed-environment (environment names)
  "Those of ENVIRONMENT, NAME=VALUE strings, whose names are among NAMES (VARIABLE-LISTED-P)."
  (remove-if-not (lambda (setting)
                   (let ((equals (position #\= setting)))
                     (and equals (variable-listed-p (subseq setting 0 equals) names))))
                 environment))

(defparameter *restricted-variables* '("HOME" "USER" "LOGNAME" "LANG" "LANGUAGE" "LC_*" "TZ" "TMPDIR")
  "The environment variables every program run with only some of the
editor's keeps (RESTRICTED-ENVIRONMENT): who you are and where your home
is, your language and time zone, and where scratch files go.")

(defun restricted-environment (names &rest settings)
  "The environment of a program given only some of the variables a program
started now has (PROGRAM-ENVIRONMENT), as NAME=VALUE strings: those named
in *RESTRICTED-VARIABLES* or NAMES, a * at the end of a name standing for
any ending; each of SETTINGS, NAME=VALUE strings, in place of the variable
of its name, a later one in place of an earlier one."
  (flet ((name (setting) (subseq setting 0 (position #\= setting))))
    (setf settings (remove-duplicates settings :key #'name :test #'string=))
    (append settings
            (remove-if (lambda (setting) (member (name setting) settings :key #'name :test #'string=))
                       (listed-environment (program-environment) (append *restricted-variables* names))))))

(defun start-program (command &key input output error if-error-exists directory (environment nil environment-p)
                                     preserve-fds pty session with-editor close-fds
                                     (external-format '(:utf-8 :replacement #\?)))
  "Start COMMAND, a program's full name and its arguments (never through a
shell and never looked for), left running. Returns its process, for the caller
to end (END-UNWANTED-PROGRAM, DONE-WITH-PROGRAM) or to close.
INPUT, OUTPUT and ERROR say what it reads and writes, as SB-EXT:RUN-PROGRAM
takes them. NIL is for nothing, :STREAM for a pipe, T for the editor's own
\(INPUT T keeps the terminal, for a program that asks for a password on it).
They may also be a stream on a descriptor, or a path (IF-ERROR-EXISTS saying
what is done with a file ERROR names that is there already). :OUTPUT as ERROR
joins its errors to what it writes.
With PTY, it runs on a new pseudo-terminal, its process's
SB-EXT:PROCESS-PTY. DIRECTORY is where it runs. ENVIRONMENT, when given, is all
it sees of one, NAME=VALUE strings (else PROGRAM-ENVIRONMENT). PRESERVE-FDS is
descriptors it is given besides.
SESSION NIL leaves it in a process group of its own. T starts it in a session
of its own (setsid), so that it stays when the editor or its terminal goes,
the process given back being setsid's, which may end at once. :TERMINAL as
well makes its pseudo-terminal its controlling terminal and gives back a
process that ends when it ends (setsid -w -c).
WITH-EDITOR true has it killed when the editor ends, killed or not
\(SETPRIV-COMMAND). It is started from the one thread kept for that
\(IN-STARTER), since it would be killed when the thread starting it ends. It is
refused when setpriv is not there.
CLOSE-FDS is file descriptors (FDs) handed over with the start, such as the
program's end of a socket. They are closed by the thread that starts it once
it is started or its start failed, so that a start given up on while under way
never finds them closed, or reused, under it. EXTERNAL-FORMAT is that of its
pipes."
  (flet ((close-handed ()
           (dolist (fd close-fds)
             (tuatara.runtime:close-fd-quietly fd "a descriptor handed to a program"))))
    (when with-editor
      (let ((tied (setpriv-command (first command) (rest command))))
        (unless tied
          (close-handed)
          (program-missing "setpriv" :install "util-linux"
                                     :why (format nil "~a would outlive the editor"
                                                  (native-file-name (first command)))))
        (multiple-value-bind (process failure)
            (in-starter (lambda ()
                          (apply #'start-program tied
                                 :input input :output output :error error :if-error-exists if-error-exists
                                 :directory directory :preserve-fds preserve-fds :pty pty :session session
                                 :close-fds close-fds :external-format external-format
                                 (and environment-p (list :environment environment))))
                        :untaken #'close-handed)
          (when failure (error failure))
          (return-from start-program process))))
    (unwind-protect
         (multiple-value-bind (program arguments)
             (if session
                 (apply #'setsid-command (first command) (rest command) (and (eq session :terminal) '("-w" "-c")))
                 (values (first command) (rest command)))
           (apply #'sb-ext:run-program program arguments
                  :wait nil :search nil :input input :output output :error error :pty pty
                  :external-format external-format :environment (if environment-p environment (program-environment))
                  (append (and if-error-exists (list :if-error-exists if-error-exists))
                          (and directory (list :directory directory))
                          (and preserve-fds (list :preserve-fds preserve-fds)))))
      (close-handed))))

(defun call-with-program (command function &key job input output error directory (environment nil environment-p)
                                               preserve-fds (external-format '(:utf-8 :replacement #\?)))
  "Start COMMAND, a program's full name and its arguments (never through a
shell), and call FUNCTION with its process, returning what FUNCTION returns.
INPUT, OUTPUT and ERROR say what the program reads and writes, as
SB-EXT:RUN-PROGRAM takes them (:STREAM for a pipe FUNCTION uses, NIL for
none, :OUTPUT for its errors with what it writes). DIRECTORY is where it
runs. ENVIRONMENT, when given, is all it sees of one (else
PROGRAM-ENVIRONMENT). PRESERVE-FDS is descriptors it is given besides (pipes
of its own, named in its arguments).
With JOB, it is the program the job runs (TUATARA.RUNTIME:JOB-PROGRAM-STARTED):
stopping the job ends it.
However FUNCTION ends, the program is ended when it still runs, with what it
started (END-UNWANTED-PROGRAM), without waiting for it, so that none is left
behind. One ended already is waited for and closed."
  (let ((process (apply #'start-program command
                        :input input :output output :error error :external-format external-format
                        :directory directory :preserve-fds preserve-fds
                        (and environment-p (list :environment environment)))))
    (when job (tuatara.runtime:job-program-started job process))
    (unwind-protect (funcall function process)
      (done-with-program process))))

;;; Programs that end with the editor

(sb-ext:defglobal **starter** nil
  "The thread programs that end with the editor are started from, made on
first use and kept as long as the editor runs: setpriv --pdeathsig
\(PR_SET_PDEATHSIG) and bwrap --die-with-parent end a program when the
thread that started it ends, not the editor, so none such is started from a
thread that may end first.")

(sb-ext:defglobal **starter-lock** (sb-thread:make-mutex :name "program starting")
  "Held while work is handed to the starting thread, or taken from it.")

(sb-ext:defglobal **starter-wake** (sb-thread:make-waitqueue :name "program starting")
  "Signalled when the starting thread is given work, and when it has done it.")

(sb-ext:defglobal **starter-work** nil
  "The work handed to the starting thread, (TOKEN FUNCTION . UNTAKEN),
FUNCTION of no arguments, until it takes it; UNTAKEN, a function or NIL,
called should the work be dropped before it is taken.")

(sb-ext:defglobal **starter-done** (make-hash-table :test 'eq)
  "What each work given to a starting thread returned, by its token, as
\(RESULT CONDITION), until it is taken; under **STARTER-LOCK**.")

(sb-ext:defglobal **starters** '()
  "Every thread programs have been started from, the one in use and those
given up on (RETIRED), which take no more work and are left to end their
programs' lives with the editor's; under **STARTER-LOCK**.")

(sb-ext:defglobal **retired** '()
  "The starting threads given up on, as one that did not answer in time.")

(defun starter-loop ()
  "What the starting thread does: run each function handed to it, keeping what
it returns, or the condition that stopped it, by the work's token, unless
that work was given up on; once itself given up on, nothing more, waiting
to be ended with the rest. The sandboxes it started are ended before it
ends: a sandbox ends with the thread that started it, and ended so it may
leave a process behind (END-SANDBOXES)."
  (unwind-protect (starter-work-loop)
    (end-sandboxes :thread sb-thread:*current-thread*)))

(defun starter-work-loop ()
  "The loop of STARTER-LOOP: each function handed to the starting thread run in turn."
  (loop
    (let ((work (sb-thread:with-mutex (**starter-lock**)
                  (loop until (and **starter-work** (not (member sb-thread:*current-thread* **retired**)))
                        do (sb-thread:condition-wait **starter-wake** **starter-lock**))
                  (shiftf **starter-work** nil))))
      (let ((result (handler-case (list (funcall (second work))) (serious-condition (e) (list nil e)))))
        (sb-thread:with-mutex (**starter-lock**)
          (if (eq (car (car work)) :abandoned)
              ;; given up on: a program it started meanwhile is ended, not left behind
              (when (sb-ext:process-p (first result))
                (end-sandbox (first result)))
              (setf (gethash (car work) **starter-done**) result))
          (sb-thread:condition-broadcast **starter-wake**))))))

(defparameter *starter-wait* 30
  "The seconds a start on the starting thread may take, a sandbox looked for
first, before it is given up and another thread made to start programs from.")

(defparameter *at-once-wait* 0.05
  "The seconds a start on a thread that must not wait may take, past which
it is left to finish in the background (IN-STARTER :AT-ONCE).")

(sb-ext:defglobal **starting** (sb-thread:make-mutex :name "program start")
  "Held by the thread having a program started on the starting thread, so that one is started at a time.")

(defun in-starter (function &key at-once held untaken)
  "What FUNCTION returns, called in the thread programs that end with the
editor are started from (made the first time, and again when it has died or
not done its last work within *STARTER-WAIT*, that one then retired, its
programs left running), and the condition that stopped it, if one did, as a
second value. FUNCTION owns what it opens: given up on, it may still run
later. AT-ONCE: when another start is under way, :BUSY at once, FUNCTION not
called; and given up on after *AT-ONCE-WAIT* (:BUSY), the thread not
retired, what FUNCTION started then ended. HELD when the caller holds the
start already (HOLDING-STARTER). UNTAKEN, a function, is called when
FUNCTION was given up on before it was ever called. START-PROGRAM with
:WITH-EDITOR starts one program so; this is for a start that does more
around it, as a sandbox's (SANDBOX-START)."
  (unless (or held (sb-thread:grab-mutex **starting** :waitp (not at-once)))
    (when untaken
      (tuatara.runtime:ignoring-errors-reported ("letting go of a start not taken") (funcall untaken)))
    (return-from in-starter :busy))
  (unwind-protect (in-starter-now function (if at-once *at-once-wait* *starter-wait*) (not at-once) untaken)
    (unless held (sb-thread:release-mutex **starting**))))

(defun in-starter-now (function wait retire untaken)
  "The work of IN-STARTER, the start held: FUNCTION waited for WAIT
seconds; the starting thread RETIRE'd when it did not answer in them. Work
an earlier asker left there untaken, gone without waiting, is let go of in
its place, its UNTAKEN called."
  (sb-thread:with-mutex (**starter-lock**)
    (unless (and **starter** (sb-thread:thread-alive-p **starter**))
      (setf **starter** (sb-thread:make-thread #'starter-loop :name "tuatara program starting"))
      (push **starter** **starters**))
    (let ((token (list :work))
          (starter **starter**)
          (deadline (tuatara.runtime:deadline-in wait)))
      ;; work its asker left untaken, gone without waiting for it, lets go of what it holds
      (let ((left (shiftf **starter-work** (list* token function untaken))))
        (when (and left (cddr left))
          (setf (car (car left)) :abandoned)
          (tuatara.runtime:ignoring-errors-reported ("letting go of a start not taken")
            (funcall (cddr left)))))
      (sb-thread:condition-broadcast **starter-wake**)
      (loop until (or (nth-value 1 (gethash token **starter-done**))
                      (not (sb-thread:thread-alive-p starter))
                      (tuatara.runtime:deadline-passed-p deadline))
            do (tuatara.runtime:wait-on **starter-wake** **starter-lock**
                                        (max 0.005 (min 1 (tuatara.runtime:seconds-left deadline)))))
      (multiple-value-bind (result done) (gethash token **starter-done**)
        (remhash token **starter-done**)
        (cond (done (values-list result))
              (t
               (setf (car token) :abandoned)
               ;; never taken: what it would have closed is closed here
               (when (eq (car **starter-work**) token)
                 (setf **starter-work** nil)
                 (when untaken
                   (tuatara.runtime:ignoring-errors-reported ("letting go of a start not taken")
                     (funcall untaken))))
               (cond (retire
                      ;; dead or stuck: retired, the next start made from another
                      (push starter **retired**)
                      (setf **starter** nil)
                      (values nil (make-condition 'simple-error
                                                  :format-control "the thread starting programs did not answer")))
                     (t :busy))))))))

(defun holding-starter (function)
  "What FUNCTION returns, called holding the one start at a time on the
starting thread, for it to start there with IN-STARTER :HELD, so that no
other start comes first; :BUSY at once, FUNCTION not called, when another
start is under way."
  (if (sb-thread:grab-mutex **starting** :waitp nil)
      (unwind-protect (funcall function) (sb-thread:release-mutex **starting**))
      :busy))

(defun end-starters ()
  "End every thread programs that end with the editor were started from, and
with them those programs, as the editor ends and before a saved image is made."
  (dolist (starter (sb-thread:with-mutex (**starter-lock**)
                     (prog1 **starters** (setf **starters** '() **retired** '() **starter** nil))))
    (when (sb-thread:thread-alive-p starter)
      (tuatara.runtime:ignoring-errors-reported ("ending the thread starting programs")
        (sb-thread:terminate-thread starter))
      (tuatara.runtime:ignoring-errors-reported ("waiting for the thread starting programs to end")
        (sb-thread:join-thread starter :default nil :timeout 2)))))

(pushnew 'end-starters sb-ext:*exit-hooks*)
(pushnew 'end-starters sb-ext:*save-hooks*)

;;; Programs in a session of their own

(defun user-shell ()
  "The shell a command typed is run with (a compile's, a terminal's): $SHELL, else /bin/sh."
  (let ((shell (own-variable "SHELL")))
    (if (plusp (length shell)) shell "/bin/sh")))

(defun session-command (command)
  "COMMAND, a program's full name and its arguments, run in a session of its
own with no terminal (setsid -w, which waits for it and gives its exit code):
a program in it that asks for a password (sudo, ssh, gpg) cannot reach the
terminal the editor runs in, and SIGNAL-SESSION reaches every program it
starts. COMMAND as it is when setsid is not there."
  (multiple-value-bind (program arguments) (setsid-command (first command) (rest command) "-w")
    (cons program arguments)))

(defun program-output (command &key input (capture t) (seconds 1) wait (environment nil environment-p) directory
                                    (most *program-output-limit*))
  "Run COMMAND, a program's full name and its arguments (never through a
shell), giving it INPUT (a string, through a pipe: never written to a file).
With CAPTURE, it takes what the program writes, as UTF-8 text (an octet that
is not UTF-8 as its raw byte's character), MOST octets of it at most (a
program writing more is stopped, its output not taken). SECONDS is the most it
may take, then it is stopped. WAIT, when given, does the waiting instead of
this thread blocking: a function of the thread doing the work and SECONDS (the
editor's interruptible wait, say). ENVIRONMENT, when given, is all the program
sees of one. DIRECTORY is where it runs.
Two values: its output (with CAPTURE) and true when it ended well. A program
that stays behind (wl-copy, serving the clipboard) is not waited for: only the
one started."
  (let* ((started (list nil))
         ;; whom the program is for is known here, not on the thread running it
         (environment (if environment-p environment (program-environment)))
         (worker (tuatara.runtime:make-guarded-thread
                  (lambda ()
                    (tuatara.runtime:ignoring-errors-reported ("running ~a" (first command))
                      (call-with-program
                       command
                       (lambda (process)
                         (setf (car started) process)
                         (when input
                           (tuatara.runtime:ignoring-errors-reported ("giving ~a its input" (first command))
                             (write-string input (sb-ext:process-input process)))
                           (tuatara.runtime:ignoring-errors-reported ("closing the input of ~a" (first command))
                             (close (sb-ext:process-input process))))
                         (multiple-value-bind (octets too-much)
                             (and capture
                                  (tuatara.runtime:ignoring-errors-reported ("reading what ~a writes" (first command))
                                    (read-stream-octets (sb-ext:process-output process) :limit most)))
                           (unless too-much (sb-ext:process-wait process))
                           (list (and octets (tuatara.text:octets-string octets))
                                 (and (not too-much) (eql 0 (sb-ext:process-exit-code process))))))
                       :input (and input :stream) :output (and capture :stream)
                       :directory directory
                       :environment environment)))
                  :name "program")))
    (if wait
        (funcall wait worker seconds)
        (sb-thread:join-thread worker :timeout seconds :default nil))
    (if (sb-thread:thread-alive-p worker)
        (progn (let ((process (car started)))
                 (when process (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
                                 (sb-ext:process-kill process sb-unix:sigterm))))
               (values nil nil))
        (values-list (sb-thread:join-thread worker :default nil)))))

(defparameter *program-said-limit* (* 64 1024)
  "The most octets kept of what a program says on its error output
\(FILTER-THROUGH-PROGRAM); the rest is read and let go.")

(defun read-outputs (fds limits drains &key deadline too-much cancel (kept t) wipe)
  "Read FDS, pipes open for reading, each to its end, on this thread alone,
waiting on them all at once so that none waits on another. Of each, the first
octets up to its limit in LIMITS (NIL: all) are kept, or only counted when its
flag in KEPT (T: all kept) is false. Past it, the rest is read and let go when
its flag in DRAINS is true, else that pipe is no longer read and TOO-MUCH, a
function of no arguments, is called; when it returns :STOP, no pipe is read
any more. Reading stops at DEADLINE
\(TUATARA.RUNTIME:DEADLINE-IN) when one is given, and when CANCEL, a cons, has
a true car. With WIPE, every octet read is filled with zeros once it is
joined into what is given, or let go.
Four values. The first is a list of what each gave, as octets, NIL for one
that gave more than its limit and was not drained, or whose octets were only
counted. The second is a list of true for each that gave more than its limit.
The third is true when DEADLINE came, or CANCEL, before every pipe ended; the
fourth is true when CANCEL did."
  (let* ((count (length fds))
         (pieces (loop repeat count collect (make-octet-pieces)))
         (read (make-list count :initial-element 0))
         (over (make-list count))
         (kept (if (eq kept t) (make-list count :initial-element t) kept))
         (open (copy-list fds))
         (cancelled nil)
         (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop named reading
          while open
          until (or (and deadline (tuatara.runtime:deadline-passed-p deadline))
                    (and cancel (car cancel) (setf cancelled t)))
          do (let ((ready (wait-for-fds open (min (if cancel 0.1 1)
                                                  (if deadline (max 0 (tuatara.runtime:seconds-left deadline)) 1)))))
               (when (consp ready)
                 (dolist (fd ready)
                   (let* ((i (position fd fds))
                          (into (nth i pieces))
                          (limit (nth i limits))
                          (got (read-fd-into fd buffer :path "a program's output")))
                     (cond ((zerop got) (setf open (remove fd open)))
                           ((nth i over))
                           (t (incf (nth i read) got)
                              (when (nth i kept)
                                (let ((n (if limit (min got (max 0 (- limit (octet-pieces-size into)))) got)))
                                  (when (plusp n) (add-octet-piece into (subseq buffer 0 n)))))
                              (when (and limit (> (nth i read) limit))
                                (setf (nth i over) t)
                                (unless (nth i drains)
                                  (setf open (remove fd open))
                                  (when (and too-much (eq (funcall too-much) :stop))
                                    (return-from reading)))))))))))
    (multiple-value-prog1
        (values (loop for into in pieces
                      for past in over
                      for drain in drains
                      for keep in kept
                      collect (and keep (or (not past) drain) (joined-octets into)))
                over
                (and open t)
                cancelled)
      (when wipe
        (fill buffer 0)
        (mapc #'wipe-octet-pieces pieces)))))

(defun filter-through-program (command write &key job directory (environment nil environment-p)
                                                 (limit *program-output-limit*) truncate input seconds)
  "Run COMMAND, a program's full name and its arguments (never through a
shell), calling WRITE with the descriptor of its standard input to give it
what it reads, a piece at a time (WRITE-OCTETS; a buffer's text from
BUFFER-TEXT, say), its input closed once WRITE returns. What it prints and
what it says on its error output are read meanwhile on a thread of their
own, so that none waits on another. A program that ends before it has read
all makes WRITE's next write fail (FILE-SYSTEM-ERROR), which WRITE may take
as the end.
With WRITE NIL, INPUT is what it reads instead, as SB-EXT:RUN-PROGRAM takes
it. T is the editor's own standard input, so that a program asking for a
password on the terminal (ssh, ssh-add) can. It may also be a file stream or
path; NIL is nothing.
With JOB, it is the program the job runs: stopping the job ends it. DIRECTORY
is where it runs. ENVIRONMENT, when given, is all it sees of one. With
SECONDS, it is given that long, then ended with every program it started
\(END-UNWANTED-PROGRAM), waited for while it ends, *PROGRAM-END-GRACE* seconds
and a little more at most.
Four values. The first is what it printed, as octets. It is NIL when it
printed more than LIMIT octets, and it is ended then. With TRUNCATE, it is
the first LIMIT octets, the rest read and let go while it runs on. A LIMIT of
NIL takes all it prints. The second is what it said, as text, its first
*PROGRAM-SAID-LIMIT* octets. The third is its exit code, NIL when it did not
exit of itself or was ended for taking too long. The fourth is true when it
printed more than LIMIT."
  (apply #'call-with-program command
         (lambda (process)
           (let* ((deadline (and seconds (tuatara.runtime:deadline-in seconds)))
                  (reader (tuatara.runtime:make-guarded-thread
                           (lambda ()
                             (destructuring-bind (&optional said over (late t) &rest cancelled)
                                 (tuatara.runtime:ignoring-errors-reported ("reading what ~a writes" (first command))
                                   (multiple-value-list
                                    (read-outputs (list (sb-sys:fd-stream-fd (sb-ext:process-output process))
                                                        (sb-sys:fd-stream-fd (sb-ext:process-error process)))
                                                  (list limit *program-said-limit*) (list truncate t)
                                                  :deadline deadline
                                                  ;; ended, so that it waits on no one reading what it prints
                                                  :too-much (lambda ()
                                                              (tuatara.runtime:ignoring-errors-reported
                                                                  ("ending ~a" process)
                                                                (sb-ext:process-kill process sb-unix:sigterm))))))
                               (declare (ignore cancelled))
                               ;; ended at its deadline, or when its pipes could not be read
                               (list said over late (and late (end-unwanted-program process :close nil)))))
                           :name "program output")))
             (when write
               (unwind-protect (funcall write (sb-sys:fd-stream-fd (sb-ext:process-input process)))
                 (tuatara.runtime:ignoring-errors-reported ("closing the input of ~a" (first command))
                   (close (sb-ext:process-input process)))))
             (destructuring-bind (&optional said over late ending) (sb-thread:join-thread reader :default nil)
               (when ending
                 (tuatara.runtime:join-within ending (1+ *program-end-grace*)))
               (let ((exited (and (not late)
                                  (if deadline
                                      (tuatara.runtime:wait-until (lambda () (not (sb-ext:process-alive-p process)))
                                                                  (max 0 (tuatara.runtime:seconds-left deadline)))
                                      (sb-ext:process-wait process)))))
                 (values (and (or truncate (not (first over))) (first said))
                         (if (second said) (tuatara.text:lenient-string (second said)) "")
                         (and exited (eq (sb-ext:process-status process) :exited) (sb-ext:process-exit-code process))
                         (first over))))))
         :job job :input (if write :stream input) :output :stream :error :stream :directory directory
         (and environment-p (list :environment environment))))

(defun said-text (octets)
  "OCTETS a program wrote to say something, as text, trimmed, octets that are
not UTF-8 shown as ?."
  (string-trim '(#\Space #\Newline #\Return #\Tab)
               (tuatara.text:lenient-string octets)))

(defun version-text (program)
  "What PROGRAM says of its version (--version), its environment empty; an
empty string when it says nothing within five seconds."
  (or (program-output (list program "--version") :seconds 5 :environment '()) ""))
