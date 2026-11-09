;;;; src/io/sandbox.lisp --- Running a program on an untrusted file, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file from elsewhere is never parsed by the editor, the terminal or the
;;;; window toolkit: a program of the system decodes it (images to pixels, PDF
;;;; to text), shut in by bubblewrap (bwrap). It sees the system's programs and
;;;; libraries (/usr) and nothing else, read only: no home directory, no other
;;;; file, no network, no other process, no way into the terminal; it starts
;;;; with nothing in its environment and can make no new namespaces. Its
;;;; memory, the processor time it takes, the files it writes in its own
;;;; scratch /tmp and what it prints are limited, and it is ended, with all it
;;;; started, when its time is up or the one waiting stops it; every sandbox
;;;; still running is ended when the editor ends, and one left behind by an
;;;; editor killed while bwrap was making it is ended when the sandbox is next
;;;; tried. A seccomp filter refuses it the system calls no decoder needs
;;;; and that reach into the kernel's most exposed parts: mounting,
;;;; namespaces, kernel modules and keys, tracing other processes, io_uring,
;;;; BPF, sockets other than local ones, and pushing keys into a terminal; nor
;;;; may it make a named pipe or a device.
;;;;
;;;; The file is not shown to it by name: it reads it on its standard input, a
;;;; copy of it in memory that nothing can change, made from a descriptor opened
;;;; here the way a safe visit opens a file, so its access time is not changed
;;;; (on your own files) and nothing else in its directory can be reached.
;;;; The file's own descriptor never reaches the program: through it, the file
;;;; could be written, its permissions and times changed.
;;;;
;;;; A program that needs more is shown more, and only that: LaTeX making a
;;;; PDF of a document sees the document's folder and TeX's own, read only,
;;;; and writes in its working directory alone (lisp/export/export.lisp).
;;;;
;;;; bwrap is found on the PATH when first needed and tried once; the result is
;;;; kept, and M-x check-health tries it again. Only SANDBOX-STATUS and
;;;; SANDBOX-RUN are used elsewhere (SANDBOX-RUN-ON-FILE by the manual page reader),
;;;; so that another way of shutting a program in can take bwrap's place.

(in-package #:tuatara.io)

(defvar *sandbox-program* "bwrap"
  "The program that shuts in the programs decoding files for the editor, and
LaTeX making PDFs: bubblewrap, named bwrap, looked for on the PATH (or a full
path to it). NIL turns decoding off: images, PDFs and the like are then shown
as octets and text only; PDFs are still made of Org and LaTeX files, LaTeX
then not shut in, and each export says so.")

(defvar *sandbox-seconds* 10
  "The seconds a program decoding a file may run before it is ended, and the
processor time it may take.")

(defvar *sandbox-memory* (* 1024 1024 1024)
  "The octets of memory a program decoding a file may take (1 GB); NIL for no
limit.")

(defvar *sandbox-input-limit* (* 1024 1024 1024)
  "The largest file a program decoding files is given (1 GB): it gets a copy
in memory, which a larger file, or one claiming a huge size with nothing in
it, would fill. A larger one is not decoded, and why is said; NIL for no
limit.")

(defvar *sandbox-total-memory* (* 2 1024 1024 1024)
  "The octets of memory all the programs decoding one file may take together
(2 GB), where the system can hold them to it (systemd-run, a scope of the
user's own); NIL for no such limit, each program still held to
*SANDBOX-MEMORY*.")

(defparameter *sandbox-path* "/usr/local/bin:/usr/bin:/bin"
  "Where a program in the sandbox is looked for: the system's own directories
of programs, the only ones it sees.")

(defparameter *sandbox-said-limit* 4096
  "The octets of what a program in the sandbox writes to its error output that
are kept, to say why it failed.")

(defparameter *sandbox-file-limit* (* 64 1024 1024)
  "The octets a program in the sandbox may write to any one file of its
scratch /tmp.")

(defparameter *sandbox-process-limit* 64
  "The processes and threads a program in the sandbox may have at once, it
and all it starts counted together (only those in the sandbox).")

(defvar *sandbox-seccomp* t
  "True to have the kernel refuse a program decoding a file the system calls no
decoder needs (see the file's header), on x86-64 and AArch64; NIL to leave
them to it, the rest of the sandbox kept.")

(sb-ext:defglobal **sandbox-state** nil
  "What was found when the sandbox was last tried (SANDBOX-STATUS), as a list
(STATUS DETAIL PATH), or NIL before it is first needed.")

(sb-ext:defglobal **sandbox-scope** nil
  "systemd-run's path when it was found, when the sandbox was last tried, to
put a sandbox in a scope of its own whose memory is limited; NIL when not.")

(sb-ext:defglobal **sandbox-lock** (sb-thread:make-mutex :name "sandbox")
  "Held while the sandbox is tried, so that two threads needing it at once try it once.")

(defparameter *default-program-path* "/usr/bin:/bin"
  "Where programs are looked for when the editor was started with no PATH.")

(defun program-search-path ()
  "The directories programs are looked for in, as the PATH lists them:
the editor's own PATH, or *DEFAULT-PROGRAM-PATH* when it has none."
  (or (own-variable "PATH") *default-program-path*))

(defun find-program (name &optional (search (program-search-path)))
  "The full path of the program NAME in the directories SEARCH lists (the
PATH), or NAME itself when it has a slash and can be run; NIL when there is no
such program. Only directories named from the root (starting with /) are
looked in: an empty entry, . or bin/ would mean wherever the editor happens
to be, which may be a folder of files not to be trusted."
  (flet ((runnable (path) (and (handler-case (sb-posix:access path sb-posix:x-ok) (sb-posix:syscall-error () nil))
                               (eq :regular (path-kind path))
                               path)))
    (if (find #\/ name)
        (runnable name)
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
the system's own folders of programs (SANDBOX-FIND-PROGRAM)."
  (let ((setpriv (sandbox-find-program "setpriv")))
    (and setpriv (list* setpriv "--pdeathsig" "KILL" "--" program arguments))))

(defparameter *program-output-limit* (* 64 1024 1024)
  "The most octets PROGRAM-OUTPUT takes of what a program writes (64 MB):
one that writes more, a clipboard of a huge copy or a program gone wrong, is
stopped and its output not taken, so that it cannot fill memory.")

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
own folders and settings with OWN-VARIABLE."
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
shell and never looked for), left running; its process, for the caller to
end (END-UNWANTED-PROGRAM, DONE-WITH-PROGRAM) or to close. INPUT, OUTPUT and
ERROR say what it reads and writes, as SB-EXT:RUN-PROGRAM takes them: NIL
for nothing, :STREAM for a pipe, T for the editor's own (INPUT T keeps the
terminal, for a program that asks for a password on it), a stream on a
descriptor, or a path (IF-ERROR-EXISTS saying what is done with a file
ERROR names that is there already); :OUTPUT as ERROR joins its errors to what
it writes. With PTY, it runs on a new pseudo-terminal, its process's
SB-EXT:PROCESS-PTY. DIRECTORY is where it runs, ENVIRONMENT, when given, all
it sees of one, NAME=VALUE strings (else PROGRAM-ENVIRONMENT);
PRESERVE-FDS, descriptors it is given besides. SESSION NIL leaves it in a
process group of its own; T starts it in a session of its own (setsid), so
that it stays when the editor or its terminal goes, the process given back
being setsid's, which may end at once; :TERMINAL as well makes its
pseudo-terminal its controlling terminal and gives back a process that ends
when it ends (setsid -w -c). WITH-EDITOR true has it killed when the
editor ends, killed or not (SETPRIV-COMMAND), started from the one thread
kept for that (IN-STARTER), since it would be killed when the thread
starting it ends; refused when setpriv is not there. CLOSE-FDS, file
descriptors (FDs) handed over with the start, such as the program's end of
a socket: closed by the thread that starts it once it is started or its
start failed, so that a start given up on while under way never finds them
closed, or reused, under it. EXTERNAL-FORMAT is that of its pipes."
  (flet ((close-handed ()
           (dolist (fd close-fds)
             (tuatara.runtime:ignoring-errors-reported ("closing a descriptor handed to a program")
               (sb-posix:close fd)))))
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
shell), and call FUNCTION with its process, returning what FUNCTION returns:
INPUT, OUTPUT and ERROR say what the program reads and writes, as
SB-EXT:RUN-PROGRAM takes them (:STREAM for a pipe FUNCTION uses, NIL for
none, :OUTPUT for its errors with what it writes); DIRECTORY is where it
runs, ENVIRONMENT, when given, all it sees of one (else PROGRAM-ENVIRONMENT); PRESERVE-FDS, descriptors
it is given besides (pipes of its own, named in its arguments). With JOB, it
is the program the job runs (TUATARA.RUNTIME:JOB-PROGRAM-STARTED): stopping
the job ends it. However FUNCTION ends, the program is ended when it still
runs, with what it started (END-UNWANTED-PROGRAM), without waiting for it,
so that none is left behind; one ended already is waited for and closed."
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

(sb-ext:defglobal **starting** (sb-thread:make-mutex :name "program start")
  "Held by the thread having a program started on the starting thread, so that one is started at a time.")

(defparameter *starter-wait* 30
  "The seconds a start on the starting thread may take, a sandbox looked for
first, before it is given up and another thread made to start programs from.")

(defparameter *at-once-wait* 0.05
  "The seconds a start on a thread that must not wait may take, past which
it is left to finish in the background (IN-STARTER :AT-ONCE).")

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

(defun proc-text (path)
  "The file at PATH under /proc read whole, each octet a character (a
program's name there may be any octets); NIL when it cannot be read."
  (let ((fd (handler-case (sb-posix:open path sb-posix:o-rdonly) (sb-posix:syscall-error () nil))))
    (when fd
      (unwind-protect
           (handler-case (map 'string #'code-char (read-fd-octets fd :path path :size-hint 1024))
             (file-system-error () nil))
        (sb-posix:close fd)))))

(defun process-unreaped-p (process)
  "True while PROCESS, a program started, has not ended and been reaped: its
number is still its own, to be signalled."
  (member (sb-ext:process-status process) '(:running :stopped)))

(defun proc-numbers (path)
  "The numbers in the file at PATH under /proc, read whole as text; NIL when it cannot be read."
  (let ((text (proc-text path)))
    (and text (mapcar #'parse-integer (remove "" (uiop:split-string text :separator '(#\Space #\Newline)) :test #'string=)))))

(defun process-children (process)
  "The numbers of the processes PROCESS, a program started, has started and
not yet seen end, from /proc; NIL once PROCESS has ended, its number no
longer its own."
  (when (process-unreaped-p process)
    (let ((pid (sb-ext:process-pid process)))
      (proc-numbers (format nil "/proc/~d/task/~d/children" pid pid)))))

(defun session-leader (process)
  "The process SESSION-COMMAND's setsid, PROCESS, started in a session of its
own: its first child, whose number is the session's; NIL when it has none,
or once PROCESS has ended, its number no longer its own."
  (first (process-children process)))

(defun process-numbers ()
  "The numbers of every process there is, from /proc; NIL when it cannot be read."
  (let ((fd (handler-case (sb-posix:open "/proc" (logior sb-posix:o-rdonly sb-posix:o-directory))
              (sb-posix:syscall-error () nil)))
        (numbers '()))
    (when fd
      (unwind-protect
           (tuatara.runtime:ignoring-errors-reported ("listing the processes")
             (map-directory-fd (lambda (name)
                                 (when (and (plusp (length name)) (every (lambda (octet) (<= 48 octet 57)) name))
                                   (push (reduce (lambda (number octet) (+ (* number 10) (- octet 48)))
                                                 name :initial-value 0)
                                         numbers)))
                               fd "/proc"))
        (sb-posix:close fd)))
    numbers))

(defun process-stat (pid)
  "What /proc says of the process with process id (PID) after its name, as
strings: its state, its parent's number, its group's, its session's and the
rest; NIL when there is no such process."
  (let ((stat (proc-text (format nil "/proc/~d/stat" pid))))
    ;; pid (name) state ppid pgrp session ...: the fields after the name's closing parenthesis
    (and stat (uiop:split-string (subseq stat (+ 2 (or (position #\) stat :from-end t) -2))) :separator " "))))

(defun process-name (pid)
  "The name of the program the process with process id (PID) runs, as /proc
gives it, its first 15 octets; an empty string when there is no such process."
  (string-right-trim '(#\Newline) (or (proc-text (format nil "/proc/~d/comm" pid)) "")))

(defun process-parent (pid)
  "The number of the parent of the process with process id (PID); NIL when
there is no such process."
  (tuatara.text:whole-number (second (process-stat pid))))

(defun process-status-numbers (pid field)
  "The numbers on the line FIELD (\"Uid\", \"NSpid\") of /proc/PID/status, the
process with process id (PID), in order; NIL when there is no such process or
line."
  (let* ((status (proc-text (format nil "/proc/~d/status" pid)))
         (at (and status (search (format nil "~%~a:" field) status)))
         (start (and at (+ at (length field) 2)))
         (end (and at (or (position #\Newline status :start start) (length status)))))
    (and at
         (mapcar #'parse-integer
                 (remove "" (uiop:split-string (subseq status start end) :separator '(#\Space #\Tab)) :test #'string=)))))

(defparameter *ssh-server-names* '("sshd" "sshd-session")
  "The names under which the OpenSSH server runs, as PROCESS-NAME gives them.")

(defun started-by-ssh-server-p (pid)
  "True when the parent of the process with process id (PID) is the OpenSSH
server (*SSH-SERVER-NAMES*) run by root in this system's own namespace of
processes, as it is of the process that carries a connection for a client on
another machine. A program run by an ordinary account cannot have such a
parent, and a container with its own processes shows none. False when the
parent changes while it is looked at."
  (let ((parent (process-parent pid)))
    (and parent
         (member (process-name parent) *ssh-server-names* :test #'string=)
         (eql 0 (first (process-status-numbers parent "Uid")))
         (= 1 (length (process-status-numbers parent "NSpid")))
         (eql parent (process-parent pid)))))

(defun process-group-and-session (pid buffer)
  "The process group and the session of the process with process id (PID), as
two values, read from /proc into BUFFER, octets enough for the line it
reads \(1024); NIL when there is no such process."
  (let ((fd (handler-case (sb-posix:open (format nil "/proc/~d/stat" pid) sb-posix:o-rdonly)
              (sb-posix:syscall-error () nil))))
    (when fd
      (let* ((n (unwind-protect (or (handler-case (read-fd-into fd buffer) (file-system-error () nil)) 0)
                  (sb-posix:close fd)))
             ;; pid (name) state ppid pgrp session ...: the fields after the name's closing parenthesis
             (name-end (position 41 buffer :end n :from-end t)))
        (when name-end
          (flet ((number-at (start)
                   (loop with value = 0
                         for at from start below n
                         for octet = (aref buffer at)
                         while (<= 48 octet 57)
                         do (setf value (+ (* value 10) (- octet 48)))
                         finally (return (values value at)))))
            (let* ((group-at (1+ (nth-value 1 (number-at (+ name-end 4)))))
                   (session-at (1+ (nth-value 1 (number-at group-at)))))
              (values (number-at group-at) (number-at session-at)))))))))

(defun processes-in (groups sessions &optional (buffer (make-array 1024 :element-type '(unsigned-byte 8))))
  "The numbers of the processes in one of the process groups GROUPS or one of
the SESSIONS, from /proc; BUFFER is read into (PROCESS-GROUP-AND-SESSION)."
  (loop for pid in (process-numbers)
        when (multiple-value-bind (group session) (process-group-and-session pid buffer)
               (and group (or (member group groups) (member session sessions))))
          collect pid))

(defun session-processes (session)
  "The numbers of the processes in the session numbered SESSION, from /proc."
  (processes-in '() (list session)))

(defun signal-process-if (pid test signal)
  "Send SIGNAL to the process with process id (PID) when TEST, a function of
no arguments looking at it in /proc, is true, the process held by a
descriptor of its own (pidfd) meanwhile, so that the signal never reaches
another given its number after it ended. Where the system has no such
descriptors, sent by its number once TEST is true. True when it was sent."
  (flet ((pidfd-signal (fd signal)
           (zerop (sb-alien:alien-funcall
                   (sb-alien:extern-alien "syscall" (function sb-alien:long sb-alien:long sb-alien:int sb-alien:int
                                                              sb-alien:unsigned-long sb-alien:unsigned-int))
                   424 fd signal 0 0))))                    ; pidfd_send_signal
    (let ((fd (sb-alien:alien-funcall
               (sb-alien:extern-alien "syscall" (function sb-alien:long sb-alien:long sb-alien:int sb-alien:unsigned-int))
               434 pid 0)))                                 ; pidfd_open
      (cond ((>= fd 0)
             (unwind-protect
                  ;; still the one held when it was looked at: no other can have its number
                  (and (funcall test) (pidfd-signal fd 0) (pidfd-signal fd signal))
               (sb-posix:close fd)))
            ((= (sb-alien:get-errno) sb-posix:enosys)
             (and (funcall test)
                  (handler-case (progn (sb-posix:kill pid signal) t) (sb-posix:syscall-error () nil))))))))

(defun signal-session (process session signal)
  "Send SIGNAL to PROCESS, started by SESSION-COMMAND, with its group, while
it has not ended (its number may be another's after), and to every process
of the session numbered SESSION (SESSION-LEADER), when known. True when one
of the session's processes was still there."
  (when (process-unreaped-p process)
    (tuatara.runtime:ignoring-errors-reported ("signalling ~a" process)
      (sb-ext:process-kill process signal :process-group)))
  (let ((there (and session (session-processes session))))
    (dolist (pid there)
      (handler-case (sb-posix:kill pid signal) (sb-posix:syscall-error () nil)))
    (and there t)))

(defun program-output (command &key input (capture t) (seconds 1) wait (environment nil environment-p) directory
                                    (most *program-output-limit*))
  "Run COMMAND, a program's full name and its arguments (never through a
shell), giving it INPUT (a string, through a pipe: never written to a file)
and, with CAPTURE, taking what it writes, as UTF-8 text (an octet that is
not UTF-8 as its raw byte's character), MOST octets of it at most (a program writing more is stopped, its output not taken); SECONDS at
most, then it is stopped. WAIT, when given, does the waiting instead of this
thread blocking: a function of the thread doing the work and SECONDS (the
editor's interruptible wait, say). ENVIRONMENT, when given, is all the
program sees of one; DIRECTORY, where it runs. Two values: its output (with
CAPTURE) and true when it ended well. A program that stays behind (wl-copy,
serving the clipboard) is not waited for: only the one started."
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
                                 (sb-ext:process-kill process 15))))
               (values nil nil))
        (values-list (sb-thread:join-thread worker :default nil)))))

(defparameter *program-said-limit* (* 64 1024)
  "The most octets kept of what a program says on its error output
\(FILTER-THROUGH-PROGRAM); the rest is read and let go.")

(defun read-outputs (fds limits drains &key deadline too-much)
  "Read FDS, pipes open for reading, each to its end, on this thread alone,
waiting on them all at once so that none waits on another: of each, the
first octets up to its limit in LIMITS (NIL: all) are kept; past it, the
rest is read and let go when its flag in DRAINS is true, else that pipe is
no longer read and TOO-MUCH, a function of no arguments, is called. Reading
stops at DEADLINE (TUATARA.RUNTIME:DEADLINE-IN) when one is given. Three
values: a list of what each gave, as octets, NIL for one that gave more than
its limit and was not drained; a list of true for each that gave more than
its limit; and true when DEADLINE came before every pipe ended."
  (let ((pieces (loop repeat (length fds) collect (make-octet-pieces)))
        (over (make-list (length fds)))
        (open (copy-list fds))
        (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop while open
          until (and deadline (tuatara.runtime:deadline-passed-p deadline))
          do (let ((ready (wait-for-fds open (if deadline (min 1 (max 0 (tuatara.runtime:seconds-left deadline))) 1))))
               (when (consp ready)
                 (dolist (fd ready)
                   (let* ((i (position fd fds))
                          (into (nth i pieces))
                          (limit (nth i limits))
                          (got (read-fd-into fd buffer :path "a program's output")))
                     (cond ((zerop got) (setf open (remove fd open)))
                           ((nth i over))
                           (t (add-octet-piece into (subseq buffer 0 got))
                              (when (and limit (> (octet-pieces-size into) limit))
                                (setf (nth i over) t)
                                (unless (nth i drains)
                                  (setf open (remove fd open))
                                  (when too-much (funcall too-much)))))))))))
    (values (loop for into in pieces
                  for limit in limits
                  for past in over
                  for drain in drains
                  collect (cond ((not past) (joined-octets into))
                                (drain (subseq (joined-octets into) 0 limit))))
            over
            (and open t))))

(defun filter-through-program (command write &key job directory (environment nil environment-p)
                                                 (limit *program-output-limit*) truncate input seconds)
  "Run COMMAND, a program's full name and its arguments (never through a
shell), calling WRITE with the descriptor of its standard input to give it
what it reads, a piece at a time (WRITE-OCTETS; a buffer's text from
BUFFER-TEXT, say), its input closed once WRITE returns; what it prints and
what it says on its error output are read meanwhile on a thread of their
own, so that none waits on another. A program that ends before it has read
all makes WRITE's next write fail (FILE-SYSTEM-ERROR), which WRITE may take
as the end. With WRITE NIL, INPUT is what it reads instead, as
SB-EXT:RUN-PROGRAM takes it: T, the editor's own standard input, so that a
program asking for a password on the terminal (ssh, ssh-add) can; a file
stream or path; NIL, nothing. With JOB, it is the program the job runs:
stopping the job ends it. DIRECTORY is where it runs, ENVIRONMENT, when
given, all it sees of one. With SECONDS, it is given that long, then ended
with every program it started (END-UNWANTED-PROGRAM), waited for while it
ends, *PROGRAM-END-GRACE* seconds and a little more at most. Four values:
what it printed, as octets (NIL when it printed more than LIMIT octets, and
it is ended then; with TRUNCATE, the first LIMIT octets, the rest read and
let go while it runs on; a LIMIT of NIL takes all it prints); what it said,
as text, its first *PROGRAM-SAID-LIMIT* octets; its exit code, NIL when it
did not exit of itself or was ended for taking too long; and true when it
printed more than LIMIT."
  (apply #'call-with-program command
         (lambda (process)
           (let* ((deadline (and seconds (tuatara.runtime:deadline-in seconds)))
                  (reader (tuatara.runtime:make-guarded-thread
                           (lambda ()
                             (destructuring-bind (&optional said over (late t))
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
                                                                (sb-ext:process-kill process 15))))))
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

(defun command-words (text &key (backslash :shell))
  "TEXT split into a program's arguments as a shell splits a command line,
though no shell runs: at spaces, tabs and line ends, but within '...', which
keeps all it holds as it is, or \"...\", either quote holding the other.
BACKSLASH says what a backslash does: :SHELL, outside quotes it keeps the
character after it as it is, within \"...\" the \" or \\ after it, and is
itself before any other; :ESCAPES, outside quotes it is itself (a Windows
path comes through whole), within \"...\" it begins one of \\t \\n \\r \\\\ and
\\\", a tab, a line end, a carriage return, a backslash and a quote, and any
other is refused. Two values, the words and T; or NIL, NIL and why as a third
when a quote is left open or an escape is refused."
  (let ((words '()) (word (make-string-output-stream)) (in-word nil) (quote-char nil) (i 0) (n (length text)))
    (flet ((end-word () (when in-word (push (get-output-stream-string word) words) (setf in-word nil)))
           (next () (and (< (1+ i) n) (char text (1+ i)))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((eql quote-char #\')
                        (if (char= c #\') (setf quote-char nil) (write-char c word)))
                       ((eql quote-char #\")
                        (cond ((char= c #\") (setf quote-char nil))
                              ((char/= c #\\) (write-char c word))
                              ((eq backslash :escapes)
                               (let ((escaped (case (next) (#\t #\Tab) (#\n #\Newline) (#\r #\Return) (#\\ #\\) (#\" #\"))))
                                 (unless escaped
                                   (return-from command-words
                                     (values nil nil (format nil "within double quotes a backslash begins an escape, and only \\t \\n \\r \\\\ \\\" are taken: ~a; single quotes take a backslash as it is"
                                                             (subseq text i (min n (+ i 2)))))))
                                 (incf i) (write-char escaped word)))
                              ((member (next) '(#\" #\\)) (incf i) (write-char (char text i) word))
                              (t (write-char c word))))
                       ((member c '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11))) (end-word))
                       ((member c '(#\' #\")) (setf quote-char c in-word t))
                       ((and (char= c #\\) (eq backslash :shell) (next)) (incf i) (write-char (char text i) word) (setf in-word t))
                       (t (write-char c word) (setf in-word t))))
               (incf i))
      (if quote-char
          (values nil nil "a quote is left open")
          (progn (end-word) (values (nreverse words) t))))))

(defun system-directory-arguments ()
  "bwrap's arguments showing the program the system's directories, read only:
/usr, the directories beside it that are links into it on most systems (made
links in the sandbox too) or directories of their own on others, and what
programs read to find libraries and alternatives."
  (append (list "--ro-bind" "/usr" "/usr")
          (loop for directory in '("/bin" "/sbin" "/lib" "/lib64" "/lib32")
                for target = (handler-case (sb-posix:readlink directory) (sb-posix:syscall-error () nil))
                append (cond (target (list "--symlink" target directory))
                             ((eq :directory (path-kind directory))
                              (list "--ro-bind" directory directory))))
          (loop for path in '("/etc/ld.so.cache" "/etc/alternatives" "/etc/fonts")
                append (list "--ro-bind-try" path path))))

(defun version-numbers (text &optional (after ""))
  "The numbers of the first version written in TEXT after AFTER, a regular
expression, as a list: (0 11 0) for \"bubblewrap 0.11.0\", (9 9) for
\"OpenSSH_9.9p1, OpenSSL 3.2.2\" after \"OpenSSH_\"; NIL when there is none.
A pre-release after a -, alpha, beta or rc and its number, adds -3, -2 or -1
and that number, so that it comes before its release (VERSION-AT-LEAST-P):
0.90.13-rc1 is (0 90 13 -1 1)."
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings (concatenate 'string after "(\\d+(?:\\.\\d+)*)(?:-(alpha|beta|rc)\\.?(\\d*))?") text)
    (when match
      (append (mapcar #'parse-integer (uiop:split-string (aref parts 0) :separator "."))
              (let ((label (aref parts 1)))
                (and label (list (cond ((string= label "alpha") -3) ((string= label "beta") -2) (t -1))
                                 (or (parse-integer (aref parts 2) :junk-allowed t) 0))))))))

(defun version-at-least-p (version needed)
  "True when VERSION, a list of numbers as VERSION-NUMBERS gives, is NEEDED,
another, or later: each number compared in turn, one left out counted 0."
  (loop for i below (max (length version) (length needed))
        for have = (or (nth i version) 0) and need = (or (nth i needed) 0)
        when (> have need) return t
        when (< have need) return nil
        finally (return t)))

(defvar *bwrap-versions* (make-hash-table :test 'equal :synchronized t)
  "The version of each bwrap program asked so far, by its path: a list of numbers, or :UNKNOWN.")

(defun bwrap-version-at-least-p (major minor)
  "True when the bwrap found for *SANDBOX-PROGRAM* is version MAJOR.MINOR or later, as it says it is."
  (let ((program (and *sandbox-program* (find-program *sandbox-program*))))
    (when program
      (let ((version (or (gethash program *bwrap-versions*)
                         (setf (gethash program *bwrap-versions*)
                               (or (version-numbers (version-text program)) :unknown)))))
        (and (consp version) (version-at-least-p version (list major minor)))))))

(defparameter *seccomp-refused*
  '((:x86-64 #xC000003E
     ;; acct add_key keyctl request_key bpf chroot pivot_root mount umount2 unshare setns
     163 248 250 249 321 161 155 165 166 272 308
     ;; init_module finit_module delete_module kexec_load kexec_file_load reboot swapon swapoff syslog
     175 313 176 246 320 169 167 168 103
     ;; quotactl quotactl_fd perf_event_open ptrace process_vm_readv process_vm_writev userfaultfd
     179 443 298 101 310 311 323
     ;; uselib lookup_dcookie open_by_handle_at name_to_handle_at fanotify_init
     134 212 304 303 300
     ;; open_tree move_mount fsopen fsconfig fsmount fspick mount_setattr
     428 429 430 431 432 433 442
     ;; io_uring_setup io_uring_enter io_uring_register vhangup clock_adjtime clock_settime
     425 426 427 153 305 227
     ;; settimeofday adjtimex iopl ioperm nfsservctl pidfd_getfd
     164 159 172 173 180 438
     ;; mknod mknodat
     133 259)
    (:arm64 #xC00000B7
     89 217 219 218 280 51 41 40 39 97 268
     105 273 106 104 294 142 224 225 116
     60 443 241 117 270 271 282
     18 265 264 262
     428 429 430 431 432 433 442
     425 426 427 58 266 112
     170 171 42 438
     33))
  "For each processor the seccomp filter knows (a feature of the Lisp, then
the kernel's AUDIT_ARCH number), the system calls refused, by number.")

(defparameter *seccomp-calls*
  '((:x86-64 :clone 56 :clone3 435 :ioctl 16 :socket 41)
    (:arm64 :clone 220 :clone3 435 :ioctl 29 :socket 198))
  "For each processor, the numbers of the system calls the seccomp filter
looks into the arguments of.")

(defun seccomp-program ()
  "The seccomp filter of the sandbox as the octets of a classic BPF program,
as bwrap's --seccomp reads it, for the processor here; NIL on one it does not
know. Another processor's calls (32-bit ones on x86-64) end the program; the
calls of *SECCOMP-REFUSED* fail with EPERM, clone3 with ENOSYS (so that its
flags, out of reach of the filter, are asked through clone), clone making a
namespace, ioctl pushing input into a terminal (TIOCSTI, TIOCLINUX) and a
socket that is not a local one (AF_UNIX) fail too; everything else is let be."
  (let* ((cpu (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*))
         (calls (and cpu (rest (assoc (first cpu) *seccomp-calls*))))
         (code '()))
    (when cpu
      (flet ((op (opcode jt jf k) (push (list opcode jt jf k) code))
             (ret (k) (push (list #x06 0 0 k) code)))
        (let ((allow #x7FFF0000) (kill #x80000000)
              (eperm (logior #x00050000 1)) (enosys (logior #x00050000 38)) (eafnosupport (logior #x00050000 97)))
          (op #x20 0 0 4)                                   ; the processor
          (op #x15 1 0 (second cpu))
          (ret kill)
          (op #x20 0 0 0)                                   ; the call
          (when (eq (first cpu) :x86-64)                    ; x32 calls
            (op #x35 0 1 #x40000000)
            (ret eperm))
          (dolist (number (cddr cpu))
            (op #x15 0 1 number)
            (ret eperm))
          (op #x15 0 1 (getf calls :clone3))
          (ret enosys)
          ;; clone with CLONE_NEWNS, NEWCGROUP, NEWUTS, NEWIPC, NEWUSER, NEWPID, NEWNET or NEWTIME
          (op #x15 0 4 (getf calls :clone))
          (op #x20 0 0 16)
          (op #x45 0 1 #x7E020080)
          (ret eperm)
          (op #x20 0 0 0)
          ;; ioctl TIOCSTI and TIOCLINUX
          (op #x15 0 5 (getf calls :ioctl))
          (op #x20 0 0 24)
          (op #x15 1 0 #x5412)
          (op #x15 0 1 #x541C)
          (ret eperm)
          (op #x20 0 0 0)
          ;; socket of another family than AF_UNIX
          (op #x15 0 4 (getf calls :socket))
          (op #x20 0 0 16)
          (op #x15 1 0 1)
          (ret eafnosupport)
          (op #x20 0 0 0)
          (ret allow)))
      (let ((octets (make-array (* 8 (length code)) :element-type '(unsigned-byte 8))))
        (loop for (opcode jt jf k) in (reverse code)
              for at from 0 by 8
              do (octets-put-integer octets opcode 2 :at at)
                 (setf (aref octets (+ at 2)) jt (aref octets (+ at 3)) jf)
                 (octets-put-integer octets k 4 :at (+ at 4)))
        octets))))

(defun sandbox-filtered-p ()
  "True when programs in the sandbox run under its seccomp filter: turned on
(*SANDBOX-SECCOMP*) and a processor it knows."
  (and *sandbox-seccomp* (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*) t))

(defun sealed-memory-file (name fill &key kept)
  "A descriptor of a new file in memory called NAME, FILL (a function of its
descriptor) having written it, at its start, sealed so that nothing, not even
a program given it, can change it: not written, not made shorter or longer;
with KEPT kept open in programs started (for bwrap to read). Closed, and an
error signalled, when it cannot be made, filled or sealed."
  (let ((fd (sb-alien:alien-funcall
             (sb-alien:extern-alien "memfd_create" (function sb-alien:int sb-alien:c-string sb-alien:unsigned-int))
             name (if kept 2 3))))                          ; MFD_ALLOW_SEALING, and MFD_CLOEXEC when not kept
    (when (minusp fd) (fail "make a file in memory called" name))
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (tuatara.runtime:ignoring-errors-reported ("closing the file in memory ~a" name)
                              (sb-posix:close fd)))))
      (funcall fill fd)
      (when (minusp (fcntl-int fd 1033 15))                 ; F_ADD_SEALS: SEAL, SHRINK, GROW, WRITE
        (fail "seal" "a file in memory"))
      (sb-posix:lseek fd 0 sb-posix:seek-set)
      fd)))

(defun memory-file (name octets &key (kept t))
  "A descriptor of a new file in memory holding OCTETS, at its start, sealed
so that nothing can change it (SEALED-MEMORY-FILE), and with KEPT kept open
in programs started (for bwrap to read)."
  (sealed-memory-file name (lambda (fd) (write-octets fd octets :path name)) :kept kept))

(defun sealed-file-size (fd)
  "The size in octets of the file open as file descriptor (FD) when it is
sealed so that nothing can write it, make it shorter or make it longer, as
SEALED-MEMORY-FILE seals one; NIL when it is not."
  (let ((seals (fcntl-int fd 1034 0)))                      ; F_GET_SEALS
    (and (>= seals 0) (= 14 (logand seals 14))               ; SHRINK, GROW, WRITE
         (sb-posix:stat-size (sb-posix:fstat fd)))))

(defparameter *sandbox-own-directories* '("/usr" "/bin" "/sbin" "/lib" "/lib64" "/lib32" "/tmp" "/proc" "/dev" "/etc")
  "The directories at the root that the sandbox makes itself, never made links
because they are links outside it.")

(defun root-link (path)
  "The link at the root PATH begins with (/home leading to /var/home) and what
it leads to, as two values, or NIL when its top is no link or a directory the
sandbox makes itself (*SANDBOX-OWN-DIRECTORIES*)."
  (let* ((top (subseq path 0 (or (position #\/ path :start 1) (length path))))
         (target (and (> (length top) 1) (not (member top *sandbox-own-directories* :test #'string=))
                      (handler-case (sb-posix:readlink top) (sb-posix:syscall-error () nil)))))
    (and target (values top target))))

(defun root-link-arguments (paths)
  "bwrap's arguments making in the sandbox the links at the root that PATHS
go through here, each as it is here (/home leading to /var/home, where the
system has it so), so that a path written either way finds what is shown at
its real place."
  (let ((made '()))
    (loop for path in paths
          append (multiple-value-bind (top target) (root-link path)
                   (when (and target (not (member top made :test #'string=)))
                     (push top made)
                     (list "--symlink" target top))))))

(defun root-resolved (path)
  "PATH with a link at the root it begins with (/home leading to /var/home)
followed, as the sandbox makes it (ROOT-LINK-ARGUMENTS), the rest as written;
no / at its end."
  (let ((path (string-right-trim "/" path)))
    (multiple-value-bind (top target) (root-link path)
      (if target
          (concatenate 'string (if (char= (char target 0) #\/) "" "/") (string-right-trim "/" target) (subseq path (length top)))
          path))))

(defun real-place (path)
  "PATH with every link in it followed as far as it exists, the rest as it is
\(REAL-PATH-SO-FAR); PATH as it is under a directory the sandbox makes itself
\(*SANDBOX-OWN-DIRECTORIES*: /tmp is its own there). No / at its end."
  (if (some (lambda (own) (or (string= path own) (uiop:string-prefix-p (concatenate 'string own "/") path)))
            *sandbox-own-directories*)
      (string-right-trim "/" path)
      (real-path-so-far path)))

(defun sandbox-place (entry)
  "Where ENTRY of a sandbox's directories goes, as two values, the directory
shown and where the program sees it: (SOURCE . TARGET); a path alone, shown
at its own place. A place outside the directories the sandbox makes itself
is its real one (REAL-PLACE), so that it is found however it is written."
  (if (consp entry)
      (values (car entry) (real-place (cdr entry)))
      (let ((real (real-place entry)))
        (values real real))))

(defun mount-arguments (directories writable overlays)
  "bwrap's arguments showing DIRECTORIES read only, WRITABLE ones to write in
and OVERLAYS, as SANDBOX-RUN has them, those that exist, a directory in
another shown after it, so that both are seen; with the links at the root
their paths go through (ROOT-LINK-ARGUMENTS). A directory a link leads to is
shown where it is written too, so that a path through the link finds it. The
root itself is never shown: the sandbox's root is its own. A directory to
write in that is, or holds, a place nothing may be written in is refused
\(CHECK-TREE-WRITING)."
  (let ((mounts '()) (written '()))
    (flet ((add (entry how)
             (multiple-value-bind (source target) (sandbox-place entry)
               (when (eq how :write)
                 (check-tree-writing source "let a program write in"))
               (push (if (consp entry) (cdr entry) entry) written)
               ;; at its real place, and where it is written when a link leads there (~/scratch to /tmp/scratch)
               (dolist (place (remove-duplicates (list target (if (consp entry) target (root-resolved entry))) :test #'string=))
                 (push (cons place (ecase how
                                     (:read (list "--ro-bind-try" source place))
                                     (:write (list "--bind-try" source place))))
                       mounts)))))
      (dolist (entry directories) (add entry :read))
      (dolist (entry writable) (add entry :write))
      (loop for (source upper work) in overlays
            do (multiple-value-bind (lower target) (sandbox-place source)
                 (push source written)
                 (push (cons target (if (bwrap-version-at-least-p 0 11)
                                        (list "--overlay-src" lower "--overlay" upper work target)
                                        (list "--ro-bind-try" lower target)))
                       mounts))))
    ;; the last given for a place wins; a parent before what is in it; the root itself never
    (setf mounts (stable-sort (remove-duplicates (remove "" (nreverse mounts) :key #'car :test #'string=)
                                                 :key #'car :test #'string= :from-end nil)
                              #'< :key (lambda (mount) (length (car mount)))))
    (append (root-link-arguments (reverse written))
            (loop for mount in mounts append (cdr mount)))))

(defun sandbox-arguments (program arguments seconds memory &key seccomp files environment directories writable overlays
                                                              (directory "/tmp") (file-limit *sandbox-file-limit*)
                                                              (processes *sandbox-process-limit*) (scratch t)
                                                              (descriptors nil))
  "Bwrap's arguments for running PROGRAM with ARGUMENTS shut in, limited to
SECONDS of processor time (NIL: no limit) and MEMORY octets (NIL: no limit)
each process, its scratch /tmp to MEMORY octets too, to PROCESSES processes
and threads, and to files of FILE-LIMIT octets (NIL: no limit); SECCOMP the
file descriptor (FD) of its seccomp filter, FILES the descriptors of files
it is shown read only, each \(PATH . FD), ENVIRONMENT what it has in its
environment besides, each (NAME VALUE), a later one for the same NAME in
place of the earlier, DIRECTORIES, WRITABLE and OVERLAYS the directories it
is shown as SANDBOX-RUN says, and DIRECTORY the one it starts in. Without
SCRATCH it has no /tmp at all; DESCRIPTORS, when given, is the most files
it may have open at once."
  (append (list "--unshare-all" "--unshare-user" "--disable-userns" "--die-with-parent" "--new-session"
                "--cap-drop" "ALL" "--clearenv"
                "--setenv" "PATH" *sandbox-path* "--setenv" "HOME" "/tmp" "--setenv" "LC_ALL" "C.UTF-8")
          (loop for (name value) in environment append (list "--setenv" name value))
          (system-directory-arguments)
          (list "--proc" "/proc" "--dev" "/dev")
          ;; the scratch /tmp is memory too: held to the memory a program may take, where bwrap can (0.10 on)
          (and scratch memory (bwrap-version-at-least-p 0 10) (list "--size" (princ-to-string memory)))
          (and scratch (list "--tmpfs" "/tmp"))
          (mount-arguments directories writable overlays)
          (loop for (path . fd) in files append (list "--ro-bind-data" (princ-to-string fd) path))
          (and seccomp (list "--seccomp" (princ-to-string seccomp)))
          (list "--chdir" directory
                "--" "/bin/sh" "-c"
                "ulimit -c 0 && ulimit -v \"$1\" && ulimit -t \"$2\" && ulimit -f \"$3\" && { ulimit -u \"$4\" 2>/dev/null || ulimit -p \"$4\"; } && { [ \"$5\" = as-is ] || ulimit -n \"$5\"; } && shift 5 && exec \"$@\""
                "sh"
                (if memory (princ-to-string (ceiling memory 1024)) "unlimited")
                (if seconds (princ-to-string (max 1 (ceiling seconds))) "unlimited")
                (if file-limit (princ-to-string (ceiling file-limit 512)) "unlimited")
                (princ-to-string processes)
                (if descriptors (princ-to-string descriptors) "as-is")
                program)
          arguments))

(defun said-text (octets)
  "OCTETS a program wrote to say something, as text, trimmed, octets that are
not UTF-8 shown as ?."
  (string-trim '(#\Space #\Newline #\Return #\Tab)
               (tuatara.text:lenient-string octets)))

(defun scope-environment ()
  "What systemd-run needs of the environment to reach your own service
manager, and nothing else."
  (loop for name in '("XDG_RUNTIME_DIR" "DBUS_SESSION_BUS_ADDRESS")
        for value = (own-variable name)
        when value collect (format nil "~a=~a" name value)))

(defun scope-arguments (total unit)
  "systemd-run's arguments putting what follows them in a scope of its own
named UNIT (SCOPE-NAME), its processes together held to TOTAL octets of
memory, none of it swapped, the scope let go once they have all ended."
  (list "--user" "--scope" "--quiet" "--collect" "--unit" unit
        "-p" (format nil "MemoryMax=~d" total) "-p" "MemorySwapMax=0" "--"))

(defun version-text (program)
  "What PROGRAM says of its version (--version), its environment empty; an
empty string when it says nothing within five seconds."
  (or (program-output (list program "--version") :seconds 5 :environment '()) ""))

(defun try-scope ()
  "systemd-run's path when it can put a program in a scope of your own here, else NIL."
  (let ((path (find-program "systemd-run")))
    (and path
         (nth-value 1 (program-output (append (list path) (scope-arguments (* 64 1024 1024) (scope-name))
                                              (list "/bin/true"))
                                      :capture nil :seconds 10 :environment (scope-environment)))
         path)))

;;; Scopes named for the editor that made them

(defparameter *scope-prefix* "tuatara-sandbox-"
  "How the name of every scope Tuatara has systemd-run make begins (SCOPE-NAME).")

(sb-ext:defglobal **scope-owner** nil
  "This run of the editor as the names of its scopes hold it (SCOPE-OWNER),
made when first needed; NIL until then.")

(sb-ext:defglobal **scope-counter** (list 0)
  "Numbers the scopes this run of the editor has made.")

(defun process-start-time (pid)
  "When the process with process id (PID) started, in clock ticks since the
system started, as /proc gives it (a string); NIL when there is no such
process. Two processes given the same number one after the other never
have the same."
  ;; the 22nd field of its stat, the 20th after the name
  (nth 19 (process-stat pid)))

(defun pid-namespace ()
  "The number of the namespace of process ids (PID) this editor runs in,
from /proc, as a string; NIL when it cannot be read."
  (let ((link (handler-case (sb-posix:readlink "/proc/self/ns/pid") (sb-posix:syscall-error () nil))))
    ;; pid:[4026531836]
    (and link (let ((open (position #\[ link)) (close (position #\] link)))
                (and open close (< open close) (subseq link (1+ open) close))))))

(defun scope-owner ()
  "This run of the editor, as the names of its scopes hold it: its namespace
of process ids (PID), its process id and its start time (PROCESS-START-TIME),
joined by dashes; no other process has all three while it runs."
  (or **scope-owner**
      (let ((pid (sb-posix:getpid)))
        (setf **scope-owner** (format nil "~a-~d-~a" (pid-namespace) pid (process-start-time pid))))))

(defun scope-name ()
  "A new name for a scope of this run of the editor: *SCOPE-PREFIX*, its
owner (SCOPE-OWNER) and a number, as tuatara-sandbox-4026531836-4242-1785-7.scope."
  (format nil "~a~a-~d.scope" *scope-prefix* (scope-owner) (sb-ext:atomic-incf (car **scope-counter**))))

(defun scope-name-owner (name)
  "The namespace of process ids (PID), process id and start time of the
editor that made the scope called NAME, as three strings; NIL when NAME is
not one SCOPE-NAME makes."
  (let ((end (- (length name) (length ".scope"))))
    (when (and (uiop:string-prefix-p *scope-prefix* name) (uiop:string-suffix-p name ".scope")
               (< (length *scope-prefix*) end))
      (let ((parts (uiop:split-string (subseq name (length *scope-prefix*) end) :separator "-")))
        (when (and (= 4 (length parts))
                   (every (lambda (part) (and (plusp (length part)) (every #'digit-char-p part))) parts))
          (values-list (butlast parts)))))))

(defun scope-left-p (name)
  "True when the scope called NAME is one an editor made (SCOPE-NAME) that
has ended: an editor of this namespace of process ids (PID) whose process
is gone, or whose number is another's now. A scope of an editor running,
of another namespace, or not made by an editor at all, is never left."
  (multiple-value-bind (namespace pid start) (scope-name-owner name)
    (and namespace
         (equal namespace (pid-namespace))
         (not (equal start (process-start-time (parse-integer pid)))))))

(defun stop-scopes (names)
  "Have the service manager stop the scopes NAMES, ending any process left
in them, without waiting for it; true when it took the request."
  (let ((systemctl (find-program "systemctl")))
    (and systemctl names
         (nth-value 1 (program-output (list* systemctl "--user" "stop" "--no-block" names)
                                      :capture nil :seconds 10 :environment (scope-environment))))))

(defun end-scopes-left ()
  "Stop the scopes left by editors that ended before theirs were let go
\(SCOPE-LEFT-P); how many."
  (let* ((systemctl (find-program "systemctl"))
         (listed (and systemctl
                      (program-output (list systemctl "--user" "list-units" "--all" "--plain" "--no-legend"
                                            (format nil "~a*" *scope-prefix*))
                                      :seconds 10 :environment (scope-environment))))
         (left (loop for line in (and listed (uiop:split-string listed :separator '(#\Newline)))
                     for words = (remove "" (uiop:split-string line :separator " ") :test #'string=)
                     ;; the name first, after a mark when the unit is in trouble
                     for name = (find-if #'scope-name-owner words :end (min 2 (length words)))
                     when (and name (scope-left-p name)) collect name)))
    (if (and left (stop-scopes left)) (length left) 0)))

;;; Sandboxes running, and their end

(defparameter *sandbox-mark-name* "TUATARA_SANDBOX"
  "The name in bwrap's own environment that marks it as one Tuatara started:
the program shut in never sees it, its environment cleared.")

(sb-ext:defglobal **sandbox-mark** nil
  "What SANDBOX-MARK gives, made when first needed; NIL until then.")

(defun sandbox-mark ()
  "The NAME=VALUE put in the environment of bwrap itself in every sandbox this
run of the editor starts, by which they are known in /proc: *SANDBOX-MARK-NAME*
and a name no one can guess."
  (or **sandbox-mark**
      (setf **sandbox-mark** (format nil "~a=~a" *sandbox-mark-name* (random-name)))))

(sb-ext:defglobal **sandboxes** '()
  "The sandboxes running, each (PROCESS . THREAD): bwrap, as started, and the
thread that started it, from their start until they are ended (END-SANDBOX).")

(sb-ext:defglobal **sandboxes-lock** (sb-thread:make-mutex :name "sandboxes")
  "Held while **SANDBOXES** changes.")

(sb-ext:defglobal **sandboxes-closed** nil
  "True once the editor is ending: a sandbox started after then is ended at once.")

(defun end-sandbox (process &key (wait t))
  "End PROCESS, bwrap as started (START-SANDBOX), and everything in its
sandbox, at once: the first process of the sandbox, bwrap's child, whose end
ends every process in it, then bwrap with its process group, which holds the
sandbox's first process too while bwrap is still making the sandbox. Its end
is waited for here, or, unless WAIT, on a thread of its own. Ended while
systemd-run was still having its scope made, before it became bwrap, that
scope is stopped then: the service manager would otherwise keep it, empty,
for good. Any other program started is ended the same way, with what it
started."
  (let ((unit (getf (sb-ext:process-plist process) :scope))
        (starting nil))
    (when (process-unreaped-p process)
      (let ((pid (sb-ext:process-pid process)))
        (setf starting (and unit (string= "systemd-run" (process-name pid))))
        (dolist (child (process-children process))
          (signal-process-if child (lambda () (eql pid (process-parent child))) sb-unix:sigkill))
        (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
          (sb-ext:process-kill process sb-unix:sigkill :process-group))))
    (sb-thread:with-mutex (**sandboxes-lock**)
      (setf **sandboxes** (remove process **sandboxes** :key #'car)))
    (flet ((reap ()
             (tuatara.runtime:ignoring-errors-reported ("waiting for ~a to end" process) (sb-ext:process-wait process))
             (tuatara.runtime:ignoring-errors-reported ("closing ~a" process) (sb-ext:process-close process))
             (when starting (stop-scopes (list unit)))))
      (if wait
          (reap)
          (tuatara.runtime:make-guarded-thread #'reap :name "sandbox ending")))))

(defun end-sandboxes (&key thread)
  "End every sandbox running (END-SANDBOX), or, with THREAD, those started
from that thread, for it to call before it ends: a sandbox ends when the
thread that started it ends, and ended that way at the wrong moment it may
leave a process behind."
  (dolist (process (sb-thread:with-mutex (**sandboxes-lock**)
                     (loop for (process . started-by) in **sandboxes**
                           when (or (null thread) (eq thread started-by))
                             collect process)))
    (end-sandbox process)))

(defun close-sandboxes ()
  "End every sandbox running, and any started after, as the editor ends."
  (setf **sandboxes-closed** t)
  (end-sandboxes))

(defun forget-sandboxes ()
  "End every sandbox running, as a saved image is made, and forget the mark of
this run's sandboxes and the owner its scopes are named for: each run of the
image has its own."
  (end-sandboxes)
  (setf **sandbox-mark** nil
        **scope-owner** nil))

(pushnew 'close-sandboxes sb-ext:*exit-hooks*)
(pushnew 'forget-sandboxes sb-ext:*save-hooks*)

(defun process-marked-p (pid)
  "True when the process with process id (PID) has *SANDBOX-MARK-NAME* in its
environment: bwrap of a sandbox Tuatara started."
  (let ((environment (proc-text (format nil "/proc/~d/environ" pid)))
        (name (format nil "~a=" *sandbox-mark-name*)))
    (and environment
         (or (uiop:string-prefix-p name environment)
             (search (concatenate 'string (string (code-char 0)) name) environment))
         t)))

(defun sandbox-left-p (pid)
  "True when the process with process id (PID) is a sandbox's first process
left behind: bwrap, marked (PROCESS-MARKED-P), the first of a namespace of
processes of its own, its parent no marked bwrap. That process waits for
bwrap's go-ahead before it makes the sandbox; bwrap ended before it gives
it, it waits for ever, and the kernel lets no signal but SIGKILL end it."
  (and (string= "bwrap" (process-name pid))
       (process-marked-p pid)
       (let ((numbers (process-status-numbers pid "NSpid")))
         ;; its number in its own namespace, the last, is 1
         (and (> (length numbers) 1) (eql 1 (car (last numbers)))))
       (let ((parent (process-parent pid)))
         (and parent (not (process-marked-p parent))))))

(defun end-sandboxes-left ()
  "End the sandboxes left behind by an editor ended before it could end them
\(SANDBOX-LEFT-P), with SIGKILL; how many."
  (loop for pid in (process-numbers)
        count (and (sandbox-left-p pid)
                   (signal-process-if pid (lambda () (sandbox-left-p pid)) sb-unix:sigkill))))

(defun start-sandbox (bwrap program arguments stdin seconds memory
                      &rest keys &key files (total *sandbox-total-memory*) (outputs :stream) started &allow-other-keys)
  "Start bwrap at the path BWRAP on PROGRAM with ARGUMENTS, STDIN its input,
shown FILES, each (PATH . TEXT), as SANDBOX-RUN has them, KEYS the rest of
SANDBOX-ARGUMENTS's; the process, among the sandboxes running until it is
ended (END-SANDBOX). In a scope whose memory is held to TOTAL
\(*SANDBOX-TOTAL-MEMORY* unless given) when systemd-run can make one. Its
outputs are streams to read, or let go when OUTPUTS is NIL. STARTED, when
given, is called with the process as soon as it is counted, before anything
can stop this thread, to keep it where the caller ends it."
  (let ((seccomp nil) (data '()))
    (unwind-protect
         (let* ((program-octets (and *sandbox-seccomp* (seccomp-program)))
                (arguments (progn
                             (when program-octets (setf seccomp (memory-file "tuatara-seccomp" program-octets)))
                             (loop for (path . text) in files
                                   do (push (cons path (memory-file "tuatara-sandbox-file"
                                                                    (tuatara.text:string-octets text)))
                                            data))
                             (apply #'sandbox-arguments program arguments seconds memory
                                    :seccomp seccomp :files (reverse data)
                                    (alexandria:remove-from-plist keys :files :total :outputs :started))))
                (keep (remove nil (cons seccomp (mapcar #'cdr data))))
                (scope (and total **sandbox-scope**))
                (unit (and scope (scope-name))))
           ;; counted among those running before this thread can be ended
           (sb-sys:without-interrupts
             (let ((process
                     (if scope
                         ;; systemd-run moves itself into the scope and becomes bwrap: still this one's child
                         (start-program (list* scope (append (scope-arguments total unit) (list bwrap) arguments))
                                        :input stdin :preserve-fds keep :output outputs :error outputs
                                        :external-format :default :environment (cons (sandbox-mark) (scope-environment)))
                         (start-program (cons bwrap arguments)
                                        :input stdin :preserve-fds keep :output outputs :error outputs
                                        :external-format :default :environment (list (sandbox-mark))))))
               (when unit (setf (getf (sb-ext:process-plist process) :scope) unit))
               (sb-thread:with-mutex (**sandboxes-lock**)
                 (push (cons process sb-thread:*current-thread*) **sandboxes**))
               (when started (funcall started process))
               (when **sandboxes-closed**
                 (end-sandbox process))
               process)))
      ;; bwrap has its own descriptors of them once started
      (when seccomp (tuatara.runtime:ignoring-errors-reported ("closing the sandbox's system call filter")
                      (sb-posix:close seccomp)))
      (dolist (file data) (tuatara.runtime:ignoring-errors-reported ("closing ~a" (car file))
                            (sb-posix:close (cdr file)))))))

(defun sandbox-start (program arguments stdin &key memory directories (processes *sandbox-process-limit*) descriptors)
  "Start PROGRAM (a full path to one it is shown) with ARGUMENTS shut in as
SANDBOX-RUN shuts in a program, left running, with no limit on the processor
time it takes, no /tmp, no file it may write into (writing one ends it) and
its outputs let go; STDIN, a stream on a descriptor, its standard input. It may take MEMORY octets (NIL: no limit),
held to them together with all it starts where systemd-run can, have
PROCESSES processes and threads and DESCRIPTORS files open (NIL: as it is);
DIRECTORIES are directories it is shown, read only, as SANDBOX-RUN has them.
The process, or NIL when there is no sandbox here (SANDBOX-STATUS). Ending
it ends all it started."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (declare (ignore detail))
    (when (eq status :ready)
      (start-sandbox bwrap program arguments stdin nil memory
                     :total memory :outputs nil :directories directories :processes processes
                     :descriptors descriptors :scratch nil :directory "/" :file-limit 0))))

(defun sandbox-memory-scope-p ()
  "True when what is started shut in is held, with all it starts, to its
memory in a scope of its own (systemd-run); without one, each process is
held only to its own address space."
  (and (eq :ready (sandbox-status)) **sandbox-scope** t))

(defun %sandbox-run (bwrap program arguments input seconds memory output-limit cancel ended-well kept &rest keys)
  "Run PROGRAM with ARGUMENTS in the sandbox bwrap at the path BWRAP makes,
KEYS those of SANDBOX-ARGUMENTS but SECCOMP, FILES each (PATH . TEXT); it
ends well with an exit code among ENDED-WELL; what it prints is KEPT, or only
counted. See SANDBOX-RUN."
  (let ((stdin (and input (sb-sys:make-fd-stream input :input t :element-type '(unsigned-byte 8)
                                                        :buffering :none :auto-close nil)))
        (process nil)
        (piece (make-array 65536 :element-type '(unsigned-byte 8)))
        ;; what it printed and said
        (output (make-octet-pieces)) (said (make-octet-pieces)) (printed 0)
        (deadline (tuatara.runtime:deadline-in seconds))
        (outcome nil))
    (flet ((left () (tuatara.runtime:seconds-left deadline))
           (take (into octets n limit)
             (let ((n (min n (- limit (octet-pieces-size into)))))
               (when (plusp n)
                 (add-octet-piece into (subseq octets 0 n))))))
      (unwind-protect
           (let* ((started (apply #'start-sandbox bwrap program arguments stdin seconds memory
                                  :started (lambda (started) (setf process started)) keys))
                  (out (sb-sys:fd-stream-fd (sb-ext:process-output started)))
                  (err (sb-sys:fd-stream-fd (sb-ext:process-error started)))
                  (open (list out err)))
             ;; what it prints, until both its outputs end
             (loop while (and open (null outcome))
                   do (cond ((and cancel (car cancel)) (setf outcome :cancelled))
                            ((<= (left) 0) (setf outcome :timeout))
                            (t (let ((ready (wait-for-fds open (min 0.1 (left)))))
                                 (when (listp ready)
                                   (dolist (fd ready)
                                     (let ((n (read-fd-into fd piece :path "a program's output")))
                                       (cond ((zerop n) (setf open (remove fd open)))
                                             ((and (= fd out) (null output-limit)))   ; read and let go
                                             ((= fd out)
                                              (incf printed n)
                                              (cond ((> printed output-limit) (setf outcome :too-much))
                                                    (kept (take output piece n output-limit))))
                                             (t (take said piece n *sandbox-said-limit*))))))))))
             ;; its end: it may have shut its outputs and gone on
             (loop while (and (null outcome) (sb-ext:process-alive-p started))
                   do (cond ((and cancel (car cancel)) (setf outcome :cancelled))
                            ((<= (left) 0) (setf outcome :timeout))
                            (t (sleep 0.005))))
             (unless outcome
               (setf outcome (if (and (eq (sb-ext:process-status started) :exited)
                                      (member (sb-ext:process-exit-code started) ended-well))
                                 :done
                                 :failed))))
        (when process (end-sandbox process)))
      (values (joined-octets output) outcome (said-text (joined-octets said))))))

(defun try-sandbox ()
  "Find bwrap and try it, as a list (STATUS DETAIL PATH): see SANDBOX-STATUS.
The sandboxes left behind by an editor ended before it could end them are
ended first (END-SANDBOXES-LEFT), and the scopes left by one stopped in the
background (END-SCOPES-LEFT)."
  (let ((path (and *sandbox-program* (find-program *sandbox-program*))))
    (cond ((null *sandbox-program*) (list :off "turned off (*sandbox-program* is nil)" nil))
          ((null path) (list :missing (format nil "~a not found on the PATH" *sandbox-program*) nil))
          (t (let ((left (end-sandboxes-left)))
               (when (plusp left)
                 (tuatara.runtime:debug-report :job "sandbox: ~d left behind by an editor gone, ended" left)))
             (let ((version (said-text (sb-ext:string-to-octets (version-text path)))))
               (multiple-value-bind (output outcome said)
                   (handler-case (tuatara.runtime:reporting-errors ("trying the sandbox ~a" path)
                                   (%sandbox-run path "true" '() nil 10 (* 64 1024 1024) 0 nil '(0) t))
                     (error (e) (values nil :failed (princ-to-string e))))
                 (declare (ignore output))
                 (setf **sandbox-scope** (try-scope))
                 (when **sandbox-scope**
                   (tuatara.runtime:make-guarded-thread
                    (lambda ()
                      (let ((left (end-scopes-left)))
                        (when (plusp left)
                          (tuatara.runtime:debug-report :job "sandbox: ~d scopes of editors gone, stopped" left))))
                    :name "sandbox scopes left"))
                 (if (eq outcome :done)
                     (list :ready (format nil "~a (~a)~@[, together held to ~a of memory (systemd-run)~]"
                                          (if (plusp (length version)) version *sandbox-program*) path
                                          (and **sandbox-scope** *sandbox-total-memory*
                                               (format nil "~,1f GB" (/ *sandbox-total-memory* (expt 1024 3)))))
                           path)
                     (list :broken (format nil "~a (~a) cannot run: ~a" (if (plusp (length version)) version *sandbox-program*)
                                           path (if (plusp (length said)) said (string-downcase outcome)))
                           path))))))))

(defun sandbox-find-program (name)
  "The full path of the program NAME where a program in the sandbox finds it
(the system's own directories of programs), or NIL when it is not there: a
program elsewhere, as in the home directory, cannot be run shut in."
  (and (not (find #\/ name)) (find-program name *sandbox-path*)))

(defun system-program (name)
  "The program NAME found: by its full path from the root, as it is; by
name, only in the system's own folders of programs (SANDBOX-FIND-PROGRAM),
never in a folder of yours your PATH may name. NIL when there is none, as
for a path not from the root (./gpg)."
  (if (uiop:string-prefix-p "/" name)
      (find-program name)
      (sandbox-find-program name)))

(defun sandbox-status (&key again)
  "Whether programs decoding files can be shut in here, as three values: a
keyword, :READY, :MISSING (bwrap is not on the PATH), :BROKEN (it is there but
cannot run, as where the system allows no user namespaces) or :OFF
(*SANDBOX-PROGRAM* is NIL); a sentence saying which program, its version and
where, or what is wrong; and bwrap's path. Tried the first time it is needed
and kept; AGAIN tries it again. Takes a few milliseconds the first time: call
it from a thread doing work in the background."
  (let ((state (sb-thread:with-mutex (**sandbox-lock**)
                 (when (or again (null **sandbox-state**)
                           (not (equal *sandbox-program* (fourth **sandbox-state**))))
                   (setf **sandbox-state** (append (try-sandbox) (list *sandbox-program*))))
                 **sandbox-state**)))
    (values (first state) (second state) (third state))))

(defun sealed-copy (fd)
  "A copy in memory of the file open as file descriptor (FD), as a new
descriptor at its start, sealed so that nothing, not even a program given
it, can change it: not written, not made shorter or longer. FD's own offset
is left alone. A file larger than *SANDBOX-INPUT-LIMIT* is refused with a
FILE-SYSTEM-ERROR."
  (let ((size (stamp-size (fd-stamp fd "a file"))))
    (when (and *sandbox-input-limit* (> size *sandbox-input-limit*))
      (error 'file-system-error :operation "decode" :path "a file"
                                :reason (format nil "it is ~:d octets, more than *sandbox-input-limit* (~:d)" size *sandbox-input-limit*)))
    (sealed-memory-file "tuatara-decode" (lambda (copy) (copy-fd-contents fd copy size)))))

(defun sealed-p (fd)
  "True when file descriptor (FD) is a copy in memory sealed as SEALED-COPY
seals it."
  (let ((seals (fcntl-int fd 1034 0)))                      ; F_GET_SEALS
    (and (>= seals 0) (= 15 (logand seals 15)))))

(defun reopen-sealed (fd)
  "A descriptor of its own on the sealed copy open as file descriptor (FD), at
its start, read only: programs reading it at once each read from their own
place."
  (handler-case (sb-posix:open (format nil "/proc/self/fd/~d" fd) sb-posix:o-rdonly)
    (sb-posix:syscall-error (e) (fail "open again" "a copy in memory" (sb-posix:syscall-errno e)))))

(defun sandbox-status-now ()
  "SANDBOX-STATUS's first value without waiting, for the editor's own thread:
what was found when the sandbox was last tried; :OFF or :MISSING, told at
once; or :UNKNOWN while bwrap is there but not tried yet (or tried again), the
trying then started in the background."
  (let ((state **sandbox-state**))
    (cond ((and state (equal *sandbox-program* (fourth state))) (first state))
          ((null *sandbox-program*) :off)
          ((null (find-program *sandbox-program*)) :missing)
          (t (unless (sb-thread:mutex-owner **sandbox-lock**)
               (tuatara.runtime:make-guarded-thread
                (lambda () (tuatara.runtime:ignoring-errors-reported ("trying the sandbox") (sandbox-status)))
                :name "sandbox trial"))
             :unknown))))

(defun cloexec-pipe ()
  "A new pipe, both its ends closed in programs started (pipe2 with
O_CLOEXEC): its reading and its writing descriptor."
  (sb-alien:with-alien ((fds (array sb-alien:int 2)))
    (unless (zerop (sb-alien:alien-funcall (sb-alien:extern-alien "pipe2" (function sb-alien:int (* (array sb-alien:int 2))
                                                                                      sb-alien:int))
                                           (sb-alien:addr fds) #x80000))         ; O_CLOEXEC
      (fail "make" "a pipe"))
    (values (sb-alien:deref fds 0) (sb-alien:deref fds 1))))

(defun secret-pipe (secret)
  "A pipe holding SECRET, a string, and a new line after it (nothing at all
for an empty one), as two values: the descriptor to read it from and the one
it was written through, which must be kept open while a program reads, then
closed. Read past SECRET, it answers at once that there is nothing more
\(EAGAIN), so that a program asking again fails, never waits nor takes an
empty line for an answer. Both ends are closed in programs started but the
one given the reading end. A secret longer than the pipe holds is refused
with a FILE-SYSTEM-ERROR, never left to block."
  (multiple-value-bind (in out) (cloexec-pipe)
    (let* ((line (if (string= secret "") (make-string 0) (concatenate 'string secret (string #\Newline))))
           (octets (tuatara.text:string-octets line))
           (made nil))
      (unwind-protect (progn (let ((room (fcntl-int out 1032 0)))         ; F_GETPIPE_SZ
                               (when (and (plusp room) (> (length octets) room))
                                 (error 'file-system-error :operation "pass" :path "a secret"
                                                           :reason (format nil "it is longer than ~:d octets" room))))
                             (write-octets out octets :path "a pipe")
                             (sb-posix:fcntl in sb-posix:f-setfl (logior (sb-posix:fcntl in sb-posix:f-getfl)
                                                                         sb-posix:o-nonblock))
                             (setf made t))
        (fill line #\Nul)
        (fill octets 0)
        (unless made (sb-posix:close in) (sb-posix:close out))))
    (values in out)))

(defun sandbox-run (program arguments &key input secret (seconds *sandbox-seconds*) (memory *sandbox-memory*)
                                           (output-limit (* 256 1024 1024)) (output-kept t) cancel files
                                           environment directories
                                           writable overlays (directory "/tmp") (file-limit *sandbox-file-limit*)
                                           (ended-well '(0)))
  "Run PROGRAM (a name looked for on the sandbox's PATH, /usr/bin:/bin, or a
full path to one it is shown) with ARGUMENTS (strings, never through a shell)
shut in: it sees the system's programs and libraries, read only, and nothing
else (no home directory, no network, no other process), its time, memory and
the files it writes in its own /tmp limited; reading INPUT, an open descriptor, on its
standard input from its start (nothing when NIL), a sealed copy of it
\(SEALED-COPY), never INPUT itself; an INPUT that is such a copy already is
read as it is, through a descriptor of its own (one copy can serve many runs,
at once too). With SECRET, a string, in place of INPUT, it reads that
on its standard input instead, through a pipe, followed by a new line (none
for an empty one), and nothing after it: asking again, it is answered at once
that there is nothing more (SECRET-PIPE). SECRET is never in its arguments,
which other programs can see, nor in the debug log.
Waits for it: call it from a thread doing work in the background. It is ended
after SECONDS, when it prints more than OUTPUT-LIMIT octets (NIL: what it
prints is let go, never kept; with OUTPUT-KEPT NIL, counted against it but
not kept), or when CANCEL, a cons, has a true car; it may
take MEMORY octets (NIL: no limit), and write files of FILE-LIMIT octets at
most (NIL: no limit). Returns three values: the octets it printed; how it
went, :DONE (it ended well, with an exit code among ENDED-WELL, 0 alone
unless given), :FAILED (it ended with an error, or ran out of memory),
:TIMEOUT, :TOO-MUCH, :CANCELLED, or :UNAVAILABLE when there is no sandbox
here (SANDBOX-STATUS); and what it said on its error output, or why there is
no sandbox.
FILES are files it is shown, read only, each (PATH . TEXT); ENVIRONMENT what
it has in its environment besides PATH, HOME (/tmp) and LC_ALL, or in place
of them, each (NAME VALUE). DIRECTORIES are directories of yours it is shown,
read only, each (SOURCE . TARGET), TARGET where it sees SOURCE (under /tmp,
its home, for LibreOffice's fonts), or a path alone, seen where it is (its
real place, links in it followed, a link at the root as /home kept as it is
here); WRITABLE the same, for directories it may write in; OVERLAYS
directories it sees where they are and may seem to write in, each (SOURCE
UPPER WORK): what it writes goes to UPPER, a directory of the editor's own,
SOURCE never changed, WORK an empty directory beside UPPER that the system
needs for it (where bwrap cannot do that, before 0.11, SOURCE is shown read
only). Those that do not exist are left out; a directory in another one shown
is seen in it. DIRECTORY is where it starts, /tmp unless given."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (if (eq status :ready)
        (multiple-value-bind (copy writer)
            (cond (secret (secret-pipe secret))
                  (input (if (sealed-p input) (reopen-sealed input) (sealed-copy input))))
          (unwind-protect
               (let ((start (get-internal-real-time)))
               (multiple-value-bind (output outcome said)
                   (%sandbox-run bwrap program arguments copy seconds memory output-limit cancel ended-well output-kept
                                 :files files :environment environment :directories directories :writable writable
                                 :overlays overlays :directory directory :file-limit file-limit)
                 (tuatara.runtime:debug-report :job "sandbox: ~a ~{~a~^ ~}: ~(~a~) in ~,3f s, ~:d octets~@[: ~a~]"
                                               program arguments outcome
                                               (tuatara.runtime:seconds-since start)
                                               (length output) (and (plusp (length said)) said))
                 (values output outcome said)))
            (when copy (sb-posix:close copy))
            (when writer (sb-posix:close writer))))
        (values (make-array 0 :element-type '(unsigned-byte 8)) :unavailable detail))))

(defun sandbox-run-on-file (path program arguments &rest keys)
  "Run PROGRAM with ARGUMENTS shut in, as SANDBOX-RUN does (KEYS are its own
but INPUT), reading the file at PATH on its standard input. The file is opened
here, its access time left alone where the system allows; what is not a
regular file, a named pipe or a device, is refused without being opened, with
a FILE-SYSTEM-ERROR."
  (let ((fd (open-for-reading path t :accept '(:regular) :operation "decode")))
    (unwind-protect (apply #'sandbox-run program arguments :input fd keys)
      (sb-posix:close fd))))
