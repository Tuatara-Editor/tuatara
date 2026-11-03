;;;; frontends/tui/client.lisp --- tuatara --daemon, --client and --stop: the processes either side of a daemon.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; tuatara --daemon starts the program again in a session of its own, told
;;;; by its environment to be the daemon (RUN-DAEMON), and returns once that
;;;; one listens. tuatara --client connects to it and paints in this
;;;; terminal what it sends, sending it the keys typed (RUN-CLIENT): it reads
;;;; no init file and loads no package, as the daemon does all that.

(in-package #:tuatara.terminal)

(defparameter *daemon-ready-seconds* 60
  "The seconds tuatara --daemon waits for the daemon it started to say it
listens before taking it as failed.")

(defun daemon-log-name (name)
  "The name of the daemon NAME's log in the cache folder."
  (format nil "daemon-~a.log" name))

(defun daemon-log-path (name)
  "Where the daemon NAME's log is, as you would type it."
  (tuatara.command:abbreviated-path
   (tuatara.io:path-join (tuatara.io:native (tuatara.io:cache-directory)) (daemon-log-name name))))

(defparameter *daemon-variables*
  '("HOME" "USER" "LOGNAME" "SHELL" "PATH" "LANG" "LANGUAGE" "LC_*" "TZ" "TMPDIR" "XDG_*" "DISPLAY"
    "WAYLAND_DISPLAY" "DBUS_SESSION_BUS_ADDRESS" "SSH_AUTH_SOCK" "GNUPGHOME" "NO_COLOR" "TUATARA_HEAP_MB")
  "The environment variables a daemon is started with, of those of the
program starting it, a * at the end standing for any ending: who you are,
where your files, folders and programs are, your language and time zone, and
the desktop and the agents it reaches; the AppImage it may run from sets its
own.
None of what one command line is run with (git's variables, a terminal's)
outlives it in the daemon.")

(defun daemon-environment (environment)
  "Those of ENVIRONMENT, NAME=VALUE strings, a daemon is started with
\(*DAEMON-VARIABLES*)."
  (remove-if-not (lambda (setting)
                   (let ((equals (position #\= setting)))
                     (and equals
                          (let ((name (subseq setting 0 equals)))
                            (some (lambda (kept)
                                    (if (char= #\* (char kept (1- (length kept))))
                                        (uiop:string-prefix-p (subseq kept 0 (1- (length kept))) name)
                                        (string= kept name)))
                                  *daemon-variables*)))))
                 environment))

(defun start-daemon (name config &key quiet)
  "Start the daemon NAME, reading CONFIG as its init file when given, in a
session of its own, and wait until it listens or says why it cannot: said on
the standard output, unless QUIET, or why not on the error output. The exit
code: 0 when it runs, 1 when not. It is given only the variables a daemon
keeps (DAEMON-ENVIRONMENT)."
  (unless tuatara.io:*saved-program*
    (format *error-output* "tuatara: a daemon is started from the built program, not from the sources~%")
    (return-from start-daemon 1))
  (multiple-value-bind (program arguments)
      (tuatara.io:setsid-command tuatara.io:*saved-program* (and config (list "-c" (tuatara.io:native config))))
    (let* ((process (sb-ext:run-program program arguments
                                        :wait nil :input nil :output :stream :error :output
                                        :environment (cons (format nil "TUATARA_DAEMON=~a" name)
                                                           (daemon-environment (sb-ext:posix-environ)))))
           (out (sb-ext:process-output process))
           (deadline (tuatara.runtime:deadline-in *daemon-ready-seconds*)))
      (unwind-protect
           (loop
             (when (tuatara.runtime:deadline-passed-p deadline)
               (format *error-output* "tuatara: the daemon ~a did not say it was ready within ~d seconds; its log is ~a~%"
                       name *daemon-ready-seconds* (daemon-log-path name))
               (return 1))
             (when (plusp (tuatara.io:poll-fd (sb-sys:fd-stream-fd out) 1 1000))
               (let ((line (read-line out nil)))
                 (cond ((null line)
                        (format *error-output* "tuatara: the daemon ~a ended before it was ready; its log is ~a~%"
                                name (daemon-log-path name))
                        (return 1))
                       ((string= line "ready")
                        (unless quiet
                          (format t "The Tuatara daemon ~a runs: tuatara --client~:[=~a~;~*~] shows it~%"
                                  name (string= name tuatara.defaults::*daemon-default-name*) name))
                        (return 0))
                       (t (format *error-output* "~a~%" (shown line)))))))
        (close out)
        (sb-ext:process-close process)))))

(defun daemon-output (name)
  "A descriptor open for writing where what the daemon NAME prints goes once
it has said it is ready: its log (TUATARA.IO:SESSION-LOG), or /dev/null when
the log cannot be made."
  (or (tuatara.io:session-log (format nil "the daemon ~a" name) :name (daemon-log-name name))
      (sb-posix:open "/dev/null" sb-posix:o-wronly)))

(defun run-daemon (name arguments)
  "Be the daemon NAME, as tuatara --daemon started this program, ARGUMENTS
its command line (-c naming its init file): listen, say ready on the
standard output, from then on a log of its own (DAEMON-OUTPUT), and run the editor until it
stops, SIGTERM ending it with no one asked (END-DAEMON-ON-TERMINATE). The
exit code: 0 once it has stopped, 1 when it could not start, why said on the
standard output."
  (sb-posix:unsetenv "TUATARA_DAEMON")
  ;; its terminal, if it had one, may close: it goes on
  (sb-sys:enable-interrupt sb-unix:sighup :ignore)
  (let* ((options (read-options arguments))
         (config (getf options :config))
         (daemon (handler-case (tuatara.defaults::open-daemon name)
                   (tuatara.runtime:refusal (condition)
                     (format t "tuatara: ~a~%" condition)
                     (finish-output)
                     (return-from run-daemon 1)))))
    (let ((log (daemon-output name)))
      (sb-posix:dup2 log 2)
      (format t "ready~%")
      (finish-output)
      (sb-posix:dup2 log 1)
      (sb-posix:close log))
    ;; it holds no folder busy
    (tuatara.runtime:ignoring-errors-reported ("going to the home folder")
      (sb-posix:chdir (tuatara.io:native (user-homedir-pathname))))
    (tuatara.defaults::set-collection-policy)
    (when config
      (setf tuatara.defaults::*config-file* (pathname (tuatara.io:expand-path config))))
    ;; logout, or kill: its changes kept as auto-saved copies, then it ends
    (tuatara.defaults::end-daemon-on-terminate daemon)
    (tuatara.defaults::run-daemon-editor daemon)
    0))

(defun client-arguments-p (arguments)
  "True when ARGUMENTS, the command line, start a client of a daemon
(--client or --stop)."
  (some (lambda (argument)
          (or (member argument '("--client" "--stop") :test #'string=)
              (member (named-option argument) '("--client" "--stop") :test #'equal)))
        (loop for argument in arguments until (string= argument "--") collect argument)))

(defun client-environment ()
  "This program's environment variables a daemon keeps for its client
\(TUATARA.COMMAND:CLIENT-VARIABLE-P), as (NAME . VALUE)."
  (loop for setting in (sb-ext:posix-environ)
        for equals = (position #\= setting)
        when (and equals (tuatara.command:client-variable-p (subseq setting 0 equals)))
          collect (cons (subseq setting 0 equals) (subseq setting (1+ equals)))))

(defun client-request (options terminal)
  "What a client started with OPTIONS (READ-OPTIONS) asks of its daemon, a
TUATARA.FRONTEND:REQUEST: its files and the paths to compare or merge named
from where it was started, the folder it was started in, its environment,
TERMINAL, the device of its terminal or NIL, and whether it stops the daemon."
  (tuatara.frontend:make-request
   :directory (tuatara.io:native (uiop:getcwd))
   :terminal terminal
   :environment (client-environment)
   :files (loop for file in (getf options :files)
                unless (equal file '(:standard-input))
                  collect (if (stringp file)
                              (list (named-here file) nil nil)
                              (list (named-here (first file)) (second file) (third file))))
   :safe (getf options :safe) :hex (getf options :hex)
   :standard-input (and (find '(:standard-input) (getf options :files) :test #'equal) t)
   :diff (mapcar #'named-here (getf options :diff))
   :merge (mapcar #'named-here (getf options :merge))
   :frames (or (getf options :frames) 1)
   :wait t
   :stop (and (getf options :stop) t)))

(defun connect-to-daemon (name start &key quiet)
  "A connection to the daemon NAME, as a descriptor; with START, the daemon
started first when none runs (START-DAEMON). NIL, why said on the error
output unless QUIET, when there is none."
  (let ((path (handler-case (tuatara.defaults::daemon-path name "socket")
                (tuatara.runtime:refusal (condition)
                  (format *error-output* "tuatara: ~a~%" condition)
                  (return-from connect-to-daemon nil)))))
    (or (tuatara.io:connect-to path)
        (and start (zerop (start-daemon name nil :quiet t)) (tuatara.io:connect-to path))
        (progn (unless quiet
                 (format *error-output* "tuatara: no daemon named ~a runs: tuatara --daemon~:[=~a~;~*~] starts one~
                                       ~@[, or tuatara --client --start~]~%"
                         name (string= name tuatara.defaults::*daemon-default-name*) name (not start)))
               nil))))

(defun refusal-words (condition name)
  "What a client of the daemon NAME says when the connection was refused, as
CONDITION, a TUATARA.FRONTEND:REMOTE-REFUSED, says why."
  (let ((other (tuatara.frontend:remote-refused-other condition)))
    (if other
        (format nil "the daemon ~a runs ~a; this is ~a: stop it (M-x daemon-quit, or ~
                     tuatara --stop~:[=~a~;~*~]) and start it again"
                name other (tuatara:version-line) (string= name tuatara.defaults::*daemon-default-name*) name)
        (tuatara.frontend:remote-refused-words condition))))

(defun run-client (options &key (terminal (keyboard-available-p)) (desktop (window-wanted-p)))
  "Be a client of the daemon OPTIONS name (READ-OPTIONS: --client or --stop):
in this terminal, or with --gui in a window of its own, as when started with
no terminal on a desktop (DESKTOP), its keys sent to the daemon and what it
paints shown, until it lets the client go; --stop with no terminal shows
nothing and asks nothing. Standard input, for -, is passed to the daemon to
read, the keys read from the terminal or the window; TERMINAL is true when
there is a terminal. The exit code: the one the daemon gave, 1 when no
daemon runs or it ended, as is said, or 2 when there is neither a terminal
nor a window to show it in; :FALLBACK, nothing said, when no daemon runs and
--fallback asks for an editor of its own instead."
  (let* ((name (or (getf options :client) (getf options :stop)))
         (stop (getf options :stop))
         (window (and (not stop) (or (getf options :gui) desktop))))
    (when (and (not terminal) (not stop) (not window))
      (format *error-output* "tuatara: a client needs a terminal to show the daemon in: run it in one, or with --gui~%")
      (return-from run-client 2))
    (let ((fd (connect-to-daemon name (getf options :start) :quiet (getf options :fallback)))
          (input (and (find '(:standard-input) (getf options :files) :test #'equal)
                      (sb-posix:fcntl 0 +f-dupfd-cloexec+ 3)))
          (code nil) (words nil) (refused nil))
      (unless fd
        (when input (sb-posix:close input))
        (return-from run-client (if (getf options :fallback) :fallback 1)))
      (unwind-protect
           (handler-case
               (let ((greeted (tuatara.frontend:greet-editor fd)))
                 (if window
                     (multiple-value-setq (code words)
                       (tuatara.frontend.gtk:run-window-client fd greeted (client-request options nil)
                                                               :standard-input (shiftf input nil)
                                                               :connect (lambda () (connect-to-daemon name nil))))
                     (multiple-value-setq (code words)
                       (run-terminal-client fd greeted options terminal (shiftf input nil)))))
             (tuatara.frontend:remote-refused (condition)
               (setf refused (refusal-words condition name))
               (tuatara.frontend:shut-and-close fd)))
        (when input (sb-posix:close input)))
      (cond (refused (format *error-output* "tuatara: ~a~%" (shown refused)) 1)
            ((null code)
             (format *error-output* "tuatara: the daemon ~a ended; its log is ~a~%" name (daemon-log-path name))
             1)
            (t (when words (format *error-output* "tuatara: ~a~%" (shown words)))
               code)))))

(defun run-terminal-client (fd greeted options terminal input)
  "Show the daemon at the other end of FD, which has said hello (GREETED, as
TUATARA.FRONTEND:GREET-EDITOR read it), in this terminal, or with no TERMINAL
nowhere, as a client started with OPTIONS (READ-OPTIONS), passing INPUT,
standard input's descriptor or NIL, to be read; until the daemon lets it go.
Two values: the exit code the daemon gave, or NIL when it went without one,
and what it said with it."
  (when terminal (read-keys-from-terminal))
  (let ((frontend (if terminal
                      (tuatara.frontend.tui:make-tui-frontend)
                      (tuatara.frontend:make-null-frontend))))
    (tuatara.frontend:frontend-start frontend)
    (unwind-protect
         (tuatara.frontend:run-remote-client
          (tuatara.frontend:make-remote-client
           fd frontend :request (client-request options (tuatara.frontend:frontend-terminal-device frontend))
                       :standard-input input :greeted greeted))
      (tuatara.frontend:frontend-stop frontend))))
