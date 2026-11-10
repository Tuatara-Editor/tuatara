;;;; frontends/tui/client.lisp --- tuatara --daemon, --client and --stop: the processes either side of a daemon.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; tuatara --daemon starts the program again in a session of its own, told
;;;; by its environment to be the daemon (RUN-DAEMON), and returns once that
;;;; one has read its init file and takes clients. tuatara --client connects
;;;; to it and paints in this terminal what it sends, sending it the keys typed
;;;; (RUN-CLIENT): it reads no init file and loads no package, as the daemon
;;;; does all that.

(in-package #:tuatara.terminal)

(defun daemon-log-name (name)
  "The name of the daemon NAME's log in the cache folder."
  (format nil "daemon-~a.log" name))

(defun daemon-log-path (name)
  "Where the daemon NAME's log is, as you would type it."
  (tuatara.command:abbreviated-path
   (tuatara.io:path-join (tuatara.io:native (tuatara.io:cache-directory)) (daemon-log-name name))))

(defparameter *daemon-variables*
  '("SHELL" "PATH" "XDG_*" "DISPLAY" "WAYLAND_DISPLAY" "DBUS_SESSION_BUS_ADDRESS" "SSH_AUTH_SOCK" "GNUPGHOME"
    "NO_COLOR" "TUATARA_HEAP_MB")
  "The environment variables a daemon is started with, of those of the
program starting it, besides TUATARA.IO:*RESTRICTED-VARIABLES*, a * at the
end standing for any ending: your shell, where your files, folders and
programs are, and the desktop and the agents it reaches; the AppImage it
may run from sets its own.
None of what one command line is run with (git's variables, a terminal's)
outlives it in the daemon.")

(defun start-daemon (name config &key quiet)
  "Start the daemon NAME, reading CONFIG as its init file when given, in a
session of its own, and wait until it is ready or says why it cannot: said on
the standard output, unless QUIET, or why not on the error output. The exit
code: 0 when it runs, 1 when not. It is given only the variables a daemon
keeps (*DAEMON-VARIABLES*)."
  (unless tuatara.io:*saved-program*
    (format *error-output* "tuatara: a daemon is started from the built program, not from the sources~%")
    (return-from start-daemon 1))
  (let* ((process (tuatara.io:start-program (list* tuatara.io:*saved-program*
                                                     (and config (list "-c" (tuatara.io:native config))))
                                              :session t :output :stream :error :output :external-format :default
                                              :environment (tuatara.io:restricted-environment
                                                            *daemon-variables* (format nil "TUATARA_DAEMON=~a" name))))
         (out (sb-ext:process-output process))
         (deadline (tuatara.runtime:deadline-in tuatara.frontend:*starting-seconds*)))
    (unwind-protect
         (loop
           (when (tuatara.runtime:deadline-passed-p deadline)
             (format *error-output* "tuatara: the daemon ~a did not say it was ready within ~d seconds; its log is ~a~%"
                     name tuatara.frontend:*starting-seconds* (daemon-log-path name))
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
      (sb-ext:process-close process))))

(defun daemon-output (name)
  "A descriptor open for writing where what the daemon NAME prints goes once
it has said it is ready: its log (TUATARA.IO:SESSION-LOG), or /dev/null when
the log cannot be made."
  (or (tuatara.io:session-log (format nil "the daemon ~a" name) :name (daemon-log-name name))
      (sb-posix:open "/dev/null" sb-posix:o-wronly)))

(defun run-daemon (name arguments)
  "Be the daemon NAME, as tuatara --daemon started this program, ARGUMENTS
its command line (-c naming its init file): listen, read the init file, say
ready on the standard output once clients are taken, from then on a log of its
own (DAEMON-OUTPUT), and run the editor until it stops, SIGTERM ending it with
no one asked (END-DAEMON-ON-TERMINATE). The exit code: 0 once it has stopped,
1 when it could not start, why said on the standard output."
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
      ;; it holds no folder busy
      (tuatara.runtime:ignoring-errors-reported ("going to the home folder")
        (sb-posix:chdir (tuatara.io:native (user-homedir-pathname))))
      (tuatara.defaults::set-collection-policy)
      (when config
        (setf tuatara.defaults::*config-file* (pathname (tuatara.io:expand-path config))))
      ;; logout, or kill: its changes kept as auto-saved copies, then it ends
      (tuatara.defaults::end-daemon-on-terminate daemon)
      (tuatara.defaults::run-daemon-editor
       daemon :ready (lambda ()
                       (unwind-protect
                            (tuatara.runtime:ignoring-errors-reported ("saying the daemon is ready")
                              (format t "ready~%")
                              (finish-output))
                         (sb-posix:dup2 log 1)
                         (sb-posix:close log)))))
    0))

(defun report-daemon-socket (name start)
  "Say, on the standard output, where the daemon NAME listens and whether it
runs, for a client on another machine. It also says what gpg and sshd here do
and where sshd brought the client's agent for this connection
\(WRITE-DAEMON-REPORT), with START having started the daemon first when it did
not run. A client of a daemon on this machine asked ssh to run tuatara
--daemon-socket here. The exit code is 0 once said, 1 when the daemons' folder
is not safe or a daemon could not be started, why said on the error output."
  (handler-case
      (let* ((socket (tuatara.defaults::daemon-path name "socket"))
             (lock (tuatara.defaults::daemon-path name "lock"))
             (runs (lambda () (tuatara.io:lock-file-held-p lock))))
        (when (and start (not (funcall runs)))
          (unless (zerop (start-daemon name nil :quiet t))
            (return-from report-daemon-socket 1))
          (let ((words (lingering-words (or (tuatara.io:own-variable "USER") (tuatara.io:own-variable "LOGNAME")))))
            (when words (format *error-output* "tuatara: the daemon ~a was started, but ~a~%" name words))))
        (write-daemon-report *standard-output* :home (tuatara.io:native (user-homedir-pathname)) :socket socket
                                               :runs (funcall runs) :gpg-socket (gpgconf-directory "agent-socket")
                                               :gpg-autostart (gpg-autostart-p) :unlink (sshd-unlinks)
                                               :agent (let ((agent (tuatara.io:own-variable "SSH_AUTH_SOCK")))
                                                        (and (forwardable-path-p agent) agent)))
        (finish-output)
        0)
    (tuatara.runtime:refusal (condition)
      (format *error-output* "tuatara: ~a~%" condition)
      1)))

(defun client-arguments-p (arguments)
  "True when ARGUMENTS, the command line, start a client of a daemon
\(--client or --stop), ask where one listens (--daemon-socket) or take a file
dropped on a client (--receive-file)."
  (some (lambda (argument)
          (or (member argument '("--client" "--stop" "--daemon-socket" "--receive-file") :test #'string=)
              (uiop:string-prefix-p "--receive-file=" argument)
              (member (named-option argument) '("--client" "--stop" "--daemon-socket") :test #'equal)))
        (loop for argument in arguments until (string= argument "--") collect argument)))

(defun client-environment (&optional far)
  "This program's environment variables a daemon keeps for its client
\(TUATARA.COMMAND:CLIENT-VARIABLE-P), FAR for a daemon on another machine,
as (NAME . VALUE)."
  (loop for setting in (sb-ext:posix-environ)
        for equals = (position #\= setting)
        when (and equals (tuatara.command:client-variable-p (subseq setting 0 equals) far))
          collect (cons (subseq setting 0 equals) (subseq setting (1+ equals)))))

(defun client-request (options terminal &key link)
  "What a client started with OPTIONS (READ-OPTIONS) asks of its daemon, a
TUATARA.FRONTEND:REQUEST: its files and the paths to compare or merge named
from where it was started, the folder it was started in, its environment,
TERMINAL, the device of its terminal or NIL, and whether it stops the daemon.
A client of a daemon on another machine, reached through LINK (an SSH-LINK),
is far: its folder is the home folder there, its files named from it, it
names no terminal, and it passes on where its agent is brought there and
what its connection said, to be shown in its frame."
  (let ((directory (if link (ssh-link-home link) (tuatara.io:native (uiop:getcwd)))))
    (flet ((named (name)
             ;; ~ there is the home folder there
             (let ((name (if (and link (uiop:string-prefix-p "~/" name)) (subseq name 2) name)))
               (tuatara.io:native (tuatara.io:expand-path name :directory directory :tilde (not link))))))
      (tuatara.frontend:make-request
       :directory directory
       :terminal (and (not link) terminal)
       :environment (client-environment (and link t))
       :files (loop for file in (getf options :files)
                    unless (equal file '(:standard-input))
                      collect (if (stringp file)
                                  (list (named file) nil nil)
                                  (list (named (first file)) (second file) (third file))))
       :safe (getf options :safe) :hex (getf options :hex)
       :standard-input (and (find '(:standard-input) (getf options :files) :test #'equal) t)
       :diff (mapcar #'named (getf options :diff))
       :merge (mapcar #'named (getf options :merge))
       :frames (or (getf options :frames) 1)
       :wait t
       :stop (and (getf options :stop) t)
       :far (and link t)
       :agent (and link (ssh-link-agent-there link))
       :notes (and link (reverse (ssh-link-notes link)))))))

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

(defun refusal-words (condition name &optional host)
  "What a client of the daemon NAME, on the machine HOST when given, says
when the connection was refused, as CONDITION, a
TUATARA.FRONTEND:REMOTE-REFUSED, says why."
  (let ((other (tuatara.frontend:remote-refused-other condition)))
    (if other
        (format nil "the daemon ~a~@[ on ~a~] runs ~a; this is ~a: stop it (M-x daemon-quit, or ~
                     tuatara --stop~:[=~a~;~*~]) and start it again"
                name host other (tuatara:version-line)
                (and (not host) (string= name tuatara.defaults::*daemon-default-name*))
                (if host (format nil "~a:~a" host name) name))
        (tuatara.frontend:remote-refused-words condition))))

(defun run-client (options &key (terminal (keyboard-available-p)) (desktop (window-wanted-p)))
  "Be a client of the daemon OPTIONS name (READ-OPTIONS: --client or --stop),
on this machine or on the one :HOST names, reached through ssh
\(OPEN-SSH-LINK, closed as the client ends). The client runs in this terminal,
or with --gui in a window of its own, as when started with no terminal on a
desktop (DESKTOP). Its keys are sent to the daemon and what it paints is
shown, until the daemon lets the client go. --stop with no terminal shows
nothing and asks nothing.
A client on another machine whose terminal closes (SIGHUP) closes its ssh
connection before it ends.
Standard input, for -, is passed to a daemon here to read, the keys read
from the terminal or the window; TERMINAL is true when there is a terminal.
The exit code is the one the daemon gave, 1 when no daemon runs, it cannot be
reached or it ended, as is said, or 2 when there is neither a terminal nor a
window to show it in. :FALLBACK, nothing said, is returned when no daemon
runs and --fallback asks for an editor of its own instead. A client in a
window that fails shows what it said in a window of its own too
\(SHOWN-IN-A-WINDOW-TOO)."
  (let* ((name (or (getf options :client) (getf options :stop)))
         (host (getf options :host))
         (stop (getf options :stop))
         (window (and (not stop) (or (getf options :gui) desktop))))
    (when (and (not terminal) (not stop) (not window))
      (format *error-output* "tuatara: a client needs a terminal to show the daemon in: run it in one, or with --gui~%")
      (return-from run-client 2))
    (when window
      (return-from run-client (shown-in-a-window-too (lambda () (connect-client options name host terminal t)))))
    (connect-client options name host terminal window)))

(defun shown-in-a-window-too (function)
  "Call FUNCTION, a client in a window, and return its exit code; when it
fails, what it said on the error output, said there too, is shown in a
window of its own as well (SHOW-WORDS), there being no terminal to read it
in."
  (let* ((said (make-string-output-stream))
         (code (let ((*error-output* (make-broadcast-stream *error-output* said)))
                 (funcall function)))
         (words (get-output-stream-string said)))
    (when (and (not (eql code 0)) (plusp (length words)))
      (tuatara.runtime:ignoring-errors-reported ("showing why the client could not show its daemon")
        (tuatara.frontend.gtk:show-in-window
         (lambda (f) (show-words f (uiop:split-string (string-right-trim '(#\Newline) words)
                                                      :separator '(#\Newline)))))))
    code))

(defun window-lines (lines width)
  "LINES as rows of a window WIDTH columns wide: each line filled at its
spaces into rows no wider than WIDTH (a word wider than that cut), an empty
line kept as an empty row."
  (loop for line in lines
        for words = (uiop:split-string line :separator " ")
        append (or (tuatara.display:fill-words (remove "" words :test #'string=) (max 10 width) :cut t)
                   (list ""))))

(defun show-words (frontend lines)
  "Show LINES, what a client said as it could not show its daemon, on
FRONTEND, a window it starts and stops, until a key is typed there or it
is closed."
  (tuatara.frontend:frontend-start frontend)
  (unwind-protect
       (loop
         (multiple-value-bind (width height) (tuatara.frontend:frontend-size frontend)
           (let ((all (window-lines (append (list "Tuatara could not show the daemon:" "") lines
                                            (list "" "Any key closes this window."))
                                    width)))
           (tuatara.frontend:frontend-begin-frame frontend)
           (loop for row below height
                 do (tuatara.frontend:frontend-put-row frontend row (or (nth row all) "")))
           (tuatara.frontend:frontend-end-frame frontend)))
         (when (member (tuatara.frontend:event-kind (tuatara.frontend:frontend-read-event frontend))
                       '(:key :close :quit))
           (return)))
    (tuatara.frontend:frontend-stop frontend)))

(defun connect-client (options name host terminal window)
  "Be the client RUN-CLIENT is, as OPTIONS say, of the daemon NAME, on HOST
when given, through ssh, in a WINDOW or else the TERMINAL: its exit code."
  (let ((link (and host (or (open-ssh-link host name :start (getf options :start))
                            (return-from connect-client 1)))))
    ;; its terminal closed: the client ends as it would, its ssh connection closed, and exits 1
    (when link
      (sb-sys:enable-interrupt sb-unix:sighup (lambda (signal info context)
                                                (declare (ignore signal info context))
                                                (sb-ext:exit :code 1))))
    (unwind-protect (run-client-of options name link terminal window)
      (when link
        (sb-sys:enable-interrupt sb-unix:sighup :default)
        (close-ssh-link link)))))

(defun run-client-of (options name link terminal window)
  "Be the client RUN-CLIENT is, as OPTIONS say, of the daemon NAME, here or
through LINK, an SSH-LINK, in a WINDOW or else in the TERMINAL, when there
is one; its exit code, or :FALLBACK, as RUN-CLIENT gives them."
  (flet ((connect (start)
           (if link
               (or (tuatara.io:connect-to (ssh-link-socket link))
                   (progn (format *error-output* "tuatara: the daemon ~a on ~a cannot be reached through ssh~%"
                                  name (ssh-link-host link))
                          nil))
               (connect-to-daemon name start :quiet (getf options :fallback)))))
    (let ((fd (connect (getf options :start)))
          (input (and (find '(:standard-input) (getf options :files) :test #'equal)
                      (sb-posix:fcntl 0 +f-dupfd-cloexec+ 3)))
          (code nil) (words nil) (refused nil))
      (unless fd
        (when input (sb-posix:close input))
        (return-from run-client-of (if (getf options :fallback) :fallback 1)))
      (unwind-protect
           (handler-case
               (let ((greeted (tuatara.frontend:greet-editor
                               fd :starting (lambda ()
                                              (format *error-output* "tuatara: the daemon ~a is still starting; ~
                                                                      waiting up to ~d seconds for it~%"
                                                      name tuatara.frontend:*starting-seconds*)))))
                 (if window
                     (multiple-value-setq (code words)
                       (tuatara.frontend.gtk:run-window-client fd greeted (client-request options nil :link link)
                                                               :standard-input (shiftf input nil)
                                                               :connect (lambda () (connect nil))
                                                               :allow (and link (link-allowances link))))
                     (multiple-value-setq (code words)
                       (run-terminal-client fd greeted options terminal (shiftf input nil) :link link))))
             (tuatara.frontend:remote-refused (condition)
               (setf refused (refusal-words condition name (and link (ssh-link-host link))))
               (tuatara.frontend:shut-and-close fd)))
        (when input (sb-posix:close input)))
      (cond (refused (format *error-output* "tuatara: ~a~%" (shown refused)) 1)
            ((null code)
             (format *error-output* "tuatara: the daemon ~a~@[ on ~a~] ended; its log is ~:[~;there, ~]~a~%"
                     name (and link (ssh-link-host link)) link (daemon-log-path name))
             1)
            (t (when words (format *error-output* "tuatara: ~a~%" (shown words)))
               code)))))

(defun run-terminal-client (fd greeted options terminal input &key link)
  "Show the daemon at the other end of FD, which has said hello (GREETED, as
TUATARA.FRONTEND:GREET-EDITOR read it), in this terminal, or with no TERMINAL
nowhere, as a client started with OPTIONS (READ-OPTIONS), passing INPUT,
standard input's descriptor or NIL, to be read; through LINK, an SSH-LINK,
when the daemon is on another machine; until the daemon lets it go.
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
           fd frontend :request (client-request options (tuatara.frontend:frontend-terminal-device frontend)
                                                :link link)
                       :standard-input input :greeted greeted :allow (and link (link-allowances link))))
      (tuatara.frontend:frontend-stop frontend))))
