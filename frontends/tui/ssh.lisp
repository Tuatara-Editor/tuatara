;;;; frontends/tui/ssh.lisp --- tuatara --client=HOST: a daemon on another machine, its socket brought here by ssh.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A client of a daemon on another machine runs the system's ssh, never one
;;;; found in a folder of yours: one connection, its control socket in a
;;;; private folder of the runtime folder here, asks the Tuatara there where
;;;; its daemon listens and which build it is (tuatara --daemon-socket), then
;;;; has that socket forwarded to one beside the control socket. The client
;;;; connects there as to a daemon here, saying it is far (REQUEST-FAR), and
;;;; closes the connection as it ends. Nothing is given to a shell here, the
;;;; host always comes after --, and the options of the command line win over
;;;; the person's ssh configuration: no agent, no X11, no forwards of its own,
;;;; host keys known already. What the other machine says is never written
;;;; raw on this terminal, and nothing typed here reaches it before the
;;;; client shows the daemon.

(in-package #:tuatara.terminal)

(defparameter *ssh-answer-seconds* 120
  "How long the other machine has, from the start of ssh's connection, to
say where its daemon listens, a password typed for ssh included; then the
connection is given up.")

;;; What the Tuatara on the daemon's machine says

(defparameter *report-keys*
  '("tuatara-daemon" "version" "home" "socket" "runs" "gpg-socket" "gpg-autostart" "unlink" "agent")
  "The lines tuatara --daemon-socket writes, each a key and its value, in
this order.")

(defun write-daemon-report (stream &key home socket runs gpg-socket gpg-autostart (unlink "unknown") agent)
  "Write on STREAM, for a client on another machine, what tuatara
--daemon-socket says of a daemon (*REPORT-KEYS*): the messages' format, this
build's version line, HOME, the home folder, SOCKET, where the daemon listens,
and whether it RUNS; GPG-SOCKET, where gpg here looks for its agent, or
NIL; whether gpg would start an agent of its own (GPG-AUTOSTART); and
UNLINK, whether sshd here removes a socket in the way of one forwarded
(yes, no or unknown); AGENT, the socket of the agent sshd brought here for
this connection (SSH_AUTH_SOCK), or NIL."
  (format stream "tuatara-daemon ~d~%version ~a~%home ~a~%socket ~a~%runs ~:[no~;yes~]~%gpg-socket ~:[-~;~:*~a~]~%~
                  gpg-autostart ~:[no~;yes~]~%unlink ~a~%agent ~:[-~;~:*~a~]~%"
          tuatara.frontend::+protocol+ (tuatara:version-line) home socket runs gpg-socket gpg-autostart unlink
          agent))

(defun read-daemon-report (text)
  "What TEXT, the output of tuatara --daemon-socket, says, as a plist:
:PROTOCOL, :VERSION, :HOME (ending in /), :SOCKET, :RUNS, :GPG-SOCKET (NIL
for none, or one ssh cannot forward), :GPG-AUTOSTART, :UNLINK (:YES, :NO
or :UNKNOWN) and :AGENT (as :GPG-SOCKET); NIL when it is not that, each line of *REPORT-KEYS* once and
in order, the home folder and the sockets from the root, the sockets ones
ssh can forward (FORWARDABLE-PATH-P), the home folder a plain path
\(PLAIN-PATH-P); none is named to ssh here as the other machine wrote it
otherwise. What comes before its first line, as a shell's start files may
print there, is passed over."
  (let* ((lines (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=))
         (start (position-if (lambda (line) (uiop:string-prefix-p "tuatara-daemon " line)) lines :from-end t))
         (lines (and start (nthcdr start lines))))
    (when (= (length lines) (length *report-keys*))
      (let ((fields (loop for line in lines
                          for key in *report-keys*
                          for space = (position #\Space line)
                          unless (and space (string= key line :end2 space)) do (return-from read-daemon-report nil)
                          collect (subseq line (1+ space)))))
        (destructuring-bind (protocol version home socket runs gpg-socket gpg-autostart unlink agent) fields
          (let ((protocol (tuatara.text:whole-number protocol)))
            (and protocol (tuatara.defaults::plain-path-p home) (forwardable-path-p socket)
                 (member runs '("yes" "no") :test #'string=)
                 (member gpg-autostart '("yes" "no") :test #'string=)
                 (member unlink '("yes" "no" "unknown") :test #'string=)
                 (list :protocol protocol :version version :home (tuatara.io:slash-ended home) :socket socket
                       :runs (string= runs "yes") :gpg-socket (and (forwardable-path-p gpg-socket) gpg-socket)
                       :gpg-autostart (string= gpg-autostart "yes")
                       :unlink (intern (string-upcase unlink) :keyword)
                       :agent (and (forwardable-path-p agent) agent)))))))))

;;; What gpg and sshd do on the daemon's machine

(defparameter *sshd-config-files* '("/etc/ssh/sshd_config.d/" "/etc/ssh/sshd_config")
  "Where sshd's settings are read from, in the order sshd reads them as the
systems ship it: a folder's files ending in .conf, then the main file.")

(defun gpgconf-directory (name)
  "The folder or socket gpgconf --list-dirs NAME names here (agent-socket,
agent-extra-socket, homedir), as it writes it; NIL when there is no
gpgconf (TUATARA.DEFAULTS::GPGCONF-PATH) or it names none."
  (let ((program (tuatara.defaults::gpgconf-path)))
    (when program
      (multiple-value-bind (code said)
          (tuatara.defaults::run-bounded program (list "--list-dirs" name) :seconds 10
                                         :environment (tuatara.io:program-environment))
        (let ((path (string-right-trim '(#\Newline #\Return) said)))
          (and (eql code 0) (plusp (length path)) (not (find #\Newline path)) path))))))

(defun gpg-autostart-p ()
  "True when gpg here would start an agent of its own when it finds none: no
line no-autostart in gpg.conf in GnuPG's folder."
  (let* ((home (gpgconf-directory "homedir"))
         (text (and home (tuatara.runtime:ignoring-errors-reported ("reading gpg.conf")
                           (uiop:read-file-string (tuatara.io:path-join home "gpg.conf"))))))
    (notany (lambda (line) (string= (string-trim '(#\Space #\Tab #\Return) line) "no-autostart"))
            (and text (uiop:split-string text :separator '(#\Newline))))))

(defun sshd-unlinks ()
  "Whether sshd here removes a socket in the way of one forwarded to this
machine (StreamLocalBindUnlink), as its settings say outside any Match
block: \"yes\", \"no\", or \"unknown\" when a file of them cannot be read,
as for one readable by root alone."
  (let ((files (loop for place in *sshd-config-files*
                     append (if (uiop:string-suffix-p place "/")
                                (loop for (entry) in (tuatara.runtime:ignoring-errors-reported ("listing ~a" place)
                                                       (tuatara.io:directory-entries place))
                                      when (uiop:string-suffix-p entry ".conf")
                                        collect (tuatara.io:path-join place entry))
                                (list place))))
        (unread nil))
    (dolist (file files (if unread "unknown" "no"))
      (when (tuatara.io:path-kind file)
        (let ((text (handler-case (uiop:read-file-string file) (error () nil))))
          (if (null text)
              (setf unread t)
              (dolist (line (uiop:split-string text :separator '(#\Newline)))
                (let ((words (cl-ppcre:split "[\\s=]+" (string-trim '(#\Space #\Tab #\Return) line))))
                  (cond ((string-equal (first words) "Match") (return))
                        ((string-equal (first words) "StreamLocalBindUnlink")
                         (return-from sshd-unlinks (if (string-equal (second words) "yes") "yes" "no"))))))))))))

(defun lingering-words (user)
  "Words saying this machine takes away USER's runtime folder, and so a
daemon's socket, once USER's last session ends (systemd-logind with no
lingering), or NIL when it keeps it or there is no telling."
  (and user (eq (tuatara.io:path-kind "/var/lib/systemd/linger/") :directory)
       (not (tuatara.io:path-kind (tuatara.io:path-join "/var/lib/systemd/linger/" user)))
       (format nil "this machine takes your runtime folder away, the daemon's socket in it, once you have ~
                    logged out everywhere: loginctl enable-linger ~a keeps it" user)))

;;; The connection to the daemon's machine

(defstruct (ssh-link (:constructor make-ssh-link (host program folder lock permissions)) (:copier nil))
  "A connection to HOST made with the ssh PROGRAM, its control socket and the
daemon's socket forwarded here in FOLDER, a private folder held by the lock
LOCK (a descriptor) while the client runs; PERMISSIONS, what HOST is trusted
with (TUATARA.DEFAULTS::SERVER-PERMISSIONS), NIL for nothing; MASTER, the
ssh holding the connection, a process of the client's own that ends with
it; AGENT, the ssh-agent of its own forwarded there, a process, while it
runs; AGENT-THERE, where that agent is brought on HOST, or NIL; NOTES, what
the connection said that the person should read, the latest first; GPG-FORWARD,
\(THERE HERE), the socket of gpg-agent there forwarded from the one here,
while it is; HOME, the home folder there, once the Tuatara there said it."
  (host "" :type string :read-only t)
  (program "" :type string :read-only t)
  (folder "" :type string :read-only t)
  (lock nil :read-only t)
  (permissions nil :read-only t)
  (master nil)
  (agent nil)
  (agent-there nil)
  (notes '())
  (gpg-forward nil)
  (home nil))

(setf (documentation 'make-ssh-link 'function) "An SSH-LINK to HOST with PROGRAM, kept in FOLDER held by LOCK."
      (documentation 'ssh-link-p 'function) "True when OBJECT is an SSH-LINK."
      (documentation 'ssh-link-host 'function) "The machine LINK reaches, as ssh takes it."
      (documentation 'ssh-link-program 'function) "The ssh program LINK runs, by its full path."
      (documentation 'ssh-link-folder 'function) "The private folder of LINK's sockets, ending in /."
      (documentation 'ssh-link-lock 'function) "The descriptor of the lock that holds LINK's folder."
      (documentation 'ssh-link-permissions 'function) "What LINK's machine is trusted with, a plist, or NIL."
      (documentation 'ssh-link-master 'function) "The ssh holding LINK's connection, a process, or NIL."
      (documentation 'ssh-link-agent 'function) "The ssh-agent LINK forwards, a process, or NIL."
      (documentation 'ssh-link-agent-there 'function) "Where LINK's agent is brought on its machine, or NIL."
      (documentation 'ssh-link-notes 'function) "What LINK's connection said for the person to read, the latest first."
      (documentation 'ssh-link-gpg-forward 'function) "The sockets of gpg-agent LINK forwards, (THERE HERE), or NIL."
      (documentation 'ssh-link-home 'function) "The home folder on LINK's machine, ending in /, or NIL.")

(defun ssh-link-socket (link)
  "Where the daemon's socket LINK forwards is here."
  (tuatara.io:path-join (ssh-link-folder link) "s"))

(defun ssh-link-agent-socket (link)
  "Where the socket of LINK's own ssh-agent is here."
  (tuatara.io:path-join (ssh-link-folder link) "a"))

(defun ssh-arguments (link &key master session agent (operation nil operation-p) forward remote command)
  "The arguments of ssh for LINK: the control socket's, with MASTER the
connection's own, held until it is told to exit or the client ends, with no
command (*SSH-OPTIONS*, the agent forwarded LINK's own when it has one); with
SESSION, COMMAND, a list of words, run over the connection, reading nothing
when SESSION is :QUIET, no ssh configuration read and no question asked
should the connection be gone, AGENT asking for the agent there for it; or
OPERATION (\"forward\", \"cancel\", \"exit\") asked of the connection,
with FORWARD, (HERE THERE), a socket there brought here, or REMOTE, (THERE
HERE), one here brought there, and no ssh configuration read, so that none
of its forwards for the host joins ours. The host comes last but for
COMMAND, after --."
  (append (list "-T" "-o" (format nil "ControlPath=~a" (cl-ppcre:regex-replace-all
                                                         "%" (tuatara.io:path-join (ssh-link-folder link) "c") "%%")))
          (when master
            (list* "-N" "-o" "ControlMaster=yes" "-o" "ControlPersist=no"
                   (tuatara.defaults::ssh-option-arguments
                    (and (ssh-link-agent link) (format nil "ForwardAgent=~a" (ssh-link-agent-socket link))))))
          (when session
            (append (and (eq session :quiet) (list "-n"))
                    (list* "-F" "none" "-o" "BatchMode=yes"
                           (tuatara.defaults::ssh-option-arguments (and agent "ForwardAgent=yes")))))
          (when operation-p (list "-F" "none" "-O" operation))
          (when forward (list "-L" (format nil "~a:~a" (first forward) (second forward))))
          (when remote (list "-R" (format nil "~a:~a" (first remote) (second remote))))
          (list "--" (ssh-link-host link))
          command))

(defun run-ssh (link arguments &key ask (seconds *ssh-answer-seconds*))
  "Run LINK's ssh with ARGUMENTS as TUATARA.DEFAULTS::RUN-BOUNDED does,
SECONDS at most, with ASK able to ask for a password on the terminal; its
three values."
  (tuatara.defaults::run-bounded (ssh-link-program link) arguments :ask ask :seconds seconds))

(defun link-note (link control &rest arguments)
  "Keep, for the person to read in the client's frame, and say on the error
output, what CONTROL and ARGUMENTS say of LINK's connection."
  (let ((words (apply #'format nil control arguments)))
    (push words (ssh-link-notes link))
    (format *error-output* "tuatara: ~a~%" (shown words))))

(defun say-what-ssh-said (host said)
  "Say on the error output SAID, what ssh and the machine HOST wrote on its
error output, each line with its control characters written out; a line
ended by a carriage return and a newline, as ssh ends its own, ends there."
  (dolist (line (uiop:split-string (string-right-trim '(#\Newline) said) :separator '(#\Newline)))
    (setf line (string-right-trim '(#\Return) line))
    (unless (zerop (length line))
      (format *error-output* "tuatara: ~a: ~a~%" host (shown line)))))

(defun link-refusal (host code said name)
  "What a client says when the Tuatara on HOST, asked through ssh where the
daemon NAME listens, ended with the exit code CODE (NIL: it did not answer
in time) having said SAID, in words; NIL when it said where
\(READ-DAEMON-REPORT) and is this build."
  (let ((report (read-daemon-report said)))
    (cond ((null code)
           (format nil "~a did not say where its daemon listens within ~d seconds: the connection is given up"
                   host *ssh-answer-seconds*))
          ((eql code 255) (format nil "ssh lost its connection to ~a (it says why below)" host))
          ((eql code 127) (format nil "~a has no tuatara on the PATH ssh's commands are run with there" host))
          ((null report)
           (format nil "the Tuatara on ~a is too old to be shown from another machine~:[~;, or could not say ~
                        where its daemon listens (it says why below)~]: update it there" host (eql code 1)))
          ((or (/= (getf report :protocol) tuatara.frontend::+protocol+)
               (string/= (getf report :version) (tuatara:version-line)))
           (format nil "the daemon ~a on ~a runs ~a; this is ~a: update one of them" name host
                   (getf report :version) (tuatara:version-line)))
          ((not (getf report :runs))
           (format nil "no daemon named ~a runs on ~a: tuatara --client=~a:~a --start starts one"
                   name host host name)))))

;;; The ssh agent forwarded to a trusted server

(defparameter *ssh-agent-program* "ssh-agent"
  "The ssh-agent run to hold the keys forwarded to a trusted server, as
TUATARA.IO:SYSTEM-PROGRAM finds it.")

(defparameter *ssh-add-program* "ssh-add"
  "The ssh-add run to put your keys in the agent forwarded to a trusted
server, as TUATARA.IO:SYSTEM-PROGRAM finds it.")

(defun confirm-refusal (host)
  "Why an ssh-agent started here could not ask you to confirm each use of a
key forwarded to HOST, in words, or NIL when it can: it asks with
TUATARA.IO:ASKPASS-PROGRAM, shown on the desktop (TUATARA.IO:DESKTOP-P),
unless SSH_ASKPASS_REQUIRE is force."
  (cond ((not (tuatara.io:askpass-program))
         (format nil "the ssh agent is not forwarded to ~a: no ssh-askpass program here asks you to confirm each ~
                      use of a key (install the one of OpenSSH, or name one in SSH_ASKPASS)" host))
        ((not (or (tuatara.io:desktop-p) (equal (tuatara.io:program-variable "SSH_ASKPASS_REQUIRE") "force")))
         (format nil "the ssh agent is not forwarded to ~a: there is no display for ssh-askpass to ask you to ~
                      confirm each use of a key on" host))))

(defun ssh-settings (program host)
  "What ssh, PROGRAM, makes of its configuration for HOST (ssh -G), as a
list of (KEY . VALUE), the keys in lower case, in the order it says them."
  (multiple-value-bind (code said) (tuatara.defaults::run-bounded program (list "-G" "--" host) :seconds 10)
    (and (eql code 0)
         (loop for line in (uiop:split-string said :separator '(#\Newline))
               for space = (position #\Space line)
               when space collect (cons (string-downcase (subseq line 0 space)) (subseq line (1+ space)))))))

(defun known-host-name (program host)
  "The name ssh, PROGRAM, knows the machine HOST ([user@]machine, or an
alias of its configuration) by in its known hosts, for ssh-add -h: its
HostKeyAlias, else its HostName, the account written before it kept."
  (let* ((at (position #\@ host))
         (settings (ssh-settings program (subseq host (if at (1+ at) 0))))
         (name (or (cdr (assoc "hostkeyalias" settings :test #'string=))
                   (cdr (assoc "hostname" settings :test #'string=))
                   (subseq host (if at (1+ at) 0)))))
    (if at (concatenate 'string (subseq host 0 (1+ at)) name) name)))

(defun written-out (path home tokens)
  "PATH as ssh writes a file's name in its configuration, with ~/ first made
HOME, and each %X of TOKENS, an alist of X to what it stands for, written
out; NIL when it names a token TOKENS lacks."
  (let ((path (if (uiop:string-prefix-p "~/" path) (concatenate 'string home (subseq path 1)) path)))
    (with-output-to-string (out)
      (loop with i = 0
            while (< i (length path))
            do (let ((c (char path i)))
                 (if (and (char= c #\%) (< (1+ i) (length path)))
                     (let ((token (assoc (char path (1+ i)) tokens)))
                       (unless token (return-from written-out nil))
                       (write-string (cdr token) out)
                       (incf i 2))
                     (progn (write-char c out) (incf i))))))))

(defun identity-files (link)
  "The key files ssh names for LINK's machine (IdentityFile in ssh -G), ~,
%d, %u, %h, %r and %% written out (WRITTEN-OUT), those that are there."
  (let* ((settings (ssh-settings (ssh-link-program link) (ssh-link-host link)))
         (home (string-right-trim "/" (tuatara.io:native (user-homedir-pathname))))
         (tokens `((#\% . "%") (#\d . ,home) (#\u . ,(or (uiop:getenv "USER") ""))
                   (#\h . ,(or (cdr (assoc "hostname" settings :test #'string=)) ""))
                   (#\r . ,(or (cdr (assoc "user" settings :test #'string=)) "")))))
    (loop for (key . value) in settings
          for path = (and (string= key "identityfile") (written-out value home tokens))
          when (and path (uiop:string-prefix-p "/" path) (eq (tuatara.io:path-kind path) :regular))
            collect path)))

(defun agent-limits (link destinations)
  "The arguments of ssh-add that limit the keys of LINK's agent to
DESTINATIONS, used from LINK's machine (-h HOST, -h HOST>DESTINATION), each
machine named as its known hosts have it (KNOWN-HOST-NAME); none for no
DESTINATIONS. As a second value, why they cannot be: OpenSSH here older than
8.9, or saying no version, which then refuses the agent rather than forward
it unlimited."
  (let* ((program (ssh-link-program link))
         (host (ssh-link-host link))
         (version (and destinations (tuatara.defaults::openssh-version program))))
    (cond ((null destinations) (values '() nil))
          ((and version (tuatara.io:version-at-least-p version '(8 9)))
           (let* ((there (known-host-name program host))
                  (machine (subseq there (1+ (or (position #\@ there) -1)))))
             (values (list* "-h" there (loop for destination in destinations
                                             append (list "-h" (format nil "~a>~a" machine
                                                                       (known-host-name program destination)))))
                     nil)))
          (t (values '() (format nil "the ssh agent is not forwarded to ~a: OpenSSH ~:[here does not say its ~
                                      version~;~:*~{~d~^.~} here~] and cannot limit the machines its keys are used ~
                                      for to those named (8.9 or later can); name none to forward it unlimited"
                                 host version))))))

(defun start-link-agent (link)
  "Start an ssh-agent of LINK's own, its socket in LINK's folder, and have
ssh-add put your keys in it, asking for their passphrases as it does: the keys ssh -G names for LINK's machine
\(IDENTITY-FILES), each use to be confirmed here (-c) and limited to
the machines LINK's machine is trusted to use them for, where OpenSSH can
\(AGENT-LIMITS). This agent is the one forwarded: the one you keep your
keys in is neither shown to the machine nor changed. It ends with the
client, killed or not (setpriv): without setpriv it is not started. NIL
once done; else why not, in words, and what the program that failed said,
as two values."
  (let* ((host (ssh-link-host link))
         (agent (tuatara.io:system-program *ssh-agent-program*))
         (add (tuatara.io:system-program *ssh-add-program*))
         (socket (ssh-link-agent-socket link))
         (command (and agent (tuatara.io:setpriv-command agent (list "-D" "-a" socket))))
         (keys (and agent add (identity-files link))))
    (cond ((not (and agent add))
           (format nil "the ssh agent is not forwarded to ~a: ssh-agent and ssh-add are not installed in the ~
                        system's folders of programs" host))
          ((not command)
           (format nil "the ssh agent is not forwarded to ~a: setpriv (util-linux) is not installed, and without ~
                        it the agent would outlive a client that is killed" host))
          ((confirm-refusal host))
          ((null keys)
           (format nil "the ssh agent is not forwarded to ~a: none of the keys ssh -G ~a names (IdentityFile) is ~
                        there" host host))
          ((nth-value 1 (agent-limits link (getf (ssh-link-permissions link) :destinations))))
          ((progn (setf (ssh-link-agent link)
                        (sb-ext:run-program (first command) (rest command)
                                            :wait nil :input nil :output nil :error nil
                                            :environment (tuatara.defaults::ssh-environment)))
                  (tuatara.runtime:wait-until (lambda () (or (eq (tuatara.io:path-kind socket :follow nil) :socket)
                                                             (not (sb-ext:process-alive-p (ssh-link-agent link)))))
                                              10)
                  (not (eq (tuatara.io:path-kind socket :follow nil) :socket)))
           (format nil "the ssh agent for ~a did not start" host))
          (t (let ((limits (agent-limits link (getf (ssh-link-permissions link) :destinations))))
               (multiple-value-bind (code said complained)
                   (tuatara.defaults::run-bounded add (append (list "-c") limits (list "--") keys)
                                                  :ask t :seconds *ssh-answer-seconds*
                                                  :environment (tuatara.defaults::ssh-environment
                                                                (format nil "SSH_AUTH_SOCK=~a" socket)))
                 (declare (ignore said))
                 (and (not (eql code 0))
                      (values (format nil "ssh-add could not put your keys in the agent forwarded to ~a" host)
                              complained))))))))

(defun stop-link-agent (link)
  "End LINK's own ssh-agent, when it has one, and the keys it held with it."
  (let ((agent (shiftf (ssh-link-agent link) nil)))
    (when agent
      (tuatara.runtime:ignoring-errors-reported ("ending the ssh agent for ~a" (ssh-link-host link))
        (when (sb-ext:process-alive-p agent)
          (sb-ext:process-kill agent sb-unix:sigterm)
          (tuatara.runtime:wait-until (lambda () (not (sb-ext:process-alive-p agent))) 5)))
      (tuatara.io:done-with-program agent))))

;;; gpg-agent forwarded to a trusted server

(defun desk-extra-socket ()
  "The extra socket of gpg-agent here, the one that refuses to export or
change keys, gpg-agent started first when it is not running; NIL when
there is none ssh can forward (FORWARDABLE-PATH-P)."
  (let ((program (tuatara.defaults::gpgconf-path)))
    (when program
      (tuatara.defaults::run-bounded program (list "--launch" "gpg-agent") :seconds 30
                                     :environment (tuatara.io:program-environment))
      (let ((path (gpgconf-directory "agent-extra-socket")))
        (and (forwardable-path-p path) path)))))

(defun forward-gpg-agent (link report)
  "Forward gpg-agent's extra socket here to where gpg looks for its agent on
LINK's machine, as REPORT (READ-DAEMON-REPORT) says, so that gpg there signs
and decrypts with the keys here, the passphrase asked here: only when gpg
there starts no agent of its own (no-autostart), which would take the
socket's place. When sshd there may not remove a socket left in the way
\(StreamLocalBindUnlink), what to add is said. NIL once done; else why not,
in words, and what ssh said, as two values."
  (let ((host (ssh-link-host link))
        (there (getf report :gpg-socket)))
    (cond ((null there)
           (format nil "gpg-agent is not forwarded to ~a: gpgconf there names no agent socket of plain letters, ~
                        digits, /, ., _ and - (is GnuPG installed there?)" host))
          ((getf report :gpg-autostart)
           (format nil "gpg-agent is not forwarded to ~a: gpg there would start an agent of its own in place of ~
                        the one forwarded; add the line no-autostart to gpg.conf in GnuPG's folder there ~
                        (gpgconf --list-dirs homedir)" host))
          (t (let ((here (desk-extra-socket)))
               (if (null here)
                   (format nil "gpg-agent is not forwarded to ~a: gpgconf here names no extra socket of gpg-agent ~
                                ssh can forward" host)
                   (multiple-value-bind (code said complained)
                       (run-ssh link (ssh-arguments link :operation "forward" :remote (list there here)))
                     (declare (ignore said))
                     (cond ((eql code 0)
                            (setf (ssh-link-gpg-forward link) (list there here))
                            (unless (eq (getf report :unlink) :yes)
                              (link-note link "sshd on ~a may not remove a socket of gpg-agent left there from ~
                                               before, which stops the next forward: add StreamLocalBindUnlink yes ~
                                               to /etc/ssh/sshd_config there" host))
                            nil)
                           (t (values (format nil "ssh could not forward gpg-agent to ~a~:[: a socket left there ~
                                                   from before may be in the way, which sshd removes with ~
                                                   StreamLocalBindUnlink yes in /etc/ssh/sshd_config~;~]"
                                              host (eq (getf report :unlink) :yes))
                                      complained))))))))))

;;; The clipboard and files dropped, for a trusted server

(defparameter *most-dropped-octets* (* 100 1024 1024)
  "The largest file dropped on a window that is sent to the machine of its
daemon, and that the Tuatara there takes.")

(defun dropped-name (path)
  "The name a file dropped from PATH is given on the daemon's machine: its
own name, each character other than an ASCII letter, a digit, ., _ or `-'
made _, and one starting with . or `-' given a _ first."
  (let* ((path (string-right-trim "/" path))
         (name (map 'string (lambda (c) (if (and (< (char-code c) 128) (or (alphanumericp c) (find c "._-"))) c #\_))
                    (subseq path (1+ (or (position #\/ path :from-end t) -1))))))
    (cond ((zerop (length name)) "dropped")
          ((find (char name 0) ".-") (concatenate 'string "_" name))
          (t name))))

(defun send-dropped-files (link paths)
  "Send the files at PATHS, dropped on a window, into the folder of LINK's
machine it is trusted with dropped files in, each through the Tuatara there
\(tuatara --receive-file), told its size first, which writes it for you
alone and runs nothing of it: their paths there; NIL, why said on the error output, when one is
no file, larger than *MOST-DROPPED-OCTETS* or could not be sent, and none
is named to the daemon."
  (let ((folder (getf (ssh-link-permissions link) :files))
        (host (ssh-link-host link)))
    (flet ((fail (control &rest arguments)
             (format *error-output* "tuatara: ~a~%" (shown (apply #'format nil control arguments)))
             (return-from send-dropped-files nil)))
      (unless folder (fail "~a is not trusted with files dropped" host))
      (loop for path in paths
            collect (let ((fd (handler-case (tuatara.io:open-for-reading path t :accept '(:regular) :wanted "a file")
                                (tuatara.io:file-system-error (condition) (fail "~a" condition)))))
                      (with-open-stream (input (sb-sys:make-fd-stream fd :input t :element-type '(unsigned-byte 8)
                                                                         :auto-close t))
                        (let ((size (sb-posix:stat-size (sb-posix:fstat fd))))
                          (when (> size *most-dropped-octets*)
                            (fail "~a is not sent to ~a: larger than ~:d octets" path host *most-dropped-octets*))
                          (multiple-value-bind (code said complained)
                              (tuatara.defaults::run-bounded
                               (ssh-link-program link)
                               (ssh-arguments link :session t
                                                   :command (list "tuatara" (format nil "--receive-file=~d:~a/~a" size
                                                                                    (string-right-trim "/" folder)
                                                                                    (dropped-name path))))
                               :input input :seconds (+ 60 (floor size 100000)))
                            (let ((there (and (eql code 0) (uiop:string-prefix-p "dropped " said)
                                              (string-right-trim '(#\Newline) (subseq said 8)))))
                              (unless (and there (tuatara.defaults::plain-path-p there))
                                (say-what-ssh-said host complained)
                                (fail "~a could not be sent to ~a" path host))
                              there)))))))))

(defun link-allowances (link)
  "What the editor at the other end of LINK is given all the same, as
TUATARA.FRONTEND:MAKE-REMOTE-CLIENT takes it: the clipboard, and files
dropped sent there, as its machine is trusted with them."
  (let ((permissions (ssh-link-permissions link)))
    (list :clipboard (getf permissions :clipboard)
          :drop (and (getf permissions :files) (lambda (paths) (send-dropped-files link paths))))))

(defun receive-dropped-file (asked &optional (input (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8)
                                                                                :buffering :full)))
  "Write what INPUT, standard input as standard, gives, a file dropped on a
window of a client on another machine, as ASKED says, SIZE:PATH: SIZE
octets, at most *MOST-DROPPED-OCTETS*, at PATH here, a plain path
\(PLAIN-PATH-P) from your home folder unless it is from the root. Its folder
is made for you alone when it is not there, and must then be a folder of
yours, no link, that no one else may write in (FOLDER-REFUSAL); the file is made for you
alone under a name nothing has yet (NAME-2 and on when NAME is taken), and
never run. Fewer octets than SIZE, or more, and the file is deleted. The
exit code: 0, its path said on the standard output after dropped; 1, why
said on the error output."
  (flet ((fail (control &rest arguments)
           (format *error-output* "tuatara: ~a~%" (shown (apply #'format nil control arguments)))
           (return-from receive-dropped-file 1)))
    (let* ((colon (position #\: asked))
           (size (and colon (tuatara.text:whole-number (subseq asked 0 colon))))
           (path (if colon (subseq asked (1+ colon)) ""))
           (slash (position #\/ path :from-end t))
           (name (subseq path (if slash (1+ slash) 0)))
           (within (if slash (subseq path 0 slash) ""))
           (folder (if (uiop:string-prefix-p "/" path)
                       within
                       (tuatara.io:path-join (tuatara.io:native (user-homedir-pathname)) within))))
      (unless (and (tuatara.defaults::plain-path-p path :relative t) (plusp (length name)) (char/= (char name 0) #\.)
                   (plusp (length folder)))
        (fail "~a cannot name a file dropped here: letters, digits, /, ., _ and -" path))
      (unless (and size (<= size *most-dropped-octets*))
        (fail "a file dropped is taken here with its size first, of at most ~:d octets" *most-dropped-octets*))
      (handler-case
          (progn
            (tuatara.io:ensure-private-directory folder)
            (let ((why (tuatara.defaults::folder-refusal (tuatara.io:native folder) "the folder of files dropped"
                                                         (sb-posix:getuid) :private nil)))
              (when why (fail "~a" why)))
            (multiple-value-bind (fd there)
                (tuatara.io:create-free-file folder (lambda (n) (tuatara.io:numbered-name name n :dash)))
              (let ((written 0) (buffer (tuatara.text:make-octets 65536)))
                (with-open-stream (out (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
                                                                 :auto-close t))
                  (loop for got = (read-sequence buffer input :end (min (length buffer) (- (1+ size) written)))
                        while (plusp got)
                        do (incf written got)
                           (when (<= written size) (write-sequence buffer out :end got))
                        until (> written size)))
                (unless (= written size)
                  (tuatara.io:remove-file there)
                  (fail "a file dropped came with ~:[~:d octets of the ~:d said~;more than the ~*~:d octets said~]: ~
                         it is not kept" (> written size) written size))
                (format t "dropped ~a~%" there)
                (finish-output)
                0)))
        ((or tuatara.runtime:refusal tuatara.io:file-system-error) (condition)
          (fail "~a" condition))))))

;;; Opening and closing

(defun open-ssh-link (host name &key start)
  "Connect to HOST with the system's ssh and bring the socket of the daemon
NAME there to a private folder here, the daemon started first with START
when none runs there, with what HOST is trusted with
\(TUATARA.DEFAULTS::SERVER-PERMISSIONS) forwarded too: the SSH-LINK, whose
socket (SSH-LINK-SOCKET) a client connects to. NIL, why said on the error
output, when that cannot be done: no ssh or setpriv, ssh failing, no
Tuatara there or another build of it, no daemon, or what it is trusted with
not forwarded. A connection left by a client that crashed is told to end
before its folder is deleted."
  (let ((program (tuatara.io:system-program tuatara.defaults::*ssh-program*)))
    (unless program
      (format *error-output* "tuatara: ssh is not installed in the system's folders of programs: a daemon on ~
                              another machine is reached with it~%")
      (return-from open-ssh-link nil))
    (multiple-value-bind (folder lock)
        (handler-case
            (let ((base (tuatara.defaults::daemon-folder)))
              (tuatara.defaults::forget-abandoned-folders
               "ssh/" :base base
                      :before (lambda (folder)
                                (let ((control (tuatara.io:path-join folder "c")))
                                  (when (and (eq (tuatara.io:path-kind control :follow nil) :socket)
                                             (forwardable-path-p control))
                                    (tuatara.defaults::run-bounded
                                     program (list "-F" "none" "-o" (format nil "ControlPath=~a" control)
                                                   "-O" "exit" "--" "localhost")
                                     :seconds 10)))))
              (tuatara.defaults::make-owned-folder "ssh/" :base base))
          ((or tuatara.runtime:refusal tuatara.io:file-system-error) (condition)
            (format *error-output* "tuatara: ~a~%" (shown (princ-to-string condition)))
            (return-from open-ssh-link nil)))
      (let ((link (make-ssh-link host program folder lock (tuatara.defaults::server-permissions host)))
            (opened nil))
        (unwind-protect
             (multiple-value-bind (why said) (connect-ssh-link link name start)
               (if why
                   (progn (format *error-output* "tuatara: ~a~%" (shown why))
                          (when said (say-what-ssh-said host said)))
                   (setf opened link)))
          (unless opened (close-ssh-link link)))
        opened))))

(defun start-master (link)
  "Start the ssh holding LINK's connection, a process of this client's
that ends with it, killed or not (setpriv --pdeathsig), and every forward
with it; ssh asks for a password or passphrase on the terminal, as it does,
what it says kept in LINK's folder. NIL once its control socket is there;
else why not, in words, and what ssh said, as two values."
  (let* ((host (ssh-link-host link))
         (command (tuatara.io:setpriv-command (ssh-link-program link) (ssh-arguments link :master t)))
         (control (tuatara.io:path-join (ssh-link-folder link) "c"))
         (said (tuatara.io:path-join (ssh-link-folder link) "e")))
    (flet ((said () (or (tuatara.runtime:ignoring-errors-reported ("reading what ssh said")
                          (tuatara.text:lenient-string
                           (tuatara.io:file-octets said :limit tuatara.io:*program-said-limit*)))
                        "")))
      (unless command
        (return-from start-master
          (format nil "setpriv (util-linux) is not installed in the system's folders of programs: without it ~
                       ssh's connection to ~a would outlive a client that is killed" host)))
      (setf (ssh-link-master link)
            (sb-ext:run-program (first command) (rest command)
                                :wait nil :input t :output nil :error said :if-error-exists :supersede
                                :environment (tuatara.defaults::ssh-environment)))
      (let ((master (ssh-link-master link)))
        (tuatara.runtime:wait-until (lambda () (or (eq (tuatara.io:path-kind control :follow nil) :socket)
                                                   (not (sb-ext:process-alive-p master))))
                                    *ssh-answer-seconds*)
        (cond ((eq (tuatara.io:path-kind control :follow nil) :socket) nil)
              ((sb-ext:process-alive-p master)
               (values (format nil "~a did not let ssh in within ~d seconds: the connection is given up"
                               host *ssh-answer-seconds*)
                       (said)))
              (t (let ((said (said))) (values (unreached-words host said) said))))))))

(defun unreached-words (host said)
  "What a client says when ssh could not reach HOST, having said SAID: that
it says why below, and, when SAID is about the machine's host key, that a
machine whose key ssh does not know yet is refused and how to check it."
  (format nil "ssh could not reach ~a (it says why below~:[~;; a machine whose key ssh does not know yet is ~
               refused: connect once with ssh ~a and check its key~])"
          host (search "host key" said :test #'char-equal) host))

(defun connect-ssh-link (link name start)
  "Make LINK's connection for OPEN-SSH-LINK, as NAME and START say: its own
agent started first when its machine is trusted with one, the connection
held (START-MASTER), the machine asked where the daemon listens, and its
socket forwarded, gpg-agent's too when it is trusted with that. NIL once
done; else why not, in words, and what ssh or the program that failed said,
as two values."
  (let ((host (ssh-link-host link))
        (agent (getf (ssh-link-permissions link) :agent)))
    (unless (forwardable-path-p (ssh-link-socket link))
      (return-from connect-ssh-link
        (format nil "~a cannot be named to ssh: XDG_RUNTIME_DIR holds other than letters, digits, /, ., _ and -"
                (ssh-link-socket link))))
    (when agent
      (multiple-value-bind (why said) (start-link-agent link)
        (when why (return-from connect-ssh-link (values why said)))))
    (multiple-value-bind (why said) (start-master link)
      (when why (return-from connect-ssh-link (values why said))))
    (multiple-value-bind (code said complained)
        (run-ssh link (ssh-arguments link :session :quiet :agent agent
                                          :command (list* "tuatara" (format nil "--daemon-socket=~a" name)
                                                          (and start (list "--start"))))
                 :ask t)
      (let ((why (link-refusal host code said name)))
        (when why (return-from connect-ssh-link (values why complained))))
      (let ((report (read-daemon-report said)))
        (setf (ssh-link-home link) (getf report :home))
        (when agent
          (setf (ssh-link-agent-there link) (getf report :agent))
          (unless (getf report :agent)
            (link-note link "sshd on ~a did not bring your agent there (AllowAgentForwarding no?): programs there ~
                             have no key of yours" host)))
        (multiple-value-bind (code said complained)
            (run-ssh link (ssh-arguments link :operation "forward"
                                              :forward (list (ssh-link-socket link) (getf report :socket))))
          (declare (ignore said))
          (cond ((not (eql code 0))
                 (values (format nil "ssh could not bring the daemon's socket on ~a here" host) complained))
                ((getf (ssh-link-permissions link) :gpg) (forward-gpg-agent link report))))))))

(defun close-ssh-link (link)
  "Close LINK: gpg-agent's socket there no longer forwarded, its connection
ended and its ssh waited for, its own agent ended too, its folder deleted
and the lock on it let go."
  (tuatara.runtime:ignoring-errors-reported ("ending the ssh connection to ~a" (ssh-link-host link))
    (when (tuatara.io:path-kind (tuatara.io:path-join (ssh-link-folder link) "c") :follow nil)
      (let ((gpg (shiftf (ssh-link-gpg-forward link) nil)))
        (when gpg
          (run-ssh link (ssh-arguments link :operation "cancel" :remote gpg) :seconds 10)))
      (run-ssh link (ssh-arguments link :operation "exit") :seconds 10)))
  (let ((master (shiftf (ssh-link-master link) nil)))
    (when master
      (tuatara.runtime:wait-until (lambda () (not (sb-ext:process-alive-p master))) 5)
      (tuatara.io:done-with-program master)))
  (stop-link-agent link)
  (tuatara.defaults::delete-private-folder (ssh-link-folder link))
  (tuatara.runtime:ignoring-errors-reported ("letting go of ~a" (ssh-link-folder link))
    (sb-posix:close (ssh-link-lock link))))
