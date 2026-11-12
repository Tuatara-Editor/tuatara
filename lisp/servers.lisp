;;;; lisp/servers.lisp --- The machines whose daemons you trust with more than keys and a screen.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A client of a daemon on another machine (tuatara --client=HOST) gives
;;;; that machine nothing of the desk but the keys typed and the size of the
;;;; screen. A machine on the list of trusted servers may be given more, each
;;;; thing turned on by itself and off until it is: the ssh agent, gpg-agent,
;;;; the clipboard and files dropped on the window. The list is kept between
;;;; sessions (a list of trust: a line not understood gives nothing) and
;;;; changed only by the commands of M-x server-list; the client reads it
;;;; before it runs ssh, so nothing on the other machine can turn one on.

(in-package #:tuatara.defaults)

(defun host-refusal (host)
  "Why HOST cannot name a machine for ssh, in words, or NIL when it can: one
to 255 ASCII letters, digits, ., -, _ and at most one @, the account's name
before it and the machine's after it each there and neither starting with -
\(which ssh could read as an option)."
  (let ((parts (uiop:split-string host :separator "@")))
    (unless (and (<= 1 (length host) 255) (<= (length parts) 2)
                 (every (lambda (c) (and (< (char-code c) 128) (or (alphanumericp c) (find c ".-_@")))) host)
                 (every (lambda (part) (and (plusp (length part)) (char/= (char part 0) #\-))) parts))
      (format nil "~a cannot name a machine for ssh: letters, digits, \".\", \"-\" and \"_\", a user@ before it if ~
                   you like, and no \"-\" first" host))))

(defun plain-path-p (path &key relative)
  "True when PATH is from the root, or with RELATIVE may be from a home
directory instead, and holds only ASCII letters, digits, /, ., _ and -, no part
of it .. and none starting with -: nothing ssh or a shell reads as a token
to expand (%u, ~), a separator (:), a space or an option."
  (and (stringp path) (plusp (length path))
       (or relative (char= (char path 0) #\/))
       (every (lambda (c) (or (and (< (char-code c) 128) (alphanumericp c)) (find c "/._-"))) path)
       (notany (lambda (part) (or (string= part "..") (and (plusp (length part)) (char= (char part 0) #\-))))
               (uiop:split-string path :separator "/"))))

(defun forwardable-path-p (path)
  "True when PATH can be named to ssh as a socket to forward: a plain path
\(PLAIN-PATH-P), short enough for a socket."
  (and (plain-path-p path) (< (length path) tuatara.io:+most-socket-path+)))

;;; Running ssh and the programs beside it

(defparameter *ssh-program* "ssh"
  "The ssh program run to reach another machine, as TUATARA.IO:SYSTEM-PROGRAM finds it.")

(defparameter *ssh-options*
  '("StrictHostKeyChecking=yes" "ForwardAgent=no" "ForwardX11=no" "GSSAPIDelegateCredentials=no"
    "ClearAllForwardings=yes" "PermitLocalCommand=no" "RemoteCommand=none" "RequestTTY=no"
    "ExitOnForwardFailure=yes" "ConnectTimeout=30" "ServerAliveInterval=15" "ServerAliveCountMax=4"
    "ForkAfterAuthentication=no" "Tunnel=no" "StdinNull=no")
  "The ssh options a connection to another machine is opened with, a
daemon's or the files there: on the command line, they win over the ssh
configuration's. Only machines whose host key is known already are reached;
no agent, display, Kerberos ticket, local command, remote command, forward
or tunnel of the configuration's goes with the connection, and ssh neither
goes into the background nor stops reading its input. What the
configuration runs to reach a machine (ProxyCommand, KnownHostsCommand, a
Match exec) still runs, as it does for ssh typed by hand.")

(defun ssh-option-arguments (&optional agent-option)
  "The arguments giving ssh *SSH-OPTIONS*, each after -o, with AGENT-OPTION,
when given, in place of ForwardAgent=no."
  (loop for option in *ssh-options*
        append (list "-o" (if (and agent-option (string= option "ForwardAgent=no")) agent-option option))))

(defparameter *ssh-variables*
  '("TERM" "SSH_AUTH_SOCK" "SSH_ASKPASS" "SSH_ASKPASS_REQUIRE" "DISPLAY" "WAYLAND_DISPLAY" "XAUTHORITY"
    "DBUS_SESSION_BUS_ADDRESS" "XDG_RUNTIME_DIR" "KRB5CCNAME")
  "The environment variables ssh is given, of those programs started here
have, besides TUATARA.IO:*RESTRICTED-VARIABLES*: your terminal, the agent
holding your keys and the program asking for a passphrase on the desktop,
and your Kerberos ticket, so that you log in to the other machine as you
would yourself. Its PATH is the system's own directories of programs, and
nothing else reaches it.")

(defun ssh-environment (&rest settings)
  "The environment ssh, and the programs beside it, run with, as NAME=VALUE
strings: the variables of *SSH-VARIABLES* programs started here have
\(TUATARA.IO:RESTRICTED-ENVIRONMENT), and PATH the system's own directories of
programs (TUATARA.IO:*SANDBOX-PATH*); each of SETTINGS, NAME=VALUE strings,
in place of the variable of its name."
  (apply #'tuatara.io:restricted-environment *ssh-variables* (format nil "PATH=~a" tuatara.io:*sandbox-path*)
         settings))

(defun ssh-askpass-window-p ()
  "The program ssh can ask for a passphrase in a window with, when a desktop
is running (TUATARA.IO:DESKTOP-P): the one SSH_ASKPASS names, as
gnome-ssh-askpass or ksshaskpass, else OpenSSH's own
\(TUATARA.IO:ASKPASS-PROGRAM); NIL when there is none or no desktop."
  (and (tuatara.io:desktop-p) (tuatara.io:askpass-program)))

(defun run-bounded (program arguments &key ask (seconds 30) (environment (ssh-environment)) input)
  "Run PROGRAM, by its full path, with ARGUMENTS in ENVIRONMENT, never
through a shell, SECONDS at most (TUATARA.IO:FILTER-THROUGH-PROGRAM). With
ASK, it has the editor's standard input, so that ssh or ssh-add can ask for
a password or passphrase on the terminal, as they do; INPUT, a file stream
or path, or octets, is its standard input, else it reads nothing. Three
values: its exit code, or NIL when it was ended for taking too long, and
what it wrote on its standard output and on its error output, the first
TUATARA.IO:*PROGRAM-SAID-LIMIT* octets of each, never to be shown raw."
  (let ((octets (and (typep input '(vector (unsigned-byte 8))) input)))
    (multiple-value-bind (output said code)
        (tuatara.io:filter-through-program
         (cons program arguments)
         (and octets (lambda (fd)
                       (handler-case (tuatara.io:write-octets fd octets :path program)
                         (tuatara.io:file-system-error () nil))))
         :input (cond (octets nil) (input) (ask t)) :seconds seconds :environment environment
         :limit tuatara.io:*program-said-limit* :truncate t)
      (values code (if output (tuatara.text:lenient-string output) "") said))))

(defun openssh-version (program)
  "The version of OpenSSH whose ssh is PROGRAM, as a list of numbers, 10.2
as (10 2) (TUATARA.IO:VERSION-NUMBERS); NIL when it does not say one."
  (tuatara.io:version-numbers (nth-value 2 (run-bounded program '("-V") :seconds 10)) "OpenSSH_"))

(defun ssh-settings (program host)
  "What ssh, PROGRAM, makes of its configuration for HOST (ssh -G), as a
list of (KEY . VALUE), the keys in lower case, in the order it says them."
  (multiple-value-bind (code said) (run-bounded program (list "-G" "--" host) :seconds 10)
    (and (eql code 0)
         (loop for line in (uiop:split-string said :separator '(#\Newline))
               for space = (position #\Space line)
               when space collect (cons (string-downcase (subseq line 0 space)) (subseq line (1+ space)))))))

(sb-ext:defglobal **ssh-machines** (make-hash-table :test 'equal :synchronized t)
  "The machine ssh reaches for each host name asked of SSH-MACHINE, as
\(MACHINE . WHEN), WHEN the internal real time it was asked.")

(defparameter *ssh-machine-seconds* 60
  "The seconds what ssh makes of a host name (SSH-MACHINE) is kept before ssh is asked again.")

(defun ssh-machine (host)
  "The machine ssh reaches for HOST ([user@]machine, or an alias of its
configuration), as hostname:port from what ssh makes of its configuration
\(SSH-SETTINGS), so that two names of one machine, and two accounts on it,
are told the same; HOST itself when ssh says none. Kept *SSH-MACHINE-SECONDS* for each
HOST. ssh -G reaches no machine, but is a program run on the thread asking:
not for the editor's own thread."
  (let ((kept (gethash host **ssh-machines**)) (now (get-internal-real-time)))
    (if (and kept (< (- now (cdr kept)) (* *ssh-machine-seconds* internal-time-units-per-second)))
        (car kept)
        (let* ((program (and (not (host-refusal host)) (tuatara.io:system-program *ssh-program*)))
               (settings (and program (ssh-settings program host)))
               (name (cdr (assoc "hostname" settings :test #'string=)))
               (machine (if name
                            (format nil "~a:~a" (string-downcase name)
                                    (or (cdr (assoc "port" settings :test #'string=)) "22"))
                            host)))
          (setf (gethash host **ssh-machines**) (cons machine now))
          machine))))

;;; The list

(defvar *servers-kept* 200
  "How many trusted servers are remembered.")

(defun server-line (entry)
  "The line of the list of trusted servers for ENTRY, (HOST AGENT
DESTINATIONS GPG CLIPBOARD FILES ADDED [PROGRAM])."
  (destructuring-bind (host agent destinations gpg clipboard files added &optional program) entry
    (format nil "~a~c~:[no~;yes~]~c~:[-~;~:*~{~a~^,~}~]~c~:[no~;yes~]~c~:[no~;yes~]~c~:[-~;~:*~a~]~c~a~@[~c~a~]"
            host #\Tab agent #\Tab destinations #\Tab gpg #\Tab clipboard #\Tab files #\Tab added
            (and program #\Tab) program)))

(defun parse-server-line (line)
  "The entry LINE of the list of trusted servers writes, (HOST AGENT
DESTINATIONS GPG CLIPBOARD FILES ADDED [PROGRAM]), or NIL for a line that makes
no sense. HOST is a machine as ssh takes it (HOST-REFUSAL). AGENT, GPG and
CLIPBOARD are true for yes. DESTINATIONS is the machines a key of the agent may
be used for from there, a list. FILES is the directory there dropped files go to,
or NIL. ADDED is the day it was trusted, as 2026-11-05. PROGRAM, when the line
has it and it is not -, is the tuatara to run there, a plain path
\(PLAIN-PATH-P), as it goes to the shell there. Every one of these fields must
be understood, or the line gives nothing; fields after them are passed over
\(STATE-LINE-FIELDS)."
  (let ((fields (state-line-fields line)))
    (when (<= 7 (length fields))
      (destructuring-bind (host agent destinations gpg clipboard files added &optional (program "-") &rest later) fields
        (declare (ignore later))
        (flet ((yes (field)
                 (cond ((string= field "yes") t)
                       ((string= field "no") nil)
                       (t (return-from parse-server-line nil)))))
          (let ((destinations (if (string= destinations "-") '() (uiop:split-string destinations :separator ","))))
            (and (null (host-refusal host))
                 (notany #'host-refusal destinations)
                 (or (string= files "-") (plain-path-p files :relative t))
                 (cl-ppcre:scan "^\\d{4}-\\d\\d-\\d\\d$" added)
                 (or (member program '("-" "") :test #'string=) (plain-path-p program :relative t))
                 (append (list host (yes agent) destinations (yes gpg) (yes clipboard)
                               (and (string/= files "-") files) added)
                         (and (not (member program '("-" "") :test #'string=)) (list program))))))))))

(define-state-list *trusted-servers* "ssh-servers" 'parse-server-line 'server-line '*servers-kept*
  :header (format nil "# The machines whose daemons are trusted with more than keys and the screen, the latest~%~
                       # first: the machine as ssh takes it; the ssh agent forwarded; the machines its keys~%~
                       # may be used for from there; gpg-agent forwarded; the clipboard shared; the directory~%~
                       # there dropped files go to; the day it was trusted; the tuatara to run there, when~%~
                       # another than the one on its PATH. Changed by M-x server-list.~%")
  :path-of #'first :fields 8 :trust t :line-key (field-line-key 7 0))

(defun server-permissions (host)
  "What the machine HOST, as given to tuatara --client=, is trusted with, as a
plist. :AGENT is the ssh agent forwarded. :DESTINATIONS is the machines its keys
may be used for from there. :GPG is gpg-agent forwarded. :CLIPBOARD is the
clipboard shared. :FILES is the directory there files dropped on the window go to,
or NIL. :PROGRAM is the tuatara to run there, or NIL for the one on its PATH.
NIL for a machine not on the list, which is given nothing."
  (let ((entry (state-list-find *trusted-servers* host)))
    (and entry
         (destructuring-bind (host agent destinations gpg clipboard files added &optional program) entry
           (declare (ignore host added))
           (list* :agent agent :destinations destinations :gpg gpg :clipboard clipboard :files files
                  (and program (list :program program)))))))

(defun change-server (host change)
  "Change what HOST, a trusted server, is trusted with: CHANGE, a function of
its entry giving the changed one, its place in the list kept."
  (let ((entry (state-list-find *trusted-servers* host)))
    (when entry
      (state-list-replace *trusted-servers* (list (funcall change (copy-list entry)))))))

;;; The list's buffer

(define-major-mode servers-mode marked-list-mode
  "The trusted servers (\\[server-list]): a line each machine whose daemon a
client here may be given more than keys and the screen, and what it is
given. \\<servers-mode>\\[server-trust] trusts another; on the line's
server, or those marked, \\[server-toggle-agent] turns forwarding the ssh agent on or off,
\\[server-agent-destinations] names the machines its keys may be used for from there,
\\[server-toggle-gpg] turns forwarding gpg-agent on or off, \\[server-toggle-clipboard] sharing the
clipboard, \\[server-toggle-files] sending dropped files to a directory there; each asks before
it gives anything. \\[server-tuatara-program] names the tuatara to run there.
\\[server-bring-language-server] and \\[server-remove-language-server] take a language server there
and away again. \\[list-mark-for-deletion] marks a server to forget, \\[list-execute] forgets those
marked, \\[list-delete-marked] the marked ones or the one on the line, after asking;
\\[revert-buffer] lists them again."
  :lighter "Servers" :interactive nil)

(defvar *servers-keymap* (mode-keymap 'servers-mode)
  "Keys of the list of trusted servers (servers-mode's keymap).")

(defparameter *servers-columns* '((" M" :width 2) ("Server" :width 24) ("Agent" :width 5) ("Keys used for" :width 24)
                                  ("GPG" :width 3)
                                  ("Clipboard" :width 9)
                                  ("Dropped files to" :width 18)
                                  ("Trusted" :width 10)
                                  ("Tuatara there"))
  "The columns of the list of trusted servers, as TABLE-HEADING-PIECES takes
them: the mark, the machine, whether the ssh agent is forwarded and the
machines its keys may be used for from there, whether gpg-agent is
forwarded and the clipboard shared, the directory dropped files go to, and the
day it was trusted.")

(defun servers-show (buffer &optional (point 2))
  "Fill BUFFER, the list of trusted servers, point on line POINT, or with
:keep on the line it was on, marks kept on the servers still there."
  (let ((entries (state-list-entries *trusted-servers*)))
    (render-table-list
     buffer *servers-columns* entries
     (lambda (entry mark section)
       (declare (ignore section))
       (destructuring-bind (host agent destinations gpg clipboard files added &optional program) entry
         (flet ((yes (on) (if on "yes" (cons "no" :list-dim))))
           (list (list-mark-sign mark) (cons host (list-mark-face mark)) (yes agent)
                 (if destinations (format nil "~{~a~^, ~}" destinations) (if agent "any" ""))
                 (yes gpg) (yes clipboard) (or files (cons "no" :list-dim)) (cons added :list-date)
                 (or program "")))))
     :marked-by (lambda (entry section) (declare (ignore section)) (first entry))
     :after (list (if entries
                      (format nil "~%   ~[No servers~;1 server~:;~:*~d servers~] trusted; a machine not listed is ~
                                   given nothing~%" (length entries))
                      (substitute-command-keys
                       (format nil "~%   No server is trusted: a daemon on another machine is given nothing of this ~
                                    one. \\<servers-mode>\\[server-trust] trusts one.~%"))))
     :point point
     :refill (lambda (buffer) (servers-show buffer :keep)))))

(keep-shown-current 'servers-mode (lambda (list) (declare (ignore list)) (state-list-entries *trusted-servers*)))

(define-command server-list ()
  "List the trusted servers, the machines whose daemons a client here
\(tuatara --client=HOST) may give more than the keys typed and the size of
the screen, each with what it is given: the ssh agent, with the machines its
keys may be used for from there; gpg-agent; the clipboard; files dropped on
the window, sent to a directory there. A machine not listed is given nothing.
There \\<servers-mode>\\[server-trust] trusts another, \\[server-toggle-agent], \\[server-toggle-gpg],
\\[server-toggle-clipboard] and \\[server-toggle-files] turn each on or off, after asking, and
\\[list-delete-marked] forgets one."
  (let ((buffer (buffer-in-mode "*Servers*" 'servers-mode)))
    (servers-show buffer)
    (show-buffer buffer)))

(define-command server-trust ()
  "Trust a machine asked for in the minibuffer, as tuatara --client= names
it (build.example, me@build.example, or an alias of ~/.ssh/config): it goes
on the list of trusted servers (\\[server-list]) with nothing given yet, each
thing then turned on there by itself."
  (let ((host (string-trim " " (read-string "Trust the server (as tuatara --client= names it): "))))
    (cond ((host-refusal host) (message "~a" (host-refusal host)))
          ((state-list-find *trusted-servers* host) (message "~a is trusted already" host))
          (t (state-list-put *trusted-servers*
                             (list (list host nil '() nil nil nil (format-time "%F" (get-universal-time)))))
             (refill-open-list "*Servers*" 'servers-mode)
             (message "~a is trusted, with nothing given yet: \\[server-list] turns each thing on" host)))))

(defun change-servers (noun ask change)
  "Change the servers marked in the list of trusted servers, or the one on
the line: ASK, a function of a host and its entry, gives the question asked
before the change, or NIL for none, and CHANGE, a function of the entry, the
changed one; NOUN says what changed, for the message."
  (act-on-list (current-buffer)
               (lambda (list hosts)
                 (let ((changed 0))
                   (dolist (host hosts)
                     (let* ((entry (state-list-find *trusted-servers* host))
                            (question (and entry (funcall ask host entry))))
                       (when (and entry (or (null question) (y-or-n-p* question)))
                         (change-server host change)
                         (incf changed))))
                   (unmark-list list)
                   (servers-show list :keep)
                   (message "~a: ~d server~:p changed" noun changed)))
               "server"))

(defun toggle-server (index noun gives)
  "Turn the permission at INDEX of the entries of the marked servers, or the
line's, on where it is off and off where it is on, asking before turning it
on, GIVES saying what the server is then given (a format of the host)."
  (change-servers noun
                  (lambda (host entry) (and (not (nth index entry)) (format nil "~?: turn it on?" gives (list host))))
                  (lambda (entry) (setf (nth index entry) (not (nth index entry))) entry)))

(define-command server-toggle-agent ()
  "Forward your ssh agent to the server on the line, or those marked, or no
longer: on, the programs there can sign with your keys while a client
shows its daemon, each use asked about here, and only for the machines
named by \\<servers-mode>\\[server-agent-destinations] where OpenSSH can limit them. Asked before it is turned on."
  (toggle-server 1 "ssh agent" "~a may then use your ssh keys, each use confirmed here"))

(define-command server-toggle-gpg ()
  "Forward gpg-agent to the server on the line, or those marked, or no
longer: on, gpg there signs and decrypts with your keys, which stay here,
and cannot export or change them. The passphrase is asked here; while
gpg-agent here keeps it (up to its max-cache-ttl, as much as 8 hours), the
server can sign and decrypt with that key again without asking you. Asked
before it is turned on."
  (toggle-server 3 "gpg-agent" "~a may then sign and decrypt with your GnuPG keys, without asking you again while ~
                                gpg-agent keeps the passphrase (up to its max-cache-ttl, as much as 8 hours)"))

(define-command server-toggle-clipboard ()
  "Share the clipboard with the server on the line, or those marked, or no
longer: on, what is copied there reaches the clipboard here, as text with
no control characters, and the daemon there may read what is copied here.
Asked before it is turned on."
  (toggle-server 4 "clipboard" "~a may then read your clipboard and copy into it"))

(define-command server-toggle-files ()
  "Send the files dropped on a window showing the daemon of the server on
the line, or those marked, to a directory there, asked for (from the home
directory there unless it begins with /), or no longer: on, a file dropped is
copied there, its size checked first, and nothing of it run."
  (change-servers "dropped files"
                  (lambda (host entry) (declare (ignore host entry)) nil)
                  (lambda (entry)
                    (setf (nth 5 entry)
                          (and (not (nth 5 entry))
                               (let ((directory (string-trim " " (read-string (format nil "Files dropped go on ~a to ~
                                                                                         the directory: " (first entry))
                                                                           :initial "Dropped"))))
                                 (if (plain-path-p directory :relative t)
                                     directory
                                     (refuse "~a: a directory is letters, digits, /, ., _ and -, no part .. or ~
                                              starting with -" directory)))))
                    entry)))

(define-command server-agent-destinations ()
  "Name, in the minibuffer, the machines the keys of your ssh agent may be
used for from the server on the line, or those marked, commas between
them (github.com, me@git.example); none for any. Kept with OpenSSH 8.9 or
later here, which refuses a key's use for any other machine."
  (change-servers "machines for the agent's keys"
                  (lambda (host entry) (declare (ignore host entry)) nil)
                  (lambda (entry)
                    (let* ((typed (read-string (format nil "Keys used from ~a for (commas between, none for any): "
                                                       (first entry))
                                               :initial (format nil "~{~a~^, ~}" (third entry))))
                           (hosts (remove "" (mapcar (lambda (h) (string-trim " " h))
                                                     (uiop:split-string typed :separator ","))
                                          :test #'string=))
                           (bad (find-if #'host-refusal hosts)))
                      (when bad (refuse "~a" (host-refusal bad)))
                      (setf (third entry) hosts)
                      entry))))

(define-command server-tuatara-program ()
  "Name, in the minibuffer, the tuatara to run on the server on the line, or
those marked, when it is not the one on the PATH of the commands ssh runs
there: /opt/tuatara/bin/tuatara, or a name found on that PATH; empty for the
one on that PATH. Letters, digits, /, ., _ and - only, as it goes to the
shell on that machine."
  (change-servers "the tuatara run there"
                  (lambda (host entry) (declare (ignore host entry)) nil)
                  (lambda (entry)
                    (let ((program (string-trim " " (read-string
                                                     (format nil "Tuatara to run on ~a (empty for its PATH's): "
                                                             (first entry))
                                                     :initial (or (nth 7 entry) "")))))
                      (unless (or (string= program "") (plain-path-p program :relative t))
                        (refuse "~a: a program is letters, digits, /, ., _ and -, no part .. or starting with -"
                                program))
                      (append (subseq entry 0 7) (and (string/= program "") (list program)))))))

(defmethod list-entry-noun ((mode servers-mode))
  "A trusted server."
  "server")

(defmethod delete-entries ((mode servers-mode) list keys mark)
  "Forget the servers, after asking: each is given nothing from then on."
  (declare (ignore mark))
  (if (y-or-n-p* (format nil "~a: each is given nothing from now on?" (list-question "Forget" "server" keys)))
      (progn (state-list-drop *trusted-servers* keys)
             (unmark-list list)
             (servers-show list :keep)
             (message "Forgot ~d server~:p" (length keys)))
      (message "Kept")))

(defun install-servers ()
  "Bind the keys of the list of trusted servers: \\`+' trusts another,
\\`A', \\`H', \\`G', \\`C' and \\`F' change what the servers marked are given,
\\`T' the tuatara run there."
  (define-keys *servers-keymap* '(("+" server-trust) ("A" server-toggle-agent) ("H" server-agent-destinations)
                                  ("G" server-toggle-gpg) ("C" server-toggle-clipboard) ("F" server-toggle-files)
                                  ("T" server-tuatara-program)))
  (commands-for-mode 'servers-mode "the list of trusted servers (\\[server-list])" 'server-toggle-agent
                     'server-agent-destinations 'server-toggle-gpg 'server-toggle-clipboard 'server-toggle-files
                     'server-tuatara-program 'server-bring-language-server 'server-remove-language-server)
  (commands-for-mode 'gpg-keys-mode "the list of GnuPG keys (\\[gpg-list-keys])" 'server-copy-keys))

;;; Public keys for the servers' keyrings

(defun run-on-server (host arguments &key input (seconds 60))
  "Run the command ARGUMENTS, words the shell there reads, on HOST through ssh,
nothing asked (BatchMode), nothing forwarded and no shared connection, SECONDS
at most: as RUN-BOUNDED, its exit code, what it wrote and what it said, the
first *PROGRAM-SAID-LIMIT* octets of each, never to be shown raw. Refused
when ssh is not in the system's directories of programs."
  (let ((program (or (tuatara.io:system-program *ssh-program*)
                     (refuse "ssh is not installed in the system's directories of programs"))))
    (run-bounded program (append '("-T" "-o" "BatchMode=yes" "-o" "ControlMaster=no" "-o" "ControlPath=none")
                                 (ssh-option-arguments) (list "--" host) arguments)
                 :input input :seconds seconds)))

(defun server-said-shown (said)
  "SAID, what a server's program wrote, on one line and at most 200
characters, as it may be shown."
  (let ((line (escape-controls (tuatara.text:one-line (string-trim '(#\Newline) said)))))
    (subseq line 0 (min 200 (length line)))))

(defun server-unreached-words (host code said seconds)
  "Why ssh's run of a command on HOST, with exit code CODE and SAID on its
error output, reached nothing there within SECONDS, in words; NIL when it did."
  (cond ((null code) (format nil "~a did not answer within ~d second~:p" host seconds))
        ((eql code 255) (format nil "ssh could not reach ~a without asking: ~a" host (server-said-shown said)))))

(defun import-on-server (host octets)
  "Import the armoured public keys OCTETS into the keyring of gpg on HOST,
through ssh with no question asked (BatchMode) and nothing forwarded: words
saying what was done (IMPORT-WORDS), as gpg there said it."
  (multiple-value-bind (code output complained)
      (run-on-server host '("gpg" "--batch" "--status-fd" "1" "--import") :input octets)
    (let ((statuses (loop for line in (uiop:split-string output :separator '(#\Newline))
                          when (uiop:string-prefix-p "[GNUPG:] " line) collect (subseq line 9))))
      (or (server-unreached-words host code complained 60)
          (format nil "On ~a: ~a" host (import-words statuses (server-said-shown complained)))))))

(define-command server-copy-keys ()
  "Copy the public keys marked in the list of GnuPG keys (\\[gpg-list-keys]),
or the one at the cursor, into gpg's keyring on a trusted server asked for,
so that \\[gpg-encrypt-file] there can encrypt to them: exported here and
imported there over ssh, in the background. Nothing secret is sent; ssh
logs in without asking, with a key of your agent."
  (let ((keys (gpg-keys-chosen))
        (hosts (mapcar #'first (state-list-entries *trusted-servers*))))
    (cond ((null keys) (message "No key on this line"))
          ((null hosts) (message "No server is trusted: \\[server-trust] trusts one"))
          (t (let ((host (completing-read (format nil "Copy ~d public key~:p to the server: " (length keys)) hosts
                                          :require-match t)))
               (when (member host hosts :test #'equal)
                 (with-exported-keys keys
                   (lambda (octets)
                     (gpg-in-background (format nil "importing ~d public key~:p on ~a" (length keys) host)
                                        (lambda (cancel) (declare (ignore cancel)) (import-on-server host octets))
                                        (lambda (words) (message "~a" words))
                                        :asker :agent)))))))))
