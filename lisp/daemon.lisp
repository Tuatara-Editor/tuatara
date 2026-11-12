;;;; lisp/daemon.lisp --- One editor in the background, shown by clients in their terminals.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; tuatara --daemon runs an editor with no screen; tuatara --client shows it
;;;; in a terminal, a frame of its own painted through a remote frontend
;;;; (src/frontend/remote.lisp). The daemon listens on a local socket in a
;;;; folder for you alone, $XDG_RUNTIME_DIR/tuatara/NAME.socket, held
;;;; by a lock beside it for as long as it runs. A connection is taken only
;;;; from a program run by the same account that sees the same files at the
;;;; same paths; it is greeted on a thread of its own, never the editor's, and
;;;; handed to the editor, which gives it a frame (ATTACH-CLIENT). Each client
;;;; has buffers of its own (src/command/clients.lisp); its frames close with
;;;; it and its buffers stay. While no client shows the editor, its one frame
;;;; is on a frontend with no screen. Nothing a client sends is run: it names
;;;; files, which are visited as any file is, and sends keys.

(in-package #:tuatara.defaults)

;;; Where a daemon listens

(defparameter *daemon-default-name* "default"
  "The name of the daemon tuatara --daemon starts, and --client and --stop
reach, when none is given.")

(defun daemon-name-p (name)
  "True when NAME may name a daemon: letters, digits, - and _, at most 40 of
them."
  (and (stringp name) (<= 1 (length name) 40)
       (every (lambda (c) (or (alphanumericp c) (member c '(#\- #\_)))) name)
       (every (lambda (c) (< (char-code c) 128)) name)))

(defun outside-owner ()
  "The owner a file shows when its real owner is one the editor's namespace
of user ids leaves out (the system's overflow id, 65534 as it comes), as a
user id, when it runs in such a namespace, as in a rootless container
\(toolbox, podman); NIL when it does not. There the system's /tmp, root's,
shows as owned by that id."
  (handler-case
      (let ((map (string-trim '(#\Space #\Newline) (uiop:read-file-string "/proc/self/uid_map"))))
        (and (not (equal (tuatara.text:split-words map) '("0" "0" "4294967295")))
             (parse-integer (uiop:read-file-string "/proc/sys/kernel/overflowuid") :junk-allowed t)))
    (file-error () nil)))

(defun runtime-folder-refusal (stamp what uid &key (private t) on-the-way)
  "Why the folder WHAT, as STAMP (not following a link) says it is, is not
safe for a daemon's socket, in words, or NIL when it is safe. It is safe when
it is a folder and no link and is UID's. With PRIVATE it must be for UID alone
(mode 700), else at least not writable by others. With ON-THE-WAY, WHAT is a
folder on the way to that one. It need only be root's or UID's, and writable
by others only with the sticky bit, as /tmp is, where no one may remove or
rename what is another's. A sticky one may also be owned by an id the editor's
namespace of user ids leaves out (OUTSIDE-OWNER), as /tmp is seen from a
container."
  (let ((mode (and stamp (tuatara.io:stamp-mode stamp))))
    (cond ((null stamp) (format nil "~a is not there" what))
          ((eq (tuatara.io:stamp-kind stamp) :symlink) (format nil "~a is a link, not a folder" what))
          ((not (eq (tuatara.io:stamp-kind stamp) :directory)) (format nil "~a is not a folder" what))
          ((not (member (tuatara.io:stamp-uid stamp)
                        (cond ((not on-the-way) (list uid))
                              ((logtest mode #o1000) (list 0 uid (outside-owner)))
                              (t (list 0 uid)))))
           (format nil "~a is another user's" what))
          ((if on-the-way
               (and (logtest mode #o022) (not (logtest mode #o1000)))
               (if private (/= (logand mode #o777) #o700) (logtest mode #o022)))
           (format nil "~a may be used by others (its mode is ~3,'0o)" what (logand mode #o777))))))

(defun folder-refusal (path what uid &key (private t))
  "Why the folder at PATH, a native name, is not safe to keep what only UID
may change in, in words naming it WHAT, or NIL when it is safe. It is checked
named from the root. Every folder on the way to it from there must be safe as
RUNTIME-FOLDER-REFUSAL says with ON-THE-WAY. A link on the way is followed and
where it leads is checked the same way. The folder itself must be no link and
safe as that says with PRIVATE."
  (flet ((named (parts) (format nil "/~{~a~^/~}" (reverse parts)))
         (on-the-way (name stamp)
           (runtime-folder-refusal stamp (format nil "~a, on the way to ~a," name what) uid :on-the-way t)))
    (if (not (uiop:string-prefix-p "/" path))
        (format nil "~a is not named from the root" what)
        (let ((passed '()) (links 0)
              (ahead (uiop:split-string path :separator "/")))
          (or (on-the-way "/" (tuatara.io:path-stamp "/" :follow nil :failure nil))
              (loop
                (let ((part (pop ahead)))
                  (cond ((and (null part) (null passed)) (return (format nil "~a is the root" what)))
                        ((null part)
                         (return (runtime-folder-refusal (tuatara.io:path-stamp (named passed) :follow nil :failure nil)
                                                         what uid :private private)))
                        ((member part '("" ".") :test #'string=))
                        ((string= part "..") (pop passed))
                        ((every (lambda (next) (string= next "")) ahead) (push part passed))
                        (t
                         (let* ((name (named (cons part passed)))
                                (stamp (tuatara.io:path-stamp name :follow nil :failure nil)))
                           (if (and stamp (eq (tuatara.io:stamp-kind stamp) :symlink))
                               (let ((target (handler-case (sb-posix:readlink name) (sb-posix:syscall-error () nil))))
                                 (when (or (null target) (> (incf links) 40))
                                   (return (format nil "~a, on the way to ~a, leads nowhere" name what)))
                                 (when (uiop:string-prefix-p "/" target) (setf passed '()))
                                 (setf ahead (append (uiop:split-string target :separator "/") ahead)))
                               (let ((why (on-the-way name stamp)))
                                 (when why (return why))
                                 (push part passed)))))))))))))

(defvar *runtime-directory* nil
  "The folder of the session's own files that daemons' sockets go in, or NIL
for the one $XDG_RUNTIME_DIR names, as the system sets it.")

(defun daemon-folder ()
  "The folder daemons' sockets are in, $XDG_RUNTIME_DIR/tuatara/ (or in
*RUNTIME-DIRECTORY*), a native name ending in /: made for you alone when it
is not there, and checked each time (RUNTIME-FOLDER-REFUSAL). Refused, saying
why, when XDG_RUNTIME_DIR is not set or the folder is not safe: there is no
other place for it."
  (let ((runtime (or *runtime-directory* (tuatara.io:own-variable "XDG_RUNTIME_DIR")))
        (uid (sb-posix:geteuid)))
    (unless (and runtime (plusp (length runtime)) (char= (char runtime 0) #\/))
      (refuse "XDG_RUNTIME_DIR is not set to a folder, and a daemon's socket goes nowhere else"))
    (let* ((runtime (tuatara.io:slash-ended runtime))
           (folder (tuatara.io:path-join runtime "tuatara/"))
           (why (folder-refusal runtime (format nil "XDG_RUNTIME_DIR (~a)" runtime) uid :private nil)))
      (when why (refuse "~a: no daemon's socket goes there" why))
      (unless (tuatara.io:path-stamp folder :follow nil :failure nil)
        (handler-case (tuatara.io:make-directory folder #o700)
          ;; made meanwhile by another start: it is checked as it is
          (tuatara.io:file-system-error () nil)))
      (let ((why (folder-refusal folder folder uid)))
        (when why (refuse "~a: no daemon's socket goes there" why)))
      folder)))

(defun daemon-path (name type)
  "The path of the daemon NAME's file of TYPE, \"socket\" or \"lock\", in
the daemons' folder (DAEMON-FOLDER); refused for a name that cannot name a
daemon (DAEMON-NAME-P) or one too long for a socket."
  (unless (daemon-name-p name)
    (refuse "~a cannot name a daemon: letters, digits, - and _, at most 40" name))
  (let ((path (concatenate 'string (daemon-folder) name "." type)))
    (when (> (length (tuatara.text:string-octets path)) tuatara.io:+most-socket-path+)
      (refuse "~a is too long a path for a socket: XDG_RUNTIME_DIR names a folder too deep" path))
    path))

;;; A daemon: its lock, its socket, and the clients being greeted

(defstruct (daemon (:constructor %make-daemon (name lock listener)) (:copier nil))
  "A daemon running in this process. NAME is its name, and its LOCK is held on
its lock file (a descriptor). LISTENER is the socket it takes connections on
(a descriptor). QUEUE holds the events of its frames, and THREAD is the thread
taking connections. GREETING is how many are being greeted. ARRIVED is the
remote frontends greeted and not yet given a frame, newest first, under MUTEX;
each is a new client's or (:WINDOW REMOTE WHAT) for a window asked of a
client. NAMESPACE is the mount namespace it runs in. ENDING is true once it
was asked to end with no one to ask (ASK-DAEMON-TO-END). LEAVING is a
semaphore signalled once it has ended while GUARD-DAEMON-LEAVING watches it,
NIL before."
  name lock listener
  (queue (tuatara.frontend:make-event-queue))
  (thread nil)
  (greeting 0 :type fixnum)
  (arrived '())
  (mutex (sb-thread:make-mutex :name "daemon"))
  (namespace (tuatara.io:mount-namespace (sb-posix:getpid)))
  (ending nil)
  (leaving nil))

(setf (documentation 'daemon-p 'function) "True when OBJECT is a DAEMON."
      (documentation 'daemon-name 'function) "The name of DAEMON."
      (documentation 'daemon-lock 'function) "The descriptor of DAEMON's lock file, its lock held."
      (documentation 'daemon-listener 'function) "The socket DAEMON takes connections on."
      (documentation 'daemon-queue 'function) "Where the events of DAEMON's frames go."
      (documentation 'daemon-thread 'function) "The thread taking DAEMON's connections."
      (documentation 'daemon-greeting 'function) "How many connections DAEMON is greeting."
      (documentation 'daemon-arrived 'function)
      "The remote frontends greeted and not yet given a frame, newest first; a window asked for as
\(:WINDOW REMOTE WHAT)."
      (documentation 'daemon-mutex 'function) "Held while DAEMON's GREETING or ARRIVED change."
      (documentation 'daemon-namespace 'function)
      "The mount namespace DAEMON runs in, as TUATARA.IO:MOUNT-NAMESPACE gives it."
      (documentation 'daemon-ending 'function) "True once DAEMON was asked to end with no one to ask.")

(defun open-daemon (name)
  "Make this process the daemon NAME: take its lock, so that no other daemon
of the name runs, take away a socket a daemon that ended left, and listen on
a new one. Refused, saying why, when one of the name runs or the folder is
not safe (DAEMON-FOLDER). Returns the DAEMON."
  (let* ((lock-path (daemon-path name "lock"))
         (socket (daemon-path name "socket"))
         (fd (handler-case (tuatara.io:open-lock-file lock-path)
               (tuatara.io:file-system-error (e)
                 (refuse "~a cannot be opened: ~a" lock-path (tuatara.io:file-system-error-reason e))))))
    (unless fd
      (refuse "A daemon named ~a runs already: tuatara --client~:[=~a~;~*~] shows it"
              name (string= name *daemon-default-name*) name))
    (handler-case
        (progn
          ;; a socket left by a daemon that ended without taking it away: no one holds its lock
          (case (tuatara.io:path-kind socket :follow nil)
            ((nil))
            (:socket (tuatara.io:remove-file socket))
            (t (refuse "Something that is no socket is at ~a: move it away" socket)))
          (%make-daemon name fd (tuatara.io:listen-at socket)))
      (error (e)
        (sb-posix:close fd)
        (error e)))))

(defun close-daemon (daemon)
  "Stop DAEMON taking connections, take its socket away and let its lock go."
  (let ((listener (shiftf (daemon-listener daemon) nil)))
    (when listener
      (tuatara.io:shut-socket listener)
      (tuatara.runtime:join-within (daemon-thread daemon) 2)
      (ignoring-errors-reported ("closing the daemon's socket") (sb-posix:close listener))
      (ignoring-errors-reported ("taking the daemon's socket away")
        (tuatara.io:remove-file (daemon-path (daemon-name daemon) "socket")))))
  (let ((lock (shiftf (daemon-lock daemon) nil)))
    (when lock (sb-posix:close lock)))
  ;; what was greeted and never given a frame is let go
  (dolist (item (sb-thread:with-mutex ((daemon-mutex daemon)) (shiftf (daemon-arrived daemon) '())))
    (ignoring-errors-reported ("letting a client go")
      (tuatara.frontend:frontend-stop (if (consp item) (second item) item)))))

;;; Taking connections

(defparameter *most-greeting* 8
  "The most connections a daemon greets at once; one more is refused.")

(defparameter *most-clients* 64
  "The most clients a daemon is shown by at once; one more is refused.")

(defun peer-refusal (uid namespace &key (own-uid (sb-posix:geteuid)) own-namespace)
  "Why a program at the other end of a connection may not be a client of a
daemon run by OWN-UID in OWN-NAMESPACE, in words, or NIL when it may. The
program is run by the account with user id (UID) UID in the mount namespace
NAMESPACE. It must be run by the same account, and see the same files at the
same paths."
  (cond ((not (eql uid own-uid))
         "This daemon is another user's: it takes clients only of the account that started it")
        ((or (null namespace) (not (equal namespace own-namespace)))
         (format nil "The daemon runs outside this client's container (another mount namespace), ~
                      where paths name other files: run tuatara --client where the daemon runs, ~
                      or a daemon in the container"))))

(defun live-client-count (editor)
  "How many clients show EDITOR now."
  (count-if (lambda (client) (and (client-remote client) (not (client-ended client)))) (editor-clients editor)))

(defun daemon-knows-root-p ()
  "True when a process shown as run by root here is the system's root: this
editor runs in the system's own namespace of user ids (OUTSIDE-OWNER), and not
as root itself."
  (and (null (outside-owner)) (plusp (sb-posix:geteuid))))

(defun peer-namespace (fd)
  "The mount namespace of the process at the other end of the connection FD,
as TUATARA.IO:MOUNT-NAMESPACE gives it, and its process id (PID), two
values: read through /proc while a pidfd of that very process
\(TUATARA.IO:PEER-PROCESS) says it still runs under that number, so that no
other process given its number meanwhile answers for it. The namespace is NIL
beside the PID when the system keeps it to itself, as it does sshd's, which
is the process connecting for a client on another machine; a third value is
then true when the OpenSSH server started that process
\(TUATARA.IO:STARTED-BY-SSH-SERVER-P) and this daemon can tell
\(DAEMON-KNOWS-ROOT-P). Namespace and PID are NIL when the
system cannot say, or the process ended; with :UNSUPPORTED as a second value
when this kernel cannot give the pidfd."
  (multiple-value-bind (pidfd pid) (tuatara.io:peer-process fd)
    (if (null pidfd)
        (values nil pid)
        (unwind-protect
             (let* ((namespace (tuatara.io:mount-namespace pid))
                    (by-ssh (and (null namespace)
                                 (daemon-knows-root-p)
                                 (tuatara.io:started-by-ssh-server-p pid))))
               (and (eql pid (tuatara.io:pidfd-process pidfd)) (values namespace pid by-ssh)))
          (sb-posix:close pidfd)))))

(defun take-connection (daemon editor fd)
  "Take the new connection FD to DAEMON, whose editor is EDITOR. It is
refused, saying why, when its peer is another account's or in another mount
namespace \(PEER-REFUSAL). It is refused too when that namespace cannot be
read and OpenSSH's server did not start it (PEER-NAMESPACE), or when as many
are greeted as may be at once \(*MOST-GREETING*). Otherwise it is greeted on a
thread of its own (GREET), and told when OpenSSH's server carries it, for only
a client on another machine may then be taken."
  (multiple-value-bind (namespace pid by-ssh) (peer-namespace fd)
    (let* ((uid (nth-value 1 (tuatara.io:peer-credentials fd)))
           (own (daemon-namespace daemon))
           (elsewhere (and by-ssh (peer-refusal uid nil :own-namespace own)))
           (why (cond ((eq pid :unsupported)
                       "This kernel cannot tell the daemon who connected (Linux 6.5 or newer is needed)")
                      ((or (null pid) (null uid)) "The daemon could not tell who connected")
                      ((peer-refusal uid (if by-ssh own namespace) :own-namespace own))
                      ((>= (daemon-greeting daemon) *most-greeting*)
                       "Too many clients are attaching at once: try again"))))
      (if why
          (progn (tuatara.runtime:debug-report :daemon "a connection refused (process ~a, user ~a): ~a" pid uid why)
                 (tuatara.frontend:refuse-connection fd why))
          (progn (sb-thread:with-mutex ((daemon-mutex daemon)) (incf (daemon-greeting daemon)))
                 (tuatara.runtime:make-guarded-thread (lambda () (greet daemon editor fd pid elsewhere))
                                                      :name "tuatara daemon greeting"))))))

(defun greet (daemon editor fd pid &optional elsewhere)
  "Greet the new connection FD to DAEMON, whose editor is EDITOR, from the
process PID (TUATARA.FRONTEND:MAKE-REMOTE-FRONTEND), and hand its frontend to
the editor, woken for it; a client refused or that says nothing it should
has its connection closed. With ELSEWHERE, words saying the process is not
known to see the files the daemon does, a new client is refused with them
before anything it sends is read, unless it says it is on another machine,
whose paths are the daemon's anyway. A connection that says it is another
window of a client, asked for by the editor from that client's process, is
handed on as that window (TUATARA.FRONTEND:TAKE-WINDOW-ASKED-FOR); it brings
nothing to open, the terminal it names is kept only when it may be lent
\(CHECKED-TERMINAL), and it is let go when no such window is waited for.
A new client is let go when as many show the editor as may (*MOST-CLIENTS*)."
  (unwind-protect
       (handler-case
           (let* ((remote (tuatara.frontend:make-remote-frontend
                           fd :queue (daemon-queue daemon)
                              :admit (lambda (request)
                                       (and elsewhere
                                            (not (tuatara.frontend:request-far request))
                                            (not (tuatara.frontend:request-joins request))
                                            elsewhere))))
                  (request (tuatara.frontend:remote-request remote))
                  (word (tuatara.frontend:request-joins request)))
             (flet ((let-go (words)
                      (setf (tuatara.frontend:remote-exit-code remote) 1
                            (tuatara.frontend:remote-exit-words remote) words)
                      (tuatara.frontend:frontend-stop remote))
                    (hand-on (item)
                      (sb-thread:with-mutex ((daemon-mutex daemon))
                        (push item (daemon-arrived daemon)))
                      (tuatara.frontend:queue-push (daemon-queue daemon) (tuatara.frontend:make-event :tick))))
               (setf (tuatara.frontend:remote-pid remote) pid)
               (if word
                   (multiple-value-bind (asker what) (tuatara.frontend:take-window-asked-for word pid)
                     (close-standard-input request)
                     (setf (tuatara.frontend:request-terminal request)
                           (checked-terminal (tuatara.frontend:request-terminal request)))
                     (if asker
                         (hand-on (list :window remote what))
                         (progn (tuatara.runtime:debug-report :daemon "a window no one asked for let go")
                                (let-go "The daemon asked for no such window"))))
                   (if (>= (live-client-count editor) *most-clients*)
                       (progn (close-standard-input request)
                              (let-go (format nil "The daemon is shown by ~d clients already, the most it takes"
                                              *most-clients*)))
                       (hand-on remote)))))
         (tuatara.frontend:remote-refused (condition)
           (tuatara.runtime:debug-report :daemon
                                         "a client refused: ~a"
                                         (tuatara.frontend:remote-refused-words condition))
           (tuatara.frontend:shut-and-close fd))
         (error (condition)
           (tuatara.runtime:debug-report :daemon
                                         "greeting a client failed: ~a"
                                         (tuatara.runtime:failure-words condition))
           (tuatara.frontend:shut-and-close fd)))
    (sb-thread:with-mutex ((daemon-mutex daemon)) (decf (daemon-greeting daemon)))))

(defun start-taking-connections (daemon editor)
  "Take DAEMON's connections on a thread of its own, until its socket is
shut (CLOSE-DAEMON), each as TAKE-CONNECTION takes it."
  (setf (daemon-thread daemon)
        (tuatara.runtime:make-guarded-thread
         (lambda ()
           (loop for fd = (let ((listener (daemon-listener daemon))) (and listener (tuatara.io:accept-on listener)))
                 while fd
                 do (take-connection daemon editor fd)))
         :name "tuatara daemon taking connections")))

;;; A client attached

(defun kept-client-environment (environment &optional far)
  "Those of ENVIRONMENT, a client's variables as (NAME . VALUE), the editor
keeps for it (TUATARA.COMMAND:CLIENT-VARIABLE-P), FAR for a client on
another machine."
  (remove-if-not (lambda (entry) (client-variable-p (car entry) far)) environment))

(defun far-client-agent (request)
  "The variable giving the programs of the frames of the client of REQUEST
the agent its ssh connection brings to this machine, as (NAME . VALUE) in a
list: SSH_AUTH_SOCK, for a client on another machine naming a path ssh can
forward (FORWARDABLE-PATH-P); else nothing."
  (let ((path (tuatara.frontend:request-agent request)))
    (and (tuatara.frontend:request-far request) path (forwardable-path-p path)
         (list (cons "SSH_AUTH_SOCK" path)))))

(defun checked-terminal (device &key (uid (sb-posix:geteuid)))
  "DEVICE, the terminal a client names, when it is one the client may lend
\(a program is given it as its terminal, GPG_TTY): a terminal device under
/dev/pts/ or a /dev/tty, its path with no .. in it, owned by the account
with user id (UID) UID; else NIL."
  (and device
       (or (uiop:string-prefix-p "/dev/pts/" device) (uiop:string-prefix-p "/dev/tty" device))
       (not (search "/.." device)) (not (search "../" device))
       (let ((stamp (tuatara.io:path-stamp device :failure nil)))
         (and stamp (eq (tuatara.io:stamp-kind stamp) :character-device) (eql (tuatara.io:stamp-uid stamp) uid)))
       device))

(defun close-standard-input (request)
  "Close the standard input REQUEST passed, unless it was taken to be read (TAKE-STANDARD-INPUT)."
  (let ((fd (shiftf (tuatara.frontend:request-standard-input request) nil)))
    (when (integerp fd)
      (ignoring-errors-reported ("closing a client's standard input") (sb-posix:close fd)))))

(defun take-standard-input (request)
  "The descriptor of the standard input REQUEST passed, or NIL, taken from
it: whoever takes it closes it."
  (let ((fd (shiftf (tuatara.frontend:request-standard-input request) nil)))
    (and (integerp fd) fd)))

(defun attach-client (editor remote)
  "Give EDITOR a client on REMOTE, a remote frontend greeted, with a frame
showing a scratch buffer of its own (CLIENT-SCRATCH, ADD-CLIENT). What the
client asked for (TUATARA.FRONTEND:REMOTE-REQUEST) is done in its frame as
soon as the editor is free (START-CLIENT); a client that ends first has its
standard input closed unread. The terminal it names is kept only when it
may be lent (CHECKED-TERMINAL). A client on another machine is given the
agent its ssh connection brings, for the programs of its frames alone
\(FAR-CLIENT-AGENT), and what its connection said is shown in its frame."
  (let* ((request (tuatara.frontend:remote-request remote))
         (named (tuatara.frontend:request-terminal request))
         (client nil))
    (setf (tuatara.frontend:request-terminal request) (checked-terminal named))
    (when (and named (not (tuatara.frontend:request-terminal request)))
      (tuatara.runtime:debug-report :daemon "a client's terminal not taken: ~a" (tuatara.frontend:shown-words named)))
    (setf client (add-client editor remote
                             :directory (tuatara.io:slash-ended (tuatara.frontend:request-directory request))
                             :environment (append (far-client-agent request)
                                                  (kept-client-environment
                                                   (tuatara.frontend:request-environment request)
                                                   (tuatara.frontend:request-far request)))
                             :buffer #'client-scratch
                             :start (lambda () (unless (client-ended client) (start-client editor client request)))))
    (push (lambda (client) (declare (ignore client)) (close-standard-input request)) (client-endings client))
    (let ((notes (tuatara.frontend:request-notes request))
          (frame (first (client-frames editor client))))
      (when (and notes frame *daemon*)
        (tuatara.frontend:queue-push
         (daemon-queue *daemon*)
         (tuatara.frontend:make-event :said :frame frame :text (escape-controls (format nil "~{~a~^; ~}" notes))))))
    (tuatara.runtime:debug-report :daemon "client ~d attached in ~a~@[ (~a)~]" (client-number client)
                                  (tuatara.frontend:shown-words (client-directory client))
                                  (tuatara.frontend:request-terminal request))
    client))

(defun take-in-clients (editor)
  "Tick function of a daemon (*TICK-FUNCTIONS*): give each client greeted
since a frame of EDITOR (ATTACH-CLIENT), and each window a client opened as
asked its frame (OPEN-FRAME-ASKED-FOR), each apart. One that cannot be
attached is let go, its standard input closed. A window asked for that has
not come in time is no longer waited for, and that is said in a frame of the
client that asked. What is attached is painted as soon as the editor paints,
inside a question too. Wants a call of its own only while a window is waited
for: what is greeted wakes the editor."
  (let ((daemon *daemon*))
    (when (and daemon (daemon-arrived daemon))
      (incf *display-generation*)
      (dolist (item (reverse (sb-thread:with-mutex ((daemon-mutex daemon)) (shiftf (daemon-arrived daemon) '()))))
        (let ((remote (if (consp item) (second item) item)))
          (handler-case (if (consp item)
                            (open-frame-asked-for editor remote (third item))
                            (attach-client editor remote))
            (error (condition)
              (tuatara.runtime:debug-report :daemon "a client could not be attached: ~a"
                                            (tuatara.runtime:failure-words condition))
              (close-standard-input (tuatara.frontend:remote-request remote))
              (ignoring-errors-reported ("letting a client go") (tuatara.frontend:frontend-stop remote)))))))
    (multiple-value-bind (late next) (tuatara.frontend:windows-not-come)
      ;; said in a frame of the client that asked, when it is still there
      (dolist (what late)
        (let ((frame (first (client-frames editor (first what)))))
          (when (and frame daemon)
            (tuatara.frontend:queue-push
             (daemon-queue daemon)
             (tuatara.frontend:make-event :said :frame frame
                                                :text (format nil "The window asked for did not open within ~d seconds"
                                                              tuatara.frontend:*window-seconds*))))))
      next)))

(defun start-client (editor client request)
  "Do what CLIENT of EDITOR asked for as it attached, REQUEST, in its frame.
It may ask to open its files (standard input among them), each where its +LINE
asks, the first shown. It may ask to compare or merge files. Asking for none,
it takes up the folder of the client that ended last, with its layouts, when
the session is kept (TAKE-UP-ENDED-FOLDER). It may also ask to open the frames
asked for, or to stop the editor. A file that cannot be opened is said, as
when the editor starts with it."
  (let* ((merge (tuatara.frontend:request-merge request))
         (entries (if merge
                      (list (fourth merge))
                      (loop for (path) in (tuatara.frontend:request-files request)
                            collect (if (or (tuatara.frontend:request-safe request)
                                            (tuatara.frontend:request-hex request))
                                        (list path :safe (tuatara.frontend:request-safe request)
                                                   :hex (tuatara.frontend:request-hex request))
                                        path))))
         (input nil))
    (when (tuatara.frontend:request-stop request)
      (close-standard-input request)
      (return-from start-client (stop-from-client editor client request)))
    (setf input (take-standard-input request))
    (when input
      (setf entries (append entries (list (list :standard-input input)))))
    (let ((buffers (visit-startup-files entries)))
      (setf (client-startup-buffers client) buffers)
      (dolist (buffer buffers) (join-client buffer client))
      (when buffers (show-buffer (first buffers))))
    (let ((places (remove-if-not #'second (tuatara.frontend:request-files request))))
      (when places (show-start-places places)))
    (let ((diff (tuatara.frontend:request-diff request)))
      (when diff
        (compare-paths (first diff) (second diff))
        (only-window (current-window))))
    (when merge (merge-files-into (first merge) (second merge) (third merge) (fourth merge)))
    (unless (or entries input (tuatara.frontend:request-diff request))
      (take-up-ended-folder editor client))
    (let ((frames (tuatara.frontend:request-frames request)))
      (when (> frames 1) (open-start-frames editor frames)))))

;;; A client closed

(defun only-clients-buffer-p (buffer client)
  "True when BUFFER is CLIENT's and no other client that has not ended has it too."
  (let ((clients (buffer-clients buffer)))
    (and (member client clients)
         (notany (lambda (other) (and (not (eq other client)) (not (client-ended other)))) clients))))

(defun client-encrypted-buffers (client)
  "The buffers of encrypted files that are CLIENT's alone (ONLY-CLIENTS-BUFFER-P)."
  (remove-if-not (lambda (buffer) (and (encrypted-buffer-p buffer) (only-clients-buffer-p buffer client)))
                 tuatara.buffer:*buffers*))

(defun close-encrypted-with-client (client)
  "Client end function (*CLIENT-END-FUNCTIONS*): close CLIENT's encrypted
buffers (CLIENT-ENCRYPTED-BUFFERS), their text not kept beyond those who
opened them, what was killed or copied to registers from them let go
\(FORGET-PLAINTEXT-COPIES); one with changes not saved is kept, as its text
would be lost, and that is said."
  (dolist (buffer (client-encrypted-buffers client))
    (forget-plaintext-copies buffer)
    (if (tuatara.buffer:buffer-modified-p buffer)
        (message "~a, encrypted, has changes not saved: kept, though its client ended"
                 (tuatara.buffer:buffer-name buffer))
        (tuatara.buffer:kill-buffer buffer))))

(pushnew 'close-encrypted-with-client *client-end-functions*)

(defun stop-reading-for-client (client)
  "Client end function (*CLIENT-END-FUNCTIONS*): stop reading the standard
input CLIENT passed, which may never end; what was read stays in *stdin*."
  (dolist (buffer (client-startup-buffers client))
    (dolist (job (tuatara.runtime:jobs-of buffer :running t))
      (when (standard-input-job-p job)
        (tuatara.runtime:job-stop job)))))

(pushnew 'stop-reading-for-client *client-end-functions*)

(defun close-client ()
  "Close the client working in the frame, as \\[save-buffers-kill-editor]
does in a client: its frames close and it exits, the editor going on. Its
encrypted buffers close with it; each with changes not saved is asked about
first: s saves it, l closes it without saving, \\`C-g' keeps the client.
Its other buffers stay, unsaved ones too, in a folder of their own in
\\[buffers], for another client to take up."
  (let* ((editor *editor*)
         (client (current-client editor)))
    (dolist (buffer (client-encrypted-buffers client))
      (when (tuatara.buffer:buffer-modified-p buffer)
        (let ((*buffers-asked-about* (list buffer)))
          (case (read-answer-key (format nil "~a is encrypted and has changes not saved: s saves it, ~
                                              l closes it without saving (C-g keeps this client) "
                                         (tuatara.buffer:buffer-name buffer))
                                 '(#\s #\l))
            (#\s (save-buffer-with-care buffer :then #'tuatara.buffer:kill-buffer))
            (#\l (tuatara.buffer:kill-buffer buffer))))))
    (end-client editor client)))

;;; A client whose last buffer is closed

(defvar *clients-without-buffers* '()
  "The clients of the daemon that lost their last buffer to a kill, not yet
ended (END-CLIENTS-WITHOUT-BUFFERS).")

(defun client-has-work-in-p (client buffer)
  "True when BUFFER is one of CLIENT's that holds something of its own: any
but a scratch buffer of CLIENT's (CLIENT-SCRATCH) while nothing is typed in
it. A buffer a client that ended had and another took up is the other's
alone (BUFFER-CLIENTS names the clients that have it); a buffer shown to
everyone, as *Messages*, is no one's; and the buffer list holds nothing of its
own, so that killing a client's last buffer from it ends that client as
\\[kill-buffer] there would."
  (and (member client (buffer-clients buffer))
       (not (typep (buffer-major-mode buffer) 'buffers-mode))
       (not (and (tuatara.buffer:buffer-local buffer :client-scratch) (not (tuatara.buffer:buffer-modified-p buffer))))))

(defun note-client-without-buffers (buffer &optional (clients (buffer-clients buffer)))
  "Kill-buffer function: note each client that has BUFFER, about to be killed,
and no other buffer holding something of its own (CLIENT-HAS-WORK-IN-P): its
last is closed. CLIENTS are those losing BUFFER: by default all that have
it, or the one that leaves it (LEAVE-SHARED-BUFFER). A client started with
no files is not noted until a buffer of its own has been closed, its scratch
buffer being all it has."
  (dolist (client clients)
    (when (and (client-remote client) (not (client-ended client))
               (notany (lambda (other) (and (not (eq other buffer)) (client-has-work-in-p client other)))
                       tuatara.buffer:*buffers*))
      (pushnew client *clients-without-buffers*))))

(pushnew 'note-client-without-buffers tuatara.buffer:*kill-buffer-functions*)

(defun shares-buffer-p (client buffer)
  "True when BUFFER is CLIENT's, a client of a daemon, and another client
that has not ended has it too (ONLY-CLIENTS-BUFFER-P): killed from CLIENT,
it is left rather than killed (LEAVE-SHARED-BUFFER)."
  (and client (client-remote client) (not (client-ended client)) (member client (buffer-clients buffer))
       (not (only-clients-buffer-p buffer client))))

(defun leave-shared-buffer (buffer client)
  "When BUFFER is CLIENT's and another running client has it too
\(SHARES-BUFFER-P), take it from CLIENT alone: it is no longer CLIENT's,
CLIENT's frames show another of its own in its place (REPLACEMENT-BUFFER),
and BUFFER stays for the others, unsaved changes and all. CLIENT, left with
nothing of its own, ends as when its last buffer is killed
\(NOTE-CLIENT-WITHOUT-BUFFERS). True when BUFFER was left."
  (when (shares-buffer-p client buffer)
    (note-client-without-buffers buffer (list client))
    (setf (tuatara.buffer:buffer-local buffer :clients) (remove client (buffer-clients buffer)))
    (replace-buffer-in-windows buffer client)
    t))

(defun end-clients-without-buffers (editor)
  "Tick function of a daemon (*TICK-FUNCTIONS*): end each client whose last
buffer was closed, as \\[save-buffers-kill-editor] does in it, with 0, but one
that is kept (CLIENT-KEPT-P); the daemon and the other clients go on."
  (loop while *clients-without-buffers*
        do (let ((client (pop *clients-without-buffers*)))
             (unless (or (client-ended client) (client-kept-p editor client))
               (end-client editor client)))))

;;; Stopping the daemon

(defun unsaved-by-folder (editor)
  "The buffers of EDITOR with changes leaving it would lose (UNSAVED-P), in
the folders of the buffer list (BUFFER-FOLDER), each (FOLDER . BUFFERS), the
folder of the client working in the frame first."
  (let ((client (current-client editor)))
    (group-sections (remove-if-not #'unsaved-p tuatara.buffer:*buffers*)
                    (lambda (b) (buffer-folder b client))
                    :first client :last :everyone)))

(defun ask-about-unsaved-by-folder (editor)
  "Ask about the changes not saved in EDITOR's buffers, folder by folder
\(UNSAVED-BY-FOLDER): s saves those of the folder, l leaves them unsaved,
\\`C-g' gives the question up, as it does any. Their saves are begun."
  (loop for (folder . buffers) in (unsaved-by-folder editor)
        do (let ((*buffers-asked-about* buffers))
             (when (char= #\s (read-answer-key
                               (format nil "~a: ~d with changes not saved (~{~a~^, ~}): s saves them, ~
                                            l leaves them (C-g keeps the daemon running) "
                                       (buffer-section-title folder) (length buffers)
                                       (mapcar #'tuatara.buffer:buffer-name buffers))
                               '(#\s #\l)))
               (save-buffers-saying buffers "Saved ~d file~:p")))))

(defparameter *leaving-seconds* 60
  "The seconds a daemon told to stop has to end. Everything it does as it
stops takes less; one still at work then is ended, so that a daemon of its
name can start again (GUARD-DAEMON-LEAVING).")

(defvar *end-stuck-daemon* (lambda () (sb-ext:exit :code 1 :abort t))
  "Called, with nothing, to end the process of a daemon that did not stop in
time (GUARD-DAEMON-LEAVING).")

(defun stuck-daemon-words (thread)
  "What a daemon that did not stop in time writes in its log: the threads
running, then the calls under way on THREAD, its editor's, innermost first,
each by its function alone, never what it was given. THREAD gets two seconds
to answer; one that cannot be asked is said so."
  (let ((calls '()) (answered (sb-thread:make-semaphore :name "the calls of a stuck daemon")))
    (tuatara.runtime:ignoring-errors-reported ("asking the editor's thread for its calls")
      (sb-thread:interrupt-thread thread (lambda ()
                                           (setf calls (let ((tuatara.runtime:*secret-at-hand* t))
                                                         (tuatara.runtime:backtrace-lines)))
                                           (sb-thread:signal-semaphore answered))))
    (sb-thread:wait-on-semaphore answered :timeout 2)
    (format nil "The daemon did not stop in time and is ended. Its threads: ~{~a~^, ~}~%~
                 ~:[Its editor's thread did not say what it waits for~;Its editor's thread waits in:~:*~{~%  ~a~}~]"
            (mapcar #'sb-thread:thread-name (sb-thread:list-all-threads))
            calls)))

(defun guard-daemon-leaving (daemon seconds)
  "From now on DAEMON, whose editor runs on this thread, has SECONDS to end:
when it has not (CLOSE-DAEMON's caller signals its LEAVING), what held it is
written in its log (STUCK-DAEMON-WORDS) and the process ends, by
*END-STUCK-DAEMON*. The first call counts; those after it change nothing."
  (unless (daemon-leaving daemon)
    (let ((left (sb-thread:make-semaphore :name "the daemon has ended"))
          (editor-thread sb-thread:*current-thread*))
      (setf (daemon-leaving daemon) left)
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (unless (sb-thread:wait-on-semaphore left :timeout seconds)
           (unwind-protect (progn (format *error-output* "~a~%" (stuck-daemon-words editor-thread))
                                  (finish-output *error-output*))
             (funcall *end-stuck-daemon*))))
       :name "tuatara daemon leaving"))))

(defun stop-daemon (editor &key (words "The daemon was stopped (M-x daemon-quit)") (here (current-client editor)))
  "Stop EDITOR, a daemon's: every client but HERE, by default the one working
in the frame, is told the daemon ends, with WORDS, and exits 1; HERE exits
0; then the editor ends, writing what it keeps as an editor does, at once
wherever this is called from (it is woken), within *LEAVING-SECONDS*
(GUARD-DAEMON-LEAVING)."
  (guard-daemon-leaving *daemon* *leaving-seconds*)
  (dolist (client (editor-clients editor))
    (unless (or (eq client here) (client-ended client) (not (client-remote client)))
      (setf (client-exit-code client) 1 (client-exit-words client) words)
      (end-client editor client)))
  (setf (editor-running editor) nil)
  (wake-editor editor))

(define-command daemon-quit ()
  "Stop the editor running in the background (tuatara --daemon), from any of
its clients, asked first about every change not saved in it, the clients
that ended included, folder by folder: s saves those of a folder, l leaves
them unsaved, \\`C-g' keeps the daemon running; the saves begun are waited
for (\\`C-g' there keeps it running too). Then every other client is told the
daemon ends, gives its terminal back and exits 1, and the daemon writes what
it keeps (the files visited lately, the places) and ends.
\\[save-buffers-kill-editor] in a client closes that client alone."
  (let ((editor *editor*))
    (unless *daemon*
      (refuse "This editor is not running in the background: ~a quits it"
              (substitute-command-keys "\\[save-buffers-kill-editor]")))
    (let ((answers (loop for query in *quit-query-functions*
                         for answer = (query-answer query "stop the daemon")
                         unless answer return :stay
                         collect answer)))
      (unless (eq answers :stay)
        (ask-about-unsaved-by-folder editor)
        ;; the saves just begun end before the daemon does, never cut short by its exit
        (unless (wait-for-saves-begun)
          (refuse "Not stopped: saves are still running"))
        (dolist (answer answers) (when (functionp answer) (call-hook-function answer)))
        (stop-daemon editor)))))

(defparameter *no-one-to-ask-save-seconds* 30
  "The seconds a daemon stopping with no one to ask (tuatara --stop with no
terminal, SIGTERM at logout) waits for the saves running to end before it
ends all the same: well within what the system gives a program asked to end.")

(defun wait-for-saves-begun (&key seconds)
  "Wait for the saves running in the background, those an exit waits for
\(TUATARA.RUNTIME:JOB-AT-EXIT :WAIT), to end, as WAIT-FOR-SAVES waits, at
most SECONDS when given: true when they all ended, or none ran."
  (let ((saves (remove-if-not (lambda (job) (eq :wait (tuatara.runtime:job-at-exit job))) (running-jobs))))
    (or (null saves) (wait-for-saves saves :seconds seconds))))

(defun unsaved-not-kept ()
  "The buffers with changes leaving the editor would lose (UNSAVED-P) that
no auto-saved copy would keep (AUTO-SAVE-WANTED-P, as the settings stand):
a new buffer with no file, an encrypted file's text, a file opened safely,
or every one when auto-saving is off."
  (remove-if (lambda (buffer) (and (tuatara.buffer:buffer-modified-p buffer) (auto-save-wanted-p buffer)))
             (remove-if-not #'unsaved-p tuatara.buffer:*buffers*)))

(defun say-in-daemon-log (control &rest arguments)
  "Say what CONTROL and ARGUMENTS make, as FORMAT makes it, in *Messages* and
on the standard error, which is a daemon's log
\(~/.cache/tuatara/daemon-NAME.log): for what a daemon does where no one sees
it."
  (let ((words (apply #'format nil control arguments)))
    (message "~a" words)
    (format *error-output* "~a~%" words)
    (finish-output *error-output*)))

(defun auto-save-unsaved ()
  "Write the changes not saved of the editor's buffers as auto-saved copies,
for \\[recover-file] and \\[list-auto-saves] to offer on the next visit:
each that may be auto-saved (AUTO-SAVE-WANTED-P). Said in *Messages* and the
daemon's log (SAY-IN-DAEMON-LOG)."
  (let ((written 0))
    (dolist (buffer tuatara.buffer:*buffers*)
      (when (and (tuatara.buffer:buffer-modified-p buffer) (auto-save-wanted-p buffer))
        (ignoring-errors-reported ("auto-saving ~a" (tuatara.buffer:buffer-name buffer))
          (auto-save-now buffer)
          (incf written))))
    (say-in-daemon-log "The daemon stopped with no one to ask: ~d buffer~:p with changes auto-saved" written)))

(defun lost-changes-count-words (lost)
  "What LOST, buffers whose changes no auto-saved copy would keep, are, in
words: how many, how many of them encrypted, and the names of the others
alone, an encrypted file's name being kept to its client."
  (let ((encrypted (count-if #'encrypted-buffer-p lost))
        (named (mapcar #'tuatara.buffer:buffer-name (remove-if #'encrypted-buffer-p lost))))
    (format nil "~d buffer~:*~p ~:*~[~;has~:;have~] changes no auto-saved copy would keep~
                 ~[~:;, ~:*~d of them encrypted~]~@[: ~{~a~^, ~}~]"
            (length lost) encrypted named)))

(defun lost-changes-words (lost)
  "Why the daemon is not stopped with no one to ask, LOST being the buffers
whose changes would be lost (LOST-CHANGES-COUNT-WORDS), and where it can be
stopped instead."
  (format nil "The daemon goes on: ~a; stop it from a terminal (tuatara --stop in one, or M-x daemon-quit in a ~
               client), where they are asked about"
          (lost-changes-count-words lost)))

(defun stop-with-no-one-to-ask (editor words &key keep-changes here)
  "Stop EDITOR, a daemon's, where no one can be asked. First the changes not
saved are written as auto-saved copies (AUTO-SAVE-UNSAVED), as the auto-saving
settings allow and never of an encrypted file's text or a file opened safely,
so that they are kept even if the process is killed while it waits. Then the
saves running are waited for, at most *NO-ONE-TO-ASK-SAVE-SECONDS* (those
still running then said in the log). Then every client but HERE is told WORDS
and exits 1, HERE exiting 0 \(STOP-DAEMON). Changes no copy would keep
(UNSAVED-NOT-KEPT) are lost, and this is said in the daemon's log
(SAY-IN-DAEMON-LOG). With KEEP-CHANGES nothing is stopped while there are such
changes, and why is returned in words \(LOST-CHANGES-WORDS). NIL once stopped.
However it goes, the process ends in the time the saves are given and
*LEAVING-SECONDS* (GUARD-DAEMON-LEAVING)."
  (let ((lost (unsaved-not-kept)))
    (if (and lost keep-changes)
        (lost-changes-words lost)
        (progn (guard-daemon-leaving *daemon* (+ *no-one-to-ask-save-seconds* *leaving-seconds*))
               (auto-save-unsaved)
               (unless (wait-for-saves-begun :seconds *no-one-to-ask-save-seconds*)
                 (say-in-daemon-log "The daemon stopped with no one to ask while saves still ran, ~
                                     cut short: ~{~a~^, ~}"
                                    (loop for job in (running-jobs)
                                          when (eq :wait (tuatara.runtime:job-at-exit job))
                                            collect (tuatara.runtime:job-buffer-name job))))
               (when lost
                 (say-in-daemon-log "The daemon stopped with no one to ask, though ~a: they are lost"
                                    (lost-changes-count-words lost)))
               (stop-daemon editor :words words :here here)
               nil))))

(defun stop-from-client (editor client request)
  "Stop EDITOR as CLIENT asked as it attached (tuatara --stop), REQUEST
saying how: asked in its terminal as \\[daemon-quit] asks. With no
terminal, where nothing can be asked, as STOP-WITH-NO-ONE-TO-ASK stops it,
keeping every change: when a change no auto-saved copy would keep is there,
the daemon is not stopped, and CLIENT is told which buffers hold such
changes. Either way not stopped, the daemon goes on and CLIENT closes,
exiting 1; stopped, CLIENT says so as it ends."
  (flet ((not-stopped (words)
           (setf (client-exit-code client) 1 (client-exit-words client) words)
           (end-client editor client)))
    (setf (client-exit-words client) (format nil "The daemon ~a is stopped" (daemon-name *daemon*)))
    (if (null (tuatara.frontend:request-terminal request))
        (let ((refused (stop-with-no-one-to-ask editor "The daemon was stopped (tuatara --stop)"
                                                :keep-changes t :here client)))
          (when refused (not-stopped refused)))
        (let ((stopped nil))
          (unwind-protect
               (progn (run-command editor 'daemon-quit)
                      (setf stopped (not (editor-running editor))))
            (unless stopped
              (not-stopped "The daemon goes on: nothing was stopped")))))))

;;; Asked to end by the system

(defun ask-daemon-to-end (daemon)
  "Have DAEMON's editor end as soon as it can, with no one asked
\(END-WHEN-ASKED): what SIGTERM does, as the desktop sends it at logout.
Fit for a signal's handler: it only marks DAEMON and wakes its editor, from a
thread of its own."
  (setf (daemon-ending daemon) t)
  (tuatara.runtime:make-guarded-thread
   (lambda () (tuatara.frontend:queue-push (daemon-queue daemon) (tuatara.frontend:make-event :tick)))
   :name "tuatara daemon asked to end"))

(defun end-daemon-on-terminate (daemon)
  "Have SIGTERM end DAEMON, its editor in this process, as ASK-DAEMON-TO-END
says, rather than end the process at once."
  (sb-sys:enable-interrupt sb-unix:sigterm (lambda (signal info context)
                                             (declare (ignore signal info context))
                                             (ask-daemon-to-end daemon)))
  nil)

(defun end-when-asked (editor)
  "Tick function of a daemon (*TICK-FUNCTIONS*). Once it was asked to end
\(ASK-DAEMON-TO-END), it stops EDITOR as tuatara --stop with no terminal does
\(STOP-WITH-NO-ONE-TO-ASK). Whatever changes would be lost, it is not asked
whether it may: every change an auto-saved copy may keep is kept so. Those
that cannot be kept are said in the log, every client exits 1, and the daemon
writes what it keeps. It is also taken in inside a question, which is given
up."
  (let ((daemon *daemon*))
    (when (and daemon (daemon-ending daemon) (editor-running editor))
      (stop-with-no-one-to-ask editor "The daemon was asked to end (SIGTERM, as at logout)" :here nil)))
  nil)

;;; Running a daemon's editor

(defun run-daemon-editor (daemon &key (setup #'install-defaults) ready)
  "Run DAEMON's editor on this thread until it stops: an editor whose frame
is on a frontend with no screen while no client shows it, its clients given
frames as they attach (START-TAKING-CONNECTIONS, TAKE-IN-CLIENTS), ended
when it is asked to (END-WHEN-ASKED), its session kept in a file of its own
\(*SESSION-NAME*). SETUP is called with the editor as RUN-EDITOR calls it
\(the init file read, the packages loaded), and READY, a function of none,
once the editor runs and takes clients. DAEMON is closed once the editor has
ended (CLOSE-DAEMON), and told it need no longer be ended by force
(GUARD-DAEMON-LEAVING). Returns the editor."
  (let ((idle (tuatara.frontend:make-null-frontend (daemon-queue daemon)))
        (*session-name* (format nil "session-daemon-~a" (daemon-name daemon))))
    (setf *daemon* daemon)
    (pushnew 'take-in-clients *tick-functions*)
    (pushnew 'end-when-asked *tick-functions*)
    (pushnew 'end-clients-without-buffers *tick-functions*)
    (unwind-protect
         (run-editor idle
                     :setup (lambda (editor)
                              (setf (editor-idle-frontend editor) idle)
                              (when setup (funcall setup editor)))
                     :start (lambda (editor)
                              (keep-for-every-client tuatara.buffer:*buffers*)
                              (start-taking-connections daemon editor)
                              (when ready (funcall ready))))
      (setf *tick-functions*
            (remove-if (lambda (f) (member f '(take-in-clients end-when-asked end-clients-without-buffers)))
                       *tick-functions*)
            *daemon* nil)
      (close-daemon daemon)
      (when (daemon-leaving daemon) (sb-thread:signal-semaphore (daemon-leaving daemon))))))
