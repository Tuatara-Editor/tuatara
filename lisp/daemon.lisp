;;;; lisp/daemon.lisp --- One editor in the background, shown by clients in their terminals.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; tuatara --daemon runs an editor with no screen; tuatara --client shows it
;;;; in a terminal, a frame of its own painted through a remote frontend
;;;; (src/frontend/remote.lisp). The daemon listens on a local socket in a
;;;; folder of the user's alone, $XDG_RUNTIME_DIR/tuatara/NAME.socket, held
;;;; by a lock beside it for as long as it runs. A connection is taken only
;;;; from a program of the same user that sees the same files at the same
;;;; paths; it is greeted on a thread of its own, never the editor's, and
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

(defun runtime-folder-refusal (stamp what uid &key (private t))
  "Why the folder WHAT, as STAMP (not following a link) says it is, is not
safe for a daemon's socket, in words, or NIL when it is: there, a folder and
no link, UID's, and with PRIVATE for UID alone (mode 700), else at least not
writable by others."
  (cond ((null stamp) (format nil "~a is not there" what))
        ((eq (tuatara.io:stamp-kind stamp) :symlink) (format nil "~a is a link, not a folder" what))
        ((not (eq (tuatara.io:stamp-kind stamp) :directory)) (format nil "~a is not a folder" what))
        ((/= (tuatara.io:stamp-uid stamp) uid) (format nil "~a is another user's" what))
        ((if private
             (/= (logand (tuatara.io:stamp-mode stamp) #o777) #o700)
             (logtest (tuatara.io:stamp-mode stamp) #o022))
         (format nil "~a may be used by others (its mode is ~3,'0o)" what (logand (tuatara.io:stamp-mode stamp) #o777)))))

(defvar *runtime-directory* nil
  "The folder of the session's own files that daemons' sockets go in, or NIL
for the one $XDG_RUNTIME_DIR names, as the system sets it.")

(defun daemon-folder ()
  "The folder daemons' sockets are in, $XDG_RUNTIME_DIR/tuatara/ (or in
*RUNTIME-DIRECTORY*), a native name ending in /: made for you alone when it
is not there, and checked each time (RUNTIME-FOLDER-REFUSAL). Refused, saying
why, when XDG_RUNTIME_DIR is not set or the folder is not safe: there is no
other place for it."
  (let ((runtime (or *runtime-directory* (uiop:getenv "XDG_RUNTIME_DIR")))
        (uid (sb-posix:geteuid)))
    (unless (and runtime (plusp (length runtime)) (char= (char runtime 0) #\/))
      (refuse "XDG_RUNTIME_DIR is not set to a folder, and a daemon's socket goes nowhere else"))
    (let* ((runtime (tuatara.io:slash-ended runtime))
           (folder (tuatara.io:path-join runtime "tuatara/"))
           (why (runtime-folder-refusal (tuatara.io:path-stamp runtime :follow nil :failure nil)
                                        (format nil "XDG_RUNTIME_DIR (~a)" runtime) uid :private nil)))
      (when why (refuse "~a: no daemon's socket goes there" why))
      (unless (tuatara.io:path-stamp folder :follow nil :failure nil)
        (handler-case (tuatara.io:make-directory folder #o700)
          ;; made meanwhile by another start: it is checked as it is
          (tuatara.io:file-system-error () nil)))
      (let ((why (runtime-folder-refusal (tuatara.io:path-stamp folder :follow nil :failure nil) folder uid)))
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
  "A daemon running in this process: NAME, its LOCK held on its lock file (a
descriptor), the LISTENER socket it takes connections on (a descriptor); its
QUEUE, the events of its frames; the THREAD taking connections; GREETING,
how many are being greeted; ARRIVED, the remote frontends greeted and not
yet given a frame, newest first, under MUTEX; NAMESPACE, the mount namespace
it runs in."
  name lock listener
  (queue (tuatara.frontend:make-event-queue))
  (thread nil)
  (greeting 0 :type fixnum)
  (arrived '())
  (mutex (sb-thread:make-mutex :name "daemon"))
  (namespace (tuatara.io:mount-namespace (sb-posix:getpid))))

(setf (documentation 'daemon-p 'function) "True when OBJECT is a DAEMON."
      (documentation 'daemon-name 'function) "The name of DAEMON."
      (documentation 'daemon-lock 'function) "The descriptor of DAEMON's lock file, its lock held."
      (documentation 'daemon-listener 'function) "The socket DAEMON takes connections on."
      (documentation 'daemon-queue 'function) "Where the events of DAEMON's frames go."
      (documentation 'daemon-thread 'function) "The thread taking DAEMON's connections."
      (documentation 'daemon-greeting 'function) "How many connections DAEMON is greeting."
      (documentation 'daemon-arrived 'function) "The remote frontends greeted and not yet given a frame, newest first."
      (documentation 'daemon-mutex 'function) "Held while DAEMON's GREETING or ARRIVED change."
      (documentation 'daemon-namespace 'function)
      "The mount namespace DAEMON runs in, as TUATARA.IO:MOUNT-NAMESPACE gives it.")

(defvar *daemon* nil
  "The daemon this editor runs as, in the background (DAEMON), or NIL.")

(defun open-daemon (name)
  "Make this process the daemon NAME: take its lock, so that no other daemon
of the name runs, take away a socket a daemon that ended left, and listen on
a new one. Refused, saying why, when one of the name runs or the folder is
not safe (DAEMON-FOLDER). Returns the DAEMON."
  (let* ((lock-path (daemon-path name "lock"))
         (socket (daemon-path name "socket"))
         (fd (handler-case (sb-posix:open lock-path (logior sb-posix:o-rdwr sb-posix:o-creat tuatara.io::+o-nofollow+
                                                             tuatara.io::+o-cloexec+)
                                          #o600)
               (sb-posix:syscall-error (e)
                 (refuse "~a cannot be opened: ~a" lock-path (tuatara.io::errno-reason (sb-posix:syscall-errno e)))))))
    (unless (tuatara.runtime:lock-fd fd)
      (sb-posix:close fd)
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
  (dolist (remote (sb-thread:with-mutex ((daemon-mutex daemon)) (shiftf (daemon-arrived daemon) '())))
    (ignoring-errors-reported ("letting a client go") (tuatara.frontend:frontend-stop remote))))

;;; Taking connections

(defparameter *most-greeting* 8
  "The most connections a daemon greets at once; one more is refused.")

(defparameter *most-clients* 64
  "The most clients a daemon is shown by at once; one more is refused.")

(defun peer-refusal (uid namespace &key (own-uid (sb-posix:geteuid)) own-namespace)
  "Why a program at the other end of a connection, of the user UID in the
mount namespace NAMESPACE, may not be a client of a daemon run by OWN-UID in
OWN-NAMESPACE, in words, or NIL when it may: it must be of the same user, and
see the same files at the same paths."
  (cond ((not (eql uid own-uid))
         "This daemon is another user's: it takes clients of the user who started it alone")
        ((or (null namespace) (not (equal namespace own-namespace)))
         (format nil "The daemon runs outside this client's container (another mount namespace), ~
                      where paths name other files: run tuatara --client where the daemon runs, ~
                      or a daemon in the container"))))

(defun live-client-count (editor)
  "How many clients show EDITOR now."
  (count-if (lambda (client) (and (client-frontend client) (not (client-ended client)))) (editor-clients editor)))

(defun take-connection (daemon editor fd)
  "Take the new connection FD to DAEMON, whose editor is EDITOR: refused,
saying why, when its peer may not be a client (PEER-REFUSAL), when as many
are greeted as may be at once (*MOST-GREETING*), or as many clients show
the editor (*MOST-CLIENTS*); else greeted on a thread of its own (GREET)."
  (multiple-value-bind (pid uid) (tuatara.io:peer-credentials fd)
    (let ((why (cond ((null pid) "The daemon could not tell who connected")
                     ((peer-refusal uid (tuatara.io:mount-namespace pid) :own-namespace (daemon-namespace daemon)))
                     ((>= (daemon-greeting daemon) *most-greeting*) "Too many clients are attaching at once: try again")
                     ((>= (live-client-count editor) *most-clients*)
                      (format nil "The daemon is shown by ~d clients already, the most it takes" *most-clients*)))))
      (if why
          (progn (tuatara.runtime:debug-report :daemon "a connection refused (process ~a, user ~a): ~a" pid uid why)
                 (tuatara.frontend:refuse-connection fd why))
          (progn (sb-thread:with-mutex ((daemon-mutex daemon)) (incf (daemon-greeting daemon)))
                 (tuatara.runtime:make-guarded-thread (lambda () (greet daemon fd)) :name "tuatara daemon greeting"))))))

(defun greet (daemon fd)
  "Greet the new connection FD to DAEMON (TUATARA.FRONTEND:MAKE-REMOTE-FRONTEND)
and hand its frontend to the editor, woken for it; a client refused or that
says nothing it should has its connection closed."
  (unwind-protect
       (handler-case
           (let ((remote (tuatara.frontend:make-remote-frontend fd :queue (daemon-queue daemon))))
             (sb-thread:with-mutex ((daemon-mutex daemon))
               (push remote (daemon-arrived daemon)))
             (tuatara.frontend:queue-push (daemon-queue daemon) (tuatara.frontend:make-event :tick)))
         (tuatara.frontend:remote-refused (condition)
           (tuatara.runtime:debug-report :daemon "a client refused: ~a" (tuatara.frontend:remote-refused-words condition))
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

(defun kept-client-environment (environment)
  "Those of ENVIRONMENT, a client's variables as (NAME . VALUE), the editor
keeps for it (TUATARA.COMMAND:CLIENT-VARIABLE-P)."
  (remove-if-not (lambda (entry) (client-variable-p (car entry))) environment))

(defun client-scratch (client)
  "A scratch buffer for CLIENT: the one shown at the start (STARTUP-BUFFER)
while it is no client's, else a new one, its name numbered; it stands in
CLIENT's folder and is CLIENT's."
  (let* ((first (startup-buffer))
         (buffer (if (buffer-clients first)
                     (let ((entry (assoc (tuatara.buffer:buffer-name first) *scratch-buffers* :test #'string=)))
                       (if entry
                           (make-scratch-buffer (tuatara.buffer:unique-buffer-name (first entry))
                                                (second entry) (third entry))
                           (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*scratch*"))))
                     first)))
    (when (client-directory client)
      (setf (buffer-given-directory buffer) (client-directory client)))
    (join-client buffer client)
    buffer))

(defun attach-client (editor remote)
  "Give EDITOR a client on REMOTE, a remote frontend greeted, with a frame
showing a scratch buffer of its own (CLIENT-SCRATCH, ADD-CLIENT). What the
client asked for (TUATARA.FRONTEND:REMOTE-REQUEST) is done in its frame as
soon as the editor is free (START-CLIENT)."
  (let* ((request (tuatara.frontend:remote-request remote))
         (client nil))
    (setf client (add-client editor remote
                             :directory (tuatara.io:slash-ended (tuatara.frontend:request-directory request))
                             :environment (kept-client-environment (tuatara.frontend:request-environment request))
                             :buffer #'client-scratch
                             :start (lambda () (start-client editor client request))))
    (tuatara.runtime:debug-report :daemon "client ~d attached in ~a~@[ (~a)~]" (client-number client)
                                  (client-directory client) (tuatara.frontend:request-terminal request))
    client))

(defun take-in-clients (editor)
  "Tick function of a daemon (*TICK-FUNCTIONS*): give each client greeted
since a frame of EDITOR (ATTACH-CLIENT). Wants no call of its own: a client
greeted wakes the editor."
  (let ((daemon *daemon*))
    (when (and daemon (daemon-arrived daemon))
      (dolist (remote (reverse (sb-thread:with-mutex ((daemon-mutex daemon)) (shiftf (daemon-arrived daemon) '()))))
        (attach-client editor remote))))
  nil)

(defun start-client (editor client request)
  "Do what CLIENT of EDITOR asked for as it attached, REQUEST, in its frame:
open its files (standard input among them), each where its +LINE asks, the
first shown; or compare or merge files; open the frames asked for; or stop
the editor. A file that cannot be opened is said, as when the editor starts
with it."
  (let* ((merge (tuatara.frontend:request-merge request))
         (entries (if merge
                      (list (fourth merge))
                      (loop for (path) in (tuatara.frontend:request-files request)
                            collect (if (or (tuatara.frontend:request-safe request) (tuatara.frontend:request-hex request))
                                        (list path :safe (tuatara.frontend:request-safe request)
                                                   :hex (tuatara.frontend:request-hex request))
                                        path))))
         (input (tuatara.frontend:request-standard-input request)))
    (when input
      (setf entries (append entries (list (list :standard-input input)))))
    (when (tuatara.frontend:request-stop request)
      (return-from start-client (stop-from-client editor client request)))
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
opened them; one with changes not saved is kept, as its text would be lost,
and that is said."
  (dolist (buffer (client-encrypted-buffers client))
    (if (tuatara.buffer:buffer-modified-p buffer)
        (message "~a, encrypted, has changes not saved: kept, though its client ended"
                 (tuatara.buffer:buffer-name buffer))
        (tuatara.buffer:kill-buffer buffer))))

(pushnew 'close-encrypted-with-client *client-end-functions*)

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

(defun stop-daemon (editor &key (words "The daemon was stopped (M-x daemon-quit)"))
  "Stop EDITOR, a daemon's: every client but the one working in the frame is
told the daemon ends, with WORDS, and exits 1; that one exits 0; then the
editor ends, writing what it keeps as an editor does."
  (let ((here (current-client editor)))
    (dolist (client (editor-clients editor))
      (unless (or (eq client here) (client-ended client) (null (client-frontend client)))
        (setf (client-exit-code client) 1 (client-exit-words client) words)
        (end-client editor client)))
    (setf (editor-running editor) nil)))

(define-command daemon-quit ()
  "Stop the editor running in the background (tuatara --daemon), from any of
its clients: asked first about every change not saved in it, the clients
that ended included, folder by folder: s saves those of a folder, l leaves
them unsaved, \\`C-g' keeps the daemon running. Then every other client is
told the daemon ends, gives its terminal back and exits 1, and the daemon
writes what it keeps (the files visited lately, the places) and ends.
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
        (dolist (answer answers) (when (functionp answer) (call-hook-function answer)))
        (stop-daemon editor)))))

(defun auto-save-unsaved ()
  "Write the changes not saved of the editor's buffers as auto-saved copies, for
\\[recover-file] and \\[list-auto-saves] to offer on the next visit: each
that may be auto-saved (AUTO-SAVE-WANTED-P, auto-saving on or not; never an
encrypted file's text). Said in *Messages*, and so in the log."
  (let ((*auto-save* t) (written 0))
    (dolist (buffer tuatara.buffer:*buffers*)
      (when (and (tuatara.buffer:buffer-modified-p buffer) (auto-save-wanted-p buffer))
        (ignoring-errors-reported ("auto-saving ~a" (tuatara.buffer:buffer-name buffer))
          (auto-save-now buffer)
          (incf written))))
    (message "The daemon stopped with no one to ask: ~d buffer~:p with changes auto-saved" written)))

(defun stop-from-client (editor client request)
  "Stop EDITOR as CLIENT asked as it attached (tuatara --stop), REQUEST
saying how: asked in its terminal as \\[daemon-quit] asks; with no terminal,
where nothing can be asked, the changes not saved written as auto-saved copies
first (AUTO-SAVE-UNSAVED). A question given up keeps the daemon running and
closes CLIENT, exiting 1."
  (if (null (tuatara.frontend:request-terminal request))
      (progn (auto-save-unsaved)
             (stop-daemon editor :words "The daemon was stopped (tuatara --stop)"))
      (let ((stopped nil))
        (unwind-protect
             (progn (run-command editor 'daemon-quit)
                    (setf stopped (not (editor-running editor))))
          (unless stopped
            (setf (client-exit-code client) 1
                  (client-exit-words client) "The daemon goes on: nothing was stopped")
            (end-client editor client))))))

;;; Running a daemon's editor

(defun run-daemon-editor (daemon &key (setup #'install-defaults) ready)
  "Run DAEMON's editor on this thread until it stops: an editor whose frame
is on a frontend with no screen while no client shows it, its clients given
frames as they attach (START-TAKING-CONNECTIONS, TAKE-IN-CLIENTS). SETUP is
called with the editor as RUN-EDITOR calls it (the init file read, the
packages loaded), and READY, a function of none, once the editor runs and
takes clients. DAEMON is closed once the editor has ended (CLOSE-DAEMON).
Returns the editor."
  (let ((idle (tuatara.frontend:make-null-frontend (daemon-queue daemon))))
    (setf *daemon* daemon)
    (pushnew 'take-in-clients *tick-functions*)
    (unwind-protect
         (run-editor idle
                     :setup (lambda (editor)
                              (setf (editor-idle-frontend editor) idle)
                              (when setup (funcall setup editor)))
                     :start (lambda (editor)
                              (start-taking-connections daemon editor)
                              (when ready (funcall ready))))
      (setf *tick-functions* (remove 'take-in-clients *tick-functions*)
            *daemon* nil)
      (close-daemon daemon))))
