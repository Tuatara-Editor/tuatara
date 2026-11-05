;;;; test/daemon.lisp --- An editor in the background and its clients, over real local sockets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test daemon
  :description "tuatara --daemon runs an editor with no screen, listening in a folder of the user's alone;
tuatara --client shows it in a terminal: a frame of its own, buffers of its own, C-x C-c closing it alone with
the exit code its files left; the daemon takes no other user's or container's program, one of a name runs at
once, and M-x daemon-quit or tuatara --stop stops it.")

;;; A daemon in this process, and clients of it on fake screens

(defvar *test-daemons* 0
  "How many daemons the tests have started: each is named for its number.")

(defun start-test-daemon (&optional (name (format nil "test~d" (incf *test-daemons*))))
  "Start a daemon in this process, NAME, by default one named for the tests,
its editor on a thread of its own, once it takes clients; three values: the
daemon, the thread, and its name."
  (let* ((daemon (tuatara.defaults::open-daemon name))
         (ready (sb-thread:make-semaphore :name "test daemon ready"))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (call-without-grammar-questions
                     (lambda ()
                       (tuatara.defaults::run-daemon-editor daemon :ready (lambda () (sb-thread:signal-semaphore ready))))))
                  :name "test daemon")))
    (unless (sb-thread:wait-on-semaphore ready :timeout 60)
      (error "The test daemon did not start"))
    (values daemon thread name)))

(defun daemon-editor ()
  "The editor of the test daemon running now."
  (tuatara.command:running-editor))

(defun in-daemon (function)
  "Call FUNCTION on this thread while the test daemon's editor waits, and
return what it returns: to look at the editor's state, or change it."
  (tuatara.command:call-with-editor-held function :words "A test"))

(defun stop-test-daemon (thread)
  "Stop the test daemon whose editor runs on THREAD, if it runs still, and
wait for it; true when it ended."
  (let ((editor (daemon-editor)))
    (when (and editor (sb-thread:thread-alive-p thread))
      (in-daemon (lambda () (setf (tuatara.command:editor-running editor) nil)))))
  (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))

(defmacro with-test-daemon ((name &optional (thread (gensym "THREAD")) (daemon (gensym "DAEMON")) named)
                            &body body)
  "Run BODY with NAME bound to the name of a daemon started in this process
\(START-TEST-DAEMON), NAMED when given, THREAD to the thread its editor runs
on and DAEMON to the daemon, stopped after."
  `(multiple-value-bind (,daemon ,thread ,name) ,(if named `(start-test-daemon ,named) '(start-test-daemon))
     (declare (ignorable ,daemon ,name))
     (unwind-protect (progn ,@body)
       (stop-test-daemon ,thread))))

(defun attach-test-client (name &key (directory "/tmp/") files safe merge diff stop terminal environment standard-input
                                     (width 70) (height 14) window joins another (frames 1) far)
  "A client of the daemon NAME on a fake screen of WIDTH by HEIGHT, a window
when WINDOW, asking for what a client started so asks for (FILES, each a
path or (PATH LINE COLUMN), SAFE, MERGE, DIFF, STOP, TERMINAL, ENVIRONMENT,
STANDARD-INPUT, a descriptor passed, JOINS, the word of a window the daemon
asked for, FRAMES, FAR as a client on another machine), ANOTHER opening a
window the daemon asks for; three values: the
fake screen, the thread running the client, which returns its exit code and
words as a list, and the client."
  (let ((local (make-instance 'queued-fake-frontend :width width :height height :font-steps (and window 0)))
        (fd (tuatara.io:connect-to (tuatara.defaults::daemon-path name "socket"))))
    (tuatara.frontend:frontend-start local)
    (let ((client (tuatara.frontend:make-remote-client
                   fd local
                   :request (tuatara.frontend:make-request
                             :directory (namestring directory) :terminal terminal :environment environment
                             :files (mapcar (lambda (file) (if (consp file) file (list (namestring file) nil nil))) files)
                             :safe safe :merge merge :diff diff :stop stop :wait t
                             :standard-input (and standard-input t) :joins joins :frames frames :far far)
                   :standard-input standard-input :another another)))
      (values local
              (sb-thread:make-thread (lambda () (multiple-value-list (tuatara.frontend:run-remote-client client)))
                                     :name "test client")
              client))))

(defun type-keys (local keys)
  "Type KEYS, a key sequence as a string (\"C-x C-c\"), at LOCAL, a client's fake screen."
  (dolist (spec (tuatara.frontend:parse-key-sequence keys))
    (type-at local (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec)))))

(defun showing-now (local text)
  "True when the last frame LOCAL, a fake screen, painted shows TEXT on a row."
  (let ((screen (tuatara.frontend.fake:fake-screen local)))
    (and screen (some (lambda (row) (search text row)) screen) t)))

(defun shows (local text &optional (seconds 20))
  "Wait until LOCAL, a client's fake screen, shows TEXT in its last frame, at
most SECONDS; true when it does."
  (wait-until (lambda () (showing-now local text)) seconds))

(defun client-ends (runner &optional (seconds 20))
  "What the client thread RUNNER returned, its exit code and words as a
list, waiting SECONDS at most; :LATE when it has not ended."
  (sb-thread:join-thread runner :timeout seconds :default :late))

(defun window-opener (name &key (after 0))
  "Two values: a function of a client and a word that opens another window of
a test client of the daemon NAME, attached with that word AFTER seconds, as
a client in a window does when the daemon asks it for one; and a cell whose
car is the list of the windows it opened, newest first, each (FAKE-SCREEN
RUNNER CLIENT) as ATTACH-TEST-CLIENT gives them."
  (let ((opened (list '())))
    (values (lambda (client word)
              (declare (ignore client))
              (flet ((open-it ()
                       (multiple-value-bind (local runner window) (attach-test-client name :window t :joins word)
                         (push (list local runner window) (car opened)))))
                (if (plusp after)
                    (sb-thread:make-thread (lambda () (sleep after) (open-it)) :name "test window opener")
                    (open-it))))
            opened)))

(defun live-client-frames ()
  "How many frames of the test daemon's editor its clients that have not
ended have: the frames shown, the one with no screen left out."
  (in-daemon (lambda ()
               (let ((editor (daemon-editor)))
                 (count-if (lambda (frame)
                             (let ((client (tuatara.command:frame-client frame)))
                               (and client (tuatara.command:client-remote client)
                                    (not (tuatara.command:client-ended client))
                                    (not (tuatara.command:idle-frame-p editor frame)))))
                           (tuatara.command:editor-frames editor))))))

;;; The folder, the lock and who may connect

(define-test (daemon folder-refusals)
  ;; the folder of sockets must be a folder, no link, the user's, and for the user alone
  (let ((uid (sb-posix:geteuid)))
    (flet ((refusal (mode &optional (owner uid) (private t))
             (tuatara.defaults::runtime-folder-refusal (tuatara.io:make-stamp 1 2 0 0 1 mode owner 0) "F" uid
                                                       :private private)))
      (false (refusal #o40700))
      (true (search "may be used by others" (refusal #o40755)))
      (true (search "may be used by others" (refusal #o40710)))
      (true (search "a link" (refusal #o120777)))
      (true (search "not a folder" (refusal #o100600)))
      (true (search "another user's" (refusal #o40700 (1+ uid))))
      ;; XDG_RUNTIME_DIR itself need only be closed to others' writing
      (false (refusal #o40755 uid nil))
      (true (refusal #o40777 uid nil))
      (true (search "not there" (tuatara.defaults::runtime-folder-refusal nil "F" uid)))))
  ;; with no XDG_RUNTIME_DIR, or one others may write in, nowhere: no fallback to /tmp
  (let ((tuatara.defaults::*runtime-directory* nil))
    (with-environment-variable ("XDG_RUNTIME_DIR" nil)
      (fail (tuatara.defaults::daemon-folder) 'tuatara.runtime:refusal)
      (sb-posix:setenv "XDG_RUNTIME_DIR" "/tmp" 1)
      (fail (tuatara.defaults::daemon-folder) 'tuatara.runtime:refusal)))
  ;; the tests' own folder, made for them alone
  (let ((folder (tuatara.defaults::daemon-folder)))
    (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp folder :follow nil))))
    (true (search "/runtime/tuatara/" folder))))

(define-test (daemon folders-on-the-way)
  ;; a folder on the way to one kept safe is root's or the user's, written in by others only with the sticky bit
  (let ((uid (sb-posix:geteuid)))
    (flet ((refusal (mode owner)
             (tuatara.defaults::runtime-folder-refusal (tuatara.io:make-stamp 1 2 0 0 1 mode owner 0) "F" uid
                                                       :on-the-way t)))
      (false (refusal #o40755 0))
      (false (refusal #o40755 uid))
      (false (refusal #o41777 0))
      (false (refusal #o41777 uid))
      (true (search "another user's" (refusal #o40755 (1+ uid))))
      (true (search "another user's" (refusal #o41777 (1+ uid))))
      (true (search "may be used by others" (refusal #o40775 uid)))
      (true (search "may be used by others" (refusal #o40777 0)))
      ;; in a rootless container, the system's /tmp shows as owned by the overflow id, which is taken when sticky
      (let ((outside (tuatara.defaults::outside-owner)))
        (when outside
          (false (refusal #o41777 outside))
          (true (search "another user's" (refusal #o40755 outside)))))))
  ;; the folder of sockets is refused when a folder on the way to it is open to others
  (with-test-directory (dir)
    (let ((open (namestring (merge-pathnames "open/runtime/" dir))))
      (ensure-directories-exist open)
      (sb-posix:chmod open #o700)
      (sb-posix:chmod (namestring (merge-pathnames "open/" dir)) #o777)
      (let ((tuatara.defaults::*runtime-directory* open))
        (fail (tuatara.defaults::daemon-folder) 'tuatara.runtime:refusal)))))

(define-test (daemon peer-refusals)
  ;; another user's program, or one in another mount namespace, is refused; the same user's in the same one taken
  (let ((uid (sb-posix:geteuid)) (here (tuatara.io:mount-namespace (sb-posix:getpid))))
    (true here)
    (false (tuatara.defaults::peer-refusal uid here :own-namespace here))
    (true (search "another user's" (tuatara.defaults::peer-refusal (1+ uid) here :own-namespace here)))
    (true (search "container" (tuatara.defaults::peer-refusal uid (cons 1 2) :own-namespace here)))
    (true (tuatara.defaults::peer-refusal uid nil :own-namespace here))))

(define-test (daemon one-of-a-name)
  ;; a second daemon of a name is refused while the first holds its lock; a socket left behind is taken away
  (let ((first (tuatara.defaults::open-daemon "lock-test")))
    (unwind-protect
         (let ((refused (handler-case (progn (tuatara.defaults::close-daemon (tuatara.defaults::open-daemon "lock-test")) nil)
                          (tuatara.runtime:refusal (condition) (princ-to-string condition)))))
           (true (search "runs already" refused)))
      (tuatara.defaults::close-daemon first)))
  ;; a socket a daemon left when it ended without taking it away: no one holds its lock, so it goes
  (let* ((path (tuatara.defaults::daemon-path "stale-test" "socket"))
         (left (tuatara.io:listen-at path)))
    (sb-posix:close left)
    (is eq :socket (tuatara.io:path-kind path :follow nil))
    (tuatara.defaults::close-daemon (tuatara.defaults::open-daemon "stale-test"))
    (false (tuatara.io:path-kind path :follow nil) "taken away as the daemon closed"))
  ;; something else at the socket's path is left as it is, and said
  (let ((path (tuatara.defaults::daemon-path "file-test" "socket")))
    (alexandria:write-string-into-file "mine" path)
    (unwind-protect
         (fail (tuatara.defaults::open-daemon "file-test") 'tuatara.runtime:refusal)
      (delete-file path)))
  ;; names that are not names
  (dolist (name '("" "a/b" "../x" "with space" "ā"))
    (false (tuatara.defaults::daemon-name-p name) "~s taken for a name" name)))

(define-test (daemon refusal-said-to-the-client)
  ;; a client refused before its hello is told why, in words it shows; one of another build is told to restart
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (tuatara.frontend:refuse-connection mine "This daemon is another user's")
    (let ((local (make-instance 'queued-fake-frontend)))
      (tuatara.frontend:frontend-start local)
      (let ((words (handler-case (progn (tuatara.frontend:make-remote-client theirs local) nil)
                     (tuatara.frontend:remote-refused (condition) (tuatara.frontend:remote-refused-words condition)))))
        (is string= "This daemon is another user's" words)))
    (sb-posix:close theirs))
  (let ((words (tuatara.terminal::refusal-words
                (make-condition 'tuatara.frontend:remote-refused :words "x" :other "Tuatara 0.0.1") "work")))
    (true (search "the daemon work runs Tuatara 0.0.1" words))
    (true (search "tuatara --stop=work" words))))

;;; Clients

(define-test (daemon client-opens-files-and-leaves-them)
  ;; a client shows its file where +LINE asks; C-x C-c closes it alone, with 0, and its buffer stays
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "kiwi.txt" dir)
                                      (format nil "~{line ~d~%~}" (loop for n from 1 to 40 collect n))))))
      (with-test-daemon (name thread)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list (list file 30 nil)))
          (true (shows local "line 30"))
          (true (shows local "kiwi.txt"))
          (type-keys local "C-x C-c")
          (is equal '(0 nil) (client-ends runner))
          (true (sb-thread:thread-alive-p thread) "the daemon goes on")
          (in-daemon (lambda ()
                       (let ((buffer (tuatara.defaults::buffer-visiting file))
                             (editor (daemon-editor)))
                         (true buffer "its buffer stays")
                         (is = 1 (length (tuatara.command:editor-frames editor)))
                         (true (tuatara.command:idle-frame-p editor (tuatara.command:editor-frame editor))
                               "the frame with no screen is back")
                         (let ((ended (find-if #'tuatara.command:client-ended
                                               (tuatara.command:buffer-clients buffer))))
                           (true ended)
                           ;; of an ended client only what names its folder is kept
                           (false (tuatara.command:client-frontend ended))
                           (false (tuatara.command:client-environment ended)))))))))))

(define-test (daemon buffers-kept-by-client)
  ;; each client is offered its own buffers; one that ended leaves its folder in M-x buffers, to be taken up
  (with-test-directory (dir)
    (let ((alpha (namestring (put-file (merge-pathnames "alpha.txt" dir) "alpha text")))
          (beta (namestring (put-file (merge-pathnames "beta.txt" dir) "beta text"))))
      (with-test-daemon (name)
        (multiple-value-bind (a a-runner) (attach-test-client name :directory dir :files (list alpha))
          (true (shows a "alpha text"))
          (multiple-value-bind (b b-runner) (attach-test-client name :directory dir :files (list beta))
            (true (shows b "beta text"))
            ;; C-x b in B offers B's own, not A's
            (type-keys b "C-x b")
            (true (shows b "Switch to buffer"))
            (false (tuatara.frontend.fake:fake-frame-showing b "alpha.txt") "A's buffer is not offered to B")
            (type-keys b "C-g")
            (type-keys a "C-x C-c")
            (is equal '(0 nil) (client-ends a-runner))
            ;; B's list of buffers: A's folder, ended, holding A's file
            (type-keys b "C-x C-b")
            (true (shows b "Ended"))
            (true (shows b "client 1"))
            (type-keys b "C-x C-c")
            (is equal '(0 nil) (client-ends b-runner))))))))

(define-test (daemon a-client-gone)
  ;; a client whose connection drops is one that ended: its frames close, the daemon and its buffers go on
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "gone.txt" dir) "still here"))))
      (with-test-daemon (name thread)
        (multiple-value-bind (local runner client) (attach-test-client name :directory dir :files (list file))
          (true (shows local "still here"))
          (tuatara.io:shut-socket (tuatara.frontend::remote-client-fd client))
          (is equal '(nil nil) (client-ends runner))
          (true (wait-until (lambda ()
                              (in-daemon (lambda () (= 1 (length (tuatara.command:editor-frames (daemon-editor)))))))
                            10))
          (true (sb-thread:thread-alive-p thread))
          (true (in-daemon (lambda () (tuatara.defaults::buffer-visiting file)))))))))

(define-test (daemon client-waits-for-a-slow-init-file)
  ;; a client that connects while the daemon still reads its init file is shown once it is read, not refused
  (with-test-directory (dir)
    (let* ((init (put-file (merge-pathnames "init.lisp" dir) "(sleep 3)"))
           (name (format nil "test~d" (incf *test-daemons*)))
           (daemon (tuatara.defaults::open-daemon name))
           (thread (sb-thread:make-thread
                    (lambda ()
                      (let ((tuatara.defaults:*config-file* init))
                        (call-without-grammar-questions
                         (lambda () (tuatara.defaults::run-daemon-editor daemon)))))
                    :name "test daemon"))
           (said 0))
      (unwind-protect
           (let ((tuatara.frontend::*hello-seconds* 1))
             (multiple-value-bind (local runner)
                 (let ((fd (tuatara.io:connect-to (tuatara.defaults::daemon-path name "socket")))
                       (local (make-instance 'queued-fake-frontend :width 70 :height 14)))
                   (tuatara.frontend:frontend-start local)
                   (let* ((greeted (tuatara.frontend:greet-editor fd :starting (lambda () (incf said))))
                          (client (tuatara.frontend:make-remote-client
                                   fd local :request (tuatara.frontend:make-request :directory "/tmp/" :wait t)
                                            :greeted greeted)))
                     (values local (sb-thread:make-thread
                                    (lambda () (multiple-value-list (tuatara.frontend:run-remote-client client)))
                                    :name "test client"))))
               (true (shows local "scratch"))
               (is = 1 said "the wait said once")
               (type-keys local "C-x C-c")
               (is equal '(0 nil) (client-ends runner))))
        (wait-until #'daemon-editor 20)
        (stop-test-daemon thread)))))

(define-test (daemon git-editor-exit-codes)
  ;; tuatara --client as git's editor: C-c C-c saves the message and ends the client with 0, C-c C-k with 1
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "COMMIT_EDITMSG" dir))))
      (with-test-daemon (name thread)
        (dolist (case '(("F i r s t C-c C-c" 0 "First") ("N o p e C-c C-k" 1 "")))
          (destructuring-bind (keys code first) case
            (alexandria:write-string-into-file (format nil "~%# Please enter the commit message.~%") file
                                               :if-exists :supersede)
            (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
              (true (shows local "Please enter"))
              (type-keys local keys)
              (is eql code (first (client-ends runner)))
              (is string= (format nil "~a~%# Please enter the commit message.~%" first) (file-text file))
              (in-daemon (lambda () (let ((buffer (tuatara.defaults::buffer-visiting file)))
                                      (when buffer (tuatara.buffer:kill-buffer buffer))))))))
        (true (sb-thread:thread-alive-p thread) "the daemon goes on")))))

(define-test (daemon merge-exit-code)
  ;; --merge through a client: 0 once MERGED is saved with no conflict left
  (with-test-directory (dir)
    (flet ((file (name text) (namestring (put-file (merge-pathnames name dir) text))))
      (let ((base (file "base.txt" (format nil "one~%two~%three~%")))
            (local-file (file "local.txt" (format nil "one~%TWO~%three~%")))
            (remote (file "remote.txt" (format nil "one~%2~%three~%")))
            (merged (file "merged.txt" (format nil "one~%two~%three~%"))))
        (with-test-daemon (name)
          (multiple-value-bind (local runner)
              (attach-test-client name :directory dir :merge (list local-file base remote merged))
            (true (shows local "<<<<<<< local"))
            (type-keys local "C-c ^ n C-c ^ l C-x C-s")
            (true (wait-until (lambda () (string= (format nil "one~%2~%three~%") (file-text merged))) 10))
            (type-keys local "C-x C-c")
            (is eql 0 (first (client-ends runner)))))))))

(define-test (daemon standard-input-read)
  ;; - through a client: what is piped to it is read by the daemon into *stdin*
  (with-test-directory (dir)
    (with-test-daemon (name)
      (multiple-value-bind (in out) (sb-posix:pipe)
        (let ((octets (tuatara.text:string-octets (format nil "piped to the client~%"))))
          (tuatara.io:with-octets-sap (sap octets) (sb-posix:write out sap (length octets))))
        (sb-posix:close out)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :standard-input in)
          (true (shows local "piped to the client"))
          (true (shows local "*stdin*"))
          (type-keys local "C-x C-c")
          (is eql 0 (first (client-ends runner))))))))

(define-test (daemon encrypted-close-with-client)
  ;; a client's encrypted buffers close with it; one with changes is asked about, C-g keeping the client
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "secret.txt" dir) "the secret"))))
      (with-test-daemon (name)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
          (true (shows local "the secret"))
          (let ((buffer (in-daemon (lambda ()
                                     (let ((buffer (tuatara.defaults::buffer-visiting file)))
                                       (setf (tuatara.buffer:buffer-local buffer :encrypted) (list :symmetric t))
                                       buffer)))))
            (type-keys local "i x")
            (true (wait-until (lambda () (tuatara.buffer:buffer-modified-p buffer)) 10))
            (type-keys local "C-x C-c")
            (true (shows local "is encrypted and has changes not saved"))
            (type-keys local "C-g")
            (true (wait-until (lambda () (not (showing-now local "is encrypted and"))) 10))
            (is eq :late (client-ends runner 1/2) "kept by C-g")
            (true (tuatara.command:live-buffer-p buffer))
            (type-keys local "C-x C-c l")
            (is eql 0 (first (client-ends runner)))
            (true (wait-until (lambda () (not (tuatara.command:live-buffer-p buffer))) 10)
                  "closed with its client")))))))

(define-test (daemon stopped-from-a-client)
  ;; M-x daemon-quit asks about the changes not saved, folder by folder; the other client exits 1, told why
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "unsaved.txt" dir) "unsaved"))))
      (with-test-daemon (name thread)
        (multiple-value-bind (a a-runner) (attach-test-client name :directory dir :files (list file))
          (multiple-value-bind (b b-runner) (attach-test-client name :directory dir)
            (true (shows a "unsaved"))
            (type-keys a "i x")
            (true (shows a "xunsaved"))
            (true (shows b "scratch"))
            (type-keys b "M-x d a e m o n - q u i t RET")
            (true (shows b "changes not saved"))
            (type-keys b "l")
            (is equal '(0 nil) (client-ends b-runner))
            (let ((other (client-ends a-runner)))
              (is eql 1 (first other))
              (true (search "stopped" (second other))))
            (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))) "the daemon ended")))))))

(define-test (daemon stopped-with-no-terminal)
  ;; tuatara --stop with no terminal asks nothing: with auto-saving off, changes would be lost, so it stops nothing
  ;; and says which buffers hold them; with it on, the changes are kept as auto-saved copies and the daemon ends
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "kept.txt" dir) "kept")))
          (was tuatara-user:*auto-save*))
      (unwind-protect
           (with-test-daemon (name thread)
             (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
               (true (shows local "kept"))
               (type-keys local "i y")
               (true (shows local "ykept"))
               (setf tuatara-user:*auto-save* nil)
               (let ((said (with-output-to-string (*error-output*)
                             (is eql 1 (tuatara.terminal::run-client (list :stop name :files '()) :terminal nil)))))
                 (true (search "kept.txt" said))
                 (true (search "goes on" said)))
               (true (sb-thread:thread-alive-p thread) "not stopped")
               (setf tuatara-user:*auto-save* t)
               (is eql 0 (tuatara.terminal::run-client (list :stop name :files '()) :terminal nil))
               (is eql 1 (first (client-ends runner)))
               (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))
               (let ((copy (tuatara.defaults::auto-save-path file)))
                 (unwind-protect (is string= "ykept" (file-text copy))
                   (ignore-errors (delete-file copy))))))
        (setf tuatara-user:*auto-save* was)))))

(define-test (daemon stop-closes-standard-input)
  ;; a client asking only to stop has its standard input closed, never read
  (with-test-directory (dir)
    (with-test-daemon (name thread)
      (multiple-value-bind (in out) (sb-posix:pipe)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :stop t :standard-input out)
          (declare (ignore local))
          (is eql 0 (first (client-ends runner)))
          (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))
          ;; the client closed its own once passed: the daemon's is the only one there was
          (true (pipe-ended-p in) "the daemon's copy of the descriptor is closed"))))))

(define-test (daemon waits-for-a-client-to-ask)
  ;; with no client, what asks waits for one, its frame then; work that asks nothing runs meanwhile
  (with-test-daemon (name)
    (let ((ran nil))
      (tuatara.command:defer-action (lambda () (setf ran (tuatara.command:frame-client
                                                           (tuatara.command:editor-frame (daemon-editor))))))
      (is eq t (in-daemon (constantly t)) "a hold runs with no client")
      (false ran)
      (multiple-value-bind (local runner) (attach-test-client name)
        (true (wait-until (lambda () ran) 10))
        (true (tuatara.command:client-remote ran) "done in the client's frame")
        (type-keys local "C-x C-c")
        (client-ends runner)))))

(defclass lending-fake-frontend (queued-fake-frontend)
  ((lent :initform '() :accessor lending-lent
         :documentation "The words shown each time the terminal was lent, newest first."))
  (:documentation "A client's fake screen that notes the terminal lent from it."))

(defmethod tuatara.frontend:frontend-lend-terminal ((f lending-fake-frontend) function &key words)
  (push words (lending-lent f))
  (funcall function))

(define-test (daemon lends-a-clients-terminal)
  ;; a program is lent a client's terminal: the client gives it back, shows the words, and takes it again
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let* ((local (make-instance 'lending-fake-frontend))
           (maker (progn (tuatara.frontend:frontend-start local)
                         (sb-thread:make-thread
                          (lambda () (tuatara.frontend:make-remote-client
                                      theirs local :request (tuatara.frontend:make-request :terminal "/dev/pts/99"))))))
           (remote (tuatara.frontend:make-remote-frontend mine))
           (client (sb-thread:join-thread maker))
           (runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client))))
           (inside nil))
      (is string= "/dev/pts/99" (tuatara.frontend:frontend-terminal-device remote))
      (is eq :done (tuatara.frontend:frontend-lend-terminal remote (lambda () (setf inside (lending-lent local)) :done)
                                                             :words "gpg asks here"))
      (is equal '("gpg asks here") inside "given back before the program ran")
      (tuatara.frontend:frontend-stop remote)
      (client-done runner))))

(define-test (daemon environment-of-a-client)
  ;; programs started from a client's frame have its variables in place of the daemon's own, and none it lacks
  (let ((display (sb-posix:getenv "DISPLAY")))
    (sb-posix:setenv "DISPLAY" ":99" 1)
    (unwind-protect
         (with-test-daemon (name)
           (multiple-value-bind (local runner)
               (attach-test-client name :environment '(("PATH" . "/client/bin") ("LANG" . "mi_NZ.UTF-8")
                                                       ("GIT_DIR" . "/elsewhere") ("GIT_SSH_COMMAND" . "evil")
                                                       ("GIT_AUTHOR_NAME" . "Aroha") ("GPG_TTY" . "/dev/pts/0")))
             (true (shows local "scratch"))
             (let ((environment (in-daemon (lambda ()
                                             (let ((editor (daemon-editor)))
                                               (tuatara.command:call-in-frame
                                                editor (first (tuatara.command:editor-frames editor))
                                                #'tuatara.io:program-environment))))))
               (true (member "PATH=/client/bin" environment :test #'string=))
               (true (member "LANG=mi_NZ.UTF-8" environment :test #'string=))
               (true (member "GIT_AUTHOR_NAME=Aroha" environment :test #'string=))
               (dolist (name '("DISPLAY=" "GIT_DIR=/elsewhere" "GIT_SSH_COMMAND=" "GPG_TTY="))
                 (false (find-if (lambda (setting) (uiop:string-prefix-p name setting)) environment) "~a passed" name)))
             (type-keys local "C-x C-c")
             (client-ends runner)))
      (if display (sb-posix:setenv "DISPLAY" display 1) (sb-posix:unsetenv "DISPLAY")))))

(define-test (daemon options-read)
  ;; the command line of a daemon, a client and a stop
  (flet ((options (&rest arguments) (tuatara.terminal::read-options arguments :directory "/nonexistent/")))
    (is string= "default" (getf (options "--daemon") :daemon))
    (is string= "work" (getf (options "--daemon=work") :daemon))
    (is string= "work" (getf (options "--client=work" "--start" "a.txt") :client))
    (true (getf (options "--client" "--start") :start))
    (is string= "default" (getf (options "--stop") :stop))
    (dolist (wrong '(("--daemon" "-Q") ("--client" "-Q") ("--daemon" "a.txt") ("--stop" "a.txt") ("--start")
                     ("--daemon=a/b") ("--daemon" "--client") ("--daemon" "--gui") ("--stop" "--gui")
                     ("--client" "-c" "init.lisp")))
      (true (getf (apply #'options wrong) :error) "~s taken" wrong))
    ;; a client in a window of its own
    (let ((options (options "--client" "--gui" "a.txt")))
      (false (getf options :error))
      (true (getf options :gui)))
    (true (search "-Q" (getf (options "--daemon" "-Q") :error)))))

(defclass listed-host (tuatara.runtime:host)
  ((variables :initarg :variables :reader listed-host-variables
              :documentation "The variables each program started is given, as NAME=VALUE strings."))
  (:documentation "A host whose programs are given VARIABLES and nothing else."))

(defmethod tuatara.io:host-program-environment ((host listed-host))
  (listed-host-variables host))

(define-test (daemon variables-kept-narrowly)
  ;; only variables that say where a client is and who it is: none naming where git works, what it runs, or a terminal
  (dolist (name '("PATH" "LANG" "LC_ALL" "TERM" "DISPLAY" "GIT_AUTHOR_NAME" "GIT_COMMITTER_EMAIL"))
    (true (tuatara.command:client-variable-p name) "~a not kept" name))
  (dolist (name '("GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_CONFIG_PARAMETERS" "GIT_EXEC_PATH" "GIT_SSH_COMMAND"
                  "GIT_EXTERNAL_DIFF" "GIT_ASKPASS" "GIT_AUTHOR_DATE" "GIT_TRACE" "GIT_EDITOR" "GPG_TTY" "LD_PRELOAD"))
    (false (tuatara.command:client-variable-p name) "~a kept" name))
  ;; a daemon started from a command line keeps none of what that line ran with
  (let ((tuatara.runtime:*host*
          (make-instance 'listed-host
                         :variables '("HOME=/home/k" "PATH=/bin" "LC_TIME=mi_NZ" "XDG_RUNTIME_DIR=/run/user/1"
                                      "GIT_DIR=/x" "GIT_INDEX_FILE=/x/index" "PWD=/tmp" "TERM=xterm"
                                      "GPG_TTY=/dev/pts/1" "TMUX=/tmp/t" "LD_LIBRARY_PATH=/x" "APPDIR=/x"
                                      "TUATARA_APPIMAGE=1" "TUATARA_DAEMON=other"))))
    (is equal '("TUATARA_DAEMON=kaka" "HOME=/home/k" "PATH=/bin" "LC_TIME=mi_NZ" "XDG_RUNTIME_DIR=/run/user/1")
        (tuatara.io:restricted-environment tuatara.terminal::*daemon-variables* "TUATARA_DAEMON=kaka"))))

(define-test (daemon terminal-checked)
  ;; a terminal a client names is lent only when it is a terminal device of the user's, by a plain path
  (false (tuatara.defaults::checked-terminal nil))
  (false (tuatara.defaults::checked-terminal "/tmp/pts"))
  (false (tuatara.defaults::checked-terminal "/dev/null") "not a terminal of the user's")
  (false (tuatara.defaults::checked-terminal "/dev/pts/../null"))
  (false (tuatara.defaults::checked-terminal "/dev/pts/ptmx") "the user's it is not")
  (false (tuatara.defaults::checked-terminal "/dev/tty" :uid (1+ (sb-posix:geteuid)))))

(define-test (daemon old-kernel-said-plainly)
  ;; a kernel that cannot give the peer's pidfd: refused, saying so in plain words
  (multiple-value-bind (namespace why) (tuatara.defaults::peer-namespace -1)
    (false namespace)
    (false (eq why :unsupported) "a bad descriptor is not an old kernel")))

(define-test (daemon kills-from-decrypted-text-go-with-it)
  ;; text killed in an encrypted file's buffer leaves the kill ring as the client that had it closes
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "secret.txt" dir) "the secret line"))))
      (with-test-daemon (name)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
          (true (shows local "the secret line"))
          (in-daemon (lambda ()
                       (let ((buffer (tuatara.defaults::buffer-visiting file)))
                         (setf (tuatara.buffer:buffer-local buffer :encrypted) (list :symmetric t)
                               (tuatara.io:buffer-plaintext-p buffer) t)
                         (let ((tuatara.command:*current-buffer* buffer))
                           (tuatara.defaults::kill-text "the secret line" :append nil)
                           (tuatara.defaults::kill-text " and more" :append t)))))
          (true (in-daemon (lambda () (find "the secret line and more"
                                            (tuatara.command:editor-kill-ring (daemon-editor)) :test #'equal))))
          (multiple-value-bind (other other-runner) (attach-test-client name :directory dir)
            (true (shows other "scratch"))
            (type-keys local "C-x C-c")
            (is eql 0 (first (client-ends runner)))
            (false (in-daemon (lambda () (find-if (lambda (kill) (and (stringp kill) (search "secret" kill)))
                                                  (tuatara.command:editor-kill-ring (daemon-editor)))))
                   "the kill stayed on the ring")
            (type-keys other "C-x C-c")
            (client-ends other-runner)))))))

(define-test (daemon registers-from-decrypted-text-go-with-it)
  ;; text kept in a register from an encrypted file's buffer goes as the client that had it closes
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "secret.txt" dir) "the secret line"))))
      (with-test-daemon (name)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
          (true (shows local "the secret line"))
          (in-daemon (lambda ()
                       (let* ((editor (daemon-editor))
                              (buffer (tuatara.defaults::buffer-visiting file))
                              (frame (find buffer (tuatara.command:editor-frames editor)
                                           :key (lambda (f) (tuatara.display:window-buffer (tuatara.command:frame-window f))))))
                         (setf (tuatara.buffer:buffer-local buffer :encrypted) (list :symmetric t)
                               (tuatara.io:buffer-plaintext-p buffer) t
                               (tuatara.buffer:buffer-mark buffer) 0
                               (tuatara.buffer:buffer-point buffer) 10
                               (tuatara.display:window-point (tuatara.command:frame-window frame)) 10))))
          (type-keys local "C-x r s a")
          (true (wait-until (lambda () (in-daemon (lambda () (gethash #\a tuatara.defaults::*registers*)))) 10))
          (multiple-value-bind (other other-runner) (attach-test-client name :directory dir)
            (true (shows other "scratch"))
            (type-keys local "C-x C-c")
            (is eql 0 (first (client-ends runner)))
            (false (in-daemon (lambda () (gethash #\a tuatara.defaults::*registers*))) "the register stayed")
            (type-keys other "C-x r i a")
            (true (shows other "Register a is empty"))
            (type-keys other "C-x r l")
            (true (shows other "Register"))
            (false (showing-now other "the secret"))
            (type-keys other "C-x C-c")
            (client-ends other-runner)))))))

(define-test (daemon refusal-names-no-encrypted-file)
  ;; a stop refused for changes that would be lost counts them, and names none of an encrypted file
  (let ((plain (tuatara.buffer:make-buffer "notes.txt"))
        (secret (tuatara.buffer:make-buffer "diary.gpg")))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local secret :encrypted) (list :symmetric t))
           (let ((words (tuatara.defaults::lost-changes-words (list plain secret))))
             (true (search "2 buffers have changes" words) "~a" words)
             (true (search "1 of them encrypted: notes.txt;" words) "~a" words)
             (false (search "diary" words)))
           (let ((words (tuatara.defaults::lost-changes-words (list secret))))
             (true (search "1 buffer has changes no auto-saved copy would keep, 1 of them encrypted;" words) "~a" words)))
      (tuatara.buffer:kill-buffer plain)
      (tuatara.buffer:kill-buffer secret))))

;;; Clients in windows of their own

(define-test (daemon window-client-opens-another-window)
  ;; C-x 5 2 in a client in a window: another window of that client, a connection of its own, showing the
  ;; same buffer; its close button closes it alone, and C-x C-c closes the client, every window with it
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "kaka.txt" dir) "kaka text"))))
      (with-test-daemon (name)
        (multiple-value-bind (open opened) (window-opener name)
          (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file) :window t
                                                                       :another open)
            (true (shows local "kaka text"))
            (type-keys local "C-x 5 2")
            (true (wait-until (lambda () (car opened)) 10) "no other window was opened")
            (destructuring-bind (&optional second second-runner &rest window) (first (car opened))
              (declare (ignore window))
              (when second
                (true (shows second "kaka text"))
                (is = 2 (live-client-frames))
                ;; one buffer, typed into in the second window, shown in the first
                (type-keys second "i w e k a <escape>")
                (true (shows local "wekakaka text"))
                ;; the second window's close button: its frame alone
                (type-at second (tuatara.frontend:make-event :close))
                (is equal '(0 nil) (client-ends second-runner))
                (is = 1 (live-client-frames))
                (true (shows local "wekakaka text"))))
            (type-keys local "C-x C-c")
            (is equal '(0 nil) (client-ends runner))
            (is = 0 (live-client-frames))))))))

(define-test (daemon first-window-closed-client-goes-on)
  ;; the window a client in windows opened first may close first: the client goes on in the other, which
  ;; then gives the exit code; every window of a client ending is given its code
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "COMMIT_EDITMSG" dir))))
      (alexandria:write-string-into-file (format nil "~%# Please enter the commit message.~%") file)
      (with-test-daemon (name)
        (multiple-value-bind (open opened) (window-opener name)
          (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file) :window t
                                                                       :another open)
            (true (shows local "Please enter"))
            (type-keys local "C-x 5 2")
            (true (wait-until (lambda () (car opened)) 10) "no other window was opened")
            (destructuring-bind (&optional second second-runner &rest window) (first (car opened))
              (declare (ignore window))
              (when second
                (true (shows second "Please enter"))
                (type-at local (tuatara.frontend:make-event :close))
                (is equal '(0 nil) (client-ends runner))
                (is = 1 (live-client-frames) "the client goes on in its other window")
                ;; given up there: the client ends with 1, as git's editor
                (type-keys second "N o p e C-c C-k")
                (is eql 1 (first (client-ends second-runner)))))
            (is = 0 (live-client-frames))))))))

(define-test (daemon window-asked-for-by-no-one)
  ;; a connection saying it is a window the daemon asked for, by a word it never gave, is let go with 1
  (with-test-daemon (name)
    (multiple-value-bind (local runner client) (attach-test-client name :window t :joins "NOSUCHWORD")
      (declare (ignore local))
      (is equal '(1 "The daemon asked for no such window") (client-ends runner))
      ;; never painted: a client in windows takes no exit code from it
      (false (tuatara.frontend:remote-client-painted client))
      (is = 0 (live-client-frames)))))

(define-test (daemon terminal-client-asked-for-no-window)
  ;; C-x 5 2 in a client in a terminal: a frame taking turns there, no connection asked for
  (with-test-daemon (name)
    (multiple-value-bind (open opened) (window-opener name)
      (multiple-value-bind (local runner) (attach-test-client name :another open)
        (true (shows local "scratch"))
        (type-keys local "C-x 5 2")
        (true (wait-until (lambda () (= 2 (live-client-frames))) 10))
        (false (car opened))
        (type-keys local "C-x C-c")
        (is equal '(0 nil) (client-ends runner))))))

(define-test (daemon rows-drawn-large-in-a-window-client)
  ;; a headline whose face is drawn two rows tall: laid out so for a client in a window, as one row in a terminal
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "tall.org" dir) (format nil "* Tall~%Under it.~%"))))
          (face (tuatara.display:find-face :org-level-1)))
      (with-test-daemon (name)
        (let ((old (in-daemon (lambda ()
                                (prog1 (tuatara.display::face-height face)
                                  (tuatara.display:set-face-attributes :org-level-1 :height 2))))))
          (unwind-protect
               (multiple-value-bind (window window-runner) (attach-test-client name :directory dir :files (list file)
                                                                                  :window t)
                 (true (shows window "Tall"))
                 (multiple-value-bind (terminal terminal-runner) (attach-test-client name :directory dir
                                                                                         :files (list file))
                   (true (shows terminal "Tall"))
                   (type-keys window "j")
                   (true (wait-until (lambda () (aref (tuatara.frontend.fake:fake-row-scales window) 0)) 10)
                         "the window's headline drawn as one row")
                   (false (aref (tuatara.frontend.fake:fake-row-scales terminal) 0))
                   (type-keys terminal "C-x C-c")
                   (client-ends terminal-runner))
                 (type-keys window "C-x C-c")
                 (client-ends window-runner))
            (in-daemon (lambda () (setf (tuatara.display::face-height face) old)))))))))

(define-test (daemon window-client-in-windows)
  ;; tuatara --client --gui in real windows on a display of their own, the daemon in this process: typing,
  ;; a paste from the desktop's clipboard, the font a step larger in one window alone, C-x 5 2 (both
  ;; windows drawn by the renderer the daemon's settings choose) and a close
  ;; button, a file and a picture dropped, and C-x C-c ending the client with 0, its buffers left in the daemon
  (if (null (tuatara.io:find-program "gtk4-broadwayd"))
      (skip "gtk4-broadwayd is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((file (namestring (put-file (merge-pathnames "kiwi.txt" dir) (format nil "kiwi window~%"))))
              (dropped (namestring (put-file (merge-pathnames "kea.txt" dir) (format nil "dropped kea~%"))))
              (picture (namestring (merge-pathnames "dot.png" dir))))
          (tuatara.frontend.gtk::load-gtk-libraries)
          (let* ((surface (tuatara.frontend.gtk::cairo-image-surface-create 0 64 48))
                 (cr (tuatara.frontend.gtk::cairo-create surface)))
            (tuatara.frontend.gtk::cairo-set-source-rgb cr 0.2d0 0.6d0 0.3d0)
            (tuatara.frontend.gtk::cairo-paint cr)
            (tuatara.frontend.gtk::cairo-surface-write-to-png surface picture)
            (tuatara.frontend.gtk::cairo-destroy cr)
            (tuatara.frontend.gtk::cairo-surface-destroy surface))
          (with-test-daemon (name)
            (let ((seen (let ((renderer tuatara-user:*gui-renderer*))
                          ;; the window client's renderer as the daemon's init file chose it
                          (setf tuatara-user:*gui-renderer* :cairo)
                          (unwind-protect (run-on-broadway dir "window-client.lisp" name
                                                           tuatara.defaults::*runtime-directory* file dropped picture
                                                           :out)
                            (setf tuatara-user:*gui-renderer* renderer)))))
              (true seen "the client wrote nothing")
              (false (getf seen :error) "~a" (getf seen :error))
              (false (getf seen :timed-out))
              (dolist (step '(:window :shows-file :typed :pasted :font-larger :another-window :another-shows
                              :typed-in-another :closed-another :dropped :picture))
                (true (getf seen step) "~(~a~) in ~s" step seen))
              (is eql 0 (getf seen :another-font-steps) "the other window's font as it started")
              (is equal '("GskCairoRenderer" "GskCairoRenderer") (getf seen :renderers))
              (is equal "cairo" (getf seen :renderer-kept))
              (false (getf seen :renderer-given) "GSK_RENDERER given to a program started")
              (is eql 0 (getf seen :exit-code))
              (is eql 0 (getf seen :windows-left))
              (is = 0 (live-client-frames))
              (in-daemon (lambda ()
                           (let ((buffer (tuatara.defaults::buffer-visiting file)))
                             (true buffer "its buffer stays in the daemon")
                             (when buffer
                               (true (search "from the desktop" (tuatara.buffer:buffer-string buffer)))
                               (true (search "zz" (tuatara.buffer:buffer-string buffer)))))
                           (true (tuatara.defaults::buffer-visiting dropped) "the file dropped opened")))))))))

(defmacro with-window-seconds ((seconds) &body body)
  "Run BODY with a window asked of a client waited for SECONDS at most."
  (let ((was (gensym "WAS")))
    `(let ((,was tuatara.frontend:*window-seconds*))
       (setf tuatara.frontend:*window-seconds* ,seconds)
       (unwind-protect (progn ,@body)
         (setf tuatara.frontend:*window-seconds* ,was)))))

(defun seconds-until (test &optional (seconds 20))
  "The seconds until TEST, a function of none, is true, waiting SECONDS at most; NIL when it never was."
  (let ((began (get-internal-real-time)))
    (and (wait-until test seconds) (tuatara.runtime:seconds-since began))))

(define-test (daemon window-declined-at-once)
  ;; a client in a window that opens no other window (as many open as it opens) says so at once: C-x 5 2
  ;; returns at once, the frame is not made, and the reason is said
  (with-test-daemon (name)
    (multiple-value-bind (local runner)
        (attach-test-client name :window t
                                 :another (lambda (client word)
                                            (tuatara.frontend:decline-window
                                             client word "the client shows 32 windows, the most it opens")))
      (true (shows local "scratch"))
      (type-keys local "C-x 5 2")
      (let ((said (seconds-until (lambda () (showing-now local "No other window was opened")) 10)))
        (true said "the refusal was not said")
        (true (and said (< said 2)) "said after ~a seconds" said))
      (true (showing-now local "32 windows"))
      (is = 1 (live-client-frames))
      (type-keys local "C-x C-c")
      (is equal '(0 nil) (client-ends runner)))))

(define-test (daemon window-never-coming-holds-no-one)
  ;; a client asked for a window that never comes holds up neither itself nor another client: each types on
  ;; at once; past the wait it is said, and the window coming later still is let go, never painted
  (with-window-seconds (2)
    (with-test-daemon (name)
      (multiple-value-bind (open opened) (window-opener name :after 4)
        (multiple-value-bind (asking asking-runner) (attach-test-client name :window t :another open)
          (multiple-value-bind (other other-runner) (attach-test-client name)
            (true (shows asking "scratch"))
            (true (shows other "scratch"))
            (type-keys asking "C-x 5 2")
            (type-keys other "i k i w i <escape>")
            (let ((typed (seconds-until (lambda () (showing-now other "kiwi")) 10)))
              (true (and typed (< typed 1)) "typed in another client after ~a seconds" typed))
            (type-keys asking "i r u r u <escape>")
            (let ((typed (seconds-until (lambda () (showing-now asking "ruru")) 10)))
              (true (and typed (< typed 1)) "typed in the asking client after ~a seconds" typed))
            (true (shows asking "did not open within 2 seconds" 10))
            ;; the window coming late: let go, its client never painted
            (true (wait-until (lambda () (car opened)) 10))
            (destructuring-bind (&optional late late-runner late-client) (first (car opened))
              (declare (ignore late))
              (when late-runner
                (is equal '(1 "The daemon asked for no such window") (client-ends late-runner))
                (false (tuatara.frontend:remote-client-painted late-client))))
            (is = 2 (live-client-frames))
            (type-keys other "C-x C-c")
            (client-ends other-runner)
            (type-keys asking "C-x C-c")
            (is equal '(0 nil) (client-ends asking-runner))))))))

(define-test (daemon frames-from-a-window-client-at-once)
  ;; --frames 3 from a client in a window: the windows asked for, and what is typed taken at once, not
  ;; after waiting for each window
  (with-window-seconds (2)
    (with-test-daemon (name)
      (multiple-value-bind (local runner)
          (attach-test-client name :window t :frames 3 :another (lambda (client word) (declare (ignore client word))))
        (true (shows local "scratch"))
        (type-keys local "i k i w i <escape>")
        (let ((typed (seconds-until (lambda () (showing-now local "kiwi")) 20)))
          (true (and typed (< typed 1)) "typed after ~a seconds" typed))
        (is = 1 (live-client-frames))
        (type-keys local "C-x C-c")
        (is equal '(0 nil) (client-ends runner))))))

(define-test (daemon window-requests-held-to-their-most)
  ;; a client asking for window after window: at most four asked for at once, and its windows held to 32
  (with-test-daemon (name)
    (let ((asked 0))
      (multiple-value-bind (local runner)
          (attach-test-client name :window t :another (lambda (client word)
                                                        (declare (ignore client word))
                                                        (incf asked)))
        (true (shows local "scratch"))
        (type-keys local "C-x 5 2 C-x 5 2 C-x 5 2 C-x 5 2 C-x 5 2 C-x 5 2")
        (true (shows local "4 windows asked for are still to come" 10))
        (is = 4 asked "asked of the client")
        (type-keys local "C-x C-c")
        (is equal '(0 nil) (client-ends runner)))))
  (with-test-daemon (name)
    (multiple-value-bind (open opened) (window-opener name)
      (multiple-value-bind (local runner) (attach-test-client name :window t :another open)
        (true (shows local "scratch"))
        (loop repeat 40
              for before = (live-client-frames)
              do (type-keys local "C-x 5 2")
                 (wait-until (lambda () (or (> (live-client-frames) before) (showing-now local "No other window"))) 10)
              until (showing-now local "No other window"))
        (is = tuatara.frontend:+most-client-windows+ (live-client-frames))
        (true (showing-now local "the client shows 32 windows"))
        (type-keys local "C-x C-c")
        (is equal '(0 nil) (client-ends runner))
        (dolist (window (car opened))
          (client-ends (second window)))))))

(define-test (daemon window-for-a-killed-buffer-shows-another)
  ;; the buffer a window was asked to show is killed before the window comes: it shows another of the
  ;; client's buffers, never closed unasked
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "moa.txt" dir) "moa text"))))
      (with-test-daemon (name)
        (multiple-value-bind (open opened) (window-opener name :after 1)
          (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file) :window t
                                                                       :another open)
            (true (shows local "moa text"))
            (type-keys local "C-x 5 2")
            (true (shows local "Opening a window") "the window was not asked for")
            (in-daemon (lambda () (tuatara.buffer:kill-buffer (tuatara.defaults::buffer-visiting file))))
            (false (in-daemon (lambda () (tuatara.defaults::buffer-visiting file))) "the buffer stays")
            (true (wait-until (lambda () (car opened)) 10))
            (destructuring-bind (&optional second second-runner &rest client) (first (car opened))
              (declare (ignore client))
              (when second
                (true (wait-until (lambda () (= 2 (live-client-frames))) 10) "the window was closed")
                (true (shows second "scratch"))
                (type-keys local "C-x C-c")
                (is equal '(0 nil) (client-ends runner))
                (client-ends second-runner)))))))))

;;; Its lifetime

(define-test (daemon second-client-while-one-asks)
  ;; a client left at a question holds no other back: the next is given its frame meanwhile, the question
  ;; still open
  (with-test-daemon (name)
    (multiple-value-bind (a a-runner) (attach-test-client name)
      (true (shows a "scratch"))
      (type-keys a "C-x b")
      (true (shows a "Switch to buffer"))
      (multiple-value-bind (b b-runner) (attach-test-client name)
        (true (shows b "scratch" 10) "shown while the first client asks")
        (true (showing-now a "Switch to buffer") "the question goes on")
        (type-keys b "C-x C-c")
        (is equal '(0 nil) (client-ends b-runner))
        (type-keys a "C-g C-x C-c")
        (is equal '(0 nil) (client-ends a-runner))))))

(define-test (daemon leader-on-c-x-spc-in-a-client)
  ;; in a client's frame C-x SPC opens the leader's keys in INSERT state, their key hints shown there
  (unwind-protect
       (progn
         (tuatara-user:bind-key "o y" 'beginning-of-line :map :leader)
         (tuatara-user:bind-key "o" nil :map :leader :label "own")
         (with-test-daemon (name)
           (multiple-value-bind (local runner) (attach-test-client name)
             (true (shows local "scratch"))
             (type-keys local "i k e a C-x SPC")
             (true (shows local "o +own"))
             (type-keys local "o y t u i")
             (true (shows local "tuikea") "the screen: ~s" (tuatara.frontend.fake:fake-screen local))
             (type-keys local "C-g C-x C-c")
             (is equal '(0 nil) (client-ends runner)))))
    (tuatara-user:unbind-key "o y" :map :leader)
    (tuatara-user:unbind-key "o" :map :leader)))

(defun daemon-terminated (daemon thread)
  "Send this process SIGTERM, as the desktop does at logout, with it ending
DAEMON (TUATARA.DEFAULTS::END-DAEMON-ON-TERMINATE), its editor running on
THREAD, and wait for that to end; true when it did. SIGTERM ends Lisp again
after, as it does by itself."
  (tuatara.defaults::end-daemon-on-terminate daemon)
  (unwind-protect
       (progn (sb-posix:kill (sb-posix:getpid) sb-unix:sigterm)
              (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))
    (sb-sys:enable-interrupt sb-unix:sigterm #'sb-unix::sigterm-handler)))

(define-test (daemon terminated-keeps-auto-saved-copies)
  ;; SIGTERM, as at logout: nothing asked, a question open given up, every client told and exiting 1, the
  ;; changes kept as auto-saved copies, never an encrypted file's text
  (with-test-directory (dir)
    (let ((plain (namestring (put-file (merge-pathnames "plain.txt" dir) "plain")))
          (secret (namestring (put-file (merge-pathnames "secret.txt" dir) "secret")))
          (was tuatara-user:*auto-save*))
      (setf tuatara-user:*auto-save* t)
      (unwind-protect
           (with-test-daemon (name thread daemon)
             (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list plain secret))
               (true (shows local "plain"))
               (in-daemon (lambda ()
                            (let ((buffer (tuatara.defaults::buffer-visiting secret)))
                              (setf (tuatara.buffer:buffer-local buffer :encrypted) (list :symmetric t)
                                    (tuatara.io:buffer-plaintext-p buffer) t)
                              (tuatara.buffer:buffer-insert buffer 0 "very "))))
               (type-keys local "i y")
               (true (shows local "yplain"))
               (type-keys local "ESC C-x b")
               (true (shows local "Switch to buffer"))
               (true (daemon-terminated daemon thread) "the daemon ended")
               (let ((ended (client-ends runner)))
                 (is eql 1 (first ended))
                 (true (search "SIGTERM" (second ended)) "~a" (second ended)))
               (let ((copy (tuatara.defaults::auto-save-path plain)))
                 (unwind-protect (is string= "yplain" (file-text copy))
                   (ignore-errors (delete-file copy))))
               (false (probe-file (tuatara.defaults::auto-save-path secret))
                      "an encrypted file's text is written nowhere")
               (is string= "plain" (file-text plain) "nothing saved")
               (is string= "secret" (file-text secret))))
        (setf tuatara-user:*auto-save* was)))))

(define-test (daemon stopping-wakes-its-loop)
  ;; a daemon stopped by work taken in on a wake, as SIGTERM's is, is woken to end at once, never left
  ;; waiting for an event that may not come
  (with-test-daemon (name thread daemon)
    (true (in-daemon (lambda ()
                       (let ((queue (tuatara.defaults::daemon-queue daemon)))
                         (loop while (tuatara.frontend:queue-pop queue 0))
                         (tuatara.defaults::stop-daemon (daemon-editor) :here nil)
                         (not (tuatara.frontend:queue-empty-p queue)))))
          "woken")
    (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))))

(define-test (daemon terminated-with-auto-saving-off)
  ;; SIGTERM cannot be refused as tuatara --stop is: with auto-saving off, nothing is written and the daemon
  ;; ends, every client exiting 1
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "off.txt" dir) "off"))))
      (with-test-daemon (name thread daemon)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
          (true (shows local "off"))
          (type-keys local "i z")
          (true (shows local "zoff"))
          (true (daemon-terminated daemon thread))
          (is eql 1 (first (client-ends runner)))
          (false (probe-file (tuatara.defaults::auto-save-path file)))
          (is string= "off" (file-text file)))))))

(defun daemon-session-path (name)
  "The file the daemon NAME keeps its session in."
  (tuatara.io:path-join (tuatara.io:native (tuatara.io:state-directory)) (format nil "session-daemon-~a" name)))

(defun client-frame-windows (editor)
  "How many windows the frame of EDITOR's one client attached now shows."
  (let ((client (find-if (lambda (client) (and (tuatara.command:client-remote client)
                                               (not (tuatara.command:client-ended client))))
                         (tuatara.command:editor-clients (daemon-editor)))))
    (length (tuatara.display:area-windows
             (tuatara.display:area-root
              (tuatara.command:frame-window (first (tuatara.command:client-frames editor client))))))))

(define-test (daemon session-kept-by-name)
  ;; with the session kept, a daemon keeps it in a file of its name, the editor's own left alone; started
  ;; again it has its clients' folders, as ended, and a client asking for no files takes up the last, its
  ;; windows as they were
  (with-test-directory (dir)
    (let* ((file (namestring (put-file (merge-pathnames "kept.txt" dir) "kept text")))
           (name (format nil "kept~d" (incf *test-daemons*)))
           (own (tuatara.defaults::session-path))
           (own-before (and (probe-file own) (file-text own)))
           (was tuatara.defaults::*keep-session*))
      (setf tuatara.defaults::*keep-session* t)
      (unwind-protect
           (progn
             (with-test-daemon (daemon-name thread daemon name)
               (multiple-value-bind (local runner) (attach-test-client daemon-name :directory dir :files (list file))
                 (true (shows local "kept text"))
                 (type-keys local "C-x 2")
                 (true (wait-until (lambda () (in-daemon (lambda () (= 2 (client-frame-windows (daemon-editor)))))) 10))
                 (type-keys local "C-x C-c")
                 (is equal '(0 nil) (client-ends runner))))
             (let ((text (file-text (daemon-session-path name))))
               (true (search (format nil "~%client 1 ") text) "~a" text)
               (true (search "kept.txt" text)))
             (is equal own-before (and (probe-file own) (file-text own)) "the editor's own session is left alone")
             ;; a daemon started again is a process of its own: none of the buffers of the one before
             (let ((buffer (tuatara.defaults::buffer-visiting file)))
               (when buffer (tuatara.buffer:kill-buffer buffer)))
             (with-test-daemon (daemon-name thread daemon name)
               (true (in-daemon (lambda ()
                                  (let ((buffer (tuatara.defaults::buffer-visiting file)))
                                    (and buffer (some #'tuatara.command:client-ended
                                                      (tuatara.command:buffer-clients buffer))))))
                     "its folder is there, as ended")
               (multiple-value-bind (local runner) (attach-test-client daemon-name :directory dir)
                 (true (shows local "kept text"))
                 (true (wait-until (lambda () (in-daemon (lambda () (= 2 (client-frame-windows (daemon-editor)))))) 10)
                       "its windows as they were")
                 (in-daemon (lambda ()
                              (let ((clients (tuatara.command:buffer-clients (tuatara.defaults::buffer-visiting file))))
                                (is = 1 (length clients))
                                (false (tuatara.command:client-ended (first clients)) "the folder taken up"))))
                 (type-keys local "C-x C-c")
                 (client-ends runner))))
        (setf tuatara.defaults::*keep-session* was)
        (ignore-errors (delete-file (daemon-session-path name)))))))

(define-test (daemon client-fallback)
  ;; tuatara --client --fallback with no daemon running: nothing said, an editor of its own instead
  (let ((name (format nil "none~d" (incf *test-daemons*))))
    (let ((said (with-output-to-string (*error-output*)
                  (is eq :fallback (tuatara.terminal::run-client (list :client name :fallback t :files '())
                                                                 :terminal t)))))
      (is string= "" said))
    ;; as the desktop's Tuatara (client) starts it, in a window, with no terminal
    (is eq :fallback (tuatara.terminal::run-client (list :client name :fallback t :gui t :files '())
                                                   :terminal nil :desktop nil))
    (let ((said (with-output-to-string (*error-output*)
                  (is eql 1 (tuatara.terminal::run-client (list :client name :files '()) :terminal t)))))
      (true (search "no daemon named" said)))))

(define-test (daemon client-collects-often)
  ;; a client of a daemon keeps the heap it was saved with, of many gigabytes, but collects its
  ;; garbage after a few megabytes of it: the memory it touches painting frame after frame stays
  ;; small and in use, not hundreds of megabytes lying idle for the system to move out to swap
  (let ((was (sb-ext:bytes-consed-between-gcs)))
    (unwind-protect
         (progn
           (tuatara.terminal::fit-heap (list "--client" "--gui"))
           (is = tuatara.defaults::*client-nursery-octets* (sb-ext:bytes-consed-between-gcs))
           (true (<= (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))))
      (setf (sb-ext:bytes-consed-between-gcs) was))))

(define-test (daemon lifetime-options-read)
  ;; --fallback goes with --client alone; --generate-autostart with nothing but --daemon=NAME
  (flet ((options (&rest arguments) (tuatara.terminal::read-options arguments :directory "/nonexistent/")))
    (true (getf (options "--client" "--fallback" "a.txt") :fallback))
    (let ((read (options "--gui" "--client" "--fallback" "a.txt")))
      (false (getf read :error) "~a" (getf read :error))
      (true (getf read :fallback)))
    (true (getf (options "--generate-autostart") :generate-autostart))
    (is string= "work" (getf (options "--generate-autostart" "--daemon=work") :daemon))
    (dolist (wrong '(("--fallback") ("--client" "--start" "--fallback") ("--stop" "--fallback")
                     ("--generate-autostart" "a.txt") ("--generate-autostart" "--client")
                     ("--generate-autostart" "-c" "init.lisp") ("--generate-autostart" "--gui")))
      (true (getf (apply #'options wrong) :error) "~s taken" wrong))))

(defun call-with-slow-saves (seconds function)
  "Call FUNCTION while every save takes SECONDS longer, as on a slow drive."
  (sb-int:encapsulate 'tuatara.io::transformed-text 'slow (lambda (transformed &rest arguments)
                                                            (sleep seconds)
                                                            (apply transformed arguments)))
  (unwind-protect (funcall function)
    (sb-int:unencapsulate 'tuatara.io::transformed-text 'slow)))

(define-test (daemon quit-waits-for-saves-begun)
  ;; M-x daemon-quit answered s: the daemon ends only once the saves it began are written, never cut short
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "slow.txt" dir) "slow"))))
      (call-with-slow-saves
       2 (lambda ()
           (with-test-daemon (name thread)
             (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
               (true (shows local "slow"))
               (type-keys local "i x")
               (true (shows local "xslow"))
               (type-keys local "M-x d a e m o n - q u i t RET")
               (true (shows local "changes not saved"))
               (type-keys local "s")
               (is eql 0 (first (client-ends runner 30)))
               (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))
               (is string= "xslow" (file-text file) "written before the daemon ended"))))))))

(define-test (daemon terminated-waits-for-saves)
  ;; SIGTERM while a save runs: the save ends before the daemon does; one that takes longer than the daemon
  ;; waits is said, by its buffer's name, and the daemon ends all the same
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "saving.txt" dir) "saving"))))
      (call-with-slow-saves
       2 (lambda ()
           (with-test-daemon (name thread daemon)
             (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
               (true (shows local "saving"))
               (type-keys local "i x C-x C-s")
               (true (wait-until (lambda () (tuatara.defaults::running-jobs)) 10) "the save runs")
               (true (daemon-terminated daemon thread))
               (is eql 1 (first (client-ends runner)))
               (is string= "xsaving" (file-text file) "written before the daemon ended")))))
      (let ((was tuatara.defaults::*no-one-to-ask-save-seconds*))
        (setf tuatara.defaults::*no-one-to-ask-save-seconds* 1)
        (unwind-protect
             (call-with-slow-saves
              6 (lambda ()
                  (with-test-daemon (name thread daemon)
                    (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
                      (true (shows local "xsaving"))
                      (type-keys local "i y C-x C-s")
                      (true (wait-until (lambda () (tuatara.defaults::running-jobs)) 10))
                      (let ((start (get-internal-real-time)))
                        (true (daemon-terminated daemon thread))
                        (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second))
                              "the daemon waits no longer than it may"))
                      (is eql 1 (first (client-ends runner)))
                      (true (search "cut short: saving.txt"
                                    (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))))
                  ;; the save cut short in a real daemon goes on here: let it end before the folder goes
                  (wait-until (lambda () (null (tuatara.defaults::running-jobs))) 10)))
          (setf tuatara.defaults::*no-one-to-ask-save-seconds* was))))))

(define-test (daemon evidence-waits-for-take-up)
  ;; a file opened safely (evidence) in the kept session is not opened as the daemon starts again: it waits
  ;; closed in its folder, said by how many, and is opened safely when a client takes the folder up; a plain
  ;; file opens again at once
  (with-test-directory (dir)
    (let* ((evidence (namestring (put-file (merge-pathnames "disk.img" dir) "evidence text")))
           (notes (namestring (put-file (merge-pathnames "notes.txt" dir) "notes text")))
           (name (format nil "evidence~d" (incf *test-daemons*)))
           (was tuatara.defaults::*keep-session*))
      (setf tuatara.defaults::*keep-session* t)
      (flet ((forget-buffers ()
               ;; a daemon started again is a process of its own: none of the buffers of the one before
               (dolist (file (list evidence notes))
                 (let ((buffer (tuatara.defaults::buffer-visiting file)))
                   (when buffer (tuatara.buffer:kill-buffer buffer))))))
        (unwind-protect
             (progn
               (with-test-daemon (daemon-name thread daemon name)
                 (multiple-value-bind (local runner) (attach-test-client daemon-name :directory dir :files (list notes))
                   (true (shows local "notes text"))
                   (type-keys local "C-x C-c")
                   (client-ends runner))
                 (multiple-value-bind (local runner)
                     (attach-test-client daemon-name :directory dir :files (list evidence) :safe t)
                   (true (shows local "evidence text"))
                   (type-keys local "C-x C-c")
                   (client-ends runner)))
               (true (search (format nil "buffer safe 0 ~a" evidence) (file-text (daemon-session-path name))))
               (forget-buffers)
               (with-test-daemon (daemon-name thread daemon name)
                 (false (in-daemon (lambda () (tuatara.defaults::buffer-visiting evidence))) "not opened at the start")
                 (true (in-daemon (lambda () (tuatara.defaults::buffer-visiting notes))) "a plain file opened again")
                 (true (search "1 file opened safely (evidence) waits closed"
                               (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
                 (multiple-value-bind (local runner) (attach-test-client daemon-name :directory dir)
                   (true (shows local "evidence text") "opened as the folder is taken up")
                   (true (in-daemon (lambda () (tuatara.io:safe-visit-p (tuatara.defaults::buffer-visiting evidence))))
                         "and safely")
                   (true (search "1 file of the folder taken up opened safely"
                                 (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
                   (type-keys local "C-x C-c")
                   (client-ends runner))))
          (setf tuatara.defaults::*keep-session* was)
          (forget-buffers)
          (ignore-errors (delete-file (daemon-session-path name))))))))

(define-test (daemon window-client-takes-up-its-windows)
  ;; a client in windows taking up the folder of one that had two windows: the second is asked of it, and
  ;; laid out as it was once it comes
  (with-test-directory (dir)
    (let* ((file (namestring (put-file (merge-pathnames "toroa.txt" dir) "toroa text")))
           (name (format nil "windows~d" (incf *test-daemons*)))
           (was tuatara.defaults::*keep-session*))
      (setf tuatara.defaults::*keep-session* t)
      (unwind-protect
           (progn
             (with-test-daemon (daemon-name thread daemon name)
               (multiple-value-bind (open opened) (window-opener daemon-name)
                 (multiple-value-bind (local runner)
                     (attach-test-client daemon-name :directory dir :files (list file) :window t :another open)
                   (true (shows local "toroa text"))
                   (type-keys local "C-x 5 2")
                   (true (wait-until (lambda () (car opened)) 10) "no other window was opened")
                   (let ((second (first (first (car opened)))))
                     (when second
                       (true (shows second "toroa text"))
                       (type-keys second "C-x 2")
                       (true (wait-until (lambda () (= 2 (count-if (lambda (row) (search "toroa text" row))
                                                                   (tuatara.frontend.fake:fake-screen second))))
                                         10))))
                   (type-keys local "C-x C-c")
                   (client-ends runner))))
             (let ((buffer (tuatara.defaults::buffer-visiting file)))
               (when buffer (tuatara.buffer:kill-buffer buffer)))
             (with-test-daemon (daemon-name thread daemon name)
               (multiple-value-bind (open opened) (window-opener daemon-name)
                 (multiple-value-bind (local runner)
                     (attach-test-client daemon-name :directory dir :window t :another open)
                   (true (shows local "toroa text"))
                   (true (wait-until (lambda () (car opened)) 10) "the second window was not asked for")
                   (let ((second (first (first (car opened)))))
                     (when second
                       (true (wait-until (lambda () (= 2 (count-if (lambda (row) (search "toroa text" row))
                                                                   (tuatara.frontend.fake:fake-screen second))))
                                         10)
                             "laid out as it was")))
                   (is = 2 (live-client-frames))
                   (type-keys local "C-x C-c")
                   (client-ends runner)))))
        (setf tuatara.defaults::*keep-session* was)
        (ignore-errors (delete-file (daemon-session-path name)))))))

(define-test (daemon terminated-auto-saves-before-waiting)
  ;; SIGTERM while a save runs: the other changes are auto-saved before the daemon waits for the save, so
  ;; that a kill during the wait still leaves them
  (with-test-directory (dir)
    (let ((saving (namestring (put-file (merge-pathnames "saving.txt" dir) "saving")))
          (other (namestring (put-file (merge-pathnames "other.txt" dir) "other")))
          (was tuatara-user:*auto-save*))
      (setf tuatara-user:*auto-save* t)
      (unwind-protect
           (call-with-slow-saves
            4 (lambda ()
                (with-test-daemon (name thread daemon)
                  (multiple-value-bind (local runner)
                      (attach-test-client name :directory dir :files (list saving other))
                    (true (shows local "saving"))
                    (in-daemon (lambda ()
                                 (tuatara.buffer:buffer-insert (tuatara.defaults::buffer-visiting other) 0 "z")))
                    (type-keys local "i x C-x C-s")
                    (true (wait-until (lambda () (tuatara.defaults::running-jobs)) 10) "the save runs")
                    (tuatara.defaults::end-daemon-on-terminate daemon)
                    (unwind-protect
                         (progn
                           (sb-posix:kill (sb-posix:getpid) sb-unix:sigterm)
                           (let ((copy (tuatara.defaults::auto-save-path other)))
                             (true (wait-until (lambda () (probe-file copy)) 3) "no copy while the daemon waits")
                             (true (sb-thread:thread-alive-p thread) "the daemon still waits for the save")
                             (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))
                             (ignore-errors (delete-file copy))))
                      (sb-sys:enable-interrupt sb-unix:sigterm #'sb-unix::sigterm-handler))
                    (client-ends runner)))))
        (setf tuatara-user:*auto-save* was)))))

(define-test (daemon lent-terminal-words-without-controls)
  ;; a file name with terminal escapes in the words shown is sent to the client without them
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let* ((local (make-instance 'lending-fake-frontend))
           (maker (progn (tuatara.frontend:frontend-start local)
                         (sb-thread:make-thread
                          (lambda () (tuatara.frontend:make-remote-client
                                      theirs local :request (tuatara.frontend:make-request :terminal "/dev/pts/99"))))))
           (remote (tuatara.frontend:make-remote-frontend mine))
           (client (sb-thread:join-thread maker))
           (runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
      (tuatara.frontend:frontend-lend-terminal remote (constantly t)
                                               :words (format nil "decrypting a~c]0;owned~c.gpg" #\Esc (code-char 7)))
      (is equal '("decrypting a]0;owned.gpg") (lending-lent local))
      (tuatara.frontend:frontend-stop remote)
      (client-done runner))))

(define-test (daemon output-without-a-log-goes-nowhere)
  ;; with no log to be had, what the daemon prints once ready goes to /dev/null, not the starter's closed pipe
  (sb-int:encapsulate 'tuatara.io:session-log 'no-log (lambda (function &rest arguments)
                                                        (declare (ignore function arguments))
                                                        nil))
  (let ((fd (unwind-protect (tuatara.terminal::daemon-output "test")
              (sb-int:unencapsulate 'tuatara.io:session-log 'no-log))))
    (true fd)
    (when fd
      (unwind-protect (is = (sb-posix:stat-rdev (sb-posix:stat "/dev/null")) (sb-posix:stat-rdev (sb-posix:fstat fd)))
        (sb-posix:close fd)))))

(define-test (daemon lock-taken-as-core-takes-locks)
  ;; the daemon's lock is made and taken by core's one way of doing it, never through a link
  (let ((opened '()))
    (sb-int:encapsulate 'tuatara.io:open-lock-file 'noting-locks
                        (lambda (function path &rest keys) (push path opened) (apply function path keys)))
    (unwind-protect (tuatara.defaults::close-daemon (tuatara.defaults::open-daemon "core-lock-test"))
      (sb-int:unencapsulate 'tuatara.io:open-lock-file 'noting-locks))
    (is equal (list (tuatara.defaults::daemon-path "core-lock-test" "lock")) opened))
  ;; a link where the lock should be is refused, saying why
  (let ((path (tuatara.defaults::daemon-path "link-lock-test" "lock")))
    (sb-posix:symlink "/nonexistent/lock" path)
    (unwind-protect
         (let ((refused (handler-case (progn (tuatara.defaults::open-daemon "link-lock-test") nil)
                          (tuatara.runtime:refusal (condition) (princ-to-string condition)))))
           (true (search "cannot be opened" refused) "~a" refused))
      (sb-posix:unlink path))))

;;; The folders of clients that ended in the buffer list

(defun faces-over (local row text)
  "The faces the last frame of LOCAL painted over the first cell of ROW showing TEXT."
  (let ((column (search text (tuatara.frontend.fake:fake-screen-line local row))))
    (and column (faces-at local row column))))

(defun remote-client-numbered (number)
  "The client of the test daemon that came as NUMBER, one on a screen of its own."
  (in-daemon (lambda ()
               (find-if (lambda (client)
                          (and (tuatara.command:client-remote client) (eql (tuatara.command:client-number client) number)))
                        (tuatara.command:editor-clients (daemon-editor))))))

(defun client-in-daemon-p (client)
  "True when CLIENT is still one of the test daemon's clients."
  (in-daemon (lambda () (and (member client (tuatara.command:editor-clients (daemon-editor))) t))))

(defun call-with-ended-folders (function)
  "Call FUNCTION with a daemon's clients: the
fake screen of a client that stays, with the buffer list showing, and the
clients that ended, A (two files, alpha and beta) and B (gamma), each a client
structure; the folder of A ended an hour after B's."
  (with-test-directory (dir)
    (let ((alpha (namestring (put-file (merge-pathnames "alpha.txt" dir) "alpha text")))
          (beta (namestring (put-file (merge-pathnames "beta.txt" dir) "beta text")))
          (gamma (namestring (put-file (merge-pathnames "gamma.txt" dir) "gamma text")))
          (delta (namestring (put-file (merge-pathnames "delta.txt" dir) "delta text"))))
      (with-test-daemon (name)
        (multiple-value-bind (a a-runner) (attach-test-client name :directory dir :files (list alpha beta))
          (true (shows a "alpha text"))
          (multiple-value-bind (b b-runner) (attach-test-client name :directory dir :files (list gamma))
            (true (shows b "gamma text"))
            (multiple-value-bind (c c-runner)
                (attach-test-client name :directory dir :files (list delta) :width 100 :height 24)
              (true (shows c "delta text"))
              (type-keys c "C-x C-b")
              (true (shows c "This client"))
              (type-keys a "C-x C-c")
              (type-keys b "C-x C-c")
              (client-ends a-runner)
              (client-ends b-runner)
              (let ((ended-a (remote-client-numbered 1))
                    (ended-b (remote-client-numbered 2)))
                (in-daemon (lambda ()
                             (setf (tuatara.command:client-ended ended-b) (encode-universal-time 0 0 12 1 1 2026)
                                   (tuatara.command:client-ended ended-a) (encode-universal-time 0 0 13 1 1 2026))))
                (tuatara.command::wake-editor (daemon-editor))
                (funcall function c ended-a ended-b)
                (type-keys c "C-x C-c")
                (client-ends c-runner)))))))))

(define-test (daemon ended-folders-titled-faint-and-ordered)
  ;; a client that ended has a faint heading beginning Ended and its time; the live ones come first, then
  ;; the ended, the last to end first, then the folder of every client
  (call-with-ended-folders
   (lambda (c ended-a ended-b)
     (declare (ignore ended-a ended-b))
     (true (shows c "Ended 13:00 — client 1 since") "the heading says when it ended")
     (let ((this (row-showing c "This client"))
           (last-ended (row-showing c "Ended 13:00"))
           (first-ended (row-showing c "Ended 12:00"))
           (everyone (row-showing c "Every client's")))
       (true (and this last-ended first-ended everyone) "all four headings are shown")
       (when (and this last-ended first-ended everyone)
         (true (< this last-ended first-ended everyone) "live, the last to end, the first to end, every client's")
         (true (member :list-dim (faces-over c last-ended "Ended 13:00")) "the ended heading is faint")
         (false (member :list-dim (faces-over c this "This client")) "the live heading is not"))))))

(defun marked-for-deletion ()
  "The keys marked for deletion in *Buffers*; call it in the daemon."
  (tuatara.defaults::list-marked (tuatara.buffer:find-buffer "*Buffers*") :deletion))

(defun goto-folder-heading (client-screen folder)
  "Put point of the buffer list on the heading of FOLDER, a client, in the daemon."
  (declare (ignore client-screen))
  (in-daemon (lambda ()
               (let ((list (tuatara.buffer:find-buffer "*Buffers*")))
                 (tuatara.defaults::list-goto-entry list (list :section folder))))))

(define-test (daemon ended-folders-return-takes-up)
  ;; RET on the heading of a folder that ended takes its buffers into this client and the folder goes
  (call-with-ended-folders
   (lambda (c ended-a ended-b)
     (declare (ignore ended-b))
     (goto-folder-heading c ended-a)
     (type-keys c "RET")
     (true (wait-until (lambda () (not (client-in-daemon-p ended-a))) 10)
           "the folder is gone")
     (let ((alpha (in-daemon (lambda () (tuatara.buffer:find-buffer "alpha.txt")))))
       (true alpha)
       (true (in-daemon (lambda ()
                          (let ((clients (tuatara.command::buffer-clients alpha)))
                            (and (notany #'tuatara.command:client-ended clients) clients))))
             "alpha is a live client's now"))
     (true (wait-until (lambda () (not (row-showing c "Ended 13:00"))) 10) "the list is made again")
     (true (row-showing c "Ended 12:00") "the other folder stays"))))

(define-test (daemon ended-folders-d-marks-the-folder)
  ;; d on the heading of a folder that ended marks each of its buffers for deletion, no others
  (call-with-ended-folders
   (lambda (c ended-a ended-b)
     (declare (ignore ended-b))
     (flet ((marked ()
              (in-daemon (lambda ()
                           (sort (mapcar #'tuatara.buffer:buffer-name (marked-for-deletion)) #'string<)))))
       (goto-folder-heading c ended-a)
       (type-keys c "d")
       (true (wait-until (lambda () (marked)) 10) "buffers are marked")
       (is equal '("*lisp-scratch*" "alpha.txt" "beta.txt") (marked) "the folder's buffers, no others")
       (type-keys c "x")
       (true (wait-until (lambda () (not (in-daemon (lambda () (tuatara.buffer:find-buffer "alpha.txt"))))) 10)
             "the folder's buffers are killed")
       (true (in-daemon (lambda () (tuatara.buffer:find-buffer "gamma.txt"))) "the other folder's stays")))))

(define-test (daemon ended-folders-evidence-stays-closed-until-take-up)
  ;; a file waiting closed in a folder that ended is not opened by showing the list, folding or marking
  ;; the folder, only by RET on its heading, and then safely
  (call-with-ended-folders
   (lambda (c ended-a ended-b)
     (declare (ignore ended-b))
     (with-test-directory (dir)
       (let ((evidence (namestring (put-file (merge-pathnames "disk.img" dir) "evidence text"))))
         (in-daemon (lambda () (push (list ended-a (list :safe 0 evidence)) tuatara.defaults::*evidence-waiting*)))
         (goto-folder-heading c ended-a)
         (type-keys c "TAB")
         (type-keys c "d")
         (true (wait-until (lambda () (in-daemon (lambda () (marked-for-deletion)))) 10)
               "the folder is marked")
         (false (in-daemon (lambda () (tuatara.defaults::buffer-visiting evidence))) "closed while the list is used")
         (goto-folder-heading c ended-a)
         (type-keys c "RET")
         (true (wait-until (lambda () (in-daemon (lambda () (tuatara.defaults::buffer-visiting evidence)))) 10)
               "opened as the folder is taken up")
         (true (in-daemon (lambda () (tuatara.io:safe-visit-p (tuatara.defaults::buffer-visiting evidence))))
               "and safely"))))))

(define-test (daemon capital-d-kills-a-clients-folder)
  ;; D on the heading of a client's folder, ended or live, kills the buffers of that folder after asking
  (call-with-ended-folders
   (lambda (c ended-a ended-b)
     (declare (ignore ended-a))
     (flet ((buffer-there-p (name) (in-daemon (lambda () (and (tuatara.buffer:find-buffer name) t)))))
       (goto-folder-heading c ended-b)
       (type-keys c "D")
       (true (shows c "Kill the 2 buffers") "asked as for a project")
       (type-keys c "y RET")
       (true (wait-until (lambda () (not (buffer-there-p "gamma.txt"))) 10) "the folder's buffers are killed")
       (true (buffer-there-p "alpha.txt") "another folder's stay")
       (true (buffer-there-p "delta.txt") "this client's stay")))))
