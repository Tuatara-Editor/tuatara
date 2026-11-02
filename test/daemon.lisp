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

(defun start-test-daemon ()
  "Start a daemon in this process, named for the tests, its editor on a
thread of its own, once it takes clients; three values: the daemon, the
thread, and its name."
  (let* ((name (format nil "test~d" (incf *test-daemons*)))
         (daemon (tuatara.defaults::open-daemon name))
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

(defmacro with-test-daemon ((name &optional (thread (gensym "THREAD"))) &body body)
  "Run BODY with NAME bound to the name of a daemon started in this process
\(START-TEST-DAEMON) and THREAD to the thread its editor runs on, stopped after."
  (let ((daemon (gensym "DAEMON")))
    `(multiple-value-bind (,daemon ,thread ,name) (start-test-daemon)
       (declare (ignore ,daemon))
       (unwind-protect (progn ,@body)
         (stop-test-daemon ,thread)))))

(defun attach-test-client (name &key (directory "/tmp/") files safe merge diff stop terminal environment standard-input
                                     (width 70) (height 14))
  "A client of the daemon NAME on a fake screen of WIDTH by HEIGHT, asking for
what a client started so asks for (FILES, each a path or (PATH LINE COLUMN),
SAFE, MERGE, DIFF, STOP, TERMINAL, ENVIRONMENT, STANDARD-INPUT, a descriptor
passed); three values: the fake screen, the thread running the client,
which returns its exit code and words as a list, and the client."
  (let ((local (make-instance 'queued-fake-frontend :width width :height height))
        (fd (tuatara.io:connect-to (tuatara.defaults::daemon-path name "socket"))))
    (tuatara.frontend:frontend-start local)
    (let ((client (tuatara.frontend:make-remote-client
                   fd local
                   :request (tuatara.frontend:make-request
                             :directory (namestring directory) :terminal terminal :environment environment
                             :files (mapcar (lambda (file) (if (consp file) file (list (namestring file) nil nil))) files)
                             :safe safe :merge merge :diff diff :stop stop :wait t
                             :standard-input (and standard-input t))
                   :standard-input standard-input)))
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
  (let ((runtime (sb-posix:getenv "XDG_RUNTIME_DIR"))
        (tuatara.defaults::*runtime-directory* nil))
    (unwind-protect
         (progn (sb-posix:unsetenv "XDG_RUNTIME_DIR")
                (fail (tuatara.defaults::daemon-folder) 'tuatara.runtime:refusal)
                (sb-posix:setenv "XDG_RUNTIME_DIR" "/tmp" 1)
                (fail (tuatara.defaults::daemon-folder) 'tuatara.runtime:refusal))
      (if runtime (sb-posix:setenv "XDG_RUNTIME_DIR" runtime 1) (sb-posix:unsetenv "XDG_RUNTIME_DIR"))))
  ;; the tests' own folder, made for them alone
  (let ((folder (tuatara.defaults::daemon-folder)))
    (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp folder :follow nil))))
    (true (search "/runtime/tuatara/" folder))))

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
                         (true (some #'tuatara.command:client-ended (tuatara.command:buffer-clients buffer)))))))))))

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
            (true (shows b "ended"))
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
  ;; tuatara --stop with no terminal asks nothing: the changes are kept as auto-saved copies
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "kept.txt" dir) "kept"))))
      (with-test-daemon (name thread)
        (multiple-value-bind (local runner) (attach-test-client name :directory dir :files (list file))
          (true (shows local "kept"))
          (type-keys local "i y")
          (true (shows local "ykept"))
          (is eql 0 (tuatara.terminal::run-client (list :stop name :files '()) :terminal nil))
          (is eql 1 (first (client-ends runner)))
          (true (not (eq :late (sb-thread:join-thread thread :timeout 30 :default :late))))
          (let ((copy (tuatara.defaults::auto-save-path file)))
            (unwind-protect (is string= "ykept" (file-text copy))
              (ignore-errors (delete-file copy)))))))))

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
        (true (tuatara.command:client-frontend ran) "done in the client's frame")
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
               (attach-test-client name :environment '(("PATH" . "/client/bin") ("LANG" . "mi_NZ.UTF-8")))
             (true (shows local "scratch"))
             (let ((environment (in-daemon (lambda ()
                                             (let ((editor (daemon-editor)))
                                               (tuatara.command:call-in-frame
                                                editor (first (tuatara.command:editor-frames editor))
                                                #'tuatara.io:program-environment))))))
               (true (member "PATH=/client/bin" environment :test #'string=))
               (true (member "LANG=mi_NZ.UTF-8" environment :test #'string=))
               (false (find-if (lambda (setting) (uiop:string-prefix-p "DISPLAY=" setting)) environment)))
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
                     ("--daemon=a/b") ("--daemon" "--client") ("--client" "--gui") ("--client" "-c" "init.lisp")))
      (true (getf (apply #'options wrong) :error) "~s taken" wrong))
    (true (search "-Q" (getf (options "--daemon" "-Q") :error)))))
