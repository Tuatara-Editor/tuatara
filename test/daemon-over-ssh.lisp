;;;; test/daemon-over-ssh.lisp --- A daemon on another machine, reached through a stand-in for ssh.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The toolbox the tests run in has no ssh server, so ssh is played by
;;;; test/ssh-stand-in.py: it takes the command lines a client gives ssh,
;;;; runs the command "there" with a directory of the test's first on its PATH,
;;;; and as the connection that stays forwards the daemon's socket the way sshd
;;;; does, one process passing octets for every connection. The daemon runs in
;;;; this process, as in test/daemon.lisp.

(in-package #:tuatara.tests)

(defun stand-in-ssh-script ()
  "The stand-in for ssh, as a native name."
  (namestring (asdf:system-relative-pathname :tuatara "test/ssh-stand-in.py")))

(defmacro with-stand-in-ssh ((dir log bin &key (version "10.2p1")) &body body)
  "Run BODY with DIR bound to a new test directory, the home directory \"there\".
LOG is bound to the file the stand-in for ssh writes the command lines it is
given in; BIN to the directory first on the PATH of the commands it runs there;
and *SSH-PROGRAM* to the stand-in, saying it is OpenSSH VERSION. The stand-in
has ssh-agent and ssh-add too, writing to LOG.agent and LOG.add. The ssh
configuration it reads is ssh_config in DIR, when the test writes one."
  `(with-test-directory (,dir)
     (let* ((,log (namestring (merge-pathnames "ssh.log" ,dir)))
            (,bin (namestring (ensure-directories-exist (merge-pathnames "bin/" ,dir)))))
       (flet ((stand-in (name mode)
                (put-program (merge-pathnames name ,dir)
                             (format nil "#!/bin/sh~%STAND_IN_CONFIG='~assh_config' STAND_IN_VERSION='~a' ~
                                          exec python3 -I '~a' ~a '~a' \"$@\"~%"
                                     (namestring ,dir) ,version (stand-in-ssh-script) mode ,log))))
         (let ((tuatara.defaults::*ssh-program* (stand-in "ssh" (format nil "'~a'" ,bin)))
               (tuatara.terminal::*ssh-agent-program* (stand-in "ssh-agent" "--agent"))
               (tuatara.terminal::*ssh-add-program* (stand-in "ssh-add" "--add")))
           ,@body)))))

(defun put-server-tuatara (bin &key report (code 0) error)
  "Put a tuatara in BIN, the directory of programs \"there\", that writes the
arguments it was given to BIN's asked, REPORT on its standard output and
ERROR on its error output, and exits with CODE."
  (let ((program (merge-pathnames "tuatara" bin)))
    (put-program program (format nil "#!/bin/sh~%printf '%s\\n' \"$@\" > '~aasked'~%~@[cat <<'END'~%~aEND~%~]~
                                      ~@[echo '~a' >&2~%~]exit ~d~%"
                                 bin report error code))))

(defun test-report (home name &key (runs t) version gpg-socket gpg-autostart (unlink "yes") agent)
  "What tuatara --daemon-socket says of the test daemon NAME, HOME its home
directory, as WRITE-DAEMON-REPORT writes it, gpg's agent socket GPG-SOCKET,
GPG-AUTOSTART, UNLINK and AGENT as it takes them; with VERSION, as another build
of that version line would."
  (let ((text (with-output-to-string (out)
                (tuatara.terminal::write-daemon-report out :home (namestring home) :runs runs
                                                           :socket (tuatara.defaults::daemon-path name "socket")
                                                           :gpg-socket gpg-socket :gpg-autostart gpg-autostart
                                                           :unlink unlink :agent agent))))
    (if version
        (cl-ppcre:regex-replace "(?m)^version .*$" text (format nil "version ~a" version))
        text)))

(defun ssh-directories ()
  "The private directories clients of daemons on other machines hold now."
  (length (tuatara.io:directory-entries
           (tuatara.defaults::private-directory-in (tuatara.defaults::daemon-directory) "ssh/"))))

(defun far-test-client (link options &key window another cell-pixels)
  "A client through LINK, an SSH-LINK, asking for what OPTIONS (READ-OPTIONS)
ask, on a fake screen, a window when WINDOW, showing pictures when
CELL-PIXELS (a cell's size) is given, ANOTHER opening the windows the
daemon asks for; three values as ATTACH-TEST-CLIENT gives them."
  (let ((local (make-instance 'queued-fake-frontend :width 70 :height 14 :font-steps (and window 0)
                                                    :cell-pixels cell-pixels))
        (fd (tuatara.io:connect-to (tuatara.terminal::ssh-link-socket link))))
    (tuatara.frontend:frontend-start local)
    (let ((client (tuatara.frontend:make-remote-client
                   fd local :request (tuatara.terminal::client-request options "/dev/pts/7" :link link)
                            :another another)))
      (values local
              (sb-thread:make-thread (lambda () (multiple-value-list (tuatara.frontend:run-remote-client client)))
                                     :name "test far client")
              client))))

(defun far-options (&rest arguments)
  "The options of a client's command line, ARGUMENTS, read as READ-OPTIONS reads them."
  (tuatara.terminal::read-options arguments :directory "/nonexistent/"))

(defun said-on-error-output (function)
  "Call FUNCTION; two values: what it returned and what it wrote on the error output."
  (let* ((value nil)
         (said (with-output-to-string (*error-output*) (setf value (funcall function)))))
    (values value said)))

(define-test (daemon client-on-another-machine)
  ;; a client reaches the daemon through ssh, opens a file named from the home directory there, types and paints;
  ;; the daemon is told the client is far and sees ssh's process; C-x C-c ends the client alone with 0, and the
  ;; connection is closed, its directory deleted
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-file (merge-pathnames "kiwi.txt" dir) "far text")
      (with-test-daemon (name thread)
        (put-server-tuatara bin :report (test-report dir name))
        (let ((before (ssh-directories))
              (link (tuatara.terminal::open-ssh-link "build.example" name)))
          (true link)
          (when link
            (unwind-protect
                 (multiple-value-bind (local runner)
                     (far-test-client link (far-options (format nil "--client=build.example:~a" name) "kiwi.txt"))
                   (true (shows local "far text"))
                   (in-daemon
                    (lambda ()
                      (let* ((client (find-if #'tuatara.command:client-far-p
                                              (tuatara.command:editor-clients (daemon-editor))))
                             (remote (and client (tuatara.command:client-frontend client))))
                        (true client "the daemon knows the client is far")
                        (when remote
                          (false (tuatara.frontend:frontend-terminal-device remote) "no terminal of the desk")
                          (isnt eql (sb-posix:getpid) (tuatara.frontend:remote-pid remote)
                                "ssh connects, not the client")
                          (is string= (namestring dir) (tuatara.command:client-directory client))))))
                   (type-keys local "i a b <escape>")
                   (true (shows local "abfar text"))
                   (type-keys local "C-x C-c")
                   (is equal '(0 nil) (client-ends runner))
                   (true (sb-thread:thread-alive-p thread) "the daemon goes on"))
              (tuatara.terminal::close-ssh-link link))
            (false (tuatara.io:path-kind (tuatara.terminal::ssh-link-directory link)) "the directory is deleted")
            (is = before (ssh-directories))
            (let ((said (uiop:read-file-string log)))
              (dolist (option '("StrictHostKeyChecking=yes" "ForwardAgent=no" "ForwardX11=no" "ClearAllForwardings=yes"
                                "PermitLocalCommand=no" "RemoteCommand=none" "ControlMaster=yes"))
                (true (search option said) "ssh was not given ~a" option))
              (true (search (format nil "--~%build.example~%tuatara~%--daemon-socket=~a~%env" name) said)
                    "the host after --, then the command")
              (true (search (format nil "-F~%none~%-O~%forward~%-L~%") said))
              (true (search (format nil "-F~%none~%-O~%exit~%--~%build.example~%env") said)
                    "the connection is ended"))))))))

(defmacro with-peers-namespace-kept-private ((by-ssh) &body body)
  "Run BODY with the mount namespace of a process no longer to be read, as
the system keeps sshd's, and that process started by the OpenSSH server when
BY-SSH is true, the daemon taken to run outside any container."
  `(let ((by-ssh ,by-ssh))
     (unwind-protect
          (progn (sb-int:encapsulate 'tuatara.io:mount-namespace 'kept-private
                                     (lambda (function pid) (declare (ignore function pid)) nil))
                 (sb-int:encapsulate 'tuatara.io:started-by-ssh-server-p 'kept-private
                                     (lambda (function pid) (declare (ignore function pid)) by-ssh))
                 (sb-int:encapsulate 'tuatara.defaults::daemon-knows-root-p 'kept-private
                                     (lambda (function) (declare (ignore function)) t))
                 ,@body)
       (sb-int:unencapsulate 'tuatara.defaults::daemon-knows-root-p 'kept-private)
       (sb-int:unencapsulate 'tuatara.io:started-by-ssh-server-p 'kept-private)
       (sb-int:unencapsulate 'tuatara.io:mount-namespace 'kept-private))))

(defun started-by-ssh-given (parents name user &optional (process-numbers '(7)))
  "What TUATARA.IO:STARTED-BY-SSH-SERVER-P says of a process whose parent's
number reads as each of PARENTS in turn, the last for good, a parent named NAME,
run by the account USER, with PROCESS-NUMBERS its numbers in each namespace of
processes."
  (let ((left parents))
    (unwind-protect
         (progn
           (sb-int:encapsulate 'tuatara.io::process-parent 'given
                               (lambda (function pid)
                                 (declare (ignore function pid))
                                 (if (rest left) (pop left) (first left))))
           (sb-int:encapsulate 'tuatara.io::process-name 'given
                               (lambda (function pid) (declare (ignore function pid)) name))
           (sb-int:encapsulate 'tuatara.io::process-status-numbers 'given
                               (lambda (function pid field)
                                 (declare (ignore function pid))
                                 (if (string= field "Uid") (and user (list user)) process-numbers)))
           (tuatara.io:started-by-ssh-server-p 42))
      (sb-int:unencapsulate 'tuatara.io::process-status-numbers 'given)
      (sb-int:unencapsulate 'tuatara.io::process-name 'given)
      (sb-int:unencapsulate 'tuatara.io::process-parent 'given))))

(define-test (daemon ssh-server-is-a-parent-run-by-root)
  (true (started-by-ssh-given '(7) "sshd-session" 0))
  (true (started-by-ssh-given '(7) "sshd" 0))
  (false (started-by-ssh-given '(7) "sshd-session" 1000))
  (false (started-by-ssh-given '(7) "sshd" nil))
  (false (started-by-ssh-given '(7) "sshd" 0 '(7 1)))
  (false (started-by-ssh-given '(7) "sshd" 0 nil))
  (false (started-by-ssh-given '(7) "dropbear" 0))
  (false (started-by-ssh-given '(nil) "sshd" 0))
  (false (started-by-ssh-given '(7 1) "sshd" 0)))

(define-test (daemon peer-whose-namespace-cannot-be-read)
  ;; sshd connects for a client on another machine and the system will not say its namespace: such a client is
  ;; taken; one on this machine is refused, its paths not known to be the daemon's
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-file (merge-pathnames "kiwi.txt" dir) "far text")
      (with-test-daemon (name thread)
        (put-server-tuatara bin :report (test-report dir name))
        (with-peers-namespace-kept-private (t)
          (let ((link (tuatara.terminal::open-ssh-link "build.example" name)))
            (true link)
            (when link
              (unwind-protect
                   (multiple-value-bind (local runner)
                       (far-test-client link (far-options (format nil "--client=build.example:~a" name) "kiwi.txt"))
                     (true (shows local "far text"))
                     (type-keys local "C-x C-c")
                     (is equal '(0 nil) (client-ends runner)))
                (tuatara.terminal::close-ssh-link link))))
          (multiple-value-bind (local runner) (attach-test-client name :directory dir)
            (declare (ignore local))
            (let ((ended (client-ends runner)))
              (is eql 1 (first ended))
              (true (search "container" (second ended)))))))
      ;; one the OpenSSH server did not start is refused, whatever it says of itself
      (with-test-daemon (name thread)
        (with-peers-namespace-kept-private (nil)
          (true (search "container" (handler-case (progn (attach-test-client name :directory dir :far t) "")
                                      (tuatara.frontend:remote-refused (condition)
                                        (tuatara.frontend:remote-refused-words condition)))))))
      ;; nor can a daemon in a container of user ids tell root from a process that only looks like it there
      (with-test-daemon (name thread)
        (with-peers-namespace-kept-private (t)
          (unwind-protect
               (progn
                 (sb-int:encapsulate 'tuatara.defaults::daemon-knows-root-p 'in-a-container
                                     (lambda (function) (declare (ignore function)) nil))
                 (true (search "container" (handler-case (progn (attach-test-client name :directory dir :far t) "")
                                             (tuatara.frontend:remote-refused (condition)
                                               (tuatara.frontend:remote-refused-words condition))))))
            (sb-int:unencapsulate 'tuatara.defaults::daemon-knows-root-p 'in-a-container))))
      ;; a namespace that can be read and differs is refused, whatever the client says of itself
      (with-test-daemon (name thread)
        (unwind-protect
             (progn
               (sb-int:encapsulate 'tuatara.io:mount-namespace 'another
                                   (lambda (function pid) (declare (ignore function pid)) (cons 1 2)))
               (true (search "container" (handler-case (progn (attach-test-client name :directory dir :far t) "")
                                           (tuatara.frontend:remote-refused (condition)
                                             (tuatara.frontend:remote-refused-words condition))))))
          (sb-int:unencapsulate 'tuatara.io:mount-namespace 'another))))))

(define-test (daemon client-on-another-machine-shows-a-picture)
  ;; a picture file opened on a daemon through ssh is shown on the client's screen, its pixels sent across
  (when (and (tuatara.io:find-program "python3") (tuatara.defaults::find-image-decoder))
    (with-stand-in-ssh (dir log bin)
      (alexandria:write-byte-vector-into-file *tiny-png* (merge-pathnames "photo.png" dir))
      (with-test-daemon (name)
        (put-server-tuatara bin :report (test-report dir name))
        (let ((link (tuatara.terminal::open-ssh-link "build.example" name)))
          (true link)
          (when link
            (unwind-protect
                 (multiple-value-bind (local runner)
                     (far-test-client link (far-options (format nil "--client=build.example:~a" name) "photo.png")
                                      :cell-pixels '(8 16))
                   (true (tuatara.runtime:wait-until (lambda () (tuatara.frontend.fake:fake-images local)) 30)
                         "the picture is drawn on the client")
                   (let ((image (first (first (tuatara.frontend.fake:fake-images local)))))
                     (true (plusp (length (tuatara.frontend:image-pixels image))) "with its pixels"))
                   (type-keys local "C-x C-c")
                   (is equal '(0 nil) (client-ends runner)))
              (tuatara.terminal::close-ssh-link link))
            (true (search "ControlMaster=yes" (uiop:read-file-string log)) "through ssh")))))))

(define-test (daemon window-client-on-another-machine-opens-another-window)
  ;; C-x 5 2 in a window client of a daemon on another machine: the window connects through the same ssh
  ;; connection, which the daemon takes as the client's own
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-file (merge-pathnames "kaka.txt" dir) "kaka text")
      (with-test-daemon (name)
        (put-server-tuatara bin :report (test-report dir name))
        (let ((link (tuatara.terminal::open-ssh-link "build.example" name))
              (opened (list '())))
          (true link)
          (when link
            (unwind-protect
                 (progn
                   (multiple-value-bind (local runner)
                       (far-test-client
                        link (far-options (format nil "--client=build.example:~a" name) "--gui" "kaka.txt")
                        :window t
                        :another (lambda (client word)
                                   (declare (ignore client))
                                   (let* ((window (make-instance 'queued-fake-frontend :width 70 :height 14
                                                                                       :font-steps 0))
                                          (fd (tuatara.io:connect-to (tuatara.terminal::ssh-link-socket link))))
                                     (tuatara.frontend:frontend-start window)
                                     (let ((other (tuatara.frontend:make-remote-client
                                                   fd window :request (tuatara.frontend:make-request
                                                                       :directory (namestring dir) :joins word
                                                                       :far t))))
                                       (push (list window (sb-thread:make-thread
                                                           (lambda () (multiple-value-list
                                                                       (tuatara.frontend:run-remote-client other)))))
                                             (car opened))))))
                     (true (shows local "kaka text"))
                     (type-keys local "C-x 5 2")
                     (true (wait-until (lambda () (car opened)) 10) "no other window was opened")
                     (destructuring-bind (&optional second second-runner) (first (car opened))
                       (when second
                         (true (shows second "kaka text"))
                         (type-at second (tuatara.frontend:make-event :close))
                         (is equal '(0 nil) (client-ends second-runner))))
                     (type-keys local "C-x C-c")
                     (is equal '(0 nil) (client-ends runner))))
              (tuatara.terminal::close-ssh-link link))))))))

(define-test (daemon another-machine-said-plainly)
  ;; no ssh connection, no Tuatara there, an older one, another build, no daemon: each said, nothing left open
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (with-test-daemon (name)
        ;; the directory of a client that crashed, its lock let go, is swept as the next one starts
        (let ((dead (multiple-value-bind (directory lock)
                        (tuatara.defaults::make-owned-directory "ssh/" :base (tuatara.defaults::daemon-directory))
                      (sb-posix:close lock)
                      directory))
              (before (ssh-directories)))
          (flet ((refused (host &key start)
                   (multiple-value-bind (link said)
                       (said-on-error-output (lambda () (tuatara.terminal::open-ssh-link host name :start start)))
                     (false link)
                     (when link (tuatara.terminal::close-ssh-link link))
                     said)))
            (let ((said (refused "nowhere.invalid")))
              (true (search "could not reach nowhere.invalid" said))
              ;; a name that does not resolve is not about the host key
              (false (search "check its key" said)))
            (true (search "has no tuatara on the PATH" (refused "build.example")))
            (put-server-tuatara bin :code 2 :error "tuatara: --daemon-socket=x is not an option of tuatara")
            (true (search "too old to be shown from another machine" (refused "build.example")))
            (put-server-tuatara bin :report (test-report dir name :version "Tuatara 0.0.1 (a1b2c3d)"))
            (true (search "runs Tuatara 0.0.1 (a1b2c3d)" (refused "build.example")))
            (put-server-tuatara bin :report (test-report dir name :runs nil))
            (let ((said (refused "build.example")))
              (true (search (format nil "no daemon named ~a runs on build.example" name) said))
              (true (search "--start starts one" said)))
            ;; --start is asked of the Tuatara there
            (refused "build.example" :start t)
            (is equal (list (format nil "--daemon-socket=~a" name) "--start")
                (uiop:read-file-lines (merge-pathnames "asked" bin))))
          (false (tuatara.io:path-kind dead) "a crashed client's directory is swept")
          (is = (1- before) (ssh-directories) "no directory is left")
          ;; each connection made is ended: all but the one to nowhere.invalid
          (let ((lines (uiop:split-string (uiop:read-file-string log) :separator '(#\Newline))))
            (is = (1- (count "ControlMaster=yes" lines :test #'string=)) (count "exit" lines :test #'string=))))))))

(define-test (daemon another-machine-old-version-said)
  ;; an old tuatara there: which program it found and its version, not only that it is old
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (with-test-daemon (name)
        (put-program (merge-pathnames "tuatara" bin)
                     (format nil "#!/bin/sh~%[ \"$1\" = --version ] && echo 'Tuatara 0.90.14 (abc, built 2026) on SBCL' ~
                                  && exit 0~%echo 'not an option of tuatara' >&2~%exit 2~%"))
        (let ((said (nth-value 1 (said-on-error-output
                                  (lambda ()
                                    (let ((link (tuatara.terminal::open-ssh-link "build.example" name)))
                                      (when link (tuatara.terminal::close-ssh-link link))))))))
          (true (search (format nil "tuatara on build.example is 0.90.14; this is ~a" (tuatara:version)) said)))))))

(define-test (daemon another-machine-same-version-keeps-refusal)
  ;; a tuatara there of this very version that was refused is not called old: the real refusal is said
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (with-test-daemon (name)
        (put-program (merge-pathnames "tuatara" bin)
                     (format nil "#!/bin/sh~%[ \"$1\" = --version ] && echo 'Tuatara ~a (abc, built 2026) on SBCL' ~
                                  && exit 0~%echo 'not an option of tuatara' >&2~%exit 2~%"
                             (tuatara:version)))
        (let ((said (nth-value 1 (said-on-error-output
                                  (lambda ()
                                    (let ((link (tuatara.terminal::open-ssh-link "build.example" name)))
                                      (when link (tuatara.terminal::close-ssh-link link))))))))
          (false (search "this is" said))
          (true (search "too old to be shown" said)))))))

(define-test (daemon another-machine-program-named)
  ;; the trust names the tuatara run there; the question to ssh for a new host is asked only when it can be answered
  (let ((link (tuatara.terminal::make-ssh-link "box" "ssh" "/nowhere/" nil (list :program "/opt/t/tuatara"))))
    (is string= "/opt/t/tuatara" (tuatara.terminal::link-tuatara link))
    (is string= "tuatara" (tuatara.terminal::link-tuatara (tuatara.terminal::make-ssh-link "b" "ssh" "/n/" nil nil)))
    (is equal "StrictHostKeyChecking=ask"
        (third (member "ControlPersist=no" (tuatara.terminal::ssh-arguments link :master t :ask-host t)
                       :test #'string=))
        "ask comes before the shared options, as ssh takes the first of an option")
    (false (member "StrictHostKeyChecking=ask" (tuatara.terminal::ssh-arguments link :master t) :test #'string=)))
  (is eq :terminal (tuatara.terminal::master-asker :terminal t :window "/bin/ask"))
  (is equal "/bin/ask" (tuatara.terminal::master-asker :terminal nil :window "/bin/ask"))
  (false (tuatara.terminal::master-asker :terminal nil :window nil)))

(define-test (daemon another-machine-options-read)
  ;; HOST[:NAME] names a daemon on another machine; a plain name one here; a host ssh could read as an option,
  ;; standard input and --fallback with a host are refused
  (let ((options (far-options "--client=build.example" "notes.org")))
    (is string= "build.example" (getf options :host))
    (is string= "default" (getf options :client)))
  (let ((options (far-options "--client=me@build.example:work")))
    (is string= "me@build.example" (getf options :host))
    (is string= "work" (getf options :client)))
  (let ((options (far-options "--client=build:" "--gui")))
    (is string= "build" (getf options :host))
    (is string= "default" (getf options :client)))
  (is string= "build" (getf (far-options "--stop=build:work") :host))
  (let ((options (far-options "--client=work")))
    (false (getf options :host))
    (is string= "work" (getf options :client)))
  (true (getf (far-options "--client=build:" "--start") :start))
  (dolist (wrong '(("--client=-oProxyCommand=x:") ("--client=me@-oProxyCommand=x") ("--client=:work")
                   ("--client=a b.example") ("--client=build.example:a/b") ("--client=a@b@c.example")
                   ("--client=build:" "-") ("--client=build:" "--fallback") ("--daemon=build.example")))
    (true (getf (apply #'far-options wrong) :error) "~s taken" wrong))
  (true (search "no \"-\" first" (getf (far-options "--client=-x.example") :error)))
  ;; the question to the Tuatara there takes --start alone
  (is string= "work" (getf (far-options "--daemon-socket=work" "--start") :daemon-socket))
  (dolist (wrong '(("--daemon-socket" "a.txt") ("--daemon-socket" "--gui") ("--daemon-socket" "--client")))
    (true (getf (apply #'far-options wrong) :error) "~s taken" wrong)))

(define-test (daemon another-machine-report)
  ;; what tuatara --daemon-socket says is read back; anything else, or a socket ssh could not forward, is not
  (with-test-daemon (name)
    (let* ((said (with-output-to-string (*standard-output*)
                   (is = 0 (tuatara.terminal::report-daemon-socket name nil))))
           (report (tuatara.terminal::read-daemon-report said)))
      (true report)
      (true (getf report :runs))
      (is string= (tuatara.defaults::daemon-path name "socket") (getf report :socket))
      (is string= (tuatara:version-line) (getf report :version))
      ;; what a shell's start files print first is passed over
      (true (tuatara.terminal::read-daemon-report (format nil "Welcome to build~%~a" said)))
      (false (tuatara.terminal::read-daemon-report (subseq said 0 (search "runs" said))))
      (false (tuatara.terminal::read-daemon-report (cl-ppcre:regex-replace "\\.socket" said ":x.socket")))
      (false (tuatara.terminal::read-daemon-report "tuatara: --daemon-socket is not an option"))))
  (let ((said (with-output-to-string (*standard-output*)
                (tuatara.terminal::report-daemon-socket "test-none-runs" nil))))
    (false (getf (tuatara.terminal::read-daemon-report said) :runs))))

(define-test (daemon far-client-gives-the-desk-nothing)
  ;; a client of a daemon on another machine gives it no clipboard and takes none, lends no terminal and names
  ;; no dropped file
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let* ((local (make-instance 'lending-fake-frontend :cell-pixels '(8 16)))
           (maker (progn (tuatara.frontend:frontend-start local)
                         (setf (tuatara.frontend.fake:fake-clipboard local) "the desk's secret")
                         (sb-thread:make-thread
                          (lambda () (tuatara.frontend:make-remote-client
                                      theirs local :request (tuatara.frontend:make-request :directory "/" :far t))))))
           (queue (tuatara.frontend:make-event-queue))
           (remote (tuatara.frontend:make-remote-frontend mine :queue queue))
           (client (sb-thread:join-thread maker))
           (runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client))))
           (ran nil))
      (true (tuatara.frontend:request-far (tuatara.frontend:remote-request remote)))
      (false (tuatara.frontend:frontend-clipboard remote) "the desk's clipboard is not given")
      (tuatara.frontend:frontend-set-clipboard remote "planted")
      (tuatara.frontend:frontend-lend-terminal remote (lambda () (setf ran t)) :words "gpg asks here")
      (true ran)
      ;; asked all the same, as a daemon on a machine broken into could
      (false (eql 1 (tuatara.frontend::ask-client remote :lend "gpg asks here" 2)))
      (false (lending-lent local) "no terminal lent")
      (type-at local (tuatara.frontend:make-event :drop :files '("/home/me/evidence.txt") :column 0 :row 0)
               (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\x)))
      (let ((event (tuatara.frontend:queue-pop queue 5)))
        (is eq :key (and event (tuatara.frontend:event-kind event)) "the drop was not sent"))
      (is string= "the desk's secret" (tuatara.frontend.fake:fake-clipboard local) "nothing planted")
      (tuatara.frontend:frontend-stop remote)
      (client-done runner))))

(define-test (daemon far-client-passes-no-descriptor)
  ;; a client saying it is far that names a terminal, or passes its standard input, is refused
  (dolist (request (list (tuatara.frontend:make-request :directory "/" :far t :terminal "/dev/pts/3")
                         (tuatara.frontend:make-request :directory "/" :far t :standard-input t)))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (let* ((local (make-instance 'queued-fake-frontend))
             (maker (progn (tuatara.frontend:frontend-start local)
                           (sb-thread:make-thread
                            (lambda () (handler-case (tuatara.frontend:make-remote-client theirs local :request request)
                                         (tuatara.frontend:remote-refused () nil)))))))
        (let ((words (handler-case (progn (tuatara.frontend:make-remote-frontend mine) "taken")
                       (tuatara.frontend:remote-refused (condition)
                         (tuatara.frontend:remote-refused-words condition)))))
          (true (search "another machine" words) "~s refused with ~s" request words))
        (tuatara.frontend:shut-and-close mine)
        ;; the client's reading thread ended before its socket's number can be another's
        (let ((client (sb-thread:join-thread maker :timeout 10 :default nil)))
          (if client
              (tuatara.frontend::close-remote-client client)
              (tuatara.frontend:shut-and-close theirs)))))))

(define-test (daemon far-client-variables)
  ;; of a far client's variables, none naming its machine is kept, and gpg asks only through its agent there
  (is equal '(("TERM" . "xterm") ("LANG" . "mi_NZ.UTF-8"))
      (tuatara.defaults::kept-client-environment
       '(("TERM" . "xterm") ("DISPLAY" . ":9") ("WAYLAND_DISPLAY" . "wayland-1") ("SSH_AUTH_SOCK" . "/tmp/a")
         ("PATH" . "/desk/bin") ("LANG" . "mi_NZ.UTF-8"))
       t))
  (is equal '(("DISPLAY" . ":9")) (tuatara.defaults::kept-client-environment '(("DISPLAY" . ":9"))))
  (with-test-daemon (name)
    (dolist (far '(nil t))
      (multiple-value-bind (local runner)
          (attach-test-client name :environment '(("TERM" . "xterm") ("DISPLAY" . ":9")) :far far)
        (true (shows local "scratch"))
        (in-daemon (lambda ()
                     (let ((client (tuatara.command:current-client (daemon-editor)))
                           (tuatara.defaults::*gpg-passphrase-from* :minibuffer))
                       (is eq far (tuatara.command:client-far-p client))
                       (is equal (if far '(("TERM" . "xterm")) '(("TERM" . "xterm") ("DISPLAY" . ":9")))
                           (tuatara.command:client-environment client))
                       (is eq (if far :agent :minibuffer) (tuatara.defaults::gpg-asker)
                           "a passphrase is asked in the editor only for a client on this machine"))))
        (type-keys local "C-x C-c")
        (client-ends runner)))))

(define-test (daemon another-machine-takes-nothing-of-the-desk)
  ;; the ssh configuration's forwards never join the connection's; the command there reads nothing typed here;
  ;; ssh is given only the variables it needs, and the PATH of the system's programs
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-file (merge-pathnames "ssh_config" dir) (format nil "Host *~%  RemoteForward 9000 localhost:9000~%"))
      (with-test-daemon (name)
        (put-server-tuatara bin :report (test-report dir name))
        (let ((link (with-environment-variable ("TUATARA_TEST_DESK_SECRET" "kept here")
                      (tuatara.terminal::open-ssh-link "build.example" name))))
          (true link)
          (when link (tuatara.terminal::close-ssh-link link))
          (let ((forwards (uiop:read-file-lines (concatenate 'string log ".forwards")))
                (said (uiop:read-file-string log)))
            (is = 1 (length forwards) "only the daemon's socket is forwarded: ~s" forwards)
            (true (search (format nil "-n~%-F~%none~%-o~%BatchMode=yes") said) "the command there reads nothing")
            (true (search "GSSAPIDelegateCredentials=no" said))
            (false (search "TUATARA_TEST_DESK_SECRET" said) "a variable ssh does not need is not given")
            (true (search " HOME" said))
            (is equal (list (format nil "PATH=~a" tuatara.io:*sandbox-path*))
                (remove-if-not (lambda (setting) (uiop:string-prefix-p "PATH=" setting))
                               (tuatara.defaults::ssh-environment)))))))))

(define-test (daemon another-machine-said-escaped)
  ;; what the other machine writes on its error output is shown with its control characters written out, so
  ;; that it can neither set this terminal's clipboard nor draw a prompt of ssh's
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-program (merge-pathnames "tuatara" bin)
                   (format nil "#!/bin/sh~%printf '\\033]52;c;cGxhbnRlZA==\\007Password: ' >&2~%exit 1~%"))
      (with-test-daemon (name)
        (multiple-value-bind (link said)
            (said-on-error-output (lambda () (tuatara.terminal::open-ssh-link "build.example" name)))
          (false link)
          (false (find (code-char 27) said) "no escape reaches the terminal")
          (false (find (code-char 7) said))
          (true (search "build.example: ^[]52;c;cGxhbnRlZA==^GPassword: " said)))))))

(define-test (daemon another-machine-answers-in-time)
  ;; a machine that does not answer is given up once the time is up, its connection ended; a program there
  ;; still holding ssh's output open leaves nothing reading it here, whose descriptor another could be given
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-program (merge-pathnames "tuatara" bin) (format nil "#!/bin/sh~%setsid sleep 8 &~%sleep 30~%"))
      (with-test-daemon (name)
        (let ((before (ssh-directories)) (started (get-internal-real-time))
              (threads (length (sb-thread:list-all-threads))))
          (multiple-value-bind (link said)
              (said-on-error-output (lambda () (let ((tuatara.terminal::*ssh-answer-seconds* 2))
                                                 (tuatara.terminal::open-ssh-link "build.example" name))))
            (false link)
            (true (search "did not say where its daemon listens within 2 seconds" said))
            (true (< (- (get-internal-real-time) started) (* 10 internal-time-units-per-second)))
            (true (<= (length (sb-thread:list-all-threads)) threads) "no thread is left reading")
            (is = before (ssh-directories))))))))

(define-test (daemon another-machine-plain-paths)
  ;; a socket or home directory that is not a plain path is refused: ssh would expand %u or ~ in it here
  (dolist (path '("/run/user/1000/tuatara/default.socket" "/home/kiwi/" "/a-b_c.d/e"))
    (true (tuatara.defaults::plain-path-p path) "~a" path))
  (dolist (path (list "/run/user/%u/x.socket" "/home/~kiwi/" "/a b" "/a:b" "relative" ""
                     (format nil "/caf~a" (code-char 233))))
    (false (tuatara.defaults::plain-path-p path) "~a" path))
  (flet ((report (home socket)
           (format nil "tuatara-daemon ~d~%version ~a~%home ~a~%socket ~a~%runs yes~%gpg-socket -~%gpg-autostart no~%~
                        unlink no~%agent -~%"
                   tuatara.frontend::+protocol+ (tuatara:version-line) home socket)))
    (true (tuatara.terminal::read-daemon-report (report "/home/me/" "/run/user/1/d.socket")))
    (false (tuatara.terminal::read-daemon-report (report "/home/me/" "/run/user/%u/d.socket")))
    (false (tuatara.terminal::read-daemon-report (report "/home/%d/" "/run/user/1/d.socket")))))

(define-test (daemon password-sent-to-another-machine)
  ;; a password asked in the frame of a client on another machine says it is typed into that machine
  (with-test-daemon (name)
    (dolist (far '(nil t))
      (multiple-value-bind (local runner) (attach-test-client name :far far)
        (true (shows local "scratch"))
        (in-daemon (lambda ()
                     (let ((prompt (tuatara.command::password-prompt "Password for a.zip: ")))
                       (if far
                           (is string= (format nil "Password for a.zip (sent to ~a): " (machine-instance)) prompt)
                           (is string= "Password for a.zip: " prompt)))))
        (type-keys local "C-x C-c")
        (client-ends runner)))))

(define-test (daemon unreached-host-key-hint)
  (true (search "check its key" (tuatara.terminal::unreached-words
                                 "far.example" "Host key verification failed.")))
  (false (search "check its key" (tuatara.terminal::unreached-words
                                  "far.example" "ssh: Could not resolve hostname far.example"))))

(define-test (daemon failure-window-lines)
  (is equal '("one two" "three" "" "abcdefghij" "k")
      (tuatara.terminal::window-lines (list "one two three" "" "abcdefghijk") 10)))

(define-test (daemon ssh-line-ends-not-shown)
  (let ((said (with-output-to-string (*error-output*)
                (tuatara.terminal::say-what-ssh-said
                 "far.example" (format nil "ssh: Could not resolve hostname far.example~c~%" #\Return)))))
    (true (search "far.example: ssh: Could not resolve" said))
    (false (search "^M" said))))

