;;;; test/ssh-servers.lisp --- The trusted servers, and what a client gives each.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun servers-session (&rest steps)
  "Run an editor session on STEPS (AUTO-SAVE-SESSION), no modal keys; the frontend."
  (let ((tuatara-user:*modal-editing* nil))
    (auto-save-session steps :width 140 :height 24)))

(define-test (daemon servers-trusted-by-commands)
  ;; a server is trusted by server-trust with nothing given; in M-x server-list each thing is turned on after a
  ;; question, the agent's machines named, and D forgets it; a machine never trusted is given nothing
  (with-fresh-state
    (false (tuatara.defaults::server-permissions "build.example"))
    (servers-session (typed-command "server-trust") (typed-keys "me@build.example") "RET" (list :wait 0.1))
    (is equal '(:agent nil :destinations () :gpg nil :clipboard nil :files nil)
        (tuatara.defaults::server-permissions "me@build.example"))
    (false (tuatara.defaults::server-permissions "build.example") "a host is trusted as written")
    (let ((f (servers-session (typed-command "server-list") (list :wait 0.1) "A" (list :wait 0.1) "y" (list :wait 0.1)
                              "C" (list :wait 0.1) "y" (list :wait 0.1) "H" "C-a C-k"
                              (typed-keys "github.com, git@git.example") "RET" (list :wait 0.1)
                              "F" "C-a C-k" (typed-keys "in/drops") "RET" (list :wait 0.1))))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows
             f "may then use your ssh keys, each use confirmed here"))
      (true (tuatara.frontend.fake:fake-frame-showing f "me@build.example")))
    (is equal '(:agent t :destinations ("github.com" "git@git.example") :gpg nil :clipboard t :files "in/drops")
        (tuatara.defaults::server-permissions "me@build.example"))
    ;; turning one off asks nothing; n to a question leaves it off
    (servers-session (typed-command "server-list") (list :wait 0.1) "C" (list :wait 0.1) "G" (list :wait 0.1) "n"
                     (list :wait 0.1))
    (let ((now (tuatara.defaults::server-permissions "me@build.example")))
      (false (getf now :clipboard))
      (false (getf now :gpg)))
    ;; a folder that is not plain is refused, the permission left as it was
    (servers-session (typed-command "server-list") (list :wait 0.1) "F" (list :wait 0.1) "F" "C-a C-k"
                     (typed-keys "../x") "RET" (list :wait 0.1))
    (false (getf (tuatara.defaults::server-permissions "me@build.example") :files))
    (servers-session (typed-command "server-list") (list :wait 0.1) "D" (list :wait 0.1) "y" (list :wait 0.1))
    (false (tuatara.defaults::server-permissions "me@build.example"))))

(define-test (daemon servers-list-fails-closed)
  ;; a line of the list not understood in every field gives nothing, never a permission; a field a later
  ;; release adds is passed over
  (flet ((line (&rest fields) (reduce (lambda (a b) (concatenate 'string a (string #\Tab) b)) fields)))
    (is equal '("box" t ("github.com") nil t "Dropped" "2026-11-05")
        (tuatara.defaults::parse-server-line (line "box" "yes" "github.com" "no" "yes" "Dropped" "2026-11-05")))
    (dolist (bad (list (line "box" "maybe" "-" "no" "no" "-" "2026-11-05")
                       (line "-oProxyCommand=x" "yes" "-" "no" "no" "-" "2026-11-05")
                       (line "box" "yes" "-evil" "no" "no" "-" "2026-11-05")
                       (line "box" "no" "-" "no" "no" "../up" "2026-11-05")
                       (line "box" "no" "-" "no" "no" "a b" "2026-11-05")
                       (line "box" "no" "-" "no" "no" "-")
                       (line "box" "no" "-" "no" "no" "-" "yesterday")))
      (false (tuatara.defaults::parse-server-line bad) "~s" bad))
    (is equal '("box" nil () nil nil nil "2026-11-05")
        (tuatara.defaults::parse-server-line (line "box" "no" "-" "no" "no" "-" "2026-11-05" "later"))))
  (true (tuatara.defaults::state-list-trust tuatara.defaults::*trusted-servers*))
  (is = 7 (tuatara.defaults::state-list-fields tuatara.defaults::*trusted-servers*)))

(defun trust-server (host &key agent destinations gpg clipboard files)
  "Put HOST on the list of trusted servers, trusted with what the keys say."
  (tuatara.defaults::state-list-put tuatara.defaults::*trusted-servers*
                                    (list (list host agent destinations gpg clipboard files "2026-11-05")))
  (tuatara.defaults::finish-state-writes))

(defmacro with-confirming-askpass ((dir) &body body)
  "Run BODY with an ssh-askpass of the test's own in DIR named by SSH_ASKPASS, shown with no display."
  `(let ((askpass (merge-pathnames "askpass" ,dir)))
     (put-program askpass (format nil "#!/bin/sh~%exit 0~%"))
     (with-environment-variable ("SSH_ASKPASS" (namestring askpass))
       (with-environment-variable ("SSH_ASKPASS_REQUIRE" "force")
         ,@body))))

(defun file-text-or-empty (path)
  "The text of the file at PATH, or an empty string when there is none."
  (if (probe-file path) (uiop:read-file-string path) ""))

(defun put-key-config (dir host &key alias key-alias)
  "Give HOST a key at ~/.ssh/keys/kea.key, HOME being DIR, in the ssh
configuration the stand-in reads, as an alias of ALIAS when given, known by
KEY-ALIAS in the known hosts when given; the key file's native name."
  (let ((key (merge-pathnames ".ssh/keys/kea.key" dir)))
    (ensure-directories-exist key)
    (put-file key "a key")
    (put-file (merge-pathnames "ssh_config" dir)
              (format nil "Host ~a~%~@[  HostName ~a~%~]~@[  HostKeyAlias ~a~%~]  IdentityFile ~~/.ssh/keys/kea.key~%~
                           Host github.com~%  HostName github.com~%"
                      host alias key-alias))
    (namestring key)))

(define-test (daemon agent-forwarded-to-a-trusted-server)
  ;; a server trusted with the agent is given an agent of the link's own, which ssh-add fills with the keys ssh's
  ;; configuration names for it (at a path of one's own, through an alias), to be confirmed at each use and
  ;; limited to the machines named as the known hosts have them; the agent ends with the link
  (when (tuatara.io:find-program "python3")
    (with-fresh-state
      (with-stand-in-ssh (dir log bin)
        (with-environment-variable ("HOME" (namestring dir))
          (let ((key (put-key-config dir "box" :alias "build.example" :key-alias "boxkey")))
            (trust-server "box" :agent t :destinations '("github.com" "git@git.example"))
            (with-confirming-askpass (dir)
              (with-test-daemon (name)
                (put-server-tuatara bin :report (test-report dir name))
                (multiple-value-bind (link said)
                    (said-on-error-output (lambda () (tuatara.terminal::open-ssh-link "box" name)))
                  (true link "~a" said)
                  (when link
                    (let ((socket (tuatara.terminal::ssh-link-agent-socket link))
                          (agent (tuatara.terminal::ssh-link-agent link))
                          (added (file-text-or-empty (format nil "~a.add" log))))
                      (true (search (format nil "ForwardAgent=~a" socket) (uiop:read-file-string log)))
                      (true (search (format nil "SSH_AUTH_SOCK=~a" socket) added) "ssh-add fills the link's agent")
                      (true (search (format nil "-c -h boxkey -h boxkey>github.com -h boxkey>git@git.example -- ~a"
                                            key)
                                    added)
                            "~a" added)
                      (true (search "asked 11" (file-text-or-empty (format nil "~a.agent" log))))
                      (tuatara.terminal::close-ssh-link link)
                      (false (sb-ext:process-alive-p agent) "the agent ends with the link")
                      (false (tuatara.io:path-kind socket :follow nil)))))))))))))

(define-test (daemon agent-forwarded-only-when-confirmed)
  ;; no agent is forwarded to a server not trusted with it; one that cannot ask for each use to be confirmed,
  ;; has no key named for it, cannot limit the machines named (OpenSSH before 8.9), has no setpriv, or whose
  ;; keys ssh-add cannot add, refuses the connection before ssh is run, the agent ended
  (when (tuatara.io:find-program "python3")
    (with-fresh-state
      (with-stand-in-ssh (dir log bin :version "8.8p1")
        (with-environment-variable ("HOME" (namestring dir))
          (with-test-daemon (name)
            (put-server-tuatara bin :report (test-report dir name))
            (trust-server "build.example" :clipboard t)
            (let ((link (tuatara.terminal::open-ssh-link "build.example" name)))
              (true link)
              (when link (tuatara.terminal::close-ssh-link link)))
            (true (search "ForwardAgent=no" (uiop:read-file-string log)))
            (false (probe-file (format nil "~a.add" log)) "no ssh-add for a server not trusted with the agent")
            (trust-server "build.example" :agent t)
            (flet ((refused (words)
                     (multiple-value-bind (link said)
                         (said-on-error-output (lambda () (tuatara.terminal::open-ssh-link "build.example" name)))
                       (false link)
                       (when link (tuatara.terminal::close-ssh-link link))
                       (true (search words said) "~a" said))))
              (with-environment-variable ("SSH_ASKPASS" nil)
                (let ((tuatara.io:*askpass-programs* (list (namestring (merge-pathnames "none" dir)))))
                  (refused "no ssh-askpass program here asks you to confirm each use")))
              (with-confirming-askpass (dir)
                (refused "none of the keys ssh -G build.example names (IdentityFile) is there")
                (put-key-config dir "build.example")
                (trust-server "build.example" :agent t :destinations '("github.com"))
                (refused "OpenSSH 8.8 here and cannot limit the machines")
                (trust-server "build.example" :agent t)
                (sb-int:encapsulate 'tuatara.io:sandbox-find-program 'no-setpriv
                                    (lambda (function name)
                                      (and (string/= name "setpriv") (funcall function name))))
                (unwind-protect (refused "setpriv is not installed (util-linux)")
                  (sb-int:unencapsulate 'tuatara.io:sandbox-find-program 'no-setpriv))
                (put-file (format nil "~a.add-fails" log) "")
                (refused "ssh-add could not put your keys in the agent forwarded to build.example")))
            (is = 1 (count-matches "ControlMaster=yes" (uiop:read-file-string log)) "ssh ran for the first alone")
            (true (search "ended" (file-text-or-empty (format nil "~a.agent" log))) "the agent was ended")
            (is = 0 (ssh-folders))))))))

(define-test (daemon agent-reaches-the-far-client-frames-alone)
  ;; the agent sshd brings for the question to the Tuatara there is named to the daemon, and the programs of that
  ;; client's frames alone are given it: another client's are not
  (when (tuatara.io:find-program "python3")
    (with-fresh-state
      (with-stand-in-ssh (dir log bin)
        (with-environment-variable ("HOME" (namestring dir))
          (put-key-config dir "build.example")
          (trust-server "build.example" :agent t)
          (with-confirming-askpass (dir)
            (with-test-daemon (name)
              (put-program (merge-pathnames "tuatara" bin)
                          (format nil "#!/bin/sh~%cat <<END~%~aEND~%"
                                  (test-report dir name :agent "$SSH_AUTH_SOCK")))
              (let ((link (tuatara.terminal::open-ssh-link "build.example" name)))
                (true link)
                (when link
                  (unwind-protect
                       (let ((there (tuatara.terminal::ssh-link-agent-there link)))
                         (true there "the Tuatara there said where the agent is")
                         (multiple-value-bind (local runner)
                             (far-test-client link (far-options "--client=build.example:"))
                           (declare (ignore runner))
                           (true (shows local "scratch"))
                           (multiple-value-bind (other other-runner) (attach-test-client name)
                             (declare (ignore other-runner))
                             (true (shows other "scratch"))
                             (in-daemon
                              (lambda ()
                                (dolist (client (tuatara.command:editor-clients (daemon-editor)))
                                  (let ((agent (cdr (assoc "SSH_AUTH_SOCK" (tuatara.command:client-environment client)
                                                           :test #'string=))))
                                    (if (tuatara.command:client-far-p client)
                                        (is equal there agent "the far client's frames have the agent")
                                        (false agent "no other client's frames have it")))))))
                           (let ((fd (tuatara.io:connect-to there)))
                             (true fd "the agent there answers")
                             (when fd (sb-posix:close fd)))
                           (type-keys local "C-x C-c")))
                    (tuatara.terminal::close-ssh-link link)))))))))))

(define-test (daemon killed-client-leaves-no-connection)
  ;; a client that ends without closing its connection, as one killed, takes the ssh connection and every forward
  ;; with it; the next client tells a connection left behind to end before its folder goes
  (when (tuatara.io:find-program "python3")
    (with-fresh-state
      (with-stand-in-ssh (dir log bin)
        (with-test-daemon (name)
          (put-server-tuatara bin :report (test-report dir name))
          (let ((link (let ((program tuatara.defaults::*ssh-program*))
                        (sb-thread:join-thread
                         (sb-thread:make-thread (lambda ()
                                                  (let ((tuatara.defaults::*ssh-program* program))
                                                    (tuatara.terminal::open-ssh-link "build.example" name)))
                                                :name "test client killed")
                         :timeout 30 :default nil))))
            (true link)
            (when link
              (let ((master (tuatara.terminal::ssh-link-master link)))
                (true (sb-ext:process-alive-p master) "ssh outlives the thread that asked for it")
                ;; as the client's process ends
                (tuatara.io::end-starters)
                (true (wait-until (lambda () (not (sb-ext:process-alive-p master))) 10)
                      "ssh ends with the thread kept to start it")
                (false (tuatara.io:connect-to (tuatara.terminal::ssh-link-socket link)) "no forward is left")
                (sb-ext:process-close master)
                (sb-posix:close (tuatara.terminal::ssh-link-lock link))))
            ;; a client that crashed with its connection still held: the next one ends it, then sweeps its folder
            (let ((left (tuatara.terminal::open-ssh-link "build.example" name)))
              (true left)
              (when left
                (sb-posix:close (tuatara.terminal::ssh-link-lock left))
                (let ((next (tuatara.terminal::open-ssh-link "build.example" name)))
                  (true next)
                  (when next (tuatara.terminal::close-ssh-link next)))
                (let ((master (tuatara.terminal::ssh-link-master left)))
                  (true (wait-until (lambda () (not (sb-ext:process-alive-p master))) 10)
                        "the connection left behind is told to end")
                  (sb-ext:process-close master))
                (false (tuatara.io:path-kind (tuatara.terminal::ssh-link-folder left)) "its folder is swept")))
            (when link
              (false (tuatara.io:path-kind (tuatara.terminal::ssh-link-folder link)) "the killed one's too"))))))))

(defun stand-in-gpgconf (dir &key extra home agent)
  "A gpg in DIR that does nothing, with a gpgconf beside it naming EXTRA as
gpg-agent's extra socket, HOME as GnuPG's folder and AGENT as its socket, and
starting nothing: the gpg's native name, for *GPG-PROGRAM*."
  (put-program (merge-pathnames "gpgconf" dir)
              (format nil "#!/bin/sh~%case \"$1 $2\" in~%  '--launch gpg-agent') ;;~%~
                           ~@[  '--list-dirs agent-extra-socket') echo '~a';;~%~]~
                           ~@[  '--list-dirs homedir') echo '~a';;~%~]~
                           ~@[  '--list-dirs agent-socket') echo '~a';;~%~]~
                             *) exit 1;;~%esac~%"
                      extra home agent))
  (put-program (merge-pathnames "gpg" dir) (format nil "#!/bin/sh~%exit 1~%")))

(define-test (daemon gpg-agent-forwarded-to-a-trusted-server)
  ;; gpg-agent's extra socket here is brought to where gpg looks for its agent there, for a server trusted with
  ;; it whose gpg starts no agent of its own; what to add to sshd's settings is said when it cannot be told; the
  ;; forward is cancelled as the link closes
  (when (tuatara.io:find-program "python3")
    (with-fresh-state
      (with-stand-in-ssh (dir log bin)
        ;; sockets in a folder of a short name, as the test folder's is too long for one
        (let* ((short (tuatara.defaults::private-directory-in (tuatara.defaults::daemon-folder)
                                                              (format nil "test-gpg-~d/" (sb-posix:getpid))))
               (extra (tuatara.io:path-join short "extra"))
               (there (tuatara.io:path-join short "agent"))
               (tuatara.defaults::*gpg-program* (stand-in-gpgconf dir :extra extra)))
          (trust-server "build.example" :gpg t)
          (with-test-daemon (name)
            (put-server-tuatara bin :report (test-report dir name :gpg-socket there :gpg-autostart t))
            (multiple-value-bind (link said)
                (said-on-error-output (lambda () (tuatara.terminal::open-ssh-link "build.example" name)))
              (false link)
              (true (search "add the line no-autostart to gpg.conf" said) "~a" said))
            (put-server-tuatara bin :report (test-report dir name :gpg-socket there :unlink "unknown"))
            (let ((listener (tuatara.io:listen-at extra)))
              (unwind-protect
                   (multiple-value-bind (link said)
                       (said-on-error-output (lambda () (tuatara.terminal::open-ssh-link "build.example" name)))
                     (true link "~a" said)
                     (true (search "add StreamLocalBindUnlink yes to /etc/ssh/sshd_config there" said))
                     (when link
                       ;; what the connection said is shown in the client's frame
                       (multiple-value-bind (local runner)
                           (far-test-client link (far-options "--client=build.example:"))
                         (declare (ignore runner))
                         (true (shows local "sshd on build.example may not remove") "said in the frame")
                         (type-keys local "C-x C-c"))
                       (let ((desk (sb-thread:make-thread
                                    (lambda ()
                                      (let ((stream (sb-sys:make-fd-stream (tuatara.io:accept-on listener)
                                                                           :output t :auto-close t)))
                                        (write-line "the desk's agent" stream)
                                        (close stream)))))
                             (fd (tuatara.io:connect-to there)))
                         (true fd "gpg there reaches the socket forwarded")
                         (when fd
                           (let ((stream (sb-sys:make-fd-stream fd :input t :auto-close t)))
                             (is string= "the desk's agent" (read-line stream nil ""))
                             (close stream)))
                         (sb-thread:join-thread desk :default nil :timeout 10))
                       (tuatara.terminal::close-ssh-link link)
                       (false (tuatara.io:path-kind there :follow nil) "the forward is gone")
                       (true (search (format nil "-O~%cancel~%-R~%~a:~a~%" there extra) (uiop:read-file-string log)))))
                (sb-posix:close listener)
                (uiop:delete-directory-tree (pathname short) :validate t)))))))))

(define-test (daemon ssh-programs-given-seconds)
  ;; ssh or a program beside it that takes too long is ended with what it started, though it pays no heed to
  ;; SIGTERM, and said to have taken too long: never waited for without end
  (with-test-directory (dir)
    (let ((program (put-program (merge-pathnames "slow" dir)
                                (format nil "#!/bin/sh~%trap '' TERM~%echo started~%sleep 60~%")))
          (began (get-internal-real-time)))
      (multiple-value-bind (code output complained) (tuatara.defaults::run-bounded program '() :seconds 1)
        (true (< (tuatara.runtime:seconds-since began) 5) "returned once its time was up")
        (false code "no exit code for a program ended")
        (is string= (format nil "started~%") output)
        (is string= "" complained)))))

(define-test (daemon gpg-and-sshd-said-for-another-machine)
  ;; tuatara --daemon-socket says where gpg looks for its agent, whether gpg starts one of its own (no
  ;; no-autostart in gpg.conf), and whether sshd removes a socket in the way, as its settings say outside Match
  (with-test-directory (dir)
    (let ((tuatara.defaults::*gpg-program*
            (stand-in-gpgconf dir :home (namestring dir) :agent (namestring (merge-pathnames "S.gpg-agent" dir)))))
      (is string= (namestring (merge-pathnames "S.gpg-agent" dir))
          (tuatara.terminal::gpgconf-directory "agent-socket"))
      (true (tuatara.terminal::gpg-autostart-p))
      (put-file (merge-pathnames "gpg.conf" dir) (format nil "# options~%  no-autostart  ~%"))
      (false (tuatara.terminal::gpg-autostart-p)))
    (let ((folder (ensure-directories-exist (merge-pathnames "sshd_config.d/" dir)))
          (main (merge-pathnames "sshd_config" dir)))
      (flet ((unlinks (main-text &optional conf)
               (put-file main main-text)
               (when conf (put-file (merge-pathnames "10-mine.conf" folder) conf))
               (let ((tuatara.terminal::*sshd-config-files* (list (namestring folder) (namestring main))))
                 (tuatara.terminal::sshd-unlinks))))
        (is string= "no" (unlinks (format nil "Port 22~%")))
        (is string= "yes" (unlinks (format nil "StreamLocalBindUnlink yes~%")))
        (is string= "yes" (unlinks (format nil "streamlocalbindunlink=yes~%")))
        (is string= "no" (unlinks (format nil "Match User kiwi~%  StreamLocalBindUnlink yes~%")))
        (is string= "no" (unlinks (format nil "StreamLocalBindUnlink yes~%") (format nil "StreamLocalBindUnlink no~%")))
        (delete-file (merge-pathnames "10-mine.conf" folder))
        (sb-posix:chmod (namestring main) 0)
        (let ((tuatara.terminal::*sshd-config-files* (list (namestring folder) (namestring main))))
          (is string= "unknown" (tuatara.terminal::sshd-unlinks)))
        (sb-posix:chmod (namestring main) #o600))))
  ;; a socket there that ssh could not forward is no socket of gpg's
  (let ((report (test-report "/home/me/" "default" :gpg-socket "/run/user/%u/S.gpg-agent")))
    (false (getf (tuatara.terminal::read-daemon-report report) :gpg-socket))
    (true (tuatara.terminal::read-daemon-report report))))

(define-test (daemon public-keys-copied-to-a-server)
  ;; public keys are imported into gpg's keyring there through ssh, which asks nothing and forwards nothing;
  ;; what gpg there did is said
  (when (tuatara.io:find-program "python3")
    (with-stand-in-ssh (dir log bin)
      (put-program (merge-pathnames "gpg" bin)
                  (format nil "#!/bin/sh~%printf '%s\\n' \"$@\" > '~aasked'~%cat > '~aimported'~%~
                               echo '[GNUPG:] IMPORT_OK 1 0123456789ABCDEF0123456789ABCDEF01234567'~%~
                               echo '[GNUPG:] IMPORT_RES 1 0 1 0 0 0 0 0 0 0 0 0 0 0'~%" bin bin))
      (let* ((key (format nil "-----BEGIN PGP PUBLIC KEY BLOCK-----~%kea~%-----END PGP PUBLIC KEY BLOCK-----~%"))
             (words (tuatara.defaults::import-on-server "build.example" (tuatara.text:string-octets key)))
             (said (uiop:read-file-string log)))
        (is string= "On build.example: Keys imported: 1 new, 0 unchanged" words)
        (is string= key (uiop:read-file-string (merge-pathnames "imported" bin)))
        (true (search (format nil "--batch~%--status-fd~%1~%--import")
                      (uiop:read-file-string (merge-pathnames "asked" bin))))
        (dolist (option '("BatchMode=yes" "StrictHostKeyChecking=yes" "ForwardAgent=no"))
          (true (search option said) "ssh was not given ~a" option))
        (true (search (format nil "--~%build.example~%gpg~%") said))))))

(defclass focusing-fake-frontend (lending-fake-frontend)
  ((focused :initform t :accessor focusing-focused :documentation "True while the window has the keyboard."))
  (:documentation "A client's fake screen whose window may be left for another."))

(defmethod tuatara.frontend::frontend-focused-p ((f focusing-fake-frontend))
  (focusing-focused f))

(define-test (daemon far-client-shares-what-it-is-let)
  ;; a far client let share the clipboard answers the daemon with it while its window has the keyboard, and
  ;; takes its copies then, with no control character, direction control or invisible one; left for another
  ;; window, it gives and takes nothing. One let have dropped files names them to the daemon with the paths
  ;; they were sent to there
  (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
    (let* ((local (make-instance 'focusing-fake-frontend :cell-pixels '(8 16)))
           (sent '())
           (maker (progn (tuatara.frontend:frontend-start local)
                         (setf (tuatara.frontend.fake:fake-clipboard local) "the desk's copy")
                         (sb-thread:make-thread
                          (lambda () (tuatara.frontend:make-remote-client
                                      theirs local :request (tuatara.frontend:make-request :directory "/" :far t)
                                                   :allow (list :clipboard t
                                                                :drop (lambda (paths)
                                                                        (setf sent paths)
                                                                        (list "/home/me/Dropped/a.txt"))))))))
           (queue (tuatara.frontend:make-event-queue))
           (remote (tuatara.frontend:make-remote-frontend mine :queue queue))
           (client (sb-thread:join-thread maker))
           (runner (sb-thread:make-thread (lambda () (tuatara.frontend:run-remote-client client)))))
      (is string= "the desk's copy" (tuatara.frontend:frontend-clipboard remote) "the clipboard is shared")
      (tuatara.frontend:frontend-set-clipboard remote (format nil "ls~c[31m~c~crm~c -rf~c x~c~%"
                                                              #\Esc #\Return (code-char 155) (code-char #x202E)
                                                              (code-char #x200B) #\Tab))
      (is string= (format nil "ls[31mrm -rf x~c~%" #\Tab)
          (wait-until (lambda () (let ((now (tuatara.frontend.fake:fake-clipboard local)))
                                   (and (string/= now "the desk's copy") now))) 10)
          "copied in, its control and invisible characters left out")
      ;; its window left for another: the daemon neither reads nor writes the clipboard
      (setf (focusing-focused local) nil
            (tuatara.frontend.fake:fake-clipboard local) "copied elsewhere")
      (false (tuatara.frontend:frontend-clipboard remote) "not read while the window is left")
      (tuatara.frontend:frontend-set-clipboard remote "swapped")
      (false (tuatara.frontend:frontend-clipboard remote))
      (is string= "copied elsewhere" (tuatara.frontend.fake:fake-clipboard local) "not written while it is left")
      (setf (focusing-focused local) t)
      (false (lending-lent local) "still no terminal lent")
      (type-at local (tuatara.frontend:make-event :drop :files '("/desk/a.txt") :column 3 :row 1))
      (let ((event (tuatara.frontend:queue-pop queue 5)))
        (is eq :drop (and event (tuatara.frontend:event-kind event)))
        (is equal '("/home/me/Dropped/a.txt") (and event (tuatara.frontend:event-files event)) "named as sent there")
        (is equal '("/desk/a.txt") sent))
      (tuatara.frontend:frontend-stop remote)
      (client-done runner))))

(define-test (daemon dropped-files-sent-to-a-trusted-server)
  ;; a file dropped is sent, as its contents, through the connection to the Tuatara there, into the folder the
  ;; server is trusted with, ssh asking nothing; one too large, or no file, is not sent, and none of the drop is
  (when (tuatara.io:find-program "python3")
    (with-fresh-state
      (with-stand-in-ssh (dir log bin)
        (trust-server "build.example" :files "Dropped")
        (with-test-daemon (name)
          (put-program (merge-pathnames "tuatara" bin)
                      (format nil "#!/bin/sh~%case \"$1\" in~%  --receive-file=*) printf '%s\\n' \"$@\" > '~aasked'; ~
                                   cat > '~areceived'; echo 'dropped /home/me/Dropped/kea_1.txt';;~%  *) cat <<'END'~%~
                                   ~aEND~%;;~%esac~%" bin bin (test-report dir name)))
          (let ((link (tuatara.terminal::open-ssh-link "build.example" name))
                (file (namestring (merge-pathnames "kea 1.txt" dir))))
            (true link)
            (when link
              (unwind-protect
                   (progn
                     (put-file file "kea's notes")
                     (is equal '("/home/me/Dropped/kea_1.txt") (tuatara.terminal::send-dropped-files link (list file)))
                     (is string= "kea's notes" (uiop:read-file-string (merge-pathnames "received" bin)))
                     (is string= (format nil "--receive-file=11:Dropped/kea_1.txt~%")
                         (uiop:read-file-string (merge-pathnames "asked" bin)))
                     (true (search (format nil "-F~%none~%-o~%BatchMode=yes~%") (uiop:read-file-string log)))
                     (let ((tuatara.terminal::*most-dropped-octets* 4))
                       (multiple-value-bind (sent said)
                           (said-on-error-output (lambda () (tuatara.terminal::send-dropped-files link (list file))))
                         (false sent)
                         (true (search "larger than 4 octets" said))))
                     (multiple-value-bind (sent said)
                           (said-on-error-output
                          (lambda () (tuatara.terminal::send-dropped-files link (list file (namestring dir)))))
                       (false sent "a folder is no file dropped")
                       (true (plusp (length said))))
                     (is equal '(:clipboard nil) (subseq (tuatara.terminal::link-allowances link) 0 2)))
                (tuatara.terminal::close-ssh-link link)))))))))

(define-test (daemon dropped-file-received)
  ;; the Tuatara on the daemon's machine writes a file dropped for you alone, under a name nothing has, in a
  ;; folder from the home folder made for you alone; a path that is not plain, a file too large, one that came
  ;; short of the size said, or a link in the folder's place, is refused and nothing kept
  (with-test-directory (dir)
    (with-environment-variable ("HOME" (namestring dir))
      (flet ((receive (path text &optional (size (length text)))
               ;; the exit code and what was said on the standard output, and what on the error output
               (let ((source (merge-pathnames "source" dir)))
                 (put-file source text)
                 (with-open-file (in source :element-type '(unsigned-byte 8))
                   (said-on-error-output
                    (lambda ()
                      (let* ((code nil)
                             (said (with-output-to-string (*standard-output*)
                                     (setf code (tuatara.terminal::receive-dropped-file
                                                 (format nil "~d:~a" size path) in)))))
                        (list code said)))))))
               (mode (name)
                 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames name dir)))))))
        (is equal (list 0 (format nil "dropped ~aDropped/kea.txt~%" (namestring dir)))
            (receive "Dropped/kea.txt" "one"))
        (is equal (list 0 (format nil "dropped ~aDropped/kea-2.txt~%" (namestring dir)))
            (receive "Dropped/kea.txt" "two"))
        (is string= "two" (uiop:read-file-string (merge-pathnames "Dropped/kea-2.txt" dir)))
        (is = #o600 (mode "Dropped/kea.txt"))
        (is = #o700 (mode "Dropped/"))
        (dolist (path '("../kea.txt" "Dropped/.hidden" "Dropped/a b" "Dropped/"))
          (is = 1 (first (receive path "x")) "~a" path))
        (let ((tuatara.terminal::*most-dropped-octets* 2))
          (multiple-value-bind (result said) (receive "Dropped/big.txt" "three")
            (is = 1 (first result))
            (true (search "at most 2 octets" said))
            (false (probe-file (merge-pathnames "Dropped/big.txt" dir)) "a file too large is not kept")))
        (multiple-value-bind (result said) (receive "Dropped/short.txt" "abc" 5)
          (is = 1 (first result))
          (true (search "3 octets of the 5 said" said) "~a" said)
          (false (probe-file (merge-pathnames "Dropped/short.txt" dir)) "a file that came short is not kept"))
        (is = 1 (first (receive "Dropped/long.txt" "abcdef" 4)))
        (false (probe-file (merge-pathnames "Dropped/long.txt" dir)))
        (ensure-directories-exist (merge-pathnames "elsewhere/" dir))
        (sb-posix:symlink (namestring (merge-pathnames "elsewhere/" dir)) (namestring (merge-pathnames "Linked" dir)))
        (multiple-value-bind (result said) (receive "Linked/kea.txt" "one")
          (is = 1 (first result))
          (true (search "is a link" said) "~a" said)
          (false (probe-file (merge-pathnames "elsewhere/kea.txt" dir)) "nothing is written through a link"))))))

(define-test (daemon dropped-file-option-read)
  ;; tuatara --receive-file=PATH takes nothing else
  (is string= "Dropped/a.txt" (getf (far-options "--receive-file=Dropped/a.txt") :receive-file))
  (true (getf (far-options "--receive-file=a" "b.txt") :error))
  (true (getf (far-options "--receive-file=a" "--gui") :error))
  (true (tuatara.terminal::client-arguments-p '("--receive-file=a"))))

(define-test (daemon refusal-shown-in-a-window)
  ;; a client in a window that cannot show its daemon shows what it said in a window of its own, there being no
  ;; terminal to read it in, until a key is typed there
  (let ((local (make-instance 'queued-fake-frontend :width 80 :height 10)))
    (type-at local (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\q)))
    (sb-int:encapsulate 'tuatara.frontend.gtk::show-in-window 'fake-window
                        (lambda (function show) (declare (ignore function)) (funcall show local)))
    (unwind-protect
         (multiple-value-bind (code said)
             (said-on-error-output
              (lambda () (tuatara.terminal::shown-in-a-window-too
                          (lambda () (format *error-output* "tuatara: the ssh agent is not forwarded~%") 1))))
           (is = 1 code)
           (true (search "the ssh agent is not forwarded" said) "said on the error output too")
           (true (tuatara.frontend.fake:fake-frame-showing local "tuatara: the ssh agent is not forwarded"))
           (true (tuatara.frontend.fake:fake-frame-showing local "Tuatara could not show the daemon")))
      (sb-int:unencapsulate 'tuatara.frontend.gtk::show-in-window 'fake-window))
    ;; one that did show it says nothing more
    (sb-int:encapsulate 'tuatara.frontend.gtk::show-in-window 'no-window
                        (lambda (function show) (declare (ignore function show)) (error "no window wanted")))
    (unwind-protect
         (is = 0 (said-on-error-output
                  (lambda () (tuatara.terminal::shown-in-a-window-too
                              (lambda () (format *error-output* "tuatara: a note~%") 0)))))
      (sb-int:unencapsulate 'tuatara.frontend.gtk::show-in-window 'no-window))))
