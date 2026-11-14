;;;; test/remote-files.lisp --- Files on another machine, through SFTP.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The toolbox the tests run in has no ssh server, so ssh is played by a
;;;; script of the test's own: it writes the arguments it is given to a log
;;;; and runs OpenSSH's sftp-server, the program sshd runs for the sftp
;;;; subsystem, on its standard input and output, or says what a hostile
;;;; server would.

(in-package #:tuatara.tests)

(define-test remote-files
  :description "Files on another machine are read, written and listed through SFTP, version 3, with ssh run as
the system has it and nothing forwarded; whatever a server answers that breaks the protocol, or not in time,
ends that connection and fails the work in hand, and nothing a server says is run here.")

(defun sftp-server-program ()
  "OpenSSH's sftp-server, here or on the machine the toolbox runs on; NIL when neither has it."
  (find-if (lambda (path) (probe-file path))
           '("/usr/libexec/openssh/sftp-server" "/usr/lib/openssh/sftp-server" "/run/host/usr/libexec/openssh/sftp-server")))

(defmacro with-stand-in-sftp ((dir log &key answers machine before) &body body)
  "Run BODY with DIR bound to a new test directory, the files \"there\", and
*SSH-PROGRAM* set, for every thread, to a stand-in for ssh that writes its
arguments in LOG, a file in DIR, and runs sftp-server with a umask of 077,
or with ANSWERS, octets, writes them and waits; every connection closed
after, and the program set back. Asked what it makes of a host (ssh -G), it
says the host is the machine MACHINE, else the host as named, for the user
written before an @ in it, else me:
kept a minute, so a test naming MACHINE names hosts of its own. BEFORE, shell
lines, run before it serves, and it then says it is OpenSSH 9.9, as one able
to ask questions (SSH-ASKING)."
  (let ((answers-file (gensym)) (old (gensym)))
    `(with-test-directory (,dir)
       (let* ((,log (namestring (merge-pathnames "ssh.log" ,dir)))
              (,answers-file (namestring (merge-pathnames "answers" ,dir)))
              (,old tuatara.defaults::*ssh-program*))
         (when ,answers
           (with-open-file (out ,answers-file :direction :output :element-type '(unsigned-byte 8))
             (write-sequence ,answers out)))
         (unwind-protect
              (progn
                (setf tuatara.defaults::*ssh-program*
                      (put-program (merge-pathnames "ssh" ,dir)
                                   (format nil "#!/bin/sh~%~:[~;if [ \"$1\" = -V ]; then echo OpenSSH_9.9p1 >&2; exit 0; fi~%~]if [ \"$1\" = -G ]; then ~
                                                case \"$3\" in *@*) u=\"${3%%@*}\";; *) u=me;; esac; ~
                                                printf 'hostname %s\\nport 22\\nuser %s\\n' ~:[\"$3\"~;'~:*~a'~] \"$u\"; ~
                                                exit 0; fi~%~
                                                printf '%s\\n' \"$@\" > '~a'~%~@[~a~%~]umask 077~%~:[exec '~a'~;cat '~a'; ~
                                                exec sleep 60~]~%"
                                           ,before ,machine ,log ,before ,answers
                                           (if ,answers ,answers-file (sftp-server-program)))))
                ,@body)
           (tuatara.defaults::close-remote-links)
           (setf tuatara.defaults::*ssh-program* ,old))))))

(defun server-octets (&rest packets)
  "The octets a server sends as PACKETS, each (KIND . FIELDS) as SFTP-PUT
writes fields, an integer first being the request's number."
  (let ((box (tuatara.frontend:make-outbox)))
    (dolist (packet packets)
      (let ((at (tuatara.frontend:outbox-fill box)))
        (tuatara.defaults::sftp-put-integer box 0 4)
        (tuatara.defaults::sftp-put-integer box (second (assoc (first packet) tuatara.defaults::*sftp-types*)) 1)
        (dolist (field (rest packet))
          (if (and (consp field) (eq (first field) :raw))
              (tuatara.frontend:put-octets box (coerce (second field) 'tuatara.text:octets))
              (tuatara.defaults::sftp-put box field)))
        (tuatara.io:octets-put-integer (tuatara.frontend:outbox-octets box) (- (tuatara.frontend:outbox-fill box) at 4)
                                       4 :at at :big-endian t)))
    (subseq (tuatara.frontend:outbox-octets box) 0 (tuatara.frontend:outbox-fill box))))

(defun remote-failure (function)
  "What the FILE-SYSTEM-ERROR FUNCTION signals says, or :NONE when it signals none."
  (handler-case (progn (funcall function) :none)
    (tuatara.io:file-system-error (condition) (princ-to-string condition))))

(define-test (remote-files sftp-reads-writes-and-lists)
  ;; against OpenSSH's own server: attributes, a directory listed, a file of many pieces read whole, written
  ;; beside and renamed over, refused when it changed, a file taken away; ssh given the system's options
  (let ((server (sftp-server-program)))
    (if (null server)
        (skip "no sftp-server here" (true nil))
        (with-stand-in-sftp (dir log)
          (let* ((there (namestring dir))
                 (octets (let ((state (sb-ext:seed-random-state 7)))
                           (coerce (loop repeat 300001 collect (random 256 state)) 'tuatara.text:octets)))
                 (copy (merge-pathnames "copy" dir)))
            (alexandria:write-byte-vector-into-file octets (merge-pathnames "big.bin" dir))
            (put-file (merge-pathnames "a.txt" dir) (format nil "hello~%"))
            (ensure-directories-exist (merge-pathnames "sub/" dir))
            (is = 6 (getf (tuatara.defaults::remote-attributes "box" (concatenate 'string there "a.txt")) :size))
            (false (tuatara.defaults::remote-attributes "box" (concatenate 'string there "none")))
            (is equal '("a.txt" "big.bin" "ssh" "ssh.log" "sub")
                (sort (mapcar #'car (tuatara.defaults::remote-listing "box" there)) #'string<))
            (is eq :directory (tuatara.defaults::remote-kind
                               (cdr (assoc "sub" (tuatara.defaults::remote-listing "box" there) :test #'string=))))
            (is string=
                (string-right-trim "/" there)
                (tuatara.defaults::remote-real-path "box" (concatenate 'string there "sub/..")))
            ;; read whole into a file here
            (let ((fd (tuatara.io:create-free-file there (constantly "copy"))))
              (unwind-protect
                   (is = 300001 (getf (tuatara.defaults::remote-download "box" (concatenate 'string there "big.bin") fd)
                                      :size))
                (sb-posix:close fd)))
            (is equalp octets (alexandria:read-file-into-byte-vector copy))
            ;; written beside, renamed over; refused when the file there is not as expected
            (let ((fd (sb-posix:open (namestring copy) sb-posix:o-rdonly))
                  (target (concatenate 'string there "a.txt")))
              (unwind-protect
                   (progn
                     (is string= (format nil "Could not write /ssh:box:~a: changed meanwhile" target)
                         (remote-failure (lambda () (tuatara.defaults::remote-upload "box" target fd 10
                                                                                     :expected (constantly "changed meanwhile")))))
                     (is = 6 (length (file-text target)))
                     (is = 1000 (getf (tuatara.defaults::remote-upload "box" target fd 1000 :expected (constantly nil))
                                      :size))
                     (is equalp (subseq octets 0 1000) (alexandria:read-file-into-byte-vector target)))
                (sb-posix:close fd)))
            (is equal '("a.txt" "big.bin" "copy" "ssh" "ssh.log" "sub")
                (sort (mapcar #'car (tuatara.defaults::remote-listing "box" there)) #'string<)
                "no file left beside")
            (tuatara.defaults::remote-change "box" :remove (concatenate 'string there "copy"))
            (false (probe-file copy))
            (true (search "there is no such file"
                          (remote-failure (lambda () (tuatara.defaults::remote-change "box" :remove (concatenate 'string there "copy"))))))
            (is = 0 (tuatara.defaults::sftp-link-waiting (tuatara.defaults::sftp-link "box")) "the connection kept")
            (let ((arguments (uiop:read-file-lines log)))
              (dolist (option '("BatchMode=yes" "StrictHostKeyChecking=yes" "ForwardAgent=no" "ForwardX11=no"
                                "PermitLocalCommand=no" "ClearAllForwardings=yes" "ForkAfterAuthentication=no"
                                "Tunnel=no" "StdinNull=no"))
                (true (member option arguments :test #'string=) "~a" option))
              (is equal '("-s" "--" "box" "sftp") (last arguments 4))))))))

(define-test (remote-files sftp-host-refused)
  ;; a host ssh could read as an option is refused before anything runs
  (with-stand-in-sftp (dir log)
    (true (search "cannot name a machine for ssh"
                  (remote-failure (lambda () (tuatara.defaults::remote-attributes "-oProxyCommand=x" "/")))))
    (false (probe-file log))))

(define-test (remote-files sftp-refuses-hostile-answers)
  ;; each answer breaking the protocol, or none in time, ends the connection and fails the work, saying so
  (let ((version '(:version 3)))
    (flet ((failure (answers function)
             (with-stand-in-sftp (dir log :answers answers)
               (prog1 (remote-failure function)
                 (false (tuatara.defaults::sftp-link-fd (tuatara.defaults::sftp-link "box")) "the connection closed")))))
      (dolist (case (list
                     ;; a packet past the bound, an answer to no request, a kind no server sends
                     (list (server-octets version '(:status (:raw #(0 0 0)))) "broke the protocol")
                     (list (concatenate 'tuatara.text:octets (server-octets version) #(0 255 0 0 104)) "packet of")
                     (list (server-octets version '(:attrs 9 0)) "not asked")
                     (list (server-octets version '(:open 1)) "no server sends")
                     ;; attributes with flags the protocol lacks, names past what the packet holds
                     (list (server-octets version '(:attrs 1 #x10)) "flags")
                     (list (server-octets version '(:name 1 1000)) "more than the packet holds")
                     ;; another version, and no answer at all
                     (list (server-octets '(:version 4)) "version 4")
                     (list (server-octets version) "did not answer within 1 second")))
        (destructuring-bind (answers words) case
          (let ((tuatara-user:*remote-seconds* 1))
            (let ((said (failure answers (lambda () (tuatara.defaults::remote-attributes "box" "/x")))))
              (true (search words said) "~s for ~s" said words)))))
      ;; data longer than was asked for
      (let ((tuatara-user:*remote-seconds* 1))
        (with-test-directory (here)
          (let ((fd (tuatara.io:create-free-file (namestring here) (constantly "copy"))))
            (unwind-protect
                 (true (search "where at most"
                               (failure (server-octets version
                                                       '(:handle 1 "h")
                                                       '(:attrs 2 (:raw #(0 0 0 5 0 0 0 0 0 0 0 4 0 0 #o201 #o244)))
                                                       '(:data 3 "longer than four"))
                                        (lambda () (tuatara.defaults::remote-download "box" "/x" fd)))))
              (sb-posix:close fd))))))))

(defparameter *refusing-write-proxy*
  "import os, select, struct, subprocess, sys
server = subprocess.Popen([sys.argv[1]], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
asked, failed = b'', False
while True:
    ready = select.select([0, server.stdout], [], [])[0]
    if 0 in ready:
        data = os.read(0, 65536)
        if not data:
            break
        asked += data
        while len(asked) >= 4 and len(asked) >= 4 + struct.unpack('>I', asked[:4])[0]:
            end = 4 + struct.unpack('>I', asked[:4])[0]
            packet, asked = asked[:end], asked[end:]
            if packet[4] == 6 and not failed:
                failed = True
                os.write(1, struct.pack('>IBII', 17, 101, struct.unpack('>I', packet[5:9])[0], 4) + bytes(8))
            else:
                server.stdin.write(packet)
                server.stdin.flush()
    if server.stdout in ready:
        data = os.read(server.stdout.fileno(), 65536)
        if not data:
            break
        if not failed:
            os.write(1, data)
"
  "A server between the client and sftp-server that answers the first write
with a failure and nothing after it, the other writes sent on unanswered.")

(define-test (remote-files failed-write-leaves-nothing-beside)
  ;; a write refused with others still unanswered leaves no new file beside the one there: it is taken away over
  ;; a new connection
  (let ((server (sftp-server-program)) (python (tuatara.io:find-program "python3")))
    (if (not (and server python))
        (skip "no sftp-server or python3 here" (true nil))
        (with-stand-in-sftp (dir log)
          (let* ((there (namestring dir))
                 (target (concatenate 'string there "kept.txt"))
                 (local (concatenate 'string there "local"))
                 (proxy (put-file (merge-pathnames "proxy.py" dir) *refusing-write-proxy*)))
            (put-file target "as it was")
            (alexandria:write-byte-vector-into-file (make-array 300000 :element-type '(unsigned-byte 8)
                                                                       :initial-element 65)
                                                    local)
            ;; the first connection refuses a write; the next is the server as it is
            (setf tuatara.defaults::*ssh-program*
                  (put-program (merge-pathnames "ssh2" dir)
                               (format nil "#!/bin/sh~%if [ -e '~amore' ]; then exec '~a'; fi~%: > '~amore'~%~
                                            exec '~a' '~a' '~a'~%"
                                       there server there python (namestring proxy) server)))
            (let ((fd (sb-posix:open local sb-posix:o-rdonly)))
              (unwind-protect
                   (true (search "the server failed"
                                 (remote-failure (lambda () (tuatara.defaults::remote-upload "box" target fd 300000
                                                                                             :expected (constantly nil))))))
                (sb-posix:close fd)))
            (true (probe-file (concatenate 'string there "more")) "a second connection was made")
            (is string= "as it was" (file-text target))
            (is equal '() (remove-if-not (lambda (name) (uiop:string-prefix-p ".kept.txt." name))
                                         (mapcar #'file-namestring (uiop:directory-files dir)))
                "no new file left beside"))))))

;;; Visiting, saving and reading again

(defun remote-session (&rest steps)
  "Run an editor session on STEPS (AUTO-SAVE-SESSION), no modal keys; the frontend."
  (let ((tuatara-user:*modal-editing* nil))
    (auto-save-session steps :width 120 :height 20)))

(defun remote-buffer-of (name)
  "The live buffer visiting a copy of the file NAME on another machine, or NIL."
  (multiple-value-bind (host path) (tuatara.defaults::remote-name-parts name)
    (let ((buffer (tuatara.defaults::remote-buffer host path)))
      (and buffer (tuatara.defaults::buffer-remote buffer) buffer))))

(defun kill-remote-buffers ()
  "Kill every buffer visiting a copy of a file on another machine, or reading one."
  (dolist (buffer (copy-list tuatara.buffer:*buffers*))
    (when (or (tuatara.defaults::buffer-remote buffer) (tuatara.buffer:buffer-local buffer :remote-reading))
      (tuatara.buffer:kill-buffer buffer))))

(defun visit-keys (name)
  "The keys visiting NAME with C-x C-f, the path offered first taken away."
  (format nil "C-x C-f C-a C-k ~a RET" (typed-keys name)))

(defun messages-since-step ()
  "What *Messages* says since the step before began."
  (subseq (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)) *session-messages-start*))

(define-test (remote-files names-read)
  ;; /ssh:HOST:PATH, its path worked out by its words; a host ssh could take for an option refused
  (is string= "/etc/hosts" (tuatara.defaults::remote-path-normal "/etc//./x/../hosts"))
  (is string= "/" (tuatara.defaults::remote-path-normal "/../.."))
  (is string= "notes/a.org" (tuatara.defaults::remote-path-normal "~/notes/a.org"))
  (is string= "" (tuatara.defaults::remote-path-normal "~"))
  (is string= "/srv/" (tuatara.defaults::remote-path-normal "/srv/x/../"))
  (is equal '("me@box" "/etc/hosts")
      (multiple-value-list (tuatara.defaults::remote-name-parts "/ssh:me@box:/etc/hosts")))
  (false (tuatara.defaults::remote-name-p "/ssh-notes/a"))
  (true (tuatara.defaults::remote-name-p "/ssh:box:"))
  (fail (tuatara.defaults::remote-name-parts "/ssh:-oProxyCommand=x:/etc/hosts") 'tuatara.runtime:refusal)
  (is string= "/ssh:box:" (tuatara.defaults::input-directory "/ssh:box:not"))
  (is string= "/ssh:box:/etc/" (tuatara.defaults::input-directory "/ssh:box:/etc/ho"))
  (is string= "/ssh:box:hosts" (tuatara.defaults::resolve-file-input "/ssh:box:ho" "hosts")))

(define-test (remote-files visited-saved-and-read-again)
  ;; C-x C-f reads the file into a private copy, the buffer named after it and standing in its directory there;
  ;; a save writes it there; revert reads it from there; the copy goes with the buffer
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (file (concatenate 'string there "notes.txt"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "first" file)
          (unwind-protect
               (progn
                 (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                 "M->" (typed-keys " more") "C-x C-s"
                                 (list :until (lambda () (string= "first more" (file-text file)))))
                 (let* ((buffer (remote-buffer-of name))
                        (copy (tuatara.io:native (tuatara.buffer:buffer-file buffer))))
                   (is string= "notes.txt" (tuatara.buffer:buffer-name buffer))
                   (is string= (format nil "/ssh:box:~a" there) (tuatara.command:buffer-directory buffer))
                   (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat copy))))
                   (true (search "/remote/" copy))
                   (false (tuatara.buffer:buffer-modified-p buffer))
                   (is equal '("first more") (list (file-text file)))
                   ;; changed there, read again from there
                   (alexandria:write-string-into-file "second text" file :if-exists :supersede)
                   (remote-session (format nil "C-x b ~a RET" (typed-keys "notes.txt"))
                                   (format nil "M-x ~a RET" (typed-keys "revert-buffer"))
                                   (list :until (lambda ()
                                                  (string= "second text" (tuatara.buffer:buffer-string buffer)))))
                   (is string= "second text" (tuatara.buffer:buffer-string buffer))
                   (tuatara.buffer:kill-buffer buffer)
                   (true (tuatara.runtime:wait-until (lambda () (not (probe-file copy))) 10) "the copy taken away")))
            (kill-remote-buffers))))))

(define-test (remote-files save-refused-when-changed-there)
  ;; a file changed on the server since it was read is not written over: the buffer stays modified
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) "conf"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "one" file)
          (unwind-protect
               (progn
                 (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                 ;; changed there meanwhile
                                 (list :until (lambda ()
                                                (alexandria:write-string-into-file "changed elsewhere" file
                                                                                   :if-exists :supersede)
                                                t))
                                 "M->" (typed-keys "!") "C-x C-s"
                                 (list :until (lambda () (search "is not written there" (messages-since-step)))))
                 (is string= "changed elsewhere" (file-text file))
                 (true (tuatara.buffer:buffer-modified-p (remote-buffer-of name)))
                 (true (search "changed on the server"
                               (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)))))
            (kill-remote-buffers))))))

(define-test (remote-files new-file-made-there)
  ;; a name not there yet is an empty buffer whose save makes the file there
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) "new.txt"))
               (name (concatenate 'string "/ssh:box:" file)))
          (unwind-protect
               (progn
                 (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                 (typed-keys "made") "C-x C-s" (list :until (lambda () (probe-file file))))
                 (true (tuatara.runtime:wait-until (lambda () (equal "made" (ignore-errors (file-text file)))) 10))
                 (is equal "made" (file-text file)))
            (kill-remote-buffers))))))

(define-test (remote-files evidence-server-read-only)
  ;; a directory there named in *evidence-directories* opens safely and is never written
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) "seized.log"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "kept" file)
          (with-evidence-directories ((list (format nil "/ssh:box:~a" (namestring dir))))
            (unwind-protect
                 (progn
                   (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name))))
                   (let ((buffer (remote-buffer-of name)))
                     (true (tuatara.io:safe-visit-p buffer))
                     (true (tuatara.defaults::remote-file-evidence (tuatara.defaults::buffer-remote buffer)))
                     (true (tuatara.defaults::remote-save-refusal buffer))
                     (setf (tuatara.buffer:buffer-read-only-p buffer) nil)
                     (remote-session (format nil "C-x b ~a RET M->" (typed-keys "seized.log"))
                                     (typed-keys "x") "C-x C-s"
                                     (list :wait 0.3))
                     (is string= "kept" (file-text file))))
              (kill-remote-buffers)))))))

(define-test (remote-files names-completed-from-the-server)
  ;; the directory there is listed as C-x C-f's candidates; a name that would not show as it is is left out
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (alexandria:write-string-into-file "x" (concatenate 'string there "alpha-file.txt"))
          (alexandria:write-string-into-file "x"
                                             (concatenate 'string there (format nil "evil~cname" (code-char #x202E))))
          (let ((names (mapcar #'tuatara.command:candidate-string
                               (tuatara.defaults::remote-candidates (concatenate 'string "/ssh:box:" there)))))
            (true (member "alpha-file.txt" names :test #'string=))
            (false (find-if (lambda (n) (search "evil" n)) names)))
          (let ((f (remote-session "C-x C-f" "C-a C-k" (typed-keys (concatenate 'string "/ssh:box:" there))
                                   (list :until (lambda ()
                                                  (tuatara.frontend.fake:fake-frame-showing
                                                   (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                   "alpha-file.txt")))
                                   "C-g")))
            (true (tuatara.frontend.fake:fake-frame-showing f "alpha-file.txt")))))))

(define-test (remote-files copies-kept-from-tools)
  ;; no language server, git or auto-save looks at a copy; the settings a file names for itself and the
  ;; shared parser only for a trusted server
  (false (member 'tuatara.defaults::auto-save-visited (tuatara.defaults::remote-find-file-functions t)))
  (false (member 'tuatara.lsp::lsp-visit (tuatara.defaults::remote-find-file-functions t)))
  (false (member 'tuatara.defaults::take-file-local-settings (tuatara.defaults::remote-find-file-functions nil)))
  (let ((buffer (tuatara.buffer:make-buffer "copy-test")))
    (unwind-protect
         (let ((remote (tuatara.defaults::make-remote-file "box" "/a.py")))
           (setf (tuatara.buffer:buffer-local buffer :remote-file) remote)
           (true (tuatara.lsp::outside-program-refusal buffer))
           (false (tuatara.syntax:host-trusted-text-p tuatara.runtime:*host* buffer))
           (setf (tuatara.defaults::remote-file-trusted remote) t)
           (true (tuatara.syntax:host-trusted-text-p tuatara.runtime:*host* buffer))
           (false (tuatara.defaults::session-entry buffer)))
      (tuatara.buffer:kill-buffer buffer))))

;;; Directories listed with rimu

(defun remote-rimu-buffer ()
  "The rimu list of a directory on another machine shown last, or NIL."
  (find-if (lambda (buffer)
             (and (tuatara.defaults::rimu-list-p buffer)
                  (tuatara.defaults::remote-rimu-source-p (tuatara.defaults::rimu-source buffer))))
           tuatara.buffer:*buffers*))

(defun remote-list-read ()
  "True once a rimu list of a directory on another machine shows what was read."
  (let ((list (remote-rimu-buffer)))
    (and list (tuatara.buffer:buffer-local list 'tuatara.defaults::rimu-listing) t)))

(defun kill-remote-lists ()
  "Kill every rimu list of a directory on another machine."
  (loop for buffer = (remote-rimu-buffer) while buffer do (tuatara.buffer:kill-buffer buffer)))

(define-test (remote-files directory-listed-with-rimu)
  ;; C-x C-f on a directory there lists it with rimu: its entries, a link with where it leads, a name that would not
  ;; show as it is shown escaped; no preview, and the commands working on files here refused
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (alexandria:write-string-into-file "x" (concatenate 'string there "alpha.txt"))
          (alexandria:write-string-into-file "x"
                                             (concatenate 'string there (format nil "evil~cname" (code-char #x202E))))
          (ensure-directories-exist (merge-pathnames "sub/" dir))
          (sb-posix:symlink "alpha.txt" (concatenate 'string there "link"))
          (unwind-protect
               (let ((f (remote-session (format nil "C-x C-f C-a C-k ~a C-j"
                                                ;; the name as typed, without the / completion would add
                                                (typed-keys (concatenate 'string "/ssh:box:"
                                                                         (string-right-trim "/" there))))
                                        (list :until #'remote-list-read))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "alpha.txt"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "<202E>") "the hiding character shown by its code")
                 (let* ((list (remote-rimu-buffer))
                        (entries (tuatara.defaults::rimu-listing-entries
                                  (tuatara.buffer:buffer-local list 'tuatara.defaults::rimu-listing)))
                        (link (find "link" entries :key #'tuatara.io:listed-name :test #'string=)))
                   (true list)
                   (is string= (format nil "/ssh:box:~a" there) (tuatara.defaults::rimu-list-directory list))
                   (false (tuatara.defaults::list-preview-p list))
                   (is eq :regular (tuatara.defaults::rimu-leads-to link))
                   (is string= "alpha.txt" (tuatara.io:listed-target link))
                   (true (search "works on files here"
                                 (tuatara.defaults::rimu-source-refusal 'tuatara.defaults::rimu-symlink list)))
                   (true (tuatara.defaults::rimu-source-refusal 'tuatara.defaults::rimu-find list))
                   (false (tuatara.defaults::rimu-source-refusal 'tuatara.defaults::rimu-copy list))))
            (kill-remote-lists))))))

(define-test (remote-files opened-by-what-they-are)
  ;; a file there opens as one here does: a picture as its picture, its copy going with the buffer; one that looks
  ;; binary in the hex view; opened safely, in the hex view whatever it is
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (binary (concatenate 'string "/ssh:box:" there "data.bin"))
               (picture (concatenate 'string "/ssh:box:" there "photo.png"))
               (drawn (and (eq (tuatara.io:sandbox-status) :ready) (tuatara.defaults::find-image-decoder))))
          (alexandria:write-byte-vector-into-file
           (coerce #(1 0 2 0 3 0 255 254) '(vector (unsigned-byte 8))) (concatenate 'string there "data.bin"))
          (alexandria:write-byte-vector-into-file *tiny-png* (concatenate 'string there "photo.png"))
          (unwind-protect
               (progn
                 (remote-session (visit-keys binary) (list :until (lambda () (remote-buffer-of binary))))
                 (let ((buffer (remote-buffer-of binary)))
                   (true (tuatara.display:hex-view-p buffer))
                   (tuatara.buffer:kill-buffer buffer))
                 (remote-session (format nil "C-u ~a" (visit-keys picture))
                                 (list :until (lambda () (remote-buffer-of picture))))
                 (let ((buffer (remote-buffer-of picture)))
                   (false (tuatara.defaults::image-view-of buffer))
                   (true (tuatara.display:hex-view-p buffer))
                   (tuatara.buffer:kill-buffer buffer))
                 (when drawn
                   ;; where pictures are not shown: its octets, the copy kept with them
                   (remote-session (visit-keys picture)
                                   (list :until (lambda ()
                                                  (let ((buffer (remote-buffer-of picture)))
                                                    (and buffer (tuatara.display:hex-view-p buffer))))))
                   (let ((buffer (remote-buffer-of picture)))
                     (is string= "photo.png" (tuatara.buffer:buffer-name buffer))
                     (true (probe-file (tuatara.buffer:buffer-file buffer)))
                     (tuatara.buffer:kill-buffer buffer))
                   (let ((tuatara-user:*modal-editing* nil))
                     (auto-save-session (list (visit-keys picture) (list :until (lambda () (remote-buffer-of picture))))
                                        :width 120 :height 20 :cell-pixels '(10 20)))
                   (let* ((buffer (remote-buffer-of picture))
                          (copy (tuatara.buffer:buffer-local buffer :viewed-file)))
                     (true (tuatara.defaults::image-view-of buffer))
                     (true (tuatara.defaults::remote-copy-p copy))
                     (is string= "photo.png" (tuatara.buffer:buffer-name buffer))
                     ;; the octets opened from the view keep the copy once the view is killed
                     (let ((tuatara-user:*modal-editing* nil))
                       (auto-save-session (list (format nil
                                                        "C-x b ~a RET M-x ~a RET"
                                                        (typed-keys "photo.png")
                                                        (typed-keys "hex-mode")))
                                          :width 120 :height 20 :cell-pixels '(10 20)))
                     (let ((octets (find-if (lambda (b)
                                              (let ((file (tuatara.buffer:buffer-file b)))
                                                (and file (equal (tuatara.io:native file) copy))))
                                            tuatara.buffer:*buffers*)))
                       (true octets)
                       (tuatara.buffer:kill-buffer buffer)
                       (true (tuatara.defaults::buffer-private-directory octets) "the directory is the octets' now")
                       (true (probe-file copy))
                       (setf buffer octets))
                     (tuatara.buffer:kill-buffer buffer)
                     (true (tuatara.runtime:wait-until (lambda () (not (probe-file copy))) 10) "the copy taken away"))))
            (image-forget)
            (kill-remote-buffers))))))

(define-test (remote-files download-stops-and-is-held-to-a-size)
  ;; asked to stop, or past the most asked for, nothing more is read and the connection stays good
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((path (concatenate 'string (namestring dir) "big.txt"))
              (copy (merge-pathnames "copy" dir)))
          (alexandria:write-string-into-file (make-string 100000 :initial-element #\x) path)
          (let ((fd (sb-posix:open (namestring copy) (logior sb-posix:o-creat sb-posix:o-rdwr) #o600)))
            (unwind-protect
                 (progn
                   (fail (let ((tuatara.defaults::*remote-cancel* (list t)))
                           (tuatara.defaults::remote-download "box" path fd))
                       'tuatara.runtime:job-stopped)
                   (is = 0 (sb-posix:stat-size (sb-posix:fstat fd)))
                   (fail (tuatara.defaults::remote-download "box" path fd :most 10) 'error)
                   (is = 0 (sb-posix:stat-size (sb-posix:fstat fd)))
                   (true (tuatara.defaults::remote-attributes "box" path) "the connection still good")
                   (tuatara.defaults::remote-download "box" path fd :most 100000)
                   (is = 100000 (sb-posix:stat-size (sb-posix:fstat fd))))
              (sb-posix:close fd)))
          (tuatara.defaults::close-remote-links)))))

(define-test (remote-files preview-only-when-asked)
  ;; a list there starts with no preview; P shows the entry's start, its copy gone afterwards; a file larger than
  ;; the most is not fetched, and says so
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (keys (format nil "C-x C-f C-a C-k ~a C-j"
                             (typed-keys (concatenate 'string "/ssh:box:" (string-right-trim "/" there)))))
               (most tuatara-user:*remote-preview-size-limit*))
          (alexandria:write-string-into-file "the-words-within" (concatenate 'string there "alpha.txt"))
          (flet ((previewing () (let ((state tuatara.defaults::*rimu-preview-state*))
                                  (and state (tuatara.defaults::rimu-preview-buffer-entry state))))
                 (preview-text () (tuatara.buffer:buffer-string (tuatara.defaults::rimu-preview-buffer
                                                                 tuatara.defaults::*rimu-preview-state*))))
            (unwind-protect
                 (let ((tuatara-user:*rimu-preview-delay* 0))
                   (let ((f (remote-session keys (list :until #'remote-list-read))))
                     (false (tuatara.defaults::list-preview-p (remote-rimu-buffer)))
                     (false (tuatara.frontend.fake:fake-frame-showing f "the-words-within")))
                   (let ((f (remote-session keys (list :until #'remote-list-read) "P" (list :until #'previewing))))
                     (true (tuatara.defaults::list-preview-p (remote-rimu-buffer)))
                     (true (tuatara.frontend.fake:fake-frame-showing f "the-words-within")))
                   (kill-remote-lists)
                   (setf tuatara-user:*remote-preview-size-limit* 3)
                   (let ((f (remote-session keys (list :until #'remote-list-read) "P" (list :until #'previewing))))
                     (true (search "Too large to preview" (preview-text)))
                     (false (search "Could not read" (preview-text)))
                     (false (tuatara.frontend.fake:fake-frame-showing f "the-words-within")))
                   (setf tuatara-user:*remote-preview-size-limit* most)
                   (kill-remote-lists)
                   (with-evidence-directories ((list (format nil "/ssh:box:~a" there)))
                     (let ((f (remote-session keys (list :until #'remote-list-read) "P" '(:wait 1))))
                       (false (previewing) "a list opened safely has no preview")
                       (true (tuatara.frontend.fake:fake-frame-showing f "No preview here"))
                       (false (tuatara.frontend.fake:fake-frame-showing f "Preview on"))
                       (false (tuatara.frontend.fake:fake-frame-showing f "the-words-within")))))
              (setf tuatara-user:*remote-preview-size-limit* most)
              (tuatara.defaults::rimu-close-preview)
              (kill-remote-lists)))))))

(define-test (remote-files directory-copied-both-ways)
  ;; files and directories copied from there to here, here to there and within the machine, never over a name there;
  ;; a link left out and said; renamed there, a move to another machine refused; a directory there deleted whole
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (with-test-directory (here)
          (let* ((there (namestring dir))
                 (local (namestring here))
                 (remote (concatenate 'string "/ssh:box:" there)))
            (flet ((work (verb from &optional to)
                     (tuatara.defaults::rimu-work verb (cons from to) (list nil)))
                   (failure (function)
                     (handler-case (progn (funcall function) :none) (error (condition) (princ-to-string condition)))))
              (ensure-directories-exist (merge-pathnames "tree/deep/" dir))
              (put-file (merge-pathnames "tree/a.txt" dir) "first")
              (put-file (merge-pathnames "tree/deep/b.txt" dir) "second")
              ;; there to here, a directory whole
              (work :copy (concatenate 'string remote "tree") (concatenate 'string local "tree"))
              (is string= "first" (file-text (merge-pathnames "tree/a.txt" here)))
              (is string= "second" (file-text (merge-pathnames "tree/deep/b.txt" here)))
              (is = #o600
                  (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames "tree/a.txt" here))))))
              ;; here to there, and never over a file there
              (put-file (merge-pathnames "up.txt" here) "from here")
              (work :copy (concatenate 'string local "up.txt") (concatenate 'string remote "up.txt"))
              (is string= "from here" (file-text (merge-pathnames "up.txt" dir)))
              (put-file (merge-pathnames "up.txt" here) "again")
              (true (search "a file of that name is there"
                            (failure (lambda () (work :copy (concatenate 'string local "up.txt")
                                                      (concatenate 'string remote "up.txt"))))))
              (is string= "from here" (file-text (merge-pathnames "up.txt" dir)))
              ;; within the machine
              (work :copy (concatenate 'string remote "up.txt") (concatenate 'string remote "copied.txt"))
              (is string= "from here" (file-text (merge-pathnames "copied.txt" dir)))
              ;; a link left out, said
              (sb-posix:symlink "a.txt" (namestring (merge-pathnames "tree/link" dir)))
              (true (search "link (a link)"
                            (failure (lambda () (work :copy (concatenate 'string remote "tree")
                                                      (concatenate 'string local "tree2"))))))
              ;; renamed there; a move to here refused
              (work :move (concatenate 'string remote "copied.txt") (concatenate 'string remote "renamed.txt"))
              (true (probe-file (merge-pathnames "renamed.txt" dir)))
              (true (search "within one machine"
                            (failure (lambda () (work :move (concatenate 'string remote "renamed.txt")
                                                      (concatenate 'string local "renamed.txt"))))))
              ;; deleted whole, a trash meaning the same
              (work :trash (concatenate 'string remote "tree/"))
              (false (probe-file (merge-pathnames "tree/a.txt" dir)))
              (false (uiop:directory-exists-p (merge-pathnames "tree/" dir)))))))))

(define-test (remote-files deleted-there-for-good-when-asked)
  ;; D in a list of a directory there asks once, plainly, that it is deleted for good, then deletes it
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (put-file (merge-pathnames "gone.txt" dir) "x")
          (unwind-protect
               (let ((f (remote-session (visit-keys (concatenate 'string "/ssh:box:" there))
                                        (list :until #'remote-list-read)
                                        "C-s g o n e RET D"
                                        (list :until (lambda ()
                                                       (tuatara.frontend.fake:fake-frame-showing
                                                        (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                        "keeps no trash")))
                                        "y e s RET"
                                        (list :until (lambda () (not (probe-file (merge-pathnames "gone.txt" dir))))))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "for good? A server keeps no trash"))
                 (false (probe-file (merge-pathnames "gone.txt" dir))))
            (kill-remote-lists))))))

(define-test (remote-files evidence-directory-there-kept)
  ;; a directory there named in *evidence-directories* is listed opened safely, and nothing in it is deleted or written
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (remote (concatenate 'string "/ssh:box:" there)))
          (put-file (merge-pathnames "seized.txt" dir) "kept")
          (with-evidence-directories ((list remote))
            (let ((list (tuatara.defaults::remote-rimu-list remote)))
              (unwind-protect
                   (progn
                     (true (tuatara.io:safe-visit-p list))
                     (false (tuatara.defaults::rimu-start-operation
                             :delete (list (list (concatenate 'string remote "seized.txt"))) list))
                     (false (tuatara.defaults::rimu-start-operation
                             :copy (list (cons (namestring (merge-pathnames "ssh.log" dir))
                                               (concatenate 'string remote "new.txt")))
                             list))
                     (is string= "kept" (file-text (merge-pathnames "seized.txt" dir)))
                     (false (probe-file (merge-pathnames "new.txt" dir))))
                (tuatara.buffer:kill-buffer list))))))))

(define-test (remote-files names-from-the-server-kept-in-their-directory)
  ;; a name a server lists that would lead out of its directory (../x) is refused, nothing copied outside
  (let ((tuatara-user:*remote-seconds* 1))
    (with-stand-in-sftp (dir log :answers (server-octets '(:version 3) '(:attrs 1 4 #o40755) '(:handle 2 "h")
                                                         '(:name 3 1 "../evil" "x" 0) '(:status 4 1 "" "")
                                                         '(:status 5 0 "" "")))
      (with-test-directory (here)
        (let ((said (handler-case (progn (tuatara.defaults::rimu-work :copy (cons "/ssh:box:/directory"
                                                                                  (namestring (merge-pathnames "copy" here)))
                                                                     (list nil))
                                         :none)
                      (error (condition) (princ-to-string condition)))))
          (true (search "a name no entry can have" said) "~a" said)
          (false (probe-file (merge-pathnames "evil" here))))))))

;;; Evidence on a server

(defun rimu-failure (verb from &optional to)
  "What doing VERB to FROM (and TO) as rimu's job does says when it fails, or :NONE."
  (handler-case (progn (tuatara.defaults::rimu-work verb (cons from to) (list nil)) :none)
    (error (condition) (princ-to-string condition))))

(define-test (remote-files evidence-held-in-a-directory-kept)
  ;; a directory there holding an evidence directory is neither deleted nor moved whole
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((remote (concatenate 'string "/ssh:box:" (namestring dir))))
          (ensure-directories-exist (merge-pathnames "case/seized/" dir))
          (put-file (merge-pathnames "case/seized/disk.img" dir) "kept")
          (with-evidence-directories ((list (concatenate 'string remote "case/seized/")))
            (true (search "evidence directory" (rimu-failure :delete (concatenate 'string remote "case"))))
            (true (search "evidence directory" (rimu-failure :move (concatenate 'string remote "case")
                                                          (concatenate 'string remote "moved"))))
            (is string= "kept" (file-text (merge-pathnames "case/seized/disk.img" dir))))))))

(define-test (remote-files evidence-told-by-machine-and-real-path)
  ;; an evidence directory there is told under any name of its machine, as written and where it really is, by a
  ;; visit and by a list of it alike
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :machine "evidence.lan")
        (let ((there (namestring dir)))
          (ensure-directories-exist (merge-pathnames "seized/" dir))
          (put-file (merge-pathnames "seized/a.txt" dir) "kept")
          (sb-posix:symlink (concatenate 'string there "seized") (concatenate 'string there "door"))
          (with-evidence-directories ((list (format nil "/ssh:ev-alias:~aseized/" there)))
            (true (tuatara.defaults::remote-evidence-words "ev-host" (concatenate 'string there "seized/a.txt"))
                  "named under another name of the machine")
            (true (tuatara.defaults::remote-evidence-words "ev-host" (concatenate 'string there "door/a.txt"))
                  "reached through a link to it")
            (false (tuatara.defaults::remote-evidence-words "ev-host" (concatenate 'string there "ssh.log")))
            (unwind-protect
                 (progn (remote-session (visit-keys (format nil "/ssh:ev-host:~aseized/" there))
                                        (list :until #'remote-list-read))
                        (true (tuatara.io:safe-visit-p (remote-rimu-buffer)) "its list opened safely"))
              (kill-remote-lists)))
          (with-evidence-directories ((list (format nil "/ssh:ev-host:~adoor/" there)))
            (true (tuatara.defaults::remote-evidence-words "ev-host" (concatenate 'string there "seized/a.txt"))
                  "named by a link that leads to it"))))))

(define-test (remote-files directory-not-copied-into-itself)
  ;; a directory there is not copied into itself, under another name of its machine either
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :machine "copies.lan")
        (let ((there (namestring dir)))
          (ensure-directories-exist (merge-pathnames "tree/" dir))
          (put-file (merge-pathnames "tree/a.txt" dir) "a")
          (true (search "into itself" (rimu-failure :copy (format nil "/ssh:copy-a:~atree" there)
                                                    (format nil "/ssh:copy-b:~atree/inner" there))))
          (false (uiop:directory-exists-p (merge-pathnames "tree/inner/" dir)))))))

;;; Bounds on what a server says, and its directories read for a question

(defun calls-of (name function)
  "How many times the function NAME is called, on any thread, while FUNCTION runs."
  (let ((count (list 0)))
    (sb-int:encapsulate name 'counted (lambda (inner &rest arguments)
                                        (sb-ext:atomic-incf (car count))
                                        (apply inner arguments)))
    (unwind-protect (funcall function)
      (sb-int:unencapsulate name 'counted))
    (car count)))

(define-test (remote-files shortest-names-taken)
  ;; the answer of fewest octets a server gives for a name, that of the root, is taken
  (let ((tuatara-user:*remote-seconds* 1))
    (with-stand-in-sftp (dir log :answers (server-octets '(:version 3) '(:name 1 1 "/" "" 0)))
      (is string= "/" (tuatara.defaults::remote-real-path "box" "/")))))

(define-test (remote-files names-from-the-server-cleaned)
  ;; where a path leads is refused, and where a link leads shown escaped, when it would not show as it is; a
  ;; name holding NUL is no entry's
  (false (tuatara.defaults::remote-entry-name-p (format nil "a~cb" (code-char 0))))
  (let ((tuatara-user:*remote-seconds* 1))
    (with-stand-in-sftp (dir log :answers (server-octets '(:version 3)
                                                         (list :name 1 1 (format nil "/evil~cname" (code-char #x202E))
                                                               "" 0)))
      (true (search "would not show" (remote-failure (lambda () (tuatara.defaults::remote-real-path "box" "/x")))))))
  (when (sftp-server-program)
    (with-stand-in-sftp (dir log)
      (let ((there (namestring dir)))
        (sb-posix:symlink (format nil "x~cy" (code-char #x202E)) (concatenate 'string there "badlink"))
        (true (search "<202E>" (car (first (tuatara.defaults::remote-link-details "box" there '("badlink"))))))))))

(define-test (remote-files listing-bounded-by-its-names)
  ;; a listing is cut past so many characters of names, as past so many entries
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (dolist (name '("aaaa" "bbbb" "cccc"))
          (put-file (merge-pathnames name dir) "x"))
        (let ((tuatara.defaults::*sftp-most-listed-characters* 10))
          (multiple-value-bind (entries cut) (tuatara.defaults::remote-listing "box" (namestring dir))
            (true cut)
            (true (< (length entries) 3)))))))

(define-test (remote-files handles-let-go-after-a-refusal)
  ;; a directory or file opened, then a request about it refused, is closed there before the connection goes on
  (let ((tuatara-user:*remote-seconds* 1)
        (answers (server-octets '(:version 3) '(:handle 1 "h") '(:status 2 3 "" "") '(:status 3 0 "" "")
                                '(:attrs 4 4 #o100644))))
    (with-stand-in-sftp (dir log :answers answers)
      (true (search "permission denied" (remote-failure (lambda () (tuatara.defaults::remote-listing "box" "/x/")))))
      (is = #o100644 (getf (tuatara.defaults::remote-attributes "box" "/y") :permissions)))
    (with-stand-in-sftp (dir log :answers answers)
      (let ((fd (tuatara.io:create-free-file (namestring dir) (constantly "copy"))))
        (unwind-protect
             (true (search "permission denied"
                           (remote-failure (lambda () (tuatara.defaults::remote-download "box" "/x" fd)))))
          (sb-posix:close fd)))
      (is = #o100644 (getf (tuatara.defaults::remote-attributes "box" "/y") :permissions)))))

(define-test (remote-files directory-there-read-once-while-asked)
  ;; the entries of a directory there are read once for a question for a file, not at each key; a path to one
  ;; typed in a buffer reads nothing there
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((directory (concatenate 'string "/ssh:box:" (namestring dir))))
          (is = 1 (calls-of 'tuatara.defaults::remote-listing
                            (lambda ()
                              (tuatara.defaults::directory-candidates directory nil)
                              (tuatara.defaults::directory-candidates directory nil))))
          (let ((tuatara.defaults::*remote-candidates-seconds* 0))
            (is = 0 (calls-of 'tuatara.defaults::remote-listing
                              (lambda ()
                                (run-keys (list "i" (key-string directory) #'wait-for-path-listing)
                                          :file (namestring (put-file (merge-pathnames "typed.txt" dir) ""))
                                          :width 160 :height 12)))))))))

;;; Saving

(define-test (remote-files save-checks-with-a-name-of-its-own)
  ;; the file read again to be compared before a save has a name the save makes: a file there named .check
  ;; keeps its copy here
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) ".check"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "one" file)
          (unwind-protect
               (progn
                 (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                 "M->" (typed-keys "!") "C-x C-s"
                                 (list :until (lambda () (string= "one!" (file-text file)))))
                 (is string= "one!" (file-text file))
                 (true (probe-file (tuatara.io:native (tuatara.buffer:buffer-file (remote-buffer-of name))))
                       "the copy here kept"))
            (kill-remote-buffers))))))

(define-test (remote-files changed-there-told-before-reading)
  ;; a file changed there in size or time is refused at once, not read again to be compared
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) "conf"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "one" file)
          (unwind-protect
               (progn
                 (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                 ;; changed there meanwhile, and not to be read now
                                 (list :until (lambda ()
                                                (alexandria:write-string-into-file "changed elsewhere" file
                                                                                   :if-exists :supersede)
                                                (sb-posix:chmod file 0)
                                                t))
                                 "M->" (typed-keys "!") "C-x C-s"
                                 (list :until (lambda () (search "is not written there" (messages-since-step)))))
                 (let ((said (tuatara.buffer:buffer-string (tuatara.command::messages-buffer))))
                   (true (search "changed on the server" said))
                   (false (search "permission denied" said))))
            (sb-posix:chmod file #o600)
            (kill-remote-buffers))))))

(define-test (remote-files saves-keep-what-sftp-can)
  ;; a save keeps the permissions of the file there, whatever the server's umask; a file of two names is
  ;; written in place, both names then holding the text, and that is said
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (shared (concatenate 'string there "shared.txt"))
               (linked (concatenate 'string there "linked.txt"))
               (other (concatenate 'string there "other.txt")))
          (alexandria:write-string-into-file "one" shared)
          (sb-posix:chmod shared #o664)
          (alexandria:write-string-into-file "two" linked)
          (sb-posix:link linked other)
          (unwind-protect
               (let ((inode (sb-posix:stat-ino (sb-posix:stat linked))))
                 (remote-session (visit-keys (concatenate 'string "/ssh:box:" shared))
                                 (list :until (lambda () (remote-buffer-of (concatenate 'string "/ssh:box:" shared))))
                                 "M->" (typed-keys "!") "C-x C-s"
                                 (list :until (lambda () (string= "one!" (file-text shared)))))
                 (is = #o664 (logand #o7777 (sb-posix:stat-mode (sb-posix:stat shared))))
                 (remote-session (visit-keys (concatenate 'string "/ssh:box:" linked))
                                 (list :until (lambda () (remote-buffer-of (concatenate 'string "/ssh:box:" linked))))
                                 "M->" (typed-keys "!") "C-x C-s"
                                 (list :until (lambda ()
                                                (search "in place: it has 2 names"
                                                        (tuatara.buffer:buffer-string
                                                         (tuatara.command::messages-buffer))))))
                 (is string= "two!" (file-text other))
                 (is = inode (sb-posix:stat-ino (sb-posix:stat linked)))
                 (true (search "in place: it has 2 names"
                               (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)))))
            (kill-remote-buffers))))))

;;; Second look: other names, accounts, paths not found, deep trees, saves

(define-test (remote-files linked-file-with-evidence-name-not-written)
  ;; a file with another name is not written in place while an evidence directory is named on its machine, as
  ;; that name may be in it
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (linked (concatenate 'string there "linked.txt"))
               (kept (concatenate 'string there "seized/other.txt"))
               (name (concatenate 'string "/ssh:box:" linked)))
          (ensure-directories-exist (merge-pathnames "seized/" dir))
          (alexandria:write-string-into-file "two" linked)
          (sb-posix:link linked kept)
          (with-evidence-directories ((list (format nil "/ssh:box:~aseized/" there)))
            (unwind-protect
                 (progn
                   (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                   "M->" (typed-keys "!") "C-x C-s"
                                   (list :until (lambda ()
                                                  (search "is not written there"
                                                          (tuatara.buffer:buffer-string
                                                           (tuatara.command::messages-buffer))))))
                   (is string= "two" (file-text kept))
                   (true (search "may be in an evidence directory"
                                 (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)))))
              (kill-remote-buffers)))))))

(define-test (remote-files evidence-holds-for-every-account)
  ;; an evidence directory named with one account on a machine holds for every account there
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :machine "accounts.lan")
        (let ((there (namestring dir)))
          (ensure-directories-exist (merge-pathnames "seized/" dir))
          (with-evidence-directories ((list (format nil "/ssh:root@acct-host:~aseized/" there)))
            (true (tuatara.defaults::remote-evidence-words "alice@acct-host"
                                                           (concatenate 'string there "seized/a.txt"))))))))

(define-test (remote-files real-path-unknown-refused)
  ;; where a path leads is asked again until it is had, never kept as not there; a write where it cannot be
  ;; had, on a machine an evidence directory is named on, is refused
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (false (tuatara.defaults::remote-kept-real-path "box" (concatenate 'string there "later/inner")))
          (ensure-directories-exist (merge-pathnames "later/inner/" dir))
          (is string= (concatenate 'string there "later/inner")
              (tuatara.defaults::remote-kept-real-path "box" (concatenate 'string there "later/inner")))
          (ensure-directories-exist (merge-pathnames "seized/" dir))
          (ensure-directories-exist (merge-pathnames "locked/sub/" dir))
          (put-file (merge-pathnames "local.txt" dir) "x")
          (sb-posix:chmod (concatenate 'string there "locked") 0)
          (unwind-protect
               (with-evidence-directories ((list (format nil "/ssh:box:~aseized/" there)))
                 (true (search "evidence" (rimu-failure :copy (concatenate 'string there "local.txt")
                                                        (format nil "/ssh:box:~alocked/sub/new.txt" there)))))
            (sb-posix:chmod (concatenate 'string there "locked") #o700))))))

(define-test (remote-files deep-or-crowded-directory-not-moved-unlooked)
  ;; a directory whose directories cannot all be looked through for evidence, too deep or listed cut short, is not
  ;; moved whole
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((remote (concatenate 'string "/ssh:box:" (namestring dir))))
          (ensure-directories-exist (merge-pathnames "deep/a/b/c/" dir))
          (dolist (name '("x/" "y/" "z/"))
            (ensure-directories-exist (merge-pathnames (concatenate 'string "wide/" name) dir)))
          (with-evidence-directories ((list "/ssh:box:/no-such-evidence/"))
            (let ((tuatara.defaults::*remote-tree-depth* 2))
              (true (search "directories deep" (rimu-failure :move (concatenate 'string remote "deep")
                                                         (concatenate 'string remote "deep2")))))
            (let ((tuatara.defaults::*sftp-most-listed* 2))
              (true (search "too many entries" (rimu-failure :move (concatenate 'string remote "wide")
                                                             (concatenate 'string remote "wide2"))))))
          (true (uiop:directory-exists-p (merge-pathnames "deep/a/b/c/" dir)))
          (true (uiop:directory-exists-p (merge-pathnames "wide/x/" dir)))))))

(define-test (remote-files evidence-told-again-at-save)
  ;; a directory named evidence after the file was opened keeps the save from writing there
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (file (concatenate 'string there "later/notes.txt"))
               (name (concatenate 'string "/ssh:box:" file)))
          (ensure-directories-exist (merge-pathnames "later/" dir))
          (alexandria:write-string-into-file "one" file)
          (unwind-protect
               (progn
                 (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name))))
                 (with-evidence-directories ((list (format nil "/ssh:box:~alater/" there)))
                   (remote-session (format nil "C-x b ~a RET M->" (typed-keys "notes.txt")) (typed-keys "!") "C-x C-s"
                                   (list :until (lambda ()
                                                  (search "is not written there"
                                                          (tuatara.buffer:buffer-string
                                                           (tuatara.command::messages-buffer)))))))
                 (is string= "one" (file-text file)))
            (kill-remote-buffers))))))

(define-test (remote-files in-place-save-left-partly-written)
  ;; a file written in place is first found the very file looked at; one left partly written is said so, and
  ;; the next save writes it whole after a yes
  (let ((server (sftp-server-program)) (python (tuatara.io:find-program "python3")))
    (if (not (and server python))
        (skip "no sftp-server or python3 here" (true nil))
        (with-stand-in-sftp (dir log)
          (let* ((there (namestring dir))
                 (linked (concatenate 'string there "linked.txt"))
                 (local (concatenate 'string there "local"))
                 (proxy (put-file (merge-pathnames "proxy.py" dir) *refusing-write-proxy*)))
            (alexandria:write-string-into-file "as it was" linked)
            (sb-posix:link linked (concatenate 'string there "other.txt"))
            (put-file local "new text")
            (flet ((upload (&optional partly)
                     (let ((fd (sb-posix:open local sb-posix:o-rdonly))
                           (like (tuatara.defaults::remote-attributes "box" linked)))
                       (unwind-protect
                            (remote-failure (lambda ()
                                              (tuatara.defaults::remote-upload "box" linked fd 8 :like like
                                                                               :expected (constantly nil)
                                                                               :partly (or partly (list nil)))))
                         (sb-posix:close fd)))))
              ;; the file opened found other than the one looked at
              (sb-int:encapsulate 'tuatara.defaults::sftp-attributes 'other
                                  (lambda (inner link path &rest arguments)
                                    (let ((attributes (apply inner link path arguments)))
                                      (if (getf arguments :handle)
                                          (list* :size 1 attributes)
                                          attributes))))
              (unwind-protect
                   (true (search "not the one found there" (upload)))
                (sb-int:unencapsulate 'tuatara.defaults::sftp-attributes 'other))
              (is string= "as it was" (file-text linked))
              ;; a write refused half-way, through a server that fails the first write
              (tuatara.defaults::close-remote-links)
              (setf tuatara.defaults::*ssh-program*
                    (put-program (merge-pathnames "ssh2" dir)
                                 (format nil "#!/bin/sh~%if [ -e '~amore' ]; then exec '~a'; fi~%: > '~amore'~%~
                                              exec '~a' '~a' '~a'~%"
                                         there server there python (namestring proxy) server)))
              (let ((partly (list nil)))
                (false (eq :none (upload partly)))
                (true (car partly) "said to be partly written"))))
          ;; the next save asks, then writes it whole
          (setf tuatara.defaults::*ssh-program* (namestring (merge-pathnames "ssh" dir)))
          (let* ((file (concatenate 'string (namestring dir) "half.txt"))
                 (name (concatenate 'string "/ssh:box:" file)))
            (alexandria:write-string-into-file "first" file)
            (unwind-protect
                 (progn
                   (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name))))
                   (setf (tuatara.defaults::remote-file-partly
                          (tuatara.defaults::buffer-remote (remote-buffer-of name)))
                         t)
                   (alexandria:write-string-into-file "fir" file :if-exists :supersede)
                   (remote-session (format nil "C-x b ~a RET M->" (typed-keys "half.txt")) (typed-keys "!") "C-x C-s"
                                   (list :until (lambda ()
                                                  (tuatara.frontend.fake:fake-frame-showing-across-rows
                                                   (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                   "partly written")))
                                   "y e s RET"
                                   (list :until (lambda () (string= "first!" (file-text file)))))
                   (is string= "first!" (file-text file))
                   (false (tuatara.defaults::remote-file-partly
                           (tuatara.defaults::buffer-remote (remote-buffer-of name)))))
              (kill-remote-buffers)))))))

(define-test (remote-files kept-candidates-let-go)
  ;; the candidates of directories there are kept only while they may be given again
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (ensure-directories-exist (merge-pathnames "a/" dir))
        (ensure-directories-exist (merge-pathnames "b/" dir))
        (let ((tuatara.defaults::*remote-candidates-seconds* 0)
              (kept tuatara.defaults::**remote-candidates-kept**))
          (clrhash kept)
          (tuatara.defaults::remote-candidates (format nil "/ssh:box:~aa/" (namestring dir)))
          (sleep 0.01)
          (tuatara.defaults::remote-candidates (format nil "/ssh:box:~ab/" (namestring dir)))
          (is = 1 (hash-table-count kept))))))

(define-test (remote-files handle-let-go-once)
  ;; a handle is closed on the server once, even when a stop is signalled and goes unhandled
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (is = 1 (calls-of 'tuatara.defaults::sftp-let-go
                            (lambda ()
                              (remote-failure
                               (lambda ()
                                 (tuatara.defaults::call-with-sftp
                                  "box" there "list"
                                  (lambda (link)
                                    (tuatara.defaults::sftp-with-handle
                                     link :opendir (list there) there "list"
                                     (lambda (handle)
                                       (declare (ignore handle))
                                       (signal 'tuatara.runtime:job-stopped)
                                       :went-on)))))))))))))

;;; What ssh asks, in the minibuffer

(defparameter *key-question*
  "The authenticity of host 'box (10.0.0.5)' can't be established.
ED25519 key fingerprint is SHA256:Xq9pLq3TgJzqK2w8mYb7Lk0dQmZp5nR1vYt3sJk0aB4.
Are you sure you want to continue connecting (yes/no/[fingerprint])? "
  "What OpenSSH asks of a machine whose key it has not seen.")

(defun asking-before (dir question)
  "Shell lines for a stand-in ssh in DIR that asks QUESTION as OpenSSH does,
through SSH_ASKPASS, writes the answer in said, and goes no further without one."
  (format nil "a=$(\"$SSH_ASKPASS\" '~a') || exit 255~%printf '%s\\n' \"$a\" > '~asaid'"
          (cl-ppcre:regex-replace-all "'" question (list "'\\''")) (namestring dir)))

(defun minibuffer-up-p ()
  "True while the running editor reads an answer ssh asked for in the minibuffer."
  (let ((editor (tuatara.command:running-editor)))
    (and (tuatara.command:editor-minibuffer-active editor)
         (tuatara.buffer:find-buffer "*Question*"))))

(defun failed-visits ()
  "The buffers of visits of n.txt on another machine that failed, saying why in the buffer."
  (remove-if-not (lambda (buffer)
                   (and (uiop:string-prefix-p "n.txt" (tuatara.buffer:buffer-name buffer))
                        (search "not connected" (tuatara.buffer:buffer-string buffer))))
                 tuatara.buffer:*buffers*))

(defun kill-failed-visits ()
  "Kill the buffers FAILED-VISITS gives, and those of visits that did not fail."
  (mapc #'tuatara.buffer:kill-buffer (failed-visits))
  (kill-remote-buffers))

(defun said-text (dir)
  "What the stand-in ssh got as an answer, in DIR, or NIL before it has one."
  (let ((file (merge-pathnames "said" dir)))
    (and (probe-file file) (uiop:read-file-string file))))

(define-test (remote-files ssh-password-asked-in-the-minibuffer)
  ;; ssh's question reaches the minibuffer from the job, naming the host; the answer goes to ssh and nowhere else
  (when (sftp-server-program)
    (let ((secret "hunter2-not-kept") (seconds tuatara.defaults::*remote-seconds*))
      (with-stand-in-sftp (dir log :before (asking-before dir "me@box's password: "))
        (let* ((there (namestring dir)) (file (concatenate 'string there "n.txt"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "text" file)
          (setf tuatara.defaults::*remote-seconds* 3)
          (let ((tuatara.runtime:*debug-log-path* (namestring (merge-pathnames "debug.log" dir)))
                (tuatara-user:*debug-mode* t))
            (unwind-protect
                 (let ((frontend
                         (remote-session
                          (visit-keys name) (list :until #'minibuffer-up-p 30)
                          (list :wait 4) (typed-keys secret) "RET"
                          (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                    (and buffer (search "text" (tuatara.buffer:buffer-string buffer)))))
                                30))))
                   (true (tuatara.frontend.fake:fake-frame-showing frontend "The server asks: me@box's password"))
                   (is string= (format nil "~a~%" secret) (said-text dir))
                   (false (search secret (tuatara.buffer:buffer-string (tuatara.command::messages-buffer))))
                   (false (search secret (format nil "~s" tuatara.command::*minibuffer-history*)))
                   (when (probe-file (merge-pathnames "debug.log" dir))
                     (false (search secret (uiop:read-file-string (merge-pathnames "debug.log" dir)))))
                   ;; waiting on the person did not run out *REMOTE-SECONDS*
                   (true (search "text" (tuatara.buffer:buffer-string (remote-buffer-of name)))))
              (setf tuatara.defaults::*remote-seconds* seconds)
              (kill-remote-buffers))))))))

(define-test (remote-files ssh-key-question-typed)
  ;; a new host's key: the question with its fingerprint in full, typed yes or no; no connection without yes
  (when (sftp-server-program)
    (dolist (answer '("yes" "y" "no"))
      (with-stand-in-sftp (dir log :before (asking-before dir *key-question*))
        (let* ((there (namestring dir)) (file (concatenate 'string there "n.txt"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "text" file)
          (unwind-protect
               (let ((frontend (remote-session (visit-keys name) (list :until #'minibuffer-up-p 30)
                                               (typed-keys answer) "RET"
                                               (list :until (lambda () (or (said-text dir)
                                                                           (failed-visits)))
                                                     30))))
                 (true (tuatara.frontend.fake:fake-frame-showing-across-rows
                        frontend "SHA256:Xq9pLq3TgJzqK2w8mYb7Lk0dQmZp5nR1vYt3sJk0aB4"))
                 (true (search "StrictHostKeyChecking=ask" (uiop:read-file-string log)))
                 (false (search "BatchMode" (uiop:read-file-string log)))
                 (if (string= answer "yes")
                     (is string= (format nil "yes~%") (said-text dir))
                     (false (said-text dir))))
            (kill-failed-visits)))))))

(define-test (remote-files ssh-question-given-up)
  ;; C-g answers nothing, and the connection fails, said plainly
  (when (sftp-server-program)
    (with-stand-in-sftp (dir log :before (asking-before dir "Enter passphrase for key '/h/.ssh/id': "))
      (let ((name (format nil "/ssh:box:~an.txt" (namestring dir))))
        (unwind-protect
             (progn
               (remote-session (visit-keys name) (list :until #'minibuffer-up-p 30) "C-g"
                               (list :until #'failed-visits 30))
               (true (failed-visits))
               (false (said-text dir))
               (false (remote-buffer-of name)))
          (kill-failed-visits))))))

(define-test (remote-files ssh-question-cleaned)
  ;; what a server puts in a question loses its control characters before it is shown
  (is string= "Password[31m for box:" (tuatara.defaults::ssh-question-words
                                         (format nil "Password~c[31m~c for box:" (code-char 27) (code-char #x202e))))
  (is string= "a b" (tuatara.defaults::ssh-question-words (format nil "a~%~%b~%"))))

(define-test (remote-files ssh-connecting-said)
  ;; a slow connection says so in the echo area, and no longer once done
  (when (sftp-server-program)
    (with-stand-in-sftp (dir log :before "sleep 2")
      (let* ((file (format nil "~an.txt" (namestring dir))) (name (format nil "/ssh:box:~a" file)))
        (alexandria:write-string-into-file "text" file)
        (unwind-protect
             (let ((frontend
                     (remote-session (visit-keys name)
                                     (list :until (lambda ()
                                                    (let ((buffer (remote-buffer-of name)))
                                                      (and buffer
                                                           (search "text" (tuatara.buffer:buffer-string buffer))
                                                           (not (equal "Connecting to box..."
                                                                       (tuatara.command:editor-message
                                                                        (tuatara.command:running-editor)))))))
                                           30))))
               (true (tuatara.frontend.fake:fake-frame-showing frontend "Connecting to box...")))
          (kill-remote-buffers))))))

(define-test (remote-files changed-host-key-said-plainly)
  ;; a changed key is never offered: ssh refuses it, and the failure says so
  (when (sftp-server-program)
    (with-stand-in-sftp (dir log
                             :before "echo '@@@ WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED! @@@' >&2; exit 255")
      (let ((message (remote-failure (lambda () (tuatara.defaults::remote-attributes "box" "/")))))
        (true (search "differs from the one ssh knows" message))))))

(define-test (remote-files git-ssh-key-question-answered)
  ;; git fetch over ssh: ssh's question about a new key, its fingerprint shown, reaches the minibuffer from the
  ;; job and the answer goes back to ssh; refused, the echo area says the key was not accepted
  (dolist (answer '("yes" "no"))
    (with-stand-in-sftp (dir log :before (format nil "a=$(\"$SSH_ASKPASS\" '~a') || ~
                                                    { echo 'Host key verification failed.' >&2; exit 255; }~%~
                                                    printf '%s\\n' \"$a\" > '~asaid'~%~
                                                    echo 'fatal: the stand-in serves no repository' >&2; exit 128"
                                                 (cl-ppcre:regex-replace-all "'" *key-question* (list "'\\''"))
                                                 (namestring dir)))
      (let ((repo (merge-pathnames "repo/" dir)))
        (assert (tuatara.defaults::plain-path-p tuatara.defaults::*ssh-program*) ()
                "the stand-in ssh must be named in git's ssh command, never the system's")
        (ensure-directories-exist repo)
        (scratch-repository repo)
        (uiop:run-program (list "git" "-C" (namestring repo) "remote" "add" "origin" "box:repo.git"))
        (unwind-protect
             (let ((frontend (git-session repo "a.txt"
                                          (list "f u" (list :until #'minibuffer-up-p 30) (typed-keys answer) "RET"
                                                (list :until (lambda () (search "git fetch:" (messages-since-step)))
                                                      30)))))
               (true (tuatara.frontend.fake:fake-frame-showing frontend "ssh to the server of git fetch"))
               (true (tuatara.frontend.fake:fake-frame-showing-across-rows
                      frontend "SHA256:Xq9pLq3TgJzqK2w8mYb7Lk0dQmZp5nR1vYt3sJk0aB4"))
               (true (search "StrictHostKeyChecking=ask" (uiop:read-file-string log)))
               (if (string= answer "yes")
                   (is string= (format nil "yes~%") (said-text dir))
                   (progn (false (said-text dir))
                          (true (search "git fetch: the server's key was not accepted"
                                        (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)))))))
          (kill-buffers-visiting repo))))))

(define-test (remote-files ssh-question-kinds)
  ;; only ssh's own shapes take the host-key or local form; a server's question, prefixed by ssh, never does
  (is eq :key (tuatara.defaults::ssh-question-kind *key-question*))
  (is eq :local (tuatara.defaults::ssh-question-kind "Enter passphrase for key '/h/.ssh/id': "))
  (is eq :local (tuatara.defaults::ssh-question-kind "Enter PIN for ED25519-SK key /h/.ssh/id_sk: "))
  (is eq :server (tuatara.defaults::ssh-question-kind "me@box's password: "))
  (is eq :server (tuatara.defaults::ssh-question-kind "(me@box) Enter passphrase for key '/h/.ssh/id': "))
  (is eq :server (tuatara.defaults::ssh-question-kind "(me@box) continue connecting (yes/no)? "))
  (is eq :server (tuatara.defaults::ssh-question-kind "Accept updated hostkeys? (yes/no): "))
  (let ((prompt (tuatara.defaults::ssh-question-prompt "box" nil "Enter passphrase for key '/h/id': " "")))
    (true (search "Your key's passphrase, stays here" prompt)))
  (multiple-value-bind (prompt kind fingerprint details)
      (tuatara.defaults::ssh-question-prompt "box" "gate" "(me@box) Code: " "Use your token."
                                             :warnings '("Not quantum safe."))
    (declare (ignore kind fingerprint))
    (true (uiop:string-prefix-p "The server asks: " prompt) "a server's words are marked as such in the prompt")
    (true (search "what you type goes to the server" details))
    (true (search "Use your token." details) "the server's instructions in full")
    (true (search "Not quantum safe." details))
    (true (search "or the jump host gate" details))
    (false (search "Use your token." prompt)))
  ;; a long question stays whole in the details and cannot push the row typed on off the screen
  (let ((long (make-string 3000 :initial-element #\a)))
    (multiple-value-bind (prompt kind fingerprint details)
        (tuatara.defaults::ssh-question-prompt "box" nil long long)
      (declare (ignore kind fingerprint))
      (true (< (length prompt) 100))
      (true (search long details)))))

(define-test (remote-files details-shown-only-while-asked)
  ;; the details are shown beside the question and go with it, the windows as they were
  (let ((seen nil) (windows nil) (after nil))
    (run-keys (list (lambda ()
                      (setf windows (length (tuatara.command:editor-windows)))
                      (tuatara.defaults::call-with-details-shown
                       "the whole question"
                       (lambda ()
                         (setf seen (list (and (tuatara.buffer:find-buffer "*Question*")
                                               (tuatara.defaults::pieces-text
                                                (list (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Question*")))))
                                          (length (tuatara.command:editor-windows))))))
                      (setf after (length (tuatara.command:editor-windows)))))
              :height 30)
    (true (search "the whole question" (first seen)) "~s" seen)
    (is = (1+ windows) (second seen))
    (is = windows after)
    (false (tuatara.buffer:find-buffer "*Question*"))))

(define-test (remote-files ssh-key-question-as-openssh-10-words-it)
  ;; OpenSSH 10 writes "fingerprint is:" and adds a line about other names: it is still its own question, not hidden
  (let ((question "The authenticity of host 'box (10.0.0.5)' can't be established.
ED25519 key fingerprint is: SHA256:LRMTH6kEUp1dDyMPGJlAvQhgiX4cSO0JX/aPMh350Sk
This key is not known by any other names.
Are you sure you want to continue connecting (yes/no/[fingerprint])? "))
    (is eq :key (tuatara.defaults::ssh-question-kind question))
    (multiple-value-bind (prompt kind fingerprint details) (tuatara.defaults::ssh-question-prompt "box" nil question "")
      (is eq :key kind)
      (is string= "SHA256:LRMTH6kEUp1dDyMPGJlAvQhgiX4cSO0JX/aPMh350Sk" fingerprint)
      (is string= "Accept this key (yes/no/fingerprint): " prompt)
      (true (search "Type yes or the fingerprint" details)))
    (is string= "SHA256:LRMTH6kEUp1dDyMPGJlAvQhgiX4cSO0JX/aPMh350Sk"
        (tuatara.defaults::ssh-key-answer "SHA256:LRMTH6kEUp1dDyMPGJlAvQhgiX4cSO0JX/aPMh350Sk"
                                          "SHA256:LRMTH6kEUp1dDyMPGJlAvQhgiX4cSO0JX/aPMh350Sk"))
    ;; a server's question stays a server's, whatever it says
    (is eq :server (tuatara.defaults::ssh-question-kind
                    "(me@box) The authenticity of host 'x' (yes/no/[fingerprint])? "))))

(define-test (remote-files ssh-warnings-kept-apart-from-what-a-server-says)
  ;; OpenSSH's warning, three lines of **, is not run into a server's banner and cut short
  (multiple-value-bind (warnings said)
      (tuatara.defaults::ssh-said-parts
       (format nil "** WARNING: connection is not using a post-quantum key exchange algorithm.~@
                    ** This session may be vulnerable to \"store now, decrypt later\" attacks.~@
                    ** The server may need to be upgraded. See https://openssh.com/pq.html~@
                    Authorised users only.~%"))
    (is eql 3 (length warnings))
    (is string= "WARNING: connection is not using a post-quantum key exchange algorithm." (first warnings))
    (is string= "Authorised users only." said))
  (multiple-value-bind (warnings said) (tuatara.defaults::ssh-said-parts "")
    (false warnings)
    (is string= "" said)))

(define-test (remote-files ssh-key-answer-exact)
  ;; only yes typed out, or the key's own fingerprint, accepts a new key
  (is equal '(:key "SHA256:Xq9pLq3TgJzqK2w8mYb7Lk0dQmZp5nR1vYt3sJk0aB4")
      (subseq (multiple-value-list (tuatara.defaults::ssh-question-prompt "box" nil *key-question* "")) 1 3))
  (is string= "yes" (tuatara.defaults::ssh-key-answer " yes " "SHA256:x"))
  (is string= "SHA256:x" (tuatara.defaults::ssh-key-answer "SHA256:x" "SHA256:x"))
  (dolist (refused '("y" "Y" "YES" "no" "" "SHA256:y"))
    (false (tuatara.defaults::ssh-key-answer refused "SHA256:x") "~s" refused)))

(define-test (remote-files ssh-answer-script-passes-no-argument)
  ;; the answer reaches ssh from the pipe, through no printf, argument or shell variable holding it
  (true (search "exec head -n 1 <&3" tuatara.defaults::*ssh-asking-script*))
  (false (search "${line" tuatara.defaults::*ssh-asking-script*)))

(define-test (remote-files ssh-options-pinned-when-asking)
  ;; nothing but logging in and a new key is asked: no updated keys, no keys added to an agent
  (let ((options (tuatara.defaults::ssh-asking-options t)))
    (dolist (option '("StrictHostKeyChecking=ask" "UpdateHostKeys=no" "AddKeysToAgent=no"))
      (true (member option options :test #'string=) "~a" option))))

(define-test (remote-files ssh-question-not-asked-after-stop)
  ;; a question whose job stopped is not put; an answer that comes after a stop is wiped
  (let ((awaited (tuatara.defaults::make-awaited-answer)) (asked nil))
    (tuatara.defaults::abandon-awaited awaited)
    (tuatara.defaults::ask-for-awaited awaited (lambda () (setf asked t) "x"))
    (false asked))
  (let ((awaited (tuatara.defaults::make-awaited-answer)) (secret (copy-seq "late")))
    (tuatara.defaults::ask-for-awaited awaited (lambda () (tuatara.defaults::abandon-awaited awaited) secret))
    (false (find #\l secret) "wiped")
    (false (tuatara.defaults::awaited-answer-answer awaited))))

(define-test (remote-files ssh-pipes-taken-away)
  ;; closing takes the pipes away, and a pipe that cannot be made leaves nothing open
  (with-test-directory (dir)
    (let* ((directory (namestring (ensure-directories-exist (merge-pathnames "c/" dir))))
           (asking (tuatara.defaults::open-ssh-asking "/bin/false" directory "box")))
      (true (probe-file (concatenate 'string directory "question")))
      (tuatara.defaults::close-ssh-asking asking)
      (false (probe-file (concatenate 'string directory "question")))
      (false (probe-file (concatenate 'string directory "answer"))))
    (let* ((directory (namestring (ensure-directories-exist (merge-pathnames "d/" dir))))
           (before (length (uiop:directory-files "/proc/self/fd/"))))
      (sb-posix:mkfifo (concatenate 'string directory "answer") #o600)
      (true (search "make a pipe"
                    (handler-case (progn (tuatara.defaults::open-ssh-asking "/bin/false" directory "box") "")
                      (tuatara.io:file-system-error (condition) (princ-to-string condition)))))
      (is = before (length (uiop:directory-files "/proc/self/fd/")) "no descriptor left open"))))

(define-test (remote-files ssh-helper-that-cannot-run-said)
  ;; ssh not able to run its question program is said plainly
  (when (sftp-server-program)
    (with-stand-in-sftp (dir log :before "echo 'ssh_askpass: exec(/c/ask): Permission denied' >&2; exit 255")
      (true (search "could not run the small program"
                    (remote-failure (lambda () (tuatara.defaults::remote-attributes "box" "/"))))))))

(define-test (remote-files nobody-to-ask-in-a-daemon-without-client)
  ;; an editor kept in the background with no client asks nothing, failing at once as BatchMode does
  (let ((editor (tuatara.command::make-editor (tuatara.frontend.fake:make-fake-frontend))))
    (true (tuatara.defaults::someone-to-ask-p editor))
    (setf (tuatara.command::editor-idle-frontend editor)
          (tuatara.command::frame-frontend (tuatara.command::editor-frame editor)))
    (false (tuatara.defaults::someone-to-ask-p editor)))
  (false (tuatara.defaults::someone-to-ask-p nil)))

;;; A large file shown as it arrives

(defun arrival-numbered-text (lines)
  "LINES lines of text, each with its number, the same for the same LINES."
  (with-output-to-string (out)
    (dotimes (i lines) (format out "line ~6,'0d of the numbered text~%" i))))

(defmacro with-arrival-held ((dir name release &key (lines 90000)) &body body)
  "Run BODY with the file NAME, /ssh:box:DIR/a.log of LINES numbered lines (more
than *MEMORY-READ-LIMIT*, here 1 MB, so it opens before it has all come), to be
visited. A transfer is held after each lot (*ARRIVAL-HOOK*) until RELEASE, a
function of no arguments, is called, which lets the transfer run to its end
\(a number given to it lets through that many lots). Every remote buffer is
killed after."
  (let ((gate (gensym "GATE")) (open (gensym "OPEN")) (old-limit (gensym "LIMIT")) (count (gensym "COUNT")))
    `(if (null (sftp-server-program))
         (skip "no sftp-server here" (true nil))
         (with-stand-in-sftp (,dir log)
           (let* ((,gate (sb-thread:make-semaphore))
                  (,open nil)
                  (,old-limit tuatara.io:*memory-read-limit*)
                  (file (concatenate 'string (namestring ,dir) "a.log"))
                  (,name (concatenate 'string "/ssh:box:" file)))
             (flet ((,release (&optional ,count)
                      (if ,count
                          (sb-thread:signal-semaphore ,gate ,count)
                          (progn (setf ,open t) (sb-thread:signal-semaphore ,gate 1000)))))
               (alexandria:write-string-into-file (arrival-numbered-text ,lines) file)
               (unwind-protect
                    (progn
                      (setf tuatara.io:*memory-read-limit* (* 1024 1024)
                            tuatara.defaults::*arrival-hook*
                            (lambda ()
                              (loop until (or ,open (car (tuatara.runtime:job-cancel tuatara.runtime:*job*))
                                              (sb-thread:wait-on-semaphore ,gate :timeout 1/20)))))
                      ,@body)
                 (setf tuatara.defaults::*arrival-hook* nil
                       tuatara.io:*memory-read-limit* ,old-limit
                       ,open t)
                 (sb-thread:signal-semaphore ,gate 1000)
                 (kill-remote-buffers))))))))

(defun arrival-frontend ()
  "The fake frontend of the editor running."
  (tuatara.command::frame-frontend (tuatara.command::editor-frame tuatara.command:*editor*)))

(define-test (remote-files arrival-order-of-pieces)
  ;; what a window wants first, then the start, then the end, then on from the start
  (let* ((piece tuatara.defaults::+sftp-piece+)
         (size (* 200 piece))
         (arrival (tuatara.defaults::make-arrival size)))
    (setf (tuatara.defaults::arrival-end arrival) size)
    (is = 0 (tuatara.defaults::arrival-next arrival '()))
    (is = piece (tuatara.defaults::arrival-next arrival (list 0)) "not one asked already")
    (tuatara.defaults::arrival-add arrival 0 tuatara.defaults::+remote-first-part+)
    (is = (- size tuatara.text:+kept-ends+) (* piece (floor (tuatara.defaults::arrival-next arrival '()) piece))
        "the end after the start")
    (setf (tuatara.defaults::arrival-wanted arrival) (cons (* 100 piece) (* 101 piece)))
    (is = (* 100 piece) (tuatara.defaults::arrival-next arrival '()) "a window's wish first")
    (setf (tuatara.defaults::arrival-wanted arrival) nil)
    (tuatara.defaults::arrival-add arrival (- size tuatara.text:+kept-ends+) size)
    (is = tuatara.defaults::+remote-first-part+ (tuatara.defaults::arrival-next arrival '()) "then on from the start")
    (is equal (list (cons tuatara.defaults::+remote-first-part+ (- size tuatara.text:+kept-ends+)))
        (tuatara.defaults::arrival-holes arrival))
    (tuatara.defaults::arrival-add arrival 0 size)
    (true (tuatara.defaults::arrival-complete-p arrival))
    (false (tuatara.defaults::arrival-next arrival '()))
    (false (tuatara.defaults::arrival-holes arrival)))
  ;; opening stops after the start and the last piece; the middle waits
  (let* ((piece tuatara.defaults::+sftp-piece+)
         (size (* 200 piece))
         (arrival (tuatara.defaults::make-arrival size :opening t)))
    (setf (tuatara.defaults::arrival-end arrival) size)
    (tuatara.defaults::arrival-add arrival 0 tuatara.defaults::+remote-first-part+)
    (is = (- size tuatara.defaults::+remote-tail-opening+) (tuatara.defaults::arrival-next arrival '()))
    (tuatara.defaults::arrival-add arrival (- size tuatara.defaults::+remote-tail-opening+) size)
    (false (tuatara.defaults::arrival-next arrival '()) "enough to open"))
  ;; a size no copy could be mapped at is refused before anything is made of it
  (fail (tuatara.defaults::remote-early-p (ash 1 47) nil) 'tuatara.runtime:refusal))

(define-test (remote-files large-file-shown-before-it-has-all-come)
  ;; the buffer opens with the first part and the end; the rest shows as on its way, then fills in,
  ;; read-only and unsaved meanwhile, the copy the whole size from the start, and whole at the end
  (let (seen-first)
    (with-arrival-held (dir name release)
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      (list :until (lambda ()
                                     (let* ((f (arrival-frontend)) (buffer (remote-buffer-of name)))
                                       (setf seen-first (tuatara.frontend.fake:fake-frame-showing f "line 000003"))
                                       (false (search "arriving" (tuatara.frontend.fake:fake-screen-line f 18))
                                              "while it arrives, only the sign P")
                                       (is = (length (arrival-numbered-text 90000)) (sb-posix:stat-size (sb-posix:stat (tuatara.io:native (tuatara.buffer:buffer-file buffer))))
                                           "the copy its full size at once")
                                       (true (tuatara.buffer:buffer-read-only-p buffer))
                                       (false (tuatara.buffer:buffer-indexed-p buffer) "lines not counted yet")
                                       t)))
                      "M->" "C-x C-s"
                      (list :until (lambda ()
                                     (true (search "still arriving" (messages-since-step)) "a save refused, saying so")
                                     (true (tuatara.frontend.fake:fake-frame-showing (arrival-frontend) "line 089999")
                                           "the last lines shown, read before the middle")
                                     t))
                      ;; found where it has come; not gone looking for through what has not
                      "M-<" "C-s l i n e" "C-g" "M-<" "C-s z z z z z"
                      (list :until (lambda ()
                                     (true (search "has not arrived" (messages-since-step)) "search refused meanwhile")
                                     (release)
                                     t))
                      "C-g"
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (null (tuatara.defaults::remote-file-arrival
                                                                   (tuatara.defaults::buffer-remote buffer))))))
                            120)
                      (list :until (lambda () (tuatara.buffer:buffer-indexed-p (remote-buffer-of name))) 120)
                      ;; saved as a file read whole is
                      "M->" "x" "C-x C-s"
                      (list :until (lambda () (uiop:string-suffix-p (file-text (concatenate 'string (namestring dir) "a.log")) "x"))
                            60))
      (let* ((buffer (remote-buffer-of name)))
        (true seen-first "the first part shown before the end")
        (is string= (concatenate 'string (arrival-numbered-text 90000) "x") (tuatara.buffer:buffer-string buffer))
        (false (tuatara.buffer:buffer-read-only-p buffer) "editable once all has come")
        (is = 90001 (tuatara.buffer:buffer-line-count buffer))))))

(define-test (remote-files arrival-kept-when-it-fails)
  ;; the connection lost mid-way: what came stays readable, marked incomplete, never saved, read again in full
  (with-arrival-held (dir name release)
    (let ((after nil))
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      ;; the server goes while the transfer is held
                      (list :until (lambda ()
                                     (sb-alien:alien-funcall
                                      (sb-alien:extern-alien "shutdown"
                                                             (function sb-alien:int sb-alien:int sb-alien:int))
                                      (tuatara.defaults::sftp-link-fd (tuatara.defaults::sftp-link "box")) 2)
                                     (release)
                                     t))
                      (list :until (lambda () (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t))))
                      "C-x C-s"
                      (list :until (lambda ()
                                     (setf after (messages-since-step))
                                     (setf tuatara.defaults::*arrival-hook* nil)
                                     t)))
      (let ((buffer (remote-buffer-of name)))
        (true (tuatara.defaults::remote-file-arrival (tuatara.defaults::buffer-remote buffer)) "marked incomplete")
        (true (search "only part of it arrived" after) "never saved")
        (true (tuatara.buffer:buffer-read-only-p buffer))))))

(define-test (remote-files killing-stops-the-arrival)
  ;; the buffer killed: its job ends and the copy's directory is taken away
  (with-arrival-held (dir name release)
    (let (copy)
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (when (and buffer (tuatara.defaults::remote-file-arrival
                                                                   (tuatara.defaults::buffer-remote buffer)))
                                                  (setf copy (tuatara.io:native (tuatara.buffer:buffer-file buffer)))
                                                  t))))
                      (list :until (lambda () (tuatara.buffer:kill-buffer (remote-buffer-of name)) t))
                      (list :until (lambda () (not (probe-file copy))) 20)
                      (list :until (lambda ()
                                     (notany (lambda (job) (typep job 'tuatara.defaults::remote-read-job))
                                             tuatara.runtime:*jobs*))
                            20))
      (false (probe-file copy) "the copy gone with its buffer"))))

(define-test (remote-files other-work-on-the-server-goes-on)
  ;; a long transfer leaves the connection to other work between its lots
  (with-arrival-held (dir name release)
    (let (other)
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      (list :until (lambda ()
                                     (setf other (tuatara.runtime:make-guarded-thread
                                                  (lambda ()
                                                    (tuatara.defaults::remote-attributes "box" (namestring dir)))
                                                  :name "other work"))
                                     t))
                      ;; the transfer goes on one lot at a time: the other work gets in between them
                      (list :until (lambda ()
                                     (release 1)
                                     (not (sb-thread:thread-alive-p other)))
                            30)
                      (list :until (lambda ()
                                     (release)
                                     (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t)))
                            60))
      (false (sb-thread:thread-alive-p other) "the other work finished while the transfer went on"))))

(define-test (remote-files place-not-here-asked-for-first)
  ;; going to a place not here shows it as on its way and asks for it first; it fills in when it has come
  (with-arrival-held (dir name release)
    (let ((place (* 2 1024 1024)) saw-way saw-text)
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      (list :until (lambda ()
                                     (setf (tuatara.buffer:buffer-point (remote-buffer-of name)) place)
                                     t))
                      (list :until (lambda ()
                                     (setf saw-way
                                           (tuatara.frontend.fake:fake-frame-showing (arrival-frontend) "on its way"))
                                     (let ((arrival (tuatara.defaults::remote-file-arrival
                                                     (tuatara.defaults::buffer-remote (remote-buffer-of name)))))
                                       (and saw-way (tuatara.defaults::arrival-wanted arrival))))
                            60)
                      (list :until (lambda () (release 1) t))
                      (list :until (lambda ()
                                     (let ((arrival (tuatara.defaults::remote-file-arrival
                                                     (tuatara.defaults::buffer-remote (remote-buffer-of name)))))
                                       (null (tuatara.defaults::arrival-absent arrival place (+ place 1) nil))))
                            60)
                      ;; going on from inside the hole goes past it, not an octet at a time
                      (list :until (lambda ()
                                     (setf (tuatara.buffer:buffer-point (remote-buffer-of name)) (- place 1000))
                                     t))
                      "C-f"
                      (list :until (lambda ()
                                     (is = place (tuatara.buffer:buffer-point (remote-buffer-of name))
                                         "C-f past the hole at once")
                                     t))
                      ;; a few lines on from the place, within what came
                      (list :until (lambda ()
                                     (setf (tuatara.buffer:buffer-point (remote-buffer-of name)) (+ place 200))
                                     t))
                      (list :until (lambda ()
                                     (setf saw-text
                                           (tuatara.frontend.fake:fake-frame-showing (arrival-frontend) "line 0635"))
                                     saw-text)
                            60)
                      (list :until (lambda ()
                                     (release)
                                     (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t)))
                            60))
      (true saw-way "the place not here says so")
      (true saw-text "and fills in once it has come"))))

(define-test (remote-files arrival-stopped-then-read-again)
  ;; stopped by hand: what came stays, marked incomplete; read again, the whole file takes its place
  (with-arrival-held (dir name release)
    (let (stopped generation)
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      (list :until (lambda ()
                                     (setf generation (tuatara.defaults::sftp-link-generation
                                                       (tuatara.defaults::sftp-link "box")))
                                     (tuatara.runtime:stop-and-settle
                                      (first (tuatara.runtime:jobs-of (remote-buffer-of name) :running t)))
                                     (is = generation (tuatara.defaults::sftp-link-generation
                                                       (tuatara.defaults::sftp-link "box"))
                                         "a job stopped leaves the connection to others")
                                     t))
                      (list :until (lambda () (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t))))
                      (list :until (lambda ()
                                     (setf stopped
                                           (tuatara.frontend.fake:fake-frame-showing (arrival-frontend) "incomplete"))
                                     (setf tuatara.defaults::*arrival-hook* nil)
                                     stopped))
                      (format nil "M-x ~a RET" (typed-keys "revert-buffer"))
                      (list :until (lambda ()
                                     (null (tuatara.defaults::remote-file-arrival
                                            (tuatara.defaults::buffer-remote (remote-buffer-of name)))))
                            60))
      (true stopped "the mode line says incomplete")
      (is string= (arrival-numbered-text 90000) (tuatara.buffer:buffer-string (remote-buffer-of name))))))

(define-test (remote-files reading-a-hole-refused)
  ;; zeros are never given as the file's contents: a copy across a hole, or a slice of one, is refused; around it, not
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "h" dir))
           (octets (make-array 200000 :element-type '(unsigned-byte 8) :initial-element 65)))
      (alexandria:write-byte-vector-into-file octets file)
      (let* ((mapping (tuatara.text:map-file (namestring file)))
             (rope (tuatara.text:mapping-rope mapping)))
        (unwind-protect
             (progn
               (setf (tuatara.text:mapping-holes mapping) (list (cons 100000 150000)))
               (is = 65 (aref (tuatara.text:rope-octets rope :start 0 :end 100000) 99999) "the part here")
               (fail (tuatara.text:rope-octets rope :start 99999 :end 100001) 'tuatara.text:not-arrived)
               (fail (tuatara.text:rope-slice rope 90000 160000) 'tuatara.text:not-arrived)
               (true (tuatara.text:rope-slice rope 150000 160000))
               ;; a display reading around it may
               (let ((out (make-array 16 :element-type '(unsigned-byte 8))))
                 (is = 16 (tuatara.text:rope-copy-into rope 100000 100016 out 0 t)))
               ;; lines do not go over it, and it says where
               (is equal '(100000 . 150000) (tuatara.text:rope-hole-at rope 120000))
               (false (tuatara.text:rope-hole-at rope 90000))
               (is = 100000 (tuatara.text:rope-line-end rope 30000) "a line stops at the hole's edge")
               (is = 150000 (tuatara.text:rope-line-start rope 180000) "and starts after it")
               ;; a package's reading of octets refuses too
               (is = 65 (tuatara::rope-octet rope 90000))
               (fail (tuatara::rope-octet rope 120000) 'tuatara.text:not-arrived)
               (fail (tuatara::rope-find-octet rope 10 :start 90000 :end 120000) 'tuatara.text:not-arrived)
               (is equal '(90000 . 100000) (tuatara.text:rope-arrived-around rope 95000 90000 120000))
               (is equal '(120000 . 120000) (tuatara.text:rope-arrived-around rope 120000 90000 130000)
                   "a stretch starting at a hole")
               (is = 100000 (tuatara.text:rope-line-start rope 120000) "inside the hole, its start")
               (is = 150000 (tuatara.text:rope-line-end rope 120000) "and its end, at once"))
          (tuatara.text:close-mapping mapping))))))

(define-test (remote-files hex-view-shows-holes)
  ;; octets that have not come are not shown as zeros
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "h" dir))
           (octets (make-array 200000 :element-type '(unsigned-byte 8) :initial-element 65)))
      (alexandria:write-byte-vector-into-file octets file)
      (let* ((mapping (tuatara.text:map-file (namestring file)))
             (rope (tuatara.text:mapping-rope mapping)))
        (unwind-protect
             (progn
               (setf (tuatara.text:mapping-holes mapping) (list (cons 100000 150000)))
               (let* ((rows (tuatara.display::hex-layout rope 100000 1))
                      (row (tuatara.display:row-string (aref rows 0))))
                 (true (search "--" row) "the hole in the hex digits")
                 (false (search "00" (subseq row 12)) "no zeros taken for it")))
          (tuatara.text:close-mapping mapping))))))

(define-test (remote-files arriving-file-not-opened-by-name)
  ;; what opens a copy still arriving by its path is refused too
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "h" dir)))
           (buffer (progn (alexandria:write-string-into-file "text" file) (tuatara.buffer:make-buffer "h" :file file))))
      (unwind-protect
           (progn
             (setf (tuatara.io:buffer-arriving-p buffer) t)
             (fail (tuatara.io:open-for-reading file t) 'tuatara.text:not-arrived)
             (setf (tuatara.io:buffer-arriving-p buffer) nil)
             (sb-posix:close (tuatara.io:open-for-reading file t)))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (remote-files undo-does-not-bring-holes-back)
  ;; a buffer incomplete, then read again: undo cannot bring the text with holes back
  (with-arrival-held (dir name release)
    (remote-session (visit-keys name)
                    (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                              (and buffer (tuatara.defaults::remote-file-arrival
                                                           (tuatara.defaults::buffer-remote buffer))))))
                    (list :until (lambda ()
                                   (tuatara.runtime:stop-and-settle
                                    (first (tuatara.runtime:jobs-of (remote-buffer-of name) :running t)))
                                   (setf tuatara.defaults::*arrival-hook* nil)
                                   t))
                    (list :until (lambda () (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t))))
                    (format nil "M-x ~a RET" (typed-keys "revert-buffer"))
                    (list :until (lambda ()
                                   (null (tuatara.defaults::remote-file-arrival
                                          (tuatara.defaults::buffer-remote (remote-buffer-of name)))))
                          60))
    (is equal '() (tuatara.buffer:buffer-undo-stack (remote-buffer-of name)) "no way back to the text with holes")))

(define-test (remote-files changed-there-while-arriving-stays-marked)
  ;; a file changed at the server during the transfer never ends as arrived
  (with-arrival-held (dir name release)
    (let ((file (concatenate 'string (namestring dir) "a.log")))
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      (list :until (lambda ()
                                     (sb-posix:utimes file 1000000 1000000)
                                     (release)
                                     t))
                      (list :until (lambda () (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t))) 60))
      (let ((buffer (remote-buffer-of name)))
        (true (tuatara.defaults::remote-file-arrival (tuatara.defaults::buffer-remote buffer))
              "still marked incomplete")
        (true (tuatara.buffer:buffer-read-only-p buffer))
        ;; every part is here, but no one version of the file: nothing is read from it as its text
        (fail (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start 0 :end 10)
            'tuatara.text:not-arrived)
        (true (search "changed on the server" (handler-case
                                                  (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)
                                                                            :start 0 :end 10)
                                                (tuatara.text:not-arrived (c) (princ-to-string c)))))))))

(define-test (remote-files new-connection-handle-not-used)
  ;; the connection closed and made again while a transfer gives it up: its handle is not used on the new one
  (with-stand-in-sftp (dir log)
    (tuatara.defaults::remote-attributes "box" (namestring dir))
    (let* ((link (tuatara.defaults::sftp-link "box"))
           (generation (tuatara.defaults::sftp-link-generation link)))
      (tuatara.defaults::sftp-close link)
      (is = (1+ generation) (tuatara.defaults::sftp-link-generation link) "closing counts up"))))

(define-test (remote-files copying-across-a-hole-refused)
  ;; a copy that reaches what has not come says so, and the kill ring holds no zeros
  (with-arrival-held (dir name release)
    (let ((said nil))
      (remote-session (visit-keys name)
                      (list :until (lambda () (let ((buffer (remote-buffer-of name)))
                                                (and buffer (tuatara.defaults::remote-file-arrival
                                                             (tuatara.defaults::buffer-remote buffer))))))
                      "C-SPC M-> M-w"
                      (list :until (lambda ()
                                     (setf said (messages-since-step))
                                     (release)
                                     t))
                      (list :until (lambda () (null (tuatara.runtime:jobs-of (remote-buffer-of name) :running t))) 60))
      (true (search "has not arrived" said) "the copy refused, saying so"))))

(define-test (remote-files room-kept-for-the-rest)
  ;; a transfer stops before the file system is left with less than the margin
  (with-test-directory (dir)
    (let ((copy (namestring (merge-pathnames "c" dir))))
      (alexandria:write-string-into-file "x" copy)
      (true (null (tuatara.defaults::remote-check-room copy 0)))
      (fail (tuatara.defaults::remote-check-room copy (ash 1 62)) 'tuatara.runtime:refusal))))

(define-test (remote-files first-scan-stops-at-a-hole)
  ;; a line end or start beyond a hole is not taken for this line's
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "h" dir))
           (octets (make-array 200000 :element-type '(unsigned-byte 8) :initial-element 65)))
      (setf (aref octets 95000) 10 (aref octets 120000) 10)
      (alexandria:write-byte-vector-into-file octets file)
      (let* ((mapping (tuatara.text:map-file (namestring file)))
             (rope (tuatara.text:mapping-rope mapping)))
        (unwind-protect
             (progn
               (setf (tuatara.text:mapping-holes mapping) (list (cons 100000 110000)))
               (is = 100000 (tuatara.text:rope-line-end rope 99000) "the newline beyond the hole is not this line's")
               (is = 110000 (tuatara.text:rope-line-start rope 115000) "nor the one before it"))
          (tuatara.text:close-mapping mapping))))))

(define-test (remote-files in-order-read-gives-way)
  ;; reading in order lets another waiting for the connection in, between short runs of requests
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) "big.bin"))
               (copy (concatenate 'string (namestring dir) "copy"))
               (lots 0))
          (alexandria:write-byte-vector-into-file
           (make-array (* 64 tuatara.defaults::+sftp-piece+) :element-type '(unsigned-byte 8) :initial-element 7) file)
          (let ((link (tuatara.defaults::sftp-link "box")))
            (sb-ext:atomic-incf (tuatara.defaults::sftp-link-wanting link))
            (let ((fd (sb-posix:open copy (logior sb-posix:o-creat sb-posix:o-wronly) #o600)))
              (unwind-protect
                   (tuatara.defaults::remote-download "box" file fd :on-piece (lambda () (incf lots)))
                (sb-posix:close fd)
                (sb-ext:atomic-decf (tuatara.defaults::sftp-link-wanting link)))))
          (true (> lots 1) "more than one lot, ~d" lots)))))


(define-test (remote-files octets-sent-from-a-range-uncopied)
  ;; a field of (:octets VECTOR START END) goes out as the octets from START below END, after their length
  (let ((box (tuatara.frontend:make-outbox))
        (vector (coerce '(1 2 3 4 5 6) 'tuatara.text:octets)))
    (tuatara.defaults::sftp-put box (list :octets vector 2 5))
    (is equalp #(0 0 0 3 3 4 5)
        (subseq (tuatara.frontend:outbox-octets box) 0 (tuatara.frontend:outbox-fill box)))))

(define-test (remote-files links-asked-for-a-lot-at-a-time)
  ;; where each link of a directory leads is asked for ahead of the answers, not one round trip after another
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)) (names (loop for n below 40 collect (format nil "link~2,'0d" n))))
          (put-file (merge-pathnames "target" dir) "x")
          (dolist (name names)
            (sb-posix:symlink (if (evenp (length name)) "target" "none") (concatenate 'string there name)))
          (let* ((details nil)
                 (flushes (calls-of 'tuatara.defaults::sftp-flush
                                   (lambda ()
                                     (setf details (tuatara.defaults::remote-link-details "box" there names))))))
            (is = 40 (length details))
            (is string= "target" (car (first details)))
            (true (getf (cdr (first details)) :size))
            (true (< flushes 10) "~d flushes for 40 links" flushes))))))

(define-test (remote-files tree-kinds-asked-once)
  ;; a tree is copied and deleted from the kinds its listings gave: no request for the kind of each entry again
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (with-test-directory (here)
          (let ((remote (concatenate 'string "/ssh:box:" (namestring dir))))
            (ensure-directories-exist (merge-pathnames "tree/sub/" dir))
            (dotimes (n 8)
              (put-file (merge-pathnames (format nil "tree/sub/f~d" n) dir) "x"))
            (is = 1 (calls-of 'tuatara.defaults::remote-or-local-kind
                              (lambda ()
                                (tuatara.defaults::rimu-work :copy (cons (concatenate 'string remote "tree")
                                                                         (concatenate 'string (namestring here) "tree"))
                                                             (list nil)))))
            (is string= "x" (file-text (merge-pathnames "tree/sub/f7" here)))
            (is = 1 (calls-of 'tuatara.defaults::remote-or-local-kind
                              (lambda ()
                                (tuatara.defaults::rimu-work :delete (cons (concatenate 'string remote "tree") nil)
                                                             (list nil)))))
            (false (probe-file (merge-pathnames "tree/" dir))))))))

(define-test (remote-files tree-copy-bounded-and-names-cleaned)
  ;; a tree deeper than *REMOTE-TREE-DEPTH* is not copied down, nor is a file whose name would not show as it is
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (with-test-directory (here)
          (let ((remote (concatenate 'string "/ssh:box:" (namestring dir))))
            (flet ((copy (from to)
                     (handler-case (progn (tuatara.defaults::rimu-work :copy (cons from to) (list nil)) :none)
                       (error (condition) (princ-to-string condition)))))
              (ensure-directories-exist (merge-pathnames "deep/a/b/c/" dir))
              (put-file (merge-pathnames "deep/a/b/c/file" dir) "x")
              (let ((tuatara.defaults::*remote-tree-depth* 2))
                (true (search "deep" (copy (concatenate 'string remote "deep") (concatenate 'string (namestring here) "deep"))))
                (false (probe-file (merge-pathnames "deep/a/b/c/file" here))))
              (ensure-directories-exist (merge-pathnames "odd/" dir))
              (put-file (merge-pathnames "odd/good" dir) "x")
              (put-file (concatenate 'string (namestring dir) (format nil "odd/evil~cname" (code-char #x202E))) "x")
              (true (search "would not show"
                            (copy (concatenate 'string remote "odd") (concatenate 'string (namestring here) "odd"))))
              (true (probe-file (merge-pathnames "odd/good" here)))
              (is = 1 (length (directory (merge-pathnames "odd/*.*" here))))))))))

(define-test (remote-files new-file-beside-the-home-directory)
  ;; a file with no directory in its path (one in the home directory there) is written
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :before (format nil "cd '~a'" (namestring dir)))
        (let* ((octets (coerce '(104 105) 'tuatara.text:octets))
               (fd (tuatara.io:memory-file "plain" octets :kept nil)))
          (unwind-protect
               (tuatara.defaults::remote-upload "box" "fresh.txt" fd 2 :expected (constantly nil))
            (sb-posix:close fd))
          (is string= "hi" (file-text (merge-pathnames "fresh.txt" dir)))))))

(define-test (remote-files brought-directory-must-be-plain-for-the-shell)
  ;; the directory servers are brought to is put in commands the server's shell reads: a space is refused
  (let ((tuatara.lsp:*brought-language-servers-directory* "my servers/"))
    (flet ((failure (function)
             (handler-case (progn (funcall function) :none) (error (condition) (princ-to-string condition)))))
      (true (search "plain path" (failure (lambda () (tuatara.defaults::manifest-path)))))
      (true (search "plain path" (failure (lambda () (tuatara.defaults::server-program-path "x"))))))))
