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

(defmacro with-stand-in-sftp ((dir log &key answers machine) &body body)
  "Run BODY with DIR bound to a new test folder, the files \"there\", and
*SSH-PROGRAM* set, for every thread, to a stand-in for ssh that writes its
arguments in LOG, a file in DIR, and runs sftp-server with a umask of 077,
or with ANSWERS, octets, writes them and waits; every connection closed
after, and the program set back. Asked what it makes of a host (ssh -G), it
says the host is the machine MACHINE, else the host as named, for the user
written before an @ in it, else me:
kept a minute, so a test naming MACHINE names hosts of its own."
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
                                   (format nil "#!/bin/sh~%if [ \"$1\" = -G ]; then ~
                                                case \"$3\" in *@*) u=\"${3%%@*}\";; *) u=me;; esac; ~
                                                printf 'hostname %s\\nport 22\\nuser %s\\n' ~:[\"$3\"~;'~:*~a'~] \"$u\"; ~
                                                exit 0; fi~%~
                                                printf '%s\\n' \"$@\" > '~a'~%umask 077~%~:[exec '~a'~;cat '~a'; ~
                                                exec sleep 60~]~%"
                                           ,machine ,log ,answers (if ,answers ,answers-file (sftp-server-program)))))
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
  ;; against OpenSSH's own server: attributes, a folder listed, a file of many pieces read whole, written
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
            (is string= (string-right-trim "/" there) (tuatara.defaults::remote-real-path "box" (concatenate 'string there "sub/..")))
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
                               (failure (server-octets version '(:handle 1 "h") '(:attrs 2 (:raw #(0 0 0 5 0 0 0 0 0 0 0 4 0 0 #o201 #o244)))
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
  ;; C-x C-f reads the file into a private copy, the buffer named after it and standing in its folder there;
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
                   (remote-session (format nil "C-x b ~a RET" (typed-keys "notes.txt")) (format nil "M-x ~a RET" (typed-keys "revert-buffer"))
                                   (list :until (lambda () (string= "second text" (tuatara.buffer:buffer-string buffer)))))
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
                 (true (search "changed on the server" (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)))))
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
  ;; a folder there named in *evidence-folders* opens safely and is never written
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((file (concatenate 'string (namestring dir) "seized.log"))
               (name (concatenate 'string "/ssh:box:" file)))
          (alexandria:write-string-into-file "kept" file)
          (with-evidence-folders ((list (format nil "/ssh:box:~a" (namestring dir))))
            (unwind-protect
                 (progn
                   (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name))))
                   (let ((buffer (remote-buffer-of name)))
                     (true (tuatara.io:safe-visit-p buffer))
                     (true (tuatara.defaults::remote-file-evidence (tuatara.defaults::buffer-remote buffer)))
                     (true (tuatara.defaults::remote-save-refusal buffer))
                     (setf (tuatara.buffer:buffer-read-only-p buffer) nil)
                     (remote-session (format nil "C-x b ~a RET M->" (typed-keys "seized.log")) (typed-keys "x") "C-x C-s" (list :wait 0.3))
                     (is string= "kept" (file-text file))))
              (kill-remote-buffers)))))))

(define-test (remote-files names-completed-from-the-server)
  ;; the folder there is listed as C-x C-f's candidates; a name that would not show as it is is left out
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (alexandria:write-string-into-file "x" (concatenate 'string there "alpha-file.txt"))
          (alexandria:write-string-into-file "x" (concatenate 'string there (format nil "evil~cname" (code-char #x202E))))
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

;;; Folders listed with rimu

(defun remote-rimu-buffer ()
  "The rimu list of a folder on another machine shown last, or NIL."
  (find-if (lambda (buffer)
             (and (tuatara.defaults::rimu-list-p buffer)
                  (tuatara.defaults::remote-rimu-source-p (tuatara.defaults::rimu-source buffer))))
           tuatara.buffer:*buffers*))

(defun remote-list-read ()
  "True once a rimu list of a folder on another machine shows what was read."
  (let ((list (remote-rimu-buffer)))
    (and list (tuatara.buffer:buffer-local list 'tuatara.defaults::rimu-listing) t)))

(defun kill-remote-lists ()
  "Kill every rimu list of a folder on another machine."
  (loop for buffer = (remote-rimu-buffer) while buffer do (tuatara.buffer:kill-buffer buffer)))

(define-test (remote-files folder-listed-with-rimu)
  ;; C-x C-f on a folder there lists it with rimu: its entries, a link with where it leads, a name that would not
  ;; show as it is shown escaped; no preview, and the commands working on files here refused
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((there (namestring dir)))
          (alexandria:write-string-into-file "x" (concatenate 'string there "alpha.txt"))
          (alexandria:write-string-into-file "x" (concatenate 'string there (format nil "evil~cname" (code-char #x202E))))
          (ensure-directories-exist (merge-pathnames "sub/" dir))
          (sb-posix:symlink "alpha.txt" (concatenate 'string there "link"))
          (unwind-protect
               (let ((f (remote-session (format nil "C-x C-f C-a C-k ~a C-j"
                                                ;; the name as typed, without the / completion would add
                                                (typed-keys (concatenate 'string "/ssh:box:" (string-right-trim "/" there))))
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

(define-test (remote-files folder-copied-both-ways)
  ;; files and folders copied from there to here, here to there and within the machine, never over a name there;
  ;; a link left out and said; renamed there, a move to another machine refused; a folder there deleted whole
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
              ;; there to here, a folder whole
              (work :copy (concatenate 'string remote "tree") (concatenate 'string local "tree"))
              (is string= "first" (file-text (merge-pathnames "tree/a.txt" here)))
              (is string= "second" (file-text (merge-pathnames "tree/deep/b.txt" here)))
              (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames "tree/a.txt" here))))))
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
  ;; D in a list of a folder there asks once, plainly, that it is deleted for good, then deletes it
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

(define-test (remote-files evidence-folder-there-kept)
  ;; a folder there named in *evidence-folders* is listed opened safely, and nothing in it is deleted or written
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let* ((there (namestring dir))
               (remote (concatenate 'string "/ssh:box:" there)))
          (put-file (merge-pathnames "seized.txt" dir) "kept")
          (with-evidence-folders ((list remote))
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

(define-test (remote-files names-from-the-server-kept-in-their-folder)
  ;; a name a server lists that would lead out of its folder (../x) is refused, nothing copied outside
  (let ((tuatara-user:*remote-seconds* 1))
    (with-stand-in-sftp (dir log :answers (server-octets '(:version 3) '(:attrs 1 4 #o40755) '(:handle 2 "h")
                                                         '(:name 3 1 "../evil" "x" 0) '(:status 4 1 "" "")
                                                         '(:status 5 0 "" "")))
      (with-test-directory (here)
        (let ((said (handler-case (progn (tuatara.defaults::rimu-work :copy (cons "/ssh:box:/folder"
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

(define-test (remote-files evidence-held-in-a-folder-kept)
  ;; a folder there holding an evidence folder is neither deleted nor moved whole
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((remote (concatenate 'string "/ssh:box:" (namestring dir))))
          (ensure-directories-exist (merge-pathnames "case/seized/" dir))
          (put-file (merge-pathnames "case/seized/disk.img" dir) "kept")
          (with-evidence-folders ((list (concatenate 'string remote "case/seized/")))
            (true (search "evidence folder" (rimu-failure :delete (concatenate 'string remote "case"))))
            (true (search "evidence folder" (rimu-failure :move (concatenate 'string remote "case")
                                                          (concatenate 'string remote "moved"))))
            (is string= "kept" (file-text (merge-pathnames "case/seized/disk.img" dir))))))))

(define-test (remote-files evidence-told-by-machine-and-real-path)
  ;; an evidence folder there is told under any name of its machine, as written and where it really is, by a
  ;; visit and by a list of it alike
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :machine "evidence.lan")
        (let ((there (namestring dir)))
          (ensure-directories-exist (merge-pathnames "seized/" dir))
          (put-file (merge-pathnames "seized/a.txt" dir) "kept")
          (sb-posix:symlink (concatenate 'string there "seized") (concatenate 'string there "door"))
          (with-evidence-folders ((list (format nil "/ssh:ev-alias:~aseized/" there)))
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
          (with-evidence-folders ((list (format nil "/ssh:ev-host:~adoor/" there)))
            (true (tuatara.defaults::remote-evidence-words "ev-host" (concatenate 'string there "seized/a.txt"))
                  "named by a link that leads to it"))))))

(define-test (remote-files folder-not-copied-into-itself)
  ;; a folder there is not copied into itself, under another name of its machine either
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :machine "copies.lan")
        (let ((there (namestring dir)))
          (ensure-directories-exist (merge-pathnames "tree/" dir))
          (put-file (merge-pathnames "tree/a.txt" dir) "a")
          (true (search "into itself" (rimu-failure :copy (format nil "/ssh:copy-a:~atree" there)
                                                    (format nil "/ssh:copy-b:~atree/inner" there))))
          (false (uiop:directory-exists-p (merge-pathnames "tree/inner/" dir)))))))

;;; Bounds on what a server says, and its folders read for a question

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
  ;; a folder or file opened, then a request about it refused, is closed there before the connection goes on
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

(define-test (remote-files folder-there-read-once-while-asked)
  ;; the entries of a folder there are read once for a question for a file, not at each key; a path to one
  ;; typed in a buffer reads nothing there
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((folder (concatenate 'string "/ssh:box:" (namestring dir))))
          (is = 1 (calls-of 'tuatara.defaults::remote-listing
                            (lambda ()
                              (tuatara.defaults::directory-candidates folder nil)
                              (tuatara.defaults::directory-candidates folder nil))))
          (let ((tuatara.defaults::*remote-candidates-seconds* 0))
            (is = 0 (calls-of 'tuatara.defaults::remote-listing
                              (lambda ()
                                (run-keys (list "i" (key-string folder) #'wait-for-path-listing)
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
  ;; a file with another name is not written in place while an evidence folder is named on its machine, as
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
          (with-evidence-folders ((list (format nil "/ssh:box:~aseized/" there)))
            (unwind-protect
                 (progn
                   (remote-session (visit-keys name) (list :until (lambda () (remote-buffer-of name)))
                                   "M->" (typed-keys "!") "C-x C-s"
                                   (list :until (lambda ()
                                                  (search "is not written there"
                                                          (tuatara.buffer:buffer-string
                                                           (tuatara.command::messages-buffer))))))
                   (is string= "two" (file-text kept))
                   (true (search "may be in an evidence folder"
                                 (tuatara.buffer:buffer-string (tuatara.command::messages-buffer)))))
              (kill-remote-buffers)))))))

(define-test (remote-files evidence-holds-for-every-account)
  ;; an evidence folder named with one account on a machine holds for every account there
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log :machine "accounts.lan")
        (let ((there (namestring dir)))
          (ensure-directories-exist (merge-pathnames "seized/" dir))
          (with-evidence-folders ((list (format nil "/ssh:root@acct-host:~aseized/" there)))
            (true (tuatara.defaults::remote-evidence-words "alice@acct-host"
                                                           (concatenate 'string there "seized/a.txt"))))))))

(define-test (remote-files real-path-unknown-refused)
  ;; where a path leads is asked again until it is had, never kept as not there; a write where it cannot be
  ;; had, on a machine an evidence folder is named on, is refused
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
               (with-evidence-folders ((list (format nil "/ssh:box:~aseized/" there)))
                 (true (search "evidence" (rimu-failure :copy (concatenate 'string there "local.txt")
                                                        (format nil "/ssh:box:~alocked/sub/new.txt" there)))))
            (sb-posix:chmod (concatenate 'string there "locked") #o700))))))

(define-test (remote-files deep-or-crowded-folder-not-moved-unlooked)
  ;; a folder whose folders cannot all be looked through for evidence, too deep or listed cut short, is not
  ;; moved whole
  (if (null (sftp-server-program))
      (skip "no sftp-server here" (true nil))
      (with-stand-in-sftp (dir log)
        (let ((remote (concatenate 'string "/ssh:box:" (namestring dir))))
          (ensure-directories-exist (merge-pathnames "deep/a/b/c/" dir))
          (dolist (name '("x/" "y/" "z/"))
            (ensure-directories-exist (merge-pathnames (concatenate 'string "wide/" name) dir)))
          (with-evidence-folders ((list "/ssh:box:/no-such-evidence/"))
            (let ((tuatara.defaults::*remote-tree-depth* 2))
              (true (search "folders deep" (rimu-failure :move (concatenate 'string remote "deep")
                                                         (concatenate 'string remote "deep2")))))
            (let ((tuatara.defaults::*sftp-most-listed* 2))
              (true (search "too many entries" (rimu-failure :move (concatenate 'string remote "wide")
                                                             (concatenate 'string remote "wide2"))))))
          (true (uiop:directory-exists-p (merge-pathnames "deep/a/b/c/" dir)))
          (true (uiop:directory-exists-p (merge-pathnames "wide/x/" dir)))))))

(define-test (remote-files evidence-told-again-at-save)
  ;; a folder named evidence after the file was opened keeps the save from writing there
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
                 (with-evidence-folders ((list (format nil "/ssh:box:~alater/" there)))
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
                                                  (tuatara.frontend.fake:fake-frame-showing
                                                   (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                   "partly written")))
                                   "y e s RET"
                                   (list :until (lambda () (string= "first!" (file-text file)))))
                   (is string= "first!" (file-text file))
                   (false (tuatara.defaults::remote-file-partly
                           (tuatara.defaults::buffer-remote (remote-buffer-of name)))))
              (kill-remote-buffers)))))))

(define-test (remote-files kept-candidates-let-go)
  ;; the candidates of folders there are kept only while they may be given again
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
