;;;; test/sandbox.lisp --- Programs decoding files, shut in by bwrap.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test sandbox
  :description "A program run on a file with TUATARA.IO:SANDBOX-RUN sees the system's programs and
nothing else, read only: not the file's directory, the home directory, the network or other
processes. It reads the file on its standard input, the file's access time left alone, and is
ended, with all it started, when its time is up, when it prints too much or is stopped; its memory
is limited. Without bwrap it is not run, and says why. The container the tests run in has bwrap.")

(defun sandbox-text (output)
  "OUTPUT, the octets a program printed, as text."
  (sb-ext:octets-to-string output :external-format :utf-8))

(defun sandbox-shell (script &rest keys)
  "Run the shell SCRIPT in the sandbox (KEYS for SANDBOX-RUN): what it printed
as text, how it went, and what it said on its error output."
  (multiple-value-bind (output outcome said) (apply #'tuatara.io:sandbox-run "sh" (list "-c" script) keys)
    (values (sandbox-text output) outcome said)))

(defun processes-running (mark)
  "How many processes have MARK in their command line."
  (count-if (lambda (directory)
              (let ((line (and (every #'digit-char-p (car (last (pathname-directory directory))))
                               (ignore-errors (alexandria:read-file-into-string
                                               (merge-pathnames "cmdline" directory) :external-format :latin-1)))))
                (and line (search mark line))))
            (uiop:subdirectories #p"/proc/")))

(define-test (sandbox programs-run-one-way)
  ;; every program run for a job is started, tied to its job and ended the one way (call-with-program)
  (let ((printf (tuatara.io:find-program "printf")) (sleep (tuatara.io:find-program "sleep")))
    ;; FUNCTION's values; what the program writes read through its pipe
    (is string= "hi"
        (tuatara.io:call-with-program (list printf "hi")
                                      (lambda (p) (read-line (sb-ext:process-output p)))
                                      :output :stream))
    ;; a program still running when FUNCTION returns, or signals, is ended, not left behind
    (let ((process nil) (start (get-internal-real-time)))
      (flet ((ended (process) (tuatara.runtime:wait-until (lambda () (not (sb-ext:process-alive-p process))) 10)))
        (tuatara.io:call-with-program (list sleep "30") (lambda (p) (setf process p)))
        (true (ended process))
        (ignore-errors
          (tuatara.io:call-with-program (list sleep "30") (lambda (p) (setf process p) (error "gone wrong"))))
        (true (ended process)))
      (true (< (- (get-internal-real-time) start) (* 15 internal-time-units-per-second))))
    ;; the program of a job: stopping the job ends it
    (let ((job (tuatara.defaults::make-vc-job nil 0)) (start (get-internal-real-time)))
      (is eq :signaled
          (tuatara.io:call-with-program (list sleep "30")
                                        (lambda (p)
                                          (is eq p (tuatara.runtime:job-process job))
                                          (tuatara.runtime:job-stop job)
                                          (sb-ext:process-wait p)
                                          (sb-ext:process-status p))
                                        :job job))
      (true (< (- (get-internal-real-time) start) (* 15 internal-time-units-per-second))))))

(define-test (sandbox programs-started-one-way)
  ;; every program is started by START-PROGRAM: the environment programs get, a session of its own, a terminal
  (flet ((said (command &rest keys)
           (let ((process (apply #'tuatara.io:start-program command :output :stream keys)))
             (unwind-protect (string-trim '(#\Newline #\Space)
                                          (uiop:slurp-stream-string (sb-ext:process-output process)))
               (tuatara.io:done-with-program process))))
         (own-session ()
           (nth-value 1 (tuatara.io::process-group-and-session (sb-posix:getpid)
                                                               (make-array 1024 :element-type '(unsigned-byte 8))))))
    (let ((sh (tuatara.io:find-program "sh"))
          (session "cut -d' ' -f6 /proc/$$/stat"))
      ;; the variables the editor keeps for itself are not given
      (flet ((path-given-p ()
               (some (lambda (line) (uiop:string-prefix-p "PATH=" line))
                     (uiop:split-string (said (list (tuatara.io:find-program "env"))) :separator '(#\Newline)))))
        (true (path-given-p))
        (let ((tuatara.io:*own-variables* '("PATH")))
          (false (path-given-p))))
      (is string= "A=1" (said (list (tuatara.io:find-program "env")) :environment '("A=1")))
      (is = (own-session) (parse-integer (said (list sh "-c" session))))
      (when (tuatara.io:find-program "setsid")
        (isnt = (own-session) (parse-integer (said (list sh "-c" session) :session t)))
        ;; a terminal's program leads its session on its pseudo-terminal, and the process ends when it ends
        (let ((process (tuatara.io:start-program
                        (list sh "-c" (concatenate 'string "[ \"$(cut -d' ' -f6 /proc/$$/stat)\" = $$ ] && "
                                                   "[ \"$(cut -d' ' -f7 /proc/$$/stat)\" != 0 ] && exit 3"))
                        :session :terminal :pty t)))
          (unwind-protect (progn (sb-ext:process-wait process)
                                 (is eql 3 (sb-ext:process-exit-code process)))
            (tuatara.io:done-with-program process)))))))

(define-test (sandbox program-refusing-to-end-killed)
  ;; a program that will not end when asked is killed, with what it started,
  ;; once its time to end is up; the one done with it does not wait for that
  (let ((mark (format nil "31.~d" (+ 100000 (random 900000))))
        (start (get-internal-real-time)))
    (tuatara.io:call-with-program (list (tuatara.io:find-program "sh") "-c"
                                        (format nil "trap '' TERM; sleep ~a & wait" mark))
                                  (lambda (process) (declare (ignore process)) (sleep 0.2)))
    (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second)))
    (true (plusp (processes-running mark)) "still there within its time to end")
    (true (gone-within mark 4))))

(define-test (sandbox program-ended-with-what-it-started)
  ;; a program no longer wanted that ends as soon as it is asked is ended with
  ;; what it started, its children not left behind; so is one run in a
  ;; session of its own (as a compile is), with all of that session
  (dolist (in-session '(nil t))
    (let* ((mark (format nil "31.~d" (+ 100000 (random 900000))))
           (command (list (tuatara.io:find-program "sh") "-c" (format nil "sleep ~a & wait" mark)))
           (start (get-internal-real-time)))
      (tuatara.io:call-with-program (if in-session (tuatara.io:session-command command) command)
                                    (lambda (process)
                                      (declare (ignore process))
                                      (true (loop repeat 200 thereis (>= (processes-running mark) (if in-session 3 2))
                                                  do (sleep 0.01))
                                            "the program and what it started running")))
      (true (< (- (get-internal-real-time) start) (* 3 internal-time-units-per-second)))
      (true (gone-within mark 3) "~:[in its group~;in a session of its own~]" in-session))))

(define-test (sandbox found)
  (multiple-value-bind (status detail path) (tuatara.io:sandbox-status :again t)
    (is eq :ready status)
    (true (search "bubblewrap" detail))
    (true (search "/bwrap" path)))
  ;; turned off, or not there: nothing runs, and why is said
  (let ((tuatara.io::*sandbox* nil))
    (is eq :off (tuatara.io:sandbox-status))
    (is eq :unavailable (nth-value 1 (tuatara.io:sandbox-run "true" '()))))
  (let ((tuatara.io::*bwrap-name* "no-such-sandbox-program"))
    (multiple-value-bind (status detail) (tuatara.io:sandbox-status)
      (is eq :missing status)
      (true (search "no-such-sandbox-program not found" detail)))
    (multiple-value-bind (output outcome said) (tuatara.io:sandbox-run "true" '())
      (is = 0 (length output))
      (is eq :unavailable outcome)
      (true (search "not found" said))))
  ;; and back to bwrap
  (is eq :ready (tuatara.io:sandbox-status)))

(define-test (sandbox setting-changed-while-tried)
  ;; the setting changed while the sandbox is tried, as an init file read meanwhile does: what was tried is kept
  (let ((tuatara.io::*sandbox* t))
    (sb-int:encapsulate 'tuatara.io::version-text 'changed-meanwhile
                        (lambda (function &rest arguments)
                          (setf tuatara.io::*sandbox* nil)
                          (apply function arguments)))
    (unwind-protect (tuatara.io:sandbox-status :again t)
      (sb-int:unencapsulate 'tuatara.io::version-text 'changed-meanwhile))
    (is eq nil tuatara.io::*sandbox*)
    (is eq :off (tuatara.io:sandbox-status))
    (is eq :off (tuatara.io:sandbox-status-now))
    (is eq :unavailable (nth-value 1 (tuatara.io:sandbox-run "true" '()))))
  (is eq :ready (tuatara.io:sandbox-status)))

(define-test (sandbox reads-the-file-only)
  (uiop:with-temporary-file (:pathname path :type "txt")
    (alexandria:write-string-into-file (format nil "evidence~%") path :if-exists :supersede)
    (sb-posix:utimes (namestring path) 946681200 946681200)
    (multiple-value-bind (output outcome said) (tuatara.io:sandbox-run-on-file path "cat" '())
      (is string= (format nil "evidence~%") (sandbox-text output))
      (is eq :done outcome)
      (is string= "" said))
    ;; read as a safe visit reads: the access time as it was
    (is = 946681200 (sb-posix:stat-atime (sb-posix:stat (namestring path))))
    ;; its name and directory are nowhere to be found
    (is string=
        (format nil "no~%")
        (sandbox-shell (format nil "test -e '~a' && echo yes || echo no" (namestring path)))))
  ;; what is not a regular file is refused unread
  (fail (tuatara.io:sandbox-run-on-file (uiop:temporary-directory) "cat" '()) tuatara.io:file-system-error))

(define-test (sandbox programs-found-from-the-root)
  ;; a program is looked for only in directories of the PATH named from the root: an empty
  ;; entry, . or a relative one would find a program in whatever directory the editor is in
  (with-test-directory (dir)
    (let* ((name (format nil "tuatara-test-program-~36r" (random (expt 36 6))))
           (program (namestring (merge-pathnames name dir)))
           (absolute (string-right-trim "/" (namestring dir)))
           ;; the same directory, named from the current one
           (relative (format nil "~{~a~}~a"
                             (loop repeat (count #\/ (string-right-trim "/" (namestring (uiop:getcwd)))) collect "../")
                             (subseq absolute 1))))
      (alexandria:write-string-into-file (format nil "#!/bin/sh~%") program)
      (sb-posix:chmod program #o700)
      (is string= program (tuatara.io:find-program name absolute))
      (true (eql 0 (ignore-errors (sb-posix:access (format nil "~a/~a" relative name) sb-posix:x-ok))))
      (false (tuatara.io:find-program name relative))
      (false (tuatara.io:find-program name (format nil ":~a" relative)))))
  ;; a program started in a session of its own: through setsid when it is there, its flags before the program
  (multiple-value-bind (run arguments) (tuatara.io:setsid-command "true" '("x") "-w")
    (if (tuatara.io:find-program "setsid")
        (progn (is string= (tuatara.io:find-program "setsid") run)
               (is equal (list "-w" (tuatara.io:find-program "true") "x") arguments))
        (progn (is string= (tuatara.io:find-program "true") run)
               (is equal '("x") arguments)))))

(define-test (sandbox cannot-change-the-file)
  ;; a program turned against the editor by the file it reads writes through
  ;; its standard input, changes its permissions and times: none of it reaches the file
  (uiop:with-temporary-file (:pathname path :type "txt")
    (alexandria:write-string-into-file (format nil "evidence~%") path :if-exists :supersede)
    (sb-posix:chmod (namestring path) #o644)
    (sb-posix:utimes (namestring path) 946681200 946681200)
    (tuatara.io:sandbox-run-on-file
     path "sh" (list "-c" "echo changed > /proc/self/fd/0; echo changed >> /proc/self/fd/0; chmod 777 /proc/self/fd/0; touch /proc/self/fd/0; touch -d 2001-01-01 /proc/self/fd/0; cat /proc/self/fd/0"))
    (let ((stat (sb-posix:stat (namestring path))))
      (is = #o644 (logand #o7777 (sb-posix:stat-mode stat)))
      (is = 946681200 (sb-posix:stat-mtime stat))
      (is = 946681200 (sb-posix:stat-atime stat)))
    (is string= (format nil "evidence~%") (alexandria:read-file-into-string path))
    ;; and the program still reads it whole
    (is string= (format nil "evidence~%") (sandbox-text (tuatara.io:sandbox-run-on-file path "cat" '())))))

(define-test (sandbox one-copy-for-many)
  ;; a sealed copy given as the input is read as it is, by many programs at once, each from its start
  (uiop:with-temporary-file (:pathname path :type "txt")
    (alexandria:write-string-into-file (format nil "evidence~%") path :if-exists :supersede)
    (let ((copy (tuatara.io:call-with-file (namestring path)
                                           (lambda (fd p size) (declare (ignore p size)) (tuatara.io:sealed-copy fd)))))
      (unwind-protect
           (let* ((results (make-array 4))
                  (threads (loop for i below 4
                                 collect (let ((i i))
                                           (sb-thread:make-thread
                                            (lambda () (setf (aref results i)
                                                             (sandbox-text (tuatara.io:sandbox-run "cat"
                                                                                                   '()
                                                                                                   :input copy)))))))))
             (mapc #'sb-thread:join-thread threads)
             (is equal (make-list 4 :initial-element (format nil "evidence~%")) (coerce results 'list))
             (true (tuatara.io::sealed-p copy)))
        (sb-posix:close copy)))))

(define-test (sandbox file-limit)
  ;; a file larger than *sandbox-input-limit*, or claiming a huge size with nothing in it, is not copied into memory
  (uiop:with-temporary-file (:pathname path :type "pdf")
    (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence (tuatara.text:string-octets "%PDF-1.4") out)
      (file-position out (* 64 1024 1024))
      (write-byte 0 out))
    (let ((tuatara.io::*sandbox-input-limit* (* 1024 1024)))
      (let ((error (handler-case (tuatara.io:call-with-file (namestring path)
                                                            (lambda (fd p size)
                                                              (declare (ignore p size))
                                                              (tuatara.io:sealed-copy fd)))
                     (tuatara.io:file-system-error (e) e))))
        (true (typep error 'tuatara.io:file-system-error))
        (true (search "*sandbox-input-limit*" (princ-to-string error)))))))

(define-test (sandbox scratch-held)
  ;; the scratch /tmp, which is memory, held to the memory a program may take
  (when (tuatara.io::bwrap-version-at-least-p 0 10)
    (multiple-value-bind (text outcome) (sandbox-shell "grep ' /tmp ' /proc/mounts" :memory (* 64 1024 1024))
      (is eq :done outcome)
      (true (search "size=65536k" text) "~a" text))))

(define-test (sandbox shut-in)
  (multiple-value-bind (text outcome)
      (sandbox-shell "echo $HOME; env | wc -l; test -e /home && echo home; test -e /var && echo var;
touch /usr/tuatara-test 2>/dev/null && echo wrote; echo made > /tmp/scratch && cat /tmp/scratch;
grep -c : /proc/net/dev; ls /proc | grep -c '^[0-9]'")
    (is eq :done outcome)
    (destructuring-bind (home environment network processes &rest rest)
        (remove "made" (uiop:split-string (string-trim '(#\Newline) text) :separator '(#\Newline)) :test #'string=)
      ;; a scratch home, and nothing in the environment but PATH, HOME, LC_ALL and the shell's own
      (is string= "/tmp" home)
      (true (<= (parse-integer environment) 6))
      ;; no network but its own loopback, and only its own few processes
      (is = 1 (parse-integer network))
      (true (<= (parse-integer processes) 6))
      ;; no home, no /var, /usr not written; its scratch /tmp written
      (is equal '() rest))
    (true (search "made" text))))

(define-test (sandbox ended-and-limited)
  ;; its time up: ended, with what it started
  (let ((start (get-internal-real-time))
        (mark (format nil "31.~d" (+ 100000 (random 900000)))))
    (is eq :timeout (nth-value 1 (sandbox-shell (format nil "sleep ~a & sleep ~a" mark mark) :seconds 0.5)))
    (true (< (- (get-internal-real-time) start) (* 3 internal-time-units-per-second)))
    (true (loop repeat 100 thereis (zerop (processes-running mark)) do (sleep 0.02))))
  ;; printing too much
  (is eq :too-much (nth-value 1 (tuatara.io:sandbox-run "head" '("-c" "100000" "/dev/zero") :output-limit 1000)))
  ;; a program run outside the sandbox that writes without end is stopped and its output not taken
  (multiple-value-bind (output ok)
      (tuatara.io:program-output (list (tuatara.io:find-program "yes")) :seconds 10 :most 100000)
    (false output)
    (false ok))
  (is string= "abcd" (tuatara.io:program-output (list (tuatara.io:find-program "printf") "abcd") :seconds 5 :most 4)
      "within the limit, all of it is taken")
  ;; out of memory: it fails, and says so
  (multiple-value-bind (output outcome said)
      (tuatara.io:sandbox-run "awk" '("BEGIN { s = \"a\"; while (1) s = s s }") :memory (* 64 1024 1024))
    (declare (ignore output))
    (is eq :failed outcome)
    (true (search "memory" said)))
  ;; no more than *SANDBOX-PROCESS-LIMIT* processes at once, threads counted
  (is string= (format nil "~d~%" tuatara.io::*sandbox-process-limit*) (sandbox-shell "ulimit -u"))
  ;; all its processes together held to *sandbox-total-memory*, where systemd-run can (as here)
  (true tuatara.io::**sandbox-scope**)
  (true (search "together held to 2.0 GB of memory" (nth-value 1 (tuatara.io:sandbox-status))))
  (flet ((three-at-once ()
           (count-if (lambda (line) (string= line "ok"))
                     (uiop:split-string (sandbox-shell "for i in 1 2 3; do awk 'BEGIN { s = \"a\"; while (length(s) < 40000000) s = s s; system(\"sleep 1\"); print \"ok\" }' & done; wait")
                                        :separator '(#\Newline)))))
    (is = 3 (three-at-once))
    (let ((tuatara-user:*sandbox-total-memory* (* 96 1024 1024)))
      (true (< (three-at-once) 3))))
  ;; stopped by the one waiting
  (let ((cancel (list nil)) (start (get-internal-real-time)))
    (sb-thread:make-thread (lambda () (sleep 0.2) (setf (car cancel) t)))
    (is eq :cancelled (nth-value 1 (tuatara.io:sandbox-run "sleep" '("30") :cancel cancel)))
    (true (< (- (get-internal-real-time) start) (* 3 internal-time-units-per-second))))
  ;; a program that fails says why
  (multiple-value-bind (output outcome said) (tuatara.io:sandbox-run "no-such-decoder" '())
    (declare (ignore output))
    (is eq :failed outcome)
    (true (search "no-such-decoder" said))))

(defun gone-within (mark seconds)
  "True once no process has MARK in its command line, looked for over SECONDS."
  (loop repeat (ceiling seconds 0.02)
        thereis (zerop (processes-running mark))
        do (sleep 0.02)))

(defun kill-marked (mark)
  "Kill (SIGKILL) every process with MARK in its command line."
  (dolist (directory (uiop:subdirectories #p"/proc/"))
    (let* ((name (car (last (pathname-directory directory))))
           (line (and (every #'digit-char-p name)
                      (ignore-errors (alexandria:read-file-into-string (merge-pathnames "cmdline" directory)
                                                                       :external-format :latin-1)))))
      (when (and line (search mark line))
        (ignore-errors (sb-posix:kill (parse-integer name) sb-unix:sigkill))))))

(defun call-with-sandbox-held (mark function &key environment)
  "Start bwrap as the editor starts a sandbox, on sleep with MARK, held just
before it lets the sandbox's first process go on (its --info-fd a full pipe),
that process made and waiting; call FUNCTION with bwrap's process. Every
process with MARK is killed after, whatever FUNCTION did."
  (multiple-value-bind (in out) (sb-posix:pipe)
    (unwind-protect
         (progn
           ;; F_SETPIPE_SZ: one page, filled, so that bwrap's first write waits
           (let ((size (tuatara.io::fcntl-int out 1031 4096)))
             (tuatara.io::write-octets out (make-array size :element-type '(unsigned-byte 8)) :path "a pipe"))
           (let ((process (sb-ext:run-program
                           (nth-value 2 (tuatara.io:sandbox-status))
                           (append (list "--info-fd" (princ-to-string out))
                                   (tuatara.io::sandbox-arguments (tuatara.io:sandbox-find-program "sleep")
                                                                  (list mark) nil nil
                                                                  :scratch nil))
                           :search nil :wait nil :input nil :output nil :error nil :preserve-fds (list out)
                           :environment environment)))
             (true (loop repeat 200
                         thereis (tuatara.io:session-leader process)
                         do (sleep 0.01))
                   "bwrap made the sandbox's first process")
             (sleep 0.05)
             (funcall function process)))
      (kill-marked mark)
      (sb-posix:close in)
      (sb-posix:close out))))

(define-test (sandbox ended-while-it-starts)
  ;; a helper ended while bwrap still makes its sandbox leaves nothing behind:
  ;; not the sandbox's first process, made and waiting for a go-ahead that
  ;; would never come, which no signal but SIGKILL ends
  (let ((mark (format nil "31.~d" (+ 100000 (random 900000)))))
    (call-with-sandbox-held
     mark
     (lambda (process)
       (multiple-value-bind (fd other) (sb-posix:pipe)
         (sb-posix:close other)
         (tuatara.syntax::stop-helper (tuatara.syntax::%make-helper process fd '() :own)))
       (true (gone-within mark 2))))))

(define-test (sandbox left-behind-ended)
  ;; a sandbox's first process left behind by an editor killed while bwrap
  ;; made it is ended when the sandbox is next tried; the editor's own
  ;; sandboxes are not
  (let ((mark (format nil "31.~d" (+ 100000 (random 900000))))
        (running (format nil "31.~d" (+ 100000 (random 900000))))
        (cancel (list nil)))
    (sb-thread:make-thread (lambda () (tuatara.io:sandbox-run "sleep" (list running) :cancel cancel)))
    (call-with-sandbox-held
     mark
     (lambda (process)
       (sb-ext:process-kill process sb-unix:sigkill)
       (sb-ext:process-wait process)
       (true (loop repeat 50 thereis (= 1 (processes-running mark)) do (sleep 0.02))
             "its first process left behind")
       (true (plusp (processes-running running)))
       (tuatara.io:sandbox-status :again t)
       (true (gone-within mark 2))
       (true (plusp (processes-running running)) "the editor's own running sandbox kept"))
     :environment (list (tuatara.io::sandbox-mark)))
    (setf (car cancel) t)
    (true (gone-within running 3))))

(defun systemctl-output (&rest arguments)
  "What systemctl, given ARGUMENTS after --user, prints of your own service manager."
  (tuatara.io:program-output (list* (tuatara.io:find-program "systemctl") "--user" arguments)
                             :seconds 10 :environment (tuatara.io::scope-environment)))

(defun scopes-with (text)
  "The names of the scopes of your own service manager whose line in its
list, the command run in them included, has TEXT."
  (loop for line in (uiop:split-string (or (systemctl-output "list-units" "--all" "--plain" "--no-legend" "--full"
                                                             "*.scope")
                                           "")
                                       :separator '(#\Newline))
        when (search text line)
          collect (find-if (lambda (word) (search ".scope" word))
                           (remove "" (uiop:split-string line :separator " ") :test #'string=))))

(define-test (sandbox scopes-let-go)
  ;; no scope systemd-run made is left once its sandbox has ended, even one ended
  ;; while systemd-run still had the scope made, which the service manager would
  ;; otherwise keep, empty, for good
  (let ((mark (format nil "31.~d" (+ 100000 (random 900000))))
        (sleep (tuatara.io:sandbox-find-program "sleep"))
        (memory (* 256 1024 1024)))
    (true tuatara.io::**sandbox-scope**)
    (unwind-protect
         (progn
           (let ((process (tuatara.io:sandbox-start sleep (list mark) nil :memory memory)))
             (true (loop repeat 200 thereis (scopes-with mark) do (sleep 0.05)) "a sandbox started in a scope")
             (tuatara.io:end-sandbox process))
           ;; ended at every moment of their start, systemd-run's request on its way included
           (dotimes (i 160)
             (let ((process (tuatara.io:sandbox-start sleep (list mark) nil :memory memory)))
               (sleep (* (mod i 40) 0.001))
               (tuatara.io:end-sandbox process)))
           (dotimes (i 5)
             (is eq :done (nth-value 1 (tuatara.io:sandbox-run "sh" (list "-c" "true" mark)))))
           (true (loop repeat 200 thereis (null (scopes-with mark)) do (sleep 0.05))
                 "no scope left of those sandboxes"))
      (let ((left (remove nil (scopes-with mark))))
        (when left (apply #'systemctl-output "stop" "--no-block" left))))))

(define-test (sandbox scopes-left-stopped)
  ;; the scopes of an editor that has ended are stopped when the sandbox is next
  ;; tried; those of an editor running, of another namespace of processes, and
  ;; any not named by an editor are kept
  (let* ((mark (format nil "31.~d" (+ 100000 (random 900000))))
         (number (+ 1000000000 (random 1000000000)))
         (namespace (tuatara.io::pid-namespace))
         (gone (let ((process (sb-ext:run-program (tuatara.io:find-program "sleep") '("30") :wait nil)))
                 (prog1 (format nil "~d-~a" (sb-ext:process-pid process)
                                (tuatara.io::process-start-time (sb-ext:process-pid process)))
                   (sb-ext:process-kill process sb-unix:sigkill)
                   (sb-ext:process-wait process)
                   (sb-ext:process-close process))))
         (left (format nil "tuatara-sandbox-~a-~a-~d.scope" namespace gone number))
         (kept (list (format nil "tuatara-sandbox-~a-~d.scope" (tuatara.io::scope-owner) number)
                     (format nil "tuatara-sandbox-1-~a-~d.scope" gone number)
                     (format nil "tuatara-sandbox-test-~d.scope" number)))
         (processes '()))
    (unwind-protect
         (progn
           (dolist (name (cons left kept))
             (push (sb-ext:run-program (tuatara.io:find-program "systemd-run")
                                       (list "--user" "--scope" "--quiet" "--collect" "--unit" name
                                             "--" (tuatara.io:find-program "sleep") mark)
                                       :wait nil :environment (tuatara.io::scope-environment))
                   processes))
           (true (loop repeat 200 thereis (= 4 (length (scopes-with mark))) do (sleep 0.05))
                 "the four scopes made")
           (tuatara.io:sandbox-status :again t)
           (true (loop repeat 200 thereis (not (find left (scopes-with mark) :test #'equal)) do (sleep 0.05))
                 "the scope of the editor gone stopped")
           (sleep 0.5)
           (let ((there (scopes-with mark)))
             (dolist (name kept)
               (true (find name there :test #'equal) "~a kept" name))))
      (let ((there (remove nil (scopes-with mark))))
        (when there (apply #'systemctl-output "stop" there)))
      (dolist (process processes)
        (sb-ext:process-kill process sb-unix:sigkill)
        (sb-ext:process-wait process)
        (sb-ext:process-close process)))))

(define-test (sandbox ended-with-their-thread)
  ;; the sandboxes a thread started are ended before it ends, and every one
  ;; as the editor does when it ends
  (let ((mark (format nil "31.~d" (+ 100000 (random 900000)))))
    (flet ((start ()
             (multiple-value-bind (in out) (sb-posix:pipe)
               (sb-posix:close out)
               (let ((stream (sb-sys:make-fd-stream in :input t :auto-close nil)))
                 (prog1 (tuatara.io:sandbox-start (tuatara.io:sandbox-find-program "sleep") (list mark) stream)
                   (sb-posix:close in))))))
      (let* ((process nil)
             (thread (sb-thread:make-thread
                      (lambda ()
                        (setf process (start))
                        (unwind-protect (sleep 30)
                          (tuatara.io::end-sandboxes :thread sb-thread:*current-thread*))))))
        (true (loop repeat 200 thereis (= 3 (processes-running mark)) do (sleep 0.01)))
        (true (find process tuatara.io::**sandboxes** :key #'car))
        (sb-thread:terminate-thread thread)
        (sb-thread:join-thread thread :default nil)
        (true (gone-within mark 2))
        (false (find process tuatara.io::**sandboxes** :key #'car)))
      (let ((process (start)))
        (true (loop repeat 200 thereis (= 3 (processes-running mark)) do (sleep 0.01)))
        ;; the helpers parsing for the editor ended first, as the editor ends them, so that
        ;; none is left looking alive with its program gone, for the tests after this one
        (tuatara.syntax:stop-helpers)
        (tuatara.io::end-sandboxes)
        (true (gone-within mark 2))
        (false (sb-ext:process-alive-p process))))))

(define-test (sandbox outcome-said)
  ;; how a run ended badly, said one way wherever a program is run shut in
  (is eq nil (tuatara.command:sandbox-outcome-words :done ""))
  (let ((tuatara-user:*sandbox-seconds* 7))
    (is string= "magick was stopped after 7 seconds (*sandbox-seconds*)"
        (tuatara.command:sandbox-outcome-words :timeout "" :label "magick")))
  (is string= "pdftotext failed: Syntax Error: no xref"
      (tuatara.command:sandbox-outcome-words :failed (format nil "Syntax Error: no xref~%more~%") :label "pdftotext"))
  (is string= "the program failed" (tuatara.command:sandbox-outcome-words :failed ""))
  (is string= "It runs only shut in the sandbox, and there is none here: bwrap is not installed"
      (tuatara.command:sandbox-outcome-words :unavailable "bwrap is not installed" :label "It"))
  (is string= "It was stopped" (tuatara.command:sandbox-outcome-words :cancelled "" :label "It"))
  (is string= "It printed more than it may" (tuatara.command:sandbox-outcome-words :too-much "" :label "It"))
  ;; what it said cut short on one line
  (is = (+ (length "It failed: ") 300)
      (length (tuatara.command:sandbox-outcome-words :failed (make-string 1000 :initial-element #\x) :label "It"))))

(define-test (sandbox system-calls-refused)
  ;; the seccomp filter: one more than the sandbox had without it
  (flet ((filters () (parse-integer (string-trim '(#\Newline) (sandbox-shell "sed -n 's/^Seccomp_filters:[[:space:]]*//p' /proc/self/status")))))
    (is = (1+ (let ((tuatara.io::*sandbox-seccomp* nil)) (filters))) (filters)))
  ;; no namespace made, by unshare or clone
  (multiple-value-bind (text outcome said) (sandbox-shell "unshare -U true")
    (declare (ignore text))
    (is eq :failed outcome)
    (true (search "not permitted" said) "~a" said))
  ;; sockets: local ones only
  (when (tuatara.io:sandbox-find-program "python3")
    (is string= (format nil "unix~%97~%")
        (sandbox-shell "python3 -c 'import socket
socket.socket(socket.AF_UNIX); print(\"unix\")
try: socket.socket(socket.AF_INET)
except OSError as e: print(e.errno)'")))
  ;; threads still made, through clone when clone3 is refused
  (is string= (format nil "4~%") (sandbox-shell "for i in 1 2 3 4; do sleep 0.01 & done; wait; echo 4")))

(define-test (sandbox secret-given)
  ;; a secret read on standard input, a new line after it; read past it, the
  ;; program is answered at once that there is no more, never left waiting
  (multiple-value-bind (text outcome said) (sandbox-shell "cat; echo \"ended $?\"" :secret "open sesame" :seconds 10)
    (is eq :done outcome)
    (is string= (format nil "open sesame~%ended 1~%") text)
    (true (search "temporarily unavailable" said) "~a" said))
  ;; an empty one: nothing at all, not even a new line
  (is string= (format nil "ended 1~%") (sandbox-shell "cat; echo \"ended $?\"" :secret "" :seconds 10))
  ;; one longer than the pipe holds is refused, never left to block
  (true (search "it is longer than"
                (handler-case (progn (sandbox-shell "cat" :secret (make-string 100000 :initial-element #\x)) "")
                  (tuatara.io:file-system-error (e) (princ-to-string e)))))
  ;; never among the arguments of what runs
  (false (search "hidden" (sandbox-shell "cat /proc/[0-9]*/cmdline </dev/null; true" :secret "hidden"))))

(define-test (sandbox no-special-files-made)
  ;; a program shut in makes no named pipe and no device
  (multiple-value-bind (text outcome said) (sandbox-shell "mkfifo /tmp/pipe && echo made")
    (is eq :failed outcome)
    (false (search "made" text))
    (true (search "not permitted" said) "~a" said)))

(define-test (sandbox files-given)
  ;; files shown read only, and what is in the environment besides
  (multiple-value-bind (text outcome)
      (sandbox-shell "cat /etc/tuatara/given.txt; echo \" $GIVEN\"; echo x > /etc/tuatara/given.txt 2>/dev/null && echo written; true"
                     :files '(("/etc/tuatara/given.txt" . "given text"))
                     :environment '(("GIVEN" "value")))
    (is eq :done outcome)
    (is string= (format nil "given text value~%") text)))

(define-test (sandbox imagemagick-policy)
  ;; ImageMagick reads the image formats it is told and nothing else, and starts no program
  (when (tuatara.io:sandbox-find-program "magick")
    (with-test-directory (dir)
      (let ((ps (merge-pathnames "page.ps" dir)) (mvg (merge-pathnames "draw.mvg" dir)))
        (alexandria:write-string-into-file (format nil "%!PS~%10 10 moveto (hi) show showpage~%") ps)
        (alexandria:write-string-into-file (format nil "viewbox 0 0 10 10~%rectangle 0 0 5 5~%") mvg)
        (dolist (case (list (list ps "PS") (list mvg "MVG") (list mvg "TXT")))
          (destructuring-bind (path format) case
            (multiple-value-bind (output outcome said)
                (apply #'tuatara.io:sandbox-run-on-file
                       (namestring path) "magick"
                       (list (format nil "~a:-[0]" format) "PAM:-")
                       (tuatara.defaults::magick-sandbox))
              (declare (ignore output))
              (is eq :failed outcome "~a" format)
              (true (search "security policy" said) "~a: ~a" format said))))
        ;; the same MVG read without the policy is drawn: the policy is what refuses it
        (is eq :done (nth-value 1 (tuatara.io:sandbox-run-on-file (namestring mvg) "magick" (list "MVG:-[0]" "PAM:-"))))))))

(define-test (sandbox directories-shown)
  ;; a directory shown read only where it is, one to write in, one whose writes go elsewhere, and where it starts
  (with-test-directory (dir)
    (let* ((root (string-right-trim "/" (namestring dir)))
           (shown (format nil "~a/shown" root)) (written (format nil "~a/written" root))
           (lower (format nil "~a/lower" root)) (upper (format nil "~a/upper" root)) (work (format nil "~a/work" root))
           (hidden (format nil "~a/hidden" root)))
      (dolist (d (list shown written lower upper work hidden)) (ensure-directories-exist (format nil "~a/" d)))
      (alexandria:write-string-into-file "shown" (format nil "~a/a.txt" shown))
      (alexandria:write-string-into-file "lower" (format nil "~a/l.txt" lower))
      (alexandria:write-string-into-file "hidden" (format nil "~a/h.txt" hidden))
      (multiple-value-bind (text outcome said)
          (sandbox-shell (format nil "pwd; cat ~a/a.txt; echo; echo x > ~a/b.txt || echo refused; echo made > made.txt;
cat ~a/l.txt; echo; echo new > ~a/n.txt && cat ~a/n.txt; cat ~a/h.txt 2>/dev/null || echo unseen"
                                 shown shown lower lower lower hidden)
                         :directories (list shown) :writable (list written) :overlays (list (list lower upper work))
                         :directory written :output-limit 4096)
        (is eq :done outcome "~a" said)
        (is equal (list written "shown" "refused" "lower" "new" "unseen")
            (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=)))
      ;; written where it may, the directory seemingly written in as it was, its writes kept apart
      (is string= (format nil "made~%") (alexandria:read-file-into-string (format nil "~a/made.txt" written)))
      (false (probe-file (format nil "~a/b.txt" shown)))
      (false (probe-file (format nil "~a/n.txt" lower)))
      (when (tuatara.io::bwrap-version-at-least-p 0 11)
        (is string= (format nil "new~%") (alexandria:read-file-into-string (format nil "~a/n.txt" upper))))
      ;; the system's own directory in WORK, left unreadable, made removable
      (when (probe-file (format nil "~a/work/" work)) (sb-posix:chmod (format nil "~a/work" work) #o700))))
  ;; a directory reached through a link at the root (/home to /var/home) is found either way
  (let ((arguments (tuatara.io::mount-arguments (list (namestring (user-homedir-pathname))) '() '())))
    (when (ignore-errors (sb-posix:readlink "/home"))
      (true (search (list "--symlink" (sb-posix:readlink "/home") "/home") arguments :test #'equal))))
  ;; a directory reached through a link into /tmp (~/scratch to /tmp/scratch): found by the path through the link
  (with-test-directory (dir)
    (let* ((real (namestring (ensure-directories-exist (format nil
                                                               "/tmp/tuatara-test-~d-~36r/"
                                                               (sb-posix:getpid)
                                                               (random (expt 36 8))))))
           (link (format nil "~alinked" (namestring dir))))
      (unwind-protect
           (progn
             (alexandria:write-string-into-file "found" (format nil "~an.txt" real))
             (sb-posix:symlink (string-right-trim "/" real) link)
             (is string= "found"
                 (sandbox-shell (format nil "cat ~a/n.txt" link) :directories (list link) :directory link)))
        (uiop:delete-directory-tree (pathname real) :validate (lambda (p) (search "/tmp/tuatara-test-" (namestring p)))))))
  ;; a directory in another is shown after it, so that it is seen
  (let ((arguments (tuatara.io::mount-arguments '(("/x/a/b" . "/x/a/b")) '(("/x/a" . "/x/a")) '())))
    (true (< (position "/x/a" arguments :test #'string=) (position "/x/a/b" arguments :test #'string=))))
  ;; what it prints let go when no limit is given; no limit on the files it writes
  (multiple-value-bind (output outcome) (tuatara.io:sandbox-run "head" '("-c" "100000" "/dev/zero") :output-limit nil)
    (is = 0 (length output))
    (is eq :done outcome))
  (is string= (format nil "unlimited~%") (sandbox-shell "ulimit -f" :file-limit nil)))

(define-test (sandbox links-out-of-directories-not-yet-made)
  ;; a path is within a directory only where it really leads, however much of it is not there yet
  (with-test-directory (dir)
    (let* ((top (tuatara.io:real-path (namestring dir)))
           (safe (concatenate 'string top "/safe"))
           (outside (concatenate 'string top "/outside")))
      (ensure-directories-exist (concatenate 'string safe "/"))
      (ensure-directories-exist (concatenate 'string outside "/"))
      (sb-posix:symlink outside (concatenate 'string safe "/link"))
      (true (tuatara.io:really-within-p (concatenate 'string safe "/new/deeper/file") safe))
      (false (tuatara.io:really-within-p (concatenate 'string safe "/link/new/file") safe) "two levels not there")
      (false (tuatara.io:really-within-p (concatenate 'string safe "/link/file") safe) "one level not there")
      (false (tuatara.io:really-within-p (concatenate 'string safe "/new/../link/file") safe) "back into the link")
      (is string= (concatenate 'string outside "/new/file")
          (tuatara.io::real-path-so-far (concatenate 'string safe "/link/new/file")))
      (is string= (concatenate 'string outside "/file")
          (tuatara.io::real-path-so-far (concatenate 'string safe "/new/../link/file"))))))

(define-test (sandbox program-not-started-reported)
  ;; a program that cannot be started gives no output and no success, and debug mode keeps why
  (drain-debug-reports)
  (unwind-protect
       (progn
         (tuatara.runtime:set-debug t)
         (multiple-value-bind (output ok) (tuatara.io:program-output (list "/nonexistent-tuatara-program") :seconds 5)
           (false output)
           (false ok))
         (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                            while report collect (princ-to-string (fourth report)))))
           (true (find-if (lambda (text) (search "running /nonexistent-tuatara-program failed" text)) texts)
                 "~s" texts)))
    (drain-debug-reports)))

(define-test (sandbox program-output-read-in-blocks)
  ;; a program's output read a block at a time: little made besides the text given back
  ;; (4 octets a character); 9 octets a character when it was read a character at a time
  (let* ((size (* 8 1024 1024))
         (consed (loop repeat 3
                       minimize (let ((before (sb-ext:get-bytes-consed)))
                                  (let ((text (tuatara.io:program-output (list (tuatara.io:find-program "head") "-c"
                                                                               (princ-to-string size) "/dev/zero")
                                                                         :seconds 30)))
                                    (is = size (length text)))
                                  (- (sb-ext:get-bytes-consed) before)))))
    (true (< (- consed (* 4 size)) (* 3 size)) "~,1f octets made for each octet of output" (/ consed size))))

(define-test (sandbox command-words)
  ;; a command line split as a shell splits it, no shell running; with :escapes a backslash
  ;; outside quotes is itself, and within double quotes begins one of a few escapes
  (is equal
      '("-H" "-a" "1,0" " " "#" "'" "\"")
      (tuatara.io:command-words "-H -a 1,0 \" \" '#' \"'\" '\"'" :backslash :escapes))
  (is equal (list (string #\Tab)) (tuatara.io:command-words "\"\\t\"" :backslash :escapes))
  (is equal '("C:\\data") (tuatara.io:command-words "'C:\\data'" :backslash :escapes))
  (is equal '("C:\\Users\\x") (tuatara.io:command-words "C:\\Users\\x" :backslash :escapes))
  (false (nth-value 1 (tuatara.io:command-words "\"C:\\data\"" :backslash :escapes)))
  (true (search "\\d" (nth-value 2 (tuatara.io:command-words "\"C:\\data\"" :backslash :escapes))))
  (is equal '("-H" "--arr") (tuatara.io:command-words (format nil "-H~%--arr~%")))
  (is equal '("") (tuatara.io:command-words "''"))
  (false (nth-value 1 (tuatara.io:command-words "-H \"open" :backslash :escapes))))

(define-test (sandbox filtered-through-a-program)
  ;; text written to a program a piece at a time, what it prints and says read meanwhile
  (let ((text (tuatara.text:string-octets (with-output-to-string (out) (dotimes (i 50000) (format out "line ~d~%" i))))))
    (multiple-value-bind (output said exit too-much)
        (tuatara.io:filter-through-program
         (list (tuatara.io:find-program "sh") "-c" "tr a-z A-Z; echo done >&2")
         (lambda (fd) (loop for at from 0 below (length text) by 4096
                            do (tuatara.io:write-octets fd text
                                                        :start at
                                                        :end (min (length text) (+ at 4096))
                                                        :path "a test"))))
      (is = (length text) (length output))
      (true (search "LINE 49999" (tuatara.text:octets-string output)))
      (is string= "done" (string-trim '(#\Newline) said))
      (is eql 0 exit)
      (false too-much)))
  ;; printing more than the limit: ended, nothing taken
  (multiple-value-bind (output said exit too-much)
      (tuatara.io:filter-through-program (list (tuatara.io:find-program "yes"))
                                         (lambda (fd) (declare (ignore fd)))
                                         :limit 100000)
    (declare (ignore said))
    (false output)
    (false exit)
    (true too-much)))

(define-test (sandbox program-given-seconds)
  ;; a program given seconds is ended with what it started once they pass, though it pays no heed to SIGTERM,
  ;; what it printed so far kept and no exit code given
  (let ((began (get-internal-real-time)))
    (multiple-value-bind (output said exit too-much)
        (tuatara.io:filter-through-program
         (list (tuatara.io:find-program "sh") "-c" "trap '' TERM; sleep 60 & echo $!; echo $$; wait")
         nil :seconds 1)
      (declare (ignore said))
      ;; far short of the minute its sleep would last, a busy machine starting and ending programs slowly
      (true (< (tuatara.runtime:seconds-since began) (+ 20 tuatara.io::*program-end-grace*))
            "waited for no longer than its seconds and its ending")
      (false exit)
      (false too-much)
      (let ((pids (uiop:split-string (string-trim '(#\Newline) (tuatara.text:octets-string output))
                                     :separator '(#\Newline))))
        (is = 2 (length pids))
        (dolist (pid pids)
          (true (wait-until (lambda () (not (probe-file (format nil "/proc/~a/" pid)))) 10)
                "~a is ended, killed once it did not end when asked" pid)))))
  ;; printing more than the limit with TRUNCATE: the first octets kept, the rest read and let go
  (multiple-value-bind (output said exit too-much)
      (tuatara.io:filter-through-program (list (tuatara.io:find-program "head") "-c" "100000" "/dev/zero") nil
                                         :limit 1000 :truncate t :seconds 10)
    (declare (ignore said))
    (is = 1000 (length output))
    (is eql 0 exit)
    (true too-much))
  ;; what it reads given as a file in place of WRITE
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "kea.txt" dir) "kea seen")))
      (is string= "kea seen" (tuatara.text:octets-string
                              (tuatara.io:filter-through-program (list (tuatara.io:find-program "cat")) nil
                                                                 :input path :seconds 10))))))

(define-test (sandbox program-ended-once)
  ;; a program kept on the editor's own terminal (:INPUT T, as ssh asking for a password is) is ended at its
  ;; deadline too, by one ending; one whose output cannot be read is ended, not waited for without end
  (let ((endings 0))
    (sb-int:encapsulate 'tuatara.io::end-unwanted-program 'counted
                        (lambda (function &rest arguments) (incf endings) (apply function arguments)))
    (unwind-protect
         (multiple-value-bind (output said exit)
             (tuatara.io:filter-through-program
              (list (tuatara.io:find-program "sh") "-c" "trap '' TERM; echo $$; sleep 60") nil :input t :seconds 1)
           (declare (ignore said))
           (false exit)
           (let ((pid (string-trim '(#\Newline) (tuatara.text:octets-string output))))
             (true (wait-until (lambda () (not (probe-file (format nil "/proc/~a/" pid)))) 10) "~a is ended" pid))
           (is = 1 endings "ended once"))
      (sb-int:unencapsulate 'tuatara.io::end-unwanted-program 'counted)))
  (sb-int:encapsulate 'tuatara.io::read-outputs 'failing
                      (lambda (function &rest arguments)
                        (declare (ignore function arguments))
                        (error "the pipes cannot be read")))
  (unwind-protect
       (let ((began (get-internal-real-time)))
         (false (nth-value 2 (tuatara.io:filter-through-program (list (tuatara.io:find-program "sleep") "60") nil)))
         (true (< (tuatara.runtime:seconds-since began) 10) "ended, not waited for"))
    (sb-int:unencapsulate 'tuatara.io::read-outputs 'failing)
    (drain-debug-reports)))

(define-test (sandbox later-setting-taken)
  ;; of two settings of one variable, the later is the one a program is given
  (let ((environment (tuatara.io:restricted-environment '() "PATH=/usr/bin" "TZ=Pacific/Auckland" "PATH=/kept")))
    (is equal '("PATH=/kept") (remove-if-not (lambda (setting) (uiop:string-prefix-p "PATH=" setting)) environment))
    (true (member "TZ=Pacific/Auckland" environment :test #'string=))))

(define-test (sandbox versions-compared)
  ;; one reading of a version for bwrap and OpenSSH, and one comparison
  (is equal '(0 11 0) (tuatara.io:version-numbers (format nil "bubblewrap 0.11.0~%")))
  (is equal '(9 9) (tuatara.io:version-numbers "OpenSSH_9.9p1, OpenSSL 3.2.2 4 Jun 2024" "OpenSSH_"))
  (is equal '(10 2) (tuatara.io:version-numbers "OpenSSH_10.2p1 Debian" "OpenSSH_"))
  (false (tuatara.io:version-numbers "no version here"))
  (false (tuatara.io:version-numbers "OpenSSL 3.2.2" "OpenSSH_"))
  (true (tuatara.io:version-at-least-p '(8 9) '(8 9)))
  (true (tuatara.io:version-at-least-p '(10 0) '(8 9)))
  (true (tuatara.io:version-at-least-p '(0 11 0) '(0 11)))
  (false (tuatara.io:version-at-least-p '(8 8) '(8 9)))
  (false (tuatara.io:version-at-least-p '(0 9) '(0 10)))
  (true (tuatara.io:version-at-least-p '(1) '(0 99)))
  (false (tuatara.io:version-at-least-p '(0) '(0 1)))
  ;; a pre-release comes before its release, alpha before beta before rc
  (is equal '(0 90 13 -1 1) (tuatara.io:version-numbers "0.90.13-rc1"))
  (false (tuatara.io:version-at-least-p (tuatara.io:version-numbers "0.90.13-rc1") '(0 90 13)))
  (true (tuatara.io:version-at-least-p (tuatara.io:version-numbers "0.90.13-rc1") '(0 90 12)))
  (true (tuatara.io:version-at-least-p (tuatara.io:version-numbers "0.90.13")
                                       (tuatara.io:version-numbers "0.90.13-rc2")))
  (false (tuatara.io:version-at-least-p (tuatara.io:version-numbers "0.90.13-beta.3")
                                        (tuatara.io:version-numbers "0.90.13-rc1")))
  (true (tuatara.io:version-at-least-p (tuatara.io:version-numbers "0.90.13-rc2")
                                       (tuatara.io:version-numbers "0.90.13-rc1"))))

(define-test (sandbox limits-set-by-every-shell)
  ;; the limits are set by the system's /bin/sh, which on Debian and Ubuntu is dash: its ulimit names
  ;; the process limit -p, not -u. Run here by bash made to answer as dash does, the limits are set
  ;; all the same and the program run
  (let* ((arguments (tuatara.io::sandbox-arguments "/bin/bash"
                                                   '("-c" "ulimit -u; ulimit -t") 7 nil
                                                   :file-limit nil
                                                   :processes 77))
         (shell (member "/bin/sh" arguments :test #'equal))
         (as-dash "ulimit() { case \"$1\" in -u) echo \"ulimit: Illegal option -u\" >&2; return 2;; -p) shift; builtin ulimit -u \"$@\";; *) builtin ulimit \"$@\";; esac; }; "))
    (true shell)
    (is equal "-c" (second shell))
    (is equal (format nil "77~%7~%")
        (uiop:run-program (list* "bash" "-c" (concatenate 'string as-dash (third shell)) (nthcdr 3 shell))
                          :output :string :ignore-error-status t))))

(define-test (sandbox starting-thread-never-waited-for-forever)
  ;; a start that ends with a condition no error, or takes too long, is
  ;; given up with that said, and the next start goes on as before
  (multiple-value-bind (result condition) (tuatara.io:in-starter (lambda () (error 'storage-condition)))
    (false result)
    (true (typep condition 'storage-condition)))
  (let ((tuatara.io::*starter-wait* 0.5)
        (began (get-internal-real-time)))
    (multiple-value-bind (result condition) (tuatara.io:in-starter (lambda () (sleep 3) :late))
      (false result)
      (true condition)
      (true (< (tuatara.runtime:seconds-since began) 2))))
  (is eq :fine (tuatara.io:in-starter (lambda () :fine))))

(define-test (sandbox given-up-starting-thread-takes-no-more)
  ;; a starting thread given up on takes no more work, its late answer
  ;; taken for no other start, and every starting thread ends with the rest
  (let ((tuatara.io::*starter-wait* 0.3))
    (multiple-value-bind (result condition) (tuatara.io:in-starter (lambda () (sleep 1) :old))
      (false result)
      (true condition)))
  (is eq :new (tuatara.io:in-starter (lambda () :new)))
  (sleep 1)
  (is eq :newer (tuatara.io:in-starter (lambda () :newer)))
  (true (>= (length tuatara.io::**starters**) 2))
  (let ((starters (copy-list tuatara.io::**starters**)))
    (tuatara.io::end-starters)
    (true (notany #'sb-thread:thread-alive-p starters))))

(define-test (sandbox programs-with-the-editor-outlive-their-thread)
  ;; a program set to end with the editor is started from the thread kept for that, so the thread asking may
  ;; end and the program go on; started so from the asking thread itself, it ends with that thread
  (let ((sleep (tuatara.io:find-program "sleep")))
    (if (null (tuatara.io:setpriv-command sleep '()))
        (skip "no setpriv here" (true nil))
        (flet ((started-from-a-thread (function)
                 ;; the thread ends once setpriv has done its work and become sleep
                 (let ((process nil))
                   (sb-thread:join-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (setf process (funcall function))
                       (tuatara.runtime:wait-until
                        (lambda () (string= "sleep" (tuatara.io::process-name (sb-ext:process-pid process)))) 5))))
                   process)))
          (let ((kept (started-from-a-thread
                       (lambda () (tuatara.io:start-program (list sleep "30") :with-editor t))))
                (lost (started-from-a-thread
                       (lambda () (tuatara.io:start-program (tuatara.io:setpriv-command sleep '("30")))))))
            (unwind-protect
                 (progn
                   (true (tuatara.runtime:wait-until (lambda () (not (sb-ext:process-alive-p lost))) 5)
                         "the control: ended with the thread that started it")
                   (true (sb-ext:process-alive-p kept)))
              (tuatara.io:done-with-program kept)
              (tuatara.io:done-with-program lost)))))))

(define-test (sandbox handed-descriptors-closed-by-the-start)
  ;; descriptors handed over with a start are closed by the thread starting the program, even when the start
  ;; is given up on while under way, so that the asker never closes one the start may still use
  (let ((true-program (tuatara.io:find-program "true")))
    (if (null (tuatara.io:setpriv-command true-program '()))
        (skip "no setpriv here" (true nil))
        (multiple-value-bind (in out) (sb-posix:pipe)
          (unwind-protect
               (let ((stream (sb-sys:make-fd-stream out :output t :buffering :none :auto-close nil)))
                 (let ((tuatara.io::*starter-wait* 0))
                   (handler-case
                       (let ((process (tuatara.io:start-program (list true-program) :output stream :with-editor t
                                                                                    :close-fds (list out))))
                         (when (sb-ext:process-p process) (tuatara.io:done-with-program process)))
                     (error () nil)))
                 ;; the pipe ends once every writing end is closed: the program's, and the one handed over
                 (true (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)))
                             (deadline (tuatara.runtime:deadline-in 10)))
                         (loop until (tuatara.runtime:deadline-passed-p deadline)
                               thereis (and (sb-sys:wait-until-fd-usable in :input 0.2)
                                            (zerop (sb-sys:with-pinned-objects (buffer)
                                                     (sb-posix:read in (sb-sys:vector-sap buffer) 64))))))))
            (sb-posix:close in))))))

(define-test (sandbox untaken-work-let-go-when-replaced)
  ;; work handed to the starting thread by an asker gone without waiting, never taken, lets go of what it
  ;; holds when the next start takes its place
  (is eq :first (tuatara.io:in-starter (lambda () :first)))
  (let ((let-go nil))
    (sb-thread:with-mutex (tuatara.io::**starter-lock**)
      (setf tuatara.io::**starter-work** (list* (list :work) (lambda () :left) (lambda () (setf let-go t)))))
    (is eq :next (tuatara.io:in-starter (lambda () :next)))
    (true let-go)))
