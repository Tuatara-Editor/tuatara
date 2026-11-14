;;;; src/io/sandbox.lisp --- Running a program on an untrusted file, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file from elsewhere is never parsed by the editor, the terminal or the
;;;; window toolkit: a program of the system decodes it (images to pixels, PDF
;;;; to text), shut in by bubblewrap (bwrap). It sees the system's programs and
;;;; libraries (/usr) and nothing else, read only: no home directory, no other
;;;; file, no network, no other process, no way into the terminal; it starts
;;;; with nothing in its environment and can make no new namespaces. Its
;;;; memory, the processor time it takes, the files it writes in its own
;;;; scratch /tmp and what it prints are limited, and it is ended, with all it
;;;; started, when its time is up or the one waiting stops it; every sandbox
;;;; still running is ended when the editor ends, and one left behind by an
;;;; editor killed while bwrap was making it is ended when the sandbox is next
;;;; tried. A seccomp filter refuses it the system calls no decoder needs
;;;; and that reach into the kernel's most exposed parts: mounting,
;;;; namespaces, kernel modules and keys, tracing other processes, io_uring,
;;;; BPF, sockets other than local ones, and pushing keys into a terminal; nor
;;;; may it make a named pipe or a device.
;;;;
;;;; The file is not shown to it by name: it reads it on its standard input, a
;;;; copy of it in memory that nothing can change, made from a descriptor opened
;;;; here the way a safe visit opens a file, so its access time is not changed
;;;; (on your own files) and nothing else in its directory can be reached.
;;;; The file's own descriptor never reaches the program: through it, the file
;;;; could be written, its permissions and times changed.
;;;;
;;;; A program that needs more is shown more, and only that: LaTeX making a
;;;; PDF of a document sees the document's directory and TeX's own, read only,
;;;; and writes in its working directory alone (lisp/export/export.lisp); a
;;;; program that must seek in a file, as an archive program, can be given a
;;;; sealed copy of it at a path of its own.
;;;; It is never shown /tmp, another directory the sandbox makes its own, or
;;;; the directory of your programs' sockets: a socket there would let it have
;;;; a program outside act for it.
;;;;
;;;; bwrap is found on the PATH when first needed and tried once; the result is
;;;; kept, and M-x check-health tries it again. Elsewhere only SANDBOX-STATUS,
;;;; SANDBOX-RUN and SANDBOX-RUN-ON-FILE, SANDBOX-START and the copies they
;;;; take (SEALED-COPY) are used, so that another way of shutting a program in
;;;; can take bwrap's place.
;;;;
;;;; How other programs are found and started is in programs.lisp, what /proc
;;;; tells of processes in processes.lisp, and the systemd scopes that hold the
;;;; sandboxes' memory together in scopes.lisp.

(in-package #:tuatara.io)

(define-setting *sandbox* t
  "Whether the programs decoding files for the editor, and LaTeX making PDFs,
run shut in, by bubblewrap (bwrap) found on the PATH.
- t: shut in (the standard). Without bwrap nothing is decoded, and M-x
  check-health says why.
- nil: decoding is off. Images, PDFs and the like are then shown as octets
  and text only; PDFs are still made of Org and LaTeX files, LaTeX then not
  shut in, and each export says so."
  :values boolean)

(defparameter *bwrap-name* "bwrap"
  "The name of bubblewrap's program, looked for on the PATH while *SANDBOX* is
true.")

(defparameter *sandbox-carried-settings*
  '(*sandbox* *bwrap-name* *sandbox-seconds* *sandbox-memory* *sandbox-total-memory* *sandbox-input-limit*
    *sandbox-seccomp* *program-output-limit*)
  "The settings a program run shut in (SANDBOX-RUN) is run by, for work in the
background that runs one to carry to its thread as they are on the editor's
when it starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defun sandbox-program ()
  "The program that shuts programs in: bubblewrap's name (*BWRAP-NAME*) while
*SANDBOX* is true, else NIL."
  (and *sandbox* *bwrap-name*))

(define-setting *sandbox-seconds* 10
  "The seconds a program decoding a file may run before it is ended, and the
processor time it may take.
Raise it for a file that takes long to decode, such as a large picture;
lower it to end a program stuck on a damaged file sooner. A file whose
program is ended is not shown.
- a number, 1 or more: the seconds allowed; 10 as standard."
  :values (number 1 *))

(define-setting *sandbox-memory* (* 1024 1024 1024)
  "The memory a program decoding a file may take.
Raise it for a decoder that needs more; lower it on a small machine, so that
a damaged file cannot take memory from the rest. A program that needs more
than this fails, and the file is not decoded.
- a whole number, 1 or more: the octets allowed; 1 GB as standard.
- nil: no limit."
  :values (or (member nil) (integer 1 *)))

(define-setting *sandbox-input-limit* (* 1024 1024 1024)
  "The largest file a program decoding files is given.
It gets a copy in memory, which a larger file, or one claiming a huge size
with nothing in it, would fill. A larger one is not decoded, and why is said.
- a whole number, 1 or more: the octets allowed; 1 GB as standard.
- nil: no limit."
  :values (or (member nil) (integer 1 *)))

(define-setting *sandbox-total-memory* (* 2 1024 1024 1024)
  "The memory all the programs decoding one file may take together.
It holds where the system can hold them to it (systemd-run, a scope of the
user's own); each program is still held to *SANDBOX-MEMORY*.
- a whole number, 1 or more: the octets allowed; 2 GB as standard.
- nil: no such limit."
  :values (or (member nil) (integer 1 *)))

(defparameter *sandbox-path* "/usr/local/bin:/usr/bin:/bin"
  "Where a program in the sandbox is looked for: the system's own directories
of programs, the only ones it sees.")

(defparameter *sandbox-said-limit* 4096
  "The octets of what a program in the sandbox writes to its error output that
are kept, to say why it failed.")

(defparameter *sandbox-file-limit* (* 64 1024 1024)
  "The octets a program in the sandbox may write to any one file of its
scratch /tmp.")

(defparameter *sandbox-process-limit* 64
  "The processes and threads a program in the sandbox may have at once, it
and all it starts counted together (only those in the sandbox).")

(define-setting *sandbox-seccomp* t
  "Whether the kernel refuses a program decoding a file the system calls no
decoder needs (see the file's header), on x86-64 and AArch64.
- t: it refuses them (the standard).
- nil: they are left to the program, the rest of the sandbox kept."
  :values boolean)

(sb-ext:defglobal **sandbox-state** nil
  "What was found when the sandbox was last tried (SANDBOX-STATUS), as a list
(STATUS DETAIL PATH PROGRAM), PROGRAM the program SANDBOX-PROGRAM gave,
or NIL before it is first needed.")

(sb-ext:defglobal **sandbox-scope** nil
  "systemd-run's path when it was found, when the sandbox was last tried, to
put a sandbox in a scope of its own whose memory is limited; NIL when not.")

(sb-ext:defglobal **sandbox-lock** (sb-thread:make-mutex :name "sandbox")
  "Held while the sandbox is tried, so that two threads needing it at once try it once.")

(defun system-directory-arguments ()
  "bwrap's arguments showing the program the system's directories, read only:
/usr, the directories beside it that are links into it on most systems (made
links in the sandbox too) or directories of their own on others, and what
programs read to find libraries and alternatives."
  (append (list "--ro-bind" "/usr" "/usr")
          (loop for directory in '("/bin" "/sbin" "/lib" "/lib64" "/lib32")
                for target = (handler-case (sb-posix:readlink directory) (sb-posix:syscall-error () nil))
                append (cond (target (list "--symlink" target directory))
                             ((eq :directory (path-kind directory))
                              (list "--ro-bind" directory directory))))
          (loop for path in '("/etc/ld.so.cache" "/etc/alternatives" "/etc/fonts")
                append (list "--ro-bind-try" path path))))

(defvar *bwrap-versions* (make-hash-table :test 'equal :synchronized t)
  "The version of each bwrap program asked so far, by its path: a list of numbers, or :UNKNOWN.")

(defun bwrap-version-at-least-p (major minor)
  "True when the bwrap found for the program SANDBOX-PROGRAM gives is version
MAJOR.MINOR or later, as it says it is."
  (let ((program (and (sandbox-program) (find-program (sandbox-program)))))
    (when program
      (let ((version (or (gethash program *bwrap-versions*)
                         (setf (gethash program *bwrap-versions*)
                               (or (version-numbers (version-text program)) :unknown)))))
        (and (consp version) (version-at-least-p version (list major minor)))))))

(defparameter *seccomp-refused*
  '((:x86-64 #xC000003E
     ;; acct add_key keyctl request_key bpf chroot pivot_root mount umount2 unshare setns
     163 248 250 249 321 161 155 165 166 272 308
     ;; init_module finit_module delete_module kexec_load kexec_file_load reboot swapon swapoff syslog
     175 313 176 246 320 169 167 168 103
     ;; quotactl quotactl_fd perf_event_open ptrace process_vm_readv process_vm_writev userfaultfd
     179 443 298 101 310 311 323
     ;; uselib lookup_dcookie open_by_handle_at name_to_handle_at fanotify_init
     134 212 304 303 300
     ;; open_tree move_mount fsopen fsconfig fsmount fspick mount_setattr
     428 429 430 431 432 433 442
     ;; io_uring_setup io_uring_enter io_uring_register vhangup clock_adjtime clock_settime
     425 426 427 153 305 227
     ;; settimeofday adjtimex iopl ioperm nfsservctl pidfd_getfd
     164 159 172 173 180 438
     ;; mknod mknodat
     133 259)
    (:arm64 #xC00000B7
     89 217 219 218 280 51 41 40 39 97 268
     105 273 106 104 294 142 224 225 116
     60 443 241 117 270 271 282
     18 265 264 262
     428 429 430 431 432 433 442
     425 426 427 58 266 112
     170 171 42 438
     33))
  "For each processor the seccomp filter knows (a feature of the Lisp, then
the kernel's AUDIT_ARCH number), the system calls refused, by number.")

(defparameter *seccomp-calls*
  '((:x86-64 :clone 56 :clone3 435 :ioctl 16 :socket 41)
    (:arm64 :clone 220 :clone3 435 :ioctl 29 :socket 198))
  "For each processor, the numbers of the system calls the seccomp filter
looks into the arguments of.")

(defun seccomp-program ()
  "The seccomp filter of the sandbox as the octets of a classic BPF program,
as bwrap's --seccomp reads it, for the processor here. It is NIL on one it
does not know. Another processor's calls (32-bit ones on x86-64) end the
program. The calls of *SECCOMP-REFUSED* fail with EPERM. clone3 fails with
ENOSYS (so that its flags, out of reach of the filter, are asked through
clone). clone making a namespace fails too, and so do ioctl pushing input into
a terminal (TIOCSTI, TIOCLINUX) and a socket that is not a local one
\(AF_UNIX). Everything else is let be."
  (let* ((cpu (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*))
         (calls (and cpu (rest (assoc (first cpu) *seccomp-calls*))))
         (code '()))
    (when cpu
      (flet ((op (opcode jt jf k) (push (list opcode jt jf k) code))
             (ret (k) (push (list #x06 0 0 k) code)))
        (let ((allow #x7FFF0000) (kill #x80000000)
              (eperm (logior #x00050000 1)) (enosys (logior #x00050000 38)) (eafnosupport (logior #x00050000 97)))
          (op #x20 0 0 4)                                   ; the processor
          (op #x15 1 0 (second cpu))
          (ret kill)
          (op #x20 0 0 0)                                   ; the call
          (when (eq (first cpu) :x86-64)                    ; x32 calls
            (op #x35 0 1 #x40000000)
            (ret eperm))
          (dolist (number (cddr cpu))
            (op #x15 0 1 number)
            (ret eperm))
          (op #x15 0 1 (getf calls :clone3))
          (ret enosys)
          ;; clone with CLONE_NEWNS, NEWCGROUP, NEWUTS, NEWIPC, NEWUSER, NEWPID, NEWNET or NEWTIME
          (op #x15 0 4 (getf calls :clone))
          (op #x20 0 0 16)
          (op #x45 0 1 #x7E020080)
          (ret eperm)
          (op #x20 0 0 0)
          ;; ioctl TIOCSTI and TIOCLINUX
          (op #x15 0 5 (getf calls :ioctl))
          (op #x20 0 0 24)
          (op #x15 1 0 #x5412)
          (op #x15 0 1 #x541C)
          (ret eperm)
          (op #x20 0 0 0)
          ;; socket of another family than AF_UNIX
          (op #x15 0 4 (getf calls :socket))
          (op #x20 0 0 16)
          (op #x15 1 0 1)
          (ret eafnosupport)
          (op #x20 0 0 0)
          (ret allow)))
      (let ((octets (make-array (* 8 (length code)) :element-type '(unsigned-byte 8))))
        (loop for (opcode jt jf k) in (reverse code)
              for at from 0 by 8
              do (octets-put-integer octets opcode 2 :at at)
                 (setf (aref octets (+ at 2)) jt (aref octets (+ at 3)) jf)
                 (octets-put-integer octets k 4 :at (+ at 4)))
        octets))))

(defun sandbox-filtered-p ()
  "True when programs in the sandbox run under its seccomp filter: turned on
(*SANDBOX-SECCOMP*) and a processor it knows."
  (and *sandbox-seccomp* (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*) t))

(defconstant +unchanging-seals+ 14
  "The seals that keep a file in memory as it is: nothing may write it, make it
shorter or make it longer (F_SEAL_SHRINK, F_SEAL_GROW, F_SEAL_WRITE).")

(defconstant +all-seals+ 15
  "The seals SEALED-MEMORY-FILE gives: +UNCHANGING-SEALS+, and no seal may be
added or taken away (F_SEAL_SEAL).")

(defun sealed-memory-file (name fill &key kept)
  "A descriptor of a new file in memory called NAME, FILL (a function of its
descriptor) having written it, at its start, sealed so that nothing, not even
a program given it, can change it: not written, not made shorter or longer;
with KEPT kept open in programs started (for bwrap to read). Closed, and an
error signalled, when it cannot be made, filled or sealed."
  ;; MFD_ALLOW_SEALING, and MFD_CLOEXEC when not kept
  (let ((fd (%memfd-create name (if kept 2 3))))
    (when (minusp fd) (fail "make a file in memory called" name))
    (tuatara.runtime:with-fd-closed-on-error (fd "the file in memory ~a" name)
      (funcall fill fd)
      (when (minusp (fcntl-int fd 1033 +all-seals+))  ; F_ADD_SEALS
        (fail "seal" "a file in memory"))
      (sb-posix:lseek fd 0 sb-posix:seek-set)
      fd)))

(defun memory-file (name octets &key (kept t))
  "A descriptor of a new file in memory holding OCTETS, at its start, sealed
so that nothing can change it (SEALED-MEMORY-FILE), and with KEPT kept open
in programs started (for bwrap to read)."
  (sealed-memory-file name (lambda (fd) (write-octets fd octets :path name)) :kept kept))

(defun sealed-p (fd &optional (seals +all-seals+))
  "True when file descriptor (FD) has every seal of SEALS (F_GET_SEALS): as
standard, sealed as SEALED-MEMORY-FILE seals a copy."
  (let ((have (fcntl-int fd 1034 0)))  ; F_GET_SEALS
    (and (>= have 0) (= seals (logand have seals)))))

(defun sealed-file-size (fd)
  "The size in octets of the file open as file descriptor (FD) when it is
sealed so that nothing can write it, make it shorter or make it longer
\(+UNCHANGING-SEALS+); NIL when it is not."
  (and (sealed-p fd +unchanging-seals+) (sb-posix:stat-size (sb-posix:fstat fd))))

(defparameter *sandbox-own-directories* '("/usr" "/bin" "/sbin" "/lib" "/lib64" "/lib32" "/tmp" "/proc" "/dev" "/etc")
  "The directories at the root that the sandbox makes itself, never made links
because they are links outside it.")

(defun root-link (path)
  "The link at the root PATH begins with (/home leading to /var/home) and what
it leads to, as two values, or NIL when its top is no link or a directory the
sandbox makes itself (*SANDBOX-OWN-DIRECTORIES*)."
  (let* ((top (subseq path 0 (or (position #\/ path :start (min 1 (length path))) (length path))))
         (target (and (> (length top) 1) (not (member top *sandbox-own-directories* :test #'string=))
                      (handler-case (sb-posix:readlink top) (sb-posix:syscall-error () nil)))))
    (and target (values top target))))

(defun root-link-arguments (paths)
  "bwrap's arguments making in the sandbox the links at the root that PATHS
go through here, each as it is here (/home leading to /var/home, where the
system has it so), so that a path written either way finds what is shown at
its real place."
  (let ((made '()))
    (loop for path in paths
          append (multiple-value-bind (top target) (root-link path)
                   (when (and target (not (member top made :test #'string=)))
                     (push top made)
                     (list "--symlink" target top))))))

(defun root-resolved (path)
  "PATH with a link at the root it begins with (/home leading to /var/home)
followed, as the sandbox makes it (ROOT-LINK-ARGUMENTS), the rest as written;
no / at its end."
  (let ((path (string-right-trim "/" path)))
    (multiple-value-bind (top target) (root-link path)
      (if target
          (concatenate 'string
                       (if (char= (char target 0) #\/) "" "/")
                       (string-right-trim "/" target)
                       (subseq path (length top)))
          path))))

(defun real-place (path)
  "PATH with every link in it followed as far as it exists, the rest as it is
\(REAL-PATH-SO-FAR); PATH as it is under a directory the sandbox makes itself
\(*SANDBOX-OWN-DIRECTORIES*: /tmp is its own there). No / at its end."
  (if (some (lambda (own) (or (string= path own) (uiop:string-prefix-p (concatenate 'string own "/") path)))
            *sandbox-own-directories*)
      (string-right-trim "/" path)
      (real-path-so-far path)))

(defun sandbox-place (entry)
  "Where ENTRY of a sandbox's directories goes, as two values, the directory
shown and where the program sees it: (SOURCE . TARGET); a path alone, shown
at its own place. A place outside the directories the sandbox makes itself
is its real one (REAL-PLACE), so that it is found however it is written."
  (if (consp entry)
      (values (car entry) (real-place (cdr entry)))
      (let ((real (real-place entry)))
        (values real real))))

(defun session-socket-directories ()
  "The directories where your own programs keep the sockets they are reached
through: /run/user/UID and $XDG_RUNTIME_DIR, each where it really is."
  (remove-duplicates (remove nil (list (format nil "/run/user/~d" (sb-posix:getuid))
                                       (let ((runtime (own-variable "XDG_RUNTIME_DIR")))
                                         (and runtime (uiop:string-prefix-p "/" runtime)
                                              (real-path-so-far runtime)))))
                     :test #'string=))

(defun shown-place-p (place)
  "True when PLACE, where it really is, may be shown to a program shut in: not
a directory the sandbox makes its own (*SANDBOX-OWN-DIRECTORIES*), whose real
one holds the sockets of programs outside it (the X server, ssh-agent, tmux),
nor the directory of your own programs' sockets (SESSION-SOCKET-DIRECTORIES)
or one holding it. A program reaching such a socket could have a program
outside the sandbox act for it. The root is let by: it is never shown."
  (let ((place (string-right-trim "/" place)))
    (not (and (plusp (length place))
              (or (member place *sandbox-own-directories* :test #'string=)
                  (some (lambda (sockets) (path-within-p sockets place)) (session-socket-directories)))))))

(defun check-shown-place (place)
  "Signal a FILE-SYSTEM-ERROR when PLACE, where it really is, may not be shown
to a program shut in (SHOWN-PLACE-P). Show the files it needs instead."
  (unless (shown-place-p place)
    (error 'file-system-error :operation "show to a program shut in" :path (string-right-trim "/" place)
                              :reason "programs outside the sandbox are reached through it")))

(defun mount-arguments (directories writable overlays)
  "bwrap's arguments showing DIRECTORIES read only, WRITABLE ones to write in
and OVERLAYS, as SANDBOX-RUN has them, those that exist, a directory in
another shown after it, so that both are seen; with the links at the root
their paths go through (ROOT-LINK-ARGUMENTS). An entry may be a single file,
shown alone. A directory a link leads to is shown where it is written too, so
that a path through the link finds it. The root itself is never shown: the
sandbox's root is its own. A place outside programs are reached through is
refused (CHECK-SHOWN-PLACE), and so is a directory to write in that is, or
holds, a place nothing may be written in (CHECK-TREE-WRITING)."
  (let ((mounts '()) (written '()))
    (flet ((add (entry how)
             (multiple-value-bind (source target) (sandbox-place entry)
               (check-shown-place (if (consp entry) (real-place source) source))
               (when (eq how :write)
                 (check-tree-writing source "let a program write in"))
               (push (if (consp entry) (cdr entry) entry) written)
               ;; at its real place, and where it is written when a link leads there (~/scratch to /tmp/scratch)
               (dolist (place (remove-duplicates (list target (if (consp entry) target (root-resolved entry)))
                                                 :test #'string=))
                 (push (cons place (ecase how
                                     (:read (list "--ro-bind-try" source place))
                                     (:write (list "--bind-try" source place))))
                       mounts)))))
      (dolist (entry directories) (add entry :read))
      (dolist (entry writable) (add entry :write))
      (loop for (source upper work) in overlays
            do (multiple-value-bind (lower target) (sandbox-place source)
                 (check-shown-place lower)
                 (push source written)
                 (push (cons target (if (bwrap-version-at-least-p 0 11)
                                        (list "--overlay-src" lower "--overlay" upper work target)
                                        (list "--ro-bind-try" lower target)))
                       mounts))))
    ;; the last given for a place wins; a parent before what is in it; the root itself never
    (setf mounts (stable-sort (remove-duplicates (remove "" (nreverse mounts) :key #'car :test #'string=)
                                                 :key #'car :test #'string= :from-end nil)
                              #'< :key (lambda (mount) (length (car mount)))))
    (append (root-link-arguments (reverse written))
            (loop for mount in mounts append (cdr mount)))))

(defun sandbox-arguments (program arguments seconds memory &key seccomp files environment directories writable overlays
                                                              (directory "/tmp") (file-limit *sandbox-file-limit*)
                                                              (processes *sandbox-process-limit*) (scratch t)
                                                              (descriptors nil))
  "Bwrap's arguments for running PROGRAM with ARGUMENTS shut in. Each process
is limited to SECONDS of processor time (NIL: no limit) and MEMORY octets (NIL:
no limit). Its scratch /tmp is limited to MEMORY octets too. It is limited to
PROCESSES processes and threads, and to files of FILE-LIMIT octets (NIL: no
limit). SECCOMP is the file descriptor (FD) of its seccomp filter. FILES is
the descriptors of files it is shown read only, each (PATH . FD). ENVIRONMENT
is what it has in its environment besides, each (NAME VALUE), a later one for
the same NAME in place of the earlier. DIRECTORIES, WRITABLE and OVERLAYS are
the directories it is shown as SANDBOX-RUN says. DIRECTORY is the one it
starts in. Without SCRATCH it has no /tmp at all. DESCRIPTORS, when given, is
the most files it may have open at once."
  (append (list "--unshare-all" "--unshare-user" "--disable-userns" "--die-with-parent" "--new-session"
                "--cap-drop" "ALL" "--clearenv"
                "--setenv" "PATH" *sandbox-path* "--setenv" "HOME" "/tmp" "--setenv" "LC_ALL" "C.UTF-8")
          (loop for (name value) in environment append (list "--setenv" name value))
          (system-directory-arguments)
          (list "--proc" "/proc" "--dev" "/dev")
          ;; the scratch /tmp is memory too: held to the memory a program may take, where bwrap can (0.10 on)
          (and scratch memory (bwrap-version-at-least-p 0 10) (list "--size" (princ-to-string memory)))
          (and scratch (list "--tmpfs" "/tmp"))
          (mount-arguments directories writable overlays)
          (loop for (path . fd) in files append (list "--ro-bind-data" (princ-to-string fd) path))
          (and seccomp (list "--seccomp" (princ-to-string seccomp)))
          (list "--chdir" directory
                "--" "/bin/sh" "-c"
                (concatenate 'string "ulimit -c 0 && ulimit -v \"$1\" && ulimit -t \"$2\" && ulimit -f \"$3\" && "
                             "{ ulimit -u \"$4\" 2>/dev/null || ulimit -p \"$4\"; } && "
                             "{ [ \"$5\" = as-is ] || ulimit -n \"$5\"; } && shift 5 && exec \"$@\"")
                "sh"
                (if memory (princ-to-string (ceiling memory 1024)) "unlimited")
                (if seconds (princ-to-string (max 1 (ceiling seconds))) "unlimited")
                (if file-limit (princ-to-string (ceiling file-limit 512)) "unlimited")
                (princ-to-string processes)
                (if descriptors (princ-to-string descriptors) "as-is")
                program)
          arguments))

;;; Sandboxes running, and their end

(defparameter *sandbox-mark-name* "TUATARA_SANDBOX"
  "The name in bwrap's own environment that marks it as one Tuatara started:
the program shut in never sees it, its environment cleared.")

(sb-ext:defglobal **sandbox-mark** nil
  "What SANDBOX-MARK gives, made when first needed; NIL until then.")

(defun sandbox-mark ()
  "The NAME=VALUE put in the environment of bwrap itself in every sandbox this
run of the editor starts, by which they are known in /proc: *SANDBOX-MARK-NAME*
and a name no one can guess."
  (or **sandbox-mark**
      (setf **sandbox-mark** (format nil "~a=~a" *sandbox-mark-name* (random-name)))))

(sb-ext:defglobal **sandboxes** '()
  "The sandboxes running, each (PROCESS . THREAD): bwrap, as started, and the
thread that started it, from their start until they are ended (END-SANDBOX).")

(sb-ext:defglobal **sandboxes-lock** (sb-thread:make-mutex :name "sandboxes")
  "Held while **SANDBOXES** changes.")

(sb-ext:defglobal **sandboxes-closed** nil
  "True once the editor is ending: a sandbox started after then is ended at once.")

(defun end-sandbox (process &key (wait t))
  "End PROCESS, bwrap as started (START-SANDBOX), and everything in its
sandbox, at once: the first process of the sandbox, bwrap's child, whose end
ends every process in it, then bwrap with its process group, which holds the
sandbox's first process too while bwrap is still making the sandbox. Its end
is waited for here, or, unless WAIT, on a thread of its own. Ended while
systemd-run was still having its scope made, before it became bwrap, that
scope is stopped then: the service manager would otherwise keep it, empty,
for good. Any other program started is ended the same way, with what it
started."
  (let ((unit (getf (sb-ext:process-plist process) :scope))
        (starting nil))
    (when (process-unreaped-p process)
      (let ((pid (sb-ext:process-pid process)))
        (setf starting (and unit (string= "systemd-run" (process-name pid))))
        (dolist (child (process-children process))
          (signal-process-if child (lambda () (eql pid (process-parent child))) sb-unix:sigkill))
        (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
          (sb-ext:process-kill process sb-unix:sigkill :process-group))))
    (sb-thread:with-mutex (**sandboxes-lock**)
      (setf **sandboxes** (remove process **sandboxes** :key #'car)))
    (flet ((reap ()
             (tuatara.runtime:ignoring-errors-reported ("waiting for ~a to end" process) (sb-ext:process-wait process))
             (tuatara.runtime:ignoring-errors-reported ("closing ~a" process) (sb-ext:process-close process))
             (when starting (stop-scopes (list unit)))))
      (if wait
          (reap)
          (tuatara.runtime:make-guarded-thread #'reap :name "sandbox ending")))))

(defun end-sandboxes (&key thread)
  "End every sandbox running (END-SANDBOX), or, with THREAD, those started
from that thread, for it to call before it ends: a sandbox ends when the
thread that started it ends, and ended that way at the wrong moment it may
leave a process behind."
  (dolist (process (sb-thread:with-mutex (**sandboxes-lock**)
                     (loop for (process . started-by) in **sandboxes**
                           when (or (null thread) (eq thread started-by))
                             collect process)))
    (end-sandbox process)))

(defun close-sandboxes ()
  "End every sandbox running, and any started after, as the editor ends."
  (setf **sandboxes-closed** t)
  (end-sandboxes))

(defun forget-sandboxes ()
  "End every sandbox running, as a saved image is made, and forget the mark of
this run's sandboxes and the owner its scopes are named for: each run of the
image has its own."
  (end-sandboxes)
  (setf **sandbox-mark** nil
        **scope-owner** nil))

(pushnew 'close-sandboxes sb-ext:*exit-hooks*)
(pushnew 'forget-sandboxes sb-ext:*save-hooks*)

(defun process-marked-p (pid)
  "True when the process with process id (PID) has *SANDBOX-MARK-NAME* in its
environment: bwrap of a sandbox Tuatara started."
  (let ((environment (proc-text (format nil "/proc/~d/environ" pid)))
        (name (format nil "~a=" *sandbox-mark-name*)))
    (and environment
         (or (uiop:string-prefix-p name environment)
             (search (concatenate 'string (string (code-char 0)) name) environment))
         t)))

(defun sandbox-left-p (pid)
  "True when the process with process id (PID) is a sandbox's first process
left behind: bwrap, marked (PROCESS-MARKED-P), the first of a namespace of
processes of its own, its parent no marked bwrap. That process waits for
bwrap's go-ahead before it makes the sandbox; bwrap ended before it gives
it, it waits for ever, and the kernel lets no signal but SIGKILL end it."
  (and (string= "bwrap" (process-name pid))
       (process-marked-p pid)
       (let ((numbers (process-status-numbers pid "NSpid")))
         ;; its number in its own namespace, the last, is 1
         (and (> (length numbers) 1) (eql 1 (car (last numbers)))))
       (let ((parent (process-parent pid)))
         (and parent (not (process-marked-p parent))))))

(defun end-sandboxes-left ()
  "End the sandboxes left behind by an editor ended before it could end them
\(SANDBOX-LEFT-P), with SIGKILL; how many."
  (loop for pid in (process-numbers)
        count (and (sandbox-left-p pid)
                   (signal-process-if pid (lambda () (sandbox-left-p pid)) sb-unix:sigkill))))

(defun start-sandbox (bwrap program arguments stdin seconds memory
                      &rest keys &key files (total *sandbox-total-memory*) (outputs :stream) started &allow-other-keys)
  "Start bwrap at the path BWRAP on PROGRAM with ARGUMENTS, STDIN its input,
shown FILES as SANDBOX-RUN has them, KEYS the rest of SANDBOX-ARGUMENTS's;
the process, among the sandboxes running until it is ended (END-SANDBOX). In
a scope whose memory is held to TOTAL (*SANDBOX-TOTAL-MEMORY* unless given)
when systemd-run can make one. Its outputs are streams to read, or let go
when OUTPUTS is NIL. STARTED, when given, is called with the process as soon
as it is counted, before anything can stop this thread, to keep it where the
caller ends it."
  (let ((seccomp nil) (data '()))
    (unwind-protect
         (let* ((program-octets (and *sandbox-seccomp* (seccomp-program)))
                (arguments (progn
                             (when program-octets (setf seccomp (memory-file "tuatara-seccomp" program-octets)))
                             (loop for (path . contents) in files
                                   do (push (cons path (if (stringp contents)
                                                           (memory-file "tuatara-sandbox-file"
                                                                        (tuatara.text:string-octets contents))
                                                           (sealed-for-program contents)))
                                            data))
                             (apply #'sandbox-arguments program arguments seconds memory
                                    :seccomp seccomp :files (reverse data)
                                    (alexandria:remove-from-plist keys :files :total :outputs :started))))
                (keep (remove nil (cons seccomp (mapcar #'cdr data))))
                (scope (and total **sandbox-scope**))
                (unit (and scope (scope-name))))
           ;; counted among those running before this thread can be ended
           (sb-sys:without-interrupts
             (let ((process
                     (if scope
                         ;; systemd-run moves itself into the scope and becomes bwrap: still this one's child
                         (start-program (list* scope (append (scope-arguments total unit) (list bwrap) arguments))
                                        :input stdin :preserve-fds keep :output outputs :error outputs
                                        :external-format :default :environment (cons (sandbox-mark)
                                                                                     (scope-environment)))
                         (start-program (cons bwrap arguments)
                                        :input stdin :preserve-fds keep :output outputs :error outputs
                                        :external-format :default :environment (list (sandbox-mark))))))
               (when unit (setf (getf (sb-ext:process-plist process) :scope) unit))
               (sb-thread:with-mutex (**sandboxes-lock**)
                 (push (cons process sb-thread:*current-thread*) **sandboxes**))
               (when started (funcall started process))
               (when **sandboxes-closed**
                 (end-sandbox process))
               process)))
      ;; bwrap has its own descriptors of them once started
      (when seccomp (tuatara.runtime:close-fd-quietly seccomp "the sandbox's system call filter"))
      (dolist (file data) (tuatara.runtime:close-fd-quietly (cdr file) "~a" (car file))))))

(defun sandbox-start (program arguments stdin &key memory directories (processes *sandbox-process-limit*) descriptors)
  "Start PROGRAM (a full path to one it is shown) with ARGUMENTS shut in as
SANDBOX-RUN shuts in a program, left running, with no limit on the processor
time it takes, no /tmp, no file it may write into (writing one ends it) and
its outputs let go; STDIN, a stream on a descriptor, its standard input. It
may take MEMORY octets (NIL: no limit), held to them together with all it
starts where systemd-run can, have PROCESSES processes and threads and
DESCRIPTORS files open (NIL: as it is); DIRECTORIES are directories it is
shown, read only, as SANDBOX-RUN has them. The process, or NIL when there is
no sandbox here (SANDBOX-STATUS). Ending it ends all it started."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (declare (ignore detail))
    (when (eq status :ready)
      (start-sandbox bwrap program arguments stdin nil memory
                     :total memory :outputs nil :directories directories :processes processes
                     :descriptors descriptors :scratch nil :directory "/" :file-limit 0))))

(defun sandbox-memory-scope-p ()
  "True when what is started shut in is held, with all it starts, to its
memory in a scope of its own (systemd-run); without one, each process is
held only to its own address space."
  (and (eq :ready (sandbox-status)) **sandbox-scope** t))

(defun %sandbox-run (bwrap program arguments input seconds memory output-limit cancel ended-well kept &rest keys)
  "Run PROGRAM with ARGUMENTS in the sandbox bwrap at the path BWRAP makes,
KEYS those of SANDBOX-ARGUMENTS but SECCOMP, FILES as SANDBOX-RUN has them; it
ends well with an exit code among ENDED-WELL; what it prints is KEPT, or only
counted. See SANDBOX-RUN."
  (let ((stdin (and input (sb-sys:make-fd-stream input :input t :element-type '(unsigned-byte 8)
                                                        :buffering :none :auto-close nil)))
        (process nil) (outcome nil) (output nil) (said nil)
        (deadline (tuatara.runtime:deadline-in seconds)))
    (unwind-protect
         (let ((started (apply #'start-sandbox bwrap program arguments stdin seconds memory
                               :started (lambda (started) (setf process started)) keys)))
           ;; what it prints, until both its outputs end
           (multiple-value-bind (outputs over late cancelled)
               (read-outputs (list (sb-sys:fd-stream-fd (sb-ext:process-output started))
                                   (sb-sys:fd-stream-fd (sb-ext:process-error started)))
                             (list output-limit *sandbox-said-limit*) (list nil t)
                             :kept (list (and output-limit kept) t) :deadline deadline :cancel cancel
                             :too-much (lambda () (setf outcome :too-much) :stop))
             (declare (ignore over))
             (setf output (first outputs) said (second outputs))
             (cond (outcome)
                   (cancelled (setf outcome :cancelled))
                   (late (setf outcome :timeout))))
           ;; its end: it may have shut its outputs and gone on
           (loop while (and (null outcome) (sb-ext:process-alive-p started))
                 do (cond ((and cancel (car cancel)) (setf outcome :cancelled))
                          ((tuatara.runtime:deadline-passed-p deadline) (setf outcome :timeout))
                          (t (sleep 0.005))))
           (unless outcome
             (setf outcome (if (and (eq (sb-ext:process-status started) :exited)
                                    (member (sb-ext:process-exit-code started) ended-well))
                               :done
                               :failed))))
      (when process (end-sandbox process)))
    (values (or output (make-array 0 :element-type '(unsigned-byte 8))) outcome
            (said-text (or said (make-array 0 :element-type '(unsigned-byte 8)))))))

(defun try-sandbox (program)
  "Find PROGRAM, bwrap or a path to it, and try it, as a list (STATUS DETAIL
PATH): see SANDBOX-STATUS. NIL for PROGRAM is the sandbox turned off.
The sandboxes left behind by an editor ended before it could end them are
ended first (END-SANDBOXES-LEFT), and the scopes left by one stopped in the
background (END-SCOPES-LEFT)."
  (let ((path (and program (find-program program))))
    (cond ((null program) (list :off "turned off (*sandbox* is nil)" nil))
          ((null path) (list :missing (format nil "~a not found on the PATH" program) nil))
          (t (let ((left (end-sandboxes-left)))
               (when (plusp left)
                 (tuatara.runtime:debug-report :job "sandbox: ~d left behind by an editor gone, ended" left)))
             (let ((version (said-text (sb-ext:string-to-octets (version-text path)))))
               (multiple-value-bind (output outcome said)
                   (handler-case (tuatara.runtime:reporting-errors ("trying the sandbox ~a" path)
                                   (%sandbox-run path "true" '() nil 10 (* 64 1024 1024) 0 nil '(0) t))
                     (error (e) (values nil :failed (princ-to-string e))))
                 (declare (ignore output))
                 (setf **sandbox-scope** (try-scope))
                 (when **sandbox-scope**
                   (tuatara.runtime:make-guarded-thread
                    (lambda ()
                      (let ((left (end-scopes-left)))
                        (when (plusp left)
                          (tuatara.runtime:debug-report :job "sandbox: ~d scopes of editors gone, stopped" left))))
                    :name "sandbox scopes left"))
                 (if (eq outcome :done)
                     (list :ready (format nil "~a (~a)~@[, together held to ~a of memory (systemd-run)~]"
                                          (if (plusp (length version)) version program) path
                                          (and **sandbox-scope** *sandbox-total-memory*
                                               (format nil "~,1f GB" (/ *sandbox-total-memory* (expt 1024 3)))))
                           path)
                     (list :broken (format nil "~a (~a) cannot run: ~a" (if (plusp (length version)) version program)
                                           path (if (plusp (length said)) said (string-downcase outcome)))
                           path))))))))

(defun sandbox-find-program (name)
  "The full path of the program NAME where a program in the sandbox finds it
(the system's own directories of programs), or NIL when it is not there: a
program elsewhere, as in the home directory, cannot be run shut in."
  (and (not (find #\/ name)) (find-program name *sandbox-path*)))

(defun system-program (name)
  "The program NAME found: by its full path from the root, as it is; by
name, only in the system's own directories of programs (SANDBOX-FIND-PROGRAM),
never in a directory of yours your PATH may name. NIL when there is none, as
for a path not from the root (./gpg)."
  (if (uiop:string-prefix-p "/" name)
      (find-program name)
      (sandbox-find-program name)))

(defun sandbox-status (&key again)
  "Whether programs decoding files can be shut in here, as three values: a
keyword, :READY, :MISSING (bwrap is not on the PATH), :BROKEN (it is there but
cannot run, as where the system allows no user namespaces) or :OFF
(*SANDBOX* is NIL); a sentence saying which program, its version and
where, or what is wrong; and bwrap's path. Tried the first time it is needed
and kept for the program tried (SANDBOX-PROGRAM), read once: tried again
when that value has changed, or with AGAIN. Takes a few milliseconds the
first time: call it from a thread doing work in the background."
  (let* ((program (sandbox-program))
         (state (sb-thread:with-mutex (**sandbox-lock**)
                  (when (or again (null **sandbox-state**) (not (equal program (fourth **sandbox-state**))))
                    (setf **sandbox-state** (append (try-sandbox program) (list program))))
                  **sandbox-state**)))
    (values (first state) (second state) (third state))))

(defun sandbox-input-refusal (size)
  "Why a file of SIZE octets is not given to a program decoding it, in words,
or NIL when it may be: it is larger than *SANDBOX-INPUT-LIMIT*."
  (when (and *sandbox-input-limit* (> size *sandbox-input-limit*))
    (format nil "it is ~:d octets, more than a decoder is given (*sandbox-input-limit*, ~:d)"
            size *sandbox-input-limit*)))

(defun sealed-copy (fd &key kept)
  "A copy in memory of the file open as file descriptor (FD), as a new
descriptor at its start, sealed so that nothing, not even a program given
it, can change it: not written, not made shorter or longer; with KEPT kept
open in programs started (for bwrap to read). FD's own offset is left alone.
A file larger than *SANDBOX-INPUT-LIMIT* is refused with a FILE-SYSTEM-ERROR."
  (let* ((size (stamp-size (fd-stamp fd "a file")))
         (refusal (sandbox-input-refusal size)))
    (when refusal
      (error 'file-system-error :operation "decode" :path "a file" :reason refusal))
    (sealed-memory-file "tuatara-decode" (lambda (copy) (copy-fd-contents fd copy size)) :kept kept)))

(defun reopen-sealed (fd)
  "A descriptor of its own on the sealed copy open as file descriptor (FD), at
its start, read only: programs reading it at once each read from their own
place."
  (handler-case (sb-posix:open (format nil "/proc/self/fd/~d" fd) sb-posix:o-rdonly)
    (sb-posix:syscall-error (e) (fail "open again" "a copy in memory" (sb-posix:syscall-errno e)))))

(defun sealed-for-program (fd)
  "A descriptor of a sealed copy of the file open as file descriptor (FD), at
its start, kept open in programs started: a new copy (SEALED-COPY), or, FD
being such a copy already, a descriptor of its own on it (REOPEN-SEALED), so
that one copy serves many programs."
  (if (sealed-p fd) (reopen-sealed fd) (sealed-copy fd :kept t)))

(defun sandbox-status-now ()
  "SANDBOX-STATUS's first value without waiting, for the editor's own thread:
what was found when the sandbox was last tried; :OFF or :MISSING, told at
once; or :UNKNOWN while bwrap is there but not tried yet (or tried again), the
trying then started in the background."
  (let ((state **sandbox-state**))
    (cond ((and state (equal (sandbox-program) (fourth state))) (first state))
          ((null (sandbox-program)) :off)
          ((null (find-program (sandbox-program))) :missing)
          (t (unless (sb-thread:mutex-owner **sandbox-lock**)
               (tuatara.runtime:make-guarded-thread
                (lambda () (tuatara.runtime:ignoring-errors-reported ("trying the sandbox") (sandbox-status)))
                :name "sandbox trial"))
             :unknown))))

(defun secret-pipe (secret)
  "A pipe holding SECRET, a string, and a new line after it (nothing at all
for an empty one), as two values: the descriptor to read it from and the one
it was written through, which must be kept open while a program reads, then
closed. Read past SECRET, it answers at once that there is nothing more
\(EAGAIN), so that a program asking again fails, never waits nor takes an
empty line for an answer. Both ends are closed in programs started but the
one given the reading end. A secret longer than the pipe holds is refused
with a FILE-SYSTEM-ERROR, never left to block."
  (multiple-value-bind (in out) (new-pipe +o-cloexec+)
    (let* ((line (if (string= secret "") (make-string 0) (concatenate 'string secret (string #\Newline))))
           (octets (tuatara.text:string-octets line))
           (made nil))
      (unwind-protect (progn (let ((room (fcntl-int out 1032 0)))         ; F_GETPIPE_SZ
                               (when (and (plusp room) (> (length octets) room))
                                 (error 'file-system-error :operation "pass" :path "a secret"
                                                           :reason (format nil "it is longer than ~:d octets" room))))
                             (write-octets out octets :path "a pipe")
                             (sb-posix:fcntl in sb-posix:f-setfl (logior (sb-posix:fcntl in sb-posix:f-getfl)
                                                                         sb-posix:o-nonblock))
                             (setf made t))
        (fill line #\Nul)
        (fill octets 0)
        (unless made (sb-posix:close in) (sb-posix:close out))))
    (values in out)))

(defun sandbox-run (program arguments &key input secret (seconds *sandbox-seconds*) (memory *sandbox-memory*)
                                           (output-limit (* 256 1024 1024)) (output-kept t) cancel files
                                           environment directories
                                           writable overlays (directory "/tmp") (file-limit *sandbox-file-limit*)
                                           (ended-well '(0)))
  "Run PROGRAM (a name looked for on the sandbox's PATH, /usr/bin:/bin, or a
full path to one it is shown) with ARGUMENTS (strings, never through a shell)
shut in. It sees the system's programs and libraries, read only, and nothing
else (no home directory, no network, no other process). Its time, memory and
the files it writes in its own /tmp are limited.
It reads INPUT, an open descriptor, on its standard input from its start
\(nothing when NIL). This is a sealed copy of it (SEALED-COPY), never INPUT
itself. An INPUT that is such a copy already is read as it is, through a
descriptor of its own (one copy can serve many runs, at once too). With
SECRET, a string, in place of INPUT, it reads that on its standard input
instead, through a pipe, followed by a new line (none for an empty one), and
nothing after it. Asking again, it is answered at once that there is nothing
more (SECRET-PIPE). SECRET is never in its arguments, which other programs
can see, nor in the debug log.
Waits for it: call it from a thread doing work in the background. It is ended
after SECONDS, when it prints more than OUTPUT-LIMIT octets (NIL: what it
prints is let go, never kept; with OUTPUT-KEPT NIL, counted against it but
not kept), or when CANCEL, a cons, has a true car. It may take MEMORY octets
\(NIL: no limit), and write files of FILE-LIMIT octets at most (NIL: no limit).
Returns three values. The first is the octets it printed. The second is how it
went: :DONE (it ended well, with an exit code among ENDED-WELL, 0 alone unless
given), :FAILED (it ended with an error, or ran out of memory), :TIMEOUT,
:TOO-MUCH, :CANCELLED, or :UNAVAILABLE when there is no sandbox here
\(SANDBOX-STATUS). The third is what it said on its error output, or why there
is no sandbox.
FILES are files it is shown at PATH, read only, each (PATH . TEXT) or
\(PATH . FD): FD the descriptor of an open file it is shown a sealed copy of,
as of INPUT, so that a program that must seek in a file (an archive) reads
it without reaching the file itself or anything beside it. ENVIRONMENT is
what it has in its environment besides PATH, HOME (/tmp) and LC_ALL, or in
place of them, each (NAME VALUE).
DIRECTORIES are directories of yours it is shown, read only, each
\(SOURCE . TARGET). TARGET is where it sees SOURCE (under /tmp, its home, for
LibreOffice's fonts), or a path alone, seen where it is (its real place, links
in it followed, a link at the root as /home kept as it is here). An entry may
be a single file, shown alone. WRITABLE is the same, for directories it may
write in. /tmp and the sandbox's other own directories, and the directory of
your programs' sockets, are refused with a FILE-SYSTEM-ERROR
\(CHECK-SHOWN-PLACE).
OVERLAYS are directories it sees where they are and may seem to write in, each
\(SOURCE UPPER WORK). What it writes goes to UPPER, a directory of the editor's
own, SOURCE never changed. WORK is an empty directory beside UPPER that the
system needs for it (where bwrap cannot do that, before 0.11, SOURCE is shown
read only). Those that do not exist are left out. A directory in another one
shown is seen in it. DIRECTORY is where it starts, /tmp unless given."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (if (eq status :ready)
        (multiple-value-bind (copy writer)
            (cond (secret (secret-pipe secret))
                  (input (sealed-for-program input)))
          (unwind-protect
               (let ((start (get-internal-real-time)))
                 (multiple-value-bind (output outcome said)
                     (%sandbox-run bwrap program arguments copy seconds memory output-limit cancel ended-well
                                   output-kept :files files :environment environment :directories directories
                                   :writable writable :overlays overlays :directory directory :file-limit file-limit)
                   (tuatara.runtime:debug-report :job "sandbox: ~a ~{~a~^ ~}: ~(~a~) in ~,3f s, ~:d octets~@[: ~a~]"
                                                 program arguments outcome (tuatara.runtime:seconds-since start)
                                                 (length output) (and (plusp (length said)) said))
                   (values output outcome said)))
            (when copy (sb-posix:close copy))
            (when writer (sb-posix:close writer))))
        (values (make-array 0 :element-type '(unsigned-byte 8)) :unavailable detail))))

(defun sandbox-run-on-file (path program arguments &rest keys)
  "Run PROGRAM with ARGUMENTS shut in, as SANDBOX-RUN does (KEYS are its own
but INPUT), reading the file at PATH on its standard input. The file is opened
here, its access time left alone where the system allows; what is not a
regular file, a named pipe or a device, is refused without being opened, with
a FILE-SYSTEM-ERROR."
  (let ((fd (open-for-reading path t :accept '(:regular) :operation "decode")))
    (unwind-protect (apply #'sandbox-run program arguments :input fd keys)
      (sb-posix:close fd))))
