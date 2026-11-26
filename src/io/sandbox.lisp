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
;;;; may it make a named pipe or a device. A program given a copy on the drive
;;;; may not change any file's permissions or access list either.
;;;;
;;;; The file is not shown to it by name: it reads it on its standard input, a
;;;; copy of it that nothing can change, made from a descriptor opened here the
;;;; way a safe visit opens a file, so its access time is not changed (on your
;;;; own files) and nothing else in its directory can be reached. The copy of
;;;; a small file is in memory and sealed. That of a large one is a file with
;;;; no name in the cache directory, which no other program can open by a
;;;; name and which is gone once closed, even after a crash: it is read only
;;;; to all, the program given a descriptor that only reads, and its stamp is
;;;; looked at again before each program is given it and after each is done,
;;;; so that a change by a program that wrote it all the same is found and the
;;;; copy not used again.
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
;;;; A package's function run by SANDBOX-CALL is shut in this way itself, and
;;;; no sandbox can be made inside it. The programs it starts run in its own
;;;; shut-in place, under the seccomp filter it was given, each in a process
;;;; group of its own with an environment of its own and the same limits, the
;;;; files they are given written at their paths there while they run. Landlock
;;;; (through util-linux's setpriv) holds each to reading the system's
;;;; directories and what it is shown, and to writing in a new directory of its
;;;; own, its home, taken away whole once it ends; so is anything it leaves
;;;; running, so that nothing one program writes reaches the next.
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
    *sandbox-memory-copy-limit* *sandbox-seccomp* *program-output-limit*)
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
- a time, 1 second or more: seconds, or words such as \"90s\" or \"2m\"; 10 as
  standard."
  :values (seconds 1 *))

(define-setting *sandbox-memory* (* 1024 1024 1024)
  "The memory a program decoding a file may take.
Raise it for a decoder that needs more; lower it on a small machine, so that
a damaged file cannot take memory from the rest. A program that needs more
than this fails, and the file is not decoded.
- a size, 1 octet or more: a whole number of octets, or words such as \"2G\"; 1 GB
  as standard.
- nil: no limit."
  :values (or (member nil) (octets 1 *)))

(define-setting *sandbox-input-limit* nil
  "The largest file a program decoding files is given.
It gets a copy: in memory up to *SANDBOX-MEMORY-COPY-LIMIT*, else in the cache
directory (*CACHE-DIRECTORY*), where the copy must leave 1 GB free, so a file
too large for the room there, or one claiming a huge size with nothing in it,
is not decoded either. A larger one is not decoded, and why is said.
- a size, 1 octet or more: the largest, as a whole number of octets or in
  words (\"20G\").
- nil: no limit but the room for the copy (the standard)."
  :values (or (member nil) (octets 1 *)))

(define-setting *sandbox-memory-copy-limit* (* 256 1024 1024)
  "The largest file whose copy for a program decoding it is kept in memory.
A larger file is copied into the cache directory (*CACHE-DIRECTORY*) instead,
so that a mailbox or PDF of many gigabytes needs room on the drive, not
memory: a file with no name only the editor can reach, read only, gone once
the programs are done with it, even after a crash. Set *CACHE-DIRECTORY* to
a directory encrypted by the kernel (LUKS, fscrypt) to keep such copies
there. Its file system must be ext4 (or ext2, ext3), XFS, btrfs, F2FS or
bcachefs, which hold files with no name (O_TMPFILE): elsewhere (tmpfs, FUSE,
SMB, NFS and the rest) a large file is not decoded, and that is said.
- a size, 0 octets or more: the largest kept in memory, as a whole number of
  octets or in words (\"512M\"); 256 MB as standard; 0 copies every file to
  the drive.
- nil: every copy in memory, whatever its size."
  :values (or (member nil) (octets 0 *)))

(define-setting *sandbox-total-memory* (* 2 1024 1024 1024)
  "The memory all the programs decoding one file may take together.
It holds where the system can hold them to it (systemd-run, a scope of the
user's own); each program is still held to *SANDBOX-MEMORY*.
- a size, 1 octet or more: a whole number of octets, or words such as \"4G\"; 2 GB
  as standard.
- nil: no such limit."
  :values (or (member nil) (octets 1 *)))

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

(sb-ext:defglobal **shut-in** nil
  "True in a process that runs shut in already (MARK-SHUT-IN).")

(sb-ext:defglobal **shut-in-files** (sb-thread:make-semaphore :name "files of programs shut in" :count 1)
  "Held by a program started in a process shut in already from the moment the
files it is given are written until they are taken away again, so that two
programs given files at the same paths never run at once.")

(sb-ext:defglobal **landlock** nil
  "Whether programs started in a process shut in already can be held to their
own files by Landlock and to their seccomp filter, once tried (TRY-LANDLOCK):
a list of setpriv's path and the file of that filter, or of NIL and why not,
in words; NIL before it is tried.")

(defun mark-shut-in ()
  "Tell the sandbox that this process runs shut in already, as a package's
function run by SANDBOX-CALL does, where no sandbox can be made inside. From
then on SANDBOX-STATUS says :READY when Landlock can hold a program there to
its own files (LANDLOCK-PROGRAM), :BROKEN when not, and SANDBOX-RUN and
SANDBOX-START start their programs in the same shut-in place, directly: with
nothing in their environment but what they are given, in a process group of
their own, held to their memory and processor time as in a sandbox of their
own, each reading only what SANDBOX-ARGUMENTS's DIRECT says and writing only
in a new directory of its own, its home. The files they are given are
written at their paths there, and all of it is taken away once they end,
with any program they left running (END-STRAYS). They make no new namespaces."
  (setf **shut-in** t))

(defparameter *landlock-reading* "execute,read-file,read-dir"
  "What a program held by Landlock may do in a directory it reads, as setpriv's
--landlock-rule names it.")

(defparameter *landlock-writing*
  (concatenate 'string "execute,read-file,read-dir,write-file,truncate,make-reg,make-dir,make-sym,make-fifo,"
               "make-sock,remove-file,remove-dir,refer")
  "What a program held by Landlock may do in a directory it writes in, as
setpriv's --landlock-rule names it.")

(defparameter *landlock-devices* '(("/dev/null" . t) ("/dev/zero" . t) ("/dev/full" . t)
                                   ("/dev/random") ("/dev/urandom"))
  "The devices a program held by Landlock may open, each (PATH . WRITABLE).")

(defun landlock-rule (path writing)
  "setpriv's arguments letting a program held by Landlock read PATH, a file or
a directory (all in it), or with WRITING write there too; NIL when nothing is
at PATH."
  (let ((kind (and path (path-kind path))))
    (when kind
      (list "--landlock-rule"
            (format nil "path-beneath:~a:~a"
                    (cond ((eq kind :directory) (if writing *landlock-writing* *landlock-reading*))
                          (writing "execute,read-file,write-file,truncate")
                          (t "execute,read-file"))
                    path)))))

(defun landlock-arguments (home &key filter files directories writable overlays)
  "setpriv's arguments holding the program it starts by Landlock to reading
the system's directories (/usr, /etc and those beside /usr), /proc, the
devices of *LANDLOCK-DEVICES*, FILES (PATH . CONTENTS) and DIRECTORIES and
the sources of OVERLAYS as SANDBOX-RUN has them, as this process sees them,
and to writing in HOME (NIL: nowhere) and WRITABLE; the files of anything
else are neither read nor written. FILTER, when given, is the file of a
seccomp filter it is put under besides (SECCOMP-PROGRAM's DIRECT)."
  (list* "--no-new-privs" "--landlock-access" "fs"
         (append (and filter (list "--seccomp-filter" filter))
         (loop for (place . writing)
                 in (append (and home (list (cons home t))) (list (list "/usr") (list "/etc") (list "/proc"))
                            (loop for directory in '("/bin" "/sbin" "/lib" "/lib64" "/lib32")
                                  when (eq :directory (path-kind directory :follow nil))
                                    collect (list directory))
                            *landlock-devices*
                            (loop for (path) in files collect (list (home-path path home)))
                            (loop for entry in directories collect (list (shut-in-place entry)))
                            (loop for entry in overlays collect (list (first entry)))
                            (loop for entry in writable collect (cons (shut-in-place entry) t)))
                 append (landlock-rule place writing)))))

(defun try-landlock ()
  "Find setpriv and try it holding a program by Landlock and the seccomp filter
of programs started directly (SECCOMP-PROGRAM's DIRECT), written once to a
new file in /tmp, as **LANDLOCK** keeps them; neither on a processor that
filter does not know."
  (let* ((setpriv (sandbox-find-program "setpriv"))
         (true (sandbox-find-program "true"))
         (octets (seccomp-program :direct t))
         (filter (and setpriv octets (format nil "/tmp/tuatara-filter-~a" (random-name)))))
    (when filter (write-new-file filter octets))
    (cond ((null setpriv)
           (list nil "setpriv (util-linux) is not there to hold each program to its own files (Landlock)"))
          ((null octets)
           (list nil (format nil "the system calls of programs started here cannot be held on this processor (~a)"
                             (machine-type))))
          ((and true (nth-value 1 (program-output (append (list setpriv)
                                                          (landlock-arguments nil :filter filter)
                                                          (list "--" true))
                                                  :capture nil :seconds 5 :environment '())))
           (list setpriv filter))
          (t (list nil (format nil "~a (~a) cannot hold a program to its own files: Landlock in the kernel ~
                                    and util-linux 2.40 or later are needed"
                               (string-trim '(#\Space #\Newline) (version-text setpriv)) setpriv))))))

(defun landlock-program ()
  "setpriv's path when it can hold a program by Landlock here, tried once, and
the file of the seccomp filter of programs started directly; NIL and
why not, in words, when it cannot."
  (let ((found (or **landlock**
                   (sb-thread:with-mutex (**sandbox-lock**)
                     (or **landlock** (setf **landlock** (try-landlock)))))))
    (values (first found) (second found))))

(defun landlock-filter ()
  "The file of the seccomp filter of programs started directly in a process
shut in already (LANDLOCK-PROGRAM), or NIL when they cannot be started."
  (landlock-program)
  (and (first **landlock**) (second **landlock**)))

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
  "What each bwrap program asked so far says of its version, by its path, trimmed.")

(defun bwrap-version-text (program)
  "What the bwrap PROGRAM (its path) says of its version, trimmed: asked once."
  (or (gethash program *bwrap-versions*)
      (setf (gethash program *bwrap-versions*)
            (string-trim '(#\Space #\Newline #\Return #\Tab) (version-text program)))))

(defvar *bwrap-descriptor-binds* (make-hash-table :test 'equal :synchronized t)
  "Whether each bwrap program asked so far shows a place given as a descriptor
\(--ro-bind-fd), by its path: T or :NO.")

(defun bwrap-binds-descriptors-p (program)
  "True when the bwrap PROGRAM (its path) shows a place given as a descriptor
\(--ro-bind-fd and --bind-fd), as its --help says: asked once. A sandbox is
never run without it, as a place given by name could be swapped after it
was checked."
  (eq t (or (gethash program *bwrap-descriptor-binds*)
            (setf (gethash program *bwrap-descriptor-binds*)
                  (if (search "--ro-bind-fd" (or (program-output (list program "--help") :seconds 5 :environment '())
                                                 ""))
                      t
                      :no)))))

(defun bwrap-version-at-least-p (major minor)
  "True when the bwrap found for the program SANDBOX-PROGRAM gives is version
MAJOR.MINOR or later, as it says it is."
  (let* ((program (and (sandbox-program) (find-program (sandbox-program))))
         (version (and program (software-version-numbers (bwrap-version-text program)))))
    (and version (software-version-at-least-p version (list major minor)))))

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

(defparameter *seccomp-guarding*
  '((:x86-64
     ;; chmod fchmod fchmodat fchmodat2 setxattr lsetxattr fsetxattr setxattrat
     90 91 268 452 188 189 190 463)
    (:arm64
     ;; fchmod fchmodat fchmodat2 setxattr lsetxattr fsetxattr setxattrat
     52 53 452 5 6 7 463))
  "For each processor, the system calls also refused to a program given a copy
on the drive (DISK-COPY), by number: those that change a file's permissions,
directly or through its access list (system.posix_acl_access), and so could
make the read-only copy writable to its owner, which the program runs as.
Without them, opening it again to write is refused by its permissions.")

(defparameter *seccomp-direct*
  '((:x86-64
     ;; setsid setpgid
     112 109
     ;; chmod fchmod fchmodat fchmodat2 chown fchown lchown fchownat
     90 91 268 452 92 93 94 260
     ;; utime utimes utimensat futimesat
     132 235 280 261
     ;; setxattr lsetxattr fsetxattr setxattrat removexattr lremovexattr fremovexattr removexattrat
     188 189 190 463 197 198 199 466)
    (:arm64
     157 154
     52 53 452 55 54
     88
     5 6 7 463 14 15 16 466))
  "For each processor, the system calls also refused to a program started
directly in a process shut in already (MARK-SHUT-IN), by number: leaving its
process group or session, so that ending the group ends all it started, and
changing a file's permissions, owner, times or extended attributes, which
Landlock does not hold, so that it cannot lock the next program out of the
directories it needs.")

(defun seccomp-cpu ()
  "The entry of *SECCOMP-REFUSED* for the processor the editor runs on, or NIL
for one the seccomp filter does not know."
  (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*))

(defparameter *seccomp-calls*
  '((:x86-64 :clone 56 :clone3 435 :ioctl 16 :socket 41)
    (:arm64 :clone 220 :clone3 435 :ioctl 29 :socket 198))
  "For each processor, the numbers of the system calls the seccomp filter
looks into the arguments of.")

(defun seccomp-program (&key guarded direct)
  "The seccomp filter of the sandbox as the octets of a classic BPF program,
as bwrap's --seccomp reads it, for the processor here. It is NIL on one it
does not know. Another processor's calls (32-bit ones on x86-64) end the
program. The calls of *SECCOMP-REFUSED* fail with EPERM, with GUARDED those
of *SECCOMP-GUARDING* too, and with DIRECT those of *SECCOMP-DIRECT*. clone3 fails with
ENOSYS (so that its flags, out of reach of the filter, are asked through
clone). clone making a namespace fails too, and so do ioctl pushing input into
a terminal (TIOCSTI, TIOCLINUX) and a socket that is not a local one
\(AF_UNIX). Everything else is let be."
  (let* ((cpu (seccomp-cpu))
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
          (dolist (number (remove-duplicates
                           (append (cddr cpu) (and guarded (rest (assoc (first cpu) *seccomp-guarding*)))
                                   (and direct (rest (assoc (first cpu) *seccomp-direct*))))))
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
  (and *sandbox-seccomp* (seccomp-cpu) t))

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
      (when (minusp (%fcntl fd 1033 +all-seals+))  ; F_ADD_SEALS
        (fail "seal" "a file in memory"))
      (sb-posix:lseek fd 0 sb-posix:seek-set)
      (if kept (fd-for-program fd) fd))))

(defun memory-file (name octets &key (kept t))
  "A descriptor of a new file in memory holding OCTETS, at its start, sealed
so that nothing can change it (SEALED-MEMORY-FILE), and with KEPT kept open
in programs started (for bwrap to read)."
  (sealed-memory-file name (lambda (fd) (write-octets fd octets :path name)) :kept kept))

(defun call-with-memory-file (name octets function &key kept)
  "Call FUNCTION with the descriptor of a new file in memory called NAME
holding OCTETS, at its start and sealed so that nothing can change it (as
MEMORY-FILE makes it, KEPT, NIL unless given, as it has it), and close the
descriptor when FUNCTION is done, whether it returns or leaves with an error.
Returns what FUNCTION returns. The file is gone once nothing holds it open."
  (let ((fd (memory-file name octets :kept kept)))
    (unwind-protect (funcall function fd)
      (tuatara.runtime:close-fd-quietly fd "the file in memory ~a" name))))

(defun sealed-p (fd &optional (seals +all-seals+))
  "True when file descriptor (FD) has every seal of SEALS (F_GET_SEALS): as
standard, sealed as SEALED-MEMORY-FILE seals a copy."
  (let ((have (%fcntl fd 1034 0)))  ; F_GET_SEALS
    (and (>= have 0) (= seals (logand have seals)))))

(defun sealed-file-size (fd)
  "The size in octets of the file open as file descriptor (FD) when it is
sealed so that nothing can write it, make it shorter or make it longer
\(+UNCHANGING-SEALS+); NIL when it is not."
  (and (sealed-p fd +unchanging-seals+) (fd-size fd)))

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

(defun place-unshown-why (place)
  "Why PLACE, a file or directory, may not be shown to a program shut in, in
words, or NIL when it may. It is looked at where it really is, its links
followed (REAL-PATH-SO-FAR). Refused: a directory the sandbox makes its own
\(*SANDBOX-OWN-DIRECTORIES*), whose real one holds the sockets of programs
outside it (the X server, ssh-agent, tmux); the directory of your own
programs' sockets (SESSION-SOCKET-DIRECTORIES) or one holding it, as a
program reaching such a socket could have a program outside the sandbox act
for it; your home directory or one holding it, where your private keys and
more sockets are; the root, which holds all of them."
  (let ((place (string-right-trim "/" (real-path-so-far place))))
    (cond ((zerop (length place)) "it is the root, which holds every file")
          ((or (member place *sandbox-own-directories* :test #'string=)
               (some (lambda (sockets) (path-within-p sockets place)) (session-socket-directories)))
           "programs outside the sandbox are reached through it")
          ((some (lambda (home) (path-within-p (real-place home) place)) (home-directory-names))
           "it is your home directory, or holds it"))))

(defun shown-place-p (place)
  "True when PLACE may be shown to a program shut in (PLACE-UNSHOWN-WHY)."
  (not (place-unshown-why place)))

(defun check-shown-place (place)
  "Signal a FILE-SYSTEM-ERROR, saying why, when PLACE may not be shown to a
program shut in (PLACE-UNSHOWN-WHY). Show the files it needs instead."
  (let ((why (place-unshown-why place)))
    (when why
      (error 'file-system-error :operation "show to a program shut in" :path (string-right-trim "/" place)
                                :reason why))))

(defun open-shown-place (source)
  "A descriptor naming SOURCE, a file or directory where it really is, as it
was checked (CHECK-SHOWN-PLACE), for bwrap to show in place of its name, so
that nothing swapped in after the check is shown: NIL when nothing is there
\(a bind of what may be missing is then left out). A FILE-SYSTEM-ERROR when a
link stands at SOURCE now, or the descriptor names another place than
SOURCE, a part of its path swapped for a link meanwhile."
  (let ((fd (handler-case (fd-for-program (sb-posix:open source (logior +o-path+ +o-nofollow+)))
              (sb-posix:syscall-error (condition)
                (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
                    (return-from open-shown-place nil)
                    (fail "show to a program shut in" source (sb-posix:syscall-errno condition)))))))
    (let ((link (= (logand (sb-posix:stat-mode (sb-posix:fstat fd)) sb-posix:s-ifmt) sb-posix:s-iflnk))
          (there (handler-case (sb-posix:readlink (format nil "/proc/self/fd/~d" fd))
                   (sb-posix:syscall-error () nil))))
      (when (or link (not (equal there source)))
        (tuatara.runtime:close-fd-quietly fd "~a" source)
        (error 'file-system-error :operation "show to a program shut in" :path source
                                  :reason "it changed while it was looked at"))
      fd)))

(defun mount-arguments (directories writable overlays &key opened)
  "bwrap's arguments showing DIRECTORIES read only, WRITABLE ones to write in
and OVERLAYS, as SANDBOX-RUN has them, those that exist, a directory in
another shown after it, so that both are seen; with the links at the root
their paths go through (ROOT-LINK-ARGUMENTS). An entry may be a single file,
shown alone. A directory a link leads to is shown where it is written too, so
that a path through the link finds it. The root itself is never shown: the
sandbox's root is its own. Each is checked and shown where it really is, its
links followed, under a directory the sandbox makes its own too: a place
outside programs are reached through is refused (CHECK-SHOWN-PLACE), and so
is a directory to write in that is, or holds, a place nothing may be written
in (CHECK-TREE-WRITING). With OPENED, a cons, each directory or file shown is
given to bwrap as a descriptor opened where it was checked (OPEN-SHOWN-PLACE),
pushed on OPENED's car for the caller to hand over and close; overlays are
given by name. An entry of DIRECTORIES (FD . TARGET), FD a descriptor open on
a file or directory, shows that very file at TARGET, by a copy of FD pushed
on OPENED's car, checked where it really is (FD-REAL-PATH)."
  (let ((mounts '()) (written '()))
    (flet ((add-open (fd target)
             (let ((source (or (fd-real-path fd)
                               (error 'file-system-error :operation "show to a program shut in" :path target
                                                         :reason "it has no name any more")))
                   (place (real-place target)))
               (check-shown-place source)
               (unless opened
                 (error 'file-system-error :operation "show to a program shut in" :path target
                                           :reason "a file given open is shown only by its descriptor"))
               (let ((given (fd-for-program (sb-posix:dup fd))))
                 (push given (car opened))
                 (push target written)
                 (push (cons place (list "--ro-bind-fd" (princ-to-string given) place)) mounts))))
           (add (entry how)
             (multiple-value-bind (as-written target) (sandbox-place entry)
               ;; bwrap follows the links in what it shows, under /tmp too: checked and shown where it really is
               (let ((source (real-path-so-far as-written)))
                 ;; the root is never shown
                 (when (zerop (length (string-right-trim "/" source))) (return-from add))
                 (check-shown-place source)
                 (when (eq how :write)
                   (check-tree-writing source "let a program write in"))
                 (push (if (consp entry) (cdr entry) entry) written)
                 ;; at its real place, and where it is written when a link leads there (~/scratch to /tmp/scratch)
                 (dolist (place (remove-duplicates (list target (if (consp entry) target (root-resolved entry)))
                                                   :test #'string=))
                   (if opened
                       (let ((fd (open-shown-place source)))
                         (when fd
                           (push fd (car opened))
                           (push (cons place (list (if (eq how :read) "--ro-bind-fd" "--bind-fd")
                                                   (princ-to-string fd) place))
                                 mounts)))
                       (push (cons place (ecase how
                                           (:read (list "--ro-bind-try" source place))
                                           (:write (list "--bind-try" source place))))
                             mounts)))))))
      (dolist (entry directories)
        (if (and (consp entry) (integerp (car entry)))
            (add-open (car entry) (cdr entry))
            (add entry :read)))
      (dolist (entry writable) (add entry :write))
      (loop for (source upper work) in overlays
            do (multiple-value-bind (as-written target) (sandbox-place source)
                 (let ((lower (real-path-so-far as-written)))
                   (check-shown-place lower)
                   (push source written)
                   (push (cons target (if (bwrap-version-at-least-p 0 11)
                                          (list "--overlay-src" lower "--overlay" upper work target)
                                          (list "--ro-bind-try" lower target)))
                         mounts)))))
    ;; the last given for a place wins; a parent before what is in it; the root itself never
    (setf mounts (stable-sort (remove-duplicates (remove "" (nreverse mounts) :key #'car :test #'string=)
                                                 :key #'car :test #'string= :from-end nil)
                              #'< :key (lambda (mount) (length (car mount)))))
    (append (root-link-arguments (reverse written))
            (loop for mount in mounts append (cdr mount)))))

(defun home-path (path home)
  "PATH, where a program is given a file (SANDBOX-RUN's FILES), with a ~/ at
its start standing for HOME, the program's home directory."
  (if (uiop:string-prefix-p "~/" path)
      (concatenate 'string (string-right-trim "/" home) (subseq path 1))
      path))

(defun shut-in-place (entry)
  "Where ENTRY of SANDBOX-RUN's DIRECTORIES or WRITABLE is in this process, as
a program started directly here finds it: its source, and for a descriptor
the file it is open on (FD-REAL-PATH); NIL when that has no name."
  (let ((source (sandbox-place entry)))
    (if (integerp source) (fd-real-path source) source)))

(defun sandbox-arguments (program arguments seconds memory &key seccomp files environment directories writable overlays
                                                              links (directory "/tmp") (file-limit *sandbox-file-limit*)
                                                              (processes *sandbox-process-limit*) (scratch t)
                                                              (descriptors nil) opened direct)
  "Bwrap's arguments for running PROGRAM with ARGUMENTS shut in. Each process
is limited to SECONDS of processor time (NIL: no limit) and MEMORY octets (NIL:
no limit). Its scratch /tmp is limited to MEMORY octets too. It is limited to
PROCESSES processes and threads, and to files of FILE-LIMIT octets (NIL: no
limit). SECCOMP is the file descriptor (FD) of its seccomp filter. FILES is
the descriptors of files it is shown read only, each (PATH . FD). ENVIRONMENT
is what it has in its environment besides, each (NAME VALUE), a later one for
the same NAME in place of the earlier. DIRECTORIES, WRITABLE and OVERLAYS are
the directories it is shown as SANDBOX-RUN says, LINKS the links it has.
DIRECTORY is the one it starts in. Without SCRATCH it has no /tmp at all.
DESCRIPTORS, when given, is the most files it may have open at once. OPENED is
MOUNT-ARGUMENTS's.
With DIRECT, the new directory of its own (ending in /) for a program
started in a process shut in already (MARK-SHUT-IN), three values in place of
bwrap's arguments: the command that runs PROGRAM held to the same limits, its
whole environment as NAME=VALUE strings, and the directory it starts in. That
directory is its home and its place for scratch files (HOME, TMPDIR), and
where it starts when DIRECTORY is /tmp. The command holds it by Landlock to
reading the system's directories, FILES once written, DIRECTORIES and
OVERLAYS, and to writing in that directory and WRITABLE alone
\(LANDLOCK-ARGUMENTS); nothing is shown or made for it then."
  (let ((command (append (list "/bin/sh" "-c"
                               ;; a number past the hard limit, or none, is held to it; any other failure stops the run
                               (concatenate 'string "hold() { h=$(ulimit \"$1\" -H) || return; case $2 in "
                                            "unlimited) [ \"$h\" = unlimited ] || set -- \"$1\" \"$h\";; "
                                            "''|*[!0-9]*) ;; "
                                            "*) [ \"$h\" = unlimited ] || [ \"$2\" -le \"$h\" ] || "
                                            "set -- \"$1\" \"$h\";; "
                                            "esac; ulimit \"$1\" \"$2\"; } && "
                                            "ulimit -c 0 && hold -v \"$1\" && hold -t \"$2\" && hold -f \"$3\" && "
                                            "{ hold -u \"$4\" 2>/dev/null || hold -p \"$4\"; } && "
                                            "{ [ \"$5\" = as-is ] || ulimit -n \"$5\"; } && shift 5 && exec \"$@\"")
                               "sh"
                               (if memory (princ-to-string (ceiling memory 1024)) "unlimited")
                               (if seconds (princ-to-string (max 1 (ceiling seconds))) "unlimited")
                               (if file-limit (princ-to-string (ceiling file-limit 512)) "unlimited")
                               (princ-to-string processes)
                               (if descriptors (princ-to-string descriptors) "as-is")
                               program)
                         arguments))
        ;; the last given for a name wins
        (environment (remove-duplicates (append (list (list "PATH" *sandbox-path*) (list "HOME" "/tmp")
                                                      (list "LC_ALL" "C.UTF-8"))
                                                environment
                                                (and direct (list (list "HOME" (string-right-trim "/" direct))
                                                                  (list "TMPDIR" (string-right-trim "/" direct)))))
                                        :key #'first :test #'string=)))
    (if direct
        (values (append (list (landlock-program))
                        (landlock-arguments direct :filter (landlock-filter) :files files :directories directories
                                                   :writable writable :overlays overlays)
                        (list "--")
                        command)
                (loop for (name value) in environment collect (format nil "~a=~a" name value))
                (if (string= directory "/tmp") direct directory))
        (append (list "--unshare-all" "--unshare-user" "--disable-userns" "--die-with-parent" "--new-session"
                      "--cap-drop" "ALL" "--clearenv")
                (loop for (name value) in environment append (list "--setenv" name value))
                (system-directory-arguments)
                (list "--proc" "/proc" "--dev" "/dev")
                ;; the scratch /tmp is memory too: held to the memory a program may take, where bwrap can (0.10 on)
                (and scratch memory (bwrap-version-at-least-p 0 10) (list "--size" (princ-to-string memory)))
                (and scratch (list "--tmpfs" "/tmp"))
                (mount-arguments directories writable overlays :opened opened)
                (loop for (path . text) in links
                      append (list "--symlink" text
                                   (path-join (real-place (parent-path path)) (native-file-name path))))
                (loop with home = (second (assoc "HOME" environment :test #'string=))
                      for (path . fd) in files
                      append (list "--ro-bind-data" (princ-to-string fd) (home-path path home)))
                (and seccomp (list "--seccomp" (princ-to-string seccomp)))
                (list "--chdir" directory "--")
                command))))

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
started. The function given as ENDED to SANDBOX-START is called once, after
PROCESS is signalled, whoever ends it. What was made for a program started
in a process shut in already, and any program it left running, are taken
away once it has ended (REMOVE-SHUT-IN-RUN)."
  (let ((unit (getf (sb-ext:process-plist process) :scope))
        (starting nil))
    (when (process-unreaped-p process)
      (let ((pid (sb-ext:process-pid process)))
        (setf starting (and unit (string= "systemd-run" (process-name pid))))
        (dolist (child (process-children process))
          (signal-process-if child (lambda () (eql pid (process-parent child))) sb-unix:sigkill))
        (tuatara.runtime:ignoring-errors-reported ("ending ~a" process)
          (sb-ext:process-kill process sb-unix:sigkill :process-group))))
    (multiple-value-bind (ended written)
        (sb-thread:with-mutex (**sandboxes-lock**)
          (setf **sandboxes** (remove process **sandboxes** :key #'car))
          (let ((plist (sb-ext:process-plist process)))
            (multiple-value-prog1 (values (getf plist :ended) (getf plist :written))
              (remf (sb-ext:process-plist process) :ended)
              (remf (sb-ext:process-plist process) :written))))
      (when ended
        (tuatara.runtime:ignoring-errors-reported ("saying ~a was ended" process) (funcall ended)))
      (flet ((reap ()
               (tuatara.runtime:ignoring-errors-reported ("waiting for ~a to end" process)
                 (sb-ext:process-wait process))
               (tuatara.runtime:ignoring-errors-reported ("closing ~a" process) (sb-ext:process-close process))
               (when starting (stop-scopes (list unit)))
               (when written (remove-shut-in-run written))))
        (if wait
            (reap)
            (tuatara.runtime:make-guarded-thread #'reap :name "sandbox ending"))))))

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

(defun end-strays ()
  "In a process shut in already as SANDBOX-CALL starts one, the first of its
own namespace of processes after bwrap, end every process there but bwrap,
this one and those this one started and has not seen end: a program started
here that left its process group, and so outlived its end (END-SANDBOX).
Each is stopped (SIGSTOP), again until no new one turns up, so that one
starting others cannot outrun it, and then killed (SIGKILL), whatever its
state: a process whose first thread has ended shows as ended while its other
threads run. They are waited for, a second at most, until none runs. How many
were killed. Elsewhere nothing is ended, and the debug
log says why."
  (let ((self (sb-posix:getpid)))
    (if (and (eql 1 (process-parent self)) (string= "bwrap" (process-name 1)))
        (labels ((stray-p (pid)
                   (and (/= pid 1) (/= pid self)
                        ;; its parents up to bwrap, this process not among them
                        (loop for at = pid then (process-parent at)
                              repeat 4096
                              do (cond ((eql at self) (return nil))
                                       ((member at '(nil 0 1)) (return t))))))
                 (strays () (remove-if-not #'stray-p (process-numbers)))
                 (running-p (pid)
                   (or (not (member (first (process-stat pid)) '(nil "Z") :test #'equal))
                       (rest (handler-case (directory-entries (format nil "/proc/~d/task/" pid))
                               (file-system-error () nil))))))
          (let ((stopped '()))
            (loop repeat 64
                  for strays = (strays)
                  until (subsetp strays stopped)
                  do (dolist (pid strays)
                       (signal-process-if pid (lambda () (stray-p pid)) sb-unix:sigstop))
                     (setf stopped (union stopped strays)))
            (let ((killed (remove-if-not (lambda (pid)
                                           (signal-process-if pid (lambda () (stray-p pid)) sb-unix:sigkill))
                                         (union stopped (strays)))))
              ;; gone, but for what bwrap has still to wait for, within a second
              (tuatara.runtime:wait-until (lambda () (notany #'running-p killed)) 1)
              (length killed))))
        (progn
          (tuatara.runtime:debug-report :job "sandbox: programs left running not looked for: this process is ~
                                              not the first bwrap started")
          0))))

(sb-ext:defglobal **shut-in-writable** '()
  "The directories programs started in a process shut in already, running,
were given to write in (MAKE-SHUT-IN-RUN's WRITABLE), each ending in /.")

(sb-ext:defglobal **shut-in-writable-lock** (sb-thread:make-mutex :name "directories written shut in")
  "Held while **SHUT-IN-WRITABLE** changes.")

(defun claim-shut-in-writable (writable)
  "Take the directories of WRITABLE, as SANDBOX-RUN has them, for one program
started in a process shut in already, as their paths ending in /: each must
be an empty directory, neither within nor around one another program running
there was given (**SHUT-IN-WRITABLE**), so that what one program writes is
never read, changed or written over by another. A FILE-SYSTEM-ERROR saying
why, nothing taken, when one is not."
  (let ((places (loop for entry in writable
                      for place = (shut-in-place entry)
                      collect (if place (slash-ended place) entry))))
    (dolist (place places)
      (unless (and (stringp place) (eq :directory (path-kind place :follow nil)))
        (error 'file-system-error :operation "write" :path (princ-to-string place)
                                  :reason "a program started here writes only in a directory of its own"))
      (when (directory-entries place)
        (error 'file-system-error :operation "write" :path place
                                  :reason "a program started here writes only in an empty directory of its own")))
    (sb-thread:with-mutex (**shut-in-writable-lock**)
      (dolist (place places)
        (when (find-if (lambda (taken) (or (uiop:string-prefix-p taken place) (uiop:string-prefix-p place taken)))
                       **shut-in-writable**)
          (error 'file-system-error :operation "write" :path place
                                    :reason "another program running here writes there")))
      (setf **shut-in-writable** (append places **shut-in-writable**)))
    places))

(defun make-shut-in-run (files writable)
  "Make what a program started in a process shut in already needs before it
starts: a new directory of its own in /tmp, its home, for its owner alone,
and FILES, each (PATH . TEXT) or (PATH . FD) as SANDBOX-RUN has them, at
their paths, a ~/ standing for that home (HOME-PATH). Each is a new file only
its owner may read, the directories it needs that are not there made for it,
for its owner alone. A directory it needs that is there already is refused,
unless it is the new home or one the sandbox makes its own (/etc, /tmp:
*SANDBOX-OWN-DIRECTORIES*), so that nothing left at a path decides what is
read there. The directories of WRITABLE are taken for it alone
\(CLAIM-SHUT-IN-WRITABLE). What was made, as REMOVE-SHUT-IN-RUN takes it:
\(HOME HELD CLAIMED . MADE), HELD true when **SHUT-IN-FILES** is held while
FILES are there, CLAIMED the directories taken, MADE the outermost paths
made, the last first. A FILE-SYSTEM-ERROR, nothing left made or taken, when
something is at a path already or a directory of WRITABLE cannot be taken."
  (let ((home (format nil "/tmp/tuatara-run-~a/" (random-name)))
        (held nil) (claimed '()) (made '()) (done nil))
    (unwind-protect
         (progn
           (setf claimed (claim-shut-in-writable writable))
           (make-directory home #o700)
           (when files
             (sb-thread:wait-on-semaphore **shut-in-files**)
             (setf held t)
             (let ((bases (cons (string-right-trim "/" home) *sandbox-own-directories*)))
               (loop for (given . contents) in files
                     for path = (home-path given home)
                     for needed = (loop for parent = (parent-path path) then (parent-path parent)
                                        until (path-kind parent :follow nil)
                                        collect (string-right-trim "/" parent))
                     for there = (string-right-trim "/" (parent-path (or (car (last needed)) path)))
                     do (unless (member there bases :test #'string=)
                          (error 'file-system-error :operation "write" :path path
                                                    :reason (format nil "~a is there already" there)))
                        (when needed (push (car (last needed)) made))
                        (dolist (directory (reverse needed))
                          (make-directory directory #o700)
                          (push directory bases))
                        (unless needed (push path made))
                        (write-new-file path (if (stringp contents)
                                                 contents
                                                 (let ((copy (sealed-for-program contents :in-memory t)))
                                                   (unwind-protect (fd-octets copy path (fd-size copy))
                                                     (close-sealed-copy copy))))))))
           (setf done t)
           (list* home held claimed made))
      (unless done (remove-shut-in-run (list* home held claimed made))))))

(defun remove-shut-in-run (run)
  "Take away what MAKE-SHUT-IN-RUN made, RUN, once its program has ended, any
program it left running ended first (END-STRAYS): each path made, a directory
with all in it, links in it not followed, and its home; then let the next
program given files be started, and its directories of WRITABLE be given to
another."
  (destructuring-bind (home held claimed . made) run
    (unwind-protect
         (progn
           (end-strays)
           (dolist (path (append made (list home)))
             (when (path-kind path :follow nil)
               (handler-case (delete-tree path)
                 (file-system-error (e)
                   (tuatara.runtime:debug-report :job "sandbox: ~a left where a program shut in ran: ~a" path e))))))
      (sb-thread:with-mutex (**shut-in-writable-lock**)
        (setf **shut-in-writable** (set-difference **shut-in-writable** claimed :test #'string=)))
      (when held (sb-thread:signal-semaphore **shut-in-files**)))))

(defun start-sandbox (bwrap program arguments stdin seconds memory
                      &rest keys &key files (total *sandbox-total-memory*) (outputs :stream) started guarded
                      &allow-other-keys)
  "Start bwrap at the path BWRAP on PROGRAM with ARGUMENTS, STDIN its input,
shown FILES as SANDBOX-RUN has them, KEYS the rest of SANDBOX-ARGUMENTS's;
with GUARDED, STDIN being a copy on the drive, under the seccomp filter that
also keeps it from being made writable (SECCOMP-PROGRAM);
the process, among the sandboxes running until it is ended (END-SANDBOX). In
a scope whose memory is held to TOTAL (*SANDBOX-TOTAL-MEMORY* unless given)
when systemd-run can make one. Its outputs are streams to read, or let go
when OUTPUTS is NIL. STARTED, when given, is called with the process as soon
as it is counted, before anything can stop this thread, to keep it where the
caller ends it.
With BWRAP NIL, in a process shut in already (MARK-SHUT-IN), PROGRAM is
started there directly (SANDBOX-ARGUMENTS's DIRECT), in a directory of its
own with FILES written at their paths (MAKE-SHUT-IN-RUN), all taken away once
it is ended. A copy on the
drive is refused there with a FILE-SYSTEM-ERROR: the filter guarding it
cannot be added."
  (let ((seccomp nil) (data '()) (opened (list nil)) (written nil) (handed nil))
    (unwind-protect
         (multiple-value-bind (command environment directory keep)
             (let ((keys (alexandria:remove-from-plist keys :files :total :outputs :started :guarded)))
               (cond (bwrap
                      (let ((program-octets (and *sandbox-seccomp* (seccomp-program :guarded guarded))))
                        (when program-octets (setf seccomp (memory-file "tuatara-seccomp" program-octets)))
                        (loop for (path . contents) in files
                              do (push (cons path (if (stringp contents)
                                                      (memory-file "tuatara-sandbox-file"
                                                                   (tuatara.text:string-octets contents))
                                                      ;; bwrap copies it into memory of its own
                                                      (sealed-for-program contents :in-memory t)))
                                       data))
                        (values (cons bwrap (apply #'sandbox-arguments program arguments seconds memory
                                                   :seccomp seccomp :files (reverse data) :opened opened keys))
                                (list (sandbox-mark))
                                nil
                                (remove nil (append (list seccomp) (mapcar #'cdr data) (car opened))))))
                     (guarded
                      (error 'file-system-error :operation "decode" :path "a file"
                                                :reason "its copy on the drive cannot be given to a program here"))
                     (t
                      (setf written (make-shut-in-run files (getf keys :writable)))
                      (apply #'sandbox-arguments program arguments seconds memory
                             :direct (first written) :files files keys))))
           (let* ((scope (and bwrap total **sandbox-scope**))
                  (unit (and scope (scope-name))))
             ;; counted among those running before this thread can be ended
             (sb-sys:without-interrupts
               (let ((process
                       ;; systemd-run moves itself into the scope and becomes bwrap: still this one's child
                       (start-program (if scope (append (list scope) (scope-arguments total unit) command) command)
                                      :input stdin :preserve-fds keep :output outputs :error outputs
                                      :external-format :default :directory directory
                                      :environment (if scope (append environment (scope-environment)) environment))))
                 (when unit (setf (getf (sb-ext:process-plist process) :scope) unit))
                 (when written (setf (getf (sb-ext:process-plist process) :written) written handed t))
                 (sb-thread:with-mutex (**sandboxes-lock**)
                   (push (cons process sb-thread:*current-thread*) **sandboxes**))
                 (when started (funcall started process))
                 (when **sandboxes-closed**
                   (end-sandbox process))
                 process))))
      ;; bwrap has its own descriptors of them once started
      (when seccomp (tuatara.runtime:close-fd-quietly seccomp "the sandbox's system call filter"))
      (dolist (file data) (tuatara.runtime:close-fd-quietly (cdr file) "~a" (car file)))
      (dolist (fd (car opened)) (tuatara.runtime:close-fd-quietly fd "a place shown to a program shut in"))
      (when (and written (not handed)) (remove-shut-in-run written)))))

(defun sandbox-start (program arguments stdin
                      &key memory directories (processes *sandbox-process-limit*) descriptors ended)
  "Start PROGRAM (a full path to one it is shown) with ARGUMENTS shut in as
SANDBOX-RUN shuts in a program, left running, with no limit on the processor
time it takes, no /tmp, no file it may write into (writing one ends it) and
its outputs let go; STDIN, a stream on a descriptor, its standard input. It
may take MEMORY octets (NIL: no limit), held to them together with all it
starts where systemd-run can, have PROCESSES processes and threads and
DESCRIPTORS files open (NIL: as it is); DIRECTORIES are directories it is
shown, read only, as SANDBOX-RUN has them. The process, or NIL when there is
no sandbox here (SANDBOX-STATUS). Ending it ends all it started. ENDED, a
function of no arguments, is called once when it is ended (END-SANDBOX), by
its caller or by another, as the thread that started it ends
\(END-SANDBOXES), so that what stands for it there is let go too."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (declare (ignore detail))
    (when (eq status :ready)
      (start-sandbox bwrap program arguments stdin nil memory
                     :total memory :outputs nil :directories directories :processes processes
                     :descriptors descriptors :scratch nil :directory "/" :file-limit 0
                     :started (and ended
                                   (lambda (process) (setf (getf (sb-ext:process-plist process) :ended) ended)))))))

(defun sandbox-memory-scope-p ()
  "True when what is started shut in is held, with all it starts, to its
memory in a scope of its own (systemd-run); without one, each process is
held only to its own address space."
  (and (eq :ready (sandbox-status)) **sandbox-scope** t))

(defun %sandbox-run (bwrap program arguments input seconds memory output-limit cancel ended-well kept
                     &rest keys &key on-output watch &allow-other-keys)
  "Run PROGRAM with ARGUMENTS in the sandbox bwrap at the path BWRAP makes,
KEYS those of SANDBOX-ARGUMENTS but SECCOMP, FILES as SANDBOX-RUN has them; it
ends well with an exit code among ENDED-WELL; what it prints is KEPT, or only
counted, each piece given to ON-OUTPUT as it comes; WATCH may end it. See
SANDBOX-RUN."
  (let ((keys (alexandria:remove-from-plist keys :on-output :watch))
        (stdin (and input (sb-sys:make-fd-stream input :input t :element-type '(unsigned-byte 8)
                                                        :buffering :none :auto-close nil)))
        (process nil) (outcome nil) (output nil) (said nil)
        (deadline (and seconds (tuatara.runtime:deadline-in seconds))))
    (unwind-protect
         (let ((started (apply #'start-sandbox bwrap program arguments stdin seconds memory
                               :started (lambda (started) (setf process started)) keys)))
           ;; what it prints, until both its outputs end
           (multiple-value-bind (outputs over late cancelled watched)
               (read-outputs (list (sb-sys:fd-stream-fd (sb-ext:process-output started))
                                   (sb-sys:fd-stream-fd (sb-ext:process-error started)))
                             (list output-limit *sandbox-said-limit*) (list nil t)
                             :kept (list (and output-limit kept) t) :deadline deadline :cancel cancel
                             :too-much (lambda () (setf outcome :too-much) :stop) :watch watch
                             :each (and on-output (lambda (index octets count)
                                                    (when (zerop index) (funcall on-output octets count)))))
             (declare (ignore over))
             (setf output (first outputs) said (second outputs))
             (cond (outcome)
                   (watched (setf outcome watched))
                   (cancelled (setf outcome :cancelled))
                   (late (setf outcome :timeout))))
           ;; its end: it may have shut its outputs and gone on
           (loop while (and (null outcome) (sb-ext:process-alive-p started))
                 do (cond ((and cancel (car cancel)) (setf outcome :cancelled))
                          ((and deadline (tuatara.runtime:deadline-passed-p deadline)) (setf outcome :timeout))
                          ((and watch (setf outcome (funcall watch))))
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
             (let ((version (bwrap-version-text path)))
               (unless (bwrap-binds-descriptors-p path)
                 (return-from try-sandbox
                   (list :broken (format nil "~a (~a) cannot be given the places it shows as descriptors ~
                                              (--ro-bind-fd): a later bubblewrap is needed"
                                         (if (plusp (length version)) version program) path)
                         path)))
               (multiple-value-bind (output outcome said)
                   (tuatara.runtime:handling-errors-reported ("trying the sandbox ~a" path)
                       (%sandbox-run path "true" '() nil 10 (* 64 1024 1024) 0 nil '(0) t)
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
never in a directory your PATH may name. NIL when there is none, as
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
first time: call it from a thread doing work in the background. In a process
shut in already (MARK-SHUT-IN), with no path: :READY when Landlock can hold
each program there to its own files (LANDLOCK-PROGRAM), :BROKEN and why
when it cannot."
  (if **shut-in**
      (multiple-value-bind (setpriv why) (landlock-program)
        (if setpriv
            (values :ready (format nil "shut in already: programs run in the same place, each held to its own ~
                                        limits and its own files (~a)" setpriv)
                    nil)
            (values :broken why nil)))
      (let* ((program (sandbox-program))
             (state (sb-thread:with-mutex (**sandbox-lock**)
                      (when (or again (null **sandbox-state**) (not (equal program (fourth **sandbox-state**))))
                        (setf **sandbox-state** (append (try-sandbox program) (list program))))
                      **sandbox-state**)))
        (values (first state) (second state) (third state)))))

(defun memory-copy-p (size)
  "True when the copy of a file of SIZE octets for a program decoding it is
kept in memory (*SANDBOX-MEMORY-COPY-LIMIT*), not in the cache directory."
  (or (null *sandbox-memory-copy-limit*) (<= size *sandbox-memory-copy-limit*)))

(sb-ext:defglobal **copies-pending** (list 0)
  "A cons whose car is the octets of copies being made in the cache directory
now (DISK-COPY), counted against its free room so that copies made at the same
time do not each count the same room. Changed under **COPIES-PENDING-LOCK**.")

(sb-ext:defglobal **copies-pending-lock** (sb-thread:make-mutex :name "copies pending")
  "Held while the room for a copy is looked at and **COPIES-PENDING** changed.")

(defun reserve-copy-room (size directory)
  "Count SIZE octets as being copied into DIRECTORY, the cache directory, when
it has room for them (COPY-ROOM-REFUSAL), the copies being made counted;
signal a FILE-SYSTEM-ERROR saying why when it has not. RELEASE-COPY-ROOM
gives the room back."
  (sb-thread:with-mutex (**copies-pending-lock**)
    (let ((refusal (copy-room-refusal size directory)))
      (when refusal
        (error 'file-system-error :operation "decode" :path "a file" :reason refusal))
      (incf (car **copies-pending**) size))))

(defun release-copy-room (size)
  "Give back the room RESERVE-COPY-ROOM counted for a copy of SIZE octets."
  (sb-thread:with-mutex (**copies-pending-lock**)
    (decf (car **copies-pending**) size)))

(defun copy-room-refusal (size directory)
  "Why a copy of SIZE octets cannot be made in DIRECTORY, the cache directory,
in words saying how much is needed, where, and the setting that moves it, or
NIL when the drive has room for it, the copies being made
\(**COPIES-PENDING**) and *SNAPSHOT-FREE-MARGIN* more, or does not say.
While the directory *CACHE-DIRECTORY* names is not there and one in memory
stands in for it (DIRECTORY-IN-MEMORY-P), no copy is made there: that is
said, naming the directory missing."
  (let* ((pending (car **copies-pending**))
         (free (let ((free (free-octets directory))) (and free (max 0 (- free pending))))))
    (cond
      ((directory-in-memory-p '*cache-directory*)
       (format nil "its copy for the decoder goes to ~a (*cache-directory*), which is not there, and is not put in ~
                    memory in its place: mount its drive, or set *cache-directory* to a directory that is there"
               (second (assoc '*cache-directory* *directories-in-memory*))))
      ((and free (< free (+ size *snapshot-free-margin*)))
      (format nil "its copy for the decoder needs ~a free in ~a (~a, and ~a to spare), which has ~a~a: make ~
                   room there, or set *cache-directory* to a directory on a drive with room"
              (octets-words (+ size *snapshot-free-margin*) :binary t) (native directory)
              (octets-words size :binary t) (octets-words *snapshot-free-margin* :binary t)
              (octets-words free :binary t)
              (if (plusp pending) (format nil " besides ~a being copied" (octets-words pending :binary t)) ""))))))

(defun sandbox-input-refusal (size &key in-memory)
  "Why a file of SIZE octets is not given to a program decoding it, in words,
with what to set to have it read, or NIL when it may be: it is larger than
*SANDBOX-INPUT-LIMIT*, or its copy goes to the cache directory
\(MEMORY-COPY-P), which has no room for it (COPY-ROOM-REFUSAL). With
IN-MEMORY, for a copy that must be in memory whatever its size, or when every
copy is kept in memory (*SANDBOX-MEMORY-COPY-LIMIT* NIL), a file larger than
*SANDBOX-MEMORY*, or than the memory to be had (AVAILABLE-MEMORY) while that is
NIL, or than *SANDBOX-MEMORY*'s standard value when the system does not say,
is refused too."
  (cond ((and *sandbox-input-limit* (> size *sandbox-input-limit*))
         (format nil "it is ~a, more than the ~a a decoder is given: set *sandbox-input-limit* above its size, ~
                      or to nil, to have it read"
                 (octets-words size :binary t) (octets-words *sandbox-input-limit* :binary t)))
        ((or in-memory (null *sandbox-memory-copy-limit*))
         (let ((most (or *sandbox-memory* (available-memory)
                         (tuatara.runtime:setting-standard-value '*sandbox-memory*))))
           (and most (> size most)
                (format nil "it is ~a, more than the ~a a program reading it is given in memory: set ~
                             *sandbox-memory* above its size to have it read (it needs that much free memory)~
                             ~:[~;, or *sandbox-memory-copy-limit* to a size, to have it copied to the drive~]"
                        (octets-words size :binary t) (octets-words most :binary t) (not in-memory)))))
        ((not (memory-copy-p size))
         (copy-room-refusal size (cache-directory)))))

(sb-ext:defglobal **disk-copies** (make-hash-table :synchronized t)
  "The copies for decoders made in the cache directory (DISK-COPY): for each
descriptor, a cons of the stamp the copy had once it was made read only, by
which it is known again and checked (CHECK-DISK-COPY), and whether it was
found changed since, never to be given again.")

(defun copy-for-decoder (fd copy size)
  "Copy the first SIZE octets of the file open as file descriptor (FD) into the
file open as COPY, stopped when the job doing it is asked to stop."
  (unless (copy-fd-contents fd copy size :path "a file"
                            :cancel (tuatara.runtime:current-job-cancel))
    (tuatara.runtime:job-check-stop)
    (error 'file-system-error :operation "copy" :path "a file" :reason "it was stopped")))

(defparameter *copy-file-systems*
  (list (cons #xef53 "ext4") (cons +xfs-magic+ "XFS") (cons +btrfs-magic+ "btrfs") (cons +f2fs-magic+ "F2FS")
        (cons +bcachefs-magic+ "bcachefs"))
  "The file systems, by magic number with a name, a copy for decoders is made
on (DISK-COPY): local ones that hold files with no name and keep them only
while open. ext2 and ext3 share ext4's number. Not tmpfs, which is memory, nor
one another machine or a program serves.")

(defparameter *file-system-names*
  (list (cons #x01021994 "tmpfs") (cons #x858458f6 "ramfs") (cons #x6969 "NFS") (cons #x65735546 "FUSE")
        (cons #xfe534d42 "SMB2") (cons #xff534d42 "CIFS") (cons +zfs-magic+ "ZFS") (cons #x4d44 "FAT")
        (cons #x2011bab0 "exFAT") (cons #x5346544e "NTFS") (cons #x794c7630 "overlayfs"))
  "Names of file systems a copy for decoders is not made on, by magic number,
for saying which one the cache directory is on.")

(defun unnamed-copy-refusal (directory why)
  "Why no copy for decoders is made in DIRECTORY, the cache directory, WHY
saying what its file system is or did, in words naming what to set."
  (format nil "the file system of the cache directory, ~a, cannot hold a copy with no name (~a), as FUSE, SMB and ~
               NFS cannot: set *cache-directory* to a local directory, or one encrypted by the kernel (LUKS, fscrypt)"
          (native directory) why))

(defun copy-file-system-refusal (magic directory)
  "NIL when a copy for decoders may be made on the file system with MAGIC, the
number statfs gives (f_type), one of *COPY-FILE-SYSTEMS*; else why not, in
words (UNNAMED-COPY-REFUSAL) naming it, DIRECTORY being the cache directory."
  (unless (and magic (assoc magic *copy-file-systems*))
    (unnamed-copy-refusal directory
                          (let ((name (and magic (cdr (assoc magic *file-system-names*)))))
                            (cond (name (format nil "it is ~a" name))
                                  (magic (format nil "it is of the kind numbered #x~(~x~)" magic))
                                  (t "its kind cannot be told"))))))

(defun disk-copy (fd size kept)
  "A copy in the cache directory of the first SIZE octets of the file open as
file descriptor (FD), as a new descriptor at its start that only reads, with
KEPT kept open in programs started. The copy is a file with no name that can
never be given one (OPEN-UNNAMED-AT, NEVER-NAMED), read only to all, gone once
every descriptor of it is closed; the one it was written through is closed
here. Refused with a FILE-SYSTEM-ERROR when the drive has no room for it
\(RESERVE-COPY-ROOM), when its file system is not one of *COPY-FILE-SYSTEMS*
\(COPY-FILE-SYSTEM-REFUSAL) or cannot hold a file with no name (there is no
other way), or when the copy cannot be made read only."
  (let ((directory (cache-directory)))
    (reserve-copy-room size directory)
    (unwind-protect
         (multiple-value-bind (writer errno)
             (call-with-directory directory
                                  (lambda (dirfd)
                                    (let ((refusal (copy-file-system-refusal (fd-file-system dirfd) directory)))
                                      (when refusal
                                        (error 'file-system-error :operation "decode" :path "a file" :reason refusal)))
                                    (open-unnamed-at dirfd #o600 :never-named t))
                                  "make a copy in")
           (unless writer
             (error 'file-system-error :operation "decode" :path "a file" :errno errno
                                       :reason (unnamed-copy-refusal directory (errno-reason errno))))
           (unwind-protect
                (progn
                  (copy-for-decoder fd writer size)
                  (sb-posix:fchmod writer #o400)
                  (let ((reader (handler-case (let ((fd (sb-posix:open (format nil "/proc/self/fd/~d" writer)
                                                                       (logior sb-posix:o-rdonly
                                                                               (if kept 0 +o-cloexec+)))))
                                                (if kept (fd-for-program fd) fd))
                                  (sb-posix:syscall-error (e)
                                    (fail "open again" "a copy in the cache directory" (sb-posix:syscall-errno e))))))
                    (tuatara.runtime:with-fd-closed-on-error (reader "a copy in the cache directory")
                      (let ((stamp (fd-stamp reader "a copy in the cache directory")))
                        (unless (= #o400 (logand #o7777 (stamp-mode stamp)))
                          (error 'file-system-error :operation "decode" :path "a file"
                                                    :reason (format nil "its copy in ~a could not be made read only"
                                                                    (native directory))))
                        (setf (gethash reader **disk-copies**) (list stamp))))
                    reader))
             (sb-posix:close writer)))
      (release-copy-room size))))

(defun disk-copy-p (fd)
  "True when file descriptor (FD) is a copy for decoders in the cache
directory (DISK-COPY): one noted when made, the same file, open only to
read."
  (let ((made (car (gethash fd **disk-copies**))))
    (when made
      (let ((now (ignore-errors (fd-stamp fd "a copy in the cache directory"))))
        (if (and now (same-stamped-file-p made now) (zerop (stamp-nlink now))
                 (= sb-posix:o-rdonly (logand (%fcntl fd 3 0) 3)))      ; F_GETFL, O_ACCMODE
            t
            (progn (remhash fd **disk-copies**) nil))))))

(defun check-disk-copy (fd)
  "Signal a FILE-SYSTEM-ERROR when the copy in the cache directory open as file
descriptor (FD) is not one made there (DISK-COPY-P), or not as it was made:
written, made shorter or longer, or its permissions changed since, as by a
program given it that found a way to write it. It is then marked, so that it
is refused each time it is given again, never copied anew."
  (let* ((entry (gethash fd **disk-copies**))
         (made (car entry))
         (now (and entry (fd-stamp fd "a copy in the cache directory"))))
    (unless (and made (not (cdr entry))
                 (= (stamp-size made) (stamp-size now)) (= (stamp-mtime made) (stamp-mtime now))
                 (= (stamp-ctime made) (stamp-ctime now)) (= (stamp-mode made) (stamp-mode now)))
      (when entry (setf (cdr entry) t))
      (error 'file-system-error :operation "decode" :path "a file"
                                :reason (if entry
                                            (format nil "its copy in the cache directory was changed after it was ~
                                                         made, by a program given it it may be: it is not used again")
                                            "its copy in the cache directory is not the one given")))))

(defun sealed-copy-intact-p (fd)
  "True when the copy for decoders open as file descriptor (FD) may be given
again: sealed in memory, or on the drive and as it was made (CHECK-DISK-COPY).
NIL for one found changed, to be made again from the file, and for any other
file."
  (cond ((sealed-p fd) t)
        ((disk-copy-p fd) (handler-case (progn (check-disk-copy fd) t) (file-system-error () nil)))))

(defun sealed-copy (fd &key kept in-memory)
  "A copy of the file open as file descriptor (FD) that nothing, not even a
program given it, can change, as a new descriptor at its start, with KEPT
kept open in programs started (for bwrap to read). FD's own offset is left
alone. A file up to *SANDBOX-MEMORY-COPY-LIMIT*, or any with IN-MEMORY, is
copied into memory and sealed: not written, not made shorter or longer. A
larger one is copied into the cache directory (DISK-COPY), read only. The
file must not change while it is copied. Refused with a FILE-SYSTEM-ERROR
when the file is larger than a decoder is given or there is no room for its
copy (SANDBOX-INPUT-REFUSAL), or it changed while it was copied. Takes as
long as copying the file: call it from a thread working in the background,
whose job's stop request (TUATARA.RUNTIME:*JOB*) stops it."
  (let* ((stamp (fd-stamp fd "a file"))
         (size (stamp-size stamp))
         (refusal (sandbox-input-refusal size :in-memory in-memory)))
    (when refusal
      (error 'file-system-error :operation "decode" :path "a file" :reason refusal))
    (let ((copy (if (or in-memory (memory-copy-p size))
                    (sealed-memory-file "tuatara-decode" (lambda (copy) (copy-for-decoder fd copy size)) :kept kept)
                    (disk-copy fd size kept))))
      (handler-bind ((error (lambda (condition)
                              (declare (ignore condition))
                              (tuatara.runtime:ignoring-errors-reported ("closing the copy of a file")
                                (close-sealed-copy copy)))))
        (unless (and (same-content-p stamp (fd-stamp fd "a file")) (= size (fd-size copy -1)))
          (error 'file-system-error :operation "decode" :path "a file"
                                    :reason "it changed while it was copied for the decoder")))
      copy)))

(defun reopen-sealed (fd)
  "A descriptor of its own on the sealed copy open as file descriptor (FD), at
its start, read only: programs reading it at once each read from their own
place."
  (handler-case (fd-for-program (sb-posix:open (format nil "/proc/self/fd/~d" fd) sb-posix:o-rdonly))
    (sb-posix:syscall-error (e) (fail "open again" "a copy in memory" (sb-posix:syscall-errno e)))))

(defun sealed-for-program (fd &key in-memory)
  "A descriptor of a sealed copy of the file open as file descriptor (FD), at
its start, kept open in programs started: a new copy (SEALED-COPY, IN-MEMORY
as it has it), or, FD being such a copy already, a descriptor of its own on
it (REOPEN-SEALED), so that one copy serves many programs. A copy in the
cache directory is checked first (CHECK-DISK-COPY). A second value is that
copy in the cache directory, FD or the new one, which the caller checks again
once the program is done; NIL for a copy in memory."
  (cond ((sealed-p fd) (values (reopen-sealed fd) nil))
        ((and (not in-memory) (disk-copy-p fd)) (check-disk-copy fd) (values (reopen-sealed fd) fd))
        (t (let ((copy (sealed-copy fd :kept t :in-memory in-memory)))
             (values copy (and (disk-copy-p copy) copy))))))

(defun close-sealed-copy (fd)
  "Close file descriptor (FD), a copy SEALED-COPY made, and forget it as a copy
in the cache directory: the one way such a copy is closed."
  (remhash fd **disk-copies**)
  (sb-posix:close fd))

(sb-ext:defglobal **sandbox-trial** (list nil)
  "A cons whose car is true while a trial of the sandbox started by
SANDBOX-STATUS-NOW runs, so that one is started at a time.")

(defun sandbox-status-now ()
  "SANDBOX-STATUS's first value without waiting, for the editor's own thread:
what was found when the sandbox was last tried; :OFF, told at once; or
:UNKNOWN while it is not tried yet (or tried again), one trial then started
in the background, which tells :MISSING too. In a process shut in already,
SANDBOX-STATUS's first value, found there at once after the first time."
  (let ((state **sandbox-state**))
    (cond (**shut-in** (values (sandbox-status)))
          ((and state (equal (sandbox-program) (fourth state))) (first state))
          ((null (sandbox-program)) :off)
          (t (when (null (sb-ext:compare-and-swap (car **sandbox-trial**) nil t))
               (tuatara.runtime:make-guarded-thread
                (lambda ()
                  (unwind-protect (tuatara.runtime:ignoring-errors-reported ("trying the sandbox") (sandbox-status))
                    (setf (car **sandbox-trial**) nil)))
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
      (unwind-protect (progn (let ((room (%fcntl out 1032 0)))         ; F_GETPIPE_SZ
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
                                           writable overlays links (directory "/tmp") (file-limit *sandbox-file-limit*)
                                           (ended-well '(0)) on-output watch)
  "Run PROGRAM (a name looked for on the sandbox's PATH, /usr/bin:/bin, or a
full path to one it is shown) with ARGUMENTS (strings, never through a shell)
shut in. It sees the system's programs and libraries, read only, and nothing
else (no home directory, no network, no other process). Its time, memory and
the files it writes in its own /tmp are limited.
It reads INPUT, an open descriptor, on its standard input from its start
\(nothing when NIL). This is a sealed copy of it (SEALED-COPY), never INPUT
itself. An INPUT that is such a copy already is read as it is, through a
descriptor of its own (one copy can serve many runs, at once too). A copy in
the cache directory found changed once the program is done is refused with
a FILE-SYSTEM-ERROR (CHECK-DISK-COPY), what the program printed not given. With
SECRET, a string, in place of INPUT, it reads that on its standard input
instead, through a pipe, followed by a new line (none for an empty one), and
nothing after it. Asking again, it is answered at once that there is nothing
more (SECRET-PIPE). SECRET is never in its arguments, which other programs
can see, nor in the debug log.
Waits for it: call it from a thread doing work in the background. It is ended
after SECONDS (NIL: no limit, by the clock or the processor), when it prints
more than OUTPUT-LIMIT octets (NIL: what it prints is let go, never kept; with
OUTPUT-KEPT NIL, counted against it but not kept), or when CANCEL, a cons, has
a true car. It may take MEMORY octets (NIL: no limit), and write files of
FILE-LIMIT octets at most (NIL: no limit).
ON-OUTPUT, when given, is called with each piece of what it prints as it
prints it: a vector of octets and how many of them are the piece (the vector
is filled again after the call), for a program telling how far it has got.
WATCH, when given, is a function of no arguments called at least once a
second while it runs: when it returns a keyword, the program is ended and
that keyword is how it went.
Returns three values. The first is the octets it printed. The second is how it
went: :DONE (it ended well, with an exit code among ENDED-WELL, 0 alone unless
given), :FAILED (it ended with an error, or ran out of memory), :TIMEOUT,
:TOO-MUCH, :CANCELLED, WATCH's keyword, or :UNAVAILABLE when there is no
sandbox here \(SANDBOX-STATUS). The third is what it said on its error output,
or why there is no sandbox.
FILES are files it is shown at PATH, read only, each (PATH . TEXT) or
\(PATH . FD), a ~/ at the start of PATH standing for its home directory: FD
the descriptor of an open file it is shown a sealed copy of, as of INPUT but
always in memory, which bwrap copies again into memory of its own
\(SEALED-FOR-PROGRAM with IN-MEMORY), so that a program that must seek in a
file (an archive) reads it without reaching the file itself or anything beside
it. ENVIRONMENT is what it has in its environment besides PATH, HOME (/tmp)
and LC_ALL, or in place of them, each (NAME VALUE).
DIRECTORIES are directories of yours it is shown, read only, each
\(SOURCE . TARGET). TARGET is where it sees SOURCE (under /tmp, its home, for
LibreOffice's fonts), or a path alone, seen where it is (its real place, links
in it followed, a link at the root as /home kept as it is here). An entry may
be a single file, shown alone. WRITABLE is the same, for directories it may
write in. /tmp and the sandbox's other own directories, and the directory of
your programs' sockets, are refused with a FILE-SYSTEM-ERROR
\(CHECK-SHOWN-PLACE). An entry of DIRECTORIES may be (FD . TARGET), FD a
descriptor open on a file or a directory: that very file is shown at
TARGET, whatever is at its path now, as long as it has a name.
OVERLAYS are directories it sees where they are and may seem to write in, each
\(SOURCE UPPER WORK). What it writes goes to UPPER, a directory of the editor's
own, SOURCE never changed. WORK is an empty directory beside UPPER that the
system needs for it (where bwrap cannot do that, before 0.11, SOURCE is shown
read only). Those that do not exist are left out. A directory in another one
shown is seen in it. LINKS are symbolic links it has at their paths, each
\(PATH . TEXT), TEXT what the link holds, with no more of PATH's directory
than the link itself: a link in your home directory can be given it so, the
directory being no place to show. Each is a new link, made when the program
starts, with that time and the program's owner, and PATH and TEXT must be
UTF-8. DIRECTORY is where it starts, /tmp unless given.
In a process shut in already, as a package's function run by SANDBOX-CALL
\(MARK-SHUT-IN), it runs in that same shut-in place, held to the same limits,
FILES written there for it alone. It reads only the system's directories,
FILES, DIRECTORIES and OVERLAYS; it writes only in WRITABLE, empty and its own
\(CLAIM-SHUT-IN-WRITABLE), and a new directory, its home, where it starts for
/tmp. No LINKS. All but WRITABLE, and anything it left running, is taken away
once it ends (SANDBOX-ARGUMENTS's DIRECT)."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (if (eq status :ready)
        (multiple-value-bind (copy second) (cond (secret (secret-pipe secret)) (input (sealed-for-program input)))
          ;; decided once: the pipe's writing end, or the copy on the drive to check once the program is done
          (let ((writer (and secret second)) (watched (and (not secret) second)))
            (unwind-protect
                 (let ((start (get-internal-real-time)))
                   (multiple-value-bind (output outcome said)
                       (%sandbox-run bwrap program arguments copy seconds memory output-limit cancel ended-well
                                     output-kept :files files :environment environment :directories directories
                                     :writable writable :overlays overlays :links links :directory directory
                                     :file-limit file-limit :guarded (and watched t) :on-output on-output
                                     :watch watch)
                     (tuatara.runtime:debug-report :job "sandbox: ~a ~{~a~^ ~}: ~(~a~) in ~,3f s, ~:d octets~@[: ~a~]"
                                                   program arguments outcome (tuatara.runtime:seconds-since start)
                                                   (length output) (and (plusp (length said)) said))
                     ;; a copy on the drive the program found a way to write is not trusted, nor what it said
                     (when watched (check-disk-copy watched))
                     (values output outcome said)))
              (when copy (close-sealed-copy copy))
              (when writer (sb-posix:close writer)))))
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
