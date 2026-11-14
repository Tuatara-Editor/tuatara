;;;; src/io/files.lisp --- Visiting and reverting files, safely on every file system.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Visiting: the text shown must not change because another program
;;;; changes the file. Small files, and the files of pseudo file systems, are
;;;; read into memory. A large file on a local file system is mapped itself,
;;;; so that what the system already has in memory of it serves at once, under
;;;; a read lease: a program that opens it for writing waits until a keeper
;;;; thread has cloned it (btrfs, XFS, bcachefs, ZFS; elsewhere copied) and
;;;; moved the mapping onto the clone. Where no lease is granted (another
;;;; user's file, one open for writing already), the file is cloned at once
;;;; where the file system shares blocks, which leaves the clone to be read from
;;;; disk; else it is mapped and copied in the background, and the mapping
;;;; moves onto the copy when it is done. Copies, and the scratch files of work
;;;; on a buffer's text, go beside the file only where its file system shares
;;;; blocks; elsewhere to the cache directory when it has room (COPIES-DIRECTORY):
;;;; a copy written back to the same drive, a USB stick with exFAT, is slower,
;;;; and gone with the drive when it is unplugged. Until then
;;;; a rewrite in place by another program is caught (CHECK-FILE notes :STALE)
;;;; and the buffer made read-only. A file on a read-only mount is not copied:
;;;; nothing can change it there. Where a file cannot be mapped at all, it is
;;;; copied first. Copies have no name and vanish when closed, even after a
;;;; crash. How the text is kept so is in isolation.lisp; saving is in
;;;; save.lisp, and watching files for changes on disk in watch.lisp.

(in-package #:tuatara.io)

(defvar *memory-read-limit* (* 64 1024 1024)
  "Files up to this many octets are read into memory when visited, where no other
program can change them, their lines counted as they open; larger ones are
mapped (see this file's header) and open at once, their lines counted in the
background (START-INDEXING), so that a file of many gigabytes shows before the
whole of it has been read.")

(defun kind-words (kind)
  "KIND of file (as STAMP-KIND gives it) in words."
  (ecase kind
    (:directory "a directory") (:fifo "a named pipe") ((:character-device :block-device) "a device")
    (:socket "a socket") (:symlink "a symbolic link") (:other "not a regular file") (:regular "a file")))

(defun file-isolation (buffer)
  "How BUFFER's text is kept from other programs' changes to its file. :MEMORY
is read into memory. :LEASED is the file itself, which a writer must wait to
change until it is cloned. :CLONE is a clone sharing blocks. :SNAPSHOT is a
copy, or the old file after a save. :PENDING is mapped while a copy is made.
:DIRECT is mapped with no copy; changes are caught but not kept out."
  (tuatara.buffer:buffer-local buffer :file-isolation :memory))

(defun file-disk-state (buffer)
  "What has become of BUFFER's file on disk, as CHECK-FILE last saw it. NIL
means it is as the buffer read or saved it. :CHANGED means another program
changed or replaced it, the buffer's text being safe. :DELETED means it is
gone. :STALE means it was rewritten in place while the buffer still read its
octets from it, so that the text can no longer be trusted (the buffer is
read-only until reverted)."
  (tuatara.buffer:buffer-local buffer :disk-state))

(defun open-examined (dirfd octets flags path &key (accept '(:regular)) safe (operation "open") (wanted "a file"))
  "A descriptor for the entry whose name is OCTETS in the directory open as
DIRFD (+AT-FDCWD+ for a name from the current directory, or a whole path),
opened with FLAGS, never as a terminal that would take this program as its
own. A symbolic link is followed unless FLAGS hold O_NOFOLLOW.
The entry is first only named (O_PATH), which opens nothing, and examined.
When it is not of a kind ACCEPT lists (as STAMP-KIND gives them) it is refused
unopened, a FILE-SYSTEM-ERROR saying for OPERATION it is not WANTED. So a named
pipe is never waited on and a device never woken (a serial line's control
lines, a tape's position, stay as they were). Then that very file is opened,
through /proc/self/fd, so that what is opened is what was examined even when
the name is changed meanwhile. With SAFE, its access time is left alone where
the system allows (your own files). PATH names it in messages."
  (multiple-value-bind (handle errno)
      (openat-octets dirfd octets (logior +o-path+ +o-cloexec+ (logand flags +o-nofollow+)))
    (unless handle (fail operation path errno))
    (unwind-protect
         (let* ((stamp (fd-stamp handle path))
                (kind (stamp-kind stamp))
                (nofollow (logand flags +o-nofollow+))
                (flags (logior (logandc2 flags +o-nofollow+) +o-cloexec+ sb-posix:o-noctty)))
           (unless (member kind accept)
             (error 'file-system-error :operation operation :path path
                                       :reason (format nil "it is ~a, not ~a" (kind-words kind) wanted)))
           (flet ((try (flags)
                    (if (proc-fd-available-p)
                        (handler-case (sb-posix:open (format nil "/proc/self/fd/~d" handle) flags)
                          (sb-posix:syscall-error (e) (values nil (sb-posix:syscall-errno e))))
                        ;; no /proc: the name opened again, and what it opens checked to be what was examined
                        (multiple-value-bind (fd errno) (openat-octets dirfd octets (logior flags nofollow))
                          (cond ((null fd) (values nil errno))
                                ((let ((now (tuatara.runtime:ignoring-errors-reported ("examining ~a" path)
                                              (fd-stamp fd path))))
                                   (and now
                                        (= (stamp-dev now) (stamp-dev stamp))
                                        (= (stamp-ino now) (stamp-ino stamp))))
                                 fd)
                                (t (sb-posix:close fd)
                                   (error 'file-system-error :operation operation :path path
                                                             :reason "it was replaced while it was opened")))))))
             (multiple-value-bind (fd errno) (try (if safe (logior flags +o-noatime+) flags))
               ;; another's file: its access time cannot be kept, it is read all the same
               (when (and (null fd) safe (eql errno sb-posix:eperm))
                 (multiple-value-setq (fd errno) (try flags)))
               (or fd (fail operation path errno)))))
      (sb-posix:close handle))))

(defun open-for-reading (path safe &key (accept '(:regular :directory)) (operation "open") (wanted "a file")
                                        (follow t))
  "A descriptor for reading the file at PATH, a symbolic link followed (but
for one at PATH itself without FOLLOW, which is then refused as no file),
when it is of a kind ACCEPT lists (a file or a directory as standard):
anything else, a named pipe or a device, is refused without being opened, a
FILE-SYSTEM-ERROR saying for OPERATION it is not WANTED (OPEN-EXAMINED).
With SAFE, its access time is left alone where the system allows (your
own files). Refused for a file still arriving (REFUSE-ARRIVING-FILE)."
  (refuse-arriving-file path)
  (open-examined +at-fdcwd+ (tuatara.text:string-octets (native path))
                 (logior sb-posix:o-rdonly sb-posix:o-nonblock (if follow 0 +o-nofollow+))
                 path :accept accept :safe safe :operation operation :wanted wanted))

(defun clear-noatime (fd)
  "Let reading from the file open as file descriptor (FD) change its access
time again."
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    (when (logtest flags +o-noatime+)
      (sb-posix:fcntl fd sb-posix:f-setfl (logandc2 flags +o-noatime+)))))

(defgeneric host-visit-safely-reason (host path &optional stamp)
  (:documentation "Why the file at PATH, about to be visited and not asked to be visited safely,
is to be all the same, a phrase, or NIL; STAMP, when given, is its stamp, by
which another name it has may be told.")
  (:method ((host tuatara.runtime:host) path &optional stamp)
    (declare (ignore path stamp))
    nil))

(defun decision-on-file (fd path stamp)
  "Why the file open as file descriptor (FD) is to open safely, by where it
really is and its STAMP (HOST-VISIT-SAFELY-REASON), or NIL; a check that fails
is answered by a phrase saying so, so that the file opens safely, and is told
in the debug log."
  (handler-case (host-visit-safely-reason tuatara.runtime:*host* (or (fd-real-path fd) path) stamp)
    (error (condition)
      (tuatara.runtime:debug-report :file "whether ~a opens safely could not be told, so it does: ~a"
                                    (native path) (tuatara.runtime:condition-words condition))
      (format nil "the check whether it opens safely failed (~a)" (tuatara.runtime:condition-words condition)))))

(define-record (opened-file (:constructor make-opened-file (rope mapping stamp isolation keeper class why))
                           (:copier nil))
  "A file's text as OPEN-FILE-TEXT opened it. ROPE is the text, MAPPING the
mapping it reads or NIL, and STAMP the file's stamp. ISOLATION says how the
text is kept from other programs' changes (see FILE-ISOLATION), and KEEPER
what keeps it so: the lease for :LEASED, the snapshot being made for
:PENDING (ATTEND tells it the buffer), else NIL. CLASS is the class of its
file system (FILE-SYSTEM-CLASS). WHY is the phrase that made the file open
safely though it was not asked to, or NIL."
  (rope nil :read-only t :documentation "The text of OPENED-FILE, a rope.")
  (mapping nil :read-only t :documentation "The mapping OPENED-FILE's rope reads, or NIL.")
  (stamp nil :read-only t :documentation "The stamp of the file OPENED-FILE was read from.")
  (isolation nil :read-only t
             :documentation "How OPENED-FILE's text is kept from other programs' changes (see FILE-ISOLATION).")
  (keeper nil :read-only t
          :documentation "What keeps OPENED-FILE's text so: the lease for :LEASED, the snapshot being made for :PENDING,
else NIL.")
  (class nil :read-only t :documentation "The class of the file system OPENED-FILE's file is on (FILE-SYSTEM-CLASS).")
  (why nil :read-only t
       :documentation "The phrase that made OPENED-FILE open safely though it was not asked to, or NIL."))

(defun open-file-text (path &key safe)
  "The text of the file at PATH, as an OPENED-FILE. Refuses what is not a
regular file without opening it, so that a named pipe or a device is never
waited on or woken. See this file's header for which file gets which.
SAFE keeps the file system the file is on exactly as it was: no clone and
no copy is made beside the file (copies go to the cache directory), and its
access time is left alone where the system allows. Unless SAFE, whether the
file is to open safely all the same is asked (HOST-VISIT-SAFELY-REASON) with
where it really is (links followed, as the descriptor says) and its stamp,
and a phrase answered makes the file opened SAFE, its access time never
changed: that phrase is the OPENED-FILE's WHY.
What is opened is what is decided on, however a link is changed meanwhile."
  (let ((fd (open-for-reading path t :accept '(:regular) :wanted "a file of text"))
        (keep-fd nil))
    (unwind-protect
         (let* ((first-stamp (fd-stamp fd path))
                (why (and (not safe) (decision-on-file fd path first-stamp)))
                (safe (or safe (and why t)))
                ;; a save in place cut short is put right before anything reads the file
                (stamp (if (recover-cut-short-save fd path :safe safe) (fd-stamp fd path) first-stamp))
                (class (file-system-class (fd-file-system fd)))
                (size (stamp-size stamp)))
           ;; a file with another name may be evidence under it: its access time is never given back
           (unless (or safe (> (stamp-nlink stamp) 1)) (clear-noatime fd))
           (multiple-value-bind (rope mapping isolation keeper)
               (if (or (eq class :pseudo) (<= size *memory-read-limit*))
                   ;; pseudo files report sizes that are not to be trusted: read to the end, but never past the limit
                   (let ((octets (read-fd-octets fd :path path :size-hint (max 4096 (min size *memory-read-limit*))
                                                 :most *memory-read-limit*)))
                     (when (> (length octets) *memory-read-limit*)
                       (error 'file-system-error :operation "read" :path path
                              :reason
                              (format nil "it gives more than ~:d octets, the most read into memory"
                                      *memory-read-limit*)))
                     (values (tuatara.text:octets->rope octets) nil :memory nil))
                   (let* ((lease (and *lease-large-files* (eq class :local) (take-read-lease fd)
                                      (let ((mapping (tuatara.runtime:ignoring-errors-reported ("mapping ~a" path)
                                                       (tuatara.text:map-fd fd size path))))
                                        (if mapping
                                            (progn (setf keep-fd t) ; the mapping owns it now
                                                   (hold-lease mapping fd path stamp
                                                               (if safe
                                                                   (cache-directory)
                                                                   (snapshot-directory path class :room size))))
                                            (release-lease fd)))))
                          (clone (and (null lease) (not safe) (eq class :local) (try-clone fd path))))
                     (cond
                       (lease
                        (values (tuatara.text:mapping-rope (lease-mapping lease)) (lease-mapping lease) :leased lease))
                       (clone
                        (let ((mapping (tuatara.text:map-fd clone size path)))
                          (values (tuatara.text:mapping-rope mapping) mapping :clone nil)))
                       (t
                        (let ((mapping (tuatara.runtime:ignoring-errors-reported ("mapping ~a" path)
                                         (tuatara.text:map-fd fd size path))))
                          (if (null mapping)
                              ;; a file system that cannot map this file: map a copy instead, when copies are wanted
                              (let ((copy (if *snapshot-large-files*
                                              (copy-to-cache fd size path)
                                              (error 'file-system-error :operation "map" :path path
                                                     :reason "it could not be mapped into memory"))))
                                (let ((mapping (tuatara.text:map-fd copy size path)))
                                  (values (tuatara.text:mapping-rope mapping) mapping :snapshot nil)))
                              (progn
                                (setf keep-fd t) ; the mapping owns it now
                                (let ((snapshot (and *snapshot-large-files* (not (read-only-mount-p fd))
                                                     ;; no snapshot is no reason not to show the file
                                                     (tuatara.runtime:ignoring-errors-reported
                                                         ("starting a copy of ~a" path)
                                                       (start-snapshot
                                                        mapping fd path stamp
                                                        (if safe
                                                            (cache-directory)
                                                            (snapshot-directory path class :room size)))))))
                                  (values (tuatara.text:mapping-rope mapping) mapping
                                          (if snapshot :pending :direct) snapshot)))))))))
             (make-opened-file rope mapping stamp isolation keeper class why)))
      (unless keep-fd (sb-posix:close fd)))))

(defun take-opened-file (buffer opened)
  "Give BUFFER, whose text is now that of OPENED, an OPENED-FILE, what OPENED
says of its file: the mapping its text reads, its stamp, how its text is kept
(FILE-ISOLATION), the class of its file system, its line ending, and its last
octets, to tell later whether it only grew (NOTE-FILE-TAIL)."
  (let ((rope (opened-file-rope opened)))
    (setf (tuatara.buffer:buffer-mapping buffer) (opened-file-mapping opened)
          (buffer-file-stamp buffer) (opened-file-stamp opened)
          (tuatara.buffer:buffer-local buffer :file-isolation) (opened-file-isolation opened)
          (tuatara.buffer:buffer-local buffer :file-class) (opened-file-class opened)
          (tuatara.buffer:buffer-line-ending buffer) (tuatara.buffer:detect-line-ending rope))
    (note-file-tail buffer rope)))

(defconstant +binary-sample+ 65536
  "The octets at the start of a file looked at to tell whether it is binary.")

(defun binary-head-p (octets &optional (end (length octets)))
  "True when OCTETS below END, the first octets of a file (FILE-HEAD), say it
is binary: a null (NUL) octet among the first 64 KB of them (+BINARY-SAMPLE+),
as executables, archives and disk images have and text has not; NIL for no
octets. Opening a file, previewing it, searching files and keeping
directories read tell binary files alike by it."
  (and octets (find 0 octets :end (min end +binary-sample+)) t))

(defvar *file-heads* nil
  "While a file is visited (CALL-WITH-FILE-HEADS), the first octets of the
files read so far, each (PATH LIMIT SIZE OCTETS), in the car, so that each is
read once; NIL otherwise.")

(defun read-fd-start (fd path size limit &key past-size)
  "The first LIMIT octets, or all when there are fewer, of the file open as
file descriptor (FD) (PATH for messages) of SIZE octets, as the system
says; with PAST-SIZE, SIZE and one more, at least 8 KB, at most LIMIT, so
that a file larger than the system says (one in /proc) is seen to be."
  (let* ((octets (make-array (min limit (if past-size (max 8192 (1+ size)) size)) :element-type '(unsigned-byte 8)))
         (got (read-fd-at fd octets 0 :path path)))
    (if (= got (length octets)) octets (subseq octets 0 got))))

(defun read-file-head (path limit)
  "The first octets of the regular file at PATH, at most LIMIT (READ-FD-START),
and its size as a second value; opened as CALL-WITH-FILE opens it. NIL when it
is no regular file or cannot be read."
  (tuatara.runtime:ignoring-errors-reported ("reading the start of ~a" path)
    (handler-case (call-with-file path (lambda (fd path size)
                                         (values (read-fd-start fd path size limit :past-size t) size)))
      (file-system-error () nil))))

(defun file-head (path &optional (limit +binary-sample+))
  "The first octets of the regular file at PATH, at most LIMIT (64 KB as
standard, +BINARY-SAMPLE+), and its size as a second value: read without
changing its access time where the system allows, never from a named pipe
or a device (NIL for what is no regular file, or cannot be read). While a
file is visited (CALL-WITH-FILE-HEADS), a file's head is read once, however
many look at it: whether it is binary (BINARY-HEAD-P), its kind (OCTETS-KIND),
whether it is a PDF or a picture to show as such."
  (let* ((heads *file-heads*)
         (native (native path))
         (kept (and heads (find native (car heads) :key #'first :test #'string=))))
    (if (and kept (or (>= (second kept) limit) (< (length (fourth kept)) (second kept))))
        (let ((octets (fourth kept)))
          (values (if (> (length octets) limit) (subseq octets 0 limit) octets) (third kept)))
        (multiple-value-bind (octets size) (read-file-head native limit)
          (when (and heads octets)
            (push (list native limit size octets) (car heads)))
          (values octets size)))))

(defun call-with-file-heads (function)
  "Call FUNCTION, of no arguments, each file's first octets read once
meanwhile (FILE-HEAD), and return what it returns: around a visit, whose
views, binary test and kind all look at the same octets."
  (if *file-heads*
      (funcall function)
      (let ((*file-heads* (list '())))
        (funcall function))))

(define-record (listed (:constructor make-listed (name octets stamp used device target target-stamp &optional label))
                      (:copier nil))
  "An entry of a directory as LIST-DIRECTORY reads it."
  (name "" :read-only t
        :documentation "The entry's name, decoded as the display decodes text: an
octet that is no UTF-8 as its raw byte's character.")
  (octets nil :read-only t :documentation "The entry's name as the system has it, its octets.")
  (stamp nil :read-only t
         :documentation "The entry's STAMP, a symbolic link not followed; NIL when it could not be examined.")
  (used 0 :read-only t :documentation "The octets the entry takes on the disk.")
  (device nil :read-only t :documentation "For a device, the numbers it stands for, (MAJOR . MINOR); else NIL.")
  (target nil :read-only t :documentation "For a symbolic link, what it holds, decoded as the name is; else NIL.")
  (target-stamp nil :read-only t
                :documentation "For a symbolic link, the STAMP of what it leads
to, NIL when that is nowhere; else NIL.")
  (label nil :read-only t
         :documentation "What ls -l marks after the entry's permissions (ENTRY-LABEL):
#\\+, #\\. or NIL; NIL as well when not asked for."))

(setf (documentation 'make-listed 'function) "Return a LISTED entry with the given fields."
      (documentation 'listed-p 'function) "True when OBJECT is a LISTED entry.")

(defun list-directory (directory &key labels)
  "The entries of DIRECTORY, those whose names start with a dot among them and
. and .. too, as LISTED entries in the order the system gives them, with
LABELS what ls -l marks after their permissions (LISTED-LABEL). Only the
directory is opened, without changing its access time where the system allows
(your own directories); what is in it is examined, never opened, so that named pipes
and devices are safe to list. NIL, and the reason as a second value, when it
cannot be read."
  (handler-case
      (let ((fd (open-for-reading directory t :accept '(:directory) :wanted "a directory"))
            (entries '()))
        (unwind-protect
             (map-directory-fd
              (lambda (octets)
                (push (multiple-value-bind (stamp used device) (statx-octets fd octets #x100)
                        (let* ((kind (and stamp (stamp-kind stamp)))
                               (target (and (eq kind :symlink) (link-octets fd octets))))
                          (make-listed (tuatara.text:octets-string octets) octets stamp (or used 0)
                                       (and (member kind '(:character-device :block-device)) device)
                                       (and target (tuatara.text:octets-string target))
                                       (and (eq kind :symlink) (values (statx-octets fd octets 0)))
                                       (and labels stamp (entry-label fd octets)))))
                      entries))
              fd directory)
          (sb-posix:close fd))
        (nreverse entries))
    (file-system-error (e) (values nil (file-system-error-reason e)))))

(defun directory-entries (directory &key stamps (order #'string<))
  "The entries of DIRECTORY, hidden ones included, sorted by name with ORDER
\(a predicate of two names, STRING< as standard). They are lists of the name
\(an octet that is no UTF-8 as its raw byte's character), the kind (as
STAMP-KIND gives it, following symbolic links; :SYMLINK for one that leads
nowhere), the size in octets and the entry's own kind (:SYMLINK for a link,
whatever it leads to; :OTHER when it could not be examined). With STAMPS they
hold the STAMP of what it leads to (NIL when it could not be examined). Read
as LIST-DIRECTORY reads it: only the directory is opened, so named pipes and
devices in it are safe to list, and nothing is examined twice. NIL when it
cannot be read."
  (sort (loop for entry in (list-directory (native (directory-pathname directory)))
              for octets = (listed-octets entry)
              unless (dot-entry-name-p octets)
                collect (let* ((own (listed-stamp entry))
                               (stamp (if (and own (eq (stamp-kind own) :symlink))
                                          (or (listed-target-stamp entry) own)
                                          own))
                               (fields (list (listed-name entry) (if stamp (stamp-kind stamp) :other)
                                             (and stamp (stamp-size stamp)) (if own (stamp-kind own) :other))))
                          (if stamps (append fields (list stamp)) fields)))
        order :key #'first))

(defun call-with-entry (directory octets function)
  "Call FUNCTION with a descriptor open on the file whose name is OCTETS in
DIRECTORY, a symbolic link followed, the file's name as text and its size;
closed after, whatever FUNCTION returns. Its access time is left alone where
the system allows (your own files). What is not a regular file is refused
without being opened (OPEN-EXAMINED), so that a named pipe or a device is
never read: a FILE-SYSTEM-ERROR, as for a file that cannot be read."
  (let* ((path (path-join directory (tuatara.text:octets-string octets)))
         (dirfd (open-for-reading directory t :accept '(:directory) :wanted "a directory")))
    (unwind-protect
         (let ((fd (open-examined dirfd octets (logior sb-posix:o-rdonly sb-posix:o-nonblock) path
                                  :safe t :wanted "a file of text")))
           (unwind-protect (funcall function fd path (stamp-size (fd-stamp fd path)))
             (sb-posix:close fd)))
      (sb-posix:close dirfd))))

(defun call-with-file (path function)
  "Call FUNCTION with a descriptor open on the file at PATH, its name and its
size, as CALL-WITH-ENTRY does: its access time left alone where the system
allows, and what is not a regular file refused unopened (a FILE-SYSTEM-ERROR)."
  (let ((fd (open-for-reading path t :accept '(:regular))))
    (unwind-protect (funcall function fd path (stamp-size (fd-stamp fd path)))
      (sb-posix:close fd))))

(defun fd-octets (fd path size &optional limit too-large)
  "The octets of the file open as file descriptor (FD), PATH's, of SIZE
octets, read whole from its start. With LIMIT, a FILE-SYSTEM-ERROR when it
has more than LIMIT octets, as SIZE says or as it turns out when read, no
more than that read; with TOO-LARGE, a function of that size, what it
returns instead."
  (flet ((too-large (size)
           (return-from fd-octets
             (if too-large
                 (funcall too-large size)
                 (error 'file-system-error :operation "read" :path path
                                           :reason (format nil "it is larger than ~:d octets" limit))))))
    (when (and limit (> size limit)) (too-large size))
    (sb-posix:lseek fd 0 sb-posix:seek-set)
    (let ((octets (read-fd-octets fd :path path :size-hint size :most limit)))
      (when (and limit (> (length octets) limit)) (too-large (length octets)))
      octets)))

(defun file-octets (path &key limit)
  "The octets of the file at PATH, read whole (CALL-WITH-FILE): its name taken
as it is, a [ or * in it only a character. With LIMIT, a FILE-SYSTEM-ERROR
for a file of more octets than that (FD-OCTETS)."
  (call-with-file path (lambda (fd name size) (fd-octets fd name size limit))))

(defun file-text (path &key limit too-large (missing nil missing-p) (replacement #\?))
  "The text of the file at PATH, read whole as UTF-8 (CALL-WITH-FILE), its
name taken as it is, each octet that is not UTF-8 read as the character
REPLACEMENT (? as standard). With LIMIT, a file of more octets than that is
not read: what TOO-LARGE, a function of the file's name and its size, returns
is returned instead, or without TOO-LARGE a FILE-SYSTEM-ERROR says so. With
MISSING, that is returned when there is no file at PATH, rather than an
error."
  (if (and missing-p (null (path-stamp path)))
      missing
      (call-with-file path
                      (lambda (fd name size)
                        (tuatara.text:lenient-string
                         (fd-octets fd name size limit
                                    (and too-large
                                         (lambda (size) (return-from file-text (funcall too-large name size)))))
                         :replacement replacement)))))

(defun read-entry-start (directory octets limit)
  "The first LIMIT octets, or all when there are fewer, of the file whose name is
OCTETS in DIRECTORY, a symbolic link followed; the file's size as a second
value. Opened as CALL-WITH-ENTRY opens it: its access time left alone where the
system allows, and what is not a regular file refused unread."
  (call-with-entry directory octets
                   (lambda (fd path size) (values (read-fd-start fd path size limit) size))))

(defun read-line-bounded (stream most)
  "The next line of STREAM without its line end, or NIL at its end: its first
MOST characters, and true as a second value when it was longer, the rest of
it read and let go."
  (let ((out (make-string-output-stream)) (count 0))
    (loop (let ((c (read-char stream nil)))
            (cond ((and (null c) (zerop count)) (return nil))
                  ((or (null c) (char= c #\Newline))
                   (return (values (get-output-stream-string out) (> count most))))
                  (t (when (< count most) (write-char c out))
                     (incf count)))))))

(defun read-stream-octets (stream &key limit cancel wipe drain)
  "Everything STREAM gives, a stream on a pipe or a file nothing has been read
from yet (a program's output), as one vector of octets (READ-OUTPUTS). With
LIMIT, reading stops once more than LIMIT octets came: NIL, and true as a
second value; with DRAIN too, it goes on to the end of STREAM, what comes
past LIMIT let go, and the first LIMIT octets are given, true as a second
value (what a program says of itself, never left unread for it to wait on).
CANCEL, a cons, stops reading when its car turns true, while it waits too:
what was read so far, and true as a third value. With WIPE, every piece read
is filled with zeros once the vector holds it, or once it is let go, so that
what was read is left only in the vector given."
  (multiple-value-bind (outputs over late cancelled)
      (read-outputs (list (sb-sys:fd-stream-fd stream)) (list limit) (list drain) :cancel cancel :wipe wipe)
    (declare (ignore late))
    (values (first outputs) (first over) cancelled)))

(defun copy-descriptor-to-scratch (input &key (path "standard input"))
  "Copy everything the open descriptor INPUT gives, to its end, into a new file
with no name in the cache directory (OPEN-SCRATCH-FILE), so that its size is
bounded by the disk and not by memory. Returns the descriptor of that file
and the octets it holds, as two values. Waits: call it from a thread doing
work in the background; in a job, asking it to stop ends the copy, however
long INPUT waits to give more (a pipe that never ends)."
  (let ((fd (open-scratch-file (cache-directory)))
        (block (make-array (* 1024 1024) :element-type '(unsigned-byte 8)))
        (size 0)
        (done nil))
    (unwind-protect
         (progn
           (loop (loop until (plusp (poll-fd input 1 1000))     ; POLLIN
                       do (tuatara.runtime:job-check-stop))
                 (let ((got (read-fd-into input block :path path)))
                   (when (zerop got) (return))
                   (write-octets fd block :end got :path path)
                   (incf size got)))
           (setf done t)
           (values fd size))
      (unless done (tuatara.runtime:close-fd-quietly fd "the copy of ~a" path)))))

(defun scratch-file-text (fd size path)
  "The text of the file open as file descriptor (FD) of SIZE octets
(COPY-DESCRIPTOR-TO-SCRATCH), as two values: a rope, and the mapping it
reads, or NIL. One of at most *MEMORY-READ-LIMIT* octets is read into
memory and FD closed; a larger one is mapped, as a large file is, and the
mapping owns FD from then on. FD is closed when it cannot be read or mapped."
  (if (<= size *memory-read-limit*)
      (unwind-protect
           (progn (sb-posix:lseek fd 0 sb-posix:seek-set)
                  (values (tuatara.text:octets->rope (read-fd-octets fd :path path :size-hint size)) nil))
        (tuatara.runtime:close-fd-quietly fd "the copy of ~a" path))
      (let ((mapping nil))
        (unwind-protect (setf mapping (tuatara.text:map-fd fd size path))
          (unless mapping
            (tuatara.runtime:close-fd-quietly fd "the copy of ~a" path)))
        (values (tuatara.text:mapping-rope mapping) mapping))))

(defun buffer-file-stamp (buffer)
  "The stamp (PATH-STAMP) BUFFER's file had when it was last read or saved, by
which a change on disk and the same file under another name are known; NIL
for a buffer whose file was not there."
  (tuatara.buffer:buffer-local buffer :file-stamp))

(defun (setf buffer-file-stamp) (stamp buffer)
  "Note STAMP as that of BUFFER's file as just read or saved (BUFFER-FILE-STAMP)."
  (setf (tuatara.buffer:buffer-local buffer :file-stamp) stamp))

(defun same-stamped-file-p (stamp other)
  "True when the STAMP OTHER, a file's, names the same file as STAMP: the same device and inode."
  (and stamp other (= (stamp-dev stamp) (stamp-dev other)) (= (stamp-ino stamp) (stamp-ino other))))

(defun buffer-for-file (path &key except safe)
  "The buffer visiting the file at PATH, other than EXCEPT, found by the file
itself and not by the name: x, ./x and a/../x are one file and have one
buffer. NIL when there is none. A file with two names (a link) shares a
buffer between them only as far as that is safe. A buffer opened safely
\(evidence, a safe directory, or asked) is the one for both names. A buffer that
is not is never given to a name asked SAFE, nor to one that
HOST-VISIT-SAFELY-REASON says is to be visited safely, which gets a buffer of
its own. One stat of PATH, none of the buffers (their STAMP is kept)."
  (let ((path (visited-path path)))
    (or (find-if (lambda (buffer) (and (not (eq buffer except)) (equal (tuatara.buffer:buffer-file buffer) path)))
                 tuatara.buffer:*buffers*)
        (let ((stamp (path-stamp path :failure nil)))
          (when stamp
            (let ((same (remove-if-not
                         (lambda (buffer)
                           (and (not (eq buffer except)) (tuatara.buffer:buffer-file buffer)
                                (same-stamped-file-p stamp (buffer-file-stamp buffer))))
                         tuatara.buffer:*buffers*)))
              (or (find-if #'safe-visit-p same)
                  (and same
                       (not (or safe
                                (tuatara.runtime:ignoring-errors-reported ("asking whether to visit ~a safely" path)
                                  (host-visit-safely-reason tuatara.runtime:*host* path))))
                       (first same)))))))))

(defun visited-path (path)
  "PATH as a file is visited under: expanded, and, when it has a .. in it, as
the file system leads (REAL-PATH-SO-FAR), so that it is named as the file
is, not as a link made the way back look."
  (let ((path (expand-path path)))
    (if (find-if (lambda (part) (member part '(:up :back ".."))) (pathname-directory path))
        (expand-path (real-path-so-far (native path)))
        path)))

(defun nearest-directory (path &key including-self)
  "The nearest directory above PATH that is there, a native name ending in /;
with INCLUDING-SELF, PATH itself when it is a directory that is there."
  (if (and including-self (eq (path-kind path) :directory))
      (concatenate 'string (string-right-trim "/" (native path)) "/")
      (loop for directory = (parent-path path) then (parent-path directory)
            until (or (string= directory "/") (path-stamp directory))
            finally (return directory))))

(defun visit-file (path &key safe)
  "The buffer visiting the file at PATH, made if no live buffer does: its text
kept safe from other programs (OPEN-FILE-TEXT); for a file not there yet, an
empty buffer that saving will create. A new buffer is passed to
TUATARA.BUFFER:*FIND-FILE-FUNCTIONS*, which give it its major mode, unless SAFE:
then the buffer is read-only, no mode and no hook sees it, and the file
system the file is on is left as it was. The buffer is read-only too when you
may not write the file, or, for a file not there yet, make it and the
directories it needs in the nearest directory that is there
\(\\[read-only-mode] makes it editable). A file not asked to be visited SAFE
is all the same when HOST-VISIT-SAFELY-REASON gives one, kept for
SAFE-VISIT-REASON. Returns the buffer, and true as a second value when a
buffer visited the file already (it comes as it is)."
  (let* ((path (visited-path path))
         (existing (buffer-for-file path :safe safe)))
    (if existing
        (values existing t)
        (let* ((name (tuatara.buffer:unique-buffer-name (native-file-name path)))
               (buffer nil)
               (why nil))
          (when (and safe (null (path-stamp path)))
            (error 'file-system-error :operation "open" :path path :reason "there is no such file"))
          (if (path-stamp path)
              (let ((opened (open-file-text path :safe safe)))
                (setf why (opened-file-why opened)
                      safe (or safe (and why t))
                      buffer (tuatara.buffer:make-buffer name :text (opened-file-rope opened) :file path))
                (take-opened-file buffer opened)
                (attend (opened-file-keeper opened) buffer))
              (setf buffer (tuatara.buffer:make-buffer name :file path)))
          (if safe
              (setf (tuatara.buffer:buffer-read-only-p buffer) t
                    (safe-visit-p buffer) (or why t))
              (progn
                (unless (if (path-stamp path)
                            (writable-p path)
                            (writable-p (nearest-directory path)))
                  (setf (tuatara.buffer:buffer-read-only-p buffer) t))
                (tuatara.runtime:run-hook tuatara.buffer:*find-file-functions* buffer)))
          (values buffer nil)))))

(defconstant +tail-sample+ 4096
  "Octets of the end of a file kept when it is read or saved, to recognise it
later as the same file grown by appending (APPEND-FROM-DISK).")

(defun note-file-tail (buffer rope)
  "Keep the last octets of ROPE, the text of BUFFER's file as read or saved, for APPEND-FROM-DISK."
  (let ((size (tuatara.text:rope-octet-length rope)))
    (setf (tuatara.buffer:buffer-local buffer :file-tail)
          (tuatara.text:rope-octets rope :start (max 0 (- size +tail-sample+)) :end size))))

(defun safe-visit-p (buffer)
  "True when BUFFER visits its file safely (VISIT-FILE's SAFE)."
  (and (tuatara.buffer:buffer-local buffer :safe-visit) t))

(defun (setf safe-visit-p) (value buffer)
  "Have BUFFER visit its file safely (SAFE-VISIT-P) when VALUE is true: T, or a
phrase saying why when that was not asked for (SAFE-VISIT-REASON); as usual
when it is NIL."
  (setf (tuatara.buffer:buffer-local buffer :safe-visit) value))

(defun safe-visit-reason (buffer)
  "Why BUFFER visits its file safely although that was not asked for, a phrase
\(HOST-VISIT-SAFELY-REASON), or NIL."
  (let ((safe (tuatara.buffer:buffer-local buffer :safe-visit)))
    (and (stringp safe) safe)))

(defun end-safe-visit (buffer)
  "Make BUFFER, visiting its file safely, visit it as usual from now on
(VISIT-FILE): read-only only when this user may not write the file, or, for a
file not there, make it in its directory. Its text stays as it was read, kept
from other programs' changes as before. Returns BUFFER."
  (let ((path (tuatara.buffer:buffer-file buffer)))
    (setf (safe-visit-p buffer) nil
          (tuatara.buffer:buffer-read-only-p buffer)
          (and path
               (not (if (path-stamp path)
                        (writable-p path)
                        (writable-p (parent-path path))))
               t))
    buffer))

;;; Noticing changes on disk

(defun disk-status (buffer recorded now)
  "How BUFFER's file on disk, now at stamp NOW (NIL when there is none),
compares with RECORDED, the stamp of what the buffer read or saved. :SAME
means no change. :CHANGED means another program replaced or rewrote it, the
buffer's text being safe. :STALE means it was rewritten in place while the
buffer still reads its octets from it, so that the text shown can no longer
be trusted. :DELETED means it is gone.
The files of pseudo file systems (/proc, /sys) change as they are read, and
count as :SAME."
  (cond ((eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo) :same)
        ((and (null recorded) (null now)) :same)
        ((null now) :deleted)
        ((null recorded) :changed)
        ((same-content-p recorded now) :same)
        ((and (reads-own-file-p buffer)
              (= (stamp-dev recorded) (stamp-dev now)) (= (stamp-ino recorded) (stamp-ino now)))
         :stale)
        (t :changed)))

(defun reads-own-file-p (buffer)
  "True when BUFFER's text still reads its file's own octets with nothing to
keep a change out: mapped directly, or waiting on a snapshot, or leased with
the lease lost."
  (case (file-isolation buffer)
    ((:direct :pending) t)
    (:leased (let ((lease (buffer-lease buffer)))
               (not (and lease (eq (lease-status lease) :moved)))))
    (t nil)))

(defun file-status (buffer)
  "How BUFFER's file on disk compares, now, with what the buffer last read or
saved (see DISK-STATUS); :UNKNOWN when the system will not say (a directory
made unreadable); NIL for a buffer without a file."
  (let ((path (tuatara.buffer:buffer-file buffer)))
    (when path
      (disk-status buffer (buffer-file-stamp buffer)
                   (handler-case (tuatara.runtime:reporting-errors ("examining ~a" path) (path-stamp path))
                     (error () (return-from file-status :unknown)))))))

(defun note-disk-status (buffer status)
  "Note STATUS, as FILE-STATUS gives it, in BUFFER's FILE-DISK-STATE. A stale
buffer stays stale until reverted, and becomes read-only when it turns stale.
Returns the state, and as a second value true when it changed."
  (let* ((old (file-disk-state buffer))
         (new (cond ((eq old :stale) :stale)
                    ((member status '(nil :unknown)) old)
                    ((eq status :same) nil)
                    (t status))))
    (unless (eq new old)
      (setf (tuatara.buffer:buffer-local buffer :disk-state) new)
      (when (eq new :stale)
        (setf (tuatara.buffer:buffer-local buffer :read-only-before-stale)
              (tuatara.buffer:buffer-read-only-p buffer)
              (tuatara.buffer:buffer-read-only-p buffer) t)))
    (values new (not (eq new old)))))

(defun check-file (buffer)
  "Look at BUFFER's file on disk now and note what has become of it (see
NOTE-DISK-STATUS); returns the state and whether it changed. The editor has
the watcher look instead (WATCH-FILES, TAKE-FILE-LOOKS), which never holds it
up."
  (note-disk-status buffer (file-status buffer)))

;;; Reverting

(defun revert-buffer-file (buffer &key map)
  "Read BUFFER's file again and make it the buffer's text: one undoable step,
unless the text was stale (then the history goes too, being no longer
trustworthy). The buffer is then unmodified and as its file. MAP, when given,
is a function of the old text and the new (ropes) returning the functions
BUFFER-SWAP-TEXT maps positions with, forward and back; by default a position
stays, cut to the new text's size. *BEFORE-EDIT-FUNCTIONS* see the whole text
changed before the file is read, and may refuse it. A buffer whose text is
not its file's octets (an encrypted file's, decrypted) is read again by its
own :REREAD function instead. A narrowed buffer is widened first. Returns
BUFFER."
  ;; the file's text comes back whole, shown whole
  (tuatara.buffer:widen-buffer buffer)
  (let ((reread (tuatara.buffer:buffer-local buffer :reread)))
    (when reread
      (funcall reread buffer)
      (return-from revert-buffer-file buffer)))
  (let* ((path (or (tuatara.buffer:buffer-file buffer)
                   (refuse "Buffer ~a visits no file." (tuatara.buffer:buffer-name buffer))))
         (stale (eq (file-disk-state buffer) :stale))
         (safe (safe-visit-p buffer))
         (read-only (if stale
                        (tuatara.buffer:buffer-local buffer :read-only-before-stale)
                        (tuatara.buffer:buffer-read-only-p buffer)))
         (old-size (tuatara.buffer:buffer-size buffer)))
    (when (save-of buffer)
      (refuse "Buffer ~a is being saved; revert it once the save is done." (tuatara.buffer:buffer-name buffer)))
    (tuatara.buffer:run-before-edit buffer :all)
    (stop-snapshots-of buffer)
    (let* ((opened (open-file-text path :safe safe))
           (rope (opened-file-rope opened)))
      (when (opened-file-why opened)
        (setf safe t
              (safe-visit-p buffer) (opened-file-why opened)))
      (let ((new-size (tuatara.text:rope-octet-length rope)) (done nil))
        ;; the file's own text coming back: a read-only buffer takes it too
        (tuatara.buffer:with-read-only-lifted (buffer :after (if done (or safe read-only) (or stale read-only)))
          (multiple-value-bind (forward backward)
              (if map
                  (funcall map (tuatara.buffer:buffer-text buffer) rope)
                  (values (lambda (position bias) (declare (ignore bias)) (min position new-size))
                          (lambda (position bias) (declare (ignore bias)) (min position old-size))))
            (let ((tuatara.buffer:*hex-view-resizing* t))
              (tuatara.buffer:buffer-swap-text buffer rope forward backward))
            (setf done t)))
        (when stale
          (tuatara.buffer:forget-history buffer))
        (setf (tuatara.buffer:buffer-local buffer :disk-state) nil)
        (take-opened-file buffer opened)
        (tuatara.buffer:buffer-mark-saved buffer)
        ;; the old text's lease serves the old text alone now, given up once nothing reads it
        (lend-file-guards buffer)
        (attend (opened-file-keeper opened) buffer)
        buffer))))

;;; A file that grew

(defun add-appended (buffer added stamp tail)
  "Add ADDED, the octets BUFFER's file grew by, to the end of BUFFER's text for
APPEND-FROM-DISK, the file being now at STAMP, its end before TAIL. Appends
one after another with nothing done between undo together."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (at-end (= (tuatara.buffer:buffer-point buffer) size))
         (unmodified (not (tuatara.buffer:buffer-modified-p buffer)))
         (stale (eq (file-disk-state buffer) :stale))
         (read-only (if stale
                        (tuatara.buffer:buffer-local buffer :read-only-before-stale)
                        (tuatara.buffer:buffer-read-only-p buffer)))
         (group (eql (tuatara.buffer:buffer-content-number buffer) (tuatara.buffer:buffer-local buffer :appended))))
    (tuatara.buffer:with-read-only-lifted (buffer :after read-only)
      (tuatara.buffer:buffer-insert buffer size added :group group))
    (when at-end
      (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-size buffer)))
    (when unmodified (tuatara.buffer:buffer-mark-saved buffer))
    (setf (tuatara.buffer:buffer-local buffer :appended) (tuatara.buffer:buffer-content-number buffer)
          (buffer-file-stamp buffer) stamp
          (tuatara.buffer:buffer-local buffer :disk-state) nil
          (tuatara.buffer:buffer-local buffer :file-tail)
          (if (>= (length added) +tail-sample+)
              (subseq added (- (length added) +tail-sample+))
              (let ((end (concatenate 'tuatara.text:octets tail added)))
                (subseq end (max 0 (- (length end) +tail-sample+))))))))

(defvar *append-at-once-most* (* 1024 1024)
  "The most octets a file grew by that APPEND-FROM-DISK reads at once, on the
editor's thread; more are read in the background (APPEND-JOB) and added when
read, so that a burst written to a followed log, or a slow network drive,
holds no keys up.")

(define-record (append-job (:include tuatara.runtime:job) (:constructor %make-append-job) (:copier nil))
  "Reading in the background what the file of the job's buffer grew by: FD, a
descriptor of the file, from offset FROM, GROWN octets, read into ADDED; the
file was STAMP then, the buffer's recorded stamp RECORDED and its file's last
octets TAIL, which must all still hold for what was read to be added."
  (fd nil :documentation "The descriptor of the file APPEND-JOB reads from.")
  (from 0 :documentation "The offset in the file where APPEND-JOB starts reading.")
  (grown 0 :documentation "How many octets APPEND-JOB reads.")
  (added nil :documentation "The octets APPEND-JOB read.")
  (stamp nil :documentation "The stamp the file had when APPEND-JOB read it.")
  (recorded nil
            :documentation "The stamp the buffer recorded for its file, which must still hold for APPEND-JOB's octets to
be added.")
  (tail nil
        :documentation "The last octets of the buffer's file, which must still hold for APPEND-JOB's octets to be
added."))

(setf (documentation '%make-append-job 'function) "A new APPEND-JOB.")

(tuatara.runtime:define-job-words append-job
  :doing "Reading what the file grew by"
  :left-words "nothing was added")

(defmethod tuatara.runtime:job-take-in ((job append-job))
  "What was read is added to the end of the buffer, when the buffer still
stands where the file was when the reading began."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (tuatara.buffer:live-buffer-p buffer)
      (setf (tuatara.buffer:buffer-local buffer :appending) nil)
      (when (and (eq (append-job-status job) :done)
                 (eq (buffer-file-stamp buffer) (append-job-recorded job))
                 (equalp (tuatara.buffer:buffer-local buffer :file-tail) (append-job-tail job))
                 (not (saving-p buffer)))
        (add-appended buffer (append-job-added job) (append-job-stamp job) (append-job-tail job))))))

(defun start-append-job (buffer fd from grown stamp recorded tail)
  "Read GROWN octets of the file open as FD from offset FROM in the background
for BUFFER, an APPEND-JOB taking FD over, and add them when read."
  (let ((job (%make-append-job :buffer buffer :fd fd :from from :grown grown :stamp stamp :recorded recorded
                               :tail tail)))
    (setf (tuatara.buffer:buffer-local buffer :appending) job)
    (tuatara.runtime:start-job-thread
     job (lambda (job)
           (let ((path (tuatara.buffer:buffer-file buffer))
                 (added (make-array grown :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (unless (= (read-fd-at fd added from :path path) grown)
                    (error 'file-system-error :operation "read" :path path :reason "it is shorter than it was"))
               (tuatara.runtime:close-fd-quietly fd "~a" path))
             (setf (append-job-added job) added)))
     :name "tuatara append")
    job))

(defun append-from-disk (buffer)
  "Add what a file grew by to the end of BUFFER's text, when BUFFER's file on
disk has only grown since the buffer read or saved it (the same file, larger,
still holding the octets it ended with). This is one undoable step, done even
when the buffer has changes of its own. The file is noted as read to its new
end. Point at the end stays at the end, following the file as it grows. An
unmodified buffer stays unmodified, and a buffer the growth made stale is
itself again. Growth of more than *APPEND-AT-ONCE-MOST* octets is read in
the background and added once read (APPEND-JOB). Returns how many octets
were added, or are being read; NIL, nothing done, when the file changed
otherwise or grew by more than *MEMORY-READ-LIMIT* octets."
  (let ((path (tuatara.buffer:buffer-file buffer))
        (recorded (buffer-file-stamp buffer))
        (tail (tuatara.buffer:buffer-local buffer :file-tail))
        (appending (tuatara.buffer:buffer-local buffer :appending)))
    (cond ((and appending (tuatara.runtime:job-running-p appending)) (append-job-grown appending))
          ((and path recorded tail (not (saving-p buffer)) (not (tuatara.buffer:buffer-local buffer :reread)))
           (let ((fd (handler-case (open-for-reading path (safe-visit-p buffer) :accept '(:regular))
                       (file-system-error () nil)))
                 (handed nil))
             (when fd
               (unwind-protect
                    (let* ((now (fd-stamp fd path))
                           (old (stamp-size recorded))
                           (grown (- (stamp-size now) old)))
                      (when (and (eq (stamp-kind now) :regular)
                                 (= (stamp-dev now) (stamp-dev recorded)) (= (stamp-ino now) (stamp-ino recorded))
                                 (< 0 grown) (<= grown *memory-read-limit*) (<= (length tail) old))
                        (let ((check (make-array (length tail) :element-type '(unsigned-byte 8))))
                          (when (and (= (read-fd-at fd check (- old (length tail)) :path path) (length tail))
                                     (equalp check tail))
                            (if (> grown *append-at-once-most*)
                                (progn (start-append-job buffer fd old grown now recorded tail)
                                       (setf handed t)
                                       grown)
                                (let ((added (make-array grown :element-type '(unsigned-byte 8))))
                                  (when (= (read-fd-at fd added old :path path) grown)
                                    (add-appended buffer added now tail)
                                    grown)))))))
                 (unless handed (sb-posix:close fd)))))))))
