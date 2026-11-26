;;;; src/runtime/locks.lisp --- Locks on open files, and logs that sessions share.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A lock on a file is taken on its descriptor (flock), shared or held
;;;; alone, and goes when the descriptor is closed. A log several sessions
;;;; write to is held with a shared lock, so that a session beginning a new
;;;; one puts the old aside only when no other is writing to it.

(in-package #:tuatara.runtime)

(defconstant +lock-shared+ 1 "flock's LOCK_SH: a lock others may share.")
(defconstant +lock-alone+ 2 "flock's LOCK_EX: a lock held alone.")
(defconstant +lock-no-wait+ 4 "flock's LOCK_NB: fail at once rather than wait for another holder.")
(defconstant +lock-let-go+ 8 "flock's LOCK_UN: let go of the lock.")

(defun lock-fd (fd &key shared (wait 0))
  "Take a lock on the file open as file descriptor (FD), shared with others
that take it SHARED, else held alone: true when it was had, waiting at most
WAIT seconds for another holder to let it go. The lock goes when FD is
closed or the process ends."
  (wait-until (lambda ()
                ;; with LOCK_NB: never blocked in the call itself
                (zerop (%flock fd (logior (if shared +lock-shared+ +lock-alone+) +lock-no-wait+))))
              wait :interval 1/200))

(defun unlock-fd (fd)
  "Let go of the lock LOCK-FD took on the file open as file descriptor (FD),
for every descriptor of it duplicated from FD too."
  (%flock fd +lock-let-go+)
  nil)

(defun lock-held-p (native &key shared)
  "True when a lock on the file at NATIVE, a path as the system spells it, is
held now, by another process or this one: one held alone, or with SHARED a
shared one as well; NIL when it is free or there is no such file. Only a plain
file counts: a link is not followed, and a named pipe or a device there is
never waited on. A log is in use while a session writes to it
\(OPEN-ROTATED-LOG holds a shared lock on it), so (LOCK-HELD-P LOG :SHARED T)
tells whether one does."
  (let ((fd (handler-case (sb-posix:open native (logior sb-posix:o-rdonly sb-posix:o-nonblock sb-posix:o-nofollow
                                                        +o-cloexec+))
              (sb-posix:syscall-error () nil))))
    (and fd
         (unwind-protect (and (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat fd)))
                              (not (lock-fd fd :shared (not shared))))
           (sb-posix:close fd)))))

(defun open-rotated-log (native &key rotate (keep 3))
  "A descriptor open for writing at the end of the log at NATIVE, made
readable and writable by you alone from the start, holding a shared lock on
it while it is open. With ROTATE (once a session), when no other session is
writing to it, a new one is begun, the logs of the sessions before kept as
NATIVE.1, the one just before, to NATIVE.KEEP; while a session writes to the
log no other puts it aside: one beginning meanwhile writes after its lines.
The debug log and the log of standard error are kept so. A symbolic link at
NATIVE is not followed, nor anything but a plain file written to (a named pipe
there is put aside when rotating, else refused), and programs started are not
given the descriptor. An error when it cannot be opened."
  (when (and rotate (not (lock-held-p native :shared t)))
    (flet ((name (n) (if (zerop n) native (format nil "~a.~d" native n))))
      (loop for n from (1- keep) downto 0
            do (handler-case (sb-posix:rename (name n) (name (1+ n)))
                 (sb-posix:syscall-error () nil)))))
  (let ((fd (sb-posix:open native (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append sb-posix:o-nofollow
                                         sb-posix:o-nonblock +o-cloexec+)
                           #o600)))
    (with-fd-closed-on-error (fd)
      (unless (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat fd)))
        (error "~a is not a plain file" native))
      (handler-case (sb-posix:fchmod fd #o600)
        (sb-posix:syscall-error () nil))
      ;; the shared lock is kept from it only for the moment another session looks whether it is in use
      (unless (lock-fd fd :shared t :wait 1/10)
        (debug-report :file "Writing to ~a without its lock: another session may put it aside" native))
      fd)))
