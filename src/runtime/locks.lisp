;;;; src/runtime/locks.lisp --- Locks on open files, and logs that sessions share.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A lock on a file is taken on its descriptor (flock), shared or held
;;;; alone, and goes when the descriptor is closed. A log several sessions
;;;; write to is held with a shared lock, so that a session beginning a new
;;;; one puts the old aside only when no other is writing to it.

(in-package #:tuatara.runtime)

(defun lock-fd (fd &key shared (wait 0))
  "Take a lock on the file open as file descriptor (FD), shared with others
that take it SHARED, else held alone: true when it was had, waiting at most
WAIT seconds for another holder to let it go. The lock goes when FD is
closed or the process ends."
  (wait-until (lambda ()
                ;; LOCK_SH 1 or LOCK_EX 2, with LOCK_NB 4: never blocked in the call itself
                (zerop (sb-alien:alien-funcall
                        (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
                        fd (logior (if shared 1 2) 4))))
              wait :interval 1/200))

(defun unlock-fd (fd)
  "Let go of the lock LOCK-FD took on the file open as file descriptor (FD),
for every descriptor of it duplicated from FD too."
  ;; LOCK_UN 8
  (sb-alien:alien-funcall (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int)) fd 8)
  nil)

(defun log-in-use-p (native)
  "True when another session has the log at NATIVE open (OPEN-ROTATED-LOG
holds a shared lock on it for as long as it writes there)."
  (let ((fd (handler-case (sb-posix:open native sb-posix:o-rdonly)
              (sb-posix:syscall-error () nil))))
    (and fd
         (unwind-protect (not (lock-fd fd))
           (sb-posix:close fd)))))

(defun open-rotated-log (native &key rotate (keep 3))
  "A descriptor open for writing at the end of the log at NATIVE, made
readable and writable by you alone from the start, holding a shared lock on
it while it is open. With ROTATE (once a session), when no other session is
writing to it, a new one is begun, the logs of the sessions before kept as
NATIVE.1, the one just before, to NATIVE.KEEP; while a session writes to the
log no other puts it aside: one beginning meanwhile writes after its lines.
The debug log and the log of standard error are kept so. An error when it
cannot be opened."
  (when (and rotate (not (log-in-use-p native)))
    (flet ((name (n) (if (zerop n) native (format nil "~a.~d" native n))))
      (loop for n from (1- keep) downto 0
            do (handler-case (sb-posix:rename (name n) (name (1+ n)))
                 (sb-posix:syscall-error () nil)))))
  (let ((fd (sb-posix:open native (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append) #o600)))
    (with-fd-closed-on-error (fd)
      (handler-case (sb-posix:fchmod fd #o600)
        (sb-posix:syscall-error () nil))
      (lock-fd fd :shared t)
      fd)))
