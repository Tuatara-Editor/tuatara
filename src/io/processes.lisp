;;;; src/io/processes.lisp --- What /proc tells of processes, and signals sent to them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A process's name, parent, group, session and children, read from its
;;;; files under /proc, and signals sent only to the processes that are still
;;;; the ones meant (SIGNAL-PROCESS-IF), for ending a program with all it
;;;; started.

(in-package #:tuatara.io)

(defun proc-text (path)
  "The file at PATH under /proc read whole, each octet a character (a
program's name there may be any octets); NIL when it cannot be read."
  (let ((fd (handler-case (sb-posix:open path sb-posix:o-rdonly) (sb-posix:syscall-error () nil))))
    (when fd
      (unwind-protect
           (handler-case (map 'string #'code-char (read-fd-octets fd :path path :size-hint 1024))
             (file-system-error () nil))
        (sb-posix:close fd)))))

(defun process-unreaped-p (process)
  "True while PROCESS, a program started, has not ended and been reaped: its
number is still its own, to be signalled."
  (member (sb-ext:process-status process) '(:running :stopped)))

(defun proc-numbers (path)
  "The numbers in the file at PATH under /proc, read whole as text; NIL when it cannot be read."
  (let ((text (proc-text path)))
    (and text
         (mapcar #'parse-integer
                 (remove "" (uiop:split-string text :separator '(#\Space #\Newline)) :test #'string=)))))

(defun process-children (process)
  "The numbers of the processes PROCESS, a program started, has started and
not yet seen end, from /proc; NIL once PROCESS has ended, its number no
longer its own."
  (when (process-unreaped-p process)
    (let ((pid (sb-ext:process-pid process)))
      (proc-numbers (format nil "/proc/~d/task/~d/children" pid pid)))))

(defun session-leader (process)
  "The process SESSION-COMMAND's setsid, PROCESS, started in a session of its
own: its first child, whose number is the session's; NIL when it has none,
or once PROCESS has ended, its number no longer its own."
  (first (process-children process)))

(defun process-numbers ()
  "The numbers of every process there is, from /proc; NIL when it cannot be read."
  (let ((fd (handler-case (sb-posix:open "/proc" (logior sb-posix:o-rdonly sb-posix:o-directory))
              (sb-posix:syscall-error () nil)))
        (numbers '()))
    (when fd
      (unwind-protect
           (tuatara.runtime:ignoring-errors-reported ("listing the processes")
             (map-directory-fd (lambda (name)
                                 (when (and (plusp (length name)) (every (lambda (octet) (<= 48 octet 57)) name))
                                   (push (reduce (lambda (number octet) (+ (* number 10) (- octet 48)))
                                                 name :initial-value 0)
                                         numbers)))
                               fd "/proc"))
        (sb-posix:close fd)))
    numbers))

(defun process-stat (pid)
  "What /proc says of the process with process id (PID) after its name, as
strings: its state, its parent's number, its group's, its session's and the
rest; NIL when there is no such process."
  (let ((stat (proc-text (format nil "/proc/~d/stat" pid))))
    ;; pid (name) state ppid pgrp session ...: the fields after the name's closing parenthesis
    (and stat (uiop:split-string (subseq stat (+ 2 (or (position #\) stat :from-end t) -2))) :separator " "))))

(defun process-name (pid)
  "The name of the program the process with process id (PID) runs, as /proc
gives it, its first 15 octets; an empty string when there is no such process."
  (string-right-trim '(#\Newline) (or (proc-text (format nil "/proc/~d/comm" pid)) "")))

(defun process-parent (pid)
  "The number of the parent of the process with process id (PID); NIL when
there is no such process."
  (tuatara.text:whole-number (second (process-stat pid))))

(defun process-status-numbers (pid field)
  "The numbers on the line FIELD (\"Uid\", \"NSpid\") of /proc/PID/status, the
process with process id (PID), in order; NIL when there is no such process or
line."
  (let* ((status (proc-text (format nil "/proc/~d/status" pid)))
         (at (and status (search (format nil "~%~a:" field) status)))
         (start (and at (+ at (length field) 2)))
         (end (and at (or (position #\Newline status :start start) (length status)))))
    (and at
         (mapcar #'parse-integer
                 (remove "" (uiop:split-string (subseq status start end) :separator '(#\Space
                                                                                      #\Tab)) :test #'string=)))))

(defparameter *ssh-server-names* '("sshd" "sshd-session")
  "The names under which the OpenSSH server runs, as PROCESS-NAME gives them.")

(defun started-by-ssh-server-p (pid)
  "True when the parent of the process with process id (PID) is the OpenSSH
server (*SSH-SERVER-NAMES*) run by root in this system's own namespace of
processes, as it is of the process that carries a connection for a client on
another machine. A program run by an ordinary account cannot have such a
parent, and a container with its own processes shows none. False when the
parent changes while it is looked at."
  (let ((parent (process-parent pid)))
    (and parent
         (member (process-name parent) *ssh-server-names* :test #'string=)
         (eql 0 (first (process-status-numbers parent "Uid")))
         (= 1 (length (process-status-numbers parent "NSpid")))
         (eql parent (process-parent pid)))))

(defun process-group-and-session (pid buffer)
  "The process group and the session of the process with process id (PID), as
two values, read from /proc into BUFFER, octets enough for the line it
reads \(1024); NIL when there is no such process."
  (let ((fd (handler-case (sb-posix:open (format nil "/proc/~d/stat" pid) sb-posix:o-rdonly)
              (sb-posix:syscall-error () nil))))
    (when fd
      (let* ((n (unwind-protect (or (handler-case (read-fd-into fd buffer) (file-system-error () nil)) 0)
                  (sb-posix:close fd)))
             ;; pid (name) state ppid pgrp session ...: the fields after the name's closing parenthesis
             (name-end (position 41 buffer :end n :from-end t)))
        (when name-end
          (flet ((number-at (start)
                   (loop with value = 0
                         for at from start below n
                         for octet = (aref buffer at)
                         while (<= 48 octet 57)
                         do (setf value (+ (* value 10) (- octet 48)))
                         finally (return (values value at)))))
            (let* ((group-at (1+ (nth-value 1 (number-at (+ name-end 4)))))
                   (session-at (1+ (nth-value 1 (number-at group-at)))))
              (values (number-at group-at) (number-at session-at)))))))))

(defun processes-in (groups sessions &optional (buffer (make-array 1024 :element-type '(unsigned-byte 8))))
  "The numbers of the processes in one of the process groups GROUPS or one of
the SESSIONS, from /proc; BUFFER is read into (PROCESS-GROUP-AND-SESSION)."
  (loop for pid in (process-numbers)
        when (multiple-value-bind (group session) (process-group-and-session pid buffer)
               (and group (or (member group groups) (member session sessions))))
          collect pid))

(defun session-processes (session)
  "The numbers of the processes in the session numbered SESSION, from /proc."
  (processes-in '() (list session)))

(defun signal-process-if (pid test signal)
  "Send SIGNAL to the process with process id (PID) when TEST, a function of
no arguments looking at it in /proc, is true, the process held by a
descriptor of its own (pidfd) meanwhile, so that the signal never reaches
another given its number after it ended. Where the system has no such
descriptors, sent by its number once TEST is true. True when it was sent."
  (flet ((pidfd-signal (fd signal)
           (zerop (%pidfd-send-signal 424 fd signal (null-pointer) 0))))  ; pidfd_send_signal
    (let ((fd (%pidfd-open 434 pid 0)))  ; pidfd_open
      (cond ((>= fd 0)
             (unwind-protect
                  ;; still the one held when it was looked at: no other can have its number
                  (and (funcall test) (pidfd-signal fd 0) (pidfd-signal fd signal))
               (sb-posix:close fd)))
            ((= (sb-alien:get-errno) sb-posix:enosys)
             (and (funcall test)
                  (handler-case (progn (sb-posix:kill pid signal) t) (sb-posix:syscall-error () nil))))))))

(defun signal-session (process session signal)
  "Send SIGNAL to PROCESS, started by SESSION-COMMAND, with its group, while
it has not ended (its number may be another's after), and to every process
of the session numbered SESSION (SESSION-LEADER), when known. True when one
of the session's processes was still there."
  (when (process-unreaped-p process)
    (tuatara.runtime:ignoring-errors-reported ("signalling ~a" process)
      (sb-ext:process-kill process signal :process-group)))
  (let ((there (and session (session-processes session))))
    (dolist (pid there)
      (handler-case (sb-posix:kill pid signal) (sb-posix:syscall-error () nil)))
    (and there t)))

(defun process-start-time (pid)
  "When the process with process id (PID) started, in clock ticks since the
system started, as /proc gives it (a string); NIL when there is no such
process. Two processes given the same number one after the other never
have the same."
  ;; the 22nd field of its stat, the 20th after the name
  (nth 19 (process-stat pid)))

(defun pid-namespace ()
  "The number of the namespace of process ids (PID) this editor runs in,
from /proc, as a string; NIL when it cannot be read."
  (let ((link (handler-case (sb-posix:readlink "/proc/self/ns/pid") (sb-posix:syscall-error () nil))))
    ;; pid:[4026531836]
    (and link (let ((open (position #\[ link)) (close (position #\] link)))
                (and open close (< open close) (subseq link (1+ open) close))))))
