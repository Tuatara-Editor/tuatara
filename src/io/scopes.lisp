;;;; src/io/scopes.lisp --- Programs held together in a systemd scope of their own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; systemd-run puts the sandboxes of one editor in scopes of your own
;;;; service manager, their memory together held to *SANDBOX-TOTAL-MEMORY*.
;;;; Each scope is named for the run of the editor that made it, so that the
;;;; scopes of an editor that ended without letting them go are found and
;;;; stopped (END-SCOPES-LEFT).

(in-package #:tuatara.io)

(defun scope-environment ()
  "What systemd-run needs of the environment to reach your own service
manager, and nothing else."
  (loop for name in '("XDG_RUNTIME_DIR" "DBUS_SESSION_BUS_ADDRESS")
        for value = (own-variable name)
        when value collect (format nil "~a=~a" name value)))

(defun scope-arguments (total unit)
  "systemd-run's arguments putting what follows them in a scope of its own
named UNIT (SCOPE-NAME), its processes together held to TOTAL octets of
memory, none of it swapped, the scope let go once they have all ended."
  (list "--user" "--scope" "--quiet" "--collect" "--unit" unit
        "-p" (format nil "MemoryMax=~d" total) "-p" "MemorySwapMax=0" "--"))

(defun try-scope ()
  "systemd-run's path when it can put a program in a scope of your own here, else NIL."
  (let ((path (find-program "systemd-run")))
    (and path
         (nth-value 1 (program-output (append (list path) (scope-arguments (* 64 1024 1024) (scope-name))
                                              (list "/bin/true"))
                                      :capture nil :seconds 10 :environment (scope-environment)))
         path)))

;;; Scopes named for the editor that made them

(defparameter *scope-prefix* "tuatara-sandbox-"
  "How the name of every scope Tuatara has systemd-run make begins (SCOPE-NAME).")

(sb-ext:defglobal **scope-owner** nil
  "This run of the editor as the names of its scopes hold it (SCOPE-OWNER),
made when first needed; NIL until then.")

(sb-ext:defglobal **scope-counter** (list 0)
  "Numbers the scopes this run of the editor has made.")

(defun scope-owner ()
  "This run of the editor, as the names of its scopes hold it: its namespace
of process ids (PID), its process id and its start time (PROCESS-START-TIME),
joined by dashes; no other process has all three while it runs."
  (or **scope-owner**
      (let ((pid (sb-posix:getpid)))
        (setf **scope-owner** (format nil "~a-~d-~a" (pid-namespace) pid (process-start-time pid))))))

(defun scope-name ()
  "A new name for a scope of this run of the editor: *SCOPE-PREFIX*, its
owner (SCOPE-OWNER) and a number, as tuatara-sandbox-4026531836-4242-1785-7.scope."
  (format nil "~a~a-~d.scope" *scope-prefix* (scope-owner) (sb-ext:atomic-incf (car **scope-counter**))))

(defun scope-name-owner (name)
  "The namespace of process ids (PID), process id and start time of the
editor that made the scope called NAME, as three strings; NIL when NAME is
not one SCOPE-NAME makes."
  (let ((end (- (length name) (length ".scope"))))
    (when (and (uiop:string-prefix-p *scope-prefix* name) (uiop:string-suffix-p name ".scope")
               (< (length *scope-prefix*) end))
      (let ((parts (uiop:split-string (subseq name (length *scope-prefix*) end) :separator "-")))
        (when (and (= 4 (length parts))
                   (every (lambda (part) (and (plusp (length part)) (every #'digit-char-p part))) parts))
          (values-list (butlast parts)))))))

(defun scope-left-p (name)
  "True when the scope called NAME is one an editor made (SCOPE-NAME) that
has ended: an editor of this namespace of process ids (PID) whose process
is gone, or whose number is another's now. A scope of an editor running,
of another namespace, or not made by an editor at all, is never left."
  (multiple-value-bind (namespace pid start) (scope-name-owner name)
    (and namespace
         (equal namespace (pid-namespace))
         (not (equal start (process-start-time (parse-integer pid)))))))

(defun stop-scopes (names)
  "Have the service manager stop the scopes NAMES, ending any process left
in them, without waiting for it; true when it took the request."
  (let ((systemctl (find-program "systemctl")))
    (and systemctl names
         (nth-value 1 (program-output (list* systemctl "--user" "stop" "--no-block" names)
                                      :capture nil :seconds 10 :environment (scope-environment))))))

(defun end-scopes-left ()
  "Stop the scopes left by editors that ended before theirs were let go
\(SCOPE-LEFT-P); how many."
  (let* ((systemctl (find-program "systemctl"))
         (listed (and systemctl
                      (program-output (list systemctl "--user" "list-units" "--all" "--plain" "--no-legend"
                                            (format nil "~a*" *scope-prefix*))
                                      :seconds 10 :environment (scope-environment))))
         (left (loop for line in (and listed (uiop:split-string listed :separator '(#\Newline)))
                     for words = (remove "" (uiop:split-string line :separator " ") :test #'string=)
                     ;; the name first, after a mark when the unit is in trouble
                     for name = (find-if #'scope-name-owner words :end (min 2 (length words)))
                     when (and name (scope-left-p name)) collect name)))
    (if (and left (stop-scopes left)) (length left) 0)))
