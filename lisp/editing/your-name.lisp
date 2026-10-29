;;;; lisp/editing/your-name.lisp --- Your name, as exported documents, templates and snippets give it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defvar *user-full-name* nil
  "Your full name, as \"Ann Taranaki\": the author of an exported document
with no #+author:, %author% in a note template and $AUTHOR in a snippet. NIL,
as standard, for the name the system has for you.")

(defun user-full-name ()
  "Your name: *USER-FULL-NAME*, else the name the system has for you (the
full name of your account, else its login name)."
  (or (and *user-full-name* (plusp (length *user-full-name*)) *user-full-name*)
      (let ((entry (ignore-errors (sb-posix:getpwuid (sb-posix:getuid)))))
        (and entry (let ((gecos (sb-posix:passwd-gecos entry)))
                     (if (plusp (length gecos))
                         (subseq gecos 0 (or (position #\, gecos) (length gecos)))
                         (sb-posix:passwd-name entry)))))))
