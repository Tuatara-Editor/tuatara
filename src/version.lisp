;;;; src/version.lisp --- Version information.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara)

(defvar *version*
  (let ((system (asdf:find-system "tuatara" nil)))
    (if system (asdf:component-version system) "unknown"))
  "The Tuatara version string, taken from the ASDF system definition at load time.")

(defun version ()
  "Return the Tuatara version as a string, for example \"0.90\".
Complexity: O(1)."
  *version*)

(defvar *build* nil
  "What the saved executable was built from, set as it is built
(tools/build-image.lisp): a list (COMMIT CHANGED DAY) of the commit's short
id, true when the tree had changes not committed, and the day of the build,
YYYY-MM-DD; NIL when Tuatara runs from its sources.")

(defvar *built-at* nil
  "When the saved executable was built, a universal time (set as *BUILD* is);
NIL when Tuatara runs from its sources.")

(defun version-line (&optional where)
  "Which Tuatara this is, as one line: Tuatara 0.1.0 (a1b2c3d, built
2026-10-02) on SBCL 2.6.8, then WHERE after a comma when given (\"in the
terminal (kitty)\"); a commit with changes not committed is written a1b2c3d+,
and one run from its sources says so."
  (let ((sbcl (let ((full (lisp-implementation-version)))
                (subseq full 0 (or (position #\- full) (length full))))))
    (format nil "Tuatara ~a (~:[from its sources~;~:*~{~a~:[~;+~], built ~a~}~]) on SBCL ~a~@[, ~a~]"
            *version* *build* sbcl where)))
