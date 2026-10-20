;;;; test/run.lisp --- Command-line test runner: sbcl --non-interactive --load test/run.lisp
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Exits 0 when every test passes, 1 otherwise. The tests keep away from the
;;;; developer's own files however they are run (test/fences.lisp).

;; The nursery size the saved executable uses: fewer,
;; cheaper collections on a machine with tens of GB of RAM.
(setf (sb-ext:bytes-consed-between-gcs) (* 256 1024 1024))
(load (merge-pathnames "../tools/quicklisp.lisp" *load-truename*))
(ql:quickload :tuatara/tests :silent t)
;; Recompile the project every run: ASDF does not track structure-layout
;; dependencies between files, and a stale fasl would fail to load.
;; Full compiler warnings (undefined variables, wrong argument counts) fail the
;; run even when ASDF would carry on; style warnings do not.
(defvar *compiler-warnings* '())
(handler-bind ((warning (lambda (w)
                          ;; ASDF repeats style warnings as a plain "had style-warnings"
                          ;; notice (compile-warned-warning); only the real ones count
                          (unless (or (typep w 'style-warning) (typep w 'uiop:compile-warned-warning))
                            (push (princ-to-string w) *compiler-warnings*)))))
  (asdf:load-system :tuatara/tests :force '(:tuatara/core :tuatara/frontend-fake :tuatara/frontend-tui
                                            :tuatara/defaults :tuatara/terminal :tuatara/tests)))
(when *compiler-warnings*
  (format t "~&~%Compiler warnings (~a):~%~{  ~a~%~}" (length *compiler-warnings*) (reverse *compiler-warnings*))
  (sb-ext:exit :code 2))
(let ((report (parachute:test :tuatara.tests :report 'parachute:plain)))
  (sb-ext:exit :code (if (eql (parachute:status report) :passed) 0 1)))
