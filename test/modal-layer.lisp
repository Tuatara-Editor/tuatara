;;;; test/modal-layer.lisp --- Running the suite with the modal layer off, and the tests that need it on.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; TUATARA_TEST_MODAL=off tools/test.sh runs every session with *modal-editing* nil, to test the Emacs keys
;;;; alone. A test that types SPC or the keys of NORMAL state on purpose, or reads the state in the mode line,
;;;; is defined with DEFINE-MODAL-TEST and runs with the layer on whatever the run's setting.

(in-package #:tuatara.tests)

(when (equal (uiop:getenv "TUATARA_TEST_MODAL") "off")
  ;; the whole suite with the modal layer off (tools/test.sh): sessions start without it
  (setf tuatara.defaults::*modal-editing* nil))

(defun call-with-modal-layer (function)
  "Call FUNCTION with the modal layer on, whatever the run's setting of
*modal-editing* (TUATARA_TEST_MODAL=off turns it off for the whole suite),
and put the setting back after. Set for every thread, not bound, so that a
session's other threads see it too."
  (let ((was tuatara.defaults::*modal-editing*))
    (setf tuatara.defaults::*modal-editing* t)
    (unwind-protect (funcall function)
      (setf tuatara.defaults::*modal-editing* was))))

(defmacro with-modal-layer (&body body)
  "Run BODY with the modal layer on (CALL-WITH-MODAL-LAYER)."
  `(call-with-modal-layer (lambda () ,@body)))

(defmacro define-modal-test (name &body body)
  "Define the test NAME as DEFINE-TEST does, run with the modal layer on. A
test that types SPC or the keys of NORMAL state, or reads the state in the
mode line, on purpose is defined with it."
  (let ((options (loop while (keywordp (first body)) collect (pop body) collect (pop body))))
    `(define-test ,name ,@options (with-modal-layer ,@body))))
