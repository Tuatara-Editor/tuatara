;;;; test/package.lisp --- Test and benchmark packages.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.bench
  (:use #:cl)
  (:documentation "The benchmark harness: measures wall time and bytes consed and compares against their budgets.")
  (:export #:measuring #:define-benchmark #:run-benchmarks #:*benchmarks*))

(defpackage #:tuatara.tests
  (:use #:cl #:parachute #:tuatara.bench)
  (:import-from #:tuatara.runtime #:wait-until)
  (:documentation "The Parachute test suite. Run with (parachute:test :tuatara.tests) or tools/test.sh.
A package repository's test run calls TEST-PACKAGE; its tests may run keys in an editor session with RUN-KEYS.")
  (:export #:test-package #:run-keys))
