;;;; test/smoke.lisp --- Smoke tests: the image loads and the harness works.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test smoke
  :description "The core loads, reports a version, and the benchmark harness measures.")

(define-test (smoke version-is-a-string)
  (of-type string (tuatara:version))
  (true (plusp (length (tuatara:version)))))

(define-test (smoke harness-measures-time-and-bytes)
  (multiple-value-bind (result seconds bytes) (measuring (make-array 100000 :element-type '(unsigned-byte 8)))
    (of-type (simple-array (unsigned-byte 8) (*)) result)
    (true (>= seconds 0d0))
    (true (>= bytes 100000) "expected at least 100000 bytes consed, got ~a" bytes)))

(define-test (smoke harness-reports-budget)
  (let ((tuatara.bench:*benchmarks* '()))
    (define-benchmark within (:budget-ms 1000 :reps 1) (+ 1 1))
    (true (run-benchmarks :stream (make-broadcast-stream)))
    (define-benchmark over (:budget-ms 0 :reps 1) (sleep 0.002))
    (false (run-benchmarks :stream (make-broadcast-stream)))))

(define-test (smoke program-options)
  ;; the program's own options, as build/tuatara and tools/tui.lisp take them
  (let ((out (with-output-to-string (*standard-output*)
               (is = 0 (tuatara.terminal:main '("--version"))))))
    ;; the line M-x version says, without where it shows
    (is string= (format nil "~a~%" (tuatara:version-line)) out)
    (true (search (format nil "Tuatara ~a (" (tuatara:version)) out)))
  (let ((out (with-output-to-string (*standard-output*)
               (is = 0 (tuatara.terminal:main '("--help"))))))
    (true (search "Usage: tuatara [--gui] [FILE...]" out))
    (true (search "~/.config/tuatara/init.lisp" out))
    (true (search "--dynamic-space-size SIZE" out))))

(define-test (smoke heap-decided-at-start)
  ;; half the machine's memory, from 2 GB to 16 GB, or TUATARA_HEAP_MB megabytes
  (flet ((wanted (setting memory-kb) (tuatara.terminal::wanted-heap-mb :setting setting :memory memory-kb)))
    (is = 3906 (wanted nil 8000000))
    (is = 16384 (wanted nil 128000000))
    (is = 2048 (wanted nil 2000000))
    (is = 3000 (wanted "3000" 128000000))
    (is = 3906 (wanted "lots" 8000000))
    (is = 3906 (wanted "0" 8000000))
    (false (wanted nil nil)))
  (true (plusp (tuatara.terminal::memory-kb)))
  ;; a memory limit on the process's control group, or one above it, counts too
  (let ((root (merge-pathnames (format nil "tuatara-cgroup-~d/" (sb-posix:getpid)) (uiop:temporary-directory))))
    (flet ((limit (group &rest limits)
             (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
             (loop for (path max) on limits by #'cddr
                   do (let ((file (merge-pathnames "memory.max" (merge-pathnames path root))))
                        (ensure-directories-exist file)
                        (with-open-file (out file :direction :output) (format out "~a~%" max))))
             (tuatara.terminal::memory-limit-kb :root (string-right-trim "/" (namestring root)) :group group)))
      (unwind-protect
           (progn
             (is = 8388608 (limit "/user.slice/app.scope" "user.slice/app.scope/" "8589934592" "user.slice/" "max"))
             (is = 4194304 (limit "/user.slice/app.scope" "user.slice/app.scope/" "max" "user.slice/" "4294967296"))
             (is = 2097152 (limit "/a/b" "a/b/" "4294967296" "a/" "2147483648"))
             (false (limit "/user.slice/app.scope" "user.slice/app.scope/" "max"))
             (false (limit "/nowhere/at/all"))
             (false (limit "no-slash"))
             (false (limit nil)))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
  ;; a restart only for the heap saved in the executable, when the machine wants one
  ;; more than a tenth larger or smaller, and never twice
  (flet ((restart (now built wanted &optional restarted)
           (tuatara.terminal::heap-to-restart-with :now (* now 1024 1024) :built (and built (* built 1024 1024))
                                                   :wanted wanted :restarted restarted)))
    (is = 3906 (restart 16384 16384 3906))
    (is = 16384 (restart 2048 2048 16384))
    (false (restart 16384 16384 15000))
    (false (restart 6144 16384 3906))
    (false (restart 16384 16384 3906 "1"))
    (false (restart 16384 nil 3906))
    (false (restart 16384 16384 nil))))
