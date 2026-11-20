;;;; test/sandbox-call.lisp --- A package's function run in a Tuatara of its own, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test sandbox-call
  :description "TUATARA:SANDBOX-CALL runs a package's function in a Tuatara of its own shut in by the
sandbox: it reads the files it is given, where they are, their access times untouched, and writes in
one directory alone; it sees no other file, no home directory and no network. Its result comes back
as JSON, its reports as they come; links it left are taken away. A run that fails, runs out of time
or memory, or is stopped says so, and stopping it ends that Tuatara at once.")

(defparameter *sandbox-call-package*
  "(defpackage #:sbxcall (:use #:cl #:tuatara))
(in-package #:sbxcall)

(defun look-around (inputs directory &key outside outside-directory)
  (dotimes (i 3) (sandbox-call-report i) (sleep 0.15))
  (with-open-file (out (merge-pathnames \"out.txt\" directory) :direction :output)
    (write-line \"written\" out))
  (sb-posix:symlink \"/etc/passwd\" (namestring (merge-pathnames \"link\" directory)))
  (list (with-open-file (in (first inputs)) (read-line in))
        (if (probe-file outside) \"seen\" \"unseen\")
        (handler-case (with-open-file (out (merge-pathnames \"x\" outside-directory) :direction :output)
                        (write-line \"x\" out) \"wrote\")
          (error () \"refused\"))
        (with-open-file (in \"/proc/net/dev\")
          (loop for line = (read-line in nil) while line count (find #\\: line)))))

(defun spin (inputs directory) (declare (ignore inputs directory)) (loop))

(defun hog (inputs directory)
  (declare (ignore inputs directory))
  (let ((kept '())) (loop (push (make-array 1000000 :element-type '(unsigned-byte 8) :initial-element 1) kept))))

(defun nap (inputs directory)
  (declare (ignore inputs))
  (with-open-file (out (merge-pathnames \"napping\" directory) :direction :output) (write-line \"z\" out))
  (sleep 600))

(defun break-down (inputs directory) (declare (ignore inputs directory)) (error \"broken on purpose\"))

(defun deep-tree (inputs directory)
  (declare (ignore inputs))
  (let ((deep (format nil \"~a~{~a/~}\" directory (make-list 70 :initial-element \"d\"))))
    (ensure-directories-exist deep)
    (with-open-file (out (concatenate 'string deep \"kept.txt\") :direction :output) (write-line \"deep\" out))
    (sb-posix:symlink \"/etc/passwd\" (concatenate 'string deep \"link\")))
  (sb-posix:chdir directory)
  (dotimes (i 20)
    (sb-posix:mkdir (make-string 250 :initial-element #\\l) #o755)
    (sb-posix:chdir (make-string 250 :initial-element #\\l)))
  (sb-posix:symlink \"/etc/passwd\" \"far-link\")
  (sb-posix:close (sb-posix:open \"far.txt\" (logior sb-posix:o-creat sb-posix:o-wronly) #o644))
  \"written\")

(defun read-opening-line (inputs directory &key wait)
  (when wait
    (with-open-file (out (merge-pathnames \"reading\" directory) :direction :output) (write-line \"r\" out))
    (sleep wait))
  (with-open-file (in (first inputs)) (read-line in)))

(defun many-files (inputs directory)
  (declare (ignore inputs))
  (dotimes (i 100)
    (with-open-file (out (format nil \"~a~d.txt\" directory i) :direction :output) (write-line \"x\" out)))
  \"written\")
"
  "The package whose functions the tests of SANDBOX-CALL run shut in.")

(defmacro with-sandbox-call-package ((dir) &body body)
  "Run BODY with DIR a new directory holding the package sbxcall, loaded as a
package repository's tests load theirs."
  `(with-test-directory (,dir)
     (let ((package-directory (ensure-directories-exist (merge-pathnames "sbxcall/" ,dir))))
       (alexandria:write-string-into-file *sandbox-call-package* (merge-pathnames "sbxcall.lisp" package-directory))
       (unwind-protect (progn (is eq nil (tuatara.packages:load-package-for-test "sbxcall" package-directory))
                              ,@body)
         (setf tuatara.packages::*loaded-package-directories*
               (remove "sbxcall" tuatara.packages::*loaded-package-directories* :key #'first :test #'string=))))))

(defun sbxcall (name)
  "The function NAME of the package sbxcall."
  (find-symbol (string-upcase name) "SBXCALL"))

(define-test (sandbox-call sees-only-what-it-is-given)
  (with-sandbox-call-package (dir)
    (let* ((input (merge-pathnames "evidence.txt" dir))
           (outside (merge-pathnames "beside.txt" dir))
           (output (ensure-directories-exist (merge-pathnames "out/" dir)))
           (elsewhere (ensure-directories-exist (merge-pathnames "elsewhere/" dir)))
           (reports '()))
      (alexandria:write-string-into-file (format nil "the first line~%") input)
      (alexandria:write-string-into-file "not to be seen" outside)
      ;; an access time long past, which reading it shut in leaves as it is
      (sb-posix:utimes (namestring input) 946684800 946684800)
      (multiple-value-bind (result failure outcome)
          (tuatara:sandbox-call (sbxcall "look-around") (list (namestring input)) (namestring output)
                                :arguments (list :outside (namestring outside)
                                                 :outside-directory (namestring elsewhere))
                                :seconds 120 :memory (* 1024 1024 1024)
                                :progress (lambda (value) (push value reports)))
        (is eq :done outcome "~a" failure)
        (false failure)
        ;; the input read where it is; the file beside it unseen; nothing written outside its directory;
        ;; no network but its own loopback
        (is equalp #("the first line" "unseen" "refused" 1) result))
      (is = 946684800 (sb-posix:stat-atime (sb-posix:stat (namestring input))))
      (false (probe-file (merge-pathnames "x" elsewhere)))
      ;; what it wrote is there, the link it left taken away
      (is string= (format nil "written~%") (alexandria:read-file-into-string (merge-pathnames "out.txt" output)))
      ;; and untrusted, wherever it is
      (let ((tuatara-user:*untrusted-directories* '()))
        (true (tuatara.defaults::untrusted-path-p (namestring (merge-pathnames "out.txt" output)))))
      (false (tuatara.io:path-kind (namestring (merge-pathnames "link" output)) :follow nil))
      (true (member 0 reports) "~a" reports))))

(define-test (sandbox-call failures-said)
  (with-sandbox-call-package (dir)
    (let ((output (namestring (ensure-directories-exist (merge-pathnames "out/" dir)))))
      ;; an error, in its words
      (multiple-value-bind (result failure outcome) (tuatara:sandbox-call (sbxcall "break-down") '() output)
        (false result)
        (is eq :failed outcome)
        (true (search "broken on purpose" failure) "~a" failure))
      ;; time up: stopped, said, and soon
      (let ((start (get-internal-real-time)))
        (multiple-value-bind (result failure outcome) (tuatara:sandbox-call (sbxcall "spin") '() output :seconds 3)
          (false result)
          (is eq :timeout outcome)
          (true (search "stopped after 3 seconds" failure) "~a" failure))
        (true (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second))))
      ;; out of memory: said so
      (multiple-value-bind (result failure outcome)
          (tuatara:sandbox-call (sbxcall "hog") '() output :memory (* 768 1024 1024) :seconds 60)
        (false result)
        (is eq :memory outcome "~a" failure)
        (true (search "ran out of memory" failure) "~a" failure))
      ;; no sandbox: not run, and why
      (let ((tuatara-user:*sandbox* nil))
        (is eq :unavailable (nth-value 2 (tuatara:sandbox-call (sbxcall "spin") '() output))))
      ;; a function of no package loaded is refused
      (fail (tuatara:sandbox-call 'car '() output) 'tuatara.runtime:refusal))))

(define-test (sandbox-call cancel-stops-it)
  (with-sandbox-call-package (dir)
    (let* ((output (namestring (ensure-directories-exist (merge-pathnames "out/" dir))))
           (cancel (list nil))
           (start nil)
           (thread (sb-thread:make-thread
                    (lambda () (multiple-value-list (tuatara:sandbox-call (sbxcall "nap") '() output :cancel cancel
                                                                                                    :seconds 600))))))
      ;; once it runs, stopped from outside
      (loop repeat 1200 until (probe-file (merge-pathnames "napping" output)) do (sleep 0.1))
      (true tuatara.io::**sandboxes**)
      (setf start (get-internal-real-time) (car cancel) t)
      (destructuring-bind (result failure outcome &rest rest) (sb-thread:join-thread thread)
        (declare (ignore rest))
        (false result)
        (is eq :cancelled outcome)
        (true (search "stopped" failure)))
      (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)))
      (false tuatara.io::**sandboxes**))))

(define-test (sandbox-call output-kept-as-written)
  ;; plain files and directories stay however deep they lie, past the length a path may have too; the
  ;; links among them are taken away, and how many is said
  (with-sandbox-call-package (dir)
    (let ((output (namestring (ensure-directories-exist (merge-pathnames "out/" dir)))))
      (unwind-protect
           (multiple-value-bind (result failure outcome removed)
               (tuatara:sandbox-call (sbxcall "deep-tree") '() output :seconds 120)
             (is eq :done outcome "~a" failure)
             (is equal "written" result)
             (is eql 2 removed)
             (let ((deep (format nil "~a~{~a/~}" output (make-list 70 :initial-element "d"))))
               (is string= (format nil "deep~%")
                   (alexandria:read-file-into-string (concatenate 'string deep "kept.txt")))
               (false (tuatara.io:path-kind (concatenate 'string deep "link") :follow nil))))
        (tuatara.io:delete-tree (concatenate 'string output (make-string 250 :initial-element #\l)))))))

(define-test (sandbox-call input-given-open)
  ;; a file given open is the one read, whatever is put at its path meanwhile; one that changes while it
  ;; is read is said to have changed, and nothing it gave is taken
  (with-sandbox-call-package (dir)
    (let ((output (namestring (ensure-directories-exist (merge-pathnames "out/" dir))))
          (judged (namestring (merge-pathnames "case/evidence.txt" dir))))
      (alexandria:write-string-into-file (format nil "the evidence~%") (ensure-directories-exist judged))
      (tuatara.io:call-with-file
       judged
       (lambda (fd path size)
         (declare (ignore path size))
         ;; the directory it is in swapped for another holding a file of the same name
         (rename-file (merge-pathnames "case/" dir) (merge-pathnames "moved/" dir))
         (alexandria:write-string-into-file (format nil "put in its place~%") (ensure-directories-exist judged))
         (multiple-value-bind (result failure outcome)
             (tuatara:sandbox-call (sbxcall "read-opening-line") (list fd) output)
           (is eq :done outcome "~a" failure)
           (is equal "the evidence" result))))
      (tuatara.io:call-with-file
       judged
       (lambda (fd path size)
         (declare (ignore size))
         (let ((thread (sb-thread:make-thread
                        (lambda () (multiple-value-list
                                    (tuatara:sandbox-call (sbxcall "read-opening-line") (list fd) output
                                                          :arguments '(:wait 2) :seconds 60))))))
           (loop repeat 600 until (probe-file (merge-pathnames "reading" output)) do (sleep 0.1))
           (tuatara.io:append-to-file path (format nil "more~%"))
           (destructuring-bind (result failure outcome &rest rest) (sb-thread:join-thread thread)
             (declare (ignore rest))
             (false result)
             (is eq :changed outcome "~a" failure)
             (true (search "changed" failure) "~a" failure))))))))

(define-test (sandbox-call output-looked-through-within-limits)
  ;; more entries written than may be looked through: said, and nothing it gave is taken
  (with-sandbox-call-package (dir)
    (let ((output (namestring (ensure-directories-exist (merge-pathnames "out/" dir))))
          (tuatara.packages::*sandbox-call-most-entries* 50))
      (multiple-value-bind (result failure outcome) (tuatara:sandbox-call (sbxcall "many-files") '() output)
        (false result)
        (is eq :too-much outcome "~a" failure)
        (true (search "more than 50" failure) "~a" failure)))))

(define-test (sandbox-call heap-told)
  ;; the heap a function run shut in has is told for the memory it is given, and is what it gets
  (is eql nil (tuatara:sandbox-call-heap nil))
  (is = (* 1024 1024 1024)
      (tuatara:sandbox-call-heap (+ (* 1024 1024 1024) tuatara.packages::*sandbox-call-heap-room*)))
  (is = (* 256 1024 1024) (tuatara:sandbox-call-heap (* 300 1024 1024)))
  (is equal "1024MB" (second (member "--dynamic-space-size"
                                     (tuatara.packages::own-program-shut-in
                                      (+ (* 1024 1024 1024) tuatara.packages::*sandbox-call-heap-room*))
                                     :test #'equal))))
