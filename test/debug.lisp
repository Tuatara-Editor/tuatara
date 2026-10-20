;;;; test/debug.lisp --- Debug mode: reports in *Debug*.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test debug-reports
  :description "Debug mode: off, a report costs a look at a variable and evaluates nothing; M-x
debug-mode switches it on, the mode line says Debug, and reports go to *Debug*, each line with
its time, thread and topic: an error in a command with the calls under way, a collection of
garbage; *Debug* keeps *debug-log-max* lines; C-h E shows it; *debug-mode* in the init file
starts the editor with it on.")

(defparameter *test-debug-log*
  (setf tuatara.runtime:*debug-log-path*
        (namestring (merge-pathnames (format nil "tuatara-debug-test-~36r.log" (random (expt 36 8) (make-random-state t)))
                                     (uiop:temporary-directory))))
  "Where the tests' debug log goes: never the debug log of the editor in use.")

(defun drain-debug-reports ()
  "Switch debug mode off, for the editors started after too, and forget the
reports not yet shown."
  (setf tuatara-user:*debug-mode* nil)
  (tuatara.runtime:set-debug nil)
  (dolist (type '("log" "log.1" "log.2" "log.3"))
    (ignore-errors (delete-file (make-pathname :type type :defaults *test-debug-log*))))
  (loop while (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)))

(define-test (debug-reports off-costs-nothing)
  (drain-debug-reports)
  (let ((evaluated nil))
    (tuatara.runtime:debug-report :test "~a" (setf evaluated t))
    (false evaluated)
    (true (sb-concurrency:queue-empty-p tuatara.runtime:*debug-reports*))))

(define-test (debug-reports switched-on)
  (drain-debug-reports)
  (unwind-protect
       (progn
         (let ((b (tuatara.buffer:find-buffer "*Debug*"))) (when b (tuatara.buffer:kill-buffer b)))
         (multiple-value-bind (f editor) (run-keys "M-x d e b u g - m o d e RET" :text "" :width 100 :height 12)
           (declare (ignore editor))
           (true tuatara.runtime:*debug-on*)
           ;; the mode line says so
           (true (tuatara.frontend.fake:fake-frame-showing f "Debug"))
           (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Debug*"))))
             (true (search "debug mode on" text))
             ;; the line's time, thread and topic before the text
             (true (cl-ppcre:scan "(?m)^ +[0-9]+\\.[0-9]{3}  \\S.* debug +debug mode on: Tuatara " text) "~a" text)))
         ;; again: off, said too
         (drain-debug-reports)
         (multiple-value-bind (f editor) (run-keys "M-x d e b u g - m o d e RET M-x d e b u g - m o d e RET" :text "" :width 100)
           (declare (ignore f editor))
           (false tuatara.runtime:*debug-on*)
           (true (search "debug mode off" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Debug*"))))))
    (drain-debug-reports)))

(define-test (debug-reports errors-and-collections)
  ;; an error in a command, with the calls under way
  (drain-debug-reports)
  (setf (gethash 'debug-test-failing tuatara.command::*commands*) (lambda () (error "boom from a test")))
  (unwind-protect
       (multiple-value-bind (f editor)
           (run-keys "M-x d e b u g - m o d e RET M-x d e b u g - t e s t - f a i l i n g RET" :text "" :width 100)
         (declare (ignore f editor))
         (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Debug*"))))
           (true (search "debug-test-failing: boom from a test" text) "~a" text)
           (true (search "    (" text) "the calls under way: ~a" text)))
    (remhash 'debug-test-failing tuatara.command::*commands*)
    (drain-debug-reports))
  ;; a collection of garbage
  (drain-debug-reports)
  (unwind-protect
       (progn
         (tuatara.runtime:set-debug t)
         (sb-ext:gc :full t)
         (true (find :gc (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                               while report collect (third report)))))
    (drain-debug-reports)))

(define-test (debug-reports lines-kept)
  (let ((b (tuatara.buffer:find-buffer "*Debug*"))) (when b (tuatara.buffer:kill-buffer b)))
  (multiple-value-bind (f editor) (run-keys "" :text "" :width 80)
    (declare (ignore f))
    (unwind-protect
         (let ((tuatara-user:*debug-log-max* 5))
           (tuatara.runtime:set-debug t)
           (dotimes (i 12) (tuatara.runtime:debug-report :test "report ~d" i))
           (tuatara.defaults::show-debug-reports editor)
           (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Debug*"))))
             (is = 5 (count #\Newline text))
             (true (search "report 11" text))
             (false (search "report 6" text))))
      (drain-debug-reports))))

(define-test (debug-reports shown-and-started)
  ;; C-h E shows *Debug*
  (multiple-value-bind (f editor) (run-keys "C-h E" :text "" :width 80 :height 20)
    (declare (ignore f))
    (is string= "*Debug*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
  ;; *debug-mode* in the init file starts the editor with it on
  (unwind-protect
       (let ((tuatara-user:*debug-mode* t))
         (multiple-value-bind (f editor) (run-keys "" :text "" :width 80)
           (declare (ignore f editor))
           (true tuatara.runtime:*debug-on*)))
    (drain-debug-reports)))

(define-test (debug-reports rules-named)
  ;; a report of rules compiled says which language's and what they are for
  (drain-debug-reports)
  (unwind-protect
       (let ((grammar (tuatara.syntax:find-grammar "rust")))
         (tuatara.runtime:set-debug t)
         (tuatara.syntax:compile-query grammar "(identifier) @variable" "colouring")
         (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                            while report collect (fourth report))))
           (true (find-if (lambda (text) (search "rust colouring rules, 22 characters, compiled in" text)) texts)
                 "~a" texts)))
    (drain-debug-reports)))

(define-test (debug-reports log-and-commands)
  ;; the commands run and their keys, typing aside; the same lines in the debug
  ;; log at once, the log of the session before kept aside
  (drain-debug-reports)
  (with-test-directory (dir)
    (let ((log (merge-pathnames "debug.log" dir)))
      (put-file log (format nil "a session before~%"))
      (unwind-protect
           (let ((tuatara.runtime:*debug-log-path* (namestring log))
                 (tuatara.runtime::*debug-log-begun* nil))
             (multiple-value-bind (f editor) (run-keys "M-x d e b u g - m o d e RET i x C-g C-e M-x b e g i n n i n g - o f - l i n e RET"
                                                       :text "abc" :width 100)
               (declare (ignore f editor))
               (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Debug*"))))
                 (true (search "command    end-of-line (C-e)" text) "~a" text)
                 (true (search "beginning-of-line (M-x)" text) "~a" text)
                 (false (search "self-insert" text)))
               ;; the same lines in the log, the one before kept aside
               (tuatara.runtime:set-debug nil)
               (let ((written (file-text log)))
                 (true (search "debug mode on: Tuatara" written) "~a" written)
                 (true (search "end-of-line (C-e)" written)))
               (is string= (format nil "a session before~%") (file-text (merge-pathnames "debug.log.1" dir)))))
        (drain-debug-reports)))))

(define-test (debug-reports errors-in-the-background)
  ;; an error in a job of the background, with the calls under way; and in Lisp evaluated
  (drain-debug-reports)
  (unwind-protect
       (progn
         (tuatara.runtime:set-debug t)
         (ignore-errors (tuatara.runtime:reporting-errors ("sorting ~a" "a test") (error "a sort gone wrong")))
         (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                            while report collect (fourth report))))
           (true (find-if (lambda (text) (and (search "sorting a test failed: a sort gone wrong" text) (search "    (" text)))
                          texts)
                 "~a" texts)))
    (drain-debug-reports))
  (unwind-protect
       (multiple-value-bind (f editor) (run-keys "M-x d e b u g - m o d e RET M-: ( e r r o r SPC \" o o p s \" ) RET" :text "" :width 100)
         (declare (ignore f editor))
         (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Debug*"))))
           (true (search "evaluating Lisp: oops" text) "~a" text)))
    (drain-debug-reports)))


(define-test (debug-reports log-private-and-kept-while-live)
  ;; the debug log is made readable by you alone from the start; while another session
  ;; writes to it, a session beginning writes after its lines, never putting it aside
  (drain-debug-reports)
  (with-test-directory (dir)
    (let ((log (merge-pathnames "debug.log" dir))
          (before (merge-pathnames "debug.log.1" dir)))
      (unwind-protect
           (let ((tuatara.runtime:*debug-log-path* (namestring log)))
             (put-file log (format nil "another session writing~%"))
             (let ((fd (sb-posix:open (tuatara.io:native log) sb-posix:o-rdonly)))
               (unwind-protect
                    (let ((tuatara.runtime::*debug-log-begun* nil))
                      (true (tuatara.runtime:lock-fd fd :shared t))
                      (tuatara.runtime:set-debug t)
                      (tuatara.runtime:debug-report :test "a line of this session")
                      (tuatara.runtime:set-debug nil)
                      (false (probe-file before))
                      (let ((written (file-text log)))
                        (true (search "another session writing" written) "~a" written)
                        (true (search "a line of this session" written) "~a" written)))
                 (sb-posix:close fd)))
             ;; that session gone: the next begins a new log, the old kept aside, the new for you alone
             (let ((tuatara.runtime::*debug-log-begun* nil))
               (tuatara.runtime:set-debug t)
               (tuatara.runtime:debug-report :test "the next session")
               (tuatara.runtime:set-debug nil))
             (true (search "another session writing" (file-text before)))
             (false (search "another session writing" (file-text log)))
             (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (tuatara.io:native log))))))
        (drain-debug-reports)))))

(define-test (debug-reports busy-editor-leaves-a-trace)
  ;; a thread busy past *hang-report-seconds* writes the calls under way in it, once
  (let ((was tuatara.runtime:*hang-report-seconds*)
        (out (make-string-output-stream)))
    (setf tuatara.runtime:*hang-report-seconds* 1)
    (unwind-protect
         (let ((busy (sb-thread:make-thread
                      (lambda ()
                        (let ((*error-output* out))
                          (tuatara.runtime:note-editor-busy t)
                          (let ((until (+ (get-internal-real-time) (* 3 internal-time-units-per-second))))
                            (loop while (< (get-internal-real-time) until)))
                          (tuatara.runtime:note-editor-busy nil)))
                      :name "busy test")))
           (tuatara.runtime:start-hang-watch busy)
           (sb-thread:join-thread busy)
           (let ((said (get-output-stream-string out)))
             (true (search "the editor has been busy for" said))
             (is = 1 (count-matches "the editor has been busy" said))))
      (setf tuatara.runtime:*hang-report-seconds* was))))

(define-test (debug-reports hang-watch-sleeps-at-rest)
  ;; with the editor at rest the hang watch does not wake each second: it waits
  ;; for work to start, then looks
  (let* ((looks 0) (going t)
         (rest (sb-thread:make-thread (lambda () (loop while going do (sleep 0.05))) :name "resting test"))
         (watch nil))
    (tuatara.runtime:note-editor-busy nil)
    (loop while (sb-thread:try-semaphore tuatara.runtime::*editor-busy-wake*))
    (setf watch (tuatara.runtime:start-hang-watch rest :while (lambda () (incf looks) going)))
    (unwind-protect
         (progn
           (sleep 3)
           (is = 0 looks)
           (tuatara.runtime:note-editor-busy t)
           (tuatara.runtime:note-editor-busy nil)
           (loop repeat 300 while (zerop looks) do (sleep 0.01))
           (true (plusp looks)))
      (setf going nil)
      (tuatara.runtime:note-editor-busy t)
      (tuatara.runtime:note-editor-busy nil)
      (sb-thread:join-thread watch :default nil :timeout 5)
      (sb-thread:join-thread rest :default nil :timeout 5))))
