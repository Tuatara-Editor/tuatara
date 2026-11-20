;;;; lisp/packages/sandbox-call.lisp --- A function of a package run shut in, in a Tuatara of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package that must take a file from outside apart (a mailbox) does it in
;;;; a process of its own: this Tuatara run again, shut in by the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN) as any program decoding a file is. It sees its
;;;; own program, the package's files and what was compiled of them, and the
;;;; files it is given, all read only and where they are, and one directory
;;;; it writes in; no home directory, no network, its time and memory limited.
;;;; It loads the package from what was compiled when the editor loaded it,
;;;; compiling nothing, calls the function asked for, and says what that
;;;; returned as JSON, the only thing the editor reads of what it says. What
;;;; it wrote stays in the directory, for the package to read within limits,
;;;; once everything there that is neither a plain file nor a directory (a
;;;; link above all) is taken away.

(in-package #:tuatara.packages)

(defparameter *sandbox-call-variable* "TUATARA_SANDBOX_CALL"
  "The name in its environment that tells a Tuatara started by SANDBOX-CALL to
run a package's function (SANDBOX-CALL-CHILD) instead of an editor.")

(defparameter *sandbox-call-request* "/tmp/tuatara-call.request"
  "Where a Tuatara started by SANDBOX-CALL finds, in its sandbox, what it is to
do, written by the editor (SANDBOX-CALL-REQUEST).")

(defparameter *sandbox-call-line-limit* (* 4 1024 1024)
  "The most octets of a line a Tuatara started by SANDBOX-CALL may say: the
result of its function, how far it has got, or why it failed.")

(defparameter *sandbox-call-most-entries* (* 1024 1024)
  "The most files, directories and other entries in the directory a Tuatara
started by SANDBOX-CALL wrote in that are looked through for links once it
has ended; past them nothing it gave is taken.")

(defparameter *sandbox-call-heap-room* (* 512 1024 1024)
  "Of the memory a Tuatara started by SANDBOX-CALL may take, the octets left
beside its heap for its program, its libraries and its stacks.")

(defparameter *sandbox-call-least-free* (expt 2 30)
  "The octets the drive a Tuatara started by SANDBOX-CALL writes on must keep
free: it is stopped when there are fewer.")

(defvar *sandbox-call-output* nil
  "In a Tuatara started by SANDBOX-CALL, the stream to the editor that its
function's result and reports go to; NIL elsewhere.")

(sb-ext:defglobal **sandbox-call-reported** nil
  "The internal real time a function run by SANDBOX-CALL last said how far it
had got (SANDBOX-CALL-REPORT), or NIL before it first did.")

(defun sandbox-call-heap (memory)
  "The octets of heap a function run by SANDBOX-CALL with MEMORY octets has
for what it makes: MEMORY less *SANDBOX-CALL-HEAP-ROOM*, 256 megabytes (MB)
at least, in whole megabytes; NIL for MEMORY NIL, no limit. A package sizes
what its function holds at once by it, so that the function stops at a
limit of its own before its memory runs out."
  (and memory (* 1024 1024 (max 256 (floor (- memory *sandbox-call-heap-room*) (* 1024 1024))))))

(defun own-program-shut-in (memory)
  "How this Tuatara is started again in the sandbox with MEMORY octets (NIL: no
limit), as three values: the command, its program's full path first; the
files and directories it must be shown; and what it has in its environment
besides. The saved program runs itself, from the AppImage's directory with
the libraries it brings; run from its sources, Steel Bank Common Lisp (SBCL)
starts from the core made for compiling packages apart (COMPILER-CORE). Its
heap is SANDBOX-CALL-HEAP of MEMORY. NIL when there is nothing to run it
from."
  (let ((runtime (append (and memory
                              (list "--dynamic-space-size"
                                    (format nil "~dMB" (floor (sandbox-call-heap memory) (* 1024 1024)))))
                         (list "--disable-ldb"))))
    (if tuatara.io:*saved-program*
        (let* ((program (sb-posix:readlink "/proc/self/exe"))
               (top (and (equal (tuatara.io:own-variable "APPIMAGE") tuatara.io:*saved-program*)
                         (tuatara.io:parent-path (tuatara.io:parent-path program)))))
          (values (cons program runtime)
                  (list (or top program))
                  (and top (list (list "LD_LIBRARY_PATH" (tuatara.io:path-join top "lib"))))))
        (let ((core (compiler-core))
              (sbcl (uiop:native-namestring sb-ext:*runtime-pathname*)))
          (and core (values (list* sbcl "--core" core "--noinform" runtime) (list sbcl core) '()))))))

(defun function-package-place (function)
  "Where the package FUNCTION, a symbol naming one of its functions, was loaded
from, as *LOADED-PACKAGE-DIRECTORIES* has it: (NAME DIRECTORY FILE). Refused
when FUNCTION is no function of a package loaded."
  (let* ((home (symbol-package function))
         (name (car (find-if (lambda (entry) (member home (cdr entry))) *package-code*)))
         (place (and name (assoc name *loaded-package-directories* :test #'string=))))
    (unless (and place (fboundp function))
      (refuse "~(~s~) is no function of a package loaded" function))
    place))

(defun sandbox-call-request (place function inputs directory arguments)
  "What a Tuatara started by SANDBOX-CALL is given to do, as text read back
without evaluating anything: load the package PLACE says (NAME DIRECTORY
FILE), from where this Tuatara keeps it compiled, then call FUNCTION on
INPUTS, DIRECTORY and ARGUMENTS, printed apart to be read once the package
has loaded."
  (destructuring-bind (name package-directory file) place
    (with-standard-io-syntax
      (let ((*print-readably* t))
        (prin1-to-string
         (list :name name :directory package-directory :file file
               :package (package-name (symbol-package function)) :function (symbol-name function)
               :inputs inputs :output directory
               :arguments (let ((*package* (find-package :keyword))) (prin1-to-string arguments))
               :cache (tuatara.io:native (tuatara.io:cache-directory)) :packages (packages-directory)
               :identity (tuatara-identity)))))))

(defun sandbox-call-open-input (input)
  "INPUT, given to SANDBOX-CALL to read, as a descriptor open on it, where it
really is (a native name, ending in / for a directory), and true when the
descriptor was opened here, as three values. A path is opened here, a link
followed, its access time left alone where the system allows; a descriptor,
an integer, is taken as it is. A FILE-SYSTEM-ERROR when it is neither a file
nor a directory, or has no name any more."
  (let* ((own (not (integerp input)))
         (fd (if own
                 (tuatara.io:open-for-reading (tuatara.io:native input) t :operation "read shut in"
                                                                           :wanted "a file or a directory")
                 input)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (when own (tuatara.runtime:close-fd-quietly fd "~a" input)))))
      (let ((kind (tuatara.io:fd-kind fd))
            (path (tuatara.io:fd-real-path fd)))
        (unless (and path (member kind '(:regular :directory)))
          (error 'tuatara.io:file-system-error :operation "read shut in" :path (if own input (or path "a file"))
                                               :reason (if path "it is neither a file nor a directory"
                                                           "it has no name any more")))
        (values fd (if (eq kind :directory) (tuatara.io:slash-ended path) path) own)))))

(defun sandbox-call-words (label outcome said reported seconds memory directory)
  "Why a function run by SANDBOX-CALL gave nothing, as a sentence beginning
with LABEL: from how its run went, OUTCOME, what it SAID on its error output,
and REPORTED, (WHAT . WORDS) of the line its Tuatara said last, the result
not among them. SECONDS and MEMORY are what it was given; DIRECTORY, where it
wrote."
  (let ((reported-words (and (stringp (cdr reported)) (escape-controls (tuatara.text:one-line (cdr reported))))))
    (cond ((eq (car reported) :memory)
           (format nil "~a ran out of memory~@[ (~a)~]" label (and memory (tuatara.io:octets-words memory))))
          ((or (eq outcome :memory) (search "exhausted" said))
           (format nil "~a ran out of memory~@[ (~a)~]" label (and memory (tuatara.io:octets-words memory))))
          ((eq outcome :timeout) (format nil "~a was stopped after ~:d seconds" label (ceiling seconds)))
          ((eq outcome :full)
           (format nil "~a was stopped: the drive of ~a has less than ~a free" label (abbreviated-path directory)
                   (tuatara.io:octets-words *sandbox-call-least-free*)))
          ((eq outcome :too-much) (format nil "~a said more than it may" label))
          ((eq (car reported) :failed) (format nil "~a failed: ~a" label reported-words))
          ((eq outcome :done) (format nil "~a ended without saying what it did" label))
          (t (sandbox-outcome-words outcome said :label label)))))

(defun sandbox-call-listener (progress)
  "What reads the lines a Tuatara started by SANDBOX-CALL says, as two
functions: one taking each piece of what it prints, a vector of octets and
how many of them are the piece, as SANDBOX-RUN's :ON-OUTPUT does; and one
giving what was heard once it has ended, as two values: a list of the
function's result, or NIL, and (WHAT . VALUE) of a failure it said, WHAT
:FAILED or :MEMORY, or NIL. Each report of how far it has got is given to
PROGRESS as it comes. A line longer than *SANDBOX-CALL-LINE-LIMIT* octets,
or not JSON after its first word, is let go."
  (let ((line (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (long nil) (result nil) (reported nil))
    (labels ((take-line ()
               (let* ((text (tuatara.text:octets-string (coerce line '(simple-array (unsigned-byte 8) (*)))))
                      (space (position #\Space text))
                      (what (subseq text 0 (or space (length text))))
                      (value (and space (handler-case (tuatara.text:json-read text :start (1+ space))
                                          (tuatara.text:json-error () (setf what "") nil)))))
                 (cond ((string= what "result") (setf result (list value)))
                       ((string= what "progress") (when progress (funcall progress value)))
                       ((string= what "failed") (setf reported (cons :failed value)))
                       ((string= what "memory") (setf reported (cons :memory value))))))
             (take (octets count)
               (dotimes (i count)
                 (let ((octet (aref octets i)))
                   (cond ((= octet 10)
                          (unless long (take-line))
                          (setf (fill-pointer line) 0 long nil))
                         ((>= (fill-pointer line) *sandbox-call-line-limit*) (setf long t))
                         (t (vector-push-extend octet line)))))))
      (values #'take (lambda () (values result reported))))))

(defun drive-watch (directory)
  "A function for SANDBOX-RUN's :WATCH that looks, once a second at most, at
the room left on the drive of DIRECTORY: :FULL when it has less than
*SANDBOX-CALL-LEAST-FREE* octets free, else NIL."
  (let ((next 0))
    (lambda ()
      (let ((now (get-internal-real-time)))
        (when (>= now next)
          (setf next (+ now internal-time-units-per-second))
          (let ((free (handler-case (tuatara.io:free-octets directory)
                        (tuatara.io:file-system-error () nil))))
            (and free (< free *sandbox-call-least-free*) :full)))))))

(defun sandbox-call-inputs-changed (inputs before)
  "The first of INPUTS, each (FD PATH), whose stamp is no longer what BEFORE,
a list of their stamps in order, says (its size, its times, its names), as
its path; NIL when none changed."
  (loop for (fd path) in inputs
        for stamp in before
        unless (equalp stamp (tuatara.io:fd-stamp fd path))
          return path))

(defun sandbox-call (function inputs directory &key arguments (seconds *sandbox-seconds*) (memory *sandbox-memory*)
                                                   (file-limit tuatara.io:*sandbox-file-limit*) progress
                                                   (cancel (and tuatara.runtime:*job*
                                                                (tuatara.runtime:job-cancel tuatara.runtime:*job*)))
                                                   (label (format nil "~(~a~)" function)))
  "Run FUNCTION, a symbol naming a function of the package calling, in a
Tuatara of its own shut in by the sandbox, on INPUTS and DIRECTORY: there it
is called as (apply FUNCTION INPUTS DIRECTORY ARGUMENTS), INPUTS the paths of
files or directories it may read, DIRECTORY one it writes in. This is how a
package takes a file from outside apart without parsing it in the editor.
Each of INPUTS is a path or a descriptor open on a file or directory (as
CALL-WITH-FILE gives one). A path is opened here, once; a descriptor is the
file the caller checked and summed through it, whatever is put at its path
meanwhile. Each is shown read only where it really is, and FUNCTION is given
that path, a directory's ending in /. Its stamp is taken before FUNCTION
runs and again once it has ended: an input changed meanwhile spoils what it
gave (:CHANGED).
That Tuatara sees its own program, the package's files and what was compiled
of them, and the directory of fetched packages, read only, INPUTS read only,
their access times untouched, and DIRECTORY, which it may write in. It sees
nothing else: no home directory, no network, no other process. It loads the
package as the editor last compiled it, compiling nothing, and none of your
settings: ARGUMENTS, numbers, strings, keywords and lists of them, carry what
it needs. It may take MEMORY octets (NIL: no limit), SECONDS of time and
processor time, and write files of FILE-LIMIT octets at most (NIL: no limit).
It is stopped when CANCEL, a cons, turns true (on a job's thread, the job's:
stopping the job stops it), or when the drive of DIRECTORY has less than 1
gigabyte (GB) free. Waits for it: call it from a job's thread.
FUNCTION's value must be one JSON can hold (TUATARA.TEXT:JSON-WRITE): it comes
back as JSON-READ reads it, the only thing read of what that Tuatara says.
What FUNCTION gives SANDBOX-CALL-REPORT is passed to PROGRESS, a function of
one argument, on this thread, as it comes. Whatever it wrote stays in
DIRECTORY, however deep, once it has ended and every symbolic link and
anything else that is neither a plain file nor a directory is taken away
\(TUATARA.IO:REMOVE-SPECIAL-ENTRIES), *SANDBOX-CALL-MOST-ENTRIES* entries at
most looked through; read it within limits, as any file from outside.
DIRECTORY is marked as come from outside before FUNCTION runs
\(MARK-FROM-OUTSIDE), so that what is written there is untrusted
\(UNTRUSTED-PATH-P) wherever it is.
Four values: what FUNCTION returned, or NIL; NIL, or why it gave nothing, as
a sentence beginning with LABEL; how it went: :DONE, :FAILED (an error, its
words said), :MEMORY, :TIMEOUT, :CANCELLED, :FULL (the drive), :TOO-MUCH (it
said more than it may, or wrote more entries than may be looked through),
:CHANGED (an input changed while it was read), or :UNAVAILABLE when there is
no sandbox here or nothing to run Tuatara from; and how many entries of what
it wrote were taken away. Refused when FUNCTION is no function of a package
loaded; a FILE-SYSTEM-ERROR when an input is neither a file nor a directory."
  (let ((place (function-package-place function))
        (directory (tuatara.io:slash-ended (tuatara.io:native directory)))
        (open-inputs '()) (opened '()))
    (unless (eq (tuatara.io:path-kind directory) :directory)
      (refuse "~a is not a directory" (abbreviated-path directory)))
    (unwind-protect
         (progn
           (dolist (input inputs)
             (multiple-value-bind (fd path own) (sandbox-call-open-input input)
               (when own (push fd opened))
               (push (list fd path) open-inputs)))
           ;; written by a program taking files from outside apart, whatever they are
           (mark-from-outside directory (or (second (car (last open-inputs))) directory))
           (sandbox-call-opened place function (reverse open-inputs) directory arguments seconds memory file-limit
                                progress cancel label))
      (dolist (fd opened) (tuatara.runtime:close-fd-quietly fd "an input read shut in")))))

(defun sandbox-call-opened (place function inputs directory arguments seconds memory file-limit progress cancel label)
  "SANDBOX-CALL once its INPUTS are open, each (FD PATH), FUNCTION's package
at PLACE, the rest as SANDBOX-CALL has them: its four values."
  (unless (eq :ready (tuatara.io:sandbox-status))
    (return-from sandbox-call-opened
      (values nil (sandbox-outcome-words :unavailable (nth-value 1 (tuatara.io:sandbox-status)) :label label)
              :unavailable 0)))
  (multiple-value-bind (command shown environment) (own-program-shut-in memory)
    (unless command
      (return-from sandbox-call-opened
        (values nil (format nil "~a cannot run: there is no Tuatara to run it in" label) :unavailable 0)))
    (multiple-value-bind (take heard) (sandbox-call-listener progress)
      (let ((before (loop for (fd path) in inputs collect (tuatara.io:fd-stamp fd path)))
            (paths (mapcar #'second inputs)))
        (multiple-value-bind (output outcome said)
            (handler-case
                (tuatara.io:sandbox-run
                 (first command) (rest command)
                 :seconds seconds :memory memory :file-limit file-limit :cancel cancel
                 :output-limit (* 64 *sandbox-call-line-limit*) :output-kept nil
                 :directories (append shown
                                      (list (packages-directory)
                                            (tuatara.io:slash-ended (second place))
                                            (compiled-directory-of (second place)))
                                      (loop for (fd path) in inputs collect (cons fd path)))
                 :writable (list directory)
                 :files (list (cons *sandbox-call-request*
                                    (sandbox-call-request place function paths directory arguments)))
                 :environment (append (list (list *sandbox-call-variable* "1") (list "TUATARA_HEAP_DECIDED" "1"))
                                      environment)
                 :on-output take :watch (drive-watch directory))
              (tuatara.io:file-system-error (condition)
                (values nil :failed (princ-to-string condition))))
          (declare (ignore output))
          (multiple-value-bind (removed whole)
              (tuatara.io:remove-special-entries directory :cancel cancel :limit *sandbox-call-most-entries*)
            (let ((changed (sandbox-call-inputs-changed inputs before)))
              (multiple-value-bind (result reported) (funcall heard)
                (cond (changed
                       (values nil (format nil "~a gave nothing that may be taken: ~a changed while it was read"
                                           label (abbreviated-path changed))
                               :changed removed))
                      ((and (not whole) (not (eq outcome :cancelled)) (not (and cancel (car cancel))))
                       (values nil (format nil "~a wrote what cannot all be looked through: more than ~:d files ~
                                                and directories, or a directory that cannot be read"
                                           label *sandbox-call-most-entries*)
                               :too-much removed))
                      ((and (eq outcome :done) result whole)
                       (values (first result) nil :done removed))
                      (t
                       (values nil (sandbox-call-words label (if whole outcome :cancelled) said reported seconds
                                                       memory directory)
                               (cond ((not whole) :cancelled)
                                     ((or (eq (car reported) :memory) (search "exhausted" said)) :memory)
                                     ((member outcome '(:done :failed)) :failed)
                                     (t outcome))
                               removed)))))))))))

(defun sandbox-call-say (what value)
  "In a Tuatara started by SANDBOX-CALL, tell the editor WHAT (\"result\",
\"progress\", \"failed\" or \"memory\") on a line of its own, VALUE after it
as JSON."
  (let ((stream *sandbox-call-output*))
    (write-string what stream)
    (write-char #\Space stream)
    (tuatara.text:json-write value stream)
    (terpri stream)
    (finish-output stream)))

(defun sandbox-call-report (value)
  "In a function run by SANDBOX-CALL, tell the editor how far it has got:
VALUE, which JSON can hold (TUATARA.TEXT:JSON-WRITE), is given to the
:PROGRESS function SANDBOX-CALL was called with, at most ten times a second,
those between left out. Anywhere else, nothing."
  (when *sandbox-call-output*
    (let ((now (get-internal-real-time)))
      (when (or (null **sandbox-call-reported**)
                (>= now (+ **sandbox-call-reported** (floor internal-time-units-per-second 10))))
        (setf **sandbox-call-reported** now)
        (sandbox-call-say "progress" value)))))

(defun sandbox-call-child ()
  "Run a package's function as the editor asked, in a Tuatara it started shut
in (SANDBOX-CALL): what it is to do read from *SANDBOX-CALL-REQUEST*, nothing
in it evaluated; the package loaded from what was compiled of it, a file that
would need compiling refused; the function called and its value said as
JSON. What the package prints goes to the error output, never to the editor.
Returns the exit code: 0 when the function returned, 1 when it failed, 3 when
memory ran out, each said to the editor first."
  (let ((*sandbox-call-output* *standard-output*))
    (handler-case
        (let* ((request (with-standard-io-syntax
                          (let ((*read-eval* nil))
                            (read-from-string (tuatara.io:file-text *sandbox-call-request*)))))
               (*standard-output* *error-output*))
          (setf *tuatara-identity* (getf request :identity)
                tuatara-user:*cache-directory* (getf request :cache)
                tuatara-user:*packages-directory* (getf request :packages))
          ;; the editor loaded it, the version of Tuatara it needs checked there
          (let* ((*package-compiling* :refused)
                 (*package-directory* (pathname (getf request :directory)))
                 (*loading-package-name* (getf request :name))
                 (*package-check* nil)
                 (entry (package-entry *package-directory* (getf request :name) (getf request :file))))
            (tuatara-user:load-package-file (or entry (refuse "The package has no entry to load"))))
          (let* ((home (or (find-package (getf request :package))
                           (refuse "The package made no Lisp package ~a" (getf request :package))))
                 (function (find-symbol (getf request :function) home))
                 (arguments (with-standard-io-syntax
                              (let ((*read-eval* nil)) (read-from-string (getf request :arguments))))))
            (unless (and function (fboundp function))
              (refuse "The package has no function ~(~a~)" (getf request :function)))
            (sandbox-call-say "result" (apply function (getf request :inputs) (getf request :output) arguments))
            0))
      (sb-kernel::heap-exhausted-error ()
        (sandbox-call-say "memory" nil)
        3)
      (serious-condition (condition)
        (sandbox-call-say "failed" (handler-case (princ-to-string condition)
                                     (serious-condition () "an error that cannot be told")))
        1))))
