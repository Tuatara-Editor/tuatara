;;;; lisp/files/differences.lisp --- two files or directories compared, by diff shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Two files or directories compared with GNU diff: M-x diff-files compares
;;;; the file shown with another, = in rimu the two entries marked. The
;;;; differences are shown unified, as git shows them, in diff-mode;
;;;; directories are compared with all in them. diff runs in the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN), seeing only the two, read only.

(in-package #:tuatara.defaults)

(define-setting *differences-limit* (* 64 1024 1024)
  "The most octets of differences shown.
When there are more, none are shown, and that is said. Raise it to see larger
differences, at the cost of the memory of all of them; lower it to refuse them
sooner.
- a whole number of octets: the limit, 64 MB as standard."
  :values (integer 0 *))

(defun sandbox-shown-paths (paths)
  "What a program shut in the sandbox comparing or merging the files or
directories PATHS is shown of them: each alone, read only, but /dev/null,
which the sandbox has of its own."
  (remove-duplicates (remove "/dev/null" paths :test #'string=) :test #'string=))

(defun file-differences (old new &key cancel)
  "The differences between OLD and NEW, two files or two directories, as diff
--unified writes them: the lines of OLD taken out after -, those of NEW put
in after +, three lines around each change. Directories are compared with
all in them, a file only one holds compared with none, /dev/null as an empty
file, links as links, never followed; of files that are not text, only that
they differ; a file and a directory, the file with the one of its name in
the directory. Times are written in your time zone (SANDBOX-TIME-ZONE). An
empty rope when they are the same. diff, shut in, sees only the two, read
only. Stopped when CANCEL's car turns true; an error saying why when they
cannot be compared, or the differences are longer than *DIFFERENCES-LIMIT*."
  (multiple-value-bind (output outcome said)
      (tuatara.io:sandbox-run "diff" (append (list "--unified" "--recursive" "--no-dereference")
                                             ;; a file missing on one side counts as empty only between two
                                             ;; directories
                                             (and (eq :directory (tuatara.io:path-kind old))
                                                  (eq :directory (tuatara.io:path-kind new))
                                                  (list "--new-file"))
                                             (list "--" (tuatara.io:real-place old) (tuatara.io:real-place new)))
                              :output-limit *differences-limit* :cancel cancel :ended-well '(0 1)
                              :environment (sandbox-time-zone) :directories (sandbox-shown-paths (list old new)))
    (unless (eq outcome :done)
      (refuse "~a" (sandbox-outcome-words outcome said :label "diff")))
    (tuatara.text:octets->rope (coerce output 'tuatara.text:octets))))

(define-record (differences-job (:include tuatara.runtime:job) (:constructor make-differences-job (buffer old new))
                               (:copier nil))
  "The differences between the files OLD and NEW being found for BUFFER, a
job; ROPE, those found."
  (old nil :documentation "The path of the older file that DIFFERENCES-JOB compares.")
  (new nil :documentation "The path of the newer file that DIFFERENCES-JOB compares.")
  (rope nil :documentation "The differences DIFFERENCES-JOB found, as a rope, NIL until it is done."))

(tuatara.runtime:define-job-words differences-job
  :doing "Comparing"
  :description (format nil "Comparing ~a with ~a" (tuatara.io:native-file-name (differences-job-old job))
                       (tuatara.io:native-file-name (differences-job-new job)))
  :verb "compare"
  :left-words "nothing compared"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-take-in ((job differences-job))
  "Show the differences found, that there are none, or why they could not be found."
  (let ((old (differences-job-old job)) (new (differences-job-new job)) (rope (differences-job-rope job)))
    (show-reading-result job old
                         (if (and rope (zerop (tuatara.text:rope-octet-length rope)))
                             (format nil
                                     "No differences: ~a and ~a are the same.~%"
                                     (abbreviated-path old)
                                     (abbreviated-path new))
                             rope)
                         (lambda (why)
                           (format nil "~a and ~a could not be compared: ~a~%"
                                   (abbreviated-path old) (abbreviated-path new) why)))))

(defun show-differences (old new)
  "Show the differences between OLD and NEW, two files or directories, in a buffer
of their own in another window, found in the background (FILE-DIFFERENCES);
\\`q' closes it. One showing these two before is made afresh; one showing
two others of the same names is left as it is."
  (let* ((name (format nil "*Diff: ~a and ~a*" (tuatara.io:native-file-name old) (tuatara.io:native-file-name new)))
         (before (tuatara.buffer:find-buffer name))
         (taken (and before (not (equal (tuatara.buffer:buffer-local before 'compared) (list old new)))))
         (buffer (fresh-made-buffer (if taken (tuatara.buffer:unique-buffer-name name) name)
                                    'diff-mode
                                    (format nil "Comparing ~a with ~a...~%" (abbreviated-path old)
                                            (abbreviated-path new)))))
    (setf (tuatara.buffer:buffer-local buffer 'compared) (list old new)
          (buffer-given-directory buffer) (tuatara.io:parent-path new))
    (tuatara.runtime:start-job-thread
     (make-differences-job buffer old new)
     (lambda (job)
       (tuatara.io:refuse-arriving-file (differences-job-old job))
       (tuatara.io:refuse-arriving-file (differences-job-new job))
       (setf (differences-job-rope job)
             (file-differences (differences-job-old job) (differences-job-new job)
                               :cancel (tuatara.runtime:job-cancel job))))
     :name "tuatara differences")
    (pop-to-buffer buffer :other t)))

(defun compare-paths (old new)
  "Show the differences between OLD and NEW (SHOW-DIFFERENCES), or say why
not: one is not there, or they are one. A buffer with changes not saved to
either is said to be left out: the files are compared as they are on disk."
  (let ((missing (find-if-not (lambda (path) (tuatara.io:path-stamp path :follow nil)) (list old new))))
    (cond (missing (message "~a is not there: nothing compared" (abbreviated-path missing)))
          ((equal (tuatara.io:clean-path old) (tuatara.io:clean-path new))
           (message "~a is compared with itself: nothing to show" (abbreviated-path old)))
          (t (show-differences old new)
             (let ((changed (find-if (lambda (path) (let ((buffer (buffer-visiting path)))
                                                      (and buffer (tuatara.buffer:buffer-modified-p buffer))))
                                     (list old new))))
               (when changed
                 (message "~a has changes not saved: compared as it is on disk" (abbreviated-path changed))))))))

(define-command diff-files ()
  "Compare the file shown with another asked for, navigating to it as
\\[find-file] does, and show the differences in a buffer of their own, which
\\`q' closes: the entry on the line in a list of a directory (there
\\<rimu-mode>\\[rimu-diff] compares the two marked); with \\[universal-argument], or where
there is none, both asked for. Unified, as git shows them: the lines of the
first taken out after -, those of the other put in after +, three lines
around each change. Two directories are compared with all in them; of files that
are not text, only that they differ. diff does it in the background, shut in
the sandbox."
  (let* ((old (shown-file-argument "Compare file: "))
         (new (read-native-path (format nil "Compare ~a with: " (tuatara.io:native-file-name old))
                                :initial (abbreviated-path (tuatara.io:parent-path old)) :kind :any)))
    (if new
        (compare-paths old new)
        (message "Nothing to compare with"))))
