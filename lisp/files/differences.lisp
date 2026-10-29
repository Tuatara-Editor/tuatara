;;;; lisp/files/differences.lisp --- two files or folders compared, by diff shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's M-x diff and dired's = do, with GNU diff: M-x diff-files
;;;; compares the file shown with another, = in rimu the two entries marked.
;;;; The differences are shown unified, as git shows them, in diff-mode;
;;;; folders are compared with all in them. diff runs in the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN), seeing only the folders of the two, read only.

(in-package #:tuatara.defaults)

(defvar *differences-limit* (* 64 1024 1024)
  "The most octets of differences shown (64 MB); when there are more, none
are shown, and that is said.")

(defun file-differences (old new &key cancel)
  "The differences between OLD and NEW, two files or two folders, as diff
--unified writes them: the lines of OLD taken out after -, those of NEW put
in after +, three lines around each change. Folders are compared with all in
them, a file only one holds compared with none, links as links, never
followed; of files that are not text, only that they differ; a file and a
folder, the file with the one of its name in the folder. Times are written
in your time zone (SANDBOX-TIME-ZONE). An empty rope
when they are the same. diff, shut in, sees only their folders, read only.
Stopped when CANCEL's car turns true; an error saying why when they cannot be
compared, or the differences are longer than *DIFFERENCES-LIMIT*."
  (flet ((seen (path)
           (let ((real (tuatara.io:real-place path)))
             (if (eq :directory (tuatara.io:path-kind real)) real (tuatara.io:parent-path real)))))
    (multiple-value-bind (output outcome said)
        (tuatara.io:sandbox-run "diff" (append (list "--unified" "--recursive" "--no-dereference")
                                               ;; a file missing on one side counts as empty only between two folders
                                               (and (eq :directory (tuatara.io:path-kind old))
                                                    (eq :directory (tuatara.io:path-kind new))
                                                    (list "--new-file"))
                                               (list "--" (tuatara.io:real-place old) (tuatara.io:real-place new)))
                                :output-limit *differences-limit* :cancel cancel :ended-well '(0 1)
                                :environment (sandbox-time-zone)
                                :directories (remove-duplicates (list (seen old) (seen new)) :test #'string=))
      (unless (eq outcome :done)
        (error "~a" (tuatara.io:sandbox-outcome-words outcome said :label "diff")))
      (tuatara.text:octets->rope (coerce output 'tuatara.text:octets)))))

(defstruct (differences-job (:include tuatara.runtime:job) (:constructor make-differences-job (buffer old new))
                            (:copier nil))
  "The differences between the files OLD and NEW being found for BUFFER, a
job; ROPE, those found."
  old new rope)

(setf (documentation 'differences-job-p 'function) "True when OBJECT is a DIFFERENCES-JOB.")

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
                         (if (and rope (zerop (tuatara.text:rope-bytes rope)))
                             (format nil "No differences: ~a and ~a are the same.~%" (abbreviated-path old) (abbreviated-path new))
                             rope)
                         (format nil "~~a and ~a could not be compared: ~~a~~%" (cl-ppcre:regex-replace-all "~" (abbreviated-path new) "~~")))))

(defun show-differences (old new)
  "Show the differences between OLD and NEW, two files or folders, in a buffer
of their own in another window, found in the background (FILE-DIFFERENCES);
\\`q' closes it. One showing these two before is made afresh; one showing
two others of the same names is left as it is."
  (let* ((name (format nil "*Diff: ~a and ~a*" (tuatara.io:native-file-name old) (tuatara.io:native-file-name new)))
         (before (tuatara.buffer:find-buffer name))
         (buffer (fresh-made-buffer (if (and before (not (equal (tuatara.buffer:buffer-local before 'compared) (list old new))))
                                        (tuatara.buffer:unique-buffer-name name)
                                        name)
                                    'diff-mode
                                    (format nil "Comparing ~a with ~a...~%" (abbreviated-path old) (abbreviated-path new)))))
    (setf (tuatara.buffer:buffer-local buffer 'compared) (list old new)
          (tuatara.buffer:buffer-local buffer :directory) (tuatara.io:parent-path new))
    (tuatara.runtime:start-job-thread
     (make-differences-job buffer old new)
     (lambda (job)
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
around each change. Two folders are compared with all in them; of files that
are not text, only that they differ. diff does it in the background, shut in
the sandbox."
  (let* ((old (shown-file-argument "Compare file: "))
         (new (read-native-path (format nil "Compare ~a with: " (tuatara.io:native-file-name old))
                                :initial (abbreviated-path (tuatara.io:parent-path old)) :kind :any)))
    (if new
        (compare-paths old new)
        (message "Nothing to compare with"))))
