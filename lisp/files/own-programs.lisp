;;;; lisp/files/own-programs.lisp --- Programs of your own run shut in on copies of files, and what they report.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A program of your own that reads files a package hands it (a face
;;;; comparer, say) runs shut in the sandbox, unlike the desktop's programs
;;;; (external.lisp): on private copies of the files named by kind and
;;;; number, an AppImage unpacked once into the cache and run from there
;;;; read only (RUN-ON-COPIES).

(in-package #:tuatara.defaults)

(defun user-program-path (name)
  "The full path of the program NAME, a setting's value, or NIL when it is
not there: a path (one with a /, ~ for your home folder) taken as it is, else
a name looked for where the sandbox finds programs (SANDBOX-FIND-PROGRAM)."
  (if (find #\/ name)
      (let ((path (tuatara.io:native (tuatara.io:expand-path name))))
        (and (eq :regular (tuatara.io:path-kind path)) path))
      (tuatara.io:sandbox-find-program name)))

(defun appimage-p (path)
  "True when the file at PATH is an AppImage: an ELF program with the AppImage type 2 mark."
  (let ((head (ignore-errors (tuatara.io:file-head path 12))))
    (and head (>= (length head) 11)
         (tuatara.io:octets-at-p head '(#x7f 69 76 70))
         (= (aref head 8) 65) (= (aref head 9) 73) (= (aref head 10) 2))))

(defun unpacked-appimage (program cancel)
  "The folder, ending in /, where the AppImage PROGRAM is unpacked, unpacking
it the first time: run with --appimage-extract, outside the sandbox, as it is
your own program, into appimages/NAME/ in the cache, NAME its file's, once for
each version of it, the older ones deleted. CANCEL, a cons, a job's, is
heeded once it is done."
  (let* ((stamp (tuatara.io:path-stamp program))
         (root (private-cache-directory
                (format nil "appimages/~a/" (attachment-file-name (tuatara.io:native-file-name program) "program"))))
         (folder (tuatara.io:path-join root (format nil "~d-~d/" (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp))))
         (unpacked (concatenate 'string folder "squashfs-root/")))
    (unless (eq :regular (tuatara.io:path-kind (concatenate 'string unpacked "AppRun")))
      (dolist (old (ignore-errors (tuatara.io:directory-entries root)))
        (ignore-errors (tuatara.io:delete-tree (tuatara.io:path-join root (first old)))))
      (tuatara.io:ensure-private-directory folder)
      (let ((ended-well (nth-value 1 (tuatara.io:program-output (list program "--appimage-extract")
                                                                :capture nil :seconds 600 :directory folder))))
        (when (car cancel) (ignore-errors (tuatara.io:delete-tree folder)) (tuatara.runtime:job-check-stop))
        (unless (and ended-well (eq :regular (tuatara.io:path-kind (concatenate 'string unpacked "AppRun"))))
          (ignore-errors (tuatara.io:delete-tree folder))
          (error "~a could not be unpacked" (abbreviated-path program)))))
    unpacked))

(defun shut-in-program (program cancel)
  "The program at PROGRAM, a full path (USER-PROGRAM-PATH), made ready to run
shut in, as two values: the path to run, and what the sandbox is shown of it,
read only, as SANDBOX-RUN's DIRECTORIES take it. An AppImage runs from its
unpacked folder (UNPACKED-APPIMAGE, CANCEL heeded), that folder shown; a
program outside the system's own folders of programs has that file alone
shown; one in them, nothing."
  (cond ((appimage-p program)
         (let ((unpacked (unpacked-appimage program cancel)))
           (values (concatenate 'string unpacked "AppRun") (list (string-right-trim "/" unpacked)))))
        ((equal program (tuatara.io:sandbox-find-program (tuatara.io:native-file-name program)))
         (values program '()))
        (t (values program (list program)))))

(defvar *copies-place* "/tmp/files/"
  "Where a program run on copies of files (RUN-ON-COPIES) sees the copies.")

(defvar *copy-name-scanner*
  (cl-ppcre:create-scanner (format nil "~a[A-Za-z]+-[0-9]+(?:\\.[A-Za-z0-9]{1,5})?" (cl-ppcre:quote-meta-chars *copies-place*)))
  "What finds the name of a copy (COPY-FILE-NAME) in *COPIES-PLACE* in a
program's words.")

(defun copy-file-name (kind n path)
  "The name the copy of the file at PATH goes by: KIND, a word of letters,
and N, with PATH's ending when it is a short one of letters and digits
\(master-1.jpg), so that nothing else of PATH reaches the program."
  (let ((ending (tuatara.io:native-file-type path)))
    (format nil "~a-~d~@[.~(~a~)~]" kind n
            (and ending (<= 1 (length ending) 5) (every #'alphanumericp ending) ending))))

(defun copies-original-names (text names)
  "TEXT with each copy's name in it, as the program run on the copies saw it,
written as its file's own path, ~ for your home folder; NAMES is the table of
copies RUN-ON-COPIES gives back."
  (cl-ppcre:regex-replace-all
   *copy-name-scanner* text
   (lambda (match)
     (let ((path (gethash match names)))
       (if path
           (abbreviated-path path)
           ;; a copy with no ending, followed by a full stop and letters
           (let* ((dot (position #\. match :start (length *copies-place*)))
                  (path (and dot (gethash (subseq match 0 dot) names))))
             (if path (concatenate 'string (abbreviated-path path) (subseq match dot)) match)))))
   :simple-calls t))

(defun copies-output-json (output)
  "OUTPUT, the octets a program printed, read as JSON from its first {, as an
AppImage may print a line of its own before; NIL when there is none."
  (let* ((text (tuatara.text:lenient-string output))
         (start (position #\{ text)))
    (and start (ignore-errors (tuatara.text:json-read text :start start)))))

(defun run-on-copies (program groups arguments &rest keys &key cancel (memory tuatara-user:*sandbox-memory*) directories
                      &allow-other-keys)
  "Run PROGRAM, a full path (USER-PROGRAM-PATH), shut in on private copies of
files, as SANDBOX-RUN runs a program (KEYS are its own), and delete the
copies once it is done. GROUPS are the files, each (KIND . PATHS): each is
copied into a new folder of its own in sandbox-copies/ in the cache
\(NEW-PRIVATE-FOLDER), named by KIND and its place in PATHS from 1
\(COPY-FILE-NAME), and the program sees them in *COPIES-PLACE*, read only.
ARGUMENTS is a function given one argument for each group, the list of its
copies' names as the program sees them, returning the program's arguments. PROGRAM is made
ready as SHUT-IN-PROGRAM makes it, an AppImage unpacked first; DIRECTORIES
are shown to it besides. The memory all the sandbox's programs may take
together (*SANDBOX-TOTAL-MEMORY*) is raised to MEMORY for the run. Returns
four values: what it printed, how it went and what it said, as SANDBOX-RUN
gives them, and a table of each copy's name as the program sees it to its
file's path (COPIES-ORIGINAL-NAMES). Call it from a job's thread, CANCEL
being the job's."
  (multiple-value-bind (run shown) (shut-in-program program cancel)
    (tuatara.runtime:job-check-stop)
    (let ((folder (new-private-folder (private-cache-directory "sandbox-copies/")))
          (names (make-hash-table :test 'equal)))
      (unwind-protect
           (let ((seen (loop for (kind . paths) in groups
                             collect (loop for path in paths
                                           for n from 1
                                           for name = (copy-file-name kind n path)
                                           for copy = (concatenate 'string *copies-place* name)
                                           do (unless (tuatara.io:copy-regular-file path (concatenate 'string folder name)
                                                                                    :cancel cancel)
                                                (tuatara.runtime:job-check-stop)
                                                (error "~a could not be copied for ~a" (abbreviated-path path)
                                                       (tuatara.io:native-file-name program)))
                                              (setf (gethash copy names) path)
                                           collect copy)))
                 (tuatara-user:*sandbox-total-memory* (and tuatara-user:*sandbox-total-memory* memory
                                                           (max tuatara-user:*sandbox-total-memory* memory))))
             (multiple-value-bind (output outcome said)
                 (apply #'tuatara.io:sandbox-run run (apply arguments seen)
                        :directories (append (list (cons (string-right-trim "/" folder) (string-right-trim "/" *copies-place*)))
                                             shown directories)
                        :memory memory :cancel cancel
                        (alexandria:remove-from-plist keys :directories :memory :cancel))
               (values output outcome said names)))
        (delete-private-folder folder)))))

(defun run-on-copies-failure (label outcome said names seconds-setting)
  "Why a program run on copies (RUN-ON-COPIES) gave nothing to show, as a
sentence beginning with LABEL, the program's name: as SANDBOX-OUTCOME-WORDS
says it, but for a program that failed or gave no report, all it said
\(SAID), its copies' names written as their files' (NAMES); OUTCOME is how
the run went, SECONDS-SETTING the setting's symbol holding its seconds."
  (case outcome
    ((:unavailable :timeout :cancelled :too-much)
     (tuatara.io:sandbox-outcome-words outcome said :label label :seconds-setting seconds-setting))
    (t (copies-original-names (string-trim '(#\Space #\Newline)
                                           (if (plusp (length said)) said (format nil "~a ended without a report" label)))
                              names))))

(defun run-on-copies-for-report (program groups arguments &rest keys &key label missing seconds-setting &allow-other-keys)
  "Run the program PROGRAM names, a name or a path (USER-PROGRAM-PATH; an
error saying MISSING when it is not there), on private copies of files, as
RUN-ON-COPIES does with GROUPS, ARGUMENTS and the rest of KEYS, for at most
the seconds SECONDS-SETTING, a setting's symbol, holds; and read the JSON
report it prints (COPIES-OUTPUT-JSON). Returns three values: the report, or
NIL; the table of the copies' names to their files' paths; and with no
report, why, as a sentence beginning with LABEL (RUN-ON-COPIES-FAILURE).
Call it from a job's thread, the job's CANCEL given; the job stopping ends it."
  (let ((path (or (user-program-path program) (error "~a" missing))))
    (multiple-value-bind (output outcome said names)
        (apply #'run-on-copies path groups arguments :seconds (symbol-value seconds-setting)
               (alexandria:remove-from-plist keys :label :missing :seconds-setting))
      (tuatara.runtime:job-check-stop)
      (let ((report (and (eq outcome :done) (copies-output-json output))))
        (values report names (and (null report) (run-on-copies-failure label outcome said names seconds-setting)))))))

(defun likelihood-face (percent)
  "The face a likelihood of PERCENT per cent is shown in: :info from 80 (very
likely and above), :warning from 45 (likely down to an even chance), :error
below."
  (cond ((>= percent 80) :info)
        ((>= percent 45) :warning)
        (t :error)))
