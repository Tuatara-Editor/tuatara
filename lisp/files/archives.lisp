;;;; lisp/files/archives.lisp --- tar archives: files and folders packed into one, one listed or unpacked, by tar shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's dired packs the files marked into an archive (c) and unpacks
;;;; one (Z), with GNU tar, compressed as the archive's name ends; M-x
;;;; list-archive (C-c e) shows what one holds, unpacking nothing. tar runs in
;;;; the sandbox (TUATARA.IO:SANDBOX-RUN): it sees what it packs read only and
;;;; writes only in the folder the archive goes to or is unpacked into, so an
;;;; archive from elsewhere, whatever names and links it holds, puts nothing
;;;; outside that folder. An archive is written under a passing name and given
;;;; its own only once it is whole.

(in-package #:tuatara.defaults)

(defparameter *archive-kinds*
  '((:tar nil ".tar") (:gzip "--gzip" ".tar.gz" ".tgz") (:bzip2 "--bzip2" ".tar.bz2" ".tbz2" ".tbz")
    (:xz "--xz" ".tar.xz" ".txz") (:zstd "--zstd" ".tar.zst" ".tzst"))
  "The kinds of tar archive made and unpacked: each the kind of its first
octets (TUATARA.IO:OCTETS-KIND), tar's option compressing it, or NIL for
none, and the endings of the names an archive of that kind is made with.")

(defvar *archive-seconds* (* 24 60 60)
  "The seconds tar may take to make or unpack one archive before it is ended:
a day, for the largest; \\[stop-job] ends it sooner.")

(defvar *archive-free-reserve* (* 1024 1024 1024)
  "The octets left free on the drive an archive is unpacked onto (1 GB):
unpacking stops before the drive has less, so that an archive holding far
more than it takes up cannot fill the drive.")

(defun archive-kind-of-name (path)
  "The entry of *ARCHIVE-KINDS* the name of PATH ends as, and as a second
value the ending; NIL for none."
  (let ((name (string-downcase (tuatara.io:native-file-name path))))
    (dolist (kind *archive-kinds*)
      (dolist (ending (cddr kind))
        (when (uiop:string-suffix-p name ending)
          (return-from archive-kind-of-name (values kind ending)))))))

(defun archive-stem (path)
  "The name of the archive at PATH without its ending (.tar.gz and the
others of *ARCHIVE-KINDS*), what its contents are unpacked into by standard."
  (let ((name (tuatara.io:native-file-name path))
        (ending (nth-value 1 (archive-kind-of-name path))))
    (if (and ending (> (length name) (length ending)))
        (subseq name 0 (- (length name) (length ending)))
        name)))

(defvar *archive-listing-limit* (* 64 1024 1024)
  "The most octets of an archive's list of contents shown (64 MB); a longer
one is not shown, and that is said.")

(defun tar-environment ()
  "What tar is given in its environment besides the sandbox's own: your time
zone (SANDBOX-TIME-ZONE), and xz held to half the memory a program shut in may
take, with as many threads as fit in it."
  (append (sandbox-time-zone)
          (let ((memory tuatara-user:*sandbox-memory*))
            (and memory (list (list "XZ_DEFAULTS" (format nil "--memlimit-compress=~dMiB" (max 64 (floor memory (* 2 1024 1024))))))))))

(defun tar-failure-words (said)
  "What tar SAID on its error output, its warnings that go on (names made
safe by taking a leading / or ../ away) and its closing line left out, so
that the error that made it fail comes first."
  (let ((lines (remove-if (lambda (line) (or (search "Removing leading" line) (search "Exiting with failure status" line)))
                          (uiop:split-string (or said "") :separator '(#\Newline)))))
    (if (some (lambda (line) (plusp (length line))) lines)
        (format nil "~{~a~^~%~}" (remove "" lines :test #'string=))
        said)))

(defun run-tar (arguments &key cancel directories writable files output-limit file-limit)
  "Run tar with ARGUMENTS shut in (TUATARA.IO:SANDBOX-RUN), shown DIRECTORIES
read only and WRITABLE to write in, FILES as it has them, writing no file
larger than FILE-LIMIT octets (NIL: no limit), its environment
TAR-ENVIRONMENT's; stopped when CANCEL's car turns true. What it printed, as
text, up to OUTPUT-LIMIT octets (NIL: let go); an error saying why when it
does not end well (TAR-FAILURE-WORDS)."
  (multiple-value-bind (output outcome said)
      (tuatara.io:sandbox-run "tar" arguments :seconds *archive-seconds* :output-limit output-limit :file-limit file-limit
                                              :cancel cancel :directories directories :writable writable :files files
                                              :environment (tar-environment))
    (unless (eq outcome :done)
      (error "~a" (tuatara.io:sandbox-outcome-words outcome (tar-failure-words said) :label "tar"
                                                    :seconds-setting '*archive-seconds*)))
    (and output-limit (tuatara.text:octets-string output))))

(defun check-compressor (kind)
  "An error when the program compressing archives of KIND, an entry of
*ARCHIVE-KINDS*, is not installed where tar finds it shut in."
  (let ((program (and (second kind) (subseq (second kind) 2))))
    (when (and program (not (tuatara.io:sandbox-find-program program)))
      (error "~a is not installed: install it for archives compressed so" program))))

(defun archive-reading-arguments (archive)
  "tar's arguments reading the archive ARCHIVE, compressed with gzip, bzip2,
xz or zstd or not at all, told by its first octets (FILE-KIND), never by
its name: --file, where it is seen in the sandbox, and how it is compressed.
An error when it is no tar archive."
  (let* ((by-octets (nth-value 1 (file-kind archive)))
         (kind (or (find by-octets *archive-kinds* :key #'first)
                   ;; tar archives of before POSIX have no mark to be told by
                   (and (null by-octets) (eq :tar (first (archive-kind-of-name archive))) (first *archive-kinds*))
                   (error "it is no tar archive, compressed or not"))))
    (check-compressor kind)
    (list* "--file" (tuatara.io:path-join (tuatara.io:real-place (tuatara.io:parent-path archive))
                                          (tuatara.io:native-file-name archive))
           (and (second kind) (list (second kind))))))

(defun write-archive (paths archive &key cancel replace)
  "Pack PATHS, files and directories with all in them, links as links, into
the tar archive ARCHIVE, compressed as its name ends (*ARCHIVE-KINDS*); each
kept inside with its path below the deepest directory all of them are in. It
is written under a passing name beside ARCHIVE and given that name once whole,
what is there already moved to the trash first with REPLACE; nothing is left
when it fails or CANCEL's car turns true. An error saying why when it does not
work."
  (let* ((kind (or (archive-kind-of-name archive)
                   (error "~a is not named as a tar archive is" (tuatara.io:native-file-name archive))))
         ;; one in a folder also given goes in with the folder, once
         (paths (remove-if (lambda (path) (some (lambda (other) (and (string/= path other) (tuatara.io:path-within-p path other)))
                                                paths))
                           paths))
         (root (paths-common-directory paths))
         (folder (tuatara.io:parent-path archive))
         (passing (tuatara.io:path-join folder (format nil ".~a.~a.part" (tuatara.io:native-file-name archive)
                                                     (tuatara.io:random-name))))
         (names (format nil "~{~a~c~}"
                        (loop for path in paths
                              collect (subseq (string-right-trim "/" path) (length root))
                              collect (code-char 0)))))
    (handler-bind ((serious-condition (lambda (e) (declare (ignore e)) (ignore-errors (tuatara.io:remove-file passing)))))
      (run-tar (append (list "--create" "--file" (tuatara.io:path-join (tuatara.io:real-place folder)
                                                                       (tuatara.io:native-file-name passing)))
                       (and (second kind) (list (second kind)))
                       (list "--directory" (tuatara.io:real-place root)
                             "--null" "--verbatim-files-from" "--files-from" "/tmp/names"))
               :cancel cancel
               :directories (remove-duplicates (cons root (mapcar #'tuatara.io:parent-path paths)) :test #'string=)
               :writable (list folder)
               :files (list (cons "/tmp/names" names)))
      (when (and cancel (car cancel))
        (error "stopped"))
      (when (and replace (tuatara.io:path-stamp archive :follow nil))
        (tuatara.io:trash-path archive))
      (tuatara.io:rename-path passing archive))
    archive))

(defun extract-archive (archive directory &key cancel)
  "Unpack the tar archive ARCHIVE, compressed with gzip, bzip2, xz or zstd or
not at all (told by its first octets), into DIRECTORY, made when it is not
there: its files yours, their modes as your umask has them, none written
outside DIRECTORY whatever names and links the archive holds, and those
there already left as they are. Stopped when CANCEL's car turns true, or
before the drive has less than *ARCHIVE-FREE-RESERVE* free, said so. An
error saying why when it does not work; what was unpacked before stays."
  (let* ((reading (archive-reading-arguments archive))
         (directory (tuatara.io:slash-ended directory))
         (made (not (tuatara.io:path-stamp directory :follow nil)))
         (unpacked nil))
    (tuatara.io:ensure-directories directory)
    (unwind-protect (progn (unpack-archive reading archive directory cancel)
                           (setf unpacked t))
      ;; a folder made for it that nothing went into is taken away again
      (when (and made (not unpacked))
        (ignore-errors (sb-posix:rmdir (string-right-trim "/" directory)))))
    directory))

(defun unpack-archive (reading archive directory cancel)
  "Unpack ARCHIVE, read with tar's arguments READING, into DIRECTORY, there
already, as EXTRACT-ARCHIVE says: stopped when CANCEL's car turns true, or
before the drive has less than *ARCHIVE-FREE-RESERVE* free, said so."
  (flet ((room-left () (- (or (tuatara.io:free-bytes directory) most-positive-fixnum) *archive-free-reserve*))
         (full ()
           (error "unpacking stopped: ~a has less than ~a free (*archive-free-reserve*)"
                  (abbreviated-path directory) (tuatara.io:octets-words *archive-free-reserve*))))
    (unless (plusp (room-left)) (full))
    ;; tar is stopped by the watch below once the drive is nearly full, and
    ;; held to files no larger than the room there was at its start
    (let* ((stop (list nil)) (filled nil) (done nil)
           (watch (tuatara.runtime:make-guarded-thread
                   (lambda ()
                     (loop until done
                           do (cond ((and cancel (car cancel)) (setf (car stop) t))
                                    ((not (plusp (room-left))) (setf filled t (car stop) t)))
                              (sleep 0.2)))
                   :name "tuatara archive room")))
      (unwind-protect
           (handler-case (run-tar (append (list "--extract") reading
                                          (list "--directory" (tuatara.io:real-place directory)
                                                "--no-same-owner" "--no-same-permissions" "--skip-old-files"
                                                "--delay-directory-restore"))
                                  :cancel stop
                                  :directories (list (tuatara.io:parent-path archive))
                                  :writable (list directory)
                                  :file-limit (room-left))
             (error (e)
               (if (or filled (< (room-left) (* 64 1024 1024))) (full) (error e))))
        (setf done t)
        (sb-thread:join-thread watch :default nil)))))

(defun archive-listing (archive &key cancel)
  "What the tar archive ARCHIVE holds, as tar --list --verbose writes it, a
line each: its kind and permissions, owner and group, size, time to the
second and name, where a link leads; a character that cannot be shown
written as \\ and its code. Nothing is unpacked; tar, shut in, sees only the
archive's folder, read only. Stopped when CANCEL's car turns true; an error
saying why when it cannot be read, or the list is longer than
*ARCHIVE-LISTING-LIMIT*."
  (run-tar (append (list "--list" "--verbose" "--full-time") (archive-reading-arguments archive))
           :cancel cancel :directories (list (tuatara.io:parent-path archive)) :output-limit *archive-listing-limit*))

;;; Shown in a buffer

(defvar *archive-listings* '()
  "The buffers listing what an archive holds.")

(defstruct (archive-listing-job (:include tuatara.runtime:job) (:constructor make-archive-listing-job (buffer path))
                                (:copier nil))
  "What the archive at PATH holds being listed for BUFFER, a job; TEXT, the list made."
  path text)

(setf (documentation 'archive-listing-job-p 'function) "True when OBJECT is an ARCHIVE-LISTING-JOB.")

(tuatara.runtime:define-job-words archive-listing-job
  :doing "Listing the archive"
  :description (format nil "Listing what ~a holds" (tuatara.io:native-file-name (archive-listing-job-path job)))
  :verb "list the archive"
  :left-words "nothing listed"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-take-in ((job archive-listing-job))
  "Show what the archive holds, or say why it could not be listed."
  (show-reading-result job (archive-listing-job-path job) (archive-listing-job-text job)
                       "~*~a~%"))

(define-command list-archive ()
  "Show what the tar archive shown holds, in a buffer of its own, which \\`q'
closes: the entry on the line in a list of a directory (\\<rimu-mode>\\[list-archive] there);
with \\[universal-argument], or where there is none, asked for. A line each, as
tar --list --verbose writes it: kind and permissions, owner and group, size,
time to the second, name, and where a link leads. gzip, bzip2, xz and zstd
archives are told by what they hold. Nothing is unpacked: tar reads it in
the background, shut in the sandbox."
  (let* ((path (shown-file-argument "List archive: "))
         (stamp (tuatara.io:path-stamp path)))
    (cond ((null stamp) (message "~a is not there" (abbreviated-path path)))
          ((not (eq (tuatara.io:stamp-kind stamp) :regular)) (message "~a is not a regular file: nothing listed" (abbreviated-path path)))
          (t (let ((name (tuatara.io:native-file-name path)))
               (show-buffer
                (reading-buffer '*archive-listings* (lambda (b) (tuatara.buffer:buffer-local b 'archive-listing)) path
                                (format nil "*Archive: ~a*" name) (format nil "Listing what ~a holds...~%" name) 'special-mode
                                (lambda (buffer)
                                  (setf (tuatara.buffer:buffer-local buffer 'archive-listing) path)
                                  (tuatara.runtime:start-job-thread
                                   (make-archive-listing-job buffer path)
                                   (lambda (job)
                                     (setf (archive-listing-job-text job)
                                           (archive-listing (archive-listing-job-path job) :cancel (tuatara.runtime:job-cancel job))))
                                   :name "tuatara archive listing")))))))))
