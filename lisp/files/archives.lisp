;;;; lisp/files/archives.lisp --- Archives: files and folders packed into a tar one, any listed or unpacked, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's dired packs the files marked into an archive (c) and unpacks
;;;; one (Z), with GNU tar, compressed as the archive's name ends; M-x
;;;; rimu-archive-list (C-c e) shows what one holds, unpacking nothing.
;;;; Zip, 7z and rar archives are listed and unpacked by 7-Zip where it is
;;;; installed, else by bsdtar (libarchive's tar), as ISO 9660 and cpio ones
;;;; are; tar ones stay with GNU tar, which lists their times to the second.
;;;; The kind is told by an archive's first octets, never its name, and the
;;;; program is held to it. Every program runs in the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN): it sees what it packs read only and writes only
;;;; in the folder the archive goes to or, to unpack one, in a new empty folder
;;;; of its own, from which what it made is moved into place without replacing
;;;; anything; so an archive from elsewhere, whatever names and links it holds,
;;;; changes nothing outside what it adds. An archive is written under a
;;;; passing name and given its own only once it is whole. A password goes to
;;;; the program on its standard input, never among its arguments, which other
;;;; programs can read.

(in-package #:tuatara.defaults)

(defparameter *archive-kinds*
  '((:tar :programs (:tar) :endings (".tar"))
    (:gzip :programs (:tar) :option "--gzip" :endings (".tar.gz" ".tgz"))
    (:bzip2 :programs (:tar) :option "--bzip2" :endings (".tar.bz2" ".tbz2" ".tbz"))
    (:xz :programs (:tar) :option "--xz" :endings (".tar.xz" ".txz"))
    (:zstd :programs (:tar) :option "--zstd" :endings (".tar.zst" ".tzst"))
    (:zip :programs (:7zz :bsdtar) :words "zip" :format "ZIP" :type "zip" :endings (".zip"))
    (:7z :programs (:7zz :bsdtar) :words "7z" :format "7-Zip" :type "7z" :endings (".7z"))
    (:rar :programs (:7zz :bsdtar) :words "rar" :format "RAR" :type "Rar" :endings (".rar"))
    (:iso :programs (:bsdtar) :words "ISO 9660" :format "ISO9660" :endings (".iso"))
    (:cpio :programs (:bsdtar) :words "cpio" :format "cpio" :endings (".cpio")))
  "The kinds of archive listed and unpacked, each the kind of its first octets
\(TUATARA.IO:OCTETS-KIND) and a property list: :PROGRAMS, the programs that
read it, the first installed that can taken (ARCHIVE-PROGRAM), GNU tar (:TAR),
which also makes the archives it reads, 7-Zip (:7ZZ) or bsdtar (:BSDTAR);
:OPTION, tar's option compressing it; :WORDS, what a message calls it;
:FORMAT, how bsdtar names the format it read (in its own words, the start of
them), and :TYPE, 7-Zip's name of it, each held to that format; and
:ENDINGS, the endings of its names. A Word document, an EPUB book or a Java
package is read as the zip archive it is (*ZIP-KINDS*).")

(defparameter *zip-kinds* '(:zip :docx :xlsx :pptx :odt :ods :odp :epub :apk :jar)
  "The kinds of file (TUATARA.IO:OCTETS-KIND) that are zip archives, listed
and unpacked as one.")

(defvar *archive-seconds* (* 24 60 60)
  "The seconds tar, 7-Zip or bsdtar may take to make, list or unpack one
archive before it is ended: a day, for the largest; \\[stop-job] ends it
sooner.")

(defparameter *archive-test-seconds* (* 60 60)
  "The seconds a password is tried on all of a locked archive before it is
unpacked (EXTRACT-ARCHIVE) may take: an hour.")

(defvar *archive-free-reserve* (* 1024 1024 1024)
  "The octets left free on the drive an archive is unpacked onto (1 GB):
unpacking stops before the drive has less, so that an archive holding far
more than it takes up cannot fill the drive.")

(defun archive-kind-of-name (path &key made)
  "The entry of *ARCHIVE-KINDS* the name of PATH ends as, and as a second
value the ending; NIL for none. With MADE, only the kinds tar makes."
  (let ((name (string-downcase (tuatara.io:native-file-name path))))
    (dolist (kind *archive-kinds*)
      (when (or (not made) (equal '(:tar) (getf (rest kind) :programs)))
        (dolist (ending (getf (rest kind) :endings))
          (when (uiop:string-suffix-p name ending)
            (return-from archive-kind-of-name (values kind ending))))))))

(defun archive-stem (path)
  "The name of the archive at PATH without its ending (.tar.gz, .zip and the
others of *ARCHIVE-KINDS*, or the last of any other), what its contents are
unpacked into by standard; with no ending to take away, the name and
-extracted."
  (let* ((name (tuatara.io:native-file-name path))
         (ending (nth-value 1 (archive-kind-of-name path)))
         (stem (if (and ending (> (length name) (length ending)))
                   (subseq name 0 (- (length name) (length ending)))
                   (tuatara.io:native-file-stem path))))
    (if (string= stem name) (format nil "~a-extracted" name) stem)))

(defvar *archive-listing-limit* (* 64 1024 1024)
  "The most octets of an archive's list of contents shown (64 MB); a longer
one is not shown, and that is said.")

(defun archive-environment ()
  "What the programs reading archives are given in their environment besides
the sandbox's own: your time zone (SANDBOX-TIME-ZONE), and xz held to half
the memory a program shut in may take, with as many threads as fit in it."
  (append (sandbox-time-zone)
          (let ((memory tuatara-user:*sandbox-memory*))
            (and memory (list (list "XZ_DEFAULTS" (format nil "--memlimit-compress=~dMiB" (max 64 (floor memory (* 2 1024 1024))))))))))

;;; 7-Zip

(defparameter *seven-zip-names* '("7zz" "7z" "7za")
  "The names 7-Zip's program has, looked for in this order: 7zz on Debian,
Ubuntu and Arch, 7z or 7za on Fedora. An older program of those names that
is not 7-Zip's own (p7zip, 16.02 and before) is not taken.")

(sb-ext:defglobal **seven-zip** nil
  "7-Zip as last found (SEVEN-ZIP): (PATH VERSION FORMATS), or NIL before.")

(defun seven-zip-said (text)
  "7-Zip's version (a list of two numbers) and the formats it reads (their
names, as its i command lists them) in TEXT, what that command printed; NIL
when TEXT is not 7-Zip's own, at version 21 or later."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "(?m)^7-Zip (?:\\[64\\] )?(\\d+)\\.(\\d+)" text)
    (when (and match (not (search "p7zip" text)))
      (let ((version (list (parse-integer (aref groups 0)) (parse-integer (aref groups 1))))
            (formats (let ((start (search (format nil "~%Formats:") text)))
                       (and start
                            (loop for line in (rest (uiop:split-string (subseq text (1+ start)) :separator '(#\Newline)))
                                  until (string= "" (string-trim " " line))
                                  for words = (rest (tuatara.text:split-words line))
                                  for name = (find-if-not (lambda (word) (find #\. word)) words)
                                  when name collect name)))))
        (and (>= (first version) 21) (list version formats))))))

(defun seven-zip ()
  "7-Zip where it is installed for programs shut in, as (PATH VERSION
FORMATS) (SEVEN-ZIP-SAID); NIL when it is not. Asked of it shut in once for
each program found, and kept."
  (let ((path (some #'tuatara.io:sandbox-find-program *seven-zip-names*)))
    (cond ((null path) nil)
          ((equal path (first **seven-zip**)) **seven-zip**)
          (t (let ((said (multiple-value-bind (output outcome)
                             (tuatara.io:sandbox-run path (list "i") :seconds 10 :output-limit (* 1024 1024))
                           (and (eq outcome :done) (seven-zip-said (tuatara.text:octets-string output))))))
               (setf **seven-zip** (and said (cons path said))))))))

(defun seven-zip-reads-p (kind)
  "True when 7-Zip is installed and reads archives of KIND, an entry of
*ARCHIVE-KINDS* (its RAR reader may be left out, as Fedora's and Debian's
own builds leave it)."
  (let ((found (seven-zip)))
    (and found (member (getf (rest kind) :type) (third found) :test #'string-equal) t)))

(defun archive-program (kind)
  "The program that reads archives of KIND, an entry of *ARCHIVE-KINDS*: the
first of its :PROGRAMS installed for programs shut in that reads that kind,
:TAR, :7ZZ or :BSDTAR; an error naming what to install when there is none."
  (or (find-if (lambda (program)
                 (ecase program
                   (:tar t)
                   (:7zz (seven-zip-reads-p kind))
                   (:bsdtar (tuatara.io:sandbox-find-program "bsdtar"))))
               (getf (rest kind) :programs))
      (refuse "bsdtar is not installed: ~a" (outside-program-install-words :archives))))

(defun program-path (program)
  "The program PROGRAM (:TAR, :7ZZ, :BSDTAR) is run as."
  (ecase program
    (:tar "tar")
    (:bsdtar "bsdtar")
    (:7zz (first (seven-zip)))))

;;; Running them

(defun archive-said-words (said)
  "What the program reading an archive SAID on its error output, its warnings
that go on (names made safe by taking a leading / or ../ away), its closing
line and its asking for a password left out, so that the error that made it
fail comes first."
  (let* ((said (cl-ppcre:regex-replace-all "(?m)Enter passphrase:|Enter password[^:\\n]*:|: (?:Unknown error -1|Success)$"
                                           (or said "") ""))
         (lines (remove-if (lambda (line)
                             (or (search "Removing leading" line) (search "Exiting with failure status" line)
                                 (search "Error exit delayed from previous errors" line)))
                           (uiop:split-string said :separator '(#\Newline)))))
    (if (some (lambda (line) (plusp (length line))) lines)
        (format nil "~{~a~^~%~}" (remove "" (mapcar (lambda (line) (string-trim " " line)) lines) :test #'string=))
        said)))

(defun archive-failure-words (program kind outcome said password)
  "Why PROGRAM (:TAR, :7ZZ, :BSDTAR) reading or unpacking an archive of KIND,
an entry of *ARCHIVE-KINDS*, did not end well, as a sentence, from OUTCOME and
what was SAID, as TUATARA.IO:SANDBOX-RUN gives them: with PASSWORD given, a
wrong one said so; an archive locked with a password that bsdtar cannot open
said so by its kind, with 7-Zip to install for it; and one holding what the
program cannot read said so."
  (let ((words (archive-said-words said))
        (kind-words (getf (rest kind) :words))
        (label (string-downcase (program-path program))))
    (cond ((not (eq outcome :failed))
           (sandbox-outcome-words outcome words :label label :seconds-setting '*archive-seconds*))
          ((or (search "Couldn't read passphrase" words) (search "Wrong password" words)
               (and (eq program :7zz) (search "E_FAIL" words)))
           (if password "the password is not right" "it is locked with a password"))
          ((search "encrypted, but currently not supported" words)
           (format nil "it is locked with a password: bsdtar opens no ~a archive so locked; 7-Zip does (~a)"
                   kind-words (outside-program-install-words :seven-zip)))
          ((and kind-words (or (search "nsupported" words) (search "not supported" words)))
           (format nil "this ~a archive holds what ~a cannot read: ~a" kind-words label
                   (escape-controls (first (uiop:split-string words :separator '(#\Newline))))))
          (t (sandbox-outcome-words outcome words :label label)))))

(defun run-archive-program (program kind arguments &key cancel directories writable files output-limit (output-kept t)
                                                         file-limit password (seconds *archive-seconds*))
  "Run PROGRAM (:TAR, :7ZZ, :BSDTAR, as ARCHIVE-PROGRAM chooses it for KIND,
an entry of *ARCHIVE-KINDS*) with ARGUMENTS shut in (TUATARA.IO:SANDBOX-RUN),
shown DIRECTORIES read only and WRITABLE to write in, FILES as it has them,
writing no file larger than FILE-LIMIT octets (NIL: no limit), its
environment ARCHIVE-ENVIRONMENT's, for SECONDS at most; stopped when CANCEL's
car turns true. 7-Zip and bsdtar are given PASSWORD, or nothing, when they
ask for one, on their standard input. What it printed, as text, up to
OUTPUT-LIMIT octets (NIL: let go; with OUTPUT-KEPT NIL, counted, never kept,
and NIL returned); an error saying why when it does not end well
\(ARCHIVE-FAILURE-WORDS)."
  (multiple-value-bind (output outcome said)
      (tuatara.io:sandbox-run (program-path program) arguments
                              :seconds seconds :output-limit output-limit :output-kept output-kept
                              :file-limit file-limit :cancel cancel :directories directories
                              :writable writable :files files :environment (archive-environment)
                              :secret (and (not (eq program :tar)) (or password "")))
    (unless (eq outcome :done)
      (refuse "~a" (archive-failure-words program kind outcome said password)))
    (and output-limit output-kept (tuatara.text:octets-string output))))

(defun check-compressor (kind)
  "An error when the program compressing archives of KIND, an entry of
*ARCHIVE-KINDS*, is not installed where tar finds it shut in."
  (let* ((option (getf (rest kind) :option))
         (program (and option (subseq option 2))))
    (when (and program (not (tuatara.io:sandbox-find-program program)))
      (refuse "~a is not installed: install it for archives compressed so" program))))

(defun archive-kind (archive)
  "The entry of *ARCHIVE-KINDS* of the archive ARCHIVE, told by its first
octets (FILE-KIND), never by its name; an error saying so when it is no
archive of those kinds."
  (let ((by-octets (nth-value 1 (file-kind archive))))
    (or (assoc (if (member by-octets *zip-kinds*) :zip by-octets) *archive-kinds*)
        ;; tar archives of before POSIX have no mark to be told by
        (and (null by-octets) (eq :tar (first (archive-kind-of-name archive))) (assoc :tar *archive-kinds*))
        (refuse "it is no archive: a tar one (gzip, bzip2, xz or zstd compressed, or not), ~
                 zip, 7z, rar, ISO 9660 or cpio"))))

(defun archive-arguments (program kind action archive &key directory)
  "The arguments of PROGRAM (:TAR, :7ZZ, :BSDTAR) reading archives of KIND
\(*ARCHIVE-KINDS*) for ACTION on the archive ARCHIVE, where it is seen in the
sandbox: :LIST its contents, a line each (7-Zip's as its technical list,
SEVEN-ZIP-LISTING reads it; bsdtar's ending with the format it read);
:EXTRACT them into DIRECTORY, the files yours, their modes as your umask has
them, none written outside DIRECTORY nor through a link, and those there
already left as they are; :TEST, 7-Zip's and bsdtar's, every file unpacked
and let go, nothing written. 7-Zip is held to the archive's format. An error
when the program compressing it is not there."
  (check-compressor kind)
  (let ((file (tuatara.io:path-join (tuatara.io:real-place (tuatara.io:parent-path archive))
                                    (tuatara.io:native-file-name archive)))
        (option (getf (rest kind) :option)))
    (ecase program
      (:tar
       (ecase action
         (:list (append (list "--list" "--verbose" "--full-time" "--file" file) (and option (list option))))
         (:extract (append (list "--extract" "--file" file) (and option (list option))
                           (list "--directory" directory "--no-same-owner" "--no-same-permissions" "--skip-old-files"
                                 "--delay-directory-restore")))))
      (:bsdtar
       (ecase action
         (:list (list "-t" "-v" "-v" "--file" file))
         (:extract (list "-x" "--file" file "-C" directory "--no-same-owner" "--no-same-permissions" "-k"
                         "--no-xattrs" "--no-acls" "--no-fflags"))
         (:test (list "-x" "-O" "--file" file))))
      (:7zz
       ;; RAR 5 archives have a reader of their own
       (let ((switches (list (format nil "-t~a" (if (and (eq (first kind) :rar)
                                                         (search "version 5" (or (file-kind archive) "")))
                                                    "Rar5"
                                                    (getf (rest kind) :type)))
                             "-sccUTF-8" "-bsp0" "-bso1" "-bse2")))
         (ecase action
           (:list (append (list "l" "-slt") switches (list "--" file)))
           (:extract (append (list "x" (format nil "-o~a" directory) "-aos" "-snl") switches (list "--" file)))
           (:test (append (list "t") switches (list "--" file)))))))))

(defun bsdtar-format-checked (kind listing)
  "LISTING, what bsdtar listed of an archive of KIND (*ARCHIVE-KINDS*), ending
with the format it read; an error when that is not KIND's :FORMAT, as when a
file named and marked as one kind is read as another (a list of files to
copy, mtree)."
  (let* ((line (find-if (lambda (line) (uiop:string-prefix-p "Archive Format: " line))
                        (uiop:split-string listing :separator '(#\Newline)) :from-end t))
         (format (and line (subseq line (length "Archive Format: ")))))
    (unless (and format (search (getf (rest kind) :format) format :end2 (or (position #\, format) (length format))
                                :test #'char-equal))
      (refuse "it was read as ~a, not as the ~a archive its first octets say it is: nothing unpacked"
              (if format (escape-controls (subseq format 0 (or (position #\, format) (length format)))) "something else")
              (getf (rest kind) :words)))
    listing))

(defun seven-zip-listing (text)
  "7-Zip's technical list of an archive's contents (TEXT, from l -slt) as the
other lists are written, a line each: kind and permissions, owner and group
\(- when the archive keeps none), size, time to the second, name, and where a
link leads."
  (let ((blocks (let ((start (search (format nil "~%----------~%") text)))
                  (and start (cl-ppcre:split "\\n\\s*\\n" (subseq text (+ start 12)))))))
    (with-output-to-string (out)
      (dolist (block blocks)
        (let ((fields (loop for line in (uiop:split-string block :separator '(#\Newline))
                            for at = (search " = " line)
                            when at collect (cons (subseq line 0 at) (subseq line (+ at 3))))))
          (flet ((field (name) (cdr (assoc name fields :test #'string=))))
            (when (field "Path")
              (let* ((attributes (tuatara.text:split-words (or (field "Attributes") "")))
                     (unix (find-if (lambda (word) (and (= (length word) 10) (find (char word 0) "-dlbcps"))) attributes))
                     (folder (or (equal (field "Folder") "+") (find "D" attributes :test #'string=)))
                     (time (or (field "Modified") ""))
                     (link (or (field "Symbolic Link") (field "Link"))))
                (format out "~a ~a/~a ~12@a ~a ~a~@[ -> ~a~]~%"
                        (or unix (if folder "d---------" "----------"))
                        (let ((user (field "User"))) (if (plusp (length user)) user "-"))
                        (let ((group (field "Group"))) (if (plusp (length group)) group "-"))
                        (or (field "Size") "")
                        (subseq time 0 (min 19 (length time)))
                        (field "Path")
                        (and (plusp (length link)) link))))))))))

(define-condition archive-locked (error)
  ((path :initarg :path :reader archive-locked-path :documentation "The archive."))
  (:report (lambda (c stream) (format stream "~a is locked with a password" (archive-locked-path c))))
  (:documentation "An archive is locked with a password, and none was given: it is asked for, then unpacked."))

(defun archive-locked-p (path kind program)
  "True when the archive at PATH, of KIND (*ARCHIVE-KINDS*) read by PROGRAM
\(ARCHIVE-PROGRAM), needs a password to unpack that PROGRAM can take: a zip
archive with an entry locked (TUATARA.IO:ZIP-LOCKED-P, from its directory
alone) for bsdtar; for 7-Zip, any of its kinds whose list says so, or which
cannot be listed without one. Takes time: call it in the background."
  (case program
    (:bsdtar (and (eq (first kind) :zip)
                  (handler-case (tuatara.io:call-with-file path (lambda (fd name size)
                                                                  (declare (ignore name))
                                                                  (tuatara.io:zip-locked-p fd size)))
                    (tuatara.io:file-system-error () nil))))
    (:7zz (handler-case (search (format nil "~%Encrypted = +")
                                (run-archive-program program kind (archive-arguments program kind :list path)
                                                     :directories (list (tuatara.io:parent-path path))
                                                     :output-limit *archive-listing-limit*))
            (refusal (e) (search "locked with a password" (princ-to-string e)))))))

(defun archive-health-words ()
  "What lists and unpacks archives other than tar ones here, as \\[check-health] says it: 7-Zip for the best of it."
  (let ((found (seven-zip)))
    (format nil "~a; ~a"
            (outside-program-health-words :archives "Zip, 7z, rar, ISO 9660 and cpio archives"
                                          (tuatara.io:sandbox-find-program "bsdtar"))
            (if found
                (format nil "7z, rar (when it reads them) and zip archives by ~a, 7-Zip ~{~d.~2,'0d~}, shut in~@[~a~]"
                        (first found) (second found)
                        (and (< (first (second found)) 25)
                             (format nil ": older than 25.00, it lacks fixes of its readers made since (CVE-2024-11477, ~
                                          CVE-2025-11001, CVE-2025-11002, CVE-2025-53816, CVE-2025-53817); shut in, ~
                                          it can harm nothing else, but is worth updating")))
                (format nil "7-Zip for the best of them, not found: ~a: ~a" (getf (outside-program :seven-zip) :what)
                        (outside-program-install-words :seven-zip))))))

;;; Making

(defun write-archive (paths archive &key cancel replace)
  "Pack PATHS, files and directories with all in them, links as links, into
the tar archive ARCHIVE, compressed as its name ends (*ARCHIVE-KINDS*); each
kept inside with its path below the deepest directory all of them are in. It
is written under a passing name beside ARCHIVE and given that name once whole,
what is there already moved to the trash first with REPLACE; nothing is left
when it fails or CANCEL's car turns true. An error saying why when it does not
work."
  (let* ((kind (or (archive-kind-of-name archive :made t)
                   (refuse "~a is not named as a tar archive is" (tuatara.io:native-file-name archive))))
         (option (getf (rest kind) :option))
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
    (handler-bind ((serious-condition (lambda (e)
                                        (declare (ignore e))
                                        (ignoring-errors-reported ("taking away ~a" passing)
                                          (tuatara.io:remove-file passing)))))
      (run-archive-program :tar kind (append (list "--create" "--file" (tuatara.io:path-join
                                                                        (tuatara.io:real-place folder)
                                                                        (tuatara.io:native-file-name passing)))
                                             (and option (list option))
                                             (list "--directory" (tuatara.io:real-place root)
                                                   "--null" "--verbatim-files-from" "--files-from" "/tmp/names"))
                           :cancel cancel
                           :directories (remove-duplicates (cons root (mapcar #'tuatara.io:parent-path paths))
                                                           :test #'string=)
                           :writable (list folder)
                           :files (list (cons "/tmp/names" names)))
      (when (and cancel (car cancel))
        (refuse "stopped"))
      (when (and replace (tuatara.io:path-stamp archive :follow nil))
        (tuatara.io:trash-path archive))
      (tuatara.io:rename-path passing archive))
    archive))

;;; Unpacking

(defun extract-archive (archive directory &key cancel password)
  "Unpack the archive ARCHIVE, of a kind of *ARCHIVE-KINDS* told by its first
octets, into DIRECTORY, made when it is not there: its files yours, their
modes as your umask has them, no setuid bit kept, none written outside
DIRECTORY whatever names and links the archive holds, no named pipe or
device made. The program unpacking it writes only in a new folder of its own
in DIRECTORY, from which each thing is then moved into place, never over
what is there: a name there already is left as it is, and returned among the
second value, the names so left. An archive locked with a password is
opened with PASSWORD, tried first on all of it, so that a wrong one unpacks
nothing; without one, ARCHIVE-LOCKED is signalled. What it makes you may
write, move and delete (MAKE-EXTRACTED-WRITABLE). Stopped when CANCEL's car
turns true, or before the drive has less than *ARCHIVE-FREE-RESERVE* free
or room for too few more files (ARCHIVE-FILES-LEFT), said so. An error
saying why when it does not work; what was unpacked before stays."
  (let* ((*secret-at-hand* (or *secret-at-hand* (and password t)))
         (kind (archive-kind archive))
         (program (archive-program kind))
         (directory (tuatara.io:slash-ended directory))
         (made (not (tuatara.io:path-stamp directory :follow nil)))
         (passing nil) (left '()))
    (when (and (null password) (archive-locked-p archive kind program))
      (error 'archive-locked :path archive))
    (when (eq program :bsdtar)
      (bsdtar-format-checked kind (run-archive-program program kind (archive-arguments program kind :list archive)
                                                       :cancel cancel :directories (list (tuatara.io:parent-path archive))
                                                       :output-limit *archive-listing-limit*)))
    (when password
      ;; what it unpacks to is held to the room unpacking would have
      (let ((room (max 0 (archive-room-left (tuatara.io:real-path-or-parent directory)))))
        (handler-case (run-archive-program program kind (archive-arguments program kind :test archive)
                                           :cancel cancel :directories (list (tuatara.io:parent-path archive))
                                           :password password :seconds *archive-test-seconds*
                                           :output-limit room :output-kept nil)
          (refusal (e)
            (if (search "printed more than it may" (princ-to-string e))
                (refuse "unpacking stopped: ~a has less than ~a free (*archive-free-reserve*)"
                        (abbreviated-path directory) (tuatara.io:octets-words *archive-free-reserve*))
                (error e))))))
    (tuatara.io:ensure-directories directory)
    (setf passing (tuatara.io:slash-ended (tuatara.io:path-join directory (format nil ".unpacking.~a.part"
                                                                                  (tuatara.io:random-name)))))
    (tuatara.io:make-directory passing #o700)
    ;; each step after it is taken whatever the others do; the first error is the one said
    (let ((failure nil))
      (flet ((attempt (function)
               (handler-case (funcall function)
                 (error (e) (unless failure (setf failure e))))))
        (unwind-protect (unpack-archive program kind archive passing cancel password)
          (attempt (lambda () (make-extracted-writable passing)))
          (attempt (lambda () (setf left (move-into-place passing directory))))
          (attempt (lambda () (tuatara.io:delete-tree passing)))
          ;; a folder made for it that nothing went into is taken away again
          (when made
            (handler-case (sb-posix:rmdir (string-right-trim "/" directory))
              (sb-posix:syscall-error () nil)))))
      (when failure (error failure)))
    (values directory left)))

(defun move-into-place (from to)
  "Move each thing in the folder FROM into the folder TO, never over what is
there (TUATARA.IO:RENAME-PATH): a folder whose name is a folder there
already has what it holds moved into that one the same way; anything else
whose name is there stays in FROM. The names so left, from FROM."
  (let ((left '()))
    (dolist (entry (tuatara.io:directory-entries from))
      (destructuring-bind (name kind size own &rest more) entry
        (declare (ignore kind size more))
        (let ((source (tuatara.io:path-join from name))
              (target (tuatara.io:path-join to name)))
          (handler-case (tuatara.io:rename-path source target)
            (tuatara.io:file-system-error (e)
              (let ((there (tuatara.io:path-kind target :follow nil)))
                (cond ((null there) (error e))
                      ((and (eq own :directory) (eq there :directory))
                       (dolist (inner (move-into-place (tuatara.io:slash-ended source) (tuatara.io:slash-ended target)))
                         (push (format nil "~a/~a" name inner) left)))
                      (t (push name left)))))))))
    (nreverse left)))

(defun make-extracted-writable (directory)
  "Give everything in DIRECTORY, the folder an archive was just unpacked
into, the modes your umask allows (TUATARA.IO:UMASK-BITS), whatever program
unpacked it, with the owner's write bit, its folders the owner's read and
enter bits too, and no setuid or setgid bit, so that what an archive put
there with other modes can be moved and deleted and runs as no one else.
Links are neither followed nor changed."
  (flet ((set-bits (path stamp bits)
           (let* ((mode (logand (tuatara.io:stamp-mode stamp) #o7777))
                  (wanted (logior (logand mode (lognot (logior #o6000 (tuatara.io:umask-bits)))) bits)))
             (when (/= mode wanted)
               (tuatara.io:set-path-mode path wanted)))))
    (walk-directory directory
                    (lambda (folder relative entry)
                      (declare (ignore relative))
                      (destructuring-bind (&optional name kind size own stamp) entry
                        (declare (ignore size))
                        (when (and entry stamp (not (eq own :symlink)))
                          (let ((path (tuatara.io:path-join folder name)))
                            (if (eq kind :directory)
                                (progn (set-bits path stamp #o700) t)
                                (progn (set-bits path stamp #o200) nil)))))))))

(defparameter *archive-least-files-left* 10000
  "The fewest more files the drive an archive is unpacked onto keeps room for
\(ARCHIVE-FILES-LEFT): unpacking stops before it has room for fewer than
these, or than 1 in 100 of all it holds, whichever is more, so that an
archive of many empty files cannot use up the drive's room for files.")

(defun archive-files-left (directory)
  "How many more files unpacking may make on the drive holding DIRECTORY
before it has room for fewer than it keeps (*ARCHIVE-LEAST-FILES-LEFT*, or
1 in 100 of all it holds when more); NIL for a drive that keeps no count."
  (multiple-value-bind (free total) (tuatara.io:free-inodes directory)
    (and free (- free (max *archive-least-files-left* (floor total 100))))))

(defun archive-room-left (directory)
  "The octets unpacking may still write on the drive holding DIRECTORY before
it has less than *ARCHIVE-FREE-RESERVE* free."
  (- (or (tuatara.io:free-bytes directory) most-positive-fixnum) *archive-free-reserve*))

(defun unpack-archive (program kind archive directory cancel password)
  "Unpack ARCHIVE, of KIND (*ARCHIVE-KINDS*), into DIRECTORY, a new empty
folder, by PROGRAM (ARCHIVE-PROGRAM), with PASSWORD, as EXTRACT-ARCHIVE
says: stopped when CANCEL's car turns true, or before the drive has less
than *ARCHIVE-FREE-RESERVE* free or room for too few more files
\(ARCHIVE-FILES-LEFT), said so."
  (flet ((room-left () (archive-room-left directory))
         (files-left () (or (archive-files-left directory) most-positive-fixnum))
         (full ()
           (refuse "unpacking stopped: ~a has less than ~a free (*archive-free-reserve*)"
                  (abbreviated-path directory) (tuatara.io:octets-words *archive-free-reserve*)))
         (crowded ()
           (refuse "unpacking stopped: the drive of ~a would have room for too few more files: ~
                    the larger of ~:d and 1 in 100 of all it holds is kept"
                   (abbreviated-path directory) *archive-least-files-left*)))
    (unless (plusp (room-left)) (full))
    (unless (plusp (files-left)) (crowded))
    ;; the program is stopped by the watch below once the drive is nearly
    ;; full or has room for too few more files, and held to files no larger
    ;; than the room there was at its start
    (let* ((stop (list nil)) (filled nil) (many nil) (done nil)
           (watch (tuatara.runtime:make-guarded-thread
                   (lambda ()
                     (loop until done
                           do (cond ((and cancel (car cancel)) (setf (car stop) t))
                                    ((not (plusp (room-left))) (setf filled t (car stop) t))
                                    ((not (plusp (files-left))) (setf many t (car stop) t)))
                              (sleep 0.2)))
                   :name "tuatara archive room")))
      (unwind-protect
           (handler-case (run-archive-program program kind (archive-arguments program kind :extract archive
                                                                              :directory (tuatara.io:real-place directory))
                                              :cancel stop
                                              :directories (list (tuatara.io:parent-path archive))
                                              :writable (list directory)
                                              :file-limit (room-left)
                                              :password password)
             (error (e)
               (cond ((or filled (< (room-left) (* 64 1024 1024))) (full))
                     ((or many (not (plusp (files-left)))) (crowded))
                     (t (error e)))))
        (setf done t)
        (sb-thread:join-thread watch :default nil)))))

(defun archive-listing (archive &key cancel)
  "What the archive ARCHIVE holds, of a kind of *ARCHIVE-KINDS* told by its
first octets, a line each: its kind and permissions, owner and group, size,
time and name, where a link leads; a tar archive's as tar --list --verbose
writes it and 7-Zip's in the same columns, times to the second, bsdtar's as
it writes them, times to the minute (the year for one more than half a year
away), and the format it read last. A character that cannot be shown, or
that would turn the direction a line is drawn in, is written as its code.
Nothing is unpacked; the program, shut in, sees only the archive's folder,
read only. Stopped when CANCEL's car turns true; an error saying why when it
cannot be read, is locked with a password 7-Zip needs, is read as another
format than it is, or the list is longer than *ARCHIVE-LISTING-LIMIT*."
  (let* ((kind (archive-kind archive))
         (program (archive-program kind))
         (text (run-archive-program program kind (archive-arguments program kind :list archive)
                                    :cancel cancel :directories (list (tuatara.io:parent-path archive))
                                    :output-limit *archive-listing-limit*)))
    (format nil "~{~a~%~}"
            (mapcar #'escape-controls
                    (remove "" (uiop:split-string (case program
                                                    (:7zz (seven-zip-listing text))
                                                    (:bsdtar (bsdtar-format-checked kind text))
                                                    (t text))
                                                  :separator '(#\Newline))
                            :test #'string=)))))

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

(define-command rimu-archive-list ()
  "Show what the archive shown holds, in a buffer of its own, which \\`q'
closes: the entry on the line in a list of a directory (\\<rimu-mode>\\[rimu-archive-list] there);
with \\[universal-argument], or where there is none, asked for. A tar archive,
plain or compressed with gzip, bzip2, xz or zstd, a zip, 7z, rar, ISO 9660 or
cpio one, told by what it holds, whatever its name; a file of none of these
kinds is refused, said so. A line each: kind and permissions, owner and
group, size, time, name, and where a link leads; times to the second for tar
archives and for those 7-Zip reads (zip, 7z and rar, where it is installed),
to the minute for those bsdtar reads, which also says the format it read. A
character that would hide another or turn a line's direction is written as
its code. Nothing is unpacked: tar, 7-Zip or bsdtar reads it in the
background, shut in the sandbox."
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
