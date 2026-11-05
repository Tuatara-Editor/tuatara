;;;; lisp/remote/files.lisp --- Files on another machine visited, saved and read again: /ssh:HOST:/path.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file on another machine is named /ssh:HOST:/path, HOST as ssh takes it.
;;;; Visiting one reads it in the background through SFTP (sftp.lisp) into a
;;;; copy of its own here, in a private folder of the cache, readable by you
;;;; alone and deleted with its buffer; the buffer visits that copy, so that a
;;;; large file is read in pieces as a large file here is, and is named and
;;;; placed after the file there. Saving writes the copy, then sends it to a
;;;; new file beside the one there, given its owner, group and permissions,
;;;; renamed over it once the file there is found as it was read; a file with
;;;; other names (hard links), or whose owner a new file cannot be given, is
;;;; written over in place instead. Nothing runs on such a copy that would read what
;;;; is around it here: no language server, git, auto-saved copy or build;
;;;; the settings a file names for itself, and the shared program parsing
;;;; trusted text, only for the servers on the trusted list (M-x server-list).
;;;; A file of evidence (*EVIDENCE-FOLDERS* naming /ssh:HOST:/folder/, HOST
;;;; any name ssh takes for that machine, the folder as written or where it
;;;; really is there) opens safely and is never written.

(in-package #:tuatara.defaults)

(defconstant +remote-hash-most+ (* 1024 1024)
  "The largest file on another machine whose contents are compared by their
SHA-256 sum before a save writes over it, besides its size and time.")

;;; Names

(defun remote-name-p (name)
  "True when NAME, a path as typed, names a file on another machine: /ssh:HOST:PATH."
  (and (stringp name) (uiop:string-prefix-p "/ssh:" name) (position #\: name :start 5) t))

(defun remote-prefix-length (name)
  "How long the /ssh:HOST: before NAME's path is, when NAME names a file on another machine; else NIL."
  (and (remote-name-p name) (1+ (position #\: name :start 5))))

(defun remote-path-normal (path)
  "PATH, a path on another machine, its . and .. parts and doubled slashes
worked out by its words alone; ~ or ~/ first taken away, a path from the
home folder there; \"\" for that home folder. A / at the end is kept."
  (let* ((path (cond ((string= path "~") "")
                     ((uiop:string-prefix-p "~/" path) (subseq path 2))
                     (t path)))
         (absolute (uiop:string-prefix-p "/" path))
         (parts '()))
    (dolist (part (uiop:split-string path :separator "/"))
      (cond ((member part '("" ".") :test #'string=))
            ((and (string= part "..") parts (string/= (first parts) "..")) (pop parts))
            ((and (string= part "..") absolute))
            (t (push part parts))))
    (let ((joined (format nil "~:[~;/~]~{~a~^/~}" absolute (reverse parts))))
      (if (and parts (uiop:string-suffix-p path "/")) (concatenate 'string joined "/") joined))))

(defun remote-name-parts (name)
  "The machine and the path NAME names, /ssh:HOST:PATH, two values: HOST as
ssh takes it, PATH as REMOTE-PATH-NORMAL makes it. Refused, saying why, for
a host ssh could take for something else (HOST-REFUSAL)."
  (let* ((end (1- (remote-prefix-length name)))
         (host (subseq name 5 end)))
    (when (host-refusal host) (refuse "~a" (host-refusal host)))
    (values host (remote-path-normal (subseq name (1+ end))))))

(defun remote-folder-name (host path)
  "The name of the folder holding PATH on HOST, ending in /."
  (let ((slash (position #\/ path :from-end t)))
    (remote-name host (if slash (subseq path 0 (1+ slash)) ""))))

(defun remote-stamp (attributes)
  "ATTRIBUTES of a file on another machine (SFTP-TAKE-ATTRIBUTES) as a
TUATARA.IO:STAMP, as a file here is described."
  (tuatara.io:make-stamp 0 0 (or (getf attributes :size) 0) (* (or (getf attributes :mtime) 0) 1000000000) 1
                         (or (getf attributes :permissions) 0) (or (getf attributes :uid) 0)
                         (or (getf attributes :gid) 0)))

(defparameter *remote-candidates-seconds* 5
  "The seconds the candidates of a folder on another machine, read for a
question for a file, are kept to be offered again (REMOTE-CANDIDATES).")

(sb-ext:defglobal **remote-candidates-kept** (make-hash-table :test 'equal :synchronized t)
  "The candidates of the folders on other machines read last, by the
folder's name, each (WHEN . CANDIDATES), WHEN the internal real time they
were read.")

(defun remote-candidates (folder)
  "Completion candidates of the entries of FOLDER, a name /ssh:HOST:PATH
ending in /, as FILE-CANDIDATES gives those of a folder here: folders first,
each with what is known of it; read from the server on the thread asking,
those read for it within *REMOTE-CANDIDATES-SECONDS* given again instead,
older ones let go of as new ones are kept. A
name that would not show as it is (REMOTE-CLEAN-NAME-P) is left out."
  (let ((kept (gethash folder **remote-candidates-kept**)) (now (get-internal-real-time)))
    (if (and kept (< (- now (car kept)) (* *remote-candidates-seconds* internal-time-units-per-second)))
        (cdr kept)
        (let ((candidates (read-remote-candidates folder))
              (oldest (- now (* *remote-candidates-seconds* internal-time-units-per-second))))
          (sb-ext:with-locked-hash-table (**remote-candidates-kept**)
            (maphash (lambda (key value) (when (<= (car value) oldest) (remhash key **remote-candidates-kept**)))
                     **remote-candidates-kept**)
            (setf (gethash folder **remote-candidates-kept**) (cons now candidates)))
          candidates))))

(defun read-remote-candidates (folder)
  "The candidates of REMOTE-CANDIDATES, read from the server."
  (multiple-value-bind (host path) (remote-name-parts folder)
    (let ((now (get-universal-time)) (folders '()) (files '()))
      (dolist (entry (sort (remote-listing host (if (string= path "") "." path)) #'tuatara.text:natural-string-lessp
                           :key #'car))
        (destructuring-bind (name . attributes) entry
          (when (remote-clean-name-p name)
            (let ((fields (stamp-fields (remote-stamp attributes) now)))
              (if (eq (remote-kind attributes) :directory)
                  (push (make-candidate (concatenate 'string name "/") :annotation fields) folders)
                  (push (make-candidate name :annotation fields) files))))))
      (nconc (nreverse folders) (nreverse files)))))

;;; The buffer of a file on another machine

(defstruct (remote-file (:constructor make-remote-file (host path)) (:copier nil))
  "What a buffer visiting a copy of a file on another machine knows of it:
HOST, as ssh takes it, and PATH there, from the root; ATTRIBUTES, those the
file had as last read or written there (NIL when it was not there); SUM, its
SHA-256 sum then, when it was small enough (+REMOTE-HASH-MOST+); SENT, the
content number of the text last there, NIL for none; EVIDENCE, why it is
evidence, or NIL; TRUSTED, true when HOST is a trusted server; PARTLY, true
while a save that failed half-way left the file there partly written."
  (host "" :type string :read-only t)
  (path "" :type string :read-only t)
  (attributes nil)
  (sum nil)
  (sent nil)
  (evidence nil)
  (trusted nil)
  (partly nil))

(setf (documentation 'make-remote-file 'function) "A REMOTE-FILE for PATH on HOST."
      (documentation 'remote-file-p 'function) "True when OBJECT is a REMOTE-FILE."
      (documentation 'remote-file-host 'function) "The machine holding FILE, as ssh takes it."
      (documentation 'remote-file-path 'function) "Where FILE is on its machine, from the root."
      (documentation 'remote-file-attributes 'function) "FILE's attributes as last read or written, or NIL."
      (documentation 'remote-file-sum 'function) "FILE's SHA-256 sum as last read or written, or NIL."
      (documentation 'remote-file-sent 'function) "The content number of the text last on FILE's machine, or NIL."
      (documentation 'remote-file-evidence 'function) "Why FILE is evidence, in words, or NIL."
      (documentation 'remote-file-trusted 'function) "True when FILE's machine is a trusted server."
      (documentation 'remote-file-partly 'function) "True while a failed save left FILE partly written there.")

(defun buffer-remote (buffer)
  "The REMOTE-FILE BUFFER visits a copy of, or NIL."
  (tuatara.buffer:buffer-local buffer :remote-file))

(defun buffer-remote-name (buffer)
  "The name, /ssh:HOST:/path, of the file on another machine BUFFER visits a copy of, or NIL."
  (let ((remote (buffer-remote buffer)))
    (and remote (remote-name (remote-file-host remote) (remote-file-path remote)))))

(defun remote-buffer (host path &key except)
  "The live buffer but EXCEPT visiting a copy of PATH on HOST, or of a file being read from there, or NIL."
  (find-if (lambda (buffer)
             (let ((remote (or (buffer-remote buffer) (tuatara.buffer:buffer-local buffer :remote-reading))))
               (and remote (not (eq buffer except))
                    (string= (remote-file-host remote) host) (string= (remote-file-path remote) path))))
           tuatara.buffer:*buffers*))

(defun remote-place-real-path (host path)
  "Where the place PATH, a path on HOST, really is: the folder it is in where
that really is there, with PATH's own name after it, not followed when it
is a link; NIL when that folder is not there. Any other failure is
signalled."
  (let* ((path (if (string= path "/") path (string-right-trim "/" path)))
         (slash (position #\/ path :from-end t)))
    (handler-case
        (if (member path '("" "/") :test #'string=)
            (remote-real-path host path)
            (format nil "~a/~a" (string-right-trim "/" (remote-real-path host (if slash (subseq path 0 (1+ slash)) "")))
                    (subseq path (if slash (1+ slash) 0))))
      (sftp-refusal (condition)
        (if (remote-missing-p condition) nil (error condition))))))

(defun remote-machine-entries (rules host)
  "The entries of RULES naming folders on the machine HOST reaches
\(/ssh:OTHER:/folder/, OTHER any name ssh takes for it, SSH-MACHINE)."
  (let ((machine nil))
    (remove-if-not (lambda (entry)
                     (let ((given (evidence-entry-given entry)))
                       (and (eq (evidence-entry-kind entry) :glob) (remote-name-p given)
                            (let ((other (subseq given 5 (1- (remote-prefix-length given)))))
                              (or (string-equal other host)
                                  (string= (ssh-machine other) (or machine (setf machine (ssh-machine host)))))))))
                   (evidence-rules-entries rules))))

(defun remote-place-names (host path)
  "PATH, a place on HOST, and where it really is there
\(REMOTE-PLACE-REAL-PATH), the names it is told by as evidence. When where it
really is cannot be found, PATH alone; but refused, saying why, when an
entry of *EVIDENCE-FOLDERS* names a folder on that machine, as it could not
then be told whether the place is evidence."
  (handler-case (list path (remote-place-real-path host path))
    (tuatara.io:file-system-error (condition)
      (let ((rules (evidence-rules)))
        (when (and rules (remote-machine-entries rules host))
          (refuse "it could not be told whether ~a is evidence: ~a" (remote-name host path) condition))
        (list path)))))

(defun remote-evidence-entries (rules host)
  "The entries of RULES naming folders on the machine HOST reaches
\(/ssh:OTHER:/folder/, OTHER any name ssh takes for it, SSH-MACHINE), each
made an entry naming them under HOST, as written and with their fixed
folders at the front where those really are there (REMOTE-KEPT-REAL-PATH)."
  (loop for entry in (remote-machine-entries rules host)
        for given = (evidence-entry-given entry)
        collect (let* ((written (first (evidence-entry-pattern entry)))
                           (glob (subseq written (remote-prefix-length written)))
                           (end (glob-fixed-end glob))
                           (real (and (plusp end) (remote-kept-real-path host (subseq glob 0 end)))))
                      (make-evidence-entry given :glob (remove-duplicates
                                                        (list (remote-name host glob)
                                                              (remote-name host (if real
                                                                                    (concatenate
                                                                                     'string (string-right-trim "/" real)
                                                                                     (subseq glob end))
                                                                                    glob)))
                                                        :test #'string=)
                                           nil nil))))

(defun remote-evidence (host names)
  "The evidence folder a place on HOST is, or is in, by *EVIDENCE-FOLDERS*,
and the entry making it one as written, two values; NIL when it is none.
NAMES are the place's paths there from the root, as written and where it
really is: each is matched against every entry by its words, and against
the entries naming folders on the machine HOST reaches under any name of it,
as written and where they really are there (REMOTE-EVIDENCE-ENTRIES). May ask
the server and run ssh -G: for work in the background."
  (let ((rules (evidence-rules)))
    (when rules
      (let ((entries (append (evidence-rules-entries rules) (remote-evidence-entries rules host))))
        (dolist (name (remove-duplicates (remove nil names) :test #'string=))
          (let ((match (written-evidence-match rules (remote-name host name) entries)))
            (when match
              (return (values (tuatara.io:slash-ended (car match)) (evidence-entry-given (cdr match)))))))))))

(defun remote-evidence-words (host path &rest reals)
  "Why PATH on HOST is evidence, in words (EVIDENCE-WORDS), or NIL: told by
PATH as written, by where it really is there (REMOTE-PLACE-REAL-PATH), and
by REALS, other paths there that lead to it (REMOTE-EVIDENCE). When an
entry names a folder on that machine and where the place, or that folder,
really is cannot be found, words saying it could not be told, so that it is
kept as evidence is. Asks the server only while *EVIDENCE-FOLDERS* names
some folder; for work in the background."
  (when (evidence-rules)
    (handler-case
        (multiple-value-bind (folder entry) (remote-evidence host (append (remote-place-names host path) reals))
          (and folder (evidence-words folder entry)))
      ((or tuatara.io:file-system-error tuatara.runtime:refusal) (condition)
        (if (remote-machine-entries (evidence-rules) host)
            (format nil "it could not be told whether ~a is evidence (~a), so nothing is written there"
                    (remote-name host path) condition)
            (multiple-value-bind (folder entry) (remote-evidence host (list path))
              (and folder (evidence-words folder entry))))))))

(defun remote-text-trusted-p (buffer)
  "True when BUFFER visits a copy of a file on a trusted server, not of evidence."
  (let ((remote (buffer-remote buffer)))
    (and remote (remote-file-trusted remote) (null (remote-file-evidence remote)))))

(defun remote-trusted-p (host)
  "True when HOST is on the list of trusted servers (\\[server-list])."
  (and (state-list-find *trusted-servers* host) t))

(defun remote-find-file-functions (trusted)
  "The functions of TUATARA.BUFFER:*FIND-FILE-FUNCTIONS* run on the copy of a
file from another machine as it is visited: its mode from its name, and with
TRUSTED the settings it names for itself; none that reads what is around the
copy here or remembers it."
  (remove-if-not (lambda (function)
                   (or (eq function 'set-auto-mode) (and trusted (eq function 'take-file-local-settings))))
                 tuatara.buffer:*find-file-functions*))

;;; Reading

(defstruct (remote-read-job (:include tuatara.runtime:job) (:constructor make-remote-read-job (buffer remote safe))
                            (:copier nil))
  "A file on another machine being read for BUFFER, a job: REMOTE, the
REMOTE-FILE as asked for (its path as typed), SAFE, true for a safe visit;
then PATH, where it is there, links followed; FOLDER, the private folder of
its copy here, and COPY, the copy's path; KIND, :FILE, :NEW for a file not
there yet, or :DIRECTORY. AGAIN, true when it is read again for a buffer
visiting it."
  remote safe path folder copy kind again)

(setf (documentation 'remote-read-job-p 'function) "True when OBJECT is a REMOTE-READ-JOB.")

(tuatara.runtime:define-job-words remote-read-job
  :doing "Reading"
  :description (format nil "Reading ~a" (remote-name (remote-file-host (remote-read-job-remote job))
                                                     (remote-file-path (remote-read-job-remote job))))
  :verb "read it"
  :left-words "nothing read")

(defun read-remote-copy (job)
  "The work of JOB, a REMOTE-READ-JOB: where the file is there found, and,
for a file, read into a new copy here in its private folder, readable by you
alone, its sum taken when it is small."
  (let* ((remote (remote-read-job-remote job))
         (host (remote-file-host remote))
         (asked (remote-file-path remote))
         (attributes (remote-attributes host (if (string= asked "") "." asked)))
         (path (if attributes
                   (remote-real-path host asked)
                   (let ((slash (position #\/ asked :from-end t)))
                     (format nil "~a/~a"
                             (string-right-trim "/" (remote-real-path host (if slash (subseq asked 0 (1+ slash)) "")))
                             (subseq asked (if slash (1+ slash) 0)))))))
    (setf (remote-read-job-path job) path
          (remote-read-job-kind job) (case (remote-kind attributes)
                                       ((nil) (if attributes :file :new))
                                       ((:file :directory) (remote-kind attributes))
                                       (t (refuse "~a is not a file" (remote-name host path)))))
    (when (and (eq :new (remote-read-job-kind job)) (zerop (length (tuatara.io:native-file-name path))))
      (refuse "~a names no file" (remote-name host path)))
    (setf (remote-file-evidence remote) (remote-evidence-words host asked path))
    (when (eq :file (remote-read-job-kind job))
      (let ((folder (or (remote-read-job-folder job) (new-private-folder (private-cache-directory "remote/")))))
        (setf (remote-read-job-folder job) folder)
        (multiple-value-bind (fd copy)
            (tuatara.io:create-free-file folder (lambda (n) (format nil "~a~:[.~d~;~*~]" (tuatara.io:native-file-name path)
                                                                     (= n 1) n)))
          (setf (remote-read-job-copy job) copy)
          (unwind-protect (setf (remote-file-attributes remote) (remote-download host path fd))
            (sb-posix:close fd))
          (when (<= (or (getf (remote-file-attributes remote) :size) 0) +remote-hash-most+)
            (setf (remote-file-sum remote) (file-sha256 copy))))))))

(defun remote-reading-text (name)
  "What the buffer of the file NAME on another machine says while it is read."
  (format nil "Reading ~a...~%" name))

(defun visit-remote (name safe show)
  "Visit the file NAME on another machine (/ssh:HOST:/path) as VISIT-PATH
visits a file: SAFE for a safe visit, SHOW a function of the buffer. A
buffer visiting it already is shown; else one saying it is being read is
shown at once and the file read in the background (a job, each request
within *REMOTE-SECONDS*), its own buffer taking that one's place once read.
A name ending in /, and one found to be a folder once read, is listed with
rimu (RIMU-SHOW-DIRECTORY). Returns the buffer shown, and true when it
visited the file already."
  (multiple-value-bind (host path) (remote-name-parts name)
    (let ((existing (remote-buffer host path)))
      (when (uiop:string-suffix-p name "/")
        (return-from visit-remote (values (rimu-show-directory name :show show :safe safe) nil)))
      (if existing
          (progn (funcall show existing) (values existing t))
          (let* ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name
                                                      (let ((own (tuatara.io:native-file-name path)))
                                                        (if (plusp (length own)) own host)))
                                                     :text (remote-reading-text (remote-name host path))))
                 (remote (make-remote-file host path))
                 (job (make-remote-read-job buffer remote safe)))
            (setf (buffer-read-only-p buffer) t
                  (remote-file-trusted remote) (remote-trusted-p host)
                  (buffer-given-directory buffer) (remote-folder-name host path)
                  (tuatara.buffer:buffer-local buffer :remote-reading) remote)
            (tuatara.runtime:start-job-thread job #'read-remote-copy :name "tuatara remote read")
            (funcall show buffer)
            (take-in-soon job *save-wait* (format nil "Reading ~a" (remote-name host path)))
            (values buffer nil))))))

(defmethod tuatara.runtime:job-take-in ((job remote-read-job))
  "Show what JOB read in its buffer, or why it could not be read."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (remote (remote-read-job-remote job))
         (folder (remote-read-job-folder job))
         (name (remote-name (remote-file-host remote) (or (remote-read-job-path job) (remote-file-path remote)))))
    (cond ((not (live-buffer-p buffer))
           (when (and folder (not (remote-read-job-again job))) (delete-private-folder folder)))
          ((not (eq (tuatara.runtime:job-status job) :done))
           (when (and folder (not (remote-read-job-again job))) (delete-private-folder folder))
           (if (remote-read-job-again job)
               (message "~a was not read again: ~a" name (tuatara.runtime:job-ending-words job))
               (show-reading-result job name nil "~a could not be read: ~a~%")))
          ((remote-read-job-again job) (remote-read-again-in buffer job))
          (t (remote-copy-in buffer job)))))

(defun remote-copy-in (placeholder job)
  "Visit the copy JOB read in a buffer of its own, which takes the place of
PLACEHOLDER, the buffer that said it was being read, in every window; a
buffer visiting the file already is shown instead. A folder is listed with
rimu in its place."
  (let* ((remote (remote-read-job-remote job))
         (host (remote-file-host remote))
         (path (remote-read-job-path job))
         (other (remote-buffer host path :except placeholder))
         (folder (remote-read-job-folder job)))
    (cond ((eq :directory (remote-read-job-kind job))
           (let ((windows (buffer-windows placeholder)))
             (rimu-show-directory (remote-name host path) :safe (remote-read-job-safe job)
                                                          :show (lambda (list)
                                                                  (dolist (window windows)
                                                                    (tuatara.display:set-window-buffer
                                                                     window list :remember nil))))
             (tuatara.buffer:kill-buffer placeholder)))
          (t
           (let ((buffer (if other
                             (progn (when folder (delete-private-folder folder)) other)
                             (remote-visit-copy remote path job)))
                 (name (tuatara.buffer:buffer-name placeholder)))
             (dolist (window (buffer-windows placeholder))
               (tuatara.display:set-window-buffer window buffer :remember nil))
             (tuatara.buffer:kill-buffer placeholder)
             (unless other
               (setf (tuatara.buffer:buffer-name buffer) (tuatara.buffer:unique-buffer-name name))))))))

(defun remote-visit-copy (remote path job)
  "The buffer visiting the copy JOB read of PATH, REMOTE's file: opened
safely when the visit is safe or the file is evidence; else with only the
find-file functions REMOTE-FIND-FILE-FUNCTIONS gives; named and placed after
the file there, its copy's folder deleted with it."
  (let* ((host (remote-file-host remote))
         (folder (remote-read-job-folder job))
         (copy (or (remote-read-job-copy job)
                   (progn (setf folder (new-private-folder (private-cache-directory "remote/")))
                          (tuatara.io:path-join folder (tuatara.io:native-file-name path)))))
         (final (make-remote-file host path))
         (safe (or (remote-read-job-safe job) (remote-file-evidence remote)))
         (buffer (let ((tuatara.buffer:*find-file-functions* (remote-find-file-functions (remote-file-trusted remote))))
                   (tuatara.io:visit-file copy :safe safe))))
    (setf (remote-file-attributes final) (remote-file-attributes remote)
          (remote-file-sum final) (remote-file-sum remote)
          (remote-file-evidence final) (remote-file-evidence remote)
          (remote-file-trusted final) (remote-file-trusted remote)
          (remote-file-sent final) (and (remote-file-attributes remote) (tuatara.buffer:buffer-content-number buffer))
          (tuatara.buffer:buffer-local buffer :remote-file) final
          (tuatara.buffer:buffer-local buffer 'private-folder) folder
          (tuatara.buffer:buffer-local buffer :reread) #'remote-read-again
          (buffer-given-directory buffer) (remote-folder-name host path))
    (remember-file (remote-name host path) :safe safe)
    (message "~a~:[~*~; (~a)~]~@[: ~a~]" (remote-name host path) (remote-file-attributes remote)
             (octets-words (or (getf (remote-file-attributes remote) :size) 0))
             (cond ((remote-file-evidence remote) (format nil "opened safely; ~a" (remote-file-evidence remote)))
                   ((null (remote-file-attributes remote)) "a new file, made there when saved")))
    buffer))

;;; Saving

(defstruct (remote-write-job (:include tuatara.runtime:job)
                             (:constructor make-remote-write-job (buffer remote copy content)) (:copier nil))
  "The copy of a file on another machine being sent there for BUFFER, a job:
REMOTE, its REMOTE-FILE; COPY, the copy's path here; CONTENT, the content
number of the text saved in it; ATTRIBUTES and SUM, the file's there once
written; IN-PLACE, why it was written in place, in words, or NIL when it
was replaced (REMOTE-UPLOAD); WHOLE, true to write it over a file there
left partly written, whatever it is now; PARTLY, true once the job left the
file there partly written."
  remote copy content attributes sum in-place whole partly)

(setf (documentation 'remote-write-job-p 'function) "True when OBJECT is a REMOTE-WRITE-JOB.")

(tuatara.runtime:define-job-words remote-write-job
  :doing "Sending"
  :description (format nil "Sending ~a" (remote-name (remote-file-host (remote-write-job-remote job))
                                                     (remote-file-path (remote-write-job-remote job))))
  :verb "send it"
  :left-words "the file there as it was")

(defun remote-changed-words (remote attributes)
  "Why the file of REMOTE, whose attributes there are now ATTRIBUTES (NIL:
not there), may not be written over, in words, or NIL when it is as last
read or written: there or not as it was, of the same size and time."
  (let ((then (remote-file-attributes remote)))
    (cond ((and (null then) attributes) "a file of that name is there now, made since it was opened here")
          ((and then (null attributes)) "it was taken away on the server since it was read")
          ((and then (or (not (eql (getf then :size) (getf attributes :size)))
                         (not (eql (getf then :mtime) (getf attributes :mtime)))))
           "it was changed on the server since it was read"))))

(defun send-remote-copy (job)
  "The work of JOB, a REMOTE-WRITE-JOB: the copy sent to its file there, made
like it (REMOTE-UPLOAD), only when it is not evidence, told again now
\(REMOTE-EVIDENCE-WORDS), and is as it was read: there or not as it was, of
the same size and time, and then, when small enough (+REMOTE-HASH-MOST+),
of the same contents, read into a file of its own beside the copy and taken
away after; a job writing it WHOLE over a file left partly written skips
the comparing. A file with other names is not written in place while
*EVIDENCE-FOLDERS* names a folder on that machine, as one of them may be in
it. The sum of what was sent taken likewise."
  (let* ((remote (remote-write-job-remote job))
         (host (remote-file-host remote))
         (path (remote-file-path remote))
         (copy (remote-write-job-copy job))
         (then (remote-file-attributes remote))
         (whole (remote-write-job-whole job))
         (partly (list nil)))
    (let ((why (remote-evidence-words host path)))
      (when why (refuse "~a" why)))
    (unless whole
      (let ((why (remote-changed-words remote (remote-attributes host path :follow nil))))
        (when why (refuse "~a" why))))
    (when (and (not whole) (remote-file-sum remote) then (<= (or (getf then :size) 0) +remote-hash-most+))
      (multiple-value-bind (fd check)
          (tuatara.io:create-free-file (tuatara.io:parent-path copy) (lambda (n) (format nil ".check.~d" n)))
        (unwind-protect
             (progn (unwind-protect (remote-download host path fd) (sb-posix:close fd))
                    (unless (equal (file-sha256 check) (remote-file-sum remote))
                      (refuse "it was changed on the server since it was read")))
          (tuatara.io:remove-file check))))
    (let ((fd (tuatara.io:open-for-reading copy nil :accept '(:regular))))
      (unwind-protect
           (let ((size (tuatara.io:stamp-size (tuatara.io:fd-stamp fd copy))))
             (handler-bind ((serious-condition (lambda (condition)
                                                 (declare (ignore condition))
                                                 (when (car partly) (setf (remote-write-job-partly job) t)))))
               (setf (values (remote-write-job-attributes job) (remote-write-job-in-place job))
                     (remote-upload host path fd size
                                    :permissions (let ((mode (getf then :permissions))) (and mode (logand mode #o7777)))
                                    :expected (lambda (attributes)
                                                (and (not whole) (remote-changed-words remote attributes)))
                                    :like then
                                    :linked-refusal (let ((rules (evidence-rules)))
                                                      (and rules (remote-machine-entries rules host)
                                                           "one of them may be in an evidence folder"))
                                    :partly partly)))
             (when (<= size +remote-hash-most+)
               (setf (remote-write-job-sum job) (file-sha256 copy))))
        (sb-posix:close fd)))))

(defun remote-save-refusal (buffer)
  "Why BUFFER, visiting a copy of a file on another machine, is not saved, in
words, or NIL: the file is evidence, or is being sent there already."
  (let ((remote (buffer-remote buffer)))
    (cond ((null remote) nil)
          ((remote-file-evidence remote)
           (format nil "~a is not saved: ~a" (tuatara.buffer:buffer-name buffer) (remote-file-evidence remote)))
          ((find-if (lambda (job) (remote-write-job-p job)) (jobs-of buffer :running t))
           (format nil "~a is being sent to ~a: save it again once that is done" (tuatara.buffer:buffer-name buffer)
                   (remote-file-host remote))))))

(defun remote-after-save (buffer)
  "After-save function: send the copy BUFFER visits of a file on another
machine there, in the background; a buffer saved as a file here visits it
from then on, the file there left. When the last save left the file there
partly written, it is written over whole only after a yes."
  (let ((remote (buffer-remote buffer)))
    (when remote
      (if (not (equal (tuatara.io:parent-path (tuatara.buffer:buffer-file buffer))
                      (buffer-private-folder buffer)))
          (setf (tuatara.buffer:buffer-local buffer :remote-file) nil
                (buffer-given-directory buffer) nil)
          (let ((job (make-remote-write-job buffer remote (tuatara.io:native (tuatara.buffer:buffer-file buffer))
                                            (tuatara.buffer:buffer-saved-number buffer))))
            (cond ((not (remote-file-partly remote)))
                  ((yes-or-no-p* (format nil "The last save left ~a partly written. Write this text over it, ~
                                              whatever it holds now?" (buffer-remote-name buffer)))
                   (setf (remote-write-job-whole job) t))
                  (t (tuatara.buffer:buffer-mark-saved buffer (or (remote-file-sent remote) -1))
                     (message "~a is not written there: it is still partly written" (buffer-remote-name buffer))
                     (return-from remote-after-save nil)))
            (tuatara.runtime:start-job-thread job #'send-remote-copy :name "tuatara remote write")
            (take-in-soon job *save-wait* (format nil "Sending ~a" (buffer-remote-name buffer))))))))

(defmethod tuatara.runtime:job-take-in ((job remote-write-job))
  "Say how sending JOB's copy went; when it failed, its buffer shows as not
saved again, while it holds the text that was to be sent, and says so when
it left the file there partly written."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (remote (remote-write-job-remote job))
         (name (remote-name (remote-file-host remote) (remote-file-path remote))))
    (cond ((eq (tuatara.runtime:job-status job) :done)
           (setf (remote-file-attributes remote) (remote-write-job-attributes job)
                 (remote-file-sum remote) (remote-write-job-sum job)
                 (remote-file-sent remote) (remote-write-job-content job)
                 (remote-file-partly remote) nil)
           (message "Wrote ~a~@[ in place: ~a~]" name (remote-write-job-in-place job)))
          (t
           (when (remote-write-job-partly job)
             (setf (remote-file-partly remote) t))
           (when (and (live-buffer-p buffer)
                      (eql (tuatara.buffer:buffer-saved-number buffer) (remote-write-job-content job)))
             (tuatara.buffer:buffer-mark-saved buffer (or (remote-file-sent remote) -1)))
           (message "~a is not written there: ~a~:[~;; the file there is now partly written~]; ~a tries again" name
                    (tuatara.runtime:job-ending-words job) (remote-write-job-partly job)
                    (substitute-command-keys "\\[save-buffer]"))))))

;;; Reading again

(defun remote-read-again (buffer)
  "Read the file on another machine BUFFER visits a copy of again, in the
background, into a new copy that takes the old one's place; then the buffer
is read again from it, one step that undo takes back."
  (let* ((old (buffer-remote buffer))
         (remote (make-remote-file (remote-file-host old) (remote-file-path old)))
         (job (make-remote-read-job buffer remote (tuatara.io:safe-visit-p buffer))))
    (setf (remote-file-trusted remote) (remote-file-trusted old)
          (remote-read-job-folder job) (buffer-private-folder buffer)
          (remote-read-job-again job) t)
    (tuatara.runtime:start-job-thread job #'read-remote-copy :name "tuatara remote read")
    (take-in-soon job *save-wait* (format nil "Reading ~a again" (buffer-remote-name buffer)))))

(defun remote-read-again-in (buffer job)
  "Put the new copy JOB read in the place of the one BUFFER visits, and read BUFFER again from it."
  (let ((remote (buffer-remote buffer)) (new (remote-read-job-remote job)) (copy (remote-read-job-copy job)))
    (cond ((null copy)
           (message "~a is not on the server any more" (buffer-remote-name buffer)))
          (t (tuatara.io:rename-path copy (tuatara.io:native (tuatara.buffer:buffer-file buffer)) :replace t)
             (tuatara.buffer:widen-buffer buffer)
             ;; read from the new copy here, not from the server again
             (let ((said (let ((reread (tuatara.buffer:buffer-local buffer :reread)))
                           (setf (tuatara.buffer:buffer-local buffer :reread) nil)
                           (unwind-protect (nth-value 1 (reload-buffer buffer))
                             (setf (tuatara.buffer:buffer-local buffer :reread) reread)))))
               (setf (remote-file-attributes remote) (remote-file-attributes new)
                     (remote-file-sum remote) (remote-file-sum new)
                     (remote-file-sent remote) (tuatara.buffer:buffer-content-number buffer))
               (tuatara.buffer:buffer-mark-saved buffer)
               (parse-afresh buffer)
               (message "Read ~a again~@[; ~a~]" (buffer-remote-name buffer) said))))))

;;; Joined to the rest of the editor

(defun install-remote-files ()
  "Have the copies of files on other machines sent there when saved, and refused a save when they are evidence."
  (pushnew 'remote-save-refusal *save-refusals*)
  (pushnew 'remote-after-save *after-save-functions*)
  (pushnew 'close-remote-links *exit-functions*))
