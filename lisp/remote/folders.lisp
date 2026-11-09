;;;; lisp/remote/folders.lisp --- Folders on another machine listed with rimu, their files copied, renamed and deleted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A folder on another machine, /ssh:HOST:/folder/, is listed with rimu as a
;;;; folder of the disk is, read through SFTP (sftp.lisp) on rimu's own thread:
;;;; its names, kinds, sizes and times, where its links lead. Nothing else is
;;;; worked out for it: no git column, no sizes of folders, no preview, and it
;;;; is not watched for changes. Its files are copied (C), renamed (R) and
;;;; deleted (D, x) through the client, as rimu does those on the disk, a job
;;;; whose every request has *REMOTE-SECONDS*; copies go from here to there,
;;;; from there to here, and between two folders there. A server keeps no
;;;; trash: what is deleted there is deleted for good, after a plain yes. A
;;;; folder of evidence there (*EVIDENCE-FOLDERS*, as REMOTE-EVIDENCE tells
;;;; it) is listed opened safely and nothing is written in it, nor is a folder
;;;; holding one deleted or moved.

(in-package #:tuatara.defaults)

;;; Names

(defun remote-directory-name (name)
  "NAME, of a folder on another machine (/ssh:HOST:PATH), as rimu keeps it,
its path worked out by its words (REMOTE-PATH-NORMAL) and ending in /, or
nothing after /ssh:HOST: for the home folder there."
  (multiple-value-bind (host path) (remote-name-parts name)
    (remote-name host (if (string= path "") "" (tuatara.io:slash-ended path)))))

(defun remote-entry-name-p (name)
  "True when NAME, given by a server as an entry of a folder, can be one: not
empty, not . nor .., with no / nor NUL in it, so that a name joined to its
folder never leads out of it, nor ends early."
  (and (plusp (length name)) (not (member name '("." "..") :test #'string=)) (not (find #\/ name))
       (not (find (code-char 0) name))))

(defun remote-join (folder name)
  "The name of the entry NAME of FOLDER, here or on another machine, one /
between them; NAME alone after a /ssh:HOST: naming the home folder there."
  (if (or (uiop:string-suffix-p folder "/") (eql (remote-prefix-length folder) (length folder)))
      (concatenate 'string folder name)
      (concatenate 'string folder "/" name)))

;;; Listed with rimu

(defstruct (remote-rimu-source (:constructor make-remote-rimu-source (host)) (:copier nil))
  "Where a rimu list of a folder on another machine reads it: HOST, as ssh
takes it; EVIDENCE, true once a folder it listed was found to be evidence
\(REMOTE-EVIDENCE-WORDS)."
  (host "" :type string :read-only t)
  (evidence nil))

(setf (documentation 'remote-rimu-source-p 'function) "True when OBJECT is a REMOTE-RIMU-SOURCE."
      (documentation 'remote-rimu-source-host 'function) "The machine SOURCE lists folders of, as ssh takes it."
      (documentation 'remote-rimu-source-evidence 'function)
      "True once a folder SOURCE listed was found to be evidence.")

(defun remote-listed (name attributes &optional target target-attributes)
  "An entry of a folder on another machine, NAME with ATTRIBUTES
\(SFTP-TAKE-ATTRIBUTES), as rimu lists one (TUATARA.IO:LISTED); for a link,
TARGET, what it holds, and TARGET-ATTRIBUTES, those of where it leads (NIL:
nowhere)."
  (tuatara.io:make-listed name (tuatara.text:string-octets name) (remote-stamp attributes)
                          (or (getf attributes :size) 0) nil target
                          (and target-attributes (remote-stamp target-attributes))))

(defun remote-link-details (host folder names)
  "For each of NAMES, links in FOLDER on HOST, a path ending in /, what it
holds and the attributes of where it leads (NIL: nowhere), as a list of
\(TARGET . ATTRIBUTES)."
  (call-with-sftp
   host folder "examine"
   (lambda (link)
     (mapcar (lambda (name)
               (let ((path (concatenate 'string folder name)))
                 (tuatara.runtime:job-check-stop)
                 (cons (let ((target (handler-case
                                         (multiple-value-bind (answer names)
                                             (sftp-ask link :readlink (list path) path "examine")
                                           (and (eq answer :name) (= 1 (length names)) (car (first names))))
                                       (sftp-refusal () nil))))
                         ;; shown, never followed by its words
                         (and target (if (remote-clean-name-p target) target (escape-controls target))))
                       (sftp-attributes link path))))
             names))))

(defun remote-rimu-listing (source directory)
  "DIRECTORY, a folder on another machine as REMOTE-DIRECTORY-NAME writes
it, read into a RIMU-LISTING, its name from the root as the server says it:
. and .. first, then its entries, those past *SFTP-MOST-LISTED* left out and
that said; a name no entry can have (REMOTE-ENTRY-NAME-P) left out. Why
not, in the listing, when it cannot be read. SOURCE, the list's
REMOTE-RIMU-SOURCE, is told when the folder is evidence."
  (multiple-value-bind (host path) (remote-name-parts directory)
    (handler-case
        (let* ((path (if (uiop:string-prefix-p "/" path)
                         path
                         (tuatara.io:slash-ended (remote-real-path host path))))
               (folder (remote-name host path)))
          (when (remote-evidence-words host path)
            (setf (remote-rimu-source-evidence source) t))
          (multiple-value-bind (entries cut) (remote-listing host path)
            (let* ((entries (remove-if-not #'remote-entry-name-p entries :key #'car))
                   (links (remove-if-not (lambda (entry) (eq :link (remote-kind (cdr entry)))) entries))
                   (details (remote-link-details host path (mapcar #'car links)))
                   (here (list :permissions #o40755)))
              (make-rimu-listing
               folder
               (coerce (list* (remote-listed "." here) (remote-listed ".." here)
                              (mapcar (lambda (entry)
                                        (let ((link (position entry links)))
                                          (if link
                                              (let ((detail (nth link details)))
                                                (remote-listed (car entry) (cdr entry) (or (car detail) "") (cdr detail)))
                                              (remote-listed (car entry) (cdr entry)))))
                                      entries))
                       'simple-vector)
               nil
               (and cut (format nil "only the first ~:d entries are listed" *sftp-most-listed*))))))
      ((or tuatara.io:file-system-error tuatara.runtime:refusal) (condition)
        (make-rimu-listing directory (vector) nil (princ-to-string condition))))))

(defvar *remote-preview-size-limit* (* 20 1000 1000)
  "The most octets a file on another machine may hold to be shown in the
preview of its list (\\<rimu-mode>\\[list-toggle-preview]), 20 MB by default: all
of it has to come from the server first. The preview of a larger one says so.")

(defmethod rimu-source-listing ((source remote-rimu-source) directory)
  "The folder as the server lists it (REMOTE-RIMU-LISTING)."
  (remote-rimu-listing source directory))

(defmethod rimu-source-call-with-entry ((source remote-rimu-source) directory octets function &key cancel)
  "The file read from the server into a copy here readable by you alone, gone
when FUNCTION returns. Refused for a file of evidence, which is not read unasked,
and one larger than *REMOTE-PREVIEW-SIZE-LIMIT*."
  (multiple-value-bind (host path)
      (remote-name-parts (concatenate 'string directory (tuatara.text:octets-string octets)))
    (let ((*remote-cancel* cancel))
      (let ((evidence (or (remote-rimu-source-evidence source) (remote-evidence-words host path))))
        (when evidence
          (refuse "not previewed, as it is evidence~@[: ~a~]" (and (stringp evidence) evidence))))
      (let ((folder (new-private-folder (private-cache-directory "remote/"))))
        (unwind-protect
             (multiple-value-bind (fd copy)
                 (tuatara.io:create-free-file folder (lambda (n) (declare (ignore n)) "preview"))
               (unwind-protect
                    (handler-case (remote-download host path fd :most *remote-preview-size-limit*)
                      (remote-too-large (condition)
                        (refuse "Too large to preview: ~a, the limit is ~a (*remote-preview-size-limit*). ~
                                 RET opens all of it"
                                (octets-words (remote-too-large-size condition)) (octets-words *remote-preview-size-limit*))))
                 (sb-posix:close fd))
               (tuatara.io:call-with-file copy function))
          (delete-private-folder folder))))))

(defmethod rimu-source-open ((source remote-rimu-source) buffer entry)
  "A folder listed in its place; a file read from the server and visited as
\\[find-file] visits one, safely when the list was opened safely."
  (let ((path (rimu-path (rimu-list-directory buffer) entry)))
    (if (rimu-directory-p entry)
        (rimu-enter buffer (tuatara.io:slash-ended path))
        (visit-path path :safe (tuatara.io:safe-visit-p buffer)))))

(defmethod rimu-source-shown ((source remote-rimu-source) buffer)
  "The list opened safely from then on once a folder it listed was found to be evidence."
  (when (remote-rimu-source-evidence source)
    (setf (tuatara.io:safe-visit-p buffer) t)))

(defmethod rimu-source-top ((source remote-rimu-source))
  "The root of the machine."
  (remote-name (remote-rimu-source-host source) "/"))

(defmethod rimu-source-buffer-name ((source remote-rimu-source) directory)
  "HOST: and the folder's name."
  (multiple-value-bind (host path) (remote-name-parts directory)
    (format nil "~a:~a" host (if (string= path "") "" (rimu-buffer-name path)))))

(defmethod rimu-source-taken ((source remote-rimu-source) buffer)
  "The preview off to begin with: each file it shows has to come from the server."
  (setf (list-preview-p buffer) nil))

(defmethod rimu-source-changes ((source remote-rimu-source))
  "Copies, moves within the machine, renames and deletions."
  '(rimu-copy rimu-move rimu-rename list-delete-marked list-execute))

(defmethod rimu-source-refusal-words ((source remote-rimu-source) command)
  "COMMAND works on files here only."
  (format nil "~(~a~) works on files here, not on ~a" command (remote-rimu-source-host source)))

(defun remote-rimu-list (directory)
  "The rimu list of DIRECTORY, a folder on another machine as
REMOTE-DIRECTORY-NAME writes it, there is, else a new one, opened safely
when the folder is evidence by its name (EVIDENCE-PATH-P), and once read
when it is found to be by the server's word (REMOTE-RIMU-LISTING)."
  (or (find-if (lambda (buffer)
                 (and (rimu-list-p buffer)
                      (remote-rimu-source-p (rimu-source buffer))
                      (not (tuatara.buffer:buffer-local buffer 'rimu-preview-buffer))
                      (equal (rimu-list-directory buffer) directory)))
               tuatara.buffer:*buffers*)
      (let* ((source (make-remote-rimu-source (remote-name-parts directory)))
             (buffer (rimu-make-list directory :name (rimu-source-buffer-name source directory))))
        (setf (tuatara.buffer:buffer-local buffer 'rimu-rewound) source)
        (rimu-source-taken source buffer)
        (when (evidence-path-p directory)
          (setf (tuatara.io:safe-visit-p buffer) t))
        buffer)))

;;; Copied, moved and deleted

(defun remote-or-local-kind (name)
  "What NAME, here or on another machine, names, a link not followed:
:DIRECTORY, :FILE, :LINK or :OTHER; NIL when nothing is there."
  (if (remote-name-p name)
      (multiple-value-bind (host path) (remote-name-parts name)
        (let ((attributes (remote-attributes host path :follow nil)))
          (and attributes (or (remote-kind attributes) :other))))
      (case (tuatara.io:path-kind name :follow nil)
        ((nil) nil)
        (:directory :directory)
        (:regular :file)
        (:symlink :link)
        (t :other))))

(defun remote-or-local-names (folder)
  "The names of the entries of FOLDER, here or on another machine, . and ..
left out; refused when the server lists more than *SFTP-MOST-LISTED*, or a
name no entry can have (REMOTE-ENTRY-NAME-P)."
  (if (remote-name-p folder)
      (multiple-value-bind (host path) (remote-name-parts folder)
        (multiple-value-bind (entries cut) (remote-listing host (if (string= path "") "." (tuatara.io:slash-ended path)))
          (when cut (refuse "~a holds more than ~:d entries" folder *sftp-most-listed*))
          (dolist (entry entries)
            (unless (remote-entry-name-p (car entry))
              (refuse "~a lists a name no entry can have: ~a" folder (escape-controls (car entry)))))
          (mapcar #'car entries)))
      (remove-if (lambda (name) (member name '("." "..") :test #'string=))
                 (mapcar #'first (tuatara.io:directory-entries folder)))))

(defun remote-copy-down (from to)
  "Copy the file FROM, on another machine, to TO, a new name here, readable
by you alone: read into a new file beside TO, given TO's name once whole,
never over a file there; taken away when the copy fails or is stopped."
  (multiple-value-bind (host path) (remote-name-parts from)
    (multiple-value-bind (fd temporary)
        (tuatara.io:create-free-file (tuatara.io:parent-path to)
                                     (lambda (n) (format nil ".~a.~d.part" (tuatara.io:native-file-name to) n)))
      (let ((done nil))
        (unwind-protect
             (progn (unwind-protect (remote-download host path fd) (sb-posix:close fd))
                    (tuatara.io:rename-path temporary to)
                    (setf done t))
          (unless done
            (ignoring-errors-reported ("taking away ~a" temporary) (tuatara.io:remove-file temporary))))))))

(defun remote-copy-up (from to)
  "Copy the file FROM, here, to TO, a new name on another machine, with
FROM's permissions, never over a file there (REMOTE-UPLOAD)."
  (multiple-value-bind (host path) (remote-name-parts to)
    (let ((fd (tuatara.io:open-for-reading from nil :accept '(:regular))))
      (unwind-protect
           (let ((stamp (tuatara.io:fd-stamp fd from)))
             (remote-upload host path fd (tuatara.io:stamp-size stamp)
                            :permissions (logand (tuatara.io:stamp-mode stamp) #o777)
                            :expected (lambda (attributes) (and attributes "a file of that name is there"))))
        (sb-posix:close fd)))))

(defun remote-copy-file (from to)
  "Copy the file FROM to TO, a new name, either or both on another machine;
between two on other machines through a private copy here, deleted after."
  (cond ((not (and (remote-name-p from) (remote-name-p to)))
         (if (remote-name-p from) (remote-copy-down from to) (remote-copy-up from to)))
        (t (let ((folder (new-private-folder (private-cache-directory "remote/"))))
             (unwind-protect
                  (let ((copy (tuatara.io:path-join folder "copy")))
                    (remote-copy-down from copy)
                    (remote-copy-up copy to))
               (delete-private-folder folder))))))

(defun remote-copy-tree (from to skip)
  "Copy FROM, a file or a folder with all in it, to the new name TO, either
or both on another machine; a link, and what is neither file nor folder,
left out, SKIP called with its name and words saying why."
  (tuatara.runtime:job-check-stop)
  (let ((kind (remote-or-local-kind from)))
    (case kind
      ((nil) (refuse "it is not there"))
      (:directory
       (if (remote-name-p to)
           (multiple-value-bind (host path) (remote-name-parts to) (remote-change host :mkdir path))
           (tuatara.io:make-directory to))
       (dolist (name (remote-or-local-names from))
         (remote-copy-tree (remote-join from name) (remote-join to name) skip)))
      (:file (remote-copy-file from to))
      (t (funcall skip from (if (eq kind :link) "a link" "neither a file nor a folder"))))))

(defparameter *remote-tree-depth* 256
  "The most levels of folders within a folder on another machine looked
through before it is deleted or moved (REMOTE-CHECK-WRITING); one deeper is
refused.")

(defun remote-check-writing (name operation &key tree)
  "Signal TUATARA.IO:WRITING-REFUSED for OPERATION on NAME, on another
machine, when it is evidence (REMOTE-EVIDENCE), told by its name and where it
really is there (REMOTE-PLACE-NAMES, refused when that is needed and cannot
be found); with TREE, NAME being a folder, also when any folder within it
is, links in it not followed, as TUATARA.IO:CHECK-TREE-WRITING does here: a
folder holding evidence is not deleted, moved or renamed as a whole, nor
one whose folders cannot all be looked through (a listing cut short, more
than *REMOTE-TREE-DEPTH* levels). Looks within only while *EVIDENCE-FOLDERS*
names some folder."
  (when (evidence-rules)
    (multiple-value-bind (host path) (remote-name-parts name)
      (labels ((check (names)
                 (multiple-value-bind (folder entry) (remote-evidence host names)
                   (when folder
                     (error 'tuatara.io:writing-refused :operation operation :path name
                                                        :reason (evidence-words folder entry)))))
               (folders (path)
                 ;; the names of the folders within, the rest of the listing let go of
                 (multiple-value-bind (entries cut) (remote-listing host path)
                   (when cut
                     (refuse "~a holds too many entries to be looked through for evidence" (remote-name host path)))
                   (loop for (entry-name . attributes) in entries
                         when (and (eq :directory (remote-kind attributes)) (remote-entry-name-p entry-name))
                           collect entry-name)))
               (walk (path real depth)
                 (tuatara.runtime:job-check-stop)
                 (when (> depth *remote-tree-depth*)
                   (refuse "~a is more than ~d folders deep to be looked through for evidence" name
                           *remote-tree-depth*))
                 (check (list path real))
                 (dolist (folder (folders path))
                   (walk (concatenate 'string path folder "/") (and real (concatenate 'string real folder "/"))
                         (1+ depth)))))
        (let* ((names (remote-place-names host path))
               (real (second names)))
          (check names)
          (when (and tree (eq :directory (remote-or-local-kind name)))
            (walk (tuatara.io:slash-ended path) (and real (tuatara.io:slash-ended real)) 0)))))))

(defun remote-into-itself-p (from to)
  "True when FROM and TO, places on other machines, are on one machine
\(SSH-MACHINE) and TO is FROM or within it, by their names or where they
really are there."
  (multiple-value-bind (from-host from-path) (remote-name-parts from)
    (multiple-value-bind (to-host to-path) (remote-name-parts to)
      (and (or (string= from-host to-host) (string= (ssh-machine from-host) (ssh-machine to-host)))
           (flet ((into-p (from to)
                    (and from to
                         (let ((from (string-right-trim "/" from)) (to (string-right-trim "/" to)))
                           (or (string= from to) (tuatara.io:path-within-p to from))))))
             (or (into-p from-path to-path)
                 (into-p (remote-place-real-path from-host from-path) (remote-place-real-path to-host to-path))))))))

(defun remote-delete-tree (name)
  "Delete NAME, on another machine, for good: a file or a link, or a folder
with all in it."
  (tuatara.runtime:job-check-stop)
  (multiple-value-bind (host path) (remote-name-parts name)
    (case (remote-or-local-kind name)
      ((nil) (refuse "it is not there"))
      (:directory
       (dolist (entry (remote-or-local-names name))
         (remote-delete-tree (remote-join name entry)))
       (remote-change host :rmdir path))
      (t (remote-change host :remove path)))))

(defun remote-rimu-work (verb from to skip)
  "Do VERB, one of rimu's (*RIMU-VERBS*), to FROM, and TO, either on another
machine: :COPY (REMOTE-COPY-TREE, SKIP told of what is left out); :MOVE
within one machine, a rename there, never over a name there; :TRASH and
:DELETE, deleting for good, as a server keeps no trash; the others refused.
Stopped between requests when the job is asked to stop, the file under way
taken away. Nothing is begun that would write in evidence there, delete or
move a folder holding it (REMOTE-CHECK-WRITING), or copy or move a folder
into itself, under any name of its machine."
  (handler-case
      (case verb
        (:copy (when (remote-name-p to)
                 (remote-check-writing to "copy")
                 (when (and (remote-name-p from) (remote-into-itself-p from to))
                   (refuse "it would be copied into itself")))
               (remote-copy-tree from to skip))
        (:move (unless (and (remote-name-p from) (remote-name-p to)
                            (string= (ssh-machine (remote-name-parts from)) (ssh-machine (remote-name-parts to))))
                 (refuse "a move is done within one machine: copy it, then delete it"))
               (remote-check-writing from "move" :tree t)
               (remote-check-writing to "move")
               (when (remote-into-itself-p from to)
                 (refuse "it would be moved into itself"))
               (multiple-value-bind (host path) (remote-name-parts from)
                 (remote-change host :rename path (nth-value 1 (remote-name-parts to)))))
        ((:trash :delete)
         (remote-check-writing from "delete" :tree t)
         (remote-delete-tree from))
        (t (refuse "rimu does not ~a files on another machine" (rimu-verb-words verb :verb))))
    (tuatara.runtime:job-stopped ()
      (refuse "stopped; the file under way taken away"))))
