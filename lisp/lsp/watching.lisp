;;;; lisp/lsp/watching.lisp --- Telling language servers of files changed on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server knows the files open in the editor from what is sent; of the rest
;;;; of its project it knows what it read. When files change on disk without the
;;;; editor (a git checkout, a build writing sources, another editor), a server
;;;; that asks to be told (it registers the files it wants watched, by patterns
;;;; such as **/*.rs) is told which were made, changed or deleted. The project's
;;;; directories are found once in the background, those *LSP-WATCH-SKIPPED*
;;;; names and hidden ones left out, and watched as the editor watches
;;;; directories; a directory found changed is listed again and compared with
;;;; what it held, in the background too.

(in-package #:tuatara.lsp)

(define-setting *lsp-watch-files* t
  "Whether language servers that ask to be told of their project's files
changed on disk, made or deleted, by programs other than the editor, are
told.
- t: they are told.
- nil: they are not."
  :values boolean)

(define-setting *lsp-watch-skipped* '("target" "node_modules" "__pycache__" "_build" "zig-out" "zig-cache" "dist")
  "The directories of a project whose files are never watched for its language
servers (hidden directories, whose names begin with a dot, never are): what a
build makes.
- a list of texts: the directories' names, \"target\", \"node_modules\",
  \"__pycache__\", \"_build\", \"zig-out\", \"zig-cache\" and \"dist\" as standard."
  :values (list-of string))

(defvar *lsp-watched-most* 5000
  "The most directories of one project watched for its language servers.")

(define-record (watched-tree (:constructor make-watched-tree (root)) (:copier nil))
  "The directories of the project at ROOT watched for its servers: DIRECTORIES,
native names ending in /, and STAMPS, by directory, a table of what each holds,
by name, a file's mark (FILE-MARK) or :DIRECTORY; READY once they were found.
Once ready, it is changed only by the work on what changed (WATCHED-CHANGES),
one at a time."
  (root nil :documentation "The root directory of the project WATCHED-TREE watches.")
  (directories '() :documentation "The directories of WATCHED-TREE as native names ending in /.")
  (stamps (make-hash-table :test 'equal)
          :documentation "A table by directory of what each holds, by name: a file's mark (FILE-MARK) or :DIRECTORY.")
  (ready nil :documentation "True once the directories of WATCHED-TREE were found."))

(defvar *lsp-watched-trees* (make-hash-table :test 'equal)
  "The projects watched for language servers, each a WATCHED-TREE by its root.")

(defvar *lsp-watchers* (make-hash-table :test 'eq)
  "What each language server asked to be told of, by server: a list of
\(ID . PATTERNS), ID the registration's, PATTERNS each (GLOBS . KINDS), GLOBS
the globs without braces a pattern stands for (GLOB-ALTERNATIVES), KINDS
the protocol's mask of made (1), changed (2) and deleted (4).")

;;; Patterns

(defun watcher-patterns (server watchers)
  "WATCHERS, the FileSystemWatchers SERVER registered, as (GLOBS . KINDS):
a pattern relative to a directory (baseUri) put under it, else matched against
the whole path; those that cannot be read left out."
  (loop for w across (tuatara.text:json-items watchers)
        for glob = (tuatara.text:json-get w "globPattern")
        for pattern = (cond ((stringp glob) glob)
                            ((hash-table-p glob)
                             (let* ((base (tuatara.text:json-get glob "baseUri"))
                                    (base (if (hash-table-p base) (tuatara.text:json-get base "uri") base))
                                    (path (or (uri-path base) (lsp-server-root server))))
                               (format nil "~a/~a" (string-right-trim "/" path)
                                       (or (tuatara.text:json-text glob "pattern") "**")))))
        for globs = (and pattern (glob-alternatives pattern))
        when globs
        collect (cons globs (or (tuatara.text:json-integer w "kind") 7))))

(define-lsp-request "client/registerCapability" (server params)
  (loop for r across (tuatara.text:json-items params "registrations")
        when (equal (tuatara.text:json-text r "method") "workspace/didChangeWatchedFiles")
        do (push (cons (tuatara.text:json-get r "id")
                       (watcher-patterns server (tuatara.text:json-get r "registerOptions" "watchers")))
                 (gethash server *lsp-watchers*))
        (watch-project (lsp-server-root server)))
  :null)

(define-lsp-request "client/unregisterCapability" (server params)
  ;; the protocol spells its key so
  (loop for u across (if (vectorp (tuatara.text:json-get params "unregisterations"))
                         (tuatara.text:json-items params "unregisterations")
                         (tuatara.text:json-items params "unregistrations"))
        do (setf (gethash server *lsp-watchers*)
                 (remove (tuatara.text:json-get u "id") (gethash server *lsp-watchers*) :key #'car :test #'equal)))
  :null)

;;; The project's directories

(defun project-directories (root)
  "The directories under ROOT, ROOT first and those nearer it before those further
down (WALK-DIRECTORY), as native names ending in /, and what each holds (a
table by directory, as WATCHED-TREE's STAMPS): hidden ones and those
*LSP-WATCH-SKIPPED* names left out, *LSP-WATCHED-MOST* at most. Two values."
  (let ((stamps (make-hash-table :test 'equal)) (found '()))
    (walk-directory root
                    (lambda (directory relative entry)
                      (declare (ignore relative))
                      (if entry
                          (not (watch-skipped-p (first entry)))
                          (push directory found)))
                    :list (lambda (directory)
                            (let ((entries (ignoring-errors-reported ("reading ~a" directory)
                                             (tuatara.io:directory-entries directory :stamps t))))
                              (setf (gethash directory stamps) (entries-contents entries))
                              entries))
                    :breadth-first t :follow-links t :limit *lsp-watched-most*)
    (values (nreverse found) stamps)))

(defun watch-skipped-p (name)
  "True when the directory called NAME is never watched: hidden, or in *LSP-WATCH-SKIPPED*."
  (or (and (plusp (length name)) (char= (char name 0) #\.))
      (member name *lsp-watch-skipped* :test #'string=)))

(defun file-mark (stamp)
  "A whole number standing for the size and the time of change STAMP gives a
file, which differs when its content has changed, as far as the system
tells; kept for each file watched in place of its STAMP, a tenth of the
room."
  (logand (logxor (tuatara.io:stamp-mtime stamp) (* (tuatara.io:stamp-size stamp) 1000003)) most-positive-fixnum))

(defun directory-contents (directory)
  "What DIRECTORY holds, a table by name of each file's mark (FILE-MARK), or
:DIRECTORY for a directory; NIL when it cannot be read."
  (entries-contents (ignoring-errors-reported ("reading ~a" directory)
                      (tuatara.io:directory-entries directory :stamps t))))

(defun entries-contents (entries)
  "ENTRIES of a directory, as TUATARA.IO:DIRECTORY-ENTRIES gives them with stamps,
as DIRECTORY-CONTENTS gives them: a table by name of each file's mark, or
:DIRECTORY for a directory; NIL for none."
  (when entries
    (let ((table (make-hash-table :test 'equal :size (length entries))))
      (loop for (name kind nil nil stamp) in entries
            do (setf (gethash name table) (cond ((eq kind :directory) :directory)
                                                (stamp (file-mark stamp))
                                                (t 0))))
      table)))

(defun watch-project (root)
  "Have the project at ROOT watched for its servers: its directories found in the
background, once."
  (when (and *lsp-watch-files* (not (gethash root *lsp-watched-trees*)))
    (let ((tree (make-watched-tree root)))
      (setf (gethash root *lsp-watched-trees*) tree)
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (multiple-value-bind (directories stamps) (project-directories root)
           (setf (watched-tree-directories tree) directories
                 (watched-tree-stamps tree) stamps
                 (watched-tree-ready tree) t)
           (directories-followed-changed)))
       :name "tuatara project directories"))))

(defvar *lsp-watched-list* (cons nil nil)
  "The directories watched for language servers, as (TREES . DIRECTORIES), made
again only when the trees watched change.")

(defun lsp-watched-directories ()
  "The directories watched for the language servers, the same list while the
projects watched and their directories are the same."
  ;; projects no running server watches let go
  (maphash (lambda (root tree)
             (declare (ignore tree))
             (unless (some (lambda (s) (and (string= (lsp-server-root s) root) (gethash s *lsp-watchers*)))
                           *lsp-servers*)
               (remhash root *lsp-watched-trees*)))
           *lsp-watched-trees*)
  (maphash (lambda (server watchers)
             (declare (ignore watchers))
             (unless (member server *lsp-servers*) (remhash server *lsp-watchers*)))
           *lsp-watchers*)
  (let ((key (loop for tree being the hash-values of *lsp-watched-trees*
                   when (watched-tree-ready tree) collect (watched-tree-directories tree))))
    (unless (and (= (length key) (length (car *lsp-watched-list*))) (every #'eq key (car *lsp-watched-list*)))
      (setf *lsp-watched-list* (cons key (loop for list in key append list))))
    (cdr *lsp-watched-list*)))

;;; What changed

(defun directory-changes (tree directory)
  "What changed in DIRECTORY of TREE since it was last listed, each (PATH
TYPE), TYPE 1 made, 2 changed, 3 deleted; its table brought up to date, a
directory made watched (its files made), one gone let go (its files deleted)."
  (let ((old (gethash directory (watched-tree-stamps tree)))
        (new (directory-contents directory))
        (changes '()))
    (flet ((path (name) (format nil "~a~a" directory name)))
      (when new
        (maphash (lambda (name what)
                   (let ((before (and old (gethash name old))))
                     (cond ((eq what :directory)
                            (when (and (null before) (not (watch-skipped-p name)))
                              (setf changes (append (directory-made tree (format nil "~a/" (path name))) changes))))
                           ((null before) (push (list (path name) 1) changes))
                           ((not (eql before what)) (push (list (path name) 2) changes)))))
                 new))
      (when old
        (maphash (lambda (name what)
                   (unless (and new (gethash name new))
                     (if (eq what :directory)
                         (let ((dir (format nil "~a/" (path name))))
                           (setf changes (append (directory-gone tree dir) changes)))
                         (push (list (path name) 3) changes))))
                 old))
      (if new
          (setf (gethash directory (watched-tree-stamps tree)) new)
          (setf changes (append (directory-gone tree directory) changes))))
    changes))

(defun directory-made (tree directory)
  "DIRECTORY, new in TREE: watched, its files made, as DIRECTORY-CHANGES gives them."
  (let ((inside (directory-contents directory))
        (changes '()))
    (setf (watched-tree-directories tree) (append (watched-tree-directories tree) (list directory))
          (gethash directory (watched-tree-stamps tree)) inside)
    (directories-followed-changed)
    (when inside
      (maphash (lambda (name what)
                 (unless (eq what :directory)
                   (push (list (format nil "~a~a" directory name) 1) changes)))
               inside))
    changes))

(defun directory-gone (tree directory)
  "DIRECTORY of TREE, and the directories under it, gone: let go, their files deleted, as DIRECTORY-CHANGES gives them."
  (let ((changes '()))
    (dolist (d (remove-if-not (lambda (d) (uiop:string-prefix-p directory d)) (watched-tree-directories tree)))
      (let ((held (gethash d (watched-tree-stamps tree))))
        (when held
          (maphash (lambda (n w) (unless (eq w :directory) (push (list (format nil "~a~a" d n) 3) changes))) held)))
      (remhash d (watched-tree-stamps tree)))
    (setf (watched-tree-directories tree)
          (remove-if (lambda (d) (uiop:string-prefix-p directory d)) (watched-tree-directories tree)))
    (directories-followed-changed)
    changes))

(defun server-watched-changes (registrations root changes)
  "Those of CHANGES, each (PATH TYPE) as DIRECTORY-CHANGES gives them, in the
project at ROOT that REGISTRATIONS, what its server asked to be told of,
match, as the protocol's FileEvents."
  (loop for (path type) in changes
        when (and (uiop:string-prefix-p root path)
                  (loop for (nil . patterns) in registrations
                        thereis (loop for (globs . kinds) in patterns
                                      thereis (and (logtest kinds (ash 1 (1- type)))
                                                   (some (lambda (glob) (glob-match glob path)) globs)))))
        collect (tuatara.text:json-object "uri" (path-uri path) "type" type)))

(defun watched-changes (trees watchers changed)
  "What each server of WATCHERS, a list of (SERVER ROOT . REGISTRATIONS), is
to be told of the files of TREES changed on disk in CHANGED, the directories
found changed: a list of (SERVER . FILE-EVENTS). The directories are listed
again and the trees brought up to date (DIRECTORY-CHANGES): work for the
background."
  (let ((changes '()))
    (dolist (tree trees)
      (dolist (directory changed)
        (when (and (uiop:string-prefix-p (watched-tree-root tree) directory)
                   (gethash directory (watched-tree-stamps tree)))
          (setf changes (append (directory-changes tree directory) changes)))))
    (when changes
      (loop for (server root . registrations) in watchers
            for events = (server-watched-changes registrations root changes)
            when events
            collect (cons server events)))))

(defvar *watch-work* nil
  "While the directories found changed are looked at in the background
\(LSP-WATCHED-CHANGED), a list of those found changed meanwhile, waiting
for it; NIL otherwise.")

(defun lsp-watched-changed (changed)
  "Tell each language server that asked of the files changed on disk in
CHANGED, the directories the watcher found changed, those its patterns match:
the directories listed and compared with what they held in the background,
the servers told once that is done. Directories found changed while it is
under way wait for it."
  (if *watch-work*
      (setf (rest *watch-work*) (union changed (rest *watch-work*) :test #'string=))
      (let ((trees (loop for tree being the hash-values of *lsp-watched-trees*
                         when (watched-tree-ready tree) collect tree))
            (watchers (loop for server being the hash-keys of *lsp-watchers* using (hash-value registrations)
                            when (eq (lsp-server-status server) :ready)
                            collect (list* server (lsp-server-root server) (copy-list registrations)))))
        (setf *watch-work* (list t))
        (lsp-in-background
         (lambda () (watched-changes trees watchers changed))
         (lambda (told)
           (let ((waiting (rest *watch-work*)))
             (setf *watch-work* nil)
             (loop for (server . events) in told
                   when (eq (lsp-server-status server) :ready)
                   do (lsp-notify server "workspace/didChangeWatchedFiles"
                                  (tuatara.text:json-object "changes" (coerce events 'vector))))
             (when waiting
               (lsp-watched-changed waiting))))))))

(defun install-lsp-watching ()
  "Watch the directories of the projects whose language servers asked to be told of their files changed on disk."
  (follow-directories-for 'lsp-watched-directories 'lsp-watched-changed :tells t))
