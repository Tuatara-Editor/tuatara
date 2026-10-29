;;;; lisp/lsp/watching.lisp --- Telling language servers of files changed on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server knows the files open in the editor from what is sent; of the
;;;; rest of its project it knows what it read. When files change on disk
;;;; without the editor (a git checkout, a build writing sources, another
;;;; editor), a server that asks to be told (it registers the files it wants
;;;; watched, by patterns such as **/*.rs) is told which were made, changed or
;;;; deleted. The project's folders are found once in the background, those
;;;; *LSP-WATCH-SKIPPED* names and hidden ones left out, and watched as the
;;;; editor watches folders; a folder found changed is listed again and
;;;; compared with what it held.

(in-package #:tuatara.defaults)

(defvar *lsp-watch-files* t
  "True for language servers that ask to be told of their project's files
changed on disk, made or deleted, by programs other than the editor, to be
told.")

(defvar *lsp-watch-skipped* '("target" "node_modules" "__pycache__" "_build" "zig-out" "zig-cache" "dist")
  "The folders of a project whose files are never watched for its language
servers, by name (hidden folders, whose names begin with a dot, never are):
what a build makes.")

(defvar *lsp-watched-most* 5000
  "The most folders of one project watched for its language servers.")

(defstruct (watched-tree (:constructor make-watched-tree (root)) (:copier nil))
  "The folders of the project at ROOT watched for its servers: DIRECTORIES,
native names ending in /, and STAMPS, by folder, a table of what each holds,
by name, a file's STAMP or :DIRECTORY; READY once they were found."
  root (directories '()) (stamps (make-hash-table :test 'equal)) (ready nil))

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
a pattern relative to a folder (baseUri) put under it, else matched against
the whole path; those that cannot be read left out."
  (loop for w across (tuatara.text:json-items watchers)
        for glob = (tuatara.text:json-get w "globPattern")
        for pattern = (cond ((stringp glob) glob)
                            ((hash-table-p glob)
                             (let* ((base (tuatara.text:json-get glob "baseUri"))
                                    (base (if (hash-table-p base) (tuatara.text:json-get base "uri") base))
                                    (path (or (uri-path base) (lsp-server-root server))))
                               (format nil "~a/~a" (string-right-trim "/" path) (or (tuatara.text:json-text glob "pattern") "**")))))
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

;;; The project's folders

(defun project-folders (root)
  "The folders under ROOT, ROOT first and those nearer it before those further
down (WALK-DIRECTORY), as native names ending in /, and what each holds (a
table by folder, as WATCHED-TREE's STAMPS): hidden ones and those
*LSP-WATCH-SKIPPED* names left out, *LSP-WATCHED-MOST* at most. Two values."
  (let ((stamps (make-hash-table :test 'equal)) (found '()))
    (walk-directory root
                    (lambda (directory relative entry)
                      (declare (ignore relative))
                      (if entry
                          (not (watch-skipped-p (first entry)))
                          (push directory found)))
                    :list (lambda (directory)
                            (let ((entries (ignore-errors (tuatara.io:directory-entries directory :stamps t))))
                              (setf (gethash directory stamps) (entries-contents entries))
                              entries))
                    :breadth-first t :follow-links t :limit *lsp-watched-most*)
    (values (nreverse found) stamps)))

(defun watch-skipped-p (name)
  "True when the folder called NAME is never watched: hidden, or in *LSP-WATCH-SKIPPED*."
  (or (and (plusp (length name)) (char= (char name 0) #\.))
      (member name *lsp-watch-skipped* :test #'string=)))

(defun folder-contents (directory)
  "What DIRECTORY holds, a table by name of each file's STAMP, or :DIRECTORY
for a folder; NIL when it cannot be read."
  (entries-contents (ignore-errors (tuatara.io:directory-entries directory :stamps t))))

(defun entries-contents (entries)
  "ENTRIES of a folder, as TUATARA.IO:DIRECTORY-ENTRIES gives them with stamps,
as FOLDER-CONTENTS gives them: a table by name of each file's STAMP, or
:DIRECTORY for a folder; NIL for none."
  (when entries
    (let ((table (make-hash-table :test 'equal)))
      (loop for (name kind nil nil stamp) in entries
            do (setf (gethash name table) (if (eq kind :directory) :directory stamp)))
      table)))

(defun watch-project (root)
  "Have the project at ROOT watched for its servers: its folders found in the
background, once."
  (when (and *lsp-watch-files* (not (gethash root *lsp-watched-trees*)))
    (let ((tree (make-watched-tree root)))
      (setf (gethash root *lsp-watched-trees*) tree)
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (multiple-value-bind (directories stamps) (project-folders root)
           (setf (watched-tree-directories tree) directories
                 (watched-tree-stamps tree) stamps
                 (watched-tree-ready tree) t)))
       :name "tuatara project folders"))))

(defvar *lsp-watched-list* (cons nil nil)
  "The folders watched for language servers, as (TREES . DIRECTORIES), made
again only when the trees watched change.")

(defun lsp-watched-directories ()
  "The folders watched for the language servers, the same list while the
projects watched and their folders are the same."
  ;; projects no running server watches let go
  (maphash (lambda (root tree)
             (declare (ignore tree))
             (unless (some (lambda (s) (and (string= (lsp-server-root s) root) (gethash s *lsp-watchers*))) *lsp-servers*)
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

(defun folder-changes (tree directory)
  "What changed in DIRECTORY of TREE since it was last listed, each (PATH
TYPE), TYPE 1 made, 2 changed, 3 deleted; its table brought up to date, a
folder made watched (its files made), one gone let go (its files deleted)."
  (let ((old (gethash directory (watched-tree-stamps tree)))
        (new (folder-contents directory))
        (changes '()))
    (flet ((path (name) (format nil "~a~a" directory name)))
      (when new
        (maphash (lambda (name what)
                   (let ((before (and old (gethash name old))))
                     (cond ((eq what :directory)
                            (when (and (null before) (not (watch-skipped-p name)))
                              (setf changes (append (folder-made tree (format nil "~a/" (path name))) changes))))
                           ((null before) (push (list (path name) 1) changes))
                           ((not (tuatara.io:same-content-p before what)) (push (list (path name) 2) changes)))))
                 new))
      (when old
        (maphash (lambda (name what)
                   (unless (and new (gethash name new))
                     (if (eq what :directory)
                         (let ((folder (format nil "~a/" (path name))))
                           (setf changes (append (folder-gone tree folder) changes)))
                         (push (list (path name) 3) changes))))
                 old))
      (if new
          (setf (gethash directory (watched-tree-stamps tree)) new)
          (setf changes (append (folder-gone tree directory) changes))))
    changes))

(defun folder-made (tree folder)
  "FOLDER, new in TREE: watched, its files made, as FOLDER-CHANGES gives them."
  (let ((inside (folder-contents folder))
        (changes '()))
    (setf (watched-tree-directories tree) (append (watched-tree-directories tree) (list folder))
          (gethash folder (watched-tree-stamps tree)) inside)
    (when inside
      (maphash (lambda (name what) (unless (eq what :directory) (push (list (format nil "~a~a" folder name) 1) changes)))
               inside))
    changes))

(defun folder-gone (tree folder)
  "FOLDER of TREE, and the folders under it, gone: let go, their files deleted, as FOLDER-CHANGES gives them."
  (let ((changes '()))
    (dolist (d (remove-if-not (lambda (d) (uiop:string-prefix-p folder d)) (watched-tree-directories tree)))
      (let ((held (gethash d (watched-tree-stamps tree))))
        (when held
          (maphash (lambda (n w) (unless (eq w :directory) (push (list (format nil "~a~a" d n) 3) changes))) held)))
      (remhash d (watched-tree-stamps tree)))
    (setf (watched-tree-directories tree)
          (remove-if (lambda (d) (uiop:string-prefix-p folder d)) (watched-tree-directories tree)))
    changes))

(defun lsp-watched-changed (changed)
  "Tell each language server that asked of the files changed on disk in
CHANGED, the folders the watcher found changed, those its patterns match."
  (let ((changes '()))
    (maphash (lambda (root tree)
               (when (watched-tree-ready tree)
                 (dolist (directory changed)
                   (when (and (uiop:string-prefix-p root directory) (gethash directory (watched-tree-stamps tree)))
                     (setf changes (append (folder-changes tree directory) changes))))))
             *lsp-watched-trees*)
    (when changes
      (maphash (lambda (server registrations)
                 (let ((mine (loop for (path type) in changes
                                   when (and (uiop:string-prefix-p (lsp-server-root server) path)
                                             (loop for (nil . patterns) in registrations
                                                   thereis (loop for (globs . kinds) in patterns
                                                                 thereis (and (logtest kinds (ash 1 (1- type)))
                                                                              (some (lambda (glob) (glob-match glob path)) globs)))))
                                     collect (tuatara.text:json-object "uri" (path-uri path) "type" type))))
                   (when (and mine (eq (lsp-server-status server) :ready))
                     (lsp-notify server "workspace/didChangeWatchedFiles"
                                 (tuatara.text:json-object "changes" (coerce mine 'vector))))))
               *lsp-watchers*))))

(defun install-lsp-watching ()
  "Watch the folders of the projects whose language servers asked to be told of their files changed on disk."
  (follow-directories-for 'lsp-watched-directories 'lsp-watched-changed))
