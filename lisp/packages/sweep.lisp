;;;; lisp/packages/sweep.lisp --- Package sources no longer named: their folders found and deleted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The folders of the packages directory that no package or source named wants,
;;;; found and deleted in the background after asking.

(in-package #:tuatara.packages)

(defun unused-package-folders ()
  "The folders of the packages directory (PACKAGES-DIRECTORY) that no
package or source named this session wants, each (FOLDER . WORDS), FOLDER a
native name ending in /, WORDS what it holds: a source's whole folder, when
no package-source names it and no load-package (left out ones too) fetches
from it; else the folder of a package of a source that is named, when no
load-package names that package. A source a fetch or check runs for now is
left alone, as are folders whose name begins with a full stop (a fetch's
scratch) and anything that is not a folder of its own (a symbolic link)."
  (let* ((top (tuatara.io:native (packages-directory)))
         (named (append (loop for (name source) in *declared-packages* when source collect (list name source))
                        *left-out-packages*))
         (whole (loop for (nil . where) in *package-sources*
                      unless (local-source-p where) collect (tuatara.io:path-file-name where)))
         (pairs (loop for (name source) in named
                      for where = (package-where source)
                      unless (local-source-p where)
                        collect (cons (tuatara.io:path-file-name where) name)))
         (busy (busy-source-folders))
         (found '()))
    (flet ((own-folders (folder)
             ;; the folders directly in FOLDER, links never followed, nor names beginning with .
             (loop for entry in (ignoring-errors-reported ("reading ~a" folder)
                                  (directory (merge-pathnames "*/" folder) :resolve-symlinks nil))
                   for name = (car (last (pathname-directory entry)))
                   for native = (tuatara.io:native entry)
                   when (and (stringp name) (plusp (length name)) (char/= (char name 0) #\.)
                             (eq :directory (tuatara.io:path-kind (string-right-trim "/" native) :follow nil))
                             (tuatara.io:really-within-p native top))
                     collect (cons name native))))
      (dolist (source (own-folders top))
        (destructuring-bind (source-name . source-folder) source
          (cond ((or (member source-name whole :test #'string=) (member (string-right-trim "/" source-folder) busy :key (lambda (b) (string-right-trim "/" b)) :test #'string=)))
                ((find source-name pairs :key #'car :test #'string=)
                 (dolist (package (own-folders source-folder))
                   (unless (find-if (lambda (pair) (and (string= (car pair) source-name) (string= (cdr pair) (car package)))) pairs)
                     (push (cons (cdr package) (format nil "~a from ~a" (car package) (tuatara.io:percent-decoded source-name)))
                           found))))
                (t (push (cons source-folder (format nil "~a, all of it" (tuatara.io:percent-decoded source-name))) found))))))
    (nreverse found)))

(defun forget-packages-within (folder)
  "Forget the versions *FETCHED-PACKAGES* remembers of the packages kept
within FOLDER, deleted. Who signed their source is not forgotten."
  (state-list-drop *fetched-packages*
                   (loop for (key) in (state-list-entries *fetched-packages*)
                         for words = (uiop:split-string key :separator " ")
                         when (and (>= (length words) 3)
                                   (tuatara.io:path-within-p
                                    (tuatara.io:native (package-kept-folder (format nil "~{~a~^ ~}" (butlast (rest words))) (first words)))
                                    folder))
                           collect key)))

(defun package-compiled-folder (folder)
  "The folder the files of what is kept in FOLDER, of the packages directory, are compiled into."
  (directory-namestring (package-fasl (concatenate 'string (tuatara.io:native folder) "x"))))

(defstruct (package-sweep (:include tuatara.runtime:job)
                          (:constructor make-package-sweep (phase folders &optional packages)) (:copier nil))
  "Package sources no longer named, or the folders of PACKAGES, the names of
packages deleted (\\[package-delete]), being looked for (PHASE :look), or one
of them being deleted (:delete): FOLDERS, each (FOLDER WORDS OCTETS NAME),
NAME the package's for PACKAGES, what was found, or the one being deleted
followed by those still to go; FREED, the octets freed so far, and DELETED,
how many folders."
  phase folders packages (freed 0) (deleted 0))

(setf (documentation 'package-sweep-p 'function) "True when OBJECT is a PACKAGE-SWEEP.")

(tuatara.runtime:define-job-words package-sweep
  :doing (if (package-sweep-packages job) "Deleting packages" "Removing package sources no longer named")
  :verb (if (package-sweep-packages job) "delete packages" "remove package sources no longer named")
  :description (cond ((package-sweep-packages job)
                      (format nil "~:[Deleting~;Measuring~] the package~p ~{~a~^, ~}" (eq (package-sweep-phase job) :look)
                              (length (package-sweep-packages job)) (package-sweep-packages job)))
                     ((eq (package-sweep-phase job) :look) "Looking for package sources no longer named")
                     (t "Deleting package sources no longer named"))
  :left-words "nothing more deleted"
  :at-exit :quiet)

(defparameter *package-sweep-settings* '(*packages-directory* *declared-packages* *left-out-packages* *package-sources*)
  "What package-remove-unused's work in the background reads as it was on
the editor's thread when that work was started.")

(defun call-with-sweep-settings (values function)
  "Call FUNCTION with *PACKAGE-SWEEP-SETTINGS* bound to VALUES, as they were
on the editor's thread."
  (progv *package-sweep-settings* values (funcall function)))

(defun start-sweep-job (job work)
  "Start JOB, a PACKAGE-SWEEP, running WORK, a function of it, in the
background with the settings of the editor's thread now (CALL-WITH-SWEEP-SETTINGS)."
  (let ((settings (mapcar #'symbol-value *package-sweep-settings*)))
    (tuatara.runtime:start-job-thread job (lambda (job) (call-with-sweep-settings settings (lambda () (funcall work job))))
                                      :name "tuatara package sweep")))

(defun sweep-entry-octets (folder)
  "The octets FOLDER, of the packages directory or the folder packages are
compiled into, takes with the files of it compiled (FOLDER-OCTETS)."
  (+ (folder-octets folder)
     (if (tuatara.io:path-within-p folder (tuatara.io:native (packages-directory)))
         (folder-octets (package-compiled-folder folder))
         0)))

(defun folder-octets (folder)
  "The octets FOLDER and what it holds take, links not followed; 0 when it is not there."
  (if (eq :directory (tuatara.io:path-kind (string-right-trim "/" folder) :follow nil))
      (or (rimu-tree-size folder) 0)
      0))

(define-command package-remove-unused ()
  "Delete the package sources Tuatara keeps that nothing in this session
names any more: a source no package-source names and no load-package
fetches from, or a package of a source no load-package names; one left out
by :disabled or :if counts as named. What would go is found in the
background and listed with its size, then asked once, with the total; nothing
is deleted without yes. Each is looked at again just before it is deleted,
as things are then: one something names by then, or whose source is being
fetched, checked or loaded from, stays. Its files compiled go with it, and
the space freed is said. Only the packages directory Tuatara keeps is
touched, never a folder of yours, never through a link. When the init file
was not read to its end, every package named after that point would look
unused, so nothing is done until it is. Who signed a source's versions is
still remembered: a package fetched from it again is checked against that
signer."
  (if *init-file-stop*
      (message "The init file was not read to its end (~d: ~a); nothing is deleted until it is"
               (car *init-file-stop*) (cdr *init-file-stop*))
      (start-sweep-job (make-package-sweep :look nil)
                       (lambda (job)
                         (setf (package-sweep-folders job)
                               (loop for (folder . words) in (unused-package-folders)
                                     collect (list folder words (sweep-entry-octets folder))))))))

(defvar *sweeping-folder* nil
  "The folder of the packages directory package-remove-unused is deleting
now, or NIL: a package kept in it is loaded once it is done (*AFTER-SWEEP*).")

(defvar *after-sweep* '()
  "What waits to be done once the folder being deleted is (*SWEEPING-FOLDER*),
the latest first, each a function.")

(defun wait-for-sweep (folder function)
  "When FOLDER is within the folder package-remove-unused is deleting now
\(*SWEEPING-FOLDER*), have FUNCTION called once that is done, and return
true; else NIL, nothing kept."
  (when (and *sweeping-folder* folder (tuatara.io:path-within-p (tuatara.io:native folder) *sweeping-folder*))
    (push function *after-sweep*)
    t))

(defun still-unused-p (folder)
  "True when FOLDER, found unused by package-remove-unused, is so still, as
things are now on the editor's thread: no package or source named since
wants it (UNUSED-PACKAGE-FOLDERS), and nothing of it is being fetched,
checked, decided on or loaded from."
  (and (member folder (unused-package-folders) :key #'car :test #'string=)
       (notany (lambda (loaded) (tuatara.io:path-within-p (second loaded) folder)) *loaded-package-folders*)))

(defun still-to-delete-p (entry)
  "True when ENTRY, (FOLDER WORDS OCTETS NAME), is to be deleted still, as
things are now on the editor's thread: for a package NAME deleted, NAME is
removed still, not loaded, and nothing of it is being fetched or loaded
\(PACKAGE-BUSY-P); else FOLDER is unused still (STILL-UNUSED-P)."
  (destructuring-bind (folder words octets &optional name) entry
    (declare (ignore words octets))
    (if name
        (and (package-removed-p name) (not (package-loaded-p name)) (not (package-busy-p name)))
        (still-unused-p folder))))

(defun sweep-next (folders freed deleted &optional packages)
  "Delete the first of FOLDERS (each (FOLDER WORDS OCTETS NAME)) still to be
deleted (STILL-TO-DELETE-P), decided now on the editor's thread, in the
background, then the rest the same way; once none is left, say how many were
deleted and the octets FREED. DELETED counts those deleted so far; PACKAGES
names the packages deleted (\\[package-delete]), NIL for sources no longer
named, the preview beside the package manager then showing its entry anew."
  (let ((rest (member-if #'still-to-delete-p folders)))
    (if (null rest)
        (if packages
            (progn
              (entry-preview-again t)
              (message "~{~a~^, ~} deleted, ~a freed; removed, so not loaded at the next start (\\[package-load] loads ~:[it~;them~])"
                       packages (tuatara.io:octets-words freed) (rest packages)))
            (message "Package sources no longer named deleted: ~d, ~a freed" deleted (tuatara.io:octets-words freed)))
        (let ((job (make-package-sweep :delete rest packages)))
          (setf *sweeping-folder* (tuatara.io:native (first (first rest)))
                (package-sweep-freed job) freed
                (package-sweep-deleted job) deleted)
          (start-sweep-job job (lambda (job) (delete-unused-folder job (first (package-sweep-folders job)))))))))

(defun delete-unused-folder (job entry)
  "Delete ENTRY's folder, (FOLDER WORDS OCTETS NAME), on the thread of JOB, a
PACKAGE-SWEEP, with its files compiled when it is a folder of the packages
directory, what is remembered of the packages in it forgotten
\(FORGET-PACKAGES-WITHIN), its octets added to those freed; left as it is
when it is no longer a folder of its own whose real path is in the packages
directory or the folder packages are compiled into, as looked at again now."
  (destructuring-bind (folder words octets &optional name) entry
    (declare (ignore words name))
    (when (and (eq :directory (tuatara.io:path-kind (string-right-trim "/" folder) :follow nil))
               (or (tuatara.io:really-within-p folder (tuatara.io:native (packages-directory)))
                   (tuatara.io:really-within-p folder (packages-compiled-directory))))
      (let ((compiled (package-compiled-folder folder)))
        (when (and (tuatara.io:path-within-p folder (tuatara.io:native (packages-directory))) (probe-file compiled))
          (delete-package-folder compiled)))
      (delete-package-folder folder)
      (forget-packages-within folder)
      (incf (package-sweep-freed job) octets)
      (incf (package-sweep-deleted job)))))

(defmethod tuatara.runtime:job-take-in ((job package-sweep))
  "Ask about what was found, go on to the next folder, or say what went wrong."
  (let ((job job))
    (when (eq (package-sweep-phase job) :delete)
      (setf *sweeping-folder* nil)
      (let ((waiting (reverse *after-sweep*)))
        (setf *after-sweep* '())
        (dolist (function waiting) (defer-action function))))
    (defer-action
     (lambda ()
       (case (package-sweep-status job)
         (:done (if (eq (package-sweep-phase job) :delete)
                    (sweep-next (rest (package-sweep-folders job)) (package-sweep-freed job) (package-sweep-deleted job)
                                (package-sweep-packages job))
                    (if (package-sweep-packages job)
                        (packages-delete-ask (package-sweep-packages job) (package-sweep-folders job))
                        (packages-sweep-ask (package-sweep-folders job)))))
         (:failed (message "~a failed: ~a; ~:[nothing was deleted~;~:*~d deleted before it, ~a freed~]"
                           (cond ((package-sweep-packages job) "Deleting packages")
                                 ((eq (package-sweep-phase job) :look) "Looking for package sources no longer named")
                                 (t "Deleting a package source"))
                           (or (tuatara.runtime:job-error job) "it ended unexpectedly")
                           (and (eq (package-sweep-phase job) :delete) (package-sweep-deleted job))
                           (tuatara.io:octets-words (package-sweep-freed job)))))))))

(defun package-busy-p (name)
  "True while the package NAME is being fetched or checked, its version waits
to be decided on, or it is compiled and loaded in the background
\(*FETCHING-PACKAGES*, *PACKAGES-IN-HAND*)."
  (and (or (member name *fetching-packages* :test #'string=)
           (assoc name *packages-in-hand* :test #'string=))
       t))

(defun busy-source-folders ()
  "The folders of the packages directory of the sources of the packages busy
now (PACKAGE-BUSY-P)."
  (loop for (name source) in *declared-packages*
        when (and source (package-busy-p name)
                  (not (local-source-p (package-where source))))
          collect (format nil "~a~a/" (tuatara.io:native (packages-directory)) (tuatara.io:path-file-name (package-where source)))))

(defun packages-sweep-ask (folders)
  "List FOLDERS, each (FOLDER WORDS OCTETS), found by package-remove-unused,
in *Help*, and ask once whether to delete them, with their total; then
delete them one by one, each looked at again first (SWEEP-NEXT)."
  (if (null folders)
      (message "No package source kept is unused")
      (let ((total (reduce #'+ folders :key #'third)))
        (show-help (format nil "Package sources kept that nothing names now, in ~a:~%~%~:{  ~10@a  ~a~%~}~%  ~10@a  in all~%"
                           (abbreviated-path (tuatara.io:native (packages-directory)))
                           (mapcar (lambda (f) (list (tuatara.io:octets-words (third f)) (second f))) folders)
                           (tuatara.io:octets-words total)))
        (if (yes-or-no-p* (format nil "Delete ~[no package source~;1 package source~:;~:*~d package sources~] no longer named, ~a?"
                                  (length folders) (tuatara.io:octets-words total)))
            (sweep-next folders 0 0)
            (message "Nothing deleted")))))
