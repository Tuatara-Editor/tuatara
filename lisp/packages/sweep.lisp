;;;; lisp/packages/sweep.lisp --- Package sources no longer named: their directories found and deleted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The directories of the packages directory that no package or source named wants,
;;;; found and deleted in the background after asking.

(in-package #:tuatara.packages)

(defun unused-package-directories ()
  "The directories of the packages directory (PACKAGES-DIRECTORY) that no
package or source named this session wants, each (DIR . WORDS), DIR a
native name ending in /, WORDS what it holds: a source's whole directory, when
no package-source names it and no load-package (left out ones too) fetches
from it; else the directory of a package of a source that is named, when no
load-package names that package. A source a fetch or check runs for now is
left alone, as are directories whose name begins with a full stop (a fetch's
scratch) and anything that is not a directory of its own (a symbolic link)."
  (let* ((top (tuatara.io:native (packages-directory)))
         (named (append (loop for (name source) in *declared-packages* when source collect (list name source))
                        *left-out-packages*))
         (whole (loop for (nil . where) in *package-sources*
                      unless (local-source-p where) collect (tuatara.io:path-file-name where)))
         (pairs (loop for (name source) in named
                      for where = (package-where source)
                      unless (local-source-p where)
                        collect (cons (tuatara.io:path-file-name where) name)))
         (busy (busy-source-directories))
         (found '()))
    (flet ((own-directories (dir)
             ;; the directories directly in DIR, links never followed, nor names beginning with .
             (loop for entry in (ignoring-errors-reported ("reading ~a" dir)
                                  (directory (merge-pathnames "*/" dir) :resolve-symlinks nil))
                   for name = (car (last (pathname-directory entry)))
                   for native = (tuatara.io:native entry)
                   when (and (stringp name) (plusp (length name)) (char/= (char name 0) #\.)
                             (eq :directory (tuatara.io:path-kind (string-right-trim "/" native) :follow nil))
                             (tuatara.io:really-within-p native top))
                     collect (cons name native))))
      (dolist (source (own-directories top))
        (destructuring-bind (source-name . source-directory) source
          (cond ((or (member source-name whole :test #'string=) (member (string-right-trim "/" source-directory) busy :key (lambda (b) (string-right-trim "/" b)) :test #'string=)))
                ((find source-name pairs :key #'car :test #'string=)
                 (dolist (package (own-directories source-directory))
                   (unless (find-if (lambda (pair)
                                      (and (string= (car pair) source-name) (string= (cdr pair) (car package)))) pairs)
                     (push (cons (cdr package)
                                 (format nil "~a from ~a" (car package) (tuatara.io:percent-decoded source-name)))
                           found))))
                (t (push (cons source-directory (format nil "~a, all of it" (tuatara.io:percent-decoded source-name))) found))))))
    (nreverse found)))

(defun forget-packages-within (directory)
  "Forget the versions *FETCHED-PACKAGES* remembers of the packages kept
within DIRECTORY, deleted. Who signed their source is not forgotten."
  (state-list-drop *fetched-packages*
                   (loop for (key) in (state-list-entries *fetched-packages*)
                         for words = (uiop:split-string key :separator " ")
                         when (and (>= (length words) 3)
                                   (tuatara.io:path-within-p
                                    (tuatara.io:native (package-kept-directory (format nil "~{~a~^ ~}" (butlast (rest words))) (first words)))
                                    directory))
                           collect key)))

(defun package-compiled-directory (directory)
  "The directory the files of what is kept in DIRECTORY, of the packages directory, are compiled into."
  (directory-namestring (package-fasl (concatenate 'string (tuatara.io:native directory) "x"))))

(defstruct (package-sweep (:include tuatara.runtime:job)
                          (:constructor make-package-sweep (phase directories &optional packages)) (:copier nil))
  "Package sources no longer named, or the directories of PACKAGES, the names of
packages deleted (\\[package-delete]), being looked for (PHASE :look), or one of
them being deleted (:delete). DIRECTORIES holds entries, each (DIRECTORY WORDS OCTETS
NAME), NAME the package's for PACKAGES, what was found, or the one being deleted
followed by those still to go. FREED is the octets freed so far, and DELETED is
how many directories."
  phase directories packages (freed 0) (deleted 0))

(setf (documentation 'package-sweep-p 'function) "True when OBJECT is a PACKAGE-SWEEP.")

(tuatara.runtime:define-job-words package-sweep
  :doing (if (package-sweep-packages job) "Deleting packages" "Removing package sources no longer named")
  :verb (if (package-sweep-packages job) "delete packages" "remove package sources no longer named")
  :description (cond ((package-sweep-packages job)
                      (format nil
                              "~:[Deleting~;Measuring~] the package~p ~{~a~^, ~}"
                              (eq (package-sweep-phase job) :look)
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
    (tuatara.runtime:start-job-thread job
                                      (lambda (job) (call-with-sweep-settings settings (lambda () (funcall work job))))
                                      :name "tuatara package sweep")))

(defun sweep-entry-octets (directory)
  "The octets DIRECTORY, of the packages directory or the directory packages are
compiled into, takes with the files of it compiled (DIRECTORY-OCTETS)."
  (+ (directory-octets directory)
     (if (tuatara.io:path-within-p directory (tuatara.io:native (packages-directory)))
         (directory-octets (package-compiled-directory directory))
         0)))

(defun directory-octets (directory)
  "The octets DIRECTORY and what it holds take, links not followed; 0 when it is not there."
  (if (eq :directory (tuatara.io:path-kind (string-right-trim "/" directory) :follow nil))
      (or (rimu-tree-size directory) 0)
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
touched, never a directory of yours, never through a link. When the init file
was not read to its end, every package named after that point would look
unused, so nothing is done until it is. Who signed a source's versions is
still remembered: a package fetched from it again is checked against that
signer."
  (if *init-file-stop*
      (message "The init file was not read to its end (~d: ~a); nothing is deleted until it is"
               (car *init-file-stop*) (cdr *init-file-stop*))
      (start-sweep-job (make-package-sweep :look nil)
                       (lambda (job)
                         (setf (package-sweep-directories job)
                               (loop for (directory . words) in (unused-package-directories)
                                     collect (list directory words (sweep-entry-octets directory))))))))

(defvar *sweeping-directory* nil
  "The directory of the packages directory package-remove-unused is deleting
now, or NIL: a package kept in it is loaded once it is done (*AFTER-SWEEP*).")

(defvar *after-sweep* '()
  "What waits to be done once the directory being deleted is (*SWEEPING-DIRECTORY*),
the latest first, each a function.")

(defun wait-for-sweep (directory function)
  "When DIRECTORY is within the directory package-remove-unused is deleting now
\(*SWEEPING-DIRECTORY*), have FUNCTION called once that is done, and return
true; else NIL, nothing kept."
  (when (and *sweeping-directory* directory (tuatara.io:path-within-p (tuatara.io:native directory)
                                                                      *sweeping-directory*))
    (push function *after-sweep*)
    t))

(defun still-unused-p (directory)
  "True when DIRECTORY, found unused by package-remove-unused, is so still, as
things are now on the editor's thread: no package or source named since
wants it (UNUSED-PACKAGE-DIRECTORIES), and nothing of it is being fetched,
checked, decided on or loaded from."
  (and (member directory (unused-package-directories) :key #'car :test #'string=)
       (notany (lambda (loaded) (tuatara.io:path-within-p (second loaded) directory)) *loaded-package-directories*)))

(defun still-to-delete-p (entry)
  "True when ENTRY, (DIRECTORY WORDS OCTETS NAME), is to be deleted still, as
things are now on the editor's thread: for a package NAME deleted, NAME is
removed still, not loaded, and nothing of it is being fetched or loaded
\(PACKAGE-BUSY-P); else DIRECTORY is unused still (STILL-UNUSED-P)."
  (destructuring-bind (directory words octets &optional name) entry
    (declare (ignore words octets))
    (if name
        (and (package-removed-p name) (not (package-loaded-p name)) (not (package-busy-p name)))
        (still-unused-p directory))))

(defun sweep-next (directories freed deleted &optional packages)
  "Delete the first of DIRECTORIES (each (DIRECTORY WORDS OCTETS NAME)) still to be
deleted (STILL-TO-DELETE-P), decided now on the editor's thread, in the
background, then the rest the same way; once none is left, say how many were
deleted and the octets FREED. DELETED counts those deleted so far; PACKAGES
names the packages deleted (\\[package-delete]), NIL for sources no longer
named, the preview beside the package manager then showing its entry anew."
  (let ((rest (member-if #'still-to-delete-p directories)))
    (if (null rest)
        (if packages
            (progn
              (entry-preview-again t)
              (message "~{~a~^, ~} deleted, ~a freed; removed, so not loaded at the next start (\\[package-load] loads ~:[it~;them~])"
                       packages (tuatara.io:octets-words freed) (rest packages)))
            (message "Package sources no longer named deleted: ~d, ~a freed" deleted (tuatara.io:octets-words freed)))
        (let ((job (make-package-sweep :delete rest packages)))
          (setf *sweeping-directory* (tuatara.io:native (first (first rest)))
                (package-sweep-freed job) freed
                (package-sweep-deleted job) deleted)
          (start-sweep-job job (lambda (job) (delete-unused-directory job (first (package-sweep-directories job)))))))))

(defun delete-unused-directory (job entry)
  "Delete ENTRY's directory, (DIRECTORY WORDS OCTETS NAME), on the thread of JOB, a
PACKAGE-SWEEP, with its files compiled when it is a directory of the packages
directory, what is remembered of the packages in it forgotten
\(FORGET-PACKAGES-WITHIN), its octets added to those freed. It is left as it is
when it is no longer a directory of its own whose real path is in the packages
directory or the directory packages are compiled into, as looked at again now."
  (destructuring-bind (directory words octets &optional name) entry
    (declare (ignore words name))
    (when (and (eq :directory (tuatara.io:path-kind (string-right-trim "/" directory) :follow nil))
               (or (tuatara.io:really-within-p directory (tuatara.io:native (packages-directory)))
                   (tuatara.io:really-within-p directory (packages-compiled-directory))))
      (let ((compiled (package-compiled-directory directory)))
        (when (and (tuatara.io:path-within-p directory (tuatara.io:native (packages-directory))) (probe-file compiled))
          (delete-package-directory compiled)))
      (delete-package-directory directory)
      (forget-packages-within directory)
      (incf (package-sweep-freed job) octets)
      (incf (package-sweep-deleted job)))))

(defmethod tuatara.runtime:job-take-in ((job package-sweep))
  "Ask about what was found, go on to the next directory, or say what went wrong."
  (let ((job job))
    (when (eq (package-sweep-phase job) :delete)
      (setf *sweeping-directory* nil)
      (let ((waiting (reverse *after-sweep*)))
        (setf *after-sweep* '())
        (dolist (function waiting) (defer-action function))))
    (defer-action
     (lambda ()
       (case (package-sweep-status job)
         (:done (if (eq (package-sweep-phase job) :delete)
                    (sweep-next (rest (package-sweep-directories job)) (package-sweep-freed job)
                                (package-sweep-deleted job)
                                (package-sweep-packages job))
                    (if (package-sweep-packages job)
                        (packages-delete-ask (package-sweep-packages job) (package-sweep-directories job))
                        (packages-sweep-ask (package-sweep-directories job)))))
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

(defun busy-source-directories ()
  "The directories of the packages directory of the sources of the packages busy
now (PACKAGE-BUSY-P)."
  (loop for (name source) in *declared-packages*
        when (and source (package-busy-p name)
                  (not (local-source-p (package-where source))))
          collect (format nil "~a~a/" (tuatara.io:native (packages-directory)) (tuatara.io:path-file-name (package-where source)))))

(defun packages-sweep-ask (directories)
  "List DIRECTORIES, each (DIRECTORY WORDS OCTETS), found by package-remove-unused,
in *Help*, and ask once whether to delete them, with their total; then
delete them one by one, each looked at again first (SWEEP-NEXT)."
  (if (null directories)
      (message "No package source kept is unused")
      (let ((total (reduce #'+ directories :key #'third)))
        (show-help (format nil "Package sources kept that nothing names now, in ~a:~%~%~:{  ~10@a  ~a~%~}~%  ~10@a  in all~%"
                           (abbreviated-path (tuatara.io:native (packages-directory)))
                           (mapcar (lambda (f) (list (tuatara.io:octets-words (third f)) (second f))) directories)
                           (tuatara.io:octets-words total)))
        (if (yes-or-no-p* (format nil "Delete ~[no package source~;1 package source~:;~:*~d package sources~] no longer named, ~a?"
                                  (length directories) (tuatara.io:octets-words total)))
            (sweep-next directories 0 0)
            (message "Nothing deleted")))))
