;;;; src/runtime/resources.lisp --- Where Tuatara's files are, the memory it has, and C libraries loaded when needed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.runtime)

;;; Where Tuatara's files are

(defparameter *source-tree*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-parent-directory-pathname
    (uiop:pathname-directory-pathname #.(or *compile-file-truename* *load-truename*))))
  "The source tree Tuatara runs from: where what it reads is found when it
does not stand beside the program. Running from its sources, the tree they
are in; the saved program, the tree whose build/ it stands in, else its own
directory (PROGRAM-SOURCE-TREE): a saved program never looks where it was
built, nor keeps that place.")

(defun program-source-tree (program)
  "The source tree the program at PROGRAM (a pathname) stands in: the
directory above PROGRAM's when that holds tuatara.asd, as build/tuatara in a
source tree does; else PROGRAM's own directory, as in a release."
  (let* ((own (uiop:pathname-directory-pathname program))
         (above (uiop:pathname-parent-directory-pathname own)))
    (if (probe-file (merge-pathnames "tuatara.asd" above)) above own)))

(defun forget-source-tree ()
  "Forget the source tree, so that a saved program does not keep the place it
was built in; it finds its own as it starts (FIND-SOURCE-TREE)."
  (setf *source-tree* nil))

(defun find-source-tree ()
  "Set *SOURCE-TREE* for the saved program as it starts, from where it stands
\(PROGRAM-SOURCE-TREE)."
  (setf *source-tree* (and sb-ext:*runtime-pathname* (program-source-tree (pathname sb-ext:*runtime-pathname*)))))

(pushnew 'forget-source-tree sb-ext:*save-hooks*)
(pushnew 'find-source-tree sb-ext:*init-hooks*)

(defun resource-directories (beside source &key existing)
  "The directories Tuatara looks in, in order, for what it reads besides its
program: BESIDE (a directory name ending in /) beside the program, as a
release lays it out, then SOURCE in the source tree (*SOURCE-TREE*); with
EXISTING, only those that exist."
  (let ((all (remove nil (list (and sb-ext:*runtime-pathname*
                                    (merge-pathnames beside
                                                     (uiop:pathname-directory-pathname sb-ext:*runtime-pathname*)))
                               (and *source-tree* (merge-pathnames source *source-tree*))))))
    (if existing (remove-if-not #'uiop:directory-exists-p all) all)))

(defun own-variable (name)
  "The value of the environment variable NAME as the editor itself has it,
or NIL when it has none: what the editor's own directories, time zone, terminal
and switches are read from. A program started for someone else is given their
variables where they came with a request (PROGRAM-VARIABLE); this one never
follows them."
  (sb-posix:getenv name))

;;; Memory

(define-record (spares (:constructor make-spares (make &key (keep-p (constantly t)))) (:copier nil))
  "Things kept to be used again, buffers above all, so that work done piece
after piece on several cores makes no new ones: MAKE, a function of none,
makes one when none is kept; KEEP-P, a function of one, says whether one given
back is worth keeping."
  (make nil :type function :documentation "The function of none making one of SPARES when none is kept.")
  (keep-p nil :type function :documentation "The function saying whether one given back to SPARES is kept.")
  (items '() :documentation "The ones SPARES keeps.")
  (lock (sb-thread:make-mutex :name
                              "tuatara spares") :documentation "The lock guarding SPARES's items."))

(setf (documentation 'make-spares 'function) "Return SPARES that makes each with MAKE and keeps
those KEEP-P is true of.")

(defun take-spare (spares)
  "One of SPARES, kept or newly made."
  (or (sb-thread:with-mutex ((spares-lock spares)) (pop (spares-items spares)))
      (funcall (spares-make spares))))

(defun give-back-spare (spares item)
  "Keep ITEM in SPARES to be used again, when it is worth keeping."
  (when (funcall (spares-keep-p spares) item)
    (sb-thread:with-mutex ((spares-lock spares)) (push item (spares-items spares))))
  nil)

(defmacro with-spare ((var spares) &body body)
  "Run BODY with VAR bound to one of SPARES (TAKE-SPARE), given back after
however BODY ends; BODY must not keep it."
  (let ((s (gensym "SPARES")))
    `(let* ((,s ,spares) (,var (take-spare ,s)))
       (unwind-protect (progn ,@body) (give-back-spare ,s ,var)))))

;;; C libraries loaded when first needed

(defvar *foreign-libraries* (make-hash-table :test 'equal :synchronized t)
  "Each C library asked for by name: :LOADED, or :MISSING when it could not be loaded.")

(defun foreign-library-p (name)
  "True when the C library NAME (a file name the system's loader finds, such
as \"libenchant-2.so.2\", or a full path) is loaded, loading it the first time
it is asked for. It is not kept in a saved image; each run loads it again when
first needed."
  (eq :loaded (or (gethash name *foreign-libraries*)
                  (setf (gethash name *foreign-libraries*)
                        (handler-case (progn (sb-alien:load-shared-object name :dont-save t) :loaded)
                          (error () :missing))))))

(defun forget-foreign-libraries ()
  "Forget the C libraries loaded, which a saved image does not keep, so that
the program loads them again when it next needs them."
  (clrhash *foreign-libraries*))

(pushnew 'forget-foreign-libraries sb-ext:*save-hooks*)

(defmacro foreign-call (name result &rest arguments)
  "Call the C function NAME of a library FOREIGN-LIBRARY-P has loaded, which
returns RESULT (an alien type), with ARGUMENTS, each a list of an alien type
and a value. Kept for the calls written with it: a new call to C is a function
of its own, made with TUATARA.IO:DEFINE-C-FUNCTION."
  `(sb-alien:alien-funcall (sb-alien:extern-alien ,name (function ,result ,@(mapcar #'first arguments)))
                           ,@(mapcar #'second arguments)))

;;; Reading nested data from a file

(defvar *reading-depth* 0
  "How many levels deep the readers of nested data from a file are now, on
this thread (WITH-DEPTH-LIMIT).")

(defmacro with-depth-limit ((limit &optional (what "The data")) &body body)
  "Run BODY one level of nesting deeper, as a reader of nested data from a
file does on going into a list, an element or an object, and return what it
returns. When that makes more than LIMIT levels, BODY is not run: it is
refused (REFUSE), WHAT, a phrase, saying what is nested too deep. Levels are
counted on each thread, by every reader under way on it."
  `(let ((*reading-depth* (1+ *reading-depth*)))
     (when (> *reading-depth* ,limit)
       (refuse "~a is nested more than ~d deep" ,what ,limit))
     ,@body))

;;; Searching a sorted run

(defmacro bisect ((var low high) before)
  "The first index from LOW below HIGH for which BEFORE, a form evaluated with
VAR bound to an index, is false, BEFORE being true for every index before
that one and false for every one after (a sorted run searched by halves);
HIGH when it is true for all. LOW and HIGH are evaluated once; no function is
called for each step."
  (let ((lo (gensym "LO")) (hi (gensym "HI")))
    `(let ((,lo ,low) (,hi ,high))
       (declare (type fixnum ,lo ,hi))
       (loop while (< ,lo ,hi)
             do (let ((,var (ash (+ ,lo ,hi) -1)))
                  (declare (type fixnum ,var))
                  (if ,before (setf ,lo (1+ ,var)) (setf ,hi ,var))))
       ,lo)))

(defun first-index-at-or-after (vector value)
  "The first index of VECTOR, integers never decreasing, whose element is at
least VALUE; its length when none."
  (bisect (mid 0 (length vector)) (< (aref vector mid) value)))
