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
                                    (merge-pathnames beside (uiop:pathname-directory-pathname sb-ext:*runtime-pathname*)))
                               (and *source-tree* (merge-pathnames source *source-tree*))))))
    (if existing (remove-if-not #'uiop:directory-exists-p all) all)))

;;; Memory

(defun heap-free ()
  "Octets of heap free now, garbage not yet collected counted as taken."
  (max 0 (- (sb-ext:dynamic-space-size) (sb-kernel:dynamic-usage))))

(defstruct (spares (:constructor make-spares (make &key (keep-p (constantly t)))) (:copier nil))
  "Things kept to be used again, buffers above all, so that work done piece
after piece on every core makes no new ones: MAKE, a function of none, makes
one when none is kept; KEEP-P, a function of one, says whether one given
back is worth keeping."
  (make nil :type function) (keep-p nil :type function) (items '()) (lock (sb-thread:make-mutex :name "tuatara spares")))

(setf (documentation 'make-spares 'function) "Return SPARES that makes each with MAKE and keeps those KEEP-P is true of."
      (documentation 'spares-p 'function) "True when OBJECT is a SPARES."
      (documentation 'spares-make 'function) "The function of none making one of SPARES when none is kept."
      (documentation 'spares-keep-p 'function) "The function saying whether one given back to SPARES is kept."
      (documentation 'spares-items 'function) "The ones SPARES keeps."
      (documentation 'spares-lock 'function) "The lock guarding SPARES's items.")

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

(defun heap-budget (setting)
  "The octets a piece of work may plan to use now: SETTING, a number of
octets, or when it is NIL a quarter of the heap free now (HEAP-FREE)."
  (or setting (floor (heap-free) 4)))

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
and a value."
  `(sb-alien:alien-funcall (sb-alien:extern-alien ,name (function ,result ,@(mapcar #'first arguments)))
                           ,@(mapcar #'second arguments)))

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
