;;;; test/definitions.lisp --- Every function is defined once in its package.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test definitions
  :description "No function, macro or command is defined twice in one package, where the later would quietly replace the earlier; no two tests have one name.")

(defun source-files (top)
  "Every Lisp source file under the folder TOP of the repository (\"lisp\", \"src\")."
  (directory (merge-pathnames (make-pathname :directory (list :relative top :wild-inferiors) :name :wild :type "lisp")
                              (asdf:system-source-directory "tuatara"))))

(defun source-definitions ()
  "Every (PACKAGE NAME FILE) the source files define with DEFUN, DEFMACRO,
DEFGENERIC or DEFINE-COMMAND, the package read from each file's IN-PACKAGE."
  (let ((found '()))
    (dolist (file (loop for top in '("src" "lisp" "frontends") append (source-files top)))
      (let ((text (uiop:read-file-string file)) (package nil))
        (cl-ppcre:do-register-groups (name) ("(?m)^\\(in-package #:([^)\\s]+)\\)" text)
          (setf package name))
        (cl-ppcre:do-register-groups (name) ("(?m)^\\((?:defun|defmacro|defgeneric|define-command) ([^\\s()]+)" text)
          (push (list package (string-downcase name) (file-namestring file)) found))))
    found))

(define-test (definitions defined-once)
  (let ((seen (make-hash-table :test 'equal)) (twice '()) (definitions (source-definitions)))
    ;; the sources were found and read
    (true (> (length definitions) 1000))
    (true (find '("tuatara.defaults" "split-selection") definitions :test #'equal
                :key (lambda (d) (list (string-downcase (first d)) (second d)))))
    (dolist (definition definitions)
      (destructuring-bind (package name file) definition
        ;; by the symbol's own package: a name another package exports, used here, is that package's
        (let* ((symbol (let ((p (find-package (string-upcase package)))) (and p (find-symbol (string-upcase name) p))))
               (package (if symbol (string-downcase (package-name (symbol-package symbol))) package))
               (key (list package name)))
          (when (and (gethash key seen) (string/= (gethash key seen) file))
            (push (format nil "~a::~a in ~a and ~a" package name (gethash key seen) file) twice))
          (setf (gethash key seen) file))))
    (true (null twice) "Defined twice: ~{~a~^; ~}" twice)))

(defun structure-accessors ()
  "Every slot reader of the structures Tuatara's packages define, as (PACKAGE NAME),
the package's name and the reader's name in lower case."
  (let ((found '()))
    (dolist (package (list-all-packages))
      (when (uiop:string-prefix-p "TUATARA" (package-name package))
        (do-symbols (symbol package)
          (when (eq (symbol-package symbol) package)
            (let ((description (sb-kernel:find-defstruct-description symbol nil)))
              (when description
                (dolist (slot (sb-kernel:dd-slots description))
                  (let ((accessor (sb-kernel:dsd-accessor-name slot)))
                    (when accessor
                      (pushnew (list (string-downcase (package-name (symbol-package accessor)))
                                     (string-downcase (symbol-name accessor)))
                               found :test #'equal))))))))))
    found))

(define-test (definitions accessors-kept)
  ;; a function defined with a structure's slot reader's name replaces the reader
  ;; for every caller, without a word
  (let ((accessors (structure-accessors))
        (clashes '()))
    (true (> (length accessors) 200))
    (true (find '("tuatara.defaults" "split-buffer") accessors :test #'equal))
    (dolist (definition (source-definitions))
      (destructuring-bind (package name file) definition
        (when (find (list (string-downcase package) name) accessors :test #'equal)
          (push (format nil "~a::~a in ~a" package name file) clashes))))
    (true (null clashes) "Defined over a structure's slot reader: ~{~a~^; ~}" clashes)))

(define-test (definitions tests-named-once)
  ;; a test's name is its own whatever its parent: one defined again replaces the
  ;; first, which then never runs
  (let ((seen (make-hash-table :test 'equal)) (twice '()) (count 0))
    (dolist (file (directory (merge-pathnames "test/*.lisp" (asdf:system-source-directory "tuatara"))))
      (cl-ppcre:do-register-groups (parent child top) ("(?m)^\\(define-test (?:\\(([^\\s()]+) ([^\\s()]+)\\)|([^\\s()]+))" (uiop:read-file-string file))
        (declare (ignore parent))
        (incf count)
        (let ((name (string-downcase (or child top))))
          (when (gethash name seen) (push (format nil "~a in ~a and ~a" name (gethash name seen) (file-namestring file)) twice))
          (setf (gethash name seen) (file-namestring file)))))
    (true (> count 300))
    (true (null twice) "Tests named twice: ~{~a~^; ~}" twice)))
