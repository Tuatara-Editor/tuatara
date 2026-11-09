;;;; test/reuse-index.lisp --- The list of what the code defines, kept in step with the code.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; test/api/reuse-index.txt has one line for each exported function, macro,
;;;; generic function and variable of the Tuatara packages, and for each
;;;; documented internal function of TUATARA.DEFAULTS: the name, the kind, the
;;;; first sentence of the docstring and the file. Look there before writing a
;;;; function, to reuse one that exists. tools/reuse-index.sh writes it again;
;;;; a commit that stages Lisp files runs it (tools/git-hooks/pre-commit).

(in-package #:tuatara.tests)

(defparameter *reuse-index-file* (asdf:system-relative-pathname :tuatara "test/api/reuse-index.txt")
  "The file the reuse index is kept in.")

(defun index-sentence (doc)
  "The first sentence of the docstring DOC on one line, or a note that there is none."
  (let ((sentence (tuatara.command:first-sentence doc)))
    (if (string= sentence "") "(no docstring)" sentence)))

(defun definition-file (symbol type)
  "The file under the source tree that defines SYMBOL as TYPE, or \"?\"."
  (let* ((source (first (ignore-errors (sb-introspect:find-definition-sources-by-name symbol type))))
         (path (and source (sb-introspect:definition-source-pathname source)))
         (tree (asdf:system-source-directory :tuatara)))
    (cond ((null path) "?")
          ((uiop:subpathp path tree) (namestring (uiop:enough-pathname path tree)))
          (t (file-namestring path)))))

(defun symbol-roles (symbol)
  "A list of (KIND DEFINITION-TYPE DOCSTRING) for each role SYMBOL fills."
  (let ((roles '()))
    (cond ((macro-function symbol)
           (push (list "macro" :macro (documentation symbol 'function)) roles))
          ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function))
           (push (list "generic" :generic-function (documentation symbol 'function)) roles))
          ((and (fboundp symbol) (not (special-operator-p symbol)))
           (push (list "function" :function (documentation symbol 'function)) roles)))
    (when (member (sb-int:info :variable :kind symbol) '(:special :constant))
      (push (list "variable" :variable (documentation symbol 'variable)) roles))
    roles))

(defun reuse-index-packages ()
  "The Tuatara packages the index covers, in name order: every one but the tests', their compile gate's and the benchmarks'."
  (sort (remove-if-not (lambda (package)
                         (let ((name (package-name package)))
                           (and (>= (length name) 7) (string-equal "TUATARA" name :end2 7)
                                (not (member name '("TUATARA.TESTS" "TUATARA.BENCH" "TUATARA-COMPILE-GATE") :test #'string=)))))
                       (list-all-packages))
        #'string< :key #'package-name))

(defun reuse-index-rows ()
  "The rows (PACKAGE NAME KIND SENTENCE FILE) of the reuse index, in order, leaving out what test files define."
  (let ((rows '()))
    (flet ((add (package symbol suffix)
             (dolist (role (symbol-roles symbol))
               (destructuring-bind (kind type doc) role
                 (let ((file (definition-file symbol type)))
                   (when (and (or (string= suffix "") doc) (not (uiop:string-prefix-p "test/" file)))
                     (push (list (string-downcase (package-name package))
                                 (format nil "~a~a" (string-downcase (symbol-name symbol)) suffix)
                                 kind (index-sentence doc) file)
                           rows)))))))
      (dolist (package (reuse-index-packages))
        (do-external-symbols (symbol package) (add package symbol ""))
        (when (string= (package-name package) "TUATARA.DEFAULTS")
          ;; undocumented internals are structure accessors, found by the structure's name
          (do-symbols (symbol package)
            (when (and (eq (symbol-package symbol) package) (eq (nth-value 1 (find-symbol (symbol-name symbol) package)) :internal)
                       (fboundp symbol) (not (special-operator-p symbol)) (not (macro-function symbol)))
              (add package symbol " (internal)"))))))
    (stable-sort rows (lambda (a b)
                        (loop for x in a for y in b
                              unless (string= x y) return (string< x y))))))

(defun reuse-index-lines ()
  "The lines of the reuse index as the code loaded now has it, the header first."
  (cons ";; The reuse index, made by tools/reuse-index.sh: package:name  kind  first sentence of the docstring  file"
        (loop for (package name kind sentence file) in (reuse-index-rows)
              collect (format nil "~a:~a  ~a  ~a  ~a" package name kind sentence file))))

(defun write-reuse-index (&optional (file *reuse-index-file*))
  "Write the reuse index of the loaded code to FILE."
  (ensure-directories-exist file)
  (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
    (dolist (line (reuse-index-lines)) (write-line line out))))

(defun read-reuse-index (&optional (file *reuse-index-file*))
  "The lines of the reuse index in FILE, or none when it is not there."
  (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
    (and in (loop for line = (read-line in nil) while line collect line))))

(define-test (definitions reuse-index-current)
  (let* ((now (reuse-index-lines)) (pinned (read-reuse-index))
         (added (set-difference now pinned :test #'string=))
         (removed (set-difference pinned now :test #'string=)))
    (flet ((shown (lines) (format nil "~{~%  ~a~}" (subseq lines 0 (min 20 (length lines))))))
      (true (> (length now) 1000))
      (false (or added removed)
             "test/api/reuse-index.txt is not what the code defines: ~d entries added, ~d removed; run tools/reuse-index.sh.~%Added (up to 20):~a~%Removed (up to 20):~a"
             (length added) (length removed) (shown (sort added #'string<)) (shown (sort removed #'string<))))))

(define-test (definitions reuse-index-leaves-out-tests)
  ;; the full run loads test/compile-gate.lisp, whose package's name starts with tuatara
  (load (asdf:system-relative-pathname :tuatara "test/compile-gate.lisp"))
  (false (find-if (lambda (line) (search "  test/" line)) (reuse-index-lines))
         "The reuse index lists a definition from a test file."))

(defun reuse-index-report (&optional (stream *standard-output*))
  "Print to STREAM whether test/api/reuse-index.txt is what the loaded code defines, and if not the lines added and
removed, after a line that counts them."
  (let* ((now (reuse-index-lines)) (pinned (read-reuse-index))
         (added (set-difference now pinned :test #'string=))
         (removed (set-difference pinned now :test #'string=)))
    (format stream "~d lines of the reuse index out of date (~d to add, ~d to remove; tools/reuse-index.sh writes it)~%"
            (+ (length added) (length removed)) (length added) (length removed))
    (dolist (line (sort added #'string<)) (format stream "  add     ~a~%" line))
    (dolist (line (sort removed #'string<)) (format stream "  remove  ~a~%" line))))
