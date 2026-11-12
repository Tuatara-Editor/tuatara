;;;; test/dead-code.lisp --- Functions and macros of the core that nothing calls.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A function or macro is in use when its name is written, outside its own
;;;; definition, in the code of the core, of the tests, of the tools, of the
;;;; init file or of a package repository: as the head of a call, as #'name or
;;;; 'name, or as a name among the arguments. The names a form binds itself
;;;; (let, flet, labels, lambda lists, loop and dolist variables, the name a
;;;; definition gives) are not uses, so a variable or local function of the same
;;;; name does not hide dead code. A package prefix is ignored: a name defined
;;;; in two packages is in use when either is. The package and API lists of
;;;; exported names do not count, and the names TUATARA-USER exports are the
;;;; init file's. What nothing calls and stays is in *DEAD-CODE-ALLOWED*.

(in-package #:tuatara.tests)

(defparameter *dead-code-allowed*
  '(("compiler-core-toplevel" "named by a string when the compiling core is saved"))
  "The definitions nothing calls that are kept, each (NAME REASON): the name as it is written, a few words on why.")

(defun listing-form-p (text start)
  "True when the form at START in TEXT only lists names (a package definition, the API's lists), not uses them."
  (flet ((at (prefix)
           (let ((end (+ start (length prefix))))
             (and (<= end (length text)) (string= prefix text :start2 start :end2 end)))))
    (or (at "(defpackage") (at "(uiop:define-package") (at "(define-package")
        (at "(defparameter *api*") (at "(defparameter *api-from-libraries*"))))

(defun usage-source-files ()
  "Every Lisp file whose text can use a name of the core: the core, the tests, the tools, the starter init file, the
ASDF system and the package repositories."
  (let ((root (asdf:system-source-directory :tuatara)))
    (append (core-source-files)
            (directory (merge-pathnames "test/**/*.lisp" root))
            (directory (merge-pathnames "tools/*.lisp" root))
            (directory (merge-pathnames "data/*.lisp" root))
            (list (merge-pathnames "tuatara.asd" root))
            (package-source-files))))

(defun tree-atoms (tree)
  "Every symbol written in TREE (READ-TREE), at any depth."
  (if (consp tree) (loop for x in tree append (tree-atoms x)) (list tree)))

(defun form-uses (tree)
  "A list of the names the form TREE (READ-TREE) uses, as written without package prefix, one for each use. Names it
binds and the name a definition gives are not uses; a name inside a quoted list is."
  (let ((uses '()))
    (labels ((note (atom vars)
               (let ((name (token-name atom)))
                 (when (and name (not (member name vars :test #'string=))) (push name uses))))
             (items (tree) (and (listp tree) tree))
             (names (tree) (remove nil (mapcar #'token-name (tree-atoms tree))))
             (walk-defaults (lambda-list vars funs)
               (dolist (parameter (items lambda-list))
                 (when (consp parameter) (walk-all (rest parameter) vars funs))))
             (walk-all (forms vars funs) (dolist (form forms) (walk form vars funs)))
             (walk (form vars funs)
               (cond ((atom form) (note form vars))
                     ((null form))
                     ((consp (first form)) (walk-all form vars funs))
                     (t (walk-call (token-name (first form)) form vars funs))))
             (walk-call (head form vars funs)
               (let ((rest (rest form)))
                 (cond
                   ((null head) (walk-all rest vars funs))
                   ((string= head "quote") (dolist (atom (tree-atoms rest)) (note atom '())))
                   ((string= head "function")
                    (if (consp (first rest))
                        (walk (first rest) vars funs)
                        (unless (member (token-name (first rest)) funs :test #'equal) (note (first rest) '()))))
                   ((member head '("let" "let*") :test #'string=)
                    (let ((bound '()))
                      (dolist (binding (items (first rest)))
                        (if (consp binding)
                            (progn (walk-all (rest binding) (if (string= head "let*") (append bound vars) vars) funs)
                                   (push (token-name (first binding)) bound))
                            (push (token-name binding) bound)))
                      (walk-all (rest rest) (append bound vars) funs)))
                   ((member head '("flet" "labels") :test #'string=)
                    (let* ((defs (remove-if-not #'consp (items (first rest))))
                           (inner (append (mapcar (lambda (def) (token-name (first def))) defs) funs)))
                      (dolist (def defs)
                        (walk-defaults (second def) vars funs)
                        (walk-all (cddr def) (append (names (second def)) vars)
                                  (if (string= head "labels") inner funs)))
                      (walk-all (rest rest) vars inner)))
                   ((string= head "lambda")
                    (walk-defaults (first rest) vars funs)
                    (walk-all (rest rest) (append (names (first rest)) vars) funs))
                   ((member head '("dolist" "dotimes" "do-symbols" "do-external-symbols" "with-open-file"
                                   "with-input-from-string" "with-output-to-string")
                            :test #'string=)
                    (let ((var (token-name (first (items (first rest))))))
                      (walk-all (rest (items (first rest))) vars funs)
                      (walk-all (rest rest) (cons var vars) funs)))
                   ((member head '("multiple-value-bind" "destructuring-bind") :test #'string=)
                    (walk (second rest) vars funs)
                    (walk-all (cddr rest) (append (names (first rest)) vars) funs))
                   ((string= head "handler-case")
                    (walk (first rest) vars funs)
                    (dolist (clause (remove-if-not #'consp (rest rest)))
                      (walk-all (cddr clause) (append (names (second clause)) vars) funs)))
                   ((string= head "loop")
                    (let ((bound '()))
                      (loop for (word next) on rest
                            when (and (stringp word) (member word '("for" "as" "with" "into") :test #'string=))
                              do (setf bound (append (names next) bound)))
                      (walk-all (remove-if (lambda (x) (and (stringp x) (member (token-name x) bound :test #'equal)))
                                           rest)
                                (append bound vars) funs)))
                   ((member head '("defun" "defmacro" "defmethod" "defgeneric" "define-command"
                                   "define-compiler-macro")
                            :test #'string=)
                    (let* ((after-name (rest rest))
                           (lambda-list (find-if #'listp after-name)))
                      (note (first form) '())
                      (walk-defaults lambda-list vars funs)
                      (walk-all (rest (member lambda-list after-name)) (append (names lambda-list) vars) funs)))
                   (t (unless (member head funs :test #'string=) (note (first form) '()))
                      (walk-all (if (and (or (uiop:string-prefix-p "def" head) (uiop:string-prefix-p "define-" head))
                                         (stringp (first rest)))
                                    (rest rest)
                                    rest)
                                vars funs))))))
      (walk tree '() '()))
    uses))

(defun defined-function-name (text start)
  "The name the DEFUN or DEFMACRO form at START in TEXT defines, without a package prefix, or NIL for any other form
and for a SETF function (its calls name the accessor, not the function)."
  (multiple-value-bind (operator name) (form-head text start)
    (and (member operator '("defun" "defmacro") :test #'equal) (char/= (char name 0) #\()
         (token-name name))))

(defun dead-code-candidates ()
  "A list of (NAME FILE LINE) for each function and macro of the core that the code uses nowhere but in its own
definition."
  (let ((uses (make-hash-table :test 'equal)) (defined '()) (core (mapcar #'namestring (core-source-files))))
    (dolist (file (usage-source-files))
      (let ((text (coerce (uiop:read-file-string file :external-format :utf-8) 'simple-string)))
        (loop for (start end) in (top-level-forms text)
              unless (listing-form-p text start)
                do (let ((name (and (member (namestring file) core :test #'string=)
                                    (defined-function-name text start)))
                         (own 0))
                     (dolist (use (loop for tree in (read-tree text start end) append (form-uses tree)))
                       (incf (gethash use uses 0))
                       (when (and name (string= use name)) (incf own)))
                     (when name
                       (push (list name (tree-relative-name file) (file-line-number text start) own) defined))))))
    (loop for (name file line own) in (nreverse defined)
          when (<= (gethash name uses 0) own)
            collect (list name file line))))

(defun init-file-name-p (name)
  "True when NAME is exported by TUATARA-USER: the names an init file is written with, which the code does not call."
  (let ((symbol (find-symbol (string-upcase name) :tuatara-user)))
    (and symbol (eq (nth-value 1 (find-symbol (symbol-name symbol) :tuatara-user)) :external))))

(defun dead-code ()
  "A list of (NAME FILE LINE) for each function and macro of the core that nothing uses and that
*DEAD-CODE-ALLOWED* does not keep."
  (remove-if (lambda (found) (or (assoc (first found) *dead-code-allowed* :test #'string=)
                                 (init-file-name-p (first found))))
             (dead-code-candidates)))

(defun dead-code-checkable-p ()
  "True when the package repositories are found, so that a name only they use can be told from a dead one."
  (and (sibling-packages-directory) t))

(define-test (definitions dead-code-skipped-without-packages)
  :description "Without the package repositories the dead-code test is skipped, not failed on names only they use."
  (let ((*packages-directory-candidates* '("/nonexistent/tuatara-packages/")))
    (false (dead-code-checkable-p))
    (is equal '() (package-repositories))
    (is equal '() (package-source-files))))

(define-test (definitions no-dead-code)
  :description "No function or macro of the core is unused by the core, the tests and the package repositories."
  (if (not (dead-code-checkable-p))
      (skip "the packages' sources are not beside the editor's" (true nil))
      (let ((dead (dead-code)))
        (false dead "~d functions or macros of the core nothing uses (name file line): ~{~%  ~{~a  ~a:~a~}~}"
               (length dead) dead))))

(defun dead-code-report (&optional (stream *standard-output*))
  "Print to STREAM the functions and macros of the core that nothing uses, then those kept in *DEAD-CODE-ALLOWED*,
each with its file and line, after a line that counts them."
  (let ((dead (dead-code)))
    (format stream "~d functions or macros of the core that nothing uses~%" (length dead))
    (unless (dead-code-checkable-p)
      (format stream "  the package repositories were not found: a name only they use is listed too~%"))
    (loop for (name file line) in dead do (format stream "  ~a  ~a:~a~%" name file line))
    (format stream "~d more are kept on purpose (*DEAD-CODE-ALLOWED*)~%" (length *dead-code-allowed*))
    (loop for (name reason) in *dead-code-allowed* do (format stream "  ~a  ~a~%" name reason))))
