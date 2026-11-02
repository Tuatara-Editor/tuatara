;;;; test/package-runner.lisp --- A package repository's tests, run against this source tree.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each package repository's test/run.lisp loads Tuatara's tests and calls
;;;; TEST-PACKAGE: its package is loaded as load-package loads one fetched
;;;; anew (TUATARA.PACKAGES:LOAD-PACKAGE-FOR-TEST), checked to use only
;;;; Tuatara's API and the library names it allows (OUTSIDE-API), and its
;;;; tests run.

(in-package #:tuatara.tests)

(define-test package-runner
  :description "A package repository's tests: its package loaded as load-package loads one fetched anew, every name it uses outside Tuatara's API said, its tests run.")

(defun tuatara-name-p (symbol)
  "True when SYMBOL is one of Tuatara's own, of a package named TUATARA...."
  (let ((home (symbol-package symbol)))
    (and home (uiop:string-prefix-p "TUATARA" (package-name home)))))

(defun outside-library-p (symbol own)
  "True when SYMBOL is of a library: a package that is not OWN, the
package's own, nor Common Lisp's, the keywords' or Tuatara's, and SYMBOL is
not one of the library names Tuatara lets packages use (*API-FROM-LIBRARIES*)."
  (let ((home (symbol-package symbol)))
    (and home
         (not (member home (list own (find-package :cl) (find-package :keyword))))
         (not (tuatara-name-p symbol))
         (not (eq symbol 'sb-int:quasiquote))
         (not (member symbol tuatara.defaults::*api-from-libraries*)))))

(defun public-p (symbol)
  "True when a package may use SYMBOL: exported from TUATARA, or a setting
TUATARA-USER exports."
  (some (lambda (package) (eq (nth-value 1 (find-symbol (symbol-name symbol) package)) :external))
        '(:tuatara :tuatara-user)))

(defun defined-p (symbol)
  "True when SYMBOL names a function, a variable or a class."
  (or (fboundp symbol) (boundp symbol) (find-class symbol nil)))

(defun tuatara-defines-p (symbol)
  "True when one of Tuatara's packages defines a function, variable or class
of SYMBOL's name."
  (some (lambda (package)
          (let ((tuatara (find-symbol (symbol-name symbol) package)))
            (and tuatara (eq (symbol-package tuatara) package) (defined-p tuatara))))
        (tuatara-packages)))

(defun source-buffer-keys (pattern)
  "The names Tuatara keeps buffers' values under, as its source writes them,
matched by PATTERN, a regular expression whose register is the name: a
table of them, upper case."
  (let ((keys (make-hash-table :test 'equal)))
    (dolist (folder '("src/" "lisp/") keys)
      (dolist (file (directory (merge-pathnames (concatenate 'string folder "**/*.lisp")
                                                (asdf:system-source-directory :tuatara))))
        (cl-ppcre:do-register-groups (name) (pattern (uiop:read-file-string file))
          (setf (gethash (string-upcase name) keys) t))))))

(defun shared-key-p (symbol keys keywords)
  "True when Tuatara keeps a buffer's value under SYMBOL's name: a keyword
of KEYWORDS, another symbol of KEYS (SOURCE-BUFFER-KEYS)."
  (gethash (symbol-name symbol) (if (keywordp symbol) keywords keys)))

(defun buffer-keyword (form)
  "The keyword FORM keeps a buffer's value under, (buffer-local BUFFER :NAME); else NIL."
  (and (eq (car form) 'tuatara.buffer:buffer-local) (consp (cdr form)) (consp (cddr form))
       (keywordp (third form)) (third form)))

(defun named-symbol (form)
  "The symbol FORM names as data, 'NAME, or as the key of (buffer-local
BUFFER 'NAME); else NIL."
  (cond ((and (eq (car form) 'quote) (consp (cdr form)) (symbolp (second form))) (second form))
        ((and (eq (car form) 'tuatara.buffer:buffer-local) (consp (cdr form)) (consp (cddr form))
              (consp (third form)) (eq (car (third form)) 'quote) (symbolp (second (third form))))
         (second (third form)))))

(defun outside-api (name folder)
  "The names the package NAME, in FOLDER, uses that are not Tuatara's API,
written with Tuatara's package in front (read from its files but test.lisp),
each with how."
  (let ((own (find-package (string-upcase name)))
        (keys (source-buffer-keys "buffer-local [^()']*'([a-z0-9*+%-]+)"))
        (keywords (source-buffer-keys "buffer-local\\s+(?:[^\\s()']+|\\([^()]*\\))\\s+:([a-z0-9*+%-]+)"))
        (found '()))
    (dolist (file (uiop:directory-files folder "*.lisp"))
      (unless (string= (pathname-name file) "test")
        (with-open-file (in file)
          (let ((*package* own) (*read-eval* t))
            (loop for form = (read in nil in) until (eq form in)
                  do (when (and (consp form) (eq (car form) 'in-package)) (setf *package* (find-package (second form))))
                     (labels ((note (format symbol)
                                (pushnew (format nil format symbol) found :test #'string=))
                              (walk (x)
                                (cond ((and (symbolp x) (tuatara-name-p x) (not (public-p x)))
                                       (note "~(~s~), written with its package" x))
                                      ((and (symbolp x) (outside-library-p x own))
                                       (note "~(~s~), a library's name Tuatara does not let packages use" x))
                                      ((consp x)
                                       (let ((key (buffer-keyword x)))
                                         (when (and key (shared-key-p key keys keywords))
                                           (note "~(~s~), a buffer's value kept by Tuatara under that key, not in its API"
                                                 key)))
                                       (let ((named (named-symbol x)))
                                         (when (and named (eq (symbol-package named) own))
                                           (cond ((and (eq (car x) 'quote) (tuatara-defines-p named) (not (defined-p named)))
                                                  (note "~(~a~), Tuatara's own named as data, not in its API" named))
                                                 ((and (consp (cdr x)) (eq (car x) 'tuatara.buffer:buffer-local)
                                                       (shared-key-p named keys keywords))
                                                  (note "~(~a~), a buffer's value kept by Tuatara under that name, not in its API"
                                                        named)))))
                                       (walk (car x)) (walk (cdr x))))))
                       (walk form)))))))
    (sort found #'string<)))

(defun test-package (name folder)
  "Test the package NAME of FOLDER, a package repository's top, against this
source tree: load it as load-package loads one fetched anew, with its tests
\(TUATARA.PACKAGES:LOAD-PACKAGE-FOR-TEST), say each name it uses outside
Tuatara's API (OUTSIDE-API), then run its tests, the test of its name.
Returns the exit code for the repository's test run: 0 when the package
loads, keeps to the API and passes its tests, 1 otherwise."
  (let ((refused (tuatara.packages:load-package-for-test name folder))
        (failed nil))
    (cond (refused
           (format t "~&~a is refused: ~a~%" name refused)
           (setf failed t))
          (t
           (dolist (use (outside-api name folder))
             (format t "~&~a uses ~a~%" name use)
             (setf failed t))
           (let ((test (find-symbol (string-upcase name) :tuatara.tests)))
             (when (and test (not (eql (status (test test :report 'plain)) :passed)))
               (setf failed t)))))
    (remove-test-directories)
    (if failed 1 0)))

(define-test (package-runner loaded-checked-and-refused)
  ;; loaded as a fetched package is, its tests too; what it reaches past the API said
  (with-test-directory (dir)
    (alexandria:write-string-into-file
     (format nil "(defpackage #:runnerpkg (:use #:cl #:tuatara))~%(in-package #:runnerpkg)~%~
                  (defun runnerpkg-fine () (current-buffer))~%~
                  (defun runnerpkg-reach () (tuatara.git::git-status-render nil))~%~
                  (defun runnerpkg-named () 'git-stage)~%")
     (merge-pathnames "runnerpkg.lisp" dir))
    (alexandria:write-string-into-file
     (format nil "(in-package #:tuatara.tests)~%(setf (get 'package-runner :tests-loaded) t)~%")
     (merge-pathnames "test.lisp" dir))
    (setf (get 'package-runner :tests-loaded) nil)
    (unwind-protect
         (progn
           (false (tuatara.packages:load-package-for-test "runnerpkg" dir))
           (true (get 'package-runner :tests-loaded))
           (let ((uses (outside-api "runnerpkg" dir)))
             (true (find "tuatara.git::git-status-render" uses :test (lambda (name use) (search name use))) "~s" uses)
             ;; a name of git's, a feature package of its own, named as data
             (true (find "git-stage, Tuatara's own" uses :test (lambda (name use) (search name use))) "~s" uses)
             (false (find "current-buffer" uses :test (lambda (name use) (search name use))) "~s" uses)))
      (when (find-package '#:runnerpkg) (delete-package '#:runnerpkg))))
  ;; one that uses a name this Tuatara lacks is refused, why said
  (with-test-directory (dir)
    (alexandria:write-string-into-file
     (format nil "(defpackage #:runnerlack (:use #:cl #:tuatara))~%(in-package #:runnerlack)~%~
                  (defun runnerlack-call () (tuatara.defaults::no-such-function-anywhere))~%")
     (merge-pathnames "runnerlack.lisp" dir))
    (unwind-protect
         (let ((refused (tuatara.packages:load-package-for-test "runnerlack" dir)))
           (true (stringp refused))
           (true (search "lacks" refused) "~a" refused))
      (when (find-package '#:runnerlack) (delete-package '#:runnerlack)))))
