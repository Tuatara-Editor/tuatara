;;;; test/stable-surface.lisp --- What packages and init files are written against, kept from one release to the next.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The stable surface is the package API (the names TUATARA exports, and
;;;; those of the libraries it lets packages use, with the arguments its
;;;; functions and macros take), the settings an init file
;;;; sets (the variables TUATARA-USER exports) and the names of the commands
;;;; keys are bound to. test/api/stable-surface.sexp pins it as the last
;;;; release had it; tools/pin-stable-surface.sh writes it again at each
;;;; version bump. From 1.0.0 on, a name pinned that is gone, or a function
;;;; whose old calls no longer fit its arguments, fails the test unless the
;;;; major version went up since the pin: a breaking change waits for a major
;;;; release. Names added never fail it.

(in-package #:tuatara.tests)

(define-test stable-surface
  :description "The package API, the init file's settings and the commands' names keep working within a major version from 1.0.0 on.")

(defparameter *stable-surface-file* (asdf:system-relative-pathname :tuatara "test/api/stable-surface.sexp")
  "The pinned stable surface: the version it was written at, then a row for each name.")

(defun lambda-list-shape (lambda-list)
  "LAMBDA-LIST in the parts that decide which calls fit it: the number of
required arguments, the names of the optional ones, the keywords, whether it
takes the rest and whether it allows other keywords."
  (let ((required 0) (optional '()) (keys '()) (rest nil) (other nil) (state :required))
    (dolist (part lambda-list)
      (case part
        ((&optional) (setf state :optional))
        ((&rest &body) (setf state :rest rest t))
        ((&key) (setf state :key))
        ((&allow-other-keys) (setf other t))
        ((&aux) (setf state :aux))
        ((&whole &environment) (setf state :skip))
        (t (ecase state
             (:required (incf required))
             (:optional (push (string (if (consp part) (first part) part)) optional))
             (:key (push (string (let ((name (if (consp part) (first part) part)))
                                   (if (consp name) (first name) name)))
                         keys))
             ((:rest :aux) nil)
             (:skip (setf state :required))))))
    (list required (reverse optional) (sort keys #'string<) rest other)))

(defun lambda-list-takes-p (old new)
  "True when every call that fits the shape OLD (LAMBDA-LIST-SHAPE) fits NEW too:
no more required arguments, room for as many in all, the same keywords and
more, the rest still taken."
  (destructuring-bind (old-required old-optional old-keys old-rest old-other) old
    (destructuring-bind (new-required new-optional new-keys new-rest new-other) new
      (and (<= new-required old-required)
           (or new-rest
               (and (not old-rest)
                    (>= (+ new-required (length new-optional)) (+ old-required (length old-optional)))))
           (or new-other (subsetp old-keys new-keys :test #'string=))
           (or (not old-other) new-other)))))

(defun library-name (symbol)
  "SYMBOL, a name of a library packages may use, written as a package writes
it: UIOP:GETENV, CL-PPCRE:SCAN."
  (format nil "~a:~a" (if (eq symbol (find-symbol (symbol-name symbol) "UIOP")) "UIOP" (package-name (symbol-package symbol)))
          (symbol-name symbol)))

(defun api-row (symbol &optional (name (symbol-name symbol)))
  "The pinned row of SYMBOL, a name of the package API written NAME: its kind,
and for a function or macro the shape of its arguments."
  (cond ((macro-function symbol)
         (list :api name :macro (lambda-list-shape (sb-introspect:function-lambda-list (macro-function symbol)))))
        ((fboundp symbol)
         (list :api name :function (lambda-list-shape (sb-introspect:function-lambda-list (fdefinition symbol)))))
        ((boundp symbol) (list :api name :variable))
        (t (list :api name :name))))

(defun stable-surface ()
  "The stable surface as it is now, a row for each name: the package API's
and the libraries' it lets packages use, the init file's settings, Tuatara's
commands."
  (let ((rows '()))
    (do-external-symbols (symbol :tuatara) (push (api-row symbol) rows))
    (dolist (symbol tuatara.defaults::*api-from-libraries*) (push (api-row symbol (library-name symbol)) rows))
    (do-external-symbols (symbol :tuatara-user)
      (when (boundp symbol) (push (list :setting (symbol-name symbol)) rows)))
    (loop for symbol being the hash-keys of tuatara.command::*commands*
          for home = (package-name (symbol-package symbol))
          when (and (uiop:string-prefix-p "TUATARA" home) (string/= home "TUATARA.TESTS"))
            do (push (list :command home (symbol-name symbol)) rows))
    (sort rows #'string< :key (lambda (row) (format nil "~{~a~^ ~}" (subseq row 0 (if (eq (first row) :command) 3 2)))))))

(defun surface-breaks (pinned now)
  "The rows of PINNED, a stable surface, that NOW no longer keeps: a name gone
or of another kind, a function or macro whose old calls do not fit it."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row now) (setf (gethash (subseq row 0 (if (eq (first row) :command) 3 2)) table) row))
    (loop for row in pinned
          for found = (gethash (subseq row 0 (if (eq (first row) :command) 3 2)) table)
          unless (and found
                      (or (not (eq (first row) :api))
                          (and (eq (third row) (third found))
                               (or (not (member (third row) '(:function :macro)))
                                   (lambda-list-takes-p (fourth row) (fourth found))))))
            collect row)))

(defun read-stable-surface (&optional (file *stable-surface-file*))
  "The version FILE was pinned at, and its rows, as two values."
  (with-open-file (in file)
    (let ((head (read in)))
      (values (getf head :version)
              (loop for row = (read in nil in) until (eq row in) collect row)))))

(defun write-stable-surface (&optional (file *stable-surface-file*))
  "Pin the stable surface as it is now in FILE, with the version it is pinned at."
  (ensure-directories-exist file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((*print-case* :downcase) (*print-right-margin* 100000))
      (format out ";; The stable surface, pinned by tools/pin-stable-surface.sh at each version bump: (:api NAME KIND [ARGUMENTS]),~%~
                   ;; ARGUMENTS (REQUIRED OPTIONAL KEYS REST OTHER-KEYS); (:setting NAME); (:command PACKAGE NAME).~%")
      (prin1 (list :version (asdf:component-version (asdf:find-system :tuatara))) out)
      (terpri out)
      (dolist (row (stable-surface)) (prin1 row out) (terpri out)))))

(defun version-major (version)
  "The major part of VERSION, 1 in 1.2.3."
  (parse-integer version :junk-allowed t))

(define-test (stable-surface calls-that-still-fit)
  ;; what a function may become without breaking its callers, and what breaks them
  (flet ((takes (old new) (lambda-list-takes-p (lambda-list-shape old) (lambda-list-shape new))))
    (true (takes '(a b) '(a b)))
    (true (takes '(a) '(a &optional b)))
    (true (takes '(a &key b) '(a &key b c)))
    (true (takes '(a &optional b) '(a &rest more)))
    (true (takes '(a &key b) '(a &key ((:b x)) &allow-other-keys)))
    (false (takes '(a) '(a b)) "a required argument added")
    (false (takes '(a b) '(a)) "an argument taken away")
    (false (takes '(a &key b) '(a &key c)) "a keyword taken away")
    (false (takes '(a &rest more) '(a &optional b)) "the rest no longer taken")
    (false (takes '(a &key b) '(a b)) "keywords no longer taken"))
  ;; names gone or changed are found; names added are not breaks
  (let ((pinned '((:api "KEPT" :function (1 () () nil nil)) (:api "GONE" :function (0 () () nil nil))
                  (:api "NARROWED" :function (1 () ("B") nil nil)) (:setting "*KEPT*") (:setting "*GONE*")
                  (:command "TUATARA.DEFAULTS" "GONE-COMMAND")))
        (now '((:api "KEPT" :function (1 ("C") () nil nil)) (:api "NARROWED" :function (1 () () nil nil))
               (:api "ADDED" :variable) (:setting "*KEPT*"))))
    (is equal '("GONE" "NARROWED" "*GONE*" "GONE-COMMAND")
        (mapcar (lambda (row) (car (last (subseq row 0 (if (eq (first row) :command) 3 2))))) (surface-breaks pinned now)))))

(define-test (stable-surface kept-within-a-major-version)
  ;; from 1.0.0 on, the surface pinned at the last release is kept until the major version goes up
  (multiple-value-bind (version pinned) (read-stable-surface)
    (true version "the pin says the version it was written at")
    (true (> (length pinned) 1000) "the pin holds the surface: ~d rows" (length pinned))
    (let ((now-major (version-major (asdf:component-version (asdf:find-system :tuatara)))))
      (when (and (>= now-major 1) (= now-major (version-major version)))
        (let ((breaks (surface-breaks pinned (stable-surface))))
          (is equal '() breaks
              "Breaking changes within ~d.x (a major release is the place for them): ~s" now-major breaks))))))
