;;;; test/feature-packages.lisp --- Features with Lisp packages of their own keep their insides.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test feature-packages
  :description "Each feature with a Lisp package of its own keeps its insides from the rest of the editor, finds its settings under the names an init file sets, and has its commands found by their names.")

(defparameter *feature-packages*
  '(("TUATARA.GIT" "GIT-STATUS-RENDER" "*GIT-LOG-LENGTH*" "git-stage")
    ("TUATARA.TERMINAL-EMULATOR" "TERMINAL-SHOWN-TEXT" "*TERMINAL-SCROLLBACK*" "terminal")
    ("TUATARA.LSP" "LSP-TAKE-IN" "*LSP-WAIT*" "lsp-restart")
    ("TUATARA.COMPILE" "COMPILE-PLACE-PATH" "*COMPILE-COMMAND*" "compile")
    ("TUATARA.PACKAGES" "PACKAGE-ENTRY" "*PACKAGE-ASK-BEFORE-LOADING*" "package-update"))
  "Each feature with a Lisp package of its own, as (PACKAGE INSIDE SETTING
COMMAND): a name of its insides, one of its settings and one of its commands.")

(define-test (feature-packages insides-kept)
  (loop for (name inside) in *feature-packages*
        for package = (find-package name)
        do (true package "~a is a package" name)
           (when package
             (is eq :internal (nth-value 1 (find-symbol inside package)) "~a::~a" name inside)
             (true (fboundp (find-symbol inside package)))
             (false (find-symbol inside :tuatara.defaults) "~a in the defaults" inside))))

(define-test (feature-packages settings-shared)
  ;; a setting is one symbol in the init file's package and in the feature's code
  (loop for (name) in *feature-packages*
        for package = (find-package name)
        do (true package "~a is a package" name)
           (when package
             (do-external-symbols (setting :tuatara-user)
               (when (eq :special (sb-int:info :variable :kind setting))
                 (is eq setting (find-symbol (symbol-name setting) package) "~a in ~a" setting name))))))

(define-test (feature-packages init-file-sets-setting)
  ;; read as an init file is read: in TUATARA-USER, form by form
  (loop for (name nil setting) in *feature-packages*
        for package = (find-package name)
        do (true package "~a is a package" name)
           (when package
             (let* ((symbol (find-symbol setting package))
                    (was (symbol-value symbol)))
               (with-test-directory (dir)
                 (let ((file (merge-pathnames "init.lisp" dir)))
                   (with-open-file (out file :direction :output)
                     (format out ";; an init file~%(setf ~(~a~) 7)~%" setting))
                   (unwind-protect
                        (let ((*package* (find-package :tuatara-user))
                              (tuatara.defaults::*init-file-outcome* :read))
                          (tuatara.defaults::read-init-file (namestring file))
                          (is eq :read tuatara.defaults::*init-file-outcome*)
                          (is eql 7 (symbol-value symbol)))
                     (setf (symbol-value symbol) was))))))))

(define-test (feature-packages commands-found-by-name)
  (loop for (name nil nil command) in *feature-packages*
        for symbol = (tuatara.command:command-symbol command)
        do (true symbol "~a is a command" command)
           (when symbol
             (is string= name (package-name (symbol-package symbol)))
             (true (tuatara.command:find-command command)))))
