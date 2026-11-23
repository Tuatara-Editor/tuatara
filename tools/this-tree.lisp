;;;; tools/this-tree.lisp --- Load Tuatara's systems from this tree and no other one Quicklisp or ASDF knows of.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by what starts the editor or its tools from the sources (tools/tui.lisp, tools/swank.lisp, in_tree of
;;;; tools/inside.sh), so that a checkout elsewhere, which ASDF may know of first, is never the one run.

(load (merge-pathnames "quicklisp.lisp" *load-truename*))

(defpackage #:tuatara.this-tree
  (:use #:cl)
  (:export #:load-this-tree)
  (:documentation "Loading Tuatara from the tree a tool is run from."))

(in-package #:tuatara.this-tree)

(defvar *tree* (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*))
  "The tree this file is in, as a directory pathname.")

(defun load-this-tree (systems &key (tree *tree*))
  "Load SYSTEMS, a system's name or a list of them, from TREE, this file's tree unless given. When ASDF finds
Tuatara elsewhere all the same, say where and end Lisp with exit 1, nothing loaded."
  (push tree asdf:*central-registry*)
  (let ((found (let ((system (asdf:find-system :tuatara nil))) (and system (asdf:system-source-directory system)))))
    (unless (and found (uiop:pathname-equal (truename found) (truename tree)))
      (format *error-output* "~&The tree found is ~a, not ~a: nothing done.~%" found tree)
      (uiop:quit 1)))
  (ql:quickload systems :silent t))
