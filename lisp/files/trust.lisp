;;;; lisp/files/trust.lisp --- Projects you trust to run their own programs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A project's language servers run its code (a build script, a plugin, a
;;;; macro), and git runs the programs a repository names for itself (hooks,
;;;; filters). Neither happens by itself for a project you have not said you
;;;; trust: once asked and kept between sessions, or in *LSP-TRUSTED-PROJECTS*.
;;;; A project is known by where it really is, every link followed, so that a
;;;; link from a trusted folder to another lends it no trust.

(in-package #:tuatara.defaults)

(defvar *lsp-trusted-projects* '()
  "Folders whose projects you trust to run their own programs: their language
servers start by themselves as their files are opened, and git runs the
programs they name for themselves (hooks, filters). Besides those trusted when
asked (kept between sessions). Names of folders, ~ for your home folder.")

(defvar *lsp-trusted-kept* 500
  "How many projects trusted when asked are remembered.")

(define-state-list *lsp-trusted* "lsp-trusted" 'unescape-path 'escape-path '*lsp-trusted-kept*)

(defun real-folder (path)
  "Where the folder PATH really is, every link followed and . and .. taken out,
ending in /; NIL when it is not there."
  (let ((real (ignore-errors (tuatara.io:real-path (tuatara.io:native (tuatara.io:expand-path path))))))
    (and real (tuatara.io:slash-ended real))))

(defvar *trusted-folders-seen* '()
  "Where the folders of *LSP-TRUSTED-PROJECTS* really are, as the setting was
last looked at: (FOLDERS . REAL), FOLDERS the setting's folders as written and
REAL where each really is, NIL for one not there then.")

(defun trusted-setting-folders ()
  "Where the folders of *LSP-TRUSTED-PROJECTS* really are, those there: each
looked up once while the setting stays the same, one not there looked for
again each time."
  (let ((folders (setting-list '*lsp-trusted-projects* *lsp-trusted-projects*))
        (seen *trusted-folders-seen*))
    (unless (equal folders (car seen))
      (setf seen (cons (copy-list folders) (make-list (length folders)))
            *trusted-folders-seen* seen))
    (loop for folder in folders
          for cell on (cdr seen)
          for real = (or (car cell) (setf (car cell) (real-folder folder)))
          when real collect real)))

(defun project-trusted-p (root)
  "True when the project at ROOT may run its own programs: it, where it really
is, was trusted when asked (kept by where it really was then), or is in one
of *LSP-TRUSTED-PROJECTS* (where they really are)."
  (let ((real (real-folder root)))
    (and real
         (or (member real (state-list-entries *lsp-trusted*) :test #'equal)
             (some (lambda (trusted) (tuatara.io:path-within-p real trusted))
                   (trusted-setting-folders))))))

(defun trust-project (root what)
  "True when the project at ROOT may run its own programs: trusted already,
or asked now, WHAT saying what of it would run (\"its language servers
rust-analyzer\"), and remembered when the answer is yes."
  (or (project-trusted-p root)
      (let ((real (real-folder root)))
        (when (and real
                   (yes-or-no-p* (format nil "Trust ~a? ~a and git runs the programs it names (hooks, filters), from now on"
                                         (abbreviated-path real) what)))
          (state-list-put *lsp-trusted* (list real))
          t))))
