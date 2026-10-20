;;;; tools/quicklisp.lisp --- Load Quicklisp, wherever it is installed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by the scripts that load Tuatara: Quicklisp's setup.lisp from
;;;; $QUICKLISP_SETUP, else ~/quicklisp/ (Quicklisp's own default), else
;;;; ~/.local/share/quicklisp/; nothing when it is loaded already, as from
;;;; ~/.sbclrc.

(unless (find-package :ql)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((given (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and given (plusp (length given)) (pathname given)))
                                          (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))
                                          (merge-pathnames ".local/share/quicklisp/setup.lisp" (user-homedir-pathname)))))))
    (if setup
        (load setup)
        (error "Quicklisp is not installed: see HACKING.org, or set QUICKLISP_SETUP to its setup.lisp"))))
