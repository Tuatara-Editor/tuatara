;;;; tools/tui.lisp --- Start Tuatara in the terminal from its sources: sbcl --script tools/tui.lisp [FILE...]
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;; tools/tuatara-tui.sh runs the saved executable instead, which starts at once.
(load (merge-pathnames "quicklisp.lisp" *load-truename*))
(ql:quickload :tuatara/terminal :silent t)
(tuatara.terminal:main (uiop:command-line-arguments))
