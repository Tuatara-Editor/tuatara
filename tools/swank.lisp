;;;; tools/swank.lisp --- Start a Swank (SLIME) or Slynk (Sly) server for the host Emacs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;; Loads the server that ships with the Emacs package, so protocol versions match:
;;;; the swank-loader.lisp or slynk-loader.lisp $SWANK_LOADER names, else the one
;;;; installed last in the usual places of an Emacs configuration, else
;;;; Quicklisp's Swank.
;;;; Listens on localhost:4005 until killed. Loads tuatara/tests so the image is ready to work in.

(load (merge-pathnames "quicklisp.lisp" *load-truename*))

(defun find-loader (pattern)
  "Return the file matching PATTERN where SLIME and SLY are usually installed
that was written last (the one installed or updated last), or NIL."
  (let ((hits (loop for dir in '(".config/emacs/elpa/" ".emacs.d/elpa/"
                                 ".config/emacs/straight/repos/" ".emacs.d/straight/repos/"
                                 ".config/emacs/.local/straight/repos/" ".emacs.d/.local/straight/repos/")
                    append (directory (merge-pathnames (concatenate 'string dir pattern)
                                                       (user-homedir-pathname))))))
    (first (sort hits #'> :key (lambda (path) (or (file-write-date path) 0))))))

(let* ((given (let ((name (uiop:getenv "SWANK_LOADER"))) (and name (plusp (length name)) (probe-file name))))
       (slynk-given (and given (search "slynk" (file-namestring given))))
       (swank (if given (and (not slynk-given) given) (find-loader "slime*/swank-loader.lisp")))
       (slynk (if given (and slynk-given given) (find-loader "sly*/slynk/slynk-loader.lisp"))))
  (cond (swank
         (format t "~&Loading Swank from ~a~%" swank)
         (load swank)
         (funcall (intern "INIT" "SWANK-LOADER"))
         (ql:quickload :tuatara/tests :silent t)
         (funcall (intern "CREATE-SERVER" "SWANK") :port 4005 :dont-close t)
         (format t "~&Swank listening on localhost:4005.  Connect: M-x slime-connect RET localhost RET 4005 RET~%"))
        (slynk
         (format t "~&Loading Slynk from ~a~%" slynk)
         (load slynk)
         (funcall (intern "INIT" "SLYNK-LOADER"))
         (ql:quickload :tuatara/tests :silent t)
         (funcall (intern "CREATE-SERVER" "SLYNK") :port 4005 :dont-close t)
         (format t "~&Slynk listening on localhost:4005.  Connect: M-x sly-connect RET localhost RET 4005 RET~%"))
        (t
         (format t "~&No SLIME or Sly found under the home directory; using Quicklisp swank.~%")
         (ql:quickload '(:swank :tuatara/tests) :silent t)
         (funcall (intern "CREATE-SERVER" "SWANK") :port 4005 :dont-close t)
         (format t "~&Swank listening on localhost:4005.~%"))))
(loop (sleep 60))
