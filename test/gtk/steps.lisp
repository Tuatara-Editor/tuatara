;;;; test/gtk/steps.lisp --- What the scripts of test/gtk/ share.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded first by each script a test runs with sbcl --script on a display of
;;;; its own: loads Tuatara from these sources, and gives the script a record
;;;; of what it saw, the windows open and what they show, keys typed in them,
;;;; and RUN-STEPS, which goes through the script's steps in a thread of their
;;;; own and always writes what was seen, also when a step fails or the steps
;;;; take too long.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara/terminal :silent t)
(import 'tuatara.runtime:wait-until)

(defvar *seen* '()
  "What was seen at each step, newest first, as a property list.")

(defvar *seen-lock* (sb-thread:make-mutex :name "seen")
  "Held while *SEEN* is written, or written out.")

(defvar *out* nil
  "The file what was seen is written to, as RUN-STEPS was given it.")

(defun see (key value)
  "Note VALUE as what was seen of KEY."
  (sb-thread:with-mutex (*seen-lock*) (setf (getf *seen* key) value)))

(defun write-seen-and-exit ()
  "Write what was seen to *OUT* and end this program."
  (sb-thread:with-mutex (*seen-lock*)
    (with-open-file (o *out* :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (prin1 *seen* o))))
  (sb-ext:exit :code 0 :abort t))

(defun windows ()
  "The frontends of the windows open, the oldest first."
  (tuatara.frontend.gtk::open-windows))

(defun shown-frame (f)
  "The frame F's window shows now, or NIL before it shows one."
  (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock f)) (tuatara.frontend.gtk::gtk-shown f)))

(defun shown-text (f)
  "The rows F's window shows now, as one string."
  (let ((frame (shown-frame f)))
    (if frame
        (format nil "~{~a~^~%~}" (loop for row across (tuatara.frontend.gtk::frame-rows frame)
                                       when row collect (first row)))
        "")))

(defun shown-pictures (f)
  "How many pictures F's window shows now."
  (let ((frame (shown-frame f)))
    (if frame (length (tuatara.frontend.gtk::frame-images frame)) 0)))

(defun first-window-shown (seconds)
  "The frontend of the first window open once it shows a frame, waiting up to
SECONDS for it; NIL when none showed in that time."
  (wait-until (lambda () (let ((open (windows))) (and open (shown-frame (first open)) (first open)))) seconds))

(defun type-keys (f keys)
  "Type KEYS, a key sequence as written (\"C-x 5 2\"), in F's window."
  (dolist (spec (tuatara.frontend:parse-key-sequence keys))
    (tuatara.frontend.gtk::push-key f (tuatara.frontend:spec-key spec))))

(defvar *to-close* nil
  "The window whose close button is pressed next (ON-CLOSE-PRESSED).")

(tuatara.frontend.gtk::define-callback on-close-pressed :boolean ((data :pointer))
  (declare (ignore data))
  ;; as the window's close button does: GTK asks the window to close
  (tuatara.runtime:foreign-call "gtk_window_close" sb-alien:void
                                (sb-sys:system-area-pointer (tuatara.frontend.gtk::gtk-window *to-close*)))
  nil)

(defun run-steps (steps out &key (give-up 240) (exit t))
  "Go through STEPS, a function of no arguments, in a thread of its own, and
the thread. What was seen is written to the file OUT and this program ended
when STEPS fails (:ERROR seen, with its message), after GIVE-UP seconds
(:TIMED-OUT seen), or, when EXIT is true, once STEPS returns; with EXIT
false the caller writes it (WRITE-SEEN-AND-EXIT)."
  (setf *out* out)
  (sb-thread:make-thread (lambda () (sleep give-up) (see :timed-out t) (write-seen-and-exit)) :name "watchdog")
  (sb-thread:make-thread (lambda ()
                           (handler-case (funcall steps)
                             (error (e) (see :error (princ-to-string e)) (write-seen-and-exit)))
                           (when exit (write-seen-and-exit)))
                         :name "steps"))
