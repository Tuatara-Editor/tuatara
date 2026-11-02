;;;; test/gtk/window-client.lisp --- A client of a daemon in real windows, for the test daemon window-client-in-windows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway):
;;;; NAME RUNTIME FILE DROPPED PICTURE OUT. Starts tuatara --client=NAME --gui
;;;; FILE from these sources, the daemon's sockets in the folder RUNTIME, then
;;;; types in the window, pastes what another program put on the clipboard,
;;;; makes the font a step larger, opens another window with C-x 5 2 (both
;;;; drawn by the renderer the daemon's settings choose), types
;;;; there and closes it with its close button's signal, drops the file
;;;; DROPPED and then the picture PICTURE on the first, and ends the client
;;;; with C-x C-c; it writes to OUT what it saw at each step, and the exit code
;;;; the client ended with, as a property list.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara/terminal :silent t)

(defvar *arguments* (uiop:command-line-arguments)
  "NAME RUNTIME FILE DROPPED PICTURE OUT, as the test gives them.")

(defvar *seen* '()
  "What was seen at each step, newest first, as a property list.")

(defvar *seen-lock* (sb-thread:make-mutex :name "seen")
  "Held while *SEEN* is written, or written out.")

(defun see (key value)
  "Note VALUE as what was seen of KEY."
  (sb-thread:with-mutex (*seen-lock*) (setf (getf *seen* key) value)))

(defun write-seen-and-exit ()
  "Write what was seen to OUT and end this program."
  (sb-thread:with-mutex (*seen-lock*)
    (with-open-file (o (sixth *arguments*) :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (prin1 *seen* o))))
  (sb-ext:exit :code 0 :abort t))

(defun windows ()
  "The frontends of the windows open, the oldest first."
  (tuatara.frontend.gtk::open-windows))

(defun shown-text (f)
  "The rows F's window shows now, as one string."
  (let ((frame (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock f)) (tuatara.frontend.gtk::gtk-shown f))))
    (if frame
        (format nil "~{~a~^~%~}" (loop for row across (tuatara.frontend.gtk::frame-rows frame)
                                       when row collect (first row)))
        "")))

(defun shown-pictures (f)
  "How many pictures F's window shows now."
  (let ((frame (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock f)) (tuatara.frontend.gtk::gtk-shown f))))
    (if frame (length (tuatara.frontend.gtk::frame-images frame)) 0)))

(defun wait-until (test &optional (seconds 30))
  "Wait until TEST, a function of no arguments, is true, SECONDS at most; its value."
  (loop repeat (* 10 seconds)
        for value = (funcall test)
        when value return value
        do (sleep 0.1)))

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

(defun font-steps (f)
  "The steps F's window's font is scaled by."
  (tuatara.frontend:frontend-font-steps f))

(defun steps ()
  "Go through the steps, noting what is seen."
  (let ((first (wait-until (lambda () (let ((open (windows))) (and open (tuatara.frontend.gtk::gtk-shown (first open))
                                                                  (first open)))))))
    (see :window (and first t))
    (when first
      (see :shows-file (and (wait-until (lambda () (search "kiwi window" (shown-text first)))) t))
      (type-keys first "i h e l l o SPC <escape>")
      (see :typed (and (wait-until (lambda () (search "hello kiwi window" (shown-text first)))) t))
      ;; another program's copy, taken from the client's clipboard as the daemon pastes
      (tuatara.frontend:frontend-set-clipboard first "from the desktop ")
      (sleep 1)
      (type-keys first "i C-y <escape>")
      (see :pasted (and (wait-until (lambda () (search "from the desktop" (shown-text first)))) t))
      ;; the font a step larger in this window
      (type-keys first "C-x C-+")
      (see :font-larger (and (wait-until (lambda () (eql 1 (font-steps first))) 10) t))
      (type-keys first "C-g")
      ;; another window of the client, its font as it started
      (type-keys first "C-x 5 2")
      (see :another-window (and (wait-until (lambda () (= 2 (length (windows))))) t))
      (let ((second (second (windows))))
        (when second
          (see :another-shows (and (wait-until (lambda () (search "hello" (shown-text second)))) t))
          (see :another-font-steps (font-steps second))
          ;; both windows drawn as the daemon's init file asks, GSK_RENDERER kept from the programs started
          (see :renderers (list (tuatara.frontend.gtk::gtk-drawn-by first) (tuatara.frontend.gtk::gtk-drawn-by second)))
          (see :renderer-kept (uiop:getenv "GSK_RENDERER"))
          (see :renderer-given (some (lambda (setting) (uiop:string-prefix-p "GSK_RENDERER=" setting))
                                     (tuatara.io:program-environment)))
          (type-keys second "i z z <escape>")
          (see :typed-in-another (and (wait-until (lambda () (search "zz" (shown-text first)))) t))
          (setf *to-close* second)
          (tuatara.gtk:g-idle-add (tuatara.gtk:callback on-close-pressed) (tuatara.io:null-pointer))
          (see :closed-another (and (wait-until (lambda () (= 1 (length (windows))))) t))))
      ;; files dropped on the window: a text, then a picture, sent as a file in memory
      (tuatara.frontend.gtk::push-event first :drop :files (list (fourth *arguments*)) :column 1 :row 1)
      (see :dropped (and (wait-until (lambda () (search "dropped kea" (shown-text first)))) t))
      (tuatara.frontend.gtk::push-event first :drop :files (list (fifth *arguments*)) :column 1 :row 1)
      (see :picture (and (wait-until (lambda () (plusp (shown-pictures first)))) t))
      (type-keys first "C-x C-c"))))

;; the daemon's sockets where the test's daemon listens
(setf tuatara.defaults::*runtime-directory* (second *arguments*))

;; given up on after four minutes, what was seen written all the same
(sb-thread:make-thread (lambda () (sleep 240) (see :timed-out t) (write-seen-and-exit)) :name "watchdog")

(defvar *steps*
  (sb-thread:make-thread (lambda ()
                           (handler-case (steps)
                             (error (e) (see :error (princ-to-string e)) (write-seen-and-exit))))
                         :name "steps")
  "The thread going through the steps.")

(let ((code (tuatara.terminal:main (list (format nil "--client=~a" (first *arguments*)) "--gui" (third *arguments*)))))
  (sb-thread:join-thread *steps* :timeout 30 :default nil)
  (see :exit-code code)
  (see :windows-left (length (windows)))
  (write-seen-and-exit))
