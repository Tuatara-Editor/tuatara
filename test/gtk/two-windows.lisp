;;;; test/gtk/two-windows.lisp --- Two frames in two real windows, for the test gtk two-windows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway):
;;;; FILE-A FILE-B OUT. Starts Tuatara from these sources with --gui --frames 2
;;;; on the two files, then types in the second window, closes it with its
;;;; close button's signal, types in the first, opens a frame with C-x 5 2 and
;;;; gives the first window the keyboard again, measures the first window's
;;;; painting as check-health does, and writes to OUT what it saw at each step,
;;;; as a property list. Every event a window hands on names its frame.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara/terminal :silent t)
(import 'tuatara.runtime:wait-until)

(defvar *seen* '()
  "What was seen at each step, newest first, as a property list.")

(defun see (key value)
  "Note VALUE as what was seen of KEY."
  (setf (getf *seen* key) value))

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

(defvar *to-present* nil
  "The window given the keyboard next (ON-PRESENT).")

(tuatara.frontend.gtk::define-callback on-present :boolean ((data :pointer))
  (declare (ignore data))
  (tuatara.gtk:gtk-window-present (tuatara.frontend.gtk::gtk-window *to-present*))
  (tuatara.gtk:gtk-widget-grab-focus (tuatara.frontend.gtk::gtk-area *to-present*))
  nil)

(defvar *unnamed* '()
  "The events a window handed on naming no frame, but the editor's wakes.")

;; every event put in the editor's queue: one of a window's names the frame it happened in
(sb-int:encapsulate 'tuatara.frontend:queue-push 'naming
                    (lambda (function queue event)
                      (unless (or (tuatara.frontend:tick-p event) (tuatara.frontend:event-frame event))
                        (push (tuatara.frontend:event-kind event) *unnamed*))
                      (funcall function queue event)))

(defun editor-frames ()
  "The frames of the editor running."
  (tuatara.command:editor-frames (tuatara.command:running-editor)))

(defun steps ()
  "Go through the steps, noting what is seen."
  (see :windows (length (wait-until (lambda () (let ((open (windows)))
                                               (and (= 2 (length open)) (every #'tuatara.frontend.gtk::gtk-shown open)
                                                    open))) 30)))
  (destructuring-bind (&optional first second &rest others) (windows)
    (declare (ignore others))
    (when second
      (see :first-shows-a (and (wait-until (lambda () (search "alpha" (shown-text first))) 30) t))
      (see :second-shows-b (and (wait-until (lambda () (search "beta" (shown-text second))) 30) t))
      ;; typing in the second window: its frame's buffer, the first window painted again only for
      ;; work taken in, never for each key
      (sleep 1)
      (let ((paints 0))
        (sb-int:encapsulate 'tuatara.frontend:frontend-end-frame 'counting
                            (lambda (function frontend)
                              (when (eq frontend first) (incf paints))
                              (funcall function frontend)))
        (type-keys second "i z y x w v u t s r q p o n m l k j i h g")
        (see :typed-in-second
             (and (wait-until (lambda () (search "zyxwvutsrqponmlkjihgbeta" (shown-text second))) 30) t))
        (sleep 0.5)
        (sb-int:unencapsulate 'tuatara.frontend:frontend-end-frame 'counting)
        (see :first-painted-while-typing paints))
      ;; the second window's close button: its frame closed, the editor going on
      (setf *to-close* second)
      (tuatara.gtk:g-idle-add (tuatara.gtk:callback on-close-pressed) (tuatara.io:null-pointer))
      (see :closed-one (and (wait-until (lambda () (= 1 (length (windows)))) 30) t))
      (see :frames-left (length (editor-frames)))
      (type-keys first "i y")
      (see :first-still-typed (and (wait-until (lambda () (search "yalpha" (shown-text first))) 30) t))
      ;; C-x 5 2 opens another window, worked in; the first given the keyboard again is the one worked in
      (type-keys first "<escape> C-x 5 2")
      (see :opened-by-key (and (wait-until (lambda () (= 2 (length (windows)))) 30) t))
      (see :new-worked-in (and (wait-until (lambda ()
                                           (eq (tuatara.command::editor-frame (tuatara.command:running-editor))
                                               (second (editor-frames)))) 30)
                               t))
      (setf *to-present* first)
      (tuatara.gtk:g-idle-add (tuatara.gtk:callback on-present) (tuatara.io:null-pointer))
      (see :focus-selects (and (wait-until (lambda ()
                                           (eq (tuatara.command::editor-frame (tuatara.command:running-editor))
                                               (first (editor-frames))))
                                         10)
                               t))
      ;; the first window's frames counted from its frame clock, as check-health measures them
      (let ((measured (vector (make-array 0 :adjustable t :fill-pointer 0) (make-array 0 :adjustable t :fill-pointer 0)
                              (make-array 0 :adjustable t :fill-pointer 0))))
        (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock first))
          (setf (tuatara.frontend.gtk::gtk-measured first) measured))
        (tuatara.gtk:g-idle-add (tuatara.gtk:callback tuatara.frontend.gtk::on-measure-frames)
                                (tuatara.frontend.gtk::window-data first))
        (sleep 0.5)
        (type-keys first "i a b c")
        (see :frames-measured (and (wait-until (lambda () (plusp (length (aref measured 2)))) 10) t)))
      (see :unnamed *unnamed*))))

(sb-thread:make-thread
 (lambda ()
   (handler-case (steps)
     (error (e) (see :error (princ-to-string e))))
   (with-open-file (o (third (uiop:command-line-arguments)) :direction :output :if-exists :supersede)
     (with-standard-io-syntax
       (prin1 *seen* o)))
   (sb-ext:exit :code 0 :abort t)))

(tuatara.terminal:main (list "--gui" "--frames" "2" (first (uiop:command-line-arguments))
                             (second (uiop:command-line-arguments))))
