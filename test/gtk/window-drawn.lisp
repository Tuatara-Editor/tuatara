;;;; test/gtk/window-drawn.lisp --- A window drawn both ways, for the test gtk window-drawn-as-painted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway):
;;;; FILE OUT [picture]. Opens a Tuatara window from these sources on FILE, and
;;;; once it shows the file (with picture, once the file's picture shows),
;;;; takes a picture of the whole window as GTK renders it, drawn as the
;;;; graphics card is given it, then another of the same window painted whole,
;;;; and writes to OUT how many textures the first held, how many octets the
;;;; two differ in, and in how many by more than 16.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara/terminal :silent t)

(defun window-picture (window)
  "The pixels of WINDOW as GTK renders it now, as a vector of octets, 32 bits a
pixel, and the texture nodes it was drawn with, by count."
  (let* ((width (cffi:foreign-funcall "gtk_widget_get_width" :pointer window :int))
         (height (cffi:foreign-funcall "gtk_widget_get_height" :pointer window :int))
         (paintable (cffi:foreign-funcall "gtk_widget_paintable_new" :pointer window :pointer))
         (snapshot (cffi:foreign-funcall "gtk_snapshot_new" :pointer)))
    (cffi:foreign-funcall "gdk_paintable_snapshot" :pointer paintable :pointer snapshot
                          :double (float width 1d0) :double (float height 1d0) :void)
    (let* ((node (let ((node (cffi:foreign-funcall "gtk_snapshot_free_to_node" :pointer snapshot :pointer)))
                   (if (cffi:null-pointer-p node) (error "the window drew nothing") node)))
           (renderer (cffi:foreign-funcall "gsk_cairo_renderer_new" :pointer))
           (textures (let ((bytes (cffi:foreign-funcall "gsk_render_node_serialize" :pointer node :pointer)))
                       (cffi:with-foreign-object (size :size)
                         (let* ((data (cffi:foreign-funcall "g_bytes_get_data" :pointer bytes :pointer size :pointer))
                                (text (cffi:foreign-string-to-lisp data :count (cffi:mem-ref size :size))))
                           (loop with start = 0
                                 for at = (search "texture {" text :start2 start)
                                 while at count t do (setf start (1+ at))))))))
      (cffi:foreign-funcall "gsk_renderer_realize" :pointer renderer :pointer (cffi:null-pointer)
                            :pointer (cffi:null-pointer) :boolean)
      (let* ((texture (cffi:foreign-funcall "gsk_renderer_render_texture" :pointer renderer :pointer node
                                            :pointer (cffi:null-pointer) :pointer))
             (pixels (make-array (* 4 width height) :element-type '(unsigned-byte 8))))
        (cffi:with-foreign-object (data :uint8 (length pixels))
          (cffi:foreign-funcall "gdk_texture_download" :pointer texture :pointer data :size (* 4 width) :void)
          (dotimes (i (length pixels)) (setf (aref pixels i) (cffi:mem-aref data :uint8 i))))
        (values pixels textures)))))

(defvar *on-card* nil
  "The picture of the window drawn as pictures the graphics card is given, and its textures by count.")

(cffi:defcallback show-on-card :boolean ((data :pointer))
  (declare (ignore data))
  (tuatara.frontend.gtk::redraw tuatara.frontend.gtk::*frontend*)
  nil)

(cffi:defcallback picture-on-card :boolean ((data :pointer))
  (declare (ignore data))
  (let ((f tuatara.frontend.gtk::*frontend*))
    (setf *on-card* (multiple-value-list (window-picture (tuatara.frontend.gtk::gtk-window f))))
    ;; then the same window painted whole
    (tuatara.frontend.gtk::drawn-on-card f nil))
  nil)

(cffi:defcallback picture-painted-whole :boolean ((data :pointer))
  (declare (ignore data))
  (let ((whole (window-picture (tuatara.frontend.gtk::gtk-window tuatara.frontend.gtk::*frontend*))))
    (destructuring-bind (on-card textures) *on-card*
      (with-open-file (o (second (uiop:command-line-arguments)) :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (prin1 (list :textures textures
                       :octets (length whole)
                       :differing (if (= (length on-card) (length whole))
                                      (count nil (map 'list #'= on-card whole))
                                      (length whole))
                       :differing-much (if (= (length on-card) (length whole))
                                           (count-if (lambda (d) (> (abs d) 16)) (map 'list #'- on-card whole))
                                           (length whole)))
                 o)))))
  (sb-ext:exit :code 0 :abort t))

(sb-thread:make-thread
 (lambda ()
   (loop until (and tuatara.frontend.gtk::*frontend* (tuatara.frontend.gtk::gtk-shown tuatara.frontend.gtk::*frontend*))
         do (sleep 0.1))
   ;; a picture: once it shows
   (when (equal (third (uiop:command-line-arguments)) "picture")
     (loop repeat 600
           until (let ((frame (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock tuatara.frontend.gtk::*frontend*))
                                (tuatara.frontend.gtk::gtk-shown tuatara.frontend.gtk::*frontend*))))
                   (and frame (tuatara.frontend.gtk::frame-images frame)))
           do (sleep 0.1)))
   ;; the file's text laid out and coloured, then each step once GTK has laid out the one before
   (sleep 2)
   (dolist (step (list (cffi:callback show-on-card) (cffi:callback picture-on-card) (cffi:callback picture-painted-whole)))
     (tuatara.frontend.gtk::g-idle-add step (cffi:null-pointer))
     (sleep 1))))

(tuatara.terminal:main (list "--gui" (first (uiop:command-line-arguments))))
