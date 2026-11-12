;;;; test/gtk/window-drawn.lisp --- A window drawn both ways, for the test gtk window-drawn-as-painted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway):
;;;; FILE OUT [picture | pictures-beside | floated]. Opens a Tuatara window from
;;;; these sources on FILE, and once it shows the file (with picture, once the
;;;; file's picture shows; with pictures-beside, once it shows in two windows side
;;;; by side; with floated, an Org note, once its two pictures show),
;;;; takes a picture of the whole window as GTK renders it, drawn as the
;;;; graphics card is given it, then another of the same window painted whole,
;;;; and writes to OUT how many textures the first held, how many octets the
;;;; two differ in, and in how many by more than 16; or, when a step failed or
;;;; it all took longer than ninety seconds, why, as :ERROR or :TIMED-OUT. With
;;;; floated, also where the window painted whole shows pure red and pure blue
;;;; (COLOUR-BOX), the cell's size, and whether the row beside the red picture's
;;;; top shows text.

(load (merge-pathnames "steps.lisp" *load-truename*))

(defmacro c (name result &rest arguments)
  "Call the C function NAME, which returns RESULT, with ARGUMENTS, alternately
a type and a value, of the types TUATARA.IO:C-TYPE knows."
  `(tuatara.runtime:foreign-call ,name ,(tuatara.io:c-type result)
                                 ,@(loop for (type value) on arguments by #'cddr
                                         collect (list (tuatara.io:c-type type) value))))

(defun window-picture (window)
  "The pixels of WINDOW as GTK renders it now, as a vector of octets, 32 bits a
pixel, the texture nodes it was drawn with, by count, and its width in
pixels; NIL while it has drawn nothing yet."
  (let* ((width (c "gtk_widget_get_width" :int :pointer window))
         (height (c "gtk_widget_get_height" :int :pointer window))
         (paintable (c "gtk_widget_paintable_new" :pointer :pointer window))
         (snapshot (c "gtk_snapshot_new" :pointer)))
    (c "gdk_paintable_snapshot" :void :pointer paintable :pointer snapshot
       :double (float width 1d0) :double (float height 1d0))
    (let* ((node (let ((node (c "gtk_snapshot_free_to_node" :pointer :pointer snapshot)))
                   (if (tuatara.io:null-pointer-p node) (return-from window-picture nil) node)))
           (renderer (c "gsk_cairo_renderer_new" :pointer))
           (textures (let ((bytes (c "gsk_render_node_serialize" :pointer :pointer node)))
                       (sb-alien:with-alien ((size sb-alien:unsigned-long 0))
                         (let* ((data (c "g_bytes_get_data" :pointer :pointer bytes
                                         :pointer (sb-alien:alien-sap (sb-alien:addr size))))
                                (text (tuatara.io:sap-string data size)))
                           (loop with start = 0
                                 for at = (search "texture {" text :start2 start)
                                 while at count t do (setf start (1+ at))))))))
      (c "gsk_renderer_realize" :boolean :pointer renderer :pointer (tuatara.io:null-pointer)
         :pointer (tuatara.io:null-pointer))
      (let* ((texture (c "gsk_renderer_render_texture" :pointer :pointer renderer :pointer node
                         :pointer (tuatara.io:null-pointer)))
             (pixels (make-array (* 4 width height) :element-type '(unsigned-byte 8))))
        (tuatara.io:with-octets-sap (data pixels)
          (c "gdk_texture_download" :void :pointer texture :pointer data :size (* 4 width)))
        (values pixels textures width)))))

(defun colour-box (pixels width blue green red)
  "Where PIXELS, WIDTH pixels to a row of four octets each (blue, green, red,
alpha), show the colour BLUE GREEN RED fully opaque: (LEFT TOP RIGHT BOTTOM),
the last pixels of it included; NIL where none does."
  (let (left top right bottom)
    (loop for i from 0 below (length pixels) by 4
          when (and (= (aref pixels i) blue) (= (aref pixels (+ i 1)) green) (= (aref pixels (+ i 2)) red)
                    (= (aref pixels (+ i 3)) 255))
            do (let ((x (mod (floor i 4) width)) (y (floor i (* 4 width))))
                 (setf left (min (or left x) x) right (max (or right x) x)
                       top (min (or top y) y) bottom (max (or bottom y) y))))
    (and left (list left top right bottom))))

(defun colours-in (pixels width left top right bottom)
  "How many colours PIXELS, WIDTH pixels to a row, show from LEFT TOP to RIGHT
BOTTOM, those included."
  (let ((seen (make-hash-table)))
    (loop for y from top to bottom
          do (loop for x from left to right
                   for i = (* 4 (+ x (* y width)))
                   do (setf (gethash (logior (aref pixels i) (ash (aref pixels (+ i 1)) 8)
                                             (ash (aref pixels (+ i 2)) 16))
                                     seen)
                            t)))
    (hash-table-count seen)))

(defvar *pictures* '()
  "The pictures of the window taken so far, the newest first: each its pixels
and the texture nodes it was drawn with, by count.")

(defvar *steps-done* 0
  "How many of the steps run in GTK's own thread have finished.")

(defvar *window* nil
  "The frontend of the window the steps are run on.")

(defmacro define-step (name &body body)
  "Define NAME as a callback running BODY in GTK's own thread, once it is
idle; what BODY signals is seen as :ERROR, named for the step, and
*STEPS-DONE* counts it run."
  `(tuatara.frontend.gtk::define-callback ,name :boolean ((data :pointer))
     (declare (ignore data))
     (handler-case (progn ,@body)
       (error (e) (see :error (format nil "~(~a~): ~a" ',name e))))
     (incf *steps-done*)
     nil))

(define-step show-on-card
  (tuatara.frontend.gtk::redraw *window*))

(defun take-picture ()
  "Push a picture of the window onto *PICTURES*, and true; NIL while it has
drawn nothing yet."
  (let ((picture (multiple-value-list (window-picture (tuatara.frontend.gtk::gtk-window *window*)))))
    (when (first picture)
      (push picture *pictures*))))

(define-step picture-on-card
  ;; then the same window painted whole
  (when (take-picture)
    (tuatara.frontend.gtk::drawn-on-card *window* nil)))

(define-step picture-painted-whole
  (take-picture))

(defun run-in-gtk (step)
  "Run STEP, a callback defined by DEFINE-STEP, in GTK's own thread, and wait
up to 30 seconds for it to finish; what it signalled is an error here."
  (let ((before *steps-done*))
    (tuatara.frontend.gtk::g-idle-add step (tuatara.io:null-pointer))
    (unless (wait-until (lambda () (> *steps-done* before)) 30)
      (error "GTK did not run ~a within 30 seconds" step))
    (let ((failed (getf *seen* :error)))
      (when failed (error "~a" failed)))))

(defun picture-in-gtk (step)
  "Run STEP, a callback defined by DEFINE-STEP that takes a picture, in GTK's
own thread until it has taken one, for up to 30 seconds."
  (let ((before (length *pictures*)))
    (unless (wait-until (lambda () (run-in-gtk step) (> (length *pictures*) before)) 30 :interval 1/10)
      (error "the window drew nothing within 30 seconds"))))

(defun steps ()
  "Wait for the window to show what was asked for, take its two pictures and
note what they hold and how they differ."
  (let ((how (third (uiop:command-line-arguments))))
    (setf *window* (or (first-window-shown 30) (error "no window showed within 30 seconds")))
    ;; a picture: once it shows, in as many windows as asked for; else the file's text
    (cond ((equal how "floated")
           (unless (wait-until (lambda () (= 2 (shown-pictures *window*))) 60)
             (error "the two pictures did not show within 60 seconds")))
          ((member how '("picture" "pictures-beside") :test #'equal)
           (unless (wait-until (lambda () (plusp (shown-pictures *window*))) 60)
             (error "the picture did not show within 60 seconds"))
           (when (equal how "pictures-beside")
             (type-keys *window* "C-x 3")
             (unless (wait-until (lambda () (= 2 (shown-pictures *window*))) 60)
               (error "the picture did not show twice within 60 seconds"))))
          ((not (wait-until (lambda () (search "lazy dog" (shown-text *window*))) 30))
           (error "the file's text did not show within 30 seconds")))
    ;; the file's text laid out and coloured, then each step once GTK has laid out the one before
    (sleep 2)
    (run-in-gtk (tuatara.gtk:callback show-on-card))
    (sleep 1)
    (picture-in-gtk (tuatara.gtk:callback picture-on-card))
    (sleep 1)
    (picture-in-gtk (tuatara.gtk:callback picture-painted-whole))
    (destructuring-bind ((whole textures-whole width) (on-card textures &rest more)) *pictures*
      (declare (ignore textures-whole more))
      (when (equal how "floated")
        (let ((red (colour-box whole width 0 0 255))
              (cell (multiple-value-list (tuatara.frontend:frontend-cell-pixels *window*))))
          (see :red red)
          (see :blue (colour-box whole width 255 0 0))
          (see :cell cell)
          ;; the row beside the red picture's top, past it: text shows there, more than the ground's colour
          (when red
            (destructuring-bind (left top right bottom) red
              (declare (ignore left bottom))
              (see :beside-top (colours-in whole width (+ right (first cell)) top (+ right (* 6 (first cell)))
                                           (+ top (second cell) -1)))))))
      (let ((same-size (= (length on-card) (length whole))))
        (see :textures textures)
        (see :octets (length whole))
        (see :differing (if same-size (count nil (map 'list #'= on-card whole)) (length whole)))
        (see :differing-much (if same-size
                                 (count-if (lambda (d) (> (abs d) 16)) (map 'list #'- on-card whole))
                                 (length whole)))))))

;; given up on after ninety seconds, what was seen written all the same
(run-steps #'steps (second (uiop:command-line-arguments)) :give-up 90)

(tuatara.terminal:main (list "--gui" (first (uiop:command-line-arguments))))
