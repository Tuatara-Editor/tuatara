;;;; frontends/gtk/gtk.lisp --- Tuatara in a window of its own: GTK4, painted with Pango and Cairo.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; GTK runs on the main thread and the editor on a thread of its own. GTK's
;;;; callbacks put keys, resizes and the close button in a queue the editor
;;;; reads; each frame the editor finishes is handed to the main thread, which
;;;; paints it on one drawing area: the rows as the editor laid them out, in the
;;;; faces' colours, on a grid of the font's cells.

(defpackage #:tuatara.frontend.gtk
  (:use #:cl #:tuatara.gtk)
  (:import-from #:tuatara-user #:*gui-font* #:*gui-renderer* #:*gui-padding* #:*window-colours-from-desktop*)
  (:documentation "The GTK4 frontend: Tuatara in a window of its own, one drawing area painted with Pango and Cairo.")
  (:export #:gtk-frontend #:make-gtk-frontend #:run-in-window #:gdk-key #:paint-frame-to-png
           #:call-with-painted-frame #:pixel-colour #:queue-push #:queue-pop #:make-event-queue
           #:make-frame #:frame-rows #:colour-rgb))

(in-package #:tuatara.frontend.gtk)

(defvar *gui-font* nil
  "The font of Tuatara's window, as Pango reads it, \"Adwaita Mono 12\" for one;
NIL for the desktop's monospace font. Set it in the init file.")

(defvar *gui-padding* 8
  "The room, in pixels, between the edges of Tuatara's window and its text, in
the ground's colour: one number for all four sides, or a list: of two, the
top and bottom, then the right and left ((4 12) is 4 above and below, 12 at
the sides); of three, the top, the right and left, the bottom; of four, the
top, right, bottom and left, clockwise. Set it in the init file, or try values
with \\[eval-expression] (setf *gui-padding* 12), which the window follows at once.")

(defvar *window-colours-from-desktop* nil
  "True to paint text and ground the theme leaves without a colour (the
terminal and plain themes in a window) in the colours of the desktop's GTK
theme, following its light or dark style as it changes; NIL, as standard, for
black text on a white ground, as a window with no theme shows it. Themes that
give their own colours (ao-dark, ao-light) are not changed. Set it in the init
file.")

(defvar *gui-renderer* :gl
  "How Tuatara's window is drawn: :gl, by the graphics card through OpenGL, as
standard; :vulkan, by the graphics card through Vulkan; :cairo, by the
processor alone; NIL, as GTK chooses. Which is faster depends on the machine:
\\[check-health], in the window, measures both and says which to use. Set it in the
init file: when it asks for another way than the window opened with, Tuatara
starts again at once, drawn that way; set later, from the next start.
GSK_RENDERER in the environment, GTK's own name for it (gl, vulkan, cairo),
goes before it.")

(defvar *window-renderer* nil
  "The way the window was opened to be drawn, as GSK_RENDERER names it (gl,
vulkan, cairo), NIL for GTK's own choice, :ENVIRONMENT when GSK_RENDERER was
set before Tuatara started.")

(defun renderer-name (renderer)
  "GTK's name (GSK_RENDERER) for RENDERER, a value of *GUI-RENDERER*; NIL for GTK's own choice."
  (case renderer (:gl "gl") (:vulkan "vulkan") (:cairo "cairo") (t nil)))

(defun window-renderer-name (window)
  "The renderer drawing WINDOW, realised, as GTK names its type (GskGLRenderer,
GskVulkanRenderer, GskCairoRenderer), or NIL when it has none."
  (let ((renderer (gtk-native-get-renderer window)))
    (unless (cffi:null-pointer-p renderer) (g-type-name-from-instance renderer))))

(defvar *desktop-colours* nil
  "The desktop's text and ground colours as GTK's theme last gave them, a list
of two lists of three numbers from 0 to 1, or NIL; kept by GTK's thread.")

;;; Events from GTK to the editor

(defstruct (event-queue (:constructor make-event-queue ()) (:copier nil))
  "Events on their way from GTK's thread to the editor's, oldest first."
  (items '()) (tail '())
  (lock (sb-thread:make-mutex :name "events"))
  (waiting (sb-thread:make-waitqueue :name "events")))

(setf (documentation 'make-event-queue 'function) "An empty EVENT-QUEUE."
      (documentation 'event-queue-p 'function) "True when OBJECT is an EVENT-QUEUE."
      (documentation 'event-queue-items 'function) "The events in QUEUE, oldest first."
      (documentation 'event-queue-tail 'function) "The last cons of QUEUE's items."
      (documentation 'event-queue-lock 'function) "The mutex guarding QUEUE."
      (documentation 'event-queue-waiting 'function) "Where the editor waits for QUEUE's next event.")

(defun tick-p (event)
  "True when EVENT is a :TICK, a wake with nothing done by you."
  (and (typep event 'tuatara.frontend:event) (eq (tuatara.frontend:event-kind event) :tick)))

(defun queue-push (queue event)
  "Add EVENT to QUEUE, waking the editor if it waits; a :TICK after another
waiting is one wake, not two."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (unless (and (tick-p event)
                 (event-queue-items queue)
                 (tick-p (first (event-queue-tail queue))))
      (let ((cell (list event)))
        (if (event-queue-items queue)
            (setf (cdr (event-queue-tail queue)) cell)
            (setf (event-queue-items queue) cell))
        (setf (event-queue-tail queue) cell)))
    (sb-thread:condition-notify (event-queue-waiting queue))))

(defun queue-empty-p (queue)
  "True when QUEUE holds no events."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (null (event-queue-items queue))))

(defun queue-pop (queue seconds)
  "The oldest event in QUEUE, waiting up to SECONDS for one (NIL: as long as it
takes); NIL when none came."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (tuatara.runtime:wait-on-until (event-queue-waiting queue) (event-queue-lock queue)
                                   (lambda () (event-queue-items queue)) seconds)
    (pop (event-queue-items queue))))

;;; Frames from the editor to GTK

(defstruct (frame (:constructor make-frame (rows)) (:copier nil))
  "What the window shows: ROWS, a vector with each row's (STRING FACE SPANS
SCALES OVER PARTS) or NIL, the cursor at CURSOR-ROW and CURSOR-COLUMN, CURSOR-SHAPE :block, :bar
or :underline, in CURSOR-COLOUR (as faces take colours; NIL for the text's),
and IMAGES, the pictures over the rows, each (IMAGE ROW COLUMN COLUMNS ROWS
OFFSET), as TUATARA.FRONTEND:FRONTEND-PUT-IMAGE takes them."
  (rows #() :type simple-vector)
  (cursor-row 0) (cursor-column 0) (cursor-shape :block) (cursor-colour nil)
  (images '()))

(setf (documentation 'make-frame 'function) "A FRAME showing ROWS, its cursor at the top left."
      (documentation 'frame-p 'function) "True when OBJECT is a FRAME."
      (documentation 'frame-rows 'function) "The rows of FRAME, each (STRING FACE SPANS SCALES OVER PARTS) or NIL."
      (documentation 'frame-cursor-row 'function) "The row FRAME's cursor is on."
      (documentation 'frame-cursor-column 'function) "The column FRAME's cursor is at."
      (documentation 'frame-cursor-shape 'function) "The shape of FRAME's cursor, :block, :bar or :underline."
      (documentation 'frame-cursor-colour 'function) "The colour of FRAME's cursor, as faces take colours, or NIL for the text's."
      (documentation 'frame-images 'function) "The pictures over FRAME's rows, each (IMAGE ROW COLUMN COLUMNS ROWS OFFSET).")

;;; The frontend

(defclass gtk-frontend ()
  ((window :initform nil :accessor gtk-window :documentation "The GTK window.")
   (area :initform nil :accessor gtk-area :documentation "The drawing area the window holds.")
   (im :initform nil :accessor gtk-im :documentation "The input method context keys pass through.")
   (fonts :initform nil :accessor gtk-fonts :documentation "The font descriptions: upright, bold, italic, bold italic.")
   (cell-width :initform 8 :accessor cell-width :documentation "The width of a cell in pixels.")
   (cell-height :initform 16 :accessor cell-height :documentation "The height of a cell in pixels.")
   (baseline :initform 12 :accessor cell-baseline :documentation "How far below a cell's top its text's baseline is, in pixels.")
   (columns :initform 80 :accessor gtk-columns :documentation "The width of the window in cells.")
   (rows :initform 24 :accessor gtk-rows :documentation "The height of the window in cells.")
   (lock :initform (sb-thread:make-mutex :name "window") :reader gtk-lock :documentation "Guards what both threads read.")
   (changed :initform (sb-thread:make-waitqueue :name "window") :reader gtk-changed :documentation "Where the editor waits for the window's size or the clipboard.")
   (sized :initform nil :accessor gtk-sized :documentation "True once the window knows its size.")
   (queue :initform (make-event-queue) :reader gtk-queue :documentation "Events for the editor.")
   (frame :initform nil :accessor gtk-frame :documentation "The frame the editor is painting.")
   (shown :initform nil :accessor gtk-shown :documentation "The frame the window shows.")
   (draw-pending :initform nil :accessor gtk-draw-pending :documentation "True while a redraw is asked for and not yet made.")
   (done :initform nil :accessor gtk-done :documentation "True once the editor has ended.")
   (focused :initform t :accessor gtk-focused :documentation "True while the window has the keyboard focus.")
   (clipboard-out :initform nil :accessor gtk-clipboard-out :documentation "Text on its way to the clipboard.")
   (clipboard-in :initform nil :accessor gtk-clipboard-in :documentation "The clipboard's text as last read, or :waiting.")
   (clipboard-kept :initform nil :accessor gtk-clipboard-kept
                   :documentation "The clipboard's text, read in the background each time it changes; NIL until it is watched.")
   (clipboard-reads :initform nil :accessor gtk-clipboard-reads
                    :documentation "How many reads of the changed clipboard are under way, or NIL while it is not watched.")
   (scroll :initform 0d0 :accessor gtk-scroll :documentation "Wheel movement not yet turned into whole turns.")
   (pointer :initform nil :accessor gtk-pointer :documentation "Where the pointer last was over the window, (X . Y) in pixels, or NIL.")
   (drag :initform nil :accessor gtk-drag :documentation "Where the first mouse button went down, (X . Y) in pixels, while it is held.")
   (drag-cell :initform nil :accessor gtk-drag-cell :documentation "The cell the editor was last told a drag reached, (COLUMN . ROW).")
   (drag-moved :initform nil :accessor gtk-drag-moved :documentation "True once a drag has reached another cell, so that its end is the button let go.")
   (drag-repeating :initform nil :accessor gtk-drag-repeating :documentation "True while a drag held past the text is told again and again, to scroll.")
   (title :initform nil :accessor gtk-title :documentation "The title the editor last gave the window.")
   (preedit :initform nil :accessor gtk-preedit :documentation "What the input method is composing, shown at the cursor until it is typed, or NIL.")
   (font-steps :initform 0 :accessor gtk-font-steps :documentation "The steps the font is scaled by from its size, each 1.2 times the last.")
   (pointer-shape :initform "text" :accessor gtk-pointer-shape :documentation "The name of the pointer's shape over the window now.")
   (toggles :initform '() :accessor gtk-toggles :documentation "The window states the editor asked to turn to or from, not yet done, oldest first.")
   (face-fonts :initform (make-hash-table :test 'equal) :reader gtk-face-fonts :documentation "The font descriptions of faces that name a font of their own (FACE-FONT), by name, at the window's size.")
   (layouts :initform nil :accessor gtk-layouts :documentation "The layouts painting keeps from one frame to the next (MAKE-LAYOUTS), or NIL; let go of when the fonts change.")
   (padding :initform '(0 0 0 0) :accessor gtk-padding :documentation "The room in pixels round the grid of cells, as the grid was last fitted: top, right, bottom and left.")
   (font-in-use :initform nil :accessor gtk-font-in-use :documentation "The name of the font the window's fonts were last made from.")
   (renderer-checked :initform nil :accessor gtk-renderer-checked :documentation "True once *GUI-RENDERER* has been looked at as the editor started (FOLLOW-RENDERER-SETTING).")
   (renderer-said :initform nil :accessor gtk-renderer-said :documentation "The way of drawing last said to be taken from the next start.")
   (measured :initform nil :accessor gtk-measured :documentation "While the drawing is measured (MEASURE-DRAWING): when each key went in, each frame was finished and each was painted, three vectors of microseconds (MEASURE-CLOCK).")
   (drawn-by :initform nil :accessor gtk-drawn-by :documentation "The renderer drawing the window, as GTK names its type (GskGLRenderer), once it is realised.")
   (settings-pending :initform nil :accessor gtk-settings-pending :documentation "True while the window is yet to take in changed settings.")
   (desktop-style :initform nil :accessor gtk-desktop-style :documentation "The desktop's style as GTK last told it, :dark or :light, or NIL.")
   (display-scale :initform 1 :accessor gtk-display-scale :documentation "How many of the display's own pixels each of the window's pixels takes across, as the window last painted: 2 on a screen scaled to 200 per cent.")
   (kept-rows :initform nil :accessor gtk-kept-rows :documentation "The rows as last painted, kept from one frame to the next (KEEP-ROWS), or NIL.")
   (grid :initform nil :accessor gtk-grid :documentation "The widget over the drawing area that holds the pictures of the rows' parts, drawn by the graphics card (PLACE-PIECES).")
   (pictures-layer :initform nil :accessor gtk-pictures-layer :documentation "The widget over the grid, under the cursor, that holds the frame's pictures (PLACE-IMAGES).")
   (shown-images :initform (make-hash-table :test 'eq) :reader gtk-shown-images :documentation "For each picture shown on the pictures layer, by its IMAGE: (PICTURE TEXTURE PLACE), PLACE how it was last placed.")
   (above :initform nil :accessor gtk-above :documentation "The widget over the grid that holds the cursor's own drawing area.")
   (cursor-area :initform nil :accessor gtk-cursor-area :documentation "The drawing area the cursor and what the input method composes are painted in, as large as they are.")
   (cursor-at :initform '(0 . 0) :accessor gtk-cursor-at :documentation "Where the cursor's drawing area stands on the grid, (X . Y) in pixels.")
   (ground :initform nil :accessor gtk-ground :documentation "The picture of the window's ground colour, stretched across it, under the rows.")
   (ground-colour :initform nil :accessor gtk-ground-colour :documentation "The colour GTK-GROUND shows, three numbers from 0 to 1, or NIL before it shows one.")
   (pictures :initform (make-array 0 :adjustable t :fill-pointer 0) :reader gtk-pictures :documentation "The pictures on the grid, each (PICTURE X Y TEXTURE WIDTH HEIGHT) as it was last placed (PLACE-PICTURE).")
   (on-card :initform nil :accessor gtk-on-card :documentation "True while the rows are shown as pictures the graphics card draws, NIL while the drawing area paints the whole frame.")
   (surfaces :initform (make-hash-table :test 'eq) :reader gtk-surfaces :documentation "The cairo surfaces of the pictures the window shows, by their IMAGE; used and let go of on the window's own thread."))
  (:documentation "Tuatara's window: a GTK4 window with one drawing area."))

(defun make-gtk-frontend ()
  "A frontend for a window of Tuatara's own."
  (make-instance 'gtk-frontend))

(defvar *frontend* nil
  "The frontend of the window open, for GTK's callbacks.")

(defun callback-failed (name condition)
  "Report on the error output that the window's callback NAME failed with CONDITION."
  (ignore-errors
   (format *error-output* "~&Tuatara: the window's ~(~a~) failed: ~a~%" name condition)
   (finish-output *error-output*)))

(defmacro define-callback (name-and-options return-type (&rest arguments) &body body)
  "Define a function for GTK to call, as CFFI:DEFCALLBACK does. NAME-AND-OPTIONS
is its name, or a list of its name and :FAILED FORM. An error in BODY is
reported on the error output and answered with the value of FORM (NIL by
default) rather than taking the editor down with GTK's thread."
  (destructuring-bind (name &key failed) (if (listp name-and-options) name-and-options (list name-and-options))
    (multiple-value-bind (forms declarations) (alexandria:parse-body body)
      `(cffi:defcallback ,name ,return-type ,arguments
         ,@declarations
         (handler-case (progn ,@forms)
           (serious-condition (condition)
             (callback-failed ',name condition)
             ,failed))))))

(defun request-draw (f)
  "Have the window painted again soon, from GTK's thread; any thread may ask."
  (unless (sb-thread:with-mutex ((gtk-lock f)) (shiftf (gtk-draw-pending f) t))
    (g-idle-add (cffi:callback on-draw-request) (cffi:null-pointer))))

(defmethod tuatara.frontend:frontend-start ((f gtk-frontend))
  ;; the window was opened on GTK's thread; wait for it to know its size
  (sb-thread:with-mutex ((gtk-lock f))
    (tuatara.runtime:wait-on-until (gtk-changed f) (gtk-lock f) (lambda () (gtk-sized f)) nil))
  ;; rows drawn large where a face asks for it (:height)
  (setf tuatara.display:*row-scaling* t))

(defmethod tuatara.frontend:frontend-stop ((f gtk-frontend))
  (setf tuatara.display:*row-scaling* nil
        (gtk-done f) t)
  (g-idle-add (cffi:callback on-close-window) (cffi:null-pointer)))

(defmethod tuatara.frontend:frontend-size ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f)) (values (gtk-columns f) (gtk-rows f))))

(defmethod tuatara.frontend:frontend-read-event ((f gtk-frontend))
  (queue-pop (gtk-queue f) nil))

(defmethod tuatara.frontend:frontend-read-event-timeout ((f gtk-frontend) seconds)
  (or (queue-pop (gtk-queue f) seconds) (tuatara.frontend:make-event :tick)))

(defmethod tuatara.frontend:frontend-wake ((f gtk-frontend))
  (queue-push (gtk-queue f) (tuatara.frontend:make-event :tick)))

(defmethod tuatara.frontend:frontend-input-pending-p ((f gtk-frontend))
  ;; what you did waits; the wakes of work in the background are no input
  (let ((queue (gtk-queue f)))
    (sb-thread:with-mutex ((event-queue-lock queue))
      (and (find-if-not #'tick-p (event-queue-items queue)) t))))

(defmethod tuatara.frontend:frontend-description ((f gtk-frontend))
  (format nil "in a GTK ~d.~d window"
          (cffi:foreign-funcall "gtk_get_major_version" :uint)
          (cffi:foreign-funcall "gtk_get_minor_version" :uint)))

(defmethod tuatara.frontend:frontend-health ((f gtk-frontend))
  (list (cons "Display" "a window of its own (GTK 4), 24-bit colour")
        (cons "Drawing" (format nil "~a~:[~;, as GSK_RENDERER in the environment says~]"
                                (or (gtk-drawn-by f) "not yet") (eq *window-renderer* :environment)))
        (cons "Clipboard" "the desktop's, through GTK")))

(defmethod tuatara.frontend:frontend-set-clipboard ((f gtk-frontend) text)
  (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-clipboard-out f) text))
  (g-idle-add (cffi:callback on-set-clipboard) (cffi:null-pointer))
  t)

(defmethod tuatara.frontend:frontend-font-steps ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f)) (gtk-font-steps f)))

(defmethod (setf tuatara.frontend:frontend-font-steps) (steps (f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-font-steps f) steps))
  (g-idle-add (cffi:callback on-rescale-font) (cffi:null-pointer))
  steps)

(defun call-with-font-families (function)
  "Call FUNCTION with each font family installed, a Pango font family."
  (load-gtk-libraries)
  (with-gtk-float-traps
    (cffi:with-foreign-objects ((families :pointer) (count :int))
      (pango-font-map-list-families (pango-cairo-font-map-get-default) families count)
      (let ((array (cffi:mem-ref families :pointer)))
        (unwind-protect
             (dotimes (i (cffi:mem-ref count :int))
               (funcall function (cffi:mem-aref array :pointer i)))
          (g-free array))))))

(defun font-families (&key monospace)
  "The names of the font families installed, sorted; with MONOSPACE, only those
whose characters all have one width."
  (let ((names '()))
    (call-with-font-families
     (lambda (family)
       (when (or (not monospace) (pango-font-family-is-monospace family))
         (push (pango-font-family-get-name family) names))))
    (sort names #'string-lessp)))

(defmethod tuatara.frontend:frontend-fonts ((f gtk-frontend))
  (font-families :monospace t))

(defun font-description-size (description)
  "The size of the Pango font DESCRIPTION, in Pango units; 11 points when it
names none."
  (let ((size (pango-font-description-get-size description)))
    (if (zerop size) (* 11 +pango-scale+) size)))

(defun font-size-in-use ()
  "The size of the window's font as it started, in Pango units: *GUI-FONT*'s, or
the desktop's monospace font's; 11 points when neither names one."
  (let ((description (pango-font-description-from-string (font-name))))
    (unwind-protect (font-description-size description)
      (pango-font-description-free description))))

(defun family-variants (family size)
  "The variants of the Pango font FAMILY drawn as themselves, as (NAME FONT)
with FONT the full name that chooses each at SIZE (Pango units), sorted by name."
  (cffi:with-foreign-objects ((faces :pointer) (count :int))
    (pango-font-family-list-faces family faces count)
    (let ((array (cffi:mem-ref faces :pointer)))
      (unwind-protect
           (sort (loop for i below (cffi:mem-ref count :int)
                       for face = (cffi:mem-aref array :pointer i)
                       unless (pango-font-face-is-synthesized face)
                         collect (let ((description (pango-font-face-describe face)))
                                   (unwind-protect
                                        (progn (pango-font-description-set-size description size)
                                               (list (pango-font-face-get-face-name face)
                                                     (pango-font-description-to-string description)))
                                     (pango-font-description-free description))))
                 #'string-lessp :key #'first)
        (g-free array)))))

(defmethod tuatara.frontend:frontend-font-faces ((f gtk-frontend))
  (let ((size (with-gtk-float-traps (load-gtk-libraries) (font-size-in-use))) (all '()))
    (call-with-font-families
     (lambda (family)
       (let ((variants (family-variants family size)))
         (when variants
           (push (list (pango-font-family-get-name family) (pango-font-family-is-monospace family) variants)
                 all)))))
    (sort all (lambda (a b)
                (if (eq (second a) (second b))
                    (string-lessp (first a) (first b))
                    (second a))))))

(defmethod tuatara.frontend:frontend-set-font ((f gtk-frontend) name)
  (with-gtk-float-traps
    (let* ((description (pango-font-description-from-string name))
           (family (pango-font-description-get-family description)))
      (unwind-protect
           (when (and family (member family (font-families) :test #'string-equal))
             (when (zerop (pango-font-description-get-size description))
               (pango-font-description-set-size description (font-size-in-use)))
             (let ((full (pango-font-description-to-string description)))
               (setf *gui-font* full)
               (g-idle-add (cffi:callback on-rescale-font) (cffi:null-pointer))
               full))
        (pango-font-description-free description)))))

(define-callback on-toggle-state :boolean ((data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*)
         (states (and f (sb-thread:with-mutex ((gtk-lock f)) (shiftf (gtk-toggles f) '()))))
         (window (and f (gtk-window f))))
    (when window
      (dolist (state states)
        (ecase state
          (:fullscreen (if (gtk-window-is-fullscreen window)
                           (gtk-window-unfullscreen window)
                           (gtk-window-fullscreen window)))
          (:maximised (if (gtk-window-is-maximized window)
                          (gtk-window-unmaximize window)
                          (gtk-window-maximize window)))))))
  nil)

(defmethod tuatara.frontend:frontend-toggle-state ((f gtk-frontend) state)
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-toggles f) (append (gtk-toggles f) (list state))))
  (g-idle-add (cffi:callback on-toggle-state) (cffi:null-pointer))
  t)

(defun settings-desktop-style ()
  "The desktop's style as GTK's settings have it (GTK 4.20 on), :dark or
:light; NIL when GTK cannot tell. On GTK's thread."
  (let ((settings (gtk-settings-get-default)))
    (when (and (not (cffi:null-pointer-p settings))
               (not (cffi:null-pointer-p
                     (cffi:foreign-funcall "g_object_class_find_property"
                                           :pointer (cffi:mem-ref settings :pointer)
                                           :string "gtk-interface-color-scheme" :pointer))))
      ;; 1: no preference, which desktops show light; 2: dark; 3: light
      (case (g-object-get-int settings "gtk-interface-color-scheme")
        (2 :dark)
        ((1 3) :light)
        (t nil)))))

(defun read-desktop-colours (widget)
  "Keep the desktop's text and ground colours for WIDGET in *DESKTOP-COLOURS*:
those the desktop's theme names for windows, else GTK's own. On GTK's thread."
  (let ((fg (widget-theme-colour widget "window_fg_color" "theme_fg_color"))
        (bg (widget-theme-colour widget "window_bg_color" "theme_bg_color")))
    (setf *desktop-colours* (and fg bg (list fg bg)))))

(define-callback on-style-changed :void ((settings :pointer) (spec :pointer) (data :pointer))
  (declare (ignore settings spec data))
  (let ((f *frontend*))
    (when f
      (let ((style (settings-desktop-style)))
        (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-desktop-style f) style))
        (when (gtk-area f)
          (read-desktop-colours (gtk-area f))
          (redraw f))
        (queue-push (gtk-queue f) (tuatara.frontend:make-event :tick))))))

(defmethod tuatara.frontend:frontend-settings-changed ((f gtk-frontend))
  (follow-renderer-setting f)
  ;; the font or the padding asked for not the ones in use: made again on GTK's thread
  (let ((font (font-name))
        (padding (multiple-value-list (tuatara.frontend:padding-sides *gui-padding*))))
    (when (sb-thread:with-mutex ((gtk-lock f))
            (and (not (gtk-settings-pending f))
                 (not (and (equal font (gtk-font-in-use f)) (equal padding (gtk-padding f))))
                 (setf (gtk-settings-pending f) t)))
      (g-idle-add (cffi:callback on-rescale-font) (cffi:null-pointer)))))

(defmethod tuatara.frontend:frontend-desktop-style ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f)) (gtk-desktop-style f)))

(defmethod tuatara.frontend:frontend-set-title ((f gtk-frontend) title)
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-title f) title))
  (g-idle-add (cffi:callback on-set-title) (cffi:null-pointer)))

(defmethod tuatara.frontend:frontend-clipboard ((f gtk-frontend))
  (read-clipboard-now f nil))

(defmethod tuatara.frontend:frontend-primary-selection ((f gtk-frontend))
  (read-clipboard-now f t))

(defun read-clipboard-now (f primary)
  "The text of F's clipboard, or with PRIMARY its primary selection: as kept
when it is watched (the clipboard), once a change being read is in, or, a
second gone, as it was before it; else read on GTK's thread, a second at
most; NIL when there is none."
  (sb-thread:with-mutex ((gtk-lock f))
    ;; watched: the text kept, once a change being read is in (a second at most)
    (when (and (not primary) (gtk-clipboard-reads f))
      (tuatara.runtime:wait-on-until (gtk-changed f) (gtk-lock f) (lambda () (zerop (gtk-clipboard-reads f))) 1)
      ;; or, the change still not in, the text kept before it
      (return-from read-clipboard-now (gtk-clipboard-kept f)))
    (setf (gtk-clipboard-in f) :waiting)
    (g-idle-add (if primary (cffi:callback on-read-primary) (cffi:callback on-read-clipboard)) (cffi:null-pointer))
    ;; a clipboard that does not answer within a second counts as none
    (tuatara.runtime:wait-on-until (gtk-changed f) (gtk-lock f) (lambda () (not (eq (gtk-clipboard-in f) :waiting))) 1)
    (let ((text (gtk-clipboard-in f)))
      (setf (gtk-clipboard-in f) nil)
      (and (stringp text) text))))

(defmethod tuatara.frontend:frontend-begin-frame ((f gtk-frontend))
  (setf (gtk-frame f) (make-frame (make-array (sb-thread:with-mutex ((gtk-lock f)) (gtk-rows f)) :initial-element nil))))

(defmethod tuatara.frontend:frontend-put-row ((f gtk-frontend) row string &key face spans scales over parts)
  (let ((rows (frame-rows (gtk-frame f))))
    (when (< -1 row (length rows))
      (setf (aref rows row) (list (copy-seq string) face (copy-list spans) (copy-list scales) (copy-tree over) (copy-list parts))))))

(defmethod tuatara.frontend:frontend-images-p ((f gtk-frontend))
  t)

(defmethod tuatara.frontend:frontend-cell-pixels ((f gtk-frontend))
  ;; in the display's own pixels: a picture made that large is drawn sharp on a scaled screen
  (sb-thread:with-mutex ((gtk-lock f))
    (let ((scale (gtk-display-scale f)))
      (values (* scale (cell-width f)) (* scale (cell-height f))))))

(defmethod tuatara.frontend:frontend-put-image ((f gtk-frontend) image row column columns rows &optional offset)
  (push (list image row column columns rows offset) (frame-images (gtk-frame f))))

(defmethod tuatara.frontend:frontend-set-cursor ((f gtk-frontend) row column shape &optional colour)
  (let ((frame (gtk-frame f)))
    (setf (frame-cursor-row frame) row (frame-cursor-column frame) column
          (frame-cursor-shape frame) shape (frame-cursor-colour frame) colour)))

(defmethod tuatara.frontend:frontend-end-frame ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-shown f) (gtk-frame f))
    (when (gtk-measured f) (vector-push-extend (measure-clock) (aref (gtk-measured f) 1))))
  (request-draw f))

;;; Keys

(defparameter *special-keyvals*
  '((#xff08 . :backspace) (#xff09 . :tab) (#xfe20 . :tab) (#xff0d . :enter) (#xff8d . :enter)
    (#xff1b . :escape) (#xffff . :delete) (#xff9f . :delete) (#xff63 . :insert) (#xff9e . :insert)
    (#xff50 . :home) (#xff95 . :home) (#xff57 . :end) (#xff9c . :end)
    (#xff51 . :left) (#xff96 . :left) (#xff52 . :up) (#xff97 . :up)
    (#xff53 . :right) (#xff98 . :right) (#xff54 . :down) (#xff99 . :down)
    (#xff55 . :page-up) (#xff9a . :page-up) (#xff56 . :page-down) (#xff9b . :page-down)
    (#xffbe . :f1) (#xffbf . :f2) (#xffc0 . :f3) (#xffc1 . :f4) (#xffc2 . :f5) (#xffc3 . :f6)
    (#xffc4 . :f7) (#xffc5 . :f8) (#xffc6 . :f9) (#xffc7 . :f10) (#xffc8 . :f11) (#xffc9 . :f12))
  "GDK's key values of the keys that type nothing, and the keys they are.")

(defun gdk-modifiers (state)
  "Three values, whether GDK's modifier STATE holds Control, Meta (Alt, or
the Meta key itself) and Shift."
  (values (logtest state 4) (logtest state (logior 8 (ash 1 28))) (logtest state 1)))

(defun gdk-key (keyval state &optional (unicode (and (< keyval #x1000000) (gdk-keyval-to-unicode keyval))))
  "The key GDK's KEYVAL with the modifiers of STATE stands for, or NIL for one
that is not a key of its own (a modifier pressed alone). UNICODE is the
character KEYVAL types, as GDK says."
  (multiple-value-bind (ctrl meta shift) (gdk-modifiers state)
    (let ((special (cdr (assoc keyval *special-keyvals*))))
      (cond (special
             (tuatara.frontend:make-key special :ctrl ctrl :meta meta :shift (or shift (= keyval #xfe20))))
            ((and unicode (<= 32 unicode) (/= unicode 127) (not (<= #xD800 unicode #xDFFF)))
             (tuatara.frontend:make-key (code-char unicode) :ctrl ctrl :meta meta))
            (t nil)))))

(defun push-key (f key)
  "Hand KEY to the editor."
  (queue-push (gtk-queue f) (tuatara.frontend:make-event :key :key key)))

(define-callback on-key-pressed :boolean ((controller :pointer) (keyval :uint) (keycode :uint) (state :uint) (data :pointer))
  (declare (ignore controller keycode data))
  (let ((f *frontend*) (key (gdk-key keyval state)))
    (when (and f key)
      (push-key f key)
      t)))

(defun command-key-p (keyval state)
  "True when the key GDK's KEYVAL with the modifiers of STATE is a command key,
one held with Control or Meta, handed to the editor at once, never to the
input method: but Control+Shift+U, which starts typing a character by its code."
  (multiple-value-bind (ctrl meta shift) (gdk-modifiers state)
    (and (or ctrl meta)
         (not (and ctrl shift (member keyval '(#x55 #x75)))))))

(define-callback on-command-key-pressed :boolean ((controller :pointer) (keyval :uint) (keycode :uint) (state :uint) (data :pointer))
  ;; before the input method: a command key gives up what it is composing (an accent
  ;; typed first, a dead key's) and runs as it always does
  (declare (ignore controller keycode data))
  (let ((f *frontend*) (key (and (command-key-p keyval state) (gdk-key keyval state))))
    (when (and f key)
      (when (gtk-preedit f)
        (gtk-im-context-reset (gtk-im f))
        (setf (gtk-preedit f) nil)
        (redraw f))
      (push-key f key)
      t)))

(define-callback on-commit :void ((context :pointer) (text :string) (data :pointer))
  (declare (ignore context data))
  (let ((f *frontend*))
    (when f
      (loop for c across text
            unless (< (char-code c) 32)
              do (push-key f (tuatara.frontend:make-key c))))))

(defun cell-at (f x y)
  "The cell of F's window at pixel X, Y: two values, its column and row, the
room round the grid (GTK-PADDING) left out. Over a part of a row drawn large
or moved down, that row's, and the column that stands for the character
drawn there (TUATARA.DISPLAY:LARGE-COLUMN)."
  (destructuring-bind (top right bottom left) (gtk-padding f)
    (declare (ignore right bottom))
    (let* ((cw (cell-width f)) (ch (cell-height f))
           (gx (/ (- x left) cw)) (gy (/ (- y top) ch))
           (frame (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f))))
      (when frame
        (loop for entry across (frame-rows frame)
              for row from 0
              do (loop for (start end scale shift height) in (fourth entry)
                       when (and (plusp scale) (<= start gx) (< gx end)
                                 (<= (+ row shift) gy) (< gy (+ row shift (or height scale))))
                         do (return-from cell-at
                              (values (+ start (tuatara.display:large-column (floor (- gx start) scale) scale)) row)))))
      (values (floor gx) (floor gy)))))

(defun modifier-key (base state column row)
  "A mouse key BASE with the modifiers of GDK's STATE, at cell COLUMN, ROW."
  (multiple-value-bind (ctrl meta shift) (gdk-modifiers state)
    (tuatara.frontend:make-key base :ctrl ctrl :meta meta :shift shift :column column :row row)))

(define-callback on-preedit-changed :void ((context :pointer) (data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when f
      (cffi:with-foreign-objects ((text :pointer) (attributes :pointer) (cursor :int))
        (setf (cffi:mem-ref text :pointer) (cffi:null-pointer)
              (cffi:mem-ref attributes :pointer) (cffi:null-pointer))
        (gtk-im-context-get-preedit-string context text attributes cursor)
        (let ((pointer (cffi:mem-ref text :pointer)) (list (cffi:mem-ref attributes :pointer)))
          (setf (gtk-preedit f)
                (unless (cffi:null-pointer-p pointer)
                  (let ((string (cffi:foreign-string-to-lisp pointer :encoding :utf-8)))
                    (and (plusp (length string)) (remove-if (lambda (c) (< (char-code c) 32)) string)))))
          (unless (cffi:null-pointer-p pointer) (g-free pointer))
          (unless (cffi:null-pointer-p list) (pango-attr-list-unref list))))
      (redraw f))))

(defun wheel-turns (f dy)
  "The turns of the wheel the scroll DY, in GDK's units, makes in F's window
with what was left over from the scrolls before: a whole number, below zero
for up; what is left of a turn is kept for the next. One scroll turns the
wheel a screenful of times at most, the rest of it dropped."
  (let* ((limit (max 1 (gtk-rows f)))
         (scroll (max (- limit) (min limit (+ (gtk-scroll f) dy))))
         (turns (truncate scroll)))
    (setf (gtk-scroll f) (- scroll turns))
    turns))

(define-callback on-scroll :boolean ((controller :pointer) (dx :double) (dy :double) (data :pointer))
  (declare (ignore dx data))
  (let ((f *frontend*))
    (when f
      (let* ((turns (wheel-turns f dy))
             (pointer (gtk-pointer f))
             (state (gtk-event-controller-get-current-event-state controller)))
        (multiple-value-bind (column row) (if pointer (cell-at f (car pointer) (cdr pointer)) (values nil nil))
          (loop repeat (abs turns)
                do (push-key f (modifier-key (if (plusp turns) :wheel-down :wheel-up) state column row)))))
      t)))

(defun pointer-shape-at (frame column row)
  "The name of the pointer's shape over cell COLUMN, ROW of FRAME: an arrow on a
mode line and on the rows below the windows, a sideways arrow on the divider
between windows side by side, the text beam elsewhere."
  (let* ((rows (and frame (frame-rows frame)))
         (entry (and rows (< -1 row (length rows)) (aref rows row))))
    (flet ((face-at (face)
             (or (and (eq (second entry) face) t)
                 (some (lambda (span) (and (eq (third span) face) (<= (first span) column) (< column (second span))))
                       (third entry)))))
      (cond ((null entry) "default")
            ((face-at :vertical-border) "col-resize")
            ((or (face-at :mode-line) (face-at :mode-line-inactive)) "default")
            ((= row (1- (length rows))) "default")
            (t "text")))))

(define-callback on-motion :void ((controller :pointer) (x :double) (y :double) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when (and f (gtk-area f))
      (setf (gtk-pointer f) (cons x y))
      ;; the pointer's shape follows what it is over, but not while a drag goes on
      (unless (gtk-drag f)
        (multiple-value-bind (column row) (cell-at f x y)
          (let ((shape (pointer-shape-at (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f)) column row)))
            (unless (string= shape (gtk-pointer-shape f))
              (setf (gtk-pointer-shape f) shape)
              (gtk-widget-set-cursor-from-name (gtk-area f) shape))))))))

(define-callback on-pointer-leave :void ((controller :pointer) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when f (setf (gtk-pointer f) nil))))

(define-callback on-pressed :void ((gesture :pointer) (presses :int) (x :double) (y :double) (data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when f
      (gtk-widget-grab-focus (gtk-area f))
      (gtk-im-context-reset (gtk-im f))
      (multiple-value-bind (column row) (cell-at f x y)
        (push-key f (modifier-key (case presses (1 :down-mouse-1) (2 :double-mouse-1) (t :triple-mouse-1))
                                  (gtk-event-controller-get-current-event-state gesture)
                                  column row))))))

(define-callback on-middle-pressed :void ((gesture :pointer) (presses :int) (x :double) (y :double) (data :pointer))
  (declare (ignore presses data))
  (let ((f *frontend*))
    (when f
      (multiple-value-bind (column row) (cell-at f x y)
        (push-key f (modifier-key :mouse-2 (gtk-event-controller-get-current-event-state gesture) column row))))))

(define-callback on-released :void ((gesture :pointer) (presses :int) (x :double) (y :double) (data :pointer))
  (declare (ignore presses data))
  (let ((f *frontend*))
    (when f
      (multiple-value-bind (column row) (cell-at f x y)
        (push-key f (modifier-key :up-mouse-1 (gtk-event-controller-get-current-event-state gesture) column row))))))

(defun drag-past-text-p (f row)
  "True when a drag at ROW is above F's window or on its last two rows, the
bottom mode line and the echo row, where holding it still keeps scrolling."
  (or (< row 0) (>= row (- (gtk-rows f) 2))))

(define-callback on-drag-repeat :boolean ((data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*) (cell (and f (gtk-drag f) (gtk-drag-cell f))))
    (cond ((and cell (drag-past-text-p f (cdr cell)))
           (when (queue-empty-p (gtk-queue f))
             (push-key f (tuatara.frontend:make-key :drag-mouse-1 :column (car cell) :row (cdr cell))))
           t)
          (t (when f (setf (gtk-drag-repeating f) nil))
             nil))))

(define-callback on-drag-begin :void ((gesture :pointer) (x :double) (y :double) (data :pointer))
  (declare (ignore gesture data))
  (let ((f *frontend*))
    (when f
      (setf (gtk-drag f) (cons x y)
            (gtk-drag-cell f) (multiple-value-call #'cons (cell-at f x y))))))

(define-callback on-drag-update :void ((gesture :pointer) (dx :double) (dy :double) (data :pointer))
  (declare (ignore gesture data))
  (let* ((f *frontend*) (start (and f (gtk-drag f))))
    (when start
      (multiple-value-bind (column row) (cell-at f (+ (car start) dx) (+ (cdr start) dy))
        (let ((cell (gtk-drag-cell f)))
          (unless (and cell (= column (car cell)) (= row (cdr cell)))
            (setf (gtk-drag-cell f) (cons column row) (gtk-drag-moved f) t)
            (push-key f (tuatara.frontend:make-key :drag-mouse-1 :column column :row row))))
        (when (and (drag-past-text-p f row) (not (gtk-drag-repeating f)))
          (setf (gtk-drag-repeating f) t)
          (g-timeout-add 50 (cffi:callback on-drag-repeat) (cffi:null-pointer)))))))

(define-callback on-drag-end :void ((gesture :pointer) (dx :double) (dy :double) (data :pointer))
  (declare (ignore gesture dx dy data))
  (let ((f *frontend*))
    (when f
      ;; a drag's end is the button let go too, which the click no longer reports
      (let ((cell (gtk-drag-cell f)))
        (when (and (gtk-drag f) cell (gtk-drag-moved f))
          (push-key f (tuatara.frontend:make-key :up-mouse-1 :column (car cell) :row (cdr cell)))))
      (setf (gtk-drag f) nil (gtk-drag-moved f) nil))))

(defun dropped-paths (value)
  "The local paths of the files in VALUE, a GValue holding a GdkFileList, as
the editor names files: a name's octets that are not UTF-8 kept as they are."
  (let* ((list (g-value-get-boxed value))
         (files (if (cffi:null-pointer-p list) (cffi:null-pointer) (gdk-file-list-get-files list)))
         (paths '()))
    (unwind-protect
         (do ((node files (cffi:mem-ref node :pointer (cffi:foreign-type-size :pointer))))
             ((cffi:null-pointer-p node))
           (let ((path (g-file-get-path (cffi:mem-ref node :pointer))))
             (unless (cffi:null-pointer-p path)
               (let* ((length (loop for i from 0 until (zerop (cffi:mem-aref path :uint8 i)) finally (return i)))
                      (octets (make-array length :element-type '(unsigned-byte 8))))
                 (dotimes (i length) (setf (aref octets i) (cffi:mem-aref path :uint8 i)))
                 (push (tuatara.text:octets-string octets) paths))
               (g-free path))))
      (unless (cffi:null-pointer-p files) (g-slist-free files)))
    (nreverse paths)))

(define-callback on-drop :boolean ((target :pointer) (value :pointer) (x :double) (y :double) (data :pointer))
  (declare (ignore target data))
  (let ((f *frontend*) (paths (dropped-paths value)))
    (when (and f paths)
      (multiple-value-bind (column row) (cell-at f x y)
        (queue-push (gtk-queue f) (tuatara.frontend:make-event :drop :files paths :column column :row row)))
      t)))

(define-callback on-focus-enter :void ((controller :pointer) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when f
      (setf (gtk-focused f) t)
      (gtk-im-context-focus-in (gtk-im f))
      (redraw f))))

(define-callback on-focus-leave :void ((controller :pointer) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when f
      (setf (gtk-focused f) nil)
      (gtk-im-context-focus-out (gtk-im f))
      (redraw f))))

(define-callback (on-close-request :failed t) :boolean ((window :pointer) (data :pointer))
  (declare (ignore window data))
  (let ((f *frontend*))
    (if (and f (not (gtk-done f)))
        (progn (queue-push (gtk-queue f) (tuatara.frontend:make-event :close)) t)
        nil)))

;;; Size

(defun fit-grid (f width height)
  "Fit F's grid of cells to its drawing area of WIDTH by HEIGHT pixels, inside
the room *GUI-PADDING* asks for, telling the editor when the number of rows or
columns changed."
  (destructuring-bind (top right bottom left)
      (setf (gtk-padding f) (multiple-value-list (tuatara.frontend:padding-sides *gui-padding*)))
    (let ((columns (max 1 (floor (- width left right) (cell-width f))))
          (rows (max 3 (floor (- height top bottom) (cell-height f)))))
      (sb-thread:with-mutex ((gtk-lock f))
        (let ((first (not (gtk-sized f))))
          (unless (and (not first) (= columns (gtk-columns f)) (= rows (gtk-rows f)))
            (setf (gtk-columns f) columns (gtk-rows f) rows (gtk-sized f) t)
            (sb-thread:condition-broadcast (gtk-changed f))
            (unless first
              (queue-push (gtk-queue f) (tuatara.frontend:make-event :resize :width columns :height rows)))))))))

(define-callback on-resize :void ((area :pointer) (width :int) (height :int) (data :pointer))
  (declare (ignore area data))
  (let ((f *frontend*))
    (when f (fit-grid f width height))))

(defun measure-font (f)
  "Set F's cell size from its fonts, laid out on a picture of one pixel."
  (let* ((probe (cairo-image-surface-create 0 1 1)) (probe-cr (cairo-create probe)))
    (unwind-protect (measure-cells f probe-cr)
      (cairo-destroy probe-cr)
      (cairo-surface-destroy probe))))

(define-callback on-rescale-font :boolean ((data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when (and f (gtk-area f))
      (let ((old (gtk-fonts f))
            (steps (sb-thread:with-mutex ((gtk-lock f))
                     (setf (gtk-settings-pending f) nil)
                     (gtk-font-steps f))))
        (setf (gtk-fonts f) (make-fonts (font-name) (expt 1.2d0 steps))
              (gtk-font-in-use f) (font-name))
        (loop for font across old do (pango-font-description-free font)))
      (forget-layouts f)
      (forget-face-fonts f)
      (measure-font f)
      (fit-grid f (gtk-widget-get-width (gtk-area f)) (gtk-widget-get-height (gtk-area f)))
      (redraw f)))
  nil)

;;; Main-loop work other threads ask for

(define-callback on-draw-request :boolean ((data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when (and f (gtk-area f))
      (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-draw-pending f) nil))
      (redraw f)))
  nil)

(define-callback on-close-window :boolean ((data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when (and f (gtk-window f))
      (gtk-window-destroy (shiftf (gtk-window f) nil))
      (setf (gtk-area f) nil)))
  nil)

(defun clipboard ()
  "The clipboard of the display GTK is connected to."
  (gdk-display-get-clipboard (gdk-display-get-default)))

(define-callback on-set-clipboard :boolean ((data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*)
         (text (and f (sb-thread:with-mutex ((gtk-lock f)) (shiftf (gtk-clipboard-out f) nil)))))
    (when text (gdk-clipboard-set-text (clipboard) (tuatara.display:displayable text :controls nil))))
  nil)

(defun shown-title (title)
  "TITLE as the window's title bar shows it: each octet of a name that is not
UTF-8 written \\xNN, as the mode line writes it, so that two such names never
look alike."
  (tuatara.display:displayable (tuatara.command:escape-controls title nil :raw-bytes) :controls nil))

(define-callback on-set-title :boolean ((data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*)
         (title (and f (sb-thread:with-mutex ((gtk-lock f)) (gtk-title f)))))
    (when (and title (gtk-window f))
      (gtk-window-set-title (gtk-window f) (shown-title title))))
  nil)

(define-callback on-clipboard-read :void ((source :pointer) (result :pointer) (data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*)
         (pointer (gdk-clipboard-read-text-finish source result (cffi:null-pointer)))
         (text (unless (cffi:null-pointer-p pointer)
                 (prog1 (cffi:foreign-string-to-lisp pointer) (g-free pointer)))))
    (when f
      (sb-thread:with-mutex ((gtk-lock f))
        (setf (gtk-clipboard-in f) (or text :none))
        (sb-thread:condition-broadcast (gtk-changed f))))))

(define-callback on-clipboard-kept :void ((source :pointer) (result :pointer) (data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*)
         (pointer (gdk-clipboard-read-text-finish source result (cffi:null-pointer)))
         (text (unless (cffi:null-pointer-p pointer)
                 (prog1 (cffi:foreign-string-to-lisp pointer) (g-free pointer)))))
    (when f
      (sb-thread:with-mutex ((gtk-lock f))
        (setf (gtk-clipboard-kept f) text
              (gtk-clipboard-reads f) (max 0 (1- (or (gtk-clipboard-reads f) 1))))
        (sb-thread:condition-broadcast (gtk-changed f))))))

(defun read-changed-clipboard (f)
  "Read the clipboard's text into F's keeping (GTK-CLIPBOARD-KEPT) in the
background, on GTK's thread, a paste meanwhile waiting for it."
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-clipboard-reads f) (1+ (or (gtk-clipboard-reads f) 0))))
  (gdk-clipboard-read-text-async (clipboard) (cffi:null-pointer) (cffi:callback on-clipboard-kept) (cffi:null-pointer)))

(define-callback on-clipboard-changed :void ((clipboard :pointer) (data :pointer))
  (declare (ignore clipboard data))
  (let ((f *frontend*))
    (when f (read-changed-clipboard f))))

(define-callback on-read-clipboard :boolean ((data :pointer))
  (declare (ignore data))
  (gdk-clipboard-read-text-async (clipboard) (cffi:null-pointer) (cffi:callback on-clipboard-read) (cffi:null-pointer))
  nil)

(define-callback on-read-primary :boolean ((data :pointer))
  (declare (ignore data))
  (gdk-clipboard-read-text-async (gdk-display-get-primary-clipboard (gdk-display-get-default))
                                 (cffi:null-pointer) (cffi:callback on-clipboard-read) (cffi:null-pointer))
  nil)

;;; Colours and fonts

(defun colour-rgb (colour)
  "COLOUR, a face's colour (a name such as :red, an index into the 256
colours, or \"#rrggbb\"), as a list of three numbers from 0 to 1; NIL when there
is none."
  (mapcar (lambda (c) (/ c 255d0)) (tuatara.display:colour-components colour)))

(defun blend (a b amount)
  "The colour AMOUNT of the way from A to B."
  (mapcar (lambda (x y) (+ x (* amount (- y x)))) a b))

(defun window-paper (part)
  "The colour of PART, :fg or :bg, where no face gives one: the desktop's with
*WINDOW-COLOURS-FROM-DESKTOP* when GTK's theme gives them, else black text on
a white ground; three numbers from 0 to 1."
  (let ((desktop (and *window-colours-from-desktop* *desktop-colours*)))
    (ecase part
      (:fg (if desktop (first desktop) '(0d0 0d0 0d0)))
      (:bg (if desktop (second desktop) '(1d0 1d0 1d0))))))

(defvar *face-colours* (tuatara.display:make-face-cache)
  "What FACE-COLOURS gives for each face spec, as a list, kept until a face or
the desktop's colours change.")

(defun face-colours (spec)
  "Five values for the face SPEC (a face name, a list of them, or NIL) as the
window paints it: the text's colour and the ground's, each three numbers from
0 to 1, its flags (:bold :italic :underline and the rest), the name of the
font it asks for, or NIL for the window's, and its underline's colour, or NIL
for the text's."
  (let ((table (tuatara.display:face-cache-table *face-colours*
                                                 (and *window-colours-from-desktop* *desktop-colours*))))
    (values-list
     (or (gethash spec table)
         (setf (gethash spec table)
               (multiple-value-bind (fg bg flags) (tuatara.display:resolve-face-spec spec)
                 (let ((fg (or (colour-rgb fg) (window-paper :fg)))
                       (bg (or (colour-rgb bg) (window-paper :bg))))
                   (when (member :reverse flags) (rotatef fg bg))
                   (when (member :dim flags) (setf fg (blend fg bg 0.45d0)))
                   (list fg bg flags (and spec (tuatara.display:resolve-face-font spec))
                         (and (member :underline flags)
                              (colour-rgb (tuatara.display:resolve-face-underline-colour spec)))))))))))

(defun draw-underline (f cr column columns y flags colour)
  "Underline COLUMNS cells from COLUMN on the row whose top is at Y, in
COLOUR (three numbers from 0 to 1, NIL for the colour in use): wavy with
:wave in FLAGS, dotted with :dotted, a box round the cells with :box, else a
straight line."
  (let* ((cw (cell-width f))
         (left (float (* column cw) 1d0))
         (right (float (* (+ column columns) cw) 1d0))
         (line (+ y (cell-baseline f) 1.5d0)))
    (when colour (set-colour cr colour))
    (cairo-set-line-width cr 1d0)
    (cond ((member :wave flags)
           ;; up and down every two pixels, a wave two pixels high
           (cairo-move-to cr left line)
           (loop for x from (+ left 2d0) by 2d0
                 for up = t then (not up)
                 while (<= x right)
                 do (cairo-line-to cr x (if up (- line 1d0) (+ line 1d0))))
           (cairo-stroke cr))
          ((member :box flags)
           (cairo-rectangle cr (+ left 0.5d0) (+ y 0.5d0) (- right left 1d0) (float (1- (cell-height f)) 1d0))
           (cairo-stroke cr))
          ((member :dotted flags)
           (loop for x from left by 3d0
                 while (< x right)
                 do (cairo-rectangle cr x (- line 0.5d0) 1d0 1d0))
           (cairo-fill cr))
          (t (cairo-move-to cr left line)
             (cairo-line-to cr right line)
             (cairo-stroke cr)))))

(defun set-colour (cr colour)
  "Draw on CR in COLOUR, three numbers from 0 to 1."
  (destructuring-bind (r g b) colour
    (cairo-set-source-rgb cr (float r 1d0) (float g 1d0) (float b 1d0))))

(defun font-name ()
  "The font the window is written in: *GUI-FONT*, or the desktop's monospace
font, or Monospace 11."
  (or *gui-font*
      (ignore-errors (g-settings-string "org.gnome.desktop.interface" "monospace-font-name"))
      "Monospace 11"))

(defun make-fonts (name &optional (scale 1))
  "The four font descriptions of the font NAME, its size times SCALE: upright,
bold, italic, bold italic. A NAME without a size is taken at 11 points."
  (let ((fonts (make-array 4)))
    (dotimes (i 4 fonts)
      (let ((description (pango-font-description-from-string name)))
        (when (or (/= scale 1) (zerop (pango-font-description-get-size description)))
          (pango-font-description-set-size
           description (max (* 2 +pango-scale+) (round (* scale (font-description-size description))))))
        (when (logtest i 1) (pango-font-description-set-weight description 700))
        (when (logtest i 2) (pango-font-description-set-style description 2))
        (setf (aref fonts i) description)))))

(defun measure-cells (f cr)
  "Set the cell size of F from its font as CR lays it out: the width and height of M."
  (let ((layout (pango-cairo-create-layout cr)))
    (unwind-protect
         (progn
           (pango-layout-set-font-description layout (aref (gtk-fonts f) 0))
           (pango-layout-set-text layout "M" -1)
           (multiple-value-bind (width height) (pango-layout-get-pixel-size layout)
             (sb-thread:with-mutex ((gtk-lock f))
               (setf (cell-width f) (max 1 width)
                     (cell-height f) (max 1 height)
                     (cell-baseline f) (round (pango-layout-get-baseline layout) +pango-scale+)))))
      (g-object-unref layout))))

;;; Painting

(defun font-index (flags)
  "Which of the four fonts FLAGS ask for."
  (+ (if (member :bold flags) 1 0) (if (member :italic flags) 2 0)))

(defun face-font-description (f font)
  "The description of the font named FONT (a face's own) at the size of F's
font, made once and kept."
  (or (gethash font (gtk-face-fonts f))
      (setf (gethash font (gtk-face-fonts f))
            (let ((description (pango-font-description-from-string font)))
              (pango-font-description-set-size description
                                               (pango-font-description-get-size (aref (gtk-fonts f) 0)))
              description))))

(defun forget-face-fonts (f)
  "Let go of F's descriptions of the fonts faces name, made at a size no longer the window's."
  (loop for description being the hash-values of (gtk-face-fonts f)
        do (pango-font-description-free description))
  (clrhash (gtk-face-fonts f)))

(defparameter *most-kept-layouts* 4096
  "The most layouts of the text drawn kept from one frame to the next, about:
once half of them have been made, those not drawn since the last time are
let go of, and made again when they are drawn again.")

(defun make-layouts ()
  "Room for the layouts painting makes as needed, kept from one frame to the
next: a table of the layouts of the text drawn, each set out once however
often it is drawn (KEPT-LAYOUT), the table of those of the frames before it
\(AGE-LAYOUTS), and a list of (SCALE . LAYOUTS) for the parts drawn SCALE times
as large, whose layouts Pango sets out for that scale (LAYOUTS-AT-SCALE)."
  (vector (make-hash-table :test 'equal) (make-hash-table :test 'equal) '()))

(defun free-layouts (layouts)
  "Let go of the layouts in LAYOUTS (MAKE-LAYOUTS), those of its scales too."
  (loop for i below 2
        do (loop for layout being the hash-values of (aref layouts i) do (g-object-unref layout)))
  (loop for (nil . own) in (aref layouts 2) do (free-layouts own)))

(defun age-layouts (layouts)
  "Before a frame: once LAYOUTS (MAKE-LAYOUTS) has made half of
*MOST-KEPT-LAYOUTS* since it last did this, let go of the layouts not drawn
since then and start again, the ones drawn since kept; for its scales too."
  (when (> (hash-table-count (aref layouts 0)) (floor *most-kept-layouts* 2))
    (loop for layout being the hash-values of (aref layouts 1) do (g-object-unref layout))
    (setf (aref layouts 1) (aref layouts 0)
          (aref layouts 0) (make-hash-table :test 'equal)))
  (loop for (nil . own) in (aref layouts 2) do (age-layouts own)))

(defun frame-layouts (f)
  "The layouts F paints a frame with (MAKE-LAYOUTS): those kept from the frames
before (AGE-LAYOUTS), made when there are none."
  (let ((layouts (or (gtk-layouts f) (setf (gtk-layouts f) (make-layouts)))))
    (age-layouts layouts)
    layouts))

(defun forget-layouts (f)
  "Let go of the layouts F keeps from one frame to the next, set out in fonts
no longer the window's, and of the rows painted with them (FORGET-KEPT-ROWS)."
  (forget-kept-rows f)
  (when (gtk-layouts f)
    (free-layouts (gtk-layouts f))
    (setf (gtk-layouts f) nil)))

(defun layouts-at-scale (layouts scale)
  "The layouts of LAYOUTS (MAKE-LAYOUTS) for text drawn SCALE times as large,
made when first asked, under that scale's transform: LAYOUTS itself for 1."
  (if (= scale 1)
      layouts
      (or (cdr (assoc scale (aref layouts 2)))
          (let ((own (make-layouts)))
            (push (cons scale own) (aref layouts 2))
            own))))

(defun font-layout (f cr font index)
  "A new layout on CR in the font named FONT (a face's own) when given, else
in F's font of INDEX (FONT-INDEX)."
  (let ((layout (pango-cairo-create-layout cr)))
    (pango-layout-set-font-description layout (if font (face-font-description f font) (aref (gtk-fonts f) index)))
    layout))

(defun kept-layout (f cr layouts text font index)
  "The layout of LAYOUTS (MAKE-LAYOUTS) holding TEXT in the font named FONT or
else F's font of INDEX: made and set out the first time it is asked for, the
same one after, from a frame before too (AGE-LAYOUTS), so that text drawn
again, as when a window scrolls, is not set out again. A layout keeps what
it was made for (the screen's scale, the font's settings): the layouts are
let go of when those change (FORGET-LAYOUTS)."
  (let ((key (list text font index)))
    (or (gethash key (aref layouts 0))
        (let ((old (gethash key (aref layouts 1))))
          (when old
            (remhash key (aref layouts 1))
            (setf (gethash key (aref layouts 0)) old)))
        (let ((layout (font-layout f cr font index)))
          (pango-layout-set-text layout text -1)
          (setf (gethash key (aref layouts 0)) layout)))))

(defun draw-text (f cr layouts text column y flags &optional font shape)
  "Draw TEXT from COLUMN on the row whose top is at Y, in the font FLAGS ask for,
or in FONT (a face's own, by name) when given, its baseline on the cells'.
LAYOUTS (MAKE-LAYOUTS) keeps the layout of each text drawn (KEPT-LAYOUT).
With SHAPE, the glyphs' outlines are filled as one shape rather than drawn
one by one, so that where glyphs overlap (the pieces of a ligature) no edge
is drawn twice."
  (let ((layout (kept-layout f cr layouts text font (font-index flags))))
    (cairo-move-to cr (float (* column (cell-width f)) 1d0)
                   (float (+ y (- (cell-baseline f) (round (pango-layout-get-baseline layout) +pango-scale+))) 1d0))
    (if shape
        (progn (pango-cairo-layout-path cr layout)
               (cairo-fill cr))
        (pango-cairo-show-layout cr layout))))

(defparameter *joining-characters* "-=_~<>|+*#:"
  "Characters a font may join into one stroke when two or more come together:
a line of dashes, an arrow, a double line. A run of signs with two of them
together is drawn as one filled shape (see DRAW-TEXT), so that the pieces meet
without the seams glyphs drawn one by one leave where they overlap.")

(defun joining-runs (text)
  "The runs of signs in TEXT (characters neither letters, digits nor spaces)
with two or more *JOINING-CHARACTERS* together in them, each (START . END), in
order: a line of dashes, an arrow, !== or <!-- whole, as the font joins them."
  (let ((runs '()) (i 0) (n (length text)))
    (flet ((sign-p (c) (and (graphic-char-p c) (not (alphanumericp c)) (char/= c #\Space)))
           (joining-p (c) (find c *joining-characters*)))
      (loop while (< i n)
            do (if (sign-p (char text i))
                   (let ((j (or (position-if-not #'sign-p text :start i) n)))
                     (when (loop for k from i below (1- j)
                                 thereis (and (joining-p (char text k)) (joining-p (char text (1+ k)))))
                       (push (cons i j) runs))
                     (setf i j))
                   (incf i))))
    (nreverse runs)))

(defun draw-ascii-text (f cr layouts text column y flags)
  "Draw TEXT, all ASCII, from COLUMN on the row whose top is at Y, as DRAW-TEXT
does, each run JOINING-RUNS finds in it as one shape."
  (let ((at 0))
    (dolist (run (joining-runs text))
      (when (< at (car run))
        (draw-text f cr layouts (subseq text at (car run)) (+ column at) y flags))
      (draw-text f cr layouts (subseq text (car run) (cdr run)) (+ column (car run)) y flags nil t)
      (setf at (cdr run)))
    (when (< at (length text))
      (draw-text f cr layouts (subseq text at) (+ column at) y flags))))

(defun paint-run (f cr layouts string start end column y spec &key each)
  "Paint the characters of STRING from START below END from COLUMN on the row
whose top is at Y, in the face SPEC; the column after them. Plain ASCII goes
in one piece; any other character on its own, at its column, so that a glyph
from another font or a wide one cannot push the rest out of the grid, a
letter with its accents, an emoji with its skin tone (a grapheme cluster,
TUATARA.TEXT:STRING-CLUSTER-END) together in the cells they take. With EACH,
every character, or cluster, on its own, at its column: drawn larger, a
font's own spacing would not keep to the grid, and a run's letters would
stand otherwise than the same letters split among colours."
  (multiple-value-bind (fg bg flags font underline) (face-colours spec)
    (let* ((columns (tuatara.display:string-columns string :start start :end end))
           (cw (cell-width f)) (ch (cell-height f)))
      (when spec
        (set-colour cr bg)
        (cairo-rectangle cr (float (* column cw) 1d0) (float y 1d0) (float (* columns cw) 1d0) (float ch 1d0))
        (cairo-fill cr))
      (set-colour cr fg)
      (flet ((alone-p (i)
               ;; an ASCII character no mark joins
               (and (< (char-code (char string i)) 128)
                    (= (tuatara.text:string-cluster-end string i end) (1+ i)))))
        (cond
          ((and font (not each))
           ;; a face's own font: the run in one piece, as that font spaces it
           (draw-text f cr layouts (subseq string start end) column y flags font))
          (t
           (let ((at column) (i start))
             (loop while (< i end)
                   do (if (and (not each) (char/= (char string i) #\Space) (alone-p i))
                          ;; a run of ASCII
                          (let ((j i))
                            (loop while (and (< j end) (alone-p j)) do (incf j))
                            (let ((text (string-right-trim " " (subseq string i j))))
                              (when (plusp (length text)) (draw-ascii-text f cr layouts text at y flags)))
                            (incf at (- j i))
                            (setf i j))
                          (let ((next (tuatara.text:string-cluster-end string i end)))
                            (unless (char= (char string i) #\Space)
                              (draw-text f cr layouts (subseq string i next) at y flags font))
                            (incf at (tuatara.display:string-columns string :start i :end next))
                            (setf i next))))))))
      (when (member :underline flags)
        (draw-underline f cr column columns y flags underline))
      (+ column columns))))

(defun char-index-at (string column)
  "The index of the character of STRING at COLUMN, the first of its grapheme
cluster (TUATARA.TEXT:STRING-CLUSTER-END), or its length when the characters
end before it. An ASCII character no mark follows takes a column of its own
and is passed at once."
  (let ((at 0) (i 0) (n (length string)))
    (loop while (< i n)
          do (when (>= at column) (return-from char-index-at i))
             (if (and (<= 32 (char-code (char string i)) 126)
                      (or (= (1+ i) n) (< (char-code (char string (1+ i))) #x300)))
                 (setf at (1+ at) i (1+ i))
                 (let ((next (tuatara.text:string-cluster-end string i)))
                   (incf at (tuatara.display:string-columns string :start i :end next))
                   (setf i next))))
    n))

(defun paint-row (f cr layouts row y width &key (pass :grid) (from 0) (to most-positive-fixnum))
  "Paint ROW, (STRING FACE SPANS SCALES OVER), on the row whose top is at Y,
WIDTH pixels wide, in three passes over all rows, as PASS says. :grid, the row
in its own cells but for the parts SCALES names, each (START END SCALE SHIFT
HEIGHT) in columns and rows, only its runs of characters that reach the columns
from FROM below TO; :large, those parts alone: the text from START on
drawn SCALE times as tall and wide, SHIFT rows lower, over the cells from
START below END of the HEIGHT rows it reaches (SCALE 0: nothing); :over, the
parts of OVER, each (LEFT TEXT SPANS), in the row's own cells over everything,
the first on top."
  (destructuring-bind (string face spans &optional scales over parts) row
    (declare (ignore parts))
    (let* ((string (tuatara.display:displayable string))
           (n (length string))
           (faces (make-array n :initial-element nil))
           (cw (cell-width f)) (ch (cell-height f)))
      (loop for (start end span-face) in spans
            do (loop with combined = (tuatara.display:combine-faces span-face face)
                     for i from (max 0 start) below (min n end)
                     do (setf (aref faces i) combined)))
      (labels ((paint-chars (string faces face start below column y &optional each (layouts layouts))
                 (let ((i start))
                   ;; no further than the columns asked for
                   (loop while (and (< i below) (< column to))
                         do (let* ((spec (or (aref faces i) face))
                                   ;; a mark or a joined emoji goes with the letter it is drawn on, whatever its own face
                                   (j (tuatara.text:string-cluster-end string i below)))
                              (loop while (and (< j below) (equal (or (aref faces j) face) spec))
                                    do (setf j (tuatara.text:string-cluster-end string j below)))
                              (setf column (if (or (>= column to)
                                                   (<= (+ column (tuatara.display:string-columns string :start i :end j)) from))
                                               (+ column (tuatara.display:string-columns string :start i :end j))
                                               (paint-run f cr layouts string i j column y spec :each each)))
                              (setf i j)))))
               (ground (face x y w h)
                 (multiple-value-bind (fg bg) (face-colours face)
                   (declare (ignore fg))
                   (set-colour cr bg)
                   (cairo-rectangle cr (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
                   (cairo-fill cr))))
        (ecase pass
          (:grid
           ;; the row's face covers the whole row, past the text too
           (when face (ground face 0 y width ch))
           (let ((i 0))
             (loop for (from below) in (append (sort (loop for (start end) in scales
                                                           collect (list (char-index-at string start) (char-index-at string end)))
                                                     #'< :key #'first)
                                               (list (list n n)))
                   do (when (< i from)
                        (paint-chars string faces face i from
                                     (tuatara.display:string-columns string :end i) y))
                      (setf i (max i below)))))
          (:large
           (loop for (start end scale shift height) in scales
                 when (plusp scale)
                   do (let ((top (+ y (* shift ch))) (band (or height scale)))
                        (cairo-save cr)
                        ;; grounds meeting between pixels, with no seam where they meet
                        (cairo-set-antialias cr 1)
                        (cairo-rectangle cr (float (* start cw) 1d0) (float top 1d0)
                                         (float (* (- end start) cw) 1d0) (float (* band ch) 1d0))
                        (cairo-clip cr)
                        (ground face (* start cw) top (* (- end start) cw) (* band ch))
                        (cairo-translate cr (float (* start cw) 1d0) (float top 1d0))
                        (cairo-scale cr (float scale 1d0) (float scale 1d0))
                        ;; scaled, each letter at its own cell, wherever the colours change, with
                        ;; the layouts of its scale, which Pango sets out for the scale in force when made
                        (paint-chars string faces face (char-index-at string start) (char-index-at string end) 0 0
                                     (/= scale 1) (layouts-at-scale layouts scale))
                        (cairo-restore cr))))
          (:over
           (dolist (part (reverse over))
             (destructuring-bind (left text text-spans) part
               (let* ((text (tuatara.display:displayable text))
                      (text-faces (make-array (length text) :initial-element nil)))
                 (loop for (start end span-face) in text-spans
                       do (loop for i from (max 0 start) below (min (length text) end)
                                do (setf (aref text-faces i) span-face)))
                 (ground nil (* left cw) y (* (tuatara.display:string-columns text) cw) ch)
                 (paint-chars text text-faces nil 0 (length text) left y))))))))))

(defun cursor-character (row column)
  "What ROW, (STRING FACE SPANS), shows at COLUMN, as two values: the
character there with the marks drawn on it (a grapheme cluster,
TUATARA.TEXT:STRING-CLUSTER-END) as a string, and the columns it takes; a
space and one column when nothing is there."
  (let ((string (and row (tuatara.display:displayable (first row)))) (at 0) (i 0))
    (when string
      (loop while (< i (length string))
            do (let ((next (tuatara.text:string-cluster-end string i)))
                 (when (= at column)
                   (return-from cursor-character
                     (values (subseq string i next)
                             (max 1 (tuatara.display:string-columns string :start i :end next)))))
                 (incf at (tuatara.display:string-columns string :start i :end next))
                 (when (> at column) (return))
                 (setf i next))))
    (values " " 1)))

(defun cursor-part (frame row column)
  "The part of FRAME's ROW drawn large or moved down that holds COLUMN, (START
END SCALE SHIFT HEIGHT), or NIL when the cell is drawn where it is."
  (let ((entry (and (< -1 row (length (frame-rows frame))) (aref (frame-rows frame) row))))
    (find-if (lambda (part) (and (plusp (third part)) (<= (first part) column) (< column (second part))))
             (fourth entry))))

(defun frame-cursor-pixels (f frame)
  "Where FRAME's cursor is drawn on F's grid, as four values: its x, y, width
and height in pixels, the room round the grid left out; on a part of its row
drawn large or moved down, the cell of that part."
  (let* ((row (frame-cursor-row frame)) (column (frame-cursor-column frame))
         (part (cursor-part frame row column))
         (cw (cell-width f)) (ch (cell-height f)))
    (if part
        (destructuring-bind (start end scale shift &optional height) part
          (declare (ignore end height))
          (let ((inside (floor (- column start) scale)))
            (values (* (+ start (* inside scale)) cw) (* (+ row shift) ch) (* scale cw) (* scale ch))))
        (values (* column cw) (* row ch) cw ch))))

(defun paint-preedit (f cr layouts frame text)
  "Paint TEXT, what the input method is composing, over the cells at FRAME's
cursor, underlined, on a part of its row drawn large or moved down where that
part's cells are; the column after it, in the window's own columns."
  (let* ((row (frame-cursor-row frame)) (column (frame-cursor-column frame))
         (part (cursor-part frame row column))
         (cw (cell-width f)) (ch (cell-height f))
         (columns (tuatara.display:string-columns text)))
    (cairo-save cr)
    (multiple-value-bind (cell top layouts scale)
        (if part
            (destructuring-bind (start end scale shift &optional height) part
              (declare (ignore end height))
              (cairo-translate cr (float (* start cw) 1d0) (float (* (+ row shift) ch) 1d0))
              (cairo-scale cr (float scale 1d0) (float scale 1d0))
              (values (floor (- column start) scale) 0 (layouts-at-scale layouts scale) scale))
            (values column (* row ch) layouts 1))
      (let ((x (float (* cell cw) 1d0)) (y (float top 1d0)))
        (multiple-value-bind (fg bg) (face-colours nil)
          (set-colour cr bg)
          (cairo-rectangle cr x y (float (* columns cw) 1d0) (float ch 1d0))
          (cairo-fill cr)
          (paint-run f cr layouts text 0 (length text) cell top nil :each (/= scale 1))
          (set-colour cr fg)
          (cairo-set-line-width cr 1d0)
          (let ((line (+ y (cell-baseline f) 1.5d0)))
            (cairo-move-to cr x line)
            (cairo-line-to cr (+ x (* columns cw)) line)
            (cairo-stroke cr))))
      (cairo-restore cr)
      (if part
          (+ (first part) (tuatara.display:large-column (+ cell columns) scale))
          (+ column columns)))))

(defun cursor-colours (colour)
  "The cursor's colours for COLOUR, as faces take colours, as two values, each
three numbers from 0 to 1: the character's under a block and the cursor's own;
that colour (TUATARA.DISPLAY:CURSOR-COLOUR) with the character in the
ground's, or in the :cursor face's text colour when the face gives the
cursor's; the :cursor face's when it gives none, the window's own."
  (let ((rgb (colour-rgb (tuatara.display:cursor-colour colour))))
    (if rgb
        (values (or (and (null colour) (tuatara.display:face-attribute :cursor :bg)
                         (colour-rgb (tuatara.display:face-attribute :cursor :fg)))
                    (nth-value 1 (face-colours :default)))
                rgb)
        (face-colours :cursor))))

(defun paint-cursor (f cr layouts frame focused &key (column (frame-cursor-column frame))
                                                     (shape (frame-cursor-shape frame)))
  "Paint FRAME's cursor, at COLUMN of its row, in its colour: a block with the
character under it, as wide as that character, a bar before the character or
a line under it, as SHAPE says; an outline when the window is not FOCUSED; none for :hidden. On a part
of the row drawn large or moved down, where its characters are and as large."
  (when (eq shape :hidden)
    (return-from paint-cursor nil))
  (let* ((row (frame-cursor-row frame))
         (entry (and (< -1 row (length (frame-rows frame))) (aref (frame-rows frame) row)))
         (part (cursor-part frame row column))
         (cw (cell-width f)) (ch (cell-height f)))
    (cairo-save cr)
    ;; on a part drawn large or moved down: drawn where its own cells are, its column the character's under it
    (multiple-value-bind (cell text-column top)
        (if part
            (destructuring-bind (start end scale shift &optional height) part
              (declare (ignore end height))
              (cairo-translate cr (float (* start cw) 1d0) (float (* (+ row shift) ch) 1d0))
              (cairo-scale cr (float scale 1d0) (float scale 1d0))
              (let ((inside (floor (- column start) scale)))
                (values inside (+ start inside) 0)))
            (values column column (* row ch)))
      (multiple-value-bind (text columns) (cursor-character entry text-column)
        (let ((x (float (* cell cw) 1d0)) (y (float top 1d0))
              (width (float (* columns cw) 1d0)))
          (multiple-value-bind (fg bg) (cursor-colours (frame-cursor-colour frame))
            (set-colour cr bg)
            (cond ((not focused)
                   (cairo-set-line-width cr 1d0)
                   (cairo-rectangle cr (+ x 0.5d0) (+ y 0.5d0) (- width 1d0) (float (- ch 1) 1d0))
                   (cairo-stroke cr))
                  ((eq shape :bar)
                   (cairo-rectangle cr x y 2d0 (float ch 1d0))
                   (cairo-fill cr))
                  ((eq shape :underline)
                   (cairo-rectangle cr x (float (- (+ y ch) 2) 1d0) width 2d0)
                   (cairo-fill cr))
                  (t
                   (cairo-rectangle cr x y width (float ch 1d0))
                   (cairo-fill cr)
                   (unless (string= text " ")
                     (set-colour cr fg)
                     ;; on a part, the layouts of its scale (see PAINT-ROW)
                     (draw-text f cr (if part (layouts-at-scale layouts (third part)) layouts) text cell top nil))))))))
    (cairo-restore cr)))

(defun image-surface (image)
  "A cairo surface holding IMAGE's pixels, its alpha applied as cairo keeps it:
copied by GDK from IMAGE's texture (IMAGE-TEXTURE), which writes them as
cairo does, 32 bits a pixel, blue first, each colour multiplied by the alpha."
  (let ((surface (cairo-image-surface-create 0 (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)))
        (texture (image-texture image)))
    (cairo-surface-flush surface)
    (gdk-texture-download texture (cairo-image-surface-get-data surface) (cairo-image-surface-get-stride surface))
    (g-object-unref texture)
    (cairo-surface-mark-dirty surface)
    surface))

(defun paint-images (f cr frame)
  "Paint FRAME's pictures on CR, each in its cells, as large as fits, its
proportions kept and never larger than it is, from the top left; one with an
offset at its own size, from that row and column of cells of it on. The
surfaces of pictures no longer shown are let go of."
  (let ((surfaces (gtk-surfaces f)) (shown '()))
    (loop for (image row column columns rows offset) in (frame-images frame)
          ;; no room: nothing painted (a scale of 0 would spoil all painted after on CR)
          when (and (plusp rows) (plusp columns))
          do (let* ((surface (or (gethash image surfaces) (setf (gethash image surfaces) (image-surface image))))
                    ;; the picture's own size is in the display's pixels: as large as that in the window's, no larger
                    (display (gtk-display-scale f))
                    (scale (if offset
                               (/ 1d0 display)
                               (tuatara.frontend:image-fit-scale image (* columns (cell-width f)) (* rows (cell-height f))
                                                                 (/ 1d0 display)))))
               (push image shown)
               (cairo-save cr)
               (cairo-translate cr (float (* column (cell-width f)) 1d0) (float (* row (cell-height f)) 1d0))
               (cairo-rectangle cr 0d0 0d0 (float (* columns (cell-width f)) 1d0) (float (* rows (cell-height f)) 1d0))
               (cairo-clip cr)
               (when offset
                 (cairo-translate cr (float (- (* (second offset) (cell-width f))) 1d0)
                                  (float (- (* (first offset) (cell-height f))) 1d0)))
               (cairo-scale cr (float scale 1d0) (float scale 1d0))
               (cairo-set-source-surface cr surface 0d0 0d0)
               (cairo-paint cr)
               (cairo-restore cr)))
    (loop for image being the hash-keys of surfaces using (hash-value surface)
          unless (member image shown)
            do (cairo-surface-destroy surface)
               (remhash image surfaces))))

(defmacro with-grid ((f cr width height grid-width) &body body)
  "Run BODY with CR moved inside the room round F's grid (GTK-PADDING) of a
window WIDTH by HEIGHT pixels and held to its whole cells, GRID-WIDTH bound to
their width in pixels."
  (let ((top (gensym "TOP")) (right (gensym "RIGHT")) (bottom (gensym "BOTTOM")) (left (gensym "LEFT"))
        (grid-height (gensym "GRID-HEIGHT")))
    `(destructuring-bind (,top ,right ,bottom ,left) (gtk-padding ,f)
       (let ((,grid-width (* (cell-width ,f) (max 1 (floor (- ,width ,left ,right) (cell-width ,f)))))
             (,grid-height (* (cell-height ,f) (max 1 (floor (- ,height ,top ,bottom) (cell-height ,f))))))
         (declare (ignorable ,grid-width))
         (cairo-save ,cr)
         (cairo-translate ,cr (float ,left 1d0) (float ,top 1d0))
         ;; nothing past the grid's whole cells: a row longer than the window stays out of the room round it
         (cairo-rectangle ,cr 0d0 0d0 (float ,grid-width 1d0) (float ,grid-height 1d0))
         (cairo-clip ,cr)
         (unwind-protect (progn ,@body)
           (cairo-restore ,cr))))))

(defstruct (kept-rows (:constructor make-kept-rows (key)) (:copier nil))
  "The rows of the window as last painted, kept from one frame to the next so
that only what changed is painted again (KEEP-ROWS): PIECES, for each row
down the window, its parts as painted, each (FROM BELOW STRIP SHOWS
TEXTURE): a picture in memory, STRIP, of the row's cells from column FROM below
BELOW, without what is drawn over the rows (the cursor, pictures, what the
input method composes), showing SHOWS (PIECE-CONTENT), NIL for a strip yet to
be painted, and TEXTURE, the strip as the graphics card is given it
\(STRIP-TEXTURE), or NIL until it is; KEY what they were painted for
\(ROWS-PAINTED-FOR)."
  key (pieces #()))

(defun forget-kept-rows (f)
  "Let go of the rows F keeps painted (KEEP-ROWS): the next frame is painted whole."
  (let ((kept (gtk-kept-rows f)))
    (when kept
      (loop for pieces across (kept-rows-pieces kept)
            do (loop for piece in pieces
                     do (cairo-surface-destroy (third piece))
                        (when (fifth piece) (g-object-unref (fifth piece)))))
      (setf (gtk-kept-rows f) nil))))

(defun rows-painted-for (f width count)
  "What F's rows are painted for, COUNT rows of a grid WIDTH pixels wide: when
any of it changes (the window's size, the faces or the theme, the desktop's
colours, the cells, the font, the screen's scale), the rows kept painted are
of no use."
  (list width count tuatara.display:*face-generation* (and *window-colours-from-desktop* *desktop-colours*)
        (cell-width f) (cell-height f) (gtk-font-in-use f) (gtk-display-scale f)))

(defun drawn-across-rows-p (row)
  "True when ROW, as a frame holds it, has parts drawn large or over others,
which reach into the rows round it."
  (and row (or (fourth row) (fifth row)) t))

(defparameter *most-row-shift* 16
  "The most rows a window's text is looked for moved by from one frame to the
next (ROW-SHIFT): moved further, as by a page, it is painted whole.")

(defun same-line-p (old new)
  "True when NEW and OLD, rows as frames hold them, show the same line, but
perhaps for its first columns (a line number counted from the cursor's): the
same from their end over three quarters of the longer at least."
  (and old new
       (let* ((a (first old)) (b (first new))
              (longer (max (length a) (length b)))
              (from-end (mismatch a b :from-end t)))
         (or (null from-end)
             (>= (- (length a) from-end) (* 3/4 longer))))))

(defun row-shift (old-rows new-rows)
  "How many rows further down than now the rows of the new frame, NEW-ROWS,
stood in the frame before, OLD-ROWS, as when a window scrolls: the shift, of
*MOST-ROW-SHIFT* at most either way, with which most rows show the same
lines as before (SAME-LINE-P); 0 when none does better than staying."
  (let ((best 0) (most -1) (n (length new-rows)))
    ;; the nearest shifts first: one that shows all the lines but two or fewer ends the search
    (loop for shift in (cons 0 (loop for k from 1 to *most-row-shift* collect k collect (- k)))
          do (let ((count (loop for i from (max 0 (- shift)) below (min n (- (length old-rows) shift))
                                count (same-line-p (aref old-rows (+ i shift)) (aref new-rows i)))))
               (when (> count most) (setf best shift most count))
               (when (>= count (- n (abs shift) 2)) (return))))
    best))

(defun paint-rows-whole (f cr layouts rows width height)
  "Paint on CR, WIDTH by HEIGHT pixels, the ground, all of it, then inside the
room round the grid (GTK-PADDING), no further than its whole cells, ROWS: each
in its cells, then what is drawn large or moved down, over the rows below it,
and what lies over all."
  (multiple-value-bind (fg bg) (face-colours nil)
    (declare (ignore fg))
    (set-colour cr bg)
    (cairo-rectangle cr 0d0 0d0 (float width 1d0) (float height 1d0))
    (cairo-fill cr))
  (with-grid (f cr width height grid-width)
    (loop for row across rows
          for y from 0 by (cell-height f)
          when row do (paint-row f cr layouts row y grid-width))
    (dolist (pass '(:large :over))
      (loop for row across rows
            for y from 0 by (cell-height f)
            when (and row (if (eq pass :large) (fourth row) (fifth row)))
              do (paint-row f cr layouts row y grid-width :pass pass)))))

(defun changed-columns (old new)
  "The columns where NEW, a row as frames hold it, shows otherwise than OLD, the
row painted there before, as two values, from and below: from its first
character changed to its last, or to its end when what follows moved to
other columns; NIL when the whole row is to be painted again (its face or
the faces of its characters changed, or there was none). A column more each
side, for a letter leaning into the next cell."
  (when (and old new (equal (second old) (second new)) (equal (third old) (third new)))
    (let* ((a (first old)) (b (first new))
           (start (mismatch a b)))
      (if (null start)
          (values 0 0)
          (let* ((end-old (max start (or (mismatch a b :from-end t) 0)))
                 (end-new (max start (or (mismatch b a :from-end t) 0)))
                 (old-columns (tuatara.display:string-columns a :end end-old))
                 (new-columns (tuatara.display:string-columns b :end end-new)))
            (values (max 0 (1- (tuatara.display:string-columns b :end (min start (length b)))))
                    ;; the rest as it was only where it stays in its columns; else to the end of the longer
                    (1+ (if (= old-columns new-columns)
                            new-columns
                            (max (tuatara.display:string-columns a) (tuatara.display:string-columns b))))))))))

(defun row-strip (f width)
  "A picture in memory of a row's cells, WIDTH pixels wide and a cell tall,
in the screen's own pixels."
  (let* ((scale (gtk-display-scale f))
         (strip (cairo-image-surface-create 1 (max 1 (* scale width)) (* scale (cell-height f)))))
    (cairo-surface-set-device-scale strip (float scale 1d0) (float scale 1d0))
    strip))

(defun row-bounds (row columns)
  "Where the parts of ROW, as frames hold rows, begin and end on a grid COLUMNS
wide: a list of (FROM . BELOW), its parts (the row's PARTS) in order."
  (let ((starts (cons 0 (remove-if-not (lambda (c) (< 0 c columns)) (and row (sixth row))))))
    (loop for (from below) on starts
          collect (cons from (or below columns)))))

(defun piece-content (row from below)
  "What ROW, as frames hold rows, shows in its columns from FROM below BELOW:
its characters there, the row's face and the faces of those characters,
counted from FROM, to be compared with what a strip shows."
  (when row
    (destructuring-bind (string face spans &rest more) row
      (declare (ignore more))
      (let ((start (char-index-at string from)) (end (char-index-at string below)))
        (list (subseq string start end) face
              (loop for (s e span-face) in spans
                    when (and (< s end) (> e start))
                      collect (list (- (max s start) start) (- (min e end) start) span-face)))))))

(defun paint-piece (f strip layouts shows row from below)
  "Paint on STRIP, the cells of a row from column FROM below BELOW, which shows
SHOWS (PIECE-CONTENT, or NIL for nothing yet), what ROW shows there: of a part
whose characters alone changed, the columns they are in (CHANGED-COLUMNS),
else all of it, its cells in the ground's colour first."
  (let ((cw (cell-width f)))
    (multiple-value-bind (left right) (changed-columns shows (piece-content row from below))
      (let ((left (+ from (or left 0)))
            (right (if left (min below (+ from right)) below))
            (cr (cairo-create strip)))
        (unwind-protect
             (when (< left right)
               (cairo-translate cr (float (- (* from cw)) 1d0) 0d0)
               (cairo-rectangle cr (float (* left cw) 1d0) 0d0 (float (* (- right left) cw) 1d0) (float (cell-height f) 1d0))
               (cairo-clip cr)
               (set-colour cr (nth-value 1 (face-colours nil)))
               (cairo-paint cr)
               (when row (paint-row f cr layouts row 0 (* below cw) :from left :to right)))
          (cairo-destroy cr))))))

(defun keep-rows (f width rows layouts)
  "The parts of ROWS, the rows of a frame, painted for F's window, whose grid
is WIDTH pixels wide (KEPT-ROWS), as a vector with each row's list of (FROM
BELOW STRIP SHOWS TEXTURE): the last part of each row, a window's text, taken from
the row that showed its line in the frame before, as the window scrolled
\(ROW-SHIFT), the others from the same row, as a gutter's numbers stay there;
only what changed in each painted again; all painted the first time, and when
what they are painted for changes (ROWS-PAINTED-FOR)."
  (let* ((columns (max 1 (floor width (cell-width f))))
         (key (rows-painted-for f width (length rows)))
         (kept (gtk-kept-rows f)))
    (unless (and kept (equal key (kept-rows-key kept)))
      (forget-kept-rows f)
      (setf kept (setf (gtk-kept-rows f) (make-kept-rows key))
            (kept-rows-pieces kept) (make-array (length rows) :initial-element '())))
    (let* ((old (kept-rows-pieces kept))
           (n (length rows))
           (cw (cell-width f))
           (bounds (map 'vector (lambda (row) (row-bounds row columns)) rows))
           ;; what each row's last part shows now and showed before, for the shift
           (lasts (map 'vector (lambda (row b) (let ((last (car (last b)))) (and row (piece-content row (car last) (cdr last)))))
                       rows bounds))
           (old-lasts (map 'vector (lambda (pieces) (fourth (car (last pieces)))) old))
           (shift (row-shift old-lasts lasts))
           (free '())
           (new (make-array n :initial-element '())))
      ;; first every piece is matched to the strip it can be painted over, the strips taken out of the old rows
      (let ((claimed (make-hash-table :test 'eq)))
        (dotimes (i n)
          (let ((b (aref bounds i)))
            (setf (aref new i)
                  (loop for (from . below) in b
                        for last = (eq (car (last b)) (assoc from b))
                        collect (let* ((j (if last (+ i shift) i))
                                       (pieces (and (< -1 j n) (aref old j)))
                                       (piece (find-if (lambda (p) (and (= (first p) from) (= (second p) below)
                                                                        (not (gethash (third p) claimed))))
                                                       pieces)))
                                  (if piece
                                      (progn (setf (gethash (third piece) claimed) t)
                                             (list from below (third piece) (fourth piece) (fifth piece)))
                                      (list from below nil nil nil)))))))
        ;; strips no row took: kept for the pieces that need one of their width
        (loop for pieces across old
              do (loop for piece in pieces
                       unless (gethash (third piece) claimed)
                         do (push piece free)))
        (dotimes (i n)
          (let ((row (aref rows i)))
            (dolist (piece (aref new i))
              (destructuring-bind (from below strip shows &optional texture) piece
                (declare (ignore texture))
                (unless strip
                  (let ((spare (find-if (lambda (p) (and (= (- (second p) (first p)) (- below from)))) free)))
                    (if spare
                        (progn (setf free (remove spare free) (third piece) (third spare))
                               (when (fifth spare) (g-object-unref (fifth spare))))
                        (setf (third piece) (row-strip f (* (- below from) cw))))
                    (setf shows nil (fourth piece) nil)))
                (let ((content (and row (piece-content row from below))))
                  (unless (and shows (equal content shows))
                    (paint-piece f (third piece) layouts shows row from below)
                    ;; its texture, of what it showed, made again when next shown (STRIP-TEXTURE)
                    (when (fifth piece) (g-object-unref (fifth piece)) (setf (fifth piece) nil))
                    (setf (fourth piece) content)))))))
        (dolist (piece free)
          (cairo-surface-destroy (third piece))
          (when (fifth piece) (g-object-unref (fifth piece)))))
      (setf (kept-rows-pieces kept) new)
      new)))

(defun strip-texture (f strip width)
  "A texture of STRIP, a part of a row WIDTH pixels wide (ROW-STRIP), for the
graphics card to draw: made once each time the strip is painted, and moved
from row to row as it is, never sent to the card again while it stands."
  (let* ((scale (gtk-display-scale f))
         (pixels-wide (max 1 (* scale width)))
         (pixels-high (* scale (cell-height f)))
         (stride (cairo-image-surface-get-stride strip)))
    (cairo-surface-flush strip)
    (let* ((bytes (g-bytes-new (cairo-image-surface-get-data strip) (* stride pixels-high)))
           (texture (gdk-memory-texture-new pixels-wide pixels-high 29 bytes stride)))
      (g-bytes-unref bytes)
      texture)))

(defun place-picture (f slot texture x y width height)
  "Show TEXTURE at X Y in the window, WIDTH by HEIGHT of its pixels, in the
picture SLOT of F's grid: made the first time, only what changed since given
again, so that a texture moved from one row to another costs nothing but its
place. The texture, in the screen's own pixels, fills the picture's size."
  (let ((pictures (gtk-pictures f)))
    (when (>= slot (length pictures))
      (let ((picture (gtk-picture-new)))
        (gtk-picture-set-content-fit picture 0)
        (gtk-widget-set-can-target picture nil)
        (gtk-fixed-put (gtk-grid f) picture 0d0 0d0)
        (vector-push-extend (list picture nil nil nil nil nil) pictures)))
    (let ((entry (aref pictures slot)))
      (destructuring-bind (picture at-x at-y shown at-width at-height) entry
        (unless (eq texture shown)
          (when (null shown) (gtk-widget-set-visible picture t))
          (gtk-picture-set-paintable picture texture))
        (unless (and (eql width at-width) (eql height at-height))
          (gtk-widget-set-size-request picture width height))
        (unless (and (eql x at-x) (eql y at-y))
          (let ((transform (gsk-transform-move x y)))
            (gtk-fixed-set-child-transform (gtk-grid f) picture transform)
            (gsk-transform-unref transform)))
        (setf (aref pictures slot) (list picture x y texture width height))))))

(defun hide-pictures-from (f slot)
  "Hide the pictures of F's grid from SLOT on, which the frame does not use."
  (let ((pictures (gtk-pictures f)))
    (loop for i from slot below (length pictures)
          for entry = (aref pictures i)
          when (fourth entry)
            do (gtk-picture-set-paintable (first entry) (cffi:null-pointer))
               (gtk-widget-set-visible (first entry) nil)
               (setf (fourth entry) nil))))

(defun show-ground (f width height)
  "Show the window's ground colour across it, WIDTH by HEIGHT pixels, under
the rows: a picture of one pixel, stretched to that size."
  (let ((colour (nth-value 1 (face-colours nil))) (ground (gtk-ground f)))
    (unless (equal colour (gtk-ground-colour f))
      (cffi:with-foreign-object (pixel :uint8 4)
        (destructuring-bind (r g b) colour
          (setf (cffi:mem-aref pixel :uint8 0) (round (* 255 b))
                (cffi:mem-aref pixel :uint8 1) (round (* 255 g))
                (cffi:mem-aref pixel :uint8 2) (round (* 255 r))
                (cffi:mem-aref pixel :uint8 3) 255))
        (let* ((bytes (g-bytes-new pixel 4))
               (texture (gdk-memory-texture-new 1 1 29 bytes 4)))
          (g-bytes-unref bytes)
          (gtk-picture-set-paintable ground texture)
          (g-object-unref texture)))
      (setf (gtk-ground-colour f) colour))
    (gtk-widget-set-size-request ground width height)))

(defun image-texture (image)
  "A texture of IMAGE's pixels as they are, for the graphics card to draw:
red, green and blue, with alpha when it has four channels."
  (let* ((width (tuatara.frontend:image-width image))
         (channels (tuatara.frontend:image-channels image))
         (pixels (tuatara.frontend:image-pixels image))
         (stride (* width channels)))
    (cffi:with-pointer-to-vector-data (data pixels)
      (let* ((bytes (g-bytes-new data (length pixels)))
             ;; 5: R8G8B8A8, 7: R8G8B8 (GdkMemoryFormat)
             (texture (gdk-memory-texture-new width (tuatara.frontend:image-height image)
                                              (if (= channels 4) 5 7) bytes stride)))
        (g-bytes-unref bytes)
        texture))))

(defun place-images (f frame left top)
  "Show FRAME's pictures on F's pictures layer as the graphics card draws them,
the grid's top left at LEFT TOP: each in the cells it takes, as large as
fits, its proportions kept and never larger than it is, from their top left,
as PAINT-IMAGES paints them. A picture is given to the card once while it is
shown; those no longer shown are let go of. Pictures shown from an offset,
larger than their cells, are painted whole (REDRAW)."
  (let ((layer (gtk-pictures-layer f)) (shown (gtk-shown-images f))
        (cw (cell-width f)) (ch (cell-height f)) (display (gtk-display-scale f))
        (now '()))
    (loop for (image row column columns rows) in (frame-images frame)
          when (and (plusp rows) (plusp columns))
            do (push image now)
               (let ((entry (or (gethash image shown)
                                (let ((picture (gtk-picture-new)) (texture (image-texture image)))
                                  (gtk-widget-set-can-target picture nil)
                                  (gtk-picture-set-content-fit picture 0)
                                  (gtk-picture-set-paintable picture texture)
                                  (gtk-fixed-put layer picture 0d0 0d0)
                                  (setf (gethash image shown) (list picture texture nil))))))
                 (destructuring-bind (picture texture place) entry
                   (declare (ignore texture))
                   (let* ((scale (tuatara.frontend:image-fit-scale image (* columns cw) (* rows ch) (/ 1d0 display)))
                          (wanted (list row column columns rows scale left top cw ch)))
                     (unless (equal wanted place)
                       (gtk-widget-set-size-request picture
                                                    (max 1 (round (* scale (tuatara.frontend:image-width image))))
                                                    (max 1 (round (* scale (tuatara.frontend:image-height image)))))
                       (let ((at (gsk-transform-move (+ left (* column cw)) (+ top (* row ch)))))
                         (gtk-fixed-set-child-transform layer picture at)
                         (gsk-transform-unref at))
                       (setf (third entry) wanted))))))
    (loop for image being the hash-keys of shown using (hash-value entry)
          unless (member image now)
            do (gtk-fixed-remove layer (first entry))
               (g-object-unref (second entry))
               (remhash image shown))))

(defun place-cursor (f frame left top)
  "Put F's cursor's drawing area where FRAME's cursor is, as large as the
cursor and what the input method is composing, the grid's top left at LEFT
TOP, and have it painted again; tell the input method where it is."
  (multiple-value-bind (x y w h) (frame-cursor-pixels f frame)
    (let* ((preedit (gtk-preedit f))
           (w (if preedit (max w (+ 2 (* (cell-width f) (tuatara.display:string-columns preedit)))) w))
           (area (gtk-cursor-area f)))
      (setf (gtk-cursor-at f) (cons x y))
      (gtk-widget-set-size-request area (ceiling w) (ceiling h))
      (let ((transform (gsk-transform-move (+ left x) (+ top y))))
        (gtk-fixed-set-child-transform (gtk-above f) area transform)
        (gsk-transform-unref transform))
      (gtk-widget-queue-draw area)
      (gtk-im-context-set-cursor-location (gtk-im f) (round (+ left x)) (round (+ top y)) (round w) (round h)))))

(define-callback on-draw-cursor :void ((area :pointer) (cr :pointer) (width :int) (height :int) (data :pointer))
  (declare (ignore area width height data))
  (let* ((f *frontend*)
         (frame (and f (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f)))))
    (when frame
      (let ((layouts (frame-layouts f)) (at (gtk-cursor-at f)))
        (cairo-translate cr (float (- (car at)) 1d0) (float (- (cdr at)) 1d0))
        (let ((preedit (gtk-preedit f)))
          (if preedit
              (paint-cursor f cr layouts frame (gtk-focused f)
                            :column (paint-preedit f cr layouts frame preedit) :shape :bar)
              (paint-cursor f cr layouts frame (gtk-focused f))))))))

(defun place-pieces (f frame)
  "Show FRAME as pictures the graphics card draws: the ground, each part of
each row as kept painted (KEEP-ROWS) and given to the card once
\(STRIP-TEXTURE), the frame's own pictures over them (PLACE-IMAGES), the
cursor over those."
  (let* ((area (gtk-area f))
         (width (gtk-widget-get-width area))
         (height (gtk-widget-get-height area))
         (scale (max 1 (gtk-widget-get-scale-factor area))))
    ;; layouts are set out for the scale of the screen they were made on
    (unless (= scale (gtk-display-scale f))
      (forget-layouts f)
      (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-display-scale f) scale)))
    (when (and *window-colours-from-desktop* (null *desktop-colours*))
      (read-desktop-colours area))
    (destructuring-bind (top right bottom left) (gtk-padding f)
      (declare (ignore bottom))
      (let* ((cw (cell-width f)) (ch (cell-height f))
             (grid-width (* cw (max 1 (floor (- width left right) cw))))
             (slot 0))
        (show-ground f width height)
        (loop for row-pieces across (keep-rows f grid-width (frame-rows frame) (frame-layouts f))
              for y from top by ch
              do (dolist (piece row-pieces)
                   (destructuring-bind (from below strip shows texture) piece
                     (declare (ignore shows))
                     (place-picture f slot (or texture (setf (fifth piece) (strip-texture f strip (* (- below from) cw))))
                                    (+ left (* from cw)) y (* (- below from) cw) ch)
                     (incf slot))))
        (hide-pictures-from f slot)
        (place-images f frame left top)
        (place-cursor f frame left top)))))

(defun let-go-of-pictures (f on-card)
  "Let go of the pictures F keeps for the way of drawing it no longer uses:
drawn by the graphics card when ON-CARD, the cairo surfaces painting whole
uses (PAINT-IMAGES); else the pictures and textures on the card's layer
\(PLACE-IMAGES)."
  (if on-card
      (let ((surfaces (gtk-surfaces f)))
        (loop for surface being the hash-values of surfaces do (cairo-surface-destroy surface))
        (clrhash surfaces))
      (let ((shown (gtk-shown-images f)))
        (loop for entry being the hash-values of shown
              do (gtk-fixed-remove (gtk-pictures-layer f) (first entry))
                 (g-object-unref (second entry)))
        (clrhash shown))))

(defun drawn-on-card (f on)
  "Show F's rows as pictures the graphics card draws (PLACE-PIECES) when ON,
else painted whole by the drawing area (PAINT-FRAME), as a frame with rows
drawn large or a picture shown from an offset is; the pictures kept for the
other way are let go of (LET-GO-OF-PICTURES)."
  (unless (eq on (gtk-on-card f))
    (setf (gtk-on-card f) on)
    (let-go-of-pictures f on)
    (gtk-drawing-area-set-draw-func (gtk-area f) (if on (cffi:null-pointer) (cffi:callback on-draw))
                                    (cffi:null-pointer) (cffi:null-pointer))
    (gtk-widget-set-visible (gtk-grid f) on)
    (gtk-widget-set-visible (gtk-pictures-layer f) on)
    (gtk-widget-set-visible (gtk-above f) on)))

(defun redraw (f)
  "Show F's window again as the frame it was last given is, on GTK's thread:
its rows and pictures as the graphics card draws them, or, for a frame with
rows drawn large, which reach into the rows round them, or a picture shown
from an offset, cut to its cells, painted whole by the drawing area."
  (when (gtk-area f)
    (let ((frame (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f))))
      (if (and frame (gtk-grid f)
               (notany #'drawn-across-rows-p (frame-rows frame))
               ;; a picture shown from an offset is cut to its cells: painted whole
               (notany #'sixth (frame-images frame)))
          (progn
            (drawn-on-card f t)
            (handler-case (place-pieces f frame)
              (error (e) (format *error-output* "~&Tuatara could not show the rows: ~a~%" e))))
          (progn
            (drawn-on-card f nil)
            (gtk-widget-queue-draw (gtk-area f)))))))

(defun paint-frame (f cr width height frame focused)
  "Paint FRAME on CR, WIDTH by HEIGHT pixels: the ground, all of it, then inside
the room round the grid (GTK-PADDING), and no further than its whole cells,
every row (as kept painted from the frame before, KEEP-ROWS, unless one is
drawn large or over others, which reaches into the rows round it), the
pictures, what the input method is composing, the cursor."
  (let ((layouts (frame-layouts f)))
    (if (null frame)
        (multiple-value-bind (fg bg) (face-colours nil)
          (declare (ignore fg))
          (set-colour cr bg)
          (cairo-rectangle cr 0d0 0d0 (float width 1d0) (float height 1d0))
          (cairo-fill cr))
        (let ((rows (frame-rows frame)))
          (if (some #'drawn-across-rows-p rows)
              (progn (forget-kept-rows f)
                     (paint-rows-whole f cr layouts rows width height))
              (progn
                (multiple-value-bind (fg bg) (face-colours nil)
                  (declare (ignore fg))
                  (set-colour cr bg)
                  (cairo-rectangle cr 0d0 0d0 (float width 1d0) (float height 1d0))
                  (cairo-fill cr))
                (with-grid (f cr width height grid-width)
                  (loop with cw = (cell-width f) and ch = (cell-height f)
                        for pieces across (keep-rows f grid-width rows layouts)
                        for y from 0 by ch
                        do (loop for (from below strip) in pieces
                                 do (cairo-set-source-surface cr strip (float (* from cw) 1d0) (float y 1d0))
                                    (cairo-rectangle cr (float (* from cw) 1d0) (float y 1d0)
                                                     (float (* (- below from) cw) 1d0) (float ch 1d0))
                                    (cairo-fill cr))))))
          (with-grid (f cr width height grid-width)
            (paint-images f cr frame)
            (let ((preedit (gtk-preedit f)))
              (if preedit
                  (paint-cursor f cr layouts frame focused
                                :column (paint-preedit f cr layouts frame preedit) :shape :bar)
                  (paint-cursor f cr layouts frame focused))))))))

(define-callback on-draw :void ((area :pointer) (cr :pointer) (width :int) (height :int) (data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when f
      ;; layouts are set out for the scale of the screen they were made on
      (let ((scale (max 1 (gtk-widget-get-scale-factor area))))
        (unless (= scale (gtk-display-scale f))
          (forget-layouts f)
          (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-display-scale f) scale))))
      (when (and *window-colours-from-desktop* (null *desktop-colours*))
        (read-desktop-colours area))
      (let ((frame (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f))))
        (handler-case (paint-frame f cr width height frame (gtk-focused f))
          (error (e) (format *error-output* "~&Tuatara could not paint: ~a~%" e)))
        ;; where the cursor is, for the input method's window
        (when frame
          (multiple-value-bind (x y w h) (frame-cursor-pixels f frame)
            (gtk-im-context-set-cursor-location (gtk-im f)
                                                (round (+ (fourth (gtk-padding f)) x)) (round (+ (first (gtk-padding f)) y))
                                                (round w) (round h))))))))

(defun call-with-painted-frame (function frame &key (columns 80) (rows 24) (font (font-name)) (focused t) preedit
                                                    (padding 0))
  "Paint FRAME as the window would, COLUMNS by ROWS cells in FONT with PADDING
pixels round them, in memory, without a display, with PREEDIT as what the input
method is composing, and call FUNCTION with the image surface, its width and
height in pixels, and the width and height of a cell; its value is returned."
  (load-gtk-libraries)
  (with-gtk-float-traps
    (let ((f (make-gtk-frontend)))
      (setf (gtk-fonts f) (make-fonts font)
            (gtk-preedit f) preedit
            (gtk-padding f) (multiple-value-list (tuatara.frontend:padding-sides padding)))
      (measure-font f)
      (let* ((width (+ (* columns (cell-width f)) (second (gtk-padding f)) (fourth (gtk-padding f))))
             (height (+ (* rows (cell-height f)) (first (gtk-padding f)) (third (gtk-padding f))))
             (surface (cairo-image-surface-create 0 width height))
             (cr (cairo-create surface)))
        (unwind-protect
             (progn (paint-frame f cr width height frame focused)
                    (cairo-surface-flush surface)
                    (funcall function surface width height (cell-width f) (cell-height f)))
          (cairo-destroy cr)
          (cairo-surface-destroy surface)
          (forget-layouts f)
          (loop for font across (gtk-fonts f) do (pango-font-description-free font))
          (forget-face-fonts f))))))

(defun pixel-colour (surface x y)
  "The colour of the pixel at X Y of the image SURFACE, as \"#rrggbb\"."
  (let* ((data (cairo-image-surface-get-data surface))
         (offset (+ (* y (cairo-image-surface-get-stride surface)) (* 4 x))))
    ;; 32 bits a pixel, blue first in memory on this machine
    (tuatara.display:colour-hex
     (list (cffi:mem-aref data :uint8 (+ offset 2)) (cffi:mem-aref data :uint8 (+ offset 1))
           (cffi:mem-aref data :uint8 offset)))))

(defun paint-frame-to-png (frame path &rest options)
  "Paint FRAME as the window would into a PNG file at PATH, without a display;
OPTIONS as for CALL-WITH-PAINTED-FRAME. Returns the width and height in pixels."
  (apply #'call-with-painted-frame
         (lambda (surface width height cell-width cell-height)
           (declare (ignore cell-width cell-height))
           (cairo-surface-write-to-png surface (namestring path))
           (values width height))
         frame options))

;;; The window

(defun open-window (f)
  "Open F's window, on GTK's thread: a drawing area sized to the font's cells,
taking keys through an input method, the wheel, the first mouse button, files
dropped on it and the focus."
  (setf (gtk-fonts f) (make-fonts (font-name))
        (gtk-font-in-use f) (font-name))
  (measure-font f)
  (let ((window (gtk-window-new))
        (area (gtk-drawing-area-new))
        (keys (gtk-event-controller-key-new))
        (command-keys (gtk-event-controller-key-new))
        (focus (gtk-event-controller-focus-new))
        (scroll (gtk-event-controller-scroll-new 5)) ; vertical, in whole turns
        (motion (gtk-event-controller-motion-new))
        (click (gtk-gesture-click-new))
        (middle (gtk-gesture-click-new))
        (drag (gtk-gesture-drag-new))
        (drop (gtk-drop-target-new (gdk-file-list-get-type) 1))
        (im (gtk-im-multicontext-new)))
    (setf (gtk-window f) window (gtk-area f) area (gtk-im f) im)
    (gtk-window-set-title window "Tuatara")
    (multiple-value-bind (top right bottom left) (tuatara.frontend:padding-sides *gui-padding*)
      (gtk-window-set-default-size window (+ (* 100 (cell-width f)) left right)
                                   (+ (* 40 (cell-height f)) top bottom)))
    ;; the desktop's light or dark style, and word when it changes
    (setf (gtk-desktop-style f) (settings-desktop-style))
    (g-signal-connect (gtk-settings-get-default) "notify::gtk-interface-color-scheme"
                      (cffi:callback on-style-changed))
    (g-signal-connect (gtk-settings-get-default) "notify::gtk-theme-name" (cffi:callback on-style-changed))
    (g-signal-connect (gtk-settings-get-default) "notify::gtk-application-prefer-dark-theme"
                      (cffi:callback on-style-changed))
    ;; over the drawing area, which takes the keys and the pointer: the rows' pictures, and the cursor over them
    (let ((overlay (gtk-overlay-new)) (grid (gtk-fixed-new)) (pictures (gtk-fixed-new)) (above (gtk-fixed-new))
          (cursor (gtk-drawing-area-new)) (ground (gtk-picture-new)))
      (gtk-overlay-set-child overlay area)
      (gtk-overlay-add-overlay overlay grid)
      (gtk-overlay-add-overlay overlay pictures)
      (gtk-overlay-add-overlay overlay above)
      (dolist (widget (list grid pictures above cursor ground)) (gtk-widget-set-can-target widget nil))
      (gtk-picture-set-content-fit ground 0)
      (gtk-fixed-put grid ground 0d0 0d0)
      (gtk-fixed-put above cursor 0d0 0d0)
      (gtk-drawing-area-set-draw-func cursor (cffi:callback on-draw-cursor) (cffi:null-pointer) (cffi:null-pointer))
      (gtk-widget-set-visible grid nil)
      (gtk-widget-set-visible pictures nil)
      (gtk-widget-set-visible above nil)
      (setf (gtk-grid f) grid (gtk-pictures-layer f) pictures (gtk-above f) above (gtk-cursor-area f) cursor
            (gtk-ground f) ground)
      (gtk-window-set-child window overlay))
    (gtk-drawing-area-set-draw-func area (cffi:callback on-draw) (cffi:null-pointer) (cffi:null-pointer))
    (g-signal-connect area "resize" (cffi:callback on-resize))
    (gtk-widget-set-focusable area t)
    (gtk-widget-set-cursor-from-name area "text")
    (gtk-im-context-set-client-widget im area)
    (gtk-event-controller-key-set-im-context keys im)
    (g-signal-connect im "commit" (cffi:callback on-commit))
    (g-signal-connect im "preedit-changed" (cffi:callback on-preedit-changed))
    (g-signal-connect keys "key-pressed" (cffi:callback on-key-pressed))
    (gtk-event-controller-set-propagation-phase command-keys 1)
    (g-signal-connect command-keys "key-pressed" (cffi:callback on-command-key-pressed))
    (g-signal-connect focus "enter" (cffi:callback on-focus-enter))
    (g-signal-connect focus "leave" (cffi:callback on-focus-leave))
    (g-signal-connect scroll "scroll" (cffi:callback on-scroll))
    (g-signal-connect motion "motion" (cffi:callback on-motion))
    (g-signal-connect motion "leave" (cffi:callback on-pointer-leave))
    (gtk-gesture-single-set-button click 1)
    (gtk-gesture-single-set-button drag 1)
    (g-signal-connect click "pressed" (cffi:callback on-pressed))
    (g-signal-connect click "released" (cffi:callback on-released))
    (gtk-gesture-single-set-button middle 2)
    (g-signal-connect middle "pressed" (cffi:callback on-middle-pressed))
    (g-signal-connect drag "drag-begin" (cffi:callback on-drag-begin))
    (g-signal-connect drag "drag-update" (cffi:callback on-drag-update))
    (g-signal-connect drag "drag-end" (cffi:callback on-drag-end))
    (g-signal-connect drop "drop" (cffi:callback on-drop))
    (dolist (controller (list command-keys keys focus scroll motion click middle drag drop))
      (gtk-widget-add-controller area controller))
    (g-signal-connect window "close-request" (cffi:callback on-close-request))
    ;; the clipboard read each time it changes, and now, so that a paste takes it at once
    (g-signal-connect (clipboard) "changed" (cffi:callback on-clipboard-changed))
    (read-changed-clipboard f)
    (gtk-window-present window)
    (gtk-widget-grab-focus area)))

(defun log-stderr-unless-terminal ()
  "When standard error goes to no terminal, as when the desktop starts the
window, send it to stderr.log in the cache directory (TUATARA.IO:SESSION-LOG),
so that what goes wrong is kept for reading afterwards."
  (unless (ignore-errors (= 1 (sb-unix:unix-isatty 2)))
    (let ((fd (tuatara.io:session-log "the window")))
      (when fd
        (sb-posix:dup2 fd 2)
        (sb-posix:close fd)))))

(defun icon-directories ()
  "The directories holding the program's icon, laid out as an icon theme is
(hicolor/48x48/apps/tuatara.png), that exist: icons/ beside the program,
then the source tree's data/icons/."
  (tuatara.runtime:resource-directories "icons/" "data/icons/" :existing t))

(defun use-program-icon ()
  "Let GTK find the program's icon where the program has it (ICON-DIRECTORIES)
as well as in the icon themes installed, and give the window that icon, called
tuatara, as the desktop's launcher does (tools/install-desktop.sh)."
  (let ((display (gdk-display-get-default)))
    (unless (cffi:null-pointer-p display)
      (let ((theme (gtk-icon-theme-get-for-display display)))
        (unless (cffi:null-pointer-p theme)
          (dolist (directory (icon-directories))
            (gtk-icon-theme-add-search-path theme (uiop:native-namestring directory)))))))
  (gtk-window-set-default-icon-name "tuatara"))

(defun choose-renderer ()
  "Before the window opens: have GTK draw it as *GUI-RENDERER* asks, or as
the start before this one decided (FOLLOW-RENDERER-SETTING), unless
GSK_RENDERER was set before Tuatara started."
  (cond ((uiop:getenv "TUATARA_RENDERER_CHOSEN")
         (setf *window-renderer* (uiop:getenv "GSK_RENDERER")))
        ((uiop:getenv "GSK_RENDERER")
         (setf *window-renderer* :environment))
        (t
         (setf *window-renderer* (renderer-name *gui-renderer*))
         (when *window-renderer* (sb-posix:setenv "GSK_RENDERER" *window-renderer* 1)))))

(defun renderer-chosen ()
  "Once the window has its renderer: GSK_RENDERER taken away again, unless it
was set before Tuatara started, so that the programs Tuatara starts draw as
they would."
  (sb-posix:unsetenv "TUATARA_RENDERER_CHOSEN")
  (unless (eq *window-renderer* :environment)
    (sb-posix:unsetenv "GSK_RENDERER")))

(defun follow-renderer-setting (f)
  "When *GUI-RENDERER* asks for another way of drawing than F's window opened
with: as the editor starts, start Tuatara again drawn that way (nothing is
open yet); later, say that it is taken from the next start, once. Run from
the sources, which cannot be started again, it is taken from the next start."
  (let ((wanted (renderer-name *gui-renderer*)))
    (unless (or (eq *window-renderer* :environment) (equal wanted *window-renderer*)
                (equal wanted (gtk-renderer-said f)))
      (cond ((null tuatara.io:*saved-program*))
            ((gtk-renderer-checked f)
             (setf (gtk-renderer-said f) wanted)
             (tuatara.command:message "The window is drawn ~a from the next start"
                                      (case *gui-renderer* (:gl "with GL") (:vulkan "with Vulkan")
                                        (:cairo "by the processor") (t "as GTK chooses"))))
            (t
             (sb-posix:setenv "TUATARA_RENDERER_CHOSEN" "1" 1)
             (if wanted
                 (sb-posix:setenv "GSK_RENDERER" wanted 1)
                 (sb-posix:unsetenv "GSK_RENDERER"))
             ;; the heap this start has, decided again by no one
             (sb-posix:setenv "TUATARA_HEAP_DECIDED" "1" 1)
             (tuatara.io:run-program-again
              (list* "--dynamic-space-size" (format nil "~dMB" (floor (sb-ext:dynamic-space-size) (* 1024 1024)))
                     (rest sb-ext:*posix-argv*)))
             ;; it could not be started again: from the next start
             (sb-posix:unsetenv "TUATARA_RENDERER_CHOSEN")
             (sb-posix:unsetenv "GSK_RENDERER")))))
  (setf (gtk-renderer-checked f) t))

(defun measure-clock ()
  "Microseconds on the monotonic clock, for measuring the drawing: finer than
GET-INTERNAL-REAL-TIME, which here counts whole milliseconds."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000) (floor nanoseconds 1000))))

(defparameter *drawing-measure-keys* 180
  "How many keys MEASURE-DRAWING gives the window, 60 a second.")

(defun drawing-figures (keys ends paints)
  "What KEYS (when each key went in), ENDS (when each frame was finished) and
PAINTS (when each was painted), vectors of microseconds (MEASURE-CLOCK) in order, say of how the
window drew, as a list: :KEYS, :FRAMES painted while the keys came and half
a second after, :MEDIAN and :SLOWEST (the slowest of every twenty)
milliseconds from a key to the painting of the frame that shows it."
  (flet ((after (vector time)
           (let ((i (position-if (lambda (x) (>= x time)) vector)))
             (and i (aref vector i)))))
    (let* ((latencies (sort (loop for key across keys
                                  for end = (after ends key)
                                  for paint = (and end (after paints end))
                                  when paint collect (/ (- paint key) 1000.0))
                            #'<))
           (count (length latencies))
           (first (if (plusp (length keys)) (aref keys 0) 0))
           (last (if (plusp (length keys)) (aref keys (1- (length keys))) 0)))
      (list :keys (length keys)
            :frames (count-if (lambda (x) (<= first x (+ last 500000))) paints)
            :median (and (plusp count) (nth (floor count 2) latencies))
            :slowest (and (plusp count) (nth (min (1- count) (floor (* 19 count) 20)) latencies))))))

(define-callback on-frame-painted :void ((clock :pointer) (data :pointer))
  (declare (ignore clock data))
  (let ((f *frontend*))
    (when f
      (let ((measured (sb-thread:with-mutex ((gtk-lock f)) (gtk-measured f))))
        (when measured (vector-push-extend (measure-clock) (aref measured 2)))))))

(define-callback on-measure-frames :boolean ((data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when (and f (gtk-window f))
      (g-signal-connect (gtk-widget-get-frame-clock (gtk-window f)) "after-paint" (cffi:callback on-frame-painted))))
  nil)

(defun measure-drawing (f out)
  "Measure how F's window draws, for \\[check-health]: once it has shown its
text, hold the down arrow, 60 keys a second for three seconds, each frame
painted taken from the window's frame clock, then write to the file OUT the
renderer's name and DRAWING-FIGURES, and end Tuatara."
  (sb-thread:make-thread
   (lambda ()
     (loop until (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f)) do (sleep 0.05))
     ;; each frame the window paints, as its frame clock says, on GTK's thread
     (g-idle-add (cffi:callback on-measure-frames) (cffi:null-pointer))
     ;; the file opened and the garbage of starting collected
     (sleep 2.5)
     (let ((measured (vector (make-array 0 :adjustable t :fill-pointer 0)
                             (make-array 0 :adjustable t :fill-pointer 0)
                             (make-array 0 :adjustable t :fill-pointer 0))))
       (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-measured f) measured))
       (let ((start (measure-clock)))
         (dotimes (i *drawing-measure-keys*)
           (loop for wait = (/ (- (+ start (floor (* i 1000000) 60)) (measure-clock)) 1000000)
                 while (plusp wait) do (sleep (min wait 0.002)))
           (sb-thread:with-mutex ((gtk-lock f)) (vector-push-extend (measure-clock) (aref measured 0)))
           (queue-push (gtk-queue f) (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key :down)))))
       (sleep 1)
       (let ((figures (sb-thread:with-mutex ((gtk-lock f))
                        (setf (gtk-measured f) nil)
                        (apply #'drawing-figures (coerce measured 'list)))))
         (with-open-file (o out :direction :output :if-exists :supersede)
           (with-standard-io-syntax
             (prin1 (list* :renderer (gtk-drawn-by f) figures) o)))))
     (sb-ext:exit :code 0 :abort t))
   :name "measuring the drawing"))

(defun graphics-card-p ()
  "True when the machine has a graphics card programs may draw with (a render
node under /dev/dri); without one, GL and Vulkan are only done by the
processor, as in many virtual machines."
  (some (lambda (path) (uiop:string-prefix-p "renderD" (file-namestring path)))
        (ignore-errors (uiop:directory-files "/dev/dri/"))))

(defmethod tuatara.frontend:frontend-renderers ((f gtk-frontend))
  ;; with no graphics card, Cairo too: drawn by the processor, it may then be the faster
  (values (cond ((null tuatara.io:*saved-program*) '())
                ((graphics-card-p) '(:gl :vulkan))
                (t '(:gl :vulkan :cairo)))
          *gui-renderer*))

(defun drawing-sample (path)
  "Write to PATH the text a window is measured drawing: four thousand records
of comma-separated values, as a large file of them has."
  (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
    (dotimes (i 4000)
      (format o "2026-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ,~d,The quick brown fox jumps over the lazy dog.,~36r~%"
              (1+ (mod i 12)) (1+ (mod i 28)) (mod i 24) (mod i 60) (mod (* 7 i) 60)
              (+ 1700000000 (* 7919 i)) (* 104729 (1+ i))))))

(defun measuring-folder (&optional (name (tuatara.io:random-name)))
  "Make the folder tuatara-drawing-NAME in the temporary directory, for you
alone, and return it as a directory pathname. NAME is random unless given;
when a folder or file of that name is already there, a FILE-SYSTEM-ERROR,
never one someone else made taken over."
  (let ((path (tuatara.io:path-join (uiop:native-namestring (uiop:temporary-directory))
                                    (tuatara.io:slash-ended (format nil "tuatara-drawing-~a" name)))))
    (tuatara.io:make-directory path #o700)
    (uiop:parse-native-namestring path :ensure-directory t)))

(defmethod tuatara.frontend:frontend-measure-drawing ((f gtk-frontend) renderer)
  ;; a copy of Tuatara, drawn as RENDERER says, that measures itself (MEASURE-DRAWING):
  ;; no init file, its state and cache in a folder of its own, gone after
  (let ((name (renderer-name renderer)))
    (when (and name tuatara.io:*saved-program*)
      (let ((directory (measuring-folder)))
        (unwind-protect
            (let ((sample (merge-pathnames "sample.csv" directory))
                  (out (merge-pathnames "figures" directory)))
              (drawing-sample sample)
              (tuatara.io:program-output
               (list tuatara.io:*saved-program* "--gui" (uiop:native-namestring sample))
               :capture nil :seconds 60
               :environment (append (list (format nil "GSK_RENDERER=~a" name)
                                          (format nil "TUATARA_MEASURE_DRAWING=~a" (uiop:native-namestring out))
                                          (format nil "TUATARA_MEASURE_FONT=~a" (or (gtk-font-in-use f) ""))
                                          "TUATARA_HEAP_DECIDED=1"
                                          (format nil "XDG_STATE_HOME=~astate" (uiop:native-namestring directory))
                                          (format nil "XDG_CACHE_HOME=~acache" (uiop:native-namestring directory))
                                          (format nil "XDG_DATA_HOME=~adata" (uiop:native-namestring directory)))
                                    (remove-if (lambda (pair)
                                                 (some (lambda (key) (uiop:string-prefix-p key pair))
                                                       '("GSK_RENDERER=" "TUATARA_" "XDG_STATE_HOME=" "XDG_CACHE_HOME=" "XDG_DATA_HOME=")))
                                               (sb-ext:posix-environ))))
              (let ((figures (ignore-errors
                              (with-open-file (in out)
                                (with-standard-io-syntax
                                  (let ((*read-eval* nil)) (read in)))))))
                (and (listp figures) (getf figures :median) figures)))
          (tuatara.io:delete-tree (uiop:native-namestring directory)))))))

(defun run-in-window (files setup)
  "Run the editor in a window of its own on FILES, calling SETUP with it as
RUN-EDITOR does: GTK on this thread, which must be the main one, the editor on
a thread of its own. Returns when the editor ends and the window has closed.
Standard error goes to the cache directory's stderr.log when no terminal
shows it (LOG-STDERR-UNLESS-TERMINAL)."
  (log-stderr-unless-terminal)
  (load-gtk-libraries)
  (with-gtk-float-traps
    ;; the desktop matches the window to tuatara.desktop by this name
    (g-set-prgname "tuatara")
    (g-set-application-name "Tuatara")
    (choose-renderer)
    (gtk-init)
    (use-program-icon)
    (let ((f (make-gtk-frontend)))
      (setf *frontend* f)
      (open-window f)
      (gtk-widget-realize (gtk-window f))
      (setf (gtk-drawn-by f) (window-renderer-name (gtk-window f)))
      (renderer-chosen)
      (let ((editor (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect (tuatara.command:run-editor f :files files :setup setup)
                         (unless (gtk-done f) (tuatara.frontend:frontend-stop f))))
                     :name "editor")))
        (let ((out (uiop:getenv "TUATARA_MEASURE_DRAWING")))
          (when out (measure-drawing f out)))
        (loop until (and (gtk-done f) (null (gtk-window f)))
              do (g-main-context-iteration (cffi:null-pointer) t))
        (sb-thread:join-thread editor :default nil)
        (setf *frontend* nil)
        0))))
