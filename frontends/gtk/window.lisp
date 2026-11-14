;;;; frontends/gtk/window.lisp --- The window itself: its size, the work other threads ask of GTK's, how it is drawn and
;;;; measured, and the editor run in it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.frontend.gtk)

;;; Size

(defconstant +fewest-rows+ 3
  "The fewest rows of cells the window shows: a row of text, its mode line and
the echo row.")

(defun font-steps-fitting (f steps)
  "STEPS, or as many fewer as F's window still shows +FEWEST-ROWS+ rows of
cells at, its font growing 1.2 times a step; never fewer than the steps in
use now. Called with F's lock held."
  (let ((room (gtk-room-height f)) (now (gtk-font-steps f)) (ch (cell-height f)))
    (if (or (null room) (<= steps now) (not (plusp ch)))
        steps
        (loop for fitting downfrom steps
              while (and (> fitting now) (< (floor room (* ch (expt 1.2d0 (- fitting now)))) +fewest-rows+))
              finally (return fitting)))))

(defun fit-grid (f width height)
  "Fit F's grid of cells to its drawing area of WIDTH by HEIGHT pixels, inside
the room *GUI-PADDING* asks for, telling the editor when the number of rows or
columns changed."
  (destructuring-bind (top right bottom left)
      (setf (gtk-padding f) (multiple-value-list (tuatara.frontend:padding-sides *gui-padding*)))
    (let ((columns (max 1 (floor (- width left right) (cell-width f))))
          (rows (max +fewest-rows+ (floor (- height top bottom) (cell-height f)))))
      (sb-thread:with-mutex ((gtk-lock f))
        (setf (gtk-room-height f) (- height top bottom))
        (let ((first (not (gtk-sized f))))
          (unless (and (not first) (= columns (gtk-columns f)) (= rows (gtk-rows f)))
            (setf (gtk-columns f) columns (gtk-rows f) rows (gtk-sized f) t)
            (sb-thread:condition-broadcast (gtk-changed f))
            (unless first
              (push-event f :resize :width columns :height rows))))))))

(define-callback on-resize :void ((area :pointer) (width :int) (height :int) (data :pointer))
  (declare (ignore area))
  (let ((f (data-window data)))
    (when f (fit-grid f width height))))

(defun measure-font (f)
  "Set F's cell size from its fonts, laid out on a picture of one pixel."
  (let* ((probe (cairo-image-surface-create 0 1 1)) (probe-cr (cairo-create probe)))
    (unwind-protect (measure-cells f probe-cr)
      (cairo-destroy probe-cr)
      (cairo-surface-destroy probe))))

(defun scale-fonts (f steps)
  "Make F's fonts STEPS steps from the size they started at, each 1.2 times
the last, and set its cell size from them."
  (let ((old (gtk-fonts f)))
    (setf (gtk-fonts f) (make-fonts (font-name) (expt 1.2d0 steps))
          (gtk-font-in-use f) (font-name))
    (loop for font across old do (pango-font-description-free font)))
  (measure-font f))

(define-callback on-rescale-font :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when (and f (gtk-area f))
      (let ((steps (sb-thread:with-mutex ((gtk-lock f))
                     (setf (gtk-settings-pending f) nil)
                     (gtk-font-steps f))))
        (scale-fonts f steps)
        ;; a font grown larger than the window shows its fewest rows at goes back as far as needed
        (loop with room = (sb-thread:with-mutex ((gtk-lock f)) (gtk-room-height f))
              while (and room (plusp steps) (< (floor room (cell-height f)) +fewest-rows+))
              do (scale-fonts f (decf steps))
                 (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-font-steps f) steps))))
      (forget-layouts f)
      (forget-face-fonts f)
      (fit-grid f (gtk-widget-get-width (gtk-area f)) (gtk-widget-get-height (gtk-area f)))
      (redraw f)))
  nil)

;;; Main-loop work other threads ask for

(define-callback on-draw-request :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when (and f (gtk-area f))
      (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-draw-pending f) nil))
      (redraw f)))
  nil)

(define-callback on-close-window :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when f
      ;; what GTK says of the window as it goes finds it no more
      (forget-window f)
      (loop for (instance . number) in (shiftf (gtk-signals f) '())
            do (g-signal-handler-disconnect instance number))
      (when (gtk-window f)
        (gtk-window-destroy (shiftf (gtk-window f) nil))
        (setf (gtk-area f) nil))))
  nil)

(define-callback on-open-window :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when f
      (unwind-protect
           (progn
             (open-window f)
             (gtk-widget-realize (gtk-window f))
             (setf (gtk-drawn-by f) (window-renderer-name (gtk-window f))))
        ;; not opened: the editor waiting for its size goes on, to say so (FRONTEND-START)
        (unless (gtk-window f)
          (forget-window f)
          (sb-thread:with-mutex ((gtk-lock f))
            (setf (gtk-sized f) t)
            (sb-thread:condition-broadcast (gtk-changed f)))))))
  nil)

(define-callback on-raise :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when (and f (gtk-window f))
      (gtk-window-present (gtk-window f))))
  nil)

(define-callback on-editor-ended :boolean ((data :pointer))
  ;; nothing to do: GTK's loop looks again whether it has anything left
  (declare (ignore data))
  nil)

(defun wake-gtk-loop ()
  "Make RUN-GTK-LOOP look again whether it is finished, from any thread."
  (g-idle-add (callback on-editor-ended) (null-pointer)))

(defun run-gtk-loop (finished-p)
  "Handle GTK's events on this thread, the main one, until FINISHED-P, a function
of no arguments, is true. It is asked after each event, and after each call of
WAKE-GTK-LOOP."
  (loop until (funcall finished-p)
        do (g-main-context-iteration (null-pointer) t)))

(defun clipboard ()
  "The clipboard of the display GTK is connected to."
  (gdk-display-get-clipboard (gdk-display-get-default)))

(define-callback on-set-clipboard :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when f
      (multiple-value-bind (text primary)
          (sb-thread:with-mutex ((gtk-lock f))
            (values (shiftf (gtk-clipboard-out f) nil) (shiftf (gtk-primary-out f) nil)))
        (when text
          (gdk-clipboard-set-text (clipboard) (tuatara.display:displayable text :controls nil)))
        (when primary
          (gdk-clipboard-set-text (gdk-display-get-primary-clipboard (gdk-display-get-default))
                                  (tuatara.display:displayable primary :controls nil))))))
  nil)

(defun shown-title (title)
  "TITLE as the window's title bar shows it: each octet of a name that is not
UTF-8 written \\xNN, as the mode line writes it, so that two such names never
look alike."
  (tuatara.display:displayable (tuatara.command:escape-controls title nil :raw-bytes) :controls nil))

(define-callback on-set-title :boolean ((data :pointer))
  (let* ((f (data-window data))
         (title (and f (sb-thread:with-mutex ((gtk-lock f)) (gtk-title f)))))
    (when (and title (gtk-window f))
      (gtk-window-set-title (gtk-window f) (shown-title title))))
  nil)

(defun finished-clipboard-text (source result)
  "The text of the clipboard read that GTK finished, SOURCE and RESULT as it gave
them, or NIL when the clipboard held no text."
  (let ((pointer (gdk-clipboard-read-text-finish source result (null-pointer))))
    (unless (null-pointer-p pointer)
      (prog1 (sap-string pointer) (g-free pointer)))))

(define-callback on-clipboard-read :void ((source :pointer) (result :pointer) (data :pointer))
  (let ((f (data-window data))
        (text (finished-clipboard-text source result)))
    (when f
      (sb-thread:with-mutex ((gtk-lock f))
        (setf (gtk-clipboard-in f) (or text :none))
        (sb-thread:condition-broadcast (gtk-changed f))))))

(define-callback on-clipboard-kept :void ((source :pointer) (result :pointer) (data :pointer))
  (let ((f (data-window data))
        (text (finished-clipboard-text source result)))
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
  (gdk-clipboard-read-text-async (clipboard) (null-pointer) (callback on-clipboard-kept) (window-data f)))

(define-callback on-clipboard-changed :void ((clipboard :pointer) (data :pointer))
  (declare (ignore clipboard))
  (let ((f (data-window data)))
    (when f (read-changed-clipboard f))))

(define-callback on-watch-clipboard :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when f
      (push (cons (clipboard) (g-signal-connect (clipboard) "changed" (callback on-clipboard-changed) data))
            (gtk-signals f))
      (read-changed-clipboard f)))
  nil)

(define-callback on-read-clipboard :boolean ((data :pointer))
  (gdk-clipboard-read-text-async (clipboard) (null-pointer) (callback on-clipboard-read) data)
  nil)

(define-callback on-read-primary :boolean ((data :pointer))
  (gdk-clipboard-read-text-async (gdk-display-get-primary-clipboard (gdk-display-get-default))
                                 (null-pointer) (callback on-clipboard-read) data)
  nil)

;;; The window

(defun open-window (f)
  "Open F's window, on GTK's thread: a drawing area sized to the font's cells,
taking keys through an input method, the wheel, the first mouse button, files
dropped on it and the focus."
  (setf (gtk-fonts f) (make-fonts (font-name))
        (gtk-font-in-use f) (font-name))
  (measure-font f)
  (let ((data (window-data f))
        (window (gtk-window-new))
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
    (let ((settings (gtk-settings-get-default)))
      (dolist (signal '("notify::gtk-interface-color-scheme" "notify::gtk-theme-name"
                        "notify::gtk-application-prefer-dark-theme"))
        (push (cons settings (g-signal-connect settings signal (callback on-style-changed) data)) (gtk-signals f))))
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
      (gtk-drawing-area-set-draw-func cursor (callback on-draw-cursor) data (null-pointer))
      (gtk-widget-set-visible grid nil)
      (gtk-widget-set-visible pictures nil)
      (gtk-widget-set-visible above nil)
      (setf (gtk-grid f) grid (gtk-pictures-layer f) pictures (gtk-above f) above (gtk-cursor-area f) cursor
            (gtk-ground f) ground)
      (gtk-window-set-child window overlay))
    (gtk-drawing-area-set-draw-func area (callback on-draw) data (null-pointer))
    (g-signal-connect area "resize" (callback on-resize) data)
    (gtk-widget-set-focusable area t)
    (gtk-widget-set-cursor-from-name area "text")
    (gtk-im-context-set-client-widget im area)
    (gtk-event-controller-key-set-im-context keys im)
    (g-signal-connect im "commit" (callback on-commit) data)
    (g-signal-connect im "preedit-changed" (callback on-preedit-changed) data)
    (g-signal-connect keys "key-pressed" (callback on-key-pressed) data)
    (gtk-event-controller-set-propagation-phase command-keys 1)
    (g-signal-connect command-keys "key-pressed" (callback on-command-key-pressed) data)
    (g-signal-connect focus "enter" (callback on-focus-enter) data)
    (g-signal-connect focus "leave" (callback on-focus-leave) data)
    (g-signal-connect scroll "scroll" (callback on-scroll) data)
    (g-signal-connect motion "motion" (callback on-motion) data)
    (g-signal-connect motion "leave" (callback on-pointer-leave) data)
    (gtk-gesture-single-set-button click 1)
    (gtk-gesture-single-set-button drag 1)
    (g-signal-connect click "pressed" (callback on-pressed) data)
    (g-signal-connect click "released" (callback on-released) data)
    (gtk-gesture-single-set-button middle 2)
    (g-signal-connect middle "pressed" (callback on-middle-pressed) data)
    (g-signal-connect drag "drag-begin" (callback on-drag-begin) data)
    (g-signal-connect drag "drag-update" (callback on-drag-update) data)
    (g-signal-connect drag "drag-end" (callback on-drag-end) data)
    (g-signal-connect drop "drop" (callback on-drop) data)
    (dolist (controller (list command-keys keys focus scroll motion click middle drag drop))
      (gtk-widget-add-controller area controller))
    (g-signal-connect window "close-request" (callback on-close-request) data)
    (gtk-window-present window)
    (gtk-widget-grab-focus area)))

(defun log-stderr-unless-terminal ()
  "When standard error goes to no terminal, as when the desktop starts the
window, send it to stderr.log in the cache directory (TUATARA.IO:SESSION-LOG),
so that what goes wrong is kept for reading afterwards."
  (unless (= 1 (sb-unix:unix-isatty 2))
    (let ((fd (tuatara.io:session-log "the window")))
      (when fd
        (sb-posix:dup2 fd 2)
        (tuatara.runtime:close-fd-quietly fd "the session log")))))

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
    (unless (null-pointer-p display)
      (let ((theme (gtk-icon-theme-get-for-display display)))
        (unless (null-pointer-p theme)
          (dolist (directory (icon-directories))
            (gtk-icon-theme-add-search-path theme (uiop:native-namestring directory)))))))
  (gtk-window-set-default-icon-name "tuatara"))

(defun choose-renderer ()
  "Before GTK starts, once: have it draw every window of this program as
*GUI-RENDERER* asks, or as the start before this one decided
\(FOLLOW-RENDERER-SETTING), unless GSK_RENDERER was set before Tuatara
started. The GSK_RENDERER set here stays for as long as Tuatara runs, GTK
reading it as each window opens; the programs Tuatara starts are not given
it (TUATARA.IO:*OWN-VARIABLES*)."
  (cond ((tuatara.io:own-variable "TUATARA_RENDERER_CHOSEN")
         (sb-posix:unsetenv "TUATARA_RENDERER_CHOSEN")
         (setf *window-renderer* (tuatara.io:own-variable "GSK_RENDERER")))
        ((tuatara.io:own-variable "GSK_RENDERER")
         (setf *window-renderer* :environment))
        (t
         (setf *window-renderer* (renderer-name *gui-renderer*))
         (when *window-renderer* (sb-posix:setenv "GSK_RENDERER" *window-renderer* 1))))
  (unless (eq *window-renderer* :environment)
    (pushnew "GSK_RENDERER" tuatara.io:*own-variables* :test #'string=)))

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
             ;; started again in an environment of its own: this one, which GTK's thread reads, left as it is;
             ;; the heap this start has, decided again by no one
             (tuatara.io:run-program-again
              (rest sb-ext:*posix-argv*)
              :heap (floor (sb-ext:dynamic-space-size) (* 1024 1024))
              :environment (apply #'tuatara.io:environment-without
                                  '("GSK_RENDERER=" "TUATARA_RENDERER_CHOSEN=" "TUATARA_HEAP_DECIDED=")
                                  "TUATARA_RENDERER_CHOSEN=1" "TUATARA_HEAP_DECIDED=1"
                                  (and wanted (list (format nil "GSK_RENDERER=~a" wanted)))))))))
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
PAINTS (when each was painted) say of how the window drew, as a list. The
three are vectors of microseconds (MEASURE-CLOCK) in order. The list is
:KEYS, :FRAMES painted while the keys came and half a second after, and
:MEDIAN and :SLOWEST (the slowest of every twenty) milliseconds from a key to
the painting of the frame that shows it."
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
  (declare (ignore clock))
  (let ((f (data-window data)))
    (when f
      (let ((measured (sb-thread:with-mutex ((gtk-lock f)) (gtk-measured f))))
        (when measured (vector-push-extend (measure-clock) (aref measured 2)))))))

(define-callback on-measure-frames :boolean ((data :pointer))
  (let ((f (data-window data)))
    (when (and f (gtk-window f))
      (g-signal-connect (gtk-widget-get-frame-clock (gtk-window f)) "after-paint" (callback on-frame-painted) data)))
  nil)

(defun measure-drawing (f out)
  "Measure how F's window draws, for \\[check-health]. Once it has shown its
text, hold the down arrow, 60 keys a second for three seconds, each frame
painted taken from the window's frame clock. Then write to the file OUT the
renderer's name and DRAWING-FIGURES, and end Tuatara with exit code 0, or 1
when the measuring failed, said in the debug log."
  (tuatara.runtime:make-guarded-thread
   (lambda ()
     (sb-ext:exit :code (if (tuatara.runtime:with-thread-guard ("Measuring the drawing")
                              (measure-and-write-drawing f out)
                              t)
                            0 1)
                  :abort t))
   :name "measuring the drawing" :niceness 0))

(defun measure-and-write-drawing (f out)
  "The work of MEASURE-DRAWING: measure how F's window draws, and write the
figures to the file OUT."
  (loop until (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f)) do (sleep 0.05))
  ;; each frame the window paints, as its frame clock says, on GTK's thread
  (g-idle-add (callback on-measure-frames) (window-data f))
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
        (push-key f (tuatara.frontend:make-key :down))))
    (sleep 1)
    (let ((figures (sb-thread:with-mutex ((gtk-lock f))
                     (setf (gtk-measured f) nil)
                     (apply #'drawing-figures (coerce measured 'list)))))
      (with-open-file (o out :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (prin1 (list* :renderer (gtk-drawn-by f) figures) o))))))

(defun graphics-card-p ()
  "True when the machine has a graphics card programs may draw with (a render
node under /dev/dri); without one, GL and Vulkan are only done by the
processor, as in many virtual machines."
  (some (lambda (path) (uiop:string-prefix-p "renderD" (file-namestring path)))
        (tuatara.runtime:ignoring-errors-reported ("listing /dev/dri/") (uiop:directory-files "/dev/dri/"))))

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

(defun measuring-directory (&optional (name (tuatara.io:random-name)))
  "Make the directory tuatara-drawing-NAME in the temporary directory, for you
alone, and return it as a directory pathname. NAME is random unless given;
when a directory or file of that name is already there, a FILE-SYSTEM-ERROR,
never one someone else made taken over."
  (let ((path (tuatara.io:path-join (uiop:native-namestring (uiop:temporary-directory))
                                    (tuatara.io:slash-ended (format nil "tuatara-drawing-~a" name)))))
    (tuatara.io:make-directory path #o700)
    (uiop:parse-native-namestring path :ensure-directory t)))

(defmethod tuatara.frontend:frontend-measure-drawing ((f gtk-frontend) renderer)
  ;; a copy of Tuatara, drawn as RENDERER says, that measures itself (MEASURE-DRAWING):
  ;; no init file, its state and cache in a directory of its own, gone after
  (let ((name (renderer-name renderer)))
    (when (and name tuatara.io:*saved-program*)
      (let ((directory (measuring-directory)))
        (unwind-protect
            (let ((sample (merge-pathnames "sample.csv" directory))
                  (out (merge-pathnames "figures" directory)))
              (drawing-sample sample)
              (tuatara.io:program-output
               (list tuatara.io:*saved-program* "--gui" (uiop:native-namestring sample))
               :capture nil :seconds 60
               :environment (tuatara.io:environment-without
                             '("GSK_RENDERER=" "TUATARA_" "XDG_STATE_HOME=" "XDG_CACHE_HOME=" "XDG_DATA_HOME=")
                             (format nil "GSK_RENDERER=~a" name)
                             (format nil "TUATARA_MEASURE_DRAWING=~a" (uiop:native-namestring out))
                             (format nil "TUATARA_MEASURE_FONT=~a" (or (gtk-font-in-use f) ""))
                             "TUATARA_HEAP_DECIDED=1"
                             (format nil "XDG_STATE_HOME=~astate" (uiop:native-namestring directory))
                             (format nil "XDG_CACHE_HOME=~acache" (uiop:native-namestring directory))
                             (format nil "XDG_DATA_HOME=~adata" (uiop:native-namestring directory))))
              (let ((figures (tuatara.runtime:ignoring-errors-reported ("reading ~a" out)
                               (with-open-file (in out)
                                 (with-standard-io-syntax
                                   (let ((*read-eval* nil)) (read in)))))))
                (and (listp figures) (getf figures :median) figures)))
          (tuatara.io:delete-tree (uiop:native-namestring directory)))))))

(defun open-first-window ()
  "Start GTK on this thread, the main one, and open the program's first
window, drawn as *GUI-RENDERER* asks (CHOOSE-RENDERER): a frontend for it,
not yet given its frame. Called within WITH-GTK-FLOAT-TRAPS, GTK's libraries
loaded."
  ;; the desktop matches the window to tuatara.desktop by this name
  (g-set-prgname "tuatara")
  (g-set-application-name "Tuatara")
  (choose-renderer)
  (gtk-init)
  (use-program-icon)
  (let ((f (make-gtk-frontend)))
    (open-window f)
    (gtk-widget-realize (gtk-window f))
    (setf (gtk-drawn-by f) (window-renderer-name (gtk-window f)))
    f))

(defun open-plain-window ()
  "Open a window for a client to show something in, as OPEN-FIRST-WINDOW does.
*GUI-RENDERER* is not looked at again for it (FOLLOW-RENDERER-SETTING): it is
a window that shows words or an editor running elsewhere."
  (let ((f (open-first-window)))
    (setf (gtk-renderer-checked f) t)
    f))

(defun run-in-window (files setup &key start)
  "Run the editor in a window of its own on FILES, calling SETUP with it, and
START once the files are shown, as RUN-EDITOR does: GTK on this thread, which
must be the main one, the editor on a thread of its own, every frame it
opens a window of its own. Returns when the editor ends and every window has
closed. Standard error goes to the cache directory's stderr.log when no
terminal shows it (LOG-STDERR-UNLESS-TERMINAL)."
  (log-stderr-unless-terminal)
  (load-gtk-libraries)
  (with-gtk-float-traps
    (let ((f (open-first-window))
          (ended nil))
      (let ((editor (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect (tuatara.command:run-editor f :files files :setup setup :start start)
                         (setf ended t)
                         (dolist (window (open-windows))
                           (unless (gtk-done window) (tuatara.frontend:frontend-stop window)))
                         (wake-gtk-loop)))
                     :name "editor")))
        (let ((out (tuatara.io:own-variable "TUATARA_MEASURE_DRAWING")))
          (when out (measure-drawing f out)))
        (run-gtk-loop (lambda () (and ended (null (open-windows)))))
        (sb-thread:join-thread editor :default nil)
        0))))

;;; A client of an editor running elsewhere, in windows of its own

(defun show-client-window (f fd greeted request input &key another painted gone allow)
  "Show in F, a window, the editor at the other end of the local socket FD,
which has said hello (GREETED, as TUATARA.FRONTEND:GREET-EDITOR read it),
asking it for REQUEST, with INPUT, a descriptor, passed to be read, until the
editor lets the window go or goes. PAINTED, a function of the exit code and
the words the editor gave, is called when it painted the window. GONE, a
function of no arguments, is called once the window has gone, whatever
happened, a failure to close it too. ANOTHER opens another window when the
editor asks for one. ALLOW says what an editor on another machine is given
all the same (TUATARA.FRONTEND:MAKE-REMOTE-CLIENT). FD is closed here when no
client could be made of it."
  (let ((client nil))
    (unwind-protect
         (handler-case
             (progn
               ;; the window, started here or already open, takes its size before the editor is told it
               (tuatara.frontend:frontend-start f)
               (setf client (tuatara.frontend:make-remote-client fd f :request request :standard-input input
                                                                       :greeted greeted :another another
                                                                       :allow allow))
               ;; its events stand for the editor's frame, as an editor's window's do
               (setf (tuatara.frontend:frontend-frame f) client)
               (multiple-value-bind (exit said) (tuatara.frontend:run-remote-client client)
                 (when (tuatara.frontend:remote-client-painted client)
                   (funcall painted exit said))))
           (tuatara.frontend:remote-refused (condition)
             (tuatara.runtime:debug-report :remote "a window refused: ~a"
                                           (tuatara.frontend:remote-refused-words condition))))
      (unless client (tuatara.frontend:shut-and-close fd))
      (unwind-protect (tuatara.frontend:frontend-stop f)
        (funcall gone)))))

(defun show-in-window (function)
  "Call FUNCTION with a window of its own, a GTK-FRONTEND it starts and
stops, on a thread of its own, GTK running on this thread, which must be
the main one, until the window has closed: for a client to show why it
could not show its daemon."
  (load-gtk-libraries)
  (with-gtk-float-traps
    (let ((f (open-plain-window)) (done nil))
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (unwind-protect (funcall function f)
           (setf done t)
           (wake-gtk-loop)))
       :name "tuatara window of words" :niceness 0)
      (run-gtk-loop (lambda () (and done (null (open-windows))))))))

(defun run-window-client (fd greeted request &key standard-input connect allow)
  "Show the editor at the other end of the local socket FD, which has said
hello (GREETED, as TUATARA.FRONTEND:GREET-EDITOR read it, its settings taken
here), in a window of its own, asking it for REQUEST, a TUATARA.FRONTEND:REQUEST,
with STANDARD-INPUT, a descriptor, passed to be read: its events sent to the
editor and what it paints shown. Each further window the editor asks for
\(another frame) is a connection of its own, which CONNECT, a function of no
arguments, makes: a socket, or NIL when none can be made. ALLOW, what an
editor on another machine is given all the same, goes to every window
\(TUATARA.FRONTEND:MAKE-REMOTE-CLIENT). GTK runs on this
thread, which must be the main one; each window is painted and read on a
thread of its own; a window it will not open, it says so to the editor at
once. Returns once every window has closed: the exit code the editor gave
the window it painted that closed last, or NIL when the editor went without
giving one, and what it said with it; a window let go of before it was
painted (one that came too late) gives none."
  (log-stderr-unless-terminal)
  (load-gtk-libraries)
  (with-gtk-float-traps
    (let ((lock (sb-thread:make-mutex :name "window client"))
          (showing 1) (code nil) (words nil))
      (labels ((painted (exit said)
                 (sb-thread:with-mutex (lock) (setf code exit words said)))
               (gone ()
                 (sb-thread:with-mutex (lock) (decf showing))
                 (wake-gtk-loop))
               (show (f fd greeted request input)
                 (show-client-window f fd greeted request input :another #'another :painted #'painted :gone #'gone
                                                                :allow allow))
               (another (asking word)
                 ;; asked for by the editor: a connection of its own, attached with WORD, or word that it is not
                 (let* ((room (sb-thread:with-mutex (lock)
                                (when (< showing tuatara.frontend:+most-client-windows+)
                                  (incf showing))))
                        (fd (and room (funcall connect))))
                   (if (null fd)
                       (progn
                         (when room (sb-thread:with-mutex (lock) (decf showing)))
                         (tuatara.frontend:decline-window
                          asking word (if room
                                          "the client could not connect again"
                                          (format nil "the client shows ~d windows, the most it opens"
                                                  tuatara.frontend:+most-client-windows+))))
                       (tuatara.runtime:make-guarded-thread
                        (lambda ()
                          ;; the settings this client took as its first window opened stay as they are
                          (let ((greeted (handler-case (tuatara.frontend:greet-editor fd :take nil)
                                           (tuatara.frontend:remote-refused (condition)
                                             (tuatara.frontend:decline-window
                                              asking word (tuatara.frontend:remote-refused-words condition))
                                             nil))))
                            (if greeted
                                (show (let ((f (make-gtk-frontend))) (setf (gtk-renderer-checked f) t) f) fd greeted
                                      (tuatara.frontend:make-request
                                       :directory (tuatara.frontend:request-directory request) :joins word
                                       :far (tuatara.frontend:request-far request))
                                      nil)
                                (progn (tuatara.frontend:shut-and-close fd)
                                       (gone)))))
                        :name "tuatara window client greeting" :niceness 0)))))
        (let ((f (open-plain-window)))
          (tuatara.runtime:make-guarded-thread (lambda () (show f fd greeted request standard-input))
                                               :name "tuatara window client" :niceness 0))
        (run-gtk-loop (lambda () (and (zerop (sb-thread:with-mutex (lock) showing)) (null (open-windows)))))
        (values code words)))))
