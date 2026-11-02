;;;; src/command/frames.lisp --- The editor's frames: the one worked in, and frames added and taken away.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A frame is one screen of the editor (editor.lisp). Commands never name
;;;; one: they work in the editor's current frame, and the command loop makes
;;;; current the frame each event happened in (TAKE-TO-ITS-FRAME, dispatch.lisp).
;;;; Every frame shares the buffers; each has its own windows, and so its own
;;;; point in a buffer it shows. The list of frames is read from other threads
;;;; (KEY-WAITING-P, HOST-BUFFER-SHOWN-P), so it is replaced, never changed in place.
;;;; Frames on a display that shows one at a time, a terminal, share its
;;;; frontend and take turns on it: the one worked in is shown, the others
;;;; wait unpainted (FRAME-TAKING-TURNS-P).

(in-package #:tuatara.command)

(defun %map-frames-windows (function editor)
  "Call FUNCTION with each window of every frame of EDITOR, those of the frame
worked in first, each frame's from the top left, making nothing to go
through them (DO-FRAMES-WINDOWS)."
  (declare (function function))
  (labels ((walk (area)
             (if (typep area 'tuatara.display:window)
                 (funcall function area)
                 (dolist (child (tuatara.display:split-children area))
                   (walk child))))
           (walk-frame (frame)
             (let ((window (frame-window frame)))
               (when window (walk (tuatara.display:area-root window))))))
    (let ((current (editor-frame editor)))
      (walk-frame current)
      (dolist (frame (editor-frames editor))
        (unless (eq frame current) (walk-frame frame))))))

(defmacro do-frames-windows ((window &optional (editor '*editor*)) &body body)
  "Run BODY with WINDOW bound to each window of every frame of EDITOR, those of
the frame worked in first, each frame's from the top left, making nothing to
go through them however many frames there are: for what holds wherever a
buffer is shown, as a list kept current while it is in sight."
  (let ((visit (gensym "VISIT")))
    `(flet ((,visit (,window) ,@body))
       (declare (dynamic-extent #',visit))
       (%map-frames-windows #',visit ,editor))))

(defun frames-windows (&optional (editor *editor*))
  "Every window of every frame of EDITOR, each frame's from the top left, those
of the frame worked in first, as a list (DO-FRAMES-WINDOWS goes through them
without one): for what holds wherever a buffer is shown, as a window keeping
its place while the buffer changes."
  (let ((windows '()))
    (do-frames-windows (window editor)
      (push window windows))
    (nreverse windows)))

(defun frame-taking-turns-p (editor frame)
  "True when FRAME shares its display with another of EDITOR's frames, as the
frames of one terminal do: one of them is shown at a time, the one worked in,
and the others wait unpainted until they are worked in again."
  (let ((frontend (frame-frontend frame)))
    (loop for other in (editor-frames editor)
          thereis (and (not (eq other frame)) (eq (frame-frontend other) frontend)))))

(defun select-frame (editor frame)
  "Make FRAME, one of EDITOR's frames, the one worked in. The selected window
of the frame left keeps its point, and that of FRAME moves its buffer's point
to its own, as selecting a window does (SELECT-WINDOW); what follows the
buffer shown is brought in line with FRAME's (*AFTER-TICK-FUNCTIONS*). On a
display the two take turns on (FRAME-TAKING-TURNS-P), FRAME is shown in place
of the other, at the display's size now, and painted whole. Returns FRAME."
  (let ((old (editor-frame editor)))
    (unless (eq old frame)
      (let ((leaving (frame-window old)))
        (when leaving (keep-window-point leaving)))
      ;; painted last as the frame worked in: painted again as one of the others
      (setf (frame-painted old) nil)
      (let ((entering (frame-window frame)))
        (when entering (give-window-buffer-point entering)))
      (setf (editor-frame editor) frame)
      (when (eq (frame-frontend frame) (frame-frontend old))
        ;; the display may have been resized while FRAME waited; its title is the other's
        (setf (frame-title-shown frame) nil)
        (multiple-value-bind (width height) (tuatara.frontend:frontend-size (frame-frontend frame))
          (unless (and (= (max 1 width) (frame-width frame)) (= (max 3 height) (frame-height frame)))
            (resize editor width height))))
      (tuatara.runtime:run-hook *after-tick-functions* editor))
    frame))

(defun call-in-frame (editor frame function)
  "Call FUNCTION with FRAME the frame EDITOR works in (SELECT-FRAME), and the
frame worked in before made so again after, or the first of the frames left
when FUNCTION took it away; returns what FUNCTION returns."
  (let ((was (editor-frame editor)))
    (if (eq was frame)
        (funcall function)
        (progn
          (select-frame editor frame)
          (unwind-protect (funcall function)
            (select-frame editor (if (frame-deleted was) (first (editor-frames editor)) was)))))))

(defun other-frames-deferred (editor)
  "The next of what waits to be done after a command in another frame of
EDITOR than the one worked in, which has nothing waiting (EDITOR-DEFERRED):
taken from that frame's, as a function doing it with that frame the one worked
in (CALL-IN-FRAME); NIL when nothing waits. Work handed on from the background
for a frame runs so without waiting for a key typed there."
  (let ((frames (editor-frames editor)))
    (when (rest frames)
      (let ((frame (find-if #'frame-deferred frames)))
        (when frame
          (let ((item (pop (frame-deferred frame))))
            (lambda () (call-in-frame editor frame (lambda () (handle-event editor item))))))))))

(defun free-frame-name (editor)
  "The name for a new frame of EDITOR: F and the smallest number from 1 that
no frame of EDITOR is named with."
  (loop for n from 1
        for name = (format nil "F~d" n)
        unless (find name (editor-frames editor) :key #'frame-name :test #'string=)
          return name))

(defun add-frame (editor frontend &optional (buffer (tuatara.display:window-buffer (editor-window editor))))
  "Give EDITOR another frame, painted through FRONTEND, and return it: as large
as FRONTEND says, with one window showing BUFFER, by default the buffer of the
window worked in, at its point, named by FREE-FRAME-NAME. A FRONTEND new to
EDITOR is started, and delivers its events through the source EDITOR's frames
share, each naming the frame returned (TUATARA.FRONTEND:FRONTEND-FRAME); one
that paints a frame of EDITOR already is shared: the frames take turns on it
\(FRAME-TAKING-TURNS-P). The frame worked in stays so."
  (let ((frame (make-frame frontend)))
    (setf (frame-name frame) (free-frame-name editor))
    (unless (find frontend (editor-frames editor) :key #'frame-frontend)
      (setf (tuatara.frontend:frontend-frame frontend) frame)
      (tuatara.frontend:frontend-start frontend))
    (multiple-value-bind (width height) (tuatara.frontend:frontend-size frontend)
      (setf (frame-width frame) (max 1 width) (frame-height frame) (max 3 height)
            (frame-window frame) (tuatara.display:make-window buffer :width (max 1 width)
                                                                     :height (max 1 (- height 2)))))
    (keep-window-point (frame-window frame))
    (setf (editor-frames editor) (append (editor-frames editor) (list frame)))
    (call-in-frame editor frame (lambda () (fit-windows editor)))
    frame))

(defparameter *most-frames* 16
  "The most frames the editor opens by itself as it starts: those --frames asks
for, and those of the session kept.")

(defun open-frame (editor &key buffer (select t))
  "Open another frame of EDITOR beside the frame worked in, showing BUFFER,
else the buffer of the window worked in, and with SELECT make it the one
worked in, brought before the others; returns it. Where the display shows
one frame at a time, as a terminal does, the new frame takes turns on it with
the others (FRAME-TAKING-TURNS-P): with SELECT it is shown in their place."
  (let ((frontend (or (tuatara.frontend:frontend-another (editor-frontend editor)) (editor-frontend editor))))
    (let ((frame (if buffer (add-frame editor frontend buffer) (add-frame editor frontend))))
      (when select
        (select-frame editor frame)
        (tuatara.frontend:frontend-raise frontend))
      frame)))

(defun remove-frame (editor frame)
  "Take FRAME out of EDITOR's frames and stop its frontend; when it was the
one worked in, the first of those left is made so. The last frame is never
taken: that is refused, as an editor has a frame for as long as it runs. Its
events still to come are dropped (TAKE-TO-ITS-FRAME); what was still to be
said in it is said in the frame worked in, and work handed on to be done in
it (DEFER-ACTION) is done there. Its frontend is stopped unless another frame
takes turns on it."
  (let ((left (remove frame (editor-frames editor))))
    (unless left
      (refuse "The editor's only frame stays while the editor runs"))
    (when (eq frame (editor-frame editor))
      (select-frame editor (first left)))
    (setf (editor-frames editor) left
          (frame-deleted frame) t
          (editor-deferred editor) (append (editor-deferred editor)
                                           (remove-if-not #'functionp (shiftf (frame-deferred frame) '()))))
    (unless (find (frame-frontend frame) left :key #'frame-frontend)
      (tuatara.runtime:ignoring-errors-reported ("closing a frame's display")
        (tuatara.frontend:frontend-stop (frame-frontend frame))))
    frame))

(defun note-frame-painted (frame)
  "Keep in FRAME what it shows as it is painted now (FRAME-PAINTED): the
display generation, the faces' generation and each window with its buffer and
the buffer's version."
  (setf (frame-painted frame)
        (list* *display-generation* tuatara.display:*face-generation*
               (loop for window in (tuatara.display:area-windows (tuatara.display:area-root (frame-window frame)))
                     for buffer = (tuatara.display:window-buffer window)
                     collect (list* window buffer (tuatara.buffer:buffer-version buffer))))))

(defun frame-shown-changed-p (frame)
  "True when what FRAME shows may have changed since it was painted
\(NOTE-FRAME-PAINTED): it was never painted so, the display or the faces
moved on, or one of its windows shows another buffer or a buffer changed.
Nothing is made to find out."
  (let ((painted (frame-painted frame)))
    (or (null painted)
        (not (eql (first painted) *display-generation*))
        (not (eql (second painted) tuatara.display:*face-generation*))
        (loop for (window buffer . version) in (cddr painted)
              thereis (or (not (eq (tuatara.display:window-buffer window) buffer))
                          (not (eq (tuatara.buffer:buffer-version buffer) version)))))))

(defvar *settings-seen* (make-hash-table :test 'eq)
  "The value each setting had when SETTINGS-MOVED-P last looked, by its name.")

(defun settings-moved-p ()
  "True when a setting (a variable TUATARA-USER exports, a package's too) has
another value than when this was last asked, however it was set: the init
file, \\[eval-expression], a command. Nothing is made while none moved. A
value changed in place, as a list edited where it is, is not seen: only a
setting given another value."
  (let ((seen *settings-seen*) (moved nil))
    (do-external-symbols (symbol :tuatara-user)
      (when (and (boundp symbol) (not (constantp symbol)))
        (let ((value (symbol-value symbol)))
          (multiple-value-bind (was there) (gethash symbol seen)
            (unless (and there (eql was value))
              (setf (gethash symbol seen) value
                    moved t))))))
    moved))

(defun look-at-settings (editor)
  "Move the display generation on when a setting moved (SETTINGS-MOVED-P),
looking only when something was done in EDITOR since the last look, and at most
every *FRAME-AT-LEAST-EVERY* milliseconds: NIL, or the seconds after which the
look that is due may be taken."
  (when (>= *last-input-time* (editor-settings-looked editor))
    (let ((since (tuatara.runtime:milliseconds-since (editor-settings-looked editor))))
      (if (< since *frame-at-least-every*)
          (/ (- *frame-at-least-every* since) 1000)
          (progn
            (setf (editor-settings-looked editor) (get-internal-real-time))
            (when (settings-moved-p)
              (incf *display-generation*))
            nil)))))

(defun paint-other-frames (editor)
  "Paint each frame of EDITOR shown on a display of its own beside the one
worked in whose text may have changed since it was painted
\(FRAME-SHOWN-CHANGED-P), a setting changed moving the display generation on
\(LOOK-AT-SETTINGS), at most every *FRAME-AT-LEAST-EVERY* milliseconds and
never while keys wait: returns NIL, or the seconds after which it wants to
be asked again, a frame or a look having waited for its turn. With one frame,
or only frames waiting their turn on the display of the one worked in
\(FRAME-TAKING-TURNS-P), nothing is done."
  (let ((frames (editor-frames editor)))
    (when (rest frames)
      (let* ((current (editor-frame editor))
             (frontend (frame-frontend current)))
        (flet ((elsewhere-p (frame) (not (eq (frame-frontend frame) frontend))))
          (when (and (some #'elsewhere-p frames) (not (tuatara.frontend:frontend-input-pending-p frontend)))
            (let ((look (look-at-settings editor)))
              (if (some (lambda (frame) (and (elsewhere-p frame) (frame-shown-changed-p frame))) frames)
                  (let ((since (tuatara.runtime:milliseconds-since (editor-others-painted editor))))
                    (if (< since *frame-at-least-every*)
                        (/ (- *frame-at-least-every* since) 1000)
                        (progn
                          (dolist (frame frames)
                            (unless (or (not (elsewhere-p frame)) (frame-deleted frame)
                                        (not (frame-shown-changed-p frame)))
                              (call-in-frame editor frame (lambda () (redisplay-or-say editor)))
                              (note-frame-painted frame)))
                          (setf (editor-others-painted editor) (get-internal-real-time))
                          look)))
                  look))))))))

(defun editor-job-waker ()
  "A function waking the editor, for the thread of work begun now to call when
it has something new to show; NIL without an editor."
  (let ((editor *editor*))
    (and editor (lambda () (wake-editor editor)))))

(defmethod tuatara.runtime:host-job-started ((host editor-host) job)
  "Tie JOB, starting now, to the editor running: its thread wakes the editor
\(EDITOR-JOB-WAKER), unless it has a way of its own, and what it says is shown
in the frame it is begun in (THREAD-FRAME)."
  (let ((editor *editor*))
    (when editor
      (unless (tuatara.runtime:job-wake job)
        (setf (tuatara.runtime:job-wake job) (editor-job-waker)))
      (setf (tuatara.runtime:job-frame job) (editor-frame editor)))))

(defmethod tuatara.runtime:host-take-in-job ((host editor-host) job function)
  "Call FUNCTION, JOB's taking in, with the frame JOB was begun in the one
worked in while it is open, else the frame worked in, so that what the taking
in says shows there."
  (let ((editor *editor*))
    (if editor
        (progn
          ;; what the job brings in may show in any frame
          (incf *display-generation*)
          (call-in-frame editor (frame-or-current editor (tuatara.runtime:job-frame job)) function))
        (funcall function))))
