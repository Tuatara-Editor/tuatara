;;;; src/command/frames.lisp --- The editor's frames: the one worked in, and frames added and taken away.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A frame is one screen of the editor (editor.lisp). Commands never name
;;;; one: they work in the editor's current frame, and the command loop makes
;;;; current the frame each event happened in (TAKE-TO-ITS-FRAME, dispatch.lisp).
;;;; Every frame shares the buffers; each has its own windows, and so its own
;;;; point in a buffer it shows. The list of frames is read from other threads
;;;; (KEY-WAITING-P, HOST-BUFFER-SHOWN-P), so it is replaced, never changed in place.

(in-package #:tuatara.command)

(defun frames-windows (&optional (editor *editor*))
  "Every window of every frame of EDITOR, each frame's from the top left, those
of the frame worked in first: for what holds wherever a buffer is shown, as
a window keeping its place while the buffer changes."
  (let ((frames (editor-frames editor)))
    (if (null (rest frames))
        (editor-windows editor)
        (let ((current (editor-frame editor)))
          (append (editor-windows editor)
                  (loop for frame in frames
                        for window = (frame-window frame)
                        unless (or (eq frame current) (null window))
                          append (tuatara.display:area-windows (tuatara.display:area-root window))))))))

(defun select-frame (editor frame)
  "Make FRAME, one of EDITOR's frames, the one worked in. The selected window
of the frame left keeps its point, and that of FRAME moves its buffer's point
to its own, as selecting a window does (SELECT-WINDOW); what follows the
buffer shown is brought in line with FRAME's (*AFTER-TICK-FUNCTIONS*). Returns
FRAME."
  (let ((old (editor-frame editor)))
    (unless (eq old frame)
      (let ((leaving (frame-window old)))
        (when leaving (keep-window-point leaving)))
      (let ((entering (frame-window frame)))
        (when entering (give-window-buffer-point entering)))
      (setf (editor-frame editor) frame)
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

(defun add-frame (editor frontend)
  "Give EDITOR another frame, painted through FRONTEND, which is started, and
return it: as large as FRONTEND says, with one window showing the buffer of
the window worked in, at its point. FRONTEND delivers its events through the
source EDITOR's frames share, each naming the frame returned
\(TUATARA.FRONTEND:EVENT-FRAME). The frame worked in stays so."
  (tuatara.frontend:frontend-start frontend)
  (let ((frame (make-frame frontend))
        (buffer (tuatara.display:window-buffer (editor-window editor))))
    (multiple-value-bind (width height) (tuatara.frontend:frontend-size frontend)
      (setf (frame-width frame) (max 1 width) (frame-height frame) (max 3 height)
            (frame-window frame) (tuatara.display:make-window buffer :width (max 1 width)
                                                                     :height (max 1 (- height 2)))))
    (keep-window-point (frame-window frame))
    (setf (editor-frames editor) (append (editor-frames editor) (list frame)))
    (call-in-frame editor frame (lambda () (fit-windows editor)))
    frame))

(defun remove-frame (editor frame)
  "Take FRAME out of EDITOR's frames and stop its frontend; when it was the
one worked in, the first of those left is made so. The last frame is never
taken: that is refused, as an editor has a frame for as long as it runs. Its
events still to come are dropped (TAKE-TO-ITS-FRAME); what was still to be
said in it is said in the frame worked in, and work handed on to be done in
it (DEFER-ACTION) is done there."
  (let ((left (remove frame (editor-frames editor))))
    (unless left
      (refuse "The editor's only frame stays while the editor runs"))
    (when (eq frame (editor-frame editor))
      (select-frame editor (first left)))
    (setf (editor-frames editor) left
          (frame-deleted frame) t
          (editor-deferred editor) (append (editor-deferred editor)
                                           (remove-if-not #'functionp (shiftf (frame-deferred frame) '()))))
    (tuatara.runtime:ignoring-errors-reported ("closing a frame's display")
      (tuatara.frontend:frontend-stop (frame-frontend frame)))
    frame))

(defun editor-job-waker ()
  "A function waking the editor, for the thread of work begun now to call when
it has something new to show; NIL without an editor."
  (let ((editor *editor*))
    (and editor (lambda () (wake-editor editor)))))

(defmethod tuatara.runtime:host-job-started ((host tuatara.runtime:host) job)
  "Tie JOB, starting now, to the editor running: its thread wakes the editor
\(EDITOR-JOB-WAKER), unless it has a way of its own, and what it says is shown
in the frame it is begun in (THREAD-FRAME)."
  (let ((editor *editor*))
    (when editor
      (unless (tuatara.runtime:job-wake job)
        (setf (tuatara.runtime:job-wake job) (editor-job-waker)))
      (setf (tuatara.runtime:job-frame job) (editor-frame editor)))))

(defmethod tuatara.runtime:host-take-in-job ((host tuatara.runtime:host) job function)
  "Call FUNCTION, JOB's taking in, with the frame JOB was begun in the one
worked in while it is open, else the frame worked in, so that what the taking
in says shows there."
  (let ((editor *editor*))
    (if editor
        (call-in-frame editor (frame-or-current editor (tuatara.runtime:job-frame job)) function)
        (funcall function))))
