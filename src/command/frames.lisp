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
of the other, at the display's size now, and painted whole, and the display
names it in the events it delivers (TUATARA.FRONTEND:FRONTEND-FRAME). Returns
FRAME."
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
        ;; what is typed now is typed in FRAME; the display may have been resized while
        ;; FRAME waited; its title is the other's
        (setf (tuatara.frontend:frontend-frame (frame-frontend frame)) frame
              (frame-title-shown frame) nil)
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
one worked in, the first of those left is made so, but for a frame closed
while it asks a question, which stays so until the question is given up, as
it is at once (READ-EVENT). The last frame is never taken: that is refused,
as an editor has a frame for as long as it runs. Its events still to come
are dropped (TAKE-TO-ITS-FRAME); what was still to be said in it is said in
the frame worked in, and work handed on to be done in it (DEFER-ACTION) is
done there, or in the first of those left while it stays the one worked in.
Its frontend is stopped unless another frame takes turns on it, which the
frontend then names in its events in its place."
  (let ((left (remove frame (editor-frames editor))))
    (unless left
      (refuse "The editor's only frame stays while the editor runs"))
    ;; a frame closed while it asks stays the one worked in until its question is given up (READ-EVENT)
    (when (and (eq frame (editor-frame editor)) (not (frame-minibuffer-active frame)) (not *key-question*))
      (select-frame editor (first left)))
    (let ((heir (if (eq frame (editor-frame editor)) (first left) (editor-frame editor))))
      (setf (editor-frames editor) left
            (frame-deleted frame) t
            (frame-deferred heir) (append (frame-deferred heir)
                                          (remove-if-not #'functionp (shiftf (frame-deferred frame) '())))))
    (let* ((frontend (frame-frontend frame))
           (sharing (find frontend left :key #'frame-frontend)))
      (cond ((null sharing)
             (tuatara.runtime:ignoring-errors-reported ("closing a frame's display")
               (tuatara.frontend:frontend-stop frontend)))
            ((eq (tuatara.frontend:frontend-frame frontend) frame)
             (setf (tuatara.frontend:frontend-frame frontend) sharing))))
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

(defun paint-other-frames (editor &key now)
  "Paint each frame of EDITOR shown on a display of its own beside the one
worked in whose text may have changed since it was painted
\(FRAME-SHOWN-CHANGED-P), a setting changed moving the display generation on
\(LOOK-AT-SETTINGS), at most every *FRAME-AT-LEAST-EVERY* milliseconds, or
with NOW at once, and never while keys wait: returns NIL, or the seconds
after which it wants to be asked again, a frame or a look having waited for
its turn. With one frame, or only frames waiting their turn on the display
of the one worked in (FRAME-TAKING-TURNS-P), nothing is done."
  (let ((frames (editor-frames editor)))
    (when (rest frames)
      (let* ((current (editor-frame editor))
             (frontend (frame-frontend current)))
        (flet ((elsewhere-p (frame) (not (eq (frame-frontend frame) frontend))))
          (when (and (some #'elsewhere-p frames) (not (tuatara.frontend:frontend-input-pending-p frontend)))
            (let ((look (look-at-settings editor)))
              (if (some (lambda (frame) (and (elsewhere-p frame) (frame-shown-changed-p frame))) frames)
                  (let ((since (tuatara.runtime:milliseconds-since (editor-others-painted editor))))
                    (if (and (not now) (< since *frame-at-least-every*))
                        (/ (- *frame-at-least-every* since) 1000)
                        (progn
                          (dolist (frame frames)
                            (unless (or (not (elsewhere-p frame)) (frame-deleted frame)
                                        (not (frame-shown-changed-p frame)))
                              (paint-other-frame editor frame)))
                          (setf (editor-others-painted editor) (get-internal-real-time))
                          look)))
                  look))))))))

(defvar *buffers-asked-about* '()
  "While a question asks about buffers beside the one its command works in
\(which of them to save, say), those buffers. A command asking so binds it
around the question: while the question waits for another frame, those
buffers are kept as it asks about them (*BUFFERS-HELD-BY-QUESTIONS*), and the
question is given up when one of them is killed.")

(defvar *buffers-held-by-questions* '()
  "While questions wait for what is done in other frames to run inside them
\(RUN-INSIDE-QUESTION), each buffer one of them is about, with the frame it
asks in, as (BUFFER . FRAME): an edit of one is refused
\(GUARD-BUFFERS-HELD-BY-QUESTIONS), a job of one waits (HOST-JOB-WAITS-P),
and work in the background on one is put off, until its question is
answered.")

(define-condition buffer-held-by-question (tuatara.runtime:refusal) ()
  (:documentation "An edit refused because a question waiting in another frame is about the
buffer (GUARD-BUFFERS-HELD-BY-QUESTIONS)."))

(defun buffer-held-by-question-p (buffer)
  "True when a question waiting in a frame is about BUFFER: what changes it is
put off until the question is answered (*BUFFERS-HELD-BY-QUESTIONS*)."
  (and *buffers-held-by-questions* (assoc buffer *buffers-held-by-questions*) t))

(defun guard-buffers-held-by-questions (buffer edits)
  "Before-edit function (TUATARA.BUFFER:*BEFORE-EDIT-FUNCTIONS*): refuse EDITS
of BUFFER while a question waiting in another frame is about it, saying in
which frame (*BUFFERS-HELD-BY-QUESTIONS*): the question holds places in its
text, which the edits would move."
  (declare (ignore edits))
  (let ((held (and *buffers-held-by-questions* (assoc buffer *buffers-held-by-questions*))))
    (when held
      (error 'buffer-held-by-question :format-control "~a has a question open in ~a"
                                      :format-arguments (list (tuatara.buffer:buffer-name buffer)
                                                              (frame-name (cdr held)))))))

(pushnew 'guard-buffers-held-by-questions tuatara.buffer:*before-edit-functions*)

(defvar *question-variables*
  '(*key-question* *minibuffer-hidden* *minibuffer-question-keymap* *minibuffer-finish* *minibuffer-history*
    *minibuffer-initial-selected* *read-key-replay* *keys-as-typed* *command-notes* *command-undo-since*
    *this-command-keys* *current-buffer* *dropped* *pasted* tuatara.buffer:*narrowing-in-force*
    tuatara.runtime:*secret-at-hand* *background-work* *buffers-asked-about*)
  "The variables a command binds that belong to the question it asks, and so
to the frame it asks in: what is done in another frame inside that question
\(RUN-INSIDE-QUESTION) runs with each as it is outside every command, and
the frame asking is painted meanwhile with its own (FRAME-HELD). A package
that binds a variable of its own while it asks, which its commands or its
drawing read, adds it here: (pushnew '*my-question-state* *question-variables*).")

(defun outside-question-values ()
  "The value each of *QUESTION-VARIABLES* has outside every command, in order."
  (mapcar #'sb-ext:symbol-global-value *question-variables*))

(defun call-with-question-of (frame function)
  "Call FUNCTION with *QUESTION-VARIABLES* as FRAME's question has them while
it waits (FRAME-HELD), else as they are outside every command; return what
it returns."
  (let ((held (frame-held frame)))
    (if held
        (progv (first held) (second held) (funcall function))
        (progv *question-variables* (outside-question-values) (funcall function)))))

(defun paint-other-frame (editor frame)
  "Paint FRAME, one of EDITOR's frames other than the one worked in, a
question waiting in it shown as it asks (CALL-WITH-QUESTION-OF), and keep
what it shows (NOTE-FRAME-PAINTED)."
  (call-in-frame editor frame (lambda () (call-with-question-of frame (lambda () (redisplay-or-say editor)))))
  (note-frame-painted frame))

(defvar *taking-in-inside-question* nil
  "True while work in the background is taken in inside an open question
\(RUN-INSIDE-QUESTION): a job begun in a frame whose question waits, or of
the buffer such a question is about, waits for that question then.")

(defmethod tuatara.runtime:host-job-waits-p ((host editor-host) job)
  "While work is taken in inside an open question (*TAKING-IN-INSIDE-QUESTION*),
true for JOB begun in a frame whose question waits, or of a buffer such a
question is about (BUFFER-HELD-BY-QUESTION-P): what it takes in could change
what that question asks about, so it is taken in once the question is
answered."
  (let ((editor *editor*))
    (and *taking-in-inside-question* editor
         (or (buffer-held-by-question-p (tuatara.runtime:job-buffer job))
             (let ((frame (tuatara.runtime:job-frame job)))
               (and frame (frame-held frame) t))))))

(defun run-free-frames-deferred (editor asking)
  "Do what waits to be done in the frames of EDITOR whose question does not
wait (FRAME-DEFERRED), each with its frame the one worked in, oldest first in
each frame, until none waits; but not in those taking turns on the display
of ASKING, the frame whose question is answered (FRAME-TAKING-TURNS-P),
whose work waits for it, so that the display goes over to another frame only
when you go there."
  (loop for frame = (find-if (lambda (frame)
                               (and (frame-deferred frame) (not (frame-held frame))
                                    (not (eq (frame-frontend frame) (frame-frontend asking)))))
                             (editor-frames editor))
        while frame
        do (let ((item (pop (frame-deferred frame))))
             (call-in-frame editor frame (lambda () (handle-event-guarded editor item))))))

(defun run-inside-question (editor frame event)
  "Handle EVENT, done in FRAME, one of EDITOR's frames, while the frame worked
in asks a question; with no EVENT, only take in work in the background. With
the variables of the question as they are outside every command
\(*QUESTION-VARIABLES*), EVENT runs in FRAME as the command loop runs it, a
question of its own asked in turn; then, as between commands, what work in
the background has for the editor is taken in (TAKE-IN-BACKGROUND), but for
the jobs a waiting question holds back (HOST-JOB-WAITS-P), and what waits to
be done in the frames whose question does not wait is done
\(RUN-FREE-FRAMES-DEFERRED). The frame asking is the one worked in again
after, its question going on, and the frames whose text changed are painted.
Meanwhile the buffers the question is about, the one its command works in and
*BUFFERS-ASKED-ABOUT*, are held as it asks about them
\(*BUFFERS-HELD-BY-QUESTIONS*). That question is given up, as \\`C-g' gives
it up, when what ran stopped the editor or killed a buffer the question is
about; when it closed the frame asking, once the question reads on
\(READ-EVENT)."
  (let ((asking (editor-frame editor))
        (about (adjoin (current-buffer) *buffers-asked-about*))
        (this-command (editor-this-command editor))
        (room (< (1+ (count-if #'frame-held (editor-frames editor))) *frames-question-depth*))
        (generation *display-generation*)
        (painted nil))
    (setf (frame-held asking) (list *question-variables* (mapcar #'symbol-value *question-variables*) about))
    (unwind-protect
         (progv (list* '*buffers-held-by-questions* *question-variables*)
             (list* (append (mapcar (lambda (buffer) (cons buffer asking)) about) *buffers-held-by-questions*)
                    (outside-question-values))
           (when event
             (select-frame editor frame)
             (setf *last-input-time* (get-internal-real-time))
             (handle-event-guarded editor event)
             ;; the frame the command ended in, drawn as the command loop draws it, but never one whose question waits
             (let ((ended (editor-frame editor)))
               (unless (or (frame-deleted ended) (frame-held ended)
                           (tuatara.frontend:frontend-input-pending-p (frame-frontend ended)))
                 (redisplay-or-say editor)
                 (setf painted ended))))
           (unless (frame-deleted asking)
             (select-frame editor asking))
           (let ((*taking-in-inside-question* t))
             (take-in-background editor))
           (when room
             (run-free-frames-deferred editor asking)))
      (setf (frame-held asking) nil
            (editor-this-command editor) this-command)
      (if (frame-deleted asking)
          (setf (editor-frame editor) asking)
          (select-frame editor asking)))
    (cond ((frame-deleted asking)) ; given up as its question reads on
          ((not (editor-running editor)) (keyboard-quit-signal))
          ((notevery #'live-buffer-p about)
           (keyboard-quit-signal (format nil "~a was killed in another frame: the question about it is given up"
                                         (tuatara.buffer:buffer-name (find-if-not #'live-buffer-p about)))))
          ((or event (/= generation *display-generation*))
           (when painted (note-frame-painted painted))
           (unless (tuatara.frontend:frontend-input-pending-p (frame-frontend asking))
             (redisplay-or-say editor))
           (paint-other-frames editor :now t)))))

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
