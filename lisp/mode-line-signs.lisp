;;;; lisp/mode-line-signs.lisp --- The signs of work before [-] in the mode line, lit and faint.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each sign stands for one kind of work: P for the buffer's jobs, U and S
;;;; for the directories kept read being read and searched, D for a daemon.
;;;; A sign is lit while its work goes on, and for a moment after it began or
;;;; ended (NOTE-SIGN-WORK), else faint, in a colour a little off the mode
;;;; line's ground that follows the theme.

(in-package #:tuatara.defaults)

(defparameter *sign-lit-seconds* 1/2
  "The least time a sign of the mode line stays lit once its work begins or
ends, so that work done in a moment is seen too.")

(defvar *signs-lit-until* (make-hash-table :test 'eq :synchronized t)
  "For each sign of the mode line, the internal real time it stays lit until.")

(sb-ext:defglobal **sign-fader** (list nil)
  "True in its car while a thread waits to wake the frontend once the signs
lit last are to be drawn faint (NOTE-SIGN-WORK): one at most, however many
signs are lit meanwhile.")

(defun note-sign-work (sign &optional editor)
  "Note that the work SIGN (a keyword: :indexing, :searching) shows began or
ended now, from any thread, so that its sign is lit for *SIGN-LIT-SECONDS*
however quick the work; wake EDITOR, when given, to show it lit now and
again once the moment is over, to show it faint, a question in the minibuffer
open or not. One thread at most waits for that moment, for every sign lit."
  (setf (gethash sign *signs-lit-until*) (tuatara.runtime:deadline-in *sign-lit-seconds*))
  (when editor
    (wake-editor editor)
    (when (null (sb-ext:compare-and-swap (car **sign-fader**) nil t))
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (flet ((left ()
                  (let ((latest 0))
                    (maphash (lambda (sign until) (declare (ignore sign)) (setf latest (max latest until)))
                             *signs-lit-until*)
                    (tuatara.runtime:seconds-left latest))))
           ;; until the last sign lit is due to fade, however often they were lit again
           (loop
             (let ((left (left)))
               (when (plusp left) (sleep left)))
             (wake-editor editor)
             (setf (car **sign-fader**) nil)
             ;; one lit as this one let go waits on here, unless another thread took it
             (unless (and (plusp (left)) (null (sb-ext:compare-and-swap (car **sign-fader**) nil t)))
               (return)))))
       :name "tuatara sign fading"))))

(defparameter *sign-idle-fraction* 1/6
  "How far the mode line's signs before [-], while their work is not going on,
stand from its ground towards its text: 0 the ground itself, unseen, 1 the
text's colour.")

(sb-ext:defglobal **sign-colours-generation** nil
  "The face generation (TUATARA.DISPLAY:*FACE-GENERATION*) the idle sign's
colour was last worked out for (FOLLOW-MODE-LINE-COLOURS).")

(defun follow-mode-line-colours ()
  "Give :mode-line-sign-idle a colour a little off the mode line's ground
\(*SIGN-IDLE-FRACTION* towards its text), from the mode line's colours as the
theme in force has them, so that any theme, or a mode line you coloured, gets
a sign that matches. Left alone when you set the face yourself, under a theme
without colours, or where the mode line's colours are not known. Worked out
again only once the faces have changed."
  (unless (or (eql **sign-colours-generation** tuatara.display:*face-generation*)
              (assoc :mode-line-sign-idle tuatara.display:*user-face-settings*)
              (let ((theme (tuatara.display:find-theme tuatara.display:*current-theme*)))
                (and theme (tuatara.display:theme-colourless theme))))
    (let* ((reverse (tuatara.display:face-attribute :mode-line :reverse))
           (fg (or (tuatara.display:face-attribute :mode-line :fg) (tuatara.display:face-attribute :default :fg)))
           (bg (or (tuatara.display:face-attribute :mode-line :bg) (tuatara.display:face-attribute :default :bg)))
           (ground (tuatara.display:colour-hex (if reverse fg bg)))
           (ink (tuatara.display:colour-hex (if reverse bg fg))))
      (when (and ground ink)
        (let ((colour (tuatara.display:mix-colours ground ink *sign-idle-fraction*)))
          (unless (equal colour (tuatara.display:face-attribute :mode-line-sign-idle :fg))
            (tuatara.display:set-face-attributes :mode-line-sign-idle :fg colour :dim nil))))))
  (setf **sign-colours-generation** tuatara.display:*face-generation*))

(defun sign-lit-p (sign working)
  "True while SIGN, a keyword naming a sign of the mode line, is to be lit:
while WORKING, and for *SIGN-LIT-SECONDS* after its work last began or ended
\(NOTE-SIGN-WORK)."
  (or working (not (tuatara.runtime:deadline-passed-p (gethash sign *signs-lit-until* 0)))))

(defun mode-line-sign (indicator working)
  "The part of the mode line for INDICATOR, a sign before [-]: lit in
:mode-line-sign while WORKING, else faint in :mode-line-sign-idle; NIL with
no indicator."
  (when (and indicator (plusp (length indicator)))
    (if working
        (list indicator :mode-line-sign)
        (progn (follow-mode-line-colours)
               (list indicator :mode-line-sign-idle)))))

(defun signs-tick (editor)
  "Tick function: while a sign of the mode line is lit, the seconds until the
next is to be drawn faint again, for the editor to look again then; NIL when
none is lit."
  (declare (ignore editor))
  (let ((now (get-internal-real-time)) (soonest nil))
    (maphash (lambda (sign until)
               (declare (ignore sign))
               (when (> until now)
                 (let ((seconds (/ (- until now) internal-time-units-per-second)))
                   (setf soonest (if soonest (min soonest seconds) seconds)))))
             *signs-lit-until*)
    soonest))
