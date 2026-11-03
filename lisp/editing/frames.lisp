;;;; lisp/editing/frames.lisp --- Frame commands: open, close, go to, and a file or buffer in another frame.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A frame is a window of Tuatara's own on the desktop, with windows of its
;;;; own inside, or in a terminal one of the screens that take turns on it;
;;;; every frame shows the same buffers (src/command/frames.lisp).

(in-package #:tuatara.defaults)

(define-command make-frame ()
  "Open another frame showing the current buffer and work in it: a window of
its own on the desktop, or in a terminal a whole screen of its own shown in
place of the one before, the frames taking turns on the terminal, the mode
line naming the one shown (F1, F2). Each frame has its own windows, echo area
and questions; the buffers are the same in every frame, a buffer shown in two
of them one buffer with a place of its own in each. \\[delete-frame] closes
it, \\[other-frame] goes from one to the next. In a window of an editor
running in the background (tuatara --client --gui), the new window opens a
moment later, once the client has opened it, and the echo area says
\"Opening a window\" meanwhile; nothing waits for it."
  (open-frame *editor*))

(define-command delete-frame ()
  "Close the frame worked in, with its windows; its buffers stay, shown in the
other frames as before. In a terminal the first of the frames left is shown
in its place. The last frame stays: \\[save-buffers-kill-editor] quits, or
in a client of an editor running in the background closes that client. A
frame's close button does the same."
  (let ((editor *editor*))
    (unless (rest (client-frames editor (current-client editor)))
      (refuse "This is the only frame; ~a ~:[quits~;closes this client~]"
              (substitute-command-keys "\\[save-buffers-kill-editor]") (client-remote (current-client editor))))
    (remove-frame editor (editor-frame editor))))

(define-command delete-other-frames ()
  "Close every frame but the one worked in; in a client of an editor running
in the background, every other frame of that client. Their buffers stay."
  (let ((editor *editor*))
    (dolist (frame (client-frames editor (current-client editor)))
      (unless (eq frame (editor-frame editor))
        (remove-frame editor frame)))))

(define-command other-frame ()
  "Go to the next frame and bring its window before the others, or in a
terminal show it in place of the frame shown; with a count N, the Nth frame
on, back when N is negative. The frames are taken in the order they were
opened, the first again after the last; in a client of an editor running in
the background, that client's frames."
  (let* ((editor *editor*)
         (frames (client-frames editor (current-client editor))))
    (if (null (rest frames))
        (message "There is no other frame")
        (let ((next (nth (mod (+ (position (editor-frame editor) frames) (prefix)) (length frames)) frames)))
          (select-frame editor next)
          (tuatara.frontend:frontend-raise (editor-frontend editor))))))

(define-command find-file-other-frame ()
  "Visit a file in another frame and work in it, asking for its name as
\\[find-file] does; a folder is listed there. Opened as \\[find-file] opens
it, safely where it opens safely."
  (find-file-reading nil :prompt "Find file in other frame: "
                         :show (lambda (buffer) (open-frame *editor* :buffer buffer))))

(define-command switch-to-buffer-other-frame ()
  "Show a buffer in another frame and work in it, completing on the live
buffers as \\[switch-to-buffer] does, the ones used lately first; a new name
makes one."
  (let ((name (read-buffer "Switch to buffer in other frame: ")))
    (unless (zerop (length name))
      (open-frame *editor* :buffer (find-or-make-buffer name)))))

(defun open-start-frames (editor count)
  "Open frames of EDITOR as it starts, COUNT in all with the first: the Nth
showing the Nth of the files it was started with while there are files, else
what the first shows; those left over stay open behind the first. The first
frame stays the one worked in."
  (let ((started (client-startup-buffers (current-client editor))))
    (loop for buffer in (append (rest started) (make-list (max 0 (- count (length started)))))
          repeat (1- count)
          do (open-frame editor :buffer buffer :select nil))))

(defparameter *frame-bindings*
  '(("C-x 5 2" make-frame) ("C-x 5 0" delete-frame) ("C-x 5 1" delete-other-frames) ("C-x 5 o" other-frame)
    ("C-x 5 f" find-file-other-frame) ("C-x 5 b" switch-to-buffer-other-frame))
  "The frame keys of the global map, under \\`C-x 5': open, close and go to
frames, and a file or a buffer in another frame.")

(defun install-frames ()
  "Bind the frame keys (*FRAME-BINDINGS*)."
  (define-keys *global-keymap* *frame-bindings*))
