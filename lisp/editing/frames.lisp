;;;; lisp/editing/frames.lisp --- Frame commands: open, close, go to, and a file or buffer in another frame.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A frame is a window of Tuatara's own on the desktop, with windows of its
;;;; own inside; every frame shows the same buffers (src/command/frames.lisp).

(in-package #:tuatara.defaults)

(define-command make-frame ()
  "Open another frame showing the current buffer, a window of its own on the
desktop, and work in it. Each frame has its own windows, echo area and
questions; the buffers are the same in every frame, a buffer shown in two of
them one buffer with a place of its own in each. \\[delete-frame] closes it,
\\[other-frame] goes from one to the next. In a terminal there is one frame:
start Tuatara with --gui for more."
  (open-frame *editor*))

(define-command delete-frame ()
  "Close the frame worked in, with its windows; its buffers stay, shown in the
other frames as before. The last frame stays: \\[save-buffers-kill-editor]
quits. A frame's close button does the same."
  (let ((editor *editor*))
    (unless (rest (editor-frames editor))
      (refuse "This is the only frame; ~a quits" (substitute-command-keys "\\[save-buffers-kill-editor]")))
    (remove-frame editor (editor-frame editor))))

(define-command delete-other-frames ()
  "Close every frame but the one worked in. Their buffers stay."
  (let ((editor *editor*))
    (dolist (frame (editor-frames editor))
      (unless (eq frame (editor-frame editor))
        (remove-frame editor frame)))))

(define-command other-frame ()
  "Go to the next frame and bring its window before the others; with a count
N, the Nth frame on, back when N is negative. The frames are taken in the
order they were opened, the first again after the last."
  (let* ((editor *editor*)
         (frames (editor-frames editor)))
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
  (loop for buffer in (append (rest *startup-buffers*) (make-list (max 0 (- count (length *startup-buffers*)))))
        repeat (1- count)
        do (open-frame editor :buffer buffer :select nil)))

(defparameter *frame-bindings*
  '(("C-x 5 2" make-frame) ("C-x 5 0" delete-frame) ("C-x 5 1" delete-other-frames) ("C-x 5 o" other-frame)
    ("C-x 5 f" find-file-other-frame) ("C-x 5 b" switch-to-buffer-other-frame))
  "The frame keys of the global map, under \\`C-x 5': open, close and go to
frames, and a file or a buffer in another frame.")

(defun install-frames ()
  "Bind the frame keys (*FRAME-BINDINGS*)."
  (define-keys *global-keymap* *frame-bindings*))
