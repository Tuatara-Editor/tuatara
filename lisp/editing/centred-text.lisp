;;;; lisp/editing/centred-text.lisp --- The text shown in a column in the middle of the window: centred-text-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's olivetti-mode (by Paul W. Rankin) shows a buffer: its text in a
;;;; column of *CENTRED-TEXT-WIDTH* columns in the middle of the window, blank
;;;; margins either side, for writing and reading with nothing else about. The
;;;; text itself is not changed; long lines are cut, or wrap, at the column's
;;;; edge as they would at the window's. The left margin is a gutter before the
;;;; others (line numbers stand at the column's edge), and the buffer tells the
;;;; layout the column's width. On where the buffer says so (M-x
;;;; centred-text-mode), else where its major mode says (*CENTRED-TEXT-MODES*),
;;;; else as *GLOBAL-CENTRED-TEXT-MODE* says.

(in-package #:tuatara.defaults)

(define-setting *centred-text-width* 80
  "How many columns of text a buffer shows when its text is centred with
\\[centred-text-mode]. The rest of the window is blank margins either side.
Set it in the init file.
- a whole number, 1 or more: the width of the column of text, in columns."
  :values (integer 1 *))

(define-display-toggle centred-text "Centred text"
  "Show the text of the current buffer in a column of *CENTRED-TEXT-WIDTH*
columns in the middle of the window, blank margins either side, or as it was:
for writing and reading with nothing else about. The mode line says Centred.
Long lines are cut, or wrap (\\[soft-wrap-mode]), at the column's edge."
  :command centred-text-mode :lookup centred-text-p
  :global *global-centred-text-mode*
  :what "show the text in a column in the middle of the window"
  :example "'((org-mode . t) (text-mode . t))"
  :mode-line "Centred"
  :unless (tuatara.display:hex-view-p "in the hex view"))

(defun centred-text-pad (editor window)
  "Gutter columns before the centred text of WINDOW: half of what the window
has over the column and the other gutters; 0 when the text is not centred,
or the window has no room over the column. The buffer's :text-width, the
column's width the layout keeps to, follows whether its text is centred."
  (let* ((buffer (tuatara.display:window-buffer window))
         (centred (centred-text-p buffer))
         (width (and centred *centred-text-width*)))
    (unless (eql width (tuatara.buffer:buffer-local buffer :text-width))
      (setf (tuatara.buffer:buffer-local buffer :text-width) width))
    (if centred
        ;; the margin takes a column between the gutters and the text
        (max 0 (floor (- (tuatara.display:window-width window) (gutter-width-except editor window :centred-text)
                         1 *centred-text-width*)
                      2))
        0)))

(defun centred-text-cell (editor window row line)
  "The margin beside ROW: blank."
  (declare (ignore editor window row line))
  (values "" nil))

(defun install-centred-text ()
  "Register the margin before the other gutters."
  (add-gutter-provider :centred-text #'centred-text-pad #'centred-text-cell :first t))
