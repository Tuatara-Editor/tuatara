;;;; lisp/editing/display-fill-column.lisp --- A line down the window at the fill column.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's display-fill-column-indicator-mode and Helix's rulers: the last
;;;; column a filled line may reach is painted down the window in
;;;; :display-fill-column, over the text's backgrounds and under the line point
;;;; is on, so that where lines are filled and wrapped can be seen; the text is
;;;; not changed and nothing wraps. On where the buffer says so, else where its
;;;; major mode says (*DISPLAY-FILL-COLUMN-MODES*), else as
;;;; *GLOBAL-DISPLAY-FILL-COLUMN-MODE* says.

(in-package #:tuatara.defaults)

(define-display-toggle display-fill-column "Fill column line"
  "Mark the current buffer's fill column (\\[set-fill-column]) with a line down
the window, or stop: the last column a filled line may reach, the 80th for a
fill column of 80, painted in the :display-fill-column face over the text's
backgrounds (a source block's, the selection's) and under the line point is
on. The text is not changed and nothing wraps; \\[soft-wrap-mode] wraps
long lines there."
  :command display-fill-column-mode :lookup display-fill-column-p
  :global *global-display-fill-column-mode*
  :what "mark the fill column with a line down the window"
  :example "'((prog-mode . t) (csv-mode . nil))"
  :unless (tuatara.display:hex-view-p "in the hex view"))

(defun fill-column-line-columns (window)
  "The columns of WINDOW's text marked by a line down it, each (COLUMN FACE):
the last one a filled line may reach, in :display-fill-column, while its
buffer wants the line (DISPLAY-FILL-COLUMN-P); else none."
  (let ((buffer (tuatara.display:window-buffer window)))
    (and (display-fill-column-p buffer)
         (list (list (max 0 (1- (buffer-fill-column buffer))) :display-fill-column)))))

(defun install-display-fill-column ()
  "Mark the fill column in the windows whose buffers want it."
  (pushnew 'fill-column-line-columns *ruler-functions*))
