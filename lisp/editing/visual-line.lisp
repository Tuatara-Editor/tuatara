;;;; lisp/editing/visual-line.lisp --- A line down the window at the fill column: visual-line-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's display-fill-column-indicator-mode and Helix's rulers: the last
;;;; column a filled line may reach is painted down the window in :visual-line,
;;;; over the text's backgrounds and under the line point is on, so that where lines are filled and wrapped
;;;; can be seen; the text is not changed and nothing wraps. On where the buffer
;;;; says so, else where its major mode says (*VISUAL-LINE-MODES*), else as
;;;; *GLOBAL-VISUAL-LINE-MODE* says.

(in-package #:tuatara.defaults)

(define-display-toggle visual-line "Visual line"
  "Mark the current buffer's fill column (\\[set-fill-column]) with a line down
the window, or stop: the last column a filled line may reach, the 80th for a
fill column of 80, painted in the :visual-line face over the text's
backgrounds (a source block's, the selection's) and under the line point is
on. The text is not changed and nothing wraps; \\[soft-wrap-mode] wraps
long lines there."
  :command visual-line-mode :lookup visual-line-p
  :global *global-visual-line-mode*
  :what "mark the fill column with a line down the window"
  :example "'((prog-mode . t) (csv-mode . nil))"
  :unless (tuatara.display:hex-view-p "in the hex view"))

(defun visual-line-columns (window)
  "The columns of WINDOW's text marked by a line down it, each (COLUMN FACE):
the last one a filled line may reach, in :visual-line, while its buffer wants
the line (VISUAL-LINE-P); else none."
  (let ((buffer (tuatara.display:window-buffer window)))
    (and (visual-line-p buffer) (list (list (max 0 (1- (buffer-fill-column buffer))) :visual-line)))))

(defun install-visual-line ()
  "Mark the fill column in the windows whose buffers want it."
  (pushnew 'visual-line-columns *ruler-functions*))
