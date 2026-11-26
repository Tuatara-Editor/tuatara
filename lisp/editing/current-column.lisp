;;;; lisp/editing/current-column.lisp --- The column point is in, highlighted: current-column-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Helix's cursorcolumn and Vim's cursorcolumn: the screen column the
;;;; cursor is in, painted down the selected window in :current-column, the
;;;; :current-line face's colour unless set apart, over the text's backgrounds (a
;;;; source block's, the selection's) and under the line point is on, past the
;;;; end of short lines too, as the line down the window at the fill column is
;;;; (display-fill-column.lisp). Off in every buffer unless the init file says
;;;; otherwise; a buffer, or the list for its major mode
;;;; (*CURRENT-COLUMN-MODES*), may choose for itself, as with current-line-mode.

(in-package #:tuatara.defaults)

(define-display-toggle current-column "Current column"
  "Highlight the column point is in, down the window, in the current buffer, or
stop: in the :current-column face (the :current-line face's colour unless set
apart), over the backgrounds of the text (a source block's, the selection's)
and under the line point is on, past the end of short lines too, in the
selected window."
  :command current-column-mode :lookup current-column-p
  :global *global-current-column-mode*
  :what "highlight the column point is in, down the window"
  :example "'((prog-mode . t) (csv-mode . nil))"
  :unless (tuatara.display:hex-view-p "in the hex view"))

(defun current-column-columns (window)
  "The column point is in, as WINDOW's rulers have them, a list of one (COLUMN
:current-column), COLUMN counted from the line's start as the window's text
is scrolled sideways: while WINDOW is the selected window, its buffer wants
the column (CURSOR-COLUMN-P) and point is shown; else none."
  (when (and *editor* (eq window (editor-window *editor*))
             (current-column-p (tuatara.display:window-buffer window)))
    (multiple-value-bind (row column) (tuatara.display:window-point-position window)
      (when row
        (list (list (+ column (if (tuatara.display:window-wraps-p window) 0 (tuatara.display:window-hscroll window)))
                    :current-column))))))

(defun install-current-column ()
  "Paint the column point is in, in the windows whose buffers want it."
  (pushnew 'current-column-columns *ruler-functions*))
