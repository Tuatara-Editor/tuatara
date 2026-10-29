;;;; lisp/editing/cursor-column.lisp --- The column point is in, highlighted: cursor-column-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Helix's cursorcolumn and Vim's cursorcolumn: the screen column the
;;;; cursor is in, painted down the selected window in :cursor-column, the
;;;; :hl-line face's colour unless set apart, over the text's backgrounds (a
;;;; source block's, the selection's) and under the line point is on, past the end of short lines too, as the line down the window at the fill
;;;; column is (visual-line.lisp). Off in every buffer unless the init file
;;;; says otherwise; a buffer, or the list for its major mode
;;;; (*CURSOR-COLUMN-MODES*), may choose for itself, as with hl-line-mode.

(in-package #:tuatara.defaults)

(define-display-toggle cursor-column "Cursor column"
  "Highlight the column point is in, down the window, in the current buffer, or
stop: in the :cursor-column face (the :hl-line face's colour unless set
apart), over the backgrounds of the text (a source block's, the selection's)
and under the line point is on, past the end of short lines too, in the
selected window."
  :command cursor-column-mode :lookup cursor-column-p
  :global *global-cursor-column-mode*
  :what "highlight the column point is in, down the window"
  :example "'((prog-mode . t) (csv-mode . nil))"
  :unless (tuatara.display:hex-view-p "in the hex view"))

(defun cursor-column-columns (window)
  "The column point is in, as WINDOW's rulers have them, a list of one (COLUMN
:cursor-column), COLUMN counted from the line's start as the window's text
is scrolled sideways: while WINDOW is the selected window, its buffer wants
the column (CURSOR-COLUMN-P) and point is shown; else none."
  (when (and *editor* (eq window (editor-window *editor*))
             (cursor-column-p (tuatara.display:window-buffer window)))
    (multiple-value-bind (row column) (tuatara.display:window-point-position window)
      (when row
        (list (list (+ column (if (tuatara.display:window-wraps-p window) 0 (tuatara.display:window-hscroll window)))
                    :cursor-column))))))

(defun install-cursor-column ()
  "Paint the column point is in, in the windows whose buffers want it."
  (pushnew 'cursor-column-columns *ruler-functions*))
