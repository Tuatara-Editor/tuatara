;;;; lisp/editing/current-line.lisp --- The line point is on, highlighted: current-line-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; current-line-mode and global-current-line-mode: the line point is on,
;;;; every row of it when it wraps, in the :current-line face out to the window's
;;;; edge, in the selected window, under the syntax colours and the selection.
;;;; On in every buffer unless the init file says otherwise; a buffer, or
;;;; the list for its major mode (*CURRENT-LINE-MODES*), may choose for itself.

(in-package #:tuatara.defaults)

(define-display-toggle current-line "Current line"
  "Highlight the line point is on in the current buffer, or stop: every row of
it, out to the window's edge, in the :current-line face, under the syntax colours
and the selection, in the selected window."
  :command current-line-mode :lookup current-line-p
  :global *global-current-line-mode* :default t
  :what "highlight the line point is on"
  :example "'((csv-mode . nil))")

(defun line-rows-range (rows position face)
  "The line holding POSITION, as a list of one (START END FACE) range over ROWS,
the rows of it shown, its line end included so that FACE goes on to the
window's edge (past the end of the text on the last line); NIL when ROWS do
not show it."
  (let ((n (length rows)))
    (when (plusp n)
      ;; the last row that starts at or before POSITION
      (let ((i (1- (tuatara.runtime:bisect (mid 0 n) (<= (tuatara.display:row-start (aref rows mid)) position)))))
        (when (>= i 0)
          (let ((first i) (last i))
            ;; the rows of a wrapped line
            (loop while (and (plusp first) (tuatara.display:row-continuation-p (aref rows first)))
                  do (decf first))
            (loop while (and (< (1+ last) n) (tuatara.display:row-continuation-p (aref rows (1+ last))))
                  do (incf last))
            (let ((start (tuatara.display:row-start (aref rows first)))
                  (end (+ (tuatara.display:row-end (aref rows last))
                          (if (tuatara.display:row-last-p (aref rows last)) 1 0))))
              (when (and (< start end) (<= position end))
                (list (list start end face))))))))))

(defun current-line-range (editor window rows)
  "The line point is on in WINDOW, as a list of one (START END :current-line) range
over ROWS, the rows of it shown there, its line end included so that the face
goes on to the window's edge; NIL unless WINDOW is the selected window and its
buffer highlights the line (HL-LINE-P)."
  (when (and (eq window (editor-window editor)) (current-line-p (tuatara.display:window-buffer window)))
    (line-rows-range rows (tuatara.display:window-point window) :current-line)))

(defun install-current-line ()
  "Paint the line point is on under everything else."
  (pushnew 'current-line-range *line-highlight-functions*))
