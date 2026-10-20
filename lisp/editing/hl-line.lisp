;;;; lisp/editing/hl-line.lisp --- The line point is on, highlighted: hl-line-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's hl-line-mode and global-hl-line-mode: the line point is on,
;;;; every row of it when it wraps, in the :hl-line face out to the window's
;;;; edge, in the selected window, under the syntax colours and the selection.
;;;; On in every buffer unless the init file says otherwise; a buffer, or
;;;; the list for its major mode (*HL-LINE-MODES*), may choose for itself.

(in-package #:tuatara.defaults)

(define-display-toggle hl-line "Hl-line"
  "Highlight the line point is on in the current buffer, or stop: every row of
it, out to the window's edge, in the :hl-line face, under the syntax colours
and the selection, in the selected window."
  :command hl-line-mode :lookup hl-line-p
  :global *global-hl-line-mode* :default t
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
      (let ((i (let ((lo 0) (hi n))
                 (setf lo (tuatara.runtime:bisect (mid lo hi) (<= (tuatara.display:row-start (aref rows mid)) position)) hi lo)
                 (1- lo))))
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

(defun hl-line-range (editor window rows)
  "The line point is on in WINDOW, as a list of one (START END :hl-line) range
over ROWS, the rows of it shown there, its line end included so that the face
goes on to the window's edge; NIL unless WINDOW is the selected window and its
buffer highlights the line (HL-LINE-P)."
  (when (and (eq window (editor-window editor)) (hl-line-p (tuatara.display:window-buffer window)))
    (line-rows-range rows (tuatara.display:window-point window) :hl-line)))

(defun install-hl-line ()
  "Paint the line point is on under everything else."
  (pushnew 'hl-line-range *line-highlight-functions*))
