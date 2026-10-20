;;;; lisp/editing/soft-wrap.lisp --- Long lines wrapped at the fill column: soft-wrap-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's visual-line-mode, wrapping at the fill column as the
;;;; visual-fill-column package does, and as Helix's soft wrap with its
;;;; wrap-at-text-width: a line longer than the window is shown on as many rows as
;;;; it needs, broken at a word's edge, at the fill column (or the window's edge
;;;; when that is nearer); the text itself is not changed. The rows that go on
;;;; from the one above are marked in the gutter, where line numbers stand. On
;;;; where the buffer says so, else where its major mode says (*SOFT-WRAP-MODES*),
;;;; else as *GLOBAL-SOFT-WRAP-MODE* says. While it is on, the keys that move
;;;; up and down go by rows, and the row point is on is painted in :soft-wrap-row
;;;; over the line's :hl-line, each of which the init file may colour.

(in-package #:tuatara.defaults)

(define-display-toggle soft-wrap "Soft wrap"
  "Wrap the long lines of the current buffer at a word's edge, or stop: a line
longer than the window is shown on as many rows as it needs, at the fill column
(\\[set-fill-column]), or the window's edge when that is nearer; the text is
not changed. The rows that go on from the one above are marked in the gutter
(*SOFT-WRAP-INDICATOR*), and the mode line says Wrap. While it is on,
\\<normal>\\[next-line] and \\[previous-line] go by rows."
  :command soft-wrap-mode :lookup soft-wrap-p
  :global *global-soft-wrap-mode*
  :what "wrap long lines at a word's edge, at the fill column"
  :example "'((org-mode . t) (text-mode . t) (csv-mode . nil))"
  :mode-line "Wrap"
  :unless (tuatara.display:hex-view-p "in the hex view"))

(defvar *soft-wrap-at-fill-column* t
  "True to wrap long lines at the buffer's fill column (\\[set-fill-column],
*FILL-COLUMN*), or at the window's edge when that is nearer; NIL to wrap at the
window's edge always.")

(defvar *soft-wrap-indicator* "↪"
  "What the gutter shows beside a row that goes on from the one above when
lines wrap, in the :wrap-indicator face, where the line number would be; NIL
for nothing.")

(defun buffer-soft-wrap (buffer)
  "How BUFFER's long lines wrap, for the display: NIL for not at all, else at
a word's edge, at the fill column (*SOFT-WRAP-AT-FILL-COLUMN*) or T for the
window's edge."
  (and (soft-wrap-p buffer)
       (if *soft-wrap-at-fill-column* (buffer-fill-column buffer) t)))

(defun wrap-indicator-width (editor window)
  "Gutter columns for the wrap indicator alone: those of *SOFT-WRAP-INDICATOR*
while WINDOW's buffer wraps its lines and shows no line numbers (with them, the
indicator stands where a number would); 0 otherwise."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (if (and *soft-wrap-indicator* (soft-wrap-p buffer) (null (line-numbers-mode buffer)))
        (text-columns *soft-wrap-indicator*)
        0)))

(defun wrap-indicator-cell (editor window row line)
  "The wrap indicator beside ROW when it goes on from the one above (LINE is NIL)."
  (declare (ignore editor window row))
  (if line (values "" nil) (values *soft-wrap-indicator* :wrap-indicator)))

(defun soft-wrap-row-range (editor window rows)
  "The row of WINDOW that point is on, as a list of one (START END :soft-wrap-row)
range over ROWS, when its buffer wraps its lines and highlights the line point
is on (HL-LINE-P): painted over the line's :hl-line, so that the two may be
coloured apart. NIL otherwise, and in windows not selected."
  (let ((buffer (tuatara.display:window-buffer window))
        (point (tuatara.display:window-point window)))
    (when (and (eq window (editor-window editor)) (soft-wrap-p buffer) (hl-line-p buffer))
      (loop for row across rows
            when (or (tuatara.display:row-column-of row point)
                     (and (<= (tuatara.display:row-start row) point) (< point (tuatara.display:row-end row))))
              ;; the row's line end too, or past the end of the text, so that the face goes on to the edge
              return (list (list (tuatara.display:row-start row)
                                 (+ (tuatara.display:row-end row) (if (tuatara.display:row-last-p row) 1 0))
                                 :soft-wrap-row))))))

(defun install-soft-wrap ()
  "Wrap the lines of the buffers that want it, mark the rows that go on in
the gutter, and paint the row point is on."
  (setf tuatara.display:*soft-wrap-function* 'buffer-soft-wrap)
  (add-gutter-provider :wrap-indicator #'wrap-indicator-width #'wrap-indicator-cell :first t)
  ;; after the line's highlight in the list, so painted over it
  (setf *line-highlight-functions*
        (append (remove 'soft-wrap-row-range *line-highlight-functions*) (list 'soft-wrap-row-range))))
