;;;; lisp/selecting/wrapping.lisp --- The selection wrapped in a pair of delimiters, or unwrapped.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defun wrap-selection (char)
  "Put the pair CHAR belongs to around the selection, or take it off when it
stands just outside it already (WRAP-BETWEEN); point on the first character of
what the pair holds, the selection over. One change for undo. A rectangle has
each of its rows wrapped, or unwrapped where every row is (WRAP-RECTANGLE)."
  (let ((b (current-buffer)))
    (if (rectangle-live-p b)
        (wrap-rectangle char)
        (multiple-value-bind (start end) (selection-range)
          (let ((at (wrap-between b start end char)))
            (setf (tuatara.buffer:buffer-mark-active-p b) nil)
            (goto at)
            (line-selection-clear))))))

(define-command wrap-selection-with-key ()
  "Wrap the selection in the delimiter pressed and its partner, or take them
off when they stand just outside it: \\`\"' on a word quotes it, and \\`\"' on
the word of a quoted word unquotes it. Either half of a pair will do: \\`)'
wraps in ( and ). Nothing inside is escaped; a count is ignored. With no
selection the key does nothing. On a rectangle, each row of the block is
wrapped, or unwrapped when the pair stands outside every row. The pairs are
*pair-delimiters*."
  (let* ((key (editor-this-key *editor*))
         (char (and key (tuatara.frontend:key-base key))))
    (multiple-value-bind (start end) (selection-range)
      (if (and start (characterp char))
          (if (< start end)
              (wrap-selection char)
              (message "The selection is empty"))
          (message "~a is undefined" (pressed-key-name))))))
