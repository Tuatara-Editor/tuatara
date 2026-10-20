;;;; lisp/selecting/paragraphs.lisp --- Moving by paragraphs and sentences, and marking words, paragraphs and sentences.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands behave as Emacs's of the same names: a paragraph ends at a
;;;; blank line, a sentence at . ? ! or … followed by a space or a line end
;;;; (two spaces, where sentences are two spaces apart: *sentence-end-double-space*).

(in-package #:tuatara.defaults)

(defun paragraph-motion (buffer position count)
  "Where COUNT paragraphs from POSITION take point: forward to the blank line
after them, or back to the blank line before them when COUNT is negative."
  (if (minusp count)
      (paragraphs-backward buffer position (- count))
      (paragraphs-forward buffer position count)))

(defun sentence-motion (buffer position count)
  "Where COUNT sentences from POSITION take point: forward to their ends, or
back to their starts when COUNT is negative."
  (if (minusp count)
      (sentences-backward buffer position (- count))
      (sentences-forward buffer position count)))

(define-command forward-paragraph ()
  "Move point to the end of the paragraph: the start of the blank line after
it, or the end of the text. With a count, that many paragraphs; a negative
count moves back."
  (goto (paragraph-motion (current-buffer) (point) (prefix))))

(define-command backward-paragraph ()
  "Move point to the start of the paragraph: the start of the blank line
before it, or the start of the text. With a count, that many paragraphs; a
negative count moves forward."
  (goto (paragraph-motion (current-buffer) (point) (- (prefix)))))

(define-command forward-sentence ()
  "Move point to the end of the sentence: after its . ? ! or … and any closing
quotes or brackets, where a space or a line end follows (two spaces, a tab or
a line end where sentences are two spaces apart, *SENTENCE-END-DOUBLE-SPACE*),
or to the end of the paragraph's text. With a count, that many sentences; a negative count moves
back."
  (goto (sentence-motion (current-buffer) (point) (prefix))))

(define-command backward-sentence ()
  "Move point to the start of the sentence: after the end of the one before,
or the start of the paragraph's text. With a count, that many sentences; a
negative count moves forward."
  (goto (sentence-motion (current-buffer) (point) (- (prefix)))))

(defun extending-mark-p (command)
  "The mark, when the region is active or the last command was COMMAND, so
that the mark is moved on from where it is; else NIL."
  (let* ((b (current-buffer)) (mark (tuatara.buffer:buffer-mark b)))
    (and mark
         (or (eq (editor-last-command *editor*) command) (tuatara.buffer:buffer-mark-active-p b))
         mark)))

(defun activate-mark-at (position)
  "Set the mark at POSITION and activate the region."
  (let ((b (current-buffer)))
    (setf (tuatara.buffer:buffer-mark b) position
          (tuatara.buffer:buffer-mark-active-p b) t)))

(define-command mark-word ()
  "Set the mark at the end of the next word, point staying, and activate the
region; with a count, that many words. Again, or with the region active, move
the mark one word further from point (back, when it is before point)."
  (let* ((b (current-buffer))
         (mark (extending-mark-p 'mark-word))
         (from (or mark (point)))
         (count (or (prefix-argument) (if (and mark (< mark (point))) -1 1))))
    (let ((to (if (minusp count) (words-backward b from (- count)) (words-forward b from count))))
      ;; a new mark puts the one before on the mark ring
      (if mark (activate-mark-at to) (push-mark :position to :activate t)))))

(define-command mark-paragraph ()
  "Select the paragraph at point: point at the blank line before it (or the
start of the text), the mark at the blank line after it. With a count, that
many paragraphs. Again, or with the region active, move the mark on by one
more paragraph."
  (let ((b (current-buffer)) (count (prefix)) (mark (extending-mark-p 'mark-paragraph)))
    (cond ((zerop count))
          (mark (activate-mark-at (paragraph-motion b mark count)))
          (t (let ((end (paragraph-motion b (point) count)))
               (push-mark :position end :activate t)
               (goto (paragraph-motion b end (- count))))))))

(define-command mark-end-of-sentence ()
  "Set the mark at the end of the sentence, point staying, and activate the
region; with a count, that many sentences. Again at once, move the mark on by
one more sentence."
  (let* ((b (current-buffer))
         (mark (and (eq (editor-last-command *editor*) 'mark-end-of-sentence) (tuatara.buffer:buffer-mark b))))
    (let ((to (sentence-motion b (or mark (point)) (prefix))))
      (if mark (activate-mark-at to) (push-mark :position to :activate t)))))
