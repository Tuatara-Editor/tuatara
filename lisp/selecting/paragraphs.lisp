;;;; lisp/selecting/paragraphs.lisp --- Moving by paragraphs and sentences, and marking
;;;; words, paragraphs and sentences.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A paragraph ends at a blank line, a sentence at . ? ! or … followed by a
;;;; space or a line end (two spaces, where sentences are two spaces apart; see
;;;; *sentence-end-double-space*). The motions read the text as octets, fast,
;;;; and look no further than *text-scan-limit*.

(in-package #:tuatara.defaults)

;;; Paragraphs, and sentences within them

(defun blank-line-at-p (buffer position)
  "True when the line holding POSITION has nothing but spaces and tabs."
  (blank-range-p buffer
                 (tuatara.buffer:buffer-line-start buffer position)
                 (tuatara.buffer:buffer-line-end buffer position)))

(defun paragraph-too-far ()
  "Signal that no paragraph break was found within *TEXT-SCAN-LIMIT*."
  (refuse "No paragraph break within ~a" (scan-limit-words)))

(defun paragraphs-forward (buffer position count &optional within)
  "Where COUNT paragraphs forward of POSITION end: past blank lines, then past
lines of text, to the start of the blank line after them or the end of the
text. The lines are read as octets, fast; farther than *TEXT-SCAN-LIMIT* is an
error. With WITHIN, no more than that many octets are read, and NIL is the
answer when that is not enough."
  (let ((text (tuatara.buffer:buffer-text buffer))
        (p (tuatara.buffer:buffer-line-start buffer position)))
    (flet ((downward (line blank)
             (or (tuatara.text:rope-line-downward text line :blank blank :limit (or within *text-scan-limit*))
                 (if within (return-from paragraphs-forward nil) (paragraph-too-far)))))
      ;; at the end of the text it stops at once, whatever is left of COUNT
      (loop repeat count
            until (>= p (tuatara.text:rope-octet-length text))
            do (setf p (downward p nil) p (downward p t))))
    p))

(defun paragraphs-backward (buffer position count &optional within)
  "Where COUNT paragraphs back from POSITION begin: from the line before
POSITION's character, up past blank lines, then up through the text to the
start of the blank line before it, or the start of the text. Farther than
*TEXT-SCAN-LIMIT* is an error. With WITHIN, no more than that many octets are
read for each line looked for, and NIL is the answer when that is not
enough."
  (let ((text (tuatara.buffer:buffer-text buffer)) (p position))
    (flet ((upward (line blank)
             (multiple-value-bind (found why) (tuatara.text:rope-line-upward text line :blank blank
                                                                             :limit (or within *text-scan-limit*))
               (when (eq why :limit)
                 (if within (return-from paragraphs-backward nil) (paragraph-too-far)))
               found)))
      (dotimes (i count p)
        (when (zerop p) (return 0))
        (let ((text-line (upward (tuatara.buffer:buffer-line-start
                                  buffer (tuatara.buffer:buffer-forward-char buffer p -1))
                                 nil)))
          (setf p (if (or (null text-line) (zerop text-line))
                      0
                      (or (upward (tuatara.buffer:buffer-line-start buffer (1- text-line)) t) 0))))))))

(defun text-before-p (buffer position)
  "True when anything but blanks comes before POSITION in BUFFER; true too when
only blanks come for *TEXT-SCAN-LIMIT* before it."
  (let ((from (max 0 (- position *text-scan-limit*))))
    (or (plusp from)
        (and (tuatara.text:rope-find-any-octet (tuatara.buffer:buffer-text buffer) tuatara.text:*text-octets*
                                               :start from :end position :from-end t)
             t))))

(defun blank-range-p (buffer start end)
  "True when BUFFER holds only blanks from START below END."
  (null (tuatara.text:rope-find-any-octet (tuatara.buffer:buffer-text buffer) tuatara.text:*text-octets*
                                          :start start :end end)))

(defparameter *sentence-end-octets* (list (char-code #\.) (char-code #\?) (char-code #\!) #xe2)
  "The octets a sentence's end can start with: . ? ! and the first octet of
… and ‽ (and of other characters, which are then looked at and passed over).")

(defun sentence-end-at (buffer position)
  "When a sentence ends at POSITION's character, where it ends: after its .
? ! or …, and any closing quotes or brackets, when a space, a line end or the
end of the text follows; in a buffer whose sentences are two spaces apart
\(SENTENCE-END-DOUBLE-SPACE-P), two spaces, a tab, a line end or the end of
the text. Not after an abbreviation's full stop (ABBREVIATION-BEFORE-P: e.g.,
Dr.). Else NIL."
  (let ((c (tuatara.buffer:buffer-char-at buffer position)))
    (when (and c (sentence-end-char-p c))
      (let* ((stop (tuatara.buffer:buffer-forward-char buffer position))
             (p stop))
        (loop while (let ((c (tuatara.buffer:buffer-char-at buffer p))) (and c (sentence-closing-char-p c)))
              do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
        (let ((gap (sentence-gap (tuatara.buffer:buffer-char-at buffer p) (sentence-end-double-space-p buffer))))
          (and (if (eq gap :blank)
                   (let ((after (tuatara.buffer:buffer-char-at buffer (tuatara.buffer:buffer-forward-char buffer p))))
                     (or (null after) (blank-char-p after)))
                   gap)
               (not (and (char= c #\.) (abbreviation-before-p buffer stop)))
               p))))))

(defun paragraph-text-end (buffer position &optional within)
  "The end of the text of the paragraph at or after POSITION: before the line
end of its last line. With WITHIN, NIL when finding it takes reading more
than that many octets (PARAGRAPHS-FORWARD)."
  (let ((end (paragraphs-forward buffer position 1 within)))
    (when end
      (let ((p end))
        (loop while (and (> p position) (blank-char-p (buffer-char-before buffer p)))
              do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
        (max p position)))))

(defun paragraph-text-start (buffer position &optional within)
  "The start of the text of the paragraph POSITION is in or after: past the
blank lines and blanks before it. With WITHIN, NIL when finding it takes
reading more than that many octets (PARAGRAPHS-BACKWARD)."
  (let ((p (paragraphs-backward buffer position 1 within)))
    (when p
      (loop while (and (< p (tuatara.buffer:buffer-size buffer))
                       (blank-char-p (tuatara.buffer:buffer-char-at buffer p)))
            do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
      p)))

(defun next-sentence-end (buffer position)
  "Where the first sentence ending after POSITION ends (SENTENCE-END-AT), a
paragraph's end aside; NIL when none does within *TEXT-SCAN-LIMIT* octets."
  (let* ((text (tuatara.buffer:buffer-text buffer))
         (limit (min (tuatara.text:rope-octet-length text) (+ position *text-scan-limit*))))
    (loop with q = position
          do (let ((candidate (tuatara.text:rope-find-any-octet text *sentence-end-octets* :start q :end limit)))
               (unless candidate (return nil))
               (let ((end (sentence-end-at buffer candidate)))
                 (when (and end (> end position)) (return end)))
               (setf q (1+ candidate))))))

(defun sentences-forward (buffer position count)
  "Where COUNT sentences forward of POSITION end: at the next full stop
(or ? ! …) and its closers followed by a blank (two spaces where sentences
are two spaces apart; see SENTENCE-END-AT), or the end of the paragraph's text
when that comes first. Only the text up to that full stop is read for the
paragraph's end; farther than *TEXT-SCAN-LIMIT* is an error."
  (let ((p position))
    (dotimes (i count p)
      (let* ((end (next-sentence-end buffer p))
             ;; the paragraph's end matters only before that sentence's
             (within (and end (+ 4096 (- end (tuatara.buffer:buffer-line-start buffer p)))))
             (limit (paragraph-text-end buffer p within)))
        (when (and limit (<= limit p))
          (let ((next (next-line-start buffer p)))
            (setf limit (paragraph-text-end buffer next (and end (+ 4096 (max 0 (- end next))))))))
        (setf p (cond ((null limit) end)
                      ((null end) limit)
                      (t (min end limit))))))))

(defun previous-sentence-start (buffer position)
  "Two values for the sentence before POSITION: where the sentence after it
begins, past the blanks after its end, when that is before POSITION; and
where the character ending it is. NIL when no sentence ends within
*TEXT-SCAN-LIMIT* octets before POSITION, a paragraph's start aside."
  (let ((text (tuatara.buffer:buffer-text buffer))
        (floor (max 0 (- position *text-scan-limit*))))
    (loop with high = position
          do (let ((r (tuatara.text:rope-find-any-octet text *sentence-end-octets* :start floor :end high :from-end t)))
               (unless r (return nil))
               (let ((end (sentence-end-at buffer r)))
                 (when end
                   (let ((start (skip-blanks buffer end)))
                     (when (< start position) (return (values start r))))))
               (setf high r)))))

(defun sentences-backward (buffer position count)
  "Where COUNT sentences back from POSITION begin: just after the end of the
sentence before and the blanks after it, or else the start of the paragraph's
text when that comes later; from the start of a paragraph, the last sentence
of the one before. Only the text back to that sentence's end is read for the
paragraph's start; farther than *TEXT-SCAN-LIMIT* is an error."
  (let ((p position))
    (dotimes (i count p)
      (when (zerop p) (return 0))
      (multiple-value-bind (start at) (previous-sentence-start buffer p)
        (let* ((within (and at (+ 4096 (- (tuatara.buffer:buffer-line-end buffer p) at))))
               (floor (paragraph-text-start buffer p within)))
          (when (and floor (>= floor p))
            (let ((back (paragraphs-backward buffer p 1 within)))
              (setf floor (and back
                               (paragraph-text-start
                                buffer back
                                (and at (+ 4096 (max 0 (- (tuatara.buffer:buffer-line-end buffer back) at)))))))))
          (setf p (cond ((and floor (or (null at) (< at floor))) (min floor p))
                        (start start)
                        (t (min (or floor 0) p)))))))))

;;; Moving by them

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
