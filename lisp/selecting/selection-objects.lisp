;;;; lisp/selecting/selection-objects.lisp --- Selecting a word, a symbol, a sentence, a paragraph or what a pair holds.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each m key selects an object with point at its start and the mark at its
;;;; end. Pressed again at once, or after another of the family, it grows the
;;;; selection by one more object: the forward keys push the mark on, the
;;;; backward keys walk point back.

(in-package #:tuatara.defaults)

;;; The family of keys that grow one selection

(defparameter *selection-object-commands*
  '(select-word select-word-backward select-symbol select-symbol-backward
    select-sentence select-sentence-backward select-paragraph select-paragraph-backward
    mark-whole-buffer mark-run-line-forward mark-run-line-backward mark-run-adopt)
  "The commands that grow one selection between them: pressed right after one
another, each adds its own object at its own end.")

(defparameter *mark-run-adjusters*
  '(mark-run-left mark-run-right mark-run-down mark-run-up
    mark-run-line-start mark-run-line-end mark-run-buffer-start mark-run-buffer-end
    mark-run-exchange mark-run-step-back mark-run-step-forward select-whole-buffer)
  "The mark run's keys that adjust its selection rather than mark an object:
after one of them the object keys still grow the selection, while it shows.")

(defun selection-extending-p (&optional companions)
  "True when this press should grow the selection the last one made: the same
command ran last, or one of COMPANIONS, and the mark is set."
  (let ((last (editor-last-command *editor*)) (this (editor-this-command *editor*)))
    (and this (or (eq last this) (member last companions))
         (tuatara.buffer:buffer-mark (current-buffer))
         t)))

(defparameter *mark-run-inert* '(mark-run-refuse tuatara-normal-inert)
  "Commands that change nothing, after which a selection still grows: a key
refused in a mark run, \\<normal>\\[tuatara-normal-inert] in NORMAL state.")

(defun selection-continuing-p ()
  "True when an object key continues the selection the family made, which is
then squared up: point at its start, the mark at its far end. After a mark
run's adjusting key, only while the selection shows."
  (when (and (selection-extending-p (append *selection-object-commands* *mark-run-adjusters* *mark-run-inert*))
             (or (not (member (editor-last-command *editor*) *mark-run-adjusters*))
                 (tuatara.buffer:buffer-mark-active-p (current-buffer))))
    (let* ((b (current-buffer)) (m (tuatara.buffer:buffer-mark b)) (p (point)))
      (when (> p m)
        (setf (tuatara.buffer:buffer-mark b) p)
        (goto m)))
    t))

(defvar *selection-reach* :ahead
  "Which neighbour a fresh object key takes from the gap between two: :ahead
for the forward keys, :behind for the backward ones.")

(defun object-count ()
  "The prefix argument as a number of objects: none and zero are one."
  (let ((n (or (prefix-argument) 1)))
    (if (and (integerp n) (/= n 0)) n 1)))

;;; Characters

(defun blank-char-p (char)
  "True for spaces, tabs and line ends."
  (member char '(#\Space #\Tab #\Newline #\Return)))

;;; Words

(defun object-start-at (buffer position test)
  "The start of the run of characters passing TEST that POSITION is in; for
words (WORD-CHAR-P) read by their octets, refused past *TEXT-SCAN-LIMIT*."
  (if (eq test #'word-char-p)
      (let* ((scan (max 0 (- position *text-scan-limit*)))
             (p (word-run-start buffer position t scan)))
        (when (and (= p scan) (plusp scan)) (error "No start of the word within ~a" (scan-limit-words)))
        p)
      (let ((p position))
        (loop while (and (plusp p) (funcall test (char-before-position buffer p)))
              do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
        p)))

(defun reach-object (buffer position test)
  "From POSITION in the gap between objects whose characters pass TEST, the
position of the object *SELECTION-REACH* takes (the other side's when that one
has none); POSITION itself when it is on one; NIL when BUFFER has none."
  (flet ((ahead ()
           (if (eq test #'word-char-p)
               ;; words read by their octets, as far as *TEXT-SCAN-LIMIT*
               (let* ((size (tuatara.buffer:buffer-size buffer))
                      (scan (min size (+ position *text-scan-limit*)))
                      (p (word-run-end buffer position nil scan)))
                 (and (< p scan) p))
               (let ((p position))
                 (loop while (and (tuatara.buffer:buffer-char-at buffer p) (not (funcall test (tuatara.buffer:buffer-char-at buffer p))))
                       do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
                 (and (tuatara.buffer:buffer-char-at buffer p) p))))
         (behind ()
           (if (eq test #'word-char-p)
               (let* ((scan (max 0 (- position *text-scan-limit*)))
                      (p (word-run-start buffer position nil scan)))
                 (and (> p scan) (tuatara.buffer:buffer-forward-char buffer p -1)))
               (let ((p position))
                 (loop while (and (plusp p) (not (funcall test (char-before-position buffer p))))
                       do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
                 (and (plusp p) (tuatara.buffer:buffer-forward-char buffer p -1))))))
    (if (funcall test (tuatara.buffer:buffer-char-at buffer position))
        position
        (if (eq *selection-reach* :behind)
            (or (behind) (ahead))
            (or (ahead) (behind))))))

(define-command select-word ()
  "Select the word at point; from the gap between two words, the one ahead
(the last one at the end of the text). Pressed again at once, or after
another of the \\`m' keys that select objects, grow the selection by one more
word. With a count, that many words; a negative count, words before."
  (let ((b (current-buffer)) (n (object-count)))
    (if (selection-continuing-p)
        (activate-mark-at (words-forward b (tuatara.buffer:buffer-mark b) (max 1 n)))
        (let ((on (reach-object b (point) #'word-char-p)))
          (unless on (error "No word at or before point"))
          (let ((start (object-start-at b on #'word-char-p)))
            (if (plusp n)
                (select-bounds (current-buffer) start (words-forward b start n))
                (select-bounds (current-buffer) (words-backward b start (- n)) (words-forward b (words-backward b start 1) 1))))))
    (message "Word marked")))

(defun select-backward (motion forward label)
  "A backward object key: grow the selection back by the count's objects
with MOTION (a function of the buffer, a position and a count), or, fresh,
select as FORWARD does from the gap's other side, then walk back the rest."
  (let ((b (current-buffer)) (n (max 1 (object-count))))
    (if (selection-continuing-p)
        (progn (goto (funcall motion b (point) n))
               (setf (tuatara.buffer:buffer-mark-active-p b) t))
        (progn
          (let ((*selection-reach* :behind)
                (argument (prefix-argument)))
            (setf (editor-prefix-argument *editor*) nil)
            (unwind-protect (funcall forward)
              (setf (editor-prefix-argument *editor*) argument)))
          (when (> n 1)
            (goto (funcall motion b (point) (1- n))))))
    (message "~a marked" label)))

(define-command select-word-backward ()
  "Select the word at point, or grow a word selection backward, as
select-word grows it forward: from the gap between two words, the one
behind. \\<normal>\\[select-word] \\[select-word]
\\[select-word-backward] on \"that\" in \"for text that is not saved\"
selects \"text that is\"."
  (select-backward #'words-backward #'select-word "Word"))

;;; Symbols

(defun trim-trailing-punctuation (buffer start end)
  "END moved back over punctuation stuck to the end of a symbol (a comma or
full stop after a name in prose), never back past START."
  (let ((p end))
    (loop while (and (> p start)
                     (let ((c (char-before-position buffer p)))
                       (and c (or (find c ",.")
                                  (and (not (symbol-char-p c buffer)) (not (blank-char-p c))
                                       (not (find c "()[]{}\"'`")))))))
          do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
    (if (> p start) p end)))

(defun lisp-buffer-p (buffer)
  "True when BUFFER is in Lisp mode, where a quote, a backquote or # before a
form belongs to it."
  (typep (buffer-major-mode buffer) 'lisp-mode))

(defun trim-leading-punctuation (buffer start end)
  "START moved forward over punctuation stuck to the front of a symbol (the
quotes around a name in prose), a quote or # before a Lisp form excepted,
never up to END."
  (let ((p start) (lisp (lisp-buffer-p buffer)))
    (loop while (and (< p end)
                     (let ((c (tuatara.buffer:buffer-char-at buffer p)))
                       (and c (not (symbol-char-p c buffer)) (not (blank-char-p c))
                            (not (find c "([{\""))
                            (not (and lisp (find c "'`#,@"))))))
          do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
    (if (< p end) p start)))

(defun symbols-forward (buffer position count)
  "Where COUNT balanced expressions forward of POSITION end, punctuation at
the end left out."
  (let ((p position))
    ;; as forward-sexp moves (SEXP-END-AT); where it cannot, no further
    (dotimes (i count) (setf p (or (sexp-end-at buffer p) p)))
    (trim-trailing-punctuation buffer position p)))

(defun symbols-backward (buffer position count)
  "Where COUNT balanced expressions back from POSITION begin, punctuation in
front left out."
  (let ((p position))
    (dotimes (i count) (setf p (or (sexp-start-at buffer p) p)))
    (trim-leading-punctuation buffer p position)))

(define-command select-symbol ()
  "Select the symbol at point (a name with its _ and -, Lisp's own
characters in Lisp), punctuation at either end left out: from the gap
between two, the one ahead, brackets and quotes crossed. Pressed again at
once, or after another object key, grow the selection by one more balanced
expression. With a count, that many."
  (let ((b (current-buffer)) (n (object-count)))
    (if (selection-continuing-p)
        (activate-mark-at (symbols-forward b (tuatara.buffer:buffer-mark b) (max 1 n)))
        (let* ((test (lambda (c) (symbol-char-p c b)))
               (on (reach-object b (point) test)))
          (unless on (error "No symbol at or before point"))
          (let* ((start (let ((s (object-start-at b on test)))
                          ;; in Lisp, the quote or # before a symbol is part of it
                          (when (lisp-buffer-p b)
                            (loop while (and (plusp s) (find (char-before-position b s) "'`#,@"))
                                  do (setf s (tuatara.buffer:buffer-forward-char b s -1))))
                          s))
                 (end (let ((p (object-start-at b on test)))
                        (loop while (funcall test (tuatara.buffer:buffer-char-at b p))
                              do (setf p (tuatara.buffer:buffer-forward-char b p)))
                        p)))
            (if (plusp n)
                (select-bounds (current-buffer) start (if (> n 1) (symbols-forward b end (1- n)) (trim-trailing-punctuation b start end)))
                (select-bounds (current-buffer) (symbols-backward b start (- n)) (symbols-forward b (symbols-backward b start 1) 1))))))
    (message "Symbol marked")))

(define-command select-symbol-backward ()
  "Select the symbol at point, or grow a symbol selection backward: from the
gap between two, the one behind."
  (select-backward #'symbols-backward #'select-symbol "Symbol"))

;;; Paragraphs, and sentences within them

(defun blank-line-at-p (buffer position)
  "True when the line holding POSITION has nothing but spaces and tabs."
  (blank-range-p buffer (tuatara.buffer:buffer-line-start buffer position) (tuatara.buffer:buffer-line-end buffer position)))

(defun paragraph-too-far ()
  "Signal that no paragraph break was found within *TEXT-SCAN-LIMIT*."
  (error "No paragraph break within ~a" (scan-limit-words)))

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
            until (>= p (tuatara.text:rope-bytes text))
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
        (let ((text-line (upward (tuatara.buffer:buffer-line-start buffer (tuatara.buffer:buffer-forward-char buffer p -1)) nil)))
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

(defun absorb-paragraph-blank (buffer start end)
  "END moved past one blank line after it when the paragraph selection from
START has none before it, so that exactly one blank line comes along."
  (if (and (not (blank-line-at-p buffer start)) (< end (tuatara.buffer:buffer-size buffer))
           (blank-line-at-p buffer end))
      (next-line-start buffer end)
      end))

(define-command select-paragraph ()
  "Select the paragraph at point with one blank line, the one before it (the
one after, for the first paragraph), so that deleting it leaves one blank line
between its neighbours: from a blank line between two, the one below. Pressed
again at once, or after another object key, grow by one more paragraph."
  (let ((b (current-buffer)) (n (object-count)) (origin (point)))
    (if (selection-continuing-p)
        (let ((start (point)))
          (activate-mark-at (absorb-paragraph-blank b start (paragraphs-forward b (tuatara.buffer:buffer-mark b) (max 1 n)))))
        (let ((start (if (and (blank-line-at-p b origin) (eq *selection-reach* :behind) (text-before-p b origin))
                         (paragraphs-backward b origin 1)
                         (paragraphs-backward b (paragraphs-forward b origin 1) 1))))
          (let ((end (absorb-paragraph-blank b start (paragraphs-forward b start (max 1 n)))))
            (when (blank-range-p b start end)
              (goto origin)
              (error "No paragraph at or before point"))
            (select-bounds (current-buffer) start end))))
    (message "Paragraph marked")))

(define-command select-paragraph-backward ()
  "Select the paragraph at point, or grow a paragraph selection backward:
from a blank line between two, the one above."
  (select-backward #'paragraphs-backward #'select-paragraph "Paragraph"))

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
        (loop while (and (> p position) (blank-char-p (char-before-position buffer p)))
              do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
        (max p position)))))

(defun paragraph-text-start (buffer position &optional within)
  "The start of the text of the paragraph POSITION is in or after: past the
blank lines and blanks before it. With WITHIN, NIL when finding it takes
reading more than that many octets (PARAGRAPHS-BACKWARD)."
  (let ((p (paragraphs-backward buffer position 1 within)))
    (when p
      (loop while (and (< p (tuatara.buffer:buffer-size buffer)) (blank-char-p (tuatara.buffer:buffer-char-at buffer p)))
            do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
      p)))

(defun next-sentence-end (buffer position)
  "Where the first sentence ending after POSITION ends (SENTENCE-END-AT), a
paragraph's end aside; NIL when none does within *TEXT-SCAN-LIMIT* octets."
  (let* ((text (tuatara.buffer:buffer-text buffer))
         (limit (min (tuatara.text:rope-bytes text) (+ position *text-scan-limit*))))
    (loop with q = position
          do (let ((candidate (tuatara.text:rope-find-any-octet text *sentence-end-octets* :start q :end limit)))
               (unless candidate (return nil))
               (let ((end (sentence-end-at buffer candidate)))
                 (when (and end (> end position)) (return end)))
               (setf q (1+ candidate))))))

(defun sentences-forward (buffer position count)
  "Where COUNT sentences forward of POSITION end: at the next full stop
(or ? ! …) and its closers followed by a blank (two spaces where sentences
are two spaces apart: SENTENCE-END-AT), or the end of the paragraph's text
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
              (setf floor (and back (paragraph-text-start buffer back (and at (+ 4096 (max 0 (- (tuatara.buffer:buffer-line-end buffer back) at)))))))))
          (setf p (cond ((and floor (or (null at) (< at floor))) (min floor p))
                        (start start)
                        (t (min (or floor 0) p)))))))))

(define-command select-sentence ()
  "Select the sentence at point, as far as its full stop; from the gap
between two, the one ahead. Pressed again at once, or after another object
key, grow by one more sentence. With a count, that many sentences."
  (let ((b (current-buffer)) (n (max 1 (object-count))) (origin (point)))
    (if (selection-continuing-p)
        (activate-mark-at (sentences-forward b (tuatara.buffer:buffer-mark b) n))
        (let ((start (sentences-backward b (sentences-forward b origin 1) 1)))
          (when (and (eq *selection-reach* :behind) (> start origin) (text-before-p b origin))
            (setf start (sentences-backward b start 1)))
          (let ((end (sentences-forward b start n)))
            (when (or (>= start end) (blank-range-p b start end))
              (goto origin)
              (error "No sentence at or before point"))
            (select-bounds (current-buffer) start end))))
    (message "Sentence marked")))

(define-command select-sentence-backward ()
  "Select the sentence at point, or grow a sentence selection backward: from
the gap between two, the one behind."
  (select-backward #'sentences-backward #'select-sentence "Sentence"))

;;; Pairs of delimiters

(defun read-pair-delimiter ()
  "The pair to select within, as (OPEN CLOSE ON-OPENER): the one at point
when point is on a delimiter, else the one whose half is typed."
  (let* ((b (current-buffer))
         (here (tuatara.buffer:buffer-char-at b (point)))
         (on-opener (and here (assoc here (pair-delimiters)) t))
         (open (or (and here (pair-open-for here))
                   (let* ((key (read-key "Delimiter (see *pair-delimiters*): "))
                          (char (tuatara.frontend:key-base key)))
                     (when (quit-key-p key) (keyboard-quit-signal))
                     (or (and (characterp char) (pair-open-for char))
                         (error "Unsupported delimiter `~a'; see *pair-delimiters*"
                                (tuatara.frontend:key-description (tuatara.frontend:key-spec key))))))))
    (list open (cdr (assoc open (pair-delimiters))) (and on-opener (eql here open)))))

(defun single-octet-p (char)
  "True when CHAR is written in one octet (ASCII), so that its octets can be
looked for alone, fast: no other character's octets can be taken for it."
  (< (char-code char) 128))

(defun scan-pair-forward (buffer from open close)
  "The position after the CLOSE balancing an OPEN before FROM, nested pairs
of the same kind counted, or NIL; NIL too once *TEXT-SCAN-LIMIT* has been
looked through."
  (if (and (single-octet-p open) (single-octet-p close))
      (tuatara.text:rope-bracket-match (tuatara.buffer:buffer-text buffer) from :inside t
                                       :openers (string open) :closers (string close)
                                       :limit *text-scan-limit*)
      (let ((depth 1) (p from) (steps 0))
        (loop (let ((c (tuatara.buffer:buffer-char-at buffer p)))
                (cond ((or (null c) (> (incf steps) *text-scan-limit*)) (return nil))
                      ((char= c open) (incf depth))
                      ((char= c close) (when (zerop (decf depth)) (return (tuatara.buffer:buffer-forward-char buffer p))))))
              (setf p (tuatara.buffer:buffer-forward-char buffer p))))))

(defun scan-pair-backward (buffer from open close)
  "The position of the OPEN balancing a CLOSE at or after FROM, nested pairs
counted, or NIL; NIL too once *TEXT-SCAN-LIMIT* has been looked through."
  (if (and (single-octet-p open) (single-octet-p close))
      (tuatara.text:rope-bracket-match (tuatara.buffer:buffer-text buffer) from :backward t :inside t
                                       :openers (string open) :closers (string close)
                                       :limit *text-scan-limit*)
      (let ((depth 1) (p from) (steps 0))
        (loop (when (or (zerop p) (> (incf steps) *text-scan-limit*)) (return nil))
              (setf p (tuatara.buffer:buffer-forward-char buffer p -1))
              (let ((c (tuatara.buffer:buffer-char-at buffer p)))
                (cond ((char= c close) (incf depth))
                      ((char= c open) (when (zerop (decf depth)) (return p)))))))))

(defun find-char (buffer from char direction)
  "The position of the next CHAR from FROM (:forward, at or after FROM) or the
last before FROM (:backward), or NIL; NIL too once *TEXT-SCAN-LIMIT* has
been looked through."
  (if (single-octet-p char)
      (let ((text (tuatara.buffer:buffer-text buffer)) (code (char-code char)))
        (if (eq direction :forward)
            (tuatara.text:rope-find-octet text code :start from
                                                    :end (min (tuatara.buffer:buffer-size buffer)
                                                              (+ from *text-scan-limit*)))
            (tuatara.text:rope-find-octet-backward text code :end from :start (max 0 (- from *text-scan-limit*)))))
      (let ((steps 0))
        (if (eq direction :forward)
            (loop for p = from then (tuatara.buffer:buffer-forward-char buffer p)
                  for c = (tuatara.buffer:buffer-char-at buffer p)
                  while (and c (<= (incf steps) *text-scan-limit*))
                  when (char= c char) return p)
            (loop for p = from then q
                  for q = (and (plusp p) (tuatara.buffer:buffer-forward-char buffer p -1))
                  while (and q (<= (incf steps) *text-scan-limit*))
                  when (char= (tuatara.buffer:buffer-char-at buffer q) char) return q)))))

(defun pair-around (buffer position open close on-opener)
  "(START . END) of the pair of OPEN and CLOSE around POSITION: START at the
opener, END after the closer; on an opener, that one's pair."
  (let ((symmetric (char= open close)))
    (flet ((fail (text) (error text (if (search "after" text) close open))))
      (if on-opener
          (let ((end (if symmetric
                         (let ((q (find-char buffer (tuatara.buffer:buffer-forward-char buffer position) close :forward)))
                           (and q (tuatara.buffer:buffer-forward-char buffer q)))
                         (scan-pair-forward buffer (tuatara.buffer:buffer-forward-char buffer position) open close))))
            (cond (end (cons position end))
                  ((not symmetric) (fail "No matching '~a' found after cursor"))
                  (t (let ((start (find-char buffer position open :backward)))
                       (if start
                           (cons start (tuatara.buffer:buffer-forward-char buffer position))
                           (fail "No matching '~a' found before cursor"))))))
          (let ((start (if (eql (tuatara.buffer:buffer-char-at buffer position) open)
                           position
                           (if symmetric
                               (find-char buffer position open :backward)
                               (scan-pair-backward buffer position open close)))))
            (unless start (fail "No '~a' found near cursor"))
            (let ((end (if symmetric
                           (let ((q (find-char buffer (tuatara.buffer:buffer-forward-char buffer start) close :forward)))
                             (and q (tuatara.buffer:buffer-forward-char buffer q)))
                           (scan-pair-forward buffer (tuatara.buffer:buffer-forward-char buffer start) open close))))
              (unless end (fail "No matching '~a' found after cursor"))
              (cons start end)))))))

(defun pair-around-level (buffer position open close on-opener levels)
  "(START . END) of the LEVELSth pair of OPEN and CLOSE out from POSITION:
each level the pair around the one before; for a delimiter that opens and
closes alike, each level one more of it on either side."
  (let ((span (pair-around buffer position open close on-opener)))
    (if (char= open close)
        (loop repeat (1- levels)
              do (let ((start (find-char buffer (car span) open :backward))
                       (end (let ((q (find-char buffer (cdr span) close :forward)))
                              (and q (tuatara.buffer:buffer-forward-char buffer q)))))
                   (unless (and start end) (error "No enclosing `~a' beyond that level" open))
                   (setf span (cons start end))))
        (loop repeat (1- levels)
              do (when (zerop (car span)) (error "No enclosing `~a' beyond that level" open))
                 (setf span (handler-case (pair-around buffer (tuatara.buffer:buffer-forward-char buffer (car span) -1)
                                                       open close nil)
                              (error () (error "No enclosing `~a' beyond that level" open))))))
    span))

(defun select-pair-around (inner)
  "Select within the pair of delimiters around point (INNER) or with it."
  (let* ((b (current-buffer))
         (state (and (selection-extending-p) (tuatara.buffer:buffer-local b 'select-pair-state)))
         (anchor (if state (first state) (point)))
         (spec (if state (rest state) (read-pair-delimiter)))
         (level (+ (if state (fourth spec) 0) (max 1 (object-count)))))
    (destructuring-bind (open close on-opener &rest ignore) spec
      (declare (ignore ignore))
      (let* ((span (pair-around-level b anchor open close on-opener level))
             (start (if inner (tuatara.buffer:buffer-forward-char b (car span)) (car span)))
             (end (if inner (tuatara.buffer:buffer-forward-char b (cdr span) -1) (cdr span))))
        (when (>= start end)
          (error "Empty selection between ~a and ~a" open close))
        (select-bounds (current-buffer) start end)
        (setf (tuatara.buffer:buffer-local b 'select-pair-state) (list anchor open close on-opener level))
        (message (if inner "Selected content for '~a'" "Selected the pair '~a' and what it holds") open)))))

(define-command select-inside-pair ()
  "Select what is inside a pair of delimiters, the delimiters left out: the
pair of the delimiter at point, or the one whose half is typed next ((, ), [,
\", ', =, * and the rest of *pair-delimiters*). A pair nested in one
of its kind is told apart. With a count, or pressed again at once, the next
pair out; for quotes and the like, the next of them out on either side."
  (select-pair-around t))

(define-command select-pair ()
  "Select a pair of delimiters and what it holds, as select-inside-pair finds it."
  (select-pair-around nil))
