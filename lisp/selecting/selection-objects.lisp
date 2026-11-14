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

(defun selection-extending-p (&rest companion-lists)
  "True when this press should grow the selection the last one made: the same
command ran last, or one in a list of COMPANION-LISTS, and the mark is set."
  (let ((last (editor-last-command *editor*)) (this (editor-this-command *editor*)))
    (and this (or (eq last this) (some (lambda (companions) (member last companions)) companion-lists))
         (tuatara.buffer:buffer-mark (current-buffer))
         t)))

(defparameter *mark-run-inert* '(mark-run-refuse tuatara-normal-inert)
  "Commands that change nothing, after which a selection still grows: a key
refused in a mark run, \\<normal>\\[tuatara-normal-inert] in NORMAL state.")

(defun selection-continuing-p ()
  "True when an object key continues the selection the family made, which is
then squared up: point at its start, the mark at its far end. After a mark
run's adjusting key, only while the selection shows."
  (when (and (selection-extending-p *selection-object-commands* *mark-run-adjusters* *mark-run-inert*)
             (or (not (member (editor-last-command *editor*) *mark-run-adjusters*))
                 (tuatara.buffer:buffer-mark-active-p (current-buffer))))
    (let* ((b (current-buffer)) (m (tuatara.buffer:buffer-mark b)) (p (point)))
      (when (> p m)
        (setf (tuatara.buffer:buffer-mark b) p)
        (goto m)))
    t))

(defvar *marked-said* nil
  "What an object key last said it marked (SAY-MARKED), which a mark run's
reminder may take the place of.")

(defun say-marked (what)
  "Say that the object key marked WHAT (Word, Line ...)."
  (message "~a marked" what)
  (setf *marked-said* (editor-message *editor*)))

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

(defun reach-object (position on ahead behind)
  "From POSITION in the gap between two objects, the position of the object
*SELECTION-REACH* takes, AHEAD or BEHIND (functions giving it, or NIL when
there is none), the other side's when that one has none; POSITION itself when
ON, it is on one."
  (cond (on position)
        ((eq *selection-reach* :behind) (or (funcall behind) (funcall ahead)))
        (t (or (funcall ahead) (funcall behind)))))

(defun reach-word (buffer position)
  "The position of the word REACH-OBJECT takes from POSITION of BUFFER, words
read by their octets; NIL when there is none within *TEXT-SCAN-LIMIT* either
way."
  (reach-object position (word-char-p (tuatara.buffer:buffer-char-at buffer position))
                (lambda ()
                  (let* ((scan (min (tuatara.buffer:buffer-size buffer) (+ position *text-scan-limit*)))
                         (p (word-run-end buffer position nil scan)))
                    (and (< p scan) p)))
                (lambda ()
                  (let* ((scan (max 0 (- position *text-scan-limit*)))
                         (p (word-run-start buffer position nil scan)))
                    (and (> p scan) (tuatara.buffer:buffer-forward-char buffer p -1))))))

(defun word-start-at (buffer position)
  "The start of the word POSITION of BUFFER is in; refused past *TEXT-SCAN-LIMIT*."
  (let* ((scan (max 0 (- position *text-scan-limit*)))
         (p (word-run-start buffer position t scan)))
    (when (and (= p scan) (plusp scan)) (refuse "No start of the word within ~a" (scan-limit-words)))
    p))

(defun reach-symbol (buffer position test)
  "The position of the symbol, a run of characters passing TEST, REACH-OBJECT
takes from POSITION of BUFFER; NIL when there is none within
*TEXT-SCAN-LIMIT* either way."
  (reach-object position (funcall test (tuatara.buffer:buffer-char-at buffer position))
                (lambda ()
                  (let ((scan (min (tuatara.buffer:buffer-size buffer) (+ position *text-scan-limit*)))
                        (p position))
                    (loop while (and (< p scan) (not (funcall test (tuatara.buffer:buffer-char-at buffer p))))
                          do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
                    (and (< p scan) p)))
                (lambda ()
                  (let ((scan (max 0 (- position *text-scan-limit*)))
                        (p position))
                    (loop while (and (> p scan) (not (funcall test (buffer-char-before buffer p))))
                          do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
                    (and (> p scan) (tuatara.buffer:buffer-forward-char buffer p -1))))))

(defun symbol-start-at (buffer position test)
  "The start of the run of characters passing TEST that POSITION of BUFFER is
in; refused past *TEXT-SCAN-LIMIT*."
  (let ((scan (max 0 (- position *text-scan-limit*)))
        (p position))
    (loop while (and (> p scan) (funcall test (buffer-char-before buffer p)))
          do (setf p (tuatara.buffer:buffer-forward-char buffer p -1)))
    (when (and (<= p scan) (plusp p) (funcall test (buffer-char-before buffer p)))
      (refuse "No start of the symbol within ~a" (scan-limit-words)))
    p))

(defun symbol-end-at (buffer position test)
  "The end of the run of characters passing TEST that POSITION of BUFFER is
in; refused past *TEXT-SCAN-LIMIT*."
  (let ((scan (min (tuatara.buffer:buffer-size buffer) (+ position *text-scan-limit*)))
        (p position))
    (loop while (and (< p scan) (funcall test (tuatara.buffer:buffer-char-at buffer p)))
          do (setf p (tuatara.buffer:buffer-forward-char buffer p)))
    (when (and (>= p scan) (funcall test (tuatara.buffer:buffer-char-at buffer p)))
      (refuse "No end of the symbol within ~a" (scan-limit-words)))
    p))

(define-command select-word ()
  "Select the word at point; from the gap between two words, the one ahead
(the last one at the end of the text). Pressed again at once, or after
another of the \\`m' keys that select objects, grow the selection by one more
word. With a count, that many words; a negative count, words before."
  (let ((b (current-buffer)) (n (object-count)))
    (if (selection-continuing-p)
        (activate-mark-at (words-forward b (tuatara.buffer:buffer-mark b) (max 1 n)))
        (let ((on (reach-word b (point))))
          (unless on (refuse "No word at or before point"))
          (let ((start (word-start-at b on)))
            (if (plusp n)
                (select-bounds (current-buffer) start (words-forward b start n))
                (select-bounds (current-buffer)
                               (words-backward b start (- n))
                               (words-forward b (words-backward b start 1) 1))))))
    (say-marked "Word")))

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
    (say-marked label)))

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
                     (let ((c (buffer-char-before buffer p)))
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
               (on (reach-symbol b (point) test)))
          (unless on (refuse "No symbol at or before point"))
          (let* ((end (symbol-end-at b on test))
                 (start (let ((s (symbol-start-at b on test)))
                          ;; in Lisp, the quote or # before a symbol is part of it
                          (when (lisp-buffer-p b)
                            (loop while (and (plusp s) (find (buffer-char-before b s) "'`#,@"))
                                  do (setf s (tuatara.buffer:buffer-forward-char b s -1))))
                          s)))
            (if (plusp n)
                (select-bounds (current-buffer) start (if (> n 1)
                                                          (symbols-forward b end (1- n))
                                                          (trim-trailing-punctuation b start end)))
                (select-bounds (current-buffer)
                               (symbols-backward b start (- n))
                               (symbols-forward b (symbols-backward b start 1) 1))))))
    (say-marked "Symbol")))

(define-command select-symbol-backward ()
  "Select the symbol at point, or grow a symbol selection backward: from the
gap between two, the one behind."
  (select-backward #'symbols-backward #'select-symbol "Symbol"))

;;; Paragraphs, and sentences within them

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
          (activate-mark-at (absorb-paragraph-blank b start
                                                    (paragraphs-forward b (tuatara.buffer:buffer-mark b) (max 1 n)))))
        (let ((start (if (and (blank-line-at-p b origin) (eq *selection-reach* :behind) (text-before-p b origin))
                         (paragraphs-backward b origin 1)
                         (paragraphs-backward b (paragraphs-forward b origin 1) 1))))
          (let ((end (absorb-paragraph-blank b start (paragraphs-forward b start (max 1 n)))))
            (when (blank-range-p b start end)
              (goto origin)
              (refuse "No paragraph at or before point"))
            (select-bounds (current-buffer) start end))))
    (say-marked "Paragraph")))

(define-command select-paragraph-backward ()
  "Select the paragraph at point, or grow a paragraph selection backward:
from a blank line between two, the one above."
  (select-backward #'paragraphs-backward #'select-paragraph "Paragraph"))

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
              (refuse "No sentence at or before point"))
            (select-bounds (current-buffer) start end))))
    (say-marked "Sentence")))

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
                         (refuse "Unsupported delimiter `~a'; see *pair-delimiters*"
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
                      ((char= c close) (when (zerop (decf depth))
                                         (return (tuatara.buffer:buffer-forward-char buffer p))))))
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

(defun char-end-after (buffer from char)
  "The position after the next CHAR at or after FROM of BUFFER (FIND-CHAR), or NIL."
  (let ((q (find-char buffer from char :forward)))
    (and q (tuatara.buffer:buffer-forward-char buffer q))))

(defun pair-end-after (buffer position open close)
  "The position after the closer of the pair OPEN and CLOSE that opens at
POSITION, or NIL when there is none; a delimiter that opens and closes alike
ends at the next one."
  (let ((inside (tuatara.buffer:buffer-forward-char buffer position)))
    (if (char= open close)
        (char-end-after buffer inside close)
        (scan-pair-forward buffer inside open close))))

(defun pair-around (buffer position open close on-opener)
  "(START . END) of the pair of OPEN and CLOSE around POSITION: START at the
opener, END after the closer; on an opener, that one's pair."
  (let ((symmetric (char= open close)))
    (if on-opener
        (let ((end (pair-end-after buffer position open close)))
          (cond (end (cons position end))
                ((not symmetric) (refuse "No matching '~a' found after cursor" close))
                (t (let ((start (find-char buffer position open :backward)))
                     (if start
                         (cons start (tuatara.buffer:buffer-forward-char buffer position))
                         (refuse "No matching '~a' found before cursor" open))))))
        (let ((start (cond ((eql (tuatara.buffer:buffer-char-at buffer position) open) position)
                           (symmetric (find-char buffer position open :backward))
                           (t (scan-pair-backward buffer position open close)))))
          (unless start (refuse "No '~a' found near cursor" open))
          (let ((end (pair-end-after buffer start open close)))
            (unless end (refuse "No matching '~a' found after cursor" close))
            (cons start end))))))

(defun pair-around-level (buffer position open close on-opener levels)
  "(START . END) of the LEVELSth pair of OPEN and CLOSE out from POSITION:
each level the pair around the one before; for a delimiter that opens and
closes alike, each level one more of it on either side."
  (let ((span (pair-around buffer position open close on-opener)))
    (if (char= open close)
        (loop repeat (1- levels)
              do (let ((start (find-char buffer (car span) open :backward))
                       (end (char-end-after buffer (cdr span) close)))
                   (unless (and start end) (refuse "No enclosing `~a' beyond that level" open))
                   (setf span (cons start end))))
        (loop repeat (1- levels)
              do (when (zerop (car span)) (refuse "No enclosing `~a' beyond that level" open))
              (setf span (handler-case (pair-around buffer
                                                    (tuatara.buffer:buffer-forward-char buffer (car span) -1)
                                                    open close nil)
                           (refusal () (refuse "No enclosing `~a' beyond that level" open))))))
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
          (refuse "Empty selection between ~a and ~a" open close))
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
