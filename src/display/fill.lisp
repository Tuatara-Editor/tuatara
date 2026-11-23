;;;; src/display/fill.lisp --- Words filled into lines, and docstrings cut into paragraphs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.display)

(defun fill-words (words column &key (start 0) (indent 0) (width #'char-columns) cut)
  "WORDS, a list of strings, filled into lines that reach no further than
COLUMN columns, a space between each two words of a line, WIDTH giving each
character's columns (CHAR-COLUMNS unless given; STRING-COLUMNS): the first
line follows START columns already taken, each later one is set in by INDENT
\(not part of the lines returned). A word too long for a line of its own has
one to itself, or with CUT is cut into pieces that fit. Returns the lines, NIL
for no words."
  (let ((lines '()) (line nil) (used start))
    (dolist (word words)
      (let ((wide (string-columns word :width width)))
        (when cut
          (loop for room = (- column (if line indent (if lines indent start)))
                while (and (plusp room) (> wide room))
                do (let ((piece (max 1 (chars-in-columns word room :width width))))
                     (when line (push line lines) (setf line nil))
                     (push (subseq word 0 piece) lines)
                     (setf word (subseq word piece)
                           wide (string-columns word :width width)))))
        (cond ((null line) (setf line word used (+ (if lines indent start) wide)))
              ((<= (+ used 1 wide) column)
               (setf line (concatenate 'string line " " word))
               (incf used (1+ wide)))
              (t (push line lines)
                 (setf line word used (+ indent wide))))))
    (when line (push line lines))
    (nreverse lines)))

(defun list-item-head (line)
  "The length of what begins LINE as an item of a list, - or * or a number and
a full stop, with the blanks after it; NIL when LINE begins no item."
  (let* ((n (length line))
         (after (cond ((zerop n) nil)
                      ((find (char line 0) "-*") 1)
                      (t (let ((digits (or (position-if-not #'digit-char-p line) n)))
                           (and (plusp digits) (< digits n) (char= (char line digits) #\.) (1+ digits))))))
         (words (and after (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))) line
                                            :start after))))
    (and after (> (or words n) after) (or words n))))

(defun docstring-paragraphs (doc)
  "DOC, a docstring, cut into its parts in order, for its text to be filled
and the rest kept as written. The parts are (:BLANK) for a blank line,
\(:AS-IS LINE) for a line set in (a table, an example; under an item of a list,
set in further than its words), and (:FILLED HEAD INDENT WORDS LINES) for
lines whose words are filled. HEAD is what begins them (an item's - or * or
number and full stop with the blanks after it, else \"\"). INDENT is the
columns the lines after the first are set in by. WORDS is their words, a list,
and LINES the lines they were written in. An item's lines go on until a blank
line or the next item. Any other lines run on into one paragraph."
  (let ((parts '()) (head nil) (indent 0) (words '()) (lines '()))
    (flet ((flush ()
             (when head
               (push (list :filled head indent (reverse words) (reverse lines)) parts))
             (setf head nil words '() lines '()))
           (add (line from)
             (setf words (revappend (remove "" (uiop:split-string (subseq line from) :separator '(#\Space #\Tab))
                                            :test #'string=)
                                    words))
             (push line lines)))
      (dolist (line (uiop:split-string doc :separator '(#\Newline)))
        (let ((item (list-item-head line)))
          (cond ((every (lambda (c) (member c '(#\Space #\Tab))) line)
                 (flush) (push (list :blank) parts))
                (item (flush)
                      (setf head (subseq line 0 item) indent item)
                      (add line item))
                ((and head (plusp indent) (<= (or (position #\Space line :test-not #'char=) 0) indent))
                 (add line 0))
                ((char= (char line 0) #\Space)
                 (flush) (push (list :as-is line) parts))
                (t (unless head (setf head "" indent 0))
                   (add line 0)))))
      (flush))
    (nreverse parts)))
