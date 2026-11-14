;;;; lisp/selecting/rectangles.lisp --- Rectangles: a block of columns selected, copied, cut, changed and pasted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A rectangle over more text than *REWRITE-THRESHOLD* never becomes a list
;;;; of rows: y keeps it by where it is (the text as it was, which never
;;;; changes, its lines and its columns), at once whatever its size, and d, c,
;;;; P and the wrap keys rewrite its lines in the background as a replacement
;;;; does (rewrite.lisp); the editor stays usable, P is lit in the mode line,
;;;; C-x J (stop-job) stops it, and one undo takes it back.

(in-package #:tuatara.defaults)

(defvar *killed-rectangle* nil
  "The rectangle last copied, cut or changed, what
\\<normal>\\[yank-rectangle] pastes: a list of its rows' texts, top
first, or a RECTANGLE-SOURCE for one over more text than *REWRITE-THRESHOLD*.")

(define-record (rectangle-source (:constructor make-rectangle-source (rope top bottom left right crlf rows tab-width))
                                (:copier nil))
  "A rectangle kept by where it is. It holds ROPE, the text as it was then (a
text never changes), and the lines from the one starting at TOP to the one
starting at BOTTOM. It lies between the columns LEFT and RIGHT on the screen,
a tab TAB-WIDTH columns at most. CRLF (carriage return and line feed) is true
when a carriage return before a newline ends its lines. ROWS is how many lines
it has."
  (rope nil :documentation "The text RECTANGLE-SOURCE was taken from, as it was then.")
  (top 0 :type fixnum :documentation "The start of the first line of RECTANGLE-SOURCE.")
  (bottom 0 :type fixnum :documentation "The start of the last line of RECTANGLE-SOURCE.")
  (left 0 :type fixnum :documentation "The column on the screen where RECTANGLE-SOURCE starts.")
  (right 0 :type fixnum :documentation "The column on the screen where RECTANGLE-SOURCE ends.")
  (crlf nil :documentation "True when a carriage return before a newline ends the lines of RECTANGLE-SOURCE.")
  (rows 0 :type fixnum :documentation "How many lines RECTANGLE-SOURCE has.")
  (tab-width 8 :type fixnum :documentation "The most columns a tab takes on the screen in RECTANGLE-SOURCE."))

(setf (documentation 'make-rectangle-source 'function) "Return a RECTANGLE-SOURCE: a rectangle kept by where it is.")

(defun rectangle-lines (buffer)
  "BUFFER's rectangle as the lines it spans, five values: the starts of its
first and last lines, its left and right columns, and how many rows it has.
While the lines are still being counted, a rectangle of no more than
*REWRITE-THRESHOLD* counts its own rows; a larger one waits for the count."
  (let* ((p (tuatara.buffer:buffer-point buffer))
         (m (tuatara.buffer:buffer-mark buffer))
         (top (tuatara.buffer:buffer-line-start buffer (min p m)))
         (bottom (tuatara.buffer:buffer-line-start buffer (max p m))))
    (multiple-value-bind (left right) (rectangle-columns buffer)
      (values top bottom left right
              (if (and (not (tuatara.buffer:buffer-indexed-p buffer)) (<= (- bottom top) *rewrite-threshold*))
                  (1+ (tuatara.text:rope-count-newlines (tuatara.buffer:buffer-text buffer) top bottom))
                  (1+ (- (tuatara.buffer:buffer-line-number buffer bottom)
                         (tuatara.buffer:buffer-line-number buffer top))))))))

(defparameter *rectangle-rows-in-memory* 65536
  "The most rows a rectangle is changed on at once, row by row: one of more is
kept by where it is and changed in the background, as one over more text than
*REWRITE-THRESHOLD* is.")

(defun large-rectangle-p (buffer)
  "True when BUFFER's rectangle spans more text than *REWRITE-THRESHOLD*, or
more rows than *RECTANGLE-ROWS-IN-MEMORY*: it is then kept by where it is, and
changed in the background. The rows are counted, not read."
  (let* ((p (tuatara.buffer:buffer-point buffer))
         (m (tuatara.buffer:buffer-mark buffer))
         (top (tuatara.buffer:buffer-line-start buffer (min p m))))
    (or (> (- (tuatara.buffer:buffer-line-end buffer (max p m)) top) *rewrite-threshold*)
        (> (nth-value 4 (rectangle-lines buffer)) *rectangle-rows-in-memory*))))

(defun line-screen-column-position (buffer line column)
  "Where COLUMN on the screen is on BUFFER's line number LINE (from 0), or the
line's end when it is shorter."
  (tuatara.display:buffer-move-to-screen-column buffer (tuatara.buffer:buffer-goto-line buffer line) column))

(defun rectangle-source-of (buffer)
  "BUFFER's rectangle kept by where it is, in its text as it is now."
  (multiple-value-bind (top bottom left right rows) (rectangle-lines buffer)
    (make-rectangle-source (tuatara.buffer:buffer-text buffer) top bottom left right (buffer-crlf-p buffer) rows
                           (tuatara.display:tab-width-of buffer))))

(defun row-inside (rope s e sc ec left right tab-width)
  "What of a rectangle's row is inside its columns LEFT to RIGHT on the screen,
the row's text in ROPE running from S to E and its columns from SC to EC
\(TUATARA.TEXT:MAP-LINE-SLICES, a tab TAB-WIDTH columns at most): four
values, the spaces before, where the text kept starts and ends, and the spaces
after. A tab across an edge is kept as the spaces it shows inside it."
  (declare (type fixnum s e sc ec left right))
  (if (or (= s e) (null tab-width) (and (>= sc left) (<= ec right)))
      (values 0 s e 0)
      (let ((before 0) (from s) (to e) (after 0) (column sc))
        (declare (type fixnum before from to after column))
        (when (< sc left)
          (let ((stop (next-tab-stop sc tab-width)))
            (when (>= stop right)
              (return-from row-inside (values (- right left) e e 0)))
            (setf before (- stop left) from (1+ s) column stop)))
        (when (> ec right)
          ;; the tab across RIGHT ends the row: where it starts
          (let* ((octets (tuatara.text:rope-octets rope :start from :end e))
                 (n (length octets))
                 (i 0) (joined 0))
            (declare (type fixnum n i joined))
            (loop while (< i n)
                  do (multiple-value-bind (code length) (tuatara.text:octets-decode octets i n)
                       (multiple-value-bind (w next) (tuatara.display:advance-column code column joined tab-width nil)
                         (when (> (+ column w) right) (return))
                         (incf column w)
                         (setf joined next)
                         (incf i length))))
            (setf to (+ from i) after (max 0 (- right column)))))
        (values before from to after))))

(defun tabs-as-spaces (octets column tab-width)
  "OCTETS, text that starts at COLUMN on the screen, each tab in it made the
spaces it shows as, TAB-WIDTH columns apart: a new vector."
  (let ((n (length octets)) (i 0) (parts '()) (from 0) (before 0))
    (loop while (< i n)
          do (multiple-value-bind (code length) (tuatara.text:octets-decode octets i n)
               (multiple-value-bind (w next) (tuatara.display:advance-column code column before tab-width nil)
                 (when (= code 9)
                   (push (subseq octets from i) parts)
                   (push w parts)
                   (setf from (+ i length)))
                 (incf column w)
                 (setf before next))
               (incf i length)))
    (push (subseq octets from) parts)
    (apply #'row-octets (nreverse parts))))

(defun row-octets (&rest parts)
  "One octet vector of PARTS, in order: a number is that many spaces (none
when it is not above zero), an octet vector its octets."
  (let ((out (tuatara.text:make-octets (loop for part in parts sum (if (integerp part) (max 0 part) (length part)))))
        (at 0))
    (dolist (part parts out)
      (if (integerp part)
          (let ((n (max 0 part))) (fill out 32 :start at :end (+ at n)) (incf at n))
          (progn (replace out part :start1 at) (incf at (length part)))))))

(defun source-rows-block (source from to &optional (repeat 1))
  "SOURCE's rows FROM below TO, each padded with spaces to the block's width
and REPEAT times across, one after another, as two values: an octet vector
holding them, and a vector of fixnums giving where each row starts in it and,
last, where the last one ends. A tab in a row becomes the spaces it showed as,
those inside the block of one across its edge, and a wide character across
its left edge a space before the row. SOURCE's text is read a block at a time."
  (let* ((rope (rectangle-source-rope source))
         (count (max 0 (- to from)))
         (starts (make-array (1+ count) :element-type 'fixnum :initial-element 0))
         (left (rectangle-source-left source))
         (right (rectangle-source-right source))
         (tab-width (rectangle-source-tab-width source))
         (width (- right left))
         (octets (tuatara.text:make-octets (max 64 (* count (max 1 width) (max 1 repeat)))))
         (used 0) (i 0))
    (declare (type fixnum left right width used i))
    (flet ((room-for (n)
             (declare (type fixnum n))
             (when (> (+ used n) (length octets))
               (let ((bigger (tuatara.text:make-octets (max (+ used n) (* 2 (length octets))))))
                 (replace bigger octets :end2 used)
                 (setf octets bigger)))))
      (when (plusp count)
        (block rows
          (apply
           #'tuatara.text:map-line-slices
           (lambda (line s e sc ec line-end block block-start block-end)
             (declare (ignore line line-end) (type fixnum s e sc ec block-start block-end))
             (multiple-value-bind (before from to after) (row-inside rope s e sc ec left right tab-width)
               (declare (type fixnum before from to after))
               (let* ((lead (if (< s e) (+ (max 0 (- sc left)) before) 0))
                      (tabbed (and (< from to) (if (and (>= from block-start) (<= to block-end))
                                                   (find 9 block :start (- from block-start) :end (- to block-start))
                                                   (find 9 (tuatara.text:rope-octets rope :start from :end to)))))
                      (text (and tabbed (tabs-as-spaces (tuatara.text:rope-octets rope :start from :end to)
                                                        (+ left lead) tab-width)))
                      (pad (max 0 (- width (if (< s e) (- (min ec right) left) 0))))
                      (row-length (+ lead (if text (length text) (- to from)) after pad))
                      (row-start used))
                 (declare (type fixnum lead pad row-length row-start))
                 (room-for (* row-length (max 1 repeat)))
                 (fill octets 32 :start used :end (+ used lead))
                 (incf used lead)
                 (cond (text (replace octets text :start1 used) (incf used (length text)))
                       (t (if (and (>= from block-start) (<= to block-end))
                              (replace octets block :start1 used :start2 (- from block-start) :end2 (- to block-start))
                              (tuatara.text:rope-copy-into rope from to octets used))
                          (incf used (- to from))))
                 (fill octets 32 :start used :end (+ used after pad))
                 (incf used (+ after pad))
                 (loop repeat (1- repeat)
                       do (replace octets octets :start1 used :start2 row-start :end2 (+ row-start row-length))
                       (incf used row-length))
                 (setf (aref starts i) row-start)
                 (incf i)
                 (setf (aref starts i) used)
                 (when (>= i count) (return-from rows)))))
           ;; from the top itself without the line numbers, which may be still being counted
           rope (if (zerop from)
                    (rectangle-source-top source)
                    (tuatara.text:line->byte rope
                                             (+ (tuatara.text:byte->line rope (rectangle-source-top source)) from)))
           (1+ (rectangle-source-bottom source)) left right
           :octets t (tuatara.display:screen-slice-keys (rectangle-source-crlf source) tab-width)))))
    (values octets starts)))

(defun source-texts (source)
  "The rows of SOURCE as texts, top first, each padded to the block's width."
  (multiple-value-bind (octets starts) (source-rows-block source 0 (rectangle-source-rows source))
    (loop for i from 0 below (rectangle-source-rows source)
          collect (tuatara.text:octets-string octets :start (aref starts i) :end (aref starts (1+ i))))))

(defun killed-rectangle-rows ()
  "How many rows the rectangle store holds."
  (if (rectangle-source-p *killed-rectangle*)
      (rectangle-source-rows *killed-rectangle*)
      (length *killed-rectangle*)))

(define-command rectangle-mark-mode ()
  "Start a rectangle: the block of columns between point and the mark on each
line from one to the other; any motion sizes it, and its corner can stand past
the end of a short line. Started with nothing selected it holds the character
at point; a selection made already becomes one by its corners.
In NORMAL state it starts on the character under the cursor; elsewhere
\(\\[rectangle-mark-mode]) with no width.
\\<normal>\\[rectangle-mark-mode] again, or \\[tuatara-quit], cancels it.
\\[copy-selection-or-char] copies the block and \\[delete-selection-or-char] cuts it, to the rectangle
store that \\[yank-rectangle] pastes from; \\[tuatara-change] changes it,
and a wrap key wraps each of its rows. \\[kill-region] and \\[kill-ring-save] kill
and copy it as a block, and \\[yank] pastes it back as a block."
  (let ((b (current-buffer)))
    (if (rectangle-live-p b)
        (progn (setf (tuatara.buffer:buffer-mark-active-p b) nil)
               (message "Rectangle: cancelled"))
        (let ((had (tuatara.buffer:buffer-mark-active-p b)))
          (fresh-selection b)
          (unless had
            (setf (tuatara.buffer:buffer-mark b) (point)
                  (tuatara.buffer:buffer-mark-active-p b) t)
            (let ((p (point)))
              (unless (or (= p (tuatara.buffer:buffer-line-end b p))
                          (not (eq (buffer-modal-class b) :editing))
                          (not (eq (modal-state b) :normal)))
                (goto (tuatara.buffer:buffer-forward-char b p)))))
          (setf (tuatara.buffer:buffer-local b :rectangle) t)
          (message "~a" (substitute-command-keys *rectangle-hint* :marks t))))))

(defun rectangle-shown-sources (buffer)
  "BUFFER's rectangle kept by where it is, as RECTANGLE-SOURCE-OF gives it,
in pieces: one for each run of its lines shown, the lines search filters
hide between them left out (TUATARA.DISPLAY:SHOWN-PARTS). How many lines
were left out is the second value."
  (multiple-value-bind (top bottom left right rows) (rectangle-lines buffer)
    (let* ((size (tuatara.buffer:buffer-size buffer))
           (high (min size (1+ bottom)))
           (parts (and (< top high) (tuatara.display:shown-parts buffer top high))))
      (if (or (null parts) (equal parts (list (cons top high))))
          (values (list (rectangle-source-of buffer)) 0)
          (flet ((last-line (start end)
                   (if (>= end high) bottom (tuatara.buffer:buffer-line-start buffer (max start (1- end)))))
                 (lines (start last)
                   (1+ (- (tuatara.buffer:buffer-line-number buffer last)
                          (tuatara.buffer:buffer-line-number buffer start)))))
            (let ((sources (loop for (start . end) in parts
                                 for last = (last-line start end)
                                 collect (make-rectangle-source (tuatara.buffer:buffer-text buffer) start last
                                                                left right (buffer-crlf-p buffer) (lines start last)
                                                                (tuatara.display:tab-width-of buffer)))))
              (values sources (- rows (reduce #'+ sources :key #'rectangle-source-rows)))))))))

(defun rectangle-texts (buffer)
  "The rows of BUFFER's rectangle as texts, a row short of the block padded
with spaces to its width; the rows of lines search filters hide left out,
how many as the second value (RECTANGLE-SHOWN-SOURCES)."
  (multiple-value-bind (sources left-out) (rectangle-shown-sources buffer)
    (values (mapcan #'source-texts sources) left-out)))

(defun store-rectangle (texts)
  "Keep TEXTS as the rectangle \\<normal>\\[yank-rectangle] pastes,
unless no row has any: true when kept."
  (when (some (lambda (text) (plusp (length text))) texts)
    (setf *killed-rectangle* texts)
    t))

(defun copy-selected-rectangle ()
  "\\<normal>\\[copy-selection-or-char] on a rectangle: copy the block to the rectangle
store, and let it go; one over more text than *REWRITE-THRESHOLD* is kept by
where it is, at once, unless search filters hide lines inside it; the rows
of those are left out. A block with no width copies nothing and stays."
  (let ((b (current-buffer)))
    (multiple-value-bind (left right) (rectangle-columns b)
      (multiple-value-bind (sources left-out) (rectangle-shown-sources b)
        (cond ((= left right) (message "Nothing to copy: the rectangle has no width"))
              ((and (zerop left-out) (large-rectangle-p b))
               (setf *killed-rectangle* (first sources)
                     (tuatara.buffer:buffer-mark-active-p b) nil)
               (message "Copied a rectangle of ~:d row~:p" (killed-rectangle-rows)))
              ((store-rectangle (mapcan #'source-texts sources))
               (setf (tuatara.buffer:buffer-mark-active-p b) nil)
               (message "Copied a rectangle of ~:d row~:p~a" (killed-rectangle-rows) (left-out-note left-out)))
              (t (message "Nothing to copy: the rectangle has no width")))))))

(defun replace-rectangle-rows (buffer rows text &key group (pad t))
  "Put TEXT in place of each of ROWS of BUFFER's rectangle, each (START END
START-COLUMN END-COLUMN) as RECTANGLE-ROWS gives it, a row short of the block
padded out to its left column first unless PAD is false, a tab across an edge
of the block made spaces outside it; one change \(DELETE-SPANS), joining the
newest with GROUP. Returns two byte offsets, just after TEXT on the last row
and on the first, and whether anything changed. Refused, nothing changed,
when that would take part of the text a fold hides, but not all of it."
  (multiple-value-bind (left right) (rectangle-columns buffer)
    (let* ((tails (loop for (nil nil nil ec) in rows collect (max 0 (- ec right))))
           (positions (delete-spans buffer rows :group group
                                    :replacements (loop for (start end sc) in rows
                                                        for tail in tails
                                                        collect (concatenate 'string
                                                                             (spaces (if (or pad (< start end))
                                                                                         (- left sc)
                                                                                         0))
                                                                             text (spaces tail))))))
      (if positions
          (values (- (aref positions (1- (length positions))) (car (last tails)))
                  (- (aref positions 0) (first tails))
                  t)
          (values (first (car (last rows))) (first (first rows)) nil)))))

(defun delete-selected-rectangle ()
  "\\<normal>\\[delete-selection-or-char] on a rectangle: cut the block to the rectangle
store, one change for undo; point stays on its row, at the block's left column.
One over more text than *REWRITE-THRESHOLD* is cut in the background. A block
with no width deletes nothing and stays; in a buffer that may not be written,
nothing is cut and the store keeps what it had. A block that would take part
of the text a fold hides, but not all of it, is not cut: the fold is shown
instead, and the key pressed again cuts; cut in the background, a row where
that would be is left as it is."
  (let ((b (current-buffer)))
    (check-writable b)
    (multiple-value-bind (left right) (rectangle-columns b)
      (cond ((= left right) (message "Nothing to delete: the rectangle has no width"))
            ((large-rectangle-p b)
             (let ((source (rectangle-source-of b)))
               (when (start-rectangle-job b :cut)
                 (setf *killed-rectangle* source
                       (tuatara.buffer:buffer-mark-active-p b) nil))))
            (t
             (let ((texts (rectangle-texts b)))
               (if (some (lambda (text) (plusp (length text))) texts)
                   (progn (replace-rectangle-rows b (region-rectangle-rows b) "" :pad nil)
                          (store-rectangle texts)
                          (setf (tuatara.buffer:buffer-mark-active-p b) nil))
                   (message "Nothing to delete: the rectangle has no width"))))))))

(defun change-selected-rectangle ()
  "\\<normal>\\[tuatara-change] on a rectangle: the block goes to the rectangle
store and is emptied on every row, a row short of it padded out to its column,
one change for undo with what is typed. One row high, INSERT state begins
there; taller, a cursor stands on every row, the real one on point's, and what
is typed appears on them all: \\<insert>\\[tuatara-normal] comes back to the cursors, and
\\<*cursor-keymap*>\\[split-quit] again ends them. A block taller than
*RECTANGLE-CURSORS-LIMIT* (or than the memory has room for) is the same to
type at. The rows in sight are emptied and hold a cursor each. The others are
emptied and written in the background as the typing ends (HOLD-VIRTUAL-BLOCK).
With *SPLIT-LIMIT-ASK* true, you are asked first whether to hold them all, as
\\[split-selection] and \\[add-cursor-below] ask. The rows are counted before
any is read."
  (let* ((b (current-buffer))
         (p (point))
         (m (tuatara.buffer:buffer-mark b))
         (top (tuatara.buffer:buffer-line-start b (min p m)))
         (bottom (tuatara.buffer:buffer-line-start b (max p m)))
         (end (next-line-start b bottom)))
    (check-writable b)
    (multiple-value-bind (left right) (rectangle-columns b)
      (multiple-value-bind (count exact) (span-lines b top end (place-count-cap *rectangle-cursors-limit*))
        (when (eq (past-limit-choice count exact "cursors" *rectangle-cursors-limit*) :in-sight)
          (return-from change-selected-rectangle (hold-virtual-block b top bottom left right count exact)))))
    (let* ((rows (region-rectangle-rows b))
           (left (rectangle-columns b))
           (top (tuatara.buffer:buffer-line-number b (first (first rows))))
           (here (tuatara.buffer:buffer-line-number b (point)))
           (before (tuatara.buffer:buffer-undo-stack b))
           ;; a block over much text is kept by where it is, not read
           (kept (if (large-rectangle-p b) (rectangle-source-of b) (rectangle-texts b))))
      (replace-rectangle-rows b rows "")
      (if (rectangle-source-p kept) (setf *killed-rectangle* kept) (store-rectangle kept))
      (setf (tuatara.buffer:buffer-mark-active-p b) nil)
      ;; no line was made or taken away: each row is still its line
      (flet ((row-at-line (line)
               (line-screen-column-position b line left)))
        (goto (row-at-line here))
        (if (rest rows)
            (let ((split (start-cursors left)))
              (declare (notinline (setf split-did)))
              (add-cursors split (loop for line from top below (+ top (length rows))
                                       unless (= line here) collect (row-at-line line)))
              (setf (split-did split) :changed)
              (enter-split-edit split nil :start :since before))
            (tuatara-insert))))))

(defun insert-rectangle-at (buffer position texts &key group)
  "Put the rows TEXTS into BUFFER one above another. The first goes at
POSITION, each other at POSITION's column on the screen on the line below the
last (a short line padded out to it with spaces, a tab across it made spaces).
New lines are made at the end as needed. It is refused when that would be past
the part shown of a narrowed buffer. One change, joining the newest with
GROUP. Returns the byte offset after the last row."
  (let* ((column (tuatara.display:buffer-screen-column buffer position))
         ;; the end of the text, or of the part shown of a narrowed buffer
         (size (tuatara.buffer:buffer-end buffer))
         (newline (tuatara.buffer:buffer-newline buffer))
         (line (tuatara.buffer:buffer-line-start buffer position))
         (edits '())                    ; (start end text after), bottom first
         (last-tail 0))
    (loop for text in texts
          for first = t then nil
          do (if line
                 (multiple-value-bind (s e sc ec)
                     (if first
                         (values position position column column)
                         (tuatara.display:buffer-screen-slice buffer line column column))
                   ;; a tab across the column: its columns either side made spaces
                   (let ((tail (if (< s e) (max 0 (- ec column)) 0))
                         (end (tuatara.buffer:buffer-line-end buffer line)))
                     (push (list s e (concatenate 'string (spaces (- column sc)) text (spaces tail))) edits)
                     (setf last-tail tail
                           line (let ((next (and (< end size) (tuatara.buffer:buffer-forward-char buffer end))))
                                  ;; the line after the part shown is not one of its own
                                  (and next (not (and (= next size) (< size (tuatara.buffer:buffer-size buffer))))
                                       next)))))
                 (progn
                   ;; new lines are made at the end of the text, never at the edge of a part shown
                   (when (< size (tuatara.buffer:buffer-size buffer))
                     (tuatara.buffer:refuse-outside-narrowing buffer))
                   (push (list size size (concatenate 'string newline (spaces column) text)) edits)
                   (setf last-tail 0))))
    ;; the rows made at the end go in with the last line's, as one insertion
    (let ((merged '()))
      (dolist (edit (nreverse edits))
        (if (and merged (= (first (first merged)) (first edit) (second edit)))
            (setf (third (first merged)) (concatenate 'string (third (first merged)) (third edit)))
            (push (copy-list edit) merged)))
      (let ((positions (tuatara.buffer:buffer-edit
                        buffer
                        (map 'simple-vector
                             (lambda (edit) (tuatara.text:make-edit (first edit) (second edit)
                                                                    (tuatara.text:string-octets (third edit))))
                             (reverse merged))
                        :group group)))
        (- (aref positions (1- (length positions))) last-tail)))))

(define-command yank-rectangle ()
  "Paste the rectangle store (what \\<normal>\\[copy-selection-or-char], \\[delete-selection-or-char]
or \\[tuatara-change] last took from a rectangle) as a block of columns: from
point, one row a line, or over a selected rectangle of as many rows in its
place. A selection that is not a rectangle is replaced. With a count, each row
that many times across, a wider block, over a rectangle too; a count of 0 or
less pastes nothing and leaves the selection as it is. A rectangle over more text than
*REWRITE-THRESHOLD* is pasted in the background. What would replace part of
the text a fold hides, but not all of it, is not pasted: the fold is shown
instead, and the key pressed again pastes; pasted in the background, a row
where that would be is left as it is."
  (let ((b (current-buffer)) (n (prefix)))
    (cond ((< n 1) (message "A count of ~d pastes nothing" n))
          ((rectangle-source-p *killed-rectangle*)
           (paste-large-rectangle b *killed-rectangle* n))
          ((notany (lambda (text) (plusp (length text))) *killed-rectangle*)
           (message "No rectangle to paste"))
          ((rectangle-live-p b)
           ;; counted before a row is read
           (let ((count (nth-value 4 (rectangle-lines b))))
             (if (/= count (length *killed-rectangle*))
                 (message "Rectangle row mismatch: selection has ~:d row~:p, copied rectangle has ~:d; paste refused"
                          count (length *killed-rectangle*))
                 (multiple-value-bind (after top-left changed) (replace-rectangle-rows b (region-rectangle-rows b) "")
                   (declare (ignore after))
                   (setf (tuatara.buffer:buffer-mark-active-p b) nil)
                   (goto (insert-rectangle-at b top-left (repeated-rows *killed-rectangle* n) :group changed))))))
          (t
           (multiple-value-bind (start end) (selection-range)
             (when (and start (< start end))
               (delete-spans b (list (cons start end)))
               (goto start)))
           (setf (tuatara.buffer:buffer-mark-active-p b) nil)
           (paste-rows-at-point (repeated-rows *killed-rectangle* n))))))

(defun repeated-rows (rows n)
  "ROWS, strings, each N times across."
  (if (= n 1)
      rows
      (mapcar (lambda (row) (repeated-text row n)) rows)))

(defun paste-rows-at-point (rows)
  "Put ROWS in as a block of columns from point, one row a line, lines added
and short ones padded as needed: the mark at its top left corner, point after
its last row."
  (let ((p (point)))
    (goto (insert-rectangle-at (current-buffer) p rows))
    (setf (tuatara.buffer:buffer-mark (current-buffer)) p)))

(defun kill-live-rectangle (delete)
  "\\[kill-region] (DELETE true) or \\[kill-ring-save] on a rectangle: the block is
cut or copied to the rectangle store, and to the kill ring as its rows a line
each, a kill made of parts (KILL-IN-PARTS) that \\[yank] and
\\<normal>\\[yank-over-selection] paste back as a block, and pasting at as many
cursors gives each its own row. One over more text than *REWRITE-THRESHOLD*
goes to the rectangle store alone."
  (let ((large (large-rectangle-p (current-buffer))) (before *killed-rectangle*))
    (if delete (delete-selected-rectangle) (copy-selected-rectangle))
    (when (and (not large) (listp *killed-rectangle*) (not (eq *killed-rectangle* before)))
      (kill-in-parts *killed-rectangle* :shape :block :kill (format nil "~{~a~^~%~}" *killed-rectangle*)))))

;;; Rectangle commands on the region: the block of columns between the mark and
;;; point, whether a rectangle is shown or not

(defun require-mark (buffer)
  "Refuse when BUFFER's mark is not set, so that it has no region."
  (unless (tuatara.buffer:buffer-mark buffer)
    (refuse "The mark is not set now, so there is no region")))

(defun call-on-region-rectangle (function)
  "Call FUNCTION with the buffer, its block of columns between the mark and
point taken as its rectangle; the selection is let go afterwards."
  (let ((b (current-buffer)))
    (require-mark b)
    (unless (rectangle-live-p b)
      (end-rectangle b)
      (setf (tuatara.buffer:buffer-local b :rectangle) t
            (tuatara.buffer:buffer-mark-active-p b) t))
    (unwind-protect (funcall function b)
      (setf (tuatara.buffer:buffer-mark-active-p b) nil)
      (end-rectangle b))))

(define-command kill-rectangle ()
  "Kill the rectangle between the mark and point: the block of columns from
one to the other on each line between, a short line's part padded with spaces,
goes to the rectangle store that \\[yank-rectangle] pastes from. Point stays
on its line, at the block's left column. A rectangle over more text than
*REWRITE-THRESHOLD* is cut in the background."
  (call-on-region-rectangle (lambda (b) (declare (ignore b)) (delete-selected-rectangle))))

(define-command copy-rectangle-as-kill ()
  "Copy the rectangle between the mark and point to the rectangle store that
\\[yank-rectangle] pastes from, the text staying as it is."
  (call-on-region-rectangle (lambda (b) (declare (ignore b)) (copy-selected-rectangle))))

(define-command delete-rectangle ()
  "Delete the rectangle between the mark and point, keeping it nowhere: the
rectangle store keeps what it had. Point stays on its line, at the block's
left column."
  (call-on-region-rectangle (lambda (b)
                              (declare (ignore b))
                              (let ((kept *killed-rectangle*))
                                (delete-selected-rectangle)
                                (setf *killed-rectangle* kept)))))

(defvar *last-rectangle-string* ""
  "The text \\[string-rectangle] last put in, offered the next time.")

(define-command string-rectangle ()
  "Replace the rectangle between the mark and point with a text read in the
minibuffer, on each of its lines; with no width, the text goes in at its
column on each line. Point goes after the text on the last line. The text
given last time is offered. A rectangle over more text than
*REWRITE-THRESHOLD* is rewritten in the background."
  (require-mark (current-buffer))
  (let ((text (read-string "String rectangle: " :initial *last-rectangle-string* :select-initial t)))
    (setf *last-rectangle-string* text)
    (call-on-region-rectangle
     (lambda (b)
       (check-writable b)
       (if (large-rectangle-p b)
           (start-rectangle-job b :change :text text)
           (let ((after (replace-rectangle-rows b (region-rectangle-rows b) text)))
             (when after (goto after))))))))

(defun rows-change-now (buffer kind &key numbering open close)
  "Make the change KIND (ROWS-JOB) to the rows of BUFFER's rectangle at once,
as the background job would make it, NUMBERING, OPEN and CLOSE as it has
them: one change for undo (DELETE-SPANS). True when anything changed. Refused,
nothing changed, when that would take part of the text a fold hides, but not
all of it."
  (multiple-value-bind (top bottom left right) (rectangle-lines buffer)
    (let ((spec (rows-job-spec buffer kind top bottom left right :numbering numbering :open open :close close))
          (replacements '()) (spans '()))
      (setf (rows-job-folds spec) nil)
      (funcall (rectangle-mapper spec)
               (lambda (start end old replacement)
                 (declare (ignore old))
                 (push (cons start end) spans)
                 (push replacement replacements))
               top (rows-job-high spec) nil)
      (and spans (delete-spans buffer (nreverse spans) :replacements (nreverse replacements)) t))))

(define-command clear-rectangle ()
  "Blank out the rectangle between the mark and point: its columns become
spaces on each line, so that what comes after stays where it was; a line that
ends inside the block is cut at its left column instead, leaving no spaces at
its end, and a line shorter than the block is left alone. Point goes to the
block's left column on its line. A rectangle over more text than
*REWRITE-THRESHOLD* is cleared in the background."
  (call-on-region-rectangle
   (lambda (b)
     (check-writable b)
     (let ((line (tuatara.buffer:buffer-line-number b (point))) (left (rectangle-columns b)))
       (if (large-rectangle-p b)
           (start-rectangle-job b :clear)
           (progn (rows-change-now b :clear)
                  (goto (line-screen-column-position b line left))))))))

(defun number-rectangle (replace)
  "Number the lines of the rectangle between the mark and point, as
\\[number-rectangle-lines] (REPLACE false) or \\[number-rectangle-lines-replace]
(REPLACE true) do, a format and a number to count from asked for with
\\[universal-argument]."
  (require-mark (current-buffer))
  (let* ((b (current-buffer))
         (rows (nth-value 4 (rectangle-lines b)))
         (ask (prefix-argument))
         (first (if ask
                    (let ((text (read-string "Number to count from: " :initial "1" :select-initial t)))
                      (or (whole-number text)
                          (refuse "Not a whole number: ~a" text)))
                    1))
         (default (format nil "%~dd~:[ ~;~]" (length (princ-to-string (+ first (max 0 (1- rows))))) replace))
         (formatter (number-formatter (if ask (read-string "Format string: " :initial default) default))))
    (call-on-region-rectangle
     (lambda (b)
       (check-writable b)
       (let ((numbering (lambda (k) (tuatara.text:string-octets (funcall formatter (+ first k)))))
             (kind (if replace :number :number-in)))
         (if (large-rectangle-p b)
             (start-rectangle-job b kind :numbering numbering)
             (let* ((p (point))
                    (line (tuatara.buffer:buffer-line-number b p))
                    (column (tuatara.display:buffer-screen-column b p))
                    (last (tuatara.buffer:buffer-line-number b (max p (tuatara.buffer:buffer-mark b))))
                    (left (rectangle-columns b)))
               (rows-change-now b kind :numbering numbering)
               (cond (replace
                      (goto (+ (line-screen-column-position b last left)
                               (length (funcall numbering (max 0 (1- rows)))))))
                     ;; point at or before the number stays before it
                     ((<= column left)
                      (goto (line-screen-column-position b line column)))))))))))

(define-command number-rectangle-lines ()
  "Number the lines of the rectangle between the mark and point: 1, 2, 3 and
on, each with a space after it, put in at the block's left column, a line
shorter than that padded out to it; the numbers are right-aligned to the width
of the last, and the text of the block moves right to make room. Point stays
by the text it was at. With \\[universal-argument], the number to count from
and the format are asked for: %d stands for the number, %3d gives it three
columns, %-3d puts it on their left, %03d pads it with zeros, %% is a percent
sign. A rectangle over more text than *REWRITE-THRESHOLD* is numbered in the
background."
  (number-rectangle nil))

(define-command number-rectangle-lines-replace ()
  "Replace the rectangle between the mark and point with numbers, 1, 2, 3 and
on down its lines, nothing after them: to renumber a list, or to turn its
bullets into numbers. The numbers are right-aligned to the width of the last,
a line shorter than the block padded out to it; with no width (the mark and
point in the same column) they go in at that column. Point goes after the
number on the last line. With \\[universal-argument], the number to count from
and the format are asked for, as \\[number-rectangle-lines] asks. A rectangle
over more text than *REWRITE-THRESHOLD* is numbered in the background."
  (number-rectangle t))

(defun wrap-rectangle (char)
  "Put the pair of the wrap key CHAR (WRAP-PAIR) around the block of the
rectangle on each of its rows, a row short of it padded out with spaces; or,
where the pair stands just outside the block on every row already, take it
off, as every wrap does. One change for undo (DELETE-SPANS), point after the
first row's opening half, or where it stood once taken off; the rectangle
over. One over more text than *REWRITE-THRESHOLD* is wrapped in the
background, or unwrapped there, the pair taken off every row it stands
around, when it stands around every row in sight."
  (let ((b (current-buffer)))
    (check-writable b)
    (multiple-value-bind (open close) (wrap-pair char)
      (if (large-rectangle-p b)
          (let ((in-sight (loop for (from . to) in (sight-ranges b)
                                append (region-rectangle-rows b :from from :to to))))
            (when (start-rectangle-job b
                                       (if (and in-sight
                                                (every (lambda (row)
                                                         (already-wrapped-p b (first row) (second row) open close))
                                                       in-sight))
                                           :unwrap
                                           :wrap)
                                       :open (string open) :close (string close))
              (setf (tuatara.buffer:buffer-mark-active-p b) nil)))
          (let* ((kind (if (every (lambda (row) (already-wrapped-p b (first row) (second row) open close))
                                  (region-rectangle-rows b))
                           :unwrap
                           :wrap))
                 (goal (multiple-value-bind (top bottom left) (rectangle-lines b)
                         (rows-job-goal b kind top bottom left nil 1 nil (string open)))))
            (let ((changed (rows-change-now b kind :open (string open) :close (string close))))
              (setf (tuatara.buffer:buffer-mark-active-p b) nil)
              (when changed (goto (funcall goal b)))))))))

;;; Large rectangles, changed in the background

;;; A rows job: what changes on each row, read by the job's mapper (to map
;;; positions through the change) and made by its own writer (the text itself)

(define-record (rows-job (:constructor make-rows-job) (:copier nil))
  "What a background job over a rectangle's rows changes. It works in ROPE, on
the lines from the one starting at TOP (line number TOP-LINE) to the one
starting at BOTTOM, the job's text ending at HIGH and the whole text at TOTAL,
between the columns LEFT and RIGHT. CRLF (carriage return and line feed) is
true when a carriage return before a newline ends the lines. The columns are
on the screen, a tab TAB-WIDTH columns at most, a tab across an edge made
spaces outside it as its row changes. With TAB-WIDTH NIL, they count
characters. KIND says what changes. :CUT and :DELETE empty each row. :CHANGE
puts TEXT (octets) in its place, a row short of the block padded out to it
with spaces unless PAD is false (with LEFT = RIGHT, TEXT goes in at that
column, or at the end of a shorter line). :APPEND puts TEXT at the end of each
line. :WRAP puts OPEN before and CLOSE after it, and :UNWRAP takes them off
where they stand just outside it (WRAPPED-MATCH). :PASTE puts SOURCE's rows,
each REPEAT times across, in place of its rows, or with LEFT = RIGHT in at
that column. The rows past the text's end go on new lines (NEWLINE) at its
end. :CLEAR blanks each row, a line ending inside the block cut at its left
column and one shorter than the block left alone. :NUMBER puts in place of
each row what NUMBERING, a function of the row's index from 0, gives (octets),
a short row padded out to the block. :NUMBER-IN puts it in at the block's left
column, replacing nothing. :TYPE writes what was typed at a cursor on each row
\(TYPED-ROW). TEXT goes in place of the columns, or at the line's end with
AT-END. The BACK characters before them and the FORWARD after them are deleted
too. The rows starting at the offsets SKIP holds (a hash table) are left as
they are. REFUSED counts, in its car, the rows :TYPE could not change. FOLDS
are the folds of the text as the job began (FOLDS-NOW). A row whose change
would take part of the text one hides, but not all of it, is left as it is,
and counted in the car of HIDDEN."
  (rope nil :documentation "The text ROWS-JOB works in.")
  (top 0 :type fixnum :documentation "The start of the first line ROWS-JOB changes.")
  (bottom 0 :type fixnum :documentation "The start of the last line ROWS-JOB changes.")
  (top-line 0 :type fixnum :documentation "The line number of the first line ROWS-JOB changes.")
  (high 0 :type fixnum :documentation "Where the text of ROWS-JOB ends.")
  (total 0 :type fixnum :documentation "Where the whole text ends.")
  (left 0 :type fixnum :documentation "The column where the block of ROWS-JOB starts.")
  (right 0 :type fixnum :documentation "The column where the block of ROWS-JOB ends.")
  (crlf nil :documentation "True when a carriage return before a newline ends the lines of ROWS-JOB.")
  (kind nil
        :documentation "What ROWS-JOB changes: :CUT, :DELETE, :CHANGE, :APPEND, :WRAP, :UNWRAP, :PASTE, :CLEAR,
:NUMBER, :NUMBER-IN or :TYPE.")
  (text nil :documentation "The octets ROWS-JOB puts in, for the kinds that put text.")
  (open nil :documentation "The octets ROWS-JOB puts before each row for :WRAP, or takes off for :UNWRAP.")
  (close nil :documentation "The octets ROWS-JOB puts after each row for :WRAP, or takes off for :UNWRAP.")
  (source nil :documentation "The rectangle :PASTE puts in.")
  (repeat 1 :type fixnum :documentation "How many times :PASTE puts each row of its SOURCE across.")
  (newline nil :documentation "The octets that start a new line when :PASTE goes past the end of the text.")
  (pad t :documentation "True when a row short of the block is padded out to it with spaces.")
  (skip nil :documentation "A hash table of the starts of the rows ROWS-JOB leaves as they are.")
  (at-end nil :documentation "True when TEXT goes at the end of the line rather than in place of the columns.")
  (back 0 :type fixnum :documentation "How many octets before the columns :TYPE deletes too.")
  (forward 0 :type fixnum :documentation "How many octets after the columns :TYPE deletes too.")
  (refused (list 0) :documentation "A list whose car counts the rows :TYPE could not change.")
  (numbering nil
             :documentation "The function giving the octets :NUMBER and :NUMBER-IN put in each row, from the row's
index from 0.")
  (folds nil :documentation "The folds of the text as ROWS-JOB began.")
  (hidden (list 0)
          :documentation "A list whose car counts the rows left as they are because their change would take part of a
hidden text.")
  (tab-width nil
             :documentation "The most columns a tab takes on the screen, or NIL when the columns count characters."))

(defun rows-job-slice-keys (spec)
  "The keys TUATARA.TEXT:MAP-LINE-SLICES takes to cut the rows of the job SPEC."
  (tuatara.display:screen-slice-keys (rows-job-crlf spec) (rows-job-tab-width spec)))

(defun row-lead (spec s e sc)
  "The spaces that go before what the job SPEC puts in a row whose columns run
from S to E, S at column SC: a row short of the block padded out to it, when
SPEC pads, and the columns outside the block of a tab across its left edge."
  (if (or (rows-job-pad spec) (< s e)) (max 0 (- (rows-job-left spec) sc)) 0))

(defun row-tail (spec ec)
  "The spaces that go after what the job SPEC puts in a row ending at column
EC: the columns outside the block of a tab across its right edge."
  (max 0 (- ec (rows-job-right spec))))

(defun row-straddled-p (spec s e sc ec)
  "True when a tab crosses an edge of the job SPEC's block on a row whose
columns run from S to E, at the columns SC to EC."
  (and (< s e) (or (< sc (rows-job-left spec)) (> ec (rows-job-right spec)))))

(defun left-tab-p (spec s e sc)
  "True when a tab crosses the left edge of the job SPEC's block on a row whose
columns run from S to E, S at the column SC."
  (and (< s e) (< sc (rows-job-left spec))))

(defun left-tab-inside (spec sc)
  "The columns inside the job SPEC's block of the tab at column SC across its left edge."
  (- (next-tab-stop sc (rows-job-tab-width spec)) (rows-job-left spec)))

(defun typed-row (spec line s e sc ec line-end)
  "Where the typing SPEC describes (the :TYPE kind) changes the row of its text
starting at LINE, whose columns run from S to E (at the columns SC to EC) and
whose text ends at LINE-END, as four values: where the change starts and ends,
and the spaces that go before and after what was typed, a row short of the
columns padded out to them when SPEC pads. It gives NIL when the characters
deleted beside the cursor are not there on this row."
  (let* ((rope (rows-job-rope spec))
         (at-end (rows-job-at-end spec))
         (from (if at-end line-end s))
         (to (if at-end line-end e))
         (pad (if at-end 0 (row-lead spec s e sc)))
         (tail (if at-end 0 (row-tail spec ec)))
         (back (rows-job-back spec)))
    (declare (type fixnum from to pad tail back))
    ;; the characters deleted before the cursor take the padding first
    (dotimes (i (max 0 (- back pad)))
      (when (<= from line) (return-from typed-row nil))
      (setf from (tuatara.text:rope-previous-boundary rope from)))
    (dotimes (i (rows-job-forward spec))
      (when (>= to line-end) (return-from typed-row nil))
      (setf to (tuatara.text:rope-next-boundary rope to)))
    (values from to (- pad back) tail)))

(define-record (row-output (:constructor make-row-output (edit spaces octets text source-row)) (:copier nil))
  "Where the changes to a rectangle's row go, part by part (ROW-EDITS). EDIT, a
function of where the text a change replaces starts and ends, begins each
change; then its replacement comes in order from SPACES (a count of spaces),
OCTETS (an octet vector and where in it the part starts and ends), TEXT (where
a stretch of the job's own text starts and ends) and SOURCE-ROW (the number of
a row of the job's source)."
  (edit nil
        :documentation "The function that begins each change of ROW-OUTPUT, given where the text it replaces starts
and ends.")
  (spaces nil :documentation "A count of spaces ROW-OUTPUT puts in.")
  (octets nil :documentation "An octet vector and where in it the part ROW-OUTPUT puts in starts and ends.")
  (text nil :documentation "Where a stretch of the job's own text starts and ends, for ROW-OUTPUT to put in.")
  (source-row nil :documentation "The number of a row of the job's source for ROW-OUTPUT to put in."))

(defun row-edits (spec k line s e sc ec line-end out)
  "Make the change the job SPEC makes to its row K through OUT (ROW-OUTPUT):
the row of its text starting at LINE, whose columns run from S to E (at the
columns SC to EC) and whose text ends at LINE-END. Every kind of change a rows
job makes is told here once, for its mapper and its writer alike. Returns how
many changes it made, or NIL when the typing could not be made on this row
\(TYPED-ROW)."
  (let ((left (rows-job-left spec)) (right (rows-job-right spec)) (changes 0))
    (declare (type fixnum left right changes))
    (flet ((edit (start end) (incf changes) (funcall (row-output-edit out) start end))
           (spaces (n) (when (plusp n) (funcall (row-output-spaces out) n)))
           (octets (octets) (funcall (row-output-octets out) octets 0 (length octets)))
           (text (from to) (when (< from to) (funcall (row-output-text out) from to))))
      (declare (inline edit spaces octets text))
      (ecase (rows-job-kind spec)
        (:type (multiple-value-bind (from to lead tail) (typed-row spec line s e sc ec line-end)
                 (unless from (return-from row-edits nil))
                 (edit from to)
                 (spaces lead)
                 (octets (rows-job-text spec))
                 (spaces tail)))
        ((:cut :delete) (when (< s e)
                          (edit s e)
                          (spaces (+ (row-lead spec s e sc) (row-tail spec ec)))))
        (:change (edit s e)
                 (spaces (row-lead spec s e sc))
                 (octets (rows-job-text spec))
                 (spaces (row-tail spec ec)))
        (:append (edit line-end line-end)
                 (octets (rows-job-text spec)))
        (:clear (cond ((or (= s e) (= s line-end)))
                      ((= e line-end) (edit s e))
                      (t (edit s e) (spaces (- ec sc)))))
        (:number (edit s e)
                 (spaces (- left sc))
                 (octets (funcall (rows-job-numbering spec) k))
                 (spaces (row-tail spec ec)))
        (:number-in (let ((tab (left-tab-p spec s e sc)))
                      ;; in at the column, a tab across it split into its spaces either side
                      (edit s (if tab (1+ s) s))
                      (spaces (- left sc))
                      (octets (funcall (rows-job-numbering spec) k))
                      (when tab (spaces (left-tab-inside spec sc)))))
        (:wrap (if (row-straddled-p spec s e sc ec)
                   (multiple-value-bind (before from to after)
                       (row-inside (rows-job-rope spec) s e sc ec left right (rows-job-tab-width spec))
                     (edit s e)
                     (spaces (- left sc))
                     (octets (rows-job-open spec))
                     (spaces before)
                     (text from to)
                     (spaces after)
                     (spaces (- right (max (min ec right) left)))
                     (octets (rows-job-close spec))
                     (spaces (row-tail spec ec)))
                   (progn (edit s s)
                          (spaces (- left sc))
                          (octets (rows-job-open spec))
                          (edit e e)
                          (spaces (- right (max ec left)))
                          (octets (rows-job-close spec)))))
        (:unwrap (multiple-value-bind (a z)
                     (wrapped-match (rows-job-rope spec) s e (rows-job-open spec) (rows-job-close spec))
                   (when a
                     (edit a s)
                     (edit e z))))
        (:paste (when (< k (rectangle-source-rows (rows-job-source spec)))
                  (edit s e)
                  (spaces (- left sc))
                  (funcall (row-output-source-row out) k)
                  (spaces (row-tail spec ec))))))
    changes))

(defun row-fold-check (spec)
  "A function of a row of the job SPEC (K LINE S E SC EC LINE-END, as
ROW-EDITS takes them), true when the change made there would take part of the
text one of SPEC's folds hides, but not all of it; NIL when the text had no
folds (ROWS-JOB-FOLDS)."
  (let ((folds (rows-job-folds spec)))
    (when folds
      (let* ((cut nil)
             (none (lambda (&optional a b c) (declare (ignore a b c))))
             (out (make-row-output (lambda (start end)
                                     (when (and (< start end) (tuatara.display:folds-cut folds start end))
                                       (setf cut t)))
                                   none none none none)))
        (lambda (k line s e sc ec line-end)
          (setf cut nil)
          (row-edits spec k line s e sc ec line-end out)
          cut)))))

(setf (documentation 'make-rows-job 'function)
      "Return a ROWS-JOB: what a background job over a rectangle's rows changes.")

(defparameter *no-octets* (tuatara.text:make-octets 0)
  "An empty replacement.")

(defun source-row-reader (spec)
  "A function of a row number giving the octets of that row of SPEC's source
as three values, a vector and where the row starts and ends in it; the rows
are read a chunk at a time, in order."
  (let ((source (rows-job-source spec)) (octets nil) (starts nil) (from 0) (to 0))
    (lambda (k)
      (declare (type fixnum k))
      (unless (and octets (<= from k) (< k to))
        (setf from k
              to (min (rectangle-source-rows source) (+ k 65536)))
        (multiple-value-setq (octets starts) (source-rows-block source from to (rows-job-repeat spec))))
      (values octets (aref starts (- k from)) (aref starts (1+ (- k from)))))))

(defun rows-bound (spec end)
  "Where the lines a piece of SPEC's job ending at END take in stop: END, or
past the last row for the piece that ends the job's text."
  (if (>= end (rows-job-high spec))
      (1+ (rows-job-bottom spec))
      (min end (1+ (rows-job-bottom spec)))))

(defun extra-rows (spec from-row)
  "The replacement, a (LENGTH . WRITER), putting the rows of SPEC's source from
FROM-ROW on at the end of the text, each on a new line at SPEC's column."
  (let* ((rows (rectangle-source-rows (rows-job-source spec)))
         (newline (rows-job-newline spec))
         (column (rows-job-left spec))
         (length 0))
    (let ((read (source-row-reader spec)))
      (loop for k from from-row below rows
            do (multiple-value-bind (octets from to) (funcall read k)
                 (declare (ignore octets))
                 (incf length (+ (length newline) column (- to from))))))
    (cons length
          (lambda (writer) (write-extra-rows spec (source-row-reader spec) from-row writer)))))

(defun write-extra-rows (spec read from-row writer)
  "Write the rows of SPEC's source from FROM-ROW, each read by READ
\(SOURCE-ROW-READER), through WRITER, each on a new line at SPEC's column."
  (let ((newline (rows-job-newline spec)) (column (rows-job-left spec)))
    (loop for k from from-row below (rectangle-source-rows (rows-job-source spec))
          do (tuatara.io:writer-octets writer newline)
          (tuatara.io:writer-spaces writer column)
          (multiple-value-bind (octets from to) (funcall read k)
            (tuatara.io:writer-octets writer octets from to)))))

(defun replacement-collector (rope read emit)
  "A ROW-OUTPUT gathering each change to a rectangle's rows as octets, for a
mapper, and a function of no arguments ending the change gathered last: EMIT
is called with (START END NIL REPLACEMENT) for each, as the next begins or as
it is ended. A part of ROPE's text is copied, a row of the source read by READ
\(SOURCE-ROW-READER). A replacement that is a whole octet vector and nothing
else is that vector itself."
  (let ((start nil) (end 0) (parts 0) (whole nil)
        (gathered (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((add (octets from to)
               (loop for i from from below to do (vector-push-extend (aref octets i) gathered)))
             (unshare ()
               (when whole
                 (add whole 0 (length whole))
                 (setf whole nil)))
             (octets (octets from to)
               (if (and (zerop parts) (= from 0) (= to (length octets)))
                   (setf whole octets)
                   (progn (unshare) (add octets from to)))
               (incf parts))
             (spaces (n)
               (unshare)
               (loop repeat n do (vector-push-extend 32 gathered))
               (incf parts))
             (finish ()
               (when start
                 (funcall emit start end nil
                          (cond (whole)
                                ((zerop (fill-pointer gathered)) *no-octets*)
                                (t (replace (tuatara.text:make-octets (fill-pointer gathered)) gathered))))
                 (setf start nil))))
      (values (make-row-output (lambda (from to)
                                 (finish)
                                 (setf start from end to parts 0 whole nil (fill-pointer gathered) 0))
                               #'spaces
                               #'octets
                               (lambda (from to)
                                 (octets (tuatara.text:rope-octets rope :start from :end to) 0 (- to from)))
                               (lambda (k)
                                 (multiple-value-bind (octets from to) (funcall read k)
                                   (octets octets from to))))
              #'finish))))

(defun rectangle-mapper (spec)
  "The mapper of a job over SPEC's rows: a function of (FUNCTION FROM TO HALT)
calling FUNCTION with (START END NIL REPLACEMENT) for each change to a row
starting in [FROM, TO), in order (ROW-EDITS)."
  (let ((rope (rows-job-rope spec)) (total (rows-job-total spec)) (hidden (row-fold-check spec)))
    (lambda (function from to halt)
      (let* ((first (tuatara.text:rope-line-start rope (max from (rows-job-top spec))))
             (bound (rows-bound spec to))
             (k0 (- (tuatara.text:byte->line rope first) (rows-job-top-line spec)))
             (source (rows-job-source spec))
             (k (1- k0)) (n 0))
        (flet ((change (start end old replacement)
                 (when (>= start from)
                   (funcall function start end old replacement))))
          (multiple-value-bind (out finish)
              (replacement-collector rope (and source (source-row-reader spec)) #'change)
            (block lines
              (when (< first bound)
                (apply #'tuatara.text:map-line-slices
                       (lambda (line s e sc ec line-end)
                         (incf k)
                         (when (and halt (zerop (logand (incf n) 1023)) (funcall halt)) (return-from lines))
                         (unless (or (and (rows-job-skip spec) (gethash line (rows-job-skip spec)))
                                     (and hidden (funcall hidden k line s e sc ec line-end)))
                           (row-edits spec k line s e sc ec line-end out)
                           (funcall finish)))
                       rope first bound (rows-job-left spec) (rows-job-right spec) (rows-job-slice-keys spec)))))
          (when (and source (>= to (rows-job-high spec)) (= to total))
            (let ((from-row (1+ k)))
              (when (< from-row (rectangle-source-rows source))
                (change total total nil (extra-rows spec from-row))))))))))

(define-record (rows-rewrite (:include rewrite) (:constructor %make-rows-rewrite) (:copier nil))
  "A REWRITE over a rectangle's rows (START-ROWS-JOB): SPEC, a ROWS-JOB, says
what changes on each row."
  (spec nil :documentation "The ROWS-JOB saying what JOB changes on each row."))

(defmethod write-piece ((job rows-rewrite) piece)
  "Rewrite PIECE of JOB, a job over its SPEC's rows, into an unnamed file: the
changes ROW-EDITS makes, as the job's mapper gives them, but read a block at a
time, the rows' source a chunk at a time alongside, and written a megabyte at a
time, so that no row looks anything up in the text."
  (let* ((spec (rows-rewrite-spec job))
         (rope (rows-job-rope spec))
         (total (rows-job-total spec))
         (start (rewrite-piece-start piece))
         (end (rewrite-piece-end piece))
         (cancel (rewrite-cancel job))
         (writer (start-piece job piece))
         (interval (rewrite-checkpoint job))
         (next-checkpoint (+ start interval))
         (skip (rows-job-skip spec))
         (hidden (row-fold-check spec))
         (crlf (rows-job-crlf spec))
         (bound (rows-bound spec end))
         (k0 (- (tuatara.text:byte->line rope start) (rows-job-top-line spec)))
         (source (rows-job-source spec))
         (read (and source (source-row-reader spec)))
         (changes 0) (rows 0)
         ;; the row being written: written up to AT, its text read from BLOCK
         (at 0) (block nil) (block-start 0) (block-end 0)
         (out (make-row-output (lambda (from to)
                                 (write-text writer rope at from block block-start block-end)
                                 (setf at to))
                               (lambda (n) (tuatara.io:writer-spaces writer n))
                               (lambda (octets from to) (tuatara.io:writer-octets writer octets from to))
                               (lambda (from to) (write-text writer rope from to block block-start block-end))
                               (lambda (k)
                                 (multiple-value-bind (octets from to) (funcall read k)
                                   (tuatara.io:writer-octets writer octets from to))))))
    (declare (type fixnum total start end next-checkpoint bound k0 changes rows at block-start block-end))
    (block lines
      (when (< start bound)
        (apply #'tuatara.text:map-line-slices
               (lambda (line s e sc ec line-end octets octets-start octets-end)
                 (declare (type fixnum line s e sc ec line-end octets-start octets-end))
                 (when (and (zerop (logand rows 1023)) (car cancel)) (return-from lines))
                 (setf at line block octets block-start octets-start block-end octets-end)
                 (let ((k (+ k0 rows))
                       (next (slice-next-line rope total crlf line-end octets octets-start octets-end)))
                   (declare (type fixnum k next))
                   (cond ((and skip (gethash line skip)))
                         ((and hidden (funcall hidden k line s e sc ec line-end))
                          (tally (rows-job-hidden spec)))
                         (t (let ((made (row-edits spec k line s e sc ec line-end out)))
                              (if made
                                  (incf changes made)
                                  (tally (rows-job-refused spec))))))
                   (write-text writer rope at next block block-start block-end)
                   (incf rows)
                   (setf (rewrite-piece-done piece) next)
                   (when (>= next next-checkpoint)
                     (piece-checkpoint piece next (piece-written piece writer) start)
                     (setf next-checkpoint (+ next interval)))))
               rope start bound (rows-job-left spec) (rows-job-right spec) :octets t (rows-job-slice-keys spec))))
    (unless (car cancel)
      ;; rows past the text's end, each on a new line at the column
      (when (and source (= end total) (>= end (rows-job-high spec)))
        (write-extra-rows spec read (+ k0 rows) writer)
        (when (< (+ k0 rows) (rectangle-source-rows source)) (incf changes)))
      (finish-piece piece writer changes rows))))

(defun rectangle-rows-words (rows)
  "A rectangle's ROWS rows in words, for what its jobs say."
  (format nil "the rectangle's ~:d row~:p" rows))

(defun corner-positions (buffer)
  "Where BUFFER's rectangle has its corners, for a job waiting its turn: the
mark and point, each (OFFSET . BIAS), as QUEUE-IF-BUSY follows them."
  (list (cons (tuatara.buffer:buffer-mark buffer) :before) (cons (tuatara.buffer:buffer-point buffer) :before)))

(defun columns-between (buffer mark point beyond)
  "The left and right columns of a rectangle of BUFFER whose corners stand at
MARK and POINT, point BEYOND columns past where it is (past its line's end)."
  (let ((a (+ beyond (tuatara.display:buffer-screen-column buffer point)))
        (b (tuatara.display:buffer-screen-column buffer mark)))
    (values (min a b) (max a b))))

(defun point-beyond (buffer)
  "How many columns BUFFER's rectangle reaches past point, past its line's end."
  (- (rectangle-point-column buffer)
     (tuatara.display:buffer-screen-column buffer (tuatara.buffer:buffer-point buffer))))

(defun start-rectangle-job (buffer kind &key text open close numbering)
  "Start rewriting the rows of BUFFER's rectangle in the background, as KIND
says (ROWS-JOB): :CUT, :CHANGE with TEXT, :WRAP or :UNWRAP with OPEN and
CLOSE, :CLEAR, or :NUMBER or :NUMBER-IN with NUMBERING; and say so, as every
job at many places says it (JOB-WORDS). Returns the job, or true when another
job runs or waits in BUFFER: it then waits its turn, its rows followed
meanwhile."
  (multiple-value-bind (top bottom left right rows) (rectangle-lines buffer)
    (declare (ignore top bottom))
    (start-on-rectangle-rows buffer (job-words kind :doing (rectangle-rows-words rows)) (block-shade buffer left right)
                             (lambda (top bottom left right)
                               (start-rows-job buffer kind top bottom left right (rows-between buffer top bottom)
                                               :text text :open open :close close :numbering numbering)))))

(defun start-on-rectangle-rows (buffer words shade starter)
  "Start the job STARTER starts on the rows of BUFFER's rectangle, a function
of the offsets where its top and bottom lines start and of its columns: at
once, or queued behind the jobs running or waiting in BUFFER, saying WORDS,
SHADE showing what it will change, its rows followed and its columns worked
out again when its turn comes, a job before it having changed the lines
\(RUN-OR-QUEUE). Returns what STARTER returns, or true when queued."
  (multiple-value-bind (top bottom) (rectangle-lines buffer)
    (let ((beyond (point-beyond buffer)))
      (run-or-queue buffer words (list* (cons top :before) (cons bottom :before) (corner-positions buffer))
                    (lambda (top bottom mark point)
                      (multiple-value-bind (left right) (columns-between buffer mark point beyond)
                        (funcall starter top bottom left right)))
                    :shade shade))))

(defun rows-between (buffer top bottom)
  "How many lines of BUFFER there are from the one starting at TOP to the one starting at BOTTOM."
  (1+ (- (tuatara.buffer:buffer-line-number buffer bottom) (tuatara.buffer:buffer-line-number buffer top))))

(defmethod job-shading ((job rows-rewrite) from to)
  "What JOB, over a rectangle's rows, shades as it goes: the rectangle's
columns on each line shown, one column where a paste goes in at one, the whole
line where text goes at its end, until the result is taken in."
  (let ((spec (rows-rewrite-spec job))
        (pieces (job-pieces-shown job from to))
        (ranges '()))
    (when pieces
      (let* ((rope (tuatara.buffer:buffer-text (rewrite-buffer job)))
             (whole (or (eq (rows-job-kind spec) :append) (rows-job-at-end spec)))
             (left (if whole 0 (rows-job-left spec)))
             (right (if whole 0 (max (rows-job-right spec) (1+ left)))))
        (apply
         #'tuatara.text:map-line-slices
         (lambda (line start end start-column end-column line-end)
           (declare (ignore start-column end-column))
           (when whole (setf start line end line-end))
           (when (< start end)
             (when (find-if (lambda (piece) (and (<= (first piece) start) (< start (second piece)))) pieces)
               (push (list start end :job-queued) ranges))))
         rope (tuatara.text:rope-line-start rope (max from (first (first pieces))))
         (min to (second (car (last pieces)))) left right (rows-job-slice-keys spec))))
    (nreverse ranges)))

(defun rows-job-goal (buffer kind top bottom left source repeat text open)
  "Where point goes when a rows job of KIND over BUFFER's lines from TOP to
BOTTOM is taken in, as the same command done at once leaves it, a function of
the buffer with the job's text in it. It is after the text written on the last
row (TEXT for a change, SOURCE's last row REPEAT times across for a paste). It
is at the column LEFT of the last row cleared. It is after OPEN on the first
row wrapped. It is where OPEN stood on the first row unwrapped. It is NIL for
the others."
  (let* ((top-line (tuatara.buffer:buffer-line-number buffer top))
         (rows (if (and (eq kind :paste) source)
                   (rectangle-source-rows source)
                   (1+ (- (tuatara.buffer:buffer-line-number buffer bottom) top-line))))
         (last-row (and (eq kind :paste) source
                        (multiple-value-bind (octets starts) (source-rows-block source (1- rows) rows (max 1 repeat))
                          (declare (ignore octets))
                          (- (aref starts 1) (aref starts 0))))))
    (flet ((after (row octets)
             (lambda (b)
               (+ (line-screen-column-position b (+ top-line row) left)
                  octets))))
      (case kind
        (:change (after (1- rows) (tuatara.text:string-octet-length text)))
        (:clear (after (1- rows) 0))
        (:wrap (after 0 (tuatara.text:string-octet-length open)))
        (:unwrap (lambda (b) (line-screen-column-position b top-line (max 0 (1- left)))))
        (:paste (and last-row (after (1- rows) last-row)))))))

(defun rows-job-spec (buffer kind top bottom left right
                      &key text open close source (repeat 1) (pad t) skip at-end (back 0) (forward 0)
                        (refused (list 0)) (hidden (list 0)) numbering
                        (tab-width (tuatara.display:tab-width-of buffer)))
  "The ROWS-JOB for the change KIND to BUFFER's lines from the one starting at
TOP to the one starting at BOTTOM, between the columns LEFT and RIGHT, as it
stands now; the other arguments are as START-ROWS-JOB takes them."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (total (tuatara.text:rope-octet-length rope)))
    (make-rows-job :rope rope :top top :bottom bottom :top-line (tuatara.text:byte->line rope top)
                   ;; after the last line's newline, its carriage return too
                   :high (min total (1+ (tuatara.text:rope-line-end rope bottom)))
                   :total total :left left :right right :crlf (buffer-crlf-p buffer) :tab-width tab-width
                   :kind kind :pad pad :skip skip :at-end at-end :back back :forward forward
                   :refused refused :hidden hidden :folds (tuatara.display:folds-now buffer)
                   :text (and text (tuatara.text:string-octets text))
                   :open (and open (tuatara.text:string-octets open))
                   :close (and close (tuatara.text:string-octets close))
                   :source source :repeat repeat :numbering numbering
                   :newline (tuatara.text:string-octets (tuatara.buffer:buffer-newline buffer)))))

(defun start-rows-job (buffer kind top bottom left right rows
                       &key text open close source (repeat 1) (pad t) report announce what verb leaves
                         skip at-end (back 0) (forward 0) (refused (list 0)) (hidden (list 0)) joins numbering
                         (tab-width (tuatara.display:tab-width-of buffer)))
  "Start the background job that rewrites ROWS lines of BUFFER from the one
starting at TOP to the one starting at BOTTOM between the columns LEFT and
RIGHT, as KIND says (ROWS-JOB). The columns are on the screen (a tab TAB-WIDTH
columns at most; with TAB-WIDTH NIL, characters). SOURCE's rows are put REPEAT
times across for a paste. A short row is padded out to the block unless PAD is
false. NUMBERING is used for :NUMBER. For :TYPE, AT-END, BACK and FORWARD are
as ROWS-JOB has them, the rows REFUSED counted in its car. The rows starting
at the offsets SKIP holds (a hash table) are left alone, and so are those
where the change would take part of the text a fold hides, but not all of it,
counted in the car of HIDDEN. REPORT (see REWRITE), ANNOUNCE (the work in
words, said to go on in the background as it starts, or :NONE for nothing),
WHAT (what it does, for the list of jobs, said as it starts unless ANNOUNCE
says otherwise), VERB and LEAVES (REWRITE) are the rectangle's unless given
\(JOB-WORDS, JOB-REPORT). JOINS is the undo stack of a change the job's joins
\(REWRITE). Returns the job."
  (tuatara.buffer:ensure-indexed buffer)
  (let* ((spec (rows-job-spec buffer kind top bottom left right :text text :open open :close close :source source
                              :repeat repeat :pad pad :skip skip :at-end at-end :back back :forward forward
                              :refused refused :hidden hidden :numbering numbering :tab-width tab-width))
         (rope (rows-job-rope spec))
         (high (rows-job-high spec))
         (rows (if source (max rows (rectangle-source-rows source)) rows))
         (what (or what (job-words kind :doing (rectangle-rows-words rows))))
         (job (%make-rows-rewrite
               :spec spec :session t :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
               :low top :high high :words "on the rectangle's rows" :what what :joins joins
               :verb (or verb (job-words kind :verb "the rectangle"))
               :leaves (or leaves "the rectangle is as it was")
               :goal (rows-job-goal buffer kind top bottom left source repeat text open)
               :point (tuatara.buffer:buffer-point buffer)
               :mapper (rectangle-mapper spec)
               :all t :doing (job-words kind :doing)
               :report (or report
                           (job-report kind (lambda (n) (declare (ignore n)) (rectangle-rows-words rows))
                                       "the rectangle" :refused refused :hidden hidden))
               ;; a paste at the very end of the text still has a piece, of no length
               :pieces (if (< top high)
                           (rewrite-pieces-for rope top high)
                           (vector (make-rewrite-piece top top)))
               :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*
               :directory (tuatara.io:buffer-scratch-directory buffer))))
    (launch-rewrite job "tuatara rectangle" (format nil "~:d row~:p, octets ~:d to ~:d" rows top high))
    (unless (eq announce :none) (say-in-background (or announce what)))
    job))

(defun paste-large-rectangle (buffer source times)
  "\\<normal>\\[yank-rectangle] of SOURCE, a rectangle kept by where it
is, in BUFFER, each row TIMES across: over a selected rectangle of as many rows,
or from point, one row a line, new lines made past the text's end; in the
background."
  (check-writable buffer)
  (if (rectangle-live-p buffer)
      (multiple-value-bind (top bottom left right rows) (rectangle-lines buffer)
        (declare (ignore top bottom))
        (if (/= rows (rectangle-source-rows source))
            (message "Rectangle row mismatch: selection has ~:d row~:p, copied rectangle has ~:d; paste refused"
                     rows (rectangle-source-rows source))
            (when (start-on-rectangle-rows buffer (job-words :paste :doing (rectangle-rows-words rows))
                                           (block-shade buffer left right)
                                           (lambda (top bottom left right)
                                             (start-rows-job buffer :paste top bottom left right
                                                             (rows-between buffer top bottom)
                                                             :source source :repeat (max 1 times))))
              (setf (tuatara.buffer:buffer-mark-active-p buffer) nil))))
      (progn
        (multiple-value-bind (start end) (selection-range)
          (when (and start (< start end))
            (delete-spans buffer (list (cons start end)))
            (goto start)))
        (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
        (let* ((p (tuatara.buffer:buffer-point buffer))
               (column (tuatara.display:buffer-screen-column buffer p))
               (line (tuatara.buffer:buffer-line-number buffer p))
               (last (tuatara.text:byte->line (tuatara.buffer:buffer-text buffer) (tuatara.buffer:buffer-size buffer)))
               (bottom-line (min last (+ line (rectangle-source-rows source) -1))))
          (when (plusp times)
            (let ((top (tuatara.buffer:buffer-line-start buffer p))
                  (bottom (tuatara.buffer:buffer-goto-line buffer bottom-line)))
              (run-or-queue buffer (job-words :paste :doing (rectangle-rows-words (rectangle-source-rows source)))
                            (list (cons top :before) (cons bottom :before) (cons p :before))
                            ;; in at point's column as it is when its turn comes
                            (lambda (top bottom at)
                              (let ((column (tuatara.display:buffer-screen-column buffer at)))
                                (start-rows-job buffer :paste top bottom column column (rows-between buffer top bottom)
                                                :source source :repeat times)))
                            ;; a strip a column wide where it goes in
                            :shade (block-shade buffer column (1+ column)))))))))
