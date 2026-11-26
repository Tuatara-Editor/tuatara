;;;; lisp/modes/list-tables.lisp --- Lists laid out in columns under titles.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; Columns

(defun hanging-columns (head &key cells)
  "The columns in where the text of the line beginning HEAD starts, once its
indentation, its list bullet (-, + or *, a number and . or )) and its checkbox
are passed: where the rows the line wraps into (\\[soft-wrap-mode]) line up.
With CELLS, an indented line with no bullet that has a second cell, set apart
from the first by two spaces or more, hangs under that cell, as a table's
rows do. At most 30, so that a narrow window keeps room for its text."
  (let* ((end (nth-value 1 (cl-ppcre:scan "^[ \\t]*(?:(?:[-+*]|[0-9]+[.)])[ \\t]+(?:\\[[ X-]\\][ \\t]+)?)?" head)))
         (indent (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) head) (length head)))
         (second-cell (and cells (plusp indent) (= end indent)
                           (let ((gap (search "  " head :start2 indent)))
                             (and gap (position #\Space head :start gap :test #'char/=))))))
    (min 30 (tuatara.display:string-columns head :end (or second-cell end)))))

(defun hanging-prefix (buffer line-start continuation)
  "What a buffer shows before the rows a long line wraps into
\(\\[soft-wrap-mode]), its :line-prefix-function: as many spaces as its HANGING
setting says, a number, more by as far as a row of a section held in others is
set in (ROW-INDENT), or for :CELLS where the text of the line beginning at
LINE-START starts (HANGING-COLUMNS); nothing before a line's first row
\(CONTINUATION false). The rows then continue under their own text."
  (when continuation
    (let* ((hang (tuatara.buffer:buffer-local buffer 'hanging))
           (columns (cond ((integerp hang) (min 30 (+ hang (row-indent buffer line-start))))
                          (hang (hanging-columns (buffer-line-head buffer line-start 128) :cells t))
                          (t 0))))
      (and (plusp columns) (spaces columns)))))

(defun set-hanging (buffer hang)
  "Make the rows a long line of BUFFER wraps into continue under their own
text: HANG, a number, is how many columns in they start, :CELLS has each line's
own indentation, bullet or second cell say (HANGING-PREFIX); NIL stops it."
  (setf (tuatara.buffer:buffer-local buffer 'hanging) hang
        (tuatara.buffer:buffer-local buffer :line-prefix-function) (and hang 'hanging-prefix)))

(defun column-text (text width &key right (elide t))
  "TEXT in a column WIDTH columns wide: padded with spaces after it, or before
it when RIGHT; when it is wider, cut, with ... for its last three, or left
whole unless ELIDE."
  (let ((columns (text-columns text)))
    (cond ((and (> columns width) (not elide)) text)
          ((> columns width)
           (let ((cut (values (tuatara.display:fit-columns text '() width :ellipsis "..."))))
             (concatenate 'string cut (spaces (- width (text-columns cut))))))
          (right (concatenate 'string (spaces (- width columns)) text))
          (t (concatenate 'string text (spaces (- width columns)))))))

(defun table-list-cell-text (cell)
  "The text of CELL, a string or (TEXT . FACE) as TABLE-ROW-PIECES takes it."
  (if (consp cell) (car cell) cell))

(defun kept-row-pieces (columns row-cells key texts)
  "The pieces of a row of ROW-CELLS under COLUMNS (TABLE-ROW-PIECES), the
texts of its cells kept in TEXTS under KEY, the entry the row is about, unless
KEY is NIL; the texts as a second value. Touches no list: a job's thread may
make rows."
  (let ((cell-texts (map 'simple-vector #'table-list-cell-text row-cells)))
    (when key (setf (gethash key texts) cell-texts))
    (values (table-row-pieces columns row-cells) cell-texts)))

(defun table-list-rows (columns items cells &key (key #'identity))
  "The rows a table list shows of ITEMS under COLUMNS, made apart from any
list, so on a job's thread too, for RENDER-MADE-TABLE-LIST: CELLS, a function
of an item, gives its cells, KEY the entry its line is about, NIL for a line
about none. Three values: the pieces of the rows, a table from each entry to
the texts of its cells, and the first row's texts."
  (let ((texts (make-hash-table :test 'eq)) (first nil))
    (values (loop for item in items
                  append (multiple-value-bind (pieces cell-texts)
                             (kept-row-pieces columns (funcall cells item) (funcall key item) texts)
                           (unless first (setf first cell-texts))
                           pieces))
            texts first)))

(defun render-made-table-list
    (list columns rows texts first &rest options &key before after first-line (margin 0) &allow-other-keys)
  "Fill LIST with a table whose ROWS are made (TABLE-LIST-ROWS, TEXTS and
FIRST as it gives them): the titles of COLUMNS over them, BEFORE and AFTER,
pieces, above the titles and below the rows; the columns kept for a filter to
try a pattern on one (KEEP-LIST-TABLE). The other OPTIONS go to
RENDER-PIECES, FIRST-LINE counted from the titles when not given. A last column
with no width continues, where a long row wraps (\\[soft-wrap-mode]), under
its own start (SET-HANGING). With MARGIN, the titles and the lines of AFTER
are set in that many columns, as ROWS are by their maker (SET-IN-PIECES)."
  (keep-list-table list (map 'simple-vector (lambda (column) (string-trim " " (first column))) columns) texts first)
  (setf (tuatara.buffer:buffer-local list 'table-rows) nil)
  (set-hanging list (and (rest columns) (not (getf (rest (car (last columns))) :width))
                         (+ 1 margin (table-columns-width (butlast columns) 0))))
  (apply #'render-pieces list
         (append before (set-in-pieces (table-heading-pieces columns) margin) rows (set-in-pieces after margin))
         :first-line (or first-line (+ 2 (count #\Newline (pieces-text before))))
         (loop for (name value) on options by #'cddr
               unless (member name '(:before :after :first-line :margin))
                 append (list name value))))

(defun render-table-list (list columns items cells
                          &rest options
                          &key (marked-by (lambda (item section) (declare (ignore section)) item))
                               sections section-title section-face section-note (keep-marks t)
                          &allow-other-keys)
  "Fill LIST with a table (TABLE-HEADING-PIECES): the titles of COLUMNS over a
row for each of ITEMS, its cells what CELLS, a function of the item, its mark
\(ENTRY-MARK) and its section, gives (TABLE-ROW-PIECES); CELLS must give the
same row for the same item and mark whenever it is called, for a row is made
again alone when its mark changes. With SECTIONS, ITEMS
are sections, each (SECTION . MEMBERS), under headings that fold
\(SECTIONED-PIECES, a section holding sections in place of members as it says;
SECTION-TITLE naming each, SECTION-FACE giving the face of its heading,
SECTION-NOTE words after it in the colour of warnings), the titles and the rows
set in two columns, so that a row starts under the text of its heading.
MARKED-BY, a function of an item and its section, gives what its mark is kept
by; with KEEP-MARKS, the marks of items no longer listed are dropped. The other
OPTIONS go to RENDER-MADE-TABLE-LIST: BEFORE and AFTER, pieces above the titles
and below the rows, and those of RENDER-PIECES. Each row's cells are kept as
text, so that \\[search-filter-show-records] can try a pattern on one column."
  (let ((texts (make-hash-table :test 'eq)) (first nil))
    (flet ((row (item section)
             (multiple-value-bind (pieces cell-texts)
                 (kept-row-pieces columns
                                  (funcall cells item (entry-mark list (funcall marked-by item section)) section)
                                  item texts)
               (unless first (setf first cell-texts))
               pieces)))
      (when keep-marks
        (keep-list-marks list (if sections
                                  (let ((keys '()))
                                    (map-section-members (lambda (member section)
                                                           (push (funcall marked-by member section) keys))
                                                         items)
                                    keys)
                                  (mapcar (lambda (item) (funcall marked-by item nil)) items))))
      (multiple-value-bind (entries rows)
          (if sections
              (sectioned-pieces list items section-title #'row :section-face section-face :section-note section-note
                                :margin 2)
              (values items (loop for item in items append (row item nil))))
        (apply #'render-made-table-list list columns rows texts first
               :entries entries :margin (if sections 2 0)
               :mark-key (if sections
                             (lambda (entry)
                               (let ((member (entry-member entry)))
                                 (and member (funcall marked-by member (third entry)))))
                             (lambda (entry) (funcall marked-by entry nil)))
               (loop for (name value) on options by #'cddr
                     unless (member name '(:marked-by :sections :section-title :section-face :section-note :keep-marks))
                       append (list name value)))
        ;; how a row is made again by REDRAW-TABLE-ROWS
        (setf (tuatara.buffer:buffer-local list 'table-rows) (list columns cells marked-by sections))))))

(defun redraw-table-rows (list items)
  "Draw the rows of ITEMS again in LIST, a table made by RENDER-TABLE-LIST,
each from what its CELLS give now, in place of its line: after a change to a
few items (a mark, a label) the rest of the list is left as it is. An item
in a folded section, or not listed, is passed over. The whole list is drawn
again (REDRAW-LIST) while search filters narrow it, since they may now leave
other entries, or when it was not made by RENDER-TABLE-LIST. O(entries + face
spans) to find the rows and move what follows them, without making the other
rows again."
  (let ((rows (tuatara.buffer:buffer-local list 'table-rows))
        (table (tuatara.buffer:buffer-local list 'list-table))
        (entries (tuatara.buffer:buffer-local list 'list-entries)))
    (if (or (null rows) (null table) (not (simple-vector-p entries)) (list-filters-on-p list))
        (redraw-list list)
        (destructuring-bind (columns cells marked-by sections) rows
          (let ((wanted (make-hash-table :test 'eq)) (first-line (tuatara.buffer:buffer-local list 'list-first-line 0)))
            (dolist (item items) (setf (gethash item wanted) t))
            (call-keeping-places
             list
             (lambda ()
               (loop for entry across entries
                     for line from first-line
                     for item = (if sections (entry-member entry) entry)
                     when (and item (gethash item wanted)
                               (not (redraw-table-row list line item (and sections (third entry))
                                                      columns cells marked-by table)))
                       ;; a line not as its row was drawn: the whole list made again
                       do (return (redraw-list list))))))))))

(defun call-keeping-places (buffer change)
  "Call CHANGE, a function of no arguments that makes lines of BUFFER again
without adding or taking away any, and keep point and the point and first
line of each window showing it on the line and column they were on, as far as
the line now goes."
  (let* ((windows (and *editor* (buffer-windows buffer)))
         (own (remove-if-not #'tuatara.display:window-point-anchor windows)))
    (flet ((place (position)
             (let ((start (tuatara.buffer:buffer-line-start buffer position)))
               (cons (tuatara.buffer:buffer-line-number buffer position) (- position start))))
           (back (place)
             (let ((start (tuatara.buffer:buffer-goto-line buffer (car place))))
               (min (+ start (cdr place)) (tuatara.buffer:buffer-line-end buffer start)))))
      (let ((point (place (tuatara.buffer:buffer-point buffer)))
            (points (mapcar (lambda (window) (place (tuatara.display:window-point window))) own))
            (starts (mapcar (lambda (window) (place (tuatara.display:window-start window))) windows)))
        (funcall change)
        (setf (tuatara.buffer:buffer-point buffer) (back point))
        (loop for window in own for place in points
              do (setf (tuatara.display:window-point window) (back place)))
        (loop for window in windows for place in starts
              do (setf (tuatara.display:window-start window) (back place)))))))

(defun redraw-table-row (list line item section columns cells marked-by table)
  "Put the row of ITEM, in SECTION, made again from CELLS under COLUMNS, in
place of LINE of LIST, its indentation kept, its cells' texts kept in TABLE
\(KEEP-LIST-TABLE), and move the face spans and buttons after it by as far as
its length changed: the work of REDRAW-TABLE-ROWS for one row. NIL, with
nothing changed, when the line does not hold the row as it was drawn."
  (let* ((texts (second table)) (old (gethash item texts)))
    (when old
      (let* ((old-text (pieces-text (table-row-pieces columns (coerce old 'list))))
             (start (tuatara.buffer:buffer-goto-line list line))
             (end (tuatara.buffer:buffer-line-end list start))
             ;; the spaces a row in a section is set in, one octet each, ahead of its cells
             (from (- end (1- (tuatara.text:string-octet-length old-text)))))
        (when (and (<= start from end)
                   (string= (tuatara.buffer:buffer-string list :start from :end end) old-text
                            :end2 (1- (length old-text)))
                   (notany (lambda (button) (and (< (first button) end) (> (second button) from)))
                           (tuatara.buffer:buffer-local list :buttons)))
          (multiple-value-bind (text spans)
              (pieces-text (kept-row-pieces columns
                                            (funcall cells item (entry-mark list (funcall marked-by item section))
                                                     section)
                                            item texts))
            (let* ((new (subseq text 0 (1- (length text))))
                   (change (- (tuatara.text:string-octet-length new) (- end from))))
              (when (eq old (third table))
                (setf (third table) (gethash item texts)))
              (tuatara.buffer:with-read-only-lifted (list)
                (tuatara.buffer:buffer-replace list from end new))
              (tuatara.buffer:forget-history list)
              (tuatara.buffer:buffer-mark-saved list)
              (unless (zerop change)
                (setf (tuatara.buffer:buffer-local list :buttons)
                      (loop for button in (tuatara.buffer:buffer-local list :buttons)
                            collect (if (>= (first button) end)
                                        (list* (+ (first button) change) (+ (second button) change) (cddr button))
                                        button))))
              (move-made-spans list from end change
                               (loop for (span-start span-end face) in spans
                                     collect (list (+ from span-start) (+ from span-end) face)))
              t)))))))

(defun move-made-spans (buffer from to change new)
  "Put NEW, face spans each (START END FACE), in place of those of BUFFER's
made text (SET-MADE-TEXT) that start from FROM below TO, and move those after
them by CHANGE octets: what a part of that text made again in place needs.
O(spans)."
  (let* ((made (tuatara.buffer:buffer-local buffer 'made-spans))
         (spans (if made (car made) #()))
         (out (make-array (+ (length spans) (length new)) :fill-pointer 0)))
    (loop for span across spans
          for (start end face) = span
          do (cond ((< start from) (vector-push span out))
                   ((< start to))
                   ((zerop change) (vector-push span out))
                   (t (vector-push (list (+ start change) (+ end change) face) out)))
          finally (let ((at (or (position-if (lambda (span) (>= (first span) from)) out) (length out))))
                    (setf out (concatenate 'simple-vector (subseq out 0 at) new (subseq out at)))))
    (setf (tuatara.buffer:buffer-local buffer 'made-spans) (and (plusp (length out)) (cons out (span-ends out))))))

(defun keep-list-table (list titles texts first)
  "Keep the columns of LIST, as it was drawn, for a filter to try a pattern on
one (LIST-TABLE-COLUMNS, LIST-TABLE-CELL): TITLES, a vector of their titles;
TEXTS, a table from each entry to a vector of the texts of its cells; FIRST,
the first row's, shown as an example. TITLES NIL: the list has no columns."
  (setf (tuatara.buffer:buffer-local list 'list-table) (and titles (list titles texts first))))

(defun list-table-cell (list entry column)
  "The text of COLUMN (from 0) of ENTRY's row in LIST, a table made by
RENDER-TABLE-LIST, as it was last drawn; NIL when it has none."
  (let* ((table (tuatara.buffer:buffer-local list 'list-table))
         (cells (and table (gethash (entry-member-or-self entry) (second table)))))
    (and cells (< column (length cells)) (svref cells column))))

(defun list-table-columns (list)
  "The columns of LIST, a table made by RENDER-TABLE-LIST, a pattern may be
tried on, as (TITLE COLUMN SAMPLE): those with a title, SAMPLE the first
row's text; NIL for a list that is no such table."
  (let ((table (tuatara.buffer:buffer-local list 'list-table)))
    (when table
      (let ((first (third table)))
        (loop for title across (first table)
              for column from 0
              when (plusp (length title))
                collect (list title column (and first (< column (length first)) (svref first column))))))))

(defun table-cell (column text)
  "TEXT in COLUMN (see TABLE-HEADING-PIECES), its control characters escaped
\(ESCAPE-CONTROLS): cut or padded to its width, as COLUMN-TEXT does, or as it
is in a column with no width."
  (destructuring-bind (title &key width right (elide t) shade) column
    (declare (ignore title shade))
    (let ((text (escape-controls text)))
      (if width (column-text text width :right right :elide elide) text))))

(defun table-columns-width (columns last)
  "The columns a row of COLUMNS (TABLE-HEADING-PIECES) takes: the width of each,
LAST for a last column with no width, and a space between each two."
  (+ (loop for column in columns sum (or (getf (rest column) :width) last))
     (max 0 (1- (length columns)))))

(defun table-heading-pieces (columns)
  "The two lines over the rows of a table, as pieces for RENDER-PIECES: the
titles of COLUMNS and dashes under them, both in :header-line. Each column is
\(TITLE &key WIDTH RIGHT ELIDE SHADE): TITLE heads it; its cells are WIDTH
columns wide (the last may have no width, taking the rest of the line), RIGHT
set to the right, cut with ... unless ELIDE is NIL; with SHADE a cell's face
covers the spaces either side of it too (TABLE-ROW-PIECES). One space between
columns."
  (let ((title (format nil "~{~a~^ ~}" (mapcar (lambda (column) (table-cell column (first column))) columns))))
    (list (cons title :header-line) (string #\Newline)
          (cons (substitute-if #\- (lambda (c) (char/= c #\Space)) title) :header-line) (string #\Newline))))

(defun table-row-pieces (columns cells)
  "A row of a table, as pieces for RENDER-PIECES: CELLS, each a string or
\(TEXT . FACE), in COLUMNS (TABLE-HEADING-PIECES), one space between them, and
the line's end."
  (let ((pieces '()) (spaced t))
    (loop for column in columns
          for cell in cells
          do (multiple-value-bind (text face) (if (consp cell) (values (car cell) (cdr cell)) (values cell nil))
               (let ((text (table-cell column text)))
                 (if (getf (cdr column) :shade)
                     (progn (push (cons (format nil "~:[ ~;~]~a " spaced text) face) pieces)
                            (setf spaced t))
                     (progn (unless spaced (push " " pieces))
                            (push (cons text face) pieces)
                            (setf spaced nil))))))
    (nreverse (cons (string #\Newline) pieces))))
