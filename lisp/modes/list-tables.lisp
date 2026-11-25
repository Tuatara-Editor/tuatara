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
\(ENTRY-MARK) and its section, gives (TABLE-ROW-PIECES). With SECTIONS, ITEMS
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
                       append (list name value)))))))

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
         (cells (and table (gethash (if (and (consp entry) (eq (first entry) :member)) (second entry) entry)
                                    (second table)))))
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
