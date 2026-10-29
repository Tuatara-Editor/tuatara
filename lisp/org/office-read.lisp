;;;; lisp/org/office-read.lisp --- Excel workbooks and PowerPoint presentations read as Org text, by Tuatara itself, safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Excel workbook (.xlsx, .xlsm, .xltx, .xltm) and a PowerPoint
;;;; presentation (.pptx, .pptm, .potx, .potm, .ppsx, .ppsm) are zip archives
;;;; of XML parts, as a Word document is, and are read here by the same
;;;; reader (docx-read.lisp), whose limits hold: the archive's (sizes, growth,
;;;; checks), each part's and the XML's (no entity of a DTD, nothing fetched,
;;;; nesting and elements held). Nothing in them is run: no formula is worked
;;;; out, no macro run, no link followed, no data connection made.
;;;;
;;;; A workbook is written as an Org note: what the file says of itself, then
;;;; a section for each sheet in its order, named as the workbook names it and
;;;; marked when hidden, very hidden, a chart or a sheet of macros; its cells
;;;; an Org table, the row numbers down its first column and the column
;;;; letters across its first row, empty rows and columns left out, hidden
;;;; ones marked; each cell's value as last saved with the file, and under the
;;;; table the formulas, as written. A presentation: a section for each slide
;;;; in its order, its title as the heading, its text as a list in the order
;;;; of its shapes, tables as Org tables, pictures by their description, the
;;;; speaker notes under it; hidden slides marked. Parts the file holds but no
;;;; list of sheets or slides names are read at the end, and said.

(in-package #:tuatara.defaults)

(defparameter *sheet-most-cells* 200000
  "The most cells of an Excel workbook read as text, all its sheets together:
the cells past them are left out, and the note says where it stopped.")

(defun docx-unlisted-parts (folder listed)
  "The parts of the document being read directly in FOLDER (xl/worksheets/,
ppt/slides/), XML ones, that are not among LISTED, the names of those its own
list names: parts its program never shows, sorted."
  (sort (loop for entry in (docx-reading-entries *docx-reading*)
              for name = (getf entry :name)
              when (and (uiop:string-prefix-p folder name) (uiop:string-suffix-p name ".xml")
                        (not (find #\/ name :start (length folder)))
                        (not (member name listed :test #'string-equal)))
                collect name)
        #'string<))

;;; Excel workbooks

(defun sheet-string-text (item)
  "The text of ITEM, a shared string or a cell's own string of an Excel
workbook: its text and its runs' joined, the reading aids set over East Asian
text left out."
  (with-output-to-string (s)
    (dolist (child (xml-children item))
      (let ((local (xml-local-name child)))
        (cond ((equal local "t") (write-string (xml-all-text child) s))
              ((equal local "r") (dolist (run-text (xml-local-children child "t")) (write-string (xml-all-text run-text) s))))))))

(defun sheet-shared-strings (relations)
  "The shared strings of the workbook being read, a vector, from the part its
RELATIONS name (xl/sharedStrings.xml as standard)."
  (let* ((related (docx-related relations "/sharedStrings"))
         (tree (docx-reading-part (if related (getf related :target) "xl/sharedStrings.xml"))))
    (coerce (mapcar #'sheet-string-text (and tree (xml-find-all tree "si" :local t))) 'vector)))

(defun sheet-format-kind (code)
  "What the number format CODE of a workbook shows a number as: :DATE, a day
\(with its time when it has one), when it shows a day, month or year; :TIME,
a time of day alone, when it shows hours or seconds alone; NIL, the number,
otherwise, and for an elapsed time ([h]:mm). Quoted text and colours are
passed over."
  (let ((plain (cl-ppcre:regex-replace-all "\"[^\"]*\"|\\\\.|_.|\\*." code "")))
    (unless (cl-ppcre:scan "(?i)\\[(h+|m+|s+)\\]" plain)
      (let ((plain (cl-ppcre:regex-replace-all "\\[[^\\]]*\\]" plain "")))
        (cond ((cl-ppcre:scan "(?i)[dy]" plain) :date)
              ((cl-ppcre:scan "(?i)[hs]" plain) :time))))))

(defun sheet-built-in-format-kind (id)
  "What the number format ID built into Excel shows a number as: :DATE, :TIME
or NIL, as SHEET-FORMAT-KIND says."
  (cond ((or (<= 14 id 17) (= id 22) (<= 27 id 36) (<= 50 id 58)) :date)
        ((or (<= 18 id 21) (= id 45) (= id 47)) :time)))

(defun sheet-date-styles (relations)
  "For each cell style of the workbook being read, by its number, what its
number format shows a number as (SHEET-FORMAT-KIND), a vector; from the part
its RELATIONS name (xl/styles.xml as standard)."
  (let* ((related (docx-related relations "/styles"))
         (tree (docx-reading-part (if related (getf related :target) "xl/styles.xml")))
         (codes (make-hash-table)))
    (dolist (format (and tree (xml-find-all tree "numFmt" :local t)))
      (let ((id (ignore-errors (parse-integer (xml-attribute format "numFmtId")))))
        (when id (setf (gethash id codes) (or (xml-attribute format "formatCode") "")))))
    (let ((cell-styles (and tree (first (xml-find-all tree "cellXfs" :local t)))))
      (coerce (mapcar (lambda (style)
                        (let ((id (ignore-errors (parse-integer (or (xml-attribute style "numFmtId") "0")))))
                          (and id (multiple-value-bind (code found) (gethash id codes)
                                    (if found (sheet-format-kind code) (sheet-built-in-format-kind id))))))
                      (and cell-styles (xml-local-children cell-styles "xf")))
              'vector))))

(defun sheet-date-text (value kind date1904)
  "VALUE, a cell's number as the workbook writes it, as the day or time it
stands for when KIND is :DATE (2024-03-01, and 13:45:00 after it when it has
a time) or :TIME (13:45:00); days counted as the workbook counts them, from
1900 (its 29 February 1900 kept) or, with DATE1904, from 1904. A number
below 0, past the year 9999 or written otherwise than in plain digits is
given as written."
  (multiple-value-bind (match parts) (cl-ppcre:scan-to-strings "^(\\d{1,7})(?:\\.(\\d{1,20}))?$" value)
    (if (not match)
        value
        (let* ((whole (parse-integer (aref parts 0)))
               (seconds (if (aref parts 1)
                            (round (* 86400 (parse-integer (aref parts 1))) (expt 10 (length (aref parts 1))))
                            0)))
          (when (= seconds 86400) (incf whole) (setf seconds 0))
          (multiple-value-bind (hours rest) (floor seconds 3600)
            (let ((time (format nil "~2,'0d:~2,'0d:~2,'0d" hours (floor rest 60) (mod rest 60))))
              (cond ((eq kind :time) time)
                    ((> whole 2958465) value)
                    (t (format nil "~a~:[~; ~a~]"
                               (if (and (not date1904) (= whole 60))
                                   "1900-02-29"
                                   (multiple-value-bind (year month day)
                                       (day-date (cond (date1904 (+ whole (day-number 1904 1 1)))
                                                       ((< whole 60) (1- whole))
                                                       (t (- whole 2))))
                                     (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
                               (plusp seconds) time)))))))))

(defun sheet-cell-value (cell strings styles date1904)
  "The value of CELL, a cell of a workbook's sheet, as text: as last saved with
the file (a formula's last result, never worked out here); a string from
STRINGS (the shared strings) or its own; TRUE or FALSE; an error as written
\(#DIV/0!); a number as written, or the day or time it stands for when its
style among STYLES (SHEET-DATE-STYLES) shows a date or a time
\(SHEET-DATE-TEXT, DATE1904 as there)."
  (let ((type (or (xml-attribute cell "t") "n"))
        (value (let ((v (first (xml-local-children cell "v")))) (and v (xml-all-text v)))))
    (cond ((equal type "inlineStr") (let ((own (first (xml-local-children cell "is")))) (if own (sheet-string-text own) "")))
          ((null value) "")
          ((equal type "s")
           (let ((i (ignore-errors (parse-integer value))))
             (if (and i (< -1 i (length strings))) (aref strings i) (format nil "[string ~a, which the workbook does not hold]" value))))
          ((equal type "b") (if (equal (string-trim " " value) "1") "TRUE" "FALSE"))
          ((member type '("str" "e" "d") :test #'equal) value)
          (t (let* ((style (ignore-errors (parse-integer (or (xml-attribute cell "s") ""))))
                    (kind (and style (< -1 style (length styles)) (aref styles style))))
               (if kind (sheet-date-text (string-trim " " value) kind date1904) value))))))

(defun sheet-column-letters (number)
  "The letters naming the column NUMBER of a sheet: A for 1, Z for 26, AA for 27."
  (let ((letters '()))
    (loop while (plusp number)
          do (multiple-value-bind (rest digit) (floor (1- number) 26)
               (push (code-char (+ (char-code #\A) digit)) letters)
               (setf number rest)))
    (coerce letters 'string)))

(defun sheet-cell-place (reference)
  "The column and row numbers a cell's REFERENCE (B12) names, as two values;
NIL for one that names none, or a place past the largest sheet."
  (multiple-value-bind (match parts) (cl-ppcre:scan-to-strings "^\\$?([A-Za-z]{1,3})\\$?(\\d{1,7})$" (or reference ""))
    (when match
      (let ((column (reduce (lambda (n c) (+ (* n 26) (- (char-code (char-upcase c)) 64))) (aref parts 0) :initial-value 0))
            (row (parse-integer (aref parts 1))))
        (and (<= 1 column 16384) (<= 1 row 1048576) (values column row))))))

(defun sheet-write (tree out strings styles date1904 left)
  "Write to OUT the cells of TREE, a workbook's sheet, as SHEET-WRITE-ROWS
does: values as SHEET-CELL-VALUE gives them from STRINGS, STYLES and
DATE1904, formulas as written, at most LEFT cells, each no longer than
*OFFICE-VALUE-LONGEST* characters. Returns how many cells were written."
  (let ((hidden-columns (make-array 16385 :element-type 'bit :initial-element 0))
        (masters (make-hash-table :test 'equal))
        (rows '()) (formulas '()) (count 0) (cut nil) (row-at 0))
    (dolist (column (xml-find-all tree "col" :local t))
      (when (member (xml-attribute column "hidden") '("1" "true") :test #'equal)
        (let ((from (ignore-errors (parse-integer (xml-attribute column "min"))))
              (to (ignore-errors (parse-integer (xml-attribute column "max")))))
          (when (and from to (<= 1 from to))
            (fill hidden-columns 1 :start (min from 16385) :end (min (1+ to) 16385))))))
    (block cells
      (dolist (row (xml-find-all tree "row" :local t))
        (let ((number (or (ignore-errors (parse-integer (xml-attribute row "r"))) (1+ row-at)))
              (hidden (member (xml-attribute row "hidden") '("1" "true") :test #'equal))
              (column-at 0) (cells '()))
          (setf row-at number)
          (dolist (cell (xml-local-children row "c"))
            (let ((column (or (sheet-cell-place (xml-attribute cell "r")) (1+ column-at))))
              (setf column-at column)
              (let* ((value (office-value (sheet-cell-value cell strings styles date1904)))
                     (formula (first (xml-local-children cell "f")))
                     (place (format nil "~a~d" (sheet-column-letters column) number)))
                (when (or (plusp (length value)) formula)
                  (when (>= count left)
                    (setf cut place)
                    (when cells (push (list number hidden (nreverse cells)) rows))
                    (return-from cells))
                  (incf count)
                  (push (cons column value) cells)
                  (when formula
                    (let ((text (office-value (xml-all-text formula *office-value-longest*))) (shared (xml-attribute formula "si")))
                      (when (and shared (plusp (length text))) (setf (gethash shared masters) place))
                      (push (cons place (cond ((plusp (length text)) (format nil "=~a" (org-safe-line text)))
                                              (shared (format nil "as ~a's, shared" (or (gethash shared masters) "another cell")))
                                              (t "a formula of no text")))
                            formulas)))))))
          (when cells (push (list number hidden (nreverse cells)) rows)))))
    (sheet-write-rows (nreverse rows) (lambda (c) (= 1 (bit hidden-columns (min c 16384)))) (nreverse formulas) cut out)
    count))

(defun sheet-write-rows (rows hidden-column-p formulas cut out)
  "Write to OUT the cells of a sheet as an Org table: ROWS, each (NUMBER
HIDDEN CELLS), CELLS each (COLUMN . VALUE) in order; the row numbers down
its first column, the column letters across its first row, the rows and
columns holding nothing left out, those hidden (HIDDEN true, a column by
HIDDEN-COLUMN-P) marked (hidden); then FORMULAS, each (PLACE . TEXT), the
formulas as written, none worked out. Cells spread so thinly that the table
would hold more than five times *SHEET-MOST-CELLS* places are written as a
list instead, each with its place. CUT, the place where the cells read
stopped (*SHEET-MOST-CELLS*), is said, or NIL."
  (let ((columns (sort (remove-duplicates (loop for (nil nil cells) in rows append (mapcar #'car cells))) #'<)))
    (flet ((row-words (number hidden) (format nil "~d~:[~; (hidden)~]" number hidden))
           (column-words (c) (format nil "~a~:[~; (hidden)~]" (sheet-column-letters c) (funcall hidden-column-p c))))
      (cond ((null rows) (format out "It holds no values.~%~%"))
            ((> (* (length rows) (length columns)) (* 5 *sheet-most-cells*))
             ;; a table mostly of empty places: the cells as a list
             (format out "Its cells, too far apart for a table:~%")
             (loop for (number hidden cells) in rows
                   do (loop for (c . value) in cells
                            do (format out "- ~a, row ~a :: ~a~%" (column-words c) (row-words number hidden) (org-safe-line value))))
             (terpri out))
            (t (org-write-table-rows
                (cons (cons "" (mapcar #'column-words columns))
                      (loop for (number hidden cells) in rows
                            collect (cons (row-words number hidden)
                                          ;; the row's cells in the order of the columns, each place once
                                          (let ((sorted (stable-sort (copy-list cells) #'< :key #'car)))
                                            (loop for c in columns
                                                  collect (progn (loop while (and sorted (< (caar sorted) c)) do (pop sorted))
                                                                 (if (and sorted (= (caar sorted) c)) (org-safe-line (cdr (pop sorted))) "")))))))
                out))))
    (when formulas
      (format out "Formulas, as written; the values above are their results as last saved, none worked out here:~%")
      (loop for (place . text) in formulas do (format out "- ~a :: ~a~%" place text))
      (terpri out))
    (when cut
      (format out "Only the first ~:d cells are read: from ~a on, this sheet and those after it are left out (*sheet-most-cells*).~%~%"
              *sheet-most-cells* cut))))

(defun xlsx-read-note (path &key octets cancel)
  "The Excel workbook at PATH as an Org note, from its OCTETS when they are
given: what the file says of itself, then each sheet in its order, a
section named as the sheet, marked when hidden or very hidden (hidden so
that only a program can show it again), a chart or a sheet of macros (never
run); its cells as an Org table and its formulas as written, as
SHEET-WRITE writes them. Values are those last saved with the file, no
formula worked out; a number is shown as written, or as the day or time it
stands for when its cell's format shows a date or a time; at most
*SHEET-MOST-CELLS* cells, the note no larger than *OFFICE-NOTE-LARGEST*
characters. Sheets the file holds that its list of sheets does not name are
read at the end. CANCEL, a cons, stops the reading when its car turns true.
Signals ZIP-ERROR or an error when it cannot be read."
  (let* ((*docx-reading* (docx-reading-of path octets cancel))
         (workbook (docx-main-tree path "xl/workbook.xml" "workbook" "Excel workbook"))
         (main (docx-main-part))
         (relations (docx-part-relations main))
         (strings (sheet-shared-strings relations))
         (styles (sheet-date-styles relations))
         (date1904 (let ((settings (first (xml-find-all workbook "workbookPr" :local t))))
                     (and settings (member (xml-attribute settings "date1904") '("1" "true") :test #'equal) t)))
         (sheets (xml-find-all workbook "sheet" :local t))
         (left *sheet-most-cells*)
         (listed '())
         (hidden 0) (very-hidden 0))
    (flet ((read-sheet (part out)
             (let ((tree (and part (docx-reading-part part))))
               (cond ((null tree) (format out "Its part~@[ ~a~] is not in the file.~%~%" (and part (org-safe-line part))))
                     ((<= left 0) (format out "Not read: the workbook's first ~:d cells were (*sheet-most-cells*).~%~%" *sheet-most-cells*))
                     (t (decf left (sheet-write tree out strings styles date1904 left)))))))
      (let ((body (office-written-text
                   (lambda (s)
                     (loop for sheet in sheets
                           for n from 1
                           do (let* ((related (docx-relation-with-id relations (xml-attribute sheet "r:id")))
                                     (part (docx-relation-target relations (xml-attribute sheet "r:id")))
                                     (type (if related (getf related :type) ""))
                                     (state (xml-attribute sheet "state")))
                                (when part (push part listed))
                                (cond ((equal state "hidden") (incf hidden))
                                      ((equal state "veryHidden") (incf very-hidden)))
                                (format s "* Sheet ~d: ~a~@[ ~a~]~%" n (org-safe-line (or (xml-attribute sheet "name") ""))
                                        (cond ((equal state "hidden") "(hidden)")
                                              ((equal state "veryHidden") "(very hidden: only a program can show it again)")))
                                (cond ((uiop:string-suffix-p type "/chartsheet") (format s "A chart, no cells.~%~%"))
                                      (t (when (search "acrosheet" type)
                                           (format s "A sheet of macros (Excel 4.0): its formulas are a program, never run.~%~%"))
                                         (when (uiop:string-suffix-p type "/dialogsheet")
                                           (format s "A dialogue sheet.~%~%"))
                                         (read-sheet part s)))))
                     (dolist (part (append (docx-unlisted-parts "xl/worksheets/" listed) (docx-unlisted-parts "xl/macrosheets/" listed)))
                       (format s "* Sheet part ~a (in the file, named by no list of sheets)~%" (org-safe-line part))
                       (read-sheet part s))))))
        (office-note path
                     (docx-read-about path :what "workbook" :program "Excel" :usual "xl/workbook.xml"
                                           :more (with-output-to-string (s)
                                                   (format s "- Sheets :: ~d~@[, ~d hidden~]~@[, ~d very hidden~]~%"
                                                           (length sheets) (and (plusp hidden) hidden) (and (plusp very-hidden) very-hidden))
                                                   (let ((connections (let ((tree (docx-reading-part "xl/connections.xml")))
                                                                        (and tree (xml-find-all tree "connection" :local t)))))
                                                     (when connections
                                                       (format s "- Data connections :: ~{~a~^, ~} (none made)~%"
                                                               (mapcar (lambda (c) (org-safe-line (or (xml-attribute c "name") "unnamed"))) connections))))
                                                   (format s "- Values :: as last saved with the file; no formula is worked out here~%")))
                     body)))))

;;; PowerPoint presentations

(defun slide-paragraph-text (paragraph)
  "The text of PARAGRAPH, a paragraph of a slide, on one line: its runs' and
fields' text joined, a line break a space."
  (with-output-to-string (s)
    (labels ((walk (node)
               (let ((local (xml-local-name node)))
                 (cond ((null local))
                       ((equal local "t") (write-string (xml-all-text node) s))
                       ((equal local "br") (write-char #\Space s))
                       (t (mapc #'walk (xml-children node)))))))
      (mapc #'walk (xml-children paragraph)))))

(defun slide-placeholder (shape)
  "The kind of placeholder SHAPE of a slide is (title, ctrTitle, body), or NIL
for a shape of its own; a placeholder of no kind given is body."
  (let ((placeholder (first (xml-find-all (first (xml-local-children shape "nvSpPr")) "ph" :local t))))
    (and placeholder (or (xml-attribute placeholder "type") "body"))))

(defun slide-title-p (shape)
  "True when SHAPE is a slide's title."
  (member (slide-placeholder shape) '("title" "ctrTitle") :test #'equal))

(defun slide-write-shapes (tree out &key only)
  "Write to OUT the text of the shapes of TREE, a slide or its notes, in
their order, as Org text: each paragraph an item of a list, indented by its
level; a table an Org table; a picture [picture: its description]; the
title left out (it is the heading). With ONLY, a placeholder kind, the text
of those shapes alone (body, a notes page's notes)."
  (labels ((paragraphs (shape)
             (dolist (paragraph (xml-find-all shape "p" :local t))
               (let ((text (string-trim " " (slide-paragraph-text paragraph))))
                 (office-spend (length text))
                 (when (plusp (length text))
                   (let* ((look (first (xml-local-children paragraph "pPr")))
                          (level (min 8 (or (ignore-errors (parse-integer (xml-attribute look "lvl"))) 0))))
                     (format out "~a- ~a~%" (make-string (* 2 level) :initial-element #\Space) (org-safe-line text)))))))
           (walk (node)
             (let ((local (xml-local-name node)))
               (cond ((null local))
                     ((equal local "sp")
                      (when (if only (equal (slide-placeholder node) only) (not (slide-title-p node)))
                        (paragraphs node)))
                     (only (mapc #'walk (xml-children node)))
                     ((equal local "tbl")
                      (fresh-line out)
                      (org-write-table-rows (loop for row in (xml-local-children node "tr")
                                                  collect (loop for cell in (xml-local-children row "tc")
                                                                collect (org-safe-line (office-value
                                                                                   (format nil "~{~a~^ ~}"
                                                                                           (mapcar (lambda (p) (string-trim " " (slide-paragraph-text p)))
                                                                                                   (xml-find-all cell "p" :local t)))))))
                                            out))
                     ((equal local "pic")
                      (let* ((names (first (xml-find-all node "cNvPr" :local t)))
                             (words (and names (find-if (lambda (w) (and w (plusp (length w))))
                                                        (list (xml-attribute names "descr") (xml-attribute names "name"))))))
                        (format out "- [picture~@[: ~a~]]~%" (and words (org-safe-line words)))))
                     ;; the same content more than one way: the first, what a program that reads it shows
                     ((equal local "AlternateContent") (let ((choice (first (xml-local-children node "Choice")))) (when choice (walk choice))))
                     (t (mapc #'walk (xml-children node)))))))
    (walk tree)))

(defun slide-write (part number hidden out)
  "Write to OUT the slide PART of the presentation being read, its NUMBER in
order, HIDDEN when it is hidden: a heading with its title, its text
\(SLIDE-WRITE-SHAPES), its speaker notes under a heading of their own."
  (let* ((tree (docx-reading-part part))
         (title (and tree (format nil "~{~a~^ ~}"
                                  (loop for shape in (xml-find-all tree "sp" :local t)
                                        when (slide-title-p shape)
                                          append (loop for p in (xml-find-all shape "p" :local t)
                                                       for text = (string-trim " " (slide-paragraph-text p))
                                                       when (plusp (length text)) collect text)))))
         (hidden (or hidden (and tree (member (xml-attribute (xml-root tree) "show") '("0" "false") :test #'equal)))))
    (format out "* ~a~:[~; (hidden)~]~@[: ~a~]~%"
            (if number (format nil "Slide ~d" number) (format nil "Slide part ~a (in the file, named by no list of slides)" (org-safe-line part)))
            hidden (and title (plusp (length title)) (org-safe-line title)))
    (if (null tree)
        (format out "Its part is not in the file.~%~%")
        (let ((notes (docx-related (docx-part-relations part) "/notesSlide")))
          (slide-write-shapes tree out)
          (let ((notes-tree (and notes (docx-reading-part (getf notes :target)))))
            (when notes-tree
              (let ((text (with-output-to-string (s) (slide-write-shapes notes-tree s :only "body"))))
                (when (plusp (length text))
                  (format out "** Speaker notes~%~a" text)))))
          (terpri out)))))

(defun pptx-read-note (path &key octets cancel)
  "The PowerPoint presentation at PATH as an Org note, from its OCTETS when
they are given: what the file says of itself, then each slide in its order,
a heading with its number and title, marked (hidden) when it is not shown in
the show; its text in the order of its shapes, as SLIDE-WRITE-SHAPES writes
it; its speaker notes under a heading of their own. A slide its list names
again is said to be the one named before, not written again. Slides the file
holds that its list of slides does not name are read at the end. The note is
no larger than *OFFICE-NOTE-LARGEST* characters; CANCEL, a cons, stops the
reading when its car turns true. Signals ZIP-ERROR or an error when it
cannot be read."
  (let* ((*docx-reading* (docx-reading-of path octets cancel))
         (presentation (docx-main-tree path "ppt/presentation.xml" "presentation" "PowerPoint presentation"))
         (main (docx-main-part))
         (relations (docx-part-relations main))
         (listed (make-hash-table :test 'equal))
         (body (office-written-text
                (lambda (s)
                  (loop for id in (xml-find-all presentation "sldId" :local t)
                        for n from 1
                        do (let* ((part (docx-relation-target relations (xml-attribute id "r:id")))
                                  (before (and part (gethash part listed))))
                             (office-spend 80)
                             (cond (before (format s "* Slide ~d: slide ~d again (~a)~%~%" n before (org-safe-line part)))
                                   (t (when part (setf (gethash part listed) n))
                                      (slide-write (or part "") n nil s)))))
                  (dolist (part (docx-unlisted-parts "ppt/slides/" (loop for part being the hash-keys of listed collect part)))
                    (slide-write part nil nil s))))))
    (office-note path
                 (docx-read-about path :what "presentation" :program "PowerPoint" :usual "ppt/presentation.xml")
                 body)))
