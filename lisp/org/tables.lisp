;;;; lisp/org/tables.lisp --- Org's tables: aligned as Org aligns them, TAB and RET between their fields.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In an Org table (lines beginning with |), TAB aligns it and goes to the
;;;; next field, a new row made after the last; S-TAB goes back a field; RET
;;;; goes to the row below, a new one made at the end or before a rule; C-c
;;;; C-c aligns it. A column of numbers is aligned right, as Org aligns it,
;;;; and <l>, <c> or <r> alone in a field aligns its column so. |- becomes a
;;;; rule as wide as the table. M-j and M-k move the row, M-h and M-l the
;;;; column, M-H deletes the column and M-L inserts one, M-S-up deletes the
;;;; row and M-S-down inserts one, C-c - puts a rule below: each one step to
;;;; undo.

(in-package #:tuatara.defaults)

(defstruct (org-table-model (:conc-name org-table-))
  "An Org table as its fields: INDENT, the white space before its rows; ROWS,
each :RULE or a list of its fields' text; ROW, FIELD and OFFSET, where point is
(the row, the field, the character of the field's text, or :START before the
spaces that align it); BEGIN and END, the
characters of its rows in the text; NEWLINE, true when they end with one;
FORMULAS, the formulas of its #+TBLFM: line, FORMULAS-END where that line ends
(NIL when it has none), FORMULAS-CHANGED true when they are to be written."
  indent rows row field offset begin end newline formulas formulas-end formulas-changed)

(defun org-table-at (tree position)
  "The Org table (not a table.el one) of TREE whose rows hold the line of the character POSITION, or NIL."
  (let ((line-start (string-line-start *org-current-text* position)))
    (org-node-at tree line-start
                 (lambda (node)
                   (and (eq (org-node-type node) 'table) (eq (org-property node :type) 'org)
                        (org-node-contents-begin node) (org-node-contents-end node)
                        (<= (org-node-contents-begin node) line-start) (< line-start (org-node-contents-end node)))))))

(defun org-table-rule-line-p (line)
  "True when LINE is a table's rule: | then -."
  (cl-ppcre:scan "^[ \\t]*\\|-" line))

(defun org-table-line-fields (line)
  "The raw fields of the table row LINE, each (START . END) of LINE: what is between its bars."
  (let* ((first-bar (position #\| line))
         (trimmed-end (length (string-right-trim " 	" line)))
         (fields '()))
    (when first-bar
      (loop with start = (1+ first-bar)
            for bar = (position #\| line :start start)
            do (if bar
                   (progn (push (cons start bar) fields) (setf start (1+ bar)))
                   (progn (when (< start trimmed-end) (push (cons start (length line)) fields))
                          (loop-finish)))))
    (nreverse fields)))

(defun org-table-read (text table position)
  "TABLE, a table node of TEXT, as an ORG-TABLE-MODEL, point at the character POSITION."
  (let* ((begin (org-node-contents-begin table))
         (end (org-node-contents-end table))
         (newline (and (< begin end) (char= (char text (1- end)) #\Newline)))
         (lines (uiop:split-string (subseq text begin (if newline (1- end) end)) :separator '(#\Newline)))
         (indent (let ((first (first lines))) (subseq first 0 (or (position #\| first) 0))))
         (row 0) (field 0) (offset 0)
         (tblfm (first (org-property table :tblfm))))
    (loop for line in lines
          for line-start = begin then (+ line-start (length previous) 1)
          for previous = line
          for index from 0
          when (and (<= line-start position) (<= position (+ line-start (length line))))
            do (let ((at (- position line-start)))
                 (setf row index
                       field (max 0 (1- (count #\| line :end at))))
                 (let ((raw (nth field (org-table-line-fields line))))
                   (setf offset (if raw
                                    (let ((content-start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line
                                                                              :start (car raw) :end (cdr raw))
                                                             (cdr raw))))
                                      (max 0 (- at content-start)))
                                    0)))))
    (make-org-table-model
     :indent indent :row row :field field :offset offset :begin begin :end end :newline newline
     :formulas (and tblfm (org-table-formula-texts tblfm))
     :formulas-end (and tblfm (let ((nl (position #\Newline text :start end))) (if nl (1+ nl) (length text))))
     :rows (loop for line in lines
                 collect (if (org-table-rule-line-p line)
                             :rule
                             (loop for (s . e) in (org-table-line-fields line)
                                   collect (string-trim " 	" (subseq line s e))))))))

(defun org-table-columns (model)
  "How many columns MODEL's table has: the fields of its longest row."
  (loop for row in (org-table-rows model) unless (eq row :rule) maximize (length row)))

(defun org-string-columns (string)
  "How many columns STRING, a table's field, takes on screen: a link as its
label ([[target][label]] as label, [[target]] as target), and a macro call
with arguments as them ({{{R(John Otago)}}} as John Otago), while links are shown so
\(*ORG-LINK-DESCRIPTIVE*)."
  (let ((shown string))
    (when *org-link-descriptive*
      (setf shown (org-links-as-labels shown))
      (when (search "{{{" shown)
        (setf shown (cl-ppcre:regex-replace-all *org-macro-call-re* shown "\\2"))))
    (tuatara.display:string-columns shown)))

(defun org-table-number-p (field)
  "True when FIELD reads as a number."
  (cl-ppcre:scan "(?i)^(?:[<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*|[<>]?[-+]?0x[0-9a-f]+|[<>]?[-+]?[0-9]+#[0-9a-z]+|nan|[-+u]?inf)$"
                 field))

(defun org-table-cookie-p (field)
  "True when FIELD is an alignment or width cookie: <l>, <c>, <r>, <10>, <l10>."
  (cl-ppcre:scan "^<[lrc]?[0-9]*>$" field))

(defun org-column-alignment (fields)
  "How a table column holding FIELDS is aligned, :LEFT, :RIGHT or :CENTRE: as
its <l>, <r> or <c> says, else right when half or more of its fields that are
not empty are numbers."
  (let ((cookie (find-if (lambda (f) (and (org-table-cookie-p f) (> (length f) 2) (alpha-char-p (char f 1))))
                         fields)))
    (if cookie
        (ecase (char-downcase (char cookie 1)) (#\l :left) (#\r :right) (#\c :centre))
        (let* ((filled (remove-if (lambda (f) (or (string= f "") (org-table-cookie-p f))) fields))
               (numbers (count-if #'org-table-number-p filled)))
          (if (and filled (>= numbers (* 1/2 (length filled)))) :right :left)))))

(defun org-table-alignments (rows columns)
  "How each of COLUMNS columns of ROWS is aligned, :LEFT, :RIGHT or :CENTRE, as
`org-column-alignment' decides."
  (loop for column below columns
        collect (org-column-alignment (loop for row in rows
                                            unless (eq row :rule)
                                              collect (or (nth column row) "")))))

(defun org-table-render (model)
  "MODEL's table aligned, as text, and where point goes in it, as a character after its BEGIN."
  (let* ((rows (org-table-rows model))
         (columns (org-table-columns model))
         (widths (loop for column below columns
                       collect (max 1 (loop for row in rows
                                            unless (eq row :rule)
                                              maximize (org-string-columns (or (nth column row) ""))))))
         (alignments (org-table-alignments rows columns))
         (indent (org-table-indent model))
         (point 0))
    (values
     (with-output-to-string (out)
       (loop for row in rows
             for index from 0
             for row-start = (file-position out)
             do (when (plusp index) (write-char #\Newline out) (incf row-start))
                (write-string indent out)
                (write-char #\| out)
                (when (and (= index (org-table-row model)) (or (eq row :rule) (zerop columns)))
                  (setf point (file-position out)))
                (if (eq row :rule)
                    (progn (loop for width in widths
                                 for first = t then nil
                                 do (unless first (write-char #\+ out))
                                    (write-string (make-string (+ width 2) :initial-element #\-) out))
                           (write-char #\| out))
                    (progn
                      (loop for column below columns
                            for width in widths
                            for alignment in alignments
                            for field = (or (nth column row) "")
                            for room = (- width (org-string-columns field))
                            for left = (case alignment (:right room) (:centre (floor room 2)) (t 0))
                            do (write-char #\Space out)
                               (when (and (= index (org-table-row model))
                                          (= column (min (org-table-field model) (1- columns))))
                                 ;; gone to (:START), the field's start, before what aligns it, as Org goes there
                                 (setf point (if (eq (org-table-offset model) :start)
                                                 (file-position out)
                                                 (+ (file-position out) left (min (org-table-offset model) (length field))))))
                               (write-string (make-string left :initial-element #\Space) out)
                               (write-string field out)
                               (write-string (make-string (- room left) :initial-element #\Space) out)
                               (write-string " |" out))
                      (when (zerop columns) (write-char #\| out))
                      (when (and (= index (org-table-row model)) (>= (org-table-field model) columns))
                        (setf point (file-position out))))))
       (when (org-table-newline model) (write-char #\Newline out)))
     point)))

(defun org-table-write (buffer model)
  "Put MODEL's table, aligned, in place of its rows in BUFFER (its #+TBLFM: line
too when its formulas changed), one step to undo, point where MODEL has it."
  (multiple-value-bind (new point) (org-table-render model)
    (let ((begin (org-table-begin model)) (end (org-table-end model)))
      (when (org-table-formulas-changed model)
        (let ((formulas (org-table-formulas model)))
          (setf new (concatenate 'string new (if (org-table-newline model) "" (string #\Newline))
                                 (if formulas
                                     (format nil "~a#+TBLFM: ~{~a~^::~}~%" (org-table-indent model) formulas)
                                     ""))
                end (or (org-table-formulas-end model) end))
          (when (and (not (org-table-newline model)) (null formulas))
            (setf new (string-right-trim '(#\Newline) new)))))
      (if (string= new (subseq *org-current-text* begin end))
          (setf (tuatara.buffer:buffer-point buffer)
                (org-char->byte buffer (+ begin point)))
          (org-replace-chars buffer begin end new :point (+ begin point))))))

(defmacro with-org-table ((buffer model) &body body)
  "Run BODY with MODEL the Org table point is in as an ORG-TABLE-MODEL and BUFFER
the current buffer, then write the table back aligned; true when point was in a
table. BODY returning :NOT-DONE leaves the table as it was. A line that does
not begin with | is no table's, and the file is not parsed for it."
  (let ((tree (gensym)) (text (gensym)) (position (gensym)) (table (gensym)) (result (gensym)))
    `(let ((,buffer (current-buffer)))
       (multiple-value-bind (,tree ,text) (and (uiop:string-prefix-p "|" (string-left-trim " 	" (org-line-head ,buffer (point))))
                                               (org-tree-now ,buffer))
         (when ,tree
           (let* ((*org-current-text* ,text)
                  (,position (org-point-char ,buffer))
                  (,table (org-table-at ,tree ,position)))
             (when ,table
               (let* ((,model (org-table-read ,text ,table ,position))
                      (,result (progn ,@body)))
                 (unless (eq ,result :not-done) (org-table-write ,buffer ,model))
                 t))))))))

(defun org-table-data-row-p (model index)
  "True when MODEL's row INDEX is a row of fields, not a rule."
  (let ((row (nth index (org-table-rows model))))
    (and row (not (eq row :rule)))))

(defun org-table-insert-empty-row (model index)
  "Put an empty row in MODEL's table before its row INDEX."
  (let ((empty (make-list (max 1 (org-table-columns model)) :initial-element "")))
    (setf (org-table-rows model)
          (append (subseq (org-table-rows model) 0 index) (list empty) (nthcdr index (org-table-rows model))))))

(defun org-table-go (model row field)
  "Put MODEL's point at the start of field FIELD of row ROW, before the spaces that align its text."
  (setf (org-table-row model) row (org-table-field model) field (org-table-offset model) :start))

(defun org-table-pad-rows (model)
  "Give every row of MODEL's table as many fields as its longest has."
  (let ((columns (org-table-columns model)))
    (setf (org-table-rows model)
          (loop for row in (org-table-rows model)
                collect (if (eq row :rule)
                            row
                            (append row (make-list (- columns (length row)) :initial-element "")))))))

(defun org-table-field-text (text)
  "TEXT, Org text, as a field of an Org table holds it: on one line (an Org
line break and each line end a space), each | written \\vert{}, which shows
as |."
  (cl-ppcre:regex-replace-all "\\|" (substitute #\Space #\Newline (cl-ppcre:regex-replace-all "\\\\\\\\\\n" text " ")) "\\\\vert{}"))

(defun org-write-table-rows (rows out &key (indent "") (rule t))
  "Write ROWS to OUT as an Org table, each a list of its fields' Org text
\(ORG-TABLE-FIELD-TEXT), each line beginning with INDENT, with RULE a rule
under the first row when there are more, and an empty line after."
  (loop for row in rows
        for first = t then nil
        do (format out "~a|~{ ~a |~}~%" indent (mapcar #'org-table-field-text row))
           (when (and rule first (rest rows)) (format out "~a|-~%" indent)))
  (terpri out))

;;; Commands, and what TAB, S-TAB, RET and C-c C-c do in a table

(defun org-table-next-field ()
  "In an Org table: align it and go to the next field, a new row made after the last; true when in one."
  (with-org-table (buffer model)
    (org-table-maybe-eval-formula model)
    (let ((row (org-table-row model)) (columns (org-table-columns model)))
      (cond ((and (org-table-data-row-p model row) (< (1+ (org-table-field model)) columns))
             (org-table-go model row (1+ (org-table-field model))))
            (t (let ((next (loop for index from (1+ row) below (length (org-table-rows model))
                                 when (org-table-data-row-p model index) return index)))
                 (unless next
                   (setf next (1+ row))
                   (org-table-insert-empty-row model next))
                 (org-table-go model next 0)))))))

(defun org-table-previous-field ()
  "In an Org table: align it and go back to the previous field; true when in one."
  (with-org-table (buffer model)
    (let* ((row (org-table-row model))
           (field (min (org-table-field model) (org-table-columns model))))
      (if (and (org-table-data-row-p model row) (plusp field))
          (org-table-go model row (1- field))
          (let ((previous (loop for index from (1- row) downto 0
                                when (org-table-data-row-p model index) return index)))
            (if previous
                (org-table-go model previous (1- (max 1 (length (nth previous (org-table-rows model))))))
                (message "Cannot move to the previous table field")))))))

(defun org-table-next-row ()
  "In an Org table: align it and go to the same field of the row below, a new
row made at the end or before a rule; true when in one."
  (with-org-table (buffer model)
    (org-table-maybe-eval-formula model)
    (let ((row (org-table-row model))
          (field (min (org-table-field model) (max 0 (1- (org-table-columns model))))))
      (unless (org-table-data-row-p model (1+ row))
        (org-table-insert-empty-row model (1+ row)))
      (org-table-go model (1+ row) field))))

(define-command org-table-align ()
  "Align the Org table point is in: each column as
wide as its widest field, numbers to the right, rules as wide as the table.
A field holding =FORMULA or :=FORMULA makes it a formula of the table, worked out."
  (unless (with-org-table (buffer model) (org-table-maybe-eval-formula model) model)
    (message "Not in a table")))

(define-command org-table-insert-hline ()
  "Put a rule below the row point is in, in an Org table (\\`C-c -')."
  (unless (with-org-table (buffer model)
            (let ((row (org-table-row model)))
              (setf (org-table-rows model)
                    (append (subseq (org-table-rows model) 0 (1+ row)) (list :rule) (nthcdr (1+ row) (org-table-rows model))))))
    (message "Not in a table")))

(defun org-table-move-row (by)
  "Move the row point is in BY rows (-1 up, 1 down) in the Org table; true when in one."
  (with-org-table (buffer model)
    (let* ((rows (org-table-rows model)) (row (org-table-row model)) (other (+ row by)))
      (if (or (minusp other) (>= other (length rows)))
          (progn (message "Cannot move row further") :not-done)
          (progn (rotatef (nth row rows) (nth other rows))
                 (setf (org-table-row model) other))))))

(defun org-table-move-column (by)
  "Move the column point is in BY columns (-1 left, 1 right) in the Org table; true when in one."
  (with-org-table (buffer model)
    (org-table-pad-rows model)
    (let* ((field (org-table-field model)) (other (+ field by)) (columns (org-table-columns model)))
      (if (or (>= field columns) (minusp other) (>= other columns))
          (progn (message "Cannot move column further") :not-done)
          (progn (dolist (row (org-table-rows model))
                   (unless (eq row :rule) (rotatef (nth field row) (nth other row))))
                 (org-table-go model (org-table-row model) other))))))

(defun org-table-delete-column ()
  "Delete the column point is in, in the Org table; true when in one."
  (with-org-table (buffer model)
    (org-table-pad-rows model)
    (let ((field (min (org-table-field model) (1- (org-table-columns model)))))
      (when (>= field 0)
        (setf (org-table-rows model)
              (loop for row in (org-table-rows model)
                    collect (if (eq row :rule) row (append (subseq row 0 field) (nthcdr (1+ field) row)))))
        (org-table-go model (org-table-row model) (max 0 (min field (1- (org-table-columns model)))))))))

(defun org-table-insert-column ()
  "Put an empty column before the one point is in, in the Org table; true when in one."
  (with-org-table (buffer model)
    (org-table-pad-rows model)
    (let ((field (min (org-table-field model) (org-table-columns model))))
      (setf (org-table-rows model)
            (loop for row in (org-table-rows model)
                  collect (if (eq row :rule) row (append (subseq row 0 field) (list "") (nthcdr field row)))))
      (org-table-go model (org-table-row model) field))))

(defun org-table-kill-row ()
  "Delete the row point is in, in the Org table, point to the same field of the row after; true when in one."
  (with-org-table (buffer model)
    (let ((row (org-table-row model)) (rows (org-table-rows model)))
      (if (= 1 (length rows))
          (progn (message "Cannot delete the only row of a table: delete the line instead") :not-done)
          (progn (setf (org-table-rows model) (append (subseq rows 0 row) (nthcdr (1+ row) rows)))
                 (setf (org-table-row model) (min row (1- (length (org-table-rows model))))))))))

(defun org-table-insert-row ()
  "Put an empty row above the one point is in, in the Org table, point in it; true when in one."
  (with-org-table (buffer model)
    (org-table-insert-empty-row model (org-table-row model))
    (org-table-go model (org-table-row model) (min (org-table-field model) (max 0 (1- (org-table-columns model)))))))

(define-command org-metaup ()
  "In an Org table, move the row up; else move the subtree up (\\`M-k')."
  (unless (org-table-move-row -1) (org-move-subtree :up)))

(define-command org-metadown ()
  "In an Org table, move the row down; else move the subtree down (\\`M-j')."
  (unless (org-table-move-row 1) (org-move-subtree :down)))

(define-command org-metaleft ()
  "In an Org table, move the column left; else promote the headline (\\`M-h')."
  (unless (org-table-move-column -1) (org-promote nil -1)))

(define-command org-metaright ()
  "In an Org table, move the column right; else demote the headline (\\`M-l')."
  (unless (org-table-move-column 1) (org-promote nil 1)))

(define-command org-shiftmetaleft ()
  "In an Org table, delete the column; else promote the subtree (\\`M-H')."
  (unless (org-table-delete-column) (org-promote t -1)))

(define-command org-shiftmetaright ()
  "In an Org table, put an empty column before this one; else demote the
subtree (\\`M-L')."
  (unless (org-table-insert-column) (org-promote t 1)))

(define-command org-shiftmetaup ()
  "In an Org table, delete the row point is in (\\`M-S-<up>')."
  (unless (org-table-kill-row) (message "Not in a table")))

(define-command org-shiftmetadown ()
  "In an Org table, put an empty row above this one (\\`M-S-<down>')."
  (unless (org-table-insert-row) (message "Not in a table")))

(defun install-org-tables ()
  "Bind Org's table keys: \\`M-h' \\`M-j' \\`M-k' \\`M-l' (and the arrows) move a
table's columns and rows, else headlines; \\`M-H' and \\`M-L' delete and insert
columns, else promote and demote subtrees; \\`C-c -' puts a rule below."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-keys keymap '(("M-j" org-metadown) ("M-k" org-metaup) ("M-<down>" org-metadown) ("M-<up>" org-metaup)
                          ("M-h" org-metaleft) ("M-l" org-metaright) ("M-<left>" org-metaleft) ("M-<right>" org-metaright)
                          ("M-H" org-shiftmetaleft) ("M-L" org-shiftmetaright)
                          ("M-S-<left>" org-shiftmetaleft) ("M-S-<right>" org-shiftmetaright)
                          ("M-S-<up>" org-shiftmetaup) ("M-S-<down>" org-shiftmetadown)
                          ("C-c -" org-ctrl-c-minus)))))
