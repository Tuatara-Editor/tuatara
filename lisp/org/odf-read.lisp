;;;; lisp/org/odf-read.lisp --- OpenDocument text, spreadsheets and presentations read as Org text, by Tuatara itself, safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An OpenDocument file, LibreOffice's own (.odt, .ott; .ods, .ots; .odp,
;;;; .otp), is a zip archive of XML parts: content.xml its text, meta.xml
;;;; what it says of itself, styles.xml its styles and its pages' headers and
;;;; footers. It is read here by the reader of Word documents
;;;; (docx-read.lisp), whose limits hold: the archive's (sizes, growth,
;;;; checks), each part's and the XML's (no entity of a DTD, nothing fetched,
;;;; nesting and elements held). What it is comes from what content.xml
;;;; holds, never from the file's name. Nothing in it is run: no formula is
;;;; worked out, no macro run, no link followed, no linked file fetched. One
;;;; locked with a password is said to be so, and not read.
;;;;
;;;; Text is written as a Word document's is: what the file says of itself,
;;;; its headers and footers, then its text, its headings, lists, tables,
;;;; notes, comments and tracked changes (a deletion struck through where it
;;;; was). A spreadsheet as an Excel workbook's sheets are (office-read.lisp),
;;;; its values those last saved, as the file shows them, and its cells'
;;;; comments listed. A presentation as a PowerPoint presentation's slides.

(in-package #:tuatara.defaults)

(defvar *odf-styles* nil
  "The styles of the OpenDocument file being read, by name: each a plist of
:LOOK (what Org's markup can say of its text, as DOCX-READ-RUN-LOOK's),
:NAME (the name it is shown by), :PARENT, :HIDDEN (a sheet or slide not
shown), :LIST (for a list's style, the levels numbered).")

(defvar *odf-notes* nil
  "The notes and comments met in the OpenDocument text being read, each
\(LABEL . TEXT), latest first.")

(defvar *odf-changes* nil
  "The tracked changes of the OpenDocument text being read, by id: each a
plist of :KIND (:INSERTED, :DELETED or :FORMAT), :AUTHOR, :DATE and :TEXT.")

(defvar *odf-open-changes* '()
  "The ids of the tracked insertions the text being read is within.")

(defvar *odf-inserted* nil
  "While an OpenDocument text is read, the text written within its tracked
insertions, in order, an adjustable string: each insertion's text the
stretch of it from its change's :FROM to its :TO.")

(defvar *odf-note-numbers* nil
  "While an OpenDocument text is read, how many footnotes, endnotes and
comments it has had so far, a plist of :FOOTNOTE, :ENDNOTE and :COMMENT.")

;;; Styles

(defun odf-read-styles ()
  "The styles of the OpenDocument file being read (*ODF-STYLES*), from
content.xml and styles.xml: the look of their text, the name each is shown
by, whether a sheet's or slide's style hides it, and a list style's
numbered levels."
  (let ((styles (make-hash-table :test 'equal)))
    (dolist (part '("styles.xml" "content.xml"))
      (let ((tree (docx-reading-part part)))
        (dolist (style (and tree (append (xml-find-all tree "style:style") (xml-find-all tree "text:list-style"))))
          (let ((text (xml-child style "style:text-properties"))
                (look '()))
            (when text
              (flet ((on (name) (let ((v (xml-attribute text name))) (and v (not (member v '("none" "normal") :test #'string=)) v))))
                (let ((weight (on "fo:font-weight")))
                  (when (and weight (or (string= weight "bold") (>= (or (ignore-errors (parse-integer weight)) 0) 600))) (push :bold look)))
                (when (on "fo:font-style") (push :italic look))
                (when (on "style:text-underline-style") (push :underline look))
                (when (on "style:text-line-through-style") (push :strike look))
                (when (equal (xml-attribute text "text:display") "none") (push :hidden look))
                (let ((position (on "style:text-position")))
                  (cond ((null position))
                        ((or (uiop:string-prefix-p "super" position) (cl-ppcre:scan "^[1-9]" position)) (push :superscript look))
                        ((or (uiop:string-prefix-p "sub" position) (uiop:string-prefix-p "-" position)) (push :subscript look))))))
            (setf (gethash (xml-attribute style "style:name") styles)
                  (list :look look
                        :name (or (xml-attribute style "style:display-name") (xml-attribute style "style:name"))
                        :parent (xml-attribute style "style:parent-style-name")
                        :hidden (or (some (lambda (p) (equal (xml-attribute p "table:display") "false"))
                                          (xml-children style))
                                    (some (lambda (p) (equal (xml-attribute p "presentation:visibility") "hidden"))
                                          (xml-children style)))
                        :list (loop for level in (xml-children style)
                                    when (equal (xml-name level) "text:list-level-style-number")
                                      collect (ignore-errors (parse-integer (xml-attribute level "text:level"))))))))))
    styles))

(defun odf-style (name key &optional (depth 0))
  "What the style NAME of the file being read says of KEY (ODF-READ-STYLES),
from the style it is based on when it says nothing itself."
  (let ((style (and name (< depth 8) (gethash name *odf-styles*))))
    (and style (or (getf style key) (odf-style (getf style :parent) key (1+ depth))))))

(defun odf-style-named-p (name shown)
  "True when the style NAME, or one it is based on, is shown by the name SHOWN (Title)."
  (loop for style = name then (getf (gethash style *odf-styles*) :parent)
        repeat 8
        while style
        thereis (string-equal (or (getf (gethash style *odf-styles*) :name) style) shown)))

;;; Text within a paragraph

(defun odf-write-spaces (element out)
  "Write to OUT the spaces ELEMENT, a text:s of the file being read, stands
for: as many as it says, from 1 to 1000, taken from the room left for the
note (OFFICE-SPEND)."
  (let ((count (max 1 (min 1000 (or (docx-read-number element "text:c") 1)))))
    (office-spend count)
    (loop repeat count do (write-char #\Space out))))

(defun odf-plain-text (node)
  "The text within NODE, a paragraph or what holds paragraphs, on one line:
its spaces as the file means them; notes, comments, who made a change or
comment and when, and what is not text left out."
  (with-output-to-string (s)
    (labels ((walk (n)
               (cond ((stringp n) (write-string (cl-ppcre:regex-replace-all "\\s+" n " ") s))
                     (t (let ((name (xml-name n)))
                          (cond ((string= name "text:s")
                                 (odf-write-spaces n s))
                                ((member name '("text:tab" "text:line-break") :test #'string=) (write-char #\Space s))
                                ((member name '("text:note" "office:annotation" "text:tracked-changes" "office:change-info"
                                                "dc:creator" "dc:date" "svg:title" "svg:desc")
                                         :test #'string=))
                                ((string= name "text:p") (when (plusp (file-position s)) (write-char #\Space s)) (mapc #'walk (xml-children n)))
                                (t (mapc #'walk (xml-children n)))))))))
      (if (stringp node) (walk node) (mapc #'walk (xml-children node))))))

(defun odf-frame-words (frame)
  "What the frame FRAME (a picture, a text box, an embedded object) is, as words in brackets."
  (let ((image (first (xml-find-all frame "draw:image")))
        (box (first (xml-find-all frame "draw:text-box")))
        (alt (let ((words (find-if (lambda (w) (and w (plusp (length w))))
                                   (list (let ((t1 (first (xml-find-all frame "svg:desc")))) (and t1 (xml-all-text t1)))
                                         (let ((t2 (first (xml-find-all frame "svg:title")))) (and t2 (xml-all-text t2)))
                                         (xml-attribute frame "draw:name")))))
               (and words (org-safe-line words)))))
    (cond (box (format nil "[text box: ~a]" (org-safe-line (string-trim " " (odf-plain-text box)))))
          (image (format nil "[picture~@[ ~a~]~@[: ~a~]]" (let ((href (xml-attribute image "xlink:href"))) (and href (org-safe-line href))) alt))
          ((xml-find-all frame "draw:object") "[an embedded object]")
          (t (format nil "[a drawing~@[: ~a~]]" alt)))))

(defun odf-read-inline (node out)
  "Write the text, spans, links, notes and comments of NODE (a paragraph or
heading, or what it holds) to OUT as Org text."
  (dolist (child (xml-children node))
    (if (stringp child)
        (let ((text (docx-read-escaped (cl-ppcre:regex-replace-all "\\s+" child " "))))
          (write-string text out)
          (when *odf-open-changes*
            (loop for c across text do (vector-push-extend c *odf-inserted*))))
        (let ((name (xml-name child)))
          (cond
            ((string= name "text:s") (odf-write-spaces child out))
            ((string= name "text:tab") (write-char #\Tab out))
            ((string= name "text:line-break") (format out "\\\\~%"))
            ((string= name "text:span")
             (write-string (docx-read-marked (with-output-to-string (s) (odf-read-inline child s))
                                             (odf-style (xml-attribute child "text:style-name") :look))
                           out))
            ((string= name "text:a")
             (let ((words (with-output-to-string (s) (odf-read-inline child s)))
                   (target (xml-attribute child "xlink:href")))
               (if (and target (plusp (length words)) (not (uiop:string-prefix-p "#" target)))
                   (write-string (office-link-text target words) out)
                   (write-string words out))))
            ((string= name "text:note")
             ;; footnotes 1, 2...; endnotes e1, e2...; comments c1, c2...
             (let ((label (if (equal (xml-attribute child "text:note-class") "endnote")
                              (format nil "e~d" (incf (getf *odf-note-numbers* :endnote 0)))
                              (format nil "~d" (incf (getf *odf-note-numbers* :footnote 0))))))
               (push (cons label (org-safe-line (string-trim " " (odf-plain-text (or (xml-child child "text:note-body") child))))) *odf-notes*)
               (format out "[fn:~a]" label)))
            ((string= name "office:annotation")
             (let* ((label (format nil "c~d" (incf (getf *odf-note-numbers* :comment 0))))
                    (author (let ((a (xml-child child "dc:creator"))) (and a (xml-all-text a))))
                    (date (let ((d (xml-child child "dc:date"))) (and d (xml-all-text d))))
                    (words (format nil "~{~a~^ ~}" (mapcar (lambda (p) (string-trim " " (odf-plain-text p))) (xml-local-children child "p")))))
               (push (cons label (format nil "Comment by ~a~@[, ~a~]: ~a" (org-safe-line (or author "someone")) (and date (org-safe-line date)) (org-safe-line words)))
                     *odf-notes*)
               (format out "[fn:~a]" label)))
            ((string= name "text:change-start")
             (let* ((id (xml-attribute child "text:change-id")) (change (gethash id *odf-changes*)))
               (when change
                 (setf (getf change :from) (fill-pointer *odf-inserted*) (getf change :to) nil
                       (gethash id *odf-changes*) change)
                 (push id *odf-open-changes*))))
            ((string= name "text:change-end")
             (let* ((id (xml-attribute child "text:change-id")) (change (gethash id *odf-changes*)))
               (when (and change (member id *odf-open-changes* :test #'equal))
                 (setf (getf change :to) (fill-pointer *odf-inserted*) (gethash id *odf-changes*) change)
                 (setf *odf-open-changes* (remove id *odf-open-changes* :test #'equal)))))
            ((string= name "text:change")
             (let ((change (gethash (xml-attribute child "text:change-id") *odf-changes*)))
               (when (and change (eq (getf change :kind) :deleted) (plusp (length (getf change :text))))
                 (write-string (docx-read-marked (getf change :text) '(:strike)) out))))
            ((string= name "text:hidden-text")
             (format out "[hidden: ~a]" (org-safe-line (or (xml-attribute child "text:string-value") ""))))
            ((member name '("draw:frame" "draw:custom-shape" "draw:rect" "draw:ellipse" "draw:g") :test #'string=)
             (write-string (odf-frame-words child) out))
            ((member name '("text:bookmark" "text:bookmark-start" "text:bookmark-end" "text:soft-page-break" "text:reference-mark"
                            "text:reference-mark-start" "text:reference-mark-end" "office:annotation-end" "text:alphabetical-index-mark"
                            "text:toc-mark" "text:user-index-mark")
                     :test #'string=))
            (t (odf-read-inline child out)))))))

;;; Blocks

(defun odf-list-numbered-p (style level)
  "True when the list style STYLE numbers its LEVEL (1 the outermost)."
  (member level (odf-style style :list)))

(defun odf-read-list (list out level style numbers)
  "Write the list LIST to OUT as an Org list at LEVEL (0 the outermost), its
items numbered when its style (STYLE when it names none) numbers that level;
NUMBERS counts the items of each level."
  (let* ((style (or (xml-attribute list "text:style-name") style))
         (numbered (odf-list-numbered-p style (1+ level))))
    (setf (gethash level numbers) 0)
    (dolist (item (xml-children list))
      (when (member (xml-name item) '("text:list-item" "text:list-header") :test #'equal)
        (let ((first t))
          (dolist (part (xml-children item))
            (let ((name (xml-name part)))
              (cond ((member name '("text:p" "text:h") :test #'equal)
                     (let ((text (docx-read-guarded (string-trim '(#\Space #\Tab) (with-output-to-string (s) (odf-read-inline part s))))))
                       (office-spend (length text))
                       (format out "~a~a ~a~%" (make-string (* 2 (min 8 level)) :initial-element #\Space)
                               (cond ((not first) " ")
                                     (numbered (format nil "~d." (incf (gethash level numbers 0))))
                                     (t "-"))
                               (substitute #\Space #\Newline text))
                       (setf first nil)))
                    ((equal name "text:list") (odf-read-list part out (min 8 (1+ level)) style numbers))
                    ((equal name "table:table") (terpri out) (odf-read-table part out))))))))))

(defun odf-table-rows (table)
  "The rows of TABLE, a table of the file being read, each a list of its
cells' elements in order, a row or cell repeated given as often as it is,
within reason, each cell taken from the room left for the note
\(OFFICE-SPEND); header rows and groups of rows taken in."
  (let ((rows '()))
    (labels ((walk (node)
               (dolist (child (xml-children node))
                 (let ((name (xml-name child)))
                   (cond ((equal name "table:table-row")
                          (let ((cells (loop for cell in (xml-children child)
                                             when (member (xml-name cell) '("table:table-cell" "table:covered-table-cell") :test #'equal)
                                               append (make-list (max 1 (min 64 (or (docx-read-number cell "table:number-columns-repeated") 1)))
                                                                 :initial-element cell))))
                            (dotimes (i (max 1 (min 64 (or (docx-read-number child "table:number-rows-repeated") 1))))
                              ;; each cell given takes room in the note, an empty one too
                              (office-spend (length cells))
                              (push cells rows))))
                         ((member name '("table:table-header-rows" "table:table-rows" "table:table-row-group") :test #'equal)
                          (walk child)))))))
      (walk table))
    (nreverse rows)))

(defun odf-read-table (table out)
  "Write TABLE, a table of OpenDocument text or a slide, to OUT as an Org
table, a rule under its first row, each cell no longer than
*OFFICE-VALUE-LONGEST* characters; a cell joined over others written with
empty ones after it, so that the columns line up."
  (let ((rows (loop for row in (odf-table-rows table)
                    collect (loop for cell in row
                                  collect (if (equal (xml-name cell) "table:covered-table-cell")
                                              ""
                                              (org-safe-line (office-value (string-trim " " (odf-plain-text cell)))))))))
    ;; rows of empty cells at the end, as a table's last rows repeated often are, left out
    (loop while (and rows (every (lambda (c) (zerop (length c))) (car (last rows)))) do (setf rows (butlast rows)))
    (when rows (org-write-table-rows rows out))))

(defun odf-read-block (node out)
  "Write the headings, paragraphs, lists, tables and sections of NODE (the
text, a section, a cell, a header) to OUT as Org text."
  (let ((numbers (make-hash-table)))
    (dolist (child (xml-children node))
      (when (consp child)
        (let ((name (xml-name child)))
          (cond
            ((string= name "text:h")
             (let ((level (max 1 (min 8 (or (docx-read-number child "text:outline-level") 1))))
                   (text (string-trim '(#\Space #\Tab) (with-output-to-string (s) (odf-read-inline child s)))))
               (office-spend (length text))
               (format out "~a ~a~%~%" (make-string level :initial-element #\*) (substitute #\Space #\Newline text))))
            ((string= name "text:p")
             (let* ((style (xml-attribute child "text:style-name"))
                    (text (string-trim '(#\Space #\Tab) (with-output-to-string (s) (odf-read-inline child s))))
                    (text (docx-read-marked text (and (member :hidden (odf-style style :look)) '(:hidden)))))
               (office-spend (length text))
               (cond ((zerop (length text)))
                     ((odf-style-named-p style "Title") (format out "#+title: ~a~%~%" (substitute #\Space #\Newline text)))
                     (t (format out "~a~%~%" (docx-read-guarded text))))))
            ((string= name "text:list") (odf-read-list child out 0 nil numbers) (terpri out))
            ((string= name "table:table") (odf-read-table child out))
            ((member name '("draw:frame" "draw:custom-shape" "draw:g") :test #'string=) (format out "~a~%~%" (odf-frame-words child)))
            ((member name '("text:tracked-changes" "text:variable-decls" "text:sequence-decls" "text:user-field-decls" "office:forms"
                            "table:named-expressions" "text:table-of-content-source" "text:alphabetical-index-source"
                            "text:illustration-index-source" "text:table-index-source" "text:object-index-source"
                            "text:user-index-source" "text:bibliography-source" "text:index-title-template")
                     :test #'string=))
            (t (odf-read-block child out))))))))

(defun odf-read-changes (text)
  "The tracked changes of TEXT, an OpenDocument text's body (*ODF-CHANGES*):
by id, each a plist of :KIND, :AUTHOR, :DATE and, for a deletion, the :TEXT
deleted."
  (let ((changes (make-hash-table :test 'equal)))
    (dolist (region (xml-find-all text "text:changed-region"))
      (let ((change (find-if (lambda (c) (member (xml-name c) '("text:insertion" "text:deletion" "text:format-change") :test #'equal))
                             (xml-children region))))
        (when change
          (let ((info (xml-child change "office:change-info")))
            (setf (gethash (or (xml-attribute region "text:id") (xml-attribute region "xml:id")) changes)
                  (list :kind (cond ((equal (xml-name change) "text:insertion") :inserted)
                                    ((equal (xml-name change) "text:deletion") :deleted)
                                    (t :format))
                        :author (let ((a (and info (xml-child info "dc:creator")))) (and a (xml-all-text a)))
                        :date (let ((d (and info (xml-child info "dc:date")))) (and d (xml-all-text d)))
                        :text (if (equal (xml-name change) "text:deletion")
                                  (docx-read-escaped (string-trim " " (odf-plain-text change)))
                                  "")))))))
    changes))

(defun odf-read-headers ()
  "The section giving the text of the headers and footers of the file's page
styles, or an empty string."
  (let ((styles (docx-reading-part "styles.xml")) (lines '()))
    (dolist (page (and styles (xml-find-all styles "style:master-page")))
      (dolist (part (xml-children page))
        (let ((kind (cdr (assoc (xml-name part) '(("style:header" . "Header") ("style:footer" . "Footer")
                                                   ("style:header-left" . "Header, left pages") ("style:footer-left" . "Footer, left pages")
                                                   ("style:header-first" . "Header, first page") ("style:footer-first" . "Footer, first page"))
                                :test #'equal))))
          (when kind
            (let ((text (org-safe-line (string-trim " " (odf-plain-text part)))))
              (when (plusp (length text))
                (push (format nil "- ~a (page style ~a) :: ~a~%" kind
                              (org-safe-line (or (xml-attribute page "style:display-name") (xml-attribute page "style:name") "")) text)
                      lines)))))))
    (if lines (format nil "* Headers and footers~%~{~a~}~%" (nreverse lines)) "")))

;;; Spreadsheets

(defun odf-cell-value (cell)
  "The value of CELL, a cell of a spreadsheet, as text: as the file shows it,
last saved (a formula's last result, never worked out here); its value as
written when it shows none."
  (let ((shown (format nil "~{~a~^ ~}" (mapcar (lambda (p) (string-trim " " (odf-plain-text p))) (xml-local-children cell "p")))))
    (if (plusp (length shown))
        shown
        (or (find-if (lambda (v) (and v (plusp (length v))))
                     (mapcar (lambda (a) (xml-attribute cell a))
                             '("office:string-value" "office:value" "office:date-value" "office:time-value" "office:boolean-value")))
            ""))))

(defun odf-sheet-write (table out left)
  "Write to OUT the cells of TABLE, a spreadsheet's sheet, as SHEET-WRITE-ROWS
does, its formulas as written, its cells' comments after them, at most
LEFT cells, each no longer than *OFFICE-VALUE-LONGEST* characters. Returns
how many cells were written."
  (let ((hidden-columns (make-array 16385 :element-type 'bit :initial-element 0))
        (rows '()) (formulas '()) (comments '()) (count 0) (cut nil) (row-at 0) (column-at 0))
    (labels ((walk-columns (node)
               (dolist (child (xml-children node))
                 (let ((name (xml-name child)))
                   (cond ((equal name "table:table-column")
                          (let ((repeat (max 1 (min 16384 (or (docx-read-number child "table:number-columns-repeated") 1)))))
                            (when (member (xml-attribute child "table:visibility") '("collapse" "filter") :test #'equal)
                              (fill hidden-columns 1 :start (min 16384 (1+ column-at)) :end (min 16385 (+ 1 column-at repeat))))
                            (incf column-at repeat)))
                         ((member name '("table:table-columns" "table:table-header-columns" "table:table-column-group") :test #'equal)
                          (walk-columns child))))))
             (row-cells (row number)
               (let ((column 0) (cells '()))
                 (dolist (cell (xml-children row) (nreverse cells))
                   (when (member (xml-name cell) '("table:table-cell" "table:covered-table-cell") :test #'equal)
                     (let* ((repeat (max 1 (min 16384 (or (docx-read-number cell "table:number-columns-repeated") 1))))
                            (value (tuatara.display:shorten-string (odf-cell-value cell) *office-value-longest*))
                            (formula (let ((f (xml-attribute cell "table:formula")))
                                       (and f (org-safe-line (tuatara.display:shorten-string f *office-value-longest*)))))
                            (note (first (xml-local-children cell "annotation"))))
                       (if (or (plusp (length value)) formula note)
                           (dotimes (i repeat)
                             (incf column)
                             (when (> column 16384) (return))
                             (when (>= count left)
                               (setf cut (format nil "~a~d" (sheet-column-letters column) number))
                               (return-from row-cells (nreverse cells)))
                             (let ((place (format nil "~a~d" (sheet-column-letters column) number)))
                               (incf count)
                               (office-spend (+ (length value) (length formula)))
                               (push (cons column value) cells)
                               (when formula (push (cons place formula) formulas))
                               (when (and note (zerop i))
                                 (push (cons place (format nil "~@[by ~a: ~]~a"
                                                           (let ((a (xml-child note "dc:creator"))) (and a (org-safe-line (office-value (xml-all-text a)))))
                                                           (org-safe-line (office-value (string-trim " " (odf-plain-text note))))))
                                       comments))))
                           (incf column repeat)))))))
             (walk-rows (node)
               (dolist (child (xml-children node))
                 (when cut (return))
                 (let ((name (xml-name child)))
                   (cond ((equal name "table:table-row")
                          (let ((repeat (max 1 (min 1048576 (or (docx-read-number child "table:number-rows-repeated") 1))))
                                (hidden (member (xml-attribute child "table:visibility") '("collapse" "filter") :test #'equal)))
                            (if (notany (lambda (c) (and (consp c) (or (plusp (length (odf-cell-value c))) (xml-attribute c "table:formula")
                                                                       (xml-local-children c "annotation"))))
                                        (xml-children child))
                                (incf row-at repeat)
                                (dotimes (i repeat)
                                  (incf row-at)
                                  (when (or cut (> row-at 1048576)) (return))
                                  (let ((cells (row-cells child row-at)))
                                    (when cells (push (list row-at hidden cells) rows)))))))
                         ((member name '("table:table-header-rows" "table:table-rows" "table:table-row-group") :test #'equal)
                          (walk-rows child)))))))
      (walk-columns table)
      (walk-rows table))
    (sheet-write-rows (nreverse rows) (lambda (c) (= 1 (bit hidden-columns (min c 16384)))) (nreverse formulas) cut out)
    (when comments
      (format out "Comments on its cells:~%")
      (loop for (place . text) in (nreverse comments) do (format out "- ~a :: ~a~%" place text))
      (terpri out))
    count))

;;; Presentations

(defun odf-slide-write-shapes (node out &key notes)
  "Write to OUT the text of the shapes of NODE, a slide or its notes, in their
order, as Org text: each paragraph an item of a list, indented by its list's
level; a table an Org table; a picture by its description; the title left
out (it is the heading). With NOTES, the notes' text alone."
  (labels ((paragraphs (node level)
             ;; a list's items one level in from the list holding it
             (dolist (child (xml-children node))
               (let ((name (xml-name child)))
                 (cond ((member name '("text:p" "text:h") :test #'equal)
                        (let ((text (string-trim " " (odf-plain-text child))))
                          (office-spend (length text))
                          (when (plusp (length text))
                            (format out "~a- ~a~%" (make-string (* 2 (min 8 level)) :initial-element #\Space) (org-safe-line text)))))
                       ((equal name "text:list")
                        (dolist (item (xml-children child))
                          (when (member (xml-name item) '("text:list-item" "text:list-header") :test #'equal)
                            (paragraphs item (if (equal (xml-name node) "text:list-item") (min 8 (1+ level)) level)))))
                       ((consp child) (paragraphs child level))))))
           (walk (n)
             (let ((name (xml-name n)) (class (and (consp n) (xml-attribute n "presentation:class"))))
               (cond ((null name))
                     ((equal name "presentation:notes"))
                     ((member class '("title" "page" "header" "footer" "date-time" "page-number") :test #'equal))
                     ((equal name "table:table") (fresh-line out) (odf-read-table n out))
                     ;; a table beside the picture of it drawn for programs that cannot read tables: the table
                     ((and (equal name "draw:frame") (xml-child n "table:table")) (fresh-line out) (odf-read-table (xml-child n "table:table") out))
                     ((and (equal name "draw:frame") (xml-child n "draw:image") (not (xml-child n "draw:text-box")))
                      (format out "- ~a~%" (odf-frame-words n)))
                     ((member name '("draw:text-box" "draw:custom-shape" "draw:rect" "draw:ellipse" "draw:polygon" "draw:path")
                              :test #'equal)
                      (paragraphs n 0))
                     (t (mapc #'walk (xml-children n)))))))
    (if notes
        (dolist (frame (xml-find-all notes "draw:frame"))
          (when (equal (xml-attribute frame "presentation:class") "notes") (paragraphs frame 0)))
        (mapc #'walk (xml-children node)))))

(defun odf-slide-write (page number out)
  "Write to OUT the slide PAGE of the presentation being read, its NUMBER in
order: a heading with its title, marked (hidden) when its style hides it, its
text, its speaker notes under a heading of their own."
  (let ((title (format nil "~{~a~^ ~}"
                       (loop for frame in (xml-children page)
                             when (and (consp frame) (equal (xml-attribute frame "presentation:class") "title"))
                               collect (string-trim " " (odf-plain-text frame)))))
        (notes (xml-child page "presentation:notes")))
    (format out "* Slide ~d~:[~; (hidden)~]~@[: ~a~]~%" number (odf-style (xml-attribute page "draw:style-name") :hidden)
            (and (plusp (length title)) (org-safe-line title)))
    (odf-slide-write-shapes page out)
    (when notes
      (let ((text (with-output-to-string (s) (odf-slide-write-shapes page s :notes notes))))
        (when (plusp (length text))
          (format out "** Speaker notes~%~a" text))))
    (terpri out)))

;;; The note

(defun odf-outside-links ()
  "The addresses outside it the file being read links to (links, linked
pictures, sections, sheets and data), sorted, each once; none is followed."
  (let ((outside '()))
    (dolist (part '("content.xml" "styles.xml"))
      (let ((tree (docx-reading-part part)))
        (labels ((walk (n)
                   (when (consp n)
                     (dolist (attribute '("xlink:href" "table:href"))
                       (let ((href (xml-attribute n attribute)))
                         (when (and href (plusp (length href)) (not (uiop:string-prefix-p "#" href))
                                    (or (cl-ppcre:scan "^[A-Za-z][A-Za-z0-9+.-]*:" href) (uiop:string-prefix-p "/" href)
                                        (uiop:string-prefix-p "../" href)))
                           (pushnew href outside :test #'string=))))
                     (mapc #'walk (xml-children n)))))
          (when tree (walk tree)))))
    (sort outside #'string<)))

(defun odf-minutes (duration)
  "DURATION, as the file writes a time spent (PT1H30M), in minutes, or NIL."
  (multiple-value-bind (match parts) (cl-ppcre:scan-to-strings "^P(?:(\\d+)D)?(?:T(?:(\\d+)H)?(?:(\\d+)M)?(?:[\\d.]+S)?)?$" (or duration ""))
    (and match (+ (* 1440 (if (aref parts 0) (parse-integer (aref parts 0)) 0))
                  (* 60 (if (aref parts 1) (parse-integer (aref parts 1)) 0))
                  (if (aref parts 2) (parse-integer (aref parts 2)) 0)))))

(defun odf-read-about (path what &optional more)
  "The first section of the note (OFFICE-ABOUT-SECTION): what the file at
PATH is, WHAT in words \(a document, a spreadsheet, a presentation): what it
says of itself, then MORE (lines of the note, or NIL), then whether it holds
macros, embedded files or links outside it."
  (let* ((meta (docx-reading-part "meta.xml"))
         (names (mapcar (lambda (e) (getf e :name)) (docx-reading-entries *docx-reading*)))
         (statistic (and meta (first (xml-find-all meta "meta:document-statistic"))))
         (template (and meta (first (xml-find-all meta "meta:template")))))
    (flet ((meta (name) (xml-first-text meta name)))
      (office-about-section
       what path
       (append
        (list (cons "Title" (meta "dc:title")) (cons "Subject" (meta "dc:subject"))
              (cons "Made by" (meta "meta:initial-creator")) (cons "Last changed by" (meta "dc:creator"))
              (cons "Made" (meta "meta:creation-date")) (cons "Last changed" (meta "dc:date"))
              (cons "Printed" (let ((by (meta "meta:printed-by")) (date (meta "meta:print-date")))
                                (and (or by date) (format nil "~@[~a~]~:[~; by ~]~@[~a~]" date (and by date) by))))
              (cons "Revision" (meta "meta:editing-cycles"))
              (cons "Keywords" (and meta (format nil "~{~a~^, ~}" (mapcar (lambda (k) (string-trim " " (xml-all-text k)))
                                                                          (xml-find-all meta "meta:keyword")))))
              (cons "Description" (meta "dc:description")) (cons "Program" (meta "meta:generator"))
              (cons "Template" (and template (format nil "~a (not fetched)" (or (xml-attribute template "xlink:title")
                                                                                (xml-attribute template "xlink:href") ""))))
              (cons "Editing time" (let ((m (odf-minutes (meta "meta:editing-duration")))) (and m (format nil "~d minutes" m))))
              (cons "Pages, words" (and statistic
                                        (let ((p (xml-attribute statistic "meta:page-count")) (w (xml-attribute statistic "meta:word-count")))
                                          (and (or p w) (format nil "~@[~a pages~]~:[~;, ~]~@[~a words~]" p (and p w) w))))))
        (loop for field in (and meta (xml-find-all meta "meta:user-defined"))
              collect (cons (format nil "Field ~a" (or (xml-attribute field "meta:name") ""))
                            (string-trim " " (xml-all-text field)))))
       :more more
       :macros (remove-if-not (lambda (n) (and (or (uiop:string-prefix-p "Basic/" n) (uiop:string-prefix-p "Scripts/" n))
                                               (not (uiop:string-suffix-p n "/"))))
                              names)
       :events (remove-duplicates (loop for part in '("content.xml" "styles.xml")
                                        for tree = (docx-reading-part part)
                                        append (and tree (mapcar (lambda (e) (or (xml-attribute e "xlink:href") (xml-attribute e "script:macro-name") ""))
                                                                 (xml-find-all tree "script:event-listener"))))
                                  :test #'string=)
       :embedded (sort (remove-duplicates (loop for n in names
                                                when (and (uiop:string-prefix-p "Object " n) (find #\/ n))
                                                  collect (subseq n 0 (position #\/ n)))
                                          :test #'string=)
                       #'string<)
       :outside (odf-outside-links)
       :changes (if *odf-changes* (hash-table-count *odf-changes*) 0)))))

(defun odf-locked-p ()
  "True when the file being read is locked with a password: its manifest says its text is encrypted."
  (let ((manifest (docx-reading-part "META-INF/manifest.xml")))
    (and manifest
         (some (lambda (entry) (and (equal (xml-attribute entry "manifest:full-path") "content.xml")
                                    (xml-child entry "manifest:encryption-data")))
               (xml-find-all manifest "manifest:file-entry")))))

(defun odf-read-text-note (path body)
  "The note of the OpenDocument text whose BODY (office:text) is being read from PATH."
  (let* ((*odf-changes* (odf-read-changes body))
         (*odf-open-changes* '())
         (*odf-inserted* (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (*odf-notes* '())
         (*odf-note-numbers* '())
         (text (office-written-text (lambda (s) (odf-read-block body s))))
         (notes (reverse *odf-notes*)))
    ;; each insertion's text, as much of it as a value shows
    (maphash (lambda (id change)
               (let ((from (getf change :from)))
                 (when from
                   (let ((to (min (or (getf change :to) (fill-pointer *odf-inserted*)) (+ from *office-value-longest*))))
                     (setf (getf change :text) (concatenate 'string (getf change :text) (subseq *odf-inserted* from to))
                           (gethash id *odf-changes*) change)))))
             *odf-changes*)
    (concatenate 'string
                 (odf-read-about path "document")
                 (odf-read-headers)
                 (office-text-section text)
                 (let ((changes '()))
                   (maphash (lambda (id change) (push (cons id change) changes)) *odf-changes*)
                   (office-changes-section (loop for (nil . change) in (sort changes #'string< :key #'car)
                                                 collect (destructuring-bind (&key kind author date text &allow-other-keys) change
                                                           (list kind author date text)))))
                 (if notes
                     (format nil "* Footnotes~%~{[fn:~a] ~a~%~}" (loop for (label . words) in notes append (list label words)))
                     ""))))

(defun odf-read-spreadsheet-note (path body)
  "The note of the OpenDocument spreadsheet whose BODY (office:spreadsheet) is being read from PATH."
  (let* ((sheets (xml-local-children body "table"))
         (hidden (count-if (lambda (sheet) (odf-style (xml-attribute sheet "table:style-name") :hidden)) sheets))
         (left *sheet-most-cells*)
         (text (office-written-text
                (lambda (s)
                  (loop for sheet in sheets
                        for n from 1
                        do (format s "* Sheet ~d: ~a~:[~; (hidden)~]~%" n (org-safe-line (or (xml-attribute sheet "table:name") ""))
                                   (odf-style (xml-attribute sheet "table:style-name") :hidden))
                           (if (<= left 0)
                               (format s "Not read: the first ~:d cells were (*sheet-most-cells*).~%~%" *sheet-most-cells*)
                               (decf left (odf-sheet-write sheet s left))))))))
    (concatenate 'string
                 (odf-read-about path "spreadsheet"
                                 (with-output-to-string (s)
                                   (format s "- Sheets :: ~d~@[, ~d hidden~]~%" (length sheets) (and (plusp hidden) hidden))
                                   (let ((sources (append (xml-find-all body "table:database-source-sql") (xml-find-all body "table:database-source-table")
                                                          (xml-find-all body "table:database-source-query") (xml-find-all body "office:dde-source"))))
                                     (when sources
                                       (format s "- Data connections :: ~{~a~^, ~} (none made)~%"
                                               (mapcar (lambda (c) (org-safe-line (or (xml-attribute c "table:database-name") (xml-attribute c "office:dde-application")
                                                                                 (xml-attribute c "office:name") "unnamed")))
                                                       sources))))
                                   (format s "- Values :: as last saved with the file; no formula is worked out here~%")))
                 text)))

(defun odf-read-presentation-note (path body)
  "The note of the OpenDocument presentation (or drawing) whose BODY is being read from PATH."
  (let* ((pages (xml-local-children body "page"))
         (hidden (count-if (lambda (page) (odf-style (xml-attribute page "draw:style-name") :hidden)) pages)))
    (concatenate 'string
                 (odf-read-about path "presentation"
                                 (format nil "- Slides :: ~d~@[, ~d hidden~]~%" (length pages) (and (plusp hidden) hidden)))
                 (office-written-text
                  (lambda (s) (loop for page in pages for n from 1 do (odf-slide-write page n s)))))))

(defun odf-read-note (path &key octets cancel)
  "The OpenDocument file at PATH (text, a spreadsheet or a presentation, by
what it holds) as an Org note, from its OCTETS when they are given, as this
file's header says, no larger than *OFFICE-NOTE-LARGEST* characters; CANCEL,
a cons, stops the reading when its car turns true. Signals ZIP-ERROR or an
error when it cannot be read, or is locked with a password."
  (let ((*docx-reading* (docx-reading-of path octets cancel)))
    (when (odf-locked-p)
      (error "~a is locked with a password, and is not read" (tuatara.io:native-file-name path)))
    (let* ((content (docx-reading-part "content.xml"))
           (root (and content (xml-root content)))
           (body (and root (equal (xml-name root) "office:document-content") (xml-child root "office:body")))
           (kind (and body (find-if #'consp (xml-children body))))
           (*odf-styles* (odf-read-styles)))
      (office-note path
                   (cond ((null kind) (error "~a holds no OpenDocument text, spreadsheet or presentation" (tuatara.io:native-file-name path)))
                         ((equal (xml-name kind) "office:text") (odf-read-text-note path kind))
                         ((equal (xml-name kind) "office:spreadsheet") (odf-read-spreadsheet-note path kind))
                         ((member (xml-name kind) '("office:presentation" "office:drawing") :test #'equal) (odf-read-presentation-note path kind))
                         (t (error "~a holds ~a, which is not read as text" (tuatara.io:native-file-name path) (xml-local-name kind))))))))
