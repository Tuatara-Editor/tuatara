;;;; lisp/org/docx-import.lisp --- A Word document given made a template pair and an Org note to write in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; DOCX-IMPORT reads a Word document someone gave you to write a report in
;;;; and makes of it what M-x make-template-from-docx promises:
;;;;
;;;; - The Word half of a template, docx-templates/NAME/: the document as it
;;;;   is up to its first heading (its cover, contents, facts), its headers,
;;;;   footers, styles, theme and page; the text from the first heading on
;;;;   become {{{body}}}, the contents {{{contents}}}, and each field a place:
;;;;   a content control (its tag or title), a form field (its name), a merge
;;;;   field, placeholder text ([Enter name], <Version>, «Date»), a table row
;;;;   or a paragraph labelled "Version: 0.1", the title and subtitle. What
;;;;   the text's tables and lists look like goes into its tuatara.txt.
;;;;   Macros, comments, tracked changes, the link to an attached template and
;;;;   tracking turned on are left out.
;;;; - Its LaTeX half, latex-templates/NAME.tex: the same page, fonts, sizes,
;;;;   spacing, headers and footers as LaTeX can say them, near enough.
;;;; - Its note template, note-templates/NAME.org, that each new note to
;;;;   write in begins from: #+template: NAME, a keyword for each field with
;;;;   what the document had in it (a comment says what kind of value it
;;;;   takes: a date in what format, one of a list, text), and the document's
;;;;   text as Org (its changes accepted, its hidden text and comments left
;;;;   out, its pictures saved in note-templates/NAME-media/ and linked by
;;;;   their whole path, so that a note anywhere shows them).
;;;;
;;;; Nothing is written beside the document.
;;;;
;;;; The document is read as docx-read.lisp reads it: nothing in it run,
;;;; fetched or expanded.

(in-package #:tuatara.defaults)

(defun docx-import-place-run (key properties)
  "A run holding the place KEY in the look PROPERTIES (a w:rPr or NIL), a
placeholder's grey style left out."
  (format nil "<w:r>~a<w:t>{{{~a}}}</w:t></w:r>"
          (cl-ppcre:regex-replace-all "<w:rStyle w:val=\"PlaceholderText\"/>" (or properties "") "")
          key))

(defun docx-import-first-properties (xml)
  "The w:rPr of the first run in XML, or NIL."
  (let ((run (xml-element xml "w:r")))
    (and run (let ((rpr (xml-element xml "w:rPr" :start (car run) :end (cdr run))))
               (and rpr (subseq xml (car rpr) (cdr rpr)))))))

;;; Fields in the XML

(defun docx-import-controls (xml)
  "XML with each content control holding a field (one with a tag or a title,
not the contents) given a place in its content, the control kept."
  (xml-replace-elements
   xml "w:sdt"
   (lambda (sdt)
     (let* ((pr-span (xml-element sdt "w:sdtPr"))
            (pr (if pr-span (subseq sdt (car pr-span) (cdr pr-span)) ""))
            (content (xml-element sdt "w:sdtContent"))
            (tag (xml-value pr "w:tag"))
            (alias (xml-value pr "w:alias"))
            (name (or (and tag (plusp (length tag)) tag)
                      (and alias (plusp (length alias)) alias))))
       (if (or (null name) (null content) (search "docPartGallery" pr))
           ;; no field of its own; one within it still found
           (if (and content (search "<w:sdt" sdt :start2 (1+ (car content))))
               (concatenate 'string (subseq sdt 0 (car content)) (docx-import-controls (subseq sdt (car content))))
               sdt)
           (let* ((key (template-field-key name))
                  (inner (xml-element-inside sdt content))
                  (placeholder (search "<w:showingPlcHdr" pr))
                  (text (string-trim " " (docx-xml-text inner)))
                  (date (xml-value pr "w:dateFormat"))
                  (full (nth-value 1 (cl-ppcre:scan-to-strings "w:fullDate=\"(\\d{4}-\\d\\d-\\d\\d)" pr)))
                  (choices (mapcar (lambda (e) (xml-text-value (subseq e 0 (position #\" e))))
                                   (cl-ppcre:all-matches-as-strings "(?<=<w:listItem w:displayText=\")[^\"]*\"" pr)))
                  (kind (cond (date :date) (choices :list) ((search "w14:checkbox" pr) :checkbox) (t :text)))
                  (example (cond (placeholder nil)
                                 ((and date full) (aref full 0))
                                 ((plusp (length text)) text))))
             (template-field key kind :example example :label (or alias name)
                                         :format date :choices choices)
             (format nil "~a<w:sdtContent>~a</w:sdtContent></w:sdt>"
                     (cl-ppcre:regex-replace-all "<w:showingPlcHdr/>" (subseq sdt 0 (car content)) "")
                     (if (search "<w:p" inner)
                         ;; a control around paragraphs: one paragraph, the first's look, holding the place
                         (let* ((paragraph (xml-element inner "w:p"))
                                (pxml (subseq inner (car paragraph) (cdr paragraph)))
                                (ppr (xml-element pxml "w:pPr")))
                           (format nil "<w:p>~@[~a~]~a</w:p>" (and ppr (subseq pxml (car ppr) (cdr ppr)))
                                   (docx-import-place-run key (docx-import-first-properties pxml))))
                         (docx-import-place-run key (docx-import-first-properties inner))))))))))

(defun docx-import-form-fields (xml)
  "XML with each form field and merge field (FORMTEXT, FORMDROPDOWN,
MERGEFIELD, as a complex field or a simple one) made a place, plain text in
the look of its result, the field itself taken away."
  (let* ((simple (cl-ppcre:regex-replace-all
                  "<w:fldSimple w:instr=\"\\s*MERGEFIELD\\s+([^\"\\s\\\\]+)[^\"]*\"[^>]*>(.*?)</w:fldSimple>" xml
                  (lambda (match name inner)
                    (declare (ignore match))
                    (let ((key (template-field-key name)))
                      (template-field key :text :label name)
                      (docx-import-place-run key (docx-import-first-properties inner))))
                  :simple-calls t))
         (out (make-string-output-stream)) (p 0))
    ;; complex fields: begin, instruction, separate, result, end, each in runs of their own
    (loop
      (let ((begin (cl-ppcre:scan "<w:r(?:\\s[^>]*)?>(?:(?!</w:r>).)*<w:fldChar w:fldCharType=\"begin\"" simple :start p)))
        (unless begin (return))
        (let* ((end-char (search "w:fldCharType=\"end\"" simple :start2 begin))
               (end (and end-char (search "</w:r>" simple :start2 end-char)))
               (field (and end (subseq simple begin (+ end 6))))
               (instruction (and field (format nil "~{~a~}" (mapcar (lambda (e) (docx-xml-text (subseq field (car e) (cdr e))))
                                                                    (xml-elements field "w:instrText")))))
               (kind (and instruction (cl-ppcre:scan-to-strings "^\\s*(FORMTEXT|FORMDROPDOWN|MERGEFIELD)" instruction))))
          (if (or (null field) (null kind) (search "fldCharType=\"begin\"" field :start2 (+ 10 (search "fldCharType=\"begin\"" field))))
              (progn (write-string (subseq simple p (+ begin 4)) out) (setf p (+ begin 4)))
              (let* ((name (or (xml-value field "w:name")
                               (let ((m (nth-value 1 (cl-ppcre:scan-to-strings "MERGEFIELD\\s+\"?([^\"\\s\\\\]+)" instruction))))
                                 (and m (xml-text-value (aref m 0))))))
                     (label (or name "field"))
                     (key (template-field-key label))
                     (separate (search "fldCharType=\"separate\"" field))
                     (result (if separate (docx-xml-text (subseq field separate)) ""))
                     (choices (mapcar (lambda (e) (xml-text-value (subseq e 0 (position #\" e))))
                                      (cl-ppcre:all-matches-as-strings "(?<=<w:listEntry w:val=\")[^\"]*\"" field)))
                     (result-runs (if separate (subseq field (or (search "</w:r>" field :start2 separate) separate)) field)))
                (template-field key (if choices :list :text) :label label :choices choices
                                                                 :example (let ((r (string-trim " " (string-trim '(#\Space #\No-break_space #\U+2002) result))))
                                                                            (and (plusp (length r)) (not (cl-ppcre:scan "^[\\x{2002}\\s]*$" r)) r)))
                (write-string (subseq simple p begin) out)
                (write-string (docx-import-place-run key (docx-import-first-properties (subseq result-runs (min (length result-runs) 6)))) out)
                (setf p (+ end 6)))))))
    (write-string (subseq simple p) out)
    (get-output-stream-string out)))

(defun docx-import-paragraph (paragraph &key front)
  "PARAGRAPH (a w:p) with its placeholder text made places; with FRONT (a
paragraph of the document before its text) a paragraph styled Title or
Subtitle made {{{title}}} or {{{subtitle}}}, and one written Label: value
made Label: {{{label}}}."
  (if (or (uiop:string-suffix-p paragraph "/>") (null (position #\> paragraph)))
      paragraph
      (multiple-value-bind (items head tail) (docx-run-items paragraph)
        (let* ((text (format nil "~{~a~}" (mapcar #'third items)))
               (style (xml-value head "w:pStyle"))
               (first-props (fourth (find :text items :key #'first))))
          (flet ((whole (key label kind)
                   ;; the paragraph's words all the place, in its first run's look
                   (template-field key kind :example (string-trim " " text) :label label)
                   (with-output-to-string (out)
                     (write-string head out)
                     (let ((written nil))
                       (dolist (item items)
                         (cond ((eq (first item) :other) (write-string (second item) out))
                               ((not written) (write-string (docx-import-place-run key first-props) out) (setf written t)))))
                     (write-string tail out))))
            (cond
              ((zerop (length (string-trim " " text))) paragraph)
              ;; places made already, in its text or its content controls
              ((or (search "{{{" text) (some (lambda (item) (search "{{{" (second item))) items))
               (template-places-in text)
               paragraph)
              ((and front (member style '("Title") :test #'equal)) (whole "title" "Title" :text))
              ((and front (member style '("Subtitle") :test #'equal)) (whole "subtitle" "Subtitle" :text))
              ((cl-ppcre:scan *template-placeholder* text)
               (let ((replaced (cl-ppcre:regex-replace-all
                                *template-placeholder* text
                                (lambda (match a b c)
                                  (let ((key (template-field-key (or a b c))))
                                    (template-field key :text :label match)
                                    (format nil "{{{~a}}}" key)))
                                :simple-calls t)))
                 ;; the paragraph's text in its first run's look, the places in it
                 (with-output-to-string (out)
                   (write-string head out)
                   (let ((written nil))
                     (dolist (item items)
                       (cond ((eq (first item) :other) (write-string (second item) out))
                             ((not written)
                              (write-string (docx-with-properties (docx-run replaced) first-props) out)
                              (setf written t)))))
                   (write-string tail out))))
              ((and front (cl-ppcre:scan "^\\s*[A-Za-z][A-Za-z /'-]{0,30}:\\s+\\S.{0,80}$" text))
               (let* ((colon (position #\: text))
                      (label (string-trim " " (subseq text 0 colon)))
                      (key (template-field-key label)))
                 (template-field key :text :example (string-trim " " (subseq text (1+ colon))) :label label)
                 ;; the label's runs kept up to the colon, the value one place in the look of the run it began in
                 (with-output-to-string (out)
                   (write-string head out)
                   (let ((seen 0) (placed nil) (value-props nil))
                     (dolist (item items)
                       (if (eq (first item) :other)
                           (write-string (second item) out)
                           (let* ((item-text (third item)) (start seen) (end (+ seen (length item-text))))
                             (setf seen end)
                             (cond ((< end (1+ colon)) (write-string (second item) out))
                                   ((<= start colon)
                                    ;; the run with the colon: up to it and its spaces; the value's look its own when it holds some
                                    (let ((keep (subseq item-text 0 (1+ (- colon start)))))
                                      (write-string (docx-with-properties (docx-run (concatenate 'string keep " ")) (fourth item)) out)
                                      (when (plusp (length (string-trim " " (subseq item-text (1+ (- colon start))))))
                                        (setf value-props (fourth item)))))
                                   (t (unless value-props (setf value-props (fourth item))))))))
                     (unless placed (write-string (docx-import-place-run key value-props) out)))
                   (write-string tail out))))
              (t paragraph)))))))

(defun docx-import-rows (xml)
  "XML with each row of a table of labels and values (a first cell of a few
words, a second of a value) given a place in its second cell: Version | 0.1
becomes Version | {{{version}}}; a further label ending in : and the value
after it along the row the same (Subject: | ... | Date: | ...)."
  (xml-replace-elements
   xml "w:tbl"
   (lambda (tbl)
     (let* ((rows (xml-elements tbl "w:tr"))
            (shaped (loop for row in rows
                          for cells = (xml-elements tbl "w:tc" :start (car row) :end (cdr row))
                          for label = (and (>= (length cells) 2) (string-trim " :" (docx-xml-text (subseq tbl (car (first cells)) (cdr (first cells))))))
                          count (and label (<= 1 (length label) 40) (alpha-char-p (char label 0))))))
       (if (or (< (length rows) 2) (< (* 2 shaped) (length rows)) (search "<w:tblHeader" tbl))
           tbl
           (let ((tout (make-string-output-stream)) (q 0))
             (dolist (row rows)
               ;; the pairs of a label and a value along the row, a later one's label ending in :
               (loop for (label-cell value-cell) on (xml-elements tbl "w:tc" :start (car row) :end (cdr row)) by #'cddr
                     for first = t then nil
                     for written = (and label-cell (docx-xml-text (subseq tbl (car label-cell) (cdr label-cell))))
                     for label = (and written (string-trim " :" written))
                     for cell-xml = (and value-cell (subseq tbl (car value-cell) (cdr value-cell)))
                     while cell-xml
                     when (and (<= 1 (length label) 40) (alpha-char-p (char label 0)) (not (search "{{{" cell-xml))
                               (or first (uiop:string-suffix-p (string-trim " " written) ":")))
                       do (let* ((key (template-field-key label))
                                 (value (string-trim " " (docx-xml-text cell-xml)))
                                 (paragraph (xml-element cell-xml "w:p"))
                                 (pxml (and paragraph (subseq cell-xml (car paragraph) (cdr paragraph))))
                                 (ppr (and pxml (xml-element pxml "w:pPr")))
                                 (tcpr (xml-element cell-xml "w:tcPr")))
                            (template-field key (if (template-date-label-p label) :date :text)
                                            :example (and (plusp (length value)) (not (cl-ppcre:scan (format nil "^(?:~a)$" *template-placeholder*) value)) value)
                                            :label label)
                            (write-string (subseq tbl q (car value-cell)) tout)
                            (format tout "<w:tc>~@[~a~]<w:p>~@[~a~]~a</w:p></w:tc>"
                                    (and tcpr (subseq cell-xml (car tcpr) (cdr tcpr)))
                                    (and ppr (subseq pxml (car ppr) (cdr ppr)))
                                    (docx-import-place-run key (and pxml (docx-import-first-properties pxml))))
                            (setf q (cdr value-cell)))))
             (write-string (subseq tbl q) tout)
             (get-output-stream-string tout)))))))

(defun docx-import-fields-in (xml &key front)
  "XML, a part or the document's front, with its fields made places
(controls, form fields, rows of labels, then each paragraph); FRONT as
DOCX-IMPORT-PARAGRAPH takes it."
  (let* ((xml (docx-import-form-fields (docx-import-controls xml)))
         (xml (if front (docx-import-rows xml) xml)))
    (xml-replace-elements xml "w:p" (lambda (paragraph) (docx-import-paragraph paragraph :front front)))))

;;; The document split: its front, its text, its page

(defun docx-import-heading-level (paragraph-xml)
  "The heading level of the paragraph PARAGRAPH-XML, 1 for a first-level
heading, by its style's outline level or name; NIL for a paragraph that is
no heading (the contents' heading neither)."
  (let ((style (xml-value paragraph-xml "w:pStyle"))
        (own (xml-value paragraph-xml "w:outlineLvl")))
    (docx-read-heading-level style (and own (ignore-errors (parse-integer own))))))

(defun docx-import-split (document)
  "The document's XML DOCUMENT split: as values its front (the XML before
its first heading), its text's top elements' XML, the w:sectPr of the
section its text begins in, the XML before the body's content and after
it, and the w:sectPr of the section each of the text's top elements is in, in order."
  (let* ((body (or (xml-element document "w:body") (error "The document has no body: nothing made")))
         (inner-start (nth-value 0 (xml-element-inner document body)))
         (inner-end (nth-value 1 (xml-element-inner document body)))
         (tops (xml-top-elements document inner-start inner-end))
         (final (find "w:sectPr" tops :key #'first :test #'string= :from-end t))
         (content (remove final tops))
         (start (or (position-if (lambda (top) (and (string= (first top) "w:p")
                                                    (docx-import-heading-level (subseq document (cadr top) (cddr top)))))
                                 content)
                    (length content)))
         (front (subseq content 0 start))
         (text (subseq content start))
         (breaks (remove-if-not (lambda (top) (and (string= (first top) "w:p") (search "<w:sectPr" document :start2 (cadr top) :end2 (cddr top))))
                                text))
         (section (if breaks
                      (let ((span (xml-element document "w:sectPr" :start (cadr (first breaks)) :end (cddr (first breaks)))))
                        (subseq document (car span) (cdr span)))
                      (and final (subseq document (cadr final) (cddr final))))))
    (values (if front (subseq document (cadr (first front)) (cddr (car (last front)))) "")
            (mapcar (lambda (top) (subseq document (cadr top) (cddr top))) text)
            section
            (subseq document 0 inner-start)
            (subseq document inner-end)
            ;; each top of the text's section: the w:sectPr of the paragraph ending it, or the document's last
            (let ((governing '()) (count 0))
              (dolist (top text)
                (incf count)
                (when (member top breaks)
                  (let* ((span (xml-element document "w:sectPr" :start (cadr top) :end (cddr top)))
                         (xml (subseq document (car span) (cdr span))))
                    (setf governing (append governing (make-list count :initial-element xml)) count 0))))
              (append governing (make-list count :initial-element (and final (subseq document (cadr final) (cddr final)))))))))

(defun docx-import-contents (front)
  "FRONT with its table of contents, a content control of contents or a TOC
field's paragraphs, made a paragraph holding {{{contents}}}; as a second
value true when it had one."
  (let ((control (find-if (lambda (span) (search "Table of Contents" front :start2 (car span) :end2 (cdr span)))
                          (xml-elements front "w:sdt"))))
    (if control
        (values (concatenate 'string (subseq front 0 (car control)) "<w:p><w:r><w:t>{{{contents}}}</w:t></w:r></w:p>" (subseq front (cdr control))) t)
        (let ((paragraphs (remove-if-not (lambda (span) (cl-ppcre:scan "<w:instrText[^>]*>\\s*TOC\\s|<w:pStyle w:val=\"TOC" (subseq front (car span) (cdr span))))
                                         (xml-elements front "w:p"))))
          (if paragraphs
              (values (concatenate 'string (subseq front 0 (car (first paragraphs))) "<w:p><w:r><w:t>{{{contents}}}</w:t></w:r></w:p>"
                                   (subseq front (cdr (car (last paragraphs)))))
                      t)
              (values front nil))))))

;;; The design of the text, for tuatara.txt

(defun docx-import-design (text-xml)
  "What the document's text TEXT-XML (its top elements) says of its design:
the lines of tuatara.txt, its tables' style and its lists' numberings."
  (let* ((all (format nil "~{~a~}" text-xml))
         (table-style (xml-value all "w:tblStyle"))
         ;; Word's tables stand at the left margin unless they say otherwise
         (table-position (let ((table (search "<w:tblPr>" all)))
                           (and table (let ((m (nth-value 1 (cl-ppcre:scan-to-strings "^<w:tblPr>(?:(?!</w:tblPr>).)*<w:jc w:val=\"(center|right|end)\"" (subseq all table)))))
                                         (cond ((null m) "left") ((string= (aref m 0) "center") "centre") (t "right"))))))
         (numbering (docx-reading-part "word/numbering.xml"))
         (bullets nil) (numbers nil))
    (cl-ppcre:do-register-groups (id) ("<w:numId w:val=\"(\\d+)\"" all)
      (let* ((num (and numbering (find id (xml-find-all numbering "w:num") :key (lambda (n) (xml-attribute n "w:numId")) :test #'equal)))
             (abstract (and num (xml-attribute (xml-child num "w:abstractNumId") "w:val"))))
        (let ((kind (cdr (assoc 0 (gethash id (docx-reading-numbering *docx-reading*))))))
          (when abstract
            (cond ((and (equal kind "bullet") (null bullets)) (setf bullets abstract))
                  ((and kind (not (member kind '("bullet" "none") :test #'string=)) (null numbers)) (setf numbers abstract)))))))
    (with-output-to-string (s)
      (format s "# How the note's text follows this template's design: made by make-template-from-docx.~%")
      (format s "table style: ~@[~a~]~%table position: ~@[~a~]~%bullets: ~@[~a~]~%numbers: ~@[~a~]~%" table-style table-position bullets numbers))))

;;; The LaTeX half

(defun docx-import-latex-place (place)
  "PLACE, {{{KEY}}}, as the LaTeX half writes it: with the date pattern of
the field KEY when it is a date's, {{{KEY:date=d MMMM yyyy}}}."
  (let* ((key (string-trim "{}" place))
         (field (and *template-import* (find key (template-import-fields *template-import*) :key #'first :test #'string=))))
    (if (and field (eq (second field) :date) (fifth field) (not (find #\} (fifth field))))
        (format nil "{{{~a:date=~a}}}" key (fifth field))
        place)))

(defun docx-import-latex-text (xml)
  "The text of XML, a header's or a paragraph's, for LaTeX: escaped, its
places kept (a date field's with its pattern, {{{date:date=d MMMM yyyy}}},
so that the PDF writes the date as the document does), a page number field
\\thepage, the number of pages \\pageref{LastPage}. What looks like a place
but is none (TEMPLATE-PLACE) is escaped as the rest is: a document's words
never become LaTeX."
  (let* ((xml (cl-ppcre:regex-replace-all "<w:fldSimple w:instr=\"\\s*PAGE\\b[^\"]*\"[^>]*>.*?</w:fldSimple>" xml "<w:t>@@PAGE@@</w:t>"))
         (xml (cl-ppcre:regex-replace-all "<w:fldSimple w:instr=\"\\s*NUMPAGES\\b[^\"]*\"[^>]*>.*?</w:fldSimple>" xml "<w:t>@@PAGES@@</w:t>"))
         (text (string-trim " " (docx-xml-text xml)))
         (pieces (cl-ppcre:split "(\\{\\{\\{[^}]*\\}\\}\\}|@@PAGES?@@)" text :with-registers-p t)))
    (format nil "~{~a~}" (mapcar (lambda (piece)
                                   (cond ((string= piece "@@PAGE@@") "\\thepage{}")
                                         ((string= piece "@@PAGES@@") "\\pageref{LastPage}")
                                         ((template-place piece :whole t) (docx-import-latex-place piece))
                                         (t (latex-escape piece))))
                                 pieces))))

(defun docx-import-latex (name front-xml headers section fields contents)
  "The LaTeX half of the template NAME: the page SECTION (a w:sectPr) gives,
the fonts, sizes and spacing of the document's styles, HEADERS (each
(KIND . XML) of its header and footer parts, KIND :HEADER, :FIRST-HEADER,
:FOOTER or :FIRST-FOOTER), its front's paragraphs FRONT-XML as plain lines,
FIELDS for its places and CONTENTS true for a table of contents."
  (multiple-value-bind (major minor) (docx-read-theme-fonts)
    (let* ((defaults (docx-read-defaults))
           (normal-id (docx-read-default-style))
           (normal (docx-read-merge defaults (if normal-id (docx-read-style-look normal-id) '())))
           (font-name (lambda (look) (let ((font (getf look :font)))
                                       (cond ((stringp font) font)
                                             ((equal font '(:theme :major)) major)
                                             ((equal font '(:theme :minor)) minor)))))
           (body-font (funcall font-name normal))
           (size (/ (or (getf normal :size) 22) 2))
           (families (installed-font-families))
           (installed (lambda (font) (and font (gethash font families))))
           (width (docx-page-number (or section "") "w:pgSz" "w:w"))
           (height (docx-page-number (or section "") "w:pgSz" "w:h"))
           (columns (or (docx-page-number (or section "") "w:cols" "w:num") 1)))
      (flet ((number (attribute element-name) (docx-page-number (or section "") element-name attribute))
             (cm (twips) (format nil "~acm" (docx-twips-cm (or twips 1440))))
             (heading (level command)
               (let* ((id (format nil "Heading~d" level))
                      (look (docx-read-merge normal (docx-read-style-look id)))
                      (font (funcall font-name look))
                      (points (/ (or (getf look :size) 22) 2)))
                 (format nil "\\titleformat{\\~a}{~:[~*~;\\fontspec{~a}~]\\fontsize{~a}{~a}\\selectfont~:[~;\\bfseries~]~@[\\color[HTML]{~a}~]}{\\the~a}{1em}{}~%\\titlespacing*{\\~a}{0pt}{~apt}{~apt}~%"
                         command (and font (not (equal font body-font)) (funcall installed font)) font
                         (points-text points) (points-text (* 6/5 points)) (getf look :bold) (getf look :colour) command
                         command (docx-twips-pt (or (getf look :before) 0)) (docx-twips-pt (or (getf look :after) 0))))))
        (with-output-to-string (s)
          (format s "% Created {{{created}}} with Tuatara, template ~a: made by make-template-from-docx from a Word~%" name)
          (format s "% document, its design as near as LaTeX says it; change it as you like.~%")
          (format s "% Intended LaTeX compiler: {{{engine}}}~%")
          (format s "\\documentclass[~dpt]{article}~%" (cond ((<= size 10) 10) ((<= size 11) 11) (t 12)))
          (format s "\\usepackage[paperwidth=~a, paperheight=~a, top=~a, bottom=~a, left=~a, right=~a]{geometry}~%"
                  (cm width) (cm height) (cm (number "w:top" "w:pgMar")) (cm (number "w:bottom" "w:pgMar"))
                  (cm (number "w:left" "w:pgMar")) (cm (number "w:right" "w:pgMar")))
          (format s "{{{packages}}}~:[{{{fonts}}}% ~a is not installed here: Tuatara's own fonts stand in for it~%~;\\setmainfont{~a}~%~]{{{font_fallbacks}}}"
                  (funcall installed body-font)
                  (if (funcall installed body-font) body-font (latex-escape (substitute-if #\Space (lambda (c) (member c '(#\Newline #\Return))) (or body-font "")))))
          (format s "\\usepackage{xcolor}~%\\usepackage{titlesec}~%\\usepackage{fancyhdr}~%\\usepackage{lastpage}~%~:[~;\\usepackage{multicol}~%~]" (> columns 1))
          (format s "\\setlength{\\parindent}{0pt}~%\\setlength{\\parskip}{~apt}~%\\linespread{~,2f}~%"
                  (docx-twips-pt (or (getf normal :after) 0))
                  (let ((line (getf normal :line))) (if (and line (equal (or (getf normal :line-rule) "auto") "auto")) (/ line 240.0) 1.0)))
          (format s "\\renewcommand{\\normalsize}{\\fontsize{~a}{~a}\\selectfont}\\normalsize~%" (points-text size) (points-text (* 6/5 size)))
          (write-string (heading 1 "section") s)
          (write-string (heading 2 "subsection") s)
          (write-string (heading 3 "subsubsection") s)
          (format s "{{{latex_header}}}\\hypersetup{colorlinks=true, linkcolor=black, urlcolor=black, citecolor=black, pdftitle={{{{pdftitle}}}}, pdfauthor={{{{pdfauthor}}}}}~%")
          (format s "\\pagestyle{fancy}~%\\fancyhf{}~%\\renewcommand{\\headrulewidth}{0pt}~%")
          (loop for (kind . xml) in headers
                for text = (docx-import-latex-text xml)
                for align = (cond ((search "w:val=\"right\"" xml) "R") ((search "w:val=\"center\"" xml) "C") (t "L"))
                when (member kind '(:header :footer))
                  do (format s "\\fancy~:[foot~;head~][~a]{~a}~%" (eq kind :header) align text))
          (let ((firsts (remove-if-not (lambda (h) (member (car h) '(:first-header :first-footer))) headers)))
            (when firsts
              (format s "\\fancypagestyle{firstpage}{\\fancyhf{}\\renewcommand{\\headrulewidth}{0pt}~%")
              (loop for (kind . xml) in firsts
                    for align = (cond ((search "w:val=\"right\"" xml) "R") ((search "w:val=\"center\"" xml) "C") (t "L"))
                    do (format s "  \\fancy~:[foot~;head~][~a]{~a}~%" (eq kind :first-header) align (docx-import-latex-text xml)))
              (format s "}~%")))
          (format s "\\begin{document}~%~:[~;\\thispagestyle{firstpage}~%~]" (find :first-header headers :key #'car))
          ;; the front: a paragraph a line, a table a tabular, its places, its words as they are
          (labels ((style-look (pxml)
                     (let ((id (xml-value pxml "w:pStyle")))
                       (and id (docx-read-merge normal (docx-read-style-look id)))))
                   (styled (pxml text)
                     ;; a paragraph of a style of its own (the title's) in its size and colour
                     (let ((look (style-look pxml)))
                       (if (and look (or (getf look :colour) (/= (or (getf look :size) 22) (or (getf normal :size) 22))))
                           (let ((points (/ (or (getf look :size) 22) 2)))
                             (format nil "{\\fontsize{~a}{~a}\\selectfont~:[~;\\bfseries~]~@[\\color[HTML]{~a}~] ~a\\par}~%"
                                     (points-text points) (points-text (* 6/5 points)) (getf look :bold) (getf look :colour) text))
                           (format nil "~a\\par~%" text))))
                   (front (xml)
                     (dolist (top (xml-top-elements xml 0 (length xml)))
                       (let ((element (subseq xml (cadr top) (cddr top))))
                         (cond
                           ((string= (first top) "w:p")
                            (let ((text (docx-import-latex-text element)))
                              (cond ((search "{{{contents}}}" text) (format s "\\tableofcontents~%\\clearpage~%"))
                                    ((search "{{{subtitle}}}" text) (format s "{{{#subtitle}}}~a{{{/subtitle}}}" (styled element text)))
                                    ((plusp (length text)) (write-string (styled element text) s)))))
                           ((string= (first top) "w:tbl")
                            (let ((rows (mapcar (lambda (row)
                                                  (mapcar (lambda (cell) (docx-import-latex-text (subseq element (car cell) (cdr cell))))
                                                          (xml-elements element "w:tc" :start (car row) :end (cdr row))))
                                                (xml-elements element "w:tr"))))
                              (format s "\\begin{tabular}{@{}~{~a~}@{}}~%~{~{~a~^ & ~}\\\\~%~}\\end{tabular}\\par~%"
                                      (make-list (reduce #'max rows :key #'length :initial-value 1) :initial-element "l") rows)))
                           ((member (first top) '("w:sdt" "w:sdtContent") :test #'string=)
                            (let ((content (xml-element element "w:sdtContent")))
                              (when content
                                (front (xml-element-inside element content))))))))))
            (front front-xml))
          (unless (search "{{{contents}}}" front-xml)
            (when contents (format s "\\tableofcontents~%")))
          ;; fields the front has no place for (and the headers neither), each a line
          (dolist (field fields)
            (unless (or (search (format nil "{{{~a}}}" (first field)) front-xml)
                        (some (lambda (header) (search (format nil "{{{~a}}}" (first field)) (cdr header))) headers)
                        (member (first field) '("title" "subtitle") :test #'string=))
              (format s "{{{#~a}}}~a: {{{~a}}}\\par~%{{{/~a}}}" (first field) (latex-escape (or (fourth field) (first field))) (first field) (first field))))
          (format s "~:[~*~;\\begin{multicols}{~d}~%~]{{{body}}}~%~:[~;\\end{multicols}~%~]\\end{document}~%"
                  (> columns 1) columns (> columns 1)))))))

;;; The note

;;; Later sections: pages turned sideways and columns, kept as the note's page settings

(defun docx-import-page (section)
  "What the w:sectPr SECTION sets of its pages, a list: :LANDSCAPE true for
pages turned sideways, :COLUMNS their number, :SIZE (SHORT LONG) the paper's
sides in twentieths of a point, :MARGINS (TOP RIGHT BOTTOM LEFT)."
  (let* ((section (or section ""))
         (width (docx-page-number section "w:pgSz" "w:w"))
         (height (docx-page-number section "w:pgSz" "w:h"))
         (columns (or (docx-page-number section "w:cols" "w:num") 1)))
    (list :landscape (or (and (search "w:orient=\"landscape\"" section) t) (and width height (> width height)))
          :columns (max 1 columns)
          :size (and width height (list (min width height) (max width height)))
          :margins (mapcar (lambda (side) (docx-page-number section "w:pgMar" side)) '("w:top" "w:right" "w:bottom" "w:left")))))

(defun docx-import-marker (landscape columns scope)
  "A paragraph standing for a page setting in the text read: LANDSCAPE, the
number of COLUMNS, SCOPE :heading or :element; DOCX-IMPORT-PAGE-DRAWERS
makes it an :EXPORT: drawer."
  (format nil "<w:p><w:r><w:t>@@TUATARA-PAGE ~:[upright~;landscape~] ~d ~(~a~)@@</w:t></w:r></w:p>" landscape columns scope))

(defun docx-import-section-marks (text-xml governing main)
  "TEXT-XML, the text's top elements, with a marker (DOCX-IMPORT-MARKER)
before those of a section (GOVERNING, each one's w:sectPr) whose pages are
turned sideways or in other columns than MAIN's, the section the template's
page is: on a heading when the section is all of what is under it, else on
each element (a list once, at its first item). As a second value what was
kept and what not, sentences."
  (let* ((tops (coerce text-xml 'simple-vector))
         (n (length tops))
         (main-page (docx-import-page main))
         (marks (make-array n :initial-element nil))
         (sideways 0) (in-columns 0) (notes '()))
    (flet ((level (i) (and (< i n) (cl-ppcre:scan "^<w:p[\\s>]" (svref tops i)) (docx-import-heading-level (svref tops i))))
           (list-item-p (i) (search "<w:numPr>" (svref tops i)))
           (worded-p (i) (let ((xml (svref tops i)))
                           (or (cl-ppcre:scan "^<w:tbl[\\s>]" xml)
                               (plusp (length (string-trim '(#\Space #\Tab) (docx-xml-text xml))))
                               (search "<w:drawing" xml)))))
      (let ((i 0))
        (loop while (< i n)
              do (let* ((section (nth i governing))
                        (end (or (position-if-not (lambda (g) (equal g section)) governing :start i) n))
                        (page (docx-import-page section))
                        (landscape (and (getf page :landscape) (not (getf main-page :landscape))))
                        (columns (if (/= (getf page :columns) (getf main-page :columns)) (getf page :columns) 1)))
                   (when (and (getf main-page :landscape) (not (getf page :landscape)))
                     (push "a section of upright pages in a document of sideways ones is not kept: its text is on the template's pages" notes))
                   (when (or landscape (> columns 1))
                     (if landscape (incf sideways) (incf in-columns))
                     ;; the margins as they are, or turned with the page
                     (unless (and (equal (getf page :size) (getf main-page :size))
                                  (let ((m (getf page :margins)))
                                    (or (equal m (getf main-page :margins))
                                        (equal (list (fourth m) (first m) (second m) (third m)) (getf main-page :margins))
                                        (equal (list (second m) (third m) (fourth m) (first m)) (getf main-page :margins)))))
                       (pushnew "a later section's own paper size or margins are not kept: its pages are the template's, turned as they were"
                                notes :test #'string=))
                     (let ((j i))
                       (loop while (< j end)
                             do (let ((heading (level j)))
                                  (if heading
                                      (let ((subtree-end (or (position-if (lambda (k) (let ((l (level k))) (and l (<= l heading))))
                                                                          (loop for k from (1+ j) below n collect k))
                                                             n)))
                                        (if (<= subtree-end end)
                                            (setf (svref marks j) (list landscape columns :heading) j subtree-end)
                                            (progn (setf (svref marks j) (list landscape columns :element)) (incf j))))
                                      (progn
                                        (when (and (worded-p j) (not (and (list-item-p j) (> j i) (list-item-p (1- j)))))
                                          (setf (svref marks j) (list landscape columns :element)))
                                        (incf j)))))))
                   (setf i end))))
      (when (plusp (+ sideways in-columns))
        (push (format nil "~@[~d later section~:p on pages turned sideways~]~:[~; and ~]~@[~d in columns~] kept as page settings (:EXPORT: page, columns)"
                      (and (plusp sideways) sideways) (and (plusp sideways) (plusp in-columns)) (and (plusp in-columns) in-columns))
              notes))
      (values (loop for i below n
                    for mark = (svref marks i)
                    when mark collect (apply #'docx-import-marker mark)
                    collect (svref tops i))
              (nreverse notes)))))

(defun docx-import-page-drawers (text)
  "TEXT, the note's text read, with each page setting's marker made an
:EXPORT: drawer: after the heading that follows it (all under that heading),
else before the element that follows it. The marker is found as the text
read writes it, its @@ kept from being markup (DOCX-READ-ESCAPED)."
  (flet ((drawer (landscape columns)
           (format nil ":EXPORT:~%~:[~;page: landscape~%~]~@[columns: ~d~%~]:END:~%"
                   (string= landscape "landscape") (let ((c (parse-integer columns))) (and (> c 1) c)))))
    (let* ((at (docx-read-escaped "@@"))
           (text (cl-ppcre:regex-replace-all (format nil "(?m)^~aTUATARA-PAGE (\\w+) (\\d+) heading~a\\n+(\\*+ [^\\n]*\\n)" at at) text
                                             (lambda (match landscape columns heading)
                                               (declare (ignore match))
                                               (concatenate 'string heading (drawer landscape columns)))
                                             :simple-calls t))
           (text (cl-ppcre:regex-replace-all (format nil "(?m)^~aTUATARA-PAGE (\\w+) (\\d+) (?:heading|element)~a\\n+" at at) text
                                             (lambda (match landscape columns)
                                               (declare (ignore match))
                                               (drawer landscape columns))
                                             :simple-calls t)))
      text)))

(defun docx-import-note (name text-xml fields title numbered contents)
  "The note template of the template NAME (TEMPLATE-NOTE-TEXT): TITLE, a
keyword for each of FIELDS, then the document's text TEXT-XML as Org, its
headings' numbers left to the export when NUMBERED, CONTENTS true for a
table of contents."
  (let* ((*docx-read-writing* t)
         (tree (xml-parse (format nil "<w:body>~{~a~}</w:body>" text-xml)))
         (body (first (xml-find-all tree "w:body")))
         (text (with-output-to-string (s) (when body (docx-read-block body s))))
         (text (if numbered (cl-ppcre:regex-replace-all "(?m)^(\\*+ )\\d+(?:\\.\\d+)*\\.?\\s+" text "\\1") text))
         (text (docx-import-page-drawers text))
         (drawers (search (format nil "~%:EXPORT:~%") (format nil "~%~a" text)))
         (notes (with-output-to-string (n) (docx-read-footnote-lines text n))))
    (template-note-text name (or title name) fields
                        (if (plusp (length notes)) (format nil "~a~%~a" text notes) text)
                        :contents contents :numbered numbered :drawers drawers)))

;;; The whole

(defun docx-import-without-changes (xml)
  "XML, a part of a template made from a Word document, without what the
import leaves out: its tracked changes accepted (what was deleted gone, what
was inserted kept as it is), and the references to its comments, footnotes
and endnotes, whose notes are not kept, so that Word finds nothing missing."
  (flet ((inside (element) (xml-element-inside element (cons 0 (length element)))))
    (let* ((xml (xml-replace-elements xml "w:del" (constantly nil)))
           (xml (xml-replace-elements xml "w:moveFrom" (constantly nil)))
           (xml (xml-replace-elements xml "w:ins" #'inside))
           (xml (xml-replace-elements xml "w:moveTo" #'inside))
           (xml (xml-replace-elements xml "w:r" (lambda (run)
                                                  (and (not (cl-ppcre:scan "<w:(?:commentReference|footnoteReference|endnoteReference)[\\s/>]" run))
                                                       run)))))
      (cl-ppcre:regex-replace-all "<w:(?:commentRangeStart|commentRangeEnd|moveFromRangeStart|moveFromRangeEnd|moveToRangeStart|moveToRangeEnd)[\\s/][^>]*>" xml ""))))

(defun docx-import-note-fetched (entries)
  "Note, of the document being imported whose parts are ENTRIES, each picture,
frame or document it shows from elsewhere (a web address, another computer's
share) and each INCLUDEPICTURE or INCLUDETEXT field: Word fetches them when a
report written from the template opens, and whoever keeps them can see it
was. Links clicked on are not noted."
  (let ((targets '()))
    (dolist (entry entries)
      (cond ((string= entry "word/_rels/settings.xml.rels"))  ; its attached template, left out
            ((uiop:string-suffix-p entry ".rels")
             (cl-ppcre:do-matches-as-strings (relation "<Relationship [^>]*TargetMode=\"External\"[^>]*>" (docx-reading-text entry))
               (unless (search "/hyperlink\"" relation)
                 (cl-ppcre:register-groups-bind (target) ("Target=\"([^\"]*)\"" relation)
                   (pushnew (xml-text-value target) targets :test #'string=)))))
            ((cl-ppcre:scan "^word/.*\\.xml$" entry)
             (cl-ppcre:do-register-groups (field) ("(INCLUDE(?:PICTURE|TEXT)\\s+(?:\\\\[a-z]\\s+)*\"?[^\"<\\s]*)" (docx-reading-text entry))
               (pushnew field targets :test #'string=)))))
    (when targets
      (template-import-note (format nil "it fetches from elsewhere when a report opens, which whoever keeps it can see: ~{~a~^, ~} (take them out in Word if they should not be)"
                                    (mapcar #'org-safe-line (reverse targets)))))))

(defun docx-import (path name &key (folder (concatenate 'string (docx-templates-directory) name "/"))
                                   (tex (concatenate 'string (latex-templates-directory) name ".tex"))
                                   note)
  "Make of the Word document at PATH the template NAME, its Word half in
FOLDER and its LaTeX half TEX, and its note template NOTE (NAME.org in
NOTE-TEMPLATES-DIRECTORY, unless given), as this file's header says, its
pictures in the folder NAME-media beside NOTE. Returns the note template's
path, and as a second value what to say of it, a list of sentences. Signals
an error when it cannot be read; writes over nothing that exists."
  (let* ((*docx-reading* (docx-reading-of path))
         (*template-import* (make-template-import))
         (note (or note (concatenate 'string (note-templates-directory) name ".org")))
         (media (template-import-media note name)))
    (docx-reading-setup)
    (template-import-check-free (list folder tex note media))
    ;; a folder's own entry, which zip -r makes, holds nothing to write
    (setf (docx-reading-entries *docx-reading*)
          (remove-if (lambda (e) (let ((name (getf e :name))) (and (uiop:string-suffix-p name "/") (docx-safe-file-name-p (string-right-trim "/" name)))))
                     (docx-reading-entries *docx-reading*)))
    ;; a part named to land outside the template's folder (../../init.lisp): nothing made at all
    (let ((unsafe (find-if-not #'docx-safe-file-name-p (docx-reading-entries *docx-reading*) :key (lambda (e) (getf e :name)))))
      (when unsafe
        (error "~a holds a part named ~s, outside the document: nothing made" (tuatara.io:native-file-name path) (getf unsafe :name))))
    (unless (string-equal (docx-main-part) "word/document.xml")
      (error "~a keeps its text in ~a, not word/document.xml as a template's is: nothing made"
             (tuatara.io:native-file-name path) (docx-main-part)))
    (let ((document (or (docx-reading-text "word/document.xml") (error "~a holds no Word document" (tuatara.io:native-file-name path)))))
      (multiple-value-bind (front text-xml section before after governing) (docx-import-split document)
        (multiple-value-bind (front contents) (docx-import-contents front)
          (let* ((front (docx-import-fields-in front :front t))
                 (title (or (let ((f (find "title" (template-import-fields *template-import*) :key #'first :test #'string=))) (and f (third f)))
                            (let ((core (docx-reading-part "docProps/core.xml")))
                              (and core (let ((e (first (xml-find-all core "dc:title")))) (and e (plusp (length (xml-all-text e))) (xml-all-text e)))))))
                 (body-paragraph "<w:p><w:r><w:t>{{{body}}}</w:t></w:r></w:p>")
                 (final (cl-ppcre:regex-replace-all "<w:type w:val=\"[^\"]*\"/>" (or section "") ""))
                 (new-document (concatenate 'string before front body-paragraph final after))
                 (numbered (some (lambda (x) (cl-ppcre:scan "<w:t[^>]*>\\s*\\d+(\\.\\d+)*\\.?\\s+[A-Za-z]" x))
                                 (remove-if-not (lambda (x) (docx-import-heading-level x)) text-xml)))
                 (entries (mapcar (lambda (e) (getf e :name)) (docx-reading-entries *docx-reading*)))
                 (headers '())
                 (marked-xml text-xml)
                 (parts '()))
            ;; later sections, sideways or in columns: page settings in the note
            (multiple-value-bind (marked words) (docx-import-section-marks text-xml governing section)
              (setf marked-xml marked)
              (dolist (w words) (template-import-note w)))
            ;; every part kept as it is but those the import changes or leaves out
            (dolist (entry entries)
              (cond
                ((string= entry "word/document.xml") (push (cons entry (docx-import-without-changes new-document)) parts))
                ((cl-ppcre:scan "(?i)vbaProject|vbaData|word/_rels/vbaProject" entry)
                 (template-import-note "its macros left out"))
                ((cl-ppcre:scan "^word/comments|^word/people\\.xml" entry)
                 (template-import-note "its comments left out"))
                ((member entry '("word/footnotes.xml" "word/endnotes.xml") :test #'string=)
                 ;; the separators kept, the notes of the text left out
                 (let ((xml (docx-reading-text entry)) (element (if (search "footnotes" entry) "w:footnote" "w:endnote")))
                   (push (cons entry (xml-replace-elements xml element
                                                           (lambda (note) (and (search "w:type=" note :end2 (position #\> note)) note))))
                         parts)))
                ((string= entry "word/settings.xml")
                 (let* ((xml (docx-reading-text entry))
                        (xml (cl-ppcre:regex-replace-all "<w:attachedTemplate [^>]*/>" xml ""))
                        (xml (cl-ppcre:regex-replace-all "<w:trackRevisions(?:\\s[^>]*)?/>" xml ""))
                        (xml (cl-ppcre:regex-replace-all "<w:documentProtection [^>]*/>" xml "")))
                   (push (cons entry xml) parts)))
                ((string= entry "word/_rels/settings.xml.rels")
                 (template-import-note "the link to its attached template left out"))
                ((cl-ppcre:scan "^word/(header|footer)\\d*\\.xml$" entry)
                 (let ((filled (docx-import-fields-in (docx-reading-text entry))))
                   (push (cons entry (docx-import-without-changes filled)) parts)))
                (t (push (cons entry (docx-reading-octets-of entry)) parts))))
            (docx-import-note-fetched entries)
            ;; the relations and types without what was left out
            (setf parts
                  (mapcar (lambda (part)
                            (cond ((string= (car part) "word/_rels/document.xml.rels")
                                   (cons (car part) (cl-ppcre:regex-replace-all "<Relationship [^>]*Target=\"(?:comments|commentsExtended|commentsIds|commentsExtensible|people|vbaProject\\.bin)[^\"]*\"[^>]*/>"
                                                                                (docx-reading-text (car part)) "")))
                                  ((string= (car part) "[Content_Types].xml")
                                   (cons (car part) (let* ((xml (docx-reading-text (car part)))
                                                           (xml (cl-ppcre:regex-replace-all "<Override PartName=\"/word/(?:comments|commentsExtended|commentsIds|commentsExtensible|people|vbaProject|vbaData)[^\"]*\"[^>]*/>" xml ""))
                                                           (xml (cl-ppcre:regex-replace-all "<Default Extension=\"bin\" ContentType=\"application/vnd.ms-office.vbaProject\"/>" xml "")))
                                                      (cl-ppcre:regex-replace-all "application/vnd\\.ms-word\\.(document|template)\\.macroEnabled\\.main\\+xml" xml
                                                                                  "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"))))
                                  (t part)))
                          parts))
            (push (cons *docx-template-design-file* (docx-import-design text-xml)) parts)
            ;; the headers and footers of the text's section, for the LaTeX half
            (let ((rels (docx-part-relations (docx-main-part))))
              (cl-ppcre:do-register-groups (element type id) ("<w:(header|footer)Reference w:type=\"(\\w+)\" r:id=\"([^\"]+)\"" (or section ""))
                (let* ((target (docx-relation-target rels id))
                       (part (and target (cdr (assoc target parts :test #'string=)))))
                  (when (stringp part)
                    (push (cons (intern (format nil "~:[~;FIRST-~]~:@(~a~)" (string= type "first") element) :keyword) part) headers)))))
            ;; written: the Word half, the LaTeX half, the pictures and the note
            (dolist (part parts)
              (template-import-write (concatenate 'string folder (car part)) (cdr part)))
            (template-import-write tex (docx-import-latex name front (nreverse headers) section (template-import-fields *template-import*) contents))
            (let* ((*docx-read-media* (abbreviated-path media))
                   (*docx-read-pictures* '())
                   (text (docx-import-note name marked-xml (template-import-fields *template-import*) title numbered contents)))
              (dolist (picture *docx-read-pictures*)
                (let ((octets (docx-reading-octets-of (car picture))))
                  (when octets
                    (template-import-write (tuatara.io:native (tuatara.io:expand-path (cdr picture))) octets))))
              (template-import-write note text))
            (template-import-result note)))))))
