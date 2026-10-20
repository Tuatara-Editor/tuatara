;;;; test/office-view.lisp --- Excel workbooks, PowerPoint presentations and OpenDocument files opened, read as text by Tuatara itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test office-view
  :description "An Excel workbook is read as an Org note: its sheets in order, hidden and very hidden ones
marked, cells as a table of their saved values (shared and inline strings, numbers as written, dates by
their format, booleans and errors), hidden rows and columns marked, formulas listed, never worked out,
a sheet no list names read at the end, the cells read held to a limit; a PowerPoint presentation as
one: each slide's title, text, tables, pictures and speaker notes, hidden slides marked. OpenDocument
text, spreadsheets and presentations are read so too, by what they hold, a locked one refused. All
refuse parts too large; all are shown as their pages by LibreOffice in the sandbox, as Word documents
are, and in rimu's preview.")

(defparameter *xlsx-test-parts*
  '(("[Content_Types].xml" . "<?xml version='1.0' encoding='UTF-8'?><Types xmlns='http://schemas.openxmlformats.org/package/2006/content-types'><Default Extension='rels' ContentType='application/vnd.openxmlformats-package.relationships+xml'/><Default Extension='xml' ContentType='application/xml'/><Default Extension='bin' ContentType='application/vnd.ms-office.vbaProject'/><Override PartName='/xl/workbook.xml' ContentType='application/vnd.ms-excel.sheet.macroEnabled.main+xml'/><Override PartName='/xl/worksheets/sheet1.xml' ContentType='application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml'/><Override PartName='/xl/worksheets/sheet2.xml' ContentType='application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml'/><Override PartName='/xl/worksheets/sheet3.xml' ContentType='application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml'/><Override PartName='/xl/sharedStrings.xml' ContentType='application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml'/><Override PartName='/xl/styles.xml' ContentType='application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml'/><Override PartName='/docProps/core.xml' ContentType='application/vnd.openxmlformats-package.core-properties+xml'/></Types>")
    ("_rels/.rels" . "<?xml version='1.0' encoding='UTF-8'?><Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='xl/workbook.xml'/><Relationship Id='rId2' Type='http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties' Target='docProps/core.xml'/></Relationships>")
    ("docProps/core.xml" . "<?xml version='1.0' encoding='UTF-8'?><cp:coreProperties xmlns:cp='http://schemas.openxmlformats.org/package/2006/metadata/core-properties' xmlns:dc='http://purl.org/dc/elements/1.1/'><dc:creator>Bo Kaikoura</dc:creator></cp:coreProperties>")
    ("xl/workbook.xml" . "<?xml version='1.0' encoding='UTF-8'?><workbook xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main' xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships'><sheets><sheet name='Birds' sheetId='1' r:id='rId1'/><sheet name='Secret' sheetId='2' state='hidden' r:id='rId2'/><sheet name='Deep' sheetId='3' state='veryHidden' r:id='rId3'/></sheets></workbook>")
    ("xl/_rels/workbook.xml.rels" . "<?xml version='1.0' encoding='UTF-8'?><Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet' Target='worksheets/sheet1.xml'/><Relationship Id='rId2' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet' Target='worksheets/sheet2.xml'/><Relationship Id='rId3' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet' Target='/xl/worksheets/sheet3.xml'/><Relationship Id='rId4' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings' Target='sharedStrings.xml'/><Relationship Id='rId5' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles' Target='styles.xml'/></Relationships>")
    ("xl/sharedStrings.xml" . "<?xml version='1.0' encoding='UTF-8'?><sst xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main' count='4' uniqueCount='4'><si><t>Bird</t></si><si><t>Count</t></si><si><t>Kea | alpine</t></si><si><r><t>Kaka</t></r><r><rPr><b/></rPr><t>po</t></r><rPh sb='0' eb='1'><t>READING AID</t></rPh></si></sst>")
    ("xl/styles.xml" . "<?xml version='1.0' encoding='UTF-8'?><styleSheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><numFmts count='1'><numFmt numFmtId='164' formatCode='yyyy\\-mm\\-dd'/></numFmts><fonts count='1'><font><sz val='11'/><name val='Calibri'/></font></fonts><fills count='1'><fill><patternFill patternType='none'/></fill></fills><borders count='1'><border/></borders><cellStyleXfs count='1'><xf numFmtId='0' fontId='0' fillId='0' borderId='0'/></cellStyleXfs><cellXfs count='4'><xf numFmtId='0' fontId='0' fillId='0' borderId='0' xfId='0'/><xf numFmtId='164' fontId='0' fillId='0' borderId='0' xfId='0' applyNumberFormat='1'/><xf numFmtId='22' fontId='0' fillId='0' borderId='0' xfId='0' applyNumberFormat='1'/><xf numFmtId='46' fontId='0' fillId='0' borderId='0' xfId='0' applyNumberFormat='1'/></cellXfs></styleSheet>")
    ("xl/worksheets/sheet1.xml" . "<?xml version='1.0' encoding='UTF-8'?><worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main' xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships'><cols><col min='3' max='3' width='9' hidden='1'/></cols><sheetData><row r='1'><c r='A1' t='s'><v>0</v></c><c r='B1' t='s'><v>1</v></c><c r='D1' t='inlineStr'><is><t>Seen</t></is></c></row><row r='2'><c r='A2' t='s'><v>2</v></c><c r='B2'><v>12</v></c><c r='C2' t='inlineStr'><is><t>x</t></is></c><c r='D2' s='1'><v>45352</v></c></row><row r='3' hidden='1'><c r='A3' t='s'><v>3</v></c><c r='B3'><v>3.5</v></c></row><row r='5'><c r='B5'><f>SUM(B2:B3)</f><v>15.5</v></c></row><row r='6'><c r='B6'><f t='shared' ref='B6:B7' si='0'>B2*2</f><v>24</v></c></row><row r='7'><c r='B7'><f t='shared' si='0'/><v>7</v></c></row><row r='8'><c r='A8' t='b'><v>1</v></c><c r='B8' t='e'><v>#DIV/0!</v></c><c r='C8' s='2'><v>45352.75</v></c><c r='D8' s='3'><v>1.5</v></c></row></sheetData><hyperlinks><hyperlink ref='A2' r:id='rId1'/></hyperlinks></worksheet>")
    ("xl/worksheets/_rels/sheet1.xml.rels" . "<?xml version='1.0' encoding='UTF-8'?><Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink' Target='https://example.org/kea' TargetMode='External'/></Relationships>")
    ("xl/worksheets/sheet2.xml" . "<?xml version='1.0' encoding='UTF-8'?><worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><sheetData><row r='1'><c r='A1' t='inlineStr'><is><t>hidden words</t></is></c></row></sheetData></worksheet>")
    ("xl/worksheets/sheet3.xml" . "<?xml version='1.0' encoding='UTF-8'?><worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><sheetData><row r='1'><c r='A1' t='inlineStr'><is><t>very hidden words</t></is></c></row></sheetData></worksheet>")
    ("xl/worksheets/sheet9.xml" . "<?xml version='1.0' encoding='UTF-8'?><worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><sheetData><row r='1'><c r='A1' t='inlineStr'><is><t>orphan words</t></is></c></row></sheetData></worksheet>")
    ("xl/vbaProject.bin" . "not run"))
  "The parts of a small Excel workbook made for the tests, as a spreadsheet program writes them.")

(defparameter *pptx-test-parts*
  (let ((p "xmlns:a='http://schemas.openxmlformats.org/drawingml/2006/main' xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships' xmlns:p='http://schemas.openxmlformats.org/presentationml/2006/main'")
        (relations "xmlns='http://schemas.openxmlformats.org/package/2006/relationships'")
        (type "http://schemas.openxmlformats.org/officeDocument/2006/relationships"))
    (flet ((shape (id placeholder &rest paragraphs)
             (format nil "<p:sp><p:nvSpPr><p:cNvPr id='~d' name='Shape ~:*~d'/><p:cNvSpPr/><p:nvPr>~@[<p:ph type='~a'/>~]</p:nvPr></p:nvSpPr><p:spPr/><p:txBody><a:bodyPr/>~{~a~}</p:txBody></p:sp>"
                     id placeholder
                     (mapcar (lambda (paragraph)
                               (destructuring-bind (text &optional level) (if (listp paragraph) paragraph (list paragraph))
                                 (format nil "<a:p>~@[<a:pPr lvl='~d'/>~]<a:r><a:t>~a</a:t></a:r></a:p>" level text)))
                             paragraphs)))
           (slide (show &rest shapes)
             (format nil "<?xml version='1.0' encoding='UTF-8'?><p:sld ~a~@[ show='~a'~]><p:cSld><p:spTree><p:nvGrpSpPr><p:cNvPr id='1' name=''/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr><p:grpSpPr/>~{~a~}</p:spTree></p:cSld></p:sld>"
                     p show shapes)))
      (list
       (cons "[Content_Types].xml" "<?xml version='1.0' encoding='UTF-8'?><Types xmlns='http://schemas.openxmlformats.org/package/2006/content-types'><Default Extension='rels' ContentType='application/vnd.openxmlformats-package.relationships+xml'/><Default Extension='xml' ContentType='application/xml'/><Override PartName='/ppt/presentation.xml' ContentType='application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml'/><Override PartName='/ppt/slides/slide1.xml' ContentType='application/vnd.openxmlformats-officedocument.presentationml.slide+xml'/><Override PartName='/ppt/slides/slide2.xml' ContentType='application/vnd.openxmlformats-officedocument.presentationml.slide+xml'/><Override PartName='/ppt/notesSlides/notesSlide1.xml' ContentType='application/vnd.openxmlformats-officedocument.presentationml.notesSlide+xml'/><Override PartName='/docProps/app.xml' ContentType='application/vnd.openxmlformats-officedocument.extended-properties+xml'/></Types>")
       (cons "_rels/.rels" (format nil "<?xml version='1.0' encoding='UTF-8'?><Relationships ~a><Relationship Id='rId1' Type='~a/officeDocument' Target='ppt/presentation.xml'/><Relationship Id='rId2' Type='~a/extended-properties' Target='docProps/app.xml'/></Relationships>" relations type type))
       (cons "docProps/app.xml" "<?xml version='1.0' encoding='UTF-8'?><Properties xmlns='http://schemas.openxmlformats.org/officeDocument/2006/extended-properties'><Application>Some Presentation Program</Application><Slides>2</Slides><Notes>1</Notes><HiddenSlides>1</HiddenSlides></Properties>")
       (cons "ppt/presentation.xml" (format nil "<?xml version='1.0' encoding='UTF-8'?><p:presentation ~a><p:sldIdLst><p:sldId id='256' r:id='rId2'/><p:sldId id='257' r:id='rId3'/></p:sldIdLst><p:sldSz cx='9144000' cy='6858000'/><p:notesSz cx='6858000' cy='9144000'/></p:presentation>" p))
       (cons "ppt/_rels/presentation.xml.rels" (format nil "<?xml version='1.0' encoding='UTF-8'?><Relationships ~a><Relationship Id='rId2' Type='~a/slide' Target='slides/slide1.xml'/><Relationship Id='rId3' Type='~a/slide' Target='slides/slide2.xml'/></Relationships>" relations type type))
       (cons "ppt/slides/slide1.xml"
             (slide nil
                    (shape 2 "title" "Kea survey")
                    (shape 3 "body" "Counted twice" '("At dawn" 1))
                    "<p:graphicFrame><p:nvGraphicFramePr><p:cNvPr id='4' name='Table'/><p:cNvGraphicFramePr/><p:nvPr/></p:nvGraphicFramePr><p:xfrm/><a:graphic><a:graphicData uri='http://schemas.openxmlformats.org/drawingml/2006/table'><a:tbl><a:tr h='0'><a:tc><a:txBody><a:bodyPr/><a:p><a:r><a:t>Bird</a:t></a:r></a:p></a:txBody></a:tc><a:tc><a:txBody><a:bodyPr/><a:p><a:r><a:t>Count</a:t></a:r></a:p></a:txBody></a:tc></a:tr><a:tr h='0'><a:tc><a:txBody><a:bodyPr/><a:p><a:r><a:t>Kea</a:t></a:r></a:p></a:txBody></a:tc><a:tc><a:txBody><a:bodyPr/><a:p><a:r><a:t>12</a:t></a:r></a:p></a:txBody></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>"
                    "<p:pic><p:nvPicPr><p:cNvPr id='5' name='Picture 5' descr='A kea on a rock'/><p:cNvPicPr/><p:nvPr/></p:nvPicPr><p:blipFill/><p:spPr/></p:pic>"
                    (shape 6 nil "* Forged heading")))
       (cons "ppt/slides/_rels/slide1.xml.rels" (format nil "<?xml version='1.0' encoding='UTF-8'?><Relationships ~a><Relationship Id='rId2' Type='~a/notesSlide' Target='../notesSlides/notesSlide1.xml'/></Relationships>" relations type))
       (cons "ppt/notesSlides/notesSlide1.xml"
             (format nil "<?xml version='1.0' encoding='UTF-8'?><p:notes ~a><p:cSld><p:spTree><p:nvGrpSpPr><p:cNvPr id='1' name=''/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr><p:grpSpPr/>~a~a~a</p:spTree></p:cSld></p:notes>"
                     p (shape 2 "sldImg") (shape 3 "body" "Say the count slowly.") (shape 4 "sldNum" "99")))
       (cons "ppt/slides/slide2.xml" (slide "0" (shape 2 "title" "Hidden findings") (shape 3 "body" "Not for the meeting")))
       (cons "ppt/slides/slide5.xml" (slide nil (shape 2 "title" "Leftover") (shape 3 "body" "Left behind"))))))
  "The parts of a small PowerPoint presentation made for the tests.")

(defun write-office-test-file (path parts)
  "Write PARTS, each (NAME . TEXT), as a zip archive at PATH; return PATH as a string."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (tuatara.defaults::zip-octets parts) out))
  (namestring path))

(defun note-has (note text)
  "Check that NOTE holds TEXT."
  (true (search text note) "~s in~%~a" text note))

(define-test (office-view workbook-read)
  (with-test-directory (dir)
    (let* ((path (write-office-test-file (merge-pathnames "birds.xlsm" dir) *xlsx-test-parts*))
           (note (tuatara.defaults::xlsx-read-note path)))
      (note-has note "#+title: birds.xlsm")
      (note-has note "* About this workbook")
      (note-has note "- Made by :: Bo Kaikoura")
      (note-has note "- Sheets :: 3, 1 hidden, 1 very hidden")
      (note-has note "- Values :: as last saved with the file; no formula is worked out here")
      (note-has note "- Macros :: yes (xl/vbaProject.bin), never run")
      (note-has note "- Links outside it :: https://example.org/kea (none followed)")
      (note-has note (format nil "* Sheet 1: Birds~%|  | A | B | C (hidden) | D |~%|-~%| 1 | Bird | Count |  | Seen |~%"))
      ;; shared strings, a | kept in its cell, a number as written, a date by its format
      (note-has note "| 2 | Kea \\vert{} alpine | 12 | x | 2024-03-01 |")
      ;; a hidden row; a string of runs, its reading aid left out
      (note-has note "| 3 (hidden) | Kakapo | 3.5 |  |  |")
      (false (search "READING AID" note))
      ;; formulas: their saved values in the table, their text below
      (note-has note "| 5 |  | 15.5 |  |  |")
      (note-has note "| 7 |  | 7 |  |  |")
      (note-has note "- B5 :: =SUM(B2:B3)")
      (note-has note "- B6 :: =B2*2")
      (note-has note "- B7 :: as B6's, shared")
      ;; true, an error, a day and its time, an elapsed time as its number
      (note-has note "| 8 | TRUE | #DIV/0! | 2024-03-01 18:00:00 | 1.5 |")
      (note-has note (format nil "* Sheet 2: Secret (hidden)~%|  | A |"))
      (note-has note "| 1 | hidden words |")
      (note-has note "* Sheet 3: Deep (very hidden: only a program can show it again)")
      (note-has note "| 1 | very hidden words |")
      (note-has note "* Sheet part xl/worksheets/sheet9.xml (in the file, named by no list of sheets)")
      (note-has note "| 1 | orphan words |"))))

(define-test (office-view workbook-dates)
  (flet ((date (value kind &optional date1904) (tuatara.defaults::sheet-date-text value kind date1904)))
    (is string= "1900-01-01" (date "1" :date))
    (is string= "1900-02-28" (date "59" :date))
    (is string= "1900-02-29" (date "60" :date))
    (is string= "1900-03-01" (date "61" :date))
    (is string= "2024-03-01" (date "45352" :date))
    (is string= "1904-01-02" (date "1" :date t))
    (is string= "12:00:00" (date "0.5" :time))
    (is string= "-3" (date "-3" :date))
    (is string= "1E+30" (date "1E+30" :date)))
  (is eq :date (tuatara.defaults::sheet-format-kind "d mmm yyyy"))
  (is eq :time (tuatara.defaults::sheet-format-kind "hh:mm:ss"))
  (is eq nil (tuatara.defaults::sheet-format-kind "[h]:mm"))
  (is eq nil (tuatara.defaults::sheet-format-kind "#,##0.00 \"days\""))
  (is eq nil (tuatara.defaults::sheet-format-kind "[Red]0.00"))
  (is string= "XFD" (tuatara.defaults::sheet-column-letters 16384))
  (is equal '(16384 1048576) (multiple-value-list (tuatara.defaults::sheet-cell-place "XFD1048576")))
  (false (tuatara.defaults::sheet-cell-place "XFE1")))

(define-test (office-view workbook-limits)
  (with-test-directory (dir)
    (let ((path (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*)))
      ;; the cells read held: where it stopped said, the sheets after not read
      (let* ((tuatara.defaults::*sheet-most-cells* 3)
             (note (tuatara.defaults::xlsx-read-note path)))
        (note-has note "| 1 | Bird | Count | Seen |")
        (false (search "Kea" note))
        (note-has note "Only the first 3 cells are read: from A2 on")
        (note-has note "Not read: the workbook's first 3 cells were (*sheet-most-cells*).")
        (false (search "hidden words" note)))
      ;; a sheet larger unpacked than a part may be, or the parts together larger than allowed: refused
      (let ((tuatara.defaults::*docx-largest-xml* 400))
        (fail (tuatara.defaults::xlsx-read-note path)))
      (let ((tuatara.defaults::*docx-largest-unpacked* 2000))
        (fail (tuatara.defaults::xlsx-read-note path)))
      ;; an archive that is none, and a presentation read as a workbook
      (alexandria:write-string-into-file "PK not really" (merge-pathnames "broken.xlsx" dir))
      (fail (tuatara.defaults::xlsx-read-note (namestring (merge-pathnames "broken.xlsx" dir))))
      (let ((deck (write-office-test-file (merge-pathnames "deck.xlsx" dir) *pptx-test-parts*)))
        (fail (tuatara.defaults::xlsx-read-note deck))))))

(define-test (office-view workbook-sparse)
  ;; cells far apart make a list, not a table of empty places; columns hidden in odd ways, a cell twice
  (with-test-directory (dir)
    (let* ((cells (with-output-to-string (s)
                    (loop for i from 1 to 10
                          do (format s "<row r='~d'><c r='~a~d' t='inlineStr'><is><t>v~d</t></is></c>~:[~;<c r='A1' t='inlineStr'><is><t>again</t></is></c>~]</row>"
                                     (* i 1000) (tuatara.defaults::sheet-column-letters (* i 1600)) (* i 1000) i (= i 1)))))
           (sheet (format nil "<worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><cols><col min='5' max='2' hidden='1'/><col min='1599' max='99999999' hidden='1'/></cols><sheetData>~a</sheetData></worksheet>" cells))
           (parts (cons (cons "xl/worksheets/sheet1.xml" sheet) (remove "xl/worksheets/sheet1.xml" *xlsx-test-parts* :key #'car :test #'string=)))
           (path (write-office-test-file (merge-pathnames "sparse.xlsx" dir) parts))
           (note (let ((tuatara.defaults::*sheet-most-cells* 11)) (tuatara.defaults::xlsx-read-note path))))
      (note-has note "Its cells, too far apart for a table:")
      (note-has note "- BIN (hidden), row 1000 :: v1")
      (note-has note "- A, row 1000 :: again")
      (note-has note "- WQJ (hidden), row 10000 :: v10")
      (false (search "|  | " (subseq note 0 (search "* Sheet 2" note)))))
    ;; a cell given twice in a row: once in the table
    (let* ((sheet "<worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><sheetData><row r='1'><c r='B1'><v>2</v></c><c r='A1'><v>1</v></c><c r='B1'><v>3</v></c></row></sheetData></worksheet>")
           (parts (cons (cons "xl/worksheets/sheet1.xml" sheet) (remove "xl/worksheets/sheet1.xml" *xlsx-test-parts* :key #'car :test #'string=)))
           (note (tuatara.defaults::xlsx-read-note (write-office-test-file (merge-pathnames "twice.xlsx" dir) parts))))
      (note-has note (format nil "|  | A | B |~%|-~%| 1 | 1 | 2 |~%")))))

(define-test (office-view presentation-read)
  (with-test-directory (dir)
    (let* ((path (write-office-test-file (merge-pathnames "kea.pptx" dir) *pptx-test-parts*))
           (note (tuatara.defaults::pptx-read-note path)))
      (note-has note "#+title: kea.pptx")
      (note-has note "* About this presentation")
      (note-has note "- Program :: Some Presentation Program")
      (note-has note "- Slides :: 2, 1 hidden, 1 with notes")
      (note-has note (format nil "* Slide 1: Kea survey~%- Counted twice~%  - At dawn~%| Bird | Count |~%|-~%| Kea | 12 |~%~%- [picture: A kea on a rock]~%"))
      ;; text on a slide never becomes a heading of the note
      (note-has note "- * Forged heading")
      (false (cl-ppcre:scan "(?m)^\\* Forged" note))
      ;; its notes, the notes page's number left out
      (note-has note (format nil "** Speaker notes~%- Say the count slowly.~%~%"))
      (false (search "99" note))
      (note-has note (format nil "* Slide 2 (hidden): Hidden findings~%- Not for the meeting~%"))
      (note-has note (format nil "* Slide part ppt/slides/slide5.xml (in the file, named by no list of slides): Leftover~%- Left behind~%"))
      (let ((tuatara.defaults::*docx-largest-xml* 300))
        (fail (tuatara.defaults::pptx-read-note path))))))

(define-test (office-view office-kinds-told)
  (with-test-directory (dir)
    (let ((xlsx (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*))
          (pptx (write-office-test-file (merge-pathnames "kea.ppsx" dir) *pptx-test-parts*))
          (old (namestring (merge-pathnames "old.xls" dir)))
          (named (namestring (merge-pathnames "fake.xls" dir))))
      (with-open-file (out old :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (coerce (append '(#xD0 #xCF #x11 #xE0 #xA1 #xB1 #x1A #xE1) (make-list 504 :initial-element 0)) '(vector (unsigned-byte 8))) out))
      (alexandria:write-string-into-file "PK but a zip archive" named)
      (is eq :xlsx (getf (tuatara.defaults::office-file-format xlsx) :kind))
      (is eq :pptx (getf (tuatara.defaults::office-file-format pptx) :kind))
      (is eq :xls (getf (tuatara.defaults::office-file-format old) :kind))
      ;; a name that says a kind whose first octets are not its own: no Office document
      (false (tuatara.defaults::office-file-format named))
      ;; the preview by contents: a compound file by its name too, a mail is none
      (is eq :xlsx (getf (tuatara.defaults::office-preview-format :xlsx "anything.bin") :kind))
      (is eq :ppt (getf (tuatara.defaults::office-preview-format :ole "talk.PPT") :kind))
      (false (tuatara.defaults::office-preview-format :ole "message.msg"))
      ;; LibreOffice given the kind's own ending, never the file's name
      (is string= "xlsx" (tuatara.defaults::office-ending (tuatara.defaults::office-file-format xlsx) "x; rm -rf ~.xlsx"))
      (is string= "ppsx" (tuatara.defaults::office-ending (tuatara.defaults::office-file-format pptx) "kea.ppsx"))
      ;; opened outside after asking (*open-externally-kinds*), with their own type
      (is string= "application/vnd.ms-excel" (tuatara.defaults::file-mime-type :ole "old.xls"))
      (is string= "application/vnd.openxmlformats-officedocument.presentationml.slideshow" (tuatara.defaults::file-mime-type :pptx "kea.ppsx"))
      (false (tuatara.defaults::open-externally-refusal xlsx))
      (false (tuatara.defaults::open-externally-refusal pptx)))))

(defun office-busy-p ()
  "True while an Office document is being read or drawn."
  (or (some #'tuatara.defaults::office-read-job-p tuatara.runtime:*jobs*)
      (some (lambda (b) (let ((view (tuatara.defaults::pdf-view-of b)))
                          (and view (or (tuatara.defaults::pdf-view-job view) (tuatara.defaults::pdf-view-text-job view)))))
            tuatara.defaults::*pdf-views*)))

(define-test (office-view office-visited-as-text)
  ;; where pages are not drawn, read as text at once, read only, in Org mode; an older one as its octets
  (with-test-directory (dir)
    (let ((xlsx (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*))
          (pptx (write-office-test-file (merge-pathnames "kea.pptx" dir) *pptx-test-parts*)))
      (dolist (case (list (list xlsx "birds.xlsx (text)" "* Sheet 1: Birds") (list pptx "kea.pptx (text)" "* Slide 1: Kea survey")))
        (destructuring-bind (path name text) case
          (multiple-value-bind (f editor) (docx-session path '())
            (declare (ignore f))
            (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (is string= name (tuatara.buffer:buffer-name buffer))
              (true (tuatara.buffer:buffer-local buffer :read-only))
              (true (tuatara.command::buffer-mode-named-p buffer 'tuatara.defaults::org-mode))
              (true (search text (tuatara.buffer:buffer-string buffer)))
              (tuatara.buffer:kill-buffer buffer))))))))

(defun office-pages-made-p (path)
  "True when LibreOffice, shut in by the sandbox, makes the pages of the
Office document at PATH here: installed with what reads its kind (Calc for a
workbook, Impress for a presentation)."
  (and (eq :ready (tuatara.io:sandbox-status))
       (tuatara.io:sandbox-find-program tuatara.defaults::*office-page-program*)
       (tuatara.defaults::find-pdf-decoder)
       (ignore-errors
        (tuatara.io:call-with-file path (lambda (fd native size)
                                          (declare (ignore size))
                                          (sb-posix:close (tuatara.defaults::office-pdf-copy
                                                           fd (tuatara.defaults::office-ending (tuatara.defaults::office-file-format native)
                                                                                               (tuatara.io:native-file-name native))))
                                          t)))))

(define-test (office-view office-pages)
  (with-test-directory (dir)
    (dolist (case `((,*xlsx-test-parts* "birds.xlsx" "Excel workbook, its pages drawn by LibreOffice" "About this workbook")
                    (,*pptx-test-parts* "kea.pptx" "PowerPoint presentation, its pages drawn by LibreOffice" "About this presentation")))
      (destructuring-bind (parts name kind about) case
        (let ((path (write-office-test-file (merge-pathnames name dir) parts)))
          (if (not (office-pages-made-p path))
              (skip ("LibreOffice, the sandbox, or LibreOffice's part for ~a missing here" name) (true nil))
              (progn
              (pdf-forget)
              ;; its pages, made by LibreOffice in the sandbox; T its text, T its pages again
              (multiple-value-bind (f editor) (docx-session path '("T" "T") :cell-pixels '(10 20))
                (true (tuatara.frontend.fake:fake-frame-showing f kind))
                (true (tuatara.frontend.fake:fake-frame-showing f "Sandboxed"))
                (true (last-images f))
                (true (tuatara.frontend.fake:fake-frame-showing f about))
                (is string= name (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
              (let ((reading (tuatara.buffer:find-buffer (format nil "~a (text)" name))))
                (true reading)
                (when reading (tuatara.buffer:kill-buffer reading)))
              (pdf-forget)
              ;; not wanted: read as text at once
              (let ((tuatara.defaults::*office-view-pages* nil))
                (multiple-value-bind (f editor) (docx-session path '() :cell-pixels '(10 20))
                  (declare (ignore f))
                  (is string= (format nil "~a (text)" name)
                      (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
              (let ((reading (tuatara.buffer:find-buffer (format nil "~a (text)" name))))
                (when reading (tuatara.buffer:kill-buffer reading))))))))))

(define-test (office-view office-why-not-pages)
  ;; without LibreOffice where pages could be shown: read as text, said why; an older one opened as its octets, said why
  (with-test-directory (dir)
    (let ((xlsx (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*)))
      (pdf-forget)
      (let ((tuatara.defaults::*office-page-program* "no-such-office-tuatara"))
        (multiple-value-bind (f editor) (docx-session xlsx '() :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "birds.xlsx read as text: LibreOffice draws the pages, and is not installed (no-such-office-tuatara)"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (let ((old (namestring (merge-pathnames "old.xls" dir))))
        (with-open-file (out old :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (coerce (append '(#xD0 #xCF #x11 #xE0 #xA1 #xB1 #x1A #xE1) (make-list 504 :initial-element 0)) '(vector (unsigned-byte 8))) out))
        (let ((tuatara.defaults::*office-view-pages* nil))
          (multiple-value-bind (f editor) (docx-session old '() :cell-pixels '(10 20))
            (true (tuatara.frontend.fake:fake-frame-showing f "its pages are not shown here (*office-view-pages*)"))
            (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (is string= "old.xls" (tuatara.buffer:buffer-name buffer))
              (tuatara.buffer:kill-buffer buffer))))))))

(define-test (office-view office-in-rimu-preview)
  (with-rimu-tree (dir)
    (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*)
    (write-office-test-file (merge-pathnames "kea.pptx" dir) *pptx-test-parts*)
    ;; read as text by Tuatara, an Org note, where pictures are not shown
    (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "birds.xlsx")) :preview t :width 250)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Excel workbook (OOXML)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "birds.xlsx (preview, text)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "About this workbook"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Sheet 1: Birds")))
    (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "kea.pptx")) :preview t :width 250)))
      (true (tuatara.frontend.fake:fake-frame-showing f "PowerPoint presentation (OOXML)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Slide 1: Kea survey")))
    ;; its first page, drawn from the PDF LibreOffice makes in the sandbox
    (when (office-pages-made-p (namestring (merge-pathnames "kea.pptx" dir)))
      (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "kea.pptx")) :preview t :width 250 :cell-pixels '(10 20))))
        ;; its hidden slide not among its pages
        (true (tuatara.frontend.fake:fake-frame-showing f "PowerPoint presentation (OOXML), 1 page"))
        (true (last-images f))))
    (rimu-forget)))

(define-test (office-view office-older-pages)
  ;; a Word document of before 2007, test/office-view/survey.doc (saved by LibreOffice from the test's
  ;; .docx): its pages, T the text of its PDF, there being no reading of its own
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "survey.doc" dir))))
      (uiop:copy-file (asdf:system-relative-pathname :tuatara "test/office-view/survey.doc") path)
      (is eq :doc (getf (tuatara.defaults::office-file-format path) :kind))
      (if (not (office-pages-made-p path))
          (skip "LibreOffice or the sandbox missing here" (true nil))
          (progn
            (pdf-forget)
            (multiple-value-bind (f editor) (docx-session path '("T") :cell-pixels '(10 20))
              (true (tuatara.frontend.fake:fake-frame-showing f "Word document of before 2007, its pages drawn by LibreOffice"))
              (true (last-images f))
              (true (tuatara.frontend.fake:fake-frame-showing f "Text, page 1"))
              (is string= "survey.doc" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
            (pdf-forget)
            ;; in rimu's preview: its first page over its octets
            (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "survey.doc")) :preview t :width 250 :cell-pixels '(10 20))))
              (true (tuatara.frontend.fake:fake-frame-showing f "Microsoft compound file (an Office document of before 2007, a mail, an installer), 1 page"))
              (true (last-images f)))
            (rimu-forget))))))

;;; OpenDocument files

(defparameter *odf-test-namespaces*
  "xmlns:office='urn:oasis:names:tc:opendocument:xmlns:office:1.0' xmlns:text='urn:oasis:names:tc:opendocument:xmlns:text:1.0' xmlns:table='urn:oasis:names:tc:opendocument:xmlns:table:1.0' xmlns:draw='urn:oasis:names:tc:opendocument:xmlns:drawing:1.0' xmlns:style='urn:oasis:names:tc:opendocument:xmlns:style:1.0' xmlns:fo='urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:meta='urn:oasis:names:tc:opendocument:xmlns:meta:1.0' xmlns:svg='urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0' xmlns:presentation='urn:oasis:names:tc:opendocument:xmlns:presentation:1.0' xmlns:script='urn:oasis:names:tc:opendocument:xmlns:script:1.0' office:version='1.3'"
  "The namespaces an OpenDocument file made for the tests declares.")

(defun odf-test-parts (kind body &key (styles "") (automatic "") manifest)
  "The parts of a small OpenDocument file of KIND (text, spreadsheet,
presentation) made for the tests: BODY its office:body's contents, AUTOMATIC
its automatic styles, STYLES what styles.xml holds within it, MANIFEST the
manifest's entries for its text when given."
  (list (cons "mimetype" (format nil "application/vnd.oasis.opendocument.~a" kind))
        (cons "META-INF/manifest.xml"
              (format nil "<?xml version='1.0' encoding='UTF-8'?><manifest:manifest xmlns:manifest='urn:oasis:names:tc:opendocument:xmlns:manifest:1.0'><manifest:file-entry manifest:full-path='/' manifest:media-type='application/vnd.oasis.opendocument.~a'/>~a</manifest:manifest>"
                      kind (or manifest "<manifest:file-entry manifest:full-path='content.xml' manifest:media-type='text/xml'/>")))
        (cons "meta.xml"
              (format nil "<?xml version='1.0' encoding='UTF-8'?><office:document-meta ~a><office:meta><meta:generator>Some Office Suite/7.6</meta:generator><dc:title>Kea report</dc:title><meta:initial-creator>Ana Timaru</meta:initial-creator><dc:creator>Bo Westland</dc:creator><meta:creation-date>2026-01-01T09:00:00</meta:creation-date><meta:editing-duration>PT1H30M</meta:editing-duration><meta:editing-cycles>4</meta:editing-cycles><meta:document-statistic meta:page-count='2' meta:word-count='40'/><meta:user-defined meta:name='Case'>K-12</meta:user-defined></office:meta></office:document-meta>"
                      *odf-test-namespaces*))
        (cons "styles.xml" (format nil "<?xml version='1.0' encoding='UTF-8'?><office:document-styles ~a>~a</office:document-styles>" *odf-test-namespaces* styles))
        (cons "content.xml"
              (format nil "<?xml version='1.0' encoding='UTF-8'?><office:document-content ~a><office:automatic-styles>~a</office:automatic-styles><office:body><office:~a>~a</office:~a></office:body></office:document-content>"
                      *odf-test-namespaces* automatic kind body kind))
        (cons "Basic/Standard/Module1.xml" "Sub Main : Shell(\"rm -rf /home\") : End Sub")))

(defparameter *odt-test-parts*
  (odf-test-parts
   "text"
   (concatenate 'string
                "<text:tracked-changes><text:changed-region text:id='ct1'><text:deletion><office:change-info><dc:creator>Ana</dc:creator><dc:date>2026-01-02T10:00:00</dc:date></office:change-info><text:p>old count</text:p></text:deletion></text:changed-region><text:changed-region text:id='ct2'><text:insertion><office:change-info><dc:creator>Bo</dc:creator><dc:date>2026-01-03T10:00:00</dc:date></office:change-info></text:insertion></text:changed-region></text:tracked-changes>"
                "<text:p text:style-name='P1'>Kea report</text:p>"
                "<text:h text:outline-level='1'>Findings</text:h>"
                "<text:p>Twelve <text:span text:style-name='T1'>kea</text:span> seen<text:note text:note-class='footnote'><text:note-citation>1</text:note-citation><text:note-body><text:p>At dawn.</text:p></text:note-body></text:note>, see <text:a xlink:href='https://example.org/kea'>the count</text:a>.<text:s text:c='2'/>Was <text:change text:change-id='ct1'/><text:change-start text:change-id='ct2'/>new count<text:change-end text:change-id='ct2'/>.</text:p>"
                "<text:p>* Forged heading</text:p>"
                "<text:p>#+include: \"/etc/passwd\"</text:p>"
                "<text:p><text:span text:style-name='T3'>secret words</text:span> shown<office:annotation><dc:creator>Cy</dc:creator><dc:date>2026-01-04</dc:date><text:p>Check this</text:p></office:annotation></text:p>"
                "<text:list text:style-name='L1'><text:list-item><text:p>First</text:p><text:list><text:list-item><text:p>Inner</text:p></text:list-item></text:list></text:list-item><text:list-item><text:p>Second</text:p></text:list-item></text:list>"
                "<table:table table:name='Counts'><table:table-column table:number-columns-repeated='2'/><table:table-row><table:table-cell><text:p>Bird</text:p></table:table-cell><table:table-cell><text:p>Count</text:p></table:table-cell></table:table-row><table:table-row><table:table-cell><text:p>Kea | alpine</text:p></table:table-cell><table:table-cell><text:p>12</text:p></table:table-cell></table:table-row></table:table>"
                "<text:p><draw:frame draw:name='Image1'><draw:image xlink:href='Pictures/kea.png'/><svg:desc>A kea on a rock</svg:desc></draw:frame></text:p>"
                "<text:p>Costs $5, \\alert{x} and [[file:/etc/passwd]]</text:p>"
                "<text:h text:outline-level='2'>Endnotes</text:h><text:p>Later<text:note text:note-class='endnote'><text:note-citation>i</text:note-citation><text:note-body><text:p>An endnote.</text:p></text:note-body></text:note></text:p>")
   :automatic "<style:style style:name='P1' style:family='paragraph' style:parent-style-name='Title'/><style:style style:name='T1' style:family='text'><style:text-properties fo:font-weight='bold'/></style:style><style:style style:name='T3' style:family='text'><style:text-properties text:display='none'/></style:style><text:list-style style:name='L1'><text:list-level-style-number text:level='1'/><text:list-level-style-bullet text:level='2'/></text:list-style>"
   :styles "<office:styles><style:style style:name='Title' style:family='paragraph'/></office:styles><office:master-styles><style:master-page style:name='Standard'><style:header><text:p>Field notes</text:p></style:header><style:footer><text:p>Page <text:page-number>1</text:page-number></text:p></style:footer></style:master-page></office:master-styles>")
  "The parts of a small OpenDocument text made for the tests.")

(defparameter *ods-test-parts*
  (odf-test-parts
   "spreadsheet"
   (concatenate 'string
                "<table:table table:name='Birds'><table:table-column/><table:table-column table:visibility='collapse'/><table:table-column table:number-columns-repeated='16382'/>"
                "<table:table-row><table:table-cell office:value-type='string'><text:p>Bird</text:p></table:table-cell><table:table-cell office:value-type='string'><text:p>Hidden</text:p></table:table-cell><table:table-cell office:value-type='string'><text:p>Count</text:p></table:table-cell><table:table-cell table:number-columns-repeated='16381'/></table:table-row>"
                "<table:table-row><table:table-cell office:value-type='string'><text:p>Kea | alpine</text:p><office:annotation><dc:creator>Cy</dc:creator><text:p>Counted twice</text:p></office:annotation></table:table-cell><table:table-cell/><table:table-cell table:formula='of:=[.D2]*2' office:value-type='float' office:value='24'><text:p>24</text:p></table:table-cell><table:table-cell office:value-type='date' office:date-value='2024-03-01'><text:p>01/03/24</text:p></table:table-cell></table:table-row>"
                "<table:table-row table:visibility='collapse'><table:table-cell office:value-type='string'><text:p>Kaka</text:p></table:table-cell></table:table-row>"
                "<table:table-row table:number-rows-repeated='1048573'><table:table-cell table:number-columns-repeated='16384'/></table:table-row></table:table>"
                "<table:table table:name='Secret' table:style-name='ta2'><table:table-row><table:table-cell office:value-type='string'><text:p>hidden words</text:p></table:table-cell></table:table-row></table:table>"
                "<table:database-ranges><table:database-range table:name='Imported'><table:database-source-sql table:database-name='Birdbase' table:sql-statement='SELECT *'/></table:database-range></table:database-ranges>")
   :automatic "<style:style style:name='ta2' style:family='table'><style:table-properties table:display='false'/></style:style>")
  "The parts of a small OpenDocument spreadsheet made for the tests.")

(defparameter *odp-test-parts*
  (odf-test-parts
   "presentation"
   (concatenate 'string
                "<draw:page draw:name='page1'><draw:frame presentation:class='title'><draw:text-box><text:p>Kea survey</text:p></draw:text-box></draw:frame>"
                "<draw:frame presentation:class='outline'><draw:text-box><text:list><text:list-item><text:p>Counted twice</text:p><text:list><text:list-item><text:p>At dawn</text:p></text:list-item></text:list></text:list-item></text:list><text:p>* Forged heading</text:p></draw:text-box></draw:frame>"
                "<draw:frame><table:table><table:table-row><table:table-cell><text:p>Bird</text:p></table:table-cell><table:table-cell><text:p>Count</text:p></table:table-cell></table:table-row><table:table-row><table:table-cell><text:p>Kea</text:p></table:table-cell><table:table-cell><text:p>12</text:p></table:table-cell></table:table-row></table:table><draw:image xlink:href='Pictures/TablePreview1.svm'/></draw:frame>"
                "<draw:frame draw:name='Picture 5'><draw:image xlink:href='Pictures/kea.png'/><svg:desc>A kea on a rock</svg:desc></draw:frame>"
                "<presentation:notes><draw:page-thumbnail/><draw:frame presentation:class='notes'><draw:text-box><text:p>Say the count slowly.</text:p></draw:text-box></draw:frame></presentation:notes></draw:page>"
                "<draw:page draw:name='page2' draw:style-name='dp2'><draw:frame presentation:class='title'><draw:text-box><text:p>Hidden findings</text:p></draw:text-box></draw:frame><draw:frame presentation:class='outline'><draw:text-box><text:p>Not for the meeting</text:p></draw:text-box></draw:frame></draw:page>")
   :automatic "<style:style style:name='dp2' style:family='drawing-page'><style:drawing-page-properties presentation:visibility='hidden'/></style:style>")
  "The parts of a small OpenDocument presentation made for the tests.")

(define-test (office-view odf-text-read)
  (with-test-directory (dir)
    (let* ((path (write-office-test-file (merge-pathnames "kea.odt" dir) *odt-test-parts*))
           (note (tuatara.defaults::odf-read-note path)))
      (note-has note "#+title: kea.odt")
      (note-has note "* About this document")
      (note-has note "- Title :: Kea report")
      (note-has note "- Made by :: Ana Timaru")
      (note-has note "- Last changed by :: Bo Westland")
      (note-has note "- Program :: Some Office Suite/7.6")
      (note-has note "- Editing time :: 90 minutes")
      (note-has note "- Pages, words :: 2 pages, 40 words")
      (note-has note "- Field Case :: K-12")
      (note-has note "- Macros :: yes (Basic/Standard/Module1.xml), never run")
      (note-has note "- Links outside it :: https://example.org/kea (none followed)")
      (note-has note "- Tracked changes :: 2 (listed at the end)")
      (note-has note "- Header (page style Standard) :: Field notes")
      (note-has note "- Footer (page style Standard) :: Page 1")
      ;; a paragraph of a style based on Title is the title; headings under the text's own
      (note-has note "#+title: Kea report")
      (note-has note "** Findings")
      (note-has note "*** Endnotes")
      (note-has note "Twelve *kea* seen[fn:1], see [[https://example.org/kea][the count]].  Was +old count+new count.")
      ;; what a document says stays its words: never a heading, a keyword, markup or a link of the note
      (note-has note (format nil "~a* Forged heading" tuatara.defaults::*docx-read-zero-width-space*))
      (false (cl-ppcre:scan "(?m)^\\* Forged" note))
      (false (cl-ppcre:scan "(?m)^#\\+include" note))
      (note-has note "Costs \\dollar{}5")
      (false (search "[[file:/etc/passwd]]" note))
      (note-has note "[hidden: secret words] shown[fn:c1]")
      (note-has note (format nil "1. First~%  - Inner~%2. Second~%"))
      (note-has note (format nil "| Bird | Count |~%|-~%| Kea \\vert{} alpine | 12 |~%"))
      (note-has note "[picture Pictures/kea.png: A kea on a rock]")
      (note-has note "* Tracked changes")
      (note-has note "- Deleted by Ana, 2026-01-02T10:00:00 :: old count")
      (note-has note "- Inserted by Bo, 2026-01-03T10:00:00 :: new count")
      (note-has note "[fn:1] At dawn.")
      (note-has note "[fn:e1] An endnote.")
      (note-has note "[fn:c1] Comment by Cy, 2026-01-04: Check this"))))

(define-test (office-view odf-spreadsheet-read)
  (with-test-directory (dir)
    (let* ((path (write-office-test-file (merge-pathnames "birds.ods" dir) *ods-test-parts*))
           (note (tuatara.defaults::odf-read-note path)))
      (note-has note "* About this spreadsheet")
      (note-has note "- Sheets :: 2, 1 hidden")
      (note-has note "- Data connections :: Birdbase (none made)")
      (note-has note "- Values :: as last saved with the file; no formula is worked out here")
      (note-has note "* Sheet 1: Birds")
      ;; the values as the file shows them; a hidden column and row marked; the empty rows repeated to the
      ;; sheet's end never walked one by one
      (note-has note (format nil "|  | A | B (hidden) | C | D |~%|-~%| 1 | Bird | Hidden | Count |  |~%| 2 | Kea \\vert{} alpine |  | 24 | 01/03/24 |~%| 3 (hidden) | Kaka |  |  |  |~%"))
      (note-has note "- C2 :: of:=[.D2]*2")
      (note-has note "Comments on its cells:")
      (note-has note "- A2 :: by Cy: Counted twice")
      (note-has note "* Sheet 2: Secret (hidden)")
      (note-has note "| 1 | hidden words |")
      ;; the cells read held to the limit, where it stopped said
      (let ((note (let ((tuatara.defaults::*sheet-most-cells* 3)) (tuatara.defaults::odf-read-note path))))
        (note-has note "Only the first 3 cells are read: from A2 on")
        (note-has note "Not read: the first 3 cells were (*sheet-most-cells*).")
        (false (search "hidden words" note))))))

(define-test (office-view odf-presentation-read)
  (with-test-directory (dir)
    (let* ((path (write-office-test-file (merge-pathnames "kea.odp" dir) *odp-test-parts*))
           (note (tuatara.defaults::odf-read-note path)))
      (note-has note "* About this presentation")
      (note-has note "- Slides :: 2, 1 hidden")
      (note-has note (format nil "* Slide 1: Kea survey~%- Counted twice~%  - At dawn~%- * Forged heading~%| Bird | Count |~%|-~%| Kea | 12 |~%~%- [picture Pictures/kea.png: A kea on a rock]~%"))
      (false (cl-ppcre:scan "(?m)^\\* Forged" note))
      (note-has note (format nil "** Speaker notes~%- Say the count slowly.~%"))
      (note-has note (format nil "* Slide 2 (hidden): Hidden findings~%- Not for the meeting~%")))))

(define-test (office-view odf-refused)
  ;; locked with a password, not what it says, or made to exhaust the reader: refused with why, never hanging
  (with-test-directory (dir)
    (let ((locked (write-office-test-file (merge-pathnames "locked.odt" dir)
                                          (odf-test-parts "text" "<text:p>scrambled</text:p>"
                                                          :manifest "<manifest:file-entry manifest:full-path='content.xml' manifest:media-type='text/xml'><manifest:encryption-data manifest:checksum-type='SHA1/1K'/></manifest:file-entry>")))
          (empty (write-office-test-file (merge-pathnames "empty.odt" dir)
                                         (remove "content.xml" (odf-test-parts "text" "") :key #'car :test #'string=)))
          (deep (write-office-test-file (merge-pathnames "deep.odt" dir)
                                        (odf-test-parts "text" (format nil "~{~a~}<text:p>deep</text:p>" (make-list 2000 :initial-element "<text:section>"))))))
      (note-has (handler-case (progn (tuatara.defaults::odf-read-note locked) "") (error (e) (princ-to-string e))) "locked with a password")
      (note-has (handler-case (progn (tuatara.defaults::odf-read-note empty) "") (error (e) (princ-to-string e))) "holds no OpenDocument")
      (note-has (handler-case (progn (tuatara.defaults::odf-read-note deep) "") (error (e) (princ-to-string e))) "nested deeper")
      (let ((tuatara.defaults::*docx-largest-xml* 300))
        (fail (tuatara.defaults::odf-read-note (write-office-test-file (merge-pathnames "kea.odt" dir) *odt-test-parts*)))))))

(define-test (office-view odf-opened)
  ;; told by contents, read as text where pages are not drawn, in rimu's preview too, opened outside after asking
  (with-test-directory (dir)
    (let ((odt (write-office-test-file (merge-pathnames "kea.odt" dir) *odt-test-parts*))
          (ods (write-office-test-file (merge-pathnames "birds.ods" dir) *ods-test-parts*))
          (odp (write-office-test-file (merge-pathnames "kea.odp" dir) *odp-test-parts*)))
      (is eq :odt (getf (tuatara.defaults::office-file-format odt) :kind))
      (is eq :ods (getf (tuatara.defaults::office-preview-format :ods "anything.bin") :kind))
      (false (tuatara.defaults::open-externally-refusal odt))
      (let ((tuatara.defaults::*office-view-pages* nil))
        (dolist (case (list (list odt "kea.odt (text)" "** Findings") (list ods "birds.ods (text)" "* Sheet 1: Birds")
                            (list odp "kea.odp (text)" "* Slide 1: Kea survey")))
          (destructuring-bind (path name text) case
            (multiple-value-bind (f editor) (docx-session path '())
              (declare (ignore f))
              (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                (is string= name (tuatara.buffer:buffer-name buffer))
                (true (tuatara.buffer:buffer-local buffer :read-only))
                (true (search text (tuatara.buffer:buffer-string buffer)))
                (tuatara.buffer:kill-buffer buffer))))))))
  (with-rimu-tree (dir)
    (write-office-test-file (merge-pathnames "kea.odt" dir) *odt-test-parts*)
    (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "kea.odt")) :preview t :width 250)))
      (true (tuatara.frontend.fake:fake-frame-showing f "OpenDocument text"))
      (true (tuatara.frontend.fake:fake-frame-showing f "About this document")))
    (rimu-forget)))

(defun replaced-parts (parts &rest replacements)
  "PARTS, each (NAME . TEXT), with those REPLACEMENTS name, a plist of NAME and TEXT, put in their place."
  (mapcar (lambda (part)
            (let ((new (getf replacements (intern (car part) :keyword) part)))
              (if (eq new part) part (cons (car part) new))))
          parts))

(define-test (office-view hostile-amplification)
  ;; a few octets standing for text copied many times: each value held, the note held, the reading
  ;; ended when asked, a slide named again said to be one met before
  (with-test-directory (dir)
    (let* ((long (make-string 5000 :initial-element #\k))
           (xlsx (write-office-test-file
                  (merge-pathnames "copies.xlsx" dir)
                  (replaced-parts *xlsx-test-parts*
                                  :|xl/sharedStrings.xml| (format nil "<sst xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><si><t>~a</t></si></sst>" long)
                                  :|xl/worksheets/sheet1.xml| (format nil "<worksheet xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main'><sheetData>~{<row r='~d'><c r='A~:*~d' t='s'><v>0</v></c></row>~}</sheetData></worksheet>"
                                                                      (loop for i from 1 to 200 collect i))))))
      (let* ((tuatara.defaults::*office-value-longest* 100)
             (note (tuatara.defaults::xlsx-read-note xlsx)))
        (false (search (make-string 101 :initial-element #\k) note))
        (true (< (length note) 50000)))
      (let ((note (let ((tuatara.defaults::*office-note-largest* 3000)) (tuatara.defaults::xlsx-read-note xlsx))))
        (note-has note "The rest is left out: the note reached 3,000 characters")
        (true (< (length note) 20000)))
      (fail (tuatara.defaults::xlsx-read-note xlsx :cancel (list t)) 'tuatara.runtime:job-stopped)
      (let* ((pptx (write-office-test-file
                    (merge-pathnames "again.pptx" dir)
                    (replaced-parts *pptx-test-parts*
                                    :|ppt/presentation.xml| (format nil "<p:presentation xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships' xmlns:p='http://schemas.openxmlformats.org/presentationml/2006/main'><p:sldIdLst>~{<p:sldId id='~d' r:id='rId2'/>~}</p:sldIdLst></p:presentation>"
                                                                    (loop for i from 256 below 456 collect i)))))
             (note (tuatara.defaults::pptx-read-note pptx)))
        (note-has note "* Slide 1: Kea survey")
        (note-has note "* Slide 200: slide 1 again (ppt/slides/slide1.xml)")
        (is = 1 (tuatara.defaults::search-count "Counted twice" note)))
      (let* ((ods (write-office-test-file
                   (merge-pathnames "repeated.ods" dir)
                   (odf-test-parts "spreadsheet"
                                   (format nil "<table:table table:name='Copies'><table:table-row table:number-rows-repeated='1000'><table:table-cell table:number-columns-repeated='16384' office:value-type='string'><text:p>~a</text:p></table:table-cell></table:table-row></table:table>" long))))
             (note (let ((tuatara.defaults::*office-note-largest* 100000)) (tuatara.defaults::odf-read-note ods))))
        (note-has note "The rest is left out: the note reached 100,000 characters")
        (true (< (length note) 150000)))
      ;; an insertion begun and never ended: its text in the list of changes held to a value's length
      (let* ((odt (write-office-test-file
                   (merge-pathnames "open.odt" dir)
                   (odf-test-parts "text"
                                   (format nil "<text:tracked-changes><text:changed-region text:id='c1'><text:insertion><office:change-info><dc:creator>Bo</dc:creator></office:change-info></text:insertion></text:changed-region></text:tracked-changes><text:p>Was <text:change-start text:change-id='c1'/>new</text:p>~{<text:p>~a</text:p>~}"
                                           (make-list 50 :initial-element "more words after the change began")))))
             (note (let ((tuatara.defaults::*office-value-longest* 40)) (tuatara.defaults::odf-read-note odt))))
        (note-has note "- Inserted by Bo :: newmore words after the change began")
        (false (cl-ppcre:scan "Inserted by Bo :: [^\\n]{60}" note))))))

(define-test (office-view office-command-takes-no-text-in)
  ;; LibreOffice is started with its name and the document's ending given as arguments,
  ;; never written into the shell's command, whatever the setting holds
  (let ((seen nil)
        (tuatara.defaults::*office-page-program* "soffice; touch /tmp/never"))
    (sb-int:encapsulate 'tuatara.io:sandbox-run 'office-command-seen
                        (lambda (real program arguments &rest keys)
                          (declare (ignore real keys))
                          (setf seen (cons program arguments))
                          (values (make-array 0 :element-type '(unsigned-byte 8)) :done nil)))
    (unwind-protect
         (ignore-errors (tuatara.defaults::office-pdf-copy 0 "docx"))
      (sb-int:unencapsulate 'tuatara.io:sandbox-run 'office-command-seen))
    (is string= "sh" (first seen))
    (false (search "soffice" (third seen)))
    (false (search "docx" (third seen)))
    (is equal '("sh" "soffice; touch /tmp/never" "docx") (last seen 3))))

(define-test (office-view empty-cells-and-spaces-take-room)
  ;; cells repeated or covered, and runs of spaces, take room in the note as they are given
  (with-test-directory (dir)
    (let* ((row "<table:table-row table:number-rows-repeated='64'><table:table-cell><text:p>x</text:p></table:table-cell><table:covered-table-cell table:number-columns-repeated='64'/></table:table-row>")
           (odt (write-office-test-file
                 (merge-pathnames "covered.odt" dir)
                 (odf-test-parts "text" (format nil "<table:table table:name='Wide'>~{~a~}</table:table>" (make-list 300 :initial-element row)))))
           (note (let ((tuatara.defaults::*office-note-largest* 100000)) (tuatara.defaults::odf-read-note odt))))
      (note-has note "The rest is left out: the note reached 100,000 characters")
      (true (< (length note) 300000) "~:d characters" (length note)))
    (let* ((odt (write-office-test-file
                 (merge-pathnames "spaces.odt" dir)
                 (odf-test-parts "text" (format nil "<text:p>a~{~a~}b</text:p>" (make-list 4000 :initial-element "<text:s text:c='1000'/>")))))
           (before (sb-ext:get-bytes-consed))
           (note (let ((tuatara.defaults::*office-note-largest* 100000)) (tuatara.defaults::odf-read-note odt)))
           (consed (- (sb-ext:get-bytes-consed) before)))
      (note-has note "The rest is left out: the note reached 100,000 characters")
      (true (< consed (* 8 1024 1024)) "~:d octets" consed))))
