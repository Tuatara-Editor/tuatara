;;;; lisp/org/docx-templates.lisp --- Word templates: a Word document of one's own, its places filled by a note.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A Word template is the Word half of a template whose LaTeX half makes the
;;;; PDF: the note's #+template: NAME names both. It is a Word document
;;;; unpacked, its parts as files in a folder, ~/.config/tuatara/docx-templates/NAME/
;;;; (word/document.xml, word/styles.xml, its header, footer and pictures), so
;;;; that it can be kept as text and written from an Org note's blocks. Its
;;;; text has places as a LaTeX template has: {{{KEY}}} for the value of the
;;;; note's #+KEY: line, {{{KEY:raw}}} as written, {{{body}}} alone in a
;;;; paragraph for the note's text, {{{contents}}} alone in one for its
;;;; contents, {{{KEY:path}}} alone in one for a picture,
;;;; and {{{#KEY}}}...{{{/KEY}}} or {{{^KEY}}}...{{{/KEY}}} around what is kept
;;;; only when KEY has a value or has none: within a paragraph, around
;;;; paragraphs, or anywhere in a table's row for the whole row. A place Word
;;;; has split among runs of different looks is found all the same. The
;;;; template's styles, page, header and footer are kept; Tuatara adds the
;;;; styles, lists, footnotes and pictures the note needs.

(in-package #:tuatara.defaults)

(defun docx-templates-directory ()
  "Where your Word templates are: docx-templates/ in the configuration
directory (~/.config/tuatara/docx-templates/), each a folder NAME/."
  (config-path "docx-templates/"))

(defun docx-template-folder (value directory)
  "The folder of the Word template VALUE names, what a note's #+template:
says, as a native name ending in /, or NIL when there is none: a name's
folder among yours (DOCX-TEMPLATES-DIRECTORY), never . or .., or for a path
\(./report.tex, ~/Templates/report) the folder of that name beside it,
without .tex, DIRECTORY the note's own, where TEMPLATE-PATH-ALLOWED-P lets it
be."
  (let ((folder (if (latex-template-path-p value)
                    (let ((path (org-export-path value directory)))
                      (concatenate 'string (if (uiop:string-suffix-p (string-downcase path) ".tex") (subseq path 0 (- (length path) 4)) path) "/"))
                    (concatenate 'string (docx-templates-directory) value "/"))))
    (and (not (member value '("." "..") :test #'string=))
         (eq :regular (tuatara.io:path-kind (concatenate 'string folder "word/document.xml")))
         (or (not (latex-template-path-p value)) (template-path-allowed-p folder directory))
         folder)))

(defun docx-part-text (parts name)
  "The part NAME of PARTS as text (UTF-8, an octet that is not UTF-8 kept as
its raw byte's character, so that it is written back as it was), or NIL."
  (let ((part (assoc name parts :test #'string=)))
    (and part (tuatara.text:octets-string (coerce (cdr part) 'tuatara.text:octets)))))

;;; The text of runs

(defun docx-xml-text (xml)
  "The text of the runs in XML, the w:t elements' joined."
  (with-output-to-string (s)
    (cl-ppcre:do-register-groups (text) ("<w:t(?:\\s[^>]*)?>([^<]*)</w:t>" xml)
      (write-string (xml-text-value text) s))))

;;; The values of places

(defvar *docx-date-format* nil
  "The date format of the content control whose place is being filled (Word's,
d MMMM yyyy), or NIL: a date given for it is written so.")

(defun docx-field (name way &key height width)
  "What the place NAME (read WAY, :TEXT, :RAW or :PATH) of a Word template
stands for in the note being written, as runs; NIL when it has no value. A
picture is as HEIGHT or WIDTH (twentieths of a point) say, when given."
  (let* ((key (string-upcase name))
         (values (cond ((string= name "title") (list (org-latex-title)))
                       ((string= name "author") (let ((author (org-latex-author))) (and author (list author))))
                       ((string= name "date") (and (org-latex-option "date" t) (org-latex-keyword-all "DATE")))
                       ((string= name "created") (list (org-latex-created)))
                       (t (org-latex-keyword-all key))))
         (values (remove-if (lambda (v) (or (null v) (zerop (length (string-trim " 	" v))))) values))
         ;; a date control's place: a date given written as the control asks
         (values (if *docx-date-format*
                     (mapcar (lambda (v) (multiple-value-bind (y m d) (docx-date-parts v)
                                           (if y (docx-format-date *docx-date-format* y m d) v)))
                             values)
                     values)))
    (org-refuse-hidden-as-written name way values)
    ;; a picture the note names from where it may not come: its place and its conditions empty
    (when (member name (org-latex-refused *org-latex*) :test #'string=)
      (setf values '()))
    (when values
      (ecase way
        (:text (format nil "~{~a~^<w:r><w:br/></w:r>~}" (mapcar (lambda (v) (docx-string (string-trim " 	" v))) values)))
        (:raw (format nil "~{~a~^<w:r><w:br/></w:r>~}" (mapcar (lambda (v) (docx-run (string-trim " 	" v))) values)))
        (:path (let* ((path (template-picture-path (first values)))
                      (written (and path (format nil "[[file:~a]]" path)))
                      (link (and written
                                 (first (org-node-children
                                         (let ((tree (org-parse written)) (found nil))
                                           (org-walk-tree tree (lambda (n) (when (and (null found) (eq (org-node-type n) 'paragraph)) (setf found n))))
                                           found))))))
                 (and link (let ((*org-latex-text* written))
                             (docx-picture link '() :height height :width width :asked t)))))))))

(defparameter *template-place-re*
  "\\{\\{\\{([#^/])?([A-Za-z][-\\w]*)(?::(text|raw|path|date=([^}]*)))?(?::(height|width)=([^}]*))?\\}\\}\\}"
  "A place of a template, as both halves read it: {{{NAME}}}, {{{NAME:WAY}}}
(text, raw, path), a date's {{{NAME:date=d MMMM yyyy}}}, a picture's
{{{NAME:path:height=1.8cm}}} (or width=), and the marks of a condition,
{{{#NAME}}}, {{{^NAME}}} and {{{/NAME}}}.")

(defun template-refused-pictures (text)
  "The names of the pictures TEXT, a template's, asks the note for
\({{{NAME:path}}}) whose files the note names from where they may not come
\(TEMPLATE-PICTURE-PATH, which says so after the export): their places, and
the conditions on them, are filled as if the note gave none."
  (let ((names '()))
    (cl-ppcre:do-scans (start end group-starts group-ends *template-place-re* text)
      (when (and (aref group-starts 2) (string-equal "path" (subseq text (aref group-starts 2) (aref group-ends 2))))
        (pushnew (string-downcase (subseq text (aref group-starts 1) (aref group-ends 1))) names :test #'string=)))
    (remove-if (lambda (name)
                 (let ((value (org-latex-keyword (string-upcase name))))
                   ;; one holding a macro is refused whole, as hidden text where it cannot be hidden
                   (or (null value) (search "{{{" value) (template-picture-path value))))
               names)))

(defun template-place (text &key whole)
  "The place TEXT begins with (all of TEXT with WHOLE), as *TEMPLATE-PLACE-RE*
reads it: a plist of :NAME (lower case), :WAY (:TEXT, :RAW or :PATH), :DATE
(the pattern), :SIZE ((:HEIGHT LENGTH) or (:WIDTH LENGTH), as written),
:CONDITION (#\\# #\\^ or #\\/) and :END (where it ends in TEXT); NIL for no
such place."
  (multiple-value-bind (start end groups-start groups-end)
      (cl-ppcre:scan *template-place-re* text)
    (when (and start (zerop start) (or (not whole) (= end (length text))))
      (flet ((group (i) (and (aref groups-start i) (subseq text (aref groups-start i) (aref groups-end i)))))
        (let ((way (group 2)))
          (list :name (string-downcase (group 1))
                :condition (and (group 0) (char (group 0) 0))
                :way (cond ((equal way "raw") :raw) ((equal way "path") :path) (t :text))
                :date (group 3)
                :size (and (group 4) (list (if (string= (group 4) "height") :height :width) (group 5)))
                :end end))))))

(defun docx-place (text)
  "The place TEXT is (TEMPLATE-PLACE), not a condition's mark, as NAME, WAY
\(:TEXT, :RAW or :PATH), as a third value the picture's size, (:HEIGHT
TWIPS) or (:WIDTH TWIPS), and as a fourth the date's pattern; NIL for one
that is no such place."
  (let ((place (template-place text :whole t)))
    (and place (null (getf place :condition))
         (values (getf place :name) (getf place :way)
                 (let ((size (getf place :size)))
                   (and size (list (first size) (docx-length (second size)))))
                 (getf place :date)))))

(defun docx-has-value-p (name)
  "True when the note being written gives the place NAME a value (asked as
text: nothing printed, so a hidden name in it is no matter)."
  (and (docx-field name :text) t))

;;; Filling a paragraph whose places Word may have split among runs

(defun docx-run-items (paragraph)
  "The parts of PARAGRAPH's XML after its properties, each (KIND XML TEXT
PROPERTIES): :TEXT for a run of text alone (its text, its w:rPr), :OTHER for
anything else (a bookmark, a field, a picture), its text empty; as a second
value the paragraph's opening tag and properties, as a third its closing tag."
  (let* ((open-end (1+ (position #\> paragraph)))
         (close (- (length paragraph) (length "</w:p>")))
         (ppr (xml-element paragraph "w:pPr" :start open-end :end close))
         (start (if ppr (cdr ppr) open-end))
         (items (loop for (name . element) in (xml-top-elements paragraph start close)
                      collect (let* ((xml (subseq paragraph (car element) (cdr element)))
                                     (rpr (let ((r (xml-element xml "w:rPr"))) (and r (subseq xml (car r) (cdr r)))))
                                     (inner (and (string= name "w:r") (cl-ppcre:regex-replace-all "<w:rPr>.*?</w:rPr>|<w:rPr/>|<w:lastRenderedPageBreak/>" xml ""))))
                                (if (and inner (cl-ppcre:scan "^<w:r(?:\\s[^>]*)?>(?:<w:t(?:\\s[^>]*)?>[^<]*</w:t>|<w:t/>)*</w:r>$" inner))
                                    (list :text xml (docx-xml-text xml) rpr)
                                    (list :other xml "" nil))))))
    (values items (subseq paragraph 0 start) "</w:p>")))

(defun docx-with-properties (runs properties)
  "RUNS, as DOCX-RUN writes them, each given PROPERTIES (a run's w:rPr from a
template) when it has none of its own."
  (if (null properties)
      runs
      (cl-ppcre:regex-replace-all "<w:r>(?!<w:rPr>)" runs (format nil "<w:r>~a" properties))))

(defun docx-fill-text (text properties)
  "TEXT of a template, its places and the conditions within it filled, as runs
looking as PROPERTIES says."
  (with-output-to-string (out)
    (let ((p 0))
      (loop
        (let ((at (search "{{{" text :start2 p)))
          (write-string (docx-with-properties (docx-run (subseq text p (or at (length text)))) properties) out)
          (unless at (return))
          (let ((close (search "}}}" text :start2 at)))
            (unless close (write-string (docx-with-properties (docx-run (subseq text at)) properties) out) (return))
            (let ((token (subseq text at (+ close 3))))
              (let ((place (template-place token :whole t)))
                (if (member (getf place :condition) '(#\# #\^))
                    ;; a condition: what is up to its end, kept or not
                    (let* ((name (getf place :name))
                           (end-token (format nil "{{{/~a}}}" name))
                           (end (search end-token text :start2 (+ close 3) :test #'char-equal)))
                      ;; one whose end is in another cell or paragraph, kept or not by its row or run
                      ;; of paragraphs already (DOCX-FILL-ROWS): the mark alone goes
                      (if (null end)
                          (setf p (+ close 3))
                          (progn
                            (when (eq (char= (getf place :condition) #\#) (docx-has-value-p name))
                              (write-string (docx-fill-text (subseq text (+ close 3) end) properties) out))
                            (setf p (+ end (length end-token))))))
                    (progn
                      (multiple-value-bind (name way size date) (docx-place token)
                        (when name
                          (let ((*docx-date-format* (or date *docx-date-format*)))
                            (write-string (docx-with-properties (or (apply #'docx-field name way size) "") properties) out))))
                      (setf p (+ close 3))))))))))))

(defun docx-date-parts (text)
  "The date TEXT gives, 2026-09-30 or <2026-09-30 Wed> or [2026-09-30 Wed 10:00],
as year, month and day; NIL when it gives none."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^\\s*[<\\[]?(\\d{4})-(\\d{1,2})-(\\d{1,2})" text)
    (and match (let ((y (parse-integer (aref groups 0))) (m (parse-integer (aref groups 1))) (d (parse-integer (aref groups 2))))
                 (and (valid-date-p y m d) (values y m d))))))

(defun word-date-pattern (format)
  "Word's date FORMAT (d, dd, ddd, dddd, M, MM, MMM, MMMM, yy, yyyy, other
characters as they are, 'quoted' text too) as a pattern of FORMAT-DAY's %
specifiers."
  (with-output-to-string (out)
    (let ((i 0) (n (length format)))
      (flet ((literal (text) (write-string (cl-ppcre:regex-replace-all "%" text "%%") out)))
        (loop while (< i n)
              do (let* ((c (char format i))
                        (run (or (position-if (lambda (x) (char/= x c)) format :start i) n))
                        (count (- run i)))
                   (cond ((char= c #\')
                          (let ((close (or (position #\' format :start (1+ i)) n)))
                            (literal (subseq format (1+ i) close))
                            (setf run (min n (1+ close)))))
                         ((char= c #\d) (write-string (case count (1 "%-d") (2 "%d") (3 "%a") (t "%A")) out))
                         ((char= c #\M) (write-string (case count (1 "%-m") (2 "%m") (3 "%b") (t "%B")) out))
                         ((char= c #\y) (write-string (if (<= count 2) "%y" "%Y") out))
                         (t (literal (subseq format i run))))
                   (setf i run)))))))

(defun docx-format-date (format year month day)
  "The date YEAR MONTH DAY written as Word's date FORMAT says (WORD-DATE-PATTERN)."
  (format-day (word-date-pattern format) (day-number year month day)))

(defun docx-fill-controls (paragraph)
  "PARAGRAPH with the places in its content controls (w:sdt, Word's fields)
filled, the controls kept: no longer showing their placeholder, a date
written in its control's format and the control set to it."
  (if (not (search "<w:sdt" paragraph))
      paragraph
      (xml-replace-elements
       paragraph "w:sdt"
       (lambda (xml)
         (let ((content (xml-element xml "w:sdtContent")))
           (if (or (null content) (not (search "{{{" xml :start2 (car content))))
               xml
               (let* ((properties (subseq xml 0 (car content)))
                      (inner (xml-element-inside xml content))
                      (*docx-date-format* (xml-value properties "w:dateFormat"))
                      (filled (let ((p (docx-fill-paragraph (concatenate 'string "<w:p>" inner "</w:p>"))))
                                (subseq p (length "<w:p>") (- (length p) (length "</w:p>"))))))
                 (format nil "~a<w:sdtContent>~a</w:sdtContent></w:sdt>"
                         (cl-ppcre:regex-replace-all "<w:showingPlcHdr/>" properties "") filled)))))
       :start (1+ (position #\> paragraph)))))

(defun docx-fill-paragraph (paragraph)
  "PARAGRAPH, a w:p of a template, its places filled: each place, wherever it
is split among runs, replaced by what it stands for in the look of the run it
begins in, those in its content controls too (DOCX-FILL-CONTROLS); a
condition's marker left alone in a paragraph (its other end elsewhere) taken
away."
  (if (uiop:string-suffix-p paragraph "/>")
      ;; an empty paragraph, <w:p/>, has nothing to fill
      paragraph
      (let ((paragraph (docx-fill-controls paragraph)))
        (multiple-value-bind (items head tail) (docx-run-items paragraph)
          (let ((text (format nil "~{~a~}" (mapcar #'third items))))
            (cond ((not (search "{{{" text)) paragraph)
                  ;; each place within one run: each run filled in its own look, the label's and the value's apart
                  ((every (lambda (item)
                            (let ((item-text (third item)))
                              (and (= (search-count "{{{" item-text) (search-count "}}}" item-text))
                                   (not (cl-ppcre:scan "\\{\\{\\{[#^/]" item-text)))))
                          items)
                   (with-output-to-string (out)
                     (write-string head out)
                     (dolist (item items)
                       (write-string (if (and (eq (first item) :text) (search "{{{" (third item)))
                                         (docx-fill-text (third item) (fourth item))
                                         (second item))
                                     out))
                     (write-string tail out)))
                  ;; else the runs of text, joined into one in the first one's look, the rest kept around them
                  (t
                   (let ((properties (fourth (find :text items :key #'first)))
                         (written nil))
                     (with-output-to-string (out)
                       (write-string head out)
                       (dolist (item items)
                         (if (eq (first item) :other)
                             (write-string (second item) out)
                             (unless written
                               (write-string (docx-fill-text text properties) out)
                               (setf written t))))
                       (write-string tail out))))))))))

;;; Filling a template's XML

(defparameter *docx-body-mark* (format nil "~c~cBODY~c" (code-char #xE000) (code-char #xE001) (code-char #xE000))
  "What stands for the note's text in a template's XML until the rest is filled.")

(defun docx-condition-of (text)
  "The condition TEXT, a paragraph's or a row's text, opens alone or holds
whole: (NAME KEEP-WHEN-VALUE) for {{{#NAME}}} or {{{^NAME}}}; NIL for none."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "\\{\\{\\{([#^])([A-Za-z][-\\w]*)\\}\\}\\}" text)
    (and match (list (string-downcase (aref groups 1)) (string= (aref groups 0) "#")))))

(defun docx-fill-rows (xml)
  "XML with each table row holding a condition, {{{#NAME}}} or {{{^NAME}}}
with its {{{/NAME}}}, kept when it holds or taken away."
  (xml-replace-elements
   xml "w:tr"
   (lambda (row-xml)
     (let* ((text (docx-xml-text row-xml))
            (condition (docx-condition-of text)))
       (and (or (null condition)
                (not (search (format nil "{{{/~a}}}" (first condition)) text :test #'char-equal))
                (eq (second condition) (docx-has-value-p (first condition))))
            row-xml)))))

(defun docx-fill-paragraph-blocks (xml)
  "XML with each run of paragraphs between a paragraph holding only
{{{#NAME}}} (or {{{^NAME}}}) and one holding only {{{/NAME}}} kept when
it holds or taken away, the two marks' paragraphs taken away."
  (let* ((paragraphs (xml-elements xml "w:p"))
         (texts (mapcar (lambda (p) (string-trim " 	" (docx-xml-text (subseq xml (car p) (cdr p))))) paragraphs))
         (drop '()))
    (loop for (p . rest) on paragraphs
          for text in texts
          for i from 0
          do (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^\\{\\{\\{([#^])([A-Za-z][-\\w]*)\\}\\}\\}$" text)
               (when match
                 (let* ((name (string-downcase (aref groups 1)))
                        (end (position (format nil "{{{/~a}}}" name) texts :start (1+ i) :test #'string-equal)))
                   (when end
                     (let ((keep (eq (string= (aref groups 0) "#") (docx-has-value-p name))))
                       (if keep
                           (progn (push p drop) (push (nth end paragraphs) drop))
                           ;; all from the opening mark's paragraph to the closing one's
                           (push (cons (car p) (cdr (nth end paragraphs))) drop))))))))
    (let ((out (make-string-output-stream)) (p 0))
      (dolist (span (sort (remove-duplicates drop :test #'equal) #'< :key #'car))
        (when (>= (car span) p)
          (write-string (subseq xml p (car span)) out)
          (setf p (cdr span))))
      (write-string (subseq xml p) out)
      (get-output-stream-string out))))

(defun docx-fill-xml (xml)
  "XML, a template's part, its places filled (rows, runs of paragraphs, then
each paragraph); a paragraph holding only {{{body}}} as *DOCX-BODY-MARK*, one
holding only {{{NAME:path}}} as that picture."
  (xml-replace-elements
   (docx-fill-paragraph-blocks (docx-fill-rows xml)) "w:p"
   (lambda (paragraph-xml)
     (let ((text (string-trim " 	" (docx-xml-text paragraph-xml))))
       (concatenate
        'string
        ;; the note's text and contents begin a page when the paragraph that held them did
        (if (and (member text '("{{{body}}}" "{{{contents}}}") :test #'string=) (search "<w:pageBreakBefore/>" paragraph-xml))
            "<w:p><w:r><w:br w:type=\"page\"/></w:r></w:p>"
            "")
        (multiple-value-bind (name way size) (docx-place text)
          (cond ((string= text "{{{body}}}") *docx-body-mark*)
                ;; the contents, unless #+OPTIONS: toc:nil leaves them out
                ((string= text "{{{contents}}}")
                 (let ((toc (org-latex-option "toc" 3))) (if toc (docx-contents (if (integerp toc) toc 3)) "")))
                ((and name (eq way :path))
                 (let ((picture (apply #'docx-field name :path size)))
                   (if picture
                       (multiple-value-bind (items head tail) (docx-run-items paragraph-xml)
                         (declare (ignore items))
                         (concatenate 'string head picture tail))
                       "")))
                (t (docx-fill-paragraph paragraph-xml)))))))))

(defun template-places (text)
  "The names of the places TEXT, a template's, holds ({{{NAME}}}, its
conditions {{{#NAME}}} and the like, a picture's {{{NAME:path:height=..}}}),
in lower case, sorted, each once."
  (let ((names '()))
    (cl-ppcre:do-scans (start end group-starts group-ends *template-place-re* text)
      (pushnew (string-downcase (subseq text (aref group-starts 1) (aref group-ends 1))) names :test #'string=))
    (sort names #'string<)))

(defun docx-template-text (parts)
  "The text of the template PARTS' document, headers and footers, together,
where its places are."
  (format nil "~{~a~^ ~}"
          (loop for (name) in parts
                when (cl-ppcre:scan "^word/(document|header\\d*|footer\\d*)\\.xml$" name)
                  collect (docx-xml-text (docx-part-text parts name)))))

(defun docx-template-places (parts)
  "The names of the places the template PARTS hold, in its document, headers
and footers, sorted, each once (TEMPLATE-PLACES)."
  (template-places (docx-template-text parts)))

(defparameter *latex-only-places* '("packages" "latex_header" "font_fallbacks" "fonts" "engine" "created" "latex_class" "latex_class_options"
                                    "secnumdepth" "tocdepth" "toc" "maketitle" "pdfauthor" "pdftitle" "pdfsubject" "pdfkeywords"
                                    "pdflang" "pdfcreator")
  "The places only a LaTeX template has use for, never asked of a Word one.")

(defun docx-pair-notes (name parts directory)
  "What to say of the template NAME's two halves when they ask for different
places: its LaTeX half's places its Word half (PARTS) has not, and the other
way, but those only LaTeX has use for (*LATEX-ONLY-PLACES*)."
  (let ((latex (ignore-errors (latex-template-for name directory))))
    (when latex
      (let* ((tex (set-difference (template-places latex) *latex-only-places* :test #'string=))
             ;; its contents a LaTeX template writes with \\tableofcontents
             (word (remove "contents" (docx-template-places parts) :test #'string=))
             (only-tex (sort (set-difference tex word :test #'string=) #'string<))
             (only-word (sort (set-difference word tex :test #'string=) #'string<)))
        (remove nil (list (and only-tex (format nil "The template ~a's Word half has no place for ~{{{{~a}}}~^, ~}, which its LaTeX half has" name only-tex))
                          (and only-word (format nil "The template ~a's LaTeX half has no place for ~{{{{~a}}}~^, ~}, which its Word half has" name only-word))))))))

;;; The template's parts with what the note needs

(defun docx-template-section (document)
  "Set the page of the document being written from the template's DOCUMENT
\(its last w:sectPr): its header and footer references, its size and margins,
and the width its text has."
  (let ((sections (xml-elements document "w:sectPr")))
    (when sections
      (let* ((section (subseq document (car (car (last sections))) (cdr (car (last sections)))))
             (references (format nil "~{~a~}" (mapcar (lambda (e) (subseq section (car e) (cdr e)))
                                                     (append (xml-elements section "w:headerReference") (xml-elements section "w:footerReference")))))
             (size (xml-element section "w:pgSz"))
             (margins (xml-element section "w:pgMar"))
             (page (concatenate 'string (if size (subseq section (car size) (cdr size)) "") (if margins (subseq section (car margins) (cdr margins)) ""))))
        (setf (docx-section *docx*) (list :references references :page page :title-page (and (search "<w:titlePg" section) t)))
        (let ((width (docx-page-number page "w:pgSz" "w:w"))
              (left (docx-page-number page "w:pgMar" "w:left"))
              (right (docx-page-number page "w:pgMar" "w:right")))
          (when (and width left right)
            (setf (docx-text-width *docx*) (- width left right))))))))

(defparameter *docx-template-design-file* "tuatara.txt"
  "The file of a Word template's folder saying how the note's text follows its
design, as lines of NAME: VALUE: table style, the style of the note's tables
\(a table style's id in the template's styles); bullets and numbers, the
abstract numbering (an abstractNumId in its numbering) the note's bulleted
and numbered lists take; table position, left, centre or right. Made by \\[make-template-from-docx]; never written into a
document.")

(defun docx-template-design (parts)
  "What the Word template PARTS says of its design (*DOCX-TEMPLATE-DESIGN-FILE*),
a plist: :TABLE-STYLE, :TABLE-POSITION, :BULLETS, :NUMBERS."
  (let ((text (docx-part-text parts *docx-template-design-file*)) (design '()))
    (dolist (line (and text (uiop:split-string text :separator '(#\Newline))) design)
      (let ((colon (position #\: line)))
        (when colon
          (let ((name (string-downcase (string-trim " 	" (subseq line 0 colon))))
                (value (string-trim " 	" (subseq line (1+ colon)))))
            (when (plusp (length value))
              (cond ((string= name "table style") (setf (getf design :table-style) value))
                    ((string= name "table position") (setf (getf design :table-position) value))
                    ((string= name "bullets") (setf (getf design :bullets) value))
                    ((string= name "numbers") (setf (getf design :numbers) value))))))))))

(defun docx-template-start (parts)
  "Make the document being written fit the template PARTS: its page, its
text's size (*DOCX-BASE-SIZE*) and its font's width (*DOCX-CHARACTER-RATIO*),
its lists numbered above the template's own, its pictures named apart from
the template's."
  (docx-template-section (or (docx-part-text parts "word/document.xml") ""))
  (let ((design (docx-template-design parts)))
    ;; the margins of its table style's cells, or of its plain tables', Word's 108 when it says none
    (when (getf design :table-style)
      (let ((styles (or (docx-part-text parts "word/styles.xml") "")))
        (setf (getf design :cell-margin)
              (or (loop for id in (list (getf design :table-style) "TableNormal")
                        for at = (search (format nil "w:styleId=\"~a\"" id) styles)
                        for end = (and at (search "</w:style>" styles :start2 at))
                        for found = (and end (nth-value 1 (cl-ppcre:scan-to-strings "<w:tblCellMar>.*?<w:(?:left|start) w:w=\"(\\d+)\"" (subseq styles at end))))
                        when found return (parse-integer (aref found 0)))
                  108))))
    ;; the levels its bullets and numbers define: a list deeper than they go numbered by Tuatara
    (let ((numbering (or (docx-part-text parts "word/numbering.xml") "")))
      (dolist (key '(:bullets :numbers))
        (let ((id (getf design key)))
          (when id
            (let* ((at (search (format nil "w:abstractNumId=\"~a\"" id) numbering))
                   (end (and at (search "</w:abstractNum>" numbering :start2 at))))
              (setf (getf design (if (eq key :bullets) :bullet-levels :number-levels))
                    (and end (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "(?<=<w:lvl w:ilvl=\")\\d+" (subseq numbering at end))))))))))
    (setf (getf (docx-section *docx*) :design) design))
  ;; its text's size, what the sizes of tables and code are parts of
  (let ((size (nth-value 1 (cl-ppcre:scan-to-strings "(?s)<w:rPrDefault>.*?<w:sz w:val=\"(\\d+)\"" (or (docx-part-text parts "word/styles.xml") "")))))
    (when size (setf *docx-base-size* (parse-integer (aref size 0)))))
  ;; its font's width, a monospaced one installed here measured
  (let ((font (nth-value 1 (cl-ppcre:scan-to-strings "(?s)<w:rPrDefault>.*?<w:rFonts[^>]*w:ascii=\"([^\"]+)\"" (or (docx-part-text parts "word/styles.xml") "")))))
    (setf *docx-character-ratio* (or (and font (font-character-ratio (aref font 0))) 3/5)))
  (let ((numbering (docx-part-text parts "word/numbering.xml")) (top 2))
    (when numbering
      (cl-ppcre:do-register-groups (id) ("w:(?:abstractNumId|numId)=\"(\\d+)\"" numbering)
        (setf top (max top (parse-integer id)))))
    (setf (docx-list-base *docx*) (1+ top)
          ;; named apart from the template's own pictures, when it has any
          (docx-media-prefix *docx*) (if (find-if (lambda (part) (uiop:string-prefix-p "word/media/" (car part))) parts)
                                         "tuatara-image"
                                         "image"))))

(defun docx-merge-into (xml closing addition)
  "XML with ADDITION put before its CLOSING tag."
  (let ((at (search closing xml :from-end t)))
    (if at (concatenate 'string (subseq xml 0 at) addition (subseq xml at)) (concatenate 'string xml addition))))

(defun docx-template-document (parts body)
  "The document part of a Word document made from the template PARTS, BODY
the note's text written: the template's document filled, BODY in its place,
the text's own last section in place of the template's last when a break
ends it and nothing follows it in the template; with sections in the text,
the first page the first section's, and what follows columns going on on
their page."
  (let* ((document (docx-fill-xml (docx-part-text parts "word/document.xml")))
         ;; the text's own last section, when a break ends it and nothing follows it in the template,
         ;; in place of the template's last (no empty section after it)
         (last nil)
         (document (let ((at (search *docx-body-mark* document)))
                     (if at
                         (let* ((after (subseq document (+ at (length *docx-body-mark*))))
                                (final (search "<w:sectPr" after)))
                           (multiple-value-bind (folded section) (docx-fold-last-break body)
                             (if (and section final (not (cl-ppcre:scan "<w:p[\\s>]" (subseq after 0 final))))
                                 (let ((end (+ (search "</w:sectPr>" after :start2 final) (length "</w:sectPr>"))))
                                   (setf last t)
                                   (concatenate 'string (subseq document 0 at) folded (subseq after 0 final) section (subseq after end)))
                                 (concatenate 'string (subseq document 0 at) body after))))
                         document))))
    ;; sections in the text: the first page is the first section's, not the last's, and the text
    ;; after columns goes on on their page, the columns balanced, not on a new one; after pages
    ;; turned sideways, on a page of its own (the type Word takes when none is given)
    (if (and (getf (docx-section *docx*) :broken) (not last))
        (let* ((last (search "<w:sectPr" document :from-end t))
               (section (cl-ppcre:regex-replace-all "<w:type(?:\\s[^>]*)?/>"
                                                    (cl-ppcre:regex-replace "<w:titlePg(?:\\s[^>]*)?/>" (subseq document last) "")
                                                    ""))
               (at (or (search "<w:pgSz" section) (search "</w:sectPr>" section))))
          (concatenate 'string (subseq document 0 last)
                       (subseq section 0 at)
                       (if (eq (getf (docx-section *docx*) :start :continuous) :continuous) "<w:type w:val=\"continuous\"/>" "")
                       (subseq section at)))
        document)))

(defun docx-template-styles (parts)
  "The styles of a Word document made from the template PARTS: its own, with
Tuatara's added where it has none of that name."
  (let ((ours (docx-styles (list (ignore-errors (chosen-font :main)) (ignore-errors (chosen-font :sans)) (ignore-errors (chosen-font :mono)))))
        (theirs (docx-part-text parts "word/styles.xml")))
    (if (null theirs)
        ours
        (docx-merge-into theirs "</w:styles>"
                         (format nil "~{~a~}"
                                 (loop for (s . e) in (xml-elements ours "w:style")
                                       for style = (subseq ours s e)
                                       for id = (nth-value 1 (cl-ppcre:scan-to-strings "w:styleId=\"([^\"]+)\"" style))
                                       unless (and id (search (format nil "w:styleId=\"~a\"" (aref id 0)) theirs))
                                         collect style))))))

(defun docx-template-numbering (parts)
  "The lists of a Word document made from the template PARTS: its own with
the note's."
  (let ((theirs (docx-part-text parts "word/numbering.xml")) (ours (docx-numbering)))
    (if (null theirs)
        ours
        (let ((abstracts (format nil "~{~a~}" (mapcar (lambda (e) (subseq ours (car e) (cdr e))) (xml-elements ours "w:abstractNum"))))
              (nums (format nil "~{~a~}" (mapcar (lambda (e) (subseq ours (car e) (cdr e))) (xml-elements ours "w:num")))))
          ;; Word wants every abstractNum before the first num
          (let* ((first-num (search "<w:num " theirs))
                 (with-abstracts (if first-num (concatenate 'string (subseq theirs 0 first-num) abstracts (subseq theirs first-num))
                                     (docx-merge-into theirs "</w:numbering>" abstracts))))
            (docx-merge-into with-abstracts "</w:numbering>" nums))))))

(defun docx-template-footnotes (parts)
  "The footnotes of a Word document made from the template PARTS: its own
with the note's."
  (let ((theirs (docx-part-text parts "word/footnotes.xml")) (ours (docx-footnotes-part)))
    (if (null theirs)
        ours
        (docx-merge-into theirs "</w:footnotes>"
                         (format nil "~{~a~}" (loop for (s . e) in (xml-elements ours "w:footnote")
                                                    for note = (subseq ours s e)
                                                    unless (search "w:type=" note) collect note))))))

(defun docx-template-relations (parts)
  "The relations of the document part of a Word document made from the
template PARTS: its own, with the parts of Tuatara's it lacks a relation to
and the pictures and links the note adds."
  (let* ((theirs (or (docx-part-text parts "word/_rels/document.xml.rels")
                     (docx-relationships "")))
         ;; the parts of ours the template lacks a relation to, newest first as DOCX-RELATIONS-XML takes them
         (needed (remove nil (list (and (not (search "/relationships/footnotes\"" theirs)) (list "rIdTuataraFootnotes" "footnotes" "footnotes.xml" nil))
                                   (and (not (search "/relationships/numbering\"" theirs)) (list "rIdTuataraNumbering" "numbering" "numbering.xml" nil))
                                   (and (not (search "/relationships/styles\"" theirs)) (list "rIdTuataraStyles" "styles" "styles.xml" nil))))))
    (docx-merge-into theirs "</Relationships>"
                     (concatenate 'string (docx-relations-xml needed) (docx-relations-xml (docx-relations *docx*))))))

(defun docx-template-types (parts)
  "The content types of a Word document made from the template PARTS: its
own, with those of the pictures and parts Tuatara adds."
  (let ((theirs (docx-part-text parts "[Content_Types].xml")))
    (docx-merge-into theirs "</Types>"
                     (format nil "~{~a~}"
                             (remove nil (list (and (not (search "Extension=\"png\"" theirs)) "<Default Extension=\"png\" ContentType=\"image/png\"/>")
                                               (and (not (search "Extension=\"jpeg\"" theirs)) "<Default Extension=\"jpeg\" ContentType=\"image/jpeg\"/>")
                                               (and (not (search "/word/numbering.xml" theirs)) "<Override PartName=\"/word/numbering.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml\"/>")
                                               (and (not (search "/word/footnotes.xml" theirs)) "<Override PartName=\"/word/footnotes.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml\"/>")
                                               (and (not (search "/docProps/core.xml" theirs)) "<Override PartName=\"/docProps/core.xml\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\"/>")))))))

(defun docx-template-other-parts (parts)
  "The parts of the template PARTS a Word document made from it takes as they
are, its headers and footers filled, each with its own relations when the
note adds pictures to it; the template's design for Tuatara left out."
  (loop for (name . octets) in parts
        ;; what the template says of its design for Tuatara, never in the document
        unless (string= name *docx-template-design-file*)
        append (if (cl-ppcre:scan "^word/(header|footer)\\d*\\.xml$" name)
                   ;; a picture in a header is named in the header's own relations
                   (let ((saved (docx-relations *docx*)))
                     (setf (docx-relations *docx*) '())
                     (let* ((filled (docx-fill-xml (docx-part-text parts name)))
                            (own (docx-relations *docx*))
                            (rels-name (format nil "word/_rels/~a.rels" (subseq name 5))))
                       (setf (docx-relations *docx*) saved)
                       (cons (cons name filled)
                             (and own
                                  (list (cons rels-name
                                              (docx-merge-into (or (docx-part-text parts rels-name)
                                                                   (docx-relationships ""))
                                                               "</Relationships>" (docx-relations-xml own))))))))
                   (list (cons name octets)))))

(defun docx-template-package (parts body)
  "The parts of a Word document made from the template PARTS, BODY the note's
text written: the template's document filled, its styles with Tuatara's
added where it has none of that name, its lists and footnotes with the
note's, its relations and types with the pictures and links the note adds."
  (setf (org-latex-refused *org-latex*) (template-refused-pictures (docx-template-text parts)))
  ;; in this order: filling the document names its pictures in the relations, filling the headers adds theirs
  (let* ((document (docx-template-document parts body))
         (styles (docx-template-styles parts))
         (numbering (docx-template-numbering parts))
         (footnotes (docx-template-footnotes parts))
         (relations (docx-template-relations parts))
         (types (docx-template-types parts))
         (other-parts (docx-template-other-parts parts))
         (replaced (list (cons "[Content_Types].xml" types)
                         (cons "word/document.xml" document)
                         (cons "word/styles.xml" styles)
                         (cons "word/numbering.xml" numbering)
                         (cons "word/footnotes.xml" footnotes)
                         (cons "word/_rels/document.xml.rels" relations)
                         (cons "docProps/core.xml" (docx-core (org-latex-plain (org-latex-title))
                                                              (org-latex-plain (or (org-latex-keyword "AUTHOR") "")))))))
    ;; the content types first, as Word writes them; the template's other parts as they are; the note's pictures
    (append (list (first replaced))
            (rest replaced)
            ;; a header's relations, when the note added to them, in place of the template's own
            (remove-duplicates (remove-if (lambda (part) (assoc (car part) replaced :test #'string=)) other-parts)
                               :key #'car :test #'string=)
            (loop for (name . octets) in (reverse (docx-media *docx*))
                  collect (cons (format nil "word/media/~a" name) octets)))))
