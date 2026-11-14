;;;; lisp/org/html.lisp --- An Org document written as HTML.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; ORG-HTML-DOCUMENT writes an Org document as one web page, its stylesheet
;;;; inside it; ORG-HTML-PARTS gives the pieces of such a page, for pages laid
;;;; out by something else (tools/manual-html.lisp, the manual). The document is
;;;; read as the LaTeX and Word exports read it (ORG-EXPORT-TAKE-IN): the same
;;;; macros, keywords, options, tags left out and link targets. Every character
;;;; of the document is escaped: nothing in it is run, and the #+HTML: lines,
;;;; raw HTML blocks and @@html: snippets a page could carry are left out, which
;;;; the export says when it is done. The stylesheet's colours come from the Ao
;;;; theme (WEB-PAGE-COLOURS).

(in-package #:tuatara.defaults)

(defparameter *org-html-style* (data-file-text "org-html.css")
  "The stylesheet of a web page made from an Org document, without its
colours: each colour is a variable (var(--bg) and the like) ORG-HTML-STYLESHEET gives.")

(defparameter *org-html-image-types* '("png" "jpg" "jpeg" "gif" "svg" "webp")
  "The kinds of file, by their names' endings, a browser shows as a picture.")

(defparameter *org-html-most-redacted* 200
  "The most characters of a black bar standing for redacted text.")

(define-record (org-html (:copier nil))
  "What writing one Org document as HTML knows beyond its reading (*ORG-EXPORT*):
CHAPTER, the number its first headline has in a larger work, or NIL for a whole
page. KEYS, true when ~text~ is a key to press (<kbd>) and not code. RECORDS,
each headline's record (LEVEL NUMBER TITLE ID) by the headline, TITLE its HTML.
HEADINGS, those records in order. IDS, by each id given so far the next number
to try after it as a name. FOOTNOTES, each (NUMBER LABEL TEXT) by order of first
reference, the last first. FOOTNOTE-ENTRIES, those with a label by the label.
FOOTNOTE-COUNT, how many there are. ELSEWHERE, true when the page is written
away from its note. CONTENTS-WRITTEN, true once a #+TOC: line has given its
contents."
  (chapter nil
           :documentation "The number the first headline of an Org HTML export has in a larger work, or NIL for a
whole page.")
  (keys nil :documentation "True when ~text~ in an Org HTML export is a key to press (<kbd>) and not code.")
  (records (make-hash-table :test 'eq)
           :documentation "The record (LEVEL NUMBER TITLE ID) of each headline of an Org HTML export, by the headline,
the TITLE as HTML.")
  (headings '() :documentation "The records of the headlines of an Org HTML export, in order.")
  (ids (make-hash-table :test 'equal)
       :documentation "The next number to try after each id an Org HTML export has given so far, to make a name.")
  (footnotes '()
             :documentation "The footnotes of an Org HTML export, each (NUMBER LABEL TEXT), by order of first
reference, the last first.")
  (footnote-entries (make-hash-table :test 'equal)
                    :documentation "The footnotes of an Org HTML export that have a label, by the label.")
  (footnote-count 0 :documentation "How many footnotes an Org HTML export has.")
  (elsewhere nil :documentation "True when the page of an Org HTML export is written away from its note.")
  (contents-written nil :documentation "True once a #+TOC: line has given its contents to an Org HTML export."))

(defvar *org-html* nil
  "The ORG-HTML of the document being written as HTML.")

(defun org-html-text (string newlines)
  "STRING, plain text of the note, as HTML: escaped, its --, --- and ... made
dashes and an ellipsis unless #+OPTIONS: -:nil; each line break a <br> with NEWLINES."
  (let ((text (xml-escape (if (org-export-option "-" t)
                              (cl-ppcre:regex-replace-all
                               "---?|\\.\\.\\." string
                               (lambda (match) (cond ((string= match "---") "—") ((string= match "--") "–") (t "…")))
                               :simple-calls t)
                              string))))
    (if newlines
        (cl-ppcre:regex-replace-all "\\n" text "<br>
")
        text)))

(defun org-html-as-written (node)
  "The text of the object NODE as the note writes it, escaped: what has no HTML of its own."
  (xml-escape (subseq *org-export-text* (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))))

(defun org-html-objects-between (objects begin end out newlines)
  "Write the text from BEGIN below END to OUT, OBJECTS the objects in it, as
HTML and the rest escaped, each line break a <br> with NEWLINES."
  (org-export-objects-between objects begin end out
                              :text (lambda (string) (org-html-text string newlines))
                              :object #'org-html-object
                              :blank (lambda (n) (spaces n))))

(defun org-html-objects-text (node &key newlines)
  "The contents of NODE, text and objects, as HTML, without the line break at its end."
  (if (org-node-contents-begin node)
      (string-right-trim
       '(#\Newline)
       (with-output-to-string (s)
         (org-html-objects-between (org-node-children node) (org-node-contents-begin node)
                                   (org-node-contents-end node) s newlines)))
      ""))

(defun org-html-line (string)
  "STRING, a line of Org text such as a keyword's value, as HTML (ORG-PARSE-LINE)."
  (let ((*org-export-text* string))
    (org-html-objects-text (org-parse-line string))))

;;; Headings and places

(defun org-html-new-id (words)
  "A new id for the place WORDS name, letters, digits and hyphens in lower
case, made different from the ids given before it; \"place\" when WORDS holds none."
  (let* ((slug (remove-if-not (lambda (c) (or (alphanumericp c) (char= c #\-))) (title-slug words)))
         (base (if (zerop (length slug)) "place" slug))
         (ids (org-html-ids *org-html*)))
    (loop for n from (gethash base ids 1)
          for name = (if (= n 1) base (format nil "~a-~d" base n))
          unless (gethash name ids)
          do (setf (gethash name ids) 2
                   (gethash base ids) (1+ n))
          (return name))))

(defun org-html-id (node)
  "The id of the place NODE, given once: a headline's by its CUSTOM_ID or else its
title, an element's by its #+NAME, a target's by its words."
  (let ((labels (org-export-labels *org-export*)))
    (or (gethash node labels)
        (setf (gethash node labels)
              (org-html-new-id (or (org-headline-property node "CUSTOM_ID")
                                   (org-property node :raw-value)
                                   (org-property node :name)
                                   (org-property node :value)
                                   ""))))))

(defun org-html-headline-title (headline)
  "HEADLINE's title alone as HTML."
  (with-output-to-string (s)
    (org-html-objects-between (org-property headline :title) (org-property headline :title-begin)
                              (org-property headline :title-end) s nil)))

(defun org-html-gather-headings (tree)
  "Record each headline of TREE left in the export (ORG-HTML-RECORDS): its level
from 1, its number as the options give it (from the chapter's number when
there is one), its title as HTML and its id."
  (let ((html *org-html*)
        (min-level (org-export-min-level *org-export*))
        (depth (org-export-option "num" t)))
    (labels ((walk (node prefix)
               (let ((count 0))
                 (dolist (child (org-node-children node))
                   (when (and (eq (org-node-type child) 'headline) (not (org-export-skipped-p child)))
                     (let* ((level (1+ (- (org-property child :level) min-level)))
                            (numbered (and (org-export-numbered-p child) (or (not (integerp depth)) (<= level depth))))
                            (number (and numbered
                                         (progn (incf count)
                                                (if prefix
                                                    (format nil "~a.~d" prefix count)
                                                    (format nil "~d" (+ (or (org-html-chapter html) 1) count -1))))))
                            (record (list level number (org-html-headline-title child) (org-html-id child))))
                       (setf (gethash child (org-html-records html)) record)
                       (push record (org-html-headings html))
                       (walk child number)))))))
      (walk tree nil))
    (setf (org-html-headings html) (nreverse (org-html-headings html)))))

(defun org-html-contents (headings &key (page "") depth)
  "HEADINGS, records (LEVEL NUMBER TITLE ID) in order, as nested numbered lists
of links into PAGE, those no deeper than level DEPTH when it is given; an empty string for none."
  (with-output-to-string (out)
    (let ((open 0))
      (flet ((close-down-to (level)
               (loop repeat (- open level) do (write-string "</li></ol>" out))))
        (dolist (heading headings)
          (destructuring-bind (level number title id) heading
            (when (or (null depth) (<= level depth))
              (cond ((> level open) (loop repeat (- level open) do (write-string "<ol><li>" out)))
                    (t (close-down-to level)
                       (write-string "</li><li>" out)))
              (setf open level)
              (format out "<a href=\"~a#~a\">~@[~a ~]~a</a>" (xml-escape page) (xml-escape id) number title))))
        (close-down-to 0)))))

;;; Elements

(defun org-html-elements (node out)
  "Write the elements in NODE to OUT."
  (dolist (child (org-node-children node))
    (org-html-element child out)))

(defun org-html-note (kind)
  "Say, when the document is made, that KIND of raw HTML was left out of it."
  (export-note "~a left out of the web page: it shows only what the note writes as text" kind))

(defun org-html-caption (node)
  "NODE's #+CAPTION as HTML, or NIL."
  (let ((caption (getf (org-property node :affiliated) :caption)))
    (and caption (org-html-line caption))))

(defun org-html-with-figure (node out write)
  "WRITE, a function of a stream, gives NODE's content; written to OUT, in a
<figure> with its #+CAPTION as the figure's caption when it has one, the
figure carrying NODE's #+NAME as its id."
  (let ((caption (org-html-caption node)) (name (org-property node :name)))
    (if (or caption name)
        (progn
          (format out "<figure~@[ id=\"~a\"~]>~%" (and name (org-html-id node)))
          (funcall write out)
          (format out "~@[<figcaption>~a</figcaption>~%~]</figure>~%" caption))
        (funcall write out))))

(defun org-html-pre (node text out &optional language)
  "Write TEXT, the text of the block NODE, to OUT as it is written, in LANGUAGE's
code when one is given; a figure when NODE has a caption (ORG-HTML-WITH-FIGURE)."
  (org-html-with-figure
   node out
   (lambda (out)
     (format out "<pre class=\"ex\">~:[~*~a~;<code class=\"language-~a\">~a</code>~]</pre>~%"
             language (and language (org-html-class-name language)) (xml-escape text)))))

(defun org-html-class-name (name)
  "NAME as a CSS class: in lower case, letters, digits and hyphens alone."
  (cl-ppcre:regex-replace-all "[^a-z0-9-]" (string-downcase name) ""))

(defun org-html-mode-line (line signs lit)
  "LINE, a mode line drawn in a text, as HTML: the letters SIGNS, when they
stand in it, each lit when it is among LIT, else faint."
  (let ((at (and signs (plusp (length signs)) (search signs line))))
    (if at
        (format nil "~a~{~a~}~a" (xml-escape (subseq line 0 at))
                (loop for c across signs
                      collect (format nil "<span class=\"~:[faint~;sign~]\">~a</span>" (find c (or lit ""))
                                      (xml-escape (string c))))
                (xml-escape (subseq line (+ at (length signs)))))
        (xml-escape line))))

(defun org-html-prompt-line (line)
  "LINE, a minibuffer's row drawn in a text, as HTML: its prompt, the words up
to its colon after any count (2/3), in bold, and the cursor drawn at its end."
  (cl-ppcre:register-groups-bind (count prompt rest) ("^(\\s*\\d+/\\d+\\s+)?([^:]*:)(.*)$" line)
                                 (return-from org-html-prompt-line
                                   (format nil "~a<b>~a</b>~a█" (xml-escape (or count "")) (xml-escape prompt)
                                           (xml-escape rest))))
  (format nil "~a█" (xml-escape line)))

(defun org-html-screen (node attributes out)
  "Write the example block NODE to OUT as a picture of the screen, as
#+ATTR_HTML: :screen t asks: :ml N and :pr N name its mode line and its prompt
row, from 1, :signs the mode line's letters and :lit the ones lit (ORG-HTML-MODE-LINE)."
  (let ((ml (parse-integer (or (getf attributes :ml) "") :junk-allowed t))
        (pr (parse-integer (or (getf attributes :pr) "") :junk-allowed t))
        (signs (getf attributes :signs))
        (lit (getf attributes :lit)))
    (org-html-with-figure
     node out
     (lambda (out)
       (write-string "<div class=\"screen\">" out)
       (loop for line in (cl-ppcre:split "\\n" (or (org-property node :value) ""))
             for row from 1
             do (cond ((eql row ml) (format out "<div class=\"ml\">~a</div>" (org-html-mode-line line signs lit)))
                      ((eql row pr) (format out "<div class=\"pr\">~a</div>" (org-html-prompt-line line)))
                      ((string= (string-trim " " line) "") (write-string "<div> </div>" out))
                      (t (format out "<div>~a</div>" (xml-escape line)))))
       (format out "</div>~%")))))

(defun org-html-special-block (node out)
  "Write the block NODE, one of Org's own kinds or none, to OUT as a <div> of the
class its name gives, the words after its #+BEGIN_NAME as its heading."
  (let ((heading (org-property node :parameters)))
    (format out "<div class=\"~a\">~%" (org-html-class-name (org-property node :type)))
    (when heading (format out "<h3>~a</h3>~%" (org-html-line heading)))
    (org-html-elements node out)
    (format out "</div>~%")))

(defun org-html-paragraph-after-headline-p (node)
  "True when the paragraph NODE is the first thing in a chapter: what comes
straight after the headline at the top of a chapter (ORG-HTML-CHAPTER)."
  (let* ((section (org-node-parent node))
         (headline (and section (org-node-parent section))))
    (and (org-html-chapter *org-html*)
         headline
         (eq (org-node-type headline) 'headline)
         (= 1 (first (gethash headline (org-html-records *org-html*))))
         (eq node (first (org-node-children section))))))

(defun org-html-image-link-p (node)
  "True when NODE is a link to a picture a web page shows as the picture
\(ORG-IMAGE-LINK-P, of *ORG-HTML-IMAGE-TYPES*)."
  (org-image-link-p node *org-html-image-types*))

(defun org-html-full-name-p (name)
  "True when NAME, a file a link names, is a full name and not relative to the page."
  (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~" name)))

(defun org-html-file-address (path)
  "The address of the local file PATH as a link names it: as it stands, encoded,
when relative to the page, a file:/// address when it is a full name; what
it says after :: is left out."
  (let ((name (subseq path 0 (search "::" path))))
    (if (org-html-full-name-p name)
        (path-uri (tuatara.io:expand-path name))
        (tuatara.io:url-path name))))

(defun org-html-picture-allowed-p (path full)
  "True when the page may show the picture a link names PATH, which is found at
FULL: it is there, it stands where an export may take files from
\(ORG-EXPORT-FILE-ALLOWED-P) and, named relative to the note, the page is beside
the note, as a browser reads such a name from the page. NIL otherwise, the
export saying why."
  (cond ((and (org-html-elsewhere *org-html*) (not (org-html-full-name-p path)))
         (export-note "~a: a page written away from its note shows only pictures named in full; its name alone"
                      path))
        ((null (tuatara.io:path-stamp full :failure nil))
         (export-note "~a: no such picture, so its name alone" path))
        (t (org-export-file-allowed-p full))))

(defun org-html-image (node attributes)
  "The <img> for the picture NODE links to, sized by #+ATTR_HTML's :width when
that is a number or a percentage and described by its :alt, its name in code
instead when the page may not show it (ORG-HTML-PICTURE-ALLOWED-P)."
  (let* ((path (org-property node :path))
         (full (or (ignoring-errors-reported ("finding ~a" path) (org-export-path path)) path))
         (name (tuatara.io:native-file-name full))
         (width (getf attributes :width)))
    (if (org-html-picture-allowed-p path full)
        (format nil "<img src=\"~a\" alt=\"~a\"~@[ width=\"~a\"~]>" (xml-escape (org-html-file-address path))
                (xml-escape (or (getf attributes :alt) name))
                (and width (cl-ppcre:scan "^\\d+%?$" width) width))
        (format nil "<code>~a</code>" (xml-escape name)))))

(defun org-html-paragraph (node out)
  "Write the paragraph NODE to OUT, the first of a chapter as its lead; a
picture alone in it as a figure (ORG-EXPORT-PICTURE-ALONE-P)."
  (if (org-export-picture-alone-p node #'org-html-image-link-p)
      (org-html-with-figure
       node out
       (lambda (out)
         (format out "~a~%" (org-html-image (first (org-node-children node)) (org-export-attributes node "html")))))
      (format out "<p~:[~; class=\"lead\"~]>~a</p>~%" (org-html-paragraph-after-headline-p node)
              (org-html-objects-text node :newlines (org-export-option "\\n" nil)))))

(defun org-html-list-item-body (item out)
  "Write what the list ITEM holds to OUT: a first paragraph alone in it, or
with only a list after it, as bare words, without its own <p>."
  (let* ((children (org-node-children item))
         (first (first children))
         (bare (and first (eq (org-node-type first) 'paragraph)
                    (every (lambda (c) (eq (org-node-type c) 'plain-list)) (rest children)))))
    (when bare (write-string (org-html-objects-text first) out))
    (dolist (child (if bare (rest children) children))
      (org-html-element child out))))

(defun org-html-plain-list (node out)
  "Write the plain list NODE to OUT as <ul>, <ol> or, with descriptions, <dl>;
a checkbox as a sign before the item."
  (let* ((kind (org-property node :type))
         (tag (case kind (descriptive "dl") (ordered "ol") (t "ul"))))
    (format out "<~a>~%" tag)
    (dolist (item (org-node-children node))
      (when (eq (org-node-type item) 'item)
        (let* ((counter (org-property item :counter))
               (check (case (org-property item :checkbox) (on "☒") (off "☐") (trans "☐")))
               (item-tag (org-property item :tag))
               (body (with-output-to-string (s) (org-html-list-item-body item s))))
          (cond ((eq kind 'descriptive)
                 (format out "<dt>~@[~a ~]~a</dt><dd>~a</dd>~%" check (if item-tag (org-html-line item-tag) "") body))
                (t (format out "<li~@[ value=\"~d\"~]>~@[~a ~]~a</li>~%"
                           (and (integerp counter) counter) check body))))))
    (format out "</~a>~%" tag)))

(defun org-html-cell (cell tag alignment out)
  "Write CELL, of a table, to OUT as TAG (td or th), its class telling ALIGNMENT apart from the left."
  (format out "<~a~@[ class=\"~a\"~]>~a</~a>" tag (case alignment (:right "right") (:centre "centre"))
          (org-html-objects-text cell) tag))

(defun org-html-table (node out)
  "Write the table NODE to OUT: the rows above its first rule as its head, when a
rule follows some row of cells; its #+CAPTION as its caption."
  (multiple-value-bind (rows special standard cookies columns) (org-export-table-shape node)
    (let* ((rule (position 'rule rows :key (lambda (r) (org-property r :type))))
           (head (if (and rule (plusp rule)) (subseq rows 0 rule) '()))
           (body (remove 'rule (nthcdr (length head) rows) :key (lambda (r) (org-property r :type))))
           (alignments (export-column-alignments node columns special standard cookies)))
      (format out "<div class=\"tablewrap\"><table>~@[<caption>~a</caption>~]~%" (org-html-caption node))
      (flet ((write-rows (rows tag)
               (dolist (row rows)
                 (format out "<tr>")
                 (loop for cell in (if special (rest (org-node-children row)) (org-node-children row))
                       for alignment across alignments
                       do (org-html-cell cell tag alignment out))
                 (format out "</tr>~%"))))
        (when head
          (format out "<thead>~%")
          (write-rows head "th")
          (format out "</thead>~%"))
        (format out "<tbody>~%")
        (write-rows body "td")
        (format out "</tbody></table></div>~%")))))

(defun org-html-keyword-element (node out)
  "Write the keyword line NODE to OUT: the first #+TOC: headlines as a table of
contents (ORG-TOC-KEYWORD), any other, tables or figures too, left out, which is
said; a raw HTML line left out, which is said."
  (let ((key (org-property node :key)) (value (or (org-property node :value) "")))
    (cond ((string-equal key "HTML") (org-html-note "A #+HTML: line"))
          ((string-equal key "TOC")
           (multiple-value-bind (kind depth) (org-toc-keyword value)
             (case kind
               (:headlines
                (cond ((org-html-contents-written *org-html*)
                       (export-note "A repeated #+TOC: line left out of the web page: only the first gives contents"))
                      (t
                       (setf (org-html-contents-written *org-html*) t)
                       (format out "<nav class=\"contents\">~a</nav>~%"
                               (org-html-contents (org-html-headings *org-html*) :depth depth)))))
               ((:tables :figures)
                (export-note "A #+TOC: ~(~a~) line left out of the web page: it lists headings only" kind))))))))

(defun org-html-element (node out)
  "Write the element NODE to OUT, as its kind is written; nothing for what is never printed."
  (case (org-node-type node)
    ((org-data section dynamic-block) (org-html-elements node out))
    (headline (org-html-headline node out))
    (paragraph (org-html-paragraph node out))
    (plain-list (org-html-plain-list node out))
    (table (org-html-table node out))
    (src-block (org-html-pre node (org-block-code node) out (org-property node :language)))
    (example-block (let ((attributes (org-export-attributes node "html")))
                     (if (getf attributes :screen)
                         (org-html-screen node attributes out)
                         (org-html-pre node (or (org-property node :value) "") out))))
    (fixed-width (org-html-pre node (or (org-property node :value) "") out))
    (export-block (when (string= (org-property node :type) "HTML") (org-html-note "A block of raw HTML")))
    (latex-environment (format out "<pre class=\"ex\">~a</pre>~%" (xml-escape (org-property node :value))))
    (keyword (org-html-keyword-element node out))
    (horizontal-rule (format out "<hr>~%"))
    (quote-block (format out "<blockquote>~%") (org-html-elements node out) (format out "</blockquote>~%"))
    (center-block (format out "<div class=\"centre\">~%") (org-html-elements node out) (format out "</div>~%"))
    (special-block (org-html-special-block node out))
    (verse-block (format out "<p class=\"verse\">~a</p>~%" (org-html-objects-text node :newlines t)))
    (drawer (when (org-export-drawer-printed-p node) (org-html-elements node out)))
    (t nil)))

(defun org-html-headline (headline out)
  "Write HEADLINE and what is under it to OUT, unless it is left out of the
export: a heading one level deeper for a whole page's title above it, and
numbered, but for the top of a chapter, whose number the contents show."
  (let ((record (gethash headline (org-html-records *org-html*))))
    (when record
      (destructuring-bind (level number title id) record
        (let* ((chapter (org-html-chapter *org-html*))
               (tag (min 6 (if chapter level (1+ level))))
               (todo (org-property headline :todo-keyword))
               (priority (org-property headline :priority))
               (tags (and (org-export-option "tags" t) (org-property headline :tags))))
          (format out "<h~d id=\"~a\">" tag (xml-escape id))
          (when (and number (not (and chapter (= level 1))))
            (format out "<span class=\"num\">~a</span> " number))
          (when (and todo (org-export-option "todo" t))
            (format out "<span class=\"todo\">~a</span> " (xml-escape todo)))
          (when (and priority (org-export-option "pri" nil))
            (format out "<span class=\"priority\">#~a</span> " (xml-escape (string (code-char priority)))))
          (write-string title out)
          (format out "~{ <span class=\"tag\">~a</span>~}" (mapcar #'xml-escape tags))
          (format out "</h~d>~%" tag)
          (org-html-elements headline out))))))

;;; Objects

(defun org-html-footnote (node out)
  "Write the footnote reference NODE to OUT as its number, a link to its text,
the text given when first referred to (ORG-HTML-FOOTNOTES)."
  (when (org-export-option "f" t)
    (let* ((label (org-property node :label))
           (entries (org-html-footnote-entries *org-html*))
           (known (and label (gethash label entries))))
      (unless known
        (let ((entry (list (incf (org-html-footnote-count *org-html*)) label nil)))
          (push entry (org-html-footnotes *org-html*))
          (when label (setf (gethash label entries) entry))
          (setf (third entry)
                (with-org-export-expanding (:footnote (or label node)) ("")
                                           (if (eq (org-property node :type) 'inline)
                                               (org-html-objects-text node)
                                               (let ((found (and label
                                                                 (gethash label (org-export-footnotes *org-export*)))))
                                                 (if found
                                                     (with-output-to-string (s) (org-html-elements found s))
                                                     ""))))
                known entry)))
      (format out "<sup><a href=\"#fn-~d\">~d</a></sup>" (first known) (first known)))))

(defun org-html-footnotes-section (out)
  "Write the footnotes referred to in the document, in order, to OUT; nothing when there are none."
  (when (org-html-footnotes *org-html*)
    (format out "<section class=\"footnotes\"><ol>~%")
    (dolist (entry (reverse (org-html-footnotes *org-html*)))
      (destructuring-bind (number label text) entry
        (declare (ignore label))
        (format out "<li id=\"fn-~d\">~a</li>~%" number text)))
    (format out "</ol></section>~%")))

(defun org-html-link (node out)
  "Write the link NODE to OUT: a picture shown; a web, mail or news address, or
a local file (a note's .org leading to its .html), as a link; a place in the
document as a link to it, in its description or else its title; anything
else as its description or its target, as words."
  (let* ((type (org-property node :type))
         (path (org-property node :path))
         (description (and (org-node-contents-begin node) (org-html-objects-text node)))
         (address (org-link-address type path)))
    (cond ((org-html-image-link-p node)
           (write-string (org-html-image node (org-export-attributes (org-node-parent node) "html")) out))
          (address
           (format out "<a href=\"~a\">~a</a>" (xml-escape address) (or description (xml-escape address))))
          ((string= type "file")
           (format out "<a href=\"~a\">~a</a>"
                   (xml-escape (org-html-file-address (if (string-equal (tuatara.io:native-file-type path) "org")
                                                          (replaced-type path "html")
                                                          path)))
                   (or description (xml-escape path))))
          (t
           (let ((target (org-export-link-target node)))
             (cond ((and target (eq (org-node-type target) 'headline))
                    (destructuring-bind (level number title id) (gethash target (org-html-records *org-html*))
                      (declare (ignore level number))
                      (format out "<a href=\"#~a\">~a</a>" (xml-escape id) (or description title))))
                   (target
                    (format out "<a href=\"#~a\">~a</a>" (xml-escape (org-html-id target))
                            (or description (xml-escape path))))
                   ((uiop:string-prefix-p "denote:" path)
                    (write-string (or description (xml-escape (subseq path 7))) out))
                   (t (write-string (or description (xml-escape path)) out))))))))

(defun org-html-object (node out)
  "Write the object NODE to OUT."
  (flet ((wrap (tag) (format out "<~a>~a</~a>" tag (org-html-objects-text node) tag)))
    (case (org-node-type node)
      (bold (wrap "strong"))
      (italic (wrap "em"))
      (underline (wrap "u"))
      (strike-through (wrap "del"))
      (code (format out "<~:[code~;kbd~]>~a</~:[code~;kbd~]>" (org-html-keys *org-html*)
                    (xml-escape (org-property node :value)) (org-html-keys *org-html*)))
      ((verbatim inline-src-block) (format out "<code>~a</code>" (xml-escape (org-property node :value))))
      (entity (let ((character (gethash (org-property node :name) *org-entities*)))
                (write-string (if (and character (org-export-option "e" t))
                                  (xml-escape character)
                                  (org-html-as-written node))
                              out)))
      (line-break (format out "<br>~%"))
      (export-snippet
       (let ((back-end (org-property node :back-end)) (value (org-property node :value)))
         (cond ((string-equal back-end "html") (org-html-note "A raw HTML snippet"))
               ((and (string-equal back-end "docx") (uiop:string-prefix-p "redacted:" value))
                (let ((n (min *org-html-most-redacted* (or (whole-number value :start 9) 1))))
                  (format out "<span class=\"redacted\">~a</span>" (make-string n :initial-element #\█)))))))
      (footnote-reference (org-html-footnote node out))
      (link (org-html-link node out))
      (target (format out "<span id=\"~a\"></span>" (xml-escape (org-html-id node))))
      (radio-target (format out "<span id=\"~a\">~a</span>" (xml-escape (org-html-id node))
                            (org-html-objects-text node)))
      (timestamp (when (org-export-option "<" t)
                   (format out "<span class=\"timestamp\">~a</span>" (xml-escape (org-property node :raw-value)))))
      (statistics-cookie (write-string (xml-escape (org-property node :value)) out))
      ((subscript superscript)
       (let ((script (org-export-option "^" "{}")))
         (if (or (null script) (and (equal script "{}") (not (org-property node :use-brackets-p))))
             (write-string (org-html-as-written node) out)
             (wrap (if (eq (org-node-type node) 'subscript) "sub" "sup")))))
      (macro (let ((value (org-export-macro-value node)))
               (write-string (if value
                                 (with-org-export-expanding (:macro (org-property node :key))
                                   ((org-html-as-written node))
                                   (org-html-line value))
                                 (org-html-as-written node))
                             out)))
      ((inline-babel-call) nil)
      (t (write-string (org-html-as-written node) out)))))

;;; The document

(defun org-html-title-block (title out)
  "Write the head of a whole page, its TITLE and any #+SUBTITLE:, #+AUTHOR: and
#+DATE: lines, to OUT, each as HTML; nothing for a page with none of them."
  (let ((subtitle (org-export-keyword "SUBTITLE"))
        (author (and (org-export-option "author" t) (org-export-keyword "AUTHOR")))
        (date (and (org-export-option "date" t) (org-export-keyword "DATE"))))
    (when (and (org-export-option "title" t)
               (or (plusp (length title)) subtitle author date))
      (format out "<header class=\"title\">~%~@[<h1>~a</h1>~%~]~@[<p class=\"lead\">~a</p>~%~]~
                   ~@[<p>~a</p>~%~]~@[<p>~a</p>~%~]</header>~%"
              (and (plusp (length title)) (org-html-line title))
              (and subtitle (org-html-line subtitle))
              (and author (org-html-line author))
              (and date (org-html-line date))))))

(defun org-html-parts (text &key file chapter keys)
  "TEXT, an Org document, FILE its file or NIL, as the parts of a web page. A
plist of :TITLE, its title as plain text escaped for a <title>; :BODY, its text
as HTML. :HEADINGS, a record (LEVEL NUMBER TITLE ID) for each heading;
:LANGUAGE, the language its #+LANGUAGE: gives, en otherwise. :OUTPUT, where
#+EXPORT_FILE_NAME says to write it, a full name ending in .html, or NIL;
:NOTES, what else to say of it, a list of sentences. Read as the enhanced PDF
export reads it (its macros expanded, what redact and protect hide never in it).
Without CHAPTER, BODY begins with its title and contents as #+OPTIONS: says.
With CHAPTER, the number of its first headline in a larger work, that headline
is the top of the chapter and BODY has no title of its own. KEYS true makes
~text~ a key to press (<kbd>)."
  (with-org-export-reading (expanded tree directory) (text file :enhanced t)
                           (let* ((output (org-export-file-name text file))
                                  (elsewhere (and output (not (equal (tuatara.io:parent-path output) directory))))
                                  (*org-html* (make-org-html :chapter chapter :keys keys :elsewhere elsewhere)))
                             (org-html-gather-headings tree)
                             (let* ((title (org-export-title))
                                    (body (with-output-to-string (s)
                                            (unless chapter
                                              (org-html-title-block title s)
                                              (let ((toc (org-export-option "toc" t)))
                                                (when (and toc (org-html-headings *org-html*))
                                                  (format s "<nav class=\"contents\">~a</nav>~%"
                                                          (org-html-contents (org-html-headings *org-html*)
                                                                             :depth (and (integerp toc) toc))))))
                                            (org-html-element tree s)
                                            (org-html-footnotes-section s))))
                               (list :title (xml-escape (org-export-plain title))
                                     :body body
                                     :headings (org-html-headings *org-html*)
                                     :language (or (org-export-keyword "LANGUAGE") "en")
                                     :output (and output (replaced-type output "html"))
                                     :notes (reverse (org-export-notes *org-export*)))))))

(defun org-html-stylesheet (&key chosen)
  "The stylesheet of a web page made from an Org document: the colours of the
light Ao theme, the dark ones when the reader's system prefers dark (WEB-PAGE-COLOURS),
then the rules of *ORG-HTML-STYLE*. With CHOSEN true the reader may choose
instead: data-theme=\"light\" or \"dark\" on the page's <html> element wins over
the system's preference, and paper is always given the light colours."
  (flet ((variables (style)
           (with-output-to-string (s)
             (loop for (name . colour) in (web-page-colours style)
                   do (format s "~&  --~a: ~a;" name colour))
             (when (eq style :dark)
               (format s "~&  color-scheme: dark;")))))
    (if chosen
        (format nil ":root {~a~%}~%@media screen and (prefers-color-scheme: dark) {~%~
                     :root:not([data-theme=\"light\"]) {~a~%}~%}~%~
                     @media screen {~%:root[data-theme=\"dark\"] {~a~%}~%}~%~a"
                (variables :light) (variables :dark) (variables :dark) *org-html-style*)
        (format nil ":root {~a~%}~%@media (prefers-color-scheme: dark) {~%:root {~a~%}~%}~%~a"
                (variables :light) (variables :dark) *org-html-style*))))

(defun org-html-page (title body &key style stylesheet script icon (language "en"))
  "A whole web page of BODY, its head naming TITLE (already escaped): STYLE the
stylesheet's text to put inside, STYLESHEET, SCRIPT and ICON (the picture a
browser shows in the page's tab) the addresses of files beside it to load. Its
content security policy lets it load nothing else and run nothing else, so a
page keeps to itself even if a browser reads it wrongly."
  (format nil "<!doctype html>
<html lang=\"~a\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; ~
style-src ~:[~;'unsafe-inline'~]~:[~; 'self'~]; img-src 'self' file:~:[~;; script-src 'self'~]; ~
base-uri 'none'; form-action 'none'\">
<title>~a</title>
~@[<link rel=\"icon\" href=\"~a\">~%~]~@[<style>~a</style>~%~]~@[<link rel=\"stylesheet\" href=\"~a\">~%~]~
~@[<script src=\"~a\" defer></script>~%~]</head>
<body>
~a</body>
</html>
"
          (xml-escape language) style stylesheet script title (and icon (xml-escape icon)) style
          (and stylesheet (xml-escape stylesheet))
          (and script (xml-escape script)) body))

(defun org-html-document (text &key file)
  "TEXT, an Org document, written as one web page with its stylesheet inside it,
FILE its file or NIL: read as the enhanced PDF export reads it, every
character escaped, raw HTML left out. Returns the page; as a second value
where #+EXPORT_FILE_NAME says to write it, a full name ending in .html, or
NIL; as a third what else to say of it, a list of sentences."
  (let ((parts (org-html-parts text :file file)))
    (values (org-html-page (getf parts :title) (format nil "<main><article>~%~a</article></main>~%" (getf parts :body))
                           :style (org-html-stylesheet) :language (getf parts :language))
            (getf parts :output)
            (getf parts :notes))))
