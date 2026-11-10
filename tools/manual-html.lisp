;;;; tools/manual-html.lisp --- The manual as web pages and as LaTeX for a PDF, by Tuatara's own Org export.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:manual-html
  (:use #:cl)
  (:documentation "The manual's own build: its chapters as web pages and as one LaTeX document.")
  (:export #:run-html #:run-pdf))

(in-package #:manual-html)

(defparameter *navigation-lines* (cl-ppcre:create-scanner "(?m)^(?:Previous|Next): \\[\\[file:[^\\n]*\\n?")
  "A chapter's lines of links to the chapters before and after it, which the
pages draw for themselves and a whole manual has no need of.")

(defun chapter-texts (tree)
  "The chapters of the manual, from doc/manual/generated/ under TREE, in order:
each a plist of :NUMBER, :FILE, :STEM, the name of the page without its ending,
and :TEXT, the Org text without its navigation lines. The chapters are the
files whose names begin with a number."
  (sort (loop for file in (directory (merge-pathnames "doc/manual/generated/*.org" tree))
              for stem = (pathname-name file)
              for number = (parse-integer stem :junk-allowed t)
              when number
                collect (list :number number :file (namestring file) :stem stem
                              :text (cl-ppcre:regex-replace-all
                                     *navigation-lines* (uiop:read-file-string file :external-format :utf-8) "")))
        #'< :key (lambda (chapter) (getf chapter :number))))

(defun page-name (chapter)
  "The name of the page of CHAPTER."
  (format nil "~a.html" (getf chapter :stem)))

(defun parts-of (chapter)
  "CHAPTER written as HTML: its parts (ORG-HTML-PARTS), what is left to say printed."
  (let ((parts (tuatara.defaults::org-html-parts (getf chapter :text) :file (getf chapter :file)
                                                 :chapter (getf chapter :number) :keys t)))
    (dolist (note (getf parts :notes))
      (format t "~&~a: ~a~%" (getf chapter :stem) note))
    parts))

(defun chapter-title (parts)
  "The title of a chapter from its PARTS: that of its first heading, as HTML."
  (third (first (getf parts :headings))))

(defun contents-list (entries)
  "The manual's contents as a list of chapters, each with its sections under it,
ENTRIES its (CHAPTER . PARTS) in order: all of them in every page, for the
filter to look through, and folded but for the chapter a page shows."
  (with-output-to-string (out)
    (write-string "<ol class=\"chapters\">" out)
    (loop for (chapter . parts) in entries
          do (format out "<li id=\"nav-~a\"><a href=\"~a\">~d ~a</a>~a</li>"
                     (getf chapter :stem) (page-name chapter) (getf chapter :number) (chapter-title parts)
                     (tuatara.defaults::org-html-contents
                      (loop for (level number title id) in (rest (getf parts :headings))
                            collect (list (1- level) number title id))
                      :page (page-name chapter) :depth 2)))
    (write-string "</ol>" out)))

(defun here-marked (contents stem)
  "CONTENTS, the contents list, with the chapter STEM unfolded and marked as the
one this page shows."
  (let ((mark (format nil "<li id=\"nav-~a\"><a href=" stem)))
    (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars mark) contents
                                (format nil "<li id=\"nav-~a\" class=\"here\"><a class=\"here\" href=" stem))))

(defun node-links (previous next &key end)
  "The bar of links above a page, or below one with END: to PREVIOUS and NEXT,
each (CHAPTER . PARTS) or NIL, and up to the contents."
  (flet ((link (word entry)
           (when entry
             (format nil "<span>~a: <a href=\"~a\">~a</a></span>"
                     word (page-name (car entry)) (chapter-title (cdr entry))))))
    (format nil "<div class=\"node~:[~; end~]\">~@[~a~]~@[~a~]~
                 <span>Up: <a href=\"index.html\">Contents</a></span></div>"
            end (link "Previous" previous) (link "Next" next))))

(defun write-file (path text)
  "Write TEXT, a string, as the whole of the file at PATH, in UTF-8."
  (alexandria:write-string-into-file text path :if-exists :supersede :external-format :utf-8))

(defun site (contents version main &key whole)
  "The HTML of a page's layout: the contents column, named CONTENTS, and MAIN beside it, for VERSION.
The column links to the whole manual on one page, or with WHOLE back to the chapters, and holds the
button that changes between light and dark colours; a link in the corner opens the contents page."
  (format nil "<div class=\"site\">
<nav class=\"toc\" id=\"contents\" aria-label=\"Contents\">
<strong>Tuatara Manual</strong>
<small>for version ~a</small>
<p class=\"tools\">~:[<a href=\"all.html\">All on one page</a>~;<a href=\"index.html\">By chapter</a>~]
<button type=\"button\" class=\"theme\" hidden>Dark</button></p>
<input type=\"search\" placeholder=\"Find a heading\" aria-label=\"Find a heading\">
~a
</nav>
<main>
~a</main>
</div>
<a class=\"to-contents\" href=\"index.html\">Contents</a>
" version whole contents main))

(defun write-page (out title body)
  "Write the page OUT (a path) of BODY named TITLE (already escaped)."
  (write-file out (tuatara.defaults::org-html-page title body :stylesheet "manual.css" :script "manual.js"
                                                               :icon "tuatara.png")))

(defparameter *manual-style*
  "body, code, kbd, pre, input, button { font-family: \"Cascadia Code\", \"Cascadia Mono\", \"JetBrains Mono\",
  \"Fira Code\", \"Source Code Pro\", \"DejaVu Sans Mono\", ui-monospace, monospace; }
nav.contents a, .node a { text-decoration: none; }
nav.toc .tools { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 14px; margin: 0 0 14px;
  font-size: 14px; }
nav.toc .tools a { color: var(--accent); }
button.theme { font-size: 13px; padding: 2px 10px; color: var(--fg); background: var(--bg);
  border: 1px solid var(--rule); border-radius: 4px; cursor: pointer; }
.to-contents { position: fixed; right: 16px; bottom: 16px; padding: 6px 12px; font: 14px var(--head); color: var(--fg);
  background: var(--panel); border: 1px solid var(--rule); border-radius: 4px; text-decoration: none; }
section.chapter + section.chapter { border-top: 1px solid var(--rule); margin-top: 48px; padding-top: 8px; }
@media (min-width: 761px) {
  nav.toc { position: sticky; top: 0; align-self: start; max-height: 100vh; overflow-y: auto; box-sizing: border-box;
    border-right: 0; }
  .site main { border-left: 1px solid var(--rule); }
}
@media print {
  nav.toc, .node, .to-contents { display: none; }
  .site { display: block; max-width: none; }
  a { color: inherit; text-decoration: none; }
  section.chapter + section.chapter { break-before: page; border-top: 0; margin-top: 0; }
}
"
  "The manual's own rules after the export's stylesheet: every text in a monospaced font, the first of
these the reader has, as the pages load no font from elsewhere; links in the contents not underlined;
the contents column kept in view on a wide screen, a link to the contents page in the corner, the
colour button; and on paper the text alone, each chapter of the whole manual from a new page.")

(defparameter *one-page-ids* (cl-ppcre:create-scanner "\\b(id|href)=\"(#?)([^\"]*)\"")
  "An id or href attribute of a chapter's HTML, with any # its value begins with.")

(defun on-one-page (html &optional stem)
  "HTML, a chapter's body (STEM its page's name) or the contents (no STEM), with its ids and links
made fit for the whole manual on one page: each id and link within the page given STEM and two
hyphens in front, as the chapters share names of headings; a link to a chapter's page made a link
to that chapter's section, and to a heading in it, to that heading there."
  (cl-ppcre:regex-replace-all
   *one-page-ids* html
   (lambda (match attribute hash value)
     (declare (ignore match))
     (multiple-value-bind (whole parts)
         (cl-ppcre:scan-to-strings "^([0-9]+-[^/#.]*)\\.html(?:#(.*))?$" value)
       (cond (whole
              (format nil "href=\"#~a~@[--~a~]\"" (aref parts 0) (aref parts 1)))
             ((and stem (string= attribute "id"))
              (format nil "id=\"~a--~a\"" stem value))
             ((and stem (string= hash "#"))
              (format nil "href=\"#~a--~a\"" stem value))
             (t (format nil "~a=\"~a~a\"" attribute hash value)))))
   :simple-calls t))

(defparameter *modal-editing-note*
  (concatenate 'string "This manual is written with modal editing on, first and foremost. "
               "Where a command also has a key that works with modal editing off, it is given as well, "
               "as the non-modal key.")
  "The sentences that open the manual, on its index page and at the start of its PDF, saying which way of
editing it is written for.")

(defun whole-page (entries contents version)
  "The page of the whole manual, every chapter of ENTRIES in turn, CONTENTS its contents list, for
VERSION: to print at once or search through with a browser's find."
  (site (on-one-page contents) version
        (format nil "<article>~%<h1>Tuatara Manual</h1>~%<p class=\"lead\">for version ~a, every chapter on ~
                     one page</p>~%<p>~a</p>~%~{~a~}</article>~%"
                version *modal-editing-note*
                (loop for (chapter . parts) in entries
                      collect (format nil "<section class=\"chapter\" id=\"~a\">~%~a</section>~%"
                                      (getf chapter :stem)
                                      (on-one-page (getf parts :body) (getf chapter :stem)))))
        :whole t))

(defun run-html (tree)
  "Write the manual under TREE as web pages to build/manual/: a page for each
chapter, index.html, all.html with every chapter on one page, and the
stylesheet, nothing loaded from elsewhere."
  (let* ((out (merge-pathnames "build/manual/" tree))
         (version (asdf:component-version (asdf:find-system :tuatara)))
         (chapters (chapter-texts tree))
         (entries (mapcar (lambda (chapter) (cons chapter (parts-of chapter))) chapters))
         (contents (contents-list entries)))
    (ensure-directories-exist out)
    (write-file (merge-pathnames "manual.css" out)
                (concatenate 'string (tuatara.defaults::org-html-stylesheet :chosen t) *manual-style*))
    (uiop:copy-file (merge-pathnames "doc/manual/manual.js" tree) (merge-pathnames "manual.js" out))
    (uiop:copy-file (merge-pathnames "data/icons/hicolor/32x32/apps/tuatara.png" tree)
                    (merge-pathnames "tuatara.png" out))
    (loop for (previous entry next) on (cons nil entries)
          while entry
          do (destructuring-bind (chapter . parts) entry
               (write-page (merge-pathnames (page-name chapter) out)
                           (format nil "~a &ndash; Tuatara Manual" (getf parts :title))
                           (site (here-marked contents (getf chapter :stem)) version
                                 (format nil "<article>~%~a~%~a~a~%</article>~%"
                                         (node-links previous next) (getf parts :body)
                                         (node-links previous next :end t))))))
    (write-page (merge-pathnames "index.html" out) "Tuatara Manual"
                (site contents version
                      (format nil "<article>~%<h1>Tuatara Manual</h1>~%<p class=\"lead\">for version ~a</p>~%~
                                   <p>~a</p>~%<nav class=\"contents\">~a</nav>~%</article>~%"
                              version *modal-editing-note* contents)))
    (write-page (merge-pathnames "all.html" out) "Tuatara Manual, all chapters" (whole-page entries contents version))
    (format t "~&~d chapters in ~a~%" (length chapters) (namestring out))))

(defparameter *pdf-front-matter*
  (format nil "#+title: Tuatara Manual~%#+options: author:nil date:nil toc:2 num:t H:4~%#+template: article~%~
               #+latex_header: \\usepackage[most]{tcolorbox}~%~
               #+latex_header: \\newenvironment{try}{\\begin{tcolorbox}~
               [breakable,enhanced,colback=black!4,boxrule=0pt,arc=0pt]}{\\end{tcolorbox}}~%~
               #+latex_header: \\usepackage{etoolbox}~%~
               #+latex_header: \\pretocmd{\\section}{\\clearpage}{}{}~%~%~a~%~%" *modal-editing-note*)
  "What begins the whole manual as one Org document, for its PDF: the title, a
page for each chapter, and the look of a box to try things in.")

(defun chapter-file-links (chapters)
  "A list of (FILE-NAME . TITLE) for CHAPTERS: each chapter's file and its first heading."
  (loop for chapter in chapters
        collect (cons (format nil "~a.org" (getf chapter :stem))
                      (nth-value 1 (cl-ppcre:scan-to-strings "(?m)^\\* (.*)$" (getf chapter :text))))))

(defparameter *longest-table* 40
  "The most rows of one table in the PDF: LaTeX measures a table whole, and its
height may not pass 16000 points, so a table of long rows cannot be many:
every command and every setting are many tables.")

(defun table-chunks (rows)
  "ROWS, the lines of one table, as tables of no more than *LONGEST-TABLE* rows
each, every one under the heading the first has, separated by a blank line."
  (let* ((head (subseq rows 0 (min 2 (length rows))))
         (body (nthcdr (length head) rows)))
    (format nil "~{~{~a~^~%~}~^~%~%~}"
            (loop while body
                  collect (append head (subseq body 0 (min *longest-table* (length body))))
                  do (setf body (nthcdr (min *longest-table* (length body)) body))))))

(defun long-tables-split (text)
  "TEXT with each table of more rows than *LONGEST-TABLE* split up (TABLE-CHUNKS)."
  (let ((out '()) (table '()))
    (flet ((flush ()
             (when table
               (let ((rows (reverse table)))
                 (push (if (> (length rows) (+ 2 *longest-table*))
                           (table-chunks rows)
                           (format nil "~{~a~^~%~}" rows))
                       out))
               (setf table '()))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (if (uiop:string-prefix-p "|" line)
            (push line table)
            (progn (flush) (push line out))))
      (flush))
    (format nil "~{~a~^~%~}" (reverse out))))

(defun whole-chapter (chapter links)
  "CHAPTER's text as a part of the whole manual: no #+title: or #+language: of its
own, a box's title its first line in bold, links between chapters made links
to the chapter's heading, LINKS as CHAPTER-FILE-LINKS gives them."
  (let* ((text (cl-ppcre:regex-replace-all "(?mi)^#\\+(?:title|language):[^\\n]*\\n" (getf chapter :text) ""))
         (text (cl-ppcre:regex-replace-all "(?mi)^#\\+begin_try[ \\t]+([^\\n]*)$" text
                                           (format nil "#+begin_try~%*\\1*")))
         (text (cl-ppcre:regex-replace-all "\\[\\[file:([0-9]+-[^]]*\\.org)\\]\\[" text
                                           (lambda (match file)
                                             (declare (ignore match))
                                             (let ((title (cdr (assoc file links :test #'string=))))
                                               (if title
                                                   (format nil "[[*~a][" (aref title 0))
                                                   (format nil "[[file:~a][" file))))
                                           :simple-calls t)))
    (long-tables-split text)))

(defun run-pdf (tree)
  "Write the manual under TREE as one LaTeX document, build/manual/tuatara-manual.tex,
through the enhanced Org export, xelatex's to make into a PDF."
  (let* ((chapters (chapter-texts tree))
         (links (chapter-file-links chapters))
         (out (merge-pathnames "build/manual/tuatara-manual.tex" tree))
         (note (merge-pathnames "doc/manual/generated/tuatara-manual.org" tree))
         (whole (format nil "~a~{~a~^~%~}" *pdf-front-matter*
                        (mapcar (lambda (chapter) (whole-chapter chapter links)) chapters))))
    (ensure-directories-exist out)
    (write-file note whole)
    (multiple-value-bind (latex named map missing notes)
        (tuatara.defaults::org-latex-document whole :file (namestring note) :enhanced t)
      (declare (ignore named map))
      (write-file out latex)
      (dolist (found missing)
        (format t "~&No font here has ~a (U+~4,'0X), first on line ~d~%"
                (car found) (char-code (car found)) (cdr found)))
      (dolist (said notes)
        (format t "~&~a~%" said)))
    (format t "~&~a~%" (namestring out))))
