;;;; test/office-reading-limits.lisp --- Office files and LaTeX documents read and imported within bounds.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (template-import docx-part-names-any-case)
  ;; Word reads part names in any case: comments named Comments.xml are left out as comments.xml are
  (with-test-directory (dir)
    (let* ((parts (mapcar (lambda (part)
                            (if (string= (car part) "word/comments.xml") (cons "word/Comments.xml" (cdr part)) part))
                          (docx-report-parts)))
           (source (write-docx-parts (merge-pathnames "report.docx" dir) parts)))
      (multiple-value-bind (made words) (import-docx-into dir source "cased")
        (declare (ignore made))
        (true (member "its comments left out" words :test #'string=) "~s" words)
        (false (probe-file (merge-pathnames "t/cased/word/Comments.xml" dir)))
        (false (probe-file (merge-pathnames "t/cased/word/comments.xml" dir)))))))

(define-test (template-import docx-two-parts-one-name-refused)
  ;; two parts whose names differ only in case: nothing made
  (with-test-directory (dir)
    (let* ((parts (append (docx-report-parts)
                          (list (cons "word/Settings.xml" "<w:settings xmlns:w='x'/>"))))
           (source (write-docx-parts (merge-pathnames "twice.docx" dir) parts)))
      (true (search "one part to Word"
                    (handler-case (progn (import-docx-into dir source "twice") "")
                      (error (e) (princ-to-string e))))))))

(define-test (docx-view template-directory-links-not-followed)
  ;; a link in a Word template's directory, to a file or a directory outside it, is never read
  (with-test-directory (dir)
    (let ((template (merge-pathnames "tpl/" dir))
          (secret (namestring (merge-pathnames "secret.txt" dir)))
          (away (merge-pathnames "away/" dir)))
      (ensure-directories-exist (merge-pathnames "word/" template))
      (ensure-directories-exist away)
      (alexandria:write-string-into-file "private words" secret)
      (alexandria:write-string-into-file "private too" (merge-pathnames "inside.xml" away))
      (alexandria:write-string-into-file "<w:document/>" (merge-pathnames "word/document.xml" template))
      (sb-posix:symlink secret (namestring (merge-pathnames "word/header1.xml" template)))
      (sb-posix:symlink (string-right-trim "/" (namestring away)) (namestring (merge-pathnames "word/media" template)))
      (let ((names (mapcar #'car (tuatara.defaults::docx-read-directory (namestring template)))))
        (is equal '("word/document.xml") names)))))

(define-test (docx-view look-rows-held-to-a-list)
  ;; the pages table, like every table of a document's look, shows no more rows than a list does
  (with-test-directory (dir)
    (let ((tuatara.defaults::*docx-reading*
            (tuatara.defaults::docx-reading-of (write-docx-report (merge-pathnames "r.docx" dir))))
          (tuatara.defaults::*office-most-listed* 2))
      (let* ((body (tuatara.text:xml-parse (format nil "<w:body>~{~a~}</w:body>"
                                                   (make-list 5 :initial-element "<w:sectPr/>"))))
             (text (with-output-to-string (s) (tuatara.defaults::docx-read-pages s body))))
        (is = 2 (count-line-matches "^\\| \\d" text))
        (true (search "And 3 sections more" text) "~a" text)))))

(defun count-line-matches (regex text)
  "How many lines of TEXT REGEX matches the start of."
  (length (cl-ppcre:all-matches-as-strings (concatenate 'string "(?m)" regex) text)))

(define-test (office-view odf-trailing-empty-rows-in-time)
  ;; many empty rows at a table's end are left out at once, not one at a time
  (with-test-directory (dir)
    (let* ((rows (with-output-to-string (s)
                   (dotimes (i 1200)
                     (write-string "<table:table-row table:number-rows-repeated='64'><table:table-cell/></table:table-row>"
                                   s))))
           (path (write-office-test-file
                  (merge-pathnames "rows.odt" dir)
                  (odf-test-parts "text" (format nil "<table:table><table:table-row><table:table-cell><text:p>Kea</text:p></table:table-cell></table:table-row>~a</table:table>" rows))))
           (started (get-internal-real-time)))
      (note-has (tuatara.defaults::odf-read-note path) "| Kea |")
      (true (< (- (get-internal-real-time) started) (* 2 internal-time-units-per-second))))))

(define-test (office-view odf-names-by-namespace)
  ;; text whose namespace is bound to a prefix of its own is read as its program shows it
  (with-test-directory (dir)
    (let ((path (write-office-test-file
                 (merge-pathnames "prefix.odt" dir)
                 (odf-test-parts "text" (concatenate 'string
                                                     "<text:section xmlns:t='urn:oasis:names:tc:opendocument:xmlns:text:1.0'>"
                                                     "<t:p>Hidden kea</t:p></text:section>")))))
      (note-has (tuatara.defaults::odf-read-note path) "Hidden kea"))))

(define-test (office-view odf-notes-take-room)
  ;; a footnote's text is held to a value's length, as a cell's is
  (with-test-directory (dir)
    (let* ((long (make-string 5000 :initial-element #\k))
           (path (write-office-test-file
                  (merge-pathnames "note.odt" dir)
                  (odf-test-parts "text" (format nil "<text:p>Words<text:note text:note-class='footnote'><text:note-body><text:p>~a</text:p></text:note-body></text:note></text:p>" long))))
           (note (tuatara.defaults::odf-read-note path)))
      (false (search long note)))))

(define-test (template-import latex-verbatim-kept-in-its-block)
  ;; an indented #+end_example in imported verbatim text cannot end the example block
  (with-test-directory (dir)
    (let ((source (namestring (merge-pathnames "v.tex" dir)))
          (tex (namestring (merge-pathnames "out/v.tex" dir)))
          (tuatara.defaults::*note-templates-directory* (namestring (merge-pathnames "nt/" dir))))
      (alexandria:write-string-into-file
       (format nil "\\documentclass{article}~%\\begin{document}~%\\section{Text}~%\\begin{verbatim}~%  #+end_example~%* Forged~%\\end{verbatim}~%\\end{document}~%")
       source)
      (let ((note (alexandria:read-file-into-string (tuatara.defaults::latex-import source "v" :tex tex))))
        (true (search "  ,#+end_example" note) "~a" note)
        (true (search ",* Forged" note) "~a" note)))))

(define-test (template-import latex-group-ends-kept-to-an-import)
  ;; the ends of a document's groups are an import's own, never left behind for another thread
  (with-test-directory (dir)
    (let ((source (namestring (merge-pathnames "g.tex" dir)))
          (tex (namestring (merge-pathnames "out/g.tex" dir)))
          (tuatara.defaults::*note-templates-directory* (namestring (merge-pathnames "nt/" dir)))
          (kept tuatara.defaults::*latex-group-ends*))
      (alexandria:write-string-into-file
       (format nil "\\documentclass{article}~%\\begin{document}~%\\section{Kea}~%\\textbf{Kea}~%\\end{document}~%")
       source)
      (true (plusp (nth-value 1 (counting-calls (tuatara.defaults::latex-group-ends)
                                  (tuatara.defaults::latex-import source "g" :tex tex)))))
      (is eq kept tuatara.defaults::*latex-group-ends*))))

(define-test (latex-export plain-verbatim-holds-its-end)
  ;; a code block holding the line \end{verbatim} is written so that the environment does not end there
  (let ((written (tuatara.defaults::org-latex-plain-verbatim (format nil "a~%\\end{verbatim}~%b"))))
    (is = 1 (count-line-matches "^\\\\end\\{verbatim\\}$" written))
    (true (search "\\texttt{\\textbackslash{}end\\{verbatim\\}}" written) "~a" written)))

(define-test (latex-export setup-files-read-once-each)
  ;; setup files naming each other are read once each, not once for each path through them
  (with-test-directory (dir)
    (let ((names (loop for i below 6 collect (format nil "s~d.org" i)))
          (tuatara.defaults::*note-templates-directory* (namestring dir)))
      (dolist (name names)
        (alexandria:write-string-into-file
         (format nil "~{#+SETUPFILE: ~a~%~}#+KEY: ~a~%" (remove name names :test #'string=) name)
         (merge-pathnames name dir)))
      (multiple-value-bind (keywords reads)
          (counting-calls (tuatara.defaults::org-export-read-setup)
            (tuatara.defaults::org-export-collect-keywords (tuatara.defaults::org-parse "#+SETUPFILE: s0.org")
                                                           (namestring dir)))
        (is = 6 (count "KEY" keywords :key #'car :test #'string=))
        (true (<= reads 40) "~d reads" reads)))))

(define-test (latex-export missing-characters-listed-once)
  ;; a character no font has is looked for and listed once, however often the text holds it
  (let ((tuatara.defaults::*org-export* (tuatara.defaults::make-org-latex "" nil nil))
        (body (make-string 500 :initial-element (code-char #xE000))))
    (setf (tuatara.defaults::org-latex-fonts tuatara.defaults::*org-export*) '((:main . "No Such Font Tuatara")))
    (multiple-value-bind (missing looks)
        (counting-calls (tuatara.defaults::font-for-character :stub nil)
          (nth-value 1 (tuatara.defaults::org-latex-font-plan body nil)))
      (is = 1 (length missing))
      (is = 1 looks))))

(define-test (template-import docx-fetched-pictures-all-noted)
  ;; a picture fetched from elsewhere is noted whatever its address says, however its attributes are spaced,
  ;; and a field fetching a file is noted in any case
  (with-test-directory (dir)
    (let* ((parts (list* (cons "word/_rels/header1.xml.rels"
                               (format nil "<Relationships><Relationship~cId=\"r1\" Type=\"http://x/image\" Target=\"http://tracker.example/hyperlink\" TargetMode = \"External\"/></Relationships>" #\Tab))
                         (cons "word/footer1.xml" "<w:ftr><w:instrText>includepicture \"http://spy.example/p.png\"</w:instrText></w:ftr>")
                         (remove-if (lambda (part) (member (car part) '("word/_rels/header1.xml.rels" "word/footer1.xml")
                                                           :test #'string=))
                                    (docx-report-parts))))
           (source (write-docx-parts (merge-pathnames "fetch.docx" dir) parts))
           (tuatara.defaults::*docx-reading* (tuatara.defaults::docx-reading-of source))
           (tuatara.defaults::*template-import* (tuatara.defaults::make-template-import)))
      (tuatara.defaults::docx-import-note-fetched (mapcar #'car parts))
      (let ((notes (format nil "~{~a~%~}" (tuatara.defaults::template-import-notes
                                           tuatara.defaults::*template-import*))))
        (true (search "tracker.example" notes) "~a" notes)
        (true (search "spy.example" notes) "~a" notes)))))

(define-test (latex-export bibliography-outside-left-out)
  ;; a bibliography named without .bib, outside the directories an export reads, is left out
  (with-test-directory (dir)
    (with-test-directory (away)
      (let ((note (namestring (merge-pathnames "note.org" dir)))
            (refs (namestring (merge-pathnames "refs" away))))
        (alexandria:write-string-into-file "" (concatenate 'string refs ".bib"))
        (let ((text (org-latex-of (format nil "#+cite_export: natbib~%#+bibliography: ~a~%* One~%[cite:@kiwi]~%~%#+print_bibliography:~%" refs)
                                  :file note)))
          (false (search refs text) "~a" text))))))

(define-test (template-import docx-header-known-by-its-relation)
  ;; a header part of any name, known by its relation to the text, has its active fields made text
  (with-test-directory (dir)
    (let* ((header "<w:hdr xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:p><w:fldSimple w:instr=\" DDEAUTO excel x\"><w:r><w:t>kept</w:t></w:r></w:fldSimple></w:p></w:hdr>")
           (parts (loop for (name . text) in (docx-report-parts)
                        collect (cond ((string= name "word/header1.xml") (cons "word/hdra.xml" header))
                                      ((string= name "word/_rels/document.xml.rels")
                                       (cons name (cl-ppcre:regex-replace "header1\\.xml" text "hdra.xml")))
                                      (t (cons name text)))))
           (source (write-docx-parts (merge-pathnames "named.docx" dir) parts)))
      (import-docx-into dir source "named")
      (let ((written (alexandria:read-file-into-string (merge-pathnames "t/named/word/hdra.xml" dir))))
        (false (search "DDEAUTO" written) "~a" written)
        (true (search "kept" written))))))
