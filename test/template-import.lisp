;;;; test/template-import.lisp --- Word and LaTeX documents given to fill in made templates and note templates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test template-import
  :description "A Word or LaTeX document given to fill in is made a template, each field a place and its
text {{{body}}}, and a note template: its fields as keywords with comments, empty for placeholder text,
%date% for a date, then its text as Org with headings, lists, tables, pictures and footnotes. A Word
document's macros, comments and tracked changes are left out and what it fetches from elsewhere is said; a
LaTeX command taking in a file outside the template is made a comment. A picture outside the document's
folder is never taken, a part named to land outside the template refused, nothing is written beside the
document or over what exists, and a Tuatara template is taken as it is.")

(defparameter *survey-latex*
  "\\documentclass{article}
\\usepackage{graphicx}
\\newcommand{\\refnumber}{REF-XXXX} % the reference
\\newcommand{\\ownerName}{The Office}
\\newcommand{\\R}[1]{#1}
\\title{Site Survey}
\\author{A. Writer}
\\date{\\today}
\\begin{document}
\\maketitle
\\begin{tabular}{ll}
Version: & 0.1 \\\\
Reviewer: & [Enter reviewer name] \\\\
\\end{tabular}

Made for: The Office
\\section{Findings}
Some \\textbf{bold} and \\emph{italic} words, 50\\% done.\\footnote{A note.}
\\begin{itemize}
\\item First
\\item Second
\\end{itemize}
\\begin{table}
\\caption{Counts}
\\begin{tabular}{lr}
\\hline
Bird & Count \\\\
\\hline
Kea & 3 \\\\
\\end{tabular}
\\end{table}
\\includegraphics[width=3cm]{logo}
\\includegraphics{../secret.png}
\\subsection*{Scope}
\\section{Conclusion}
\\appendix
\\section{Terms}
Words.
\\end{document}
"
  "A LaTeX document to fill in, with each kind of field and text the import reads.")

(define-test (template-import latex-made-template)
  (with-test-directory (dir)
    (let* ((given (merge-pathnames "given/" dir))
           (source (namestring (merge-pathnames "survey.tex" given)))
           (templates (namestring (merge-pathnames "note-templates/" dir)))
           (tex (namestring (merge-pathnames "survey-template.tex" dir)))
           (tuatara.defaults::*note-templates-directory* templates))
      (ensure-directories-exist given)
      (alexandria:write-string-into-file *survey-latex* source)
      (alexandria:write-string-into-file "not really a picture" (merge-pathnames "logo.png" given))
      (alexandria:write-string-into-file "private" (merge-pathnames "secret.png" dir))
      (multiple-value-bind (made words) (tuatara.defaults::latex-import source "survey" :tex tex)
        (is string= (concatenate 'string templates "survey.org") made)
        (true (search "fields (" (first words)))
        (let ((note (alexandria:read-file-into-string made))
              (template (alexandria:read-file-into-string tex))
              (media (concatenate 'string templates "survey-media/")))
          (flet ((has (text in) (true (search text in) "~s in~%~a" text in)))
            ;; the template: each field a place, the text {{{body}}}, the note's packages loaded unless loaded
            (has "\\newcommand{\\refnumber}{{{{refnumber}}}}" template)
            (has "\\newcommand{\\ownerName}{{{{owner_name}}}}" template)
            (has "\\newcommand{\\R}[1]{#1}" template)
            (has "\\title{{{{title}}}}" template)
            (has "\\date{{{{date}}}}" template)
            (has "Version: & {{{version}}}" template)
            (has "Reviewer: & {{{reviewer}}}" template)
            (has "Made for: {{{made_for}}}" template)
            (has "{{{body}}}" template)
            (has "{{{packages}}}" template)
            (has "\\@ifpackageloaded" template)
            (false (search "Findings" template))
            ;; the note template: its fields as keywords
            (has "#+title: Site Survey" note)
            (has "#+template: survey" note)
            (has "redact:nil protect:nil" note)
            (has (format nil "# Refnumber, as REF-XXXX~%#+refnumber:~%") note)
            (has "#+owner_name: The Office" note)
            (has "#+date: %date%" note)
            (has "#+version: 0.1" note)
            (has (format nil "# Reviewer~%#+reviewer:~%") note)
            (has "#+made_for: The Office" note)
            ;; its text as Org
            (has "* Findings" note)
            (has "*bold*" note)
            (has "/italic/" note)
            (has "50% done.[fn:1]" note)
            (has "[fn:1] A note." note)
            (has "- First" note)
            (has "#+caption: Counts" note)
            (has (format nil "| Bird | Count |~%|-~%| Kea | 3 |") note)
            (has (format nil "[[file:~a]]" (tuatara.command:abbreviated-path (concatenate 'string media "logo.png"))) note)
            (has "[picture ../secret.png]" note)
            (has (format nil "** Scope~%:EXPORT:~%numbered: no~%:END:~%# Write the scope.") note)
            (has (format nil "* Conclusion~%# Write the conclusion.~%~%* Terms") note)
            (has "* Terms :appendix:" note)
            ;; the picture in its folder saved, the one outside it not; nothing beside the document
            (true (probe-file (concatenate 'string media "logo.png")))
            (is = 1 (length (tuatara.io:directory-entries media)))
            (is equal '("logo.png" "survey.tex") (sort (mapcar #'first (tuatara.io:directory-entries (namestring given))) #'string<))))
        ;; written over by none: a second import refused
        (fail (tuatara.defaults::latex-import source "survey" :tex tex))))))

(define-test (template-import latex-own-template)
  (with-test-directory (dir)
    (let* ((source (namestring (merge-pathnames "mine.tex" dir)))
           (tuatara.defaults::*note-templates-directory* (namestring (merge-pathnames "nt/" dir)))
           (tex (namestring (merge-pathnames "out/mine.tex" dir))))
      ;; a Tuatara template already: taken as it is, its places the fields
      (alexandria:write-string-into-file (format nil "\\documentclass{article}~%{{{packages}}}~%\\begin{document}~%Owner: {{{owner}}}~%{{{body}}}~%\\end{document}~%") source)
      (let ((note (alexandria:read-file-into-string (tuatara.defaults::latex-import source "mine" :tex tex))))
        (true (search (format nil "# Owner~%#+owner:~%") note))
        (false (search "#+packages" note))
        (is string= (alexandria:read-file-into-string source) (alexandria:read-file-into-string tex)))
      ;; groups within groups a hundred thousand deep: read in a moment, kept as written
      (alexandria:write-string-into-file (format nil "\\begin{document}~%\\section{Deep}~%~a~a~%\\end{document}~%"
                                                 (make-string 100000 :initial-element #\{) (make-string 100000 :initial-element #\}))
                                         (merge-pathnames "deep.tex" dir))
      (true (tuatara.defaults::latex-import (namestring (merge-pathnames "deep.tex" dir)) "deep" :tex (namestring (merge-pathnames "out/deep.tex" dir))))
      ;; no document in it: refused, nothing made
      (alexandria:write-string-into-file "\\section{Only a part}" (merge-pathnames "part.tex" dir))
      (fail (tuatara.defaults::latex-import (namestring (merge-pathnames "part.tex" dir)) "part" :tex (namestring (merge-pathnames "out/part.tex" dir))))
      (false (probe-file (merge-pathnames "out/part.tex" dir))))))

(define-test (template-import placeholder-values)
  ;; what only shows where a value goes: the note template's keyword left empty
  (dolist (text '("REF-XXXX-20XX" "X X" "XXXX-XX-XX" "[Enter name]" "«Date»" "<Version>" "..." "___" ""))
    (true (tuatara.defaults::template-placeholder-p text) "~s" text))
  (dolist (text '("Request for data" "0.3" "XXL shirts" "Max" "The Office"))
    (false (tuatara.defaults::template-placeholder-p text) "~s" text)))

(define-test (template-import tagged-fields)
  (with-test-directory (dir)
    (let ((source (namestring (merge-pathnames "tagged.tex" dir)))
          (tuatara.defaults::*note-templates-directory* (namestring (merge-pathnames "nt/" dir))))
      ;; fields tagged before the import: each a keyword, named as tagged, a date's pattern kept
      (alexandria:write-string-into-file
       (format nil "\\documentclass{article}~%\\begin{document}~%Reference: {{{owner_ref}}}\\\\~%Due {{{due:date=d MMMM yyyy}}}~%\\section{Text}~%Words.~%\\end{document}~%")
       source)
      (let* ((tex (namestring (merge-pathnames "out/tagged.tex" dir)))
             (note (alexandria:read-file-into-string (tuatara.defaults::latex-import source "tagged" :tex tex))))
        (true (search (format nil "# Owner ref~%#+owner_ref:~%") note))
        (true (search (format nil "# Due, a date, as 2026-10-05; the document writes it d MMMM yyyy~%#+due: %date%") note))
        (true (search "* Text" note))
        (true (search "{{{due:date=d MMMM yyyy}}}" (alexandria:read-file-into-string tex)))))))

(define-test (template-import latex-outside-files)
  (with-test-directory (dir)
    (let ((source (namestring (merge-pathnames "outside.tex" dir)))
          (tex (namestring (merge-pathnames "out/outside.tex" dir)))
          (tuatara.defaults::*note-templates-directory* (namestring (merge-pathnames "nt/" dir))))
      ;; a file taken in from a home folder: a comment in the template made, and said
      (alexandria:write-string-into-file
       (format nil "\\documentclass{article}~%\\usepackage{graphicx}~%\\includegraphics{~~/x.png}~%\\begin{document}~%\\section{Text}~%Words.~%\\end{document}~%")
       source)
      (multiple-value-bind (made words) (tuatara.defaults::latex-import source "outside" :tex tex)
        (declare (ignore made))
        (let ((template (alexandria:read-file-into-string tex)))
          (true (search (format nil "~%%\\includegraphics{~~/x.png} (left out on import") template) "~a" template)
          (false (search (format nil "~%\\includegraphics{~~/x.png}") template)))
        (true (member "1 command taking in files outside it (~/..., /... or ../...) made comments" words :test #'string=) "~s" words)))))

(define-test (template-import docx-made-template)
  (with-test-directory (dir)
    (let* ((source (write-docx-report (merge-pathnames "report.docm" dir)))
           (folder (namestring (merge-pathnames "pair/" dir)))
           (tex (namestring (merge-pathnames "pair.tex" dir)))
           (note-path (namestring (merge-pathnames "note.org" dir))))
      (multiple-value-bind (made words) (tuatara.defaults::docx-import source "pair" :folder folder :tex tex :note note-path)
        (is string= note-path made)
        (is string= "6 fields (issue_date, version, reviewer, title, made_for, plan_code)" (first words))
        (true (member "its macros left out" words :test #'string=))
        (true (member "its comments left out" words :test #'string=)))
      (let ((note (alexandria:read-file-into-string note-path))
            (document (alexandria:read-file-into-string (merge-pathnames "pair/word/document.xml" dir)))
            (design (alexandria:read-file-into-string (merge-pathnames "pair/tuatara.txt" dir)))
            (latex (alexandria:read-file-into-string tex)))
        (flet ((has (text in) (true (search text in) "~s in~%~a" text in)))
          ;; the template: its tracked changes accepted, no reference left to the comments left out
          (has "Kept words" document)
          (false (search "Struck words" document))
          (false (search "<w:ins" document))
          (false (search "commentReference" document))
          (false (search "commentRange" document))
          ;; the note: a keyword for each field, what kind of value, the text to write over
          (has "#+template: pair" note)
          (has "#+version: 0.3" note)
          (has (format nil "#+reviewer:~%") note)
          (false (search "#+reviewer: [Enter" note))
          (has (format nil "# Issue date, a date, as 2026-10-05; the document writes it d MMMM yyyy~%#+issue_date: %date%") note)
          (has "redact:nil protect:nil" note)
          (has "#+made_for: The Office" note)
          (has "* Findings" note)
          (has "1. First site" note)
          (has (format nil "#+caption: Counts~%| Both |") note)
          (false (search "Check" note))
          ;; the Word half: its cover kept, its fields places, its text {{{body}}}, the contents {{{contents}}}
          (has "{{{version}}}" document)
          (has "{{{issue_date}}}" document)
          (has "{{{contents}}}" document)
          (has "{{{body}}}" document)
          (false (search "Findings" document))
          (false (search "showingPlcHdr" document))
          (has "{{{plan_code}}}" (alexandria:read-file-into-string (merge-pathnames "pair/word/header1.xml" dir)))
          (false (probe-file (merge-pathnames "pair/word/vbaProject.bin" dir)))
          (false (probe-file (merge-pathnames "pair/word/comments.xml" dir)))
          (false (search "trackRevisions" (alexandria:read-file-into-string (merge-pathnames "pair/word/settings.xml" dir))))
          (false (search "macroEnabled" (uiop:read-file-string (uiop:parse-native-namestring (namestring (merge-pathnames "pair/[Content_Types].xml" dir))))))
          (has "table style: BlueGrid" design)
          (has "numbers: 5" design)
          ;; the LaTeX half: its page, its places, its header
          (has "paperwidth=21.00cm" latex)
          (has "{{{body}}}" latex)
          (has "{{{version}}}" latex)
          (has "\\fancyhead[R]{Survey {{{plan_code}}} page \\thepage{}}" latex)
          ;; a date field's place in the LaTeX half keeps the document's pattern
          (has "{{{issue_date:date=d MMMM yyyy}}}" latex)))
      ;; written over by none: a second import refused
      (fail (tuatara.defaults::docx-import source "pair" :folder folder :tex tex :note note-path)))))

(define-test (template-import docx-note-and-pictures)
  (with-test-directory (dir)
    (let* ((w *docx-report-w*)
           (given (merge-pathnames "given/" dir))
           (templates (namestring (merge-pathnames "note-templates/" dir)))
           (source (namestring (merge-pathnames "form.docx" given)))
           (tuatara.defaults::*note-templates-directory* templates))
      (ensure-directories-exist given)
      (with-open-file (out source :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets
                         `(("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:p><w:r><w:t>Doc: {{{doc_ref}}}</w:t></w:r></w:p><w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>Request</w:t></w:r></w:p><w:p><w:r><w:drawing><wp:inline><wp:docPr id=\"1\" name=\"Logo\"/><a:graphic><a:graphicData><a:blip r:embed=\"rId9\"/></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p></w:body></w:document>" w))
                           ("word/_rels/document.xml.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId9\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/image\" Target=\"media/image1.png\"/></Relationships>")
                           ("word/media/image1.png" . "not really a picture")))
                        out))
      ;; the note template in the note templates' folder, its picture beside it, linked by its whole path
      (let ((made (tuatara.defaults::docx-import source "form" :folder (namestring (merge-pathnames "form/" dir))
                                                               :tex (namestring (merge-pathnames "form.tex" dir))))
            (picture (concatenate 'string templates "form-media/image1.png")))
        (is string= (concatenate 'string templates "form.org") made)
        (true (probe-file picture))
        (true (search (format nil "[[file:~a]]" (tuatara.command:abbreviated-path picture)) (alexandria:read-file-into-string made)))
        (true (search "#+template: form" (alexandria:read-file-into-string made)))
        ;; a field tagged before the import a keyword of its own
        (true (search (format nil "# Doc ref~%#+doc_ref:~%") (alexandria:read-file-into-string made)))
        ;; nothing written beside the document given
        (is equal '("form.docx") (mapcar #'first (tuatara.io:directory-entries (namestring given))))
        ;; offered for each new note, and its picture taken into an export of a note kept anywhere
        (true (find "form" (tuatara.defaults::formatted-templates) :key #'first :test #'equal))
        (true (tuatara.defaults::org-export-file-allowed-p picture))
        (false (tuatara.defaults::org-export-file-allowed-p source))))))

(defun wait-for-buffer (name &optional (tries 1000))
  "A turn waiting, without holding up the editor, until a buffer called NAME
is there or TRIES turns have gone."
  (lambda ()
    (unless (or (tuatara.buffer:find-buffer name) (<= tries 1))
      (sleep 0.01)
      (tuatara.frontend.fake:fake-queue-action (tuatara.command:editor-frontend tuatara.command:*editor*)
                                               (wait-for-buffer name (1- tries))))))

(define-test (template-import docx-made-by-command)
  (with-test-directory (dir)
    (let* ((source (namestring (write-docx-report (ensure-directories-exist (merge-pathnames "given/report.docx" dir)))))
           (templates (namestring (merge-pathnames "note-templates/" dir)))
           (fenced tuatara.defaults::*note-templates-directory*)
           (config (uiop:getenv "XDG_CONFIG_HOME")))
      ;; set, not bound: the import runs in a thread of its own
      (setf tuatara.defaults::*note-templates-directory* templates)
      (sb-posix:setenv "XDG_CONFIG_HOME" (namestring (merge-pathnames "config/" dir)) 1)
      (unwind-protect
        (multiple-value-bind (f editor)
            (run-keys (list (format nil "M-x m a k e - t e m p l a t e - f r o m - d o c x RET ~a C-j RET" (typed-keys source))
                            (wait-for-buffer "untitled"))
                      :width 120 :height 20)
          (declare (ignore f))
          (let ((note (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            ;; a new note begun from the note template, not yet saved anywhere
            (is string= "untitled" (tuatara.buffer:buffer-name note))
            (false (tuatara.buffer:buffer-file note))
            (true (search "#+template: report" (tuatara.buffer:buffer-string note)))
            (true (search "#+version: 0.3" (tuatara.buffer:buffer-string note)))
            (true (probe-file (concatenate 'string templates "report.org")))
            ;; nothing written beside the document given
            (is equal '("report.docx") (mapcar #'first (tuatara.io:directory-entries (namestring (merge-pathnames "given/" dir)))))
            (true (probe-file (merge-pathnames "config/tuatara/docx-templates/report/word/document.xml" dir)))))
        (setf tuatara.defaults::*note-templates-directory* fenced)
        (sb-posix:setenv "XDG_CONFIG_HOME" config 1)))))

(define-test (template-import docx-hostile-documents)
  (with-test-directory (dir)
    (let ((w *docx-report-w*))
      ;; parts named to land outside the template's folder: refused, nothing written anywhere
      (let ((path (namestring (merge-pathnames "slip.docx" dir))))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (tuatara.defaults::zip-octets
                           `(("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:p><w:r><w:t>Hi</w:t></w:r></w:p></w:body></w:document>" w))
                             ("../../escaped.txt" . "owned")
                             ("word/../../../escaped2.txt" . "owned")))
                          out))
        (fail (tuatara.defaults::docx-import path "slip" :folder (namestring (merge-pathnames "t/slip/" dir))
                                                         :tex (namestring (merge-pathnames "t/slip.tex" dir))
                                                         :note (namestring (merge-pathnames "slip.org" dir))))
        (false (probe-file (merge-pathnames "escaped.txt" dir)))
        (false (probe-file (merge-pathnames "t/" dir)))
        (false (probe-file (merge-pathnames "slip.org" dir))))
      ;; a folder's own entry, as zip -r makes them: nothing to refuse; a pictures' folder there already: refused
      (let ((path (namestring (merge-pathnames "folders.docx" dir))))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (tuatara.defaults::zip-octets
                           `(("word/" . "")
                             ("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:p><w:r><w:t>Hi</w:t></w:r></w:p></w:body></w:document>" w))))
                          out))
        (ensure-directories-exist (merge-pathnames "kept-media/" dir))
        (true (search "kept-media exists already"
                      (handler-case (progn (tuatara.defaults::docx-import path "kept" :folder (namestring (merge-pathnames "k/kept/" dir))
                                                                                       :tex (namestring (merge-pathnames "k/kept.tex" dir))
                                                                                       :note (namestring (merge-pathnames "kept.org" dir)))
                                           "")
                        (error (e) (princ-to-string e)))))
        (true (tuatara.defaults::docx-import path "folders" :folder (namestring (merge-pathnames "f/folders/" dir))
                                                            :tex (namestring (merge-pathnames "f/folders.tex" dir))
                                                            :note (namestring (merge-pathnames "folders.org" dir))))))))

(define-test (template-import docx-odd-bodies)
  ;; the importer on odd bodies: no body, one closing itself, an empty control
  (with-test-directory (dir)
    (flet ((imported (document)
             (let ((path (namestring (merge-pathnames "odd.docx" dir))))
               (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
                 (write-sequence (tuatara.defaults::zip-octets (list (cons "word/document.xml" document))) out))
               (let ((name (format nil "odd~d" (random 1000000))))
                 (tuatara.defaults::docx-import path name :folder (namestring (merge-pathnames (format nil "~a/" name) dir))
                                                          :tex (namestring (merge-pathnames (format nil "~a.tex" name) dir))
                                                          :note (namestring (merge-pathnames (format nil "~a.org" name) dir)))))))
      (fail (imported (format nil "<w:document ~a></w:document>" *docx-report-w*)))
      (true (imported (format nil "<w:document ~a><w:body/></w:document>" *docx-report-w*)))
      (true (imported (format nil "<w:document ~a><w:body><w:p><w:sdt><w:sdtPr><w:tag w:val=\"t\"/></w:sdtPr><w:sdtContent/></w:sdt></w:p><w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>H</w:t></w:r></w:p></w:body></w:document>" *docx-report-w*))))))

(defun docx-sectioned-parts ()
  "The test report's parts, its text in sections: upright, then a table alone
on pages turned sideways, upright again, then an appendix sideways in two
columns with margins of its own, as Word writes sections."
  (let* ((w *docx-report-w*)
         (upright "<w:headerReference w:type=\"default\" r:id=\"rId4\"/><w:pgSz w:w=\"11906\" w:h=\"16838\"/><w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" w:header=\"708\" w:footer=\"708\" w:gutter=\"0\"/><w:cols w:space=\"708\"/>")
         (sideways "<w:headerReference w:type=\"default\" r:id=\"rId4\"/><w:type w:val=\"nextPage\"/><w:pgSz w:w=\"16838\" w:h=\"11906\" w:orient=\"landscape\"/><w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" w:header=\"708\" w:footer=\"708\" w:gutter=\"0\"/><w:cols w:space=\"708\"/>")
         (appendix "<w:headerReference w:type=\"default\" r:id=\"rId4\"/><w:type w:val=\"nextPage\"/><w:pgSz w:w=\"16838\" w:h=\"11906\" w:orient=\"landscape\"/><w:pgMar w:top=\"1134\" w:right=\"1134\" w:bottom=\"1134\" w:left=\"1134\" w:header=\"567\" w:footer=\"567\" w:gutter=\"0\"/><w:cols w:num=\"2\" w:space=\"567\"/>"))
    (flet ((p (text &key style) (format nil "<w:p>~@[<w:pPr><w:pStyle w:val=\"~a\"/></w:pPr>~]<w:r><w:t xml:space=\"preserve\">~a</w:t></w:r></w:p>" style text))
           (section-end (section) (format nil "<w:p><w:pPr><w:sectPr>~a</w:sectPr></w:pPr></w:p>" section)))
      (cons (cons "word/document.xml"
                  (concatenate 'string
                               (format nil "<w:document ~a><w:body>" w)
                               (p "Bird Survey" :style "Title")
                               (p "Findings" :style "Heading1") (p "Upright findings.")
                               (section-end upright)
                               "<w:tbl><w:tblPr><w:tblStyle w:val=\"BlueGrid\"/></w:tblPr><w:tr><w:tc><w:p><w:r><w:t>Wide</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>table</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"
                               (section-end sideways)
                               (p "Discussion" :style "Heading1") (p "Upright again.")
                               (section-end upright)
                               (p "Appendix" :style "Heading1") (p "Notes run on in two columns.")
                               "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"7\"/></w:numPr></w:pPr><w:r><w:t>One</w:t></w:r></w:p>"
                               "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"7\"/></w:numPr></w:pPr><w:r><w:t>Two</w:t></w:r></w:p>"
                               (format nil "<w:sectPr>~a</w:sectPr></w:body></w:document>" appendix)))
            (remove "word/document.xml" (docx-report-parts) :key #'car :test #'string=)))))

(define-test (template-import docx-sections-kept)
  (with-test-directory (dir)
    (let ((source (namestring (merge-pathnames "sections.docx" dir)))
          (note-path (namestring (merge-pathnames "sections.org" dir))))
      (with-open-file (out source :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets (docx-sectioned-parts)) out))
      (multiple-value-bind (made words)
          (tuatara.defaults::docx-import source "sect" :folder (namestring (merge-pathnames "sect/" dir))
                                                       :tex (namestring (merge-pathnames "sect.tex" dir)) :note note-path)
        (declare (ignore made))
        (let ((note (alexandria:read-file-into-string note-path)))
          (flet ((has (text) (true (search text note) "~s in~%~a" text note)))
            ;; the table alone sideways: its own drawer; the appendix: on its heading, all under it, sideways in two columns
            (has (format nil ":EXPORT:~%page: landscape~%:END:~%|"))
            (has (format nil "* Appendix~%:EXPORT:~%page: landscape~%columns: 2~%:END:~%"))
            ;; the upright sections as they are, the drawers hidden from Org's own exports, the list whole
            (false (search (format nil "* Discussion~%:EXPORT:") note))
            (has "d:(not \"EXPORT\" \"LOGBOOK\")")
            (is = 2 (count-matches ":EXPORT:" note))
            (false (search "TUATARA-PAGE" note))))
        ;; said: what was kept, and that the appendix's own margins were not
        (true (find-if (lambda (w) (search "2 later sections on pages turned sideways" w)) words) "~s" words)
        (true (find-if (lambda (w) (search "own paper size or margins are not kept" w)) words)))
      ;; written again: Word's sections turned as they were, the PDF's pages too
      (let* ((note (cl-ppcre:regex-replace "#\\+template: sect" (alexandria:read-file-into-string note-path) "#+template: ./sect"))
             (all (docx-text (tuatara.defaults::org-docx-document note :file note-path)))
             (orients (let ((o '()))
                        (cl-ppcre:do-register-groups (attrs) ("<w:sectPr>.*?<w:pgSz([^>]*)/>" all)
                          (push (if (search "landscape" attrs) :sideways :upright) o))
                        (nreverse o))))
        ;; upright; the table; upright; the appendix's heading, its two columns, and the end balancing them
        (is equal '(:upright :sideways :upright :sideways :sideways :sideways) orients "~s" orients)
        ;; no upright section left empty after the appendix: the document's last is the appendix's own
        (true (search "w:orient=\"landscape\"" all :start2 (search "<w:sectPr>" all :from-end t))))
      (let ((latex (tuatara.defaults::org-latex-document
                    (cl-ppcre:regex-replace "#\\+template: sect" (alexandria:read-file-into-string note-path) "#+template: ./sect")
                    :file note-path :enhanced t)))
        (is = 2 (count-matches "\\begin{tuataralandscape}" latex))
        (is = 2 (count-matches "\\end{tuataralandscape}" latex))))))

(define-test (template-import docx-fetched-from-elsewhere)
  (with-test-directory (dir)
    (let ((source (namestring (merge-pathnames "fetching.docx" dir)))
          (w *docx-report-w*))
      ;; a picture shown from a web address: named in what is said; a link clicked on is not
      (with-open-file (out source :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets
                         `(("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:p><w:r><w:t>Hi</w:t></w:r></w:p></w:body></w:document>" w))
                           ("word/_rels/document.xml.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId9\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/image\" Target=\"http://example.com/seen.png\" TargetMode=\"External\"/><Relationship Id=\"rId10\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink\" Target=\"http://example.com/clicked\" TargetMode=\"External\"/></Relationships>")))
                        out))
      (multiple-value-bind (made words)
          (tuatara.defaults::docx-import source "fetching" :folder (namestring (merge-pathnames "t/fetching/" dir))
                                                           :tex (namestring (merge-pathnames "t/fetching.tex" dir))
                                                           :note (namestring (merge-pathnames "fetching.org" dir)))
        (declare (ignore made))
        (let ((said (find-if (lambda (s) (search "fetches from elsewhere" s)) words)))
          (true (and said (search "http://example.com/seen.png" said)) "~s" words)
          (false (and said (search "clicked" said))))))))


(define-test (template-import docx-words-never-latex)
  ;; what a Word document says, its font's name and its header's words, never becomes LaTeX in the template
  (is string= "{{{title}}} \\textbackslash{}thing" (tuatara.defaults::docx-import-latex-text "<w:p><w:r><w:t>{{{title}}} \\thing</w:t></w:r></w:p>"))
  (let ((text (tuatara.defaults::docx-import-latex-text "<w:p><w:r><w:t>{{{x \\input{/etc/passwd} }}}</w:t></w:r></w:p>")))
    (false (search "\\input" text))
    (true (search "\\textbackslash{}input" text)))
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "font.docx" dir)))
          (tex (namestring (merge-pathnames "font.tex" dir))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets
                         `(("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:p><w:r><w:t>Hi</w:t></w:r></w:p></w:body></w:document>" *docx-report-w*))
                           ("word/styles.xml" . ,(format nil "<w:styles ~a><w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii=\"No Such Font&#10;\\input{/etc/passwd}\"/></w:rPr></w:rPrDefault></w:docDefaults></w:styles>" *docx-report-w*))))
                        out))
      (true (tuatara.defaults::docx-import path "font" :folder (namestring (merge-pathnames "font/" dir)) :tex tex
                                                       :note (namestring (merge-pathnames "font.org" dir))))
      (let ((latex (alexandria:read-file-into-string tex)))
        (false (search "\\input{/etc/passwd}" latex))
        (false (cl-ppcre:scan "(?m)^\\\\input" latex))))))

(define-test (template-import picture-names-kept-apart)
  ;; two pictures of one name from two folders of a document: each saved under a name of its own
  (let ((pictures '()))
    (flet ((name (source) (let ((file (tuatara.defaults::import-picture-file source "m" (tuatara.io:native-file-name source) pictures)))
                            (pushnew (cons source file) pictures :test #'equal)
                            file)))
      (is string= "m/a.png" (name "word/media/a.png"))
      (is string= "m/a (2).png" (name "word/x/a.png"))
      (is string= "m/a.png" (name "word/media/a.png")))))

(define-test (template-import latex-outside-files-every-form)
  ;; files above the document's folder, named without braces, read by \openin or tested for: each made a comment
  (let ((tuatara.defaults::*template-import* (tuatara.defaults::make-template-import))
        (outside '("\\input{../../.ssh/x}" "\\input /etc/passwd" "\\input/etc/passwd" "\\openin\\x=/etc/passwd"
                   "\\IfFileExists{/etc/passwd}" "\\includegraphics{\"~/x\"}" "\\import{sec/}{../../f}"
                   "\\includegraphics[width=2cm]{figs/../../x.png}"))
        (inside '("\\input{chapter}" "\\includegraphics[width=0.5\\textwidth]{figs/a.png}" "\\input{a}~\\cite{b}" "\\include{a/b}")))
    (let ((template (tuatara.defaults::latex-import-without-outside-files (format nil "~{~a~%~}" (append outside inside)))))
      (dolist (line outside)
        (true (search (format nil "%~a (left out on import" line) template) "~a in~%~a" line template))
      (dolist (line inside)
        (true (search (format nil "~%~a~%" line) template) "~a in~%~a" line template)))))
