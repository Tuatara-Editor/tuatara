;;;; lisp/export/templates.lisp --- Templates made from a Word or LaTeX document given you to fill in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x make-template-from-docx and M-x make-template-from-latex ask for the
;;;; document and the template's name, make the template and its note
;;;; template in the background (docx-import.lisp, latex-import.lisp), then
;;;; show a new note begun from the note template; M-x insert-note-template
;;;; begins each one after.

(in-package #:tuatara.defaults)

(defstruct (template-import-job (:include tuatara.runtime:job) (:constructor make-template-import-job (buffer path name make)) (:copier nil))
  "A document at PATH being made the template NAME and its note template by
MAKE, a function of PATH and NAME (DOCX-IMPORT, LATEX-IMPORT), a job for
BUFFER; NOTE, the note template's path once made; WORDS, what to say of it."
  path name make note words)

(setf (documentation 'template-import-job-p 'function) "True when OBJECT is a TEMPLATE-IMPORT-JOB.")

(tuatara.runtime:define-job-words template-import-job
  :doing "Making the template"
  :description (format nil "Making ~a the template ~a" (tuatara.io:native-file-name (template-import-job-path job)) (template-import-job-name job))
  :verb "make the template"
  :left-words "no template made")

(defmethod tuatara.runtime:job-take-in ((job template-import-job))
  "Show a new note begun from the note template made, and say what the template has, or why none was made."
  (if (eq (tuatara.runtime:job-status job) :done)
      (let ((buffer (new-empty-buffer))
            (name (template-import-job-name job)))
        (set-major-mode buffer 'org-mode)
        (tuatara.buffer:buffer-insert buffer 0 (formatted-template-text (list name :file (template-import-job-note job))))
        (message "Template ~a made: ~{~a~^; ~}. A new note begun from it; \\[insert-note-template] ~a begins another"
                 name (template-import-job-words job) name))
      (message "~a" (tuatara.runtime:job-ending-words job))))

(defun start-template-import (path make)
  "Ask the template's name (offered from PATH's) and make the document at
PATH a template and its note template with MAKE in the background; nothing
when PATH is NIL, no document named."
  (let ((name (and path (read-string "Template name: " :initial (title-slug (tuatara.io:native-file-stem path))))))
    (cond ((null path))
          ((zerop (length name)) (message "No name: nothing made"))
          ((not (cl-ppcre:scan "^[A-Za-z0-9][-A-Za-z0-9_]*$" name)) (message "A template's name is letters, digits, - and _: nothing made"))
          (t (tuatara.runtime:start-job-thread
              (make-template-import-job (current-buffer) path name make)
              (lambda (job)
                (multiple-value-bind (note words) (funcall (template-import-job-make job) (template-import-job-path job) (template-import-job-name job))
                  (setf (template-import-job-note job) note (template-import-job-words job) words)))
              :name "tuatara template import")
             (message "Making ~a the template ~a..." (tuatara.io:native-file-name path) name)))))

(define-command make-template-from-docx ()
  "Make a Word document given to you a template to write reports in: asked
for the document (the one shown, if any) and the template's name, it makes
the template's Word half (the document up to its first heading, its headers,
footers, styles and page, each of its fields a place to fill: a content
control, a form field, placeholder text such as [Enter name], a table row or
a line labelled \"Version: 0.1\"), its LaTeX half (the same design as near as
LaTeX says it) and its note template, NAME in
~/.config/tuatara/note-templates/: #+template: NAME, a comment and a keyword
for each field (empty where the document had placeholder text, %date% for a
date), then the document's text, its tables Org tables, to write over. When
it is done a new note begun from it is shown, and \\[insert-note-template]
NAME begins each one after. \\[org-export-docx] then writes a document that
looks as the one given, your values in its fields and your text in its
design. Its macros, comments and tracked changes are left out; nothing in it
is run, and nothing is written beside it. In the background."
  (let* ((buffer (current-buffer))
         (shown (let ((shown (or (let ((view (pdf-view-of buffer))) (and view (pdf-view-maker view) (pdf-view-path view)))
                                 (tuatara.buffer:buffer-local buffer 'office-reading))))
                  (and shown (eq :docx (getf (office-file-format shown) :kind)) shown))))
    (start-template-import (or shown (read-native-path "Word document to make a template of: " :kind :file))
                           #'docx-import)))

(define-command make-template-from-latex ()
  "Make a LaTeX document given to you a template to write reports in: asked
for the document (the one visited, if any) and the template's name, it makes
the LaTeX template NAME in ~/.config/tuatara/latex-templates/ (the document
up to its first \\section, each of its fields a place to fill: \\title,
\\author, \\date, a value its preamble defines such as
\\newcommand{\\refnumber}{REF-0001}, placeholder text such as [Enter name], a
tabular row of a label and its value, a line labelled \"Version: 0.1\"; the
rest {{{body}}}) and its note template, NAME in
~/.config/tuatara/note-templates/: #+template: NAME, a comment and a keyword
for each field, then the document's text as Org (sections, lists, tables,
pictures, footnotes) to write over. When it is done a new note begun from it
is shown, and \\[insert-note-template] NAME begins each one after.
\\[org-export-latex-pdf] then writes a PDF that looks as the document given.
A document that has {{{body}}} already is taken as it is. Nothing in it is
run, the files it \\inputs are not read, and nothing is written beside it.
In the background."
  (let* ((file (tuatara.buffer:buffer-file (current-buffer)))
         (shown (and file (string-equal "tex" (tuatara.io:native-file-type file)) (tuatara.io:native file))))
    (start-template-import (or shown (read-native-path "LaTeX document to make a template of: " :kind :file))
                           #'latex-import)))
