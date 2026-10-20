;;;; lisp/org/template-import.lisp --- What a template imported from a Word or LaTeX document has, and its note template.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A document given to you to fill in, a Word document (docx-import.lisp)
;;;; or a LaTeX one (latex-import.lisp), is made a template and a note
;;;; template the same way. Each import gathers the fields it finds here, a
;;;; key, a kind and what the document had in it, and TEMPLATE-NOTE-TEXT
;;;; writes the note template every new note to fill in begins from:
;;;;
;;;; - #+title:, #+template: NAME, and #+options: with redact:nil and
;;;;   protect:nil, a comment saying what {{{R(...)}}} and {{{P(...)}}} do;
;;;; - for each field a comment naming it, with the document's example, and
;;;;   its keyword: empty where the document had placeholder text ([Enter
;;;;   name], XXXX), %date% for a date, %author% for an empty author, else
;;;;   what the document had; and
;;;; - the document's text as Org, a heading with nothing under it given a
;;;;   comment saying what to write there.
;;;;
;;;; A place written in the document already, {{{case}}}, is a field too:
;;;; tagging the fields before the import names them as you want.
;;;;
;;;; Both imports also refuse to write over anything, write their files and
;;;; say what they made the same way, with the functions here.

(in-package #:tuatara.defaults)

(defstruct (template-import (:constructor make-template-import ()) (:copier nil))
  "What an import has found: FIELDS, each (KEY KIND EXAMPLE LABEL FORMAT
CHOICES) in the order met; NOTES, what to say of it."
  (fields '()) (notes '()))

(defvar *template-import* nil
  "The TEMPLATE-IMPORT under way.")

(defparameter *template-import-reserved-keys* '("body" "contents" "created" "engine" "packages" "fonts" "toc" "latex_header" "font_fallbacks")
  "Names a field cannot have, the places Tuatara fills itself: such a field is named with _field after.")

(defun template-field-key (text)
  "TEXT, a field's tag, title, label or placeholder, as the name of a place
and a keyword: lower case, words joined by _, the words that ask (enter,
insert, type, your) left out: \"[Enter surveyor name]\" is surveyor_name."
  (let* ((words (cl-ppcre:all-matches-as-strings "[A-Za-z0-9]+" (string-downcase text)))
         (words (loop for rest on words
                      while (and (rest rest) (member (first rest) '("enter" "insert" "type" "add" "select" "choose" "your" "the" "click" "or" "tap" "here" "to")
                                                     :test #'string=))
                      finally (return rest)))
         (key (format nil "~{~a~^_~}" (subseq words 0 (min 4 (length words))))))
    (cond ((zerop (length key)) "field")
          ((digit-char-p (char key 0)) (concatenate 'string "field_" key))
          ((member key *template-import-reserved-keys* :test #'string=) (concatenate 'string key "_field"))
          (t key))))

(defun template-field (key kind &key example label format choices)
  "Note the field KEY of KIND (:TEXT, :DATE, :LIST, :CHECKBOX) with what the
document had in it, EXAMPLE; LABEL, how the document named it; FORMAT, a
date's; CHOICES, a list's. A key met before keeps its first description."
  (unless (find key (template-import-fields *template-import*) :key #'first :test #'string=)
    (setf (template-import-fields *template-import*)
          (append (template-import-fields *template-import*) (list (list key kind example label format choices))))))

(defun template-import-note (words)
  "Note WORDS, a sentence, to say of the import when it is done; said once."
  (pushnew words (template-import-notes *template-import*) :test #'string=))

(defun template-import-check-free (places)
  "Signal an error unless each of PLACES, the files and folders an import
makes, is free: an import writes over nothing that exists."
  (dolist (place places)
    (when (tuatara.io:path-stamp place)
      (error "~a exists already: nothing made" (abbreviated-path place)))))

(defun template-import-media (note name)
  "The folder the pictures of the note template NOTE of the template NAME are
saved in: NAME-media beside NOTE."
  (concatenate 'string (tuatara.io:native (tuatara.io:parent-path note)) name "-media"))

(defun template-import-write (path contents)
  "Write CONTENTS, a string (as UTF-8) or octets, as the file PATH (REPLACE-FILE), its folders made first."
  (tuatara.io:ensure-directories path)
  (tuatara.io:replace-file path contents))

(defun template-import-result (note)
  "What an import returns once it has written the note template NOTE: NOTE,
and as a second value what to say of it, a list of sentences: its fields
first, then what was noted (TEMPLATE-IMPORT-NOTE)."
  (let ((fields (template-import-fields *template-import*)))
    (values note
            (cons (format nil "~d field~:p (~{~a~^, ~})" (length fields) (mapcar #'first fields))
                  (reverse (template-import-notes *template-import*))))))

(defparameter *template-placeholder*
  "\\[([^\\[\\]{}]{2,60})\\]|«([^«»{}]{2,60})»|<([^<>{}]{2,40})>"
  "Placeholder text in a template's text: [Enter name], «Name», <Version>.")

(defun template-placeholder-p (text)
  "True when TEXT, what a document has in a field, only shows where a value
goes: [Enter name], «Name», <Version>, XXXX or 20XX in it, X X, ..., ___."
  (let ((text (string-trim '(#\Space #\Tab) (or text ""))))
    (or (zerop (length text))
        (cl-ppcre:scan (format nil "^(?:~a)$" *template-placeholder*) text)
        (cl-ppcre:scan "(?:^|[^A-Za-z])(?:X{2,}|x{3,})(?:[^A-Za-z]|$)" text)
        (cl-ppcre:scan "^[Xx](?:[\\s./:-]+[Xx])*$" text)
        (cl-ppcre:scan "^(?:\\.{3,}|…+|_{3,})$" text))))

(defun template-date-label-p (label)
  "True when LABEL, how a document names a field, names a date."
  (cl-ppcre:scan "(?i)\\bdated?\\b" label))

(defun template-places-in (text)
  "Note a field for each place TEXT has written already, {{{case}}} or a
date's {{{issue_date:date=d MMMM yyyy}}}, but those Tuatara fills itself
\(body, title, contents, the LaTeX places) and a condition's marks."
  (cl-ppcre:do-matches (start end *template-place-re* text)
    (let ((place (template-place (subseq text start end))))
      (when (and place (null (getf place :condition))
                 (not (member (getf place :name) (list* "body" "title" "contents" *latex-only-places*) :test #'string=)))
        (let ((name (getf place :name)))
          (template-field name (if (or (getf place :date) (template-date-label-p (substitute #\Space #\_ name))) :date :text)
                          :format (getf place :date) :label (template-field-words name nil)))))))

(defun template-field-words (key label)
  "How a note template's comment names the field KEY the document called
LABEL: LABEL without the brackets of a placeholder, else KEY in words, its
first letter a capital."
  (let ((label (and label (string-trim " :" (cl-ppcre:regex-replace "^[\\[«<](.*)[\\]»>]$" label "\\1")))))
    (let ((words (if (and label (plusp (length label))) label (substitute #\Space #\_ key))))
      (concatenate 'string (string-upcase (subseq words 0 1)) (subseq words 1)))))

(defun template-note-fields (fields stream)
  "Write FIELDS (each as TEMPLATE-FIELD keeps them) to STREAM as a note
template has them: a comment naming each and what it takes, then its keyword."
  (dolist (field fields)
    (destructuring-bind (key kind example label format choices) field
      (let* ((words (org-safe-line (template-field-words key label)))
             (empty (template-placeholder-p example))
             (example (and example (plusp (length (string-trim " " example))) (org-safe-line example))))
        (unless (string= key "title")
          (ecase kind
            (:date (format stream "# ~a, a date, as 2026-10-05~@[; the document writes it ~a~]~%#+~a: %date%~%"
                           words (and format (org-safe-line format)) key))
            (:list (format stream "# ~a, one of ~{~a~^, ~}~%#+~a: ~@[~a~]~%"
                           words (mapcar #'org-safe-line choices) key (and (not empty) example)))
            (:checkbox (format stream "# ~a, yes or no~%#+~a: ~@[~a~]~%" words key (and (not empty) example)))
            (:text (cond
                     ((and empty (string= key "author"))
                      (format stream "# ~a~%#+author: %author%~%" words))
                     (empty
                       (format stream "# ~a~@[, as ~a~]~%#+~a:~%" words
                               (and example (not (string-equal example words))
                                    (not (cl-ppcre:scan (format nil "^(?:~a)$" *template-placeholder*) example))
                                    example)
                               key))
                     (t (format stream "# ~a~%#+~a: ~a~%" words key example))))))))))

(defun template-heading-guidance (text)
  "TEXT, an Org note's text, each heading with nothing under it before the
next heading of its level or above given a comment line saying what to write
there: # Write the executive summary."
  (let* ((lines (coerce (cl-ppcre:split "\\n" text :limit most-positive-fixnum) 'vector))
         (count (length lines))
         (out (make-string-output-stream)))
    (flet ((level (line) (let ((m (nth-value 1 (cl-ppcre:scan-to-strings "^(\\*+) " line)))) (and m (length (aref m 0))))))
      (loop with i = 0
            while (< i count)
            do (let* ((line (aref lines i))
                      (level (level line)))
                 (write-line line out)
                 (when level
                   ;; what follows, past blank lines and drawers: a heading as deep or less, or the end
                   (let ((j (1+ i)) (in-drawer nil))
                     (loop while (< j count)
                           do (let ((next (string-trim " " (aref lines j))))
                                (cond (in-drawer (when (string-equal next ":END:") (setf in-drawer nil)))
                                      ((cl-ppcre:scan "^:[A-Za-z_-]+:$" next) (setf in-drawer t))
                                      ((zerop (length next)))
                                      (t (return))))
                              (incf j))
                     (let ((next-level (and (< j count) (level (aref lines j)))))
                       (when (or (>= j count) (and next-level (<= next-level level)))
                         ;; the drawers under the heading first, then the comment, then the blank lines
                         (loop for k from (1+ i) below j
                               unless (zerop (length (string-trim " " (aref lines k))))
                                 do (write-line (aref lines k) out))
                         (let* ((title (string-trim " " (cl-ppcre:regex-replace "^\\*+ +" line "")))
                                (title (cl-ppcre:regex-replace "\\s+:[\\w@:]+:$" title ""))
                                (title (if (and (> (length title) 1) (lower-case-p (char title 1)))
                                           (concatenate 'string (string-downcase (subseq title 0 1)) (subseq title 1))
                                           title)))
                           (format out "# Write the ~a.~%" (org-safe-line title)))
                         (when (< (1+ i) j) (terpri out))
                         (setf i (1- j)))))))
               (incf i)))
    (string-right-trim '(#\Newline) (get-output-stream-string out))))

(defun template-note-text (name title fields body &key contents numbered drawers)
  "The note template of the template NAME: TITLE, the keywords of FIELDS
\(TEMPLATE-NOTE-FIELDS), then BODY, the document's text as Org, its empty
headings given what to write (TEMPLATE-HEADING-GUIDANCE). CONTENTS, true
for a table of contents; NUMBERED, for numbered headings; DRAWERS, for
export drawers in BODY to leave out of the document."
  (with-output-to-string (s)
    (if (template-placeholder-p title)
        (format s "# The title~@[, as ~a~]~%#+title:~%" (and title (plusp (length title)) (org-safe-line title)))
        (format s "#+title: ~a~%" (org-safe-line title)))
    (format s "#+template: ~a~%#+options: toc:~:[nil~;t~] num:~:[nil~;t~] redact:nil protect:nil~:[~; d:(not \"EXPORT\" \"LOGBOOK\")~]~%"
            name contents numbered drawers)
    (format s "# redact:t covers what {{{R(...)}}} wraps, protect:t what {{{P(...)}}} wraps (C-c m wraps the text selected).~%")
    (when fields
      (format s "~%")
      (template-note-fields fields s))
    (let ((body (string-trim '(#\Newline #\Space) body)))
      (when (plusp (length body))
        (format s "~%~a~%" (template-heading-guidance body))))))
