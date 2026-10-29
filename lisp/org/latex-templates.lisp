;;;; lisp/org/latex-templates.lisp --- LaTeX templates an Org document's keywords fill in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A template is a LaTeX document with places for what an Org document
;;;; gives: {{{body}}} for its text written as LaTeX, {{{title}}}, {{{author}}}
;;;; and the like, and {{{KEY}}} for the value of any #+KEY: line. The plain
;;;; export's PDFs come from the built-in template org; the enhanced
;;;; export takes the template #+template: names, by name (yours in
;;;; latex-templates/ in the configuration directory before a built-in one) or
;;;; as the path of a file of its own, else the built-in article. Only the
;;;; general ones are built in, data/latex-templates/*.tex, read into the
;;;; program when it is built; a report, a letter or any other design of your
;;;; own is a file of yours.

(in-package #:tuatara.defaults)

(defparameter *built-in-latex-templates*
  (loop for (file text) in (built-in-data-files "latex-templates/" :type "tex")
        collect (cons (pathname-name file) text))
  "The templates that come with Tuatara, each (NAME . TEMPLATE): org, the
plain export's document, and article, the enhanced export's
own look for a note that sets none.")

(defun latex-templates-directory ()
  "Where your own templates are: latex-templates/ in the configuration
directory (~/.config/tuatara/latex-templates/), each NAME.tex."
  (config-path "latex-templates/"))

(defun latex-templates ()
  "The templates there are, each (NAME . TEMPLATE): yours, the files NAME.tex
in LATEX-TEMPLATES-DIRECTORY, then the built-in ones none of yours takes the
place of (IN-PLACE-OF-OWN)."
  (in-place-of-own (loop for (file . text) in (folder-files (list (latex-templates-directory)))
                         when (and text (string-equal (name-extension file) ".tex"))
                           collect (cons (subseq file 0 (- (length file) 4)) text))
                   *built-in-latex-templates*
                   #'car))

(defun latex-template-names ()
  "The names of the templates there are, yours and the built-in ones, sorted."
  (sort (mapcar #'car (latex-templates)) #'string<))

(defun find-latex-template (name &key built-in)
  "The template called NAME: yours (LATEX-TEMPLATES-DIRECTORY) before the
built-in one, or with BUILT-IN the built-in one alone; NIL when there is none."
  (cdr (assoc name (if built-in *built-in-latex-templates* (latex-templates)) :test #'string-equal)))

(defun latex-template-path-p (value)
  "True when VALUE, what #+template: says, is the path of a file rather
than a name: it holds a /, starts with ~ or ends with .tex."
  (or (find #\/ value) (uiop:string-prefix-p "~" value) (uiop:string-suffix-p (string-downcase value) ".tex")))

(defun template-path-allowed-p (path directory)
  "True when a note in DIRECTORY may name the template at PATH by its path:
PATH stands in DIRECTORY or a folder in it, in your own templates' folder, or
in one of *EXPORT-TRUSTED-FOLDERS*, links followed, so that a note given to
you cannot make its export take in a file of yours from elsewhere."
  (path-in-folders-p path (list* (or directory (tuatara.io:home-directory)) (latex-templates-directory) *export-trusted-folders*)))

(defun latex-template-for (value directory)
  "The template VALUE names, what a note's #+template: says: a file's
path (LATEX-TEMPLATE-PATH-P; .tex added when it has none, ./report for
report.tex), ~ your home directory and a relative one taken
from DIRECTORY, the note's own (your home directory for a note with no
file), and only where TEMPLATE-PATH-ALLOWED-P lets it be; else a name
\(FIND-LATEX-TEMPLATE). Its path is a second value, NIL for one named.
Signals, saying what there is, when there is no such template."
  (if (latex-template-path-p value)
      (let* ((path (org-export-path value directory))
             ;; a pair named without .tex (./report): its LaTeX half report.tex, its Word half the folder report/
             (path (if (uiop:string-suffix-p (string-downcase path) ".tex") path (concatenate 'string path ".tex"))))
        (cond ((not (eq :regular (tuatara.io:path-kind path)))
               (error "There is no LaTeX template at ~a (#+template:)" (abbreviated-path path)))
              ((not (template-path-allowed-p path directory))
               (error "The template ~a is outside the note's folder (#+template:); *export-trusted-folders* names the folders it may also come from"
                      (abbreviated-path path)))
              (t (values (tuatara.io:file-text path :limit *org-file-read-limit*) path))))
      (or (find-latex-template value)
          (error "There is no LaTeX template called ~a (#+template:): yours are in ~a, and there are ~
                  ~{~a~^, ~}; a file of its own is named by its path (./report.tex, ~~/Templates/report.tex)"
                 value (abbreviated-path (latex-templates-directory)) (latex-template-names)))))

(defun fill-latex-template (template field)
  "TEMPLATE with its places filled by FIELD, a function of a name and a way,
:TEXT, :RAW or :PATH, giving the string for it, or NIL when there is none.
{{{NAME}}} is the value, {{{NAME:raw}}} and {{{NAME:path}}} the value read
the other ways, {{{NAME:date=d MMMM yyyy}}} a date written in Word's pattern
\(DOCX-FORMAT-DATE: 30 September 2026), as an imported Word template's date
field has it; {{{#NAME}}}...{{{/NAME}}} is kept when NAME has a value that
is not empty, {{{^NAME}}}...{{{/NAME}}} when it has none. A brace before a
place, as in \\title{{{{title}}}}, stays a brace."
  (let ((n (length template)))
    (labels ((place-at (i)
               ;; the place at I, read as the Word half reads its places (TEMPLATE-PLACE), and its end
               (when (and (<= (+ i 3) n) (string= "{{{" template :start2 i :end2 (+ i 3)))
                 (let ((place (template-place (subseq template i (min n (+ i 120))))))
                   (when (and place (< (getf place :end) 100))
                     (values place (+ i (getf place :end)))))))
             (value (place)
               (let ((key (getf place :name)) (date (getf place :date)))
                 (if date
                     ;; a date written in the pattern given, else the value as it is
                     (let ((given (funcall field key :raw)))
                       (multiple-value-bind (y m d) (and given (docx-date-parts given))
                         (if y
                             (latex-escape (docx-format-date date y m d))
                             (funcall field key :text))))
                     ;; a picture's size is the Word half's: here its path alone
                     (funcall field key (getf place :way)))))
             (fill-from (start end out)
               ;; fill from START below END into OUT; returns where it stopped
               (let ((i start))
                 (loop while (< i end)
                       do (multiple-value-bind (place after) (place-at i)
                            ;; {{{{ is a brace, then a place: its name holds no brace
                            (cond ((null place) (write-char (char template i) out) (incf i))
                                  ((member (getf place :condition) '(#\# #\^))
                                   (let* ((key (getf place :name))
                                          (closing (format nil "{{{/~a}}}" key))
                                          (stop (search closing template :start2 after :test #'char-equal))
                                          (present (let ((v (funcall field key :text))) (and v (plusp (length v))))))
                                     (unless stop (error "The template has ~a with no {{{/~a}}}" (subseq template i after) key))
                                     (when (if (char= (getf place :condition) #\#) present (not present))
                                       (fill-from after stop out))
                                     (setf i (+ stop (length closing)))))
                                  ;; a condition's end with no beginning: nothing
                                  ((getf place :condition) (setf i after))
                                  (t (write-string (or (value place) "") out)
                                     (setf i after))))))))
      (with-output-to-string (out)
        (fill-from 0 n out)))))
