;;;; lisp/org/keywords.lisp --- The #+KEYWORD lines an Org note may hold that Tuatara reads, in plain words.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *ORG-KEYWORD-SETTINGS* lists every keyword line Tuatara acts on, with what
;;;; it does and whether Tuatara alone reads it; *ORG-OPTION-SETTINGS* does the
;;;; same for the words of #+OPTIONS:, and *ORG-STARTUP-WORDS* (fontify.lisp)
;;;; for those of #+STARTUP:. M-x describe-org-keyword-settings shows them, each
;;;; with what holds when a note does not give it, read from the variables and
;;;; tables the code itself reads (*ORG-OPTION-DEFAULTS* and the like).

(in-package #:tuatara.defaults)

(defparameter *org-keyword-settings*
  '(("#+TITLE: text" nil "the document's title, printed above it")
    ("#+SUBTITLE: text" nil "printed under the title")
    ("#+AUTHOR: name" nil "printed under the title unless #+OPTIONS: author:nil")
    ("#+DATE: text" nil "printed under the title unless #+OPTIONS: date:nil")
    ("#+DESCRIPTION: text" nil "the subject of a PDF's properties")
    ("#+KEYWORDS: words" nil "the keywords of a PDF's properties")
    ("#+LANGUAGE: en" nil "the language code of the document (en, de-DE): its hyphenation, and the language the
     Word document and web page say")
    ("#+EXPORT_FILE_NAME: name" nil "the name the export is written to, in place of the note's name")
    ("#+SETUPFILE: file" nil
     "the lines of another file are read in this one's place; for an untrusted note
      (*UNTRUSTED-DIRECTORIES*), from the note's directory only")
    ("#+MACRO: name text" nil "defines {{{name}}}; $1 to $9 in the text stand for the arguments")
    ("#+OPTIONS: word:value ..." nil "see the options below")
    ("#+STARTUP: word ..." nil "see the words below; the last word of each kind wins")
    ("#+TOC: headlines 2" nil "a table of contents here in an export, 2 levels deep; tables or figures give a list
     of those; see the tables of contents below")
    ("a heading tagged :TOC:" nil "the links to the note's headings are kept as a list under it, made again when the
     note is saved. :TOC_3: says 3 levels, :TOC_2_org: or :TOC_2_gh: the style of the links")
    ("#+LATEX: text" nil "a line of LaTeX in the PDF; the web page and Word document leave it out")
    ("#+HTML: text" nil "a line of HTML Tuatara's web page leaves out, and says so")
    ("#+LATEX_CLASS: article" nil "the LaTeX document class of the PDF")
    ("#+LATEX_CLASS_OPTIONS: [11pt]" nil "the options of the document class")
    ("#+LATEX_HEADER: text" nil "a line added to the LaTeX preamble; read for formulas shown as pictures only in a
     trusted project")
    ("#+LATEX_HEADER_EXTRA: text" nil "the same, after those")
    ("#+LATEX_COMPILER: xelatex" nil "the program that makes the PDF")
    ("#+CITE_EXPORT: biblatex" nil "the way citations are made: biblatex, natbib or bibtex, then the bibliography
     style and the citation style")
    ("#+BIBLIOGRAPHY: file.bib" nil "a bibliography file; a name LaTeX cannot take is left out, and the export says so")
    ("#+PRINT_BIBLIOGRAPHY:" nil "the bibliography printed here")
    ("#+TEMPLATE: name" t "the template that gives a PDF or Word document its look")
    ("#+LATEX_TEMPLATE: name" t "an older name for #+TEMPLATE:")
    ("#+LATEX_STYLE: name" t "an older name for #+TEMPLATE:")
    ("#+LOGO: file" t "the picture a template's logo takes, from *LOGOS-DIRECTORY*")
    ("#+KEY: value" t "any other line a template names: its value is put where the template says")
    ("#+SELECT_TAGS: export" nil "only the headings with these tags are exported, with what is above and under them")
    ("#+EXCLUDE_TAGS: noexport" nil "the headings with these tags are left out; noexport is standard")
    ("#+EXPORT_SELECT_TAGS: tag" t "read as #+SELECT_TAGS: is")
    ("#+EXPORT_EXCLUDE_TAGS: tag" t "read as #+EXCLUDE_TAGS: is")
    ("#+TODO: TODO NEXT | DONE" nil
     "the keywords of tasks: those before the bar are open, those after it done. #+SEQ_TODO: and #+TYP_TODO: are
      read alike, and all the lines make one set; (t) and (t@/!) after a word are dropped")
    ("#+FILETAGS: :a:b:" nil "tags every heading of the note has, for the agenda and its matches")
    ("#+CATEGORY: name" nil "the category the agenda shows for the note's entries")
    ("#+ARCHIVE: %s_archive::" nil "where the clock table looks for archived entries; %s stands for the file's name")
    ("#+PROPERTY: name value" nil "a property every heading of the note has")
    ("#+CAPTION: text" nil "the caption of the table, picture or block after it; #+NAME: names it for links")
    ("#+ATTR_ORG: :width 50%" nil "sizes and places the picture after it; see M-x describe-inline-picture-settings")
    ("#+TBLFM: $3=$1+$2" nil "the formulas of the table above it; see M-x describe-table-formula-settings")
    ("#+BEGIN: name" nil "a dynamic block, up to #+END:; the lists of formatted names and the clock table are such
     blocks"))
  "The keyword lines of an Org note that Tuatara acts on, each as it is
written, true when only Tuatara reads it (other Org programs ignore it), and
what it does.")

(defparameter *org-option-settings*
  '(("title:t, title:nil" "print the title, or leave it out; the document class's title page too")
    ("author:t, author:nil" "print the author, or leave it out")
    ("date:t, date:nil" "print the date, or leave it out")
    ("toc:t, toc:nil, toc:2" "t: a table of contents, of the levels each export lists; nil: none; a number: that many
     levels")
    ("num:t, num:nil, num:2" "number the headings, or not; a number: only that many levels in the web page")
    ("H:3" "how many levels of headings are sections; those under are lists")
    ("tags:t, tags:nil" "print the headings' tags, or not")
    ("todo:t, todo:nil" "print the TODO keywords of headings, or not")
    ("pri:t, pri:nil" "print the priorities of headings, or not")
    ("d:t, d:nil, d:(not \"EXPORT\" \"LOGBOOK\")" "which drawers print: t all but LOGBOOK, nil none, (not \"A\") all
     but those, (\"A\") only those")
    ("arch:t, arch:nil, arch:headline" "export archived headings too, or not; headline exports their titles only")
    ("f:t, f:nil" "print the footnotes, or leave them out")
    ("e:t, e:nil" "print entities (\\alpha) as characters, or as written")
    ("-:t, -:nil" "turn -- and --- and ... into dashes and an ellipsis, or keep them as written")
    ("<:t, <:nil" "print the timestamps, or leave them out")
    ("\\n:t, \\n:nil" "keep the line breaks of a paragraph, or not")
    ("^:t, ^:nil, ^:{}" "sub and superscripts from every _ and ^, none, or only those in braces: x_{2}")
    ("redact:t, redact:nil" "hide what redact(...) holds, or print it")
    ("protect:t, protect:nil" "hide what protect(...) holds, or print it"))
  "The words of an Org note's #+OPTIONS: line that Tuatara reads, each as it
is written and what it does. A word is NAME:VALUE, a value one word or a list
in parentheses.")

(defun shown-value (value)
  "VALUE as a person reads it in a note or a setting: t, nil, a number, a word
or a text without quotes, in lower case."
  (format nil "~(~a~)" value))

(defun org-latex-engine-default ()
  "The program that makes the PDF of a note with no #+LATEX_COMPILER:: the first
of *LATEX-ENGINES*."
  (let ((engines (symbol-value 'tuatara.export::*latex-engines*)))
    (if (listp engines) (first engines) engines)))

(defparameter *org-keyword-defaults*
  (list (cons "TITLE" (lambda () "the note's name without its ending"))
        (cons "AUTHOR" (lambda () "your name"))
        (cons "LANGUAGE" (lambda ()
                           (format nil "~a for the PDF and web page, ~a for the Word document"
                                   *org-default-language* *docx-default-language*)))
        (cons "EXPORT_FILE_NAME" (lambda () "the note's name, with the ending of the export"))
        (cons "TOC" (lambda () "no contents here; toc of #+OPTIONS: decides for an export"))
        (cons "a heading tagged :TOC:"
              (lambda ()
                (format nil "~d level~:p (*ORG-TOC-MAX-DEPTH*) when #+OPTIONS: gives no toc:N, ~
                            the links in style ~a (*ORG-TOC-STYLE*)"
                        *org-toc-max-depth* *org-toc-style*)))
        (cons "LATEX_CLASS" (lambda () *org-latex-default-class*))
        (cons "LATEX_CLASS_OPTIONS" (lambda () *org-latex-default-class-options*))
        (cons "LATEX_COMPILER" (lambda ()
                                 (format nil "~a, the first of *LATEX-ENGINES*" (org-latex-engine-default))))
        (cons "CITE_EXPORT" (lambda () "none; the document sets up no bibliography"))
        (cons "TEMPLATE" (lambda () "the enhanced PDF has the article look for the classes article, report and book
or none, else the org template; the plain PDF has the org template"))
        (cons "EXCLUDE_TAGS" (lambda () (format nil "~{~a~^ ~}" *org-export-default-exclude-tags*)))
        (cons "TODO" (lambda ()
                       (format nil "~{~a~^ ~} | ~{~a~^ ~}, as *ORG-STANDARD-TODO-KEYWORDS* says"
                               (car *org-standard-todo-keywords*) (cdr *org-standard-todo-keywords*))))
        (cons "CATEGORY" (lambda () "the file's name without its ending"))
        (cons "ARCHIVE" (lambda () *org-default-archive*)))
  "What holds when a note does not give a keyword line, each (NAME . FUNCTION):
the function returns it as words. A line with no entry has no default.")

(defparameter *org-option-default-notes*
  (list (cons "toc" (lambda ()
                      (format nil "; the PDF and the Word document list as many levels as H: makes sections (~d ~
unless it says), the web page all"
                              (org-option-default "H"))))
        (cons "^" (lambda () "; the plain PDF takes t")))
  "What more to say of an option's default, each (NAME . FUNCTION).")

(defparameter *org-startup-defaults*
  (list (cons :visibility (lambda () "everything shown; no setting changes that"))
        (cons :drawers (lambda ()
                         (format nil "as *ORG-FOLD-METADATA* says, now ~a: t folds them, in a file under ~d octets"
                                 (shown-value *org-fold-metadata*) *org-fold-metadata-most*)))
        (cons :indent (lambda ()
                       (format nil "as *ORG-STARTUP-INDENTED* says, now ~a"
                               (shown-value *org-startup-indented*))))
        (cons :links (lambda ()
                       (format nil "as *ORG-LINK-DESCRIPTIVE* says, now ~a: t shows labels"
                               (shown-value *org-link-descriptive*))))
        (cons :pictures (lambda ()
                          (format nil "as *GLOBAL-INLINE-PICTURES-MODE* says, now ~s" *global-inline-pictures-mode*))))
  "What holds when a note gives no #+STARTUP: word of a kind (the kinds of
*ORG-STARTUP-WORDS*), each (KIND . FUNCTION): the function returns it as words.")

(defun org-keyword-default-note (written)
  "The words \"Not set: ...\" for the keyword line WRITTEN (\"#+NAME: value\"),
or NIL when it has no default. A row not a #+ line is looked up as written."
  (let ((entry (assoc (if (uiop:string-prefix-p "#+" written) (subseq written 2 (position #\: written)) written)
                      *org-keyword-defaults* :test #'string=)))
    (and entry (format nil "Not set: ~a." (funcall (cdr entry))))))

(defun org-option-default-note (written)
  "The words \"Not set: NAME:VALUE\" for the option WRITTEN (\"NAME:VALUE\"), the
value of *ORG-OPTION-DEFAULTS*, and what more is said of it."
  (let* ((name (subseq written 0 (position #\: written)))
         (note (cdr (assoc name *org-option-default-notes* :test #'string=))))
    (format nil "Not set: ~a:~a~@[~a~]." name (shown-value (org-option-default name)) (and note (funcall note)))))

(defun org-toc-rows ()
  "The rows of the section on tables of contents: what makes each, with its
number."
  (list "#+OPTIONS: toc:N" "the table at the top of an export (PDF, web page, Word document)"
        "#+TOC: headlines N" "a table where that line stands, in an export only; #+TOC: tables and #+TOC: figures
give lists of those"
        ":TOC_N: on a heading" "the table under that heading, in the note"
        ":TOC: on a heading" (format nil "the same, N from #+OPTIONS: toc:N, else ~d (*ORG-TOC-MAX-DEPTH*)"
                                     *org-toc-max-depth*)))

(defun org-startup-rows ()
  "The rows of the #+STARTUP: words: each word and what it does, the first of
each kind ending with what holds when none of that kind is given."
  (let ((seen '()))
    (loop for (word kind words) in *org-startup-words*
          append (list word (if (member kind seen)
                                words
                                (progn (push kind seen)
                                       (format nil "~a. Not set: ~a." words
                                               (funcall (cdr (assoc kind *org-startup-defaults*))))))))))

(define-command describe-org-keyword-settings ()
  "Show every #+KEYWORD line an Org note may hold that Tuatara reads, and the
words of #+OPTIONS: and #+STARTUP: lines: what each does, what holds when
a note does not give it, and which are Tuatara's own, which other programs
that read Org notes ignore. The lines are filled to *HELP-COLUMN*."
  (show-help
   (with-output-to-string (s)
     (write-help-section s "Keyword lines of an Org note"
                         :intro "A line #+KEYWORD: value near the top of a note changes how it is shown and
                         exported. The name may be written in any case. Lines Tuatara does not read stay as
                         they are, and other programs may read them. #+TAGS: and #+PRIORITIES: are not read;
                         tags and priorities are those written on the headings."
                         :example "#+TITLE: Report
#+STARTUP: indent inlineimages
#+TODO: TODO NEXT | DONE
#+OPTIONS: toc:2 num:nil")
     (write-help-section s "The lines"
                         :rows (loop for (written own words) in *org-keyword-settings*
                                     for note = (org-keyword-default-note written)
                                     append (list written
                                                  (format nil "~:[~;(Tuatara's own) ~]~a~@[. ~a~]" own words note)))
                         :width 30)
     (write-help-section s "Words of #+OPTIONS:"
                         :intro "Each is NAME:VALUE, apart by spaces. Not every export reads every one. What follows
                         \"Not set:\" is what holds when a note does not give the word."
                         :rows (loop for (written words) in *org-option-settings*
                                     append (list written
                                                  (format nil "~a. ~a" words (org-option-default-note written))))
                         :width 28)
     (write-help-section s "Tables of contents"
                         :intro "Each number is for its own table; none wins over another. With toc:t and a #+TOC:
                         line an export has two tables; toc:nil keeps only the #+TOC: one."
                         :rows (org-toc-rows)
                         :width 22)
     (write-help-section s "Words of #+STARTUP:"
                         :intro "Of each kind of word the last in the note wins. What follows \"Not set:\" is what
                         holds when the note has none of that kind."
                         :rows (org-startup-rows)
                         :width 18)
     (write-help-section s "Only Tuatara reads"
                         :intro "The lines marked (Tuatara's own), the :EXPORT: drawer
                         (\\[describe-export-settings]), the macros redact and protect, the options redact and
                         protect, the :appendix: and :TOC: tags, the NOTIFY_BEFORE property, #+ATTR_ORG: :float
                         and the startup words inlineimages and latexpreview (shown only when
                         *ORG-STARTUP-SHOWS-PICTURES* is t). Org's own programs ignore them or, for the macros,
                         stop with an error; the manual's chapter on Org says what each does there."))))
