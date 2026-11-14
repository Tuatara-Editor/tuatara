;;;; lisp/files/formatted-links.lisp --- Links to your files with formatted names, found by matching their names.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands that make a link to every file whose name matches a regular
;;;; expression (match-formatted-files-link, and match-denote-files-link for
;;;; the kind other programs read too), and what following such a link does:
;;;; list the files matching, to visit one. Loaded after Org, whose link
;;;; text and link following it uses.

(in-package #:tuatara.defaults)

(defun denote-files-scanner (query)
  "A scanner for QUERY as denote:query-filenames: links match names, ignoring case;
QUERY that is no regular expression is matched as text."
  (tuatara.syntax:compile-pattern query :case-fold t :or-text t))

(defun files-matching (scanner files)
  "Those of FILES, your files with formatted names, each a list whose first is
its path from *FORMATTED-FILE-DIRECTORY*, whose path SCANNER matches; NIL
with no SCANNER."
  (and scanner (remove-if-not (lambda (file) (cl-ppcre:scan scanner (car file))) files)))

(defun denote-files-matching (query files)
  "Those of FILES, your files with formatted names (FILES-MATCHING), whose
path QUERY matches as denote:query-filenames: links match (DENOTE-FILES-SCANNER)."
  (files-matching (denote-files-scanner query) files))

(defun formatted-files-scanner (regex)
  "A scanner for REGEX, a regular expression, ignoring case unless it has a
capital letter (smart case); NIL when it is no regular expression."
  (ignoring-errors-reported ("reading the pattern ~a" regex) (tuatara.syntax:compile-pattern regex)))

(defun formatted-files-matching (regex files)
  "Those of FILES, your files with formatted names (FILES-MATCHING), whose
path REGEX, a regular expression, matches, ignoring case unless it has a
capital letter; NIL when REGEX is no regular expression."
  (files-matching (formatted-files-scanner regex) files))

(defun read-formatted-files-query (words matching)
  "Ask for a regular expression the names of your files with formatted names are
to match, for what WORDS says is done with it, The prompt names its kind first
as a search's does (Regex match file names: ). The files MATCHING (a function of
a regular expression and the files) gives are listed as it is typed, the files
found in the background meanwhile \(CALL-WITH-FORMATTED-FILES);
\\<pattern>\\[search-toggle-regex] switches to literal text and back.
\\<pattern-answer>\\[pattern-finish] takes what is typed, whichever file is
selected, be it a regular expression or literal text with its special characters
escaped; a regular expression that cannot be used says why in the prompt,
waiting for it to be mended."
  (let ((typed "") (regex t) (refused nil))
    (call-with-formatted-files
     (lambda (listed source)
       (let ((*toggle-pattern-kind* (lambda () (setf regex (not regex))))
             (*pattern-finish* (lambda ()
                               (if (pattern-problem (editor-minibuffer-text *editor*) regex)
                                   (setf refused (editor-minibuffer-text *editor*))
                                   (finish-minibuffer)))))
         (completing-read (lambda ()
                            (let ((text (editor-minibuffer-text *editor*)))
                              (refusable-pattern-prompt regex words (pattern-problem text regex) (equal refused text))))
                          (lambda (input)
                            (setf typed input)
                            (mapcar #'third (funcall matching (pattern-regex input regex) (funcall listed))))
                          :filter nil
                          :select (lambda (candidates input) (declare (ignore candidates input)) :input) :reselect t
                          :source source :keymap *pattern-answer-keymap*
                          :none (format nil "none in ~a"
                                        (abbreviated-path (directory-name *formatted-file-directory*)))))))
    (keep-typed-pattern typed)
    (pattern-regex typed regex)))

(defun show-formatted-files (title keep)
  "List your files with formatted names whose path from
*FORMATTED-FILE-DIRECTORY* KEEP is true of, found in the background, under
TITLE, to choose one to visit (READ-FORMATTED-FILE)."
  (let ((chosen (read-formatted-file title :keep keep :preview t)))
    (when chosen
      (visit-path (concatenate 'string (directory-name *formatted-file-directory*) chosen)))))

(defun show-files-matching (scanner pattern)
  "List your files with formatted names whose names SCANNER matches, made of
PATTERN, to choose one to visit."
  (show-formatted-files (format nil "Files matching ~a: " pattern)
                        (lambda (relative) (cl-ppcre:scan scanner relative))))

(defun follow-formatted-files-link (regex)
  "List your files with formatted names whose names REGEX matches, to choose
one to visit, as a link match-formatted-files-link makes is followed."
  (let ((scanner (formatted-files-scanner regex)))
    (if (null scanner)
        (message "~a is no regular expression" regex)
        (show-files-matching scanner regex))))

(defun follow-denote-files-link (query)
  "List your files with formatted names whose names QUERY matches, to choose
one to visit, where a denote:query-filenames: link leads."
  (show-files-matching (denote-files-scanner query) query))

(define-command match-formatted-files-link ()
  "Insert a link to your files with formatted names whose names match a
regular expression, asked for, the files it matches listed as it is typed:
[[formatted-match:REGEX][LABEL]]. The text selected becomes the link, its
label; with none, the label is asked for, REGEX offered. Following it (\\`RET'
or \\[org-open-at-point] in Org) lists the files in *FORMATTED-FILE-DIRECTORY*
and under it whose path from there REGEX matches, ignoring case unless it has
a capital letter, to visit one. Brackets in it are written \\=\\[ and \\=\\]
in the link. It is Tuatara's own kind of link, which other Org programs do not
follow; match-denote-files-link makes one that other programs reading
denote: links follow too."
  (let* ((buffer (current-buffer))
         (region (selected-region buffer))
         (regex (read-formatted-files-query "match file names" #'formatted-files-matching)))
    (cond ((string= (string-trim " " regex) "") (message "No link inserted: nothing to match"))
          ((null (formatted-files-scanner regex)) (message "No link inserted: ~a is no regular expression" regex))
          (t (insert-labelled-link buffer region (if (find #\] regex) "matching files" regex)
                                   (lambda (label)
                                     (org-link-text (format nil "~a:~a" *formatted-files-link-type* regex) label)))))))

(defun match-denote-files-link-text (query label extension)
  "The denote: link to the files whose names QUERY matches, labelled LABEL, as
written in a file ending in EXTENSION: [[denote:query-filenames:QUERY][LABEL]]
in Org (and others), [LABEL](denote:query-filenames:QUERY) in Markdown,
<denote:query-filenames:QUERY> in text, which has no label."
  (cond ((string-equal extension ".md") (format nil "[~a](denote:query-filenames:~a)" label query))
        ((string-equal extension ".txt") (format nil "<denote:query-filenames:~a>" query))
        (t (org-link-text (format nil "denote:query-filenames:~a" query) label))))

(define-command match-denote-files-link ()
  "Insert a link to your files with formatted names whose names match what is
asked for, the files it matches listed as it is typed, as a denote: link,
which other programs reading those links follow too,
written [[denote:query-filenames:TEXT][LABEL]]; in Markdown
[LABEL](denote:query-filenames:TEXT) and in text files
<denote:query-filenames:TEXT>. The text selected becomes the link, its label; with
none, the label is asked for, TEXT offered. Following it (\\`RET' or
\\[org-open-at-point] in Org) lists the files in *FORMATTED-FILE-DIRECTORY* and
under it whose names match, ignoring case, to visit one. Brackets and
parentheses in what is matched are refused, as these links cannot hold them."
  (let* ((buffer (current-buffer))
         (region (selected-region buffer))
         (query (read-formatted-files-query "match file names (Denote)" #'denote-files-matching))
         (file (tuatara.buffer:buffer-file buffer))
         (extension (if file (name-extension (tuatara.io:native-file-name file)) ".org")))
    (cond ((string= (string-trim " " query) "") (message "No link inserted: nothing to match"))
          ((or (find #\[ query) (find #\] query) (find #\( query) (find #\) query))
           (message "No link inserted: denote links take no brackets or parentheses"))
          (t (insert-labelled-link buffer region query
                                   (lambda (label) (match-denote-files-link-text query label extension))
                                   :label (and (string-equal extension ".txt") ""))))))
