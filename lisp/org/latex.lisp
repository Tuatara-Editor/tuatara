;;;; lisp/org/latex.lisp --- An Org document written as LaTeX, for a PDF.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; ORG-LATEX-PARTS writes an Org document as a LaTeX document, read by
;;;; ORG-PARSE, the way Org's own LaTeX exporter (ox-latex, Org 9.7) writes it:
;;;; the same preamble, keywords (#+title, #+author, #+date, #+subtitle,
;;;; #+options, #+latex_class, #+latex_class_options, #+latex_header,
;;;; #+export_file_name, #+export_select_tags, #+export_exclude_tags,
;;;; #+setupfile, #+macro, #+toc, #+attr_latex), sectioning, lists, tables,
;;;; blocks, markup, links, footnotes and entities, and citations for biblatex,
;;;; natbib and bibtex (#+cite_export, #+bibliography, #+print_bibliography,
;;;; as Org's oc-biblatex, oc-natbib and oc-bibtex), so that a document and its
;;;; setup file give the PDF they give there. Nothing in the document is run:
;;;; code blocks are written as they are, a macro whose value would be
;;;; evaluated is left empty, and only local setup files are read. What
;;;; every export shares (keywords, setup files, macros, the files an export
;;;; may take in, the headlines and tables written, quote marks) is in
;;;; export-core.lisp.

(in-package #:tuatara.defaults)

(define-record (org-latex (:include org-export) (:constructor make-org-latex (text directory file)) (:copier nil))
  "What writing an Org note as LaTeX knows beyond what every export knows
\(ORG-EXPORT): the LaTeX program, the marked elements, the fonts and the files
the template and the note's pictures use."
  (engine "xelatex" :documentation "The LaTeX program ORG-LATEX is for.")
  (marked (make-array 64 :adjustable t :fill-pointer 0)
          :documentation "Where each element of ORG-LATEX whose LaTeX is marked is in the text, (BEGIN . END)
\(ORG-LATEX-MARK).")
  (appendix nil :documentation "True once ORG-LATEX has reached the annexes (ORG-LATEX-APPENDIX-TAG-P).")
  (fonts '()
         :documentation "The font families chosen for the template's {{{fonts}}} in ORG-LATEX, each (PART . FAMILY).")
  (fallbacks '()
             :documentation "The fonts ORG-LATEX falls back to for a character the chosen fonts lack, each (FAMILY .
COMMAND).")
  (paths '() :documentation "The files the template's {{{NAME:path}}} places of ORG-LATEX were filled with.")
  (pictures '() :documentation "The files the note's own pictures of ORG-LATEX are drawn from (ORG-LATEX-IMAGE).")
  (bibliography-files '() :documentation "The bibliography files ORG-LATEX names (ORG-LATEX-BIBLIOGRAPHIES)."))

(defvar *org-latex-template* ""
  "The text of the template the document being written is laid out by.")

(defparameter *org-latex-image-types* '("png" "jpg" "jpeg" "gif" "pdf" "eps" "tif" "tiff" "bmp")
  "The kinds of file, by their names' endings, a link to which is shown as a picture.")

(defparameter *org-latex-enhanced-image-types* '("svg" "webp" "heic" "heif" "avif" "ico")
  "The kinds of file, by their names' endings, a link to which is shown as a
picture in the enhanced export too, made into PNG (LATEX-PICTURE).")

(defparameter *org-latex-sections*
  '(("article" "section" "subsection" "subsubsection" "paragraph" "subparagraph")
    ("report" "part" "chapter" "section" "subsection" "subsubsection")
    ("book" "part" "chapter" "section" "subsection" "subsubsection"))
  "The sectioning commands of each document class, from the highest level;
a class not named here has the article's.")

(defvar *org-latex-in-columns* nil
  "True while what is written is set in columns (multicols), where LaTeX's
tables across pages (longtable) cannot be: a table is then kept whole.")

;;; Text

(defun latex-escape (string &key (special-strings t) (newlines nil) (quotes nil))
  "STRING with the characters LaTeX gives meanings to written so they print
as themselves; with SPECIAL-STRINGS, ... as an ellipsis, \\- as a place to
hyphenate, and the words LaTeX and TeX as their logos; with NEWLINES, each
line break kept as one; with QUOTES, each straight quote as the opening or
closing mark of the note's language (EXPORT-QUOTE-MARK-AT). A zero-width space,
which in a note only keeps the text beside it from being read as markup, is
left out."
  (with-output-to-string (out)
    (loop with n = (length string)
          for i from 0 below n
          for c = (char string i)
          do (cond ((and quotes (find c "\"'"))
                    (write-string (export-quote-mark-at string i) out))
                   ((and special-strings (char= c #\\) (< (1+ i) n) (char= (char string (1+ i)) #\-))
                    (write-string "\\-" out) (incf i))
                   ((and special-strings (char= c #\L) (<= (+ i 5) n) (string= "LaTeX" string :start2 i :end2 (+ i 5)))
                    (write-string "\\LaTeX{}" out) (incf i 4))
                   ((and special-strings (char= c #\T) (<= (+ i 3) n) (string= "TeX" string :start2 i :end2 (+ i 3)))
                    (write-string "\\TeX{}" out) (incf i 2))
                   ((and special-strings (char= c #\.) (< (+ i 2) n) (string= "..." string :start2 i :end2 (+ i 3)))
                    (write-string "\\ldots{}" out) (incf i 2))
                   ((char= c #\\) (write-string "\\textbackslash{}" out))
                   ((char= c #\ZERO_WIDTH_SPACE))
                   ((char= c #\~) (write-string "\\textasciitilde{}" out))
                   ((char= c #\^) (write-string "\\textasciicircum{}" out))
                   ((find c "{}$%#&_") (write-char #\\ out) (write-char c out))
                   ((and newlines (char= c #\Newline)) (write-string "\\\\\\relax" out) (write-char c out))
                   (t (write-char c out))))))

(defparameter *latex-code-break-after* "-/._:=,;|)@"
  "The characters after which code in a line of text may go on to the next line
in the enhanced export, when a letter, a digit, \\, ( or [ follows and three
characters at least stay on each side, so that a key such as C-x is kept whole.
A number such as 10.25 and a repeat such as {1,2} are kept whole too.")

(defparameter *latex-code-run-limit* 16
  "The most characters of code in a line of text the enhanced export keeps
together where nothing else lets it go on to the next line: a long digest,
hexadecimal number or address goes on after that many, so that no table
column is made as wide as the longest of them.")

(defun latex-code-break-p (string i &optional (run 0) start end)
  "True when the code STRING may go on to the next line after its character
at I: after one of *LATEX-CODE-BREAK-AFTER*, or anywhere once RUN, the
characters since the last place it could, reaches *LATEX-CODE-RUN-LIMIT*;
three characters at least kept on each side. START and END, where the word
I is in begins and ends, are looked for when not given."
  (let ((c (char string i))
        (next (and (< (1+ i) (length string)) (char string (1+ i))))
        (blank (lambda (c) (member c '(#\Space #\Tab #\Newline)))))
    (and next (not (funcall blank next)) (not (funcall blank c))
         (or (and (find c *latex-code-break-after*) (or (alphanumericp next) (find next "\\(["))
                  (not (and (digit-char-p next) (plusp i) (digit-char-p (char string (1- i))))))
             (>= run *latex-code-run-limit*))
         (let ((start (or start (let ((b (position-if blank string :end i :from-end t))) (if b (1+ b) 0))))
               (end (or end (position-if blank string :start i) (length string))))
           (and (>= (- (1+ i) start) 3) (>= (- end (1+ i)) 3))))))

(defun latex-texttt (string)
  "STRING in a typewriter face, printed as it is; in the enhanced export, a
long name, path or pattern may go on to the next line after a - / . _ : =
, ; | ) or @, and a long digest or number after every *LATEX-CODE-RUN-LIMIT*
characters, rather than run into the margin, the next line beginning with
a small arrow, as a code block's lines do when they go on, so that the
reader takes the two parts as one (\\codebreak, LATEX-CODE-BREAK-P). The
arrow is drawn but not copied: text copied from the PDF has none."
  (if (and *org-export* (org-export-enhanced *org-export*))
      (with-output-to-string (out)
        (write-string "\\texttt{" out)
        (loop with start = 0 and run = 0 and word-start = nil and word-end = nil
              for i from 0 below (length string)
              do (cond ((member (char string i) '(#\Space #\Tab #\Newline)) (setf run 0 word-start nil))
                       (t (incf run)
                          (unless word-start
                            (setf word-start i
                                  word-end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                                            string :start i)
                                               (length string))))))
              when (and word-start (latex-code-break-p string i run word-start word-end))
              do (write-string (latex-escape (subseq string start (1+ i)) :special-strings nil) out)
              (write-string "\\codebreak{}" out)
              (setf start (1+ i) run 0)
              finally (write-string (latex-escape (subseq string start) :special-strings nil) out))
        (write-string "}" out))
      (format nil "\\texttt{~a}" (latex-escape string :special-strings nil))))

(defun latex-url (url)
  "Uniform resource locator (URL) written for \\href and \\url: a backslash
or brace, and any character past ASCII (as its UTF-8 octets), in it
percent-encoded (%5C, %7B, %7D, %C3%A9), as a web address may have it, and
each % and # escaped."
  (with-output-to-string (out)
    (loop for c across url
          do (case c
               (#\\ (write-string "\\%5C" out))
               (#\{ (write-string "\\%7B" out))
               (#\} (write-string "\\%7D" out))
               ((#\% #\#) (write-char #\\ out) (write-char c out))
               (t (if (< (char-code c) 128)
                      (write-char c out)
                      (loop for octet across (tuatara.text:string-octets (string c))
                            do (format out "\\%~2,'0X" octet))))))))

(defun latex-file-name-p (path)
  "True when PATH can stand as it is in a LaTeX command's argument naming a
file: none of % # { } \\ ~ ^ in it, which LaTeX reads as markup there."
  (notany (lambda (c) (find c "%#{}\\~^")) path))

(defun latex-file-name-refusal (name)
  "The sentence saying why the file NAME (as written) is left out: LaTeX cannot
take a file named with markup characters in it (LATEX-FILE-NAME-P)."
  (format nil "~a: LaTeX cannot take a file named with % # { } \\ ~~ or ^" name))

(defun latex-label-name (string)
  "STRING made fit for a LaTeX label: ASCII letters, digits and :-_. as they
are, any other character written as its code in hexadecimal after an x, so
that no character a font might lack is in a label."
  (with-output-to-string (out)
    (loop for c across string
          do (cond ((and (< (char-code c) 128) (or (alphanumericp c) (find c ":-_."))) (write-char c out))
                   ((< (char-code c) 128) (write-char #\- out))
                   (t (format out "x~(~x~)" (char-code c)))))))

(defun org-latex-new-label (prefix)
  "A label not given before, beginning with PREFIX."
  (format nil "~a~d" prefix (incf (org-export-counter *org-export*))))

;;; Headlines

(defun org-latex-label (node)
  "The label of NODE, a headline or an element with a name, given once."
  (or (gethash node (org-export-labels *org-export*))
      (setf (gethash node (org-export-labels *org-export*))
            (let ((custom (and (eq (org-node-type node) 'headline) (org-headline-property node "CUSTOM_ID"))))
              (if custom
                  (latex-label-name custom)
                  (org-latex-new-label (if (eq (org-node-type node) 'headline) "sec:" "org")))))))

(defun org-listed-headline-ends (parent)
  "The first and the last, as a cons, of the headlines under PARENT that are
written, which are the ones a list is begun and ended by; worked out once for
each PARENT."
  (let ((table (org-export-list-ends *org-export*)))
    (or (gethash parent table)
        (setf (gethash parent table)
              (let ((written (remove-if-not (lambda (n)
                                              (and (eq (org-node-type n) 'headline) (not (org-export-skipped-p n))))
                                            (org-node-children parent))))
                (cons (first written) (car (last written))))))))

(defun org-latex-class-sections ()
  "The sectioning commands of the note's #+LATEX_CLASS, from the highest
level (*ORG-LATEX-SECTIONS*)."
  (let ((class (or (org-export-keyword "LATEX_CLASS") *org-latex-default-class*)))
    (rest (or (assoc class *org-latex-sections* :test #'string-equal) (first *org-latex-sections*)))))

(defun org-latex-section-command (level)
  "The sectioning command for a headline of LEVEL (from 1, after the
highest), or NIL when it is written as a list item instead (past H)."
  (and (<= level (org-export-headline-levels)) (nth (1- level) (org-latex-class-sections))))

(defun org-latex-depth (levels)
  "The number LaTeX's tocdepth and secnumdepth take to reach headlines of 1
to LEVELS levels. LaTeX numbers its sections part -1, chapter 0, section 1,
subsection 2, so in a report or a book, whose highest headlines are parts,
the number is LEVELS less 2, and in an article LEVELS itself."
  ;; how many steps above section the highest headline's command is
  (- levels (or (position (first (org-latex-class-sections)) '("section" "chapter" "part") :test #'string=) 0)))

(defun org-latex-appendix-tag-p (tag)
  "True when TAG begins the annexes in the enhanced export: appendix, in any case."
  (and (org-export-enhanced *org-export*) (string-equal tag "appendix")))

(defun org-latex-headline-title (headline out)
  "Write HEADLINE's title, with its TODO keyword and tags as the options say, to OUT."
  (let ((todo (org-property headline :todo-keyword)))
    (when (and todo (org-export-option "todo"))
      (format out "\\textbf{~a} " (latex-escape todo))))
  (let ((priority (org-property headline :priority)))
    (when (and priority (org-export-option "pri"))
      (format out "\\framebox{\\#~a} " (latex-escape (string (code-char priority))))))
  (org-latex-objects-between (org-property headline :title) (org-property headline :title-begin)
                             (org-property headline :title-end) out)
  (let ((tags (remove-if #'org-latex-appendix-tag-p (org-property headline :tags))))
    (when (and tags (org-export-option "tags"))
      (format out "\\hfill{}\\textsc{~{~a~^:~}}" (mapcar #'latex-escape tags)))))

(defun org-latex-headline (headline out)
  "Write HEADLINE and what is under it to OUT: a section, or past the
deepest level sections go to (H), an item of a list; out of the contents
when its :EXPORT: drawer says contents: no."
  (unless (org-export-skipped-p headline)
    (let* ((level (1+ (- (org-property headline :level) (org-export-min-level *org-export*))))
           (command (org-latex-section-command level))
           (title (with-output-to-string (s) (org-latex-headline-title headline s)))
           (numbered (org-export-numbered-p headline))
           (listed (not (export-setting-no-p headline "contents")))
           ;; what is under it, in the columns and type its :EXPORT: drawer gives it
           (contents (with-output-to-string (s)
                       (org-latex-with-settings headline s
                                                (lambda (s)
                                                  (dolist (child (org-node-children headline))
                                                    (org-latex-element child s)))))))
      ;; the first headline tagged appendix, or set annex: yes, begins the annexes, on a page of
      ;; their own, the rest as the template makes them
      (when (and (not (org-latex-appendix *org-export*))
                 (or (some #'org-latex-appendix-tag-p (org-property headline :tags))
                     (string-equal (org-export-setting headline "annex") "yes")))
        (setf (org-latex-appendix *org-export*) t)
        (format out "\\clearpage~%\\appendix~%"))
      (write-string (org-latex-mark headline) out)
      (if command
          (progn
            (if (and numbered (not listed))
                ;; numbered, but its line in the contents never written
                (format out "{\\expandafter\\let\\expandafter\\addcontentsline\\csname @gobblethree\\endcsname~%~
\\~a{~a}~%\\label{~a}}~%"
                        command title (org-latex-label headline))
                (progn
                  (format out "\\~a~:[*~;~]{~a}~%" command numbered title)
                  (when (and (not numbered) listed (org-export-option "toc"))
                    (format out "\\addcontentsline{toc}{~a}{~a}~%" command title))
                  (format out "\\label{~a}~%" (org-latex-label headline))))
            (write-string contents out))
          ;; past H: a list, one for the headlines side by side
          (let* ((ends (org-listed-headline-ends (org-node-parent headline)))
                 (environment (if (org-export-option "num") "enumerate" "itemize")))
            (when (eq headline (car ends)) (format out "\\begin{~a}~%" environment))
            (format out "\\item ~a~%\\label{~a}~%~a" title (org-latex-label headline) contents)
            (when (eq headline (cdr ends)) (format out "\\end{~a}~%" environment)))))))

;;; Elements

(defun org-latex-elements (node out)
  "Write the elements in NODE to OUT."
  (dolist (child (org-node-children node))
    (org-latex-element child out)))

(defun org-latex-caption (node)
  "NODE's #+CAPTION as LaTeX, or NIL when it has none or an empty one."
  (let ((caption (getf (org-property node :affiliated) :caption)))
    (and (stringp caption)
         (plusp (length (string-trim *org-blanks* caption)))
         (org-latex-string caption))))

(defun org-latex-name-label (node)
  "\\label{...} for NODE when it has a #+NAME, else an empty string."
  (if (org-property node :name) (format nil "\\label{~a}" (org-latex-label node)) ""))

(defun latex-verbatim-escape (string &key (tab-width 8))
  "STRING for a Verbatim environment that reads commands (commandchars \\, { and
}). Those three are written as the characters they are, tabs as spaces to the
next stop of TAB-WIDTH, and a place the line may break after every
*LATEX-CODE-RUN-LIMIT* characters with no space or mark of punctuation \(a break
the Verbatim environment makes of its own) among them, the rest as it is."
  (with-output-to-string (out)
    (let ((column 0) (run 0))
      (loop for c across string
            do (case c
                 (#\\ (write-string "\\char92{}" out) (incf column))
                 (#\{ (write-string "\\char123{}" out) (incf column))
                 (#\} (write-string "\\char125{}" out) (incf column))
                 (#\Tab (let ((spaces (- tab-width (mod column tab-width))))
                          (dotimes (i spaces) (write-char #\Space out))
                          (incf column spaces)))
                 (#\Newline (write-char c out) (setf column 0))
                 (t (write-char c out) (incf column)))
            (if (or (find c '(#\Space #\Tab #\Newline)) (find c *latex-code-break-after*))
                (setf run 0)
                (when (>= (incf run) *latex-code-run-limit*)
                  (write-string "\\discretionary{}{}{}" out)
                  (setf run 0)))))))

(defun org-latex-code-faces (code name)
  "The face of each character of CODE read as the language NAME is (as a
source block names it, emacs-lisp, sh...), as Tuatara colours it: a vector,
NIL where no face is; NIL when the language is not known or not coloured."
  (let ((language (and name (plusp (length name)) (org-src-language name))))
    (when language
      (multiple-value-bind (style level) (language-colouring language)
        (unless (eq style :none)
          ;; its rules made here, quietly, on whatever thread exports
          (let ((*rules-quiet* t))
            (language-compiled-query language :colouring) (language-compiled-query language :injection)
            (language-compiled-query language :locals)
            (let* ((rope (tuatara.text:make-rope code))
                   (bytes (tuatara.text:rope-octet-length rope))
                   (faces (make-array (length code) :initial-element nil))
                   (highlights (ignoring-errors-reported ("colouring the code of a ~a block" name)
                                                         (tuatara.syntax:language-highlights
                                                          (injection-resolver style level)
                                                          (symbol-name (language-name language))
                                                          (list (cons 0 bytes)) rope 0 bytes))))
              ;; outer ranges first: the innermost face of a character is its last
              (loop for (start end face) in highlights
                    for last = (min (length code) (tuatara.text:byte->char rope end))
                    do (loop for i from (tuatara.text:byte->char rope start) below last
                             do (setf (aref faces i) face)))
              faces)))))))

(defun org-latex-face-look (face cache)
  "How FACE prints in the enhanced export, from *LIGHT-THEME* (the page is
paper): a list of its colour as RRGGBB or NIL, bold and italic; NIL when it
prints as plain text. CACHE, a hash table, keeps what was worked out."
  (multiple-value-bind (look found) (gethash face cache)
    (if found
        look
        (setf (gethash face cache)
              (let* ((theme tuatara-user:*light-theme*)
                     (hex (tuatara.display:colour-hex (tuatara.display:face-attribute-in-theme face :fg theme)))
                     (bold (tuatara.display:face-attribute-in-theme face :bold theme))
                     (italic (tuatara.display:face-attribute-in-theme face :italic theme)))
                (and (or hex bold italic) (list (and hex (string-upcase (subseq hex 1))) bold italic)))))))

(defun org-latex-code-lines (code faces)
  "CODE for a Verbatim environment that reads commands, each run of one face
of FACES (ORG-LATEX-CODE-FACES) begun by switches to its colour, bold and
italic (only those that change, each line beginning plain), so that the text
stays where its lines can be broken anywhere."
  (let ((cache (make-hash-table :test 'eq))
        (plain '(nil nil nil)))
    (with-output-to-string (out)
      (loop with n = (length code)
            with i = 0
            with now = plain
            while (< i n)
            do (let* ((face (and faces (aref faces i)))
                      (end (loop for j from i below n
                                 when (or (char= (char code j) #\Newline) (not (eq (and faces (aref faces j)) face)))
                                 return j
                                 finally (return n))))
                 (if (= end i)
                     ;; the line's end: the next begins plain
                     (progn (write-char #\Newline out) (setf now plain) (incf i))
                     (destructuring-bind (colour bold italic) (or (and face (org-latex-face-look face cache)) plain)
                       (destructuring-bind (was-colour was-bold was-italic) now
                         (unless (equal colour was-colour)
                           (if colour (format out "\\color[HTML]{~a}" colour) (write-string "\\normalcolor{}" out)))
                         (unless (eq (and bold t) (and was-bold t))
                           (write-string (if bold "\\bfseries{}" "\\mdseries{}") out))
                         (unless (eq (and italic t) (and was-italic t))
                           (write-string (if italic "\\itshape{}" "\\upshape{}") out)))
                       (setf now (list colour bold italic))
                       (write-string (latex-verbatim-escape (subseq code i end)) out)
                       (setf i end))))))))

(defun org-latex-plain-verbatim (value)
  "VALUE, the text of a code or example block, as a verbatim environment of the
plain export holds it: a line holding \\end{verbatim}, which would end the
environment there, set apart in typewriter letters between two of them, so
that it reads as the block wrote it."
  (cl-ppcre:regex-replace-all "(?m)^.*\\\\end\\s*\\{verbatim\\}.*$" value
                              (lambda (line)
                                (format nil "\\end{verbatim}~%\\noindent\\texttt{~a}~%\\begin{verbatim}"
                                        (latex-escape line)))
                              :simple-calls t))

(defun org-latex-verbatim (value out &key caption node plain)
  "Write VALUE as it is, in a verbatim environment, to OUT, in a figure with
CAPTION when there is one; nothing when VALUE is blank. In the enhanced export
its long lines are broken where they meet the margin (at a space, or after / - .
, ; : =, or in a run with none, as a long digest, after every
*LATEX-CODE-RUN-LIMIT* characters). The rest of the line is indented one em,
with a mark where it breaks that text copied from the PDF leaves out. The code
of a source block NODE is coloured as Tuatara colours its language
\(ORG-LATEX-CODE-FACES), and NODE's #+ATTR_LATEX :columns N sets it in N
columns, :font in that type (\\small). With PLAIN, those two are not read."
  (let* ((value (string-right-trim '(#\Newline) value))
         (enhanced (org-export-enhanced *org-export*))
         (attributes (and node enhanced (not plain) (org-export-attributes node "latex")))
         (columns (let ((c (getf attributes :columns))) (and c (whole-number c))))
         (font (let ((f (getf attributes :font))) (and f (plusp (length f)) f))))
    (when (or (and columns (> columns 1)) font)
      (let ((inner (with-output-to-string (s) (org-latex-verbatim value s :caption caption :node node :plain t))))
        (format out "~@[{~a~%~]~:[~*~a~;\\begin{multicols}{~d}~%~a\\end{multicols}~%~]~:[~;}~%~]"
                font (and columns (> columns 1)) columns inner font)
        (return-from org-latex-verbatim)))
    (let ((block (cond (enhanced
                        (format nil "\\begin{Verbatim}[breaklines,breakautoindent=false,breakindent=1em,~
breakafter=/-.\\,;:=,commandchars=\\\\\\{\\}]~%~a~%\\end{Verbatim}~%"
                                (org-latex-code-lines value (and node (eq (org-node-type node) 'src-block)
                                                                 (org-latex-code-faces
                                                                  value (org-property node :language))))))
                       (t (format nil "\\begin{verbatim}~%~a~%\\end{verbatim}~%" (org-latex-plain-verbatim value))))))
      (cond
        ((zerop (length (string-trim '(#\Space #\Tab #\Newline) value))) nil)
        (caption
         (format out "\\begin{figure}[H]~%\\caption{~a}~a~%~a\\end{figure}~%"
                 caption (org-latex-name-label node) block))
        (t (write-string block out))))))

(defparameter *org-latex-mark-open* (code-char #xD800)
  "The character that opens a mark in LaTeX being written (ORG-LATEX-MARK): a
lone surrogate, which no text read from a file holds, so that a note's own
characters, private ones too, are never taken for a mark.")

(defparameter *org-latex-mark-close* (code-char #xD801)
  "The character that closes a mark (ORG-LATEX-MARK), a lone surrogate as the
one opening it is.")

(defun org-latex-mark (node)
  "A mark saying that the LaTeX after it comes from NODE, taken out again
when the document is done (ORG-LATEX-UNMARK)."
  (let ((marked (org-latex-marked *org-export*)))
    (vector-push-extend (cons (org-node-begin node)
                              (max (org-node-begin node) (1- (- (org-node-end node) (org-node-post-blank node)))))
                        marked)
    (format nil "~c~d~c" *org-latex-mark-open* (1- (fill-pointer marked)) *org-latex-mark-close*)))

(defun org-latex-unmark (latex)
  "LATEX with its marks taken out, and as a second value which place in the
text as written each of its lines comes from: a vector of (LINE BEGIN END),
LINE from 1, in order, for the lines where an element's LaTeX begins, BEGIN
and END where the element is in the text."
  (let ((map (make-array 64 :adjustable t :fill-pointer 0))
        (line 1)
        (marked (org-latex-marked *org-export*)))
    (values
     (with-output-to-string (out)
       (loop with n = (length latex)
             with i = 0
             while (< i n)
             do (let ((c (char latex i)))
                  (cond ((char= c *org-latex-mark-open*)
                         (let* ((close (position *org-latex-mark-close* latex :start i))
                                (index (parse-integer latex :start (1+ i) :end close))
                                (place (list line (org-export-original-position (car (aref marked index)))
                                             (org-export-original-position (cdr (aref marked index))))))
                           ;; the last element begun on a line, the innermost, speaks for it
                           (if (and (plusp (fill-pointer map)) (= line (first (aref map (1- (fill-pointer map))))))
                               (setf (aref map (1- (fill-pointer map))) place)
                               (vector-push-extend place map))
                           (setf i (1+ close))))
                        (t (when (char= c #\Newline) (incf line))
                           (write-char c out)
                           (incf i))))))
     (coerce map 'simple-vector))))

(defun org-latex-visible-start (string)
  "The first character of STRING that is not part of a mark, or NIL."
  (loop with i = 0
        while (< i (length string))
        do (if (char= (char string i) *org-latex-mark-open*)
               (setf i (1+ (position *org-latex-mark-close* string :start i)))
               (return (char string i)))))

(defun org-latex-element (node out)
  "Write the element NODE to OUT, marked as coming from it (ORG-LATEX-MARK)
when it is printed, in what its :EXPORT: drawer says of any element
\(ORG-LATEX-WITH-SETTINGS), a new page and space before and after it as it
says, then the blank lines after it; nothing for one written already as
part of another."
  (let ((type (org-node-type node)))
    (unless (gethash node (org-export-consumed *org-export*))
      (when (org-latex-printed-p type)
        (write-string (org-latex-mark node) out))
      (call-with-export-breaks
       node out
       (lambda (out)
         (org-latex-with-page node out
                              (lambda (out)
                                (if (eq type 'headline)
                                    (org-latex-element-itself node type out)
                                    (org-latex-with-settings node out (lambda (s)
                                                                        (org-latex-element-itself node type s)))))))
       :length #'org-latex-length
       :page (lambda (out) (format out "~&\\clearpage~%"))
       :space (lambda (out length) (format out "~&\\par\\vspace{~a}~%" length)))
      (when (and (org-latex-printed-p type) (plusp (org-node-post-blank node)))
        (terpri out)))))

(defun org-latex-element-itself (node type out)
  "Write the element NODE, of TYPE, to OUT, as its kind is written."
  (case type
    ((org-data section dynamic-block) (org-latex-elements node out))
    (headline (org-latex-headline node out))
    (paragraph (org-latex-paragraph node out))
    (plain-list (org-latex-plain-list node out))
    (table (org-latex-table node out))
    (src-block (org-latex-verbatim (org-block-code node) out
                                   :caption (org-latex-caption node) :node node))
    ((example-block fixed-width) (org-latex-verbatim (or (org-property node :value) "") out
                                                     :caption (org-latex-caption node) :node node))
    (export-block (when (member (org-property node :type) '("LATEX" "TEX") :test #'string=)
                    (write-string (org-property node :value) out)))
    (latex-environment (write-string (org-property node :value) out))
    (keyword (org-latex-keyword-element node out))
    (horizontal-rule (format out "\\noindent\\rule{\\linewidth}{0.5pt}~%"))
    (quote-block (format out "\\begin{quote}~%") (org-latex-elements node out) (format out "\\end{quote}~%"))
    (center-block (format out "\\begin{center}~%") (org-latex-elements node out) (format out "\\end{center}~%"))
    (special-block (let ((name (org-property node :type)))
                     (format out "\\begin{~a}~a~%" name (org-latex-name-label node))
                     (org-latex-elements node out)
                     (format out "\\end{~a}~%" name)))
    (verse-block (format out "\\begin{verse}~%")
                 (write-string (org-latex-objects-text node :newlines t) out)
                 (format out "~%\\end{verse}~%"))
    (drawer (when (org-export-drawer-printed-p node) (org-latex-elements node out)))
    (table-row nil)
    ;; what is never printed: properties, planning, clocks, comments, calls, footnotes' definitions (at their
    ;; references)
    (t nil)))

(defun org-latex-printed-p (type)
  "True when an element of TYPE is printed, as it is (a headline is printed by
its own writer), so that it is marked and the blank lines after it are kept."
  (not (member type '(org-data section headline property-drawer node-property planning clock comment comment-block
                      babel-call diary-sexp footnote-definition))))

(defun org-latex-keyword-element (node out)
  "Write the keyword line NODE to OUT: #+LATEX: lines as they are, #+TOC: as a table of contents,
#+PRINT_BIBLIOGRAPHY: as the bibliography (ORG-LATEX-PRINT-BIBLIOGRAPHY)."
  (let ((key (org-property node :key)) (value (or (org-property node :value) "")))
    (cond ((string-equal key "LATEX") (format out "~a~%" value))
          ((string-equal key "PRINT_BIBLIOGRAPHY") (write-string (org-latex-print-bibliography) out))
          ((string-equal key "TOC")
           (multiple-value-bind (kind depth) (org-toc-keyword value)
             (case kind
               ;; for this table alone (etoc), so that neither a later table nor the bookmarks take its depth
               (:headlines (format out "\\etocsetnexttocdepth{~d}~%\\tableofcontents~%"
                                   (org-latex-depth (or depth (org-export-headline-levels)))))
               (:tables (format out "\\listoftables~%"))
               (:figures (format out "\\listoffigures~%"))))))))

(defun org-latex-tables-of-contents-p ()
  "True when the note has a #+TOC: headlines line, a table of contents where
it stands, each such table as deep as its own line says (the LaTeX package
etoc)."
  (some (lambda (value) (eq :headlines (org-toc-keyword value))) (org-export-keyword-all "TOC")))

(defun org-latex-image-link-p (node)
  "True when NODE is a link to a picture the PDF export shows as the picture
\(ORG-IMAGE-LINK-P), of the types the enhanced export takes when it is the
one running."
  (org-image-link-p node (if (org-export-enhanced *org-export*)
                             (append *org-latex-image-types* *org-latex-enhanced-image-types*)
                             *org-latex-image-types*)))

(defun org-latex-image (node attributes)
  "\\includegraphics for the picture NODE links to, as ATTRIBUTES (#+ATTR_LATEX) size it;
its name in a box instead when it cannot be shown, is not there or is named so
that LaTeX cannot take it (LATEX-FILE-NAME-P; which the export then says), or
is outside the directories an export may take files from
\(ORG-EXPORT-FILE-ALLOWED-P)."
  (let* ((path (org-property node :path))
         (full (or (ignoring-errors-reported ("finding ~a" path) (org-export-path path))
                   path))
         (width (getf attributes :width))
         (height (getf attributes :height))
         (options (format nil "~{~a~^,~}" (remove nil
                                                  (list (and width (plusp (length width)) (format nil "width=~a" width))
                                                        (and height
                                                             (plusp (length height))
                                                             (format nil "height=~a" height))
                                                        (let ((o (getf attributes :options)))
                                                          (and o (plusp (length o)) o)))))))
    (flet ((not-shown ()
             (return-from org-latex-image
               (format nil
                       "\\fbox{\\texttt{~a}}"
                       (latex-escape (tuatara.io:native-file-name full) :special-strings nil)))))
      (unless (org-export-file-allowed-p full)
        (not-shown))
      ;; one not there, or not to be read, said and boxed: the LaTeX program would stop at it
      (unless (org-export-enhanced *org-export*)
        (let ((why (nth-value 2 (file-kind full))))
          (when why
            (export-note "~a cannot be read: ~a" (tuatara.io:native-file-name full) why)
            (not-shown))))
      ;; in the enhanced export, a picture the LaTeX programs cannot take made into one they can
      (when (org-export-enhanced *org-export*)
        (multiple-value-bind (picture why) (latex-picture full)
          (if picture
              (setf full picture)
              (progn (export-note "~a" why)
                     (not-shown)))))
      (unless (latex-file-name-p full)
        (export-note "~a, so left out" (latex-file-name-refusal (tuatara.io:native-file-name full)))
        (not-shown))
      (pushnew full (org-latex-pictures *org-export*) :test #'string=))
    (format nil "\\includegraphics[~a]{~a}" (if (and (null width) (null height) (zerop (length options)))
                                                (format nil "width=~a" *picture-latex-width*)
                                                options)
            full)))

(defun org-latex-paragraph (node out)
  "Write the paragraph NODE to OUT: a picture alone in it centred, or in a
figure when it has a caption; in the enhanced export as wide, placed,
against a margin and beside the paragraph after it as its :EXPORT: drawer
says (width, place, position, beside). Text is set as its align says."
  (let* ((children (org-node-children node))
         (alone (org-export-picture-alone-p node)))
    (if alone
        (let* ((attributes (picture-attributes node :latex t))
               (beside (org-export-setting node "beside"))
               (text (and beside (org-export-paragraph-after node)))
               (position (export-position node))
               (caption (org-latex-caption node)))
          (cond (text
                 ;; side by side: the picture as wide as its half, its caption under it, the paragraph after it beside
                 ;; it
                 (setf (gethash text (org-export-consumed *org-export*)) t)
                 (let ((picture (format nil "\\begin{minipage}[c]{0.47\\linewidth}~%\\centering~%~a~%~
~@[\\captionof{figure}{~a}~a~%~]\\end{minipage}~%"
                                        (org-latex-image (first children) (list* :width "\\linewidth" attributes))
                                        caption (if caption (org-latex-name-label node) "")))
                       (words (format nil "\\begin{minipage}[c]{0.47\\linewidth}~%~a~a~%\\end{minipage}~%"
                                      (org-latex-mark text)
                                      (org-latex-objects-text text :newlines (org-export-option "\\n")))))
                   (format out "\\par\\noindent~%~a\\hfill~%~a\\par~%"
                           (if (string-equal beside "right") words picture)
                           (if (string-equal beside "right") picture words))))
                ((and caption position)
                 ;; against a margin: the picture and its caption together, the caption under the picture
                 (format out "\\begin{figure}[~a]~%~a~%\\sbox0{~a}%~%\\begin{minipage}{\\wd0}~%\\usebox0\\par~%~
\\caption{~a}~a~%\\end{minipage}~%\\end{figure}~%"
                         (or (getf attributes :placement) "htbp")
                         (if (eq position :left) "\\raggedright" "\\raggedleft")
                         (org-latex-image (first children) attributes) caption (org-latex-name-label node)))
                ((or caption (getf attributes :float))
                 (format out "\\begin{figure}[~a]~%~a~%~a~%~@[\\caption{~a}~]~a~%\\end{figure}~%"
                         (or (getf attributes :placement) "htbp")
                         (case position (:left "\\raggedright") (:right "\\raggedleft") (t "\\centering"))
                         (org-latex-image (first children) attributes) caption (org-latex-name-label node)))
                (t (let ((environment (case position (:left "flushleft") (:right "flushright") (t "center"))))
                     (format out "\\begin{~a}~%~a~%\\end{~a}~%" environment
                             (org-latex-image (first children) attributes) environment)))))
        (let ((text (org-latex-objects-text node :newlines (org-export-option "\\n"))))
          (case (export-paragraph-align node)
            (:left (format out "{\\raggedright~%~a\\par}~%" text))
            (:right (format out "{\\raggedleft~%~a\\par}~%" text))
            (:centre (format out "{\\centering~%~a\\par}~%" text))
            (t (format out "~a~%" text)))))))

(defun org-latex-roman (n)
  "N, from 1 to 4, in small roman numerals, as LaTeX names its list counters (enumii)."
  (nth (1- n) '("i" "ii" "iii" "iv")))

(defun org-latex-enumerate-label (bullet)
  "How a numbered list whose first BULLET is as written labels its items, a
FORMAT string of the counter's name (\\alph{~a}) for a, \\Alph for A,
\\arabic for numbers, with the bullet's . or ); NIL for 1. as LaTeX has it."
  (let* ((bullet (string-trim '(#\Space #\Tab) (or bullet "")))
         (mark (and (plusp (length bullet)) (char bullet (1- (length bullet)))))
         (first (and (plusp (length bullet)) (char bullet 0))))
    (cond ((null first) nil)
          ((lower-case-p first) (format nil "\\alph{~~a}~c" mark))
          ((upper-case-p first) (format nil "\\Alph{~~a}~c" mark))
          ((eql mark #\)) "\\arabic{~a})")
          (t nil))))

(defun org-latex-plain-list (node out)
  "Write the plain list NODE to OUT, as itemize, enumerate or description;
in the enhanced export as the style its :EXPORT: drawer gives says (bullets,
none, numbers, letters, Letters, roman, Roman)."
  (let* ((style (export-list-style (org-export-setting node "style")))
         (numbered (third style))
         (environment (cond ((eq (org-property node :type) 'descriptive) "description")
                            (numbered "enumerate")
                            (style "itemize")
                            ((eq (org-property node :type) 'ordered) "enumerate")
                            (t "itemize")))
         (depth (count 'plain-list
                       (loop for up = (org-node-parent node) then (org-node-parent up) while up
                             collect (org-node-type up))
                       :test #'eq))
         (level (org-latex-roman (1+ (min depth 3)))))
    (format out "\\begin{~a}~%" environment)
    (cond (numbered
           (format out "\\renewcommand{\\labelenum~a}{~a}~%" level (format nil numbered (format nil "enum~a" level))))
          ((and (eq (second style) :none) (string= environment "itemize"))
           (format out "\\renewcommand{\\labelitem~a}{}~%" level)))
    ;; in the enhanced export a numbered list keeps its kind of number: a) A. 1)
    (when (and (org-export-enhanced *org-export*) (string= environment "enumerate") (not numbered)
               (eq (org-property node :type) 'ordered))
      (let ((label (org-latex-enumerate-label (org-property (first (org-node-children node)) :bullet))))
        (when label
          (format out "\\renewcommand{\\labelenum~a}{~a}~%" level (format nil label (format nil "enum~a" level))))))
    (dolist (item (org-node-children node))
      (when (eq (org-node-type item) 'item)
        (let ((counter (org-property item :counter))
              (checkbox (case (org-property item :checkbox)
                          (on "$\\boxtimes$") (off "$\\square$") (trans "$\\boxminus$")))
              (tag (org-property item :tag))
              (contents (string-trim '(#\Newline #\Space) (with-output-to-string (s) (org-latex-elements item s)))))
          (when (and (integerp counter) (string= environment "enumerate"))
            (format out "\\setcounter{enum~a}{~d}~%" level (1- counter)))
          ;; a checkbox in place of the bullet, before the tag of a description
          (let ((label (cond (tag (format nil "~@[~a ~]~a" checkbox (org-latex-string tag))) (checkbox checkbox))))
            (write-string (org-latex-mark item) out)
            (write-string "\\item" out)
            (cond (label (format out "[{~a}] " label))
                  ;; text beginning with [ is not taken for the item's label
                  ((eql #\[ (org-latex-visible-start contents)) (write-string " {}" out))
                  (t (write-char #\Space out)))
            (format out "~a~%" contents)))))
    (format out "\\end{~a}~%" environment)))

;;; Tables

(defun org-latex-column-letter (side)
  "The letter of LaTeX's column specification for a column aligned at SIDE
\(:LEFT, :RIGHT or :CENTRE): l, r or c."
  (ecase side (:left "l") (:right "r") (:centre "c")))

(defparameter *org-latex-text-characters* 78
  "About how many characters of the normal size go across the text, for the
enhanced export to tell a table too wide for it.")

(defun org-latex-least-width (piece)
  "How many characters of text a table column is at least as wide as, its
longest part that cannot go on to the next line PIECE characters long: a
character of code is a third wider than one of text, and a cell has room on
each side."
  (+ (ceiling (* 4 piece) 3) 2))

(defun org-latex-table-columns
    (node columns lengths widest wraps longest fitted table-width special standard cookies lines
     &optional (characters *org-latex-text-characters*))
  "The column specification of the table NODE in the enhanced export, its
COLUMNS each as its :EXPORT: drawer's widths and align say. Else every column a
share of TABLE-WIDTH, as wide as its widest text (WIDEST) is of theirs, when the
table is given a width. Else, when FITTED, the columns with words to wrap
\(WRAPS) share what the others leave in proportion to their text (LENGTHS, the
lengths of each column's cells). Each is at least as wide as its longest part
that cannot go on to the next line (LONGEST). The others are as wide as their
text, CHARACTERS going across the text in the type the table is printed in.
Lines between the columns when LINES \(TABLE-LINES) has them. SPECIAL, STANDARD
and COOKIES are as ORG-EXPORT-TABLE-SHAPE found them."
  (let ((widths (export-table-widths node columns))
        (aligns (tuatara.text:split-words (org-export-setting node "align")))
        (shares (and fitted
                     (org-latex-column-shares
                      (loop for i below columns for wrap in wraps
                            when wrap collect (/ (reduce #'+ (nth i lengths)) (max 1 (length (nth i lengths)))))
                      (loop for wrap in wraps for piece in longest when wrap collect (org-latex-least-width piece))
                      (- characters (* 2 (1- columns))
                         (loop for wrap in wraps for w in widest unless wrap sum w)))))
        (alignments (export-column-alignments node columns special standard cookies)))
    (flet ((letter (i) (org-latex-column-letter (aref alignments i)))
           (ragged (i) (case (export-side (nth i aligns))
                         (:right "\\raggedleft") (:centre "\\centering") (t "\\raggedright"))))
      (format nil (if (member :columns lines) "~{~a~^ | ~}" "~{~a~^ ~}")
              (cond (widths
                     ;; as the drawer says: a share, a length, * an equal share of what is left, auto as the text is
                     (loop for i below columns
                           for width in widths
                           collect (cond ((eq width :share) (format nil ">{~a\\arraybackslash}X" (ragged i)))
                                         ((or (eq width :auto) (null (org-latex-length width))) (letter i))
                                         (t (format nil ">{~a\\arraybackslash}p{~a}" (ragged i)
                                                    (org-latex-length width))))))
                    (table-width
                     ;; every column a share of the table's width, as wide as its widest text is of theirs
                     (loop for i below columns
                           collect (format nil ">{~a\\arraybackslash\\hsize=~,3f\\hsize}X" (ragged i)
                                           (/ (* columns (nth i widest)) (max 1 (reduce #'+ widest))))))
                    (t
                     (loop with left = shares
                           for i below columns
                           collect (if (and fitted (nth i wraps))
                                       (format nil ">{~a\\arraybackslash\\hsize=~,3f\\hsize}X" (ragged i) (pop left))
                                       (letter i)))))))))

(defun org-latex-column-shares (weights pieces room)
  "How much of the width the wrapping columns of a table each take, as
\\hsize does: numbers that add up to how many there are, in proportion to
WEIGHTS, yet each column at least as wide as its longest part that cannot
go on to the next line (PIECES), out of ROOM characters for them all."
  (let* ((n (length weights))
         (floors (mapcar (lambda (piece) (/ (* n piece) (max 1 room))) pieces)))
    (if (>= (reduce #'+ floors) n)
        ;; even their unbreakable parts do not fit: each in proportion to those
        (mapcar (lambda (least) (/ (* n least) (reduce #'+ floors))) floors)
        (let ((pinned (make-list n :initial-element nil)))
          ;; the columns whose share would be narrower than their floor take
          ;; the floor; the others share what is left, until none is too narrow
          (loop
           (let* ((free (- n (loop for p in pinned for f in floors when p sum f)))
                  (total (loop for p in pinned for w in weights unless p sum w))
                  (shares (loop for p in pinned for w in weights for f in floors
                                collect (cond (p f) ((plusp total) (/ (* free w) total)) (t 0))))
                  (narrow (loop for p in pinned for s in shares for f in floors
                                collect (and (not p) (< s f)))))
             (cond ((every #'identity pinned)
                    (return (mapcar (lambda (least) (/ (* n least) (reduce #'+ floors))) floors)))
                   ((some #'identity narrow)
                    (setf pinned (mapcar (lambda (p q) (or p q)) pinned narrow)))
                   (t (return shares)))))))))

(defun org-latex-table-body (out rows columns special lines repeat caption rule-across)
  "Write ROWS, a table's, to OUT in the enhanced export, each of its COLUMNS
\(SPECIAL true when the first column, only marking rows, is left out), with the
lines LINES (TABLE-LINES) asks for, each rule drawn by RULE-ACROSS, a function
of :TOP, :MIDDLE or :BOTTOM. Its header is in bold, and with REPEAT again at the
top of each page, under CAPTION continued when it has one."
  (let* ((header (export-table-header rows))
         (top (member :top lines))
         (under-header (and (or (member :header lines) (member :rows lines)) (funcall rule-across :middle))))
    (when top (format out "~a~%" (funcall rule-across :top)))
    (loop for tail on rows
          for row = (car tail)
          for next = (cadr tail)
          for last = (null (cdr tail))
          for index from 0
          do (cond ((eq (org-property row :type) 'rule)
                    (cond ((and (eql index header) repeat caption)
                           ;; the caption once; on the pages after, the header again under the caption continued
                           (format out "~@[~a~%~]\\endfirsthead~%\\caption[]{~a (continued)}\\\\~%~@[~a~%~]"
                                   under-header caption (and top (funcall rule-across :top)))
                           (loop for above in rows
                                 for i below header
                                 unless (eq (org-property above :type) 'rule)
                                 do (org-latex-row-line out above special columns t))
                           (format out "~@[~a~%~]\\endhead~%" under-header))
                          ((eql index header) (format out "~@[~a~%~]~:[~;\\endhead~%~]" under-header repeat))
                          ((or (zerop index) last) nil)
                          ((or (member :rules lines) (member :rows lines))
                           (format out "~a~%" (funcall rule-across :middle)))))
                   (t (org-latex-row-line out row special columns (and header (< index header)))
                      ;; between every row: under each but the last and those a rule follows
                      (when (and (member :rows lines) next (not (eq (org-property next :type) 'rule)))
                        (format out "~a~%" (funcall rule-across :middle))))))
    (when (member :bottom lines) (format out "~a~%" (funcall rule-across :bottom)))))

(defun org-latex-table-run-limit (columns special standard)
  "The most characters of code in a cell of the table with COLUMNS columns
\(SPECIAL and STANDARD as ORG-EXPORT-TABLE-SHAPE found them) the enhanced
export keeps together (*LATEX-CODE-RUN-LIMIT*): the usual, or fewer, the
first that lets the table, in the smallest type it is set in, go across the
text; the fewest when none does. A second value is the measure of the
columns at that limit (EXPORT-TABLE-MEASURE), as a list."
  (let ((limits (list *latex-code-run-limit* 10 6)))
    (loop for (limit . more) on limits
          do (let* ((*latex-code-run-limit* limit)
                    (measure (multiple-value-list (export-table-measure columns special standard))))
               (destructuring-bind (lengths widest wraps longest) measure
                 (declare (ignore lengths))
                 (when (or (null more)
                           (<= (+ (loop for w in widest for wrap in wraps unless wrap sum w) (* 2 (1- columns))
                                  (loop for wrap in wraps for piece in longest when wrap
                                        sum (max 12 (org-latex-least-width piece))))
                               (* 1.25 *org-latex-text-characters*)))
                   (return (values limit measure))))))))

(defun org-latex-fitted-table (node out rows special standard columns cookies &optional measure)
  "Write the table NODE to OUT in the enhanced export, laid out to fit: as
wide as it is when that fits across the text, else as wide as the text, the
columns with words in them wrapping and sharing what the others leave, in
proportion to their text; in smaller type when even that is too wide. It
breaks across pages, the rows above its first rule (its header) in bold,
again at the top of each page. Its :EXPORT: drawer may give each column's
width and alignment, the table's width (full: margin to margin, its columns
sharing it as their text does), its position (left, centre or right),
whether the header repeats, and the lines printed (TABLE-LINES)
\(ORG-EXPORT-SETTING). ROWS, SPECIAL, STANDARD, COLUMNS and COOKIES are as
ORG-LATEX-TABLE found them, MEASURE the columns' EXPORT-TABLE-MEASURE as a
list when it has been made."
  (multiple-value-bind (lengths widest wraps longest)
      (values-list (or measure (multiple-value-list (export-table-measure columns special standard))))
    (let* ((natural (+ (reduce #'+ widest) (* 2 (1- columns))))
           ;; the least it can be: the columns with words to wrap no narrower than their longest part kept whole
           (fixed (+ (loop for w in widest for wrap in wraps unless wrap sum w) (* 2 (1- columns))
                     (loop for wrap in wraps for piece in longest
                           when wrap sum (max 12 (org-latex-least-width piece)))))
           (widths (export-table-widths node columns))
           (lines (table-lines node))
           ;; with lines down the table, rules that meet them (booktabs' leave gaps where they cross)
           (upright (intersection lines '(:left :right :columns)))
           (rule-across (lambda (kind)
                          (if upright "\\hline" (ecase kind
                                                  (:top "\\toprule")
                                                  (:middle "\\midrule")
                                                  (:bottom "\\bottomrule")))))
           (font (cond (widths nil)
                       ((or (<= natural *org-latex-text-characters*) (<= fixed *org-latex-text-characters*)) nil)
                       ((<= fixed (* 1.12 *org-latex-text-characters*)) "\\small")
                       (t "\\footnotesize")))
           ;; how many characters go across the text in the type the table is printed in
           (characters (* *org-latex-text-characters* (cond ((equal font "\\small") 1.12) (font 1.25) (t 1))))
           ;; the table's own width (full: margin to margin), its columns sharing it as their text does
           (table-width (let ((w (org-export-setting node "width")))
                          (and w (if (string-equal w "full") "\\textwidth" (org-latex-length w)))))
           (fitted (cond (widths (and (or table-width (member :share widths)) t))
                         (table-width t)
                         (t (and (> natural (* (if font 1.12 1) *org-latex-text-characters*))
                                 (some #'identity wraps)))))
           (align (org-latex-table-columns node columns lengths widest wraps longest fitted table-width special
                                           standard cookies lines
                                           characters))
           ;; in columns a table is kept whole, a float of its own: no header repeated on later pages
           (in-columns *org-latex-in-columns*)
           (position (and (not in-columns)
                          (case (export-position node)
                            (:left "{\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{\\fill}")
                            (:right "{\\setlength{\\LTleft}{\\fill}\\setlength{\\LTright}{0pt}"))))
           (repeat (and (not in-columns) (not (export-setting-no-p node "repeat header"))))
           ;; rows a little taller between lines that meet, as booktabs' rules leave room
           (font (if upright (format nil "~@[~a~]\\renewcommand{\\arraystretch}{1.2}" font) font))
           (caption (org-latex-caption node))
           (environment (cond (in-columns (if fitted "tabularx" "tabular")) (fitted "xltabular") (t "longtable")))
           (left (if (member :left lines) "|" "@{}"))
           (right (if (member :right lines) "|" "@{}")))
      (if in-columns
          (progn
            (format out "~@[{~a~%~]\\begin{table}[H]~%" font)
            (when caption (format out "\\caption{~a}~a~%" caption (org-latex-name-label node)))
            (format out "\\begin{~a}~:[~*~;{~a}~]{~a~a~a}~%" environment fitted (or table-width "\\linewidth") left
                    align right))
          (progn
            (format out "~@[{~a~%~]~@[~a~%~]\\begin{~a}~:[~*~;{~a}~]{~a~a~a}~%"
                    font position environment fitted (or table-width "\\textwidth") left align right)
            (when caption
              (format out "\\caption{~a}~a\\\\~%" caption (org-latex-name-label node)))))
      (org-latex-table-body out rows columns special lines repeat caption rule-across)
      (format out "\\end{~a}~%~:[~;\\end{table}~%~]~:[~;}~%~]~:[~;}~%~]" environment in-columns position font))))

(defun org-latex-row-start (texts)
  "TEXTS, the cells of a table's row, with an empty group before the first
when it begins with [, which LaTeX would read as a length after the row
before it ends (\\\\[2mm])."
  (let ((first (first texts)))
    (if (and first (plusp (length first)) (char= (char first 0) #\[))
        (cons (concatenate 'string "{}" first) (rest texts))
        texts)))

(defun org-latex-row-line (out row special columns &optional bold)
  "Write the table row ROW to OUT as a line of LaTeX: its mark, its cells
padded to COLUMNS and joined by &, then the end of the row; SPECIAL true when
the first column, only marking rows, is left out; BOLD for a header's row."
  (format out "~a~{~a~^ & ~}\\\\~%" (org-latex-mark row)
          (org-latex-row-start
           (loop with cells = (nthcdr (if special 1 0) (org-node-children row))
                 for i below columns
                 for cell = (pop cells)
                 for text = (if cell (org-latex-objects-text cell) "")
                 collect (if (and bold (plusp (length text))) (format nil "\\textbf{~a}" text) text)))))

(defun org-latex-table (node out)
  "Write the table NODE to OUT, laid out as its #+ATTR_LATEX says: its
environment (tabular, tabularx, tabulary, longtable), :width, :align,
:placement, :center, :font and :booktabs, and captioned by #+CAPTION. In the
enhanced export a table with no :environment or :align of its own is laid
out to fit (ORG-LATEX-FITTED-TABLE)."
  (if (not (eq (org-property node :type) 'org))
      (org-latex-verbatim (subseq *org-export-text* (org-node-post-affiliated node)
                                  (- (org-node-end node) (org-node-post-blank node)))
                          out)
      (multiple-value-bind (rows special standard cookies columns) (org-export-table-shape node)
        (let* ((attributes (org-export-attributes node "latex"))
               (environment (or (getf attributes :environment) "tabular"))
               (width (getf attributes :width))
               (booktabs (let ((b (getf attributes :booktabs))) (and b (not (string-equal b "nil")))))
               (align (or (getf attributes :align)
                          (format nil "~{~a~}" (map 'list #'org-latex-column-letter
                                                    (table-column-alignments columns (if special 1 0)
                                                                             standard cookies)))))
               (caption (org-latex-caption node))
               (placement (or (getf attributes :placement) "htbp"))
               (centred (not (equal (getf attributes :center) "nil")))
               (font (getf attributes :font))
               (fit (and (org-export-enhanced *org-export*)
                         (not (getf attributes :environment))
                         (not (getf attributes :align))))
               (body (with-output-to-string (s)
                       (loop for (row . more) on rows
                             for first = t then nil
                             do (if (eq (org-property row :type) 'rule)
                                    (format s "~a~%" (cond ((not booktabs) "\\hline")
                                                           (first "\\toprule")
                                                           ((null more) "\\bottomrule")
                                                           (t "\\midrule")))
                                    (org-latex-row-line s row special columns)))))
               (sized (member environment '("tabularx" "tabulary") :test #'string=))
               (table (format nil "\\begin{~a}~:[~*~;{~a}~]{~a}~%~a\\end{~a}~%"
                              environment sized (or width "\\textwidth") align body environment)))
          (cond (fit (multiple-value-bind (limit measure) (org-latex-table-run-limit columns special standard)
                       (let ((*latex-code-run-limit* limit))
                         (org-latex-fitted-table node out rows special standard columns cookies measure))))
                ((string= environment "longtable")
                 (format out "~@[~a~%~]\\begin{longtable}{~a}~%~@[\\caption{~a}~a\\\\~%~]~a\\end{longtable}~%"
                         font align caption (if caption (org-latex-name-label node) "") body))
                ((or caption (getf attributes :float))
                 (format out "\\begin{table}[~a]~%~@[\\caption{~a}~]~a~%~:[~;\\centering~%~]~@[~a~%~]~a\\end{table}~%"
                         placement caption (org-latex-name-label node) centred font table))
                (centred (format out "\\begin{center}~%~@[~a~%~]~a\\end{center}~%" font table))
                (t (format out "~@[{~a~%~]~a~:[~;}~%~]" font table font)))))))

;;; Objects

(defun org-latex-objects-between (objects begin end out &key newlines)
  "Write the text from BEGIN below END to OUT, OBJECTS, the objects in it, as LaTeX and the rest escaped."
  (let ((special (org-export-option "-")))
    (org-export-objects-between objects begin end out
                                :text (lambda (s) (latex-escape s :newlines newlines :special-strings special
                                                                :quotes (and *org-export*
                                                                             (org-export-enhanced *org-export*))))
                                :object #'org-latex-object
                                :blank (lambda (n) (spaces n)))))

(defun org-latex-objects-text (node &key newlines)
  "The contents of NODE, text and objects, as LaTeX, without the line break at its end."
  (if (org-node-contents-begin node)
      (string-right-trim '(#\Newline)
                         (with-output-to-string (s)
                           (org-latex-objects-between (org-node-children node) (org-node-contents-begin node)
                                                      (org-node-contents-end node) s :newlines newlines)))
      ""))

(defun org-latex-string (string)
  "STRING, a line of Org text such as a keyword's value, as LaTeX (ORG-PARSE-LINE)."
  (let ((*org-export-text* string))
    (org-latex-objects-text (org-parse-line string))))

(defun org-latex-link (node out)
  "Write the link NODE to OUT: to the web as \\href, a picture shown, a place
in the document as \\hyperref with its description. One with none shows what
it leads to by its number (3, by \\ref), and in the enhanced export by its kind
and number (section 3, Table 2, Figure 1, by \\autoref); one to a heading with no
number by its title, to a target with none by its words. Anything else as its
description (ORG-EXPORT-LINK-KIND)."
  (let ((path (org-property node :path))
        (description (and (org-node-contents-begin node) (org-latex-objects-text node))))
    (multiple-value-bind (kind value) (org-export-link-kind node #'org-latex-image-link-p)
      (ecase kind
        (:picture (write-string (org-latex-image node (org-export-attributes (org-node-parent node) "latex")) out))
        (:address (if description
                      (format out "\\href{~a}{~a}" (latex-url value) description)
                      (format out "\\url{~a}" (latex-url value))))
        (:file (format out "\\href{~a}{~a}" (latex-url (org-export-file-address value))
                       (or description (latex-escape value))))
        ((:heading :target)
         (cond ((and (null description)
                     (if (eq kind :heading)
                         (org-export-numbered-p value)
                         ;; an element has a number when captioned, a target that of where it stands
                         (or (not (org-export-enhanced *org-export*)) (org-latex-caption value))))
                (format out "\\~:[ref~;autoref~]{~a}" (org-export-enhanced *org-export*) (org-latex-label value)))
               ((eq kind :heading)
                (format out "\\hyperref[~a]{~a}" (org-latex-label value)
                        (or description (with-output-to-string (s)
                                          (let ((*org-export-text* *org-export-text*))
                                            (org-latex-objects-between (org-property value :title)
                                                                       (org-property value :title-begin)
                                                                       (org-property value :title-end) s))))))
               (t (format out "\\hyperref[~a]{~a}" (org-latex-label value) (or description (latex-escape path))))))
        (:plain (write-string (or description (latex-escape value)) out))))))

(defun org-latex-footnote (node out)
  "Write the footnote reference NODE to OUT as \\footnote with its definition."
  (let ((definition (org-export-footnote-text node #'org-latex-objects-text
                                               (lambda (found)
                                                 (string-trim '(#\Newline #\Space)
                                                              (with-output-to-string (s)
                                                                (org-latex-elements found s)))))))
    (when (org-export-option "f")
      (format out "\\footnote{~a}" (or definition "")))))

(defun org-latex-as-written (node)
  "The object NODE as it stands in the text, without the spaces after it, as LaTeX."
  (latex-escape (subseq *org-export-text* (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))))

(defun org-latex-object (node out)
  "Write the object NODE to OUT."
  (case (org-node-type node)
    (bold (format out "\\textbf{~a}" (org-latex-objects-text node)))
    (italic (format out "\\emph{~a}" (org-latex-objects-text node)))
    (underline (format out "\\uline{~a}" (org-latex-objects-text node)))
    (strike-through (format out "\\sout{~a}" (org-latex-objects-text node)))
    ((code verbatim) (write-string (latex-texttt (org-property node :value)) out))
    (inline-src-block (write-string (latex-texttt (org-property node :value)) out))
    (entity (let ((entry (gethash (org-property node :name) *org-entities-latex*)))
              (if (and entry (org-export-option "e"))
                  (format out (if (cdr entry) "\\(~a\\)" "~a~:[~;{}~]") (car entry)
                          (and (not (cdr entry)) (alpha-char-p (char (car entry) (1- (length (car entry)))))))
                  (write-string (org-latex-as-written node) out))))
    (latex-fragment (write-string (org-property node :value) out))
    ;; \relax, as a line after the break beginning with [ would be read as a length
    (line-break (format out "\\\\\\relax~%"))
    (export-snippet (when (member (org-property node :back-end) '("latex" "tex" "beamer") :test #'string-equal)
                      (write-string (org-property node :value) out)))
    (footnote-reference (org-latex-footnote node out))
    (link (org-latex-link node out))
    (target (format out "\\label{~a}" (org-latex-label node)))
    (radio-target (format out "\\label{~a}~a" (org-latex-label node) (org-latex-objects-text node)))
    (timestamp (when (org-export-option "<")
                 (format out "\\textit{~a}" (latex-escape (org-property node :raw-value)))))
    (statistics-cookie (write-string (latex-escape (org-property node :value)) out))
    ((subscript superscript)
     (if (org-export-script-p node (if (org-export-enhanced *org-export*) (org-option-default "^") t))
         (format out (if (eq (org-node-type node) 'subscript) "\\textsubscript{~a}" "\\textsuperscript{~a}")
                 (org-latex-objects-text node))
         (write-string (org-latex-as-written node) out)))
    ((inline-babel-call) nil)
    ;; a call in a keyword's value (the text's own are expanded before it is read)
    (macro (write-string (org-export-macro-expansion node (org-latex-as-written node) #'org-latex-string) out))
    (citation (org-latex-citation node out))
    ;; what is left as written: a macro not known
    (t (write-string (org-latex-as-written node) out))))

;;; Citations

(defparameter *org-cite-names*
  '(("a" . "author") ("t" . "text") ("n" . "nocite") ("na" . "noauthor") ("l" . "locators")
    ("b" . "bare") ("c" . "caps") ("f" . "full") ("bc" . "bare-caps"))
  "The short names of the styles and variants of a citation ([cite/t/c:@key]
is of style text, variant caps), each (SHORT . NAME).")

(defun org-latex-cite-export ()
  "The way of making a bibliography the document's #+CITE_EXPORT: names,
biblatex, natbib or bibtex, in lower case, and the bibliography style and the
citation style it names after it, as three values; NIL when it names none of
those. A style written BIBSTYLE/CITESTYLE gives both."
  (let ((words (tuatara.text:split-words (or (org-export-keyword "CITE_EXPORT") ""))))
    (when (and words (member (string-downcase (first words)) '("biblatex" "natbib" "bibtex") :test #'string=))
      (let* ((style (second words))
             (slash (and style (position #\/ style))))
        (values (string-downcase (first words))
                (if slash (subseq style 0 slash) style)
                (or (third words) (and slash (subseq style (1+ slash)))))))))

(defun org-latex-bibliographies ()
  "The full names of the bibliography files the document's #+BIBLIOGRAPHY:
lines name, a relative one taken from the note's directory (ORG-EXPORT-PATH), so
that they are found wherever the LaTeX file is written. One named so that LaTeX
cannot take it (LATEX-FILE-NAME-P), one not there (with .bib added when it
names none, as BibTeX reads it), or one outside the directories an export may
take files from (ORG-EXPORT-FILE-ALLOWED-P) is left out, which is said. Those
taken are kept in the note's BIBLIOGRAPHY-FILES, to be shown to the programs
reading them."
  (loop for value in (org-export-keyword-all "BIBLIOGRAPHY")
        for name = (string-trim " \"" value)
        for full = (and (plusp (length name))
                        (or (ignoring-errors-reported ("expanding ~a" name) (org-export-path name)) name))
        for read = (and full (if (find #\. (tuatara.io:native-file-name full)) full (concatenate 'string full ".bib")))
        when (and full (not (latex-file-name-p full)))
          do (export-note "~a, so left out" (latex-file-name-refusal name))
        when (and full (latex-file-name-p full) (null (tuatara.io:path-stamp read :failure nil)))
          do (export-note "~a: no such bibliography, so left out" name)
        when (and full (latex-file-name-p full) (tuatara.io:path-stamp read :failure nil)
                  (org-export-file-allowed-p read))
          do (when (org-latex-p *org-export*)
               (pushnew read (org-latex-bibliography-files *org-export*) :test #'string=))
          and collect full))

(defun org-latex-citation-packages ()
  "The lines of the preamble the document's #+CITE_EXPORT: calls for: biblatex,
in the styles it names, with each of its bibliographies; natbib; or nothing."
  (multiple-value-bind (way bib-style cite-style) (org-latex-cite-export)
    (cond ((equal way "biblatex")
           (format nil "\\usepackage~@[[~a]~]{biblatex}~%~{\\addbibresource{~a}~%~}"
                   (cond ((and bib-style cite-style) (format nil "bibstyle=~a,citestyle=~a" bib-style cite-style))
                         ((or bib-style cite-style) (format nil "style=~a" (or bib-style cite-style))))
                   (org-latex-bibliographies)))
          ((equal way "natbib") (format nil "\\usepackage{natbib}~%"))
          (t ""))))

(defun org-latex-print-bibliography ()
  "The bibliography where #+PRINT_BIBLIOGRAPHY: stands, as the document's
#+CITE_EXPORT: makes it: \\printbibliography for biblatex; for bibtex and natbib
its style (plain, plainnat as standard) and its files; nothing for none."
  (multiple-value-bind (way style) (org-latex-cite-export)
    (cond ((null way) "")
          ((string= way "biblatex") (format nil "\\printbibliography~%"))
          (t (format nil "\\bibliographystyle{~a}~%\\bibliography{~{~a~^,~}}~%"
                     (or style (if (string= way "natbib") "plainnat" "plain"))
                     (mapcar (lambda (file)
                               (if (uiop:string-suffix-p file ".bib") (subseq file 0 (- (length file) 4)) file))
                             (org-latex-bibliographies)))))))

(defun org-cite-command (way style variant)
  "The LaTeX command, without its backslash, a citation of STYLE and VARIANT
\(full names, *ORG-CITE-NAMES*; NIL for none) is written as for WAY (biblatex,
natbib or bibtex), as Org writes it."
  (let* ((bare (member variant '("bare" "bare-caps") :test #'equal))
         (caps (member variant '("caps" "bare-caps") :test #'equal))
         (command (cond ((equal style "nocite") "nocite")
                        ((string= way "bibtex") "cite")
                        ((string= way "biblatex")
                         (cond ((equal style "author") "citeauthor")
                               ((equal style "text") "textcite")
                               ((equal style "noauthor") "autocite*")
                               ((equal style "locators") "notecite")
                               (bare "cite")
                               (t "autocite")))
                        (t (cond ((equal style "author") "citeauthor")
                                 ((equal style "text") (if bare "citealt" "citet"))
                                 ((equal style "noauthor") "citeyearpar")
                                 (bare "citealp")
                                 (t "citep"))))))
    (concatenate 'string
                 (if (and caps (string/= command "nocite")) (string-capitalize command :end 1) command)
                 (if (and (string= way "natbib") (equal variant "full") (not (find #\* command))) "*" ""))))

(defun org-latex-citation (node out)
  "Write the citation NODE ([cite/STYLE/VARIANT:global prefix;prefix @key
suffix;...;global suffix]) to OUT as the LaTeX command of its style for the
document's #+CITE_EXPORT: (ORG-CITE-COMMAND), as Org writes it. For biblatex
one reference with no global notes is \\autocite[prefix][suffix]{key},
several with no notes \\autocite{key,...}, and with notes the command's form
for many, \\autocites(global
prefix)(global suffix)[prefix][suffix]{key}...; for natbib and bibtex the keys
are given together, with the notes of the one reference or the global ones
\(bibtex has the one after alone). Without #+CITE_EXPORT: naming one of those,
as written."
  (let* ((way (org-latex-cite-export))
         (parts (uiop:split-string (subseq *org-export-text*
                                           (org-node-contents-begin node)
                                           (org-node-contents-end node))
                                   :separator ";"))
         (keyed (mapcar (lambda (part) (and (cl-ppcre:scan *org-citation-key-re* part) t)) parts))
         ;; a key LaTeX would read as markup ends the PDF run: the citation written as text instead
         (unfit (loop for part in parts
                      for key-p in keyed
                      when key-p
                        do (multiple-value-bind (start end) (cl-ppcre:scan *org-citation-key-re* part)
                             (when (find-if (lambda (c) (find c "%#{}\\~^$&")) part :start (1+ start) :end end)
                               (return (subseq part (1+ start) end)))))))
    (when (and way unfit)
      (export-note "@~a: LaTeX cannot take a citation key with % # { } \\ ~~ ^ $ or & in it, so the citation is ~
written as text" unfit))
    (if (or (null way) (notany #'identity keyed) unfit)
        (write-string (latex-escape (subseq *org-export-text*
                                            (org-node-begin node)
                                            (- (org-node-end node) (org-node-post-blank node))))
                      out)
        (flet ((note (text) (let ((text (string-trim " " text))) (and (plusp (length text)) (latex-escape text))))
               (full (name) (or (cdr (assoc name *org-cite-names* :test #'string=)) name)))
          (let* ((global-prefix (and (not (first keyed)) (first parts)))
                 (global-suffix (and (not (car (last keyed))) (rest parts) (car (last parts))))
                 (references (loop for part in parts for key-p in keyed
                                   when key-p
                                   collect (multiple-value-bind (start end) (cl-ppcre:scan *org-citation-key-re* part)
                                             (list (note (subseq part 0 start))
                                                   (subseq part (1+ start) end)
                                                   (note (subseq part end))))))
                 (style-words (let ((style (org-property node :style)))
                                (and style (uiop:split-string style :separator "/"))))
                 (command (org-cite-command way (and (first style-words) (full (first style-words)))
                                            (and (second style-words) (full (second style-words)))))
                 (gpre (and global-prefix (note global-prefix)))
                 (gpost (and global-suffix (note global-suffix))))
            (flet ((notes (before after)
                     ;; [before][after], [after] alone, [before][] or nothing
                     (format nil "~:[~*~;[~a]~]~:[~*~;[~a]~]" before before (or after before) (or after ""))))
              (cond ((string= command "nocite")
                     (format out "\\nocite{~{~a~^,~}}" (mapcar #'second references)))
                    ((string= way "biblatex")
                     (cond
                       ((and (null (rest references)) (null gpre) (null gpost))
                        (destructuring-bind (before key after) (first references)
                          (format out "\\~a~a{~a}" command (notes before after) key)))
                       ;; several references with no notes: one command for them all
                       ((notany (lambda (reference) (or (first reference) (third reference) gpre gpost)) references)
                        (format out "\\~a{~{~a~^,~}}" command (mapcar #'second references)))
                       (t
                        (if (member command '("autocite" "Autocite" "textcite" "Textcite" "cite" "Cite")
                                    :test #'string=)
                            (format out "\\~as~:[~2*~;(~a)(~a)~]~{~a~}" command
                                    (or gpre gpost) (or gpre "") (or gpost "")
                                    (loop for (before key after) in references
                                          collect (format nil "~a{~a}" (notes before after) key)))
                            (format out "\\~a{~{~a~^,~}}" command (mapcar #'second references))))))
                    (t
                     ;; the notes of the one reference, or the global ones
                     (multiple-value-bind (before after)
                         (if (rest references)
                             (values gpre gpost)
                             (values (or (first (first references)) gpre) (or (third (first references)) gpost)))
                       (format out "\\~a~a{~{~a~^,~}}" command
                               (if (string= way "bibtex") (if after (format nil "[~a]" after) "") (notes before after))
                               (mapcar #'second references)))))))))))

;;; The document

(defparameter *org-latex-landscape*
  "\\makeatletter
\\newcommand\\tuatara@turn{\\clearpage
  \\setlength{\\@tempdima}{\\paperwidth}\\setlength{\\paperwidth}{\\paperheight}\\setlength{\\paperheight}{\\@tempdima}%
  \\ifdefined\\pdfpagewidth\\pdfpagewidth=\\paperwidth\\pdfpageheight=\\paperheight\\fi
  \\ifdefined\\pagewidth\\pagewidth=\\paperwidth\\pageheight=\\paperheight\\fi
  \\setlength{\\@tempdimb}{\\paperwidth}\\addtolength{\\@tempdimb}{-\\paperheight}%
  \\addtolength{\\textwidth}{\\@tempdimb}\\addtolength{\\textheight}{-\\@tempdimb}%
  \\setlength{\\hsize}{\\textwidth}\\setlength{\\columnwidth}{\\textwidth}\\setlength{\\linewidth}{\\textwidth}%
  \\setlength{\\vsize}{\\textheight}\\setlength{\\@colht}{\\textheight}\\setlength{\\@colroom}{\\textheight}%
  \\ifdefined\\headwidth\\setlength{\\headwidth}{\\textwidth}\\fi}
\\newenvironment{tuataralandscape}{\\tuatara@turn}{\\tuatara@turn}
\\makeatother
"
  "LaTeX for pages turned sideways (a page: landscape setting), with the paper's
width and height swapped from a page of their own on and back after, the
text as much wider and shorter, so that headers, footers, columns and tables
lie across the page as a Word document's landscape section has them.")

(defun org-latex-packages (engine)
  "The packages every document loads, for the LaTeX program ENGINE."
  (format nil "~:[\\usepackage[utf8]{inputenc}~%\\usepackage[T1]{fontenc}~%~;~]~
\\usepackage{graphicx}~%\\usepackage{longtable}~%\\usepackage{wrapfig}~%\\usepackage{rotating}~%~
\\usepackage[normalem]{ulem}~%\\usepackage{amsmath}~%\\usepackage{amssymb}~%~:[~;\\usepackage{fontspec}~%~]~
\\usepackage{capt-of}~%\\usepackage{hyperref}~%"
          (member engine '("xelatex" "lualatex") :test #'string-equal)
          (member engine '("xelatex" "lualatex") :test #'string-equal)))

(defparameter *org-latex-enhanced-packages*
  "\\usepackage{booktabs}~%\\usepackage{xltabular}~%\\usepackage{multicol}~%~
   \\usepackage{float}~%\\usepackage{xcolor}~%\\usepackage{fvextra}~%~
   \\usepackage{pict2e}~%~a~
   \\DeclareRobustCommand{\\redactedbox}[1]{{\\sbox0{#1}~
   \\rule[-0.8ex]{\\wd0}{2.5ex}}}~%~
   \\DeclareRobustCommand{\\breakmark}{\\hbox{\\setlength{\\unitlength}{0.07em}~
   \\linethickness{0.035em}\\begin{picture}(10,4)(0,-0.5)~
   \\qbezier(2,3)(0.5,3)(0.5,2)\\qbezier(0.5,2)(0.5,1)(2,1)~
   \\put(2,1){\\line(1,0){7.5}}~
   \\put(9.5,1){\\line(-3,2){2}}\\put(9.5,1){\\line(-3,-2){2}}\\end{picture}}}~%~
   \\fvset{breaksymbolleft=\\breakmark}~%~
   \\DeclareRobustCommand{\\codebreak}{\\discretionary{}{\\breakmark}{}}~%~
   \\pdfstringdefDisableCommands{\\let\\codebreak\\relax}~%~
   \\setlength{\\emergencystretch}{3em}~%\\tolerance=1000~%"
  "What the enhanced export's documents load and define besides what Org's
do, a FORMAT control taking *ORG-LATEX-LANDSCAPE*: tables across pages, code
that breaks its lines with a mark, the black boxes of hidden text.")

(defun org-latex-field (body)
  "A function giving the value of each place of a template
\(FILL-LATEX-TEMPLATE) for the document being written, BODY its text as LaTeX:
the document's title, author, date, class, packages, fonts, headers, table of
contents and PDF facts, and any keyword of its own (KEY for #+KEY:). That is Org
text written as LaTeX (several lines of it joined by \\newline), as written
\(:RAW, joined by line breaks) or as a file's full name (:PATH). The code font
has its ligatures off, so that :: and // in code are copied from the PDF as they
are written."
  (let* ((engine (org-latex-engine *org-export*))
         (title (org-export-title))
         (author (org-export-author))
         (date (and (org-export-option "date") (org-export-keyword "DATE")))
         (toc (org-export-option "toc"))
         (num (org-export-option "num"))
         (language (or (org-export-keyword "LANGUAGE") *org-default-language*)))
    (lambda (name way)
      (flet ((keyword-values () (org-export-keyword-all (string-upcase name))))
        (cond
          ((string= name "body") body)
          ((string= name "created") (org-export-created))
          ((string= name "engine") engine)
          ((string= name "latex_class") (or (org-export-keyword "LATEX_CLASS") *org-latex-default-class*))
          ((string= name "latex_class_options")
           (or (org-export-keyword "LATEX_CLASS_OPTIONS") *org-latex-default-class-options*))
          ((string= name "packages")
           (concatenate 'string (org-latex-packages engine)
                        (if (org-latex-tables-of-contents-p) (format nil "\\usepackage{etoc}~%") "")
                        (org-latex-language-setup engine)
                        (org-latex-citation-packages)
                        (if (org-export-enhanced *org-export*)
                            (format nil *org-latex-enhanced-packages* *org-latex-landscape*)
                            "")))
          ((string= name "latex_header")
           (format nil "~{~a~%~}" (append (org-export-keyword-all "LATEX_HEADER")
                                          (org-export-keyword-all "LATEX_HEADER_EXTRA"))))
          ((string= name "secnumdepth") (and (integerp num) (princ-to-string (org-latex-depth num))))
          ((string= name "fonts")
           (format nil "~:{\\set~(~a~)font~a{~a}~%~}"
                   (loop for (part . family) in (org-latex-fonts *org-export*)
                         collect (list part
                                       (if (eq part :mono)
                                           "[Scale=MatchLowercase,Contextuals=NoAlternate,Ligatures=NoCommon]"
                                           "")
                                       family))))
          ((string= name "font_fallbacks")
           ;; itself for bold and italic: a font of symbols has no such faces, whatever a setup file's defaults say
           (format nil "~:{\\newfontfamily\\~a{~a}[BoldFont={~a},ItalicFont={~a},BoldItalicFont={~a}]~%~}"
                   (mapcar (lambda (f) (list (cdr f) (car f) (car f) (car f) (car f)))
                           (org-latex-fallbacks *org-export*))))
          ((string= name "title") (if (eq way :raw) title (org-latex-string title)))
          ((string= name "author") (if author (if (eq way :raw) author (org-latex-string author)) ""))
          ((string= name "date") (if date (if (eq way :raw) date (org-latex-string date)) ""))
          ((string= name "maketitle") (and (org-export-option "title") (plusp (length title)) "t"))
          ((string= name "toc") (and toc "t"))
          ((string= name "tocdepth")
           (let ((depth (org-export-contents-depth))) (and depth (princ-to-string (org-latex-depth depth)))))
          ((string= name "pdfauthor") (latex-escape (org-export-plain (or author ""))))
          ((string= name "pdftitle") (latex-escape (org-export-plain title)))
          ((string= name "pdfkeywords") (latex-escape (org-export-plain (or (org-export-keyword "KEYWORDS") ""))))
          ((string= name "pdfsubject") (latex-escape (org-export-plain (or (org-export-keyword "DESCRIPTION") ""))))
          ((string= name "pdflang") (if (string-equal language "en") "English" (latex-escape language)))
          ((null (keyword-values)) nil)
          ;; a picture the note names from where it may not come: its place and its conditions empty
          ((member name (org-export-refused *org-export*) :test #'string=) nil)
          ((progn (org-refuse-hidden-as-written name way (keyword-values)) nil))
          ((eq way :raw) (format nil "~{~a~^~%~}" (keyword-values)))
          ((eq way :path)
           (let ((path (template-picture-path (first (keyword-values)))))
             (when path (push path (org-latex-paths *org-export*)))
             path))
          (t (format nil "~{~a~^\\newline ~}" (mapcar #'org-latex-string (keyword-values)))))))))

(defun org-latex-parts (text &key file (engine "xelatex") enhanced lines)
  "TEXT, an Org document, FILE its file or NIL, written as a LaTeX document for
the LaTeX program ENGINE (#+LATEX_COMPILER chooses another), as the built-in
template org lays it out (FILL-LATEX-TEMPLATE); with ENHANCED, Tuatara's own
improvements on what Org writes, in the template #+TEMPLATE names
\(ORG-TEMPLATE-NAME, LATEX-TEMPLATE-FOR). LINES, for a part of a file exported
alone, is the file's line of each line of TEXT, for the lines an error names.
A plist of :LATEX, the LaTeX; :OUTPUT, where #+EXPORT_FILE_NAME says to write
it, a full name ending in .tex, or NIL; :MAP, which place in TEXT each line of
the LaTeX comes from, a vector of (LINE BEGIN END) (ORG-LATEX-UNMARK), for what
LaTeX says of a line to be told at the Org text's; :MISSING, the characters no
font here has, each (CHARACTER . LINE) (enhanced only); :NOTES, what else to
say of it, a list of sentences (a picture not shown); :TRUSTED, the texts of
its template and setup files that are your own (EXPORT-TRUSTED-PATH-P), which
alone may name pictures from other directories; :PICTURES, the files its own
pictures are drawn from (ORG-LATEX-IMAGE), the only ones of the note's
directory a LaTeX program is shown when that directory may not be shown
whole, and, when the template is your own, the files it asks the note for by
its {{{NAME:path}}} places (a logo, TEMPLATE-PICTURE-PATH); :LANGUAGE, the
code of the language the note's #+LANGUAGE sets \(sv), or NIL, for the
hyphenation patterns it needs to be looked for \(LATEX-LANGUAGE-PATTERNS-NOTE)."
  (with-org-export-reading (expanded tree directory) (text file :enhanced enhanced :lines lines :latex t)
    (org-latex-written tree directory text file engine enhanced)))

(defun org-latex-written (tree directory text file engine enhanced)
  "The parts of the note being written (*ORG-EXPORT*) as ORG-LATEX-PARTS gives
them, TREE its parse, DIRECTORY its directory, TEXT its text as written in
FILE, for the LaTeX program ENGINE, ENHANCED or not."
  (let ((compiler (org-export-keyword "LATEX_COMPILER")))
    (setf (org-latex-engine *org-export*)
          (if (and compiler (plusp (length compiler))) (string-trim " " compiler) engine)))
  (multiple-value-bind (template template-path) (org-latex-chosen-template enhanced directory)
    (let ((body (org-latex-body tree template text file))
          (own-template (or (null template-path) (export-trusted-path-p template-path)))
          (missing '()))
      (when enhanced
        (multiple-value-setq (body missing) (org-latex-font-plan body (search "{{{fonts}}}" template))))
      (setf (org-export-refused *org-export*) (template-refused-pictures template))
      (multiple-value-bind (latex map)
          (let ((*org-latex-template* template))
            (org-latex-unmark (fill-latex-template template (org-latex-field body))))
        (list :latex latex
              :output (org-export-file-name text file)
              :map map
              :missing (org-latex-missing-lines missing latex)
              :notes (reverse (org-export-notes *org-export*))
              :trusted (if own-template
                           (cons template (org-export-trusted *org-export*))
                           (org-export-trusted *org-export*))
              :pictures (append (reverse (org-latex-pictures *org-export*))
                                (and own-template (reverse (org-latex-paths *org-export*))))
              :bibliographies (reverse (org-latex-bibliography-files *org-export*))
              :language (let ((row (org-latex-language-row)))
                          (and row (not (latex-loads-language-p template)) (first row))))))))

(defun org-latex-chosen-template (enhanced directory)
  "The text of the template the note being written is laid out by, and as a
second value its path when the note named one: the one its #+TEMPLATE names
\(found from DIRECTORY) in the ENHANCED export; else the built-in org template
for the plain export; else the enhanced article look for a note that sets no
look of its own in one of LaTeX's own kinds of document (article, report,
book) or none; else the org template."
  (let ((named (and enhanced (org-template-name))))
    (cond (named (latex-template-for named directory))
          ((not enhanced) (find-latex-template "org" :built-in t))
          ((and (notany #'org-export-keyword '("LATEX_HEADER" "LATEX_HEADER_EXTRA"))
                (member (string-trim " " (or (org-export-keyword "LATEX_CLASS") *org-latex-default-class*))
                        '("article" "report" "book") :test #'string-equal))
           (find-latex-template "article"))
          (t (find-latex-template "org")))))

(defun org-latex-body (tree template text file)
  "The LaTeX of TREE, the note TEXT from FILE parsed, as laid out by TEMPLATE:
its tables kept whole when the template sets the text in columns, and its
source blocks coloured by a program of the export's own, as a file's text is."
  (let ((*org-latex-in-columns* (latex-template-body-in-columns-p template))
        (*org-latex-text-characters* (or (latex-template-text-characters template) *org-latex-text-characters*)))
    (tuatara.syntax:with-own-helper ()
      ;; what goes wrong counted against the note, however often exported
      (let ((tuatara.syntax:*parsing-buffer* (tuatara.syntax:text-key :file file :text text)))
        (with-output-to-string (s) (org-latex-element tree s))))))

(defun org-latex-missing-lines (missing latex)
  "Each character of MISSING, those no font here has, with the line of LATEX
it is first on in the document's body, (CHARACTER . LINE)."
  (let ((start (or (search "\\begin{document}" latex) 0)))
    (loop for c in missing
          for at = (position c latex :start start)
          when at collect (cons c (1+ (count #\Newline latex :end at))))))

(defun org-latex-header-fonts ()
  "The fonts the document's #+LATEX_HEADER lines set with fontspec, (PART .
FAMILY) for \\setmainfont, \\setsansfont and \\setmonofont."
  (let ((found '()))
    (dolist (line (append (org-export-keyword-all "LATEX_HEADER") (org-export-keyword-all "LATEX_HEADER_EXTRA"))
             (nreverse found))
      (cl-ppcre:do-register-groups (part family) ("\\\\set(main|sans|mono)font(?:\\[[^]]*\\])?\\{([^}]*)\\}" line)
                                   (push (cons (intern (string-upcase part) :keyword) (string-trim " " family))
                                         found)))))

(defun org-latex-font-plan (body chosen)
  "Plan the fonts of the enhanced export's document, BODY its text as LaTeX,
CHOSEN true when its template sets fonts found here ({{{fonts}}}): the fonts set
\(CHOSEN-FONT, else those its #+LATEX_HEADER lines name, else LaTeX's own,
taken to hold *LATEX-ASSUMED-CHARACTERS*), and for each character of BODY
they lack, a font installed that has it, under a command of its own. Returns
BODY with each such character set in its font, and as a second value the
characters no font here has. Only for xelatex and lualatex, which load the
system's fonts."
  (let ((engine (org-latex-engine *org-export*)))
    (unless (member engine '("xelatex" "lualatex") :test #'string-equal)
      (return-from org-latex-font-plan (values body '())))
    (when chosen
      (setf (org-latex-fonts *org-export*)
            (loop for part in '(:main :sans :mono)
                  for family in (chosen-fonts)
                  when family collect (cons part family))))
    (let* ((families (remove-duplicates (mapcar #'cdr (or (org-latex-fonts *org-export*) (org-latex-header-fonts)))
                                        :test #'string-equal))
           ;; a font fontconfig does not know (one of LaTeX's own) taken to hold Latin
           (ranges (mapcar (lambda (family) (or (font-characters family) *latex-assumed-characters*))
                           (or families (list nil))))
           (lacking (let ((seen (make-hash-table)))
                      (loop for c across body
                            for code = (char-code c)
                            ;; the marks of the LaTeX's lines are no characters of the text
                            when (and (> code 127) (not (<= #xD800 code #xDFFF))
                                      ;; each character is tested once, and listed the first time only
                                      (not (nth-value 1 (gethash c seen)))
                                      (setf (gethash c seen)
                                            (notevery (lambda (r) (and r (characters-have-p r code))) ranges)))
                            collect c)))
           (commands (make-hash-table)) (missing '()) (fonts '())
           (installed (and lacking (fallback-fonts))))
      (dolist (c lacking)
        (let ((family (font-for-character (char-code c) installed)))
          (if family
              (setf (gethash c commands)
                    (or (cdr (assoc family fonts :test #'string-equal))
                        (let ((name (format nil "tuataraFallback~:@(~r~)" (1+ (length fonts)))))
                          ;; letters only, as a command's name must be
                          (setf name (remove-if-not #'alpha-char-p name))
                          (push (cons family name) fonts)
                          name)))
              (push c missing))))
      (setf (org-latex-fallbacks *org-export*) (reverse fonts))
      (values (if (plusp (hash-table-count commands))
                  (let ((files (cl-ppcre:all-matches "\\\\includegraphics(?:\\[[^\\]]*\\])?\\{[^}]*\\}" body)))
                    ;; a picture's file name is no text: it stays as it is
                    (with-output-to-string (out)
                      (loop for c across body
                            for i from 0
                            for command = (gethash c commands)
                            do (loop while (and files (>= i (second files))) do (setf files (cddr files)))
                               (if (and command (not (and files (>= i (first files)))))
                                   (format out "{\\~a{}~c}" command c)
                                   (write-char c out)))))
                  body)
              (nreverse missing)))))
