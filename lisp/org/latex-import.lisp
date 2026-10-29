;;;; lisp/org/latex-import.lisp --- A LaTeX document given made a template and a note template to write in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; LATEX-IMPORT reads a LaTeX document someone gave you to write a report
;;;; in and makes of it what M-x make-template-from-latex promises:
;;;;
;;;; - A LaTeX template, latex-templates/NAME.tex: the document as it is up
;;;;   to its first \section (or \chapter, \part), its preamble, cover and
;;;;   facts, with each field a place: \title, \author and \date; a value
;;;;   defined in the preamble (\newcommand{\refnumber}{REF-0001}, \def);
;;;;   in the front, placeholder text ([Enter name], <Version>, «Date»), a
;;;;   tabular row of labels and values (REFERENCE: & REF-XXXX) and a line
;;;;   labelled "Version: 0.1". The text from the first section on becomes
;;;;   {{{body}}}; the packages an Org note's text needs are loaded before
;;;;   \begin{document} unless the document loads them itself.
;;;; - Its note template, note-templates/NAME.org, as template-import.lisp
;;;;   writes one: the fields, then the document's text as Org: its
;;;;   sections headings (a starred one unnumbered, those after \appendix
;;;;   tagged :appendix:), its lists, tables with their captions, pictures
;;;;   (saved in note-templates/NAME-media/), footnotes, bold, italic, links
;;;;   and formulas.
;;;;
;;;; A place written in it already, {{{case}}}, is a field as it is. A
;;;; document that has {{{body}}} already is a Tuatara template: it is taken
;;;; as it is, and its places are the note template's fields.
;;;;
;;;; The document is only read: nothing in it is run, \input files are not
;;;; read, and a picture is taken only from its folder or the folders in it.
;;;; Nothing is written beside it.

(in-package #:tuatara.defaults)

(defparameter *latex-import-picture-limit* (* 64 1024 1024)
  "The most octets a picture a LaTeX document shows may have to be saved with
the note template made of it; a larger one is left a mention.")

(defvar *latex-org-source* ""
  "The LaTeX document being read as Org, as written.")

(defvar *latex-org-text* ""
  "The LaTeX document being read as Org, its comments made spaces, so that a
place in it is the same place in *LATEX-ORG-SOURCE*.")

(defparameter *latex-org-depth-limit* 200
  "How deep groups and environments within each other are read as Org; deeper
ones are kept as the document writes them.")

(defvar *latex-org-depth* 0
  "How deep within groups and environments the reading is.")

(defvar *latex-org-indent* ""
  "What each line written begins with: a list item's indentation.")

(defvar *latex-org-levels* '()
  "The sectioning commands of the document read, from the outermost, as heading levels.")

(defvar *latex-org-appendix* nil
  "True after \\appendix: the outermost headings are tagged :appendix:.")

(defvar *latex-org-drawers* nil
  "True when a heading was given an :EXPORT: drawer.")

(defvar *latex-org-footnotes* '()
  "The footnotes met, newest first, as their Org text.")

(defvar *latex-org-pictures* '()
  "The pictures met, each (SOURCE . FILE): SOURCE the picture's full name, FILE as the note links it.")

(defvar *latex-org-directory* nil
  "The folder of the LaTeX document being read, its pictures' only source.")

(defvar *latex-org-media* nil
  "The folder pictures are saved to, as links write it, or NIL for none saved.")

;;; Reading LaTeX

(defun latex-blank-comments (text)
  "TEXT with each comment (from an unescaped % to its line's end) made
spaces, its length and every other character as they were."
  (let ((out (copy-seq text)) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char out i)))
               (cond ((char= c #\\) (incf i 2))
                     ((char= c #\%)
                      (loop while (and (< i n) (char/= (char out i) #\Newline))
                            do (setf (char out i) #\Space) (incf i)))
                     (t (incf i)))))
    out))

(defun latex-group-end (text start)
  "Where the group TEXT opens at START ({ or [) ends: the position after its
closing } or ], braces within counted, an escaped one not; NIL when it never ends."
  (let ((open (char text start)) (depth 0) (i start) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c #\\) (incf i))
                     ((char= c #\{) (incf depth))
                     ((char= c #\}) (decf depth)
                      (when (and (char= open #\{) (zerop depth)) (return-from latex-group-end (1+ i))))
                     ((and (char= open #\[) (char= c #\]) (= depth 0)) (return-from latex-group-end (1+ i)))))
             (incf i))
    nil))

(defun latex-skip-space (text i &optional (end (length text)))
  "The first position from I in TEXT that is no space, tab or single line end."
  (let ((lines 0))
    (loop while (and (< i end) (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
          do (when (char= (char text i) #\Newline) (incf lines))
             (when (> lines 1) (return))
             (incf i))
    i))

(defun latex-command-at (text i)
  "The command TEXT has at I (a backslash): its name (letters, a * after
kept; else the one character) and where it ends."
  (let* ((n (length text)) (j (1+ i)))
    (cond ((>= j n) (values "" j))
          ((alpha-char-p (char text j))
           (loop while (and (< j n) (alpha-char-p (char text j))) do (incf j))
           (when (and (< j n) (char= (char text j) #\*)) (incf j))
           (values (subseq text (1+ i) j) j))
          (t (values (string (char text j)) (1+ j))))))

(defun latex-arguments (text i count &optional (end (length text)))
  "The groups after I in TEXT: optional [..] ones and up to COUNT {..} ones,
each (KIND START . END) with START and END inside its brackets, and where
they end."
  (let ((found '()) (mandatory 0))
    (loop
      (let ((j (latex-skip-space text i end)))
        (when (>= j end) (return))
        (let ((c (char text j)))
          (cond ((and (char= c #\[) (zerop mandatory))
                 (let ((e (latex-group-end text j)))
                   (unless (and e (<= e end)) (return))
                   (push (list* :optional (1+ j) (1- e)) found)
                   (setf i e)))
                ((and (char= c #\{) (< mandatory count))
                 (let ((e (latex-group-end text j)))
                   (unless (and e (<= e end)) (return))
                   (push (list* :mandatory (1+ j) (1- e)) found)
                   (incf mandatory)
                   (setf i e)))
                (t (return))))))
    (values (nreverse found) i)))

(defun latex-mandatory (arguments &optional (n 0))
  "The Nth mandatory group of ARGUMENTS (LATEX-ARGUMENTS), (START . END), or NIL."
  (cdr (nth n (remove :optional arguments :key #'first))))

(defun latex-environment-end (text name start end)
  "Where \\end{NAME} closes the environment NAME whose content begins at START
in TEXT, as two values: where the \\end begins and where it ends; nested
ones of the same name counted. NIL when it is not closed before END."
  (let ((open (format nil "\\begin{~a}" name)) (close (format nil "\\end{~a}" name)) (depth 1) (i start))
    (loop
      (let ((o (search open text :start2 i :end2 end))
            (c (search close text :start2 i :end2 end)))
        (cond ((null c) (return nil))
              ((and o (< o c)) (incf depth) (setf i (+ o (length open))))
              (t (decf depth)
                 (when (zerop depth) (return (values c (+ c (length close)))))
                 (setf i (+ c (length close)))))))))

(defun latex-find-command (text names start end)
  "Where TEXT first has one of the commands NAMES (without backslash) from
START below END, as two values: its start and its name; NIL when it has none."
  (let ((i start))
    (loop
      (let ((b (position #\\ text :start i :end end)))
        (unless b (return nil))
        (multiple-value-bind (name after) (latex-command-at text b)
          (when (member name names :test #'string=)
            (return (values b name)))
          (setf i (max after (1+ b))))))))

;;; LaTeX as Org

(defparameter *latex-org-dropped*
  '("maketitle" "tableofcontents" "listoffigures" "listoftables" "clearpage" "cleardoublepage" "newpage" "pagebreak" "noindent"
    "centering" "raggedright" "raggedleft" "small" "footnotesize" "scriptsize" "tiny" "large" "Large" "LARGE" "huge" "Huge"
    "normalsize" "bfseries" "itshape" "ttfamily" "sffamily" "rmfamily" "normalfont" "hline" "toprule" "midrule" "bottomrule"
    "endhead" "endfirsthead" "endfoot" "endlastfoot" "medskip" "bigskip" "smallskip" "hfill" "vfill" "null" "relax" "protect"
    "selectlanguage" "pagestyle" "thispagestyle" "label" "vspace" "vspace*" "hspace" "hspace*" "cline" "cmidrule" "addlinespace"
    "setlength" "addtocounter" "setcounter" "color" "phantomsection" "addcontentsline" "index" "nopagebreak" "linebreak")
  "Commands whose words an Org note does not keep, with their groups.")

(defparameter *latex-org-markup*
  '(("textbf" "*" "*") ("textit" "/" "/") ("emph" "/" "/") ("textsl" "/" "/") ("underline" "_" "_") ("uline" "_" "_")
    ("texttt" "~" "~") ("sout" "+" "+") ("st" "+" "+"))
  "Commands an Org note writes as its emphasis: the name, what goes before, what goes after.")

(defparameter *latex-org-accents*
  '(("'" . #x301) ("`" . #x300) ("^" . #x302)
    ("\"" . #x308) ("~" . #x303) ("c" . #x327)
    ("=" . #x304) ("." . #x307) ("u" . #x306)
    ("v" . #x30C) ("H" . #x30B) ("r" . #x30A))
  "Accent commands and the codes of the combining characters they stand for.")

(defparameter *latex-org-raw-environments*
  '("equation" "equation*" "align" "align*" "gather" "gather*" "multline" "multline*" "eqnarray" "eqnarray*" "displaymath" "math"
    "tikzpicture")
  "Environments an Org note keeps as LaTeX, as the export writes them.")

(defparameter *latex-org-tables* '("tabular" "tabular*" "tabularx" "tabulary" "longtable" "xltabular" "tabu")
  "Environments that are tables.")

(defun latex-org-words (text)
  "Plain TEXT of a LaTeX document as an Org note's words (DOCX-READ-ESCAPED),
its lines after the first begun with *LATEX-ORG-INDENT*, its ligatures for
quotes and dashes the characters they make."
  (let* ((text (cl-ppcre:regex-replace-all "---" text "—"))
         (text (cl-ppcre:regex-replace-all "--" text "–"))
         (text (cl-ppcre:regex-replace-all "``" text "“"))
         (text (cl-ppcre:regex-replace-all "''" text "”"))
         (text (substitute #\Space #\~ text))
         (text (docx-read-guarded (docx-read-escaped text))))
    (cl-ppcre:regex-replace-all "\\n[ \\t]*" text
                                (lambda (match) (declare (ignore match)) (format nil "~%~a" *latex-org-indent*))
                                :simple-calls t)))

(defun latex-org-string (start end)
  "The text from START below END of the document being read, as Org, on one line."
  (string-trim " " (cl-ppcre:regex-replace-all "\\s*\\n\\s*"
                                               (let ((*latex-org-indent* "")) (with-output-to-string (s) (latex-org start end s)))
                                               " ")))

(defun latex-org-picture (name)
  "The Org link to the picture NAME a LaTeX document shows, saved with the
note template when it is in the document's folder or a folder in it (an
ending tried when it has none); else words saying what it was."
  (let* ((name (string-trim " " name))
         (safe (and *latex-org-directory* *latex-org-media*
                    (plusp (length name))
                    (not (member (char name 0) '(#\/ #\~ #\\)))
                    (not (search ".." name))
                    (not (find #\: name))))
         (found (and safe
                     (loop for ending in '("" ".png" ".jpg" ".jpeg" ".pdf" ".svg")
                           for path = (concatenate 'string *latex-org-directory* name ending)
                           when (and (eq :regular (tuatara.io:path-kind path))
                                     (org-path-beside-p path *latex-org-directory*)
                                     (<= (or (ignore-errors (tuatara.io:call-with-file path (lambda (fd p size) (declare (ignore fd p)) size)))
                                             (1+ *latex-import-picture-limit*))
                                         *latex-import-picture-limit*))
                             return path))))
    (if found
        (let ((file (import-picture-file found *latex-org-media* (substitute #\- #\/ (subseq found (length *latex-org-directory*)))
                                         *latex-org-pictures*)))
          (pushnew (cons found file) *latex-org-pictures* :test #'equal)
          (format nil "[[file:~a]]" file))
        (format nil "[picture ~a]" (org-safe-line name)))))

(defun latex-org-table (start end out)
  "Write the table whose rows are from START below END (a tabular's inside,
after its column spec) to OUT as an Org table, a line under its first row
when a rule follows it."
  (let* ((text *latex-org-text*)
         (rows '()) (rule-after-first nil) (row-start start) (cells '()) (cell-start start) (depth 0) (i start))
    (flet ((cell (from to) (push (latex-org-string from to) cells))
           (rule-follows (from) (cl-ppcre:scan "^\\s*(?:\\[[^\\]]*\\])?\\s*\\\\(?:hline|midrule|toprule|cmidrule)" text :start from :end end)))
      (loop while (< i end)
            do (let ((c (char text i)))
                 (cond ((and (char= c #\\) (< (1+ i) end) (char= (char text (1+ i)) #\\) (zerop depth))
                        (cell cell-start i)
                        (push (nreverse cells) rows)
                        (when (and (= (length rows) 1) (rule-follows (+ i 2))) (setf rule-after-first t))
                        (setf cells '() i (+ i 2))
                        ;; a row's height, \\[2pt]
                        (let ((j (latex-skip-space text i end)))
                          (when (and (< j end) (char= (char text j) #\[))
                            (let ((e (latex-group-end text j))) (when (and e (<= e end)) (setf i e)))))
                        (setf cell-start i row-start i))
                       ((char= c #\\) (incf i 2))
                       ((char= c #\{) (incf depth) (incf i))
                       ((char= c #\}) (decf depth) (incf i))
                       ((and (char= c #\&) (zerop depth)) (cell cell-start i) (setf cell-start (1+ i)) (incf i))
                       (t (incf i)))))
      (when (plusp (length (string-trim '(#\Space #\Newline #\Tab) (latex-org-string row-start end))))
        (cell cell-start end)
        (push (nreverse cells) rows))
      (let ((rows (remove-if (lambda (row) (every (lambda (c) (zerop (length c))) row)) (nreverse rows))))
        (format out "~%")
        (org-write-table-rows rows out :indent *latex-org-indent* :rule rule-after-first)))))

(defun latex-org-list (kind start end out)
  "Write the list KIND (itemize, enumerate, description) whose inside is from
START below END to OUT as an Org list, nested lists within indented."
  (let ((text *latex-org-text*) (items '()) (i start))
    ;; the \items of this list, not of lists within it
    (loop
      (multiple-value-bind (at name) (latex-find-command text '("item" "begin") i end)
        (unless at (return))
        (if (string= name "item")
            (progn (push at items) (setf i (+ at 5)))
            (multiple-value-bind (args after) (latex-arguments text (+ at 6) 1 end)
              (let ((inner (latex-mandatory args)))
                (setf i (or (and inner (nth-value 1 (latex-environment-end text (subseq text (car inner) (cdr inner)) after end)))
                            after)))))))
    (setf items (nreverse items))
    (loop for (at next) on items
          for number from 1
          do (multiple-value-bind (args after) (latex-arguments text (+ at 5) 0 (or next end))
               (let* ((term (find :optional args :key #'first))
                      (bullet (cond ((string= kind "enumerate") (format nil "~d. " number))
                                    ((and term (string= kind "description")) (format nil "- ~a :: " (latex-org-string (cadr term) (cddr term))))
                                    (term (format nil "- ~a " (latex-org-string (cadr term) (cddr term))))
                                    (t "- "))))
                 (format out "~%~a~a" *latex-org-indent* bullet)
                 (let ((*latex-org-indent* (concatenate 'string *latex-org-indent* (make-string (length bullet) :initial-element #\Space))))
                   (write-string (string-trim '(#\Space #\Newline) (with-output-to-string (s) (latex-org after (or next end) s))) out)))))
    (format out "~%")))

(defun latex-org-heading (name arguments out)
  "Write the sectioning command NAME (section, subsection*...) with its
ARGUMENTS to OUT as an Org heading: unnumbered when starred, tagged
:appendix: after \\appendix when outermost."
  (let* ((starred (uiop:string-suffix-p name "*"))
         (base (string-right-trim "*" name))
         (level (1+ (or (position base *latex-org-levels* :test #'string=) 0)))
         (title (let ((m (latex-mandatory arguments))) (if m (latex-org-string (car m) (cdr m)) ""))))
    (format out "~%~%~a ~a~:[~; :appendix:~]~%" (make-string level :initial-element #\*) title (and *latex-org-appendix* (= level 1)))
    (when starred
      (setf *latex-org-drawers* t)
      (format out ":EXPORT:~%numbered: no~%:END:~%"))))

(defun latex-org-flush (plain out)
  "Write the words PLAIN, a string output stream, holds to OUT as an Org
note's (LATEX-ORG-WORDS), leaving PLAIN empty."
  (let ((words (get-output-stream-string plain)))
    (when (plusp (length words))
      (write-string (latex-org-words words) out))))

(defun latex-org-environment (env start stop close close-end out)
  "Write the environment ENV of the LaTeX document being read to OUT as Org:
its \\begin at START, its content from STOP below CLOSE, its \\end ending at
CLOSE-END. Returns CLOSE-END."
  (let ((text *latex-org-text*))
    (cond
      ((member env *latex-org-raw-environments* :test #'string=)
       (format out "~%~a~%" (subseq *latex-org-source* start close-end)))
      ((member env '("verbatim" "Verbatim" "lstlisting" "minted" "alltt") :test #'string=)
       (let ((from (if (string= env "minted") (nth-value 1 (latex-arguments text stop 1 close)) stop)))
         (format out "~%#+begin_example~%~a~&#+end_example~%"
                 (cl-ppcre:regex-replace-all "(?m)^(\\*|#\\+|,)" (string-trim '(#\Newline) (subseq *latex-org-source* from close)) ",\\1"))))
      ((member env *latex-org-tables* :test #'string=)
       ;; past the width and the column spec
       (let ((from (nth-value 1 (latex-arguments text stop (if (member env '("tabular*" "tabularx" "tabulary" "xltabular") :test #'string=) 2 1) close))))
         (latex-org-table from close out)))
      ((member env '("itemize" "enumerate" "description") :test #'string=)
       (latex-org-list env stop close out))
      ((member env '("quote" "quotation") :test #'string=)
       (format out "~%#+begin_quote~%~a~%#+end_quote~%"
               (string-trim '(#\Space #\Newline) (let ((*latex-org-indent* "")) (with-output-to-string (s) (latex-org stop close s))))))
      ((string= env "abstract")
       (format out "~%~%* Abstract~%") (latex-org stop close out))
      (t (latex-org stop close out)))
    close-end))

(defun latex-org-character-command (name start after end out plain)
  "Write the command NAME of the LaTeX document being read when it stands for
characters, a break or a mark of its own (an escaped character, an accent,
a line break, par, verb, appendix), its backslash at START and its name
ending at AFTER, below END, to OUT as Org, its plain words to PLAIN for
LATEX-ORG-FLUSH. Returns the position after it; NIL for a command of
another kind."
  (let ((text *latex-org-text*))
    (flet ((flush () (latex-org-flush plain out))
           (raw (from to) (subseq *latex-org-source* from to)))
      (cond
        ((member name '("(" "[") :test #'string=)
         (let* ((close (search (if (string= name "(") "\\)" "\\]") text :start2 after :end2 end))
                (stop (if close (+ close 2) end)))
           (flush)
           (write-string (raw start stop) out)
           stop))
        ((string= name "\\")
         (flush)
         (format out "\\\\~%~a" *latex-org-indent*)
         (let ((j (latex-skip-space text after end)))
           (or (and (< j end) (char= (char text j) #\[)
                    (let ((e (latex-group-end text j))) (and e (<= e end) e)))
               after)))
        ((member name '("&" "%" "$" "#" "_" "{" "}") :test #'string=)
         (write-string name plain) after)
        ((member name '(" " "," ";" ":" "!" "/" "@") :test #'string=) (write-char #\Space plain) after)
        ((string= name "-") after)
        ((assoc name *latex-org-accents* :test #'string=)
         (multiple-value-bind (args stop) (latex-arguments text after 1 end)
           (let* ((m (latex-mandatory args))
                  (letter (cond (m (subseq text (car m) (cdr m)))
                                ((< after end) (prog1 (string (char text after)) (setf stop (1+ after))))
                                (t ""))))
             (write-string (sb-unicode:normalize-string
                            (concatenate 'string letter (string (code-char (cdr (assoc name *latex-org-accents* :test #'string=))))) :nfc)
                           plain)
             stop)))
        ((string= name "par") (flush) (format out "~%~%") after)
        ((string= name "newline") (flush) (format out "\\\\~%~a" *latex-org-indent*) after)
        ((member name '("ldots" "dots") :test #'string=) (write-string "…" plain) after)
        ((string= name "today") (write-string "%date%" plain) after)
        ((member name '("LaTeX" "TeX") :test #'string=) (write-string name plain) after)
        ((string= name "appendix") (setf *latex-org-appendix* t) after)
        ((string= name "verb")
         (let* ((delimiter (and (< after end) (char text after)))
                (close (and delimiter (position delimiter text :start (1+ after) :end end))))
           (flush)
           (cond (close (format out "=~a=" (raw (1+ after) close)) (1+ close))
                 (t end))))))))

(defun latex-org-command (name start after end out plain)
  "Write the command NAME of the LaTeX document being read, its backslash at
START and its name ending at AFTER, with its groups below END, to OUT as Org,
its plain words to PLAIN for LATEX-ORG-FLUSH. Returns the position after it."
  (let ((text *latex-org-text*))
    (flet ((flush () (latex-org-flush plain out))
           (raw (from to) (subseq *latex-org-source* from to))
           (group (count index function)
             ;; its COUNT groups read, FUNCTION called with the INDEXth mandatory one, (START . END), when it has one
             (multiple-value-bind (args stop) (latex-arguments text after count end)
               (let ((m (latex-mandatory args index)))
                 (latex-org-flush plain out)
                 (when m (funcall function m))
                 stop))))
      (or
       (latex-org-character-command name start after end out plain)
       (cond
         ((member (string-right-trim "*" name) '("part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph")
                  :test #'string=)
          (multiple-value-bind (args stop) (latex-arguments text after 1 end)
            (flush) (latex-org-heading name args out) stop))
         ((assoc name *latex-org-markup* :test #'string=)
          (let ((marks (assoc name *latex-org-markup* :test #'string=)))
            (group 1 0 (lambda (m)
                         (let ((inner (latex-org-string (car m) (cdr m))))
                           (when (plusp (length inner))
                             (format out "~a~a~a" (second marks) inner (third marks))))))))
         ((string= name "url")
          (group 1 0 (lambda (m) (let ((target (raw (car m) (cdr m)))) (write-string (office-link-text target target) out)))))
         ((string= name "href")
          (multiple-value-bind (args stop) (latex-arguments text after 2 end)
            (let ((target (latex-mandatory args 0)) (words (latex-mandatory args 1)))
              (flush)
              (when (and target words)
                (write-string (office-link-text (raw (car target) (cdr target)) (latex-org-string (car words) (cdr words))) out))
              stop)))
         ((string= name "footnote")
          (group 1 0 (lambda (m)
                       (push (latex-org-string (car m) (cdr m)) *latex-org-footnotes*)
                       (format out "[fn:~d]" (length *latex-org-footnotes*)))))
         ((string= name "includegraphics")
          (group 1 0 (lambda (m) (write-string (latex-org-picture (raw (car m) (cdr m))) out))))
         ((string= name "caption")
          (group 1 0 (lambda (m) (format out "~%~a#+caption: ~a~%~a" *latex-org-indent* (latex-org-string (car m) (cdr m)) *latex-org-indent*))))
         ((member name '("textcolor" "colorbox") :test #'string=)
          (group 2 1 (lambda (m) (latex-org (car m) (cdr m) out))))
         ((string= name "begin")
          (multiple-value-bind (args stop) (latex-arguments text after 1 end)
            (let* ((m (latex-mandatory args))
                   (env (and m (subseq text (car m) (cdr m)))))
              (flush)
              (multiple-value-bind (close close-end) (and env (latex-environment-end text env stop end))
                (if close
                    (latex-org-environment env start stop close close-end out)
                    stop)))))
         ((member name *latex-org-dropped* :test #'string=)
          (nth-value 1 (latex-arguments text after 2 end)))
         (t
          ;; another command: its one group's words kept, several groups left out
          (multiple-value-bind (args stop) (latex-arguments text after 3 end)
            (let ((mandatory (remove :optional args :key #'first)))
              (flush)
              (when (= (length mandatory) 1)
                (latex-org (cadr (first mandatory)) (cddr (first mandatory)) out))
              stop))))))))

(defun latex-org (start end out)
  "Write the LaTeX document being read from START below END to OUT as Org;
past *LATEX-ORG-DEPTH-LIMIT* groups deep, as it is written."
  (when (>= *latex-org-depth* *latex-org-depth-limit*)
    (write-string (latex-org-words (subseq *latex-org-source* start end)) out)
    (return-from latex-org))
  (let ((*latex-org-depth* (1+ *latex-org-depth*))
        (text *latex-org-text*) (i start) (plain (make-string-output-stream)))
    (loop while (< i end)
          do (let ((c (char text i)))
               (cond
                 ;; formulas as written: $...$, $$...$$; \(...\) and \[...\] are commands
                 ((char= c #\$)
                  (let* ((double (and (< (1+ i) end) (char= (char text (1+ i)) #\$)))
                         (close (search (if double "$$" "$") text :start2 (+ i (if double 2 1)) :end2 end)))
                    (latex-org-flush plain out)
                    (if close
                        (let ((stop (+ close (if double 2 1))))
                          (write-string *latex-org-source* out :start i :end stop) (setf i stop))
                        (progn (write-string "\\dollar{}" out) (incf i)))))
                 ((char= c #\{)
                  (let ((e (or (latex-group-end text i) end)))
                    (latex-org-flush plain out)
                    (latex-org (1+ i) (min (max (1+ i) (1- e)) end) out)
                    (setf i (min e end))))
                 ((char= c #\}) (incf i))
                 ((char= c #\&) (write-char #\Space plain) (incf i))
                 ((char= c #\\)
                  (multiple-value-bind (name after) (latex-command-at text i)
                    (setf i (latex-org-command name i after end out plain))))
                 (t (write-char c plain) (incf i)))))
    (latex-org-flush plain out)))

(defun latex-org-note-text (start end)
  "The text of the LaTeX document being read from START below END as an Org
note's: its blank lines at most one, its footnotes at the end."
  (let* ((*latex-org-footnotes* '())
         (text (with-output-to-string (s) (latex-org start end s)))
         (text (cl-ppcre:regex-replace-all "(?m)[ \\t]+$" text ""))
         (text (cl-ppcre:regex-replace-all "\\n{3,}" text (format nil "~%~%")))
         (text (string-trim '(#\Newline #\Space) text)))
    (if *latex-org-footnotes*
        (format nil "~a~2%~{~a~%~}" text (loop for note in (reverse *latex-org-footnotes*) for n from 1 collect (format nil "[fn:~d] ~a" n note)))
        text)))

;;; The fields

(defun latex-import-plain (start end)
  "The words of the document being read from START below END, as a field's
example: on one line, nothing escaped."
  (let ((*latex-org-pictures* '()) (*latex-org-media* nil))
    (string-trim " " (cl-ppcre:regex-replace-all (format nil "\\s+|~a" *docx-read-zero-width-space*)
                                                 (with-output-to-string (s) (let ((*latex-org-indent* "")) (latex-org start end s)))
                                                 " "))))

(defun latex-import-camel-words (name)
  "NAME, a command's name, in words: refNumber is ref Number."
  (cl-ppcre:regex-replace-all "([a-z])([A-Z])" name "\\1 \\2"))

(defun latex-import-overlaps-p (edits start end)
  "True when the text from START below END overlaps one of EDITS, each (START END REPLACEMENT)."
  (some (lambda (e) (and (< start (second e)) (< (first e) end))) edits))

(defun latex-import-edit (edits start end replacement)
  "EDITS with REPLACEMENT for START below END added, unless it overlaps one there."
  (if (latex-import-overlaps-p edits start end)
      edits
      (cons (list start end replacement) edits)))

(defun latex-import-value-edit (edits start end label)
  "EDITS with the value from START below END, labelled LABEL, made the place
of a field named for LABEL (a date when LABEL says so)."
  (if (latex-import-overlaps-p edits start end)
      edits
      (let ((key (template-field-key label)))
        (template-field key (if (template-date-label-p label) :date :text) :example (latex-import-plain start end) :label label)
        (latex-import-edit edits start end (format nil "{{{~a}}}" key)))))

(defun latex-import-preamble-fields (text end edits)
  "EDITS with the values the preamble of TEXT (below END) defines without
arguments, \\newcommand{\\refnumber}{REF-0001} or \\def\\owner{...}, made
places."
  (let ((i 0))
    (loop
      (multiple-value-bind (at name) (latex-find-command text '("newcommand" "renewcommand" "providecommand" "def") i end)
        (unless at (return edits))
        (setf i (1+ at))
        (let* ((p (latex-skip-space text (+ at 1 (length name)) end))
               (macro (cond ((>= p end) nil)
                            ((char= (char text p) #\{)
                             (let ((e (latex-group-end text p)))
                               (and e (cl-ppcre:scan "^\\{\\s*\\\\[A-Za-z]+\\s*\\}$" (subseq text p e))
                                    (cons (string-trim "{} \\" (subseq text p e)) e))))
                            ((char= (char text p) #\\)
                             (multiple-value-bind (n after) (latex-command-at text p)
                               (and (alpha-char-p (char n 0)) (cons n after)))))))
          (when macro
            (let ((v (latex-skip-space text (cdr macro) end)))
              ;; a value with no arguments, of words alone
              (when (and (< v end) (char= (char text v) #\{))
                (let ((e (latex-group-end text v)))
                  (when (and e (<= e end))
                    (let ((value (subseq text (1+ v) (1- e))))
                      (when (and (plusp (length (string-trim " " value)))
                                 (not (find #\# value))
                                 (cl-ppcre:scan "^(?:\\\\[&%$_{} ]|[^\\\\{}])*$" value))
                        (let ((key (template-field-key (latex-import-camel-words (car macro)))))
                          (template-field key (if (template-date-label-p (car macro)) :date :text)
                                          :example (latex-import-plain (1+ v) (1- e)) :label (latex-import-camel-words (car macro)))
                          (setf edits (latex-import-edit edits (1+ v) (1- e) (format nil "{{{~a}}}" key))))))))))))))))

(defun latex-import-title-fields (text end edits)
  "EDITS with what \\title, \\author and \\date of TEXT (below END) hold made
{{{title}}}, {{{author}}} and {{{date}}}; the title's words as a second value."
  (let ((title nil))
    (dolist (name '("title" "author" "date"))
      (let ((at (latex-find-command text (list name) 0 end)))
        (when at
          (let ((m (latex-mandatory (latex-arguments text (+ at 1 (length name)) 1 end))))
            (when m
              (let ((example (latex-import-plain (car m) (cdr m))))
                (cond ((string= name "title") (setf title example))
                      ((string= name "date")
                       (template-field "date" :date :example (unless (string= example "%date%") example) :label "Date"))
                      (t (template-field name :text :example example :label "Author")))
                (setf edits (latex-import-edit edits (car m) (cdr m) (format nil "{{{~a}}}" name)))))))))
    (values edits title)))

(defun latex-import-front-fields (text start end edits)
  "EDITS with the fields of the front of TEXT, from START below END, made
places: tabular rows of labels and values, lines labelled \"Version: 0.1\",
then placeholder text ([Enter name], «Name», <Version>)."
  ;; tables of labels and values: a cell ending in : and the one after it
  (let ((i start))
    (loop
      (let ((at (search "\\begin{tabular" text :start2 i :end2 end)))
        (unless at (return))
        (multiple-value-bind (args stop) (latex-arguments text (+ at 6) 1 end)
          (let* ((m (latex-mandatory args))
                 (env (and m (subseq text (car m) (cdr m))))
                 (from (nth-value 1 (latex-arguments text stop (if (equal env "tabular") 1 2) end)))
                 (close (and env (latex-environment-end text env stop end))))
            (unless close (return))
            (let ((cells '()) (cell-start from) (depth 0) (j from))
              (flet ((cell (to) (push (cons cell-start to) cells)))
                (loop while (< j close)
                      do (let ((c (char text j)))
                           (cond ((and (char= c #\\) (< (1+ j) close) (char= (char text (1+ j)) #\\) (zerop depth))
                                  (cell j) (push :row cells) (setf j (+ j 2) cell-start j))
                                 ((char= c #\\) (incf j 2))
                                 ((char= c #\{) (incf depth) (incf j))
                                 ((char= c #\}) (decf depth) (incf j))
                                 ((and (char= c #\&) (zerop depth)) (cell j) (setf cell-start (1+ j)) (incf j))
                                 (t (incf j)))))
                (cell close))
              (loop for (label value) on (nreverse cells)
                    when (and (consp label) (consp value))
                      do (let ((words (latex-import-plain (car label) (cdr label))))
                           (when (and (cl-ppcre:scan "^[A-Za-z][A-Za-z0-9 /'.()-]{0,40}:$" words)
                                      (not (search "{{{" (subseq text (car value) (cdr value))))
                                      (cl-ppcre:scan "^\\s*(?:\\\\text(?:bf|it|tt)\\{)?[^\\\\{}]*\\}?\\s*$" (subseq text (car value) (cdr value))))
                             (let* ((inner (cl-ppcre:scan-to-strings "\\S(?:.*\\S)?" (subseq text (car value) (cdr value))))
                                    (vs (if inner (search inner text :start2 (car value) :end2 (cdr value)) (car value)))
                                    (ve (if inner (+ vs (length inner)) (car value))))
                               (setf edits (latex-import-value-edit edits vs ve (string-right-trim ":" words))))))))
            (setf i (or (nth-value 1 (latex-environment-end text env stop end)) end))))))
    ;; lines labelled Label: value
    (cl-ppcre:do-scans (ms me rs re "(?m)^[ \\t]*(?:\\\\textbf\\{)?([A-Za-z][A-Za-z /'-]{0,30}):\\}?[ \\t]+([^\\\\{}%&\\n]*[^\\\\{}%&\\s])[ \\t]*(?:\\\\\\\\.*)?$" text nil :start start :end end)
      (setf edits (latex-import-value-edit edits (aref rs 1) (aref re 1) (subseq text (aref rs 0) (aref re 0)))))
    ;; placeholder text, never a command's optional group
    (cl-ppcre:do-scans (ms me rs re *template-placeholder* text nil :start start :end end)
      (when (or (char/= (char text ms) #\[)
                (and (> ms 0) (member (char text (1- ms)) '(#\Space #\Tab #\Newline)) (find #\Space (subseq text ms me))))
        (let ((key (template-field-key (subseq text (1+ ms) (1- me)))))
          (unless (latex-import-overlaps-p edits ms me)
            (template-field key :text :label (subseq text ms me))
            (setf edits (latex-import-edit edits ms me (format nil "{{{~a}}}" key)))))))
    edits))

(defparameter *latex-import-packages*
  "% What an Org note's text needs, each package loaded unless the document loads it already
\\makeatletter
\\let\\tuatara@usepackage\\usepackage
\\renewcommand{\\usepackage}[2][]{\\@ifpackageloaded{#2}{}{\\tuatara@usepackage[#1]{#2}}}
\\makeatother
{{{packages}}}{{{latex_header}}}{{{font_fallbacks}}}\\makeatletter
\\let\\usepackage\\tuatara@usepackage
\\makeatother
"
  "What a template made from a LaTeX document has before \\begin{document}.")

;;; The whole

(defparameter *latex-file-commands*
  "\\\\(?:includegraphics|includepdf|includesvg|input|include|InputIfFileExists|IfFileExists|lstinputlisting|verbatiminput|import|subimport|openin)(?![A-Za-z@])\\*?[ \\t]*(?:\\[[^\\]\\n]*\\][ \\t]*)*(?:(?:\\\\[A-Za-z@]+|[0-9]+)[ \\t]*=?[ \\t]*)?(?:(?<=import)\\{[^}\\n]*\\}[ \\t]*)?(?:\\{[ \\t\"]*(?:[~/]|[^}\\n]*?\\.\\.[/\\\\])[^}\\n]*\\}|\"?(?:[~/]|[^\\s{}%\"]*?\\.\\.[/\\\\])[^\\s%]*)"
  "A LaTeX command taking in a file named from the root, from a home folder
or from a folder above the document's, in braces or not:
\\includegraphics{~/photo.png}, \\input{/etc/name}, \\input /etc/name,
\\input{../../name}, \\openin\\stream=/etc/name, \\IfFileExists{/etc/name}.")

(defun latex-import-without-outside-files (template)
  "TEMPLATE, a LaTeX document being made a template, with each command taking
in a file named from the root, a home folder or a folder above it
\(*LATEX-FILE-COMMANDS*) made a comment of its own line,
and that noted: a document given to you could otherwise put files of yours
into every report written from it."
  (let ((count 0))
    (prog1 (cl-ppcre:regex-replace-all *latex-file-commands* template
                                       (lambda (match) (incf count) (format nil "%~a (left out on import: a file outside the template)~%" match))
                                       :simple-calls t)
      (when (plusp count)
        (template-import-note (format nil "~d command~:p taking in files outside it (~~/..., /... or ../...) made comments" count))))))

(defun latex-import-apply-edits (text edits)
  "TEXT with EDITS made, each (START END REPLACEMENT), none overlapping another:
REPLACEMENT in place of the text from START below END."
  (with-output-to-string (out)
    (let ((at 0))
      (dolist (edit (stable-sort (copy-list edits) (lambda (a b) (or (< (first a) (first b))
                                                                    (and (= (first a) (first b)) (< (second a) (second b)))))))
        (destructuring-bind (start end replacement) edit
          (write-string text out :start at :end start)
          (write-string replacement out)
          (setf at end)))
      (write-string text out :start at))))

(defun latex-import (path name &key (tex (concatenate 'string (latex-templates-directory) name ".tex"))
                                    (note (concatenate 'string (note-templates-directory) name ".org")))
  "Make of the LaTeX document at PATH the template NAME, TEX, and its note
template NOTE, as this file's header says, its pictures in the folder
NAME-media beside NOTE. Returns the note template's path, and as a second
value what to say of it, a list of sentences. Signals an error when it
cannot be read; writes over nothing that exists."
  (let* ((source (read-file-text path))
         (text (latex-blank-comments source))
         (media (template-import-media note name))
         (*template-import* (make-template-import))
         (*latex-org-source* source)
         (*latex-org-text* text)
         (*latex-org-directory* (tuatara.io:native (tuatara.io:parent-path path)))
         (*latex-org-media* (abbreviated-path media))
         (*latex-org-pictures* '())
         (*latex-org-appendix* nil)
         (*latex-org-drawers* nil)
         (begin (search "\\begin{document}" text))
         (finish (and begin (search "\\end{document}" text :start2 begin))))
    (template-import-check-free (list tex note media))
    (unless (and begin finish)
      (error "~a has no \\begin{document} and \\end{document}: no LaTeX document to fill in" (tuatara.io:native-file-name path)))
    (let ((*latex-org-levels* (let ((present (remove-if-not (lambda (n) (latex-find-command text (list n (format nil "~a*" n)) begin finish))
                                                            '("part" "chapter"))))
                                (append present '("section" "subsection" "subsubsection" "paragraph" "subparagraph"))))
          (template source) (title nil) (body ""))
      (if (search "{{{body}}}" source)
          ;; a Tuatara template already: its places the fields
          (template-places-in source)
          (let* ((body-start (or (latex-find-command text '("part" "part*" "chapter" "chapter*" "section" "section*") begin finish) finish))
                 (edits '()))
            ;; the fields tagged already, {{{case}}}, first, in the order written
            (template-places-in (subseq source 0 body-start))
            (multiple-value-setq (edits title) (latex-import-title-fields text body-start edits))
            (setf edits (latex-import-preamble-fields text begin edits))
            (setf edits (latex-import-front-fields text (+ begin (length "\\begin{document}")) body-start edits))
            (push (list body-start finish (format nil "{{{body}}}~%")) edits)
            (push (list begin begin *latex-import-packages*) edits)
            (setf body (latex-org-note-text body-start finish))
            (setf template (format nil "% Made the Tuatara template ~a from ~a by make-template-from-latex: each field a~%% place, {{{body}}} the note's text. Change it as you like.~%~a"
                                   name (tuatara.io:native-file-name path) (latex-import-apply-edits source edits)))))
      (setf template (latex-import-without-outside-files template))
      (when (search "\\input" text) (template-import-note "its \\input files not read (the template still names them)"))
      (dolist (picture *latex-org-pictures*)
        (template-import-write (tuatara.io:native (tuatara.io:expand-path (cdr picture)))
                               (tuatara.io:file-octets (car picture))))
      (template-import-write tex template)
      (template-import-write note (template-note-text name (or title name) (template-import-fields *template-import*) body
                                                      :drawers *latex-org-drawers*))
      (template-import-result note))))
