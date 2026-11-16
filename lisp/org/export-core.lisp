;;;; lisp/org/export-core.lisp --- What every export of an Org document shares.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Org document exported as a PDF (latex.lisp), a Word document
;;;; (docx.lisp) or a web page (html.lisp) is read the same way first. Its
;;;; keywords and options are read, with those of its setup files, which are
;;;; taken only when local and in a directory an export may read. Its macros
;;;; are expanded with what redact and protect hide left out, its drawers kept
;;;; or left out as its options say, and its link targets found. ORG-EXPORT
;;;; records the export in hand. Nothing in the document is run.

(in-package #:tuatara.defaults)

(defvar *org-macro-limit* (* 16 1024 1024)
  "The most characters a note may grow to as its macro calls are put in place
for an export, 16 M: calls that would make a note of a few lines gigabytes
long are past it. The calls past it are left as they are written, and that
is said.")

(define-record (org-export (:constructor make-org-export (text directory file)) (:copier nil))
  "What exporting one Org note knows, as a PDF, a Word document or a web page
alike: the note as parsed, its file, its keywords and options, and what the
export gathers on the way (labels, footnotes, notes to the person, settings)."
  (text nil :documentation "The note ORG-EXPORT exports, as parsed.")
  (directory nil :documentation "The directory of the file of ORG-EXPORT, or NIL for none.")
  (file nil :documentation "The name of the file of ORG-EXPORT, or NIL for none.")
  (keywords '()
            :documentation "The keywords of the note of ORG-EXPORT and of its setup files, each (KEY . VALUE), in
order.")
  (options (make-hash-table :test 'equalp) :documentation "The #+OPTIONS of the note of ORG-EXPORT, as a table.")
  (min-level 1 :documentation "The level of the highest headlines of the note of ORG-EXPORT.")
  (footnotes (make-hash-table :test 'equal)
             :documentation "The footnote definitions of the note of ORG-EXPORT, by label.")
  (labels (make-hash-table :test 'eq)
          :documentation "The label given each node of the note of ORG-EXPORT that is linked to.")
  (counter 0 :documentation "The counter ORG-EXPORT takes new labels from.")
  (kept nil :documentation "The headlines ORG-EXPORT exports when some carry a select tag, NIL for all.")
  (list-ends (make-hash-table :test 'eq)
             :documentation "For each headline whose children ORG-EXPORT writes as a list, the first and the last of
them written, as a cons.")
  (macro-counters (make-hash-table :test 'equal) :documentation "The counters of the n macro of ORG-EXPORT.")
  (tree nil :documentation "The parse tree of the note of ORG-EXPORT.")
  (expansions '()
              :documentation "How each round of macros expanded in ORG-EXPORT moved the text, the last first
\(ORG-EXPORT-ORIGINAL-POSITION).")
  (enhanced nil :documentation "True when ORG-EXPORT applies Tuatara's own improvements on what Org writes.")
  (notes '()
         :documentation "What is said of the note of ORG-EXPORT after it is made, such as a picture not shown, the
latest first.")
  (settings nil :documentation "What the :EXPORT: drawers set in ORG-EXPORT, by node, in the enhanced export.")
  (consumed (make-hash-table :test 'eq)
            :documentation "The nodes ORG-EXPORT has written already as part of another, such as a paragraph beside a
picture.")
  (trusted '()
           :documentation "The texts of the setup files ORG-EXPORT read from the person's own directories
\(EXPORT-TRUSTED-PATH-P).")
  (refused '()
           :documentation "The names of the pictures the template of ORG-EXPORT asks for that the note names from
where they may not come, left out.")
  (targets nil
           :documentation "What a link inside the note of ORG-EXPORT may lead to, made once (ORG-EXPORT-LINK-TARGETS).")
  (setups (make-hash-table :test 'equal)
          :documentation "The text of each setup file ORG-EXPORT read, by its path, NIL for one that could not be.")
  (macro-room *org-macro-limit*
              :documentation "How many more characters the macro calls of ORG-EXPORT may add, below 0 once they would
pass *ORG-MACRO-LIMIT*."))

(defvar *org-export* nil
  "The ORG-EXPORT of the note being exported: an ORG-LATEX when it is written
as LaTeX.")

(defun export-note (format &rest arguments)
  "Say, after the document being exported (*ORG-EXPORT*) is made, FORMAT with
ARGUMENTS, once however often it comes; NIL."
  (when *org-export*
    (pushnew (apply #'format nil format arguments) (org-export-notes *org-export*) :test #'string=))
  nil)

(defvar *org-export-text* ""
  "The text whose nodes are being exported: the document's, or a keyword's value.")

(defparameter *org-export-setup-depth* 5
  "How deep setup files may name further setup files.")

;;; Keywords and options

(define-setting *export-trusted-directories* '()
  "More directories that exporting a note may take files from.
A note's export (to PDF or to a Word document) takes the setup files its
#+SETUPFILE lines name and the pictures its [[file:...]] links show from the
note's own directory and the directories in it, and your note templates' directory
(NOTE-TEMPLATES-DIRECTORY). A file anywhere else is left out of the export,
and the export says so when it is done, so that a note given to you cannot
have its export carry your other files into the PDF or document it makes.
- a list of directories, ~ standing for your home directory: those too. A directory of
  setup files shared by notes kept in directories of their own goes here:
  '(\"~/notes/latex/\").
- nil: no others."
  :values (list-of directory))

(defun org-export-path (name &optional (directory (and *org-export* (org-export-directory *org-export*))))
  "NAME, a file a note names, as a native path: ~ standing for your home
directory, a relative name taken from DIRECTORY, the directory of the note being
exported when not given, your home directory for a note with no file."
  (tuatara.io:native (tuatara.io:expand-path name :directory (or directory (tuatara.io:home-directory)))))

(defun export-directory-p (path directory &optional directories)
  "True when the file at PATH (native) stands where an export may take files
from: DIRECTORY, the document's directory, or a directory in it, one of
DIRECTORIES, or one of *EXPORT-TRUSTED-DIRECTORIES*, links followed
\(TUATARA.IO:REALLY-WITHIN-P)."
  (or (and directory (tuatara.io:really-within-p path directory))
      (tuatara.io:path-in-directories-p path (append directories *export-trusted-directories*))))

(defun org-export-directory-p (path)
  "True when PATH (native) stands where the export of the document *ORG-EXPORT*
is writing may take files from: the document's directory or a directory in it,
your note templates' directory (NOTE-TEMPLATES-DIRECTORY, where the pictures of
a template made from a Word document are), or one of *EXPORT-TRUSTED-DIRECTORIES*,
links followed (EXPORT-DIRECTORY-P)."
  (export-directory-p path (and *org-export* (org-export-directory *org-export*)) (list (note-templates-directory))))

(defun org-export-file-allowed-p (path)
  "True when exporting the document *ORG-EXPORT* is writing may take in the
file at PATH (native), it standing where exports take files from
\(ORG-EXPORT-DIRECTORY-P). When it may not, the export says so (ORG-EXPORT-NOTES)
and NIL is returned; a file that does not exist may be named, there being
nothing to take in."
  (or (null (tuatara.io:path-stamp path :failure nil)) ; nothing there to take in
      (org-export-directory-p path)
      (export-note "~a: outside the note's directory, so left out of the export ~
(*export-trusted-directories* names the directories it may also come from)"
                   (abbreviated-path path))))

(define-setting *logos-directory* nil
  "The directory of the pictures your templates ask a note for.
This is where the logo a note's #+logo: names is found (LOGOS-DIRECTORY).
- a directory: that directory.
- nil, as standard: logos/ in the configuration directory."
  :values (or (member nil) directory))

(defun logos-directory ()
  "Where the pictures your templates ask a note for are kept (the logo its
#+logo: names): *LOGOS-DIRECTORY*, or logos/ in the configuration directory
\(~/.config/tuatara/logos/), ending in /."
  (let ((directory (if *logos-directory*
                       (tuatara.io:native (tuatara.io:expand-path *logos-directory*))
                       (config-path "logos/"))))
    (tuatara.io:slash-ended directory)))

(defun template-picture-path (value)
  "The file of the picture a template asks the note for with {{{NAME:path}}},
VALUE what the note's keyword says: a name alone (logo.png) in your logos directory
\(LOGOS-DIRECTORY), else a path, read as the note's links are. NIL, said after
the export, for one that is not there, or stands elsewhere than the logos directory
and the directories an export takes files from \(ORG-EXPORT-DIRECTORY-P), so that a
note given to you cannot have your template take in a picture of yours."
  (let* ((name (string-trim " " value))
         (path (if (or (find #\/ name) (uiop:string-prefix-p "~" name))
                   (org-export-path name)
                   (concatenate 'string (logos-directory) name))))
    (cond ((not (or (tuatara.io:path-in-directories-p path (list (logos-directory))) (org-export-directory-p path)))
           (export-note "~a: a picture a template asks for comes from ~a or the note's own directory, so left out"
                        (abbreviated-path path) (abbreviated-path (logos-directory))))
          ((not (tuatara.io:path-stamp path))
           (export-note "~a: no such picture, so left out" (abbreviated-path path)))
          (t path))))

(defun export-trusted-path-p (path)
  "True when the file at PATH (native) is your own to an export: in your
LaTeX templates' directory or one of *EXPORT-TRUSTED-DIRECTORIES*, links followed;
not merely beside the note, which may have come with it."
  (tuatara.io:path-in-directories-p path (cons (latex-templates-directory) *export-trusted-directories*)))

(defun org-export-read-setup (value directory)
  "The text of the setup file VALUE names, relative to DIRECTORY, with its
directory as a second value; NIL for one on the network, not readable, or
outside the directories an export may take files from (ORG-EXPORT-FILE-ALLOWED-P)."
  (let ((name (string-trim "\" " value)))
    (unless (or (zerop (length name)) (search "://" name))
      (let ((path (ignoring-errors-reported ("finding the setup file ~a" name) (org-export-path name directory))))
        (when (and path (eq :regular (tuatara.io:path-kind path))
                   (org-export-file-allowed-p path))
          (let ((text (org-export-setup-text path)))
            (and text (values text (tuatara.io:parent-path path) path))))))))

(defun org-export-setup-text (path)
  "The text of the setup file at PATH (native), read once for the document
being exported: NIL when it cannot be read, or when it is larger than
*ORG-FILE-READ-LIMIT* octets, which is said."
  (let ((setups (and *org-export* (org-export-setups *org-export*))))
    (multiple-value-bind (text known) (if setups (gethash path setups) (values nil nil))
      (if known
          text
          (let ((text (flet ((too-large (name size)
                               (declare (ignore name size))
                               (export-note "~a: a setup file larger than ~a, so not read"
                                            (abbreviated-path path) (octets-words *org-file-read-limit*))
                               nil))
                        (ignoring-errors-reported ("reading the setup file ~a" path)
                                                  (tuatara.io:file-text path :limit *org-file-read-limit*
                                                                        :too-large #'too-large)))))
            (when (and text *org-export* (export-trusted-path-p path))
              (push text (org-export-trusted *org-export*)))
            (when setups (setf (gethash path setups) text))
            text)))))

(defun org-export-collect-keywords (tree directory &optional (depth 0) (seen (make-hash-table :test 'equal)))
  "The keywords of TREE, a parsed document whose file is in DIRECTORY, each
(KEY . VALUE) in order, KEY in capitals; a #+SETUPFILE's in its place, the
first time it is named only (SEEN, a table of the setup files taken), so that
setup files naming each other are read once each."
  (let ((found '()))
    (org-walk-tree tree (lambda (node)
                          (when (eq (org-node-type node) 'keyword)
                            (let ((key (org-property node :key)) (value (or (org-property node :value) "")))
                              (if (string-equal key "SETUPFILE")
                                  (when (< depth *org-export-setup-depth*)
                                    (multiple-value-bind (text setup-directory path)
                                        (org-export-read-setup value directory)
                                      (when (and text (not (gethash path seen)))
                                        (setf (gethash path seen) t)
                                        (let ((theirs (org-export-collect-keywords (org-parse text) setup-directory
                                                                                   (1+ depth) seen)))
                                          (setf found (revappend theirs found))))))
                                  (push (cons (string-upcase key) value) found))))))
    (nreverse found)))

(defun org-export-keyword (key &optional (export *org-export*))
  "The value of the last #+KEY in the note EXPORT reads, or NIL."
  (cdr (find key (org-export-keywords export) :key #'car :test #'string= :from-end t)))

(defun org-export-keyword-all (key &optional (export *org-export*))
  "The values of every #+KEY in the note EXPORT reads, in order."
  (loop for (k . v) in (org-export-keywords export) when (string= k key) collect v))

(defun org-export-read-options (export)
  "Fill EXPORT's table of options from its note's #+OPTIONS lines, anew."
  (clrhash (org-export-options export))
  (dolist (line (org-export-keyword-all "OPTIONS" export))
    (cl-ppcre:do-register-groups (key value) ("(?:^|\\s)([^\\s:]+):(\\((?:\"[^\"]*\"|[^()\"])*\\)|\\S+)" line)
                                 (setf (gethash key (org-export-options export)) value))))

(defun org-export-option (key default)
  "The value of the option KEY (#+OPTIONS: KEY:VALUE): T, NIL, a number, or
the word as written; DEFAULT when not given."
  (multiple-value-bind (value found) (gethash key (org-export-options *org-export*))
    (if (not found)
        default
        (cond ((string-equal value "t") t)
              ((string-equal value "nil") nil)
              ((every #'digit-char-p value) (parse-integer value))
              (t value)))))

;;; Drawers, by #+OPTIONS: d:

(defun org-export-drawer-printed-p (node)
  "True when the export prints the drawer NODE: not an :EXPORT: drawer, which
holds settings; not the :LOGBOOK: unless named; the others as #+OPTIONS: d:
says: t for all, nil for none, a list such as (not \"NOTES\") for all but
those named, (\"NOTES\") for those only."
  (let* ((name (org-property node :drawer-name))
         (value (org-export-option "d" t))
         (listed (and (stringp value) (plusp (length value)) (char= (char value 0) #\()
                      (mapcar (lambda (word) (string-trim "\"" word))
                              (tuatara.text:split-words (string-trim "()" value))))))
    (cond ((string-equal name "EXPORT") nil)
          ((null value) nil)
          ((null listed) (not (string-equal name "LOGBOOK")))
          ((string-equal (first listed) "not") (not (member name (rest listed) :test #'string-equal)))
          (t (and (member name listed :test #'string-equal) t)))))

(defun org-export-attributes (node key)
  "NODE's #+ATTR_KEY: line as a plist of keywords and the text after each;
a setting no export uses (ORG-KNOWN-KEYWORD) is left out."
  (let ((line (getf (org-property node :affiliated) (org-known-keyword (format nil "ATTR_~a" key)))))
    (when line
      (loop for (setting . value) in (org-keyword-arguments line)
            for name = (org-known-keyword setting)
            when name
            append (list name value)))))

;;; Macros

(defvar *org-export-expanding* '()
  "The footnotes and macro calls being written, each (KIND . NAME): one met
again within itself (a footnote that refers to itself, macros that call each
other) is written as it is written, not again, never without end.")

(defmacro with-org-export-expanding ((kind name) (&body again) &body body)
  "Run BODY with KIND's NAME among *ORG-EXPORT-EXPANDING*, or AGAIN when it is
there already: being written within itself."
  (let ((key (gensym "KEY")))
    `(let ((,key (cons ,kind ,name)))
       (if (member ,key *org-export-expanding* :test #'equal)
           (progn ,@again)
           (let ((*org-export-expanding* (cons ,key *org-export-expanding*)))
             ,@body)))))

(defun org-export-macro-value (node)
  "What the macro call NODE stands for, as Org text, or NIL for a macro not
known, and NIL for every call once those put in place for the document
would have made it longer than *ORG-MACRO-LIMIT* characters. In the
enhanced export redact and protect are always Tuatara's own
\(ORG-EXPORT-REDACTION); elsewhere a call of them with no #+MACRO: line for it
is an error, never its text printed."
  (let ((room (if *org-export* (org-export-macro-room *org-export*) *org-macro-limit*)))
    (unless (minusp room)
      (let ((value (%org-export-macro-text node room))
            (growth (- (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))))
        (when (stringp value) (incf growth (length value)))
        (cond ((null value) nil)
              ((or (eq value :too-long) (> growth room))
               (when *org-export*
                 (setf (org-export-macro-room *org-export*) -1)
                 (export-note "Macro calls would make the note longer than ~:d characters, ~
so those past that are left as written"
                              *org-macro-limit*))
               nil)
              (t (when *org-export* (decf (org-export-macro-room *org-export*) growth))
                 value))))))

(defun %org-export-macro-text (node left)
  "What the macro call NODE stands for, as ORG-EXPORT-MACRO-VALUE gives it, or
:TOO-LONG when a macro of your own would put more than LEFT characters of
its arguments in."
  (let ((key (org-property node :key))
        (args (mapcar (lambda (a) (cl-ppcre:regex-replace-all "\\\\," a ",")) (org-property node :args))))
    (flet ((joined (key) (format nil "~{~a~^ ~}" (org-export-keyword-all key))))
      (cond ((and (org-hiding-kind key) (org-export-enhanced *org-export*))
             ;; the whole of what is inside, commas and all
             (org-export-redaction (org-hiding-kind key) (format nil "~{~a~^, ~}" args)))
            ((string= key "title") (joined "TITLE"))
            ((member key '("author" "date" "email") :test #'string=) (or (org-export-keyword (string-upcase key)) ""))
            ((string= key "keyword") (or (org-export-keyword (string-upcase (or (first args) ""))) ""))
            ((string= key "input-file")
             (if (org-export-file *org-export*) (tuatara.io:native-file-name (org-export-file *org-export*)) ""))
            ((member key '("time" "modification-time") :test #'string=)
             (format-time (or (first args) "%Y-%m-%d") (get-universal-time)))
            ((string= key "n")
             (let* ((name (or (first args) ""))
                    (counters (org-export-macro-counters *org-export*)))
               (cond ((equal (second args) "-") (princ-to-string (gethash name counters 0)))
                     ((and (second args) (every #'digit-char-p (second args)) (plusp (length (second args))))
                      (princ-to-string (setf (gethash name counters) (parse-integer (second args)))))
                     (t (princ-to-string (incf (gethash name counters 0)))))))
            (t
             (let ((definition (assoc key (org-macro-definitions) :test #'string-equal)))
               (when (and (null definition) (org-hiding-kind key))
                 (refuse "{{{~a(...)}}} hides text only in the enhanced export (M-x org-export-latex-pdf-enhanced), ~
                         or with a #+macro: ~a line of your own" key key))
               (when definition
                 (let ((body (cdr definition)))
                   ;; a value that is code is never run
                   (if (uiop:string-prefix-p "(eval" body)
                       ""
                       (let ((value (cl-ppcre:regex-replace-all "\\$([0-9])" body
                                                                (lambda (match n)
                                                                  (declare (ignore match))
                                                                  ;; $1 the first argument; $0 none
                                                                  (let* ((k (parse-integer n))
                                                                         (argument (or (and (plusp k) (nth (1- k) args))
                                                                                       "")))
                                                                    (if (minusp (decf left (length argument)))
                                                                        ""
                                                                        argument)))
                                                                :simple-calls t)))
                         (if (minusp left) :too-long value)))))))))))

(defun org-export-redacting-p (kind)
  "True when the text KIND marks, redact or protect, is hidden: redact unless
#+OPTIONS: redact:nil, protect when #+OPTIONS: protect:t."
  (and (org-export-option kind (string= kind "redact")) t))

(defun org-keyword-lines (text)
  "The lines of the Org TEXT that begin with #+, alone: enough to read its
keywords, the lines that begin and end its blocks still telling them from
what a block holds, the rest never parsed."
  (with-output-to-string (out)
    (cl-ppcre:do-matches-as-strings (line "(?m)^[ \\t]*#\\+.*$" text)
      (write-line line out))))

(defmacro with-org-note-keywords ((text directory &optional file) &body body)
  "Run BODY with the keywords of the Org note TEXT read as its export reads
them, those of its #+SETUPFILE: in their place (ORG-EXPORT-KEYWORD), DIRECTORY
the note's directory for setup files named relative to it and FILE its file.
Only its #+ lines are read, so that a long note is looked at in an instant."
  (let ((note (gensym)) (dir (gensym)))
    `(let* ((,note ,text) (,dir ,directory)
            (*org-export* (make-org-export ,note ,dir ,file)))
       (setf (org-export-keywords *org-export*)
             (org-export-collect-keywords (org-parse (org-keyword-lines ,note)) ,dir))
       ,@body)))

(defun org-hidden-kinds (text directory)
  "Which of redact and protect the Org note TEXT hides, as its export decides
(ORG-EXPORT-REDACTING-P): its #+OPTIONS: read with those of its #+SETUPFILE:,
DIRECTORY the note's directory for setup files named relative to it."
  (with-org-note-keywords (text directory)
    (org-export-read-options *org-export*)
    (remove-if-not #'org-export-redacting-p '("redact" "protect"))))

(defun org-macro-definitions (&optional (export *org-export*))
  "The macros the note EXPORT reads defines with #+MACRO: lines, its setup
files' too, as (NAME . BODY), each name once with its last definition, in
the order of their first."
  (let ((definitions '()))
    (dolist (line (org-export-keyword-all "MACRO" export))
      (let ((name (first (tuatara.text:split-words line))))
        (when name
          (let ((body (string-trim '(#\Space #\Tab) (subseq line (+ (search name line) (length name)))))
                (known (assoc name definitions :test #'string-equal)))
            (if known (setf (cdr known) body) (push (cons name body) definitions))))))
    (nreverse definitions)))

(defparameter *redacted-words* "[redacted]"
  "What stands for hidden text where only plain words can be: a PDF's
bookmarks and facts, a Word document's contents, links and properties.")

(defparameter *org-redaction-scanner*
  (cl-ppcre:create-scanner
   (concatenate 'string "(?:@@latex:\\\\texorpdfstring\\{\\\\redactedbox\\{n*\\}\\}\\{[^}]*\\}@@)?"
                "@@docx:redacted:(\\d+)@@"))
  "Hidden text as ORG-EXPORT-REDACTION writes it, how many characters it hid a
group; the Word document's part alone too.")

(defun org-export-redaction (kind text)
  "What {{{KIND(TEXT)}}} stands for, KIND redact or protect: TEXT as it is when
it is shown (ORG-EXPORT-REDACTING-P), else a black box as wide as TEXT would be.
For LaTeX it is made from as many letters n, [redacted] in its bookmarks, and
for a Word document as many characters (@@docx:redacted:N@@). This way TEXT is
in neither, nor in what is made from them. What is made so is read back by
*ORG-REDACTION-SCANNER*."
  (if (org-export-redacting-p kind)
      (format nil "@@latex:\\texorpdfstring{\\redactedbox{~a}}{~a}@@@@docx:redacted:~d@@"
              (make-string (max 1 (length text)) :initial-element #\n) *redacted-words* (max 1 (length text)))
      text))

(defun org-export-plain (string)
  "STRING, the value of a keyword, as plain text for the PDF's facts: the
text redacted or protected in it, when it is hidden, as [redacted]."
  (with-output-to-string (out)
    (let ((copied 0))
      (map-org-hiding-calls
       (lambda (start end name from to)
         (write-string string out :start copied :end start)
         (if (and (org-export-enhanced *org-export*) (org-export-redacting-p (org-hiding-kind name)))
             (write-string *redacted-words* out)
             (write-string string out :start from :end to))
         (setf copied end))
       string)
      (write-string string out :start copied))))

(defun org-export-expand-macros (text)
  "TEXT with the macro calls it holds replaced by what they stand for, again
while what they stand for holds more, at most eight times: a call Org's
syntax reads as one (never in a block), a macro not known left as it is,
and the calls that would make TEXT longer than *ORG-MACRO-LIMIT* characters
left as they are."
  (setf (org-export-macro-room *org-export*) (- *org-macro-limit* (length text)))
  (loop repeat 8
        until (minusp (org-export-macro-room *org-export*))
        do (let ((tree (org-parse text)) (calls '()))
             (setf (org-export-keywords *org-export*)
                   (org-export-collect-keywords tree (org-export-directory *org-export*)))
             ;; the options before the calls: they say what redact and protect hide
             (org-export-read-options *org-export*)
             (org-walk-tree tree (lambda (node)
                                   (when (eq (org-node-type node) 'macro)
                                     (let ((value (org-export-macro-value node)))
                                       (when value (push (list node value) calls))))))
             (when (null calls) (return text))
             ;; where each call's text lands after the round, for errors to be told at the text as written
             (let ((shift 0) (moves '()))
               (dolist (call (sort (copy-list calls) #'< :key (lambda (c) (org-node-begin (first c)))))
                 (destructuring-bind (node value) call
                   (let* ((begin (org-node-begin node))
                          (end (- (org-node-end node) (org-node-post-blank node)))
                          (after (+ begin shift)))
                     (incf shift (- (length value) (- end begin)))
                     (push (list after (+ after (length value)) begin shift) moves))))
               (push (coerce (nreverse moves) 'vector) (org-export-expansions *org-export*)))
             ;; the text written once, each call's value in its place
             (setf text (with-output-to-string (out)
                          (let ((at 0))
                            (dolist (call (sort calls #'< :key (lambda (c) (org-node-begin (first c)))))
                              (destructuring-bind (node value) call
                                (write-string text out :start at :end (org-node-begin node))
                                (write-string value out)
                                (setf at (- (org-node-end node) (org-node-post-blank node)))))
                            (write-string text out :start at)))))
        finally (return text)))

(defun org-export-original-position (position)
  "Where POSITION, in the text with its macros expanded, was in the text as
written: the start of the call when it is inside what a call stood for."
  (dolist (moves (org-export-expansions *org-export*) position)
    ;; the last move that begins at or before POSITION, by bisection
    (let ((low 0) (high (length moves)))
      (loop while (< low high)
            do (let ((middle (floor (+ low high) 2)))
                 (if (<= (first (aref moves middle)) position) (setf low (1+ middle)) (setf high middle))))
      (when (plusp low)
        (destructuring-bind (after after-end begin shift) (aref moves (1- low))
          (declare (ignore after))
          (setf position (if (< position after-end) begin (- position shift))))))))

(defun org-image-link-p (node accepts)
  "True when NODE is a link to a picture, with no description, shown as the
picture: a file whose type is one of ACCEPTS, a list of types (png, jpg), or
which ACCEPTS, a function of the file's name, takes."
  (and (eq (org-node-type node) 'link)
       (equal (org-property node :type) "file")
       (null (org-node-contents-begin node))
       (let ((path (org-property node :path)))
         (if (functionp accepts)
             (funcall accepts path)
             (member (tuatara.io:native-file-type path) accepts :test #'string-equal)))
       t))

(defun org-export-link-targets ()
  "What a link inside the document being written may lead to, a table made
once from its tree, the first of each in order: headlines by (:CUSTOM-ID .
ID), (:ID . ID) and (:TITLE . TITLE), targets by (:TARGET . WORDS), elements
named by (:NAME . NAME). What is left out of the export (ORG-EXPORT-SKIPPED-P)
is not among them: a link to it is written as its words."
  (or (org-export-targets *org-export*)
      (setf (org-export-targets *org-export*)
            (let ((table (make-hash-table :test 'equal)))
              (labels ((put (key value n)
                         (let ((key (cons key value))) (unless (gethash key table) (setf (gethash key table) n))))
                       (walk (n)
                         (let ((type (org-node-type n)))
                           (unless (and (eq type 'headline) (org-export-skipped-p n))
                             (cond ((eq type 'headline)
                                    (put :custom-id (org-headline-property n "CUSTOM_ID") n)
                                    (put :id (org-headline-property n "ID") n)
                                    (put :title (org-property n :raw-value) n))
                                   ((member type '(target radio-target)) (put :target (org-property n :value) n))
                                   ((org-property n :name) (put :name (org-property n :name) n)))
                             (when (eq type 'headline) (mapc #'walk (org-property n :title)))
                             (mapc #'walk (org-node-children n))))))
                (walk (org-export-tree *org-export*)))
              table))))

(defun org-export-link-target (node)
  "The node a link NODE inside the document leads to, or NIL (ORG-EXPORT-LINK-TARGETS)."
  (let ((type (org-property node :type)) (path (org-property node :path)) (targets (org-export-link-targets)))
    (flet ((found (key value) (gethash (cons key value) targets)))
      (cond ((string= type "id") (found :id path))
            ((member type '("custom-id" "fuzzy") :test #'string=)
             (loop for (kind . value) in (org-link-target-kinds (if (string= type "custom-id")
                                                                    (concatenate 'string "#" path) path))
                   thereis (found kind value)))))))

(defun org-refuse-hidden-as-written (name way values)
  "Refuse the template's place NAME read WAY when it prints VALUES, a keyword's,
as written (:RAW or :PATH) and one holds a name to hide: printed as written,
the call hides nothing."
  (when (and (member way '(:raw :path)) (some #'org-export-hidden-call-p values))
    (refuse "#+~(~a~): holds {{{redact(...)}}} or {{{protect(...)}}}, which the template prints as written, ~
            its text with it: not exported" name)))

(defun org-export-hidden-call-p (string)
  "True when STRING holds a call of redact or protect whose text the enhanced
export hides (ORG-EXPORT-REDACTING-P)."
  (map-org-hiding-calls (lambda (start end name from to)
                          (declare (ignore start end from to))
                          (when (org-export-redacting-p (org-hiding-kind name))
                            (return-from org-export-hidden-call-p t)))
                        string)
  nil)

(defun org-export-check-hidden (text original lines)
  "Refuse, with an error naming its line, a call of redact or protect left in
TEXT once its macros are expanded (ORIGINAL the text as written): one Org
reads no macros in (a code or example block, a link's address, raw LaTeX),
which would print its text, when that text is to be hidden. A keyword's line
is let be (its value is printed as Org text, where the call works), but for
#+LATEX_HEADER(_EXTRA) and #+LATEX:, printed as written. LINES, when given,
is the line of the file each line of the text comes from (a part exported
alone)."
  (let ((previous nil) (keyword nil))
    (map-org-hiding-calls
     (lambda (at end name from to)
       (declare (ignore end from to))
       ;; the line's keyword read once, however many calls the line holds
       (when (or (null previous) (find #\Newline text :start previous :end at))
         (setf keyword (nth-value 1 (cl-ppcre:scan-to-strings "^[ \\t]*#\\+([A-Za-z_]+):" text
                                                              :start (string-line-start text at)))))
       (setf previous at)
       (let ((kind (org-hiding-kind name)))
         (when (and (org-export-redacting-p kind)
                    (or (null keyword)
                        (member (aref keyword 0) '("LATEX_HEADER" "LATEX_HEADER_EXTRA" "LATEX") :test #'string-equal)))
           (let* ((position (org-export-original-position at))
                  (line (1+ (count #\Newline original :end (min position (length original)))))
                  (line (if lines (aref lines (min (1- (length lines)) (1- line))) line)))
             (refuse "{{{~a(...)}}} on line ~d is where Org reads no macros (a code or example block, a link's ~
                     address, raw LaTeX): its text would be printed, not hidden; not exported" kind line)))))
     text :every-opening t)))

(defun org-template-name ()
  "The template the note being written names, by name or path: its
#+TEMPLATE:, or #+LATEX_TEMPLATE: or #+LATEX_STYLE:, that keyword's older
names; NIL when none."
  (let ((value (or (org-export-keyword "TEMPLATE")
                   (org-export-keyword "LATEX_TEMPLATE")
                   (org-export-keyword "LATEX_STYLE"))))
    (and value (plusp (length (string-trim " " value))) (string-trim " " value))))

(defun org-export-take-in (text tree directory)
  "Have *ORG-EXPORT* know the note being written, TEXT (its macros expanded)
parsed as TREE, DIRECTORY its directory: its keywords and options, its lowest
level of headline, its footnotes' definitions by label, the parts it keeps
out, and, in the enhanced export, its :EXPORT: drawers' settings; for the
PDF and the Word export alike."
  (setf (org-export-text *org-export*) text
        (org-export-tree *org-export*) tree
        (org-export-targets *org-export*) nil
        (org-export-keywords *org-export*) (org-export-collect-keywords tree directory))
  (org-export-read-options *org-export*)
  (let ((levels '()))
    (org-walk-tree tree (lambda (n)
                          (case (org-node-type n)
                            (headline (push (org-property n :level) levels))
                            (footnote-definition (setf (gethash (org-property n :label)
                                                                (org-export-footnotes *org-export*)) n)))))
    (setf (org-export-min-level *org-export*) (if levels (reduce #'min levels) 1)))
  (setf (org-export-kept *org-export*) (org-export-find-kept tree))
  (when (org-export-enhanced *org-export*)
    (setf (org-export-settings *org-export*) (org-export-gather-settings tree))))

(defmacro with-org-export-reading ((text tree directory) (source file &key enhanced lines latex) &body body)
  "Run BODY with SOURCE, the text of an Org note from FILE (or NIL), read as
every export reads it: *ORG-EXPORT* made for it (an ORG-LATEX when LATEX is
true, to be written as LaTeX), ENHANCED or not, its macros expanded, what
redact and protect hide checked for (enhanced only; LINES as
ORG-EXPORT-CHECK-HIDDEN takes them), parsed and taken in (ORG-EXPORT-TAKE-IN).
TEXT is bound to the text as expanded, TREE to its parse and DIRECTORY to
FILE's directory, or NIL."
  (let ((original (gensym "ORIGINAL")) (where (gensym "FILE")) (enhanced-p (gensym "ENHANCED")))
    `(let* ((,original ,source)
            (,where ,file)
            (,enhanced-p ,enhanced)
            (,directory (and ,where (tuatara.io:parent-path ,where)))
            (*org-list-allow-alphabetical* ,enhanced-p)
            (*org-export* (let ((export (if ,latex
                                            (make-org-latex ,original ,directory ,where)
                                            (make-org-export ,original ,directory ,where))))
                            (setf (org-export-enhanced export) ,enhanced-p)
                            export))
            (,text (let ((expanded (org-export-expand-macros ,original)))
                     ;; no name to hide printed where Org reads no macros
                     (when ,enhanced-p (org-export-check-hidden expanded ,original ,lines))
                     expanded))
            (,tree (org-parse ,text))
            (*org-export-text* ,text))
       (declare (ignorable ,text ,tree ,directory))
       (org-export-take-in ,text ,tree ,directory)
       ,@body)))
