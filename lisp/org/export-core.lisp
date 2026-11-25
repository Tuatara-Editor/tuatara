;;;; lisp/org/export-core.lisp --- What every export of an Org document shares.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Org document exported as a PDF (latex.lisp), a Word document
;;;; (docx.lisp) or a web page (html.lisp) is read the same way first. Its
;;;; keywords and options are read, with those of its setup files, which are
;;;; taken only when local and in a directory an export may read. Its macros
;;;; are expanded with what redact and protect hide left out, its drawers kept
;;;; or left out as its options say, and its link targets found. ORG-EXPORT
;;;; records the export in hand. Each export then asks the same questions of
;;;; it here: which headlines are written and numbered, how a table is laid
;;;; out, the text between objects with its quote marks, the title, author
;;;; and the file written. Nothing in the document is run.

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
  (untrusted :unknown
             :documentation "Whether the note of ORG-EXPORT is untrusted (ORG-EXPORT-UNTRUSTED-P), :UNKNOWN until
asked.")
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
           :documentation "The texts of the setup files ORG-EXPORT read from trusted directories
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
pass *ORG-MACRO-LIMIT*.")
  (macro-definitions nil
                     :documentation "The macros the keywords of ORG-EXPORT define, as (KEYWORDS . DEFINITIONS), made
again when its keywords change (ORG-MACRO-DEFINITIONS)."))

(defparameter *export-list-styles*
  '(("bullets" :bullet nil nil) ("none" :none nil "no bullets") ("numbers" :decimal "\\arabic{~a}." nil)
    ("letters" :lower-letter "\\alph{~a})" "a") ("Letters" :upper-letter "\\Alph{~a}." "A")
    ("roman" :lower-roman "\\roman{~a}." "i") ("Roman" :upper-roman "\\Roman{~a}." "I"))
  "The styles the style setting of an :EXPORT: drawer gives a list, each
\(WORD KIND LATEX-LABEL SHOWN): the word naming it; how the Word export marks
the items (DOCX-LIST-KIND); the label LaTeX numbers them with, a format of
the counter's name, NIL for bullets or none; and what the marks look like,
said with the word in help, or NIL.")

(defun export-list-style (word)
  "The entry of *EXPORT-LIST-STYLES* that WORD, a style setting's value, names:
the one written as WORD is, else one in any case (NUMBERS, None); NIL for none."
  (and word (or (assoc word *export-list-styles* :test #'string=)
                (assoc word *export-list-styles* :test #'string-equal))))

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

(defun org-export-path (name &optional (directory (and *org-export* (org-export-directory *org-export*))))
  "NAME, a file a note names, as a native path: ~ standing for your home
directory, a relative name taken from DIRECTORY, the directory of the note being
exported when not given, your home directory for a note with no file."
  (tuatara.io:native (tuatara.io:expand-path name :directory (or directory (tuatara.io:home-directory)))))

(defun org-export-full-name-p (name)
  "True when NAME, a file a link names, is a full name (/..., ~/...) and not
relative to the document."
  (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~" name)))

(defun org-export-file-address (path &key whole)
  "The address a document links to the local file PATH, a link's path, with:
a file:/// address for a full name (ORG-EXPORT-FULL-NAME-P) or with WHOLE,
the name relative to the note taken from its directory (ORG-EXPORT-PATH);
else PATH as it stands, relative to the document, encoded as an address."
  (if (or whole (org-export-full-name-p path))
      (path-uri (org-export-path path))
      (tuatara.io:url-path path)))

(defun org-export-untrusted-p (&optional (export *org-export*))
  "True when the note EXPORT exports is untrusted (UNTRUSTED-PATH-P): its file,
links followed, or for one read by its directory alone that directory; a note
with neither, text of your own, is trusted. Decided once for each export."
  (let ((known (org-export-untrusted export)))
    (if (eq known :unknown)
        (setf (org-export-untrusted export)
              (let ((file (org-export-file export)) (directory (org-export-directory export)))
                (cond (file (untrusted-path-p file))
                      (directory (untrusted-path-p directory)))))
        known)))

(defun export-directory-p (path directory
                           &optional directories (untrusted (and directory (untrusted-path-p directory))))
  "True when the file at PATH (native) stands where an export may take files
from. A trusted document (UNTRUSTED NIL, as standard when DIRECTORY, its
directory, is trusted: UNTRUSTED-PATH-P) may take them from anywhere but an
evidence directory (EVIDENCE-PATH-P), whose files are read only from within
the document's own directory; an untrusted one only from DIRECTORY or a
directory in it, or one of DIRECTORIES, links followed
\(TUATARA.IO:REALLY-WITHIN-P), so that a note given to you cannot have its
export carry your other files into what it makes."
  (or (and directory (tuatara.io:really-within-p path directory))
      (tuatara.io:path-in-directories-p path directories)
      (and (not untrusted) (not (evidence-path-p path :follow t)))))

(defun org-export-directory-p (path)
  "True when PATH (native) stands where the export of the document *ORG-EXPORT*
is writing may take files from: anywhere for a trusted document; for an
untrusted one, its directory or a directory in it, or your note templates'
directory (NOTE-TEMPLATES-DIRECTORY, where the pictures of a template made from
a Word document are), links followed (EXPORT-DIRECTORY-P)."
  (export-directory-p path (and *org-export* (org-export-directory *org-export*)) (list (note-templates-directory))
                      (and *org-export* (org-export-untrusted-p))))

(defun org-export-file-allowed-p (path)
  "True when exporting the document *ORG-EXPORT* is writing may take in the
file at PATH (native), it standing where exports take files from
\(ORG-EXPORT-DIRECTORY-P). When it may not, the export says so (ORG-EXPORT-NOTES)
and NIL is returned; a file that does not exist may be named, there being
nothing to take in."
  (or (null (tuatara.io:path-stamp path :failure nil)) ; nothing there to take in
      (org-export-directory-p path)
      (if (and *org-export* (not (org-export-untrusted-p)))
          (export-note "~a: in an evidence directory, so left out of the export" (abbreviated-path path))
          (export-note "~a: outside the note's directory, so left out of the export ~
(the note is untrusted: *untrusted-directories*)"
                       (abbreviated-path path)))))

(define-setting *logos-directory* nil
  "The directory, or directories, of the pictures your templates ask a note for.
This is where the logo a note's #+logo: names is found (LOGOS-DIRECTORIES).
- a directory: that directory.
- a list of directories: those, looked in in order; the first that has the
  picture gives it.
- nil, as standard: logos/ in the configuration directory."
  :values (or (member nil) directory (list-of directory)))

(defun logos-directories ()
  "Where the pictures your templates ask a note for are kept (the logo its
#+logo: names), in the order they are looked in: *LOGOS-DIRECTORY*, or logos/
in the configuration directory (~/.config/tuatara/logos/), each ending in /."
  (if *logos-directory*
      (mapcar (lambda (directory) (tuatara.io:slash-ended (tuatara.io:native (tuatara.io:expand-path directory))))
              (alexandria:ensure-list *logos-directory*))
      (list (tuatara.io:slash-ended (config-path "logos/")))))

(defun logos-picture-path (name)
  "The file NAME, a name alone (logo.png), stands for in your logos directories
\(LOGOS-DIRECTORIES): in the first that has it, else in the first of them."
  (let ((paths (mapcar (lambda (directory) (concatenate 'string directory name)) (logos-directories))))
    (or (find-if #'tuatara.io:path-stamp paths) (first paths))))

(defun template-picture-path (value)
  "The file of the picture a template asks the note for with {{{NAME:path}}},
VALUE what the note's keyword says: a name alone (logo.png) in your logos directories
\(LOGOS-PICTURE-PATH), else a path, read as the note's links are. NIL, said after
the export, for one that is not there, or stands elsewhere than the logos directories
and the directories an export takes files from \(ORG-EXPORT-DIRECTORY-P), so that a
note given to you cannot have your template take in a picture of yours."
  (let* ((name (string-trim " " value))
         (path (if (or (find #\/ name) (uiop:string-prefix-p "~" name))
                   (org-export-path name)
                   (logos-picture-path name))))
    (cond ((not (or (tuatara.io:path-in-directories-p path (logos-directories)) (org-export-directory-p path)))
           (export-note "~a: a picture a template asks for comes from ~{~a~^, ~} or the note's own directory, ~
                         so left out"
                        (abbreviated-path path) (mapcar #'abbreviated-path (logos-directories))))
          ((not (tuatara.io:path-stamp path))
           (export-note "~a: no such picture, so left out" (abbreviated-path path)))
          (t path))))

(defun export-trusted-path-p (path)
  "True when the file at PATH (native) is your own to an export: in your
LaTeX templates' directory, links followed, or trusted (UNTRUSTED-PATH-P)."
  (or (tuatara.io:path-in-directories-p path (list (latex-templates-directory)))
      (not (untrusted-path-p path))))

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

(defun org-options-pairs (line)
  "The options an #+OPTIONS: line gives, LINE being what follows the
keyword: each KEY:VALUE as (KEY . VALUE), in order; a value in brackets,
d:(not \"LOGBOOK\"), kept whole."
  (let ((pairs '()))
    (cl-ppcre:do-register-groups (key value) ("(?:^|\\s)([^\\s:]+):(\\((?:\"[^\"]*\"|[^()\"])*\\)|\\S+)" line)
      (push (cons key value) pairs))
    (nreverse pairs)))

(defun org-export-read-options (export)
  "Fill EXPORT's table of options from its note's #+OPTIONS lines, anew."
  (clrhash (org-export-options export))
  (dolist (line (org-export-keyword-all "OPTIONS" export))
    (loop for (key . value) in (org-options-pairs line)
          do (setf (gethash key (org-export-options export)) value))))

(defparameter *org-option-defaults*
  '(("title" . t) ("author" . t) ("date" . t) ("toc" . t) ("num" . t) ("H" . 3) ("tags" . t) ("todo" . t)
    ("pri" . nil) ("d" . t) ("arch" . headline) ("f" . t) ("e" . t) ("-" . t) ("<" . t) ("\\n" . nil)
    ("^" . "{}") ("redact" . t) ("protect" . nil))
  "What each word of #+OPTIONS: stands for when a note does not give it, each
as (NAME . VALUE): the values ORG-EXPORT-OPTION returns.")

(defparameter *org-default-language* "en"
  "The language of a note with no #+LANGUAGE:, for its PDF and web page.")

(defparameter *docx-default-language* "en-GB"
  "The language a Word document is in when the note's #+LANGUAGE: is not a language's code.")

(defparameter *org-latex-default-class* "article"
  "The LaTeX document class of a note with no #+LATEX_CLASS:.")

(defparameter *org-latex-default-class-options* "[11pt]"
  "The options of the document class of a note with no #+LATEX_CLASS_OPTIONS:.")

(defparameter *org-export-default-exclude-tags* '("noexport")
  "The tags that leave a heading out of an export when the note has no #+EXCLUDE_TAGS:.")

(defun org-option-default (key)
  "What the word KEY of #+OPTIONS: stands for when a note does not give it
\(*ORG-OPTION-DEFAULTS*)."
  (cdr (assoc key *org-option-defaults* :test #'string=)))

(defun org-export-option (key &optional (default (org-option-default key)))
  "The value of the option KEY (#+OPTIONS: KEY:VALUE): T, NIL, a number, or
the word as written; DEFAULT, the option's own (ORG-OPTION-DEFAULT) unless
another is given, when not given."
  (multiple-value-bind (value found) (gethash key (org-export-options *org-export*))
    (if (not found)
        default
        (cond ((string-equal value "t") t)
              ((string-equal value "nil") nil)
              ((every #'digit-char-p value) (parse-integer value))
              (t value)))))

(defun org-export-headline-levels ()
  "How many levels of headlines are written as sections, as #+OPTIONS: H:N
says; 3 when it says nothing. The PDF writes deeper headlines as the items
of a list."
  (let ((h (org-export-option "H")))
    (if (and (integerp h) (plusp h)) h (org-option-default "H"))))

(defun org-export-contents-depth ()
  "How many levels of headlines the table of contents lists, as #+OPTIONS:
toc says: toc:N those of 1 to N stars, counted from the note's highest;
toc:t, or nothing said, as many as are sections (ORG-EXPORT-HEADLINE-LEVELS);
NIL for toc:nil, which leaves the table out."
  (let ((toc (org-export-option "toc")))
    (cond ((null toc) nil)
          ((and (integerp toc) (plusp toc)) toc)
          (t (org-export-headline-levels)))))

;;; Drawers, by #+OPTIONS: d:

(defun org-export-drawer-printed-p (node)
  "True when the export prints the drawer NODE: not an :EXPORT: drawer, which
holds settings; not the :LOGBOOK: unless named; the others as #+OPTIONS: d:
says: t for all, nil for none, a list such as (not \"NOTES\") for all but
those named, (\"NOTES\") for those only."
  (let* ((name (org-property node :drawer-name))
         (value (org-export-option "d"))
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
            ((string= key "time") (format-time (or (first args) "%Y-%m-%d") (get-universal-time)))
            ((string= key "modification-time")
             (let ((stamp (and (org-export-file *org-export*) (tuatara.io:path-stamp (org-export-file *org-export*)))))
               (format-time (or (first args) "%Y-%m-%d")
                            (if stamp (mtime-universal-time (tuatara.io:stamp-mtime stamp)) (get-universal-time)))))
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
  (and (org-export-option kind) t))

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
  (let ((made (org-export-macro-definitions export)))
    (if (and made (eq (car made) (org-export-keywords export)))
        (cdr made)
        (let ((definitions '()))
          (dolist (line (org-export-keyword-all "MACRO" export))
            (let ((name (first (tuatara.text:split-words line))))
              (when name
                (let ((body (string-trim '(#\Space #\Tab) (subseq line (+ (search name line) (length name)))))
                      (known (assoc name definitions :test #'string-equal)))
                  (if known (setf (cdr known) body) (push (cons name body) definitions))))))
          (cdr (setf (org-export-macro-definitions export)
                     (cons (org-export-keywords export) (nreverse definitions))))))))

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
left as they are. The parse of the text returned, its keywords those of
*ORG-EXPORT*, is the second value when a round found no more calls, else NIL."
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
             (when (null calls) (return (values text tree)))
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

(defun org-export-take-in (text tree directory &key keywords-read)
  "Have *ORG-EXPORT* know the note being written, TEXT (its macros expanded)
parsed as TREE, DIRECTORY its directory: its keywords (unless KEYWORDS-READ
says *ORG-EXPORT* holds those of TREE already) and options, its lowest
level of headline, its footnotes' definitions by label, the parts it keeps
out, and, in the enhanced export, its :EXPORT: drawers' settings; for the
PDF and the Word export alike."
  (setf (org-export-text *org-export*) text
        (org-export-tree *org-export*) tree
        (org-export-targets *org-export*) nil)
  (unless keywords-read
    (setf (org-export-keywords *org-export*) (org-export-collect-keywords tree directory)))
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
  (let ((original (gensym "ORIGINAL")) (where (gensym "FILE")) (enhanced-p (gensym "ENHANCED"))
        (parsed (gensym "PARSED")))
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
            (,parsed nil)
            (,text (multiple-value-bind (expanded tree) (org-export-expand-macros ,original)
                     ;; no name to hide printed where Org reads no macros
                     (when ,enhanced-p (org-export-check-hidden expanded ,original ,lines))
                     (setf ,parsed tree)
                     expanded))
            (,tree (or ,parsed (org-parse ,text)))
            (*org-export-text* ,text))
       (declare (ignorable ,text ,tree ,directory))
       (org-export-take-in ,text ,tree ,directory :keywords-read ,parsed)
       ,@body)))

;;; What every export shares

(defvar *export-text-before* nil
  "While the text between a paragraph's objects is written: the character
before it in the note, :OBJECT when an object ends there, NIL at the start of
the text.")

(defvar *export-text-after* nil
  "While the text between a paragraph's objects is written: the character
after it in the note, :OBJECT when an object begins there, NIL at the end.")

(defun export-quote-boundary-p (c)
  "True when C, the character beside a quote mark, is a place a quotation
begins or ends: nothing, a space, a bracket or a dash, or another quote mark."
  (or (null c)
      (and (characterp c)
           (or (member c '(#\Space #\Tab #\Newline #\No-break_space))
               (find c "([{<)]}>-–—/*_+\"'“‘”’")))))

(defparameter *quote-elisions* '("tis" "twas" "twere" "em" "n")
  "The words a single quote may begin as an apostrophe, as in 'tis, 'twas and
rock 'n' roll, not as an opening mark.")

(defun export-quote-mark-at (string i)
  "The typographic mark for the straight quote at I in STRING (\" or ') in the
style of the note's language (*EXPORT-QUOTE-MARKS*): opening where a word
begins, closing everywhere else, and an apostrophe inside a word. A single
quote before a digit ('90s) or before one of *QUOTE-ELISIONS* is an
apostrophe too, so a quoted word that begins like one of them, 'em or 'tis
meant as a quotation, is misread. The characters beside STRING are
*EXPORT-TEXT-BEFORE* and *EXPORT-TEXT-AFTER* (:OBJECT where an object stands)."
  (destructuring-bind (open-double close-double open-single close-single) (export-quote-marks)
    (let* ((n (length string))
           (c (char string i))
           (before (if (plusp i) (char string (1- i)) *export-text-before*))
           (after (if (< (1+ i) n) (char string (1+ i)) *export-text-after*))
           (opening (and (export-quote-boundary-p before)
                         (not (and (characterp before) (find before ")]}>")))
                         (not (export-quote-boundary-p after))
                         (not (and (char= c #\')
                                   (characterp after)
                                   (or (digit-char-p after)
                                       (let ((end (or (position-if-not #'alpha-char-p string :start (1+ i)) n)))
                                         (member (subseq string (1+ i) end) *quote-elisions*
                                                 :test #'string-equal))))))))
      (string (if (char= c #\")
                  (if opening open-double close-double)
                  (if opening open-single close-single))))))

(defun export-smart-quotes (string)
  "STRING with each straight quote written as the mark for it (EXPORT-QUOTE-MARK-AT)."
  (if (find-if (lambda (c) (find c "\"'")) string)
      (with-output-to-string (out)
        (dotimes (i (length string))
          (let ((c (char string i)))
            (if (find c "\"'") (write-string (export-quote-mark-at string i) out) (write-char c out)))))
      string))

(defun org-export-tag-list (settings default)
  "The tags of the keywords SETTINGS (a list: EXPORT_EXCLUDE_TAGS and
EXCLUDE_TAGS ...), or DEFAULT when none is given."
  (let ((values (mapcan #'org-export-keyword-all settings)))
    (if values (mapcan #'tuatara.text:split-words values) default)))

(defun org-export-skipped-p (headline)
  "True when HEADLINE and what is under it are left out: commented, a tag of
#+EXCLUDE_TAGS or #+EXPORT_EXCLUDE_TAGS (noexport), or not among those kept by
#+SELECT_TAGS or #+EXPORT_SELECT_TAGS (export) when some headline has one."
  (let ((tags (org-property headline :tags)))
    (or (org-property headline :commentedp)
        (intersection tags (org-export-tag-list '("EXCLUDE_TAGS" "EXPORT_EXCLUDE_TAGS")
                                                *org-export-default-exclude-tags*)
                      :test #'string=)
        (and (org-property headline :archivedp) (not (eq (org-export-option "arch") t)))
        (let ((kept (org-export-kept *org-export*)))
          (and kept (not (gethash headline kept)))))))

(defun org-export-find-kept (tree)
  "The headlines exported when some carry a select tag: those, what is above
them and what is under them, as a table; NIL when none carries one."
  (let ((select (org-export-tag-list '("SELECT_TAGS" "EXPORT_SELECT_TAGS") '("export")))
        (kept (make-hash-table :test 'eq))
        (any nil))
    (org-walk-tree tree (lambda (node)
                          (when (and (eq (org-node-type node) 'headline)
                                     (intersection (org-property node :tags) select :test #'string=))
                            (setf any t)
                            (org-walk-tree node
                                           (lambda (n)
                                             (when (eq (org-node-type n) 'headline) (setf (gethash n kept) t))))
                            (loop for up = (org-node-parent node) then (org-node-parent up)
                                  while up do (setf (gethash up kept) t)))))
    (and any kept)))

(defun org-export-numbered-p (headline)
  "True when HEADLINE is printed with a number, as #+OPTIONS: num says, but
not with :UNNUMBERED: t, nor numbered: no in its :EXPORT: drawer."
  (and (org-export-option "num") (not (org-headline-property headline "UNNUMBERED"))
       (not (export-setting-no-p headline "numbered"))))

(defun org-toc-keyword (value)
  "What a #+TOC: line of VALUE asks for, as two values: :HEADLINES, :TABLES or
:FIGURES (NIL for another word), and for headlines how many levels deep, a
number below 100, or NIL for all."
  (let ((words (tuatara.text:split-words value)))
    (values (cdr (assoc (first words) '(("headlines" . :headlines) ("tables" . :tables) ("figures" . :figures))
                        :test #'string-equal))
            (let ((depth (second words)))
              (and depth (<= 1 (length depth) 2) (every #'digit-char-p depth) (parse-integer depth))))))

(defun org-export-picture-alone-p (paragraph &optional (picture-p #'org-latex-image-link-p))
  "True when PARAGRAPH holds a picture alone, a link to one with nothing else
but white space around it; PICTURE-P says which links are pictures, those a
PDF shows unless it is given."
  (let ((children (org-node-children paragraph)))
    (and (= 1 (length children)) (funcall picture-p (first children))
         (string= "" (string-trim '(#\Space #\Tab #\Newline)
                                  (concatenate 'string
                                               (subseq *org-export-text*
                                                       (org-node-contents-begin paragraph)
                                                       (org-node-begin (first children)))
                                               (subseq *org-export-text*
                                                       (org-node-end (first children))
                                                       (org-node-contents-end paragraph))))))))

(defun org-export-paragraph-after (node)
  "The paragraph right after NODE among its parent's children, or NIL."
  (let ((next (second (member node (org-node-children (org-node-parent node))))))
    (and next (eq (org-node-type next) 'paragraph) next)))

(defun org-export-cell-text (cell)
  "What CELL holds as written, without the spaces around it."
  (if (org-node-contents-begin cell)
      (string-trim '(#\Space #\Tab)
                   (subseq *org-export-text* (org-node-contents-begin cell) (org-node-contents-end cell)))
      ""))

(defun org-export-cookie-row-p (row)
  "True when ROW holds only column widths and alignments (<l> <r10>...), which are not printed."
  (let ((cells (mapcar #'org-export-cell-text (org-node-children row))))
    (and (some (lambda (c) (plusp (length c))) cells)
         (every (lambda (c) (or (string= c "") (org-table-cookie-p c))) cells))))

(defun org-export-special-column-p (rows)
  "True when the first column of ROWS only marks rows for calculations (! # * ^ _ $ or nothing)."
  (let ((firsts (loop for row in rows
                      when (eq (org-property row :type) 'standard)
                      collect (let ((cell (first (org-node-children row)))) (if cell (org-export-cell-text cell) "")))))
    (and firsts
         (some (lambda (c) (plusp (length c))) firsts)
         (every (lambda (c) (member c '("" "!" "#" "*" "^" "_" "$") :test #'string=)) firsts))))

(defun org-export-table-shape (table)
  "The parts of TABLE an export lays out, as five values: its rows without
the row of widths and alignments, whether its first column only marks rows
for calculations, its rows of cells, its row of widths and alignments or
NIL, and how many columns it prints."
  (let* ((rows (remove-if #'org-export-cookie-row-p (org-node-children table)))
         (special (org-export-special-column-p rows))
         (standard (remove 'rule rows :key (lambda (r) (org-property r :type)))))
    (values rows special standard
            (find-if #'org-export-cookie-row-p (org-node-children table))
            (max 1 (loop for row in standard maximize (- (length (org-node-children row)) (if special 1 0)))))))

(defun org-export-visible-text (text)
  "About what TEXT, a table cell as written, prints as: a redaction its box,
a link its description, markup without its marks."
  (let ((text (cl-ppcre:regex-replace-all *org-redaction-scanner* text
                                          (lambda (match count)
                                            (declare (ignore match))
                                            (make-string (parse-integer count) :initial-element #\n))
                                          :simple-calls t)))
    (org-plain-text (cl-ppcre:regex-replace-all *org-export-snippet-re* text ""))))

(defun org-export-visible-length (text)
  "About how many characters TEXT, a table cell as written, prints as
\(ORG-EXPORT-VISIBLE-TEXT)."
  (length (org-export-visible-text text)))

(defun org-export-longest-piece (text)
  "How many characters the longest part of TEXT, a table cell as written, is
that cannot go on to the next line: a word, or the part of a name or path
between the places LATEX-CODE-BREAK-P allows."
  (let ((text (org-export-visible-text text))
        (run 0)
        (longest 0)
        (word-start nil)
        (word-end nil))
    (dotimes (i (length text) longest)
      (cond ((member (char text i) '(#\Space #\Tab #\Newline)) (setf run 0 word-start nil))
            (t (setf longest (max longest (incf run)))
               (unless word-start
                 (setf word-start i
                       word-end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline))) text :start i)
                                    (length text))))
               (when (latex-code-break-p text i run word-start word-end) (setf run 0)))))))

(defun org-export-objects-between (objects begin end out &key text object blank)
  "Write the note's text from BEGIN below END to OUT, OBJECTS the objects in
it: the text between them as TEXT, a function of a string, gives it, each
object by OBJECT, a function of it and OUT, and the spaces after one as
BLANK, a function of how many, gives them."
  (let ((p begin) (blanks 0))
    (dolist (o objects)
      (let ((*export-text-before* (cond ((and (> p begin) (plusp blanks)) #\Space)
                                       ((> p begin) :object)
                                       ((plusp p) (char *org-export-text* (1- p)))))
            (*export-text-after* :object))
        (write-string (funcall text (subseq *org-export-text* p (org-node-begin o))) out))
      (funcall object o out)
      (when (plusp (org-node-post-blank o))
        (write-string (funcall blank (org-node-post-blank o)) out))
      (setf blanks (org-node-post-blank o)
            p (org-node-end o)))
    (when (and p end (< p end))
      (let ((*export-text-before* (cond ((and (> p begin) (plusp blanks)) #\Space)
                                       ((> p begin) :object)
                                       ((plusp p) (char *org-export-text* (1- p)))))
            (*export-text-after* (and (< end (length *org-export-text*)) (char *org-export-text* end))))
        (write-string (funcall text (subseq *org-export-text* p end)) out)))))

(defun replaced-type (path type)
  "PATH with the ending of its name (after its last dot) replaced by TYPE, or TYPE added when it has none."
  (let ((dot (position #\. path :start (length (tuatara.io:parent-path path)) :from-end t)))
    (concatenate 'string (subseq path 0 (or dot (length path))) "." type)))

(defun org-export-file-name (text file)
  "Where the Org note TEXT, of FILE or NIL, says its export goes: its
#+EXPORT_FILE_NAME as a full name ending in .tex whatever ending it gives (so
that an export never writes over a file of another kind), or NIL when it gives
none. Its #+ lines alone are read, as written: every export takes the name from
here, so that the name a note is checked for is the name it is written to."
  (with-org-note-keywords (text (and file (tuatara.io:parent-path file)) file)
    (let ((name (org-export-keyword "EXPORT_FILE_NAME")))
      (when (and name (plusp (length (string-trim " " name))))
        (replaced-type (org-export-path (string-trim " " name)) "tex")))))

(defun org-export-title ()
  "The title of the document being written: its #+TITLE: lines joined by
spaces, else its file's name without its ending, else an empty string."
  (let ((title (format nil "~{~a~^ ~}" (org-export-keyword-all "TITLE")))
        (file (org-export-file *org-export*)))
    (if (and (zerop (length title)) file) (tuatara.io:native-file-stem file) title)))

(defun org-export-author ()
  "The author of the document being written, when #+OPTIONS: says to show one:
its #+AUTHOR:, else your name (ORG-LATEX-USER-NAME); NIL when not shown."
  (and (org-export-option "author") (or (org-export-keyword "AUTHOR") (user-full-name))))

(defun org-export-created ()
  "The time the document is being written, as its {{{created}}} place says it."
  (format-time "%Y-%m-%d %a %H:%M" (get-universal-time)))
