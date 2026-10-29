;;;; lisp/org/formatted-blocks.lisp --- Dynamic blocks of files with formatted names: links, backlinks, their text, sequences.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's denote-org writes its dynamic blocks (by Protesilaos Stavrou,
;;;; GPL-3.0-or-later), line for line, so that a note kept in both editors
;;;; stays the same in each: formatted-links lists links to the files whose
;;;; names a regular expression matches, formatted-missing-links those of them
;;;; the file does not link to yet, formatted-backlinks the files that link to
;;;; it, formatted-files and formatted-files-as-headings their text, and
;;;; formatted-sequence a file and those under it in a sequence. Denote's own
;;;; names for them (denote-links and the rest) make the same blocks, their
;;;; regular expressions read as Emacs writes them and case ignored, as Denote
;;;; reads them; formatted-... reads them as Tuatara's searches do (smart
;;;; case). Like every dynamic block they are made on a thread of their own:
;;;; the files are read only as text, the kinds notes are kept in.

(in-package #:tuatara.defaults)

;;; Regular expressions in Org's regexp syntax

(defun org-regexp-char-class (name)
  "What a set of characters holds for the class [:NAME:] of Org's regexp
syntax, written as
CL-PPCRE reads it inside [...]: ranges of letters, Latin ones past ASCII
among them; NIL for a class Tuatara does not know."
  (let ((latin (format nil "~c-~c" (code-char #xC0) (code-char #x24F)))
        (punct "!-/:-@\\[-`{-~"))
    (cond ((string= name "digit") "0-9")
          ((string= name "xdigit") "0-9a-fA-F")
          ((string= name "alpha") (format nil "a-zA-Z~a" latin))
          ((string= name "alnum") (format nil "0-9a-zA-Z~a" latin))
          ((string= name "upper") (format nil "A-Z~c-~c" (code-char #xC0) (code-char #xDE)))
          ((string= name "lower") (format nil "a-z~c-~c" (code-char #xDF) (code-char #xFF)))
          ((string= name "word") "\\w")
          ((member name '("space") :test #'string=) "\\s")
          ((string= name "blank") " \\t")
          ((string= name "punct") punct)
          ((string= name "cntrl") (format nil "~c-~c" (code-char 0) (code-char 31)))
          ((member name '("ascii" "unibyte") :test #'string=) (format nil "~c-~c" (code-char 0) (code-char 127)))
          ((member name '("nonascii" "multibyte") :test #'string=) (format nil "~c-~c" (code-char 128) (code-char (1- char-code-limit))))
          ((string= name "graph") (format nil "!-~~~c-~c" (code-char 161) (code-char (1- char-code-limit))))
          ((string= name "print") (format nil " -~~~c-~c" (code-char 160) (code-char (1- char-code-limit))))
          (t nil))))

(defun org-regexp-to-ppcre (regex)
  "REGEX, a regular expression in Org's regexp syntax, as one CL-PPCRE reads:
its groups, alternatives and counts (a backslash before the bracket, the
bar or the brace) made CL-PPCRE's, and a bracket, bar or brace with no
backslash a letter; its marks for the text's start and end, the edges of
words and symbols, words and spaces made CL-PPCRE's, [:class:] as
ORG-REGEXP-CHAR-CLASS has it; a backslash in [...] a letter; ^ and $ anchors only
where that syntax takes them so, * + ? at the start letters. NIL for what cannot be
read so: a category, a syntax class not listed, a backslash at the end."
  (let ((n (length regex)) (i 0) (start t)
        (out (make-string-output-stream)))
    (flet ((emit (text) (write-string text out))
           (fail () (return-from org-regexp-to-ppcre nil)))
      (loop while (< i n)
            do (let ((c (char regex i)) (next-start nil))
                 (incf i)
                 (cond
                   ((char= c #\\)
                    (when (>= i n) (fail))
                    (let ((d (char regex i)))
                      (incf i)
                      (case d
                        (#\( (cond ((and (< (1+ i) n) (char= (char regex i) #\?) (char= (char regex (1+ i)) #\:))
                                    (emit "(?:") (incf i 2))
                                   ((and (< i n) (char= (char regex i) #\?))
                                    ;; \(?3: a group of a number given: a group
                                    (let ((colon (position #\: regex :start i)))
                                      (unless (and colon (> colon (1+ i)) (every #'digit-char-p (subseq regex (1+ i) colon)))
                                        (fail))
                                      (emit "(") (setf i (1+ colon))))
                                   (t (emit "(")))
                             (setf next-start t))
                        (#\) (emit ")"))
                        (#\| (emit "|") (setf next-start t))
                        (#\{ (emit "{"))
                        (#\} (emit "}"))
                        (#\` (emit "\\A"))
                        (#\' (emit "\\z"))
                        (#\= nil)
                        (#\b (emit "\\b"))
                        (#\B (emit "\\B"))
                        (#\< (emit "\\b(?=\\w)"))
                        (#\> (emit "\\b(?<=\\w)"))
                        (#\_ (when (>= i n) (fail))
                         (case (char regex i)
                           (#\< (emit "(?<![\\w_-])(?=[\\w_-])"))
                           (#\> (emit "(?<=[\\w_-])(?![\\w_-])"))
                           (t (fail)))
                         (incf i))
                        (#\w (emit "\\w"))
                        (#\W (emit "\\W"))
                        ((#\s #\S)
                         (when (>= i n) (fail))
                         (let ((class (char regex i)))
                           (incf i)
                           (emit (cond ((member class '(#\- #\Space)) (if (char= d #\s) "\\s" "\\S"))
                                       ((char= class #\w) (if (char= d #\s) "\\w" "\\W"))
                                       ((char= class #\_) (if (char= d #\s) "[\\w_-]" "[^\\w_-]"))
                                       ((char= class #\.) (format nil "[~:[^~;~]~a]" (char= d #\s) (org-regexp-char-class "punct")))
                                       (t (fail))))))
                        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (emit (format nil "\\~c" d)))
                        ((#\c #\C) (fail))
                        (t (emit (cl-ppcre:quote-meta-chars (string d)))))))
                   ((char= c #\[)
                    ;; a set of characters: ] first a letter, a backslash a letter, [:class:] as it is
                    (emit "[")
                    (when (and (< i n) (char= (char regex i) #\^)) (emit "^") (incf i))
                    (when (and (< i n) (char= (char regex i) #\])) (emit "\\]") (incf i))
                    (loop (when (>= i n) (fail))
                          (let ((e (char regex i)))
                            (cond ((char= e #\]) (incf i) (emit "]") (return))
                                  ((and (char= e #\[) (< (1+ i) n) (char= (char regex (1+ i)) #\:))
                                   (let* ((close (search ":]" regex :start2 (+ i 2)))
                                          (class (and close (org-regexp-char-class (subseq regex (+ i 2) close)))))
                                     (unless class (fail))
                                     (emit class)
                                     (setf i (+ close 2))))
                                  ((find e "\\[") (emit (format nil "\\~c" e)) (incf i))
                                  (t (emit (string e)) (incf i))))))
                   ((char= c #\^) (emit (if start "^" "\\^")) (setf next-start start))
                   ((char= c #\$)
                    (emit (if (or (= i n) (and (< (1+ i) n) (char= (char regex i) #\\) (find (char regex (1+ i)) ")|")))
                              "$" "\\$")))
                   ((find c "*+?") (emit (if start (format nil "\\~c" c) (string c))))
                   ((find c "()|{}") (emit (format nil "\\~c" c)))
                   (t (emit (string c))))
                 (setf start next-start))))
    (let ((translated (get-output-stream-string out)))
      (and (ignore-errors (tuatara.syntax:compile-pattern translated))
           translated))))

(defun formatted-block-scanner (params key denote)
  "A scanner for the regular expression a dynamic block's PARAMS give under
KEY: with DENOTE in Org's regexp syntax, case ignored; else as
Tuatara's searches read it, case ignored unless it has a capital letter. NIL
when there is none; :BAD, and said, when it is no regular expression or is
not text (a list)."
  (let ((regex (formatted-param params key)))
    (cond ((null regex) nil)
          ((not (stringp regex))
           (message "The block's ~(~s~) is not read: write it as a string" key)
           :bad)
          (t (let ((translated (if denote (org-regexp-to-ppcre regex) regex)))
               (or (and translated
                        (ignore-errors (if denote
                                           (tuatara.syntax:compile-pattern translated :case-fold t)
                                           (tuatara.syntax:compile-pattern translated))))
                   (progn (message "The block's ~(~s~) ~s is no regular expression" key regex)
                          :bad)))))))

(defun formatted-param (params key)
  "The value a dynamic block's PARAMS give KEY, the words nil and t taken
as NIL and T."
  (let ((value (getf params key)))
    (cond ((equal value "nil") nil)
          ((equal value "t") t)
          (t value))))

;;; The files, listed as Denote lists them

(defparameter *formatted-block-most-files* 100000
  "The most files a dynamic block of files with formatted names looks at: the
rest of a larger directory left out, and said to be.")

(defun formatted-block-listing (&optional excluded)
  "The files with formatted names in *FORMATTED-FILE-DIRECTORY* and its
subdirectories, as a dynamic block lists them, as Denote does: in each
directory the files of its subdirectories first, then its own, each in order
of name; hidden files and directories, backups and names with no identifier
left out, links to directories not followed, and the directories whose path
from there EXCLUDED, a scanner, matches. A list of (PATH . RELATIVE), PATH
the native path, RELATIVE the path from that directory. Each directory is
listed as every walk lists it (DIRECTORY-LISTING)."
  (let ((count 0) (said nil))
    (labels ((walk (directory relative)
               (declare (notinline index-directory-entries))
               (dblock-check-stop)
               (let ((entries (sort (loop for (name kind) in (index-directory-entries (directory-listing directory :rules nil))
                                          collect (list (if (eq kind :directory) (concatenate 'string name "/") name)
                                                        name kind))
                                    #'string< :key #'car))
                     (below '()) (own '()))
                 (loop for (nil name kind) in entries
                       do (let ((path (concatenate 'string directory name))
                                (inside (concatenate 'string relative name)))
                              (case kind
                                (:directory
                                 (unless (and excluded (cl-ppcre:scan excluded inside))
                                   (setf below (append below (walk (concatenate 'string path "/") (concatenate 'string inside "/"))))))
                                (:regular
                                 (when (name-parts name)
                                   (if (>= count *formatted-block-most-files*)
                                       (unless said
                                         (setf said t)
                                         (message "A dynamic block looks at the first ~:d files only" *formatted-block-most-files*))
                                       (progn (incf count) (push (cons path inside) own))))))))
                 (append below (nreverse own)))))
      (walk (directory-name *formatted-file-directory*) ""))))

(defun formatted-own-path ()
  "The native path of the file of the buffer whose block is made, or NIL."
  (let ((file (and *current-buffer* (tuatara.buffer:buffer-file *current-buffer*))))
    (and file (tuatara.io:native file))))

(defun formatted-block-files (params denote &key (omit t) (regexp :regexp))
  "The files a dynamic block's PARAMS name: those whose path from *FORMATTED-FILE-DIRECTORY* the REGEXP
parameter matches (all when it has none), the file of the block left out
with OMIT, then those whose full path :NOT-REGEXP matches left out, the
directories :EXCLUDED-DIRS-REGEXP matches not looked in (FORMATTED-BLOCK-SCANNER
reads them, DENOTE saying how). :BAD when one of them is no regular expression."
  (let ((matching (and regexp (formatted-block-scanner params regexp denote)))
        (leaving (formatted-block-scanner params :not-regexp denote))
        (excluded (formatted-block-scanner params :excluded-dirs-regexp denote))
        (own (and omit (formatted-own-path))))
    (if (member :bad (list matching leaving excluded))
        :bad
        (loop for file in (formatted-block-listing excluded)
              when (and (or (null matching) (cl-ppcre:scan matching (cdr file)))
                        (not (and own (string= own (car file))))
                        (not (and leaving (cl-ppcre:scan leaving (car file)))))
                collect file))))

;;; Names, kinds and titles

(defun formatted-file-name-of (file)
  "The name of FILE, (PATH . RELATIVE), without its directory."
  (let ((slash (position #\/ (cdr file) :from-end t)))
    (if slash (subseq (cdr file) (1+ slash)) (cdr file))))

(defun formatted-identifier (file)
  "The identifier in the name of FILE, (PATH . RELATIVE), or NIL."
  (values (name-parts (formatted-file-name-of file))))

(defun formatted-signature (file)
  "The signature in the name of FILE, (PATH . RELATIVE), or NIL."
  (nth-value 1 (name-parts (formatted-file-name-of file))))

(defun formatted-name-title (file)
  "The title in the name of FILE, (PATH . RELATIVE), as it is written there
(my-title), or NIL."
  (nth-value 2 (name-parts (formatted-file-name-of file))))

(defun formatted-name-keywords (file)
  "The keywords in the name of FILE, (PATH . RELATIVE), as one string as they
are written there (alpha_beta), or NIL."
  (let ((keywords (nth-value 3 (name-parts (formatted-file-name-of file)))))
    (and keywords (format nil "~{~a~^_~}" keywords))))

(defun formatted-file-kind (file &optional text)
  "The kind of note FILE, (PATH . RELATIVE), is by the end of its name, as
Denote tells them: :ORG (.org), :MARKDOWN-YAML or, when its TEXT has a
title = line and no title: line, :MARKDOWN-TOML (.md), :TEXT (.txt); NIL for
another kind of file, an encrypted one too."
  (let ((extension (name-extension (formatted-file-name-of file))))
    (cond ((string-equal extension ".org") :org)
          ((string-equal extension ".md")
           (if (and text (not (cl-ppcre:scan "(?im)^title[ \\t]*:" text)) (cl-ppcre:scan "(?im)^title[ \\t]*=" text))
               :markdown-toml
               :markdown-yaml))
          ((string-equal extension ".txt") :text)
          (t nil))))

(defun formatted-file-text (file)
  "The text of FILE, (PATH . RELATIVE), as a dynamic block reads it
(DBLOCK-SOURCE-TEXT): its buffer's when open, else the disk's; NIL when it
cannot be read, and for an encrypted file (ENCRYPTED-FILE-P), even one a
buffer shows decrypted, so that no block carries its text, its links or its
title into another file."
  (let ((text (dblock-source-text (car file))))
    (unless (encrypted-file-p (car file) text)
      text)))

(defun formatted-front-matter (text kind)
  "The title and the tags the front matter of TEXT gives, as two values, read
as a note of KIND (FORMATTED-FILE-KIND) writes them (FRONT-MATTER)."
  (front-matter text (if (member kind '(:markdown-yaml :markdown-toml)) :markdown kind)))

(defun formatted-title-or-name (file kind)
  "The title of FILE, (PATH . RELATIVE), its front matter read as a note of KIND writes it: for a note of a
kind notes are kept in, the title of its front matter, else of its name, else
an empty string."
  (let ((name-title (formatted-name-title file)))
    (if (formatted-file-kind file)
        (or (and kind (let ((text (formatted-file-text file))) (and text (formatted-front-matter text kind))))
            name-title
            "")
        (or name-title ""))))

(defun formatted-link-description (file &optional kind)
  "The label of a link to FILE, (PATH . RELATIVE): its
signature, two spaces and its title when it has a signature, else its title;
the title read as a note of KIND writes it (FORMATTED-TITLE-OR-NAME), of its
own kind when KIND is NIL."
  (let* ((kind (or kind (formatted-file-kind file (formatted-file-text file))))
         (title (formatted-title-or-name file kind))
         (signature (formatted-signature file)))
    (if signature (format nil "~a  ~a" signature title) title)))

(defun formatted-denote-link (file description id-only)
  "An Org link to FILE, (PATH . RELATIVE), by its identifier, labelled
DESCRIPTION; with ID-ONLY, or with no DESCRIPTION, the identifier alone."
  (org-link-text (format nil "denote:~a" (formatted-identifier file)) (if id-only "" description)))

(defun formatted-identifier-date (identifier)
  "The date IDENTIFIER (20260106T090000) stands for, 2026-01-06, or NIL when it is not a date."
  (and identifier (identifier-at-start-p identifier) (identifier-date identifier)))

(defun formatted-link-lines (files id-only include-date)
  "The lines of a block of links to FILES, (PATH . RELATIVE) each: - and a link, labelled as FORMATTED-LINK-DESCRIPTION
labels it, the titles as their notes write them; with INCLUDE-DATE the title and
the date of its identifier; with ID-ONLY the identifier alone."
  (with-output-to-string (out)
    (loop for file in files
          for i from 0
          do (dblock-progress (/ i (max 1 (length files))))
             (let ((description
                     (cond (id-only nil)
                           (include-date
                            (let ((title (formatted-title-or-name file :org))
                                  (date (formatted-identifier-date (formatted-identifier file))))
                              (if date (format nil "~a (~a)" title date) title)))
                           (t (formatted-link-description file :org)))))
               (format out "- ~a~%" (formatted-denote-link file description id-only))))))

;;; Sorting

(defun formatted-block-sorted (files component reverse)
  "FILES, (PATH . RELATIVE) each, sorted by
COMPONENT: \"title\", \"keywords\", \"signature\" or \"identifier\" of their
names (one with none after those with one), \"last-modified\" (the latest
first), \"random\"; another, or NIL, leaves them in order. REVERSE turns the
order round, but for random."
  (let ((part (cond ((equal component "title") #'formatted-name-title)
                    ((equal component "keywords") #'formatted-name-keywords)
                    ((equal component "signature") #'formatted-signature)
                    ((equal component "identifier") #'formatted-identifier))))
    (cond ((equal component "random")
           (let ((vector (coerce files 'vector)))
             (loop for i from (1- (length vector)) downto 1
                   do (rotatef (aref vector i) (aref vector (random (1+ i)))))
             (coerce vector 'list)))
          (t (let ((sorted (cond (part
                                  (stable-sort (copy-list files)
                                               (lambda (a b)
                                                 (let ((one (funcall part a)) (two (funcall part b)))
                                                   (cond ((or (null one) (string= one "")) nil)
                                                         ((or (null two) (string= two "")) t)
                                                         (t (string< one two)))))))
                                 ((equal component "last-modified")
                                  (flet ((modified (file)
                                           (let ((stamp (ignore-errors (tuatara.io:path-stamp (car file)))))
                                             (if stamp (tuatara.io:stamp-mtime stamp) 0))))
                                    (stable-sort (copy-list files) #'> :key #'modified)))
                                 (t (copy-list files)))))
               (if reverse (reverse sorted) sorted))))))

;;; Links in a note's text

(defparameter *formatted-org-link-re*
  "\\[\\[denote:([^\\]\\[]*?)(?:::.*)?\\]\\[(.*?)\\]\\]|\\[\\[denote:([^\\]\\[]*?)\\]\\]"
  "A link to a note by its identifier as an Org note writes it, labelled or not: the
identifier the first or the third group.")

(defparameter *formatted-markdown-link-re*
  "\\[(.*?)\\]\\(denote:([^\\]\\[]*?)\\)|\\[\\[denote:([^\\]\\[]*?)\\]\\]"
  "A link to a note by its identifier as Markdown writes it, or as an Org note
writes one with no label: the identifier the second or the third group.")

(defun formatted-linked-identifiers (text kind)
  "The identifiers the links of TEXT, a note of KIND, lead to, each once, in a
hash table."
  (let ((found (make-hash-table :test 'equal))
        (markdown (member kind '(:markdown-yaml :markdown-toml))))
    (cl-ppcre:do-scans (start end starts ends (if markdown *formatted-markdown-link-re* *formatted-org-link-re*) text)
      (let ((group (if markdown
                       (if (aref starts 1) 1 2)
                       (if (aref starts 0) 0 2))))
        (when (aref starts group)
          (setf (gethash (subseq text (aref starts group) (aref ends group)) found) t))))
    found))

;;; Backlinks

(defun formatted-backlinks (identifier files)
  "The FILES, (PATH . RELATIVE) each, that link to the note IDENTIFIER, in the
order: by kind, Org, Markdown, then
text; of each kind first those with a link to the note itself, then those
with a link to a place in it (::), each in the order of FILES."
  (let ((by-kind (make-hash-table)) (texts (make-hash-table :test 'eq)))
    (loop for file in files
          for i from 0
          do (dblock-progress (/ i (max 1 (length files))))
             (let ((kind (formatted-file-kind file)))
               (when kind
                 (let* ((text (formatted-file-text file))
                        (kind (and text (formatted-file-kind file text))))
                   (when (and text (gethash identifier (formatted-linked-identifiers text kind)))
                     (setf (gethash file texts) text)
                     (push file (gethash kind by-kind)))))))
    (loop for kind in '(:org :markdown-yaml :markdown-toml :text)
          for linking = (reverse (gethash kind by-kind))
          for markdown = (member kind '(:markdown-yaml :markdown-toml))
          for whole = (format nil (if markdown "(denote:~a)" "[denote:~a]") identifier)
          for place = (format nil (if markdown "(denote:~a::" "[denote:~a::") identifier)
          append (let ((first (remove-if-not (lambda (file) (search whole (gethash file texts))) linking)))
                   (append first
                           (remove-if-not (lambda (file) (and (not (member file first)) (search place (gethash file texts))))
                                          linking))))))

(defun formatted-heading-backlinks (identifier custom-id files)
  "The FILES, (PATH . RELATIVE) each, whose text links to the place CUSTOM-ID
of the note IDENTIFIER (IDENTIFIER::#CUSTOM-ID), in order."
  (let ((target (format nil "~a::#~a" identifier custom-id)))
    (loop for file in files
          for i from 0
          do (dblock-progress (/ i (max 1 (length files))))
          when (and (formatted-file-kind file)
                    (let ((text (formatted-file-text file))) (and text (search target text))))
            collect file)))

(defun formatted-own-custom-id ()
  "The CUSTOM_ID of the headline the block being made is under, or NIL."
  (let* ((tree (first *dblock-own*))
         (headline (and tree *dblock-position* (org-current-headline tree *dblock-position*))))
    (and headline (org-headline-property headline "CUSTOM_ID"))))

;;; The text of files

(defparameter *formatted-separator* (format nil "~%~%~a~%~%~%" (make-string 50 :initial-element #\-))
  "What stands between the texts of files a block holds when its
:file-separator is t.")

(defparameter *formatted-block-most-text* (* 64 1024 1024)
  "The most characters of files' text a dynamic block holds: the files past
that left out, and said to be.")

(defun formatted-escape-code (text)
  "TEXT with a zero-width space before each #+ that begins a line, spaces
before it or not, as a file's text is written into a block, so that
its keywords are not taken for the note's own."
  (cl-ppcre:regex-replace-all "(?m)^([ \\t]*)#\\+" text (format nil "\\1~c#+" (code-char #x200B))))

(defun formatted-indent-lines (text column)
  "TEXT with each line that holds more than spaces begun at COLUMN, its own
indentation left out, and each line of spaces only made empty."
  (cl-ppcre:regex-replace-all
   "(?m)^[ \\t]*([^\\n]?)"
   text
   (lambda (match first &rest more)
     (declare (ignore match more))
     (if (string= first "") "" (concatenate 'string (make-string column :initial-element #\Space) first)))
   :simple-calls t))

(defun formatted-without-front-matter (text how)
  "TEXT with its front matter left out, as a block's :no-front-matter says
HOW: a number, that many lines; else up to the first empty line and it, the
text kept whole when it has none."
  (if (integerp how)
      (let ((at 0))
        (loop repeat how
              while (< at (length text))
              do (setf at (let ((newline (position #\Newline text :start at))) (if newline (1+ newline) (length text)))))
        (subseq text at))
      (multiple-value-bind (start end) (cl-ppcre:scan "(?m)^$" text)
        (declare (ignore start))
        (if (and end (< end (length text)))
            (subseq text (1+ end))
            text))))

(defun formatted-file-contents (file no-front-matter add-links)
  "The text of FILE, (PATH . RELATIVE), as a block of files' text holds it: its front matter left out as NO-FRONT-MATTER says; with ADD-LINKS a
link to it first and its text indented by two, the link of its identifier
alone when ADD-LINKS is id-only; each #+ beginning a line escaped. NIL when it
cannot be read."
  (let ((text (formatted-file-text file)))
    (when text
      (let* ((text (if no-front-matter (formatted-without-front-matter text no-front-matter) text))
             (text (if add-links (formatted-indent-lines text 2) text)))
        (concatenate 'string
                     (if add-links
                         (let ((id-only (equalp add-links "id-only")))
                           (format nil "- ~a~%~%" (formatted-denote-link file (and (not id-only) (formatted-link-description file)) id-only)))
                         "")
                     (formatted-escape-code text))))))

(defun formatted-heading-contents (file add-links exclude-tags)
  "The text of FILE, (PATH . RELATIVE), an Org note, as a block of files'
text under headlines holds it: its front matter, up to the first
empty line, left out and its #+title made a headline above its own, which go
one level down, its #+filetags that headline's tags unless EXCLUDE-TAGS, the
title a link to it with ADD-LINKS; each #+ beginning a line escaped. A note
with no title or no tags gets no headline. NIL when it cannot be read."
  (let ((text (formatted-file-text file)))
    (when text
      (multiple-value-bind (title tags) (formatted-front-matter text :org)
        (let* ((tags (cond (exclude-tags "") (tags (format nil ":~{~a:~}" tags))))
               (body (formatted-without-front-matter text t))
               (heading (cond ((not (and title tags)) "")
                              (add-links (format nil "* [[denote:~a][~a]] ~a~%~%" (formatted-identifier file) title tags))
                              (t (format nil "* ~a ~a~%~%" title tags))))
               ;; the tags put in their column first, then the note's own headlines a level down
               (aligned (and title tags (not exclude-tags)))
               (heading (if aligned (formatted-tags-aligned heading) heading))
               (body (if aligned (formatted-tags-aligned body) body))
               (body (org-shift-levels body 1)))
          (formatted-escape-code (concatenate 'string heading body)))))))

(defun formatted-tags-aligned (text)
  "TEXT, Org's, with the tags of its headlines where *ORG-TAGS-COLUMN* says."
  (let ((edits (org-tags-alignment-edits (org-headlines (org-parse text)) text)))
    (loop for (start end new) in (sort edits #'> :key #'first)
          do (setf text (concatenate 'string (subseq text 0 start) new (subseq text end))))
    text))

(defun formatted-joined (pieces separator)
  "PIECES, texts, one after another with SEPARATOR between, at most
*FORMATTED-BLOCK-MOST-TEXT* characters of them: those past it left out, and
said to be."
  (let ((total 0) (kept '()))
    (dolist (piece pieces)
      (incf total (length piece))
      (if (> total *formatted-block-most-text*)
          (progn (message "The block holds the first ~:d characters of the files only" *formatted-block-most-text*)
                 (return))
          (push piece kept)))
    (format nil (concatenate 'string "~{~a~^" (cl-ppcre:regex-replace-all "~" separator "~~") "~}") (nreverse kept))))

(defun formatted-final-newline (text)
  "TEXT, a block's, ending with a line end unless it is empty."
  (if (string= text "") text (org-ensure-newline text)))

;;; Sequences

(defun formatted-sequence-p (signature)
  "True when SIGNATURE is a sequence, numbered 1=1=2 or written with letters,
1a2."
  (and signature
       (plusp (length signature))
       (digit-char-p (char signature 0))
       (not (char= (char signature (1- (length signature))) #\=))
       (every (lambda (c) (or (alphanumericp c) (char= c #\=))) signature)))

(defun formatted-sequence-numeric-p (sequence)
  "True when SEQUENCE is numbered: digits and =."
  (and (formatted-sequence-p sequence) (every (lambda (c) (or (digit-char-p c) (char= c #\=))) sequence)))

(defun formatted-sequence-parts (sequence)
  "The levels of SEQUENCE: 1=1=2 as (1 1 2), 1a2 as (1 a 2), as strings."
  (if (formatted-sequence-numeric-p sequence)
      (remove "" (cl-ppcre:split "=" sequence) :test #'string=)
      (cl-ppcre:all-matches-as-strings "[0-9]+|[A-Za-z]+" (remove #\= sequence))))

(defun formatted-sequence-key (sequence)
  "What SEQUENCE is ordered by: each level padded
on the left to five places, the levels joined by =."
  (format nil "~{~a~^=~}" (mapcar (lambda (part) (format nil "~5@a" part)) (formatted-sequence-parts sequence))))

(defun formatted-sequence-files (files)
  "Those of FILES, (PATH . RELATIVE) each, whose signature is a sequence."
  (remove-if-not (lambda (file) (formatted-sequence-p (formatted-signature file))) files))

(defun formatted-sequence-family (sequence files)
  "Of FILES with sequences, the one whose sequence is SEQUENCE (an Org one
first when there are more) and those under it at any depth, numbered as it
is; NIL when there is none of the first."
  (let* ((parts (formatted-sequence-parts sequence))
         (depth (length parts))
         (same (remove-if-not (lambda (file) (string= sequence (formatted-signature file))) files))
         (parent (if (rest same)
                     (or (find :org same :key #'formatted-file-kind) (find-if #'formatted-file-kind same) (first same))
                     (first same)))
         (children (remove-if-not (lambda (file)
                                    (let ((other (formatted-signature file)))
                                      (and (formatted-sequence-numeric-p other)
                                           (let ((other-parts (formatted-sequence-parts other)))
                                             (and (> (length other-parts) depth)
                                                  (every #'string= parts other-parts))))))
                                  files)))
    (and parent (cons parent children))))

(defun formatted-sequence-lines (files depth)
  "The lines of a sequence block of FILES with sequences: in the order of their sequences, those within DEPTH levels of the
first (all with no DEPTH), each a list item indented by two for each level
below it, its link labelled by its sequence, a colon and its label."
  (let* ((sorted (stable-sort (copy-list files) #'string< :key (lambda (file) (formatted-sequence-key (formatted-signature file)))))
         (root (length (formatted-sequence-parts (formatted-signature (first sorted)))))
         (kept (remove-if-not (lambda (file)
                                (or (not (integerp depth))
                                    (<= (length (formatted-sequence-parts (formatted-signature file))) (+ root depth -1))))
                              sorted)))
    (with-output-to-string (out)
      (dolist (file kept)
        (let ((sequence (formatted-signature file)))
          (format out "~a- ~a~%"
                  (make-string (* 2 (- (length (formatted-sequence-parts sequence)) root)) :initial-element #\Space)
                  (formatted-denote-link file (format nil "~a: ~a" sequence (formatted-link-description file)) nil)))))))

;;; The writers

(defun formatted-block-name-line (params)
  "The #+name: line a block's :block-name asks for, or an empty string."
  (let ((name (formatted-param params :block-name)))
    (if name (format nil "#+name: ~a~%" name) "")))

(defun formatted-block-text (kind params denote)
  "The text of a dynamic block of files with formatted names of KIND (links,
missing-links, backlinks, files, files-as-headings, sequence) its PARAMS ask
for; DENOTE when the block has its denote- name (its
regular expressions in Org's regexp syntax). NIL, and said, when it cannot
be made: the block is then left as it is."
  (let ((sort (formatted-param params :sort-by-component))
        (reverse (formatted-param params :reverse-sort))
        (id-only (formatted-param params :id-only))
        (include-date (formatted-param params :include-date)))
    (flet ((files (&rest keys)
             (let ((files (apply #'formatted-block-files params denote keys)))
               (if (eq files :bad) (return-from formatted-block-text nil) files))))
      (cond
        ((string= kind "links")
         (let ((files (formatted-block-sorted (files) sort reverse)))
           (concatenate 'string (formatted-block-name-line params) (formatted-link-lines files id-only include-date))))
        ((string= kind "missing-links")
         (let* ((all (files))
                (linked (formatted-linked-identifiers *org-current-text* :org))
                (missing (remove-if (lambda (file) (gethash (formatted-identifier file) linked)) all)))
           ;; under its denote- name, a note that links to none of them is given none
           (when (and denote (zerop (hash-table-count linked)))
             (setf missing '()))
           (concatenate 'string (formatted-block-name-line params)
                        (formatted-link-lines (formatted-block-sorted missing sort reverse) id-only include-date))))
        ((string= kind "backlinks")
         (let* ((own (formatted-own-path))
                (identifier (and own (values (name-parts (subseq own (1+ (or (position #\/ own :from-end t) -1))))))))
           (cond ((null identifier)
                  (message "This file has no identifier in its name: nothing links to it")
                  nil)
                 ((not (uiop:string-prefix-p (directory-name *formatted-file-directory*) own))
                  ;; a file outside the directory has none
                  (string #\Newline))
                 (t
                  (let* ((heading (formatted-param params :this-heading-only))
                         (custom-id (and heading (formatted-own-custom-id))))
                    (when (and heading (null custom-id))
                      (message "The headline of the block has no CUSTOM_ID to link to")
                      (return-from formatted-block-text nil))
                    (let ((found (if heading
                                     (formatted-heading-backlinks identifier custom-id (files :regexp nil))
                                     (formatted-backlinks identifier (files :regexp nil :omit nil)))))
                      (if (null found)
                          (string #\Newline)
                          (let* ((leaving (formatted-block-scanner params :not-regexp denote))
                                 (found (formatted-block-sorted found sort reverse))
                                 (found (if (and leaving (not (eq leaving :bad)))
                                            (remove-if (lambda (file) (cl-ppcre:scan leaving (car file))) found)
                                            found)))
                            (formatted-link-lines found id-only include-date)))))))))
        ((string= kind "files")
         (if (null (formatted-param params :regexp))
             ""
             (let* ((files (remove-if-not #'formatted-file-kind (formatted-block-sorted (files) sort reverse)))
                    (separator (let ((s (formatted-param params :file-separator)))
                                 (cond ((null s) "") ((stringp s) s) (t *formatted-separator*))))
                    (no-front-matter (formatted-param params :no-front-matter))
                    (add-links (formatted-param params :add-links))
                    (pieces (loop for file in files
                                  for i from 0
                                  do (dblock-progress (/ i (max 1 (length files))))
                                  collect (or (formatted-file-contents file no-front-matter add-links) ""))))
               (formatted-final-newline
                (concatenate 'string (formatted-block-name-line params) (formatted-joined pieces separator))))))
        ((string= kind "files-as-headings")
         (if (null (formatted-param params :regexp))
             ""
             (let* ((files (remove-if-not (lambda (file) (eq (formatted-file-kind file) :org))
                                          (formatted-block-sorted (files) sort reverse)))
                    (add-links (formatted-param params :add-links))
                    (exclude-tags (formatted-param params :exclude-tags))
                    (pieces (loop for file in files
                                  for i from 0
                                  do (dblock-progress (/ i (max 1 (length files))))
                                  collect (or (formatted-heading-contents file add-links exclude-tags) ""))))
               (formatted-final-newline
                (concatenate 'string (formatted-block-name-line params) (formatted-joined pieces ""))))))
        ((string= kind "sequence")
         (let* ((sequence (let ((s (formatted-param params :sequence))) (if (stringp s) s (and s (princ-to-string s)))))
                (depth (formatted-param params :depth))
                (all (formatted-sequence-files (files :regexp nil :omit nil)))
                (family (if (or (null sequence) (string= sequence "") (string= sequence ".+"))
                            all
                            (or (formatted-sequence-family sequence all)
                                (progn (message "No file has the sequence ~a" sequence)
                                       (return-from formatted-block-text nil))))))
           (if (null family)
               (string #\Newline)
               (concatenate 'string (formatted-block-name-line params) (formatted-sequence-lines family depth)))))))))

(defparameter *formatted-block-kinds* '("links" "missing-links" "backlinks" "files" "files-as-headings" "sequence")
  "The kinds of dynamic blocks of files with formatted names: each is made by
the writer formatted-KIND, and under the name denote-KIND too.")

(dolist (kind *formatted-block-kinds*)
  (let ((kind kind))
    (setf (gethash (format nil "formatted-~a" kind) *dblock-writers*)
          (lambda (params) (formatted-block-text kind params nil))
          (gethash (format nil "denote-~a" kind) *dblock-writers*)
          (lambda (params) (formatted-block-text kind params t)))))

;;; Putting the blocks in

(defun read-formatted-block-regex (words)
  "Ask for a regular expression the names of files with formatted names are
to match, for what WORDS says is done with it, read as Tuatara's searches
read it (smart case, \\<pattern>\\[search-toggle-regex] for literal text), the files it
matches listed as it is typed; NIL, and said, when it is no regular expression."
  (let ((regex (read-formatted-files-query words #'formatted-files-matching)))
    (cond ((string= regex "") (message "No regular expression given") nil)
          ((formatted-files-scanner regex) regex)
          (t (message "~a is no regular expression" regex) nil))))

(defparameter *formatted-sort-components* '("identifier" "title" "keywords" "signature" "last-modified" "random")
  "What a block of files with formatted names can be sorted by: a part of
their names, when they last changed, or chance.")

(defun read-formatted-sort-component ()
  "Ask what a block's files are to be sorted by (*FORMATTED-SORT-COMPONENTS*),
as a word for its #+BEGIN: line."
  (make-symbol (string-upcase (completing-read "Sort the files by: " *formatted-sort-components* :require-match t))))

(defun read-formatted-sequence ()
  "Ask for one of the files with formatted names whose signature is a
sequence, listed in the order of their sequences as they are found in the
background (READ-FORMATTED-FILE); its sequence, or NIL when none was chosen."
  (flet ((sequence-of (relative) (nth-value 1 (name-parts (formatted-file-name-of (cons nil relative))))))
    (let ((chosen (read-formatted-file "The file whose sequence to list: "
                                       :keep (lambda (relative) (formatted-sequence-p (sequence-of relative)))
                                       :predicate #'string<
                                       :key (lambda (entry) (formatted-sequence-key (sequence-of (first entry))))
                                       :none "none has a sequence")))
      (and chosen (sequence-of chosen)))))

(define-command insert-formatted-links-block ()
  "Put in a dynamic block of links to the files with formatted names whose
names a regular expression matches (a block also written denote-links): asked
for, the files it matches listed as it is typed; the links labelled with the
files' signatures and titles. \\[org-dblock-update] on the block makes it
again; the parameters on its #+BEGIN: line say what more it does
\(:not-regexp, :sort-by-component, :reverse-sort, :id-only, :include-date)."
  (let ((regex (read-formatted-block-regex "links to files whose names match")))
    (when regex
      (dblock-insert "formatted-links" (list :regexp regex :not-regexp nil :excluded-dirs-regexp nil
                                             :sort-by-component nil :reverse-sort nil :id-only nil :include-date nil)))))

(define-command insert-formatted-missing-links-block ()
  "Put in a dynamic block of links to the files with formatted names whose
names a regular expression matches and that this file does not link to yet,
(a block also written denote-missing-links): the regular expression asked for,
the files it matches listed as it is typed."
  (let ((regex (read-formatted-block-regex "missing links to files whose names match")))
    (when regex
      (dblock-insert "formatted-missing-links" (list :regexp regex :not-regexp nil :excluded-dirs-regexp nil
                                                     :sort-by-component nil :reverse-sort nil :id-only nil :include-date nil)))))

(define-command insert-formatted-backlinks-block ()
  "Put in a dynamic block of links to the files with formatted names that link
to this one (a block also written denote-backlinks). With :this-heading-only t
on its #+BEGIN: line, those that link to the headline it is under, by its
CUSTOM_ID."
  (dblock-insert "formatted-backlinks" (list :excluded-dirs-regexp nil :sort-by-component nil :reverse-sort nil
                                             :id-only nil :this-heading-only nil :include-date nil)))

(define-command insert-formatted-files-block ()
  "Put in a dynamic block holding the text of the files with formatted names
whose names a regular expression matches (a block also written denote-files):
the regular expression and the order of the files asked for. Only notes are
read, Org, Markdown and text files; :no-front-matter, :file-separator and
:add-links on its #+BEGIN: line say more of how."
  (let ((regex (read-formatted-block-regex "text of files whose names match")))
    (when regex
      (dblock-insert "formatted-files" (list :regexp regex :not-regexp nil :excluded-dirs-regexp nil
                                             :sort-by-component (read-formatted-sort-component) :reverse-sort nil
                                             :no-front-matter nil :file-separator nil :add-links nil)))))

(define-command insert-formatted-files-as-headings-block ()
  "Put in a dynamic block holding the text of the Org files with formatted
names whose names a regular expression matches, each under a headline of its
title and tags, its own headlines a level down (a block also written
denote-files-as-headings): the regular expression and the order of the
files asked for."
  (let ((regex (read-formatted-block-regex "Org files whose names match, as headlines")))
    (when regex
      (dblock-insert "formatted-files-as-headings" (list :regexp regex :not-regexp nil :excluded-dirs-regexp nil
                                                         :sort-by-component (read-formatted-sort-component) :reverse-sort nil
                                                         :add-links nil :exclude-tags nil)))))

(define-command insert-formatted-sequence-block ()
  "Put in a dynamic block listing a file with a sequence for its signature and
those under it (a block also written denote-sequence): the file asked for, and
how many levels to list, its own the first."
  (let ((sequence (read-formatted-sequence)))
    (when sequence
      (let ((depth (read-whole-number "Levels to list, the file's own the first (default ~d): " 2)))
        (dblock-insert "formatted-sequence" (list :sequence sequence :depth depth))))))

(loop for (name command) in '(("formatted-links" insert-formatted-links-block)
                              ("formatted-missing-links" insert-formatted-missing-links-block)
                              ("formatted-backlinks" insert-formatted-backlinks-block)
                              ("formatted-files" insert-formatted-files-block)
                              ("formatted-files-as-headings" insert-formatted-files-as-headings-block)
                              ("formatted-sequence" insert-formatted-sequence-block))
      do (setf (gethash name *dblock-inserters*) command))
