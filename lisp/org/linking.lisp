;;;; lisp/org/linking.lisp --- Links and pictures written in Org, Markdown and LaTeX, and links stored where you are.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; org-store-link keeps a link to where point is: a headline of an Org
;;;; file (by its CUSTOM_ID, else its title), one of your notes (denote:ID),
;;;; a line of another file. insert-link (C-c C-l) asks for a link, the
;;;; stored ones and the kinds of link offered (file and denote ones chosen
;;;; from your files), and a label, and writes it as the mode writes links:
;;;; [[link][label]] in Org, [label](link) in Markdown, \href{link}{label} in
;;;; LaTeX. On an Org link, it edits it. A stored link to the Org file it is
;;;; put in becomes a link within it, as Org makes it. inline-add-picture
;;;; (C-c C-x p) asks for a picture and writes a link to it where it is, on
;;;; lines of its own with the properties a picture takes (caption, name,
;;;; width) written out to be changed in place.

(in-package #:tuatara.defaults)

(defvar *org-stored-links* '()
  "The links org-store-link kept, the latest first, each (LINK LABEL), LABEL NIL for none.")

(defparameter *org-link-kinds*
  '("file:" "denote:" "https:" "http:" "mailto:" "id:" "doi:" "formatted-match:")
  "The kinds of link insert-link offers besides the stored links. The file and
denote ones go on to ask for the file or the note.")

(defun written-path (path directory &key (home t))
  "PATH (native) as a link writes it from a file in DIRECTORY (native, ending
in /): from DIRECTORY when it is in it or under it, else whole, the home
directory written ~ unless HOME is NIL (for a program that does not read ~)."
  (cond ((and directory (uiop:string-prefix-p directory path)) (subseq path (length directory)))
        (home (abbreviated-path path))
        (t path)))

(defun org-note-title (path)
  "The title of the note at PATH: the title its front matter gives
\(NOTE-FRONT-MATTER), else the title its formatted name holds, hyphens as
spaces."
  (or (note-front-matter path)
      (let ((title (nth-value 2 (name-parts (tuatara.io:native-file-name path)))))
        (and title (substitute #\Space #\- title)))))

(defun org-link-to-here (buffer)
  "A link to where point is in BUFFER and its label, as two values: in one of
your notes, denote:ID and its title; under
an Org headline, file:PATH::#CUSTOM_ID or file:PATH::*Title and the title; in
a file not Org, file:PATH::the line point is on; NIL where there is no file."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (when file
      (let* ((path (tuatara.io:native file))
             (name (tuatara.io:native-file-name path))
             (identifier (and (identifier-at-start-p name) (name-parts name))))
        (cond
          ((typep (buffer-major-mode buffer) 'org-mode)
           (let* ((parse (org-parse-now buffer))
                  (headline (and parse (org-current-headline (org-parse-tree parse) (org-parse-point parse))))
                  (custom-id (and headline (org-headline-property headline "CUSTOM_ID")))
                  (title (and headline (org-property headline :raw-value))))
             (cond ((and identifier (null headline)) (values (format nil "denote:~a" identifier) (org-note-title path)))
                   (custom-id (values (format nil "file:~a::#~a" (abbreviated-path path) custom-id) title))
                   (headline (values (format nil "file:~a::*~a" (abbreviated-path path) title) title))
                   (t (values (format nil "file:~a" (abbreviated-path path)) nil)))))
          (identifier (values (format nil "denote:~a" identifier) (org-note-title path)))
          (t (let* ((point (tuatara.buffer:buffer-point buffer))
                    (line (string-trim *org-blanks* (tuatara.buffer:buffer-line-text
                                                     buffer (tuatara.buffer:buffer-line-start buffer point)))))
               (values (if (plusp (length line))
                           (format nil "file:~a::~a" (abbreviated-path path) line)
                           (format nil "file:~a" (abbreviated-path path)))
                       nil))))))))

(define-command org-store-link ()
  "Keep a link to where point is, for insert-link (\\[insert-link]) to
offer: under an Org headline, to it (by its
CUSTOM_ID, else its title); in one of your notes, to the note (denote:ID); in
another file, to the line point is on."
  (multiple-value-bind (link label) (org-link-to-here (current-buffer))
    (if (null link)
        (message "No link to store here: this buffer has no file")
        (progn
          (setf *org-stored-links*
                (cons (list link label) (remove link *org-stored-links* :key #'first :test #'string=)))
          (message "Stored: ~a" (or label link))))))

(defun file-link-parts (link directory)
  "The file LINK, a file: link, leads to, its native name with a relative one
taken from DIRECTORY, and what follows its :: (NIL for nothing), as two
values; NIL for a link of another kind."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^file:(.+?)(?:::(.*))?$" link)
    (when match
      (values (tuatara.io:native (tuatara.io:expand-path (aref groups 0) :directory directory)) (aref groups 1)))))

(defun org-link-within (link buffer)
  "LINK, a stored link, as it is put in BUFFER: a file link to a
headline of BUFFER's own file made a link within it (*Title, #id); another
file link written from BUFFER's directory when the file is in it or under it."
  (let ((file (tuatara.buffer:buffer-file buffer))
        (directory (buffer-directory buffer)))
    (multiple-value-bind (path search) (file-link-parts link directory)
      (cond ((null path) link)
            ((and file search (plusp (length search)) (find (char search 0) "*#")
                  (string= path (tuatara.io:native file)))
             search)
            (t (format nil "file:~a~@[::~a~]" (written-path path directory) search))))))

(defun plain-link-target (link buffer)
  "LINK as a file that is not Org writes it in BUFFER: a file: link as the
path alone, from BUFFER's directory when the file is in it or under it, else
whole (WRITTEN-PATH), what follows its :: left out; any other link as it is."
  (let* ((directory (buffer-directory buffer))
         (path (file-link-parts link directory)))
    (if path (written-path path directory :home nil) link)))

(defun read-note-link ()
  "Ask for one of your notes, as open-file-formatted lists them
(READ-FORMATTED-FILE), and return a denote: link to it and its title, as two
values; NIL when none was chosen."
  (let ((chosen (read-formatted-file "Link to note: ")))
    (when chosen
      (values (format nil "denote:~a" (name-parts (tuatara.io:native-file-name chosen)))
              (org-note-title (concatenate 'string (directory-name *formatted-file-directory*) chosen))))))

(defun read-file-link (buffer)
  "Ask for a file, as find-file asks, and return a file: link to it from BUFFER's directory."
  (let ((path (read-file-path "File link: ")))
    (and path
         (let ((directory (buffer-directory buffer)))
           (format nil "file:~a" (written-path (tuatara.io:native (tuatara.io:expand-path path :directory directory))
                                               directory))))))

(defun read-link-target (buffer)
  "Ask for a link, offering the stored links and the kinds of link (file
and denote go on to ask for the file or note). RET with nothing typed takes
the link stored last. The link and a label to offer for it, as two values;
NIL when none was given."
  (let* ((stored (loop for (link label) in *org-stored-links*
                       for rank from 0
                       collect (make-candidate link :annotation label :group "Stored links" :rank rank)))
         (kinds (mapcar (lambda (kind) (make-candidate kind :group "Kinds of link")) *org-link-kinds*))
         (chosen (completing-read "Link: " (append stored kinds)
                                  :select (lambda (shown input)
                                            (or (and (string= (string-trim " " input) "") *org-stored-links*
                                                     (position (first (first *org-stored-links*)) shown
                                                               :key #'candidate-string :test #'string=))
                                                :input))
                                  :reselect t)))
    (cond ((string= chosen "file:") (values (read-file-link buffer) nil))
          ((string= chosen "denote:") (read-note-link))
          ((string= (string-trim " " chosen) "") nil)
          (t (let ((entry (find chosen *org-stored-links* :key #'first :test #'string=)))
               (values chosen (second entry)))))))

(defun org-link-text (link label)
  "The Org link to LINK labelled LABEL: [[LINK][LABEL]], [[LINK]] when LABEL is empty, LINK escaped (ORG-LINK-ESCAPE)."
  (if (plusp (length label))
      (format nil "[[~a][~a]]" (org-link-escape link) label)
      (format nil "[[~a]]" (org-link-escape link))))

(defgeneric mode-link-text (mode buffer link label)
  (:documentation "The link to LINK labelled LABEL (empty for none) as a file of
MODE writes it, put in BUFFER."))

(defmethod mode-link-text ((mode org-mode) buffer link label)
  "[[LINK][LABEL]], a stored link to a headline of BUFFER's file made a link
within it (ORG-LINK-WITHIN)."
  (org-link-text (org-link-within link buffer) label))

(defun markdown-link-destination (target)
  "TARGET as a Markdown link's destination: between < and > when it holds a
space or a parenthesis, which would end it."
  (if (find-if (lambda (c) (find c " ()")) target) (format nil "<~a>" target) target))

(defmethod mode-link-text ((mode markdown-mode) buffer link label)
  "[LABEL](LINK), a file: link as the path alone (PLAIN-LINK-TARGET), square
brackets in LABEL written \\=\\[ and \\=\\]; the link itself its label when
LABEL is empty."
  (let ((target (plain-link-target link buffer)))
    (format nil "[~a](~a)"
            (cl-ppcre:regex-replace-all "([\\[\\]])" (if (plusp (length label)) label target) "\\\\\\1")
            (markdown-link-destination target))))

(defmethod mode-link-text ((mode latex-mode) buffer link label)
  "\\href{LINK}{LABEL}, or \\url{LINK} when LABEL is empty, a file: link as
the path alone (PLAIN-LINK-TARGET), written as the export writes addresses
\(LATEX-URL)."
  (let ((target (latex-url (plain-link-target link buffer))))
    (if (plusp (length label))
        (format nil "\\href{~a}{~a}" target label)
        (format nil "\\url{~a}" target))))

(defgeneric mode-link-label-problem (mode label)
  (:documentation "Why LABEL cannot be the label of a link in a file of MODE, or NIL.")
  (:method (mode label)
    (declare (ignore mode label))
    nil))

(defmethod mode-link-label-problem ((mode org-mode) label)
  "One holding ]] or ending in ] would end the link early (LINK-LABEL-PROBLEM)."
  (link-label-problem label))

(defmethod mode-link-label-problem ((mode latex-mode) label)
  "One whose braces do not pair would end the link early or not at all;
\\=\\{ and \\} are braces written as text."
  (let ((depth 0) (i 0))
    (loop while (and (< i (length label)) (>= depth 0))
          do (case (char label i)
               (#\\ (incf i))
               (#\{ (incf depth))
               (#\} (decf depth)))
             (incf i))
    (unless (zerop depth)
      "a label whose braces do not pair would end the link early")))

(define-command insert-link ()
  "Insert a link (\\`C-c C-l' in Org, Markdown and LaTeX files), written as
the file's mode writes links: [[LINK][LABEL]] in Org, [LABEL](LINK) in
Markdown, \\href{LINK}{LABEL} in LaTeX. It asks for the link, offering those
org-store-link (\\[org-store-link]) kept and the kinds of link (file asks for
a file, denote for one of your notes), then for its label, one offered
selected (the stored link's, the note's title) to take or type over; the text
selected becomes the label instead. A stored link to a headline of an Org file
put in that file becomes a link within it (*Title); in Markdown and LaTeX a
file link is the file's path alone. On an Org link, edit it: its link and
label offered. With \\[universal-argument], a link to a file."
  (let* ((buffer (current-buffer))
         (mode (buffer-major-mode buffer))
         (on-link (and (typep mode 'org-mode)
                       (let* ((parse (org-parse-now buffer))
                              (link (and parse (org-link-at (org-parse-tree parse) (org-parse-point parse)))))
                         (and link (eq (org-property link :format) 'bracket)
                              (list link parse))))))
    (if on-link
        (destructuring-bind (link parse) on-link
          (multiple-value-bind (answer parse places)
              (org-ask-keeping-places
               parse (list (cons (org-node-begin link) (- (org-node-end link) (org-node-post-blank link))))
               (lambda ()
                 (let ((target (read-string "Link: " :initial (org-property link :raw-link)))
                       (old-label (if (org-node-contents-begin link)
                                      (subseq (org-parse-text parse) (org-node-contents-begin link)
                                              (org-node-contents-end link))
                                      "")))
                   (list target (read-string "Label: " :initial old-label :select-initial t)))))
            (destructuring-bind (target label) answer
              (let* ((begin (first places))
                     (link (and begin (org-link-at (org-parse-tree parse) begin)))
                     (problem (link-label-problem label)))
                (cond (problem (message "Link not changed: ~a" problem))
                      ((not (and link (= (org-node-begin link) begin) (eq (org-property link :format) 'bracket)))
                       (refuse "The link is no longer there: not changed"))
                      (t (org-replace-chars parse begin (- (org-node-end link) (org-node-post-blank link))
                                            (org-link-text target label)
                                            :point (+ begin (length (org-link-text target label))))))))))
        (let ((region (selected-region buffer)))
          (multiple-value-bind (target suggested)
              (if (prefix-argument) (values (read-file-link buffer) nil) (read-link-target buffer))
            (if (null target)
                (message "No link inserted")
                (when (insert-labelled-link buffer region (or suggested "")
                                            (lambda (label) (mode-link-text mode buffer target label))
                                            :problem (lambda (label) (mode-link-label-problem mode label)))
                  (setf *org-stored-links* (remove target *org-stored-links* :key #'first :test #'string=)))))))))

;;; Pictures

(defparameter *picture-latex-width* ".9\\linewidth"
  "The width a picture is given in LaTeX when nothing else says, as the export
to LaTeX gives it, and as inline-add-picture writes it.")

(defparameter *picture-markdown-width* "100%"
  "The width inline-add-picture writes for a picture in Markdown, the whole
width of the text.")

(defun picture-label (path)
  "The name a picture at PATH is referred to by, its file's name without its
ending after fig:, each run of characters other than letters, digits, - and _
written -."
  (format nil "fig:~a" (string-trim "-" (cl-ppcre:regex-replace-all
                                          "[^A-Za-z0-9_-]+"
                                          (file-name-base-of (tuatara.io:native-file-name path)) "-"))))

(defgeneric mode-picture-lines (mode buffer path)
  (:documentation "The lines that show the picture at PATH (native) in a file of
MODE, put in BUFFER, each of the properties a picture takes written out with
its standard value, to be seen and changed in place; true as a second value
when they are to be indented as the mode indents."))

(defmethod mode-picture-lines ((mode org-mode) buffer path)
  "An empty #+CAPTION:, #+NAME: with the picture's label (PICTURE-LABEL),
#+ATTR_ORG: :width t (its own width), #+ATTR_LATEX: :width as the export
gives it, then [[file:PATH]]."
  (list "#+CAPTION:"
        (format nil "#+NAME: ~a" (picture-label path))
        "#+ATTR_ORG: :width t"
        (format nil "#+ATTR_LATEX: :width ~a" *picture-latex-width*)
        (org-link-text (format nil "file:~a" (written-path path (buffer-directory buffer))) "")))

(defmethod mode-picture-lines ((mode markdown-mode) buffer path)
  "![](PATH){width=100%}: empty alt text, then the width as Markdown programs
that read attributes take it."
  (list (format nil "![](~a){width=~a}"
                (markdown-link-destination (written-path path (buffer-directory buffer) :home nil))
                *picture-markdown-width*)))

(defmethod mode-picture-lines ((mode latex-mode) buffer path)
  "A figure placed as LaTeX places one when nothing is said (tbp), centred,
the picture as wide as the export makes it, an empty \\caption{} and a
\\label{} with the picture's label (PICTURE-LABEL). Refused for a file whose
name LaTeX cannot take (LATEX-FILE-NAME-P)."
  (let ((written (written-path path (buffer-directory buffer) :home nil)))
    (unless (latex-file-name-p written)
      (refuse "LaTeX cannot take a file named with % # { } \\ ~~ or ^: ~a" written))
    (values (list "\\begin{figure}[tbp]"
                  "\\centering"
                  (format nil "\\includegraphics[width=~a]{~a}" *picture-latex-width* written)
                  "\\caption{}"
                  (format nil "\\label{~a}" (picture-label path))
                  "\\end{figure}")
            t)))

(defun read-picture-path (buffer)
  "Ask for a picture, as find-file asks for a file, only pictures and
directories offered and the picture selected shown meanwhile; its native
name, taken from BUFFER's directory, or NIL when none was named. One that is
not there, or whose name does not say it is a picture, is refused."
  (let ((typed (read-file-path "Picture: " :preview t
                                           :keep (lambda (name) (or (directory-input-p name)
                                                                    (inline-picture-path-p name))))))
    (when typed
      (let ((path (tuatara.io:native (tuatara.io:expand-path typed :directory (buffer-directory buffer)))))
        (cond ((not (inline-picture-path-p path)) (refuse "Not a picture: ~a" (abbreviated-path path)))
              ((not (eq (tuatara.io:path-kind path) :regular)) (refuse "No picture at ~a" (abbreviated-path path)))
              (t path))))))

(define-command inline-add-picture ()
  "Ask for a picture and write a link to it on lines of its own (\\`C-c C-x p'
in Org, Markdown and LaTeX files), the picture selected shown while you
choose. The picture is linked where it is, never copied or changed. The
properties a picture takes are written out with their standard values, to be
changed in place: in Org an empty #+CAPTION:, a #+NAME:, #+ATTR_ORG: :width
and #+ATTR_LATEX: :width above [[file:PICTURE]]; in Markdown
![](PICTURE){width=100%}, the alt text between the brackets; in LaTeX a figure
with \\includegraphics[width=...]{PICTURE}, \\caption{} and \\label{}.
Pictures among the lines (\\[inline-pictures-mode]) stay as they were."
  (let* ((buffer (current-buffer))
         (path (read-picture-path buffer)))
    (when path
      (multiple-value-bind (lines indent) (mode-picture-lines (buffer-major-mode buffer) buffer path)
        (multiple-value-bind (first end) (insert-lines-alone buffer lines)
          (when indent (indent-lines buffer first end)))))))

(defun install-linking ()
  "Bind \\`C-c C-l' to insert-link and \\`C-c C-x p' to inline-add-picture in
Org, Markdown and LaTeX buffers, where they work alone."
  (dolist (mode '(org-mode markdown-mode latex-mode))
    (commands-for-mode mode "Org, Markdown and LaTeX files" 'insert-link 'inline-add-picture)
    (define-key (mode-keymap mode) "C-c C-l" 'insert-link)
    (define-key (mode-keymap mode) "C-c C-x p" 'inline-add-picture)))
