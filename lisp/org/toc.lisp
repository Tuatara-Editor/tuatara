;;;; lisp/org/toc.lisp --- A table of contents kept in an Org file, its links working.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's toc-org package keeps one (by Sergei Nosov, GPL-3.0-or-later):
;;;; the first headline tagged :TOC: holds a list of links to the file's
;;;; headlines, made again each time the file is saved. Made line for line as
;;;; toc-org makes it, so that a file kept in both editors stays the same in
;;;; each: the headlines up to the depth its tag gives (:TOC_3:), their TODO
;;;; keywords, priorities, tags and links taken out, those under COMMENT and
;;;; :noexport: left out, each a link to a GitHub anchor (#a-headline) or, with
;;;; :TOC_2_org:, to its title. Its links lead to their headlines here too. A
;;;; Markdown file has one the same way, under its first heading tagged :TOC:
;;;; (## Contents <!-- :TOC: -->), its links written [Title](#title).

(in-package #:tuatara.defaults)

(defvar *org-toc-on-save* t
  "True to make the table of contents of an Org file again each time it is
saved, under its first headline tagged :TOC:; NIL to
leave it to \\[toc-org-insert-toc].")

(defvar *org-toc-max-depth* 2
  "How deep the headlines of a table of contents go when its tag says nothing
(:TOC:): 2 for those of one and two stars. :TOC_3: says 3.")

(defvar *org-toc-style* "gh"
  "How a table of contents links to its headlines when its tag says nothing:
\"gh\" for GitHub's anchors (#a-headline, which
GitHub shows a README's headlines by), \"org\" for their titles. :TOC_2_org:
or :TOC_2_gh: says it for one table.")

(defvar *markdown-toc-on-save* t
  "True to make the table of contents of a Markdown file again each time it is
saved, under its first heading tagged :TOC: (## Contents <!-- :TOC: -->);
NIL to leave it to \\[toc-org-insert-toc].")

(defparameter *org-toc-largest* (* 8 1024 1024)
  "The largest Org or Markdown file whose table of contents is made again on
saving, in octets: a larger one is saved as it is.")

(defstruct (toc-line (:constructor make-toc-line (position text last)) (:copier nil))
  "A headline as the table of contents is made from it: where its line begins
(a character of the text), what is left of its line, and whether it is the
text's last line, with no line end after it."
  position text last)

(defparameter *toc-longest-line* 4096
  "The most characters of a headline line the table of contents reads: a longer
line is taken as its beginning, its tags past that unseen.")

(defun toc-line-tags (line)
  "The tags at the end of the headline LINE: the words
between colons of the last run of characters with no space in it, which ends
with a colon (a \" -->\" after it left out); NIL when the line ends in none."
  (let* ((trimmed (string-right-trim '(#\Space #\Tab) line))
         (trimmed (if (uiop:string-suffix-p trimmed "-->")
                      (string-right-trim '(#\Space #\Tab) (subseq trimmed 0 (- (length trimmed) 3)))
                      trimmed))
         (space (position #\Space trimmed :from-end t))
         (run (subseq trimmed (if space (1+ space) 0)))
         (first (position #\: run))
         (end (1- (length run))))
    (and first (< first end) (char= (char run end) #\:)
         (cl-ppcre:split ":" (subseq run (1+ first) end) :limit most-positive-fixnum))))

(defun toc-tag-of (line)
  "The tag of LINE that marks the table of contents, TOC, TOC_3, TOC_2_org
\(any case), as written; NIL when it has none."
  (find-if (lambda (tag) (cl-ppcre:scan "(?i)^toc([@_][0-9]([@_][a-zA-Z]+)?)?$" tag)) (toc-line-tags line)))

(defun toc-without-tags (line)
  "LINE without the tags at its end and the spaces before them, taken out
from the first colon of the last run of tag characters
\(letters, digits, _, @ and :) that ends the line with a colon."
  (let* ((end (length (string-right-trim '(#\Space) line)))
         (j end))
    (loop while (and (> j 0) (let ((c (char line (1- j)))) (or (alphanumericp c) (find c "_@:"))))
          do (decf j))
    (let ((colon (position #\: line :start j :end end)))
      (if (and colon (> end (1+ colon)) (char= (char line (1- end)) #\:))
          (string-right-trim '(#\Space) (subseq line 0 colon))
          line))))

(defun toc-headline-lines (text)
  "The lines of TEXT that are headlines (stars, then a space), as TOC-LINEs,
in order; and the lines starting with #+, as (POSITION . LINE)."
  (let ((headlines '()) (keywords '()) (n (length text)) (start 0))
    (loop while (< start n)
          do (let ((end (or (position #\Newline text :start start) n)))
              (cond ((char= (char text start) #\*)
                    (let ((stars (or (position #\* text :start start :end end :test #'char/=) end)))
                      (when (and (< stars end) (char= (char text stars) #\Space))
                        (push (make-toc-line start (subseq text start (min end (+ start *toc-longest-line*))) (= end n)) headlines))))
                   ((and (< (1+ start) end) (char= (char text start) #\#) (char= (char text (1+ start)) #\+))
                    (push (cons start (subseq text start end)) keywords)))
              (setf start (1+ end))))
    (values (nreverse headlines) (nreverse keywords))))

(defun toc-states-scanner (keywords)
  "The pattern of a TODO keyword after a headline's stars, taken out of a
table of contents: those of *ORG-STANDARD-TODO-KEYWORDS*, or the keywords of
#+TODO: lines among KEYWORDS; its group the stars and spaces before it, which
stay."
  (let ((custom (loop for (nil . line) in keywords
                      for groups = (nth-value 1 (cl-ppcre:scan-to-strings "(?i)^#\\+(TODO|SEQ_TODO|TYP_TODO):(.*)$" line))
                      when groups
                        append (remove "" (cl-ppcre:split "[ \\f\\t\\n\\r\\v|]+" (aref groups 1)) :test #'string=))))
    (cl-ppcre:create-scanner
     (format nil "^(\\*+ +)(?:~{~a +~^|~})"
             (mapcar (lambda (k) (cl-ppcre:quote-meta-chars (cl-ppcre:regex-replace-all "\\(.+?\\)" k "")))
                     (or custom (append (car *org-standard-todo-keywords*) (cdr *org-standard-todo-keywords*)))))
     :case-insensitive-mode t)))

(defun toc-stars (line)
  "The run of stars LINE begins with."
  (subseq line 0 (or (position #\* line :test #'char/=) (length line))))

(defun toc-drop-subtrees (lines keep-depth)
  "LINES without those KEEP-DEPTH, a function of a line's text, gives a depth
for, and the headlines under each: the depth is how many levels under it
stay (0 for the headline itself going too); those that stay are looked at
again."
  (let ((out '()))
    (loop while lines
          do (let* ((line (pop lines))
                    (depth (funcall keep-depth (toc-line-text line))))
               (if (null depth)
                   (push line out)
                   (let* ((under (concatenate 'string (toc-stars (toc-line-text line)) "*"))
                          (skip (concatenate 'string under (make-string (max (1- depth) 0) :initial-element #\*)))
                          (kept '()))
                     (when (plusp depth) (push line out))
                     (loop while (and lines (uiop:string-prefix-p under (toc-stars (toc-line-text (first lines)))))
                           do (let ((next (pop lines)))
                                (unless (or (zerop depth) (uiop:string-prefix-p skip (toc-stars (toc-line-text next))))
                                  (push next kept))))
                     (setf lines (append (nreverse kept) lines))))))
    (nreverse out)))

(defun toc-edit (lines scanner replacement)
  "LINES with each one's first match of SCANNER replaced by REPLACEMENT, as
cl-ppcre takes it (\\\\1 its first group)."
  (dolist (line lines lines)
    (setf (toc-line-text line) (cl-ppcre:regex-replace scanner (toc-line-text line) replacement))))

(defun toc-gh-anchor (heading taken)
  "HEADING as a GitHub anchor: spaces made dashes, in
lower case, all but letters, digits, _ and - left out, after a #; -1, -2 and
on added until it is none of TAKEN (a hash table), which it is put in."
  (let* ((base (concatenate 'string "#"
                            (remove-if-not (lambda (c) (or (alphanumericp c) (find c "_-")))
                                           (string-downcase (substitute #\- #\Space heading)))))
         (anchor base))
    (loop for i from 1 while (gethash anchor taken) do (setf anchor (format nil "~a-~d" base i)))
    (setf (gethash anchor taken) t)
    anchor))

(defun toc-entries (text &key markdown)
  "The table of contents of the Org TEXT, when a headline
is tagged :TOC:: five values, its entries (each the line written, the target
of its link and the character its headline begins at), where the tagged
headline begins, what goes before and after them (a quote block's lines with
:QUOTE:), and the tag's match. NIL when no headline is tagged. With MARKDOWN,
TEXT is a Markdown file as TOC-MARKDOWN-VIEW shows it, and the entries are
written as Markdown links, the quote a fenced block."
  (multiple-value-bind (lines keywords) (toc-headline-lines text)
    (let* ((tagged (find-if (lambda (line) (toc-tag-of (toc-line-text line))) lines)))
      (when tagged
        (let* ((found (toc-tag-of (toc-line-text tagged)))
               (tag (and (> (length found) 3) (subseq found 3)))
               (depth (if tag (digit-char-p (char tag 1)) *org-toc-max-depth*))
               (style (string-downcase (if (and tag (>= (length tag) 4)) (subseq tag 3) *org-toc-style*)))
               (quote (member "quote" (toc-line-tags (toc-line-text tagged)) :test #'string-equal))
               (lines (remove tagged lines :count 1))
                 (leave-states (some (lambda (k) (cl-ppcre:scan "(?i)^#\\+OPTIONS:.* todo:t[ \\n]"
                                                                (format nil "~a~%" (cdr k))))
                                     keywords)))
            (unless leave-states (toc-edit lines (toc-states-scanner keywords) "\\1"))
            (setf lines (toc-drop-subtrees lines (lambda (text) (and (cl-ppcre:scan "^\\*+ +COMMENT +" text) 0))))
            (setf lines (toc-drop-subtrees lines (lambda (text)
                                                   (let ((tag (find-if (lambda (tag) (cl-ppcre:scan "(?i)^noexport([@_][0-9])?$" tag))
                                                                       (toc-line-tags text))))
                                                     (and tag (if (> (length tag) 8) (digit-char-p (char tag 9)) 0))))))
            (toc-edit lines (cl-ppcre:create-scanner "^(\\*+ +)\\[#.\\] +") "\\1")
            (dolist (line lines)
              (setf (toc-line-text line) (org-links-as-labels (toc-without-tags (toc-line-text line)))))
            (setf lines (remove-if (lambda (line) (> (length (toc-stars (toc-line-text line))) depth)) lines))
            (let ((taken (make-hash-table :test 'equal)))
              (values
               (loop for line in lines
                     for text = (toc-line-text line)
                     for stars = (length (toc-stars text))
                     for spaces = (or (position #\Space text :start stars :test #'char/=) (length text))
                     for heading = (string-right-trim '(#\Space #\Tab) (subseq text spaces))
                     for visible = (cl-ppcre:regex-replace-all (load-time-value (cl-ppcre:create-scanner (format nil " *~a *" *org-statistics-cookie-re*)))
                                                                 heading "")
                     for target = (if (string= style "org") visible (toc-gh-anchor heading taken))
                     collect (list (if markdown
                                       (format nil "~a-~a[~a](~a)" (make-string (* 2 (1- stars)) :initial-element #\Space)
                                               (subseq text stars spaces) visible target)
                                       (format nil "~a-~a[[~a][~a]]" (make-string (* 2 (1- stars)) :initial-element #\Space)
                                               (subseq text stars spaces) target visible))
                                   target (toc-line-position line) (toc-line-last line)))
               (toc-line-position tagged)
               (cond ((not quote) "") (markdown (format nil "```~%")) (t (format nil "#+BEGIN_QUOTE~%")))
               (cond ((not quote) "") (markdown (format nil "```~%")) (t (format nil "#+END_QUOTE~%")))
               style)))))))

(defun toc-markdown-view (text)
  "The Markdown TEXT as it is read for its table of contents, each
character where it was: its fenced code blocks (from a line beginning ``` to
the next) made spaces, its headings' leading #s made stars, the #s closing a
line made spaces, and <-- and --> around a heading's :TOC: tag spaces."
  (let ((view (copy-seq text)) (n (length text)) (start 0) (fence nil))
    (flet ((blank (from to)
             (loop for i from from below to
                   unless (char= (char view i) #\Newline) do (setf (char view i) #\Space))))
      ;; fenced code first, so that a # in it is no heading
      (loop while (< start n)
            do (let ((end (or (position #\Newline text :start start) n)))
                 (when (and (<= (+ start 3) end) (string= "```" text :start2 start :end2 (+ start 3)))
                   (if fence
                       (progn (blank fence end) (setf fence nil))
                       (setf fence start)))
                 (setf start (1+ end))))
      (setf start 0)
      (loop while (< start n)
            do (let ((end (or (position #\Newline view :start start) n)))
                 (let ((hashes (or (position #\# view :start start :end end :test #'char/=) end)))
                   (when (and (> hashes start) (< hashes end) (char= (char view hashes) #\Space))
                     (fill view #\* :start start :end hashes)))
                 ;; spaces and #s closing the line
                 (let ((last (position #\# view :start start :end end :test #'char/= :from-end t)))
                   (when (and last (< (1+ last) end) (char= (char view last) #\Space))
                     (let ((spaces (or (position #\Space view :start start :end (1+ last) :test #'char/= :from-end t) (1- start))))
                       (blank (1+ spaces) end))))
                 (when (and (< start end) (char= (char view start) #\*))
                   (multiple-value-bind (match-start match-end starts ends)
                       (cl-ppcre:scan "(?i)^(\\*.*)(<-- )(:toc[^ ]*:)( -->)$" view :start start :end end)
                     (declare (ignore match-end))
                     (when match-start
                       (blank (aref starts 1) (aref ends 1))
                       (blank (aref starts 3) (aref ends 3)))))
                 (setf start (1+ end)))))
    view))

(defun toc-text (entries before after)
  "The table of contents ENTRIES make, BEFORE and AFTER it, as written in the
file: each a line, but for the text's last line when it had no line end."
  (with-output-to-string (out)
    (write-string before out)
    (dolist (entry entries)
      (write-string (first entry) out)
      (unless (fourth entry) (terpri out)))
    (write-string after out)))

(defun toc-next-star-line (text from &optional (mark #\*))
  "Where the first line of TEXT beginning with MARK, a star (a # in
Markdown), at or after the character FROM (a line's start) begins, or NIL."
  (let ((n (length text)) (p from))
    (loop while (< p n)
          do (when (char= (char text p) mark) (return p))
             (setf p (1+ (or (position #\Newline text :start p) n))))))

(defun toc-place (text headline &optional (mark #\*))
  "Where the table of contents goes in TEXT under the headline beginning at the
character HEADLINE, three values: the start and end of the
text it replaces, from the line after the headline (past its drawers and the
blank lines after each) to the end of the line before the next line beginning
with MARK, a star (a # in Markdown); what goes before it, a line end when the
headline is the last line and has none; and true when a line end goes after
it, the next headline being right under the drawers."
  (let* ((n (length text))
         (line-end (or (position #\Newline text :start headline) n))
         (lead (if (= line-end n) (string #\Newline) ""))
         (point (if (= line-end n) n (1+ line-end))))
    (when (plusp (length lead))
      (return-from toc-place (values n n lead nil)))
    ;; past each drawer line (:PROPERTIES:, :END:) ending before the next line with a star, and the space after it
    (let* ((star (toc-next-star-line text point mark))
           (bound (if star (1+ star) n)))
      (loop
        (let ((drawer (let ((p (if (= point (string-line-start text point))
                                   point
                                   (1+ (or (position #\Newline text :start point) (1- n))))))
                        (loop while (< p n)
                              do (let ((e (or (position #\Newline text :start p) n)))
                                   (when (> e bound) (return nil))
                                   (when (cl-ppcre:scan "^[ \\t]*:(?:\\w|[-_])+:[ \\t]*$" text :start p :end e) (return e))
                                   (setf p (1+ e)))))))
          (unless drawer (return))
          (setf point (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))) text :start drawer) n)))))
    (setf point (string-line-start text point))
    (if (and (< point n) (char= (char text point) mark))
        (values point point "" t)
        (let ((star (toc-next-star-line text point mark)))
          (values point (if star (1- star) n) "" nil)))))

(defun toc-change (text &key markdown)
  "What making the table of contents of the Org TEXT again changes: four
values, the start and end of the characters replaced, what
replaces them, and how many headlines it lists; :SAME when it is as it should
be, :STYLE (and the style) for a tag naming no style it knows; NIL when no
headline is tagged :TOC:. With MARKDOWN, of the Markdown TEXT, its headings
written with #s."
  (multiple-value-bind (entries headline before after style)
      (toc-entries (if markdown (toc-markdown-view text) text) :markdown markdown)
    (cond ((null headline) nil)
          ((not (member style '("gh" "org") :test #'string=)) (values :style style))
          (t (let ((new (toc-text entries before after)))
               (multiple-value-bind (start end lead open) (toc-place text headline (if markdown #\# #\*))
                 (if (and (not open) (string= lead "") (string= (subseq text start end) new))
                     :same
                     (values start end (concatenate 'string lead new (if open (string #\Newline) "")) (length entries)))))))))

(defun toc-markdown-p (buffer)
  "True when BUFFER is in Markdown mode, whose table of contents is written as Markdown."
  (typep (buffer-major-mode buffer) 'markdown-mode))

(defun toc-update (buffer &key quiet)
  "Make BUFFER's table of contents again, as on saving; true when
there is one. A table as it should be already is left alone. QUIET says
nothing of it. The text is read as it is, nothing parsed, and looked at no
further when no :TOC appears in it. A buffer in Markdown mode has it written
as Markdown."
  (let ((text (org-buffer-text buffer)))
    (multiple-value-bind (start end replacement count)
        (if (cl-ppcre:scan "(?i):toc" text) (toc-change text :markdown (toc-markdown-p buffer)) nil)
      (case start
        ((nil) nil)
        (:same (unless quiet (message "The table of contents is up to date")) t)
        (:style (message "The table of contents is not made: no style ~s (gh or org)" end) t)
        (t (let ((p (org-point-char buffer)))
             (org-replace-chars buffer start end replacement
                                :point (cond ((<= p start) p)
                                             ((>= p end) (+ p (- (length replacement) (- end start))))
                                             (t start))))
           (unless quiet (message "Table of contents made: ~:d headline~:p" count))
           t)))))

(define-command toc-org-insert-toc ()
  "Make the table of contents of the current Org file again, under its first
headline tagged :TOC: (as saving does, with
*org-toc-on-save*): a list of links to its headlines, two levels deep unless
the tag says how deep (:TOC_3:), to GitHub's anchors unless it says :TOC_2_org:
for their titles; :TOC:QUOTE: puts it in a quote block. Headlines tagged
:noexport: (or :noexport_1: for those under them), and those under a COMMENT
headline, are left out; TODO keywords, priorities, tags and links are taken
out of the titles. \\`RET' or \\[org-open-at-point] on a link goes to its headline.
In a Markdown file the same, under its first heading tagged :TOC: (## Contents
<!-- :TOC: -->), its links [Title](#title), in a fenced block with :QUOTE:,
saving making it again with *markdown-toc-on-save*."
  (let ((buffer (current-buffer)))
    (check-writable buffer)
    (unless (toc-update buffer)
      (message "No headline is tagged :TOC:"))))

(defun toc-before-save (buffer)
  "Before BUFFER is saved: when it is an Org file of no more than
*ORG-TOC-LARGEST* octets with a headline tagged :TOC: and *ORG-TOC-ON-SAVE*
is true, or such a Markdown file and *MARKDOWN-TOC-ON-SAVE*, make its table of
contents again."
  (when (and (or (and *org-toc-on-save* (typep (buffer-major-mode buffer) 'org-mode))
                 (and *markdown-toc-on-save* (toc-markdown-p buffer)))
             (<= (tuatara.buffer:buffer-size buffer) *org-toc-largest*)
             (not (tuatara.buffer:buffer-local buffer :read-only)))
    (let ((*current-buffer* buffer))
      (toc-update buffer :quiet t))))

(pushnew 'toc-before-save *before-save-functions*)

(defun toc-target-position (text target &key markdown)
  "The character of TEXT where the headline begins that the table of
contents's link TARGET (#an-anchor, or a title) leads to, or NIL; with
MARKDOWN, of the Markdown TEXT."
  (multiple-value-bind (entries headline) (toc-entries (if markdown (toc-markdown-view text) text) :markdown markdown)
    (when headline
      (third (find target entries :key #'second :test #'string=)))))

;;; Links in Markdown

(defparameter *markdown-link-re*
  "(!)?\\[([^\\]^][^\\]]*|)\\]\\(([^)]*?)(?:\\s+(\"[^\"]*\"))?\\)"
  "A Markdown link, [label](target \"title\"), or a picture's, ![label](target):
the label the second group, the target the third.")

(defun markdown-link-at (text position)
  "The Markdown link of TEXT holding the character POSITION, as two values: its
label and its target; NIL when there is none there. Only the line is looked
at, at most *TOC-LONGEST-LINE* characters of it."
  (let* ((start (string-line-start text position))
         (end (min (string-line-end text position) (+ start *toc-longest-line*))))
    (cl-ppcre:do-scans (s e starts ends *markdown-link-re* text nil :start start :end end)
      (when (and (<= s position) (< position e))
        (return-from markdown-link-at
          (values (subseq text (aref starts 1) (aref ends 1)) (subseq text (aref starts 2) (aref ends 2))))))
    nil))

(defun markdown-heading-named (text label)
  "The character of the Markdown TEXT where the first heading whose title is
LABEL begins (#s, a space, then LABEL); NIL when none is."
  (let ((view (toc-markdown-view text)) (start 0) (n (length text)))
    (loop while (< start n)
          do (let ((end (or (position #\Newline view :start start) n)))
               (when (and (< start end) (char= (char view start) #\*))
                 (let ((title (string-left-trim "* " (subseq view start end))))
                   (when (uiop:string-prefix-p label title) (return start))))
               (setf start (1+ end))))))

(defun markdown-follow-link (buffer)
  "Follow the Markdown link point is on in BUFFER: to the heading of this file
#an-anchor (a table of contents's) leads to, else one titled as its label; to
a web or mail address in the desktop's program, after asking; to a file,
visited. True when point is on a link."
  (let* ((point (tuatara.buffer:buffer-point buffer))
         (start (tuatara.buffer:buffer-line-start buffer point))
         (line (org-line-head buffer point (* 4 *toc-longest-line*)))
         (column (and (<= (- point start) (tuatara.text:string-octet-length line))
                      (length (tuatara.buffer:buffer-string buffer :start start :end point)))))
    (multiple-value-bind (label target) (and column (markdown-link-at line column))
      (when target
        (cond ((uiop:string-prefix-p "#" target)
               (let* ((text (org-buffer-text buffer))
                      (at (or (toc-target-position text target :markdown t) (markdown-heading-named text label))))
                 (if at
                     (org-goto-char buffer at)
                     (message "No heading here for ~a" target))))
              ((open-address-p target) (open-address target))
              ((string= target "") (message "The link leads nowhere"))
              (t (visit-path (tuatara.io:native (tuatara.io:expand-path (subseq target 0 (or (position #\# target) (length target)))
                                                                        :directory (buffer-directory buffer))))))
        t))))

(define-command markdown-follow-link-at-point ()
  "Follow the Markdown link point is on (\\`C-c C-o', and \\`RET' in NORMAL state): to
the heading a table of contents's link (#an-anchor) leads to, a web or mail
address in the desktop's program after asking, or a file, visited."
  (unless (markdown-follow-link (current-buffer))
    (message "No link here")))

(defun markdown-follow-link-for-return (buffer)
  "For \\`RET' in NORMAL state: follow the Markdown link point is on, when
BUFFER is in Markdown mode; true when it did."
  (and (toc-markdown-p buffer) (markdown-follow-link buffer)))
