;;;; lisp/lsp/org-blocks.lisp --- A note's source blocks seen by their languages' servers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In an Org note, the code of each language's source blocks is a document
;;;; of its own for that language's servers: the note's lines, those of no
;;;; such block left empty and a block's own indentation and Org's escaping
;;;; commas left out, so that a place in it is the same line of the note. What
;;;; a server finds wrong in the code is underlined in the note; completion,
;;;; definitions, descriptions, renames and fixes work within the blocks as in
;;;; a file of the language. All of one language's blocks are one document, so
;;;; that a name defined in one is known in the next. Blocks written to one file
;;;; of the project (:tangle, as Org's header arguments give it) are seen as
;;;; that file; others by the note's name with the language's ending
;;;; (notes.org.py), a file that is never written. As the note is changed, a
;;;; server is sent only what changed in that code: the blocks a change
;;;; reaches are read again, the others moved with the lines before them
;;;; (ORG-BLOCKS-SHADOW-CHANGES).

(in-package #:tuatara.lsp)

(defun org-source-block-edge (line open)
  "What LINE, a line of a note's text, is to its source blocks: with OPEN true
\(a block begun before it), true when it ends that block; else the name of
the language when it begins one; NIL when it does neither."
  (if open
      (and (cl-ppcre:scan "(?i)^[ \\t]*#\\+end_src\\b" line) t)
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "(?i)^[ \\t]*#\\+begin_src[ \\t]+([^ \\t]+)" line)
        (and match (aref groups 0)))))

(defun org-pair-source-blocks (lines)
  "The source blocks of a note whose lines that may begin or end one are
LINES, each (NUMBER . TEXT) in order, as ORG-SOURCE-BLOCKS gives them."
  (let ((blocks '()) (open nil))
    (loop for (n . line) in lines
          do (if open
                 (when (org-source-block-edge line t)
                   (push (list (first open) (second open) n) blocks)
                   (setf open nil))
                 (let ((name (org-source-block-edge line nil)))
                   (when name
                     (setf open (list name (1+ n)))))))
    (nreverse blocks)))

(defun org-source-blocks (text)
  "The source blocks of TEXT, an Org note, in order, each (NAME FIRST LAST),
NAME the language it is given, FIRST and LAST the 0-based lines of its code
\(LAST past it), line for line, as the note's documents need them."
  (org-pair-source-blocks (loop for line in (uiop:split-string text :separator '(#\Newline))
                                for n from 0
                                collect (cons n line))))

(defun org-code-line-reader (buffer line)
  "A function giving, each time it is called, the next line of BUFFER, a note,
from its 0-based LINE on, that may begin or end a source block, as its number
and its text (two values), NIL past the last: those its parts keep as lines
that may say where blocks are (ORG-BUFFER-PARTS). Complexity: O(log of the
text) a line."
  (let* ((parts (org-buffer-parts buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (from (tuatara.text:line->byte rope (min line (tuatara.text:rope-lines rope))))
         (index (org-part-index parts from))
         (offsets (and (< index (org-parts-count parts)) (org-part-code-lines (org-part-at parts index)))))
    (lambda ()
      (loop
        (cond (offsets
               (let ((at (+ (org-part-start parts index) (pop offsets))))
                 (when (>= at from)
                   (return (values (tuatara.text:byte->line rope at)
                                   (tuatara.text:rope-string rope :start at
                                                             :end (tuatara.text:rope-line-end rope at)))))))
              ((< (incf index) (org-parts-count parts))
               (setf offsets (org-part-code-lines (org-part-at parts index))))
              (t (return nil)))))))

(defun org-note-source-blocks (buffer)
  "The source blocks of BUFFER, a note, as ORG-SOURCE-BLOCKS gives them of its
text, found among the lines that may begin or end one (ORG-CODE-LINE-READER).
Complexity: O(parts + those lines x log of the text)."
  (let ((next (org-code-line-reader buffer 0)))
    (org-pair-source-blocks (loop for (n line) = (multiple-value-list (funcall next))
                                  while n collect (cons n line)))))

(defun org-blocks-tangle-values (buffer language)
  "The :tangle values of LANGUAGE's source blocks of BUFFER, a note, as Org
merges its header arguments (ORG-BLOCK-HEADER-ARGUMENTS), each with the
block's language as written, from the note's last parse; NIL before there is
one."
  (let ((tree (let ((parse (org-buffer-parse buffer))) (and parse (org-parse-tree parse))))
        (found '()))
    (labels ((walk (node)
               (when (and (eq (org-node-type node) 'src-block) (not (org-commented-p node))
                          (eq (org-src-language (or (org-property node :language) "")) language))
                 (push (cons (cdr (assoc "tangle" (org-block-header-arguments tree node) :test #'string=))
                             (org-property node :language))
                       found))
               (mapc #'walk (org-node-children node))))
      (when tree (walk tree)))
    found))

(define-record (code-block (:constructor make-code-block (first last octets &optional name)) (:copier nil))
  "A stretch of a note's lines that is the code of a source block: the
0-based lines FIRST below LAST; OCTETS, a vector of the octets left out at
the start of each line in what a document shows of it, NIL for a block of
another language, or one read again whose language is not looked at yet;
NAME, for one read again, the language as its header names it."
  (first nil :documentation "The first line of CODE-BLOCK, counted from 0, the line of its code below its header.")
  (last nil :documentation "The last line of CODE-BLOCK, counted from 0, below the last line of its code.")
  (octets nil
          :documentation "A vector of the octets left out at the start of each line of CODE-BLOCK in what a document
shows of it; NIL for a block of another language or one read again whose language is not looked at yet.")
  (name nil :documentation "The language of CODE-BLOCK as its header names it, for a block read again."))

(define-record (line-stretch (:constructor make-line-stretch (old-first old-last new-first new-last)))
  "Lines of a note changed since its text was last sent: OLD-FIRST to
OLD-LAST of the text then, NEW-FIRST to NEW-LAST of the text now, each the
last of them in it."
  (old-first nil :documentation "The first line of LINE-STRETCH in the text of the note when it was last sent.")
  (old-last nil :documentation "The last line of LINE-STRETCH in the text of the note when it was last sent.")
  (new-first nil :documentation "The first line of LINE-STRETCH in the text of the note now.")
  (new-last nil :documentation "The last line of LINE-STRETCH in the text of the note now."))

(setf (documentation 'copy-line-stretch 'function) "A LINE-STRETCH like STRETCH, apart from it.")

(defun stretch-first (stretch side)
  "The first line of STRETCH in the text then (SIDE :OLD) or now (:NEW)."
  (if (eq side :old) (line-stretch-old-first stretch) (line-stretch-new-first stretch)))

(defun stretch-last (stretch side)
  "The last line of STRETCH in the text then (SIDE :OLD) or now (:NEW)."
  (if (eq side :old) (line-stretch-old-last stretch) (line-stretch-new-last stretch)))

(defun org-block-shown (rope first last)
  "The code of the block of ROPE's lines FIRST below LAST as its document
shows it, its indentation and Org's escaping commas left out: a list of the
text of its lines, and a vector of the octets left out at the start of each."
  (let* ((lines (loop for n from first below last collect (rope-line-text rope n)))
         (indent (loop for line in lines
                       unless (zerop (length (string-trim '(#\Space #\Tab) line)))
                       minimize (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line) 0)))
         (skipped (make-array (length lines))))
    (values (loop for line in lines
                  for n from 0
                  for cut = (min (or indent 0) (length line))
                  ;; Org's comma before a line that would read as a headline or a keyword
                  for comma = (let ((rest (subseq line cut)))
                                (if (or (uiop:string-prefix-p ",*" rest) (uiop:string-prefix-p ",#+" rest)) 1 0))
                  do (setf (svref skipped n) (tuatara.text:string-octet-length line :end (+ cut comma)))
                  collect (subseq line (+ cut comma)))
            skipped)))

(defun write-code-lines (lines first at out)
  "Write LINES, the text of lines FIRST on, to OUT, where line AT was the last
written to, the lines between them empty; return the number of the last."
  (loop for line in lines
        for n from first
        do (loop repeat (- n at) do (write-char #\Newline out))
        (write-string line out)
        (setf at n))
  at)

(defun org-blocks-shadow (buffer language &optional doc stretches)
  "The code of LANGUAGE's source blocks of BUFFER, a note, line for line with
it, and the stretches of its lines that are code, CODE-BLOCKs, their OCTETS
the octets left out at the start of each line of LANGUAGE's blocks, NIL for
another's: as ORG-BLOCK-DOC-SPECS's documents show it. Given DOC, the
document as last sent, and STRETCHES, what changed since, what changed in
that code instead (ORG-BLOCKS-SHADOW-CHANGES). Complexity: O(blocks, their
code and the lines of the note)."
  (if doc
      (org-blocks-shadow-changes buffer language doc stretches)
      (let* ((rope (tuatara.buffer:buffer-text buffer))
             (lines (1+ (tuatara.text:rope-lines rope)))
             (at 0) (kept '()))
        (values (with-output-to-string (out)
                  (loop for (name first last) in (org-note-source-blocks buffer)
                        do (if (eq (org-src-language name) language)
                               (multiple-value-bind (shown skipped) (org-block-shown rope first last)
                                 (setf at (write-code-lines shown first at out))
                                 (push (make-code-block first last skipped) kept))
                               (push (make-code-block first last nil) kept)))
                  (loop repeat (- lines 1 at) do (write-char #\Newline out)))
                (coerce (nreverse kept) 'simple-vector)))))

(defun stretches-lines (stretches before after)
  "STRETCHES, as CHANGED-STRETCHES gives them, as the lines they are on: a
list in order of LINE-STRETCH, of BEFORE, the text then, and of AFTER, the
text now; stretches on a line in common made one. A stretch just after a
carriage return takes in its line too: what follows the return decides
whether it ends a line."
  (let ((lines '()))
    (flet ((first-line (rope start)
             (tuatara.text:byte->line rope (if (and (plusp start) (= 13 (tuatara.text:rope-octet rope (1- start))))
                                               (1- start)
                                               start))))
      (loop for (old-start old-end new-start new-end) in stretches
            do (let ((line (make-line-stretch (first-line before old-start) (tuatara.text:byte->line before old-end)
                                              (first-line after new-start) (tuatara.text:byte->line after new-end)))
                     (last (first lines)))
                 (if (and last (<= (line-stretch-old-first line) (line-stretch-old-last last)))
                     (setf (line-stretch-old-last last) (line-stretch-old-last line)
                           (line-stretch-new-last last) (line-stretch-new-last line))
                     (push line lines)))))
    (nreverse lines)))

(defun reading-start (old i line)
  "The line of the note, as it was, to read its blocks again from for a change
beginning at LINE, OLD being its blocks then and I the first of them not yet
kept: the line beginning the block LINE is in, else LINE itself, or after the
last block when it is past them all."
  (let* ((count (length old))
         (k (+ i (tuatara.runtime:bisect (mid 0 (- count i)) (< (code-block-last (svref old (+ i mid))) line)))))
    (cond ((and (< k count) (<= (1- (code-block-first (svref old k))) line)) (1- (code-block-first (svref old k))))
          ((< k count) line)
          ((plusp count) (1+ (code-block-last (svref old (1- count)))))
          (t 0))))

(defun org-blocks-read-again (buffer old lines)
  "The source blocks of BUFFER, a note, now, from OLD, those of its text as
last sent (CODE-BLOCKs), and LINES, the lines changed since (STRETCHES-LINES).
The result is a vector in order of OLD's blocks moved by the lines added and
taken away before them, and of those read again, which have a NAME. Only the
lines that may begin or end a block are read, from the block a change begins
in until a block begins where one began before. Complexity: O(blocks, and
the lines read again)."
  (let* ((count (length old))
         (out (make-array count :fill-pointer 0 :adjustable t))
         (i 0) (shift 0))
    (labels ((keep-before (line)
               ;; OLD's blocks ending before LINE of the text then, moved by SHIFT
               (loop while (and (< i count) (< (code-block-last (svref old i)) line))
                     do (let ((block (svref old i)))
                          (vector-push-extend (if (zerop shift)
                                                  block
                                                  (make-code-block (+ (code-block-first block) shift)
                                                                   (+ (code-block-last block) shift)
                                                                   (code-block-octets block)))
                                              out))
                     (incf i)))
             (old-block-at (line)
               ;; the index of OLD's block beginning on the line after LINE of the text then, or NIL
               (let ((j (+ i (tuatara.runtime:bisect (mid 0 (- count i))
                                                     (< (code-block-first (svref old (+ i mid))) (1+ line))))))
                 (and (< j count) (= (code-block-first (svref old j)) (1+ line)) j)))
             (read-from (start covered)
               ;; the blocks from START now, the changes met taken in, until one begins past them
               ;; where one began before (the rest kept as it was) or the end is read
               (let ((next (org-code-line-reader buffer start)) (open nil))
                 (loop
                   (multiple-value-bind (n text) (funcall next)
                     (unless n
                       (setf i count lines '())
                       (return))
                     (loop while (and lines (<= (line-stretch-new-first (first lines)) n))
                           do (let ((line (pop lines)))
                                (setf covered (line-stretch-new-last line)
                                      shift (- (line-stretch-new-last line) (line-stretch-old-last line)))))
                     (if open
                         (when (org-source-block-edge text t)
                           (vector-push-extend (make-code-block (second open) n nil (first open)) out)
                           (setf open nil))
                         (let ((name (org-source-block-edge text nil)))
                           (when name
                             (let ((again (and (> n covered) (old-block-at (- n shift)))))
                               (when again
                                 (setf i again)
                                 (return)))
                             (setf open (list name (1+ n)))))))))))
      (loop while lines
            do (let* ((line (pop lines))
                      (start (reading-start old i (line-stretch-old-first line))))
                 (keep-before start)
                 (let ((from (+ start shift)))
                   (setf shift (- (line-stretch-new-last line) (line-stretch-old-last line)))
                   (read-from from (line-stretch-new-last line)))))
      (keep-before most-positive-fixnum))
    out))

(defun stretch-holding (changed line side)
  "The lines of CHANGED, LINE-STRETCHes in order, that hold LINE of the text
then (SIDE :OLD) or now (:NEW), a new LINE-STRETCH: those whole when LINE is
among them, else LINE and where it is in the other text."
  (let* ((at (1- (tuatara.runtime:bisect (mid 0 (length changed)) (<= (stretch-first (svref changed mid) side) line))))
         (span (and (>= at 0) (svref changed at))))
    (cond ((null span) (make-line-stretch line line line line))
          ((<= line (stretch-last span side)) (copy-line-stretch span))
          (t (let ((other (+ line (- (stretch-last span (if (eq side :old) :new :old)) (stretch-last span side)))))
               (if (eq side :old)
                   (make-line-stretch line line other other)
                   (make-line-stretch other other line line)))))))

(defun stretch-between (changed begin end side)
  "The lines from BEGIN to END of the text then (SIDE :OLD) or now (:NEW), and
those of CHANGED they reach into, as a LINE-STRETCH (STRETCH-HOLDING)."
  (let ((low (stretch-holding changed begin side))
        (high (stretch-holding changed end side)))
    (make-line-stretch (line-stretch-old-first low) (line-stretch-old-last high)
                       (line-stretch-new-first low) (line-stretch-new-last high))))

(defun widen-over-blocks (changed line blocks side)
  "Grow LINE, a LINE-STRETCH, over the blocks of BLOCKS, CODE-BLOCKs of the
text then (SIDE :OLD) or now (:NEW), that reach into it, as CHANGED has their
lines; true when it grew."
  (let* ((low (stretch-first line side))
         (high (stretch-last line side))
         (at (tuatara.runtime:bisect (mid 0 (length blocks)) (< (code-block-last (svref blocks mid)) low)))
         (grown nil))
    (loop for i from at below (length blocks)
          for block = (svref blocks i)
          while (<= (1- (code-block-first block)) high)
          do (let ((span (stretch-between changed (1- (code-block-first block)) (code-block-last block) side)))
               (when (or (< (line-stretch-old-first span) (line-stretch-old-first line))
                         (> (line-stretch-old-last span) (line-stretch-old-last line)))
                 (setf (line-stretch-old-first line) (min (line-stretch-old-first line) (line-stretch-old-first span))
                       (line-stretch-new-first line) (min (line-stretch-new-first line) (line-stretch-new-first span))
                       (line-stretch-old-last line) (max (line-stretch-old-last line) (line-stretch-old-last span))
                       (line-stretch-new-last line) (max (line-stretch-new-last line) (line-stretch-new-last span))
                       grown t))))
    grown))

(defun merged-stretches (lines)
  "LINES, LINE-STRETCHes, in order, those that meet made one."
  (loop with out = '()
        for line in (sort lines #'< :key #'line-stretch-old-first)
        for last = (first out)
        do (if (and last (<= (line-stretch-old-first line) (line-stretch-old-last last)))
               (setf (line-stretch-old-last last) (max (line-stretch-old-last last) (line-stretch-old-last line))
                     (line-stretch-new-last last) (max (line-stretch-new-last last) (line-stretch-new-last line)))
               (push line out))
        finally (return (nreverse out))))

(defun widen-to-blocks (lines old new read-again)
  "LINES, as STRETCHES-LINES gives them, with the lines of READ-AGAIN, blocks
of NEW read again (ORG-BLOCKS-READ-AGAIN). Each is widened until it holds
whole every block of OLD, the blocks as last sent, and of NEW, those now, that
it reaches into: from the line beginning one to the line ending it, then and
now alike. Those that come to meet are made one."
  (let* ((changed (coerce lines 'simple-vector))
         (lines (merged-stretches
                 (append (mapcar #'copy-line-stretch lines)
                         (loop for block in read-again
                               collect (stretch-between changed (1- (code-block-first block)) (code-block-last block)
                                                        :new))))))
    (loop
      (let ((grown nil))
        (dolist (line lines)
          (when (widen-over-blocks changed line old :old) (setf grown t))
          (when (widen-over-blocks changed line new :new) (setf grown t)))
        (let ((count (length lines)))
          (setf lines (merged-stretches lines))
          (unless (or grown (/= count (length lines)))
            (return lines)))))))

(defun org-blocks-shadow-changes (buffer language doc stretches)
  "What changed in the code of LANGUAGE's source blocks of BUFFER, a note,
since DOC, its document, was last sent, STRETCHES the stretches of the note
changed since (CHANGED-STRETCHES): as DOC-CHANGES gives them, and the
stretches of its lines that are code now, as ORG-BLOCKS-SHADOW gives them.
Only the lines changed are read again, with every block they reach into
whole (ORG-BLOCKS-READ-AGAIN, WIDEN-TO-BLOCKS). Complexity: O(blocks, and
the lines of the blocks changed)."
  (let* ((before (lsp-doc-sent-rope doc))
         (after (tuatara.buffer:buffer-text buffer))
         (shown (lsp-doc-rope doc))
         (old (lsp-doc-skipped doc))
         (spans (stretches-lines stretches before after))
         (new (coerce (org-blocks-read-again buffer old spans) 'simple-vector))
         (lines (widen-to-blocks spans old new (loop for block across new when (code-block-name block) collect block)))
         (ours (make-hash-table :test 'equal))
         (changes '()))
    (flet ((ours-p (block)
             (let ((name (code-block-name block)))
               (if (null name)
                   (and (code-block-octets block) t)
                   (multiple-value-bind (known found) (gethash name ours)
                     (if found known (setf (gethash name ours) (eq (org-src-language name) language))))))))
      (dolist (line lines)
        ;; the code of these lines now, the blocks in them read again
        (let* ((new-first (line-stretch-new-first line))
               (new-last (line-stretch-new-last line))
               (j (tuatara.runtime:bisect (mid 0 (length new)) (< (code-block-first (aref new mid)) new-first)))
               (text (with-output-to-string (out)
                       (let ((at new-first))
                         (loop while (and (< j (length new)) (<= (code-block-first (aref new j)) new-last))
                               do (let* ((block (aref new j))
                                         (first (code-block-first block))
                                         (last (code-block-last block)))
                                    (setf (aref new j)
                                          (if (ours-p block)
                                              (multiple-value-bind (code skipped) (org-block-shown after first last)
                                                (setf at (write-code-lines code first at out))
                                                (make-code-block first last skipped))
                                              (make-code-block first last nil))))
                               (incf j))
                         (loop repeat (- new-last at) do (write-char #\Newline out)))))
               (change (trimmed-change shown (tuatara.text:line->byte shown (line-stretch-old-first line))
                                       (line-end-byte shown (line-stretch-old-last line)) text)))
          (when change (push change changes)))))
    (values (coerce (nreverse changes) 'simple-vector) new)))

(defun trimmed-change (rope start end text)
  "The change of ROPE's octets START below END for the string TEXT, as
DOC-CHANGES gives one, less what the two begin and end with alike; NIL when
they are the same."
  (let* ((old (tuatara.text:rope-string rope :start start :end end))
         (head (or (mismatch old text) (return-from trimmed-change nil)))
         (tail (- (length old) (mismatch old text :from-end t)))
         (tail (min tail (- (length old) head) (- (length text) head))))
    (list (+ start (tuatara.text:string-octet-length old :end head))
          (- end (tuatara.text:string-octet-length old :start (- (length old) tail)))
          (subseq text head (- (length text) tail)))))

(defun org-blocks-uri (buffer language)
  "The address LANGUAGE's source blocks of BUFFER are seen at: the file of
its project they are all written to (:tangle), when no buffer has it open;
else the note's own name with LANGUAGE's ending."
  (let* ((file (tuatara.io:native (tuatara.buffer:buffer-file buffer)))
         (tangled (remove-duplicates (org-blocks-tangle-values buffer language) :test #'equal :key #'car))
         (target (and (= 1 (length tangled))
                      (destructuring-bind ((tangle . name)) tangled
                        (cond ((member tangle '("no" "") :test #'string-equal) nil)
                              ((string-equal tangle "yes")
                               (format nil "~a.~a" (tuatara.io:native-file-stem file)
                                       (or (cdr (assoc name *code-file-extensions* :test #'string=))
                                           (language-extension language))))
                              (t tangle)))))
         (path (and target (tuatara.io:clean-path
                            (tuatara.io:native
                             (tuatara.io:expand-path target :directory (tuatara.io:parent-path file)))))))
    (path-uri (if (and path
                       (tuatara.io:really-within-p path (lsp-project-of buffer language))
                       (not (buffer-visiting path)))
                  path
                  (format nil "~a.~a" file (language-extension language))))))

(defun org-blocks-languages (buffer)
  "The languages a server knows of BUFFER's source blocks, a note, in the
order they first come: from its last parse (ORG-BUFFER-PARSE), those of
commented subtrees left out; from its text before there is one."
  (let ((tree (let ((parse (org-buffer-parse buffer))) (and parse (org-parse-tree parse))))
        (languages '()))
    (flet ((take (name)
             (let ((language (org-src-language (or name ""))))
               (when (and language (language-server-commands language))
                 (pushnew language languages)))))
      (if tree
          (labels ((walk (node)
                     (if (eq (org-node-type node) 'src-block)
                         (unless (org-commented-p node) (take (org-property node :language)))
                         (mapc #'walk (org-node-children node)))))
            (walk tree))
          (dolist (block (org-source-blocks (tuatara.text:rope-string (tuatara.buffer:buffer-text buffer))))
            (take (first block)))))
    (nreverse languages)))

(defun org-block-specs-key (buffer)
  "What ORG-BLOCK-DOC-SPECS's answer for BUFFER, a note, depends on, as data
to compare: its blocks (ORG-BLOCKS-KEY) and the files open in buffers, as
a block written to a file open in one is seen by the note's own name."
  (cons (sort (loop for other in tuatara.buffer:*buffers*
                    for file = (tuatara.buffer:buffer-file other)
                    when file collect (namestring file))
              #'string<)
        (org-blocks-key buffer)))

(defun org-blocks-size (buffer blocks language)
  "About the octets of the code of LANGUAGE's source blocks of BUFFER, a note
whose blocks are BLOCKS (ORG-NOTE-SOURCE-BLOCKS), as ORG-BLOCKS-SHADOW makes
it: a line end for each of the note's lines and the octets of the blocks'
lines, their indentation counted. Complexity: O(blocks x log of the text)."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (+ (tuatara.text:rope-lines rope)
       (loop for (name first last) in blocks
             when (eq (org-src-language name) language)
             sum (- (tuatara.text:line->byte rope last) (tuatara.text:line->byte rope first))))))

(defun org-block-doc-specs (buffer)
  "What servers see of BUFFER when it is a note with source blocks of a
language a server knows: for each such language, its blocks, as
*LSP-DOC-SPEC-FUNCTIONS* has them, each document's size as it was then;
NIL for any other buffer. Worked out from
the note's parse once for each change of what its blocks are (ORG-BLOCKS-KEY),
not again for text typed elsewhere; again when the files open change
\(ORG-BLOCK-SPECS-KEY)."
  (when (and (typep (buffer-major-mode buffer) 'org-mode) (tuatara.buffer:buffer-file buffer))
    (let ((key (org-block-specs-key buffer))
          (kept (tuatara.buffer:buffer-local buffer 'org-block-specs)))
      (if (and kept (equal key (car kept)))
          (cdr kept)
          (let* ((blocks (org-note-source-blocks buffer))
                 (specs (loop for language in (org-blocks-languages buffer)
                              collect (let ((language language))
                                        (list language (org-blocks-uri buffer language)
                                              (lambda (b &optional doc stretches)
                                                (org-blocks-shadow b language doc stretches))
                                              (org-blocks-size buffer blocks language))))))
            ;; kept only when read from the parse of the text as it is now
            (when (and (org-buffer-parse buffer) (org-parse-current-p buffer))
              (setf (tuatara.buffer:buffer-local buffer 'org-block-specs) (cons key specs)))
            specs)))))

(defun org-blocks-visit (buffer)
  "As a note is opened: have its changes noted, so that source blocks added
later are seen by their servers too (in a trusted project)."
  (when (and (typep (buffer-major-mode buffer) 'org-mode) (tuatara.buffer:buffer-file buffer)
             (null (lsp-refusal buffer)))
    (pushnew 'lsp-note-change (tuatara.buffer:buffer-change-functions buffer))))

(defun install-org-blocks ()
  "Have notes' source blocks seen by their languages' servers."
  (pushnew 'org-block-doc-specs *lsp-doc-spec-functions*)
  (setf tuatara.buffer:*find-file-functions*
        (append (remove 'org-blocks-visit tuatara.buffer:*find-file-functions*) (list 'org-blocks-visit))))
