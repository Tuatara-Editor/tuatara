;;;; lisp/org/dblock.lisp --- Dynamic blocks: #+BEGIN: name ... #+END:, their text made again by a writer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Org's dynamic blocks: a block opening with #+BEGIN: and a writer's
;;;; name, its parameters after it (:scope file :maxlevel 3), closing with
;;;; #+END:, has the text between made again on asking, by the writer of that
;;;; name. The clock table (clocktable) is written as Org writes it, line for
;;;; line, so that a file kept in both editors stays the same in each. Your own
;;;; writers come from your init file (define-dblock-writer). The parameters are
;;;; read as data, never run: a file names a writer, it cannot make one.

(in-package #:tuatara.defaults)

(defvar *dblock-writers* (make-hash-table :test 'equalp)
  "The writers of dynamic blocks, by name (as #+BEGIN: names them): each a
function of the block's parameters, a property list (:SCOPE \"file\"), returning
the block's text, its lines each ended by a line end.")

(defmacro define-dblock-writer (name lambda-list &body body)
  "Define the writer of the dynamic blocks named NAME (a string):
LAMBDA-LIST takes the block's parameters, a property
list whose keys are keywords and whose values are numbers, strings, words (as
strings, \"thisyear\") and lists of them; BODY returns the block's new text,
each line ended by a line end. BODY runs on a thread of its own, away from
the editor's: it works from the parameters, :CONTENT the block's text as it
was, and changes no buffer; what it says (MESSAGE) is shown once the block is
put in. For the init file:
\(define-dblock-writer \"hello\" (params) (format nil \"Hello, ~a~%\" (getf params :who)))"
  `(setf (gethash ,name *dblock-writers*) (lambda ,lambda-list ,@body)))

;;; Parameters, read as data

(defun dblock-read-params (text)
  "The parameters written after a dynamic block's name, TEXT, as a property
list, read as data only: :KEYWORDs (of up to 63 characters; one that neither
Tuatara nor your init file uses, ORG-KNOWN-KEYWORD, is kept as its text,":name"), numbers (of up to 18 digits), \"strings\" (\\n a line end in
one), words (symbols, as strings), ?x characters and ( lists ) of them,
\(2 . ?t) a pair; a quote before one is passed over. NIL for what cannot be
read."
  (let ((i 0) (n (length text)))
    (labels ((blank () (loop while (and (< i n) (member (char text i) '(#\Space #\Tab #\Newline #\Return #\'))) do (incf i)))
             (item ()
               (blank)
               (when (< i n)
                 (let ((c (char text i)))
                   (cond ((char= c #\()
                          (incf i)
                          (loop collect (progn (blank) (if (and (< i n) (char/= (char text i) #\))) (item) (loop-finish)))
                                into items
                                finally (incf i)
                                        (let ((items (remove :none items)))
                                          ;; (2 . ?t): the pair, as Emacs reads it
                                          (return (if (and (= (length items) 3) (equal (second items) "."))
                                                      (cons (first items) (third items))
                                                      items)))))
                         ((char= c #\))
                          (incf i) :none)
                         ((char= c #\")
                          (incf i)
                          (with-output-to-string (out)
                            (loop while (< i n)
                                  do (let ((d (char text i)))
                                       (incf i)
                                       (cond ((char= d #\") (return))
                                             ((and (char= d #\\) (< i n))
                                              (write-char (case (char text i) (#\n #\Newline) (#\t #\Tab) (t (char text i))) out)
                                              (incf i))
                                             (t (write-char d out)))))))
                         (t (let* ((start i)
                                   (end (or (position-if (lambda (d) (member d '(#\Space #\Tab #\Newline #\Return #\( #\) #\")))
                                                         text :start i)
                                            n))
                                   (word (subseq text start end)))
                              (setf i end)
                              (cond ((and (< 1 (length word) 64) (char= (char word 0) #\:))
                                     ;; a keyword no writer uses stays a word, as text
                                     (or (org-known-keyword (subseq word 1)) word))
                                    ;; a number of a size numbers come in; longer, a word, read in no time
                                    ((and (<= (length word) 18) (every #'digit-char-p word)) (parse-integer word))
                                    ((and (< 1 (length word) 19) (char= (char word 0) #\-) (every #'digit-char-p (subseq word 1)))
                                     (parse-integer word))
                                    ((and (= (length word) 2) (char= (char word 0) #\?)) (char word 1))
                                    (t word)))))))))
      (handler-case
          (loop for item = (item)
                while item
                unless (eq item :none) collect item
                do (blank)
                while (< i n))
        (error () nil)))))

;;; Finding and updating blocks

(defun dblock-of-node (node text)
  "The dynamic block NODE, a node of TEXT's parse, as DBLOCK-BLOCKS gives each."
  (let* ((begin (or (org-node-post-affiliated node) (org-node-begin node)))
         (content (min (length text) (1+ (string-line-end text begin)))))
    (list begin content (or (org-node-contents-end node) content)
          (org-property node :block-name) (or (org-property node :arguments) ""))))

(defun dblock-blocks (tree text)
  "The dynamic blocks of TREE, the parse of TEXT, in order: each (BEGIN-LINE
CONTENT-START CONTENT-END NAME PARAMS), characters of TEXT, CONTENT-START the
start of the line after #+BEGIN:, CONTENT-END the start of the #+END: line."
  (let ((found '()))
    (org-walk-tree tree (lambda (node)
                          (when (eq (org-node-type node) 'dynamic-block)
                            (push (dblock-of-node node text) found))))
    (nreverse found)))

(defvar *dblock-position* nil
  "While a dynamic block is made again, where it begins, a character of its
buffer's parse: the place its writer works from (a clock table's subtree).")

;;; Made in the background

(defvar *dblock-own* nil
  "While dynamic blocks are made on a thread of their own: the parse of the
buffer holding them as it was when they were asked for, (TREE TEXT).")

(defvar *dblock-sources* nil
  "While dynamic blocks are made on a thread of their own: the text of each
file open in a buffer when they were asked for, by its native path (the
buffer's rope, which never changes, or a copy of a file mapped from disk and
changed), read in place of the file on disk; NIL otherwise.")

(defvar *dblock-job* nil
  "While dynamic blocks are made on a thread of their own, their job: where
their writers say how far they have got (DBLOCK-PROGRESS) and learn whether
they are to stop (DBLOCK-CHECK-STOP).")

(defparameter *dblock-carried-settings*
  '(*org-now* *org-agenda-files* tuatara-user:*default-directory* *formatted-file-directory* *org-tags-column*)
  "The settings dynamic blocks are made with, carried to the thread that makes
them as they are on the editor's when they are asked for
\(TUATARA.RUNTIME:CARRY-SETTINGS).")

(defparameter *dblock-wait* 1/10
  "The seconds a command making dynamic blocks again waits for them: blocks
made by then are put in at once, as part of the command; else they are put
in when they are made, the editor free meanwhile, P lit in the mode line
\(*PROCESSING-INDICATOR*).")

(defstruct (dblock-job (:include tuatara.runtime:job) (:constructor make-dblock-job (buffer pieces words count))
                       (:copier nil))
  "Dynamic blocks of BUFFER made again, a job (TUATARA.RUNTIME:JOB). PIECES
are the blocks, each (BEGIN START END FIRST WRITER PARAMS POSITION): anchors
at the start of its #+BEGIN: line, of its text and of its #+END: line, which
follow the edits made meanwhile; FIRST its #+BEGIN: line as it was, line end
and all; WRITER and PARAMS what makes its text; POSITION where it began, a
character of the parse. WORDS say what the job does, for the list of jobs;
COUNT, when the job is all the buffer's blocks, how many there are, said once
they are put in. TEXTS hold each block's new text in order, NIL for one left
as it was; MESSAGES what the writers said (a cell, as *MESSAGES-KEPT* takes
it); DONE how many blocks are made and PART how far the one under way has
got, from 0 to 1."
  pieces words count (texts '()) (messages (list nil)) (done 0) (part 0))

(setf (documentation 'dblock-job-p 'function) "True when OBJECT is a DBLOCK-JOB.")

(defun dblock-check-stop ()
  "For a writer of dynamic blocks between two pieces of its work: end the job
there when it has been asked to stop (TUATARA.RUNTIME:JOB-STOPPED)."
  (when (and *dblock-job* (car (dblock-job-cancel *dblock-job*)))
    (error 'tuatara.runtime:job-stopped)))

(defun dblock-progress (fraction)
  "For a writer of dynamic blocks: say it has got FRACTION of its way, from 0
to 1, for the mode line to show; and end the job there when it has been
asked to stop."
  (when *dblock-job*
    (setf (dblock-job-part *dblock-job*) (max 0 (min 1 fraction)))
    (dblock-check-stop)))

(defmethod tuatara.runtime:job-percent ((job dblock-job))
  "How much of JOB's dynamic blocks is made, as a percentage."
  (tuatara.runtime:running-percent (+ (dblock-job-done job) (dblock-job-part job)) (length (dblock-job-pieces job))))

(tuatara.runtime:define-job-words dblock-job
  :doing "Making blocks"
  :description (dblock-job-words job)
  :verb "make the dynamic blocks"
  :left-words "the dynamic blocks are as they were")

(defmethod tuatara.runtime:job-guarded-spans ((job dblock-job))
  "The blocks being made, each from the start of its #+BEGIN: line to past the
end of its #+END: line, where they stand now."
  (let* ((buffer (dblock-job-buffer job))
         (version (tuatara.buffer:buffer-version buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope)))
    (loop for (begin nil end) in (dblock-job-pieces job)
          for b = (tuatara.text:anchor-position begin version)
          for e = (tuatara.text:anchor-position end version)
          collect (cons (min b e) (min size (1+ (tuatara.text:rope-line-end rope (min (max b e) size))))))))

(defun dblock-snapshot-sources ()
  "The text of each file open in a buffer now, by its native path, for
*DBLOCK-SOURCES*: the buffer's rope, which never changes; for a file mapped
from disk, a copy when it was changed and no larger than a dynamic block
reads (*ORG-FILE-READ-LIMIT*), else nothing (it is read from disk)."
  (let ((sources (make-hash-table :test 'equal)))
    (dolist (buffer tuatara.buffer:*buffers* sources)
      (let ((file (tuatara.buffer:buffer-file buffer)))
        (when file
          (let ((path (tuatara.io:native file)))
            (when path
              (cond ((null (tuatara.buffer:buffer-mapping buffer))
                     (setf (gethash path sources) (tuatara.buffer:buffer-text buffer)))
                    ((and (tuatara.buffer:buffer-modified-p buffer)
                          (<= (tuatara.buffer:buffer-size buffer) *org-file-read-limit*))
                     (setf (gethash path sources) (tuatara.buffer:buffer-string buffer)))))))))))

(defun dblock-source-text (path)
  "The text of the file at PATH, a native path, for a dynamic block: on the
thread that makes it, its buffer's as it was when the block was asked for,
else the disk's; else its buffer's now or the disk's (ORG-AGENDA-FILE-SOURCE).
Its lines end in a newline alone; NIL when it cannot be read, as
ORG-READ-FILE-TEXT reads files."
  (if *dblock-sources*
      (multiple-value-bind (source found) (gethash path *dblock-sources*)
        (org-agenda-source-text path (if found source :disk)))
      (org-agenda-source-text path (org-agenda-file-source path))))

(defun dblock-make (job tree text sources)
  "Make the blocks of JOB on the thread it runs on, from TREE and TEXT, its
buffer's parse, with the SOURCES (*DBLOCK-SOURCES*) taken when it was asked
for; it ends :DONE, or :STOPPED when asked to stop
\(TUATARA.RUNTIME:JOB-STOPPED). A writer that fails leaves its block as it was,
and says why."
  (let ((*dblock-job* job)
        (*messages-kept* (dblock-job-messages job))
        (*dblock-sources* sources)
        (*dblock-own* (list tree text))
        (*current-buffer* (dblock-job-buffer job))
        (*org-current-text* text))
    (dolist (piece (dblock-job-pieces job))
      (destructuring-bind (begin start end first writer params position) piece
        (declare (ignore begin start end first))
        (setf (dblock-job-part job) 0)
        (dblock-check-stop)
        (let ((new (let ((*dblock-position* position))
                     (handler-case (funcall writer params)
                       (error (e) (message "The dynamic block ~a: ~a" (getf params :name) e) nil)))))
          (setf (dblock-job-texts job) (append (dblock-job-texts job) (list (and (stringp new) new))))
          (incf (dblock-job-done job)))))
    (dblock-check-stop)
    (setf (dblock-job-status job) :done)))

(defun dblock-start (buffer tree text blocks &key count)
  "Make BLOCKS of BUFFER again (as DBLOCK-BLOCKS gives them, of TEXT, BUFFER's
parse as it is now with TREE), each by the writer its
#+BEGIN: line names, on a thread of their own, as a job of BUFFER: put in at
once when they are made within *DBLOCK-WAIT* seconds, else once they are
made, as the job is taken in, the editor free meanwhile. A job running in BUFFER
already has this one wait its turn. COUNT, when BLOCKS are all of BUFFER's,
how many, said once they are put in."
  (let* ((returns (org-returns-of buffer))
         (words (if count "Making the dynamic blocks again" "Making a dynamic block again")))
    (unless (queue-if-busy buffer words (list (cons (org-char->byte buffer (first (first blocks)) returns) :after))
                           (lambda (offset) (dblock-start-queued buffer offset count)))
      (let* ((version (tuatara.buffer:buffer-version buffer))
             (pieces (loop for (begin content end name params) in blocks
                           for writer = (gethash name *dblock-writers*)
                           if (null writer)
                             do (message "No writer for the dynamic block ~a: define-dblock-writer makes one" name)
                           else
                             collect (list (tuatara.text:make-anchor version (org-char->byte buffer begin returns) :bias :after)
                                           (tuatara.text:make-anchor version (org-char->byte buffer content returns))
                                           (tuatara.text:make-anchor version (org-char->byte buffer end returns) :bias :after)
                                           (subseq text begin content)
                                           writer
                                           (append (dblock-read-params params) (list :name name :content (subseq text content end)))
                                           begin)))
             (job (make-dblock-job buffer pieces words count)))
        (let ((sources (dblock-snapshot-sources)))
          (tuatara.runtime:start-job-thread job (tuatara.runtime:carry-settings
                                                 *dblock-carried-settings*
                                                 (lambda (job) (dblock-make job tree text sources)))
                                            :name "tuatara dynamic blocks"
                                            :details (format nil "~d block~:p" (length pieces)))
          (take-in-soon job *dblock-wait*))))))

(defun dblock-start-queued (buffer offset count)
  "Make again, its turn come, the dynamic block of BUFFER whose #+BEGIN: line
begins at its octet OFFSET, or with COUNT all of its blocks, from its text as
it is now."
  (let ((*current-buffer* buffer))
    (with-org-structure (buffer tree text position)
      (let ((blocks (if count
                        (dblock-blocks tree text)
                        (let ((block (dblock-at tree text (org-byte->char buffer offset))))
                          (and block (list block))))))
        (cond ((null blocks) (message "The dynamic block is no longer there"))
              ((tuatara.buffer:buffer-local buffer :read-only) (message "~a is read-only" (tuatara.buffer:buffer-name buffer)))
              (t (dblock-start buffer tree text blocks :count (and count (length blocks)))))))))

(defun dblock-piece-place (buffer piece)
  "Where the text of PIECE, a block of a job of BUFFER, stands in BUFFER now,
as two octets, its start and end; NIL when its #+BEGIN: line, as it was, its
text, as it was, or its #+END: line is no longer there."
  (destructuring-bind (begin start end first writer params &rest more) piece
    (declare (ignore writer more))
    (let* ((version (tuatara.buffer:buffer-version buffer))
           (rope (tuatara.buffer:buffer-text buffer))
           (b (tuatara.text:anchor-position begin version))
           (s (tuatara.text:anchor-position start version))
           (e (tuatara.text:anchor-position end version)))
      (and (<= b s e)
           (= b (tuatara.text:rope-line-start rope b))
           (= e (tuatara.text:rope-line-start rope e))
           (string= first (remove #\Return (tuatara.text:rope-string rope :start b :end s)))
           (string= (getf params :content) (remove #\Return (tuatara.text:rope-string rope :start s :end e)))
           (cl-ppcre:scan (org-re *org-dynamic-block-end-re*)
                          (remove #\Return (tuatara.text:rope-string rope :start e :end (tuatara.text:rope-line-end rope e))))
           (values s e)))))

(defmethod tuatara.runtime:job-take-in ((job dblock-job))
  "Show what the writers of JOB said, and put the texts JOB made in their
blocks of its buffer, in one step to undo: a block whose #+BEGIN: line, text
or #+END: line changed meanwhile is left as it is, and said to be; point moved
past the changes before it. Nothing is put in once the job was stopped, or its
buffer killed, or when it may no longer take them (RESULT-REFUSED-P)."
  (let ((buffer (dblock-job-buffer job)))
    (dolist (said (reverse (car (dblock-job-messages job))))
      (message "~a" said))
    (cond ((not (live-buffer-p buffer)) nil)
          ((member (dblock-job-status job) '(:stopped :failed))
           (message "~a" (tuatara.runtime:job-ending-words job)))
          (t
           (let ((rope (tuatara.buffer:buffer-text buffer))
                 (edits '())
                 (moved 0))
             (loop for piece in (dblock-job-pieces job)
                   for new in (dblock-job-texts job)
                   when new
                     do (multiple-value-bind (s e) (dblock-piece-place buffer piece)
                          (if (null s)
                              (progn (incf moved)
                                     (message "A dynamic block changed while it was made: \\[org-dblock-update] makes it again"))
                              (let ((new (text-for-buffer new buffer)))
                                (unless (string= new (tuatara.text:rope-string rope :start s :end e))
                                  (push (list s e (tuatara.text:string-octets new)) edits))))))
             (setf edits (sort edits #'< :key #'first))
             (when (and edits (result-refused-p buffer "the update of the dynamic blocks"
                                                (loop for (s e octets) in edits sum (- (length octets) (- e s)))))
               (return-from tuatara.runtime:job-take-in))
             (when edits
               (let* ((point (tuatara.buffer:buffer-point buffer))
                      (shift 0) (inside nil))
                 (loop for (s e octets) in edits
                       do (cond ((<= e point) (incf shift (- (length octets) (- e s))))
                                ((< s point) (setf inside (+ s shift)) (loop-finish))))
                 (tuatara.buffer:buffer-edit
                  buffer
                  (coerce (loop for (s e octets) in edits
                                collect (tuatara.text:make-edit s e (if (plusp (length octets)) octets nil)))
                          'vector))
                 (setf (tuatara.buffer:buffer-point buffer) (or inside (+ point shift)))))
             (let ((count (dblock-job-count job)))
               (when (and count (zerop moved))
                 (message "~[No dynamic block here~;Made 1 dynamic block again~:;Made ~:*~d dynamic blocks again~]" count))))))))

(defun dblock-update (buffer tree text block)
  "Make BLOCK of BUFFER (as DBLOCK-BLOCKS gives it, of TEXT, BUFFER's parse
with TREE) again by its writer, in the background (DBLOCK-START)."
  (dblock-start buffer tree text (list block)))

(defun dblock-at (tree text position)
  "The dynamic block of TREE, the parse of TEXT, the character POSITION is in
\(on its #+BEGIN: or #+END: line, or between), as DBLOCK-BLOCKS gives each, or
NIL."
  (let* ((node (org-node-at tree position (lambda (n) (eq (org-node-type n) 'dynamic-block))))
         (block (and node (dblock-of-node node text))))
    (and block (<= (first block) position (string-line-end text (third block))) block)))

(define-command org-dblock-update ()
  "Make the dynamic block point is in again (\\`C-c C-x C-u'): the text
between #+BEGIN: and #+END: written anew by the writer #+BEGIN: names, in the
background, the editor free meanwhile. With \\[universal-argument], every
dynamic block of the buffer."
  (if (prefix-argument)
      (org-update-all-dblocks)
      (with-org-structure (buffer tree text position)
        (let ((block (dblock-at tree text position)))
          (if block
              (progn (check-writable buffer) (dblock-update buffer tree text block))
              (message "Not in a dynamic block (#+BEGIN: ... #+END:)"))))))

(define-command org-update-all-dblocks ()
  "Make every dynamic block of the current Org buffer again: each written from
the text as it is now, in the
background, the editor free meanwhile, and put in together, in one step to undo."
  (with-org-structure (buffer tree text position)
    (check-writable buffer)
    (let ((blocks (dblock-blocks tree text)))
      (if blocks
          (dblock-start buffer tree text blocks :count (length blocks))
          (message "No dynamic block here")))))

;;; New blocks

(defvar *dblock-inserters* (make-hash-table :test 'equal)
  "The commands that put a new dynamic block in, by the name its #+BEGIN: line
gives it: \\[org-dynamic-block-insert-dblock] offers them.")

(defun dblock-param-text (value)
  "VALUE, a dynamic block's parameter, as its #+BEGIN: line has it:
a string in quotes, its quotes and backslashes escaped; a keyword
with its colon; a word (a symbol), a number as it is; NIL as nil."
  (cond ((null value) "nil")
        ((stringp value) (format nil "\"~a\"" (cl-ppcre:regex-replace-all "([\"\\\\])" value "\\\\\\1")))
        ((keywordp value) (format nil ":~(~a~)" (symbol-name value)))
        ((symbolp value) (string-downcase (symbol-name value)))
        (t (princ-to-string value))))

(defun dblock-insert (name params)
  "Put a new dynamic block NAME in the current Org buffer: on the line point
is on when it is blank, else on a line of its own after it; its #+BEGIN: line
NAME and PARAMS, a property list, written as DBLOCK-PARAM-TEXT writes them, an
empty line and #+END:; then make it (in the background), point on its #+BEGIN:
line."
  (if (not (typep (buffer-major-mode (current-buffer)) 'org-mode))
      (message "Dynamic blocks are Org's: this buffer is not in Org mode")
      (with-org-structure (buffer tree text position)
        (check-writable buffer)
        (let* ((line-start (string-line-start text position))
               (line-end (string-line-end text position))
               (blank (blank-text-p (subseq text line-start line-end)))
               (at (if blank position line-end))
               (column (if blank (- position line-start) 0))
               (first (format nil "#+BEGIN: ~a~{ ~a~}" name (mapcar #'dblock-param-text params)))
               (new (format nil "~:[~%~;~]~a~%~%~a#+END:~%" blank first (make-string column :initial-element #\Space)))
               (begin (if blank at (1+ at))))
          (org-apply-edits buffer (list (list at at new)) :point begin :final t)
          (with-org-structure (buffer tree text position)
            (let ((block (dblock-at tree text position)))
              (when block (dblock-update buffer tree text block))))))))

(define-command org-dynamic-block-insert-dblock ()
  "Put a new dynamic block in (\\`C-c C-x x'): its kind asked for among those
that can be (a clock table, links to files with formatted names...), then what
that kind asks."
  (let ((names (sort (alexandria:hash-table-keys *dblock-inserters*) #'string<)))
    (let ((chosen (completing-read "Dynamic block: " names :require-match t)))
      (let ((command (gethash chosen *dblock-inserters*)))
        (when command (funcall command))))))

(define-command org-clock-report ()
  "Make the clock table point is in again, or put a new one in: its scope the
subtree point is in, or the file before the
first headline, its depth two levels. With \\[universal-argument], the first clock table of
the buffer, point moved to it."
  (with-org-structure (buffer tree text position)
    (let* ((first (and (prefix-argument)
                       (find "clocktable" (dblock-blocks tree text) :key #'fourth :test #'string-equal)))
           (here (or first (let ((block (dblock-at tree text position)))
                             (and block (string-equal (fourth block) "clocktable") block)))))
      (cond (here
             (check-writable buffer)
             (when first (org-goto-char buffer (first first)))
             (dblock-update buffer tree text here))
            (t (dblock-insert "clocktable"
                              (list :scope (if (org-current-headline tree position) 'subtree 'file) :maxlevel 2)))))))

(setf (gethash "clocktable" *dblock-inserters*) 'org-clock-report)

;;; The clock table

(defvar *clocktable-parses* nil
  "While a clock table is made: what it read, by file path (:own for the
block's own buffer, (:files . SCOPE) for a scope's files), each read once
for the table and all its steps (MEMO-VALUES); NIL otherwise.")

(defvar *clocktable-clocks* nil
  "While a clock table is made: the clocks of each headline's own text, read
once (CLOCKTABLE-CLOCKS), by its node; NIL otherwise.")

(defun clocktable-clocks (headline text)
  "The clocks of HEADLINE's own text (TEXT its parse), in order: its clock
lines (ORG-READ-CLOCK-LINE) wherever they stand in it, in a drawer or a list
item too, not in a block; each (START . END) in universal time, or the
minutes of a line giving a time alone (CLOCK: => 1:30), one still running left
out; read once for a table
while *CLOCKTABLE-CLOCKS* holds them."
  (flet ((read-them ()
           (let ((found '()))
             (labels ((walk (node)
                        (dolist (child (org-node-children node))
                          (case (org-node-type child)
                            (headline)
                            (clock
                             (multiple-value-bind (started stopped duration) (org-read-clock-line text (org-node-begin child))
                               (cond (stopped
                                      (let ((ts (org-read-ts started)) (te (org-read-ts stopped)))
                                        (when (and ts te) (push (cons (org-ts-time ts) (org-ts-time te)) found))))
                                     ((and duration (null started))
                                      (let ((colon (position #\: duration)))
                                        (push (+ (* 60 (parse-integer duration :end colon)) (parse-integer duration :start (1+ colon)))
                                              found))))))
                            (t (walk child))))))
               (walk headline))
             (nreverse found))))
    (memo-values *clocktable-clocks* headline #'read-them)))

(defun clocktable-minutes (headline text tstart tend)
  "The minutes clocked in HEADLINE's own text (CLOCKTABLE-CLOCKS; TEXT its
parse), each clock counted only within TSTART and TEND (universal times, NIL
for no bound), a time given alone counted whole."
  (let ((minutes 0))
    (dolist (clock (clocktable-clocks headline text) minutes)
      (if (consp clock)
          (let ((s (if tstart (max (car clock) tstart) (car clock)))
                (e (if tend (min (cdr clock) tend) (cdr clock))))
            (when (> e s) (incf minutes (floor (- e s) 60))))
          (incf minutes clock)))))

(defun clocktable-block (block now &key (wstart 1) (mstart 1))
  "The time a clock table's :block covers as three values, its start and end
\(universal times) and what the caption says of it (the year 2026): today,
yesterday, thisweek, lastweek, thismonth,
lastmonth, thisyear, lastyear, today-N and the like, 2026, 2026-03, 2026-03-01,
2026-W10, untilnow; a week beginning on the day WSTART (Monday 1, Sunday 7)
and a month on its day MSTART. NIL for a block it does not know."
  (let* ((word (string-downcase (princ-to-string block)))
         (today (org-today now)))
    (multiple-value-bind (year month) (day-date today)
      (labels ((day-range (day)
                 (values (day-universal-time day) (day-universal-time (1+ day))
                         (format-day "%A, %B %d, %Y" day)))
               (week-range (first)
                 (values (day-universal-time first) (day-universal-time (+ first 7))
                         (format-day "week %G-W%V" first)))
               (month-range (y mo)
                 ;; from the month's day MSTART to the next month's, rolled over as Emacs rolls dates
                 (values (day-universal-time (day-number y mo mstart)) (day-universal-time (day-number y (1+ mo) mstart))
                         (format-day "%B %Y" (day-number y mo 1))))
               (year-range (y)
                 (values (day-universal-time (day-number y 1 1)) (day-universal-time (day-number (1+ y) 1 1))
                         (format nil "the year ~d" y)))
               (shift (base)
                 (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings (format nil "^~a(?:-(\\d+))?$" base) word)
                   (and match (if (aref groups 0) (parse-integer (aref groups 0)) 0)))))
        (let ((first (week-start today wstart)) (n nil))
          (cond ((setf n (shift "today")) (day-range (- today n)))
                ((string= word "yesterday") (day-range (1- today)))
                ((setf n (shift "thisweek")) (week-range (- first (* 7 n))))
                ((string= word "lastweek") (week-range (- first 7)))
                ((setf n (shift "thismonth")) (multiple-value-call #'month-range (add-months year month (- n))))
                ((string= word "lastmonth") (multiple-value-call #'month-range (add-months year month -1)))
                ((setf n (shift "thisyear")) (year-range (- year n)))
                ((string= word "lastyear") (year-range (1- year)))
                ((string= word "untilnow") (values nil now "now"))
                ((cl-ppcre:scan "^\\d{4}$" word) (year-range (parse-integer word)))
                ((cl-ppcre:scan "^\\d{4}-\\d{2}$" word)
                 (month-range (parse-integer word :end 4) (parse-integer word :start 5)))
                ((eql (nth-value 3 (written-date-at word 0 (length word) :iso)) (length word))
                 (multiple-value-bind (y m d) (written-date-at word 0 (length word) :iso) (day-range (day-number y m d))))
                ((cl-ppcre:scan "^\\d{4}-w\\d{2}$" word)
                 (week-range (iso-week-monday (parse-integer word :end 4) (parse-integer word :start 6))))
                (t (values nil nil nil))))))))

(defun clocktable-time (minutes)
  "MINUTES as a clock table writes a time
by default: 1:05, and a day or more with its days, 1d 4:30."
  (multiple-value-bind (days rest) (floor minutes 1440)
    (multiple-value-bind (h m) (floor rest 60)
      (if (plusp days) (format nil "~dd ~d:~2,'0d" days h m) (format nil "~d:~2,'0d" h m)))))

(defstruct (clocktable-entry (:conc-name clocktable-entry-))
  "A headline of a clock table: its LEVEL, TITLE (as written), MINUTES (its
own and its sub-headlines' within the table's time), HEADLINE (the parse
node), TAGS (its own and those it inherits, the file's first), CATEGORY and
TIMESTAMP (what the :timestamp column shows, or NIL)."
  level title minutes headline tags category timestamp)

(defun clocktable-timestamps-in (text start end &key skip-clocks)
  "The timestamps written in TEXT from the character START below END, in
order, each (ACTIVE . TEXT), a range (<a>--<b>) one; with SKIP-CLOCKS, those
on CLOCK: lines and in property drawers left out (it reads none from a drawer's :KEY: lines)."
  (let ((found '()) (last-line -1) (skip nil))
    (cl-ppcre:do-matches (s e *org-timestamp-or-range-scanner* text nil :start start :end end)
      (let* ((whole (subseq text s e))
             (first (subseq whole 0 (or (search "--" whole) (length whole))))
             (line-start (string-line-start text s)))
        ;; what the line begins with, looked at once for the line, and only at its beginning
        (unless (= line-start last-line)
          (setf last-line line-start
                skip (and skip-clocks
                          (cl-ppcre:scan "^[ \\t]*(?:CLOCK:|:[^\\s:]+:[ \\t])" text :start line-start :end (min s (+ line-start 256))))))
        (when (and (org-read-ts first) (not skip))
          (push (cons (char= (char whole 0) #\<) whole) found))))
    (nreverse found)))

(defun clocktable-headline-timestamp (entry)
  "The timestamp a clock table's :timestamp column shows for the headline of
ENTRY (an ORG-AGENDA-ENTRY): its SCHEDULED,
else its DEADLINE, else the first active timestamp on its line or in its own
text after its planning line, else the first inactive one; NIL for none."
  (let* ((text (agenda-entry-text entry))
         (begin (agenda-entry-position entry))
         (own-end (agenda-entry-end entry))
         (line-end (string-line-end text begin))
         (after (if (or (agenda-entry-scheduled entry) (agenda-entry-deadline entry) (agenda-entry-closed entry))
                    (org-line-after text (1+ line-end))
                    (min (length text) (1+ line-end))))
         (stamps (append (clocktable-timestamps-in text begin line-end)
                         (clocktable-timestamps-in text after (max after own-end) :skip-clocks t))))
    (or (agenda-entry-scheduled entry)
        (agenda-entry-deadline entry)
        (cdr (find t stamps :key #'car))
        (cdr (find nil stamps :key #'car)))))

(defun clocktable-rows (tree text file maxlevel tstart tend &key root match timestamp)
  "The rows of a clock table of TREE (TEXT its parse, FILE its path),
CLOCKTABLE-ENTRY objects: MINUTES those clocked on the headline and all under
it within TSTART and TEND, the headlines with none left out, those deeper than
MAXLEVEL counted in the ones above; with ROOT, only that headline's subtree;
with MATCH (an Org match, as :match gives it), only the clocks of the
headlines it selects, the headlines above them shown with their time; with
TIMESTAMP, each entry's timestamp found. Every headline is read
\(ORG-WALK-ENTRIES), those under COMMENT and archived ones too. Two values: the
rows and the minutes of them all."
  (let ((predicate (and match (org-match-predicate match)))
        (own (make-hash-table :test 'eq))
        (sums (make-hash-table :test 'eq))
        (rows '()))
    (org-walk-entries (lambda (entry headline)
                        (setf (gethash headline own)
                              (if (or (null predicate) (funcall predicate entry))
                                  (clocktable-minutes headline text tstart tend)
                                  0))
                        (when (<= (agenda-entry-level entry) maxlevel)
                          (push (make-clocktable-entry :level (agenda-entry-level entry) :title (agenda-entry-title entry)
                                                       :minutes 0 :headline headline :tags (agenda-entry-tags entry)
                                                       :category (agenda-entry-category entry)
                                                       :timestamp (and timestamp (clocktable-headline-timestamp entry)))
                                rows)))
                      file text tree :root root)
    (labels ((sum (headline)
               ;; the minutes of HEADLINE and all under it, worked out once
               (or (gethash headline sums)
                   (setf (gethash headline sums)
                         (+ (gethash headline own 0)
                            (loop for child in (org-node-children headline)
                                  when (eq (org-node-type child) 'headline) sum (sum child)))))))
      (dolist (row rows)
        (setf (clocktable-entry-minutes row) (sum (clocktable-entry-headline row))))
      ;; the headlines with no time left out
      (values (remove-if-not #'plusp (nreverse rows) :key #'clocktable-entry-minutes)
              (if root
                  (sum root)
                  (loop for child in (org-node-children tree) when (eq (org-node-type child) 'headline) sum (sum child)))))))

(defun org-link-search-words (title)
  "TITLE as a link to its headline searches for it: statistics cookies ([2/3],
[66%]) out, each
run of blanks one space, none at either end."
  (string-trim " " (cl-ppcre:regex-replace-all
                    "[ \\t]+"
                    (cl-ppcre:regex-replace-all (load-time-value (cl-ppcre:create-scanner *org-statistics-cookie-re*)) title " ")
                    " ")))

(defun clocktable-headline (title link link-path)
  "TITLE as a clock table's row shows it: with LINK, a link to the headline,
in the file at LINK-PATH when given, labelled with its title, statistics
cookies and links' brackets taken out."
  ;; a title of pages (no headline is) cut, since the patterns below look back over it
  (when (> (length title) 4096) (setf title (subseq title 0 4096)))
  (if (not link)
      title
      (let ((words (org-link-search-words title)))
        (org-link-text (format nil "~:[~*~;file:~a::~]*~a" link-path link-path words)
                       (org-links-as-labels words)))))

(defun clocktable-property (tree headline name inherit)
  "The value of the property NAME at HEADLINE of TREE,
as a clock table takes it: its own (any case), with INHERIT also from the
headlines above and the file; NIL when it has none."
  (or (org-headline-property headline name)
      (and inherit (org-inherited-property tree headline name))))

(defun clocktable-file-table (tree text file params tstart tend &key root link-path)
  "The clock table data of one file:
(FILE MINUTES ENTRIES), each entry (LEVEL HEADLINE TIME PROPERTIES TAGS
TIMESTAMP), the properties PARAMS asks for, (NAME . VALUE) of those the
headline has, its tags with the inherited, and its timestamp when asked."
  (let ((maxlevel (clocktable-maxlevel params))
        (properties (clocktable-properties params))
        (inherit (clocktable-flag params :inherit-props))
        (link (clocktable-flag params :link))
        (match (let ((m (getf params :match)))
                 (cond ((not (and (stringp m) (plusp (length (string-trim " " m))))) nil)
                       ;; a pattern from a file is never run: it could take for ever
                       ((find #\{ m) (message "The clock table's :match is left out: a {pattern} in a file is not run") nil)
                       (t m)))))
    (multiple-value-bind (rows total)
        (clocktable-rows tree text file maxlevel tstart tend :root root :match match :timestamp (clocktable-flag params :timestamp))
      (list file total
            (loop for entry in rows
                  for headline = (clocktable-entry-headline entry)
                  collect (list (clocktable-entry-level entry)
                                (clocktable-headline (clocktable-entry-title entry) link link-path)
                                (clocktable-entry-minutes entry)
                                (loop for name in properties
                                      for value = (clocktable-property tree headline name inherit)
                                      when value collect (cons name value))
                                (clocktable-entry-tags entry)
                                (clocktable-entry-timestamp entry)))))))

(defparameter *clocktable-languages*
  '(("en" "File" "L" "Timestamp" "Headline" "Time" "ALL" "Total time" "File time" "Clock summary at")
    ("de" "Datei" "E" "Zeitstempel" "Kopfzeile" "Dauer" "GESAMT" "Gesamtdauer" "Dateizeit" "Erstellt am")
    ("es" "Archivo" "N" "Fecha y hora" "Tarea" "Duración" "TODO" "Duración total" "Tiempo archivo" "Generado el")
    ("fr" "Fichier" "N" "Horodatage" "En-tête" "Durée" "TOUT" "Durée totale" "Durée fichier" "Horodatage sommaire à")
    ("nl" "Bestand" "N" "Tijdstip" "Rubriek" "Duur" "ALLES" "Totale duur" "Bestandstijd" "Klok overzicht op")
    ("nn" "Fil" "N" "Tidspunkt" "Overskrift" "Tid" "ALLE" "Total tid" "Filtid" "Tidsoversyn")
    ("pl" "Plik" "P" "Data i godzina" "Nagłówek" "Czas" "WSZYSTKO" "Czas całkowity" "Czas pliku" "Poddumowanie zegara na")
    ("pt-BR" "Arquivo" "N" "Data e hora" "Título" "Hora" "TODOS" "Hora total" "Hora do arquivo" "Resumo das horas em")
    ("sk" "Súbor" "L" "Časová značka" "Záhlavie" "Čas" "VŠETKO" "Celkový čas" "Čas súboru" "Časový súhrn pre"))
  "A clock table's words in each language Org has them in (:lang): the file,
level, timestamp, headline
and time columns, the row of all files, the total, a file's time and the
caption's first words.")

(defun clocktable-word (params word)
  "WORD, one of a clock table's English words, in the language the :lang of
PARAMS names; WORD itself in a language Org has no words for."
  (let* ((lang (let ((l (getf params :lang))) (if (stringp l) l "en")))
         (row (assoc lang *clocktable-languages* :test #'string-equal))
         (i (position word (rest (first *clocktable-languages*)) :test #'string=)))
    (or (and row i (nth i (rest row))) word)))

(defvar *clocktable-formatters* (make-hash-table :test 'equalp)
  "The formatters a clock table's :formatter may name, by name: each a
function of the tables and the parameters (DEFINE-CLOCKTABLE-FORMATTER).")

(defmacro define-clocktable-formatter (name lambda-list &body body)
  "Define the formatter NAME (a string) a clock table's :formatter names, as
Org's :formatter names a function: LAMBDA-LIST takes the tables, a list of
\(FILE MINUTES ENTRIES), each entry (LEVEL HEADLINE MINUTES PROPERTIES TAGS
TIMESTAMP), and the block's parameters (:multifile among them); BODY returns
the block's whole text, each line ended by a line end. For the init file."
  `(setf (gethash ,name *clocktable-formatters*) (lambda ,lambda-list ,@body)))

(defun clocktable-flag (params key)
  "True when the clock table's PARAMS give KEY a value other than nil."
  (let ((value (getf params key)))
    (and value (not (equalp value "nil")))))

(defun clocktable-maxlevel (params)
  "The deepest level a clock table of PARAMS shows (:maxlevel), 2 by default."
  (let ((m (getf params :maxlevel))) (if (integerp m) m 2)))

(defun clocktable-properties (params)
  "The property names a clock table of PARAMS has columns for (:properties)."
  (let ((value (getf params :properties)))
    (cond ((listp value) (remove-if-not #'stringp value))
          ((and (stringp value) (not (equalp value "nil"))) (list value))
          (t '()))))

(defun clocktable-percent (part whole)
  "PART of WHOLE in hundredths, written with one decimal: the double-precision
value, rounded to the nearest, halves to even."
  (let ((tenths (round (* 10 (rational (* 100 (/ (float part 1d0) (float whole 1d0))))))))
    (format nil "~d.~d" (floor tenths 10) (mod tenths 10))))

(defun clocktable-lines (tables params)
  "The lines of a clock table of TABLES (CLOCKTABLE-FILE-TABLE's, one for each
file) before aligning them,
PARAMS its parameters: a column for the file when :MULTIFILE, a level column
\(:level), a timestamp column (:timestamp), a tags column (:tags), the
property columns, :emphasize, :compact, :narrow (40! by default: headlines
cut to 40), :tcolumns, :filetitle, a hundredths column (:formula %)."
  (let* ((multifile (getf params :multifile))
         (maxlevel (clocktable-maxlevel params))
         (emph (clocktable-flag params :emphasize))
         (compact (clocktable-flag params :compact))
         (narrow (let ((n (getf params :narrow)))
                   (cond ((member :narrow params) (if (equalp n "nil") (and compact "40!") n))
                         (t "40!"))))
         (level? (and (not compact) (clocktable-flag params :level)))
         (timestamp (clocktable-flag params :timestamp))
         (tags (clocktable-flag params :tags))
         (percent (equal (getf params :formula) "%"))
         (filetitle (clocktable-flag params :filetitle))
         (properties (clocktable-properties params))
         (indent (or compact (if (member :indent params) (clocktable-flag params :indent) t)))
         (time-columns (if (or compact (< maxlevel 2))
                           1
                           (let ((levels (loop for (nil minutes entries) in tables
                                               when (and (integerp minutes) (/= minutes 0))
                                                 append (mapcar #'first entries)))
                                 (most (getf params :tcolumns)))
                             (min maxlevel (if (integerp most) most 100) (if levels (reduce #'max levels) 1)))))
         (total (reduce #'+ tables :key #'second))
         (cut nil))
    (flet ((bars (n) (make-string (max 0 n) :initial-element #\|)))
      ;; a whole number with a link: cut all the same
      (when (and (integerp narrow) (clocktable-flag params :link))
        (setf narrow (format nil "~d!" narrow)))
      (when (and (stringp narrow) (cl-ppcre:scan "^[0-9]+!$" narrow))
        (setf cut t narrow (parse-integer narrow :junk-allowed t)))
      (with-output-to-string (out)
        (when (and (integerp narrow) (not cut))
          (format out "|~:[~;|~]~:[~;|~]~:[~;|~]~:[~;|~]~a<~d>| |~%" multifile level? timestamp tags (bars (length properties)) narrow))
        ;; the header
        (format out "|~a~a~a~:[~;Tags |~]~{~a|~}~a|~a|~a~:[~;%|~]~%"
                (if multifile (format nil "~a|" (clocktable-word params "File")) "")
                (if level? (format nil "~a|" (clocktable-word params "L")) "")
                (if timestamp (format nil "~a|" (clocktable-word params "Timestamp")) "")
                tags properties (clocktable-word params "Headline") (clocktable-word params "Time")
                (bars (1- time-columns)) percent)
        ;; the total
        (format out "|-~%|~a~:[~;|~]~:[~;|~]~:[~;|~]~a*~a*| *~a*|~a~:[~;~:*~a|~]~%"
                (if multifile (format nil "| ~a " (clocktable-word params "ALL")) "")
                level? timestamp tags (bars (length properties))
                (clocktable-word params "Total time")
                (clocktable-time total) (bars (1- time-columns))
                (and percent (if (plusp total) "100.0" "0.0")))
        (when (plusp total)
          (loop for (file minutes entries) in tables
                when (or (plusp minutes) (not (clocktable-flag params :fileskip0)))
                  do (format out "|-~%")
                     (when multifile
                       (format out "| ~a ~:[~;| ~]| ~:[~;| ~]~:[~;| ~]~a*~a* | *~a*|~a~:[~;~:* ~a |~]~%"
                               (or (and filetitle (clocktable-file-title file)) (if file (tuatara.io:native-file-name file) ""))
                               level? timestamp tags
                               (bars (length properties)) (clocktable-word params "File time")
                               (clocktable-time minutes) (bars (1- time-columns))
                               (and percent (clocktable-percent minutes total))))
                     (when (> maxlevel 0)
                       (loop for (level headline minutes props entry-tags stamp) in entries
                             do (when cut
                                  (setf headline
                                        (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings "^\\[\\[((?:[^]\\\\]|\\\\.)*)\\]\\[(.*)\\]\\]$" headline))))
                                          (if groups
                                              (format nil "[[~a][~a]]" (aref groups 0) (tuatara.display:shorten-string (aref groups 1) narrow :ellipsis "..."))
                                              (tuatara.display:shorten-string headline narrow :ellipsis "...")))))
                                (flet ((field (text)
                                         (format nil (cond ((not emph) "~a |") ((= level 1) "*~a* |") ((= level 2) "/~a/ |") (t "~a |")) text)))
                                  (format out "|~:[~;|~]~:[~;~:*~d|~]~:[~;~:*~a|~]~:[~;~:*~a|~]~{~a|~}~a~a~a~a~a~:[~;~:*~a |~]~%"
                                          multifile
                                          (and level? level)
                                          (and timestamp (or stamp ""))
                                          (and tags (format nil "~{~a~^, ~}" entry-tags))
                                          (mapcar (lambda (name) (or (cdr (assoc name props :test #'string=)) "")) properties)
                                          (if (and indent (> level 1)) (format nil "\\_~a" (make-string (* 2 (1- level)) :initial-element #\Space)) "")
                                          (field headline)
                                          (bars (1- (min time-columns level)))
                                          (field (clocktable-time minutes))
                                          (bars (- time-columns level))
                                          (and percent (clocktable-percent minutes total))))))))))))

(defun clocktable-file-title (file)
  "The #+TITLE: of the Org file at FILE, as its clock table's :filetitle shows it, or NIL."
  (multiple-value-bind (tree text) (clocktable-file-tree file)
    (declare (ignore tree))
    (and text (org-file-keyword text "TITLE"))))

(defun clocktable-rows-of (lines)
  "The rows of the clock table LINES (CLOCKTABLE-LINES): :RULE for a rule
line, else the cells between the bars, each trimmed."
  (loop for line in (cl-ppcre:split "\\n" (string-right-trim '(#\Newline) lines))
        collect (if (and (> (length line) 1) (char= (char line 1) #\-))
                    :rule
                    (let ((cells (cl-ppcre:split "\\|" (subseq line 1) :limit most-positive-fixnum)))
                      ;; the bar that ends the line ends its last cell
                      (when (and cells (string= (car (last cells)) "")) (setf cells (butlast cells)))
                      (mapcar (lambda (cell) (string-trim " 	" cell)) cells)))))

(defun clocktable-leading-number (text)
  "The number TEXT begins with, read as far as it goes (ORG-CALC-READ-NUMBER,
its exponent held); 0 for none."
  (let ((match (cl-ppcre:scan-to-strings "^[-+]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?" text)))
    (or (and match (ignore-errors (org-calc-value (org-calc-read-number match)))) 0)))

(defun clocktable-sort-key (cell kind)
  "What a clock table's :sort compares CELL by, KIND #\\a (its text without
emphasis marks, case folded), #\\n (the number it begins with) or #\\t (the
universal time of a timestamp in it, else the minutes of a duration, 1d 4:30,
or of the first H:MM in it; 0 for none)."
  (ecase kind
    (#\a (string-downcase (org-plain-text (subseq cell 0 (min (length cell) 4096)))))
    (#\n (clocktable-leading-number cell))
    (#\t (let ((stamp (cl-ppcre:scan-to-strings *org-one-timestamp-scanner* cell)))
           (cond ((and stamp (org-read-ts stamp)) (org-ts-time (org-read-ts stamp)))
                 ((cl-ppcre:scan "^(?:[0-9]+d(?: [0-9]+:[0-9]{2})?|[0-9]+:[0-9]{2})$" cell)
                  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(?:([0-9]+)d)? ?(?:([0-9]+):([0-9]{2}))?$" cell)
                    (declare (ignore match))
                    (flet ((int (i) (if (aref groups i) (parse-integer (aref groups i)) 0)))
                      (+ (* 1440 (int 0)) (* 60 (int 1)) (int 2)))))
                 (t (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings "\\b([0-9]+):([0-9]{2})\\b" cell))))
                      (if groups (+ (* 60 (parse-integer (aref groups 0))) (parse-integer (aref groups 1))) 0))))))))

(defun clocktable-sorted (rows sort)
  "ROWS of a clock table (CLOCKTABLE-ROWS-OF) with the rows about the third
row of cells, between the rules around it, sorted as :sort (COLUMN . KIND)
says: by the cell in COLUMN, KIND ?a alphabetically, ?n as numbers, ?t as
times, a capital the other way round, rows alike kept in their order; as they
are without a third row or a sort it knows."
  (let* ((kind (and (consp sort) (characterp (cdr sort)) (char-downcase (cdr sort))))
         (data (loop for row in rows for i from 0 unless (eq row :rule) collect i))
         (third (nth 2 data)))
    (if (not (and kind (member kind '(#\a #\n #\t)) (integerp (car sort)) third))
        rows
        (let* ((from (1+ (or (position :rule rows :end third :from-end t) -1)))
               (to (or (position :rule rows :start third) (length rows)))
               (column (max 1 (car sort)))
               (less (if (char= kind #\a) #'string< #'<))
               (key (lambda (row) (clocktable-sort-key (or (nth (1- column) row) "") kind)))
               (sorted (stable-sort (copy-list (subseq rows from to))
                                    (if (upper-case-p (cdr sort)) (lambda (a b) (funcall less b a)) less)
                                    :key key)))
          (append (subseq rows 0 from) sorted (subseq rows to))))))

(defun clocktable-text (lines params)
  "The clock table LINES (CLOCKTABLE-LINES) as the block shows them: sorted as
PARAMS' :sort says, their formulas (:formula, a #+TBLFM: line; or the old
block's, kept) worked out and written under them, aligned, each column as wide as its widest cell."
  (let* ((rows (clocktable-sorted (clocktable-rows-of lines) (getf params :sort)))
         (columns (loop for row in rows unless (eq row :rule) maximize (length row)))
         (rows (loop for row in rows
                     collect (if (eq row :rule) row (append row (make-list (- columns (length row)) :initial-element "")))))
         (model (make-org-table-model :indent "" :rows rows :row -1 :field 0 :offset 0 :newline t))
         (formula (getf params :formula))
         ;; a formula of the block's, or the old block's #+TBLFM: line kept, as Org keeps it
         (tblfm (cond ((and (stringp formula) (not (equal formula "%")) (not (equalp formula "nil")))
                       (format nil "#+TBLFM: ~a" formula))
                      ((and (stringp (getf params :content)) (cl-ppcre:scan "\\S" (getf params :content)))
                       (cl-ppcre:scan-to-strings "(?im)^[ \\t]*#\\+tblfm:.*$" (getf params :content))))))
    (when tblfm
      (handler-case (org-apply-formulas model (org-runnable-formulas (org-table-formula-texts (subseq tblfm (1+ (position #\: tblfm))))))
        (org-formula-error (e) (message "The clock table's formulas: ~a" (org-formula-error-text e)))))
    (format nil "~a~@[~a~%~]" (values (org-table-render model)) tblfm)))

(defun org-path-beside-p (path folder &optional agenda)
  "True when the file at PATH (native) stands in FOLDER or under it, every
link and .. in either followed (TUATARA.IO:REALLY-WITHIN-P), or is one of AGENDA, the
agenda's files: what a file may have the editor read when it names a file."
  (or (member path agenda :test #'equal)
      (and folder (tuatara.io:really-within-p path folder))))

(defun path-in-folders-p (path folders)
  "True when the file at PATH (native) stands in one of FOLDERS or under it,
links followed (ORG-PATH-BESIDE-P); FOLDERS are folder names, ~ standing for
your home folder."
  (some (lambda (folder)
          (let ((folder (tuatara.io:native (tuatara.io:expand-path folder))))
            (and folder (org-path-beside-p path folder))))
        folders))

(defun org-archive-files-of (path text &optional (agenda (org-agenda-file-paths)))
  "The archive files of the Org file at PATH (native; TEXT its text):
the file its #+ARCHIVE: setting
names before the ::, %s standing for the file's name, NAME_archive beside it
without one, and the files its :ARCHIVE: properties name; of those, the
regular files that exist, standing in the file's folder or under it or
among AGENDA's files, since a file must not have the editor read elsewhere."
  (let* ((folder (tuatara.io:parent-path path))
         (name (tuatara.io:native-file-name path))
         (locations (cons (or (org-file-keyword text "ARCHIVE") "%s_archive::")
                          (let ((found '()))
                            (cl-ppcre:do-register-groups (value) ("(?im)^[ \\t]*:ARCHIVE:[ \\t]+(.*?)[ \\t]*$" text)
                              (push value found))
                            (nreverse found))))
         (files '()))
    (dolist (location locations (nreverse files))
      (let ((separator (search "::" location)))
        (when separator
          (let ((pattern (string-trim " 	" (subseq location 0 separator))))
            (when (plusp (length pattern))
              (let ((file (ignore-errors
                           (tuatara.io:native
                            (tuatara.io:expand-path (cl-ppcre:regex-replace-all "%s" pattern (lambda (&rest r) (declare (ignore r)) name) :simple-calls t)
                                                    :directory (or folder (tuatara.io:default-directory)))))))
                (when (and file
                           (not (member file files :test #'equal))
                           (eq :regular (tuatara.io:path-kind file))
                           (org-path-beside-p file folder agenda))
                  (push file files))))))))))

(defun clocktable-scope-files (scope buffer)
  "The files a clock table's SCOPE names, as native paths: the agenda's for
agenda, with their archive files for agenda-with-archives; the buffer's own
file with its archives for file-with-archives (ORG-ARCHIVE-FILES-OF); of a
list, each name read from the folder of BUFFER's file (the block's own),
those that are agenda files or stand beside that file (in its folder or
under it, links followed); another is left out and said, since a file must
not have the editor read what it names. NIL for the scopes within the
buffer."
  (cond ((listp scope)
         (let* ((agenda (org-agenda-file-paths))
                (file (tuatara.buffer:buffer-file buffer))
                (folder (and file (tuatara.io:parent-path file))))
           (loop for name in scope
                 when (stringp name)
                   append (let ((path (ignore-errors
                                       (tuatara.io:native (tuatara.io:expand-path name :directory (or folder (tuatara.io:default-directory)))))))
                            (cond ((null path) '())
                                  ((org-path-beside-p path folder agenda) (list path))
                                  (t (message "The clock table leaves ~a out: neither an agenda file nor beside this one" name)
                                     '()))))))
        ((string-equal scope "agenda-with-archives")
         (let ((agenda (org-agenda-file-paths)))
           (remove-duplicates (loop for path in agenda
                                    append (cons path (let ((text (nth-value 1 (clocktable-file-tree path))))
                                                        (and text (org-archive-files-of path text agenda)))))
                              :test #'equal :from-end t)))
        ((string-equal scope "file-with-archives")
         (let ((file (tuatara.buffer:buffer-file buffer)))
           (and file (let ((path (tuatara.io:native file)))
                       (cons path (let ((text (nth-value 1 (clocktable-own-tree buffer))))
                                    (and text (org-archive-files-of path text))))))))
        ((string-equal scope "agenda") (org-agenda-file-paths))
        (t nil)))

(defun clocktable-file-tree (path)
  "The parse of the Org file at PATH as two values, its tree and text, from
its buffer when it is open, else from the disk, read once for a table
\(*CLOCKTABLE-PARSES*); NIL when it cannot be read."
  (memo-values *clocktable-parses* path
               (lambda ()
                 (flet ((parsed (text) (and text (values (org-parse text) text))))
                   (if *dblock-sources*
                       ;; made on a thread of its own: the text its buffer had, parsed here
                       (parsed (dblock-source-text path))
                       (let ((source (org-agenda-file-source path)))
                         (if (typep source 'tuatara.buffer:buffer)
                             (org-tree-now source)
                             (parsed (and source (org-agenda-source-text path source))))))))))

(defun clocktable-own-tree (buffer)
  "The parse of BUFFER, the one holding the block, as two values, read once for a table (*CLOCKTABLE-PARSES*)."
  (memo-values *clocktable-parses* :own (lambda () (if *dblock-own* (values-list *dblock-own*) (org-tree-now buffer)))))

(defun clocktable-week-start (params)
  "The day a clock table's PARAMS begin the week on (:wstart), Sunday 0 to Saturday 6; Monday without one."
  (let ((w (getf params :wstart))) (if (integerp w) (mod w 7) 1)))

(defun clocktable-month-start (params)
  "The day a clock table's PARAMS begin the month on (:mstart), 1 to 31; the first without one."
  (let ((m (getf params :mstart))) (if (integerp m) (max 1 (min 31 m)) 1)))

(defun clocktable-block-of (params now)
  "The time the :block of a clock table's PARAMS covers, as CLOCKTABLE-BLOCK
gives it, the week and the month beginning as :wstart and :mstart say."
  (clocktable-block (getf params :block) now :wstart (clocktable-week-start params) :mstart (clocktable-month-start params)))

(defparameter *clocktable-most-steps* 1000
  "The most tables a stepped clock table writes: steps past that are left out, and said to be.")

(defun clocktable-param-time (value)
  "The universal time a clock table's :tstart or :tend VALUE names, a
timestamp as written; NIL for none."
  (let ((ts (and (stringp value) (org-read-ts value))))
    (and ts (org-ts-time ts))))

(defun clocktable-one (params)
  "One clock table of PARAMS, as two values: its text (its caption, or
:header's text instead, then the table) and its minutes."
  (let* ((now (org-now))
         (scope-param (or (getf params :scope) "file"))
         (scope (if (listp scope-param) scope-param (string-downcase (princ-to-string scope-param))))
         (block (getf params :block)))
    (multiple-value-bind (bstart bend words) (if block (clocktable-block-of params now) (values nil nil nil))
      (when (and block (null words))
        (message "The clock table's :block ~a is not one Org knows" block)
        (return-from clocktable-one (values (or (getf params :content) "") 0)))
      ;; the block's time when there is one, else the times given
      (let* ((tstart (if block bstart (clocktable-param-time (getf params :tstart))))
             (tend (if block bend (clocktable-param-time (getf params :tend))))
             (buffer (current-buffer))
             (files (memo-values *clocktable-parses* (cons :files scope) (lambda () (clocktable-scope-files scope buffer))))
             (tables
               (if files
                   (loop for path in files
                         collect (multiple-value-bind (tree text) (clocktable-file-tree path)
                                   (if tree
                                       (let ((*org-current-text* text))
                                         (clocktable-file-table tree text path params tstart tend :link-path path))
                                       (list path 0 '()))))
                   (multiple-value-bind (tree text) (clocktable-own-tree buffer)
                     (let ((root (and (member scope '("subtree" "tree") :test #'equal)
                                      (let ((here (org-current-headline tree (or *dblock-position* (org-point-char buffer)))))
                                        (if (string= scope "tree")
                                            (loop for h = here then (org-node-parent h)
                                                  while (and h (eq (org-node-type (org-node-parent h)) 'headline))
                                                  finally (return h))
                                            here))))
                           (file (tuatara.buffer:buffer-file buffer)))
                       (list (clocktable-file-table tree text (and file (tuatara.io:native file)) params tstart tend
                                                    :root root :link-path (and file (tuatara.io:native file))))))))
             ;; a file with its archives is one file to the table
             (multifile (and files (not (clocktable-flag params :hidefiles))
                             (not (and (stringp scope) (string-equal scope "file-with-archives")))))
             (header (getf params :header))
             (caption (if (and (stringp header) (not (equalp header "nil")))
                          header
                          (format nil "#+CAPTION: ~a ~a~@[, for ~a.~]~%" (clocktable-word params "Clock summary at")
                                  (org-timestamp-text now) words)))
             (formatter (let ((name (getf params :formatter)))
                          (and (stringp name) (not (equalp name "nil"))
                               (or (gethash name *clocktable-formatters*)
                                   (progn (message "No clock table formatter is named ~a: define-clocktable-formatter makes one" name)
                                          nil))))))
        (values (if formatter
                    (funcall formatter tables (list* :multifile multifile params))
                    (concatenate 'string caption
                                 (clocktable-text (clocktable-lines tables (list* :multifile multifile params)) params)))
                (reduce #'+ tables :key #'second))))))

(defun clocktable-step-next (start step wstart mstart)
  "When the clock table step after the one beginning at the universal time
START begins: STEP day, the next day;
week, the next day WSTART of the week (Monday 1); semimonth, the 16th or the
next month's first; month, the next month's day MSTART; quarter, three months
on; year, the next year's first day."
  (let ((day (time-day start)))
    (multiple-value-bind (y m d) (day-date day)
      (flet ((at (day month year) (day-universal-time (day-number year month day))))
        (cond ((string-equal step "day") (at (1+ d) m y))
              ((string-equal step "week")
               (let ((dow (weekday day)))
                 (at (+ d (if (= dow wstart) 7 (mod (- wstart dow) 7))) m y)))
              ((string-equal step "semimonth") (if (< d 16) (at 16 m y) (at 1 (1+ m) y)))
              ((string-equal step "month") (at mstart (1+ m) y))
              ((string-equal step "quarter") (at mstart (+ 3 m) y))
              (t (at 1 1 (1+ y))))))))

(defun clocktable-day-stamp (time)
  "The day of the universal TIME as an inactive timestamp without a time: [2026-09-28 Mon]."
  (format-time "[%F %a]" time))

(defun clocktable-steps (params)
  "The clock tables of PARAMS one for each :step (day, week, semimonth, month,
quarter, year) of its :block, or from its :tstart to its :tend: each under a
line saying its step and
first day, those with no time left out with :stepskip0 (an empty line when
all are, and after the last one written when those after it are); the
block's old text when it has neither a block nor both times."
  (let* ((step (string-downcase (princ-to-string (getf params :step))))
         (title (cond ((string= step "day") "Daily report: ")
                      ((string= step "week") "Weekly report starting on: ")
                      ((string= step "semimonth") "Semimonthly report starting on: ")
                      ((string= step "month") "Monthly report starting on: ")
                      ((string= step "quarter") "Quarterly report starting on: ")
                      ((string= step "year") "Annual report starting on: ")))
         (wstart (clocktable-week-start params))
         (mstart (clocktable-month-start params))
         (block (getf params :block))
         (skip0 (clocktable-flag params :stepskip0)))
    (multiple-value-bind (bstart bend words) (if block (clocktable-block-of params (org-now)) (values nil nil nil))
      ;; without a start, from the year Org was born, as Org has it
      (let ((start (or (and block bstart) (clocktable-param-time (getf params :tstart)) (day-universal-time (day-number 2003 1 1))))
            (end (if block bend (clocktable-param-time (getf params :tend)))))
        (cond ((null title)
               (message "The clock table's :step ~a is not one Org knows: day, week, semimonth, month, quarter or year" step)
               (or (getf params :content) ""))
              ((and block (null words))
               (message "The clock table's :block ~a is not one Org knows" block)
               (or (getf params :content) ""))
              ((null end)
               (message "A clock table's :step needs a :block, or a :tend")
               (or (getf params :content) ""))
              (t
               (let* ((last-skipped nil)
                      (written (with-output-to-string (out)
                                (loop for made from 0
                                      while (< start end)
                                      do (when (>= made *clocktable-most-steps*)
                                           (message "The clock table stops at ~d steps" *clocktable-most-steps*)
                                           (return))
                                         (let ((next (clocktable-step-next start step wstart mstart)))
                                           ;; a step that gets nowhere would go on for ever
                                           (when (<= next start)
                                             (message "The clock table's steps go nowhere from ~a" (clocktable-day-stamp start))
                                             (return))
                                           (multiple-value-bind (text total)
                                               (clocktable-one (list* :header "" :step nil :block nil
                                                                      :tstart (org-timestamp-text start) :tend (org-timestamp-text (min end next))
                                                                      params))
                                             (setf last-skipped (and skip0 (zerop total)))
                                             (unless last-skipped
                                               (format out "~%~a~a~%~a" title (clocktable-day-stamp start) text)))
                                           (setf start next))))))
                 (cond ((zerop (length written)) (string #\Newline))
                       (last-skipped (concatenate 'string written (string #\Newline)))
                       (t written)))))))))

(define-dblock-writer "clocktable" (params)
  ;; each file read and parsed once, each headline's clocks read once, for the table and all its steps
  (let ((*clocktable-parses* (make-hash-table :test 'equal))
        (*clocktable-clocks* (make-hash-table :test 'eq)))
    (if (clocktable-flag params :step)
        (clocktable-steps params)
        (values (clocktable-one params)))))
