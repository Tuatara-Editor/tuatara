;;;; lisp/org/parse.lisp --- An Org buffer's parse, kept up in the background and read.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A buffer's whole parse is made of its parts' parses (parts.lisp) when a
;;;; command asks for it (ORG-BUFFER-PARSE, ORG-PARSE-NOW), by a worker
;;;; thread for a large buffer while the editor goes on. A parse is of the
;;;; text as Org reads it, a line's carriage return before its newline left
;;;; out; positions in it are turned into the buffer's and back here, and
;;;; ORG-ASK-KEEPING-PLACES asks a question while keeping places of a parse
;;;; where they were.

(in-package #:tuatara.defaults)

;;; The parse of a buffer, kept up

(defun org-strip-returns (string)
  "STRING with the carriage return before each newline left out, as a note whose
lines end with carriage return and line feed (CR LF) is read, and each carriage
return that ends a line alone read as a newline. Two values: the text and the
places in it of the newlines that lost one (a vector in order), or NIL when none
did."
  (if (not (find #\Return string))
      (values string nil)
      (let ((out (make-string (length string)))
            (returns (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
            (j 0) (n (length string)))
        (dotimes (i n)
          (let ((c (char string i)))
            (cond ((and (char= c #\Return) (< (1+ i) n) (char= (char string (1+ i)) #\Newline))
                   (vector-push-extend j returns))
                  ((char= c #\Return) (setf (char out j) #\Newline) (incf j))
                  (t (setf (char out j) c) (incf j)))))
        (values (subseq out 0 j)
                (and (plusp (length returns)) (coerce returns '(simple-array fixnum (*))))))))

(defun lone-returns-as-newlines (string)
  "STRING with each carriage return that no newline follows, which ends a
line, made a newline: as long as STRING, every character where it was."
  (if (not (find #\Return string))
      string
      (let ((out (copy-seq string)))
        (dotimes (i (length out) out)
          (when (and (char= (char out i) #\Return)
                     (not (and (< (1+ i) (length out)) (char= (char out (1+ i)) #\Newline))))
            (setf (char out i) #\Newline))))))

(defun org-text-as-read (raw crlf)
  "RAW, text of an Org buffer, as Org reads it, two values: the text and where
it lost a carriage return before a newline (ORG-STRIP-RETURNS), for carriage
return and line feed (CRLF) true, a buffer whose lines end with both; else a
carriage return that ends a line alone read as a newline
(LONE-RETURNS-AS-NEWLINES)."
  (if crlf (org-strip-returns raw) (values (lone-returns-as-newlines raw) nil)))

(defun org-buffer-text (buffer &key (start 0) (end (tuatara.buffer:buffer-size buffer)))
  "The text Org reads of BUFFER from octet START below END, all of it unless
they are given: in a buffer whose lines end with a carriage return and a
newline, each return before a newline left out; a carriage return that ends
a line alone read as a newline."
  (values (org-text-as-read (tuatara.buffer:buffer-string buffer :start start :end end) (buffer-crlf-p buffer))))

(defun org-checkbox-line-p (line)
  "True when LINE, the start of a line of Org text, begins a list item with a
checkbox: - [ ], 1. [X], + [-]."
  (and (cl-ppcre:scan *org-checkbox-line-scanner* line) t))

(defun org-returns-in-rope (rope &key (start 0) (end (tuatara.text:rope-octet-length rope)))
  "Where the text Org reads of ROPE from the octet START below END (all of it
unless they are given), the carriage returns before its newlines left out,
lost one: as ORG-STRIP-RETURNS gives them, found in one pass over the
octets, no copy of the text made; NIL when none did."
  (let ((returns (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (chars 0)
        (pending nil))                  ; a carriage return ending a chunk, its newline yet to come
    (loop for chunk from start below end by 1048576
          do (let ((octets (tuatara.text:rope-octets rope :start chunk :end (min end (+ chunk 1048576)))))
               (declare (type (simple-array (unsigned-byte 8) (*)) octets))
               (loop for octet across octets
                     for i from 0
                     do (when pending
                          (setf pending nil)
                          (if (= octet 10) (vector-push-extend chars returns) (incf chars)))
                        (cond ((= octet 13)
                               (if (< (1+ i) (length octets))
                                   (if (= (aref octets (1+ i)) 10) (vector-push-extend chars returns) (incf chars))
                                   (setf pending t)))
                              ((/= (logand octet #xC0) #x80) (incf chars))))))
    (when pending (incf chars))
    (and (plusp (length returns)) (coerce returns '(simple-array fixnum (*))))))

(defun org-returns-of (buffer)
  "Where the text Org reads of BUFFER lost a carriage return before a newline
(ORG-STRIP-RETURNS): its parse's, while that is of the text as it is now,
else put together from its parts' (ORG-PARTS-RETURNS), once for each
change; NIL for a buffer whose lines end with a newline alone."
  (let ((state (org-buffer-state buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (cond ((not (buffer-crlf-p buffer)) nil)
          ((eql (org-state-version state) version) (org-state-returns state))
          (t (let ((kept (tuatara.buffer:buffer-local buffer 'org-fresh-returns)))
               (if (and kept (eql (car kept) version))
                   (cdr kept)
                   (let ((returns (org-parts-returns buffer)))
                     (setf (tuatara.buffer:buffer-local buffer 'org-fresh-returns) (cons version returns))
                     returns)))))))

(defun org-returns-before (returns position)
  "How many of RETURNS (ORG-STRIP-RETURNS) are before POSITION."
  (tuatara.runtime:bisect (mid 0 (length returns)) (< (aref returns mid) position)))

(defun org-raw-char->char (returns char)
  "The character of a text Org reads at the character CHAR of the raw text it
was read from, which lost RETURNS (ORG-STRIP-RETURNS): the carriage returns
before it left out, one itself taken as its newline."
  (if (null returns)
      char
      ;; the Ith return left out stood at the character (+ (aref returns i) i) of the raw text
      (- char (tuatara.runtime:bisect (mid 0 (length returns)) (< (+ (aref returns mid) mid) char)))))

(defun org-char->byte (buffer position returns &optional (base 0))
  "The octet of BUFFER where the character POSITION of the text Org reads of
it is: the carriage returns it left out (RETURNS) counted back in, a position
at a line's end standing before its carriage return and line feed (CR LF). For
a text read from a part of BUFFER, BASE is the character of BUFFER the part
begins at, RETURNS those the part's text left out."
  (tuatara.text:char->byte (tuatara.buffer:buffer-text buffer)
                           (+ base (if returns (+ position (org-returns-before returns position)) position))))

(defun org-byte->char (buffer byte returns &optional (base 0))
  "The character of the text Org reads of BUFFER at its octet BYTE: the
carriage returns before it (RETURNS) left out, one itself taken as its
newline. For a text read from a part of BUFFER, BASE is the character of
BUFFER the part begins at, RETURNS those the part's text left out."
  (org-raw-char->char returns (- (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) byte) base)))

(defun org-buffer-state (buffer)
  "BUFFER's ORG-STATE, made the first time."
  (or (tuatara.buffer:buffer-local buffer 'org-state)
      (setf (tuatara.buffer:buffer-local buffer 'org-state) (make-org-state))))

(defun org-job-text (job)
  "The text JOB, as ORG-WANT-PARSE asks for it, has parsed, as Org reads it:
two values, the text and where it lost a carriage return before a newline
\(ORG-STRIP-RETURNS)."
  (destructuring-bind (version source crlf keywords &optional secret) job
    (declare (ignore version keywords secret))
    (if (org-snapshot-p source)
        (org-snapshot-text source)
        (let ((raw (if (stringp source)
                       source
                       (destructuring-bind (rope start end) source
                         (tuatara.text:rope-string rope :start start :end end)))))
          (org-text-as-read raw crlf)))))

(defun org-unparsed (text returns)
  "What stands for the parse of TEXT, which lost RETURNS (ORG-STRIP-RETURNS),
when the parser cannot read it, as ORG-WORKER-PARSE gives a parse: TEXT read
as holding nothing Org shows (ORG-EMPTY-TREE)."
  (list (org-empty-tree (length text)) text returns nil))

(defun org-worker-parse (job)
  "Parse what JOB, as ORG-WANT-PARSE asks for it, holds: (TREE TEXT RETURNS
PLACES), PLACES where the parts are in a whole buffer's parse (ORG-ASSEMBLE).
For a text the parser cannot read, the text read as holding nothing
\(ORG-UNPARSED), what went wrong said in the debug log and given as a
second value; NIL for a text that cannot be read at all. The text of a
buffer kept secret (its fifth, true) leaves only the kind of failure said."
  (let ((source (second job))
        (tuatara.runtime:*secret-at-hand* (or tuatara.runtime:*secret-at-hand* (fifth job))))
    (handler-case
        (if (org-snapshot-p source)
            (multiple-value-list (org-assemble source))
            (multiple-value-bind (text returns) (org-job-text job)
              (list (org-parse text :todo-keywords (fourth job)) text returns nil)))
      (error (condition)
        (let ((failed (tuatara.runtime:failure-words condition)))
          (tuatara.runtime:debug-report
           :parse "the Org text at version ~a was not parsed, shown plain until it changes: ~a"
           (org-version-number (first job)) failed)
          (values (ignoring-errors-reported ("reading the Org text to show it plain")
                    (multiple-value-call #'org-unparsed (org-job-text job)))
                  failed))))))

(defun org-parse-worker (state editor)
  "Parse in the background what STATE wants, the latest asked for, again as
long as something newer is asked for meanwhile; each parse put in STATE's
RESULT in one step, EDITOR, when given, woken for it."
  (unwind-protect
       (loop
         (let* ((job (sb-thread:with-mutex ((org-state-lock state)) (org-state-wanted state)))
                (start (get-internal-real-time)))
           (multiple-value-bind (parse failed) (org-worker-parse job)
             (setf (org-state-parse-seconds state) (tuatara.runtime:seconds-since start))
             ;; the result put in whatever came of it: a text the parser cannot read is not asked for again and again
             (destructuring-bind (&optional tree text returns places) parse
               (setf (org-state-result state)
                     (list tree text (first job) returns (and tree (org-src-language-names tree))
                           (and places (cons (second job) places)) failed))
               (when editor (wake-editor editor))))
           (sb-thread:with-mutex ((org-state-lock state))
             (sb-thread:condition-broadcast (org-state-done state))
             (when (eq job (org-state-wanted state))
               (setf (org-state-wanted state) nil
                     (org-state-worker state) nil)
               (return)))))
    ;; ended however it ended: the next parse asked for starts a worker of its own
    (sb-thread:with-mutex ((org-state-lock state))
      (when (eq (org-state-worker state) sb-thread:*current-thread*)
        (setf (org-state-worker state) nil)))))

(defun org-want-parse (buffer state version source &key todo-keywords)
  "Have BUFFER's text at VERSION parsed in the background into STATE: SOURCE,
a function of no arguments called when the job is made, gives what is
parsed, an ORG-SNAPSHOT of its parts for the whole buffer (ORG-ASSEMBLE) or
the octets (START . END) of a part, parsed with TODO-KEYWORDS. The worker
running takes it next, or one is started. A buffer of a file mapped from
disk gives the worker a copy of a part's text, made when a worker starts,
since its mapping may go with the buffer; another gives its text as it is,
which never changes."
  (let ((mapped (tuatara.buffer:buffer-mapping buffer)))
    (flet ((job ()
             (let ((what (funcall source)))
               (list version
                     (cond ((org-snapshot-p what) what)
                           (mapped (tuatara.buffer:buffer-string buffer :start (car what) :end (cdr what)))
                           (t (list (tuatara.buffer:buffer-text buffer) (car what) (cdr what))))
                     (buffer-crlf-p buffer) todo-keywords (tuatara.runtime:secret-buffer-p buffer)))))
      (sb-thread:with-mutex ((org-state-lock state))
        (unless (eql (car (org-state-wanted state)) version)
          (cond ((let ((worker (org-state-worker state))) (and worker (sb-thread:thread-alive-p worker)))
                 (unless mapped
                   (setf (org-state-wanted state) (job))))
                (t
                 (setf (org-state-wanted state) (job)
                       (org-state-worker state)
                       (let ((editor *editor*))
                         (tuatara.runtime:make-guarded-thread (lambda () (org-parse-worker state editor))
                                                              :name "tuatara org parse"))))))))))

(defun org-src-language-names (tree)
  "The languages the source blocks of TREE name, each once, as written."
  (let ((names '()))
    (org-walk-tree tree (lambda (node)
                          (when (and (eq (org-node-type node) 'src-block) (org-property node :language))
                            (pushnew (org-property node :language) names :test #'string-equal))))
    names))

(defun org-warm-src-languages (names)
  "Have the colouring rules of the languages of NAMES (source blocks' languages
as written) compiled in the background, those not compiled yet: each block's
code is coloured as soon as it is shown, the note redrawn as each is ready."
  (dolist (name names)
    (let ((language (ignoring-errors-reported ("finding the language ~a" name) (org-src-language name))))
      (when (and language (not (rules-ready-p language)))
        (warm-rules language)))))

(defun org-version-number (version)
  "VERSION, a buffer's version or the number of one, as a number that grows with each change."
  (if (integerp version) version (tuatara.text:version-number version)))

(defun org-take-parse (state)
  "Take into STATE the parse its worker finished last, when it is newer than STATE's own."
  (let ((result (org-state-result state)))
    (when (and result (> (org-version-number (third result)) (org-version-number (org-state-version state))))
      (destructuring-bind (tree text parsed returns names made failed) result
        (setf (org-state-tree state) tree
              (org-state-text state) text
              (org-state-returns state) returns
              (org-state-version state) parsed
              (org-state-failed state) failed)
        (when made
          (org-keep-whole-places (car made) tree (cdr made))
          ;; the parts are let go of with it, once taken
          (setf (nth 5 result) nil))
        (org-warm-src-languages names)))))

(defun org-await-parse (state version)
  "Wait at most *ORG-PARSE-WAIT* seconds for STATE's worker to have parsed the
text at VERSION, once for each VERSION; true when it has. A buffer whose last
parse took longer than that is not waited for: typing never waits on it, its
colours coming with the parse."
  (unless (or (eql (org-state-waited state) version)
              (let ((took (org-state-parse-seconds state))) (and took (> took *org-parse-wait*))))
    (setf (org-state-waited state) version)
    (let ((deadline (tuatara.runtime:deadline-in *org-parse-wait*)))
      (sb-thread:with-mutex ((org-state-lock state))
        (loop for result = (org-state-result state)
              until (and result (>= (org-version-number (third result)) (org-version-number version)))
              do (let ((left (tuatara.runtime:seconds-left deadline)))
                   (when (<= left 0) (return nil))
                   (tuatara.runtime:wait-on (org-state-done state) (org-state-lock state) left))
              finally (return t))))))

(defun org-buffer-parse (buffer)
  "BUFFER's ORG-PARSE, of its text as it is now or a little before. Made of
the parses of BUFFER's parts (ORG-ASSEMBLE), each part parsed again only
after a change within it, when BUFFER changed: at once while it is small
\(*ORG-PARSE-AT-ONCE*), else in the background, the latest text first,
waited for a moment (*ORG-PARSE-WAIT*) and the parse before returned until
that one is done; NIL while there has been none. A text the parser cannot
read is read as holding nothing (ORG-UNPARSED) until it changes, what went
wrong kept in the ORG-STATE's FAILED and said in the debug log. ORG-PARSE-NOW
gives the parse of the text as it is now."
  (let ((state (org-buffer-state buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (org-take-parse state)
    (unless (eql version (org-state-version state))
      (let ((parts (org-buffer-parts buffer)))
        (if (<= (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
            (let ((snapshot (org-parts-snapshot buffer parts)))
              (multiple-value-bind (parse failed)
                  (org-worker-parse (list version snapshot (buffer-crlf-p buffer) nil
                                          (tuatara.runtime:secret-buffer-p buffer)))
                (destructuring-bind (&optional tree text returns places) parse
                  (setf (org-state-tree state) tree
                        (org-state-text state) text
                        (org-state-returns state) returns
                        (org-state-version state) version
                        (org-state-failed state) failed)
                  (when tree
                    (org-keep-whole-places snapshot tree places)
                    (org-warm-src-languages (org-src-language-names tree))))))
            (progn
              (org-want-parse buffer state version (lambda () (org-parts-snapshot buffer parts)))
              (when (org-await-parse state version)
                (org-take-parse state))))))
    (when (and (org-state-failed state) (null (org-state-tree state)))
      ;; not even its text could be read where it was parsed: read as holding nothing, as it is now
      (destructuring-bind (tree text returns places)
          (multiple-value-call #'org-unparsed
            (let ((raw (tuatara.buffer:buffer-string buffer)))
              (org-text-as-read raw (buffer-crlf-p buffer))))
        (declare (ignore places))
        (setf (org-state-tree state) tree
              (org-state-text state) text
              (org-state-returns state) returns
              (org-state-version state) version)))
    (org-state-parse-of buffer state)))

(defun org-parse-current-p (buffer)
  "True when BUFFER's parse is of its text as it is now."
  (eql (org-state-version (org-buffer-state buffer)) (tuatara.buffer:buffer-version buffer)))

(defun org-parse-now (buffer)
  "BUFFER's ORG-PARSE of its text as it is now, NIL when it has no Org
structure: a small buffer parsed at once (*ORG-PARSE-AT-ONCE*); a large one
whose parse is behind its text waited for while it is parsed in the
background, the editor reading keys meanwhile (\\`C-g' gives the command up)."
  (let ((state (org-buffer-state buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (if (or (<= (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
            (org-parse-current-p buffer)
            (null *editor*))
        (let ((*org-parse-at-once* most-positive-fixnum))
          (org-buffer-parse buffer))
        (progn
          ;; asked for: the worker parses it, the parse before taken meanwhile
          (org-buffer-parse buffer)
          (flet ((ready-p ()
                   (let ((result (org-state-result state)))
                     (and result (>= (org-version-number (third result)) (org-version-number version))))))
            (unless (or (ready-p) (wait-until-interruptibly #'ready-p :message "Reading the file..."))
              (keyboard-quit-signal)))
          (org-buffer-parse buffer)))))

;;; A parse and the buffer it is of

(define-record (org-parse (:constructor make-org-parse (buffer version root text returns)) (:copier nil)
                          (:predicate nil))
  "The parse of an Org buffer, as ORG-BUFFER-PARSE and ORG-PARSE-NOW give it:
its tree of elements and objects (ORG-PARSE-TREE), whose positions are
characters of TEXT, the text BUFFER had at VERSION as Org reads it. A position
of the parse goes to an octet of BUFFER by ORG-PARSE-OCTET and back by
ORG-PARSE-CHAR, RETURNS theirs to use. While BUFFER is at VERSION they count
by the parse's own text; once it has changed, by the text it has now, so that
a position given after an edit is one of the text after it. A command that
asks something keeps its places across the answer by ORG-ASK-KEEPING-PLACES.
ORG-TEXT-NOW makes one with no tree, of the text alone. KEYWORDS, its TODO
keywords once ORG-PARSE-TODO-KEYWORDS has found them."
  (buffer nil :documentation "The buffer ORG-PARSE is of.")
  (version nil :documentation "The version ORG-PARSE's buffer had when its text was read.")
  (root nil :documentation "The root of the tree of ORG-PARSE, or NIL when it has no tree.")
  (text nil :documentation "The text of ORG-PARSE's buffer at its version, as Org reads it.")
  (returns nil
           :documentation "Where the text Org read lost a carriage return before a newline, for ORG-PARSE's own
version.")
  (keywords nil
            :documentation "The TODO keywords of ORG-PARSE once
ORG-PARSE-TODO-KEYWORDS has found them, or NIL before."))

(defun org-parse-tree (parse)
  "The tree of PARSE, its root an ORG-NODE of type ORG-DATA over all of its
text. A parse of the text alone (ORG-TEXT-NOW) has none, and is refused."
  (or (org-parse-root parse)
      (error "The text of ~a was read, not parsed: it has no tree"
             (tuatara.buffer:buffer-name (org-parse-buffer parse)))))

(defun org-state-parse-of (buffer state)
  "The ORG-PARSE of STATE, BUFFER's whole parse, made once for each tree; NIL
while it has none."
  (let ((tree (org-state-tree state))
        (kept (org-state-parse state)))
    (cond ((null tree) nil)
          ((and kept (eq (org-parse-root kept) tree) (eql (org-parse-version kept) (org-state-version state))) kept)
          (t (setf (org-state-parse state)
                   (make-org-parse buffer (org-state-version state) tree
                                   (org-state-text state) (org-state-returns state)))))))

(defun org-parse-todo-keywords (parse)
  "The TODO keywords of PARSE's text, as ORG-TODO-KEYWORDS-OF gives them:
those its buffer's parts keep (ORG-BUFFER-PARTS) while they are of the text
PARSE is of, else read from that text; found once for each parse."
  (or (org-parse-keywords parse)
      (setf (org-parse-keywords parse)
            (let ((parts (tuatara.buffer:buffer-local (org-parse-buffer parse) 'org-parts)))
              (or (and parts (eq (org-parts-version parts) (org-parse-version parse)) (org-parts-todo-keywords parts))
                  (org-todo-keywords-of (org-parse-text parse)))))))

(defun org-text-now (buffer)
  "BUFFER's text as Org reads it now (ORG-BUFFER-TEXT), as an ORG-PARSE with
no tree, for a command that reads the text without parsing it: its
positions go to the buffer's octets as a parse's do, and ORG-PARSE-TREE
refuses it."
  (make-org-parse buffer (tuatara.buffer:buffer-version buffer) nil (org-buffer-text buffer) (org-returns-of buffer)))

(defun org-parse-returns-now (parse)
  "Where the text Org reads of PARSE's buffer, as it is now, lost a carriage
return before a newline: PARSE's own while the buffer is as it was parsed."
  (let ((buffer (org-parse-buffer parse)))
    (if (eql (org-parse-version parse) (tuatara.buffer:buffer-version buffer))
        (org-parse-returns parse)
        (org-returns-of buffer))))

(defun org-parse-octet (parse position)
  "The octet of PARSE's buffer, as it is now, where the character POSITION of
PARSE's text is; a position at a line's end stands before its carriage
return and line feed. After an edit, POSITION is a character of the text
after it."
  (org-char->byte (org-parse-buffer parse) position (org-parse-returns-now parse)))

(defun org-parse-char (parse octet)
  "The character of PARSE's text at OCTET of its buffer, as it is now: a
carriage return before a newline taken as that newline."
  (org-byte->char (org-parse-buffer parse) octet (org-parse-returns-now parse)))

(defun org-parse-point (parse)
  "The point of PARSE's buffer as a character of PARSE's text."
  (let ((buffer (org-parse-buffer parse)))
    (org-parse-char parse (tuatara.buffer:buffer-point buffer))))

(defun org-ask-keeping-places (parse places ask)
  "Ask by calling ASK, a function of no arguments that waits for an answer (a
prompt, a key), PARSE's buffer free to change meanwhile, as a job's result
taken in or another window's typing changes it. PLACES are what the command
found before asking, each the characters (FROM . TO) of PARSE that hold it: a
headline's line, a link, a table. Three values: the answer; PARSE itself while
the buffer is as it was, else the buffer's parse as it is after (ORG-PARSE-NOW);
and for each place, where it begins in that parse, or NIL when its text is not
there as it was (taken away or changed meanwhile). A command finds what it
asked about again at these places, and refuses where one is NIL."
  (let* ((buffer (org-parse-buffer parse))
         (text (org-parse-text parse))
         (version (tuatara.buffer:buffer-version buffer))
         (anchors (mapcar (lambda (place)
                            (cons (tuatara.text:make-anchor version (org-parse-octet parse (car place)) :bias :after)
                                  (tuatara.text:make-anchor version (org-parse-octet parse (cdr place)) :bias :before)))
                          places))
         (answer (funcall ask))
         (now (tuatara.buffer:buffer-version buffer)))
    (if (and (eql now version) (eql now (org-parse-version parse)))
        (values answer parse (mapcar #'car places))
        (let* ((after (org-parse-now buffer))
               (after-text (org-parse-text after)))
          (values answer after
                  (loop for (from-anchor . to-anchor) in anchors
                        for (old-from . old-to) in places
                        collect (let ((from (org-parse-char after (tuatara.text:anchor-position from-anchor now)))
                                      (to (org-parse-char after (tuatara.text:anchor-position to-anchor now))))
                                  (and (= (- to from) (- old-to old-from))
                                       (string= after-text text :start1 from :end1 to :start2 old-from :end2 old-to)
                                       from))))))))
