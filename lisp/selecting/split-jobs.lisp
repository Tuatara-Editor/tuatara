;;;; lisp/selecting/split-jobs.lisp --- Split: searching a large text and writing out of sight, in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Over a large stretch a split's search runs in the background, cut into
;;;; pieces searched on all but two cores at once, and the split stands when it
;;;; ends. Past its limit a split holds only the places in sight; what the verb
;;;; did at those is written at the others by a job of its own once it is done,
;;;; and the buffer is not saved before that job is.

(in-package #:tuatara.defaults)

;;; Searching a large text in the background

(defun scope-extent (scope)
  "The octets SCOPE, a list of (START . END) stretches or a BLOCK-SCOPE, spans."
  (if (block-scope-p scope)
      (- (block-scope-high scope) (block-scope-top scope))
      (loop for (start . end) in scope sum (- end start))))

(defun scope-positions (scope)
  "What SCOPE, a list of (START . END) stretches or a BLOCK-SCOPE, reads its
text by, as (OFFSET . BIAS) for anchors to follow while a job over it waits."
  (if (block-scope-p scope)
      (list (cons (block-scope-top scope) :before) (cons (block-scope-high scope) :after))
      (loop for (start . end) in scope collect (cons start :before) collect (cons end :after))))

(defun scope-at (scope positions)
  "SCOPE as its SCOPE-POSITIONS stand now, at POSITIONS."
  (if (block-scope-p scope)
      (make-block-scope (first positions) (second positions) (block-scope-left scope) (block-scope-right scope)
                        (block-scope-wide scope) (block-scope-crlf scope) (block-scope-tab-width scope))
      (loop for (start end) on positions by #'cddr collect (cons start end))))

(defun scope-shade (buffer scope &key matcher)
  "The SHADE of a job over SCOPE of BUFFER waiting its turn: a block's columns
row by row, or its stretches; with MATCHER, only the matches of it in them as
the text stands now (MATCHER-SHADING)."
  (cond ((block-scope-p scope)
         (let ((rows (block-shade buffer (block-scope-left scope) (block-scope-right scope)
                                  :tab-width (block-scope-tab-width scope) :wide (block-scope-wide scope))))
           ;; a block reaches past its last row's end; its rows are the lines starting before that
           (lambda (positions from to)
             (funcall rows (list (first positions) (max (first positions) (1- (second positions)))) from to))))
        (matcher
         (lambda (positions from to)
           (let ((stretches (loop for (start end) in (ranges-shade positions from to) collect (cons start end))))
             (when stretches
               (matcher-shading (tuatara.buffer:buffer-text buffer) matcher stretches t)))))
        (t #'ranges-shade)))

(defun scope-groups (rope scope)
  "SCOPE, a list of (START . END) stretches or a BLOCK-SCOPE, cut into pieces for
the workers, as a list of (STRETCH . PIECES) in order: a stretch's pieces end
at line starts (REWRITE-PIECES-FOR); between two stretches, a piece of the
text left alone, its stretch NIL, so that the pieces cover what the first
stretch starts to what the last ends."
  (if (block-scope-p scope)
      (let ((stretch (cons (block-scope-top scope) (block-scope-high scope))))
        (list (cons stretch (coerce (rewrite-pieces-for rope (car stretch) (cdr stretch)) 'list))))
      (let ((groups '()) (at nil))
        (dolist (range scope (nreverse groups))
          (when (and at (< at (car range)))
            (push (cons nil (list (make-rewrite-piece at (car range)))) groups))
          (push (cons range (coerce (rewrite-pieces-for rope (car range) (cdr range)) 'list)) groups)
          (setf at (cdr range))))))

(defun split-matcher (regexp block)
  "How a split finds REGEXP, the MATCHER the search prompt's replacements use
too (MAKE-PATTERN-MATCHER). Case is ignored when it has no capital letter. It
is found as literal text when nothing in it is special. ^ and $ match at every
kind of line end. It is always compiled for a block's rows (BLOCK), whose
parts of lines are matched as text. Signals why when it does not compile."
  (multiple-value-bind (matcher problem)
      (make-pattern-matcher regexp :regex t :compiled block)
    (or matcher (refuse "Regexp ~a: ~a" regexp problem))))

(defun stretch-holding (groups from to)
  "The stretch of GROUPS (SCOPE-GROUPS) that holds the text from FROM to TO, as
(START . END), or NIL when they lie in none, in the text between stretches."
  (car (find-if (lambda (group)
                  (let ((stretch (car group)))
                    (and stretch (<= (car stretch) from) (<= to (cdr stretch)))))
                groups)))

(defun piece-matches (function rope matcher scope groups from to halt &key texts)
  "Call FUNCTION as STRETCH-MATCHES does for each match of MATCHER in the piece
of SCOPE from FROM below TO, in order. Matches are found within the stretch of
GROUPS each is part of (none in the text between stretches). A match may run on
past TO to the stretch's end, and the empty match at the stretch's end is
included when the piece ends it. For a block, the matches are found within each
of the rows the piece holds, with its line before it as the pattern's context
\(TUATARA.SYNTAX:MAP-SLICE-MATCHES). The empty match at its end is included, and
the search starts from the whole row FROM is in (a caller that starts after a
match passes over those before it). True when FUNCTION stopped it."
  (if (block-scope-p scope)
      (let ((wide (block-scope-wide scope))
            (size (tuatara.text:rope-octet-length rope))
            (rows 0))
        (declare (type fixnum rows))
        (block each-row
          (tuatara.syntax:with-decoder (decoder)
            (tuatara.text:with-octet-block (reused (* 1024 1024))
              (apply
               #'tuatara.text:map-line-slices
               (lambda (line s e start-column end-column line-end)
                 (declare (ignore start-column end-column))
                 (when (and halt (zerop (logand (incf rows) 1023)) (funcall halt)) (return-from each-row nil))
                 (let ((s (if wide line s)) (e (if wide line-end e)))
                   ;; the empty line after the last newline holds no match
                   (unless (and (= s e size) (plusp size))
                     (when (tuatara.syntax:map-slice-matches function rope (matcher-scanner matcher) line s e
                                                             :decoder decoder :texts texts)
                       (return-from each-row t)))))
               ;; from the start of the line FROM is in: the rows are read whole, and a
               ;; walk over the matches may begin just after one
               rope (tuatara.text:rope-line-start rope from) to
               (if wide 0 (block-scope-left scope)) (if wide 0 (block-scope-right scope))
               :block reused
               (tuatara.display:screen-slice-keys (block-scope-crlf scope) (block-scope-tab-width scope)))))
          nil))
      (let ((range (stretch-holding groups from to)))
        (and range (stretch-matches function rope matcher from to (eql to (cdr range)) texts halt (cdr range))))))

(define-record (search-rewrite (:include rewrite) (:constructor %make-search-rewrite) (:copier nil))
  "A REWRITE that writes nothing: a split's search for the matches of MATCHER
over SCOPE (START-SPLIT-SEARCH), its pieces those of GROUPS (SCOPE-GROUPS).
Each piece's matches are kept at its place (INDEX) in FOUND. The matches FOLDS
hide any of, when FOLDS is not NIL, are counted in the car of HIDDEN. Once
there are more than LIMIT in all, counted in the car of COUNT, the car of
TOO-MANY is set and the search stops."
  (matcher nil :documentation "The MATCHER JOB finds the matches of.")
  (scope nil :documentation "What JOB searches: a list of (START . END) stretches or a BLOCK-SCOPE.")
  (groups nil :documentation "The stretches of the scope of JOB with their pieces (SCOPE-GROUPS).")
  (found nil :documentation "A vector holding each piece's matches of JOB, start and end offsets in pairs.")
  (index nil :documentation "A hash table of each piece of JOB to its place in FOUND.")
  (limit nil :documentation "The most matches JOB finds before it stops, NIL for no limit.")
  (count nil :documentation "A cons whose car counts the matches JOB has found.")
  (too-many nil :documentation "A cons whose car is set once JOB has found more than its LIMIT.")
  (folds nil :documentation "The folds of the buffer as JOB began, NIL when none.")
  (hidden nil :documentation "A cons whose car counts the matches JOB left out because a fold hides them."))

(defmethod write-piece ((job search-rewrite) piece)
  "Find the matches of JOB's MATCHER in PIECE (PIECE-MATCHES). Their start and
end offsets are kept in pairs at the piece's place (INDEX) in FOUND. When
FOLDS is not NIL, the matches that FOLDS hide any of
\(TUATARA.DISPLAY:FOLDS-HIDING) are left out and counted in the car of HIDDEN.
Once there are more than LIMIT matches in all, counted in the car of COUNT, set
the car of TOO-MANY and stop the job. A match may run past the piece's end
\(MEND-SEAMS)."
  (start-piece job piece nil)
  (let ((cancel (rewrite-cancel job))
        (offsets (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (end (rewrite-piece-end piece))
        (folds (search-rewrite-folds job))
        (limit (search-rewrite-limit job)))
    (piece-matches (lambda (s e m g)
                     (declare (ignore m g))
                     (setf (rewrite-piece-done piece) e
                           (rewrite-piece-reach piece) (max e (rewrite-piece-reach piece)))
                     (if (and folds (tuatara.display:folds-hiding folds s e))
                         (tally (search-rewrite-hidden job))
                         (progn
                           (vector-push-extend s offsets)
                           (vector-push-extend e offsets)
                           (when (and limit (>= (tally (search-rewrite-count job)) limit))
                             (setf (car (search-rewrite-too-many job)) t (car cancel) t))))
                     (car cancel))
                   (rewrite-rope job) (search-rewrite-matcher job) (search-rewrite-scope job)
                   (search-rewrite-groups job) (rewrite-piece-start piece) end (lambda () (car cancel)))
    (setf (svref (search-rewrite-found job) (gethash piece (search-rewrite-index job))) offsets)
    (unless (car cancel) (setf (rewrite-piece-done piece) end))))

(defun search-ended-here-p (buffer content)
  "Why what a search in the background found in BUFFER cannot be taken up now,
a message, or NIL when it can: BUFFER is still the one here, with the text it
had (CONTENT, its content number), in NORMAL state and with nothing asked. It
is busy too while something made meanwhile would be taken over: a split or
cursors in force, a selection, a rectangle or a mark run."
  (flet ((not-held (why) (format nil (substitute-command-keys why :for-format t) (tuatara.buffer:buffer-name buffer))))
    (cond ((not (eq buffer (current-buffer)))
           (not-held "Not held: ~a was left while \\<normal>\\[split-selection] searched it"))
          ((not (eql content (tuatara.buffer:buffer-content-number buffer)))
           (not-held "Not held: ~a changed while \\<normal>\\[split-selection] searched it"))
          ((or (editor-minibuffer-active *editor*) (not (eq (modal-state buffer) :normal))
               (split-here-p buffer) (tuatara.buffer:buffer-mark-active-p buffer) (mark-run-on-p))
           (not-held "Not held: ~a was busy when \\<normal>\\[split-selection] finished searching it")))))

(defun split-search-take-in (buffer regexp scope words whole found content limit hidden)
  "Hold as a split what a search in the background FOUND in BUFFER, in order,
the matches of REGEXP in SCOPE, named WORDS (the whole buffer with WHOLE). It
holds no more than LIMIT places, past which those in sight are held, to act on
every match in the background. HIDDEN is the matches left alone because a fold
hides them, as HOLD-SPLIT takes it. Only while the search can be taken up
\(SEARCH-ENDED-HERE-P). Returns what to say."
  (or (search-ended-here-p buffer content)
      (if (> (loop for offsets across found sum (floor (length offsets) 2)) limit)
          (armed-between-commands (hold-virtual-matches buffer regexp scope words whole (first-found found) :move nil))
          (let ((spans (loop for offsets across found
                             nconc (loop for k from 0 below (length offsets) by 2
                                         collect (cons (aref offsets k) (aref offsets (1+ k)))))))
            ;; point stays where moving about meanwhile left it: the verbs go to the places
            (armed-between-commands (hold-split buffer regexp spans words whole :move nil :hidden hidden))))))

(defun first-found (found)
  "The first match a search in the background FOUND, (START . END), or NIL."
  (loop for offsets across found
        when (plusp (length offsets)) return (cons (aref offsets 0) (aref offsets 1))))

(defun armed-between-commands (said)
  "Return SAID, after a transient map was set between commands: the next
command is asked whether it keeps it, as the one that set it would not be."
  (setf (editor-transient-fresh *editor*) nil)
  said)

(defun start-split-search (buffer regexp scope words whole)
  "Search SCOPE of BUFFER (a list of (START . END) stretches or a BLOCK-SCOPE),
named WORDS, for REGEXP in the background, cut into pieces searched on every
core at once, and hold the matches as a split when it ends
\(SPLIT-SEARCH-TAKE-IN); or, as soon as there are more than a split may hold
\(PLACE-LIMIT), hold those in sight, to act on them all. A match that a fold
hides any of as the search starts is left out. Returns the job, or :QUEUED when
another job runs or waits in BUFFER: it then waits its turn, SCOPE followed
meanwhile."
  (when (queue-if-busy buffer (job-words :search :doing (format nil "~a for ~a" words regexp)) (scope-positions scope)
                       (lambda (&rest positions)
                         (start-split-search buffer regexp (scope-at scope positions) words whole))
                       :shade (scope-shade buffer scope))
    (return-from start-split-search :queued))
  (multiple-value-bind (limit why) (place-limit)
    (let* ((rope (tuatara.buffer:buffer-text buffer))
           (folds (tuatara.display:folds-now buffer))
           (hidden (list 0))
           (matcher (split-matcher regexp (block-scope-p scope)))
           (groups (scope-groups rope scope))
           (pieces (coerce (loop for group in groups append (cdr group)) 'simple-vector))
           (index (let ((table (make-hash-table :test 'eq)))
                    (loop for piece across pieces for i from 0 do (setf (gethash piece table) i))
                    table))
           (found (make-array (length pieces) :initial-element #()))
           (too-many (list nil))
           (content (tuatara.buffer:buffer-content-number buffer))
           (job (%make-search-rewrite
                 :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                 :low (car (car (first groups))) :high (cdr (car (car (last groups))))
                 :words (format nil "the matches of ~a" regexp)
                 :what (job-words :search :doing (format nil "~a for ~a" words regexp))
                 :count-only t :doing (job-words :search :doing)
                 :verb (job-words :search :verb (format nil "for ~a" regexp)) :leaves "nothing held"
                 :matcher matcher :scope scope :groups groups :found found :index index
                 :limit limit :count (list 0) :too-many too-many :folds folds :hidden hidden
                 :report (lambda (job how changes seconds kept)
                           (declare (ignore job changes seconds kept))
                           (ecase how
                             (:done (split-search-take-in buffer regexp scope words
                                                          whole found content limit (car hidden)))
                             ;; stopped at the limit, not by hand: the places found are held
                             (:stopped (and (car too-many)
                                            (or (search-ended-here-p buffer content)
                                                (armed-between-commands
                                                 (hold-virtual-matches
                                                  buffer regexp scope words whole (first-found found)
                                                  :move nil)))))))
                 :pieces pieces
                 :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*)))
      (launch-rewrite job "tuatara split" (format nil "~:d octets in ~d piece~:p, at most ~:d (~(~a~))"
                                                  (scope-extent scope) (length pieces) limit why))
      ;; the selection searched let go meanwhile: a key typed before the split comes acts at the cursor, not on all of
      ;; it
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (say-in-background (job-words :search :doing (format nil "~a for ~a" words regexp))
                         "the split comes when it is done")
      job)))

;;; Writing at the places out of sight

(defvar *split-writers* '()
  "The jobs writing in the background at the places of splits they did not
hold, each (BUFFER JOB WORDS JOINS): JOB a REWRITE, or a QUEUED-JOB waiting its
turn, WORDS what the places are called, and JOINS the buffer's undo stack as
the verb left it at the places in sight, whose change the job's joins.")

(defun note-split-writer (buffer job words joins)
  "Note JOB, writing at the places of a split in BUFFER called WORDS (a
REWRITE, or :QUEUED when it waits its turn there), so that the buffer is not
saved before it is done; JOINS is the buffer's undo stack as the verb left
it at the places in sight, so that an undo of that stops the job
(STOP-SPLIT-WRITER)."
  (let ((entry (list buffer (if (eq job :queued) (car (last (queued-in buffer))) job) words joins)))
    (when (queued-job-p (second entry))
      ;; the job its turn starts takes the waiting one's place
      (let* ((queued (second entry)) (starter (queued-job-starter queued)))
        (setf (queued-job-starter queued)
              (lambda (&rest positions)
                (let ((started (apply starter positions)))
                  (when (rewrite-p started) (setf (second entry) started))
                  started)))))
    (when (second entry) (push entry *split-writers*))))

(defun split-writer (buffer)
  "The entry of *SPLIT-WRITERS* for a job writing at a split's places in
BUFFER that is not done yet: running, waiting its turn, or ended with its
result not yet taken in; NIL when none."
  (setf *split-writers*
        (remove-if-not (lambda (entry)
                         (let ((job (second entry)))
                           (if (queued-job-p job) (member job *job-queue*) (member job tuatara.runtime:*jobs*))))
                       *split-writers*))
  (find buffer *split-writers* :key #'first))

(defun stop-split-writer (buffer)
  "Before an undo in BUFFER that takes back what a split's verb did at the
places in sight: stop the job writing the same at the places out of sight, or
drop it from the queue when it waits its turn, so that the undo takes the verb
back whole and nothing is written behind it. A job that has ended, its result
not yet taken in, is stopped as well: it is taken in as stopped, its result
left out. Only while that change is the newest in BUFFER's undo history. Says
so, and returns true, when it stopped one."
  (let* ((writer (split-writer buffer))
         (job (second writer))
         (stack (tuatara.buffer:buffer-undo-stack buffer)))
    (when (and writer stack (fourth writer) (eq (cdr stack) (cdr (fourth writer)))
               (cond ((queued-job-p job) (drop-queued-job job) t)
                     ;; running, or ended and not taken in yet (SPLIT-WRITER)
                     (t (tuatara.runtime:job-stop job) t)))
      (message "The writing at ~a is stopped: they are as they were, and the undo takes back the places in sight"
               (third writer))
      t)))

(defun typed-match (rope s e mode text back forward)
  "What the typing at a split's places makes of the match from S to E of ROPE,
as it made the places it was typed at: TEXT (octets) at the match's start or
end (MODE :START or :END) or in place of it (:CLEAR), the text BACK (octets)
just before the place and FORWARD just after it deleted too. Three values,
where the change starts and ends and the octets put there; NIL when the very
text deleted beside the place it was typed at is not there beside this one."
  (declare (type fixnum s e) (type tuatara.text:octets back forward))
  (let* ((from (if (eq mode :end) e s))
         (to (if (eq mode :start) s e))
         (start (- from (length back)))
         (end (+ to (length forward))))
    (declare (type fixnum from to start end))
    (when (and (>= start 0) (<= end (tuatara.text:rope-octet-length rope))
               (loop for i of-type fixnum from 0 below (length back)
                     always (= (tuatara.text:rope-octet rope (+ start i)) (aref back i)))
               (loop for i of-type fixnum from 0 below (length forward)
                     always (= (tuatara.text:rope-octet rope (+ to i)) (aref forward i))))
      (values start end text))))

(define-record (matches-job (:constructor make-matches-job) (:copier nil))
  "What a background job at every match of a split's pattern changes
\(START-EVERY-MATCH-JOB): in ROPE, the matches MATCHER finds in SCOPE, cut into
GROUPS (SCOPE-GROUPS), up to HIGH, the scope's end. KIND says what changes.
:TYPE writes what was typed at the places (TYPED-MATCH: TEXT as MODE says, the
octets BACK before the match and FORWARD after it deleted too). :DELETE takes
the match away. :WRAP puts OPEN before it and CLOSE after it, the match's text
read for it (TEXTS). :UNWRAP takes them off where they stand just outside it
\(WRAPPED-MATCH). The matches meeting a place of SKIP (a vector of (START . END)
in order), done in sight, are left as they are. FOLDS are the folds of the text
as the job began (FOLDS-NOW). A match one hides any of is left as it is, counted
in the car of LEFT. One whose change would take part of the text one hides, but
not all of it, is counted in the car of HIDDEN. REFUSED counts in its car the
matches :TYPE could not change. Matches that touch are refused, as a split
that holds every place refuses them: the first two found, each (START . END),
are kept in the car of TOUCHING, and the job, whose stop flag is CANCEL, is
stopped with nothing written."
  (rope nil :documentation "The text MATCHES-JOB works in.")
  (matcher nil :documentation "The matcher MATCHES-JOB finds its matches with.")
  (scope nil :documentation "The scope MATCHES-JOB searches.")
  (groups nil :documentation "The scope of MATCHES-JOB cut into groups.")
  (high 0 :documentation "Where the scope of MATCHES-JOB ends.")
  (kind nil :documentation "What MATCHES-JOB changes: :TYPE, :DELETE, :WRAP or :UNWRAP.")
  (texts nil :documentation "The text of each match, read for :WRAP.")
  (mode nil :documentation "Where :TYPE writes its text: :START, :END or :CLEAR.")
  (text nil :documentation "The octets :TYPE writes at each match.")
  (back *no-octets* :documentation "The octets :TYPE deletes just before each match.")
  (forward *no-octets* :documentation "The octets :TYPE deletes just after each match.")
  (open nil :documentation "The octets :WRAP puts before each match, or :UNWRAP takes off.")
  (close nil :documentation "The octets :WRAP puts after each match, or :UNWRAP takes off.")
  (skip nil
        :documentation "A vector of (START . END) in order, the matches in sight already done, which MATCHES-JOB
leaves as they are.")
  (folds nil :documentation "The folds of the text as MATCHES-JOB began.")
  (refused (list 0) :documentation "A list whose car counts the matches :TYPE could not change.")
  (hidden (list 0)
          :documentation "A list whose car counts the matches left as they are because their change would take part of
a hidden text.")
  (left (list 0)
        :documentation "A list whose car counts the matches left as they are because a fold hides some of them.")
  (touching (list nil)
            :documentation "A list whose car is NIL, or the first two matches MATCHES-JOB found touching, each
(START . END).")
  (cancel nil :documentation "The stop flag of the job of MATCHES-JOB, a cons whose car is set to stop it."))

(setf (documentation 'make-matches-job 'function) "Return a MATCHES-JOB: what a background job at every match changes.")

(defun start-every-match-job (buffer regexp scope words kind &key typed open close skip joins report matcher)
  "Write at every match of REGEXP in SCOPE of BUFFER (a list of stretches or a
BLOCK-SCOPE, named WORDS), but those meeting a place of SKIP (a vector of
(START . END) in order), in the background on all but two cores: KIND :TYPE as
TYPED says (TYPED-MATCH), :DELETE, :WRAP each in OPEN and CLOSE, or :UNWRAP
the pair off where it stands outside it. A match that a fold hides any of as
the job starts is left alone, and the report counts it; and one whose change
would take part of the text a fold hides, but not all of it, is left as it is.
Matches that touch are refused: the job stops as soon as it finds two, with
nothing written, and says where they are (EVERY-MATCH-EDITS).
One change for undo, joining JOINS's (REWRITE), edits made meanwhile kept,
\\[stop-job] to stop it; REPORT, when given, says how it ended; MATCHER, when
given, finds REGEXP as SPLIT-MATCHER made it for the split. Returns the job,
or :QUEUED when another job runs or waits in the buffer, or its lines are
still being counted: it then waits its turn, the matches' stretches and SKIP's
places followed meanwhile."
  (let ((what (job-words kind :doing (format nil "every other match of ~a in ~a" regexp words)))
        (matcher (or matcher (split-matcher regexp (block-scope-p scope)))))
    (when (or (job-busy-p buffer) (queued-in buffer) (lines-uncounted-p buffer))
      (let ((followed (and skip (spans-followed buffer skip))))
        (when (queue-if-busy buffer what (scope-positions scope)
                             (lambda (&rest positions)
                               (start-every-match-job buffer regexp (scope-at scope positions) words kind
                                                      :typed typed :open open :close close
                                                      :report report :matcher matcher
                                                      :skip (and followed (funcall followed))))
                             :shade (scope-shade buffer scope :matcher matcher)
                             :counted t)
          (return-from start-every-match-job :queued))))
    (tuatara.buffer:ensure-indexed buffer)
    (let* ((rope (tuatara.buffer:buffer-text buffer))
           (groups (scope-groups rope scope))
           (spec (make-matches-job :rope rope :scope scope :groups groups :high (cdr (car (car (last groups))))
                                   :matcher matcher
                                   :kind kind :texts (eq kind :wrap)
                                   :mode (getf typed :mode) :text (getf typed :text)
                                   :back (getf typed :back-octets *no-octets*)
                                   :forward (getf typed :forward-octets *no-octets*)
                                   :open (and open (tuatara.text:string-octets open))
                                   :close (and close (tuatara.text:string-octets close))
                                   :skip skip :folds (tuatara.display:folds-now buffer)))
           (job (%make-match-rewrite
                 :session t :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                 :low (car (car (first groups))) :high (matches-job-high spec)
                 :words (format nil "every other match of ~a" regexp)
                 :what what
                 :joins joins :hidden-left (matches-job-left spec)
                 :mapper (lambda (function from to halt) (every-match-edits spec function from to halt))
                 :all t :doing (job-words kind :doing)
                 :verb (job-words kind :verb (format nil "the matches of ~a out of sight" regexp))
                 :leaves "the matches out of sight are as they were"
                 :report (touching-report
                          spec
                          (or report
                              (job-report kind
                                          (lambda (n)
                                            (format nil "~:d more match~:*~[es~;~:;es~] of ~a, ~
out of sight," n regexp))
                                          (format nil "the other matches of ~a" regexp)
                                          :refused (matches-job-refused spec)
                                          :hidden (matches-job-hidden spec))))
                 :pieces (coerce (loop for group in groups append (cdr group)) 'simple-vector)
                 :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*
                 :directory (tuatara.io:buffer-scratch-directory buffer))))
      (setf (matches-job-cancel spec) (rewrite-cancel job))
      (launch-rewrite job "tuatara every match"
                      (format nil "~:d octets in ~d piece~:p, ~:d held"
                              (scope-extent scope) (length (rewrite-pieces job)) (length skip)))
      job)))

(defun match-change (spec s e m &optional count)
  "What SPEC, a MATCHES-JOB, writes at the match from S below E, M its text when
SPEC reads it. It returns three values: the stretch from A below Z it writes
over and the octets it puts there. It returns NIL when that match cannot be
changed: what was typed beside the places is not there beside it (counted in
REFUSED when COUNT is true), or there is no pair around it to take off."
  (let ((rope (matches-job-rope spec)) (open (matches-job-open spec)) (close (matches-job-close spec)))
    (ecase (matches-job-kind spec)
      (:type (multiple-value-bind (a z octets)
                 (typed-match rope s e (matches-job-mode spec) (matches-job-text spec)
                              (matches-job-back spec) (matches-job-forward spec))
               (when (and count (not a)) (tally (matches-job-refused spec)))
               (values a z octets)))
      (:delete (values s e *no-octets*))
      (:wrap (values s e (tuatara.text:string-octets (concatenate 'string (tuatara.text:octets-string open)
                                                                  m (tuatara.text:octets-string close)))))
      (:unwrap (multiple-value-bind (a z) (wrapped-match rope s e open close)
                 (and a (values a z (tuatara.text:rope-octets rope :start s :end e))))))))

(defun touching-report (spec report)
  "REPORT, how a job at every match says it ended (REWRITE), saying instead,
when the job stopped because two matches of SPEC, its MATCHES-JOB, touch, where
they are and that nothing was written out of sight."
  (lambda (job how changes seconds kept)
    (let ((touching (car (matches-job-touching spec))))
      (if (and touching (eq how :stopped))
          (format nil "~a, and a split needs a character between places: the matches out of sight are as they were"
                  (touching-words (matches-job-rope spec) (first touching) (second touching)))
          (funcall report job how changes seconds kept)))))

(defun match-ending-at (spec from)
  "The match of SPEC, a MATCHES-JOB, that ends just at FROM, where a piece of
its stretch starts, as (START . END), or NIL: looked for on the line before
FROM, so that a match starting there is known to touch it."
  (let ((stretch (stretch-holding (matches-job-groups spec) from from))
        (rope (matches-job-rope spec))
        (last nil))
    (when (and stretch (< (car stretch) from))
      (stretch-matches (lambda (s e m g) (declare (ignore m g)) (setf last (cons s e)) nil)
                       rope (matches-job-matcher spec)
                       (max (car stretch) (tuatara.text:rope-line-start rope (1- from))) from nil nil nil (cdr stretch))
      (and last (= (cdr last) from) last))))

(defun every-match-edits (spec function from to halt)
  "The mapper of a job at every match (REWRITE), SPEC a MATCHES-JOB. It calls
FUNCTION with (A Z NIL OCTETS) for each change MATCH-CHANGE makes to a match
starting from FROM below TO, in order (PIECE-MATCHES). A match that is left as
it is gets told as passed over (PASS-OVER). A match whose change would overlap
the one before, or run past the scope, is left as it is too. What folds keep it
from, and what MATCH-CHANGE refuses, is counted as the pieces are written (HALT
given), not as its result is mapped or its matches lit. As the pieces are
written, two matches that touch, not both of them places done in sight, stop
the job with nothing written (the car of TOUCHING): the one before a piece's
first is looked for on the line before it (MATCH-ENDING-AT)."
  (let ((previous from) (skip (matches-job-skip spec)) (folds (matches-job-folds spec))
        (before (and halt :unknown)) (before-held nil))
    (piece-matches
     (lambda (s e m g)
       (declare (ignore g))
       (let ((held (and skip (spans-meet-p skip s e))))
         (when halt
           (when (eq before :unknown)
             (setf before (and (= s from) (not (block-scope-p (matches-job-scope spec))) (match-ending-at spec from))
                   before-held (and before skip (spans-meet-p skip (car before) (cdr before)))))
           (when (and before (<= s (cdr before)) (not (and held before-held)))
             (sb-ext:compare-and-swap (car (matches-job-touching spec)) nil (list before (cons s e)))
             (setf (car (matches-job-cancel spec)) t)
             (return-from every-match-edits t))
           (setf before (cons s e) before-held held))
         (cond
           (held (pass-over function s e))
           ((and folds (tuatara.display:folds-hiding folds s e))
            (when halt (tally (matches-job-left spec)))
            (pass-over function s e))
           (t
            (multiple-value-bind (a z octets) (match-change spec s e m halt)
              (cond ((not (and a (>= a previous) (<= z (matches-job-high spec))))
                     (pass-over function s e))
                    ((and folds (tuatara.display:folds-cut folds a z))
                     (when halt (tally (matches-job-hidden spec)))
                     (pass-over function s e))
                    (t (setf previous z)
                       (funcall function a z nil octets)))))))
       nil)
     (matches-job-rope spec) (matches-job-matcher spec) (matches-job-scope spec) (matches-job-groups spec)
     from to halt :texts (matches-job-texts spec))))

(defun virtual-typed (typing new)
  "What TYPING, a SPLIT-TYPING, typed at the places, NEW the text the primary
place holds now, for TYPED-MATCH: :TEXT, and the text deleted beside the
places, :BACK-OCTETS and :FORWARD-OCTETS, and how many characters it is,
:BACK and :FORWARD, with the offsets of its line breaks."
  (multiple-value-bind (before after) (edge-texts (split-typing-edges typing))
    (list :text (tuatara.text:string-octets new)
          :back-octets (tuatara.text:string-octets before) :forward-octets (tuatara.text:string-octets after)
          :back (length before) :back-breaks (line-break-offsets before)
          :forward (length after) :forward-breaks (line-break-offsets after))))

(defun start-virtual-job (split &key typing kind open close)
  "Write at the places of SPLIT past its limit that it does not hold, in the
background, what was done at those it holds: what TYPING (a SPLIT-TYPING)
typed there, or KIND :DELETE, :WRAP or :UNWRAP (in the pair OPEN and CLOSE).
The places held are left as they are, and the change joins theirs for undo
when nothing comes between. Says so; returns the job, :QUEUED or NIL."
  (let* ((b (split-buffer split))
         (virtual (split-virtual split))
         (typed (and typing (virtual-typed typing (split-text split))))
         (joins (tuatara.buffer:buffer-undo-stack b)))
    (when (and typing (equal (split-typing-original typing) (split-text split)) (null (split-typing-edges typing))
               (not (eq (virtual-mode virtual) :clear)))
      (return-from start-virtual-job nil))
    (let ((job (write-virtual-places virtual split (or kind :type)
                                     :typed typed :text (if typing (split-text split) "") :open open :close close
                                     :skip (held-spans split) :joins joins)))
      (when job
        (setf (virtual-started virtual) t)
        (note-split-writer b job (virtual-words virtual) joins)
        (say-in-background (job-words (or kind :type) :doing (virtual-words virtual))))
      job)))

(defmethod write-virtual-places ((virtual matches-virtual) split kind &key typed text open close skip joins)
  "Write at every other match of the pattern (START-EVERY-MATCH-JOB)."
  (declare (ignore text))
  (let ((b (split-buffer split)))
    (start-every-match-job b (virtual-regexp virtual) (virtual-scope-now b virtual) (split-scope split) kind
                           :typed (and typed (list* :mode (virtual-mode virtual) typed))
                           :open open :close close :skip skip :joins joins
                           :matcher (virtual-matcher virtual))))

(defmethod write-virtual-places ((virtual rows-virtual) split kind &key typed text open close skip joins)
  "Write at every other row (START-LINES-JOB); a wrap, and a deletion across a
line's end, are made only at the rows in sight."
  (declare (ignore open close))
  (cond ((member kind '(:wrap :unwrap))
         (message "~@(~a~) were left as they were: a wrap is made only at those in sight" (virtual-words virtual))
         nil)
        ((or (getf typed :back-breaks) (getf typed :forward-breaks))
         (message "~@(~a~) were left as they were: a deletion across a line's end is made only at those in sight"
                  (virtual-words virtual))
         nil)
        (t (start-lines-job (split-buffer split) virtual kind
                            :text text :back (or (getf typed :back) 0) :forward (or (getf typed :forward) 0)
                            :skip skip :joins joins))))
