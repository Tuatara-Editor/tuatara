;;;; lisp/selecting/split.lisp --- Split: every match of a regexp in the selection held, and acted on at once.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A split holds places, stretches of one buffer: the matches of f, or the
;;;; cursors t makes (cursors.lisp). What is typed at the place point is in is
;;;; written at every other place after each command, one change for undo, and a
;;;; deletion just past the place's edge is made at every other place's edge,
;;;; at a match only where the very text deleted is there (else the split ends).
;;;; The buffer is not saved while a split is written at, and a split typed at
;;;; ends before the buffer is read again from its file.
;;;; Over a large stretch the search runs in the background (split-jobs.lisp);
;;;; no more places are held than *SPLIT-LIMIT*, those in sight past it, and
;;;; what is done at those is written at the others in the background.

(in-package #:tuatara.defaults)

;;; Places

(define-record (place (:constructor %make-place (start end)) (:copier nil))
  "A stretch of text a split holds, between the anchors START (which stays
before text put in at it) and END (which goes after it). In a split of cursors a
place is where its cursor is and what the cursor selects. AT-END puts the cursor
at the end rather than the start. ANCHORED says the cursor selects from the
other end. LINE says that it selects its whole line. OFFSET says how far into
that line it stands. MEMORY holds what the selecting keys left there to grow on
the next press."
  (start nil :documentation "The anchor where PLACE begins.")
  (end nil :documentation "The anchor where PLACE ends.")
  (at-end nil :documentation "True when PLACE's cursor is at its end rather than its start.")
  (anchored nil :documentation "True when PLACE's cursor selects from the place's other end.")
  (line nil :documentation "True when PLACE's cursor selects its whole line.")
  (offset 0 :documentation "How far into its line PLACE's cursor stands, when it selects the line.")
  (memory nil :documentation "What the selecting keys left at PLACE's cursor, to grow on the next press."))

(setf (documentation 'place-p 'function) "True when OBJECT is a PLACE of a split.")

(defun make-place (buffer start end)
  "A place of BUFFER from START to END."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (%make-place (tuatara.text:make-anchor version start :bias :before)
                 (tuatara.text:make-anchor version end :bias :after))))

(defun place-bounds (buffer place)
  "Two values: where PLACE of BUFFER starts and ends now."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (values (tuatara.text:anchor-position (place-start place) version)
            (tuatara.text:anchor-position (place-end place) version))))

(defun move-place (buffer place start end)
  "Make PLACE of BUFFER run from START to END."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (tuatara.text:anchor-move (place-start place) version start)
    (tuatara.text:anchor-move (place-end place) version end)
    place))

(defun place-text (buffer place)
  "The text PLACE of BUFFER holds."
  (multiple-value-bind (start end) (place-bounds buffer place)
    (tuatara.buffer:buffer-string buffer :start start :end end)))

;;; The split

(define-record (split (:constructor make-split (buffer)) (:copier nil))
  "The split in force: its BUFFER and PLACES (a vector, in buffer order).
PRIMARY is the place that is point's, and TEXT is its text (as last seen), which
the others follow. PHASE is :SELECT while a verb is awaited or :EDIT while
typing at the places. DID is what the verb did, for the report. SCOPE is what
was searched, named for the reminder. AGREE is true when every place held the
same text. TICK is the buffer's content number when the split was made. SINCE is
the undo stack as the typing began. BOUNDS is the primary place's start and end
as the last change left them. EDGES is the deletions a command made just past
the primary place. STRAYED is true when a command changed text away from the
places. VIRTUAL, for a split past its limit, holds what it needs to write at the
places it does not hold, those out of sight (a VIRTUAL). A split holds no match
that a fold hides any of; HIDDEN counts those it left alone, for what it says.
LEFT counts the places the typing left as they were when it ended, the text
beside them not what was deleted beside the others. REMINDER is what the echo
area last said while it lasts, with what that depends on (SPLIT-HINT). A split
of cursors is a CURSOR-SPLIT."
  (buffer nil :documentation "The buffer SPLIT holds places in.")
  (places #() :documentation "SPLIT's places, a vector in buffer order.")
  (primary nil :documentation "The place of SPLIT that point is in, whose text the others follow.")
  (text "" :documentation "What SPLIT's primary place held when last looked at.")
  (phase :select :documentation "SPLIT's phase: :SELECT while a verb is awaited, :EDIT while typing.")
  (did nil :documentation "What SPLIT's verb did, for the report as it ends:
:BEFORE, :AFTER, :LINE-STARTS, :LINE-ENDS, :OPENED-BELOW, :OPENED-ABOVE (typed
there), :CHANGED, :DELETED, :WRAPPED, :UNWRAPPED, :PASTED, :EDITED, or NIL for
nothing.")
  (scope "this line" :documentation "What SPLIT searched, named for the reminder.")
  (agree nil :documentation "True when every place of SPLIT held the same text.")
  (tick nil :documentation "The content number of SPLIT's buffer as the split was made.")
  (since nil :documentation "The undo stack of SPLIT's buffer as its typing began.")
  (bounds nil :documentation "The primary place's start and end, as the last change left them.")
  (edges nil :documentation "Deletions just past the primary place's edges, newest first, as (SIDE . TEXT).")
  (strayed nil :documentation "True when a command changed text away from SPLIT's places.")
  (virtual nil
           :documentation "For SPLIT past its limit, what it needs to write at the
places it does not hold (a VIRTUAL), or NIL.")
  (hidden 0 :documentation "How many matches SPLIT left alone because a fold hides them.")
  (left 0 :documentation "How many places the typing at SPLIT left as they were when it ended.")
  (reminder nil :documentation "What the echo area last said while SPLIT lasts, as (WHAT-IT-DEPENDS-ON . TEXT)."))

(define-record (cursor-split (:include split) (:conc-name split-)
                             (:constructor make-cursor-split (buffer column &aux (scope "a column"))) (:copier nil))
  "A split of cursors (cursors.lisp): a SPLIT whose places are where its
cursors are and what they select. COLUMN is where new cursors go, ORDER lists
the cursors added after the first, newest first, LAST is the command the
cursors last ran, RUNNING is true in a mark run, whose steps are HISTORY and
REDO, and REPEAT is what \\<*cursor-keymap*>\\[cursors-repeat] runs again."
  (column 0 :documentation "The column on the screen new cursors of CURSOR-SPLIT go to.")
  (order nil :documentation "The cursors added to CURSOR-SPLIT after the first, newest first.")
  (last nil :documentation "The command CURSOR-SPLIT's cursors last ran together.")
  (running nil :documentation "True while CURSOR-SPLIT's cursors are in a mark run.")
  (history nil :documentation "The steps of the cursors' mark run, newest first, for
\\<*mark-run-keymap*>\\[mark-run-step-back].")
  (redo nil :documentation "The steps \\<*mark-run-keymap*>\\[mark-run-step-back] took
back in the cursors' mark run, for \\[mark-run-step-forward].")
  (repeat nil :documentation "What \\<*cursor-keymap*>\\[cursors-repeat] runs again at
CURSOR-SPLIT's cursors."))

(define-record (block-scope (:constructor make-block-scope (top high left right wide crlf tab-width)) (:copier nil))
  "A rectangle's rows as a split searches them, read as they come rather than
gathered first: the lines starting from TOP below HIGH, between the columns
LEFT and RIGHT on the screen (a tab TAB-WIDTH columns at most), or with WIDE
their whole lines; carriage return and line feed (CRLF) when lines end with a
carriage return and a newline."
  (top 0 :documentation "The start of the first line of BLOCK-SCOPE.")
  (high 0 :documentation "Where BLOCK-SCOPE ends: the lines read start below it.")
  (left 0 :documentation "The column on the screen where BLOCK-SCOPE starts.")
  (right 0 :documentation "The column on the screen where BLOCK-SCOPE ends.")
  (wide nil :documentation "True when BLOCK-SCOPE takes whole lines.")
  (crlf nil :documentation "True when a carriage return before a newline ends the lines of BLOCK-SCOPE.")
  (tab-width 8 :documentation "The most columns a tab takes on the screen in BLOCK-SCOPE."))

(define-record (virtual (:constructor nil) (:copier nil))
  "What a split past its limit needs to write at the places it does not hold,
those out of sight, when the work at those it holds is done: the matches of a
pattern (a MATCHES-VIRTUAL) or a cursor on each of many lines (a
ROWS-VIRTUAL). MODE is what the verb did to the places first: :START or :END,
narrowed to that edge, or :CLEAR, emptied. COUNT is how many places there are,
at least unless EXACT; LIMIT the most the split holds, and SETTING the setting
it comes from, for what is said. STARTED is true once a job was started for it.
PASSED lists the matches come into sight while typing that were let go, the
text beside them not what was deleted beside the others, or emptying them
taking part of the text a fold hides, so that they are not held again. SEEN is
what the places in sight were last looked for in: the text's version, the
places held, what the verb made of them, and the stretches in sight
\(VIRTUAL-FILL)."
  (mode :start
        :documentation "What the verb did to the places of VIRTUAL first: :START or :END, narrowed to that edge, or
:CLEAR, emptied.")
  (count 0 :documentation "How many places VIRTUAL has, at least unless EXACT.")
  (exact nil :documentation "True when the COUNT of VIRTUAL is exact rather than a lower bound.")
  (limit 0 :documentation "The most places the split of VIRTUAL holds.")
  (setting nil :documentation "The setting the LIMIT of VIRTUAL comes from, for what is said.")
  (started nil :documentation "True once a job was started for VIRTUAL.")
  (passed '()
          :documentation "The matches come into sight while typing that VIRTUAL let go, so that they are not held
again.")
  (seen nil
        :documentation "What the places of VIRTUAL in sight were last looked for in: the text's version, the places
held, what the verb made of them, and the stretches in sight."))

(define-record (matches-virtual (:include virtual) (:conc-name virtual-) (:constructor make-matches-virtual)
                                (:copier nil))
  "The matches past a split's limit: those of REGEXP, found by MATCHER, in the
scope made of SCOPE at the anchors ANCHORS (SCOPE-AT)."
  (regexp nil :documentation "The regular expression whose matches MATCHES-VIRTUAL writes at.")
  (matcher nil :documentation "The matcher that finds the matches of MATCHES-VIRTUAL.")
  (scope nil :documentation "What the scope of MATCHES-VIRTUAL is made of.")
  (anchors nil :documentation "The anchors the scope of MATCHES-VIRTUAL is made at."))

(define-record (rows-virtual (:include virtual) (:conc-name virtual-) (:constructor make-rows-virtual) (:copier nil))
  "The cursors past a split's limit: the lines from the one starting at the
anchor TOP to the one starting at the anchor BOTTOM, a cursor on each at
COLUMN (a number, or :END for the line's end), but with SHOWN on those a fold
or a search filter hides. The place of the verb under way on each row runs
from column LEFT to column RIGHT, or with AT-END is the line's end, a row short
of LEFT padded out to it when PAD; the columns count characters, or with
TAB-WIDTH they are on the screen, a tab that many columns at most, as a
rectangle's are."
  (top nil :documentation "The anchor at the start of the first line of ROWS-VIRTUAL.")
  (bottom nil :documentation "The anchor at the start of the last line of ROWS-VIRTUAL.")
  (shown nil :documentation "True when ROWS-VIRTUAL passes over the lines a fold or a search filter hides.")
  (column 0 :documentation "The column of the cursor on each row of ROWS-VIRTUAL: a number, or :END for the line's
end.")
  (left 0 :documentation "The column where the place of ROWS-VIRTUAL on each row starts.")
  (right 0 :documentation "The column where the place of ROWS-VIRTUAL on each row ends.")
  (at-end nil :documentation "True when the place of ROWS-VIRTUAL on each row is the line's end.")
  (pad nil :documentation "True when a row of ROWS-VIRTUAL short of LEFT is padded out to it.")
  (tab-width nil
             :documentation "The most columns a tab takes on the screen in ROWS-VIRTUAL, or NIL when the columns count
characters."))

(define-record (split-diff (:constructor %make-split-diff) (:copier nil))
  "What turns a place holding one text into one holding another. First comes
whether the text CHANGED. Then come the deletions just past its edges (EDGES
true), BEFORE and AFTER it, and where their line breaks fall. The new text is
WHOLE, and MIDDLE is what differs, as octets. The octets the two share at the
start and end are PREFIX-OCTETS and SUFFIX-OCTETS, and OLD-OCTETS is the old
text's."
  (changed nil :documentation "True when the text of SPLIT-DIFF changed.")
  (edges nil :documentation "True when there are deletions just past the edges of the place.")
  (before "" :documentation "The text deleted just before the place in SPLIT-DIFF.")
  (after "" :documentation "The text deleted just after the place in SPLIT-DIFF.")
  (before-breaks nil :documentation "Where the line breaks fall in the BEFORE text of SPLIT-DIFF.")
  (after-breaks nil :documentation "Where the line breaks fall in the AFTER text of SPLIT-DIFF.")
  (whole nil :documentation "The new text of SPLIT-DIFF.")
  (middle nil :documentation "What differs between the old and new text of SPLIT-DIFF, as octets.")
  (prefix-octets 0 :documentation "How many octets the old and new text of SPLIT-DIFF share at the start.")
  (suffix-octets 0 :documentation "How many octets the old and new text of SPLIT-DIFF share at the end.")
  (old-octets 0 :documentation "The length in octets of the old text of SPLIT-DIFF."))

(defvar *split* nil
  "The split in force, or NIL.")

(defvar *split-keeping* nil
  "True while the split takes its own keys down on purpose, so that it stays.")

(defvar *split-copying* nil
  "True while the split writes its places itself, which are not its reader's changes.")

(define-record (split-typing (:constructor make-split-typing (original)) (:copier nil))
  "The typing under way at the places of a split, its matches or its cursors:
the primary place's ORIGINAL text as it began, the deletions just past its
edges since then (EDGES, oldest first, each (SIDE . TEXT)), and the places
written as it goes (LIVE, a set); the others are written once it ends. COVERED
is where in the split's places, the vector PLACES, those looked at in sight so
far are: (FROM . TO) runs of their indices, in order (PLACES-COME-INTO-SIGHT)."
  (original "" :documentation "The primary place's text as the typing of SPLIT-TYPING began.")
  (edges '()
         :documentation "The deletions just past the edges of the primary place since SPLIT-TYPING began, oldest
first, each (SIDE . TEXT).")
  (live (make-hash-table :test 'eq) :documentation "The places SPLIT-TYPING has written as it goes, a set.")
  (covered '()
           :documentation "The runs (FROM . TO) of indices into PLACES that SPLIT-TYPING has looked at in sight so
far, in order.")
  (places nil :documentation "The vector of the places of the split of SPLIT-TYPING."))

(defvar *split-typing* nil
  "The typing at the split in force, or NIL: while it lasts, each key is written
only at the places in sight, the others when it ends, so that a key costs the
same at a hundred places as at a million.")

(defvar *split-sight-octets* 65536
  "How far before and after what a window shows, and around point, the places
written as each key is typed reach at least, so that those the typing itself
brings into sight are written by then.")

;;; What differs between a split of matches and one of cursors

(defgeneric place-noun (split)
  (:documentation "What a place of SPLIT is called: \"place\", or \"cursor\" in
a split of cursors."))

(defgeneric split-hint-mood (split)
  (:documentation "What, besides its phase and places, the echo area's words
for SPLIT depend on (SPLIT-HINT): NIL for a split of matches; for one of
cursors, :RUNNING in a mark run, :SELECTING when a cursor selects something
while a verb is awaited, else :CURSORS."))

(defgeneric split-hint-text (split kept mood)
  (:documentation "What the echo area says while SPLIT lasts, the places out of
sight still to be written when KEPT, MOOD as SPLIT-HINT-MOOD gave it."))

(defgeneric split-edit-primary (split clear where)
  (:documentation "Make SPLIT ready for the typing a verb starts, the places
emptied first when CLEAR, typed before or after (WHERE :START or :END), and
return the place the typing starts at: whether its places agree, and past the
limit what the verb makes of those out of sight."))

(defgeneric split-exact-edges-p (split)
  (:documentation "True when a deletion just past the primary place's edges is
made at another place of SPLIT only where the very text deleted is there (at a
match), rather than where as many characters are (at a cursor)."))

(defgeneric wrap-targets (split)
  (:documentation "The places of SPLIT a wrap acts on, as a list in order:
every match, or every cursor selecting something."))

(defgeneric settle-places (split)
  (:documentation "Fit SPLIT to its places after a verb moved them and the
split stays: in a split of cursors, each cursor put back on its place."))

(defgeneric quit-split (split)
  (:documentation "What \\<*split-chooser-keymap*>\\[split-quit] does to SPLIT:
end it, keeping what it changed; with cursors in a mark run, or selecting, let
go of the selections and keep the cursors."))

(defgeneric end-split-typing (split)
  (:documentation "Once the typing at SPLIT's places is over and written: end a
split of matches, or take a split of cursors back to its cursors."))

(defgeneric split-keeps-p (split command)
  (:documentation "True when SPLIT stays after COMMAND, besides the commands
every split stays after (SPLIT-KEEP-P): one that only moved about while a split
of matches waits for a verb, or \\<normal>\\[split-selection] at cursors, which
leaves them where nothing matches."))

(defgeneric place-lit-p (split place)
  (:documentation "True when PLACE of SPLIT is shown in :split-place: every
match, or a cursor's selection."))

(defgeneric place-cursor (split buffer place)
  (:documentation "Where the cursor of PLACE of SPLIT in BUFFER is drawn while
a verb is awaited: a match's start, or where the cursor is."))

(defgeneric ready-new-places (split places)
  (:documentation "Ready PLACES, just held by SPLIT as they came into sight
while a verb is awaited: in a split of cursors, a cursor at the start of
each."))

(defmethod place-noun ((split split))
  "A match is a place."
  "place")

(defmethod split-hint-mood ((split split))
  "Nothing but the phase and the places changes what a split of matches says."
  nil)

(defmethod split-hint-text ((split split) kept mood)
  "What a split of matches says: what typing writes at, or the verbs it waits
for."
  (declare (ignore mood))
  (if (eq (split-phase split) :edit)
      (split-edit-hint split kept "to finish")
      (split-say "Split: ~a in ~a~:[~;, those out of sight written in the background~]~a; ~
\\<*split-chooser-keymap*>\\[split-insert] before, \\[split-append] after, \\[split-change] change, ~
\\[split-delete] delete, \\[split-wrap] wrap, \\[split-quit]"
                 (split-places-phrase split) (split-scope split) (split-virtual split)
                 (hidden-left-words (split-hidden split)))))

(defmethod split-edit-primary ((split split) clear where)
  "Typing starts at the first match. Past the limit, the matches out of sight
need not agree with those held, and the verb says what is made of them."
  (let ((virtual (split-virtual split)))
    (when virtual
      (setf (split-agree split) nil
            (virtual-mode virtual) (cond (clear :clear) ((eq where :end) :end) (t :start)))))
  (aref (split-places split) 0))

(defmethod split-exact-edges-p ((split split))
  "At a match, only the very text deleted."
  t)

(defmethod wrap-targets ((split split))
  "Every match is wrapped."
  (coerce (split-places split) 'list))

(defmethod settle-places ((split split))
  "A match needs no fitting."
  nil)

(defmethod quit-split ((split split))
  "A split of matches ends."
  (dissolve-split))

(defmethod end-split-typing ((split split))
  "A split of matches ends with its typing."
  (dissolve-split))

(defmethod split-keeps-p ((split split) command)
  "A split of matches stays while COMMAND only looked about (SPLIT-LOOKED-P)."
  (declare (ignore command))
  (split-looked-p))

(defmethod place-lit-p ((split split) place)
  "Every match is lit."
  (declare (ignore place))
  t)

(defmethod place-cursor ((split split) buffer place)
  "A match's cursor is at its start."
  (place-bounds buffer place))

(defmethod ready-new-places ((split split) places)
  "A match needs nothing more."
  (declare (ignore places))
  nil)

(defgeneric virtual-spans (virtual buffer from to)
  (:documentation "The places of VIRTUAL in BUFFER's text from the line FROM is
on to the end of the line TO is on, (START . END) in order, but those it passes
over: the matches there a fold hides none of, or on each row the columns LEFT
to RIGHT, or the line's end."))

(defgeneric virtual-held-p (virtual split span)
  (:documentation "True when SPLIT, past its limit as VIRTUAL says, holds the
place SPAN, (START . END), already, or let it go (VIRTUAL-PASSED): a place
meeting it, or for rows, one reaching into its line."))

(defgeneric virtual-places-touch-p (virtual)
  (:documentation "True when two places of VIRTUAL may touch, which a split
refuses: matches may; rows, one to a line, never do."))

(defgeneric virtual-sight-key (virtual tail)
  (:documentation "TAIL, a list, with what the places of VIRTUAL in sight
depend on besides the text, the places held and the mode put before it: for
rows, the columns and how each row's place is made (VIRTUAL-FILL)."))

(defgeneric clear-padding (virtual buffer start end)
  (:documentation "Two values for a place of VIRTUAL in BUFFER from START to
END about to be emptied: the spaces put before what is typed there, and those
left after it. A row short of its block's left column is padded out to it, and
a tab across a block's edge leaves its columns outside it as spaces; a match
has none."))

(defgeneric virtual-words (virtual)
  (:documentation "What the places past a split's limit that it does not hold,
as VIRTUAL says, are called."))

(defgeneric write-virtual-places (virtual split kind &key typed text open close skip joins)
  (:documentation "Start the job writing at the places of SPLIT past its limit
that it does not hold, as VIRTUAL says, what was done at those it holds
\(START-VIRTUAL-JOB): KIND :TYPE as TYPED says (VIRTUAL-TYPED), the primary
place holding TEXT, or :DELETE, :WRAP or :UNWRAP in the pair OPEN and CLOSE;
the places of SKIP, held, left as they are, the change joining JOINS for undo.
Returns the job, :QUEUED, or NIL, saying why, when nothing is written."))

(defun split-here-p (&optional (buffer (current-buffer)))
  "True when a split is in force in BUFFER."
  (and *split* (eq (split-buffer *split*) buffer) (plusp (length (split-places *split*)))))

(defun places-phrase (split n)
  "N written as a count of the places of SPLIT, or of its cursors."
  (format nil "~:d ~a~:[s~;~]" n (place-noun split) (= n 1)))

(defun split-places-phrase (split)
  "The places or cursors of SPLIT counted, for what the echo area says: past its
limit, those held and how many there are in all."
  (let ((n (length (split-places split))) (virtual (split-virtual split)))
    (if virtual
        (format nil "~a of ~:[more than ~;~]~:d" (places-phrase split n)
                (virtual-exact virtual) (virtual-count virtual))
        (places-phrase split n))))

(defun split-hint (split)
  "What the echo area says while SPLIT lasts, worked out again only when
something it says has changed."
  (let* ((n (length (split-places split)))
         (virtual (split-virtual split))
         (phase (split-phase split))
         (mood (split-hint-mood split))
         (kept (and (eq phase :edit)
                    (or virtual (and *split-typing* (< (hash-table-count (split-typing-live *split-typing*)) n)))
                    t))
         (key (list phase mood kept n (and virtual (virtual-exact virtual))
                    (and virtual (virtual-count virtual)) (split-scope split) (split-hidden split)
                    tuatara.command:*keymaps-generation*))
         (said (split-reminder split)))
    (declare (dynamic-extent key))
    (if (equal key (car said))
        (cdr said)
        (cdr (setf (split-reminder split) (cons (copy-list key) (split-hint-text split kept mood)))))))

(defun split-say (control &rest arguments)
  "CONTROL, a format control naming keys as SUBSTITUTE-COMMAND-KEYS reads them,
with ARGUMENTS filled in: what a split's reminder says."
  (apply #'format nil (substitute-command-keys control :marks t :for-format t) arguments))

(defun split-edit-hint (split kept finish)
  "What the echo area says while typing at SPLIT's places, the places out of
sight still to be written when KEPT, FINISH saying what
\\<insert>\\[tuatara-normal] does."
  (split-say "Split: writing at ~a~:[~;, those out of sight as you finish~]; \\<insert>\\[tuatara-normal] ~a"
             (split-places-phrase split) kept finish))

(defparameter *split-did-words*
  '((:before . "typed before") (:after . "typed after") (:line-starts . "typed at the line starts of")
    (:line-ends . "typed at the line ends of") (:opened-below . "typed on new lines below")
    (:opened-above . "typed on new lines above") (:changed . "changed") (:deleted . "deleted")
    (:wrapped . "wrapped") (:unwrapped . "unwrapped") (:pasted . "pasted at") (:edited . "edited at"))
  "What a split says its verb did (SPLIT-DID) to its places as it ends.")

(defun places-left-words (n)
  "What is said of N places left as they were when the typing at a split's
places ended, as a job at the places out of sight says it (JOB-REPORT); \"\"
for none."
  (if (plusp n) (format nil "; ~:d left as they were, the text beside them not the same" n) ""))

(defun split-report (split)
  "What to say as SPLIT ends: what its verb did, and to how many places, how
many the typing left as they were, and how many matches it left alone because
a fold hides them."
  (let* ((written (- (length (split-places split)) (split-left split)))
         (places (if (split-virtual split)
                     (format nil "~a in sight~:[~;, ~a in the background (~a stops it)~]"
                             (places-phrase split written)
                             (virtual-started (split-virtual split)) (virtual-words (split-virtual split))
                             (substitute-command-keys "\\[stop-job]"))
                     (places-phrase split written)))
         (did (and (not (eql (split-tick split) (tuatara.buffer:buffer-content-number (split-buffer split))))
                   (cdr (assoc (split-did split) *split-did-words*)))))
    (format nil "~:[Split ended: ~a left alone~;Split: ~:*~a ~a~]~a~a"
            did places (places-left-words (split-left split)) (hidden-left-words (split-hidden split)))))

(defun close-split-edit (split &key (background t))
  "End the typing at SPLIT's places: the places it has not written yet are
written, each judged alone, and all it wrote becomes one change for undo. A
place where the text deleted beside the others is not there is left as it
was, and counted (SPLIT-LEFT). Past the limit, the places not held are then
written in the background, unless BACKGROUND is NIL."
  (when (eq (split-phase split) :edit)
    (let ((typing *split-typing*)
          (left (write-pending-places split)))
      (setf (split-left split) left)
      (when (plusp left)
        (message "Written at ~a~a" (places-phrase split (- (length (split-places split)) left))
                 (places-left-words left)))
      (tuatara.buffer:merge-undo-groups (split-buffer split) (split-since split))
      (setf (tuatara.buffer:buffer-change-functions (split-buffer split))
            (remove 'split-note-change (tuatara.buffer:buffer-change-functions (split-buffer split))))
      (setf (split-phase split) :select)
      ;; past the limit, the places not held are written as those held were
      (when (and typing background (split-virtual split))
        (start-virtual-job split :typing typing)))))

(defun dissolve-split (&key quiet (background t))
  "Take the split in force down, saying what it did unless QUIET; BACKGROUND
NIL leaves the places past its limit that it does not hold as they are. Its
keys go with it, whichever keymap it armed (ARM-SPLIT)."
  (let ((split *split*))
    (when split
      (setf *split* nil)
      (close-split-edit split :background background)
      (unless quiet (message "~a" (split-report split)))
      (when (eq (editor-transient-exit *editor*) #'split-exit)
        (let ((*split-keeping* t)) (clear-transient-map *editor*))))))

(defun split-exit ()
  "When the split's keys are taken down by a key they do not have: end it."
  (unless *split-keeping* (dissolve-split)))

(defvar *split-undo-commands* '(undo cursors-undo)
  "The commands that take a change back. Before one runs, the typing at a
split's places ends, so that it is one change at every place, and a job still
writing out of sight what the undo takes back is stopped (SPLIT-BEFORE-UNDO).")

(defvar *split-redo-commands* '(redo cursors-redo)
  "The commands that make a change undone again. Before one runs, the typing at
a split's places ends, as before an undo (SPLIT-BEFORE-UNDO).")

(defun split-before-undo (editor command)
  "Before COMMAND runs, when it is an undo or a redo (*SPLIT-UNDO-COMMANDS*,
*SPLIT-REDO-COMMANDS*) and a split is typed at in the current buffer: end the
split first. The places it holds are written and all that was typed is one
change, so that the undo takes it back at every place, those out of sight
too, and a redo makes it again at them all. The places past its limit that it
does not hold are left as they are. Before an undo, a job still writing out of
sight what the undo takes back in sight is stopped too (STOP-SPLIT-WRITER)."
  (declare (ignore editor))
  (let ((undo (member command *split-undo-commands*)))
    (when (or undo (member command *split-redo-commands*))
      (let ((split *split*))
        (when (and split (eq (split-buffer split) (current-buffer)) (eq (split-phase split) :edit))
          (dissolve-split :quiet t :background nil)
          (message "Split ended: what was typed at its places is one change")))
      (when undo (stop-split-writer (current-buffer))))))

(defun split-before-reload (buffer)
  "Before BUFFER is read again from its file while typing at the places of a
split there: end the split first, the places it holds written and what was
typed one change for undo, the reading again a step of its own; the places past
its limit that it does not hold are left as they are. Returns what to say of
it, or NIL."
  (let ((split *split*))
    (when (and split (eq (split-buffer split) buffer) (eq (split-phase split) :edit))
      (dissolve-split :quiet t :background nil)
      "the split typed at ended first")))

(defun split-save-refusal (buffer)
  "Why BUFFER may not be saved yet, for *SAVE-REFUSALS*: typing at a split's
places there, or a split's writing at the places out of sight still under way;
NIL when it may."
  (let ((split *split*) (writer (split-writer buffer)))
    (cond ((and split (eq (split-buffer split) buffer) (eq (split-phase split) :edit))
           (substitute-command-keys
            "Not saved: the split is still being typed at; \\<insert>\\[tuatara-normal] ends it, then save again"))
          ((and writer (rewrite-p (second writer)) (eq (rewrite-status (second writer)) :running))
           (format nil "Not saved: the split is still writing at ~a (~d%); save again when it is done"
                   (third writer) (rewrite-progress (second writer))))
          (writer
           (format nil "Not saved: the split's writing at ~a is not done yet; save again when it is" (third writer))))))

;;; Finding the places

(defun split-ranges (whole wide)
  "The stretches of the current buffer a split searches, (START . END) in
order, and what they are called: the whole buffer with WHOLE (the part shown
of a narrowed one); else what is selected (SELECTED-RANGES, WIDE as it takes
it), or the line at point."
  (let ((b (current-buffer)))
    (if whole
        (values (list (multiple-value-call #'cons (tuatara.buffer:buffer-bounds b)))
                (if (tuatara.buffer:buffer-narrowed-p b) "the part shown" "the buffer"))
        (multiple-value-bind (ranges words) (selected-ranges wide)
          (if ranges
              (values ranges words)
              (values (list (cons (tuatara.buffer:buffer-line-start b (point))
                                  (tuatara.buffer:buffer-line-end b (point))))
                      "this line"))))))

(defun selected-ranges (wide)
  "What is selected in the current buffer, as the stretches a split or a
replacement acts on, (START . END) in order, and what they are called: the
cursors' selections, or their lines; a rectangle's block, or with WIDE its
rows' whole lines (for a rectangle over more than *REWRITE-THRESHOLD*, a
BLOCK-SCOPE in place of the list, its rows read as the search comes to them);
banked lines with a selection made at the time; the selection. NIL when
nothing is."
  (let ((b (current-buffer)))
    (cond ((split-here-p b)
           (let* ((places (coerce (split-places *split*) 'list))
                  (selected (some #'cursor-selecting-p places)))
             (values (mapcar (lambda (place)
                               (if selected
                                   (multiple-value-bind (s e) (place-bounds b place) (cons s e))
                                   (multiple-value-bind (s e) (cursor-line-bounds b place) (cons s e))))
                             places)
                     (if selected "the cursors' selections" "the cursors' lines"))))
          ((and (rectangle-live-p b) (large-rectangle-p b))
           (let ((top (tuatara.buffer:buffer-line-start b (min (point) (tuatara.buffer:buffer-mark b))))
                 (bottom (tuatara.buffer:buffer-line-start b (max (point) (tuatara.buffer:buffer-mark b)))))
             (multiple-value-bind (left right) (rectangle-columns b)
               (values (make-block-scope top (next-line-start b bottom) left right wide (buffer-crlf-p b)
                                         (tuatara.display:tab-width-of b))
                       (if wide "the block's rows" "the block")))))
          ((rectangle-live-p b)
           (values (mapcar (lambda (row)
                             (if wide
                                 (cons (tuatara.buffer:buffer-line-start b (first row))
                                       (tuatara.buffer:buffer-line-end b (first row)))
                                 (cons (first row) (second row))))
                           (region-rectangle-rows b))
                   (if wide "the block's rows" "the block")))
          ((banked-p b)
           (let* ((banked (coerce (banked-spans-now b) 'list))
                  (lines (span-line-count b banked)))
             (multiple-value-bind (start end) (selection-range)
               (values (tuatara.text:merge-ranges
                        (sort (append (mapcar (lambda (span)
                                                (cons (car span)
                                                      (if (member (buffer-char-before b (cdr span))
                                                                  '(#\Newline #\Return))
                                                          (tuatara.buffer:buffer-forward-char b (cdr span) -1)
                                                          (cdr span))))
                                              banked)
                                      (and start (< start end) (list (cons start end))))
                              #'< :key #'car))
                       (format nil "~d banked line~:p~:[~; and the selection~]" lines (and start (< start end)))))))
          (t (multiple-value-bind (start end) (selection-range)
               (and start (< start end)
                    (values (list (cons start end)) "the selection")))))))

(define-setting *split-limit* 100000
  "The most places \\<normal>\\[split-selection] holds. What is typed is written
as it is typed only at the places in sight, so a key costs the same however
many there are; holding the places, and writing the others when the typing
ends, take about a second a million. Past the limit, the split holds the
places in sight, and the others are written in the background once the verb
is done at those. Raise it to hold more places at once, at the cost of 768
octets of heap for each (TUATARA_HEAP_MB sets the heap); lower it to keep the
editor light.
- a whole number, 0 or more: the most places; 100000 as standard.
- nil: as many as the memory has room for."
  :values (or (member nil) (integer 0 *)))

(defvar *split-budget* nil
  "Octets of heap a split may plan to use, or NIL for a quarter of what is free
when it is made. Its places are counted against it as they are found, so that a
split too large for the memory is refused before the heap runs out.")

(defconstant +split-key-octets+ 256
  "The heap each key typed at a place of a split keeps, for undo, with room to spare.")

(defconstant +split-place-octets+ (* 3 +split-key-octets+)
  "The heap a place of a split is planned to take: itself, and what typing at
it keeps for undo, written once when the typing ends, with room to spare.")

(defun place-limit (&optional (setting *split-limit*))
  "Three values: the most places a split may hold now, matches or cursors, the
lower of SETTING (*SPLIT-LIMIT* for \\<normal>\\[split-selection],
*CURSORS-LIMIT* for \\[add-cursor-below], *RECTANGLE-CURSORS-LIMIT* for
\\[tuatara-change] on a rectangle; NIL for none but the memory's) and what the
memory allows (*SPLIT-BUDGET*); :MEMORY when it is the memory's, else
:SETTING; and the memory's alone."
  (let ((memory (floor (heap-budget *split-budget*) +split-place-octets+)))
    (if (and setting (<= setting memory))
        (values setting :setting memory)
        (values memory :memory memory))))

(defun place-count-cap (&optional (setting *split-limit*))
  "How many places to count, at most, before PAST-LIMIT-CHOICE: one more than
there may be (PLACE-LIMIT), or than the memory has room for when the question
may be asked (*SPLIT-LIMIT-ASK*)."
  (multiple-value-bind (limit why memory) (place-limit setting)
    (1+ (if (and (eq why :setting) *split-limit-ask*) memory limit))))

(defun past-limit-choice (n exact noun &optional (setting *split-limit*))
  "Whether to hold every one of N places, matches or cursors (NOUN, as
\"cursors\"), or only those in sight, the others written in the background. The
answer is :ALL when they are no more than there may be (PLACE-LIMIT). Past
SETTING the question is asked, when *SPLIT-LIMIT-ASK* is true, N is counted
EXACT and the memory has room. It is asked the same for
\\<normal>\\[split-selection], \\[add-cursor-below] and \\[tuatara-change] on a
rectangle. The answer is :ALL when it is answered yes, else :IN-SIGHT."
  (multiple-value-bind (limit why memory) (place-limit setting)
    (if (or (<= n limit)
            (and (eq why :setting) *split-limit-ask* exact (<= n memory)
                 (y-or-n-p* (format nil "Hold all ~:d ~a?" n noun)
                            :details (format nil "y    holds them all, every key slow at that many~%~
                                                  n    holds those in sight, the others written in the background"))))
        :all
        :in-sight)))

(defun split-room-to-write-p (split)
  "True when the heap has room, twice over, for one more key typed at the
places of SPLIT it is written at: those written so far and in sight while the
typing writes as it goes, else every place."
  (heap-room-p (* 2 +split-key-octets+
                  (if *split-typing*
                      (+ (hash-table-count (split-typing-live *split-typing*)) 4096)
                      (length (split-places split))))))

(defun distinct-spans (spans)
  "SPANS, (START . END), in order of their starts, each once. SPANS is
reordered in doing so."
  (let ((sorted (sort spans (lambda (a b) (or (< (car a) (car b))
                                              (and (= (car a) (car b)) (< (cdr a) (cdr b))))))))
    (loop for rest on sorted
          unless (and (cdr rest) (equal (first rest) (second rest)))
          collect (first rest))))

(defun split-search (matcher ranges &key limit halt)
  "Every match of MATCHER (SPLIT-MATCHER) in RANGES of the current buffer,
(START . END) in order, the empty match at a range's end too (STRETCH-MATCHES).
The empty line after the last newline holds no match, and neither does text a
fold hides: a match it hides any of is left out (TUATARA.DISPLAY:FOLDS-HIDING,
nothing looked at when the buffer has no folds). Three values: the matches,
NIL, and how many a fold hides; with LIMIT, the search stops once it finds
more than that many, returning NIL and true. HALT, when given, is asked now
and then whether to give way: NIL and :STOPPED then."
  (let* ((b (current-buffer)) (rope (tuatara.buffer:buffer-text b)) (folds (tuatara.display:folds-now b))
         (spans '()) (count 0) (hidden '()))
    (dolist (range ranges)
      (when (stretch-matches (lambda (s e m g)
                               (declare (ignore m g))
                               (if (and folds (tuatara.display:folds-hiding folds s e))
                                   (progn (push (cons s e) hidden) nil)
                                   (progn (push (cons s e) spans)
                                          (and limit (> (incf count) limit)))))
                             rope matcher (car range) (cdr range) t nil halt)
        (return-from split-search (values nil t 0)))
      (when (and halt (funcall halt))
        (return-from split-search (values nil :stopped 0))))
    (let ((spans (distinct-spans spans)))
      (values spans nil (length (distinct-spans hidden))))))

(defun split-where (rope position)
  "Where POSITION of the text ROPE is, in words: on which line, or at which
octet while the lines of a large text are still being counted."
  (if (tuatara.text:rope-indexed-p rope)
      (format nil "on line ~:d" (1+ (tuatara.text:byte->line rope position)))
      (format nil "at octet ~:d" position)))

(defun places-agree-p (buffer places)
  "True when every one of PLACES of BUFFER holds the text the first holds."
  (or (<= (length places) 1)
      (multiple-value-bind (s e) (place-bounds buffer (aref places 0))
        (let ((size (- e s)))
          ;; the lengths first, which cost no text
          (and (loop for i from 1 below (length places)
                     always (multiple-value-bind (s2 e2) (place-bounds buffer (aref places i)) (= (- e2 s2) size)))
               (or (zerop size)
                   (let ((first (tuatara.buffer:buffer-string buffer :start s :end e)))
                     (loop for i from 1 below (length places)
                           always (string= first (place-text buffer (aref places i)))))))))))

(defparameter *split-chooser-bindings*
  '(("i" split-insert) ("a" split-append) ("c" split-change)
    ("d" split-delete) ("w" split-wrap) ("C-g" split-quit))
  "The keys of a split while it waits for a verb, besides each delimiter's.")

(defvar *split-chooser-keymap* (make-keymap :name "split")
  "The keys a split takes while it waits for a verb:
\\<*split-chooser-keymap*>\\[split-insert] \\[split-append]
\\[split-change] \\[split-delete] \\[split-wrap], each
delimiter, \\[split-quit].")

(defvar *split-commands*
  '(split-insert split-append split-change split-delete
    split-wrap split-wrap-key split-quit)
  "The commands after which a split stays; the cursors' add to them.")

(defvar *split-inert-commands* '(tuatara-normal-inert universal-argument recenter scroll-by-wheel)
  "Commands that change nothing a split holds, which keep it:
\\<normal>\\[tuatara-normal-inert], a count, \\[recenter], the mouse wheel.")

(defun split-looked-p ()
  "True when the command just run only looked about while the split in force
waits for a verb: its buffer still the one here, its text as it was, nothing
selected, in NORMAL state; point and the view may have moved."
  (let ((split *split*))
    (and split (eq (split-phase split) :select)
         (split-here-p)
         (let ((b (split-buffer split)))
           (and (eql (split-tick split) (tuatara.buffer:buffer-content-number b))
                (not (tuatara.buffer:buffer-mark-active-p b))
                (eq (modal-state b) :normal))))))

(defun split-keep-p (command)
  "True while the split stays after COMMAND: one of its own, one that changes
nothing, or one that only moved about while it waits for a verb
(SPLIT-LOOKED-P); for the cursors, \\<normal>\\[split-selection] too, which leaves
them where nothing matches. Never once the split is gone: its keys then go
after the command."
  (and *split*
       (or (member command *split-commands*) (member command *split-inert-commands*)
           (split-keeps-p *split* command))))

(defun arm-split (split map)
  "Put MAP before every other keymap for SPLIT, the split ending when a key it
does not have is pressed."
  (setf (split-phase split) :select)
  (let ((*split-keeping* t))
    (set-transient-map *editor* map #'split-keep-p #'split-exit)))

(defun touching-words (rope a b)
  "What is said of the matches A and B of the text ROPE, each (START . END),
that touch: where they are, and their text, no more than 60 octets of it."
  (format nil "Matches touch ~a (~a)" (split-where rope (car a))
          (tuatara.text:rope-string rope :start (car a)
                                         :end (tuatara.text:rope-char-start rope (min (cdr b) (+ (car a) 60))))))

(defun spans-touch (buffer spans)
  "Why SPANS, matches in BUFFER in order, cannot be a split's places: where two
touch, as what to say; NIL when none do."
  (let ((touching (loop for (a b) on spans when (and b (<= (car b) (cdr a))) return (list a b))))
    (when touching
      (format nil "~a: a split needs a character between places"
              (touching-words (tuatara.buffer:buffer-text buffer) (first touching) (second touching))))))

(defun sight-touching (buffer virtual)
  "Why the matches of VIRTUAL in sight in BUFFER (SIGHT-RANGES) cannot be a
split's places: where two touch, as SPANS-TOUCH says it; NIL when none do."
  (loop for (from . to) in (sight-ranges buffer)
        thereis (spans-touch buffer (virtual-spans virtual buffer from to))))

(defun begin-split (split)
  "SPLIT, new, made the split of its buffer about to be held, the split in
force ended and the selection let go first."
  (let ((buffer (split-buffer split)))
    (when *split* (dissolve-split :quiet t))
    (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
    (fresh-selection buffer)
    (setf (split-tick split) (tuatara.buffer:buffer-content-number buffer))
    split))

(defun await-verb (split banked)
  "Make SPLIT, whose places are held, the split in force, and wait for a verb;
the lines BANKED for it are spent. Returns its reminder."
  (setf (split-primary split) (aref (split-places split) 0)
        (split-text split) (place-text (split-buffer split) (split-primary split))
        *split* split)
  (build-split-chooser-keymap)
  (arm-split split *split-chooser-keymap*)
  ;; spent once the split stands, as y and d spend them
  (when banked (spend-banks (split-buffer split) banked))
  (split-hint split))

(defun banked-for-split (buffer whole)
  "The lines banked in BUFFER that a split about to be made there spends: none
for one over the whole buffer, or while a split or a rectangle is there."
  (and (not whole) (not (split-here-p buffer)) (not (rectangle-live-p buffer))
       (coerce (banked-spans-now buffer) 'list)))

(defun hold-virtual-matches (buffer regexp scope words whole first &key (move t) count)
  "Hold the matches of REGEXP in SCOPE of BUFFER, named WORDS (the whole buffer
with WHOLE), more than a split holds whole (PLACE-LIMIT), as a split of those
in sight, as many as it may hold, and wait for a verb: the others are written
in the background once the verb is done at those held. Point moves to FIRST,
the first match's (START . END), unless MOVE is false; a match that a fold
hides any of is not held, and the writing in the background leaves it alone
and counts it. COUNT, when known, is how many there are in all. Matches in
sight that touch are refused as HOLD-SPLIT refuses them, before anything is
let go, point left where it was. Returns what to say."
  (multiple-value-bind (limit why) (place-limit)
    (let* ((banked (banked-for-split buffer whole))
           (version (tuatara.buffer:buffer-version buffer))
           (point (tuatara.buffer:buffer-point buffer))
           (virtual (make-matches-virtual :regexp regexp :matcher (split-matcher regexp (block-scope-p scope))
                                          :scope scope
                                          :anchors (mapcar (lambda (position)
                                                             (tuatara.text:make-anchor version (car position)
                                                                                       :bias (cdr position)))
                                                           (scope-positions scope))
                                          :count (or count limit) :exact (and count t) :limit limit :setting why)))
      (when (and move first) (goto (car first)))
      (let ((touching (sight-touching buffer virtual)))
        (when touching
          (goto point)
          (return-from hold-virtual-matches touching)))
      (let ((split (begin-split (make-split buffer))))
        (setf (split-virtual split) virtual
              (split-scope split) words)
        (virtual-fill split)
        (when (and (zerop (length (split-places split))) first)
          (setf (split-places split) (vector (make-place buffer (car first) (cdr first)))))
        (if (zerop (length (split-places split)))
            (format nil "Nothing matched ~a" regexp)
            (await-verb split banked))))))

(defun hold-split (buffer regexp spans scope whole &key (move t) (hidden 0))
  "Hold SPANS of BUFFER, the matches of REGEXP found in SCOPE (the whole buffer
with WHOLE), as the places of a new split and wait for a verb, point moved to
the first unless MOVE is false; the lines banked for it are spent. HIDDEN is
how many matches were left alone because a fold hides them, for what the split
says (SPLIT-HIDDEN). Returns what to say: the split's reminder, or why it is
not held."
  (let ((touching (spans-touch buffer spans)))
    (cond ((null spans) (format nil "Nothing matched ~a~a" regexp (hidden-left-words hidden)))
          (touching touching)
          (t
           (let* ((banked (banked-for-split buffer whole))
                  (split (begin-split (make-split buffer))))
             (setf (split-places split)
                   (map 'simple-vector (lambda (span) (make-place buffer (car span) (cdr span))) spans)
                   (split-scope split) scope
                   (split-hidden split) hidden
                   (split-agree split) (places-agree-p buffer (split-places split)))
             (when move (goto (car (first spans))))
             (await-verb split banked))))))

(defun split-in-background-p (buffer scope)
  "True when a split's search over SCOPE of BUFFER runs in the background: a
large rectangle's rows (a BLOCK-SCOPE), or stretches of *REWRITE-THRESHOLD*
octets or more in all, however many (banked lines), not the cursors'."
  (or (block-scope-p scope)
      (and (not (split-here-p buffer))
           (>= (scope-extent scope) *rewrite-threshold*))))

(defun split-input (text regex)
  "What TEXT typed at \\<normal>\\[split-selection]'s question asks for, as two
values: whether a leading % asks for the whole buffer, and the pattern after
it as a regular expression (REGEX tells whether it was typed as one)."
  (let ((whole (and (plusp (length text)) (char= (char text 0) #\%))))
    (values whole (pattern-regex (if whole (subseq text 1) text) regex))))

(define-command split-selection ()
  "Hold every match of a regexp in the selection, then wait for a verb:
\\<*split-chooser-keymap*>\\[split-insert] types before every match and
\\[split-append] after, \\[split-change] empties them first, \\[split-delete]
deletes them, \\[split-wrap] or any delimiter wraps them (again, to take the
pair off), and \\[split-quit] lets go. What is typed at one match appears at all
of them, and the writing is one change for undo. The selection says what is
searched: a rectangle's block (with \\[universal-argument] each row's whole
line), banked lines with any selection made at the time, the selection, or else
the line at point; the regexp with % in front, %foo, searches the whole buffer
whatever is selected. As the regexp is typed, how many matches it has there
shows before the prompt. Only what is shown is acted on, whatever is searched: a
match that a fold hides any of is no place and is left alone, and the split says
how many were (a folded headline's line selected is the line shown, not what its
fold hides). Case is ignored when the regexp has no capital letter. Matches that
touch are refused. Past *SPLIT-LIMIT* the matches in and near the window are
held, more as you scroll to them. What is done at them is written at the others
in the background once the verb is done; an undo straight after the verb stops
that writing, so that the verb is taken back whole. Matches that touch are
refused there too: those in or near the window when they are seen, and those
out of sight by the writing in the background, which stops when it finds two
and writes nothing. With *SPLIT-LIMIT-ASK* true, you are
asked first whether to hold them all, as \\[add-cursor-below] and
\\[tuatara-change] on a rectangle ask (not for a search in the background).
Moving about keeps the split waiting for its verb; any other key ends it, doing
its own job. A large text is searched in the background, on all but two cores at
once, the mode line saying how far it has got (\\[stop-job] stops it); the split
stands when the search ends, point left where it is, unless the text has changed
or another buffer is shown by then, or a selection, cursors or another split
were made meanwhile, which it leaves alone. From the cursors of
\\<normal>\\[add-cursor-below], each cursor's selection or line is searched, and
the matches take the cursors' place. A match of more than 4 KB (4,096 characters
of ASCII) that runs across lines and needs text further on, as
BEGIN[\\s\\S]*?END around a large block, can be missed;
\\[split-selection-thorough] finds every match of up to a megabyte."
  (let* ((wide (prefix-argument))
         (b (current-buffer))
         (selected (split-ranges nil wide))
         (counter (make-pattern-count)))
    (multiple-value-bind (whole regexp)
        (multiple-value-call #'split-input
          (unwind-protect
               (read-pattern "split on"
                             :count (lambda (text regex)
                                      (multiple-value-bind (whole regexp) (split-input text regex)
                                        (let ((scope (if whole (list (multiple-value-call #'cons
                                                                       (tuatara.buffer:buffer-bounds b))) selected)))
                                          ;; a rectangle's block is searched row by row as the split comes to it
                                          (if (block-scope-p scope)
                                              (stretch-pattern-count counter b '() "" t)
                                              (stretch-pattern-count counter b scope regexp t :every t))))))
            (tuatara.runtime:stop-background-work (pattern-count-job counter))))
      (if (zerop (length regexp))
          (message "Nothing to split on")
          (multiple-value-bind (scope words) (split-ranges whole wide)
            (if (split-in-background-p b scope)
                (start-split-search b regexp scope words whole)
                (multiple-value-bind (spans too-many hidden)
                    ;; a pattern slow on this text has its search in the background
                    (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                      (split-search (split-matcher regexp nil) scope :limit (place-count-cap)
                                                                     :halt (search-stopper nil nil)))
                  (case too-many
                    (:slow (start-split-search b regexp scope words whole))
                    (:stopped (message "Stopped: nothing split"))
                    (t (message "~a" (if (or too-many
                                             (and spans (not (spans-touch b spans))
                                                  (eq (past-limit-choice (length spans) t "matches") :in-sight)))
                                         (hold-virtual-matches b regexp scope words whole (first spans)
                                                               :count (and (not too-many) (length spans)))
                                         (hold-split b regexp spans words whole :hidden hidden))))))))))))

(define-thorough-command split-selection-thorough split-selection
  "Hold every match of a regexp in the selection, as \\<normal>\\[split-selection]
does, finding every match of up to a megabyte (about a million characters of
ASCII) whole however many lines it spans, where \\[split-selection] may miss one of
more than 4 KB that runs across lines and needs text further on; the writing
out of sight the same. A match of more than a megabyte is still cut short, or
missed when it needs text further on. As quick as \\[split-selection] for a
pattern kept within a line or no longer than 4 KB; about twice as slow for one
that may run on across lines without end. In its place, in the init file:
(bind-key \"f\" 'split-selection-thorough :map :normal).")

;;; Past the limit: the places in sight held, the others written in the background

(defun virtual-now (buffer anchor)
  "Where ANCHOR stands in BUFFER now."
  (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer)))

(defun virtual-scope-now (buffer virtual)
  "The scope VIRTUAL's matches are in, as it stands in BUFFER now."
  (scope-at (virtual-scope virtual)
            (mapcar (lambda (anchor) (virtual-now buffer anchor)) (virtual-anchors virtual))))

(defun lines-around (buffer from to)
  "Two values: the start of the line of BUFFER that FROM is on, and the end of
the line TO is on."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (values (tuatara.text:rope-line-start rope from) (tuatara.text:rope-line-end rope to))))

(defmethod virtual-spans ((virtual matches-virtual) buffer from to)
  "The matches there of VIRTUAL's pattern in its scope that a fold hides none
of (TUATARA.DISPLAY:FOLDS-HIDING)."
  (multiple-value-bind (from to) (lines-around buffer from to)
    (let ((rope (tuatara.buffer:buffer-text buffer))
          (scope (virtual-scope-now buffer virtual))
          (matcher (virtual-matcher virtual))
          (folds (tuatara.display:folds-now buffer))
          (spans '()))
      (flet ((found (s e m g)
               (declare (ignore m g))
               (unless (and folds (tuatara.display:folds-hiding folds s e)) (push (cons s e) spans))
               nil))
        (if (block-scope-p scope)
            (let ((a (max from (block-scope-top scope))) (z (min to (block-scope-high scope))))
              (when (< a z) (piece-matches #'found rope matcher scope nil a z nil)))
            (loop for (start . end) in scope
                  for a = (max from start) for z = (min to end)
                  when (<= a z) do (stretch-matches #'found rope matcher a z (= z end) nil nil))))
      (nreverse spans))))

(defmethod virtual-spans ((virtual rows-virtual) buffer from to)
  "On each row there the columns LEFT to RIGHT, or the line's end, but on the
lines VIRTUAL passes over (SHOWN)."
  (multiple-value-bind (from to) (lines-around buffer from to)
    (let ((a (max (virtual-now buffer (virtual-top virtual)) from))
          (z (min (1+ to) (1+ (virtual-now buffer (virtual-bottom virtual)))))
          (folds (and (virtual-shown virtual) (tuatara.display:folds-now buffer)))
          (spans '()))
      (when (< a z)
        (apply
         #'tuatara.text:map-line-slices
         (lambda (line s e start-column end-column line-end)
           (declare (ignore start-column end-column))
           (unless (and folds (tuatara.display:folds-hide-line-p folds line))
             (push (if (virtual-at-end virtual) (cons line-end line-end) (cons s e)) spans)))
         (tuatara.buffer:buffer-text buffer) a z (virtual-left virtual) (virtual-right virtual)
         (tuatara.display:screen-slice-keys (buffer-crlf-p buffer) (virtual-tab-width virtual))))
      (nreverse spans))))

(defmethod virtual-held-p ((virtual matches-virtual) split span)
  "A place meeting SPAN is held, or was let go."
  (let ((b (split-buffer split)))
    (flet ((meets-p (place)
             (multiple-value-bind (s e) (place-bounds b place)
               (and (<= s (cdr span)) (<= (car span) e)))))
      (or (some #'meets-p (places-in split (car span) (cdr span)))
          (some #'meets-p (virtual-passed virtual))))))

(defmethod virtual-held-p ((virtual rows-virtual) split span)
  "A place reaching into SPAN's line is held, or was let go."
  (let* ((b (split-buffer split))
         (start (tuatara.buffer:buffer-line-start b (car span)))
         (end (tuatara.buffer:buffer-line-end b (car span))))
    (and (or (places-in split start end)
             (some (lambda (place) (<= start (place-bounds b place) end)) (virtual-passed virtual)))
         t)))

(defmethod virtual-places-touch-p ((virtual matches-virtual))
  "Two matches may touch."
  t)

(defmethod virtual-places-touch-p ((virtual rows-virtual))
  "Rows are a line apart."
  nil)

(defmethod virtual-sight-key ((virtual matches-virtual) tail)
  "The matches depend on nothing more."
  tail)

(defmethod virtual-sight-key ((virtual rows-virtual) tail)
  "The rows' places depend on the cursors' column and how each row's place is made."
  (list* (virtual-column virtual) (virtual-left virtual) (virtual-right virtual)
         (virtual-at-end virtual) (virtual-pad virtual) tail))

(defmethod clear-padding ((virtual matches-virtual) buffer start end)
  "A match is emptied with no spaces."
  (declare (ignore buffer start end))
  (values 0 0))

(defmethod clear-padding ((virtual rows-virtual) buffer start end)
  "A row short of the left column padded out to it (with PAD), and with the
columns on the screen, a tab across either edge giving its columns outside."
  (let* ((tabs (virtual-tab-width virtual))
         (column (if tabs
                     #'tuatara.display:buffer-screen-column
                     #'tuatara.buffer:buffer-column)))
    (values (if (or (virtual-pad virtual) (and (< start end) tabs))
                (max 0 (- (virtual-left virtual) (funcall column buffer start)))
                0)
            (if (and (< start end) tabs)
                (max 0 (- (funcall column buffer end) (virtual-right virtual)))
                0))))

(defmethod virtual-words ((virtual matches-virtual))
  "The other matches of the pattern."
  (format nil "the other matches of ~a" (virtual-regexp virtual)))

(defmethod virtual-words ((virtual rows-virtual))
  "The other lines."
  "the other lines")

(defun catch-up-places (split places)
  "Make PLACES, just held by SPLIT while typing at its places, what the typing
made the others: narrowed to the edge they were narrowed to, or emptied (a
block's row padded out to its column), then what was typed written; from then
on each key is written at them as it is typed. A match without the very text
deleted beside the others is let go first, left as it is, and so is one whose
emptying would take part of the text a fold hides, but not all of it. Returns
the places kept."
  (let* ((b (split-buffer split)) (virtual (split-virtual split)) (typing *split-typing*)
         (since (and typing (split-diff (split-typing-original typing)
                                        (split-text split) (split-typing-edges typing)))))
    (labels ((pass (other)
               (when other
                 (let ((gone (make-hash-table :test 'eq)))
                   (dolist (place other) (setf (gethash place gone) t))
                   (flet ((kept (places) (remove-if (lambda (place) (gethash place gone)) places)))
                     (setf places (kept places)
                           (split-places split) (kept (split-places split))
                           (virtual-passed virtual) (append other (virtual-passed virtual)))))))
             (let-go ()
               (when (and since (split-diff-edges since) (split-exact-edges-p split))
                 (pass (remove-if-not (lambda (place) (eq (place-edit b place since nil :exact t) :differs)) places))))
             (let-go-hidden ()
               ;; a place whose emptying would take part of what a fold hides
               (let ((folds (tuatara.display:folds-now b)))
                 (when folds
                   (pass (remove-if-not (lambda (place)
                                          (multiple-value-bind (s e) (place-bounds b place)
                                            (tuatara.display:folds-cut folds s e)))
                                        places))))))
      (if (eq (virtual-mode virtual) :clear)
          ;; emptied as one change (joining the typing's), a block's short rows padded out
          (progn
            (let-go-hidden)
            (let-go)
            (let ((tails '()))
              (when places
                (let ((*split-copying* t))
                  (delete-spans b (held-spans-of b places) :folds-checked t :group (and typing t)
                                :replacements
                                (loop for place in places
                                      collect (multiple-value-bind (pad tail)
                                                  (multiple-value-call #'clear-padding virtual b
                                                    (place-bounds b place))
                                                (push tail tails)
                                                (and (plusp (+ pad tail)) (spaces (+ pad tail))))))))
              ;; each place is now its padding: the cursor goes after it, before the spaces past the block
              (loop for place in places
                    for tail in (nreverse tails)
                    do (let ((e (- (nth-value 1 (place-bounds b place)) tail))) (move-place b place e e)))))
          (progn
            (dolist (place places)
              (multiple-value-bind (s e) (place-bounds b place)
                (if (eq (virtual-mode virtual) :end) (move-place b place e e) (move-place b place s s))))
            (let-go))))
    (when typing
      (when since (write-places split (mapcar (lambda (place) (cons place since)) places)))
      (dolist (place places) (setf (gethash place (split-typing-live typing)) t)))
    places))

(defun virtual-fill (split &key prepare)
  "Hold the places of SPLIT's virtual part that have come into sight
(SIGHT-RANGES), as many as it may hold: a cursor at each of a split of cursors,
and while typing, or with PREPARE, each made what the verb made the others
(CATCH-UP-PLACES). A match that a fold hides any of is not held. Matches that
touch are never held. While the split waits for its first verb, the text as
it was found, none of those come into sight is held then, and the second
value is why, as SPANS-TOUCH says it. Afterwards those that touch are passed
over, for the writing in the background to refuse (EVERY-MATCH-EDITS).
Nothing is looked for again while the text, the places, what the verb made of
them and what is in sight are as they were. Returns those held."
  (let* ((virtual (split-virtual split))
         (b (split-buffer split))
         (room (and virtual (- (virtual-limit virtual) (length (split-places split)))))
         (ranges (and room (plusp room) (sight-ranges b)))
         (seen (and ranges (list* (tuatara.buffer:buffer-version b) (length (split-places split))
                                  (virtual-sight-key virtual (cons (virtual-mode virtual) ranges))))))
    (when (and seen (not (equal seen (virtual-seen virtual))))
      (setf (virtual-seen virtual) seen)
      (let ((new '())
            (taken (make-hash-table))
            (touch (virtual-places-touch-p virtual))
            (waiting (not (or prepare (eq (split-phase split) :edit)
                              (not (eql (split-tick split) (tuatara.buffer:buffer-content-number b)))))))
        (dolist (range ranges)
          (let ((spans (virtual-spans virtual b (car range) (cdr range))))
            (when touch
              (if waiting
                  (let ((touching (spans-touch b spans)))
                    (when touching (return-from virtual-fill (values nil touching))))
                  (loop for (before after) on spans
                        when (and after (<= (car after) (cdr before)))
                        do (setf (gethash (car before) taken) t (gethash (car after) taken) t))))
            (dolist (span spans)
              (unless (or (gethash (car span) taken) (virtual-held-p virtual split span))
                (setf (gethash (car span) taken) t)
                (push span new)))))
        (when new
          (let* ((sorted (sort new #'< :key #'car))
                 (places (mapcar (lambda (span) (make-place b (car span) (cdr span)))
                                 (subseq sorted 0 (min room (length sorted))))))
            (unless (or (eq (split-phase split) :edit) prepare)
              (ready-new-places split places))
            (setf (split-places split)
                  (merge 'simple-vector (copy-seq (split-places split)) (coerce places 'simple-vector) #'<
                         :key (lambda (place) (place-bounds b place))))
            (if (or prepare (eq (split-phase split) :edit))
                (catch-up-places split places)
                places)))))))

(defun held-spans-of (buffer places)
  "Where PLACES of BUFFER are now, (START . END), in their order, as a vector."
  (map 'simple-vector (lambda (place) (multiple-value-bind (s e) (place-bounds buffer place) (cons s e))) places))

(defun held-spans (split)
  "Where the places SPLIT holds are now, (START . END), in order, as a vector."
  (held-spans-of (split-buffer split) (split-places split)))

(defun held-lines (buffer spans)
  "The starts of the lines of BUFFER that SPANS, (START . END), reach into, as keys of a hash table."
  (let ((lines (make-hash-table)))
    (loop for (start . end) across spans
          do (loop for line = (tuatara.buffer:buffer-line-start buffer start)
                   then (next-line-start buffer line)
                   do (setf (gethash line lines) t)
                   while (< (tuatara.buffer:buffer-line-end buffer line) end)))
    lines))

;;; The verbs

(defun delete-places (split)
  "Delete the text of every place of SPLIT, one change (DELETE-SPANS); returns
what they held. When that would take part of the text a fold hides, but not
all of it, nothing is deleted: the fold is shown instead."
  (let* ((b (split-buffer split))
         (texts (map 'list (lambda (place) (place-text b place)) (split-places split))))
    (delete-spans b (held-spans split))
    texts))

(defun check-places-shown (split)
  "Refuse, saying so, when a place of SPLIT lies outside the part of its buffer
shown, as when the buffer was narrowed after the places were made: nothing is
done at a place that cannot be seen."
  (let ((b (split-buffer split)))
    (when (tuatara.buffer:buffer-narrowed-p b)
      (multiple-value-bind (low high) (tuatara.buffer:buffer-bounds b)
        (when (find-if (lambda (place) (multiple-value-bind (s e) (place-bounds b place) (or (< s low) (> e high))))
                       (split-places split))
          (refuse "Some ~as are outside the part of ~a shown: widen shows them again"
                  (place-noun split) (tuatara.buffer:buffer-name b)))))))

(defun check-split-writable (split)
  "Refuse, saying so, before a verb changes anything at the places of SPLIT:
when its buffer is read-only (CHECK-WRITABLE), or a place lies outside the part
of it shown (CHECK-PLACES-SHOWN)."
  (check-writable (split-buffer split))
  (check-places-shown split))

(defun enter-split-edit (split clear where &key (since (tuatara.buffer:buffer-undo-stack (split-buffer split))))
  "Start typing at every place of SPLIT: point at the primary place's start,
or its end when WHERE is :END; with CLEAR, every place emptied first, what
they held on the kill ring. Places that differ are narrowed to that edge, so
only what is typed is copied. What is changed from SINCE, the buffer's undo stack
before the verb began (as it is now by default), to the typing's end is one
change for undo, the emptying with the typing, as \\<normal>\\[tuatara-change]
is at every kind of many places: a split's matches, cursors, a rectangle's
rows. Refused, nothing changed, in a read-only buffer and while a place lies
outside the part of the buffer shown (CHECK-SPLIT-WRITABLE)."
  (check-split-writable split)
  (let* ((b (split-buffer split)) (virtual (split-virtual split)) (primary (split-edit-primary split clear where)))
    (unless (or clear (split-agree split))
      (loop for place across (split-places split)
            do (multiple-value-bind (s e) (place-bounds b place)
                 (let ((edge (if (eq where :end) e s))) (move-place b place edge edge)))))
    (when clear
      (let ((texts (delete-places split)))
        ;; past the limit only the places in sight are known: nothing to the kill ring
        (unless virtual (kill-in-parts texts))))
    (setf (split-did split) (cond (clear :changed) ((eq where :end) :after) (t :before)))
    (let ((*split-keeping* t)) (clear-transient-map *editor*))
    (multiple-value-bind (s e) (place-bounds b primary)
      (goto (if (eq where :end) e s))
      (setf (split-primary split) primary
            (split-text split) (place-text b primary)
            (split-bounds split) (cons s e)
            (split-edges split) '()
            (split-strayed split) nil
            (split-since split) since
            (split-phase split) :edit)
      ;; written as it goes where it can be seen, the rest at the end
      (setf *split-typing* (let ((typing (make-split-typing (split-text split))))
                             (setf (gethash primary (split-typing-live typing)) t)
                             typing)))
    (pushnew 'split-note-change (tuatara.buffer:buffer-change-functions b))
    (tuatara-insert)
    (echo "~a" (split-hint split))))

(defun live-split ()
  "The split in force in this buffer; signals when there is none."
  (unless (split-here-p) (refuse "No split here"))
  *split*)

(define-command split-insert ()
  "Type before every place of the split, keeping what is there: what is typed
at one appears at all. \\<insert>\\[tuatara-normal] ends it, keeping what
was typed."
  (enter-split-edit (live-split) nil :start))

(define-command split-append ()
  "Type after every place of the split, keeping what is there."
  (enter-split-edit (live-split) nil :end))

(define-command split-change ()
  "Empty every place of the split, then type at all of them. What they held
goes on the kill ring as one kill: one copy when they held the same text,
else one a line. When a place would take part of the text a fold hides, but
not all of it, nothing is emptied: the fold is shown instead, and the key
pressed again empties them; a match out of sight in such a place is left as
it is."
  (enter-split-edit (live-split) t :start))

(define-command split-delete ()
  "Delete every place of the split, one change for undo, and end the split.
What they held goes on the kill ring as one kill. When a place would take
part of the text a fold hides, but not all of it, nothing is deleted: the
fold is shown instead, and the key pressed again deletes; a match out of
sight in such a place is left as it is."
  (let ((split (live-split)))
    (check-split-writable split)
    (let ((texts (delete-places split)))
      (unless (split-virtual split) (kill-in-parts texts)))
    (setf (split-did split) :deleted)
    (when (split-virtual split) (start-virtual-job split :kind :delete))
    (dissolve-split)))

(defun wrap-places (split char)
  "Wrap every place of SPLIT (in a split of cursors, every one selecting) in
the pair of the wrap key CHAR (WRAP-PAIR), or take it off where it stands
outside every one; one change (DELETE-SPANS), the places still held inside."
  (check-split-writable split)
  (multiple-value-bind (open close) (wrap-pair char)
    (let* ((b (split-buffer split))
           (targets (wrap-targets split)))
      (if (null targets)
          (message "~a is undefined" (pressed-key-name))
          (multiple-value-bind (positions off)
              (wrap-spans b (mapcar (lambda (place) (multiple-value-list (place-bounds b place))) targets) open close)
            ;; each place goes back between its pair, or where the pair was
            (loop with closing = (tuatara.text:string-octet-length (string close))
                  for place in targets
                  for i from 0 by 2
                  do (move-place b place (aref positions i)
                                 (if off (aref positions (1+ i)) (- (aref positions (1+ i)) closing))))
            (setf (split-did split) (if off :unwrapped :wrapped)
                  (split-text split) (place-text b (split-primary split)))
            (when (split-virtual split)
              (start-virtual-job split :kind (if off :unwrap :wrap) :open (string open) :close (string close)))
            (settle-places split)
            (echo "~a" (split-hint split)))))))

(define-command split-wrap ()
  "Wrap every place of the split in the pair of the delimiter asked for, or
take the pair off where it stands outside every place already. The split
stays, so pairs nest and a verb can still follow."
  (wrap-places (live-split) (read-typed-char "Wrap every place in: " "to wrap in")))

(define-command split-wrap-key ()
  "Wrap every place of the split in the pair of the delimiter pressed, or take
it off; see split-wrap."
  (let ((key (editor-this-key *editor*)))
    (wrap-places (live-split) (tuatara.frontend:key-base key))))

(define-command split-quit ()
  "End the split, keeping what it changed. With cursors in a mark run, or
selecting, let go of the selections and keep the cursors."
  (when *split* (quit-split *split*)))

;;; Typing at every place

(defun note-edge (split side octets)
  "Note that the command deleted OCTETS just past SIDE (:BEFORE or :AFTER) of SPLIT's primary place."
  (push (cons side (tuatara.text:octets-string octets)) (split-edges split)))

(defun split-note-change (buffer batch)
  "After each change to the split's buffer while typing at its places: note a
deletion reaching past the primary place's edges, and a change away from the
places, and follow the primary place's bounds, through a background job's
result taken in too (which is no typing of yours)."
  (let ((split *split*))
    (when (and split (eq buffer (split-buffer split)) (eq (split-phase split) :edit))
      (destructuring-bind (s . e) (split-bounds split)
        (unless (or *split-copying* (tuatara.text:batch-swap batch))
          (loop for edit across (tuatara.text:batch-edits batch)
                for a = (tuatara.text:edit-start edit)
                for z = (tuatara.text:edit-end edit)
                for inserts = (plusp (tuatara.text:edit-insert-length edit))
                for deleted = (tuatara.text:edit-deleted edit)
                do (cond ((and (<= s a) (<= z e)))
                         ((and (not inserts) (< a s) (>= z s) (<= z e))
                          (note-edge split :before (subseq deleted 0 (- s a))))
                         ((and (not inserts) (<= s a e) (> z e))
                          (note-edge split :after (subseq deleted (- e a))))
                         ((and (not inserts) (< a s) (> z e))
                          (note-edge split :before (subseq deleted 0 (- s a)))
                          (note-edge split :after (subseq deleted (- e a))))
                         (t (setf (split-strayed split) t)))))
        (setf (split-bounds split) (cons (tuatara.text:map-position s batch :before)
                                         (tuatara.text:map-position e batch :after)))))))

(defun place-at (split position)
  "The place of SPLIT that POSITION is in or at the edge of, or NIL."
  (let* ((b (split-buffer split)) (places (split-places split))
         (lo (tuatara.runtime:bisect (mid 0 (length places)) (<= (place-bounds b (aref places mid)) position))))
    ;; the last place starting at or before POSITION, or the one after when an
    ;; empty place stands just there
    (loop for i from (max 0 (- lo 1)) below (min (length places) (+ lo 1))
          do (multiple-value-bind (s e) (place-bounds b (aref places i))
               (when (<= s position e) (return (aref places i)))))))

(defun line-break-offsets (text)
  "The offsets of the line breaks in TEXT."
  (loop for i from 0 below (length text) when (char= (char text i) #\Newline) collect i))

(defun common-ends (old new)
  "Two values: how many characters OLD and NEW share at the start, and at the
end after that."
  (let* ((prefix (or (mismatch old new) (length old)))
         (room (- (min (length old) (length new)) prefix))
         (suffix (loop for k from 0 below room
                       while (char= (char old (- (length old) 1 k)) (char new (- (length new) 1 k)))
                       count t)))
    (values prefix suffix)))

(defun edge-texts (edges)
  "The text deleted just before a place and the text deleted just after it,
as two values, from EDGES (oldest first, each (SIDE . TEXT), SIDE :BEFORE or
:AFTER)."
  (values (with-output-to-string (out)
            (dolist (edge (reverse edges)) (when (eq (car edge) :before) (write-string (cdr edge) out))))
          (with-output-to-string (out)
            (dolist (edge edges) (when (eq (car edge) :after) (write-string (cdr edge) out))))))

(defun split-diff (old new edges)
  "What turns a place holding OLD into one holding NEW, EDGES (oldest first,
each (SIDE . TEXT)) deleted just past its edges: a SPLIT-DIFF, or NIL when
nothing changes."
  (multiple-value-bind (before after) (edge-texts edges)
    (let ((changed (string/= old new)))
      (when (or changed edges)
        (multiple-value-bind (prefix suffix) (common-ends old new)
          (%make-split-diff :changed changed :edges (and edges t)
                            :before before :after after
                            :before-breaks (line-break-offsets before) :after-breaks (line-break-offsets after)
                            :whole (tuatara.text:string-octets new)
                            :middle (tuatara.text:string-octets (subseq new prefix (- (length new) suffix)))
                            :prefix-octets (tuatara.text:string-octet-length old :end prefix)
                            :suffix-octets (tuatara.text:string-octet-length old :start (- (length old) suffix))
                            :old-octets (tuatara.text:string-octet-length old)))))))

(defun char-steps (buffer text)
  "How many steps of BUFFER-FORWARD-CHAR in BUFFER pass over TEXT: its
characters, a carriage return and the newline after it one step where
BUFFER's lines end with both."
  (if (buffer-crlf-p buffer)
      (- (length text) (search-count (coerce '(#\Return #\Newline) 'string) text))
      (length text)))

(defun place-edit (buffer place diff previous-end &key exact)
  "Two values: the edit that changes PLACE of BUFFER as DIFF says (NIL when
none is needed), and where the place then reaches to. :DIFFERS when the
deletions past its edges cannot be made there the same way, that is the same number
of characters with the line breaks where they were, or with EXACT the very
same text; :MEETS when they would reach PREVIOUS-END, the end of the place
before (when given)."
  (multiple-value-bind (s e) (place-bounds buffer place)
    (let* ((before (split-diff-before diff))
           (after (split-diff-after diff))
           (from (if (plusp (length before))
                     (tuatara.buffer:buffer-forward-char buffer s (- (char-steps buffer before))) s))
           (to (if (plusp (length after)) (tuatara.buffer:buffer-forward-char buffer e (char-steps buffer after)) e)))
      (flet ((same-p (there text breaks)
               (if exact
                   (string= there text)
                   (and (= (length there) (length text)) (equal (line-break-offsets there) breaks)))))
        (cond ((not (and (or (zerop (length before))
                             (same-p (tuatara.buffer:buffer-string buffer :start from :end s)
                                     before (split-diff-before-breaks diff)))
                         (or (zerop (length after))
                             (same-p (tuatara.buffer:buffer-string buffer :start e :end to)
                                     after (split-diff-after-breaks diff)))))
               :differs)
              ((and previous-end (<= from previous-end)) :meets)
              (t (values (flet ((octets (vector) (and (plusp (length vector)) vector)))
                           (cond ((or (split-diff-edges diff) (/= (- e s) (split-diff-old-octets diff)))
                                  (tuatara.text:make-edit from to (octets (split-diff-whole diff))))
                                 ((split-diff-changed diff)
                                  (tuatara.text:make-edit (+ s (split-diff-prefix-octets diff))
                                                          (- e (split-diff-suffix-octets diff))
                                                          (octets (split-diff-middle diff))))))
                         to)))))))

(defun place-index (split place index)
  "Where PLACE is in the places of SPLIT, INDEX tried first; NIL when it is
none of them."
  (let ((places (split-places split)))
    (if (and (< -1 index (length places)) (eq (aref places index) place))
        index
        (let ((start (place-bounds (split-buffer split) place)))
          (position place places :start (max 0 (1- (places-index-range split start start))))))))

(defun write-places (split items &key check leave)
  "Write ITEMS of SPLIT, each (PLACE . DIFF): as the SPLIT-DIFF says, nothing
for NIL or :HERE (the primary place, as it should be already); one change
joining the command's for undo. With CHECK, ITEMS are in the order of their
places, each checked against the place before it (PLACE-EDIT), as that place
is written here or stands; the primary place too, whose deletion is made
already. A deletion past the edges is made at a match only
where the very text deleted is there, at a cursor where as many characters
are. True when written; else, writing nothing, NIL and why: :DIFFERS when the
text beside a place is not the same, :MEETS when a deletion would reach the
place before. With LEAVE, a place that cannot be written so is left as it is
and the others are written: the third value counts those left."
  (let ((b (split-buffer split)) (places (split-places split)) (left 0)
        (edits '()) (previous-end -1) (index -1) (exact (split-exact-edges-p split)))
    (loop for (place . diff) in items
          do (when check
               (let ((here (place-index split place (1+ index))))
                 (when here
                   ;; the place before, when not written just now, as it stands
                   (unless (= here (1+ index))
                     (setf previous-end
                           (if (plusp here) (nth-value 1 (place-bounds b (aref places (1- here)))) -1)))
                   (setf index here))))
             (if (or (null diff) (eq diff :here))
                 (multiple-value-bind (s e) (place-bounds b place)
                   ;; the deletion made here already has reached the place before
                   (when (and check (eq diff :here) (<= s previous-end))
                     (return-from write-places (values nil :meets)))
                   (setf previous-end e))
                 (multiple-value-bind (edit end) (place-edit b place diff (and check previous-end) :exact exact)
                   (cond ((not (member edit '(:differs :meets)))
                          (setf previous-end end)
                          (when edit (push edit edits)))
                         (leave
                          (incf left)
                          (setf previous-end (nth-value 1 (place-bounds b place))))
                         (t (return-from write-places (values nil edit)))))))
    (when edits
      (let ((*split-copying* t))
        (tuatara.buffer:buffer-edit b (coerce (sort edits #'< :key #'tuatara.text:edit-start) 'simple-vector)
                                    :group t)))
    (values t nil left)))

(defun write-every-place (split step here)
  "Write STEP (a SPLIT-DIFF, or NIL) at every place of SPLIT but HERE, each
checked against its neighbours; the places the typing has not written yet
(*SPLIT-TYPING*) are brought up to the primary place as last copied first,
each judged alone, one where that cannot be done left as it was; from then on
every key is written at them all. True when STEP is written; else NIL and
why, as WRITE-PLACES says. The third value counts the places left as they
were."
  (let ((typing *split-typing*) (left 0))
    (when typing
      (let ((since (split-diff (split-typing-original typing) (split-text split) (split-typing-edges typing)))
            (live (split-typing-live typing)))
        (when since
          (setf left (nth-value 2 (write-places split
                                                (loop for place across (split-places split)
                                                      collect (cons place (and (not (gethash place live)) since)))
                                                :check t :leave t))))
        (loop for place across (split-places split) do (setf (gethash place live) t))))
    (multiple-value-bind (written why)
        (or (null step)
            (write-places split (loop for place across (split-places split)
                                      collect (cons place (if (eq place here) :here step)))
                          :check t))
      (values written why left))))

(defun sight-ranges (buffer)
  "The stretches of BUFFER in sight or near it, as (FROM . TO) in order, none
touching another: in every window showing it, from a little before each
stretch of text the window shows to a little after it (what a fold or a search
filter hides between them left out), and around point."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (point (tuatara.buffer:buffer-point buffer))
         (ranges (list (cons (max 0 (- point *split-sight-octets*)) (min size (+ point *split-sight-octets*))))))
    (dolist (window (frames-windows *editor*))
      (when (eq (tuatara.display:window-buffer window) buffer)
        (let ((start (min size (tuatara.display:window-start window)))
              (rows (tuatara.display:window-rows window)))
          (loop for (from . to) in (or (tuatara.display:shown-ranges rows :buffer buffer) (list (cons start start)))
                for margin = (max *split-sight-octets* (* 4 (- to from)))
                do (push (cons (max 0 (- from margin)) (min size (+ to margin))) ranges)))))
    (tuatara.text:merge-ranges ranges)))

(defun places-come-into-sight (split typing)
  "The places of SPLIT come in sight or near it (SIGHT-RANGES) since TYPING, its
typing, last looked, in order. Each stretch in sight is found among the places
by halves, and only the places not looked at before are gone through, so that
a key costs the same however many places are already in sight."
  (let ((places (split-places split)) (new '()))
    (unless (eq places (split-typing-places typing))
      (setf (split-typing-places typing) places (split-typing-covered typing) '()))
    (loop for (from . to) in (sight-ranges (split-buffer split))
          do (multiple-value-bind (lo hi) (places-index-range split from to)
               (let ((i lo) (covered (split-typing-covered typing)))
                 (loop for (done-from . done-to) in covered
                       while (< i hi)
                       do (loop for j from i below (min hi done-from) do (push (aref places j) new))
                       (setf i (max i done-to)))
                 (when (< i hi)
                   (loop for j from i below hi do (push (aref places j) new))
                   (setf (split-typing-covered typing) (tuatara.text:merge-ranges (acons lo hi covered)))))))
    (nreverse new)))

(defun write-places-in-sight (split typing here step text edges)
  "Write while TYPING at SPLIT's places: STEP (a SPLIT-DIFF, or NIL) at the
places it has written, but HERE, where it was typed; and at the places come
into sight since it last looked (PLACES-COME-INTO-SIGHT), and HERE when not
written yet, all that was typed since it began, the primary place holding TEXT
and EDGES deleted past its edges by this command as well. Those are written
from then on. A deletion past the edges is checked against the neighbours of
the places written. True when written; else NIL and why, as WRITE-PLACES says."
  (let ((live (split-typing-live typing))
        (b (split-buffer split))
        (since :unknown)
        (items '()))
    (loop for place being the hash-keys of live
          do (push (cons place (if (eq place here) :here step)) items))
    (dolist (place (if (gethash here live)
                       (places-come-into-sight split typing)
                       (cons here (places-come-into-sight split typing))))
      (unless (gethash place live)
        (when (eq since :unknown)
          (setf since (split-diff (split-typing-original typing) text (append (split-typing-edges typing) edges))))
        (push (cons place since) items)
        (setf (gethash place live) t)))
    (if (find-if (lambda (item) (and (split-diff-p (cdr item)) (split-diff-edges (cdr item)))) items)
        (write-places split (sort items #'< :key (lambda (item) (place-bounds b (car item)))) :check t)
        (write-places split items))))

(defun copy-to-places (split here)
  "Write what the command did at the place HERE onto the other places of
SPLIT: its text, and the deletions just past its edges, one change joining the
command's for undo. While typing (*SPLIT-TYPING*), only at the places in sight
or near it, and those written so before (WRITE-PLACES-IN-SIGHT); the others
once the typing ends (WRITE-PENDING-PLACES). True when written; else, the
command's change left at HERE alone and the split as it was before it, NIL
and why (WRITE-PLACES)."
  (let* ((b (split-buffer split))
         (typing *split-typing*)
         (old (split-text split))
         (new (place-text b here))
         (edges (reverse (split-edges split)))
         (step (split-diff old new edges)))
    (setf (split-edges split) '())
    (multiple-value-bind (written why)
        (cond ((null step) t)
              ((null typing) (write-every-place split step here))
              (t (write-places-in-sight split typing here step new edges)))
      (when written
        (when (and typing edges)
          (setf (split-typing-edges typing) (append (split-typing-edges typing) edges)))
        (setf (split-text split) new))
      (values written why))))

(defun write-pending-places (split)
  "The typing at SPLIT's places is over: write the places it has not written
yet as the primary place stood when last copied, one change. Returns how many
were left as they were, the deletion past the edges not to be made there the
same way."
  (prog1 (if *split-typing* (nth-value 2 (write-every-place split nil nil)) 0)
    (setf *split-typing* nil)))

(defun split-sync (editor)
  "After every command while a split lasts: copy what was typed at the place
point is in onto the others, end the split when the typing is over or went
astray, and keep its reminder up."
  (declare (ignore editor))
  (let ((split *split*))
    (when split
      (let ((b (split-buffer split)))
        (when (and (split-virtual split) (live-buffer-p b) (eq (current-buffer) b)
                   (or (not (eq (split-phase split) :edit)) (eq (modal-state b) :insert)))
          ;; the places come into sight held, unless they touch
          (let ((touching (nth-value 1 (virtual-fill split))))
            (when touching
              (dissolve-split :quiet t)
              (message "Split ended: ~a" touching)
              (return-from split-sync))))
        (cond ((not (live-buffer-p b)) (setf *split* nil *split-typing* nil))
              ((not (eq (split-phase split) :edit))
               (when (and (null (editor-message *editor*)) (eq (current-buffer) b))
                 (echo "~a" (split-hint split))))
              ((not (eq (modal-state b) :insert))
               ;; the typing is over
               (close-split-edit split)
               (end-split-typing split))
              ((split-strayed split)
               (dissolve-split :quiet t)
               (message "Split ended: an edit away from the places"))
              (t (sync-typing split)))))))

(defun sync-typing (split)
  "After a command typed at SPLIT's places: copy what it did at the place
point is in onto the others, or end the split when that cannot be done."
  (let* ((b (split-buffer split))
         (here (place-at split (tuatara.buffer:buffer-point b))))
    (cond ((null here) (dissolve-split))
          (t
           (unless (eq here (split-primary split))
             ;; the place moved into holds what was last copied, as the places in sight then do
             (when *split-typing*
               (write-places-in-sight split *split-typing* here nil (split-text split) '()))
             (multiple-value-bind (s e) (place-bounds b here)
               (setf (split-primary split) here
                     (split-text split) (place-text b here)
                     (split-bounds split) (cons s e))))
           (if (not (split-room-to-write-p split))
               ;; the heap would run out: the split ends before it does
               (let ((n (length (split-places split))))
                 (dissolve-split :quiet t)
                 (message "Split ended: no room in memory to write at ~:d places; what was typed is at one of them"
                          n))
               (multiple-value-bind (written why) (copy-to-places split here)
                 (cond (written
                        (when (null (editor-message *editor*)) (echo "~a" (split-hint split))))
                       (t (dissolve-split :quiet t)
                          (message (if (eq why :differs)
                                       "Split ended: the text deleted beside this place differs at another; ~
it was deleted here only"
                                       "Split ended: an edit beside a place could not be made at every place"))))))))))

;;; Showing the places and their cursors

(defun places-index-range (split from to)
  "Where the places of SPLIT that reach into FROM to TO are in its places, as
two values: the index of the first and the index after the last."
  (let* ((b (split-buffer split)) (places (split-places split)) (n (length places))
         ;; the first place ending at or after FROM, the first starting after TO
         (lo (tuatara.runtime:bisect (mid 0 n) (< (nth-value 1 (place-bounds b (aref places mid))) from))))
    (values lo (tuatara.runtime:bisect (mid lo n) (<= (place-bounds b (aref places mid)) to)))))

(defun places-in (split from to)
  "The places of SPLIT that reach into FROM to TO."
  (multiple-value-bind (lo hi) (places-index-range split from to)
    (loop for i from lo below hi collect (aref (split-places split) i))))

(defun split-highlights (editor window rows)
  "The places the split holds among ROWS of WINDOW, in :split-place: every
match, or what each cursor selects."
  (declare (ignore editor))
  (let ((split *split*))
    (when (and split (eq (tuatara.display:window-buffer window) (split-buffer split)) (plusp (length rows)))
      (let ((b (split-buffer split)))
        (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer b)
              nconc (loop for place in (places-in split from to)
                          for (s e) = (multiple-value-list (place-bounds b place))
                          when (and (< s e) (place-lit-p split place))
                          collect (list s e :split-place)))))))

(defun split-cursor-positions (editor window rows)
  "Where the split's cursors are drawn in ROWS of WINDOW besides point: at the
start of every other match, or while typing as far into it as point is into
its own; in a split of cursors, at every other cursor."
  (declare (ignore editor))
  (let ((split *split*))
    (when (and split (eq (tuatara.display:window-buffer window) (split-buffer split)))
      (let* ((b (split-buffer split))
             (offset (and (eq (split-phase split) :edit)
                          (- (tuatara.buffer:buffer-point b) (car (split-bounds split))))))
        (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer b)
              nconc (loop for place in (places-in split from to)
                          unless (eq place (split-primary split))
                          collect (if offset
                                      (multiple-value-bind (s e) (place-bounds b place) (max s (min e (+ s offset))))
                                      (place-cursor split b place))))))))

(defvar *split-chooser-pairs* nil
  "The pairs (PAIR-DELIMITERS) the split's keys were last made with, or NIL
before then.")

(defun build-split-chooser-keymap ()
  "Make the keys a split waits for a verb on:
\\<*split-chooser-keymap*>\\[split-insert] \\[split-append]
\\[split-change] \\[split-delete] \\[split-wrap]
\\[split-quit], and each delimiter of *PAIR-DELIMITERS* as it
stands now: again only once those have changed."
  (let ((pairs (pair-delimiters)))
    (unless (and *split-chooser-pairs* (eq pairs *split-chooser-pairs*))
      (setf *split-chooser-keymap* (make-keymap :name "split")
            *split-chooser-pairs* pairs)
      (define-keys *split-chooser-keymap* *split-chooser-bindings*)
      (bind-free-delimiter-keys *split-chooser-keymap* 'split-wrap-key))))

(defun install-split (editor)
  "Bind the split's keys, and have EDITOR copy what is typed at its places and
show them; a buffer is not saved while a split is written at there, and a split
typed at ends before its buffer is read again from its file, and before an
undo or a redo."
  (build-split-chooser-keymap)
  (pushnew 'split-highlights *highlight-functions*)
  (pushnew 'split-cursor-positions *other-cursor-functions*)
  (pushnew 'split-save-refusal *save-refusals*)
  (pushnew 'split-before-reload *before-reload-functions*)
  (pushnew 'split-before-undo (editor-pre-command-functions editor))
  (pushnew 'split-sync (editor-post-command-functions editor)))
