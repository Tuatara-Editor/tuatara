;;;; src/text/version.lisp --- Text versions, edit batches and anchors.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A VERSION is a rope plus the batch of edits that produced it, and its
;;;; LINK, its place in a chain of the batches made since, which holds no
;;;; text.  Anyone holding an old version (a worker with a snapshot) can map
;;;; byte positions forward to the current version by walking the links and
;;;; mapping through each batch in O(log edits) (VERSION-BATCHES), and keeps
;;;; its own rope alive but never a later one.  An anchor holds no version,
;;;; only the version's link, so that an anchor resolved long ago keeps the
;;;; edits since alive but never a rope.  Versions no longer referenced are
;;;; garbage collected, so the history is exactly as long as something needs
;;;; it.

(in-package #:tuatara.text)

(define-record (swap (:constructor make-swap (before after forward backward)) (:copier nil))
  "A change of a whole text too large to describe edit by edit: the rope BEFORE
and the rope AFTER, with FORWARD mapping a byte position and a bias from the
one to the other and BACKWARD mapping back. A replacement across a file of many
gigabytes arrives this way."
  (before nil :type rope :read-only t :documentation "The rope a SWAP replaced.")
  (after nil :type rope :read-only t :documentation "The rope a SWAP put in its place.")
  (forward nil :type function :read-only t
           :documentation "SWAP's map of (POSITION BIAS) from the rope before to the rope after.")
  (backward nil :type function :read-only t
            :documentation "SWAP's map of (POSITION BIAS) from the rope after to the rope before."))

(setf (documentation 'make-swap 'function) "Return a swap from rope BEFORE to rope AFTER with the two position maps.")

(define-record (batch (:constructor %make-batch (edits deltas positions))
                     (:constructor %make-swap-batch (swap))
                     (:copier nil))
  "An applied edit batch: EDITS (a sorted simple-vector of EDIT), DELTAS (a
fixnum vector where DELTAS[k] is the total change in length after edits 0..k),
and POSITIONS (the vector APPLY-EDITS returned). A batch that swapped the whole
text has no edits and its SWAP instead."
  (edits #() :type simple-vector :read-only t :documentation "The sorted edits of BATCH.")
  (deltas (make-array 0 :element-type 'fixnum) :type fixvec :read-only t
          :documentation "Prefix sums of length change per edit of BATCH.")
  (positions (make-array 0 :element-type 'fixnum) :type fixvec :read-only t
             :documentation "The positions APPLY-EDITS returned for BATCH.")
  (swap nil :type (or null swap) :read-only t
        :documentation "The SWAP of a batch that replaced the whole text, or NIL."))

(define-record (link (:constructor make-link (map)) (:copier nil))
  "A version's place in its text's history, without its text: MAP takes a
position of the version before to this one (NIL for the first, otherwise a batch of
edits, or a function of a position and a bias for a swap of the whole text);
NEXT is the link of the version after, or NIL while it is current."
  (map nil :type (or null batch function) :read-only t
       :documentation "How a LINK's version maps positions of the one before: a
batch, a function, or NIL.")
  (next nil :type (or null link) :documentation "The link of the version after, or NIL."))

(setf (documentation 'make-link 'function) "Return a link whose position map from the version before is MAP.")

(define-record (version (:constructor %make-version (rope number batch &aux (link (make-link (batch-link-map batch)))))
                       (:copier nil))
  "One version of a text. ROPE is its content; NUMBER counts versions from 0;
BATCH is the batch that produced it from the previous version (NIL for the
first); LINK its place in the history without the text (anchors hold it),
which leads to the batches made since but to no later text."
  (rope (make-rope) :type rope :read-only t :documentation "The rope holding VERSION's text.")
  (number 0 :type fixnum :read-only t :documentation "VERSION's sequence number, 0 for the first.")
  (batch nil :type (or null batch) :read-only t :documentation "The batch that produced VERSION, or NIL for the first.")
  (link nil :type link :read-only t :documentation "VERSION's place in the history, without its text (a LINK)."))

(defun batch-link-map (batch)
  "What a link keeps of BATCH to map positions through it: the batch, or for a
swap of the whole text its function forward alone, which holds no rope."
  (and batch (if (batch-swap batch) (swap-forward (batch-swap batch)) batch)))

(defun make-version (&optional (rope (make-rope)))
  "Return a first version holding ROPE."
  (%make-version rope 0 nil))

(defun version-current-p (version)
  "True when nothing has replaced VERSION yet."
  (null (link-next (version-link version))))

(defun edits-batch (rope edits)
  "Apply EDITS (a sequence of EDIT, sorted and non-overlapping) to ROPE, which is
left unchanged: the new rope, and the batch that made it, as an undo step
takes it back. Complexity: that of APPLY-EDITS plus O(edits)."
  (multiple-value-bind (new positions) (apply-edits rope edits)
    (let* ((edits (coerce edits 'simple-vector))
           (n (length edits))
           (deltas (make-array n :element-type 'fixnum))
           (delta 0))
      (declare (type fixnum delta))
      (loop for k of-type fixnum from 0 below n
            for e of-type edit = (aref edits k)
            do (incf delta (- (edit-insert-length e) (edit-delete-length e)))
               (setf (aref deltas k) delta))
      (values new (%make-batch edits deltas positions)))))

(defun swap-batch (before after forward backward)
  "The batch of a swap of the whole text BEFORE for AFTER, FORWARD mapping a
byte position and a bias from the one to the other and BACKWARD back, as an
undo step takes it back."
  (%make-swap-batch (make-swap before after forward backward)))

(defun advance-version (version rope batch what)
  "The version after VERSION, which must be current, holding ROPE, made by
BATCH and linked after VERSION; returned with BATCH. When VERSION is not
current, an error saying WHAT applies to the newest version only."
  (unless (version-current-p version)
    (error "Version ~a is not current; ~a applies to the newest version only." (version-number version) what))
  (let ((new (%make-version rope (1+ (version-number version)) batch)))
    (setf (link-next (version-link version)) (version-link new))
    (values new batch)))

(defun version-commit (version edits)
  "Apply EDITS (a sequence of EDIT, sorted and non-overlapping) to VERSION, which
must be current, and return the new version, linked after VERSION. Also
returns the batch as a second value (EDITS-BATCH). Complexity: that of
APPLY-EDITS plus O(edits)."
  (multiple-value-bind (rope batch) (edits-batch (version-rope version) edits)
    (advance-version version rope batch "an edit")))

(defun version-swap (version rope forward backward)
  "Replace the whole text of VERSION, which must be current, with ROPE: return
the new version, linked after VERSION, and its batch, whose SWAP maps
positions with FORWARD and back with BACKWARD (functions of a byte position and
a bias). Complexity: O(1)."
  (advance-version version rope (%make-swap-batch (make-swap (version-rope version) rope forward backward)) "a swap"))

(defun version-adopt (version rope)
  "Make ROPE, the same octets as VERSION's text with more known of them (their
counts), the text of a new version linked after VERSION; VERSION must be
current. The new version's batch has no edits: every position stays where it
is. Returns the new version and its batch. Complexity: O(1)."
  (advance-version version rope (%make-batch #()
                                             (make-array 0 :element-type 'fixnum)
                                             (make-array 0 :element-type 'fixnum))
                   "adopting a text"))

(defun version-undo-edits (version)
  "Return the edits that undo the batch that produced VERSION, applicable to
VERSION's text, or NIL for a first version."
  (let ((batch (version-batch version)))
    (and batch (inverse-edits (batch-edits batch) (batch-positions batch)))))

(defun map-position (position batch bias)
  "Map byte POSITION in the text before BATCH to the text after it. BIAS is
:BEFORE or :AFTER: a position at an insertion point, or inside deleted text,
lands before or after the inserted text respectively. Complexity: O(log edits)."
  (declare (type fixnum position))
  (let ((swap (batch-swap batch)))
    (when swap
      (return-from map-position (funcall (swap-forward swap) position bias))))
  (let* ((edits (batch-edits batch))
         (deltas (batch-deltas batch))
         (n (length edits)))
    (declare (type simple-vector edits) (type fixvec deltas))
    (if (zerop n)
        position
        ;; k: the last edit whose start is < POSITION (bias :before) or <= POSITION
        ;; (bias :after); -1 when there is none.
        (let* ((strict (eq bias :before))
               (k (1- (tuatara.runtime:bisect (mid 0 n)
                        (let ((start (edit-start (the edit (aref edits mid)))))
                          (declare (type fixnum start))
                          (if strict (< start position) (<= start position)))))))
          (declare (type fixnum k))
          (if (< k 0)
              position
              (let* ((e (aref edits k))
                     (before (if (zerop k) 0 (aref deltas (1- k)))))
                (declare (type edit e) (type fixnum before))
                (if (> position (edit-end e))
                    (+ position (aref deltas k))
                    (+ (edit-start e) before
                       (if (eq bias :after) (edit-insert-length e) 0)))))))))

(defun map-forward (position from to bias)
  "Map byte POSITION in version FROM to version TO, which must be FROM or one of
its successors, with BIAS as in MAP-POSITION; FROM and TO may be versions or
their links. Complexity: O(versions between × log edits)."
  (declare (type fixnum position))
  (let ((link (if (version-p from) (version-link from) from))
        (to (if (version-p to) (version-link to) to)))
    (loop until (eq link to)
          do (let ((next (link-next link)))
               (unless next
                 (error "A version is not a successor of the one a position was given in."))
               (let ((map (link-map next)))
                 (setf position (if (functionp map) (funcall map position bias) (map-position position map bias))
                       link next))))
    position))

;;; What changed between two versions

(defun stretches-through (stretches batch)
  "STRETCHES, as CHANGED-STRETCHES gives them up to the text before BATCH,
carried through BATCH: each edit of it joined to the stretches it meets or
touches, those after it moved. STRETCHES are taken apart to make the result,
which is made in them where it can be. Complexity: O(edits + stretches)."
  (let* ((edits (batch-edits batch))
         (n (length edits))
         (k 0) (out '()) (spare '())
         ;; between stretches, the text before BATCH less the first version's; after it less before
         (shift 0) (delta 0))
    (declare (type fixnum n k shift delta))
    (loop
      (let ((stretch (first stretches))
            (edit (and (< k n) (svref edits k))))
        (unless (or stretch edit) (return))
        (let* ((start (min (if stretch (third stretch) most-positive-fixnum)
                           (if edit (edit-start edit) most-positive-fixnum)))
               (end start) (old-start (- start shift)) (grown 0))
          (declare (type fixnum start end old-start grown))
          ;; everything that meets what is gathered so far, gathered
          (loop
            (let ((stretch (first stretches))
                  (edit (and (< k n) (svref edits k))))
              (cond ((and stretch (<= (the fixnum (third stretch)) end))
                     (setf end (max end (the fixnum (fourth stretch)))
                           shift (- (the fixnum (fourth stretch)) (the fixnum (second stretch))))
                     (let ((cell stretches))
                       (setf stretches (cdr cell) (cdr cell) spare spare cell)))
                    ((and edit (<= (edit-start edit) end))
                     (setf end (max end (edit-end edit)))
                     (incf grown (- (edit-insert-length edit) (edit-delete-length edit)))
                     (incf k))
                    (t (return)))))
          (if spare
              (let ((cell spare))
                (setf spare (cdr cell) (cdr cell) out out cell)
                (setf (first (car cell)) old-start (second (car cell)) (- end shift)
                      (third (car cell)) (+ start delta) (fourth (car cell)) (+ end delta grown)))
              (push (list old-start (- end shift) (+ start delta) (+ end delta grown)) out))
          (incf delta grown))))
    (nreverse out)))

(defun version-batches (from to)
  "The batches of edits that made each version after FROM up to TO, in order:
NIL when TO is FROM, :WHOLE when the whole text was swapped between them or TO
is not after FROM. Only the history between them is read, never a text.
Complexity: O(versions between)."
  (let ((batches '()) (at (version-link from)) (end (version-link to)))
    (loop until (eq at end)
          do (let ((next (link-next at)))
               (unless (and next (batch-p (link-map next)))
                 (return-from version-batches :whole))
               (push (link-map next) batches)
               (setf at next)))
    (nreverse batches)))

(defun batch-rope-before (batch after)
  "The text BATCH, a batch of edits, was made to, from AFTER, the text it
made: its edits undone, AFTER left as it is. Complexity: that of APPLY-EDITS."
  (values (apply-edits after (inverse-edits (batch-edits batch) (batch-positions batch)))))

(defun changed-stretches (from to)
  "The stretches of a text that differ between its versions FROM and TO, TO
the later: a list in order of (OLD-START OLD-END NEW-START NEW-END), byte
offsets of each in FROM's text and in TO's, the text between them the same in
both; :WHOLE when that is not known, the whole text swapped between them or TO
not after FROM. The edits of every version between are taken in one pass.
Complexity: O(edits + stretches) for each version between."
  (let ((batches (version-batches from to)))
    (if (eq batches :whole)
        :whole
        (let ((stretches '()))
          (dolist (batch batches stretches)
            (setf stretches (stretches-through stretches batch)))))))

;;; A count of lines carried through the edits made since

(defvar *carried-count-octets* (* 1024 1024)
  "Octets of changed text, before and after, up to which a count of lines made
of one version of a text gives the line numbers of later versions
\(CARRY-TALLY); past it they wait for the text to be counted again.")

(define-record (change-window (:constructor make-change-window ()) (:copier nil))
  "A stretch of a text that differs between the version a count of its lines
was made of and a later one, with an octet more on each side: from OLD-START
below OLD-END in the text counted, from NEW-START below NEW-END in the later
one. BEFORE and AFTER are the line ends the text has gained before the window
and through it; FIRST-LINE the line at OLD-START once the count has passed
it, else -1; OLD-LINES and NEW-LINES the line ends in the window on each
side."
  (old-start 0 :type fixnum :documentation "Where a CHANGE-WINDOW begins in the text counted.")
  (old-end 0 :type fixnum :documentation "Where a CHANGE-WINDOW ends in the text counted.")
  (new-start 0 :type fixnum :documentation "Where a CHANGE-WINDOW begins in the text now.")
  (new-end 0 :type fixnum :documentation "Where a CHANGE-WINDOW ends in the text now.")
  (before 0 :type fixnum :documentation "The line ends the text has gained before a CHANGE-WINDOW.")
  (after 0 :type fixnum :documentation "The line ends the text has gained up to a CHANGE-WINDOW's end.")
  (first-line -1 :type fixnum
              :documentation "The line at a CHANGE-WINDOW's start in the text
counted, or -1 while the count has not passed it.")
  (old-lines 0 :type fixnum :documentation "The line ends in a CHANGE-WINDOW in the text counted.")
  (new-lines 0 :type fixnum :documentation "The line ends in a CHANGE-WINDOW in the text now."))

(setf (documentation 'make-change-window 'function) "Return a CHANGE-WINDOW, for CARRIED-WINDOWS to fill in.")

(define-record (carried-tally (:constructor %carry-tally (tally rope at)) (:copier nil))
  "TALLY, the count of the lines of ROPE, the text of one version, carried to
the versions after it: the line numbers of a text edited while it is counted,
known as soon as the count has passed them, as exact as the count. AT is the
link (VERSION-LINK) of the latest version it was carried to, which holds no
text; STRETCHES what differs between ROPE and that version's text
\(CHANGED-STRETCHES); WINDOWS those stretches as CHANGE-WINDOWs, in order;
TOUCHED, pairs of offsets, the stretches the edits reached since the carry
before, in the text now. LOST is :LOST once the edits cannot be carried (the
whole text swapped, a version not after the last, more changed than
*CARRIED-COUNT-OCTETS*), else NIL."
  (tally nil :type tally :read-only t :documentation "The count of lines a CARRIED-TALLY carries.")
  (rope nil :type rope :read-only t :documentation "The text a CARRIED-TALLY's count is of.")
  (at nil :type link :documentation "The link of the latest version a CARRIED-TALLY was carried to.")
  (stretches '()
             :documentation "What differs between a CARRIED-TALLY's text and the
latest it was carried to (CHANGED-STRETCHES).")
  (windows '() :documentation "The CHANGE-WINDOWs of a CARRIED-TALLY, in order.")
  (touched (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0) :read-only t
           :documentation "Pairs of offsets: the stretches the edits since a CARRIED-TALLY was last carried reached.")
  (lost nil :documentation "LOST once a CARRIED-TALLY cannot be carried, else NIL."))

(defun carry-tally (tally version)
  "Return TALLY, a count of the lines of VERSION's text, ready to be carried
to the versions after it (CARRIED-LINE-NUMBER and the others)."
  (%carry-tally tally (version-rope version) (version-link version)))

(defun note-touched (touched batch)
  "Carry TOUCHED, pairs of offsets, through BATCH, and add the stretches its
edits wrote, as offsets in the text after it."
  (loop for i from 0 below (fill-pointer touched) by 2
        do (setf (aref touched i) (map-position (aref touched i) batch :before)
                 (aref touched (1+ i)) (map-position (aref touched (1+ i)) batch :after)))
  (let ((edits (batch-edits batch)) (deltas (batch-deltas batch)))
    (dotimes (k (length edits))
      (let* ((edit (svref edits k))
             (start (+ (edit-start edit) (if (zerop k) 0 (aref deltas (1- k))))))
        (vector-push-extend start touched)
        (vector-push-extend (+ start (edit-insert-length edit)) touched)))))

(defun touched-p (touched start end)
  "True when one of TOUCHED's stretches reaches from START to END, both
included."
  (loop for i from 0 below (fill-pointer touched) by 2
          thereis (and (<= (aref touched i) end) (>= (aref touched (1+ i)) start))))

(defun carried-windows (carried version)
  "Carry CARRIED to VERSION, the version it was last carried to or one after
it: its CHANGE-WINDOWs, :LOST when the edits cannot be carried, from then on.
The line ends of a window are counted again only when the edits since reached
it, those of its old side only when it is new. Called on the editor's thread:
CARRIED is changed in place. Complexity: O(edits since it was last carried +
windows + the octets of the windows the edits reached)."
  (let ((link (version-link version)))
    (cond ((carried-tally-lost carried) :lost)
          ((eq (carried-tally-at carried) link) (carried-tally-windows carried))
          (t (let ((at (carried-tally-at carried))
                   (stretches (carried-tally-stretches carried))
                   (touched (carried-tally-touched carried)))
               (setf (fill-pointer touched) 0)
               (flet ((lose ()
                        (setf (carried-tally-stretches carried) '() (carried-tally-windows carried) '()
                              (carried-tally-lost carried) :lost)
                        (return-from carried-windows :lost)))
                 (loop until (eq at link)
                       do (let* ((next (link-next at))
                                 (map (and next (link-map next))))
                            (unless (batch-p map) (lose))
                            (setf stretches (stretches-through stretches map))
                            (note-touched touched map)
                            (setf at next)))
                 (let ((old (carried-tally-rope carried)) (new (version-rope version))
                       (spare (carried-tally-windows carried)) (free '()) (windows '()) (size 0)
                       (from -1) (to 0) (new-from 0) (new-to 0))
                   (declare (type fixnum size from to new-from new-to))
                   (flet ((finish ()
                            ;; the window from FROM below TO, as the one before it where it is the same
                            (incf size (+ (- to from) (- new-to new-from)))
                            (when (> size *carried-count-octets*) (lose))
                            (loop while (and spare (< (change-window-old-start (car spare)) from))
                                  do (let ((cell spare)) (setf spare (cdr cell) (cdr cell) free free cell)))
                            (let* ((same (and spare (= (change-window-old-start (car spare)) from)
                                              (= (change-window-old-end (car spare)) to)))
                                   (cell (cond (same (let ((cell spare)) (setf spare (cdr cell)) cell))
                                               (free (let ((cell free)) (setf free (cdr cell)) cell))
                                               (t (list (make-change-window)))))
                                   (window (car cell)))
                              (if same
                                  (when (or (/= (- new-to new-from)
                                                (- (change-window-new-end window) (change-window-new-start window)))
                                            (touched-p touched new-from new-to))
                                    (setf (change-window-new-lines window) -1))
                                  (setf (change-window-old-start window) from (change-window-old-end window) to
                                        (change-window-first-line window) -1
                                        (change-window-old-lines window) (rope-count-newlines old from to)
                                        (change-window-new-lines window) -1))
                              (setf (change-window-new-start window) new-from (change-window-new-end window) new-to)
                              (when (minusp (change-window-new-lines window))
                                (setf (change-window-new-lines window) (rope-count-newlines new new-from new-to)))
                              (setf (cdr cell) windows windows cell))))
                     (dolist (stretch stretches)
                       (destructuring-bind (old-start old-end new-start new-end) stretch
                         (declare (type fixnum old-start old-end new-start new-end))
                         (let ((start (max 0 (1- old-start))))
                           (if (and (>= from 0) (< start to))
                               (setf to (min (rope-octet-length old) (1+ old-end))
                                     new-to (min (rope-octet-length new) (1+ new-end)))
                               (progn (when (>= from 0) (finish))
                                      (setf from start
                                            to (min (rope-octet-length old) (1+ old-end))
                                            new-from (max 0 (1- new-start))
                                            new-to (min (rope-octet-length new) (1+ new-end))))))))
                     (when (>= from 0) (finish)))
                   (setf windows (nreverse windows))
                   (let ((gained 0))
                     (declare (type fixnum gained))
                     (dolist (window windows)
                       (setf (change-window-before window) gained)
                       (incf gained (- (change-window-new-lines window) (change-window-old-lines window)))
                       (setf (change-window-after window) gained)))
                   (setf (carried-tally-at carried) at
                         (carried-tally-stretches carried) stretches
                         (carried-tally-windows carried) windows))))))))

(defun carried-lost-p (carried version)
  "True when CARRIED cannot be carried to VERSION (CARRIED-TALLY's LOST): its
line numbers are then those of a count made again."
  (eq :lost (carried-windows carried version)))

(defun last-window (windows offset &key old)
  "The last of WINDOWS (CHANGE-WINDOWs) beginning at OFFSET or before, in the
text now, or with OLD in the text counted; NIL when none does."
  (let ((found nil))
    (dolist (window windows found)
      (if (<= (if old (change-window-old-start window) (change-window-new-start window)) offset)
          (setf found window)
          (return found)))))

(defun window-first-line (carried window)
  "The line at WINDOW's start in CARRIED's text counted, kept once the count
has passed it; NIL before."
  (if (>= (change-window-first-line window) 0)
      (change-window-first-line window)
      (let ((line (tally-line-number (carried-tally-tally carried) (carried-tally-rope carried)
                                     (change-window-old-start window))))
        (when line (setf (change-window-first-line window) line))
        line)))

(defun carried-line-number (carried version byte)
  "The line, from 0, holding offset BYTE of VERSION's text, from CARRIED's
count once it has passed that place; NIL before, or when the edits since
cannot be carried. Reads at most a segment of the text and the part of a
changed window before BYTE."
  (let ((windows (carried-windows carried version))
        (tally (carried-tally-tally carried)) (old (carried-tally-rope carried)))
    (unless (eq windows :lost)
      (let ((window (last-window windows byte)))
        (cond ((null window) (tally-line-number tally old byte))
              ((< byte (change-window-new-end window))
               (let ((line (window-first-line carried window)))
                 (and line (+ line (change-window-before window)
                              (rope-count-newlines (version-rope version) (change-window-new-start window) byte)))))
              (t (let ((line (tally-line-number tally old (+ (change-window-old-end window)
                                                            (- byte (change-window-new-end window))))))
                   (and line (+ line (change-window-after window))))))))))

(defun carried-line-start (carried version line)
  "Where line LINE, from 0, of VERSION's text starts, from CARRIED's count once
it has passed that line; NIL before, or when the edits since cannot be
carried. Reads at most a segment of the text, or of a changed window."
  (let ((windows (carried-windows carried version))
        (tally (carried-tally-tally carried)) (old (carried-tally-rope carried))
        (shift 0) (gained 0))
    (declare (type fixnum shift gained))
    (unless (eq windows :lost)
      (flet ((in-gap ()
               (let ((start (tally-line-start tally old (- line gained))))
                 (and start (+ start shift)))))
        (dolist (window windows (in-gap))
          (let ((first (window-first-line carried window)))
            (unless first (return nil))
            (let ((first (+ first (change-window-before window))))
              (when (<= line first) (return (in-gap)))
              (when (<= line (+ first (change-window-new-lines window)))
                (let ((end (rope-nth-newline (version-rope version) (change-window-new-start window)
                                             (change-window-new-end window) (- line first))))
                  (return (and end (1+ end)))))))
          (setf shift (- (change-window-new-end window) (change-window-old-end window))
                gained (change-window-after window)))))))

(defun carried-counted-lines (carried version)
  "Two values: the line ends CARRIED's count has passed in VERSION's text, from
its start, and the offset there; NIL when the edits since cannot be carried."
  (let ((windows (carried-windows carried version)))
    (unless (eq windows :lost)
      (multiple-value-bind (lines reached) (tally-counted-lines (carried-tally-tally carried))
        (let ((window (last-window windows reached :old t)))
          (cond ((null window) (values lines reached))
                ((< reached (change-window-old-end window))
                 (let ((line (carried-line-number carried version (change-window-new-start window))))
                   (and line (values line (change-window-new-start window)))))
                (t (values (+ lines (change-window-after window))
                           (+ (change-window-new-end window) (- reached (change-window-old-end window)))))))))))

;;; Anchors

(define-record (anchor (:constructor %make-anchor (link cached bias)) (:copier nil))
  "A byte position that follows the text through edits. CACHED is the position
in the version of LINK; ANCHOR-POSITION maps it forward on demand and caches
the result. BIAS is :BEFORE (stays before text inserted at it) or :AFTER
\(moves after it)."
  (link nil :type link :documentation "The link of the version ANCHOR's cached position is valid in.")
  (cached 0 :type fixnum :documentation "ANCHOR's position in the version of ANCHOR-LINK,
possibly stale; use ANCHOR-POSITION.")
  (bias :before :type (member :before :after) :documentation "ANCHOR's bias, :BEFORE or :AFTER."))

(defun make-anchor (version position &key (bias :before))
  "Return an anchor at byte POSITION of VERSION with BIAS (:BEFORE by default)."
  (declare (type fixnum position))
  (unless (<= 0 position (rope-octet-length (version-rope version)))
    (error "Anchor position ~a is outside the text (0..~a)." position (rope-octet-length (version-rope version))))
  (%make-anchor (version-link version) position bias))

(defun anchor-position (anchor version)
  "Return ANCHOR's byte position in VERSION, mapping it forward from the version
it was last resolved in and caching the result. VERSION must be that version or
a successor of it."
  (let ((link (version-link version)))
    (unless (eq (anchor-link anchor) link)
      (setf (anchor-cached anchor)
            (map-forward (anchor-cached anchor) (anchor-link anchor) link (anchor-bias anchor))
            (anchor-link anchor) link)))
  (anchor-cached anchor))

(defun anchor-move (anchor version position)
  "Set ANCHOR to byte POSITION of VERSION."
  (declare (type fixnum position))
  (unless (<= 0 position (rope-octet-length (version-rope version)))
    (error "Anchor position ~a is outside the text (0..~a)." position (rope-octet-length (version-rope version))))
  (setf (anchor-cached anchor) position
        (anchor-link anchor) (version-link version))
  anchor)
