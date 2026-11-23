;;;; lisp/search/record-filters.lisp --- A file of records narrowed by hiding the lines no filter leaves.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file of records, one a line (a log, a CSV under its header line), is
;;;; narrowed by the same commands and filters as a list: the lines no filter
;;;; leaves are hidden from its windows (TUATARA.DISPLAY:HIDE-RANGES), its text
;;;; never changed, and what each filter leaves is kept as runs of lines, so
;;;; that a file of gigabytes costs no more memory than the runs it shows.
;;;; Whether a buffer is narrowed so is its mode's to say
;;;; (FILTERS-HIDE-LINES-P).

(in-package #:tuatara.defaults)

;;; Records, a line each, hidden where no filter leaves them

(defun text-unchanged-since (version buffer)
  "True when BUFFER's text is as it was at VERSION, one of its versions:
nothing changed since, only more learnt of it (its lines counted)."
  (let ((batches (tuatara.text:version-batches version (tuatara.buffer:buffer-version buffer))))
    (and (listp batches)
         (every (lambda (batch)
                  (and (null (tuatara.text:batch-swap batch)) (zerop (length (tuatara.text:batch-edits batch)))))
                batches))))

(defun records-start (rope header &optional ends)
  "Where the records of ROPE begin, after its first HEADER records, a line
each or as ENDS ends them (RECORD-ENDS: a CSV's header may hold a quoted
line break): its start when it has none, its end when it has no more than
that."
  (let ((start 0) (total (tuatara.text:rope-octet-length rope)))
    (dotimes (i header start)
      (setf start (min total (nth-value 1 (record-bounds rope start ends)))))))

(defun add-run (runs from to)
  "Add the lines from FROM below TO to RUNS (LINES-MATCHING), made one with
the run before when it ends at FROM; nothing when FROM is not before TO."
  (declare (type fixnum from to))
  (when (< from to)
    (let ((fill (fill-pointer runs)))
      (if (and (plusp fill) (= from (aref runs (1- fill))))
          (setf (aref runs (1- fill)) to)
          (progn (vector-push-extend from runs) (vector-push-extend to runs))))))

(defun runs-turned-round (runs first low high)
  "Make the runs of RUNS from its FIRST place on, all within LOW below HIGH,
the lines between them there instead."
  (declare (type fixnum first low high))
  (let ((kept (subseq runs first)) (at low))
    (declare (type fixnum at))
    (setf (fill-pointer runs) first)
    (loop for i from 0 below (length kept) by 2
          do (add-run runs at (aref kept i))
             (setf at (aref kept (1+ i))))
    (add-run runs at high)))

(defun line-holding (rope at from crlf)
  "Three values for the line of ROPE holding octet AT: where it starts, where
its text ends (before its newline, and before a carriage return there with
carriage return and line feed (CRLF)) and where the line after it starts. FROM
is a line's start at or before AT, from which the text is read: a line holding
many matches is read once."
  (declare (type fixnum at from))
  (let* ((total (tuatara.text:rope-octet-length rope))
         (first (tuatara.text:rope-find-line-end rope :start from))
         (start from) (newline first))
    (when (and first (< first at))
      (setf start (1+ (or (tuatara.text:rope-find-line-end-backward rope :start (1+ first) :end at) first))
            newline (tuatara.text:rope-find-line-end rope :start at)))
    (if newline
        (values start
                (tuatara.text:rope-line-text-end rope start newline crlf)
                (1+ newline))
        (values start total total))))

(defun lines-found (matcher rope ranges crlf &key progress halt most ends hide carry)
  "LINES-MATCHING for one filter tried on whole lines, found by MATCHER as a
search finds it: a line is matched when any part of a match of one scan of
the stretch lies on it (MAP-MATCH-UNITS), from the line holding the match's
first octet through the line holding its last, or an empty match is in it.
With ENDS, the records that may hold line breaks are matched the same way,
whole, and each of their lines kept with them; with HIDE, the lines not
matched are those left. CARRY, when given, is (FROM . REACH): a match of the
text before ran on to REACH, so a stretch starting at FROM has its lines
matched up to there and its scan goes on from there. Nothing is decoded but
where a regular expression is tried. A fourth value, the CARRY for the text
after: when the last stretch's last match runs past its end, (END . REACH)."
  (let* ((total (tuatara.text:rope-octet-length rope))
         (runs (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (count 0) (looked 0) (onward nil)
         ;; with ENDS, the record walked to: its start (-1 before any), its text's end, the next one's start
         (at -1) (at-end 0) (at-after 0))
    (declare (type fixnum total count looked at at-end at-after))
    (block finding
      (loop for i from 0 below (length ranges) by 2
            when (< (aref ranges i) (aref ranges (1+ i)))
            do (let ((low (aref ranges i)) (high (aref ranges (1+ i)))
                     (first (fill-pointer runs))
                     (marked 0) (matched 0) (units 0) (told 0) (reach 0) (scan 0))
                 (declare (type fixnum low high first marked matched units told reach scan))
                 (labels ((read-record ()
                            (multiple-value-bind (text-end after) (funcall ends rope at)
                              (setf at-end (record-text-end rope text-end crlf) at-after after)))
                          (next-record ()
                            (when (>= at low) (incf units))
                            (setf at at-after)
                            (when (< at total) (read-record)))
                          (unit-holding (s from)
                            (if ends
                                (progn (loop while (and (<= at-after s) (< at total)) do (next-record))
                                       (values at at-end at-after))
                                (line-holding rope s from crlf)))
                          (take (start text-end after)
                            (declare (ignore text-end) (type fixnum start after))
                            (add-run runs (max start low) (min after high))
                            (when (>= start low)
                              (incf matched)
                              (when (and most (not hide) (>= (+ count matched) most))
                                (incf count matched)
                                (return-from finding))))
                          (try (s e m g)
                            (declare (ignore m g) (type fixnum s e))
                            (setf reach (max reach e)
                                  marked (map-match-units #'take #'unit-holding s e marked high))
                            (when (and progress (> (- s told) 262144))
                              (sb-ext:atomic-incf (car progress) (- s told))
                              (setf told s))
                            nil))
                   (setf told low marked low)
                   (when ends
                     ;; the record holding the stretch's first line: walked to from the last one
                     ;; when that is before it, else looked for
                     (if (<= 0 at low)
                         (loop while (and (<= at-after low) (< at total)) do (next-record))
                         (progn (setf at (line-record (make-record-memo) rope low
                                                      (min total (1+ (tuatara.text:rope-line-end rope low))) ends))
                                (read-record)))
                     (setf marked at))
                   (setf scan marked)
                   (when (and carry (= low (car carry)) (> (cdr carry) low))
                     ;; a match of the text before runs into this stretch
                     (try low (cdr carry) nil nil)
                     (setf scan (cdr carry)))
                   (when (< scan high)
                     (stretch-matches #'try rope matcher scan high (= high total) nil halt total))
                   (when ends
                     ;; the records to the stretch's end counted; one going on past it matched whole
                     (loop while (and (< at high) (< at-after high)) do (next-record))
                     (when (and (< at high) (> at-after high) (< marked at-after))
                       (stretch-matches #'try rope matcher (max high marked) at-after (= at-after total)
                                        nil halt total))
                     (when (< at high) (next-record)))
                   (when progress (sb-ext:atomic-incf (car progress) (max 0 (- high told))))
                   (unless ends
                     (setf units (+ (tuatara.text:rope-count-newlines rope low high)
                                    (if (and (< low high)
                                             (= high total)
                                             (not (tuatara.text:rope-line-start-p rope high))) 1 0))))
                   (incf looked units)
                   (if hide
                       (progn (runs-turned-round runs first low high) (incf count (- units matched)))
                       (incf count matched))
                   (setf onward (and (> reach high) (cons high reach)))
                   (when (and halt (funcall halt)) (return-from finding))))))
    (values (coerce runs '(simple-array fixnum (*))) count looked onward)))

(defun lines-decoded (filter rope ranges crlf &key progress halt most ends)
  "LINES-MATCHING for one FILTER that tries its pattern in its own way (a
column, SEARCH-FILTER-TEST): each line decoded and tried as it stands, a line
longer than *REWRITE-LINE-LIMIT* octets on its first that many; with ENDS a
record over several lines tried once, whole, its lines left or not with it.
Reads a block of the text at a time, and makes nothing for a line that does
not match."
  (let* ((holder (list (make-string 256)))
         (runs (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (count 0) (looked 0) (limit *rewrite-line-limit*) (since 0)
         (memo (and ends (make-record-memo))))
    (declare (type fixnum count looked since limit))
    (block matching
      (labels ((leaves-p (octets from to)
                 (let ((length (decode-into octets from to holder)))
                   (search-filter-leaves-line-p filter (car holder) length)))
               (try (line end next octets at)
                 ;; the line from LINE, its text to END, the next from NEXT; its octets in OCTETS from AT
                 (declare (type fixnum line end next at))
                 (incf since (- next line))
                 (when (and progress (> since 262144))
                   (sb-ext:atomic-incf (car progress) since)
                   (setf since 0))
                 (multiple-value-bind (start text-end after)
                     (if ends (line-record memo rope line next ends) (values line end next))
                   (let* ((first (= start line))
                          (leaves (if (and first (= after next))
                                      (leaves-p octets at (+ at (- (min end (+ line limit)) line)))
                                      ;; a line of a record over several lines: the record tried once, whole
                                      (record-verdict memo rope start text-end crlf limit holder
                                                      (lambda (text length)
                                                        (search-filter-leaves-line-p filter text length))))))
                     (when first (incf looked))
                     (when leaves
                       (when first (incf count))
                       (add-run runs line next)
                       (when (and most (>= count most)) (return-from matching)))))
                 ;; a string grown for a long line is not kept for the short ones after it
                 (when (> (length (the string (car holder))) 1048576)
                   (setf (car holder) (make-string 256)))
                 (when (and halt (zerop (logand looked 1023)) (funcall halt))
                   (return-from matching))))
        (loop for i from 0 below (length ranges) by 2
              do (map-piece-lines
                  (lambda (slice)
                    (let ((line (line-slice-start slice)) (line-end (line-slice-end slice)))
                      (multiple-value-bind (octets at) (slice-octets slice line (min line-end (+ line limit)))
                        (try line line-end (line-slice-next slice) octets at))))
                  rope (aref ranges i) (aref ranges (1+ i)) crlf))))
    (when progress (sb-ext:atomic-incf (car progress) since))
    (values (coerce runs '(simple-array fixnum (*))) count looked)))

(defun filter-lines-of (filter rope ranges crlf &key progress halt most ends carry)
  "LINES-MATCHING for FILTER alone: found as a search finds its pattern
\(LINES-FOUND, CARRY as it takes it, and the carry onward as a fourth
value), or each line decoded for a filter that tries it in its own way
\(LINES-DECODED)."
  (if (search-filter-test filter)
      (lines-decoded filter rope ranges crlf :progress progress :halt halt :most most :ends ends)
      (lines-found (search-filter-matcher filter) rope ranges crlf :progress progress :halt halt :most most :ends ends
                                                              :hide (search-filter-hide filter) :carry carry)))

(defun lines-matching (filters rope ranges crlf &key progress halt most ends carries)
  "The lines of ROPE starting in RANGES that every one of FILTERS leaves. RANGES
is a vector of fixnums, the start and end of each stretch in turn, each starting
at a line's start. Three values come back. They are the runs of lines left, a
vector of fixnums holding the start of each run's first line and the start of
the line after its last; how many lines they are; and how many were looked at by
the first filter. A filter tried on the whole line leaves every line on which
any part of a match lies, as one scan of the stretch finds the matches. That is
from the line holding a match's first octet through the line holding its last, a
match running over several lines leaving them all. An empty match leaves the
line it is in. Its pattern's text is skipped to at the speed of a search, and no
line is read as text but where a regular expression is tried. A filter trying a
column decodes each line (LINES-DECODED). PROGRESS, a cons, has the octets
looked at added to its car now and then. HALT, a function of none, stops the
work when it answers true, asked now and then. MOST, when given, stops it once
that many lines are left. The stretches are then taken a megabyte at a time, a
match running from one into the next as one scan finds it. With ENDS
\(RECORD-ENDS), the lines are those of records that may hold line breaks. A
record over several lines is matched whole, its lines left or not with it, and
counted once. CARRIES, one for each filter or NIL, and a fourth value, the same
for the text after RANGES, carry a match running past the end of one part of the
text into the next. LINES-FOUND takes and gives them."
  (flet ((through (ranges most carries)
           ;; every filter in turn, each on what the one before left
           (let ((count 0) (looked nil) (onward '()))
             (loop for (filter . more) on filters
                   for carry = (pop carries)
                   do (multiple-value-bind (runs left seen reach)
                          (filter-lines-of filter rope ranges crlf :progress progress :halt halt
                                                                   :most (and (null more) most)
                                                                   :ends ends :carry carry)
                        (push reach onward)
                        (setf ranges runs count left looked (or looked seen))))
             (values ranges count (or looked 0) (nreverse onward)))))
    (if (null most)
        (through ranges nil carries)
        (let ((parts '()) (count 0) (looked 0))
          (block pieces
            (map-line-pieces (lambda (piece)
                               (multiple-value-bind (runs left seen onward) (through piece (- most count) carries)
                                 (push runs parts)
                                 (incf count left)
                                 (incf looked seen)
                                 (setf carries onward))
                               (when (or (>= count most) (and halt (funcall halt))) (return-from pieces)))
                             rope ranges 1048576 (piece-lines)))
          (values (join-runs (nreverse parts)) count looked carries)))))

(defun map-line-pieces (function rope ranges size lines)
  "Call FUNCTION with each piece of RANGES, as LINES-MATCHING takes them, cut
and gathered into pieces of about SIZE octets and no more than LINES lines,
every cut at a line's start: a vector of the same kind, in order. A piece is
cut only when it is reached, so leaving FUNCTION early reads no more of ROPE."
  (let ((piece '()) (held 0) (held-lines 0))
    (declare (type fixnum held held-lines))
    (flet ((close-piece ()
             (when piece
               (let ((done (coerce (nreverse piece) '(simple-array fixnum (*)))))
                 (setf piece '() held 0 held-lines 0)
                 (funcall function done)))))
      (loop for i from 0 below (length ranges) by 2
            do (tuatara.text:map-rope-line-pieces
                (lambda (start end)
                  (let ((more (tuatara.text:rope-count-newlines rope start end)))
                    (when (> (+ held-lines more) lines) (close-piece))
                    (push start piece) (push end piece)
                    (incf held (- end start))
                    (incf held-lines more)
                    (when (>= held size) (close-piece))))
                rope (aref ranges i) (aref ranges (1+ i)) size :lines lines))
      (close-piece))))

(defun join-runs (parts)
  "The runs of PARTS, vectors of runs (LINES-MATCHING) in order, as one
vector, a run ending where the next begins made one with it."
  (let ((out (make-array (reduce #'+ parts :key #'length) :element-type 'fixnum)) (w 0))
    (dolist (part parts)
      (loop for i from 0 below (length part) by 2
            do (if (and (plusp w) (= (aref out (1- w)) (aref part i)))
                   (setf (aref out (1- w)) (aref part (1+ i)))
                   (setf (aref out w) (aref part i) (aref out (1+ w)) (aref part (1+ i)) w (+ w 2)))))
    (subseq out 0 w)))

(defun lines-matching-everywhere (filter rope ranges crlf progress halt hook &optional ends)
  "LINES-MATCHING for FILTER over RANGES of ROPE, cut into pieces of
*FILTER-PIECE* octets matched on all but two cores at once, the runs put
together in order: the same three values, the same as one scan finds them;
a piece into which a match of the pieces before runs is matched again from
where that match ends. Each piece begins by calling HOOK, when given
\(*REWRITE-PIECE-HOOK* as the work began); records ending as ENDS says
\(RECORD-ENDS). HALT, a function of none, asked after each piece is cut and
now and then while they are matched, stops the work when it answers true,
nothing found when that is before matching began."
  (let* ((pieces (let ((all '()))
                   (map-line-pieces (lambda (piece)
                                      (push piece all)
                                      (when (and halt (funcall halt))
                                        (return-from lines-matching-everywhere
                                          (values (make-array 0 :element-type 'fixnum) 0 0))))
                                    rope ranges *filter-piece* (piece-lines (sort-workers nil)))
                   (coerce (nreverse all) 'simple-vector)))
         (results (tuatara.runtime:run-tasks
                   (map 'simple-vector (lambda (piece)
                                         (lambda ()
                                           (when hook (funcall hook))
                                           (multiple-value-list
                                            (lines-matching (list filter) rope piece crlf
                                                            :progress progress :halt halt :ends ends))))
                        pieces)
                   :parallelism (max 1 (- (tuatara.runtime:cpu-count) 2))))
         (carry nil))
    (dotimes (k (length pieces))
      (when (and carry (= (car carry) (aref (aref pieces k) 0)) (> (cdr carry) (car carry)))
        (setf (svref results k)
              (multiple-value-list (lines-matching (list filter) rope (aref pieces k) crlf
                                                   :halt halt :ends ends :carries (list carry)))))
      (setf carry (first (fourth (svref results k)))))
    (values (join-runs (map 'list #'first results))
            (reduce #'+ results :key #'second)
            (reduce #'+ results :key #'third))))

(defun stretch-count (buffer stretches filter &key every ends stop)
  "How many matches FILTER's pattern has in STRETCHES of BUFFER, (START .
END) each: with EVERY each match, else the lines FILTER leaves
\(LINES-MATCHING), records ending as ENDS says (RECORD-ENDS); NIL once STOP
answers true. Counted on all but two cores, with EVERY on nine in ten."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (if every
        (count-in-stretches rope (search-filter-matcher filter) stretches :every t :stop stop)
        (let ((count (nth-value 1 (lines-matching-everywhere
                                   filter rope
                                   (coerce (loop for (start . end) in stretches collect start collect end)
                                           '(simple-array fixnum (*)))
                                   (buffer-crlf-p buffer) nil stop nil ends))))
          (and (not (and stop (funcall stop))) count)))))

(defun runs-octets (runs)
  "How many octets of the text RUNS (LINES-MATCHING) take."
  (loop for i from 0 below (length runs) by 2 sum (- (aref runs (1+ i)) (aref runs i))))

(defun all-records (rope header &optional ends)
  "Every record of ROPE as LINES-MATCHING takes stretches: from after its
first HEADER records, ending as ENDS says (RECORDS-START), to its end."
  (make-array 2
              :element-type 'fixnum
              :initial-contents (list (records-start rope header ends) (tuatara.text:rope-octet-length rope))))

(defun hidden-lines-bounds (rope runs crlf header &optional ends)
  "What to hide of ROPE (TUATARA.DISPLAY:HIDE-RANGES) so that only its first
HEADER records (RECORDS-START, ENDS as it takes it) and the lines of RUNS
\(LINES-MATCHING) are shown. Each range hidden runs from the line end of each
line shown to the one ending the last line hidden after it, from the carriage
return before a newline when carriage return and line feed (CRLF). The lines
hidden at the very start, before any line shown, run from the start to the first
line shown. O(runs)."
  (let* ((total (tuatara.text:rope-octet-length rope))
         (out (make-array (+ 2 (length runs)) :element-type 'fixnum :fill-pointer 0))
         (shown-end (records-start rope header ends)))
    (labels ((text-end-before (at)
               ;; where the text of the line ending just before AT, a line's start, ends
               (if (plusp at) (tuatara.text:rope-line-text-end rope 0 (1- at) crlf) 0))
             (hide (from to)
               ;; the lines from FROM, a line's start, to TO, the end of the last of them
               (let ((start (text-end-before from)))
                 (when (< start to) (vector-push start out) (vector-push to out)))))
      (loop for i from 0 below (length runs) by 2
            do (when (< shown-end (aref runs i))
                 (if (zerop shown-end)
                     ;; no line shown before these: they go with their line breaks
                     (hide 0 (aref runs i))
                     (hide shown-end (text-end-before (aref runs i)))))
               (setf shown-end (aref runs (1+ i))))
      (when (< shown-end total)
        (hide shown-end (if (and (plusp shown-end) (tuatara.text:rope-line-start-p rope total))
                            (text-end-before total)
                            total))))
    (coerce out '(simple-array fixnum (*)))))

(defun screen-deadline ()
  "The internal real time by which the work at a key typed shows what it found."
  (+ (get-internal-real-time) (ceiling (* *search-filter-screen-seconds* internal-time-units-per-second))))

(defun window-rows-of (buffer)
  "How many rows the window showing BUFFER has, 50 when none shows it."
  (let ((window (and *editor* (buffer-window buffer))))
    (if window (tuatara.display:window-height window) 50)))

(defun lines-on-screen (buffer chain)
  "The lines of BUFFER's text from the top of its window on that every filter
of CHAIN matches, as many as fill the window, or as are found within
*SEARCH-FILTER-SCREEN-SECONDS*: runs, as LINES-MATCHING gives them, and how
many lines they hold."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (window (and *editor* (buffer-window buffer)))
         (top (max (records-start rope (list-filtering-lines (list-filtering buffer)) (record-ends buffer))
                   (if window (tuatara.buffer:buffer-line-start buffer (tuatara.display:window-start window)) 0)))
         (deadline (screen-deadline)))
    (multiple-value-bind (runs count)
        (lines-matching chain rope (make-array 2
                                               :element-type 'fixnum
                                               :initial-contents (list top (tuatara.text:rope-octet-length rope)))
                       (buffer-crlf-p buffer)
                       :ends (record-ends buffer)
                       :most (window-rows-of buffer)
                       :halt (lambda () (> (get-internal-real-time) deadline)))
      (values runs count))))

;;; Records shown narrowed

(defun show-filtered-lines (buffer)
  "Hide the lines of BUFFER that its search filters leave out, as worked out
for the version of its text they are worked out for, carried on to the text
as it is (TUATARA.DISPLAY:HIDE-RANGES); every line shown when none is
worked out yet."
  (let* ((filtering (list-filtering buffer))
         (source (list-filtering-source filtering))
         (shown (list-filtering-shown filtering)))
    (tuatara.display:hide-ranges
     buffer (and source shown (hidden-lines-bounds (tuatara.text:version-rope source) shown (buffer-crlf-p buffer)
                                                   (list-filtering-lines filtering) (record-ends buffer)))
     :version source)))

(defun refilter-lines (buffer)
  "Narrow BUFFER, whose lines search filters hide, again after its filters or
its text changed: when its text changed since the filters were worked out,
every filter is worked out again for it as it is now, the lines edited
meanwhile included; else only those not worked out yet. With no filters
left, every line is shown."
  (let ((filtering (list-filtering buffer)))
    (stop-filtering-job filtering)
    (cond ((null (search-filter-chain filtering))
           (setf (list-filtering-source filtering) nil (list-filtering-shown filtering) nil
                 (list-filtering-total filtering) nil
                 *buffers-filtered-by-lines* (remove buffer *buffers-filtered-by-lines*))
           (tuatara.display:hide-ranges buffer nil))
          (t (pushnew buffer *buffers-filtered-by-lines*)
             (let ((source (list-filtering-source filtering)))
               (unless (and source (text-unchanged-since source buffer))
                 (dolist (filter (search-filter-chain filtering))
                   (setf (search-filter-source filter) nil (search-filter-kept filter) nil))
                 (setf (list-filtering-source filtering) (tuatara.buffer:buffer-version buffer)
                       (list-filtering-total filtering) nil)))
             (work-out-filters buffer filtering)
             (show-filtered-lines buffer)))))

(defun show-filtered (buffer)
  "Show BUFFER as its search filters narrow it now: a list made again from
the whole list it keeps (SHOW-FILTERED-LIST), or a file's lines hidden
\(SHOW-FILTERED-LINES)."
  (if (filters-lines-p buffer) (show-filtered-lines buffer) (show-filtered-list buffer)))

(defun refilter (buffer)
  "Narrow BUFFER, a list or a file of records, again after its filters
changed (REFILTER-LIST, REFILTER-LINES)."
  (if (filters-lines-p buffer) (refilter-lines buffer) (refilter-list buffer)))

(defun filtered-counts (buffer)
  "How many entries or records BUFFER's search filters leave and how many it
has, two values; NIL as the second while not known."
  (let ((filtering (list-filtering buffer)))
    (values (list-filtering-shown-count filtering) (filter-total filtering))))

(defun filters-mode-line (editor buffer)
  "Mode line function. In a file whose lines search filters hide, what they
leave of its records (FILTER-COUNT-WORDS), as in Filtered by kiwi: 12 of 1,000
records."
  (declare (ignore editor))
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-lines filtering) (search-filter-chain filtering))
      (filter-count-words filtering (list-filtering-total filtering)))))

(defun refilter-touched (editor)
  "Tick function: work the search filters out again in each buffer whose
lines they hide where a change reached into those lines, or replaced its
whole text (reading its file again, sorting it), in the background where
that takes time."
  (declare (ignore editor))
  (dolist (buffer *buffers-filtered-by-lines*)
    (cond ((not (live-buffer-p buffer))
           (setf *buffers-filtered-by-lines* (remove buffer *buffers-filtered-by-lines*)))
          ((tuatara.display:hidden-touched-p buffer)
           (refilter-lines buffer))))
  nil)

(defun refilter-reloaded (buffer)
  "After BUFFER's file was read again: when search filters hide its lines,
work them out again for its new text, in the background where that takes
time."
  (when (and (filters-lines-p buffer) (search-filter-chain (list-filtering buffer)))
    (refilter-lines buffer)))

(defun guard-hidden-lines (buffer edits)
  "Before-edit function (TUATARA.BUFFER:*BEFORE-EDIT-FUNCTIONS*): refuse
EDITS of BUFFER that would change the lines search filters hide there
\(TUATARA.DISPLAY:HIDDEN-EDIT-P), whatever command makes them. The whole
text replaced at once (reading the file again, a sort done in the
background) and undo and redo go through, the filters worked out again
after them (REFILTER-TOUCHED)."
  (let ((bounds (tuatara.display:hidden-bounds buffer)))
    (when (and bounds
               (not (and *editor* (member (editor-this-command *editor*) '(undo redo))))
               (not (and (= 1 (length edits))
                         (let ((edit (aref edits 0)))
                           (and (zerop (tuatara.text:edit-start edit))
                                (= (tuatara.text:edit-end edit) (tuatara.buffer:buffer-size buffer))
                                (null (tuatara.text:edit-insert edit)))))))
      (let ((rope (tuatara.buffer:buffer-text buffer)))
        (loop for edit across edits
              for start = (tuatara.text:edit-start edit)
              for end = (tuatara.text:edit-end edit)
              do (loop for i from (stretch-before bounds start) below (length bounds) by 2
                       while (<= (aref bounds i) end)
                       when (tuatara.display:hidden-edit-p edit (aref bounds i) (aref bounds (1+ i)) rope)
                         do (refuse-hidden-lines)))))))

(defun stretch-before (bounds position)
  "The index in BOUNDS (TUATARA.DISPLAY:HIDE-RANGES) of the first stretch
that ends at POSITION or after it, less one, by halving."
  (let ((k (tuatara.runtime:bisect (k 0 (ash (length bounds) -1)) (< (1+ (aref bounds (1+ (* 2 k)))) position))))
    (* 2 k)))
