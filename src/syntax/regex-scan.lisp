;;;; src/syntax/regex-scan.lisp --- Regular-expression search over decoded windows of a rope.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Regular expressions go through cl-ppcre over decoded windows, built from
;;;; the pattern's traits (regex-analysis.lisp): the text is decoded a window at
;;;; a time into buffers reused from one search to the next, and the scan jumps
;;;; to the fixed text or run of places the pattern needs wherever it has one.
;;;; Directional searches go parallel beyond a serial window, and can be stopped
;;;; between pieces.

(in-package #:tuatara.syntax)

(define-record (decoder (:constructor make-decoder ()) (:copier nil))
  "Buffers reused while decoding windows of a rope for a regex scan: the OCTETS
read, the decoded text in BASE (a base string, for ASCII windows) or WIDE (for
the rest), TABLE from character index to octet offset for WIDE, and SKIP for
the literal scan that skips ahead to a pattern's required text."
  (octets (tuatara.text:make-octets 0) :type tuatara.text:octets
          :documentation "A DECODER's buffer of the octets last read.")
  (skip (tuatara.text:make-octets 0) :type tuatara.text:octets
        :documentation "A DECODER's buffer for skipping ahead to a pattern's fixed text.")
  (base (make-string 0 :element-type 'base-char) :type simple-base-string
        :documentation "A DECODER's text decoded from an ASCII window, a base string.")
  (wide (make-string 0 :element-type 'character) :type (simple-array character (*))
        :documentation "A DECODER's text decoded from a window past ASCII.")
  (table (make-array 0 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (*))
         :documentation "A DECODER's octet offset of each character of its wide text."))

(setf (documentation 'make-decoder 'function) "Return a decoder with empty buffers; they grow on first use.")

(sb-ext:defglobal **spare-decoders**
    (tuatara.runtime:make-spares (lambda ()
                                   (make-decoder)) :keep-p (lambda (decoder)
                                                             (<= (length (decoder-octets decoder)) (* 256 1024))))
  "Decoders not in use, kept so that the pieces of a regex search on several
cores reuse their buffers instead of making new ones; one a long match grew
past 256 KB is let go.")

(defmacro with-decoder ((var) &body body)
  "Run BODY with VAR bound to a decoder taken from the spares and given back after."
  `(tuatara.runtime:with-spare (,var **spare-decoders**) ,@body))

(defun decode-window (decoder rope from to)
  "Decode ROPE's octets from FROM below TO into DECODER's buffers. Returns the
string, the number of characters in it (the string may be longer), and the
table from character index to octet offset from FROM, or NIL when every octet
is ASCII and the offsets equal the indices."
  (declare (type decoder decoder) (type fixnum from to) (optimize speed (safety 1)))
  (let ((n (- to from)))
    (declare (type fixnum n))
    (when (< (length (decoder-octets decoder)) n)
      (setf (decoder-octets decoder) (tuatara.text:make-octets n)))
    (let ((octets (decoder-octets decoder)))
      (tuatara.text:rope-copy-into rope from to octets)
      (if (loop for i of-type fixnum from 0 below n always (< (aref octets i) #x80))
          (progn
            (when (< (length (decoder-base decoder)) n)
              (setf (decoder-base decoder) (make-string n :element-type 'base-char)))
            (let ((string (decoder-base decoder)))
              (loop for i of-type fixnum from 0 below n
                    do (setf (schar string i) (code-char (aref octets i))))
              (values string n nil)))
          (progn
            (when (< (length (decoder-wide decoder)) n)
              (setf (decoder-wide decoder) (make-string n :element-type 'character)
                    (decoder-table decoder) (make-array (1+ n) :element-type '(unsigned-byte 32))))
            (let ((string (decoder-wide decoder)) (table (decoder-table decoder)) (i 0) (k 0))
              (declare (type fixnum i k))
              (loop while (< i n)
                    do (multiple-value-bind (code len) (tuatara.text:octets-decode octets i n)
                         (setf (schar string k) (code-char code) (aref table k) i)
                         (incf k) (incf i len)))
              (setf (aref table k) n)
              (values string k table)))))))

(declaim (inline index-octet))
(defun index-octet (table index)
  "Octet offset of character INDEX through TABLE, or INDEX itself for an ASCII window."
  (if table (aref (the (simple-array (unsigned-byte 32) (*)) table) index) index))

(defun octet-index (table octet count)
  "The first character index whose octet offset is at least OCTET through
TABLE (COUNT characters), or OCTET itself for an ASCII window."
  (declare (type fixnum octet count))
  (if table
      (locally (declare (type (simple-array (unsigned-byte 32) (*)) table))
        (tuatara.runtime:bisect (mid 0 count) (< (aref table mid) octet)))
      octet))

(defun matched-text (texts string start end)
  "What a scan gives FUNCTION for the text a match took, from START below END
of STRING: NIL without TEXTS; the text itself when TEXTS is T; else what
TEXTS, a function of STRING, START and END, makes of it (its letter case,
say), so that nothing need be copied for each of a great many matches."
  (cond ((null texts) nil)
        ((eq texts t) (subseq string start end))
        (t (funcall texts string start end))))

(defun matched-groups (texts string group-starts group-ends)
  "The texts of a match's groups, as a vector (NIL for a group that took no
part), when TEXTS is T and the pattern has groups; NIL otherwise."
  (and (eq texts t) (plusp (length group-starts))
       (map 'simple-vector (lambda (gs ge) (and gs (subseq string gs ge))) group-starts group-ends)))

(defun map-slice-matches (function rope scanner line start end &key decoder texts)
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED GROUPS), byte offsets, for
each match of SCANNER in ROPE starting at or after START and ending by END, a
slice of the line that starts at LINE: the line's text before START is the
pattern's context, none after END is seen, empty matches are reported, at END
too. MATCHED and GROUPS only with TEXTS, as MAP-REGEX-MATCHES gives them.
Made for many short slices, a rectangle's rows: it decodes no more than the
line up to END. Stops as soon as FUNCTION returns true and returns that value;
else returns NIL. DECODER, when given, holds the text decoded; else a spare
one does (WITH-DECODER)."
  (declare (type fixnum line start end) (type function function))
  (unless decoder
    (return-from map-slice-matches
      (with-decoder (spare) (map-slice-matches function rope scanner line start end :decoder spare :texts texts))))
  (multiple-value-bind (string count table) (decode-window decoder rope line end)
    (let ((from (octet-index table (- start line) count))
          (scanner (scanner-for-rope scanner rope)))
      (declare (type fixnum from))
      (loop
        (when (> from count) (return nil))
        (multiple-value-bind (ms me group-starts group-ends)
            (tuatara.runtime:stoppable (cl-ppcre:scan scanner string :start from :end count :real-start-pos 0))
          (unless ms (return nil))
          (let* ((bs (+ line (index-octet table ms)))
                 (be (+ line (index-octet table me)))
                 (result (funcall function bs be
                                  (matched-text texts string ms me)
                                  (matched-groups texts string group-starts group-ends))))
            (when result (return result))
            (setf from (if (= ms me) (1+ ms) me))))))))

(defun scanner-for-rope (scanner rope &optional (traits (gethash scanner **regex-traits**)))
  "The scanner to match SCANNER's pattern with in ROPE: where ROPE holds no
carriage return, its lines ending at newlines alone, the pattern as written
\(REGEX-TRAITS-PLAIN), which matches the same there and quicker; else SCANNER.
O(1)."
  (let ((plain (and traits (regex-traits-plain traits))))
    (if (and plain (not (tuatara.text:rope-holds-return-p rope))) plain scanner)))

(defun needle-line (octets filled needle fold from)
  "The line of the first occurrence of NEEDLE (folded when FOLD) in the
first FILLED octets of OCTETS at or after FROM, as two offsets. They are where
it starts, FROM at the earliest, and where it ends, after the octet that ends
it (TUATARA.TEXT:LINE-END-OCTET-P; a carriage return as the last octet filled
taken for one a newline may follow) or at FILLED. NIL when NEEDLE is not
there."
  (declare (type tuatara.text:octets octets needle) (type fixnum filled from) (optimize speed))
  (let ((at (scan-chunk octets needle fold from filled filled)))
    (when at
      (flet ((ends-line-p (i)
               (declare (type fixnum i))
               (let ((b (aref octets i)))
                 (or (= b 10) (and (= b 13) (< (1+ i) filled) (/= 10 (aref octets (1+ i))))))))
        (declare (inline ends-line-p))
        (let ((start (loop for i of-type fixnum from (1- (the fixnum at)) downto from
                           when (ends-line-p i) return (1+ i)
                           finally (return from)))
              (end (loop for i of-type fixnum from at below filled
                         when (ends-line-p i) return (1+ i)
                         finally (return filled))))
          (values start end))))))

(defun line-start-within (rope position floor)
  "Where the line holding POSITION of ROPE starts, or FLOOR when that is
later. Complexity: O(log n + POSITION - FLOOR)."
  (let ((newline (tuatara.text:rope-find-line-end-backward rope :start floor :end position)))
    (if newline (1+ newline) floor)))

(defun map-regex-matches (function rope scanner start end &key bound decoder halt (texts t) empty)
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED-STRING GROUPS), byte
offsets, the text matched and a vector of the groups' texts (NIL for a group
that took no part; NIL instead of the vector for a pattern without groups).
Both are NIL when TEXTS is NIL, which spares copying them. With TEXTS a
function, it is what it makes of the text and no groups: MATCHED-TEXT. It is
called for each non-empty, non-overlapping match of SCANNER in ROPE that
starts at or after START and before END, in order. With EMPTY, each empty
match too (^ at every line start, for one). The pattern sees no text past
BOUND (the text end by default), so every match ends by then. Stops as soon as
FUNCTION returns true and returns that value; else returns NIL. HALT, when
given, is asked before each window and gives up the scan (returning NIL) when
it answers true.
For a scanner from COMPILE-REGEX (see REGEX-TRAITS), stretches without the
text every match holds, or the shape (REQUIRED-SHAPE), are skipped at literal
speed. A pattern whose matches lie within a line gets windows that end after a
line and are looked for from the start of the line its fixed text is on. Its
matches are so found whole and from where they begin on lines up to
*REGEX-LONGEST-MATCH* octets. When the fixed text does not begin them, only
the lines holding it are tried. \\z and \\Z are looked for at the end only.
Otherwise a match needing text more than the pattern's reach (REGEX-TRAITS:
+REGEX-OVERLAP+ octets, a thorough scanner's *REGEX-LONGEST-MATCH*) past the
stretch it starts in may be cut short or missed. So may one whose fixed text
lies further than that from its start.
DECODER, when given, holds the text decoded; else a spare one does
\(WITH-DECODER), so that a search piece after piece on several cores makes no
new buffers."
  (declare (type fixnum start end) (type function function))
  (unless decoder
    (return-from map-regex-matches
      (with-decoder (spare)
        (map-regex-matches function rope scanner start end
                           :bound bound :decoder spare :halt halt :texts texts :empty empty))))
  (let* ((total (tuatara.text:rope-octet-length rope))
         (bound (if bound (tuatara.text:rope-char-start rope (min bound total)) total))
         (end (min end bound))
         (pos (tuatara.text:rope-char-start rope (min start total)))
         (last-end -1)
         (traits (gethash scanner **regex-traits**))
         (scanner (scanner-for-rope scanner rope traits))
         (needle (and traits (regex-traits-needle traits)))
         (shape (and traits (regex-traits-shape traits)))
         (needle-length (cond (needle (length needle)) (shape (length shape)) (t 0)))
         (prefix-p (and traits (regex-traits-prefix traits)))
         (line-bound (and traits (regex-traits-line-bound traits)))
         ;; each match lies within a line holding NEEDLE: only those lines are tried
         (lines (and needle line-bound (not prefix-p) (plusp (length needle))))
         (overlap (if traits (regex-traits-reach traits) +regex-overlap+))
         (context (if traits (regex-traits-context traits) +regex-context+))
         ;; the window a filter may read the rope around (*WINDOW-ROPE*): one list, set for each window
         (window (list rope 0 0 bound))
         (chunk (and (or needle shape)
                     (let ((size (+ +search-chunk+ needle-length)))
                       (when (< (length (decoder-skip decoder)) size)
                         (setf (decoder-skip decoder) (tuatara.text:make-octets size)))
                       (decoder-skip decoder))))
         ;; where an occurrence of NEEDLE or SHAPE may start and still lie within a match
         ;; starting before END: past END by the pattern's reach, or for matches within a
         ;; line, to the end of the line holding END - 1
         (scan-end (cond ((or (not (or needle shape)) (>= pos end)) end)
                         (prefix-p end)
                         (line-bound
                          (let ((cap (min bound (+ end *regex-longest-match*))))
                            (or (tuatara.text:rope-find-line-end rope :start (max pos (1- end)) :end cap) cap)))
                         (t (min bound (+ end overlap))))))
    (declare (type fixnum total bound end pos last-end overlap context scan-end))
    (when (and traits (regex-traits-end-anchored traits))
      ;; \z and \Z match only at the end: look there alone, in one window reaching BOUND
      (setf pos (max pos (tuatara.text:rope-char-start rope (max 0 (- bound *regex-longest-match*))))))
    (loop while (< pos end)
          do (when (and halt (funcall halt)) (return))
             (let ((range (if (and traits (regex-traits-end-anchored traits))
                              (max 1 (- end pos))
                              (max +search-chunk+ overlap))))
               (when (or needle shape)
                 ;; every match holds NEEDLE, or SHAPE: find its next occurrence at literal speed and
                 ;; start there when matches begin with it; else at the start of its line
                 ;; when matches lie within a line, or where a match of the pattern's reach
                 ;; could contain it. After a long jump the occurrences are sparse: only
                 ;; the starts up to this one are worth the regex, the next jump finds the
                 ;; next occurrence.
                 (let ((q (if needle
                              (scan-range-forward rope needle (regex-traits-fold traits) pos scan-end chunk)
                              (scan-shape-forward rope shape pos scan-end chunk))))
                   (unless q (return))
                   (let ((to (cond (prefix-p q)
                                   (line-bound (line-start-within rope q (max pos (- q *regex-longest-match*))))
                                   (t (tuatara.text:rope-char-start rope (max 0 (- (+ q needle-length) overlap)))))))
                     (if shape
                         ;; a shape is found as fast as the window is made: try the pattern
                         ;; only from where it can start up to this place of the shape
                         (setf pos (max pos to) range (max 1 (- (1+ q) pos)))
                         (progn (when (> to (+ pos overlap)) (setf range (max overlap (- (1+ q) to))))
                                (setf pos (max pos to)))))
                   (when (>= pos end) (return))))
             (let* ((context-start (tuatara.text:rope-char-start rope (max 0 (- pos context))))
                    (limit (let ((l (tuatara.text:rope-char-start rope (min end (+ pos range)))))
                             (if (> l pos) l (min bound (tuatara.text:rope-next-boundary rope pos)))))
                    (reach (cond ((and traits (regex-traits-end-anchored traits)) bound)
                                 ;; every match lies within a line: the window ends after the
                                 ;; line holding the last start it answers for, and is exact
                                 (line-bound
                                  (let* ((cap (min bound (+ limit *regex-longest-match*)))
                                         (newline (tuatara.text:rope-find-line-end rope :start (max pos (1- limit))
                                                                                        :end cap)))
                                    (if newline (1+ newline) cap)))
                                 (t (+ limit overlap))))
                    ;; a match reported from the range before may run into this one
                    (resume (max pos last-end)))
               (declare (type fixnum context-start limit reach resume))
               (loop ; once per window size: a match reaching the window's end grows the window
                 (let ((window-end (tuatara.text:rope-char-start rope (min bound reach))))
                   (multiple-value-bind (string count table) (decode-window decoder rope context-start window-end)
                     (let ((from (octet-index table (- resume context-start) count))
                           (window (progn (setf (second window) context-start (third window) window-end)
                                          window)))
                       (declare (type fixnum from))
                       (when (loop
                               (when (> from count) (return t))
                               (let ((try-end count))
                                 (declare (type fixnum try-end))
                                 (when lines
                                   ;; the pattern is tried on the next line holding its text alone
                                   (multiple-value-bind (line-start line-end)
                                       (needle-line (decoder-octets decoder) (- window-end context-start) needle
                                                    (regex-traits-fold traits) (index-octet table from))
                                     (unless line-start (return t))
                                     (setf from (max from (octet-index table line-start count))
                                           try-end (octet-index table line-end count))))
                                 (multiple-value-bind (ms me group-starts group-ends reached)
                                     (tuatara.runtime:stoppable
                                       (let ((*window-open* (and (= try-end count) (< window-end bound)))
                                             (*window-rope* window)
                                             (*window-reached* nil))
                                         (multiple-value-bind (ms me group-starts group-ends)
                                             (cl-ppcre:scan scanner string :start from :end try-end :real-start-pos 0)
                                           (values ms me group-starts group-ends *window-reached*))))
                                   (if (null ms)
                                       (if (= try-end count)
                                           (return t)
                                           (setf from try-end))
                                       (let ((bs (+ context-start (index-octet table ms))))
                                         (when (>= bs limit) (return t))
                                         (when (and reached (= me count) (> me ms))
                                           ;; a filter read the match on past the window: it ends there
                                           (let* ((whole (and texts
                                                              (tuatara.text:octets-string
                                                               (tuatara.text:rope-octets rope :start bs :end reached)
                                                               :base t)))
                                                  (result (funcall function bs reached
                                                                   (and whole
                                                                        (matched-text texts whole 0 (length whole)))
                                                                   (matched-groups texts string
                                                                                   group-starts group-ends))))
                                             (when result (return-from map-regex-matches result))
                                             (setf last-end reached)
                                             (return t)))
                                         (when (and (= me count) (< window-end bound)
                                                    (< (- window-end context-start) *regex-longest-match*))
                                           ;; the match may go on past the window: look again in a larger one
                                           (setf resume (+ context-start (index-octet table from))
                                                 reach (+ window-end (- window-end context-start)))
                                           (return nil))
                                         (if (= ms me)
                                             (progn ; an empty match: reported when asked for, then look further on
                                               (when empty
                                                 (let ((result (funcall function bs bs
                                                                        (matched-text texts "" 0 0) nil)))
                                                   (when result (return-from map-regex-matches result))))
                                               (setf from (1+ ms)))
                                             (let* ((be (+ context-start (index-octet table me)))
                                                    (result (funcall function bs be
                                                                     (matched-text texts string ms me)
                                                                     (matched-groups texts string
                                                                                     group-starts group-ends))))
                                               (when result (return-from map-regex-matches result))
                                               (setf last-end be from me))))))))
                         (return))))))
               (setf pos (max limit last-end)))))
    nil))

(defun regex-scanner (regex case-fold)
  "REGEX itself when it is already a compiled scanner, else its scanner."
  (if (stringp regex) (compile-regex regex :case-fold case-fold) regex))

(defun first-regex-match (rope scanner start end bound decoder &key halt empty)
  "The first match starting in [START, END) and ending by BOUND as a list
(START END STRING GROUPS), or NIL; HALT and EMPTY as for MAP-REGEX-MATCHES."
  (map-regex-matches (lambda (s e m g) (list s e m g)) rope scanner start end
                     :bound bound :decoder decoder :halt halt :empty empty))

(defun regex-resync-start (rope scanner position floor)
  "Where a scan should begin so that the matches of SCANNER it finds from
POSITION of ROPE on are those one scan from further back finds, not a match
begun in the middle of a longer one. It is the start of POSITION's line for a
pattern whose matches lie within a line, else the pattern's reach before
POSITION (REGEX-TRAITS). It is never before FLOOR."
  (let ((traits (gethash scanner **regex-traits**)))
    (if (and traits (regex-traits-line-bound traits))
        (line-start-within rope position (max floor (- position *regex-longest-match*)))
        (tuatara.text:rope-char-start
         rope (max floor (- position (if traits (regex-traits-reach traits) +regex-overlap+)))))))

(defun last-regex-match (rope scanner start end bound decoder &key halt (floor start) empty)
  "The last match starting in [START, END) and ending by BOUND as a list
(START END STRING GROUPS), or NIL. Scans +SEARCH-CHUNK+ pieces from END
backward, stopping at the first piece with a match; a piece with one is
scanned again from further back (REGEX-RESYNC-START, never before FLOOR), so
that of a run the pattern matches whole the start of the run is found, as a
forward scan finds it. The pieces are scanned without the texts of their
matches; the text and groups of the one kept are taken after. HALT and EMPTY
as for MAP-REGEX-MATCHES."
  (let ((last -1) (last-end -1))
    (declare (type fixnum last last-end))
    (flet ((keep (s e m g)
             (declare (ignore m g))
             (setf last s last-end e)
             nil))
      (loop for b of-type fixnum = end then a
            for a of-type fixnum = (max start (- b +search-chunk+))
            while (< a b)
            do (map-regex-matches #'keep rope scanner a b
                                  :bound bound :decoder decoder :halt halt :empty empty :texts nil)
               (when (and halt (funcall halt)) (return-from last-regex-match nil))
               (when (>= last a)
                 (let ((from (regex-resync-start rope scanner a floor)))
                   (when (< from a)
                     (setf last -1)
                     (map-regex-matches (lambda (s e m g) (when (>= s a) (keep s e m g)))
                                        rope scanner from b :bound bound :decoder decoder :halt halt :empty empty
                                                            :texts nil)))
                 (when (>= last a)
                   (return-from last-regex-match
                     (or (first-regex-match rope scanner last (1+ last) bound decoder :empty empty)
                         (list last last-end nil nil)))))
            until (= a start)))))

(defun regex-serial-window ()
  "Octets a regex search scans on the calling thread before going parallel: at
most a mebibyte, since a pattern without fixed text scans at tens of megabytes
a second."
  (min *serial-window* (* 1024 1024)))

(defun regex-search-forward (rope regex &key (start 0) (end (tuatara.text:rope-octet-length rope)) bound
                                              (case-fold (and (stringp regex) (regex-case-fold-p regex)))
                                              (threads (tuatara.runtime:cpu-count)) stop empty)
  "The first match of REGEX (a pattern string or a compiled scanner) in ROPE
that starts at or after START and before END and ends by BOUND (the text end by
default; the pattern sees nothing past it): values match start and end (byte
offsets), the matched string and its groups (as MAP-REGEX-MATCHES); NIL when
none. With EMPTY, an empty match counts too (^ at a line start, for one). A
pattern without upper-case letters ignores case. The first REGEX-SERIAL-WINDOW
octets are scanned on the calling thread, the rest in pieces run in order on
THREADS threads (DIRECTIONAL-SEARCH). STOP, when given, is asked on the calling
thread between windows of the scan, every few milliseconds; when it answers true
the search gives up and returns NIL and :STOPPED."
  (let ((scanner (regex-scanner regex case-fold)))
    (multiple-value-bind (hit stopped)
        (directional-search start (min end (or bound end)) (regex-serial-window)
                            (lambda (from to halt)
                              (with-decoder (decoder)
                                (first-regex-match rope scanner from to bound decoder :halt halt :empty empty)))
                            :stop stop :threads threads)
      (if hit (values-list hit) (values nil stopped)))))

(defun regex-search-backward (rope regex &key (start (tuatara.text:rope-octet-length rope)) (end 0) bound
                                               (case-fold (and (stringp regex) (regex-case-fold-p regex)))
                                               (threads (tuatara.runtime:cpu-count)) stop empty)
  "The last match of REGEX in ROPE that starts before START and at or after END
and ends by BOUND: values as REGEX-SEARCH-FORWARD, EMPTY too; NIL when none.
Matches are found as a forward scan finds them, so of a run that a pattern
matches whole the start of the run is found. Scans back from START a
REGEX-SERIAL-WINDOW on the calling thread, then pieces going back, in parallel;
STOP as for SEARCH-FORWARD."
  (let ((scanner (regex-scanner regex case-fold)))
    (multiple-value-bind (hit stopped)
        (directional-search start end (regex-serial-window)
                            (lambda (from to halt)
                              (with-decoder (decoder)
                                (last-regex-match rope scanner from to bound decoder
                                                  :halt halt :floor end :empty empty)))
                            :backward t :stop stop :threads threads)
      (if hit (values-list hit) (values nil stopped)))))

(defun regex-search-all (rope regex &key (start 0) (end (tuatara.text:rope-octet-length rope)) bound
                                          (case-fold (and (stringp regex) (regex-case-fold-p regex)))
                                          (limit most-positive-fixnum) halt)
  "All non-empty, non-overlapping matches of REGEX in ROPE that start at or
after START and before END and end by BOUND, as a list of (START END STRING
GROUPS) in order (GROUPS as for MAP-REGEX-MATCHES), at most LIMIT of them. It
runs on the calling thread, for a range of the size of a screen; for more,
COLLECT-MATCHES runs in parallel and can be stopped. HALT, when given, is
asked before each window; once it answers true the matches found so far are
returned, and :STOPPED as a second value."
  (let ((scanner (regex-scanner regex case-fold)) (matches '()) (count 0) (halted nil))
    (map-regex-matches (lambda (s e m g)
                         (push (list s e m g) matches)
                         (>= (incf count) limit))
                       rope scanner start end :bound bound :halt (and halt (lambda () (setf halted (funcall halt)))))
    (values (nreverse matches) (and halted :stopped))))

(defun map-literal-matches (function rope string start end &key bound (case-fold (case-fold-p string)) texts halt)
  "Call FUNCTION with (MATCH-START MATCH-END TEXT NIL) for each non-overlapping
occurrence of STRING in ROPE that starts at or after START and before END and
ends by BOUND (the text end by default), in order. TEXT is the text matched
when TEXTS is true, else NIL. Stops and returns FUNCTION's value as soon as it
returns true, or returns NIL when HALT, asked before each chunk, answers true;
else returns NIL. CASE-FOLD ignores case, as for SEARCH-FORWARD.
The text is read a chunk at a time into a spare decoder's buffer
\(WITH-DECODER), so that a search piece after piece on several cores makes no
new ones. A match's text is taken from that chunk, and given again as it was
for a match of the same octets as the one before, so that billions of matches
of a few texts make nothing each."
  (declare (type fixnum start end))
  (when (by-pattern-p string case-fold)
    (return-from map-literal-matches
      (map-regex-matches function rope (literal-scanner string case-fold) (character-start-from rope start) end
                         :bound bound :halt halt :texts texts)))
  (with-decoder (decoder)
    (let* ((needle (needle-octets string case-fold)) (n (length needle))
           (total (tuatara.text:rope-octet-length rope))
           (end (min end (1+ (- (min (or bound total) total) n)))) ; the last start that fits, exclusive
           (chunk (progn (when (< (length (decoder-skip decoder)) (+ +search-chunk+ n))
                           (setf (decoder-skip decoder) (tuatara.text:make-octets (+ +search-chunk+ n))))
                         (decoder-skip decoder)))
           (next start)
           ;; the octets last matched and what was given for them: a pattern matches few texts
           (last-octets (and texts (tuatara.text:make-octets n))) (last-text nil) (last-p nil))
      (declare (type fixnum n total end next))
      (flet ((text-at (hit)
               (declare (type fixnum hit))
               (if (and last-p (loop for k of-type fixnum below n
                                     always (= (aref chunk (+ hit k)) (aref last-octets k))))
                   last-text
                   (let ((text (tuatara.text:octets-string chunk :start hit :end (+ hit n))))
                     (replace last-octets chunk :start2 hit :end2 (+ hit n))
                     (setf last-p t last-text (matched-text texts text 0 (length text)))))))
      (when (plusp n)
        (loop for pos of-type fixnum from start below end by +search-chunk+
              do (when (and halt (funcall halt)) (return-from map-literal-matches nil))
                 (let* ((chunk-end (min total (+ pos +search-chunk+ (1- n))))
                        (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                        (below (min filled +search-chunk+ (- end pos)))
                        (i (max 0 (- next pos)))
                        (other-case (cons nil nil)))
                   (declare (type fixnum filled below i))
                   (loop
                     (let ((hit (and (< i below) (scan-chunk chunk needle case-fold i below filled other-case))))
                       (unless hit (return))
                       (let* ((s (+ pos hit)) (e (+ s n))
                              (result (funcall function s e (and texts (text-at hit)) nil)))
                         (when result (return-from map-literal-matches result))
                         (setf next e i (+ hit n)))))))))
      nil)))
