;;;; lisp/search/substitute.lisp --- Replacing from the search prompt: pattern/replacement/flags.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The search prompt reads what is typed as a substitute command: a string
;;;; without an unescaped / is a plain search; pattern/replacement/flags replaces
;;;; on RET instead, on the line the search began on, in the region when one was
;;;; active, or in the whole buffer after a leading %. A match that folded text
;;;; hides is replaced too, the fold kept; asked at, its fold is opened first.
;;;; Every replacement is one edit batch, so one undo takes it back. When the
;;;; pattern ignored case, each replacement takes the letter case of what it
;;;; replaces.

(in-package #:tuatara.defaults)

(defvar *last-search-pattern* nil
  "The pattern of the most recent search or replacement as (PATTERN . REGEX-P).
A replacement typed with an empty pattern, //new, uses it.")

(define-record (search-line (:constructor make-search-line (&key whole-buffer pattern replacement flags problem))
                           (:copier nil))
  "A search string read as [%]pattern[/replacement[/flags]]: WHOLE-BUFFER true
after a leading %, the PATTERN, the REPLACEMENT after an unescaped / (NIL when
there is none, for a plain search), the FLAGS after a second /, and PROBLEM, the
reason the string cannot be used yet, or NIL."
  (whole-buffer nil :documentation "True when the search line began with %: the whole buffer.")
  (pattern "" :type string :documentation "The pattern of a search line.")
  (replacement nil :documentation "The replacement of a search line, or NIL for a plain search.")
  (flags "" :type string :documentation "The flags of a search line, a string of g, c, n, i and I.")
  (problem nil :documentation "Why a search line cannot be used yet, or NIL."))

(setf (documentation 'make-search-line 'function) "Return a search line with the given parts.")

(defun pattern-run-end (pattern i)
  "Where the set ([...]) or the quoted text (\\Q...\\E) that starts at I of
PATTERN, a regular expression, ends; NIL when neither starts there. Nothing in
either is special: a slash or a back reference there is itself."
  (let ((n (length pattern)))
    (cond ((char= (char pattern i) #\[) (tuatara.syntax:bracket-end pattern i))
          ((and (char= (char pattern i) #\\) (< (1+ i) n) (char= (char pattern (1+ i)) #\Q))
           (let ((close (search "\\E" pattern :start2 (+ i 2))))
             (if close (+ close 2) n))))))

(defun split-search-line (string regex)
  "Split STRING at the slashes that separate a pattern, a replacement and flags,
into a list of one to three strings; the third takes the rest of STRING. A
slash does not count after a backslash, nor, when REGEX is true, inside a
set of the pattern such as [,/] or quoted text \\Q.../...\\E."
  (let ((parts '()) (start 0) (i 0) (n (length string)))
    (loop while (and (< i n) (< (length parts) 2))
          do (let ((c (char string i))
                   (run (and regex (null parts) (pattern-run-end string i))))
               (cond (run (setf i run))
                     ((char= c #\\) (incf i 2))
                     ((char= c #\/)
                      (push (subseq string start i) parts)
                      (setf start (1+ i) i (1+ i)))
                     (t (incf i)))))
    (push (subseq string (min start n)) parts)
    (nreverse parts)))

(defun unescape-literal (text)
  "TEXT typed as literal text: \\/ stands for a slash and \\\\ for one
backslash; any other backslash is itself."
  (with-output-to-string (out)
    (let ((i 0) (n (length text)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (if (and (char= c #\\) (< (1+ i) n) (find (char text (1+ i)) "/\\"))
                     (progn (write-char (char text (1+ i)) out) (incf i 2))
                     (progn (write-char c out) (incf i))))))))

(defun parse-search-line (string regex)
  "Read STRING, the text typed at the search prompt, as a search or a
replacement: [%]pattern[/replacement[/flags]]. REGEX tells whether the pattern
is a regular expression (brackets then hide slashes) or literal text (where \\/
and \\\\ stand for a slash and a backslash). A leading % means the whole
buffer; \\% at the start stands for a % itself. An empty pattern before a
replacement reuses the last search's. Flags: g c n i I. Returns a SEARCH-LINE."
  (let* ((whole (and (plusp (length string)) (char= (char string 0) #\%)))
         (parts (split-search-line (if whole (subseq string 1) string) regex))
         (pattern (first parts))
         (replacement (second parts))
         (flags (or (third parts) ""))
         (problem nil))
    (unless regex
      (when (and (not whole) (>= (length pattern) 2) (string= "\\%" pattern :end2 2))
        (setf pattern (subseq pattern 1)))
      (setf pattern (unescape-literal pattern)))
    (when (and replacement (zerop (length pattern)))
      (if *last-search-pattern*
          (setf pattern (car *last-search-pattern*))
          (setf problem "no previous pattern")))
    (let ((bad (find-if-not (lambda (c) (find c "gcniI")) flags)))
      (when bad (setf problem (format nil "unknown flag ~a" bad))))
    (make-search-line :whole-buffer whole :pattern pattern :replacement replacement :flags flags :problem problem)))

(defun search-line-fold (line regex)
  "True when LINE's pattern is matched ignoring case: flag i says so and I says
not, the later of them winning; otherwise when the pattern has no upper-case
letter (outside an escape such as \\W, for a regular expression; REGEX tells),
and a regular expression names no case ([[:upper:]], \\p{Lu})."
  (let ((flag (find-if (lambda (c) (find c "iI")) (search-line-flags line) :from-end t)))
    (cond ((eql flag #\i) t)
          ((eql flag #\I) nil)
          (regex (tuatara.syntax:regex-case-fold-p (search-line-pattern line)))
          (t (tuatara.syntax:case-fold-p (search-line-pattern line))))))

(defun search-line-matcher (line regex)
  "How to find LINE's pattern (MAKE-PATTERN-MATCHER): a MATCHER, or NIL and
the reason the pattern does not compile. REGEX tells whether it is a regular
expression; case is ignored as its flags say (SEARCH-LINE-FOLD)."
  (make-pattern-matcher (search-line-pattern line) :regex regex :fold (search-line-fold line regex)))

(defun expand-replacement (replacement matched groups &optional (newline (string #\Newline)))
  "The text REPLACEMENT produces for a match of the text MATCHED whose groups
matched GROUPS (a vector, or NIL). & and \\0 stand for the whole match and \\1
to \\9 for a group. \\n and \\r stand for a line break, NEWLINE (the buffer's,
BUFFER-NEWLINE), and \\t for a tab. \\u and \\l make the next character upper
or lower case, \\U and \\L all that follows until \\E or \\e. Any other escaped
character, such as \\& \\/ or \\\\, stands for itself."
  (let ((mode nil) (once nil))
    (with-output-to-string (out)
      (flet ((emit (text)
               (loop for c across text
                     do (write-char (cond (once (prog1 (if (eq once :upper) (char-upcase c) (char-downcase c))
                                                  (setf once nil)))
                                          ((eq mode :upper) (char-upcase c))
                                          ((eq mode :lower) (char-downcase c))
                                          (t c))
                                    out))))
        (let ((i 0) (n (length replacement)))
          (loop while (< i n)
                do (let ((c (char replacement i)))
                     (cond ((char= c #\&) (emit matched) (incf i))
                           ((and (char= c #\\) (< (1+ i) n))
                            (let ((d (char replacement (1+ i))))
                              (incf i 2)
                              (case d
                                (#\0 (emit matched))
                                ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                 (let ((k (1- (digit-char-p d))))
                                   (when (and groups (< k (length groups)) (aref groups k))
                                     (emit (aref groups k)))))
                                ((#\n #\r) (emit newline))
                                (#\t (emit (string #\Tab)))
                                (#\u (setf once :upper))
                                (#\l (setf once :lower))
                                (#\U (setf mode :upper))
                                (#\L (setf mode :lower))
                                ((#\E #\e) (setf mode nil))
                                (t (emit (string d))))))
                           (t (emit (string c)) (incf i))))))))))

(defun literal-replacement (replacement &optional (newline (string #\Newline)))
  "REPLACEMENT taken as literal text: \\n and \\t stand for a line break,
NEWLINE (the buffer's, BUFFER-NEWLINE), and a tab, \\/ and \\\\ for a slash
and a backslash; everything else is itself."
  (with-output-to-string (out)
    (let ((i 0) (n (length replacement)))
      (loop while (< i n)
            do (let ((c (char replacement i)))
                 (if (and (char= c #\\) (< (1+ i) n) (find (char replacement (1+ i)) "nt/\\"))
                     (let ((d (char replacement (1+ i))))
                       (case d (#\n (write-string newline out)) (#\t (write-char #\Tab out)) (t (write-char d out)))
                       (incf i 2))
                     (progn (write-char c out) (incf i))))))))

(defun next-line-start-after (rope end &optional (start 0))
  "The start of the line after the one a match ending at END ends on: the first
offset the next match may start at when only the first on each line counts.
A match that ends just after a newline ends on the newline's line; an empty
match, START being END, on its own line."
  (1+ (tuatara.text:rope-line-end rope (max start (1- end)))))

(defun first-per-line (rope matches)
  "Of MATCHES, sorted lists (START END ...) in ROPE, the first on each line,
which a replacement without the g flag replaces: after a kept match, the next
one kept starts on a later line than the one the match ends on."
  (let ((allowed 0) (kept '()))
    (dolist (m matches (nreverse kept))
      (when (>= (first m) allowed)
        (push m kept)
        (setf allowed (next-line-start-after rope (second m) (first m)))))))

(defun matched-lines (rope matches)
  "How many lines of ROPE MATCHES, sorted lists (START END ...), start on."
  (let ((line-end -1) (lines 0))
    (dolist (m matches lines)
      (when (> (first m) line-end)
        (incf lines)
        (setf line-end (tuatara.text:rope-line-end rope (first m)))))))

(defun match-case-kind (matched &optional (from 0) (to (length matched)))
  "The letter case of MATCHED, from FROM below TO, for a replacement to
follow: :UPPER when it has no small letter and a word of two letters or more
\(FOX); :WORDS when every word starts with a capital (Fox, FOx, A); NIL
otherwise, or with no letters. A word starts where a letter or digit
follows another character or the start of MATCHED; one starting with a digit
starts with no capital. Makes nothing, so that it costs nothing for each of a
great many matches, read where they are."
  (let ((lower nil) (upper nil) (multiletter nil) (small-initial nil) (after-word nil))
    (loop for i from from below to
          for c = (char matched i)
          do (cond ((lower-case-p c)
                    (setf lower t)
                    (if after-word (setf multiletter t) (setf small-initial t)))
                   ((upper-case-p c)
                    (setf upper t)
                    (when after-word (setf multiletter t)))
                   ((and (word-char-p c) (not after-word)) (setf small-initial t)))
             (setf after-word (word-char-p c)))
    (cond ((and (not lower) multiletter) :upper)
          ((and (not small-initial) upper) :words))))

(defun case-like (text kind)
  "TEXT in the letter case KIND (MATCH-CASE-KIND) says: all capitals for
:UPPER, a capital at the start of each word for :WORDS, else as it is."
  (case kind
    (:upper (string-upcase text))
    (:words (let ((start t))
              (map 'string (lambda (c)
                             (prog1 (if (and start (word-char-p c)) (char-upcase c) c)
                               (setf start (not (word-char-p c)))))
                   text)))
    (t text)))

(defvar *replacement-budget* nil
  "Octets of heap a replacement may plan to use, or NIL for a quarter of what is
free when it starts. Matches beyond it are not collected, so a replacement too
large to hold in memory is refused before the heap runs out.")

(defun replacement-budget ()
  "The octets a replacement may plan to use now (*REPLACEMENT-BUDGET*)."
  (heap-budget *replacement-budget*))

(defun line-matches (rope matcher regex low high &key stop count-only)
  "Every match of MATCHER in ROPE starting at or after LOW and ending by HIGH,
found in parallel within the replacement budget (TUATARA.SYNTAX:COLLECT-MATCHES).
Three values: the matches as sorted lists (START END TEXT GROUPS), NIL or
:STOPPED or :OVER, and how many were found. TEXT is NIL for a literal search
(REGEX false) that matched case exactly, whose replacement needs nothing from
the match; and with COUNT-ONLY, which also keeps the estimate to the matches
alone."
  (let ((texts (and (not count-only) (or regex (matcher-fold matcher)))))
    (tuatara.syntax:collect-matches
     (lambda (function from to halt)
       ;; empty matches too (^, $), and the one at the very end of the scope
       (stretch-matches function rope matcher from to (eql to high) texts halt high))
     low high :stop stop :cost-limit (replacement-budget)
              :match-cost (if count-only 96 400) :block-size (if count-only 1 16384))))

(defun count-line-matches (rope matcher low high &key stop (bound high) hidden)
  "How many matches LINE-MATCHES finds in ROPE from LOW to HIGH, counted in
parallel without keeping any, so that a count of millions takes no memory:
two values, the count and NIL, or what was counted and :STOPPED once STOP
answered true. With BOUND past HIGH, the matches that start before HIGH in
a scan of the text up to BOUND: those of a count to BOUND that come before
HIGH. With HIDDEN, the stretches search filters hide as
TUATARA.DISPLAY:HIDDEN-BOUNDS gives them, the matches starting in them are not
counted, as a search passes over them."
  (multiple-value-bind (found status count)
      (tuatara.syntax:collect-matches
       (lambda (function from to halt)
         (stretch-matches (if hidden
                              (lambda (s e m g)
                                (unless (tuatara.display:hidden-stretch-at hidden s) (funcall function s e m g)))
                              function)
                          rope matcher from to (and (eql to high) (eql high bound)) nil halt bound))
       low high :stop stop :keep nil)
    (declare (ignore found))
    (values count status)))

(defun count-first-per-line (rope matcher low high &key stop)
  "How many matches a replacement without the g flag replaces in ROPE from LOW
to HIGH, the first on each line (FIRST-PER-LINE), counted in parallel pieces
cut at line starts without keeping any: two values, the count and NIL, or
NIL and :STOPPED once STOP answered true. A match running into the next
piece has that piece counted again from where it ends, as one scan of the
whole text goes on (TUATARA.SYNTAX:JOIN-PIECES)."
  (labels ((count-piece (from to allowed)
             ;; the matches from FROM below TO as one scan finds them, those starting at or
             ;; after ALLOWED kept: how many, where the next kept may start, where the last ends
             (let ((count 0) (last-end from))
               (declare (type fixnum count allowed last-end))
               (stretch-matches (lambda (s e m g)
                                  (declare (ignore m g) (type fixnum s e))
                                  (when (>= s allowed)
                                    (incf count)
                                    (setf allowed (next-line-start-after rope e s)))
                                  (setf last-end e)
                                  nil)
                                rope matcher from to (eql to high) nil stop high)
               (values count allowed last-end))))
    (let* ((pieces (coerce (tuatara.text:rope-line-pieces rope low high tuatara.syntax:*parallel-piece*)
                           'simple-vector))
           (results (tuatara.runtime:run-tasks
                     (map 'simple-vector (lambda (piece)
                                           (lambda ()
                                             (multiple-value-list (count-piece (first piece) (second piece) 0))))
                          pieces)))
           (total 0) (allowed 0))
      (declare (type fixnum total allowed))
      (when (and stop (funcall stop))
        (return-from count-first-per-line (values nil :stopped)))
      (tuatara.syntax:join-pieces
       (length pieces) (lambda (k) (first (aref pieces k)))
       (lambda (k from)
         (let ((piece (aref pieces k)))
           (if from
               ;; the match before runs into this piece: on from where it ends
               (multiple-value-bind (again after ends) (count-piece (min from (second piece)) (second piece) allowed)
                 (incf total again)
                 (setf allowed after)
                 ends)
               (destructuring-bind (count next end) (aref results k)
                 (incf total count)
                 (setf allowed (max allowed next))
                 end)))))
      (values total nil))))

(defun next-line-match (buffer matcher regex from high)
  "The first match of MATCHER in BUFFER starting at or after FROM and ending by
HIGH as (START END TEXT GROUPS), or NIL, TEXT given when REGEX or case is
ignored."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (when (<= from high)
      (multiple-value-bind (s e m g) (matcher-search rope matcher from high)
        (and s (list s e (or m (and (or regex (matcher-fold matcher)) (tuatara.text:rope-string rope :start s :end e)))
                     g))))))

(defun replacement-sets-case-p (replacement)
  "True when REPLACEMENT, read in a regular expression, says the case it
writes: \\u, \\U, \\l or \\L in it (EXPAND-REPLACEMENT)."
  (loop with i = 0
        while (< i (1- (length replacement)))
        do (if (char= (char replacement i) #\\)
               (if (find (char replacement (1+ i)) "uUlL")
                   (return t)
                   (incf i 2))
               (incf i))))

(defun replacement-text-function (replacement regex fold &optional (newline (string #\Newline)))
  "A function of a match's TEXT and GROUPS giving the text REPLACEMENT puts in
its place: & and \\1 read in a regular expression (REGEX,
EXPAND-REPLACEMENT), plain text otherwise (LITERAL-REPLACEMENT); the case of
what it replaces followed when FOLD, its pattern ignoring case (CASE-LIKE),
unless REPLACEMENT says the case itself (REPLACEMENT-SETS-CASE-P). A
replacement the match does not enter is made once for each case, and the
same text given again, so that a great many matches make nothing. What it
needs of a match as a second value: NIL, nothing (TEXT is not looked at);
:CASE, only its letter case, given as TEXT (MATCH-CASE-KIND); :TEXT, the
text itself. A line break written in REPLACEMENT is NEWLINE, the buffer's."
  (let* ((follow (and fold (not (and regex (replacement-sets-case-p replacement)))))
         (constant (or (not regex) (notany (lambda (c) (find c "&\\")) replacement)))
         (expand (if regex
                     (lambda (m g) (expand-replacement replacement m g newline))
                     (let ((text (literal-replacement replacement newline)))
                       (lambda (m g) (declare (ignore m g)) text)))))
    (cond ((and constant follow)
           (let* ((base (funcall expand "" nil))
                  (upper (case-like base :upper))
                  (words (case-like base :words)))
             (values (lambda (kind g)
                       (declare (ignore g))
                       (case kind (:upper upper) (:words words) (t base)))
                     :case)))
          (constant (let ((base (funcall expand "" nil)))
                      (values (lambda (m g) (declare (ignore m g)) base) nil)))
          (follow (values (lambda (m g) (case-like (funcall expand m g) (match-case-kind m))) :text))
          (t (values expand :text)))))

(defun replacement-function (replacement regex matcher &optional (newline (string #\Newline)))
  "A function of a match (START END TEXT GROUPS) giving the text REPLACEMENT
puts in its place, as REPLACEMENT-TEXT-FUNCTION makes it for MATCHER, case
followed when MATCHER ignores case, a line break in it NEWLINE."
  (multiple-value-bind (text-of needs) (replacement-text-function replacement regex (matcher-fold matcher) newline)
    (if (eq needs :case)
        (lambda (m) (funcall text-of (match-case-kind (third m)) (fourth m)))
        (lambda (m) (funcall text-of (third m) (fourth m))))))

(defun pieces-line-matches (rope matcher regex pieces all &key stop count-only)
  "LINE-MATCHES over each of PIECES, (START . END) in order, the matches of all
together; without ALL the first on each line of each piece only. Three values
as LINE-MATCHES gives them, gathering stopped at the first piece that could
not be finished."
  (let ((runs '()) (counted 0))
    (flet ((found () (loop for run in (reverse runs) append run)))
      (dolist (piece pieces (values (found) nil counted))
        (multiple-value-bind (these status count)
            (line-matches rope matcher regex (car piece) (cdr piece) :stop stop :count-only count-only)
          (incf counted count)
          (when status (return (values (found) status counted)))
          (push (if all these (first-per-line rope these)) runs))))))

(defun pieces-next-match (buffer matcher regex pieces all)
  "A NEXT-MATCH for QUERY-REPLACE-LOOP over PIECES of BUFFER, (START . END) in
order: from an offset, the next match within a piece, without ALL the first
on each of its lines. The pieces after a replacement move by the change it
made to the buffer's size, as only one is made between two calls; as a
second value, a REST for its \\`!' over what is left of them."
  (let ((pieces (map 'vector (lambda (piece) (cons (car piece) (cdr piece))) pieces))
        (i 0) (fresh t) (size (tuatara.buffer:buffer-size buffer)))
    (flet ((settle ()
             ;; the replacement just made lies in piece I: it and those after it move
             (let ((delta (- (tuatara.buffer:buffer-size buffer) size)))
               (setf size (tuatara.buffer:buffer-size buffer))
               (unless (zerop delta)
                 (when (< i (length pieces)) (incf (cdr (aref pieces i)) delta))
                 (loop for j from (1+ i) below (length pieces)
                       do (incf (car (aref pieces j)) delta) (incf (cdr (aref pieces j)) delta))))))
      (values
       (lambda (pos)
         (settle)
         (loop while (< i (length pieces))
               do (let* ((piece (aref pieces i))
                         (rope (tuatara.buffer:buffer-text buffer))
                         (from (cond ((or fresh (<= pos (car piece))) (max pos (car piece)))
                                     (all pos)
                                     (t (min (cdr piece) (next-line-start-after rope pos)))))
                         (m (next-line-match buffer matcher regex from (cdr piece))))
                    (if m
                        (progn (setf fresh nil) (return m))
                        (setf i (1+ i) fresh t)))))
       (lambda (pos)
         (settle)
         (let ((rest (loop for j from i below (length pieces)
                           collect (if (= j i)
                                       (cons (max pos (car (aref pieces j))) (cdr (aref pieces j)))
                                       (aref pieces j)))))
           (pieces-line-matches (tuatara.buffer:buffer-text buffer) matcher regex rest all)))))))

(defun in-sight-lines (buffer low high)
  "The whole lines of BUFFER within LOW and HIGH that are in sight or near it
\(SIGHT-RANGES), as (START . END) in order, END the line's end."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (tuatara.text:merge-ranges
     (sort (loop for (from . to) in (sight-ranges buffer)
                 for s = (max low from) and e = (min high to)
                 when (< s e)
                   collect (cons (max low (tuatara.text:rope-line-start rope s))
                                 (min high (tuatara.text:rope-line-end rope e))))
           #'< :key #'car))))

(defun rewrite-after-sight (buffer line regex low high words flags matcher replacement-for all)
  "Replace LINE's matches between LOW and HIGH of BUFFER as START-REWRITE does,
those on the lines in sight at once (IN-SIGHT-LINES), so that what is seen
changes on RET, and the rest in the background, both one change for undo;
the report counts them together. With another job running or waiting in
BUFFER, or nothing out of sight, all of it is done in the background."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (lines (and (not (job-busy-p buffer)) (not (queued-in buffer)) (in-sight-lines buffer low high)))
         (rest-out-of-sight (and lines (or (> (car (first lines)) low)
                                           (< (cdr (car (last lines))) high)
                                           (rest lines)))))
    (multiple-value-bind (found status) (and rest-out-of-sight (pieces-line-matches rope matcher regex lines all))
      (if (or (null found) status)
          (start-rewrite buffer line regex low high words flags)
          (let* ((shifts (loop for m in found
                               collect (cons (second m)
                                             (- (tuatara.text:string-octet-length (funcall replacement-for m))
                                                (- (second m) (first m))))))
                 (moved (lambda (at) (+ at (loop for (end . shift) in shifts while (<= end at) sum shift))))
                 (shown (replace-matches buffer found replacement-for))
                 (shown-lines (matched-lines rope found))
                 (joins (tuatara.buffer:buffer-undo-stack buffer)))
            (start-rewrite buffer line regex low (funcall moved high) words flags
                           :skip (map 'vector (lambda (l) (cons (funcall moved (car l)) (funcall moved (cdr l)))) lines)
                           :joins joins
                           :report (lambda (job how count seconds kept)
                                     (let ((lines (+ shown-lines (nth-value 1 (rewrite-totals job)))))
                                       (case how
                                         (:none (format nil "~a ~a" (replaced-words shown shown-lines) words))
                                         (:done (format nil "~a ~a ~a" (replaced-words (+ shown count) lines)
                                                        words (job-done-words seconds kept)))
                                         (:stopped (format nil "Stopped: ~:d replaced in sight, ~
the rest left as it was" shown)))))))))))

(defun run-substitution (buffer line regex low high words origin &key (flags (search-line-flags line)) stop pieces)
  "Carry out LINE, a search line with a replacement, in BUFFER on the matches
that start at or after LOW and end by HIGH, the scope WORDS names (\"on this
line\"). REGEX tells whether the pattern is a regular expression, and so
whether the replacement reads & and \\1 (EXPAND-REPLACEMENT) or is literal text
(LITERAL-REPLACEMENT). FLAGS, those typed and those of the command that opened
the prompt: without g only the first match on each line is replaced; c asks at
each match; n only counts. When the pattern ignored case, each replacement
takes the case of what it replaces (MATCH-CASE-KIND, CASE-LIKE). The
replacements are one edit batch and point stays at ORIGIN. A scope of more
than *REWRITE-THRESHOLD* octets, matches too many to hold in memory, or a
pattern so slow on this text that one line holds it up
\(TUATARA.RUNTIME:*SLOW-CALL-SECONDS*), go to a replacement in the background
instead (START-REWRITE), and the editor stays usable. PIECES, when given, are
the stretches (START . END) in order within LOW and HIGH the replacement is
limited to, the cursors' lines say; their matches are gathered here, never in
the background. A match a fold hides is replaced too, whatever the scope, the
fold kept folded; asked at with c, its fold is opened first
\(QUERY-REPLACE-LOOP). When STOP answers true while matches are gathered,
nothing changes and a message says so. Returns the number of matches replaced
or counted here."
  (let ((matcher (search-line-matcher line regex)))
    (let* ((all (find #\g flags))
           (replacement (search-line-replacement line))
           (replacement-for (replacement-function replacement regex matcher (tuatara.buffer:buffer-newline buffer))))
      (setf *last-search-pattern* (cons (search-line-pattern line) regex)
            (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (cond
        ((and pieces (find #\c flags) (not (find #\n flags)))
         (goto (car (first pieces)))
         (multiple-value-bind (next rest) (pieces-next-match buffer matcher regex pieces all)
           (query-replace-loop buffer next replacement-for (search-line-pattern line) replacement :rest rest)))
        ((and (find #\c flags) (not (find #\n flags)))
          ;; ask at each match; the scope's end moves as replacements change its length
          (let ((tail (- (tuatara.buffer:buffer-size buffer) high)) (first t))
            (flet ((from-for (pos rope high)
                     (if (or all first)
                         pos
                         (min high (next-line-start-after rope pos)))))
              (goto low)
              (query-replace-loop buffer
                                  (lambda (pos)
                                    (let* ((rope (tuatara.buffer:buffer-text buffer))
                                           (high (- (tuatara.buffer:buffer-size buffer) tail))
                                           (from (from-for pos rope high)))
                                      (setf first nil)
                                      (next-line-match buffer matcher regex from high)))
                                  replacement-for (search-line-pattern line) replacement
                                  :rest (lambda (pos)
                                          ;; ! : the rest at once from the match asked at, gathered within the budget
                                          (let* ((rope (tuatara.buffer:buffer-text buffer))
                                                 (high (- (tuatara.buffer:buffer-size buffer) tail)))
                                            (multiple-value-bind (found status count)
                                                (line-matches rope matcher regex pos high :stop stop)
                                              (if (eq status :over)
                                                  ;; too many to hold: those in sight at once, the rest in the
                                                  ;; background
                                                  (progn (rewrite-after-sight buffer line regex pos high words flags
                                                                              matcher replacement-for all)
                                                         (values nil :background count))
                                                  (values (if all found (first-per-line rope found))
                                                          status count)))))))))
        (t
         (let ((rope (tuatara.buffer:buffer-text buffer)) (counting (find #\n flags)))
           (when (and (null pieces) (large-for-here-p low high))
             ;; a large scope: what is in sight at once, the rest in the background, so nothing waits on it
             (goto origin)
             (rewrite-after-sight buffer line regex low high words flags matcher replacement-for all)
             (return-from run-substitution 0))
           (multiple-value-bind (found status count)
               (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                 (if pieces
                     (pieces-line-matches rope matcher regex pieces all :stop stop :count-only counting)
                     (line-matches rope matcher regex low high :stop stop :count-only counting)))
             (goto origin)
             (when (and (null pieces) (member status '(:over :slow)))
               ;; too many to hold in memory: what is in sight at once, the rest in the background;
               ;; a pattern slow on this text has all of it in the background
               (if (eq status :over)
                   (rewrite-after-sight buffer line regex low high words flags matcher replacement-for all)
                   (start-rewrite buffer line regex low high words flags :slow t))
               (return-from run-substitution 0))
             (let ((matches (if (or all pieces) found (first-per-line rope found))))
               (cond ((eq status :slow) (message "The pattern is too slow here to replace with: nothing changed") 0)
                     (status (report-unreplaced status count words counting) 0)
                     ((null matches) (message "No match ~a" words) 0)
                     (counting
                      (message "~a" (matches-on-lines-words (length matches) (matched-lines rope matches)))
                      (length matches))
                     (t (let ((lines (matched-lines rope matches))
                              (replaced (replace-matches buffer matches replacement-for)))
                          (message "~a" (replaced-words replaced lines))
                          replaced)))))))))))

(defun report-unreplaced (status count words counting)
  "Say why a replacement or count gathered COUNT matches WORDS (\"in the
buffer\") and stopped without changing anything: STATUS :STOPPED for a key
typed meanwhile, :OVER for more matches than memory can hold."
  (ecase status
    (:stopped (message "Stopped after ~:d match~:*~[es~;~:;es~]; nothing ~:[replaced~;counted~]" count counting))
    (:over (message "Over ~:d matches ~a: too many to ~:[replace~;count~] in memory; nothing changed"
                    count words counting))))
