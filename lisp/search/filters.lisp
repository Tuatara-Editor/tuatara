;;;; lisp/search/filters.lisp --- A list narrowed to what searches match, the searches kept as filters.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Any list a command makes with RENDER-PIECES (or SHOW-LIST-TEXT, as
;;;; rimu's lists of directories and the agenda are made) and its entries can be
;;;; narrowed: C-x / s reads a pattern at the search prompt, and as it is
;;;; typed the list shows only the entries whose lines match, or the more
;;;; text a list gives of each entry (SET-SEARCH-FILTERING: a mailbox gives
;;;; each message's headers and contents). RET keeps the pattern as a
;;;; filter, and the next search narrows what that one left; the filters are
;;;; listed with C-x / l, and taken away one at a time or all at once. The
;;;; whole list is kept as it was made, and narrowed again when it is made
;;;; again. This file holds the filters, what a list keeps of its filtering,
;;;; and which entries a filter leaves; the work in the background is in
;;;; filter-jobs.lisp, files of records in record-filters.lisp, the narrowed
;;;; list's display in list-filters.lisp and the commands in
;;;; filter-commands.lisp.

(in-package #:tuatara.defaults)

(define-record (search-filter (:constructor %make-search-filter (string kind fold matcher where test part hide))
                             (:copier nil))
  "A pattern narrowing a list. STRING is the pattern as typed. KIND is :REGEX
for a regular expression or :LITERAL for literal text. FOLD is true when it
ignores case. MATCHER says how it is found in a text (MAKE-PATTERN-MATCHER).
COMPILED is it compiled to try on a string, made when first asked for
\(SEARCH-FILTER-SCANNER). WHERE is the part of each entry it looks at as the list
names parts (NIL for the line and all the list gives of the entry). In a file of
records, TEST, when given, tries COMPILED on a record's line in its own way
\(FILTER-LINE-MATCHER: a column). PART is the words naming what it tries
\(\"$2\"). HIDE is true when it leaves the entries it does not match. KEPT is
which of the list's entries it and the filters before it leave, a bit for each,
worked out for the whole list SOURCE. COUNT is how many entries that is,
headings aside. CUT is how many records TEST was tried on the start of alone,
being longer than *REWRITE-LINE-LIMIT* octets."
  (string nil :documentation "The pattern of a search filter, as typed.")
  (kind nil :documentation "How a search filter's pattern is read: :REGEX or :LITERAL.")
  (fold nil :documentation "True when a search filter ignores case.")
  (matcher nil :documentation "How a search filter's pattern is found in a text, a MATCHER.")
  (compiled nil :documentation "A search filter's pattern compiled for strings, once asked for, or NIL.")
  (where nil :documentation "The part of an entry a search filter looks at, or NIL for all of it.")
  (test nil
        :documentation "How a search filter tries its pattern on a record's
line (a column), or NIL for the whole line.")
  (part nil :documentation "Words naming what of a record a search filter tries (\"$2\"), or NIL for the whole line.")
  (hide nil :documentation "True when a search filter leaves the entries it does not match.")
  (kept nil :documentation "Which entries a search filter and those before it leave, a bit each, or NIL.")
  (source nil :documentation "The whole list a search filter's KEPT was worked out for.")
  (count 0 :documentation "How many entries a search filter and those before it leave.")
  (cut 0 :documentation "How many records a search filter tried on their start alone, being too long."))

(defvar *lines-cut* nil
  "When a cons, LINES-DECODED adds to its car each line, or record, it tries
on its first *REWRITE-LINE-LIMIT* octets alone because it is longer; the
work on every core of LINES-MATCHING-EVERYWHERE counts into the same cons.")

(defun search-filter-scanner (filter)
  "FILTER's pattern compiled to try on a string: its matcher's regular
expression, or for literal text one made the first time it is asked for."
  (or (search-filter-compiled filter)
      (setf (search-filter-compiled filter)
            (let ((matcher (search-filter-matcher filter)))
              (if (eq (matcher-kind matcher) :regex)
                  (matcher-scanner matcher)
                  (tuatara.syntax:compile-pattern (matcher-pattern matcher)
                                                  :regex nil :case-fold (matcher-fold matcher)))))))

(defun make-search-filter (string &key (regex t) where test part hide matcher (fold :as-typed))
  "A SEARCH-FILTER for STRING, a regular expression when REGEX is true, else
literal text, nothing in it special; case ignored when it has no capital
letter, as a search ignores it, or as FOLD says when it is T or NIL. MATCHER, when given, is how it is found
\(SEARCH-MATCHER, made for it already); else one is made. WHERE names the
part of each entry it looks at; TEST and PART how and what of a record's line
it tries; HIDE true to leave what it does not match. Returns NIL and why when
the pattern does not compile."
  (let* ((line (make-search-line :pattern string))
         (fold (if (eq fold :as-typed) (search-line-fold line regex) fold)))
    (multiple-value-bind (matcher why)
        (or matcher (make-pattern-matcher (search-line-pattern line) :regex regex :fold fold))
      (if matcher
          (%make-search-filter string (if regex :regex :literal) fold matcher where test part hide)
          (values nil why)))))

(defun search-filter-match-p (filter text)
  "True when the pattern of FILTER, a SEARCH-FILTER, matches somewhere in TEXT, a string."
  (and (tuatara.runtime:stoppable (cl-ppcre:scan (search-filter-scanner filter) text)) t))

(defun search-filter-leaves-line-p (filter text length)
  "True when FILTER leaves the record whose line TEXT holds in its first
LENGTH characters: its pattern found in the line, or in the part of it the
filter tries (a column); not found, for a filter that hides what it
matches."
  (let ((test (search-filter-test filter)) (scanner (search-filter-scanner filter)))
    (if (if test
            (funcall test scanner text length)
            (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start 0 :end length)))
        (not (search-filter-hide filter))
        (search-filter-hide filter))))

(defun search-filter-words (filter)
  "FILTER's pattern as the lists show it: a regular expression as it is,
literal text in double quotes, after the part of a record it tries ($2:)
and not, for one that hides what it matches."
  (format nil "~:[~;not ~]~@[~a: ~]~:[\"~a\"~;~a~]" (search-filter-hide filter) (search-filter-part filter)
          (search-filter-regex-p filter) (search-filter-string filter)))

(defun search-filter-regex-p (filter)
  "True when FILTER's pattern is a regular expression, NIL for literal text."
  (eq (search-filter-kind filter) :regex))

;;; What a list keeps of its filtering

(define-record (list-source (:constructor make-list-source (text spans buttons entries first-line)) (:copier nil))
  "A list as it was made, before filters narrowed it: its TEXT, SPANS and
BUTTONS as PIECES-TEXT gives them, its ENTRIES (a vector), shown from line
FIRST-LINE on, and LINES, the text of each entry's line, and TOTAL, how many
entries it shows, headings aside, each worked out once."
  (text nil :documentation "The whole text of a list, as made.")
  (spans nil :documentation "The parts of a list's whole text shown in a face.")
  (buttons nil :documentation "The buttons of a list's whole text.")
  (entries nil :documentation "The entries of a list, as made, a vector.")
  (first-line nil :documentation "The line of a list's whole text its first entry is on.")
  (lines nil :documentation "The text of each entry's line, or NIL until worked out.")
  (total nil :documentation "How many entries a list shows, headings aside, or NIL until counted."))

(setf (documentation 'make-list-source 'function)
      "A LIST-SOURCE of a list's TEXT, SPANS, BUTTONS, ENTRIES and FIRST-LINE.")

(define-record (list-filtering (:constructor make-list-filtering ()) (:copier nil))
  "How a list is narrowed. FILTERS are those kept, in the order they were added.
TYPING is the filter being typed at the search prompt, or NIL. SOURCE is the
whole list (a LIST-SOURCE) while filters narrow it. SHOWN is the bits of the
entries shown, or NIL for all. JOB is the filters being worked out in the
background. The rest is what the list says of itself (SET-SEARCH-FILTERING):
TEXT, NOUNS, HEADING, EMPTY-HEADINGS, DECORATION, SEARCHES, PARTS and
LIGHT-PART. For a file of records, LINES is how many header lines it has, always
shown: 0 for none. There SOURCE is the version of its text the filters were
worked out for, SHOWN the runs of lines shown and TOTAL how many records it has,
once known. While JOB works the filters out, SHOWN is what they match from the
window's top. SLOW is true when it works them out because a pattern was too slow
on this text for the editor's thread. DUE holds the filters whose job waits for
typing to pause."
  (filters '() :documentation "The filters kept on a list, in the order they were added.")
  (typing nil :documentation "The filter being typed at the search prompt, or NIL.")
  (source nil :documentation "The whole list, a LIST-SOURCE, while filters narrow it.")
  (shown nil :documentation "The bits of the entries shown, or NIL for all of them.")
  (shown-count 0 :documentation "How many entries are shown, headings aside.")
  (job nil :documentation "The job working the filters out, or NIL.")
  (text nil :documentation "A function of an entry and a part's name giving more text to match, or NIL.")
  (nouns "entries" :documentation "What the list calls its entries, in the plural.")
  (heading nil :documentation "A function true of an entry that heads a section, or NIL.")
  (empty-headings t :documentation "True when a heading with nothing under it stays while filters narrow the list.")
  (decoration nil :documentation "A function true of a line's entry that goes with its section's heading, or NIL.")
  (searches nil :documentation "True when the list's searches narrow it.")
  (parts nil :documentation "The names of the parts of an entry a filter may look at alone, or NIL.")
  (light-part nil
              :documentation "A function of a part's name and a regular expression giving the expression
that lights that part's matches in an entry opened, or NIL.")
  (lines nil
         :documentation "For a file of records whose lines the filters hide, how many header
lines it has (0 for none); NIL for a list of entries.")
  (total nil :documentation "How many records a file narrowed by filters has, or NIL until known.")
  (slow nil
        :documentation "True when the filters are worked out in the background
because a pattern was slow on this text.")
  (due nil
       :documentation "The filters still to work out and what the ones before leave, (LEFT
PREVIOUS), while their job waits for typing to pause; or NIL."))

(defvar *buffers-filtered-by-lines* '()
  "The buffers whose lines search filters hide, looked at before each
redisplay for a change that reached into what they hide (REFILTER-TOUCHED).")

(defun list-filtering (buffer)
  "How BUFFER, a list, is narrowed by search filters (a LIST-FILTERING), or NIL."
  (tuatara.buffer:buffer-local buffer 'list-filtering))

(defun list-filtering-made (buffer)
  "BUFFER's LIST-FILTERING, made when it has none: one hiding lines, its
entries called records, for a buffer whose mode says so (FILTERS-HIDE-LINES-P)."
  (or (list-filtering buffer)
      (let ((filtering (make-list-filtering)))
        (let ((mode (buffer-major-mode buffer)))
          (when (filters-hide-lines-p mode buffer)
            (setf (list-filtering-lines filtering) (if (list-header-p buffer) 1 0)
                  (list-filtering-nouns filtering) "records")))
        (setf (tuatara.buffer:buffer-local buffer 'list-filtering) filtering))))

(defun set-search-filtering (buffer &key text (nouns "entries") heading (empty-headings t) decoration searches parts
                                        light-part)
  "Say how BUFFER, a list made by RENDER-PIECES, is narrowed by search filters
\(\\[search-filter-regex]). TEXT is a function of an entry and the name of a part
of it (NIL for all). It gives more of the entry to match than its line, as a
string or NIL, and is called in the background (it must not touch the editor).
NOUNS names the entries in the plural, for the line saying how many are shown.
HEADING is a function of an entry, true of one that heads a section. A heading
is shown while an entry under it is, and is never matched itself. Unless
EMPTY-HEADINGS is NIL, it is also shown while nothing at all is under it.
DECORATION is a function of an entry, true of a line that goes with the heading
above it (a time grid's line under a day). Such a line is never matched nor
counted, and is shown while that heading is. With SEARCHES,
\\[regex-search-forward] and \\[regex-search-backward] there narrow the list
too. PARTS names the parts of an entry a filter may look at alone (the
headers of a message), each a string or (NAME . WORDS), WORDS shown beside
NAME when one is asked for: \\[search-filter-show-records] and
\\[search-filter-hide-records] offer them in place of a table's columns, and
TEXT is then called with the part's name, giving that part's text, or NIL
for an entry without it, which the filter does not match. LIGHT-PART, a
function of a part's name and a regular expression, gives the expression
that lights the matches of a filter on that part in an entry opened
\(LIGHT-SEARCH-FILTERS), its own groups written (?:...), none that count;
without it such a filter lights nothing. Returns
BUFFER."
  (let ((filtering (list-filtering-made buffer)))
    (setf (list-filtering-text filtering) text (list-filtering-nouns filtering) nouns
          (list-filtering-heading filtering) heading (list-filtering-empty-headings filtering) empty-headings
          (list-filtering-decoration filtering) decoration (list-filtering-searches filtering) searches
          (list-filtering-parts filtering) parts (list-filtering-light-part filtering) light-part))
  buffer)

(defun search-filters-list-p (buffer &optional searches)
  "True when BUFFER is a list a search can narrow, one made by RENDER-PIECES
with its entries; with SEARCHES, only when its searches narrow it
\(SET-SEARCH-FILTERING)."
  (and (not (eq :none (tuatara.buffer:buffer-local buffer 'list-entries :none)))
       (or (not searches)
           (let ((filtering (list-filtering buffer))) (and filtering (list-filtering-searches filtering))))))

(defun records-file-p (buffer)
  "True when BUFFER is a file of records, a line each: a CSV or TSV, a log,
a JSON Lines file and the others *LINE-RECORD-MODES* names
\(FILTERS-HIDE-LINES-P)."
  (filters-hide-lines-p (buffer-major-mode buffer) buffer))

(defun say-not-records (command &optional instead)
  "Say that COMMAND, a command's name, was not run because the buffer is no
file of records (RECORDS-FILE-P), as SAY-NOT-RUN says it, and INSTEAD, words
naming what may serve, when given; NIL."
  (message "Not a file of records (a CSV, a log): ~(~a~) was not run~@[; ~a~]" command instead)
  nil)

(defun filter-question (words what)
  "The question asking for a filter's pattern, WORDS (keep lines) heading it,
naming WHAT of each record or entry the pattern is tried on when given: keep
lines whose CITY holds."
  (format nil "~a~@[ whose ~a holds~]" words what))

(defun search-filterable-p (buffer)
  "True when search filters can narrow BUFFER: a list with its entries
\(SEARCH-FILTERS-LIST-P), or a file of records whose mode hides the lines
they leave out (RECORDS-FILE-P)."
  (or (search-filters-list-p buffer) (records-file-p buffer)))

(defun say-unfilterable (command)
  "Say that COMMAND, a command's name, was not run because only a list or a
file of records takes a filter (SAY-NOT-RUN); NIL."
  (say-not-run command "Not a list or a file of records (a CSV, a log)"))

(defun filters-lines-p (buffer)
  "True when search filters narrow BUFFER by hiding its lines."
  (let ((filtering (list-filtering buffer)))
    (and filtering (list-filtering-lines filtering))))

(defun search-filter-chain (filtering)
  "The filters narrowing a list, as FILTERING has them: those kept, then the one being typed."
  (append (list-filtering-filters filtering)
          (and (list-filtering-typing filtering) (list (list-filtering-typing filtering)))))

(defun search-filters (buffer)
  "The search filters kept on BUFFER, a list, in the order they were added;
NIL for none."
  (let ((filtering (list-filtering buffer)))
    (and filtering (list-filtering-filters filtering))))

;;; Which entries a filter leaves

(defun list-source-entry-lines (source)
  "The text of the line of each entry of SOURCE, a LIST-SOURCE, a vector of
strings, worked out the first time. O(size of the text)."
  (or (list-source-lines source)
      (let* ((text (list-source-text source))
             (entries (list-source-entries source))
             (lines (make-array (length entries) :initial-element ""))
             (start 0))
        (loop repeat (list-source-first-line source)
              while start
              do (setf start (let ((end (position #\Newline text :start start))) (and end (1+ end)))))
        (loop for i from 0 below (length entries)
              while (and start (< start (length text)))
              do (let ((end (or (position #\Newline text :start start) (length text))))
                   (setf (svref lines i) (subseq text start end)
                         start (1+ end))))
        (setf (list-source-lines source) lines))))

(defun decoration-entry-p (filtering entry)
  "True when ENTRY is a line of the list FILTERING narrows that goes with the
heading above it, as the list's own DECORATION says (SET-SEARCH-FILTERING)."
  (let ((decoration (list-filtering-decoration filtering)))
    (and entry decoration (funcall decoration entry) t)))

(defun heading-entry-p (filtering entry)
  "True when ENTRY heads a section of the list FILTERING narrows, goes with
a heading, or shows no entry at all: a section's line, (:SECTION NAME) as
every list writes it, or what the list's own HEADING or DECORATION says is
one. Such a line is never matched nor counted."
  (or (null entry)
      (and (consp entry) (eq (first entry) :section))
      (let ((heading (list-filtering-heading filtering)))
        (and heading (funcall heading entry) t))
      (decoration-entry-p filtering entry)))

(defun listed-entry-count (filtering source)
  "How many entries SOURCE, the whole list FILTERING narrows, shows, headings
aside, an entry shown on several lines counted once (ENTRY-START-P): counted
the first time, O(entries), and kept with SOURCE."
  (or (list-source-total source)
      (setf (list-source-total source)
            (let ((entries (list-source-entries source)))
              (loop for i from 0 below (length entries)
                    count (and (entry-start-p entries i) (not (heading-entry-p filtering (svref entries i)))))))))

(defun entry-matches-p (filtering filter entry line-matched)
  "True when FILTER matches ENTRY of the list FILTERING narrows: its line,
when LINE-MATCHED is true (LIST-LINES-MATCHED), or the more text the list
gives of it; only that text when FILTER looks at a part of it. An entry
whose text cannot be had is not matched by it."
  (let ((text (list-filtering-text filtering)) (where (search-filter-where filter)))
    (when (and (consp where) (eq (first where) :column))
      ;; a column of a table: the text of that cell as the list was last drawn
      (destructuring-bind (column list) (rest where)
        (let ((cell (list-table-cell list entry column)))
          (return-from entry-matches-p (and cell (search-filter-match-p filter cell))))))
    (or (and (null where) line-matched)
        (and text
             (let ((more (ignoring-errors-reported ("reading an entry's text to filter it")
                           (funcall text entry where))))
               (and (stringp more) (search-filter-match-p filter more)))))))

(defun entry-end (entries index)
  "The index of the first line of ENTRIES, a list's, after INDEX that does not
show the entry line INDEX shows: the next entry's first line."
  (let ((entry (svref entries index)))
    (if entry
        (or (position-if-not (lambda (other) (eq other entry)) entries :start (1+ index)) (length entries))
        (1+ index))))

(defun list-lines-matched (filter lines low high candidate-p)
  "Which of LINES, the lines of a list (strings), from LOW below HIGH, FILTER's
pattern matches, as bits from LOW. Each run of lines in a row for which
CANDIDATE-P, a function of a line's index, is true, is searched as one text, a
line after another. A line is matched when any part of a match lies on it, from
the line holding the match's first character through the line holding its last,
or when an empty match is in it. The lines of a file of records are matched the
same way (LINES-MATCHING)."
  (let ((bits (make-array (- high low) :element-type 'bit :initial-element 0))
        (scanner (search-filter-scanner filter))
        (k low))
    (loop while (< k high)
          do (if (not (funcall candidate-p k))
                 (incf k)
                 (let* ((end (or (loop for j from (1+ k) below high unless (funcall candidate-p j) return j) high))
                        (starts (make-array (1+ (- end k)) :element-type 'fixnum))
                        (text (with-output-to-string (out)
                                (loop for j from k below end
                                      for at = 0 then (+ at 1 (length (svref lines (1- j))))
                                      do (setf (aref starts (- j k)) at)
                                         (write-string (svref lines j) out)
                                         (write-char #\Newline out))))
                        (size (length text)))
                   (setf (aref starts (- end k)) size)
                   (labels ((line-at (at) (1- (tuatara.runtime:bisect (j 0 (length starts)) (<= (aref starts j) at))))
                            (unit-of (at from)
                              (declare (ignore from))
                              (let ((j (line-at at)))
                                (values (aref starts j) (1- (aref starts (1+ j))) (aref starts (1+ j)))))
                            (take (start text-end after)
                              (declare (ignore text-end after))
                              (setf (sbit bits (- (+ k (line-at start)) low)) 1)))
                     (let ((marked 0))
                       (tuatara.runtime:stoppable
                        (cl-ppcre:do-scans (s e rs re scanner text)
                          (when (< s size)
                            (setf marked (map-match-units #'take #'unit-of s e marked size)))))))
                   (setf k end))))
    bits))

(defun work-out-filter (filtering filter source previous &optional progress)
  "Which entries of SOURCE, a LIST-SOURCE, FILTER leaves of those PREVIOUS
leaves (bits, or NIL for all), as two values: their bits and how many they
are. The lines of the entries left are matched together (LIST-LINES-MATCHED),
a match running over several lines matching each of them; an entry shown on
several lines in a row is matched by any of them, and left or not as a
whole. PROGRESS, a function of no arguments, is called for each entry looked
at. Headings are left for LIST-SHOWN-LINES. O(entries) matches."
  (let* ((entries (list-source-entries source))
         (lines (list-source-entry-lines source))
         (total (length entries))
         (bits (make-array total :element-type 'bit :initial-element 0))
         (matched (and (null (search-filter-where filter))
                       (list-lines-matched filter lines 0 total
                                           (lambda (k) (and (or (null previous) (= 1 (sbit previous k)))
                                                            (not (heading-entry-p filtering (svref entries k))))))))
         (count 0) (i 0))
    (loop while (< i total)
          do (let ((entry (svref entries i))
                   (end (entry-end entries i)))
               (when (and (or (null previous) (= 1 (sbit previous i)))
                          (not (heading-entry-p filtering entry)))
                 (when progress (funcall progress))
                 (when (eq (not (search-filter-hide filter))
                           (entry-matches-p filtering filter entry (and matched (find 1 matched :start i :end end) t)))
                   (fill bits 1 :start i :end end)
                   (incf count)))
               (setf i end)))
    (values bits count)))

(defun forget-worked-out (filters)
  "Have each of FILTERS worked out again: what it leaves, and the list or
text it was worked out for, are forgotten."
  (dolist (filter filters)
    (setf (search-filter-source filter) nil (search-filter-kept filter) nil)))

(defun keep-worked-out (filter source bits count &optional (cut 0))
  "Keep in FILTER that it leaves the entries BITS, COUNT of them, of SOURCE,
the whole list, CUT records tried on their start alone (LINES-DECODED).
Returns BITS."
  (setf (search-filter-kept filter) bits
        (search-filter-count filter) count
        (search-filter-cut filter) cut
        (search-filter-source filter) source)
  bits)

(defun carry-filters-over (filtering old new)
  "Have the filters of FILTERING, every one worked out for OLD, a list's
source, stand for NEW, the list drawn again with entries OLD had (EQUAL),
as when an entry is marked or a section folded: what each leaves carried
over, nothing matched again. True when that could be done; NIL, nothing
changed, when NEW has an entry OLD had not or a filter is not worked out.
O(entries)."
  (let ((chain (search-filter-chain filtering))
        (places (make-hash-table :test 'equal))
        (entries (list-source-entries new)))
    (when (and chain (every (lambda (filter) (eq (search-filter-source filter) old)) chain))
      (loop for entry across (list-source-entries old)
            for i from 0
            when entry do (setf (gethash entry places) i))
      (when (every (lambda (entry) (or (null entry) (nth-value 1 (gethash entry places)))) entries)
        (dolist (filter chain t)
          (let ((before (search-filter-kept filter))
                (bits (make-array (length entries) :element-type 'bit :initial-element 0))
                (count 0))
            (loop for entry across entries
                  for i from 0
                  when (and entry (= 1 (sbit before (gethash entry places))))
                    do (setf (sbit bits i) 1)
                       (when (and (entry-start-p entries i) (not (heading-entry-p filtering entry))) (incf count)))
            (keep-worked-out filter new bits count)))))))

(defun filters-to-work-out (filtering)
  "The filters of FILTERING's chain whose entries are not yet worked out for
its whole list, with the bits of the last one before them that is (NIL for
all entries), as two values."
  (let ((source (list-filtering-source filtering)) (previous nil) (left '()) (stale nil))
    (dolist (filter (search-filter-chain filtering))
      (if (and (not stale) (eq (search-filter-source filter) source))
          (setf previous (search-filter-kept filter))
          (progn (setf stale t) (push filter left))))
    (values (nreverse left) previous)))

(defparameter *search-filter-quick-characters* (* 2 1024 1024)
  "The most characters of text a list may have to be filtered at once, as it is
typed; a larger list, or one that gives more text of its entries, is filtered
in the background.")

(defvar *search-filter-screen-seconds* 1/100
  "Seconds a search filter typed in a large list or file
\(\\[search-filter-regex]) spends at each key on the lines from the top of the
window, so that what it matches there is shown at once. The rest is matched in
the background on all but two cores, the mode line saying how far it has got,
and the next key starts again. The value is a hundredth of a second, within the
time a frame is drawn in, so that typing never waits on it.")

(defun filter-quickly-p (filtering &optional previous)
  "True when what FILTERING narrows is small enough to filter at once, on the
editor's thread. For a list, that means no more text given of its entries than
their lines, and no more text than *SEARCH-FILTER-QUICK-CHARACTERS* characters.
For a file of records, it means no more octets to look at than that, namely
those of the runs PREVIOUS leaves (LINES-MATCHING), or of every record when it
is NIL."
  (if (list-filtering-lines filtering)
      (< (if previous
             (runs-octets previous)
             (tuatara.text:rope-octet-length (tuatara.text:version-rope (list-filtering-source filtering))))
         *search-filter-quick-characters*)
      (and (null (list-filtering-text filtering))
           (< (length (list-source-text (list-filtering-source filtering))) *search-filter-quick-characters*))))
