;;;; lisp/editing/sort.lisp --- Sorting lines and other records; deleting duplicate lines.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Emacs's commands, each doing one thing: sort-lines, reverse-region,
;;;; sort-fields, sort-numeric-fields, sort-columns, sort-regexp-fields,
;;;; sort-paragraphs, sort-pages and delete-duplicate-lines; and a -reverse
;;;; command for each sort, in descending order. They act on what the selection
;;;; touches, the lines commands on its whole lines, and without a selection do
;;;; nothing and say so. Records are sorted stably, the text between them
;;;; staying where it was; a prefix argument sorts in descending order, ties
;;;; reversed with the rest, as Emacs has it.
;;;;
;;;; Every one runs on a thread of its own and is waited for briefly: a small
;;;; region looks done at once, a large one carries on in the background with
;;;; its progress in the mode line. A region under *SORT-IN-MEMORY-LIMIT* is
;;;; handled in memory. A larger region of lines is cut into pieces at line
;;;; starts, and the pool's workers handle a piece each: for a sort, into an
;;;; unnamed run file, the runs then merged into the result; for duplicates,
;;;; into a file of the lines kept, the files then joined. So a hundred million
;;;; lines sort without their text in memory. The result arrives as one undoable
;;;; step; an edit to the region meanwhile abandons it, edits elsewhere are kept.

(in-package #:tuatara.defaults)

(define-setting *sort-fold-case* nil
  "Whether sorting ignores the case of ASCII letters.
- t: ignore it; \"b\" sorts before \"C\".
- nil: letters sort as they are written (the standard)."
  :values boolean)

(defvar *sort-numeric-base* 10
  "The base sort-numeric-fields reads numbers in, unless they begin with 0x.")

(defvar *sort-in-memory-limit* (* 32 1024 1024)
  "Regions of this many octets or more are sorted or cleared of duplicate
lines on disk, in pieces, in the background.")

(defvar *sort-piece* (* 64 1024 1024)
  "Octets of a large region each worker sorts at a time.")

(defvar *sort-piece-lines* (* 4 1024 1024)
  "Lines a piece of a large region holds at most, and lines a region may have
to be sorted or cleared of duplicate lines in memory; fewer when the heap has
less room (PIECE-LINES). A piece of short or empty lines is cut at this many,
and a region of more is handled on disk in pieces, or refused by the commands
that work in memory alone.")

(defconstant +octets-per-piece-line+ 56
  "Octets work on a piece of lines holds for each line: where it starts, where
its text and its key start and end, the next line's start, its place in the
order, its number and the sort's scratch.")

(defun piece-lines (&optional (workers 1))
  "Lines one piece of a region may hold while WORKERS pieces are worked on at
once: *SORT-PIECE-LINES*, or fewer when the heap's room (HEAP-BUDGET) shared
among them holds fewer at +OCTETS-PER-PIECE-LINE+ octets a line, though not
fewer than 1,024 for that."
  (min *sort-piece-lines* (max 1024 (floor (heap-budget) (* workers +octets-per-piece-line+)))))

(defvar *filter-piece* (* 8 1024 1024)
  "Octets of a large region each worker clears of duplicate lines at a time
\(delete-duplicate-lines), or of a large file's records it matches search
filters against (search-filter-regex).")

(defparameter *sort-carried-settings*
  '(*sort-piece* *sort-piece-lines* *filter-piece* *sort-numeric-base* *sort-in-memory-limit*)
  "The settings a sort or filter reads as it works, carried to the thread it
works on as they are on the editor's when it starts
\(TUATARA.RUNTIME:CARRY-SETTINGS).")

;;; What is selected

(defun selected-lines (buffer &key columns)
  "The start and end of the whole lines BUFFER's selection touches, or NIL
without a selection: from the start of its first line to the start of the line
after its last, or the text's end. Lines are those a window shows, as a
line selection takes them (WHOLE-LINES): a folded headline's line is whole
with all the fold hides. The last line is the one its end is in, except that
an end at the start of a line leaves that line out, unless COLUMNS is true
\(sort-columns counts it)."
  (when (tuatara.buffer:buffer-mark-active-p buffer)
    (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
      (when (and start (< start end))
        (whole-lines buffer start end :visible t :columns columns)))))

(defvar *sort-descending* nil
  "True while one of the reverse sort commands runs (\\[sort-lines-reverse] and
the others): the sort is in descending order, whatever the prefix argument.")

(defvar *sort-command* nil
  "The reverse sort command running, to name in what is said, or NIL.")

(defun sort-command-name (command)
  "The name to say for COMMAND: the reverse sort command running it, if any."
  (or *sort-command* command))

(defun sort-descending-p ()
  "True to sort in descending order: in a reverse sort command, or with a
prefix argument."
  (or *sort-descending* (and (prefix-argument) t)))

(defmacro with-selected-lines ((buffer start end command &key columns) &body body)
  "Run BODY with START and END bound to the whole lines the current buffer's
selection touches and BUFFER to the buffer; without a selection, say that
COMMAND was not run. Refused when the buffer is read-only (CHECK-WRITABLE) or
search filters hide any of those lines (CHECK-HIDDEN-LINES)."
  `(let ((,buffer (current-buffer)))
     (multiple-value-bind (,start ,end) (selected-lines ,buffer :columns ,columns)
       (if (null ,start)
           (say-not-run (sort-command-name ,command))
           (progn (check-writable ,buffer)
                  (check-hidden-lines ,buffer ,start ,end)
                  ,@body)))))

;;; Lines of octets

(defun scan-lines (octets crlf)
  "The lines of OCTETS, which hold whole lines: three fixnum vectors of each
line's start, the end of its text (before the octet that ends it, and in a
carriage return and line feed (CRLF) buffer before a carriage return ahead of
a newline) and the start of the line after."
  (declare (type tuatara.text:octets octets))
  (let* ((size (length octets))
         (n (+ (tuatara.text:octets-line-ends octets 0 size)
               (if (and (plusp size) (not (tuatara.text:octets-line-end-p octets (1- size) size))) 1 0)))
         (starts (make-array n :element-type 'fixnum))
         (ends (make-array n :element-type 'fixnum))
         (nexts (make-array n :element-type 'fixnum))
         (p 0) (k 0))
    (declare (type fixnum size p k))
    (loop while (< p size)
          do (let* ((newline (or (tuatara.text:octets-line-end-position octets p size) size))
                    (text-end (if (< newline size) (tuatara.text:line-text-end octets p newline crlf) newline))
                    (next (min size (1+ newline))))
               (declare (type fixnum newline text-end next))
               (setf (aref starts k) p (aref ends k) text-end (aref nexts k) next)
               (incf k)
               (setf p next)))
    (values starts ends nexts)))

(defun rope-line-before (rope at crlf)
  "The text of ROPE's line that ends just before AT, a line's start, as octets,
without its line end (a carriage return too with carriage return and line feed
(CRLF))."
  (let* ((end (tuatara.text:rope-line-text-end rope 0 (1- at) crlf))
         (start (tuatara.text:rope-line-start rope (max 0 end))))
    (tuatara.text:rope-octets rope :start start :end end)))

(defun lines-in-memory-p (rope start end)
  "True when the lines of ROPE from START below END are few enough to sort or
clear of duplicates in memory: fewer than *SORT-IN-MEMORY-LIMIT* octets and
fewer line ends than one piece may hold (PIECE-LINES)."
  (and (< (- end start) *sort-in-memory-limit*)
       (< (tuatara.text:rope-count-newlines rope start end) (piece-lines))))

(defun line-preview (octets start end)
  "The line of OCTETS from START below END as text to show in a message, cut to 60 columns."
  (let ((text (tuatara.text:octets-string octets :start start :end (min end (+ start 240)))))
    (tuatara.display:shorten-string text 60)))

;;; Keys

(declaim (inline space-or-tab-octet-p fold-octet))
(defun space-or-tab-octet-p (octet)
  "True for a space or a tab: the octets between a line's fields."
  (or (= octet 32) (= octet 9)))

(defun fold-octet (octet)
  "OCTET with an ASCII capital letter made small."
  (if (<= 65 octet 90) (+ octet 32) octet))

(defmacro define-field-bounds (name type blank-p)
  "Define NAME, a function finding a field of a line held in a vector of TYPE,
BLANK-P telling apart what separates its fields."
  `(defun ,name (text start end n)
     ,(format nil "Where field N of the line from START below END of TEXT~@
                   (~(~a~)) begins and ends: fields are separated by spaces and~@
                   tabs and counted from 1, or from the end of the line when N is~@
                   negative. NIL when the line has too few." type)
     (declare (type ,type text) (type fixnum start end n))
     (if (plusp n)
         (let ((p start))
           (declare (type fixnum p))
           (loop repeat (1- n)
                 do (loop while (and (< p end) (,blank-p (aref text p))) do (incf p))
                    (loop while (and (< p end) (not (,blank-p (aref text p)))) do (incf p)))
           (loop while (and (< p end) (,blank-p (aref text p))) do (incf p))
           (when (< p end)
             (let ((e p))
               (declare (type fixnum e))
               (loop while (and (< e end) (not (,blank-p (aref text e)))) do (incf e))
               (values p e))))
         (let ((p end))
           (declare (type fixnum p))
           (loop repeat (1- (- n))
                 do (loop while (and (> p start) (,blank-p (aref text (1- p)))) do (decf p))
                    (loop while (and (> p start) (not (,blank-p (aref text (1- p))))) do (decf p)))
           (loop while (and (> p start) (,blank-p (aref text (1- p)))) do (decf p))
           (when (> p start)
             (let ((s p))
               (declare (type fixnum s))
               (loop while (and (> s start) (not (,blank-p (aref text (1- s))))) do (decf s))
               (values s p)))))))

(define-field-bounds field-bounds tuatara.text:octets space-or-tab-octet-p)
(define-field-bounds text-field-bounds simple-string space-or-tab-char-p)

(defun digit-value (octet)
  "The value of OCTET as a digit of up to base 36, or NIL."
  (cond ((<= 48 octet 57) (- octet 48))
        ((<= 97 (fold-octet octet) 122) (+ 10 (- (fold-octet octet) 97)))
        (t nil)))

(defun field-number (octets start end)
  "The number written at START of OCTETS, read no further than END, as a
double-float: 0x and hexadecimal digits, else an optional sign and digits in
*SORT-NUMERIC-BASE*, in base 10 with a fraction and an exponent too; 0 when
none is there. A leading zero does not make a number octal, so zero-padded
numbers sort as the numbers they are."
  (declare (type tuatara.text:octets octets) (type fixnum start end))
  (let ((p start) (sign 1) (base *sort-numeric-base*))
    (declare (type fixnum p))
    (when (and (< p end) (member (aref octets p) '(43 45)))
      (when (= (aref octets p) 45) (setf sign -1))
      (incf p))
    (when (and (< (1+ p) end) (= (aref octets p) 48) (member (aref octets (1+ p)) '(120 88)))
      (setf base 16)
      (incf p 2))
    ;; past 20 significant digits, more than a double holds, only their count matters
    (let ((whole 0) (digits 0) (kept 0) (shift 0))
      (declare (type fixnum digits kept shift))
      (loop while (< p end)
            do (let ((d (digit-value (aref octets p))))
                 (unless (and d (< d base)) (return))
                 (if (< kept 20)
                     (progn (setf whole (+ (* whole base) d))
                            (when (plusp whole) (incf kept)))
                     (incf shift))
                 (incf digits)
                 (incf p)))
      (let ((value whole) (exponent 0))
        (when (= base 10)
          (when (and (< p end) (= (aref octets p) 46))
            (incf p)
            (let ((scale 1))
              (loop while (and (< p end) (<= 48 (aref octets p) 57))
                    do (when (< kept 20)
                         (setf scale (* scale 10) value (+ value (/ (- (aref octets p) 48) scale)))
                         (when (plusp value) (incf kept)))
                       (incf digits)
                       (incf p))))
          (when (and (plusp digits) (< (1+ p) end) (member (aref octets p) '(101 69)))
            (let ((q (1+ p)) (exponent-sign 1) (exponent-digits 0))
              (declare (type fixnum q))
              (when (and (< q end) (member (aref octets q) '(43 45)))
                (when (= (aref octets q) 45) (setf exponent-sign -1))
                (incf q))
              (loop while (and (< q end) (<= 48 (aref octets q) 57))
                    do (setf exponent (min 400 (+ (* exponent 10) (- (aref octets q) 48)))
                             exponent-digits (1+ exponent-digits))
                       (incf q))
              (unless (plusp exponent-digits) (setf exponent 0))
              (setf exponent (* exponent-sign exponent)))))
        (setf value (* value (expt base (max -400 (min 1100 (+ shift (if (= base 10) exponent 0)))))))
        (cond ((zerop digits) 0d0)
              ;; past what a double holds: the largest there is, of the sign
              (t (handler-case (coerce (* sign value) 'double-float)
                   (arithmetic-error () (if (minusp sign) most-negative-double-float most-positive-double-float)))))))))

(defun column-bounds (octets start end from to &optional (tab-width 8))
  "Where the text in display columns FROM below TO of the line from START below
END of OCTETS begins and ends: a tab reaches the next multiple of TAB-WIDTH,
other characters take the columns the display gives them."
  (declare (type tuatara.text:octets octets) (type fixnum start end from to tab-width))
  (let ((column 0) (p start) (key-start nil) (before 0))
    (declare (type fixnum column p before))
    (loop while (< p end)
          do (when (and (null key-start) (>= column from)) (setf key-start p))
             (when (>= column to) (return))
             (multiple-value-bind (code length) (tuatara.text:octets-decode octets p end)
               (multiple-value-bind (columns next) (tuatara.display:advance-column code column before tab-width nil)
                 (incf column columns)
                 (setf before next))
               (incf p length)))
    (values (or key-start p) p)))

(defun line-keys (octets starts ends key)
  "The sort keys of the lines of OCTETS (as SCAN-LINES gives them) for KEY, one
of (:LINE), (:INDEX), (:FIELD N), (:NUMERIC N) and (:COLUMNS FROM TO TAB-WIDTH):
two fixnum vectors of where each key starts and ends, and for :NUMERIC a
double-float vector of the numbers. A line with too few fields stops the sort,
naming it."
  (let* ((n (length starts))
         (key-starts (make-array n :element-type 'fixnum))
         (key-ends (make-array n :element-type 'fixnum))
         (numbers (and (eq (first key) :numeric) (make-array n :element-type 'double-float))))
    (dotimes (i n)
      (let ((s (aref starts i)) (e (aref ends i)))
        (ecase (first key)
          ((:line :index) (setf (aref key-starts i) s (aref key-ends i) e))
          ((:field :numeric)
           (multiple-value-bind (fs fe) (field-bounds octets s e (second key))
             (unless fs
               (refuse "A line has too few fields for field ~d~@[: ~a~]"
                       (second key) (and (not *secret-at-hand*) (line-preview octets s e))))
             (setf (aref key-starts i) fs (aref key-ends i) fe)
             (when numbers (setf (aref numbers i) (field-number octets fs fe)))))
          (:columns
           (multiple-value-bind (cs ce) (column-bounds octets s e (second key) (third key) (or (fourth key) 8))
             (setf (aref key-starts i) cs (aref key-ends i) ce))))))
    (values key-starts key-ends numbers)))

(defun octet-runs-compare (octets a1 e1 a2 e2 fold)
  "-1, 0 or 1 as OCTETS from A1 below E1 come before, are the same as, or come
after those from A2 below E2: octet by octet, which for UTF-8 is the order of
the characters' codes, a shorter run before a longer one it begins; the case of
ASCII letters ignored with FOLD."
  (declare (type tuatara.text:octets octets) (type fixnum a1 e1 a2 e2) (optimize speed (safety 0)))
  (loop
    (cond ((= a1 e1) (return (if (= a2 e2) 0 -1)))
          ((= a2 e2) (return 1)))
    (let ((x (aref octets a1)) (y (aref octets a2)))
      (when fold (setf x (fold-octet x) y (fold-octet y)))
      (cond ((< x y) (return -1))
            ((> x y) (return 1))))
    (incf a1) (incf a2)))

(defun sap-compare (sap1 a1 e1 sap2 a2 e2 fold)
  "OCTET-RUNS-COMPARE over memory: SAP1 from A1 below E1 against SAP2 from A2 below E2."
  (declare (type sb-sys:system-area-pointer sap1 sap2) (type fixnum a1 e1 a2 e2) (optimize speed (safety 0)))
  (loop
    (cond ((= a1 e1) (return (if (= a2 e2) 0 -1)))
          ((= a2 e2) (return 1)))
    (let ((x (sb-sys:sap-ref-8 sap1 a1)) (y (sb-sys:sap-ref-8 sap2 a2)))
      (when fold (setf x (fold-octet x) y (fold-octet y)))
      (cond ((< x y) (return -1))
            ((> x y) (return 1))))
    (incf a1) (incf a2)))

(defun key-order (octets key-starts key-ends numbers key reverse fold)
  "The order of the lines by their keys: a fixnum vector of line indices,
sorted stably; for the :INDEX key the lines as they are. REVERSE reverses the
whole order, so that descending order reverses ties too."
  (let* ((n (length key-starts)) (order (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref order i) i))
    (unless (eq (first key) :index)
      (setf order
            (if numbers
                (stable-sort order (lambda (i j) (declare (type fixnum i j)) (< (aref numbers i) (aref numbers j))))
                (stable-sort order (lambda (i j)
                                     (declare (type fixnum i j))
                                     (minusp (octet-runs-compare octets (aref key-starts i) (aref key-ends i)
                                                             (aref key-starts j) (aref key-ends j) fold)))))))
    (if reverse (nreverse order) order)))

(defun arrange-lines (octets starts ends nexts order)
  "Fresh octets holding the lines of OCTETS in ORDER, each line end staying
where it was: place I holds the text of line (ORDER I), then the end of line I."
  (let ((out (make-array (length octets) :element-type '(unsigned-byte 8))) (dst 0))
    (declare (type fixnum dst))
    (dotimes (slot (length order) out)
      (let ((r (aref order slot)))
        (replace out octets :start1 dst :start2 (aref starts r) :end2 (aref ends r))
        (incf dst (- (aref ends r) (aref starts r)))
        (replace out octets :start1 dst :start2 (aref ends slot) :end2 (aref nexts slot))
        (incf dst (- (aref nexts slot) (aref ends slot)))))))

(defun each-kept-line (octets starts nexts marks emit)
  "Call EMIT with OCTETS, a start and an end for each line of OCTETS that MARKS,
a bit vector, does not mark (1 for a line to delete), its line end with it."
  (dotimes (i (length starts))
    (when (zerop (sbit marks i))
      (funcall emit octets (aref starts i) (aref nexts i)))))

(defun kept-lines (octets starts nexts marks)
  "Fresh octets holding the lines of OCTETS that MARKS leaves (EACH-KEPT-LINE), each with its line end."
  (let ((out (make-array (length octets) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (each-kept-line octets starts nexts marks (lambda (from start end) (setf out (put-octets out from start end))))
    (coerce out 'tuatara.text:octets)))

;;; Marking lines to delete

(defun octet-run-hash (octets start end)
  "A hash of OCTETS from START below END, the same for the same octets."
  (ldb (byte 62 0) (tuatara.text:octets-hash octets :start start :end end)))

(defun duplicate-marks (octets starts ends reverse adjacent keep-blanks)
  "A bit vector marking the lines of OCTETS that delete-duplicate-lines deletes:
all but the first of identical lines (the last with REVERSE); with ADJACENT only
a line identical to the one before it; never an empty line with KEEP-BLANKS.
Returns it and how many are marked. Lines are compared, and kept to compare,
as the octets they are: nothing is made for a line."
  (declare (type tuatara.text:octets octets) (type (simple-array fixnum (*)) starts ends))
  (let* ((n (length starts)) (marks (make-array n :element-type 'bit :initial-element 0)) (count 0)
         (seen (unless adjacent
                 ;; a line by its index, compared and hashed by its octets
                 (make-hash-table :test (lambda (i j)
                                          (declare (type fixnum i j))
                                          (zerop (octet-runs-compare octets (aref starts i) (aref ends i)
                                                                     (aref starts j) (aref ends j) nil)))
                                  :hash-function (lambda (i)
                                                   (declare (type fixnum i))
                                                   (octet-run-hash octets (aref starts i) (aref ends i)))
                                  :size (max 16 (min n 100000)))))
         (previous-start -1) (previous-end -1))
    (declare (type fixnum n count previous-start previous-end))
    (flet ((visit (i)
             (let ((s (aref starts i)) (e (aref ends i)))
               (unless (and keep-blanks (= s e))
                 (if adjacent
                     (progn
                       (when (and (>= previous-start 0)
                                  (zerop (octet-runs-compare octets s e previous-start previous-end nil)))
                         (setf (sbit marks i) 1)
                         (incf count))
                       (setf previous-start s previous-end e))
                     (if (gethash i seen)
                         (progn (setf (sbit marks i) 1) (incf count))
                         (setf (gethash i seen) t)))))))
      (if reverse
          (loop for i from (1- n) downto 0 do (visit i))
          (dotimes (i n) (visit i))))
    (values marks count)))

;;; Other records, in memory: paragraphs, pages, regexp matches

(defun blank-line-p (octets start end)
  "True when the line of OCTETS from START below END holds only spaces, tabs and carriage returns."
  (loop for i from start below end always (tuatara.text:blank-line-octet-p (aref octets i))))

(defun paragraph-records (octets starts ends nexts)
  "The paragraphs of OCTETS (lines as SCAN-LINES gives them): a list of (START
END) running from the first line of each paragraph to the start of the line
after its last; blank lines between them are not in any."
  (let ((records '()) (open nil))
    (dotimes (i (length starts))
      (if (blank-line-p octets (aref starts i) (aref ends i))
          (when open (push (list open (aref starts i)) records) (setf open nil))
          (unless open (setf open (aref starts i))))
      (when (and open (= i (1- (length starts))))
        (push (list open (aref nexts i)) records)))
    (nreverse records)))

(defun page-records (octets)
  "The pages of OCTETS: a list of (START END), each starting after the
line ends that begin it and ending just after the form feed at the start of a
line that ends it, or at the end."
  (let ((records '()) (p 0) (size (length octets)))
    (loop while (< p size)
          do (loop while (and (< p size) (tuatara.text:octets-line-end-p octets p size)) do (incf p))
             (when (< p size)
               (let ((end (loop for i from p below size
                                when (and (= (aref octets i) 12)
                                          (or (= i 0) (tuatara.text:octets-line-end-p octets (1- i) size))
                                          (> i p))
                                  return (1+ i)
                                finally (return size))))
                 (push (list p end) records)
                 (setf p end))))
    (nreverse records)))

(defun arrange-records (octets records order extra)
  "Fresh octets holding OCTETS with its RECORDS, (START END) in order, put in
ORDER, the text between them staying where it was; EXTRA, when not NIL, is
added to a record that ends the text without a line end, so that records moved
do not run together."
  (let ((pieces '()) (at 0) (records (coerce records 'simple-vector)))
    (dotimes (slot (length records))
      (destructuring-bind (s e) (aref records slot)
        (push (subseq octets at s) pieces)
        (destructuring-bind (rs re) (aref records (aref order slot))
          (push (subseq octets rs re) pieces)
          (when (and extra (plusp (- re rs)) (/= (aref octets (1- re)) 10) (/= (aref order slot) slot))
            (push extra pieces)))
        (setf at e)))
    (push (subseq octets at) pieces)
    (let ((out (tuatara.text:make-octets (reduce #'+ pieces :key #'length))) (at 0))
      (dolist (piece (nreverse pieces) out)
        (replace out piece :start1 at)
        (incf at (length piece))))))

(defun record-order (octets records reverse fold)
  "The order of RECORDS, (START END) of OCTETS, by their text, stably; REVERSE as for KEY-ORDER."
  (key-order octets (map '(simple-array fixnum (*)) #'first records) (map '(simple-array fixnum (*)) #'second records)
             nil (list :line) reverse fold))

(defun regexp-records (text record-regexp key)
  "The matches of RECORD-REGEXP in TEXT as a list of (START END KEY), character
indices: KEY is \\& for the whole match, \\N for group N, else a regular
expression whose first match in the record is its key (an empty key when it has
none). Empty matches are passed over."
  (let* ((records '())
         (group (cond ((string= key "\\&") 0)
                      ((and (= (length key) 2) (char= (char key 0) #\\) (digit-char-p (char key 1)))
                       (digit-char-p (char key 1)))))
         (key-scanner (and (null group) (tuatara.syntax:compile-regex key)))
         (scanner (tuatara.syntax:compile-regex record-regexp))
         (pos 0))
    (loop while (<= pos (length text))
          do (multiple-value-bind (ms me rs re) (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start pos))
               (unless ms (return))
               (when (< ms me)
                 (push (list ms me
                             (cond ((eql group 0) (subseq text ms me))
                                   (group (let ((a (and (< (1- group) (length rs)) (aref rs (1- group)))))
                                            (if a (subseq text a (aref re (1- group))) "")))
                                   (t (multiple-value-bind (ks ke)
                                          (tuatara.runtime:stoppable (cl-ppcre:scan key-scanner text :start ms :end me))
                                        (if ks (subseq text ks ke) "")))))
                       records))
               (setf pos (if (= ms me) (1+ me) me))))
    (nreverse records)))

(defun sort-regexp-text (text record-regexp key reverse fold)
  "TEXT with the matches of RECORD-REGEXP sorted stably by KEY (REGEXP-RECORDS),
the text between them staying where it was; with REVERSE descending. Returns the
new text and the number of records."
  (let* ((records (coerce (regexp-records text record-regexp key) 'simple-vector))
         (n (length records))
         (order (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref order i) i))
    (setf order (stable-sort order (lambda (i j)
                                     (let ((a (third (aref records i))) (b (third (aref records j))))
                                       (if fold (string-lessp a b) (string< a b))))))
    (when reverse (setf order (nreverse order)))
    (values (with-output-to-string (out)
              (let ((at 0))
                (dotimes (slot n)
                  (destructuring-bind (s e key) (aref records slot)
                    (declare (ignore key))
                    (write-string text out :start at :end s)
                    (destructuring-bind (rs re key) (aref records (aref order slot))
                      (declare (ignore key))
                      (write-string text out :start rs :end re))
                    (setf at e)))
                (write-string text out :start at)))
            n)))

;;; Writing files a piece at a time

(defun file-rope (fd length)
  "A rope over the first LENGTH octets of the unnamed file open as file
descriptor (FD), which it takes: mapped, its lines counted later with the
rest of the result, all at once (START-SORT)."
  (if (zerop length)
      (progn (sb-posix:close fd) (tuatara.text:make-rope))
      (tuatara.text:mapping-rope (tuatara.text:map-fd fd length "a sorted region"))))

;;; Large regions: pieces, runs and the merge

(define-record (sort-job (:include tuatara.runtime:job) (:constructor %make-sort-job) (:copier nil))
  "A sort or filter of BUFFER's lines, a job (TUATARA.RUNTIME:JOB). WORDS says
what it does (\"Sorting\") and WHAT says it in full. PROGRESS is a cons whose
car counts the octets handled, of TOTAL. COUNTING, once the result is written,
is a cons whose car counts the octets of it whose lines are counted, of its cdr.
RESULT is the rope that replaces the region once done, SAME true when it is the
text it would replace, and MESSAGE is what to say then. REGION-START and
REGION-END follow the region through edits made meanwhile. CHANGED turns true
when one touches it, which abandons the result. BASE is the text the job began
on (JOB-BEGINS-WHERE-GIVEN), the region BASE-START below BASE-END in it, and
SINCE the buffer's undo stack there. The result stands in the undo history
there, before the changes made meanwhile
\(TUATARA.BUFFER:BUFFER-TAKE-IN-JOB-RESULT)."
  (words nil :documentation "What JOB does, as a word such as \"Sorting\".")
  (what nil :documentation "What JOB does, said in full.")
  (progress (list 0) :documentation "A cons whose car counts the octets JOB has handled.")
  (total 1 :documentation "How many octets JOB has to handle.")
  (counting nil
            :documentation "Once the result of JOB is written, a cons whose car
counts the octets of it whose lines are counted, of its cdr; NIL before.")
  (result nil :documentation "The rope that replaces the region once JOB is done, NIL before.")
  (same nil :documentation "True once JOB is done when its result is the text it would replace.")
  (message nil :documentation "What to say once JOB is done.")
  (region-start 0 :documentation "Where the region of JOB starts, following the region through edits made meanwhile.")
  (region-end 0 :documentation "Where the region of JOB ends, following the region through edits made meanwhile.")
  (changed nil :documentation "True when an edit touched the region of JOB, which abandons its result.")
  (change-function nil
                   :documentation "The function JOB put on the buffer's change
functions to follow the region, NIL when none.")
  (base nil :documentation "The text JOB began on.")
  (base-start 0 :documentation "Where the region of JOB starts in its BASE.")
  (base-end 0 :documentation "Where the region of JOB ends in its BASE.")
  (since nil :documentation "The buffer's undo stack at the place JOB began."))

(define-record (sort-run (:constructor make-sort-run (fd length lines ends)) (:copier nil))
  "A piece of a region sorted into an unnamed file open as file descriptor
(FD) of LENGTH octets. Its LINES are records in their sorted order, each a
header (the line's index in the piece, where its key starts and ends in its
text, the text's length, and a number as two halves) and the line's text.
ENDS is the length of each line's line end in the lines' first order."
  (fd nil :documentation "The descriptor (FD) of the unnamed file holding RUN, NIL once it is mapped.")
  (length nil :documentation "How many octets the file of RUN holds.")
  (lines nil :documentation "How many lines RUN has, as records in their sorted order.")
  (ends nil :documentation "The length of each line end of RUN, in the lines' first order.")
  (mapping nil :documentation "The mapping of the file of RUN into memory, NIL until it is mapped."))

(setf (documentation 'make-sort-run 'function) "A run of LINES records in the file FD of LENGTH octets.")

(defconstant +record-header+ 24 "Octets of a run record's header.")

(defmacro with-scratch-file ((fd directory) &body body)
  "Run BODY with file descriptor (FD) a new unnamed file in DIRECTORY
(OPEN-SCRATCH-FILE), closed, its octets freed, unless BODY returns a
result: when it is stopped, fails (the disk full) or returns NIL. Otherwise
what it returns holds the file open."
  (let ((result (gensym "RESULT")))
    `(let ((,fd (tuatara.io:open-scratch-file ,directory)) (,result nil))
       (unwind-protect (setf ,result (multiple-value-list (progn ,@body)))
         (unless (first ,result) (tuatara.runtime:close-fd-quietly ,fd "a file of sorted lines")))
       (values-list ,result))))

(defun discard-run (run)
  "Let go of RUN's file, a run or a piece's output, closing its file
descriptor (FD)."
  (let ((fd (if (consp run) (first run) (sort-run-fd run))))
    (tuatara.runtime:close-fd-quietly fd "a file of sorted lines")))

(defun sort-piece (job rope piece key reverse crlf fold directory)
  "Sort the lines of PIECE, (START END) of ROPE, into an unnamed run file in
DIRECTORY and return the RUN; NIL when JOB was stopped first."
  (unless (car (sort-job-cancel job))
    (let* ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece)))
           (third (floor (length octets) 3)))
      ;; the piece counts in three steps, read, sorted and written, so that progress moves
      (sb-ext:atomic-incf (car (sort-job-progress job)) third)
      (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
        (multiple-value-bind (key-starts key-ends numbers) (line-keys octets starts ends key)
          (let ((order (prog1 (key-order octets key-starts key-ends numbers key reverse fold)
                         (sb-ext:atomic-incf (car (sort-job-progress job)) third))))
            (with-scratch-file (fd directory)
              (let ((writer (tuatara.io:make-writer fd))
                    (header (tuatara.text:make-octets +record-header+))
                    (line-ends (line-end-codes octets ends nexts)))
                (loop for r across order
                      do (let* ((s (aref starts r)) (e (aref ends r))
                                (number (if numbers (aref numbers r) 0d0)))
                           (flet ((put (at value) (tuatara.text:octets-put-integer header value 4 :at at)))
                             (put 0 r)
                             (put 4 (- (aref key-starts r) s))
                             (put 8 (- (aref key-ends r) s))
                             (put 12 (- e s))
                             (put 16 (ldb (byte 32 0) (sb-kernel:double-float-low-bits number)))
                             (put 20 (ldb (byte 32 0) (sb-kernel:double-float-high-bits number))))
                           (tuatara.io:writer-octets writer header)
                           (tuatara.io:writer-octets writer octets s e)))
                (tuatara.io:writer-flush writer)
                (sb-ext:atomic-incf (car (sort-job-progress job)) (- (length octets) third third))
                (make-sort-run fd (tuatara.io:writer-length writer) (length starts) line-ends)))))))))

(define-record (sort-cursor (:constructor make-sort-cursor (run index sap end)) (:copier nil))
  "Where the merge is in a run: the RUN's INDEX among the runs, its memory SAP
and END, the record at POS, and that record's header decoded."
  (run nil :documentation "The run CURSOR is in.")
  (index 0 :type fixnum :documentation "The index of the run of CURSOR among the runs.")
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer
       :documentation "The memory address of the start of the run of CURSOR.")
  (end 0 :type fixnum :documentation "The memory address of the end of the run of CURSOR.")
  (pos 0 :type fixnum :documentation "The memory address of the record CURSOR is at.")
  (line 0 :type fixnum :documentation "The index in its piece of the line of the record CURSOR is at.")
  (key-start 0 :type fixnum :documentation "The memory address where the key of the record CURSOR is at starts.")
  (key-end 0 :type fixnum :documentation "The memory address where the key of the record CURSOR is at ends.")
  (text 0 :type fixnum :documentation "The memory address of the text of the record CURSOR is at.")
  (length 0 :type fixnum :documentation "The length in octets of the text of the record CURSOR is at.")
  (number 0d0 :type double-float :documentation "The number of the record CURSOR is at, as a double float."))

(setf (documentation 'make-sort-cursor 'function)
      "A cursor at the start of the run RUN, the INDEXth, mapped at SAP up to END.")

(defun sort-cursor-read (cursor)
  "Decode the header of the record CURSOR is at; NIL at the end of its run."
  (let ((sap (sort-cursor-sap cursor)) (pos (sort-cursor-pos cursor)))
    (when (< pos (sort-cursor-end cursor))
      (let ((low (sb-sys:sap-ref-32 sap (+ pos 16))) (high (sb-sys:sap-ref-32 sap (+ pos 20))))
        (setf (sort-cursor-line cursor) (sb-sys:sap-ref-32 sap pos)
              (sort-cursor-text cursor) (+ pos +record-header+)
              (sort-cursor-key-start cursor) (+ pos +record-header+ (sb-sys:sap-ref-32 sap (+ pos 4)))
              (sort-cursor-key-end cursor) (+ pos +record-header+ (sb-sys:sap-ref-32 sap (+ pos 8)))
              (sort-cursor-length cursor) (sb-sys:sap-ref-32 sap (+ pos 12))
              (sort-cursor-number cursor)
              (sb-kernel:make-double-float (if (>= high #x80000000) (- high #x100000000) high) low)))
      t)))

(defun sort-cursor-advance (cursor)
  "Move CURSOR to its run's next record; NIL at the end."
  (setf (sort-cursor-pos cursor) (+ (sort-cursor-text cursor) (sort-cursor-length cursor)))
  (sort-cursor-read cursor))

(defun sort-cursor-before-p (a b numeric reverse fold)
  "True when the record under cursor A comes before the one under B in the
merge: by key, descending with REVERSE, and between equal keys by run, the
later run first with REVERSE so that ties reverse as the pieces' did."
  (let ((c (if numeric
               (let ((x (sort-cursor-number a)) (y (sort-cursor-number b))) (cond ((< x y) -1) ((> x y) 1) (t 0)))
               (sap-compare (sort-cursor-sap a) (sort-cursor-key-start a) (sort-cursor-key-end a)
                            (sort-cursor-sap b) (sort-cursor-key-start b) (sort-cursor-key-end b) fold))))
    (cond ((/= c 0) (if reverse (> c 0) (< c 0)))
          (reverse (> (sort-cursor-index a) (sort-cursor-index b)))
          (t (< (sort-cursor-index a) (sort-cursor-index b))))))

(defun merge-runs (job runs function numeric reverse fold)
  "Merge RUNS, calling FUNCTION with the cursor of each record in merged order
(its record is under the cursor only during the call); stops when JOB is
stopped. The runs' files are mapped for the merge and let go after."
  (let ((heap (make-array (length runs))) (size 0))
    (flet ((before (i j) (sort-cursor-before-p (aref heap i) (aref heap j) numeric reverse fold))
           (swap (i j) (rotatef (aref heap i) (aref heap j))))
      (labels ((up (i)
                 (loop while (and (plusp i) (before i (floor (1- i) 2)))
                       do (swap i (floor (1- i) 2)) (setf i (floor (1- i) 2))))
               (down (i)
                 (loop (let* ((l (1+ (* 2 i))) (r (1+ l)) (m i))
                         (when (and (< l size) (before l m)) (setf m l))
                         (when (and (< r size) (before r m)) (setf m r))
                         (when (= m i) (return))
                         (swap i m) (setf i m)))))
        (unwind-protect
             (progn
               (loop for run in runs for index from 0
                     when (plusp (sort-run-length run))
                       do (let* ((mapping (tuatara.text:map-fd (sort-run-fd run) (sort-run-length run) "a sort run"))
                                 (cursor (make-sort-cursor run index (tuatara.text:mapping-sap mapping)
                                                           (sort-run-length run))))
                            (setf (sort-run-mapping run) mapping (sort-run-fd run) nil)
                            (when (sort-cursor-read cursor)
                              (setf (aref heap size) cursor)
                              (incf size)
                              (up (1- size)))))
               (let ((count 0))
                 (loop while (plusp size)
                       do (let ((cursor (aref heap 0)))
                            (when (and (zerop (mod (incf count) 65536)) (car (sort-job-cancel job))) (return))
                            (funcall function cursor)
                            (if (sort-cursor-advance cursor)
                                (down 0)
                                (progn (setf (aref heap 0) (aref heap (1- size))) (decf size) (down 0)))))))
          (dolist (run runs)
            (when (sort-run-mapping run) (tuatara.text:close-mapping (sort-run-mapping run)))
            (when (sort-run-fd run)
              (tuatara.runtime:close-fd-quietly (sort-run-fd run) "a file of sorted lines"))))))))

(defun line-end-code (octets end next)
  "The code of the end of a line of OCTETS whose text ends at END, the next
line starting at NEXT (SCAN-LINES), for WRITE-KEPT-LINE-END: 0 for none, 1
for a newline, 2 for a carriage return and a newline, 3 for a carriage
return alone."
  (declare (type tuatara.text:octets octets) (type fixnum end next))
  (case (- next end)
    (0 0)
    (1 (if (= 13 (aref octets end)) 3 1))
    (t 2)))

(defun line-end-codes (octets ends nexts)
  "The LINE-END-CODE of each line of OCTETS, whose texts end at ENDS and the
lines after start at NEXTS (SCAN-LINES), as a vector of 2-bit codes."
  (let ((codes (make-array (length ends) :element-type '(unsigned-byte 2))))
    (dotimes (i (length ends) codes)
      (setf (aref codes i) (line-end-code octets (aref ends i) (aref nexts i))))))

(defun write-kept-line-end (writer code crlf)
  "Write to WRITER the end a line had, by CODE (LINE-END-CODE): none for 0, a
newline for 1, for 2 a carriage return and a newline when carriage return and
line feed (CRLF) is true, else a newline, and a carriage return for 3."
  (let ((newline (if crlf
                     (load-time-value (coerce #(13 10) 'tuatara.text:octets) t)
                     (load-time-value (coerce #(10) 'tuatara.text:octets) t))))
    (case code
      (0 nil)
      (1 (tuatara.io:writer-octets writer newline (1- (length newline)) (length newline)))
      (3 (tuatara.io:writer-octets writer (load-time-value (coerce #(13) 'tuatara.text:octets) t) 0 1))
      (t (tuatara.io:writer-octets writer newline 0 (length newline))))))

(defun sort-workers (&optional (most 8))
  "How many of the pool's workers a job of the sort commands takes: all the
processors but two, at least one, and no more than MOST unless it is NIL."
  (let ((all (max 1 (- (tuatara.runtime:cpu-count) 2))))
    (if most (min most all) all)))

(defun sort-runs (job rope pieces key reverse crlf fold directory)
  "Sort each of PIECES of ROPE into a run in DIRECTORY (SORT-PIECE), the
pool's workers taking a piece each: the runs, a list, or NIL with every run
discarded when JOB was stopped or a piece could not be sorted."
  (let ((runs (coerce (tuatara.runtime:run-tasks
                       (map 'simple-vector
                            (lambda (piece) (lambda () (sort-piece job rope piece key reverse crlf fold directory)))
                            pieces)
                       :parallelism (sort-workers) :discard #'discard-run)
                      'list)))
    (if (or (car (sort-job-cancel job)) (some #'null runs))
        (progn (dolist (run runs) (when run (discard-run run))) nil)
        runs)))

(defun sort-on-disk (job rope start end key reverse crlf fold directory)
  "Sort the lines of ROPE from START below END as the pool's workers can, a
piece at a time into run files, then merge them into an unnamed file: the rope
over it, and how many lines there are. NIL when JOB was stopped."
  (let ((runs (sort-runs job rope
                         (tuatara.text:rope-line-pieces rope start end *sort-piece* :lines (piece-lines (sort-workers)))
                         key reverse crlf fold directory)))
    (when runs
      (with-scratch-file (fd directory)
        (let* ((writer (tuatara.io:make-writer fd))
               ;; the line ends stay where they were: place I takes the end of line I
               (ends (let ((all (make-array (reduce #'+ runs :key #'sort-run-lines) :element-type '(unsigned-byte 2)))
                           (at 0))
                       (dolist (run runs all)
                         (replace all (sort-run-ends run) :start1 at)
                         (incf at (sort-run-lines run)))))
               (slot 0))
          (merge-runs job runs
                      (lambda (cursor)
                        (tuatara.io:writer-sap writer
                                               (sort-cursor-sap cursor)
                                               (sort-cursor-text cursor)
                                               (+ (sort-cursor-text cursor) (sort-cursor-length cursor)))
                        (write-kept-line-end writer (aref ends slot) crlf)
                        (sb-ext:atomic-incf (car (sort-job-progress job)) (+ 1 (sort-cursor-length cursor)))
                        (incf slot))
                      (eq (first key) :numeric) reverse fold)
          (tuatara.io:writer-flush writer)
          (unless (car (sort-job-cancel job))
            (values (file-rope fd (tuatara.io:writer-length writer)) (length ends))))))))

(defun keep-piece (job rope piece crlf files marks-for)
  "Write the lines of PIECE, (START END) of ROPE, that MARKS-FOR (a function of
the piece's octets, starts and ends, returning a bit vector of the lines to
delete and how many go, or NIL for that count) leaves, at the end of this
thread's file among FILES (PIECE-WRITER). Returns the writer, where in its file
the piece's lines begin and end, the lines deleted and the piece's lines; NIL
when JOB was stopped. The piece counts twice in the job's progress: read and
marked, then written."
  (unless (car (sort-job-cancel job))
    (let ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece))))
      (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
        (multiple-value-bind (marks count) (funcall marks-for octets starts ends)
          (sb-ext:atomic-incf (car (sort-job-progress job)) (length octets))
          (let* ((writer (piece-writer files))
                 (from (tuatara.io:writer-length writer)))
            (each-kept-line octets starts nexts marks
                            (lambda (octets start end) (tuatara.io:writer-octets writer octets start end)))
            (sb-ext:atomic-incf (car (sort-job-progress job)) (length octets))
            (list writer from (tuatara.io:writer-length writer) count (length starts))))))))

(defun keep-pieces (job rope pieces crlf directory marks-for-piece)
  "Write the lines of each of PIECES of ROPE that the function
MARKS-FOR-PIECE gives for its index leaves (KEEP-PIECE), the pool's workers
taking a piece each, into a file a worker in DIRECTORY (PIECE-FILES).
Returns the rope of what is kept and how many lines were deleted (NIL when
the marks came without their count); NIL when JOB was stopped."
  (let ((files (make-piece-files directory)))
    (unwind-protect
         (let* ((index -1)
                (outputs (coerce (tuatara.runtime:run-tasks
                                  (map 'simple-vector
                                       (lambda (piece)
                                         (let ((marks-for (funcall marks-for-piece (incf index))))
                                           (lambda () (keep-piece job rope piece crlf files marks-for))))
                                       pieces)
                                  :parallelism (sort-workers nil))
                                 'list)))
           (unless (or (car (sort-job-cancel job)) (some #'null outputs))
             (values (apply #'tuatara.text:splice-rope (tuatara.text:make-rope) 0 0 (piece-files-ropes files outputs))
                     (and (every #'fourth outputs) (reduce #'+ outputs :key #'fourth)))))
      ;; the files not mapped by then: stopped, or failed
      (discard-piece-files files))))

(defun dedupe-on-disk (job rope start end reverse keep-blanks crlf directory)
  "Delete all but the first (the last with REVERSE) of identical lines of ROPE
from START below END: the lines are sorted a piece at a time into runs, merged
to find the lines that repeat, and the region written again without them.
Returns the rope and how many lines were deleted; NIL when JOB was stopped.
The region counts four times in the job's progress: sorted, merged, and twice
as it is written again (KEEP-PIECE)."
  (setf (sort-job-total job) (* 4 (max 1 (- end start))))
  (let* ((pieces (tuatara.text:rope-line-pieces rope start end *sort-piece* :lines (piece-lines (sort-workers))))
         (runs (sort-runs job rope pieces (list :line) nil crlf nil directory)))
    (unless runs (return-from dedupe-on-disk nil))
    (let* ((firsts (let ((at 0)) (map 'vector (lambda (run) (prog1 at (incf at (sort-run-lines run)))) runs)))
           (marks (make-array (reduce #'+ runs :key #'sort-run-lines) :element-type 'bit :initial-element 0))
           (count 0)
           ;; the line kept last: its run's index, where its text starts and ends there, its number
           (saps (make-array (length runs) :initial-element nil))
           (previous -1) (previous-start 0) (previous-end 0) (previous-line 0))
      (declare (type fixnum previous previous-start previous-end previous-line))
      ;; equal lines come together, in their first order: all but the first (or last) are marked
      (merge-runs job runs
                  (lambda (cursor)
                    (let* ((index (sort-cursor-index cursor))
                           (line (+ (aref firsts index) (sort-cursor-line cursor)))
                           (text (sort-cursor-text cursor))
                           (length (sort-cursor-length cursor)))
                      (flet ((keep ()
                               (setf previous index previous-start text previous-end (+ text length)
                                     previous-line line)))
                        (unless (svref saps index) (setf (svref saps index) (sort-cursor-sap cursor)))
                        (unless (and keep-blanks (zerop length))
                          (cond ((and (>= previous 0)
                                      (zerop (sap-compare (sort-cursor-sap cursor) text (+ text length)
                                                          (svref saps previous) previous-start previous-end nil)))
                                 (incf count)
                                 (cond (reverse (setf (sbit marks previous-line) 1) (keep))
                                       (t (setf (sbit marks line) 1))))
                                (t (keep)))))
                      (sb-ext:atomic-incf (car (sort-job-progress job)) (1+ length))))
                  nil nil nil)
      (when (car (sort-job-cancel job)) (return-from dedupe-on-disk nil))
      ;; write the region again without the marked lines, a piece at a time
      (let ((firsts (coerce firsts 'simple-vector)))
        (multiple-value-bind (kept gone)
            (keep-pieces job rope pieces crlf directory
                         (lambda (index)
                           (let ((first (svref firsts index)))
                             (lambda (octets starts ends)
                               (declare (ignore octets ends))
                               (let ((local (make-array (length starts) :element-type 'bit)))
                                 (dotimes (i (length starts) local)
                                   (setf (sbit local i) (sbit marks (+ first i)))))))))
          (declare (ignore gone))
          (and kept (values kept count)))))))

(defun reverse-on-disk (job rope start end crlf directory)
  "Reverse the order of the lines of ROPE from START below END into an unnamed
file, the pieces from the last, each piece's lines from its last, the line ends
staying where they were. The rope over it and how many lines there are; NIL
when JOB was stopped."
  (let* ((pieces (tuatara.text:rope-line-pieces rope start end *sort-piece* :lines (piece-lines)))
         ;; the line ends in order, first: place I keeps the end of line I
         (ends (let ((all '()))
                 (dolist (piece pieces (apply #'concatenate '(vector (unsigned-byte 2)) (nreverse all)))
                   (let ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece))))
                     (multiple-value-bind (starts line-ends nexts) (scan-lines octets crlf)
                       (declare (ignore starts))
                       (push (line-end-codes octets line-ends nexts) all))))))
         (slot 0))
    (with-scratch-file (fd directory)
      (let ((writer (tuatara.io:make-writer fd)))
        (dolist (piece (reverse pieces))
          (when (car (sort-job-cancel job)) (return-from reverse-on-disk nil))
          (let ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece))))
            (multiple-value-bind (starts line-ends) (scan-lines octets crlf)
              (loop for i from (1- (length starts)) downto 0
                    do (tuatara.io:writer-octets writer octets (aref starts i) (aref line-ends i))
                       (write-kept-line-end writer (aref ends slot) crlf)
                       (incf slot)))
            (sb-ext:atomic-incf (car (sort-job-progress job)) (* 2 (length octets)))))
        (tuatara.io:writer-flush writer)
        (values (file-rope fd (tuatara.io:writer-length writer)) (length ends))))))

;;; Jobs

(defun sort-progress (job)
  "How much of JOB is done, from 0 to 99 while it runs."
  (tuatara.runtime:running-percent (car (sort-job-progress job)) (sort-job-total job)))

(tuatara.runtime:define-job-words sort-job
  :doing (sort-job-words job)
  :description (or (sort-job-what job) (sort-job-words job))
  :verb (let ((words (sort-job-words job)))
          (cond ((string= words "Reversing") "reverse the lines")
                ((string= words "Deleting duplicates") "delete the duplicate lines")
                (t "sort"))))

(defmethod tuatara.runtime:job-percent ((job sort-job))
  "A region of *SORT-IN-MEMORY-LIMIT* octets or more has its result's lines
counted once written: sorting is the first half, counting the second."
  (tuatara.runtime:two-phase-percent (sort-progress job) (sort-job-counting job)
                                     :halves (>= (floor (sort-job-total job) 2) *sort-in-memory-limit*)))

(defmethod tuatara.runtime:job-state-words ((job sort-job))
  "What JOB is doing, reading or sorting, and how far it has gone, in words."
  (tuatara.runtime:two-phase-words (sort-job-words job) (sort-progress job) (sort-job-counting job)))

(defmethod tuatara.runtime:job-guarded-spans ((job sort-job))
  "The region a sort or filter works on, until an edit has touched it."
  (unless (sort-job-changed job)
    (list (cons (sort-job-region-start job) (sort-job-region-end job)))))

(defun sort-note-batch (job batch)
  "Change function while JOB runs: follow the region through BATCH, and note
when BATCH touches it (TUATARA.RUNTIME:SPAN-TOUCHED-P), that is an insertion at
its start or inside it, a deletion or replacement overlapping it, the whole text
swapped."
  (unless (sort-job-changed job)
    (let ((region (tuatara.buffer:span-after-batch (cons (sort-job-region-start job) (sort-job-region-end job)) batch)))
      (if region
          (setf (sort-job-region-start job) (car region) (sort-job-region-end job) (cdr region))
          (setf (sort-job-changed job) t)))))

(defun lines-done-words (words buffer start end)
  "WORDS (\"Sorting\") followed by what they are done to: the whole buffer
when START and END take in all of BUFFER, else the selection's lines."
  (format nil "~a ~:[the selection's lines~;the whole buffer~]" words
          (and (zerop start) (= end (tuatara.buffer:buffer-size buffer)))))

(defun start-sort (buffer start end words work)
  "Run WORK, a function of the job, BUFFER's rope, START and END returning the
octets or rope to replace START below END and what to say then, as a job of
BUFFER (TUATARA.RUNTIME:START-JOB-THREAD), with *SORT-CARRIED-SETTINGS* bound
there as they are here. Waits up to *SORT-WAIT* seconds and takes it in if it is
done by then, else leaves it running with its progress in the mode line, the
editor woken when it ends. Returns the job, or :QUEUED when another job runs or
waits in BUFFER, or its lines are still being counted: it then waits its turn,
START and END followed meanwhile."
  (let ((what (lines-done-words words buffer start end)))
    (when (queue-if-busy buffer what (list (cons start :before) (cons end :after))
                         (lambda (start end) (start-sort buffer start end words work))
                         :counted t)
      (return-from start-sort :queued))
    (tuatara.buffer:ensure-indexed buffer)
    (multiple-value-bind (rope base-start base-end since) (job-begins-where-given buffer start end :untouched t)
      (let ((job (%make-sort-job :buffer buffer :words words :what what :region-start start :region-end end
                                 :base rope :base-start base-start :base-end base-end :since since
                                 :total (* 2 (max 1 (- end start))))))
        (let ((function (lambda (b batch) (declare (ignore b)) (sort-note-batch job batch))))
          (setf (sort-job-change-function job) function)
          (push function (tuatara.buffer:buffer-change-functions buffer)))
        (tuatara.runtime:start-job-thread
         job (tuatara.runtime:carry-settings
              *sort-carried-settings*
              (lambda (job)
                (multiple-value-bind (result message) (funcall work job rope base-start base-end)
                  ;; a result written to files has its lines counted here,
                  ;; all of it at once on nine cores in ten, its progress shown
                  (when (and (typep result 'tuatara.text:rope) (not (tuatara.text:rope-indexed-p result))
                             (not (car (sort-job-cancel job))))
                    (let ((counting (cons 0 0)))
                      (setf (sort-job-counting job) counting
                            result (tuatara.text:index-rope result :progress counting :cancel (sort-job-cancel job)))))
                  ;; a result made in memory is compared with the text here, not on the editor's thread
                  (when (vectorp result)
                    (let ((before (tuatara.text:rope-octets rope :start base-start :end base-end)))
                      (setf (sort-job-same job) (and (= (length before) (length result)) (not (mismatch before result)))
                            result (tuatara.text:make-rope result))))
                  (if (or (null result) (car (sort-job-cancel job)))
                      (setf (sort-job-status job) :stopped)
                      (setf (sort-job-result job) result (sort-job-message job) message
                            (sort-job-status job) :done)))))
         :name "tuatara sort" :details (format nil "octets ~:d to ~:d" start end))
        (take-in-soon job *sort-wait* words)
        job))))

(defmethod tuatara.runtime:job-take-in ((job sort-job))
  "Put JOB's result in its buffer as one undoable step, on the editor's thread,
unless the region changed meanwhile or the buffer may no longer take it
\(RESULT-REFUSED-P); or say why there is none."
  (let ((buffer (sort-job-buffer job)))
    (setf (tuatara.buffer:buffer-change-functions buffer)
          (remove (sort-job-change-function job) (tuatara.buffer:buffer-change-functions buffer)))
    (when (live-buffer-p buffer)
      (ecase (sort-job-status job)
        ((:stopped :failed) (message "~a" (tuatara.runtime:job-ending-words job)))
        (:done
         (cond
           ((sort-job-changed job)
            (message "The lines changed while ~(~a~) them; nothing was changed" (sort-job-words job)))
           ((result-refused-p buffer (format nil "the result of ~(~a~)" (sort-job-words job))
                              (- (tuatara.text:rope-octet-length (sort-job-result job))
                                 (- (sort-job-region-end job) (sort-job-region-start job)))))
           (t
             (let* ((start (sort-job-region-start job)) (end (sort-job-region-end job))
                    (result (sort-job-result job))
                    (point (tuatara.buffer:buffer-point buffer)))
               (tuatara.display:call-keeping-window-lines
                (buffer-windows buffer) start end
                (lambda ()
                  (let ((same (sort-job-same job)))
                    (flet ((spliced (rope from to)
                             ;; ROPE with the region FROM below TO in it sorted, and maps to and from it
                             (let ((delta (- (tuatara.text:rope-octet-length result) (- to from))))
                               (values (tuatara.text:splice-rope rope from to result)
                                       (lambda (p bias)
                                         (declare (ignore bias))
                                         (cond ((< p from) p) ((>= p to) (+ p delta)) (t from)))
                                       (lambda (p bias)
                                         (declare (ignore bias))
                                         (cond ((< p from) p) ((>= p (+ to delta)) (- p delta)) (t from)))))))
                      (unless same
                        ;; standing where the job began, before the changes made meanwhile elsewhere
                        (multiple-value-bind (final final-forward final-backward)
                            (spliced (tuatara.buffer:buffer-text buffer) start end)
                          (multiple-value-bind (done forward backward)
                              (spliced (sort-job-base job) (sort-job-base-start job) (sort-job-base-end job))
                            (tuatara.buffer:buffer-take-in-job-result
                             buffer final final-forward final-backward
                             :since (sort-job-since job)
                             :begun (tuatara.text:make-swap (sort-job-base job) done forward backward)))))))))
               (when (<= start point end)
                 (setf (tuatara.buffer:buffer-point buffer) start))
               (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
               (message "~a" (sort-job-message job))))))))))

;;; The commands

(defun sort-lines-by (buffer start end key reverse words done)
  "Sort BUFFER's lines from START below END by KEY (see LINE-KEYS), in reverse
with REVERSE, in memory or on disk by size; DONE is the past tense for the
message (\"Sorted\")."
  (let ((crlf (buffer-crlf-p buffer)) (fold *sort-fold-case*)
        (directory (tuatara.io:buffer-scratch-directory buffer)))
    (start-sort buffer start end words
                (lambda (job rope start end)
                  (if (lines-in-memory-p rope start end)
                      (let ((octets (tuatara.text:rope-octets rope :start start :end end)))
                        (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
                          (multiple-value-bind (key-starts key-ends numbers) (line-keys octets starts ends key)
                            (values (arrange-lines octets starts ends nexts
                                                   (key-order octets key-starts key-ends numbers key reverse fold))
                                    (format nil "~a ~:d line~:p" done (length starts))))))
                      (multiple-value-bind (rope lines)
                          (if (eq (first key) :index)
                              (reverse-on-disk job rope start end crlf directory)
                              (sort-on-disk job rope start end key reverse crlf fold directory))
                        (and rope (values rope (format nil "~a ~:d line~:p" done lines)))))))))

(defun field-argument ()
  "The field the prefix argument names for sort-fields: the first by default."
  (let ((arg (prefix-argument)))
    (if (or (null arg) (not (integerp arg)) (zerop arg)) 1 arg)))

(define-command sort-lines ()
  "Sort the lines the selection touches by their text: by the codes of their
characters, the case of ASCII letters ignored when *SORT-FOLD-CASE* is true.
With a prefix argument, in descending order. Lines that compare equal keep
their order (reversed with the rest in descending order). Without a selection,
does nothing and says so; a large selection is sorted in the background."
  (with-selected-lines (b start end 'sort-lines)
    (sort-lines-by b start end (list :line) (sort-descending-p) "Sorting" "Sorted")))

(define-command reverse-region ()
  "Reverse the order of the lines the selection touches. Without a selection,
does nothing and says so."
  (with-selected-lines (b start end 'reverse-region)
    (sort-lines-by b start end (list :index) t "Reversing" "Reversed")))

(defun record-field-order (octets ends crlf each field numeric reverse fold)
  "The records of OCTETS, whole records ending as ENDS says (RECORD-ENDS), and
their order by FIELD (EACH: a RECORD-FIELDS-EACH; NIL for the whole record),
as text, or with NUMERIC by the number its value begins with (FIELD-NUMBER),
a record without the field taking an empty value: two values, the records as
\(START END) and the order, as KEY-ORDER gives it."
  (let* ((rope (tuatara.text:octets->rope octets))
         (size (length octets))
         (holder (list (make-string 256)))
         (keys (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (records '()) (starts '()) (finishes '()) (numbers '()))
    (loop with at = 0
          while (< at size)
          do (multiple-value-bind (text-end after) (record-bounds rope at ends)
               (let* ((length (decode-into octets at (record-text-end rope text-end crlf) holder))
                      (value (if field
                                 (or (funcall each (car holder) length field
                                              (lambda (string from to) (subseq string from to)))
                                     "")
                                 (subseq (car holder) 0 length)))
                      (key (tuatara.text:string-octets value)))
                 (push (list at after) records)
                 (push (fill-pointer keys) starts)
                 (loop for octet across key do (vector-push-extend octet keys))
                 (push (fill-pointer keys) finishes)
                 (when numeric (push (field-number key 0 (length key)) numbers))
                 (setf at after))))
    (let ((key-octets (coerce keys 'tuatara.text:octets)))
      (values (nreverse records)
              (key-order key-octets
                         (coerce (nreverse starts) '(simple-array fixnum (*)))
                         (coerce (nreverse finishes) '(simple-array fixnum (*)))
                         (and numeric (coerce (nreverse numbers) '(simple-array double-float (*))))
                         (list :field) reverse fold)))))

(defun sort-records-by-field (buffer start end command numeric descending)
  "Sort the records of BUFFER, a file of records, that the lines from START
below END touch, its header aside, by a field asked for (READ-RECORD-FIELD), as
text, or with NUMERIC by number, in descending order when DESCENDING: each
record whole, though it holds line breaks (RECORD-ENDS). Worked in memory
\(SORT-IN-MEMORY-ONLY). COMMAND names the command in messages."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (ends (record-ends buffer))
         (crlf (buffer-crlf-p buffer))
         (fields (mode-record-fields (buffer-major-mode buffer) buffer))
         (field (and fields (read-record-field buffer fields "Sort by the column: ")))
         (each (and fields (record-fields-each fields)))
         (reverse descending) (fold *sort-fold-case*)
         (newline (tuatara.text:string-octets (tuatara.buffer:buffer-newline buffer)))
         (memo (make-record-memo))
         (first (if (list-header-p buffer) (records-start rope 1 ends) 0))
         ;; the whole records the lines touch
         (low (max first
                   (line-record memo rope start (min (tuatara.text:rope-octet-length rope)
                                                     (1+ (tuatara.text:rope-line-end rope start))) ends)))
         (high (max low (nth-value 2 (line-record memo rope (tuatara.text:rope-line-start rope (max start (1- end)))
                                                  end ends)))))
    (if (>= low high)
        (say-not-run (sort-command-name command) "No records to sort but the header")
        (sort-in-memory-only buffer low high command "Sorting"
                             (lambda (octets)
                               (multiple-value-bind (records order)
                                   (record-field-order octets ends crlf each field numeric reverse fold)
                                 (values (arrange-records octets records order newline)
                                         (format nil "Sorted ~:d record~:p" (length records)))))))))

(define-command sort-fields ()
  "Sort the lines the selection touches by one field of each, as text: fields
are separated by spaces and tabs, and the prefix argument names the field (the
first by default; a negative one counts from the end of the line). A line with
too few fields stops the sort. In a file of records, a CSV, a log, JSON Lines
and the others \\[search-filter-show-records] names, the records are sorted
instead by the column asked for, as \\[search-filter-show-records] asks for it,
each record whole and the header line staying first; a record without the column
sorts as empty. Without a selection, does nothing and says so."
  (let ((field (field-argument)))
    (with-selected-lines (b start end 'sort-fields)
      (if (records-file-p b)
          (sort-records-by-field b start end 'sort-fields nil *sort-descending*)
          (sort-lines-by b start end (list :field field) *sort-descending* "Sorting" "Sorted")))))

(define-command sort-numeric-fields ()
  "Sort the lines the selection touches by the number at the start of one
field of each (the prefix argument's, as for sort-fields): 0x and hexadecimal
digits, or a decimal number, with a fraction and exponent too; a field
without a number counts as 0. A leading zero does not make a number octal. In a file of records, the column is asked for
and the records sorted whole, as sort-fields sorts them. Without a
selection, does nothing and says so."
  (let ((field (field-argument)))
    (with-selected-lines (b start end 'sort-numeric-fields)
      (if (records-file-p b)
          (sort-records-by-field b start end 'sort-numeric-fields t *sort-descending*)
          (sort-lines-by b start end (list :numeric field) *sort-descending* "Sorting" "Sorted")))))

(define-command sort-columns ()
  "Sort the lines from the one where the selection starts to the one where it
ends by the text between the columns of its two ends, as text; with a prefix
argument, in descending order. A tab counts to the next tab stop (every
*tab-width* columns, or as set for the buffer's mode), a wide character two
columns. Without a selection, does nothing and says so."
  (with-selected-lines (b start end 'sort-columns :columns t)
    (multiple-value-bind (a z) (tuatara.buffer:buffer-region b)
      (let ((c1 (tuatara.display:buffer-screen-column b a))
            (c2 (tuatara.display:buffer-screen-column b z)))
        (sort-lines-by b start end (list :columns (min c1 c2) (max c1 c2) (buffer-tab-width b))
                       (sort-descending-p) "Sorting" "Sorted")))))

(defun sort-in-memory-only (buffer start end command words work)
  "Run WORK, a function of the region's octets giving the octets that replace
it and a message, as a sort job of BUFFER from START below END, when the region
is small enough to handle in memory (LINES-IN-MEMORY-P); else say that COMMAND
is not done on so much yet."
  (if (not (lines-in-memory-p (tuatara.buffer:buffer-text buffer) start end))
      (message "~(~a~) works in memory, up to ~:d MB and ~:d lines; the selection is ~:d MB of ~:d lines"
               (sort-command-name command)
               (floor *sort-in-memory-limit* (* 1024 1024)) (piece-lines)
               (floor (- end start) (* 1024 1024))
               (tuatara.text:rope-count-newlines (tuatara.buffer:buffer-text buffer) start end))
      (start-sort buffer start end words
                  (lambda (job rope start end)
                    (declare (ignore job))
                    (funcall work (tuatara.text:rope-octets rope :start start :end end))))))

(define-command sort-paragraphs ()
  "Sort the paragraphs the selection touches by their text: paragraphs are
separated by blank lines, which stay where they were. With a prefix argument, in
descending order. Without a selection, does nothing and says so."
  (let ((reverse (sort-descending-p)) (fold *sort-fold-case*))
    (with-selected-lines (b start end 'sort-paragraphs)
      (let ((newline (tuatara.text:string-octets (tuatara.buffer:buffer-newline b))) (crlf (buffer-crlf-p b)))
        (sort-in-memory-only b start end 'sort-paragraphs "Sorting"
                             (lambda (octets)
                               (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
                                 (let ((records (paragraph-records octets starts ends nexts)))
                                   (values (arrange-records octets records (record-order octets records reverse fold)
                                                            newline)
                                           (format nil "Sorted ~:d paragraph~:p" (length records)))))))))))

(define-command sort-pages ()
  "Sort the pages the selection touches by their text, pages ending with a form
feed at the start of a line. With a prefix argument, in descending order.
Without a selection, does nothing and says so."
  (let ((reverse (sort-descending-p)) (fold *sort-fold-case*))
    (with-selected-lines (b start end 'sort-pages)
      (sort-in-memory-only b start end 'sort-pages "Sorting"
                           (lambda (octets)
                             (let ((records (page-records octets)))
                               (values (arrange-records octets records (record-order octets records reverse fold) nil)
                                       (format nil "Sorted ~:d page~:p" (length records)))))))))

(define-command sort-regexp-fields ()
  "Sort the matches of a regular expression within the selection: asks for the
pattern of the records, then for their key, either \\& for the whole record, \\1
to \\9 for a group of the record's pattern, or a pattern whose first match in
the record is its key (an empty key when there is none). Keys compare as text;
with a prefix argument, in descending order. The text between the records stays
where it was. Without a selection, does nothing and says so."
  (let ((b (current-buffer)) (reverse (sort-descending-p)) (fold *sort-fold-case*))
    (multiple-value-bind (start end)
        (selected-region-or-say b (sort-command-name 'sort-regexp-fields) :change t)
      (when start
        (let* ((record (multiple-value-call #'pattern-regex (read-pattern "sort the records")))
               (key (multiple-value-bind (text regex) (read-pattern "sort them by the key" :initial "\\&")
                      ;; \& and \1 to \9 name the record or its group, whichever the kind
                      (if (cl-ppcre:scan "^\\\\[&1-9]$" text) text (pattern-regex text regex)))))
          (if (zerop (length record))
              (say-not-run (sort-command-name 'sort-regexp-fields) "No pattern")
              (sort-in-memory-only b start end 'sort-regexp-fields "Sorting"
                                   (lambda (octets)
                                     (multiple-value-bind (text count)
                                         (sort-regexp-text (tuatara.text:octets-string octets)
                                                           record key reverse fold)
                                       (values (tuatara.text:string-octets text)
                                               (format nil "Sorted ~:d record~:p" count)))))))))))

(defmacro define-reverse-sort (name command doc)
  "Define NAME, a command running COMMAND in descending order, with DOC."
  `(define-command ,name ()
     ,doc
     (let ((*sort-descending* t) (*sort-command* ',name))
       (,command))))

(define-reverse-sort sort-lines-reverse sort-lines
  "Sort the lines the selection touches by their text in descending order, as
\\[sort-lines] does with a prefix argument: lines that compare equal come in
reverse order too. Without a selection, does nothing and says so.")

(define-reverse-sort sort-fields-reverse sort-fields
  "Sort the lines the selection touches by one field of each, as text, in
descending order: as \\[sort-fields], the prefix argument naming the field (a
negative one counting from the end of the line).")

(define-reverse-sort sort-numeric-fields-reverse sort-numeric-fields
  "Sort the lines the selection touches by the number at the start of one
field of each, largest first: as \\[sort-numeric-fields], the prefix argument
naming the field.")

(define-reverse-sort sort-columns-reverse sort-columns
  "Sort the lines from the one where the selection starts to the one where it
ends by the text between the columns of its two ends, in descending order: as
\\[sort-columns] does with a prefix argument.")

(define-reverse-sort sort-paragraphs-reverse sort-paragraphs
  "Sort the paragraphs the selection touches by their text in descending
order: as \\[sort-paragraphs] does with a prefix argument.")

(define-reverse-sort sort-pages-reverse sort-pages
  "Sort the pages the selection touches by their text in descending order: as
\\[sort-pages] does with a prefix argument.")

(define-reverse-sort sort-regexp-fields-reverse sort-regexp-fields
  "Sort the matches of a regular expression within the selection by their
keys in descending order: as \\[sort-regexp-fields] does with a prefix
argument, asking for the records' pattern and their key.")

(define-command delete-duplicate-lines ()
  "Delete all but one of each set of identical lines the selection touches:
the first of them is kept, with \\[universal-argument] the last; with
\\[universal-argument] \\[universal-argument] only a line identical to the one
before it goes, which is quicker for sorted text; with
\\[universal-argument] \\[universal-argument] \\[universal-argument] blank lines
are all kept. Says how many were deleted. Without a selection, does nothing and
says so; a large selection is handled in the background."
  (let* ((arg (prefix-argument))
         (reverse (eql arg 4)) (adjacent (eql arg 16)) (keep-blanks (eql arg 64)))
    (with-selected-lines (b start end 'delete-duplicate-lines)
      (let ((crlf (buffer-crlf-p b))
            (directory (tuatara.io:buffer-scratch-directory b)))
        (flet ((report (count)
                 (format nil "Deleted ~:d ~:[~;adjacent ~]duplicate line~p~:[~; backward~]"
                         count adjacent count reverse)))
          (start-sort b start end "Deleting duplicates"
                      (lambda (job rope start end)
                        (cond ((lines-in-memory-p rope start end)
                               (let ((octets (tuatara.text:rope-octets rope :start start :end end)))
                                 (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
                                   (multiple-value-bind (marks count)
                                       (duplicate-marks octets starts ends reverse adjacent keep-blanks)
                                     (values (kept-lines octets starts nexts marks) (report count))))))
                              (adjacent
                               ;; lines repeat only next to each other: each piece alone, and the
                               ;; first line of a piece against the last of the one before
                               (let ((pieces (tuatara.text:rope-line-pieces rope start end *filter-piece*
                                                                           :lines (piece-lines (sort-workers nil)))))
                                 (multiple-value-bind (kept count)
                                     (keep-pieces job rope pieces crlf directory
                                                  (lambda (index)
                                                    (let ((before (and (plusp index)
                                                                       (rope-line-before rope (first (elt pieces index))
                                                                                         crlf))))
                                                      (lambda (octets starts ends)
                                                        (multiple-value-bind (marks count)
                                                            (duplicate-marks octets starts ends nil t keep-blanks)
                                                          (when (and before (plusp (length starts))
                                                                     (not (and keep-blanks
                                                                               (= (aref starts 0) (aref ends 0))))
                                                                     (equalp before (subseq octets (aref starts 0)
                                                                                            (aref ends 0))))
                                                            (setf (sbit marks 0) 1)
                                                            (incf count))
                                                          (values marks count))))))
                                   (and kept (values kept (report count))))))
                              (t (multiple-value-bind (kept count)
                                     (dedupe-on-disk job rope start end reverse keep-blanks crlf directory)
                                   (and kept (values kept (report count)))))))))))))

(defun sort-shading (job from to)
  "The region JOB, a sort or filter, is changing, from FROM below TO only, as
a range to shade in :JOB-QUEUED; none once an edit has touched it, which
gives up the result."
  (let ((start (max (sort-job-region-start job) from))
        (end (min (sort-job-region-end job) to)))
    (when (and (not (sort-job-changed job)) (< start end))
      (list (list start end :job-queued)))))

(defun sort-highlights (editor window rows)
  "Job highlight function: the region a sort or filter running in WINDOW's
buffer is changing, in ROWS only (SORT-SHADING)."
  (declare (ignore editor))
  (let ((job (tuatara.runtime:running-job (tuatara.display:window-buffer window) 'sort-job)))
    (when (and job (plusp (length rows)))
      (sort-shading job (tuatara.display:row-start (aref rows 0))
                    (tuatara.display:row-end (aref rows (1- (length rows))))))))

(defun install-sorting ()
  "Show the region sorts and filters are changing."
  (pushnew 'sort-highlights *job-highlight-functions*))
