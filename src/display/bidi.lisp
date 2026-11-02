;;;; src/display/bidi.lisp --- Right-to-left text: the order a line's characters are shown in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The Unicode Bidirectional Algorithm (UAX #9). A line's characters stay in
;;;; the buffer in the order they are written; these functions give each its
;;;; embedding level and, from the levels, the order a row shows them in. The
;;;; classes, the mirrored glyphs and the paired brackets are those of the
;;;; Unicode Character Database SBCL carries (SB-UNICODE).

(in-package #:tuatara.display)

(defconstant +bidi-max-depth+ 125 "The deepest embedding level the algorithm reaches.")

(defconstant +bracket-pairs-most+ 63 "The most opening brackets awaiting their closing one in a run.")

(defun code-bidi-class (code)
  "The bidirectional class of the character of CODE, a keyword such as :L
\(left to right), :R (right to left), :AL (Arabic letter), :EN (European
digit), :AN (Arabic digit), :WS (blank), :ON (other neutral), or one of the
invisible characters that set the direction text is drawn in (:LRE :RLE :LRO
:RLO :PDF :LRI :RLI :FSI :PDI)."
  (sb-unicode:bidi-class (code-char code)))

(declaim (inline right-to-left-code-p))
(defun right-to-left-code-p (code)
  "True when the character of CODE can change the order a line is shown in: a
letter written from right to left (Hebrew, Arabic and the others), an Arabic
digit, or one of the invisible characters that set the direction text is
drawn in. Every such character is at U+0590 or above, so any below is
answered at once. Complexity: O(1)."
  (declare (type fixnum code))
  (and (>= code #x590)
       (or (<= code #x8FF) (<= #x200F code #x2069) (<= #xFB1D code #xFEFE)
           (<= #x10800 code #x10FFF) (<= #x1E800 code #x1EFFF))
       (member (code-bidi-class code) '(:r :al :an :lre :rle :lro :rlo :pdf :lri :rli :fsi :pdi))
       t))

(declaim (inline strong-code-p))
(defun strong-code-p (code)
  "True when the character of CODE is a letter, one whose direction is its
own: a Latin letter, or any of class :L, :R or :AL. Complexity: O(1)."
  (declare (type fixnum code))
  (if (< code 128)
      (or (<= 65 code 90) (<= 97 code 122))
      (and (member (code-bidi-class code) '(:l :r :al)) t)))

(defun bracket-pair (code)
  "Two values for the character of CODE when it is a paired bracket: the code
of the bracket it pairs with, and :OPEN or :CLOSE; NIL when it is none. The
angle brackets U+2329 and U+232A are given as U+3008 and U+3009, the same
brackets."
  (let ((char (code-char code)))
    (flet ((same (code) (case code (#x2329 #x3008) (#x232A #x3009) (t code))))
      (case (sb-unicode:general-category char)
        ((:ps :pe)
         (let ((other (sb-unicode:bidi-mirroring-glyph char)))
           (when (and other (member (sb-unicode:general-category other) '(:ps :pe))
                      (not (eq (sb-unicode:general-category other) (sb-unicode:general-category char))))
             (values (same (char-code other)) (if (eq (sb-unicode:general-category char) :ps) :open :close)))))))))

(defun mirrored-char (char)
  "The character CHAR is shown as in text written from right to left: the
other of a pair such as ( and ), < and >, « and »; CHAR itself when it has
none."
  (if (< (char-code char) 128)
      (case char (#\( #\)) (#\) #\() (#\< #\>) (#\> #\<) (#\[ #\]) (#\] #\[) (#\{ #\}) (#\} #\{) (t char))
      (or (sb-unicode:bidi-mirroring-glyph char) char)))

(declaim (inline isolate-initiator-p removed-class-p strong-direction))
(defun isolate-initiator-p (class)
  "True when CLASS begins an isolate: :LRI, :RLI or :FSI."
  (member class '(:lri :rli :fsi)))

(defun removed-class-p (class)
  "True when a character of CLASS takes no part in working out the levels of
the others: the embeddings and overrides, their end and the boundary
neutrals (:BN)."
  (member class '(:lre :rle :lro :rlo :pdf :bn)))

(defun strong-direction (class)
  "The direction CLASS counts as beside neutrals: :L for :L, :R for :R, :AL,
:EN and :AN, NIL for any other."
  (case class (:l :l) ((:r :al :en :an) :r)))


(defun direction-of-level (level)
  "The direction of embedding LEVEL: :R when it is odd, :L when it is even."
  (if (oddp level) :r :l))

;;; Scratch room, kept and used again

(defstruct (bidi-scratch (:constructor make-bidi-scratch ()) (:copier nil))
  "The room the levels of a paragraph are worked out in, kept from one
paragraph to the next so that working them out makes no garbage once it is
large enough: for each of up to SIZE characters, their CLASSES as written,
their TYPES as the rules change them, their CODES, their LEVELS and the
EXPLICIT levels the embeddings give them, the index of the end or beginning
of the isolate each begins or ends (MATCH), the level run it is in (RUN-OF),
and STACK, room for a stack; for each level run its RUN-FIRST and RUN-LAST
characters; for the isolating run sequence worked on, the indices of its
characters (SEQUENCE), their classes (TS) and each opening bracket's closing
one (PARTNER); for a line, the levels it is shown at (LINE) and the order
\(ORDER). The embeddings' stack and the brackets' are of fixed size."
  (size 0 :type fixnum)
  (classes #() :type simple-vector)
  (types #() :type simple-vector)
  (codes (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (levels (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (explicit (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (match (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (run-of (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (stack (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (run-first (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (run-last (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (sequence (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (ts #() :type simple-vector)
  (partner (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (line (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (order (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (stack-level (make-array (+ 2 +bidi-max-depth+) :element-type 'fixnum) :type (simple-array fixnum (*)))
  (stack-override (make-array (+ 2 +bidi-max-depth+) :initial-element nil) :type simple-vector)
  (stack-isolate (make-array (+ 2 +bidi-max-depth+) :initial-element nil) :type simple-vector)
  (open-code (make-array +bracket-pairs-most+ :element-type 'fixnum) :type (simple-array fixnum (*)))
  (open-at (make-array +bracket-pairs-most+ :element-type 'fixnum) :type (simple-array fixnum (*))))

(setf (documentation 'bidi-scratch-p 'function) "True when OBJECT is a BIDI-SCRATCH."
      (documentation 'bidi-scratch-size 'function) "How many characters the SCRATCH has room for."
      (documentation 'bidi-scratch-classes 'function) "The classes of the characters as written."
      (documentation 'bidi-scratch-types 'function) "The classes of the characters as the rules change them."
      (documentation 'bidi-scratch-codes 'function) "The codes of the characters."
      (documentation 'bidi-scratch-levels 'function) "The levels worked out for the characters."
      (documentation 'bidi-scratch-explicit 'function) "The levels the embeddings give the characters."
      (documentation 'bidi-scratch-match 'function) "For each isolate's beginning or end, the index of the other; -1 for none."
      (documentation 'bidi-scratch-run-of 'function) "For each character, the level run it is in."
      (documentation 'bidi-scratch-stack 'function) "Room for a stack of indices."
      (documentation 'bidi-scratch-run-first 'function) "For each level run, its first character."
      (documentation 'bidi-scratch-run-last 'function) "For each level run, its last character."
      (documentation 'bidi-scratch-sequence 'function) "The indices of the characters of the sequence worked on."
      (documentation 'bidi-scratch-ts 'function) "The classes of the characters of the sequence worked on."
      (documentation 'bidi-scratch-partner 'function) "For each opening bracket of the sequence, its closing one's place; -1 else."
      (documentation 'bidi-scratch-line 'function) "The levels a line's characters are shown at."
      (documentation 'bidi-scratch-order 'function) "The order a line's characters are shown in."
      (documentation 'bidi-scratch-stack-level 'function) "The embeddings' stack: levels."
      (documentation 'bidi-scratch-stack-override 'function) "The embeddings' stack: overrides."
      (documentation 'bidi-scratch-stack-isolate 'function) "The embeddings' stack: whether each is an isolate."
      (documentation 'bidi-scratch-open-code 'function) "The brackets' stack: the closing bracket awaited."
      (documentation 'bidi-scratch-open-at 'function) "The brackets' stack: where the opening one is.")

(defun bidi-scratch-room (scratch n)
  "Make SCRATCH large enough for N characters, its arrays made anew, twice as
large, only when it is not. Returns SCRATCH."
  (let ((size (bidi-scratch-size scratch)))
    (when (> n size)
      (let ((size (max n (* 2 size) 64)))
        (flet ((fixnums () (make-array size :element-type 'fixnum))
               (octets () (make-array size :element-type '(unsigned-byte 8))))
          (setf (bidi-scratch-size scratch) size
                (bidi-scratch-classes scratch) (make-array size :initial-element :l)
                (bidi-scratch-types scratch) (make-array size :initial-element :l)
                (bidi-scratch-ts scratch) (make-array size :initial-element :l)
                (bidi-scratch-codes scratch) (fixnums)
                (bidi-scratch-levels scratch) (octets)
                (bidi-scratch-explicit scratch) (octets)
                (bidi-scratch-line scratch) (octets)
                (bidi-scratch-match scratch) (fixnums)
                (bidi-scratch-run-of scratch) (fixnums)
                (bidi-scratch-stack scratch) (fixnums)
                (bidi-scratch-run-first scratch) (fixnums)
                (bidi-scratch-run-last scratch) (fixnums)
                (bidi-scratch-sequence scratch) (fixnums)
                (bidi-scratch-partner scratch) (fixnums)
                (bidi-scratch-order scratch) (fixnums))))))
  scratch)

;;; The levels of a paragraph (rules P2 to I2)

(defun matching-isolates (scratch n)
  "Fill SCRATCH's MATCH for its first N characters: for each isolate's
beginning (:LRI, :RLI, :FSI) the index of the :PDI that ends it, and for that
:PDI the index of its beginning; -1 where there is none. A paragraph's end
\(:B) ends every isolate."
  (let ((classes (bidi-scratch-classes scratch))
        (match (bidi-scratch-match scratch))
        (stack (bidi-scratch-stack scratch))
        (depth 0))
    (declare (type fixnum depth))
    (fill match -1 :end n)
    (dotimes (i n)
      (let ((class (svref classes i)))
        (cond ((isolate-initiator-p class) (setf (aref stack depth) i) (incf depth))
              ((and (eq class :pdi) (plusp depth))
               (decf depth)
               (let ((j (aref stack depth))) (setf (aref match j) i (aref match i) j)))
              ((eq class :b) (setf depth 0)))))))

(defun first-strong-level (classes start end)
  "The level of a paragraph of CLASSES from START below END as its first
letter says, the isolates passed over: 0 for :L, 1 for :R or :AL, NIL when
there is none before END or the paragraph's end (:B)."
  (let ((depth 0))
    (declare (type fixnum depth))
    (loop for i from start below end
          for class = (svref classes i)
          do (cond ((isolate-initiator-p class) (incf depth))
                   ((eq class :pdi) (when (plusp depth) (decf depth)))
                   ((eq class :b) (return nil))
                   ((plusp depth))
                   ((eq class :l) (return 0))
                   ((member class '(:r :al)) (return 1))))))

(defun explicit-levels (scratch n paragraph-level)
  "Give each of SCRATCH's first N characters its embedding level in LEVELS as
the embeddings, overrides and isolates around it say, and its class in
TYPES as an override changes it (rules X1 to X8)."
  (let ((classes (bidi-scratch-classes scratch)) (types (bidi-scratch-types scratch))
        (levels (bidi-scratch-levels scratch)) (match (bidi-scratch-match scratch))
        (stack-level (bidi-scratch-stack-level scratch))
        (stack-override (bidi-scratch-stack-override scratch))
        (stack-isolate (bidi-scratch-stack-isolate scratch))
        (depth 0) (overflow-isolates 0) (overflow-embeddings 0) (valid-isolates 0))
    (declare (type fixnum depth overflow-isolates overflow-embeddings valid-isolates))
    (labels ((reset ()
               (setf depth 0 overflow-isolates 0 overflow-embeddings 0 valid-isolates 0
                     (aref stack-level 0) paragraph-level (svref stack-override 0) nil (svref stack-isolate 0) nil))
             (top () (aref stack-level depth))
             (next-level (odd)
               ;; the least odd level above the top's, or the least even one
               (let ((top (top)))
                 (if (eq (and odd t) (evenp top)) (+ top 1) (+ top 2))))
             (push-entry (level override isolate)
               (incf depth)
               (setf (aref stack-level depth) level
                     (svref stack-override depth) override
                     (svref stack-isolate depth) isolate))
             (overridden (i)
               (let ((override (svref stack-override depth)))
                 (when override (setf (svref types i) override)))))
      (reset)
      (dotimes (i n)
        (let ((class (svref classes i)))
          (case class
            ((:rle :lre :rlo :lro)
             (setf (aref levels i) (top))
             (let ((level (next-level (member class '(:rle :rlo)))))
               (cond ((and (<= level +bidi-max-depth+) (zerop overflow-isolates) (zerop overflow-embeddings))
                      (push-entry level (case class (:rlo :r) (:lro :l)) nil))
                     ((zerop overflow-isolates) (incf overflow-embeddings)))))
            ((:rli :lri :fsi)
             (setf (aref levels i) (top))
             (overridden i)
             (let* ((odd (case class
                           (:rli t)
                           (:lri nil)
                           (t (let ((end (if (minusp (aref match i)) n (aref match i))))
                                (eql 1 (first-strong-level classes (1+ i) end))))))
                    (level (next-level odd)))
               (cond ((and (<= level +bidi-max-depth+) (zerop overflow-isolates) (zerop overflow-embeddings))
                      (incf valid-isolates)
                      (push-entry level nil t))
                     (t (incf overflow-isolates)))))
            (:pdi
             (cond ((plusp overflow-isolates) (decf overflow-isolates))
                   ((zerop valid-isolates))
                   (t (setf overflow-embeddings 0)
                      (loop until (svref stack-isolate depth) do (decf depth))
                      (decf depth)
                      (decf valid-isolates)))
             (setf (aref levels i) (top))
             (overridden i))
            (:pdf
             (cond ((plusp overflow-isolates))
                   ((plusp overflow-embeddings) (decf overflow-embeddings))
                   ((and (not (svref stack-isolate depth)) (plusp depth)) (decf depth)))
             (setf (aref levels i) (top)))
            (:b
             (setf (aref levels i) paragraph-level)
             (reset))
            (:bn (setf (aref levels i) (top)))
            (t (setf (aref levels i) (top))
               (overridden i))))))))

(defun level-runs (scratch n)
  "Find the level runs of SCRATCH's first N characters, those removed from
the working out (REMOVED-CLASS-P) left out: each run's first and last
character in RUN-FIRST and RUN-LAST, each character's run in RUN-OF. Returns
how many runs there are."
  (let ((classes (bidi-scratch-classes scratch)) (levels (bidi-scratch-levels scratch))
        (run-of (bidi-scratch-run-of scratch))
        (run-first (bidi-scratch-run-first scratch)) (run-last (bidi-scratch-run-last scratch))
        (runs 0) (level -1))
    (declare (type fixnum runs level))
    (dotimes (i n runs)
      (if (removed-class-p (svref classes i))
          (setf (aref run-of i) -1)
          (progn
            (unless (and (plusp runs) (= (aref levels i) level))
              (setf (aref run-first runs) i level (aref levels i))
              (incf runs))
            (setf (aref run-last (1- runs)) i (aref run-of i) (1- runs)))))))

(defun fill-sequence (scratch run)
  "Fill SCRATCH's SEQUENCE with the isolating run sequence beginning with
level RUN (rule X10): its characters, then those of the run an isolate's end
begins after one ending it with the isolate's beginning, and so on. Returns
how many characters it holds."
  (let ((classes (bidi-scratch-classes scratch)) (match (bidi-scratch-match scratch))
        (run-of (bidi-scratch-run-of scratch))
        (run-first (bidi-scratch-run-first scratch)) (run-last (bidi-scratch-run-last scratch))
        (sequence (bidi-scratch-sequence scratch))
        (m 0))
    (declare (type fixnum m))
    (loop
      (loop for i from (aref run-first run) to (aref run-last run)
            unless (removed-class-p (svref classes i))
              do (setf (aref sequence m) i) (incf m))
      (let* ((last (aref run-last run))
             (end (aref match last)))
        (if (and (isolate-initiator-p (svref classes last)) (>= end 0)
                 (= end (aref run-first (aref run-of end))))
            (setf run (aref run-of end))
            (return m))))))

(defun continues-sequence-p (scratch run)
  "True when level RUN of SCRATCH begins with an isolate's end whose
beginning ends a run: the run goes on the sequence of that one (FILL-SEQUENCE)."
  (let* ((first (aref (bidi-scratch-run-first scratch) run))
         (start (aref (bidi-scratch-match scratch) first)))
    (and (eq (svref (bidi-scratch-classes scratch) first) :pdi)
         (>= start 0)
         (= start (aref (bidi-scratch-run-last scratch) (aref (bidi-scratch-run-of scratch) start))))))

(defun level-beside (classes levels i step n paragraph-level)
  "The level of the first character from I on, going by STEP (1 or -1)
within the first N, that takes part in working out the levels; the
paragraph's level when there is none."
  (loop for j = (+ i step) then (+ j step)
        while (and (<= 0 j) (< j n))
        unless (removed-class-p (svref classes j))
          do (return (aref levels j))
        finally (return paragraph-level)))

(defun resolve-weak-types (ts m sos)
  "Resolve the weak classes of the first M of TS, the classes of an isolating
run sequence in order, in place: marks, digits and their separators and
terminators (rules W1 to W7), SOS being the direction before the sequence."
  ;; W1: a mark takes the class of what it is on
  (dotimes (k m)
    (when (eq (svref ts k) :nsm)
      (setf (svref ts k) (cond ((zerop k) sos)
                               ((member (svref ts (1- k)) '(:lri :rli :fsi :pdi)) :on)
                               (t (svref ts (1- k)))))))
  ;; W2: a European digit after an Arabic letter is an Arabic one; W3: an Arabic letter is right to left
  (let ((strong sos))
    (dotimes (k m)
      (case (svref ts k)
        ((:l :r :al) (setf strong (svref ts k)))
        (:en (when (eq strong :al) (setf (svref ts k) :an))))))
  (dotimes (k m) (when (eq (svref ts k) :al) (setf (svref ts k) :r)))
  ;; W4: one separator between two digits of a kind
  (loop for k from 1 below (1- m)
        do (let ((before (svref ts (1- k))) (after (svref ts (1+ k))))
             (case (svref ts k)
               (:es (when (and (eq before :en) (eq after :en)) (setf (svref ts k) :en)))
               (:cs (cond ((and (eq before :en) (eq after :en)) (setf (svref ts k) :en))
                          ((and (eq before :an) (eq after :an)) (setf (svref ts k) :an)))))))
  ;; W5: terminators beside a European digit; W6: separators and terminators left are neutral
  (let ((k 0))
    (declare (type fixnum k))
    (loop while (< k m)
          do (if (eq (svref ts k) :et)
                 (let ((j (loop for j from k below m unless (eq (svref ts j) :et) return j finally (return m))))
                   (when (or (and (plusp k) (eq (svref ts (1- k)) :en)) (and (< j m) (eq (svref ts j) :en)))
                     (fill ts :en :start k :end j))
                   (setf k j))
                 (incf k))))
  (dotimes (k m) (when (member (svref ts k) '(:es :et :cs)) (setf (svref ts k) :on)))
  ;; W7: a European digit after a left-to-right letter is left to right
  (let ((strong sos))
    (dotimes (k m)
      (case (svref ts k)
        ((:l :r) (setf strong (svref ts k)))
        (:en (when (eq strong :l) (setf (svref ts k) :l)))))))

(defun resolve-brackets (scratch m sos level)
  "Give the pairs of brackets of the isolating run sequence of SCRATCH, M
characters at LEVEL, the direction of what they hold or stand among (rule
N0), SOS being the direction before the sequence."
  (let ((ts (bidi-scratch-ts scratch)) (sequence (bidi-scratch-sequence scratch))
        (classes (bidi-scratch-classes scratch)) (codes (bidi-scratch-codes scratch))
        (partner (bidi-scratch-partner scratch))
        (open-code (bidi-scratch-open-code scratch)) (open-at (bidi-scratch-open-at scratch))
        (depth 0) (embedding (direction-of-level level)))
    (declare (type fixnum depth))
    (fill partner -1 :end m)
    ;; the pairs (BD16): each opening bracket's closing one in PARTNER; none at all past 63 open
    (dotimes (k m)
      (when (eq (svref ts k) :on)
        (multiple-value-bind (other kind) (bracket-pair (aref codes (aref sequence k)))
          (case kind
            (:open (when (= depth +bracket-pairs-most+)
                     (fill partner -1 :end m)
                     (return))
                   (setf (aref open-code depth) other (aref open-at depth) k)
                   (incf depth))
            (:close (let ((code (case (aref codes (aref sequence k)) (#x2329 #x3008) (#x232A #x3009)
                                  (t (aref codes (aref sequence k))))))
                      (loop for d downfrom (1- depth) to 0
                            when (= (aref open-code d) code)
                              do (setf (aref partner (aref open-at d)) k depth d)
                                 (return))))))))
    (flet ((set-bracket (k direction)
             (setf (svref ts k) direction)
             ;; marks written after a bracket go with it
             (loop for j from (1+ k) below m
                   while (eq (svref classes (aref sequence j)) :nsm)
                   do (setf (svref ts j) direction))))
      (dotimes (open m)
        (let ((close (aref partner open)))
          (when (>= close 0)
            (let ((inside (loop with found = nil
                                for k from (1+ open) below close
                                for strong = (strong-direction (svref ts k))
                                do (cond ((eq strong embedding) (return embedding))
                                         (strong (setf found strong)))
                                finally (return found))))
              (when inside
                (let ((direction (if (eq inside embedding)
                                     embedding
                                     (let ((before (or (loop for k downfrom (1- open) to 0
                                                             for strong = (strong-direction (svref ts k))
                                                             when strong return strong)
                                                       sos)))
                                       (if (eq before inside) inside embedding)))))
                  (set-bracket open direction)
                  (set-bracket close direction))))))))))

(defun resolve-neutrals (ts m sos eos level)
  "Give the neutrals of the first M of TS, the classes of an isolating run
sequence at LEVEL, the direction of the text around them when it is the same
on both sides, else that of LEVEL (rules N1 and N2); SOS and EOS are the
directions before and after the sequence."
  (let ((k 0))
    (declare (type fixnum k))
    (flet ((neutral-p (class) (member class '(:b :s :ws :on :lri :rli :fsi :pdi))))
      (loop while (< k m)
            do (if (neutral-p (svref ts k))
                   (let* ((j (loop for j from k below m unless (neutral-p (svref ts j)) return j finally (return m)))
                          (before (if (zerop k) sos (strong-direction (svref ts (1- k)))))
                          (after (if (= j m) eos (strong-direction (svref ts j)))))
                     (fill ts (if (eq before after) before (direction-of-level level)) :start k :end j)
                     (setf k j))
                   (incf k))))))

(defun resolve-levels (scratch n paragraph-level &key brackets)
  "Work out the levels of the first N characters of SCRATCH, whose classes
and codes are filled in, into its LEVELS, as the Unicode Bidirectional
Algorithm resolves them: PARAGRAPH-LEVEL 0 for a paragraph written from left
to right, 1 from right to left, NIL from its first letter (0 when it has
none); with BRACKETS, the paired brackets by their codes. A character
removed from the working out (an embedding, an override or their end, a
boundary neutral) takes the level of the one before it, an end of embedding
that of the one after it. Returns the paragraph's level. Makes no garbage.
Complexity: O(n), but for brackets O(n × 63)."
  (let* ((classes (bidi-scratch-classes scratch))
         (types (bidi-scratch-types scratch))
         (levels (bidi-scratch-levels scratch))
         (explicit (bidi-scratch-explicit scratch))
         (sequence (bidi-scratch-sequence scratch))
         (ts (bidi-scratch-ts scratch))
         (paragraph-level (or paragraph-level (first-strong-level classes 0 n) 0)))
    (replace types classes :end2 n)
    (matching-isolates scratch n)
    (explicit-levels scratch n paragraph-level)
    (replace explicit levels :end2 n)
    (dotimes (run (level-runs scratch n))
      (unless (continues-sequence-p scratch run)
        (let* ((m (fill-sequence scratch run))
               (first (aref sequence 0))
               (last (aref sequence (1- m)))
               (level (aref explicit first))
               (sos (direction-of-level (max level (level-beside classes explicit first -1 n paragraph-level))))
               (eos (direction-of-level (max level (if (isolate-initiator-p (svref classes last))
                                                       paragraph-level
                                                       (level-beside classes explicit last 1 n paragraph-level))))))
          (dotimes (k m) (setf (svref ts k) (svref types (aref sequence k))))
          (resolve-weak-types ts m sos)
          (when brackets (resolve-brackets scratch m sos level))
          (resolve-neutrals ts m sos eos level)
          ;; I1 and I2: each character's level from its direction
          (dotimes (k m)
            (let* ((i (aref sequence k)) (at (aref levels i)) (type (svref ts k)))
              (setf (aref levels i)
                    (if (evenp at)
                        (case type (:r (+ at 1)) ((:an :en) (+ at 2)) (t at))
                        (case type ((:l :en :an) (+ at 1)) (t at)))))))))
    ;; the characters left out: beside the one before them, an end of embedding beside the one after
    (dotimes (i n)
      (let ((class (svref classes i)))
        (when (removed-class-p class)
          (setf (aref levels i)
                (if (eq class :pdf)
                    (level-beside classes levels i 1 n paragraph-level)
                    (if (plusp i) (aref levels (1- i)) paragraph-level))))))
    paragraph-level))

(defun fill-line-levels (classes levels n paragraph-level line)
  "Fill LINE with the levels the first N characters of one line, of CLASSES
at LEVELS, are shown at, as rule L1 sets them: a tab (:S) or a paragraph's
end (:B), the blanks and isolates before it, and those at the line's end, at
PARAGRAPH-LEVEL."
  (let ((trailing t))
    (loop for i downfrom (1- n) to 0
          for class = (svref classes i)
          do (cond ((member class '(:s :b)) (setf trailing t (aref line i) paragraph-level))
                   ((and trailing (or (member class '(:ws :lri :rli :fsi :pdi)) (removed-class-p class)))
                    (setf (aref line i) paragraph-level))
                   (t (setf trailing nil (aref line i) (aref levels i)))))
    line))

(defun fill-visual-order (line n order)
  "Fill ORDER with the order the first N characters of a line at levels LINE
\(FILL-LINE-LEVELS) are shown in from left to right, as their indices: from
the highest level to the lowest odd one, each run at that level or above
turned round (rule L2). Complexity: O(n × levels)."
  (dotimes (i n) (setf (aref order i) i))
  (when (plusp n)
    (let ((highest 0) (lowest 255))
      (dotimes (i n) (setf highest (max highest (aref line i)) lowest (min lowest (aref line i))))
      (loop for level downfrom highest to (if (oddp lowest) lowest (1+ lowest))
            do (let ((i 0))
                 (declare (type fixnum i))
                 (loop while (< i n)
                       do (if (>= (aref line (aref order i)) level)
                              (let ((j (loop for j from i below n
                                             when (< (aref line (aref order j)) level) return j
                                             finally (return n))))
                                (loop for a from i and b downfrom (1- j) while (< a b)
                                      do (rotatef (aref order a) (aref order b)))
                                (setf i j))
                              (incf i)))))))
  order)

(defun bidi-levels (classes &key paragraph-level codes)
  "The embedding level of each character of a paragraph whose bidirectional
classes (CODE-BIDI-CLASS) are CLASSES, a simple vector, as a new vector of
\(UNSIGNED-BYTE 8), and the paragraph's level as the second value
\(RESOLVE-LEVELS): PARAGRAPH-LEVEL 0, 1, or NIL for its first letter's;
CODES, the characters' codes, pairs the brackets."
  (let* ((n (length classes))
         (scratch (bidi-scratch-room (make-bidi-scratch) n)))
    (replace (bidi-scratch-classes scratch) classes)
    (when codes (replace (bidi-scratch-codes scratch) codes))
    (let ((level (resolve-levels scratch n paragraph-level :brackets codes)))
      (values (subseq (bidi-scratch-levels scratch) 0 n) level))))

(defun line-levels (classes levels paragraph-level)
  "The levels the characters of one line, of CLASSES at LEVELS, are shown at
\(FILL-LINE-LEVELS), as a new vector."
  (let ((n (length classes)))
    (fill-line-levels classes levels n paragraph-level (make-array n :element-type '(unsigned-byte 8)))))

(defun visual-order (levels)
  "The order a line of characters at LEVELS, as LINE-LEVELS gives them, is
shown in from left to right (FILL-VISUAL-ORDER), as a new vector of their
indices."
  (let ((n (length levels)))
    (fill-visual-order levels n (make-array n :element-type 'fixnum))))

;;; Reading a line's text

(defmacro do-line-codes ((code at rope from to) &body body)
  "Run BODY for each character of ROPE from FROM below TO, both character
starts, CODE bound to its code and AT to where it begins, reading the text a
block at a time; BODY leaves early by RETURN-FROM a block around it."
  (let ((block (gensym "BLOCK")) (pos (gensym "POS")) (stop (gensym "STOP")) (have (gensym "HAVE"))
        (i (gensym "I")) (length (gensym "LENGTH")) (total (gensym "TOTAL")))
    `(let ((,total (tuatara.text:rope-bytes ,rope)))
       (tuatara.text:with-octet-block (,block (+ 65536 4))
         (block nil
           (let ((,pos ,from))
             (declare (type fixnum ,pos))
             (loop while (< ,pos ,to)
                   do (let* ((,stop (min ,to (+ ,pos 65536)))
                             (,have (min ,total (+ ,stop 3)))
                             (,i 0))
                        (declare (type fixnum ,stop ,have ,i))
                        (tuatara.text:rope-copy-into ,rope ,pos ,have ,block)
                        (loop while (< ,i (- ,stop ,pos))
                              do (multiple-value-bind (,code ,length)
                                     (let ((octet (aref ,block ,i)))
                                       (if (< octet 128) (values octet 1)
                                           (tuatara.text:octets-decode ,block ,i (- ,have ,pos))))
                                   (declare (type fixnum ,code ,length))
                                   (let ((,at (+ ,pos ,i)))
                                     (declare (ignorable ,at))
                                     ,@body)
                                   (incf ,i ,length)))
                        (setf ,pos (+ ,pos ,i))))))))))

(defun first-strong-direction (rope from to)
  "The direction of the first letter of ROPE's text from FROM below TO, both
character starts, the isolates passed over, stopping at the line's end: :L
for one written from left to right, :R from right to left, NIL for none.
An ASCII octet is looked at at once. Complexity: O(TO - FROM), stopping at
the letter."
  (let ((depth 0))
    (declare (type fixnum depth))
    (do-line-codes (code at rope from to)
      (cond ((< code 128)
             (cond ((or (= code 10) (= code 13)) (return-from first-strong-direction nil))
                   ((and (zerop depth) (or (<= 65 code 90) (<= 97 code 122))) (return-from first-strong-direction :l))))
            (t (let ((class (code-bidi-class code)))
                 (cond ((isolate-initiator-p class) (incf depth))
                       ((eq class :pdi) (when (plusp depth) (decf depth)))
                       ((eq class :b) (return-from first-strong-direction nil))
                       ((plusp depth))
                       ((eq class :l) (return-from first-strong-direction :l))
                       ((member class '(:r :al)) (return-from first-strong-direction :r)))))))
    nil))

(defun direction-controls-between-p (rope from to)
  "True when ROPE's text from FROM below TO holds an embedding, override or
isolate (U+202A to U+202E, U+2066 to U+2069), whose effect on the text after
it cannot be told without it: found by their octets alone. Complexity:
O(TO - FROM)."
  (let ((total (tuatara.text:rope-bytes rope)))
    (tuatara.text:with-octet-block (block (+ 65536 2))
      (loop for pos of-type fixnum from from below to by 65536
            do (let* ((stop (min to (+ pos 65536)))
                      (have (min total (+ stop 2))))
                 (tuatara.text:rope-copy-into rope pos have block)
                 (loop for i of-type fixnum from 0 below (- stop pos)
                       do (when (and (= (aref block i) #xE2) (< (+ i 2) (- have pos)))
                            (let ((second (aref block (1+ i))) (third (aref block (+ i 2))))
                              (when (or (and (= second #x80) (<= #xAA third #xAE))
                                        (and (= second #x81) (<= #xA6 third #xA9)))
                                (return-from direction-controls-between-p t))))))))
    nil))

(defun right-to-left-between-p (rope from to)
  "True when a character of ROPE from FROM below TO, both character starts,
can change the order a line is shown in (RIGHT-TO-LEFT-CODE-P). Only the
octets that begin a character at U+0590 or above are decoded. Complexity:
O(TO - FROM)."
  (declare (type fixnum from to))
  (let ((total (tuatara.text:rope-bytes rope)))
    (tuatara.text:with-octet-block (block (+ 65536 4))
      (loop for pos of-type fixnum from from below to by 65536
            do (let* ((stop (min to (+ pos 65536)))
                      (have (min total (+ stop 3))))
                 (tuatara.text:rope-copy-into rope pos have block)
                 (loop for i of-type fixnum from 0 below (- stop pos)
                       do (when (and (>= (aref block i) #xD6)
                                     (right-to-left-code-p (tuatara.text:octets-decode block i (- have pos))))
                            (return-from right-to-left-between-p t)))))
      nil)))

;;; Paragraphs worked out, kept for the next layout

(defstruct (bidi-paragraph (:constructor make-bidi-paragraph ()) (:copier nil))
  "A stretch of a line worked out for showing right-to-left text, its arrays
used again for another once ROPE is not the text shown: the octets from
START below END of ROPE, as DIRECTION (:AUTO, :LEFT-TO-RIGHT,
:RIGHT-TO-LEFT) said; COUNT characters, where each begins (BYTES), their
bidirectional CLASSES and resolved LEVELS, and the LEVEL of the paragraph, 1
for a line written from right to left. STORED is true when the stretch, far
into a long line, is shown in the order stored, as what comes before it
cannot be told."
  (rope nil)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (direction nil)
  (count 0 :type fixnum)
  (bytes (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (classes #() :type simple-vector)
  (levels (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (level 0 :type fixnum)
  (stored nil))

(setf (documentation 'bidi-paragraph-p 'function) "True when OBJECT is a BIDI-PARAGRAPH."
      (documentation 'bidi-paragraph-rope 'function) "The text the PARAGRAPH was worked out of, or NIL when free."
      (documentation 'bidi-paragraph-start 'function) "Where the text of the PARAGRAPH worked out begins."
      (documentation 'bidi-paragraph-end 'function) "Where the text of the PARAGRAPH worked out ends."
      (documentation 'bidi-paragraph-direction 'function) "The direction the PARAGRAPH was worked out for."
      (documentation 'bidi-paragraph-count 'function) "How many characters the PARAGRAPH holds."
      (documentation 'bidi-paragraph-bytes 'function) "The offset each character of the PARAGRAPH begins at, in order."
      (documentation 'bidi-paragraph-classes 'function) "The bidirectional class of each character of the PARAGRAPH."
      (documentation 'bidi-paragraph-levels 'function) "The level each character of the PARAGRAPH is shown at."
      (documentation 'bidi-paragraph-level 'function) "The level of the PARAGRAPH: 0 from left to right, 1 from right to left."
      (documentation 'bidi-paragraph-stored 'function) "True when the PARAGRAPH is shown in the order stored.")

(defstruct (bidi-cache (:constructor make-bidi-cache ()) (:copier nil))
  "What a window keeps of right-to-left text from one layout to the next: the
PARAGRAPHS worked out of ROPE, the text it last showed, those of a text
before free to be filled again (NEXT, the one to fill when none is); the
SCRATCH they are worked out in; whether right-to-left text was found
between SCAN-FROM and SCAN-TO of SCAN-ROPE (SCAN-FOUND); and the room a row
is put in order in: its STRING, FACES, CHARS and BYTES copied, and UNITS, five
numbers for each character shown with the marks on it."
  (rope nil)
  (paragraphs (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (next 0 :type fixnum)
  (scratch (make-bidi-scratch) :type bidi-scratch)
  (scan-rope nil) (scan-from 0 :type fixnum) (scan-to 0 :type fixnum) (scan-found nil)
  (string (make-string 0) :type simple-string)
  (faces #() :type simple-vector)
  (chars (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (bytes (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (units (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*))))

(setf (documentation 'bidi-cache-p 'function) "True when OBJECT is a BIDI-CACHE."
      (documentation 'make-bidi-cache 'function) "A new BIDI-CACHE, holding nothing yet."
      (documentation 'bidi-cache-rope 'function) "The text the CACHE's paragraphs were worked out of."
      (documentation 'bidi-cache-paragraphs 'function) "The paragraphs the CACHE keeps."
      (documentation 'bidi-cache-next 'function) "Which of the CACHE's paragraphs is filled next when none is free."
      (documentation 'bidi-cache-scratch 'function) "The room the CACHE's paragraphs are worked out in."
      (documentation 'bidi-cache-scan-rope 'function) "The text the CACHE last looked through for right-to-left text."
      (documentation 'bidi-cache-scan-from 'function) "Where that look began."
      (documentation 'bidi-cache-scan-to 'function) "Where that look ended."
      (documentation 'bidi-cache-scan-found 'function) "Whether that look found right-to-left text."
      (documentation 'bidi-cache-string 'function) "Room for a row's string while it is put in order."
      (documentation 'bidi-cache-faces 'function) "Room for a row's faces while it is put in order."
      (documentation 'bidi-cache-chars 'function) "Room for a row's characters' offsets while it is put in order."
      (documentation 'bidi-cache-bytes 'function) "Room for a row's columns' offsets while it is put in order."
      (documentation 'bidi-cache-units 'function) "Room for a row's characters shown, five numbers each.")

(defconstant +paragraphs-kept+ 256 "The most paragraphs a BIDI-CACHE keeps.")

(defun right-to-left-before-p (cache rope from to)
  "True when ROPE from FROM below TO holds right-to-left text
\(RIGHT-TO-LEFT-BETWEEN-P), as CACHE remembers from the last time it was
asked the same, else looked for and remembered."
  (if (and (eq rope (bidi-cache-scan-rope cache))
           (= from (bidi-cache-scan-from cache)) (= to (bidi-cache-scan-to cache)))
      (bidi-cache-scan-found cache)
      (setf (bidi-cache-scan-rope cache) rope (bidi-cache-scan-from cache) from (bidi-cache-scan-to cache) to
            (bidi-cache-scan-found cache) (right-to-left-between-p rope from to))))

(defun free-paragraph (cache)
  "A paragraph of CACHE to fill: one of a text no longer shown, else a new
one while it keeps fewer than +PARAGRAPHS-KEPT+, else the next in turn."
  (let ((paragraphs (bidi-cache-paragraphs cache)))
    (or (loop for paragraph across paragraphs
              unless (eq (bidi-paragraph-rope paragraph) (bidi-cache-rope cache))
                return paragraph)
        (if (< (length paragraphs) +paragraphs-kept+)
            (let ((paragraph (make-bidi-paragraph)))
              (vector-push-extend paragraph paragraphs)
              paragraph)
            (prog1 (aref paragraphs (bidi-cache-next cache))
              (setf (bidi-cache-next cache) (mod (1+ (bidi-cache-next cache)) +paragraphs-kept+)))))))

(defun fill-paragraph (paragraph scratch rope from to direction level stored)
  "Work PARAGRAPH out of ROPE's text from FROM below TO in SCRATCH, as
DIRECTION says, or at LEVEL (0 or 1) when it is not NIL; STORED marks it
shown in the order stored. Its arrays are made larger only when they must."
  (let ((room (- to from)))
    (bidi-scratch-room scratch room)
    (when (< (length (bidi-paragraph-bytes paragraph)) room)
      (let ((size (max room 64)))
        (setf (bidi-paragraph-bytes paragraph) (make-array size :element-type 'fixnum)
              (bidi-paragraph-classes paragraph) (make-array size :initial-element :l)
              (bidi-paragraph-levels paragraph) (make-array size :element-type '(unsigned-byte 8))))))
  (let ((n 0) (classes (bidi-scratch-classes scratch)) (codes (bidi-scratch-codes scratch))
        (bytes (bidi-paragraph-bytes paragraph)))
    (declare (type fixnum n))
    (do-line-codes (code at rope from to)
      (setf (aref bytes n) at (aref codes n) code (svref classes n) (code-bidi-class code))
      (incf n))
    (let ((level (resolve-levels scratch n (or level (case direction (:left-to-right 0) (:right-to-left 1)))
                                 :brackets t)))
      (replace (bidi-paragraph-classes paragraph) classes :end2 n)
      (replace (bidi-paragraph-levels paragraph) (bidi-scratch-levels scratch) :end2 n)
      (setf (bidi-paragraph-rope paragraph) rope (bidi-paragraph-start paragraph) from
            (bidi-paragraph-end paragraph) to (bidi-paragraph-direction paragraph) direction
            (bidi-paragraph-count paragraph) n (bidi-paragraph-level paragraph) level
            (bidi-paragraph-stored paragraph) stored)))
  paragraph)

(defun cached-paragraph (cache rope from to direction line-start reach)
  "The BIDI-PARAGRAPH of ROPE's text from FROM below TO, both character
starts on the line beginning at LINE-START, as DIRECTION (:AUTO,
:LEFT-TO-RIGHT, :RIGHT-TO-LEFT) says: the one CACHE keeps for them, else
worked out and kept, in arrays used again (no garbage once they are large
enough). A stretch beginning after LINE-START takes its direction under
:AUTO from the line's first letter within REACH octets of its start; it is
shown in the order stored (BIDI-PARAGRAPH-STORED) when an embedding,
override or isolate comes before it on the line, or it begins more than
REACH octets into the line, as the order read cannot then be told.
Complexity: O(TO - FROM) the first time, O(paragraphs kept) after."
  (unless (eq rope (bidi-cache-rope cache))
    (setf (bidi-cache-rope cache) rope))
  (or (loop for paragraph across (bidi-cache-paragraphs cache)
            when (and (eq (bidi-paragraph-rope paragraph) rope) (= (bidi-paragraph-start paragraph) from)
                      (= (bidi-paragraph-end paragraph) to) (eq (bidi-paragraph-direction paragraph) direction))
              return paragraph)
      (let* ((inside (> from line-start))
             (level (and inside (eq direction :auto)
                         (case (first-strong-direction rope line-start (min from (+ line-start reach)))
                           (:l 0) (:r 1))))
             (stored (and inside (or (> (- from line-start) reach)
                                     (direction-controls-between-p rope line-start from)))))
        (fill-paragraph (free-paragraph cache) (bidi-cache-scratch cache) rope from to direction level stored))))

;;; Arabic letters joined, for a frontend that cannot join them itself

(defparameter *arabic-forms*
  (let ((table (make-hash-table)))
    (loop for (letter isolated final initial medial)
            in '((#x0621 #xFE80 nil nil nil) (#x0622 #xFE81 #xFE82 nil nil) (#x0623 #xFE83 #xFE84 nil nil)
                 (#x0624 #xFE85 #xFE86 nil nil) (#x0625 #xFE87 #xFE88 nil nil) (#x0626 #xFE89 #xFE8A #xFE8B #xFE8C)
                 (#x0627 #xFE8D #xFE8E nil nil) (#x0628 #xFE8F #xFE90 #xFE91 #xFE92) (#x0629 #xFE93 #xFE94 nil nil)
                 (#x062A #xFE95 #xFE96 #xFE97 #xFE98) (#x062B #xFE99 #xFE9A #xFE9B #xFE9C)
                 (#x062C #xFE9D #xFE9E #xFE9F #xFEA0) (#x062D #xFEA1 #xFEA2 #xFEA3 #xFEA4)
                 (#x062E #xFEA5 #xFEA6 #xFEA7 #xFEA8) (#x062F #xFEA9 #xFEAA nil nil) (#x0630 #xFEAB #xFEAC nil nil)
                 (#x0631 #xFEAD #xFEAE nil nil) (#x0632 #xFEAF #xFEB0 nil nil) (#x0633 #xFEB1 #xFEB2 #xFEB3 #xFEB4)
                 (#x0634 #xFEB5 #xFEB6 #xFEB7 #xFEB8) (#x0635 #xFEB9 #xFEBA #xFEBB #xFEBC)
                 (#x0636 #xFEBD #xFEBE #xFEBF #xFEC0) (#x0637 #xFEC1 #xFEC2 #xFEC3 #xFEC4)
                 (#x0638 #xFEC5 #xFEC6 #xFEC7 #xFEC8) (#x0639 #xFEC9 #xFECA #xFECB #xFECC)
                 (#x063A #xFECD #xFECE #xFECF #xFED0) (#x0641 #xFED1 #xFED2 #xFED3 #xFED4)
                 (#x0642 #xFED5 #xFED6 #xFED7 #xFED8) (#x0643 #xFED9 #xFEDA #xFEDB #xFEDC)
                 (#x0644 #xFEDD #xFEDE #xFEDF #xFEE0) (#x0645 #xFEE1 #xFEE2 #xFEE3 #xFEE4)
                 (#x0646 #xFEE5 #xFEE6 #xFEE7 #xFEE8) (#x0647 #xFEE9 #xFEEA #xFEEB #xFEEC)
                 (#x0648 #xFEED #xFEEE nil nil) (#x0649 #xFEEF #xFEF0 nil nil) (#x064A #xFEF1 #xFEF2 #xFEF3 #xFEF4))
          do (setf (gethash letter table) (vector isolated final initial medial)))
    table)
  "For each Arabic letter of U+0621 to U+064A, the characters of Arabic
Presentation Forms-B it is shown as (isolated, final, initial, medial; NIL
where it has none), from the <isolated>, <final>, <initial> and <medial>
decompositions of UnicodeData.txt. A letter with a final form joins the one
before it, one with an initial form also the one after it.")

(defun arabic-joining (code)
  "How the character of CODE joins its neighbours: :DUAL (both sides), :RIGHT
\(the one before it only), :CAUSING (the tatweel and the zero-width joiner,
which join on both sides without a form of their own), :TRANSPARENT (a mark,
passed over), or NIL (none)."
  (let ((forms (gethash code *arabic-forms*)))
    (cond (forms (cond ((svref forms 2) :dual) ((svref forms 1) :right)))
          ((or (= code #x640) (= code #x200D)) :causing)
          ((and (>= code #x300) (mark-code-p code)) :transparent))))

(defun right-to-left-face-p (face)
  "True when FACE, a face name or a list of them, holds :RIGHT-TO-LEFT."
  (if (listp face) (member :right-to-left face) (eq face :right-to-left)))

(defun joined-arabic (string spans)
  "STRING, a row as drawn, with each Arabic letter of a run of right-to-left
text in the order it is read (the characters SPANS, lists (START END FACE),
give :RIGHT-TO-LEFT) as the form of Arabic Presentation Forms-B that joins it
to the letters beside it, as text is joined where it is written: a run's
order turned round, so that the letter written before stands at its right.
STRING itself, unchanged, when nothing in it is joined; it is never longer."
  (if (notany (lambda (span) (right-to-left-face-p (third span))) spans)
      string
      (let ((shaped nil))
        (flet ((joining (i start end step)
                 ;; how the first letter from I on, going by STEP within START and END, joins, marks passed over
                 (loop for j = (+ i step) then (+ j step)
                       while (and (<= start j) (< j end))
                       do (let ((join (arabic-joining (char-code (char string j)))))
                            (unless (eq join :transparent) (return join))))))
          (loop for (start end face) in spans
                when (right-to-left-face-p face)
                  do (loop for i from (max 0 start) below (min end (length string))
                           do (let* ((code (char-code (char string i)))
                                     (forms (gethash code *arabic-forms*)))
                                (when forms
                                  (let* ((own (arabic-joining code))
                                         (before (and (member own '(:dual :right))
                                                      (member (joining i start end 1) '(:dual :causing))))
                                         (after (and (member own '(:dual :causing))
                                                     (member (joining i start end -1) '(:dual :right :causing))))
                                         (form (or (svref forms (cond ((and before after) 3) (before 1) (after 2) (t 0)))
                                                   (svref forms 0))))
                                    (unless shaped (setf shaped (copy-seq string)))
                                    (setf (char shaped i) (code-char form))))))))
        (or shaped string))))
