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

(defun matching-isolates (classes start end)
  "A vector, as long as CLASSES, giving for each isolate's beginning between
START and END (:LRI, :RLI, :FSI) the index of the :PDI that ends it, and for
that :PDI the index of its beginning; -1 where there is none. A paragraph's
end (:B) ends every isolate."
  (let ((match (make-array (length classes) :element-type 'fixnum :initial-element -1))
        (open '()))
    (loop for i from start below end
          for class = (svref classes i)
          do (cond ((isolate-initiator-p class) (push i open))
                   ((and (eq class :pdi) open)
                    (let ((j (pop open))) (setf (aref match j) i (aref match i) j)))
                   ((eq class :b) (setf open '()))))
    match))

(defun first-strong-level (classes start end)
  "The level of a paragraph of CLASSES from START below END as its first
letter says, the isolates passed over: 0 for :L, 1 for :R or :AL, NIL when
there is none before END or the paragraph's end (:B)."
  (let ((depth 0))
    (loop for i from start below end
          for class = (svref classes i)
          do (cond ((isolate-initiator-p class) (incf depth))
                   ((eq class :pdi) (when (plusp depth) (decf depth)))
                   ((eq class :b) (return nil))
                   ((plusp depth))
                   ((eq class :l) (return 0))
                   ((member class '(:r :al)) (return 1))))))

(defun explicit-levels (classes types levels match paragraph-level)
  "Give each character of CLASSES its embedding level in LEVELS as the
embeddings, overrides and isolates around it say, and its class in TYPES as
an override changes it (rules X1 to X8)."
  (let ((stack-level (make-array (+ 2 +bidi-max-depth+) :element-type 'fixnum))
        (stack-override (make-array (+ 2 +bidi-max-depth+) :initial-element nil))
        (stack-isolate (make-array (+ 2 +bidi-max-depth+) :initial-element nil))
        (depth 0) (overflow-isolates 0) (overflow-embeddings 0) (valid-isolates 0)
        (n (length classes)))
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

(defun isolating-run-sequences (classes levels match start end)
  "The isolating run sequences of the characters from START below END, each a
vector of their indices in order: runs of one level, those removed from the
working out (REMOVED-CLASS-P) left out, an isolate's beginning joined to the
run its end begins (rule X10)."
  (let ((runs '()) (run '()) (run-level -1))
    ;; the runs of one level, each a list of indices in order
    (loop for i from start below end
          unless (removed-class-p (svref classes i))
            do (if (and run (= (aref levels i) run-level))
                   (push i run)
                   (progn (when run (push (nreverse run) runs))
                          (setf run (list i) run-level (aref levels i)))))
    (when run (push (nreverse run) runs))
    (let* ((runs (coerce (nreverse runs) 'simple-vector))
           (run-of (make-hash-table)))
      (loop for r across runs for k from 0 do (setf (gethash (first r) run-of) k))
      (loop for r across runs
            unless (let ((first (first r)))
                     (and (eq (svref classes first) :pdi) (>= (aref match first) 0)))
              collect (let ((indices '()) (current r))
                        (loop
                          (setf indices (revappend current indices))
                          (let ((last (car (last current))))
                            (if (and (isolate-initiator-p (svref classes last)) (>= (aref match last) 0)
                                     (gethash (aref match last) run-of))
                                (setf current (svref runs (gethash (aref match last) run-of)))
                                (return))))
                        (coerce (nreverse indices) 'simple-vector))))))

(defun direction-of-level (level)
  "The direction of embedding LEVEL: :R when it is odd, :L when it is even."
  (if (oddp level) :r :l))

(defun level-beside (classes levels i step start end paragraph-level)
  "The level of the first character from I on, going by STEP (1 or -1) and
before END or from START, that takes part in working out the levels; the
paragraph's level when there is none."
  (loop for j = (+ i step) then (+ j step)
        while (and (<= start j) (< j end))
        unless (removed-class-p (svref classes j))
          do (return (aref levels j))
        finally (return paragraph-level)))

(defun resolve-weak-types (ts sos)
  "Resolve the weak classes of TS, the classes of an isolating run sequence
in order, in place: marks, digits and their separators and terminators
\(rules W1 to W7), SOS being the direction before the sequence."
  (let ((n (length ts)))
    ;; W1: a mark takes the class of what it is on
    (dotimes (k n)
      (when (eq (svref ts k) :nsm)
        (setf (svref ts k) (cond ((zerop k) sos)
                                 ((member (svref ts (1- k)) '(:lri :rli :fsi :pdi)) :on)
                                 (t (svref ts (1- k)))))))
    ;; W2: a European digit after an Arabic letter is an Arabic one; W3: an Arabic letter is right to left
    (let ((strong sos))
      (dotimes (k n)
        (case (svref ts k)
          ((:l :r :al) (setf strong (svref ts k)))
          (:en (when (eq strong :al) (setf (svref ts k) :an))))))
    (dotimes (k n) (when (eq (svref ts k) :al) (setf (svref ts k) :r)))
    ;; W4: one separator between two digits of a kind
    (loop for k from 1 below (1- n)
          do (let ((before (svref ts (1- k))) (after (svref ts (1+ k))))
               (case (svref ts k)
                 (:es (when (and (eq before :en) (eq after :en)) (setf (svref ts k) :en)))
                 (:cs (cond ((and (eq before :en) (eq after :en)) (setf (svref ts k) :en))
                            ((and (eq before :an) (eq after :an)) (setf (svref ts k) :an)))))))
    ;; W5: terminators beside a European digit; W6: separators and terminators left are neutral
    (let ((k 0))
      (loop while (< k n)
            do (if (eq (svref ts k) :et)
                   (let ((j (or (position-if (lambda (class) (not (eq class :et))) ts :start k) n)))
                     (when (or (and (plusp k) (eq (svref ts (1- k)) :en)) (and (< j n) (eq (svref ts j) :en)))
                       (fill ts :en :start k :end j))
                     (setf k j))
                   (incf k))))
    (dotimes (k n) (when (member (svref ts k) '(:es :et :cs)) (setf (svref ts k) :on)))
    ;; W7: a European digit after a left-to-right letter is left to right
    (let ((strong sos))
      (dotimes (k n)
        (case (svref ts k)
          ((:l :r) (setf strong (svref ts k)))
          (:en (when (eq strong :l) (setf (svref ts k) :l))))))
    ts))

(defun resolve-brackets (ts originals codes sos level)
  "Give the pairs of brackets of TS, the classes of an isolating run sequence
at LEVEL, the direction of what they hold or stand among (rule N0): CODES
their characters' codes, ORIGINALS their classes as written, SOS the
direction before the sequence."
  (let ((n (length ts)) (open '()) (depth 0) (pairs '()) (embedding (direction-of-level level)))
    (block pairing
      (dotimes (k n)
        (when (eq (svref ts k) :on)
          (multiple-value-bind (other kind) (bracket-pair (aref codes k))
            (case kind
              (:open (when (= depth +bracket-pairs-most+)
                       (setf pairs '())
                       (return-from pairing))
                     (push (cons other k) open)
                     (incf depth))
              (:close (let* ((code (case (aref codes k) (#x2329 #x3008) (#x232A #x3009) (t (aref codes k))))
                             (at (position code open :key #'car)))
                        (when at
                          (push (cons (cdr (nth at open)) k) pairs)
                          (setf open (nthcdr (1+ at) open))
                          (decf depth (1+ at))))))))))
    (flet ((set-pair (pair direction)
             (dolist (k (list (car pair) (cdr pair)))
               (setf (svref ts k) direction)
               ;; marks written after a bracket go with it
               (loop for j from (1+ k) below n
                     while (eq (svref originals j) :nsm)
                     do (setf (svref ts j) direction)))))
      (dolist (pair (sort pairs #'< :key #'car))
        (let ((inside (loop with found = nil
                            for k from (1+ (car pair)) below (cdr pair)
                            for strong = (strong-direction (svref ts k))
                            do (cond ((eq strong embedding) (return embedding))
                                     (strong (setf found strong)))
                            finally (return found))))
          (cond ((null inside))
                ((eq inside embedding) (set-pair pair embedding))
                (t (let ((before (or (loop for k downfrom (1- (car pair)) to 0
                                           for strong = (strong-direction (svref ts k))
                                           when strong return strong)
                                     sos)))
                     (set-pair pair (if (eq before inside) inside embedding))))))))
    ts))

(defun resolve-neutrals (ts sos eos level)
  "Give the neutrals of TS, the classes of an isolating run sequence at LEVEL,
the direction of the text around them when it is the same on both sides,
else that of LEVEL (rules N1 and N2); SOS and EOS are the directions before
and after the sequence."
  (let ((n (length ts)) (k 0))
    (flet ((neutral-p (class) (member class '(:b :s :ws :on :lri :rli :fsi :pdi))))
      (loop while (< k n)
            do (if (neutral-p (svref ts k))
                   (let* ((j (or (position-if-not #'neutral-p ts :start k) n))
                          (before (if (zerop k) sos (strong-direction (svref ts (1- k)))))
                          (after (if (= j n) eos (strong-direction (svref ts j)))))
                     (fill ts (if (eq before after) before (direction-of-level level)) :start k :end j)
                     (setf k j))
                   (incf k))))
    ts))

(defun sequence-sides (sequence classes levels paragraph-level)
  "The directions before and after SEQUENCE, an isolating run sequence, as
two values (sos and eos): each that of the higher of its level and the
level beside it, the paragraph's beside an isolate's beginning left open.
LEVELS are the levels the embeddings give, before any is resolved."
  (let* ((n (length classes))
         (first (svref sequence 0))
         (last (svref sequence (1- (length sequence))))
         (level (aref levels first)))
    (values (direction-of-level (max level (level-beside classes levels first -1 0 n paragraph-level)))
            (direction-of-level (max level (if (isolate-initiator-p (svref classes last))
                                               paragraph-level
                                               (level-beside classes levels last 1 0 n paragraph-level)))))))

(defun bidi-levels (classes &key paragraph-level codes)
  "The embedding level of each character of a paragraph whose bidirectional
classes (CODE-BIDI-CLASS) are CLASSES, a simple vector, as the Unicode
Bidirectional Algorithm resolves them, as a vector of (UNSIGNED-BYTE 8): an
odd level is shown from right to left. PARAGRAPH-LEVEL is 0 for a paragraph
written from left to right, 1 from right to left, or NIL to take it from its
first letter (0 when it has none). CODES, a vector of the characters' codes
when given, pairs the brackets. A character removed from the working out (an
embedding, an override or their end, a boundary neutral) takes the level of
the one before it, an end of embedding that of the one after it. The level
of the paragraph is the second value. Complexity: O(n) for a paragraph of n
characters, but for brackets, O(n × 63)."
  (let* ((n (length classes))
         (paragraph-level (or paragraph-level (first-strong-level classes 0 n) 0))
         (types (copy-seq classes))
         (levels (make-array n :element-type '(unsigned-byte 8) :initial-element paragraph-level))
         (match (matching-isolates classes 0 n)))
    (explicit-levels classes types levels match paragraph-level)
    (let ((sequences (loop with explicit = (copy-seq levels)
                           for sequence in (isolating-run-sequences classes levels match 0 n)
                           collect (multiple-value-call #'list sequence
                                     (sequence-sides sequence classes explicit paragraph-level)))))
      (loop for (sequence sos eos) in sequences
            do (let ((level (aref levels (svref sequence 0)))
                     (ts (map 'simple-vector (lambda (i) (svref types i)) sequence)))
                 (resolve-weak-types ts sos)
                 (when codes
                   (resolve-brackets ts (map 'simple-vector (lambda (i) (svref classes i)) sequence)
                                     (map 'vector (lambda (i) (aref codes i)) sequence) sos level))
                 (resolve-neutrals ts sos eos level)
                 ;; I1 and I2: each character's level from its direction
                 (loop for i across sequence
                       for type across ts
                       do (let ((at (aref levels i)))
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
                    (level-beside classes levels i 1 0 n paragraph-level)
                    (if (plusp i) (aref levels (1- i)) paragraph-level))))))
    (values levels paragraph-level)))

(defun line-levels (classes levels paragraph-level)
  "The levels of the characters of one line, of CLASSES at LEVELS (a simple
vector and a vector of their levels), as rule L1 sets them for showing: a tab
\(:S) or a paragraph's end (:B), the blanks and isolates before it, and those
at the line's end, at PARAGRAPH-LEVEL. Returns a new vector of (UNSIGNED-BYTE
8)."
  (let* ((n (length classes))
         (line (make-array n :element-type '(unsigned-byte 8)))
         (trailing t))
    (loop for i downfrom (1- n) to 0
          for class = (svref classes i)
          do (cond ((member class '(:s :b)) (setf trailing t (aref line i) paragraph-level))
                   ((and trailing (or (member class '(:ws :lri :rli :fsi :pdi)) (removed-class-p class)))
                    (setf (aref line i) paragraph-level))
                   (t (setf trailing nil (aref line i) (aref levels i)))))
    line))

(defun visual-order (levels)
  "The order a line of characters at LEVELS, a vector of their levels as
LINE-LEVELS gives them, is shown in from left to right, as a vector of their
indices: from the highest level to the lowest odd one, each run at that level
or above turned round (rule L2). Complexity: O(n × levels)."
  (let* ((n (length levels))
         (order (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref order i) i))
    (when (plusp n)
      (let ((highest (reduce #'max levels))
            (lowest-odd (let ((lowest (reduce #'min levels))) (if (oddp lowest) lowest (1+ lowest)))))
        (loop for level downfrom highest to lowest-odd
              do (let ((i 0))
                   (loop while (< i n)
                         do (if (>= (aref levels (aref order i)) level)
                                (let ((j (or (position-if (lambda (k) (< (aref levels k) level)) order :start i) n)))
                                  (setf (subseq order i j) (reverse (subseq order i j)))
                                  (setf i j))
                                (incf i)))))))
    order))
