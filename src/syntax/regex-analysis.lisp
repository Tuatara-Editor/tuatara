;;;; src/syntax/regex-analysis.lisp --- What a pattern's matches hold, found from its parse tree before reading text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The fixed text or the run of places every match holds, whether a match can
;;;; cross a line, and how far before and after a range a match may reach, so
;;;; that a scan can jump to the text it needs and decode no more than it must
;;;; (REGEX-TRAITS).

(in-package #:tuatara.syntax)

;;; Regular expressions
;;;
;;; A pattern is matched against windows of decoded text: each window starts
;;; +REGEX-CONTEXT+ octets before the range it answers for, so ^, $, \b and
;;; short lookbehinds see the neighbouring characters. It ends after the line
;;; holding the range's last start when the pattern cannot match across lines
;;; (most can't), else +REGEX-OVERLAP+ octets past the range, growing to twice
;;; its size while a match reaches its end, up to *REGEX-LONGEST-MATCH* octets.
;;; When every match holds some fixed text, the scan jumps to it at literal
;;; speed. Invalid UTF-8 decodes byte by byte as raw bytes, U+DC80 to U+DCFF,
;;; so that a pattern can name them (\xA9) and they match only themselves.

(defconstant +regex-context+ 64
  "Octets of text before a regex window's range decoded with it, at the
least, so that ^ and \b see the characters before the range; a pattern with
a longer lookbehind has as many as it looks back (REGEX-TRAITS-CONTEXT).")

(defvar *regex-longest-match* (* 1024 1024)
  "Octets a regular-expression window may grow to so that a long match is found
whole; a match longer than this is cut short, but for a block that a filter
reads on past the window, Base32 or Base64 (TEXT-READER).")

(defun literal-runs (tree)
  "Three values for a cl-ppcre parse TREE: the text of every match when TREE
matches exactly one text (\"\" for an assertion, which matches none), else NIL;
a list of texts each of which every match contains; and the text every match
starts with, or NIL. A newline is no known text, since it matches a line end
of either kind (LINE-TREE). Forms not understood contribute nothing, which is
always safe."
  (flet ((zero-width-p (x)
           (or (member x '(:start-anchor :end-anchor :modeless-start-anchor :modeless-end-anchor
                           :modeless-end-anchor-no-newline :word-boundary :non-word-boundary :void))
               (and (consp x) (member (first x) '(:positive-lookahead :negative-lookahead
                                                  :positive-lookbehind :negative-lookbehind))))))
    (cond ((and (stringp tree) (find #\Newline tree))
           ;; a newline matches a line end of one or two octets: no text of it is known
           (literal-runs (cons :sequence (coerce tree 'list))))
          ((stringp tree) (values tree (list tree) tree))
          ((eql tree #\Newline) (values nil '() nil))
          ((characterp tree) (let ((s (string tree))) (values s (list s) s)))
          ((zero-width-p tree) (values "" '() ""))
          ((atom tree) (values nil '() nil))
          (t (case (first tree)
               ((:register :group :standalone) (literal-runs (car (last tree))))
               (:named-register
                (if (uiop:string-prefix-p *filter-mark* (second tree))
                    ;; text a filter reads: nothing known of it
                    (values nil '() nil)
                    (literal-runs (third tree))))
               (:sequence
                (let ((whole "") (runs '()) (current "") (prefix "") (prefix-open t))
                  (dolist (item (rest tree))
                    (multiple-value-bind (w r p) (literal-runs item)
                      (cond (w (setf current (concatenate 'string current w))
                               (when whole (setf whole (concatenate 'string whole w)))
                               (when prefix-open (setf prefix (concatenate 'string prefix w))))
                            (t (when (plusp (length current)) (push current runs))
                               (when prefix-open
                                 (setf prefix (concatenate 'string prefix (or p "")) prefix-open nil))
                               (setf current "" whole nil runs (append r runs))))))
                  (when (plusp (length current)) (push current runs))
                  (values whole runs prefix)))
               ((:greedy-repetition :non-greedy-repetition)
                (destructuring-bind (min max item) (rest tree)
                  (if (and (integerp min) (plusp min))
                      (multiple-value-bind (w r p) (literal-runs item)
                        (cond ((and w (eql min max))
                               (let ((text (apply #'concatenate 'string (make-list min :initial-element w))))
                                 (values text (list text) text)))
                              (w (values nil (list w) w))
                              (t (values nil r p))))
                      (values nil '() nil))))
               (t (values nil '() nil)))))))

(defun required-literal (tree)
  "Two values: the text every match of the cl-ppcre parse TREE contains, or
NIL when there is none, the pattern sets its own flags or the text holds raw
bytes' characters; and true when every match starts with that text. A prefix
of three octets or more, or as long as the longest text, is preferred."
  (labels ((has-flags (x) (or (eq x :flags) (and (consp x) (or (has-flags (car x)) (has-flags (cdr x))))))
           (octets (text) (tuatara.text:string-octet-length text)))
    (unless (has-flags tree)
      (multiple-value-bind (whole runs prefix) (literal-runs tree)
        (declare (ignore whole))
        (let ((best nil) (best-length 0))
          (dolist (run runs)
            (when (> (octets run) best-length) (setf best run best-length (octets run))))
          (multiple-value-bind (text prefix-p)
              (if (and prefix (plusp (length prefix)) (>= (octets prefix) (min 3 best-length)))
                  (values prefix t)
                  (values best nil))
            (when (and text (notany (lambda (c) (tuatara.text:raw-byte-code-p (char-code c))) text))
              (values text prefix-p))))))))

;;; Shapes: what every match holds when no text does, a run of places each of
;;; which is one of a few octets ([0-9A-Fa-f]{8}- in a UUID), found at the speed
;;; of a loop over the octets rather than by trying the pattern everywhere.

(defun octet-set (&rest codes)
  "A set of octets, a bit for each of the 256, holding CODES."
  (let ((set (make-array 256 :element-type 'bit :initial-element 0)))
    (dolist (code codes set) (setf (sbit set code) 1))))

(sb-ext:defglobal **odd-cases** nil
  "The letters that are neither the lower case they have nor its upper case,
as the title case letters ǅ and ǈ, by that lower case, once looked for.")

(defun odd-cases (char)
  "The letters whose lower case is CHAR that are neither CHAR nor its upper
case, as ǅ is for ǆ: found once for every character."
  (gethash char (or **odd-cases**
                    (setf **odd-cases**
                          (let ((table (make-hash-table)))
                            (dotimes (code char-code-limit table)
                              (let* ((c (code-char code)) (down (and c (char-downcase c))))
                                (unless (or (null c) (char= c down) (char= c (char-upcase down)))
                                  (push c (gethash down table))))))))))

(defun case-octet-sets (char)
  "The sets of octets (OCTET-SET) that CHAR and the characters it matches
ignoring case are written in, one for each octet, when all of them take as
many octets; else NIL. Those characters are the ones CHAR-EQUAL takes as the
same letter: its upper and lower case, theirs, and the title case letters of
its lower case (ODD-CASES)."
  (let* ((cases (list* char (char-upcase char) (char-downcase char)
                       (char-upcase (char-downcase char)) (char-downcase (char-upcase char))
                       (append (odd-cases (char-downcase char)) (odd-cases (char-downcase (char-upcase char))))))
         (forms (mapcar (lambda (c) (tuatara.text:string-octets (string c)))
                        (remove-duplicates (remove-if-not (lambda (c) (char-equal c char)) cases))))
         (n (length (first forms))))
    (when (every (lambda (form) (= (length form) n)) forms)
      (loop for k below n
            collect (apply #'octet-set (mapcar (lambda (form) (aref form k)) forms))))))

(defun char-octet-sets (char case-fold)
  "The sets of octets (OCTET-SET) CHAR is written in, one for each of its
octets, the octets of its other case in them when CASE-FOLD
\(CASE-OCTET-SETS). NIL when CHAR cannot be told by its octets. That is a
newline, which matches a carriage return and line feed (CR LF) too, a raw
octet's character, a letter whose cases are written in different numbers of
octets, or with CASE-FOLD a character past ASCII that an ASCII letter matches
too (FOLDS-WITH-ASCII-P)."
  (let ((code (char-code char)))
    (cond ((or (char= char #\Newline) (tuatara.text:raw-byte-code-p code)) nil)
          ((and case-fold (folds-with-ascii-p char)) nil)
          ((and case-fold (both-case-p char)) (case-octet-sets char))
          (t (map 'list #'octet-set (tuatara.text:string-octets (string char)))))))

(defun class-octet-set (items case-fold)
  "The set of octets (OCTET-SET) a character class of ITEMS (a cl-ppcre parse
tree's) matches, when all it matches is ASCII other than a newline; else NIL."
  (let ((set (octet-set)))
    (flet ((add (char)
             (let ((sets (char-octet-sets char case-fold)))
               (unless (and sets (null (rest sets)) (< (char-code char) 128))
                 (return-from class-octet-set nil))
               (bit-ior set (first sets) set))))
      (dolist (item items set)
        (cond ((characterp item) (add item))
              ((and (consp item) (eq (first item) :range) (< (char-code (third item)) 128))
               (loop for code from (char-code (second item)) to (char-code (third item))
                     do (add (code-char code))))
              (t (return nil)))))))

(defun shapes-union (shapes)
  "One shape from SHAPES, each a list of octet sets: at each place, as far as
the shortest, every octet any of them allows there."
  (let ((length (reduce #'min shapes :key #'length)))
    (loop for k below length
          collect (let ((set (octet-set)))
                    (dolist (shape shapes set) (bit-ior set (nth k shape) set))))))

(defun shape-runs (tree case-fold)
  "Three values for a cl-ppcre parse TREE, as LITERAL-RUNS gives them for
text, each shape a list of octet sets, a set for each octet (CHAR-OCTET-SETS).
The first is the shape of every match when every match is of one length and
shape, else :NONE. The second is a list of shapes each of which every match
holds. The third is the shape every match starts with, perhaps empty. A
positive lookahead at the start gives the shape the text there starts with.
Forms not understood contribute nothing, which is always safe."
  (flet ((zero-width-p (x)
           (or (member x '(:start-anchor :end-anchor :modeless-start-anchor :modeless-end-anchor
                           :modeless-end-anchor-no-newline :word-boundary :non-word-boundary :void))
               (and (consp x) (member (first x) '(:negative-lookahead :positive-lookbehind :negative-lookbehind)))))
         (fixed (sets) (if sets (values sets (list sets) sets) (values :none '() '()))))
    (cond ((characterp tree) (fixed (char-octet-sets tree case-fold)))
          ((stringp tree) (shape-runs (cons :sequence (coerce tree 'list)) case-fold))
          ((zero-width-p tree) (values '() '() '()))
          ((atom tree) (values :none '() '()))
          (t (case (first tree)
               (:char-class (let ((set (class-octet-set (rest tree) case-fold)))
                              (if set (fixed (list set)) (values :none '() '()))))
               ((:register :group :standalone) (shape-runs (car (last tree)) case-fold))
               (:named-register
                (if (uiop:string-prefix-p *filter-mark* (second tree))
                    (values :none '() '())
                    (shape-runs (third tree) case-fold)))
               (:positive-lookahead
                ;; no text of the match, but what the text from here starts with
                (multiple-value-bind (w r p) (shape-runs (second tree) case-fold)
                  (declare (ignore w r))
                  (values '() (and p (list p)) '() p)))
               (:sequence
                (let ((whole '()) (runs '()) (current '()) (prefix '()) (prefix-open t) (ahead nil))
                  (dolist (item (rest tree))
                    (multiple-value-bind (w r p looked) (shape-runs item case-fold)
                      (when (and looked prefix-open (null prefix) (null ahead)) (setf ahead looked))
                      (cond ((listp w)
                             (setf current (append current w))
                             (unless (eq whole :none) (setf whole (append whole w)))
                             (when prefix-open (setf prefix (append prefix w)))
                             (setf runs (append r runs)))
                            (t (when current (push current runs))
                               (when prefix-open (setf prefix (append prefix p) prefix-open nil))
                               (setf current '() whole :none runs (append r runs))))))
                  (when current (push current runs))
                  (values whole runs (if (and ahead (> (shape-bits ahead) (shape-bits prefix))) ahead prefix))))
               (:alternation
                (let ((branches (mapcar (lambda (branch) (multiple-value-list (shape-runs branch case-fold)))
                                        (rest tree))))
                  (let ((wholes (mapcar #'first branches)) (prefixes (mapcar #'third branches)))
                    (let ((whole (if (and (notany (lambda (w) (eq w :none)) wholes)
                                          (= 1 (length (remove-duplicates (mapcar #'length wholes)))))
                                     (shapes-union wholes)
                                     :none))
                          (prefix (if (every #'identity prefixes) (shapes-union prefixes) '())))
                      (values whole (and prefix (list prefix)) prefix)))))
               ((:greedy-repetition :non-greedy-repetition)
                (destructuring-bind (min max item) (rest tree)
                  (if (and (integerp min) (plusp min))
                      (multiple-value-bind (w r p) (shape-runs item case-fold)
                        (cond ((and (listp w) w (eql min max))
                               (fixed (loop repeat min append w)))
                              ((and (listp w) w)
                               (let ((least (loop repeat min append w)))
                                 (values :none (list least) least)))
                              (t (values :none r p))))
                      (values :none '() '()))))
               (t (values :none '() '())))))))

(defun shape-bits (shape)
  "How much SHAPE, a list of octet sets, tells of a text: for each place, the
bits of the octets it rules out, eight for one octet allowed."
  (loop for set in shape sum (log (/ 256 (max 1 (count 1 set))) 2)))

(defun required-shape (tree case-fold)
  "Two values: the shape (SHAPE-RUNS) every match of the parse TREE holds that
tells most of a text, as a vector of octet sets, or NIL; and true when every
match starts with it, or the text at a match's start does. A prefix telling
nearly as much is preferred."
  (multiple-value-bind (whole runs prefix) (shape-runs tree case-fold)
    (declare (ignore whole))
    (let ((best (first (sort (copy-list runs) #'> :key #'shape-bits))))
      (multiple-value-bind (shape prefix-p)
          (if (and prefix (>= (shape-bits prefix) (* 3/4 (if best (shape-bits best) 0))))
              (values prefix t)
              (values best nil))
        (when shape (values (coerce shape 'simple-vector) prefix-p))))))

(defun class-item-covers (item char surely)
  "Whether the char-class ITEM of a parse tree includes CHAR, a character that
ends a line (the newline or the carriage return). Items not understood count
as including it unless SURELY is true."
  (cond ((characterp item) (char= item char))
        ((and (consp item) (eq (first item) :range)) (char<= (second item) char (third item)))
        ((member item '(:whitespace-char-class :non-digit-class :non-word-char-class)) t)
        ((member item '(:digit-class :word-char-class :non-whitespace-char-class)) nil)
        (t (not surely))))

(defparameter *filters-crossing-lines* '("base32" "base64")
  "The filters a named pattern may hold (*PATTERN-FILTERS*) whose matches
may go on over lines.")

(defun filter-crosses-lines-p (name)
  "True when NAME, a group's name in a parse tree, marks a filter whose
matches may go on over lines (*FILTERS-CROSSING-LINES*)."
  (and (stringp name) (uiop:string-prefix-p *filter-mark* name)
       (member (subseq name (length *filter-mark*)) *filters-crossing-lines* :test #'string=)
       t))

(defun crosses-lines-p (tree)
  "True unless every match of the cl-ppcre parse TREE, and all its lookaheads
inspect, surely lies within one line (with . matching neither a newline nor
a carriage return, as COMPILE-REGEX makes it). Forms not understood count as
crossing."
  (labels ((walk (x)
             (cond ((stringp x) (or (find #\Newline x) (find #\Return x)))
                   ((characterp x) (or (char= x #\Newline) (char= x #\Return)))
                   ((member x '(:start-anchor :end-anchor :modeless-start-anchor :modeless-end-anchor
                                :modeless-end-anchor-no-newline :word-boundary :non-word-boundary :void
                                :everything :digit-class :word-char-class :non-whitespace-char-class))
                    nil)
                   ((atom x) t)
                   (t (case (first x)
                        ((:sequence :alternation) (some #'walk (rest x)))
                        ((:group :register :standalone :positive-lookahead :negative-lookahead) (walk (car (last x))))
                        (:named-register (or (filter-crosses-lines-p (second x)) (walk (third x))))
                        ((:greedy-repetition :non-greedy-repetition) (walk (fourth x)))
                        ((:positive-lookbehind :negative-lookbehind) nil) ; before the match, in the context
                        (:property (or (funcall (character-property (second x)) #\Newline)
                                       (funcall (character-property (second x)) #\Return)))
                        (:inverted-property (not (and (funcall (character-property (second x)) #\Newline)
                                                      (funcall (character-property (second x)) #\Return))))
                        (:char-class (some (lambda (item) (or (class-item-covers item #\Newline nil)
                                                              (class-item-covers item #\Return nil)))
                                           (rest x)))
                        (:inverted-char-class
                         (notevery (lambda (char) (some (lambda (item) (class-item-covers item char t)) (rest x)))
                                   '(#\Newline #\Return)))
                        (t t))))))
    (and (walk tree) t)))

(defun line-alone-p (tree)
  "True when a match of the parse TREE, one that lies within a line
\(CROSSES-LINES-P says so), depends on nothing outside that line: it has no
lookbehind, which may look into the line before, and none of \\A, \\z and
\\Z, which tell where the text begins and ends. Such a pattern finds the
same in a file whatever stands before and after it."
  (not (tree-has tree :positive-lookbehind :negative-lookbehind :modeless-start-anchor :modeless-end-anchor
                 :modeless-end-anchor-no-newline)))

(defun tree-has (tree &rest keywords)
  "True when one of KEYWORDS occurs anywhere in the parse TREE."
  (labels ((walk (x) (or (and (symbolp x) (member x keywords)) (and (consp x) (or (walk (car x)) (walk (cdr x)))))))
    (and (walk tree) t)))

(defun end-anchored-p (tree)
  "True when every match of the parse TREE ends at the end of the text: it
ends in \\z or \\Z, outside any repeat or lookaround, or each of its
choices does."
  (labels ((walk (x)
             (cond ((member x '(:modeless-end-anchor :modeless-end-anchor-no-newline)) t)
                   ((atom x) nil)
                   (t (case (first x)
                        (:sequence (walk (car (last x))))
                        (:alternation (every #'walk (rest x)))
                        ((:group :register :named-register) (walk (car (last x))))
                        (t nil))))))
    (walk tree)))

(define-record (regex-traits (:constructor make-regex-traits
                                          (&key needle shape fold variants narrow prefix line-bound alone end-anchored
                                             reach context plain))
                            (:copier nil))
  "What COMPILE-REGEX learnt about a pattern for scanning a rope. NEEDLE is the
octets of text every match holds (ASCII case folded when FOLD), or NIL. SHAPE,
when it tells more than NEEDLE, is a vector of octet sets (REQUIRED-SHAPE)
every match holds in its place, or NIL. VARIANTS are the octets of the
characters past ASCII that a letter of the pattern is one with when case is
ignored (FOLD-VARIANTS): a match may hold one in place of a letter of NEEDLE or
SHAPE, so where one stands is looked at as where they stand is. NARROW, for
such a pattern, is a cons of its scanners without those characters, the one for
any text and the one for text without a carriage return (or NIL when they are
the same): a window that is all ASCII is searched with them. There are none
when a character of the pattern past ASCII is one with an ASCII letter
\(FOLDS-WITH-ASCII-P), as such a window may then hold what only the pattern
folded finds. PREFIX is true when every match starts with the one or the other.
LINE-BOUND is true when every match lies within a line. ALONE is true when,
more than that, no match depends on anything outside its line (LINE-ALONE-P).
END-ANCHORED is true when the pattern uses \\z or \\Z, which only match at the
end of the text. REACH is the octets past the stretch a match starts in that a
scan looks, +REGEX-OVERLAP+ or for a thorough scanner *REGEX-LONGEST-MATCH*.
CONTEXT is the octets before a window's range decoded with it, enough for the
longest lookbehind. PLAIN is the scanner for text without a carriage return, or
NIL when it is the same."
  (needle nil :documentation "The octets of text every match of the pattern holds, or NIL.")
  (shape nil :documentation "The octet sets every match of the pattern holds
in their place, or NIL.")
  (fold nil :documentation "True when the needle is ASCII case folded and is looked for so.")
  (variants nil :documentation "The octets of each character past ASCII a letter of
the pattern is one with when case is ignored, a list of octet vectors.")
  (narrow nil :documentation "The pattern's scanners for text that is all
ASCII, a cons of the one for any line ends and the one for none with a
carriage return, or NIL: when no letter of it folds with a character past
ASCII, and when a character of it past ASCII folds with an ASCII letter.")
  (prefix nil :documentation "True when every match of the pattern starts
with its needle or shape.")
  (line-bound nil :documentation "True when every match of the pattern lies within a line.")
  (alone nil :documentation "True when no match of the pattern depends on
anything outside the line it lies on.")
  (end-anchored nil :documentation "True when the pattern matches only at
the end of the text (\\z, \\Z).")
  (reach +regex-overlap+ :documentation "Octets past the stretch a match starts in that
a scan for the pattern looks.")
  (context +regex-context+ :documentation "Octets before a window's range a scan for the
pattern decodes with it, for its lookbehinds to see.")
  (plain nil :documentation "The pattern's scanner for text without a
carriage return, or NIL when it is the same."))

(setf (documentation 'make-regex-traits 'function) "Return the REGEX-TRAITS of a pattern from what its keys say.")

(defvar *regex-thorough* nil
  "True while a thorough command runs (regex-search-forward-thorough and the
others): the scanners COMPILE-REGEX makes then find every match of up to
*REGEX-LONGEST-MATCH* octets whole, however many lines it spans, where the
others may miss one that needs text more than +REGEX-OVERLAP+ octets past the
stretch of +SEARCH-CHUNK+ it starts in. About twice as slow for a pattern whose
matches may cross lines; the same for one kept within a line.")

(sb-ext:defglobal **regex-traits** (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The REGEX-TRAITS of each scanner COMPILE-REGEX made.")

(defun line-tree (tree)
  "TREE, a cl-ppcre parse tree, with its lines ending as every text's do
\(TUATARA.TEXT:LINE-END-OCTET-P), as the Rust regex crate reads a pattern in
its carriage return and line feed (CRLF) mode. ^ matches after a newline and
after a carriage return no newline follows. $ matches before a newline no
carriage return comes before and before a carriage return, neither between a
carriage return and its newline. A newline in the pattern outside a set (\n)
matches one line end as $ sees it: a carriage return and its newline
together, a newline, or a carriage return no newline follows; in a lookbehind,
which looks back over a fixed length, a newline or a carriage return no
newline follows. In a set it is the newline alone. . matches neither a
carriage return nor a newline, but in single-line mode ((?s)). Where (?-m)
makes ^ and $ the text's ends, they are left as they are. Plain text stays
plain: a tree that is a string alone is given as a sequence."
  (labels ((mode-after (flags mode on off)
             (cond ((member on flags) t)
                   ((member off flags) nil)
                   (t mode)))
           (walk (node single multi behind)
             (cond ((and multi (eq node :end-anchor))
                    '(:alternation (:positive-lookahead #\Return)
                      (:sequence (:negative-lookbehind #\Return) (:positive-lookahead #\Newline))
                      :modeless-end-anchor-no-newline))
                   ((and multi (eq node :start-anchor))
                    '(:alternation :modeless-start-anchor (:positive-lookbehind #\Newline)
                      (:sequence (:positive-lookbehind #\Return) (:negative-lookahead #\Newline))))
                   ((eq node :everything) (if single node '(:inverted-char-class #\Return #\Newline)))
                   ((eql node #\Newline)
                    (if behind
                        '(:alternation #\Newline (:sequence #\Return (:negative-lookahead #\Newline)))
                        '(:alternation (:sequence #\Return #\Newline) #\Newline
                          (:sequence #\Return (:negative-lookahead #\Newline)))))
                   ((and (stringp node) (find #\Newline node))
                    (walk (cons :sequence (coerce node 'list)) single multi behind))
                   ((atom node) node)
                   ((eq (first node) :sequence)
                    ;; inline flags change the mode for the rest of the sequence
                    (let ((single single) (multi multi))
                      (cons :sequence
                            (loop for child in (rest node)
                                  collect (if (and (consp child) (eq (first child) :flags))
                                              (progn (setf single (mode-after (rest child) single :single-line-mode-p
                                                                              :not-single-line-mode-p)
                                                           multi (mode-after (rest child) multi :multi-line-mode-p
                                                                             :not-multi-line-mode-p))
                                                     child)
                                              (walk child single multi behind))))))
                   ((eq (first node) :group)
                    (let ((flags (butlast (rest node))))
                      (append (list :group) flags
                              (list (walk (car (last node))
                                          (mode-after flags single :single-line-mode-p :not-single-line-mode-p)
                                          (mode-after flags multi :multi-line-mode-p :not-multi-line-mode-p)
                                          behind)))))
                   ((member (first node) '(:positive-lookbehind :negative-lookbehind))
                    (list (first node) (walk (second node) single multi t)))
                   ((member (first node)
                            '(:char-class :inverted-char-class :property :inverted-property :back-reference :filter))
                    node)
                   (t (cons (first node) (mapcar (lambda (child) (walk child single multi behind)) (rest node)))))))
    (let ((walked (walk tree nil t nil)))
      (if (stringp walked) (list :sequence walked) walked))))

(defun fold-tree (tree case-fold)
  "TREE, a cl-ppcre parse tree, with each character it matches ignoring case
that is one with more than its other case (FOLD-CLASS) made the set of all of
them: s also the long s, k the Kelvin sign, sigma the final sigma, as Unicode
simple case folding has it; a set holding such a character gains the others,
and so does a range holding a letter of theirs that has two cases. CASE-FOLD
tells whether the pattern ignores case where it sets no flag of its own; (?i)
and (?-i) turn it on and off for what follows them, and a group's own for the
group. As a second value, every such character met. TREE itself when it has
none."
  (let ((met '()))
    (labels ((mode-after (flags fold)
               (cond ((member :case-insensitive-p flags) t)
                     ((member :case-sensitive-p flags) nil)
                     (t fold)))
             (folds-of (char)
               (let ((class (fold-class char)))
                 (when class (pushnew char met))
                 class))
             (widened-items (items)
               ;; the items of a set, and the characters those of them fold with
               (let ((more '()))
                 (dolist (item items)
                   (cond ((characterp item)
                          (loop for other across (or (folds-of item) "") do (pushnew other more)))
                         ;; a range gains what a letter in it folds with, not what a sign in it does:
                         ;; a to z the long s, but all past ASCII no k for the Kelvin sign in it
                         ((and (consp item) (eq (first item) :range))
                          (maphash (lambda (char class)
                                     (when (and (both-case-p char) (char<= (second item) char (third item)))
                                       (pushnew char met)
                                       (loop for other across class do (pushnew other more))))
                                   **fold-classes**))))
                 (append items (set-difference more items))))
             (walk (node fold)
               (cond ((characterp node)
                      (let ((class (and fold (folds-of node))))
                        (if class (cons :char-class (coerce class 'list)) node)))
                     ((stringp node)
                      (if (and fold (some #'fold-class node))
                          (cons :sequence
                                (let ((parts '()) (start 0))
                                  (loop for i from 0 below (length node)
                                        do (when (fold-class (char node i))
                                             (when (< start i) (push (subseq node start i) parts))
                                             (push (walk (char node i) t) parts)
                                             (setf start (1+ i))))
                                  (when (< start (length node)) (push (subseq node start) parts))
                                  (nreverse parts)))
                          node))
                     ((atom node) node)
                     ((member (first node) '(:char-class :inverted-char-class))
                      (if fold (cons (first node) (widened-items (rest node))) node))
                     ((eq (first node) :sequence)
                      ;; inline flags change the mode for the rest of the sequence
                      (let ((fold fold))
                        (cons :sequence
                              (loop for child in (rest node)
                                    collect (if (and (consp child) (eq (first child) :flags))
                                                (progn (setf fold (mode-after (rest child) fold)) child)
                                                (walk child fold))))))
                     ((eq (first node) :group)
                      (let ((flags (butlast (rest node))))
                        (append (list :group) flags (list (walk (car (last node)) (mode-after flags fold))))))
                     ((eq (first node) :named-register)
                      (list :named-register (second node) (walk (third node) fold)))
                     ((member (first node) '(:property :inverted-property :back-reference :filter :flags)) node)
                     (t (cons (first node) (mapcar (lambda (child) (walk child fold)) (rest node)))))))
      (let ((walked (walk tree case-fold)))
        (if met (values walked met) (values tree nil))))))

(defun text-end-filter (position)
  "A cl-ppcre filter standing for \\z: POSITION when the text scanned ends
there and that is where the text the pattern may see ends, not a window cut
short of it (*WINDOW-CUT*); else NIL."
  (and (= position cl-ppcre::*end-pos*) (null *window-cut*) position))

(defun text-end-or-last-newline-filter (position)
  "A cl-ppcre filter standing for \\Z: POSITION when the text the pattern may
see ends there, or one newline that ends it stands there; else NIL. Where
the text scanned is a window cut short of that end (*WINDOW-CUT*), the rope
tells what is left after it (*WINDOW-ROPE*)."
  (let ((end cl-ppcre::*end-pos*) (cut *window-cut*))
    (declare (type fixnum position end))
    (cond ((= position end)
           (and (or (null cut)
                    (destructuring-bind (rope context octet bound) *window-rope*
                      (declare (ignore context octet))
                      (and (= (1+ cut) bound) (= 10 (tuatara.text:rope-octet rope cut)))))
                position))
          ((and (null cut) (= position (1- end)) (char= (char cl-ppcre::*string* position) #\Newline))
           position))))

(defun text-end-tree (tree)
  "TREE, a cl-ppcre parse tree, with \\z and \\Z made filters that match at
the end of the text the pattern may see and not at the end of each window of
it a scan tries the pattern on (TEXT-END-FILTER,
TEXT-END-OR-LAST-NEWLINE-FILTER). TREE itself when it has neither."
  (if (tree-has tree :modeless-end-anchor :modeless-end-anchor-no-newline)
      (labels ((walk (node)
                 (cond ((eq node :modeless-end-anchor-no-newline) (list :filter #'text-end-filter 0))
                       ((eq node :modeless-end-anchor) (list :filter #'text-end-or-last-newline-filter 0))
                       ((atom node) node)
                       ((member (first node) '(:char-class :inverted-char-class :property :inverted-property
                                               :back-reference :filter :flags))
                        node)
                       (t (cons (first node) (mapcar #'walk (rest node)))))))
        (walk tree))
      tree))

(defun tree-max-octets (tree)
  "The most octets a match of TREE, a cl-ppcre parse tree, and the text it
looks at past its end can take, counting four for each character and 90
characters for a filter's mark (*FILTER-MARK*); NIL when nothing bounds it (a
repetition with no most, a back-reference)."
  (labels ((walk (node)
             (cond ((characterp node) 4)
                   ((stringp node) (* 4 (length node)))
                   ((member node '(:everything :digit-class :word-char-class :whitespace-char-class
                                   :non-digit-class :non-word-char-class :non-whitespace-char-class))
                    4)
                   ;; anchors, boundaries, flags and :void take no text
                   ((symbolp node) 0)
                   (t (case (first node)
                        (:sequence (let ((total 0))
                                     (dolist (child (rest node) total)
                                       (let ((most (walk child))) (if most (incf total most) (return nil))))))
                        (:alternation (let ((largest 0))
                                        (dolist (child (rest node) largest)
                                          (let ((most (walk child)))
                                            (if most (setf largest (max largest most)) (return nil))))))
                        (:named-register
                         (cond ((filter-crosses-lines-p (second node)) nil)
                               ;; text a filter reads: at most a Bitcoin address's 90 characters
                               ((uiop:string-prefix-p *filter-mark* (second node)) (* 4 90))
                               (t (walk (car (last node))))))
                        ((:group :register :standalone) (walk (car (last node))))
                        ((:greedy-repetition :non-greedy-repetition)
                         (destructuring-bind (least most body) (rest node)
                           (declare (ignore least))
                           (let ((one (walk body))) (and most one (* most one)))))
                        ((:positive-lookahead :negative-lookahead) (walk (second node)))
                        ((:positive-lookbehind :negative-lookbehind :flags) 0)
                        ((:char-class :inverted-char-class :property :inverted-property) 4)
                        (t nil))))))
    (walk tree)))

(defun lookbehind-octets (tree)
  "The most octets the lookbehinds of TREE, a cl-ppcre parse tree, look back
from where they stand, a lookbehind inside one adding to it, counting four
for each character (TREE-MAX-OCTETS); 0 when it has none."
  (labels ((walk (node)
             (cond ((atom node) 0)
                   ((member (first node) '(:positive-lookbehind :negative-lookbehind))
                    (+ (or (tree-max-octets (second node)) *regex-longest-match*) (walk (second node))))
                   (t (reduce #'max (rest node) :key #'walk :initial-value 0)))))
    (walk tree)))
