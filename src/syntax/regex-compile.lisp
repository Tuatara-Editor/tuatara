;;;; src/syntax/regex-compile.lisp --- A pattern made into a scanner, the scanners kept while in use.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; COMPILE-REGEX gives the cl-ppcre scanner for a pattern with its
;;;; REGEX-TRAITS, and keeps what it made for as long as it is used.
;;;; COMPILE-PATTERN reads a pattern as text or as a regular expression, and
;;;; refuses one that would take too long to match.

(in-package #:tuatara.syntax)

(sb-ext:defglobal **compiled-regexes** (make-hash-table :test 'equal :weakness :value :synchronized t)
  "The scanners COMPILE-REGEX made that are still in use, by the pattern and
how it was compiled (case folding, CRLF, thoroughness): the same pattern is
compiled once while its scanner is kept.")

(sb-ext:defglobal **recent-regexes** (make-array 4 :initial-element nil)
  "The scanners COMPILE-REGEX made last, kept so that a pattern compiled again
soon after, as a prompt is drawn again, is not compiled anew.")

(sb-ext:defglobal **recent-regex-count** 0
  "How many scanners COMPILE-REGEX has made, which place of **RECENT-REGEXES** the next goes in.")
(declaim (type fixnum **recent-regex-count**))

(defun forget-compiled-regexes ()
  "Compile every pattern anew from now on: what a named pattern stands for changed."
  (sb-ext:with-locked-hash-table (**compiled-regexes**)
    (clrhash **compiled-regexes**)
    (fill **recent-regexes** nil)))

(defun compile-regex (regex &key (case-fold (regex-case-fold-p regex)) (thorough *regex-thorough*))
  "A cl-ppcre scanner for REGEX (PCRE-style syntax) in which ^ and $ match at
line boundaries. CASE-FOLD ignores case; by default when REGEX has no upper-case
letter outside an escape. What cl-ppcre lacks is put in first (ENGINE-PATTERN):
named patterns ([[:ipv4:]], [[:creditcard:]], whose card numbers must pass the
Luhn check), POSIX classes in a set, \\Q...\\E, \\h, \\v, \\R, \\xNN from 80 to
FF the octet NN standing on its own; named groups and character properties
\(\\p{L}) are read too (WITH-PATTERN-SYNTAX). Lines end as they do in every
text: ^ and $ match at a newline, at a carriage return and a newline, and at a
carriage return alone; \\n matches any one of those line ends, [\\n] the newline
alone; and . matches neither a carriage return nor a newline (LINE-TREE).
THOROUGH, by default *REGEX-THOROUGH*, has a scan find every match of up to
*REGEX-LONGEST-MATCH* octets whole, looking as far as the pattern can reach
\(TREE-MAX-OCTETS). What the pattern allows a scan to skip (REGEX-TRAITS) is
registered with the scanner. The same REGEX compiled the same way gives the same
scanner while one is kept, or soon after. Signals PATTERN-REFUSED, or cl-ppcre's
syntax error, for a pattern that cannot be used (REGEX-PROBLEM words why)."
  (let ((key (list regex (and case-fold t) (and thorough t))))
    (or (gethash key **compiled-regexes**)
        (let ((scanner (with-pattern-syntax
                         (compile-engine-pattern (engine-pattern regex) case-fold thorough))))
          (setf (first key) (copy-seq regex))
          (sb-ext:with-locked-hash-table (**compiled-regexes**)
            (setf (gethash key **compiled-regexes**) scanner
                  (svref **recent-regexes** (mod (incf **recent-regex-count**) (length **recent-regexes**)))
                  scanner))))))

(defun compile-engine-pattern (regex case-fold thorough)
  "COMPILE-REGEX's work on REGEX, as ENGINE-PATTERN gives it."
  (let* ((tree (handler-case (cl-ppcre:parse-string regex) (cl-ppcre:ppcre-syntax-error () nil)))
         (filtered (and tree (search *filter-mark* regex)))
         (lines (and tree (line-tree tree)))
         (scanner (let ((cl-ppcre:*use-bmh-matchers* t))
                    ;; constant text found by skipping (Boyer-Moore-Horspool): three times faster
                    ;; where matches are many and case is folded
                    (cl-ppcre:create-scanner (cond (filtered
                                                    ;; card numbers: read by code as they are matched
                                                    (filter-tree lines))
                                                   (tree lines)
                                                   (t regex))
                                             :case-insensitive-mode case-fold :multi-line-mode t)))
         ;; for text without a carriage return, where its lines end at newlines alone, the
         ;; pattern as written does the same, and quicker
         (plain (and tree (not (equal (if (stringp tree) (list :sequence tree) tree) lines))
                     (let ((cl-ppcre:*use-bmh-matchers* t))
                       (cl-ppcre:create-scanner (if filtered (filter-tree tree) tree)
                                                :case-insensitive-mode case-fold :multi-line-mode t)))))
    (when tree
      ;; (?i) and the like, and a group's own, (?i:a): what a match must hold, and
      ;; whether it stays within a line, cannot be read from the text then
      (let ((flags (tree-has tree :flags :case-insensitive-p :case-sensitive-p :multi-line-mode-p
                             :not-multi-line-mode-p :single-line-mode-p :not-single-line-mode-p)))
        (multiple-value-bind (literal prefix-p) (and (not flags) (required-literal tree))
          ;; octet folding knows ASCII only: letters beyond it keep the full scan
          (unless (or (not case-fold)
                      (every (lambda (c) (or (< (char-code c) 128) (not (both-case-p c)))) (or literal "")))
            (setf literal nil))
          (multiple-value-bind (shape shape-prefix-p) (and (not flags) (required-shape tree case-fold))
            ;; a shape where the text is none or too short to skip by, telling more than
            ;; it, and enough to be worth looking for
            (if (and shape (>= (shape-bits (coerce shape 'list)) 20)
                     (or (null literal) (< (tuatara.text:string-octet-length literal) 3))
                     (> (shape-bits (coerce shape 'list))
                        (if literal (* (if case-fold 7 8) (tuatara.text:string-octet-length literal)) 0)))
                (setf literal nil prefix-p shape-prefix-p)
                (setf shape nil))
          (setf (gethash scanner **regex-traits**)
                (let ((line-bound (and (not flags) (not (crosses-lines-p tree)))))
                  (make-regex-traits :needle (and literal (needle-octets literal case-fold)) :fold case-fold
                                     :shape shape :plain plain
                                     :prefix (and (or literal shape) prefix-p)
                                     :line-bound line-bound
                                     :end-anchored (end-anchored-p tree)
                                     :context (max +regex-context+ (lookbehind-octets lines))
                                     :reach (let ((most (and thorough (not line-bound) (tree-max-octets tree))))
                                              (cond ((or (not thorough) line-bound) +regex-overlap+)
                                                    (most (min *regex-longest-match* (max +regex-overlap+ most)))
                                                    (t *regex-longest-match*))))))))))
    scanner))

(defun pattern-group-count (pattern)
  "How many numbered groups PATTERN, a regular expression, has, its named
groups among them; 0 for a pattern that cannot be used."
  (let ((count 0))
    (labels ((walk (x)
               (when (consp x)
                 (when (member (first x) '(:register :named-register)) (incf count))
                 (mapc #'walk (rest x)))))
      (walk (tuatara.runtime:ignoring-errors-reported ("counting the groups of ~s" pattern)
              (handler-case (with-pattern-syntax (cl-ppcre:parse-string (engine-pattern pattern)))
                ((or cl-ppcre:ppcre-syntax-error pattern-refused) () nil)))))
    count))

(defparameter *repeat-bound-most* 10
  "The largest bound of a repeat taken as bounded by NESTED-REPEAT-TREE-P: a
repeat up to more times counts as one without bound.")

(defun class-matches-p (items inverted char)
  "True when the set ITEMS of a parse tree's (:CHAR-CLASS ...), the whole set
taken the other way round when INVERTED, matches CHAR in either case."
  (flet ((one (c)
           (some (lambda (item)
                   (cond ((characterp item) (char= item c))
                         ((symbolp item)
                          (ecase item
                            (:digit-class (char<= #\0 c #\9))
                            (:non-digit-class (not (char<= #\0 c #\9)))
                            (:whitespace-char-class (tuatara.text:blank-char-p c))
                            (:non-whitespace-char-class (not (tuatara.text:blank-char-p c)))
                            (:word-char-class (or (alphanumericp c) (char= c #\_)))
                            (:non-word-char-class (not (or (alphanumericp c) (char= c #\_))))))
                         ((eq (first item) :range) (char<= (second item) c (third item)))
                         ((eq (first item) :property) (funcall (character-property (second item)) c))
                         ((eq (first item) :inverted-property) (not (funcall (character-property (second item)) c)))))
                 items)))
    (let ((found (or (one char) (one (char-upcase char)) (one (char-downcase char)))))
      (if inverted (not found) (and found t)))))

(defun class-characters (items inverted)
  "Every character the set ITEMS of a parse tree's (:CHAR-CLASS ...) matches,
or :OPEN when there are too many to list: a set taken the other way round
\(INVERTED), one naming a property or a class without end (\\w, \\D), or a
range of more than 65536 characters."
  (if inverted
      :open
      (let ((characters '()))
        (dolist (item items characters)
          (cond ((characterp item) (push item characters))
                ((eq item :digit-class) (setf characters (append (coerce "0123456789" 'list) characters)))
                ((eq item :whitespace-char-class) (setf characters (append tuatara.text:*blank-characters* characters)))
                ((and (consp item) (eq (first item) :range)
                      (<= (- (char-code (third item)) (char-code (second item))) 65536))
                 (loop for code from (char-code (second item)) to (char-code (third item))
                       do (push (code-char code) characters)))
                (t (return :open)))))))

(defun choices-apart-p (alternation)
  "True when each choice of ALTERNATION, a parse tree's (:ALTERNATION ...),
starts with a character, a set or a class (\\d, \\s) that no other choice's
first one shares in either case, so that at any place one choice at most can
match: (\\d|-) and (?:\\s|,) are, (a|ab) and (\\w|\\d) are not."
  (labels ((first-set (x)
             ;; the first character's set as (ITEMS . INVERTED), or NIL when it is not known
             (cond ((characterp x) (list (list x)))
                   ((stringp x) (and (plusp (length x)) (list (list (char x 0)))))
                   ((member x '(:digit-class :non-digit-class :whitespace-char-class :non-whitespace-char-class
                                :word-char-class :non-word-char-class))
                    (list (list x)))
                   ((atom x) nil)
                   ((eq (first x) :char-class) (list (rest x)))
                   ((eq (first x) :inverted-char-class) (cons (rest x) t))
                   ((member (first x) '(:property :inverted-property)) (list (list x)))
                   ((eq (first x) :sequence) (and (rest x) (first-set (second x))))
                   ((member (first x) '(:group :register :named-register :standalone))
                    (first-set (car (last x))))
                   ((and (member (first x) '(:greedy-repetition :non-greedy-repetition)) (plusp (second x)))
                    (first-set (fourth x)))
                   (t nil)))
           (apart-p (a b)
             (let ((listed (class-characters (car a) (cdr a)))
                   (other b))
               (when (eq listed :open)
                 (setf listed (class-characters (car b) (cdr b)) other a))
               (and (listp listed)
                    (notany (lambda (char) (class-matches-p (car other) (cdr other) char)) listed)))))
    (let ((firsts (mapcar #'first-set (rest alternation))))
      (and (every #'identity firsts)
           (loop for (a . others) on firsts
                 always (every (lambda (b) (apart-p a b)) others))))))

(defun nested-repeat-tree-p (tree &key alternations)
  "True when TREE, a regular expression as CL-PPCRE:PARSE-STRING reads one,
repeats without bound, or more than *REPEAT-BOUND-MOST* times, a part that
itself repeats, as (a+)+ or (x*y){2,30} do, or a back-reference to a group
that repeats, as (a*)\\1+ does; or with ALTERNATIONS one that holds a choice
whose choices may start alike (CHOICES-APART-P), as (a|aa)* does: a scan of
such a pattern can take time growing exponentially with the text it fails on.
The true value says which: :REPEAT, :BACK-REFERENCE or :CHOICE. Repeats one
after another, as .*.*.*x, take time growing as a power of the text's length,
and are not refused."
  (let ((groups '()))
    (labels ((repeat-p (x) (and (consp x) (member (first x) '(:greedy-repetition :non-greedy-repetition))))
             (find-groups (x)
               (when (consp x)
                 (when (member (first x) '(:register :named-register)) (push x groups))
                 (mapc #'find-groups (rest x))))
             (named-p (reference group)
               (if (integerp reference)
                   (eql reference (1+ (position group groups)))
                   (and (eq (first group) :named-register) (equal reference (second group)))))
             (holds-repeat-p (x seen)
               (and (consp x)
                    (cond ((and (repeat-p x) (not (eql (third x) 1))) :repeat)
                          ((eq (first x) :back-reference)
                           (and (not (member (second x) seen :test #'equal))
                                (some (lambda (group)
                                        (and (named-p (second x) group)
                                             (holds-repeat-p (car (last group)) (cons (second x) seen))))
                                      groups)
                                :back-reference))
                          ((and alternations (eq (first x) :alternation) (not (choices-apart-p x))) :choice)
                          (t (some (lambda (part) (holds-repeat-p part seen)) (rest x))))))
             (walk (x)
               (and (consp x)
                    (or (and (repeat-p x) (or (null (third x)) (> (third x) *repeat-bound-most*))
                             (holds-repeat-p (fourth x) '()))
                        (some #'walk (rest x))))))
      (find-groups tree)
      (setf groups (nreverse groups))
      (walk tree))))

(defun nested-repeat-p (pattern)
  "True when PATTERN, a regular expression as every search reads one, repeats
without bound a part that itself repeats, a back-reference to a group that
repeats or a choice whose choices may start alike; the true value says which,
as NESTED-REPEAT-TREE-P's does."
  (nested-repeat-tree-p (handler-case (with-pattern-syntax (cl-ppcre:parse-string (engine-pattern pattern)))
                          ((or cl-ppcre:ppcre-syntax-error pattern-refused) () nil))
                        :alternations t))

(defun compile-pattern (pattern &key (regex t) (case-fold nil fold-given) or-text untrusted)
  "A scanner for PATTERN as every search reads it (COMPILE-REGEX): a regular
expression when REGEX is true, else literal text, nothing in it special.
CASE-FOLD ignores case; when it is not given, case is ignored while PATTERN
has no upper-case letter (for a regular expression, outside an escape, and
naming no case: REGEX-CASE-FOLD-P). With OR-TEXT, a PATTERN that is no
regular expression is taken as literal text; otherwise that signals, as
COMPILE-REGEX does. With UNTRUSTED, for a pattern from a file, a regular
expression that repeats a repeated part without bound (NESTED-REPEAT-P) is
refused as one that is no regular expression is."
  (flet ((text ()
           (compile-regex (cl-ppcre:quote-meta-chars pattern)
                          :case-fold (if fold-given case-fold (case-fold-p pattern))))
         (regex ()
           (let ((cause (and untrusted (nested-repeat-p pattern))))
             (when cause
               (refuse-pattern (format nil "not run from a file: ~a, which can take for ever"
                                       (ecase cause
                                         (:repeat "a repeat of a part that itself repeats")
                                         (:back-reference "a repeat of a back-reference to a group that repeats")
                                         (:choice "a repeat of a choice whose choices may start alike"))))))
           (compile-regex pattern :case-fold (if fold-given case-fold (regex-case-fold-p pattern)))))
    (cond ((not regex) (text))
          (or-text (or (tuatara.runtime:ignoring-errors-reported ("reading ~s as a regular expression" pattern)
                         (handler-case (regex)
                           ((or cl-ppcre:ppcre-syntax-error pattern-refused) () nil)))
                       (text)))
          (t (regex)))))
