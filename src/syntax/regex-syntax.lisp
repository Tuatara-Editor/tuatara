;;;; src/syntax/regex-syntax.lisp --- The pattern language: what a search pattern may hold and how it is read.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A pattern is rewritten into what cl-ppcre reads (ENGINE-PATTERN): the POSIX
;;;; classes, \Q...\E, character properties, named groups and possessive
;;;; quantifiers PCRE has and cl-ppcre lacks, and a refusal of what it cannot
;;;; match as PCRE does. Nothing here looks at text.

(in-package #:tuatara.syntax)

;;; The pattern language
;;;
;;; Patterns are read as PCRE reads them, by cl-ppcre, with what it lacks put
;;; in first (ENGINE-PATTERN): POSIX classes inside a set ([[:alpha:]], ASCII
;;; as PCRE has them), \Q...\E, \h, \v and \R, character properties (\p{L},
;;; \p{Greek}), named groups ((?<year>...), \k<year>) and possessive
;;; quantifiers (a++ as (?>a+)). An escape PCRE gives a meaning cl-ppcre does
;;; not know (\K, \G) is refused, never read as a plain letter.

(define-condition pattern-refused (error)
  ((words :initarg :words :reader pattern-refused-words))
  (:report (lambda (condition stream) (write-string (pattern-refused-words condition) stream)))
  (:documentation "Signalled for a pattern holding what the regular expressions here do
not support, or a name they do not know; WORDS say what, as the prompt shows
it (\"not supported: \\K\")."))

(setf (documentation 'pattern-refused-words 'function) "What a refused pattern holds that is not supported, in words.")

(defun refuse-pattern (control &rest arguments)
  "Signal PATTERN-REFUSED, its words CONTROL formatted with ARGUMENTS."
  (error 'pattern-refused :words (apply #'format nil control arguments)))

(defparameter *posix-classes*
  `(("alpha" . "A-Za-z") ("digit" . "0-9") ("alnum" . "0-9A-Za-z") ("upper" . "A-Z") ("lower" . "a-z")
    ("space" . ,(coerce (list #\Tab #\Newline (code-char 11) #\Page #\Return #\Space) 'string))
    ("blank" . ,(coerce (list #\Space #\Tab) 'string)) ("punct" . "!-/:-@\\[-`{-~") ("xdigit" . "0-9A-Fa-f")
    ("word" . "\\w") ("cntrl" . ,(format nil "~c-~c~c" (code-char 0) (code-char 31) (code-char 127)))
    ("print" . " -~") ("graph" . "!-~") ("ascii" . ,(format nil "~c-~c" (code-char 0) (code-char 127))))
  "The POSIX classes a set may hold, [:alpha:] in [[:alpha:]x], each (NAME .
MEMBERS), MEMBERS as written inside a set: ASCII, as PCRE has them.")

(defparameter *posix-negations* '(("digit" . "\\D") ("space" . "\\S") ("word" . "\\W"))
  "The POSIX classes whose negation, [:^digit:], is an escape of its own, each
(NAME . MEMBERS); the others are turned round by POSIX-COMPLEMENT.")

(sb-ext:defglobal **posix-complements** (make-hash-table :test 'equal :synchronized t)
  "The members of each negated POSIX class worked out so far (POSIX-COMPLEMENT), by name.")

(defun posix-complement (name)
  "The members, as written inside a set, of every character not in the POSIX
class NAME, for [:^NAME:]: the ASCII characters outside it and every
character past ASCII, as ranges; NIL for a name that is no class."
  (let ((members (cdr (assoc name *posix-classes* :test #'string=))))
    (when members
      (or (gethash name **posix-complements**)
          (setf (gethash name **posix-complements**)
                (let ((scanner (cl-ppcre:create-scanner (format nil "[~a]" members)))
                      (from nil))
                  (with-output-to-string (out)
                    (flet ((range (a b)
                             ;; each end as itself, the characters special in a set escaped
                             (dolist (code (if (= a b) (list a) (list a :to b)))
                               (if (eq code :to)
                                   (write-char #\- out)
                                   (let ((c (code-char code)))
                                     (when (find c "]\\^-[") (write-char #\\ out))
                                     (write-char c out))))))
                      (loop for code from 0 below 128
                            for in = (cl-ppcre:scan scanner (string (code-char code)))
                            do (cond ((and (not in) (null from)) (setf from code))
                                     ((and in from) (range from (1- code)) (setf from nil))))
                      (range (or from 128) (1- char-code-limit))))))))))

(defun posix-class-at (string i)
  "When a POSIX class, [:name:] or [:^name:], or a named pattern ([:ipv4:])
opens at index I of STRING, two values: the index after it and its name, ^
included; else NIL. A name is small letters, then digits too."
  (let ((n (length string)))
    (when (and (< (1+ i) n) (char= (char string i) #\[) (char= (char string (1+ i)) #\:))
      (let* ((from (if (and (< (+ i 2) n) (char= (char string (+ i 2)) #\^)) (+ i 3) (+ i 2)))
             (to (or (position-if-not (lambda (c) (or (char<= #\a c #\z) (char<= #\0 c #\9))) string :start from) n)))
        (when (and (> to from)
                   (char<= #\a (char string from) #\z)
                   (< (1+ to) n)
                   (char= (char string to) #\:)
                   (char= (char string (1+ to)) #\]))
          (values (+ to 2) (subseq string (+ i 2) to)))))))

(defun bracket-end (string i)
  "The index after the set opening at index I of STRING (a regular
expression), or the length of STRING when it is not closed. A ] right after
[ or [^ is a member, as are escaped characters and POSIX classes such as
[:alpha:]. Two more values: true when the set is closed by its ], and the
index of its first member, after the [ and any ^ and leading ]."
  (let* ((n (length string))
         (j (1+ i))
         (members (progn (when (and (< j n) (char= (char string j) #\^)) (incf j))
                         (when (and (< j n) (char= (char string j) #\])) (incf j))
                         j)))
    (loop while (and (< j n) (char/= (char string j) #\]))
          do (cond ((char= (char string j) #\\) (incf j 2))
                   ((posix-class-at string j) (setf j (posix-class-at string j)))
                   (t (incf j))))
    (values (min n (1+ j)) (< j n) members)))

(defparameter *horizontal-space*
  (coerce (append (list #\Tab #\Space) (mapcar #'code-char '(#xA0 #x1680 #x180E #x202F #x205F #x3000))
                  (list (code-char #x2000) #\- (code-char #x200A)))
          'string)
  "What \\h matches, as written inside a set: a tab, a space and the other
horizontal spaces of Unicode.")

(defparameter *vertical-space*
  (coerce (append (list #\Newline (code-char 11) #\Page #\Return) (mapcar #'code-char '(#x85 #x2028 #x2029))) 'string)
  "What \\v matches, as written inside a set: a line feed, a vertical tab, a
form feed, a carriage return and the line and paragraph separators.")

(defun hex-escape (regex i out)
  "Write to OUT the \\x escape at index I of REGEX as ENGINE-PATTERN gives it,
and return the index after it."
  (let* ((n (length regex))
         (brace (and (< (+ i 2) n) (char= (char regex (+ i 2)) #\{)))
         (close (and brace (position #\} regex :start (+ i 3))))
         (code (if brace
                   (and close (> close (+ i 3))
                        (every (lambda (h) (digit-char-p h 16)) (subseq regex (+ i 3) close))
                        (parse-integer regex :start (+ i 3) :end close :radix 16))
                   (and (<= (+ i 4) n)
                        (every (lambda (h) (digit-char-p h 16)) (subseq regex (+ i 2) (+ i 4)))
                        (parse-integer regex :start (+ i 2) :end (+ i 4) :radix 16)))))
    (cond ((and brace code (< code char-code-limit) (not (<= #xD800 code #xDFFF)))
           ;; a character by its code, escaped where the pattern would read it otherwise
           (let ((char (code-char code)))
             (unless (or (alphanumericp char) (>= code 128)) (write-char #\\ out))
             (write-char char out)
             (1+ close)))
          ((and (not brace) code (>= code #x80))
           (write-char (tuatara.text:raw-byte-char code) out)
           (+ i 4))
          (t (write-string "\\x" out) (+ i 2)))))

(defun escape-for-engine (regex i out in-set)
  "Write to OUT the escape at index I of REGEX, inside a set when IN-SET, as
ENGINE-PATTERN gives it, and return the index after it."
  (let ((n (length regex)) (d (char regex (1+ i))))
    (case d
      (#\x (hex-escape regex i out))
      (#\Q
       ;; the text as it is, up to \E or the end
       (let ((to (or (search "\\E" regex :start2 (+ i 2)) n)))
         (loop for k from (+ i 2) below to
               for c = (char regex k)
               do (unless (alphanumericp c) (write-char #\\ out))
                  (write-char c out))
         (min n (+ to 2))))
      (#\E (+ i 2))
      ((#\h #\v #\H #\V)
       (let ((members (if (char-equal d #\h) *horizontal-space* *vertical-space*)))
         (cond ((lower-case-p d) (format out (if in-set "~a" "[~a]") members))
               (in-set (refuse-pattern "not supported: \\~c in a set" d))
               (t (format out "[^~a]" members))))
       (+ i 2))
      (#\R
       (when in-set (refuse-pattern "not supported: \\R in a set"))
       (format out "(?:\\r\\n|[~a])" *vertical-space*)
       (+ i 2))
      ((#\p #\P)
       (cond ((and (< (+ i 2) n) (char/= (char regex (+ i 2)) #\{))
              (format out "\\~c{~c}" d (char regex (+ i 2)))
              (+ i 3))
             (t (write-char #\\ out) (write-char d out) (+ i 2))))
      (#\k
       (unless (and (not in-set) (< (+ i 2) n) (char= (char regex (+ i 2)) #\<))
         (refuse-pattern "not supported: \\k without <name>"))
       (write-string "\\k" out)
       (+ i 2))
      (#\c
       ;; a control character: \c and the character after it, whatever it is
       (write-string regex out :start i :end (min n (+ i 3)))
       (min n (+ i 3)))
      (t
       (when (and (alpha-char-p d) (< (char-code d) 128)
                  (not (find d (if in-set "abdDefnrsStwW" "abBdDefnrsStwWAzZ"))))
         (refuse-pattern "not supported~:[~; in a set~]: \\~c" in-set d))
       (write-char #\\ out)
       (write-char d out)
       (+ i 2)))))

(defun quantifier-end (regex i)
  "The index after the quantifier opening at index I of REGEX (*, +, ?,
{n}, {n,} or {n,m}), or NIL when none opens there."
  (let ((n (length regex)))
    (case (char regex i)
      ((#\* #\+ #\?) (1+ i))
      (#\{ (let* ((first-end (or (position-if-not #'digit-char-p regex :start (1+ i)) n))
                  (comma (and (> first-end (1+ i)) (< first-end n) (char= (char regex first-end) #\,)))
                  (close (if comma (or (position-if-not #'digit-char-p regex :start (1+ first-end)) n) first-end)))
             (and (> first-end (1+ i)) (< close n) (char= (char regex close) #\}) (1+ close))))
      (t nil))))

(defun quoted-last-length (regex i)
  "How many characters ENGINE-PATTERN writes for the last character quoted by
the \\Q opening at index I of REGEX: 2 when it is escaped, 1 when not, NIL
when \\Q...\\E quotes nothing."
  (let ((to (or (search "\\E" regex :start2 (+ i 2)) (length regex))))
    (when (> to (+ i 2))
      (if (alphanumericp (char regex (1- to))) 1 2))))

(defun set-class-members (name closed)
  "The members, as written inside a set, of the POSIX class NAME standing in
a set, [:alpha:] or turned round, [:^alpha:]. A named pattern there is
refused, but in a set not CLOSED yet, still being typed, which is left for
cl-ppcre to say so. Signals PATTERN-REFUSED for a name no class has."
  (if (char= (char name 0) #\^)
      (let ((name (subseq name 1)))
        (or (cdr (assoc name *posix-negations* :test #'string=))
            (posix-complement name)
            (and (named-pattern-p name)
                 (refuse-pattern "invalid: a named pattern is not turned round: [[:^~a:]]" name))
            (refuse-pattern "invalid: no POSIX class [:~a:]" name)))
      (or (cdr (assoc name *posix-classes* :test #'string=))
          (and (named-pattern-p name)
               (if closed
                   (refuse-pattern "invalid: [:~a:] stands alone in its brackets: [[:~a:]]" name name)
                   "x"))
          (refuse-pattern "invalid: no POSIX class [:~a:]" name))))

(defun engine-pattern (regex)
  "REGEX as cl-ppcre is given it. A named pattern alone in its brackets
\([[:ipv4:]], *NAMED-PATTERNS*) becomes the group it stands for, and a date
with its bounds ([[:date<2026-07-01:]], DATE-PATTERN-AT) likewise. POSIX
classes in a set ([[:alpha:]]) become the ASCII characters PCRE gives them.
\\Q...\\E becomes that text quoted. \\h and \\H, \\v and \\V become sets of
horizontal and vertical space. \\R becomes a line break of any kind. \\pL
becomes \\p{L}. A stray \\E becomes nothing. Each \\xNN whose NN is 80 to FF
becomes the octet NN standing on its own, not inside a character (the raw byte
the display shows as \\xNN). Each \\x{H...} becomes the character of that code.
A possessive quantifier (a++, a*+, a?+, a{2,5}+) becomes the atomic group it
stands for ((?>a+)). The rest is as it is.
Signals PATTERN-REFUSED for an escape PCRE gives a meaning cl-ppcre does not
know (\\K, \\G, \\X), for one that cannot go in a set (\\R there), for a POSIX
class it has no name for, for [:alpha:] outside a set, and for a named
pattern in a set with other members or turned round."
  (let ((n (length regex)) (i 0) (set-end -1) (set-closed nil)
        (out-text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        ;; where the output of what a quantifier would repeat begins, and of each group open
        (repeat-from nil) (groups '())
        ;; what DATE-PATTERN-AT gave for the set opening here, when one does
        (date nil))
    (with-output-to-string (out out-text)
      (loop while (< i n)
            do (let ((c (char regex i)) (in-set (< i set-end)) (start (fill-pointer out-text)))
                 (cond
                   ((and (char= c #\\) (< (1+ i) n))
                    (let ((d (char regex (1+ i))) (quoted (quoted-last-length regex i)))
                      (setf i (escape-for-engine regex i out in-set))
                      (unless (or in-set (char= d #\E))
                        (setf repeat-from (if (char= d #\Q)
                                       (and quoted (- (fill-pointer out-text) quoted))
                                       start)))))
                   (in-set
                    (multiple-value-bind (after name) (posix-class-at regex i)
                      (if (null after)
                          (progn (write-char c out) (incf i))
                          (progn (write-string (set-class-members name set-closed) out)
                                 (setf i after)))))
                   ((and (char= c #\[) (not in-set) (setf date (multiple-value-list (date-pattern-at regex i)))
                         (first date))
                    ;; a date, with its bounds or not: what it stands for, as a group
                    (destructuring-bind (after pattern) date
                      (format out "(?:~a)" pattern)
                      (setf repeat-from start
                            i after)))
                   ((and (char= c #\[) (not in-set)
                         (multiple-value-bind (after name) (posix-class-at regex (1+ i))
                           (and after name (< after n) (char= (char regex after) #\]) (named-pattern-p name))))
                    ;; a named pattern alone in its brackets: what it stands for, as a group
                    (multiple-value-bind (after name) (posix-class-at regex (1+ i))
                      (format out "(?:~a)" (named-pattern name))
                      (setf repeat-from start
                            i (1+ after))))
                   ((char= c #\[)
                    (let ((after (posix-class-at regex i)))
                      (when after
                        (refuse-pattern "invalid: ~a goes inside a set: [~:*~a]" (subseq regex i after))))
                    ;; the set's opening, ^ and a leading ] as they are; its members by the rules of a set
                    (multiple-value-bind (end closed members) (bracket-end regex i)
                      (setf set-end (1- end)
                            set-closed closed
                            repeat-from start)
                      (write-string regex out :start i :end members)
                      (setf i members)))
                   ((= i set-end) (write-char c out) (incf i))
                   ((and repeat-from (quantifier-end regex i))
                    (let ((end (quantifier-end regex i)))
                      (write-string regex out :start i :end end)
                      (cond ((and (< end n) (char= (char regex end) #\+))
                             ;; possessive: what it repeats, as often as it can, never given back
                             (let ((repeated (subseq out-text repeat-from)))
                               (setf (fill-pointer out-text) repeat-from)
                               (format out "(?>~a)" repeated))
                             (setf i (1+ end)))
                            ((and (< end n) (char= (char regex end) #\?))
                             (write-char #\? out)
                             (setf i (1+ end)))
                            (t (setf i end)))
                      (setf repeat-from nil)))
                   ((and (char= c #\() (< (+ i 2) n) (string= "?#" regex :start2 (1+ i) :end2 (+ i 3)))
                    ;; a comment, as it is
                    (let ((end (min n (1+ (or (position #\) regex :start i) n)))))
                      (write-string regex out :start i :end end)
                      (setf i end)))
                   ((char= c #\()
                    (push start groups)
                    (setf repeat-from nil)
                    (write-char c out)
                    (incf i))
                   ((char= c #\))
                    (setf repeat-from (pop groups))
                    (write-char c out)
                    (incf i))
                   ((char= c #\|)
                    (setf repeat-from nil)
                    (write-char c out)
                    (incf i))
                   (t (setf repeat-from start)
                      (write-char c out)
                      (incf i))))))
    (coerce out-text 'simple-string)))

(sb-ext:defglobal **scripts** nil
  "The Unicode scripts, as keywords, by their names in small letters without
spaces, - or _, once looked for.")

(defun unicode-script (key)
  "The Unicode script KEY names, in small letters without spaces, - or _
\(PROPERTY-KEY), as a keyword (:OLD-ITALIC for olditalic); NIL when none
does. Every character's script is found once, the first time."
  (gethash key (or **scripts**
                   (setf **scripts**
                         (let ((scripts (make-hash-table :test 'eq)) (names (make-hash-table :test 'equal)))
                           (dotimes (code char-code-limit)
                             (let ((char (code-char code)))
                               (when char (setf (gethash (sb-unicode:script char) scripts) t))))
                           (loop for script being the hash-keys of scripts
                                 do (setf (gethash (property-key (symbol-name script)) names) script))
                           names)))))

(defparameter *property-names*
  '(("letter" . "l") ("mark" . "m") ("number" . "n") ("punctuation" . "p") ("symbol" . "s") ("separator" . "z")
    ("other" . "c") ("uppercaseletter" . "lu") ("lowercaseletter" . "ll") ("titlecaseletter" . "lt")
    ("decimalnumber" . "nd") ("spaceseparator" . "zs") ("casedletter" . "lc") ("l&" . "lc"))
  "Long names of character properties and the short ones they stand for.")

(defparameter *general-categories*
  '("lu" "ll" "lt" "lm" "lo" "mn" "mc" "me" "nd" "nl" "no" "pc" "pd" "ps" "pe" "pi" "pf"
    "po" "sm" "sc" "sk" "so" "zs" "zl" "zp" "cc" "cf" "cs" "co" "cn")
  "The general categories of Unicode, by their two letters.")

(defun property-key (name)
  "NAME, a character property as \\p{NAME} gives it, as CHARACTER-PROPERTY
looks it up: in small letters, without spaces, - and _ or an Is before it, a
long name of a general category as its short one (lu for Uppercase_Letter)."
  (let* ((key (string-downcase (remove-if (lambda (c) (find c " _-")) name)))
         (key (if (and (> (length key) 2) (string= "is" key :end2 2)) (subseq key 2) key)))
    (or (cdr (assoc key *property-names* :test #'string=)) key)))

(defun case-property-p (name)
  "True when the character property NAME names a case: upper-case, lower-case
or title-case letters (Lu, Ll, Lt)."
  (and (member (property-key name) '("lu" "ll" "lt") :test #'string=) t))

(defun character-property (name)
  "A function true of the characters having the property NAME, as \\p{NAME}
reads it. NAME is a general category of Unicode by its letter or two (L, Lu,
Nd, P, Zs, LC for any cased letter) or long name (Letter, Uppercase_Letter), a
script (Greek, Latin, Han), Alphabetic, White_Space, Any or ASCII. Case,
spaces, - and _ do not matter, and Is before a name is allowed. Signals
PATTERN-REFUSED for a name it does not know."
  (let ((key (property-key name)))
    (cond ((string= key "any") (constantly t))
          ((string= key "ascii") (lambda (c) (< (char-code c) 128)))
          ((string= key "alphabetic") #'sb-unicode:alphabetic-p)
          ((string= key "whitespace") #'sb-unicode:whitespace-p)
          ((string= key "lc") (lambda (c) (member (sb-unicode:general-category c) '(:lu :ll :lt))))
          ((and (= (length key) 1) (find (char key 0) "lmnpszc"))
           (let ((letter (char-upcase (char key 0))))
             (lambda (c) (char= letter (char (symbol-name (sb-unicode:general-category c)) 0)))))
          ((member key *general-categories* :test #'string=)
           (let ((category (intern (string-upcase key) :keyword)))
             (lambda (c) (eq category (sb-unicode:general-category c)))))
          (t (let ((script (unicode-script key)))
               (unless script (refuse-pattern "invalid: no character property \\p{~a}" name))
               (lambda (c) (eq script (sb-unicode:script c))))))))

(defmacro with-pattern-syntax (&body body)
  "Run BODY with cl-ppcre reading patterns as ENGINE-PATTERN leaves them:
named groups allowed, \\p{...} read by CHARACTER-PROPERTY; and with the
extended mode that (?x) turns on bound here, so that one pattern's (?x),
read by CL-PPCRE:PARSE-STRING, is not taken up by every pattern read after
it (which would then pass over their spaces and take # for a note)."
  `(let ((cl-ppcre:*allow-named-registers* t)
         (cl-ppcre:*property-resolver* #'character-property)
         (cl-ppcre::*extended-mode-p* nil))
     ,@body))

(defun repeated-size (text)
  "How large TEXT, a pattern as cl-ppcre reads it, is once its repeats are
counted: each character once, what a repeat {N} or {N,M} holds N or M times,
and what + or * holds twice, so that a short pattern repeating itself a
thousand times a thousand times is seen as the million copies it stands for.
The length of TEXT when it cannot be read."
  (labels ((size (tree)
             (cond ((stringp tree) (length tree))
                   ((and (consp tree) (member (first tree) '(:greedy-repetition :non-greedy-repetition)))
                    (destructuring-bind (low high inner) (rest tree)
                      (* (max 1 (or high (* 2 (max 1 low)))) (size inner))))
                   ((consp tree) (max 1 (reduce #'+ (rest tree) :key #'size)))
                   (t 1))))
    (handler-case (size (with-pattern-syntax (cl-ppcre:parse-string text)))
      (error () (length text)))))

(defun literal-pattern-p (pattern)
  "True when PATTERN has none of the characters that make a regular expression
special, nor a raw byte (which as plain octets would be found inside a valid
character too), so searching for it as plain text finds exactly the same
matches."
  (not (find-if (lambda (c) (or (find c "\\^$.|?*+()[]{}") (tuatara.text:raw-byte-code-p (char-code c)))) pattern)))

(defun regex-case-fold-p (pattern)
  "True when a search for PATTERN ignores case (smart case). That is when it
has no upper-case letter outside an escape such as \\W or \\S, a character's
code (\\x4A, \\x{1F600}), a control character (\\cA), the name of a group
\((?<Year>...), \\k<Year>) and the name of a character property (\\pL,
\\p{Greek}). It must name no case either:
no [:upper:] or [:lower:] (or [:^upper:], [:^lower:]) in a set, no \\p{Lu},
\\p{Ll} or \\p{Lt} (written in any case, or by its long name,
Uppercase_Letter), which ask for case as a capital letter does."
  (let ((i 0) (n (length pattern)))
    (flet ((past (char from)
             ;; just past the next CHAR at or after FROM, or the end
             (let ((at (position char pattern :start (min from n)))) (if at (1+ at) n))))
      (loop while (< i n)
            do (let ((c (char pattern i))
                     (d (and (< (1+ i) n) (char pattern (1+ i)))))
                 (cond ((char= c #\\)
                        (cond ((and (eql d #\x) (< (+ i 2) n) (char= (char pattern (+ i 2)) #\{))
                               (setf i (past #\} i)))
                              ((eql d #\x)
                               ;; up to two hexadecimal digits
                               (incf i 2)
                               (loop repeat 2 while (and (< i n) (digit-char-p (char pattern i) 16)) do (incf i)))
                              ((and (eql d #\k) (< (+ i 2) n) (char= (char pattern (+ i 2)) #\<))
                               (setf i (past #\> i)))
                              ((eql d #\c)
                               ;; a control character, which has no case, by the letter after \c
                               (incf i 3))
                              ((not (and (< (+ i 2) n) (find d "pP"))) (incf i 2))
                              ((char= (char pattern (+ i 2)) #\{)
                               (let ((close (or (position #\} pattern :start i) n)))
                                 (when (case-property-p (subseq pattern (+ i 3) close))
                                   (return-from regex-case-fold-p nil))
                                 (setf i (1+ close))))
                              (t (incf i 3))))
                       ((and (char= c #\() (< (+ i 3) n) (string= "?<" pattern :start2 (1+ i) :end2 (+ i 3))
                             (not (find (char pattern (+ i 3)) "=!")))
                        ;; a named group: its name is no capital letter
                        (setf i (past #\> i)))
                       ((upper-case-p c) (return-from regex-case-fold-p nil))
                       ((char= c #\[)
                        (multiple-value-bind (after name) (posix-class-at pattern i)
                          (when (member (string-left-trim "^" (or name "")) '("upper" "lower") :test #'equal)
                            (return-from regex-case-fold-p nil))
                          (setf i (or after (1+ i)))))
                       (t (incf i))))))
    t))
