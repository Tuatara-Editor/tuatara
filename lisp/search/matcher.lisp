;;;; lisp/search/matcher.lisp --- How a pattern is found: the matcher of searches, filters and replacements.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One matcher finds a pattern, a regular expression or literal text, for
;;;; the search prompt, its replacements, the search filters and the search
;;;; of files; the words saying why a pattern cannot be searched for; the
;;;; symbol at a place and a text quoted to be found as it stands.

(in-package #:tuatara.defaults)

(defparameter *regex-problems*
  '(("Variable length look-behind" . "invalid: a lookbehind must have a fixed length")
    ("Quantifier '~A' not allowed" . "invalid: nothing for ~a to repeat")
    ("Expected end of string" . "invalid: a ) with no ( before it")
    ("Backreference to register ~A which has not been defined" . "invalid: no group ~a to refer back to")
    ("Illegal back-reference" . "invalid: no such group to refer back to")
    ("Register 0 doesn't exist" . "invalid: there is no group 0 to refer back to")
    ("Invalid range from ~S to ~S" . "invalid: the range ~a-~a runs backwards")
    ("Invalid character in named register group" . "invalid: a group's name holds letters and digits only")
    ("may not follow '(?<'" . "invalid: (?< goes on with =, ! or a group's name")
    ("may not follow '(?'" . "invalid: (?~a is no kind of group")
    ("Sequence '~A' not recognized" . "invalid: ~a is no kind of group")
    ("No character for hex-code" . "invalid: no character has that code")
    ("Branch test must be" . "invalid: a condition is a lookaround or a group's number")
    ("choices in branch" . "invalid: a condition takes one or two choices")
    ("Empty character class" . "invalid: an empty set")
    ("Opening paren has no matching closing paren" . "incomplete")
    ("Missing right bracket" . "incomplete")
    ("Unexpected end of string" . "incomplete")
    ("End of string following" . "incomplete")
    ("String ends with backslash" . "incomplete")
    ("Comment group not closed" . "incomplete")
    ("Unexpected EOF after" . "incomplete")
    ("Expected left brace after" . "incomplete")
    ("Character missing after" . "incomplete")
    ("in named group has no closing" . "incomplete"))
  "Words for the prompt on why a pattern does not compile: each (TEXT . WORDS),
TEXT a part of how cl-ppcre words the problem, WORDS a format string given
the things it names.")

(defun regex-problem (condition)
  "Words for the prompt on why a pattern does not compile, from CONDITION:
\"incomplete\" when it lacks a closing parenthesis or bracket or ends early,
else \"invalid\" and the reason in plain words (*REGEX-PROBLEMS*), or \"not supported\" and what
(TUATARA.SYNTAX:PATTERN-REFUSED). A problem not
known is said by where it is, never by what the program holds."
  (cond ((typep condition 'tuatara.syntax:pattern-refused) (tuatara.syntax:pattern-refused-words condition))
        ((typep condition 'cl-ppcre:ppcre-syntax-error)
         (let* ((control (simple-condition-format-control condition))
                (known (and (stringp control)
                            (find-if (lambda (text) (search text control)) *regex-problems* :key #'car))))
           (if known
               (apply #'format nil (cdr known)
                      (mapcar (lambda (thing) (if (or (stringp thing) (characterp thing) (realp thing)) thing "?"))
                              (simple-condition-format-arguments condition)))
               (let ((position (cl-ppcre:ppcre-syntax-error-pos condition)))
                 (format nil "invalid~@[ at character ~d~]" (and position (1+ position)))))))
        (t "invalid pattern")))

(define-record (matcher (:constructor %make-matcher (kind pattern scanner fold)) (:copier nil))
  "How a pattern is found in a text: KIND :LITERAL, its text PATTERN found as
octets, case ignored in ASCII when FOLD is true; or KIND :REGEX, SCANNER a
compiled regular expression (TUATARA.SYNTAX:COMPILE-REGEX), FOLD saying
whether it ignores case."
  (kind nil :documentation "How MATCHER finds its pattern: :LITERAL as octets, or :REGEX compiled.")
  (pattern nil :documentation "The pattern MATCHER finds, as it was given.")
  (scanner nil :documentation "MATCHER's compiled regular expression, for one of KIND :REGEX.")
  (fold nil :documentation "True when MATCHER ignores case."))

(defun make-pattern-matcher (pattern &key regex (fold (if regex
                                                           (tuatara.syntax:regex-case-fold-p pattern)
                                                           (tuatara.syntax:case-fold-p pattern)))
                                       compiled)
  "How to find PATTERN, a regular expression when REGEX is true, else literal
text: a MATCHER, or NIL and the reason the pattern does not compile. FOLD
ignores case, by default when the pattern has no capital letter and names no
case ([[:upper:]], \\p{Lu}). A regular expression with no special character is
found as literal text, which is several times faster and finds the same
matches. Not so when COMPILED asks for it compiled whatever it is (a
rectangle's rows are matched as text). Literal text is compiled when case is
ignored and it has letters beyond ASCII, whose other case only the regular
expression knows. It is also compiled when it holds a raw byte, which then
matches only a byte standing on its own, never one inside a character. ^ and
$ match at every kind of line end (TUATARA.SYNTAX:COMPILE-REGEX)."
  (let ((past-ascii (and fold (some (lambda (c) (and (>= (char-code c) 128) (both-case-p c))) pattern)))
        (raw (some (lambda (c) (tuatara.text:raw-byte-code-p (char-code c))) pattern)))
    (cond ((and (not regex) (or past-ascii raw))
           (%make-matcher :regex pattern (tuatara.syntax:compile-pattern pattern :regex nil :case-fold fold) fold))
          ((or (not regex)
               (and (not compiled) (not past-ascii) (plusp (length pattern))
                    (tuatara.syntax:literal-pattern-p pattern)))
           (%make-matcher :literal pattern nil fold))
          (t (handler-case
                 (reporting-errors ("compiling a search pattern")
                   (handler-case (%make-matcher :regex pattern
                                                (tuatara.syntax:compile-regex pattern :case-fold fold) fold)
                     ((or tuatara.syntax:pattern-refused cl-ppcre:ppcre-syntax-error) (e)
                       (values nil (regex-problem e)))))
               (error (e) (values nil (regex-problem e))))))))

(defun map-matcher-matches (function rope matcher from to &key (bound to) texts halt empty)
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED GROUPS) for each match of
MATCHER in ROPE starting in [FROM, TO) and ending by BOUND, in order, as one
scan from FROM finds them; MATCHED and GROUPS only with TEXTS, empty matches
only with EMPTY (literal text has none). True, stopped, when FUNCTION returns
true; NIL when HALT answers true, or all are done."
  (if (eq (matcher-kind matcher) :literal)
      (tuatara.syntax:map-literal-matches function rope (matcher-pattern matcher) from to
                                          :bound bound :case-fold (matcher-fold matcher) :texts texts :halt halt)
      (tuatara.syntax:map-regex-matches function rope (matcher-scanner matcher) from to
                                        :bound bound :texts texts :halt halt :empty empty)))

(defun rope-empty-match-at-p (rope scanner position)
  "True when SCANNER, a compiled regular expression, matches nothing just at
POSITION of ROPE, the end of a stretch searched, with the line before it for
context (4 KB at most)."
  (let ((text (tuatara.text:lenient-string
               (tuatara.text:rope-octets rope
                                         :start (max (tuatara.text:rope-line-start rope position) (- position 4096))
                                         :end position))))
    (multiple-value-bind (start end)
        (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start (length text) :real-start-pos 0))
      (and start (= start end (length text))))))

(defun stretch-matches (function rope matcher start end ends texts halt &optional (bound end))
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED GROUPS) for each match of
MATCHER in ROPE from START below END, the pattern seeing no text past BOUND
\(END unless given), empty ones too, one just after a match as well; with
ENDS, the empty match just at END as well, which a scan does not reach
\(EMPTY-MATCH-AT-END-P). MATCHED and GROUPS only with TEXTS. True, stopped,
when FUNCTION returns true; NIL when HALT, when given, answers true, or the
stretch is done."
  (if (eq (matcher-kind matcher) :literal)
      ;; plain text, found as octets: never an empty match
      (map-matcher-matches function rope matcher start end :bound bound :texts texts :halt halt)
      (or (map-matcher-matches function rope matcher start end :empty t :bound bound :texts texts :halt halt)
          (and ends (not (and halt (funcall halt))) (<= start end)
               (empty-match-at-end-p rope matcher end)
               (funcall function end end (and texts "") nil)))))

(defun empty-match-at-end-p (rope matcher end)
  "True when MATCHER, a regular expression, matches nothing just at END of
ROPE, the end of a stretch searched, where a scan of the stretch does not
reach; never just after a text's last newline, on the empty line that is no
line."
  (let ((size (tuatara.text:rope-octet-length rope)))
    (and (eq (matcher-kind matcher) :regex)
         (not (and (= end size) (plusp size) (= end (tuatara.text:rope-line-start rope end))))
         (rope-empty-match-at-p rope (matcher-scanner matcher) end))))

(defun map-match-units (function unit-of start end marked limit)
  "Call FUNCTION with (UNIT-START TEXT-END AFTER) for each line or record a
match from START to END lies on, in order. That is from the one holding its
first octet through the one holding its last, a line's end belonging to the
line it ends. An empty match lies on the one holding it when it is no further
on than that one's text end. Those before MARKED, given already, are left out,
and so are those starting at LIMIT or after. UNIT-OF, a function of a position
and MARKED, gives the three values for the one holding the position. Returns
where the one after the last looked at starts, MARKED for the next match."
  (declare (type fixnum start end marked limit))
  (let ((last (if (> end start) (1- end) start))
        (at (max start marked)))
    (declare (type fixnum last at))
    (loop while (and (<= at last) (or (< at limit) (= start end)))
          do (multiple-value-bind (unit-start text-end after) (funcall unit-of at marked)
               (declare (type fixnum unit-start text-end after))
               (when (>= unit-start limit) (return))
               (when (or (> end start) (<= at text-end))
                 (funcall function unit-start text-end after))
               (setf marked after)
               (when (<= after at) (return))
               (setf at after)))
    marked))

(defun matcher-search (rope matcher from limit &key (forward t) (bound limit) stop)
  "The first match of MATCHER in ROPE from FROM towards LIMIT (later when
FORWARD, else earlier), its start and end as two values (and for a regular
expression the matched text and groups), or NIL; a regular expression sees no
text past BOUND. Going forward a match starts at or after FROM, going back
before it. Empty matches are found too, as every count and replacement finds
them (STRETCH-MATCHES): going forward, the one just at LIMIT as well. STOP,
when given, is asked now and then whether to give way, and :STOPPED is the
end then."
  (let ((fold (matcher-fold matcher)))
    (if (eq (matcher-kind matcher) :literal)
        (if forward
            (tuatara.syntax:search-forward rope
                                           (matcher-pattern matcher)
                                           :start from
                                           :end limit
                                           :case-fold fold
                                           :stop stop)
            (tuatara.syntax:search-backward rope
                                            (matcher-pattern matcher)
                                            :start from
                                            :end limit
                                            :case-fold fold
                                            :stop stop))
        (if forward
            (multiple-value-call
                (lambda (&optional start end &rest more)
                  (cond (start (apply #'values start end more))
                        ((and (null end) (<= from limit) (= limit bound) (empty-match-at-end-p rope matcher limit))
                         (values limit limit "" nil))
                        (t (values start end))))
              (tuatara.syntax:regex-search-forward rope (matcher-scanner matcher) :start from :end limit :bound bound
                                                                                  :stop stop :empty t))
            (tuatara.syntax:regex-search-backward rope (matcher-scanner matcher) :start from :end limit :bound bound
                                                                                 :stop stop :empty t)))))

(defun symbol-char-p (char &optional (buffer (current-buffer)))
  "True for the characters of a symbol in BUFFER: letters, digits, _ and -, and
in Lisp every character a Lisp name may hold (LISP-NAME-PART-P: *foo*, 1+,
pkg:name)."
  (and char
       (or (word-char-p char)
           (find char "_-")
           (and (typep (buffer-major-mode buffer) 'lisp-mode) (lisp-name-part-p char)))))

(defun symbol-at (rope byte &optional (buffer (current-buffer)))
  "Two values: the start and end of the symbol in ROPE, BUFFER's text, at byte
offset BYTE, or just before it when BYTE ends one; NIL when there is none."
  (let ((total (tuatara.text:rope-octet-length rope)))
    (flet ((symbol-at-p (position) (symbol-char-p (tuatara.text:rope-char-at rope position) buffer))
           (before (position) (tuatara.text:rope-previous-boundary rope position)))
      (let ((at (cond ((and (< byte total) (symbol-at-p byte)) byte)
                      ((and (plusp byte) (symbol-at-p (before byte))) (before byte)))))
        (when at
          (let ((start at) (end at))
            (loop while (and (plusp start) (symbol-at-p (before start)))
                  do (setf start (before start)))
            (loop while (and (< end total) (symbol-at-p end))
                  do (setf end (tuatara.text:rope-next-boundary rope end)))
            (values start end)))))))

(defun symbol-chars-pattern (&optional (buffer (current-buffer)))
  "A set, written as a pattern, of the characters SYMBOL-CHAR-P takes for a
symbol's in BUFFER."
  (if (typep (buffer-major-mode buffer) 'lisp-mode)
      "[^\\s()\"'`,;|]"
      "[\\p{L}0-9_-]"))

(defun search-quote (text regex &key whole (parsed t))
  "TEXT written so that the search prompt finds it as it is, that is in a regular
expression (REGEX true) its special characters escaped, and with WHOLE true
kept from matching inside a longer symbol of the current buffer: no symbol
character (SYMBOL-CHAR-P) just before or after it. PARSED true, the default,
when the prompt reads a pattern and a replacement: then in literal text / and
\\ are escaped, and a % that would open the string; else literal text is
itself."
  (if (and (not regex) (not parsed))
      text
      (let ((quoted (with-output-to-string (out)
                      (loop for c across text
                            do (when (find c (if regex "\\^$.|?*+()[]{}/" "\\/")) (write-char #\\ out))
                               (write-char c out)))))
        (when (and regex whole (plusp (length text)))
          (let ((chars (symbol-chars-pattern)))
            (setf quoted (format nil "(?<!~a)~a(?!~a)" chars quoted chars))))
        (if (and (plusp (length quoted)) (char= (char quoted 0) #\%))
            (concatenate 'string "\\" quoted)
            quoted))))
