;;;; lisp/search/substitute.lisp --- Replacing from the search prompt, as vim's :substitute.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The search prompt reads what is typed the way vim reads :s: a string
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

(defstruct (search-line (:constructor make-search-line (&key whole-buffer pattern replacement flags problem))
                        (:copier nil))
  "A search string read as [%]pattern[/replacement[/flags]]: WHOLE-BUFFER true
after a leading %, the PATTERN, the REPLACEMENT after an unescaped / (NIL when
there is none, for a plain search), the FLAGS after a second /, and PROBLEM, the
reason the string cannot be used yet, or NIL."
  (whole-buffer nil)
  (pattern "" :type string)
  (replacement nil)
  (flags "" :type string)
  (problem nil))

(setf (documentation 'make-search-line 'function) "Return a search line with the given parts."
      (documentation 'search-line-p 'function) "True when OBJECT is a SEARCH-LINE."
      (documentation 'search-line-whole-buffer 'function) "True when the search line began with %: the whole buffer."
      (documentation 'search-line-pattern 'function) "The pattern of a search line."
      (documentation 'search-line-replacement 'function) "The replacement of a search line, or NIL for a plain search."
      (documentation 'search-line-flags 'function) "The flags of a search line, a string of g, c, n, i and I."
      (documentation 'search-line-problem 'function) "Why a search line cannot be used yet, or NIL.")

(defun split-search-line (string regex)
  "Split STRING at the slashes that separate a pattern, a replacement and flags,
into a list of one to three strings; the third takes the rest of STRING. A
slash does not count after a backslash, nor, when REGEX is true, inside a
set of the pattern such as [,/] or quoted text \\Q.../...\\E."
  (let ((parts '()) (start 0) (i 0) (n (length string)))
    (loop while (and (< i n) (< (length parts) 2))
          do (let ((c (char string i)))
               (cond ((and regex (null parts) (char= c #\\) (< (1+ i) n) (char= (char string (1+ i)) #\Q))
                      (setf i (let ((close (search "\\E" string :start2 (+ i 2)))) (if close (+ close 2) n))))
                     ((char= c #\\) (incf i 2))
                     ((and regex (null parts) (char= c #\[)) (setf i (tuatara.syntax:bracket-end string i)))
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
else \"invalid: \" and the reason in plain words (*REGEX-PROBLEMS*), or
\"not supported: \" and what (TUATARA.SYNTAX:PATTERN-REFUSED). A problem not
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

;;; How a pattern is found: one matcher for f, the search prompt and its replacements

(defstruct (matcher (:constructor %make-matcher (kind pattern scanner fold)) (:copier nil))
  "How a pattern is found in a text: KIND :LITERAL, its text PATTERN found as
octets, case ignored in ASCII when FOLD is true; or KIND :REGEX, SCANNER a
compiled regular expression (TUATARA.SYNTAX:COMPILE-REGEX), FOLD saying
whether it ignores case."
  kind pattern scanner fold)

(setf (documentation 'matcher-p 'function) "True when OBJECT is a MATCHER."
      (documentation 'matcher-kind 'function) "How MATCHER finds its pattern: :LITERAL as octets, or :REGEX compiled."
      (documentation 'matcher-pattern 'function) "The pattern MATCHER finds, as it was given."
      (documentation 'matcher-scanner 'function) "MATCHER's compiled regular expression, for one of KIND :REGEX."
      (documentation 'matcher-fold 'function) "True when MATCHER ignores case.")

(defun make-pattern-matcher (pattern &key regex (fold (if regex
                                                           (tuatara.syntax:regex-case-fold-p pattern)
                                                           (tuatara.syntax:case-fold-p pattern)))
                                       crlf compiled)
  "How to find PATTERN, a regular expression when REGEX is true, else literal
text: a MATCHER, or NIL and the reason the pattern does not compile. FOLD
ignores case, by default when the pattern has no capital letter and names no
case ([[:upper:]], \\p{Lu}). A regular
expression with no special character is found as literal text, which is
several times faster and finds the same matches, unless COMPILED asks for it
compiled whatever it is (a rectangle's rows are matched as text); literal text
is compiled when case is ignored and it has letters beyond ASCII, whose other
case only the regular expression knows, and when it holds a raw byte, which
then matches only a byte standing on its own, never one inside a character. With CRLF, for text whose lines end
with a carriage return and a newline, $ matches before the carriage return."
  (let ((past-ascii (and fold (some (lambda (c) (and (>= (char-code c) 128) (both-case-p c))) pattern)))
        (raw (some (lambda (c) (tuatara.text:raw-byte-code-p (char-code c))) pattern)))
    (cond ((and (not regex) (or past-ascii raw))
           (%make-matcher :regex pattern (tuatara.syntax:compile-pattern pattern :regex nil :case-fold fold) fold))
          ((or (not regex)
               (and (not compiled) (not past-ascii) (plusp (length pattern)) (tuatara.syntax:literal-pattern-p pattern)))
           (%make-matcher :literal pattern nil fold))
          (t (handler-case (%make-matcher :regex pattern (tuatara.syntax:compile-regex pattern :case-fold fold :crlf crlf) fold)
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
               (tuatara.text:rope-octets rope :start (max (tuatara.text:rope-line-start rope position) (- position 4096))
                                              :end position))))
    (multiple-value-bind (start end)
        (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start (length text) :real-start-pos 0))
      (and start (= start end (length text))))))

(defun stretch-matches (function rope matcher start end ends texts halt &optional (bound end))
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED GROUPS) for each match of
MATCHER in ROPE from START below END, the pattern seeing no text past BOUND
\(END unless given),
empty ones too, one just after a match as well; with ENDS, the empty match
just at END as well, which a scan does not reach (EMPTY-MATCH-AT-END-P).
MATCHED and
GROUPS only with TEXTS. True, stopped, when FUNCTION returns true; NIL when
HALT, when given, answers true, or the stretch is done."
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
  (let ((size (tuatara.text:rope-bytes rope)))
    (and (eq (matcher-kind matcher) :regex)
         (not (and (= end size) (plusp size) (= end (tuatara.text:rope-line-start rope end))))
         (rope-empty-match-at-p rope (matcher-scanner matcher) end))))

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
            (tuatara.syntax:search-forward rope (matcher-pattern matcher) :start from :end limit :case-fold fold :stop stop)
            (tuatara.syntax:search-backward rope (matcher-pattern matcher) :start from :end limit :case-fold fold :stop stop))
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

(defun search-line-matcher (line regex &optional buffer)
  "How to find LINE's pattern in BUFFER (MAKE-PATTERN-MATCHER): a MATCHER, or
NIL and the reason the pattern does not compile. REGEX tells whether it is a
regular expression; case is ignored as its flags say (SEARCH-LINE-FOLD); where
BUFFER's lines end with a carriage return and a newline, $ matches before the
carriage return."
  (make-pattern-matcher (search-line-pattern line) :regex regex :fold (search-line-fold line regex)
                                                   :crlf (and buffer (buffer-crlf-p buffer))))

(defun expand-replacement (replacement matched groups &optional (newline (string #\Newline)))
  "The text REPLACEMENT produces for a match of the text MATCHED whose groups
matched GROUPS (a vector, or NIL): & and \\0 stand for the whole match and \\1
to \\9 for a group; \\n and \\r for a line break, NEWLINE (the buffer's,
BUFFER-NEWLINE), and \\t for a tab; \\u and \\l
make the next character upper or lower case, \\U and \\L all that follows until
\\E or \\e. Any other escaped character, such as \\& \\/ or \\\\, stands for
itself."
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

(defun follow-case (text matched)
  "TEXT with the letter case of MATCHED carried over, for a replacement whose
pattern ignored case: all capitals when MATCHED has two or more letters, all
capitals; a capital at the start of each word when every word of MATCHED
starts with one and goes on in lower case; else TEXT as it is."
  (case-like text (match-case-kind matched)))

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
  (let* ((total (tuatara.text:rope-bytes rope))
         (at (cond ((and (< byte total) (symbol-char-p (tuatara.text:rope-char-at rope byte) buffer)) byte)
                   ((and (plusp byte)
                         (symbol-char-p (tuatara.text:rope-char-at rope (tuatara.text:rope-previous-boundary rope byte)) buffer))
                    (tuatara.text:rope-previous-boundary rope byte)))))
    (when at
      (let ((start at) (end at))
        (loop while (and (plusp start)
                         (symbol-char-p (tuatara.text:rope-char-at rope (tuatara.text:rope-previous-boundary rope start)) buffer))
              do (setf start (tuatara.text:rope-previous-boundary rope start)))
        (loop while (and (< end total) (symbol-char-p (tuatara.text:rope-char-at rope end) buffer))
              do (setf end (tuatara.text:rope-next-boundary rope end)))
        (values start end)))))

(defun symbol-chars-pattern (&optional (buffer (current-buffer)))
  "A set, written as a pattern, of the characters SYMBOL-CHAR-P takes for a
symbol's in BUFFER."
  (if (typep (buffer-major-mode buffer) 'lisp-mode)
      "[^\\s()\"'`,;|]"
      "[\\p{L}0-9_-]"))

(defun search-quote (text regex &key whole (parsed t))
  "TEXT written so that the search prompt finds it as it is: in a regular
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

(defvar *replacement-budget* nil
  "Octets of heap a replacement may plan to use, or NIL for a quarter of what is
free when it starts. Matches beyond it are not collected, so a replacement too
large to hold in memory is refused before the heap runs out.")

(defun replacement-budget ()
  "The octets a replacement may plan to use now (*REPLACEMENT-BUDGET*)."
  (tuatara.runtime:heap-budget *replacement-budget*))

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

(defun count-line-matches (rope matcher low high &key stop (bound high))
  "How many matches LINE-MATCHES finds in ROPE from LOW to HIGH, counted in
parallel without keeping any, so that a count of millions takes no memory:
two values, the count and NIL, or what was counted and :STOPPED once STOP
answered true. With BOUND past HIGH, the matches that start before HIGH in
a scan of the text up to BOUND: those of a count to BOUND that come before
HIGH."
  (multiple-value-bind (found status count)
      (tuatara.syntax:collect-matches
       (lambda (function from to halt)
         (stretch-matches function rope matcher from to (and (eql to high) (eql high bound)) nil halt bound))
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
    (let* ((pieces (coerce (tuatara.text:rope-line-pieces rope low high tuatara.syntax:*parallel-piece*) 'simple-vector))
           (results (tuatara.runtime:run-tasks
                     (map 'simple-vector (lambda (piece)
                                           (lambda () (multiple-value-list (count-piece (first piece) (second piece) 0))))
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
what it replaces followed when FOLD, its pattern ignoring case (FOLLOW-CASE),
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
                           collect (if (= j i) (cons (max pos (car (aref pieces j))) (cdr (aref pieces j))) (aref pieces j)))))
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
         (rest-out-of-sight (and lines (or (> (car (first lines)) low) (< (cdr (car (last lines))) high) (rest lines)))))
    (multiple-value-bind (found status) (and rest-out-of-sight (pieces-line-matches rope matcher regex lines all))
      (if (or (null found) status)
          (start-rewrite buffer line regex low high words flags)
          (let* ((shifts (loop for m in found
                               collect (cons (second m) (- (tuatara.text:string-octet-length (funcall replacement-for m))
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
                                         (:done (format nil "~a ~a ~a"
                                                        (replaced-words (+ shown count) lines) words (job-done-words seconds kept)))
                                         (:stopped (format nil "Stopped: ~:d replaced in sight, the rest left as it was" shown)))))))))))

(defun run-substitution (buffer line regex low high words origin &key (flags (search-line-flags line)) stop pieces)
  "Carry out LINE, a search line with a replacement, in BUFFER on the matches
that start at or after LOW and end by HIGH, the scope WORDS names (\"on this
line\"). REGEX tells whether the pattern is a regular expression, and so
whether the replacement reads & and \\1 (EXPAND-REPLACEMENT) or is literal text
(LITERAL-REPLACEMENT). FLAGS, those typed and those of the command that opened
the prompt: without g only the first match on each line is replaced; c asks at
each match; n only counts. When the pattern ignored case, each replacement
takes the case of what it replaces (FOLLOW-CASE). The replacements are one edit
batch and point stays at ORIGIN. A scope of *REWRITE-THRESHOLD* octets or
more, matches too many to hold in memory, or a pattern so slow on this text
that one line holds it up (TUATARA.RUNTIME:*SLOW-CALL-SECONDS*), go to a replacement in the
background instead (START-REWRITE), and the editor stays usable. PIECES, when
given, are the stretches (START . END) in order within LOW and HIGH the
replacement is limited to, the cursors' lines say; their matches are gathered
here, never in the background. A match a
fold hides is replaced too, whatever the scope, the fold kept folded; asked at
with c, its fold is opened first (QUERY-REPLACE-LOOP). When STOP
answers true while matches are gathered, nothing changes and a message says
so. Returns the number of matches replaced or counted here."
  (let ((matcher (search-line-matcher line regex buffer)))
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
        (pieces
         (multiple-value-bind (found status count)
             (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
               (pieces-line-matches (tuatara.buffer:buffer-text buffer) matcher regex pieces all :stop stop
                                    :count-only (find #\n flags)))
           (goto origin)
           (cond ((eq status :slow) (message "The pattern is too slow here to replace with: nothing changed") 0)
                 (status (report-unreplaced status count words (find #\n flags)) 0)
                 ((null found) (message "No match ~a" words) 0)
                 (t (let ((lines (matched-lines (tuatara.buffer:buffer-text buffer) found)))
                      (if (find #\n flags)
                          (progn (message "~a" (matches-on-lines-words (length found) lines))
                                 (length found))
                          (let ((replaced (replace-matches buffer found replacement-for)))
                            (message "~a" (replaced-words replaced lines))
                            replaced)))))))
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
                                                  ;; too many to hold: those in sight at once, the rest in the background
                                                  (progn (rewrite-after-sight buffer line regex pos high words flags matcher replacement-for all)
                                                         (values nil :background count))
                                                  (values (if all found (first-per-line rope found)) status count)))))))))
        (t
          (let ((rope (tuatara.buffer:buffer-text buffer)) (counting (find #\n flags)))
            (when (>= (- high low) *rewrite-threshold*)
              ;; a large scope: what is in sight at once, the rest in the background, so nothing waits on it
              (goto origin)
              (rewrite-after-sight buffer line regex low high words flags matcher replacement-for all)
              (return-from run-substitution 0))
            (multiple-value-bind (found status count)
                (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                  (line-matches rope matcher regex low high :stop stop :count-only counting))
              (goto origin)
              (when (member status '(:over :slow))
                ;; too many to hold in memory: what is in sight at once, the rest in the background;
                ;; a pattern slow on this text: all of it in the background
                (if (eq status :over)
                    (rewrite-after-sight buffer line regex low high words flags matcher replacement-for all)
                    (start-rewrite buffer line regex low high words flags :slow t))
                (return-from run-substitution 0))
              (if status
                  (progn (report-unreplaced status count words counting) 0)
                  (let* ((matches (if all found (first-per-line rope found)))
                         (lines (matched-lines rope matches)))
                    (cond ((null matches) (message "No match ~a" words) 0)
                          (counting
                           (message "~a" (matches-on-lines-words (length matches) lines))
                           (length matches))
                          (t (let ((replaced (replace-matches buffer matches replacement-for)))
                               (message "~a" (replaced-words replaced lines))
                               replaced))))))))))))

(defun report-unreplaced (status count words counting)
  "Say why a replacement or count gathered COUNT matches WORDS (\"in the
buffer\") and stopped without changing anything: STATUS :STOPPED for a key
typed meanwhile, :OVER for more matches than memory can hold."
  (ecase status
    (:stopped (message "Stopped after ~:d match~:*~[es~;~:;es~]; nothing ~:[replaced~;counted~]" count counting))
    (:over (message "Over ~:d matches ~a: too many to ~:[replace~;count~] in memory; nothing changed"
                    count words counting))))
