;;;; src/text/strings.lisp --- Helpers for strings and octets that are not the rope's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

(defun octets-latin-1 (octets &key (start 0) end)
  "OCTETS from START below END (or their end) as Latin-1 text: each octet the
character of its code, as ASCII and ISO 8859-1 read it."
  (let* ((end (or end (length octets)))
         (out (make-string (- end start))))
    (loop for i from start below end
          for k from 0
          do (setf (schar out k) (code-char (aref octets i))))
    out))

(defun utf-16-text (octets &key (start 0) (end (length octets)) big-endian)
  "OCTETS from START below END as UTF-16 text, least significant octet first
unless BIG-ENDIAN or a byte-order mark says otherwise; the mark, a null (NUL)
at its end and what follows left out."
  (let* ((end (- end (mod (- end start) 2)))
         (big (cond ((and (< (1+ start) end) (= (aref octets start) #xFE) (= (aref octets (1+ start)) #xFF))
                     (incf start 2) t)
                    ((and (< (1+ start) end) (= (aref octets start) #xFF) (= (aref octets (1+ start)) #xFE))
                     (incf start 2) nil)
                    (t big-endian)))
         (text (or (tuatara.runtime:ignoring-errors-reported ("reading ~d octets as UTF-16" (- end start))
                     (sb-ext:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                                              :start start :end end
                                              :external-format (list (if big :utf-16be :utf-16le) :replacement #\?)))
                   "")))
    (subseq text 0 (or (position (code-char 0) text) (length text)))))

(defparameter *charset-aliases*
  '(("us-ascii" . :latin-1) ("ascii" . :latin-1) ("ansi_x3.4-1968" . :latin-1) ("utf8" . :utf-8) ("latin1" . :latin-1)
    ("iso-8859-8-i" . :iso-8859-8) ("gb2312" . :gbk) ("x-gbk" . :gbk) ("cp936" . :gbk) ("cp932" . :shift_jis)
    ("windows-31j" . :shift_jis) ("x-sjis" . :shift_jis) ("unicode" . :utf-16le) ("utf-16" . :utf-16le)
    ("unicode-1-1-utf-7" . nil) ("utf-7" . nil))
  "Charsets that mail and web pages name otherwise than the system's
encodings are named, each (NAME . ENCODING), ENCODING NIL for one read as
UTF-8.")

(defun charset-encoding (charset)
  "The encoding the charset named CHARSET (as mail and web pages name one:
utf-8, iso-8859-1, windows-1252, shift_jis) stands for, a keyword the system
decodes; NIL for UTF-8 or a charset not known."
  (when (and charset (plusp (length charset)) (< (length charset) 40))
    (let* ((name (string-downcase (string-trim " \"'" charset)))
           (alias (assoc name *charset-aliases* :test #'string=))
           (encoding (if alias (cdr alias) (find-symbol (string-upcase name) :keyword))))
      (and encoding (not (eq encoding :utf-8))
           (tuatara.runtime:ignoring-errors-reported ("reading the charset ~a" charset)
             (sb-ext:octets-to-string (make-array 0 :element-type '(unsigned-byte 8)) :external-format encoding))
           encoding))))

(defun charset-text (octets charset &key (start 0) (end (length octets)))
  "OCTETS from START below END as text in the charset named CHARSET
\(CHARSET-ENCODING): an octet the charset has no character for as U+FFFD; in
UTF-8, or a charset not known, as OCTETS-STRING reads them, an octet that is
not UTF-8 kept as its raw byte."
  (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
        (encoding (charset-encoding charset)))
    (or (and encoding
             (tuatara.runtime:ignoring-errors-reported ("reading text in the charset ~a" charset)
               (sb-ext:octets-to-string octets :start start :end end
                                               :external-format (list encoding :replacement (code-char #xFFFD)))))
        (octets-string octets :start start :end end))))

(defun word-char-p (char)
  "True for characters that make up words: letters and digits."
  (and char (or (alpha-char-p char) (digit-char-p char))))

(defun letter-or-mark-p (char)
  "True for a character words of a language are made of: a letter, or a mark
joined to one (the accent of a decomposed é, the vowel signs of Devanagari);
never a digit, as WORD-CHAR-P takes."
  (or (alpha-char-p char) (and (member (sb-unicode:general-category char) '(:mn :mc :me)) t)))

(defun name-char-p (char)
  "True for characters that make up a name, a word with _ in it as well:
letters, digits and _."
  (or (word-char-p char) (eql char #\_)))

(defparameter *blank-characters* '(#\Space #\Tab #\Newline #\Return #\Page)
  "The characters that are blanks between words in text: space, tab, newline,
return and form feed; a list STRING-TRIM takes.")

(declaim (inline blank-char-p))
(defun blank-char-p (char)
  "True when CHAR is one of *BLANK-CHARACTERS*: a space, tab, newline, return
or form feed."
  (member char *blank-characters*))

(defun lowercase-hex-p (text &optional length)
  "True when TEXT is made of hexadecimal digits in lower case alone, and when
LENGTH is given, of exactly that many (64 for a SHA-256 sum as sha256sum writes it)."
  (and (stringp text) (plusp (length text))
       (or (null length) (= (length text) length))
       (every (lambda (c) (find c "0123456789abcdef")) text)))

(declaim (inline space-or-tab-char-p))
(defun space-or-tab-char-p (char)
  "True for a space or a tab: the characters between a line's fields, and those
a blank line holds."
  (or (char= char #\Space) (char= char #\Tab)))

(defun blank-text-p (string &optional (start 0) end)
  "True when STRING from START below END (its end when NIL) holds nothing but
spaces and tabs, or nothing at all."
  (not (position-if-not #'space-or-tab-char-p string :start start :end end)))

(defun split-words (string &optional (separators '(#\Space #\Tab)))
  "The parts of STRING between SEPARATORS (characters, or a string of them),
the empty ones left out; NIL for NIL."
  (and string (remove "" (uiop:split-string string :separator separators) :test #'string=)))

(defun command-words (text &key (backslash :shell))
  "TEXT split into a program's arguments as a shell splits a command line,
though no shell runs. It is split at spaces, tabs and line ends, but within
'...', which keeps all it holds as it is, or \"...\", either quote holding the
other.
BACKSLASH says what a backslash does. With :SHELL, outside quotes it keeps the
character after it as it is. Within \"...\" it keeps the \" or \\ after it, and
is itself before any other. With :ESCAPES, outside quotes it is itself (a
Windows path comes through whole). Within \"...\" it begins one of \\t \\n \\r
\\\\ and \\\", a tab, a line end, a carriage return, a backslash and a quote,
and any other is refused.
Two values, the words and T; or NIL, NIL and why as a third when a quote is
left open or an escape is refused."
  (let ((words '()) (word (make-string-output-stream)) (in-word nil) (quote-char nil) (i 0) (n (length text)))
    (flet ((end-word () (when in-word (push (get-output-stream-string word) words) (setf in-word nil)))
           (next () (and (< (1+ i) n) (char text (1+ i)))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((eql quote-char #\')
                        (if (char= c #\') (setf quote-char nil) (write-char c word)))
                       ((eql quote-char #\")
                        (cond ((char= c #\") (setf quote-char nil))
                              ((char/= c #\\) (write-char c word))
                              ((eq backslash :escapes)
                               (let ((escaped (case (next)
                                                (#\t #\Tab)
                                                (#\n #\Newline)
                                                (#\r #\Return)
                                                (#\\ #\\)
                                                (#\" #\"))))
                                 (unless escaped
                                   (return-from command-words
                                     (values nil nil (format nil "within double quotes a backslash begins an escape, ~
                                                                  and only \\t \\n \\r \\\\ \\\" are taken: ~a; ~
                                                                  single quotes take a backslash as it is"
                                                             (subseq text i (min n (+ i 2)))))))
                                 (incf i) (write-char escaped word)))
                              ((member (next) '(#\" #\\)) (incf i) (write-char (char text i) word))
                              (t (write-char c word))))
                       ((or (blank-char-p c) (char= c (code-char 11))) (end-word))
                       ((member c '(#\' #\")) (setf quote-char c in-word t))
                       ((and (char= c #\\) (eq backslash :shell) (next)) (incf i) (write-char (char text i) word)
                        (setf in-word t))
                       (t (write-char c word) (setf in-word t))))
               (incf i))
      (if quote-char
          (values nil nil "a quote is left open")
          (progn (end-word) (values (nreverse words) t))))))

(defun shell-quoted (string)
  "STRING quoted for the shell, as one word whatever it holds."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for c across string
          do (if (char= c #\') (write-string "'\\''" out) (write-char c out)))
    (write-char #\' out)))

(defun software-version-numbers (text &optional (after ""))
  "The numbers of the first version written in TEXT after AFTER, a regular
expression, as a list: (0 11 0) for \"bubblewrap 0.11.0\", (9 9) for
\"OpenSSH_9.9p1, OpenSSL 3.2.2\" after \"OpenSSH_\"; NIL when there is none.
A pre-release after a -, alpha, beta or rc and its number, adds -3, -2 or -1
and that number, so that it comes before its release (SOFTWARE-VERSION-AT-LEAST-P):
0.90.13-rc1 is (0 90 13 -1 1)."
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings (concatenate 'string after "(\\d+(?:\\.\\d+)*)(?:-(alpha|beta|rc)\\.?(\\d*))?") text)
    (when match
      (append (mapcar #'parse-integer (uiop:split-string (aref parts 0) :separator "."))
              (let ((label (aref parts 1)))
                (and label (list (cond ((string= label "alpha") -3) ((string= label "beta") -2) (t -1))
                                 (or (parse-integer (aref parts 2) :junk-allowed t) 0))))))))

(defun software-version-at-least-p (version needed)
  "True when VERSION, a list of numbers as SOFTWARE-VERSION-NUMBERS gives, is NEEDED,
another, or later: each number compared in turn, one left out counted 0."
  (loop for i below (max (length version) (length needed))
        for have = (or (nth i version) 0) and need = (or (nth i needed) 0)
        when (> have need) return t
        when (< have need) return nil
        finally (return t)))

(defun octets-words (n &key binary compact)
  "N octets in words, to one decimal: 950 B, 12.3 MB, 11.5 GB; with BINARY in
units of 1,024, as 950 B, 954.0 MiB, 1.2 TiB. With COMPACT, as narrow as a
column of sizes wants it, in units of 1,024 with one letter for the unit:
260, 4.1K, 24K, 1.0M; below 10 with one decimal, every figure rounded up."
  (if compact
      (if (< n 1024)
          (princ-to-string n)
          (loop for unit across "KMGTPEZY"
                for scale = 1024 then (* scale 1024)
                do (let ((value (/ n scale)))
                     (if (< value 10)
                         (let ((tenths (ceiling (* 10 value))))
                           (return (if (< tenths 100)
                                       (format nil "~d.~d~c" (floor tenths 10) (mod tenths 10) unit)
                                       (format nil "10~c" unit))))
                         (let ((whole (ceiling value)))
                           (when (< whole 1024)
                             (return (format nil "~d~c" whole unit))))))))
      (let ((base (if binary 1024 1000)))
        (loop for (unit . more) on (if binary '("B" "KiB" "MiB" "GiB" "TiB" "PiB") '("B" "kB" "MB" "GB" "TB"))
              for scale = 1 then (* scale base)
              when (or (< n (* scale base)) (null more))
                return (if (= scale 1) (format nil "~d B" n) (format nil "~,1f ~a" (/ n scale) unit))))))

(defun spaces (count)
  "A new string of COUNT spaces; an empty one when COUNT is not above zero."
  (make-string (max 0 count) :initial-element #\Space))

(defun search-count (needle text &key (end (length text)))
  "How many times NEEDLE stands in TEXT below END, none overlapping; 0 for an
empty NEEDLE."
  (if (zerop (length needle))
      0
      (loop with start = 0
            for at = (search needle text :start2 start :end2 end)
            while at count t do (setf start (+ at (length needle))))))

(defun nul-fields (octets &key (start 0) (end (length octets)))
  "The fields of OCTETS from START below END, a program's output with a null
\(NUL) after each field (git's -z, find's -print0), as strings, decoded as
OCTETS-STRING decodes them; text after the last NUL is a field too, an empty
one left out. Empty fields between NULs are kept, for records of a fixed
number of fields."
  (loop for from = start then (1+ at)
        for at = (or (position 0 octets :start from :end end) end)
        when (or (< at end) (< from at))
          collect (octets-string octets :start from :end at)
        while (< at end)))

(defun whole-number (text &key (start 0) end (radix 10))
  "TEXT, a string, or its part from START to END, read as a whole number in
RADIX: digits, a + or - right before them, and spaces, tabs or line breaks
around, as CL:PARSE-INTEGER reads them (\" -42\" is -42, \"ff\" in radix 16
is 255). NIL when TEXT is not a string, when START or END are not within it,
and when the part is anything else: empty, blanks alone, a sign alone, a
stray character anywhere."
  (when (stringp text)
    (let ((end (or end (length text))))
      (when (and (integerp start) (integerp end) (<= 0 start end (length text)))
        (let ((at start))
          (loop while (and (< at end) (blank-char-p (char text at))) do (incf at))
          (when (and (< at end) (member (char text at) '(#\+ #\-))) (incf at))
          (let ((digits at))
            (loop while (and (< at end) (digit-char-p (char text at) radix)) do (incf at))
            (when (> at digits)
              (loop while (and (< at end) (blank-char-p (char text at))) do (incf at))
              (when (= at end)
                (parse-integer text :start start :end end :radix radix)))))))))

(defun parse-decimal (text &key (most-digits 12))
  "TEXT, digits with perhaps a point and more digits after (as 595.276), as
an exact number; NIL when it is anything else or has more than MOST-DIGITS
digits. Only the digits and the point are read, never anything else."
  (let ((point (position #\. text)))
    (and (plusp (length text))
         (<= (length text) (1+ most-digits))
         (every (lambda (c) (or (digit-char-p c) (char= c #\.))) text)
         (= (count #\. text) (if point 1 0))
         (let ((whole (subseq text 0 (or point (length text))))
               (part (if point (subseq text (1+ point)) "")))
           (and (plusp (+ (length whole) (length part)))
                (+ (if (plusp (length whole)) (parse-integer whole) 0)
                   (if (plusp (length part)) (/ (parse-integer part) (expt 10 (length part))) 0)))))))

(defparameter *hex-pairs*
  (let ((pairs (make-array 256)))
    (dotimes (i 256 pairs)
      (setf (aref pairs i) (coerce (format nil "~(~2,'0x~)" i) 'simple-base-string))))
  "The two lower-case hex digits of each octet, by its value.")

(defun octets-hex (octets &key (start 0) end upper-case separator)
  "OCTETS from START below END (or their end) written in hexadecimal, two
digits each, in lower case unless UPPER-CASE, SEPARATOR (a string) between
each two when given."
  (let* ((end (or end (length octets)))
         (count (max 0 (- end start)))
         (gap (length (or separator "")))
         (out (make-string (max 0 (- (* count (+ 2 gap)) gap))))
         (at 0))
    (declare (type fixnum end count gap at))
    (loop for i from start below end
          do (when (and separator (> i start))
               (replace out separator :start1 at)
               (incf at gap))
             (let ((pair (svref *hex-pairs* (aref octets i))))
               (setf (char out at) (if upper-case (char-upcase (char pair 0)) (char pair 0))
                     (char out (1+ at)) (if upper-case (char-upcase (char pair 1)) (char pair 1)))
               (incf at 2)))
    out))

;;; Hashes of text

(defun string-hash (text)
  "A hash of the string TEXT: FNV-1a over its octets in UTF-8, as a fixnum,
the same in every version of Tuatara and of the Lisp it runs on."
  (ldb (byte 62 0) (octets-hash (string-octets text))))

;;; Lines of a string

(defun ends-with-newline-p (string)
  "True when STRING ends with a newline."
  (and (plusp (length string)) (char= (char string (1- (length string))) #\Newline)))

(defun ensure-final-newline (string)
  "STRING ending with a newline: itself when it does, else with one added."
  (if (ends-with-newline-p string)
      string
      (concatenate 'string string (string #\Newline))))

(defun text-lines (text)
  "The lines of TEXT, the newline it ends with not making one more; none for an empty TEXT."
  (and (plusp (length text))
       (uiop:split-string (if (ends-with-newline-p text) (subseq text 0 (1- (length text))) text)
                          :separator '(#\Newline))))

(defun string-line-start (text position)
  "Where the line of the string TEXT holding POSITION begins: just after the
newline before POSITION, or 0."
  (let ((newline (and (plusp position) (position #\Newline text :end (min position (length text)) :from-end t))))
    (if newline (1+ newline) 0)))

(defun string-line-end (text position)
  "Where the line of the string TEXT holding POSITION ends: the newline at or
after POSITION, or the end of TEXT."
  (or (position #\Newline text :start (min position (length text))) (length text)))

;;; Ranges of octets

(defun merge-ranges (ranges &key (touching t))
  "RANGES, each (START . END), in any order, put in order of their starts, with
those that overlap made one, and with TOUCHING (the default) those that touch
too, one ending where the next starts. RANGES itself is left as it was."
  (let ((merged '()))
    (dolist (range (sort (copy-list ranges) #'< :key #'car) (nreverse merged))
      (if (and merged (if touching
                          (<= (car range) (cdr (first merged)))
                          (< (car range) (cdr (first merged)))))
          (setf (cdr (first merged)) (max (cdr (first merged)) (cdr range)))
          (push (cons (car range) (cdr range)) merged)))))

(defparameter *number-width-most* 1000
  "The widest a number may be padded to by NUMBER-FORMATTER's %d.")

(defun number-formatter (control)
  "A function of a number giving the text CONTROL makes of it: CONTROL's
characters as they are, %d (or %s) standing for the number, with - before
the width to put it on the left, 0 to pad with zeros and a width (%3d, %-3d,
%03d), and %% for a percent sign. An error for any other %, and for a width
over *NUMBER-WIDTH-MOST*."
  (let ((parts '()) (i 0) (n (length control)))
    (loop while (< i n)
          do (let ((c (char control i)))
               (if (char/= c #\%)
                   (progn (push (string c) parts) (incf i))
                   (let ((j (1+ i)) (left nil) (zeros nil))
                     (loop while (and (< j n) (find (char control j) "-0"))
                           do (if (char= (char control j) #\-) (setf left t) (setf zeros t))
                              (incf j))
                     (let ((width-end (or (position-if-not #'digit-char-p control :start j) n)))
                       (cond ((and (= j (1+ i)) (< j n) (char= (char control j) #\%))
                              (push "%" parts) (setf i (1+ j)))
                             ((and (< width-end n) (find (char control width-end) "ds") (> width-end j)
                                   (or (> (- width-end j) 4)
                                       (> (parse-integer control :start j :end width-end) *number-width-most*)))
                              (error "Format string: a width of more than ~:d is not taken" *number-width-most*))
                             ((and (< width-end n) (find (char control width-end) "ds"))
                              (push (list (if (> width-end j)
                                              (parse-integer control :start j :end width-end)
                                              0) left zeros)
                                    parts)
                              (setf i (1+ width-end)))
                             (t (error "Format string: ~a is not understood; %d stands for the number"
                                       (subseq control i (min n (1+ width-end)))))))))))
    (setf parts (nreverse parts))
    (lambda (number)
      (with-output-to-string (out)
        (dolist (part parts)
          (if (stringp part)
              (write-string part out)
              (destructuring-bind (width left zeros) part
                (let* ((digits (princ-to-string number))
                       (pad (max 0 (- width (length digits)))))
                  (cond (left (write-string digits out) (write-string (spaces pad) out))
                        (zeros (when (minusp number) (write-char #\- out) (setf digits (subseq digits 1)))
                               (write-string (make-string pad :initial-element #\0) out)
                               (write-string digits out))
                        (t (write-string (spaces pad) out)
                           (write-string digits out)))))))))))

(defun natural-string-lessp (a b)
  "True when the string A comes before B in the order people sort names in:
letters alphabetically whatever their case, and a run of digits by the number
it writes (file2 before file10, Notes before notes.txt); two names alike but
for case, or for zeros before a number, in the order their characters' codes
give. Complexity: O(length of the shorter), nothing made."
  (declare (type string a b) (optimize speed))
  (let ((la (length a)) (lb (length b)) (i 0) (j 0))
    (declare (type fixnum la lb i j))
    (flet ((digit-p (c) (char<= #\0 c #\9)))
      (declare (inline digit-p))
      (loop
        (cond ((>= i la) (return (if (< j lb) t (and (string< a b) t))))
              ((>= j lb) (return nil)))
        (let ((x (char a i)) (y (char b j)))
          (if (and (digit-p x) (digit-p y))
              ;; a number each: the one with fewer digits, its leading zeros left out, the smaller
              (let ((sa i) (sb j) (ea i) (eb j))
                (declare (type fixnum sa sb ea eb))
                (loop while (and (< sa la) (char= #\0 (char a sa))) do (incf sa))
                (loop while (and (< sb lb) (char= #\0 (char b sb))) do (incf sb))
                (setf ea sa eb sb)
                (loop while (and (< ea la) (digit-p (char a ea))) do (incf ea))
                (loop while (and (< eb lb) (digit-p (char b eb))) do (incf eb))
                (unless (= (- ea sa) (- eb sb))
                  (return (< (- ea sa) (- eb sb))))
                (loop for p of-type fixnum from sa below ea
                      for q of-type fixnum from sb
                      do (let ((d (char a p)) (e (char b q)))
                           (unless (char= d e) (return-from natural-string-lessp (char< d e)))))
                (setf i ea j eb))
              (let ((fx (char-downcase x)) (fy (char-downcase y)))
                (cond ((char< fx fy) (return t))
                      ((char> fx fy) (return nil))
                      (t (incf i) (incf j))))))))))
