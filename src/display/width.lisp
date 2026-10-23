;;;; src/display/width.lisp --- Display width of characters in a monospace grid.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.display)

;; The two tables below are generated from the Unicode Character Database:
;; EastAsianWidth.txt of version 16.0 (the W and F characters, with the
;; regional indicators, which terminals draw two wide as Emoji_Presentation
;; says) and emoji-variation-sequences.txt of version 18.0 (the bases of an
;; emoji sequence that are otherwise one column wide).

(defparameter *wide-ranges*
  '((#x1100 . #x115F) (#x231A . #x231B) (#x2329 . #x232A) (#x23E9 . #x23EC) (#x23F0 . #x23F0)
    (#x23F3 . #x23F3) (#x25FD . #x25FE) (#x2614 . #x2615) (#x2630 . #x2637) (#x2648 . #x2653)
    (#x267F . #x267F) (#x268A . #x268F) (#x2693 . #x2693) (#x26A1 . #x26A1) (#x26AA . #x26AB)
    (#x26BD . #x26BE) (#x26C4 . #x26C5) (#x26CE . #x26CE) (#x26D4 . #x26D4) (#x26EA . #x26EA)
    (#x26F2 . #x26F3) (#x26F5 . #x26F5) (#x26FA . #x26FA) (#x26FD . #x26FD) (#x2705 . #x2705)
    (#x270A . #x270B) (#x2728 . #x2728) (#x274C . #x274C) (#x274E . #x274E) (#x2753 . #x2755)
    (#x2757 . #x2757) (#x2795 . #x2797) (#x27B0 . #x27B0) (#x27BF . #x27BF) (#x2B1B . #x2B1C)
    (#x2B50 . #x2B50) (#x2B55 . #x2B55) (#x2E80 . #x2E99) (#x2E9B . #x2EF3) (#x2F00 . #x2FD5)
    (#x2FF0 . #x303E) (#x3041 . #x3096) (#x3099 . #x30FF) (#x3105 . #x312F) (#x3131 . #x318E)
    (#x3190 . #x31E5) (#x31EF . #x321E) (#x3220 . #x3247) (#x3250 . #xA48C) (#xA490 . #xA4C6)
    (#xA960 . #xA97C) (#xAC00 . #xD7A3) (#xF900 . #xFAFF) (#xFE10 . #xFE19) (#xFE30 . #xFE52)
    (#xFE54 . #xFE66) (#xFE68 . #xFE6B) (#xFF01 . #xFF60) (#xFFE0 . #xFFE6) (#x16FE0 . #x16FE4)
    (#x16FF0 . #x16FF1) (#x17000 . #x187F7) (#x18800 . #x18CD5) (#x18CFF . #x18D08) (#x1AFF0 . #x1AFF3)
    (#x1AFF5 . #x1AFFB) (#x1AFFD . #x1AFFE) (#x1B000 . #x1B122) (#x1B132 . #x1B132) (#x1B150 . #x1B152)
    (#x1B155 . #x1B155) (#x1B164 . #x1B167) (#x1B170 . #x1B2FB) (#x1D300 . #x1D356) (#x1D360 . #x1D376)
    (#x1F004 . #x1F004) (#x1F0CF . #x1F0CF) (#x1F18E . #x1F18E) (#x1F191 . #x1F19A) (#x1F1E6 . #x1F202)
    (#x1F210 . #x1F23B) (#x1F240 . #x1F248) (#x1F250 . #x1F251) (#x1F260 . #x1F265) (#x1F300 . #x1F320)
    (#x1F32D . #x1F335) (#x1F337 . #x1F37C) (#x1F37E . #x1F393) (#x1F3A0 . #x1F3CA) (#x1F3CF . #x1F3D3)
    (#x1F3E0 . #x1F3F0) (#x1F3F4 . #x1F3F4) (#x1F3F8 . #x1F43E) (#x1F440 . #x1F440) (#x1F442 . #x1F4FC)
    (#x1F4FF . #x1F53D) (#x1F54B . #x1F54E) (#x1F550 . #x1F567) (#x1F57A . #x1F57A) (#x1F595 . #x1F596)
    (#x1F5A4 . #x1F5A4) (#x1F5FB . #x1F64F) (#x1F680 . #x1F6C5) (#x1F6CC . #x1F6CC) (#x1F6D0 . #x1F6D2)
    (#x1F6D5 . #x1F6D7) (#x1F6DC . #x1F6DF) (#x1F6EB . #x1F6EC) (#x1F6F4 . #x1F6FC) (#x1F7E0 . #x1F7EB)
    (#x1F7F0 . #x1F7F0) (#x1F90C . #x1F93A) (#x1F93C . #x1F945) (#x1F947 . #x1F9FF) (#x1FA70 . #x1FA7C)
    (#x1FA80 . #x1FA89) (#x1FA8F . #x1FAC6) (#x1FACE . #x1FADC) (#x1FADF . #x1FAE9) (#x1FAF0 . #x1FAF8)
    (#x20000 . #x2FFFD) (#x30000 . #x3FFFD))
  "Code point ranges displayed two columns wide: East Asian wide and fullwidth
characters, the emoji shown as pictures and the regional indicators.")

(defparameter *emoji-variation-bases*
  '((#x23 . #x23) (#x2A . #x2A) (#x30 . #x39) (#xA9 . #xA9) (#xAE . #xAE) (#x203C . #x203C) (#x2049 . #x2049)
    (#x2122 . #x2122) (#x2139 . #x2139) (#x2194 . #x2199) (#x21A9 . #x21AA) (#x2328 . #x2328)
    (#x23CF . #x23CF) (#x23ED . #x23EF) (#x23F1 . #x23F2) (#x23F8 . #x23FA) (#x24C2 . #x24C2)
    (#x25AA . #x25AB) (#x25B6 . #x25B6) (#x25C0 . #x25C0) (#x25FB . #x25FC) (#x2600 . #x2604)
    (#x260E . #x260E) (#x2611 . #x2611) (#x2618 . #x2618) (#x261D . #x261D) (#x2620 . #x2620)
    (#x2622 . #x2623) (#x2626 . #x2626) (#x262A . #x262A) (#x262E . #x262F) (#x2638 . #x263A)
    (#x2640 . #x2640) (#x2642 . #x2642) (#x265F . #x2660) (#x2663 . #x2663) (#x2665 . #x2666)
    (#x2668 . #x2668) (#x267B . #x267B) (#x267E . #x267E) (#x2692 . #x2692) (#x2694 . #x2697)
    (#x2699 . #x2699) (#x269B . #x269C) (#x26A0 . #x26A0) (#x26A7 . #x26A7) (#x26B0 . #x26B1)
    (#x26C8 . #x26C8) (#x26CF . #x26CF) (#x26D1 . #x26D1) (#x26D3 . #x26D3) (#x26E9 . #x26E9)
    (#x26F0 . #x26F1) (#x26F4 . #x26F4) (#x26F7 . #x26F9) (#x2702 . #x2702) (#x2708 . #x2709)
    (#x270C . #x270D) (#x270F . #x270F) (#x2712 . #x2712) (#x2714 . #x2714) (#x2716 . #x2716)
    (#x271D . #x271D) (#x2721 . #x2721) (#x2733 . #x2734) (#x2744 . #x2744) (#x2747 . #x2747)
    (#x2763 . #x2764) (#x27A1 . #x27A1) (#x2934 . #x2935) (#x2B05 . #x2B07) (#x1F170 . #x1F171)
    (#x1F17E . #x1F17F) (#x1F321 . #x1F321) (#x1F324 . #x1F32C) (#x1F336 . #x1F336) (#x1F37D . #x1F37D)
    (#x1F396 . #x1F397) (#x1F399 . #x1F39B) (#x1F39E . #x1F39F) (#x1F3CB . #x1F3CE) (#x1F3D4 . #x1F3DF)
    (#x1F3F3 . #x1F3F3) (#x1F3F5 . #x1F3F5) (#x1F3F7 . #x1F3F7) (#x1F43F . #x1F43F) (#x1F441 . #x1F441)
    (#x1F4FD . #x1F4FD) (#x1F549 . #x1F54A) (#x1F56F . #x1F570) (#x1F573 . #x1F579) (#x1F587 . #x1F587)
    (#x1F58A . #x1F58D) (#x1F590 . #x1F590) (#x1F5A5 . #x1F5A5) (#x1F5A8 . #x1F5A8) (#x1F5B1 . #x1F5B2)
    (#x1F5BC . #x1F5BC) (#x1F5C2 . #x1F5C4) (#x1F5D1 . #x1F5D3) (#x1F5DC . #x1F5DE) (#x1F5E1 . #x1F5E1)
    (#x1F5E3 . #x1F5E3) (#x1F5E8 . #x1F5E8) (#x1F5EF . #x1F5EF) (#x1F5F3 . #x1F5F3) (#x1F5FA . #x1F5FA)
    (#x1F6CB . #x1F6CB) (#x1F6CD . #x1F6CF) (#x1F6E0 . #x1F6E5) (#x1F6E9 . #x1F6E9) (#x1F6F0 . #x1F6F0)
    (#x1F6F3 . #x1F6F3))
  "Code point ranges of the characters one column wide that the emoji
presentation selector, U+FE0F, after them shows as a picture two columns wide.")

(defparameter *zero-width-ranges*
  '((#x0300 . #x036F) (#x0483 . #x0489) (#x0591 . #x05BD) (#x0610 . #x061A) (#x064B . #x065F)
    (#x0E31 . #x0E31) (#x0E34 . #x0E3A) (#x1AB0 . #x1AFF) (#x1DC0 . #x1DFF) (#x200B . #x200D)
    (#x2060 . #x2064) (#x20D0 . #x20FF) (#xFE00 . #xFE0F) (#xFE20 . #xFE2F) (#xFEFF . #xFEFF)
    (#x1160 . #x11FF) (#xD7B0 . #xD7FF) (#x1F3FB . #x1F3FF) (#xE0100 . #xE01EF))
  "Code point ranges displayed with no column of their own besides the marks
that combine with the character before them (ZERO-WIDTH-CODE-P): joiners,
variation selectors, the word joiner, the byte-order mark, skin tones and the
vowels and finals of a Hangul syllable written as jamo.")

(defparameter *direction-controls*
  '((#x061C . #x061C) (#x200E . #x200F) (#x202A . #x202E) (#x2066 . #x2069))
  "Code point ranges of the invisible characters that change the direction text
is drawn in. They are shown as a stand-in of one column of their own, so that
none can make a file name, buffer name or message read in another order.")

(defparameter *invisible-characters*
  '((#x00AD . #x00AD) (#x061C . #x061C) (#x115F . #x1160) (#x17B4 . #x17B5) (#x180E . #x180E)
    (#x200B . #x200F) (#x202A . #x202E) (#x2060 . #x2064) (#x2066 . #x206F) (#xFEFF . #xFEFF)
    (#xFFF9 . #xFFFB) (#x1D173 . #x1D17A) (#xE0001 . #xE0001) (#xE0020 . #xE007F))
  "Code point ranges of the characters that draw nothing of their own and
join no letter: the soft hyphen, the byte-order mark, the zero-width spaces
and joiners, the word joiner, the marks and controls that set the direction
text is drawn in, the fillers, the annotation and musical format controls and
the tags. While a buffer is set to show them, each is shown in its text as
its code, <FEFF> for the byte-order mark, so that none can hide, or make two
names that differ look alike.")

(defun in-ranges-p (code ranges)
  "True when CODE falls in one of RANGES, a list of (LOW . HIGH) conses."
  (loop for (low . high) in ranges thereis (<= low code high)))

(defun char-columns (char &optional invisible)
  "Number of columns CHAR occupies in the grid: 0 for combining marks, 2 for wide
characters, 2 for control characters shown as ^X, 4 for a raw byte shown as
\xNN, 1 otherwise. With INVISIBLE, a character that draws nothing
\(`*invisible-characters*') takes the columns of its code, as <FEFF> does.
Tabs are handled by the layout engine, not here."
  (code-columns (char-code char) invisible))

(defun invisible-code-p (code)
  "True when the character of CODE draws nothing of its own and joins no
letter (`*invisible-characters*'). Complexity: O(1)."
  (and (>= code #xAD) (in-ranges-p code *invisible-characters*)))

(defun invisible-code-display (code)
  "The text the character of CODE, one that draws nothing, is shown as while
its buffer shows such characters: its code in hexadecimal between angle
brackets, four digits at least, as <FEFF> or <E0020>."
  (format nil "<~4,'0X>" code))

(defun invisible-code-columns (code)
  "The columns INVISIBLE-CODE-DISPLAY's text for CODE takes: its hexadecimal
digits, four at least, and the two angle brackets."
  (+ 2 (max 4 (ceiling (integer-length code) 4))))

(defun mark-code-p (code)
  "True when the character of CODE is a mark that combines with the character
before it (an accent, a vowel sign), one shown on *MARK-BASE* where nothing
comes before it."
  (and (>= code #x300) (member (sb-unicode:general-category (code-char code)) '(:mn :me)) t))

(defun zero-width-code-p (code)
  "True when the character of CODE takes no column of its own: a mark that
combines with the character before it (an accent, a vowel sign), or one of
`*zero-width-ranges*'. Complexity: O(1)."
  (and (>= code #x300)
       (or (in-ranges-p code *zero-width-ranges*) (mark-code-p code))))

(defun ranges-columns (code)
  "Number of columns the character of CODE, 160 or above, occupies in the grid
as the ranges of characters set out above say (CODE-COLUMNS)."
  (cond ((tuatara.text:raw-byte-code-p code) 4)
        ((in-ranges-p code *direction-controls*) 1)
        ((zero-width-code-p code) 0)
        ((in-ranges-p code *wide-ranges*) 2)
        (t 1)))

(defparameter *plane-columns* (make-array #x40000 :element-type '(unsigned-byte 8) :initial-element 0)
  "The columns of each character of the first four planes of codes (below
#x40000, every emoji and ideograph among them), plus one, as RANGES-COLUMNS
gives them, filled in as they are first asked for; 0 for a code not yet asked
for. Loading this file again starts it afresh.")

(defun code-columns (code &optional invisible)
  "Number of columns the character of CODE occupies in the grid (CHAR-COLUMNS);
with INVISIBLE, a character that draws nothing takes those of its code
\(INVISIBLE-CODE-DISPLAY)."
  (cond ((< code 32) 2)                  ; ^X
        ((= code 127) 2)                 ; ^?
        ((< code 160) (if (< code 128) 1 4)) ; C1 controls shown as \xNN
        ((and invisible (invisible-code-p code)) (invisible-code-columns code))
        ((< code #x40000)
         (let ((table *plane-columns*))
           (declare (type (simple-array (unsigned-byte 8) (#x40000)) table))
           (let ((known (aref table code)))
             (if (plusp known)
                 (1- known)
                 (let ((columns (ranges-columns code)))
                   (setf (aref table code) (1+ columns))
                   columns)))))
        (t (ranges-columns code))))

(defparameter *mark-base* (code-char #x25CC)
  "The character a mark that combines with the character before it is shown
on where there is none before it, at a line's start: a dotted circle.")

(defun cluster-columns (code before own &optional invisible)
  "Two values: the columns the character of CODE adds to the line after the
character of code BEFORE (0 at the line's start), OWN being its own columns,
and the code to give as BEFORE for the character after it. A grapheme
cluster takes the columns of its first character, whatever follows it in the
cluster: an emoji joined to the one before by U+200D adds none, nor does the
second regional indicator of a flag, and the emoji presentation selector
U+FE0F after a character one column wide shown as a picture
\(`*emoji-variation-bases*') adds the one that makes it two. A mark at the
line's start, with nothing to combine with, takes one column, shown on
*MARK-BASE*. With INVISIBLE, a joiner is shown as its code and joins nothing.
Complexity: O(1)."
  (declare (type fixnum code before own))
  (cond ((< code #x300) (values own code))
        ((and (= before #x200D) (not invisible) (tuatara.text:cluster-joins-p before code 0))
         (values 0 -1))
        ((tuatara.text:regional-indicator-code-p code)
         (if (tuatara.text:regional-indicator-code-p before) (values 0 -1) (values own code)))
        ((and (= code #xFE0F) (in-ranges-p before *emoji-variation-bases*) (= (code-columns before) 1))
         (values 1 code))
        ((and (zerop before) (zerop own) (mark-code-p code)) (values 1 code))
        (t (values own code))))

(defun string-columns (string &key (start 0) (end (length string)) (width #'char-columns))
  "The columns the characters of STRING from START below END take, WIDTH
giving each character's own (CHAR-COLUMNS unless given) and a grapheme
cluster the columns of its first character (CLUSTER-COLUMNS)."
  (let ((before 0) (sum 0))
    (declare (type fixnum before sum))
    (loop for i from start below end
          do (let ((char (char string i)))
               (multiple-value-bind (columns next) (cluster-columns (char-code char) before (funcall width char))
                 (incf sum columns)
                 (setf before next))))
    sum))

(defun string-column-starts (string &key (width #'char-columns))
  "A vector of the screen column each character of STRING starts at, and one
more, the columns of all of them, as STRING-COLUMNS counts: a character
joined to the one before it in a grapheme cluster takes no columns of its own."
  (let ((starts (make-array (1+ (length string)) :element-type 'fixnum)) (column 0) (before 0))
    (declare (type fixnum column before))
    (dotimes (i (length string))
      (setf (aref starts i) column)
      (let ((char (char string i)))
        (multiple-value-bind (columns next) (cluster-columns (char-code char) before (funcall width char))
          (incf column columns)
          (setf before next))))
    (setf (aref starts (length string)) column)
    starts))

(defun chars-in-columns (string columns &key (width #'char-columns))
  "How many characters of STRING, from its start, fit in COLUMNS columns,
WIDTH giving each character's own (CHAR-COLUMNS unless given) and a grapheme
cluster the columns of its first character (STRING-COLUMNS): where text is
cut to fit a width, never inside a cluster. The columns those characters
take are the second value."
  (let ((used 0) (before 0))
    (declare (type fixnum used before))
    (loop for i from 0 below (length string)
          do (let ((char (char string i)))
               (multiple-value-bind (more next) (cluster-columns (char-code char) before (funcall width char))
                 (incf used more)
                 (setf before next)))
             (when (> used columns)
               (let ((kept (tuatara.text:string-cluster-start string (1+ i))))
                 (return-from chars-in-columns (values kept (string-columns string :end kept :width width))))))
    (values (length string) used)))

(defun pad-columns (string columns &key (width #'char-columns) left)
  "STRING with spaces after it, or with LEFT before it, to take COLUMNS
columns, WIDTH giving each character's own (CHAR-COLUMNS unless given;
STRING-COLUMNS); STRING itself when it takes that many or more."
  (let ((missing (- columns (string-columns string :width width))))
    (cond ((not (plusp missing)) string)
          (left (concatenate 'string (make-string missing :initial-element #\Space) string))
          (t (concatenate 'string string (make-string missing :initial-element #\Space))))))

(defun fit-columns (string spans columns &key middle words (width #'char-columns) (ellipsis "…"))
  "STRING and its SPANS, lists (START END FACE) over its characters, cut to
take at most COLUMNS columns, WIDTH giving each character's own (CHAR-COLUMNS
unless given; STRING-COLUMNS). ELLIPSIS (… unless given) ends a cut string: with WORDS
just after the last word that ends before it, when one does, rather than
inside a word. With MIDDLE the ellipsis stands for what is cut from the
middle, a third of it kept before and the rest after (the start of a path,
and its last directories and its name). Returns the string and the spans,
STRING itself when it fits."
  (flet ((columns-of (&key (start 0) (end (length string))) (string-columns string :start start :end end :width width))
         (fitting (columns) (chars-in-columns string columns :width width)))
    (cond ((<= (columns-of) columns) (values string spans))
          ((not (plusp columns)) (values "" '()))
          (middle
           (let* ((head (fitting (floor (1- columns) 3)))
                  (room (- columns 1 (columns-of :end head)))
                  (tail (loop for start from (length string) downto head
                              while (<= (columns-of :start start) room)
                              finally (return (1+ start))))
                  (tail (min (length string) (max head tail)))
                  (shift (- (1+ head) tail)))
             (values (concatenate 'string (subseq string 0 head) "…" (subseq string tail))
                     (loop for (start end face) in spans
                           append (append (when (< start head) (list (list start (min end head) face)))
                                          (when (> end tail)
                                            (list (list (+ shift (max start tail)) (+ shift end) face))))))))
          (t
           (let* ((keep (fitting (max 0 (- columns (string-columns ellipsis :width width)))))
                  (keep (or (and words
                                 (loop for i from (min keep (1- (length string))) downto 2
                                       when (and (char/= (char string (1- i)) #\Space) (char= (char string i) #\Space))
                                         return i))
                            keep))
                  (cut (concatenate 'string (subseq string 0 keep) ellipsis)))
             (values cut
                     (loop for (start end face) in spans
                           when (< start (length cut))
                             collect (list start (min end (length cut)) face))))))))

(defun shorten-string (string most &key one-line (ellipsis "…"))
  "STRING shortened to take no more than MOST columns: as it is when it fits,
else cut after its last word that fits and ELLIPSIS (… unless given) added,
or cut where it must when no word ends early enough (FIT-COLUMNS). With
ONE-LINE, put on one line first (TUATARA.TEXT:ONE-LINE)."
  (values (fit-columns (if one-line (tuatara.text:one-line string) string) '() most :words t :ellipsis ellipsis)))

(defun fill-words (words column &key (start 0) (indent 0) (width #'char-columns) cut)
  "WORDS, a list of strings, filled into lines that reach no further than
COLUMN columns, a space between each two words of a line, WIDTH giving each
character's columns (CHAR-COLUMNS unless given; STRING-COLUMNS): the first
line follows START columns already taken, each later one is set in by INDENT
\(not part of the lines returned). A word too long for a line of its own has
one to itself, or with CUT is cut into pieces that fit. Returns the lines, NIL
for no words."
  (let ((lines '()) (line nil) (used start))
    (dolist (word words)
      (let ((wide (string-columns word :width width)))
        (when cut
          (loop for room = (- column (if line indent (if lines indent start)))
                while (and (plusp room) (> wide room))
                do (let ((piece (max 1 (chars-in-columns word room :width width))))
                     (when line (push line lines) (setf line nil))
                     (push (subseq word 0 piece) lines)
                     (setf word (subseq word piece)
                           wide (string-columns word :width width)))))
        (cond ((null line) (setf line word used (+ (if lines indent start) wide)))
              ((<= (+ used 1 wide) column)
               (setf line (concatenate 'string line " " word))
               (incf used (1+ wide)))
              (t (push line lines)
                 (setf line word used (+ indent wide))))))
    (when line (push line lines))
    (nreverse lines)))

(sb-ext:define-load-time-global **octet-escapes**
    (let ((texts (make-array 256)))
      (dotimes (octet 256 texts)
        (setf (svref texts octet) (concatenate 'string "\\x" (string-upcase (svref tuatara.text:*hex-pairs* octet))))))
  "The text \\xNN of each octet, by its value, made once.")

(sb-ext:define-load-time-global **control-displays**
    (let ((texts (make-array 160 :initial-element nil)))
      (dotimes (code 160 texts)
        (setf (svref texts code)
              (cond ((< code 32) (coerce (list #\^ (code-char (+ code 64))) 'string))
                    ((= code 127) "^?")
                    ((>= code 128) (svref **octet-escapes** code))))))
  "The text each control character is shown as, by its code below 160, made once.")

(defun control-char-display (char)
  "The text a control character is shown as: ^A for code 1, ^? for 127, \\x85 for C1 controls.
The text is shared: it must not be changed."
  (svref **control-displays** (char-code char)))

(defun raw-byte-display (code)
  "The text the character of CODE, a raw byte's (an octet that is not UTF-8,
U+DC80..U+DCFF), is shown as: \\x80 for the octet #x80. The text is shared:
it must not be changed."
  (svref **octet-escapes** (logand code #xFF)))

(sb-ext:define-load-time-global **spaces**
    (let ((texts (make-array 65)))
      (dotimes (n 65 texts)
        (setf (svref texts n) (make-string n :initial-element #\Space))))
  "Strings of spaces of each length up to 64, the most a tab can take, made once.")

(declaim (inline next-tab-stop))
(defun next-tab-stop (column width)
  "The column of the first tab stop after COLUMN, stops being every WIDTH columns."
  (declare (type fixnum column width))
  (* width (1+ (floor column width))))

(declaim (inline advance-column))
(defun advance-column (code column before tab-width invisible)
  "Two values: the columns the character of CODE takes on the screen where it
stands at COLUMN after the character of code BEFORE (0 at a line's start),
and the code to give as BEFORE for the character after it. A tab reaches the
next of every TAB-WIDTH columns, a control character takes two (^X), any
other what CODE-COLUMNS says (with INVISIBLE, a character that draws nothing
the columns of its code shown), and a grapheme cluster the columns of its
first character (CLUSTER-COLUMNS). Every count of columns along a line goes
by this."
  (declare (type fixnum code column before tab-width))
  (cond ((= code 9) (values (- (next-tab-stop column tab-width) column) code))
        ((< code 128) (values (if (tuatara.text:control-code-p code) 2 1) code))
        (t (cluster-columns code before (code-columns code invisible) invisible))))

(defun char-display (char column tab-width &optional invisible)
  "How CHAR, at COLUMN, is shown, as three values: the text shown (CHAR
itself when it is shown as itself, else a string), the columns it takes, and
:RAW-BYTE or :INVISIBLE-CHARACTER when that text stands for a character that
cannot be shown as itself, else NIL; text it gives is shared and must not be
changed. A tab is shown as spaces
up to the next of every TAB-WIDTH columns, a control character as ^X or \\xNN
\(CONTROL-CHAR-DISPLAY), a raw byte as \\xNN, and with INVISIBLE a character
that draws nothing as its code (INVISIBLE-CODE-DISPLAY)."
  (let ((code (char-code char)))
    (cond ((char= char #\Tab)
           (let ((n (- (next-tab-stop column tab-width) column)))
             (values (if (< n (length **spaces**)) (svref **spaces** n) (make-string n :initial-element #\Space))
                     n nil)))
          ((tuatara.text:control-code-p code)
           (let ((text (control-char-display char))) (values text (length text) nil)))
          ((tuatara.text:raw-byte-code-p code)
           (values (raw-byte-display code) 4 :raw-byte))
          ((and invisible (invisible-code-p code))
           (let ((text (invisible-code-display code))) (values text (length text) :invisible-character)))
          (t (values char (char-columns char) nil)))))

(defun displayable (string &key (controls t))
  "STRING as it can be drawn or sent on, keeping its length: a lone surrogate
\(a raw byte's character, which has no UTF-8) as U+FFFD; with CONTROLS, a tab
as a space, any other control character (codes 0 to 31, 127 to 159) and any
character that changes the direction text is drawn in (`*direction-controls*')
as ?, and the character of a picture's cell (U+10EEEE) as U+FFFD, so that none
moves a terminal's cursor, draws as nothing, reorders the text around it or
draws a piece of a picture. STRING itself when nothing needs it."
  (flet ((unsafe (c)
           (let ((code (char-code c)))
             (or (<= #xD800 code #xDFFF)
                 (and controls (or (tuatara.text:control-code-p code)
                                   (= code tuatara.frontend:+kitty-placeholder+)
                                   (in-ranges-p code *direction-controls*)))))))
    (if (find-if #'unsafe string)
        (map 'string (lambda (c)
                       (let ((code (char-code c)))
                         (cond ((not (unsafe c)) c)
                               ((<= #xD800 code #xDFFF) (code-char #xFFFD))
                               ((= code tuatara.frontend:+kitty-placeholder+) (code-char #xFFFD))
                               ((char= c #\Tab) #\Space)
                               (t #\?))))
             string)
        string)))
