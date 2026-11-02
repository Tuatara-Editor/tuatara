;;;; src/display/layout.lisp --- The layout engine: from a text snapshot to glyph rows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Layout is a pure function of a rope, a start byte and a size: it decodes only
;;;; the text that lands in the window, so its cost is proportional to the visible
;;;; area, never to the file.  Rows are what a painter
;;;; draws: a string to print, and the byte offset behind every column so that
;;;; point and the mouse can be mapped both ways.

(in-package #:tuatara.display)

(defparameter *fold-ellipsis* "..."
  "What a window shows in place of a fold.")

(defstruct (row (:constructor %make-row (start columns &key (room 0)
                                         &aux (string (make-array room :element-type 'character :adjustable t :fill-pointer 0))
                                              (faces (make-array room :adjustable t :fill-pointer 0))
                                              (char-bytes (make-array room :element-type 'fixnum :adjustable t :fill-pointer 0))
                                              (bytes (make-array room :element-type 'fixnum :adjustable t :fill-pointer 0))))
                (:copier nil))
  "One screen row of a window. STRING is what to print; BYTES holds, for each of
the COLUMNS columns, the byte offset of the character shown there (a wide
character fills two columns with the same offset); START and END delimit the
text the row covers; CONTINUATION-P is true for the second and later rows of a
wrapped line; LAST-P is true for the row holding the end of the text. EXTEND
is the face, or faces, carried on past the end of the row's line. SCALE is how
many rows' height the row is drawn in, its letters as many times as wide
\(a headline drawn large): 1, or a rational such as 5/4 or 2. SET-IN lists
the characters of STRING shown where no text of the buffer is (an inlay
hint, a replacement shown before it is made), each run as (FROM . TO),
indices into STRING; highlights of the text around them leave them as they
are. MIXED is T for a row whose characters stand for offsets in no order (the
hex view), and for one whose characters are in the order they are read,
right-to-left text among them, rather than stored, a list (:VISUAL
END-COLUMN PADDING): ROW-END-COLUMN and ROW-PADDING."
  (string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0) :type string)
  (faces (make-array 0 :adjustable t :fill-pointer 0))
  (char-bytes (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (bytes (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (shown-end 0 :type fixnum)
  (columns 0 :type fixnum)
  (continuation-p nil :type boolean)
  (truncated-p nil :type boolean)
  (last-p nil :type boolean)
  (mixed nil)
  (extend nil)
  (scale 1 :type rational)
  (set-in '() :type list))

(setf (documentation 'row-p 'function) "True when OBJECT is a ROW."
      (documentation 'row-string 'function) "The text to print for ROW."
      (documentation 'row-bytes 'function) "Byte offset behind each column of ROW, -1 for none."
      (documentation 'row-faces 'function) "The face of each character of ROW's string: NIL, a face name, or a list of face names with the highest priority first."
      (documentation 'row-char-bytes 'function) "The byte offset of the source character behind each character of ROW's string; never decreasing."
      (documentation 'row-start 'function) "Byte offset where ROW's text starts."
      (documentation 'row-end 'function) "Byte offset just after ROW's text (after its newline, if any)."
      (documentation 'row-shown-end 'function) "Byte offset after the last character ROW actually shows: its newline, a wrap point, or where truncation began."
      (documentation 'row-truncated-p 'function) "True when ROW's line was cut off at the right edge."
      (documentation 'row-columns 'function) "Number of columns ROW uses."
      (documentation 'row-continuation-p 'function) "True when ROW continues a wrapped line."
      (documentation 'row-last-p 'function) "True when ROW holds the end of the text."
      (documentation 'row-mixed 'function) "True when ROW's characters stand for offsets not in order: T in the hex view, (:VISUAL END-COLUMN PADDING) where they are in the order right-to-left text is read."
      (documentation 'row-extend 'function) "The face, or faces with the highest priority first, painted from the end of ROW's line to the window's edge: those of highlights covering its newline that extend; NIL for none."
      (documentation 'row-set-in 'function) "The runs of ROW's characters shown where no text of the buffer is, each (FROM . TO) of indices into its string; highlights leave them alone."
      (documentation 'row-scale 'function) "How many rows' height ROW is drawn in, its letters as many times as wide: 1, or more for a line drawn large (5/4, 3/2, 2).")

(defun row-end-column (row)
  "The column where the end of ROW's line is shown when not after its text:
left of the text of a line written from right to left set against the right
edge; NIL otherwise."
  (let ((mixed (row-mixed row))) (and (consp mixed) (second mixed))))

(defun row-padding (row)
  "The blanks before ROW's text that set a line written from right to left
against the right edge, (FROM . TO) of indices into its string; NIL for
none."
  (let ((mixed (row-mixed row))) (and (consp mixed) (third mixed))))

(defun make-row (start &optional (room 0) spares)
  "Return an empty row starting at byte START, with room for ROOM characters
and columns before it needs more: one taken from SPARES, a vector with a fill
pointer of rows no longer shown, when it has any, emptied (its room kept)."
  (if (and spares (plusp (fill-pointer spares)))
      (let ((row (vector-pop spares)))
        (setf (fill-pointer (row-string row)) 0
              (fill-pointer (row-faces row)) 0
              (fill-pointer (row-char-bytes row)) 0
              (fill-pointer (row-bytes row)) 0
              (row-start row) start
              (row-end row) 0
              (row-shown-end row) 0
              (row-columns row) 0
              (row-continuation-p row) nil
              (row-truncated-p row) nil
              (row-last-p row) nil
              (row-mixed row) nil
              (row-extend row) nil
              (row-scale row) 1
              (row-set-in row) '())
        row)
      (%make-row start 0 :room room)))

(defun row-add (row text byte columns &optional face)
  "Append TEXT, a string or a single character, to ROW as the display of the
character at BYTE, taking COLUMNS columns, in FACE (NIL for none: faces are
mostly applied afterwards by APPLY-HIGHLIGHTS, which keeps this one beneath
them)."
  (declare (type fixnum byte columns))
  (flet ((add (c)
           (vector-push-extend c (row-string row))
           (vector-push-extend face (row-faces row))
           (vector-push-extend byte (row-char-bytes row))))
    (if (characterp text)
        (add text)
        (loop for c across text do (add c))))
  (dotimes (i columns) (vector-push-extend byte (row-bytes row)))
  (incf (row-columns row) columns)
  row)

(defun row-spans (row)
  "The faces of ROW as a list of (START END FACE) over character indices of its
string, consecutive characters with the same face merged; unfaced runs are
omitted. FACE is a face name or a list of names, highest priority first."
  (let ((faces (row-faces row)) (spans '()) (start 0) (current nil))
    (loop for i from 0 below (length faces)
          for face = (aref faces i)
          do (unless (equal face current)
               (when current (push (list start i current) spans))
               (setf start i current face)))
    (when current (push (list start (length faces) current) spans))
    (nreverse spans)))

(defun merge-face (new old)
  "The face of a character that had OLD when NEW is painted over it: NEW alone
when there was none, otherwise a list with NEW first (it has higher priority)."
  (cond ((null old) new)
        ((eq old new) old)
        ((listp old) (if (member new old) old (cons new old)))
        (t (list new old))))

(defun first-index-at-or-after (vector value)
  "The first index of VECTOR (never decreasing fixnums) whose element is at least VALUE; its length when none."
  (tuatara.runtime:bisect (mid 0 (length vector)) (< (aref vector mid) value)))

(defun apply-highlights (rows &rest layers)
  "Paint each of LAYERS, lists of (START END FACE) byte ranges, onto the
characters of ROWS, the ranges of a list and the lists themselves given lowest
priority first, so that a later range wins over an earlier one where they
overlap (the region over syntax colours, for example). A face that extends,
covering the newline that ends a row's line, is carried on past it
\(ROW-EXTEND); so is one reaching past the end of the text, on the last row,
and one covering a row of a wrapped line to where the next row goes on.
Characters shown where no text of the buffer is (ROW-SET-IN) keep their own
face. A run of characters that had the same face takes the same merged face
\(MERGE-FACE), not a new one for each. Returns ROWS. Complexity: O(ranges ×
log rows + characters painted)."
  (declare (dynamic-extent layers))
  (let ((n (length rows)))
    (dolist (ranges layers rows)
      (dolist (range ranges)
        (destructuring-bind (start end face) range
          (when (and face (< start end) (plusp n))
            ;; the first row that ends after START (rows are in byte order)
            (let ((lo (tuatara.runtime:bisect (mid 0 n) (and (<= (row-end (aref rows mid)) start)
                                                            (not (row-last-p (aref rows mid))))))
                  (last-old '#:none) (last-merged nil))
              (flet ((paint (old)
                       (if (eq old last-old)
                           last-merged
                           (setf last-old old last-merged (merge-face face old)))))
                (declare (inline paint))
                (loop for r from lo below n
                      for row = (aref rows r)
                      while (< (row-start row) end)
                      do (let ((newline (1- (row-end row))))
                           (when (and (cond ((row-last-p row)
                                             (and (<= start (row-end row)) (< (row-end row) end)))
                                            ((< (row-shown-end row) (row-end row))
                                             (and (<= start newline) (< newline end)))
                                            ;; a row a wrapped line goes on from, covered to its end
                                            (t (and (< start (row-end row)) (<= (row-end row) end))))
                                      (not (eq (row-mixed row) t))
                                      (symbolp face) (face-attribute face :extend))
                             (setf (row-extend row) (paint (row-extend row)))
                             ;; and before the text of a line set against the right edge
                             (let ((padding (row-padding row)))
                               (when padding
                                 (loop with faces = (row-faces row)
                                       for i from (car padding) below (cdr padding)
                                       do (setf (aref faces i) (paint (aref faces i))))))))
                         (let* ((bytes (row-char-bytes row))
                                (faces (row-faces row))
                                (count (length bytes)))
                           (if (row-mixed row)
                               ;; offsets out of order (the hex view): every character is looked at
                               (dotimes (i count)
                                 (when (<= start (aref bytes i) (1- end))
                                   (setf (aref faces i) (paint (aref faces i)))))
                               (let ((set-in (row-set-in row)))
                                 (loop for i from (first-index-at-or-after bytes start) below count
                                       while (< (aref bytes i) end)
                                       unless (and set-in (loop for (from . below) in set-in
                                                                  thereis (and (<= from i) (< i below))))
                                         do (setf (aref faces i) (paint (aref faces i))))))))))))))))

(defun row-column-of (row byte)
  "The column of ROW where byte offset BYTE is shown; where the end of its
line is shown (after the last glyph, or ROW-END-COLUMN) when BYTE is the
newline or text end that finishes the row; NIL when BYTE is not shown in ROW
\(outside it, wrapped to the next row, truncated, or scrolled off
horizontally). A character of no width of its own (a combining mark, a
joiner) is shown at the column of the glyph it joins, the one before it, or
where the next glyph stands when there is none before it in the row."
  (declare (type fixnum byte))
  (cond ((< byte (row-start row)) nil)
        ((< byte (row-shown-end row))
         (or (position byte (row-bytes row))
             (and (consp (row-mixed row))
                  ;; a character with no column of its own, in a row in the order it is read: its glyph's
                  (let* ((chars (row-char-bytes row))
                         (i (position byte chars)))
                    (and i (loop for k downfrom i to 0
                                 for column = (and (>= (aref chars k) 0) (position (aref chars k) (row-bytes row)))
                                 when column return column))))
             (and (not (row-mixed row))
                  (let* ((chars (row-char-bytes row))
                         (i (first-index-at-or-after chars byte)))
                    ;; a character the row holds without a column
                    (and (< i (length chars)) (= (aref chars i) byte)
                         (let* ((bytes (row-bytes row))
                                (after (first-index-at-or-after bytes byte)))
                           (if (and (plusp after) (>= (aref bytes (1- after)) 0))
                               (first-index-at-or-after bytes (aref bytes (1- after)))
                               after)))))))
        ((and (= byte (row-shown-end row))
              (not (row-truncated-p row))
              (or (row-last-p row) (< (row-shown-end row) (row-end row))))
         (or (row-end-column row) (row-columns row)))
        (t nil)))

(defstruct (reader (:constructor %make-reader (rope position end chunk)) (:copier nil))
  "A sequential UTF-8 reader over a rope: decodes characters from POSITION below
END, reading the text into CHUNK +READER-CHUNK+ octets at a time."
  (rope nil :type tuatara.text:rope :read-only t)
  (position 0 :type fixnum)
  (end 0 :type fixnum :read-only t)
  (chunk (make-array 0 :element-type '(unsigned-byte 8)) :type tuatara.text:octets)
  (chunk-start 0 :type fixnum)
  (chunk-end 0 :type fixnum))

(setf (documentation 'reader-p 'function) "True when OBJECT is a READER."
      (documentation 'reader-position 'function) "Byte offset the READER will decode next.")

(defconstant +reader-chunk+ 4096 "Octets a READER fetches from the rope at a time.")

(defun make-reader (rope position &optional (end (tuatara.text:rope-bytes rope)) chunk)
  "Return a reader over ROPE from byte POSITION below END, reading into CHUNK,
octets at least +READER-CHUNK+ long (new ones when not given)."
  (%make-reader rope position end (or chunk (tuatara.text:make-octets +reader-chunk+))))

(defun reader-fill (reader)
  "Ensure the reader's chunk covers its position; return NIL at END."
  (let ((pos (reader-position reader)))
    (cond ((>= pos (reader-end reader)) nil)
          ((and (<= (reader-chunk-start reader) pos) (< pos (reader-chunk-end reader))) t)
          (t (let* ((start pos)
                    (end (min (reader-end reader) (+ start +reader-chunk+))))
               (tuatara.text:rope-copy-into (reader-rope reader) start end (reader-chunk reader))
               (setf (reader-chunk-start reader) start
                     (reader-chunk-end reader) end)
               t)))))

(defun reader-octet (reader offset)
  "Octet at absolute byte OFFSET, fetching a chunk if needed; NIL past END."
  (when (< offset (reader-end reader))
    (unless (and (<= (reader-chunk-start reader) offset) (< offset (reader-chunk-end reader)))
      (let ((save (reader-position reader)))
        (setf (reader-position reader) offset)
        (reader-fill reader)
        (setf (reader-position reader) save)))
    (aref (reader-chunk reader) (- offset (reader-chunk-start reader)))))

(defun reader-next (reader)
  "Decode and return the next character and advance, or NIL at END. Octets
that are not UTF-8 come as raw bytes' characters, one each."
  (when (reader-fill reader)
    (let ((pos (reader-position reader)))
      (if (and (< pos (reader-chunk-end reader)) (< (aref (reader-chunk reader) (- pos (reader-chunk-start reader))) #x80))
          (progn (incf (reader-position reader)) (code-char (aref (reader-chunk reader) (- pos (reader-chunk-start reader)))))
          ;; at most four octets: read from the chunk, or from the rope where they may run past it
          (let ((end (min (reader-end reader) (+ pos 4)))
                (start (reader-chunk-start reader)))
            (multiple-value-bind (code length)
                (if (<= end (reader-chunk-end reader))
                    (tuatara.text:octets-decode (reader-chunk reader) (- pos start) (- end start))
                    (let ((window (make-array 4 :element-type '(unsigned-byte 8))))
                      (declare (dynamic-extent window))
                      (tuatara.text:rope-copy-into (reader-rope reader) pos end window)
                      (tuatara.text:octets-decode window 0 (- end pos))))
              (incf (reader-position reader) length)
              (code-char code)))))))

(defun hidden-end (entry)
  "Where ENTRY of a layout's HIDDEN ends: (START . END), or (START END TEXT FACE)."
  (if (consp (cdr entry)) (second entry) (cdr entry)))

(defun hidden-passed-p (entry at)
  "True when the layout at byte AT is past ENTRY of its HIDDEN: at or after
its end, or, for text shown where nothing is hidden (its start its end, an
inlay hint), after it."
  (if (= (car entry) (hidden-end entry))
      (> at (car entry))
      (>= at (hidden-end entry))))

(defparameter *cut-glyph* "…"
  "What a row shows, in one column, for a character too wide for the whole
row when lines wrap (a wide character in a window one column wide): the
character itself cannot be shown there, and the next row goes on after it.")

;;; Columns on the screen along a line

(defparameter *screen-column-reach* (* 1024 1024)
  "Octets of a line read to tell a column on the screen; past them, each
character is counted one column (each octet while the text's characters are
still being counted), so that a line of a gigabyte costs no wait.")

(defconstant +scan-block+ 65536 "Octets SCAN-COLUMNS reads from the text at a time.")

(defun scan-columns (rope from to tab-width &key (column 0) (limit most-positive-fixnum) crlf invisible)
  "Walk ROPE's characters from FROM, a character's start, counting the columns
they take on the screen from COLUMN on, as ADVANCE-COLUMN does with TAB-WIDTH
and INVISIBLE. Stops at
the first character's start at or after TO, at the first where the count has
reached LIMIT (past the marks and joined emoji of the cluster there), or
before the line's end (a newline or a carriage return that ends the line,
or with carriage return and line feed (CRLF) a carriage return just before a
newline). Returns three values: where it stopped, the column there, and true
when that is the line's end. Complexity: O(octets walked), read a block at a
time, an ASCII octet looked at once."
  (declare (type fixnum from to tab-width column limit) (optimize speed))
  (let* ((total (tuatara.text:rope-bytes rope))
         (to (min to total))
         (pos from)
         (before 0)
         ;; read about as much as the columns left need, more each time more is read
         (size (min +scan-block+ (max 0 (- to from))
                    (if (< (- limit column) +scan-block+) (+ 64 (* 4 (- limit column))) +scan-block+))))
    (declare (type fixnum total to pos before size))
    (tuatara.text:with-octet-block (block (+ +scan-block+ 8))
      (loop
        (when (or (>= pos to) (>= column limit)) (return (values pos column nil)))
        (let* ((stop (min to (+ pos size)))
               ;; four octets more, to decode a character begun before STOP and see past a return
               (have (min total (+ stop 5)))
               (avail (- have pos))
               (cut (- stop pos))
               (i 0))
          (declare (type fixnum stop have avail cut i))
          (tuatara.text:rope-copy-into rope pos have block)
          (loop while (< i cut)
                do (let ((b (aref block i)))
                     (when (and (or (= b 10) (= b 13)) (tuatara.text:line-text-ends-p block i avail crlf))
                       (return-from scan-columns (values (+ pos i) column t)))
                     (multiple-value-bind (code length)
                         (if (< b 128) (values b 1) (tuatara.text:octets-decode block i avail))
                       (declare (type fixnum code length))
                       (multiple-value-bind (more next) (advance-column code column before tab-width invisible)
                         (declare (type fixnum more next))
                         ;; at LIMIT; past it, on past what goes with the cluster before
                         (when (and (>= column limit) (or (< code 128) (and (plusp more) (/= code #xFE0F))))
                           (return))
                         (incf column more)
                         (setf before next))
                       (incf i length))))
          (incf pos i)
          (setf size (min +scan-block+ (* 2 size))))))))

(defun display-column (rope line-start byte &key (tab-width 8) invisible)
  "Number of columns between LINE-START and BYTE on the same line, counting
wide characters two and a tab to the next multiple of TAB-WIDTH, with
INVISIBLE a character that draws nothing as its code shown, every character
read. Complexity: O(BYTE - LINE-START)."
  (nth-value 1 (scan-columns rope line-start byte tab-width :invisible invisible)))

(defun far-columns (rope from to)
  "The columns from FROM to TO, both character starts on one line, past a
line's *SCREEN-COLUMN-REACH*: one a character, or one an octet while ROPE's
characters are still being counted. Complexity: O(log n)."
  (if (tuatara.text:rope-indexed-p rope)
      (- (tuatara.text:byte->char rope to) (tuatara.text:byte->char rope from))
      (- to from)))

(defun far-position (rope from count end crlf)
  "Where COUNT columns after FROM end past a line's *SCREEN-COLUMN-REACH*
\(FAR-COLUMNS): a character's start, no further than END nor the line's end
\(the octet that ends it; with carriage return and line feed (CRLF), the
carriage return before its newline). Complexity: O(log n) once ROPE's
characters are counted, O(COUNT) before."
  (let* ((indexed (tuatara.text:rope-indexed-p rope))
         (line-end (if indexed
                       (min end (tuatara.text:rope-line-end rope from))
                       (or (tuatara.text:rope-find-line-end rope :start from :end (min end (+ from count)))
                           end)))
         (line-end (if (and crlf (> line-end from) (tuatara.text:rope-crlf-p rope (1- line-end)))
                       (1- line-end)
                       line-end))
         (target (if indexed
                     (let ((char (+ (tuatara.text:byte->char rope from) count)))
                       (if (>= char (tuatara.text:byte->char rope line-end))
                           line-end
                           (tuatara.text:char->byte rope char)))
                     (let ((at (min line-end (+ from count))))
                       (if (or (= at line-end) (tuatara.text:char-boundary-p rope at))
                           at
                           (tuatara.text:rope-previous-boundary rope at))))))
    (max from target)))

(defun skip-columns (rope line-start from column limit end &key (tab-width 8) crlf invisible)
  "Where a row of the line beginning at LINE-START that skips LIMIT columns
starts showing, walking from FROM, which stands at COLUMN: the first
character's start whose column is LIMIT or more, no further than END nor the
line's end. Columns are counted exactly over the line's first
*SCREEN-COLUMN-REACH* octets (with INVISIBLE, a character that draws nothing
as its code shown) and a character each past them (FAR-COLUMNS). Returns that
position and its column. Complexity: O(*SCREEN-COLUMN-REACH* +
log n)."
  (let ((edge (+ line-start *screen-column-reach*)))
    (multiple-value-bind (position at line-end)
        (scan-columns rope from (min end edge) tab-width :column column :limit limit :crlf crlf :invisible invisible)
      (if (and (< at limit) (not line-end) (>= position edge) (< position end))
          (let ((target (far-position rope position (- limit at) end crlf)))
            (values target (+ at (far-columns rope position target))))
          (values position at)))))

(defun line-screen-column (rope line-start byte &key (tab-width 8) crlf invisible)
  "The column on the screen of BYTE on the line beginning at LINE-START:
counted exactly over the line's first *SCREEN-COLUMN-REACH* octets (with
INVISIBLE, a character that draws nothing as its code shown), a
character each past them (FAR-COLUMNS), as the rows of a window scrolled
sideways count it. Complexity: O(*SCREEN-COLUMN-REACH* + log n)."
  (multiple-value-bind (position column line-end)
      (scan-columns rope line-start (min byte (+ line-start *screen-column-reach*)) tab-width :crlf crlf :invisible invisible)
    (if (and (< position byte) (not line-end))
        (+ column (far-columns rope position byte))
        column)))

(defparameter *wrap-stretch* 65536
  "Octets of a line whose rows, where lines wrap, are worked out in one go: a
longer line has a row begin every so many octets (at the start of the
character there, WRAP-ANCHOR), so that the row holding a place in it is found
reading no more of the line than that.")

(defun wrap-anchor (rope line-start position)
  "Where the rows holding POSITION are worked out from, where lines wrap: the
start of its line, beginning at LINE-START, or of a long line, the last place
at or before POSITION where a row begins every *WRAP-STRETCH* octets (the
start of the character there). Complexity: O(log n)."
  (loop for k downfrom (floor (- position line-start) *wrap-stretch*) above 0
        do (let ((anchor (tuatara.text:rope-char-start rope (+ line-start (* k *wrap-stretch*)))))
             (when (<= anchor position) (return anchor)))
        finally (return line-start)))

(defun next-wrap-anchor (rope line-start position)
  "The first place after POSITION where a row of the line beginning at
LINE-START begins, where lines wrap, however the rows before it fall
\(WRAP-ANCHOR); NIL when that would be past the end of ROPE's text.
Complexity: O(log n)."
  (let ((total (tuatara.text:rope-bytes rope)))
    (loop for k from (1+ (floor (- position line-start) *wrap-stretch*))
          for grid = (+ line-start (* k *wrap-stretch*))
          do (when (>= grid total) (return nil))
             (let ((anchor (tuatara.text:rope-char-start rope grid)))
               (when (> anchor position) (return anchor))))))

;;; Right-to-left text in a row

(defun right-to-left-between-p (rope from to)
  "True when a character of ROPE from FROM below TO, both character starts,
can change the order a line is shown in (RIGHT-TO-LEFT-CODE-P). Only the
octets that begin a character at U+0590 or above are decoded. Complexity:
O(TO - FROM)."
  (declare (type fixnum from to))
  (let ((total (tuatara.text:rope-bytes rope)))
    (tuatara.text:with-octet-block (block (+ +scan-block+ 4))
      (loop for pos of-type fixnum from from below to by +scan-block+
            do (let* ((stop (min to (+ pos +scan-block+)))
                      (have (min total (+ stop 3))))
                 (tuatara.text:rope-copy-into rope pos have block)
                 (loop for i of-type fixnum from 0 below (- stop pos)
                       do (when (and (>= (aref block i) #xD6)
                                     (right-to-left-code-p (tuatara.text:octets-decode block i (- have pos))))
                            (return-from right-to-left-between-p t)))))
      nil)))

(defstruct (bidi-paragraph (:constructor make-bidi-paragraph (start end bytes classes levels level)) (:copier nil))
  "A stretch of a line worked out for showing right-to-left text: the octets
from START below END, the offset where each character begins (BYTES), their
bidirectional CLASSES and resolved LEVELS (BIDI-LEVELS), and the LEVEL of the
paragraph, 1 for a line written from right to left."
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (bytes #() :type (simple-array fixnum (*)) :read-only t)
  (classes #() :type simple-vector :read-only t)
  (levels #() :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (level 0 :type fixnum :read-only t))

(setf (documentation 'bidi-paragraph-p 'function) "True when OBJECT is a BIDI-PARAGRAPH."
      (documentation 'bidi-paragraph-start 'function) "Where the text of the PARAGRAPH worked out begins."
      (documentation 'bidi-paragraph-end 'function) "Where the text of the PARAGRAPH worked out ends."
      (documentation 'bidi-paragraph-bytes 'function) "The offset each character of the PARAGRAPH begins at, in order."
      (documentation 'bidi-paragraph-classes 'function) "The bidirectional class of each character of the PARAGRAPH."
      (documentation 'bidi-paragraph-levels 'function) "The level each character of the PARAGRAPH is shown at."
      (documentation 'bidi-paragraph-level 'function) "The level of the PARAGRAPH: 0 from left to right, 1 from right to left.")

(defun read-paragraph (rope from to direction)
  "The BIDI-PARAGRAPH of ROPE's text from FROM below TO, both character starts on
one line, its levels resolved as DIRECTION says: :LEFT-TO-RIGHT,
:RIGHT-TO-LEFT, or :AUTO for the direction of its first letter. Complexity:
O(TO - FROM)."
  (let ((reader (make-reader rope from to))
        (bytes (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (codes (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0)))
    (loop for at = (reader-position reader)
          for char = (reader-next reader)
          while char
          do (vector-push-extend at bytes)
             (vector-push-extend (char-code char) codes))
    (let ((classes (map 'simple-vector #'code-bidi-class codes)))
      (multiple-value-bind (levels level)
          (bidi-levels classes :codes codes :paragraph-level (case direction (:left-to-right 0) (:right-to-left 1)))
        (make-bidi-paragraph from to (coerce bytes '(simple-array fixnum (*))) classes levels level)))))

(defun row-paragraph (rope row line-start total direction known)
  "The BIDI-PARAGRAPH ROW's characters are shown by, of the line beginning at
LINE-START, as READ-PARAGRAPH works it out with DIRECTION: KNOWN, the one
the row before used, when it holds them; else the whole line, or of a line
longer than twice *WRAP-STRETCH*, the stretch of it from where the rows
holding ROW are worked out from (WRAP-ANCHOR), no further than TOTAL."
  (let* ((shown (loop for byte across (row-char-bytes row) when (>= byte 0) minimize byte into low
                      finally (return (if (< low most-positive-fixnum) low (row-start row)))))
         (until (row-shown-end row)))
    (if (and known (<= (bidi-paragraph-start known) shown) (<= until (bidi-paragraph-end known)))
        known
        (let* ((line-end (min total (tuatara.text:rope-line-end rope shown)))
               (from (wrap-anchor rope line-start shown))
               (to (if (<= (- line-end from) (* 2 *wrap-stretch*))
                       line-end
                       (let ((end (min line-end (max until (+ from (* 2 *wrap-stretch*))))))
                         (if (< end line-end) (tuatara.text:rope-char-start rope end) end)))))
          (read-paragraph rope from (max to until) direction)))))

(defun reorder-row (row paragraph limit align)
  "Put ROW's characters in the order they are read, by the levels of
PARAGRAPH, the one holding them: runs written from right to left turned
round (rules L1 and L2), a mark kept after the letter it is on, a bracket
among them mirrored (rule L4). With ALIGN, a row of a paragraph written from
right to left is set against the right of LIMIT columns, the end of its line
shown left of its text (ROW-END-COLUMN). What is shown before the row's text
\(a prefix) stays first. ROW is left as it is when nothing moves."
  (let* ((string (row-string row)) (chars (row-char-bytes row)) (bytes (row-bytes row))
         (count (length string)) (columns (row-columns row))
         (prefix (or (position-if-not #'minusp chars) count))
         (prefix-columns (or (position-if-not #'minusp bytes) columns))
         (units (make-array 16 :adjustable t :fill-pointer 0))
         (level (bidi-paragraph-level paragraph))
         (pad (if (and align (oddp level)) (max 0 (- limit columns)) 0)))
    ;; each character shown, with the marks on it: (FROM BELOW COLUMN-FROM COLUMN-BELOW BYTE)
    (loop with i = prefix and c = prefix-columns
          while (< i count)
          do (let* ((byte (aref chars i))
                    (j (or (position-if (lambda (other) (/= other byte)) chars :start i) count))
                    (k (or (position-if (lambda (other) (/= other byte)) bytes :start c) columns)))
               (if (and (= k c) (plusp (length units)))
                   (setf (second (aref units (1- (length units)))) j)
                   (vector-push-extend (list i j c k byte) units))
               (setf i j c k)))
    (let* ((n (length units))
           (classes (make-array n))
           (levels (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (u n)
        (let* ((byte (fifth (aref units u)))
               (starts (bidi-paragraph-bytes paragraph))
               (at (first-index-at-or-after starts byte))
               (found (and (< at (length starts)) (= byte (aref starts at)))))
          (setf (svref classes u) (if found (svref (bidi-paragraph-classes paragraph) at) :on)
                (aref levels u) (if found (aref (bidi-paragraph-levels paragraph) at) level))))
      (let* ((shown (line-levels classes levels level))
             (order (visual-order shown)))
        (unless (and (zerop pad) (loop for u below n always (= u (aref order u))))
          (let ((old-string (copy-seq string)) (old-faces (copy-seq (row-faces row)))
                (old-chars (copy-seq chars)) (old-bytes (copy-seq bytes))
                (moved '()))
            (setf (fill-pointer string) prefix (fill-pointer (row-faces row)) prefix
                  (fill-pointer chars) prefix (fill-pointer bytes) prefix-columns)
            (dotimes (i pad)
              (vector-push-extend #\Space string) (vector-push-extend nil (row-faces row))
              (vector-push-extend -1 chars) (vector-push-extend -1 bytes))
            (loop for u across order
                  do (destructuring-bind (from below column-from column-below byte) (aref units u)
                       (declare (ignore byte))
                       (push (list from below (length string)) moved)
                       (loop for i from from below below
                             do (vector-push-extend (if (and (oddp (aref shown u)) (= below (1+ from)))
                                                        (mirrored-char (char old-string i))
                                                        (char old-string i))
                                                    string)
                                (vector-push-extend (aref old-faces i) (row-faces row))
                                (vector-push-extend (aref old-chars i) chars))
                       (loop for c from column-from below column-below
                             do (vector-push-extend (aref old-bytes c) bytes))))
            (setf (row-columns row) (+ columns pad)
                  (row-mixed row) (list :visual
                                        (and (plusp pad) (not (row-truncated-p row))
                                             (or (row-last-p row) (< (row-shown-end row) (row-end row)))
                                             (+ prefix-columns pad -1))
                                        (and (plusp pad) (cons prefix (+ prefix pad))))
                  (row-set-in row) (loop for (from . below) in (row-set-in row)
                                         collect (let ((unit (find-if (lambda (m) (and (<= (first m) from)
                                                                                       (< from (second m))))
                                                                      moved)))
                                                   (if unit
                                                       (let ((shift (- (third unit) (first unit))))
                                                         (cons (+ from shift) (+ below shift)))
                                                       (cons from below))))))))))
  row)

(declaim (inline shown-text-end))
(defun shown-text-end (rope text-end)
  "Where the text LAYOUT shows of ROPE ends: TEXT-END when given, no further
than ROPE's end; else ROPE's end."
  (if text-end (min text-end (tuatara.text:rope-bytes rope)) (tuatara.text:rope-bytes rope)))

(defun layout (rope start width height &key (wrap nil) wrap-column word-wrap (tab-width 8) (hscroll 0) highlights crlf hidden
                                             prefix scale until invisible spares text-end direction)
  "Lay out ROPE from byte START into at most HEIGHT rows of WIDTH columns.
WRAP true wraps long lines at the column, WRAP-COLUMN when given (no further
than WIDTH), and with WORD-WRAP at a word's edge: a row ends after the last
blank that fits, a blank that does not fit ends it unseen, and only a word too
long for a row is cut; otherwise lines are truncated after skipping HSCROLL
columns, the rest of a wide character or a tab that edge cuts shown blank.
A tab reaches the next multiple of TAB-WIDTH columns. A newline, and a
carriage return no newline follows, end a line and are not shown; with
carriage return and line feed (CRLF), a carriage return just before a newline
is part of the line end and not shown. HIGHLIGHTS, if given, is passed to
APPLY-HIGHLIGHTS once the rows exist. HIDDEN, ranges of octets in order, are
not shown: a row reaching one, (START . END) (BUFFER-FOLDS), shows
*FOLD-ELLIPSIS* in :fold-ellipsis, or, (START END TEXT FACE), TEXT in FACE,
standing for its START, and goes on after it. PREFIX, when given, is a
function of a line's start and whether the row continues it, returning text
shown at the row's start and its face (an indentation that is no text):
columns that stand for no character. SCALE, when given, is a function of a
line's start giving how many rows' height its rows are drawn in (ROW-SCALE), a
rational of 1 or more: such a row holds as many times fewer columns, and
HEIGHT is then the rows' heights added up, not their number. Where lines wrap,
a row of a long line also begins every *WRAP-STRETCH* octets
\(NEXT-WRAP-ANCHOR). UNTIL, an octet, ends the layout before a row that would
begin past it. TEXT-END, an octet, is where the text shown ends, ROPE's end
unless given: of a narrowed buffer, the end of the part shown, nothing of the
text after it drawn there \(no HIDDEN text or fold, PREFIX or SCALE of its).
With INVISIBLE, a character that draws nothing (`*invisible-characters*') is
shown as its code, <FEFF>, in :invisible-character, taking those columns, as a
raw byte is shown as \\xNN in :raw-byte. A grapheme cluster takes the columns
of its first character (CLUSTER-COLUMNS), and a mark at a line's start is
shown on *MARK-BASE*. Every row takes at least one character: one too wide for
a whole wrapped row is shown as *CUT-GLYPH* (a tab as the spaces that fit).
DIRECTION, unless NIL, shows the rows of a line holding right-to-left text
\(RIGHT-TO-LEFT-CODE-P) in the order they are read (REORDER-ROW), each line a
paragraph written from left to right (:LEFT-TO-RIGHT), right to left
\(:RIGHT-TO-LEFT, every line so), or as its first letter says (:AUTO); a line
written from right to left is set against the right edge unless scrolled
sideways. A line with none of that text is laid out as with NIL.
The rows are made anew, or taken from SPARES, a vector with a fill pointer of
rows no longer shown (MAKE-ROW). Returns a simple-vector of ROW. Complexity:
O(WIDTH × HEIGHT), and for each row scrolled sideways O(*SCREEN-COLUMN-REACH*
+ log n) however long its line (SKIP-COLUMNS)."
  (declare (type fixnum start width height hscroll))
  (tuatara.text:with-octet-block (chunk +reader-chunk+)
    (let ((rows (make-array 0 :adjustable t :fill-pointer 0))
          (total (shown-text-end rope text-end))
          ;; the text shown ends before the rope does: nothing of what lies after is drawn
          (cut (and text-end (< text-end (tuatara.text:rope-bytes rope))))
          (reader (make-reader rope start (shown-text-end rope text-end) chunk))
          (position start)
          (line-start (tuatara.text:rope-line-start rope start))
          (continuation nil)
          ;; the code of the character before on the line (CLUSTER-COLUMNS), 0 at its start
          (before 0)
          (anchor nil)
          (line-scale 1)
          (used 0)
          (full-limit (if (and wrap wrap-column) (max 1 (min width wrap-column)) width))
          ;; with DIRECTION: whether the line holds right-to-left text so far, the first of its rows
          ;; not yet put in the order it is read, and the paragraph the last row was put in order by
          (right-to-left nil)
          (waiting 0)
          (paragraph nil))
      (declare (type fixnum position total full-limit before waiting) (type rational line-scale used))
      ;; a window beginning inside a line, where lines wrap, begins with a row that continues it
      (when (/= start line-start)
        (setf continuation (and wrap t) before -1
              right-to-left (and direction (right-to-left-between-p rope (wrap-anchor rope line-start start) start))))
      (loop while (and (< used height) (<= position total) (or (null until) (<= position until)))
            do (unless (and continuation (plusp (length rows)))
                 (setf line-scale (if (and scale (not (and cut (>= position total)))) (max 1 (or (funcall scale (tuatara.text:rope-line-start rope position)) 1)) 1)))
               ;; a large row fits, or nothing more is laid out (but a first row, made to fit)
               (let ((room (- height used)))
                 (when (> line-scale room)
                   (if (plusp (length rows)) (return) (setf line-scale room))))
               (let ((row (make-row position width spares))
                     (limit (max 1 (floor full-limit line-scale)))
                     (skipped 0)         ; columns consumed before the hscroll offset (truncate mode)
                     (wrapped nil)
                     (finished nil)
                     ;; with WORD-WRAP, where the row could end: after its last blank
                     (break-byte -1) (break-chars 0) (break-columns 0))
                 (declare (type fixnum skipped break-byte break-chars break-columns))
                 (setf (row-continuation-p row) continuation)
                 (unless (or continuation (zerop (length rows)))
                   (setf line-start position before 0 right-to-left nil waiting (length rows)))
                 (setf anchor (and wrap (next-wrap-anchor rope line-start position)))
                 (when (and prefix (not (and cut (>= line-start total))))
                   (multiple-value-bind (text face) (funcall prefix line-start continuation)
                     (when (and text (plusp (length text)))
                       (loop for c across text
                             do (vector-push-extend c (row-string row))
                                (vector-push-extend face (row-faces row))
                                (vector-push-extend -1 (row-char-bytes row))
                                (vector-push-extend -1 (row-bytes row)))
                       (incf (row-columns row) (length text)))))
                 (loop
                   ;; the columns scrolled off, walked over at once as far as the next fold
                   (when (< skipped hscroll)
                     (let ((at (reader-position reader)))
                       (loop while (and hidden (hidden-passed-p (first hidden) at)) do (pop hidden))
                       (multiple-value-bind (to column)
                           (skip-columns rope line-start at skipped hscroll
                                         (if hidden (min total (max at (car (first hidden)))) total)
                                         :tab-width tab-width :crlf crlf :invisible invisible)
                         (when (and direction (not right-to-left) (right-to-left-between-p rope at to))
                           (setf right-to-left t))
                         (setf (reader-position reader) to skipped column before -1))))
                   ;; what of a wide character or a tab the left edge cuts off is left blank
                   (when (> skipped hscroll)
                     (row-add row (make-string (- skipped hscroll) :initial-element #\Space) -1 (- skipped hscroll))
                     (setf skipped hscroll))
                   ;; a fold: its ellipsis, then on after it
                   (let ((at (reader-position reader)))
                     (loop while (and hidden (hidden-passed-p (first hidden) at)) do (pop hidden))
                     (when (and hidden (>= at (car (first hidden)))
                                ;; a sign or a fold of the text after the part shown is not drawn at its end
                                (not (and cut (>= (car (first hidden)) total)
                                          (/= (car (first hidden)) (hidden-end (first hidden))))))
                       (let ((entry (pop hidden)))
                         (when (>= skipped hscroll)
                           (multiple-value-bind (text face) (if (consp (cdr entry))
                                                                (values (third entry) (fourth entry))
                                                                (values *fold-ellipsis* :fold-ellipsis))
                             (let ((from (length (row-string row))))
                               (row-add row text at (string-columns text) face)
                               ;; text standing where nothing is hidden keeps its own face
                               (when (= (car entry) (hidden-end entry))
                                 (push (cons from (length (row-string row))) (row-set-in row))))))
                         (setf (reader-position reader) (min total (hidden-end entry))))))
                   ;; a long line's row begins where its rows are worked out from
                   (let ((at (reader-position reader)))
                     (when (and anchor (>= at anchor) (> at (row-start row)))
                       (setf (row-end row) at (row-shown-end row) at wrapped t)
                       (return)))
                   (let ((byte (reader-position reader))
                         (char (reader-next reader)))
                     (cond ((null char)
                            (setf (row-end row) total (row-shown-end row) total (row-last-p row) t finished t)
                            (return))
                           ((char= char #\Newline)
                            (setf (row-end row) (reader-position reader) (row-shown-end row) byte)
                            (return))
                           ((and crlf (char= char #\Return) (eql (reader-octet reader (reader-position reader)) 10))
                            ;; the line ends here, unseen: point at the return shows after the last glyph
                            (incf (reader-position reader))
                            (setf (row-end row) (reader-position reader) (row-shown-end row) byte)
                            (return))
                           ((and (char= char #\Return) (not (eql (reader-octet reader (reader-position reader)) 10)))
                            ;; a carriage return alone ends the line, unseen, as a newline does
                            (setf (row-end row) (reader-position reader) (row-shown-end row) byte)
                            (return))
                           (t
                            (when (and direction (not right-to-left) (right-to-left-code-p (char-code char)))
                              (setf right-to-left t))
                            (multiple-value-bind (string own shown)
                                (char-display char (+ skipped (row-columns row)) tab-width invisible)
                              (multiple-value-bind (columns next)
                                  (advance-column (char-code char) (+ skipped (row-columns row)) before tab-width invisible)
                                (declare (type fixnum columns))
                                (setf before next)
                                (when (and (= columns 1) (zerop own))
                                  (if (char= char (code-char #xFE0F))
                                      ;; a picture's second column that does not fit: kept with it
                                      (when (> (+ (row-columns row) 1) limit) (setf columns 0))
                                      ;; a mark with nothing before it, on a dotted circle, in a column of its own
                                      (setf string (coerce (list *mark-base* char) 'string) own 1)))
                              (cond ((< skipped hscroll)
                                     (incf skipped columns))
                                    ((> (+ (row-columns row) columns) limit)
                                     (cond ((and wrap word-wrap (char= char #\Space))
                                            ;; the blank that does not fit ends the row, unseen
                                            (setf (row-end row) (reader-position reader)
                                                  (row-shown-end row) byte
                                                  wrapped t)
                                            (return))
                                           ((and wrap word-wrap (> break-byte (row-start row)))
                                            ;; the row ends after its last blank; the word goes on the next
                                            (setf (fill-pointer (row-string row)) break-chars
                                                  (fill-pointer (row-faces row)) break-chars
                                                  (fill-pointer (row-char-bytes row)) break-chars
                                                  (fill-pointer (row-bytes row)) break-columns
                                                  (row-columns row) break-columns
                                                  (reader-position reader) break-byte
                                                  (row-end row) break-byte
                                                  (row-shown-end row) break-byte
                                                  wrapped t)
                                            (return))
                                           ((and wrap (= byte (row-start row)))
                                            ;; too wide for the whole row: cut to it, so that every row takes a character
                                            (let ((room (max 1 (- limit (row-columns row)))))
                                              (row-add row (if (char= char #\Tab)
                                                               (make-string room :initial-element #\Space)
                                                               *cut-glyph*)
                                                       byte (if (char= char #\Tab) room 1) shown))
                                            (setf (row-end row) (reader-position reader)
                                                  (row-shown-end row) (reader-position reader)
                                                  wrapped t)
                                            (return))
                                           (wrap
                                            (setf (reader-position reader) byte
                                                  (row-end row) byte
                                                  (row-shown-end row) byte
                                                  wrapped t)
                                            (return))
                                           (t
                                            (let ((nl (let ((end (tuatara.text:rope-line-end rope byte)))
                                                        ;; however long the line, found by its number
                                                        (and (< end total) end))))
                                              (setf (row-shown-end row) byte (row-truncated-p row) t)
                                              (cond (nl (setf (reader-position reader) (1+ nl)
                                                              (row-end row) (1+ nl)))
                                                    (t (setf (reader-position reader) total
                                                             (row-end row) total
                                                             (row-last-p row) t
                                                             finished t)))
                                              (return)))))
                                    ((and (= columns 1) (zerop own))
                                     ;; U+FE0F: the picture's second column, standing for the character before
                                     (let ((bytes (row-bytes row)))
                                       (vector-push-extend char (row-string row))
                                       (vector-push-extend nil (row-faces row))
                                       (vector-push-extend byte (row-char-bytes row))
                                       (vector-push-extend (if (plusp (length bytes)) (aref bytes (1- (length bytes))) byte) bytes)
                                       (incf (row-columns row))))
                                    ((zerop columns)
                                     ;; the rest of a cluster scrolled off the left is not shown
                                     (let ((bytes (row-char-bytes row)))
                                       (unless (and (plusp hscroll)
                                                    (or (zerop (length bytes)) (minusp (aref bytes (1- (length bytes))))))
                                         (row-add row string byte 0))))
                                    (t (row-add row string byte columns shown)
                                       (when (and word-wrap (or (char= char #\Space) (char= char #\Tab)))
                                         (setf break-byte (reader-position reader)
                                               break-chars (length (row-string row))
                                               break-columns (row-columns row)))))))))))
                 (setf (row-scale row) line-scale)
                 (vector-push-extend row rows)
                 ;; the line's rows, those before its first right-to-left character too, in the order they are read
                 (when (and direction (or right-to-left (eq direction :right-to-left)))
                   (loop for k from waiting below (length rows)
                         do (let ((shown (aref rows k)))
                              (setf paragraph (row-paragraph rope shown line-start total direction paragraph))
                              (reorder-row shown paragraph limit (zerop hscroll))))
                   (setf waiting (length rows)))
                 (incf used line-scale)
                 (setf position (row-end row)
                       continuation wrapped)
                 (when finished (return))))
      (let ((result (coerce rows 'simple-vector)))
        (when highlights (apply-highlights result highlights))
        result))))

(defun point-row-column (rows byte)
  "Two values: the row index and column where byte offset BYTE is shown in ROWS,
or NIL when it is not visible."
  (loop for row across rows
        for index from 0
        do (let ((column (row-column-of row byte)))
             (when column (return (values index column))))
        finally (return nil)))
