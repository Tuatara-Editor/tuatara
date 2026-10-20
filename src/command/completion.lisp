;;;; src/command/completion.lisp --- Matching in any order, candidate sorting, key-hint entries.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Matching: the input is split into
;;;; space-separated components that must all match somewhere in a candidate,
;;;; in any order; each component matches as a substring, an initialism or,
;;;; failing those, a flex subsequence. Pure functions; the minibuffer's
;;;; question is in minibuffer.lisp and its panel in panel.lisp.

(in-package #:tuatara.command)

(defstruct (candidate (:constructor make-candidate (string &key annotation value group columns suffix rank)) (:copier nil))
  "One completion candidate: STRING is what is matched, and what is shown
unless COLUMNS says otherwise; SUFFIX, fields shown right after STRING, such
as a command's keys after its name in M-x; ANNOTATION what is shown beside
it at the right edge (a docstring line, a size and a date); VALUE what
the caller gets back (STRING when NIL); GROUP the title of the group it is
listed under, or NIL; COLUMNS, when not NIL, the fields shown in its place,
each lined up with the same field of the candidates around it. An annotation
is a string or a list of fields. A field is a string, or a list (TEXT FACE WIDTH
CUT): FACE a face name, or a list of (START END FACE) over TEXT; WIDTH, when
given, the columns it takes, its text at the left of them, or at the right for
a negative WIDTH, or a fraction, that share of the row, the text cut to fit at
its end, or in its middle when CUT is :MIDDLE. RANK, when given, puts it with
the entries of a completing read's history, the lower the sooner: for files,
how lately it was chosen."
  (string "" :type string :read-only t)
  (annotation nil :type (or null string list) :read-only t)
  (value nil :read-only t)
  (group nil :type (or null string) :read-only t)
  (columns nil :type list :read-only t)
  (suffix nil :type list :read-only t)
  (rank nil :read-only t))

(setf (documentation 'make-candidate 'function) "Return a CANDIDATE for STRING with ANNOTATION, VALUE, GROUP and COLUMNS."
      (documentation 'candidate-p 'function) "True when OBJECT is a CANDIDATE."
      (documentation 'candidate-string 'function) "The text of CANDIDATE."
      (documentation 'candidate-annotation 'function) "The annotation of CANDIDATE, or NIL."
      (documentation 'candidate-value 'function) "The value CANDIDATE stands for, or NIL to mean its string."
      (documentation 'candidate-group 'function) "The title of the group CANDIDATE is listed under, or NIL."
      (documentation 'candidate-columns 'function) "The fields CANDIDATE is shown as, or NIL to show its string."
      (documentation 'candidate-suffix 'function) "The fields shown right after CANDIDATE's string, or NIL."
      (documentation 'candidate-rank 'function) "How soon CANDIDATE comes with a read's history (lower sooner), or NIL.")

(defun candidate-result (candidate)
  "What choosing CANDIDATE returns: its value, or its string."
  (or (candidate-value candidate) (candidate-string candidate)))

(defun smart-case-p (component)
  "True when COMPONENT has an upper-case letter, so matching is case-sensitive."
  (some #'upper-case-p component))

(defun word-separator-p (c)
  "True when C separates the words of a name: - _ / . * or a space."
  (find c "-_/.* "))

(defun word-start-p (c previous)
  "True when C, after the character PREVIOUS (NIL at the start), begins a word:
words are separated by WORD-SEPARATOR-P, or by a change to upper case."
  (and (not (word-separator-p c))
       (or (null previous) (word-separator-p previous)
           (and (lower-case-p previous) (upper-case-p c)))))

(defun initialism-p (component string case-sensitive)
  "True when COMPONENT's characters are the first characters of STRING's first words (WORD-START-P)."
  (let ((i 0) (n (length component)) (previous nil))
    (loop for c across string
          while (< i n)
          do (when (word-start-p c previous)
               (unless (if case-sensitive (char= c (char component i)) (char-equal c (char component i)))
                 (return-from initialism-p nil))
               (incf i))
             (setf previous c))
    (= i n)))

(defun flex-match-p (component string case-sensitive)
  "True when COMPONENT's characters occur in STRING in order, not necessarily adjacent."
  (let ((i 0))
    (loop for c across string
          while (< i (length component))
          do (when (if case-sensitive (char= c (char component i)) (char-equal c (char component i)))
               (incf i)))
    (= i (length component))))

(defun component-match (component string &optional (case-sensitive (smart-case-p component)))
  "How COMPONENT matches STRING: :prefix, :substring, :initialism, :flex, or
NIL, and as a second value where it was found for the first two.
Case-insensitive unless CASE-SENSITIVE, true when COMPONENT contains upper
case."
  (let ((at (search component string :test (if case-sensitive #'char= #'char-equal))))
    (cond ((and at (zerop at)) (values :prefix 0))
          (at (values :substring at))
          ((initialism-p component string case-sensitive) :initialism)
          ((flex-match-p component string case-sensitive) :flex)
          (t nil))))

(defparameter *match-scores* '((:prefix . 0) (:substring . 100) (:initialism . 300) (:flex . 400))
  "Rank of each match kind, lower is better: what COMPONENT-SCORE starts from.")

(defun flex-spread (component string case-sensitive)
  "How spread out COMPONENT's characters are in STRING, found in order from its
start: the gaps between them counted ten each, and the characters skipped
inside the match one each; 0 when they stand together."
  (let ((i 0) (first nil) (last nil) (gaps 0))
    (loop for c across string
          for at from 0
          while (< i (length component))
          do (when (if case-sensitive (char= c (char component i)) (char-equal c (char component i)))
               (when (and last (> at (1+ last))) (incf gaps))
               (unless first (setf first at))
               (setf last at)
               (incf i)))
    (if first (+ (* 10 gaps) (- (1+ last) first (length component))) 0)))

(defun component-score (component string &optional (case-sensitive (smart-case-p component)))
  "How well COMPONENT matches STRING, lower better, or NIL when it does not:
its kind of match (*MATCH-SCORES*, COMPONENT-MATCH), a part of a name found
earlier and at a word's start better, letters in order less spread out
better (FLEX-SPREAD); CASE-SENSITIVE as COMPONENT-MATCH has it. The second
value says how closely it matches, lower closer: 0 found at the start of a
word, 1 found inside one, 2 the first letters of its words, 3 its letters
apart."
  (multiple-value-bind (kind at) (component-match component string case-sensitive)
    (when kind
      ;; at the start of a word before inside one
      (let ((inside (and (eq kind :substring) (not (word-separator-p (char string (1- at)))))))
        (values (+ (cdr (assoc kind *match-scores*))
                   (case kind
                     (:substring (+ (min 49 at) (if inside 50 0)))
                     (:flex (min 99 (flex-spread component string case-sensitive)))
                     (t 0)))
                (case kind
                  ((:prefix :substring) (if inside 1 0))
                  (:initialism 2)
                  (t 3)))))))

(defun input-components (input)
  "INPUT's components (SPLIT-COMPONENTS), each as (COMPONENT . CASE-SENSITIVE)."
  (mapcar (lambda (component) (cons component (smart-case-p component))) (tuatara.text:split-words input)))

(defun components-score (components string)
  "The score of STRING against COMPONENTS, as INPUT-COMPONENTS gives them: the
sum of theirs (COMPONENT-SCORE), or NIL when one does not match; the second
value how closely the one matching least closely matches (0 with none)."
  (let ((score 0) (closeness 0))
    (loop for (component . case-sensitive) in components
          do (multiple-value-bind (one close) (component-score component string case-sensitive)
               (unless one (return-from components-score nil))
               (incf score one)
               (setf closeness (max closeness close))))
    (values score closeness)))

(defun candidate-score (input candidate)
  "The score of CANDIDATE against INPUT, the sum of its components' (lower is
better, COMPONENT-SCORE), or NIL when some component does not match."
  (components-score (input-components input) (candidate-string candidate)))

(defun exact-first (input candidates)
  "CANDIDATES, a list, with the one whose string is INPUT exactly brought to
the front, the others in their order."
  (let ((exact (and (plusp (length input))
                    (find input candidates :key #'candidate-string :test #'string=))))
    (if exact (cons exact (remove exact candidates :count 1)) candidates)))

(defun filter-candidates (input candidates &key history keep-order)
  "Return the CANDIDATES matching INPUT as a simple-vector sorted best first:
the one whose string is INPUT exactly; then those matching more closely
\(COMPONENT-SCORE's second value: found at the start of a word before inside
one, before the first letters of words, before letters apart); among those
matching as closely, entries of HISTORY (a list of strings, most recent
first) and candidates with a RANK before others, in that order; then those
matching better (CANDIDATE-SCORE: most of the name in one piece, near its
start), then shorter strings, then alphabetical order. With
KEEP-ORDER, those matching in the order of CANDIDATES instead, the exact one
still first. With an empty INPUT every candidate matches. The second value
lists those matching in the order of CANDIDATES."
  (let ((components (input-components input)))
    (when keep-order
      (let ((matching (remove-if-not (lambda (c) (components-score components (candidate-string c))) candidates)))
        (return-from filter-candidates (values (coerce (exact-first input matching) 'simple-vector) matching))))
    (let ((scored '()) (matching '()) (exact (plusp (length input)))
          (places (let ((table (make-hash-table :test 'equal)))
                    (loop for entry in history
                          for i from 0
                          unless (gethash entry table) do (setf (gethash entry table) i))
                    table)))
      (dolist (c candidates)
        (multiple-value-bind (score closeness) (components-score components (candidate-string c))
          (when score
            (push c matching)
            (let ((h (or (gethash (candidate-string c) places) (candidate-rank c))))
              (push (cons (list (if (and exact (string= input (candidate-string c))) 0 1) closeness (if h 0 1) (or h 0) score
                                (length (candidate-string c)))
                          c)
                    scored)))))
      (values (sorted-by-keys scored) (nreverse matching)))))

(defun sorted-by-keys (entries)
  "The candidates of ENTRIES, each (KEYS . CANDIDATE), KEYS a list of integers
of the same length, as a simple-vector in the order of their KEYS, the first
deciding first, then of their strings; those alike keep their order."
  (let ((lows '()) (shifts '()) (width 0))
    ;; each key made a field of one integer, lowest bits for the last key
    (when entries
      (loop for i from (1- (length (car (first entries)))) downto 0
            do (let ((low most-positive-fixnum) (high most-negative-fixnum))
                 (dolist (entry entries)
                   (let ((key (nth i (car entry))))
                     (when (< key low) (setf low key))
                     (when (> key high) (setf high key))))
                 (push low lows)
                 (push width shifts)
                 (incf width (integer-length (- high low))))))
    (let ((packed (map 'simple-vector
                       (lambda (entry)
                         (cons (loop for key in (car entry)
                                     for low in lows
                                     for shift in shifts
                                     sum (ash (- key low) shift))
                               (cdr entry)))
                       entries)))
      (map-into packed #'cdr
                (stable-sort packed (lambda (a b)
                                      (let ((x (car a)) (y (car b)))
                                        (or (< x y)
                                            (and (= x y) (string< (candidate-string (cdr a)) (candidate-string (cdr b))) t)))))))))

;;; Groups, fields and the parts of a candidate that match

(defun group-candidates (candidates)
  "CANDIDATES (a simple-vector) with those of the same group brought together,
the groups in the order their first candidate comes and the candidates of each
in their order: the best match of each group decides where the group comes.
CANDIDATES itself when none has a group."
  (if (notany #'candidate-group candidates)
      candidates
      (let ((order '()) (members (make-hash-table :test 'equal)))
        (loop for c across candidates
              for group = (candidate-group c)
              do (unless (nth-value 1 (gethash group members)) (push group order))
                 (push c (gethash group members)))
        (let ((result (make-array (length candidates))) (i 0))
          (dolist (group (nreverse order) result)
            (dolist (c (reverse (gethash group members)))
              (setf (aref result i) c)
              (incf i)))))))

(defun panel-char-columns (char)
  "The columns CHAR takes in the panel under the prompt: two for a wide
character, four for an octet that is not UTF-8 (written \\xNN), none for a
combining mark or for the marks round a key written into a text (KEY-MARK-P),
one for any other (a control character shows there as a single sign)."
  (let ((code (char-code char)))
    (cond ((key-mark-p char) 0)
          ((tuatara.text:raw-byte-code-p code) 4)
          ((or (< code 160) (<= #xD800 code #xDFFF)) 1)
          (t (tuatara.display:char-columns char)))))

(defun panel-columns (string &key (start 0) (end (length string)))
  "The columns STRING takes in the panel under the prompt (PANEL-CHAR-COLUMNS)."
  (tuatara.display:string-columns string :start start :end end :width #'panel-char-columns))

(defun columns-end (string columns)
  "How many characters of STRING fit in COLUMNS columns (PANEL-COLUMNS)."
  (tuatara.display:chars-in-columns string columns :width #'panel-char-columns))

(defun field-parts (field default-face)
  "FIELD (a string, or a list (TEXT FACE WIDTH CUT)) as its text, face, width
and way of cutting (:MIDDLE, or NIL for the end); a field without a face has
DEFAULT-FACE. Control and invisible characters in the text are written out
(ESCAPE-CONTROLS), the spans of a FACE that is a list moved to match."
  (if (stringp field)
      (values (escape-controls field) default-face nil nil)
      (destructuring-bind (text &optional (face default-face) width cut) field
        (if (listp face)
            (multiple-value-bind (text face) (escape-controls text face)
              (values text face width cut))
            (values (escape-controls text) face width cut)))))

(defun join-fields (fields &key (separator "  ") (default-face :completion-annotation) (row-width 80))
  "FIELDS, a list of fields (see CANDIDATE), as one string with SEPARATOR
between them and a list of spans (START END FACE) for their faces. A field
with no text and no width is left out. A width that is a fraction (1/2) is
that share of ROW-WIDTH, and the field's text is cut to fit it: at its end,
or in its middle when the field's fourth element is :MIDDLE."
  (let ((out (make-string-output-stream)) (spans '()) (pos 0) (first t))
    (dolist (field fields)
      (multiple-value-bind (text face width cut) (field-parts field default-face)
        (when (and width (not (integerp width)))
          (let ((columns (floor (* (abs width) row-width))))
            (multiple-value-bind (cut cut-spans) (tuatara.display:fit-columns text (and (listp face) face) columns
                                                                              :middle (eq cut :middle) :width #'panel-char-columns)
              (setf text cut
                    face (if (listp face) cut-spans face)
                    width (if (minusp width) (- columns) columns)))))
        (unless (and (zerop (length text)) (null width))
          (unless first
            (write-string separator out)
            (incf pos (length separator)))
          (setf first nil)
          (let* ((pad (if width (max 0 (- (abs width) (panel-columns text))) 0))
                 (start (if (and width (minusp width)) (+ pos pad) pos)))
            (when (and width (minusp width)) (write-string (make-string pad :initial-element #\Space) out))
            (write-string text out)
            (cond ((null face))
                  ((listp face) (loop for (s e f) in face do (push (list (+ start s) (+ start e) f) spans)))
                  ((plusp (length text)) (push (list start (+ start (length text)) face) spans)))
            (when (and width (plusp width)) (write-string (make-string pad :initial-element #\Space) out))
            (incf pos (+ pad (length text)))))))
    (values (get-output-stream-string out) (nreverse spans))))

(defun field-columns (field default-face row-width)
  "The columns FIELD takes when joined (JOIN-FIELDS) in a row ROW-WIDTH wide."
  (multiple-value-bind (text face width) (field-parts field default-face)
    (declare (ignore face))
    (cond ((null width) (panel-columns text))
          ((integerp width) (max (abs width) (panel-columns text)))
          (t (floor (* (abs width) row-width))))))

(defun shown-fields (fields)
  "FIELDS without those JOIN-FIELDS leaves out: no text and no width."
  (remove-if (lambda (field)
               (multiple-value-bind (text face width) (field-parts field nil)
                 (declare (ignore face))
                 (and (zerop (length text)) (null width))))
             fields))

(defun fields-columns (fields row-width)
  "The columns FIELDS take in full when joined in a row ROW-WIDTH wide."
  (let ((fields (shown-fields fields)))
    (if (null fields)
        0
        (+ (loop for field in fields sum (field-columns field nil row-width))
           (* 2 (1- (length fields)))))))

(defun fit-fields (fields limit &key (row-width limit) (default-face :completion-annotation))
  "FIELDS (see CANDIDATE) as JOIN-FIELDS joins them, made to fit LIMIT columns:
a field whose width is a share of ROW-WIDTH takes no more than the others
leave it (and none, less than eight); then fields give way from the left, the
last one left cut at its end. Returns the string and its spans."
  (let* ((fields (shown-fields fields))
         (share-p (lambda (field) (multiple-value-bind (text face width) (field-parts field default-face)
                                    (declare (ignore text face))
                                    (and width (not (integerp width))))))
         (fixed (loop for field in fields
                      unless (funcall share-p field) sum (field-columns field default-face row-width)))
         (shares (count-if share-p fields))
         (separators (* 2 (max 0 (1- (length fields))))))
    ;; the shares take what the other fields leave, down to eight columns each
    (when (plusp shares)
      (let ((each (floor (- limit fixed separators) shares)))
        (setf fields
              (loop for field in fields
                    unless (and (funcall share-p field) (< each 8))
                      collect (if (funcall share-p field)
                                  (multiple-value-bind (text face width cut) (field-parts field default-face)
                                    (let ((columns (min each (floor (* (abs width) row-width)))))
                                      (list text face (if (minusp width) (- (/ columns row-width)) (/ columns row-width)) cut)))
                                  field)))))
    ;; fields give way from the left while they do not fit
    (loop while (and (rest fields) (> (fields-columns fields row-width) limit))
          do (pop fields))
    (multiple-value-bind (string spans) (join-fields fields :row-width row-width :default-face default-face)
      (tuatara.display:fit-columns string spans limit :width #'panel-char-columns))))

(defun word-initial-positions (string)
  "The positions in STRING of the first characters of its words (WORD-START-P)."
  (loop for c across string
        for i from 0
        when (word-start-p c (and (plusp i) (char string (1- i)))) collect i))

(defun component-positions (component string)
  "The positions of the characters of STRING that COMPONENT matches, as
COMPONENT-MATCH matches it (a substring, the initials of words, or letters in
order), or NIL when it does not match."
  (let* ((cs (smart-case-p component))
         (test (if cs #'char= #'char-equal))
         (pos (search component string :test test)))
    (cond ((zerop (length component)) nil)
          (pos (loop for i from pos below (+ pos (length component)) collect i))
          ((let ((initials (word-initial-positions string)))
             (and (<= (length component) (length initials))
                  (loop for a across component for p in initials always (funcall test a (char string p)))
                  (subseq initials 0 (length component)))))
          (t (let ((i 0) (found '()))
               (loop for c across string
                     for p from 0
                     while (< i (length component))
                     do (when (funcall test c (char component i)) (push p found) (incf i)))
               (and (= i (length component)) (nreverse found)))))))

(defparameter *match-faces* #(:completion-match-0 :completion-match-1 :completion-match-2 :completion-match-3)
  "The faces of the parts of a candidate the words typed match: the first word's,
the second's, the third's, and the fourth's and those after.")

(defun match-spans (components string)
  "Spans (START END FACE) over STRING for the characters that each of
COMPONENTS (the words typed) matches, each word in its face of *MATCH-FACES*;
a character two words match keeps the first word's face."
  (let ((faces (make-array (length string) :initial-element nil)))
    (loop for component in components
          for k from 0
          do (dolist (p (component-positions component string))
               (unless (aref faces p)
                 (setf (aref faces p) (aref *match-faces* (min k (1- (length *match-faces*))))))))
    (let ((spans '()) (i 0) (n (length string)))
      (loop while (< i n)
            do (let ((face (aref faces i)))
                 (if (null face)
                     (incf i)
                     (let ((start i))
                       (loop while (and (< i n) (eq (aref faces i) face)) do (incf i))
                       (push (list start i face) spans)))))
      (nreverse spans))))

(defun first-sentence (string)
  "The first sentence of STRING, a docstring, on one line: up to its first
full stop followed by a space or a line end, or its first blank line; an empty
string for NIL."
  (if (null string)
      ""
      (let* ((stop (loop for i from 0 below (length string)
                         when (and (char= (char string i) #\.)
                                   (or (= (1+ i) (length string))
                                       (member (char string (1+ i)) '(#\Space #\Newline))))
                           return (1+ i)))
             (blank (search (format nil "~%~%") string))
             (end (min (or stop (length string)) (or blank (length string)))))
        (substitute #\Space #\Newline (string-trim '(#\Space #\Newline) (subseq string 0 end))))))

(defun first-line (string)
  "The first line of STRING, or an empty string for NIL."
  (if string
      (subseq string 0 (or (position #\Newline string) (length string)))
      ""))

(defun keymap-entries (keymap)
  "The bindings of KEYMAP and its parents as (KEY-SPEC . BINDING), a child's binding shadowing its parents'."
  (let ((seen (make-hash-table :test 'equal)) (entries '()))
    (loop for map = keymap then (keymap-parent map)
          while map
          do (loop for spec being the hash-keys of (keymap-table map) using (hash-value binding)
                   unless (gethash spec seen)
                     do (setf (gethash spec seen) t)
                        (push (cons spec binding) entries)))
    entries))

(defun prefix-entries (keymaps prefix)
  "For the key specs PREFIX, the entries reachable one key further in KEYMAPS
(consulted in order, earlier maps shadowing later ones): a list of
(KEY-DESCRIPTION . NAME) sorted by key, NAME being the command name or
\"+prefix\" for a nested keymap, or the label the key was given instead
(SET-KEY-LABEL). A key taken away there (:UNDEFINED) is left out. The
prefix is followed into each keymap and each of its parents, the nearest
first, as a key typed after it is: a mode's g keys and those its parent
mode's g has (g r) are shown together."
  (let ((seen (make-hash-table :test 'equal)) (entries '()))
    (dolist (map keymaps)
      (dolist (target (if prefix
                          (loop for ancestor = map then (keymap-parent ancestor)
                                while ancestor
                                for found = (lookup-key ancestor prefix :parents nil)
                                when (keymap-p found) collect found)
                          (list map)))
        (dolist (entry (keymap-entries target))
          (unless (gethash (car entry) seen)
            (setf (gethash (car entry) seen) t)
            ;; a key taken away (:UNDEFINED) is not shown, and hides it in the keymaps after
            (unless (eq (cdr entry) :undefined)
              (push (cons (tuatara.frontend:key-description (car entry))
                          (let ((label (key-label target (car entry))))
                            (if (keymap-p (cdr entry))
                                (format nil "+~a" (or label (keymap-name (cdr entry)) "prefix"))
                                (or label (string-downcase (symbol-name (cdr entry)))))))
                    entries))))))
    (sort entries #'string< :key #'car)))

(defvar *where-is-tables* '()
  "The tables WHERE-IS-TABLE made lately, each (GENERATION KEYMAPS TABLE), the
newest first; a few kept.")

(defun where-is-table (keymaps)
  "A table from each command bound in KEYMAPS, a list, to the key sequences,
each a list of key specs, that run it there, including through prefix
keymaps, in the order the keymaps are walked, the last first. Made once for
a list of keymaps while no keymap changes (*KEYMAPS-GENERATION*). A key
sequence counts where typing it runs the command (MAP-KEYMAP-BINDINGS).
Complexity: O(1) when made before, else that of MAP-KEYMAP-BINDINGS."
  (let ((generation *keymaps-generation*))
    (or (third (find-if (lambda (kept) (and (eql (first kept) generation) (equal (second kept) keymaps)))
                        *where-is-tables*))
        (let ((table (make-hash-table :test 'eq)))
          (map-keymap-bindings (lambda (keys command) (push keys (gethash command table))) keymaps)
          (setf *where-is-tables*
                (cons (list generation (copy-list keymaps) table)
                      (loop for kept in *where-is-tables*
                            for i below 7
                            when (eql (first kept) generation) collect kept)))
          table))))

(defun where-is (command keymaps)
  "Key descriptions that run COMMAND in KEYMAPS, including through prefix
keymaps, as a list of strings. Complexity: O(its keys) once the keymaps'
table is made (WHERE-IS-TABLE)."
  (mapcar #'tuatara.frontend:key-sequence-description (reverse (gethash command (where-is-table keymaps)))))

