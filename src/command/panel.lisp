;;;; src/command/panel.lisp --- The panel under the prompt: completion candidates and key hints.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defparameter *panel-max-rows* 10
  "Most rows the bottom panel takes for candidates, prompt row included.")

(defparameter *prefix-help-key* "C-h"
  "The key that, after a prefix key, lists in *Help* every key under the prefix.")

(defparameter *taller-panel-key* "M-t"
  "The key that makes the candidates of a question as tall as the frame has
room for, or as tall as usual again.")

(define-setting *key-hint-rows* nil
  "The most rows the key hints take after a prefix key (\\`C-x', \\`SPC').
The echo row under them is included. Where they do not all fit, the last row
ends in ... and \\`C-h' after the prefix lists every key under it
\(*PREFIX-HELP-KEY*).
- nil: as many as the frame has room for, as a menu takes (the standard).
- a whole number, 1 or more: that many rows at most."
  :values (or (member nil) (integer 1 *)))

(defun panel-room (editor rows tall)
  "The most rows the panel takes for what it shows: ROWS, or when TALL all
the frame has; never more than leave two rows of the frame, a row of a window
and its mode line, nor fewer than one."
  (let ((most (max 1 (- (editor-height editor) 2))))
    (if tall most (min rows most))))

(defun key-hint-room (editor)
  "The most rows the key hints take, the echo row under them included:
*KEY-HINT-ROWS*, or when that is NIL all the frame has room for (PANEL-ROOM)."
  (panel-room editor (or *key-hint-rows* 0) (null *key-hint-rows*)))

(defun narrowed-candidates (completion all query history fresh)
  "The candidates of ALL matching QUERY, sorted as FILTER-CANDIDATES sorts
them: when QUERY goes on from the one COMPLETION filtered ALL for last, and
FRESH is false (ALL has not had more come into it since), only those that
matched it are looked at again, for whatever matches QUERY matched that one.
Complexity: O(those matching the query before)."
  (destructuring-bind (&optional before before-query matching) (completion-narrowed completion)
    (multiple-value-bind (candidates matched)
        (filter-candidates query
                           (if (and (not fresh) (eq before all) (<= (length before-query) (length query))
                                    (string= before-query query :end2 (length before-query)))
                               matching
                               all)
                           :history history :keep-order (completion-keep-order completion))
      (setf (completion-narrowed completion) (list all query matched))
      candidates)))

(defun update-candidates (editor)
  "Recompute the completion candidates for the current minibuffer input if it
changed, or when more have come from the background (COMPLETION-STALE), the
selected one kept selected then; the first time, select the one the read's
SELECT function names."
  (let ((completion (editor-completion editor)))
    (when completion
      (let* ((input (editor-minibuffer-text editor))
             (same (equal input (completion-input-seen completion))))
        (unless (and same (not (completion-stale completion)))
          (let* ((table (completion-table completion))
                 (all (if (functionp table) (funcall table input) table))
                 (query (if (functionp table) (input-query input (completion-separator completion)) input))
                 (history (let ((h (completion-history completion))) (if (functionp h) (funcall h) h)))
                 (kept (and same (completion-moved completion) (selected-candidate completion)))
                 (was (completion-index completion))
                 (select (and (not same)
                              (or (null (completion-input-seen completion)) (completion-reselect completion))
                              (completion-select completion)))
                 (candidates (group-candidates
                              (default-first
                               completion input
                               (if (or (completion-filter completion) (not (functionp table)))
                                   (narrowed-candidates completion all query history same)
                                   (coerce all 'simple-vector))))))
            (setf (completion-candidates completion) candidates
                  (completion-index completion)
                  (let ((index (cond (kept (position (candidate-string kept) candidates
                                                     :key #'candidate-string :test #'string=))
                                     ;; more come, the line typed was selected: it stays so
                                     ((and same (= was -1) (input-selectable-p completion)) :input)
                                     (select (funcall select candidates input)))))
                    (cond ((and (eq index :input) (input-selectable-p completion)) -1)
                          ((and (integerp index) (< -1 index (length candidates))) index)
                          ;; nothing to choose: the line typed, where it can be taken
                          ((and (zerop (length candidates)) (input-selectable-p completion)) -1)
                          (t 0)))
                  (completion-stale completion) nil
                  (completion-moved completion) (and same (completion-moved completion))
                  (completion-input-seen completion) input)))))))

(defun default-candidate-p (completion candidate)
  "True when CANDIDATE stands for COMPLETION's DEFAULT: its value is the
default, or its string when the default is a string."
  (let ((default (completion-default completion)))
    (or (equal (candidate-result candidate) default)
        (and (stringp default) (string= (candidate-string candidate) default)))))

(defun default-first (completion input candidates)
  "CANDIDATES, a simple-vector, with the one standing for COMPLETION's DEFAULT
first while INPUT is empty, the others in their order."
  (let ((at (and (completion-default completion) (zerop (length input))
                 (position-if (lambda (c) (default-candidate-p completion c)) candidates))))
    (if (and at (plusp at))
        (concatenate 'simple-vector (vector (aref candidates at)) (subseq candidates 0 at) (subseq candidates (1+ at)))
        candidates)))

(defun default-words (completion)
  "COMPLETION's DEFAULT as its prompt names it: its candidate's string, or the
default itself."
  (let* ((default (completion-default completion))
         (table (completion-table completion))
         (candidate (and (listp table) (find-if (lambda (c) (default-candidate-p completion c)) table))))
    (cond (candidate (candidate-string candidate))
          ((stringp default) default)
          ((symbolp default) (string-downcase (symbol-name default)))
          (t (princ-to-string default)))))

(defun prompt-with-words (prompt words)
  "PROMPT with WORDS in brackets before its colon, as \"Open file: \" given
\"listing, 12 so far\". That gives \"Open file (listing, 12 so far): \".
PROMPT itself when WORDS is NIL."
  (let ((trimmed (string-right-trim " " prompt)))
    (cond ((null words) prompt)
          ((and (plusp (length trimmed)) (char= (char trimmed (1- (length trimmed))) #\:))
           (format nil "~a (~a): " (subseq trimmed 0 (1- (length trimmed))) words))
          (t (format nil "~a(~a) " prompt words)))))

(defun shown-prompt (completion)
  "The prompt COMPLETION's question shows, the one asked, with in brackets the
default it takes (default X), and while candidates still come, how many so far
as its SOURCE tells (listing, 12 so far), or once all have come and there are
none, its NONE words; for example, Open file (listing, 12 so far): ."
  (let ((words (remove nil (list (and (completion-default completion)
                                      (format nil "default ~a" (default-words completion)))
                                 (cond ((completion-working completion)
                                        (and (completion-listed completion)
                                             (format nil "listing, ~:d so far" (completion-listed completion))))
                                       ((and (completion-none completion) (eql 0 (completion-listed completion)))
                                        (completion-none completion)))))))
    (prompt-with-words (let ((prompt (completion-prompt completion)))
                         (if (functionp prompt) (funcall prompt) prompt))
                       (and words (format nil "~{~a~^; ~}" words)))))

(defun take-in-source (editor)
  "Call the SOURCE of the completing read under way, when it has one, to take
in what was found in the background: its candidates worked out again when
they may have changed, the prompt saying how many so far (SHOWN-PROMPT).
True while more may come."
  (let* ((completion (editor-completion editor))
         (source (and completion (completion-source completion))))
    (when source
      (multiple-value-bind (changed working listed) (funcall source)
        (setf (completion-working completion) working
              (completion-listed completion) listed)
        (when changed (setf (completion-stale completion) t))
        (when (editor-minibuffer-active editor)
          (let ((prompt (values (escape-controls (shown-prompt completion)))))
            (unless (equal prompt (editor-minibuffer-prompt editor))
              (setf (editor-minibuffer-prompt editor) prompt))))
        working))))

(defun input-selectable-p (completion)
  "True when COMPLETION's read takes what is typed, so that the line typed can
be selected as its candidates are."
  (not (completion-require-match completion)))

(defun selected-candidate (completion)
  "The candidate COMPLETION has selected, or NIL when it is the line typed or there is none."
  (let ((candidates (completion-candidates completion)) (index (completion-index completion)))
    (and (< -1 index (length candidates)) (aref candidates index))))

(defun preview-selection (editor)
  "Call the preview function of the completing read under way, when it has
one, with the selected candidate (NIL when there is none) if that is not the
one it was last called with."
  (let ((completion (editor-completion editor)))
    (when (and completion (completion-preview completion))
      (let ((candidate (selected-candidate completion)))
        (unless (eq candidate (completion-previewed completion))
          (setf (completion-previewed completion) candidate)
          (funcall (completion-preview completion) candidate))))))

(defun input-query (input &optional (separator #\/))
  "The part of INPUT that filters candidates when the table is a function that
already narrowed by the input: the text after the last SEPARATOR, so that file
completion matches names within the directory typed (a slash), and a list of
words the word being typed (a comma)."
  (let ((at (position separator input :from-end t)))
    (if at (subseq input (1+ at)) input)))

(defvar *key-hint-focus-functions* '()
  "Functions of the editor and the keys typed so far, giving what the key hints
show first, as (TITLE KEY...): TITLE what the cursor is on (\"link\"), each KEY
the description of a key after those typed (\"C-o\") that acts on it; NIL to
leave it to the next. The keys bound are shown under TITLE, then the others.")

(defun key-hint-focus (editor entries)
  "ENTRIES, the key hints for the keys EDITOR has pending, split by
*KEY-HINT-FOCUS-FUNCTIONS*: three values, the title, the entries for what the
cursor is on, in the order the function gives, and the rest; NIL when none
is for it."
  (let ((focus (tuatara.runtime:run-hook-until-success *key-hint-focus-functions* editor (editor-pending editor))))
    (when focus
      (let ((chosen (loop for key in (rest focus)
                          for entry = (find key entries :key #'car :test #'string=)
                          when entry collect entry)))
        (when chosen
          (values (first focus) chosen (remove-if (lambda (e) (member e chosen)) entries)))))))

(defun key-hints-unavailable (editor entries)
  "The keys of ENTRIES, key hints for the keys EDITOR has pending, whose
commands cannot run here now (HOST-COMMAND-REFUSAL), shown dimmed."
  (let* ((keymaps (active-keymaps editor))
         (pending (editor-pending editor))
         (found '()))
    (dolist (map keymaps)
      (let ((target (if pending (lookup-key map pending) map)))
        (when (keymap-p target)
          (dolist (entry (keymap-entries target))
            (let ((key (tuatara.frontend:key-description (car entry))))
              (when (and (assoc key entries :test #'string=) (not (member key found :test #'string=)))
                (let ((command (keymaps-lookup keymaps (append pending (list (car entry))))))
                  (when (and command (symbolp command) (not (keywordp command))
                             (host-command-refusal tuatara.runtime:*host* command))
                    (push key found)))))))))
    found))

(defun key-hint-column-width (&rest lists)
  "The width of a column of key hints holding every entry of LISTS, each a
list of (KEY . NAME): the widest, KEY NAME, and two spaces after it. Groups
laid out with it (KEY-HINT-ROWS' COLUMN-WIDTH) line up."
  (+ 2 (reduce #'max lists
               :key (lambda (list)
                      (reduce #'max list :key (lambda (entry) (+ (length (car entry)) 1 (length (cdr entry))))
                                         :initial-value 0))
               :initial-value 0)))

(defun key-hint-columns (entries width column-width)
  "How many columns of key-hint ENTRIES, a list of (KEY . NAME), fit in WIDTH,
and how wide each is, as two values: COLUMN-WIDTH wide when given, else as
wide as the widest entry (KEY-HINT-COLUMN-WIDTH)."
  (let ((column-width (max (or column-width 1) (if entries (key-hint-column-width entries) 1))))
    (values (max 1 (floor width column-width)) column-width)))

(defun key-hint-row-count (entries width max-rows &key column-width)
  "How many rows KEY-HINT-ROWS lays ENTRIES out in, given the same WIDTH,
MAX-ROWS and COLUMN-WIDTH, without laying them out."
  (min (max 0 max-rows) (ceiling (length entries) (key-hint-columns entries width column-width))))

(defun key-hint-rows (entries width max-rows
                      &key (note (format nil "... ~a lists all" *prefix-help-key*)) column-width unavailable)
  "Lay out key-hint ENTRIES, a list of (KEY . NAME), in columns fitting WIDTH
and at most MAX-ROWS rows, as panel rows (STRING FACE SPANS). The columns are
COLUMN-WIDTH wide when given (KEY-HINT-COLUMN-WIDTH, the same for groups shown
one under another), else as wide as the widest entry. NOTE ends them when not
all fit (as standard, the key that lists them all, *PREFIX-HELP-KEY*). It
comes after the last row's entries, or on a row of its own when it is too long
for that. The keys get :key-hint-key, prefix names (starting with +)
:key-hint-prefix, commands :key-hint-command, and those of the keys
UNAVAILABLE, whose commands cannot run here, :key-hint-unavailable."
  (multiple-value-bind (columns column-width) (key-hint-columns entries width column-width)
    (let* ((entries (coerce entries 'simple-vector))
           (cells (map 'simple-vector (lambda (e) (format nil "~a ~a" (car e) (cdr e))) entries))
           (rows-needed (ceiling (length cells) columns)))
      (labels ((lay-out (rows)
                 (let ((shown (min (length cells) (* rows columns)))
                       (result '()))
                   (dotimes (r rows)
                     (let ((line (make-string-output-stream)) (spans '()) (pos 0))
                       (loop for c from 0 below columns
                             for i = (+ r (* c rows))
                             when (< i shown)
                               do (destructuring-bind (key . name) (svref entries i)
                                    (let ((cell (svref cells i)))
                                      (push (list pos (+ pos (length key)) :key-hint-key) spans)
                                      (push (list (+ pos (length key) 1) (+ pos (length cell))
                                                  (cond ((member key unavailable :test #'string=) :key-hint-unavailable)
                                                        ((and (plusp (length name)) (char= (char name 0) #\+))
                                                         :key-hint-prefix)
                                                        (t :key-hint-command)))
                                            spans)
                                      (write-string cell line)
                                      (incf pos (length cell))
                                      (when (< (1+ c) columns)
                                        (let ((pad (- column-width (length cell))))
                                          (write-string (tuatara.text:spaces pad) line)
                                          (incf pos pad))))))
                       (push (list (string-right-trim " " (get-output-stream-string line)) nil (nreverse spans))
                             result)))
                   (values (nreverse result) (< shown (length cells))))))
        (multiple-value-bind (rows cut) (lay-out (min rows-needed max-rows))
          (let ((last (first (car (last rows)))))
            (cond ((or (not cut) (null rows)) rows)
                  ((or (<= (+ (length last) 2 (length note)) width) (< max-rows 2))
                   (append (butlast rows) (list (cons (format nil "~a  ~a" last note) (rest (car (last rows)))))))
                  (t (append (lay-out (1- max-rows)) (list (list note nil '())))))))))))

(define-setting *prompt-above-candidates* t
  "Whether the prompt of a completing read is shown above its candidates.
- t: above them, the list reading down from it (the standard).
- nil: on the bottom row, under them."
  :values boolean)

(defparameter *group-line-character* (code-char #x2500)
  "The character the line beside a group's title is drawn with: ─.")

(defun flatten-spans (spans length &key last)
  "SPANS, lists (START END FACE) over a string of LENGTH characters, laid
flat: in order, none overlapping, a run of the same faces one span, nothing
where no span is. Where spans overlap, a character has the faces of all of
them, those of the later spans first, or with LAST the face of the last of
them alone."
  (let* ((spans (remove-if-not (lambda (span) (< (max 0 (first span)) (min (second span) length))) spans))
         (cuts (sort (remove-duplicates (loop for (start end) in spans
                                              collect (max 0 start) collect (min end length)))
                     #'<))
         (flat '()))
    (loop for (from to) on cuts
          while to
          do (let ((faces '()))
               (loop for (start end face) in spans
                     when (and (<= start from) (>= end to))
                       do (setf faces (if last
                                          (list face)
                                          (append (if (listp face) face (list face)) faces))))
               (when faces
                 (let ((face (if (rest faces) faces (first faces)))
                       (before (first flat)))
                   ;; a run of the same faces is one span
                   (if (and before (= (second before) from) (equal (third before) face))
                       (setf (second before) to)
                       (push (list from to face) flat))))))
    (nreverse flat)))

(defun group-title-row (title width)
  "The panel row that opens the group TITLE: a short line, the title and a
line on to the last column but one."
  (let* ((lead (make-string 4 :initial-element *group-line-character*))
         (label (fit-panel-columns (format nil " ~a " (escape-controls title)) nil (max 0 (- width 5))))
         (tail (make-string (max 0 (- width 1 4 (panel-columns label))) :initial-element *group-line-character*))
         (string (concatenate 'string lead label tail)))
    (list string nil (remove-if (lambda (span) (>= (first span) (second span)))
                                (list (list 0 4 :completion-group-line)
                                      (list 4 (+ 4 (length label)) :completion-group-title)
                                      (list (+ 4 (length label)) (length string) :completion-group-line))))))

(defun candidate-page (candidates index room)
  "What the panel shows of CANDIDATES in ROOM rows so that the one at INDEX is
among them: a list of (:TITLE GROUP) and (:CANDIDATE I), a group's title
before its first candidate shown."
  (flet ((items-from (first)
           (let ((items '()) (rows 0) (previous :none))
             (loop for i from first below (length candidates)
                   for group = (candidate-group (aref candidates i))
                   do (let ((title (and group (not (equal group previous)))))
                        (cond ((<= (+ rows (if title 2 1)) room)
                               (when title (push (list :title group) items) (incf rows))
                               (push (list :candidate i) items)
                               (incf rows))
                              ((zerop rows) (push (list :candidate i) items) (incf rows))
                              (t (loop-finish)))
                        (setf previous group)))
             (nreverse items))))
    (let* ((first (max 0 (min (- index (floor room 2)) (- (length candidates) room))))
           (items (items-from first)))
      (loop until (or (>= first index)
                      (find index items :key (lambda (item) (and (eq (first item) :candidate) (second item)))))
            do (incf first)
               (setf items (items-from first)))
      items)))

(defun candidate-left (candidate widths)
  "The text of CANDIDATE's row before its annotation, with its spans, and how
much of it the words typed are matched in: its string, and its suffix right
after it; or its columns, each but the last padded to its width in WIDTHS.
Control and invisible characters are written out (ESCAPE-CONTROLS)."
  (let ((columns (candidate-columns candidate))
        (suffix (candidate-suffix candidate))
        (string (escape-controls (candidate-string candidate))))
    (cond (columns
           (multiple-value-bind (text spans)
               (join-fields (loop for field in columns
                                  for rest on columns
                                  for width in widths
                                  collect (multiple-value-bind (text face) (field-parts field nil)
                                            (let ((text (if (rest rest)
                                                            (fit-panel-columns text nil width)
                                                            text)))
                                              (list text face (and (rest rest) width)))))
                            :default-face nil)
             (values text spans (length text))))
          (suffix
           (multiple-value-bind (text spans) (join-fields suffix :default-face nil)
             (let ((offset (1+ (length string))))
               (values (concatenate 'string string " " text)
                       (loop for (start end face) in spans
                             collect (list (+ offset start) (+ offset end) face))
                       (length string)))))
          (t (values string '() (length string))))))

(defun fit-key-list (string spans columns name-end)
  "STRING, a name ending at NAME-END and the keys that run it in brackets after
it, as \"undo (u, C-/, C-_)\", and its SPANS, lists of (START END FACE), cut
to take at most COLUMNS columns. The keys give way whole from the end of the
list, \"…\" standing for them. Then the list goes. Then the name is cut at its
end (FIT-COLUMNS). Returns the string and the spans."
  (flet ((cut (keep tail)
           (let ((text (concatenate 'string (subseq string 0 keep) tail)))
             (values text (loop for (start end face) in spans
                                when (< start keep)
                                  collect (list start (if (>= end (length string))
                                                          (length text)
                                                          (min end keep)) face))))))
    (if (<= (panel-columns string) columns)
        (values string spans)
        (let ((keep (loop for at = (search ", " string :from-end t) then (search ", " string :from-end t :end2 at)
                          while (and at (> at name-end))
                          when (<= (+ (panel-columns string :end at) 4) columns) return at)))
          (cond (keep (cut keep ", …)"))
                ((<= (panel-columns string :end name-end) columns) (cut name-end ""))
                (t (fit-panel-columns (subseq string 0 name-end) spans columns)))))))

(defun fit-left (candidate left spans columns matched)
  "LEFT, the text of CANDIDATE's row before its annotation, and its SPANS,
cut to take at most COLUMNS columns: a candidate with keys after its name
gives up keys (FIT-KEY-LIST), one in columns is cut at its end, any other
gives up its middle, as a path keeps its start and its name (FIT-COLUMNS).
MATCHED is how much of LEFT is the candidate's own string."
  (cond ((candidate-suffix candidate) (fit-key-list left spans columns matched))
        (t (fit-panel-columns left spans columns :middle (null (candidate-columns candidate))))))

(defun candidate-row (candidate selected width widths components annotation)
  "The panel row of CANDIDATE: its string or columns at the left, the parts
COMPONENTS match highlighted, and ANNOTATION ending at the last column but one.
When both do not fit, a long string gives up its middle down to half the row,
or the keys after its name down to the name (FIT-LEFT), then the annotation
is cut, or left out when little of it would show.
SELECTED paints the row in :completion-selected."
  (multiple-value-bind (left left-spans matched) (candidate-left candidate widths)
    (let* ((left-spans (flatten-spans (append left-spans (match-spans components (subseq left 0 matched)))
                                      (length left)))
           (room (max 1 (1- width)))
           (fields (cond ((null annotation) '())
                         ((stringp annotation) (list annotation))
                         (t annotation)))
           (wanted (fields-columns fields room)))
      (when (> (panel-columns left) room)
        (multiple-value-setq (left left-spans) (fit-left candidate left left-spans room matched)))
      ;; a long candidate gives up its middle down to half the row, or its keys down to its name,
      ;; so that its annotation shows whole
      (let ((over (- (+ (panel-columns left) 2 wanted) room)))
        (when (and (plusp over) (plusp wanted) (null (candidate-columns candidate)))
          (let ((keep (max (if (candidate-suffix candidate) (panel-columns left :end matched) (ceiling room 2))
                           (- (panel-columns left) over))))
            (when (< keep (panel-columns left))
              (multiple-value-setq (left left-spans) (fit-left candidate left left-spans keep matched))))))
      (multiple-value-bind (right right-spans)
          (let ((free (- room (panel-columns left) 2)))
            (if (or (null fields) (< free 8))
                (values "" '())
                (fit-fields fields free :row-width room)))
        (let* ((gap (if (zerop (length right)) 0 (- room (panel-columns left) (panel-columns right))))
               (offset (+ (length left) gap)))
          (list (concatenate 'string left (tuatara.text:spaces gap) right)
                (and selected :completion-selected)
                (append left-spans
                        (loop for (start end face) in right-spans
                              collect (list (+ offset start) (+ offset end) face)))))))))

(defun column-widths (candidates indices room annotation-width)
  "The width of each column of the candidates at INDICES that have columns:
the widest text of that column among them, at most half of ROOM; the widest
column narrows, down to ten, so that the columns and ANNOTATION-WIDTH fit."
  (let ((widths '()))
    (dolist (i indices)
      (loop for field in (candidate-columns (aref candidates i))
            for k from 0
            do (let ((w (min (floor room 2) (panel-columns (field-parts field nil)))))
                 (if (< k (length widths))
                     (setf (nth k widths) (max (nth k widths) w))
                     (setf widths (append widths (list w)))))))
    (let ((over (- (+ (reduce #'+ widths) (* 2 (max 0 (1- (length widths))))
                      (if (plusp annotation-width) (+ 2 annotation-width) 0))
                   room)))
      (when (and (plusp over) widths)
        (let ((widest (position (reduce #'max widths) widths)))
          (setf (nth widest widths) (max (min 10 (nth widest widths)) (- (nth widest widths) over))))))
    widths))

(defun completion-page (editor completion)
  "What the panel shows of COMPLETION's candidates in the rows it has for
them (CANDIDATE-PAGE): NIL when there are none."
  (let ((candidates (completion-candidates completion)))
    (and (plusp (length candidates))
         (candidate-page candidates (max 0 (completion-index completion))
                         (1- (panel-room editor *panel-max-rows* (completion-tall completion)))))))

(defun completion-rows (editor completion width items)
  "The panel rows of COMPLETION's candidates WIDTH wide, ITEMS being what of
them is shown (COMPLETION-PAGE): group titles, and the candidates around the
selected one."
  (let* ((candidates (completion-candidates completion))
         (index (completion-index completion))
         (indices (loop for (kind i) in items when (eq kind :candidate) collect i))
         (annotate (completion-annotate completion))
         (annotations (let ((table (make-hash-table)))
                        (dolist (i indices table)
                          (let ((c (aref candidates i)))
                            (setf (gethash i table)
                                  (or (candidate-annotation c) (and annotate (funcall annotate c))))))))
         (room (max 1 (1- width)))
         (widths (column-widths candidates indices room
                                (loop for i in indices
                                      maximize (let ((a (gethash i annotations)))
                                                 (fields-columns (if (stringp a) (list a) a) room)))))
         (table (completion-table completion))
         (input (editor-minibuffer-text editor))
         (components (tuatara.text:split-words (if (functionp table)
                                                   (input-query input (completion-separator completion))
                                                   input))))
    (loop for (kind value) in items
          collect (if (eq kind :title)
                      (group-title-row value width)
                      (candidate-row (aref candidates value) (= value index) width widths components
                                     (gethash value annotations))))))

(defvar *minibuffer-initial-selected* nil
  "True while the text a question offered is selected: what is typed takes its
place, \\<minibuffer>\\[minibuffer-backward-delete-char] clears it, any other key leaves it as it is.")

(defun prompt-pieces (editor)
  "The minibuffer's prompt as the panel shows it, as two values: the rows
holding its start over the minibuffer's row, when it is too wide to leave a
third of the row for the answer, broken between words; and what of it stays
on the minibuffer's row (all of it when it fits). A long question is so read
whole, its start never scrolled off the left."
  (let ((width (max 10 (1- (editor-width editor)))))
    (prompt-broken (editor-minibuffer-prompt editor) width
                   (- width (length (completion-count-prefix editor)) (floor width 3)))))

(defun prompt-broken (prompt width room)
  "PROMPT, a question, as three values: the rows of WIDTH columns holding its
start, broken between words, when it takes more than ROOM columns; what of
it is left for the last row, no more than ROOM (all of it when it fits);
and where in PROMPT each of those rows begins."
  (if (<= (panel-columns prompt) room)
      (values '() prompt '())
      (let ((rows '()) (starts '()) (start 0))
        (loop while (> (panel-columns prompt :start start) room)
              do (let* ((limit (+ start (max 1 (columns-end (subseq prompt start) width))))
                        (break (or (position #\Space prompt :start (1+ start) :end limit :from-end t) limit)))
                   (push (subseq prompt start break) rows)
                   (push start starts)
                   (setf start
                         (min (length prompt)
                              (if (and (< break (length prompt))
                                       (char= (char prompt break) #\Space)) (1+ break) break)))))
        (values (nreverse rows) (subseq prompt start) (nreverse starts)))))

(defun spans-within (spans start end)
  "Those of SPANS, each (START END FACE), that reach into START to END, cut
to it and counted from START."
  (loop for (from to face) in spans
        when (and (> to start) (< from end))
          collect (list (- (max from start) start) (- (min to end) start) face)))

(defun key-question-pieces (editor)
  "While a question of one key waits in the echo area (READ-KEY), its prompt
as PROMPT-BROKEN gives it. The first value is the rows over the echo row
holding its start, when it is wider than the row, and what of it stays on the
echo row, the cursor after it. Else it is NIL and the message shown. The third
value is where in the message each of those rows begins. A long question is so
read whole, as the minibuffer's are."
  (let ((text (or (editor-message editor) "")))
    (if (and *key-question* (echo-question-p editor))
        (let ((width (max 10 (1- (editor-width editor)))))
          (prompt-broken text width width))
        (values '() text '()))))

(defun key-hint-parts (editor)
  "The key hints for the keys EDITOR has pending, as the panel shows them: a
list of (TITLE ENTRIES MAX-ROWS COLUMN-WIDTH), each entry (KEY . NAME). What
acts on what the cursor is on comes first under its title, then the rest
\(KEY-HINT-FOCUS); else all of them, with no title. MAX-ROWS of the first is
what its rows may take, of the rest what the first leaves; COLUMN-WIDTH is
shared by both, or NIL."
  (let ((entries (prefix-entries (active-keymaps editor) (editor-pending editor)))
        (room (key-hint-room editor)))
    (multiple-value-bind (title chosen rest) (key-hint-focus editor entries)
      (if chosen
          (let ((column (key-hint-column-width chosen rest)))
            (list (list title chosen (- room 2) column) (list nil rest (- room 2) column)))
          (list (list nil entries (1- room) nil))))))

(defun key-hint-panel-rows (editor width)
  "The panel rows of the key hints for the keys EDITOR has pending, WIDTH
wide (KEY-HINT-PARTS), the keys whose commands cannot run here dimmed
\(KEY-HINTS-UNAVAILABLE)."
  (let* ((parts (key-hint-parts editor))
         (unavailable (key-hints-unavailable editor (loop for part in parts append (second part))))
         (rows '()))
    (loop for (title entries max-rows column) in parts
          do (when title (setf rows (append rows (list (group-title-row title width)))))
             (setf rows (append rows (key-hint-rows entries width (max 0 (- max-rows (max 0 (1- (length rows)))))
                                                    :column-width column :unavailable unavailable))))
    rows))

(defun key-hint-panel-height (editor width)
  "How many rows KEY-HINT-PANEL-ROWS gives, counted without making them."
  (let ((rows 0))
    (loop for (title entries max-rows column) in (key-hint-parts editor)
          do (when title (incf rows))
             (incf rows (key-hint-row-count entries width (max 0 (- max-rows (max 0 (1- rows))))
                                            :column-width column)))
    rows))

(defun panel-rows (editor)
  "The rows of the bottom panel as a list of (STRING FACE SPANS), and the
index of the minibuffer or echo row among them.
While the minibuffer reads, they are the prompt row (its prompt in
:minibuffer-prompt) and the candidates of a completing read, the selected one
in :completion-selected. The candidates are above or below the prompt as
*PROMPT-ABOVE-CANDIDATES* says, as many rows as the most it has shown for the
question (COMPLETION-TALLEST), at most CANDIDATE-ROOM. After the text typed
comes what a key in it said, in brackets.
Otherwise they are the menu open (OPEN-MENU), while no key of it is half
typed; the key hints under a pending prefix key; else the echo row."
  (let ((width (editor-width editor)))
    (cond ((editor-minibuffer-active editor)
           (let* ((completion (editor-completion editor))
                  (items (and completion (completion-page editor completion)))
                  (rows (and items (completion-rows editor completion width items)))
                  ;; the panel grows but does not shrink while the question is open: blank
                  ;; rows on the side away from the prompt, so that the windows above stay put
                  (rows (if completion
                            (let ((blank (max 0 (- (setf (completion-tallest completion)
                                                         (max (completion-tallest completion) (length rows)))
                                                   (length rows)))))
                              (if *prompt-above-candidates*
                                  (append rows (make-list blank :initial-element (list "" nil nil)))
                                  (append (make-list blank :initial-element (list "" nil nil)) rows)))
                            rows))
                  (head '())
                  (prompt-row (multiple-value-bind (rows last) (prompt-pieces editor)
                                (setf head (mapcar (lambda (text)
                                                     (list text nil (list (list 0 (length text) :minibuffer-prompt))))
                                                   rows))
                                (minibuffer-prompt-row editor completion last))))
             (if *prompt-above-candidates*
                 (values (append head (list prompt-row) rows) (length head))
                 (values (append rows head (list prompt-row)) (+ (length rows) (length head))))))
          ((and (editor-menu editor) (menu-keys-pending-p editor))
           (let ((rows (menu-panel-rows editor width (1- (panel-room editor 0 t)))))
             (values (append rows (list (echo-row editor)))
                     (length rows))))
          ((and (editor-key-hints editor) (editor-pending editor))
           (let ((rows (key-hint-panel-rows editor width)))
             (values (append rows (list (echo-row editor)))
                     (length rows))))
          (t (multiple-value-bind (rows last starts) (key-question-pieces editor)
               (declare (ignore last))
               ;; the start of a long question of one key, its keys coloured as on the echo row
               (let* ((spans (and rows (text-spans (editor-message editor))))
                      (head (loop for row in rows
                                  for start in starts
                                  collect (list row nil (spans-within spans start (+ start (length row)))))))
                 (values (append head (list (echo-row editor))) (length head))))))))

(defun minibuffer-prompt-row (editor completion prompt)
  "The panel row of EDITOR's minibuffer, as (STRING FACE SPANS): PROMPT, what
of the prompt stays on it (PROMPT-PIECES), in :minibuffer-prompt, the text
typed, the text a question offers selected in :region, the line typed in
:completion-selected while it is selected over COMPLETION's candidates, and
what a key said, in brackets."
  (multiple-value-bind (whole typed hidden) (minibuffer-row editor prompt)
    (let* ((prompt-start (max 0 (- (length (completion-count-prefix editor)) hidden)))
           (prompt-end (max 0 (- typed hidden)))
           (row (subseq whole hidden))
           (said (editor-message editor)))
      (list (if said (concatenate 'string row " [" said "]") row)
            nil
            (append
             (and (< prompt-start prompt-end)
                  (list (list prompt-start prompt-end :minibuffer-prompt)))
             ;; the text a question offers, selected: typing takes its place
             (and *minibuffer-initial-selected* (< prompt-end (length row))
                  (list (list prompt-end (length row) :region)))
             ;; the line typed selected, over candidates: what RET takes, painted as they are
             (and completion (= (completion-index completion) -1)
                  (plusp (length (completion-candidates completion)))
                  (< prompt-end (length row))
                  (list (list prompt-end (length row) :completion-selected)))
             ;; the keys in what a key said, coloured as in the echo area
             (let ((offset (+ (length row) 2)))
               (loop for (start end face) in (text-spans said)
                     collect (list (+ start offset) (+ end offset) face))))))))

(defun panel-height (editor)
  "How many rows the bottom panel takes now, as many as PANEL-ROWS gives,
counted without making them: no candidate is annotated, no menu entry or key
hint laid out."
  (let ((width (editor-width editor)))
    (cond ((editor-minibuffer-active editor)
           (let ((completion (editor-completion editor)))
             (+ (length (prompt-pieces editor)) 1
                (if completion
                    (max (completion-tallest completion) (length (completion-page editor completion)))
                    0))))
          ((and (editor-menu editor) (menu-keys-pending-p editor))
           (1+ (menu-panel-height editor width (1- (panel-room editor 0 t)))))
          ((and (editor-key-hints editor) (editor-pending editor))
           (1+ (key-hint-panel-height editor width)))
          (t (1+ (length (key-question-pieces editor)))))))

(define-setting *show-completion-count* t
  "Whether a completing read shows the place of the selected candidate and how
many there are, before the prompt.
They change with what is typed: 3/120 (*/0 when nothing matches and the text
typed can be taken, !/0 when it cannot). Before a search's prompt it is the
match the cursor is at and how many there are.
- t: they are shown (the standard).
- nil: they are not."
  :values boolean)

(defun completion-count-prefix (editor)
  "What the minibuffer row shows before the prompt, in a place six columns
wide so that the prompt stays put as it changes: the candidate count of a
completing read (*SHOW-COMPLETION-COUNT*), what another question counts
\(EDITOR-MINIBUFFER-COUNT, the place of a search's match), or nothing."
  (let ((completion (editor-completion editor))
        (count (editor-minibuffer-count editor)))
    (cond ((not *show-completion-count*) "")
          (completion
           (let* ((total (length (completion-candidates completion)))
                  (place (cond ((and (plusp total) (>= (completion-index completion) 0))
                                (1+ (completion-index completion)))
                               ((completion-require-match completion) "!")
                               (t "*"))))
             (format nil "~6a " (format nil "~a/~a" place total))))
          (count (format nil "~6a " count))
          (t ""))))

(defun minibuffer-row-text (editor &optional (prompt (nth-value 1 (prompt-pieces editor))))
  "The minibuffer's row whole, before any of it is scrolled off its left: the
candidate count, PROMPT, what of the prompt stays on the row (PROMPT-PIECES),
and the text typed as it is shown; and where in it the text typed begins."
  (let ((head (concatenate 'string (completion-count-prefix editor) prompt)))
    (values (concatenate 'string head (minibuffer-shown-text editor)) (length head))))

(defun scrolled-off (row index room)
  "How many characters of ROW, a row of the panel, are scrolled off its
left so that the character at INDEX (or the cursor after the row's text)
stands within ROOM columns: none when it does, else whole grapheme clusters,
wide characters counting their two columns (PANEL-COLUMNS)."
  (let ((hidden (min (length row) index))
        (columns 0))
    ;; back from the cursor for as many characters as fit
    (loop while (plusp hidden)
          do (let* ((start (tuatara.text:string-cluster-start row hidden))
                    (more (panel-columns row :start start :end hidden)))
               (when (> (+ columns more) room) (return))
               (incf columns more)
               (setf hidden start)))
    hidden))

(defun minibuffer-row (editor &optional (prompt (nth-value 1 (prompt-pieces editor))))
  "The minibuffer's row as three values: the row whole (MINIBUFFER-ROW-TEXT,
PROMPT what of the prompt stays on it), where in it the text typed begins,
and how many of its characters are scrolled off its left, so that the cursor
stays on the screen when the row takes more columns than it has
\(SCROLLED-OFF)."
  (multiple-value-bind (row typed) (minibuffer-row-text editor prompt)
    (values row typed (scrolled-off row (+ typed (editor-minibuffer-point editor)) (1- (editor-width editor))))))

(defun echo-question-p (editor)
  "True while a question in the echo area waits for a key, the cursor
standing after its text rather than in the window: a question of one key
\(READ-KEY), a key sequence half typed, a count begun with
\\[universal-argument], a menu open. The minibuffer has its own cursor."
  (and (not (editor-minibuffer-active editor))
       (or *key-question* (editor-pending editor) (editor-collecting-prefix editor) (editor-menu editor))
       t))

(defun echo-row (editor)
  "The echo row of the panel, as (STRING FACE SPANS): the message shown,
with the keys in it coloured; while a question of one key waits there
\(READ-KEY), what of its prompt stays on that row, its start on the rows
over it when it is wider than the row (KEY-QUESTION-PIECES). A message shown
while a menu is open or a key sequence half typed is shown from its start,
whole, as any message."
  (let* ((text (or (editor-message editor) ""))
         (spans (text-spans (editor-message editor)))
         (hidden (- (length text) (length (nth-value 1 (key-question-pieces editor))))))
    (if (zerop hidden)
        (list text nil spans)
        (list (subseq text hidden) nil
              (loop for (start end face) in spans
                    when (> end hidden)
                      collect (list (max 0 (- start hidden)) (- end hidden) face))))))

(defun echo-cursor-column (editor)
  "The column of the echo row the cursor stands at while a question there
waits for a key: after the text shown (ECHO-ROW), or its last column when
that text takes the whole row."
  (min (panel-columns (first (echo-row editor))) (1- (editor-width editor))))

(defun minibuffer-column (editor index)
  "The column on the minibuffer's row where the character INDEX of the text
typed is shown, past what is scrolled off its left (MINIBUFFER-ROW): wide
characters before it two columns, marks none."
  (multiple-value-bind (row typed hidden) (minibuffer-row editor)
    (panel-columns row :start hidden :end (max hidden (min (length row) (+ typed index))))))
