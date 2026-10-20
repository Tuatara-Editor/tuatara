;;;; src/command/popup.lisp --- Completing a word in the text from a list beside it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Completion in the text, as Corfu gives it in Emacs: the candidates for the
;;;; word at the cursor are listed in a box under it (over it where there is no
;;;; room under it), matched as the minibuffer's are, words in any order, and
;;;; narrowed as more of the word is typed. C-n and C-p choose, RET or TAB put
;;;; the chosen one in place of the word, C-g closes the list, and so does any
;;;; command that takes the cursor out of the word or puts in what cannot be
;;;; part of it. Beside the list a second box shows what the chosen candidate
;;;; is, as Corfu's popupinfo does. In the minibuffer the list opens over the
;;;; prompt. A box of words under the cursor (SHOW-NOTE) says what is wrong
;;;; where it stands, until it moves. The boxes are laid over the rows of the
;;;; screen as they are sent to the frontend, so every frontend shows them alike.

(in-package #:tuatara.command)

(defparameter *popup-rows* 10
  "The most candidates the list beside the cursor shows at once; the list
scrolls to keep the chosen one in view.")

(defparameter *popup-width-limits* '(15 . 100)
  "The fewest and the most columns the list beside the cursor takes.")

(defparameter *popup-documentation-limits* '(60 . 12)
  "The most columns and rows the documentation beside the list takes.")

(defparameter *note-limits* '(80 . 12)
  "The most columns and rows the box of words under the cursor (SHOW-NOTE) takes.")

(defparameter *popup-reach* 256
  "The most characters the word completed in the text may hold: past that the
list closes.")

(defvar *popup-keymap*
  (let ((map (make-keymap :name "completion list")))
    (define-keys map '(("C-n" popup-next) ("<down>" popup-next) ("C-p" popup-previous) ("<up>" popup-previous)
                       ("M-<" popup-first) ("M->" popup-last)
                       ("C-v" popup-page-down) ("<next>" popup-page-down)
                       ("M-v" popup-page-up) ("<prior>" popup-page-up)
                       ("TAB" popup-insert) ("C-M-i" popup-insert) ("M-TAB" popup-insert)
                       ("C-g" popup-quit) ("M-SPC" popup-insert-separator)))
    map)
  "The keys of the list of completions beside the cursor, before every other
while it is shown: \\<popup>\\[popup-next] and \\[popup-previous] (and the arrows) choose, \\[popup-first] and \\[popup-last] the
first and last, \\[popup-page-down] and \\[popup-page-up] a page on or back, \\[popup-insert] puts the chosen
one in (the first, when none is chosen yet), and so does \\`RET' once one is
chosen (*POPUP-CHOSEN-KEYMAP*), \\[popup-quit] closes the list, \\[popup-insert-separator] puts in a space the
word goes on past. Every other key does what it always does.")

(defvar *popup-chosen-keymap*
  (let ((map (make-keymap :name "completion list, one chosen" :parent *popup-keymap*)))
    (define-key map "RET" 'popup-insert)
    map)
  "The keys of the list of completions beside the cursor while a candidate is
chosen, over *POPUP-KEYMAP*: \\`RET' puts the chosen one in. A list that
opened by itself as a path was typed has none chosen until \\<popup>\\[popup-next] or \\[popup-previous],
and \\`RET' then ends the line as it always does.")

(defun popup-keys (editor)
  "The keymap of the completion list shown in EDITOR, or NIL when none is: a
buffer's list has no keys while the minibuffer reads."
  (let ((popup (editor-popup editor)))
    (and popup
         (eq (eq (popup-target popup) :minibuffer) (and (editor-minibuffer-active editor) t))
         (if (minusp (popup-index popup)) *popup-keymap* *popup-chosen-keymap*))))

(defun close-popup (editor)
  "Close EDITOR's completion list, if one is shown."
  (setf (editor-popup editor) nil))

;;; The word completed

(defun popup-word-p (popup word)
  "True when WORD can still be the word POPUP completes: not too long, and
made of its constituents, or of spaces too once \\<popup>\\[popup-insert-separator] put one
in."
  (and (<= (length word) *popup-reach*)
       (every (lambda (c)
                (or (funcall (popup-constituent popup) c)
                    (and (popup-separator popup) (char= c #\Space))))
              word)))

(defun popup-region (popup)
  "Where the word POPUP completes is now, as three values: its start, its end
and the word; NIL when there is no longer a word to complete: the cursor gone
out of it, its buffer no longer the one shown, or what cannot be part of a
word put in it."
  (let ((editor *editor*) (target (popup-target popup)))
    (if (eq target :minibuffer)
        (let ((text (editor-minibuffer-text editor))
              (point (editor-minibuffer-point editor))
              (start (popup-start popup))
              (tail (popup-end popup)))
          (when (and (editor-minibuffer-active editor) text (<= (length tail) (length text)))
            (let ((end (- (length text) (length tail))))
              (when (and (<= start point end) (string= tail text :start2 end))
                (let ((word (subseq text start end)))
                  (when (popup-word-p popup word)
                    (values start end word)))))))
        (let ((window (editor-window editor)))
          (when (and window (eq (tuatara.display:window-buffer window) target))
            (let* ((version (tuatara.buffer:buffer-version target))
                   (start (ignore-errors (tuatara.text:anchor-position (popup-start popup) version)))
                   (end (ignore-errors (tuatara.text:anchor-position (popup-end popup) version)))
                   (point (tuatara.buffer:buffer-point target)))
              (when (and start end (<= start point end) (<= (- end start) (* 4 *popup-reach*)))
                (let ((word (tuatara.buffer:buffer-string target :start start :end end)))
                  (when (popup-word-p popup word)
                    (values start end word))))))))))

(defun popup-refresh (popup)
  "Match POPUP's candidates against the word it completes when the word has
changed. False when the list should close: the word gone, or nothing matching
it and no \\<popup>\\[popup-insert-separator] typed."
  (multiple-value-bind (start end word) (popup-region popup)
    (declare (ignore start end))
    (cond ((null word) nil)
          ((equal word (popup-input-seen popup)) t)
          (t
           (let* ((seen (popup-input-seen popup))
                  ;; more typed after what was matched: only what matched it can match now
                  (pool (if (and seen (> (length word) (length seen)) (string= seen word :end2 (length seen)))
                            (coerce (popup-candidates popup) 'list)
                            (popup-table popup))))
             (setf (popup-candidates popup) (filter-candidates word pool :keep-order (popup-keep-order popup))
                   (popup-index popup) (if (popup-auto popup) -1 0)
                   (popup-scroll popup) 0
                   (popup-input-seen popup) word
                   (popup-shown popup) nil)
             (or (plusp (length (popup-candidates popup))) (popup-separator popup)))))))

(defun popup-follow (editor)
  "Keep EDITOR's completion list with the word it completes: its candidates
matched anew when the word changed, the list closed when the word is gone or
nothing matches it."
  (let ((popup (editor-popup editor)))
    (when (and popup (not (popup-refresh popup)))
      (close-popup editor))))

(defun complete-in-text (target start end table &key annotate document (constituent #'graphic-char-p) keep-order auto)
  "Complete the word from START to END in TARGET, a buffer whose START and END
are octet offsets, or :MINIBUFFER, where they are character indices of its
text, from TABLE, a list of CANDIDATE, matched against the word as the
minibuffer matches: when one candidate alone matches it goes in at once, when
more do they are listed beside the cursor (see POPUP), and when none does the
echo area says so. With AUTO, the list opens by itself as the word is typed:
listed with none chosen whenever anything matches, a sole candidate too, unless
that is the word itself, and nothing said otherwise. ANNOTATE, DOCUMENT,
CONSTITUENT and KEEP-ORDER as POPUP has them."
  (let* ((editor *editor*)
         (popup (if (eq target :minibuffer)
                    (%make-popup target start (subseq (editor-minibuffer-text editor) end)
                                 table annotate document constituent keep-order auto)
                    (let ((version (tuatara.buffer:buffer-version target)))
                      (%make-popup target (tuatara.text:make-anchor version start)
                                   (tuatara.text:make-anchor version end :bias :after)
                                   table annotate document constituent keep-order auto)))))
    (close-popup editor)
    (if (not (popup-refresh popup))
        (unless auto (message "No match"))
        (let* ((candidates (popup-candidates popup))
               (only (and (= 1 (length candidates)) (aref candidates 0))))
          (cond ((and only (string= (candidate-string only) (popup-input-seen popup)))
                 (unless auto (message "Sole completion")))
                ((and only (not auto)) (popup-put popup only))
                (t (setf (editor-popup editor) popup)))))))

(defvar *popup-insert-functions* '()
  "Functions called with a buffer, the candidate just put in it from the list
beside the cursor and where its text starts and ends: what a candidate stands
for that is more than its text (a language server's snippet).")

(defun popup-put (popup candidate)
  "Put CANDIDATE's text in place of the word POPUP completes, the cursor after
it, and close the list, whether the text could go in or not; then call
*POPUP-INSERT-FUNCTIONS*."
  (unwind-protect
       (multiple-value-bind (start end) (popup-region popup)
         (when start
           (let ((text (candidate-string candidate)) (target (popup-target popup)) (editor *editor*))
             (if (eq target :minibuffer)
                 (replace-minibuffer-text start end text editor)
                 (let ((after (tuatara.buffer:buffer-replace target start end text)))
                   (setf (tuatara.buffer:buffer-point target) after)
                   (close-popup *editor*)
                   (dolist (f *popup-insert-functions*)
                     (funcall f target candidate start after)))))))
    (close-popup *editor*)))

;;; Keys

(defmacro with-popup ((popup) &body body)
  "Run BODY with POPUP bound to the completion list shown, or say there is none."
  `(let ((,popup (editor-popup *editor*)))
     (declare (ignorable ,popup))
     (if (null ,popup)
         (message "No list of completions is shown")
         (progn ,@body))))

(defun popup-choose (popup index)
  "Choose candidate INDEX of POPUP, wrapping round at either end, and scroll
the list to show it."
  (let ((count (length (popup-candidates popup))))
    (when (plusp count)
      (let ((index (mod index count))
            (rows (min *popup-rows* count)))
        (setf (popup-index popup) index)
        (cond ((< index (popup-scroll popup)) (setf (popup-scroll popup) index))
              ((>= index (+ (popup-scroll popup) rows)) (setf (popup-scroll popup) (1+ (- index rows)))))))))

(define-command popup-next ()
  "Choose the next candidate in the list beside the cursor, the first after the last."
  (with-popup (p) (popup-choose p (1+ (popup-index p)))))

(define-command popup-previous ()
  "Choose the previous candidate in the list beside the cursor, the last before the first."
  (with-popup (p) (popup-choose p (if (minusp (popup-index p)) -1 (1- (popup-index p))))))

(define-command popup-first ()
  "Choose the first candidate in the list beside the cursor."
  (with-popup (p) (popup-choose p 0)))

(define-command popup-last ()
  "Choose the last candidate in the list beside the cursor."
  (with-popup (p) (popup-choose p (1- (length (popup-candidates p))))))

(define-command popup-page-down ()
  "Choose the candidate a list's height further on in the list beside the cursor."
  (with-popup (p)
    (popup-choose p (min (1- (length (popup-candidates p))) (+ (popup-index p) *popup-rows*)))))

(define-command popup-page-up ()
  "Choose the candidate a list's height back in the list beside the cursor."
  (with-popup (p) (popup-choose p (max 0 (- (popup-index p) *popup-rows*)))))

(define-command popup-insert ()
  "Put the chosen candidate of the list beside the cursor in place of the word,
the first when none is chosen, and close the list."
  (with-popup (p)
    (if (plusp (length (popup-candidates p)))
        (popup-put p (aref (popup-candidates p) (max 0 (popup-index p))))
        (close-popup *editor*))))

(define-command popup-quit ()
  "Close the list of completions beside the cursor, the word as it is."
  (with-popup (p) (close-popup *editor*)))

(define-command popup-insert-separator ()
  "Put a space in the word being completed, which goes on past it: the words
typed on either side match in any order, as the minibuffer matches them."
  (with-popup (p)
    (setf (popup-separator p) t)
    (if (eq (popup-target p) :minibuffer)
        (minibuffer-insert " ")
        (let ((b (popup-target p)))
          (setf (tuatara.buffer:buffer-point b)
                (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-point b) " "))))))

;;; Drawing: boxes laid over the screen's rows

(defun popup-annotation (popup candidate)
  "What POPUP says beside CANDIDATE: its annotation, as a string, or an empty one."
  (or (candidate-annotation-text candidate)
      (and (popup-annotate popup) (ignore-errors (funcall (popup-annotate popup) candidate)))
      ""))

(defun candidate-annotation-text (candidate)
  "CANDIDATE's own annotation as one string, its fields joined; NIL when it has none."
  (let ((annotation (candidate-annotation candidate)))
    (cond ((null annotation) nil)
          ((stringp annotation) annotation)
          (t (values (join-fields annotation))))))

(defun popup-list-box (popup room-columns)
  "The rows of POPUP's list, each (STRING SPANS), for a box at most
ROOM-COLUMNS wide: a column of margin either side, the candidates' names, the
matches of the word typed in their colours, and what is said of each beside
it; the chosen one in :popup-current, the scroll bar in the right margin.
Returns the rows and the box's width."
  (let* ((candidates (popup-candidates popup))
         (count (length candidates))
         (rows (max 1 (min *popup-rows* count)))
         (first (popup-scroll popup))
         (shown (loop for i from first below (min count (+ first rows)) collect i))
         ;; control and invisible characters and octets not UTF-8 written out, as names are everywhere
         (names (mapcar (lambda (i) (shown-name (candidate-string (aref candidates i)))) shown))
         (notes (mapcar (lambda (i) (let ((note (popup-annotation popup (aref candidates i))))
                                      (if note (shown-name note) note)))
                        shown))
         (name-width (reduce #'max names :key #'panel-columns :initial-value 0))
         (note-width (reduce #'max notes :key #'panel-columns :initial-value 0))
         (limit (max 4 (min (cdr *popup-width-limits*) room-columns)))
         ;; what is said beside the names gives way first, then the names
         (note-width (min note-width (max 0 (- limit 2 name-width 2))))
         (inner (max (- (min (car *popup-width-limits*) limit) 2)
                     (+ name-width (if (plusp note-width) (+ 2 note-width) 0))))
         (inner (min inner (- limit 2)))
         (name-width (min name-width (- inner (if (plusp note-width) (+ 2 note-width) 0))))
         (width (+ inner 2))
         (components (tuatara.text:split-words (or (popup-input-seen popup) "")))
         (thumb (when (> count rows)
                  (let* ((size (max 1 (round (* rows rows) count)))
                         (top (round (* (- rows size) first) (max 1 (- count rows)))))
                    (cons top (+ top size))))))
    (values
     (if (zerop count)
         (list (let ((text (tuatara.display:fit-columns "No match" nil (- width 2) :width #'panel-char-columns)))
                 (list (box-row-text text width)
                       (list (list 0 width '(:popup-annotation :popup))))))
         (loop for i in shown
               for name in names
               for note in notes
               for row from 0
               collect (let* ((current (= i (popup-index popup)))
                              (base (if current '(:popup-current :popup) '(:popup)))
                              (name (tuatara.display:fit-columns name nil name-width :width #'panel-char-columns))
                              (note (if (plusp note-width) (tuatara.display:fit-columns note nil note-width :width #'panel-char-columns) ""))
                              (text (with-output-to-string (out)
                                      (write-char #\Space out)
                                      (write-string (tuatara.display:pad-columns name name-width :width #'panel-char-columns) out)
                                      (when (plusp note-width)
                                        (write-string "  " out)
                                        (write-string (tuatara.display:pad-columns note note-width :width #'panel-char-columns) out))
                                      (write-char #\Space out)))
                              (note-start (+ 1 (length name) (- name-width (panel-columns name)) 2))
                              ;; the chosen one's face first: where it has a colour of its own, it shows
                              (over (lambda (face) (if current (list :popup-current face :popup) (list face :popup))))
                              (spans (append
                                      (list (list 0 (length text) base))
                                      (loop for (s e face) in (match-spans components name)
                                            collect (list (1+ s) (1+ e) (funcall over face)))
                                      (when (plusp (length note))
                                        (list (list note-start (+ note-start (length note)) (funcall over :popup-annotation))))
                                      (when (and thumb (<= (car thumb) row) (< row (cdr thumb)))
                                        (list (list (1- (length text)) (length text) '(:popup-bar :popup)))))))
                         (list text (flatten-spans spans (length text) :last t)))))
     width)))

(defun wrap-text (text width)
  "TEXT, a docstring, as lines of at most WIDTH columns, broken between words
\(TUATARA.DISPLAY:FILL-WORDS), a word longer than a line cut. Its lines run
on into one paragraph, filled anew; a blank line ends a paragraph and is
kept, and an indented line, as an example is, stands on its own, as it is
where it fits."
  (let ((lines '()) (words '()))
    (flet ((flush ()
             (when words
               (setf lines (revappend (tuatara.display:fill-words (nreverse words) width :width #'panel-char-columns :cut t)
                                      lines)
                     words '()))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (let ((line-words (remove "" (uiop:split-string line :separator '(#\Space)) :test #'string=)))
          (cond ((null line-words) (flush) (push "" lines))
                ((member (char line 0) '(#\Space #\Tab))
                 ;; as it is where it fits, else filled
                 (flush)
                 (if (<= (panel-columns line) width)
                     (push line lines)
                     (progn (setf words (reverse line-words)) (flush))))
                (t (setf words (revappend line-words words))))))
      (flush))
    (or (nreverse lines) (list ""))))

(defun boxed-lines (paragraphs room-columns limits)
  "PARAGRAPHS, each (TEXT . FACE), as the lines of a box at most ROOM-COLUMNS
wide and as LIMITS, (WIDTH . HEIGHT), lets it be: each TEXT broken between
words to fit inside a space either side (WRAP-TEXT), the lines past the height
left out. Two values: the lines, each (TEXT . FACE), and the box's width, as
narrow as its widest line lets it."
  (let* ((width (min room-columns (car limits)))
         (lines (loop for (text . face) in paragraphs
                      append (mapcar (lambda (line) (cons line face)) (wrap-text text (- width 2)))))
         (lines (subseq lines 0 (min (length lines) (cdr limits)))))
    (values lines (min width (+ 2 (reduce #'max lines :key (lambda (line) (panel-columns (car line))) :initial-value 0))))))

(defun box-row-text (text width)
  "TEXT as a row of a box WIDTH columns wide: a space either side, padded
with spaces to the width."
  (format nil " ~a " (tuatara.display:pad-columns text (- width 2) :width #'panel-char-columns)))

(defun box-rows (lines width)
  "The rows of a box WIDTH columns wide showing LINES, each (TEXT FACES KEYS):
TEXT with a space either side, padded to the width, in FACES, a list of
faces, the parts of it KEYS names, each (START END), in :help-key-binding over
them. Each row (STRING SPANS)."
  (loop for (text faces keys) in lines
        collect (let ((row (box-row-text text width)))
                  (list row (flatten-spans (cons (list 0 (length row) faces)
                                              (loop for (start end) in keys
                                                    collect (list (1+ start) (1+ end) (cons :help-key-binding faces))))
                                             (length row) :last t)))))

(defun documentation-lines (popup candidate width)
  "The lines of the box beside POPUP's list showing CANDIDATE's documentation
at most WIDTH columns wide (BOXED-LINES), and the box's width, kept for the
next time; NIL when there is none (yet: it may come later, asked of a language
server, and is asked again then)."
  (let ((shown (popup-shown popup)))
    (if (and shown (eq (first shown) candidate) (= (second shown) width))
        (values (third shown) (fourth shown))
        (let ((text (ignore-errors (funcall (popup-document popup) candidate))))
          (when (and text (plusp (length (string-trim '(#\Space #\Newline) text))))
            (multiple-value-bind (lines box-width)
                (boxed-lines (list (list (string-trim '(#\Newline) text))) width *popup-documentation-limits*)
              (setf (popup-shown popup) (list candidate width lines box-width))
              (values lines box-width)))))))

(defun popup-documentation-box (popup room-columns)
  "The rows of the box beside POPUP's list that shows its chosen candidate's
documentation, each (STRING SPANS), at most ROOM-COLUMNS wide and
*POPUP-DOCUMENTATION-LIMITS* high, its first line in :popup-heading; NIL when
there is nothing to show or no room. Returns the rows and the box's width."
  (let ((candidates (popup-candidates popup)))
    (when (and (popup-document popup) (plusp (length candidates)) (>= (popup-index popup) 0) (>= room-columns 20))
      (multiple-value-bind (lines width)
          (documentation-lines popup (aref candidates (popup-index popup)) (min room-columns (car *popup-documentation-limits*)))
        (when lines
          (let ((open nil))
            (values (box-rows (loop for (line) in lines
                                    for k from 0
                                    collect (multiple-value-bind (plain keys)
                                                ;; a key the line before broke off goes on here
                                                (unmark-keys (if open (concatenate 'string (string +key-open+) line) line))
                                              (setf open (let ((start (position +key-open+ line :from-end t)))
                                                           (and start (not (find +key-close+ line :start start)))))
                                              (list plain (if (zerop k) '(:popup-heading :popup) '(:popup)) keys)))
                              width)
                    width)))))))

(defun box-top (row height below)
  "The screen row a box HEIGHT rows tall starts on beside screen ROW, BELOW
rows free under that row and ROW over it: under the row where there is room,
else over it where there is, else on the side with the more room. Two
values: the row, and how many rows of the box fit there."
  (let ((top (cond ((>= below height) (1+ row))
                   ((>= row height) (- row height))
                   ((>= below row) (1+ row))
                   (t 0))))
    (values top (min height (if (> top row) below row)))))

(defun popup-anchor (editor popup panel-top minibuffer-row)
  "Where on the screen the word POPUP completes starts, as two values: the
row and the column; NIL when it is not shown. PANEL-TOP is the first row of
the panel under the windows and MINIBUFFER-ROW the minibuffer's row among the
panel's."
  (if (eq (popup-target popup) :minibuffer)
      (values (+ panel-top minibuffer-row)
              (minibuffer-column editor (popup-start popup)))
      (let ((start (popup-region popup)))
        (when start
          (text-anchor (editor-window editor) start (tuatara.buffer:buffer-point (popup-target popup)))))))

(defun text-anchor (window position &optional instead)
  "Where on the screen POSITION of WINDOW's text is, as two values: the screen
row and column; INSTEAD's place when POSITION is scrolled out of sight; NIL
when neither is shown."
  (let ((rows (tuatara.display:window-rows window)))
    (multiple-value-bind (row column)
        (multiple-value-bind (row column) (tuatara.display:point-row-column rows position)
          (if (or row (null instead))
              (values row column)
              (tuatara.display:point-row-column rows instead)))
      (when (and row (< row (tuatara.display:window-height window)))
        ;; a row drawn large, or moved down under rows drawn large: the last screen row its band covers
        (let* ((scale (tuatara.display:row-scale (aref rows row)))
               (shift (loop for i below row sum (1- (tuatara.display:row-scale (aref rows i)))))
               (last (+ row (ceiling (+ shift scale)) -1)))
          (values (+ (tuatara.display:window-text-top window) (min last (1- (tuatara.display:window-height window))))
                  (+ (tuatara.display:area-left window) (tuatara.display:window-margin window)
                     (tuatara.display:large-column column scale))))))))

(defun box-over-area-p (box row column columns rows)
  "True when BOX, one of POPUP-BOXES, lies over any of the ROWS by COLUMNS
cells from screen ROW and COLUMN."
  (destructuring-bind (top left lines) box
    (let ((width (reduce #'max lines :key (lambda (line) (panel-columns (first line))) :initial-value 0)))
      (and (< top (+ row rows)) (< row (+ top (length lines)))
           (< left (+ column columns)) (< column (+ left width))))))

(defun popup-boxes (editor tree-rows minibuffer-row)
  "The boxes to lay over the screen, each (ROW COLUMN ROWS), ROWS being
\(STRING SPANS) from screen ROW and COLUMN down: EDITOR's completion list and
the documentation beside it (COMPLETION-BOXES), else the box of words under
the cursor (NOTE-BOXES). TREE-ROWS rows hold the windows and MINIBUFFER-ROW is
the minibuffer's row in the panel under them."
  (if (editor-popup editor)
      (completion-boxes editor tree-rows minibuffer-row)
      (note-boxes editor tree-rows)))

(defun completion-boxes (editor tree-rows minibuffer-row)
  "The boxes to lay over the screen for EDITOR's completion list, each (ROW
COLUMN ROWS), ROWS being (STRING SPANS) from screen ROW and COLUMN down: the
list under the word it completes, or over it where there is no room under it
(in the minibuffer always over it), and the documentation beside the list.
TREE-ROWS rows hold the windows and MINIBUFFER-ROW is the minibuffer's row in
the panel under them."
  (let ((popup (editor-popup editor)) (width (editor-width editor)))
    (when popup
      (multiple-value-bind (row column) (popup-anchor editor popup tree-rows minibuffer-row)
        (when row
          (multiple-value-bind (list-rows list-width) (popup-list-box popup width)
            (multiple-value-bind (top fit)
                (box-top row (length list-rows) (if (eq (popup-target popup) :minibuffer) 0 (- tree-rows (1+ row))))
              (let* ((list-rows (subseq list-rows 0 (min (length list-rows) (max 1 fit))))
                     (left (max 0 (min (1- column) (- width list-width))))
                     (boxes (list (list top left list-rows)))
                     (room-right (- width (+ left list-width)))
                     (room-left left))
                (multiple-value-bind (doc-rows doc-width)
                    (popup-documentation-box popup (max room-right room-left))
                  (when doc-rows
                    (let* ((doc-left (if (>= room-right room-left) (+ left list-width) (- left doc-width)))
                           (limit (if (eq (popup-target popup) :minibuffer) row tree-rows))
                           (doc-top (max 0 (min top (- limit (length doc-rows)))))
                           (doc-rows (subseq doc-rows 0 (min (length doc-rows) (- limit doc-top)))))
                      (when (and doc-rows (>= doc-left 0))
                        (push (list doc-top doc-left doc-rows) boxes)))))
                boxes))))))))

;;; A box of words under the cursor

(defun show-note (editor buffer position paragraphs)
  "Show PARAGRAPHS in a box under the cursor, at POSITION of BUFFER, while the
cursor stays there and no completion list or question is shown: each
paragraph (TEXT FACE), TEXT broken to fit, in FACE over :popup (or :popup
alone with FACE NIL)."
  (setf (editor-note editor) (list buffer position paragraphs)))

(defun close-note (editor)
  "No longer show the box of words under the cursor."
  (setf (editor-note editor) nil))

(defun note-rows (paragraphs room-columns)
  "The rows of the box of PARAGRAPHS (SHOW-NOTE), each (STRING SPANS), at most
ROOM-COLUMNS wide and *NOTE-LIMITS* high. Returns the rows and the box's width."
  (multiple-value-bind (lines width)
      (boxed-lines (loop for (text face) in paragraphs collect (cons (string-trim '(#\Newline #\Space) text) face))
                   room-columns *note-limits*)
    (values (box-rows (loop for (line . face) in lines collect (list line (if face (list face :popup) '(:popup))))
                      width)
            width)))

(defun note-boxes (editor tree-rows)
  "The box of EDITOR's note (SHOW-NOTE) as POPUP-BOXES gives boxes, under the
cursor's row, over it where there is no room under it (BOX-TOP); NIL when
there is no note, or the cursor has left it, or the minibuffer reads."
  (let ((note (editor-note editor)) (window (editor-window editor)))
    (when (and note (not (editor-minibuffer-active editor)))
      (destructuring-bind (buffer position paragraphs) note
        (when (and (eq buffer (tuatara.display:window-buffer window))
                   (eql position (tuatara.buffer:buffer-point buffer)))
          (multiple-value-bind (row column) (text-anchor window position)
            (when row
              (multiple-value-bind (rows width) (note-rows paragraphs (editor-width editor))
                (multiple-value-bind (top fit) (box-top row (length rows) (- tree-rows (1+ row)))
                  (when (plusp fit)
                    (list (list top (max 0 (min column (- (editor-width editor) width))) (subseq rows 0 fit)))))))))))))

(defun splice-row (string spans column box box-spans)
  "STRING, a row of the screen, and its SPANS (START END FACE), with BOX, a
string with spans BOX-SPANS, laid over it from screen COLUMN: the columns BOX
takes show it instead of what was there, and a wide character either edge
cuts becomes spaces. Two values: the string and its spans."
  (let* ((n (length string))
         (starts (tuatara.display:string-column-starts string)))
    (let* ((box-end (+ column (panel-columns box)))
           ;; the characters wholly before the box, and the first one wholly after it
           (head (or (loop for i from 0 below n
                           when (> (aref starts (1+ i)) column) return i)
                     n))
           (tail (or (loop for i from head below n
                           when (>= (aref starts i) box-end) return i)
                     n))
           (pad-before (max 0 (- column (aref starts head))))
           (pad-after (if (< tail n) (- (aref starts tail) box-end) 0))
           (box-at (+ head pad-before))
           (shift (- (+ box-at (length box) pad-after) tail)))
      (values (concatenate 'string (subseq string 0 head) (make-string pad-before :initial-element #\Space)
                           box (make-string pad-after :initial-element #\Space) (subseq string tail))
              (append (loop for (s e face) in spans
                            when (< s head) collect (list s (min e head) face))
                      (loop for (s e face) in box-spans collect (list (+ s box-at) (+ e box-at) face))
                      (loop for (s e face) in spans
                            when (> e tail) collect (list (+ (max s tail) shift) (+ e shift) face)))))))

(defun boxes-on-row (row boxes)
  "The parts of BOXES (POPUP-BOXES) on screen row ROW, each (LEFT TEXT SPANS),
the uppermost first."
  (loop for (top left rows) in boxes
        when (and (<= top row) (< row (+ top (length rows))))
          collect (cons left (nth (- row top) rows))))

(defun lay-boxes-over (string spans face row boxes width)
  "The row ROW of the screen, STRING with SPANS and under them all FACE, with
the parts of BOXES (see POPUP-BOXES) on that row laid over it, the row WIDTH
columns wide. Three values: the string, its spans and the face under them all,
which is NIL once a box is laid over the row, the row's face then among its
spans; as they came when no box is on the row."
  (let ((mine (boxes-on-row row boxes)))
    (if (null mine)
        (values string spans face)
        (let* ((string (tuatara.display:pad-columns string width :width #'panel-char-columns))
               ;; the row's face under the padding too
               (spans (if face (flatten-spans (cons (list 0 (length string) face) spans) (length string)) spans)))
          (dolist (box (reverse mine))
            (destructuring-bind (left text box-spans) box
              (multiple-value-setq (string spans) (splice-row string spans left text box-spans))))
          (values string spans nil)))))
