;;;; lisp/modes/special.lisp --- Buffers a command fills: their text and faces, point kept on its entry.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The lists, help pages and reports commands make (*Buffers*, *Jobs*, *Help*,
;;;; *Apropos*, the agenda, *Health*, the calendar, rimu's lists, the trash,
;;;; *Fonts*, *xref*...) are filled one way. SET-MADE-TEXT puts the whole text
;;;; in, read-only, unmodified and with nothing to undo, keeping the parts shown
;;;; in a face, which special-mode's one highlight method finds by halves;
;;;; RENDER-PIECES makes that text from pieces, point kept on its line or on the
;;;; entry it was on; TABLE-HEADING-PIECES and TABLE-ROW-PIECES lay out columns
;;;; under titles, control characters in their cells escaped; the lists of
;;;; marked-list-mode have their entries marked in one way, those of
;;;; sectioned-list-mode their sections folded in one way. Each list says how
;;;; it is made again (its :REFILL local), which
;;;; revert-buffer runs, on r and g r in every one of them; KEEP-SHOWN-CURRENT
;;;; names the lists made again by themselves while shown, when what they show
;;;; has changed; q leaves each of them alike (quit-window). The marks are in
;;;; list-marks.lisp, the sections in list-sections.lisp, the columns in
;;;; list-tables.lisp, and making again, keeping current and moving from entry
;;;; to entry in list-kept-current.lisp.

(in-package #:tuatara.defaults)

(defvar *shown-lists* '()
  "The kinds of list made again by themselves while a window shows them
\(KEEP-SHOWN-CURRENT): each (MODE KEY FOLLOW).")

;;; The text

(defun buffer-in-mode (name mode)
  "The buffer called NAME, made when there is none, in the major MODE: set to
it when it is in another, left as it is when it is in MODE already."
  (let ((buffer (find-or-make-buffer name)))
    (unless (typep (buffer-major-mode buffer) mode)
      (set-major-mode buffer mode))
    buffer))

(defun fresh-made-buffer (name mode text)
  "A new buffer NAME in the major MODE, one by that name before killed,
holding TEXT (a string or a rope), read-only text a command made
\(SET-MADE-TEXT, VIEW-MADE-TEXT), point at its start."
  (let ((old (tuatara.buffer:find-buffer name)))
    (when old (tuatara.buffer:kill-buffer old)))
  (let ((buffer (buffer-in-mode name mode)))
    (set-made-text buffer text)
    (setf (tuatara.buffer:buffer-point buffer) 0)
    (view-made-text buffer)))

(defvar *made-text-keymap* (make-keymap :name "text made by a command")
  "Keys of read-only text a command shows in a buffer of its own, in a major
mode other than special-mode (a commit git shows, a file's metadata), before
its mode's: \\`q' closes it (quit-window).")

(defun view-made-text (buffer)
  "Have BUFFER, read-only text a command made in a major mode other than
special-mode, read as special-mode's buffers are in modal editing (moving,
selecting, copying, nothing that edits), coloured as its mode colours it,
\\`q' closing it (*MADE-TEXT-KEYMAP*). Returns BUFFER."
  (setf (tuatara.buffer:buffer-local buffer :modal-class) :support
        (buffer-override-keymap buffer) *made-text-keymap*)
  buffer)

(defun span-ends (spans)
  "For SPANS, a vector of (START END FACE) in the order of their starts, the
furthest end reached by each span and those before it, as a vector of the
same length: what MADE-SPANS-BETWEEN halves. O(n)."
  (let ((ends (make-array (length spans) :element-type 'fixnum)) (furthest 0))
    (loop for i from 0 below (length spans)
          do (setf furthest (max furthest (second (svref spans i)))
                   (aref ends i) furthest))
    ends))

(defun shown-list-kind (buffer)
  "The entry of *SHOWN-LISTS* for BUFFER's major mode, or NIL when it is not a
list kept current while shown."
  (let ((mode (buffer-major-mode buffer)))
    (find-if (lambda (kind) (typep mode (first kind))) *shown-lists*)))

(defun set-made-text (buffer text &key spans buttons (key nil key-p) refill)
  "Make TEXT the whole of BUFFER, a read-only buffer whose text the editor
makes: read-only again however the change ends, unmodified after (so that
killing it asks nothing), with nothing to undo. SPANS, each (START END FACE)
in octets of TEXT, a list or a vector, are the parts of it shown in a face,
found by special-mode's highlight method (MADE-SPANS-BETWEEN). BUTTONS, each
\(START END ACTION), are its buttons (\\[push-button]). KEY is what the list
was made from, for KEEP-SHOWN-CURRENT, worked out by its kind's key function
when not given. REFILL, when given, is how BUFFER is made again (its :REFILL
local, which \\[revert-buffer] calls with BUFFER). A narrowed BUFFER is
widened first. Point is left where the change leaves it. O(size of TEXT +
number of SPANS)."
  (tuatara.buffer:widen-buffer buffer)
  (tuatara.buffer:with-read-only-lifted (buffer :after t)
    (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text))
  (let ((spans (coerce spans 'simple-vector)))
    (unless (loop for i from 1 below (length spans)
                  always (<= (first (svref spans (1- i))) (first (svref spans i))))
      (setf spans (stable-sort (copy-seq spans) #'< :key #'first)))
    (setf (tuatara.buffer:buffer-local buffer 'made-spans)
          (and (plusp (length spans)) (cons spans (span-ends spans)))))
  (setf (tuatara.buffer:buffer-local buffer :buttons) buttons
        (tuatara.buffer:buffer-local buffer 'list-key)
        (if key-p key (let ((kind (shown-list-kind buffer))) (and kind (funcall (second kind) buffer)))))
  (tuatara.buffer:forget-history buffer)
  (when refill
    (setf (tuatara.buffer:buffer-local buffer :refill) refill))
  (tuatara.buffer:buffer-mark-saved buffer)
  buffer)

(defun made-spans-between (buffer from to)
  "The parts of BUFFER's made text (SET-MADE-TEXT) shown in a face that reach
between the octets FROM and TO, each (START END FACE), in the order of their
starts; the first found by halves, so O(log n + those found)."
  (let ((made (tuatara.buffer:buffer-local buffer 'made-spans)))
    (when made
      (destructuring-bind (spans . ends) made
        (let ((low (tuatara.runtime:bisect (middle 0 (length spans)) (<= (aref ends middle) from))))
          (loop for i from low below (length spans)
                for span = (svref spans i)
                while (< (first span) to)
                when (> (second span) from) collect span))))))

(defun made-spans-shown (buffer rows)
  "The parts of BUFFER's made text shown in a face within ROWS, the rows laid
out for a window, what a fold or a search filter hides between them left out
(MADE-SPANS-BETWEEN)."
  (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
        nconc (made-spans-between buffer from to)))

(defmethod mode-syntax-highlights ((mode special-mode) buffer rows)
  "Colour the text a command made in BUFFER as it was made, else as its mode
colours text."
  (if (tuatara.buffer:buffer-local buffer 'made-spans)
      (list (made-spans-shown buffer rows))
      (call-next-method)))

(defun piece-parts (piece)
  "PIECE, as PIECES-TEXT takes it, as three values: its text, its face and its action."
  (cond ((stringp piece) (values piece nil nil))
        ((listp (cdr piece)) (values (first piece) (second piece) (third piece)))
        (t (values (car piece) (cdr piece) nil))))

(defun pieces-text (pieces &key keys)
  "PIECES made one text, as three values: the text; the parts of it shown in a
face, each (START END FACE) in octets, in order; and its buttons, each (START
END ACTION). A piece is a string, (TEXT . FACE), or (TEXT FACE ACTION), where
FACE shows TEXT when there is one and ACTION, a function of no arguments, makes
TEXT a button. With KEYS, the keys SUBSTITUTE-COMMAND-KEYS marked in the
pieces' texts are unmarked and shown in :help-key-binding, over the piece's
face. O(size of the text)."
  (let ((out (make-string-output-stream)) (spans '()) (buttons '()) (octets 0))
    (dolist (piece pieces)
      (multiple-value-bind (text face action) (piece-parts piece)
        (multiple-value-bind (plain marked) (if keys (unmark-keys text) (values text nil))
          (let ((end (+ octets (tuatara.text:string-octet-length plain))))
            (write-string plain out)
            (when (and face (< octets end)) (push (list octets end face) spans))
            (let ((octet-at (tuatara.text:make-octet-counter plain octets)))
              (loop for (start stop key-face) in marked
                    do (push (list (funcall octet-at start) (funcall octet-at stop) key-face) spans)))
            (when (and action (< octets end)) (push (list octets end action) buttons))
            (setf octets end)))))
    (values (get-output-stream-string out) (nreverse spans) (nreverse buttons))))

(defun list-entry-at (&optional (buffer (current-buffer)) (position (tuatara.buffer:buffer-point buffer)))
  "The entry RENDER-PIECES was given for the line of BUFFER (the current
buffer when not given) holding POSITION (point when not given), or NIL on a
line that shows none (titles, a summary). A buffer whose text grows as it
comes, a compile's output, finds its entries itself: its :ENTRY-AT local, a
function of the buffer and the position, gives the entry there."
  (let ((finder (tuatara.buffer:buffer-local buffer :entry-at)))
    (if finder
        (funcall finder buffer position)
        (list-line-entry buffer (tuatara.buffer:buffer-line-number buffer position)))))

(defun list-line-entry (buffer line)
  "The entry RENDER-PIECES was given for LINE (from 0) of the list BUFFER, or
NIL for a line that shows none."
  (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries))
        (index (- line (tuatara.buffer:buffer-local buffer 'list-first-line 0))))
    (and (< -1 index (length entries)) (elt entries index))))

(defun list-shown-entries (buffer)
  "The entries the list BUFFER shows, in their order, as a list, those its
lines leave out (NIL) skipped; as a second value, a vector of the line (from
0) each is on."
  (let ((first (tuatara.buffer:buffer-local buffer 'list-first-line 0)) (shown '()) (lines '()))
    (map nil (let ((line first))
               (lambda (entry)
                 (when entry (push entry shown) (push line lines))
                 (incf line)))
         (tuatara.buffer:buffer-local buffer 'list-entries #()))
    (values (nreverse shown) (coerce (nreverse lines) 'vector))))

(defun render-pieces
    (buffer pieces &key (point :keep) (entries nil entries-p) (first-line 0) keys (key nil key-p) refill
                        redraw mark-key)
  "Make PIECES (as PIECES-TEXT joins them, the keys marked in them shown with
KEYS) the text of BUFFER, read-only (SET-MADE-TEXT, with KEY and REFILL), and
put point back. PIECES may be a function of no arguments making them: it is
called, and called again when the text is made again (\\[revert-buffer]), unless
REFILL says otherwise. REDRAW, a function of BUFFER, draws it again from what
it holds, when its marks or folds change, where making it again (REFILL) would
read the disk (REDRAW-LIST). ENTRIES, a list or a vector, are what the lines
from line FIRST-LINE on show, one each, for LIST-ENTRY-AT; MARK-KEY, a
function of an entry, gives what a mark on it is kept by (ENTRY-MARK-KEY), NIL
for an entry that takes none. POINT :keep keeps point on the entry it was on
while that is still listed (an entry made again found by its likeness), else
on its line, at the same column as far as the line goes (point on an entry
gone stays among the entries' lines). :line does the same at the line's start.
:start puts it at the start. :button puts it on the first button (else the
start). :first-entry puts it at the start of the line of the first entry that
is not a section's heading (LIST-HEADING-P), or of line FIRST-LINE when
there is none: where a list just opened is used from. A number puts it at the
start of that line, one of the entries' lines when there are ENTRIES. Each
window showing BUFFER keeps its own point so too, and shows it from the same
line as before. Returns BUFFER."
  (when (functionp pieces)
    (let ((make pieces))
      (setf pieces (funcall make))
      (unless refill (setf refill (lambda (buffer) (render-pieces buffer make :keys keys))))))
  (multiple-value-bind (text spans buttons) (pieces-text pieces :keys keys)
    (apply #'show-list-text buffer text spans buttons :point point :first-line first-line :refill refill
           :redraw redraw :mark-key mark-key
           (append (and entries-p (list :entries entries)) (and key-p (list :key key))))))

(defun show-list-text (buffer text spans buttons &key (point :keep) (entries nil entries-p) (first-line 0)
                                                    (key nil key-p) refill redraw mark-key (capture t))
  "Make TEXT, with its SPANS and BUTTONS as PIECES-TEXT gives them, the text of
BUFFER, as RENDER-PIECES does with the rest of its arguments. A list narrowed by
search filters shows only the entries they leave (FILTER-LIST-TEXT), TEXT kept
as the whole list to narrow again unless CAPTURE is NIL, when it is that whole
list being narrowed again. Returns BUFFER."
  (when entries-p
    (multiple-value-setq (text spans buttons entries first-line)
      (filter-list-text buffer text spans buttons entries first-line capture))
    ;; kept as a vector, so that the entry on a line is found at once
    (setf entries (coerce entries 'simple-vector)))
  (let* ((old-entries (tuatara.buffer:buffer-local buffer 'list-entries :none))
         (old-first (tuatara.buffer:buffer-local buffer 'list-first-line 0))
         (windows (and *editor* (buffer-windows buffer)))
         (own-points (remove-if-not #'tuatara.display:window-point-anchor windows)))
    (labels ((line-of (position)
               (ignoring-errors-reported ("finding the line of ~d in ~a" position (tuatara.buffer:buffer-name buffer))
                 (tuatara.buffer:buffer-line-number buffer position)))
             (place (position)
               ;; where POSITION is: its line, its column, and the index among ENTRIES of its entry
               (let ((line (line-of position)))
                 (list line
                       (if (and line (eq point :keep))
                           (- position (tuatara.buffer:buffer-line-start buffer position))
                           0)
                       (and entries-p (member point '(:keep :line)) line (not (eq old-entries :none))
                            ;; an entry made again is found by its likeness, the one on the same line first
                            (let ((entry (list-entry-at buffer position))
                                  (index (- line old-first)))
                              (and entry
                                   (if (and (< -1 index (length entries)) (equal entry (elt entries index)))
                                       index
                                       (position entry entries :test #'equal))))))))
             (on-entry-p (line)
               (or (eq old-entries :none) (null line)
                   (and (<= old-first line) (< line (+ old-first (length old-entries)))))))
      (let ((point-place (place (tuatara.buffer:buffer-point buffer)))
            (window-places (mapcar (lambda (window) (place (tuatara.display:window-point window))) own-points))
            (start-lines (mapcar (lambda (window) (line-of (tuatara.display:window-start window))) windows)))
        (apply #'set-made-text buffer text :spans spans :buttons buttons :refill refill (and key-p (list :key key)))
        (when entries-p
          (setf (tuatara.buffer:buffer-local buffer 'list-entries) entries
                (tuatara.buffer:buffer-local buffer 'list-first-line) first-line))
        (when mark-key
          (setf (tuatara.buffer:buffer-local buffer 'list-mark-key) mark-key))
        (when redraw
          (setf (tuatara.buffer:buffer-local buffer 'list-redraw) redraw))
        (labels ((among-entries (line) (max first-line (min line (+ first-line (max 0 (1- (length entries)))))))
                 (within-text (line) (min line (max 0 (1- (tuatara.buffer:buffer-line-count buffer)))))
                 (at-column (line column)
                   (let ((start (tuatara.buffer:buffer-goto-line buffer line)))
                     (min (+ start column) (tuatara.buffer:buffer-line-end buffer start))))
                 (put-back (place)
                   (destructuring-bind (line column found) place
                     (cond ((eq point :start) 0)
                           ((eq point :button) (let ((first (first buttons))) (if first (first first) 0)))
                           ((eq point :first-entry)
                            (let ((index (position-if (lambda (entry) (and entry (not (list-heading-p buffer entry))))
                                                      entries)))
                              (tuatara.buffer:buffer-goto-line buffer (within-text (+ first-line (or index 0))))))
                           ((integerp point)
                            (tuatara.buffer:buffer-goto-line buffer
                                                             (if entries-p (among-entries point) (within-text point))))
                           (found (at-column (+ first-line found) column))
                           ((and entries-p (on-entry-p line)) (at-column (among-entries (or line first-line)) column))
                           (t (at-column (within-text (or line 0)) column))))))
          (setf (tuatara.buffer:buffer-point buffer) (put-back point-place))
          (loop for window in own-points
                for place in window-places
                do (setf (tuatara.display:window-point window) (put-back place)))
          (loop for window in windows
                for line in start-lines
                do (setf (tuatara.display:window-start window)
                         (tuatara.buffer:buffer-goto-line buffer (within-text (or line 0))))))))
    buffer))
