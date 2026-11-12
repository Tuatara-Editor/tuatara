;;;; lisp/viewers/inline-pictures.lisp --- Pictures shown among the lines of a buffer's text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A mode says which stretches of its text stand for pictures (an Org link to
;;;; diagram.png alone on its line, a formula), through the buffer's
;;;; :inline-pictures-function; while inline-pictures-mode is on, each is shown
;;;; as its picture on rows of its own, its line as tall as the picture, or
;;;; for a formula inside a line, in the columns it stands in, the text itself
;;;; unchanged. A picture's source says its size and draws its pixels
;;;; (PICTURE-SOURCE-SIZE, PICTURE-SOURCE-IMAGE): a file's size is read from
;;;; its header first, so that the rows are held before its pixels come, which
;;;; are drawn in the background by the program the image view uses, shut in
;;;; the sandbox, only for the pictures in sight, and kept for each size they
;;;; are shown at; a formula is drawn by LaTeX (lisp/export/formulas.lisp).
;;;; Point on it shows the text instead. A terminal that shows no pictures
;;;; shows a line saying what a file's picture is, and a formula as written.

(in-package #:tuatara.defaults)

(define-display-toggle inline-pictures "Inline pictures"
  "Show the pictures the current buffer links to among its lines, and its
formulas as LaTeX sets them, or the text as written. A picture is shown on
rows of its own, as large as its width asks (in Org, :width on its
#+ATTR_ORG: line) or as *INLINE-PICTURE-WIDTH* says, never wider than the
text: the window's, or up to the fill column (\[set-fill-column]) where that
is narrower. It stands to the left, the centre or the right of the text as
its :align says. A formula alone on its lines is centred on rows of its own,
one inside a line drawn in its columns (*FORMULA-PREAMBLE*). Point on one
shows the text again, to be edited. The text is not changed: saving,
searching, copying and undo see it. A terminal that shows no pictures shows
a line naming each linked picture and its size instead, and formulas as
written. Never in a file opened safely."
  :command inline-pictures-mode :lookup inline-pictures-p
  :global *global-inline-pictures-mode*
  :what "show the pictures a buffer links to and its formulas among its lines"
  :example "'((org-mode . t))"
  :unless (tuatara.io:safe-visit-p "in a file opened safely"))

(define-setting *inline-picture-width* t
  "How wide a picture shown among the lines of text (\\[inline-pictures-mode])
is drawn when nothing gives its width (in Org, :width on its #+ATTR_ORG:
line). It is never drawn wider than the window's text, nor past the fill
column.
- t: its own width (the standard).
- a whole number: that many pixels."
  :values (or (member t) (integer 1 *)))

(defparameter *inline-picture-memory* (* 256 1024 1024)
  "The most octets the pictures drawn for showing among lines of text are kept
in; past it, those shown least lately are let go of, and drawn again when
they come back into sight.")

(defstruct (inline-picture (:constructor make-inline-picture (start end source &key width align float))
                           (:copier nil))
  "A picture a buffer shows among its lines: the text from START below END
\(octets) stands for it and is shown as the picture: whole lines, on rows of
their own, or a formula inside a line (FORMULA-INLINE), in its row. SOURCE,
what is drawn: the picture's file, a native path, or a FORMULA. WIDTH, how
wide it is asked to be: NIL as *INLINE-PICTURE-WIDTH* says, T its own width, a
whole number of pixels, or a fraction of the text's width (1/2,
INLINE-PICTURE-ROOM); ALIGN, :left, :center or :right. FLOAT, :left or :right,
has it stand on that side instead, the text from its own line down going
round it (INLINE-PICTURE-FLOATS); NIL, rows of its own."
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (source nil :read-only t)
  (width nil :read-only t)
  (align :left :read-only t)
  (float nil :read-only t))

(setf (documentation 'inline-picture-p 'function) "True when OBJECT is an INLINE-PICTURE."
      (documentation 'inline-picture-start 'function)
      "Where the text standing for the picture begins: a line's start, or a formula's inside a line."
      (documentation 'inline-picture-end 'function)
      "Where the text standing for the picture ends: before its last line's newline, or after a formula."
      (documentation 'inline-picture-source 'function) "What the picture is drawn from: a native path, or a FORMULA."
      (documentation 'inline-picture-width 'function) "How wide the picture is asked to be, or NIL."
      (documentation 'inline-picture-align 'function)
      "Where the picture stands across the window: :left, :center or :right."
      (documentation 'inline-picture-float 'function)
      "The side the picture stands on with the text going round it, :left or :right, or NIL.")

(defstruct (formula (:constructor make-formula (text preamble &key inline)) (:copier nil))
  "A formula shown as a picture: TEXT, the LaTeX of it with its delimiters
\(dollar signs, an equation environment), as a document's body takes it;
PREAMBLE, the LaTeX before \\begin{document} it is drawn with; INLINE, true for
one inside a line of text, drawn in the columns it stands in, as tall as a
row at most. KEY, the key it was last looked up by in the formulas drawn
\(FORMULA-LOOKUP-KEY), kept with the row height and colour it was for."
  (text "" :type string :read-only t)
  (preamble "" :type string :read-only t)
  (inline nil :read-only t)
  (key nil))

(setf (documentation 'formula-p 'function) "True when OBJECT is a FORMULA."
      (documentation 'formula-text 'function) "The LaTeX of the formula, with its delimiters."
      (documentation 'formula-preamble 'function) "The LaTeX before \\begin{document} the formula is drawn with."
      (documentation 'formula-inline 'function) "True for a formula inside a line of text."
      (documentation 'formula-key 'function) "The key the formula was last looked up by, or NIL.")

(define-setting *formula-preamble* (format nil "\\usepackage{amsmath}~%\\usepackage{amssymb}")
  "The LaTeX put before \\begin{document} for every formula shown as a picture
\(\[inline-pictures-mode]), before what the file adds (an Org note's
#+LATEX_HEADER: lines).
- a string of LaTeX lines; amsmath and amssymb as standard."
  :values string)

(defgeneric picture-source-size (buffer source)
  (:documentation "The kind, width and height in pixels of the picture SOURCE draws
for BUFFER, as three values, as it is drawn now: a file's from its header
\(INLINE-PICTURE-SIZE), a formula's once drawn. NIL while it is not known, or
there is none to draw; a formula is then begun in the background, in a job
of BUFFER's."))

(defgeneric picture-source-image (buffer source width height)
  (:documentation "The picture SOURCE draws for BUFFER, WIDTH by HEIGHT pixels as
INLINE-PICTURE-CELLS asks (a formula at its own size, the frontend fitting
it), or NIL while it is drawn in the background or could not be."))

(defgeneric picture-source-words (source)
  (:documentation "The line a frontend that shows no pictures shows for SOURCE's
picture, or NIL for its text as written."))

(defun inline-formula-p (picture)
  "True when PICTURE is a formula inside a line of text (FORMULA-INLINE)."
  (let ((source (inline-picture-source picture)))
    (and (formula-p source) (formula-inline source))))

(defun inline-picture-path-p (path)
  "True when PATH names a picture the image view draws, by its name's ending
\(diagram.png, photo.jpeg)."
  (some (lambda (kind) (assoc kind *image-formats*)) (tuatara.io:name-kinds path)))

;;; Which lines stand for pictures

(defparameter *inline-picture-block* 8192
  "How many octets of a buffer's text its mode is asked at a time which of
their lines stand for pictures; what it says is kept while the text is
unchanged.")

(defun inline-pictures-in-block (buffer block)
  "The pictures BUFFER's mode shows among its lines (its
:inline-pictures-function, a function of the buffer, START and END giving
those whose lines begin from START below END, in order, and true as a second
value when that is the text as it is now) whose lines begin in BLOCK, the
BLOCKth stretch of *INLINE-PICTURE-BLOCK* octets; kept while the text is
unchanged, unless the mode does not know yet (its parse behind the text)."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (kept (tuatara.buffer:buffer-local buffer 'inline-picture-blocks)))
    (unless (and kept (eq (car kept) version))
      (setf kept (setf (tuatara.buffer:buffer-local buffer 'inline-picture-blocks)
                       (cons version (make-hash-table)))))
    (multiple-value-bind (pictures known) (gethash block (cdr kept))
      (if known
          pictures
          (let ((function (tuatara.buffer:buffer-local buffer :inline-pictures-function))
                (start (* block *inline-picture-block*)))
            (multiple-value-bind (pictures current)
                (and function (< start (max 1 (tuatara.buffer:buffer-size buffer)))
                     (funcall function buffer start (+ start *inline-picture-block*)))
              (when current (setf (gethash block (cdr kept)) pictures))
              pictures))))))

(defun inline-pictures-found (buffer start end)
  "The pictures BUFFER's mode shows among its lines whose lines begin from
START below END, in order (INLINE-PICTURES-IN-BLOCK)."
  (loop for block from (floor start *inline-picture-block*) to (floor (max start (1- end)) *inline-picture-block*)
        nconc (loop for picture in (inline-pictures-in-block buffer block)
                    when (< (1- start) (inline-picture-start picture) end)
                      collect picture)))

(defun inline-picture-on-line (buffer line-start)
  "The picture BUFFER shows on rows of its own, or floated, from its line
beginning at LINE-START, or NIL."
  (find-if (lambda (picture) (and (= (inline-picture-start picture) line-start) (not (inline-formula-p picture))))
           (inline-pictures-in-block buffer (floor line-start *inline-picture-block*))))

(defun inline-picture-around (buffer line-start)
  "The picture BUFFER shows on rows of its own whose lines go on past the one
beginning at LINE-START (a formula of several lines), that line among them
but not its first; NIL when there is none."
  (let ((block (floor line-start *inline-picture-block*)))
    (loop for b from block downto (max 0 (1- block))
            thereis (find-if (lambda (picture)
                               (and (< (inline-picture-start picture) line-start (1+ (inline-picture-end picture)))
                                    (not (inline-formula-p picture))))
                             (inline-pictures-in-block buffer b)))))

(defvar *picture-showing-commands* '()
  "Commands after which a picture is shown though point is on its line: those
that change it as you look (\\[org-resize-picture]).")

(defun inline-picture-shown-p (buffer picture)
  "True when PICTURE, BUFFER's, is shown as a picture rather than as its text:
inline-pictures-mode is on there and point is not on its line, unless the
command just run was one of *PICTURE-SHOWING-COMMANDS*."
  (and (inline-pictures-p buffer)
       (or (not (<= (inline-picture-start picture)
                    (tuatara.buffer:buffer-point buffer)
                    (inline-picture-end picture)))
           (and *editor* (member (editor-last-command *editor*) *picture-showing-commands*)))))

;;; The size of a picture

(defvar *inline-picture-sizes* (make-hash-table :test 'equal)
  "For each picture file looked at, (CHECKED STAMP KIND WIDTH HEIGHT): when it
was last looked at (internal real time), its stamp then, and its kind and size
as its header gives them (NIL when it is no picture, or gives none).")

(defun inline-picture-size (path)
  "The kind (TUATARA.IO:OCTETS-KIND's keyword), width and height in pixels of
the picture at PATH, as three values, from its file's first octets; NIL when
there is none, it is no picture the image view draws, or its header gives no
size. Read again when its file changes, looked at no more than once a second."
  (let ((kept (gethash path *inline-picture-sizes*))
        (now (get-internal-real-time)))
    (unless (and kept (< (- now (first kept)) internal-time-units-per-second))
      (let ((stamp (tuatara.io:path-stamp path :failure nil)))
        (if (and kept stamp (equalp stamp (second kept)))
            (setf (first kept) now)
            (setf kept (setf (gethash path *inline-picture-sizes*)
                             (list* now stamp
                                    (let* ((head (and stamp (tuatara.io:file-head path)))
                                           (kind (and head (nth-value 1 (tuatara.io:octets-kind head)))))
                                      (multiple-value-bind (width height)
                                          (and kind (assoc kind *image-formats*) (tuatara.io:octets-picture-size head))
                                        (and width (plusp width) (plusp height) (list kind width height))))))))))
    (values-list (cddr kept))))

(defun inline-picture-asked-width (picture own room)
  "How many pixels wide PICTURE is asked to be, OWN its own width and ROOM the
pixels of text the window has: as its width says, else as
*INLINE-PICTURE-WIDTH* says; a formula, its own width."
  (let ((asked (or (inline-picture-width picture)
                   (if (formula-p (inline-picture-source picture)) t *inline-picture-width*))))
    (cond ((integerp asked) asked)
          ((realp asked) (* asked room))
          (t own))))

(defparameter *inline-picture-float-share* 2/3
  "The most of the text's width (INLINE-PICTURE-ROOM) a picture the text goes
round (INLINE-PICTURE-FLOATS) is drawn as wide as, leaving the rest for the
text.")

(defun inline-picture-room (buffer picture window)
  "How many of WINDOW's columns PICTURE, BUFFER's, may take: those of the
window's text, or up to BUFFER's fill column when that is narrower, less the
prefix before its line. Its width, a share of the text and its :align are
measured in these."
  (max 1 (- (min (tuatara.display:window-text-width window) (buffer-fill-column buffer))
            (tuatara.display:line-prefix-columns buffer (inline-picture-start picture)))))

(defun inline-picture-cells (buffer picture window)
  "Where WINDOW shows PICTURE, BUFFER's, as five values: its width and height
in pixels, the columns and rows it takes and the column of the window's text
it begins at. In a window that draws rows large (*ROW-SCALING*) these are
exact, fractions of a cell; in a terminal, whole cells. It stays inside
INLINE-PICTURE-ROOM. A picture the text goes round stands on its side, no
wider than *INLINE-PICTURE-FLOAT-SHARE* of that and no taller than the
window. A formula inside a line is no taller than a row, at the column its
row shows its start at (0 here). NIL when it is not shown as a picture: the
frontend shows none, its cells' size is not known, or its source gives no
size (PICTURE-SOURCE-SIZE)."
  (let ((frontend (and *editor* (editor-frontend *editor*))))
    (when (and frontend (tuatara.frontend:frontend-images-p frontend))
      (multiple-value-bind (cell-width cell-height) (tuatara.frontend:frontend-cell-pixels frontend)
        (multiple-value-bind (kind own-width own-height) (picture-source-size buffer (inline-picture-source picture))
          (when (and cell-width kind)
            (if (inline-formula-p picture)
                (inline-formula-cells own-width own-height cell-width cell-height)
                (let* ((exact tuatara.display:*row-scaling*)
                       (prefix (tuatara.display:line-prefix-columns buffer (inline-picture-start picture)))
                       (room (inline-picture-room buffer picture window))
                       (float (inline-picture-float picture))
                       (width (max 1 (round (min (* room cell-width (if float *inline-picture-float-share* 1))
                                                 (inline-picture-asked-width picture own-width (* room cell-width))
                                                 ;; one floated no taller than the window
                                                 (if float
                                                     (/ (* own-width cell-height (tuatara.display:window-height window))
                                                        own-height)
                                                     most-positive-fixnum)))))
                       (height (max 1 (round (* width own-height) own-width)))
                       (columns (if exact (/ width cell-width) (ceiling width cell-width)))
                       (rows (max 1 (if exact (/ height cell-height) (ceiling height cell-height))))
                       (spare (max 0 (- room columns)))
                       (left (case (or float (inline-picture-align picture))
                               (:center (if exact (/ spare 2) (floor spare 2)))
                               (:right spare)
                               (t 0))))
                  (values width height columns rows (+ prefix left))))))))))

(defun inline-formula-cells (own-width own-height cell-width cell-height)
  "Where a formula inside a line, OWN-WIDTH by OWN-HEIGHT pixels, is shown in
cells CELL-WIDTH by CELL-HEIGHT pixels, as INLINE-PICTURE-CELLS gives it: no
taller than a row, as much narrower, its columns whole but where rows are
drawn large (*ROW-SCALING*), one row at most, at column 0."
  (let* ((exact tuatara.display:*row-scaling*)
         (scale (min 1 (/ cell-height own-height)))
         (width (max 1 (round (* own-width scale))))
         (height (max 1 (round (* own-height scale)))))
    (values width height (if exact (/ width cell-width) (ceiling width cell-width))
            (if exact (/ height cell-height) 1) 0)))

(defun inline-picture-rows (buffer line-start window)
  "How many rows tall WINDOW draws BUFFER's line beginning at LINE-START when
it shows a picture on rows of its own (INLINE-PICTURE-CELLS), as a line's
height is asked for \(a :line-scale-function), and true as a second value:
the picture begins below one the text goes round. 0 for a picture the text
goes round (INLINE-PICTURE-FLOATS) while point is elsewhere and a line
follows: its line takes no row, the text beside it beginning at its top; 0
too for a line after the first of a picture of several lines (a formula), all
of them shown on the first's rows. NIL when the line shows its text."
  (let ((picture (and (inline-pictures-p buffer) (inline-picture-on-line buffer line-start)))
        (around (and (inline-pictures-p buffer) (inline-picture-around buffer line-start))))
    (when (and around (inline-picture-shown-p buffer around) (inline-picture-cells buffer around window))
      (return-from inline-picture-rows 0))
    (when (and picture (inline-picture-shown-p buffer picture))
      (if (inline-picture-float picture)
          (and (not (<= line-start (tuatara.buffer:buffer-point buffer) (inline-picture-end picture)))
               (< (inline-picture-end picture) (tuatara.buffer:buffer-size buffer))
               (inline-picture-cells buffer picture window)
               0)
          (let ((rows (nth-value 3 (inline-picture-cells buffer picture window))))
            (and rows (values rows t)))))))

;;; Text going round a picture

(defparameter *inline-picture-float-gap* 1
  "How many columns are left blank between a picture the text goes round and
the text beside it.")

(defun inline-picture-floats (buffer line-start window)
  "What the text goes round when WINDOW shows the picture on BUFFER's line
beginning at LINE-START floated left or right, as a layout's float is given
\(a :line-float-function): its side, :left or :right; how many of the
window's columns it keeps from the text on that side, whole ones, the picture
and *INLINE-PICTURE-FLOAT-GAP*; and how many rows' height it takes, from its
own line down. NIL when there is no such picture there, or it is not shown
as one (where pictures cannot be drawn, the line naming it is a line as any
other)."
  (let ((picture (and (inline-pictures-p buffer) (inline-picture-on-line buffer line-start))))
    (when (and picture (inline-picture-float picture) (inline-picture-shown-p buffer picture))
      (multiple-value-bind (width height columns rows left) (inline-picture-cells buffer picture window)
        (declare (ignore height))
        (when width
          (values (inline-picture-float picture)
                  (if (eq (inline-picture-float picture) :left)
                      (+ (ceiling (+ left columns)) *inline-picture-float-gap*)
                      (+ (- (tuatara.display:window-text-width window) (floor left)) *inline-picture-float-gap*))
                  rows))))))

(defun inline-picture-float-before (buffer position window)
  "The start of the nearest line above POSITION in BUFFER with a picture the
text goes round (INLINE-PICTURE-FLOATS) whose rows may reach down to
POSITION: no more lines above it than WINDOW has rows, a floated picture
being no taller than the window, a line whose end a fold hides counting as
one with what the fold hides; NIL when there is none (a
:float-before-function). The text folds hide is not looked through."
  (when (and (inline-pictures-p buffer) (plusp position))
    (let ((reach (visible-line-start-away buffer position (- (1+ (tuatara.display:window-height window)))))
          (size *inline-picture-block*))
      (loop with block = (floor (1- position) size)
            while (>= block (floor reach size))
            do (let* ((from (* block size))
                      (fold (tuatara.display:fold-hiding buffer (min (1- position) (+ from size -1)))))
                 (if (and fold (<= (car fold) from))
                     ;; a stretch a fold hides whole: on from where the fold begins
                     (setf block (min (1- block) (floor (car fold) size)))
                     (let ((found (find-if (lambda (picture)
                                             (let ((start (inline-picture-start picture)))
                                               (and (inline-picture-float picture) (< start position)
                                                    (not (and (plusp start)
                                                              (tuatara.display:fold-hiding buffer (1- start)))))))
                                           (inline-pictures-in-block buffer block) :from-end t)))
                       (when found
                         (return (and (>= (inline-picture-start found) reach)
                                      (inline-picture-start found))))
                       (decf block))))))))

;;; What a line standing for a picture shows instead

(defmethod picture-source-size (buffer (source string))
  "A file's kind and size from its header (INLINE-PICTURE-SIZE)."
  (declare (ignore buffer))
  (inline-picture-size source))

(defmethod picture-source-words ((source string))
  "A file's name and its size, [picture: diagram.png, 800×600]; NIL when its
file gives no size."
  (multiple-value-bind (kind width height) (inline-picture-size source)
    (and kind (format nil "[picture: ~a, ~d×~d]" (escape-controls (tuatara.io:native-file-name source)) width height))))

(defun inline-picture-replacements (buffer start end)
  "What BUFFER shows in place of the lines from START below END that stand for
pictures shown (INLINE-PICTURE-SHOWN-P), as a buffer's display replacements
take them, (START END TEXT FACE): nothing where the frontend shows the
picture, the line held as tall as it; blanks as wide as a formula inside a
line, drawn over them; where the frontend shows none, a line naming a file's
picture (PICTURE-SOURCE-WORDS), a formula as written. A picture whose source
gives no size yet is shown as its text."
  (when (inline-pictures-p buffer)
    (let* ((frontend (and *editor* (editor-frontend *editor*)))
           (images (and frontend (tuatara.frontend:frontend-images-p frontend))))
      (loop for picture in (inline-pictures-found buffer start end)
            for source = (inline-picture-source picture)
            for text = (and (inline-picture-shown-p buffer picture)
                            (cond ((not images) (picture-source-words source))
                                  ((inline-formula-p picture)
                                   (let ((columns (nth-value 2 (inline-picture-cells buffer picture nil))))
                                     (and columns (make-string (ceiling columns) :initial-element #\Space))))
                                  ((picture-source-size buffer source) "")))
            when text
              collect (list (inline-picture-start picture) (inline-picture-end picture) text
                            (if (formula-p source) nil :link))))))

;;; The pictures' pixels, drawn in the background

(defvar *inline-picture-images* (make-hash-table :test 'equal)
  "The pictures drawn to be shown among lines of text, by (PATH STAMP WIDTH
HEIGHT): :PENDING while one is drawn, :FAILED when none could be, else
\(IMAGE . USED), USED the count of *INLINE-PICTURE-USES* it was last shown at.")

(defvar *inline-picture-uses* 0
  "How many times a picture kept in *INLINE-PICTURE-IMAGES* has been asked for:
the order they were last shown in.")

(defstruct (inline-picture-job (:include decoding-job)
                               (:constructor make-inline-picture-job (buffer key path kind width height))
                               (:copier nil))
  "A picture being drawn for BUFFER to show among its lines: the file PATH, of
KIND, drawn WIDTH by HEIGHT pixels, to be kept under KEY in
*INLINE-PICTURE-IMAGES*; IMAGE, once done, the picture or NIL."
  key path kind width height image)

(setf (documentation 'inline-picture-job-p 'function) "True when OBJECT is an INLINE-PICTURE-JOB.")

(tuatara.runtime:define-job-words inline-picture-job
  :doing "Drawing"
  :description (format nil "Drawing ~a" (tuatara.io:native-file-name (inline-picture-job-path job)))
  :verb "draw it"
  :left-words "nothing drawn")

(defmethod tuatara.runtime:job-take-in ((job inline-picture-job))
  "Keep the picture drawn, or that none could be, and let go of those shown
least lately past *INLINE-PICTURE-MEMORY*; the windows show it at the next
frame."
  (let ((image (and (eq (tuatara.runtime:job-status job) :done) (inline-picture-job-image job))))
    (if image
        (setf (gethash (inline-picture-job-key job) *inline-picture-images*) (cons image (incf *inline-picture-uses*)))
        (setf (gethash (inline-picture-job-key job) *inline-picture-images*) :failed))
    (forget-pictures-past *inline-picture-memory*)))

(defun forget-pictures-past (memory &optional (table *inline-picture-images*))
  "Let go of the pictures of TABLE (*INLINE-PICTURE-IMAGES* unless given, each
entry (IMAGE . USED) as kept there) shown least lately until those kept take
no more than MEMORY octets."
  (let ((kept (loop for key being the hash-keys of table using (hash-value entry)
                    when (consp entry)
                      collect (list (cdr entry) key (length (tuatara.frontend:image-pixels (car entry)))))))
    (loop with total = (reduce #'+ kept :key #'third)
          for (nil key octets) in (sort kept #'< :key #'first)
          while (> total memory)
          do (remhash key table)
             (decf total octets))))

(defun inline-picture-image (buffer picture width height)
  "PICTURE, BUFFER's, drawn WIDTH by HEIGHT pixels, or NIL while it is drawn
in the background (begun now when it is not yet) or could not be
\(PICTURE-SOURCE-IMAGE)."
  (picture-source-image buffer (inline-picture-source picture) width height))

(defmethod picture-source-image (buffer (path string) width height)
  "The picture of the file at PATH drawn WIDTH by HEIGHT pixels by the image
view's program, shut in, in a job of BUFFER's; kept by its file's stamp and
size in *INLINE-PICTURE-IMAGES*."
  (let* (;; the stamp kept with its size: the same object while the file is unchanged
         (key (list path (second (gethash path *inline-picture-sizes*)) width height))
         (entry (gethash key *inline-picture-images*)))
    (cond ((consp entry)
           (setf (cdr entry) (incf *inline-picture-uses*))
           (car entry))
          (entry nil)
          (t
           (setf (gethash key *inline-picture-images*) :pending)
           (let ((kind (inline-picture-size path)))
             (tuatara.runtime:start-job-thread
              (make-inline-picture-job buffer key path kind width height)
              (tuatara.runtime:carry-settings
               *decoding-carried-settings*
               (lambda (job)
                 (setf (inline-picture-job-image job)
                       (handler-case
                           (tuatara.io:call-with-file
                            path (lambda (fd name size)
                                   (declare (ignore name size))
                                   (decode-image fd kind width height :enlarge t
                                                                      :cancel (tuatara.runtime:job-cancel job))))
                         (tuatara.io:file-system-error () nil)
                         (storage-condition () nil)))))
              :name "tuatara inline picture"))
           nil))))

(defun inline-pictures-in-rows (window rows)
  "The pictures WINDOW shows among ROWS, those it draws, each (IMAGE ROW
COLUMN COLUMNS ROWS SKIPPED) for row ROW of them, a column of its text: as a
:row-pictures-function gives them. A picture the text goes round has
SKIPPED, the rows of it above ROW not shown (one begun above the window's
start), and is cut at the window's foot rather than made smaller; one on rows
of its own has none; a formula inside a line stands at the column its row
shows its start at. Those not drawn yet are begun in the background
\(INLINE-PICTURE-IMAGE), only those in sight."
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (inline-pictures-p buffer)
      (flet ((place (picture row skipped)
               (multiple-value-bind (width height columns tall left) (inline-picture-cells buffer picture window)
                 (let ((image (and width (inline-picture-image buffer picture width height))))
                   (and image (list (if (inline-picture-float picture)
                                        (list image row left columns (- tall skipped) skipped)
                                        (list image row left columns tall))))))))
        (nconc
         ;; a picture the text goes round, begun above the window's start
         (multiple-value-bind (floating line)
             (tuatara.display:window-float-in-force window (tuatara.display:window-start window))
           (when floating
             (let ((picture (inline-picture-on-line buffer line)))
               (place picture 0 (- (nth-value 3 (inline-picture-cells buffer picture window)) (third floating))))))
         (loop for r from 0 below (min (length rows) (tuatara.display:window-rows-drawn window))
               for row = (aref rows r)
               for picture = (and (not (tuatara.display:row-continuation-p row))
                                  (inline-picture-on-line buffer (tuatara.display:row-start row)))
               for floated = (and (tuatara.display:row-float-start row)
                                  (inline-picture-on-line buffer (tuatara.display:row-float-start row)))
               when (and picture (inline-picture-shown-p buffer picture))
                 nconc (place picture r 0)
               ;; one begun beside this row, its own line taking none
               when floated
                 nconc (place floated r 0)
               ;; formulas inside the row's text, where it shows them
               nconc (loop for formula in (inline-pictures-found buffer (tuatara.display:row-start row)
                                                                 (tuatara.display:row-end row))
                           for column = (and (inline-formula-p formula) (inline-picture-shown-p buffer formula)
                                             (tuatara.display:row-column-of row (inline-picture-start formula)))
                           when column
                             nconc (let ((placed (place formula r 0)))
                                     (when placed (setf (third (first placed)) column))
                                     placed))))))))

(defun show-inline-pictures (buffer function)
  "Have BUFFER show among its lines the pictures FUNCTION finds, while
\\[inline-pictures-mode] is on there: FUNCTION, of BUFFER, START and END,
gives the INLINE-PICTUREs whose lines begin from START below END, in order,
and true as a second value when that is of the text as it is now. BUFFER's
:line-scale-function asks INLINE-PICTURE-ROWS how tall such a line is, asked
in a terminal too while the pictures are shown (:tall-lines-function), and
its display replacements take INLINE-PICTURE-REPLACEMENTS in. The rows
beside a picture floated left or right are narrower, the text going round it
\(:line-float-function and :float-before-function). A buffer whose mode gives
it no :line-scale-function or display replacements is given
INLINE-PICTURE-ROWS and INLINE-PICTURE-REPLACEMENTS as them
\(FORGET-INLINE-PICTURES takes them away again)."
  (setf (tuatara.buffer:buffer-local buffer :inline-pictures-function) function
        (tuatara.buffer:buffer-local buffer :tall-lines-function) 'inline-pictures-p
        (tuatara.buffer:buffer-local buffer :row-pictures-function) 'inline-pictures-in-rows
        (tuatara.buffer:buffer-local buffer :line-float-function) 'inline-picture-floats
        (tuatara.buffer:buffer-local buffer :float-before-function) 'inline-picture-float-before)
  (unless (tuatara.buffer:buffer-local buffer :line-scale-function)
    (setf (tuatara.buffer:buffer-local buffer :line-scale-function) 'inline-picture-rows))
  (unless (tuatara.display:buffer-replacements-function buffer)
    (setf (tuatara.display:buffer-replacements-function buffer) 'inline-picture-replacements)))

(defun forget-inline-pictures (buffer)
  "Have BUFFER show no pictures among its lines any more: what
SHOW-INLINE-PICTURES gave it taken away, as its mode is left."
  (dolist (local '(:inline-pictures-function :tall-lines-function :row-pictures-function :line-float-function
                   :float-before-function inline-picture-blocks))
    (setf (tuatara.buffer:buffer-local buffer local) nil))
  (when (eq (tuatara.buffer:buffer-local buffer :line-scale-function) 'inline-picture-rows)
    (setf (tuatara.buffer:buffer-local buffer :line-scale-function) nil))
  (when (eq (tuatara.display:buffer-replacements-function buffer) 'inline-picture-replacements)
    (setf (tuatara.display:buffer-replacements-function buffer) nil)))
