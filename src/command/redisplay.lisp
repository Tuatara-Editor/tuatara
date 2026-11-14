;;;; src/command/redisplay.lisp --- Painting a frame: the windows, their mode lines, the panel and the cursor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(define-setting *cursor-shape* :block
  "The cursor's shape, in the window and the terminal alike.
The modal state can have its own (*TUATARA-INSERT-CURSOR-SHAPE* and the
rest).
- :block: a block over the character (the standard).
- :bar: a line before the character.
- :underline: a line under the character."
  :values (member :block :bar :underline))

(define-setting *cursor-colour* nil
  "The cursor's colour.
The modal states can have their own (*TUATARA-INSERT-CURSOR-COLOUR* and the
rest).
- nil: the colour of the text, the character under a block then in the colour
  of the ground (the standard).
- a name such as :red: that colour, as faces take colours.
- a number from 0 to 255: that colour of the 256-colour palette.
- \"#rrggbb\": that colour, as red, green and blue in two hexadecimal digits
  each."
  :values (or (member nil) (integer 0 255) string "a colour name such as :red"))

(defvar *cursor-colour-functions* '()
  "Functions of the editor that may colour the cursor for the moment, the
first to give a colour (as faces take colours) winning over the others: the
current match of a search, the cursor on it, in the match's colour.")

(defun cursor-look (editor)
  "The cursor's shape and colour, and whether text is being typed, as three
values: from EDITOR's cursor function (EDITOR-CURSOR-SHAPE-FUNCTION) where it
says, else *CURSOR-SHAPE* and *CURSOR-COLOUR*; a colour of
*CURSOR-COLOUR-FUNCTIONS* before either."
  (multiple-value-bind (shape colour typing)
      (and (editor-cursor-shape-function editor) (funcall (editor-cursor-shape-function editor) editor))
    (values (or shape *cursor-shape* :block)
            (or (tuatara.runtime:run-hook-until-success *cursor-colour-functions* editor) colour *cursor-colour*)
            typing)))

(defvar *gutter-providers* '()
  "Gutter providers, each a plist (:name NAME :width WIDTH-FN :cell CELL-FN),
with :prepare PREPARE-FN too when it has one.
WIDTH-FN is called with (EDITOR WINDOW) and returns the columns the provider
needs (0 to stay hidden). PREPARE-FN is called once a frame, with (EDITOR
WINDOW ROWS), the rows shown, and returns what its cells are worked out from.
CELL-FN is called with (EDITOR WINDOW ROW LINE), and that as a fifth argument
when it has a PREPARE-FN. LINE is the 0-based line number of ROW's start, or
NIL for a continuation row. It returns two values, the text for that row
\(padded to the width) and its face.
A function that fails is said once (TUATARA.RUNTIME:CALL-HOOK-FUNCTION). Its
provider shows nothing, the rest of the gutter and the screen drawn.")

(defun add-gutter-provider (name width cell &key prepare first)
  "Put the gutter provider NAME, with its functions WIDTH, CELL and PREPARE
as *GUTTER-PROVIDERS* describes them, in the gutter in place of one of that
name: after the others, or with FIRST before them."
  (let ((provider (list* :name name :width width :cell cell (and prepare (list :prepare prepare))))
        (others (remove name *gutter-providers* :key (lambda (p) (getf p :name)) :test #'equal)))
    (setf *gutter-providers* (if first (cons provider others) (append others (list provider))))))

(defun gutter-width (editor window provider)
  "The columns PROVIDER, one of *GUTTER-PROVIDERS*, wants in WINDOW: 0 when
its width function fails."
  (or (tuatara.runtime:call-hook-function (getf provider :width) editor window) 0))

(defun gutter-width-except (editor window &optional name)
  "The columns the gutter providers want in WINDOW together (GUTTER-WIDTH),
the one called NAME left out when it is given."
  (loop for p in *gutter-providers*
        unless (and name (equal (getf p :name) name))
          sum (gutter-width editor window p)))

(defun window-margin-for (editor window)
  "The margin WINDOW needs: the widths of the gutter providers plus one
separating column when any is visible; none in the hex view, whose offsets
stand where line numbers would."
  (if (tuatara.display:hex-view-p (tuatara.display:window-buffer window))
      0
      (let ((total (gutter-width-except editor window)))
        (if (plusp total) (1+ total) 0))))

(defparameter *gutter-count-reach* 65536
  "The most octets between one row's start and the next that the gutter counts
the next row's line number on across, rather than looking it up afresh: a
fold hiding more is looked past.")

(defun gutter-cells (editor window rows)
  "For each row of ROWS, a list of (STRING . FACE) gutter pieces, in provider order."
  (when *gutter-providers*
    (let* ((buffer (tuatara.display:window-buffer window))
           (widths (mapcar (lambda (p) (gutter-width editor window p)) *gutter-providers*))
           (prepared (loop for p in *gutter-providers*
                           for width in widths
                           collect (and (plusp width) (getf p :prepare)
                                       (tuatara.runtime:call-hook-function (getf p :prepare) editor window rows))))
           ;; not waited for while the lines are counted: known where the count has passed
           (line (and (plusp (length rows))
                      (tuatara.io:buffer-known-line-number buffer (tuatara.display:row-start (aref rows 0))))))
      (loop for row across rows
            for index from 0
            collect (let ((row-line (if (tuatara.display:row-continuation-p row) nil line)))
                      (prog1 (loop for p in *gutter-providers*
                                   for width in widths
                                   for ready in prepared
                                   when (plusp width)
                                     collect (multiple-value-bind (text face)
                                                 (let ((cell (getf p :cell)))
                                                   (if (getf p :prepare)
                                                       (tuatara.runtime:call-hook-function
                                                        cell editor window row row-line ready)
                                                       (tuatara.runtime:call-hook-function
                                                        cell editor window row row-line)))
                                               ;; a cell that failed gives its error in the face's place
                                               (when (typep face 'error) (setf face nil))
                                               (cons (let ((s (or text "")))
                                                       (if (< (length s) width)
                                                           (concatenate 'string (make-string (- width (length s))
                                                                                             :initial-element
                                                                                             #\Space) s)
                                                           (subseq s 0 width)))
                                                     face)))
                        ;; the next line begins where the next row does not go on from this one:
                        ;; its own number where it is known, as lines hidden by a fold are skipped,
                        ;; counted on from this row when it is near
                        (when (and line (< (1+ index) (length rows))
                                   (not (tuatara.display:row-continuation-p (aref rows (1+ index)))))
                          (let ((from (tuatara.display:row-start row))
                                (to (tuatara.display:row-start (aref rows (1+ index)))))
                            (setf line (if (and (tuatara.buffer:buffer-indexed-p buffer)
                                                (<= 0 (- to from) *gutter-count-reach*))
                                           (+ line (tuatara.text:rope-count-newlines (tuatara.buffer:buffer-text buffer)
                                                                                     from to))
                                           (or (tuatara.io:buffer-known-line-number buffer to)
                                               (1+ line))))))))))))

(defvar *syntax-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning syntax colours for the text in
ROWS, as lists of (START END FACE) byte ranges in a list, each painted over
the one before: the major mode's colours, a language server's. Painted first,
so everything else shows over them.")

(defvar *job-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS that work in the background is changing (a
replacement, a sort): painted over the syntax colours and under everything
else, so that search matches, selections and the region show over them.")

(defvar *line-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS painted under all others, the syntax colours
included: the line point is on (hl-line-mode).")

(defvar *text-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS SYNTAX) returning lists of (START END
FACE) byte ranges for the text in ROWS, painted over the syntax colours,
SYNTAX, the lists of ranges those are painted in (WINDOW-SYNTAX): what tells
apart the kinds of text, a comment from code, is taken from them (the words
spelt wrong). SYNTAX is lent for the frame: a function keeping it for later
keeps a copy.")

(defvar *highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS, painted over the syntax colours: search matches
and the like. The region is painted last.")

(defvar *view-change-functions* '()
  "Functions of (EDITOR WINDOW ROWS) called as WINDOW is drawn with ROWS when
the stretch of its buffer it shows is not the one it showed when last drawn,
or its buffer is another: to ask for what the lines now in sight need, as a
language server's inlay hints. What they return is not used.")

(defvar *window-views* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "For each window drawn, what it showed when last drawn: a vector of its
buffer and the first and last octet of its rows.")

(defun note-window-view (editor window rows)
  "Call the functions of *VIEW-CHANGE-FUNCTIONS* when WINDOW of EDITOR, drawn
with ROWS, shows another stretch of text than when it was last drawn."
  (when (and *view-change-functions* (plusp (length rows)))
    (let ((buffer (tuatara.display:window-buffer window))
          (start (tuatara.display:row-start (aref rows 0)))
          (end (tuatara.display:row-end (aref rows (1- (length rows)))))
          (view (or (gethash window *window-views*)
                    (setf (gethash window *window-views*) (vector nil 0 0)))))
      (unless (and (eq (svref view 0) buffer) (eql (svref view 1) start) (eql (svref view 2) end))
        (setf (svref view 0) buffer (svref view 1) start (svref view 2) end)
        (dolist (f *view-change-functions*)
          (tuatara.runtime:call-hook-function f editor window rows))))))

(defun look-at-views-again (buffer)
  "Have the functions of *VIEW-CHANGE-FUNCTIONS* called for every window
showing BUFFER when it is next drawn, as when what it shows has changed: for
what those functions ask for, which could not be had before, as when a
language server has just begun."
  (maphash (lambda (window view)
             (declare (ignore window))
             (when (eq (svref view 0) buffer) (setf (svref view 0) nil)))
           *window-views*))

(defun rectangle-live-p (buffer)
  "True while BUFFER's selection is a rectangle: the block of columns between
point and the mark on each line from one to the other."
  (and (tuatara.buffer:buffer-local buffer :rectangle) (tuatara.buffer:buffer-mark-active-p buffer)))

(defun end-rectangle (buffer)
  "Make BUFFER's selection, if it has one, plain again."
  (setf (tuatara.buffer:buffer-local buffer :rectangle) nil
        (tuatara.buffer:buffer-local buffer :rectangle-point) nil))

(defun rectangle-point-column (buffer)
  "The column on the screen of the corner of BUFFER's rectangle at point:
point's own, or one further right when a motion took the corner past the end
of a short line. With no rectangle, point's column on the screen."
  (let* ((p (tuatara.buffer:buffer-point buffer))
         (column (tuatara.display:buffer-screen-column buffer p))
         (kept (tuatara.buffer:buffer-local buffer :rectangle-point)))
    (if (and kept (rectangle-live-p buffer) (= (car kept) p) (> (cdr kept) column))
        (cdr kept)
        column)))

(defun keep-rectangle-column (buffer column)
  "While BUFFER's selection is a rectangle, stand its corner at point in
COLUMN, past the end of point's line when that is shorter."
  (when (rectangle-live-p buffer)
    (setf (tuatara.buffer:buffer-local buffer :rectangle-point)
          (cons (tuatara.buffer:buffer-point buffer) column))))

(defun rectangle-rows (buffer point mark &key (from 0) (to most-positive-fixnum)
                                             (point-column (tuatara.display:buffer-screen-column buffer point)))
  "The rows of the rectangle with corners POINT and MARK in BUFFER, top
first, each (START END START-COLUMN END-COLUMN). START and END are the byte
span of its columns on the screen on that line, cut short where the line is, a
tab across an edge in it. START-COLUMN and END-COLUMN are the columns that span
starts and ends at (outside the rectangle's where a tab crosses its edge,
inside where a wide character does). Only the rows of lines between FROM and
TO. POINT-COLUMN is the column of the corner at POINT, when that is past the
end of its line. The text is read once (TUATARA.TEXT:MAP-LINE-SLICES)."
  (let ((low (min point mark)) (high (max point mark)))
    (when (and (<= from high) (>= to low))
      (let* ((c1 point-column)
             (c2 (tuatara.display:buffer-screen-column buffer mark))
             (left (min c1 c2)) (right (max c1 c2))
             (top (tuatara.buffer:buffer-line-start buffer (max low from)))
             (bottom (tuatara.buffer:buffer-line-start buffer (min high to)))
             (rows '()))
        (apply #'tuatara.text:map-line-slices (lambda (line start end start-column end-column line-end)
                                                (declare (ignore line line-end))
                                                (push (list start end start-column end-column) rows))
               (tuatara.buffer:buffer-text buffer) top (1+ bottom) left right
               (tuatara.display:buffer-slice-keys buffer))
        (nreverse rows)))))

(defun rectangle-columns (buffer)
  "Two values: the left and right columns on the screen of BUFFER's rectangle."
  (let ((a (rectangle-point-column buffer))
        (b (tuatara.display:buffer-screen-column buffer (tuatara.buffer:buffer-mark buffer))))
    (values (min a b) (max a b))))

(defun region-rectangle-rows (buffer &key (from 0) (to most-positive-fixnum))
  "The rows of BUFFER's rectangle, as RECTANGLE-ROWS gives them."
  (rectangle-rows buffer (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-mark buffer)
                  :from from :to to :point-column (rectangle-point-column buffer)))

(defun window-syntax (editor window rows)
  "The ranges the text in ROWS of WINDOW is coloured in by its syntax, as lists
of (START END FACE) painted in turn: those of each of
*SYNTAX-HIGHLIGHT-FUNCTIONS*, one that fails left out
\(TUATARA.RUNTIME:CALL-HOOK-FUNCTION says so once). The lists may be the ones
a function keeps for the next frame, so they are read, never changed."
  (loop for f in *syntax-highlight-functions*
        append (tuatara.runtime:call-hook-function f editor window rows)))

(defun window-highlights-for (editor window rows)
  "The highlight ranges for the text in ROWS of WINDOW, as the lists of them
TUATARA.DISPLAY:APPLY-HIGHLIGHTS paints, lowest priority first:
*LINE-HIGHLIGHT-FUNCTIONS*, syntax, *TEXT-HIGHLIGHT-FUNCTIONS*, then
*JOB-HIGHLIGHT-FUNCTIONS*, then *HIGHLIGHT-FUNCTIONS*, then the active
region, which only the selected window shows. A way of marking text that
fails is left out and said once (TUATARA.RUNTIME:CALL-HOOK-FUNCTION), so
that the rest of the screen is drawn. The lists may be the ones the
functions keep for the next frame, so they are read, never changed."
  (let* ((buffer (tuatara.display:window-buffer window))
         (layers (list nil))
         (tail layers))
    (flet ((add (ranges)
             (when ranges
               (setf (cdr tail) (list ranges)
                     tail (cdr tail)))))
      ;; a mode's colours, and the line point is on, are for text, not for the hex view's digits
      (unless (tuatara.display:hex-view-p buffer)
        (dolist (f *line-highlight-functions*)
          (add (tuatara.runtime:call-hook-function f editor window rows)))
        (let ((syntax (window-syntax editor window rows)))
          (mapc #'add syntax)
          (dolist (f *text-highlight-functions*)
            (add (tuatara.runtime:call-hook-function f editor window rows syntax)))))
      ;; the hex view's octet at its cursor, in its hex digits and its character both
      (when (tuatara.display:hex-view-p buffer)
        (let ((p (tuatara.display:hex-point buffer)))
          (when (< p (tuatara.buffer:buffer-size buffer))
            (add (list (list p (1+ p) :hex-mirror))))))
      (note-window-view editor window rows)
      (dolist (f *job-highlight-functions*)
        (add (tuatara.runtime:call-hook-function f editor window rows)))
      (dolist (f *highlight-functions*)
        (add (tuatara.runtime:call-hook-function f editor window rows)))
      (when (and (eq window (editor-window editor)) (tuatara.buffer:buffer-mark-active-p buffer))
        (if (and (tuatara.buffer:buffer-local buffer :rectangle) (plusp (length rows)))
            ;; a rectangle: its columns on each line shown, and only those
            (add (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
                       ;; the line a range ends at the start of is not in it
                       nconc (loop for (start end) in (region-rectangle-rows buffer :from from :to (max from (1- to)))
                                   when (< start end)
                                     collect (list start end :region))))
            (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
              (when (and start (< start end))
                (add (list (list start end :region)))))))
      (rest layers))))

(defvar *mode-line-functions* '()
  "Functions of (EDITOR BUFFER) returning what to add to the mode line: a string,
a list (STRING FACE) for text shown in a face of its own, or NIL.")

(defvar *mode-line-lead-functions* '()
  "Functions of (EDITOR BUFFER) returning what goes first in the mode line,
before [-], [*] or [%], as *MODE-LINE-FUNCTIONS* return their parts, or a list
of such parts written together with no space between them: signs that keep one
place whatever else the mode line shows (the signs of directories kept read).")

(defvar *mode-line-name-functions* '()
  "Functions of (EDITOR BUFFER) returning what goes just before the buffer's
name in the mode line, after [-], [*] or [%], as *MODE-LINE-FUNCTIONS* return
their parts: what is said of the buffer itself (Safe, for a file opened
safely).")

(defun modified-indicator (buffer)
  "BUFFER's state as the mode line shows it: [%] when it is read-only, else
[*] when it has changes not saved, else [-]."
  (cond ((tuatara.buffer:buffer-read-only-p buffer) "[%]")
        ((tuatara.buffer:buffer-modified-p buffer) "[*]")
        (t "[-]")))

(defvar *shown-default-directory* nil
  "The directory *DEFAULT-DIRECTORY* named when the mode line last looked, as
\(SETTING HOME TIME NAME): the setting and the HOME variable then, the
internal real time, and the name as the mode line shows it.")

(defun shown-default-directory ()
  "The directory *DEFAULT-DIRECTORY* names, as the mode line shows it
\(ABBREVIATED-PATH): looked up again when the setting or the home directory
changes, else at most once a second, so that drawing makes no call to the
file system."
  (let ((kept *shown-default-directory*)
        (home (tuatara.io:own-variable "HOME"))
        (now (get-internal-real-time)))
    (if (and kept (eq (first kept) tuatara-user:*default-directory*) (equal (second kept) home)
             (< (- now (third kept)) internal-time-units-per-second))
        (fourth kept)
        (fourth (setf *shown-default-directory*
                      (list tuatara-user:*default-directory* home now
                            (abbreviated-path (tuatara.io:native (tuatara.io:default-directory)))))))))

(defun buffer-directory-name (buffer)
  "The directory BUFFER stands in (BUFFER-DIRECTORY) as the mode line shows
it: the home directory written ~, a / at the end."
  (if (or (buffer-given-directory buffer) (tuatara.buffer:buffer-file buffer))
      (abbreviated-path (buffer-directory buffer))
      (shown-default-directory)))

(define-setting *mode-line-name-limit* nil
  "The most columns a buffer's name takes in the mode line.
A longer name keeps its start and its end, … standing for its middle.
Whatever this says, a name is cut so that the mode line shows whole in its
window, all but the directory at its end, down to 12 columns.
- nil: as many as it has (the standard).
- a whole number, 1 or more: that many columns."
  :values (or (member nil) (integer 1 *)))

(defun mode-line-name (name room)
  "NAME as the mode line shows it with ROOM columns for it: whole when it fits
and is no longer than *MODE-LINE-NAME-LIMIT*; else cut in its middle to the
smaller of the two, … standing for what is cut, but to no fewer than 12
columns."
  (let ((limit (max 12 (min room (or *mode-line-name-limit* room)))))
    (if (<= (panel-columns name) limit)
        name
        (values (fit-panel-columns name '() limit :middle t)))))

(defun buffer-mode-name (buffer)
  "The name of BUFFER's major mode as its mode line shows it (Org, Terminal),
or NIL before it has one."
  (tuatara.buffer:buffer-local buffer :mode-name))

(defun (setf buffer-mode-name) (name buffer)
  "Have BUFFER's mode line call its major mode NAME (BUFFER-MODE-NAME)."
  (setf (tuatara.buffer:buffer-local buffer :mode-name) name))

(defvar *mode-lines-kept* (make-hash-table :test 'eq :weakness :key)
  "Each window's mode line as DEFAULT-MODE-LINE last made it, (INPUTS TEXT
SPANS): made again only when what it is made of, INPUTS, differs.")

(defun default-mode-line (editor &optional (window (editor-window editor)))
  "The mode line of WINDOW, the selected window by default.
It shows, in order, [-], [*] or [%] as the buffer is saved, changed or
read-only (MODIFIED-INDICATOR). Then comes the buffer's name (cut in its
middle when long, MODE-LINE-NAME). Then, while frames take turns on one
display, the name of the frame (F2, FRAME-TAKING-TURNS-P). Then come the line
and column of the window's point, digits grouped with commas (L100,000,001) so
large numbers read at a glance. Then comes the major mode's name when the
buffer has one (BUFFER-MODE-NAME), then what *MODE-LINE-FUNCTIONS* add, and
last the buffer's directory in the :mode-line-directory face.
What *MODE-LINE-LEAD-FUNCTIONS* give goes first, before [-], and what
*MODE-LINE-NAME-FUNCTIONS* give just before the name.
Returns the text and, as a second value, the spans of it in faces of their
own, each (START END FACE). It is made again only when one of those parts
differs from the last time for WINDOW."
  (flet ((call-all (functions buffer)
           (loop for f in functions collect (tuatara.runtime:call-hook-function f editor buffer))))
    (let* ((buffer (tuatara.display:window-buffer window))
           (point (tuatara.display:window-point window))
           (frame (editor-frame editor))
           (file (tuatara.buffer:buffer-file buffer))
           (given (buffer-given-directory buffer))
           (inputs (list buffer (tuatara.display:window-width window)
                         (call-all *mode-line-lead-functions* buffer) (call-all *mode-line-name-functions* buffer)
                         (call-all *mode-line-functions* buffer)
                         (modified-indicator buffer) (and (frame-taking-turns-p editor frame) (frame-name frame))
                         ;; not waited for: while a large file's lines are counted, the line where the count
                         ;; has passed it, else L…
                         (let ((line (tuatara.io:buffer-known-line-number buffer point)))
                           (and line (1+ line)))
                         ;; on a line too long to read back over while its lines are counted, C…
                         (tuatara.buffer:buffer-column buffer point :most tuatara.display:*screen-column-reach*)
                         (buffer-mode-name buffer) (tuatara.buffer:buffer-name buffer) file given
                         (and (not file) (not given) (shown-default-directory))))
           (kept (gethash window *mode-lines-kept*)))
      (if (and kept (equal inputs (first kept)))
          (values (second kept) (third kept))
          (multiple-value-bind (text spans) (apply #'mode-line-text window (cddr inputs))
            (setf (gethash window *mode-lines-kept*) (list inputs text spans))
            (values text spans))))))

(defun mode-line-text (window leads names extras indicator frame-name line column mode-name buffer-name file given
                       default-directory)
  "The text of WINDOW's mode line and its spans, as DEFAULT-MODE-LINE makes
them from its parts: what the lead, name and other mode line functions gave
\(LEADS, NAMES and EXTRAS), the buffer's state INDICATOR, the FRAME-NAME
shown or NIL, the LINE and COLUMN of point or NIL, the MODE-NAME, the
BUFFER-NAME, and its directory from its FILE, the directory it was GIVEN, or
the DEFAULT-DIRECTORY shown."
  (let* ((spans '())
         (before (with-output-to-string (out)
                   (flet ((parts (results)
                            (loop for lead in results
                                  ;; one part, or several written together
                                  for pieces = (cond ((stringp lead) (list (list lead nil)))
                                                     ((and (consp lead) (consp (first lead))) lead)
                                                     ((consp lead) (list lead)))
                                  when pieces
                                    do (loop for (part face) in pieces
                                             do (when face
                                                  (push (list (file-position out)
                                                              (+ (file-position out) (length part))
                                                              face) spans))
                                                (write-string part out))
                                       (write-char #\Space out))))
                     (write-char #\Space out)
                     (parts leads)
                     (format out "~a " indicator)
                     (when frame-name (format out "~a " frame-name))
                     (parts names))))
         (after (format nil "  L~:[…~;~:*~:d~] C~:[…~;~:*~:d~]~@[  ~a~]" line column mode-name))
         (extras (loop for extra in extras
                       when (stringp extra) collect (list extra nil)
                       when (consp extra) collect (list (first extra) (second extra))))
         (directory (cond (given (abbreviated-path given))
                          (file (abbreviated-path (tuatara.io:parent-path file)))
                          (t default-directory)))
         (name (mode-line-name (escape-controls buffer-name)
                               (- (tuatara.display:window-width window) 1
                                  (panel-columns before) (panel-columns after)
                                  (loop for (part) in extras sum (+ 2 (panel-columns part))))))
         (text (with-output-to-string (out)
                 (write-string before out)
                 (write-string name out)
                 (write-string after out)
                 (loop for (part face) in (append extras (list (list directory :mode-line-directory)))
                       do (write-string "  " out)
                          (when face
                            (push (list (file-position out) (+ (file-position out) (length part)) face) spans))
                          (write-string part out))
                 (write-char #\Space out))))
    (values text (nreverse spans))))

(defparameter *window-divider* "│"
  "The character drawn down the line between windows side by side, in the
:vertical-border face.")

(defun put-window-divider (window column screen row)
  "Put the line between WINDOW and the window beside it, when it has one
\(WINDOW-DIVIDER), at COLUMN of ROW of SCREEN, as WINDOW-PIECES puts its
pieces."
  (when (tuatara.display:window-divider window)
    (push (list column 1 *window-divider* '((0 1 :vertical-border)) nil) (aref screen row))))

(defun fit-to-columns (string columns)
  "STRING cut or padded with spaces to fill COLUMNS columns of the screen, a
wide character counting two. Two values: that string, and how many characters
of STRING it keeps. Each character is looked at once; one of printable ASCII
alone takes a column each."
  (declare (type fixnum columns))
  (let ((n (length string)))
    (multiple-value-bind (kept used)
        (if (every (lambda (c) (char<= #\Space c #\~)) string)
            (let ((k (min n columns))) (values k k))
            (tuatara.display:chars-in-columns string columns))
      (declare (type fixnum kept used))
      (let ((cut (if (= kept n) string (subseq string 0 kept))))
        (values (if (< used columns)
                    (concatenate 'string cut (tuatara.text:spaces (- columns used)))
                    cut)
                kept)))))

(defun row-parts (pieces)
  "The columns where a part of a screen row made of PIECES begins after the
first (see PUT-SCREEN-ROW): each piece but one from column 0, and a window's
text after its gutter, in order."
  (sort (remove-duplicates
         (loop for (left nil nil nil nil nil text-from) in pieces
               when (plusp left) collect left
               when (and text-from (plusp text-from)) collect (+ left text-from)))
        #'<))

(defun put-screen-row (frontend row pieces width &optional boxes)
  "Paint screen ROW, WIDTH columns, from PIECES, each (LEFT COLUMNS STRING
SPANS FACE DRAW TEXT-FROM). STRING is in the COLUMNS columns from LEFT, SPANS
over its characters. FACE is under the whole piece (a mode line) or NIL. DRAW
is how it is drawn otherwise (PIECE-SCALES). TEXT-FROM is the column of the
piece where a window's text begins after its gutter. The parts of BOXES
\(POPUP-BOXES) on the row are laid over them. The frontend is told where the
row's parts begin (ROW-PARTS), so that it may keep each painted apart. A raw
byte's character is written \\xNN (ESCAPE-CONTROLS). On a row with parts drawn
large or moved down, the boxes' parts are given apart, to be drawn over all
\(OVER), not laid into the text."
  (multiple-value-bind (string spans face scales) (screen-row-text pieces width)
    (multiple-value-bind (string spans) (escape-controls string spans :raw-bytes)
      (if scales
          (tuatara.frontend:frontend-put-row frontend row string :face face :spans spans :scales scales
                                                                 :over (boxes-on-row row boxes))
          (multiple-value-bind (string spans face) (lay-boxes-over string spans face row boxes width)
            (tuatara.frontend:frontend-put-row frontend row string :face face :spans spans
                                                                   :parts (row-parts pieces)))))))

(defun screen-row-text (pieces width)
  "The text of a screen row WIDTH columns wide made of PIECES (see
PUT-SCREEN-ROW), as four values: the string, its spans, the face under the
whole row and the parts drawn otherwise (PIECE-SCALES). A piece alone across
the row is taken as it is, FACE the row's."
  (if (and (null (rest pieces)) (zerop (first (first pieces))) (= width (second (first pieces))))
      (destructuring-bind (left columns string spans face &optional draw text-from) (first pieces)
        (declare (ignore text-from))
        (values string spans face (piece-scales left columns draw)))
      (let ((text '()) (spans '()) (at 0) (column 0) (scales '()))
        (dolist (piece (sort (copy-list pieces) #'< :key #'first))
          (destructuring-bind (left columns string piece-spans face &optional draw text-from) piece
            (declare (ignore text-from))
            (setf scales (append scales (piece-scales left columns draw)))
            (when (> left column)
              (push (tuatara.text:spaces (- left column)) text)
              (incf at (- left column)))
            (multiple-value-bind (fitted kept) (fit-to-columns string columns)
              (let ((inner (loop for (start end span-face) in piece-spans
                                 when (and span-face (< start kept))
                                   collect (list start (min end kept) span-face))))
                (dolist (span (if face (flatten-spans (cons (list 0 (length fitted) face) inner)
                                                      (length fitted)) inner))
                  (push (list (+ at (first span)) (+ at (second span)) (third span)) spans)))
              (push fitted text)
              (incf at (length fitted))
              (setf column (+ left columns)))))
        (values (apply #'concatenate 'string (nreverse text)) (nreverse spans) nil scales))))

(defun piece-scales (left columns draw)
  "The parts of a piece from column LEFT, COLUMNS wide, drawn otherwise than
in its own cells, as the frontend takes them (TUATARA.FRONTEND:FRONTEND-PUT-ROW's
SCALES), from DRAW, (OFFSET SCALE SHIFT). They start OFFSET columns in, SCALE
times as large, the whole piece SHIFT rows lower, its gutter before OFFSET as
it is but as tall as the band. SCALE 0 is for a piece left to the rows moved
over it. The result is NIL for a piece drawn as it is."
  (when draw
    (destructuring-bind (offset scale shift) draw
      (cond ((zerop scale) (list (list left (+ left columns) 0 0 0)))
            ((plusp offset)
             (list (list left (+ left offset) 1 shift scale) (list (+ left offset) (+ left columns) scale shift scale)))
            (t (list (list (+ left offset) (+ left columns) scale shift scale)))))))

(defvar *other-cursor-functions* '()
  "Functions of (EDITOR WINDOW ROWS) giving the byte offsets of cursors to draw
in ROWS of WINDOW besides point, in any order: the cursors of a split, for one.
Each is drawn in :split-cursor, or :split-cursor-insert while the real cursor
is a bar.")

(defvar *point-marked-functions* '()
  "Functions of a buffer, true when it shows where point is in a look of its
own (the calendar its day): no cursor is then drawn over it, in its window
selected or not.")

(defun point-marked-p (buffer)
  "True when BUFFER shows where point is in a look of its own
\(*POINT-MARKED-FUNCTIONS*), so that no cursor is drawn over it."
  (tuatara.runtime:run-hook-until-success *point-marked-functions* buffer))

(defun row-at (rows position)
  "The index of the row of ROWS showing POSITION, or at whose end it stands;
NIL when POSITION is outside them."
  (let ((lo (tuatara.runtime:bisect (mid 0 (length rows)) (<= (tuatara.display:row-start (aref rows mid)) position))))
    (let ((r (1- lo)))
      (and (>= r 0)
           (let ((row (aref rows r)))
             (and (or (< position (tuatara.display:row-end row))
                      (and (tuatara.display:row-last-p row) (= position (tuatara.display:row-end row))))
                  r))))))

(defun cursor-elsewhere-p (editor window)
  "True when the cursor, where typing goes, is not in WINDOW: WINDOW is not
the selected one, or the minibuffer or a question in the echo area waits
for typing (ECHO-QUESTION-P). WINDOW's point is then drawn in :cursor-inactive."
  (or (not (eq window (editor-window editor)))
      (and (editor-minibuffer-active editor) t)
      (echo-question-p editor)))

(defun other-cursors-in (editor window rows)
  "The cursors to draw in ROWS of WINDOW besides the real one, as two
values: the highlight ranges of those on a character, and for each row with
one after the end of its text, (ROW . FACE). Those of *OTHER-CURSOR-FUNCTIONS*
are in :split-cursor (:split-cursor-insert while text is typed); WINDOW's
point, while the cursor is elsewhere (CURSOR-ELSEWHERE-P), in :cursor-inactive,
but in the hex view, which marks its octet itself, and in a buffer that marks
point in its own way (*POINT-MARKED-FUNCTIONS*)."
  (let* ((buffer (tuatara.display:window-buffer window))
         (point (and (cursor-elsewhere-p editor window)
                     (not (tuatara.display:hex-view-p buffer))
                     (not (point-marked-p buffer))
                     (tuatara.display:window-point window))))
    (when (and (or point *other-cursor-functions*) (plusp (length rows)))
      (let* ((from (tuatara.display:row-start (aref rows 0)))
             (to (tuatara.display:row-end (aref rows (1- (length rows)))))
             (split-face (if (nth-value 2 (cursor-look editor)) :split-cursor-insert :split-cursor))
             (ranges '()) (ends '()))
        (flet ((place (p face)
                 (let ((r (and (<= from p to) (row-at rows p))))
                   (when r
                     (let* ((row (aref rows r)) (shown (tuatara.display:row-shown-end row)))
                       (cond ((< p shown)
                              (push (list p (tuatara.buffer:buffer-forward-char buffer p) face) ranges))
                             ((and (= p shown) (not (tuatara.display:row-truncated-p row))
                                   (or (tuatara.display:row-last-p row) (< shown (tuatara.display:row-end row)))
                                   (not (assoc r ends)))
                              (push (cons r face) ends))))))))
          (dolist (f *other-cursor-functions*)
            (dolist (p (tuatara.runtime:call-hook-function f editor window rows))
              (place p split-face)))
          (when point (place point :cursor-inactive)))
        (values (nreverse ranges) ends)))))

(defvar *ruler-functions* '()
  "Functions of a window giving the columns of its text marked by a line down
the window, over the text's backgrounds but under *FACES-OVER-RULERS*: each a
list of (COLUMN FACE),
COLUMN counted from 0 at the line's start (the fill column's in
:display-fill-column, point's in :cursor-column), or NIL for none.")

(defparameter *faces-over-rulers* '(:hl-line :soft-wrap-row :split-cursor :split-cursor-insert)
  "The faces a line down the window (*RULER-FUNCTIONS*) is painted under: the
line point is on, and the other cursors. Every other face of a character, a
source block's background or the selection's, is painted under the line.")

(defun ruler-cell (text spans at ruler-face)
  "TEXT and SPANS, (START END FACE) over its characters, as two values, with the
character at screen column AT painted in RULER-FACE over the faces it has but
those of *FACES-OVER-RULERS*; TEXT padded with spaces to reach AT when it is
shorter."
  (let* ((starts (tuatara.display:string-column-starts text))
         (column (aref starts (length text)))
         (index (loop for i from 0 below (length text)
                      when (> (aref starts (1+ i)) at) return i)))
    (unless index
      (setf text (concatenate 'string text (tuatara.text:spaces (- (1+ at) column)))
            index (1- (length text))))
    (let ((cell (list index (1+ index) ruler-face)) (out '()))
      (dolist (span spans)
        (destructuring-bind (start end face) span
          (cond ((or (<= end index) (> start index))
                 (when (and cell (> start index)) (push cell out) (setf cell nil))
                 (push span out))
                (t
                 (when (< start index) (push (list start index face) out))
                 (push (list index (1+ index)
                             (let ((faces (if (listp face) face (list face))))
                               (flet ((over (f) (member f *faces-over-rulers*)))
                                 (append (remove-if-not #'over faces) (list ruler-face) (remove-if #'over faces)))))
                       out)
                 (when (< (1+ index) end) (push (list (1+ index) end face) out))
                 (setf cell nil)))))
      (when cell (push cell out))
      (values text (nreverse out)))))

(defun ruler-cells (text spans columns)
  "TEXT and SPANS as RULER-CELL leaves them after painting each of COLUMNS, each (AT FACE)."
  (loop for (at face) in columns
        do (multiple-value-setq (text spans) (ruler-cell text spans at face)))
  (values text spans))

(defvar *frame-images* '()
  "The pictures of the frame being painted, each (IMAGE ROW COLUMN COLUMNS
ROWS OFFSET): where WINDOW-PIECES left a window's text blank for its buffer's
picture, and the buffer's :image-offset (see TUATARA.FRONTEND:FRONTEND-PUT-IMAGE).")

(defun picture-in-place (frontend buffer)
  "The picture BUFFER shows in place of its text on FRONTEND: its :image local
where the frontend shows pictures and its :image-hidden local is false; NIL
when it shows its text."
  (and (tuatara.frontend:frontend-images-p frontend)
       (not (tuatara.buffer:buffer-local buffer :image-hidden))
       (tuatara.buffer:buffer-local buffer :image)))

(defun shown-as-picture-p (frontend buffer)
  "True when BUFFER's windows show a picture in place of its text on FRONTEND
\(PICTURE-IN-PLACE), or it is shown as pictures (:picture-view) and its
picture is shown or on its way: its text is not there to put a cursor in."
  (or (picture-in-place frontend buffer)
      (and (tuatara.frontend:frontend-images-p frontend)
           (not (tuatara.buffer:buffer-local buffer :image-hidden))
           (tuatara.buffer:buffer-local buffer :picture-view))))

(defun frontend-row (window row)
  "The row, counted from WINDOW's first row of text, the frontend is told for
row ROW of WINDOW's last layout: ROW itself where the frontend moves rows
down under rows drawn large (*ROW-SCALING*), else the screen row it is
drawn on (TUATARA.DISPLAY:WINDOW-ROW-TOP)."
  (if tuatara.display:*row-scaling* row (tuatara.display:window-row-top window row)))

(defun layout-row (window row)
  "The row of WINDOW's last layout shown at ROW, a row the frontend gives
counted from WINDOW's first row of text (FRONTEND-ROW, the other way)."
  (if (or tuatara.display:*row-scaling* (minusp row)) row (tuatara.display:window-row-at window row)))

(defun lay-out-window (editor window)
  "Lay WINDOW out with its point visible, its gutter's width worked out
again: the rows it shows. Point is kept visible in the rows the window has
under a panel of one row, so that a taller panel, while it lasts, only
hides rows."
  (setf (tuatara.display:window-margin window) (window-margin-for editor window))
  (let ((shown (tuatara.display:window-height window)))
    (setf (tuatara.display:window-height window)
          (or (tuatara.display:window-full-height window) shown))
    (unwind-protect (tuatara.display:window-ensure-point-visible window)
      (setf (tuatara.display:window-height window) shown))))

(defun lay-out-windows (editor)
  "Lay out every window of EDITOR's frame as a frame drawn now would, its
point visible, painting nothing: for commands that work from what a window
shows (scrolling, moving to a row of it) while nothing is drawn."
  (fit-windows editor)
  (dolist (window (editor-windows editor))
    (when (plusp (tuatara.display:area-height window))
      (lay-out-window editor window))))

(defun text-row-piece (row pieces margin left width cursor rulers shift)
  "The piece of a screen row (see PUT-SCREEN-ROW) showing ROW of a window LEFT
columns from the screen's edge and WIDTH wide: its gutter of MARGIN columns
from PIECES, each (STRING . FACE), its text in its faces, CURSOR (the face of
another cursor after the end of its line, or NIL), the columns RULERS marks,
each (AT FACE), and SHIFT, how far rows drawn large above move it down."
  (let* ((gutter (if (plusp margin)
                     (concatenate 'string (apply #'concatenate 'string (mapcar #'car pieces)) " ")
                     ""))
         ;; the gutter's colours only where it is shown (not in the hex view, whose offsets stand there)
         (spans (append (and (plusp margin)
                             (let ((pos 0))
                               (loop for (text . face) in pieces
                                     collect (prog1 (list pos (+ pos (length text)) face) (incf pos (length text))))))
                        (mapcar (lambda (span)
                                  (list (+ (first span) (length gutter))
                                        (+ (second span) (length gutter))
                                        (third span)))
                                (tuatara.display:row-spans row))))
         (text (concatenate 'string gutter (tuatara.display:row-string row)))
         (cursor-cell (and (< (+ (length gutter) (tuatara.display:row-columns row)) width) cursor))
         (extend (tuatara.display:row-extend row))
         ;; a face carried past the line's end fills the row to the window's edge
         (fill (if extend
                   (max 0 (- width (length gutter) (tuatara.display:row-columns row) (if cursor-cell 1 0)))
                   0)))
    ;; another cursor after the end of the line's text
    (when cursor-cell
      (setf spans (append spans (list (list (length text) (1+ (length text)) cursor-cell)))
            text (concatenate 'string text " ")))
    (when (plusp fill)
      (let ((last (car (last spans))) (end (+ (length text) fill)))
        (setf spans (if (and last (= (second last) (length text)) (equal (third last) extend))
                        (append (butlast spans) (list (list (first last) end extend)))
                        (append spans (list (list (length text) end extend))))
              text (concatenate 'string text (tuatara.text:spaces fill)))))
    (multiple-value-bind (text spans) (ruler-cells text (remove nil spans :key #'third) rulers)
      ;; a row drawn large, from after its gutter, or moved down by those above
      (let ((scale (tuatara.display:text-scale (tuatara.display:row-scale row))))
        (list left width text spans nil
              (and (or (/= scale 1) (plusp shift)) (list (length gutter) scale shift))
              (tuatara.display:string-columns gutter))))))

(define-record (drawn-row (:constructor make-drawn-row ()) (:copier nil))
  "What a window's screen row was last drawn from, and the PIECE made of it
\(ROW-PIECE): the TEXT of the row of the buffer it showed and the FACES of its
characters (copies), its EXTEND, COLUMNS and SCALE, the GUTTER pieces, the
CURSOR, the RULERS, the LEFT, WIDTH and MARGIN of the window and the SHIFT of
the rows above. TEXT is NIL for a row past the buffer's text. START is where
that row began in the buffer's text, an octet, and FRAME the window's frame
that last drew it (DRAWN-ROWS-FRAME)."
  (text nil :documentation "The text of the buffer's row drawn, or NIL past the text.")
  (faces nil :documentation "The faces of the row's characters drawn.")
  (extend nil :documentation "The face carried past the end of the row's line.")
  (columns 0 :documentation "The columns the row's text took.")
  (scale 1 :documentation "How large the row was drawn.")
  (gutter nil :documentation "The gutter's pieces beside the row.")
  (cursor nil :documentation "The face of another cursor after the row's line, or NIL.")
  (rulers nil :documentation "The columns marked down the window.")
  (left 0 :documentation "The window's left column.")
  (width 0 :documentation "The window's width.")
  (margin 0 :documentation "The width of the window's gutter.")
  (shift 0 :documentation "How far rows drawn large above moved the row down.")
  (piece nil :documentation "The piece of the screen row made, or NIL before.")
  (start nil :documentation "Where the buffer's row drawn began, an octet, or NIL past the text.")
  (frame -1 :type fixnum :documentation "The window's frame that last drew the row."))

(setf (documentation 'make-drawn-row 'function) "A DRAWN-ROW of nothing drawn yet.")

(define-record (drawn-rows (:constructor make-drawn-rows ()) (:copier nil))
  "The rows of a window's screen as drawn: NOW, the DRAWN-ROW of each of the
first NOW-COUNT screen rows of the frame being drawn, BEFORE and BEFORE-COUNT
those of the frame before, and FRAME, the number of the frame being drawn."
  (now (vector) :type simple-vector :documentation "The DRAWN-ROW of each screen row of the frame being drawn.")
  (now-count 0 :type fixnum :documentation "How many screen rows the frame being drawn has.")
  (before (vector) :type simple-vector :documentation "The DRAWN-ROW of each screen row of the frame before.")
  (before-count 0 :type fixnum :documentation "How many screen rows the frame before had.")
  (frame 0 :type fixnum :documentation "The number of the frame being drawn."))

(setf (documentation 'make-drawn-rows 'function) "The DRAWN-ROWS of a window not drawn yet.")

(defvar *drawn-rows* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "For each window, its DRAWN-ROWS.")

(defun begin-drawing-rows (window height)
  "Begin a frame of WINDOW's HEIGHT screen rows: the rows of the frame drawn
last become those it was drawn from (DRAWN-ROWS-BEFORE)."
  (let ((drawn (or (gethash window *drawn-rows*) (setf (gethash window *drawn-rows*) (make-drawn-rows)))))
    (rotatef (drawn-rows-now drawn) (drawn-rows-before drawn))
    (when (< (length (drawn-rows-now drawn)) height)
      (setf (drawn-rows-now drawn) (make-array height :initial-element nil)))
    (setf (drawn-rows-before-count drawn) (drawn-rows-now-count drawn)
          (drawn-rows-now-count drawn) height)
    (incf (drawn-rows-frame drawn))
    drawn))

(defun drawn-row-starting (drawn start)
  "The row DRAWN, a DRAWN-ROWS, drew in the frame before from where START, an
octet, began, or NIL: looked for by halves, the rows drawn from the text going
down it in order."
  (let ((rows (drawn-rows-before drawn)))
    (loop with low = 0 and high = (1- (drawn-rows-before-count drawn))
          while (<= low high)
          do (let* ((middle (floor (+ low high) 2))
                    (row (svref rows middle))
                    (at (and row (drawn-row-start row))))
               (cond ((null at) (setf high (1- middle)))
                     ((= at start) (return row))
                     ((< at start) (setf low (1+ middle)))
                     (t (setf high (1- middle))))))))

(defun drawn-as-before-p (drawn row pieces margin left width cursor rulers shift)
  "True when DRAWN, a DRAWN-ROW, was drawn from what the screen row is to be
drawn from now: ROW of the buffer (NIL past its text), with PIECES, MARGIN,
LEFT, WIDTH, CURSOR, RULERS and SHIFT as ROW-PIECE takes them."
  (and (drawn-row-piece drawn)
       (if row
           (let ((text (drawn-row-text drawn)) (faces (drawn-row-faces drawn))
                 (now (tuatara.display:row-faces row)))
             (and text (string= text (tuatara.display:row-string row))
                  (= (length faces) (length now))
                  (loop for i from 0 below (length faces) always (equal (svref faces i) (aref now i)))
                  (equal (drawn-row-extend drawn) (tuatara.display:row-extend row))
                  (= (drawn-row-columns drawn) (tuatara.display:row-columns row))
                  (eql (drawn-row-scale drawn) (tuatara.display:row-scale row))
                  (equal (drawn-row-gutter drawn) pieces)
                  (equal (drawn-row-cursor drawn) cursor)))
           (null (drawn-row-text drawn)))
       (equal (drawn-row-rulers drawn) rulers)
       (= (drawn-row-left drawn) left) (= (drawn-row-width drawn) width)
       (= (drawn-row-margin drawn) margin) (= (drawn-row-shift drawn) shift)))

(defun row-piece (rows r row pieces margin left width cursor rulers shift)
  "The piece of screen row R of the window whose DRAWN-ROWS are ROWS (see
PUT-SCREEN-ROW): ROW of its buffer as TEXT-ROW-PIECE makes it from PIECES,
MARGIN, LEFT, WIDTH, CURSOR, RULERS and SHIFT, or with no ROW a row past the
text. The piece made the last time the row was drawn is given again while it
is drawn from the same, at the same screen row or, the window moved, at
another: a row is made only when something it shows has changed."
  (let* ((frame (drawn-rows-frame rows))
         (there (and (< r (drawn-rows-before-count rows)) (svref (drawn-rows-before rows) r)))
         (drawn (flet ((as-before (drawn)
                         (and drawn (/= (drawn-row-frame drawn) frame)
                              (drawn-as-before-p drawn row pieces margin left width cursor rulers shift)
                              drawn)))
                  (or (as-before there)
                      (and row (as-before (drawn-row-starting rows (tuatara.display:row-start row))))))))
    (unless drawn
      ;; made again: in the row that showed this part of the text, else in a new one, as the row
      ;; drawn here may yet be wanted lower down, where the window moved it
      (setf drawn (if (and there (/= (drawn-row-frame there) frame)
                           (or (null (drawn-row-start there))
                               (and row (= (drawn-row-start there) (tuatara.display:row-start row)))))
                      there
                      (make-drawn-row))
            (drawn-row-piece drawn) nil))
    (setf (drawn-row-frame drawn) frame
          (svref (drawn-rows-now rows) r) drawn)
    (if (drawn-row-piece drawn)
        (drawn-row-piece drawn)
        (progn
          (setf (drawn-row-start drawn) (and row (tuatara.display:row-start row))
                (drawn-row-text drawn) (and row (copy-seq (tuatara.display:row-string row)))
                (drawn-row-faces drawn) (and row (coerce (copy-seq (tuatara.display:row-faces row)) 'simple-vector))
                (drawn-row-extend drawn) (and row (tuatara.display:row-extend row))
                (drawn-row-columns drawn) (if row (tuatara.display:row-columns row) 0)
                (drawn-row-scale drawn) (and row (tuatara.display:row-scale row))
                (drawn-row-gutter drawn) pieces (drawn-row-cursor drawn) cursor (drawn-row-rulers drawn) rulers
                (drawn-row-left drawn) left (drawn-row-width drawn) width (drawn-row-margin drawn) margin
                (drawn-row-shift drawn) shift)
          (setf (drawn-row-piece drawn)
                (if row
                    (text-row-piece row pieces margin left width cursor rulers shift)
                    (multiple-value-bind (text spans) (ruler-cells "" '() rulers)
                      ;; below rows moved down: left to them
                      (list left width text spans nil (and (plusp shift) (list 0 0 0))))))))))

(defun note-window-pictures (editor window rows)
  "Note in *FRAME-IMAGES* the pictures WINDOW shows, ROWS its layout, where
EDITOR's frontend shows pictures; return the rows to draw: ROWS, or none
where a picture stands in place of the text or is on its way.
A buffer with a picture (its :image local) has it shown in place of its text,
unless its :image-hidden local is true, at its own size from its
:image-offset when it has one. A buffer shown as pictures (:picture-view)
whose picture is on its way (:picture-pending) shows blank rows meanwhile.
Pictures a buffer places over its text (its :pictures local, each (IMAGE ROW
COLUMN COLUMNS ROWS) of its text's cells, a terminal's) are noted with its
text shown as it is. Pictures among the rows of its text (its
:row-pictures-function local, a function of WINDOW and its rows) are given
as (IMAGE ROW COLUMN COLUMNS ROWS) for row ROW, left as tall as the picture or
taller, the picture at its foot, or (IMAGE ROW COLUMN COLUMNS ROWS SKIPPED)
for one the text goes round, SKIPPED of its rows above ROW not shown."
  (let* ((frontend (editor-frontend editor))
         (buffer (tuatara.display:window-buffer window))
         (top (tuatara.display:window-text-top window))
         (left (tuatara.display:area-left window))
         (text-left (+ left (tuatara.display:window-margin window)))
         (height (tuatara.display:window-height window))
         (image (picture-in-place frontend buffer)))
    (when image
      (push (list image top left (tuatara.display:window-width window) height
                  (tuatara.buffer:buffer-local buffer :image-offset))
            *frame-images*)
      (setf rows #()))
    (when (tuatara.frontend:frontend-images-p frontend)
      (loop with text-width = (tuatara.display:window-text-width window)
            for (picture row column columns tall) in (tuatara.buffer:buffer-local buffer :pictures)
            for shown-rows = (min tall (- height row))
            for shown-columns = (min columns (- text-width column))
            when (and (>= row 0) (>= column 0) (plusp shown-rows) (plusp shown-columns))
              do (push (list picture (+ top row) (+ text-left column) shown-columns shown-rows nil) *frame-images*))
      (let ((place (tuatara.buffer:buffer-local buffer :row-pictures-function)))
        (when (and place (plusp (length rows)))
          (loop with drawn = (min (length rows) (tuatara.display:window-rows-drawn window))
                for (picture row column columns tall skipped) in (funcall place window rows)
                for row-top = (tuatara.display:window-row-top window row)
                when (< -1 row drawn)
                  do (push (if skipped
                               ;; at its own size, from the rows above the window's start on, cut at its foot
                               (list picture (+ top row-top) (+ text-left column) columns
                                     (min tall (- height row-top)) (list skipped 0))
                               ;; at the foot of its row, which begins higher when it begins below a float
                               (list picture (+ top row-top (- (tuatara.display:row-scale (aref rows row)) tall))
                                     (+ text-left column) columns tall nil))
                           *frame-images*))))
      ;; a buffer shown as pictures (:picture-view) whose picture is on its way: blank, not its text
      (when (and (null image)
                 (not (tuatara.buffer:buffer-local buffer :image-hidden))
                 (tuatara.buffer:buffer-local buffer :picture-view)
                 (tuatara.buffer:buffer-local buffer :picture-pending))
        (setf rows #())))
    rows))

(defun put-window-rows (window rows cells cursor-ends rulers screen)
  "Add to SCREEN the pieces of WINDOW's text rows, as WINDOW-PIECES says: each
of ROWS, its layout, with its gutter's CELLS, the other cursor after its
line's end from CURSOR-ENDS ((ROW . FACE)) and the columns RULERS marks, those
past the window's text blank. Where the frontend moves rows down under rows
drawn large (*ROW-SCALING*) each row is put on its own screen row and told
how far down it goes; elsewhere a tall row's band is the screen rows below
it, left blank. The rows past those WINDOW draws (WINDOW-ROWS-DRAWN) are left
out."
  (let* ((top (tuatara.display:window-text-top window))
         (left (tuatara.display:area-left window))
         (width (tuatara.display:window-width window))
         (height (tuatara.display:window-height window))
         (margin (tuatara.display:window-margin window))
         (shift 0) (s 0)
         (drawn (min (length rows) (tuatara.display:window-rows-drawn window)))
         (drawn-rows (begin-drawing-rows window height))
         (in-place (not tuatara.display:*row-scaling*)))
    (flet ((put (piece)
             (push piece (aref screen (+ top s)))
             (put-window-divider window (+ left width) screen (+ top s))
             (incf s)))
      (dotimes (r drawn)
        (let ((row (aref rows r)))
          (put (row-piece drawn-rows s row (pop cells) margin left width (cdr (assoc r cursor-ends))
                          rulers (if in-place 0 shift)))
          (if in-place
              (loop repeat (1- (ceiling (tuatara.display:row-scale row)))
                    while (< s height)
                    do (put (row-piece drawn-rows s nil nil margin left width nil rulers 0)))
              (incf shift (1- (tuatara.display:row-scale row))))))
      (loop while (< s height)
            do (put (row-piece drawn-rows s nil nil margin left width nil rulers shift))))))

(defun window-pieces (editor window selected screen)
  "Lay WINDOW out with its point visible and add what it shows to SCREEN, a
vector of the pieces of each screen row (PUT-SCREEN-ROW). It adds its buffer's
header line when it has one, in :header-line, its text rows with their gutter,
the divider beside a window to its right, and its mode line, in :mode-line
when it is SELECTED, else in :mode-line-inactive. Point is kept visible in the
rows the window has under a panel of one row, so that a taller panel, while it
lasts, only hides rows. Pictures in place of its text or among its rows are
noted where the frontend shows pictures (NOTE-WINDOW-PICTURES), and rows
drawn large take as many screen rows where the frontend draws all rows alike
\(PUT-WINDOW-ROWS)."
  (let ((top (tuatara.display:window-text-top window))
        (left (tuatara.display:area-left window))
        (width (tuatara.display:window-width window))
        (height (tuatara.display:window-height window)))
    (when (plusp (tuatara.display:area-height window))
      (let* ((rows (lay-out-window editor window))
             (cursor-ends (multiple-value-bind (ranges ends) (other-cursors-in editor window rows)
                            ;; other cursors last, over everything else
                            (apply #'tuatara.display:apply-highlights rows
                                   (nconc (window-highlights-for editor window rows) (list ranges)))
                            ends))
             (margin (tuatara.display:window-margin window))
             ;; a gutter's cells only where it is shown
             (cells (and (plusp margin) (gutter-cells editor window rows)))
             ;; the screen columns marked down the window
             (rulers (let ((shift (if (tuatara.display:window-wraps-p window)
                                      0
                                      (tuatara.display:window-hscroll window))))
                       (loop for (column face) in (loop for f in *ruler-functions*
                                                        append (tuatara.runtime:call-hook-function f window))
                             for at = (+ margin (- column shift))
                             when (and (<= margin at) (< at width)) collect (list at face)))))
        ;; what follows the rows in a large file read in before it is wanted
        (when (plusp (length rows))
          (tuatara.text:read-ahead (tuatara.buffer:buffer-text (tuatara.display:window-buffer window))
                                   (tuatara.display:row-start (aref rows 0))
                                   (tuatara.display:row-end (aref rows (1- (length rows))))))
        (when (plusp (tuatara.display:window-header-rows window))
          (let ((header-top (tuatara.display:area-top window)))
            (multiple-value-bind (text spans)
                (tuatara.runtime:ignoring-errors-reported ("drawing a header line")
                  (funcall (tuatara.display:buffer-header-line (tuatara.display:window-buffer window)) window))
              (push (list left width (if (stringp text) text "") (and (listp spans) (remove nil spans :key #'third))
                          :header-line)
                    (aref screen header-top)))
            (put-window-divider window (+ left width) screen header-top)))
        ;; a picture in place of the text leaves no rows
        (setf rows (note-window-pictures editor window rows))
        (put-window-rows window rows cells cursor-ends rulers screen)
        (multiple-value-bind (line spans)
            (funcall (or (editor-mode-line-function editor) #'default-mode-line) editor window)
          (push (list left (tuatara.display:area-width window) line spans (if selected :mode-line :mode-line-inactive))
                (aref screen (+ top height))))))))

(defun cursor-column-past-line (window column &optional (scale 1))
  "COLUMN, where WINDOW's point is drawn, moved on to where the corner of a
rectangle stands past the end of point's line, each column beyond it SCALE
columns wide on a row drawn large; kept inside the window."
  (let ((buffer (tuatara.display:window-buffer window)))
    (if (rectangle-live-p buffer)
        (let ((beyond (- (rectangle-point-column buffer)
                         (tuatara.display:buffer-screen-column buffer (tuatara.buffer:buffer-point buffer)))))
          (min (+ column (round (* beyond scale)))
               (max column (1- (tuatara.display:window-text-width window)))))
        column)))

(define-setting *gui-title* "~a - Tuatara"
  "The title of the window tuatara --gui opens, where the desktop names it.
It is a format control given the name of the buffer the selected window
shows.
- \"~a - Tuatara\": the buffer's name, a dash, Tuatara (the standard).
- \"~a\": the buffer's name alone.
- any other text: that text, ~a standing for the buffer's name."
  :values string)

(defvar *title-functions* '()
  "Functions of the editor returning text to show before the window's title
(the clock running on an Org task), or NIL.")

(defun sandbox-outcome-words (outcome said &key (label "the program")
                                                   (seconds-setting 'tuatara-user:*sandbox-seconds*))
  "Why a program run shut in (SANDBOX-RUN) gave nothing, as a sentence
beginning with LABEL, from how it went, OUTCOME, and what it SAID, as
SANDBOX-RUN gives them. The reason is one of these. There is no sandbox here,
and why. It was stopped after the seconds SECONDS-SETTING (a setting's symbol)
holds, or by you. It printed more than it may. Or it failed, with the first
line of what it said. The result is NIL when OUTCOME is :DONE."
  (case outcome
    (:done nil)
    (:unavailable (format nil "~a runs only shut in the sandbox, and there is none here: ~a" label said))
    (:timeout (format nil "~a was stopped after ~d seconds (~(~a~))"
                      label (symbol-value seconds-setting) seconds-setting))
    (:cancelled (format nil "~a was stopped" label))
    (:too-much (format nil "~a printed more than it may" label))
    (t (let ((line (string-trim " " (or (first (uiop:split-string (or said "") :separator '(#\Newline))) ""))))
         (format nil "~a failed~@[: ~a~]" label
                 (and (plusp (length line))
                      (escape-controls (subseq line 0 (min 300 (length line))))))))))

(defmethod tuatara.runtime:host-say-hook-failure ((host editor-host) text)
  "Say TEXT in the echo area and *Messages*, its controls written out."
  (message "~a" text))

(defun show-title (editor buffer)
  "Give EDITOR's frontend the title *GUI-TITLE* makes of BUFFER's name, with
what *TITLE-FUNCTIONS* add before it, when that has changed since it was last
given; control and direction characters in it are written out, and octets
that are not UTF-8 as \\xNN, as the mode line writes them (ESCAPE-CONTROLS)."
  (let ((name (tuatara.buffer:buffer-name buffer))
        (extras (loop for f in *title-functions*
                      for extra = (tuatara.runtime:call-hook-function f editor)
                      when extra collect extra))
        (shown (editor-title-shown editor)))
    (unless (and shown (eq name (first shown)) (eq *gui-title* (second shown)) (equal extras (third shown)))
      (setf (editor-title-shown editor) (list name *gui-title* extras))
      (tuatara.frontend:frontend-set-title
       (editor-frontend editor)
       (shown-name (format nil "~{~a ~}~a" extras
                           (or (tuatara.runtime:ignoring-errors-reported ("making the title of ~a" name)
                                 (format nil *gui-title* name))
                               name)))))))

(defun frame-secret-p (editor)
  "True when a window of EDITOR shows a buffer whose text is secret
\(TUATARA.RUNTIME:SECRET-BUFFER-P), or the buffer commands act on is one:
drawing the frame counts then as having a secret at hand. Looks at each
window once, making nothing."
  (labels ((secret-in (area)
             (if (typep area 'tuatara.display:window)
                 (tuatara.runtime:secret-buffer-p (tuatara.display:window-buffer area))
                 (some #'secret-in (tuatara.display:split-children area)))))
    (let ((window (editor-window editor)))
      (or (tuatara.runtime:secret-buffer-p *current-buffer*)
          (and window (secret-in (tuatara.display:area-root window)))))))

(defun redisplay (editor)
  "Lay out every window with its point visible and paint a frame: each
window's text rows and its mode line, the selected window's in :mode-line and
the others' in :mode-line-inactive, a divider between windows side by side,
the bottom panel (echo area, or the minibuffer with its candidates, or the key
hints), and the cursor. Nothing here waits for a large file's lines to be
counted: what needs them shows without them meanwhile. While recorded keys
play, only the candidates of a question are brought up to date: nothing is
drawn. While a window shows a secret buffer (FRAME-SECRET-P), a failure in
drawing is reported without its text."
  ;; what fails while a secret buffer is shown is reported without its text
  (let ((tuatara.runtime:*secret-at-hand* (or tuatara.runtime:*secret-at-hand* (frame-secret-p editor))))
    (update-candidates editor)
    (when (editor-keys-playing editor) (return-from redisplay nil))
    (preview-selection editor)
    (popup-follow editor)
    (multiple-value-bind (panel minibuffer-row) (panel-rows editor)
      (let* ((frame (editor-frame editor))
             (frontend (frame-frontend frame))
             (tree-rows (fit-windows editor (length panel)))
             (selected (frame-window frame))
             (screen (make-array tree-rows :initial-element '()))
             (width (frame-width frame))
             (*frame-images* '()))
        (dolist (window (editor-windows editor))
          (window-pieces editor window (eq window selected) screen))
        (let ((boxes (popup-boxes editor tree-rows minibuffer-row)))
          (tuatara.frontend:frontend-begin-frame frontend)
          (dotimes (r tree-rows)
            (put-screen-row frontend r (or (aref screen r) (list (list 0 width "" nil nil))) width boxes))
          (loop for (string face spans) in panel
                for r from tree-rows
                do (multiple-value-bind (string spans) (escape-controls string spans :raw-bytes)
                     (multiple-value-bind (string spans face) (lay-boxes-over string spans face r boxes width)
                       (tuatara.frontend:frontend-put-row frontend r string :face face :spans spans))))
          ;; a picture a completion list lies over is left out while it is there: the list stays readable;
          ;; one in a window with no room for it, too
          (loop for (image row column columns rows offset) in *frame-images*
                unless (or (not (plusp rows)) (not (plusp columns))
                           (some (lambda (box) (box-over-area-p box row column columns rows)) boxes))
                  do (tuatara.frontend:frontend-put-image frontend image row column columns rows offset)))
        (cond
          ((editor-minibuffer-active editor)
           ;; the minibuffer's cursor: the look for all, *CURSOR-SHAPE* and *CURSOR-COLOUR*
           (tuatara.frontend:frontend-set-cursor
            frontend (+ tree-rows minibuffer-row)
            (minibuffer-column editor (editor-minibuffer-point editor))
            (or *cursor-shape* :block) *cursor-colour*))
          ((echo-question-p editor)
           ;; a question in the echo area: the cursor after it, as the minibuffer's
           (tuatara.frontend:frontend-set-cursor
            frontend (+ tree-rows minibuffer-row) (echo-cursor-column editor)
            (or *cursor-shape* :block) *cursor-colour*))
          (t
           (multiple-value-bind (row column) (tuatara.display:window-point-position selected)
             (multiple-value-bind (shape colour) (cursor-look editor)
               (if (and row (< row (tuatara.display:window-rows-drawn selected)))
                   (tuatara.frontend:frontend-set-cursor
                    frontend
                    (+ (tuatara.display:window-text-top selected) (frontend-row selected row))
                    (+ (tuatara.display:area-left selected) (tuatara.display:window-margin selected)
                       (cursor-column-past-line selected column
                                                (tuatara.display:text-scale
                                                 (tuatara.display:row-scale
                                                  (aref (tuatara.display:window-rows selected) row)))))
                    ;; no cursor over a picture shown in place of the text, nor while one is on its
                    ;; way, nor where the buffer marks point itself (the calendar's day)
                    (let ((buffer (tuatara.display:window-buffer selected)))
                      (if (or (shown-as-picture-p frontend buffer) (point-marked-p buffer)) :hidden shape))
                    colour)
                   ;; point is under the panel for now: the cursor waits after the echo row's text
                   (tuatara.frontend:frontend-set-cursor
                    frontend (+ tree-rows minibuffer-row)
                    (min (panel-columns (first (nth minibuffer-row panel))) (1- (editor-width editor)))
                    shape colour))))))
        (tuatara.frontend:frontend-end-frame frontend)
        (show-title editor (tuatara.display:window-buffer selected))))))
