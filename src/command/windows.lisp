;;;; src/command/windows.lisp --- The windows of the editor, and where buffers are shown.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The windows are the areas of one layout (tuatara.display's splits); the
;;;; editor holds the selected one. Its point is its buffer's point, so that
;;;; commands move and edit through the buffer alone; every other window keeps
;;;; its point in an anchor, and SELECT-WINDOW trades the two.

(in-package #:tuatara.command)

(defun editor-windows (&optional (editor *editor*))
  "The windows of EDITOR from the top left, the order \\[other-window] goes through them."
  (let ((window (editor-window editor)))
    (and window (tuatara.display:area-windows (tuatara.display:area-root window)))))

(defun buffer-windows (buffer &optional (editor *editor*))
  "The windows of EDITOR showing BUFFER, from the top left."
  (remove buffer (editor-windows editor) :key #'tuatara.display:window-buffer :test-not #'eq))

(defun buffer-window (buffer &optional (editor *editor*))
  "The first window of EDITOR, from the top left, showing BUFFER, or NIL."
  (find buffer (editor-windows editor) :key #'tuatara.display:window-buffer))

(defun one-window-p (&optional (editor *editor*))
  "True when EDITOR has one window only."
  (null (tuatara.display:area-parent (editor-window editor))))

(defun fit-windows (editor &optional (panel (length (panel-rows editor))))
  "Lay the windows out on the rows above the panel, PANEL rows: made for a
panel of one row, the windows along the bottom then give up what a taller
panel takes, so that the others stay as they are. Returns the rows the windows
cover."
  (let ((window (editor-window editor)))
    (if (null window)
        0
        (let* ((root (tuatara.display:area-root window))
               (width (editor-width editor))
               (base (max 1 (1- (editor-height editor))))
               (rows (max 1 (min base (- (editor-height editor) panel)))))
          (tuatara.display:layout-area root 0 0 width base width)
          (tuatara.display:trim-area-bottom root (- base rows))
          rows))))

(defun select-window (window &optional (editor *editor*))
  "Make WINDOW the selected window, the one commands act on. The window left
keeps its point; WINDOW's point becomes its buffer's point. Returns WINDOW."
  (let ((old (editor-window editor)))
    (unless (eq old window)
      (when old
        (let ((buffer (tuatara.display:window-buffer old)))
          (setf (tuatara.display:window-point-anchor old)
                (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer)
                                          (tuatara.buffer:buffer-point buffer) :bias :after))))
      (let ((anchor (tuatara.display:window-point-anchor window))
            (buffer (tuatara.display:window-buffer window)))
        (when anchor
          (setf (tuatara.buffer:buffer-point buffer)
                (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer))
                (tuatara.display:window-point-anchor window) nil)))
      (setf (editor-window editor) window))
    (setf (tuatara.display:window-use-time window) (incf (editor-selections editor)))
    window))

(defun follow-selected-state (editor)
  "After windows traded what they show, select the one that took the selected
window's point (the one whose point is its buffer's)."
  (let ((selected (find-if-not #'tuatara.display:window-point-anchor (editor-windows editor))))
    (when (and selected (not (eq selected (editor-window editor))))
      (setf (editor-window editor) selected
            (tuatara.display:window-use-time selected) (incf (editor-selections editor))))))

(defun split-window (area side &key size buffer)
  "Make a new window on SIDE of AREA (:above, :below, :left or :right; AREA is
a window or the whole layout) taking SIZE rows or columns of it, half when
SIZE is NIL. It shows BUFFER from its start, or when BUFFER is NIL, what the
window AREA shows, from the same line with the same point. Returns the new
window; signals an error when there is not enough room."
  (let ((editor *editor*))
    (fit-windows editor)
    (let* ((like (and (null buffer) (typep area 'tuatara.display:window) area))
           (buffer (or buffer (tuatara.display:window-buffer area)))
           (new (tuatara.display:make-window buffer))
           (version (tuatara.buffer:buffer-version buffer)))
      (tuatara.display:split-area area side new size)
      (setf (tuatara.display:window-point-anchor new)
            (tuatara.text:make-anchor version (if like (tuatara.display:window-point like) (tuatara.buffer:buffer-point buffer))
                                      :bias :after))
      (when like
        (setf (tuatara.display:window-start new) (tuatara.display:window-start like)
              (tuatara.display:window-hscroll new) (tuatara.display:window-hscroll like)
              (tuatara.display:window-wrap new) (tuatara.display:window-wrap like)))
      (fit-windows editor)
      new)))

(defun most-recent-window (windows)
  "Of WINDOWS, the one selected last."
  (reduce (lambda (a b) (if (> (tuatara.display:window-use-time b) (tuatara.display:window-use-time a)) b a))
          windows))

(defun remove-window (&optional (window (editor-window *editor*)))
  "Delete WINDOW, the selected window by default: its room goes to the window
beside it, and when it was selected, the window used before it is selected.
Signals an error for the only window."
  (let* ((editor *editor*)
         (selected (eq window (editor-window editor)))
         (root (tuatara.display:delete-area window)))
    (when selected
      (select-window (most-recent-window (tuatara.display:area-windows root)) editor))
    (when (null (tuatara.display:area-parent (editor-window editor)))
      (setf (tuatara.display:window-kept (editor-window editor)) nil))
    (fit-windows editor)
    root))

(defun only-window (&optional (window (editor-window *editor*)))
  "Make WINDOW, the selected window by default, fill the screen, deleting the
others, and select it."
  (let ((editor *editor*)
        (root (tuatara.display:area-root window)))
    (unless (eq root window)
      (tuatara.display:replace-area root window))
    (select-window window editor)
    (setf (tuatara.display:window-kept window) nil)
    (fit-windows editor)
    window))

(defun swap-windows (a b)
  "Make windows A and B trade what they show: buffer, start, point and the
rest. The selection goes with the selected window's buffer."
  (let ((state-a (tuatara.display:window-state a))
        (state-b (tuatara.display:window-state b)))
    (tuatara.display:set-window-state a state-b)
    (tuatara.display:set-window-state b state-a)
    (follow-selected-state *editor*)))

(defun rotate-window-states (windows)
  "Move what each of WINDOWS shows to the next of them, the last's to the first.
The selection goes with the selected window's buffer."
  (when (rest windows)
    (let ((states (mapcar #'tuatara.display:window-state windows)))
      (loop for window in windows
            for state in (cons (car (last states)) (butlast states))
            do (tuatara.display:set-window-state window state))
      (follow-selected-state *editor*))))

(defun window-cursor-cell (window)
  "The screen row and column of WINDOW's point, or of its top left corner when
point was not in its last layout."
  (multiple-value-bind (row column) (tuatara.display:window-point-position window)
    (values (+ (tuatara.display:window-text-top window) (or row 0))
            (+ (tuatara.display:area-left window) (tuatara.display:window-margin window) (or column 0)))))

(defun window-at-cell (column row &optional (editor *editor*))
  "The window of EDITOR whose text is shown at screen cell COLUMN, ROW; NIL on
a mode line, a header line, a divider or the echo row."
  (find-if (lambda (window)
             (let ((top (tuatara.display:window-text-top window)) (left (tuatara.display:area-left window)))
               (and (<= top row (+ top (tuatara.display:window-height window) -1))
                    (<= left column (+ left (tuatara.display:window-width window) -1)))))
           (editor-windows editor)))

(defun window-text-cell (window column row)
  "The cell of WINDOW's text at screen cell COLUMN, ROW, as two values, its
column and row from 0 counted from the first cell of text (after the line
numbers' margin and any header line): either less than 0 when the screen
cell is before the text."
  (values (- column (tuatara.display:area-left window) (tuatara.display:window-margin window))
          (- row (tuatara.display:window-text-top window))))

(defun window-cell-position (window column row)
  "The position of WINDOW's buffer shown at screen cell COLUMN, ROW, as its last
layout had it: the character there (in the hex view, the octet), the end of
the line when the cell is past its text, the last character of a row a long
line goes on from, the end of what the window shows when the cell is below it."
  (let* ((rows (tuatara.display:window-rows window))
         (r (max 0 (nth-value 1 (window-text-cell window column row))))
         (c (max 0 (nth-value 0 (window-text-cell window column row))))
         ;; on a row drawn large, as many times fewer columns
         (c (if (< r (length rows)) (floor c (tuatara.display:row-scale (aref rows r))) c)))
    (if (zerop (length rows))
        (tuatara.display:window-start window)
        (let* ((shown (aref rows (min r (1- (length rows)))))
               (bytes (tuatara.display:row-bytes shown))
               (columns (tuatara.display:row-columns shown)))
          (flet ((mapped (from to step)
                   ;; the first column from FROM towards TO showing a character (hex rows have gaps)
                   (loop for i = from then (+ i step)
                         while (if (plusp step) (< i to) (> i to))
                         when (>= (aref bytes i) 0) return (aref bytes i))))
            (cond ((>= r (length rows)) (tuatara.display:row-shown-end shown))
                  ((< c columns) (or (mapped c -1 -1) (mapped c columns 1) (tuatara.display:row-start shown)))
                  ((and (plusp columns)
                        (or (tuatara.display:row-truncated-p shown)
                            (and (< (1+ r) (length rows)) (tuatara.display:row-continuation-p (aref rows (1+ r))))))
                   (or (mapped (1- columns) -1 -1) (tuatara.display:row-shown-end shown)))
                  (t (tuatara.display:row-shown-end shown))))))))

(defun window-towards (direction &optional (window (editor-window *editor*)))
  "The window next to WINDOW in DIRECTION (:left, :right, :above or :below),
seen from WINDOW's point, or NIL."
  (fit-windows *editor*)
  (multiple-value-bind (row column) (window-cursor-cell window)
    (tuatara.display:window-in-direction window direction :row row :column column)))

;;; Where buffers are shown

(defstruct (buffer-place (:constructor make-buffer-place (which place size keep select)) (:copier nil))
  "A rule of PLACE-BUFFERS: the buffers WHICH names are shown in PLACE, SIZE
big, in a window kept for them when KEEP, and selected when SELECT."
  (which '() :read-only t)
  (place :other :read-only t)
  (size nil :read-only t)
  (keep nil :read-only t)
  (select nil :read-only t))

(setf (documentation 'make-buffer-place 'function) "Return a rule of where buffers are shown (see PLACE-BUFFERS)."
      (documentation 'buffer-place-p 'function) "True when OBJECT is a rule of where buffers are shown."
      (documentation 'buffer-place-which 'function) "The buffers a rule is for: names, kinds and major modes."
      (documentation 'buffer-place-place 'function) "Where a rule shows its buffers."
      (documentation 'buffer-place-size 'function) "How big a rule makes a new window, or NIL for half."
      (documentation 'buffer-place-keep 'function) "True when a rule's window is kept for its buffers."
      (documentation 'buffer-place-select 'function) "True when a rule's window is selected.")

(defvar *buffer-places* '()
  "The rules of where buffers are shown, the rule made last first: PLACE-BUFFERS
makes them, DESCRIBE-BUFFER-PLACES lists them.")

(defparameter *places* '(:here :other :below :beside :bottom :top :left :right :right-or-alone :nowhere)
  "The places PLACE-BUFFERS knows.")

(defvar *split-height-threshold* 80
  "The fewest rows a window must have for a buffer shown in another window to
split it into one above the other; a lone window is split so whenever it has
room.")

(defvar *split-width-threshold* 160
  "The fewest columns a window must have for a buffer shown in another window
to split it into two side by side.")

(defun place-buffers (which place &key size keep select)
  "Show the buffers WHICH names in PLACE when a command shows one of them in
another window, as help and *Messages* are shown. One call is one rule; a rule
for the same WHICH replaces the one before, and of the rules that fit a
buffer, the one made last counts.

WHICH is a buffer's name (\"*Messages*\"), a kind of buffer (:help for help,
:messages for *Messages*), a major mode ('help-mode, the modes made from it
too), or a list of those.

PLACE is :here (the selected window), :other (a window already showing the
buffer, else a new one split off a big window: beside it when it has
*SPLIT-WIDTH-THRESHOLD* columns, below it when it has *SPLIT-HEIGHT-THRESHOLD*
rows, below a lone window; else the window used least lately), :below or
:beside (a new window below or right of the selected one), :bottom, :top,
:left or :right (a new window across the whole bottom or top, or down the
whole left or right side),
:right-or-alone (down the whole right side when the screen has
*SPLIT-WIDTH-THRESHOLD* columns, else the only window on it, the others
closed: their buffers stay), or :nowhere (not shown at all).

SIZE is how big a new window is: a fraction of the screen's height, or width
for :beside, :left and :right (0.3), a number of rows of text or of columns (12), or
:fit, as many rows as the buffer has lines, up to half the screen. Half of
what is split when not given.

KEEP true keeps the window for these buffers: the next of them is shown there,
no other buffer is, and quitting it (q) closes it. SELECT true moves the focus
to the window.

  (place-buffers :messages :bottom :size 0.3 :keep t :select t)
  (place-buffers :help :right :size 0.4)
  (place-buffers \"*Buffers*\" :here)"
  (let ((which (if (listp which) which (list which))))
    (dolist (item which)
      (unless (or (stringp item) (symbolp item))
        (error "~s names no buffers: use a name, a kind such as :help, or a major mode." item)))
    (unless (member place *places*)
      (error "~s is no place; use one of ~{~(~s~)~^ ~}." place *places*))
    (unless (or (null size) (eq size :fit) (and (realp size) (plusp size)))
      (error "~s is no size; use a fraction such as 0.3, a number of rows or columns, or :fit." size))
    (let ((rule (make-buffer-place which place size keep select)))
      (setf *buffer-places* (cons rule (remove which *buffer-places* :key #'buffer-place-which :test #'equal)))
      rule)))

(defun buffer-mode-named-p (buffer name)
  "True when BUFFER's major mode is the mode called NAME (a symbol from any
package) or one made from it."
  (let ((mode (tuatara.buffer:buffer-local buffer :major-mode)))
    (and mode
         (some (lambda (class) (string= (symbol-name (class-name class)) (symbol-name name)))
               (sb-mop:class-precedence-list (class-of mode))))))

(defun buffer-place-fits-p (rule buffer)
  "True when RULE is for BUFFER."
  (some (lambda (item)
          (cond ((stringp item) (string= item (tuatara.buffer:buffer-name buffer)))
                ((keywordp item) (eq item (tuatara.buffer:buffer-local buffer :kind)))
                (t (buffer-mode-named-p buffer item))))
        (buffer-place-which rule)))

(defun buffer-place-for (buffer)
  "The rule of where BUFFER is shown, or NIL when none is made for it."
  (find-if (lambda (rule) (buffer-place-fits-p rule buffer)) *buffer-places*))

(defvar *buffer-order* '()
  "The buffers in the order they were last in the selected window, the latest
first: what OTHER-BUFFER goes by.")

(defun note-selected-buffer (buffer)
  "Put BUFFER first in *BUFFER-ORDER*, the buffer in use now."
  (unless (eq buffer (first *buffer-order*))
    (setf *buffer-order* (cons buffer (delete buffer *buffer-order*)))))

(defvar *new-buffer-function* nil
  "A function of one argument, a buffer being killed or NIL, making the buffer
to show when there is no other: a scratch buffer as the editor starts with
(the one being killed may have its name). NIL for an empty *scratch*.")

(defun new-buffer (&optional dying)
  "The buffer made to show when there is no other (*NEW-BUFFER-FUNCTION*),
DYING the one being killed, if any."
  (if *new-buffer-function*
      (funcall *new-buffer-function* dying)
      (tuatara.buffer:make-buffer
       (loop for n from 1
             for name = (if (= n 1) "*scratch*" (format nil "*scratch*<~d>" n))
             unless (tuatara.buffer:find-buffer name) return name))))

(defun buffers-in-use-order ()
  "Every live buffer, those used lately first, the latest first
\(*BUFFER-ORDER*), then the others, the one made last first.
Complexity: O(buffers)."
  (let ((live (make-hash-table :test 'eq)) (seen (make-hash-table :test 'eq)))
    (dolist (b tuatara.buffer:*buffers*) (setf (gethash b live) t))
    (append (loop for b in *buffer-order*
                  when (and (gethash b live) (not (gethash b seen)))
                    do (setf (gethash b seen) t)
                    and collect b)
            (loop for b in tuatara.buffer:*buffers*
                  unless (gethash b seen) collect b))))

(defun other-buffer (&optional (buffer (current-buffer)) visible-ok)
  "The buffer used most recently other than BUFFER: one no window shows before
one that some window shows, unless VISIBLE-OK; then any other buffer; never
one whose name begins with a space; a new one (NEW-BUFFER) when there is none."
  (let ((shown (mapcar #'tuatara.display:window-buffer (editor-windows)))
        (order (buffers-in-use-order)))
    (flet ((usable (b) (let ((name (tuatara.buffer:buffer-name b)))
                         (and (not (eq b buffer))
                              (not (and (plusp (length name)) (char= #\Space (char name 0))))))))
      (or (find-if (lambda (b) (and (usable b) (or visible-ok (not (member b shown))))) order)
          (find-if #'usable order)
          (new-buffer)))))

(defun replacement-buffer (window &optional except)
  "The buffer to show in WINDOW instead of its own: the one it showed before,
else one no window shows, else any other; never EXCEPT, the buffer being
killed. A new one (NEW-BUFFER) when there is none."
  (let ((own (tuatara.display:window-buffer window))
        (shown (mapcar #'tuatara.display:window-buffer (editor-windows))))
    (flet ((usable (b) (and (not (eq b own)) (not (eq b except)))))
      (or (find-if (lambda (b) (and (usable b) (live-buffer-p b)))
                   (mapcar #'first (tuatara.display:window-history window)))
          (find-if (lambda (b) (and (usable b) (not (member b shown)))) tuatara.buffer:*buffers*)
          (find-if #'usable tuatara.buffer:*buffers*)
          (new-buffer except)))))

(defun place-size (size area arrangement buffer)
  "The rows or columns SIZE, a size of PLACE-BUFFERS, gives a new window
beside AREA, laid out as ARRANGEMENT, to show BUFFER; NIL for half of AREA."
  (let ((screen (tuatara.display:area-extent (tuatara.display:area-root area) arrangement))
        (stacked (eq arrangement :stacked)))
    (cond ((null size) nil)
          ((eq size :fit)
           (and stacked (min (floor screen 2) (max tuatara.display:*window-min-height*
                                                   ;; lines still being counted: a large text
                                                   (if (tuatara.buffer:buffer-indexed-p buffer)
                                                       (1+ (tuatara.buffer:buffer-line-count buffer))
                                                       screen)))))
          ((integerp size) (if stacked (1+ size) size))
          (t (max 1 (round (* size screen)))))))

(defun window-area-size (window)
  "WINDOW's area in cells, to find the largest."
  (* (tuatara.display:area-width window) (tuatara.display:area-height window)))

(defun split-sensibly (buffer)
  "A new window for BUFFER split off the largest window, or the one used least
lately: below it when it has *SPLIT-HEIGHT-THRESHOLD* rows, beside it when it
has *SPLIT-WIDTH-THRESHOLD* columns, below a lone window when it has room.
NIL when no window may be split."
  (let* ((windows (remove-if #'tuatara.display:window-kept (editor-windows)))
         (largest (and windows (reduce (lambda (a b) (if (> (window-area-size b) (window-area-size a)) b a)) windows)))
         (least (and windows (reduce (lambda (a b) (if (< (tuatara.display:window-use-time b) (tuatara.display:window-use-time a)) b a))
                                     windows))))
    (dolist (window (remove-duplicates (remove nil (list largest least))))
      (let ((height (tuatara.display:area-height window))
            (width (tuatara.display:area-width window))
            (twice-height (* 2 tuatara.display:*window-min-height*))
            (twice-width (* 2 tuatara.display:*window-min-width*)))
        (let ((new (or (and (>= height (max *split-height-threshold* twice-height))
                            (ignore-errors (split-window window :below :buffer buffer)))
                       (and (>= width (max *split-width-threshold* twice-width))
                            (ignore-errors (split-window window :right :buffer buffer)))
                       (and (null (tuatara.display:area-parent window)) (>= height twice-height)
                            (ignore-errors (split-window window :below :buffer buffer))))))
          (when new (return new)))))))

(defun new-window-for (buffer place rule)
  "A new window made for BUFFER in PLACE, as RULE sizes it, or NIL when there
is no room; for :RIGHT-OR-ALONE on a screen too narrow to split, the selected
window, made the only one and showing BUFFER."
  (let* ((selected (editor-window *editor*))
         (root (tuatara.display:area-root selected))
         (size (and rule (buffer-place-size rule))))
    (flet ((make (area side)
             (ignore-errors
              (split-window area side :buffer buffer
                                      :size (place-size size area (tuatara.display:arrangement-of side) buffer)))))
      (case place
        (:below (make selected :below))
        (:beside (make selected :right))
        (:bottom (make root :below))
        (:top (make root :above))
        (:left (make root :left))
        (:right (make root :right))
        (:right-or-alone (if (>= (tuatara.display:area-width root) *split-width-threshold*)
                             (make root :right)
                             (progn (only-window selected) (show-in-window selected buffer rule))))
        (t (split-sensibly buffer))))))

(defun show-in-window (window buffer rule)
  "Show BUFFER in WINDOW for RULE (or none): the window notes what quitting it
brings back, and is kept for RULE's buffers when RULE says so. Returns WINDOW."
  (let ((old (tuatara.display:window-buffer window)))
    (unless (or (eq old buffer) (eq (tuatara.display:window-quit-restore window) :delete))
      (setf (tuatara.display:window-quit-restore window) old))
    (tuatara.display:set-window-buffer window buffer)
    (when (and rule (buffer-place-keep rule))
      (setf (tuatara.display:window-kept window) rule))
    window))

(defun display-buffer (buffer &key other at)
  "Show BUFFER in a window as the rules of PLACE-BUFFERS say, in another window
when no rule is for it (as the place :other), and return the window; NIL when
its place is :nowhere. With OTHER true, never in the selected window if another
will do. AT :START shows it from its start, point there; AT :END with point at
its end. The window is selected only when the rule says so. A buffer that has
been killed is not shown: that is an error."
  (unless (live-buffer-p buffer)
    (error "~a has been killed" (tuatara.buffer:buffer-name buffer)))
  (let* ((editor *editor*)
         (rule (buffer-place-for buffer))
         (place (if rule (buffer-place-place rule) :other))
         (selected (editor-window editor)))
    (fit-windows editor)
    (when (and (eq place :here)
               (or other (and (tuatara.display:window-kept selected) (not (one-window-p editor))
                              (not (buffer-place-fits-p (tuatara.display:window-kept selected) buffer)))))
      (setf place :other))
    (let ((window
            (case place
              (:nowhere nil)
              (:here (show-in-window selected buffer rule))
              (t
               (let ((windows (editor-windows editor)))
                 (or (let ((showing (find buffer windows :key #'tuatara.display:window-buffer)))
                       (and showing (not (and other (eq showing selected))) showing))
                     (let ((kept (and rule (buffer-place-keep rule)
                                      (find rule windows :key #'tuatara.display:window-kept))))
                       (and kept (show-in-window kept buffer rule)))
                     (let ((new (new-window-for buffer place rule)))
                       (when new
                         (setf (tuatara.display:window-quit-restore new) :delete)
                         (when (and rule (buffer-place-keep rule))
                           (setf (tuatara.display:window-kept new) rule))
                         new))
                     (let ((spare (remove-if (lambda (w) (or (eq w selected) (tuatara.display:window-kept w))) windows)))
                       (and spare
                            (show-in-window (reduce (lambda (a b)
                                                      (if (< (tuatara.display:window-use-time b) (tuatara.display:window-use-time a)) b a))
                                                    spare)
                                            buffer rule)))
                     (progn (setf (tuatara.display:window-kept selected) nil)
                            (show-in-window selected buffer rule))))))))
      (when (and window rule (buffer-place-select rule))
        (select-window window editor))
      (when window
        (case at
          (:start (setf (tuatara.display:window-point window) 0
                        (tuatara.display:window-start window) 0))
          (:end (setf (tuatara.display:window-point window) (tuatara.buffer:buffer-size buffer)))))
      (fit-windows editor)
      window)))

(defun display-buffer-at (buffer place &key size)
  "Show BUFFER in PLACE whatever the rules of PLACE-BUFFERS say: :here, the
selected window; :bottom, :top, :left or :right, a new window across the
whole bottom or top, or down the whole left or right side, SIZE big as those
rules take it (half of what is split when not given), which quitting closes.
Returns the window, or NIL when there is no room for a new one."
  (unless (live-buffer-p buffer)
    (error "~a has been killed" (tuatara.buffer:buffer-name buffer)))
  (unless (member place '(:here :bottom :top :left :right))
    (error "~s is no place for it; use :here, :bottom, :top, :left or :right." place))
  (let ((editor *editor*))
    (fit-windows editor)
    (let ((window (if (eq place :here)
                      (show-in-window (editor-window editor) buffer nil)
                      (let ((new (new-window-for buffer place (make-buffer-place '() place size nil nil))))
                        (when new (setf (tuatara.display:window-quit-restore new) :delete))
                        new))))
      (fit-windows editor)
      window)))

(defun pop-to-buffer (buffer &key other)
  "Show BUFFER as DISPLAY-BUFFER does (never in the selected window if another
will do, with OTHER true) and select its window. Returns the window."
  (let ((window (display-buffer buffer :other other)))
    (when window (select-window window))
    window))

(defun leave-window (&optional (window (editor-window *editor*)))
  "Put WINDOW back as it was before its buffer was shown in it: close it when
it was made for the buffer or is kept for buffers like it, else show the
buffer it showed before."
  (let ((restore (tuatara.display:window-quit-restore window)))
    (if (and (tuatara.display:area-parent window)
             (or (eq restore :delete) (tuatara.display:window-kept window)))
        (remove-window window)
        (progn
          (setf (tuatara.display:window-quit-restore window) nil
                (tuatara.display:window-kept window) nil)
          (tuatara.display:set-window-buffer window
                                             (if (and (typep restore 'tuatara.buffer:buffer) (live-buffer-p restore)
                                                      (not (eq restore (tuatara.display:window-buffer window))))
                                                 restore
                                                 (replacement-buffer window)))
          (fit-windows *editor*)))))

(defun replace-buffer-in-windows (buffer)
  "BUFFER is being killed: close the windows made or kept for it (all but the
last), show another buffer in the other windows showing it, and forget it in
every window's history. Runs from the kill-buffer functions."
  (setf *buffer-order* (delete buffer *buffer-order*))
  (let ((editor *editor*))
    (when (and editor (editor-window editor))
      (dolist (window (editor-windows editor))
        (tuatara.display:forget-window-buffer window buffer)
        (when (eq (tuatara.display:window-quit-restore window) buffer)
          (setf (tuatara.display:window-quit-restore window) nil)))
      (dolist (window (editor-windows editor))
        (when (eq (tuatara.display:window-buffer window) buffer)
          (if (and (tuatara.display:area-parent window)
                   (or (tuatara.display:window-kept window) (eq (tuatara.display:window-quit-restore window) :delete)))
              (remove-window window)
              (progn
                (setf (tuatara.display:window-kept window) nil
                      (tuatara.display:window-quit-restore window) nil)
                (tuatara.display:set-window-buffer window (replacement-buffer window buffer) :remember nil))))))))

(pushnew 'replace-buffer-in-windows tuatara.buffer:*kill-buffer-functions*)
