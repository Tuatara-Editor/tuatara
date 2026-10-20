;;;; src/display/splits.lisp --- Splits: windows laid out one above another and side by side.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The screen above the echo area is one area: a window, or a split of areas
;;;; one above another or side by side, splits nested in splits. Each area has
;;;; a share of its split, so that the layout keeps its proportions when the
;;;; screen changes size; LAYOUT-AREA turns the shares into rows and columns.

(in-package #:tuatara.display)

(defclass split (area)
  ((arrangement :initarg :arrangement :accessor split-arrangement
                :documentation ":STACKED when the areas in the split are one above another, :SIDE-BY-SIDE when they are side by side.")
   (children :initarg :children :initform '() :accessor split-children
             :documentation "The areas in the split, from the top or from the left."))
  (:documentation "A split of the screen: two areas or more, one above another or side by side."))

(setf (documentation 'split-arrangement 'function) "How SPLIT holds its areas: :STACKED or :SIDE-BY-SIDE."
      (documentation 'split-children 'function) "The areas in SPLIT, from the top or from the left.")

(defvar *window-min-height* 4
  "The fewest rows, its mode line included, a window may be left with when it
is split or made smaller.")

(defvar *window-min-width* 10
  "The fewest columns a window may be left with when it is split or made narrower.")

(defun area-root (area)
  "The area holding AREA and every other: the whole layout."
  (loop while (area-parent area) do (setf area (area-parent area)))
  area)

(defun area-windows (area)
  "The windows in AREA, from the top left: the order \\[other-window] goes through them."
  (if (typep area 'window)
      (list area)
      (loop for child in (split-children area) append (area-windows child))))

(defun arrangement-of (side)
  "The arrangement a new area on SIDE (:above, :below, :left or :right) makes."
  (if (member side '(:above :below)) :stacked :side-by-side))

(defun area-extent (area arrangement)
  "AREA's rows when ARRANGEMENT is :STACKED, else its columns."
  (if (eq arrangement :stacked) (area-height area) (area-width area)))

(defun area-least (area arrangement &optional (least-height 2) (least-width 2))
  "The fewest rows (ARRANGEMENT :STACKED) or columns AREA can do with when each
window in it needs LEAST-HEIGHT rows and LEAST-WIDTH columns."
  (etypecase area
    (window (if (eq arrangement :stacked) least-height least-width))
    (split (let ((parts (mapcar (lambda (child) (area-least child arrangement least-height least-width))
                                (split-children area))))
             (if (eq (split-arrangement area) arrangement)
                 (reduce #'+ parts)
                 (reduce #'max parts))))))

(defun distribute (extent shares leasts)
  "Whole sizes adding up to EXTENT, in proportion to SHARES, each at least its
entry in LEASTS when EXTENT has room for them all. A vector."
  (let* ((n (length shares))
         (sizes (make-array n :initial-element 0))
         (sum 0) (edge 0))
    (loop for share in shares
          for i from 0
          do (incf sum share)
             (let ((next (if (= i (1- n)) extent (round (* extent sum)))))
               (setf (aref sizes i) (- next edge) edge next)))
    (when (>= extent (reduce #'+ leasts))
      (loop for least in leasts
            for i from 0
            do (loop while (< (aref sizes i) least)
                     do (let ((donor (loop with best = nil
                                           for other in leasts
                                           for j from 0
                                           when (and (/= j i) (> (aref sizes j) other)
                                                     (or (null best) (> (- (aref sizes j) other)
                                                                        (- (aref sizes best) (nth best leasts)))))
                                             do (setf best j)
                                           finally (return best))))
                          (unless donor (return))
                          (decf (aref sizes donor))
                          (incf (aref sizes i))))))
    sizes))

(defun layout-area (area left top width height screen-width)
  "Place AREA in the rectangle of WIDTH columns and HEIGHT rows at LEFT and TOP,
and every area in it by its share. A window not at the right edge of the
screen, SCREEN-WIDTH columns, gives its last column to the line dividing it
from the window beside it; its last row is its mode line, and its first its
buffer's header line when it has one (WINDOW-HEADER-ROWS)."
  (setf (area-left area) left (area-top area) top
        (area-width area) width (area-height area) height)
  (etypecase area
    (window
     (let ((divider (< (+ left width) screen-width)))
       (setf (window-divider area) divider
             (window-width area) (max 0 (- width (if divider 1 0)))
             (window-height area) (max 0 (- height 1 (window-header-rows area))))))
    (split
     (let* ((arrangement (split-arrangement area))
            (stacked (eq arrangement :stacked))
            (children (split-children area))
            (sizes (distribute (if stacked height width) (mapcar #'area-share children)
                               (mapcar (lambda (child) (area-least child arrangement)) children)))
            (at (if stacked top left)))
       (loop for child in children
             for size across sizes
             do (if stacked
                    (layout-area child left at width size screen-width)
                    (layout-area child at top size height screen-width))
                (incf at size)))))
  area)

(defun shift-area (area rows)
  "Move AREA and everything in it ROWS rows up."
  (decf (area-top area) rows)
  (when (typep area 'split)
    (dolist (child (split-children area)) (shift-area child rows))))

(defun trim-area-bottom (area rows)
  "Take ROWS from the bottom of AREA, laid out already, for the panel below it:
the windows along its bottom edge give them, the lowest first, each keeping a
row of text and its mode line while the screen has room. Each window notes
the rows of text it had (WINDOW-FULL-HEIGHT)."
  (dolist (window (area-windows area))
    (setf (window-full-height window) (window-height window)))
  (trim-area-rows area rows))

(defun trim-area-rows (area rows)
  "TRIM-AREA-BOTTOM's work: take ROWS from the bottom of AREA."
  (when (plusp rows)
    (let ((rows (min rows (area-height area))))
      (decf (area-height area) rows)
      (etypecase area
        (window (setf (window-height area) (max 0 (- (area-height area) 1 (window-header-rows area)))))
        (split
         (if (eq (split-arrangement area) :side-by-side)
             (dolist (child (split-children area)) (trim-area-rows child rows))
             (let* ((children (split-children area))
                    (gives (make-list (length children) :initial-element 0))
                    (need rows))
               ;; what each gives, from the bottom: first what it has to spare,
               ;; then, when the screen is too small, what it has
               (loop for pass in '(:spare :all)
                     do (loop for child in (reverse children)
                              for i downfrom (1- (length children))
                              while (plusp need)
                              do (let* ((has (- (area-height child) (nth i gives)
                                                (if (eq pass :spare) (area-least child :stacked) 0)))
                                        (give (max 0 (min need has))))
                                   (incf (nth i gives) give)
                                   (decf need give))))
               (let ((shift 0))
                 (loop for child in children
                       for give in gives
                       do (shift-area child shift)
                          (trim-area-rows child give)
                          (incf shift give))))))))))

(defun replace-area (old new)
  "Put area NEW where OLD is in the layout, with OLD's share."
  (let ((parent (area-parent old)))
    (setf (area-parent new) parent
          (area-share new) (area-share old))
    (when parent
      (setf (split-children parent) (substitute new old (split-children parent))))
    (setf (area-parent old) nil)
    new))

(defun split-area (area side new &optional size)
  "Put the window NEW on SIDE of AREA (:above, :below, :left or :right),
taking SIZE rows or columns of AREA, as laid out last (half of them, the
smaller half, when SIZE is NIL). Signals an error when the new window or what
is left of AREA would be smaller than *WINDOW-MIN-HEIGHT* or
*WINDOW-MIN-WIDTH* allow. Returns NEW."
  (let* ((arrangement (arrangement-of side))
         (extent (area-extent area arrangement))
         (size (or size (floor extent 2)))
         (least (if (eq arrangement :stacked) *window-min-height* *window-min-width*)))
    (when (or (< size least)
              (< (- extent size) (area-least area arrangement *window-min-height* *window-min-width*)))
      (error "Not enough room to split the window"))
    (let ((part (/ size extent))
          (parent (area-parent area))
          (after (member side '(:below :right))))
      (if (and parent (eq (split-arrangement parent) arrangement))
          (let ((children (split-children parent))
                (share (* part (area-share area))))
            (setf (area-parent new) parent
                  (area-share new) share
                  (area-share area) (- (area-share area) share)
                  (split-children parent)
                  (loop for child in children
                        if (eq child area)
                          append (if after (list area new) (list new area))
                        else collect child)))
          (let ((split (make-instance 'split :arrangement arrangement)))
            (replace-area area split)
            (setf (area-parent area) split (area-parent new) split
                  (area-share area) (- 1 part) (area-share new) part
                  (split-children split) (if after (list area new) (list new area)))
            ;; the new split takes AREA's place and size until the next layout
            (setf (area-left split) (area-left area) (area-top split) (area-top area)
                  (area-width split) (area-width area) (area-height split) (area-height area)))))
    new))

(defun collapse-split (split)
  "Put the one area left in SPLIT in its place; a split so put in a split
arranged as it is gives its areas to that split."
  (let* ((only (first (split-children split)))
         (parent (area-parent split)))
    (replace-area split only)
    (when (and parent (typep only 'split) (eq (split-arrangement only) (split-arrangement parent)))
      (let ((share (area-share only)))
        (dolist (child (split-children only))
          (setf (area-share child) (* share (area-share child))
                (area-parent child) parent))
        (setf (split-children parent)
              (loop for child in (split-children parent)
                    if (eq child only) append (split-children only) else collect child))))
    only))

(defun delete-area (window)
  "Take WINDOW out of the layout. Its room goes to the area before it in its
split (above or to the left), or when it is first, to the one after it; a
split left holding one area gives way to it. Signals an error for the only
window. Returns the whole layout that remains."
  (let ((parent (area-parent window)))
    (unless parent (error "This is the only window"))
    (let* ((siblings (split-children parent))
           (position (position window siblings))
           (heir (if (plusp position) (nth (1- position) siblings) (nth (1+ position) siblings))))
      (incf (area-share heir) (area-share window))
      (setf (split-children parent) (remove window siblings)
            (area-parent window) nil)
      (when (null (rest (split-children parent)))
        (collapse-split parent))
      (area-root heir))))

(defun window-in-direction (window direction &key (row (area-top window)) (column (area-left window)))
  "The window next to WINDOW in DIRECTION, :left, :right, :above or :below: of
the windows touching that edge of it, the one across screen ROW (to the left
or right) or COLUMN (above or below), else the nearest to it. NIL when WINDOW
is at that edge of the screen."
  (let* ((left (area-left window)) (top (area-top window))
         (right (+ left (area-width window))) (bottom (+ top (area-height window)))
         (across (member direction '(:left :right)))
         (reference (if across row column))
         (best nil) (best-distance nil))
    (dolist (other (area-windows (area-root window)) best)
      (let ((other-left (area-left other)) (other-top (area-top other))
            (other-right (+ (area-left other) (area-width other)))
            (other-bottom (+ (area-top other) (area-height other))))
        (when (and (not (eq other window))
                   (ecase direction
                     (:right (and (= other-left right) (< other-top bottom) (> other-bottom top)))
                     (:left (and (= other-right left) (< other-top bottom) (> other-bottom top)))
                     (:below (and (= other-top bottom) (< other-left right) (> other-right left)))
                     (:above (and (= other-bottom top) (< other-left right) (> other-right left)))))
          (multiple-value-bind (low high) (if across
                                              (values other-top other-bottom)
                                              (values other-left other-right))
            (let ((distance (cond ((< reference low) (- low reference))
                                  ((>= reference high) (1+ (- reference high)))
                                  (t 0))))
              (when (or (null best) (< distance best-distance))
                (setf best other best-distance distance)))))))))

(defun turn-layout (area how)
  "Rearrange the splits in AREA. HOW is :clockwise or :anticlockwise, a quarter
turn of the whole picture; :transpose, rows made columns and columns rows;
:flip-horizontally, mirrored left to right; or :flip-vertically, mirrored top
to bottom. The windows keep their shares. Returns AREA."
  (when (typep area 'split)
    (let ((stacked (eq (split-arrangement area) :stacked)))
      (when (member how '(:clockwise :anticlockwise :transpose))
        (setf (split-arrangement area) (if stacked :side-by-side :stacked)))
      (when (ecase how
              (:clockwise stacked)
              (:anticlockwise (not stacked))
              (:transpose nil)
              (:flip-horizontally (not stacked))
              (:flip-vertically stacked))
        (setf (split-children area) (reverse (split-children area)))))
    (dolist (child (split-children area)) (turn-layout child how)))
  area)

(defun balance-area (area)
  "Give the areas of every split in AREA equal shares."
  (when (typep area 'split)
    (let ((n (length (split-children area))))
      (dolist (child (split-children area))
        (setf (area-share child) (/ 1 n))
        (balance-area child)))))

(defun resize-area (window delta arrangement)
  "Make WINDOW DELTA rows taller (ARRANGEMENT :STACKED) or columns wider
(:SIDE-BY-SIDE), or smaller when DELTA is negative, as laid out last: the rows
or columns come from the areas after it in the nearest split so arranged, then
from those before it, the nearest first, and go to the next area (or the one
before). No window is left smaller than *WINDOW-MIN-HEIGHT* or
*WINDOW-MIN-WIDTH* allow. Returns how many rows or columns it changed by, 0
when there is no room."
  (let ((area window))
    (loop while (and (area-parent area) (not (eq (split-arrangement (area-parent area)) arrangement)))
          do (setf area (area-parent area)))
    (let ((parent (area-parent area)))
      (if (null parent)
          0
          (let* ((children (split-children parent))
                 (extent (area-extent parent arrangement))
                 (sizes (mapcar (lambda (child) (area-extent child arrangement)) children))
                 (leasts (mapcar (lambda (child) (area-least child arrangement *window-min-height* *window-min-width*))
                                 children))
                 (index (position area children))
                 (after (loop for i from (1+ index) below (length children) collect i))
                 (before (loop for i downfrom (1- index) to 0 collect i))
                 (done 0))
            (if (plusp delta)
                (dolist (i (append after before))
                  (let ((give (min (- delta done) (max 0 (- (nth i sizes) (nth i leasts))))))
                    (decf (nth i sizes) give)
                    (incf done give)))
                (let ((give (min (- delta) (max 0 (- (nth index sizes) (nth index leasts))))))
                  (when (and (plusp give) (or after before))
                    (incf (nth (first (or after before)) sizes) give)
                    (setf done (- give)))))
            (unless (zerop done)
              (incf (nth index sizes) done)
              (loop for child in children
                    for size in sizes
                    do (setf (area-share child) (/ size extent))))
            done)))))
