;;;; lisp/org/items.lisp --- Org's list items: checkboxes and their statistics, C-c C-c, C-c -, levels.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-c does what Org's does where point is: on an item with a checkbox
;;;; it ticks it or clears it, the statistics cookies above ([1/3], [33%]) and
;;;; the checkbox of the item above ([-] when some of its items are ticked)
;;;; brought up to date; on a cookie it brings that one up to date. (The <s
;;;; TAB templates are snippets, fold.lisp.) Items are set in and out a level
;;;; as Org's org-indent-item and org-outdent-item set them, by M-l and M-h
;;;; (tables.lisp), and by TAB on an empty item; M-j and M-k move an item
;;;; past its siblings.

(in-package #:tuatara.defaults)

;;; Checkboxes and statistics

(defun org-checkbox-position (item text)
  "Where ITEM's checkbox is in TEXT: the [ of its [ ], [X] or [-] after the bullet, or NIL."
  (when (org-property item :checkbox)
    (search "[" text :start2 (org-item-after-bullet item text) :end2 (string-line-end text (org-node-begin item)))))

(defun org-child-items (node)
  "The items right under NODE: those of the plain lists it holds (an item's own, or a section's)."
  (loop for child in (org-node-children node)
        when (eq (org-node-type child) 'plain-list)
          append (org-children-of child 'item)
        when (eq (org-node-type child) 'section)
          append (org-child-items child)))

(defun org-checkbox-state (item &optional states)
  "The state of ITEM's checkbox, OFF, TRANS or ON, or NIL when it has none: as
STATES gives it when it names ITEM, else as the parse read it. STATES is an
alist from items to the states an edit about to be made gives them."
  (let ((given (assoc item states :test #'eq)))
    (if given (cdr given) (org-property item :checkbox))))

(defun org-checkbox-text (state)
  "The checkbox STATE, OFF, TRANS or ON, as written: [ ], [-] or [X]."
  (ecase state (off "[ ]") (trans "[-]") (on "[X]")))

(defun org-checkbox-counts (node &optional states)
  "How many of the items right under NODE have a checkbox, and how many of
those are ticked, as two values; the states of STATES (ORG-CHECKBOX-STATE)
counted in place of those read."
  (let ((boxes 0) (ticked 0))
    (dolist (item (org-child-items node))
      (let ((state (org-checkbox-state item states)))
        (when state
          (incf boxes)
          (when (eq state 'on) (incf ticked)))))
    (values boxes ticked)))

(defun org-headline-todo-counts (headline)
  "How many of the headlines right under HEADLINE have a TODO keyword, and how
many of those are done, as two values."
  (let ((total 0) (done 0))
    (dolist (child (org-node-children headline))
      (when (and (eq (org-node-type child) 'headline) (org-property child :todo-keyword))
        (incf total)
        (when (eq (org-property child :todo-type) 'done) (incf done))))
    (values total done)))

(defun org-cookie-counts (node &optional (added 0) states)
  "What the statistics cookies of NODE, an item or a headline, count, as two
values, all and done: a headline's TODO sub-headlines, or with none, the
checkboxes right under it; an item's checkboxes right under it. ADDED
checkboxes, not ticked, are counted beside those, for an item being put in,
and the checkboxes of STATES as it gives them (ORG-CHECKBOX-STATE)."
  (multiple-value-bind (total done) (if (eq (org-node-type node) 'headline)
                                        (org-headline-todo-counts node)
                                        (values 0 0))
    (if (plusp total)
        (values total done)
        (multiple-value-bind (boxes ticked) (org-checkbox-counts node states)
          (values (+ boxes added) ticked)))))

(defun org-cookie-text (value done total)
  "A statistics cookie written as VALUE is, a share ([66%]) or a count
\([2/3]), for DONE of TOTAL."
  (if (find #\% value)
      (format nil "[~d%]" (if (zerop total) 0 (round (* 100 done) total)))
      (format nil "[~d/~d]" done total)))

(defun org-cookies-of (node text)
  "The statistics cookies on NODE's own line (a headline's title, an item's
first line), TEXT the text NODE was parsed of."
  (let ((objects (if (eq (org-node-type node) 'headline)
                     (org-property node :title)
                     (let ((paragraph (first (org-node-children node))))
                       (and paragraph (eq (org-node-type paragraph) 'paragraph)
                            (let ((line-end (string-line-end text (org-node-begin node))))
                              (remove-if (lambda (o) (>= (org-node-begin o) line-end))
                                         (org-node-children paragraph))))))))
    (remove-if-not (lambda (object) (eq (org-node-type object) 'statistics-cookie)) objects)))

(defun org-cookie-edits (node text done total)
  "The edits, as a list of (FROM TO NEW), that make the statistics cookies of
NODE, of TEXT, say DONE of TOTAL, leaving out those that already do."
  (loop for cookie in (org-cookies-of node text)
        for value = (org-property cookie :value)
        for new = (org-cookie-text value done total)
        unless (string= new value)
          collect (list (org-node-begin cookie) (+ (org-node-begin cookie) (length value)) new)))

(defun org-statistics-edits (text start &key (added 0) states)
  "The edits that bring up to date the cookies and checkbox of START, a node
of TEXT, and of each item and the headline above it, as a list of (FROM TO
NEW), characters of TEXT. ADDED checkboxes, not ticked, are counted under the
first item or headline met, for items being put in there. STATES, from items
to the checkbox states the edit gives them (ORG-CHECKBOX-STATE), are counted
in place of those read; with the states these edits give the items above, they
are the second value. The parse is left as it was read."
  (let ((edits '()))
    (loop for node = start then (org-node-parent node)
          while node
          when (member (org-node-type node) '(item headline))
            do (multiple-value-bind (boxes ticked) (org-cookie-counts node added states)
                 (setf added 0)
                 (setf edits (append (org-cookie-edits node text ticked boxes) edits))
                 (when (and (plusp boxes) (eq (org-node-type node) 'item) (org-checkbox-state node states))
                   (let ((at (org-checkbox-position node text)))
                     (when at
                       (let ((state (cond ((= ticked boxes) 'on) ((zerop ticked) 'off) (t 'trans))))
                         (push (cons node state) states)
                         (push (list at (+ at 3) (org-checkbox-text state)) edits))))))
          when (eq (org-node-type node) 'headline) do (loop-finish))
    (values edits states)))

(defun org-apply-edits (parse edits &key point final)
  "Make EDITS, (FROM TO NEW) of the characters of PARSE (an ORG-PARSE), in its
buffer in one step to undo, each its own edit of the text, so that what stands
between them (folds, the clock's line, the windows' points) stays where it is.
Then point at the character POINT, moved past the edits before it, or, with
FINAL, taken as it is in the text after. In a buffer whose lines end with a
carriage return and a newline, each newline put in goes in as both."
  (let ((edits (stable-sort (remove-if (lambda (edit)
                                         (and (= (first edit) (second edit)) (zerop (length (third edit))))) edits)
                            #'< :key #'first)))
    (when edits
      (let ((buffer (org-parse-buffer parse))
            (shift 0))
        (when (and point (not final))
          (dolist (edit edits)
            (destructuring-bind (start end new) edit
              (when (<= end point) (incf shift (- (length new) (- end start)))))))
        (tuatara.buffer:buffer-edit
         buffer
         (coerce (loop for (start end new) in edits
                       collect (let ((octets (tuatara.text:string-octets (text-for-buffer new buffer))))
                                 (tuatara.text:make-edit (org-parse-octet parse start) (org-parse-octet parse end)
                                                         (if (plusp (length octets)) octets nil))))
                 'vector))
        (when point
          (setf (tuatara.buffer:buffer-point buffer) (org-parse-octet parse (+ point shift))))))))

(defun org-set-checkbox (parse position item state)
  "Set ITEM's checkbox, of PARSE (an ORG-PARSE, POSITION point as a character
of it), to STATE, OFF, TRANS or ON, written [ ], [-] or [X], and bring up to
date what depends on it: the counts above, the boxes of the items around and
the task's keyword. The parse is left as it was read, the edit refused or not."
  (let* ((text (org-parse-text parse))
         (at (org-checkbox-position item text))
         (old (org-property item :checkbox)))
    (org-apply-edits parse (append (list (list at (+ at 3) (org-checkbox-text state)))
                                   (org-statistics-edits text (org-node-parent item) :states (list (cons item state)))
                                   (org-checkbox-state-edits item text old state))
                     :point position)
    (org-summary-todo (org-parse-buffer parse) (org-parse-octet parse (org-node-begin item)))))

(defun org-toggle-checkbox (parse position item)
  "Tick ITEM's checkbox, of PARSE (POSITION point as a character of it), or
clear it, and bring up to date what depends on it."
  (org-set-checkbox parse position item (if (eq (org-property item :checkbox) 'on) 'off 'on)))

(define-setting *org-return-cycles-checkboxes* t
  "Whether RET on a list item's line in NORMAL state cycles its checkbox.
- t: \\<normal>\\[tuatara-normal-return] in NORMAL state anywhere on a list item's line
  with a checkbox cycles it, [ ] to [-] to [X] and round, the counts above
  brought up to date (a link point is on is followed first).
- nil: the checkbox is left to \\[org-act-here]."
  :values boolean)

(defparameter *org-checkbox-cycle* '((off . trans) (trans . on) (on . off))
  "How \\<normal>\\[tuatara-normal-return] cycles a checkbox: for each state, the state after it.")

(defun org-cycle-checkbox (parse position item)
  "Turn ITEM's checkbox, of PARSE (POSITION point as a character of it), to the
next of *ORG-CHECKBOX-CYCLE*, and bring up to date what depends on it."
  (org-set-checkbox parse position item (cdr (assoc (org-property item :checkbox) *org-checkbox-cycle*))))

(defun org-checkbox-return (buffer)
  "\\<normal>\\[tuatara-normal-return] in an Org buffer on a list item's line with a checkbox:
cycle it (*ORG-RETURN-CYCLES-CHECKBOXES*); true when it did."
  ;; the file parsed only for a line that holds a checkbox
  (when (and *org-return-cycles-checkboxes* (typep (buffer-major-mode buffer) 'org-mode)
             (org-checkbox-line-p (buffer-line-head buffer (tuatara.buffer:buffer-point buffer))))
    (check-writable buffer)
    (let* ((parse (org-parse-now buffer))
           (position (and parse (org-parse-point parse)))
           (item (and parse (org-item-on-line (org-parse-tree parse)
                                              (string-line-start (org-parse-text parse) position)))))
      (when (and item (org-property item :checkbox))
        (org-cycle-checkbox parse position item)
        t))))

(define-command org-rule-or-bullet ()
  "Do what Org's C-c - does where point is: in a table, a rule line below the
row (org-table-insert-hline); in a list, the next kind of bullet
\(org-cycle-list-bullet)."
  (let ((line (buffer-line-head (current-buffer) (point))))
    (if (uiop:string-prefix-p "|" (string-left-trim *org-blanks* line))
        (org-table-insert-hline)
        (org-cycle-list-bullet))))

(defparameter *org-list-bullet-cycle* '("-" "+" "*" "1." "1)")
  "The bullets \\[org-cycle-list-bullet] goes through, in order: a * only for a
list set in from the margin, where it cannot be a headline.")

(define-command org-cycle-list-bullet ()
  "Give the list point is in its next kind of bullet (\\`C-c -'): - to + to *
to 1. to 1) and round, every item of the list at once, numbered items counted
from 1; * is passed over for a list at the margin. Checkboxes and text are
kept."
  (with-org-structure (parse buffer tree text position)
    (let* ((item (org-current-item tree text position))
           (list (and item (org-node-parent item))))
      (if (null list)
          (message "Not in a list")
          (let* ((items (org-children-of list 'item))
                 (indent (org-leading-blanks text (org-node-begin (first items))))
                 (current (string-trim " " (org-property (first items) :bullet)))
                 (kind (cond ((cl-ppcre:scan "^[0-9]+\\.$" current) "1.")
                             ((cl-ppcre:scan "^[0-9]+\\)$" current) "1)")
                             (t current)))
                 (choices (if (zerop indent)
                              (remove "*" *org-list-bullet-cycle* :test #'string=)
                              *org-list-bullet-cycle*))
                 (next (or (second (member kind choices :test #'string=)) (first choices))))
            (org-apply-edits
             parse
             (loop for each in items
                   for n from 1
                   for begin = (org-node-begin each)
                   for at = (+ begin (org-leading-blanks text begin))
                   for old = (string-trim " " (org-property each :bullet))
                   collect (list at (+ at (length old))
                                 (if (digit-char-p (char next 0)) (format nil "~d~a" n (subseq next 1)) next)))
             :point position)
            (message "Bullets: ~a" next))))))

;;; Items set in and out

(defun org-bullet-after (bullet)
  "The bullet of the item after one whose bullet is BULLET, blanks trimmed:
the next number for a numbered one, its . or ) kept; BULLET itself otherwise."
  (cl-ppcre:regex-replace "^(\\d+)" (string-trim *org-blanks* bullet)
                          (lambda (match number)
                            (declare (ignore match))
                            (princ-to-string (1+ (parse-integer number))))
                          :simple-calls t))

(defun org-outermost-list (item)
  "The plain list holding ITEM that no item holds: the whole list ITEM is in,
its sub-lists with it."
  (let ((list (org-node-parent item)))
    (loop for above = (org-node-parent list)
          while (and above (eq (org-node-type above) 'item))
          do (setf list (org-node-parent above)))
    list))

(defun org-list-entries (items &optional parent)
  "ITEMS and every item under them, in order, each as (ITEM . PARENT): PARENT
the item it is right under, PARENT for those of ITEMS."
  (loop for item in items
        collect (cons item parent)
        append (org-list-entries (org-child-items item) item)))

(defun org-item-bullet (item)
  "ITEM's bullet, its blanks trimmed: -, +, *, or a number and . or )."
  (string-trim *org-blanks* (org-property item :bullet)))

(defun org-list-new-parents (entries targets by)
  "Where each item of ENTRIES (ORG-LIST-ENTRIES of a whole list) stands when
the items TARGETS holds (a hash table) are set in a level, BY 1, or out, BY
-1: a hash table of each item's new parent, NIL at the top.
Set in, an item goes under the sibling before it that is not moved, the items
right under it staying at their depth. Set out, it goes beside its parent,
the siblings after it going under it. A string saying why when they cannot
move."
  (let ((old (make-hash-table :test 'eq))
        (new (make-hash-table :test 'eq))
        (children (make-hash-table :test 'eq))
        (left '()))
    (loop for (item . parent) in entries
          do (setf (gethash item old) parent (gethash item new) parent)
             (push item (gethash (or parent :top) children)))
    (maphash (lambda (parent items) (setf (gethash parent children) (nreverse items))) children)
    (loop for (item . parent) in entries
          when (and (gethash item targets) (not (and parent (gethash parent targets))))
            do (if (plusp by)
                   (let ((before (loop with found = nil
                                       for sibling in (gethash (or parent :top) children)
                                       until (eq sibling item)
                                       unless (gethash sibling targets) do (setf found sibling)
                                       finally (return found))))
                     (unless before
                       (return-from org-list-new-parents "Cannot indent the first item of a list"))
                     (setf (gethash item new) before)
                     (dolist (child (gethash item children))
                       (unless (gethash child targets) (setf (gethash child new) before))))
                   (progn
                     (when (find-if-not (lambda (child) (gethash child targets)) (gethash item children))
                       (return-from org-list-new-parents "Cannot outdent an item without its children"))
                     (unless parent
                       (return-from org-list-new-parents "Cannot outdent top-level items"))
                     (setf (gethash item new) (gethash parent old))
                     (pushnew parent left))))
    ;; the siblings after an item set out go under it
    (dolist (parent left)
      (let ((out nil))
        (dolist (sibling (gethash parent children))
          (cond ((not (eq (gethash sibling new) parent)) (setf out sibling))
                (out (setf (gethash sibling new) out))))))
    new))

(defun org-list-new-bullets (entries parents)
  "The bullets of the items of ENTRIES (ORG-LIST-ENTRIES) once each stands
under its parent of PARENTS (ORG-LIST-NEW-PARENTS), as a hash table: in each
list whose items changed, the first numbered from 1 and each after it the
bullet of the one before it; elsewhere the bullets they have."
  (let ((old (make-hash-table :test 'eq))
        (new (make-hash-table :test 'eq))
        (bullets (make-hash-table :test 'eq)))
    (loop for (item . parent) in entries
          do (push item (gethash (or parent :top) old))
             (push item (gethash (or (gethash item parents) :top) new))
             (setf (gethash item bullets) (org-item-bullet item)))
    (maphash (lambda (parent items)
               (let ((items (reverse items)))
                 (unless (equal items (reverse (gethash parent old)))
                   (loop for (item . more) on items
                         for bullet = (cl-ppcre:regex-replace "^\\d+" (gethash item bullets) "1")
                           then (org-bullet-after bullet)
                         do (setf (gethash item bullets) bullet)))))
             new)
    bullets))

(defun org-list-edits (entries text columns bullets)
  "The edits, (FROM TO NEW) of TEXT, giving each item of ENTRIES
\(ORG-LIST-ENTRIES) the column COLUMNS has for it and the bullet BULLETS has
\(hash tables), and moving the lines of its own text as far as its text
moved; at the margin a * bullet, which would make a headline, becomes -."
  (loop for (item) in entries
        for begin = (org-node-begin item)
        for old-column = (org-blank-columns text begin)
        for old-bullet = (org-item-bullet item)
        for column = (gethash item columns old-column)
        for bullet = (let ((bullet (gethash item bullets old-bullet)))
                       (if (and (zerop column) (string= bullet "*")) "-" bullet))
        for by = (- (+ column (length bullet)) (+ old-column (length old-bullet)))
        unless (and (= column old-column) (string= bullet old-bullet))
          collect (list begin (org-item-after-bullet item text) (concatenate 'string (spaces column) bullet))
        unless (zerop by)
          append (loop with end = (org-item-own-end item)
                       for line = (1+ (string-line-end text begin)) then (1+ (string-line-end text line))
                       while (< line end)
                       unless (string= "" (string-trim *org-blanks* (subseq text line (string-line-end text line))))
                         collect (multiple-value-bind (blanks after) (org-blank-columns text line)
                                   (list line after (spaces (max 0 (+ blanks by))))))))

(defun org-list-shift-edits (text entries targets by)
  "The edits, (FROM TO NEW) of TEXT, setting the items TARGETS holds (a hash
table) of the whole list whose items are ENTRIES (ORG-LIST-ENTRIES) in a
level, BY 1, or out, BY -1. Each item that
moves, and each whose place changed with it, goes to the column of the item
before it in its new list, or, the first there, where the text of the item it
stands under begins. A string saying why when they cannot move."
  (let ((parents (org-list-new-parents entries targets by)))
    (if (stringp parents)
        parents
        (let ((bullets (org-list-new-bullets entries parents))
              (columns (make-hash-table :test 'eq))
              (moved (make-hash-table :test 'eq))
              (last-under (make-hash-table :test 'eq))
              (top (org-blank-columns text (org-node-begin (car (first entries))))))
          (loop for (item . old) in entries
                for parent = (gethash item parents)
                for before = (gethash (or parent :top) last-under)
                for old-column = (org-blank-columns text (org-node-begin item))
                for column = (cond ((not (or (gethash item targets) (not (eq parent old))
                                             (and parent (gethash parent moved))))
                                    old-column)
                                   (before (gethash before columns))
                                   (parent (+ (gethash parent columns) (length (gethash parent bullets)) 1))
                                   (t top))
                do (setf (gethash item columns) column
                         (gethash (or parent :top) last-under) item)
                   (unless (and (= column old-column) (= (length (gethash item bullets))
                                                         (length (org-item-bullet item))))
                     (setf (gethash item moved) t)))
          (org-list-edits entries text columns bullets)))))

(defun org-list-moved-whole-edits (text entries by)
  "The edits, (FROM TO NEW) of TEXT, moving the whole list whose items are
ENTRIES (ORG-LIST-ENTRIES) a column in, BY 1, or out, BY -1, all its lines
with it; a string saying why when it is at the margin already."
  (if (and (minusp by) (zerop (org-blank-columns text (org-node-begin (car (first entries))))))
      "Cannot outdent beyond margin"
      (let ((columns (make-hash-table :test 'eq)))
        (loop for (item) in entries
              do (setf (gethash item columns) (max 0 (+ (org-blank-columns text (org-node-begin item)) by))))
        (org-list-edits entries text columns (make-hash-table :test 'eq)))))

(defun org-items-to-shift (parse)
  "The list items of PARSE that \\[org-demote] and the like move, as a list,
and whether they were selected as a second value: those whose first line is
among the selection's lines; else the innermost item point is in. NIL when
the selection begins on a headline or none is there."
  (let* ((buffer (org-parse-buffer parse))
         (tree (org-parse-tree parse))
         (text (org-parse-text parse))
         (region (selected-region buffer)))
    (if region
        (let ((from (string-line-start text (org-parse-char parse (first region))))
              (to (org-parse-char parse (second region)))
              (found '()))
          (labels ((walk (node)
                     (when (and (eq (org-node-type node) 'item) (<= from (org-node-begin node)))
                       (push node found))
                     (org-children-in-range node from to #'walk)))
            (unless (org-headline-on-line tree text from)
              (walk tree)))
          (values (nreverse found) t))
        (let ((item (org-current-item tree text (org-parse-point parse))))
          (and item (list item))))))

(defun org-shift-items (by subtree)
  "Set the list item point is in a level in, BY 1, or out, BY -1, or every
item the selection's lines begin; with SUBTREE, the items under each with
it. At the first item of a list, with SUBTREE the whole list moves a column.
True when there were items to move, whether they moved or why not was said;
NIL elsewhere, for a headline's command to run."
  (let ((parse (org-parse-now (current-buffer))))
    (multiple-value-bind (items selected) (and parse (org-items-to-shift parse))
      (when items
        (check-writable (current-buffer))
        (let ((text (org-parse-text parse))
              (targets (make-hash-table :test 'eq))
              (edits '()))
          (dolist (entry (org-list-entries items))
            (when (or subtree (member (car entry) items)) (setf (gethash (car entry) targets) t)))
          (dolist (list (remove-duplicates (mapcar #'org-outermost-list items)))
            (let* ((entries (org-list-entries (org-children-of list 'item)))
                   (first-p (and (not selected) (eq (first items) (car (first entries)))))
                   (found (cond ((and first-p subtree) (org-list-moved-whole-edits text entries by))
                                (first-p (concatenate 'string "At first item: use \\[org-demote-subtree] or "
                                                      "\\[org-promote-subtree] to move the whole list"))
                                (t (org-list-shift-edits text entries targets by)))))
              (when (stringp found)
                (message found)
                (return-from org-shift-items t))
              (setf edits (append found edits))))
          (org-apply-edits parse edits :point (org-parse-point parse))
          t)))))

(defun org-move-item (direction)
  "Move the list item point is in, with the items under it, over its sibling
in DIRECTION, :up or :down, point going with it; numbered, the two keep the
numbers of their places. True when point was in an item, whether it moved or
why not was said; NIL elsewhere, for a headline's command to run."
  (let* ((parse (org-parse-now (current-buffer)))
         (text (and parse (org-parse-text parse)))
         (item (and parse (org-current-item (org-parse-tree parse) text (org-parse-point parse)))))
    (when item
      (let* ((siblings (org-children-of (org-node-parent item) 'item))
             (index (position item siblings))
             (other (if (eq direction :down) (nth (1+ index) siblings) (and (plusp index) (nth (1- index) siblings)))))
        (if (null other)
            (message "Cannot move past the ~:[first~;last~] item of the list" (eq direction :down))
            (let* ((first (if (eq direction :down) item other))
                   (second (if (eq direction :down) other item))
                   (numbered (every (lambda (node) (digit-char-p (char (org-item-bullet node) 0)))
                                    (list first second))))
              (labels ((end (node) (org-last-content-end text (org-node-begin node) (org-node-end node)))
                       (written (node bullet)
                         ;; NODE's text, its bullet BULLET when numbered
                         (let* ((begin (org-node-begin node))
                                (at (+ begin (org-leading-blanks text begin))))
                           (concatenate 'string (subseq text begin at)
                                        (if numbered bullet (org-item-bullet node))
                                        (subseq text (+ at (length (org-item-bullet node))) (end node))))))
                (check-writable (current-buffer))
                (let* ((ahead (written second (org-item-bullet first)))
                       (gap (subseq text (end first) (org-node-begin second)))
                       (from (org-node-begin first))
                       (offset (- (org-parse-point parse) (org-node-begin item))))
                  (org-apply-edits parse
                                   (list (list from (end second)
                                               (concatenate 'string ahead gap
                                                            (written first (org-item-bullet second)))))
                                   :point (+ (if (eq item first) (+ from (length ahead) (length gap)) from) offset)
                                   :final t)))))
        t))))

(defun org-item-may-go-out-p (item)
  "True when \\`TAB' may set the empty list ITEM out a level: it stands under
an item, the last of its list, with no item under it."
  (let ((above (org-node-parent (org-node-parent item))))
    (and above (eq (org-node-type above) 'item)
         (eq item (car (last (org-children-of (org-node-parent item) 'item))))
         (null (org-child-items item)))))

(defun org-shift-item (parse item by)
  "Set ITEM of PARSE (an ORG-PARSE) alone a level in, BY 1, or out, BY -1, in
one step to undo, point kept on its character; true when it could move."
  (let ((targets (make-hash-table :test 'eq)))
    (setf (gethash item targets) t)
    (let ((edits (org-list-shift-edits (org-parse-text parse)
                                       (org-list-entries (org-children-of (org-outermost-list item) 'item))
                                       targets by)))
      (unless (stringp edits)
        (org-apply-edits parse edits :point (org-parse-point parse))
        t))))

(defun org-cycle-item-indentation (parse)
  "\\`TAB' on an empty list item of PARSE, one with nothing after its bullet
and its checkbox: set it in a level, then on each \\`TAB' after it further in
while it can go, then out a level at a time, and then back where it was.
True when point was on such an item."
  (let* ((buffer (org-parse-buffer parse))
         (text (org-parse-text parse))
         (start (string-line-start text (org-parse-point parse)))
         (end (string-line-end text start))
         (item (org-item-on-line (org-parse-tree parse) start))
         (state (tuatara.buffer:buffer-local buffer 'org-item-cycle))
         (going (and item state (eq (editor-last-command *editor*) 'org-cycle)
                     (eql (getf state :version) (tuatara.text:version-number (tuatara.buffer:buffer-version buffer))))))
    (when (and item (or going (and (cl-ppcre:scan *org-empty-item-scanner* text :start start :end end)
                                   (string= "" (string-trim '(#\Space #\Tab #\Newline)
                                                            (subseq text end (org-node-end item)))))))
      (check-writable buffer)
      (let ((column (org-blank-columns text start))
            (first-p (eq item (first (org-children-of (org-node-parent item) 'item)))))
        (flet ((move (by &optional (began state))
                 (org-shift-item parse item by)
                 (setf (tuatara.buffer:buffer-local buffer 'org-item-cycle)
                       (list* :version (tuatara.text:version-number (tuatara.buffer:buffer-version buffer)) began))))
          (cond ((not going)
                 (let ((began (list :column column)))
                   (cond ((not first-p) (move 1 began))
                         ((org-item-may-go-out-p item) (move -1 began))
                         (t (setf (tuatara.buffer:buffer-local buffer 'org-item-cycle) nil)
                            (message "Cannot move item")))))
                ((and (> column (getf state :column)) (not first-p)) (move 1))
                ((and (< column (getf state :column)) (org-item-may-go-out-p item)) (move -1))
                (t
                 ;; back where it began a level at a time, the list numbered again; from there out a level
                 ;; when it was going in, else the cycle is over
                 (loop with began = (getf state :column)
                       repeat 16
                       for now = parse then (org-parse-now buffer)
                       for here = (org-item-on-line (org-parse-tree now) start)
                       for at = (org-blank-columns (org-parse-text now) start)
                       while (and here (/= at began) (org-shift-item now here (if (> at began) -1 1))))
                 (setf (tuatara.buffer:buffer-local buffer 'org-item-cycle) nil)
                 (let* ((again (and (> column (getf state :column)) (org-parse-now buffer)))
                        (back (and again (org-item-on-line (org-parse-tree again) start))))
                   (when (and back (org-item-may-go-out-p back))
                     (setf parse again item back)
                     (move -1)))))))
      t)))

(defun org-update-cookie (parse position cookie)
  "Bring COOKIE, of PARSE (an ORG-PARSE), up to date with what the item or
headline it is on counts (`org-cookie-counts'), point left at POSITION."
  (let ((owner (loop for node = (org-node-parent cookie) then (org-node-parent node)
                     while node
                     when (member (org-node-type node) '(item headline)) return node)))
    (when owner
      (multiple-value-bind (total done) (org-cookie-counts owner)
        (let ((value (org-property cookie :value)))
          (org-apply-edits parse (list (list (org-node-begin cookie) (+ (org-node-begin cookie) (length value))
                                             (org-cookie-text value done total)))
                           :point position))))))

(defun org-cookie-at (tree position)
  "The statistics cookie of TREE holding the character POSITION, or NIL."
  (org-node-at tree position
               (lambda (node) (and (eq (org-node-type node) 'statistics-cookie) (org-before-blank-p node position)))
               :titles t))

(define-command org-act-here ()
  "Do what Org's C-c C-c does where point is: on a dynamic block's #+BEGIN:
line, make the block again (\\[org-dblock-update]). On an item with a checkbox,
tick it or clear it, the counts above brought up to date. On a statistics
cookie, bring it up to date. In a table, align it. On its #+TBLFM: line, work
its formulas out. On a CLOCK: line, write its duration again."
  (with-org-structure (parse buffer tree text position :part t)
    (let ((cookie (org-cookie-at tree position))
          (item (org-current-item tree text position)))
      (cond ((let ((block (dblock-at tree text position)))
               ;; on a dynamic block's #+BEGIN: line: the block made again, from the whole note
               (when (and block (<= (first block) position (string-line-end text (first block))))
                 (check-writable buffer)
                 (let ((whole (org-whole-parse parse)))
                   (dblock-update whole (dblock-at (org-parse-tree whole) (org-parse-text whole)
                                                   (org-parse-point whole))))
                 t)))
            ((org-recalculate-from-tblfm parse position))
            ((org-update-clock-line parse position))
            ((org-table-at tree text position) (org-table-align))
            (cookie (org-update-cookie parse position cookie))
            ((and item (org-property item :checkbox)) (org-toggle-checkbox parse position item))
            (item (message "This item has no checkbox: C-c C-c ticks one ([ ])"))
            (t (message "Nothing for C-c C-c to do here"))))))

;;; What C-c's key hints show first

(defun org-src-block-at (tree position)
  "The source block of TREE holding the character POSITION, from its
#+begin_src line to its #+end_src line, or NIL."
  (org-node-at tree position (lambda (node)
                               (and (eq (org-node-type node) 'src-block) (org-before-blank-p node position)))))

(defun org-context-keys (editor prefix)
  "For \\`C-c' in an Org buffer: what the cursor is on, as
*KEY-HINT-FOCUS-FUNCTIONS* has it, the keys that act on it first, be it a link,
a table, a source block, a list item or a heading; nothing while the parse
of the part point is in is behind its text."
  (declare (ignore editor))
  (let ((buffer (current-buffer)))
    (when (and (equalp prefix (tuatara.frontend:parse-key-sequence "C-c"))
               (typep (buffer-major-mode buffer) 'org-mode))
      ;; hints from a parse of the text as it is now, of the part point is in in a large note, never waited for
      (let ((parse (or (org-part-parse-at buffer (point))
                       (and (or (<= (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
                                (org-parse-current-p buffer))
                            (org-buffer-parse buffer)))))
        (when (and parse (or (org-parse-view parse) (org-parse-current-p buffer)))
          (let* ((tree (org-parse-tree parse))
                 (text (org-parse-text parse))
                 (position (org-parse-point parse))
                 (line-start (string-line-start text position)))
            (cond ((org-link-at tree position) '("link at point" "C-o" "C-l"))
                  ((org-table-at tree text position) '("table at point" "C-c" "*" "=" "-"))
                  ((org-src-block-at tree position) '("source block at point" "C-v" "C-c"))
                  ((org-current-item tree text position) '("list item at point" "C-c" "-"))
                  ((and (< line-start (length text)) (char= (char text line-start) #\*)
                        (cl-ppcre:scan *org-headline-line-scanner* text :start line-start))
                   '("heading at point" "C-t" "C-q" "C-s" "C-d" "C-w" "C-z" "C-x")))))))))

(define-setting *org-complete-checkboxes* t
  "Whether [ typed right after a list item's bullet makes a checkbox.
- t: [ typed after the bullet and its space (- [) makes it a checkbox at
  once, - [ ] with point after the space, ready for the item's text.
- nil: [ is typed as anywhere else."
  :values boolean)

(defun org-checkbox-typed (buffer char)
  "After CHAR was typed into BUFFER: when it is [ right after a list item's
bullet and space in an Org buffer, nothing after it but the ] that closing
pairs writes, make it the checkbox [ ] and a space, point after them
\(*ORG-COMPLETE-CHECKBOXES*)."
  (when (and *org-complete-checkboxes* (eql char #\[) (typep (buffer-major-mode buffer) 'org-mode))
    (let* ((p (point))
           (line (tuatara.buffer:buffer-line-start buffer p))
           (end (tuatara.buffer:buffer-line-end buffer p))
           (before (tuatara.buffer:buffer-string buffer :start line :end p))
           (after (tuatara.buffer:buffer-string buffer :start p :end end)))
      (when (and (member after '("" "]") :test #'string=)
                 (cl-ppcre:scan (load-time-value (cl-ppcre:create-scanner (format nil "^~a \\[$" *org-bullet-re*)))
                                before))
        (tuatara.buffer:buffer-edit buffer
                                    (vector (tuatara.text:make-edit (1- p) end (tuatara.text:string-octets "[ ] ")))
                                    :group t)
        (goto (+ p 3))))))

(defun install-org-items ()
  "Bind C-c C-c in Org buffers, have C-c's key hints show first the keys for
what point is on (ORG-CONTEXT-KEYS), have typing - [ make a checkbox and RET
in NORMAL state on its line cycle it, and on a headline's line with a TODO
keyword change that (ORG-TODO-RETURN)."
  (define-key (mode-keymap 'org-mode) "C-c C-c" 'org-act-here)
  (pushnew 'org-context-keys *key-hint-focus-functions*)
  ;; after closing pairs, whose ] it takes in
  (setf *post-self-insert-functions*
        (append (remove 'org-checkbox-typed *post-self-insert-functions*) (list 'org-checkbox-typed)))
  ;; after following a link point is on
  (setf *enter-dwim-functions*
        (append (remove-if (lambda (f) (member f '(org-checkbox-return org-todo-return))) *enter-dwim-functions*)
                (list 'org-checkbox-return 'org-todo-return))))

;;; Cookies kept up to date as checkbox items are killed

(defun org-killed-checkbox-items (tree start end)
  "The items with a checkbox of TREE that hold a character from START below
END, each before the items under it."
  (let ((found '()))
    (labels ((walk (node)
               (org-children-in-range node start end
                                      (lambda (child)
                                        (when (and (eq (org-node-type child) 'item) (org-property child :checkbox))
                                          (push child found))
                                        (walk child)))))
      (walk tree))
    (nreverse found)))

(defmethod mode-kill-follow-up ((mode org-mode) buffer start end)
  "Once checkbox items are killed in an Org buffer, the statistics cookies and
checkboxes above them count without them. Done only when the buffer's parse of
its text as it is now is at hand, never waited for."
  (let ((parse (and (or (<= (tuatara.buffer:buffer-size buffer) *org-parse-at-once*) (org-parse-current-p buffer))
                    (org-parse-now buffer))))
    (when parse
      (let* ((text (org-parse-text parse))
             (from (org-parse-char parse start))
             (to (org-parse-char parse end))
             (killed (remove-if-not (lambda (item)
                                      (let ((at (org-checkbox-position item text)))
                                        (and at (< at to) (> (+ at 3) from))))
                                    (org-killed-checkbox-items (org-parse-tree parse) from to)))
             (states (mapcar (lambda (item) (cons item nil)) killed))
             (counted (make-hash-table :test 'eq))
             (edits (make-hash-table)))
        ;; the counts made as if the items were gone, each list once; those under first, so that an item above
        ;; counts the boxes below it as they will be
        (dolist (item (reverse killed))
          (let ((parent (org-node-parent item)))
            (unless (gethash parent counted)
              (setf (gethash parent counted) t)
              (multiple-value-bind (found after) (org-statistics-edits text parent :states states)
                (setf states after)
                (dolist (edit found)
                  (unless (and (< (first edit) to) (> (second edit) from))
                    (setf (gethash (first edit) edits) edit)))))))
        (when (plusp (hash-table-count edits))
          (let ((edits (loop for edit being the hash-values of edits collect edit)))
            (lambda () (org-apply-edits parse edits))))))))
