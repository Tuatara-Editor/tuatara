;;;; lisp/org/items.lisp --- Org's checkboxes and their statistics, C-c C-c and C-c -.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-c does what Org's does where point is: on an item with a checkbox
;;;; it ticks it or clears it, the statistics cookies above ([1/3], [33%]) and
;;;; the checkbox of the item above ([-] when some of its items are ticked)
;;;; brought up to date; on a cookie it brings that one up to date. (The <s
;;;; TAB templates are snippets, fold.lisp.)

(in-package #:tuatara.defaults)

;;; Checkboxes and statistics

(defun org-checkbox-position (item text)
  "Where ITEM's checkbox is in TEXT: the [ of its [ ], [X] or [-] after the bullet, or NIL."
  (when (org-property item :checkbox)
    (let* ((begin (org-node-begin item))
           (line-end (string-line-end text begin))
           (after-bullet (+ begin (- (length (org-line-at text begin)) (length (string-left-trim " 	" (org-line-at text begin))))
                            (length (string-right-trim " " (org-property item :bullet))))))
      (search "[" text :start2 after-bullet :end2 line-end))))

(defun org-child-items (node)
  "The items right under NODE: those of the plain lists it holds (an item's own, or a section's)."
  (loop for child in (org-node-children node)
        when (eq (org-node-type child) 'plain-list)
          append (remove 'item (org-node-children child) :key #'org-node-type :test-not #'eq)
        when (member (org-node-type child) '(section))
          append (org-child-items child)))

(defun org-checkbox-counts (node)
  "How many of the items right under NODE have a checkbox, and how many of those are ticked, as two values."
  (let ((boxes 0) (ticked 0))
    (dolist (item (org-child-items node))
      (when (org-property item :checkbox)
        (incf boxes)
        (when (eq (org-property item :checkbox) 'on) (incf ticked))))
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

(defun org-cookie-counts (node)
  "What the statistics cookies of NODE, an item or a headline, count, as two
values, all and done: a headline's TODO sub-headlines, or with none, the
checkboxes right under it; an item's checkboxes right under it."
  (multiple-value-bind (total done) (if (eq (org-node-type node) 'headline)
                                        (org-headline-todo-counts node)
                                        (values 0 0))
    (if (plusp total)
        (values total done)
        (org-checkbox-counts node))))

(defun org-cookie-text (value done total)
  "A statistics cookie written as VALUE is, a share ([66%]) or a count
\([2/3]), for DONE of TOTAL."
  (if (find #\% value)
      (format nil "[~d%]" (if (zerop total) 0 (round (* 100 done) total)))
      (format nil "[~d/~d]" done total)))

(defun org-cookies-of (node)
  "The statistics cookies on NODE's own line (a headline's title, an item's first line)."
  (let ((objects (if (eq (org-node-type node) 'headline)
                     (org-property node :title)
                     (let ((paragraph (first (org-node-children node))))
                       (and paragraph (eq (org-node-type paragraph) 'paragraph)
                            (let ((line-end (string-line-end *org-current-text* (org-node-begin node))))
                              (remove-if (lambda (o) (>= (org-node-begin o) line-end)) (org-node-children paragraph))))))))
    (remove 'statistics-cookie objects :key #'org-node-type :test-not #'eq)))

(defun org-cookie-edits (node done total)
  "The edits, as a list of (FROM TO NEW), that make the statistics cookies of
NODE say DONE of TOTAL, leaving out those that already do."
  (loop for cookie in (org-cookies-of node)
        for value = (org-property cookie :value)
        for new = (org-cookie-text value done total)
        unless (string= new value)
          collect (list (org-node-begin cookie) (+ (org-node-begin cookie) (length value)) new)))

(defun org-statistics-edits (text item)
  "The edits that bring up to date what depends on ITEM's checkbox: the
checkbox of each item above it and the cookies of it and the headline above,
as a list of (FROM TO NEW), characters of TEXT."
  (let ((edits '()))
    (loop for node = (org-node-parent item) then (org-node-parent node)
          while node
          when (member (org-node-type node) '(item headline))
            do (multiple-value-bind (boxes ticked) (org-cookie-counts node)
                 (when (plusp boxes)
                   (setf edits (append (org-cookie-edits node ticked boxes) edits))
                   (when (and (eq (org-node-type node) 'item) (org-property node :checkbox))
                     (let ((at (org-checkbox-position node text)))
                       (when at
                         (let ((state (cond ((= ticked boxes) "[X]") ((zerop ticked) "[ ]") (t "[-]"))))
                           (setf (getf (org-node-properties node) :checkbox)
                                 (cond ((string= state "[X]") 'on) ((string= state "[ ]") 'off) (t 'trans)))
                           (push (list at (+ at 3) state) edits)))))))
          when (eq (org-node-type node) 'headline) do (loop-finish))
    edits))

(defun org-apply-edits (buffer edits &key point final)
  "Make EDITS, (FROM TO NEW) of BUFFER's characters as parsed, in one step to
undo, each its own edit of the text, so that what stands between them (folds,
the clock's line, the windows' points) stays where it is; then point at the
character POINT, moved past the edits before it, or, with FINAL, taken as it
is in the text after. In a buffer whose lines end with a carriage return and
a newline, each newline put in goes in as both."
  (let ((edits (stable-sort (remove-if (lambda (edit) (and (= (first edit) (second edit)) (zerop (length (third edit))))) edits)
                            #'< :key #'first)))
    (when edits
      (let ((returns (org-returns-of buffer))
            (shift 0))
        (when (and point (not final))
          (dolist (edit edits)
            (destructuring-bind (start end new) edit
              (when (<= end point) (incf shift (- (length new) (- end start)))))))
        (tuatara.buffer:buffer-edit
         buffer
         (coerce (loop for (start end new) in edits
                       collect (let ((octets (tuatara.text:string-octets (text-for-buffer new buffer))))
                                 (tuatara.text:make-edit (org-char->byte buffer start returns) (org-char->byte buffer end returns)
                                                         (if (plusp (length octets)) octets nil))))
                 'vector))
        (when point
          (setf (tuatara.buffer:buffer-point buffer) (org-char->byte buffer (+ point shift))))))))

(defun org-toggle-checkbox (buffer tree text position item)
  "Tick ITEM's checkbox or clear it, and bring up to date what depends on it."
  (declare (ignore tree))
  (let* ((at (org-checkbox-position item text))
         (old (org-property item :checkbox))
         (new (if (eq old 'on) "[ ]" "[X]")))
    (setf (getf (org-node-properties item) :checkbox) (if (string= new "[X]") 'on 'off))
    (org-apply-edits buffer (append (list (list at (+ at 3) new))
                                    (org-statistics-edits text item)
                                    (org-checkbox-state-edits item text old (org-property item :checkbox)))
                     :point position)
    (org-summary-todo buffer (org-node-begin item))))

(defvar *org-return-cycles-checkboxes* t
  "True for \\<normal>\\[tuatara-normal-return] in NORMAL state anywhere on a list item's line
with a checkbox to cycle it, [ ] to [-] to [X] and round, the counts above
brought up to date (a link point is on is followed first); NIL to leave it to
\\[org-ctrl-c-ctrl-c].")

(defparameter *org-checkbox-cycle* '((off "[-]" trans) (trans "[X]" on) (on "[ ]" off))
  "How \\<normal>\\[tuatara-normal-return] cycles a checkbox: for each state, what it is written as next and the state that is.")

(defun org-cycle-checkbox (buffer tree text position item)
  "Turn ITEM's checkbox to the next of *ORG-CHECKBOX-CYCLE*, and bring up to date what depends on it."
  (declare (ignore tree))
  (let* ((at (org-checkbox-position item text))
         (old (org-property item :checkbox))
         (next (rest (assoc old *org-checkbox-cycle*))))
    (setf (getf (org-node-properties item) :checkbox) (second next))
    (org-apply-edits buffer (append (list (list at (+ at 3) (first next)))
                                    (org-statistics-edits text item)
                                    (org-checkbox-state-edits item text old (second next)))
                     :point position)
    (org-summary-todo buffer (org-node-begin item))))

(defun org-checkbox-return (buffer)
  "\\<normal>\\[tuatara-normal-return] in an Org buffer on a list item's line with a checkbox:
cycle it (*ORG-RETURN-CYCLES-CHECKBOXES*); true when it did."
  ;; the file parsed only for a line that holds a checkbox
  (when (and *org-return-cycles-checkboxes* (typep (buffer-major-mode buffer) 'org-mode)
             (org-checkbox-line-p (org-line-head buffer (tuatara.buffer:buffer-point buffer))))
    (check-writable buffer)
    (multiple-value-bind (tree text) (org-tree-now buffer)
      (let* ((*org-current-text* text)
             (position (org-point-char buffer))
             (item (and tree (org-item-on-line tree (string-line-start text position)))))
        (when (and item (org-property item :checkbox))
          (org-cycle-checkbox buffer tree text position item)
          t)))))

(define-command org-ctrl-c-minus ()
  "Do what Org's C-c - does where point is: in a table, a rule line below the
row (org-table-insert-hline); in a list, the next kind of bullet
\(org-cycle-list-bullet)."
  (let ((line (org-line-head (current-buffer) (point))))
    (if (uiop:string-prefix-p "|" (string-left-trim " 	" line))
        (org-table-insert-hline)
        (org-cycle-list-bullet))))

(defparameter *org-list-bullet-cycle* '("-" "+" "*" "1." "1)")
  "The bullets \\[org-cycle-list-bullet] goes through, in order: a * only for a
list set in from the margin, where it
cannot be a headline.")

(define-command org-cycle-list-bullet ()
  "Give the list point is in its next kind of bullet (\\`C-c -'): - to + to *
to 1. to 1) and round, every item
of the list at once, numbered items counted from 1; * is passed over for a
list at the margin. Checkboxes and text are kept."
  (with-org-structure (buffer tree text position)
    (let* ((item (org-current-item tree text position))
           (list (and item (org-node-parent item))))
      (if (null list)
          (message "Not in a list")
          (let* ((items (remove 'item (org-node-children list) :key #'org-node-type :test-not #'eq))
                 (first-line (org-line-at text (org-node-begin (first items))))
                 (indent (- (length first-line) (length (string-left-trim " 	" first-line))))
                 (current (string-trim " " (org-property (first items) :bullet)))
                 (kind (cond ((cl-ppcre:scan "^[0-9]+\\.$" current) "1.")
                             ((cl-ppcre:scan "^[0-9]+\\)$" current) "1)")
                             (t current)))
                 (choices (if (zerop indent) (remove "*" *org-list-bullet-cycle* :test #'string=) *org-list-bullet-cycle*))
                 (next (or (second (member kind choices :test #'string=)) (first choices))))
            (org-apply-edits
             buffer
             (loop for each in items
                   for n from 1
                   for begin = (org-node-begin each)
                   for line = (org-line-at text begin)
                   for at = (+ begin (- (length line) (length (string-left-trim " 	" line))))
                   for old = (string-trim " " (org-property each :bullet))
                   collect (list at (+ at (length old))
                                 (if (digit-char-p (char next 0)) (format nil "~d~a" n (subseq next 1)) next)))
             :point position)
            (message "Bullets: ~a" next))))))

(defun org-update-cookie (buffer position cookie)
  "Bring COOKIE, of the Org text of BUFFER, up to date with what the item or
headline it is on counts (`org-cookie-counts'), point left at POSITION."
  (let ((owner (loop for node = (org-node-parent cookie) then (org-node-parent node)
                     while node
                     when (member (org-node-type node) '(item headline)) return node)))
    (when owner
      (multiple-value-bind (total done) (org-cookie-counts owner)
        (let ((value (org-property cookie :value)))
          (org-apply-edits buffer (list (list (org-node-begin cookie) (+ (org-node-begin cookie) (length value))
                                              (org-cookie-text value done total)))
                           :point position))))))

(defun org-cookie-at (tree position)
  "The statistics cookie of TREE holding the character POSITION, or NIL."
  (org-node-at tree position
               (lambda (node) (and (eq (org-node-type node) 'statistics-cookie) (org-before-blank-p node position)))
               :titles t))

(define-command org-ctrl-c-ctrl-c ()
  "Do what Org's C-c C-c does where point is: on a dynamic block's #+BEGIN:
line, make the block again (\\[org-dblock-update]); on an item with a checkbox, tick
it or clear it, the counts above brought up to date; on a statistics cookie,
bring it up to date; in a table, align it; on its #+TBLFM: line, work its formulas out; on a CLOCK: line, write its duration again."
  (with-org-structure (buffer tree text position)
    (let ((cookie (org-cookie-at tree position))
          (item (org-current-item tree text position)))
      (cond ((let ((block (dblock-at tree text position)))
               ;; on a dynamic block's #+BEGIN: line: the block made again
               (when (and block (<= (first block) position (string-line-end text (first block))))
                 (check-writable buffer)
                 (dblock-update buffer tree text block)
                 t)))
            ((org-recalculate-from-tblfm buffer tree text position))
            ((org-update-clock-line buffer text position))
            ((org-table-at tree position) (org-table-align))
            (cookie (org-update-cookie buffer position cookie))
            ((and item (org-property item :checkbox)) (org-toggle-checkbox buffer tree text position item))
            (item (message "This item has no checkbox: C-c C-c ticks one ([ ])"))
            (t (message "Nothing for C-c C-c to do here"))))))

;;; What C-c's key hints show first

(defun org-src-block-at (tree position)
  "The source block of TREE holding the character POSITION, from its
#+begin_src line to its #+end_src line, or NIL."
  (org-node-at tree position (lambda (node) (and (eq (org-node-type node) 'src-block) (org-before-blank-p node position)))))

(defun org-context-keys (editor prefix)
  "For \\`C-c' in an Org buffer: what the cursor is on, as
*KEY-HINT-FOCUS-FUNCTIONS* has it, the keys that act on it first: a link,
a table, a source block, a list item, a heading; nothing while the file's
parse is behind its text."
  (declare (ignore editor))
  (let ((buffer (current-buffer)))
    (when (and (equalp prefix (tuatara.frontend:parse-key-sequence "C-c"))
               (typep (buffer-major-mode buffer) 'org-mode))
      ;; hints from the parse when it is of the text as it is now, never waited for
      (multiple-value-bind (tree text) (org-buffer-tree buffer)
        (when (and tree (org-parse-current-p buffer))
          (let* ((*org-current-text* text)
                 (position (org-point-char buffer))
                 (line-start (string-line-start text position)))
            (cond ((org-link-at tree position) '("link at point" "C-o" "C-l"))
                  ((org-table-at tree position) '("table at point" "C-c" "*" "=" "-"))
                  ((org-src-block-at tree position) '("source block at point" "C-v" "C-c"))
                  ((org-current-item tree text position) '("list item at point" "C-c" "-"))
                  ((and (< line-start (length text)) (char= (char text line-start) #\*)
                        (cl-ppcre:scan "^\\*+ " text :start line-start))
                   '("heading at point" "C-t" "C-q" "C-s" "C-d" "C-w" "C-z" "C-x")))))))))

(defvar *org-complete-checkboxes* t
  "True for [ typed right after a list item's bullet and its space (- [) to
make it a checkbox at once, - [ ] with point after the space, ready for the
item's text; NIL to type [ as anywhere else.")

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
                 (cl-ppcre:scan "^(?:[ \\t]*(?:[-+]|[0-9]+[.)])|[ \\t]+\\*) \\[$" before))
        (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit (1- p) end (tuatara.text:string-octets "[ ] ")))
                                    :group t)
        (goto (+ p 3))))))

(defun install-org-items ()
  "Bind C-c C-c in Org buffers, have C-c's key hints show first the keys for
what point is on (ORG-CONTEXT-KEYS), have typing - [ make a checkbox and RET
in NORMAL state on its line cycle it."
  (define-key (mode-keymap 'org-mode) "C-c C-c" 'org-ctrl-c-ctrl-c)
  (pushnew 'org-context-keys *key-hint-focus-functions*)
  ;; after closing pairs, whose ] it takes in
  (setf *post-self-insert-functions*
        (append (remove 'org-checkbox-typed *post-self-insert-functions*) (list 'org-checkbox-typed)))
  ;; after following a link point is on
  (setf *enter-dwim-functions*
        (append (remove 'org-checkbox-return *enter-dwim-functions*) (list 'org-checkbox-return))))
