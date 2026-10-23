;;;; lisp/org/edit.lisp --- Org's structure edited: subtrees moved, promoted and demoted, TODO states, new headlines and items.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands that edit an Org document by its structure, as Org's do:
;;;; a subtree moved over its sibling (org-move-subtree-up and -down),
;;;; a headline promoted or demoted alone or with its subtree, the TODO
;;;; keyword cycled through the document's keywords (a task done noted with
;;;; CLOSED: and when, as org-log-done 'time does), a new headline after the
;;;; subtree (M-RET, as Emacs Org's org-insert-heading-respect-content) or a new item in a list.
;;;; Each is one step to undo.

(in-package #:tuatara.defaults)

;;; Where point is in the structure


(defun org-point-char (buffer)
  "BUFFER's point as a character of its parse."
  (org-byte->char buffer (tuatara.buffer:buffer-point buffer)))

(defun org-current-headline (tree position)
  "The innermost headline of TREE whose subtree holds the character POSITION
(the end of the text the last one's), or NIL: down the tree, never through
every headline."
  (let ((found nil))
    (labels ((holds-p (node)
               (and (<= (org-node-begin node) position)
                    (or (< position (org-node-end node))
                        (and (= position (org-node-end node)) (= position (length *org-current-text*))))))
             (walk (node)
               (when (eq (org-node-type node) 'headline) (setf found node))
               (let ((child (find-if (lambda (c) (and (eq (org-node-type c) 'headline) (holds-p c))) (org-node-children node))))
                 (when child (walk child)))))
      (walk tree))
    found))

(defun org-siblings (headline tree)
  "The headlines beside HEADLINE, under the same parent (TREE's top for the first level), in order."
  (org-child-headlines (or (org-node-parent headline) tree)))

(defun org-replace-chars (buffer from to text &key point)
  "Put TEXT in place of BUFFER's characters FROM below TO (of its parse), one
step to undo, and point at the character POINT (of the text after) when given.
In a buffer whose lines end with a carriage return and a newline, each
newline of TEXT goes in as both, so that its line ends stay one kind."
  (let* ((returns (org-returns-of buffer))
         (start (org-char->byte buffer from returns))
         (end (org-char->byte buffer to returns)))
    (tuatara.buffer:buffer-replace buffer start end (text-for-buffer text buffer))
    (when point
      (setf (tuatara.buffer:buffer-point buffer) (org-char->byte buffer point)))))

(defmacro with-org-structure ((buffer tree text position) &body body)
  "Run BODY with BUFFER the current Org buffer, TREE and TEXT its parse as it
is now and POSITION its point as a character of it."
  `(let ((,buffer (current-buffer)))
     (multiple-value-bind (,tree ,text) (org-tree-now ,buffer)
       (let ((*org-current-text* (or ,text ""))
             (,position (org-point-char ,buffer)))
         (declare (ignorable ,position))
         (if (null ,tree)
             (message "No Org structure here")
             (progn ,@body))))))

(defun org-ends-with-newline-p (string)
  "True when STRING ends with a newline."
  (and (plusp (length string)) (char= (char string (1- (length string))) #\Newline)))

(defun org-ensure-newline (string)
  "STRING ending with a newline."
  (if (org-ends-with-newline-p string)
      string
      (concatenate 'string string (string #\Newline))))

(defun org-text-lines (text)
  "The lines of TEXT, the newline it ends with not making one more; none for an empty TEXT."
  (and (plusp (length text))
       (uiop:split-string (if (org-ends-with-newline-p text) (subseq text 0 (1- (length text))) text)
                          :separator '(#\Newline))))

(defun org-entry-from-lines (headline lines final-newline)
  "The entry made of HEADLINE, its headline's line, and LINES under it, ending with a newline when FINAL-NEWLINE is true."
  (format nil "~a~{~%~a~}~:[~;~%~]" headline lines final-newline))

;;; Moving subtrees

(defun org-move-subtree (direction)
  "Move the subtree point is in over its sibling in DIRECTION, :up or :down,
point going with it, each kept folded when it was."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not in a subtree")
          (let* ((siblings (org-siblings headline tree))
                 (index (position headline siblings))
                 (other (and index (let ((at (if (eq direction :down) (1+ index) (1- index))))
                                     (and (>= at 0) (nth at siblings))))))
            (if (or (null other) (and (eq direction :up) (zerop index)))
                (message "Cannot move past superior level or buffer limit")
                (let* ((first (if (eq direction :down) headline other))
                       (second (if (eq direction :down) other headline))
                       (from (org-node-begin first))
                       (to (org-node-end second))
                       (first-text (subseq text from (org-node-end first)))
                       (second-text (subseq text (org-node-begin second) to))
                       (ended (and (plusp to) (char= (char text (1- to)) #\Newline)))
                       (moved (concatenate 'string (org-ensure-newline second-text) (org-ensure-newline first-text)))
                       (moved (if ended moved (subseq moved 0 (1- (length moved)))))
                       ;; folded before: folded after, where each went
                       (first-folded (let ((r (org-fold-range first text))) (and r (org-folded-p buffer r))))
                       (second-folded (let ((r (org-fold-range second text))) (and r (org-folded-p buffer r))))
                       (offset (- position (org-node-begin headline)))
                       ;; after the move, SECOND stands at FROM and FIRST after it
                       (second-at from)
                       (first-at (+ from (length (org-ensure-newline second-text))))
                       (new-begin (if (eq direction :down) first-at second-at)))
                  (org-replace-chars buffer from to moved :point (+ new-begin offset))
                  (when (or first-folded second-folded)
                    (multiple-value-bind (tree text) (org-tree-now buffer)
                      (let ((*org-current-text* text))
                        (flet ((refold (begin)
                                 (let ((h (find begin (org-headlines tree) :key #'org-node-begin)))
                                   (when h
                                     (let ((r (org-fold-range h text)))
                                       (when r (org-fold-chars buffer (car r) (cdr r))))))))
                          (when first-folded (refold first-at))
                          (when second-folded (refold second-at)))))))))))))

(define-command org-move-subtree-down ()
  "Move the subtree point is in down, past its next sibling."
  (org-move-subtree :down))

(define-command org-move-subtree-up ()
  "Move the subtree point is in up, before its sibling above."
  (org-move-subtree :up))

;;; Promoting and demoting

(defun org-change-levels (buffer headlines text by)
  "Change the levels of HEADLINES of BUFFER (whose parse's text is TEXT) by BY
stars, one step to undo, point staying on its character."
  (let* ((position (org-point-char buffer))
         (from (reduce #'min headlines :key #'org-node-begin))
         (to (reduce #'max headlines :key (lambda (h) (+ (org-node-begin h) (org-property h :level)))))
         (shift 0)
         (pieces '())
         (at from))
    (dolist (headline (sort (copy-list headlines) #'< :key #'org-node-begin))
      (let ((begin (org-node-begin headline)) (level (org-property headline :level)))
        (push (subseq text at begin) pieces)
        (push (make-string (+ level by) :initial-element #\*) pieces)
        ;; stars wholly before point move it; point among them stays
        (when (<= (+ begin level) position) (incf shift by))
        (setf at (+ begin level))))
    (push (subseq text at to) pieces)
    (org-replace-chars buffer from to (apply #'concatenate 'string (nreverse pieces))
                       :point (max from (+ position shift)))))

(defun org-promote (subtree by)
  "Change the level of the headline point is in by BY (-1 promotes, 1
demotes), with SUBTREE all the headlines under it too."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (cond ((null headline) (message "Not in a subtree"))
            ((and (minusp by) (= 1 (org-property headline :level))) (message "Cannot promote to level 0"))
            (t (org-change-levels buffer (if subtree (org-headlines headline) (list headline)) text by))))))

(define-command org-do-promote ()
  "Promote the headline point is in a level, its subtree's other headlines as
they are."
  (org-promote nil -1))

(define-command org-do-demote ()
  "Demote the headline point is in a level, its subtree's other headlines as
they are."
  (org-promote nil 1))

(define-command org-promote-subtree ()
  "Promote the headline point is in a level, and every headline under it."
  (org-promote t -1))

(define-command org-demote-subtree ()
  "Demote the headline point is in a level, and every headline under it."
  (org-promote t 1))

;;; TODO states

(defun org-keyword-sequence (text)
  "The TODO keywords of TEXT in the order they cycle: those not done, then those done."
  (let ((keywords (org-todo-keywords-of text)))
    (append (car keywords) (cdr keywords))))

(defun org-state-note (new old now)
  "The note Org logs of a change of state from OLD to NEW at the universal time NOW, as
  - State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]."
  (format nil "- State ~12a from ~12a ~a" (format nil "\"~a\"" new) (format nil "\"~a\"" (or old "")) (org-timestamp-text now)))

(defun org-entry-lines-with (lines property value)
  "LINES, those of an entry after its headline, with PROPERTY set to VALUE in
its property drawer, made after its planning line when it has none."
  (let* ((planning (and lines (org-planning-line-p (first lines))))
         (head (if planning (list (first lines)) '()))
         (rest (if planning (rest lines) lines))
         (drawer '()))
    (if (and rest (string-equal (string-trim " 	" (first rest)) ":PROPERTIES:"))
        (let ((end (position-if (lambda (l) (string-equal (string-trim " 	" l) ":END:")) rest)))
          (setf drawer (subseq rest 0 (if end (1+ end) (length rest)))
                rest (if end (subseq rest (1+ end)) '())))
        (setf drawer (list ":PROPERTIES:" ":END:")))
    (let* ((key (format nil ":~a:" property))
           (line (format nil "~10a ~a" key value))
           (at (position-if (lambda (l)
                              (let ((from (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) l) (length l))))
                                (and (<= (+ from (length key)) (length l))
                                     (string-equal key l :start2 from :end2 (+ from (length key))))))
                            drawer)))
      (if at
          (setf (nth at drawer) line)
          (setf drawer (append (butlast drawer) (list line) (last drawer)))))
    (append head drawer rest)))

(defun org-line-after (text position)
  "Where the line after the one ending at or after POSITION in TEXT begins."
  (let ((newline (position #\Newline text :start (max 0 (min position (length text))))))
    (if newline (1+ newline) (length text))))

(defun org-log-place (owner text)
  "Where what OWNER logs goes in TEXT, OWNER a headline or a list item with a
checkbox, as two values: where a :LOGBOOK: drawer is made for it, or a line
goes with none (the line after a headline's own line, its planning line and
its properties; after an item's first line); and where the line after the
first line of its :LOGBOOK: drawer begins, the first drawer of that name in its
own text (a sub-headline's or a list item's left to them), or NIL when it has none."
  (let* ((*org-text* text)
         (item-p (eq (org-node-type owner) 'item))
         (children (if item-p
                       (org-node-children owner)
                       (let ((section (find 'section (org-node-children owner) :key #'org-node-type)))
                         (and section (org-node-children section)))))
         (place (org-line-after text (org-node-begin owner)))
         (drawer (find-if (lambda (child) (and (eq (org-node-type child) 'drawer)
                                               (string-equal (org-property child :drawer-name) "LOGBOOK")))
                          children)))
    (unless item-p
      (loop for child in children
            while (member (org-node-type child) '(planning property-drawer))
            do (setf place (org-line-after text (max (org-node-begin child) (1- (org-content-end child)))))))
    (values place (and drawer (org-line-after text (org-node-begin drawer))))))

(defun org-log-edit (owner text lines &key (drawer t))
  "The edit, (FROM TO NEW) of TEXT's characters, logging LINES for OWNER, a
headline or a list item with a checkbox, newest first: at the top of its
:LOGBOOK: drawer, one made where ORG-LOG-PLACE says when it has none; with
DRAWER NIL, there with no drawer. An item's lines are set in under it, as far
as the text after its bullet."
  (multiple-value-bind (place logbook) (org-log-place owner text)
    (let* ((indent (if (eq (org-node-type owner) 'item) (org-item-indent owner text) ""))
           (body (format nil "~{~a~a~%~}" (loop for line in lines collect indent collect line))))
      (if (and drawer logbook)
          (list logbook logbook body)
          (let ((new (if drawer (format nil "~a:LOGBOOK:~%~a~a:END:~%" indent body indent) body)))
            (list place place
                  ;; after a last line with no newline
                  (if (and (= place (length text)) (plusp place) (char/= (char text (1- place)) #\Newline))
                      (concatenate 'string (string #\Newline) new)
                      new)))))))

(defun org-entry-with-log (headline text lines drawer)
  "The text of HEADLINE's entry in TEXT (from its line to its first
sub-headline), with LINES logged as ORG-LOG-EDIT logs them (DRAWER as it
takes it); none when LINES is NIL."
  (let ((begin (org-node-begin headline))
        (end (org-entry-end headline text)))
    (if (null lines)
        (subseq text begin end)
        (destructuring-bind (from to new) (org-log-edit headline text lines :drawer drawer)
          (concatenate 'string (subseq text begin from) new (subseq text to end))))))

(defun org-entry-end (headline text)
  "Where the entry of HEADLINE ends in TEXT: at its first sub-headline, else at its end."
  (let ((child (find 'headline (org-node-children headline) :key #'org-node-type)))
    (if child (org-node-begin child) (min (length text) (org-node-end headline)))))

(defun org-keyword-place (text begin level line-end)
  "Where in TEXT the TODO keyword of the headline at BEGIN, LEVEL stars deep,
is or would be put: after its stars and the spaces and tabs after them, at
LINE-END, its line's end, at the latest."
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text :start (min line-end (+ begin level)) :end line-end)
      line-end))

(defun org-repeat-entry (buffer headline text position old done now)
  "Repeat the task of HEADLINE (in BUFFER, TEXT its text), with timestamps
that repeat, as it goes from OLD to the done keyword DONE at the universal
time NOW: those timestamps moved to their next time, its keyword the first of
its sequence again, and with *ORG-LOG-REPEAT* LAST_REPEAT set and the change
of state logged; in one step to undo. A task by rule (ORG-RULE-TIMESTAMP-IN)
keeps its rule, LAST_REPEAT, set whatever *ORG-LOG-REPEAT* says, saying the
day it is done for (ORG-RULE-DONE-TEXT)."
  (let* ((begin (org-node-begin headline))
         (line-end (string-line-end text begin))
         (end (org-entry-end headline text))
         (level (org-property headline :level))
         (keyword-at (org-keyword-place text begin level line-end))
         (old-end (if old (min line-end (+ keyword-at (length old) 1)) keyword-at))
         (first-todo (first (car (org-todo-keywords-of text))))
         (entry (org-entry-with-log headline text (and *org-log-repeat* (list (org-state-note done old now))) *org-log-into-drawer*))
         (entry-line-end (or (position #\Newline entry) (length entry)))
         (headline-text (concatenate 'string (subseq entry 0 (- keyword-at begin)) first-todo " " (subseq entry (- old-end begin) entry-line-end)))
         (body (if (< entry-line-end (length entry)) (subseq entry (1+ entry-line-end)) ""))
         (moved (org-repeat-timestamps body now))
         (rule (org-rule-timestamp-in body 0 (length body)))
         (done-for (and rule (org-rule-done-text rule (org-headline-property headline "LAST_REPEAT") now)))
         (lines (org-text-lines moved))
         (lines (cond (done-for (org-entry-lines-with lines "LAST_REPEAT" done-for))
                      (*org-log-repeat* (org-entry-lines-with lines "LAST_REPEAT" (org-timestamp-text now)))
                      (t lines)))
         (new (org-entry-from-lines headline-text lines (or (org-ends-with-newline-p moved)
                                                            (and (null lines) (< entry-line-end (length entry)))))))
    (org-replace-chars buffer begin end new
                       :point (if (<= position line-end) (min position (+ begin (length headline-text))) begin))
    (if done-for
        (let ((next (org-rule-next rule (1+ (org-ts-day-number (org-read-ts done-for))))))
          (message "Done for ~a~@[; next ~a~]: ~a" (subseq done-for 1 (1- (length done-for)))
                   (and next (format-day "%F %a" next)) first-todo))
        (message "Entry repeats: ~a" first-todo))))

(defun org-set-todo (step)
  "Set the TODO keyword of the headline point is in to the one STEP (1 or -1)
from it in the document's sequence, none past either end; reaching a done
keyword from one not done notes when (CLOSED:), leaving it takes that away."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not in a subtree")
          (let* ((sequence (org-keyword-sequence text))
                 (done-keywords (cdr (org-todo-keywords-of text)))
                 (old (org-property headline :todo-keyword))
                 (index (and old (position old sequence :test #'string=)))
                 (new (cond ((null old) (if (plusp step) (first sequence) (car (last sequence))))
                            (t (let ((next (+ index step))) (and (< -1 next (length sequence)) (nth next sequence))))))
                 (begin (org-node-begin headline))
                 (level (org-property headline :level))
                 (line-end (string-line-end text begin))
                 (keyword-at (org-keyword-place text begin level line-end))
                 (old-end (if old (min line-end (+ keyword-at (length old) 1)) keyword-at))
                 (inserted (if new (format nil "~a " new) ""))
                 (was-done (and old (member old done-keywords :test #'string=)))
                 (now-done (and new (member new done-keywords :test #'string=)))
                 (position (if (>= position old-end) (+ position (- (length inserted) (- old-end keyword-at))) position))
                 (now (org-now)))
            (when (and now-done (not was-done)
                       (or (org-repeating-timestamp-in-p text line-end (org-entry-end headline text))
                           (org-rule-timestamp-in text line-end (org-entry-end headline text))))
              (return-from org-set-todo (org-repeat-entry buffer headline text (org-point-char buffer) old new now)))
            ;; the planning line under the headline: CLOSED: added or taken away
            (let* ((next-line (min (length text) (1+ line-end)))
                   (planning (and (< next-line (length text))
                                  (org-planning-line-p text :start next-line :end (string-line-end text next-line))))
                   (planning-end (and planning (string-line-end text next-line)))
                   (planning-text (and planning (subseq text next-line planning-end)))
                   (new-planning
                     (cond ((and now-done (not was-done) (not *org-log-done*)) nil)
                           ((and now-done (not was-done))
                            (if planning
                                (cl-ppcre:regex-replace "^([ \\t]*)" planning-text
                                                        (format nil "\\1CLOSED: ~a " (org-timestamp-text now)))
                                :new))
                           ((and was-done (not now-done) planning)
                            (string-right-trim " " (cl-ppcre:regex-replace "CLOSED:[ \\t]*\\[[^]]*\\][ \\t]*" planning-text "")))
                           (t nil))))
              (let* ((headline-text (concatenate 'string (subseq text begin keyword-at) inserted (subseq text old-end line-end)))
                     (region-end (cond ((null new-planning) line-end)
                                       ((eq new-planning :new) line-end)
                                       (t planning-end)))
                     (replacement
                       (cond ((null new-planning) headline-text)
                             ((eq new-planning :new)
                              (format nil "~a~%CLOSED: ~a" headline-text (org-timestamp-text now)))
                             ((string= (string-trim " 	" new-planning) "")
                              ;; the planning line emptied: it goes, with its newline
                              (setf region-end (min (length text) (1+ planning-end)))
                              (format nil "~a~%" headline-text))
                             (t (format nil "~a~%~a" headline-text new-planning)))))
                (org-replace-chars buffer begin region-end replacement :point position)
                (message "~:[No keyword~;~:*~a~]" new))))))))

(define-command org-todo ()
  "Change the TODO keyword of the headline point is in to the next of the
document's (#+TODO:, else TODO and DONE), none after the last; a task done is
noted with CLOSED: and when. On a list item's
line with a checkbox, tick it, or clear it, as a task done or not, logged
\(*org-log-checkboxes*)."
  (unless (call-on-checkbox-item (lambda (buffer tree text position item)
                                   (org-toggle-checkbox buffer tree text position item)))
    (org-set-todo 1)
    (org-todo-changed (current-buffer))))

(define-command org-shiftright ()
  "On a headline, its next TODO keyword, as \\[org-todo] does."
  (org-set-todo 1)
  (org-todo-changed (current-buffer)))

(define-command org-shiftleft ()
  "On a headline, its TODO keyword before, none before the first."
  (org-set-todo -1)
  (org-todo-changed (current-buffer)))

;;; Tags and notes

(defvar *org-tags-column* 0
  "Where \\[org-set-tags-command] and \\[org-align-tags] put a headline's
tags: 0, one space after the headline's text; negative, so that they end at
that column (-77 to end at the 77th); positive, so that they start there.")

(defun org-tags-from-text (text)
  "The tags TEXT names, :work:urgent: or work urgent or work, urgent: each
once, in order."
  (remove-duplicates (remove "" (cl-ppcre:split "[:,\\s]+" text) :test #'string=) :test #'string= :from-end t))

(defun read-org-tags (buffer &optional old)
  "Ask for tags, OLD, a list, written to begin with: the tags used in BUFFER
and the agenda files (ORG-KNOWN-TAGS) offered for the one being typed after
the last colon, those already written left out; work urgent and
:work:urgent: both taken. Returns the tags, a list."
  (let ((known (org-known-tags buffer)))
    (org-tags-from-text
     (completing-read "Tags: "
                      (lambda (input)
                        (let* ((cut (position #\: input :from-end t))
                               (before (if cut (subseq input 0 (1+ cut)) ""))
                               (written (org-tags-from-text before)))
                          (append
                           ;; nothing begun after the colon: what is written, as it is
                           (when (zerop (length (string-trim " " (subseq input (length before)))))
                             (list (make-candidate "" :value input)))
                           (loop for tag in known
                                 unless (member tag written :test #'string=)
                                   collect (make-candidate tag :value (concatenate 'string before tag ":"))))))
                      :separator #\: :initial (if old (format nil ":~{~a~^:~}:" old) "")))))

(defun org-headline-with-tags (line tags)
  "The headline LINE with its tags TAGS (a list), put at *ORG-TAGS-COLUMN*,
or none."
  (let* ((stripped (string-right-trim " 	" (cl-ppcre:regex-replace "[ \\t]+:[\\w@#%:]+:[ \\t]*$" line "")))
         ;; a headline of stars alone keeps the space that makes it one
         (bare (if (every (lambda (c) (char= c #\*)) stripped) (concatenate 'string stripped " ") stripped)))
    (if (null tags)
        bare
        (let* ((text (format nil ":~{~a~^:~}:" tags))
               (used (text-columns bare))
               (column (if (minusp *org-tags-column*) (- (abs *org-tags-column*) (text-columns text)) *org-tags-column*)))
          (concatenate 'string bare (make-string (max 1 (- column used)) :initial-element #\Space) text)))))

(define-command org-set-tags-command ()
  "Set the tags of the headline point is under
(\\`C-c C-q'): asked for with its tags offered, work urgent or :work:urgent:,
nothing for none; they go at the end of its line, where *ORG-TAGS-COLUMN*
says, by default one space after its text. With \\[universal-argument], the
tags of every headline of the buffer put there instead (\\[org-align-all-tags])."
  (when (prefix-argument)
    (return-from org-set-tags-command (org-align-all-tags)))
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not under a headline")
          (let* ((begin (org-node-begin headline))
                 (line-end (string-line-end text begin))
                 (line (subseq text begin line-end))
                 (old (org-property headline :tags))
                 (tags (read-org-tags buffer old))
                 (new (org-headline-with-tags line tags)))
            (if (string= new line)
                (message "Tags as they were")
                (progn (org-replace-chars buffer begin line-end new
                                          :point (if (> position line-end)
                                                     (+ position (- (length new) (- line-end begin)))
                                                     (min position (+ begin (length new)))))
                       (message "~:[No tags~;~:*Tags: ~{~a~^ ~}~]" tags))))))))

(defun org-tags-alignment-edits (headlines text)
  "The edits, (FROM TO NEW) of TEXT's characters, putting the tags of each of
HEADLINES that has any where *ORG-TAGS-COLUMN* says; none for those already there."
  (loop for headline in headlines
        for tags = (org-property headline :tags)
        for begin = (org-node-begin headline)
        for line-end = (string-line-end text begin)
        for line = (subseq text begin line-end)
        for new = (and tags (org-headline-with-tags line tags))
        when (and new (string/= new line))
          collect (list begin line-end new)))

(defun org-align-tags-in (buffer headlines text position)
  "Put the tags of HEADLINES of BUFFER (TEXT its parse, point at the character
POSITION) where *ORG-TAGS-COLUMN* says, in one step to undo; point kept by the
text it is in, or by the tags it is in; how many lines changed."
  (let* ((edits (org-tags-alignment-edits headlines text))
         ;; on a line that changes, as far from its end as it was, past the edits before the line
         (inside (find-if (lambda (edit) (<= (first edit) position (second edit))) edits))
         (point (if inside
                    (destructuring-bind (begin line-end new) inside
                      (let ((title-end (length (string-right-trim " 	" (cl-ppcre:regex-replace "[ \\t]+:[\\w@#%:]+:[ \\t]*$" new ""))))
                            (before (loop for (start end other) in edits
                                          when (<= end begin) sum (- (length other) (- end start)))))
                        (+ before
                           (if (<= position (+ begin title-end))
                               position
                               (max (+ begin title-end) (- (+ begin (length new)) (- line-end position)))))))
                    position)))
    (when edits
      (check-writable buffer)
      (org-apply-edits buffer edits :point point :final (and inside t)))
    (length edits)))

(define-command org-align-tags ()
  "Put the tags of the headline point is under where *ORG-TAGS-COLUMN* says:
by default one space after its text. With
\\[universal-argument], the tags of every headline of the buffer
\(\\[org-align-all-tags])."
  (if (prefix-argument)
      (org-align-all-tags)
      (with-org-structure (buffer tree text position)
        (let ((headline (org-current-headline tree position)))
          (cond ((null headline) (message "Not under a headline"))
                ((null (org-property headline :tags)) (message "No tags on this headline"))
                ((zerop (org-align-tags-in buffer (list headline) text position)) (message "Tags in place already"))
                (t (message "Tags put in place")))))))

(define-command org-align-all-tags ()
  "Put the tags of every headline of the buffer where *ORG-TAGS-COLUMN* says:
by default one space after each one's text, in
one step to undo."
  (with-org-structure (buffer tree text position)
    (let ((count (org-align-tags-in buffer (org-headlines tree) text position)))
      (message (if (zerop count) "Tags in place already" (format nil "Tags put in place on ~d headline~:p" count))))))

(define-command org-add-note ()
  "Add a note to the entry point is under (\\`C-c C-z'):
the note is asked for, and logged where Org logs notes, at the top of its
:LOGBOOK: drawer with *ORG-LOG-INTO-DRAWER*, else after its planning line and
properties: - Note taken on [2026-10-01 Thu 18:00] \\\\ and the note under it.
On a list item's line with a checkbox, on that item, in a :LOGBOOK: drawer
under it. An empty note is not kept."
  (with-org-structure (buffer tree text position)
    (let* ((item (org-checkbox-item-at tree text position))
           (owner (or item (org-current-headline tree position))))
      (if (null owner)
          (message "Not under a headline")
          (let ((note (string-trim " 	" (read-string "Note: "))))
            (if (zerop (length note))
                (message "No note taken: it was empty")
                (progn
                  (org-apply-edits buffer (list (org-log-edit owner text (list (format nil "- Note taken on ~a \\\\" (org-timestamp-text (org-now)))
                                                                               (format nil "  ~a" note))
                                                              :drawer (or item *org-log-into-drawer*)))
                                   :point position)
                  (message "Note taken"))))))))

;;; New headlines and items

(defun org-current-item (tree text position)
  "The innermost list item of TREE holding the character POSITION of TEXT, or
NIL; at the very end of a text not ending with a newline, the item of its
last line."
  (org-node-at tree (if (and (= position (length text)) (plusp position) (char/= (char text (1- position)) #\Newline))
                        (1- position)
                        position)
               (lambda (node) (eq (org-node-type node) 'item)) :innermost t))

(defun org-insert-heading (todo)
  "Insert a new headline or item for what point is in: in a list, a new item
after the one point is in and its sub-items, of the same kind (its bullet, the
next number, a checkbox [ ] when it has one); else a headline of the level
point is under after its subtree, with
TODO a task."
  (with-org-structure (buffer tree text position)
    (let ((item (and (not todo) (org-current-item tree text position)))
          (headline (org-current-headline tree position)))
      (cond
        ((and item (not (and headline (= (org-node-begin headline) (string-line-start text position)))))
         (let* ((end (org-last-content-end text (org-node-begin item) (org-node-end item)))
                (bullet (org-property item :bullet))
                (indent (subseq text (org-node-begin item)
                                (+ (org-node-begin item) (- (length (org-line-at text (org-node-begin item)))
                                                            (length (string-left-trim " 	" (org-line-at text (org-node-begin item))))))))
                (next (cl-ppcre:regex-replace "^(\\d+)" (string-trim " " bullet)
                                              (lambda (match number) (declare (ignore match)) (princ-to-string (1+ (parse-integer number))))
                                              :simple-calls t))
                (new (format nil "~%~a~a ~:[~;[ ] ~]" indent next (org-property item :checkbox)))
                (after (rest (member item (org-node-children (org-node-parent item))))))
           (org-apply-edits buffer
                            (cons (list end end new)
                                  (when (digit-char-p (char next 0))
                                    (loop for each in after
                                          for n from (1+ (parse-integer next :junk-allowed t))
                                          when (eq (org-node-type each) 'item)
                                            append (org-renumber-edit text each n))))
                            :point end)))
        (headline
         (let* ((level (org-property headline :level))
                (content-end (org-last-content-end text (org-node-begin headline) (org-node-end headline)))
                (blank (< (1+ content-end) (org-node-end headline)))
                (new (format nil "~%~:[~;~%~]~a ~:[~;TODO ~]" blank (make-string level :initial-element #\*) todo)))
           (org-replace-chars buffer content-end content-end new :point (+ content-end (length new)))))
        (t
         (let* ((line-end (string-line-end text position))
                (new (format nil "~%* ~:[~;TODO ~]" todo)))
           (org-replace-chars buffer line-end line-end new :point (+ line-end (length new)))))))))

(defun org-renumber-edit (text item number)
  "The edit, a list of one (FROM TO NEW), giving the numbered ITEM of TEXT the
NUMBER, its . or ) kept; NIL for an item with no number."
  (let* ((line (org-line-at text (org-node-begin item)))
         (at (+ (org-node-begin item) (- (length line) (length (string-left-trim " 	" line)))))
         (digits (or (position-if-not #'digit-char-p text :start at) (length text))))
    (when (> digits at)
      (list (list at digits (princ-to-string number))))))

(define-command org-meta-return ()
  "A new item or headline after what point is in and all under it: in a list,
an item of the same kind after the one point is in and all under it (its
bullet, the next number, [ ] when it has a checkbox); past the list, in a
section's text or on a headline, a headline of the section's level after its
subtree."
  (org-insert-heading nil))

(define-command org-insert-todo-heading ()
  "A new TODO headline of this level after this subtree."
  (org-insert-heading t))

(defun install-org-editing ()
  "Bind Org's keys for TODO states and new headlines (M-h M-j M-k M-l, shared with tables, are bound by INSTALL-ORG-TABLES)."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-keys keymap '(("C-c C-t" org-todo) ("S-<right>" org-shiftright) ("S-<left>" org-shiftleft)
                          ("C-c C-q" org-set-tags-command) ("C-c C-z" org-add-note) ("C-c C-w" org-refile)
                          ("M-RET" org-meta-return) ("M-S-RET" org-insert-todo-heading)
                          ("C-c C-x C-u" org-dblock-update)
                          ("C-c C-x x" org-dynamic-block-insert-dblock)))))
