;;;; lisp/modes/list-marks.lisp --- Lists whose entries are marked: the marks, their count, deleting and opening.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; Marks

(define-major-mode marked-list-mode special-mode
  "A list whose entries can be marked, for its commands to act on them
together, as the buffer list (\\[buffer-manager]) and the bookmark manager
\(\\[bookmark-manager]) are. \\<marked-list-mode>\\[list-mark-entry] marks the
entry on the line (>). \\[list-mark-for-deletion] marks it for deletion (D).
\\[list-unmark-entry] takes its mark off. Each of these then moves down, a
count marking as many. \\[list-unmark-all] takes every mark off.
\\[list-toggle-marks] marks those not marked and unmarks those marked, as in
rimu. \\[list-delete-marked] deletes those marked, or the one on the line.
\\[list-execute] deletes those marked for deletion, after asking."
  :lighter "List" :interactive nil)

(defvar *list-mark-order* 0
  "How many marks have been given in the lists, the order of each mark kept
with it (LIST-MARK-TABLE).")

(defun list-mark-table (list)
  "The marks on the entries of LIST, a list made by RENDER-PIECES, NIL while
it has none: a table from what the list's mark key gives of an entry
\(ENTRY-MARK-KEY) to (MARK . ORDER), MARK :MARKED (>) or :DELETION (D), ORDER
saying which marks were given first."
  (tuatara.buffer:buffer-local list 'list-marks))

(defun list-marks (list)
  "The marks on the entries of LIST, each (KEY . MARK): KEY what the list's
mark key gives of the entry (ENTRY-MARK-KEY), MARK :MARKED (>) or :DELETION
\(D); the last marked first."
  (let ((table (list-mark-table list)) (marks '()))
    (when table
      (maphash (lambda (key value) (push (list* (cdr value) key (car value)) marks)) table))
    (mapcar #'cdr (sort marks #'> :key #'car))))

(defun entry-mark (list key)
  "The mark of the entry of LIST known by KEY: :MARKED, :DELETION or NIL."
  (let ((table (list-mark-table list)))
    (and table (car (gethash key table)))))

(defun list-marked (list mark)
  "The keys of the entries of LIST marked MARK (:MARKED or :DELETION), the
first marked first."
  (reverse (loop for (key . m) in (list-marks list) when (eq m mark) collect key)))

(defun set-entry-mark (list key mark)
  "Give the entry of LIST known by KEY the MARK :MARKED or :DELETION, in place
of the one it had, or with NIL none."
  (let ((table (or (list-mark-table list)
                   (setf (tuatara.buffer:buffer-local list 'list-marks) (make-hash-table :test 'equal)))))
    (note-marks-changed list)
    (if mark
        (setf (gethash key table) (cons mark (incf *list-mark-order*)))
        (remhash key table))))

(defun keep-list-marks (list keys)
  "Keep only the marks of LIST on entries still listed, KEYS being theirs."
  (let ((table (list-mark-table list)))
    (when (and table (plusp (hash-table-count table)))
      (note-marks-changed list)
      (let ((listed (make-hash-table :test 'equal)))
        (dolist (key keys) (setf (gethash key listed) t))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (unless (gethash key listed) (remhash key table)))
                 table)))))

(defun list-mark-sign (mark)
  "What a list shows for MARK in its mark column: > marked (:marked), D for
deletion (:deletion), a mark that is a character (as rimu keeps them, M a
master) that character, else a space."
  (cond ((eq mark :marked) ">")
        ((eq mark :deletion) "D")
        ((characterp mark) (string mark))
        (t " ")))

(defun list-mark-face (mark)
  "The face of an entry's name in a list for MARK: :list-marked for :marked
or #\\>, :list-deletion for :deletion or #\\D, :list-marked-other for any other
character (a mark of another kind, as rimu's M of a master); NIL for no mark."
  (case mark
    ((:marked #\>) :list-marked)
    ((:deletion #\D) :list-deletion)
    (t (and (characterp mark) :list-marked-other))))

(defun entry-mark-key (list entry)
  "What a mark on ENTRY of LIST is kept by: what the list's mark key gives of
it (RENDER-PIECES' MARK-KEY), else ENTRY itself; NIL for no entry, or one
that takes no mark (a section's heading)."
  (and entry (funcall (tuatara.buffer:buffer-local list 'list-mark-key #'identity) entry)))

(defun list-line-after (list position count)
  "The start of the line COUNT lines after the one of LIST holding POSITION
\(before it for a negative COUNT), as far as the text goes."
  (let ((line (+ (tuatara.buffer:buffer-line-number list position) count)))
    (tuatara.buffer:buffer-goto-line list (max 0 (min line (1- (tuatara.buffer:buffer-line-count list)))))))

(defgeneric mark-entries (mode list mark count)
  (:documentation "Give the entry on the current line of LIST, a buffer in MODE, MARK
\(:MARKED, :DELETION, or NIL for none) in place of the one it had, and as many
as COUNT says, each moving down a line, or with a negative COUNT up a line
first: what \\[list-mark-entry], \\[list-mark-for-deletion] and \\[list-unmark-entry] do in that list.")
  (:method ((mode marked-list-mode) list mark count)
    "A line showing no entry, or one that takes no mark, is passed over; the
rows of those marked are drawn again (REDRAW-TABLE-ROWS), or in a list that
is no table the whole list, once (REDRAW-LIST)."
    (let ((changed '()))
      (loop repeat (abs count)
            do (when (minusp count)
                 (setf (tuatara.buffer:buffer-point list) (list-line-after list (tuatara.buffer:buffer-point list) -1)))
               (let* ((entry (list-entry-at list)) (key (entry-mark-key list entry)))
                 (when key
                   (set-entry-mark list key mark)
                   (push (entry-member-or-self entry) changed)))
               (when (plusp count)
                 (setf (tuatara.buffer:buffer-point list) (list-line-after list (tuatara.buffer:buffer-point list) 1))))
      (redraw-table-rows list changed))))

(defgeneric unmark-entries (mode list)
  (:documentation "Take every mark off the entries of LIST, a buffer in MODE, as
\\[list-unmark-all] does there. Returns how many marks were taken off.")
  (:method ((mode marked-list-mode) list)
    "Every mark, > and D, the list drawn again."
    (let* ((table (list-mark-table list)) (count (if table (hash-table-count table) 0)))
      (unmark-list list)
      count)))

(defgeneric swap-entry-marks (mode list)
  (:documentation "Mark the entries LIST, a buffer in MODE, shows that are not marked and
take the mark off those marked, as \\[list-toggle-marks] does there; those marked
for deletion, or given a mark of another kind, stay so. Returns how many are
marked now.")
  (:method ((mode marked-list-mode) list)
    "The entries a filter or a folded section hides stay as they are, those
marked among them counted."
    (let ((seen (make-hash-table :test 'equal)))
      (map nil (lambda (entry)
                 (let ((key (entry-mark-key list entry)))
                   (when (and key (not (gethash key seen)))
                     (setf (gethash key seen) t)
                     (case (entry-mark list key)
                       (:marked (set-entry-mark list key nil))
                       ((nil) (set-entry-mark list key :marked))))))
           (tuatara.buffer:buffer-local list 'list-entries))
      (redraw-list list)
      (values (list-mark-counts mode list)))))

(defun unmark-list (list)
  "Take every mark off the entries of LIST and draw it again."
  (let ((table (list-mark-table list)))
    (when table (clrhash table)))
  (note-marks-changed list)
  (redraw-list list))

(defun list-acted-on (list)
  "What a command for the marked entries of LIST acts on: the keys of those
marked (>), the first marked first, or with none marked the key of the entry
on the current line; NIL on a line showing none, or one that takes no mark."
  (or (list-marked list :marked)
      (let ((key (entry-mark-key list (list-entry-at list)))) (and key (list key)))))

(define-command list-mark-entry ()
  "Mark the entry on the current line of the list (>), for the list's commands
that act on those marked, and move down; with a count, as many entries, up the
list for a negative one."
  (mark-entries (buffer-major-mode (current-buffer)) (current-buffer) :marked (prefix)))

(define-command list-mark-for-deletion ()
  "Mark the entry on the current line of the list for deletion (D), for the
list's command that deletes them, and move down; with a count, as many
entries, up the list for a negative one."
  (mark-entries (buffer-major-mode (current-buffer)) (current-buffer) :deletion (prefix)))

(define-command list-unmark-entry ()
  "Take the mark off the entry on the current line of the list, of any kind,
and move down; with a count, off as many entries, up the list for a negative
one."
  (mark-entries (buffer-major-mode (current-buffer)) (current-buffer) nil (prefix)))

(define-command list-toggle-marks ()
  "Mark the entries the list shows that are not marked and take the mark off
those marked; those marked for deletion (D), or given a mark of another kind,
stay so, as do the entries a filter or a folded section hides."
  (message "~d marked" (swap-entry-marks (buffer-major-mode (current-buffer)) (current-buffer))))

(define-command list-unmark-all ()
  "Take every mark off the entries of the list, of any kind; in rimu, those
of the files marked in other directories too."
  (message "~d mark~:p taken away" (unmark-entries (buffer-major-mode (current-buffer)) (current-buffer))))

(defvar *list-hidden-count* 0
  "How many of the entries a list command acts on the list's search filters
hide, while it runs (ACT-ON-LIST); its question says so (LIST-QUESTION).")

(defun list-hidden-count (list keys)
  "How many of the entries of LIST known by KEYS (ENTRY-MARK-KEY) its search
filters hide (KEYS-HIDDEN-BY-FILTERS): 0 when it is not narrowed."
  (length (keys-hidden-by-filters list keys)))

(defvar *list-folded-count* 0
  "How many of the entries a list command acts on the list's folded sections
hide, while it runs (ACT-ON-LIST); its question says so (LIST-QUESTION).")

(defun list-folded-count (list keys)
  "How many of the entries of LIST known by KEYS (ENTRY-MARK-KEY) its folded
sections hide: 0 when none is folded, or while search filters narrow it."
  (let ((all (tuatara.buffer:buffer-local list 'list-every-entry)))
    (if (and all (not (list-filters-on-p list)))
        (let ((shown (make-hash-table :test 'equal)) (folded (make-hash-table :test 'equal)))
          (map nil (lambda (entry) (setf (gethash (entry-mark-key list entry) shown) t))
               (tuatara.buffer:buffer-local list 'list-entries #()))
          (dolist (entry all) (setf (gethash (entry-mark-key list entry) folded) t))
          (count-if (lambda (key) (and (gethash key folded) (not (gethash key shown)))) keys))
        0)))

(defun act-on-list (list act noun &key deletion)
  "Call ACT with LIST and the keys of what a command for its marked entries acts
on: with DELETION, those marked for deletion (D); else those marked (>), or with
none marked the entry on the current line (LIST-ACTED-ON); how many of them the
list's search filters hide is *LIST-HIDDEN-COUNT* meanwhile, how many its
folded sections hide *LIST-FOLDED-COUNT*. With none, say so,
NOUN naming an entry (\"bookmark\"): no NOUNs marked for deletion, or no NOUN on
this line."
  (let* ((keys (if deletion (list-marked list :deletion) (list-acted-on list)))
         (*list-hidden-count* (list-hidden-count list keys))
         (*list-folded-count* (list-folded-count list keys)))
    (cond (keys (funcall act list keys))
          (deletion (message "No ~as marked for deletion" noun))
          (t (message "No ~a on this line" noun)))))

(defun plural-noun (noun)
  "The plural of NOUN, an English noun or words ending in one, made by the
common rule: entry, entries; buffer, buffers."
  (let ((length (length noun)))
    (if (and (> length 1) (char= (char noun (1- length)) #\y) (not (find (char noun (- length 2)) "aeiou")))
        (concatenate 'string (subseq noun 0 (1- length)) "ies")
        (concatenate 'string noun "s"))))

(defun list-question (verb noun keys &key (name-of #'identity) where after (the t) details)
  "What a list asks before VERB (\"Delete\") acts on its entries known by KEYS,
NOUN naming one, as two values: the question, short and without the question
mark, and its details, the lines shown beside it (CALL-WITH-QUESTION-DETAILS),
or NIL when there are none. The question is VERB the NOUN and its name (what
NAME-OF gives of the key) for one, VERB the N NOUNs for more (PLURAL-NOUN),
then AFTER when given (\"to:\"). With THE NIL, the name of one alone (VERB
NAME), and N NOUNs for more; with VERB NIL, the words for the entries alone.
The details say how
many of them the list's search filters hide (*LIST-HIDDEN-COUNT*) and how many
its folded sections hide (*LIST-FOLDED-COUNT*), then WHERE, words saying where
they are (\"in 2 directories\"), then DETAILS, lines of the caller's own."
  (let* ((count (length keys))
         (hidden (min *list-hidden-count* count))
         (folded (min *list-folded-count* count))
         (lines (remove nil (list (and (plusp hidden)
                                       (format nil "~:[~d of them ~:*~[~;is~:;are~]~;It is~*~] hidden by the filter."
                                               (= count 1) hidden))
                                  (and (plusp folded)
                                       (format nil "~:[~d of them ~:*~[~;is in a folded section~:;are in folded ~
                                                    sections~]~;It is in a folded section~*~]."
                                               (= count 1) folded))
                                  (and where (format nil "They are ~a." where))))))
    (values (format nil "~@[~a ~]~a~@[ ~a~]"
                    verb
                    (if (rest keys)
                        (format nil "~:[~;the ~]~d ~a" the count (plural-noun noun))
                        (format nil "~:[~*~;the ~a ~]~a" the noun (funcall name-of (first keys))))
                    after)
            (cond ((and lines details) (format nil "~{~a~%~}~%~a" lines details))
                  (lines (format nil "~{~a~^~%~}" lines))
                  (t details)))))

;; The two values of LIST-QUESTION go to the asker together, so that no caller drops the details.
(defmacro ask-list-question ((asker &key (end "?")) question-form &rest arguments)
  "Ask a question about the entries of a list: QUESTION-FORM, a call of
LIST-QUESTION or RIMU-QUESTION, gives the question and its details, and ASKER
(Y-OR-N-P*, YES-OR-NO-P*, READ-STRING, READ-NATIVE-PATH) is called with the
question followed by END (a string, \"?\" when not given; \" \" before a
prompt to type into), then ARGUMENTS, and the details as :DETAILS. Gives
what ASKER gives."
  (let ((question (gensym "QUESTION")) (details (gensym "DETAILS")))
    `(multiple-value-bind (,question ,details) ,question-form
       (,asker (format nil "~a~a" ,question ,end) ,@arguments :details ,details))))

(defun names-shown (items &key (most 20) (name-of #'identity) (separator :lines))
  "The names of ITEMS, a list, what NAME-OF gives of each, the first MOST of
them and how many more, or NIL for none. With SEPARATOR :LINES one name to a
line, the last line \"and 5 more\"; with :WORDS run together as words, \"a, b
and c\" or \"a, b, c and 2 more\". All are named when one more is all that
would be left out."
  (let* ((count (length items))
         (shown (if (> count (1+ most)) most count))
         (names (mapcar name-of (subseq items 0 shown)))
         (more (and (< shown count) (- count shown))))
    (cond ((null names) nil)
          ((eq separator :lines) (format nil "~{~a~^~%~}~@[~%and ~d more~]" names more))
          (more (format nil "~{~a~^, ~} and ~d more" names more))
          (t (format nil "~{~a~#[~; and ~:;, ~]~}" names)))))

(defun list-every-entry (list)
  "The entries of LIST in order, a sequence: those its lines show, with those
its folded sections or its search filters hide."
  (declare (notinline list-filtering-source list-source-entries))
  (let ((source (and (list-filters-on-p list) (list-filtering-source (list-filtering list)))))
    (cond (source (list-source-entries source))
          ((tuatara.buffer:buffer-local list 'list-every-entry))
          (t (tuatara.buffer:buffer-local list 'list-entries #())))))

(defun list-entries-of (list keys)
  "The entries of LIST whose mark keys (ENTRY-MARK-KEY) are among KEYS, in
order, those its folded sections or its search filters hide too
\(LIST-EVERY-ENTRY); in a list in sections the members of those lines
\(ENTRY-MEMBER)."
  (let ((wanted (make-hash-table :test 'equal)))
    (dolist (key keys) (setf (gethash key wanted) t))
    (map 'list #'entry-member-or-self
         (remove-if-not (lambda (entry) (gethash (entry-mark-key list entry) wanted)) (list-every-entry list)))))

;;; How many are marked, in the mode line

(defgeneric list-mark-counts (mode list)
  (:documentation "How many entries of LIST, a buffer in major mode MODE, are marked (>) and
how many are marked for deletion (D), as two values; both 0 for a buffer that
has no marks.")
  (:method (mode list) (declare (ignore mode list)) (values 0 0))
  (:method ((mode marked-list-mode) list)
    "Counted from the list's table of marks (LIST-MARK-TABLE)."
    (let ((table (list-mark-table list)) (marked 0) (deletion 0))
      (when table
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (if (eq (car value) :deletion) (incf deletion) (incf marked)))
                 table))
      (values marked deletion))))

(defun note-marks-changed (list)
  "Say that the marks of LIST have changed, so that the mode line counts them
again (LIST-MARKS-MODE-LINE) the next time it is drawn. Whatever puts or takes
marks calls this."
  (setf (tuatara.buffer:buffer-local list 'list-marks-text) nil))

(defun list-marks-text (list)
  "What the mode line says of the marks of LIST: a list of the text and its
face, \"3 marked, 1 for deletion\", or :NONE when no entry is marked. Counted
once after the marks change (NOTE-MARKS-CHANGED), then kept."
  (or (tuatara.buffer:buffer-local list 'list-marks-text)
      (setf (tuatara.buffer:buffer-local list 'list-marks-text)
            (multiple-value-bind (marked deletion) (list-mark-counts (buffer-major-mode list) list)
              (if (and (zerop marked) (zerop deletion))
                  :none
                  (list (cond ((zerop deletion) (format nil " ~d marked " marked))
                              ((zerop marked) (format nil " ~d for deletion " deletion))
                              (t (format nil " ~d marked, ~d for deletion " marked deletion)))
                        :list-marked))))))

(defun list-marks-mode-line (editor list)
  "Mode line function, just before the buffer's name: how many entries of a
list are marked (>) and marked for deletion (D), while any are."
  (declare (ignore editor))
  (let ((text (list-marks-text list)))
    (and (consp text) text)))

(defun install-list-marks ()
  "Have the mode line of every list with marks say how many entries are marked."
  (pushnew 'list-marks-mode-line *mode-line-name-functions*))

;;; Deleting and opening entries

(defgeneric list-entry-noun (mode)
  (:documentation "What one entry of a list in major mode MODE is called in what the
list says (\"buffer\", \"bookmark\"): no buffer on this line, no buffers marked
for deletion.")
  (:method (mode) (declare (ignore mode)) "entry"))

(defgeneric delete-entries (mode list keys mark)
  (:documentation "Delete the entries of LIST, a buffer in MODE, known by KEYS (what its
mark key gives of them, ENTRY-MARK-KEY), after asking: with MARK :DELETION
those marked for deletion (D), as \\[list-execute] does there, with :MARKED those
marked (>) or the one on the line, as \\[list-delete-marked] does.")
  (:method (mode list keys mark)
    "A list that deletes nothing says so."
    (declare (ignore mode list keys mark))
    (message "Nothing is deleted from this list")))

(defgeneric delete-list-entries (mode list mark)
  (:documentation "What \\[list-execute] (MARK :DELETION) and \\[list-delete-marked] (MARK :MARKED) do in
LIST, a buffer in MODE.")
  (:method ((mode marked-list-mode) list mark)
    "Those marked for deletion (D), or those marked (>) or with none marked the
one on the line (ACT-ON-LIST), deleted as the list deletes (DELETE-ENTRIES);
the preview beside the list then shows the entry on its line anew."
    (act-on-list list (lambda (list keys) (delete-entries mode list keys mark)) (list-entry-noun mode)
                 :deletion (eq mark :deletion))
    (entry-preview-again)))

(define-command list-execute ()
  "Delete the entries of the list marked for deletion (D), after asking, as the
list deletes: the buffer list kills its buffers, rimu puts files in the trash,
the list of jobs drops those waiting and stops those running."
  (delete-list-entries (buffer-major-mode (current-buffer)) (current-buffer) :deletion))

(define-command list-delete-marked ()
  "Delete the marked entries of the list (>), or with none marked the one on the
current line, after asking, as the list deletes, as \\[list-execute] does those
marked for deletion; in rimu, with \\[universal-argument], for good rather than
to the trash."
  (delete-list-entries (buffer-major-mode (current-buffer)) (current-buffer) :marked))

(defgeneric visit-entry (mode list entry other)
  (:documentation "Open ENTRY of LIST, a buffer in MODE, what its line shows: the buffer,
the bookmark's place, the file; with OTHER in another window. What
\\[list-visit] and \\[list-visit-other] do there; in a list in sections, ENTRY is a member of
a section (ENTRY-MEMBER).")
  (:method (mode list entry other)
    "A list whose entries name a file and line (ENTRY-PLACE) opens the file at
that line in another window, selected, or with OTHER the list kept selected;
a list whose entries open nothing says so."
    (declare (ignore list))
    (multiple-value-bind (file line) (entry-place mode entry)
      (if file
          (keeping-list-selected other (lambda () (show-file-line file line :other t)))
          (message "Nothing to open on this line")))))

(defun keeping-list-selected (keep function)
  "Call FUNCTION, which shows something in another window and selects it, and
return what it returns; with KEEP, the window selected before, the list's, is
selected again afterwards while it is still there."
  (let ((here (current-window)))
    (multiple-value-prog1 (funcall function)
      (when (and keep (member here (editor-windows)))
        (select-window here)))))

(defgeneric visit-section (mode list section other)
  (:documentation "What \\[list-visit] and \\[list-visit-other] do on the heading of SECTION in
LIST, a buffer in MODE.")
  (:method (mode list section other)
    "Fold or unfold the section."
    (declare (ignore mode other))
    (list-toggle-section list section)))

(defun visit-entry-at (other)
  "Open the entry on the current line of the list (VISIT-ENTRY), in another
window with OTHER; on a section's heading, what the list does there
\(VISIT-SECTION), folding or unfolding the section as standard."
  (let* ((list (current-buffer))
         (mode (buffer-major-mode list))
         (entry (list-entry-at list))
         (sectioned (typep mode 'sectioned-list-mode))
         (shown (if sectioned (entry-member entry) entry)))
    (cond ((and sectioned (eq (first entry) :section)) (visit-section mode list (second entry) other))
          (shown (visit-entry mode list shown other))
          (t (message "No ~a on this line" (list-entry-noun mode))))))

(define-command list-visit ()
  "Open the entry on the current line of the list: the buffer, the bookmark's
place, the file; on a section's heading, fold or unfold the section."
  (visit-entry-at nil))

(define-command list-visit-other ()
  "Open the entry on the current line of the list in another window; on a
section's heading, fold or unfold the section."
  (visit-entry-at t))
