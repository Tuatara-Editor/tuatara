;;;; lisp/org/checklist.lisp --- List items with a checkbox kept as tasks: ticked, clocked and logged.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An item with a checkbox, - [ ] call Anna Raglan, is a task as a TODO headline is:
;;;; the agenda lists it (an open box TODO, a ticked one DONE), and C-c C-t
;;;; ticks it, C-c C-x C-i clocks in on it and C-c C-z notes on it, on its line
;;;; in the file or on its line in the agenda. What is logged of it goes into a
;;;; :LOGBOOK: drawer of its own, set in under it, newest first, as Org lets a
;;;; drawer stand in an item; Emacs keeps those lines, and counts their time
;;;; towards the headline the item is under.

(in-package #:tuatara.defaults)

(defvar *org-log-checkboxes* t
  "True to log when a list item's checkbox is ticked or cleared, in a
:LOGBOOK: drawer under the item: - State \"DONE\" from \"TODO\" and the time,
as a TODO headline's change of state is logged; NIL to log nothing.")

(defun org-checkbox-item-at (tree text position)
  "The list item with a checkbox whose first line holds the character POSITION of TEXT, or NIL."
  (let ((item (org-item-on-line tree (string-line-start text position))))
    (and item (org-property item :checkbox) item)))

(defun org-checkbox-title (item text)
  "ITEM's text on its first line after its bullet and checkbox: what it is about."
  (let* ((line (org-line-at text (org-node-begin item)))
         (groups (nth-value 1 (cl-ppcre:scan-to-strings "^[ \\t]*\\S+[ \\t]+\\[[ X-]\\][ \\t]*(.*)$" line))))
    (string-trim " 	" (if groups (aref groups 0) line))))

(defun org-item-indent (item text)
  "The spaces lines under ITEM begin with: as far in as the text after its bullet."
  (let* ((line (org-line-at text (org-node-begin item)))
         (indent (- (length line) (length (string-left-trim " 	" line)))))
    (make-string (+ indent (length (string-trim " " (org-property item :bullet))) 1) :initial-element #\Space)))

(defun org-item-own-end (item)
  "Where ITEM's own text ends: at its first sub-item, else at its end."
  (let ((children (org-child-items item)))
    (if children (org-node-begin (first children)) (org-node-end item))))

(defun org-tick-taken-back-edit (item text)
  "The edit taking away the note of ITEM's tick when it is the newest of its
:LOGBOOK: drawer and was made this minute, the drawer too when nothing else
is in it; NIL otherwise."
  (let ((logbook (nth-value 1 (org-log-place item text))))
    (when logbook
      (let* ((line-end (string-line-end text logbook))
             (next (min (length text) (1+ line-end))))
        (when (string= (string-trim '(#\Space #\Tab) (subseq text logbook line-end))
                       (org-state-note "DONE" "TODO" (org-now)))
          (if (string-equal (string-trim '(#\Space #\Tab) (subseq text next (string-line-end text next))) ":END:")
              (list (string-line-start text (1- logbook)) (min (length text) (1+ (string-line-end text next))) "")
              (list logbook next "")))))))

(defun org-checkbox-state-edits (item text old new)
  "What logging ITEM's checkbox turned from the state OLD to NEW (OFF, TRANS,
ON) adds (*ORG-LOG-CHECKBOXES*): a note of DONE when it is ticked, of TODO
when a tick is taken away; NIL otherwise. A tick taken back the minute it
was made, as cycling the checkbox round does, takes its note away instead,
so that the log and the agenda's log mode keep no tick that did not stay."
  (when (and *org-log-checkboxes* (not (eq (eq old 'on) (eq new 'on))))
    (let ((taken-back (and (eq old 'on) (org-tick-taken-back-edit item text))))
      (list (or taken-back
                (org-log-edit item text (list (if (eq new 'on)
                                                  (org-state-note "DONE" "TODO" (org-now))
                                                  (org-state-note "TODO" "DONE" (org-now))))))))))

(defun call-on-checkbox-item (function)
  "When point is on the first line of a list item with a checkbox in the
current Org buffer, call FUNCTION with the buffer, its parse (tree and text),
point as a character of it and the item, and return true; else NIL."
  (let ((buffer (current-buffer)))
    (when (org-checkbox-line-p (org-line-head buffer (point)))
      (multiple-value-bind (tree text) (org-tree-now buffer)
        (let* ((*org-current-text* (or text ""))
               (position (org-point-char buffer))
               (item (and tree (org-checkbox-item-at tree text position))))
          (when item
            (funcall function buffer tree text position item)
            t))))))

(defvar *org-todo-follows-checkboxes* t
  "True for a TODO headline with a statistics cookie ([2/3], [66%]) to become
done by itself once all it counts are done, its TODO sub-headlines (or, with
none, the checkboxes right under it), with CLOSED: and the time, and not done
again when one is not
yet done; up the tree, a headline above with a cookie following in turn. A
task whose keyword the TODO keys pass over (*org-todo-keywords-not-cycled*),
such as one CANCELLED, is left as it is. NIL to leave the keyword to you; the
cookies are kept up to date either way.")

(defun org-summary-todo (buffer position)
  "After a checkbox or a sub-headline's TODO keyword changed at the character
POSITION of BUFFER's parse: the cookies of the headline it is under brought up
to date from its TODO sub-headlines, and that headline made done, with
CLOSED:, or not, as all it counts are done or not
\(*ORG-TODO-FOLLOWS-CHECKBOXES*); then the same for the headline above it
when this one changed; point kept where it is."
  (multiple-value-bind (tree text) (org-tree-now buffer)
    (let* ((*org-current-text* text)
           (headline (and tree (org-current-headline tree position))))
      (when headline
        (multiple-value-bind (total done) (org-cookie-counts headline)
          (let ((point (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer)
                                                 (tuatara.buffer:buffer-point buffer) :bias :before))
                (begin (org-node-begin headline))
                (changed nil))
            ;; its cookies, when it counts TODO sub-headlines
            (when (plusp (org-headline-todo-counts headline))
              (let ((edits (org-cookie-edits headline done total)))
                (when edits (org-apply-edits buffer edits :point (org-point-char buffer)))))
            (when (and *org-todo-follows-checkboxes* (org-property headline :todo-keyword) (org-cookies-of headline) (plusp total))
              (let* ((keyword (org-property headline :todo-keyword))
                     (sequence (org-keyword-sequence text))
                     (cycled (member keyword sequence :test #'string=))
                     (done-keywords (cdr (org-todo-keywords-of text)))
                     (is-done (member keyword done-keywords :test #'string=))
                     (target (cond ((not cycled) nil)
                                   ((and (= done total) (not is-done))
                                    (find-if (lambda (k) (member k done-keywords :test #'string=)) sequence))
                                   ((and is-done (< done total))
                                    (find-if-not (lambda (k) (member k done-keywords :test #'string=)) sequence)))))
                (when target
                  (setf (tuatara.buffer:buffer-point buffer) (org-char->byte buffer begin))
                  ;; done by itself: noted with CLOSED: and when, whatever *org-log-done* says
                  (let ((*org-log-done* :time))
                    (org-set-todo-to target))
                  (setf changed t))))
            (setf (tuatara.buffer:buffer-point buffer)
                  (tuatara.text:anchor-position point (tuatara.buffer:buffer-version buffer)))
            ;; the headline above, counting this one among its TODO sub-headlines
            (when changed
              (let ((parent (org-node-parent headline)))
                (when (and parent (eq (org-node-type parent) 'headline))
                  (org-summary-todo buffer (org-node-begin parent)))))))))))

(defun org-todo-changed (buffer)
  "After the TODO keyword of the headline point is on changed in BUFFER: the
headline above brought up to date, as ORG-SUMMARY-TODO does."
  (multiple-value-bind (tree text) (org-tree-now buffer)
    (let* ((*org-current-text* text)
           (headline (and tree (org-current-headline tree (org-point-char buffer))))
           (parent (and headline (org-node-parent headline))))
      (when (and parent (eq (org-node-type parent) 'headline))
        (org-summary-todo buffer (org-node-begin parent))))))
