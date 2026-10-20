;;;; lisp/editing/selection.lisp --- Selecting by the syntax tree.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Selections that follow the structure of the code, as Helix makes them:
;;;; the function, type, parameter or comment around point, by each
;;;; language's selection rules (syntax/textobjects/); and a selection grown
;;;; to the piece of syntax around it, shrunk back, or moved to the piece
;;;; beside it.

(in-package #:tuatara.defaults)

;;; What a selection is

(defun selection-bounds (buffer)
  "The selection of BUFFER, its start and end as two values: the region when
it is active, else the character at point."
  (if (tuatara.buffer:buffer-mark-active-p buffer)
      (tuatara.buffer:buffer-region buffer)
      (let ((p (tuatara.buffer:buffer-point buffer)))
        (values p (tuatara.buffer:buffer-forward-char buffer p 1)))))

(defun selected-region (buffer)
  "BUFFER's selection as a list (START END) of octets when text is selected,
the region active and not empty; else NIL."
  (and (region-active-p buffer)
       (let ((region (multiple-value-list (tuatara.buffer:buffer-region buffer))))
         (and (< (first region) (second region)) region))))

(defun select-bounds (buffer start end &optional forward)
  "Make the region of BUFFER the text from START below END, point at START,
or at END when FORWARD: a plain selection, whatever kind the one before was
\(FRESH-SELECTION)."
  (fresh-selection buffer)
  (setf (tuatara.buffer:buffer-mark buffer) (if forward start end)
        (tuatara.buffer:buffer-mark-active-p buffer) t)
  (goto (if forward end start)))

;;; Functions, types, parameters, comments, tests, entries and elements

(defparameter *text-objects*
  '(("function" . "function") ("class" . "type") ("parameter" . "parameter") ("comment" . "comment")
    ("test" . "test") ("entry" . "entry") ("xml-element" . "element"))
  "The kinds of text object, each the name its rules capture under and the
word said for it.")

(defun disable-unused-patterns (query)
  "Turn off the patterns of QUERY that capture none of *TEXT-OBJECTS*, which
would only make the query slower."
  (let* ((octets (sb-ext:string-to-octets (tuatara.syntax:query-source query) :external-format :utf-8))
         (starts (tuatara.syntax:query-starts query))
         (wanted (loop for (name) in *text-objects* collect (format nil "@~a." name))))
    (dotimes (i (length starts))
      (let ((text (sb-ext:octets-to-string octets :start (aref starts i)
                                                  :end (if (< (1+ i) (length starts)) (aref starts (1+ i)) (length octets))
                                                  :external-format :utf-8)))
        (unless (some (lambda (capture) (search capture text)) wanted)
          (tuatara.syntax:disable-query-pattern query i))))))

(defun text-object-ranges (query tree rope name start end)
  "What QUERY, a language's selection rules, captures as NAME (such as
function.inside) in TREE, the tree of ROPE, in the matches touching octets
START below END: a list of (START . END), the nodes one match captures as NAME
taken together."
  (let ((number (position name (tuatara.syntax:query-captures query) :test #'string=))
        (ranges '()))
    (when number
      (tuatara.syntax:map-query-matches
       (lambda (pattern captures count)
         (declare (ignore pattern))
         (let ((first nil) (last nil))
           (dotimes (i count)
             (when (= number (aref captures (* 4 i)))
               (let ((s (aref captures (+ 1 (* 4 i)))) (e (aref captures (+ 2 (* 4 i)))))
                 (setf first (if first (min first s) s)
                       last (if last (max last e) e)))))
           (when first (push (cons first last) ranges))))
       query tree rope start end))
    ranges))

(defun smallest-around (ranges start end selected)
  "The smallest of RANGES, each (START . END), holding octets START below END:
more than them when SELECTED (they are a selection already), else holding
the character at START."
  (let ((best nil))
    (dolist (r ranges best)
      (when (and (<= (car r) start)
                 (if selected
                     (and (<= end (cdr r)) (not (and (= (car r) start) (= (cdr r) end))))
                     (< start (cdr r)))
                 (or (null best) (< (- (cdr r) (car r)) (- (cdr best) (car best)))))
        (setf best r)))))

(defun mark-text-object (object inner)
  "Select the OBJECT around point, one of *TEXT-OBJECTS* by the name its rules
capture under, or the one around the selection: inside it when INNER, such as
a function's body, else all of it. With \\[universal-argument] N, the Nth
out."
  (let* ((b (current-buffer))
         (word (cdr (assoc object *text-objects* :test #'string=)))
         (language (mode-language (buffer-major-mode b)))
         (query (and language (language-compiled-query language :text-objects)))
         (syntax (and query (ready-syntax b))))
    (if (null syntax)
        (message "No syntax tree to select by")
        (let* ((rope (tuatara.buffer:buffer-text b))
               (active (tuatara.buffer:buffer-mark-active-p b)))
          (multiple-value-bind (start end) (if active (tuatara.buffer:buffer-region b) (values (point) (point)))
            (let ((ranges (text-object-ranges query (tuatara.syntax:syntax-tree syntax) rope
                                              (format nil "~a.~:[around~;inside~]" object inner)
                                              (max 0 (- start 65536))
                                              (min (tuatara.text:rope-bytes rope) (+ end 65536))))
                  (found nil))
              (dotimes (i (max 1 (prefix)))
                (let ((next (smallest-around ranges start end (or active found))))
                  (unless next (return))
                  (setf found next start (car next) end (cdr next))))
              (if found
                  (progn (select-bounds b (car found) (cdr found))
                         (message "Marked ~:[outer~;inner~] ~a" inner word))
                  (message "No ~a around point" word))))))))

;;; Growing, shrinking and moving the selection

(defun selection-history (buffer)
  "The selections of BUFFER before each growing of it that is still in force,
the newest first: each (START END FORWARD ACTIVE); NIL once the text has
changed since."
  (let ((kept (tuatara.buffer:buffer-local buffer 'selection-history)))
    (and kept (eq (car kept) (tuatara.buffer:buffer-text buffer)) (cdr kept))))

(defun (setf selection-history) (history buffer)
  "Keep HISTORY as the selections of BUFFER before each growing of it."
  (setf (tuatara.buffer:buffer-local buffer 'selection-history)
        (cons (tuatara.buffer:buffer-text buffer) history))
  history)

(defun selection-forward-p (buffer)
  "True when point is at the end of BUFFER's active region."
  (and (tuatara.buffer:buffer-mark-active-p buffer)
       (> (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-mark buffer))))

(defvar *selection-ranges-functions* '()
  "Functions giving, for a buffer with no syntax tree and a position in it,
the pieces of its text around the position, the smallest first, each (START
. END), or NIL: how \\[expand-selection] grows the selection there (as a
language server sees the code).")

(defun expand-selection-by-ranges (b)
  "Grow B's selection to the next piece around it *SELECTION-RANGES-FUNCTIONS*
give, as many steps as the prefix says; true when it grew."
  (multiple-value-bind (start end) (selection-bounds b)
    (let ((ranges (some (lambda (f) (funcall f b (tuatara.buffer:buffer-point b))) *selection-ranges-functions*))
          (forward (selection-forward-p b))
          (history (selection-history b))
          (was (list start end (selection-forward-p b) (tuatara.buffer:buffer-mark-active-p b)))
          (grown nil))
      (dotimes (i (max 1 (prefix)))
        (let ((next (find-if (lambda (r) (and (<= (car r) start) (<= end (cdr r)) (< (- end start) (- (cdr r) (car r)))))
                             ranges)))
          (unless next (return))
          (push was history)
          (setf start (car next) end (cdr next) grown t
                was (list start end forward t))))
      (when grown
        (select-bounds b start end forward)
        (setf (selection-history b) history))
      grown)))

(define-command expand-selection ()
  "Select the smallest piece of syntax around the selection, or around the
character at point: the word, then the expression holding it, then the
statement, and so on out; in a buffer with no syntax tree, as its language
server sees the code. With \\[universal-argument] N, N steps out;
\\[shrink-selection] goes back."
  (let* ((b (current-buffer))
         (syntax (ready-syntax b)))
    (if (null syntax)
        (unless (expand-selection-by-ranges b)
          (message "No syntax tree to select by"))
        (multiple-value-bind (start end) (selection-bounds b)
          (let ((forward (selection-forward-p b))
                (history (selection-history b))
                (was (list start end (selection-forward-p b) (tuatara.buffer:buffer-mark-active-p b)))
                (grown nil))
            (dotimes (i (max 1 (prefix)))
              (multiple-value-bind (s e) (tuatara.syntax:select-node syntax start end :larger)
                (unless s (return))
                (push was history)
                (setf start s end e grown t
                      was (list s e forward t))))
            (if grown
                (progn (select-bounds b start end forward)
                       (setf (selection-history b) history))
                (message "Nothing larger to select")))))))

(define-command shrink-selection ()
  "Go back to the selection before the last \\[expand-selection], or else
select the first piece of syntax inside the selection; without a selection,
the smallest piece of syntax at point. With \\[universal-argument] N, N steps.
In a buffer with no syntax tree, back through the growings alone."
  (let* ((b (current-buffer))
         (syntax (ready-syntax b)))
    (if (and (null syntax) (null (selection-history b)))
        (message "No syntax tree to select by")
        (let ((history (selection-history b))
              (done nil))
          (dotimes (i (max 1 (prefix)))
            (multiple-value-bind (start end) (selection-bounds b)
              (let ((previous (pop history)))
                (cond ((and previous (tuatara.buffer:buffer-mark-active-p b)
                            (<= start (first previous)) (<= (second previous) end))
                       ;; back to what the last growing started from
                       (destructuring-bind (s e forward active) previous
                         (if active
                             (select-bounds b s e forward)
                             (progn (setf (tuatara.buffer:buffer-mark-active-p b) nil) (goto s))))
                       (setf done t))
                      (t
                       (setf history '())
                       (unless syntax (return))
                       (multiple-value-bind (s e)
                           (tuatara.syntax:select-node syntax start end
                                                       (if (tuatara.buffer:buffer-mark-active-p b) :first-child :smallest))
                         (unless s (return))
                         (select-bounds b s e (selection-forward-p b))
                         (setf done t)))))))
          (setf (selection-history b) history)
          (unless done (message "Nothing smaller to select"))))))

(defun step-selection (how)
  "Select the piece of syntax beside the selection, or beside the one at point:
the next one when HOW is :NEXT, the one before when :PREVIOUS, going out a
level at the last one; \\[universal-argument] N steps N times."
  (let* ((b (current-buffer))
         (syntax (ready-syntax b)))
    (if (null syntax)
        (message "No syntax tree to select by")
        (multiple-value-bind (start end) (selection-bounds b)
          (let ((moved nil))
            (dotimes (i (max 1 (prefix)))
              (multiple-value-bind (s e) (tuatara.syntax:select-node syntax start end how)
                (unless s (return))
                (setf start s end e moved t)))
            (if moved
                (progn (select-bounds b start end (eq how :next))
                       (setf (selection-history b) '()))
                (message "Nothing ~:[before~;after~] it to select" (eq how :next))))))))

(define-command select-next-sibling ()
  "Select the next piece of syntax beside the selection, or beside the one at
point: the next argument, statement or item, and after the last one, the next
beside the piece around them. With \\[universal-argument] N, N steps."
  (step-selection :next))

(define-command select-previous-sibling ()
  "Select the piece of syntax before the selection, or before the one at
point: the argument, statement or item before it, and before the first one,
the one before the piece around them. With \\[universal-argument] N, N steps."
  (step-selection :previous))
