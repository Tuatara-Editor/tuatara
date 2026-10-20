;;;; lisp/selecting/mark-run.lisp --- The mark run: a selection grown key by key, as the m prefix held down.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M marks a word and holds the m prefix down: w W b B s S grow the one
;;;; selection as m w and the rest do, h j k l adjust it, u and U step
;;;; through its history, and any other key ends the run and does its job
;;;; on what is selected.

(in-package #:tuatara.defaults)

(define-command mark-run-left ()
  "Move point back a character, or the count's, without ending the mark run."
  (char-motion (- (prefix))))

(define-command mark-run-right ()
  "Move point forward a character, or the count's, without ending the mark run."
  (char-motion (prefix)))

(define-command mark-run-down ()
  "Move point down a line, or the count's, without ending the mark run."
  (vertical-motion (prefix)))

(define-command mark-run-up ()
  "Move point up a line, or the count's, without ending the mark run."
  (vertical-motion (- (prefix))))

(pushnew 'mark-run-down *vertical-motion-commands*)
(pushnew 'mark-run-up *vertical-motion-commands*)

(define-command mark-run-line-start ()
  "Stretch the run's start back to the start of its line, or with no run go
there. \\<*mark-run-keymap*>\\[mark-run-line-start]
\\[mark-run-line-end] in a run is the whole line's text."
  (let ((b (current-buffer)) (extending (selection-continuing-p)))
    (goto (tuatara.buffer:buffer-line-start b (point)))
    (when extending (setf (tuatara.buffer:buffer-mark-active-p b) t))))

(define-command mark-run-line-end ()
  "Stretch the run's end on to the end of its line, or with no run go there."
  (let ((b (current-buffer)))
    (if (selection-continuing-p)
        (activate-mark-at (tuatara.buffer:buffer-line-end b (tuatara.buffer:buffer-mark b)))
        (goto (tuatara.buffer:buffer-line-end b (point))))))

(define-command mark-run-buffer-start ()
  "Stretch the run's start back to the start of the text, or with no run go there."
  (let ((b (current-buffer)) (extending (selection-continuing-p)))
    (goto 0)
    (when extending (setf (tuatara.buffer:buffer-mark-active-p b) t))))

(define-command mark-run-buffer-end ()
  "Stretch the run's end on to the end of the text, or with no run go there."
  (let ((b (current-buffer)))
    (if (selection-continuing-p)
        (activate-mark-at (tuatara.buffer:buffer-size b))
        (goto (tuatara.buffer:buffer-size b)))))

(define-command mark-run-line-forward ()
  "Select the line point is on, its line end included, or grow the run's end
down by whole lines: a run ending mid-line first takes the rest of that line."
  (let* ((b (current-buffer)) (n (max 1 (prefix))))
    (flet ((lines-down (p n) (let ((q p)) (dotimes (i n q) (setf q (next-line-start b q))))))
      (if (selection-continuing-p)
          (activate-mark-at (lines-down (tuatara.buffer:buffer-mark b) n))
          (let ((start (tuatara.buffer:buffer-line-start b (point))))
            (select-bounds (current-buffer) start (lines-down start n)))))
    (message "Line marked")))

(define-command mark-run-line-backward ()
  "Select the line point is on, or grow the run's start up by whole lines: a
run starting mid-line first takes the start of that line."
  (let* ((b (current-buffer)) (n (max 1 (prefix))))
    (flet ((line-up (p) (if (plusp p) (tuatara.buffer:buffer-line-start b (1- p)) 0)))
      (if (selection-continuing-p)
          (let ((p (point)))
            (if (= p (tuatara.buffer:buffer-line-start b p))
                (dotimes (i n) (setf p (line-up p)))
                (progn (setf p (tuatara.buffer:buffer-line-start b p))
                       (dotimes (i (1- n)) (setf p (line-up p)))))
            (goto p)
            (setf (tuatara.buffer:buffer-mark-active-p b) t))
          (let ((start (tuatara.buffer:buffer-line-start b (point))))
            (select-bounds (current-buffer) start (next-line-start b start))
            (let ((p start)) (dotimes (i (1- n)) (setf p (line-up p))) (goto p)))))
    (message "Line marked")))

(define-command mark-run-exchange ()
  "Swap point and the mark, so that
\\<*mark-run-keymap*>\\[mark-run-left] \\[mark-run-down]
\\[mark-run-up] \\[mark-run-right] adjust the run's other end."
  (let* ((b (current-buffer)) (m (tuatara.buffer:buffer-mark b)))
    (when m
      (setf (tuatara.buffer:buffer-mark b) (point)
            (tuatara.buffer:buffer-mark-active-p b) t)
      (goto m))))

(define-command mark-run-refuse ()
  "Refuse a key that would take the mark run's selection from it
(\\<*mark-run-keymap*>\\`v', \\[mark-run-refuse]): leave the run
first, with \\[mark-run-cancel] or \\[tuatara-quit]."
  (message "The mark run holds the selection: \\<*mark-run-keymap*>\\[mark-run-cancel] or \\[tuatara-quit] ends it first"))

(define-command mark-run-cancel ()
  "Drop the selection and end the mark run."
  (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) nil)
  (message "Mark run: cancelled"))

;;; History, for u and U

(defvar *mark-run-history* '()
  "The run's earlier shapes, newest first, each (POINT MARK ACTIVE): what
\\<*mark-run-keymap*>\\[mark-run-step-back] steps back to.")

(defvar *mark-run-redo* '()
  "The shapes \\<*mark-run-keymap*>\\[mark-run-step-back] stepped
back out of, newest first: what \\[mark-run-step-forward] steps forward
to.")

(defun selection-shape ()
  "The selection as it stands, (POINT MARK ACTIVE)."
  (let ((b (current-buffer)))
    (list (point) (tuatara.buffer:buffer-mark b) (tuatara.buffer:buffer-mark-active-p b))))

(defun restore-selection-shape (shape)
  "Put point, the mark and its activation back as SHAPE has them."
  (destructuring-bind (p m active) shape
    (let ((b (current-buffer)))
      (goto (min p (tuatara.buffer:buffer-size b)))
      (if (and m active)
          (setf (tuatara.buffer:buffer-mark b) (min m (tuatara.buffer:buffer-size b))
                (tuatara.buffer:buffer-mark-active-p b) t)
          (setf (tuatara.buffer:buffer-mark-active-p b) nil)))))

(define-command mark-run-step-back ()
  "Put the mark run back where its last press found it;
\\<*mark-run-keymap*>\\[mark-run-step-forward] puts it forward again."
  (unless *mark-run-history* (error "No earlier step in this run"))
  (push (selection-shape) *mark-run-redo*)
  (restore-selection-shape (pop *mark-run-history*)))

(define-command mark-run-step-forward ()
  "Put the mark run forward where
\\<*mark-run-keymap*>\\[mark-run-step-back] stepped it back from."
  (unless *mark-run-redo* (error "No later step in this run"))
  (push (selection-shape) *mark-run-history*)
  (restore-selection-shape (pop *mark-run-redo*)))

;;; The run itself

(defvar *mark-run-keymap* (make-keymap :name "mark run")
  "The keys of a mark run: \\<*mark-run-keymap*>\\[select-word]
\\[select-symbol] \\[select-word-backward]
\\[select-symbol-backward] \\[select-sentence]
\\[select-sentence-backward] grow the selection as
\\<normal>\\[select-word] and the rest do;
\\<*mark-run-keymap*>\\[mark-run-left] \\[mark-run-down]
\\[mark-run-up] \\[mark-run-right], \\[mark-run-line-start]
\\[mark-run-line-end], \\[mark-run-buffer-start] \\`g e' and
\\[mark-run-buffer-end] adjust or stretch it;
\\[mark-run-line-forward] and \\[mark-run-line-backward] take
whole lines; \\[mark-run-exchange] swaps its ends;
\\[mark-run-step-back] and \\[mark-run-step-forward] step through
the run; \\[repeat] repeats; \\`v' and \\[mark-run-refuse] are refused;
\\[mark-run-cancel] ends it. \\<normal>\\[select-paragraph] and
\\[select-paragraph-backward] still work, and grow the run by paragraphs.")

(defparameter *mark-run-bindings*
  '(("w" select-word) ("W" select-symbol) ("b" select-word-backward)
    ("B" select-symbol-backward) ("s" select-sentence) ("S" select-sentence-backward)
    ("M" mark-run-cancel) ("h" mark-run-left) ("l" mark-run-right)
    ("j" mark-run-down) ("k" mark-run-up)
    ("g h" mark-run-line-start) ("g l" mark-run-line-end)
    ("g g" mark-run-buffer-start) ("g e" mark-run-buffer-end)
    ("J" mark-run-line-forward) ("K" mark-run-line-backward)
    ("*" mark-run-exchange) ("u" mark-run-step-back) ("U" mark-run-step-forward)
    ("." repeat) ("v" mark-run-refuse) ("V" mark-run-refuse))
  "The keys of *MARK-RUN-KEYMAP*.")


(defparameter *mark-run-hint*
  (concatenate 'string
               "Mark run: \\<*mark-run-keymap*>"
               "\\[select-word]/\\[select-word-backward] words, "
               "\\[select-symbol]/\\[select-symbol-backward] symbols, "
               "\\[select-sentence]/\\[select-sentence-backward] sentences, "
               "\\[select-paragraph]/\\[select-paragraph-backward] paragraphs, "
               "\\[mark-run-exchange] other end, \\[mark-run-cancel] to cancel")
  "What the echo area says while a mark run lasts.")

(defun mark-run-member-p (command)
  "True when COMMAND continues a mark run: an object key, a run's own key, or
one that does nothing."
  (or (member command *selection-object-commands*)
      (member command *mark-run-adjusters*)
      (member command *mark-run-inert*)))

(defun pressed-command (command)
  "The command a press stands for: for repeat, the one it repeats."
  (if (eq command 'repeat)
      (car (editor-repeatable *editor*))
      command))

(defun mark-run-keep-p (command)
  "True while the mark run lasts: COMMAND (or what repeat repeats) is one of
the run's, or starts a count."
  (or (mark-run-member-p (pressed-command command))
      (eq command 'universal-argument)))

(defun mark-run-record (editor command)
  "Before a press of a mark run changes the selection, note it for
\\<*mark-run-keymap*>\\[mark-run-step-back]."
  (when (eq (editor-transient-map editor) *mark-run-keymap*)
    (let ((command (pressed-command command)))
      (when (and (mark-run-member-p command)
                 (not (member command *mark-run-inert*))
                 (not (member command '(mark-run-step-back mark-run-step-forward))))
        (push (selection-shape) *mark-run-history*)
        (setf *mark-run-redo* '())))))

(defun mark-run-remind (editor)
  "While a mark run lasts, keep its reminder in the echo area, over the
object keys' Word marked and the like, though not over a complaint."
  (when (eq (editor-transient-map editor) *mark-run-keymap*)
    (let ((said (editor-message editor)))
      (when (or (null said) (and (> (length said) 7) (string= " marked" said :start2 (- (length said) 7))))
        (echo "~a" (substitute-command-keys *mark-run-hint* :marks t))))))

(defun mark-run-exit ()
  "Forget the run's history once it ends."
  (setf *mark-run-history* '() *mark-run-redo* '()))

(defun mark-run-enter ()
  "Start a mark run: its keys first, its reminder shown."
  (set-transient-map *editor* *mark-run-keymap* #'mark-run-keep-p #'mark-run-exit)
  (setf *mark-run-history* '() *mark-run-redo* '())
  (echo "~a" (substitute-command-keys *mark-run-hint* :marks t)))

(define-command mark-run-adopt ()
  "Take the selection into a mark run: its keys then grow it from where it is."
  (multiple-value-bind (start end) (selection-range :widen t)
    (unless (and start (< start end)) (error "No selection to adopt"))
    (select-bounds (current-buffer) start end)
    (mark-run-enter)))

(define-command mark-run-toggle ()
  "Start a mark run: the \\`m' prefix held down. \\[mark-run-toggle]
marks the word at point (from a gap, the one ahead), and then
\\<*mark-run-keymap*>\\[select-word] \\[select-symbol]
\\[select-word-backward] \\[select-symbol-backward]
\\[select-sentence] \\[select-sentence-backward] grow it as
\\<normal>\\[select-word] \\[select-symbol]
\\[select-word-backward] \\[select-symbol-backward]
\\[select-sentence] \\[select-sentence-backward] do:
\\[mark-run-toggle] \\<*mark-run-keymap*>\\[select-word]
\\[select-word-backward] selects what \\<normal>\\[select-word]
\\[select-word] \\[select-word-backward] selects.
\\<*mark-run-keymap*>\\[mark-run-left] \\[mark-run-down]
\\[mark-run-up] \\[mark-run-right] move point without ending the
run, \\[mark-run-line-start] \\[mark-run-line-end]
\\[mark-run-buffer-start] \\`g e' stretch it to the line's or text's
edges, \\[mark-run-line-forward] and \\[mark-run-line-backward]
take whole lines, \\[mark-run-exchange] swaps its ends,
\\[mark-run-step-back] and \\[mark-run-step-forward] step back and
forward through it, and \\[mark-run-cancel] ends it. Any other key ends
the run and does its job on what is selected: \\[mark-run-toggle]
\\[select-word] \\[delete-selection-or-char] deletes two words; a key that does
nothing leaves the run as it is. With a selection already made,
\\[mark-run-toggle] takes it into the run; a rectangle, which has no end
for the object keys to grow, is let go instead, and the run starts on the word
at point. \\[select-whole-buffer] in a run selects the whole text and the
run goes on."
  (let ((b (current-buffer)))
    (when (rectangle-live-p b)
      (setf (tuatara.buffer:buffer-mark-active-p b) nil))
    (multiple-value-bind (start end) (selection-range)
      (if (and start (< start end))
          (progn (setf (editor-this-command *editor*) 'mark-run-adopt)
                 (mark-run-adopt))
          (progn
            (setf (tuatara.buffer:buffer-mark-active-p b) nil)
            (let ((last (editor-last-command *editor*)))
              (setf (editor-last-command *editor*) nil)
              (handler-case (progn (let ((argument (prefix-argument)))
                                     (setf (editor-prefix-argument *editor*) nil)
                                     (unwind-protect (select-word)
                                       (setf (editor-prefix-argument *editor*) argument)))
                                   (setf (editor-this-command *editor*) 'select-word))
                (error () (setf (editor-last-command *editor*) last))))
            (mark-run-enter))))))

(defun install-mark-run (editor)
  "Bind the mark run's keys and have EDITOR note its steps and show its reminder."
  (define-keys *mark-run-keymap* *mark-run-bindings*)
  (pushnew 'mark-run-record (editor-pre-command-functions editor))
  (pushnew 'mark-run-remind (editor-post-command-functions editor)))
