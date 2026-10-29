;;;; lisp/modes/lisp-debug.lisp --- Errors shown with the calls under way: toggle-debug-on-error, *Backtrace*.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's debug-on-error: while it is on, an error in a command or in
;;;; Lisp evaluated in the editor shows, besides the message, the calls that
;;;; were under way when it was signalled, innermost first, in *Backtrace*.
;;;; The calls have ended by then (the editor has carried on), so there is no
;;;; continuing them as Emacs's debugger can: *Backtrace* is a record to read,
;;;; where RET (or M-.) goes to the definition of the function called on the
;;;; line and q closes it.

(in-package #:tuatara.defaults)

(define-major-mode backtrace-mode special-mode
  "The calls under way when an error was met, innermost first, shown in
*Backtrace* while debug-on-error is on: \\<backtrace-mode>\\[xref-find-definitions]
(or \\<global>\\[xref-find-definitions]) goes to the definition of the function
called on the line, \\[xref-go-back] comes back, \\<backtrace-mode>\\[quit-window]
closes the list."
  :lighter "Backtrace" :interactive nil)

(defvar *backtrace-keymap* (mode-keymap 'backtrace-mode)
  "Keys of *Backtrace* (backtrace-mode's keymap).")

(defun show-backtrace (condition frames)
  "Show CONDITION and FRAMES, the calls under way when it was signalled
(innermost first, as CAPTURE-BACKTRACE gives them), in *Backtrace*, and select
its window: the error as a heading, the calls' numbers dim."
  (let ((b (buffer-in-mode "*Backtrace*" 'backtrace-mode)))
    (setf (tuatara.buffer:buffer-local b :kind) :backtrace)
    ;; on the innermost call
    (render-pieces b (list* (cons (format nil "Debugger entered--Lisp error: ~a [~(~a~)]" (tuatara.runtime:condition-words condition) (type-of condition))
                                  :heading)
                            (string #\Newline)
                            (loop for frame in frames
                                  for i from 0
                                  collect (cons (format nil "~4d:" i) :line-number)
                                  collect (format nil " ~a~%" frame)))
                   :point (min 1 (length frames)))
    (pop-to-buffer b)))

(defun backtrace-call-name (buffer position)
  "The name of the function called on the line of *Backtrace* BUFFER holding
POSITION, as it is written there; NIL on the error's line, or for a call
without a name (a lambda, a top-level form)."
  (let* ((line (tuatara.buffer:buffer-line-start buffer position))
         (text (tuatara.buffer:buffer-string buffer :start line :end (tuatara.buffer:buffer-line-end buffer position)))
         (open (search ": (" text)))
    (when (and open (plusp line))
      (let* ((start (+ open 3))
             (end (or (position-if (lambda (c) (member c '(#\Space #\)))) text :start start) (length text)))
             (name (subseq text start end)))
        (and (plusp (length name)) (lisp-name-part-p (char name 0)) name)))))

(defmethod mode-name-at-point ((mode backtrace-mode) buffer position)
  "The name of the function called on the backtrace's line at POSITION."
  (backtrace-call-name buffer position))

(defmethod mode-find-definitions ((mode backtrace-mode) buffer name)
  "Where the function NAME, read in TUATARA-USER, is defined."
  (declare (ignore buffer))
  (let ((symbol (lisp-symbol-named name (find-package :tuatara-user))))
    (and symbol (lisp-definitions symbol))))

(defmethod mode-name-candidates ((mode backtrace-mode) buffer)
  "The names of the functions there are, to choose one to go to."
  (declare (ignore buffer))
  (function-candidates))

(define-command toggle-debug-on-error ()
  "Switch debug-on-error on or off: while it is on, an error in a command, or
in Lisp evaluated in the editor
(\\<lisp-mode>\\[eval-last-expression], \\[eval-top-level-form], \\[eval-expression],
\\<lisp-interaction-mode>\\[eval-print-last-expression]), shows the calls under way
when it was signalled in *Backtrace*, innermost first, besides the message.
With a count, on when it is positive, off when it is zero or less."
  (setf *debug-on-error* (switched-by-count *debug-on-error*))
  (message "Debug on Error ~:[disabled~;enabled~] globally" *debug-on-error*))

(defun install-backtrace ()
  "Show *Backtrace* for errors while *DEBUG-ON-ERROR* is on, and bind its keys."
  (setf *backtrace-function* 'show-backtrace)
  (define-key *backtrace-keymap* "RET" 'xref-find-definitions))
