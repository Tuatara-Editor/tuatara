;;;; lisp/editing/questions.lisp --- Asking for a number, and the histories of answers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The questions many commands ask in the minibuffer beside a line of text:
;;;; a whole number, a decimal or a fraction. A pattern is asked for in
;;;; lisp/search/questions.lisp.

(in-package #:tuatara.defaults)

(defparameter *history-kept* 200
  "How many of the newest entries a history kept while the editor runs holds:
the commands run by name, the patterns typed, the places definitions were
looked up from.")

(defun pushed-and-kept (item history)
  "HISTORY, a list, with ITEM pushed on it, no more than its newest
*HISTORY-KEPT* entries."
  (let ((history (cons item history)))
    (when (> (length history) *history-kept*)
      (setf (cdr (nthcdr (1- *history-kept*) history)) nil))
    history))

(defun read-number-as (parse prompt default valid refusal)
  "Ask for a number in the minibuffer, read from what is typed by PARSE, a
function of the text giving the number or NIL; PROMPT a format string given
DEFAULT, the number offered: the number typed, or DEFAULT when nothing is.
Asks again, saying REFUSAL first, while PARSE gives no number that VALID, a
function of it, is true for."
  (loop for asking = (format nil prompt default) then (format nil "~a ~?" refusal prompt (list default))
        do (let* ((text (read-string asking :trim t))
                  (n (and (plusp (length text)) (funcall parse text))))
             (cond ((zerop (length text)) (return default))
                   ((and n (funcall valid n)) (return n))))))

(defun read-whole-number (prompt default &key (valid #'plusp) (refusal "Not a whole number above 0."))
  "Ask for a whole number in the minibuffer, PROMPT a format string given
DEFAULT, the number offered: the number typed, or DEFAULT when nothing is.
Asks again, saying REFUSAL first, while what is typed is no whole number that
VALID, a function of it, is true for (as standard, more than 0)."
  (read-number-as (lambda (text) (and (every #'digit-char-p text) (parse-integer text)))
                  prompt default valid refusal))

(defun read-decimal-number (prompt default &key (valid #'plusp) (refusal "Not a number above 0."))
  "Ask for a number in the minibuffer, digits with perhaps a point and more
digits after (0.8), PROMPT a format string given DEFAULT, the number offered:
the number typed, exact, or DEFAULT when nothing is. Asks again, saying
REFUSAL first, while what is typed is no such number that VALID, a function
of it, is true for (as standard, more than 0)."
  (read-number-as #'tuatara.text:parse-decimal prompt default valid refusal))

(defun fraction-p (value)
  "True when VALUE is a number above 0 and at most 1, as a share or a
confidence is."
  (and (realp value) (< 0 value) (<= value 1)))

(defun read-fraction (prompt default)
  "Ask for a number above 0 and at most 1 (FRACTION-P) in the minibuffer, as
READ-DECIMAL-NUMBER asks, PROMPT a format string given DEFAULT, the number
offered."
  (read-decimal-number prompt default :valid #'fraction-p :refusal "Not a number above 0 and at most 1."))
