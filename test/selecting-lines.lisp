;;;; test/selecting-lines.lisp --- The whole lines a selection touches, one way for every command.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test selecting-lines
  :description "The whole lines a selection touches: the same lines for sorting and filtering as for a line
selection's delete, a folded headline whole with its body.")

(defparameter *folded-lines* (format nil "b~%* head~%body1~%body2~%a~%")
  "Lines with a headline whose body (the two lines after it) a fold hides, from octet 8 to 20.")

(define-test (selecting-lines folded-headline-whole)
  ;; the selection ends on a folded headline: the lines it touches are the headline and
  ;; all its fold hides, for flush-lines as for a line selection
  (multiple-value-bind (f editor)
      (run-keys-in-turns '("") :text *folded-lines* :folds '((8 . 20)) :point 5 :mark 0)
    (declare (ignore f))
    (let ((b (session-buffer editor)))
      (is equal '(0 21) (multiple-value-list (tuatara.defaults::selected-lines b)))
      (is equal '(0 . 21) (tuatara.defaults::whole-lines-span b 0 5))))
  (is string= (format nil "b~%* head~%a~%")
      (session-text (nth-value 1 (run-keys-in-turns '("M-x f l u s h - l i n e s RET b o d y RET")
                                                    :text *folded-lines* :folds '((8 . 20)) :point 5 :mark 0))))
  ;; without a fold, the lines as they are
  (multiple-value-bind (f editor) (run-keys-in-turns '("") :text *folded-lines* :point 5 :mark 0)
    (declare (ignore f))
    (is equal '(0 9) (multiple-value-list (tuatara.defaults::selected-lines (session-buffer editor))))))
