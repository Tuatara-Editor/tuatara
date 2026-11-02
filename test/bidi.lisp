;;;; test/bidi.lisp --- Right-to-left text: the order lines are shown in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test bidi
  :description "Right-to-left text: levels and order as the Unicode Bidirectional Algorithm gives them.")

(defun bidi-case (line)
  "The levels and the order shown of LINE, written as a line of the Unicode
conformance file BidiCharacterTest.txt, as Tuatara works them out and as the
line wants them: four values, levels (NIL for a character left out) and
order, worked out then wanted."
  (destructuring-bind (codes direction level levels order) (uiop:split-string line :separator ";")
    (declare (ignore level))
    (flet ((numbers (text radix)
             (mapcar (lambda (word) (if (string= word "x") nil (parse-integer word :radix radix)))
                     (remove "" (uiop:split-string text :separator " ") :test #'string=))))
      (let* ((codes (coerce (numbers codes 16) 'vector))
             (classes (map 'simple-vector #'tuatara.display::code-bidi-class codes))
             (removed (map 'list (lambda (class) (member class '(:lre :rle :lro :rlo :pdf :bn))) classes)))
        (multiple-value-bind (resolved paragraph-level)
            (tuatara.display::bidi-levels classes :codes codes
                                                  :paragraph-level (case (parse-integer direction) (0 0) (1 1)))
          (let ((shown (tuatara.display::line-levels classes resolved paragraph-level)))
            (values (loop for level across shown for gone in removed collect (if gone nil level))
                    (loop for i across (tuatara.display::visual-order shown) unless (nth i removed) collect i)
                    (numbers levels 10)
                    (numbers order 10))))))))

(define-test (bidi levels-as-unicode-gives-them)
  ;; lines of BidiCharacterTest.txt (Unicode 15.0): brackets, marks, numbers, isolates, embeddings, overrides
  (dolist (line '("05D0 0028 05D1 0029 0331;0;0;1 1 1 1 1;4 3 2 1 0"
                  "05D0 0028 0029 2680 0061 0028 0029 2681;1;1;1 1 1 1 2 1 1 1;7 6 5 4 3 2 1 0"
                  "05D0 0028 2680 0061 2681 0028 0029 0029;1;1;1 1 1 2 1 1 1 1;7 6 5 4 3 2 1 0"
                  "05D0 202A 05D1 202C 0020 0031 0020 0032;0;0;1 x 3 x 0 0 0 0;2 0 4 5 6 7"
                  "0061 202B 0062 202C 0020 0031 0020 0032;1;1;2 x 4 x 1 2 1 2;7 6 5 4 0 2"
                  #.(concatenate 'string "202E 0061 202A 0062 202C 2066 0063 2069 202A 0064 202C 0065 202C;2;0;"
                                 "x 1 x 2 x 1 2 1 x 2 x 1 x;11 9 7 6 5 3 1")
                  "202B 0061 0028 0062 202C 202E 0029;2;0;x 2 2 2 x x 1;6 1 2 3"))
    (multiple-value-bind (levels order wanted-levels wanted-order) (bidi-case line)
      (is equal wanted-levels levels "levels of ~a" line)
      (is equal wanted-order order "order of ~a" line))))

(define-test (bidi right-to-left-characters-all-found)
  ;; every character of a class that can change a line's order is found, and none other
  (let ((wrong (loop for code from 0 below char-code-limit
                     unless (eq (tuatara.display::right-to-left-code-p code)
                                (and (member (tuatara.display::code-bidi-class code)
                                             '(:r :al :an :lre :rle :lro :rlo :pdf :lri :rli :fsi :pdi))
                                     t))
                       collect code)))
    (is equal '() wrong)))
