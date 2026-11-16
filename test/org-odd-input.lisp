;;;; test/org-odd-input.lisp --- Org's tables and blocks given odd text from a file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun odd-input-formula-outcome (texts &optional (rows (list (list "1" "2" ""))))
  "What the table formulas TEXTS make of ROWS: the rows worked out, :REFUSED
when a formula is refused in words, or :LISP-ERROR for any other error."
  (handler-case
      (let ((model (tuatara.defaults::make-org-table-model :rows (mapcar #'copy-list rows) :indent "" :row 0
                                                           :field 0 :offset 0))
            (formulas (tuatara.defaults::org-runnable-formulas texts)))
        (if formulas
            (progn (tuatara.defaults::org-apply-formulas model formulas)
                   (tuatara.defaults::org-table-rows model))
            :refused))
    (tuatara.defaults::org-formula-error () :refused)
    (error () :lisp-error)))

(define-test (org regexp-back-reference-before-digit)
  ;; \1 then the digit 0 is the first group then a 0, never a tenth group
  (let ((translated (tuatara.defaults::org-regexp-to-ppcre "\\(a\\)\\10")))
    (true translated)
    (true (and translated (cl-ppcre:scan translated "aa0")))
    (false (and translated (cl-ppcre:scan translated "aa")))))

(define-test (org formulas-odd-references)
  ;; a row or column number in a larger reference, and a range of no references, are refused in words
  (is eq :refused (odd-input-formula-outcome (list "$3=@#$2")))
  (is eq :refused (odd-input-formula-outcome (list "$3=@1$#")))
  (is eq :refused (odd-input-formula-outcome (list "x..y=1")))
  ;; the row number alone still works
  (is equal (list (list "1" "2" "1")) (odd-input-formula-outcome (list "$3=@#"))))

(define-test (org formulas-past-doubles)
  ;; a number too large for the machine's floats shows #ERROR, with the float traps off as in the window too
  (let ((rows (list (list (format nil "1~a" (make-string 400 :initial-element #\0)) ""))))
    (dolist (formula (list "$2=$1;%e" "$2=$1;%.2f" "$2=sqrt($1*1.5)" "$2=exp(1000)"))
      (is equal (list (list (first (first rows)) "#ERROR"))
          (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact)
            (odd-input-formula-outcome (list formula) rows))
          "~a" formula))))
