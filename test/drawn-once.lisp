;;;; test/drawn-once.lisp --- What every frame shows worked out once a frame: line numbers, the modal mode line.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test drawn-once
  :description "The gutter's line numbers find the window's current line once a frame, not once a row; the
modal mode line makes nothing while it stays the same.")

(define-test (drawn-once line-numbers-a-frame)
  ;; the window's current line is looked up for the frame, not for each row's number
  (multiple-value-bind (f editor)
      (run-keys (list "M-x d i s p l a y - r e l a t i v e - l i n e - n u m b e r s - m o d e RET" "C-n C-n")
                :text (format nil "~{~a~%~}" (loop for i below 200 collect i)))
    (declare (ignore f))
    (let* ((window (tuatara.command:editor-window editor))
           (frame (tuatara.defaults::line-numbers-prepare editor window nil))
           (calls 0))
      (sb-int:encapsulate 'tuatara.buffer:buffer-known-line-number 'counted
                          (lambda (function &rest arguments) (incf calls) (apply function arguments)))
      (unwind-protect
           (progn
             (is equal '("2" "3" "3") (loop for line in '(0 2 5)
                                            collect (tuatara.defaults::line-numbers-cell editor window nil line frame)))
             (dotimes (line 100) (tuatara.defaults::line-numbers-cell editor window nil line frame)))
        (sb-int:unencapsulate 'tuatara.buffer:buffer-known-line-number 'counted))
      (is = 0 calls))))

(define-test (drawn-once modal-mode-line-kept)
  (multiple-value-bind (f editor) (run-keys "" :text "kia ora")
    (declare (ignore f))
    (let ((window (tuatara.command:editor-window editor)))
      (tuatara.defaults::modal-mode-line editor window)
      (let ((modal (least-bytes (lambda () (tuatara.defaults::modal-mode-line editor window)) 2000))
            (default (least-bytes (lambda () (tuatara.defaults::default-mode-line editor window)) 2000)))
        (true (< modal (+ default 32)) "~,1f octets a mode line, ~,1f for the default under it" modal default)
        (is eq (tuatara.defaults::modal-mode-line editor window) (tuatara.defaults::modal-mode-line editor window))))))
