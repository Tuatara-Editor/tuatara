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

(define-test (drawn-once line-numbers-counted-on)
  ;; each row's number is counted on from the row before, not looked up afresh,
  ;; and lines hidden by a fold are passed over
  (let ((tuatara-user:*line-numbers-default* :absolute))
    (multiple-value-bind (f editor) (run-keys "" :text (format nil "~{l~a~%~}" (loop for i below 200 collect i)) :height 12)
      (declare (ignore f))
      (let* ((window (tuatara.command:editor-window editor))
             (b (tuatara.display:window-buffer window))
             (calls 0))
        (tuatara.display:fold-region b (tuatara.buffer:buffer-line-end b (tuatara.buffer:buffer-goto-line b 2))
                                     (tuatara.buffer:buffer-line-end b (tuatara.buffer:buffer-goto-line b 6)))
        (setf (tuatara.display:window-start window) 0)
        (tuatara.display:window-layout window)
        (sb-int:encapsulate 'tuatara.buffer:buffer-known-line-number 'counted
                            (lambda (function &rest arguments) (incf calls) (apply function arguments)))
        (unwind-protect
             (let ((cells (tuatara.command::gutter-cells editor window (tuatara.display:window-rows window))))
               (is equal '("1" "2" "3" "8" "9" "10")
                   (loop for row in cells repeat 6
                         collect (string-trim " " (car (first row))))))
          (sb-int:unencapsulate 'tuatara.buffer:buffer-known-line-number 'counted))
        ;; the first row's, and the line point is on
        (true (<= calls 2) "~d line numbers looked up for a frame" calls)))))

(define-test (drawn-once modal-mode-line-kept)
  (multiple-value-bind (f editor) (run-keys "" :text "kia ora")
    (declare (ignore f))
    (let ((window (tuatara.command:editor-window editor)))
      (tuatara.defaults::modal-mode-line editor window)
      (let ((modal (least-bytes (lambda () (tuatara.defaults::modal-mode-line editor window)) 2000))
            (default (least-bytes (lambda () (tuatara.defaults::default-mode-line editor window)) 2000)))
        (true (< modal (+ default 32)) "~,1f octets a mode line, ~,1f for the default under it" modal default)
        (is eq (tuatara.defaults::modal-mode-line editor window) (tuatara.defaults::modal-mode-line editor window))))))

(tuatara.modes:define-major-mode modal-kept-mode tuatara.defaults::text-mode
  "A mode for the tests: a parent whose modal class the test changes."
  :interactive nil)

(tuatara.modes:define-major-mode modal-kept-child-mode modal-kept-mode
  "A mode for the tests: a child that inherits its parent's modal class."
  :interactive nil)

(define-test (drawn-once modal-class-known-once)
  (let ((mode (make-instance 'modal-kept-child-mode)))
    (is eq :editing (tuatara.modes:mode-modal-class mode))
    (true (< (least-bytes (lambda () (tuatara.modes:mode-modal-class mode)) 2000) 1)
          "a mode's modal class is looked up, not worked out again")
    (unwind-protect
         (progn
           (tuatara.modes:register-major-mode 'modal-kept-mode 'tuatara.defaults::text-mode "Changed."
                                              :modal-class :support)
           (is eq :support (tuatara.modes:mode-modal-class mode)))
      (tuatara.modes:register-major-mode 'modal-kept-mode 'tuatara.defaults::text-mode
                                         "A mode for the tests: a parent whose modal class the test changes."))
    (is eq :editing (tuatara.modes:mode-modal-class mode))))
