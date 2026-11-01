;;;; test/registers.lisp --- Registers: places, text, rectangles and numbers kept under one character.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test registers
  :description "C-x r s keeps the selection, C-x r p a place, C-x r n a number under a character; C-x r i puts
text, a rectangle or a number in, C-x r j goes back to a place, C-x r + adds, C-x r l lists them.")

(defmacro with-clean-registers (&body body)
  "Run BODY, no register kept before it, none after."
  `(let ((tuatara.defaults::*modal-editing* nil))
     (clrhash tuatara.defaults::*registers*)
     (unwind-protect (progn ,@body)
       (forget-file-buffers)
       (clrhash tuatara.defaults::*registers*))))

(defun register-value-of (name)
  "What the register called NAME keeps, or NIL for none."
  (let ((register (gethash name tuatara.defaults::*registers*)))
    (and register (tuatara.defaults::register-value register))))

(define-test (registers text-kept-and-put-in)
  (with-clean-registers
    ;; the first word kept; put in at the end; the kill ring is as it was
    (multiple-value-bind (f editor) (run-keys "C-SPC M-f C-x r s a C-e C-x r i a" :text "one two")
      (declare (ignore f))
      (is string= "one twoone" (session-text editor))
      (is string= "one" (register-value-of #\a))
      (false (tuatara.command:editor-kill-ring editor)))
    ;; a capital is another register
    (run-keys "M-f C-SPC M-f C-x r s A" :text "one two")
    (is string= " two" (register-value-of #\A))
    (is string= "one" (register-value-of #\a))))

(define-test (registers text-added-to)
  (with-clean-registers
    ;; one, then two after it
    (run-keys "C-SPC M-f C-x r s a M-f C-SPC M-b C-x r + a" :text "one two")
    (is string= "onetwo" (register-value-of #\a))
    ;; the second word before the first
    (run-keys "C-SPC M-f C-x r s a M-f C-SPC M-b M-x p r e p e n d - t o - r e g i s t e r RET a" :text "one two")
    (is string= "twoone" (register-value-of #\a))
    ;; no text there: said so, nothing added
    (multiple-value-bind (f editor) (run-keys "C-SPC M-f C-x r + b" :text "one two")
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Register b is empty")))))

(define-test (registers numbers)
  (with-clean-registers
    (run-keys "C-u 5 C-x r n a")
    (is = 5 (register-value-of #\a))
    ;; one added without a count, then the count
    (run-keys "C-x r + a")
    (is = 6 (register-value-of #\a))
    (run-keys "C-u 1 0 C-x r + a")
    (is = 16 (register-value-of #\a))
    (run-keys "C-u - 3 C-x r + a")
    (is = 13 (register-value-of #\a))
    ;; asked for when there is no count; written into the text
    (multiple-value-bind (f editor) (run-keys "C-x r n b 7 RET C-x r i b" :text "n=")
      (declare (ignore f))
      (is = 7 (register-value-of #\b))
      (is string= "7n=" (session-text editor)))))

(define-test (registers rectangle-kept)
  (with-clean-registers
    (let ((tuatara.defaults::*killed-rectangle* (list "keep")))
      (multiple-value-bind (f editor)
          (run-keys "C-f M-x r e c t a n g l e - m a r k - m o d e RET C-n C-f C-x r s a M-> C-x r i a"
                    :text (format nil "abc~%def~%ghi"))
        (declare (ignore f))
        (is equal '("b" "e") (register-value-of #\a))
        (is equal '("keep") tuatara.defaults::*killed-rectangle*)
        (is string= (format nil "abc~%def~%ghib~%   e") (session-text editor))))))

(define-test (registers place-follows-the-text)
  (with-clean-registers
    ;; kept on the second line's first character; lines put above it do not lose it
    (multiple-value-bind (f editor) (run-keys "C-n C-x r p a M-< x y z RET C-x r j a"
                                              :text (format nil "one~%two~%three~%"))
      (declare (ignore f))
      (is string= (format nil "xyz~%one~%two~%three~%") (session-text editor))
      (is = 8 (session-point editor)))))

(define-test (registers place-back-in-its-file)
  (with-test-directory (dir)
    (with-clean-registers
      (let ((path (merge-pathnames "notes.txt" dir)))
        (alexandria:write-string-into-file (format nil "one~%two~%three~%") path)
        (run-keys "C-n C-f C-x r p a" :file (namestring path) :width 80)
        (forget-file-buffers)
        ;; the buffer is closed; the file changed meanwhile, lines put above the place
        (let ((place (register-value-of #\a)))
          (false (tuatara.defaults::register-place-buffer place))
          (is = 5 (tuatara.defaults::register-place-offset place)))
        (alexandria:write-string-into-file (format nil "zero~%one~%two~%three~%") path :if-exists :supersede)
        (multiple-value-bind (f editor) (run-keys "C-x r j a" :width 80)
          (declare (ignore f))
          (is string= "notes.txt" (tuatara.buffer:buffer-name (session-buffer editor)))
          (is = 10 (session-point editor)))
        ;; the file gone: said so
        (forget-file-buffers)
        (delete-file path)
        (multiple-value-bind (f editor) (run-keys "C-x r j a" :width 80)
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "is no longer there")))))))

(define-test (registers place-in-a-buffer-without-a-file)
  (with-clean-registers
    (run-keys "C-n C-x r p a" :text (format nil "one~%two"))
    ;; the scratch buffer of that session is killed by the next
    (multiple-value-bind (f editor) (run-keys "C-x r j a")
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "its buffer was closed")))))

(define-test (registers wrong-kind-and-empty-said)
  (with-clean-registers
    (run-keys "C-u 5 C-x r n a")
    (flet ((says (keys words)
             (let ((f (run-keys keys :text "x" :width 80)))
               (true (tuatara.frontend.fake:fake-frame-showing f words)))))
      (says "C-x r j a" "Register a keeps a number")
      (says "C-x r i z" "Register z is empty")
      (says "C-x r j z" "Register z is empty")
      (says "C-x r s a" "The region is empty"))
    ;; the number is as it was
    (is = 5 (register-value-of #\a))))

(define-test (registers listed)
  (with-test-directory (dir)
    (with-clean-registers
      (let ((path (merge-pathnames "notes.txt" dir)))
        (alexandria:write-string-into-file (format nil "one~%two~%three~%") path)
        (run-keys "C-n C-x r p b C-SPC C-e C-x r s c C-u 4 C-x r n d" :file (namestring path) :width 80)
        (let ((text (session-text (nth-value 1 (run-keys "C-x r l" :width 100 :height 12)))))
          (true (cl-ppcre:scan "(?m)^ *b +A place +notes.txt, octet 4" text) "~a" text)
          (true (cl-ppcre:scan "(?m)^ *c +Text +two" text) "~a" text)
          (true (cl-ppcre:scan "(?m)^ *d +A number +4" text) "~a" text))
        ;; RET on the place goes there; d x y empties the register on the line
        (forget-file-buffers)
        (let ((editor (nth-value 1 (run-keys "C-x r l RET" :width 100 :height 12))))
          (is string= "notes.txt" (tuatara.buffer:buffer-name (session-buffer editor)))
          (is = 4 (session-point editor)))
        (forget-file-buffers)
        (run-keys "C-x r l d x y" :width 100 :height 12)
        (is equal '(#\c #\d) (tuatara.defaults::register-names))
        (let ((text (session-text (nth-value 1 (run-keys "C-x r l" :width 100 :height 12)))))
          (false (cl-ppcre:scan "(?m)^ *b +" text)))
        (clrhash tuatara.defaults::*registers*)
        (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x r l" :width 100 :height 12) "No registers"))))))

(define-test (registers in-the-modal-layer)
  ;; the keys under C-x work in NORMAL state as they do elsewhere
  (clrhash tuatara.defaults::*registers*)
  (unwind-protect
       (let ((editor (nth-value 1 (run-keys "v l l l C-x r s a g l C-x r i a" :text "one two"))))
         (is string= "one twoone" (session-text editor)))
    (clrhash tuatara.defaults::*registers*)))
