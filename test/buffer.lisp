;;;; test/buffer.lisp --- Buffers: editing, point and mark, undo and redo, files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test buffer
  :description "Buffers: editing through versions, point and mark following edits, undo and redo, saving and reopening files.")

(defmacro with-test-buffer ((var &rest args) &body body)
  "Run BODY with VAR bound to a fresh buffer made with ARGS, killing it afterwards."
  `(let ((,var (tuatara.buffer:make-buffer ,@args)))
     (unwind-protect (progn ,@body)
       (tuatara.buffer:kill-buffer ,var))))

(define-test (buffer edit-point-mark)
  (with-test-buffer (b "*t*" :text "hello world")
    (is = 0 (tuatara.buffer:buffer-point b))
    (setf (tuatara.buffer:buffer-point b) 5)
    (setf (tuatara.buffer:buffer-mark b) 11)
    (is = 10 (tuatara.buffer:buffer-insert b 5 ", big"))    ; returns the offset after the insertion
    (is string= "hello, big world" (tuatara.buffer:buffer-string b))
    ;; point (bias :after) moved past the insertion; mark followed the text
    (is = 10 (tuatara.buffer:buffer-point b))
    (is = 16 (tuatara.buffer:buffer-mark b))
    (multiple-value-bind (s e) (tuatara.buffer:buffer-region b) (is = 10 s) (is = 16 e))
    (tuatara.buffer:buffer-delete b 0 7)
    (is string= "big world" (tuatara.buffer:buffer-string b))
    (is = 3 (tuatara.buffer:buffer-point b))
    (is = 9 (tuatara.buffer:buffer-mark b))
    (is = 5 (tuatara.buffer:buffer-replace b 0 3 "small"))
    (is string= "small world" (tuatara.buffer:buffer-string b))
    (true (tuatara.buffer:buffer-modified-p b))
    (fail (setf (tuatara.buffer:buffer-point b) 99))
    (with-test-buffer (m "*multi*" :text "aé😀")
      (fail (setf (tuatara.buffer:buffer-point m) 2))       ; inside é
      (is = 3 (tuatara.buffer:buffer-forward-char m 1))
      (is = 3 (tuatara.buffer:buffer-forward-char m 0 2))
      (is = 7 (tuatara.buffer:buffer-forward-char m 0 9))
      (is = 1 (tuatara.buffer:buffer-forward-char m 3 -1))
      (is char= #\😀 (tuatara.buffer:buffer-char-at m 3))
      (false (tuatara.buffer:buffer-char-at m 7)))))

(define-test (buffer splice-spans-checked)
  ;; no spans: nothing done; an edge inside a character, or spans out of order: refused
  (with-test-buffer (b "*spans*" :text "aé b c d")
    (false (tuatara.buffer:buffer-splice-spans b '() nil))
    (is string= "aé b c d" (tuatara.buffer:buffer-string b))
    (fail (tuatara.buffer:buffer-splice-spans b (list (cons 0 2)) nil))
    (fail (tuatara.buffer:buffer-splice-spans b (list (cons 6 7) (cons 0 1)) nil))
    (is string= "aé b c d" (tuatara.buffer:buffer-string b))
    ;; positions in, between and after the spans follow the text, undo brings it back
    (setf (tuatara.buffer:buffer-point b) 8 (tuatara.buffer:buffer-mark b) 5)
    (is = 2 (tuatara.buffer:buffer-splice-spans b (list (cons 0 1) (cons 4 6) (cons 7 8)) (tuatara.text:string->rope "XY")))
    (is string= "XYé cd" (tuatara.buffer:buffer-string b))
    (is = 6 (tuatara.buffer:buffer-point b))
    (is = 5 (tuatara.buffer:buffer-mark b))
    (tuatara.buffer:buffer-undo b)
    (is string= "aé b c d" (tuatara.buffer:buffer-string b))))

(define-test (buffer undo-redo)
  (with-test-buffer (b "*u*" :text "abc")
    (tuatara.buffer:buffer-insert b 3 "def")
    (tuatara.buffer:buffer-delete b 0 2)
    (is string= "cdef" (tuatara.buffer:buffer-string b))
    (true (tuatara.buffer:buffer-undo b))
    (is string= "abcdef" (tuatara.buffer:buffer-string b))
    (is = 0 (tuatara.buffer:buffer-point b))
    (true (tuatara.buffer:buffer-undo b))
    (is string= "abc" (tuatara.buffer:buffer-string b))
    (false (tuatara.buffer:buffer-undo b))
    (true (tuatara.buffer:buffer-redo b))
    (is string= "abcdef" (tuatara.buffer:buffer-string b))
    (is = 6 (tuatara.buffer:buffer-point b))
    (true (tuatara.buffer:buffer-redo b))
    (is string= "cdef" (tuatara.buffer:buffer-string b))
    (false (tuatara.buffer:buffer-redo b))
    ;; a new edit after undo discards the redo stack
    (tuatara.buffer:buffer-undo b)
    (tuatara.buffer:buffer-insert b 0 "X")
    (false (tuatara.buffer:buffer-redo b))
    (is string= "Xabcdef" (tuatara.buffer:buffer-string b))))

(define-test (buffer lines-and-columns)
  (with-test-buffer (b "*l*" :text (format nil "one~%twoé~%three"))
    (is = 3 (tuatara.buffer:buffer-line-count b))
    (is = 4 (tuatara.buffer:buffer-goto-line b 1))
    (is = 10 (tuatara.buffer:buffer-goto-line b 2))
    (is = 10 (tuatara.buffer:buffer-goto-line b 99))
    (is = 1 (tuatara.buffer:buffer-line-number b 6))
    (is = 4 (tuatara.buffer:buffer-line-start b 7))
    (is = 9 (tuatara.buffer:buffer-line-end b 4))
    (is = 15 (tuatara.buffer:buffer-line-end b 12))
    (is = 4 (tuatara.buffer:buffer-column b 9))             ; t w o é -> byte 9 is after é, column 4
    (is = 7 (column-by-steps b 4 3))
    (is = 9 (column-by-steps b 4 50))))

(define-test (buffer file-round-trip)
  (let* ((s (random-text 30000)) (octets (to-octets s)))
    (with-temp-file (path octets)
      (let ((b (tuatara.io:visit-file path)))
        (unwind-protect
             (progn
               (true (eq b (tuatara.io:visit-file path)))
               (is string= s (tuatara.buffer:buffer-string b))
               (false (tuatara.buffer:buffer-modified-p b))
               (tuatara.buffer:buffer-insert b 0 "PREFIX ")
               (true (tuatara.buffer:buffer-modified-p b))
               (tuatara.io:save-buffer-file b)
               (false (tuatara.buffer:buffer-modified-p b))
               ;; the file on disk has the new content; the old mapping is still valid for the buffer
               (is string= (concatenate 'string "PREFIX " s)
                   (sb-ext:octets-to-string (alexandria:read-file-into-byte-vector path) :external-format :utf-8))
               (is string= (concatenate 'string "PREFIX " s) (tuatara.buffer:buffer-string b)))
          (tuatara.buffer:kill-buffer b))
        ;; reopen: fresh buffer with the saved text
        (let ((b2 (tuatara.io:visit-file path)))
          (unwind-protect (is string= (concatenate 'string "PREFIX " s) (tuatara.buffer:buffer-string b2))
            (tuatara.buffer:kill-buffer b2)))))))

(define-test (buffer new-file)
  (let ((path (merge-pathnames (format nil "tuatara-new-~a.txt" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (let ((b (tuatara.io:visit-file path)))
           (unwind-protect
                (progn (is = 0 (tuatara.buffer:buffer-size b))
                       (tuatara.buffer:buffer-insert b 0 "created")
                       (tuatara.io:save-buffer-file b)
                       (true (probe-file path))
                       (is string= "created" (sb-ext:octets-to-string (alexandria:read-file-into-byte-vector path))))
             (tuatara.buffer:kill-buffer b)))
      (ignore-errors (delete-file path)))))

(define-test (buffer back-to-saved)
  ;; undoing back to the text last saved leaves the buffer unmodified, and redo modifies it again
  (let ((b (tuatara.buffer:make-buffer "*saved-test*" :text "abc")))
    (unwind-protect
         (progn
           (false (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-insert b 0 "x")
           (true (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-undo b)
           (false (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-redo b)
           (true (tuatara.buffer:buffer-modified-p b))
           ;; saved as "xabc": undo modifies, redo brings the saved text back
           (tuatara.buffer:buffer-mark-saved b)
           (false (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-undo b)
           (true (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-redo b)
           (false (tuatara.buffer:buffer-modified-p b))
           ;; typing grouped into one undo step
           (tuatara.buffer:buffer-insert b 4 "1")
           (tuatara.buffer:buffer-insert b 5 "2" :group t)
           (true (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-undo b)
           (false (tuatara.buffer:buffer-modified-p b))
           ;; a whole-text swap
           (tuatara.buffer:buffer-swap-text b (tuatara.text:string->rope "zz")
                                            (lambda (p bias) (declare (ignore bias)) (min p 2))
                                            (lambda (p bias) (declare (ignore bias)) (min p 4)))
           (true (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-undo b)
           (false (tuatara.buffer:buffer-modified-p b))
           (is string= "xabc" (tuatara.buffer:buffer-string b)))
      (tuatara.buffer:kill-buffer b))))
