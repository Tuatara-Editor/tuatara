;;;; test/user-lisp.lisp --- The API's basics for the Lisp one types, eldoc, C-h f.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test user-lisp
  :description "The Lisp one types reaches the API's functions for the basics of buffers and their text,
positions counted in octets from 0; a Lisp buffer shows the parameters of the call point is in; C-h f
describes any function.")

(defmacro with-lisp-buffer ((buffer text) &body body)
  "Run BODY with BUFFER bound to a new buffer holding TEXT, point at its start,
killed afterwards."
  `(let ((,buffer (tuatara.buffer:make-buffer (format nil "*user-lisp-~d*" (random 1000000)) :text ,text)))
     (unwind-protect (progn ,@body)
       (tuatara.buffer:kill-buffer ,buffer))))

(define-test (user-lisp forward-line-in-octets)
  ;; "héllo" is six octets, é two of them: the second line starts at 7, the text ends at 13
  (with-lisp-buffer (b (format nil "héllo~%world~%"))
    (flet ((on (byte n) (multiple-value-list (tuatara:buffer-forward-line b byte n))))
      (is equal '(7 0) (on 0 1))
      (is equal '(7 0) (on 3 1))
      (is equal '(13 0) (on 0 2))
      ;; past the last line: the end, a line short
      (is equal '(13 1) (on 0 3))
      (is equal '(0 0) (on 9 -1))
      (is equal '(0 2) (on 9 -3))
      (is equal '(7 0) (on 9 0))
      ;; narrowed to the second line: its lines only
      (tuatara:narrow-buffer b 7 13)
      (is equal '(7 1) (on 9 -1))
      (is equal '(13 0) (on 9 1))))
  ;; a last line without a line end counts as gone to when the end is reached from before it
  (with-lisp-buffer (b "abc")
    (is equal '(3 0) (multiple-value-list (tuatara:buffer-forward-line b 1 1)))
    (is equal '(3 1) (multiple-value-list (tuatara:buffer-forward-line b 3 1)))))

(define-test (user-lisp count-words-in-octets)
  (with-lisp-buffer (b (format nil "héllo wörld~%again~%"))
    (is = 3 (tuatara:buffer-count-words b 0 (tuatara:buffer-size b)))
    (is = 3 (tuatara:buffer-count-words b (tuatara:buffer-size b) 0))
    ;; from the second word on: "wörld" starts at octet 7
    (is = 2 (tuatara:buffer-count-words b 7 (tuatara:buffer-size b)))
    (is = 0 (tuatara:buffer-count-words b 6 6))
    (fail (tuatara:buffer-count-words b 0 99))
    (fail (tuatara:buffer-count-words b -1 3))))

(define-test (user-lisp narrowing-kept)
  ;; "one two three four" on four lines: narrowed to the second and third, octets 4 to 14
  (with-lisp-buffer (b (format nil "one~%two~%three~%four~%"))
    (tuatara:narrow-buffer b 4 14)
    ;; the body can widen and read outside; the narrowing is back after
    (is string= "one"
        (tuatara:with-narrowing-kept (b)
          (tuatara:widen-buffer b)
          (tuatara:buffer-string b :start 0 :end 3)))
    (is equal '(4 14) (list (tuatara:buffer-start b) (tuatara:buffer-end b)))
    ;; and after an error inside
    (fail (tuatara:with-narrowing-kept (b)
            (tuatara:widen-buffer b)
            (error "stop")))
    (is equal '(4 14) (list (tuatara:buffer-start b) (tuatara:buffer-end b)))
    ;; text put in meanwhile before the part moves it along, é two octets of it
    (tuatara:with-narrowing-kept (b)
      (tuatara:widen-buffer b)
      (tuatara:buffer-insert b 0 "é"))
    (is equal '(6 16) (list (tuatara:buffer-start b) (tuatara:buffer-end b)))
    ;; a buffer that was not narrowed is widened again
    (tuatara:widen-buffer b)
    (tuatara:with-narrowing-kept (b)
      (tuatara:narrow-buffer b 0 3))
    (false (tuatara:buffer-narrowed-p b))))

(define-test (user-lisp point-and-buffer-kept)
  (with-lisp-buffer (b "first")
    (with-lisp-buffer (other "")
      ;; the body's buffer is current, then the one before
      (tuatara:with-buffer-current (b)
        (tuatara:with-buffer-current (other)
          (is eq other (tuatara:current-buffer))
          (tuatara:buffer-insert other 0 "in other"))
        (is eq b (tuatara:current-buffer)))
      (is string= "in other" (tuatara:buffer-string other))
      (fail (tuatara:with-buffer-current ("first") nil))
      ;; point back where it was, moved along by what went in before it: two octets for é
      (setf (tuatara:buffer-point b) 3)
      (tuatara:with-point-kept (b)
        (setf (tuatara:buffer-point b) 0)
        (tuatara:buffer-insert b 0 "é"))
      (is = 5 (tuatara:buffer-point b))
      ;; and after an error
      (fail (tuatara:with-point-kept (b)
              (setf (tuatara:buffer-point b) 0)
              (error "stop")))
      (is = 5 (tuatara:buffer-point b)))))

(define-test (user-lisp narrowed-lisp-at-the-keyboard)
  (flet ((run (lisp)
           (multiple-value-bind (f editor)
               (narrowing-keys (format nil "C-n C-SPC C-n C-n C-x x n M-: ~a RET" (key-string lisp))
                         :text (format nil "one~%two~%three~%four~%five~%"))
             (declare (ignore f))
             (session-text editor))))
    ;; M-: works on the part shown, its start where the part starts
    (is string= (format nil "one~%Xtwo~%three~%four~%five~%")
        (run "(let ((b (current-buffer))) (buffer-insert b (buffer-start b) \"X\"))"))
    ;; with-narrowing-kept and widen-buffer reach outside
    (is string= (format nil "ABCone~%two~%three~%four~%five~%")
        (run "(let ((b (current-buffer))) (with-narrowing-kept (b) (widen-buffer b) (buffer-insert b 0 \"ABC\")))"))))

(define-modal-test (user-lisp at-the-keyboard)
  ;; M-: sees the API: what goes in goes in the buffer M-: was typed in
  (multiple-value-bind (f editor)
      (run-keys (format nil "M-: ~a RET" (key-string "(buffer-insert (current-buffer) 0 \"x\")")) :text "abc")
    (declare (ignore f))
    (is string= "xabc" (session-text editor)))
  ;; a Lisp buffer shows the parameters of the call point is in
  (let ((f (fresh-start (format nil "g e o ~a" (key-string "(buffer-insert ")) :width 160)))
    (true (showing f "(buffer-insert buffer byte text &key group)")))
  ;; C-h f describes any function: what it is and what it does
  (let ((f (fresh-start (format nil "C-h f ~a RET" (key-string "buffer-count-words")) :width 160)))
    (true (tuatara.frontend.fake:fake-frame-showing f "buffer-count-words is a function in TUATARA.DEFAULTS"))
    (true (tuatara.frontend.fake:fake-frame-showing f "How many words BUFFER's text has")))
  ;; and commands still, with their keys
  (let ((f (fresh-start (format nil "C-h f ~a RET" (key-string "goto-line")) :width 160)))
    (true (tuatara.frontend.fake:fake-frame-showing f "goto-line is a command."))
    (true (tuatara.frontend.fake:fake-frame-showing f "Keys:"))))

(define-test (user-lisp call-shown-without-copying-the-form)
  ;; the operator of the call point is in is found in the form's octets read into the
  ;; same place each time, nothing made for each octet before point
  (flet ((operator (text &optional (at (length text)))
           (let ((b (tuatara.buffer:make-buffer "*operator*" :text text)))
             (unwind-protect (tuatara.defaults::operator-at b at)
               (tuatara.buffer:kill-buffer b)))))
    (is string= "mapcar" (operator "(defun f ()
  (mapcar #'car "))
    (is string= "list" (operator "(a \"(b\" ; (c
 (list ;|
"))
    (is eq nil (operator "'(a b "))
    (is eq nil (operator "(f) x"))
    (is string= "f" (operator "(f (g) |a (b| "))
    (flet ((consed (count)
             (let ((b (tuatara.buffer:make-buffer "*operator*"
                                                  :text (format nil
                                                                "(defparameter *data*~%  (vector ~{~d~^ ~}"
                                                                (loop for i below count collect i)))))
               (unwind-protect
                    (progn (tuatara.defaults::operator-at b (tuatara.buffer:buffer-size b))
                           (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring
                                                                  (tuatara.defaults::operator-at b (tuatara.buffer:buffer-size b))))))
                 (tuatara.buffer:kill-buffer b)))))
      (let ((per-octet (/ (- (consed 9000) (consed 1500)) (* 7500 4.9))))
        (true (< per-octet 0.05) "~,3f octets consed for each octet before point" per-octet)))))

(define-test (user-lisp name-characters-alike)
  ;; a name's characters are the same for completion, indentation and reading expressions: all
  ;; but a bar, which the name at point leaves out
  (dotimes (code 256)
    (let ((char (code-char code)))
      (is eq (and (tuatara.defaults::lisp-symbol-octet-p code) (/= code 124))
          (tuatara.defaults::lisp-name-part-p char) "~s" char)))
  (true (tuatara.defaults::lisp-name-part-p (code-char #x101)))
  ;; reading expressions cuts an atom where the octets say
  (is equal
      '((0 3 0) (4 7 0))
      (tuatara.defaults::lisp-elements (tuatara.text:string-octets (format nil "abc~cdef" #\Page)))))
