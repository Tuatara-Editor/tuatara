;;;; test/tabs.lisp --- Tab stops, what TAB puts in, and the settings for a mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test tabs
  :description "TAB goes on to the next tab stop with spaces, or a tab where tabs are wanted; a tab shows
to the next stop; the width and the choice are set for all and for a major mode; TAB does nothing in
NORMAL state but in Org.")

(defmacro with-mode-tabs ((&rest settings) &body body)
  "Run BODY with the tab SETTINGS, each (MODE :WIDTH N) or (MODE :TABS BOOLEAN), made
for major modes, and the settings made for modes before it put back afterwards."
  (let ((widths (gensym)) (tabs (gensym)))
    `(let ((,widths (alexandria:copy-hash-table tuatara.defaults::*mode-tab-widths*))
           (,tabs (alexandria:copy-hash-table tuatara.defaults::*mode-indent-tabs*)))
       (unwind-protect
            (progn
              ,@(loop for (mode key value) in settings
                      collect (ecase key
                                (:width `(tuatara-user:set-tab-width ',mode ,value))
                                (:tabs `(tuatara-user:set-indent-tabs-mode ',mode ,value))))
              ,@body)
         (setf tuatara.defaults::*mode-tab-widths* ,widths
               tuatara.defaults::*mode-indent-tabs* ,tabs)))))

(define-test (tabs tab-key)
  ;; spaces to the next stop, 4 columns apart
  (is string= "    abc" (session-text (nth-value 1 (run-keys "i TAB" :text "abc"))))
  (is string= "ab  c" (session-text (nth-value 1 (run-keys "l l i TAB" :text "abc"))))
  (is string= "        abc" (session-text (nth-value 1 (run-keys "i C-u 2 TAB" :text "abc"))))
  ;; a tab before point counts to its stop: x at column 4, point at 5, the stop at 8
  (is string= (format nil "~cx   y" #\Tab) (session-text (nth-value 1 (run-keys "l l i TAB" :text (format nil "~cxy" #\Tab)))))
  ;; tabs where they are wanted
  (let ((tuatara-user:*indent-tabs-mode* t))
    (is string= (format nil "~cabc" #\Tab) (session-text (nth-value 1 (run-keys "i TAB" :text "abc")))))
  (let ((tuatara-user:*tab-width* 8))
    (is string= "        abc" (session-text (nth-value 1 (run-keys "i TAB" :text "abc")))))
  ;; C-q TAB puts a tab character in whatever the settings say, C-q C-l a form feed
  (is string= (format nil "~cabc" #\Tab) (session-text (nth-value 1 (run-keys "i C-q TAB" :text "abc"))))
  (is string= (format nil "~cabc" (code-char 12)) (session-text (nth-value 1 (run-keys "i C-q C-l" :text "abc"))))
  ;; M-x indent-tabs-mode switches one buffer
  (multiple-value-bind (f editor) (run-keys "M-x i n d e n t - t a b s - m o d e RET i TAB" :text "abc" :width 80)
    (is string= (format nil "~cabc" #\Tab) (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Indent-Tabs mode enabled in current buffer"))))

(define-test (tabs normal-state)
  ;; TAB changes no text in NORMAL state
  (is string= "abc" (session-text (nth-value 1 (run-keys "TAB" :text "abc"))))
  (is string= "abc" (session-text (nth-value 1 (run-keys "M-x l i s p - m o d e RET TAB" :text "abc"))))
  ;; but in Org it does what TAB does there
  (is string= "    abc" (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET TAB" :text "abc"))))
  (let ((tuatara-user:*tuatara-normal-tab-modes* '()))
    (is string= "abc" (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET TAB" :text "abc"))))))

(define-test (tabs for-a-mode)
  ;; a mode's own width, and the modes derived from it
  (with-mode-tabs ((text :width 2))
    (is string= "  abc" (session-text (nth-value 1 (run-keys "M-x t e x t - m o d e RET i TAB" :text "abc"))))
    (is string= "  abc" (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET i TAB" :text "abc"))))
    ;; other modes keep *tab-width*
    (is string= "    abc" (session-text (nth-value 1 (run-keys "M-x l i s p - m o d e RET i M-i" :text "abc")))))
  (with-mode-tabs ((lisp-mode :tabs t))
    (is string= (format nil "~cabc" #\Tab) (session-text (nth-value 1 (run-keys "M-x l i s p - m o d e RET i M-i" :text "abc"))))
    (is string= "    abc" (session-text (nth-value 1 (run-keys "i TAB" :text "abc")))))
  ;; the nearest setting counts
  (with-mode-tabs ((text :width 2) (org :width 6))
    (is string= "      abc" (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET i TAB" :text "abc")))))
  ;; tab-separated data has tabs
  (is string= (format nil "~cabc" #\Tab) (session-text (nth-value 1 (run-keys "M-x t s v - m o d e RET i TAB" :text "abc"))))
  ;; every language has spaces unless the init file asks for tabs: the
  ;; starter init file asks for them where a language's own tools require
  ;; them (a Makefile's recipes, Go's gofmt), a line each
  (is string= "    abc" (session-text (nth-value 1 (run-keys "M-x m a k e f i l e - m o d e RET i M-i" :text "abc"))))
  (is string= "    abc" (session-text (nth-value 1 (run-keys "M-x g o - m o d e RET i M-i" :text "abc"))))
  (with-mode-tabs ((makefile :tabs t))
    (is string= (format nil "~cabc" #\Tab) (session-text (nth-value 1 (run-keys "M-x m a k e f i l e - m o d e RET i M-i" :text "abc")))))
  ;; names are checked
  (fail (tuatara-user:set-tab-width 'no-such 4) error)
  (fail (tuatara-user:set-tab-width 'org 0) error)
  (fail (tuatara-user:set-tab-width 'org 65) error))

(define-test (tabs mode-width-limited-as-the-setting)
  ;; a mode's width is taken from 1 to 64, as *tab-width* is
  (let ((b (tuatara.buffer:make-buffer "*tab widths*" :text "")))
    (unwind-protect
         (dolist (width '(1 17 64))
           (with-mode-tabs ((fundamental :width width))
             (is = width (tuatara.defaults::buffer-tab-width b))))
      (tuatara.buffer:kill-buffer b)))
  (dolist (width '(0 65 2.5))
    (fail (tuatara-user:set-tab-width 'org width) error)))

(define-test (tabs tab-characters-shown)
  ;; a tab shows to the next stop, 4 columns apart unless set
  (let ((f (run-keys "" :text (format nil "~cx~%ab~cy" #\Tab #\Tab))))
    (is string= "    x" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))
    (is string= "ab  y" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 1))))
  (let ((f (let ((tuatara-user:*tab-width* 8)) (run-keys "" :text (format nil "~cx" #\Tab)))))
    (is string= "        x" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0))))
  (with-mode-tabs ((text :width 3))
    (let ((f (run-keys "M-x t e x t - m o d e RET" :text (format nil "~cx" #\Tab))))
      (is string= "   x" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0))))))

(define-test (tabs indentation)
  ;; a new line is indented with spaces, or with tabs and spaces where tabs are wanted
  (flet ((typed (keys) (session-text (nth-value 1 (run-keys keys :text "")))))
    (let ((keys (format nil "M-x l i s p - m o d e RET i ~a RET" (key-string "(let ((a 1)"))))
      (is string= (format nil "(let ((a 1)~%      ") (typed keys))
      (let ((tuatara-user:*indent-tabs-mode* t))
        (is string= (format nil "(let ((a 1)~%~c  " #\Tab) (typed keys)))
      ;; less than a tab's columns is spaces
      (let ((tuatara-user:*indent-tabs-mode* t) (tuatara-user:*tab-width* 8))
        (is string= (format nil "(let ((a 1)~%      ") (typed keys))))))

(define-test (tabs lisp-indent-reads-a-bounded-way-back)
  ;; a form starting far back is read from the first line within reach, not from its start
  (let* ((tuatara.defaults::*lisp-indent-reach* 1000)
         (text (format nil "(defparameter *data*~%  '(~{~a~^~%    ~}))~%(defun f ()~%x)"
                       (loop repeat 400 collect "(1 2 3)")))
         (rope (tuatara.text:string->rope text))
         (last (1+ (position #\Newline text :from-end t)))
         (data-line (1+ (position #\Newline text :start (floor (length text) 2)))))
    ;; the nearest form start within reach is taken
    (is = (search "(defun" text) (tuatara.defaults::top-level-start rope last nil))
    ;; deep in the data, only the reach is read
    (let ((start (tuatara.defaults::top-level-start rope data-line nil)))
      (true (<= (- data-line start) 1000))
      (true (plusp start)))
    ;; the line is indented all the same
    (is equal (list (cons last 2)) (tuatara.defaults::lisp-indent-lines rope last (length text)))))

(define-test (tabs tab-indents-where-the-mode-indents)
  (flet ((typed (keys text) (session-text (nth-value 1 (run-keys keys :text text :width 120)))))
    ;; in Lisp TAB indents the line, point from the blanks to the text
    (multiple-value-bind (f editor) (run-keys "M-x l i s p - m o d e RET j i TAB" :text (format nil "(defun f ()~%foo)") :width 120)
      (declare (ignore f))
      (is string= (format nil "(defun f ()~%  foo)") (session-text editor))
      (is = (+ 12 2) (session-point editor)))
    ;; in Rust too
    (is string= (format nil "fn f() {~%    x~%}") (typed "M-x r u s t - m o d e RET j i TAB" (format nil "fn f() {~%x~%}")))
    ;; indented already: TAB completes the name at point (*tab-always-indent* :complete, as standard)
    (true (search "(buffer-substring" (typed "M-x l i s p - m o d e RET j A TAB" (format nil "(progn~%  (buffer-subs"))))
    ;; but not where no name is being typed
    (is string= (format nil "(progn~%  (foo ") (typed "M-x l i s p - m o d e RET j A TAB" (format nil "(progn~%  (foo ")))
    ;; T only indents
    (let ((tuatara-user:*tab-always-indent* t))
      (is string= (format nil "(progn~%  (buffer-subs") (typed "M-x l i s p - m o d e RET j A TAB" (format nil "(progn~%  (buffer-subs"))))
    ;; NIL: in the line's text, the next tab stop
    (let ((tuatara-user:*tab-always-indent* nil))
      ;; point at column 5, the stop at 8
      (is string= (format nil "(progn~%  (x)   ") (typed "M-x l i s p - m o d e RET j A TAB" (format nil "(progn~%  (x)"))))
    ;; with the selection active, the lines it touches
    (is string= (format nil "(progn~%  foo~%  bar)") (typed "M-x l i s p - m o d e RET i C-x h TAB" (format nil "(progn~%foo~%bar)")))
    ;; where the mode does not indent, the tab stop; M-i there anywhere
    (is string= "    abc" (typed "M-x t e x t - m o d e RET i TAB" "abc"))
    (is string= "(a) b" (typed "M-x l i s p - m o d e RET A M-i b" "(a)"))))


(define-test (tabs widths-kept-sensible)
  (let ((b (tuatara.buffer:make-buffer "*tab widths*" :text "")))
    (unwind-protect
         (progn
           (dolist (case '((3 3) (64 64) (1 1) (0 4) (65 4) (2.5 4) (nil 4) ("8" 4)))
             (destructuring-bind (width wanted) case
               (let ((tuatara-user:*tab-width* width) (tuatara.defaults::*tab-columns* nil))
                 (is = wanted (tuatara.defaults::buffer-tab-width b) "buffer ~s" width)
                 (is = wanted (tuatara.defaults::tab-columns) "columns ~s" width))))
           ;; the screen's 8 when nothing sensible is said
           (let ((tuatara.display:*tab-width-function* (constantly 0)))
             (is = 8 (tuatara.display:tab-width-of b)))
           (let ((tuatara.display:*tab-width-function* (constantly 5)))
             (is = 5 (tuatara.display:tab-width-of b)))
           (let ((tuatara.display:*tab-width-function* nil))
             (is = 8 (tuatara.display:tab-width-of b))))
      (tuatara.buffer:kill-buffer b))))

(define-test (tabs indenting-a-line-reads-only-its-head)
  ;; indenting a line copies no more of it than its blanks and the words it begins with
  (flet ((per-octet (function)
           (flet ((consed (length)
                    (let ((rope (tuatara.text:string->rope (format nil "(foo~%  ~a" (make-string length :initial-element #\x)))))
                      (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring (funcall function rope)))))))
             (consed 1000)
             (/ (- (consed 1000000) (consed 100000)) 900000.0))))
    (let ((lisp (per-octet (lambda (rope) (tuatara.defaults::lisp-indent-lines rope 5 5))))
          (columns (per-octet (lambda (rope) (tuatara.defaults::line-indent-column rope 1)))))
      (true (< lisp 0.5) "Lisp: ~,2f octets consed for each octet of the line" lisp)
      (true (< columns 0.5) "a line's column: ~,2f octets consed for each octet of the line" columns))))

(define-test (tabs lisp-indent-methods-named-once)
  ;; each form's way of indenting is given once: a second entry for a name never applies
  (let* ((source (uiop:read-file-string (asdf:system-relative-pathname :tuatara "lisp/modes/lisp-indent.lisp")))
         (start (search "(defparameter *lisp-indent-methods*" source))
         (end (search "(defparameter" source :start2 (1+ start)))
         (names (cl-ppcre:all-matches-as-strings "(?<=\\(\")[^\"]+(?=\" )" (subseq source start end))))
    (true (> (length names) 50))
    (is equal '() (loop for (name . more) on names when (member name more :test #'string=) collect name))))
