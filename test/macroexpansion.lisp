;;;; test/macroexpansion.lisp --- What a macro call expands to: C-c C-m, C-c M-m.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test macroexpansion
  :description "C-c C-m shows what the macro call at point expands to once, C-c M-m all the way down,
in *Pp Macroexpand Output*; with C-u the expansion goes in after the call.")

(defun expansion-shown ()
  "The text of *Pp Macroexpand Output*, or NIL when there is none."
  (let ((b (tuatara.buffer:find-buffer "*Pp Macroexpand Output*")))
    (and b (tuatara.buffer:buffer-string b))))

(defun squeezed (text)
  "TEXT with each run of blanks and newlines made one space."
  (cl-ppcre:regex-replace-all "\\s+" (string-trim '(#\Space #\Newline) text) " "))

(define-test (macroexpansion which-expression)
  (let ((b (tuatara.buffer:make-buffer "*macroexpand-test*" :text "(when a b) (unless c d)")))
    (unwind-protect
         (flet ((at (position) (multiple-value-list (tuatara.defaults::expression-at-or-before b position))))
           ;; on an opening bracket: the expression beginning there
           (is equal '(0 10) (at 0))
           (is equal '(11 23) (at 11))
           ;; elsewhere: the one before point
           (is equal '(11 23) (at 23))
           (is equal '(0 10) (at 10)))
      (tuatara.buffer:kill-buffer b))))

(define-test (macroexpansion from-the-keys)
  (flet ((expanded (keys)
           (let ((old (tuatara.buffer:find-buffer "*Pp Macroexpand Output*")))
             (when old (tuatara.buffer:kill-buffer old)))
           (nth-value 1 (fresh-start keys :width 120 :height 20))))
    ;; once, the call before point
    (expanded (format nil "o ~a C-g C-c C-m" (key-string "(when a b)")))
    (is string= "(if a b)" (squeezed (expansion-shown)))
    ;; from a terminal, where C-m is RET
    (expanded (format nil "o ~a C-g C-c RET" (key-string "(when a b)")))
    (is string= "(if a b)" (squeezed (expansion-shown)))
    ;; all the way down
    (expanded (format nil "o ~a C-g C-c M-m" (key-string "(when a (unless b c))")))
    (is string= "(if a (if b nil c))" (squeezed (expansion-shown)))
    ;; the cursor on the opening bracket: the call beginning there
    (expanded (format nil "o ~a C-g C-a C-c C-m" (key-string "(when a b)")))
    (is string= "(if a b)" (squeezed (expansion-shown)))
    ;; with C-u, put in after the call
    (let ((editor (expanded (format nil "o ~a C-g C-u C-c C-m" (key-string "(when a b)")))))
      (true (search "(when a b) (if a b)" (squeezed (session-text editor)))))
    ;; not a macro call: shown as it is
    (expanded (format nil "o ~a C-g C-c C-m" (key-string "(+ 1 2)")))
    (is string= "(+ 1 2)" (squeezed (expansion-shown)))
    ;; a macro that fails says so
    (true (showing (fresh-start (format nil "o ~a C-g C-c C-m" (key-string "(defstruct)")) :width 120) "Error:"))
    ;; nothing in the text runs: #. is not read, a macrolet's macros are not expanded
    (let ((marker (format nil "~a/tuatara-macroexpand-~d" (uiop:getenv "XDG_CACHE_HOME") (random 1000000))))
      (true (showing (fresh-start (format nil "o ~a C-g C-c C-m"
                                          (key-string (format nil "(when x #.(with-open-file (s ~s :direction :output) 1))" marker)))
                                  :width 160)
                     "Not expanded: #. in it would run code as it is read"))
      (true (showing (fresh-start (format nil "o ~a C-g C-c M-m"
                                          (key-string (format nil "(macrolet ((m () (with-open-file (s ~s :direction :output) 1) nil)) (m))" marker)))
                                  :width 160)
                     "Not expanded: it holds a macrolet"))
      (false (probe-file marker)))))

(define-test (macroexpansion circular)
  ;; an expansion that holds itself is written with labels, the printing ends
  (unwind-protect
       (progn
         (fresh-start (format nil "o ~a C-M-x o ~a C-g C-c C-m"
                              (key-string "(defmacro macroexpand-test-circular () (let ((l (list 1 2))) (setf (cddr l) l) (list 'quote l)))")
                              (key-string "(macroexpand-test-circular)"))
                      :width 120 :height 20)
         (true (search "#1=" (expansion-shown))))
    (let ((symbol (find-symbol "MACROEXPAND-TEST-CIRCULAR" :tuatara-user)))
      (when symbol (fmakunbound symbol) (unintern symbol :tuatara-user)))))

(define-test (macroexpansion only-the-call-is-read)
  ;; the call is read once, #. refused, and anything after it is refused rather than evaluated
  (let ((marker (format nil "~a/tuatara-macroexpand-~d" (uiop:getenv "XDG_CACHE_HOME") (random 1000000)))
        (package (find-package :tuatara-user)))
    (flet ((refusal (text) (nth-value 1 (tuatara.defaults::read-expansion-call text package))))
      (is equal (list 'when (intern "A" package) (intern "B" package))
          (tuatara.defaults::read-expansion-call "(when a b)" package))
      (false (refusal (format nil "  (when a b)  ~%")))
      ;; a bracket too many: the text after it would be a second form
      (is string= "the text holds more than the one expression"
          (refusal (format nil "(when a b)) (with-open-file (s ~s :direction :output) 1)" marker)))
      (is string= "the text holds more than the one expression"
          (refusal (format nil "(when a b) (with-open-file (s ~s :direction :output) 1)" marker)))
      (is string= "#. in it would run code as it is read"
          (refusal (format nil "(when a #.(with-open-file (s ~s :direction :output) 1))" marker)))
      (true (search "cannot be read" (refusal "(when a |b)"))))
    ;; from the keys, the cursor on a call with a form after it: that call alone expanded, the form not run
    (fresh-start (format nil "o ~a C-g C-a C-c C-m"
                         (key-string (format nil "(when a b) (with-open-file (s ~s :direction :output) 1)" marker)))
                 :width 200)
    (is string= "(if a b)" (squeezed (expansion-shown)))
    (false (probe-file marker))))

(define-test (macroexpansion form-at-a-bracket)
  ;; the form opening at a bracket, for C-M-x's top-level form and the call C-c C-m expands alike
  (let* ((text (format nil "(defun f ()~%  (list 1 2))~%(g)~%(open"))
         (b (tuatara.buffer:make-buffer "*bracket-form*" :text text))
         (rope (tuatara.buffer:buffer-text b))
         (first-end (1+ (search "2))" text :from-end nil))))
    (unwind-protect
         (progn
           (is equal (list 0 (+ 2 first-end)) (multiple-value-list (tuatara.defaults::bracket-form-bounds rope 0)))
           (is equal (list 0 (+ 2 first-end)) (multiple-value-list (tuatara.defaults::top-level-form-bounds b 5)))
           (is equal (list 0 (+ 2 first-end)) (multiple-value-list (tuatara.defaults::expression-at-or-before b 0)))
           (let ((list (search "(list" text)))
             (is equal (list list (+ list 10)) (multiple-value-list (tuatara.defaults::expression-at-or-before b list))))
           ;; no bracket there, or one not closed
           (is eq nil (tuatara.defaults::bracket-form-bounds rope 1))
           (is eq nil (tuatara.defaults::bracket-form-bounds rope (search "(open" text))))
      (tuatara.buffer:kill-buffer b))))

(define-test (macroexpansion refused-where-opened-safely)
  ;; expanding runs the editor's macros on the text, so a file opened safely is never expanded
  (with-test-directory (dir)
    (let ((file (namestring (put-file (merge-pathnames "hostile.lisp" dir) (format nil "(when a b)~%")))))
      (dolist (keys '("C-c C-m" "C-c M-m"))
        (let ((old (tuatara.buffer:find-buffer "*Pp Macroexpand Output*")))
          (when old (tuatara.buffer:kill-buffer old)))
        (let ((f (run-keys (list (format nil "C-u C-x C-f C-a C-k ~a RET M-x l i s p - m o d e RET" (key-string file))
                                 (format nil "M-< ~a" keys))
                           :width 160 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "is opened safely: nothing was evaluated"))
          (false (expansion-shown)))
        (kill-buffers-visiting dir)))))
