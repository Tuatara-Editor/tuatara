;;;; test/show-paren.lisp --- The bracket that pairs with the one at point, shown.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test show-paren
  :description "The bracket before point that closes, or else after it that opens, and its partner are
shown in :paren-match; one of another kind or none in :paren-mismatch; brackets in strings and
comments count for nothing; M-x show-paren-mode switches it.")

(defun faces-at (frontend row column)
  "The faces the last frame FRONTEND painted over COLUMN of ROW, a list."
  (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans frontend) row)
        when (and (<= start column) (< column end))
          append (if (listp face) face (list face))))

(defun row-showing (frontend text)
  "The row of the last frame FRONTEND painted that holds TEXT, or NIL."
  (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen frontend))
        when (search text (tuatara.frontend.fake:fake-screen-line frontend row)) return row))

(define-test (show-paren partners)
  (flet ((partner (text at &optional spans)
           (multiple-value-list (tuatara.defaults::counted-partner (tuatara.text:string->rope text) (coerce spans 'simple-vector)
                                                                   at (find (char text at) "([{") 0 (length text)))))
    (is equal '(8 t) (partner "(a (b) c)" 0))
    (is equal '(5 t) (partner "(a (b) c)" 3))
    (is equal '(0 t) (partner "(a (b) c)" 8))
    (is equal '(1 nil) (partner "(]" 0))
    (is equal '(0 nil) (partner "(]" 1))
    (is equal '(nil nil) (partner "(a" 0))
    (is equal '(nil nil) (partner "a)" 1))
    ;; brackets in a string count for nothing
    (is equal '(8 t) (partner "(a \")\" b)" 0 (list (cons 3 6))))
    (is equal '(0 t) (partner "(a \"(\" b)" 8 (list (cons 3 6))))
    ;; nor do brackets escaped by a backslash
    (is equal '(7 t) (partner "(a \\) b)" 0))
    (is equal '(5 t) (partner "(a \\\\) b)" 0))))

(define-test (show-paren one-partner-for-showing-and-typing)
  ;; the bracket shown at point and the one a typed bracket is said to close are found alike
  (let ((buffer (tuatara.buffer:make-buffer "partners" :text "(a (b] c)")))
    (unwind-protect
         (progn
           (is equal '(8 t) (multiple-value-list (tuatara.defaults::bracket-partner buffer 0 t)))
           (is equal '(3 nil) (multiple-value-list (tuatara.defaults::bracket-partner buffer 5 nil)))
           (is equal '((5 6 :paren-mismatch) (3 4 :paren-mismatch)) (tuatara.defaults::paren-ranges buffer 6)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (show-paren by-the-tree)
  (flet ((ranges (mode text point)
           (let ((b (tuatara.buffer:make-buffer (format nil "*paren-~d*" (random 1000000)) :text text)))
             (unwind-protect
                  (progn (tuatara.modes:set-major-mode b mode)
                         (let ((tuatara.syntax:*syntax-wait* 5)) (tuatara.defaults::ready-syntax b))
                         (tuatara.defaults::paren-ranges b point))
               (tuatara.buffer:kill-buffer b)))))
    ;; a partner further than the counting reaches, found by the tree
    (let ((text (format nil "(progn~%~{~a~}  1)" (loop repeat 3000 collect (format nil "  (list \"a (b\" 'c) ; ) d~%")))))
      (is equal (list (list (1- (length text)) (length text) :paren-match) (list 0 1 :paren-match))
          (ranges 'tuatara.defaults::lisp-mode text (length text))))
    ;; a character literal's bracket is text
    (false (ranges 'tuatara.defaults::lisp-mode "(list #\\()" 9))
    ;; not closed: counted to the text's end, where it has none
    (is equal '((0 1 :paren-mismatch)) (ranges 'tuatara.defaults::lisp-mode "(a (b c)" 0))
    ;; a shell case pattern's parenthesis pairs with nothing
    (false (ranges 'tuatara.defaults::bash-mode (format nil "case x in~%  a) echo;;~%esac~%") 13))
    ;; C's brackets of every kind
    (is equal '((8 9 :paren-match) (23 24 :paren-match)) (ranges 'tuatara.defaults::c-mode "int f() { return a[0]; }" 8))
    (is equal '((20 21 :paren-match) (18 19 :paren-match)) (ranges 'tuatara.defaults::c-mode "int f() { return a[0]; }" 21))))

(define-test (show-paren highlighted)
  (flet ((typed (text) (fresh-start (format nil "o ~a" (key-string text)) :width 80)))
    ;; a closing bracket before point: it and its partner
    (let* ((f (typed "(a (b) c)"))
           (row (row-showing f "(a (b) c)")))
      (true row)
      (true (member :paren-match (faces-at f row 0)))
      (true (member :paren-match (faces-at f row 8)))
      (false (member :paren-match (faces-at f row 3))))
    ;; the inner pair, with point after its closing bracket
    (let* ((f (fresh-start (format nil "o ~a C-g h h h" (key-string "(a (b) c)")) :width 80))
           (row (row-showing f "(a (b) c)")))
      (true (member :paren-match (faces-at f row 3)))
      (true (member :paren-match (faces-at f row 5)))
      (false (member :paren-match (faces-at f row 0))))
    ;; one of another kind
    (let* ((f (typed "(a]"))
           (row (row-showing f "(a]")))
      (true (member :paren-mismatch (faces-at f row 0)))
      (true (member :paren-mismatch (faces-at f row 2))))
    ;; none before the text starts: the comments above hold no brackets that count
    (let* ((f (typed "a)"))
           (row (row-showing f "a)")))
      (true (member :paren-mismatch (faces-at f row 1))))
    ;; a bracket in a string is only text
    (let* ((f (fresh-start (format nil "o ~a C-g h" (key-string "(\"a)\"")) :width 80))
           (row (row-showing f "(\"a)\"")))
      (false (member :paren-match (faces-at f row 3)))
      (false (member :paren-mismatch (faces-at f row 3))))
    ;; switched off (here for this test alone)
    (let ((tuatara-user:*show-paren-mode* t))
      (let* ((f (fresh-start (format nil "M-x s h o w - p a r e n - m o d e RET o ~a" (key-string "(a)")) :width 80))
             (row (row-showing f "(a)")))
        (true (showing f "Show-Paren mode disabled"))
        (false (member :paren-match (faces-at f row 0)))))))

(define-test (show-paren brackets-inside-a-node)
  ;; C, Rust and shell nodes that hold their brackets inside them, not at their ends
  (flet ((ranges (mode text point)
           (let ((b (tuatara.buffer:make-buffer (format nil "*paren-~d*" (random 1000000)) :text text)))
             (unwind-protect
                  (progn (tuatara.modes:set-major-mode b mode)
                         (let ((tuatara.syntax:*syntax-wait* 5)) (tuatara.defaults::ready-syntax b))
                         (tuatara.defaults::paren-ranges b point))
               (tuatara.buffer:kill-buffer b))))
         (pair (open close) (list (list open (1+ open) :paren-match) (list close (1+ close) :paren-match))))
    (let* ((text "void f() { for (i = 0; i < n; i++) { x(i); } }")
           (open (search "(i =" text))
           (close (search ") {" text :start2 open)))
      ;; for's own brackets, not its body's
      (is equal (pair open close) (ranges 'tuatara.defaults::c-mode text open))
      (is equal (reverse (pair open close)) (ranges 'tuatara.defaults::c-mode text (1+ close))))
    (let* ((text "int g() { return values[i] + (size_t)(n + 1); }")
           (open (search "[i]" text)))
      ;; a subscript's closing bracket, after a long name
      (is equal (reverse (pair open (+ open 2))) (ranges 'tuatara.defaults::c-mode text (+ open 3)))
      ;; a cast's own brackets, not its operand's
      (is equal (pair (search "(size_t)" text) (+ 7 (search "(size_t)" text)))
          (ranges 'tuatara.defaults::c-mode text (search "(size_t)" text))))
    (let ((text (format nil "foo() {~%  echo hi~%}~%")))
      (is equal (pair 3 4) (ranges 'tuatara.defaults::bash-mode text 3)))
    (let ((text "fn f() { let a = Some(x); }"))
      (is equal (reverse (pair (search "(x)" text) (+ 2 (search "(x)" text))))
          (ranges 'tuatara.defaults::rust-mode text (+ 3 (search "(x)" text)))))))

(define-test (show-paren which-brackets-are-shown)
  ;; an escaped bracket is no bracket, in Lisp as in text
  (flet ((ranges (mode text point)
           (let ((b (tuatara.buffer:make-buffer (format nil "*paren-~d*" (random 1000000)) :text text)))
             (unwind-protect
                  (progn (when mode (tuatara.modes:set-major-mode b mode))
                         (let ((tuatara.syntax:*syntax-wait* 5)) (tuatara.defaults::ready-syntax b))
                         (tuatara.defaults::paren-ranges b point))
               (tuatara.buffer:kill-buffer b)))))
    (true (ranges 'tuatara.defaults::lisp-mode "(insert)" 8))
    (false (ranges 'tuatara.defaults::lisp-mode "(insert\\) )" 9))
    (false (ranges nil "(a\\)" 4))
    (true (ranges nil "(a\\\\)" 5))
    ;; the closing bracket before point, then the opening one after it, nothing inside
    (is equal '((7 8 :paren-match) (0 1 :paren-match)) (ranges 'tuatara.defaults::lisp-mode "(insert)" 8))
    (false (ranges 'tuatara.defaults::lisp-mode "(insert)" 7))
    (is equal '((0 1 :paren-match) (7 8 :paren-match)) (ranges 'tuatara.defaults::lisp-mode "(insert)" 0))
    (false (ranges 'tuatara.defaults::lisp-mode "(insert)" 1))
    ;; an opening bracket without its partner: a mismatch
    (is equal '((0 1 :paren-mismatch)) (ranges 'tuatara.defaults::lisp-mode "(insert" 0)))
  ;; the line an opening bracket out of sight is shown by
  (flet ((line-of (text at)
           (let ((b (tuatara.buffer:make-buffer (format nil "*paren-~d*" (random 1000000)) :text text)))
             (unwind-protect (tuatara.defaults::open-bracket-line-text b at)
               (tuatara.buffer:kill-buffer b)))))
    (is string= "(defun foo ()" (line-of (format nil "(defun foo ()~%                  (dummy))") 0))
    (is string= "int foo() {" (line-of (format nil "int foo() {~%   int blah;~%}") 10))
    (is string= "int foo()...{" (line-of (format nil "int foo()~%               {~%  int blah;~%}") 25))
    (is string= "{" (line-of "{" 0))))

(define-test (show-paren typing-a-closing-bracket)
  ;; its partner out of sight above the window: its line said, as blink-matching-open
  (let ((f (fresh-start (format nil "o ~a RET ~{~a~} ~a" (key-string "(defun far-away ()")
                                (loop repeat 30 collect "x RET ") ")")
                        :width 120)))
    (true (showing f "Matches (defun far-away ()")))
  ;; none, or one of another kind
  (true (showing (fresh-start (format nil "o ~a" (key-string "a)")) :width 120) "No matching parenthesis found"))
  (true (showing (fresh-start (format nil "o ~a" (key-string "(a]")) :width 120) "Mismatched parentheses"))
  ;; in sight: nothing said, the brackets shown
  (false (showing (fresh-start (format nil "o ~a" (key-string "(a)")) :width 120) "Matches"))
  ;; off
  (let ((tuatara-user:*blink-matching-paren* nil))
    (false (showing (fresh-start (format nil "o ~a" (key-string "a)")) :width 120) "No matching parenthesis found"))))

