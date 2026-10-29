;;;; test/lists.lisp --- Lists and definitions: Emacs's C-M- commands.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Many of the cases are those of Emacs's test/lisp/emacs-lisp/lisp-tests.el
;;;; (GNU Emacs 30.2, GPL-3.0-or-later), written again for Common Lisp.

(in-package #:tuatara.tests)

(define-test lists
  :description "C-M-f and C-M-b at the edges of the text and over character literals; C-M-n and
C-M-p over groups, C-M-u and up-list out of them, of strings and of comments, C-M-d into them;
kill-sexp, C-M-DEL and C-M-SPC; delete-pair; C-M-a, C-M-e and mark-defun, by brackets at the start
of lines in Lisp and by each language's rules in Rust, C and shell scripts; M-q in a Lisp string.")

(defun session-in (mode text keys &key (point 0) mark (width 60) (height 10))
  "Run KEYS in an editor session on a buffer holding TEXT in major mode MODE
(a mode's name in TUATARA.DEFAULTS, or NIL for the standard one), point at
POINT and, with MARK, the mark there and active, its syntax tree waited for;
return the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (let ((b (tuatara.buffer:make-buffer "*scratch*" :text text)))
      (when mode (tuatara.modes:set-major-mode b (find-symbol (symbol-name mode) :tuatara.defaults)))
      (setf (tuatara.buffer:buffer-point b) point)
      (when mark
        (setf (tuatara.buffer:buffer-mark b) mark (tuatara.buffer:buffer-mark-active-p b) t))
      (let ((tuatara.syntax:*syntax-wait* 5)) (tuatara.defaults::ready-syntax b)))
    (setf tuatara.defaults::*command-history* '())
    (tuatara.frontend.fake:fake-queue-keys f keys)
    (let ((tuatara.syntax:*syntax-wait* 5))
      (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                              :buffer (tuatara.buffer:find-buffer "*scratch*"))))))

(defun echo-text (frontend)
  "What the echo area says in FRONTEND's last frame, without the blanks around it."
  (string-trim " " (tuatara.frontend.fake:fake-screen-line frontend (1- (length (tuatara.frontend.fake:fake-screen frontend))))))

(defun session-mark (editor)
  "The mark of EDITOR's current buffer, and whether it is active."
  (let ((b (session-buffer editor)))
    (values (tuatara.buffer:buffer-mark b) (tuatara.buffer:buffer-mark-active-p b))))

(defun marked-text (text)
  "TEXT without its =!NAME= markers, and as a second value a list of each NAME,
a keyword, with where it stood: (NAME . POSITION)."
  (let ((out (make-string-output-stream)) (places '()) (i 0) (length 0))
    (loop while (< i (length text))
          do (let ((close (and (< (1+ i) (length text)) (char= (char text i) #\=) (char= (char text (1+ i)) #\!)
                               (position #\= text :start (+ i 2)))))
               (if close
                   (progn (push (cons (intern (string-upcase (subseq text (+ i 2) close)) :keyword) length) places)
                          (setf i (1+ close)))
                   (progn (write-char (char text i) out) (incf length) (incf i)))))
    (values (get-output-stream-string out) places)))

(define-test (lists markers-read)
  (multiple-value-bind (text places) (marked-text "a=!one=bc=!two==!three=d")
    (is string= "abcd" text)
    (is = 1 (cdr (assoc :one places)))
    (is = 3 (cdr (assoc :two places)))
    (is = 3 (cdr (assoc :three places)))))

;;; Balanced expressions

(define-test (lists sexp-edges)
  (flet ((moved (mode text keys &optional (point 0))
           (multiple-value-bind (f editor) (session-in mode text keys :point point)
             (list (session-point editor) (echo-text f)))))
    (dolist (mode '(:lisp-mode nil))
      ;; at the end of the text, and at its start, no complaint
      (is equal '(2 "") (moved mode "()" "C-M-f"))
      (is equal '(2 "") (moved mode "()" "C-M-f C-M-f"))
      (is equal '(2 "") (moved mode "()" "C-u 2 C-M-f"))
      (is equal '(0 "") (moved mode "()" "C-M-b" 2))
      (is equal '(0 "") (moved mode "()" "C-M-b C-M-b" 2))
      (is equal '(0 "") (moved mode "()" "C-u 2 C-M-b" 2))
      ;; a bracket without its partner
      (is equal '(0 "The brackets do not balance") (moved mode "(" "C-M-f"))
      (is equal '(1 "At the start of the expression around point") (moved mode "(" "C-M-b" 1))
      ;; at the end of the group around point
      (is equal '(4 "At the end of the expression around point") (moved mode "(a b)" "C-M-f" 4))
      ;; a negative count goes the other way
      (is equal '(0 "") (moved mode "(a) (b)" "C-u - 2 C-M-f" 7))
      (is equal '(7 "") (moved mode "(a) (b)" "C-u - 2 C-M-b"))
      ;; from inside a name, to its end
      (is = 14 (first (moved mode (format nil "hide-ifdef-env ~a" (coerce (loop for c from 32 to 126 collect (code-char c)) 'string))
                             "C-M-f" 4))))
    ;; character literals are no brackets or quote marks to the grammar
    (is equal '(12 "") (moved :lisp-mode "(insert #\\;)" "C-M-f"))
    (is equal '(12 "") (moved :lisp-mode "(insert #\\\")" "C-M-f"))
    (is equal '(12 "") (moved :lisp-mode "(insert #\\))" "C-M-f"))))

;;; Groups

(define-test (lists out-of-groups)
  (flet ((ups (text keys point)
           (multiple-value-bind (f editor) (session-in :lisp-mode text keys :point point)
             (list (session-point editor) (echo-text f)))))
    (let ((up "M-x u p - l i s t RET"))
      ;; up-list in (1 1 (1 1) 1 (1 1) 1): from inside the first inner group past its
      ;; closing bracket, then past the whole's, then no further
      (is equal '(10 "") (ups "(1 1 (1 1) 1 (1 1) 1)" up 8))
      (is equal '(21 "") (ups "(1 1 (1 1) 1 (1 1) 1)" (format nil "~a ~a" up up) 8))
      (is equal '(21 "At top level") (ups "(1 1 (1 1) 1 (1 1) 1)" (format nil "~a ~a ~a" up up up) 8))
      (is equal '(21 "At top level") (ups "(1 1 (1 1) 1 (1 1) 1)" (format nil "C-u 3 ~a" up) 8))
      ;; C-M-u from there: back to the inner group's opening bracket, to the whole's, then no further
      (is equal '(5 "") (ups "(1 1 (1 1) 1 (1 1) 1)" "C-M-u" 8))
      (is equal '(0 "") (ups "(1 1 (1 1) 1 (1 1) 1)" "C-M-u C-M-u" 8))
      (is equal '(0 "At top level") (ups "(1 1 (1 1) 1 (1 1) 1)" "C-M-u C-M-u C-M-u" 8))
      ;; out of a string: an unclosed bracket in the string counts for nothing
      (is equal '(17 "") (ups "1 (1 \"2 2 (2 2 2\" 1) 1" up 14))
      (is equal '(20 "") (ups "1 (1 \"2 2 (2 2 2\" 1) 1" (format nil "C-u 2 ~a" up) 14))
      (is equal '(20 "At top level") (ups "1 (1 \"2 2 (2 2 2\" 1) 1" (format nil "C-u 3 ~a" up) 14))
      ;; brackets in two strings never pair
      (is equal '(10 "") (ups "(1 \"2 ( 2\" 1 \"2 ) 2\" 1)" up 8))
      (is equal '(23 "") (ups "(1 \"2 ( 2\" 1 \"2 ) 2\" 1)" (format nil "C-u 2 ~a" up) 8))
      (is equal '(23 "At top level") (ups "(1 \"2 ( 2\" 1 \"2 ) 2\" 1)" (format nil "C-u 3 ~a" up) 8))
      ;; a negative count goes the other way
      (is equal '(5 "") (ups "(1 1 (1 1) 1 (1 1) 1)" (format nil "C-u - ~a" up) 8))
      (is equal '(10 "") (ups "(1 1 (1 1) 1 (1 1) 1)" "C-u - C-M-u" 8)))
    ;; in a string, the brackets inside it around point first, then to its start
    (is equal '(6 "") (ups "(a \"b (c d) e\")" "C-M-u" 8))
    (is equal '(3 "") (ups "(a \"b (c d) e\")" "C-M-u C-M-u" 8))
    (is equal '(0 "") (ups "(a \"b (c d) e\")" "C-M-u C-M-u C-M-u" 8))
    ;; in a comment, the brackets inside it, then on to the group around it
    (is equal '(5 "") (ups (format nil "(a ; (b c~%   d)") "C-M-u" 8))
    (is equal '(0 "") (ups (format nil "(a ; b c~%   d)") "C-M-u" 8))
    (is equal '(14 "") (ups (format nil "(a ; b c~%   d)") "M-x u p - l i s t RET" 8)))
  ;; without a grammar every bracket counts
  (multiple-value-bind (f editor) (session-in nil "x [y {z}] \"(\"" "C-M-u" :point 7)
    (declare (ignore f))
    (is = 5 (session-point editor))))

(define-test (lists into-groups)
  (flet ((downs (mode text keys &optional (point 0))
           (multiple-value-bind (f editor) (session-in mode text keys :point point)
             (list (session-point editor) (echo-text f)))))
    (is equal '(1 "") (downs :lisp-mode "(a (b) c)" "C-M-d"))
    (is equal '(4 "") (downs :lisp-mode "(a (b) c)" "C-M-d C-M-d"))
    (is equal '(4 "") (downs :lisp-mode "(a (b) c)" "C-u 2 C-M-d"))
    (is equal '(4 "At bottom level") (downs :lisp-mode "(a (b) c)" "C-M-d C-M-d C-M-d"))
    (is equal '(0 "At bottom level") (downs :lisp-mode "a b" "C-M-d"))
    ;; backward, to the inside of the group's end
    (is equal '(8 "") (downs :lisp-mode "(a (b) c)" "C-u - C-M-d" 9))
    (is equal '(5 "") (downs :lisp-mode "(a (b) c)" "C-u - C-M-d C-u - C-M-d" 9))
    ;; strings are passed over, and are no place to go down from
    (is equal '(7 "") (downs :lisp-mode "\"(\" x (y)" "C-M-d"))
    (is equal '(5 "This command doesn't work in strings or comments") (downs :lisp-mode "(a \"bc\")" "C-M-d" 5))
    (is equal '(6 "") (downs nil "x [y {z}]" "C-u 2 C-M-d"))))

(define-test (lists over-groups)
  (flet ((overs (mode text keys &optional (point 0))
           (multiple-value-bind (f editor) (session-in mode text keys :point point)
             (list (session-point editor) (echo-text f)))))
    (is equal '(5 "") (overs :lisp-mode "a (b) c (d) e" "C-M-n"))
    (is equal '(11 "") (overs :lisp-mode "a (b) c (d) e" "C-M-n C-M-n"))
    ;; with no group left, to the end of the text
    (is equal '(13 "") (overs :lisp-mode "a (b) c (d) e" "C-M-n C-M-n C-M-n"))
    (is equal '(8 "") (overs :lisp-mode "a (b) c (d) e" "C-M-p" 13))
    (is equal '(0 "") (overs :lisp-mode "a (b) c (d) e" "C-u 3 C-M-p" 13))
    (is equal '(8 "") (overs :lisp-mode "a (b) c (d) e" "C-u - C-M-n" 13))
    ;; the group around point ends first
    (is equal '(7 "No next group") (overs :lisp-mode "(a (b) c)" "C-M-n" 7))
    (is equal '(1 "No previous group") (overs :lisp-mode "(a (b) c)" "C-M-p" 1))
    ;; a string's brackets count for nothing; a quote before a group is not part of it
    (is equal '(7 "") (overs :lisp-mode "\"(\" (a) b" "C-M-n"))
    (is equal '(1 "") (overs :lisp-mode "'(a b) c" "C-M-p" 6))
    (is equal '(0 "The brackets do not balance") (overs nil "\"(\" (a) b" "C-M-n"))
    (is equal '(9 "") (overs nil "x [y] {z}" "C-M-n C-M-n"))))

;;; Killing, marking and deleting pairs

(define-test (lists sexps-killed)
  (flet ((kills (text keys &optional (point 0))
           (multiple-value-bind (f editor) (session-in :lisp-mode text keys :point point)
             (list (session-text editor) (session-point editor)
                   (let ((kill (first (tuatara.command:editor-kill-ring editor)))) (and (stringp kill) kill))
                   (echo-text f)))))
    (let ((kill "M-x k i l l - s e x p RET"))
      (is equal '(" c" 0 "(a b)" "") (kills "(a b) c" kill))
      (is equal '(" c" 0 "a b" "") (kills "a b c" (format nil "C-u 2 ~a" kill)))
      (is equal '("(a b)" 4 nil "No next sexp") (kills "(a b)" kill 4))
      (is equal '("a " 2 "(b c)" "") (kills "a (b c)" (format nil "C-u - ~a" kill) 7)))
    (is equal '("a " 2 "(b c)" "") (kills "a (b c)" "C-M-DEL" 7))
    ;; kills in a row are one, the earlier text first
    (is equal '("x " 2 "(a) (b)" "") (kills "x (a) (b)" "C-M-DEL C-M-DEL" 9))
    (is equal '("x (a) (b)" 9 "(a) (b)" "Mark set") (kills "x (a) (b)" "C-M-DEL C-M-DEL C-y" 9))
    (is equal '("(a b)" 1 nil "No previous sexp") (kills "(a b)" "C-M-DEL" 1))))

(define-test (lists sexps-marked)
  (flet ((marks (text keys &key (point 0) mark)
           (multiple-value-bind (f editor) (session-in :lisp-mode text keys :point point :mark mark)
             (multiple-value-bind (mark active) (session-mark editor)
               (list (session-point editor) mark active (echo-text f))))))
    (is equal '(0 3 t "Mark set") (marks "(a) (b) c" "C-M-SPC"))
    (is equal '(0 7 t "") (marks "(a) (b) c" "C-M-SPC C-M-SPC"))
    (is equal '(0 7 t "Mark set") (marks "(a) (b) c" "C-u 2 C-M-SPC"))
    (is equal '(0 9 t "") (marks "(a) (b) c" "C-M-SPC C-M-SPC C-M-SPC C-M-SPC"))
    (is equal '(7 4 t "Mark set") (marks "(a) (b) c" "C-u - C-M-SPC" :point 7))
    ;; with the region active the mark goes one further, back when it is before point
    (is equal '(7 0 t "") (marks "(a) (b) c" "C-M-SPC" :point 7 :mark 4))
    (is equal '(0 7 t "") (marks "(a) (b) c" "C-M-SPC" :mark 3))
    ;; none to mark
    (is equal '(4 nil nil "No sexp to select") (marks "(a b)" "C-M-SPC" :point 4))
    (is equal '(1 6 t "No more sexp to select") (marks "((a) b) c" "C-M-SPC C-M-SPC C-M-SPC" :point 1))))

(define-test (lists pair-deleted)
  (flet ((deletes (mode text keys &optional (point 0))
           (multiple-value-bind (f editor) (session-in mode text keys :point point)
             (list (session-text editor) (session-point editor) (echo-text f)))))
    (let ((delete "M-x d e l e t e - p a i r RET"))
      ;; the brackets, or the quote marks, of the expression after point
      (is equal '("foo" 0 "") (deletes :lisp-mode "(foo)" delete))
      (is equal '("foo" 0 "") (deletes :lisp-mode "\"foo\"" delete))
      ;; in text a quoted word goes whole, and quote marks around more are left, not the wrong
      ;; ones deleted
      (is equal '("foo" 0 "") (deletes nil "\"foo\"" delete))
      (is equal '("\"foo bar\"" 0 "Not before matching pair") (deletes nil "\"foo bar\"" delete))
      (is equal '("foo" 0 "Not before matching pair") (deletes :lisp-mode "foo" delete))
      ;; every pair the pair commands know, those added in the init file too
      (is equal '("foo" 0 "") (deletes nil "«foo»" delete))
      (is equal '("foo" 0 "") (deletes nil "‘foo’" delete))
      ;; spaces before the pair passed over; point stays
      (is equal '("x  a b y" 1 "") (deletes nil "x  [a b] y" delete 1))
      ;; the pair around the expressions before point
      (is equal '("x foo y" 5 "") (deletes :lisp-mode "x (foo) y" (format nil "C-u - ~a" delete) 7))
      (is equal '("(foo" 0 "The brackets do not balance") (deletes :lisp-mode "(foo" delete))
      ;; both go in one step, undone together
      (is equal '("x (foo) y" 2 "") (deletes :lisp-mode "x (foo) y" (format nil "~a C-/" delete) 2)))))

;;; Definitions

(defparameter *mark-defun-text* ";; Comment header
=!before-1=
(defun func-1 (arg)
  =!inside-1=\"docstring\"
  body)
=!after-1==!before-2=
;; Comment before a defun
(d=!inside-2=efun func-2 (arg)
  \"docstring\"
  body)
=!after-2==!before-3=
(defun func-3 (arg)
  \"docstring\"=!inside-3=
  body)
=!after-3==!before-4=(defun func-4 (arg)
  \"docstring\"=!inside-4=
  body)
=!after-4=
;; end
"
  "Four definitions and comments for mark-defun, its =!NAME= markers where point and the mark are set and looked for.")

(define-test (lists defuns-marked)
  (flet ((marked (source keys from &optional mark)
           ;; point and the mark after KEYS from marker FROM, as (POINT MARK) by marker name
           (multiple-value-bind (text places) (marked-text source)
             (multiple-value-bind (f editor) (session-in :lisp-mode text keys :point (cdr (assoc from places))
                                                                               :mark (and mark (cdr (assoc mark places))))
               (declare (ignore f))
               (list (session-point editor) (session-mark editor)))))
         (at (source &rest names)
           (let ((places (nth-value 1 (marked-text source))))
             (mapcar (lambda (name) (cdr (assoc name places))) names))))
    (let ((mark "M-x m a r k - d e f u n RET")
          (text *mark-defun-text*))
      ;; no count, the region inactive
      (is equal (at text :before-1 :after-1) (marked text mark :inside-1))
      (is equal (at text :before-2 :after-2) (marked text mark :inside-2))
      (is equal (at text :before-3 :after-3) (marked text mark :inside-3))
      (is equal (at text :before-4 :after-4) (marked text mark :inside-4))
      (is equal (at text :before-1 :after-1) (marked text mark :before-1))
      (is equal (at text :before-3 :after-3) (marked text mark :before-3))
      (is equal (at text :before-2 :after-2) (marked text mark :before-2))
      ;; no count, the region active
      (is equal (at text :before-1 :after-2) (marked text mark :before-1 :after-1))
      (is equal (at text :before-1 :after-3) (marked text mark :before-1 :after-2))
      ;; a count, the region active
      (is equal (at text :before-1 :after-3) (marked text (format nil "C-u 2 ~a" mark) :before-1 :after-1))
      (is equal (at text :before-1 :after-2) (marked text (format nil "C-u - ~a" mark) :before-2 :after-2))
      (is equal (at text :before-1 :after-3) (marked text (format nil "C-u - 2 ~a" mark) :before-3 :after-3))
      ;; a positive count, the region inactive
      (is equal (at text :before-1 :after-2) (marked text (format nil "C-u 2 ~a" mark) :inside-1))
      (is equal (at text :before-3 :after-4) (marked text (format nil "C-u 2 ~a" mark) :before-3))
      (is equal (at text :before-2 :after-3) (marked text (format nil "C-u 2 ~a" mark) :before-2))
      ;; a negative count, the region inactive
      (is equal (at text :before-1 :after-1) (marked text (format nil "C-u - ~a" mark) :inside-1))
      (is equal (at text :before-2 :after-2) (marked text (format nil "C-u - ~a" mark) :after-2))
      (is equal (at text :before-1 :after-1) (marked text (format nil "C-u - ~a" mark) :before-2))
      (is equal (at text :before-3 :after-4) (marked text (format nil "C-u - 2 ~a" mark) :inside-4))
      (is equal (at text :before-1 :after-2) (marked text (format nil "C-u - 2 ~a" mark) :before-3))
      (let ((drew (format nil "(defun a ()~%  nil)~%=!before-b=(defun b ()~%=!in-b=  nil)~%=!after-b=;;;;~%(defun c ()~%  nil)~%")))
        (is equal (at drew :before-b :after-b) (marked drew (format nil "C-u - ~a" mark) :in-b)))
      ;; a definition near the start of the text, comments before it
      (dolist (source (list (format nil ";; Comment at the bob~%=!before=~%(defun func (arg)=!inside=~%  \"docstring\"~%  body)~%=!after=")
                            (format nil "=!before=~%;; Comment before the defun~%(defun func (arg)=!inside=~%  \"docstring\"~%  body)~%=!after=")
                            (format nil "=!before=;; Comment at the bob before the defun~%(defun func (arg)=!inside=~%  \"docstring\"~%  body)~%=!after=")
                            (format nil "~%;; Comment before the defun~%=!before=~%(defun func (arg)=!inside=~%  \"docstring\"~%  body)~%=!after=")))
        (is equal (at source :before :after) (marked source mark :inside))))))

(define-test (lists defun-ends)
  (flet ((point-after (text keys &optional (point 0))
           (session-point (nth-value 1 (session-in :lisp-mode text keys :point point)))))
    ;; C-M-e twice goes where C-u 2 C-M-e goes
    (dolist (text (list (format nil "(defun a ())~%~%(defun b ())~%~%(defun c ())")
                        (format nil "(defun a ())~%(defun b ())~%(defun c ())")))
      (is = (point-after text "C-M-e C-M-e") (point-after text "C-u 2 C-M-e")))
    (is = 27 (point-after (format nil "(defun a ())~%~%(defun b ())~%~%(defun c ())") "C-M-e C-M-e"))
    (multiple-value-bind (text places)
        (marked-text (format nil ";; Comment header~%~%(defun func-1 (arg)~%  \"docstring\"~%  body)~%=!p1=~%;; Comment before a defun~%(defun func-2 (arg)~%  \"docstring\"~%  body)~%~%(defun func-3 (arg)~%  \"docstring\"~%  body)~%=!p2=(defun func-4 (arg)~%  \"docstring\"~%  body)~%~%;; end~%"))
      (is = (cdr (assoc :p2 places)) (point-after text "C-u 2 C-M-e" (cdr (assoc :p1 places)))))
    ;; C-M-a to the start of the definition, of the one before, then of the text; a bracket
    ;; at the start of a line in a string or comment starts none
    (let ((text (format nil "(a)~%(b~% \"~%(not one\")~%#|~%(nor this~%|#~%(c)")))
      (is = 37 (point-after text "C-M-a" 39))
      (is = 4 (point-after text "C-M-a C-M-a" 39))
      (is = 0 (point-after text "C-M-a C-M-a C-M-a C-M-a" 39))
      (is = 4 (point-after text "C-u - C-M-a" 1))
      (is = 37 (point-after text "C-u - 2 C-M-a" 1))
      (is = 40 (point-after text "C-u - 3 C-M-a" 1)))))

(defparameter *rust-definitions* "fn a() {
    1
}

impl Foo {
    fn b() {
        2
    }

    fn c() {
        let f = |x| {
            x
        };
    }
}

struct S {
    x: i32,
}
"
  "Rust with definitions inside another and a closure, which is none.")

(defun line-and-column (text position)
  "POSITION of TEXT as (LINE COLUMN), the first line 1."
  (let ((newline (position #\Newline text :end position :from-end t)))
    (list (1+ (count #\Newline text :end position)) (- position (if newline (1+ newline) 0)))))

(define-test (lists definitions-by-rules)
  (flet ((goes (mode text from keys)
           (multiple-value-bind (f editor) (session-in mode text keys :point (search from text))
             (append (line-and-column text (session-point editor)) (list (echo-text f))))))
    (let ((text *rust-definitions*))
      ;; back out to the start of the definition around point, then of those beside and around it
      (is equal '(10 0 "Mark set") (goes :rust-mode text "x
" "C-M-a"))
      (is equal '(6 0 "") (goes :rust-mode text "x
" "C-M-a C-M-a"))
      (is equal '(5 0 "") (goes :rust-mode text "x
" "C-M-a C-M-a C-M-a"))
      (is equal '(1 0 "Mark set") (goes :rust-mode text "x
" "C-u 4 C-M-a"))
      (is equal '(1 0 "No definition before point") (goes :rust-mode text "x
" "C-u 4 C-M-a C-M-a"))
      ;; on to the ends, and past the line end after them
      (is equal '(9 0 "Mark set") (goes :rust-mode text "2" "C-M-e"))
      (is equal '(15 0 "") (goes :rust-mode text "2" "C-M-e C-M-e"))
      (is equal '(16 0 "") (goes :rust-mode text "2" "C-M-e C-M-e C-M-e"))
      (is equal '(20 0 "Mark set") (goes :rust-mode text "2" "C-u 4 C-M-e"))
      (is equal '(20 0 "No definition after point") (goes :rust-mode text "2" "C-u 4 C-M-e C-M-e"))
      ;; the other way, never into a definition
      (is equal '(5 0 "Mark set") (goes :rust-mode text "1" "C-u - C-M-a"))
      (is equal '(17 0 "Mark set") (goes :rust-mode text "1" "C-u - 2 C-M-a"))
      (is equal '(16 0 "Mark set") (goes :rust-mode text "x: i32" "C-u - C-M-e"))
      ;; mark-defun marks the function around point, the blank line above it too
      (multiple-value-bind (f editor) (session-in :rust-mode text "M-x m a r k - d e f u n RET" :point (search "let f" text))
        (declare (ignore f))
        (is equal '(9 0) (line-and-column text (session-point editor)))
        (is equal '(15 0) (line-and-column text (session-mark editor)))))
    (let ((text (format nil "#include <stdio.h>~%#define MAX 10~%~%struct point {~%    int x;~%};~%~%static int~%add(int a, int b)~%{~%    return a + b;~%}~%")))
      ;; C's functions, structs and macros; an #include is none
      (is equal '(8 0 "Mark set") (goes :c-mode text "return" "C-M-a"))
      (is equal '(4 0 "") (goes :c-mode text "return" "C-M-a C-M-a"))
      (is equal '(2 0 "Mark set") (goes :c-mode text "return" "C-u 3 C-M-a"))
      (is equal '(2 0 "No definition before point") (goes :c-mode text "return" "C-u 3 C-M-a C-M-a"))
      ;; past the semicolon after a struct
      (is equal '(7 0 "Mark set") (goes :c-mode text "int x" "C-M-e"))
      (is equal '(13 0 "Mark set") (goes :c-mode text "#include" "C-u 3 C-M-e")))
    (let ((text (format nil "#!/bin/bash~%f() {~%  echo a~%}~%~%g() {~%  echo b~%}~%echo done~%")))
      ;; shell functions; other commands are none
      (is equal '(5 0 "Mark set") (goes :bash-mode text "#!" "C-M-e"))
      (is equal '(9 0 "") (goes :bash-mode text "#!" "C-M-e C-M-e"))
      (is equal '(9 0 "No definition after point") (goes :bash-mode text "#!" "C-M-e C-M-e C-M-e"))
      (is equal '(6 0 "Mark set") (goes :bash-mode text "done" "C-M-a")))))

;;; Filling a Lisp string

(define-test (lists docstrings-filled)
  (flet ((filled (text from keys)
           (session-text (nth-value 1 (session-in :lisp-mode text keys :point (search from text))))))
    ;; the keywords after the string stay as they are
    (let ((text (format nil "~%(defcustom custom value~%  \"First~%~%Second~%~%Third line\"~%  :type 'sexp~%  :version \"26.1\"~%  :group 'lisp-tests)")))
      (is string= text (filled text "Third line" "M-q RET")))
    ;; keywords in the string are words like others; the code after it is left
    (is string= (format nil "~%(defun foo ()~%  \"Summary.~%Testing keywords: :one :two~%:three\"~%  (body))")
        (filled (format nil "~%(defun foo ()~%  \"Summary.~%Testing keywords: :one :two :three\"~%  (body))") "Testing" "C-u 3 0 M-q"))
    ;; the first line alone, its words on from the start of the lines after
    (is string= (format nil "(defun foo ()~%  \"A summary line that goes on~%well past the fill column of~%thirty.~%and more\"~%  (body))")
        (filled (format nil "(defun foo ()~%  \"A summary line that goes on well past the fill column of thirty.~%and more\"~%  (body))")
                "summary" "C-u 3 0 M-q"))
    ;; outside strings and comments, nothing
    (multiple-value-bind (f editor) (session-in :lisp-mode "(foo bar)" "M-q" :point 2)
      (is string= "(foo bar)" (session-text editor))
      (is string= "Nothing to fill here: Lisp mode fills only comments and strings" (echo-text f)))))

(define-test (lists fill-region-reads-on)
  ;; filling a region of Markdown reads whether each line is in a code fence on from the line
  ;; before, not back over the file for each paragraph: the same lines read for each paragraph
  ;; however many there are; a fenced line stays as it is
  (flet ((filled (paragraphs)
           (let* ((head (format nil "~{~a~%~}```~%a fenced line that is long and stays as it is~%```~%"
                                (loop repeat 3000 collect "x")))
                  (text (format nil "~a~{~a~^~%~%~}~%" head
                                (loop repeat paragraphs collect "some words that make a line longer than twenty")))
                  (read 0))
             (sb-int:encapsulate 'tuatara.buffer:buffer-line-text 'counted
                                 (lambda (function &rest arguments) (incf read) (apply function arguments)))
             (unwind-protect
                  (let ((after (session-text (nth-value 1 (session-in :markdown-mode text "C-u 2 0 M-x f i l l - r e g i o n RET"
                                                                      :point (search "```" text) :mark (length text))))))
                    (true (search "a fenced line that is long and stays as it is" after))
                    (false (search "longer than twenty" after)))
               (sb-int:unencapsulate 'tuatara.buffer:buffer-line-text 'counted))
             read)))
    (let ((few (filled 10)) (many (filled 20)))
      (true (< (/ (- many few) 10) 300) "~d lines read for 10 paragraphs, ~d for 20" few many))))

(define-test (lists paragraph-bounds-edges)
  (flet ((bounds (mode text at &optional (how 'tuatara.defaults::paragraph-bounds))
           (let ((b (tuatara.buffer:make-buffer "*paragraph bounds*" :text text)))
             (unwind-protect
                  (progn (when mode (tuatara.modes:set-major-mode b (find-symbol (symbol-name mode) :tuatara.defaults)))
                         (let ((tuatara.syntax:*syntax-wait* 5)) (tuatara.defaults::ready-syntax b))
                         (multiple-value-list (funcall how b (if (stringp at) (search at text) at))))
               (tuatara.buffer:kill-buffer b)))))
    ;; text: the first paragraph, the last one with no newline at its end, a blank line between
    (let ((text (format nil "one~%two~%~%three")))
      (is equal '(0 8) (bounds nil text "two"))
      (is equal '(9 14) (bounds nil text "three"))
      (is equal '(nil) (bounds nil text 8)))
    ;; an item of a list is a paragraph of its own: up from its last line to its first, down to the next item
    (let ((text (format nil "intro~%- a~%  b~%- c~%")))
      (is equal '(0 6) (bounds nil text "intro"))
      (is equal '(6 14) (bounds nil text "  b"))
      (is equal '(14 18) (bounds nil text "- c")))
    ;; a paragraph larger than the limit is refused, above and below
    (let ((tuatara.defaults::*fill-paragraph-limit* 8)
          (text (format nil "aaaa~%bbbb~%cccc~%dddd~%eeee~%")))
      (fail (bounds nil text "cccc"))
      (fail (bounds nil text "aaaa")))
    ;; in a string: the first line alone, then paragraphs between blank lines up to the closing quote mark
    (let ((text (format nil "(defun f ()~%  \"Sum.~%~%one~%two~%~%three\")")))
      (is equal (list (search "Sum" text) (+ 2 (search (format nil ".~%") text)))
          (bounds :lisp-mode text "Sum" 'tuatara.defaults::string-paragraph-bounds))
      (is equal (list (search "one" text) (+ 4 (search "two" text)))
          (bounds :lisp-mode text "two" 'tuatara.defaults::string-paragraph-bounds))
      (is equal (list (search "three" text) (+ 5 (search "three" text)))
          (bounds :lisp-mode text "three" 'tuatara.defaults::string-paragraph-bounds)))))

(define-test (lists symbol-motion-bounded)
  ;; a symbol is found by its octets, no further than the scan limit
  (let ((b (tuatara.buffer:make-buffer "*symbol motion*" :text (format nil "(ab~c cd) xyz" (code-char #x101)))))
    (unwind-protect
         (progn
           (is = 5 (tuatara.defaults::sexp-end-after b 1))
           (is = 1 (tuatara.defaults::sexp-start-before b 5))
           (is = 13 (tuatara.defaults::sexp-end-after b 10))
           (is = 10 (tuatara.defaults::sexp-start-before b 13))
           (tuatara.buffer:buffer-insert b 0 (make-string 5000 :initial-element #\x))
           (let ((tuatara.defaults::*text-scan-limit* 100))
             (fail (tuatara.defaults::sexp-end-after b 0))
             (fail (tuatara.defaults::sexp-start-before b 5000))
             ;; the text's own end or start within it is found
             (is = 5013 (tuatara.defaults::sexp-end-after b 5010))
             (is = 0 (tuatara.defaults::sexp-start-before b 50))))
      (tuatara.buffer:kill-buffer b))))

(define-test (lists line-openers-sought-in-one-block)
  ;; looking for a line's opening bracket through text without one reads it into one block of
  ;; its own, not a new one for each stretch: about as much made for 4 MB of text as for 1 MB
  (flet ((made (megabytes)
           (let* ((line (format nil "plain words on a line~%"))
                  (rope (tuatara.text:string->rope
                         (with-output-to-string (o) (dotimes (i (floor (* megabytes 1024 1024) (length line))) (write-string line o)))))
                  (size (tuatara.text:rope-bytes rope)))
             (is eq nil (tuatara.defaults::line-opener-after rope 0))
             (is eq nil (tuatara.defaults::line-opener-before rope size))
             (+ (least-bytes (lambda () (tuatara.defaults::line-opener-after rope 0)) 3)
                (least-bytes (lambda () (tuatara.defaults::line-opener-before rope size :block (load-time-value (make-array 65537 :element-type '(unsigned-byte 8))))) 2)))))
    (let ((small (made 1)) (large (made 4)))
      (true (< (- large small) 150000) "~:d octets made for 1 MB, ~:d for 4 MB" small large)
      (true (< large 200000) "~:d octets made for 4 MB" large))))

(define-test (lists line-openers-found-alike-both-ways)
  ;; the opening brackets at a line's start, looked for back and on, as the text has them
  (let ((state (sb-ext:seed-random-state 7)))
    (dotimes (round 40)
      (let* ((chars (format nil "ab ([{~%~%"))
             (text (coerce (loop repeat (random 300 state) collect (char chars (random (length chars) state))) 'string))
             (rope (tuatara.text:string->rope text))
             (starts (loop for j below (length text)
                           when (and (find (char text j) "([{") (or (zerop j) (char= (char text (1- j)) #\Newline)))
                             collect j))
             (block (make-array 65537 :element-type '(unsigned-byte 8))))
        (loop for position from 0 to (length text)
              do (is eql (find-if (lambda (j) (< j position)) starts :from-end t)
                     (tuatara.defaults::line-opener-before rope position) "before ~d of ~s" position text)
                 (is eql (find-if (lambda (j) (< j position)) starts :from-end t)
                     (tuatara.defaults::line-opener-before rope position :block block) "into a block, before ~d of ~s" position text)
                 (when (< position (length text))
                   (is eql (find-if (lambda (j) (> j position)) starts)
                       (tuatara.defaults::line-opener-after rope position) "after ~d of ~s" position text))))))
  ;; within a reach, said to be far when the text goes on past it
  (let ((rope (tuatara.text:string->rope (format nil "(a~%~a~%(b~%" (make-string 100 :initial-element #\x)))))
    (is equal '(nil :far) (multiple-value-list (tuatara.defaults::line-opener-before rope 104 :reach 50)))
    (is equal '(nil :far) (multiple-value-list (tuatara.defaults::line-opener-after rope 1 :reach 50)))
    (is eql 104 (tuatara.defaults::line-opener-after rope 1 :reach 200))))
