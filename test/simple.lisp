;;;; test/simple.lisp --- Emacs's simple.el commands, case by case as Emacs does them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test simple
  :description "Basic editing commands, checked case by case against the results expected: RET and C-o
with counts, M-^ joining lines (with C-u, a selection, brackets and Lisp's quote marks), M-\\ and M-SPC
and the spaces about point, M-m, C-x C-o and the blank lines, delete-trailing-whitespace (form feeds
kept, the blank lines at the end, a selection).")

(defparameter *editing-cases*
  '(
    (:lisp-mode "(a b<POINT> c d)" "M-x n e w l i n e RET" "(a b
   <POINT>c d)")
    (:lisp-mode "(a b<POINT> c d)" "C-u 5 M-x n e w l i n e RET" "(a b




   <POINT>c d)")
    (:lisp-mode "(a b <POINT>c d)" "M-x n e w l i n e RET" "(a b
   <POINT>c d)")
    (:text-mode "some text   <POINT>" "M-x n e w l i n e RET" "some text
<POINT>")
    (:lisp-mode "(a b<POINT> c d)" "C-o" "(a b<POINT>
 c d)")
    (:lisp-mode "(a b<POINT> c d)" "C-u 5 C-o" "(a b<POINT>




 c d)")
    (:lisp-mode "(a b <POINT>c d)" "C-o" "(a b <POINT>
c d)")
    (nil " first 
 second 
 third 
 fourth <POINT>" "M-^" " first 
 second 
 third<POINT> fourth ")
    (nil " first 
 second 
 third<POINT> fourth " "M-^" " first 
 second<POINT> third fourth ")
    (nil "<POINT> first 
 second third fourth " "C-u M-^" " first<POINT> second third fourth ")
    (nil "<MARK> first 
 second 
 third <POINT>" "M-^" " first<POINT> second third ")
    (nil "

 third 
 
 
 sixth 

<POINT>" "M-^" "

 third 
 
 
 sixth 
<POINT>")
    (nil "

 third 
 
 
 sixth 
<POINT>" "M-^" "

 third 
 
 
 sixth<POINT>")
    (nil "

 third 
 
 
 sixth<POINT>" "M-^" "

 third 
 
<POINT>sixth")
    (nil "<POINT>

 third 
 
sixth" "C-u M-^" "<POINT>
 third 
 
sixth")
    (nil "<POINT>
 third 
 
sixth" "C-u M-^" "<POINT>third 
 
sixth")
    (nil "<POINT>third 
 
sixth" "C-u M-^" "third<POINT>
sixth")
    (nil "third<POINT>
sixth" "C-u M-^" "third<POINT> sixth")
    (nil " first 
 second 
 third <POINT>" "C-u M-^" " first 
 second 
 third <POINT>")
    (nil " first <POINT>
 second 
 third " "M-^" "<POINT> first 
 second 
 third ")
    (nil " first<MARK> 
<POINT> second 
 third 
 fourth " "M-^" " first<POINT> second 
 third 
 fourth ")
    (:lisp-mode "(foo
   <POINT>bar)" "M-^" "(foo<POINT> bar)")
    (:lisp-mode "(foo
  <POINT>)" "M-^" "(foo<POINT>)")
    (:lisp-mode "'
  <POINT>(a)" "M-^" "'<POINT>(a)")
    (:lisp-mode "(
  <POINT>a)" "M-^" "(<POINT>a)")
    (nil "a  <POINT>  b" "M-\\" "a<POINT>b")
    (nil "a  <POINT>  b" "C-u M-\\" "a<POINT>  b")
    (nil "a  <POINT>  b" "M-x j u s t - o n e - s p a c e RET" "a <POINT>b")
    (nil "a  <POINT>  b" "C-u 3 M-x j u s t - o n e - s p a c e RET" "a   <POINT>b")
    (nil "a 
 <POINT> 
 b" "C-u - 1 M-x j u s t - o n e - s p a c e RET" "a <POINT>b")
    (nil "a  <POINT>  b" "M-SPC" "a <POINT>b")
    (nil "a  <POINT>  b" "M-SPC M-SPC" "a<POINT>b")
    (nil "a  <POINT>  b" "M-SPC M-SPC M-SPC" "a  <POINT>  b")
    (nil "   foo<POINT>" "M-m" "   <POINT>foo")
    (nil "  	 <POINT>" "M-m" "  	 <POINT>")
    (nil "a


<POINT>

b" "C-x C-o" "a
<POINT>
b")
    (nil "a
<POINT>
b" "C-x C-o" "a
<POINT>b")
    (nil "a<POINT>


b" "C-x C-o" "a<POINT>
b")
    (nil "a

<POINT>
" "C-x C-o" "a
<POINT>")
    (nil "<POINT>


b" "C-x C-o" "<POINT>
b")
    (nil "a
  
<POINT> 	 
  
b" "C-x C-o" "a
<POINT> 	 
b")
    (nil " <FF> 
 <FF> <FF> 

last
<POINT>" "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET" " <FF>
 <FF> <FF>

last
<POINT>")
    (nil "a  
b	


<POINT>" "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET" "a
b
<POINT>")
    (nil "a  
<MARK>b  
c  <POINT>
d  

" "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET" "a  
b
c<POINT>
d  

")
    )
  "Each case: a major mode or NIL, a text, the keys typed on it and the text expected after them.")

(defun point-and-mark-text (text)
  "TEXT with <POINT>, <MARK> and <FF> (a form feed) taken out, and the offsets
where <POINT> and <MARK> were, as three values; NIL for one not there."
  (let* ((text (cl-ppcre:regex-replace-all "<FF>" text (string (code-char 12))))
         (point (search "<POINT>" text))
         (text (if point (concatenate 'string (subseq text 0 point) (subseq text (+ point 7))) text))
         (mark (search "<MARK>" text))
         (text (if mark (concatenate 'string (subseq text 0 mark) (subseq text (+ mark 6))) text)))
    (when (and point mark (< mark point)) (decf point 6))
    ;; the buffer counts octets
    (flet ((octets (end) (length (tuatara.text:string-octets (subseq text 0 end)))))
      (values text (if point (octets point) 0) (and mark (octets mark))))))

(defun text-with-marks (buffer)
  "BUFFER's text with <POINT> where point is, and <MARK> where the mark is
while the selection is active; a form feed as <FF>."
  (let* ((text (tuatara.buffer:buffer-string buffer))
         (point (length (tuatara.buffer:buffer-string buffer :end (tuatara.buffer:buffer-point buffer))))
         (mark (and (tuatara.buffer:buffer-mark-active-p buffer)
                    (length (tuatara.buffer:buffer-string buffer :end (tuatara.buffer:buffer-mark buffer)))))
         (places (sort (remove nil (list (cons point "<POINT>") (and mark (cons mark "<MARK>")))) #'> :key #'car)))
    (dolist (place places)
      (setf text (concatenate 'string (subseq text 0 (car place)) (cdr place) (subseq text (car place)))))
    (cl-ppcre:regex-replace-all (string (code-char 12)) text "<FF>")))

(define-test (simple editing-cases)
  (loop for (mode text keys expected) in *editing-cases*
        do (multiple-value-bind (plain point mark) (point-and-mark-text text)
             (multiple-value-bind (f editor) (session-in mode plain keys :point point :mark mark :width 100)
               (declare (ignore f))
               (is string= expected (text-with-marks (session-buffer editor))
                   "~s then ~a~@[ in ~(~a~)~]" text keys mode)))))

(define-test (simple case-change-of-another-length)
  ;; a letter whose other case takes more or fewer octets: point ends after the word changed
  (loop for (text keys expected) in (list (list (format nil "<POINT>~cx yz" (code-char #x2C65)) "M-u" (format nil "~cX<POINT> yz" (code-char #x23A)))
                                          (list (format nil "<POINT>~cbc then" (code-char #x250)) "M-u" (format nil "~cBC<POINT> then" (code-char #x2C6F)))
                                          (list (format nil "<POINT>~cbc then" (code-char #x23A)) "M-l" (format nil "~cbc<POINT> then" (code-char #x2C65)))
                                          (list (format nil "<POINT>~cbc then" (code-char #x2C65)) "M-c M-c" (format nil "~cbc Then<POINT>" (code-char #x23A))))
        do (multiple-value-bind (plain point mark) (point-and-mark-text text)
             (let ((editor (nth-value 1 (session-in nil plain keys :point point :mark mark :width 100))))
               (is string= expected (text-with-marks (session-buffer editor)) "~s then ~a" text keys)))))

(define-test (simple newline-count-zero-inserts-none)
  ;; newline with a count of 0 inserts no line end and leaves the blanks before point, as open-line does
  (loop for keys in '("C-u 0 M-x n e w l i n e RET" "C-u 0 M-x o p e n - l i n e RET")
        do (let ((editor (nth-value 1 (session-in 'tuatara.defaults::lisp-mode "(a  b)" keys :point 3 :width 100))))
             (is string= "(a <POINT> b)" (text-with-marks (session-buffer editor)) "~a" keys))))

(defparameter *paragraph-cases*
  '(("o<POINT>ne two.~%three~%~%  four five~%six~%~%~%seven~%" "M-}" "one two.~%three~%<POINT>~%  four five~%six~%~%~%seven~%")
    ("o<POINT>ne two.~%three~%~%  four five~%six~%~%~%seven~%" "M-} M-}" "one two.~%three~%~%  four five~%six~%<POINT>~%~%seven~%")
    ("o<POINT>ne two.~%three~%~%  four five~%six~%~%~%seven~%" "M-} M-} M-}" "one two.~%three~%~%  four five~%six~%~%~%seven~%<POINT>")
    ("one two.~%three~%~%  four five~%six~%~%~%sev<POINT>en~%" "M-{" "one two.~%three~%~%  four five~%six~%~%<POINT>~%seven~%")
    ("one two.~%three~%~%  four five~%six~%~%~%sev<POINT>en~%" "M-{ M-{" "one two.~%three~%<POINT>~%  four five~%six~%~%~%seven~%")
    ("one two.~%three~%~%  four five~%six~%~%~%sev<POINT>en~%" "M-{ M-{ M-{" "<POINT>one two.~%three~%~%  four five~%six~%~%~%seven~%")
    ("one two.~%three~%~%  four five~%six~%~%~%sev<POINT>en~%" "C-u - 1 M-}" "one two.~%three~%~%  four five~%six~%~%<POINT>~%seven~%")
    ("One t<POINT>wo. Three four! Five?~%Six seven.~%~%Eight." "M-e" "One two.<POINT> Three four! Five?~%Six seven.~%~%Eight.")
    ("One t<POINT>wo. Three four! Five?~%Six seven.~%~%Eight." "M-e M-e M-e" "One two. Three four! Five?<POINT>~%Six seven.~%~%Eight.")
    ("One t<POINT>wo. Three four! Five?~%Six seven.~%~%Eight." "M-e M-e M-e M-e" "One two. Three four! Five?~%Six seven.<POINT>~%~%Eight.")
    ("One two. Three fo<POINT>ur! Five?" "M-a" "One two. <POINT>Three four! Five?")
    ("One two. Three fo<POINT>ur! Five?" "M-a M-a" "<POINT>One two. Three four! Five?")
    ("One two.~%~%Three f<POINT>our." "M-a M-a" "<POINT>One two.~%~%Three four.")
    ("He said \"Yes.\" Then l<POINT>eft. Done." "M-a M-e M-e" "He said \"Yes.\" Then left. Done.<POINT>")
    ("one tw<POINT>o three four" "M-@" "one tw<POINT>o<MARK> three four")
    ("one tw<POINT>o three four" "M-@ M-@" "one tw<POINT>o three<MARK> four")
    ("one tw<POINT>o three four" "C-u 3 M-@" "one tw<POINT>o three four<MARK>")
    ("a~%b~%~%c<POINT> d~%e~%~%f~%" "M-h" "a~%b~%<POINT>~%c d~%e~%<MARK>~%f~%")
    ("a~%b~%~%c<POINT> d~%e~%~%f~%g~%~%h~%" "M-h M-h" "a~%b~%<POINT>~%c d~%e~%~%f~%g~%<MARK>~%h~%")
    ("c<POINT> d~%e~%~%f~%" "M-h" "<POINT>c d~%e~%<MARK>~%f~%")
    ("One two. Th<POINT>ree four. Five six." "M-x m a r k - e n d - o f - s e n t e n c e RET" "One two. Th<POINT>ree four.<MARK> Five six.")
    ("One two. Th<POINT>ree four. Five six. Seven eight." "C-u 2 M-x m a r k - e n d - o f - s e n t e n c e RET" "One two. Th<POINT>ree four. Five six.<MARK> Seven eight."))
  "Paragraph and sentence motions and the mark commands, each as (TEXT KEYS
EXPECTED), ~% a newline; what Emacs 30 does with sentence-end-double-space NIL.")

(define-test (simple paragraphs-and-sentences)
  (loop for (text keys expected) in *paragraph-cases*
        do (multiple-value-bind (plain point mark) (point-and-mark-text (format nil text))
             (multiple-value-bind (f editor) (session-in :text-mode plain keys :point point :mark mark :width 100)
               (declare (ignore f))
               (is string= (format nil expected) (text-with-marks (session-buffer editor))
                   "~s then ~a" text keys)))))

(define-test (simple beyond-the-cases)
  ;; RET: the trailing spaces go with the new line end as one step, so C-/ takes both back
  (multiple-value-bind (f editor) (session-in :text-mode "some text   " "M-x n e w l i n e RET C-/" :point 12)
    (declare (ignore f))
    (is string= "some text   " (session-text editor)))
  ;; inside a string the spaces stay
  (multiple-value-bind (f editor) (session-in :lisp-mode "(a \"b   c\")" "M-x n e w l i n e RET" :point 8)
    (declare (ignore f))
    (is string= (format nil "(a \"b   ~%c\")") (session-text editor)))
  ;; a negative count is refused
  (dolist (keys '("C-u - 1 C-o" "C-u - 1 M-x n e w l i n e RET"))
    (multiple-value-bind (f editor) (session-in nil "ab" keys :point 1)
      (is string= "ab" (session-text editor))
      (is string= "Repetition argument has to be non-negative" (echo-text f))))
  ;; what delete-trailing-whitespace did is said
  (flet ((said (text)
           (echo-text (session-in nil text "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET" :width 100))))
    (is string= "Deleted trailing whitespace on 2 lines, and 2 blank lines at the end"
        (said (format nil "a  ~%b ~%c~%~%~%")))
    (is string= "Deleted 1 blank line at the end" (said (format nil "a~%~%")))
    (is string= "No trailing whitespace" (said (format nil "a~%b"))))
  ;; the line ends of a Windows file stay whole
  (with-test-directory (dir)
    (let ((path (merge-pathnames "crlf.txt" dir)))
      (put-file path (format nil "a  ~C~%b~C~C~%~C~%" #\Return #\Tab #\Return #\Return))
      (multiple-value-bind (f editor)
          (run-keys "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET" :file (namestring path) :width 100)
        (declare (ignore f))
        (is string= (format nil "a~C~%b~C~%" #\Return #\Return) (session-text editor)))))
  ;; a large buffer is done in the background, with the same result
  (let ((tuatara.defaults::*rewrite-threshold* 0)
        (tuatara.defaults::*rewrite-piece* 40)
        (tuatara.defaults::*rewrite-checkpoint* 16)
        (text (with-output-to-string (o) (dotimes (i 30) (format o "line ~d   ~%" i)))))
    (multiple-value-bind (f editor)
        (run-keys "M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET" :text text :width 120)
      (true (tuatara.frontend.fake:fake-frame-showing f "in the background"))
      (is string= (cl-ppcre:regex-replace-all " +\\n" text (string #\Newline)) (session-text editor)))))

(defparameter *transpose-cases*
  '(
    (nil "ab<POINT>cd" "C-t" "acb<POINT>d")
    (nil "abc<POINT>" "C-t" "acb<POINT>")
    (nil "abc<POINT>
def" "C-t" "acb<POINT>
def")
    (nil "<POINT>abc" "C-t" "ERROR Beginning of buffer")
    (nil "a<POINT>bcd" "C-u 2 C-t" "bca<POINT>d")
    (nil "abc<POINT>d" "C-u - 1 C-t" "ac<POINT>bd")
    (nil "a<MARK>bcd<POINT>e" "C-u 0 C-t" "a<POINT>ecd<MARK>b")
    (nil "aé<POINT>b" "C-t" "abé<POINT>")
    (nil "one <POINT>two three" "M-t" "two one<POINT> three")
    (nil "one two<POINT> three" "M-t" "one three two<POINT>")
    (nil "one tw<POINT>o three" "M-t" "one three two<POINT>")
    (nil "one two three<POINT>" "M-t" "ERROR Don't have two things to transpose")
    (nil "<POINT>one two three" "M-t" "two one<POINT> three")
    (nil "one <POINT>two three four" "C-u 2 M-t" "two three one<POINT> four")
    (nil "one two three <POINT>four" "C-u - 2 M-t" "three<POINT> one two four")
    (nil "one, <POINT>two. three" "M-t" "two, one<POINT>. three")
    (nil "a
b<POINT>
c" "C-x C-t" "b
a
<POINT>c")
    (nil "a<POINT>
b
c" "C-x C-t" "b
a
<POINT>c")
    (nil "a
b
c<POINT>" "C-x C-t" "a
c
b
<POINT>")
    (nil "a
b<POINT>
c
d" "C-u 2 C-x C-t" "b
c
a
<POINT>d")
    (nil "a
b
c
d<POINT>
e" "C-u - 2 C-x C-t" "c
<POINT>a
b
d
e")
    (nil "<MARK>a
b
c<POINT>
d" "C-u 0 C-x C-t" "<POINT>c
b
a
<MARK>d")
    (:lisp-mode "(s1) (s2) (s3) (s4) <POINT>(s5)" "C-u - 1 C-M-t" "(s1) (s2) (s4)<POINT> (s3) (s5)")
    (:lisp-mode "(s1) (s2) (s3) (s4) <POINT>(s5)" "C-u - 2 C-M-t" "(s1) (s4)<POINT> (s2) (s3) (s5)")
    (:lisp-mode "(foo bar<POINT> baz)" "C-M-t" "(foo baz bar<POINT>)")
    (:lisp-mode "(foo bar <POINT>baz)" "C-M-t" "(foo baz bar<POINT>)")
    (:lisp-mode "(foo (a b)<POINT> \"c d\")" "C-M-t" "(foo \"c d\" (a b)<POINT>)")
    (:lisp-mode "(foo b<POINT>ar baz)" "C-M-t" "(foo baz bar<POINT>)")
    (:lisp-mode "(foo bar baz qux<POINT>)" "C-u 2 C-M-t" "ERROR Not between two complete sexps")
    )
  "C-t, M-t, C-x C-t and C-M-t on each text, with the results expected; \"ERROR ...\" for what
is refused, the text left as it was.")

(define-test (simple transposing-cases)
  (loop for (mode text keys expected) in *transpose-cases*
        do (multiple-value-bind (plain point mark) (point-and-mark-text text)
             (multiple-value-bind (f editor) (session-in mode plain keys :point point :mark mark :width 100)
               (if (uiop:string-prefix-p "ERROR " expected)
                   (progn (is string= (subseq expected 6) (echo-text f) "~s then ~a" text keys)
                          (is string= plain (session-text editor) "~s then ~a" text keys))
                   (is string= expected (text-with-marks (session-buffer editor)) "~s then ~a~@[ in ~(~a~)~]" text keys mode))))))

(defparameter *kill-cases*
  '(
    (nil "-2
-1
A<POINT>B
1
2
" "C-u 0 C-S-<backspace>" "-2
-1
<POINT>
1
2
" "AB")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u 1 C-S-<backspace>" "-2
-1
<POINT>1
2
" "AB
")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u 2 C-S-<backspace>" "-2
-1
<POINT>2
" "AB
1
")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u 3 C-S-<backspace>" "-2
-1
<POINT>" "AB
1
2
")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u 9 C-S-<backspace>" "-2
-1
<POINT>" "AB
1
2
")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u - 1 C-S-<backspace>" "-2
-1<POINT>
1
2
" "
AB")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u - 2 C-S-<backspace>" "-2<POINT>
1
2
" "
-1
AB")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u - 3 C-S-<backspace>" "<POINT>
1
2
" "-2
-1
AB")
    (nil "-2
-1
A<POINT>B
1
2
" "C-u - 9 C-S-<backspace>" "<POINT>
1
2
" "-2
-1
AB")
    (nil "<POINT>" "C-u - 1 C-S-<backspace>" "ERROR Beginning of buffer" nil)
    (nil "<POINT>" "C-u 1 C-S-<backspace>" "ERROR End of buffer" nil)
    (nil "-1
<POINT>" "C-u 1 C-S-<backspace>" "ERROR End of buffer" nil)
    (nil "-1
A<POINT>" "C-u 1 C-S-<backspace>" "-1
<POINT>" "A")
    (nil "<POINT>
1" "C-u - 1 C-S-<backspace>" "ERROR Beginning of buffer" nil)
    (nil "<POINT>A
1" "C-u 1 C-S-<backspace>" "<POINT>1" "A
")
    (nil "-1
<POINT>
1
" "C-u 1 C-S-<backspace>" "-1
<POINT>1
" "
")
    (nil "-1
<POINT>
1
" "C-u - 1 C-S-<backspace>" "-1<POINT>
1
" "
")
    (nil "-1
A<POINT>
1
" "C-u 1 C-S-<backspace>" "-1
<POINT>1
" "A
")
    (nil "-1
A<POINT>
1
" "C-u - 1 C-S-<backspace>" "-1<POINT>
1
" "
A")
    (nil "-1
<POINT>A
1
" "C-u 1 C-S-<backspace>" "-1
<POINT>1
" "A
")
    (nil "-1
<POINT>A
1
" "C-u - 1 C-S-<backspace>" "-1<POINT>
1
" "
A")
    (nil "one
two<POINT>
three
" "C-S-<backspace> C-S-<backspace>" "one
<POINT>" "two
three
")
    (nil "A<POINT>X<MARK>B" "C-w C-S-<backspace>" "<POINT>" "AXB")
    (nil "<POINT>abcde" "M-z c" "<POINT>de" "abc")
    (nil "<POINT>abcde abc123" "C-u 2 M-z c" "<POINT>123" "abcde abc")
    (nil "<POINT>abcdeCXYZ" "M-z C" "<POINT>XYZ" "abcdeC")
    (nil "<POINT>abcdeCXYZ" "M-z c" "<POINT>deCXYZ" "abc")
    (nil "abc<POINT>de" "C-u - 1 M-z a" "<POINT>de" "abc")
    (nil "<POINT>abcde" "M-z z" "ERROR Search failed: \"z\"" nil)
    (nil "<POINT>abcde" "M-z C-b" "ERROR C-b is no character to zap to" nil)
    (nil "<POINT>héllo world" "M-z o" "<POINT> world" "héllo")
    (nil "<POINT>abcde" "M-x z a p - u p - t o - c h a r RET c" "<POINT>cde" "ab")
    (nil "<POINT>abcde abc123" "C-u 2 M-x z a p - u p - t o - c h a r RET c" "<POINT>c123" "abcde ab")
    (nil "abc<POINT>de" "C-u - 1 M-x z a p - u p - t o - c h a r RET a" "a<POINT>de" "bc")
    )
  "C-S-<backspace> (kill-whole-line), M-z (zap-to-char) and zap-up-to-char on each text, with the
results expected and what the kill ring then holds first; \"ERROR ...\" for what is refused.")

(define-test (simple killing-cases)
  (loop for (mode text keys expected kill) in *kill-cases*
        do (multiple-value-bind (plain point mark) (point-and-mark-text text)
             (multiple-value-bind (f editor) (session-in mode plain keys :point point :mark mark :width 100)
               (if (uiop:string-prefix-p "ERROR " expected)
                   (progn (is string= (subseq expected 6) (echo-text f) "~s then ~a" text keys)
                          (is string= plain (session-text editor) "~s then ~a" text keys))
                   (progn (is string= expected (text-with-marks (session-buffer editor)) "~s then ~a" text keys)
                          (is equal kill (first (tuatara.command:editor-kill-ring editor)) "~s then ~a: the kill" text keys)))))))

(define-test (simple counting-words)
  ;; lines, sentences, words and characters, sentences two spaces apart (and one space
  ;; apart, below); blank lines at the end add no sentence
  (loop for (text . counts) in '(("Hello there, world. This is it.
Second line here" 2 2 9 48)
                                 ("One.  Two.  Three." 1 3 3 18)
                                 ("One. Two. Three." 1 1 3 16)
                                 ("A sentence.

A paragraph
of two lines

And a third." 6 3 10 51)
                                 ("\"Quoted.\"  Next one!  Really?  Yes" 1 4 5 34)
                                 ("héllo wörld, ça va? oui.  fin" 1 2 6 29)
                                 ("12.5 is a number.  x_y and don't" 1 2 10 32)
                                 ("no end at all" 1 1 4 13)
                                 ("end with newline.
" 1 1 3 18)
                                 ("Wait… what‽  Go." 1 2 3 16)
                                 ("" 0 0 0 0))
        do (let ((rope (tuatara.text:string->rope text)))
             (is equal counts (multiple-value-list (tuatara.defaults::count-text rope 0 (tuatara.text:rope-bytes rope) :double-space t))
                 "~s" text)))
  ;; one space apart, as sentences are by default, a full stop and a blank end a sentence
  (loop for (text . counts) in '(("One. Two. Three." 1 3 3 16) ("One.  Two." 1 2 2 10) ("Dr. Smith. Yes" 1 2 3 14))
        do (let ((rope (tuatara.text:string->rope text)))
             (is equal counts (multiple-value-list (tuatara.defaults::count-text rope 0 (tuatara.text:rope-bytes rope)))
                 "~s" text)))
  ;; M-= says them, of the selection or the whole buffer
  (is string= "Buffer has 2 lines, 3 sentences, 9 words, and 48 characters"
      (echo-text (session-in nil (format nil "Hello there, world. This is it.~%Second line here") "M-=" :width 100)))
  (is string= "Region has 1 line, 1 sentence, 2 words, and 12 characters"
      (echo-text (session-in nil (format nil "Hello there, world. This is it.~%Second line here") "M-=" :point 0 :mark 12 :width 100)))
  ;; a count that takes a while goes on in the background, said when it ends; C-x ... stops one
  (let ((tuatara.defaults::*count-wait* 0) (tuatara.defaults::*count-piece* 8)
        (text (with-output-to-string (o) (dotimes (i 200) (format o "Word ~d here.  " i)))))
    (multiple-value-bind (f editor) (session-in nil text "M-=" :width 120)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Counting the buffer in the background"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Buffer has 1 line, 200 sentences, 600 words, and 3,090 characters")))))

(define-test (simple counting-words-past-bad-octets-and-abbreviations)
  ;; an octet that is no part of a UTF-8 character is one character, as it is shown; an
  ;; abbreviation's full stop ends no sentence, as in the sentence commands and filling
  (loop for (octets . counts) in '((#(99 97 102 #xE9 32 97 117) 1 1 2 7)
                                   (#(#xE9 #xE9 32 #xC3 #xA9) 1 1 1 4)
                                   (#(#xA9 120 #xC3) 1 1 1 3)
                                   (#(120 #xF0 #x9F 32 #xFF) 1 1 1 5))
        do (let ((rope (tuatara.text:octets->rope (coerce octets '(vector (unsigned-byte 8))))))
             (is equal counts (multiple-value-list (tuatara.defaults::count-text rope 0 (tuatara.text:rope-bytes rope)))
                 "~s" octets)))
  (loop for (text . counts) in '(("Dr.  Smith went home.  He slept." 1 2 6 32)
                                 ("I met Mr.  Rotorua, e.g.  at noon.  Then." 1 2 9 41)
                                 ("End.  Dr.  Ann.  Taranaki." 1 3 4 26))
        do (let ((rope (tuatara.text:string->rope text)))
             (is equal counts (multiple-value-list (tuatara.defaults::count-text rope 0 (tuatara.text:rope-bytes rope) :double-space t))
                 "~s" text))))

(define-test (simple counted-alike-in-any-pieces)
  ;; a character cut between the pieces the text is read in counts as one, however small the pieces
  (let* ((octets (coerce (append (coerce (tuatara.text:string-octets "Kia ora tūī ✓ 😀. Ōtaki!  ") 'list)
                                 '(#xE9 32 #xF0 #x9F 32 #xC3 #xA9 #xFF 10 #xE2 #x82))
                         '(vector (unsigned-byte 8))))
         (rope (tuatara.text:octets->rope octets))
         (whole (multiple-value-list (tuatara.defaults::count-text rope 0 (length octets)))))
    (dotimes (size 9)
      (let ((tuatara.defaults::*count-piece* (1+ size)))
        (is equal whole (multiple-value-list (tuatara.defaults::count-text rope 0 (length octets)))
            "pieces of ~d" (1+ size))))))

(defun large-text (turns text &key mode (width 100))
  "The text after TURNS typed on TEXT, each once the work in the background the
one before began is done, with any selection too large for the editor's
thread (*rewrite-threshold* 0)."
  (with-background ()
    (session-text (nth-value 1 (run-keys-in-turns (append turns '("")) :text text :width width :height 10 :mode mode)))))

(defun small-text (turns text &key mode (width 100))
  "The text after TURNS typed on TEXT, each once the work in the background the one before began is done."
  (session-text (nth-value 1 (run-keys-in-turns (append turns '("")) :text text :width width :height 10 :mode mode))))

(define-test (simple large-selections)
  ;; a selection too large to work on at once is changed in the background, as the small one is at once
  (flet ((both (keys text &optional mode)
           (let ((small (small-text (list keys) text :mode mode))
                 (large (large-text (list keys) text :mode mode)))
             (is string= small large "~a on ~s" keys text)
             large)))
    ;; the case of the region, a word at a time
    (is string= "HELLO WORLD_WIDE" (both "% M-x u p c a s e - r e g i o n RET" "hello world_wide"))
    (is string= "hello world_wide" (both "% M-x d o w n c a s e - r e g i o n RET" "HELLO WORLD_WIDE"))
    (is string= "Hello World_Wide" (both "% M-x c a p i t a l i z e - r e g i o n RET" "hello WORLD_wide"))
    ;; joining lines: one space, or none by brackets
    (is string= (format nil "a b (c)d") (both "V j j j g j" (format nil "a~%  b~%(c~%)d")))
    (is string= (format nil "a b~%c") (both "V j g j" (format nil "a  ~%  b~%c")))
    (is string= "a b c" (both "% M-x d e l e t e - i n d e n t a t i o n RET" (format nil "a~%  b~%c")))
    ;; commenting lines, and uncommenting them, and one undo
    (let ((mode 'tuatara.defaults::lisp-mode))
      (is string= (format nil ";; (foo)~%;;   (bar)~%") (both "V j C" (format nil "(foo)~%  (bar)~%") mode))
      (is string= (format nil "(foo)~%  (bar)~%") (both "V j C" (format nil ";; (foo)~%;;   (bar)~%") mode))
      (is string= (format nil "  ;; (foo)~%~%  ;;   (bar)~%") (both "V j j C" (format nil "  (foo)~%~%    (bar)~%") mode))
      (is string= (format nil ";; ;; a~%;; (b)~%") (both "V j C" (format nil ";; a~%(b)~%") mode))
      (is string= (format nil "(foo)~%  (bar)~%")
          (large-text '("V j C" "u") (format nil "(foo)~%  (bar)~%") :mode mode))))
  ;; and it goes there, saying so
  (with-background ()
    (flet ((shows (keys text words &optional mode)
             (true (tuatara.frontend.fake:fake-frame-showing
                    (run-keys-in-turns (list keys "") :text text :width 150 :height 10 :mode mode) words)
                   "~a" keys)))
      (shows "% M-x u p c a s e - r e g i o n RET" "hello" "Changing the case of the region in the background")
      (shows "V j g j" (format nil "a~%b") "Joining the lines in the background")
      (shows "V j C" (format nil "(a)~%(b)") "Commenting or uncommenting the lines in the background"
             'tuatara.defaults::lisp-mode)))
  ;; what has no way to the background says so and changes nothing
  (with-background ()
    (flet ((says (keys text what &optional mode)
             (let ((f (run-keys-in-turns (list keys "") :text text :width 120 :height 10 :mode mode)))
               (true (tuatara.frontend.fake:fake-frame-showing f (format nil "~a works on at most 0 B at once" what)) "~a" keys))))
      (says "% >" (format nil "(foo~%bar)") "Indenting" 'tuatara.defaults::lisp-mode)
      (says "% g q" (format nil "some words~%and more") "Filling")
      (says "% M-x e v a l - r e g i o n RET" "(+ 1 2)" "Evaluating" 'tuatara.defaults::lisp-mode)))
  ;; transposed far apart: the texts moved as they are, the same result
  (let ((plain (small-text '("w M-t") "hello world")))
    (is string= "world hello" plain)
    (let ((tuatara.buffer:*large-edit-octets* 0))
      (is string= plain (small-text '("w M-t") "hello world"))))
  ;; a paste repeated past what is kept as a string is made of the kill, not copied: the same text
  (let ((plain (small-text '("v l y C-u 3 p") "abc")))
    (let ((tuatara.defaults::*clipboard-max-size* 2))
      (is string= plain (small-text '("v l y C-u 3 p") "abc"))))
  ;; pasting at every cursor more than is written at once is refused
  (with-background ()
    (true (tuatara.frontend.fake:fake-frame-showing
           (run-keys-in-turns '("v l y t t C-u 3 p" "") :text (format nil "abc~%abc~%abc") :width 150 :height 10)
           "more than is written at once")))
  ;; banked lines of any size: copied as one kill, deleted and pasted over as one change
  (let* ((five (format nil "a~%b~%c~%d~%e"))
         (deleted (small-text '("m l j j m l d") five))
         (pasted (small-text '("v l y" "j m l j j m l p") five)))
    (let ((tuatara.defaults::*clipboard-max-size* 1)
          (tuatara.buffer:*large-edit-octets* 1))
      (multiple-value-bind (f editor) (run-keys "m l j j m l y" :text five)
        (declare (ignore f))
        (is string= (format nil "a~%c~%") (tuatara.text:rope-string (first (tuatara.command:editor-kill-ring editor)))))
      (is string= deleted (small-text '("m l j j m l d") five))
      (is string= five (small-text '("m l j j m l d" "u") five))
      (is string= pasted (small-text '("v l y" "j m l j j m l p") five))))
  ;; f over many stretches goes to the background however many there are
  (let ((tuatara.defaults::*rewrite-threshold* 10))
    (true (tuatara.defaults::split-in-background-p
           (tuatara.buffer:make-buffer "*stretches*" :text (make-string 200 :initial-element #\a))
           (loop for i below 100 collect (cons (* 2 i) (1+ (* 2 i))))))
    (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "*stretches*"))))

(define-test (simple soft-wrap-mode)
  (let ((tuatara-user:*fill-column* 8)
        (text "aaa bbb ccc ddd eee"))
    (flet ((screen (keys &key (text text) (width 40))
             (let ((f (run-keys keys :text text :width width :height 8)))
               (loop for i below 8 collect (tuatara.frontend.fake:fake-screen-line f i)))))
      ;; M-x soft-wrap-mode wraps at the fill column, at a word's edge, the rows
      ;; that go on marked in the gutter; the mode line says Wrap
      (let ((lines (screen "M-x s o f t - w r a p - m o d e RET")))
        (true (search "aaa bbb" (first lines)))
        (true (search "↪ ccc ddd" (second lines)))
        (true (search "↪ eee" (third lines)))
        (true (some (lambda (line) (search "Wrap" line)) lines)))
      ;; with line numbers the mark stands where a number would
      (let ((lines (screen "M-x s o f t - w r a p - m o d e RET M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET")))
        (true (search "1 aaa bbb" (first lines)))
        (true (search "↪ ccc ddd" (second lines))))
      ;; the lines after a wrapped one keep their numbers
      (let ((lines (screen "SPC t l M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET"
                           :text (format nil "~a~%fff~%" text))))
        (true (search "2 fff" (fourth lines)))
        (true (search "~" (fifth lines))))
      ;; SPC t l does the same; again, and the line is one row as before
      (true (search "↪ ccc ddd" (second (screen "SPC t l"))))
      (false (search "ccc" (second (screen "SPC t l SPC t l"))))
      ;; the row point is on is painted in :soft-wrap-row over the line's :hl-line
      (flet ((faces-of (f row)
               (loop for (nil nil face) in (aref (tuatara.frontend.fake:fake-spans f) row)
                     append (if (listp face) face (list face)))))
        (let* ((tuatara-user:*global-hl-line-mode* t)
               (f (run-keys "SPC t l l j" :text text :width 40 :height 8)))
          (true (member :soft-wrap-row (faces-of f 1)))
          (false (member :soft-wrap-row (faces-of f 0)))
          (true (member :hl-line (faces-of f 0)))))
      ;; j and k go by rows, keeping the column
      (flet ((point-after (keys)
               (session-point (nth-value 1 (run-keys keys :text text :width 40 :height 8)))))
        (is = 9 (point-after "SPC t l l j"))
        (is = 17 (point-after "SPC t l l j j"))
        (is = 1 (point-after "SPC t l l j j k k")))
      ;; the line's start and end keys go by rows, as Emacs's visual-line-mode
      ;; has them; I, A, o and O keep to the whole line
      (flet ((point-after (keys) (session-point (nth-value 1 (run-keys keys :text text :width 40 :height 8))))
             (text-after (keys) (session-text (nth-value 1 (run-keys keys :text text :width 40 :height 8)))))
        (is = 7 (point-after "SPC t l C-e"))
        (is = 7 (point-after "SPC t l g l"))
        (is = 8 (point-after "SPC t l j C-a"))
        (is = 8 (point-after "SPC t l j g h"))
        (is = 15 (point-after "SPC t l j g l"))
        (is = 19 (point-after "SPC t l j j g l"))
        (is = 19 (point-after "C-e"))
        (is string= "aaa bbb ccc ddd eeeX" (text-after "SPC t l l A X C-g"))
        (is string= "Xaaa bbb ccc ddd eee" (text-after "SPC t l j I X C-g"))
        (is string= (format nil "~a~%X" text) (text-after "SPC t l l o X C-g")))
      ;; the page keys and z z go by rows
      (let ((words (format nil "~{w~2,'0d~^ ~}~%end~%" (loop for i from 1 to 40 collect i))))
        (flet ((top (keys) (tuatara.frontend.fake:fake-screen-line (run-keys keys :text words :width 40 :height 8) 0)))
          (true (search "w09 w10" (top "SPC t l C-v")))
          (true (search "w17 w18" (top "SPC t l C-v C-v")))
          (true (search "w01 w02" (top "SPC t l C-v M-v")))
          (true (search "w37 w38" (top "SPC t l g e z z")))))
      ;; the settings: by major mode before the global one
      (let ((tuatara-user:*global-soft-wrap-mode* t))
        (true (search "↪ ccc ddd" (second (screen ""))))
        (let ((tuatara-user:*soft-wrap-modes* '((tuatara.defaults::lisp-interaction-mode . nil)
                                                  (tuatara.defaults::fundamental-mode . nil))))
          (false (search "ccc" (second (screen ""))))))
      ;; at the window's edge rather than the fill column
      (let ((tuatara-user:*soft-wrap-at-fill-column* nil))
        (let ((lines (screen "SPC t l" :width 20)))
          (true (search "aaa bbb ccc ddd" (first lines)))
          (true (search "↪ eee" (second lines))))))))

(define-test (simple soft-wrap-under-an-indented-headline)
  ;; in Org's indented view, j from a headline's first column goes to the text's first character
  ;; under it, never to the indentation shown before it, which stands for no character
  (multiple-value-bind (f editor)
      (run-keys "M-x o r g - m o d e RET M-x s o f t - w r a p - m o d e RET j j j"
                :text (format nil "* Head~%Text under it~%more~%** Sub~%last~%") :width 60 :height 10)
    (declare (ignore f))
    (is = (length (format nil "* Head~%Text under it~%more~%")) (session-point editor))
    (false (search "outside buffer" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))))

(define-test (simple visual-line-mode)
  (let ((tuatara-user:*fill-column* 8)
        (text (format nil "aaa bbb ccc~%x~%")))
    (flet ((ruled (f row)
             ;; the screen columns of ROW in :visual-line, whatever faces are over it
             (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) row)
                   when (member :visual-line (if (listp face) face (list face)))
                     append (loop for c from start below end collect c)))
           (face-at (f row column)
             (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) row)
                   when (and (<= start column) (< column end)) return face)))
      ;; SPC t v marks the 8th column down the window: over the text, past a
      ;; short line's end and below the buffer's; the text neither changes nor wraps
      (multiple-value-bind (f editor) (run-keys "SPC t v" :text text :width 40 :height 8)
        (is equal '(7) (ruled f 0))
        (is equal '(7) (ruled f 1))
        (is equal '(7) (ruled f 4))
        (true (search "aaa bbb ccc" (tuatara.frontend.fake:fake-screen-line f 0)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Visual line on"))
        (is string= text (session-text editor)))
      ;; again, and it is gone; M-x visual-line-mode the same
      (is equal '() (ruled (run-keys "SPC t v SPC t v" :text text :width 40 :height 8) 1))
      (is equal '(7) (ruled (run-keys "M-x v i s u a l - l i n e - m o d e RET" :text text :width 40 :height 8) 1))
      ;; it goes with the fill column
      (is equal '(3) (ruled (run-keys "SPC t v C-u 4 C-x f" :text text :width 40 :height 8) 1))
      ;; under the line point is on, and over every other face
      (let ((tuatara-user:*global-hl-line-mode* t))
        (let ((face (face-at (run-keys "SPC t v" :text text :width 40 :height 8) 0 7)))
          (is eq :hl-line (first face))
          (is eq :visual-line (second face))))
      ;; over a source block's background, in the block's lines and past their ends
      (let ((tuatara-user:*global-hl-line-mode* nil)
            (block (format nil "#+begin_src sh~%echo hello world~%e~%#+end_src~%")))
        (let ((f (run-keys "M-x o r g - m o d e RET SPC t v" :text block :width 40 :height 8)))
          (dolist (row '(1 2))
            (let ((face (face-at f row 7)))
              (true (listp face))
              (is eq :visual-line (first face))
              (true (member :org-block face))))))
      ;; with long lines wrapped at the fill column, beside the rows the gutter
      ;; marks, the rows ending by it
      (let ((f (run-keys "SPC t l SPC t v" :text text :width 40 :height 8)))
        (true (search "↪ ccc" (tuatara.frontend.fake:fake-screen-line f 1)))
        (is equal '(9) (ruled f 0))
        (is equal '(9) (ruled f 1)))
      ;; scrolled sideways, it moves with the text, and is not shown once passed
      (let ((long (format nil "~a~%" (make-string 60 :initial-element #\x)))
            (tuatara-user:*fill-column* 30))
        (flet ((scrolled (keys)
                 (multiple-value-bind (f editor) (run-keys keys :text long :width 20 :height 8)
                   (values (ruled f 0) (tuatara.display:window-hscroll (tuatara.command:editor-window editor))))))
          (multiple-value-bind (columns hscroll) (scrolled "SPC t v C-u 2 5 l")
            (true (< 0 hscroll 30))
            (is equal (list (- 29 hscroll)) columns))
          (multiple-value-bind (columns hscroll) (scrolled "SPC t v g l")
            (true (> hscroll 29))
            (is equal '() columns))))
      ;; the settings: by major mode before the global one
      (let ((tuatara-user:*global-visual-line-mode* t))
        (is equal '(7) (ruled (run-keys "" :text text :width 40 :height 8) 1))
        (let ((tuatara-user:*visual-line-modes* '((tuatara.defaults::lisp-interaction-mode . nil)
                                                  (tuatara.defaults::fundamental-mode . nil))))
          (is equal '() (ruled (run-keys "" :text text :width 40 :height 8) 1))
          ;; a buffer's own choice before both
          (is equal '(7) (ruled (run-keys "SPC t v" :text text :width 40 :height 8) 1)))))))

(defparameter *comment-and-pair-cases*
  `((:lisp-mode "(a<POINT>)
(b)
" "C-x C-;" ";; (a)
<POINT>(b)
")
    (:lisp-mode ";; (a<POINT>)
(b)
" "C-x C-;" "(a)
<POINT>(b)
")
    (:lisp-mode "(x
  (a<POINT>))
" "C-x C-;" "(x
  ;; (a))
<POINT>")
    (:lisp-mode "(a<POINT>)

  (b)
<MARK>(c)
" "C-x C-;" ";; (a<POINT>)

;;   (b)
;; (c)
")
    (:lisp-mode ";; (a<POINT>)
(b)<MARK>
" "C-x C-;" ";; ;; (a<POINT>)
;; (b)
")
    (:bash-mode "echo a<POINT>
" "C-x C-;" "# echo a
<POINT>")
    (:css-mode "a { b: c; }<POINT>
" "C-x C-;" "/* a { b: c; } */
<POINT>")
    (:lisp-mode "<POINT>
(b)
" "M-;" ";; <POINT>
(b)
")
    (:lisp-mode "(a)<POINT> ; note
" "M-;" "(a) ; <POINT>note
")
    (:lisp-mode "(a<POINT>)
" "M-;" ,(format nil "(a)~a; <POINT>~%" (make-string 37 :initial-element #\Space)))
    (:lisp-mode "<MARK>(a)
(b)<POINT>
" "M-;" ";; (a)
;; (b)<POINT>
")
    (nil "a<POINT>b" "M-(" "a (<POINT>) b")
    (nil "x <MARK>word<POINT> y" "M-(" "x (<POINT>word) y")
    (nil "x (<MARK>word<POINT>) y" "M-(" "x <POINT>word y"))
  "Commenting (C-x C-; and M-;) and pairs (M-(), each (MODE TEXT KEYS EXPECTED), as Emacs's
comment-line, comment-dwim and insert-parentheses do them (a comment at a line's end in the
buffer's spaces or tabs, its marker followed by a space; M-( on a region a pair already
stands around takes it off).")

(define-test (simple comments-and-pairs)
  (loop for (mode text keys expected) in *comment-and-pair-cases*
        do (multiple-value-bind (plain point mark) (point-and-mark-text text)
             (multiple-value-bind (f editor) (session-in mode plain keys :point point :mark mark :width 100)
               (declare (ignore f))
               (is string= expected (text-with-marks (session-buffer editor))
                   "~s then ~a~@[ in ~(~a~)~]" text keys mode))))
  ;; electric-pair-mode: an opening delimiter typed closes itself, the closer typed steps over, DEL takes both
  (let ((tuatara.defaults::*electric-pair-mode* nil))
    (multiple-value-bind (f editor) (session-in nil "" "M-x e l e c t r i c - p a i r - m o d e RET i ( a )")
      (true (tuatara.frontend.fake:fake-frame-showing f "Electric pair mode on"))
      (is string= "(a)<POINT>" (text-with-marks (session-buffer editor))))
    (multiple-value-bind (f editor) (session-in nil "" "i ( DEL")
      (declare (ignore f))
      (is string= "<POINT>" (text-with-marks (session-buffer editor)))))
  ;; the name it had as DONKEY's still sets it
  (let ((tuatara.defaults::*electric-pair-mode* nil))
    (setf tuatara-user:*electric-pair-mode* t)
    (true tuatara.defaults::*electric-pair-mode*)))

(define-test (simple cursor-column-mode)
  (let ((text (format nil "aaa bbb ccc~%x~%~%dddd eeee~%")))
    (flet ((marked (f row)
             ;; the screen columns of ROW in :cursor-column, whatever faces are over it
             (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) row)
                   when (member :cursor-column (if (listp face) face (list face)))
                     append (loop for c from start below end collect c)))
           (face-at (f row column)
             (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) row)
                   when (and (<= start column) (< column end)) return face)))
      ;; SPC t | marks the column point is in down the window: over the text, past a short
      ;; line's end and an empty line, below the buffer's end; the text unchanged
      (multiple-value-bind (f editor) (run-keys "SPC t | C-u 5 C-f" :text text :width 40 :height 9)
        (is equal '(5) (marked f 0))
        (is equal '(5) (marked f 1))
        (is equal '(5) (marked f 2))
        (is equal '(5) (marked f 3))
        (is equal '(5) (marked f 6))
        (true (tuatara.frontend.fake:fake-frame-showing f "Cursor column on"))
        (is string= text (session-text editor)))
      ;; it follows point to another line and column
      (is equal '(2) (marked (run-keys "SPC t | C-n C-n C-n C-f C-f" :text text :width 40 :height 9) 0))
      ;; again, and it is gone
      (is equal '() (marked (run-keys "SPC t | SPC t |" :text text :width 40 :height 9) 1))
      ;; under the line point is on, in the same colour by standard, and over every other face
      (let ((tuatara-user:*global-hl-line-mode* t))
        (let ((face (face-at (run-keys "SPC t | C-f C-f" :text text :width 40 :height 9) 0 2)))
          (true (listp face))
          (is eq :hl-line (first face))
          (is eq :cursor-column (second face))))
      (let ((tuatara-user:*global-hl-line-mode* nil))
        (let ((face (face-at (run-keys "C-SPC C-n C-n SPC t |" :text text :width 40 :height 9) 1 0)))
          (true (listp face))
          (is eq :cursor-column (first face))))
      (is eq :hl-line (tuatara.display:face-inherit (tuatara.display:find-face :cursor-column)))
      ;; with the line at the fill column too, both, each in its face
      (let ((tuatara-user:*fill-column* 8))
        (let ((f (run-keys "SPC t | SPC t v C-f C-f" :text text :width 40 :height 9)))
          (is equal '(2) (marked f 1))
          (true (member :visual-line (let ((face (face-at f 1 7))) (if (listp face) face (list face)))))))
      ;; only in the selected window: another showing the buffer has none
      (let ((tuatara-user:*global-cursor-column-mode* t))
        (let ((f (run-keys "C-x 2 C-f C-f" :text text :width 40 :height 16)))
          (is equal '(2) (marked f 1))
          (is equal '() (marked f 9))))
      ;; scrolled sideways, it is where the cursor is on the screen
      (let ((long (format nil "~a~%" (make-string 60 :initial-element #\x)))
            (tuatara-user:*global-cursor-column-mode* t))
        (multiple-value-bind (f editor) (run-keys "C-u 3 5 C-f" :text long :width 20 :height 6)
          (let ((hscroll (tuatara.display:window-hscroll (tuatara.command:editor-window editor))))
            (true (plusp hscroll))
            (is equal (list (- 35 hscroll)) (marked f 0)))))
      ;; never in the hex view
      (let ((tuatara-user:*global-cursor-column-mode* t))
        (is equal '() (marked (run-keys "M-x h e x - m o d e RET" :text text :width 80 :height 9) 1)))
      ;; the settings: by major mode before the global one, the buffer's own choice before both
      (let ((tuatara-user:*global-cursor-column-mode* t))
        (is equal '(0) (marked (run-keys "" :text text :width 40 :height 9) 1))
        (let ((tuatara-user:*cursor-column-modes* '((tuatara.defaults::lisp-interaction-mode . nil)
                                                    (tuatara.defaults::fundamental-mode . nil))))
          (is equal '() (marked (run-keys "" :text text :width 40 :height 9) 1))
          (is equal '(0) (marked (run-keys "SPC t |" :text text :width 40 :height 9) 1)))))))


(define-test (simple huge-counts)
  ;; a count of billions stops at once where the text ends, the editor never held up
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (f editor) (run-keys "C-u 9 9 9 9 9 9 9 9 9 9 C-f" :text (format nil "abc~%def"))
      (declare (ignore f))
      (is = 7 (session-point editor)))
    (multiple-value-bind (f editor) (run-keys "C-u 9 9 9 9 9 9 9 9 9 9 M-f" :text "one two three")
      (declare (ignore f))
      (is = 13 (session-point editor)))
    (multiple-value-bind (f editor) (run-keys "C-u 9 9 9 9 9 9 9 9 9 9 M-}" :text (format nil "a~%~%b~%"))
      (declare (ignore f))
      (is = 5 (session-point editor)))
    (multiple-value-bind (f editor) (run-keys "g e C-u 9 9 9 9 9 9 9 9 9 9 M-b" :text "one two three")
      (declare (ignore f))
      (is = 0 (session-point editor)))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second))))
  ;; a count too large to insert: said, nothing inserted
  (multiple-value-bind (f editor) (run-keys "i C-u 9 9 9 9 9 9 9 9 9 9 a" :text "x")
    (true (tuatara.frontend.fake:fake-frame-showing f "more than can be inserted at once"))
    (is string= "x" (session-text editor))))

(define-test (simple negative-counts)
  ;; a negative count goes the other way, as in Emacs
  (multiple-value-bind (f editor) (run-keys "g e C-u - 2 M-f" :text "one two three")
    (declare (ignore f))
    (is = 4 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "C-u - 1 M-b" :text "one two three")
    (declare (ignore f))
    (is = 3 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "g e C-u - 2 C-d" :text "abcdef")
    (declare (ignore f))
    (is string= "abcd" (session-text editor))))

(define-test (simple screen-columns)
  ;; C-n keeps the column on the screen: after a tab (4 columns in *scratch*) and x, column 5 below, not character 2
  (multiple-value-bind (f editor) (run-keys "C-f C-f C-n" :text (format nil "~cx~%abcdefghijk" #\Tab))
    (declare (ignore f))
    (is = 8 (session-point editor)))
  ;; over a wide character, two columns
  (multiple-value-bind (f editor) (run-keys "C-e C-n" :text (format nil "日本~%abcdef"))
    (declare (ignore f))
    (is = (+ 7 4) (session-point editor))))
