;;;; Lisp indented wrongly on purpose, for the indentation test: indenting
;;;; all of it gives indent-after.lisp, as Emacs's lisp-mode indents it.

(defun area (shape &key (scale 1)
&aux (unit 2))
"The area of SHAPE.
These lines of the docstring
    stay as they are."
(let ((width (first shape))
(height (second shape)))
(* width height scale unit)))

(defmethod describe-shape :around ((shape list)
 &optional stream)
 (declare (ignore stream))
 (call-next-method))

(defmacro with-shape ((var shape) &body body)
`(let ((,var ,shape))
,@body))

(defun classify (x)
(cond ((< x 0) :negative)
((= x 0)
:zero)
(t
:positive)))

(defun collect-evens (items)
(loop for item in items
when (evenp item)
collect item
finally (return nil)))

(defun handle (thunk)
(handler-case (funcall thunk)
(error (condition)
(format t "~a" condition)
nil)))

(flet ((double (n)
(* 2 n)))
(double 21))

(defparameter *table*
'((:a 1)
(:b 2)))

(make-instance 'point :x 1
:y 2)

(list (;; a comment right after the bracket
a b
c d)
e)

(
foo bar
baz)

(foo (bar
baz) qux
quux)

(mapcar #'(lambda (x)
(* x x))
'(1 2 3))

(tagbody
start
(print 1)
(go start))

(if (plusp 1)
:yes
:no)

(let ((x 1)) ; a comment after code stays where it is
; a comment of one semicolon
;; a comment of two
;;; a comment of three stays as it is
x)

(defun documented ()
"Returns a list:
(1 2 3)
as its value."
(list 1
2 3))

#|
  a block comment
|#

#||
  another, ended so ||#
(foo
bar)
