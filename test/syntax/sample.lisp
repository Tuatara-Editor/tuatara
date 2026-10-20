;;;; A sample of Common Lisp for the colouring tests.

(in-package #:cl-user)

(defpackage #:sample
  (:use #:cl)
  (:export #:area #:make-point))

(defvar *count* 0
  "How many there are.")

(defparameter *limit* 10 "The most there may be.")

(defconstant +pi+ 3.14159d0)

(defstruct (point (:constructor make-point (x y)))
  "A point on the plane."
  (x 0 :type real)
  (y 0 :type real))

(defclass shape ()
  ((name :initarg :name :accessor shape-name
         :documentation "The shape's name."))
  (:documentation "Something with an area."))

(defgeneric area (shape)
  (:documentation "The area of SHAPE."))

(defmethod area ((s shape))
  "Shapes in general have none."
  0)

(defmethod area :around ((s shape))
  "Round the area."
  (round (call-next-method)))

(defun (setf point-x*) (value point)
  "Set the x of POINT to VALUE."
  (setf (point-x point) value))

(defmacro with-counting ((var) &body body)
  "Run BODY with VAR counting up; see `*count*' for the total."
  `(let ((,var (incf *count*)))
     ,@body))

;; Keywords, control structures and errors.
(defun classify (n &optional (limit *limit*) &key verbose)
  "Say what N is, compared with LIMIT."
  (declare (type integer n))
  (let* ((kind (cond ((minusp n) :negative)
                     ((> n limit) :large)
                     (t :small))))
    (when verbose
      (format t "~a is ~(~a~)~%" n kind))
    (unless (integerp n)
      (error "Not an integer: ~s" n))
    (loop for i from 0 below n
          collect (* i i) into squares
          finally (return (values kind squares)))))

(defun match-digits (text)
  "Find runs of digits in TEXT with the pattern [^a-z]+ and friends."
  (handler-case (parse-integer text)
    (error () (warn "no number in ~s" text) nil)))

#| A block
   comment. |#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-posix))

(with-open-file (stream "/dev/null" :direction :output :if-exists :append)
  (write-line "done" stream))

(dolist (x '(1 2 3))
  (print (lambda (y) "Add X." (+ x y))))

(catch 'done
  (throw 'done #\a))
