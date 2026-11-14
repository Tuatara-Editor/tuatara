;;;; lisp/files/glob.lisp --- Names matched against globs, as shells and .gitignore files write them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; GLOB-MATCH matches a name or a path against a glob: * ** ? classes and
;;;; braces, in time growing with the glob's length times the name's at most.
;;;; Evidence directories, searches, rimu and the language servers use it.

(in-package #:tuatara.defaults)

(defun glob-class (glob start &optional function)
  "Read the class of characters GLOB begins at START, its [: ! or ^ after the
[ makes it the characters not in it, a ] first in it stands for itself (so
that []] and [!]] hold a ]), \\ makes the next character only itself, and
a-z is a range. FUNCTION, when given, is called with the first and the last
character of each range it holds, a character alone as both. Returns whether
it is negated and the position of its ], as two values; NIL when it is not
closed, and the [ is then only itself."
  (let* ((n (length glob))
         (i (1+ start))
         (negated (and (< i n) (member (char glob i) '(#\! #\^)) t)))
    (when negated (incf i))
    (flet ((next ()
             ;; the character at I, \ taking the one after it as it is
             (if (and (char= (char glob i) #\\) (< (1+ i) n))
                 (prog1 (char glob (1+ i)) (incf i 2))
                 (prog1 (char glob i) (incf i)))))
      (loop with first = t
            while (< i n)
            do (when (and (char= (char glob i) #\]) (not first))
                 (return-from glob-class (values negated i)))
               (let ((low (next)))
                 (if (and (< (1+ i) n) (char= (char glob i) #\-) (char/= (char glob (1+ i)) #\]))
                     (progn (incf i)
                            (let ((high (next)))
                              (when function (funcall function low high))))
                     (when function (funcall function low low))))
               (setf first nil))
      nil)))

(defun glob-step (glob p char test)
  "Where GLOB goes on after its part at P, ? or a class ([abc], [a-z], [!abc]
or [^abc] those not in it, GLOB-CLASS) or \\ and a character or any other
character, when that part takes CHAR, compared by TEST; NIL when it does not."
  (let ((c (char glob p)))
    (cond ((char= c #\?) (1+ p))
          ((and (char= c #\\) (< (1+ p) (length glob)))
           (and (funcall test (char glob (1+ p)) char) (+ p 2)))
          ((char= c #\[)
           (let ((in nil)
                 (fold (not (eq test #'char=))))
             (flet ((take (low high)
                      (when (if (char= low high)
                                (funcall test low char)
                                (if fold
                                    (char<= (char-downcase low) (char-downcase char) (char-downcase high))
                                    (char<= low char high)))
                        (setf in t))))
               (declare (dynamic-extent #'take))
               (multiple-value-bind (negated end) (glob-class glob p #'take)
                 (if end
                     (and (if negated (not in) in) (1+ end))
                     (and (funcall test c char) (1+ p)))))))
          (t (and (funcall test c char) (1+ p))))))

(defun glob-alternatives (glob &key (limit 256))
  "The globs without braces that GLOB stands for, in order: {a,b} in it
either a or b, groups inside groups too, one left open closed at the end, and
a , or } outside a group only itself. NIL when there would be more than LIMIT
of them."
  (let ((i 0) (n (length glob)))
    (labels ((unit-end (i)
               ;; past the part of GLOB at I: a class, \ and a character, or one character
               (cond ((and (char= (char glob i) #\\) (< (1+ i) n)) (+ i 2))
                     ((char= (char glob i) #\[)
                      (let ((end (nth-value 1 (glob-class glob i))))
                        (if end (1+ end) (1+ i))))
                     (t (1+ i))))
             (joined (heads tails)
               (when (> (* (length heads) (length tails)) limit)
                 (return-from glob-alternatives nil))
               (loop for head in heads
                     nconc (loop for tail in tails collect (concatenate 'string head tail))))
             (run (inside)
               ;; from I to the end, or to the , or } that ends the group the run is INSIDE
               (let ((found (list "")))
                 (loop while (< i n)
                       do (let ((c (char glob i)))
                            (cond ((and inside (find c ",}")) (return))
                                  ((char= c #\{) (incf i) (setf found (joined found (group))))
                                  (t (let ((start i))
                                       (loop do (setf i (unit-end i))
                                             while (and (< i n) (not (find (char glob i) (if inside "{,}" "{")))))
                                       (setf found (joined found (list (subseq glob start i)))))))))
                 found))
             (group ()
               (let ((choices '()))
                 (loop (setf choices (append choices (run t)))
                       (when (> (length choices) limit)
                         (return-from glob-alternatives nil))
                       (cond ((>= i n) (return choices))
                             ((char= (char glob i) #\}) (incf i) (return choices))
                             (t (incf i)))))))
      (run nil))))

(defun glob-star (glob p)
  "How many characters the star of GLOB at P is: 3 for **/, 2 for **, 1 for *,
0 when there is none there."
  (let ((n (length glob)))
    (cond ((char/= (char glob p) #\*) 0)
          ((not (and (< (1+ p) n) (char= (char glob (1+ p)) #\*))) 1)
          ((and (< (+ p 2) n) (char= (char glob (+ p 2)) #\/)) 3)
          (t 2))))

(defun glob-pattern-p (name)
  "True when NAME, a file name or the last part of a path, holds a * or a ?:
a pattern names are matched against (GLOB-MATCH), not a name."
  (find-if (lambda (c) (or (char= c #\*) (char= c #\?))) name))

(defun glob-match (pattern name &key (test #'char=) braces (start 0))
  "True when NAME, from START, is all a name or path that PATTERN stands for:
* any characters but /, ** any characters, / too (**/ any number of directories,
none too), ? any one character but /, [abc] one of those ([a-z] a range of
them, [!abc] or [^abc] one not among them, GLOB-CLASS), \\ the next character
only itself, the rest as written, compared by TEST (CHAR-EQUAL for any case).
With BRACES, {a,b} stands for either a or b (GLOB-ALTERNATIVES). It never
steps back, so that its time grows with the length of PATTERN times that of
NAME at most, whatever the two hold."
  (if braces
      (some (lambda (glob) (glob-match glob name :test test :start start))
            (glob-alternatives pattern))
      (let* ((pl (length pattern))
             (stars (make-array pl :element-type '(unsigned-byte 2)))
             (now (make-array (1+ pl) :element-type 'bit :initial-element 0))
             (next (make-array (1+ pl) :element-type 'bit :initial-element 0)))
        (declare (dynamic-extent stars now next) (fixnum pl))
        ;; the plain characters PATTERN ends with, after its last / (which **/ may stand for), must
        ;; end NAME: most names fail there at once
        (let ((plain (loop for i from (1- pl) downto 0 until (find (char pattern i) "*?[]\\/") count t)))
          (unless (and (<= plain (- (length name) start))
                       (loop for k from 1 to plain
                             always (funcall test (char pattern (- pl k)) (char name (- (length name) k)))))
            (return-from glob-match nil))
          (when (= plain pl)
            (return-from glob-match (= pl (- (length name) start)))))
        (dotimes (p pl) (setf (aref stars p) (glob-star pattern p)))
        (flet ((close-over-stars (set)
                 ;; a star may stand for nothing: a place before it is a place after it too
                 (declare (simple-bit-vector set))
                 (loop for p = (position 1 set) then (position 1 set :start (1+ p))
                       while (and p (< p pl))
                       do (let ((k (aref stars p)))
                            (when (plusp k) (setf (sbit set (+ p k)) 1))))))
          (setf (sbit now 0) 1)
          (close-over-stars now)
          (loop for n from start below (length name)
                for c = (char name n)
                do (fill next 0)
                   (loop for p = (position 1 now) then (position 1 now :start (1+ p))
                         while (and p (< p pl))
                         do (case (aref stars p)
                              (3 (setf (sbit next p) 1)
                               (when (char= c #\/) (setf (sbit next (+ p 3)) 1)))
                              (2 (setf (sbit next p) 1))
                              (1 (unless (char= c #\/) (setf (sbit next p) 1)))
                              (t (unless (and (char= c #\/) (char= (char pattern p) #\?))
                                   (let ((q (glob-step pattern p c test)))
                                     (when q (setf (sbit next q) 1)))))))
                   (close-over-stars next)
                   (rotatef now next)
                   (unless (find 1 now) (return-from glob-match nil)))
          (= 1 (sbit now pl))))))

(defun glob-components (path)
  "The names of PATH, a glob or a path from the root, in order."
  (remove "" (uiop:split-string path :separator "/") :test #'string=))
