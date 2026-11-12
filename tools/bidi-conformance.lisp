;;;; tools/bidi-conformance.lisp --- Tuatara's right-to-left order against the Unicode conformance tests.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by tools/bidi-conformance.sh with Tuatara's core: runs every case of
;;;; BidiTest.txt and BidiCharacterTest.txt (UAX #9) through BIDI-LEVELS,
;;;; LINE-LEVELS and VISUAL-ORDER, and says how many agree.

(in-package #:tuatara.display)

(defun conformance-line-result (classes paragraph-level codes)
  "The levels and the order shown of one line of CLASSES (and CODES), as the
conformance files write them: a list of levels, NIL for a character removed
from the working out, the order of the others' indices, and the paragraph's
level. A paragraph's end (:B) inside the line ends a paragraph: each is
worked out on its own, at PARAGRAPH-LEVEL or from its own first letter."
  (let* ((n (length classes))
         (levels (make-array n :element-type '(unsigned-byte 8)))
         (first-level nil))
    (loop with start = 0
          while (< start n)
          do (let* ((end (let ((b (position :b classes :start start))) (if b (1+ b) n)))
                    (part (subseq classes start end)))
               (multiple-value-bind (part-levels level)
                   (bidi-levels part :paragraph-level paragraph-level :codes (and codes (subseq codes start end)))
                 (unless first-level (setf first-level level))
                 (replace levels (line-levels part part-levels level) :start1 start))
               (setf start end)))
    (values (loop for i below n collect (if (removed-class-p (svref classes i)) nil (aref levels i)))
            (loop for i across (visual-order levels)
                  unless (removed-class-p (svref classes i)) collect i)
            first-level)))

(defun read-words (text)
  "The words of TEXT, between blanks and tabs."
  (remove "" (uiop:split-string text :separator (list #\Space #\Tab)) :test #'string=))

(defun read-levels (text)
  "The levels a conformance file lists in TEXT, NIL for each x."
  (mapcar (lambda (word) (if (string= word "x") nil (parse-integer word))) (read-words text)))

(defun read-numbers (text &optional (radix 10))
  "The numbers listed in TEXT, written in RADIX."
  (mapcar (lambda (word) (parse-integer word :radix radix)) (read-words text)))

(defun run-bidi-test (path)
  "Run every case of BidiTest.txt at PATH; returns how many passed and how many there were."
  (let ((passed 0) (total 0) (levels nil) (order nil) (shown 0))
    (with-open-file (in path :external-format :utf-8)
      (loop for line = (read-line in nil)
            while line
            do (cond ((or (zerop (length line)) (char= (char line 0) #\#)))
                     ((uiop:string-prefix-p "@Levels:" line) (setf levels (read-levels (subseq line 8))))
                     ((uiop:string-prefix-p "@Reorder:" line) (setf order (read-numbers (subseq line 9))))
                     ((char= (char line 0) #\@))
                     (t (let* ((semicolon (position #\; line))
                               (classes (map 'simple-vector (lambda (w) (intern (string-upcase w) :keyword))
                                             (read-words (subseq line 0 semicolon))))
                               (bits (parse-integer line :start (1+ semicolon) :junk-allowed t)))
                          (loop for (bit level) in '((1 nil) (2 0) (4 1))
                                when (logtest bit bits)
                                  do (incf total)
                                     (multiple-value-bind (got-levels got-order)
                                         (conformance-line-result classes level nil)
                                       (if (and (equal got-levels levels) (equal got-order order))
                                           (incf passed)
                                           (when (< shown 10)
                                             (incf shown)
                                             (format t "~&BidiTest ~a (~a): levels ~a, wanted ~a; ~
                                                        order ~a, wanted ~a~%"
                                                     (subseq line 0 semicolon) bit got-levels levels got-order order))))))))))
    (values passed total)))

(defun run-bidi-character-test (path)
  "Run every case of BidiCharacterTest.txt at PATH; returns how many passed and how many there were."
  (let ((passed 0) (total 0) (shown 0))
    (with-open-file (in path :external-format :utf-8)
      (loop for line = (read-line in nil)
            while line
            do (unless (or (zerop (length line)) (char= (char line 0) #\#))
                 (destructuring-bind (codes direction level levels order) (uiop:split-string line :separator ";")
                   (let* ((codes (coerce (read-numbers codes 16) 'vector))
                          (classes (map 'simple-vector #'code-bidi-class codes))
                          (wanted-levels (read-levels levels))
                          (wanted-order (read-numbers order)))
                     (incf total)
                     (multiple-value-bind (got-levels got-order got-level)
                         (conformance-line-result classes (case (parse-integer direction) (0 0) (1 1) (t nil)) codes)
                       (if (and (equal got-levels wanted-levels) (equal got-order wanted-order)
                                (= got-level (parse-integer level)))
                           (incf passed)
                           (when (< shown 10)
                             (incf shown)
                             (format t "~&BidiCharacterTest ~a: levels ~a; order ~a~%"
                                     line got-levels got-order)))))))))
    (values passed total)))

(defun run-bidi-brackets (path)
  "Check BRACKET-PAIR against every bracket BidiBrackets.txt at PATH lists,
and that it finds no other; returns how many agreed and how many there were."
  (let ((listed (make-hash-table)) (passed 0) (total 0))
    (with-open-file (in path :external-format :utf-8)
      (loop for line = (read-line in nil)
            while line
            do (let ((line (subseq line 0 (position #\# line))))
                 (when (find #\; line)
                   (destructuring-bind (code other kind) (mapcar (lambda (f) (string-trim " " f))
                                                                 (uiop:split-string line :separator ";"))
                     (let ((code (parse-integer code :radix 16)) (other (parse-integer other :radix 16)))
                       (setf (gethash code listed) t)
                       (incf total)
                       (multiple-value-bind (got got-kind) (bracket-pair code)
                         (if (and (eql got (case other (#x2329 #x3008) (#x232A #x3009) (t other)))
                                  (eq got-kind (if (string= kind "o") :open :close)))
                             (incf passed)
                             (format t "~&BidiBrackets ~x: ~a ~a~%" code got got-kind)))))))))
    (loop for code below char-code-limit
          when (and (bracket-pair code) (not (gethash code listed)))
            do (incf total) (format t "~&BidiBrackets: ~x is not listed~%" code))
    (values passed total)))

(defun run-bidi-conformance (directory)
  "Run the conformance files in DIRECTORY and say how many cases of each passed."
  (multiple-value-bind (passed total) (run-bidi-brackets (merge-pathnames "BidiBrackets.txt" directory))
    (format t "~&BidiBrackets.txt: ~d of ~d agree~%" passed total))
  (multiple-value-bind (passed total) (run-bidi-test (merge-pathnames "BidiTest.txt" directory))
    (format t "~&BidiTest.txt: ~d of ~d passed~%" passed total))
  (multiple-value-bind (passed total) (run-bidi-character-test (merge-pathnames "BidiCharacterTest.txt" directory))
    (format t "~&BidiCharacterTest.txt: ~d of ~d passed~%" passed total)))
