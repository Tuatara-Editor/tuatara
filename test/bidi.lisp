;;;; test/bidi.lisp --- Right-to-left text: the order lines are shown in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test bidi
  :description "Right-to-left text: levels and order as the Unicode Bidirectional Algorithm gives them, rows in
the order they are read, the cursor and the region on them, the characters that set the direction set apart,
and left-to-right text laid out as before.")

(defun letters (&rest codes)
  "A string of the characters of CODES, numbers or strings of their own."
  (format nil "~{~a~}" (mapcar (lambda (code) (if (integerp code) (code-char code) code)) codes)))

(defun utf-8-octet-count (text)
  "How many octets TEXT takes in UTF-8."
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun bidi-case (line)
  "The levels and the order shown of LINE, written as a line of the Unicode
conformance file BidiCharacterTest.txt, as Tuatara works them out and as the
line wants them: four values, levels (NIL for a character left out) and
order, worked out then wanted."
  (destructuring-bind (codes direction level levels order) (uiop:split-string line :separator ";")
    (declare (ignore level))
    (flet ((numbers (text radix)
             (mapcar (lambda (word) (if (string= word "x") nil (parse-integer word :radix radix)))
                     (remove "" (uiop:split-string text :separator " ") :test #'string=))))
      (let* ((codes (coerce (numbers codes 16) 'vector))
             (classes (map 'simple-vector #'tuatara.display::code-bidi-class codes))
             (removed (map 'list (lambda (class) (member class '(:lre :rle :lro :rlo :pdf :bn))) classes)))
        (multiple-value-bind (resolved paragraph-level)
            (tuatara.display::bidi-levels classes :codes codes
                                                  :paragraph-level (case (parse-integer direction) (0 0) (1 1)))
          (let ((shown (tuatara.display::line-levels classes resolved paragraph-level)))
            (values (loop for level across shown for gone in removed collect (if gone nil level))
                    (loop for i across (tuatara.display::visual-order shown) unless (nth i removed) collect i)
                    (numbers levels 10)
                    (numbers order 10))))))))

(define-test (bidi levels-as-unicode-gives-them)
  ;; lines of BidiCharacterTest.txt (Unicode 15.0): brackets, marks, numbers, isolates, embeddings, overrides
  (dolist (line '("05D0 0028 05D1 0029 0331;0;0;1 1 1 1 1;4 3 2 1 0"
                  "05D0 0028 0029 2680 0061 0028 0029 2681;1;1;1 1 1 1 2 1 1 1;7 6 5 4 3 2 1 0"
                  "05D0 0028 2680 0061 2681 0028 0029 0029;1;1;1 1 1 2 1 1 1 1;7 6 5 4 3 2 1 0"
                  "05D0 202A 05D1 202C 0020 0031 0020 0032;0;0;1 x 3 x 0 0 0 0;2 0 4 5 6 7"
                  "0061 202B 0062 202C 0020 0031 0020 0032;1;1;2 x 4 x 1 2 1 2;7 6 5 4 0 2"
                  #.(concatenate 'string "202E 0061 202A 0062 202C 2066 0063 2069 202A 0064 202C 0065 202C;2;0;"
                                 "x 1 x 2 x 1 2 1 x 2 x 1 x;11 9 7 6 5 3 1")
                  "202B 0061 0028 0062 202C 202E 0029;2;0;x 2 2 2 x x 1;6 1 2 3"))
    (multiple-value-bind (levels order wanted-levels wanted-order) (bidi-case line)
      (is equal wanted-levels levels "levels of ~a" line)
      (is equal wanted-order order "order of ~a" line))))

(define-test (bidi right-to-left-characters-all-found)
  ;; every character of a class that can change a line's order is found, and none other
  (let ((wrong (loop for code from 0 below char-code-limit
                     unless (eq (tuatara.display::right-to-left-code-p code)
                                (and (member (tuatara.display::code-bidi-class code)
                                             '(:r :al :an :lre :rle :lro :rlo :pdf :lri :rli :fsi :pdi))
                                     t))
                       collect code)))
    (is equal '() wrong)))

(defun bidi-rows (text width &rest keys)
  "TEXT laid out WIDTH columns wide, with KEYS for LAYOUT, :DIRECTION :AUTO unless given."
  (apply #'tuatara.display:layout (tuatara.text:string->rope text) 0 width 10 (append keys '(:direction :auto))))

(define-test (bidi rows-in-the-order-read)
  (let* ((hebrew (letters "abc " #x5D0 #x5D1 #x5D2 " def"))
         (row (aref (bidi-rows hebrew 20) 0)))
    ;; a run written from right to left turned round within a line written from left to right
    (is string= (letters "abc " #x5D2 #x5D1 #x5D0 " def") (tuatara.display:row-string row))
    (is eq :visual (first (tuatara.display:row-mixed row)))
    ;; the first letter of the run, at offset 4, shown where the run ends
    (is equal '(0 6) (multiple-value-list (tuatara.display:point-row-column (bidi-rows hebrew 20) 4)))
    (is equal '(0 4) (multiple-value-list (tuatara.display:point-row-column (bidi-rows hebrew 20) 8)))
    ;; stored order without a direction
    (is string= hebrew (tuatara.display:row-string (aref (bidi-rows hebrew 20 :direction nil) 0))))
  ;; a line written from right to left: against the right edge, its number read from left to right,
  ;; its end shown left of it
  (let* ((text (letters #x5D0 #x5D1 #x5D2 " 123"))
         (rows (bidi-rows text 12))
         (row (aref rows 0)))
    (is string= (letters "     123 " #x5D2 #x5D1 #x5D0) (tuatara.display:row-string row))
    (is = 4 (tuatara.display:row-end-column row))
    (is equal '(0 4) (multiple-value-list (tuatara.display:point-row-column rows (utf-8-octet-count text))))
    (is equal '(0 11) (multiple-value-list (tuatara.display:point-row-column rows 0)))
    ;; the line point is on lit out to the edge: on that side, the blanks before its text
    (tuatara.display:apply-highlights rows (list (list 0 (1+ (utf-8-octet-count text)) :hl-line)))
    (is eq :hl-line (aref (tuatara.display:row-faces row) 0))
    (is eq :hl-line (aref (tuatara.display:row-faces row) 4)))
  ;; brackets mirrored in text written from right to left
  (is string= (letters "(" #x5D1 ")" #x5D0)
      (string-left-trim " " (tuatara.display:row-string (aref (bidi-rows (letters #x5D0 "(" #x5D1 ")") 10) 0))))
  ;; every line written from right to left when told so, a line of Latin letters too
  (let ((row (aref (bidi-rows "abc" 6 :direction :right-to-left) 0)))
    (is string= "   abc" (tuatara.display:row-string row))
    (is = 2 (tuatara.display:row-end-column row)))
  ;; a mark stays after the letter it is on
  (let ((row (aref (bidi-rows (letters #x5D0 #x5B8 #x5D1) 10) 0)))
    (is string= (letters #x5D1 #x5D0 #x5B8)
        (string-left-trim " " (tuatara.display:row-string row)))))

(define-test (bidi wrapped-lines-ordered-row-by-row)
  ;; a line written from right to left, wrapped: each row in the order read, the first row's letters at the right
  (let* ((words (letters #x5D0 #x5D1 " " #x5D2 #x5D3 " " #x5D4 #x5D5))
         (rows (bidi-rows words 6 :wrap t :word-wrap t)))
    (is = 2 (length rows))
    (is string= (letters " " #x5D3 #x5D2 " " #x5D1 #x5D0) (tuatara.display:row-string (aref rows 0)))
    (is string= (letters "    " #x5D5 #x5D4) (tuatara.display:row-string (aref rows 1))))
  ;; a window beginning inside a line written from right to left: its rows still against the right edge
  (let* ((text (format nil "~c~a" (code-char #x5D0) (make-string 14 :initial-element #\x)))
         (rope (tuatara.text:string->rope text))
         (rows (tuatara.display:layout rope 11 10 3 :wrap t :direction :auto)))
    (is string= "     xxxxx" (tuatara.display:row-string (aref rows 0)))))

(define-test (bidi direction-controls-set-apart)
  ;; an override turns the letters within round, and each control shows as ? in :invisible-character
  (let* ((text (format nil "a~cbc~cd" (code-char #x202E) (code-char #x202C)))
         (row (aref (bidi-rows text 10) 0)))
    (is string= "a?cb?d" (tuatara.display:displayable (tuatara.display:row-string row)))
    (is eq :invisible-character (aref (tuatara.display:row-faces row) 1))
    (is eq :invisible-character (aref (tuatara.display:row-faces row) 4))
    (is eq nil (aref (tuatara.display:row-faces row) 2)))
  ;; and in stored order too
  (let ((row (aref (bidi-rows (format nil "a~cb" (code-char #x2067)) 10 :direction nil) 0)))
    (is eq :invisible-character (aref (tuatara.display:row-faces row) 1))))

(define-test (bidi left-to-right-rows-as-before)
  ;; text with no right-to-left character laid out alike with a direction and without, at the same cost
  (let* ((text (with-output-to-string (out)
                 (dotimes (i 60)
                   (format out "line ~d ab~cあ~cc~cd ~c 12,5 (x) [y]~%" i #\Tab (code-char #x301) (code-char #x200D)
                           (code-char #xE9)))))
         (rope (tuatara.text:string->rope text)))
    (flet ((rows (direction height) (tuatara.display:layout rope 0 30 height :direction direction :wrap t)))
      (loop for with across (rows :auto 40)
            for without across (rows nil 40)
            do (is string= (tuatara.display:row-string without) (tuatara.display:row-string with))
               (is equalp (tuatara.display:row-bytes without) (tuatara.display:row-bytes with))
               (is eq nil (tuatara.display:row-mixed with))
               (is eq nil (tuatara.display:row-end-column with)))
      (rows :auto 40) (rows nil 40)
      (flet ((per-row (direction)
               (/ (- (nth-value 2 (measuring (rows direction 40))) (nth-value 2 (measuring (rows direction 20)))) 20)))
        (is = (per-row nil) (per-row :auto))))))

(define-test (bidi cursor-and-region-in-a-session)
  (let ((text (letters "abc " #x5D0 #x5D1 #x5D2 " def")))
    ;; C-f goes through the text as stored; the cursor stands where the character is shown
    (let ((f (run-keys "C-f C-f C-f C-f C-f" :text text :width 30 :height 6)))
      (true (tuatara.frontend.fake:fake-frame-showing f (letters "abc " #x5D2 #x5D1 #x5D0 " def")))
      (is equal '(0 . 5) (tuatara.frontend.fake:fake-cursor f)))
    ;; the region over the first two letters of the run lights the two columns they are shown in
    (let* ((f (run-keys "C-f C-f C-f C-f C-SPC C-f C-f" :text text :width 30 :height 6))
           (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
      (flet ((lit-p (column)
               (some (lambda (span)
                       (destructuring-bind (from below face) span
                         (and (<= from column) (< column below)
                              (if (listp face) (member :region face) (eq face :region)))))
                     spans)))
        (true (lit-p 5))
        (true (lit-p 6))
        (false (lit-p 4))
        (false (lit-p 3))))
    ;; text-direction-mode: the characters in the order stored, and back
    (let ((f (run-keys "M-x t e x t - d i r e c t i o n - m o d e RET" :text text :width 30 :height 6)))
      (true (search text (tuatara.frontend.fake:fake-screen-line f 0))))))
