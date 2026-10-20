;;;; test/sort.lisp --- Sorting, reversing, filtering and deduplicating lines, in memory and on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test sorting
  :description "The sorting commands on the lines the selection touches, nothing without one; stable, descending with C-u; the same results in memory and on disk in pieces; flush-lines, keep-lines and delete-duplicate-lines.")

(defun lines-text (&rest lines)
  "LINES joined, each ending with a newline."
  (format nil "~{~a~%~}" lines))

(defun sorted-text (keys text &key (on-disk nil) (width 100) here)
  "The text after typing KEYS on TEXT: C-x h selects everything first. With
ON-DISK, the work goes to disk in tiny pieces so that a small text exercises it;
the lines rewritten in the background, in pieces of a few lines each. With
HERE, the text is kept off the disk, as an encrypted file's is, so that lines
are rewritten at once."
  (flet ((run ()
           (session-text (nth-value 1 (run-keys (list (lambda ()
                                                        (when here
                                                          (setf (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*scratch*") :plaintext) t)))
                                                      (format nil "C-x h ~a" keys))
                                                :text text :width width)))))
    (if on-disk
        (let ((tuatara.defaults::*sort-in-memory-limit* 0)
              (tuatara.defaults::*sort-piece* 16)
              (tuatara.defaults::*filter-piece* 16)
              (tuatara.defaults::*rewrite-piece* 16)
              (tuatara.defaults::*rewrite-checkpoint* 8))
          (run))
        (run))))

(defparameter *sort-sample*
  (lines-text "pear 3 0x10 b" "apple 12 0x2 a" "fig 7 0xff c" "Banana 12 0x1 d" "apple 1 0x3 e" "cherry 007 0x0 f")
  "Lines to sort: fields of words, decimal numbers (one zero-padded), hex numbers, letters.")

(define-test (sorting sort-lines)
  (dolist (on-disk '(nil t))
    ;; by character code: capitals first; equal lines keep their order
    (is string= (lines-text "Banana 12 0x1 d" "apple 1 0x3 e" "apple 12 0x2 a" "cherry 007 0x0 f" "fig 7 0xff c" "pear 3 0x10 b")
        (sorted-text "M-x s o r t - l i n e s RET" *sort-sample* :on-disk on-disk))
    ;; descending
    (is string= (lines-text "pear 3 0x10 b" "fig 7 0xff c" "cherry 007 0x0 f" "apple 12 0x2 a" "apple 1 0x3 e" "Banana 12 0x1 d")
        (sorted-text "C-u M-x s o r t - l i n e s RET" *sort-sample* :on-disk on-disk))
    ;; by the second field as numbers: 007 is seven, not octal
    (is string= (lines-text "apple 1 0x3 e" "pear 3 0x10 b" "fig 7 0xff c" "cherry 007 0x0 f" "apple 12 0x2 a" "Banana 12 0x1 d")
        (sorted-text "C-u 2 M-x s o r t - n u m e r i c - f i e l d s RET" *sort-sample* :on-disk on-disk))
    ;; by the third field, hexadecimal
    (is string= (lines-text "cherry 007 0x0 f" "Banana 12 0x1 d" "apple 12 0x2 a" "apple 1 0x3 e" "pear 3 0x10 b" "fig 7 0xff c")
        (sorted-text "C-u 3 M-x s o r t - n u m e r i c - f i e l d s RET" *sort-sample* :on-disk on-disk))
    ;; by the last field, as text
    (is string= (lines-text "apple 12 0x2 a" "pear 3 0x10 b" "fig 7 0xff c" "Banana 12 0x1 d" "apple 1 0x3 e" "cherry 007 0x0 f")
        (sorted-text "C-u - 1 M-x s o r t - f i e l d s RET" *sort-sample* :on-disk on-disk))
    ;; reversed as they are
    (is string= (lines-text "cherry 007 0x0 f" "apple 1 0x3 e" "Banana 12 0x1 d" "fig 7 0xff c" "apple 12 0x2 a" "pear 3 0x10 b")
        (sorted-text "M-x r e v e r s e - r e g i o n RET" *sort-sample* :on-disk on-disk))
    ;; a last line without a newline: the line ends stay where they were
    (is string= (format nil "a~%b~%c") (sorted-text "M-x s o r t - l i n e s RET" (format nil "c~%b~%a") :on-disk on-disk))))

(define-test (sorting ignoring-case-and-selections)
  (let ((tuatara.defaults::*sort-fold-case* t))
    (is string= (lines-text "apple 1 0x3 e" "apple 12 0x2 a" "Banana 12 0x1 d" "cherry 007 0x0 f" "fig 7 0xff c" "pear 3 0x10 b")
        (sorted-text "M-x s o r t - l i n e s RET" *sort-sample*)))
  ;; without a selection nothing happens, and the echo area says so
  (multiple-value-bind (f editor) (run-keys "M-x s o r t - l i n e s RET" :text (lines-text "b" "a") :width 100)
    (is string= (lines-text "b" "a") (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No selection: sort-lines was not run")))
  ;; only the lines the selection touches: from the middle of the second line into the third
  (is string= (lines-text "z" "a" "y" "b")
      (session-text (nth-value 1 (run-keys "C-n C-f C-SPC C-n M-x s o r t - l i n e s RET"
                                           :text (lines-text "z" "y" "a" "b") :width 100)))))

(define-test (sorting duplicates)
  (let ((text (lines-text "a" "b" "a" "" "c" "b" "" "a")))
    (dolist (on-disk '(nil t))
      ;; the first of each kept
      (is string= (lines-text "a" "b" "" "c")
          (sorted-text "M-x d e l e t e - d u p l i c a t e - l i n e s RET" text :on-disk on-disk))
      ;; with C-u the last
      (is string= (lines-text "c" "b" "" "a")
          (sorted-text "C-u M-x d e l e t e - d u p l i c a t e - l i n e s RET" text :on-disk on-disk))
      ;; with C-u C-u C-u blank lines are all kept
      (is string= (lines-text "a" "b" "" "c" "")
          (sorted-text "C-u C-u C-u M-x d e l e t e - d u p l i c a t e - l i n e s RET" text :on-disk on-disk)))
    ;; with C-u C-u only those next to one like them
    (dolist (on-disk '(nil t))
      (is string= (lines-text "a" "b" "a" "c")
          (sorted-text "C-u C-u M-x d e l e t e - d u p l i c a t e - l i n e s RET" (lines-text "a" "a" "b" "b" "b" "a" "c" "c")
                       :on-disk on-disk)))
    ;; lines longer than a piece on disk: each its own piece, compared with the one before all the same
    (let ((x (make-string 20 :initial-element #\x)) (y (make-string 20 :initial-element #\y)))
      (is string= (lines-text x y x)
          (sorted-text "C-u C-u M-x d e l e t e - d u p l i c a t e - l i n e s RET" (lines-text x x x y y x) :on-disk t)))
    (multiple-value-bind (f editor) (run-keys "C-x h M-x d e l e t e - d u p l i c a t e - l i n e s RET" :text text :width 100)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted 4 duplicate lines")))))

(define-test (sorting few-files-open)
  ;; work on lines on disk in hundreds of pieces holds a file a worker, not one a piece:
  ;; a window's limit of 1024 open files was passed by a file of gigabytes
  (flet ((open-files () (length (uiop:directory-files "/proc/self/fd/"))))
    (let* ((text (format nil "~{~a~%~}" (loop for i below 3000 collect (format nil "line ~d ~:[odd~;even~]" i (evenp i)))))
           (before (open-files)))
      ;; keep-lines, a job over lines
      (let ((kept (let ((tuatara.defaults::*rewrite-piece* 64))
                    (sorted-text "M-x k e e p - l i n e s RET e v e n RET" text))))
        (is = 1500 (count #\Newline kept))
        (true (<= (- (open-files) before) (+ 4 (tuatara.runtime:cpu-count)))
              "~d files more open than before" (- (open-files) before)))
      ;; adjacent duplicate lines, cleared a piece at a time; the files the
      ;; result of the job before is read from stay open with it
      (setf before (open-files))
      (let ((kept (let ((tuatara.defaults::*sort-in-memory-limit* 0)
                        (tuatara.defaults::*filter-piece* 64))
                    (sorted-text "C-u C-u M-x d e l e t e - d u p l i c a t e - l i n e s RET"
                                 (format nil "~{~a~%~a~%~}" (loop for i below 1500 collect i collect i))))))
        (is = 1500 (count #\Newline kept))
        (true (<= (- (open-files) before) (+ 4 (tuatara.runtime:cpu-count)))
              "~d files more open than before" (- (open-files) before))))))

(define-test (sorting flush-and-keep)
  (let ((text (lines-text "INFO start" "ERROR disk" "" "info again" "ERROR net" "")))
    ;; at once, in the background, and in the background in pieces of a few lines: the same lines
    (loop for (on-disk here) in '((nil t) (nil nil) (t nil))
          do (is string= (lines-text "INFO start" "" "info again" "")
                 (sorted-text "M-x f l u s h - l i n e s RET E R R O R RET" text :on-disk on-disk :here here))
             ;; smart case: lower case ignores case
             (is string= (lines-text "ERROR disk" "" "ERROR net" "")
                 (sorted-text "M-x f l u s h - l i n e s RET i n f o RET" text :on-disk on-disk :here here))
             ;; ^$ is an empty line
             (is string= (lines-text "INFO start" "ERROR disk" "info again" "ERROR net")
                 (sorted-text "M-x f l u s h - l i n e s RET ^ $ RET" text :on-disk on-disk :here here))
             (is string= (lines-text "ERROR disk" "ERROR net")
                 (sorted-text "M-x k e e p - l i n e s RET E R R O R RET" text :on-disk on-disk :here here))
             ;; a last line without a newline
             (is string= (format nil "INFO start~%info again")
                 (sorted-text "M-x k e e p - l i n e s RET i n f o RET" (format nil "INFO start~%ERROR disk~%info again")
                              :on-disk on-disk :here here)))
    ;; in a CSV, on the column asked for, by name or number from 0, its value unquoted; the header stays
    (let ((csv (lines-text "id,name,city" "1,\"ann\",Oslo" "2,bob,\"Bergen, \"\"west\"\"\"" "3,annette,Oslo")))
      (loop for (on-disk here) in '((nil t) (nil nil) (t nil))
            do (is string= (lines-text "id,name,city" "1,\"ann\",Oslo")
                   (sorted-text "M-x c s v - m o d e RET C-x h M-x k e e p - l i n e s RET n a m e RET ^ a n n $ RET" csv
                                :on-disk on-disk :here here))
               (is string= (lines-text "id,name,city" "1,\"ann\",Oslo" "3,annette,Oslo")
                   (sorted-text "M-x c s v - m o d e RET C-x h M-x f l u s h - l i n e s RET $ 2 RET w e s t \" RET" csv
                                :on-disk on-disk :here here))
               (is string= (lines-text "id,name,city" "2,bob,\"Bergen, \"\"west\"\"\"")
                   (sorted-text "M-x c s v - m o d e RET C-x h M-x f l u s h - l i n e s RET $ * RET O s l o RET" csv
                                :on-disk on-disk :here here))))
    (multiple-value-bind (f editor) (run-keys "M-x k e e p - l i n e s RET" :text text :width 100)
      (is string= text (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "No selection: keep-lines was not run")))))

(define-test (sorting pattern-question)
  ;; keep-lines and flush-lines ask as a search does: the kind first, M-r for literal
  ;; text, how many lines match before the prompt, a pattern that cannot be used refused
  (let ((text (lines-text "a.b" "axb" "ERROR disk" "ERROR net")))
    (is string= (lines-text "a.b") (sorted-text "M-x k e e p - l i n e s RET M-r a . b RET" text :here t))
    (is string= (lines-text "a.b" "axb") (sorted-text "M-x k e e p - l i n e s RET a . b RET" text :here t))
    (is string= (lines-text "axb" "ERROR disk" "ERROR net")
        (sorted-text "M-x f l u s h - l i n e s RET M-r a . b RET" text :here t))
    (multiple-value-bind (f editor)
        (run-keys (list "C-x h M-x k e e p - l i n e s RET E R R O R" (lambda () (sleep 0.2)) :tick
                        "M-r" :tick "C-g")
                  :text text :width 100)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "2      Regex keep lines: ERROR"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Literal keep lines: ERROR")))
    ;; a regular expression that cannot be used waits for RET, saying why, the text kept
    (multiple-value-bind (f editor)
        (run-keys "C-x h M-x k e e p - l i n e s RET ( RET C-g" :text text :width 120)
      (true (tuatara.frontend.fake:fake-frame-showing f "Regex keep lines [incomplete; finish it, or C-g gives up]: ("))
      (is string= text (session-text editor)))
    ;; the pattern typed is kept for the next question, M-p bringing it back
    (is string= (lines-text "ERROR disk" "ERROR net")
        (sorted-text "M-x k e e p - l i n e s RET E R R O R RET u C-x h M-x k e e p - l i n e s RET M-p RET" text :here t))))

(define-test (sorting sort-stands-where-begun)
  ;; a sort in the background stands in the undo history where it began: an edit made
  ;; elsewhere while it ran is undone first, then the sort
  (let* ((lines (loop for i from 1999 downto 0 collect (format nil "line ~4,'0d" i)))
         (text (format nil "~{~a~%~}end~%" lines))
         (sorted (format nil "~{~a~%~}end~%" (sort (copy-list lines) #'string<)))
         (tuatara.defaults::*sort-in-memory-limit* 0)
         (tuatara.defaults::*sort-piece* 16)
         (tuatara.defaults::*sort-wait* 0)
         (b nil) (running nil))
    (run-keys (list (lambda () (setf b (tuatara.command:current-buffer)))
                    "g g V C-u 1 9 9 9 j M-x s o r t - l i n e s RET"
                    (lambda ()
                      (setf running (and (tuatara.runtime:jobs-of b :running t) t))
                      (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-size b) "Z"))
                    (lambda () (loop while (tuatara.runtime:jobs-of b :running t) do (sleep 0.02)))
                    :tick :tick)
              :text text :width 100 :height 10)
    (true running "the sort ran while the edit was made")
    (is string= (concatenate 'string sorted "Z") (tuatara.buffer:buffer-string b))
    (tuatara.buffer:buffer-undo b)
    (is string= sorted (tuatara.buffer:buffer-string b))
    (tuatara.buffer:buffer-undo b)
    (is string= text (tuatara.buffer:buffer-string b))))

(define-test (sorting sort-stands-where-given-queued)
  ;; a sort waiting its turn behind a replacement, then an edit typed on another line: undo
  ;; takes back the edit first, then the sort, then the replacement, the order they were given in
  (let ((tuatara.defaults::*rewrite-threshold* 0)
        (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2)))
        (b nil))
    (run-keys (list (lambda () (setf b (tuatara.command:current-buffer)))
                    "r r % z / y RET" "g g j V j M-x s o r t - l i n e s RET" "g g i X C-g"
                    (lambda () (loop while (tuatara.runtime:jobs-of b :running t) do (sleep 0.05)))
                    :tick :tick
                    (lambda () (loop while (or (tuatara.runtime:jobs-of b :running t) (tuatara.defaults::queued-in b))
                                     do (sleep 0.05)))
                    :tick)
              :text (lines-text "zz" "b" "a") :width 100 :height 8)
    (let ((steps (list (tuatara.buffer:buffer-string b))))
      (loop repeat 3 do (tuatara.buffer:buffer-undo b) (push (tuatara.buffer:buffer-string b) steps))
      (is equal (list (lines-text "Xyy" "a" "b") (lines-text "yy" "a" "b") (lines-text "yy" "b" "a") (lines-text "zz" "b" "a"))
          (nreverse steps)))))

(define-test (sorting filter-keeps-edits)
  ;; a filter in the background keeps the edits made to its lines meanwhile: text
  ;; typed elsewhere stays, and a line typed in is kept as typed, not deleted
  (let ((text (lines-text "keep" "drop" "keep too" "end"))
        (tuatara.defaults::*sort-wait* 0)
        (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (multiple-value-bind (f editor)
        (run-keys "C-x h M-x f l u s h - l i n e s RET d r o p RET M-< i x C-g" :text text :width 120)
      (true (tuatara.frontend.fake:fake-frame-showing f "Deleted 1 matching line"))
      (is string= (lines-text "xkeep" "keep too" "end") (session-text editor)))
    (multiple-value-bind (f editor)
        (run-keys "C-x h M-x f l u s h - l i n e s RET d r o p RET M-< C-n C-f C-f i X C-g" :text text :width 120)
      (true (tuatara.frontend.fake:fake-frame-showing f "1 kept as you edited them"))
      (is string= (lines-text "keep" "drXop" "keep too" "end") (session-text editor)))
    ;; the filter stands in the undo history where it began: the edit made meanwhile is
    ;; undone first, then the filter
    (multiple-value-bind (f editor)
        (run-keys-in-turns '("C-x h M-x f l u s h - l i n e s RET d r o p RET M-< i x C-g" "C-/") :text text :width 120)
      (declare (ignore f))
      (is string= (lines-text "keep" "keep too" "end") (session-text editor)))
    (multiple-value-bind (f editor)
        (run-keys-in-turns '("C-x h M-x f l u s h - l i n e s RET d r o p RET M-< i x C-g" "C-/ C-/") :text text :width 120)
      (declare (ignore f))
      (is string= text (session-text editor)))))

(define-test (sorting lines-too-long-to-read)
  ;; a line longer than *rewrite-line-limit* is left as it is, not given to the
  ;; pattern, and said; at once and in the background alike
  (let ((tuatara.defaults::*rewrite-line-limit* 10)
        (text (lines-text "drop" "a long line to drop" "keep")))
    (loop for (on-disk here) in '((nil t) (nil nil) (t nil))
          do (is string= (lines-text "a long line to drop" "keep")
                 (sorted-text "M-x f l u s h - l i n e s RET d r o p RET" text :on-disk on-disk :here here)))
    (true (tuatara.frontend.fake:fake-frame-showing
           (run-keys "C-x h M-x f l u s h - l i n e s RET d r o p RET" :text text :width 120)
           "Deleted 1 matching line in" "1 line too long to look at left as it is"))
    (true (tuatara.frontend.fake:fake-frame-showing
           (run-keys (list (lambda () (setf (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*scratch*") :plaintext) t))
                           "C-x h M-x f l u s h - l i n e s RET d r o p RET")
                     :text text :width 120)
           "Deleted 1 matching line; 1 line too long to look at left as it is")))
  ;; a line's text is read whole however long, up to the limit, a character of two octets one character
  (let ((long (format nil "~a é ~a" (make-string 3000 :initial-element #\a) (make-string 3000 :initial-element #\b))))
    (is string= (lines-text long)
        (sorted-text "M-x k e e p - l i n e s RET a . . . b RET" (lines-text "x" long "y é b")))))

(define-test (sorting other-records)
  ;; paragraphs, separated by blank lines that stay where they were
  (is string= (format nil "alpha~%one~%~%beta~%~%gamma~%two~%")
      (sorted-text "M-x s o r t - p a r a g r a p h s RET" (format nil "gamma~%two~%~%beta~%~%alpha~%one~%")))
  ;; records and keys by regular expression: words sorted by the number after them
  (is string= "a1 b2 c3" (sorted-text "M-x s o r t - r e g e x p - f i e l d s RET [ a - z ] \\ d RET C-a C-k [ 0 - 9 ] RET" "c3 a1 b2"))
  ;; columns: the text between the columns of the selection's ends
  (is string= (lines-text "x a 9" "z b 1" "y c 5")
      (session-text (nth-value 1 (run-keys "C-f C-f C-SPC C-n C-n C-f M-x s o r t - c o l u m n s RET"
                                           :text (lines-text "z b 1" "y c 5" "x a 9") :width 100)))))

(define-test (sorting reverse-commands)
  ;; each sort has a -reverse command sorting in descending order, as the prefix argument does
  (dolist (on-disk '(nil t))
    (is string= (sorted-text "C-u M-x s o r t - l i n e s RET" *sort-sample* :on-disk on-disk)
        (sorted-text "M-x s o r t - l i n e s - r e v e r s e RET" *sort-sample* :on-disk on-disk))
    ;; the fields ones, whose prefix argument names the field: largest number first, ties reversed too
    (is string= (lines-text "Banana 12 0x1 d" "apple 12 0x2 a" "cherry 007 0x0 f" "fig 7 0xff c" "pear 3 0x10 b" "apple 1 0x3 e")
        (sorted-text "C-u 2 M-x s o r t - n u m e r i c - f i e l d s - r e v e r s e RET" *sort-sample* :on-disk on-disk))
    (is string= (lines-text "cherry 007 0x0 f" "apple 1 0x3 e" "Banana 12 0x1 d" "fig 7 0xff c" "pear 3 0x10 b" "apple 12 0x2 a")
        (sorted-text "C-u - 1 M-x s o r t - f i e l d s - r e v e r s e RET" *sort-sample* :on-disk on-disk)))
  (is string= (format nil "gamma~%two~%~%beta~%~%alpha~%one~%")
      (sorted-text "M-x s o r t - p a r a g r a p h s - r e v e r s e RET" (format nil "beta~%~%gamma~%two~%~%alpha~%one~%")))
  (let ((pages (format nil "b~%~c~%c~%~c~%a~%" (code-char 12) (code-char 12))))
    (is string= (sorted-text "C-u M-x s o r t - p a g e s RET" pages)
        (sorted-text "M-x s o r t - p a g e s - r e v e r s e RET" pages)))
  (is string= "c3 b2 a1" (sorted-text "M-x s o r t - r e g e x p - f i e l d s - r e v e r s e RET [ a - z ] \\ d RET C-a C-k [ 0 - 9 ] RET" "c3 a1 b2"))
  (is string= (lines-text "y c 5" "z b 1" "x a 9")
      (session-text (nth-value 1 (run-keys "C-f C-f C-SPC C-n C-n C-f M-x s o r t - c o l u m n s - r e v e r s e RET"
                                           :text (lines-text "z b 1" "x a 9" "y c 5") :width 100))))
  ;; without a selection, the command run is named
  (true (tuatara.frontend.fake:fake-frame-showing
         (run-keys "M-x s o r t - l i n e s - r e v e r s e RET" :text "b a" :width 100)
         "No selection: sort-lines-reverse was not run")))

(define-test (sorting large-in-background)
  ;; a region over the limit goes to disk and the background; edits elsewhere
  ;; are kept, and one undo takes the sort back
  (let* ((lines (loop for i below 3000 collect (format nil "~5,'0d ~a" (- 3000 i) (make-string 20 :initial-element #\x))))
         (text (format nil "top~%~{~a~%~}" lines))
         (tuatara.defaults::*sort-in-memory-limit* 1000)
         (tuatara.defaults::*sort-piece* 7000))
    (multiple-value-bind (f editor) (run-keys "C-n C-SPC M-> M-x s o r t - l i n e s RET" :text text :width 100)
      (declare (ignore f))
      (let* ((b (session-buffer editor)))
        (is string= (format nil "top~%~{~a~%~}" (sort (copy-list lines) #'string<)) (tuatara.buffer:buffer-string b))
        ;; its lines counted, all of the result at once, before it is taken in
        (true (tuatara.text:rope-indexed-p (tuatara.buffer:buffer-text b)))
        (tuatara.buffer:buffer-undo b)
        (is string= text (tuatara.buffer:buffer-string b)))))
  ;; while the result's lines are counted, that is what the list of jobs says,
  ;; not 99%; the mode line's one figure rises on through it
  (let ((job (tuatara.defaults::%make-sort-job :words "Sorting" :total 100)))
    (setf (car (tuatara.defaults::sort-job-progress job)) 100)
    (is string= "Sorting 99%" (tuatara.runtime:job-state-words job))
    (is = 99 (tuatara.runtime:job-percent job))
    (setf (tuatara.defaults::sort-job-counting job) (cons 50 100))
    (is string= "Sorting: counting lines 50%" (tuatara.runtime:job-state-words job))
    (is = 75 (tuatara.runtime:job-percent job)))
  ;; a region large enough to be sorted through files: sorting is the first half
  (let ((job (tuatara.defaults::%make-sort-job :words "Sorting" :total (* 2 tuatara.defaults::*sort-in-memory-limit*))))
    (setf (car (tuatara.defaults::sort-job-progress job)) tuatara.defaults::*sort-in-memory-limit*)
    (is = 25 (tuatara.runtime:job-percent job))))

(define-test (sorting safe-visits)
  ;; a file opened safely: read-only, no mode, nothing made beside it, Safe in the mode line
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "evidence.csv" dir) (lines-text "a,b" "1,2"))))
      (let ((tuatara.io:*memory-read-limit* 4))
        (multiple-value-bind (b existing) (tuatara.io:visit-file path :safe t)
          (unwind-protect
               (progn
                 (false existing)
                 (true (tuatara.io:safe-visit-p b))
                 (true (tuatara.buffer:buffer-local b :read-only))
                 (false (tuatara.buffer:buffer-local b :major-mode))
                 (fail (tuatara.buffer:buffer-insert b 0 "x"))
                 (is string= "Safe" (tuatara.defaults::safe-mode-line nil b))
                 (false (tuatara.io:snapshots))
                 (is equal '("evidence.csv") (directory-names dir))
                 ;; the scratch files of large sorts and replacements go to the
                 ;; cache, not beside the evidence, even once editing is allowed
                 (is equal (tuatara.io:cache-directory) (tuatara.io:buffer-scratch-directory b))
                 ;; reverting keeps it safe
                 (tuatara.io:revert-buffer-file b)
                 (true (tuatara.buffer:buffer-local b :read-only)))
            (tuatara.buffer:kill-buffer b))))
      ;; through the keys: C-u C-x C-f, and nothing to open safely that is not there
      (multiple-value-bind (f editor)
          (run-keys (format nil "C-u C-x C-f ~a RET" (key-string (tuatara.io:native path))) :width 140)
        (true (tuatara.frontend.fake:fake-frame-showing f "Find file safely:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Safe"))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      (fail (tuatara.io:visit-file (merge-pathnames "absent" dir) :safe t) tuatara.io:file-system-error)
      ;; a file visited the ordinary way keeps them by the rule for its folder: beside it
      ;; where its file system shares blocks
      (with-visited (b path)
        (is equal (tuatara.io:scratch-directory path :room (tuatara.buffer:buffer-size b))
            (tuatara.io:buffer-scratch-directory b))
        (when (tuatara.io::shares-blocks-p dir)
          (is equal dir (tuatara.io:buffer-scratch-directory b)))))))

(define-test (sorting line-diff)
  ;; the git gutter's own diff: each hunk (OLD-START OLD-COUNT NEW-START NEW-COUNT), a
  ;; side without lines giving the line after which the other side's go
  (flet ((hunks (old new)
           (tuatara.defaults::diff-hunks (tuatara.defaults::line-hashes (to-octets old))
                                         (tuatara.defaults::line-hashes (to-octets new)))))
    (is equal '() (hunks (lines-text "a" "b") (lines-text "a" "b")))
    (is equal '((2 1 2 1)) (hunks (lines-text "a" "b" "c") (lines-text "a" "B" "c")))       ; changed
    (is equal '((1 0 2 2)) (hunks (lines-text "a" "c") (lines-text "a" "x" "y" "c")))       ; added after line 1
    (is equal '((2 1 1 0)) (hunks (lines-text "a" "b" "c") (lines-text "a" "c")))           ; deleted after line 1
    (is equal '((1 1 1 1) (3 0 4 1)) (hunks (lines-text "a" "b" "c") (lines-text "A" "b" "c" "d")))))

(define-test (sorting lines-of-strings-compared)
  ;; two lists of lines compared as the gutter compares texts: each hunk's lines removed, then those added
  (is equal '() (tuatara:line-diff '("a" "b") '("a" "b")))
  (is equal '((:removed "b") (:added "B")) (tuatara:line-diff '("a" "b" "c") '("a" "B" "c")))
  (is equal '((:added "x") (:added "y")) (tuatara:line-diff '("a" "c") '("a" "x" "y" "c")))
  (is equal '((:removed "kā") (:added "d")) (tuatara:line-diff (vector "kā" "b" "c") (vector "b" "c" "d")))
  ;; past the limit, all between the shared ends removed and added
  (is equal '((:removed "a") (:removed "b") (:added "b") (:added "a"))
      (tuatara:line-diff '("a" "b") '("b" "a") :limit 0)))

(define-test (sorting other-window-keeps-its-line)
  ;; a window on the buffer, not the one sorting, keeps its line: its own point and first
  ;; line on the same line numbers, however the lines moved
  (let ((tuatara.defaults::*modal-editing* nil)
        (text (format nil "~{line ~d foo~%~}" (loop for i from 1 to 2000 collect (- 2001 i)))))
    (multiple-value-bind (f editor)
        (run-keys '("C-x 2 M-g g 1 5 0 0 RET C-x o C-x h M-x s o r t - l i n e s RET") :text text :width 80 :height 30)
      (declare (ignore f))
      (let* ((other (find-if-not (lambda (w) (eq w (tuatara.command:editor-window editor)))
                                 (tuatara.command:editor-windows editor)))
             (buffer (tuatara.display:window-buffer other)))
        (true (tuatara.display:window-point-anchor other))
        (is = 1499 (tuatara.buffer:buffer-line-number buffer (tuatara.display:window-point other)))))))

(define-test (sorting line-ends-kept-on-disk)
  (flet ((run (keys text crlf)
           (let ((tuatara.defaults::*sort-in-memory-limit* 0)
                 (tuatara.defaults::*sort-piece* 16))
             (session-text (nth-value 1 (run-keys (list (lambda ()
                                                          (when crlf
                                                            (setf (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*scratch*") :line-ending) :crlf)))
                                                        (format nil "C-x h ~a" keys))
                                                  :text text :width 100)))))
         (lines (separator &rest lines) (format nil (concatenate 'string "~{~a~^" separator "~}") lines)))
    (let ((crlf (coerce '(#\Return #\Newline) 'string)))
      (dolist (sep (list (string #\Newline) crlf))
        (let ((on (string= sep crlf)))
          ;; place I keeps the end of line I, the last line's none
          (is string= (lines sep "a" "b" "c") (run "M-x s o r t - l i n e s RET" (lines sep "c" "b" "a") on))
          (is string= (concatenate 'string (lines sep "c" "b" "a") sep)
              (run "M-x r e v e r s e - r e g i o n RET" (concatenate 'string (lines sep "a" "b" "c") sep) on))))
      ;; a lone newline among carriage returns and newlines stays where it was
      (is string= (format nil "a~%b~a" crlf) (run "M-x s o r t - l i n e s RET" (format nil "b~%a~a" crlf) t))
      (is string= (format nil "a~%b~a" crlf) (run "M-x r e v e r s e - r e g i o n RET" (format nil "b~%a~a" crlf) t)))))
