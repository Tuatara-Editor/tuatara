;;;; test/rewrite.lisp --- Replacements in the background: splicing, swaps, jobs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test rewrite
  :description "Replacements too large for memory: ropes spliced without copying, whole-text swaps with undo, and jobs that rewrite in the background while the buffer stays usable.")

(define-test (rewrite splice)
  (let* ((a (tuatara.text:string->rope (random-text 20000)))
         (b (tuatara.text:string->rope (random-text 3000)))
         (sa (tuatara.text:rope-string a)) (sb (tuatara.text:rope-string b)))
    (loop repeat 20
          do (let* ((i (tuatara.text:rope-char-start a (random (1+ (tuatara.text:rope-bytes a)))))
                    (j (tuatara.text:rope-char-start a (+ i (random (1+ (- (tuatara.text:rope-bytes a) i))))))
                    (spliced (tuatara.text:splice-rope a i j b b)))
               (true (tuatara.text:check-rope spliced))
               (is equalp (concatenate '(vector (unsigned-byte 8))
                                       (subseq (to-octets sa) 0 i) (to-octets sb) (to-octets sb) (subseq (to-octets sa) j))
                   (tuatara.text:rope-octets spliced))))
    (is string= "" (tuatara.text:rope-string (tuatara.text:splice-rope a 0 (tuatara.text:rope-bytes a))))))

(define-test (rewrite swap)
  ;; a whole-text swap: anchors follow the position maps; undo and redo swap back and forth
  (let ((b (tuatara.buffer:make-buffer "*swap-test*" :text "abcdef")))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-point b) 4)
           (tuatara.buffer:buffer-swap-text b (tuatara.text:string->rope "ABCDEFGH")
                                            (lambda (p bias) (declare (ignore bias)) (+ p 2))
                                            (lambda (p bias) (declare (ignore bias)) (max 0 (- p 2))))
           (is string= "ABCDEFGH" (tuatara.buffer:buffer-string b))
           (is = 6 (tuatara.buffer:buffer-point b))
           (true (tuatara.buffer:buffer-modified-p b))
           (tuatara.buffer:buffer-undo b)
           (is string= "abcdef" (tuatara.buffer:buffer-string b))
           (is = 4 (tuatara.buffer:buffer-point b))
           (tuatara.buffer:buffer-redo b)
           (is string= "ABCDEFGH" (tuatara.buffer:buffer-string b))
           (is = 6 (tuatara.buffer:buffer-point b)))
      (tuatara.buffer:kill-buffer b))))

(defmacro with-background (() &body body)
  "Run BODY with every replacement going to the background in tiny pieces and
checkpoints, so that a small text exercises all of the job's machinery."
  `(let ((tuatara.defaults::*rewrite-threshold* 0)
         (tuatara.defaults::*rewrite-piece* 40)
         (tuatara.defaults::*rewrite-checkpoint* 16))
     ,@body))

(defun replaced-text (keys text &key background)
  "The text after typing KEYS on TEXT, the replacement made in memory or in the background."
  (if background
      (with-background () (session-text (nth-value 1 (run-keys keys :text text :width 120))))
      (session-text (nth-value 1 (run-keys keys :text text :width 120)))))

(defun key-string (string)
  "STRING typed as keys: one token per character, SPC for a space."
  (format nil "~{~a~^ ~}" (map 'list (lambda (c) (if (char= c #\Space) "SPC" (string c))) string)))

(define-test (rewrite same-as-in-memory)
  ;; in the background, in pieces of 40 octets, the result is the one made in memory, and a change
  (let ((text (with-output-to-string (o)
                (loop repeat 60 do (format o "foo Foo 12 bar, FOO baz ~a~%" (random 1000))))))
    (flet ((alike (keys text)
             (let ((in-memory (replaced-text keys text)))
               (false (string= text in-memory) "~a changes nothing" keys)
               (is string= in-memory (replaced-text keys text :background t) "~a" keys))))
      ;; every match after a leading %, else the first on each line
      (dolist (typed '("%o/0" "o/0" "%foo/qux" "%[0-9]+/<&>" "%(ba)(r|z)/\\2\\1" "^foo/X" "%\\d+$/N" "%o /_"))
        (alike (format nil "r r ~a RET" (key-string typed)) text))
      ;; literal text, after M-r
      (alike (format nil "r r ~a M-r RET" (key-string "%o./!")) "go. do.. o.")
      ;; a pattern that may cross lines, cut at line starts all the same
      (alike (format nil "r r ~a RET" (key-string "%[^,]*,/;")) text))))

(define-test (rewrite seams-same-as-one-scan)
  ;; in pieces of 40 octets, a pattern whose matches run over line ends, and past a piece's
  ;; end or over several pieces, replaces what one scan finds, with g and without
  (let ((text (with-output-to-string (o)
                (loop for i below 40
                      do (format o "foo ~d bar~a  baz ~a~%" i (make-string (mod i 4) :initial-element #\Newline)
                                 (make-string (mod (* 7 i) 5) :initial-element #\Space))))))
    (dolist (typed '("%\\s+/_" "\\s+/_" "%\\n\\n+/|" "r\\s+b/-" "%\\D/k" "%r[\\s\\S]*?z/Q" "r[\\s\\S]*?z/Q"
                     "%a[\\s\\S]{0,90}?f/W" "% *\\n *\\n/=" "%$\\n^/+"))
      (let ((keys (format nil "r r ~a RET" (key-string typed))))
        (let ((in-memory (replaced-text keys text)))
          (false (string= text in-memory) "~a changes nothing" typed)
          (is string= in-memory (replaced-text keys text :background t) "~a" typed))))
    ;; and on all but two cores: a pattern that may run over a line end is cut into pieces as any other
    (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.05))) (pieces nil))
      (with-background ()
        (run-keys (list "r r % \\ D / k RET"
                        (lambda ()
                          (let ((job (tuatara.defaults::rewrite-of (tuatara.command:current-buffer))))
                            (setf pieces (length (tuatara.defaults::rewrite-pieces job)))
                            (tuatara.runtime:job-stop job))))
                  :text text :width 100 :height 8))
      (true (> pieces 10) "~a pieces" pieces))))

(define-test (rewrite case-followed-alike)
  ;; with case ignored the replacement follows each match's case, the same in memory and in the
  ;; background; a replacement that says its case (\\L, \\U) is written so in both
  (let ((text (format nil "kea KEA Kea k3a~%")))
    (dolist (case '(("%kea/bird" "bird BIRD Bird k3a") ("%k.a/bird" "bird BIRD Bird bird")
                    ("%kea/\\L&" "kea kea kea k3a") ("%kea/\\U&!" "KEA! KEA! KEA! k3a")
                    ("%\\d/m" "kea KEA Kea kma")))
      (destructuring-bind (typed expected) case
        (let ((keys (format nil "r r ~a RET" (key-string typed)))
              (expected (format nil "~a~%" expected)))
          (is string= expected (replaced-text keys text) "~a in memory" typed)
          (is string= expected (replaced-text keys text :background t) "~a in the background" typed))))))

(define-test (rewrite makes-no-garbage-for-each-match)
  ;; a replacement the match does not enter makes nothing for each match, so that a large file
  ;; replaced in the background never keeps the collector, which every thread waits on, busy:
  ;; what four times the matches cost more than the first, for each match added
  (flet ((replaced (lines)
           (let* ((text (with-output-to-string (o)
                          (loop for i below lines do (format o "2026-10-01T12:34:56Z,1402606439,The quick brown fox ~d~%" i))))
                  (consed nil))
             (let ((tuatara.defaults::*rewrite-threshold* 0))
               (multiple-value-bind (f editor)
                   (run-keys (list (lambda () (setf consed (sb-ext:get-bytes-consed)))
                                   (format nil "r r ~a RET" (key-string "%\\d/m"))
                                   (lambda () (loop while (tuatara.runtime:jobs-of (tuatara.command:current-buffer) :running t)
                                                    do (sleep 0.02)))
                                   (lambda () (setf consed (- (sb-ext:get-bytes-consed) consed))))
                             :text text :width 120)
                 (declare (ignore f))
                 (false (find-if #'digit-char-p (session-text editor)))))
             (values consed (count-if #'digit-char-p text)))))
    (multiple-value-bind (small small-matches) (replaced 20000)
      (multiple-value-bind (large large-matches) (replaced 80000)
        ;; some 160 octets a match before; none now
        (let ((each (/ (- large small) (- large-matches small-matches))))
          (true (< each 8) "~,1f octets a match" each))))))

(define-test (rewrite literal-makes-no-garbage-for-each-match)
  ;; a literal pattern, its letter case ignored and followed, as x for h over a
  ;; file of x's: the text of each match is given again as it was for the same
  ;; octets, so a great many matches make nothing each
  (flet ((replaced (lines)
           (let* ((text (with-output-to-string (o)
                          (loop for i below lines do (format o "xxxx-xx-xxTxx:xx:xxZ,xxxxxxxxxx,The quick brown fox~%"))))
                  (consed nil))
             (let ((tuatara.defaults::*rewrite-threshold* 0))
               (multiple-value-bind (f editor)
                   (run-keys (list (lambda () (setf consed (sb-ext:get-bytes-consed)))
                                   (format nil "r r ~a RET" (key-string "%x/h"))
                                   (lambda () (loop while (tuatara.runtime:jobs-of (tuatara.command:current-buffer) :running t)
                                                    do (sleep 0.02)))
                                   (lambda () (setf consed (- (sb-ext:get-bytes-consed) consed))))
                             :text text :width 120)
                 (declare (ignore f))
                 (false (find #\x (session-text editor)))))
             (values consed (count #\x text)))))
    (multiple-value-bind (small small-matches) (replaced 20000)
      (multiple-value-bind (large large-matches) (replaced 80000)
        ;; some 270 octets a match before; none now
        (let ((each (/ (- large small) (- large-matches small-matches))))
          (true (< each 8) "~,1f octets a match" each))))))

(define-test (rewrite read-only-meanwhile)
  ;; a buffer made read-only while a replacement runs in the background: the change is not forced on it, and said
  (with-background ()
    (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
      (multiple-value-bind (f editor)
          (run-keys-in-turns '("r r % o / 0 RET M-x r e a d - o n l y - m o d e RET" "" "") :text "foo boo" :width 160)
        (is string= "foo boo" (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "became read-only meanwhile"))
        (false (tuatara.frontend.fake:fake-frame-showing f "Error in")))
      ;; turned to the hex view: a replacement that would change the text's size is left out alike
      (multiple-value-bind (f editor)
          (run-keys-in-turns '("r r % o / 0 0 RET M-x h e x - m o d e RET" "" "") :text "foo boo" :width 160)
        (is string= "foo boo" (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "turned to the hex view, which only types over octets, meanwhile"))
        (false (tuatara.frontend.fake:fake-frame-showing f "Error in")))
      ;; one keeping its size is made there
      (multiple-value-bind (f editor)
          (run-keys-in-turns '("r r % o / 0 RET M-x h e x - m o d e RET" "" "") :text "foo boo" :width 160)
        (is string= "f00 b00" (session-text editor))
        (false (tuatara.frontend.fake:fake-frame-showing f "Error in"))))))

(define-test (rewrite counting-shown)
  ;; once all is written, the list of jobs says the lines of the text written
  ;; are being counted; the mode line's one figure rises through both, writing
  ;; the first half and counting the second, and stops short of 100
  (let ((job (tuatara.defaults::%make-rewrite :doing "Pasting")))
    (is string= "Pasting 100%" (tuatara.runtime:job-state-words job))
    (is = 50 (tuatara.runtime:job-percent job))
    (setf (tuatara.defaults::rewrite-counting job) (cons 50 200))
    (is string= "Pasting: counting lines 25%" (tuatara.runtime:job-state-words job))
    (is = 62 (tuatara.runtime:job-percent job))
    (setf (tuatara.defaults::rewrite-counting job) (cons 200 200))
    (is = 99 (tuatara.runtime:job-percent job))
    (setf (tuatara.defaults::rewrite-counting job) (cons 0 0))
    (is string= "Pasting: counting lines 0%" (tuatara.runtime:job-state-words job))
    (is = 50 (tuatara.runtime:job-percent job)))
  ;; a count has no lines to count: its pieces are all of it
  (is = 99 (tuatara.runtime:job-percent (tuatara.defaults::%make-rewrite :count-only t))))

(defun job-spans (spans-per-row)
  "The spans in SPANS-PER-ROW (a frame's spans, a vector of lists of (START END
FACE)) in a job's face, as (ROW START END FACE)."
  (loop for row from 0
        for spans across spans-per-row
        append (loop for (start end face) in spans
                     for job = (find-if (lambda (f) (member f '(:job-queued :job-waiting)))
                                        (if (listp face) face (list face)))
                     when job collect (list row start end job))))

(defun painted-job-spans (frontend)
  "The job spans (JOB-SPANS) of every frame FRONTEND painted, oldest first."
  (loop for (spans) in (reverse (tuatara.frontend.fake::fake-face-history frontend))
        append (job-spans spans)))

(define-test (rewrite job-shading)
  ;; the pieces of a job, followed through edits made since it began
  (let ((b (tuatara.buffer:make-buffer "*shading-test*" :text (format nil "aaaa~%bbbb~%cccc~%"))))
    (unwind-protect
         (let* ((pieces (vector (tuatara.defaults::make-rewrite-piece 0 5) (tuatara.defaults::make-rewrite-piece 5 15)))
                (job (tuatara.defaults::%make-rewrite :buffer b :base (tuatara.buffer:buffer-version b)
                                                      :rope (tuatara.buffer:buffer-text b) :low 0 :high 15 :pieces pieces)))
           (setf (tuatara.defaults::rewrite-piece-done (aref pieces 0)) 3
                 (tuatara.defaults::rewrite-piece-done (aref pieces 1)) 5)
           ;; all of it till the result is taken in, where the workers have been too: the text
           ;; shown there is still the text before the change
           (is equal '((0 5 :job-queued) (5 15 :job-queued))
               (tuatara.defaults::job-shading job 0 15))
           ;; only what is shown
           (is equal '((6 15 :job-queued)) (tuatara.defaults::job-shading job 6 20))
           ;; text typed before the job's scope since it began moves it along
           (tuatara.buffer:buffer-insert b 0 "XY")
           (is equal '((2 7 :job-queued) (7 17 :job-queued))
               (tuatara.defaults::job-shading job 0 20)))
      (tuatara.buffer:kill-buffer b)))
  ;; a replacement in the background shades what it has still to change, and nothing once
  ;; done; its pieces, run side by side, are held a little longer each, so as to finish in turn
  (let* ((started 0)
         (lock (sb-thread:make-mutex))
         (tuatara.defaults::*rewrite-piece-hook*
           (lambda () (sleep (* 0.3 (sb-thread:with-mutex (lock) (incf started))))))
         (text (format nil "~{foo ~2,'0d~%~}" (loop for i below 12 collect i))))
    (with-background ()
      (let* ((f (run-keys "r r % o / 0 RET" :text text :width 60 :height 16))
             (spans (painted-job-spans f)))
        (true (find :job-queued spans :key #'fourth))
        ;; only the matches it will change, never whole lines: here the two o of each foo
        (true (every (lambda (span) (<= (- (third span) (second span)) 2)) spans))
        (false (job-spans (car (first (tuatara.frontend.fake::fake-face-history f)))))))))

(define-test (rewrite undo-stands-where-begun)
  ;; a job's change stands in the undo history where it began: an edit made while it ran is
  ;; undone first, then the whole replacement, the part made in sight with it
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
    (with-background ()
      (flet ((undone (keys text height)
               (let ((b nil))
                 (run-keys (list (lambda () (setf b (tuatara.command:current-buffer))) keys
                                 (lambda () (loop while (tuatara.runtime:jobs-of b :running t) do (sleep 0.05)))
                                 :tick)
                           :text text :width 100 :height height)
                 (let ((steps (list (tuatara.buffer:buffer-string b))))
                   (loop repeat 2 do (tuatara.buffer:buffer-undo b) (push (tuatara.buffer:buffer-string b) steps))
                   (loop repeat 2 do (tuatara.buffer:buffer-redo b) (push (tuatara.buffer:buffer-string b) steps))
                   (nreverse steps)))))
        ;; an insertion away from the matches
        (is equal '("Xf00 b00" "f00 b00" "foo boo" "f00 b00" "Xf00 b00")
            (undone "r r % o / 0 RET i X C-g" "foo boo" 8))
        ;; an edit inside a match, kept as edited: undone, the match is as it was
        (is equal '("fo b00" "foo b00" "foo boo" "foo b00" "fo b00")
            (undone "r r % o o / 0 0 RET l d" "foo boo" 8))
        ;; a large text: what is in sight changed at once, the rest in the background, an edit
        ;; at the end meanwhile; the replacement one step, under the edit
        (let* ((text (format nil "~{foo ~d~%~}" (loop for i below 2000 collect i)))
               (done (format nil "~{f00 ~d~%~}" (loop for i below 2000 collect i)))
               (steps (undone "r r % o / 0 RET g e i Z C-g" text 10)))
          (is string= (concatenate 'string done "Z") (first steps))
          (is string= done (second steps))
          (is string= text (third steps))
          (is string= (concatenate 'string done "Z") (fifth steps)))))))

(define-test (rewrite typing-meanwhile-made-again)
  ;; several keys typed while a job runs, each its own edit, are made again by redo where they
  ;; were typed, in their order, after the job's change, and a queued job's after that: the
  ;; text the same keys make with no job in the background
  (dolist (case '(("r r % o / 0 RET i X Y Z C-g" 2)
                  ("r r % o / 0 RET g e i ! ? C-g i < > C-g" 3)
                  ("r r % o / 0 RET r r % b / c RET i Q R S C-g" 3)))
    (destructuring-bind (keys steps) case
      (let ((done (session-text (nth-value 1 (run-keys keys :text "foo boo" :width 100 :height 8))))
            (b nil)
            (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
        (with-background ()
          (flet ((ran () (loop while (tuatara.runtime:jobs-of b :running t) do (sleep 0.05))))
            (run-keys (list (lambda () (setf b (tuatara.command:current-buffer))) keys
                            #'ran :tick :tick #'ran :tick :tick)
                      :text "foo boo" :width 100 :height 8)))
        (is string= done (tuatara.buffer:buffer-string b) "~a done" keys)
        (loop repeat steps do (tuatara.buffer:buffer-undo b))
        (is string= "foo boo" (tuatara.buffer:buffer-string b) "~a undone" keys)
        (loop repeat steps do (tuatara.buffer:buffer-redo b))
        (is string= done (tuatara.buffer:buffer-string b) "~a made again" keys)))))

(define-test (rewrite redo-leaves-point)
  ;; redo of a job's change, what it changed in sight at once and the whole text swapped,
  ;; leaves point where it is, as the text moves it, not at the last place changed in sight;
  ;; redo of typing still puts point after what it types
  (let ((b (tuatara.buffer:make-buffer "*redo-test*" :text (format nil "~{line ~d~%~}" (loop for i below 200 collect i)))))
    (unwind-protect
         (let ((since (tuatara.buffer:buffer-undo-stack b)))
           (tuatara.buffer:buffer-insert b 900 "X")
           (tuatara.buffer:buffer-swap-text b (tuatara.text:string->rope (concatenate 'string (tuatara.buffer:buffer-string b) "Y"))
                                            (lambda (p bias) (declare (ignore bias)) p)
                                            (lambda (p bias) (declare (ignore bias)) p))
           (tuatara.buffer:merge-undo-groups b since)
           (tuatara.buffer:buffer-undo b)
           (setf (tuatara.buffer:buffer-point b) 3)
           (tuatara.buffer:buffer-redo b)
           (is = 3 (tuatara.buffer:buffer-point b))
           (is char= #\Y (char (tuatara.buffer:buffer-string b) (1- (length (tuatara.buffer:buffer-string b))))))
      (tuatara.buffer:kill-buffer b)))
  (let ((b (tuatara.buffer:make-buffer "*redo-test*" :text "abc")))
    (unwind-protect
         (progn (tuatara.buffer:buffer-insert b 3 "def")
                (tuatara.buffer:buffer-undo b)
                (setf (tuatara.buffer:buffer-point b) 0)
                (tuatara.buffer:buffer-redo b)
                (is = 6 (tuatara.buffer:buffer-point b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (rewrite undo-stands-where-given-queued)
  ;; one replacement running, a second waiting its turn, then an edit typed: undo takes back
  ;; the edit first, then the second replacement, then the first, the order they were given in
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
    (with-background ()
      (let ((b nil))
        (flet ((settled ()
                 (loop while (or (tuatara.runtime:jobs-of b :running t)
                                 (tuatara.defaults::queued-in b))
                       do (sleep 0.05))))
          (run-keys (list (lambda () (setf b (tuatara.command:current-buffer)))
                          "r r % o / 0 RET" "r r % b / c RET" "i X C-g"
                          (lambda () (loop while (tuatara.runtime:jobs-of b :running t) do (sleep 0.05)))
                          :tick :tick
                          (lambda () (settled))
                          :tick)
                    :text "foo boo" :width 100 :height 8))
        (let ((steps (list (tuatara.buffer:buffer-string b))))
          (loop repeat 3 do (tuatara.buffer:buffer-undo b) (push (tuatara.buffer:buffer-string b) steps))
          (loop repeat 3 do (tuatara.buffer:buffer-redo b) (push (tuatara.buffer:buffer-string b) steps))
          (is equal '("Xf00 c00" "f00 c00" "f00 b00" "foo boo" "f00 b00" "f00 c00" "Xf00 c00")
              (nreverse steps)))))))

(define-test (rewrite lit-after-edits)
  ;; while a job runs, only what it will change is lit, after an edit made meanwhile too
  ;; each piece slow enough that none is written by the frame after the edit
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 2)))
        (text (format nil "~{foo ~d bar~%~}" (loop for i from 10 below 40 collect i))))
    (with-background ()
      (multiple-value-bind (f editor)
          (run-keys (list "r r % \\ d / x RET" "g e i Z C-g" :tick) :text text :width 60 :height 12)
        (declare (ignore editor))
        (let ((spans (painted-job-spans f)))
          ;; lit, before the edit and after it, but only the two digits of each line, never the line
          (true spans)
          (true (every (lambda (span) (<= (- (third span) (second span)) 2)) spans)))))))

(define-test (rewrite lit-quickly-after-much-typing)
  ;; what a running job lights is found as quickly after thousands of keys typed meanwhile as
  ;; after a few: the stretches edited are walked, not every version since the job began
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 5)))
        (text (format nil "~{foo ~d bar ~d~%~}" (loop for i below 2000 append (list i (* 7 i)))))
        (spent nil))
    (with-background ()
      (run-keys (list "r r % \\ d / x RET"
                      (lambda ()
                        (let* ((b (tuatara.command:current-buffer))
                               (job (tuatara.defaults::rewrite-of b)))
                          (flet ((lighting ()
                                   (let ((start (get-internal-real-time)))
                                     (dotimes (i 20) (tuatara.defaults::job-shading job 0 4000))
                                     (/ (- (get-internal-real-time) start) 20))))
                            (dotimes (i 10) (tuatara.buffer:buffer-insert b 0 "a" :group t))
                            (let ((few (lighting)))
                              (dotimes (i 3000) (tuatara.buffer:buffer-insert b 0 "a" :group t))
                              (setf spent (list few (lighting))))
                            (dolist (j (tuatara.runtime:jobs-of b :running t)) (tuatara.runtime:job-stop j))))))
                :text text :width 100 :height 40))
    (destructuring-bind (few many) spent
      (true (< many (+ (* 3 few) (/ internal-time-units-per-second 200))) "~,2f ms after a few keys, ~,2f after many"
            (/ few 1000.0) (/ many 1000.0)))))

(define-test (rewrite lit-quickly-under-filters)
  ;; search filters set while a job runs hide most lines: the job lights only what the rows
  ;; show, never all that lies hidden between them, which kept the editor busy for seconds
  (let* ((path (records-file (with-output-to-string (o)
                               (format o "n,a,b~%")
                               (loop for i below 200000 do (format o "~d,~d,~d~%" i (* 7 i) (* 13 i))))))
         (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 5)))
         (tuatara.defaults::*rewrite-threshold* 0)
         (b nil) (running nil) (spent nil))
    (unwind-protect
         (run-keys (list (lambda () (setf b (tuatara.command:current-buffer)))
                         "r r % \\ d / x RET" :tick
                         (lambda () (setf spent (get-internal-real-time)))
                         "C-x / s ^ ( 5 | 1 9 9 9 9 5 ) , RET" :tick :tick
                         (lambda () (setf spent (- (get-internal-real-time) spent)
                                          running (and (tuatara.defaults::rewrite-of b) t))
                                    (dolist (j (tuatara.runtime:jobs-of b :running t)) (tuatara.runtime:job-stop j))))
                   :file path :width 100 :height 20)
      (kill-records b path))
    (true running "the job still runs")
    (true (< spent internal-time-units-per-second) "the filter and its frames took ~,2f s" (/ spent internal-time-units-per-second))))

(define-test (rewrite while-editing)
  ;; keys typed while the job runs act at once; the job's result keeps them
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
    (with-background ()
      ;; an insertion away from the matches is carried over
      (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET i X C-g" :text "foo boo" :width 100)
        (true (tuatara.frontend.fake:fake-frame-showing f "Replacing"))
        (is string= "Xf00 b00" (session-text editor)))
      ;; an edit inside a match wins over the replacement of that match
      (multiple-value-bind (f editor) (run-keys "r r % o o / 0 0 RET l d" :text "foo boo" :width 100)
        (true (tuatara.frontend.fake:fake-frame-showing f "1 kept as you edited them"))
        (is string= "fo b00" (session-text editor)))
      ;; stopping, after asking: nothing changes
      (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET M-x s t o p - j o b RET y RET" :text "foo boo" :width 100)
        (true (tuatara.frontend.fake:fake-frame-showing f "Stop replacing at"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: nothing replaced"))
        (is string= "foo boo" (session-text editor)))
      ;; killing the buffer asks first
      (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET C-x k n RET" :text "foo boo" :width 120)
        (true (tuatara.frontend.fake:fake-frame-showing f "In the background: *scratch*: Replacing" "stop it, then kill the buffer?"))
        (is string= "f00 b00" (session-text editor)))
      ;; so does leaving the editor; answering no, the job goes on and completes
      (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET C-x C-c n RET" :text "foo boo" :width 120)
        (true (tuatara.frontend.fake:fake-frame-showing f "In the background: *scratch*: Replacing" "stop it, then exit?"))
        (is string= "f00 b00" (session-text editor))))))

(define-test (rewrite in-sight-first)
  ;; a large replacement changes what is in sight on RET, the rest in the background, one undo for both
  (let ((text (format nil "~{foo ~d~%~}" (loop for i below 20000 collect i))))
    (with-background ()
      (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET" :text text :width 100 :height 10)
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
              (done (format nil "~{f00 ~d~%~}" (loop for i below 20000 collect i))))
          (true (tuatara.frontend.fake:fake-frame-showing f "f00 3"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Replaced 40,000 occurrences on 20,000 lines"))
          (is string= done (tuatara.buffer:buffer-string b))
          (tuatara.buffer:buffer-undo b)
          (is string= text (tuatara.buffer:buffer-string b)))))
    ;; stopped: what was replaced in sight stays, and the message says so
    (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
      (with-background ()
        (multiple-value-bind (f editor) (run-keys "r r % o / 0 RET M-x s t o p - j o b RET y RET" :text text :width 120 :height 10)
          (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: " "replaced in sight, the rest left as it was"))
          (is string= "f00 0" (subseq (session-text editor) 0 5)))))
    ;; the first match on each line only: the lines in sight are left to the job no second time
    (with-background ()
      (multiple-value-bind (f editor) (run-keys "% r r o / 0 RET" :text text :width 100 :height 10)
        (declare (ignore f))
        (is string= (format nil "~{f0o ~d~%~}" (loop for i below 20000 collect i)) (session-text editor))))))

(define-test (rewrite after)
  (with-background ()
    ;; one undo takes the whole replacement back, and point stays with its text
    (multiple-value-bind (f editor) (run-keys "g e r r % o / 0 0 RET" :text "foo boo" :width 100)
      (declare (ignore f))
      (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= "f0000 b0000" (tuatara.buffer:buffer-string b))
        (is = 11 (tuatara.buffer:buffer-point b))
        (tuatara.buffer:buffer-undo b)
        (is string= "foo boo" (tuatara.buffer:buffer-string b))
        (tuatara.buffer:buffer-redo b)
        (is string= "f0000 b0000" (tuatara.buffer:buffer-string b))))
    ;; a search's count of matches, made in the background
    (multiple-value-bind (f editor) (run-keys (list "C-s o" #'wait-for-search-count :tick "C-g") :text (format nil "foo~%boo~%x") :width 100)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1/4    Regex search forward: o")))))

(define-test (rewrite as-in-memory)
  ;; work done in the background ends as the same work done at once: the text, and where point is
  (flet ((outcome (keys text crlf background)
           (setf tuatara.defaults::*killed-rectangle* (list "XY" "Z"))
           (let ((text (if crlf (cl-ppcre:regex-replace-all (string #\Newline) text (format nil "~c~%" #\Return)) text)))
             (multiple-value-bind (f editor)
                 (if background
                     (let ((tuatara.defaults::*rewrite-threshold* 0) (tuatara.defaults::*rewrite-piece* 7)
                           (tuatara.defaults::*rewrite-checkpoint* 3))
                       (run-keys-in-turns (list keys) :text text :crlf crlf :width 120))
                     (run-keys-in-turns (list keys) :text text :crlf crlf :width 120))
               (declare (ignore f))
               (list (session-text editor) (session-point editor))))))
    (dolist (crlf '(nil t))
      (dolist (text (list (format nil "abcdef~%gh~%ijklmnop~%") (format nil "abcdef~%gh~%ijklmnop")))
        (dolist (keys '(;; a paste waiting for a cut before it: in at point's column as the cut leaves it
                        "C-f C-x SPC C-n C-n C-f C-f C-x r k M-> C-x r y"
                        ;; point after what was written, cleared or wrapped, as at once
                        "C-e C-x SPC C-n C-n C-x r t Q RET" "C-f C-x SPC C-n C-n C-f C-f C-x r c"
                        "C-f C-x SPC C-n C-n C-f C-f C-x r M-w C-x r y" "l m v j j l (" "m v j j l l ~" "l m v j j l y g e P"
                        ;; deleted, numbered in and numbered over, as at once
                        "C-f C-x SPC C-n C-n C-f C-f C-x r d" "C-f C-x SPC C-n C-n C-f C-f C-x r N"
                        "C-f C-x SPC C-n C-n C-f C-f M-x n u m b e r - r e c t a n g l e - l i n e s - r e p l a c e RET"
                        ;; joining, and a case change over CR LF
                        "% M-^" "C-f C-x SPC C-n C-n C-f C-f M-x u p c a s e - r e g i o n RET"))
          (is equal (outcome keys text crlf nil) (outcome keys text crlf t) "~a~:[~; (CR LF)~] on ~s" keys crlf text))))))

(define-test (rewrite split-typing-across-a-job)
  ;; typing at a split's places goes on when a job before it is taken in
  (let ((tuatara.defaults::*rewrite-piece* 8) (tuatara.defaults::*rewrite-checkpoint* 2) (tuatara.defaults::*rewrite-threshold* 0)
        (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (multiple-value-bind (f editor) (run-keys-in-turns '("r r % a / b b b RET l m v j j c Q" "W E C-g")
                                                       :text (format nil "aa x~%aa x~%aa x~%") :width 100)
      (is string= (format nil "bbbQWE x~%bbbQWE x~%bbbQWE x~%") (session-text editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "an edit away from the places")))))

(define-test (rewrite killed-while-stuck)
  ;; a job that does not stop when told (a piece taking long, its stop flag unread meanwhile): the
  ;; buffer is killed within the wait, the job left to end on its own, its text still there to read
  (let* ((tuatara.defaults::*rewrite-threshold* 0) (tuatara.defaults::*rewrite-piece* 8)
         (tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 1.5)))
         (tuatara.runtime:*stop-wait* 0.2)
         (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8))
         (took nil) (thread nil))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (let ((b (tuatara.buffer:make-buffer "*scratch*" :text (format nil "aa x~%aa x~%aa x~%"))))
      (tuatara.frontend.fake:fake-queue-keys f "r r % a / b RET")
      (tuatara.frontend.fake:fake-queue-action
       f (lambda ()
           (sleep 0.1)
           (let ((job (find b tuatara.runtime:*jobs* :key #'tuatara.runtime:job-buffer)))
             (setf thread (and job (tuatara.defaults::rewrite-thread job))))
           (let ((start (get-internal-real-time)))
             (tuatara.buffer:kill-buffer b)
             (setf took (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer b)
      (true thread "a job ran")
      (true (and took (< took 1)) "killed in ~a s" took)
      (false (tuatara.buffer:live-buffer-p b))
      (true (tuatara.buffer:buffer-lent-p b))
      ;; and the job ends by itself, without an error
      (when thread
        (true (tuatara.runtime:join-within thread 10))))))
