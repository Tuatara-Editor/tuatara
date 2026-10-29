;;;; test/counting-lines.lisp --- A large file opens at once, its lines counted in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test counting-lines
  :description "A file larger than *index-at-open-limit* opens before its lines are counted: they are counted in the background, taking what was counted before; what needs them waits, and the screen shows without them.")

(defun sample-lines (n)
  "N lines of text with characters of every width, for counting."
  (with-output-to-string (out)
    (dotimes (i n)
      (format out "~d é 中 😀 line~%" i))))

(defun sample-records (n)
  "N lines of comma-separated fields, mostly ASCII as data is, every hundredth
with an accented letter."
  (with-output-to-string (out)
    (dotimes (i n)
      (format out "~d,2026-09-28,~:[plain~;café~],~d~%" i (zerop (mod i 100)) (* i 7)))))

(define-test (counting-lines donor-and-stopping)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) (sample-records 160000))))
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (declare (ignore mapping))
        (false (tuatara.text:rope-indexed-p rope))
        ;; stopped before it begins: nothing
        (false (tuatara.text:index-rope rope :cancel (list t)))
        (let* ((counted (tuatara.text:index-rope rope))
               ;; the uncounted text changed in two places, at line starts, as an undo back to it might
               (a (tuatara.text:line->byte counted 5))
               (c (tuatara.text:line->byte counted 10000))
               (edited (tuatara.text:splice-rope
                        (tuatara.text:splice-rope rope c c (tuatara.text:string->rope (format nil "new~%")))
                        a (+ a 2) (tuatara.text:string->rope "XY")))
               (progress (cons 0 0))
               (again (tuatara.text:index-rope edited :donor counted :progress progress))
               (fresh (tuatara.text:index-rope edited)))
          (true (tuatara.text:rope-indexed-p again))
          ;; the same counts as counting it all afresh
          (is = (tuatara.text:rope-lines fresh) (tuatara.text:rope-lines again))
          (is = (tuatara.text:rope-chars fresh) (tuatara.text:rope-chars again))
          (is = (tuatara.text:rope-utf16 fresh) (tuatara.text:rope-utf16 again))
          (loop for byte from 0 below (tuatara.text:rope-bytes edited) by 4099
                do (is = (tuatara.text:byte->line fresh byte) (tuatara.text:byte->line again byte))
                   (is = (tuatara.text:byte->char fresh byte) (tuatara.text:byte->char again byte)))
          ;; what the donor had counted was not read again: the pieces at the changes, a tenth of it at most
          (let ((afresh (cons 0 0)))
            (tuatara.text:index-rope edited :progress afresh)
            (true (< (cdr progress) (floor (cdr afresh) 10)) "~:d octets read again, ~:d afresh" (cdr progress) (cdr afresh)))
          (is = (cdr progress) (car progress)))))))

(defun uncounted-sample-octets (random)
  "Octets for editing a text not counted yet: lines of characters of every
width, with now and then a raw byte or the unfinished start of a sequence, as
a file of evidence may hold, drawn by RANDOM, a random state."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dotimes (i 20000)
      (loop for octet across (tuatara.text:string-octets (format nil "~d é 中 😀 line~%" i))
            do (vector-push-extend octet out))
      (when (zerop (random 40 random))
        (vector-push-extend (elt '(#xff #xc3 #x80 #xe4) (random 4 random)) out)))
    (coerce out 'tuatara.text:octets)))

(defun random-boundary (rope random)
  "A character boundary of ROPE drawn by RANDOM."
  (loop for byte from (random (1+ (tuatara.text:rope-bytes rope)) random)
        when (tuatara.text:char-boundary-p rope byte) return byte))

(defun edited-octets (octets edits)
  "OCTETS with EDITS (sorted, apart) made, the plain way: the model a rope's
edits are checked against."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (at 0))
    (flet ((put (piece) (loop for octet across piece do (vector-push-extend octet out))))
      (dolist (e edits)
        (put (subseq octets at (tuatara.text:edit-start e)))
        (when (tuatara.text:edit-insert e) (put (tuatara.text:edit-insert e)))
        (setf at (tuatara.text:edit-end e)))
      (put (subseq octets at)))
    (coerce out 'tuatara.text:octets)))

(defun random-edits (rope random)
  "One to six edits of ROPE at character boundaries, sorted and apart, drawn by
RANDOM: deletions, insertions of characters of every width, or both."
  (let ((points (sort (remove-duplicates (loop repeat (1+ (random 6 random)) collect (random-boundary rope random))) #'<))
        (reach -1) (edits '()))
    (loop for (start next) on points
          for end = (if (and next (zerop (random 2 random))) next start)
          for insert = (and (plusp (random 3 random))
                            (tuatara.text:string-octets (elt (list "x" "ü" "中" (format nil "new~%") "😀") (random 5 random))))
          do (when (and (>= start reach) (or (< start end) insert))
               (push (tuatara.text:make-edit start end insert) edits)
               (setf reach end)))
    (nreverse edits)))

(define-test (counting-lines edited-before-counted)
  ;; a text not counted yet takes edits at once, its untouched octets left uncounted; counted
  ;; afterwards, afresh or from what was counted before, it has the octets and the counts of
  ;; the text as edited, raw bytes and all
  (with-test-directory (dir)
    (let* ((random (sb-ext:seed-random-state 1414))
           (octets (uncounted-sample-octets random))
           (path (merge-pathnames "uncounted.txt" dir)))
      (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope (namestring path))
        (declare (ignore mapping))
        (let ((model octets) (edited rope) (donor (tuatara.text:index-rope rope)))
          (dotimes (round 25)
            (let ((edits (random-edits edited random)))
              (when edits
                (setf edited (tuatara.text:apply-edits edited (mapcar (lambda (e) (tuatara.text:make-edit (tuatara.text:edit-start e) (tuatara.text:edit-end e) (tuatara.text:edit-insert e))) edits))
                      model (edited-octets model edits)))))
          (false (tuatara.text:rope-indexed-p edited))
          (true (tuatara.text:check-rope edited))
          (true (equalp model (tuatara.text:rope-octets edited)))
          (let ((plain (tuatara.text:octets->rope model)))
            (dolist (counted (list (tuatara.text:index-rope edited) (tuatara.text:index-rope edited :donor donor)))
              (true (tuatara.text:check-rope counted))
              (true (equalp model (tuatara.text:rope-octets counted)))
              (is = (tuatara.text:rope-lines plain) (tuatara.text:rope-lines counted))
              (is = (tuatara.text:rope-chars plain) (tuatara.text:rope-chars counted))
              (is = (tuatara.text:rope-utf16 plain) (tuatara.text:rope-utf16 counted))
              (loop for byte from 0 below (length model) by 997
                    do (is = (tuatara.text:byte->line plain byte) (tuatara.text:byte->line counted byte))
                       (is = (tuatara.text:byte->char plain byte) (tuatara.text:byte->char counted byte))))))))))

(define-test (counting-lines characters-without-counts)
  ;; counted by reading, across the 64 KB blocks, the same as the counts say
  (let* ((text (sample-lines 8000))
         (rope (tuatara.text:string->rope text)))
    (dolist (range (list (list 0 (tuatara.text:rope-bytes rope)) (list 65530 65550) (list 1000 170000)))
      (destructuring-bind (start end) range
        (let ((start (loop for b from start when (tuatara.text:char-boundary-p rope b) return b))
              (end (loop for b from end when (tuatara.text:char-boundary-p rope b) return b)))
          (is = (- (tuatara.text:byte->char rope end) (tuatara.text:byte->char rope start))
              (tuatara.text:rope-count-chars rope start end)))))))

(defun counted-in-time (buffer &optional (seconds 30))
  "Take in the countings that end (TUATARA.IO:FINISH-INDEXING), the editor's
session over, until BUFFER's lines are counted; true when they are within
SECONDS."
  (loop repeat (* seconds 50)
        do (tuatara.io:finish-indexing)
        until (tuatara.buffer:buffer-indexed-p buffer)
        do (sleep 0.02))
  (tuatara.buffer:buffer-indexed-p buffer))

(defmacro with-lines-counted-slowly ((&key (pause 0.3)) &body body)
  "Run BODY with every visited file mapped and opened before its lines are
counted, as a file of gigabytes is, each counting held PAUSE seconds before it
begins."
  `(let ((tuatara.io::*memory-read-limit* 0)
         (tuatara.io:*index-at-open-limit* 0)
         (tuatara.io:*index-job-hook* (lambda () (sleep ,pause))))
     ,@body))

(define-test (counting-lines failing)
  ;; a counting that fails is started once on the same text, not again and again, and a command
  ;; waiting for it says why rather than wait for ever
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 5000))))
          (started 0))
      (let ((tuatara.io::*memory-read-limit* 0)
            (tuatara.io:*index-at-open-limit* 0)
            (tuatara.buffer:*line-scan-limit* 100)
            (tuatara.io:*index-job-hook* (lambda () (incf started) (error "the disk went away"))))
        (multiple-value-bind (f editor) (run-keys "M-g g 4 2 RET" :file path :width 120 :height 12)
          (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            (true (tuatara.frontend.fake:fake-frame-showing f "could not be counted: the disk went away"))
            (true (<= started 2) "started ~d times" started)
            (tuatara.buffer:kill-buffer b)))))))

(define-test (counting-lines in-the-editor)
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 5000)))))
      (unwind-protect
           (with-lines-counted-slowly ()
             ;; it shows at once, L… in the mode line and the counting's progress, then the line
             (multiple-value-bind (f editor) (run-keys "g e" :file path :width 100 :height 12)
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "L…"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Counting lines"))
                 (true (tuatara.buffer:buffer-indexed-p b))
                 (true (tuatara.frontend.fake:fake-frame-showing f "L5,001"))
                 (tuatara.buffer:kill-buffer b)))
             ;; going to a line near the start does not wait for the counting: the lines are
             ;; counted from the start there and then
             (multiple-value-bind (f editor) (run-keys "M-g g 4 2 RET" :file path :width 100 :height 12)
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                 (is = 41 (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))
                 (tuatara.buffer:kill-buffer b)))
             ;; one further in than *line-scan-limit* waits for the counting, and gets there
             (let ((tuatara.buffer:*line-scan-limit* 100))
               (multiple-value-bind (f editor) (run-keys "M-g g 4 2 RET" :file path :width 100 :height 12)
                 (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (true (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt up to line 42"))
                   (is = 41 (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))
                   (tuatara.buffer:kill-buffer b))))
             ;; a rectangle of no great size counts its own rows: m v j j y copies three
             (multiple-value-bind (f editor) (run-keys "m v j j y" :file path :width 100 :height 12)
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Copied a rectangle of 3 rows"))
                 (tuatara.buffer:kill-buffer b)))
             ;; an edit does not wait for the counting; once it is counted again around the
             ;; edits, the counts come out right, and undo still works
             (multiple-value-bind (f editor) (run-keys "i X Y SPC C-g j j o n e w C-g" :file path :width 100 :height 12)
               (let* ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                      (text (tuatara.buffer:buffer-string b)))
                 (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                 (true (counted-in-time b))
                 (is = (count #\Newline text) (tuatara.text:rope-lines (tuatara.buffer:buffer-text b)))
                 (is string= "XY 0 é 中 😀 line" (subseq text 0 (position #\Newline text)))
                 (true (tuatara.buffer:buffer-modified-p b))
                 (loop repeat 3 while (tuatara.buffer:buffer-undo b))
                 (is string= (sample-lines 5000) (tuatara.buffer:buffer-string b))
                 (false (tuatara.buffer:buffer-modified-p b))
                 (tuatara.buffer:kill-buffer b)))
             ;; moving, selecting and copying do not wait for the counting: j and k find the
             ;; lines by their newlines, V j y copies two of them
             (multiple-value-bind (f editor) (run-keys "j j k J V j y" :file path :width 100 :height 12)
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "L…"))
                 (let ((lines (uiop:split-string (sample-lines 5000) :separator (string #\Newline))))
                   (is string= (format nil "~a~%~a~%" (nth 2 lines) (nth 3 lines))
                       (first (tuatara.command:editor-kill-ring editor))))
                 (tuatara.buffer:kill-buffer b)))
             ;; nor does a copy too large for the system clipboard: the text not yet counted is
             ;; kept as it is, and a buffer it is pasted into counts it in the background
             (let ((tuatara-user:*clipboard-max-size* 8)
                   (tuatara.buffer:*large-edit-octets* 10))
               (multiple-value-bind (f editor) (run-keys "C-x h M-w C-x b n e w RET C-y" :file path :width 100 :height 12)
                 (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                   (is string= "new" (tuatara.buffer:buffer-name b))
                   (true (tuatara.buffer:buffer-indexed-p b))
                   (is string= (sample-lines 5000) (tuatara.buffer:buffer-string b))
                   (tuatara.buffer:kill-buffer b)))
               (kill-buffers-visiting dir))
             ;; killing the buffer while it counts stops the counting
             (multiple-value-bind (f editor) (run-keys "C-x k" :file path :width 100 :height 12)
               (declare (ignore f editor))
               (false (tuatara.io::index-jobs))))
        (kill-buffers-visiting dir)))))

(defun session-until (path keys done &key (seconds 30))
  "Run an editor session visiting PATH with KEYS typed, then going on until
DONE, a function of no arguments, is true or SECONDS pass; return the frontend
and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 12))
        (start (get-internal-real-time)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f keys)
    (labels ((check ()
               (unless (or (funcall done)
                           (> (- (get-internal-real-time) start) (* seconds internal-time-units-per-second)))
                 (sleep 0.02)
                 (tuatara.frontend.fake:fake-queue-action f #'check))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(define-test (counting-lines rows-written-once-counted)
  ;; typing at a cursor on every line of a file still being counted waits for nothing: the lines
  ;; out of sight are written by a job queued until the lines are counted, then on its own
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 5000)))))
      (unwind-protect
           (with-lines-counted-slowly (:pause 1)
             ;; few cursors held, so that most lines are written out of sight
             (let ((buffer nil) (queued nil) (tuatara-user:*cursors-limit* 100))
               (multiple-value-bind (f editor)
                   (session-until path "% t i X C-g"
                                  (lambda ()
                                    (setf buffer (tuatara.display:window-buffer
                                                  (tuatara.command:editor-window tuatara.command:*editor*)))
                                    (when tuatara.defaults::*job-queue* (setf queued t))
                                    ;; the keys are taken first: this is checked after them
                                    (and (tuatara.buffer:buffer-indexed-p buffer)
                                         (null tuatara.defaults::*job-queue*)
                                         (null (tuatara.defaults::jobs-of buffer)))))
                 (declare (ignore editor))
                 (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                 (true queued)
                 (true (tuatara.buffer:buffer-indexed-p buffer))
                 (let ((lines (uiop:split-string (tuatara.buffer:buffer-string buffer) :separator (string #\Newline))))
                   (is = 5001 (length lines))
                   (true (every (lambda (line) (or (string= line "") (uiop:string-prefix-p "X" line))) lines)
                         "~s" (find-if-not (lambda (line) (or (string= line "") (uiop:string-prefix-p "X" line))) lines)))
                 (tuatara.buffer:kill-buffer buffer))))
        (kill-buffers-visiting dir)))))

(define-test (counting-lines scanning-for-a-line)
  ;; a line found by counting from the start, the line numbers not needed
  (let ((b (tuatara.buffer:make-buffer "*scan-test*" :text (format nil "a~%bb~%ccc~%"))))
    (unwind-protect
         (progn
           (is = 0 (tuatara.buffer:buffer-scan-line b 0))
           (is = 2 (tuatara.buffer:buffer-scan-line b 1))
           (is = 5 (tuatara.buffer:buffer-scan-line b 2))
           ;; the line after the last newline, and past it the last line
           (is = 9 (tuatara.buffer:buffer-scan-line b 3))
           (is = 9 (tuatara.buffer:buffer-scan-line b 10))
           ;; not further in than the limit
           (is = 2 (tuatara.buffer:buffer-scan-line b 1 :limit 3))
           (false (tuatara.buffer:buffer-scan-line b 2 :limit 3))
           (false (tuatara.buffer:buffer-scan-line b 10 :limit 3)))
      (tuatara.buffer:kill-buffer b))))

(define-test (counting-lines a-job-like-any)
  ;; the counting is a job of its buffer: listed, never lighting P, stopped quietly, and once
  ;; stopped not started again by itself on the same text
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "t.txt" dir) (sample-lines 3000)))
           (b (tuatara.buffer:make-buffer "*counting*" :text (tuatara.text:open-file-rope (namestring path)))))
      (unwind-protect
           (let ((tuatara.io:*index-job-hook* (lambda () (sleep 0.3))))
             (let ((job (tuatara.io:start-indexing b)))
               (true job)
               (is equal (list job) (tuatara.runtime:jobs-of b))
               (false (tuatara.runtime:job-shows-processing-p job))
               (is eq :quiet (tuatara.runtime:job-at-exit job))
               (tuatara.runtime:job-stop job)
               (sb-thread:join-thread (tuatara.runtime:job-thread job) :default nil)
               (tuatara.io:finish-indexing)
               (is eq :stopped (tuatara.runtime:job-status job))
               (false (tuatara.io:index-job-of b))
               (false (tuatara.buffer:buffer-indexed-p b))
               ;; asked for, it counts again
               (true (tuatara.io:start-indexing b))
               (true (counted-in-time b))))
        (tuatara.buffer:kill-buffer b)))))

(define-test (counting-lines counted-again-after-a-pause)
  ;; a large text typed in while its lines were counted is counted again once the edits
  ;; pause, not at every change, which would go through all its pieces each time
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "t.txt" dir) (sample-lines 3000)))
           (b (tuatara.buffer:make-buffer "*typed-in*" :text (tuatara.text:open-file-rope (namestring path)))))
      (unwind-protect
           (let ((tuatara.io::*index-again-octets* 0)
                 (tuatara.io::*index-again-pause* 0.4)
                 (tuatara.io:*index-job-hook* (lambda () (sleep 0.2))))
             (let ((job (tuatara.io:start-indexing b)))
               (tuatara.buffer:buffer-insert b 0 "x")
               (sb-thread:join-thread (tuatara.runtime:job-thread job) :default nil))
             (dotimes (i 3)
               (true (tuatara.io:finish-indexing))
               (false (tuatara.io:index-job-of b))
               (false (tuatara.buffer:buffer-indexed-p b))
               (tuatara.buffer:buffer-insert b 0 "x")
               (sleep 0.1))
             ;; a tick sees the last edit; the pause is counted from there
             (tuatara.io:finish-indexing)
             (sleep 0.5)
             (tuatara.io:finish-indexing)
             (true (tuatara.io:index-job-of b))
             (true (counted-in-time b))
             (is = 3001 (tuatara.buffer:buffer-line-count b)))
        (tuatara.buffer:kill-buffer b)))))

(define-test (counting-lines replacement-queued-while-counted)
  ;; a replacement asked for while the file's lines are still being counted waits its turn in
  ;; the queue, the editor answering keys meanwhile, and starts by itself once they are
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 5000)))))
      (unwind-protect
           (with-lines-counted-slowly (:pause 1)
             (with-background ()
               (multiple-value-bind (f editor) (run-keys "r r % l i n e / r o w RET" :file path :width 150)
                 (true (tuatara.frontend.fake:fake-frame-showing f "Queued, first in line: Replacing"))
                 (false (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt"))
                 (let ((text (session-text editor)))
                   (false (search "line" text))
                   (is = 5000 (count-matches "row" text))))))
        (kill-buffers-visiting dir)))))

(define-test (counting-lines column-on-a-long-line)
  ;; the column of a place far along a line not yet counted is not read for: NIL past the reach
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "t.txt" dir) (format nil "a~%~a" (make-string 5000 :initial-element #\x))))
           (b (tuatara.buffer:make-buffer "*long-line*" :text (tuatara.text:open-file-rope (namestring path)))))
      (unwind-protect
           (progn
             (false (tuatara.buffer:buffer-indexed-p b))
             (is = 4000 (tuatara.buffer:buffer-column b 4002 :most 5000))
             (false (tuatara.buffer:buffer-column b 4002 :most 1000))
             (is = 4000 (tuatara.buffer:buffer-column b 4002)))
        (tuatara.buffer:kill-buffer b)))))

(define-test (counting-lines counted-so-far)
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "t.txt" dir) (sample-lines 3000)))
           (rope (tuatara.text:open-file-rope (namestring path)))
           (tally (tuatara.text:make-tally))
           (counted (tuatara.text:index-rope rope :threads 1 :chunk 4096 :tally tally)))
      (true (> (length (tuatara.text::tally-lines tally)) 8))
      ;; counted through: every line's start and every place's line, as the counted text has them
      (loop for line from 0 to (tuatara.text:rope-lines counted) by 37
            do (is = (tuatara.text:line->byte counted line) (tuatara.text:tally-line-start tally rope line)))
      (is = (tuatara.text:rope-bytes counted)
          (tuatara.text:tally-line-start tally rope (tuatara.text:rope-lines counted)))
      (loop for line from 0 below (tuatara.text:rope-lines counted) by 41
            for byte = (tuatara.text:line->byte counted line)
            do (is = line (tuatara.text:tally-line-number tally rope byte))
               (is = line (tuatara.text:tally-line-number tally rope (tuatara.text:rope-next-boundary counted byte))))
      ;; asked in any order, down, up and far off, each answer counted from the one
      ;; before when that is nearer, as the rows of a window are
      (let ((state (sb-ext:seed-random-state 7)))
        (loop repeat 300
              for byte = (random (tuatara.text:rope-bytes counted) state)
              do (is = (tuatara.text:byte->line counted byte) (tuatara.text:tally-line-number tally rope byte))))
      ;; newlines counted where they lie, before the text is counted and after, as reading them one by one does
      (let ((text (tuatara.text:rope-octets rope))
            (state (sb-ext:seed-random-state 11)))
        (loop repeat 200
              for a = (random (tuatara.text:rope-bytes rope) state)
              for b = (+ a (random (- (tuatara.text:rope-bytes rope) a -1) state))
              do (is = (count 10 text :start a :end b) (tuatara.text:rope-count-newlines rope a b))
                 (is = (count 10 text :start a :end b) (tuatara.text:rope-count-newlines counted a b))))
      ;; counted only so far from the start: known there, not past it
      (let* ((partial (tuatara.text:make-tally))
             (reached (aref (tuatara.text::tally-ends tally) 4))
             (lines (tuatara.text:byte->line counted reached)))
        (tuatara.text::tally-fill partial (tuatara.text::tally-starts tally) (tuatara.text::tally-ends tally)
                                  (let ((lines (copy-seq (tuatara.text::tally-lines tally))))
                                    (fill lines -1 :start 5)
                                    lines))
        (is equal (list lines reached) (multiple-value-list (tuatara.text:tally-counted-lines partial)))
        (is = 3 (tuatara.text:tally-line-number partial rope (tuatara.text:line->byte counted 3)))
        (is = lines (tuatara.text:tally-line-number partial rope reached))
        (false (tuatara.text:tally-line-number partial rope (+ reached 1000)))
        (is = (tuatara.text:line->byte counted lines) (tuatara.text:tally-line-start partial rope lines))
        (false (tuatara.text:tally-line-start partial rope (1+ lines)))
        ;; a buffer whose counting has got that far knows its line numbers there
        (let* ((b (tuatara.buffer:make-buffer "*counted-so-far*" :text rope))
               (job (tuatara.io::%make-index-job b rope nil)))
          (setf (tuatara.io::index-job-tally job) partial)
          (push job tuatara.runtime:*jobs*)
          (unwind-protect
               (progn
                 (is = 3 (tuatara.io:counted-line-number b (tuatara.text:line->byte counted 3)))
                 (false (tuatara.io:counted-line-number b (+ reached 1000)))
                 (is = (tuatara.text:line->byte counted 7) (tuatara.io:counted-line-start b 7))
                 (is = lines (tuatara.io:counted-lines b)))
            (tuatara.runtime:forget-job job)
            (tuatara.buffer:kill-buffer b)))))))
