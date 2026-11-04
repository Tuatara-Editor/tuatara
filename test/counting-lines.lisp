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
        (let ((b (tuatara.buffer:make-buffer "*counted-so-far*" :text rope)))
          (setf (tuatara.buffer:buffer-local b :counted-so-far)
                (tuatara.text:carry-tally partial (tuatara.buffer:buffer-version b)))
          (unwind-protect
               (progn
                 (is = 3 (tuatara.io:counted-line-number b (tuatara.text:line->byte counted 3)))
                 (false (tuatara.io:counted-line-number b (+ reached 1000)))
                 (is = (tuatara.text:line->byte counted 7) (tuatara.io:counted-line-start b 7))
                 (is = lines (tuatara.io:counted-lines b)))
            (tuatara.buffer:kill-buffer b)))))))

(defun mixed-line-ends (n)
  "N lines of plain text ending in a newline, a carriage return and newline, or
a carriage return alone, in turn."
  (with-output-to-string (out)
    (dotimes (i n)
      (format out "line ~d~a" i (case (mod i 3) (0 (string #\Newline)) (1 (format nil "~c~c" #\Return #\Newline))
                                   (t (string #\Return)))))))

(define-test (counting-lines carried-through-edits)
  ;; a count of the text as opened gives the line numbers of the text as edited since, exactly,
  ;; line ends typed, deleted, split and joined (a carriage return meeting a newline) included
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "t.txt" dir) (mixed-line-ends 3000)))
           (rope (tuatara.text:open-file-rope (namestring path)))
           (b (tuatara.buffer:make-buffer "*carried*" :text rope))
           (tally (tuatara.text:make-tally))
           (state (sb-ext:seed-random-state 1919))
           (pieces (list "x" (string #\Newline) (string #\Return) (format nil "~c~c" #\Return #\Newline)
                         (format nil "ab~%cd") "")))
      (tuatara.text:index-rope rope :threads 1 :chunk 4096 :tally tally)
      (unwind-protect
           (let ((carried (tuatara.text:carry-tally tally (tuatara.buffer:buffer-version b))))
             (dotimes (round 40)
               ;; a few edits, mostly near one another as typing is, now and then far off
               (dotimes (i (1+ (random 4 state)))
                 (let* ((size (tuatara.buffer:buffer-size b))
                        (start (if (zerop (random 5 state)) (random size state) (min size (+ 20000 (random 40 state)))))
                        (end (min size (+ start (random 3 state)))))
                   (tuatara.buffer:buffer-replace b start end (elt pieces (random (length pieces) state)))))
               (let* ((version (tuatara.buffer:buffer-version b))
                      (counted (tuatara.text:index-rope (tuatara.buffer:buffer-text b)))
                      (size (tuatara.text:rope-bytes counted)))
                 (dotimes (i 25)
                   (let ((byte (if (evenp i) (random (1+ size) state) (min size (+ 19990 (random 80 state))))))
                     (is = (tuatara.text:byte->line counted byte)
                         (tuatara.text:carried-line-number carried version byte)
                         "line at ~d after round ~d" byte round)))
                 (dotimes (i 25)
                   (let ((line (if (evenp i)
                                   (random (1+ (tuatara.text:rope-lines counted)) state)
                                   (+ (tuatara.text:byte->line counted 19990) (random 8 state)))))
                     (is = (tuatara.text:line->byte counted line) (tuatara.text:carried-line-start carried version line)
                         "start of line ~d after round ~d" line round)))
                 (is equal (list (tuatara.text:rope-lines counted) size)
                     (multiple-value-list (tuatara.text:carried-counted-lines carried version))))))
        (tuatara.buffer:kill-buffer b)))))

(defun counted-carried (text)
  "A buffer holding TEXT as a file opened, and a whole count of its lines ready
to be carried (TUATARA.TEXT:CARRY-TALLY), as two values."
  (let* ((dir (ensure-directories-exist
               (merge-pathnames (format nil "carried-~36r/" (random (expt 36 8) (make-random-state t)))
                                (tuatara.io:cache-directory))))
         (path (put-file (merge-pathnames "t.txt" dir) text))
         (rope (tuatara.text:open-file-rope (namestring path)))
         (tally (tuatara.text:make-tally)))
    (tuatara.text:index-rope rope :threads 1 :chunk 4096 :tally tally)
    (let ((b (tuatara.buffer:make-buffer (format nil "*carried-~a*" (namestring dir)) :text rope)))
      (setf (tuatara.buffer:buffer-local b 'scratch-folder) dir)
      (values b (tuatara.text:carry-tally tally (tuatara.buffer:buffer-version b))))))

(defun forget-counted-carried (buffer)
  "Kill BUFFER, made by COUNTED-CARRIED, and remove its folder."
  (let ((dir (tuatara.buffer:buffer-local buffer 'scratch-folder)))
    (tuatara.buffer:kill-buffer buffer)
    (uiop:delete-directory-tree dir :validate (lambda (p) (search "/carried-" (namestring p))))))

(define-test (counting-lines carried-count-lost)
  ;; a count that cannot be carried gives no line numbers rather than wrong ones: more changed than
  ;; *carried-count-octets*, the whole text swapped, a version not after the one it was carried to
  (multiple-value-bind (b carried) (counted-carried (mixed-line-ends 3000))
    (unwind-protect
         (let ((tuatara.text:*carried-count-octets* 4096))
           (tuatara.buffer:buffer-replace b 100 105 "x")
           (is = (tuatara.text:byte->line (tuatara.text:index-rope (tuatara.buffer:buffer-text b)) 20000)
               (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 20000))
           (tuatara.buffer:buffer-replace b 1000 9000 (make-string 10 :initial-element #\y))
           (true (tuatara.text:carried-lost-p carried (tuatara.buffer:buffer-version b)))
           (false (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 20000))
           (false (tuatara.text:carried-line-start carried (tuatara.buffer:buffer-version b) 10))
           (false (tuatara.text:carried-counted-lines carried (tuatara.buffer:buffer-version b))))
      (forget-counted-carried b)))
  (multiple-value-bind (b carried) (counted-carried (mixed-line-ends 3000))
    (unwind-protect
         (progn
           (tuatara.buffer:buffer-swap-text b (tuatara.text:string->rope "swapped")
                                            (lambda (position bias) (declare (ignore bias)) (min position 7))
                                            (lambda (position bias) (declare (ignore bias)) position))
           (true (tuatara.text:carried-lost-p carried (tuatara.buffer:buffer-version b)))
           (false (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 3)))
      (forget-counted-carried b)))
  (multiple-value-bind (b carried) (counted-carried (mixed-line-ends 300))
    (unwind-protect
         (let ((other (tuatara.buffer:make-buffer "*not-after*" :text "a
b")))
           (unwind-protect
                (progn (true (tuatara.text:carried-lost-p carried (tuatara.buffer:buffer-version other)))
                       (false (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 3)))
             (tuatara.buffer:kill-buffer other)))
      (forget-counted-carried b))))

(define-test (counting-lines carried-count-reads-only-what-changed)
  ;; after many changes far apart (a replacement through the file), a key typed at one of them
  ;; reads that one again, not all of them
  (multiple-value-bind (b carried) (counted-carried (mixed-line-ends 30000))
    (unwind-protect
         (let ((read 0)
               (count (fdefinition 'tuatara.text:rope-count-newlines)))
           (loop with piece = (format nil "~{~a~}" (loop repeat 30 collect (format nil "replaced~%")))
                 for at from 300000 above 1000 by 1500
                 do (tuatara.buffer:buffer-replace b at (+ at 300) piece))
           (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 5000)
           (true (> (length (tuatara.text::carried-tally-windows carried)) 150))
           (tuatara.buffer:buffer-insert b 2500 "q")
           (with-functions-replaced ((tuatara.text:rope-count-newlines
                                      (lambda (rope start end)
                                        (incf read (- end start))
                                        (funcall count rope start end))))
             (is = (tuatara.text:byte->line (tuatara.text:index-rope (tuatara.buffer:buffer-text b)) 320000)
                 (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 320000)))
           ;; the window typed in, and a segment of the count: not the hundred thousand octets of them all
           (true (< read 6000) "~d octets read again" read))
      (forget-counted-carried b))))

(define-test (counting-lines carried-count-holds-no-old-text)
  ;; a count carried to a version does not keep that version's text once the buffer has moved on
  (multiple-value-bind (b carried) (counted-carried (mixed-line-ends 3000))
    (unwind-protect
         (progn
           (tuatara.buffer:buffer-insert b 10 "a")
           (let ((pointer (sb-ext:make-weak-pointer (tuatara.buffer:buffer-version b))))
             (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 20)
             (dotimes (i 3) (tuatara.buffer:buffer-insert b 10 "b"))
             (sb-ext:gc :full t)
             (false (sb-ext:weak-pointer-value pointer))
             (is = (tuatara.text:byte->line (tuatara.text:index-rope (tuatara.buffer:buffer-text b)) 500)
                 (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 500))))
      (forget-counted-carried b))))

(define-test (counting-lines new-count-serves-when-the-kept-one-is-lost)
  ;; a whole count kept while a new one runs, lost (the text swapped), gives way to the new one
  (multiple-value-bind (b carried) (counted-carried (mixed-line-ends 3000))
    (unwind-protect
         (multiple-value-bind (other running) (counted-carried (mixed-line-ends 3000))
           (let ((job (tuatara.io::%make-index-job b (tuatara.buffer:buffer-text b) nil)))
             (setf (tuatara.io::index-job-carried job)
                   (tuatara.text:carry-tally (tuatara.text::carried-tally-tally running)
                                             (tuatara.buffer:buffer-version b))
                   (tuatara.buffer:buffer-local b :counted-so-far) carried)
             (push job tuatara.runtime:*jobs*)
             (unwind-protect
                  (progn
                    ;; lost: carried to a version of another text
                    (setf (tuatara.text::carried-tally-lost carried) :lost)
                    (is = (tuatara.text:byte->line (tuatara.text:index-rope (tuatara.buffer:buffer-text b)) 700)
                        (tuatara.io:counted-line-number b 700))
                    (is eq (tuatara.io::index-job-carried job) (tuatara.buffer:buffer-local b :counted-so-far)))
               (tuatara.runtime:forget-job job)
               (forget-counted-carried other))))
      (forget-counted-carried b))))

(define-test (counting-lines line-number-shown-while-typed-and-counted)
  ;; typed in while its lines are counted, a large file keeps its line number in the mode line
  ;; once the count has passed point, and it is right; nothing waits for the count to be made again
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 5000))))
          (rows nil))
      (unwind-protect
           (with-lines-counted-slowly ()
             (let ((tuatara.io::*index-again-octets* 0)
                   (tuatara.io::*index-again-pause* 3))
               (multiple-value-bind (f editor)
                   (run-keys (list "i X RET Y"
                                   (lambda ()
                                     (loop for job = (tuatara.io:index-job-of (tuatara.command:current-buffer))
                                           while (and job (eq :running (tuatara.io:index-job-status job)))
                                           do (sleep 0.02)))
                                   "Z"
                                   (lambda ()
                                     (let ((f (tuatara.command:editor-frontend tuatara.command::*editor*)))
                                       (setf rows (loop for row below 12
                                                        collect (tuatara.frontend.fake:fake-screen-line f row))))))
                             :file path :width 100 :height 12)
                 (declare (ignore f))
                 (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (true (some (lambda (row) (search "L2 C2" row)) rows) "~{~a~%~}" rows)
                   (false (some (lambda (row) (search "L…" row)) rows))
                   (true (counted-in-time b))
                   (is = 1 (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))
                   (tuatara.buffer:kill-buffer b)))))
        (kill-buffers-visiting dir)))))

(defmacro with-every-file-large (() &body body)
  "Run BODY with every mapped file counted as large (*LARGE-MAPPING-OCTETS*),
on every thread, the background's too."
  `(let ((was tuatara.text:*large-mapping-octets*))
     (unwind-protect (progn (setf tuatara.text:*large-mapping-octets* 0) ,@body)
       (setf tuatara.text:*large-mapping-octets* was))))

(defun pages-held (path &optional (start 0) end)
  "Two values: how many pages of the file at PATH, of those from octet START
below END (its end when not given), the system holds in memory, and how many
there are."
  (let ((mapping (tuatara.text:map-file path)))
    (unwind-protect
         (let* ((page (sb-posix:getpagesize))
                (from (* page (floor start page)))
                (pages (tuatara.text::pages-in-memory (sb-sys:sap+ (tuatara.text:mapping-sap mapping) from)
                                                      (- (or end (tuatara.text:mapping-length mapping)) from))))
           (values (count 1 pages) (length pages)))
      (tuatara.text:close-mapping mapping))))

(defun pages-held-inside (path)
  "PAGES-HELD of the file at PATH between the ends that a pass reading it
through keeps in memory (TUATARA.TEXT::+KEPT-ENDS+)."
  (let ((size (sb-posix:stat-size (sb-posix:stat path))))
    (pages-held path tuatara.text::+kept-ends+ (- size tuatara.text::+kept-ends+))))

(defun let-go-of-file (path)
  "Have the system write the file at PATH out and let go of its pages; true
when it holds none of them afterwards, which a file system in memory never
does."
  (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
    (unwind-protect (progn (sb-posix:fsync fd)
                           (tuatara.text:drop-cached fd 0 (sb-posix:stat-size (sb-posix:fstat fd))))
      (sb-posix:close fd)))
  (zerop (pages-held path)))

(define-test (counting-lines memory-left-as-found)
  ;; counting a large file's lines reads it through once: the pages the system did not hold
  ;; before are let go of again, so that a file larger than the memory does not push out what is
  ;; in use, the editor's own memory included; those it held, it holds still
  (with-every-file-large ()
    (with-test-directory (dir)
      (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 1200000)))))
        (flet ((count-lines ()
                 (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
                   (true (tuatara.text:rope-indexed-p (tuatara.text:index-rope rope :threads 4 :chunk 262144)))
                   (tuatara.text:close-mapping mapping))))
          (if (let-go-of-file path)
              (progn
                (count-lines)
                (multiple-value-bind (held all) (pages-held-inside path)
                  (true (< held (/ all 20)) "~d of ~d pages held after counting" held all))
                (alexandria:read-file-into-byte-vector path)
                (count-lines)
                (multiple-value-bind (held all) (pages-held path)
                  (is = all held)))
              (skip "this file system holds its files in memory" (true nil))))))))

(defun shown-start (editor)
  "Where the rows of EDITOR's window begin, as an octet of its buffer."
  (tuatara.display:row-start (aref (tuatara.display:window-rows (tuatara.command:editor-window editor)) 0)))

(define-test (counting-lines read-ahead-where-the-window-goes)
  ;; what a window shows of a large file, and the stretch past it the way the window moves, is
  ;; read in ahead on a thread of its own, asked for again only near the stretch's edge: paging
  ;; down from the top, the stretch reaches on past the rows; paging up from the end, back before
  ;; them; and the system holds it once asked
  (with-every-file-large ()
    (with-test-directory (dir)
      (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 400000))))
            (asked '())
            (tuatara.io::*memory-read-limit* 0))
        (sb-int:encapsulate 'tuatara.text::want-read 'asked
                            (lambda (function mapping start end)
                              (push (list start end) asked)
                              (funcall function mapping start end)))
        (unwind-protect
             (flet ((pages (way) (format nil "~{~a~^ ~}" (make-list 30 :initial-element way))))
               (let-go-of-file path)
               (multiple-value-bind (f editor) (run-keys (pages "C-v") :file path :width 80 :height 20)
                 (declare (ignore f))
                 (let ((at (shown-start editor)))
                   (true (< 0 (length asked) 4) "asked ~d times for 30 pages down" (length asked))
                   (destructuring-bind (start end) (first asked)
                     (true (<= start at))
                     (true (>= end (+ at (* 200 1024))) "read ahead to ~d, shown from ~d" end at)
                     (true (loop repeat 100
                                 thereis (multiple-value-bind (held all) (pages-held path start end) (= held all))
                                 do (sleep 0.02))
                           "the stretch asked for read in"))
                   (tuatara.buffer:kill-buffer (session-buffer editor))))
               (setf asked '())
               (multiple-value-bind (f editor)
                   (run-keys (concatenate 'string "g e " (pages "M-v")) :file path :width 80 :height 20)
                 (declare (ignore f))
                 (let ((at (shown-start editor)))
                   (true (< 0 (length asked) 5) "asked ~d times for 30 pages up from the end" (length asked))
                   (destructuring-bind (start end) (first asked)
                     (true (<= start (- at (* 200 1024))) "read ahead from ~d, shown from ~d" start at)
                     (true (> end at)))
                   (tuatara.buffer:kill-buffer (session-buffer editor)))))
          (sb-int:unencapsulate 'tuatara.text::want-read 'asked))))))

(define-test (counting-lines scrolling-reads-near-the-window)
  ;; paging through a large file whose lines are counted reads what is shown and what is read
  ;; ahead of it, never the rest of the file: a bound on the pages brought into memory for each
  ;; page of text scrolled, beyond what opening the file read (its head and its end, where its
  ;; settings may stand); the looks through a whole file of a size that has them, for conflicts
  ;; and against git, left out
  (with-every-file-large ()
    (with-test-directory (dir)
      (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 800000))))
            (tuatara.io::*memory-read-limit* 0)
            (tuatara.defaults::*conflict-markers-max-bytes* 0)
            (tuatara.defaults::*vc-gutter-max-bytes* 0))
        (flet ((paged (n)
                 ;; the pages held after paging down N times, and the octets paged through
                 (let-go-of-file path)
                 (multiple-value-bind (f editor)
                     (run-keys (format nil "~{~a~^ ~}" (make-list n :initial-element "C-v"))
                               :file path :width 80 :height 20)
                   (declare (ignore f))
                   (multiple-value-prog1 (values (pages-held-inside path) (shown-start editor))
                     (tuatara.buffer:kill-buffer (session-buffer editor))))))
          (if (let-go-of-file path)
              (let ((opened (paged 0)))
                (multiple-value-bind (held at) (paged 40)
                  (multiple-value-bind (all-held all) (pages-held-inside path)
                    (declare (ignore all-held))
                    ;; the rows paged through, 256 KB read ahead, and what the system reads around a page
                    (true (< (- held opened) (+ (ceiling at 4096) 64 128))
                          "~d pages held more after paging through ~d octets" (- held opened) at)
                    (true (< held (/ all 4)) "~d of ~d pages held" held all))))
              (skip "this file system holds its files in memory" (true nil))))))))

(defun reading-ahead-garbage (keys path)
  "The octets READ-AHEAD made while KEYS were typed on the file at PATH, and
how many times it was called, as two values."
  (let ((octets 0) (calls 0))
    (sb-int:encapsulate 'tuatara.text:read-ahead 'garbage
                        (lambda (function &rest arguments)
                          (incf calls)
                          (let ((before (sb-ext:get-bytes-consed)))
                            (multiple-value-prog1 (apply function arguments)
                              (incf octets (- (sb-ext:get-bytes-consed) before))))))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys keys :file path :width 80 :height 20)
           (declare (ignore f))
           (tuatara.buffer:kill-buffer (session-buffer editor)))
      (sb-int:unencapsulate 'tuatara.text:read-ahead 'garbage))
    (values octets calls)))

(define-test (counting-lines reading-ahead-makes-no-garbage)
  ;; reading ahead is looked at for every frame a window of a large file draws: what one more line
  ;; moved down costs it, in octets, stays near nothing (each key a frame; a stretch is asked for
  ;; only now and then; the looks through the whole file in the background, which make what they
  ;; need meanwhile, left out)
  (with-every-file-large ()
    (with-test-directory (dir)
      (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 400000))))
            (tuatara.io::*memory-read-limit* 0)
            (tuatara.defaults::*conflict-markers-max-bytes* 0)
            (tuatara.defaults::*vc-gutter-max-bytes* 0))
        (flet ((keys (n) (format nil "~{~a~^ ~}" (make-list n :initial-element "C-n"))))
          (reading-ahead-garbage (keys 20) path)
          (multiple-value-bind (few few-calls) (reading-ahead-garbage (keys 200) path)
            (multiple-value-bind (more more-calls) (reading-ahead-garbage (keys 1000) path)
              ;; looked at for each frame drawn
              (true (>= (- more-calls few-calls) 800) "~d calls more" (- more-calls few-calls))
              (true (< (/ (- more few) 800.0) 8) "~,1f octets a line" (/ (- more few) 800.0)))))))))

(defmacro with-large-file ((path dir &key (lines 1200000)) &body body)
  "Run BODY with PATH bound to a file of LINES lines in DIR, mapped when it is
visited, the system holding none of it in memory; or say the test is skipped
where the file system holds its files in memory."
  `(let ((,path (namestring (put-file (merge-pathnames "big.txt" ,dir) (sample-lines ,lines))))
         (tuatara.io::*memory-read-limit* 0))
     (if (let-go-of-file ,path)
         (progn ,@body)
         (skip "this file system holds its files in memory" (true nil)))))

(defun few-pages-held (path)
  "Check that the system holds in memory less than a twentieth of the file at
PATH, its ends left out (PAGES-HELD-INSIDE), as a pass reading it through
once leaves it."
  (multiple-value-bind (held all) (pages-held-inside path)
    (true (< held (/ all 20)) "~d of ~d pages held" held all)))

(defun read-in-the-background (path function)
  "Call FUNCTION with a rope over the file at PATH as work in the background
does (TUATARA.RUNTIME::CALL-AS-LONG-WORK), then let the rope's mapping
go; return what FUNCTION returned."
  (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
    (unwind-protect (tuatara.runtime::call-as-long-work (lambda () (funcall function rope)))
      (tuatara.text:close-mapping mapping))))

(define-test (counting-lines work-in-the-background-reads-through-once)
  ;; a background result reading a large file whole, as a count of matches does, leaves the
  ;; memory as it found it: what it reads is let go of behind it
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
          (let ((cell (tuatara.runtime:start-background-result
                       (lambda ()
                         (let ((block (tuatara.text:make-octets 65536)))
                           (loop for at from 0 below (tuatara.text:rope-bytes rope) by 65536
                                 sum (tuatara.text:rope-copy-into
                                                      rope at (min (tuatara.text:rope-bytes rope) (+ at 65536)) block))))
                       "a test reading" nil)))
            (loop repeat 500 until (car cell) do (sleep 0.01))
            (is = (tuatara.text:rope-bytes rope) (car cell))
            (tuatara.text:close-mapping mapping)
            (few-pages-held path)))))))

(define-test (counting-lines conflict-look-reads-through-once)
  ;; looking through a large file for conflicts, as its job does, leaves the memory as it found it
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (is equal '() (read-in-the-background path #'tuatara.defaults::conflict-find-all))
        (few-pages-held path)))))

(define-test (counting-lines git-comparison-reads-through-once)
  ;; the lines of a large file hashed to compare it with git, as its job does, leave the memory
  ;; as it was found
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (is = 1200000 (length (read-in-the-background path #'tuatara.defaults::rope-line-hashes)))
        (few-pages-held path)))))

(define-test (counting-lines search-reads-through-once)
  ;; a search through all of a large file, literal and by pattern, on all the cores, and one going
  ;; back from the end, leaves the memory as it found it
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (is = 1200000 (length (read-in-the-background path (lambda (rope) (tuatara.syntax:search-all rope "line"))))))
      (with-large-file (path dir)
        (is = 11999 (length (read-in-the-background
                            path (lambda (rope) (tuatara.syntax:regex-search-all rope "^\\d+00 "))))))
      (with-large-file (path dir)
        (false (read-in-the-background path (lambda (rope) (tuatara.syntax:search-backward rope "kiwi"))))
        (few-pages-held path)))))

(defun run-until-jobs-end (keys path)
  "Type KEYS on the file at PATH, then wait for the work they started in the
background to end, within a minute, and for the system to hold less than
eight megabytes of PATH, within fifteen seconds more, as counts of matches started
alongside end; the pages of PATH the system holds then, its ends left out
\(PAGES-HELD-INSIDE), and how many there are."
  (let ((held nil))
    (run-keys (list keys
                    (lambda ()
                      (loop repeat 3000
                            while (some #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*)
                            do (sleep 0.02))
                      (loop repeat 750
                            do (setf held (multiple-value-list (pages-held-inside path)))
                            until (< (first held) 2048)
                            do (sleep 0.02))))
              :file path :width 80 :height 20)
    (kill-buffers-visiting (directory-namestring path))
    (values-list held)))

(define-test (counting-lines replace-reads-through-once)
  ;; a replacement in every line of a large file, made in the background, leaves the memory as it
  ;; found it
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir :lines 1600000)
        (let ((tuatara.defaults::*rewrite-threshold* 0))
          (multiple-value-bind (held all) (run-until-jobs-end "C-x h r r % l i n e / w o r d RET" path)
            ;; what the window and the editor itself read, the file's head and its end among them, and a
          ;; megabyte about each: a few megabytes at most
          (true (< held 2048) "~d of ~d pages held" held all)))))))

(define-test (counting-lines sort-reads-through-once)
  ;; sorting the lines of a large file on disk leaves the memory as it found it
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir :lines 1600000)
        (let ((tuatara.defaults::*sort-in-memory-limit* 0))
          (multiple-value-bind (held all) (run-until-jobs-end "C-x h M-x s o r t - l i n e s RET" path)
            ;; what the window and the editor itself read, the file's head and its end among them, and a
          ;; megabyte about each: a few megabytes at most
          (true (< held 2048) "~d of ~d pages held" held all)))))))

(define-test (counting-lines copy-reads-through-once)
  ;; a large file copied, as a snapshot of it is made, leaves the memory as it found it, though
  ;; the kernel cannot copy it itself (into memory, here) and it is read and written in pieces
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (let ((in (sb-posix:open path sb-posix:o-rdonly))
              (size (sb-posix:stat-size (sb-posix:stat path))))
          (unwind-protect
               (let ((copy (tuatara.io::sealed-memory-file
                            "copy" (lambda (out) (tuatara.io:copy-fd-contents in out size)))))
                 (is = size (sb-posix:stat-size (sb-posix:fstat copy)))
                 (sb-posix:close copy))
            (sb-posix:close in))
          (few-pages-held path))))))

(define-test (counting-lines copy-of-a-file-cut-short-fails-cleanly)
  ;; a large file cut short while a copy reads it through fails the copy as one that changed
  ;; while it was copied, and the pass the copy read in ends without reading past the new end
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (let ((in (sb-posix:open path sb-posix:o-rdonly))
              (out (sb-posix:open (namestring (merge-pathnames "copy" dir))
                                  (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-trunc) #o600))
              (size (sb-posix:stat-size (sb-posix:stat path))))
          (unwind-protect
               (progn
                 (sb-int:encapsulate 'tuatara.io:copy-fd-range 'cut-short
                                     (lambda (function &rest arguments)
                                       (sb-posix:truncate path 100000)
                                       (apply function arguments)))
                 (unwind-protect
                      (is eq :changed (handler-case (progn (tuatara.io:copy-fd-contents in out size :path path) :copied)
                                        (tuatara.io:file-system-error () :changed)))
                   (sb-int:unencapsulate 'tuatara.io:copy-fd-range 'cut-short)))
            (sb-posix:close out)
            (sb-posix:close in)))))))

(defun thread-major-faults ()
  "How many times the calling thread has waited for the disk to read a page
it touched, as /proc counts them."
  (let ((stat (uiop:read-file-string "/proc/thread-self/stat")))
    (parse-integer (nth 10 (uiop:split-string (subseq stat (1+ (position #\) stat :from-end t))) :separator " ")))))

(define-test (counting-lines pass-ended-after-memory-let-go-reads-nothing)
  ;; a pass that ends after the system let go of what it asked for, as it does when memory is
  ;; short, waits for nothing on its way from the disk: it reads none of it back in
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
          (declare (ignore rope))
          (let ((pass (tuatara.text::make-read-pass)) (at (* 3 tuatara.text::+read-piece+)))
            (let ((tuatara.text::*read-pass* pass))
              (tuatara.text:note-read mapping at (+ at 65536)))
            (sleep 0.2)
            (true (let-go-of-file path))
            (let ((before (thread-major-faults)))
              (tuatara.text::end-read-pass pass)
              (is = before (thread-major-faults))))
          (tuatara.text:close-mapping mapping))))))

(define-test (counting-lines sum-reads-through-once)
  ;; the SHA-256 sum of a large file leaves the memory as it found it
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (true (tuatara.defaults::file-sha256 path))
        (few-pages-held path)))))

(define-test (counting-lines settings-at-the-end-read-alone)
  ;; the settings a large file may name at its end are read without the megabytes around them
  ;; that a read of a mapped file brings in on some file systems
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (let ((size (sb-posix:stat-size (sb-posix:stat path))))
          (multiple-value-bind (held all)
              (run-until-jobs-end "" path)
            (declare (ignore all))
            ;; the end a pass keeps aside, the octets read there and a megabyte about them reach no further
            (multiple-value-bind (before-end pages)
                (pages-held path (- size (* 2 tuatara.text::+kept-ends+)) (- size tuatara.text::+kept-ends+))
              (true (< before-end 64) "~d of the ~d pages before the end held, ~d in all"
                    before-end pages held))))))))

(define-test (counting-lines pass-finished-after-stopped-work)
  ;; a pass whose work is stopped, the tasks it had not begun passed over, still finishes once its
  ;; tasks are done, so that what it held is given back
  (let ((pass nil) (cancel (list nil)))
    (tuatara.text:with-read-pass ()
      (setf pass tuatara.text::*read-pass*)
      (tuatara.runtime:stoppable-work (cancel)
        (tuatara.runtime:run-tasks
         (coerce (loop repeat 256 collect (lambda () (setf (car cancel) t) (sleep 0.001))) 'simple-vector))))
    (true (loop repeat 500 thereis (tuatara.text::read-pass-finished pass) do (sleep 0.01))
          "~d tasks still counted" (tuatara.text::read-pass-users pass))))

(define-test (counting-lines slices-read-nothing)
  ;; a slice of a large file whose lines are not counted yet, as a look at the file shown takes,
  ;; reads none of the file: the spans following each other in it are joined as they lie
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
          (let ((slice (tuatara.text:rope-slice rope 0 (tuatara.text:rope-bytes rope))))
            (is = (tuatara.text:rope-bytes rope) (tuatara.text:rope-bytes slice))
            (true (< (pages-held path) 8) "~d pages held" (pages-held path)))
          (tuatara.text:close-mapping mapping))))))

(define-test (counting-lines stopped-reading-leaves-the-records-sound)
  ;; reading through a pass cut short again and again inside its stoppable calls, as C-g and the
  ;; watch on slow calls cut a search short, leaves no piece held once the passes are done, the
  ;; records of what is held whole, no count left standing by a stop in the middle of one, and
  ;; almost none of the file in memory once what the disk was still reading has come
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
          (let ((size (tuatara.text:rope-bytes rope)))
            (dotimes (round 40)
              (let* ((cancel (list nil))
                     (reader (sb-thread:make-thread
                              (lambda ()
                                (tuatara.text:with-read-pass ()
                                  (tuatara.runtime:stoppable-work (cancel)
                                    (let ((block (tuatara.text:make-octets 65536)))
                                      (loop for at from (* round 65536) below size by 65536
                                            do (tuatara.runtime:stoppable
                                                 (tuatara.text:rope-copy-into
                                                  rope at (min size (+ at 65536)) block))))))))))
                (sleep (random 0.01))
                (tuatara.runtime:stop-work cancel)
                (sb-thread:join-thread reader :default nil)))
            (let ((pieces (tuatara.text::mapping-pieces mapping)))
              (true (loop repeat 500 thereis (zerop (hash-table-count pieces)) do (sleep 0.01))
                    "~d pieces still held" (hash-table-count pieces))
              (let ((entries 0))
                (maphash (lambda (piece entry) (declare (ignore piece entry)) (incf entries)) pieces)
                (is = 0 entries))))
          ;; what the disk brought in after it was let go of, let go of again
          (true (loop repeat 500 thereis (null tuatara.text::**late-looks**) do (sleep 0.01)))
          (tuatara.text:close-mapping mapping))
        (few-pages-held path)))))

(define-test (counting-lines cancelled-sum-lets-go)
  ;; a sum of a large file stopped before it reads anything lets go of the descriptor it opened
  ;; to ask the system and of all it asked for: no descriptor and no mapping are left
  (with-every-file-large ()
    (with-test-directory (dir)
      (with-large-file (path dir)
        (flet ((open-fds () (length (directory "/proc/self/fd/*" :resolve-symlinks nil))))
          (let ((fds (open-fds)) (mappings (length (tuatara.text:live-mappings)))
                (fd (sb-posix:open path sb-posix:o-rdonly)))
            (unwind-protect (false (tuatara.defaults::fd-sha256 fd path :cancel (list t)))
              (sb-posix:close fd))
            (is = fds (open-fds))
            (is = mappings (length (tuatara.text:live-mappings)))))
        (few-pages-held path)))))
