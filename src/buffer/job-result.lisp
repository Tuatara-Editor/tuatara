;;;; src/buffer/job-result.lisp --- A job's result taken into a buffer, its undo laid out where it began.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A job's change stands in the undo history where the job began: its text
;;;; is swapped in once, and the changes made while it ran are carried over
;;;; onto its result as pieces, so that undo takes them back first.

(in-package #:tuatara.buffer)

(define-record (carry-piece (:constructor make-carry-piece (start linear move shift)) (:copier nil))
  "A stretch of a text edited while work in the background ran, from START up
to the next piece's START, and where it stands in the work's result with the
edits made there again (CARRIED-POSITION): with LINEAR, text the edits put,
START + MOVE on, at that offset; else the work's map at the position less
MOVE, plus SHIFT."
  (start 0 :type fixnum :documentation "The offset where CARRY-PIECE starts.")
  (linear nil :documentation "True when CARRY-PIECE is text the edits put, standing MOVE on from its offsets.")
  (move 0 :type fixnum
        :documentation "The amount added to a position in CARRY-PIECE when it is LINEAR, else taken from it before the
work's map is used.")
  (shift 0 :type fixnum
         :documentation "The amount added to what the work's map gives for a position in CARRY-PIECE, when it is not
LINEAR."))

(defun carry-pieces ()
  "The pieces (CARRY-PIECE) of a text no edit has touched yet: one, from 0 on,
where the work's map takes it."
  (make-array 1 :adjustable t :fill-pointer 1 :initial-element (make-carry-piece 0 nil 0 0)))

(defun carry-piece-at (pieces position)
  "The index in PIECES of the piece POSITION stands in: the last one starting
at or before it. O(log pieces)."
  (let ((lo 0) (hi (fill-pointer pieces)))
    (loop while (> (- hi lo) 1)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (<= (carry-piece-start (aref pieces mid)) position) (setf lo mid) (setf hi mid))))
    lo))

(defun carried-position (pieces map position bias)
  "Where POSITION in the text PIECES cover stands in the work's result, MAP
taking the text no edit put there, with BIAS, as it takes the text before the
edits."
  (let ((piece (aref pieces (carry-piece-at pieces position))))
    (if (carry-piece-linear piece)
        (+ position (carry-piece-move piece))
        (+ (funcall map (- position (carry-piece-move piece)) bias) (carry-piece-shift piece)))))

(defun carry-pieces-past (pieces map edits)
  "Carry PIECES, a text's pieces as CARRIED-POSITION reads them with MAP, past
EDITS (an applied batch's, sorted, in the text before them), made on both:
the pieces of the text after EDITS, an edit's inserted text going to where it
is made again, the rest moved by what the edits before it took and put, in
either text. Only the pieces the edits touch are made again; those after are
moved where they stand, so carrying a batch past costs its edits and the
pieces after them, not the batches carried before."
  (let* ((theres (map 'simple-vector (lambda (e)
                                       (carried-position pieces map (tuatara.text:edit-start e) :before)) edits))
         (ends (map 'simple-vector (lambda (e) (carried-position pieces map (tuatara.text:edit-end e) :after)) edits))
         (count (fill-pointer pieces))
         (first (carry-piece-at pieces (tuatara.text:edit-start (aref edits 0))))
         (last (carry-piece-at pieces (tuatara.text:edit-end (aref edits (1- (length edits))))))
         (made '())
         ;; DELTA, how far the edits passed moved this text; SHIFT, the text made elsewhere
         (delta 0) (shift 0))
    (labels ((piece-end (i)
               (if (< (1+ i) count) (carry-piece-start (aref pieces (1+ i))) most-positive-fixnum))
             (moved (piece from)
               ;; PIECE from FROM on, after the edits passed so far
               (if (carry-piece-linear piece)
                   (make-carry-piece (+ from delta) t (+ (carry-piece-move piece) (- delta) shift) 0)
                   (make-carry-piece (+ from delta) nil (+ (carry-piece-move piece) delta)
                                     (+ (carry-piece-shift piece) shift))))
             (keep (from below)
               ;; the old pieces over FROM up to BELOW, in the text before the edits
               (loop for i from first to last
                     for piece = (aref pieces i)
                     for a = (max from (carry-piece-start piece))
                     when (< a (min below (piece-end i)))
                       do (push (moved piece a) made))))
      (let ((from (carry-piece-start (aref pieces first))))
        (loop for e across edits
              for there across theres
              for end across ends
              for inserted = (length (or (tuatara.text:edit-insert e) #()))
              do (keep from (tuatara.text:edit-start e))
                 (when (plusp inserted)
                   (let ((start (+ (tuatara.text:edit-start e) delta)))
                     (push (make-carry-piece start t (- (+ there shift) start) 0) made)))
                 (incf delta (- inserted (- (tuatara.text:edit-end e) (tuatara.text:edit-start e))))
                 (incf shift (- inserted (- end there)))
                 (setf from (tuatara.text:edit-end e)))
        (keep from (piece-end last)))
      ;; the pieces after the last one touched: moved where they stand
      (loop for i from (1+ last) below count
            for piece = (aref pieces i)
            do (incf (carry-piece-start piece) delta)
               (if (carry-piece-linear piece)
                   (incf (carry-piece-move piece) (- shift delta))
                   (progn (incf (carry-piece-move piece) delta)
                          (incf (carry-piece-shift piece) shift))))
      ;; the pieces FIRST to LAST give way to those made
      (let* ((made (coerce (nreverse made) 'simple-vector))
             (removed (1+ (- last first)))
             (new-count (+ (- count removed) (length made))))
        (when (> new-count (array-dimension pieces 0))
          (adjust-array pieces (max new-count (* 2 (array-dimension pieces 0)))))
        (setf (fill-pointer pieces) (max count new-count))
        (replace pieces pieces :start1 (+ first (length made)) :start2 (1+ last) :end2 count)
        (replace pieces made :start1 first)
        (setf (fill-pointer pieces) new-count)
        pieces))))

(defvar *made-content-number* 0
  "The last content number given to a text an undo step was made to lead to
without the buffer ever holding it (BUFFER-TAKE-IN-JOB-RESULT): counted down
from 0, so that it is never a version's number nor a file's saved content.")

(defun rope-leaf-lengths (rope)
  "ROPE's leaves and file spans in order, each as (NODE . OCTETS IN IT)."
  (let ((pieces '()))
    (tuatara.text:map-leaf-pieces (lambda (node start end) (push (cons node (- end start)) pieces)) rope)
    (nreverse pieces)))

(defun ropes-same-octets-p (a b)
  "True when the ropes A and B hold the same octets. A leaf or file span both
share, at the same place, is passed over unread, so that two versions of a
large text made from one are told apart by reading only what was changed in
either; the reading stops at the first octet that differs. NIL too when what
would have to be read has not arrived from its file yet."
  (and (= (tuatara.text:rope-octet-length a) (tuatara.text:rope-octet-length b))
       (let ((left (rope-leaf-lengths a)) (right (rope-leaf-lengths b))
             (left-at 0) (right-at 0) (at 0)
             (one (make-array 4096 :element-type '(unsigned-byte 8)))
             (other (make-array 4096 :element-type '(unsigned-byte 8))))
         (declare (type fixnum left-at right-at at) (dynamic-extent one other))
         (handler-case
             (loop
               (loop while (and left (= left-at (cdar left))) do (setf left (cdr left) left-at 0))
               (loop while (and right (= right-at (cdar right))) do (setf right (cdr right) right-at 0))
               (when (or (null left) (null right))
                 (return (and (null left) (null right))))
               (let* ((shared (and (eq (caar left) (caar right)) (= left-at right-at)))
                      (n (min (- (cdar left) left-at) (- (cdar right) right-at) (if shared most-positive-fixnum 4096))))
                 (declare (type fixnum n))
                 (unless shared
                   (tuatara.text:rope-check-arrived a at (+ at n))
                   (tuatara.text:rope-check-arrived b at (+ at n))
                   (tuatara.text:piece-copy (caar left) left-at (+ left-at n) one 0)
                   (tuatara.text:piece-copy (caar right) right-at (+ right-at n) other 0)
                   (when (mismatch one other :end1 n :end2 n)
                     (return nil)))
                 (incf left-at n)
                 (incf right-at n)
                 (incf at n)))
           (tuatara.text:not-arrived () nil)))))

(defun lay-out-where-begun (buffer since begun final final-forward final-backward join)
  "Make FINAL BUFFER's text as one swap (FINAL-FORWARD and FINAL-BACKWARD map
the text now to it and back), the undo history laid out as if BEGUN, a swap of
the text as it stood when BUFFER's undo stack was SINCE for the work's result
there, had been made then (see BUFFER-TAKE-IN-JOB-RESULT). With JOIN, that
step and the newest one under SINCE are one. NIL, and nothing done, when the
history no longer passes through SINCE, a change made meanwhile swapped the
whole text, or the changes carried over do not lead to FINAL; true otherwise."
  (let* ((rope (tuatara.text:swap-after begun))
         (forward (tuatara.text:swap-forward begun))
         (stack (buffer-undo-stack buffer))
         (later (and (not (eq stack since)) (tailp since stack) (ldiff stack since))))
    (when (and (or (eq stack since) later)
               (notany (lambda (group) (some #'tuatara.text:batch-swap group)) later))
      ;; the changes made meanwhile, carried over onto ROPE, made one after another without the buffer
      (let ((text rope) (pieces (carry-pieces)) (groups '()))
        (dolist (group (reverse later))
          (let ((batches '()))
            (dolist (batch (reverse group))
              (let ((edits (tuatara.text:batch-edits batch)))
                (multiple-value-bind (next made)
                    (tuatara.text:edits-batch
                     text (map 'simple-vector
                               (lambda (e)
                                 (tuatara.text:make-edit
                                  (carried-position pieces forward (tuatara.text:edit-start e) :before)
                                  (carried-position pieces forward (tuatara.text:edit-end e) :after)
                                  (tuatara.text:edit-insert e)))
                               edits))
                  (setf text next)
                  (when (plusp (length edits)) (carry-pieces-past pieces forward edits))
                  (push made batches))))
            (push batches groups)))
        (when (ropes-same-octets-p text final)
          (let* ((table (buffer-batch-contents buffer))
                 (began (let ((oldest (and later (car (last (car (last later)))))))
                          (and oldest (car (gethash oldest table)))))
                 (swapped (swap-whole-text buffer final final-forward final-backward))
                 (now (buffer-content-number buffer))
                 (before (or began (car (gethash swapped table))))
                 (work (tuatara.text:swap-batch (tuatara.text:swap-before begun) rope forward
                                                (tuatara.text:swap-backward begun))))
            (remhash swapped table)
            ;; the content numbers before and after each step, for undo to know the text saved
            (setf (gethash work table) (cons before (if groups (decf *made-content-number*) now)))
            (loop for (batches . more) on (reverse groups)
                  for from = (cdr (gethash work table)) then to
                  for to = (if more (decf *made-content-number*) now)
                  do (dolist (batch batches) (setf (gethash batch table) (cons from to))))
            (setf (buffer-undo-stack buffer)
                  (append groups
                          (if (and join since)
                              (cons (cons work (car since)) (cdr since))
                              (cons (list work) since)))
                  (buffer-redo-stack buffer) '())
            (add-held buffer (- (+ (batch-held-octets buffer work)
                                   (loop for group in groups sum (group-held-octets buffer group)))
                                (loop for group in later sum (group-held-octets buffer group)))
                      0)
            ;; a place in the history at SINCE or above it stays above the same changes, the work under them
            (dolist (place (buffer-local buffer 'undo-places))
              (when (and (tailp (car place) stack) (tailp since (car place)))
                (setf (car place) (nthcdr (- (length groups) (length (ldiff (car place) since)))
                                          (buffer-undo-stack buffer)))))
            (let-go-of-oldest-changes buffer)
            (keep-within-narrowing buffer)
            t))))))

(defun buffer-take-in-job-result (buffer final forward backward &key (since nil since-p) begun join)
  "Make FINAL, a background job's result with the changes made to BUFFER
meanwhile in it, BUFFER's whole text as one undoable step, FORWARD mapping a
byte position and a bias from the text now to FINAL and BACKWARD back, as
BUFFER-SWAP-TEXT does: point, marks and places move once, to where they
belong. SINCE is BUFFER's undo stack when the job began (NIL when it was
empty; for a job that waited its turn, the place in the history kept by
MAKE-UNDO-PLACE), BEGUN a swap (TUATARA.TEXT:MAKE-SWAP) of the text the job
began on for its result there. Given both, the step stands in the undo
history where the job began: undo takes back the changes made meanwhile
first, each its own step, then the job's. It goes on top, as the newest step,
when BEGUN is not given, when the history was undone past SINCE, or when a
change made meanwhile swapped the whole text. With JOIN, the step and the
newest one under SINCE (a change the same work made first, in sight) are one
step, where it began or on top when nothing came between them. Signals an
error when the buffer is read-only. Returns :WHERE-BEGUN or :ON-TOP, for
where it went."
  (if (and since-p begun (lay-out-where-begun buffer since begun final forward backward join))
      :where-begun
      (progn (buffer-swap-text buffer final forward backward)
             (when (and join since (eq (cdr (buffer-undo-stack buffer)) since))
               (merge-undo-groups buffer (cdr since)))
             :on-top)))
