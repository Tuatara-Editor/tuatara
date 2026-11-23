;;;; src/syntax/regex-collect.lisp --- Every match of a search, found in parallel pieces.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; COLLECT-MATCHES gathers all the matches of a literal or regular-expression
;;;; scan for replacing and counting, in pieces on several cores joined in order.

(in-package #:tuatara.syntax)

;;; Collecting every match, for replacing and counting
;;;
;;; A replacement needs all of its matches before it changes anything, and on a
;;; file of many gigabytes they may not fit in memory. COLLECT-MATCHES finds them
;;; in parallel pieces, keeps an estimate of what they will cost, and gives up as
;;; soon as that passes a limit or a key is waiting, before the heap runs out.

(defconstant +counted-firsts+ 16
  "How many of its first matches a piece counted without keeping them
remembers, to join it to the piece before (COLLECT-MATCHES).")

(defun collect-matches (mapper start end &key (threads (tuatara.runtime:cpu-count)) stop cost-limit
                                               (match-cost 400) (block-size 16384) (keep t))
  "Every match that MAPPER finds from START below END, in order, exactly as one
scan from START finds them, gathered in parallel pieces of *PARALLEL-PIECE*
octets on THREADS threads. MAPPER is a function of (FUNCTION FROM TO HALT)
that calls FUNCTION with (START END TEXT GROUPS) for each non-overlapping
match starting in [FROM, TO) as a scan from FROM finds them. It stops when
FUNCTION returns true or HALT answers true (MAP-REGEX-MATCHES and
MAP-LITERAL-MATCHES serve, wrapped). A match that runs out of its piece has
the next piece's matches found again from where it ends.
Three values. The first is the matches as a list of (START END TEXT GROUPS).
The second is NIL when all were found, :STOPPED when STOP, asked on the
calling thread every few milliseconds, answered true, or :OVER when the
estimated cost passed COST-LIMIT (octets). The cost is counted as MATCH-COST
for each match and BLOCK-SIZE for each block of that many octets the matches
touch; then the list is NIL. The third is how many matches were found, all or
before giving up. STOP is also asked once before the pieces start, so it is
heard even when the pool's workers take every piece.
With KEEP false the matches are only counted, the list always NIL. Nothing is
kept for each, so counting a hundred million costs no more memory than one."
  (declare (type fixnum start end))
  (let* ((piece *parallel-piece*)
         (count (max 0 (ceiling (- end start) piece)))
         (cost (list 0)) (found (list 0)) (over (list nil)))
    (multiple-value-bind (results stopped)
        (run-pieces count
                    (lambda (k stopping)
                      (let ((a (+ start (* k piece))) (b (min end (+ start (* (1+ k) piece))))
                            (acc '()) (local 0) (published 0) (matches 0) (last-block -1) (last-end -1))
                        (declare (type fixnum local published matches last-block last-end))
                        (labels ((publish ()
                                   (when (> local published)
                                     (let ((delta (- local published)))
                                       (setf published local)
                                       (when (and cost-limit
                                                  (> (+ (sb-ext:atomic-incf (car cost) delta) delta) cost-limit))
                                         (setf (car over) t)))))
                                 (halt ()
                                   (publish)
                                   (or (car over) (funcall stopping))))
                          (unless (halt)
                            (funcall mapper
                                     (lambda (s e m g)
                                       (declare (type fixnum s e))
                                       (when (or keep (< matches +counted-firsts+))
                                         (push (list s e m g) acc))
                                       ;; a kept text costs its length: a Base64 block may be long
                                       (when (and keep m) (incf local (- e s)))
                                       (setf last-end e)
                                       (incf matches)
                                       (incf local match-cost)
                                       (let ((blk (floor s block-size)))
                                         (unless (= blk last-block) (setf last-block blk) (incf local block-size)))
                                       nil)
                                     a b #'halt))
                          (publish)
                          (sb-ext:atomic-incf (car found) matches)
                          (list (nreverse acc) matches last-end))))
                    stop threads)
      (cond (stopped (values nil :stopped (car found)))
            ((car over) (values nil :over (car found)))
            (t (multiple-value-bind (matches total) (join-collected-pieces mapper results start end piece keep)
                 (values matches nil total)))))))

(defun join-collected-pieces (mapper results start end piece keep)
  "The matches COLLECT-MATCHES found in its pieces of PIECE octets from START
below END, from their RESULTS, (OWN COUNT LAST-END) each, OWN the piece's
matches (only its first ones unless KEEP). A piece the match before runs into
has its matches found again from where that match ends (JOIN-PIECES), until
one is among its own. From there they agree, and past its own remembered the
rest is counted again. Two values: the matches in order (NIL unless KEEP),
and how many there are."
  (let ((out '()) (total 0))
    (declare (type fixnum total))
    (join-pieces (length results) (lambda (k) (+ start (* k piece)))
                 (lambda (k from)
                   (destructuring-bind (own count own-end) (aref results k)
                     (declare (type fixnum count own-end))
                     (if (not from)
                         (progn (when keep (setf out (revappend own out)))
                                (incf total count)
                                (and (plusp count) own-end))
                         (let ((b (min end (+ start (* (1+ k) piece)))) (rest own) (passed 0) (met nil) (ends nil))
                           (declare (type fixnum passed))
                           (funcall mapper
                                    (lambda (s e m g)
                                      (loop while (and rest (< (first (car rest)) s)) do (pop rest) (incf passed))
                                      (if (and rest (= (first (car rest)) s) (= (second (car rest)) e))
                                          (setf met t)
                                          (progn (when keep (push (list s e m g) out))
                                                 (incf total) (setf ends e) nil)))
                                    (min from b) b nil)
                           (if met
                               (progn (when keep (setf out (revappend rest out)))
                                      (incf total (- count passed))
                                      own-end)
                               ends))))))
    (values (and keep (nreverse out)) total)))

(defun join-pieces (count start-of join)
  "Go through COUNT pieces of a text cut for work in parallel, in order, as one
scan from the first piece's start goes on, so that a match is never cut at a
piece's start. JOIN is a function of a piece's index and FROM. It takes that
piece's part and returns where the last match it took ends, or NIL when it
took none. FROM is NIL, or, when a match of the pieces before runs past the
piece's start (START-OF, a function of the index, gives it), where that match
ends. The piece's matches are then those one scan finds from there, past its
end too when the match runs past it."
  (let ((reach -1))
    (declare (type fixnum reach))
    (dotimes (k count)
      (let ((ends (funcall join k (and (> reach (funcall start-of k)) reach))))
        (when ends (setf reach (max reach ends)))))))
