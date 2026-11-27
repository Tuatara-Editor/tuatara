;;;; lisp/search/rewrite.lisp --- Replacements by a pattern too large for memory, made in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A replacement or count of the matches of a pattern across a file of many
;;;; gigabytes runs as a background job (MATCH-REWRITE) of the kind
;;;; lisp/editing/rewrites.lisp describes: the matches are found in each piece
;;;; as one scan would find them, replaced as the replacement says, and the
;;;; matches still to change are shaded in the windows meanwhile.
;;;;
;;;; A pattern whose matches may cross lines is cut into pieces as any other:
;;;; where a match runs from one piece into the next, the next is done again
;;;; from where it ends once all are done (MEND-SEAMS).

(in-package #:tuatara.defaults)

(define-record (match-rewrite (:include rewrite) (:constructor %make-match-rewrite) (:copier nil))
  "A REWRITE whose MAPPER only finds matches, keeping no count of its own: a
replacement, a count, a split's change at every match out of sight. The
matches still to change are shaded rather than the whole stretch
\(JOB-MATCH-SHADING). HIDDEN-LEFT, when not NIL, is a cons whose car counts the
matches left alone because a fold hides them (HIDDEN-LEFT-WORDS). SHADED is
what JOB-MATCH-SHADING told last, kept while the text shown stays the same."
  (hidden-left nil
               :documentation "A cons whose car counts the matches JOB left alone because a fold hides them, NIL when
none.")
  (shaded nil
          :documentation "What JOB-MATCH-SHADING last told of a MATCH-REWRITE, (STRETCHES VERSION RANGES), kept
while the stretches shown and the buffer's text (its version) stay the same; or NIL."))

(defun hidden-left-words (count)
  "What is said of COUNT matches left alone because a fold hides them, after
what a piece of work did: \"; N hidden in folds left alone\", or \"\" for none.
COUNT is a number, or a job (MATCH-REWRITE) whose HIDDEN-LEFT counts them."
  (let ((n (cond ((numberp count) count)
                 ((and (match-rewrite-p count) (match-rewrite-hidden-left count))
                  (car (match-rewrite-hidden-left count)))
                 (t 0))))
    (if (plusp n) (format nil "; ~:d hidden in folds left alone" n) "")))

(defun rewrite-mapper-for (rope matcher texts high)
  "A function of (FUNCTION FROM TO HALT) calling FUNCTION with each match of
MATCHER in ROPE starting in [FROM, TO), as one scan from FROM finds them,
empty ones too and the one at HIGH, the scope's end (STRETCH-MATCHES); the
pattern sees no text past HIGH, so that a match may run past TO. TEXTS, what
the replacement needs of each match, as TUATARA.SYNTAX:MATCHED-TEXT takes it
\(REWRITE-EXPANDER)."
  (lambda (function from to halt)
    (stretch-matches function rope matcher from to (eql to high) texts halt high)))

(defun rewrite-expander (replacement regex fold &optional (newline (string #\Newline)))
  "A function of a match's TEXT and GROUPS giving its replacement as octets, the
text REPLACEMENT-TEXT-FUNCTION gives, as RUN-SUBSTITUTION makes it: the same
text encoded once and its octets given again, so that a replacement the match
does not enter makes nothing however many matches there are. An expander may
also give, for a replacement too large to hold, a cons of its length in octets
and a function of a writer (TUATARA.IO:WRITER) that writes it
\(REPLACEMENT-LENGTH). As a second value, the TEXTS a scan is to give it
\(TUATARA.SYNTAX:MATCHED-TEXT), one of NIL, T, or MATCH-CASE-KIND when only the
match's letter case counts, so that no match's text is copied. A line break
written in REPLACEMENT is NEWLINE, the buffer's."
  (multiple-value-bind (text-of needs) (replacement-text-function replacement regex fold newline)
    (let (;; the text last encoded and its octets, one cons read whole, shared by the pieces' threads
          (last (cons nil nil)))
      (values (lambda (m g)
                (let ((text (funcall text-of m g))
                      (known last))
                  (if (eq text (car known))
                      (cdr known)
                      (let ((new (cons text (tuatara.text:string-octets text))))
                        (setf last new)
                        (cdr new)))))
              (case needs ((nil) nil) (:case #'match-case-kind) (t t))))))

(defmethod job-match-shading ((job match-rewrite) stretches)
  "What JOB will change within STRETCHES, (START . END) of the text shown, as
ranges in :JOB-QUEUED (CHANGES-IN-VIEW, by the job's own mapper, in the text
it reads), and as a second value true when that could be told: not when the
pattern is too slow on this text to light it. After edits made since the job
began, the stretches are taken back to the text it reads (JOB-POSITION-READ)
and its matches followed forward to the text now (JOB-POSITION-NOW), a match
an edit reached into left out, so that only what will change is lit whatever
was typed meanwhile, at a cost that does not grow with the typing. What was
told is kept while the stretches and the buffer's text stay the same, so that
the frames the job's progress draws look for nothing again."
  (let ((version (tuatara.buffer:buffer-version (rewrite-buffer job)))
        (kept (match-rewrite-shaded job)))
    (if (and kept (eq (second kept) version) (equal (first kept) stretches))
        (values (third kept) t)
        (multiple-value-bind (ranges told) (match-shading-now job stretches)
          (when told (setf (match-rewrite-shaded job) (list stretches version ranges)))
          (values ranges told)))))

(defun match-shading-now (job stretches)
  "What JOB, a MATCH-REWRITE, will change within STRETCHES, looked for now, as
JOB-MATCH-SHADING tells it."
  (let ((edited (job-edited-p job)))
    (multiple-value-bind (ranges slow)
        (changes-in-view (rewrite-rope job)
                         (lambda (function from to)
                           (funcall (rewrite-mapper job)
                                    (lambda (a z m g)
                                      (unless (eq m :passed) (funcall function a z m g))
                                      nil)
                                    from to nil))
                         (if edited
                             (loop for (start . end) in stretches
                                   collect (cons (job-position-read job start :before)
                                                 (job-position-read job end :after)))
                             stretches)
                         (rewrite-all job) :low (rewrite-low job))
      (unless slow
        (values (loop for (start end) in ranges
                      for here-start = (if edited (job-position-now job start :after) start)
                      for here-end = (if edited (job-position-now job end :before) end)
                      ;; a match an edit reached into is no longer what the job will change
                      when (= (- here-end here-start) (- end start))
                      collect (list here-start here-end :job-queued))
                t)))))

(defun rewrite-description (line words flags)
  "What the list of jobs says of replacing LINE's matches, or with the n flag
in FLAGS counting them, in full, WORDS saying where."
  (if (counting-flags-p flags)
      (format nil "Counting ~a ~a" (search-line-pattern line) words)
      (format nil "Replacing ~a with ~a ~a" (search-line-pattern line) (search-line-replacement line) words)))

(defun spans-meet-p (spans s e)
  "True when one of SPANS, a vector of (START . END) in order, meets the
stretch from S to E, the ends of both included."
  (let ((i (tuatara.runtime:bisect (mid 0 (length spans)) (< (cdr (aref spans mid)) s))))
    (and (< i (length spans)) (<= (car (aref spans i)) e))))

(defun mapper-after-sight (mapper lines replaced)
  "A job's mapper (REWRITE) for a text of which some matches were replaced
already, those on the lines in sight: MAPPER finds the matches in the text as
it was before, and each is told where it stands in the text now. REPLACED, a
vector in order of (START END LENGTH), are the matches replaced, where each
stood and the octets now in its place; LINES, a vector of (START . END) in
order, the lines in sight as they were, ends included. A match starting on one
of them is told as passed over (PASS-OVER), replaced or not, and every other
as it is. So the job changes what one scan of the text as it was finds off
those lines, whatever the replacements in sight made of them. The pieces it
is asked for, in the text now, are taken back to the text before: one
beginning inside a replacement begins after the match replaced.
Complexity: O(log of the matches replaced) for each match."
  (let* ((n (length replaced))
         (starts (make-array n :element-type 'fixnum)) (ends (make-array n :element-type 'fixnum))
         (lengths (make-array n :element-type 'fixnum))
         ;; where each replacement starts in the text now
         (news (make-array n :element-type 'fixnum)))
    (declare (type fixnum n))
    (loop with shift fixnum = 0
          for k from 0 below n
          do (destructuring-bind (start end length) (aref replaced k)
               (setf (aref starts k) start (aref ends k) end (aref lengths k) length
                     (aref news k) (+ start shift))
               (incf shift (- length (- end start)))))
    (flet ((now (at)
             ;; where AT of the text before, in no match replaced, stands now
             (declare (type fixnum at))
             (let ((k (1- (tuatara.runtime:bisect (mid 0 n) (<= (aref ends mid) at)))))
               (if (minusp k) at (+ (aref news k) (aref lengths k) (- at (aref ends k))))))
           (before (at)
             ;; where AT of the text now stood, the end of a match replaced for a place inside its replacement
             (declare (type fixnum at))
             (let ((k (1- (tuatara.runtime:bisect (mid 0 n) (< (aref news mid) at)))))
               (if (minusp k) at (+ (aref ends k) (max 0 (- at (aref news k) (aref lengths k)))))))
           (replaced-at (s e)
             (declare (type fixnum s e))
             (let ((k (tuatara.runtime:bisect (mid 0 n) (< (aref starts mid) s))))
               (loop while (and (< k n) (= (aref starts k) s))
                     do (when (= (aref ends k) e) (return k))
                        (incf k)))))
      (lambda (function from to halt)
        (funcall mapper
                 (lambda (s e m g)
                   (declare (type fixnum s e))
                   (let ((k (replaced-at s e)))
                     (if k
                         (pass-over function (aref news k) (+ (aref news k) (aref lengths k)))
                         (let ((at (now s)))
                           (declare (type fixnum at))
                           (if (spans-meet-p lines s s)
                               (pass-over function at (+ at (- e s)))
                               (funcall function at (+ at (- e s)) m g))))))
                 (before from) (before to) halt)))))

(defun changes-in-view (rope finder stretches all &key (low 0) keep)
  "What a job at matches changes within STRETCHES, (START . END) of ROPE's text
shown, as (START END) ranges in order. They are the matches FINDER finds, a
function of (FUNCTION FROM TO) calling FUNCTION with each match's start, end,
text matched and groups from FROM below TO. Without ALL, only the first on
each line is shown, looked for from the line's start, not before LOW, where
the replacement begins: a position, or a function of a stretch's start giving
one. A match of no length is shown on the character after it. With KEEP, a
function of a match's start, end, text and groups, what it gives for each
match in place of its range, the match not cut at the stretch's end. As a
second value :SLOW, the ranges NIL, when the pattern is too slow on this text
to tell them. The one way the matches a replacement changes are found for
what is shown: lit while it is asked for, and shaded while its job, or a
split's, runs or waits."
  (multiple-value-bind (ranges cut)
      (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
        (let ((out '()) (size (tuatara.text:rope-octet-length rope)))
          (loop for (start . end) in stretches
                do (let* ((from (if all
                                    start
                                    (max (if (functionp low) (funcall low start) low)
                                         (tuatara.text:rope-line-start rope start))))
                          (gate (if all (constantly t) (first-on-line-gate rope from))))
                     (funcall finder
                              (lambda (a z m g)
                                (when (and (funcall gate a z) (>= a start))
                                  (push (if keep
                                            (funcall keep a z m g)
                                            (list a (if (< a z) (min z end) (min size (1+ a)))))
                                        out)))
                              from end)))
          (nreverse out)))
    (if (eq cut :slow) (values nil :slow) (values ranges nil))))

(defun matcher-shading (rope matcher stretches all &key (low 0))
  "What a job at the matches of MATCHER in ROPE changes within STRETCHES, (START
. END) of the text shown, as (START END) ranges in order (CHANGES-IN-VIEW),
the first on each line only without ALL, looked for from no further back
than LOW; the whole stretches when the pattern is too slow on this text to
light the matches. Looks at the text shown only, so that it may be asked for
each frame."
  (multiple-value-bind (ranges slow)
      (changes-in-view rope (lambda (function from to)
                              (stretch-matches (lambda (a z m g) (funcall function a z m g) nil)
                                               rope matcher from to nil nil nil))
                       stretches all :low low)
    (if slow (loop for (start . end) in stretches collect (list start end)) ranges)))

(defun matches-shade (buffer line regex flags)
  "A SHADE (QUEUE-IF-BUSY) for LINE's replacement waiting its turn in BUFFER:
what it will change in the text shown, as the text stands now
\(MATCHER-SHADING); nothing for a count."
  (let ((matcher (search-line-matcher line regex))
        (all (every-match-flags-p flags)))
    (lambda (positions from to)
      (destructuring-bind (low high &rest more) positions
        (declare (ignore more))
        (let ((start (max low from)) (end (min high to)))
          (when (and (< start end) (not (counting-flags-p flags)))
            (matcher-shading (tuatara.buffer:buffer-text buffer) matcher (list (cons start end)) all :low low)))))))

(defun start-rewrite (buffer line regex low high words flags &key slow mapper joins report)
  "Start replacing, or with the n flag counting, LINE's matches between LOW and
HIGH of BUFFER in the background (see this file's header), and say so, that
the pattern is slow on this text first when SLOW is true; the matches a fold
hides are replaced too, the folds kept. MAPPER, when given, a function of
what the replacement needs of each match (REWRITE-EXPANDER), gives the job's
mapper in place of one over all the matches (REWRITE-MAPPER-FOR): those
replaced in sight already are left as they are (MAPPER-AFTER-SIGHT); the
job's change then joins JOINS for undo, and REPORT says how it ended (both as
REWRITE takes them). Returns the job, or :QUEUED when another job runs or
waits in BUFFER: it then waits its turn (QUEUE-IF-BUSY), LOW and HIGH followed
meanwhile. A replacement in a text whose lines are still being counted, a
large file just opened or the result of a replacement before it, waits its
turn the same way until they are, the editor answering keys meanwhile."
  (when (queue-if-busy buffer (rewrite-description line words flags)
                       (list (cons low :before) (cons high :after))
                       (lambda (low high)
                         (start-rewrite buffer line regex low high words flags
                                        :slow slow :mapper mapper :joins joins :report report))
                       :shade (matches-shade buffer line regex flags)
                       :counted (not (counting-flags-p flags)))
    (return-from start-rewrite :queued))
  (unless (counting-flags-p flags)
    (check-hidden-lines buffer low high)
    (tuatara.buffer:ensure-indexed buffer))
  (let* ((matcher (search-line-matcher line regex))
         (count-only (counting-flags-p flags)))
    ;; what the replacement needs of each match: no more is copied from the text
    (multiple-value-bind (expand texts)
        (if count-only
            (values nil nil)
            (rewrite-expander (search-line-replacement line) regex (matcher-fold matcher)
                              (tuatara.buffer:buffer-newline buffer)))
      (multiple-value-bind (rope low high since made)
          (if (or count-only joins)
              (values (tuatara.buffer:buffer-text buffer) low high nil '())
              (job-begins-where-given buffer low high))
        (let* ((job (%make-match-rewrite
                     :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                     :low low :high high :words words :what (rewrite-description line words flags)
                     :mapper (if mapper (funcall mapper texts) (rewrite-mapper-for rope matcher texts high))
                     :joins joins :report report
                     :expand expand
                     :all (every-match-flags-p flags) :count-only count-only
                     :pieces (rewrite-pieces-for rope low high)
                     :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*
                     :directory (unless count-only (tuatara.io:buffer-scratch-directory buffer)))))
          (launch-rewrite job "tuatara rewrite"
                          (format nil "octets ~:d to ~:d in ~d piece~:p" low high (length (rewrite-pieces job)))
                          :since since :made made)
          (let ((doing (format nil "~:[replacing~;counting~] ~a" count-only words)))
            (say-in-background (if slow
                                   (format nil "The pattern is slow here: ~a" doing)
                                   (string-capitalize doing :end 1))))
          job)))))
