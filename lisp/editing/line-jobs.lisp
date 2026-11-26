;;;; lisp/editing/line-jobs.lisp --- Work over the lines of a text, in the background when large.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The pieces a job over lines is made of: the line it reads at a time
;;;; (LINE-SLICE), and the functions that give each line a new text or none
;;;; (CHANGE-LINES, REWRITE-LINES), which flush-lines, keep-lines, join-line
;;;; and the case and comment commands use on large text. START-LINE-JOB, which
;;;; runs them, is in lisp/editing/rewrites.lisp.

(in-package #:tuatara.defaults)

;;; The line read at a time

(declaim (inline block-octet))
(defun block-octet (rope block block-start block-end p)
  "The octet at offset P of ROPE: from BLOCK, the octets from BLOCK-START below
BLOCK-END, when it holds it."
  (if (and (>= p block-start) (< p block-end))
      (aref block (- p block-start))
      (tuatara.text:rope-octet rope p)))

(define-record (line-slice (:constructor make-line-slice (rope)) (:copier nil))
  "A line of ROPE as work over lines reads it (START-LINE-JOB, MAP-PIECE-LINES):
START its first octet, END where its text ends (before its newline, or the
carriage return before that), NEXT the next line's start. BLOCK holds the
octets read around it, ROPE's from BLOCK-START below BLOCK-END, which
SLICE-OCTET and SLICE-OCTETS read the line from. One slice is filled again for
each line in turn: it is not kept."
  (rope nil :documentation "The text SLICE is a line of.")
  (start 0 :type fixnum :documentation "Where the line of SLICE starts.")
  (end 0 :type fixnum :documentation "Where the text of the line of SLICE ends, before its line end.")
  (next 0 :type fixnum :documentation "Where the line after the one of SLICE starts, or the end of the text.")
  (block nil :documentation "The octets read around the line of SLICE.")
  (block-start 0 :type fixnum :documentation "Where the octets read around the line of SLICE start in its text.")
  (block-end 0 :type fixnum :documentation "Where the octets read around the line of SLICE end in its text."))

(declaim (inline slice-octet))
(defun slice-octet (slice p)
  "The octet at offset P of the text SLICE is a line of: from the octets read
around the line when they hold it."
  (block-octet (line-slice-rope slice) (line-slice-block slice)
               (line-slice-block-start slice) (line-slice-block-end slice) p))

(defun slice-octets (slice &optional (from (line-slice-start slice)) (to (line-slice-end slice)))
  "Three values, octets holding the text SLICE is a line of from FROM below
TO and where that starts and ends in them. By default it is the line's text
without its line end. The octets are those read around the line when they
hold it, else the text read now."
  (declare (type fixnum from to))
  (let ((block-start (line-slice-block-start slice)))
    (if (and (>= from block-start) (<= to (line-slice-block-end slice)))
        (values (line-slice-block slice) (- from block-start) (- to block-start))
        (values (tuatara.text:rope-octets (line-slice-rope slice) :start from :end to) 0 (- to from)))))

(declaim (inline slice-next-line))
(defun slice-next-line (rope total crlf line-end block block-start block-end)
  "Where the line after the one ending at LINE-END of ROPE starts: past its
newline, and past the carriage return before it when carriage return and line
feed (CRLF); TOTAL, ROPE's length, after the last line. BLOCK holds ROPE's
octets from BLOCK-START below BLOCK-END."
  (declare (type fixnum total line-end))
  (cond ((>= line-end total) total)
        ((and crlf (= 13 (block-octet rope block block-start block-end line-end)))
         (min total (+ line-end 2)))
        (t (1+ line-end))))

(defun map-piece-lines (function rope start end crlf &optional halt)
  "Call FUNCTION with a LINE-SLICE for each line of ROPE starting from START
below END, read a block at a time (carriage return and line feed (CRLF) when a
carriage return before a newline ends them). The slice and the block it holds,
one of a megabyte kept for reading into (TUATARA.TEXT:WITH-OCTET-BLOCK), are
filled again for each line: FUNCTION must not keep them. Stop when HALT,
checked now and then, answers true."
  (let ((n 0) (total (tuatara.text:rope-octet-length rope)) (slice (make-line-slice rope)))
    (declare (type fixnum n total))
    (tuatara.text:with-octet-block (reused (* 1024 1024))
      (block lines
        (tuatara.text:map-line-slices
         (lambda (line s e sc ec line-end block block-start block-end)
           (declare (ignore s e sc ec) (type fixnum line line-end block-start block-end))
           (when (and halt (zerop (logand (incf n) 1023)) (funcall halt)) (return-from lines))
           (setf (line-slice-start slice) line
                 (line-slice-end slice) line-end
                 (line-slice-next slice) (slice-next-line rope total crlf line-end block block-start block-end)
                 (line-slice-block slice) block
                 (line-slice-block-start slice) block-start
                 (line-slice-block-end slice) block-end)
           (funcall function slice))
         rope start end 0 0 :crlf crlf :octets t :block reused)))))

;;; Lines given a new text or none

(defvar *sort-wait* 1/2
  "Seconds a sort or filter is waited for before it carries on in the background.")

(defun put-octets (out from start end)
  "Add FROM's octets from START below END at the end of OUT, an adjustable
octet vector with a fill pointer, made larger when it has no room; OUT."
  (let ((dst (fill-pointer out)))
    (when (> (+ dst (- end start)) (array-dimension out 0))
      (setf out (adjust-array out (max (* 2 (array-dimension out 0)) (+ dst (- end start) 64)))))
    (setf (fill-pointer out) (+ dst (- end start)))
    (replace out from :start1 dst :start2 start :end2 end)
    out))

(defvar *rewrite-line-limit* (* 16 1024 1024)
  "Octets of a line a filter trying a column reads (LINES-DECODED): a longer
line is tried on its first that many, so that a line of gigabytes is never
held in memory whole. A search filter counts such lines on the mode line,
and flush-lines and keep-lines by a column then delete nothing.")

(defun decode-into (octets start end holder)
  "Decode OCTETS from START below END into the string in the car of HOLDER,
a larger one put there first when it may not hold them; the number of
characters."
  (declare (type tuatara.text:octets octets) (type fixnum start end) (optimize speed))
  (let ((needed (- end start)))
    (declare (type fixnum needed))
    (when (< (length (the (simple-array character (*)) (car holder))) needed)
      (setf (car holder) (make-string (min (* 2 needed) (+ needed 1048576)))))
    (let ((string (car holder)) (n 0) (i start))
      (declare (type (simple-array character (*)) string) (type fixnum n i))
      (loop while (< i end)
            do (let ((b (aref octets i)))
                 (if (< b #x80)
                     (setf (schar string n) (code-char b) i (1+ i))
                     (multiple-value-bind (code length) (tuatara.text:octets-decode octets i end)
                       (declare (type fixnum code length))
                       (setf (schar string n) (code-char code) i (+ i length))))
                 (incf n)))
      n)))

(defun thread-own (owned make)
  "This thread's own thing among OWNED, a cons whose car lists (THREAD .
THING): made by calling MAKE the first time a thread asks."
  (let ((thread sb-thread:*current-thread*))
    (or (cdr (assoc thread (car owned) :test #'eq))
        (let ((thing (funcall make)))
          (sb-ext:atomic-push (cons thread thing) (car owned))
          thing))))

(defun filtered-line (filter keep gone &key ends crlf)
  "Two values: a function deleting each line for START-LINE-JOB that FILTER
leaves, or with KEEP each it does not (LINES-MATCHING), and a function of
\(ROPE LOW HIGH HALT) to call first. It works out which lines those are from
LOW below HIGH of ROPE, on all but two cores, as one scan of that text finds
them (LINES-MATCHING-EVERYWHERE), until HALT, a function of none, answers
true. The car of GONE counts the lines deleted, but not while a job is taken
in, when its lines are read again to follow positions onto its result. With
ENDS (RECORD-ENDS), the lines are those of records that may hold line breaks,
each kept or deleted with its record, the record counted once."
  (let ((runs (make-array 0 :element-type 'fixnum))
        (memos (list '()))
        (none (tuatara.text:make-octets 0)))
    (declare (type (simple-array fixnum (*)) runs))
    (values
     (lambda (function slice)
       ;; the run ending after the line's start, found by halving
       (let* ((line (line-slice-start slice))
              (next (line-slice-next slice))
              (k (* 2 (tuatara.runtime:bisect (k 0 (ash (length runs) -1)) (<= (aref runs (1+ (* 2 k))) line))))
              (left (and (< k (length runs)) (<= (aref runs k) line))))
         (unless (eq keep left)
           (when (and (not tuatara.runtime:*job-taken-in*)
                      (or (null ends) (= line (line-record (thread-own memos #'make-record-memo)
                                                           (line-slice-rope slice) line next ends))))
             (sb-ext:atomic-incf (car gone)))
           (funcall function line next none))))
     (lambda (rope low high halt)
       (setf runs
             (lines-matching-everywhere filter rope (make-array 2
                                                                :element-type 'fixnum
                                                                :initial-contents (list low high))
                                        crlf nil halt nil ends))))))

(defun fate-line (fate limit gone changed long &key ends crlf)
  "A function changing each line for START-LINE-JOB as FATE decides
\(REWRITE-LINES): the line's text decoded into a string each thread keeps,
FATE called on it, and the line deleted with its line end, given its new
text, or left as it is. A line of more than LIMIT octets is left as it is,
unread. The cars of GONE, CHANGED and LONG count the lines deleted, changed
and too long, but not while a job is taken in, when its lines are read again
to follow positions onto its result. With ENDS (RECORD-ENDS), the lines are
those of records that may hold line breaks: FATE, which then keeps or deletes
and changes nothing, is called on each record's whole text (RECORD-VERDICT),
and each of its lines is kept or deleted with it, the record counted once."
  (let ((holders (list '()))
        (memos (list '()))
        (none (tuatara.text:make-octets 0)))
    (lambda (function slice)
      (let ((line (line-slice-start slice))
            (line-end (line-slice-end slice))
            (next (line-slice-next slice))
            (rope (line-slice-rope slice))
            (counting (not tuatara.runtime:*job-taken-in*))
            (holder (thread-own holders (lambda () (list (make-string 256)))))
            (memo (thread-own memos #'make-record-memo)))
        (multiple-value-bind (start text-end after)
            (if ends
                (line-record memo rope line next ends)
                (values line line-end next))
          (if (and (= start line) (= after next))
              (if (> (- line-end line) limit)
                  (when counting (sb-ext:atomic-incf (car long)))
                  (multiple-value-bind (octets at end) (slice-octets slice)
                    (let* ((length (decode-into octets at end holder))
                           (new (funcall fate (car holder) length)))
                      ;; a string grown for a long line is not kept for the short ones after it
                      (when (> (length (car holder)) 1048576)
                        (setf (car holder) (make-string 256)))
                      (cond ((null new)
                             (when counting (sb-ext:atomic-incf (car gone)))
                             (funcall function line next none))
                            ((stringp new)
                             (when counting (sb-ext:atomic-incf (car changed)))
                             (funcall function line line-end (tuatara.text:string-octets new)))))))
              ;; a line of a record over several lines: the record decided once, whole
              (unless (record-verdict memo rope start text-end crlf limit holder fate)
                (when (and counting (= start line)) (sb-ext:atomic-incf (car gone)))
                (funcall function line next none))))))))

(defun too-long-words (count)
  "What is said of COUNT lines too long to read, after what was done to the
others: \"; N lines too long to look at left as they are\", or \"\" for none."
  (format nil "~[~:;; ~:*~:d line~:p too long to look at left as ~:*~[~;it is~:;they are~]~]" count))

(defun rewrite-lines-here (buffer start end per-line)
  "Change the lines of BUFFER from START below END as PER-LINE says
\(START-LINE-JOB), at once on this thread, as one change to undo."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (octets (tuatara.text:rope-octets rope :start start :end end))
         (out (make-array (length octets) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
         (pos 0)
         (edit (lambda (a z new)
                 (setf out (put-octets (put-octets out octets pos (- a start)) new 0 (length new))
                       pos (- z start)))))
    (map-piece-lines (lambda (slice) (funcall per-line edit slice)) rope start end (buffer-crlf-p buffer))
    (setf out (put-octets out octets pos (length octets)))
    (let ((result (coerce out 'tuatara.text:octets)))
      (unless (equalp result octets)
        (tuatara.buffer:buffer-replace buffer start end result)))))

(defun change-lines (buffer start end words per-line report &key prepare)
  "Change the lines of BUFFER from START below END, octets at line starts, as
PER-LINE says (START-LINE-JOB). PREPARE, when given, is called first with the
text, where the lines start and end, and a function of none answering true once
the work is stopped. The work runs in the background on all but two cores,
waited for *SORT-WAIT* seconds so that quick work looks done at once, and edits
made to the lines meanwhile are kept. A buffer whose text never goes to disk (an
encrypted file's) is changed at once instead, up to *REWRITE-THRESHOLD* octets.
One step to undo either way. REPORT, a function of the words saying how long the
job took (NIL when done at once), gives what is said once done. WORDS names the
work (\"Filtering\"). Returns the job, :QUEUED (QUEUE-IF-BUSY), or NIL. Refused
when search filters hide any of those lines (CHECK-HIDDEN-LINES)."
  (check-writable buffer)
  (check-hidden-lines buffer start end)
  (when (< start end)
    (let ((what (lines-done-words words buffer start end)))
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (cond ((tuatara.io:buffer-scratch-directory buffer)
             ;; TO is on the last line: END is that line's end, or the next one's start
             (let ((job (start-line-job buffer start (1- end) (constantly per-line)
                                        :prepare (and prepare
                                                      (lambda (job)
                                                        (declare (notinline rewrite-rope rewrite-low rewrite-high
                                                                            rewrite-cancel))
                                                        (funcall prepare
                                                                 (rewrite-rope job)
                                                                 (rewrite-low job)
                                                                 (rewrite-high job)
                                                                 (lambda () (car (rewrite-cancel job))))))
                                        :doing words :what what
                                        :verb "change the lines" :leaves "the lines are as they were"
                                        :report (lambda (job status changes seconds kept)
                                                  (declare (ignore job changes))
                                                  (unless (eq status :stopped)
                                                    (funcall report (job-done-words seconds kept)))))))
               (unless (eq job :queued) (take-in-soon job *sort-wait* what))
               job))
            ((not (too-large-here-p start end words))
             (when prepare (funcall prepare (tuatara.buffer:buffer-text buffer) start end nil))
             (rewrite-lines-here buffer start end per-line)
             (message "~a" (funcall report nil))
             nil)))))

(defun rewrite-lines (buffer start end words fate report &key ends)
  "Give each line of BUFFER from START below END, octets at line starts, what
FATE gives it. FATE, a function of a string and how many of its first
characters are the line's text (without its line end), gives NIL to delete
the line, T to keep it, or a string, its new text. The string it is given is
used again for the next line. A line of more than *REWRITE-LINE-LIMIT* octets
is left as it is, FATE never asked. The lines are done as CHANGE-LINES does
them, in the background when large, one step to undo. FATE runs on other
threads, several at once, so it changes nothing outside itself. REPORT, a
function of how many lines went and how many changed, gives what is said
once done, followed by how many were too long to look at. WORDS names the
work (\"Rearranging\"). Returns the job, :QUEUED (QUEUE-IF-BUSY), or NIL. With
ENDS (RECORD-ENDS), FATE keeps or deletes records that may hold line breaks,
each whole (FATE-LINE)."
  (let ((gone (list 0)) (changed (list 0)) (long (list 0)))
    (change-lines buffer start end words
                  (fate-line fate *rewrite-line-limit* gone changed long :ends ends :crlf (buffer-crlf-p buffer))
                  (lambda (done)
                    (format nil "~a~@[ ~a~]~a" (funcall report (car gone) (car changed)) done
                            (too-long-words (car long)))))))
