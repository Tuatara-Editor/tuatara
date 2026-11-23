;;;; src/syntax/literal-search.lisp --- Literal search over ropes, in chunks and in parallel pieces.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Literal search runs over octets in chunks that overlap by the needle length,
;;;; so a match across a leaf or chunk boundary is found and the text is never
;;;; decoded. Directional searches go parallel beyond a serial window, as one
;;;; batch of pieces taken in order, and can be stopped between pieces, so an
;;;; incremental search gives way to the next key. Regular expressions are in
;;;; regex-scan.lisp.

(in-package #:tuatara.syntax)

(defconstant +search-chunk+ 65536 "Octets searched per chunk.")
(defconstant +regex-overlap+ 4096
  "Octets of overlap between regex chunks; the longest match found across a
chunk edge.")

(defun needle-octets (string case-fold)
  "STRING as UTF-8 octets, folded when CASE-FOLD."
  (let ((octets (tuatara.text:string-octets string)))
    (when case-fold
      (loop for i from 0 below (length octets) do (setf (aref octets i) (aref *fold-table* (aref octets i)))))
    octets))

(defun case-fold-p (string)
  "True when STRING has no upper-case letter, so a search for it ignores case."
  (notany #'upper-case-p string))

(defun by-pattern-p (string case-fold)
  "True when a search for STRING goes by the pattern of its text
\(LITERAL-SCANNER), not by its octets alone: when, ignoring case (CASE-FOLD),
it must fold letters past ASCII, as é with É; or when STRING holds a newline,
which matches a line end of either kind, as \\n in a pattern does."
  (some (lambda (c) (or (char= c #\Newline) (and case-fold (>= (char-code c) 128) (both-case-p c)))) string))

(defun character-start-from (rope byte)
  "The first character's start of ROPE at or after BYTE."
  (if (tuatara.text:char-boundary-p rope byte) byte (tuatara.text:rope-next-boundary rope byte)))

(defun literal-scanner (string case-fold)
  "A scanner for STRING's text, nothing in it special, ignoring case when
CASE-FOLD (COMPILE-REGEX)."
  (compile-regex (cl-ppcre:quote-meta-chars string) :case-fold case-fold))

(defvar *serial-window* (* 4 1024 1024)
  "Octets a directional search scans on the calling thread before going
parallel; a common needle is found here without waking the pool.")

(defvar *parallel-piece* (* 2 1024 1024)
  "Octets per task when a directional search goes parallel. The pieces of the
whole range form one batch that the pool takes in order, so small pieces cost
little and keep every thread busy on a file of tens of megabytes; a slow regex
scans a piece in a few tens of milliseconds, which bounds how long a search
takes to notice STOP.")

(defun run-pieces (count function stop threads)
  "Call FUNCTION with each piece index below COUNT and a HALT function, as
one batch on the worker pool and the calling thread, taken in index order, on
up to THREADS threads, and return the vector of what it gave for each. HALT
answers true once the work is to give up: STOP, when given, is asked through
it on the calling thread only, and when STOP answers true every piece's HALT
does, pieces not yet begun are skipped, a piece inside one long stoppable call
is cut short (TUATARA.RUNTIME:STOPPABLE), and the values are NIL and
:STOPPED. A piece should ask HALT every few milliseconds of work. STOP is also
asked once before the pieces start, so it is heard even when the pool's
workers take every piece."
  (when (and stop (funcall stop))
    (return-from run-pieces (values nil :stopped)))
  (let* ((cancel (list nil)) (caller sb-thread:*current-thread*)
         (halt (lambda ()
                 (or (car cancel)
                     (and stop (eq sb-thread:*current-thread* caller) (funcall stop)
                          (tuatara.runtime:stop-work cancel)))))
         (tasks (make-array count)))
    (dotimes (k count)
      (setf (aref tasks k) (let ((k k)) (lambda () (funcall function k halt)))))
    (let ((results (tuatara.runtime:stoppable-work (cancel)
                     ;; work through much of a large file reads it through once
                     (tuatara.text:with-read-pass ()
                       (tuatara.runtime:run-tasks tasks :parallelism (max 1 threads))))))
      (if (car cancel) (values nil :stopped) results))))

(defun search-pieces (count function stop threads)
  "The result of FUNCTION, called with a piece index and a HALT function, for
the lowest index below COUNT that gives one, or NIL when none does. The pieces
run as RUN-PIECES runs them, STOP and HALT as there; once a piece gives a
result the pieces after it are skipped. When STOP answers true the values are
NIL and :STOPPED."
  (let ((best (list count)))
    (multiple-value-bind (results stopped)
        (run-pieces count
                    (lambda (k halt)
                      (unless (or (> k (car best)) (funcall halt))
                        (let ((result (funcall function k halt)))
                          (when result
                            ;; lower the index pieces must stay below to be worth running
                            (loop for old = (car best)
                                  while (< k old)
                                  until (eql old (sb-ext:compare-and-swap (car best) old k))))
                          result)))
                    stop threads)
      (if stopped
          (values nil :stopped)
          (find-if-not #'null results)))))

(defun directional-search (start end serial scan &key backward stop threads)
  "What SCAN finds first going from START towards END, forward or, when
BACKWARD, back (START then above END); NIL when it finds nothing, NIL and
:STOPPED when STOP answers true. SCAN is a function of (FROM TO HALT) giving
the first match starting in [FROM, TO) when going forward, the last when
going back, or NIL; it gives up when HALT answers true. The first SERIAL
octets are scanned on the calling thread, STOP asked through HALT; the rest in
pieces of *PARALLEL-PIECE* octets run in order on THREADS threads
\(SEARCH-PIECES)."
  (declare (type fixnum start end serial) (type function scan))
  (let* ((halted nil)
         (halt (and stop (lambda () (or halted (setf halted (and (funcall stop) t))))))
         (serial-end (if backward (max end (- start serial)) (min end (+ start serial))))
         (hit (if backward (funcall scan serial-end start halt) (funcall scan start serial-end halt))))
    (cond (hit hit)
          (halted (values nil :stopped))
          ((= serial-end end) nil)
          (t (let ((piece *parallel-piece*))
               (search-pieces (ceiling (abs (- end serial-end)) piece)
                              (lambda (k halt)
                                (if backward
                                    (let ((b (- serial-end (* k piece))))
                                      (funcall scan (max end (- b piece)) b halt))
                                    (let ((a (+ serial-end (* k piece))))
                                      (funcall scan a (min end (+ a piece)) halt))))
                              stop threads))))))

(defconstant +halt-chunks+ 16
  "Chunks a scan reads between asking whether to give up: a megabyte, a
fraction of a millisecond in most text, a few milliseconds where what is
looked for begins with nearly every octet.")

(defun scan-chunks-forward (rope start end chunk n look &optional halt)
  "The first place starting in [START, END) of ROPE that LOOK finds, reading
ROPE into CHUNK +SEARCH-CHUNK+ octets at a time and N - 1 more, for what is
looked for to be seen whole across the edge. LOOK is a function of the octets
filled and the index below which a place must start. It gives the index in
CHUNK of the first, or NIL. Returns NIL when none, and NIL and :STOPPED when
HALT, a function asked every +HALT-CHUNKS+ chunks, answers true."
  (declare (type fixnum start end n) (type tuatara.text:octets chunk) (type function look))
  (let ((total (tuatara.text:rope-octet-length rope)))
    (loop for pos of-type fixnum from start below end by +search-chunk+
          for k of-type fixnum from 1
          do (when (and halt (zerop (mod k +halt-chunks+)) (funcall halt))
               (return (values nil :stopped)))
             (let* ((chunk-end (min total (+ pos +search-chunk+ (1- n))))
                    (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                    (hit (funcall look filled (min filled +search-chunk+ (- end pos)))))
               (when hit (return (+ pos hit))))
          finally (return nil))))

(defun scan-range-forward (rope needle case-fold start end chunk &optional halt)
  "First occurrence of NEEDLE starting in [START, END) of ROPE, reading through
CHUNK; NIL when none; NIL and :STOPPED when HALT answers true
\(SCAN-CHUNKS-FORWARD)."
  (declare (type fixnum start end) (type tuatara.text:octets needle chunk))
  (flet ((look (filled below) (scan-chunk chunk needle case-fold 0 below filled)))
    (declare (dynamic-extent #'look))
    (scan-chunks-forward rope start end chunk (length needle) #'look halt)))

(defun scan-shape-chunk (chunk shape from below filled)
  "Index of the first place from FROM below BELOW in the first FILLED octets
of CHUNK where SHAPE, a vector of octet sets (REQUIRED-SHAPE), stands whole;
NIL when none."
  (declare (type tuatara.text:octets chunk) (type simple-vector shape) (type fixnum from below filled)
           (optimize speed))
  (let* ((n (length shape))
         (first (svref shape 0))
         (last (- (min below (1+ (- filled n))) 1)))
    (declare (type simple-bit-vector first) (type fixnum n last))
    (loop for i of-type fixnum from from to last
          when (and (= 1 (sbit first (aref chunk i)))
                    (loop for k of-type fixnum from 1 below n
                          always (= 1 (sbit (the simple-bit-vector (svref shape k)) (aref chunk (+ i k))))))
            return i)))

(defun scan-shape-forward (rope shape start end chunk)
  "First place starting in [START, END) of ROPE where SHAPE, a vector of octet
sets, stands whole, reading through CHUNK; NIL when none."
  (declare (type fixnum start end) (type simple-vector shape) (type tuatara.text:octets chunk))
  (flet ((look (filled below) (scan-shape-chunk chunk shape 0 below filled)))
    (declare (dynamic-extent #'look))
    (scan-chunks-forward rope start end chunk (length shape) #'look)))

(defun search-forward (rope string &key (start 0) (end (tuatara.text:rope-octet-length rope))
                                        (case-fold (case-fold-p string)) (threads (tuatara.runtime:cpu-count)) stop)
  "Byte offset of the first occurrence of STRING in ROPE starting at or after
START and ending at or before END, and the offset after it, as two values; NIL
when there is none. CASE-FOLD ignores case (the default when STRING has no
upper-case letter): ASCII case on the octets, and for a STRING with letters
past ASCII, case of every letter, through the pattern of its text. A newline
in STRING matches a line end of either kind (BY-PATTERN-P). The first
*SERIAL-WINDOW* octets are scanned on the calling thread; beyond that the
range is cut into pieces of *PARALLEL-PIECE* octets run in order on THREADS
threads (DIRECTIONAL-SEARCH), so a rare needle costs O(distance / THREADS).
STOP, when given, is asked every megabyte or so (+HALT-CHUNKS+); when it
answers true the search gives up and returns NIL and :STOPPED.
Complexity: O(distance scanned)."
  (declare (type fixnum start end))
  (when (by-pattern-p string case-fold)
    (return-from search-forward
      (multiple-value-bind (found after)
          (regex-search-forward rope (literal-scanner string case-fold) :start (character-start-from rope start)
                                                                        :end end :bound end :threads threads :stop stop)
        (values found after))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle))
         (end (- end (1- n))))              ; last position a match may start at, exclusive
    (when (and (plusp n) (< start end))
      (multiple-value-bind (hit stopped)
          (directional-search start end *serial-window*
                              (lambda (from to halt)
                                (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
                                  (scan-range-forward rope needle case-fold from to chunk halt)))
                              :stop stop :threads threads)
        (cond (hit (values hit (+ hit n)))
              (stopped (values nil :stopped)))))))

(defun scan-range-backward (rope needle case-fold start end chunk &optional halt)
  "Last occurrence of NEEDLE starting in [END, START) of ROPE, scanning backward
through CHUNK; NIL when none; NIL and :STOPPED when HALT, a function asked
every +HALT-CHUNKS+ chunks, answers true."
  (declare (type fixnum start end) (type tuatara.text:octets needle chunk))
  (let ((n (length needle)) (total (tuatara.text:rope-octet-length rope)))
    (when (< end start)
      (loop for pos of-type fixnum = (max end (- start +search-chunk+)) then (max end (- pos +search-chunk+))
            for k of-type fixnum from 1
            do (when (and halt (zerop (mod k +halt-chunks+)) (funcall halt))
                 (return (values nil :stopped)))
               (let* ((chunk-end (min total (+ pos +search-chunk+ (1- n)) (+ start (1- n))))
                      (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                      (hit (scan-chunk-backward chunk needle case-fold (- (min filled (- start pos)) 1) filled)))
                 (when hit (return (+ pos hit)))
                 (when (= pos end) (return nil)))))))

(defun search-backward (rope string &key (start (tuatara.text:rope-octet-length rope)) (end 0)
                                         (case-fold (case-fold-p string)) (threads (tuatara.runtime:cpu-count)) stop)
  "Byte offset of the last occurrence of STRING in ROPE that starts before START
and at or after END, and the offset after it, as two values; NIL when none.
Serial for the first *SERIAL-WINDOW* octets, then pieces going back, in
parallel, case and STOP as for SEARCH-FORWARD."
  (declare (type fixnum start end))
  (when (by-pattern-p string case-fold)
    (return-from search-backward
      (multiple-value-bind (found after)
          (regex-search-backward rope (literal-scanner string case-fold) :start start :end end
                                                                         :threads threads :stop stop)
        (values found after))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle)))
    (when (and (plusp n) (< end start))
      (multiple-value-bind (hit stopped)
          (directional-search start end *serial-window*
                              (lambda (from to halt)
                                (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
                                  (scan-range-backward rope needle case-fold to from chunk halt)))
                              :backward t :stop stop :threads threads)
        (cond (hit (values hit (+ hit n)))
              (stopped (values nil :stopped)))))))
