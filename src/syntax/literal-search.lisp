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

(sb-ext:define-load-time-global **fold-classes**
    (let ((table (make-hash-table)))
      (flet ((fold-together (codes)
               (let ((class (map 'string #'code-char codes)))
                 (loop for char across class do (setf (gethash char table) class)))))
        (mapc #'fold-together
              '((#x4B #x6B #x212A)              ; K k and the Kelvin sign
                (#x53 #x73 #x17F)               ; S s and the long s
                (#xB5 #x39C #x3BC)              ; the micro sign and mu
                (#xC5 #xE5 #x212B)              ; A with ring and the Angstrom sign
                (#xDF #x1E9E)                   ; the sharp s and its capital
                ;; DZ, LJ, NJ: three cases
                (#x1C4 #x1C5 #x1C6) (#x1C7 #x1C8 #x1C9) (#x1CA #x1CB #x1CC) (#x1F1 #x1F2 #x1F3)
                (#x345 #x399 #x3B9 #x1FBE)      ; iota, the one written under and the one beside
                (#x392 #x3B2 #x3D0)             ; beta and its symbol
                (#x395 #x3B5 #x3F5)             ; epsilon and the lunate one
                (#x398 #x3B8 #x3D1 #x3F4)       ; theta and its symbols
                (#x39A #x3BA #x3F0)             ; kappa and its symbol
                (#x3A0 #x3C0 #x3D6)             ; pi and its symbol
                (#x3A1 #x3C1 #x3F1)             ; rho and its symbol
                (#x3A3 #x3C2 #x3C3)             ; sigma and the final sigma
                (#x3A6 #x3C6 #x3D5)             ; phi and its symbol
                (#x3A9 #x3C9 #x2126)            ; omega and the ohm sign
                (#x412 #x432 #x1C80) (#x414 #x434 #x1C81) (#x41E #x43E #x1C82) (#x421 #x441 #x1C83)
                (#x422 #x442 #x1C84 #x1C85) (#x42A #x44A #x1C86) (#x462 #x463 #x1C87) ; Cyrillic letters of old print
                (#x1C88 #xA64A #xA64B)
                (#x1E60 #x1E61 #x1E9B)))        ; s with dot above and the long one
        ;; Roman numerals and circled letters: two cases that are no letters' cases
        (dotimes (k 16) (fold-together (list (+ #x2160 k) (+ #x2170 k))))
        (dotimes (k 26) (fold-together (list (+ #x24B6 k) (+ #x24D0 k)))))
      table)
  "The characters that Unicode simple case folding (CaseFolding.txt, statuses C
and S) folds together where comparing the two cases of a letter does not:
each to the string of all it folds with, itself among them. The capital I
with dot above folds with nothing, as that folding has it.")

(defun fold-class (char)
  "The characters CHAR is one with when case is ignored, by Unicode simple case
folding, as a string with CHAR among them, where they are more than a
letter's two cases (**FOLD-CLASSES**): the long s with s, the Kelvin sign with
k, the final sigma with sigma. NIL for every other character. O(1)."
  (values (gethash char **fold-classes**)))

(defun folds-with-ascii-p (char)
  "True when CHAR, a character past ASCII, is one with an ASCII letter when
case is ignored (FOLD-CLASS): the long s with s, the Kelvin sign with k. Text
that is all ASCII then holds matches of it, and no octets of its own stand in
every match. O(1)."
  (and (>= (char-code char) 128)
       (some (lambda (other) (< (char-code other) 128)) (or (fold-class char) ""))))

(defun fold-variants (characters)
  "The UTF-8 octets of each character past ASCII that one of CHARACTERS, a
sequence, is one with when case is ignored (FOLD-CLASS), as a list of octet
vectors without repeats; NIL when none of them has any."
  (let ((variants '()))
    (map nil (lambda (char)
               (loop for other across (or (fold-class char) "")
                     when (>= (char-code other) 128)
                       do (pushnew (tuatara.text:string-octets (string other)) variants :test #'equalp)))
         characters)
    (nreverse variants)))

(defun variant-position (chunk variants from below filled)
  "Index of the first place from FROM below BELOW in the first FILLED octets of
CHUNK where one of VARIANTS (FOLD-VARIANTS) stands whole; NIL when none. Each
is looked for as SCAN-CHUNK looks for octets, many at a time."
  (declare (type tuatara.text:octets chunk) (type fixnum from below filled))
  (let ((best below))
    (declare (type fixnum best))
    (dolist (variant variants)
      (let ((at (scan-chunk chunk variant nil from best filled)))
        (when at (setf best at))))
    (and (< best below) best)))

(defun needle-octets (needle case-fold)
  "NEEDLE as UTF-8 octets, folded when CASE-FOLD; octets given as NEEDLE are
taken as they are."
  (if (stringp needle)
      (let ((octets (tuatara.text:string-octets needle)))
        (when case-fold
          (loop for i from 0 below (length octets) do (setf (aref octets i) (aref *fold-table* (aref octets i)))))
        octets)
      (coerce needle 'tuatara.text:octets)))

(defun case-fold-p (string)
  "True when STRING has no upper-case letter, so a search for it ignores case."
  (notany #'upper-case-p string))

(defun by-pattern-p (string case-fold)
  "True when a search for STRING goes by the pattern of its text
\(LITERAL-SCANNER), not by its octets alone: when, ignoring case (CASE-FOLD),
it must fold letters past ASCII, as é with É or the final sigma with sigma
\(FOLD-CLASS); when STRING holds a newline, which matches a line end of
either kind, as \\n in a pattern does; or when it holds a raw byte, which
matches only a byte standing on its own, never one inside a character."
  (some (lambda (c)
          (or (char= c #\Newline) (tuatara.text:raw-byte-code-p (char-code c))
              (and case-fold (>= (char-code c) 128) (or (both-case-p c) (fold-class c)))))
        string))

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
                                        (case-fold (and (stringp string) (case-fold-p string)))
                                        (threads (tuatara.runtime:cpu-count)) stop)
  "Byte offset of the first occurrence of STRING in ROPE starting at or after
START and ending at or before END, and the offset after it, as two values; NIL
when there is none. CASE-FOLD ignores case (the default when STRING has no
upper-case letter): ASCII case on the octets, and for a STRING with letters
past ASCII, case of every letter, through the pattern of its text; by Unicode
simple case folding, so that s finds the long s and k the Kelvin sign too
\(FOLD-CLASS), a stretch of text holding such a character searched through
the pattern. A newline
in STRING matches a line end of either kind, and a raw byte only a byte on
its own (BY-PATTERN-P). STRING may instead be a vector of octets, found as
exactly those octets wherever they are, case kept whatever CASE-FOLD
says. The first
*SERIAL-WINDOW* octets are scanned on the calling thread; beyond that the
range is cut into pieces of *PARALLEL-PIECE* octets run in order on THREADS
threads (DIRECTIONAL-SEARCH), so a rare needle costs O(distance / THREADS).
STOP, when given, is asked every megabyte or so (+HALT-CHUNKS+); when it
answers true the search gives up and returns NIL and :STOPPED.
Complexity: O(distance scanned)."
  (declare (type fixnum start end))
  (unless (stringp string) (setf case-fold nil))
  (when (and (stringp string) (by-pattern-p string case-fold))
    (return-from search-forward
      (multiple-value-bind (found after)
          (regex-search-forward rope (literal-scanner string case-fold) :start (character-start-from rope start)
                                                                        :end end :bound end :threads threads :stop stop)
        (values found after))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle))
         (bound end)
         (end (- end (1- n)))               ; last position a match may start at, exclusive
         ;; a letter of STRING written another way (the long s): where one stands, by the pattern
         (variants (and case-fold (stringp string) (fold-variants string)))
         (span (if variants (* 3 n) n)))
    (when (and (plusp n) (< start end))
      (multiple-value-bind (hit stopped)
          (directional-search
           start end *serial-window*
           (lambda (from to halt)
             (tuatara.text:with-octet-block (chunk (+ +search-chunk+ span))
               (if variants
                   (loop
                     (let ((flagged nil))
                       (flet ((look (filled below)
                                (if (variant-position chunk variants 0 filled filled)
                                    (progn (setf flagged t) 0)
                                    (scan-chunk chunk needle case-fold 0 below filled))))
                         (declare (dynamic-extent #'look))
                         (multiple-value-bind (at why) (scan-chunks-forward rope from to chunk span #'look halt)
                           (cond ((or (null at) (not flagged)) (return (values at why)))
                                 (t (let ((stretch-end (min to (+ at +search-chunk+))))
                                      (multiple-value-bind (found after)
                                          (regex-search-forward rope (literal-scanner string t)
                                                                :start (character-start-from rope at)
                                                                :end stretch-end :bound bound :threads 1 :stop halt)
                                        (when found (return (cons found after)))
                                        (when (or (eq after :stopped) (>= stretch-end to))
                                          (return (values nil after))))
                                      (setf from stretch-end))))))))
                   (scan-range-forward rope needle case-fold from to chunk halt))))
           :stop stop :threads threads)
        (cond ((consp hit) (values (car hit) (cdr hit)))
              (hit (values hit (+ hit n)))
              (stopped (values nil :stopped)))))))

(defun scan-range-backward (rope needle case-fold start end chunk &optional halt variants by-pattern)
  "Last occurrence of NEEDLE starting in [END, START) of ROPE, scanning backward
through CHUNK; NIL when none; NIL and :STOPPED when HALT, a function asked
every +HALT-CHUNKS+ chunks, answers true. With VARIANTS (FOLD-VARIANTS), a
stretch read that holds one of them is searched by BY-PATTERN instead, a
function of the stretch's start and end giving the last match starting in
it as (START . END), or NIL; CHUNK then holds three times NEEDLE's length
past a stretch."
  (declare (type fixnum start end) (type tuatara.text:octets needle chunk))
  (let* ((n (length needle)) (span (if variants (* 3 n) n)) (total (tuatara.text:rope-octet-length rope)))
    (when (< end start)
      (loop for pos of-type fixnum = (max end (- start +search-chunk+)) then (max end (- pos +search-chunk+))
            for k of-type fixnum from 1
            do (when (and halt (zerop (mod k +halt-chunks+)) (funcall halt))
                 (return (values nil :stopped)))
               (let* ((chunk-end (min total (+ pos +search-chunk+ (1- span)) (+ start (1- span))))
                      (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk)))
                 (if (and variants (variant-position chunk variants 0 filled filled))
                     (let ((found (funcall by-pattern pos (min start (+ pos +search-chunk+)))))
                       (when found (return found)))
                     (let ((hit (scan-chunk-backward chunk needle case-fold (- (min filled (- start pos)) 1) filled)))
                       (when hit (return (+ pos hit)))))
                 (when (= pos end) (return nil)))))))

(defun search-backward (rope string &key (start (tuatara.text:rope-octet-length rope)) (end 0)
                                         (case-fold (and (stringp string) (case-fold-p string)))
                                         (threads (tuatara.runtime:cpu-count)) stop)
  "Byte offset of the last occurrence of STRING in ROPE that starts before START
and at or after END, and the offset after it, as two values; NIL when none.
Serial for the first *SERIAL-WINDOW* octets, then pieces going back, in
parallel; STRING, case and STOP as for SEARCH-FORWARD."
  (declare (type fixnum start end))
  (unless (stringp string) (setf case-fold nil))
  (when (and (stringp string) (by-pattern-p string case-fold))
    (return-from search-backward
      (multiple-value-bind (found after)
          (regex-search-backward rope (literal-scanner string case-fold) :start start :end end
                                                                         :threads threads :stop stop)
        (values found after))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle))
         (variants (and case-fold (stringp string) (fold-variants string))))
    (when (and (plusp n) (< end start))
      (multiple-value-bind (hit stopped)
          (directional-search
           start end *serial-window*
           (lambda (from to halt)
             (tuatara.text:with-octet-block (chunk (+ +search-chunk+ (if variants (* 3 n) n)))
               (scan-range-backward rope needle case-fold to from chunk halt variants
                                    (lambda (low high)
                                      (multiple-value-bind (found after)
                                          (regex-search-backward rope (literal-scanner string t)
                                                                 :start high :end low :threads 1 :stop halt)
                                        (and found (cons found after)))))))
           :backward t :stop stop :threads threads)
        (cond ((consp hit) (values (car hit) (cdr hit)))
              (hit (values hit (+ hit n)))
              (stopped (values nil :stopped)))))))
