;;;; lisp/search/files.lisp --- Searching the text of many files as you type: search-in-files, search-in-formatted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The files under a directory are read on a thread of their own when the
;;;; search starts, into texts of a few megabytes each, while you type; what is
;;;; typed is searched on a thread of its own too, on nine cores in ten, each
;;;; new text searched as it comes, a new key stopping the search and starting
;;;; the next, and the lines found are the candidates as they come. Choosing
;;;; one visits its file at the match, and stops the reading and the search.

(in-package #:tuatara.defaults)

(define-setting *search-files-limit* (* 128 1024 1024)
  "The most octets of text kept in memory for a search of many files.
It holds for \\[search-in-files] and \\[search-in-formatted] and for each
directory of *INDEXED-DIRECTORIES* (and on disk with it,
*INDEXES-KEPT-ON-DISK*). Text files that do not fit are not left out but
searched from disk at each search, a piece at a time, their lines coming
after the others: memory stays at this, and a search of them takes longer,
about a second for each 1.5 GB on a fast disk, against a quarter of that kept
in memory. Raise it to keep more in memory, (* 4 1024 1024 1024) for 4 GB.
- a whole number, 0 or more: the octets; 128 MB as standard."
  :values (integer 0 *))

(define-setting *search-files-min-input* 3
  "How many characters search-in-files and search-in-formatted wait for before
searching.
- a whole number, 0 or more: the characters; 3 as standard."
  :values (integer 0 *))

(define-setting *search-files-lines-limit* 2000
  "The most matching lines search-in-files and search-in-formatted list.
Raise it to see more of a pattern that matches widely, at the cost of time
and memory to list them; lower it for a quicker list.
- a whole number, 1 or more: the lines; 2000 as standard."
  :values (integer 1 *))

(define-record (file-corpus (:constructor make-file-corpus (rope paths starts root)) (:copier nil))
  "The text of files read for searching: ROPE holds them one after another,
each ending with a newline; PATHS are their paths from ROOT, and STARTS the
offset in ROPE where each begins, in the same order."
  (rope nil :read-only t :documentation "The text of the files of a FILE-CORPUS, one after another.")
  (paths #() :type simple-vector :read-only t :documentation "The paths from the root of the files of a FILE-CORPUS.")
  (starts #() :type simple-vector :read-only t :documentation "Where each file of a FILE-CORPUS begins in its text.")
  (root "" :type string :read-only t :documentation "The directory the paths of a FILE-CORPUS are from."))

(setf (documentation 'make-file-corpus 'function) "Return the text of files read for searching (see FILE-CORPUS).")

(defun corpus-file-at (corpus position)
  "The index in CORPUS of the file holding byte POSITION of its text."
  (let ((starts (file-corpus-starts corpus)))
    (max 0 (1- (tuatara.runtime:bisect (middle 0 (length starts)) (<= (aref starts middle) position))))))

(defvar *search-input-matchers* (cons nil nil)
  "The input last searched for and its matchers, as ((INPUT REGEX) . MATCHERS),
so that a search of many files makes them once, not once a file.")

(defun search-input-problem (input regex)
  "Why INPUT cannot be searched for, as PATTERN-PROBLEM says it of a word
that is no regular expression, with REGEX; NIL when it can."
  (and regex (some (lambda (word) (pattern-problem word t)) (tuatara.text:split-words input " "))))

(defun search-input-matchers (input regex)
  "The matchers of INPUT, the longest first: with REGEX, one for each of its
words (separated by spaces, MAKE-PATTERN-MATCHER), none when a word is no
regular expression (SEARCH-INPUT-PROBLEM says why); else one for the whole of
it as literal text, spaces and all. Made once however many pieces and files
the same input is searched in, from any thread."
  (let ((key (list input regex))
        (last *search-input-matchers*))
    (if (equal (car last) key)
        (cdr last)
        ;; one cons replaced whole: a thread reads the input and its matchers together
        (cdr (setf *search-input-matchers*
                   (cons key
                         (if regex
                             ;; a word that is no regular expression finds nothing, so neither do they all
                             (let ((matchers (mapcar (lambda (word) (make-pattern-matcher word :regex t))
                                                     ;; the longest word, likely the rarest, finds the lines to look at
                                                     (stable-sort (tuatara.text:split-words input " ")
                                                                  #'> :key #'length))))
                               (and (notany #'null matchers) matchers))
                             (list (make-pattern-matcher input :regex nil)))))))))

(sb-ext:defglobal **corpus-line-endings** (make-hash-table :test 'eq :weakness :key :synchronized t)
  "For each FILE-CORPUS searched, which of its files have lines ending with a
carriage return and a newline (CORPUS-CRLF).")

(defun corpus-crlf (corpus)
  "A bit for each file of CORPUS, 1 when its lines end with a carriage return
and a newline, as its first line end in its first 64 KB tells, as visiting the
file tells it."
  (or (gethash corpus **corpus-line-endings**)
      (setf (gethash corpus **corpus-line-endings**)
            (let* ((rope (file-corpus-rope corpus)) (starts (file-corpus-starts corpus))
                   (size (tuatara.text:rope-octet-length rope))
                   (bits (make-array (length starts) :element-type 'bit)))
              (dotimes (i (length starts) bits)
                (let ((start (aref starts i))
                      (end (if (< (1+ i) (length starts)) (aref starts (1+ i)) size)))
                  (when (eq :crlf (tuatara.buffer:detect-line-ending rope :start start :end end))
                    (setf (aref bits i) 1))))))))

(defparameter *search-chunk* (* 1024 1024)
  "The least octets of text each thread searches at once for the lines of a
search (CORPUS-LINES); a text smaller than two of them is searched on one.")

(defun corpus-lines
    (corpus input &key (regex t) stop (most *search-files-lines-limit*) (start 0) end (parallel t) carry)
  "The lines of CORPUS matching INPUT: with REGEX, its words (separated by
spaces) each a regular expression, all found on the line, in any order; else
the whole of it as literal text. A line is matched by the longest word when any
part of a match of it lies on the line, from the line holding the match's first
octet through the line holding its last, a match never running from one file
into the next; each such line is given once, when the other words are found on
it too. A list of (INDEX LINE OFFSET TEXT) in file order, at most MOST
\(*SEARCH-FILES-LINES-LIMIT*), between the octets START and END of its text
\(all of it by default). INDEX is the file's index and LINE the line's number
in it (from 1). OFFSET is the offset in the line where the longest word's first
match on it begins (0 for a match coming from the line before). TEXT is the
line. A file whose lines end with a carriage return and a newline is searched
as such (CORPUS-CRLF): $ matches before the carriage return, and the line given
leaves it off. The text is cut at line starts into chunks, at most four for
each core. Each is searched on its own and their lines joined in order. A chunk
into which a match of the one before runs is searched again from where that
match ends. So a search of words found on many lines takes no longer than one
of a rare word and finds what one scan finds. With PARALLEL false, the search
runs on this thread alone. The text past END is seen by a match running on into
it, and a third value is where the last such match ends; CARRY, when given, is
where one of the text before START ended, the lines from START through it
matched and the search going on from there. NIL at once when STOP, asked every
few milliseconds on each thread, answers true, and a second value true then."
  (let* ((rope (file-corpus-rope corpus))
         (crlf (corpus-crlf corpus))
         (starts (file-corpus-starts corpus))
         (size (min (or end (tuatara.text:rope-octet-length rope)) (tuatara.text:rope-octet-length rope))))
    (labels ((file-end (index)
               (if (< (1+ index) (length starts)) (aref starts (1+ index)) (tuatara.text:rope-octet-length rope)))
             (scan (from to carry)
               ;; the lines from FROM below TO, in order, at most MOST, a match before having run on
               ;; to CARRY; T as a second value when stopped; where its last match ends when past TO
               (let* ((lines '()) (count 0) (stopped nil) (marked from) (reach (or carry 0))
                      (halt (and stop (lambda () (when (funcall stop) (setf stopped t))))))
                 (loop with index = (corpus-file-at corpus from)
                       while (and (< from to) (< count most) (not stopped))
                       do (let* ((ending (aref crlf index))
                                 ;; the files from INDEX on whose lines end alike, searched as one
                                 (next (or (loop for k from (1+ index) below (length starts)
                                                 when (/= (aref crlf k) ending) return k)
                                           (length starts)))
                                 (stretch-end (if (< next (length starts)) (min to (aref starts next)) to))
                                 (matchers (search-input-matchers input regex)))
                            (when matchers
                              (setf marked
                                    (scan-stretch from stretch-end (and carry (> carry from) carry) marked
                                                  (first matchers) (rest matchers) (= ending 1)
                                                  (lambda (line) (push line lines) (incf count) (>= count most))
                                                  (lambda (e) (setf reach (max reach e)))
                                                  halt)))
                            (setf from stretch-end index next carry nil)))
                 (values (nreverse lines) stopped (and (> reach to) reach))))
             (scan-stretch (from to carry marked first others crlf give note-end halt)
               ;; GIVE each line from FROM below TO that FIRST finds and OTHERS find on too, those
               ;; before MARKED given already, a match before having run on to CARRY: where the
               ;; next line to give may start
               (let ((done nil) (s 0) (file -1) (file-limit 0))
                 (declare (type fixnum s file file-limit))
                 (labels ((take (line-start text-end after)
                            (declare (ignore after))
                            ;; one line for each line matched, when the other words are on it too
                            (when (and (not done)
                                       (every (lambda (matcher)
                                                (map-matcher-matches (lambda (&rest match) (declare (ignore match)) t)
                                                                     rope matcher line-start text-end))
                                              others))
                              (let ((line (1+ (- (tuatara.text:byte->line rope line-start)
                                                 (tuatara.text:byte->line rope (aref starts file))))))
                                (setf done (funcall give (list file line (max 0 (- s line-start))
                                                               (tuatara.text:rope-string rope :start line-start
                                                                                              :end text-end)))))))
                          (unit-of (at from) (line-holding rope at from crlf))
                          (found (start e)
                            (setf s start)
                            (when (>= start file-limit)
                              (setf file (corpus-file-at corpus start) file-limit (file-end file)))
                            (funcall note-end e)
                            (setf marked (map-match-units #'take #'unit-of start e marked to))
                            done)
                          (scan-from (at to bound crossing)
                            ;; the matches from AT below TO seeing no text past BOUND; with
                            ;; CROSSING, one running past its file's end stops it, and is its start
                            (let ((crossed nil))
                              (map-matcher-matches
                               (lambda (start e m g)
                                 (declare (ignore m g))
                                 (if (and crossing (> e (if (>= start file-limit)
                                                            (file-end (corpus-file-at corpus start))
                                                            file-limit)))
                                     (setf crossed start)
                                     (found start e)))
                               rope first at to :bound bound :texts nil :halt halt)
                              crossed)))
                   (when carry
                     (setf file (corpus-file-at corpus from) file-limit (file-end file))
                     (found from carry)
                     (setf from carry))
                   (let ((bound (if (< from to) (file-end (corpus-file-at corpus (1- to))) to)))
                     (loop while (and (< from to) (not done))
                           do (let ((crossed (scan-from from to bound t)))
                                (if crossed
                                    ;; a match may not run into the next file: that file's rest searched alone
                                    (let ((end (file-end (corpus-file-at corpus crossed))))
                                      (scan-from crossed (min end to) end nil)
                                      (setf from end))
                                    (return)))))
                   marked))))
      (when (and stop (funcall stop))
        (return-from corpus-lines (values nil t)))
      (let* ((length (- size start))
             (chunks (if (or (not parallel) (< length (* 2 *search-chunk*)))
                         1
                         (min (* 4 (tuatara.runtime:cpu-count)) (ceiling length *search-chunk*))))
             (bounds (let ((at start))
                       (append (loop for k from 1 below chunks
                                     collect (setf at (max at (tuatara.text:rope-line-start
                                                               rope (+ start (floor (* k length) chunks))))))
                               (list size))))
             (froms (cons start (butlast bounds)))
             (results (tuatara.runtime:run-tasks
                       (coerce (loop for from in froms
                                     for to in bounds
                                     for first = t then nil
                                     collect (let ((from from) (to to) (carry (and first carry)))
                                               (lambda () (multiple-value-list (scan from to carry)))))
                               'simple-vector))))
        ;; a chunk a match of the one before runs into searched again from where that match ends
        (loop for k from 1 below (length results)
              for carry = (third (svref results (1- k)))
              for from = (nth k froms)
              when (and carry (> carry from) (not (second (svref results (1- k)))))
                do (setf (svref results k) (multiple-value-list (scan from (nth k bounds) carry))))
        (if (some #'second results)
            (values nil t)
            (let ((all (loop for (lines) across results append lines)))
              (values (subseq all 0 (min most (length all))) nil (third (svref results (1- (length results)))))))))))

(defparameter *search-disk-piece* (* 4 1024 1024)
  "How many octets of a file too large to be kept in memory for a search are
searched at a time (LINES-ON-DISK). As many again are read after it, or as far
as a match may reach (TUATARA.SYNTAX:*REGEX-LONGEST-MATCH*) when that is more.
That way a match running on past a piece's end is seen as one scan of the whole
file sees it.")

(defparameter *search-disk-longest-line* (* 64 1024 1024)
  "The longest line of a file searched from disk that is searched whole, and
how much further than the octets read ahead a match is followed
\(LINES-ON-DISK). What is read at a time grows by up to this to hold such a
line or such a match. A longer line is searched in pieces of this size, and a
match across their edge is missed.")

(defun lines-on-disk
    (path relative root input &key (regex t) cancel (most *search-files-lines-limit*) (inside "") (parallel t))
  "Completion candidates for the lines of the file at PATH matching INPUT (a
regular expression's words with REGEX, else literal text: CORPUS-LINES), as
LINE-CANDIDATES makes them (RELATIVE its path from ROOT, a directory's path
INSIDE left off). The file is read from disk and searched a piece of
*SEARCH-DISK-PIECE* octets at a time, each ending at a line end and let go of
before the next, so that a gigantic file is never held in memory whole. The
octets after a piece are read with it, a match running on into them seen whole
and the next piece searched on from where it ends, so that the lines are those
one scan of the whole file finds (what is read grows for a longer line or
match, up to *SEARCH-DISK-LONGEST-LINE* more). Nothing for a file that is no
text (READ-TEXT-FILE). At most MOST; CANCEL, a cons, stops it when its car
turns true."
  (let ((found '()) (count 0))
    (when (and (uiop:string-prefix-p inside relative)
               (multiple-value-bind (octets why) (read-text-file path :most 0)
                 (or octets (eq why :too-large))))
      (let ((fd (ignoring-errors-reported ("opening ~a to search" path) (tuatara.io:open-for-reading path t))))
        (when fd
          (unwind-protect
               (let* ((size (let ((stamp (ignoring-errors-reported ("examining ~a" path)
                                           (tuatara.io:fd-stamp fd path))))
                              (if stamp (tuatara.io:stamp-size stamp) *search-disk-piece*)))
                      (ahead (max *search-disk-piece* tuatara.syntax:*regex-longest-match*))
                      (limit (+ *search-disk-longest-line* ahead))
                      ;; no larger than the file, so that a small one costs little
                      (buffer (make-array (max 1 (min (+ *search-disk-piece* ahead) size))
                                          :element-type '(unsigned-byte 8)))
                      (offset 0) (lines-before 0) (carry nil))
                 (loop
                   (when (or (car cancel) (>= count most)) (return))
                   (let ((got (or (ignoring-errors-reported ("reading ~a to search" path)
                                    (tuatara.io:read-fd-at fd buffer offset :path path))
                                  0)))
                     (when (zerop got) (return))
                     (let* ((whole (or (< got (length buffer)) (>= (+ offset got) size)))
                            ;; the piece: up to its last line end before what is read ahead
                            (newline (and (not whole)
                                          (or (tuatara.text:octets-line-end-position-backward
                                               buffer 0 (max 0 (min got (- (length buffer) ahead))) got)
                                              (tuatara.text:octets-line-end-position-backward buffer 0 got nil))))
                            (growing (< (length buffer) limit)))
                       (if (and (not whole) (null newline) growing)
                           (setf buffer (grown-octets buffer limit))
                           (let ((end (if newline (1+ newline) got))
                                 (corpus (make-file-corpus (tuatara.text:octets->rope
                                                            (coerce (subseq buffer 0 got) 'tuatara.text:octets))
                                                           (vector relative) (vector 0) root)))
                             (multiple-value-bind (lines stopped reach)
                                 (corpus-lines corpus input :regex regex :most (- most count)
                                                            :stop (lambda () (car cancel))
                                                            :parallel parallel :end end :carry carry)
                               (declare (ignore stopped))
                               (if (and reach (>= reach got) (not whole) growing)
                                   ;; a match running to the end of what was read: read more and search again
                                   (setf buffer (grown-octets buffer limit))
                                   (progn
                                     (dolist (candidate (line-candidates corpus
                                                                         (mapcar (lambda (line)
                                                                                   (list* (first line)
                                                                                          (+ lines-before (second line))
                                                                                          (cddr line)))
                                                                                 lines)
                                                                         inside))
                                       (push candidate found)
                                       (incf count))
                                     (incf lines-before (tuatara.text:octets-line-ends buffer 0 end got))
                                     (incf offset end)
                                     (setf carry (and reach (> reach end) (- reach end))))))))))))
            (ignoring-errors-reported ("closing ~a" path) (sb-posix:close fd))))))
    (nreverse found)))

(defun grown-octets (octets limit)
  "An empty vector of octets twice as long as OCTETS, but no longer than LIMIT."
  (make-array (min limit (* 2 (length octets))) :element-type '(unsigned-byte 8)))

(defun lines-on-disk-many (files root input &key (regex t) cancel (most *search-files-lines-limit*) (inside ""))
  "LINES-ON-DISK for each of FILES, each (RELATIVE PATH), INPUT and REGEX as
it takes them, searched a file to a core at once, a batch after another: their
candidates, in the order of FILES, at most MOST; CANCEL, a cons, stops it when
its car turns true."
  (let ((found '()) (count 0)
        (batch (* 2 (tuatara.runtime:cpu-count))))
    (loop for rest on files by (lambda (list) (nthcdr batch list))
          while (and (< count most) (not (car cancel)))
          do (let* ((some (loop for file in rest repeat batch collect file))
                    (results (tuatara.runtime:run-tasks
                              (coerce (mapcar (lambda (file)
                                                (destructuring-bind (relative path) file
                                                  (lambda ()
                                                    (lines-on-disk path relative root input :regex regex :cancel cancel
                                                                                            :most most :inside inside
                                                                                            :parallel nil))))
                                              some)
                                      'simple-vector))))
               (loop for candidates across results
                     do (dolist (candidate candidates)
                          (when (< count most)
                            (push candidate found)
                            (incf count))))))
    (nreverse found)))

(defun line-candidates (corpus lines &optional (inside ""))
  "Completion candidates for LINES of CORPUS (CORPUS-LINES) as path:line: text,
each standing for (PATH LINE OFFSET TEXT), TEXT the line as listed, NIL for one cut
short (\\[search-files-list] edits the others); with INSIDE, a directory's path from
the corpus's root, only those of files in it, their paths from it."
  (loop for (index line offset text) in lines
        for path = (aref (file-corpus-paths corpus) index)
        when (uiop:string-prefix-p inside path)
          collect (let* ((shown (string-left-trim '(#\Space #\Tab) text))
                         (long (> (length shown) 200)))
                    (make-candidate (format nil "~a:~d: ~a" (subseq path (length inside)) line
                                            (if long (subseq shown 0 200) shown))
                                    :value (list (concatenate 'string (file-corpus-root corpus) path) line offset
                                                 (and (not long) shown))))))

(defparameter *search-files-piece* (* 4 1024 1024)
  "About how many octets of text each piece of the files read for a search
holds: each is searched as soon as it is read.")

(define-record (corpus-gatherer (:constructor make-corpus-gatherer (limit piece give-piece give-large)) (:copier nil))
  "How the text of files is taken in for searching, in pieces, by the search of
many files and by the directories kept read alike. LIMIT is the most octets
kept in all. PIECE is about how many each piece holds. GIVE-PIECE is a function
given each piece as it is full, its TEXTS, each (RELATIVE OCTETS STAMP), in
order, and its SIZE, their octets with a newline after each. GIVE-LARGE is one
given the RELATIVE and PATH of each text file too large for what is left of
LIMIT, to be searched from disk (LINES-ON-DISK). TOTAL is the octets kept so
far. TEXTS and SIZE are the piece being filled."
  (limit nil :documentation "The most octets of text GATHERER keeps in all.")
  (piece nil :documentation "About how many octets each piece GATHERER gives holds.")
  (give-piece nil :documentation "The function GATHERER gives each piece as it is full, its TEXTS and its SIZE.")
  (give-large nil
              :documentation "The function GATHERER gives the RELATIVE name and PATH of each text file too large for
what is left of its limit.")
  (total 0 :documentation "How many octets of text GATHERER has kept so far.")
  (texts '() :documentation "The texts of the piece GATHERER is filling, latest first, each (RELATIVE OCTETS STAMP).")
  (size 0 :documentation "The octets of the piece GATHERER is filling, with a newline after each text."))

(setf (documentation 'make-corpus-gatherer 'function) "Return a CORPUS-GATHERER.")

(defun gather-piece (gatherer)
  "Give the piece GATHERER is filling, when it holds any text, and begin another."
  (when (corpus-gatherer-texts gatherer)
    (funcall (corpus-gatherer-give-piece gatherer)
             (nreverse (corpus-gatherer-texts gatherer))
             (corpus-gatherer-size gatherer))
    (setf (corpus-gatherer-texts gatherer) '()
          (corpus-gatherer-size gatherer) 0)))

(defun gather-text-file (gatherer relative path &key stamp (octets nil given))
  "Take in the file at PATH, RELATIVE its path from the root, with GATHERER:
its text, OCTETS when GIVEN, else read (READ-TEXT-FILE) unless STAMP says it
is too large for what is left of the limit; one too large given as such, to
be searched from disk. :TEXT when its text was taken, :LARGE when it was too
large, NIL when it is not text."
  (let ((limit (corpus-gatherer-limit gatherer))
        (total (corpus-gatherer-total gatherer)))
    (flet ((large ()
             (funcall (corpus-gatherer-give-large gatherer) relative path)
             :large))
      (if (and (not given) stamp (> (+ total (tuatara.io:stamp-size stamp) 1) limit))
          (large)
          (multiple-value-bind (octets why) (if given octets (read-text-file path :most (- limit total 1)))
            (cond ((eq why :too-large) (large))
                  ((null octets) nil)
                  ((> (+ total (length octets) 1) limit) (large))
                  (t (push (list relative octets stamp) (corpus-gatherer-texts gatherer))
                     (incf (corpus-gatherer-size gatherer) (1+ (length octets)))
                     (incf (corpus-gatherer-total gatherer) (1+ (length octets)))
                     (when (>= (corpus-gatherer-size gatherer) (corpus-gatherer-piece gatherer))
                       (gather-piece gatherer))
                     :text)))))))

(defun gather-file-texts (root)
  "Start reading the text files under the directory ROOT (FILES-UNDER, what
.gitignore files leave out left out) on a thread of their own, up to
*SEARCH-FILES-LIMIT* octets, in pieces of about *SEARCH-FILES-PIECE*, each a
FILE-CORPUS given as it is read (GATHER-TEXT-FILE); the GATHERING. A text
file too large for what is left of the limit, or past the most files read
\(*INDEXED-FILES-MOST*), is given as (:LARGE RELATIVE PATH), to be searched
from disk (LINES-ON-DISK)."
  (let ((limit *search-files-limit*)
        (piece *search-files-piece*)
        (most *indexed-files-most*)
        (root (directory-name root)))
    (start-gathering
     "tuatara reading files to search"
     (lambda (give cancel)
       (flet ((large (relative path) (funcall give (list :large relative path))))
         (let ((gatherer (make-corpus-gatherer limit piece
                                               (lambda (texts size)
                                                 (funcall give (make-corpus (mapcar (lambda (text)
                                                                                      (cons (first text) (second text)))
                                                                                    texts)
                                                                            size root)))
                                               #'large)))
           (files-under root :gitignore t :cancel cancel :limit most
                             :each (lambda (path relative)
                                     (unless (car cancel)
                                       (gather-text-file gatherer relative path)))
                             :past (lambda (path relative) (large relative path)))
           (gather-piece gatherer)))))))

(defun make-corpus (texts size root)
  "A FILE-CORPUS of TEXTS, each (RELATIVE . OCTETS), SIZE octets with a
newline after each, their paths from ROOT."
  (let ((all (make-array size :element-type '(unsigned-byte 8)))
        (paths (make-array (length texts)))
        (starts (make-array (length texts)))
        (at 0))
    (loop for (path . octets) in texts
          for i from 0
          do (setf (aref paths i) path (aref starts i) at)
             (replace all octets :start1 at)
             (incf at (length octets))
             (setf (aref all at) 10)
             (incf at))
    (make-file-corpus (tuatara.text:octets->rope all) paths starts root)))

(defun corpus-range (corpus inside)
  "The octets of CORPUS's text holding the files under INSIDE, a directory's
path from its root (\"\" for all), as two values, START and END; NIL when it
holds none. The files of a directory lie together, as they were read."
  (let* ((paths (file-corpus-paths corpus))
         (starts (file-corpus-starts corpus))
         (first (position-if (lambda (path) (uiop:string-prefix-p inside path)) paths)))
    (when first
      (let ((last (or (position-if (lambda (path) (uiop:string-prefix-p inside path)) paths :from-end t) first)))
        (values (aref starts first)
                (if (< (1+ last) (length starts))
                    (aref starts (1+ last))
                    (tuatara.text:rope-octet-length (file-corpus-rope corpus))))))))

(defvar *index-searches* (list 0)
  "A cons whose car counts the searches over a directory kept read
\(*INDEXED-DIRECTORIES*) under way now, for the mode line's sign of them.")

(define-record (found-lines (:constructor make-found-lines (lines index words)) (:copier nil))
  "The lines a search of files found, as a list of places (STEP-PLACE): LINES,
a vector of (PATH LINE OFFSET), in the order listed, INDEX, the one gone to
last, and WORDS, what was searched for."
  (lines #() :type simple-vector :read-only t
         :documentation "The lines a search of files found, each (PATH LINE OFFSET).")
  (index 0 :type fixnum :documentation "The line of a FOUND-LINES gone to last.")
  (words "" :type string :read-only t
         :documentation "What the search of files that found a FOUND-LINES' lines searched for."))

(defmethod places-name ((places found-lines))
  "The search for what was searched for, shortened to a few words: the search for kiwi."
  (format nil "the search for ~a"
          (tuatara.display:shorten-string (escape-controls (found-lines-words places)) 40 :one-line t)))

(defmethod step-place ((places found-lines) direction)
  "Visit the file of the line found after the one gone to last (DIRECTION 1),
or before it (-1), at that line, as choosing it in the search does."
  (let* ((lines (found-lines-lines places))
         (next (+ (found-lines-index places) direction)))
    (if (< -1 next (length lines))
        (destructuring-bind (path line offset &optional text) (svref lines next)
          (declare (ignore text))
          (setf (found-lines-index places) next)
          (show-file-line path line :offset offset)
          (message "Line ~:d of ~:d found" (1+ next) (length lines)))
        (say-places-end places direction))))

(defvar *list-found-lines* nil
  "While \\[search-in-files] asks: a function of no arguments that ends its question
and has the lines found listed in a buffer of their own, for \\[search-files-list].")
(pushnew '*list-found-lines* *question-variables*)

(defvar *search-files-keymap* (make-keymap :name "search-files" :parent *pattern-keymap*)
  "The keys of the question \\[search-in-files] asks: those of *PATTERN-KEYMAP*, and
\\<search-files>\\[search-files-list].")

(define-command search-files-list ()
  "While searching files (\\[search-in-files]), list the lines found so far in a
buffer of their own, one each as path:line: text, to go through with
\\<search-results-mode>\\[list-visit] or to change at once, with \\[search-results-edit], and write
back into their files."
  (if *list-found-lines*
      (funcall *list-found-lines*)
      (message "Lines found are listed while searching files")))

(define-keys *search-files-keymap* '(("C-c C-l" search-files-list)))

(defun found-lines-cut (count most ended)
  "What to say of COUNT lines found by a search of files when they are listed
without all it may find, or NIL when they are all: the search stopped at MOST
\(*SEARCH-FILES-LINES-LIMIT* when it began), or had not ENDED."
  (cond ((>= count most)
         (format nil "The first ~:d lines found only: the search stops there, and more may be found ~
(*search-files-lines-limit* to see more)" count))
        ((not ended)
         (format nil "Only the ~:d line~:p found so far: the search had not ended" count))))

(defun search-files-as-read (root prompt &key preview)
  "Ask with PROMPT for words to find in the files under ROOT, read in the
background meanwhile (GATHER-FILE-TEXTS), or kept read already
\(*INDEXED-DIRECTORIES*): the lines that have them listed as they are typed,
and as more files are read; visit the chosen line's file there. Each input is
searched on a thread of its own, the lines coming in as they are found. A new
key stops it and starts the next, so that typing never waits. What is typed is
a regular expression's words, \\<pattern>\\[search-toggle-regex] switching to
literal text and back, and PROMPT is shown after the kind (Regex search files
in ~/x/: , Literal search files in ~/x/: , PATTERN-KIND-WORDS). In a directory
under one kept read, only its files' text is searched. Files past the limits
are searched from disk once the rest is (LINES-ON-DISK-MANY). Choosing, or
leaving the question, stops the reading and the search. With PREVIEW, the
file of the line selected is shown meanwhile, at that line, opened safely
\(CALL-WITH-WINDOW-PREVIEW)."
  (declare (notinline directory-index-corpora directory-index-large directory-index-root))
  (multiple-value-bind (index inside) (index-for root)
    (let* ((reading (and (null index) (gather-file-texts root)))
           (inside (or inside ""))
           ;; the pieces of text read so far, kept read (*INDEXED-DIRECTORIES*): all there at once;
           ;; the searching thread takes them under the lock as they come
           (lock (sb-thread:make-mutex :name "search pieces"))
           ;; woken when pieces come or all is read
           (more-read (sb-thread:make-waitqueue :name "search pieces read"))
           (corpora (if index (coerce (directory-index-corpora index) 'list) '()))
           ;; the text files too large to keep or past the most files, each (RELATIVE PATH), searched from
           ;; disk; those of a reading come in at the end, after LARGE-END, its last cons
           (large (if index (directory-index-large index) '()))
           (large-end (last large))
           (root-name (if index (directory-index-root index) (directory-name root)))
           (read-all (null reading))
           (editor *editor*)
           ;; the search of the input typed last, and the lines it found
           (searching nil) (searched-input nil) (found '())
           ;; true once the search of the input typed last has ended; the most lines it gives
           (searched-all nil) (most *search-files-lines-limit*)
           ;; true once the lines found are to be listed in a buffer, not one of them visited
           (listing nil)
           ;; whether what is typed is a regular expression's words, else literal text;
           ;; TOGGLED once it is switched, till the lines are worked out again
           (regex t) (toggled nil)
           ;; true until the search of the input typed last has given lines: the lines of
           ;; the input before stay shown meanwhile, so that the list and the preview do
           ;; not empty and fill again at every key
           (fresh nil)
           ;; the input and kind last looked at, and why it cannot be searched for: worked out once
           (checked (list nil)))
      (labels ((files-read () (loop for corpus in corpora sum (length (file-corpus-paths corpus))))
               (problem (input)
                 (let ((key (cons input regex)))
                   (if (equal (car checked) key)
                       (cdr checked)
                       (cdr (setf checked (cons key (search-input-problem input regex)))))))
               (piece (i cancel)
                 ;; the I-th piece, waited for while more may come; NIL when there are no more or
                 ;; CANCEL's car turned true
                 (sb-thread:with-mutex (lock)
                   (loop until (or (< i (length corpora)) read-all (car cancel))
                         do (sb-thread:condition-wait more-read lock :timeout 0.1))
                   (and (< i (length corpora)) (not (car cancel)) (nth i corpora))))
               (start-search (input)
                 (when searching (stop-gathering searching))
                 (setf searched-input (list input regex) fresh t searched-all nil
                       searching
                       (start-gathering
                        "tuatara searching files"
                        (lambda (give cancel)
                          (when index
                            (sb-ext:atomic-incf (car *index-searches*))
                            (note-sign-work :searching editor))
                          (unwind-protect
                              (let ((count 0))
                                (loop for i from 0
                                      for corpus = (piece i cancel)
                                      while (and corpus (< count most))
                                      do (multiple-value-bind (from to) (corpus-range corpus inside)
                                           (when from
                                             (let ((lines (corpus-lines corpus input :regex regex :start from :end to
                                                                                     :most (- most count)
                                                                                     :stop (lambda () (car cancel)))))
                                               (incf count (length lines))
                                               (dolist (candidate (line-candidates corpus lines inside))
                                                 (funcall give candidate))))))
                                ;; then the files too large to keep, from disk, once all is read
                                (when (< count most)
                                  (dolist (candidate (lines-on-disk-many (sb-thread:with-mutex (lock) (copy-list large))
                                                                         root-name input :regex regex :cancel cancel
                                                                         :most (- most count) :inside inside))
                                    (funcall give candidate))))
                            (when index
                              (sb-ext:atomic-decf (car *index-searches*))
                              (note-sign-work :searching editor)))))))
               (take ()
                 (let ((changed (shiftf toggled nil)) (working nil))
                   (when reading
                     (multiple-value-bind (new done) (take-gathered reading)
                       (sb-thread:with-mutex (lock)
                         (dolist (thing new)
                           (if (and (consp thing) (eq (first thing) :large))
                               (let ((end (list (rest thing))))
                                 (if large-end (setf (cdr large-end) end) (setf large end))
                                 (setf large-end end))
                               (setf corpora (append corpora (list thing)))))
                         (when done (setf read-all t))
                         (when (or new done) (sb-thread:condition-broadcast more-read)))
                       (unless done (setf working t))))
                   (when searching
                     (multiple-value-bind (new done) (take-gathered searching)
                       (cond (new
                              (setf found (if fresh new (append found new)) fresh nil changed t))
                             ;; the search ended with nothing: the lines before go too
                             ((and done fresh)
                              (setf found '() fresh nil changed t)))
                       (if done (setf searched-all t) (setf working t))))
                   ;; while files are read: how many so far
                   (values changed working (and (not read-all) (files-read)))))
               (lines (input)
                 (cond ((or (< (length (string-trim " " input)) *search-files-min-input*)
                           (problem input))
                        (when searching (stop-gathering searching) (setf searching nil))
                        (setf searched-input nil found '() fresh nil)
                        '())
                       (t
                        (unless (equal (list input regex) searched-input)
                          (start-search input))
                        found))))
        (unwind-protect
             (let ((chosen (let ((*toggle-pattern-kind* (lambda () (setf regex (not regex) toggled t)))
                                 (*list-found-lines* (lambda ()
                                                       (if found
                                                           (progn (setf listing t) (finish-minibuffer))
                                                           (message "No lines found yet")))))
                             (call-with-window-preview
                              (lambda (candidate made)
                                (destructuring-bind (path line offset &optional text) (candidate-value candidate)
                                  (declare (ignore text))
                                  (let ((buffer (preview-file-buffer path made)))
                                    (when buffer
                                      (values buffer (+ (tuatara.buffer:buffer-goto-line buffer (1- line)) offset))))))
                              (lambda (show)
                                (completing-read (lambda ()
                                                   (pattern-prompt regex prompt
                                                                   (problem (editor-minibuffer-text *editor*))))
                                                 #'lines
                                                 :separator (code-char 0) :filter nil :require-match t
                                                 :source #'take :keymap *search-files-keymap*
                                                 :preview (and preview show)))))))
               (when listing
                 (show-search-results found (or (first searched-input) "")
                                      :cut (found-lines-cut (length found) most searched-all)))
               (when (consp chosen)
                 ;; the lines found, for M-g n and M-g p to go on from the one chosen
                 (let ((lines (map 'vector #'candidate-value found)))
                   (use-places (make-found-lines lines
                                                 (or (position chosen lines :test #'eq) 0)
                                                 (or (first searched-input) ""))))
                 (destructuring-bind (path line offset &optional text) chosen
                   (declare (ignore text))
                   (show-file-line path line :offset offset))))
          (when reading (stop-gathering reading))
          (when searching (stop-gathering searching)))))))

(define-setting *project-root-markers* '(".git" ".project")
  "The names of the files or directories that mark the root of a project.
\\[search-in-files] searches all of the project a buffer's file is in, from
the nearest directory above the file holding one of them. Your home
directory, and those above it, are never a project's root.
- a list of names: the markers; (\".git\" \".project\") as standard."
  :values (list-of string))

(defun project-root (directory)
  "The root of the project DIRECTORY is in: the nearest directory from it up
holding one of *PROJECT-ROOT-MARKERS* (MARKED-DIRECTORY, as REPOSITORY-TOP
finds a repository), below the home directory (by any of its names); NIL when
there is none."
  (marked-directory directory (setting-list '*project-root-markers* *project-root-markers*) :below-home t))

(define-command search-in-files ()
  "Find text in the files of the project the current buffer's file is in (see
*PROJECT-ROOT-MARKERS*), or under its directory when it is in none, or under
the directory it lists (\\[rimu]), or *DEFAULT-DIRECTORY* for another buffer
with no file; with a prefix argument, in a directory asked for. As you type,
every line with all the words typed (each a regular expression, in any order,
ignoring case unless it has a capital) is listed as path:line: text, after
three characters. The files are read in the background as you type and searched
as they are read. \\<pattern>\\[search-toggle-regex] switches to literal text,
what is typed found as it stands, spaces and all, and back.
\\<minibuffer>\\[minibuffer-finish] visits the file at that line (the first,
once all are read, unless you moved to one), and then \\[next-place] and
\\[previous-place] go to the lines listed after it and before it; with
*preview-file-search*, the file of the line selected is shown meanwhile, at
that line, opened safely. \\<search-files>\\[search-files-list] lists the lines
found in a buffer instead, to change them and write them back into their files.
Hidden files and directories, backups, binary files and what .gitignore files
exclude are left out; the access times of those read are not changed."
  (let* ((here (buffer-directory (current-buffer)))
         (root (if (prefix-argument)
                   (read-directory-path "Search files in: " :initial (abbreviated-path here))
                   (or (project-root here) here))))
    (search-files-as-read root (format nil "search files in ~a" (abbreviated-path (directory-name root)))
                          :preview *preview-file-search*)))

(define-command search-in-formatted ()
  "Find text in *FORMATTED-FILE-DIRECTORY*, where your files with formatted
names are: every file under it, as search-in-files finds text in a directory;
the lines with the words typed are listed as you type, the files read in the
background meanwhile, \\<pattern>\\[search-toggle-regex] switching to literal text and back,
and \\<minibuffer>\\[minibuffer-finish] visits one. With
*preview-formatted-search*, the file of the line selected is shown meanwhile,
at that line, opened safely."
  (search-files-as-read *formatted-file-directory* "search formatted files" :preview *preview-formatted-search*))
