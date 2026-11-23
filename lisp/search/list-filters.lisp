;;;; lisp/search/list-filters.lisp --- A list shown narrowed to the entries its filters leave.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The text, spans, buttons and entries of a list made with RENDER-PIECES,
;;;; cut down to what the filters leave, with the count of entries left in the
;;;; heading, and made again when the list is.

(in-package #:tuatara.defaults)

(defun list-on-screen (buffer filtering source chain)
  "The entries of SOURCE, the whole list FILTERING narrows, from the one at
the top of BUFFER's window on, that every filter of CHAIN matches by its
line, as many as fill the window, or as are found within
*SEARCH-FILTER-SCREEN-SECONDS*: their bits and how many, those before it
and after the last looked at left out. The lines are matched a few hundred
at a time, each time as LIST-LINES-MATCHED matches them. What more a list
gives of its entries is left to the background, and so are the filters
looking at a part of them."
  (let* ((entries (list-source-entries source))
         (lines (list-source-entry-lines source))
         (total (length entries))
         (bits (make-array total :element-type 'bit :initial-element 0))
         (window (and *editor* (buffer-window buffer)))
         (top (or (and window (let ((entry (list-entry-at buffer (tuatara.display:window-start window))))
                                (and entry (position entry entries))))
                  0))
         (rows (window-rows-of buffer))
         (deadline (screen-deadline))
         (count 0) (i top))
    (when (some #'search-filter-where chain)
      (return-from list-on-screen (values bits 0)))
    (loop while (and (< i total) (< count rows) (<= (get-internal-real-time) deadline))
          do (let* ((high (let ((at (min total (+ i (max 256 (* 4 rows))))))
                            (if (< at total) (entry-end entries (1- at)) at)))
                    (left (make-array (- high i) :element-type 'bit :initial-element 0)))
               ;; the lines every filter so far leaves, from the entries' lines left before
               (loop for k from i below high
                     unless (heading-entry-p filtering (svref entries k)) do (setf (sbit left (- k i)) 1))
               (dolist (filter chain)
                 (let ((matched (list-lines-matched filter lines i high (lambda (k) (= 1 (sbit left (- k i))))))
                       (k i))
                   (loop while (< k high)
                         do (let ((end (entry-end entries k)))
                              (when (= 1 (sbit left (- k i)))
                                (unless (eq (not (search-filter-hide filter))
                                            (and (find 1 matched :start (- k i) :end (- end i)) t))
                                  (fill left 0 :start (- k i) :end (- end i))))
                              (setf k end)))))
               (loop for k = i then (entry-end entries k)
                     while (< k high)
                     when (= 1 (sbit left (- k i)))
                       do (fill bits 1 :start k :end (entry-end entries k))
                          (incf count))
               (setf i high)))
    (values bits count)))

;;; The list shown narrowed

(defun list-shown-lines (filtering source &optional folded)
  "Which lines of SOURCE's text FILTERING shows, as a function of a line's
number. Shown are the lines before and after the entries, the entries its last
worked out filter leaves, and lines of no entry. Also shown are the headings of
sections with an entry shown, or with none at all unless the list says otherwise
\(EMPTY-HEADINGS), and the lines that go with a heading shown
\(DECORATION-ENTRY-P). But no line is shown under the heading of a section of
FOLDED (its names, as LIST-FOLDED gives them), whose heading alone shows, nor
the heading of a section it holds; the heading of a section holding others
shows when one of theirs does. An empty line of no entry, as between sections,
is shown only between lines shown, one of a run."
  (let* ((entries (list-source-entries source))
         (shown (list-filtering-shown filtering))
         (first (list-source-first-line source))
         (keep (make-array (length entries) :element-type 'bit :initial-element 1)))
    (when shown
      ;; from the end, so that a heading knows whether an entry under it is shown
      ;; a section held in another (:SECTION NAME PARENT) passes on to it what it holds
      (let ((any nil) (under nil) (empty (list-filtering-empty-headings filtering))
            (held (make-hash-table :test 'equal)))
        (loop for i from (1- (length entries)) downto 0
              for entry = (svref entries i)
              do (cond ((null entry))
                       ((decoration-entry-p filtering entry))
                       ((heading-entry-p filtering entry)
                        (let ((section (and (consp entry) (eq (first entry) :section) (rest entry))))
                          (multiple-value-bind (kept holds) (gethash (first section) held)
                            (when holds (setf any (or any kept) under t)))
                          (setf (sbit keep i) (if (or any (and empty (not under))) 1 0))
                          (when (second section)
                            (setf (gethash (second section) held)
                                  (or (gethash (second section) held) (= 1 (sbit keep i))))))
                        (setf any nil under nil))
                       ((= 1 (sbit shown i)) (setf any t under t))
                       (t (setf (sbit keep i) 0 under t)))))
      ;; a line that goes with a heading shows as that heading does
      (when (list-filtering-decoration filtering)
        (let ((heading-kept 1))
          (loop for i from 0 below (length entries)
                for entry = (svref entries i)
                do (cond ((decoration-entry-p filtering entry) (setf (sbit keep i) heading-kept))
                         ((heading-entry-p filtering entry) (when entry (setf heading-kept (sbit keep i)))))))))
    ;; a folded section's lines hidden, its heading kept: the list has them all while filters narrow it
    (when folded
      ;; a section held in a folded one is hidden, heading and all
      (let ((hiding nil) (closed (make-hash-table :test 'equal)))
        (loop for i from 0 below (length entries)
              for entry = (svref entries i)
              do (cond ((and (consp entry) (eq (first entry) :section))
                        (let ((within (and (third entry) (gethash (third entry) closed))))
                          (when within (setf (sbit keep i) 0))
                          (setf hiding (or within (and (member (second entry) folded :test #'equal) t))
                                (gethash (second entry) closed) hiding)))
                       ((and hiding (or entry (plusp (length (svref (list-source-entry-lines source) i)))))
                        (setf (sbit keep i) 0))))))
    (when (or shown folded)
      (let ((lines (list-source-entry-lines source)) (before nil) (blank nil))
        (loop for i from 0 below (length entries)
              do (cond ((and (null (svref entries i)) (zerop (length (svref lines i))))
                        (setf (sbit keep i) 0)
                        (when (and before (not blank)) (setf blank i)))
                       ((= 1 (sbit keep i))
                        (when blank (setf (sbit keep blank) 1 blank nil))
                        (setf before t))))))
    (lambda (line)
      (let ((i (- line first)))
        (or (not (< -1 i (length entries))) (= 1 (sbit keep i)))))))

(defun moved-spans (spans segments)
  "SPANS, each (START END . MORE) in octets of a text, as they fall in the
text made of its SEGMENTS, each (FROM TO AT): the octets FROM below TO put at
AT; a span cut where a segment ends, the parts of it outside the segments
left out. Both in the order of their starts."
  (let ((out '()) (segments (coerce segments 'simple-vector)))
    (dolist (span (coerce spans 'list))
      (destructuring-bind (start end . more) span
        (let ((i (tuatara.runtime:bisect (middle 0 (length segments)) (<= (second (svref segments middle)) start))))
          (loop for k from i below (length segments)
                for (from to at) = (svref segments k)
                while (< from end)
                do (let ((s (max start from)) (e (min end to)))
                     (when (< s e) (push (list* (+ at (- s from)) (+ at (- e from)) more) out)))))))
    (nreverse out)))

(defun filter-left (filtering total)
  "How many of TOTAL entries or records FILTERING's filters leave, or NIL
while they are being worked out in the background or wait to be."
  (unless (or (list-filtering-job filtering) (list-filtering-due filtering))
    (if (list-filtering-shown filtering) (list-filtering-shown-count filtering) (or total 0))))

(defun filter-total (filtering)
  "How many entries FILTERING's list has, or records its file has; NIL while
not known."
  (if (list-filtering-lines filtering)
      (list-filtering-total filtering)
      (listed-entry-count filtering (list-filtering-source filtering))))

(defun filter-count-words (filtering total)
  "What FILTERING's filters leave of TOTAL entries or records (NIL while not
known), for a list's first line and a file's mode line, as in Filtered by kiwi,
nz: 12 of 1,000 records. While they are being worked out in the background,
it reads Filtered by kiwi: ... of 1,000 records (counting)."
  (let ((patterns (mapcar #'search-filter-words (search-filter-chain filtering)))
        (nouns (list-filtering-nouns filtering))
        (left (filter-left filtering total)))
    (if left
        (format nil "Filtered by ~{~a~^, ~}: ~:d of ~:d ~a" patterns left (or total 0) nouns)
        (format nil "Filtered by ~{~a~^, ~}: …~@[ of ~:d~] ~a (counting)" patterns total nouns))))

(defun filter-count-figures (buffer)
  "What the filter being typed on BUFFER, a list or a file of records, and the
ones kept leave of it, as its question shows that before its prompt: 12/1000;
NIL while not known, or with nothing typed."
  (let ((filtering (list-filtering buffer)))
    (when (and filtering (list-filtering-typing filtering) (list-filtering-source filtering))
      (let* ((total (filter-total filtering))
             (left (and total (filter-left filtering total))))
        (and left (format nil "~d/~d" left total))))))

(defun filter-summary (filtering)
  "The line a list narrowed by FILTERING's filters starts with: the patterns,
how many of its entries are shown (FILTER-COUNT-WORDS), and the keys for the
filters."
  (substitute-command-keys
   (format nil "~a; \\[search-filter-list] lists the filters, \\[search-filter-remove-all] takes them away~%"
           (filter-count-words filtering (listed-entry-count filtering (list-filtering-source filtering))))))

(defun filtered-list-text (filtering &optional folded)
  "The text FILTERING's whole list shows narrowed, as five values: text, spans,
buttons, entries and the line of the first, a line saying how it is
narrowed above the rest; the lines of the sections FOLDED hidden under
their headings (LIST-SHOWN-LINES). It ends in a line break when the whole
list does, so that its last line is an entry's as the whole list's is.
O(size of the text); while the filters are still worked out, only what is
known, the entries from the top of the window, is gone through, up to the
last of them shown (O(size of that part)): the whole text is made once they
are worked out."
  (let* ((source (list-filtering-source filtering))
         (text (list-source-text source))
         (shows (list-shown-lines filtering source folded))
         (last (let ((shown (list-filtering-shown filtering)))
                 (and (or (list-filtering-job filtering) (list-filtering-due filtering))
                      (typep shown 'bit-vector)
                      (+ (list-source-first-line source) (or (position 1 shown :from-end t) -1)))))
         (summary (filter-summary filtering))
         (summary-octets (tuatara.text:string-octet-length summary))
         (entries (list-source-entries source))
         (first (list-source-first-line source))
         (out (make-string-output-stream))
         (segments '()) (kept '()) (octet 0) (at summary-octets) (start 0) (line 0) (line-break nil))
    (write-string summary out)
    ;; each line's break written before the next line shown, so that none ends the text the whole list does not end in
    (loop while (and (< start (length text)) (or (null last) (<= line last)))
          do (let* ((end (let ((e (position #\Newline text :start start))) (if e (1+ e) (length text))))
                    (body (if (char= (char text (1- end)) #\Newline) (1- end) end))
                    (octets (tuatara.text:string-octet-length text :start start :end end)))
               (when (funcall shows line)
                 (when line-break
                   (write-char #\Newline out)
                   (incf at))
                 (write-string text out :start start :end body)
                 (push (list octet (+ octet (- octets (- end body))) at) segments)
                 (incf at (- octets (- end body)))
                 (setf line-break (< body end))
                 (let ((i (- line first)))
                   (when (< -1 i (length entries)) (push (svref entries i) kept))))
               (incf octet octets)
               (setf start end)
               (incf line)))
    (when (and line-break (char= (char text (1- (length text))) #\Newline))
      (write-char #\Newline out))
    (let ((segments (nreverse segments)))
      (values (get-output-stream-string out)
              (cons (list 0 (max 0 (1- summary-octets)) :list-filters)
                    (moved-spans (list-source-spans source) segments))
              (moved-spans (list-source-buttons source) segments)
              (nreverse kept)
              (1+ first)))))

(defun filter-list-text (buffer text spans buttons entries first-line capture)
  "What BUFFER, a list made with ENTRIES from line FIRST-LINE of TEXT (with its
SPANS and BUTTONS), shows while search filters narrow it: the five as they
are when none do; else the entries the filters leave, under a line saying
so (FILTERED-LIST-TEXT). With CAPTURE, TEXT is the list made anew, kept as
the whole list and narrowed again: a list searched in the background, drawn
again with no entry it had not, keeps what the filters left of it
\(CARRY-FILTERS-OVER)."
  (let ((filtering (list-filtering buffer)))
    (cond ((or (null filtering) (null (search-filter-chain filtering)))
           (when filtering
             (stop-filtering-job filtering)
             (setf (list-filtering-source filtering) nil (list-filtering-shown filtering) nil))
           (values text spans buttons entries first-line))
          (t (when (or capture (null (list-filtering-source filtering)))
               (let ((old (list-filtering-source filtering))
                     (new (make-list-source text spans buttons (coerce entries 'simple-vector) first-line)))
                 (setf (list-filtering-source filtering) new)
                 ;; a list searched in the background, drawn again with the same entries: its matches kept
                 (if (and old (null (list-filtering-job filtering)) (not (filter-quickly-p filtering))
                          (carry-filters-over filtering old new))
                     (shown-as-last-filter filtering)
                     (progn (setf (list-filtering-shown filtering) nil)
                            (work-out-filters buffer filtering)))))
             (filtered-list-text filtering (list-folded buffer))))))

(defun drop-search-filters (buffer)
  "Take every search filter off BUFFER, a list, and the one being worked out,
without showing it again: for a list about to show other entries altogether,
which it then shows whole. In a file whose lines they hid, every line is
shown again, and the filtering forgotten, as when it leaves the mode whose
records they were (FILTERS-HIDE-LINES-P)."
  (let ((filtering (list-filtering buffer)))
    (when filtering
      (stop-filtering-job filtering)
      (setf (list-filtering-filters filtering) '() (list-filtering-typing filtering) nil
            (list-filtering-source filtering) nil (list-filtering-shown filtering) nil)
      (when (list-filtering-lines filtering)
        (tuatara.display:hide-ranges buffer nil)
        (setf *buffers-filtered-by-lines* (remove buffer *buffers-filtered-by-lines*)
              (tuatara.buffer:buffer-local buffer 'list-filtering) nil)))))

(defun list-filters-on-p (buffer)
  "True when search filters narrow BUFFER, a list: it is then made with the
members of its folded sections too, which the filtering hides, so that what
the filters leave is counted and judged in the whole list."
  (let ((filtering (list-filtering buffer)))
    (and filtering (search-filter-chain filtering) t)))

(defun keys-hidden-by-filters (list keys)
  "Those of KEYS, the mark keys of entries of LIST (ENTRY-MARK-KEY), whose
entries the search filters narrowing LIST hide now, in the order of KEYS:
what a command on the marked entries takes though they are not on the
screen. A key of no entry of the whole list (a file marked in another
directory) is not hidden by them. NIL when filters narrow LIST not. O(entries)."
  (let ((source (and (list-filters-on-p list) (list-filtering-source (list-filtering list)))))
    (when source
      (let ((hidden (make-hash-table :test 'equal)))
        (loop for entry across (list-source-entries source)
              do (let ((key (entry-mark-key list entry))) (when key (setf (gethash key hidden) t))))
        (loop for entry across (coerce (tuatara.buffer:buffer-local list 'list-entries #()) 'vector)
              do (let ((key (entry-mark-key list entry))) (when key (remhash key hidden))))
        (remove-if-not (lambda (key) (gethash key hidden)) keys)))))

(defun show-filtered-list (buffer)
  "Show BUFFER, a list, as its search filters narrow it now, from the whole
list it keeps, point on the entry it was on while that is shown, what it
was made from (its key, KEEP-SHOWN-CURRENT) as it was; with its filters gone
and sections folded, made again, folded as they are."
  (let* ((filtering (list-filtering buffer))
         (source (and filtering (list-filtering-source filtering))))
    (when (and source (null (search-filter-chain filtering)) (list-folded buffer))
      (stop-filtering-job filtering)
      (setf (list-filtering-source filtering) nil (list-filtering-shown filtering) nil)
      (redraw-list buffer)
      (return-from show-filtered-list))
    (when source
      (show-list-text buffer (list-source-text source) (list-source-spans source) (list-source-buttons source)
                      :entries (list-source-entries source) :first-line (list-source-first-line source)
                      :key (tuatara.buffer:buffer-local buffer 'list-key) :capture nil))))

(defun refilter-list (buffer)
  "Narrow BUFFER, a list, again after its filters changed: at once where
that is quick, else showing it as it was till the filters are worked out in
the background."
  (let ((filtering (list-filtering buffer)))
    (cond ((null filtering))
          ((null (list-filtering-source filtering))
           ;; narrowed for the first time: its text now is the whole list
           (when (search-filter-chain filtering)
             (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries))
                   (made (tuatara.buffer:buffer-local buffer 'made-spans)))
               (show-list-text buffer (tuatara.buffer:buffer-string buffer)
                               (and made (coerce (car made) 'list))
                               (tuatara.buffer:buffer-local buffer :buttons)
                               :entries entries :first-line (tuatara.buffer:buffer-local buffer 'list-first-line 0)
                               :key (tuatara.buffer:buffer-local buffer 'list-key)))))
          (t (work-out-filters buffer filtering)
             (show-filtered-list buffer)))))
