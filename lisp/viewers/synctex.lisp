;;;; lisp/viewers/synctex.lisp --- SyncTeX files read: where each line of a LaTeX file went on the pages.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A LaTeX program run with -synctex writes, beside the PDF, a file saying
;;;; for each page where each box, glue, kern and rule was put and which line
;;;; of which file made it. READ-SYNCTEX reads that file as data alone: every
;;;; number held to 32 bits, every count to a limit, a line it does not know
;;;; passed over, nothing in it run. With the PDF's page boxes, SYNCTEX-PLACE
;;;; finds where a line of a file is on the pages, and SYNCTEX-LINE which line
;;;; made a point of a page, both in fractions of the page as it is shown.
;;;; The format is the one the SyncTeX parser of TeX Live reads.

(in-package #:tuatara.defaults)

(defparameter *synctex-largest* (* 256 1024 1024)
  "The most octets of a SyncTeX file read: a larger one is refused.")

(defparameter *synctex-most-records* 4000000
  "The most boxes, glues, kerns and rules of a SyncTeX file kept: past them
the rest of the pages are left out.")

(defparameter *synctex-most-pages* 100000
  "The most pages of a SyncTeX file kept: those past them are left out.")

(defparameter *synctex-most-inputs* 100000
  "The most input files a SyncTeX file may name: those past them are left out.")

(defparameter *synctex-most-depth* 1000
  "How deep boxes within boxes of a SyncTeX file are followed: those deeper
are taken as within the last box followed.")

(defconstant +synctex-stride+ 8
  "The numbers each record of a SYNCTEX takes: kind and input, line, x, y,
width, height, depth, and the record of the box holding it.")

(defparameter *synctex-kinds* "(hvrkg$x"
  "The records of a SyncTeX file kept, by their first character, each kind its
place in the string: a box of a line and the void boxes, rules, kerns,
glues, mathematics and points between them.")

(defstruct (synctex (:constructor make-synctex (inputs records count pages index boxes scale x-offset y-offset))
                    (:copier nil))
  "A SyncTeX file read (READ-SYNCTEX). INPUTS are the files it names, a hash
table of each file's number to its path. RECORDS hold COUNT records,
+SYNCTEX-STRIDE+ numbers each: the kind (its place in *SYNCTEX-KINDS*) times
2^24 plus the input's number, the line, x and y of the point it was put at
(y down the page), its width, height and depth, and the record of the line's
box holding it, or -1. PAGES are the pages, each (PAGE FIRST END), its
records from FIRST below END. INDEX holds the records by input and line, in
the order of the pages within each line (SYNCTEX-KEY). BOXES are the PDF's
page boxes (PDF-PAGE-BOXES) or NIL. SCALE turns its units to points, after
X-OFFSET and Y-OFFSET (in its units) are added."
  inputs records count pages index boxes scale x-offset y-offset)

(setf (documentation 'synctex-p 'function) "True when OBJECT is a SYNCTEX.")

;;; Reading

(defun synctex-number (octets i end)
  "The whole number, maybe negative, written in OCTETS from I below END, and
where it ends; NIL when there is none there or it does not fit in 32 bits."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum i end))
  (let ((sign 1) (n 0) (j i))
    (declare (type fixnum j n))
    (when (and (< j end) (= (aref octets j) 45))
      (setf sign -1)
      (incf j))
    (let ((start j))
      (loop while (and (< j end) (<= 48 (aref octets j) 57) (< (- j start) 11))
            do (setf n (+ (* n 10) (- (aref octets j) 48)))
               (incf j))
      (when (and (> j start) (<= n #x7fffffff))
        (values (* sign n) j)))))

(defun synctex-path (path)
  "PATH, a file's name as a SyncTeX file or the editor gives it, as the two
are compared: each /./ and // within it one /."
  (let ((path (cl-ppcre:regex-replace-all "/(\\./)+" path "/")))
    (cl-ppcre:regex-replace-all "//+" path "/")))

(defparameter *synctex-headers* '(("Unit:" . :unit) ("Magnification:" . :magnification)
                                   ("X Offset:" . :x-offset) ("Y Offset:" . :y-offset))
  "The lines of a SyncTeX file saying how its numbers are read, each the
words it begins with and what they set.")

(defun synctex-header (octets start end)
  "What the line of OCTETS from START below END sets, when it is one of
*SYNCTEX-HEADERS* with a number after it (Unit:1), as two values: the setting
and the number; NIL for any other line."
  (let ((header (find-if (lambda (header) (tuatara.io:octets-at-p octets (car header) start)) *synctex-headers*)))
    (when header
      (let ((number (synctex-number octets (+ start (length (car header))) end)))
        (and number (values (cdr header) number))))))

(defun synctex-record (octets start end)
  "The numbers of the record of OCTETS from START below END, after its kind's
character, written tag,line[,column]:x,y[:width[,height,depth]], as seven
values: the input's number, the line, x, y, width, height and depth (those
not written 0); NIL when it is not of that shape or its input's number is
out of range."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (flet ((at-p (i code) (and i (< i end) (= (aref octets i) code))))
    (multiple-value-bind (tag i) (synctex-number octets start end)
      (when (and tag (< 0 tag (ash 1 24)) (at-p i 44))
        (multiple-value-bind (line i) (synctex-number octets (1+ i) end)
          ;; a column after the line is passed over
          (when (and line (at-p i 44))
            (setf i (nth-value 1 (synctex-number octets (1+ i) end))))
          (when (and line (at-p i 58))
            (multiple-value-bind (x i) (synctex-number octets (1+ i) end)
              (when (and x (at-p i 44))
                (multiple-value-bind (y i) (synctex-number octets (1+ i) end)
                  (when y
                    (let ((w 0) (h 0) (d 0))
                      (when (at-p i 58)
                        (multiple-value-bind (v j) (synctex-number octets (1+ i) end)
                          (when v
                            (setf w v)
                            (when (at-p j 44)
                              (multiple-value-bind (v k) (synctex-number octets (1+ j) end)
                                (when (at-p k 44)
                                  (let ((v2 (synctex-number octets (1+ k) end)))
                                    (when v2 (setf h v d v2)))))))))
                      (values tag line x y w h d))))))))))))

(defun synctex-key (records record)
  "The number RECORD of RECORDS is sorted by in a SYNCTEX's index: its input,
then its line."
  (declare (type (simple-array (signed-byte 32) (*)) records) (type fixnum record))
  (let ((at (* record +synctex-stride+)))
    (+ (ash (logand (aref records at) #xffffff) 32) (aref records (+ at 1)) #x80000000)))

(defun sorted-synctex-records (records count)
  "The first COUNT records of RECORDS by input and line, those of one line in
the order they were written, as a vector of record numbers."
  (let ((index (make-array count :element-type '(unsigned-byte 32))))
    (dotimes (i count) (setf (aref index i) i))
    (stable-sort index #'< :key (lambda (record) (synctex-key records record)))))

(defun read-synctex (octets &key boxes)
  "The SyncTeX file whose octets are OCTETS, read as a SYNCTEX, with BOXES,
the pages' boxes of the PDF it describes (PDF-PAGE-BOXES), kept for its
lookups. Nothing in it is run: each line is read for what it says, numbers
past 32 bits and lines of any other shape passed over, at most
*SYNCTEX-MOST-RECORDS* records, *SYNCTEX-MOST-PAGES* pages,
*SYNCTEX-MOST-INPUTS* files and boxes
followed *SYNCTEX-MOST-DEPTH* deep, each line read in a time of its own
length. NIL when it says no SyncTeX version. In a job, it ends when the job
is asked to stop."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((n (length octets)))
    (unless (and (> n 16) (tuatara.io:octets-at-p octets "SyncTeX Version:" 0))
      (return-from read-synctex nil))
    (let* ((most (min *synctex-most-records* (1+ (count 10 octets))))
           (records (make-array (* +synctex-stride+ most) :element-type '(signed-byte 32)))
           (inputs (make-hash-table))
           (count 0)
           (pages (make-array 0 :adjustable t :fill-pointer 0))
           (page nil) (first 0)
           (settings (list :unit 1 :magnification 1000 :x-offset 0 :y-offset 0))
           ;; for each box open, the line's box holding what came before it; DEEP, those past the depth followed
           (open (make-array 64 :adjustable t :fill-pointer 0))
           (deep 0) (holder -1) (lines 0))
      (declare (type fixnum count lines holder deep most))
      (labels ((open-box (record)
                 (if (< (fill-pointer open) *synctex-most-depth*)
                     (progn (vector-push-extend holder open)
                            (when (>= record 0) (setf holder record)))
                     (incf deep)))
               (close-box ()
                 (cond ((plusp deep) (decf deep))
                       ((plusp (fill-pointer open)) (setf holder (vector-pop open)))))
               (record (kind start end)
                 (multiple-value-bind (tag line x y w h d) (synctex-record octets start end)
                   (when (and tag (< count most))
                     (let ((at (* count +synctex-stride+)))
                       (setf (aref records at) (logior (ash kind 24) tag) (aref records (+ at 1)) line
                             (aref records (+ at 2)) x (aref records (+ at 3)) y (aref records (+ at 4)) w
                             (aref records (+ at 5)) h (aref records (+ at 6)) d (aref records (+ at 7)) holder))
                     (incf count)
                     (when (zerop kind) (open-box (1- count))))))
               (input (start end)
                 ;; Input:tag:path
                 (multiple-value-bind (tag i) (synctex-number octets (+ start 6) end)
                   (when (and tag (< 0 tag (ash 1 24)) (< i end) (= (aref octets i) 58)
                              (< (hash-table-count inputs) *synctex-most-inputs*) (< (- end i) 4096))
                     (setf (gethash tag inputs) (synctex-path (octets-text octets (1+ i) end)))))))
        (loop with start = 0
              while (< start n)
              do (let* ((end (or (position 10 octets :start start) n))
                        (line-end (if (and (> end start) (= (aref octets (1- end)) 13)) (1- end) end)))
                   (when (zerop (logand (incf lines) #xffff)) (tuatara.runtime:job-check-stop))
                   (when (< start line-end)
                     (let* ((c (code-char (aref octets start)))
                            (kind (position c *synctex-kinds*)))
                       (cond ((and kind page) (record kind (1+ start) line-end))
                             ((and (char= c #\[) page) (open-box -1))
                             ((and (member c '(#\] #\))) page) (close-box))
                             ((char= c #\{)
                              (setf page (synctex-number octets (1+ start) line-end) first count
                                    (fill-pointer open) 0 deep 0 holder -1))
                             ((char= c #\})
                              (when (and page (plusp page) (< (fill-pointer pages) *synctex-most-pages*))
                                (vector-push-extend (list page first count) pages))
                              (setf page nil))
                             ((tuatara.io:octets-at-p octets "Input:" start) (input start line-end))
                             (t (multiple-value-bind (setting number) (synctex-header octets start line-end)
                                  (when setting (setf (getf settings setting) number)))))))
                   (setf start (1+ end))))
        (make-synctex inputs records count (coerce pages 'simple-vector) (sorted-synctex-records records count)
                      boxes
                      ;; units of TeX's points scaled by 2^16, magnified, as PDF points
                      (/ (* (max 1 (getf settings :unit)) (/ (max 1 (getf settings :magnification)) 1000)) 65781.76d0)
                      (getf settings :x-offset) (getf settings :y-offset))))))

(defun synctex-octets (synctex)
  "The memory SYNCTEX's records and their index take, in octets."
  (* 4 (+ (length (synctex-records synctex)) (length (synctex-index synctex)))))

;;; Pages and places

(defun synctex-page-box (synctex page)
  "The box of PAGE in SYNCTEX's PDF, as two values: (LEFT TOP WIDTH HEIGHT),
the part shown in points from the top left of its media box, where TeX's
coordinates begin; and how far it is turned clockwise. A page past the boxes
read takes the last one's, and without boxes a page of A4."
  (let* ((boxes (synctex-boxes synctex))
         (entry (and boxes (plusp (length boxes)) (svref boxes (min (1- (length boxes)) (max 0 (1- page)))))))
    (if entry
        (destructuring-bind ((ml mb mr mt) (l b r tp) turn) entry
          (declare (ignore mb mr))
          (values (list (- l ml) (- mt tp) (- r l) (- tp b)) turn))
        (values (list 0 0 595.276d0 841.89d0) 0))))

(defun synctex-turned (fx fy turn &optional back)
  "FX and FY, fractions of an upright page from its top left, as they are on
the page turned TURN degrees clockwise, as two values; with BACK, the other
way."
  (case (if back (mod (- turn) 360) turn)
    (90 (values (- 1 fy) fx))
    (180 (values (- 1 fx) (- 1 fy)))
    (270 (values fy (- 1 fx)))
    (t (values fx fy))))

(defun synctex-fractions (synctex page x y)
  "The point X, Y in SYNCTEX's units on PAGE as fractions of the page shown
\(turned as it is shown) from its top left, as two values."
  (multiple-value-bind (box turn) (synctex-page-box synctex page)
    (destructuring-bind (left top width height) box
      (let ((scale (synctex-scale synctex)))
        (synctex-turned (/ (- (* scale (+ x (synctex-x-offset synctex))) left) width)
                        (/ (- (* scale (+ y (synctex-y-offset synctex))) top) height)
                        turn)))))

(defun synctex-units (synctex page fx fy)
  "The point at FX, FY, fractions of PAGE as it is shown from its top left,
in SYNCTEX's units, as two values."
  (multiple-value-bind (box turn) (synctex-page-box synctex page)
    (destructuring-bind (left top width height) box
      (multiple-value-bind (ux uy) (synctex-turned fx fy turn t)
        (let ((scale (synctex-scale synctex)))
          (values (- (/ (+ left (* ux width)) scale) (synctex-x-offset synctex))
                  (- (/ (+ top (* uy height)) scale) (synctex-y-offset synctex))))))))

(defun synctex-page-of (synctex record)
  "The entry of SYNCTEX's PAGES holding RECORD, found by halves."
  (let* ((pages (synctex-pages synctex))
         (at (tuatara.runtime:bisect (middle 0 (length pages)) (<= (third (svref pages middle)) record))))
    (and (< at (length pages)) (svref pages at))))

(defun synctex-file-tags (synctex file)
  "The numbers SYNCTEX gives FILE, a path: each time the file was read in."
  (let ((path (synctex-path file)) (tags '()))
    (maphash (lambda (tag name) (when (string= name path) (push tag tags))) (synctex-inputs synctex))
    tags))

(defun synctex-indexed (synctex key)
  "Where in SYNCTEX's index the first record of KEY (SYNCTEX-KEY), or the
first after it, is, found by halves."
  (let ((index (synctex-index synctex)) (records (synctex-records synctex)))
    (tuatara.runtime:bisect (middle 0 (length index)) (< (synctex-key records (aref index middle)) key))))

(defun synctex-line-record (synctex tags line)
  "The record SYNCTEX-PLACE shows for LINE of the inputs TAGS: the first of
the line, a kern, glue or other mark rather than a line's box on the same
page; failing that the first of the nearest line after it, else before it.
Found in SYNCTEX's index by halves."
  (let ((index (synctex-index synctex)) (records (synctex-records synctex))
        (first nil) (leaf nil) (after nil) (after-line 0) (before-line 0) (before-tag nil))
    (flet ((tag-of (record) (logand (aref records (* record +synctex-stride+)) #xffffff))
           (line-of (record) (aref records (1+ (* record +synctex-stride+))))
           (leaf-p (record) (plusp (ash (aref records (* record +synctex-stride+)) -24)))
           (key (tag line) (+ (ash tag 32) line #x80000000)))
      (dolist (tag tags)
        (let ((at (synctex-indexed synctex (key tag line))))
          (loop for i from at below (length index)
                for record = (aref index i)
                while (= (synctex-key records record) (key tag line))
                do (when (or (null first) (< record first)) (setf first record))
                   (when (and (leaf-p record) (or (null leaf) (< record leaf))) (setf leaf record)))
          (let ((next (synctex-indexed synctex (key tag (1+ line)))))
            (when (< next (length index))
              (let ((record (aref index next)))
                (when (and (= (tag-of record) tag)
                           (or (null after) (< (line-of record) after-line)
                               (and (= (line-of record) after-line) (< record after))))
                  (setf after record after-line (line-of record))))))
          (when (plusp at)
            (let ((record (aref index (1- at))))
              (when (and (= (tag-of record) tag) (< 0 (line-of record) line) (> (line-of record) before-line))
                (setf before-line (line-of record) before-tag tag))))))
      (or (and leaf (eq (synctex-page-of synctex leaf) (synctex-page-of synctex first)) leaf)
          first after
          (and before-tag (aref index (synctex-indexed synctex (key before-tag before-line))))))))

(defun synctex-place (synctex file line)
  "Where LINE (from 1) of FILE is on the pages SYNCTEX describes, as five
values: the page, and the left, top, right and bottom of the line's box as
fractions of the page as shown, from its top left; the left the point where
the line's own text begins. The first page the line is on, or failing that
the nearest line after it that is on a page, else the nearest before. NIL
when FILE is not among SYNCTEX's files or none of its lines is on a page."
  (let* ((tags (synctex-file-tags synctex file))
         (records (synctex-records synctex))
         (record (and tags (synctex-line-record synctex tags line)))
         (page (and record (synctex-page-of synctex record))))
    (declare (type (simple-array (signed-byte 32) (*)) records))
    (when page
      (let* ((at (* record +synctex-stride+))
             (holder (aref records (+ at 7)))
             (box (if (and (plusp (ash (aref records at) -24)) (>= holder 0)) (* holder +synctex-stride+) at))
             (x (aref records (+ at 2)))
             (bx (aref records (+ box 2))) (by (aref records (+ box 3)))
             (height (max (aref records (+ box 5)) 1)) (depth (aref records (+ box 6)))
             (width (aref records (+ box 4))))
        (multiple-value-bind (left top) (synctex-fractions synctex (first page) (max bx x) (- by height))
          (multiple-value-bind (right bottom) (synctex-fractions synctex (first page) (max (+ bx width) (1+ x))
                                                                 (+ by depth))
            (values (first page) (min left right) (min top bottom) (max left right) (max top bottom))))))))

(defun synctex-line (synctex page fx fy)
  "The file and line (from 1) that made the point at FX, FY of PAGE, fractions
of the page as shown from its top left, by SYNCTEX, as two values: in the
smallest line's box holding the point, or failing that the line's box
nearest to it, the last kern, glue or other mark at or before the point (the
box's own line when it has none); on a page with no line's box, the record
nearest to the point. NIL when SYNCTEX says nothing of PAGE."
  (let ((entry (find page (synctex-pages synctex) :key #'first))
        (records (synctex-records synctex)))
    (declare (type (simple-array (signed-byte 32) (*)) records))
    (when entry
      (multiple-value-bind (px py) (synctex-units synctex page fx fy)
        (destructuring-bind (first end) (rest entry)
          (let ((box nil) (box-area 0) (near-box nil) (near-box-distance 0) (near nil) (near-distance 0))
            (loop for record from first below end
                  for at = (* record +synctex-stride+)
                  for line-box = (zerop (ash (aref records at) -24))
                  for x = (aref records (+ at 2)) and y = (aref records (+ at 3))
                  for w = (aref records (+ at 4)) and h = (aref records (+ at 5)) and d = (aref records (+ at 6))
                  for distance = (+ (abs (- py (max (- y h) (min py (+ y d)))))
                                    (/ (abs (- px (max x (min px (+ x w))))) 4))
                  do (when (and line-box (zerop distance) (or (null box) (< (* w (+ h d)) box-area)))
                       (setf box record box-area (* w (+ h d))))
                     (when (and line-box (or (null near-box) (< distance near-box-distance)))
                       (setf near-box record near-box-distance distance))
                     (when (and (plusp (aref records (+ at 1))) (or (null near) (< distance near-distance)))
                       (setf near record near-distance distance)))
            (let* ((box (or box near-box))
                   (record (if box
                               ;; within the box, the last mark at or before the point, else its first
                               (let ((best nil) (best-x 0) (first-mark nil))
                                 (loop for r from (1+ box) below end
                                       for at = (* r +synctex-stride+)
                                       for x = (aref records (+ at 2))
                                       when (and (= (aref records (+ at 7)) box) (plusp (aref records (+ at 1))))
                                         do (unless first-mark (setf first-mark r))
                                            (when (and (<= x px) (or (null best) (>= x best-x)))
                                              (setf best r best-x x)))
                                 (or best first-mark box))
                               near)))
              (when record
                (let* ((at (* record +synctex-stride+))
                       (file (gethash (logand (aref records at) #xffffff) (synctex-inputs synctex))))
                  (and file (values file (aref records (+ at 1)))))))))))))
