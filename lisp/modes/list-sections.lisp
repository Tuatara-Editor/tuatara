;;;; lisp/modes/list-sections.lisp --- Lists in sections: grouping, headings, folding.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; Sections

(define-major-mode sectioned-list-mode marked-list-mode
  "A list whose entries are in sections, each under a heading saying how many it
holds, as the buffer list (\\[buffer-manager]) and the bookmark manager
\(\\[bookmark-manager]) are: ▾ before an open section's heading, ▸ before a
folded one, its entries hidden. \\<sectioned-list-mode>\\[list-toggle-fold]
folds or unfolds the section of the current line, \\[list-toggle-all-folds] all
of them; the entries are marked as in any list of marked entries."
  :lighter "List" :interactive nil)

(defun list-folded (list)
  "The sections folded in LIST, a list in sections (SECTIONED-PIECES)."
  (tuatara.buffer:buffer-local list 'list-folded '()))

(defun group-sections (items key &key first last order)
  "ITEMS in sections, each (SECTION . ITEMS): SECTION what KEY, a function of
an item, gives of it, items with EQUAL keys in one section, in their order in
ITEMS. The sections come in the order of their first items, or as ORDER, a
function of two sections, sorts them, when given; the section FIRST, when
there is one, before them all, and the section LAST after them. O(items)."
  (let ((table (make-hash-table :test 'equal)) (sections '()))
    (dolist (item items)
      (let* ((section (funcall key item))
             (place (gethash section table)))
        (if place
            (push item (cdr place))
            (push (setf (gethash section table) (list section item)) sections))))
    (let* ((sections (mapcar (lambda (place) (cons (car place) (reverse (cdr place)))) (nreverse sections)))
           (others (remove-if (lambda (place)
                                (or (and first (equal (car place) first)) (and last (equal (car place) last))))
                              sections)))
      (flet ((alone (section)
               (let ((place (and section (assoc section sections :test #'equal)))) (and place (list place)))))
        (append (alone first)
                (if order (stable-sort others order :key #'car) others)
                (alone last))))))

(defun inner-sections (members)
  "The sections MEMBERS, what a section of a list holds (SECTIONED-PIECES),
are when it holds sections, written (:SECTIONS . SECTIONS); NIL when they are
members."
  (and (consp members) (eq (first members) :sections) (rest members)))

(defun map-section-members (function sections)
  "Call FUNCTION with each member of SECTIONS, each (SECTION . MEMBERS) as
SECTIONED-PIECES takes them, and the section holding it, those of sections
within sections too, in their order."
  (loop for (section . members) in sections
        for inner = (inner-sections members)
        do (if inner
               (map-section-members function inner)
               (dolist (member members) (funcall function member section)))))

(defun section-leaves (members)
  "The members a section holding MEMBERS has, as SECTIONED-PIECES takes them:
MEMBERS themselves, or those of every section it holds, in order."
  (let ((inner (inner-sections members)))
    (if inner
        (let ((leaves '()))
          (map-section-members (lambda (member section) (declare (ignore section)) (push member leaves)) inner)
          (nreverse leaves))
        members)))

(defun section-members (sections section)
  "The members of SECTION among SECTIONS, each (SECTION . MEMBERS) as
SECTIONED-PIECES takes them, those of the sections it holds too; NIL when it
is not there."
  (loop for (key . members) in sections
        do (when (equal key section) (return (section-leaves members)))
           (let ((found (section-members (inner-sections members) section)))
             (when found (return found)))))

(defun section-entry (section parent)
  "The entry of the heading of SECTION in a list (SECTIONED-PIECES), PARENT
the section holding it, NIL for one at the top."
  (list* :section section (and parent (list parent))))

(defun section-parent (list section)
  "The section of LIST that holds SECTION, or NIL for one at the top."
  (cdr (assoc section (tuatara.buffer:buffer-local list 'list-section-parents) :test #'equal)))

(defun row-indent (list position)
  "How many columns the line of LIST holding POSITION is set in for the
sections around its section (SECTIONED-PIECES): two for each, 0 for a line of
a section at the top or of no member."
  (let ((entry (and (tuatara.buffer:buffer-local list 'list-section-parents) (list-entry-at list position))))
    (if (and (consp entry) (eq (first entry) :member))
        (loop for section = (section-parent list (third entry)) then (section-parent list section)
              while section
              sum 2)
        0)))

(defun map-set-in-pieces (function pieces spaces)
  "Call FUNCTION with each of PIECES, as PIECES-TEXT takes them, in order, and
with SPACES, a string, before each line they make, but an empty line; a line
that begins inside a piece is not set in. With SPACES NIL, with PIECES alone."
  (let ((line-start t))
    (dolist (piece pieces)
      (let ((text (piece-parts piece)))
        (when (and spaces line-start (plusp (length text)) (char/= (char text 0) #\Newline))
          (funcall function spaces))
        (funcall function piece)
        (setf line-start (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline)))))))

(defun set-in-pieces (pieces columns)
  "PIECES, as PIECES-TEXT takes them, each line they make set COLUMNS spaces
in, as MAP-SET-IN-PIECES sets them."
  (if (zerop columns)
      pieces
      (let ((out '()))
        (map-set-in-pieces (lambda (piece) (push piece out)) pieces (spaces columns))
        (nreverse out))))

(defun sectioned-pieces (list sections title row &key count after section-face section-note (margin 0))
  "The lines of LIST for SECTIONS, each (SECTION . MEMBERS), as two values:
their entries, for RENDER-PIECES, and their pieces. A section may hold sections
in place of members, (SECTION :SECTIONS . SECTIONS), each the same way; every
section, held or not, has a key of its own (EQUAL). Each section has a heading:
▾, or ▸ when it is folded, its title (TITLE, a function of the section) and how
many it holds in brackets, all in :heading, its entry (:SECTION SECTION), or
\(:SECTION SECTION PARENT) for one held in PARENT, its heading set in two spaces
for each section around it. A member's lines are set in MARGIN columns (0 unless
given) and two more for each section around its section, as far as its heading
is (MAP-SET-IN-PIECES): with MARGIN 2, as the tables of RENDER-TABLE-LIST have
it, a line starts under the text of its heading at any depth. How many is the
number of its members, those of the sections it holds for one that holds
sections, or what COUNT, a function of the section and those members, gives, no
brackets when that is NIL. Unless the section is folded, each member has a line
under it, and each section it holds its heading and lines; a section folded
hides those it holds, each of which keeps its own fold (all shown while search
filters narrow LIST, which then hide them). ROW (a function of the member and
the section) gives its pieces, its entry (:MEMBER MEMBER SECTION). For a member
shown on several lines, or on lines with entries of their own, ROW gives as a
second value their entries, a sequence of one for each line. AFTER, a function
of the section and its members, gives lines following the members of an open
section the same way, their pieces and their entries (\"... 3 more\"). An empty
line follows each open section another comes after. SECTION-FACE, a function of
a section, gives the face of its heading, :heading when it gives none.
SECTION-NOTE, a function of a section, gives words shown after its heading in
:warning, or NIL. The sections are kept for \\[list-toggle-all-folds], and
while some are folded, every entry in order, those they hide too, for
LIST-EVERY-ENTRY."
  (let* ((folded (list-folded list)) (filtered (list-filters-on-p list))
         (keys '()) (parents '()) (entries '()) (pieces '())
         (all (and folded (not filtered) (list '()))))
    (labels ((enter (entry)
               (push entry entries)
               (when all (push entry (car all))))
             (add (spaces &optional more-pieces more-entries)
               (flet ((put-piece (piece) (push piece pieces)))
                 (declare (dynamic-extent #'put-piece))
                 (map-set-in-pieces #'put-piece more-pieces spaces))
               (map nil #'enter more-entries))
             (put (sections parent indent)
               (loop for (section . members) in sections
                     for inner = (inner-sections members)
                     for open-before = nil then open
                     for open = (not (member section folded :test #'equal))
                     for number = (if count
                                      (funcall count section (section-leaves members))
                                      (length (section-leaves members)))
                     do (push section keys)
                        (when parent (push (cons section parent) parents))
                        (when open-before
                          (enter nil)
                          (push (string #\Newline) pieces))
                        (enter (section-entry section parent))
                        (push (cons (format nil "~a~:[▸~;▾~] ~a~@[ (~:d)~]"
                                            indent open (escape-controls (funcall title section)) number)
                                    (or (and section-face (funcall section-face section)) :heading))
                              pieces)
                        (let ((note (and section-note (funcall section-note section))))
                          (when note (push (cons (format nil " ~a" (escape-controls note)) :warning) pieces)))
                        (push (string #\Newline) pieces)
                        (if (or open filtered)
                            (if inner
                                (put inner section (concatenate 'string indent "  "))
                                (let ((spaces (let ((columns (+ margin (length indent))))
                                                (and (plusp columns) (spaces columns)))))
                                  (dolist (member members)
                                    (multiple-value-bind (row-pieces row-entries) (funcall row member section)
                                      (add spaces row-pieces (or row-entries (list (list :member member section))))))
                                  (when after
                                    (multiple-value-call #'add spaces (funcall after section members)))))
                            (when all
                              (map-section-members (lambda (member section)
                                                     (push (list :member member section) (car all)))
                                                   (list (cons section members))))))))
      (put sections nil ""))
    (setf (tuatara.buffer:buffer-local list 'list-sections) (nreverse keys)
          (tuatara.buffer:buffer-local list 'list-section-parents) parents
          (tuatara.buffer:buffer-local list 'list-every-entry) (and all (nreverse (car all))))
    (values (nreverse entries) (nreverse pieces))))

(defun entry-lines (function)
  "Call FUNCTION with a function of a line's TEXT and, as it chooses, its
FACE and its ENTRY, which adds that line; TEXT may also be a list of pieces,
each a string or (STRING . FACE), the line made of them in their faces. The
lines added, as pieces for RENDER-PIECES, and their entries, a vector, as two
values."
  (let ((pieces '()) (entries '()))
    (funcall function (lambda (text &optional face entry)
                        (if (listp text)
                            (progn (dolist (piece text) (push piece pieces))
                                   (push (string #\Newline) pieces))
                            (let ((text (format nil "~a~%" text)))
                              (push (if face (cons text face) text) pieces)))
                        (push entry entries)))
    (values (nreverse pieces) (coerce (nreverse entries) 'vector))))

(defmacro collecting-pieces ((put) &body body)
  "Run BODY with PUT, a local function of a TEXT and, as it chooses, its FACE
and its ACTION (a function of no arguments that makes TEXT a button), which
adds that piece; TEXT may also be a piece already made, added as it is. The
pieces added, in order, as RENDER-PIECES and PIECES-TEXT take them."
  (let ((pieces (gensym "PIECES")))
    `(let ((,pieces '()))
       (flet ((,put (text &optional face action)
                (push (cond ((consp text) text)
                            (action (list text face action))
                            (face (cons text face))
                            (t text))
                      ,pieces)))
         (declare (ignorable #',put))
         ,@body)
       (nreverse ,pieces))))

(defun entry-member (entry)
  "The member ENTRY, a line of a list in sections (SECTIONED-PIECES), shows;
NIL for a heading's or another line's."
  (and (consp entry) (eq (first entry) :member) (second entry)))

(defun entry-member-or-self (entry)
  "The member ENTRY, a line of a list, shows when it is a section's member line
\(ENTRY-MEMBER); else ENTRY itself, as a list without sections has it."
  (if (and (consp entry) (eq (first entry) :member)) (second entry) entry))

(defun list-member-at (list)
  "The member of a section on the current line of LIST (SECTIONED-PIECES), or
NIL on a heading or another line."
  (entry-member (list-entry-at list)))

(defun list-section-at (list)
  "The section the current line of LIST (SECTIONED-PIECES) is the heading of
or one of the lines under; NIL on another line."
  (let ((entry (list-entry-at list)))
    (case (and (consp entry) (first entry))
      (:section (second entry))
      (:member (third entry))
      ((nil) nil)
      (t ;; a line with an entry of its own kind belongs to the heading above it
       (let ((entries (tuatara.buffer:buffer-local list 'list-entries)))
         (loop for index downfrom (- (tuatara.buffer:buffer-line-number list (tuatara.buffer:buffer-point list))
                                     (tuatara.buffer:buffer-local list 'list-first-line 0))
                 to 0
               for entry = (svref entries index)
               do (cond ((null entry) (return nil))
                        ((and (consp entry) (eq (first entry) :section)) (return (second entry)))
                        ((and (consp entry) (eq (first entry) :member)) (return (third entry))))))))))

(defun list-goto-entry (list entry)
  "Put point of LIST at the start of the line showing ENTRY (EQUAL to one
RENDER-PIECES was given), when one shows it."
  (let ((index (position entry (tuatara.buffer:buffer-local list 'list-entries) :test #'equal)))
    (when index
      (setf (tuatara.buffer:buffer-point list)
            (tuatara.buffer:buffer-goto-line list (+ index (tuatara.buffer:buffer-local list 'list-first-line 0)))))))

(defun fold-list-sections (list folded)
  "Fold the sections FOLDED of LIST, the others open, and make it again; when
the section point was in is folded, or one holding it, point goes to the
heading of the outermost of them."
  (let ((around (loop for section = (list-section-at list) then (section-parent list section)
                      while section
                      collect (cons section (section-parent list section)))))
    (setf (tuatara.buffer:buffer-local list 'list-folded) folded)
    (redraw-list list)
    (let ((closed (find-if (lambda (place) (member (car place) folded :test #'equal)) around :from-end t)))
      (when closed
        (list-goto-entry list (section-entry (car closed) (cdr closed)))))))

(defun list-toggle-section (list section)
  "Fold SECTION of LIST, or unfold it when it is folded."
  (let ((folded (list-folded list)))
    (fold-list-sections list (if (member section folded :test #'equal)
                                 (remove section folded :test #'equal)
                                 (cons section folded)))))

(defun unfold-list-sections (list sections)
  "Unfold those of SECTIONS of LIST that are folded, so that what a command
acts on shows."
  (let ((folded (list-folded list)))
    (when (intersection sections folded :test #'equal)
      (fold-list-sections list (remove-if (lambda (section) (member section sections :test #'equal)) folded)))))

(define-command list-toggle-fold ()
  "Fold the section of the list the current line is in, its heading left
showing, or unfold it when it is folded."
  (let ((section (list-section-at (current-buffer))))
    (when section (list-toggle-section (current-buffer) section))))

(define-command list-fold-heading (:interactive nil)
  "On a heading of the list, fold its section when it is open, as
\\[list-visit] opens it again; on any other line nothing happens. Bound to
\\`h' and \\`C-b', which move back, in a list in sections."
  (let* ((list (current-buffer))
         (section (list-section-at list)))
    (when (and section (eq (first (list-entry-at list)) :section)
               (not (member section (list-folded list) :test #'equal)))
      (list-toggle-section list section))))

(define-command list-toggle-all-folds ()
  "Fold every section of the list, or unfold them all when every one is
folded; a section not in the list now keeps its fold for when it comes back."
  (let* ((list (current-buffer))
         (sections (tuatara.buffer:buffer-local list 'list-sections))
         (folded (list-folded list)))
    (fold-list-sections list (if (subsetp sections folded :test #'equal)
                                 (set-difference folded sections :test #'equal)
                                 (union folded sections :test #'equal)))))
