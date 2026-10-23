;;;; lisp/org/fold.lisp --- Org's subtrees folded and shown: TAB and S-TAB, as Org cycles them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; TAB on a headline cycles its subtree as Org's org-cycle does: folded (the
;;;; headline alone), its children (their headlines, their bodies folded), all
;;;; of it, and folded again. S-TAB cycles the whole buffer as
;;;; org-global-cycle does: the overview (the first level's headlines), the
;;;; contents (every headline), everything. Folding hides text from the
;;;; windows (src/display/folds.lisp) and changes none; point is kept out of
;;;; what is hidden. #+STARTUP: overview, content and showall set how a file
;;;; opens.

(in-package #:tuatara.defaults)

;;; Point kept out of folds

(defvar *point-before-command* nil
  "The buffer of the selected window and its point before the command running, (BUFFER . POINT).")

(defvar *version-before-command* nil
  "The version of the selected window's buffer before the command running, to
tell whether the command changed it.")

(defun note-point-before-command (editor command)
  "Pre-command function: remember the selected buffer's point and version, for
POINT-OUT-OF-FOLDS and OPEN-STALE-ORG-FOLDS."
  (declare (ignore command))
  (let ((buffer (tuatara.display:window-buffer (editor-window editor))))
    (setf *point-before-command* (cons buffer (tuatara.buffer:buffer-point buffer))
          *version-before-command* (tuatara.buffer:buffer-version buffer))))

(defun keep-point-out-of-folds (editor)
  "Post-command function: in an Org buffer the command changed, a fold left
after a line that no longer folds is opened (OPEN-STALE-ORG-FOLDS); then a
point the command moved into a fold, or into the lines search filters hide,
goes to its end when it came from before the fold, else to its start."
  (let* ((buffer (tuatara.display:window-buffer (editor-window editor)))
         (before (and (eq (car *point-before-command*) buffer) (cdr *point-before-command*))))
    (when (tuatara.display:buffer-hides-p buffer)
      (when (and before (not (eq *version-before-command* (tuatara.buffer:buffer-version buffer)))
                 (typep (buffer-major-mode buffer) 'org-mode))
        (open-stale-org-folds buffer before))
      (tuatara.display:point-out-of-folds buffer before))))

;;; Subtrees

(defun org-tree-now (buffer)
  "BUFFER's parse as its text is now, as two values, the tree and its text: a
small buffer parsed at once (*ORG-PARSE-AT-ONCE*); a large one whose parse
is behind its text waited for while it is parsed in the background, the
editor reading keys meanwhile (\\`C-g' gives the command up)."
  (let ((state (org-buffer-state buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (if (or (<= (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
            (org-parse-current-p buffer)
            (null *editor*))
        (let ((*org-parse-at-once* most-positive-fixnum))
          (org-buffer-tree buffer))
        (progn
          ;; asked for: the worker parses it, the parse before taken meanwhile
          (org-buffer-tree buffer)
          (flet ((ready-p ()
                   (let ((result (org-state-result state)))
                     (and result (>= (org-version-number (third result)) (org-version-number version))))))
            (unless (or (ready-p) (wait-until-interruptibly #'ready-p :message "Reading the file..."))
              (keyboard-quit-signal)))
          (org-buffer-tree buffer)))))

(defun org-headlines (tree)
  "Every headline of TREE, in order."
  (let ((found '()))
    (labels ((walk (node)
               (when (eq (org-node-type node) 'headline) (push node found))
               (when (member (org-node-type node) '(org-data headline))
                 (mapc #'walk (org-node-children node)))))
      (walk tree))
    (nreverse found)))

(defun org-headline-titled (tree title)
  "The first headline of TREE whose title, as written, is TITLE; NIL for none."
  (find title (org-headlines tree) :key (lambda (h) (org-property h :raw-value)) :test #'string=))

(defun org-headline-on-line (tree text position)
  "The headline whose line holds the character POSITION of TEXT, or NIL."
  (let ((line (string-line-start text position)))
    (org-node-at tree line (lambda (node) (and (eq (org-node-type node) 'headline) (= (org-node-begin node) line))))))

(defun org-child-headlines (headline)
  "The headlines right under HEADLINE."
  (remove 'headline (org-node-children headline) :key #'org-node-type :test-not #'eq))

(defun org-last-content-end (text from to)
  "Where the text from FROM below TO ends, the blank lines after it left out:
the newline ending its last line that is not blank (or TO)."
  (let ((last (loop for i downfrom (1- to) to from
                    unless (member (char text i) '(#\Space #\Tab #\Newline #\Return)) return i)))
    (if last (string-line-end text last) from)))

(defun org-fold-chars (buffer from to)
  "Fold BUFFER's characters FROM below TO, its text's as parsed."
  (when (< from to)
    (tuatara.display:fold-region buffer (org-char->byte buffer from) (org-char->byte buffer to))))

(defun org-fold-range (node text &optional (limit (org-node-end node)))
  "The characters of TEXT folding NODE, a headline or a list item, hides,
\(START . END): from the end of its first line to the end of its last line
that is not blank before LIMIT (its end when not given); NIL when it has
nothing under its first line."
  (let* ((start (string-line-end text (org-node-begin node)))
         (end (org-last-content-end text start limit)))
    (and (< start end) (cons start end))))

(defun org-section-range (headline text)
  "The characters of TEXT holding HEADLINE's own text, before its first child
headline, (START . END) as ORG-FOLD-RANGE has them; NIL when there is none."
  (let ((child (first (org-child-headlines headline))))
    (org-fold-range headline text (if child (org-node-begin child) (org-node-end headline)))))

(defun org-folded-p (buffer range)
  "True when BUFFER's folds hide the whole of RANGE, characters (START . END) of its parse."
  (let* ((start (org-char->byte buffer (car range)))
         (end (org-char->byte buffer (cdr range))))
    (find-if (lambda (fold) (and (<= (car fold) start) (>= (cdr fold) end))) (tuatara.display:buffer-folds buffer))))

(defvar *org-cycle-hide-drawers* t
  "True for drawers (:LOGBOOK:, :PROPERTIES:) to stay folded when \\`TAB' shows
a headline's or an item's children or all of it; \\`TAB' on a drawer's line
opens it. NIL to show them
with the rest.")

(defparameter *org-drawer-line-re* (concatenate 'string "^" *org-drawer-re*)
  "A line beginning an Org drawer, :LOGBOOK: or :PROPERTIES:, as the parser reads one (*ORG-DRAWER-RE*).")

(defun org-drawer-range (buffer line)
  "The octets of BUFFER folding the drawer whose first line begins at LINE
hides, (START . END): from the end of that line to the end of its :END: line;
NIL when LINE begins no drawer that ends before the next headline. As a
second value, where the search for its end stopped: the start of the :END:
line, of the next headline, or the text's end."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (first-end (tuatara.buffer:buffer-line-end buffer line))
         (head (org-line-head buffer line)))
    (when (and (cl-ppcre:scan *org-drawer-line-re* head)
               (not (cl-ppcre:scan "(?i)^[ \\t]*:END:" head)))
      (loop for next = (1+ (tuatara.text:rope-line-end rope line)) then (1+ (tuatara.text:rope-line-end rope next))
            while (< next size)
            do (cond ((org-stars rope next) (return (values nil next)))
                     ((cl-ppcre:scan "(?i)^[ \\t]*:END:[ \\t]*$" (org-line-head buffer next))
                      (return (values (cons first-end (tuatara.buffer:buffer-line-end buffer next)) next))))
            finally (return (values nil size))))))

(defun org-fold-drawers-in (buffer start end)
  "Fold each drawer of BUFFER whose first line begins from octet START below
END, each line looked at once: what follows a drawer that is never ended
holds no drawer's end either, up to the next headline."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (line (tuatara.buffer:buffer-line-start buffer start)))
    (loop while (and (< line end) (< line size))
          do (let ((next (1+ (tuatara.text:rope-line-end rope line)))
                   (first (loop for i from line below (min end size)
                                for octet = (tuatara.text:rope-octet rope i)
                                while (member octet '(32 9))
                                finally (return (and (< i size) (tuatara.text:rope-octet rope i))))))
               (when (eql first 58)    ; a line whose text begins with a colon
                 (multiple-value-bind (range stop) (org-drawer-range buffer line)
                   (when range (tuatara.display:fold-region buffer (car range) (cdr range)))
                   (when stop (setf next (max next stop)))))
               (setf line next)))))

(defun org-cycle-drawer (buffer line)
  "Fold the drawer whose first line begins at LINE of BUFFER, or open it when
it is folded; true when LINE begins a drawer."
  (let ((range (org-drawer-range buffer line)))
    (when range
      (if (find-if (lambda (fold) (and (<= (car fold) (car range)) (>= (cdr fold) (cdr range))))
                   (tuatara.display:buffer-folds buffer))
          (progn (tuatara.display:unfold-region buffer (car range) (cdr range)) (message "Drawer shown"))
          (progn (tuatara.display:fold-region buffer (car range) (cdr range)) (message "Drawer folded")))
      t)))

(defun org-cycle-ranges (buffer range children)
  "Cycle a subtree of BUFFER whose fold hides RANGE and its direct children's
folds CHILDREN (a list, NIL for a child with nothing under it), all (START .
END) in octets: folded, then its own text and its children folded, then all
of it, then folded again."
  (let ((folds (coerce (tuatara.display:buffer-folds buffer) 'simple-vector)))
    (flet ((folded-p (r)
             ;; the folds are apart and in order: the last one starting at or before R holds it, or none does
             (let ((lo -1) (hi (length folds)))
               (loop while (< (1+ lo) hi)
                     do (let ((mid (ash (+ lo hi) -1)))
                          (if (<= (car (aref folds mid)) (car r)) (setf lo mid) (setf hi mid))))
               (and (>= lo 0) (>= (cdr (aref folds lo)) (cdr r)))))
           (fold (r) (tuatara.display:fold-region buffer (car r) (cdr r)))
           (unfold (r) (tuatara.display:unfold-region buffer (car r) (cdr r))))
      (cond
        ((null range) (message "Empty entry"))
        ((folded-p range)
         (unfold range)
         (when *org-cycle-hide-drawers* (org-fold-drawers-in buffer (car range) (cdr range)))
         (if children
             (progn (dolist (child children) (when child (fold child)))
                    (message "Children"))
             (message "Subtree")))
        ((and children
              (every (lambda (child) (or (null child) (folded-p child))) children)
              (some #'identity children))
         (unfold range)
         (when *org-cycle-hide-drawers* (org-fold-drawers-in buffer (car range) (cdr range)))
         (message "Subtree"))
        (t
         (unfold range)
         (fold range)
         (message "Folded"))))))

(defun org-cycle-headline (buffer headline text)
  "Cycle HEADLINE's subtree: folded, then its children, then all of it, then folded again."
  (flet ((bytes (r) (and r (cons (org-char->byte buffer (car r)) (org-char->byte buffer (cdr r))))))
    (org-cycle-ranges buffer (bytes (org-fold-range headline text))
                      (mapcar (lambda (child) (bytes (org-fold-range child text))) (org-child-headlines headline)))))

(defun org-stars (rope line)
  "The level of the headline whose line starts at the octet LINE of ROPE (how
many stars begin it, a space or the line's end after them), or NIL when the
line is no headline."
  (let ((size (tuatara.text:rope-bytes rope)))
    (loop for i from line below size
          for octet = (tuatara.text:rope-octet rope i)
          while (= octet 42)
          finally (return (and (> i line)
                               (or (>= i size) (member (tuatara.text:rope-octet rope i) tuatara.text:*blank-octets*))
                               (- i line))))))

(defun org-line-folds-p (buffer line)
  "True when the line of BUFFER starting at the octet LINE begins what \\`TAB'
folds: a headline, a list item or a drawer; only its first 512 octets read."
  (let ((text (tuatara.buffer:buffer-string
               buffer :start line :end (min (tuatara.buffer:buffer-line-end buffer line) (+ line 512)))))
    (or (org-stars (tuatara.buffer:buffer-text buffer) line)
        (cl-ppcre:scan *org-drawer-line-re* text)
        (cl-ppcre:scan (concatenate 'string "^" (org-item-re)) text))))

(defun open-stale-org-folds (buffer previous)
  "Show again what is folded after the line point is on, or the one it was on
before the command (PREVIOUS), when that line no longer begins a headline, a
list item or a drawer (ORG-LINE-FOLDS-P): a heading whose stars were deleted
leaves its text in sight, under a plain line, rather than hidden there. Only
those two lines are looked at."
  (let ((size (tuatara.buffer:buffer-size buffer)))
    (dolist (position (remove-duplicates (remove nil (list (tuatara.buffer:buffer-point buffer)
                                                           (and previous (min previous size))))))
      (let* ((line (tuatara.buffer:buffer-line-start buffer position))
             (end (tuatara.buffer:buffer-line-end buffer line))
             (fold (tuatara.display:fold-from buffer end)))
        (when (and fold (= (car fold) end) (not (org-line-folds-p buffer line)))
          (tuatara.display:unfold-region buffer (car fold) (cdr fold)))))))

(defun org-text-end-between (buffer from to)
  "Where BUFFER's text from the octet FROM below TO ends, the blank lines after
it left out: the end of its last line that is not blank (before its line
end), or FROM."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (loop with p = to
          while (> p from)
          do (let* ((line (max from (tuatara.buffer:buffer-line-start buffer (1- p))))
                    (end (min p (tuatara.buffer:buffer-line-end buffer line))))
               (when (loop for i from line below end
                           thereis (not (member (tuatara.text:rope-octet rope i) tuatara.text:*blank-octets*)))
                 (return end))
               (setf p line))
          finally (return from))))

(defun org-cycle-headline-at (buffer line)
  "Cycle the subtree of the headline whose line starts at the octet LINE of
BUFFER, as ORG-CYCLE-HEADLINE does, found by reading its lines rather than by
parsing the file: it ends at the next headline of its level or above, its
direct children are the headlines in it with no shallower one before them."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (level (org-stars rope line))
         (start (tuatara.buffer:buffer-line-end buffer line))
         (children '())
         (end size))
    (loop with shallowest = most-positive-fixnum
          for next = (1+ (tuatara.text:rope-line-end rope line)) then (1+ (tuatara.text:rope-line-end rope next))
          while (< next size)
          do (let ((stars (org-stars rope next)))
               (when stars
                 (when (<= stars level) (setf end next) (return))
                 (when (<= stars shallowest) (setf shallowest stars) (push next children)))))
    (setf children (nreverse children))
    (flet ((range (from limit) (let ((e (org-text-end-between buffer from limit))) (and (< from e) (cons from e)))))
      (org-cycle-ranges buffer (range start end)
                        ;; a child's subtree ends where the next child begins: a headline of its level or
                        ;; above before then would be a child itself
                        (loop for (child . more) on children
                              collect (range (tuatara.buffer:buffer-line-end buffer child) (or (first more) end)))))))

(defvar *org-cycle-include-plain-lists* t
  "True for \\`TAB' on a list item with sub-items or text under its first line
to fold it, as on a headline: folded, then its sub-items, then all of it;
NIL for \\`TAB' there to indent.")

(defun org-item-on-line (tree position)
  "The list item of TREE beginning at the character POSITION (a line's start), or NIL."
  (org-node-at tree position (lambda (node) (and (eq (org-node-type node) 'item) (= (org-node-begin node) position)))))

(defun org-cycle-item (buffer item text)
  "Cycle the list ITEM as ORG-CYCLE-HEADLINE cycles a headline: folded, then its
sub-items, then all of it; one with nothing under its first line is said to
be empty."
  (let ((range (org-fold-range item text)))
    (if (null range)
        (message "Empty entry")
        (flet ((bytes (r) (and r (cons (org-char->byte buffer (car r)) (org-char->byte buffer (cdr r))))))
          (org-cycle-ranges buffer (bytes range)
                            (mapcar (lambda (child) (bytes (org-fold-range child text))) (org-child-items item)))))
    t))

(defparameter *org-drawer-look-back* 256
  "The most lines \\`TAB' looks back over for the start of a drawer it may be
in (ORG-LINE-IN-DRAWER-P).")

(defun org-line-in-drawer-p (buffer line)
  "True when the line of BUFFER beginning at the octet LINE is inside a
drawer (:LOGBOOK:, :PROPERTIES: and the like): after a drawer's first line
with no :END: between, under the same headline; looked for no further back
than *ORG-DRAWER-LOOK-BACK* lines."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (loop repeat *org-drawer-look-back*
          for start = line then above
          for above = (and (plusp start) (tuatara.buffer:buffer-line-start buffer (1- start)))
          while above
          do (let ((text (tuatara.buffer:buffer-string buffer :start above
                                                              :end (min (+ above 512) (tuatara.buffer:buffer-line-end buffer above)))))
               (cond ((org-stars rope above) (return nil))
                     ((cl-ppcre:scan *org-drawer-end-re* text) (return nil))
                     ((cl-ppcre:scan *org-drawer-line-re* text) (return t)))))))

(defun org-line-kept-from-tab-p (buffer line text)
  "True when \\`TAB' leaves the line of BUFFER beginning at the octet LINE,
whose first characters are TEXT, as it is: a planning line (CLOSED:,
SCHEDULED:, DEADLINE:), a drawer's :END:, or a line inside a drawer."
  (or (org-planning-line-p text)
      (cl-ppcre:scan *org-drawer-end-re* text)
      (org-line-in-drawer-p buffer line)))

(define-command org-cycle ()
  "On a headline, fold its subtree to its headline alone, then show its
children's headlines, then all of it, and fold it again, a step each time.
On a list item with something under it, the same for
the item (*org-cycle-include-plain-lists*). On a drawer's first line
\(:LOGBOOK:), fold or open the drawer; on a planning line (CLOSED:,
SCHEDULED:, DEADLINE:), a drawer's :END: or a line inside a drawer, nothing,
so that what Org keeps there stays as written. In a table, align it and go
to the next field. After a snippet's key (<s for a source block), the snippet
\(\\[insert-snippet]). Elsewhere, what \\`TAB' does in text."
  (let* ((buffer (current-buffer))
         (start (tuatara.buffer:buffer-line-start buffer (point)))
         ;; the line's beginning tells what it is: a line of a million characters is not read whole
         (line (org-line-head buffer start))
         (bare (string-left-trim " 	" line)))
    ;; a line that is no headline, table, template or list item to fold: indented, the file not parsed for it
    ;; a drawer's first line: the drawer folded or opened, found by its lines
    (when (and (uiop:string-prefix-p ":" bare) (org-cycle-drawer buffer start))
      (return-from org-cycle))
    ;; planning lines and a drawer's lines: left as Org writes them
    (when (org-line-kept-from-tab-p buffer start line)
      (return-from org-cycle))
    (unless (or (uiop:string-prefix-p "*" line) (uiop:string-prefix-p "|" bare) (uiop:string-prefix-p "<" bare)
                (and *org-cycle-include-plain-lists* (cl-ppcre:scan (concatenate 'string "^" (org-item-re)) line)))
      (return-from org-cycle (indent-for-tab-command)))
    ;; a list item of a large file whose parse is not ready: said, the parse under way
    (when (and (not (uiop:string-prefix-p "*" line)) (not (uiop:string-prefix-p "|" bare)) (not (uiop:string-prefix-p "<" bare))
               (> (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
               (progn (org-buffer-tree buffer)
                      (not (org-parse-current-p buffer))))
      (return-from org-cycle (message "The list folds once the file is read, in a moment")))
    ;; a headline of a large file whose parse is not ready: its subtree found by its lines, nothing parsed
    (when (and (org-stars (tuatara.buffer:buffer-text buffer) start)
               (> (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
               (not (org-parse-current-p buffer)))
      (return-from org-cycle (org-cycle-headline-at buffer start)))
    (multiple-value-bind (tree text) (org-tree-now buffer)
      (let* ((headline (and tree (org-headline-on-line tree text (org-byte->char buffer (point))))))
        (cond ((org-table-next-field))
              ((expand-snippet-at-point))
              (headline (org-cycle-headline buffer headline text))
              ((and *org-cycle-include-plain-lists* tree
                    (let ((item (org-item-on-line tree (string-line-start text (org-byte->char buffer (point))))))
                      (and item (org-cycle-item buffer item text)))))
              (t (indent-for-tab-command)))))))

(defun org-overview (buffer tree text)
  "Show only BUFFER's headlines of the first level, all else folded."
  (tuatara.display:unfold-all buffer)
  (dolist (headline (org-node-children tree))
    (when (eq (org-node-type headline) 'headline)
      (let ((range (org-fold-range headline text)))
        (when range (org-fold-chars buffer (car range) (cdr range)))))))

(defun org-contents (buffer tree text)
  "Show all of BUFFER's headlines, their own text folded."
  (tuatara.display:unfold-all buffer)
  (dolist (headline (org-headlines tree))
    (let ((section (org-section-range headline text)))
      (when section (org-fold-chars buffer (car section) (cdr section))))))

(define-command org-global-cycle ()
  "Cycle the whole buffer: its overview (the headlines of the first level),
then its contents (every headline), then everything, and the overview again.
In a table, go back a field, aligned."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (tree text) (org-tree-now buffer)
      (when (and tree (not (org-table-previous-field)))
        (case (tuatara.buffer:buffer-local buffer 'org-global-view)
          (:overview (org-contents buffer tree text)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :contents)
                     (message "Contents"))
          (:contents (tuatara.display:unfold-all buffer)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :all)
                     (message "Show all"))
          (t (org-overview buffer tree text)
             (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :overview)
             (message "Overview")))
        (tuatara.display:point-out-of-folds buffer nil)))))

(defun org-startup-visibility (buffer options)
  "Fold BUFFER as its #+STARTUP: words OPTIONS ask (ORG-STARTUP-OPTIONS), the
last of them saying: overview (or fold), content, or showall (nofold); the
file parsed only when one of the first two does."
  (let ((choice (org-startup-choice options '("overview" "fold" "content" "showall" "nofold"))))
    (when (member choice '("overview" "fold" "content") :test #'string=)
      (multiple-value-bind (tree text) (org-tree-now buffer)
        (when tree
          (if (string= choice "content")
              (progn (org-contents buffer tree text)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :contents))
              (progn (org-overview buffer tree text)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :overview))))))))

(defmethod mode-enter ((mode org-mode) buffer)
  "Show BUFFER as its #+STARTUP: lines say: folded or open, indented or not."
  (call-next-method)
  (let ((options (ignore-errors (org-startup-options buffer))))
    (ignore-errors (org-startup-visibility buffer options))
    (ignore-errors (org-startup-indentation buffer options))))

(defmethod mode-exit ((mode org-mode) buffer)
  "Let go of the view Org mode gave BUFFER (indentation, signs, folds, large
headlines) and of its parse, so that the mode that follows shows the text as
it is."
  (dolist (local '(:line-scale-function :line-prefix-function :display-replacements-function :folds
                   org-indent org-links-literal org-state org-fresh-returns org-hidden org-hidden-anchors
                   org-src-colours))
    (setf (tuatara.buffer:buffer-local buffer local) nil))
  (call-next-method))

(defun install-org-folding (editor)
  "Bind TAB and S-TAB in Org buffers to cycle their subtrees, and keep point out of what is folded."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "TAB" 'org-cycle)
    (define-key keymap "S-TAB" 'org-global-cycle))
  (pushnew 'note-point-before-command (editor-pre-command-functions editor))
  (pushnew 'keep-point-out-of-folds (editor-post-command-functions editor)))
