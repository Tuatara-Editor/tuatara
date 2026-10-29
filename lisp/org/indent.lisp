;;;; lisp/org/indent.lisp --- Org's indented view and bullets: each line indented by its headline's level.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The indented view (org-indent-mode in Emacs) shows each line of an Org
;;;; buffer indented by the level of the headline it is under: a headline's
;;;; stars but the last hidden, the text under it two columns further in for
;;;; each level. The bullets (Emacs's org-bullets) show a headline's last star
;;;; as a sign of its level. Neither changes the text: what is shown is laid
;;;; out so, the file is as it was.

(in-package #:tuatara.defaults)

(defun org-headline-index (buffer)
  "Where BUFFER's headlines begin, in octets, with their levels: a vector of
(BEGIN . LEVEL) in order, made of its parse once, again when the parse is
another (not at each change of the text: the parse is behind the text until
the next one, and its headlines stand where it had them)."
  (let ((state (org-buffer-state buffer)))
    (multiple-value-bind (tree) (org-buffer-tree buffer)
      (if (and tree (eq (org-state-headlines-tree state) tree))
          (org-state-headlines state)
          (let ((rope (tuatara.buffer:buffer-text buffer)))
            (when (and tree (tuatara.text:rope-indexed-p rope))
              (let* ((chars (tuatara.text:rope-chars rope))
                     (returns (org-state-returns state))
                     (headlines (remove-if (lambda (headline) (> (org-node-begin headline) chars)) (org-headlines tree)))
                     ;; the parse's characters leave out the carriage returns the text has
                     (index (coerce (loop for headline in headlines
                                          for p = (org-node-begin headline)
                                          collect (cons (tuatara.text:char->byte rope (min chars (if returns (+ p (org-returns-before returns p)) p)))
                                                        (org-property headline :level)))
                                    'simple-vector)))
                (setf (org-state-headlines state) index
                      (org-state-headlines-tree state) tree)
                index)))))))

(defun org-headline-before (index position)
  "The entry of INDEX (ORG-HEADLINE-INDEX) of the last headline beginning at or before POSITION, or NIL."
  (let ((low 0) (high (length index)))
    (setf low (tuatara.runtime:bisect (middle low high) (<= (car (svref index middle)) position)) high low)
    (and (plusp low) (svref index (1- low)))))

(defun org-line-prefix (buffer line-start continuation)
  "What the indented view shows before the line of BUFFER beginning at
LINE-START (a row CONTINUATION of it when true): before a headline of level N,
N - 1 columns, its hidden stars making the rest; before the text under it,
2N columns; before what is above the first headline, nothing."
  (when (org-indented-p buffer)
    (let* ((index (org-headline-index buffer))
           (headline (and index (org-headline-before index line-start))))
      (when headline
        (let ((level (cdr headline)))
          (make-string (if (and (= (car headline) line-start) (not continuation))
                           (1- level)
                           (* 2 level))
                       :initial-element #\Space))))))

(defvar *org-level-heights* (cons -1 nil)
  "Whether any headline level's face has a :height, as of the faces'
generation in the car: looked up once for each change of the faces.")

(defun org-level-heights-p ()
  "True when some headline level's face has a :height, so that a headline may
be drawn large; looked up once for each change of the faces (*FACE-GENERATION*)."
  (let ((generation tuatara.display:*face-generation*))
    (unless (eql (car *org-level-heights*) generation)
      (setf *org-level-heights*
            (cons generation (loop for level from 1 to 8 thereis (tuatara.display:face-attribute (org-level-face level) :height)))))
    (cdr *org-level-heights*)))

(defun org-line-scale (buffer line-start)
  "How many rows tall the line of BUFFER beginning at LINE-START is drawn
where the window can draw rows large: a headline as tall as its level's face's
:height, (set-face :org-level-1 :height 1.25); one past the eighth level as
the eighth, whose colours go round to the first's again; any other
line one. Nothing is looked up while no level's face has a :height."
  (unless (org-level-heights-p) (return-from org-line-scale 1))
  (let* ((index (org-headline-index buffer))
         (headline (and index (org-headline-before index line-start)))
         (rope (tuatara.buffer:buffer-text buffer)))
    (or (and headline (= (car headline) line-start)
             ;; a parse a moment behind the text: only where a star still stands
             (< line-start (tuatara.text:rope-bytes rope)) (= 42 (tuatara.text:rope-octet rope line-start))
             (tuatara.display:face-attribute (org-level-face (min 8 (cdr headline))) :height))
        1)))

(defun org-headline-signs (buffer start end)
  "The signs headlines from octet START below END show in place of their last
star (*ORG-BULLETS*), as ORG-DISPLAY-REPLACEMENTS has them."
  (let ((index (org-headline-index buffer))
        (rope (tuatara.buffer:buffer-text buffer))
        (found '()))
    (when index
      (loop for i from (tuatara.runtime:bisect (middle 0 (length index)) (< (car (svref index middle)) start))
              below (length index)
            for (begin . level) = (svref index i)
            while (< begin end)
            do (let ((star (+ begin level -1)))
                 ;; a parse a moment behind the text: only where a star still stands
                 (when (and (< star (tuatara.text:rope-bytes rope)) (= 42 (tuatara.text:rope-octet rope star)))
                   (push (list star (1+ star) (let ((bullets (setting-list '*org-bullets* *org-bullets*)))
                                                 (and bullets (nth (mod (1- level) (length bullets)) bullets)))
                               (org-level-face level))
                         found)))))
    (nreverse found)))

(defun org-item-signs (buffer start end)
  "The signs list items from octet START below END show for their bullets
\(*ORG-LIST-BULLETS*) and checkboxes (*ORG-CHECKBOX-SIGNS*), as
ORG-DISPLAY-REPLACEMENTS has them: only items the parse found, each sign put
only where the text still holds what it stands for."
  (let* ((tree (org-buffer-tree buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (returns (org-state-returns (org-buffer-state buffer)))
         (from (org-byte->char buffer start returns))
         (to (org-byte->char buffer (min end size) returns))
         (found '()))
    (labels ((octets-at (at n) (and (<= (+ at n) size) (tuatara.text:rope-string rope :start at :end (+ at n))))
             (walk (node)
               (when (and (< (org-node-begin node) to) (> (org-node-end node) from))
                 (when (and (eq (org-node-type node) 'item) (<= from (org-node-begin node)))
                   (let* ((line (org-char->byte buffer (org-node-begin node) returns))
                          (bullet (string-trim " " (org-property node :bullet)))
                          (head (octets-at line (min 200 (- size line))))
                          (at (and head (position-if-not (lambda (c) (member c '(#\Space #\Tab))) head)))
                          (sign (cdr (assoc bullet (setting-list '*org-list-bullets* *org-list-bullets* :pairs t) :test #'string=))))
                     (when (and at (< (+ line at) end) (uiop:string-prefix-p bullet (subseq head at)))
                       (when sign
                         (push (list (+ line at) (+ line at (length bullet)) sign :org-list-bullet) found))
                       ;; the checkbox after the bullet and its spaces
                       (let* ((box-at (position #\[ head :start (+ at (length bullet))))
                              (box (and box-at (<= (+ box-at 3) (length head))
                                        (string-trim " " (subseq head (+ at (length bullet)) box-at))))
                              (box-sign (and box (string= box "") (org-property node :checkbox)
                                             (cdr (assoc (subseq head box-at (+ box-at 3)) (setting-list '*org-checkbox-signs* *org-checkbox-signs* :pairs t) :test #'string=)))))
                         ;; a space after the sign, so that one drawn wider than its cell is not half under the cursor
                         (when box-sign
                           (push (list (+ line box-at) (+ line box-at 3) (concatenate 'string box-sign " ") :org-checkbox)
                                 found))))))
                 (org-children-in-range node from to #'walk))))
      (when (and tree (or *org-list-bullets* *org-checkbox-signs*))
        (walk tree)))
    (sort found #'< :key #'first)))

(defun org-display-replacements (buffer start end)
  "What BUFFER shows otherwise from START below END: with *ORG-BULLETS*, each
headline's last star as its level's sign, in its level's face; list items' bullets and checkboxes
as signs (*ORG-LIST-BULLETS*, *ORG-CHECKBOX-SIGNS*); while links are shown as
their labels (\\[org-toggle-link-display]), what is around each label hidden,
each entity shown as its character, and the braces around what redact and
protect hide, but in the call point is in."
  (let ((found (merge 'list
                      (and *org-bullets* (org-headline-signs buffer start end))
                      (and (or *org-list-bullets* *org-checkbox-signs*) (org-item-signs buffer start end))
                      #'< :key #'first)))
    ;; the links' ranges are kept between frames: merged as a copy, never spliced into
    (if (org-links-descriptive-p buffer)
        (merge 'list found (copy-list (org-hidden-ranges-shown buffer start end)) #'< :key #'first)
        found)))

(define-command org-indent-mode ()
  "Show the current Org buffer indented, or as it is again: each line
indented by the level of the headline it is under, a headline's stars but the
last hidden. The text is not changed.
*org-startup-indented* opens every Org file so."
  (let ((buffer (current-buffer)))
    (setf (tuatara.buffer:buffer-local buffer 'org-indent) (not (org-indented-p buffer)))
    (message "Indented view ~:[off~;on~]" (org-indented-p buffer))))

(defun org-startup-indentation (buffer options)
  "Show BUFFER, an Org buffer whose #+STARTUP: words are OPTIONS
\(ORG-STARTUP-OPTIONS), indented as *ORG-STARTUP-INDENTED* says, or as the last
of indent and noindent there says; give it the indented view's and the
bullets' ways of showing it, and links shown as their labels or as written as
*ORG-LINK-DESCRIPTIVE* or the last of descriptivelinks and literallinks says."
  (setf (tuatara.buffer:buffer-local buffer :line-prefix-function) 'org-line-prefix
        (tuatara.buffer:buffer-local buffer :line-scale-function) 'org-line-scale
        (tuatara.buffer:buffer-local buffer :display-replacements-function) 'org-display-replacements)
  (let ((indent (org-startup-choice options '("indent" "noindent")))
        (links (org-startup-choice options '("literallinks" "descriptivelinks"))))
    (setf (tuatara.buffer:buffer-local buffer 'org-indent)
          (if indent (string= indent "indent") *org-startup-indented*))
    (setf (tuatara.buffer:buffer-local buffer 'org-links-literal)
          (if links (string= links "literallinks") (not *org-link-descriptive*)))))
