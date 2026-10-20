;;;; lisp/editing/snippets.lisp --- Snippets: text with fields to fill in, put in by a key and TAB.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A snippet is a file in the folder of a major mode under
;;;; ~/.config/tuatara/snippets/ (org-mode/, prog-mode/, fundamental-mode/ for
;;;; every buffer; a mode's snippets serve the modes made from it too):
;;;;
;;;;   # name: Source block
;;;;   # key: <s
;;;;   # --
;;;;   #+begin_src ${1|shell,python,rust|}
;;;;   $0
;;;;   #+end_src
;;;;
;;;; Its key typed before the cursor and \`TAB' put it in, the key's place
;;;; taken; M-x insert-snippet chooses one by name, each listed with its key
;;;; and whose it is; M-x copy-snippet makes one of Tuatara's yours to change.
;;;; Its text is written as
;;;; language servers write theirs: $1, $2... are fields \`TAB' goes to in
;;;; turn and \`S-TAB' back, ${1:text} one holding text that typing replaces,
;;;; ${1|a,b,c|} one offering a choice, $0 where the cursor ends; a field's
;;;; number used again is filled in as the field is. $NAME stands for a value:
;;;; the date ($CURRENT_YEAR, $CURRENT_MONTH, $CURRENT_DATE...), the file
;;;; ($TM_FILENAME, $TM_FILENAME_BASE, $TM_DIRECTORY), the text selected
;;;; ($TM_SELECTED_TEXT), your name ($AUTHOR); \$ is a dollar sign. Nothing in
;;;; a snippet is ever run. Tuatara's own snippets come first; one of yours with
;;;; the same name in the same mode takes its place.

(in-package #:tuatara.defaults)

(defvar *snippet-directories* nil
  "The folders your snippets are in, each with a folder for each major mode
\(org-mode/, prog-mode/, fundamental-mode/), a snippet a file in it; NIL, as
standard, for snippets/ in your configuration folder
\(~/.config/tuatara/snippets/, XDG_CONFIG_HOME's when it is set).")

(defun snippet-directories ()
  "The folders your snippets are in (*SNIPPET-DIRECTORIES*): snippets/ in
your configuration folder (CONFIG-PATH) when it names none."
  (or (setting-list '*snippet-directories* *snippet-directories*)
      (list (config-path "snippets/"))))

(defvar *snippet-tab* t
  "True for a snippet's key before the cursor and \\`TAB' to put the snippet in.")

(defparameter *built-in-snippets* (built-in-data-files "snippets/" :by-folder t)
  "Tuatara's own snippets, each (MODE-NAME FILE-NAME TEXT): Org's blocks and
keyword lines (<s, <q, <e ...).")

;;; Snippets and where they are

(defstruct (snippet (:constructor make-snippet (name key body)) (:copier nil))
  "A snippet: NAME, what it is called; KEY, what typed before the cursor and
\\`TAB' put it in (NIL for none); BODY, its text as written; MODE, the name of
the mode whose folder it is in, FILE its file's name there and OWN true for
one of Tuatara's (NIL for one of yours)."
  name key body mode file own)

(setf (documentation 'snippet-p 'function) "True when OBJECT is a SNIPPET.")

(defun read-snippet (file-name text)
  "The snippet a file called FILE-NAME holding TEXT is: its # name: and # key:
lines before # -- (its name the file's when it gives none), its body after;
the whole text its body when it has no such head (TEMPLATE-HEAD)."
  (multiple-value-bind (head body) (template-head text)
    (let ((key (cdr (assoc "key" head :test #'string=))))
      (make-snippet (or (cdr (assoc "name" head :test #'string=)) (pathname-name file-name))
                    (and key (plusp (length key)) key)
                    body))))

(defun read-snippet-of (mode-name file-name text own)
  "The snippet of the mode called MODE-NAME a file called FILE-NAME holding
TEXT is (READ-SNIPPET), OWN true for one of Tuatara's."
  (let ((s (read-snippet file-name text)))
    (setf (snippet-mode s) mode-name (snippet-file s) file-name (snippet-own s) own)
    s))

(defvar *snippet-folders* (make-hash-table :test 'equal)
  "Your snippets as read from each folder of them: its native name to the
names and stamps of its files then, and the snippets read from them.")

(defun user-snippets-of (mode-name)
  "Your snippets for the mode called MODE-NAME, as SNIPPETs: those of its
folder in each of *SNIPPET-DIRECTORIES*, read again only from a folder whose
files have changed, come or gone since (FOLDER-FILES)."
  (loop for directory in (snippet-directories)
        for place = (tuatara.io:native (own-folder directory mode-name))
        for files = (loop for (name kind nil nil stamp) in (tuatara.io:directory-entries place :stamps t)
                          when (and (eq kind :regular) (own-file-name-p name))
                            collect (cons name stamp))
        append (let ((kept (gethash place *snippet-folders*)))
                 (if (and kept (equalp (car kept) files))
                     (cdr kept)
                     (let ((snippets (loop for (file . text) in (folder-files (list directory) mode-name)
                                           when text collect (read-snippet-of mode-name file text nil))))
                       (setf (gethash place *snippet-folders*) (cons files snippets))
                       snippets)))))

(defvar *built-in-snippets-read* nil
  "Tuatara's own snippets as read: (LIST . TABLE), TABLE from each mode's name
to its SNIPPETs, read from LIST, the *BUILT-IN-SNIPPETS* of then.")

(defun built-in-snippets-of (mode-name)
  "Tuatara's own snippets for the mode called MODE-NAME, as SNIPPETs: read once,
and again only when *BUILT-IN-SNIPPETS* is another list."
  (let ((read *built-in-snippets-read*))
    (unless (eq (car read) *built-in-snippets*)
      (let ((table (make-hash-table :test 'equal)))
        (loop for (name file text) in (reverse *built-in-snippets*)
              do (push (read-snippet-of name file text t) (gethash name table)))
        (setf read (cons *built-in-snippets* table)
              *built-in-snippets-read* read)))
    (values (gethash mode-name (cdr read)))))

(defun mode-snippets (buffer)
  "The snippets for BUFFER: those of its major mode and of the modes it is
made from, the nearest first, then fundamental-mode's; in each, Tuatara's own
and yours, one of yours taking the place of one of the same name."
  (let ((snippets '()))
    (dolist (mode (remove-duplicates (append (mode-precedence (buffer-major-mode buffer)) '(fundamental-mode)) :from-end t))
      (let* ((name (string-downcase (symbol-name mode)))
             (yours (user-snippets-of name)))
        (dolist (s (in-place-of-own yours (built-in-snippets-of name) #'snippet-name))
          (unless (find (snippet-name s) snippets :key #'snippet-name :test #'string=)
            (push s snippets)))))
    (nreverse snippets)))

;;; The body: language servers' snippet syntax

(defparameter *snippet-depth* 16
  "How deep fields within fields of a snippet are read; deeper, ${ is text.")

(defun snippet-name-char-p (char)
  "True for CHAR when it can begin the name of a value ($NAME): a letter or _."
  (or (alpha-char-p char) (char= char #\_)))

(defun snippet-run-end (text at test)
  "Where the run of characters of TEXT from AT that TEST is true for ends."
  (or (position-if-not test text :start at) (length text)))

(defun snippet-word-end (text at)
  "Where the name of a value ($NAME) in TEXT from AT ends: after its letters, digits and _."
  (snippet-run-end text at (lambda (c) (or (alphanumericp c) (char= c #\_)))))

(defun parse-snippet-brace (text at depth)
  "The part of a snippet's body TEXT written ${...}, its inside beginning at AT
and DEPTH fields deep, and the index after it, as two values: a field ${1} or
${1:text}, a choice ${1|a,b|} or a value ${NAME} or ${NAME:text}. NIL when
what follows ${ is none of them, the ${ then being text."
  (let ((n (length text)))
    (flet ((inner (from)
             (multiple-value-bind (parts after) (parse-snippet-body text :start from :stop #\} :depth (1+ depth))
               (values parts (min n (1+ after)))))
           ;; anything up to the } (a transform) left out
           (past-brace (from) (min n (1+ (or (position #\} text :start from) (1- n))))))
      (cond ((digit-char-p (char text at))
             (let* ((end (snippet-run-end text at #'digit-char-p))
                    (number (parse-integer text :start at :end end)))
               (case (and (< end n) (char text end))
                 (#\: (multiple-value-bind (parts after) (inner (1+ end))
                        (values (list :field number parts) after)))
                 (#\| (let ((close (search "|}" text :start2 end)))
                        (and close (values (list :choice number (split-choices-of (subseq text (1+ end) close)))
                                           (+ close 2)))))
                 (t (values (list :field number '()) (past-brace end))))))
            ((snippet-name-char-p (char text at))
             (let ((end (snippet-word-end text at)))
               (if (and (< end n) (char= (char text end) #\:))
                   (multiple-value-bind (parts after) (inner (1+ end))
                     (values (list :value (subseq text at end) parts) after))
                   (values (list :value (subseq text at end) '()) (past-brace end)))))))))

(defun parse-snippet-body (text &key (start 0) (stop nil) (depth 0))
  "TEXT, a snippet's body, as a list of parts: strings, (:FIELD N PARTS),
\(:CHOICE N OPTIONS) and (:VALUE NAME PARTS), PARTS what a field holds or a
value stands in for when it has none. Reading ends at STOP, a character
\(the } closing a field), or the end; the index after it is the second value.
DEPTH is how many fields this one is within (*SNIPPET-DEPTH* at most)."
  (let ((parts '()) (text-run (make-string-output-stream)) (i start) (n (length text)))
    (flet ((flush ()
             (let ((s (get-output-stream-string text-run)))
               (when (plusp (length s)) (push s parts))))
           (take (part after)
             (push part parts)
             (setf i after)))
      (loop while (< i n)
            do (let ((c (char text i))
                     (next (and (< (1+ i) n) (char text (1+ i)))))
                 (cond ((and stop (char= c stop)) (return))
                       ((and (char= c #\\) next (find next "$}\\,|"))
                        (write-char next text-run) (incf i 2))
                       ((and (char= c #\$) next (digit-char-p next))
                        (flush)
                        (let ((end (snippet-run-end text (1+ i) #'digit-char-p)))
                          (take (list :field (parse-integer text :start (1+ i) :end end) '()) end)))
                       ((and (char= c #\$) next (snippet-name-char-p next))
                        (flush)
                        (let ((end (snippet-word-end text (1+ i))))
                          (take (list :value (subseq text (1+ i) end) '()) end)))
                       ((and (char= c #\$) (eql next #\{) (< (+ i 2) n) (< depth *snippet-depth*)
                             (multiple-value-bind (part after) (parse-snippet-brace text (+ i 2) depth)
                               (when part (flush) (take part after) t))))
                       (t (write-char c text-run) (incf i)))))
      (flush)
      (values (nreverse parts) i))))

(defun split-choices-of (text)
  "The options of a choice field written TEXT, apart by commas, \\, a comma."
  (let ((options '()) (current (make-string-output-stream)) (i 0))
    (loop while (< i (length text))
          do (let ((c (char text i)))
               (cond ((and (char= c #\\) (< (1+ i) (length text)))
                      (write-char (char text (1+ i)) current) (incf i 2))
                     ((char= c #\,) (push (get-output-stream-string current) options) (incf i))
                     (t (write-char c current) (incf i)))))
    (push (get-output-stream-string current) options)
    (nreverse options)))

(defparameter *snippet-date-values*
  '(("CURRENT_YEAR" . "%Y") ("CURRENT_YEAR_SHORT" . "%y") ("CURRENT_MONTH" . "%m")
    ("CURRENT_MONTH_NAME" . "%B") ("CURRENT_MONTH_NAME_SHORT" . "%b") ("CURRENT_DATE" . "%d")
    ("CURRENT_DAY_NAME" . "%A") ("CURRENT_DAY_NAME_SHORT" . "%a")
    ("CURRENT_HOUR" . "%H") ("CURRENT_MINUTE" . "%M") ("CURRENT_SECOND" . "%S"))
  "The values a snippet's $NAME gives of the date and time, each (NAME .
PATTERN), PATTERN as FORMAT-TIME writes it.")

(defun snippet-value (name buffer selected)
  "What $NAME stands for in BUFFER, SELECTED the text selected: a string, or
NIL for a name it does not know."
  (let ((file (tuatara.buffer:buffer-file buffer))
        (date (cdr (assoc name *snippet-date-values* :test #'string=))))
    (cond (date (format-time date (get-universal-time)))
          ((string= name "TM_SELECTED_TEXT") (or selected ""))
          ((string= name "TM_FILENAME") (if file (tuatara.io:native-file-name file) ""))
          ((string= name "TM_FILENAME_BASE") (if file (tuatara.io:native-file-stem file) ""))
          ((string= name "TM_DIRECTORY") (if file (tuatara.io:native (tuatara.io:parent-path file)) ""))
          ((string= name "TM_FILEPATH") (if file (tuatara.io:native file) ""))
          ((string= name "AUTHOR") (user-full-name)))))

;;; Fields

(defstruct (snippet-field (:constructor make-snippet-field (number start choices outer index)) (:copier nil))
  "A place in a snippet that is filled in: NUMBER, its number ($1 is 1);
START and END, where it begins and ends, octets of the snippet's text as it is
made, then anchors in the buffer; CHOICES, a choice's options; OUTER, the
field it is within, or NIL; INDEX, how many fields come before it in the text;
MIRRORS, the other places its number stands, when it is the one \\`TAB' goes
to; GONE, true once the text it was in was replaced."
  number start (end 0) choices outer index (mirrors '()) (gone nil))

(defun field-within-p (field other)
  "True when FIELD is within OTHER, however deep."
  (loop for outer = (snippet-field-outer field) then (snippet-field-outer outer)
        while outer
        thereis (eq outer other)))

(defun render-snippet (parts buffer selected indent)
  "PARTS, a snippet's body read (PARSE-SNIPPET-BODY), as the text it puts in
BUFFER, each new line after INDENT, and its fields: two values, the text and a
list of SNIPPET-FIELDs in the order of the text, their START and END octets of
the text."
  (let ((out (make-string-output-stream)) (at 0) (fields '()) (count 0))
    (labels ((emit (s)
               (let ((s (cl-ppcre:regex-replace-all (string #\Newline) s (format nil "~%~a" indent))))
                 (write-string s out)
                 (incf at (tuatara.text:string-octet-length s))))
             (walk (parts outer)
               (dolist (part parts)
                 (if (stringp part)
                     (emit part)
                     (destructuring-bind (kind name inner) part
                       (if (eq kind :value)
                           (let ((value (snippet-value name buffer selected)))
                             (if value (emit value) (walk inner outer)))
                           (let ((field (make-snippet-field name at (and (eq kind :choice) inner) outer count)))
                             (incf count)
                             (push field fields)
                             (when (eq kind :field) (walk inner field))
                             (setf (snippet-field-end field) at))))))))
      (walk parts nil))
    (values (get-output-stream-string out) (nreverse fields))))

(defun order-fields (fields)
  "FIELDS, SNIPPET-FIELDs in the order of the text, in the order \\`TAB' goes
to them: 1, 2... then 0. Of the places a number stands, the first is the
field, the outermost when they are within each other, and the others its
mirrors (SNIPPET-FIELD-MIRRORS), filled in as it is; but never a place within
it or holding it. A snippet with no 0 ends after its text."
  (let ((chosen (make-hash-table)))
    (dolist (field fields)
      (unless (gethash (snippet-field-number field) chosen)
        (setf (gethash (snippet-field-number field) chosen) field)))
    (dolist (field (reverse fields))
      (let ((first (gethash (snippet-field-number field) chosen)))
        (unless (or (eq field first) (field-within-p field first) (field-within-p first field))
          (push field (snippet-field-mirrors first)))))
    (let ((order (sort (loop for field being the hash-values of chosen collect field) #'< :key #'snippet-field-number)))
      (if (zerop (snippet-field-number (first order)))
          (append (rest order) (list (first order)))
          order))))

;;; A snippet being filled in

(defstruct (snippet-session (:constructor make-snippet-session (buffer fields order start end)) (:copier nil))
  "A snippet put in BUFFER whose fields are being filled in: FIELDS, its
SNIPPET-FIELDs in the order of the text; ORDER, those \\`TAB' goes to, in
turn (ORDER-FIELDS); START and END, anchors at the ends of all of it; AT, the
index in ORDER of the field the cursor went to; FRESH, true while that
field's text is as the snippet gave it, typing replacing it; KEPT, the
buffer's override keymap before; UNDO-SINCE, the buffer's undo stack before a
field's text was replaced, so that the replacing is one change to undo."
  buffer fields order start end (at -1) (fresh nil) (kept nil) (undo-since nil))

(defvar *snippet-keymap*
  (let ((map (make-keymap :name "snippet")))
    (define-keys map '(("TAB" snippet-next-field) ("S-TAB" snippet-previous-field)))
    map)
  "The keys of a snippet being filled in, before the buffer's own: \\`TAB' to
the next field, \\`S-TAB' back.")

(defun buffer-snippet (buffer)
  "The snippet being filled in in BUFFER, or NIL."
  (tuatara.buffer:buffer-local buffer 'snippet-session))

(defun current-field (session)
  "The field of SESSION the cursor went to, or NIL."
  (let ((at (snippet-session-at session)))
    (and (>= at 0) (nth at (snippet-session-order session)))))

(defun snippet-field-bounds (session field)
  "Where FIELD of SESSION is now, as two values: its start and end octets."
  (let ((version (tuatara.buffer:buffer-version (snippet-session-buffer session))))
    (values (tuatara.text:anchor-position (snippet-field-start field) version)
            (tuatara.text:anchor-position (snippet-field-end field) version))))

(defun aim-fields (session target)
  "Make the ends of SESSION's fields keep to their own text while TARGET's
changes: text put in at TARGET's ends is TARGET's and that of the fields
holding it, never that of a field before or after it."
  (let ((version (tuatara.buffer:buffer-version (snippet-session-buffer session))))
    (dolist (field (snippet-session-fields session))
      (multiple-value-bind (start end) (snippet-field-bounds session field)
        (multiple-value-bind (start-bias end-bias)
            (cond ((or (eq field target) (field-within-p target field)) (values :before :after))
                  ((< (snippet-field-index field) (snippet-field-index target)) (values :before :before))
                  (t (values :after :after)))
          (setf (snippet-field-start field) (tuatara.text:make-anchor version start :bias start-bias)
                (snippet-field-end field) (tuatara.text:make-anchor version end :bias end-bias)))))))

(defun drop-fields-within (session field)
  "Forget the fields of SESSION within FIELD, whose text was replaced: they are
no longer gone to nor filled in."
  (when (some (lambda (f) (field-within-p f field)) (snippet-session-fields session))
    (let ((current (current-field session)))
      (setf (snippet-session-fields session)
            (remove-if (lambda (f) (and (field-within-p f field) (setf (snippet-field-gone f) t)))
                       (snippet-session-fields session))
            (snippet-session-order session) (remove-if #'snippet-field-gone (snippet-session-order session))
            (snippet-session-at session) (or (position current (snippet-session-order session)) -1)))))

(defun end-snippet (session)
  "Stop filling SESSION's snippet in: its keys gone."
  (let ((buffer (snippet-session-buffer session)))
    (when (eq (buffer-snippet buffer) session)
      (setf (tuatara.buffer:buffer-local buffer 'snippet-session) nil
            (tuatara.buffer:buffer-local buffer :override-keymap) (snippet-session-kept session)))))

(defun leave-snippet (session)
  "End SESSION's snippet with the cursor after $0's text, or after the
snippet's when it has no $0."
  (let* ((buffer (snippet-session-buffer session))
         (zero (find 0 (snippet-session-order session) :key #'snippet-field-number)))
    (end-snippet session)
    (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
    (goto (if zero
              (nth-value 1 (snippet-field-bounds session zero))
              (tuatara.text:anchor-position (snippet-session-end session) (tuatara.buffer:buffer-version buffer))))))

(defun go-to-field (session index)
  "Go to the field at INDEX of SESSION's fields, its text selected for typing
to replace, a choice's options offered; past the last, end the snippet where
$0 is (or after it). At $0 the snippet is done: at once when $0 holds no
text, else once its text is typed over or left."
  (let ((buffer (snippet-session-buffer session))
        (fields (snippet-session-order session)))
    (if (>= index (length fields))
        (leave-snippet session)
        (let ((field (nth index fields)))
          (setf (snippet-session-at session) index)
          (aim-fields session field)
          (multiple-value-bind (start end) (snippet-field-bounds session field)
            (cond ((snippet-field-choices field)
                   ;; a choice: its options listed beside the cursor
                   (goto start)
                   (setf (snippet-session-fresh session) nil)
                   (complete-in-text buffer start end (mapcar #'make-candidate (snippet-field-choices field))
                                     :constituent (lambda (c) (char/= c #\Newline)) :keep-order t))
                  ((< start end)
                   (select-bounds buffer start end t)
                   (setf (snippet-session-fresh session) t))
                  (t (goto start)
                     (setf (snippet-session-fresh session) nil))))
          (when (and (zerop (snippet-field-number field)) (not (snippet-session-fresh session)))
            (end-snippet session))))))

(defun snippet-indent (buffer start)
  "The spaces and tabs the line of BUFFER holding START begins with, up to START."
  (let ((text (tuatara.buffer:buffer-string buffer :start (tuatara.buffer:buffer-line-start buffer start) :end start)))
    (subseq text 0 (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text) (length text)))))

(defun put-snippet (buffer start end snippet &key selected)
  "Put SNIPPET in BUFFER in place of the text from START below END (its key,
or the selection, SELECTED its text), one change to undo, and go to its first
field."
  (multiple-value-bind (text fields)
      (render-snippet (parse-snippet-body (snippet-body snippet)) buffer selected (snippet-indent buffer start))
    (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit start end (and (plusp (length text)) (tuatara.text:string-octets text)))))
    (let ((version (tuatara.buffer:buffer-version buffer))
          (after (+ start (tuatara.text:string-octet-length text)))
          (old (buffer-snippet buffer)))
      (dolist (field fields)
        (setf (snippet-field-start field) (tuatara.text:make-anchor version (+ start (snippet-field-start field)))
              (snippet-field-end field) (tuatara.text:make-anchor version (+ start (snippet-field-end field)))))
      (when old (end-snippet old))
      (if (null fields)
          (goto after)
          (let ((session (make-snippet-session buffer fields (order-fields fields)
                                               (tuatara.text:make-anchor version start :bias :before)
                                               (tuatara.text:make-anchor version after :bias :after))))
            (setf (snippet-session-kept session) (tuatara.buffer:buffer-local buffer :override-keymap)
                  (tuatara.buffer:buffer-local buffer 'snippet-session) session
                  (tuatara.buffer:buffer-local buffer :override-keymap) *snippet-keymap*)
            (go-to-field session 0))))))

(define-command snippet-next-field ()
  "Go to the next field of the snippet being filled in; past the last, end it where it puts the cursor."
  (let ((session (buffer-snippet (current-buffer))))
    (when session
      (go-to-field session (1+ (snippet-session-at session))))))

(define-command snippet-previous-field ()
  "Go back to the field of the snippet before the one the cursor is in."
  (let ((session (buffer-snippet (current-buffer))))
    (when session
      (go-to-field session (max 0 (1- (snippet-session-at session)))))))

(defun snippet-before-command (editor command)
  "Before a command in a buffer filling a snippet in: typing or yanking into
a field as the snippet gave it replaces its text, and the fields within it
are gone; undoing ends the snippet, its fields no longer following the text."
  (declare (ignore editor))
  (let* ((buffer (current-buffer))
         (session (buffer-snippet buffer)))
    (cond ((null session))
          ((member command '(undo redo)) (end-snippet session))
          ((snippet-session-fresh session)
           (setf (snippet-session-fresh session) nil)
           (when (member command '(self-insert-command yank))
             (let ((field (current-field session)))
               (multiple-value-bind (start end) (snippet-field-bounds session field)
                 (when (< start end)
                   (setf (tuatara.buffer:buffer-mark-active-p buffer) nil
                         (snippet-session-undo-since session) (tuatara.buffer:buffer-undo-stack buffer))
                   (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit start end nil)))
                   (drop-fields-within session field)
                   (goto start)))))))))

(defun fill-mirrors (session)
  "Fill in the places each field of SESSION's number stands again as the field
is now; the fields within a place filled in are gone."
  (let ((buffer (snippet-session-buffer session)) (changed nil))
    (dolist (field (snippet-session-order session))
      (let ((mirrors (remove-if #'snippet-field-gone (snippet-field-mirrors field))))
        (when mirrors
          (let ((text (multiple-value-bind (start end) (snippet-field-bounds session field)
                        (tuatara.buffer:buffer-string buffer :start start :end end))))
            (dolist (mirror mirrors)
              (multiple-value-bind (start end) (snippet-field-bounds session mirror)
                (unless (string= text (tuatara.buffer:buffer-string buffer :start start :end end))
                  (aim-fields session mirror)
                  (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit start end (and (plusp (length text)) (tuatara.text:string-octets text))))
                                              :group t)
                  (drop-fields-within session mirror)
                  (setf changed t))))))))
    (let ((current (current-field session)))
      (when (and changed current) (aim-fields session current)))))

(defun snippet-after-command (editor)
  "After a command in a buffer filling a snippet in: the places a field's
number stands again filled in as it is, one change to undo with the field's
text replaced; the snippet ended once the cursor has left it, or once $0's
text was typed over or left."
  (declare (ignore editor))
  (let* ((buffer (current-buffer))
         (session (buffer-snippet buffer)))
    (when session
      (let* ((version (tuatara.buffer:buffer-version buffer))
             (from (tuatara.text:anchor-position (snippet-session-start session) version))
             (to (tuatara.text:anchor-position (snippet-session-end session) version))
             (point (tuatara.buffer:buffer-point buffer))
             (current (current-field session)))
        (if (not (<= from point to))
            (end-snippet session)
            (fill-mirrors session))
        (let ((since (snippet-session-undo-since session)))
          (when since
            (setf (snippet-session-undo-since session) nil)
            (tuatara.buffer:merge-undo-groups buffer since)))
        (when (and current (zerop (snippet-field-number current)) (not (snippet-session-fresh session)))
          (end-snippet session))))))

;;; Putting one in

(defun snippet-key-before (buffer)
  "The snippet whose key stands before point in BUFFER, after a blank or at
its line's start, and where the key starts, as two values; NIL when there is none."
  (let* ((point (tuatara.buffer:buffer-point buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (from (loop for from from (max (tuatara.buffer:buffer-line-start buffer point) (- point 256))
                     until (tuatara.text:char-boundary-p rope from)
                     finally (return from)))
         (before (tuatara.buffer:buffer-string buffer :start from :end point))
         (word (subseq before (1+ (or (position-if (lambda (c) (member c '(#\Space #\Tab))) before :from-end t) -1)))))
    (when (plusp (length word))
      (let ((snippet (find word (mode-snippets buffer) :key #'snippet-key :test #'equal)))
        (when snippet
          (values snippet (- point (tuatara.text:string-octet-length word))))))))

(defun expand-snippet-at-point ()
  "Put in the snippet whose key stands before point, when one does
\(*SNIPPET-TAB*); true when it did."
  (let ((buffer (current-buffer)))
    (when (and *snippet-tab* (not (tuatara.buffer:buffer-local buffer :read-only)) (not (tuatara.buffer:buffer-mark-active-p buffer)))
      (multiple-value-bind (snippet start) (snippet-key-before buffer)
        (when snippet
          (put-snippet buffer start (tuatara.buffer:buffer-point buffer) snippet)
          t)))))

(defun snippet-candidate (snippet value)
  "SNIPPET as a candidate to choose by its name, said with its key and whose
it is, VALUE what choosing it gives."
  (make-candidate (snippet-name snippet) :value value
                  :annotation (format nil "~@[~a  ~]~:[yours~;Tuatara's~]~@[ (~a)~]" (snippet-key snippet) (snippet-own snippet)
                                      (and (snippet-mode snippet) (string/= (snippet-mode snippet) "fundamental-mode") (snippet-mode snippet)))))

(define-command insert-snippet ()
  "Put in a snippet chosen by its name among those of the current buffer's
mode (each listed with its key), at point; with the selection active, in its
place, the selected text where it has $TM_SELECTED_TEXT."
  (let* ((buffer (current-buffer))
         (snippets (mode-snippets buffer)))
    (if (null snippets)
        (message "No snippets for this buffer's mode (~a)" (abbreviated-path (first (snippet-directories))))
        (let ((snippet (completing-read-value "Snippet: " (mapcar (lambda (s) (snippet-candidate s s)) snippets) :require-match t)))
          (when (snippet-p snippet)
            (multiple-value-bind (start end) (if (tuatara.buffer:buffer-mark-active-p buffer)
                                                 (tuatara.buffer:buffer-region buffer)
                                                 (values (point) (point)))
              (let ((selected (and (< start end) (tuatara.buffer:buffer-string buffer :start start :end end))))
                (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
                (put-snippet buffer start end snippet :selected selected))))))))

(define-command copy-snippet ()
  "Copy one of Tuatara's own snippets, chosen by its name, into your snippets
folder (the first of *snippet-directories*, in the folder of its mode) and
visit it: your copy, changed as you like, takes the place of Tuatara's. One
you have already is visited as it is."
  (copy-chosen-own-file "Copy snippet: "
                        (loop for (mode file text) in *built-in-snippets*
                              collect (snippet-candidate (read-snippet-of mode file text t) (list mode file text)))
                        (snippet-directories)
                        "snippet"))

(defun install-snippets (editor)
  "Bind \\`C-c y' to \\[insert-snippet], but in the buffers made by commands
\(special-mode and those made from it: lists, help, the terminal), where
nothing is typed; keep snippets' fields filled in as they are typed."
  (define-key *global-keymap* "C-c y" 'insert-snippet)
  (define-key (mode-keymap 'special-mode) "C-c y" :undefined)
  (pushnew 'snippet-before-command (editor-pre-command-functions editor))
  (pushnew 'snippet-after-command (editor-post-command-functions editor)))
