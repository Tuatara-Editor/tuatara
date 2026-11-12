;;;; lisp/editing/word-completion.lisp --- Completing a word from the words in the buffers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The words that start with the text before point, nearest first: those before point
;;;; and then those after it in this buffer, then those of the other buffers, the ones
;;;; shown first and then by how lately they were used. Only a stretch around point is
;;;; read, so that a large file costs no more than a small one. WORD-COMPLETION-AT-POINT
;;;; gives them to the list beside the cursor that completion at point shows
;;;; (COMPLETE-AT-POINT-IN); EXPAND-WORD puts them in one after another. The idea
;;;; is that of Emacs's dynamic abbreviations (dabbrev).

(in-package #:tuatara.defaults)

(define-setting *word-completion-keep-case* t
  "Whether word completion finds words whatever the case typed, and keeps the
case typed: with \"foo\" typed, a word Foobar is put in as foobar, and with
\"FOO\" typed, it is put in as FOOBAR.
- t: find words whatever the case, and keep the case typed (the standard).
- nil: find only words that start with exactly what is typed, put in as they
  are."
  :values boolean)

(defparameter *word-completion-reach* 65536
  "The octets read either side of point in the buffer a word is completed in.")

(defparameter *word-completion-other-reach* 16384
  "The octets read either side of the point of each other buffer.")

(defparameter *word-completion-other-total* 1048576
  "The most octets read from other buffers in all, so that many buffers cost no
more than a few.")

(defparameter *word-completion-most* 500
  "The most words found for one completion.")

(defparameter *word-completion-longest* 128
  "The most characters of the text before point taken as the start of a word,
and of a word found.")

(defgeneric mode-hyphen-in-words-p (mode)
  (:documentation "True when a hyphen is part of a word in buffers of MODE, as in
prose and in Lisp or CSS names; NIL where it is an operator or a dash.")
  (:method (mode) "No hyphen in words where the mode does not say." (declare (ignore mode)) nil)
  (:method ((mode fundamental-mode)) "Prose joins words with hyphens (well-known)." t)
  (:method ((mode prog-mode)) "In code a hyphen is an operator (a-b is a minus b)." nil)
  (:method ((mode lisp-mode)) "Lisp names are joined by hyphens (buffer-name)." t)
  (:method ((mode emacs-lisp-mode)) "Emacs Lisp names are joined by hyphens." t)
  (:method ((mode clojure-mode)) "Clojure names are joined by hyphens." t)
  (:method ((mode scheme-mode)) "Scheme names are joined by hyphens." t)
  (:method ((mode css-mode)) "CSS names are joined by hyphens (font-size)." t)
  (:method ((mode scss-mode)) "SCSS names are joined by hyphens (font-size)." t))

(defun word-part-p (char)
  "True when CHAR can be part of a word being completed: a letter, a mark joined
to one, a digit, _ or the joiner of an emoji sequence."
  (or (word-character-p char) (digit-char-p char) (char= char #\_) (char= char (code-char #x200D))))

(defun word-part-with-hyphen-p (char)
  "True for what WORD-PART-P is true of, and a hyphen."
  (or (char= char #\-) (word-part-p char)))

(defun word-part-function (buffer)
  "The function telling a character of a word in BUFFER: WORD-PART-P, with
hyphens too where the major mode has them in words (MODE-HYPHEN-IN-WORDS-P)."
  (if (mode-hyphen-in-words-p (buffer-major-mode buffer)) #'word-part-with-hyphen-p #'word-part-p))

(defun word-completion-kind (buffer)
  "What kind of text BUFFER holds as to who may see it: :ENCRYPTED for the
decrypted text of an encrypted file, :EVIDENCE for a file in an evidence folder,
:SAFE for another file opened safely, NIL for any other text."
  (cond ((encrypted-buffer-p buffer) :encrypted)
        ((buffer-evidence buffer) :evidence)
        ((tuatara.io:safe-visit-p buffer) :safe)))

(defun word-completion-found (word prefix)
  "WORD, a word that starts with PREFIX apart from its case, as it is put in
after PREFIX: PREFIX itself followed by the rest of WORD, or all of WORD in
capitals when PREFIX is in capitals, if *WORD-COMPLETION-KEEP-CASE*."
  (cond ((not *word-completion-keep-case*) word)
        ((and (> (count-if #'alpha-char-p prefix) 1) (string= prefix (string-upcase prefix)))
         (sb-unicode:uppercase word))
        (t (concatenate 'string prefix (subseq word (length prefix))))))

(defun map-words-near (function text point prefix part-p skip-point cut-start cut-end)
  "Call FUNCTION with the start and end, as indices of TEXT, of each word in it
that starts with PREFIX, nearest to index POINT first: the word around POINT
unless SKIP-POINT, then those before it, the last first, then those after it,
until FUNCTION returns true. PART-P tells the characters of a word. Words
cut short by an edge of TEXT that CUT-START or CUT-END says is not an edge of
the buffer, and those longer than *WORD-COMPLETION-LONGEST*, are left out.
True when FUNCTION stopped it. Complexity: O(length of TEXT), no garbage."
  (let* ((end (length text)) (size (length prefix))
         (test (if *word-completion-keep-case* #'char-equal #'char=)))
    (flet ((try (start stop)
             (and (>= (- stop start) size)
                  (<= (- stop start) *word-completion-longest*)
                  (not (and (zerop start) cut-start))
                  (not (and (= stop end) cut-end))
                  (null (mismatch prefix text :test test :start2 start :end2 (+ start size)))
                  (funcall function start stop))))
      (let ((around-start (let ((p (position-if-not part-p text :end point :from-end t))) (if p (1+ p) 0)))
            (around-end (or (position-if-not part-p text :start point) end)))
        (or (and (not skip-point) (< around-start around-end) (try around-start around-end))
            (loop with at = around-start
                  for last = (position-if part-p text :end at :from-end t)
                  while last
                  do (let ((start (let ((p (position-if-not part-p text :end last :from-end t))) (if p (1+ p) 0))))
                       (setf at start)
                       (when (try start (1+ last)) (return t))))
            (loop with at = around-end
                  for start = (position-if part-p text :start at)
                  while start
                  do (let ((stop (or (position-if-not part-p text :start start) end)))
                       (setf at stop)
                       (when (try start stop) (return t)))))))))

(defun buffer-words-around (function buffer point prefix reach skip-point)
  "Call FUNCTION with each word of BUFFER that starts with PREFIX, in the REACH
octets either side of byte POINT, nearest first, until it returns true
\(MAP-WORDS-NEAR)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (low (tuatara.buffer:buffer-start buffer))
         (high (tuatara.buffer:buffer-end buffer))
         (from (max low (tuatara.text:rope-char-start rope (max 0 (- point reach)))))
         (to (tuatara.text:rope-char-start rope (min high (+ point reach))))
         (text (tuatara.buffer:buffer-string buffer :start from :end to))
         (index (loop with at = from
                      for i from 0
                      while (and (< at point) (< i (length text)))
                      do (incf at (tuatara.text:char-octet-length (char text i)))
                      finally (return i))))
    (map-words-near (lambda (start stop) (funcall function text start stop))
                    text index prefix (word-part-function buffer) skip-point (> from low) (< to high))))

(defun word-completion-other-buffers (buffer)
  "The buffers whose words complete a word in BUFFER: those shown in a window
first, then the others, those used lately first. Only buffers of the same kind
of text (WORD-COMPLETION-KIND) that visit a file or can be typed in are used, so
that nothing a command made out of a file's content, which may be evidence,
gives words, nor do buffers whose names start with a space, nor hex views."
  (let ((shown (mapcar #'tuatara.display:window-buffer (tuatara.command:editor-windows)))
        (kind (word-completion-kind buffer)))
    (flet ((usable (other)
             (let ((name (tuatara.buffer:buffer-name other)))
               (and (not (eq other buffer))
                    (not (and (plusp (length name)) (char= #\Space (char name 0))))
                    (not (tuatara.display:hex-view-p other))
                    (eq kind (word-completion-kind other))
                    (or (tuatara.buffer:buffer-file other)
                        (not (buffer-read-only-p other)))))))
      (remove-if-not #'usable
                     (remove-duplicates (append shown (tuatara.command:buffers-in-use-order))
                                        :from-end t)))))

(defun word-completion-point (buffer)
  "Where point is in BUFFER, as the window showing it has it."
  (let ((window (tuatara.command:buffer-window buffer)))
    (if window (tuatara.display:window-point window) (tuatara.buffer:buffer-point buffer))))

(defun word-completion-candidates (buffer position)
  "The words that complete the start of a word ending at byte POSITION of
BUFFER, as three values. These are where that start is, a list of CANDIDATE,
nearest first (each with the name of the buffer it came from as its value, NIL
for BUFFER itself), and the start as typed. NIL when no word ends at POSITION
or none begins with it. At most *WORD-COMPLETION-MOST* are found, from about
*WORD-COMPLETION-REACH* octets of BUFFER around POSITION and some of the others
\(*WORD-COMPLETION-OTHER-REACH*, *WORD-COMPLETION-OTHER-TOTAL*)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (low (tuatara.buffer:buffer-start buffer))
         (from (max low (tuatara.text:rope-char-start rope (max 0 (- position (* 4 *word-completion-longest*))))))
         (typed (tuatara.buffer:buffer-string buffer :start from :end position))
         (part-p (word-part-function buffer))
         (cut (position-if-not part-p typed :from-end t))
         (prefix (subseq typed (if cut (1+ cut) 0))))
    (when (and (plusp (length prefix)) (<= (length prefix) *word-completion-longest*) (or cut (= from low)))
      (let ((seen (make-hash-table :test 'equal)) (found '()) (count 0)
            (left *word-completion-other-total*))
        (setf (gethash prefix seen) t)
        (flet ((taker (name)
                 (lambda (text start stop)
                   (let ((word (word-completion-found (subseq text start stop) prefix)))
                     (when (and (not (gethash word seen)) (some #'name-char-p word))
                       (setf (gethash word seen) t)
                       (incf count)
                       (push (tuatara.command:make-candidate word :value name) found)))
                   (>= count *word-completion-most*))))
          (buffer-words-around (taker nil) buffer position prefix *word-completion-reach* t)
          (dolist (other (word-completion-other-buffers buffer))
            (when (or (<= left 0) (>= count *word-completion-most*)) (return))
            (decf left (* 2 *word-completion-other-reach*))
            (buffer-words-around (taker (tuatara.buffer:buffer-name other)) other (word-completion-point other)
                                 prefix *word-completion-other-reach* nil)))
        (when found
          (values (- position (tuatara.text:string-octet-length prefix)) (nreverse found) prefix))))))

(defun word-completion-annotation (candidate)
  "The name of the buffer CANDIDATE, a word found, comes from, none for this buffer."
  (or (tuatara.command:candidate-value candidate) ""))

(defun word-completion-at-point (buffer position)
  "The words that complete the start of the word before POSITION of BUFFER, as
MODE-COMPLETION-AT-POINT gives its offer. These are the start and end of the
text to replace, the candidates, nearest first, the function giving the buffer
each comes from, no documentation, what can be part of a word and true for the
order to be kept. NIL when there are none."
  (multiple-value-bind (start candidates) (word-completion-candidates buffer position)
    (when start
      (values start position candidates #'word-completion-annotation nil (word-part-function buffer) t))))

(defvar *word-expansion* nil
  "The words EXPAND-WORD is putting in one after another, while it is, as a
list \(BUFFER VERSION START END WORDS INDEX PREFIX UNDO). BUFFER and VERSION
are the buffer and the version of its text after the last word went in. START
and END are where the text replaced starts and ends. WORDS are the words, INDEX
the one in now (-1 for the text as typed), PREFIX the text as typed, and UNDO
the buffer's undo history before the first word went in.")

(defun word-expansion-continues-p (buffer)
  "True when the last command was EXPAND-WORD and nothing has changed since it
put in a word at point of BUFFER."
  (let ((state *word-expansion*))
    (and state
         (eq (editor-last-command *editor*) 'expand-word)
         (eq (first state) buffer)
         (eq (second state) (tuatara.buffer:buffer-version buffer))
         (= (fourth state) (tuatara.buffer:buffer-point buffer)))))

(defun word-expansion-put (buffer state index)
  "Replace the text STATE (see *WORD-EXPANSION*) says went in by word INDEX, or
by the text as typed for -1, and remember it. All the words put in one after
another are one step to undo."
  (destructuring-bind (b version start end words old prefix undo) state
    (declare (ignore b version old))
    (let* ((text (if (minusp index) prefix (nth index words)))
           (after (tuatara.buffer:buffer-replace buffer start end text)))
      (setf (tuatara.buffer:buffer-point buffer) after)
      (tuatara.buffer:merge-undo-groups buffer undo)
      (setf *word-expansion*
            (list buffer (tuatara.buffer:buffer-version buffer) start after words index prefix undo)))))

(define-command expand-word ()
  "Put in place of the start of the word before point the nearest word that
starts with it, looked for in this buffer, then in the others (those shown
first). Typed again straight after, it puts in the next nearest in its place;
when the words are out the text as typed comes back, and one more press starts
again. One undo takes all of them back. A word is letters, digits and _, and
hyphens in prose and Lisp. About 64 KiB around point are read here, less in
other buffers; files opened safely, evidence and decrypted files give words
only to buffers of the same kind. \\[complete-word] lists the words instead.
With *WORD-COMPLETION-KEEP-CASE* the case typed is kept."
  (let ((b (current-buffer)))
    (cond ((tuatara.display:hex-view-p b) (message "Nothing to expand in the hex view"))
          ((buffer-read-only-p b)
           (message "Buffer is read-only: ~a" (tuatara.buffer:buffer-name b)))
          ((word-expansion-continues-p b)
           (let* ((state *word-expansion*) (next (1+ (sixth state))))
             (cond ((< next (length (fifth state))) (word-expansion-put b state next))
                   (t (word-expansion-put b state -1)
                      (message "No more words start with ~a" (seventh state))))))
          (t
           (let ((undo (tuatara.buffer:buffer-undo-stack b)))
             (multiple-value-bind (start candidates prefix) (word-completion-candidates b (point))
               (if (null start)
                   (message "No word to expand here")
                   (word-expansion-put b (list b nil start (point)
                                               (mapcar #'tuatara.command:candidate-string candidates) -1 prefix undo)
                                       0))))))))

(define-command complete-word ()
  "List the words in the buffers that start with the text before point beside
the cursor, the nearest first, each with the buffer it is in when that is not
this one; \\[expand-word] puts them in one after another. A plain terminal sends
this key as C-M-_, which is redo there; \\[completion-at-point] (C-M-i) lists
the words as well where the major mode has no completion of its own. Typing
narrows the list;
\\<popup>\\[popup-next] and \\[popup-previous] choose, \\[popup-insert] or
\\`TAB' put the chosen one in, \\[popup-quit] closes it. When one word alone
fits, it goes in at once."
  (let ((b (current-buffer)))
    (cond ((tuatara.display:hex-view-p b) (message "Nothing to complete in the hex view"))
          ((buffer-read-only-p b)
           (message "Buffer is read-only: ~a" (tuatara.buffer:buffer-name b)))
          (t (complete-at-point-in b :words :only)))))
