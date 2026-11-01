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

(defvar *word-completion-keep-case* t
  "True to find words that start with what is typed whatever the case, and to
keep the case typed: with \"foo\" typed, a word Foobar is put in as foobar, and
with \"FOO\" typed, it is put in as FOOBAR. NIL to find only words that start
with exactly what is typed, put in as they are.")

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

(defun word-part-p (char)
  "True when CHAR can be part of a word being completed: a letter, a digit, _
or a hyphen."
  (or (name-char-p char) (char= char #\-)))

(defun word-completion-private-p (buffer)
  "True when BUFFER holds text that must not turn up in other documents: a file
opened safely, or the decrypted text of an encrypted one."
  (or (tuatara.io:safe-visit-p buffer) (encrypted-buffer-p buffer)))

(defun word-completion-found (word prefix)
  "WORD, a word that starts with PREFIX apart from its case, as it is put in
after PREFIX: PREFIX itself followed by the rest of WORD, or all of WORD in
capitals when PREFIX is in capitals, if *WORD-COMPLETION-KEEP-CASE*."
  (cond ((not *word-completion-keep-case*) word)
        ((and (> (count-if #'alpha-char-p prefix) 1) (string= prefix (string-upcase prefix))) (string-upcase word))
        (t (concatenate 'string prefix (subseq word (length prefix))))))

(defun words-in-text (text point prefix skip-point cut-start cut-end)
  "The words in TEXT that start with PREFIX, nearest to index POINT first: those
that end before it, the last first, then those after it. The word around POINT
is left out when SKIP-POINT, and the words cut short by an edge of TEXT that
CUT-START or CUT-END says is not an edge of the buffer. Each is as WORD-COMPLETION-FOUND gives it. Complexity:
O(length of TEXT)."
  (let ((before '()) (after '()) (end (length text)) (size (length prefix))
        (test (if *word-completion-keep-case* #'char-equal #'char=)))
    (loop with i = 0
          while (< i end)
          do (let ((start (position-if #'word-part-p text :start i)))
               (if (null start)
                   (setf i end)
                   (let ((stop (or (position-if-not #'word-part-p text :start start) end)))
                     (setf i stop)
                     (when (and (>= (- stop start) size)
                                (<= (- stop start) *word-completion-longest*)
                                (not (and skip-point (<= start point stop)))
                                (not (and (zerop start) cut-start))
                                (not (and (= stop end) cut-end))
                                (null (mismatch prefix text :test test :start2 start :end2 (+ start size))))
                       (let ((word (subseq text start stop)))
                         (if (<= stop point) (push word before) (push word after))))))))
    (mapcar (lambda (word) (word-completion-found word prefix))
            (nconc before (nreverse after)))))

(defun buffer-words-around (buffer point prefix reach skip-point)
  "The words of BUFFER that start with PREFIX, in the REACH octets either side of
byte POINT, nearest first (WORDS-IN-TEXT)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (low (tuatara.buffer:buffer-start buffer))
         (high (tuatara.buffer:buffer-end buffer))
         (from (max low (tuatara.text:rope-char-start rope (max 0 (- point reach)))))
         (to (tuatara.text:rope-char-start rope (min high (+ point reach))))
         (before (tuatara.buffer:buffer-string buffer :start from :end (min point to)))
         (after (tuatara.buffer:buffer-string buffer :start (min point to) :end to)))
    (words-in-text (concatenate 'string before after) (length before) prefix skip-point (> from low) (< to high))))

(defun word-completion-other-buffers (buffer)
  "The buffers whose words complete a word in BUFFER: those shown in a window
first, then the others, those used lately first. Left out are the buffers whose
names start with a space, the hex views, and, unless BUFFER holds such text
itself, those of private text (WORD-COMPLETION-PRIVATE-P)."
  (let* ((shown (mapcar #'tuatara.display:window-buffer (tuatara.command:editor-windows)))
         (own (word-completion-private-p buffer)))
    (flet ((usable (other)
             (let ((name (tuatara.buffer:buffer-name other)))
               (and (not (eq other buffer))
                    (not (and (plusp (length name)) (char= #\Space (char name 0))))
                    (not (tuatara.display:hex-view-p other))
                    (or own (not (word-completion-private-p other)))))))
      (remove-if-not #'usable
                     (remove-duplicates (append shown (tuatara.command:buffers-in-use-order))
                                        :from-end t)))))

(defun word-completion-point (buffer)
  "Where point is in BUFFER, as the window showing it has it."
  (let ((window (tuatara.command:buffer-window buffer)))
    (if window (tuatara.display:window-point window) (tuatara.buffer:buffer-point buffer))))

(defun word-completion-candidates (buffer position)
  "The words that complete the start of a word ending at byte POSITION of
BUFFER, as three values: where that start is, a list of CANDIDATE, nearest first
\(each with the name of the buffer it came from as its value, NIL for BUFFER
itself), and the start as typed; NIL when no word ends at POSITION or none
begins with it. At most *WORD-COMPLETION-MOST* are found, from
about *WORD-COMPLETION-REACH* octets of BUFFER around POSITION and some of the
others (*WORD-COMPLETION-OTHER-REACH*, *WORD-COMPLETION-OTHER-TOTAL*)."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (from (max (tuatara.buffer:buffer-start buffer)
                    (tuatara.text:rope-char-start rope (max 0 (- position (* 4 *word-completion-longest*))))))
         (typed (tuatara.buffer:buffer-string buffer :start from :end position))
         (cut (position-if-not #'word-part-p typed :from-end t))
         (prefix (subseq typed (if cut (1+ cut) 0))))
    (when (and (plusp (length prefix)) (<= (length prefix) *word-completion-longest*)
               (or cut (= from (tuatara.buffer:buffer-start buffer))))
      (let ((seen (make-hash-table :test 'equal)) (found '()) (count 0)
            (left *word-completion-other-total*))
        (setf (gethash prefix seen) t)
        (flet ((take (words name)
                 (dolist (word words)
                   (when (and (< count *word-completion-most*) (not (gethash word seen)) (some #'name-char-p word))
                     (setf (gethash word seen) t)
                     (incf count)
                     (push (tuatara.command:make-candidate word :value name) found)))))
          (take (buffer-words-around buffer position prefix *word-completion-reach* t) nil)
          (dolist (other (word-completion-other-buffers buffer))
            (when (or (<= left 0) (>= count *word-completion-most*)) (return))
            (decf left (* 2 *word-completion-other-reach*))
            (take (buffer-words-around other (word-completion-point other) prefix *word-completion-other-reach* nil)
                  (tuatara.buffer:buffer-name other))))
        (when found
          (values (- position (tuatara.text:string-octet-length prefix)) (nreverse found) prefix))))))

(defun word-completion-annotation (candidate)
  "The name of the buffer CANDIDATE, a word found, comes from, none for this buffer."
  (or (tuatara.command:candidate-value candidate) ""))

(defun word-completion-at-point (buffer position)
  "The words that complete the start of the word before POSITION of BUFFER, as
MODE-COMPLETION-AT-POINT gives its offer: the start and end of the text to
replace, the candidates, nearest first, the function giving the buffer each
comes from, no documentation, what can be part of a word and true for the
order to be kept; NIL when there are none."
  (multiple-value-bind (start candidates) (word-completion-candidates buffer position)
    (when start
      (values start position candidates #'word-completion-annotation nil #'word-part-p t))))

(defvar *word-expansion* nil
  "The words EXPAND-WORD is putting in one after another, while it is, as a list
\(BUFFER VERSION START END WORDS INDEX PREFIX): the buffer and the version of
its text after the last word went in, where the text replaced starts and ends,
the words, the one in now (-1 for the text as typed) and the text as typed.")

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
by the text as typed for -1, and remember it."
  (destructuring-bind (b version start end words old prefix) state
    (declare (ignore b version old))
    (let* ((text (if (minusp index) prefix (nth index words)))
           (after (tuatara.buffer:buffer-replace buffer start end text)))
      (setf (tuatara.buffer:buffer-point buffer) after
            *word-expansion* (list buffer (tuatara.buffer:buffer-version buffer) start after words index prefix)))))

(define-command expand-word ()
  "Put in place of the start of the word before point the nearest word that
starts with it, in this buffer and then in the others (those shown first). Typed
again straight after, it puts in the next nearest in place of that one, and
says so when the words are out, the text as typed back. The words come from
about 64 KiB around point, and from files opened safely and encrypted ones only
into a buffer of the same kind. \\[complete-word] lists them instead. With
*WORD-COMPLETION-KEEP-CASE* the case typed is kept."
  (let ((b (current-buffer)))
    (cond ((tuatara.display:hex-view-p b) (message "Nothing to expand in the hex view"))
          ((tuatara.buffer:buffer-local b :read-only) (message "Buffer is read-only: ~a" (tuatara.buffer:buffer-name b)))
          ((word-expansion-continues-p b)
           (let* ((state *word-expansion*) (next (1+ (sixth state))))
             (cond ((< next (length (fifth state))) (word-expansion-put b state next))
                   (t (word-expansion-put b state -1)
                      (message "No more words start with ~a" (seventh state))))))
          (t
           (multiple-value-bind (start candidates prefix) (word-completion-candidates b (point))
             (if (null start)
                 (message "No word to expand here")
                 (let ((end (point)))
                   (word-expansion-put b (list b nil start end (mapcar #'tuatara.command:candidate-string candidates) -1 prefix)
                                       0))))))))

(define-command complete-word ()
  "List the words in the buffers that start with the text before point beside
the cursor, the nearest first, each with the buffer it is in when that is not
this one; \\[expand-word] puts them in one after another. Typing narrows the list;
\\<popup>\\[popup-next] and \\[popup-previous] choose, \\[popup-insert] or
\\`TAB' put the chosen one in, \\[popup-quit] closes it. When one word alone
fits, it goes in at once."
  (let ((b (current-buffer)))
    (cond ((tuatara.display:hex-view-p b) (message "Nothing to complete in the hex view"))
          ((tuatara.buffer:buffer-local b :read-only) (message "Buffer is read-only: ~a" (tuatara.buffer:buffer-name b)))
          (t (complete-at-point-in b :words :only)))))
