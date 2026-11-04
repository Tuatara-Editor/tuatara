;;;; lisp/selecting/registers.lisp --- Registers: a place, text, a block or a number kept under a character.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A register is a short-lived bookmark or kill: one character names it, it
;;;; lasts as long as the editor runs, and it keeps one of four things. A place
;;;; is an anchor in its buffer (so it follows the text through edits) with the
;;;; file place a bookmark keeps (so it is found again once
;;;; the buffer is closed). Text is a kill, a block the rectangle store's own
;;;; kind of value, a number a whole number.

(in-package #:tuatara.defaults)

(defstruct (register (:constructor make-register (kind value &optional sources)) (:copier nil))
  "What a register keeps: KIND :POSITION, :TEXT, :BLOCK or :NUMBER, and VALUE,
a REGISTER-PLACE, a string or a rope (as the kill ring holds), a rectangle
\(what *KILLED-RECTANGLE* holds) or a whole number; SOURCES, the buffers of
decrypted text its text or block came from, with which it goes
\(FORGET-PLAINTEXT-COPIES)."
  kind value sources)

(setf (documentation 'make-register 'function)
      "Return a REGISTER of KIND keeping VALUE, taken from the buffers of decrypted text SOURCES."
      (documentation 'register-p 'function) "True when OBJECT is a REGISTER.")

(defun plaintext-sources (buffer &optional sources)
  "SOURCES, buffers of decrypted text something kept came from, with BUFFER
when its text is decrypted (TUATARA.IO:BUFFER-PLAINTEXT-P)."
  (if (tuatara.io:buffer-plaintext-p buffer) (adjoin buffer sources) sources))

(defstruct (register-place (:constructor make-register-place (buffer anchor place)) (:copier nil))
  "A place a register keeps: in BUFFER, at ANCHOR, which follows the text through
edits, and PLACE, the FILE-PLACE to go back to once BUFFER is closed (NIL for a
buffer with no file). BUFFER is NIL once it is closed."
  buffer anchor place)

(setf (documentation 'make-register-place 'function) "Return a REGISTER-PLACE."
      (documentation 'register-place-p 'function) "True when OBJECT is a REGISTER-PLACE.")

(defvar *registers* (make-hash-table)
  "The registers, each known by the character that names it, as a REGISTER.
They last as long as the editor runs.")

(defun register-names ()
  "The characters naming a register, in order."
  (sort (loop for name being the hash-keys of *registers* collect name) #'char<))

(defun read-register-name (prompt)
  "Ask with PROMPT for the character naming a register, one key, no RET: a
letter, a digit or a sign, a capital not the same as its small letter.
\\`C-g' quits. Returns the character, or NIL, said why, for a key that is
none."
  (let ((key (read-key prompt)))
    (when (quit-key-p key) (keyboard-quit-signal))
    (let ((base (tuatara.frontend:key-base key)))
      (cond ((and (characterp base) (graphic-char-p base) (char/= base #\Space)
                  (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key)))
             base)
            (t (message "~a names no register: a letter, a digit or a sign does"
                        (tuatara.frontend:key-description (tuatara.frontend:key-spec key)))
               nil)))))

(defun kept-in-register (name kinds)
  "The register NAME when it keeps one of KINDS, a list; else NIL, said what
it keeps instead, or that it keeps nothing."
  (let ((register (gethash name *registers*)))
    (cond ((null register) (message "Register ~a is empty" name) nil)
          ((member (register-kind register) kinds) register)
          (t (message "Register ~a keeps ~a" name (register-kind-words register)) nil))))

(defun register-kind-words (register)
  "What REGISTER keeps, in a word or two: a place, text, a rectangle, a number."
  (ecase (register-kind register)
    (:position "a place") (:text "text") (:block "a rectangle") (:number "a number")))

;;; Keeping

(defun copy-to-register-value (buffer)
  "What \\[register-copy] keeps from BUFFER: its rectangle when one is shown
\(its rows, left in the rectangle store as they were) as :BLOCK, else the
region's text as :TEXT. Two values, the kind and the value; NIL, said why,
when there is nothing to keep."
  (if (rectangle-live-p buffer)
      (let ((before *killed-rectangle*))
        (unwind-protect
             (progn (copy-selected-rectangle)
                    (unless (eq before *killed-rectangle*)
                      (values :block *killed-rectangle*)))
          (setf *killed-rectangle* before)))
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
        (cond ((and start (< start end))
               (multiple-value-prog1 (values :text (region-text buffer start end))
                 (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)))
              (t (message "The region is empty") nil)))))

(define-command register-copy ()
  "Keep the selection under a character you type, as long as the editor runs:
the text between point and mark, or the block of columns when a rectangle is
selected (\\[rectangle-mark-mode]). \\[register-insert] puts it in again,
anywhere, \\[register-add] adds to it. Keeping again under the same
character replaces what was there. The kill ring is left as it was."
  (let ((name (read-register-name "Copy to register: ")))
    (when name
      (multiple-value-bind (kind value) (copy-to-register-value (current-buffer))
        (when kind
          (setf (gethash name *registers*) (make-register kind value (plaintext-sources (current-buffer))))
          (message "Register ~a keeps ~a" name (register-kind-words (gethash name *registers*))))))))

(define-command register-point ()
  "Keep where point is under a character you type, as long as the editor
runs: \\[register-jump] goes back there, the text above it changed or not,
and even after the buffer is closed when it was a file. A place worth keeping
beyond this session is a bookmark (\\[bookmark-set])."
  (let ((name (read-register-name "Point to register: ")))
    (when name
      (let* ((buffer (current-buffer))
             (offset (point))
             (anchor (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) offset)))
        (setf (gethash name *registers*)
              (make-register :position (make-register-place buffer anchor (file-place-here buffer offset))))
        (message "Register ~a keeps this place" name)))))

(define-command register-number ()
  "Keep a whole number under a character you type: the prefix argument, else
one asked for. \\[register-add] adds to it and \\[register-insert] writes
it into the text."
  (let ((name (read-register-name "Number to register: ")))
    (when name
      (let ((number (or (prefix-argument)
                        (read-whole-number "Number (~d): " 0 :valid (constantly t) :refusal "Not a whole number."))))
        (setf (gethash name *registers*) (make-register :number number))
        (message "Register ~a keeps ~d" name number)))))

(defun add-to-text-register (name register buffer front)
  "Add the region of BUFFER to the text REGISTER, known as NAME: after what it
keeps, or before with FRONT."
  (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
    (if (not (and start (< start end)))
        (message "The region is empty")
        (let ((text (region-text buffer start end)) (kept (register-value register)))
          (setf (gethash name *registers*)
                (make-register :text (if front (kill-join text kept) (kill-join kept text))
                               (plaintext-sources buffer (register-sources register)))
                (tuatara.buffer:buffer-mark-active-p buffer) nil)
          (message "Added to register ~a" name)))))

(define-command register-add ()
  "Add to what a register keeps: a number grows by the prefix argument (1
without one, below 0 to take away), text has the selected text put after it.
Asks for the character; the register must keep one of the two already."
  (let ((name (read-register-name "Add to register: ")))
    (when name
      (let ((register (kept-in-register name '(:number :text))))
        (when register
          (if (eq (register-kind register) :number)
              (let ((sum (+ (register-value register) (prefix 1))))
                (setf (gethash name *registers*) (make-register :number sum))
                (message "Register ~a keeps ~d" name sum))
              (add-to-text-register name register (current-buffer) nil)))))))

(define-command register-prepend ()
  "Put the selected text before the text a register keeps, asked for by its
character, as \\[register-add] puts it after."
  (let ((name (read-register-name "Prepend to register: ")))
    (when name
      (let ((register (kept-in-register name '(:text))))
        (when register (add-to-text-register name register (current-buffer) t))))))

;;; Decrypted text kept no longer than its buffer

(defun forget-plaintext-copies (buffer)
  "Let go of every copy of BUFFER's decrypted text the editor keeps beside
it: the kills taken from it (*PLAINTEXT-KILLS*) leave the kill ring, and the
registers keeping its text or a block of it are emptied. Runs as a buffer is
killed, and as its client ends."
  (let ((editor (or *editor* (running-editor))))
    (when editor
      (setf (editor-kill-ring editor)
            (remove-if (lambda (kill) (member buffer (gethash kill *plaintext-kills*))) (editor-kill-ring editor)))))
  (loop for name in (register-names)
        when (member buffer (register-sources (gethash name *registers*)))
          do (remhash name *registers*)))

(pushnew 'forget-plaintext-copies tuatara.buffer:*kill-buffer-functions*)

;;; Using

(defun jump-to-register-place (name place &key other)
  "Go to PLACE, kept by register NAME: in its buffer while that is open, where
its anchor stands now; else in the file it was in, at its octet or where the
text that followed it is now (GO-TO-FILE-PLACE); in another window with OTHER.
A place in a buffer that kept no file, once closed, is said to be gone."
  (let ((buffer (register-place-buffer place)))
    (cond ((live-buffer-p buffer)
           (if other (pop-to-buffer buffer :other t) (show-buffer buffer))
           (goto-shown buffer (or (anchor-in buffer (register-place-anchor place))
                                  (file-place-found buffer (register-place-place place)))))
          ((register-place-place place)
           (go-to-file-place (format nil "Register ~a" name) (register-place-place place) :other other))
          (t (message "Register ~a: its buffer was closed and had no file to go back to" name)))))

(define-command register-jump ()
  "Go to the place a register keeps (\\[register-point]), asked for by its
character: in its buffer, shown, point where it was set, moved with the text
around it. When the buffer has been closed the file is visited again."
  (let ((name (read-register-name "Jump to register: ")))
    (when name
      (let ((register (kept-in-register name '(:position))))
        (when register (jump-to-register-place name (register-value register)))))))

(define-command register-insert ()
  "Put what a register keeps into the text at point, asked for by its
character: text as it was, with point after it and the mark before; a block
\(\\[register-copy] of a rectangle) as a block of columns, as
\\[yank-rectangle] pastes one, with a prefix argument each row that many
times across; a number as digits. The register keeps what it had."
  (let ((name (read-register-name "Insert register: ")))
    (when name
      (let ((register (kept-in-register name '(:text :block :number))))
        (when register
          (let ((buffer (current-buffer)) (value (register-value register)))
            (check-writable buffer)
            (ecase (register-kind register)
              (:text (insert-with-mark-before (text-for-buffer value buffer (point))))
              (:number (insert-with-mark-before (format nil "~d" value)))
              (:block (let ((*killed-rectangle* value)) (yank-rectangle))))))))))

;;; Registers whose buffer is closed

(defun keep-registered-places (buffer)
  "Kill-buffer function: each place registers keep in BUFFER becomes the file
place BUFFER has now (its file as it is named now, the octet and the text
following), so that the register still leads there."
  (when (plusp (hash-table-count *registers*))
    (maphash (lambda (name register)
               (let ((kept (and (eq (register-kind register) :position) (register-value register))))
                 (when (and kept (eq (register-place-buffer kept) buffer))
                   (let* ((old (register-place-place kept))
                          (offset (or (anchor-in buffer (register-place-anchor kept))
                                      (and old (file-place-offset old))
                                      0))
                          (now (or (file-place-here buffer offset)
                                   (and old (make-file-place (file-place-path old) offset
                                                             (text-after-place buffer offset)
                                                             (file-place-safe old))))))
                     (setf (gethash name *registers*)
                           (make-register :position (make-register-place nil nil now)))))))
             *registers*)))

;;; The list

(define-major-mode registers-mode marked-list-mode
  "The list of registers (\\[register-list]): a line for each, with the
character that names it, what it keeps and a look at that.
\\<registers-mode>\\[list-visit] or \\`RET' goes to the place on the line (a message for the
others, \\[list-visit-other] in another window), \\[list-mark-entry] marks a register (>),
\\[list-mark-for-deletion] marks it for deletion (D), \\[list-unmark-entry] and
\\[list-unmark-all] unmark, \\[list-delete-marked] empties the marked registers (the one on the
line when none is) after asking, \\[list-execute] those marked for deletion;
\\[revert-buffer] lists them again; \\[quit-window] or \\`h' returns."
  :lighter "Registers" :interactive nil)

(defparameter *registers-title-lines* 2
  "The lines of *Registers* above its first register: the column titles and the dashes under them.")

(defparameter *registers-columns* '((" M" :width 2) ("Register" :width 10) ("Keeps" :width 12) ("Contents"))
  "The columns of the register list, as TABLE-HEADING-PIECES takes them: the
mark (> or D), the character, what it keeps and a look at that.")

(defun register-contents-words (register)
  "A look at what REGISTER keeps, on one line: the file or buffer and octet of
a place, the start of a text and how long it is, a block's size, a number."
  (let ((value (register-value register)))
    (ecase (register-kind register)
      (:position (let ((buffer (register-place-buffer value)))
                   (format nil "~a, octet ~:d" (if (live-buffer-p buffer)
                                                    (tuatara.buffer:buffer-name buffer)
                                                    (if (register-place-place value)
                                                        (abbreviated-path (file-place-path (register-place-place value)))
                                                        "buffer closed"))
                           (or (and buffer (anchor-in buffer (register-place-anchor value)))
                               (and (register-place-place value) (file-place-offset (register-place-place value)))
                               0))))
      (:text (format nil "~a (~:d character~:p)"
                     (if (stringp value)
                         (tuatara.display:shorten-string value 60 :one-line t)
                         "kept where it is")
                     (kill-size value)))
      (:block (if (rectangle-source-p value)
                  (format nil "~:d row~:p, kept where they are" (rectangle-source-rows value))
                  (format nil "~:d row~:p: ~a" (length value)
                          (tuatara.display:shorten-string (first value) 40 :one-line t))))
      (:number (format nil "~d" value)))))

(defun registers-render (list &optional (point :line))
  "Fill LIST, the register list, with a line for each register (REGISTER-NAMES),
marks kept on those still there, point on the same register or line, at its
start, or with POINT :keep at its column."
  (render-table-list
   list *registers-columns* (register-names)
   (lambda (name mark section)
     (declare (ignore section))
     (let ((register (gethash name *registers*)))
       (list (list-mark-sign mark)
             (cons (string name) (list-mark-face mark))
             (string-capitalize (register-kind-words register) :end 1)
             (register-contents-words register))))
   :after (and (null (register-names))
               (list (format nil "~%   No registers: ~a keeps a selection, ~a a place, ~a a number~%"
                             (substitute-command-keys "\\[register-copy]")
                             (substitute-command-keys "\\[register-point]")
                             (substitute-command-keys "\\[register-number]"))))
   :point point :first-line *registers-title-lines*
   :refill (lambda (list) (registers-render list :keep))))

(defun registers-key (list)
  "What the register list LIST shows, cheaply: the registers in order, each
the object it keeps; another key means the list is out of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (mapcar (lambda (name) (cons name (gethash name *registers*))) (register-names)))

(keep-shown-current 'registers-mode 'registers-key)

(define-command register-list ()
  "List the registers in *Registers*: the character that names each, what it
keeps (a place, text, a rectangle, a number) and a look at that.
\\<registers-mode>\\[list-visit] or \\`RET' goes to the place on the line,
\\[list-mark-entry] marks a register, \\[list-delete-marked] empties the marked ones (the one on the line
when none is), \\[quit-window] or \\`h' returns."
  (let ((list (buffer-in-mode "*Registers*" 'registers-mode)))
    (registers-render list)
    (show-buffer list)))

(defmethod list-entry-noun ((mode registers-mode))
  "A register."
  "register")

(defmethod visit-entry ((mode registers-mode) list name other)
  "Go to the place register NAME keeps, in another window with OTHER; for the
others, say what they keep and how to put it in."
  (declare (ignore list))
  (let ((register (gethash name *registers*)))
    (cond ((null register))
          ((eq (register-kind register) :position)
           (jump-to-register-place name (register-value register) :other other))
          (t (message "Register ~a keeps ~a: ~a puts it in the text where point is"
                      name (register-kind-words register) (substitute-command-keys "\\[register-insert]"))))))

(defmethod delete-entries ((mode registers-mode) list names mark)
  "Empty the registers NAMES, after asking."
  (declare (ignore mark))
  (when (y-or-n-p* (format nil "~a?" (list-question "Empty" "register" names)))
    (dolist (name names) (remhash name *registers*))
    (unmark-list list)
    (registers-render list)
    (message "~d register~:p emptied" (length names))))

(defun install-registers ()
  "Have a register's place follow its buffer's file once the buffer is closed,
and give the register list its keys."
  (pushnew 'keep-registered-places tuatara.buffer:*kill-buffer-functions*)
  (entry-list-keys 'registers-mode "the register list (\\[register-list])"))
