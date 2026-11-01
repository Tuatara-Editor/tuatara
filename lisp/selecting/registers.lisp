;;;; lisp/selecting/registers.lisp --- Registers: a place, text, a block or a number kept under a character.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A register is a short-lived bookmark or kill: one character names it, it
;;;; lasts as long as the editor runs, and it keeps one of four things. A place
;;;; is an anchor in its buffer (so it follows the text through edits) with the
;;;; file, octet and following text a bookmark keeps (so it is found again once
;;;; the buffer is closed). Text is a kill, a block the rectangle store's own
;;;; kind of value, a number a whole number.

(in-package #:tuatara.defaults)

(defstruct (register (:constructor make-register (kind value)) (:copier nil))
  "What a register keeps: KIND :POSITION, :TEXT, :BLOCK or :NUMBER, and VALUE,
a REGISTER-PLACE, a string or a rope (as the kill ring holds), a rectangle
\(what *KILLED-RECTANGLE* holds) or a whole number."
  kind value)

(setf (documentation 'make-register 'function) "Return a REGISTER of KIND keeping VALUE."
      (documentation 'register-p 'function) "True when OBJECT is a REGISTER.")

(defstruct (register-place (:constructor make-register-place (buffer anchor path offset context safe))
                           (:copier nil))
  "A place a register keeps: in BUFFER, at ANCHOR, which follows the text through
edits. Once BUFFER is closed, BUFFER is NIL and the file at PATH (NIL for a
buffer with none) is gone back to, at OFFSET or where CONTEXT, the text that
followed the place, now stands; SAFE when the file was opened safely."
  buffer anchor path offset context safe)

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
  "What \\[copy-to-register] keeps from BUFFER: its rectangle when one is shown
\(its rows, left in the rectangle store as they were) as :BLOCK, else the
region's text as :TEXT. Two values, the kind and the value; NIL, said why,
when there is nothing to keep."
  (if (rectangle-live-p buffer)
      (let ((before *killed-rectangle*))
        (copy-selected-rectangle)
        (unless (eq before *killed-rectangle*)
          (values :block (shiftf *killed-rectangle* before))))
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
        (cond ((and start (< start end))
               (multiple-value-prog1 (values :text (region-text buffer start end))
                 (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)))
              (t (message "The region is empty") nil)))))

(define-command copy-to-register ()
  "Keep the selection under a character you type, as long as the editor runs:
the text between point and mark, or the block of columns when a rectangle is
selected (\\[rectangle-mark-mode]). \\[insert-register] puts it in again,
anywhere, \\[add-to-register] adds to it. Keeping again under the same
character replaces what was there. The kill ring is left as it was."
  (let ((name (read-register-name "Copy to register: ")))
    (when name
      (multiple-value-bind (kind value) (copy-to-register-value (current-buffer))
        (when kind
          (setf (gethash name *registers*) (make-register kind value))
          (message "Register ~a keeps ~a" name (register-kind-words (gethash name *registers*))))))))

(define-command point-to-register ()
  "Keep where point is under a character you type, as long as the editor
runs: \\[jump-to-register] goes back there, the text above it changed or not,
and even after the buffer is closed when it was a file. A place worth keeping
beyond this session is a bookmark (\\[bookmark-set])."
  (let ((name (read-register-name "Point to register: ")))
    (when name
      (let* ((buffer (current-buffer))
             (offset (point))
             (anchor (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) offset)))
        (setf (gethash name *registers*)
              (make-register :position
                             (make-register-place buffer anchor (bookmark-target buffer) offset
                                                  (bookmark-context buffer offset) (tuatara.io:safe-visit-p buffer))))
        (message "Register ~a keeps this place" name)))))

(define-command number-to-register ()
  "Keep a whole number under a character you type: the prefix argument, else
one asked for. \\[add-to-register] adds to it and \\[insert-register] writes
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
                (make-register :text (if front (kill-join text kept) (kill-join kept text)))
                (tuatara.buffer:buffer-mark-active-p buffer) nil)
          (message "Added to register ~a" name)))))

(define-command add-to-register ()
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

(define-command prepend-to-register ()
  "Put the selected text before the text a register keeps, asked for by its
character, as \\[add-to-register] puts it after."
  (let ((name (read-register-name "Prepend to register: ")))
    (when name
      (let ((register (kept-in-register name '(:text))))
        (when register (add-to-text-register name register (current-buffer) t))))))

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
                                  (bookmark-place buffer (register-place-offset place)
                                                  (register-place-context place)))))
          ((register-place-path place)
           (go-to-file-place (format nil "Register ~a" name) (register-place-path place)
                             (register-place-offset place) (register-place-context place)
                             (register-place-safe place) :other other))
          (t (message "Register ~a: its buffer was closed and had no file to go back to" name)))))

(define-command jump-to-register ()
  "Go to the place a register keeps (\\[point-to-register]), asked for by its
character: in its buffer, shown, point where it was set, moved with the text
around it. When the buffer has been closed the file is visited again."
  (let ((name (read-register-name "Jump to register: ")))
    (when name
      (let ((register (kept-in-register name '(:position))))
        (when register (jump-to-register-place name (register-value register)))))))

(define-command insert-register ()
  "Put what a register keeps into the text at point, asked for by its
character: text as it was, with point after it and the mark before; a block
\(\\[copy-to-register] of a rectangle) as a block of columns, as
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
  "Kill-buffer function: each place registers keep in BUFFER becomes the file,
octet and text following it that BUFFER had last, so that the register still
leads there."
  (loop for name in (register-names)
        for register = (gethash name *registers*)
        for place = (and (eq (register-kind register) :position) (register-value register))
        when (and place (eq (register-place-buffer place) buffer))
          do (let ((offset (or (anchor-in buffer (register-place-anchor place)) (register-place-offset place))))
               (setf (gethash name *registers*)
                     (make-register :position
                                    (make-register-place nil nil (register-place-path place) offset
                                                         (bookmark-context buffer offset) (register-place-safe place)))))))

;;; The list

(define-major-mode registers-mode marked-list-mode
  "The list of registers (\\[list-registers]): a line for each, with the
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
                                                    (if (register-place-path value)
                                                        (abbreviated-path (register-place-path value))
                                                        "buffer closed"))
                           (or (and buffer (anchor-in buffer (register-place-anchor value)))
                               (register-place-offset value)))))
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
                             (substitute-command-keys "\\[copy-to-register]")
                             (substitute-command-keys "\\[point-to-register]")
                             (substitute-command-keys "\\[number-to-register]"))))
   :point point :first-line *registers-title-lines*
   :refill (lambda (list) (registers-render list :keep))))

(defun registers-key (list)
  "What the register list LIST shows, cheaply: the registers in order, each
the object it keeps; another key means the list is out of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (mapcar (lambda (name) (cons name (gethash name *registers*))) (register-names)))

(keep-shown-current 'registers-mode 'registers-key)

(define-command list-registers ()
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
                      name (register-kind-words register) (substitute-command-keys "\\[insert-register]"))))))

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
  (entry-list-keys 'registers-mode "the register list (\\[list-registers])"))
