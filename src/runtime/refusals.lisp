;;;; src/runtime/refusals.lisp --- Refusals and failures told in words, on one line.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.runtime)

(define-condition refusal (simple-error) ()
  (:documentation "The editor saying no to what was asked, in words of its own (REFUSE): told in
full even while a secret is at hand, so its words never hold text of a buffer."))

(defun refuse (control &rest arguments)
  "Say no to what was asked, in the words FORMAT makes of CONTROL and
ARGUMENTS, as an error (a REFUSAL): the command stops and the words are told
as they are, a secret at hand or not. ARGUMENTS may name a buffer, a file or
a key, never hold a buffer's text."
  (error 'refusal :format-control control :format-arguments arguments))

(declaim (inline control-code-p))
(defun control-code-p (code)
  "True when CODE is a control character's: 0 to 31, 127 to 159."
  (or (< code 32) (<= 127 code 159)))

(defun one-line (text)
  "TEXT on one line: each run of line ends, tabs and other control
characters in it one space, and the spaces at its ends left out, so that it
can be shown or written where a line is expected (a name, a value, a reason
given by a program). NIL for NIL."
  (and text
       (let ((out (make-string-output-stream)) (blank nil))
         (loop for c across text
               do (if (control-code-p (char-code c))
                      (setf blank t)
                      (progn (when blank (write-char #\Space out) (setf blank nil))
                             (write-char c out))))
         (string-trim " " (get-output-stream-string out)))))

(defun failure-words (condition &key said)
  "CONDITION, an error caught, in words, for the debug log or, with SAID true,
for a message saying what failed. It is what the condition itself says (\"No
such file\"). While a secret is at hand (*SECRET-AT-HAND* true, as the editor
makes it around work on a buffer kept secret), it is only its kind, \"a
file-error\", since what it says could hold the secret. With SAID, a REFUSAL
keeps its words even then: they are the editor's own. Say a failure through
it rather than writing the condition into a message yourself, and nothing
secret is ever shown."
  (if (and *secret-at-hand* (not (and said (typep condition 'refusal))))
      (format nil "a ~(~a~)" (type-of condition))
      (condition-words condition)))
