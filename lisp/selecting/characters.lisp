;;;; lisp/selecting/characters.lisp --- Characters by their Unicode names: C-x 8 RET.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defun unicode-name (char)
  "CHAR's Unicode name, as the standard writes it (LATIN SMALL LETTER E WITH
ACUTE), or an empty string for one that has none: a control character, or a
code with no character given it yet."
  (let ((name (char-name char)))
    (if (and name (> (length name) 1)
             (not (eq (sb-unicode:general-category char) :cc))
             (not (cl-ppcre:scan "^U[0-9A-F]+$" name)))
        (substitute #\Space #\_ (string-upcase name))
        "")))

(defvar *character-names* nil
  "Every character that has a Unicode name, as completion candidates in order
of code, once listed (CHARACTER-NAMES); NIL before.")

(defvar *character-names-thread* nil
  "The thread listing the characters' names, running or ended, or NIL.")

(defun character-shown (char)
  "CHAR as a list of characters shows it: a mark that combines with the
character before it on a dotted circle, as Unicode's charts show one."
  (if (member (sb-unicode:general-category char) '(:mn :me))
      (format nil "~c~c" (code-char #x25CC) char)
      (string char)))

(defun list-character-names ()
  "Every character that has a Unicode name, as completion candidates in order
of code: each matched by its name, shown as the character, its name and its
code (U+00E9), its value the code. Control characters are left out."
  (loop for code below char-code-limit
        for char = (code-char code)
        for spaced = (if char (unicode-name char) "")
        when (plusp (length spaced))
        collect (make-candidate spaced :value code
                                :columns (list (list (character-shown char) nil 3)
                                               spaced
                                               (list (format nil "U+~4,'0X" code) :completion-annotation)))))

(defun character-names ()
  "The characters' names as completion candidates (*CHARACTER-NAMES*), listed
on a thread of their own the first time, the editor reading keys meanwhile:
\\`C-g' gives up the command, the listing going on, for next time. A listing
that failed is started again; NIL while there are still no names."
  (or *character-names*
      (let ((thread (if (and *character-names-thread* (sb-thread:thread-alive-p *character-names-thread*))
                        *character-names-thread*
                        (setf *character-names-thread*
                              (tuatara.runtime:make-guarded-thread
                               (lambda () (setf *character-names* (list-character-names)))
                               :name "tuatara character names")))))
        (unless (wait-interruptibly thread :message "Listing the characters' names...")
          (keyboard-quit-signal))
        *character-names*)))

(defun character-code-from (input)
  "The character code INPUT gives: a Unicode name in any case, its words apart by spaces or _; hex digits (e9,
1F600), U+ and hex digits, #x and hex, #o and octal, #10r and decimal. NIL
when it gives none, or a code past Unicode's."
  (let* ((text (string-trim " " input))
         (code (or (let ((char (name-char (substitute #\_ #\Space text))))
                     (and char (> (length text) 1) (char-code char)))
                   (multiple-value-bind (match groups)
                       (cl-ppcre:scan-to-strings
                        "^(?i)(?:(?:U\\+|#x)([0-9a-f]+)|([0-9a-f]+)|#o([0-7]+)|#10r([0-9]+))$" text)
                     (when match
                       (cond ((aref groups 0) (parse-integer (aref groups 0) :radix 16))
                             ((aref groups 1) (parse-integer (aref groups 1) :radix 16))
                             ((aref groups 2) (parse-integer (aref groups 2) :radix 8))
                             (t (parse-integer (aref groups 3)))))))))
    (and code (< code char-code-limit) (code-char code) code)))

(define-command insert-char ()
  "Insert a character by its Unicode name or its code. Every named character
is listed as you type, shown with its code; e9, U+00E9 or #xe9 for its code in
hex, #o351 in octal. With a count, that many."
  (let ((b (current-buffer)))
    (check-writable b)
    (let* ((names (or (character-names) (refuse "No character names could be listed")))
           (chosen (completing-read "Insert character (Unicode name or hex): " names
                                    :reselect t
                                    :select (lambda (shown input)
                                              (or (position (string-trim " " input) shown
                                                            :key #'candidate-string :test #'string-equal)
                                                  0))))
           (code (if (integerp chosen) chosen (character-code-from chosen))))
      (if code
          (goto (tuatara.buffer:buffer-insert b (point)
                                              (make-string (max 1 (prefix)) :initial-element (code-char code))))
          (message "No character is called ~a" chosen)))))

(defun install-characters (editor)
  "Bind \\[insert-char]."
  (define-key (editor-global-keymap editor) "C-x 8 RET" 'insert-char))
