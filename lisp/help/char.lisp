;;;; lisp/help/char.lisp --- What is at point: C-x = and describe-char.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-x = says in the echo area which character is at point and where point
;;;; is, as Emacs's what-cursor-position does. With C-u, or as M-x
;;;; describe-char, *Help* shows all that is known of the character: its code,
;;;; name and kind, its octets, the face it shows in, and in a buffer parsed
;;;; with tree-sitter the syntax node it is in and the colouring rules that
;;;; capture it.

(in-package #:tuatara.defaults)

(defparameter *general-categories*
  '((:lu . "an upper-case letter") (:ll . "a lower-case letter") (:lt . "a title-case letter")
    (:lm . "a modifier letter") (:lo . "a letter") (:mn . "a combining mark") (:mc . "a spacing mark")
    (:me . "an enclosing mark") (:nd . "a digit") (:nl . "a number written as a letter") (:no . "a number")
    (:pc . "connecting punctuation") (:pd . "a dash") (:ps . "an opening bracket or quotation")
    (:pe . "a closing bracket or quotation") (:pi . "an opening quotation mark") (:pf . "a closing quotation mark")
    (:po . "punctuation") (:sm . "a maths symbol") (:sc . "a currency symbol") (:sk . "a modifier symbol")
    (:so . "a symbol") (:zs . "a space") (:zl . "a line separator") (:zp . "a paragraph separator")
    (:cc . "a control character") (:cf . "a format character") (:cs . "a surrogate")
    (:co . "a character for private use") (:cn . "a code point not assigned"))
  "What each Unicode general category means, in words, as \\[describe-char] says it.")

(defun char-shown-as (char)
  "CHAR as \\[what-cursor-position] names it: itself; SPC for a space; ^X for a
control character; \\xNN for a raw octet."
  (let ((code (char-code char)))
    (cond ((tuatara.text:raw-byte-code-p code) (tuatara.display:raw-byte-display code))
          ((= code 32) "SPC")
          ((or (< code 32) (= code 127)) (tuatara.display:control-char-display char))
          (t (string char)))))

(defun char-codes-text (char)
  "The code of CHAR in decimal, octal and hexadecimal, as (97, #o141, #x61); for
a raw octet, that it is one, and its value."
  (if (tuatara.text:raw-byte-code-p (char-code char))
      (format nil "(a raw octet, not UTF-8, #x~a)" (svref tuatara.text:*hex-pairs* (- (char-code char) #xDC00)))
      (let ((code (char-code char)))
        (format nil "(~d, #o~o, #x~(~x~))" code code code))))

(defun cursor-position-text (buffer position)
  "What \\[what-cursor-position] says of octet POSITION in BUFFER: the character
there and its codes, point as a count of characters from 1, of how many and as
a share of them, the octet and the column; the counts shown as … while a large
text's lines are still counted."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (indexed (tuatara.text:rope-indexed-p rope))
         (char (tuatara.buffer:buffer-char-at buffer position))
         (at (and indexed (tuatara.text:byte->char rope position)))
         (chars (and indexed (tuatara.text:rope-chars rope))))
    (format nil "~@[Char: ~a ~]point=~:[…~;~:*~:d~] of ~:[…~;~:*~:d~]~@[ (~d%)~] octet=~:d column=~:d"
            (and char (format nil "~a ~a" (char-shown-as char) (char-codes-text char)))
            (and at (1+ at)) chars
            (and at (round (* 100 at) (max 1 chars)))
            position (tuatara.buffer:buffer-column buffer position))))

(define-command what-cursor-position ()
  "Say in the echo area which character is at point, with its code in decimal,
octal and hexadecimal, and where point is: its place counted in characters
from 1, of how many there are and as a share of them, its octet and its
column. With \\[universal-argument], show all that is known of the character
instead, as \\[describe-char] does."
  (if (prefix-argument)
      (describe-char)
      (message "~a" (cursor-position-text (current-buffer) (point)))))

(defun captures-at (query tree rope position faces)
  "The captures of QUERY, a language's colouring rules, of the nodes of TREE (the
tree of ROPE) that hold octet POSITION: each (NAME FACE), FACE from FACES by
capture number, innermost first, and of two of the same node the one of the
later rule first, as colouring chooses; the rules' own names (_, and those
beginning with it) left out."
  (let ((names (tuatara.syntax:query-captures query)) (found '()))
    (tuatara.syntax:map-query-matches
     (lambda (pattern captures count)
       (dotimes (i count)
         (let ((capture (aref captures (* 4 i)))
               (start (aref captures (+ 1 (* 4 i))))
               (end (aref captures (+ 2 (* 4 i)))))
           (when (and (<= start position) (< position end)
                      (not (char= #\_ (char (aref names capture) 0))))
             (push (list (aref names capture) (aref faces capture) (- end start) pattern) found)))))
     query tree rope position (1+ position))
    (mapcar (lambda (capture) (list (first capture) (second capture)))
            (remove-duplicates
             (sort found (lambda (a b) (or (< (third a) (third b))
                                           (and (= (third a) (third b)) (> (fourth a) (fourth b))))))
             :key #'first :test #'string= :from-end t))))

(defun face-shown-at (buffer position)
  "The face the text at octet POSITION of BUFFER shows in as the selected window
paints it, the colours of its mode and language; NIL when that window does
not show it."
  (let* ((window (current-window))
         (rows (and (eq (tuatara.display:window-buffer window) buffer) (tuatara.display:window-rows window)))
         (face nil))
    (when (and rows (plusp (length rows)))
      (loop for (start end painted) in (mode-syntax-highlights (buffer-major-mode buffer) buffer rows)
            when (and (<= start position) (< position end))
              do (setf face painted)))
    (if (eq face :none) nil face)))

(defun node-names-text (nodes)
  "NODES, syntax nodes from the innermost out, as a line of their kinds: a named
node by its name, a token by its text in quotation marks, one with an error so
marked."
  (format nil "~{~a~^, in ~}"
          (mapcar (lambda (node)
                    (format nil (if (tuatara.syntax:tree-node-named node) "~a~@[ (with an error)~]" "\"~a\"~@[ (with an error)~]")
                            (tuatara.syntax:tree-node-name node) (tuatara.syntax:tree-node-broken node)))
                  nodes)))

(defun describe-char-text (buffer position)
  "The text \\[describe-char] shows for the character at octet POSITION of BUFFER."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (char (tuatara.buffer:buffer-char-at buffer position))
         (indexed (tuatara.text:rope-indexed-p rope))
         (end (if char (tuatara.buffer:buffer-forward-char buffer position) position))
         (mode (buffer-major-mode buffer))
         (language (mode-language mode))
         (syntax (and language (ready-syntax buffer)))
         (place (format nil "point ~:[…~;~:*~:d~] of ~:[…~;~:*~:d~], octet ~:d, line ~:[…~;~:*~:d~], column ~:d"
                        (and indexed (1+ (tuatara.text:byte->char rope position)))
                        (and indexed (tuatara.text:rope-chars rope))
                        position
                        (and indexed (1+ (tuatara.text:byte->line rope position)))
                        (tuatara.buffer:buffer-column buffer position))))
    (with-output-to-string (out)
      (cond ((null char)
             (format out "Point is at the end of the buffer.~%~%  position     ~a~%" place))
            ((tuatara.text:raw-byte-code-p (char-code char))
             (format out "~a at point is an octet that is not UTF-8, #x~a.~%~%  position     ~a~%"
                     (char-shown-as char) (svref tuatara.text:*hex-pairs* (- (char-code char) #xDC00)) place))
            (t
             (let ((code (char-code char)))
               (format out "~a at point: U+~4,'0x~@[, ~a~].~%~%" (char-shown-as char) code
                       (let ((name (unicode-name char))) (and (plusp (length name)) name)))
               (format out "  position     ~a~%" place)
               (format out "  code         ~d, #o~o, #x~(~x~)~%" code code code)
               (let ((category (sb-unicode:general-category char)))
                 (format out "  kind         ~a (~a)~%"
                         (or (cdr (assoc category *general-categories*)) "a character")
                         (string-capitalize (symbol-name category))))
               (format out "  octets       #x~a~%"
                       (tuatara.text:octets-hex (tuatara.text:rope-octets rope :start position :end end) :separator " #x"))
               (let ((columns (tuatara.display:char-columns char)))
                 (format out "  width        ~d column~:p~%" columns)))))
      (let ((face (face-shown-at buffer position)))
        (when face (format out "  face         ~(~s~)~%" face)))
      (format out "  mode         ~(~a~)~@[, the language ~(~a~)~]~%" (type-of mode) (and language (language-name language)))
      (when (and syntax char)
        (let ((nodes (tuatara.syntax:node-ancestry syntax position end)))
          (when nodes
            (format out "  syntax node  ~a~%" (node-names-text nodes))))
        (let ((query (language-compiled-query language :colouring)))
          (when query
            (let ((captures (captures-at query (tuatara.syntax:syntax-tree syntax) (tuatara.syntax:syntax-rope syntax)
                                         position (multiple-value-call #'query-faces query (language-colouring language mode))))
                  (shown nil))
              (loop for (name face) in captures
                    for first = t then nil
                    do (format out "  ~:[             ~;rules        ~]@~a~@[  ~(~s~)~]~:[~;, the colour shown~]~%"
                               first name (and face (not (eq face :none)) face)
                               (and face (not shown) (setf shown t)))))))))))

(define-command describe-char ()
  "Show in *Help* all that is known of the character at point: where it is, its
code in decimal, octal and hexadecimal, its Unicode name and kind, its octets
and the columns it takes, the face it shows in and the major mode; and in a
buffer parsed with tree-sitter the syntax node it is in, with the nodes
around it, and the colouring rules that capture it, innermost first, the one
whose colour shows marked."
  (show-help (describe-char-text (current-buffer) (point))))

(defun install-char-help ()
  "Bind \\[what-cursor-position] to \\`C-x ='."
  (define-key *global-keymap* "C-x =" 'what-cursor-position))
