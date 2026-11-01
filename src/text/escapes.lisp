;;;; src/text/escapes.lisp --- Where a terminal's escape sequences end in a text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

(defun c1-control-p (char)
  "True when CHAR is one of the C1 controls, codes 128 to 159."
  (<= 128 (char-code char) 159))

(defun escape-sequence-end (text start &key c1)
  "Where the escape sequence of TEXT beginning at START ends, as four values:
the index after its last character; for a control sequence (ESC [), its final
character, else NIL; and the start and end of its parameters, the characters
between the introducer and the final one. Operating system commands (ESC ])
end at a bell or a string terminator (ESC \\), and device control strings and
the others like them (ESC P, X, ^ and _) at a string terminator, or at the end
of TEXT when it has none; any other ESC with the character after it. With C1,
the C1 controls introduce the same sequences (U+009B a control sequence,
U+009D an operating system command, U+0090, U+0098, U+009E and U+009F strings)
and end them (U+009C), and any other C1 control is a sequence of its own."
  (let* ((end (length text))
         (first (char text start))
         (escaped (char= first #\Escape))
         (kind (cond (escaped (and (< (1+ start) end) (char text (1+ start))))
                     (c1 (case (char-code first)
                           (#x9B #\[) (#x9D #\]) ((#x90 #x98 #x9E #x9F) #\P)
                           (t first))))))
    (flet ((string-end (bell)
             ;; after the string beginning at START: its terminator, else the end of TEXT
             (loop for j from (+ start (if escaped 2 1)) below end
                   for ch = (char text j)
                   when (and bell (char= ch #\Bel)) return (1+ j)
                   when (and c1 (= (char-code ch) #x9C)) return (1+ j)
                   when (and (char= ch #\Escape) (< (1+ j) end) (char= (char text (1+ j)) #\\)) return (+ j 2)
                   finally (return end))))
      (case kind
        (#\[ (let* ((from (+ start (if escaped 2 1)))
                    (stop (or (position-if (lambda (ch) (<= #x40 (char-code ch) #x7e)) text :start (min from end))
                              (1- end))))
               (values (1+ stop) (char text stop) from stop)))
        (#\] (values (string-end t) nil start start))
        ((#\P #\X #\^ #\_) (values (string-end nil) nil start start))
        (t (values (min end (+ start (if escaped 2 1))) nil start start))))))
