;;;; src/text/escapes.lisp --- A terminal's escape sequences in a text: where they end, what SGR sets.
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

(defun sgr-parameters (text from end)
  "The parameters of a control sequence setting graphics (SGR, final m)
written in TEXT from FROM below END (as ESCAPE-SEQUENCE-END gives them), as a
list: each a number, or for one written in parts with colons (4:3, 38:2::R:G:B)
a list of its parts' numbers. An empty one is 0, and none is taken above
65,535."
  (let ((parameters '()) (parts '()) (value 0))
    (loop for i from from to end
          for char = (if (< i end) (char text i) #\;)
          do (case char
               (#\: (push value parts) (setf value 0))
               (#\; (push (if parts (nreverse (cons value parts)) value) parameters)
                (setf parts '() value 0))
               (t (let ((digit (digit-char-p char)))
                    (when digit (setf value (min 65535 (+ (* value 10) digit))))))))
    (nreverse parameters)))

(defun sgr-colour (parameters)
  "The colour the SGR PARAMETERS (a list, from just after 38 or 48) begin
with, as three values: :PALETTE and an index of the 256-colour palette for
5;N; :RGB and the number #xRRGGBB for 2;R;G;B; NIL and NIL, the default
colour, for any other; then the parameters after it."
  (flet ((part (p) (min 255 (or p 0))))
    (case (first parameters)
      (5 (values :palette (part (second parameters)) (cddr parameters)))
      (2 (values :rgb
                 (logior (ash (part (second parameters)) 16) (ash (part (third parameters)) 8)
                         (part (fourth parameters)))
                 (nthcdr 4 parameters)))
      (t (values nil nil (rest parameters))))))

(defun sgr-changes (function text from end)
  "Call FUNCTION with each change made by the control sequence setting graphics
\(SGR, final m) written in TEXT from FROM below END (as ESCAPE-SEQUENCE-END
gives them), in order, as two arguments, what changes and its new value:
:RESET and NIL for all back to the defaults; :BOLD, :ITALIC or :UNDERLINE and
true or NIL; :FOREGROUND or :BACKGROUND and NIL for the default colour, else
an index of the 256-colour palette (the sixteen colours as 0 to 15); or
:FOREGROUND-RGB or :BACKGROUND-RGB and the colour as the number #xRRGGBB. A
colour may be written with semicolons (38;5;N, 38;2;R;G;B) or colons (38:5:N,
38:2::R:G:B), an underline's style with colons (4:3 curly, 4:0 none). Other
parameters are passed over."
  (flet ((colour (code kind value)
           (funcall function
                    (if (eq kind :rgb)
                        (if (= code 38) :foreground-rgb :background-rgb)
                        (if (= code 38) :foreground :background))
                    value)))
    (let ((parameters (sgr-parameters text from end)))
      (loop while parameters
            do (let ((code (pop parameters)))
                 (if (consp code)
                     (case (first code)
                       (4 (funcall function :underline (not (eql 0 (second code)))))
                       ((38 48)
                        ;; 38:2::R:G:B holds a colour space after the 2, left out
                        (let ((parts (rest code)))
                          (multiple-value-bind (kind value)
                              (sgr-colour (if (and (eql (first parts) 2) (= (length parts) 5))
                                              (list* 2 (cddr parts))
                                              parts))
                            (colour (first code) kind value)))))
                     (case code
                       (0 (funcall function :reset nil))
                       (1 (funcall function :bold t))
                       (3 (funcall function :italic t))
                       (4 (funcall function :underline t))
                       ((21 22) (funcall function :bold nil))
                       (23 (funcall function :italic nil))
                       (24 (funcall function :underline nil))
                       (39 (funcall function :foreground nil))
                       (49 (funcall function :background nil))
                       ((38 48)
                        (multiple-value-bind (kind value rest) (sgr-colour parameters)
                          (setf parameters rest)
                          (colour code kind value)))
                       (t (cond ((<= 30 code 37) (funcall function :foreground (- code 30)))
                                ((<= 40 code 47) (funcall function :background (- code 40)))
                                ((<= 90 code 97) (funcall function :foreground (+ 8 (- code 90))))
                                ((<= 100 code 107) (funcall function :background (+ 8 (- code 100)))))))))))))
