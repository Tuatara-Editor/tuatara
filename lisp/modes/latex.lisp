;;;; lisp/modes/latex.lisp --- LaTeX environments: C-c C-e opens one, C-c ] closes one.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Both commands read the text as it stands, with no syntax tree: an environment
;;;; is a \begin{NAME} not yet matched by an \end{NAME}, % starts a comment, and
;;;; \% is a percent sign. The scan for an open one runs back from point in
;;;; chunks that double in size, so a nearby one costs a few kilobytes and a file
;;;; with none is read up to *TEXT-SCAN-LIMIT* and no further.

(in-package #:tuatara.defaults)

(defparameter *latex-common-environments*
  '("itemize" "enumerate" "description" "figure" "table" "tabular" "equation" "align" "center" "quote" "verbatim"
    "minipage" "abstract")
  "The environments offered by LATEX-INSERT-ENVIRONMENT after those the
buffer already uses.")

(defparameter *latex-environment-reach* (* 256 1024)
  "The most octets either side of point looked through for the names of the
environments a buffer uses.")

(defvar *latex-last-environment* nil
  "The name of the environment last given to LATEX-INSERT-ENVIRONMENT, or NIL.")

(defparameter *latex-environment-scanner*
  (cl-ppcre:create-scanner "\\\\[\\\\%]|%[^\\n]*|\\\\(begin|end)\\{([^{}\\n]*)\\}")
  "Matches what the scan for environments reads: an escaped backslash or
percent sign, a comment, or \\begin{NAME} and \\end{NAME} (the word and the
name as groups).")

(defparameter *latex-skipped-environments* '("verbatim" "verbatim*" "lstlisting" "minted" "comment" "Verbatim")
  "The environments of a LaTeX file whose text is never read for formulas or for
environments.")

(defun latex-environment-marks (buffer start end)
  "The \\begin and \\end commands of BUFFER between octets START and END, outside
comments and the text of verbatim environments (*LATEX-SKIPPED-ENVIRONMENTS*),
as a list of (BEGINNING OCTET NAME) in text order: BEGINNING true for \\begin,
OCTET where the command starts. START is the start of a line. A verbatim
environment that ends before END is left out whole; one that does not end
there is given as open, and what follows it is not read."
  (let ((text (tuatara.buffer:buffer-string buffer :start start :end end))
        (marks '()) (at 0) (octet start) (counted 0))
    (loop
      (multiple-value-bind (match-start match-end starts ends)
          (cl-ppcre:scan *latex-environment-scanner* text :start at)
        (unless match-start (return))
        (setf at (max match-end (1+ match-start)))
        (when (aref starts 0)
          (let* ((beginning (char= #\b (char text (aref starts 0))))
                 (name (subseq text (aref starts 1) (aref ends 1)))
                 (verbatim (and beginning (member name *latex-skipped-environments* :test #'string=)))
                 (closing (and verbatim (search (format nil "\\end{~a}" name) text :start2 match-end))))
            (cond ((and verbatim closing)
                   (setf at (+ closing (length name) 6)))
                  (t
                   (incf octet (length (tuatara.text:string-octets (subseq text counted match-start))))
                   (setf counted match-start)
                   (push (list beginning octet name) marks)
                   (when verbatim (return))))))))
    (nreverse marks)))

(defun latex-open-environment (buffer position)
  "The environment still open at POSITION of BUFFER: the name and the octet
where the \\begin of the nearest \\begin not yet closed before POSITION starts,
as two values; NIL when each one is closed. Refused when none is found within
*TEXT-SCAN-LIMIT* octets."
  (let ((end position)
        (limit (max 0 (- position *text-scan-limit*)))
        (width (* 64 1024))
        (depth 0))
    (loop
      (let ((start (tuatara.buffer:buffer-line-start buffer (max limit (- end width)))))
        (when (< start end)
          (dolist (mark (reverse (latex-environment-marks buffer start end)))
            (destructuring-bind (beginning octet name) mark
              (cond ((not beginning) (incf depth))
                    ((zerop depth) (return-from latex-open-environment (values name octet)))
                    (t (decf depth)))))
          (setf end start))
        (cond ((zerop start) (return nil))
              ((<= start limit) (refuse "No open environment within ~a" (scan-limit-words))))
        (setf width (* 2 width))))))

(defun latex-environments-used (buffer position)
  "The names of the environments BUFFER uses near POSITION, the most used first."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (start (tuatara.buffer:buffer-line-start buffer (max 0 (- position *latex-environment-reach*))))
         (end (tuatara.buffer:buffer-line-end buffer (min size (+ position *latex-environment-reach*))))
         (counts '()))
    (loop for (beginning nil name) in (latex-environment-marks buffer start end)
          when (and beginning (plusp (length name)))
            do (incf (cdr (or (assoc name counts :test #'string=) (car (push (cons name 0) counts))))))
    (mapcar #'car (stable-sort (nreverse counts) #'> :key #'cdr))))

(defun insert-lines-alone (buffer lines)
  "Put LINES, strings, in BUFFER at point each on a line of its own and return
the octet where the first starts and the octet after the last. On a line
holding only blanks they replace the blanks; elsewhere the line is split at
point, so that no text of it shares a line with them. Point is left after them."
  (flet ((text-between-p (start end)
           (not (tuatara.text:blank-text-p (tuatara.buffer:buffer-string buffer :start start :end end)))))
    (let* ((point (tuatara.buffer:buffer-point buffer))
           (line-start (tuatara.buffer:buffer-line-start buffer point))
           (line-end (tuatara.buffer:buffer-line-end buffer point))
           (blank (not (text-between-p line-start line-end)))
           (newline (tuatara.buffer:buffer-newline buffer))
           (before (and (not blank) (text-between-p line-start point)))
           (after (and (not blank) (text-between-p point line-end)))
           (text (format nil "~{~a~}" (loop for (line . more) on lines collect line when more collect newline)))
           (from (if blank line-start point))
           (first (+ from (if before (length newline) 0)))
           (end (+ first (length (tuatara.text:string-octets text)))))
      (tuatara.buffer:buffer-edit
       buffer (vector (tuatara.text:make-edit
                       from (if blank line-end point)
                       (tuatara.text:string-octets
                        (concatenate 'string (if before newline "") text (if after newline ""))))))
      (setf (tuatara.buffer:buffer-point buffer) end)
      (values first end))))

(defun latex-environment-name (buffer)
  "Ask for the name of an environment, the one used last offered first, then
those BUFFER uses, then the usual ones; the name typed, or refused when empty
or holding braces."
  (let* ((used (latex-environments-used buffer (tuatara.buffer:buffer-point buffer)))
         (names (remove-duplicates (append (and *latex-last-environment* (list *latex-last-environment*))
                                           used *latex-common-environments*)
                                   :test #'string= :from-end t))
         (name (string-trim " " (completing-read
                                 "Environment: "
                                 (mapcar (lambda (name)
                                           (make-candidate name :annotation (and (member name used :test #'string=)
                                                                                 "used here")))
                                         names)
                                 :keep-order t :default *latex-last-environment*))))
    (when (or (zerop (length name)) (find-if (lambda (char) (find char "{}%")) name)
              (find-if (lambda (char) (member char '(#\Newline #\Return))) name))
      (refuse "Not a name for an environment"))
    (setf *latex-last-environment* name)))

(defun latex-wrap-region (buffer begin end)
  "Put the lines the selection of BUFFER touches between the lines BEGIN and
END, indent them as the mode does, end the selection and leave point at the
end of the line END."
  (multiple-value-bind (start stop) (tuatara.buffer:buffer-region buffer)
    (let* ((newline (tuatara.buffer:buffer-newline buffer))
           (from (tuatara.buffer:buffer-line-start buffer start))
           (to (tuatara.buffer:buffer-line-end buffer (range-last-place buffer start stop)))
           (end-line (+ 2 (tuatara.buffer:buffer-line-number buffer to))))
      (tuatara.buffer:buffer-edit
       buffer (vector (tuatara.text:make-edit from from
                                              (tuatara.text:string-octets (concatenate 'string begin newline)))
                      (tuatara.text:make-edit to to
                                              (tuatara.text:string-octets (concatenate 'string newline end)))))
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (let ((last (tuatara.buffer:buffer-line-end buffer (tuatara.buffer:buffer-goto-line buffer end-line))))
        (indent-lines buffer from last :blank-lines t)
        (goto (tuatara.buffer:buffer-line-end buffer (tuatara.buffer:buffer-goto-line buffer end-line)))))))

(define-command latex-insert-environment ()
  "Ask for the name of an environment and write its \\begin{NAME} and
\\end{NAME} on lines of their own, indented as the mode indents them, with point
on an empty line between. Names already used in the buffer are offered after
the last one given, then the usual ones (itemize, enumerate, figure, table,
equation, align and more); any other name can be typed. With the selection
active the lines it touches are put between them instead.
\\<latex-mode>\\[latex-insert-environment] in LaTeX buffers; the environment
still open is closed by \\[latex-close-environment]."
  (let* ((b (current-buffer))
         (name (latex-environment-name b))
         (begin (format nil "\\begin{~a}" name))
         (end (format nil "\\end{~a}" name)))
    (if (tuatara.buffer:buffer-mark-active-p b)
        (latex-wrap-region b begin end)
        (multiple-value-bind (first stop) (insert-lines-alone b (list begin "" end))
          (let ((line (tuatara.buffer:buffer-line-number b first)))
            (indent-lines b first stop :blank-lines t)
            (goto (tuatara.buffer:buffer-line-end b (tuatara.buffer:buffer-goto-line b (1+ line)))))))))

(define-command latex-close-environment ()
  "Write \\end{NAME} for the nearest \\begin{NAME} before point that no
\\end{NAME} closes yet, on a line of its own when the line has text, indented
as that \\begin is. Comments and the text of verbatim environments do not count.
It says so when every environment before point is closed. \\<latex-mode>\\[latex-close-environment] in LaTeX
buffers; \\[latex-insert-environment] opens one."
  (let ((b (current-buffer)))
    (multiple-value-bind (name octet) (latex-open-environment b (tuatara.buffer:buffer-point b))
      (unless name (refuse "Every environment before point is closed"))
      (let* ((line (tuatara.buffer:buffer-line-start b octet))
             (indent (tuatara.buffer:buffer-string b :start line :end (+ line (line-indentation b line)))))
        (insert-lines-alone b (list (format nil "~a\\end{~a}" indent name)))))))

(defun install-latex ()
  "Give LaTeX buffers their keys: \\`C-c C-e' opens an environment and \\`C-c ]'
closes the open one; their commands work there alone."
  (commands-for-mode 'latex-mode "LaTeX buffers" 'latex-insert-environment 'latex-close-environment)
  (let ((map (mode-keymap 'latex-mode)))
    (define-key map "C-c C-e" 'latex-insert-environment)
    (define-key map "C-c ]" 'latex-close-environment)))

;;; Formulas shown as pictures

(defparameter *latex-math-environments*
  '("equation" "equation*" "align" "align*" "gather" "gather*" "multline" "multline*" "flalign" "flalign*"
    "alignat" "alignat*" "eqnarray" "eqnarray*" "displaymath" "math")
  "The environments of a LaTeX file shown as pictures, as formulas
\(\\[inline-pictures-mode]).")

(defparameter *latex-paragraph-reach* 65536
  "The most octets before and after a stretch of a LaTeX file looked through
for the blank line its paragraph begins or ends at, when finding its
formulas: a formula never goes past one.")

(defun latex-preamble (buffer)
  "Where BUFFER's body begins, after \\begin{document}, and the LaTeX of its
preamble, what is between its \\documentclass line and that, as two values;
0 and NIL for a file without \\begin{document} (a part another file takes
in), or one further than *LATEX-ENVIRONMENT-REACH* from the start. Kept
while the text is unchanged."
  (let ((version (tuatara.buffer:buffer-version buffer))
        (kept (tuatara.buffer:buffer-local buffer 'latex-preamble)))
    (unless (and kept (eq (car kept) version))
      (let* ((text (tuatara.buffer:buffer-string buffer :end (min (tuatara.buffer:buffer-size buffer)
                                                                  *latex-environment-reach*)))
             (begin (cl-ppcre:scan "(?m)^[^%\\n]*?\\\\begin\\{document\\}" text))
             (body (and begin (search "\\begin{document}" text :start2 begin)))
             (class (and body (cl-ppcre:scan "(?m)^[^%\\n]*?\\\\documentclass" text :end body)))
             (after-class (if class (or (position #\Newline text :start class) body) 0)))
        (setf kept (setf (tuatara.buffer:buffer-local buffer 'latex-preamble)
                         (list version
                               (if body (tuatara.text:string-octet-length text :end (+ body 16)) 0)
                               (and body (string-trim '(#\Newline) (subseq text (min after-class body) body))))))))
    (values (second kept) (third kept))))

(defun text-at-p (text position prefix)
  "True when TEXT holds the string PREFIX at POSITION."
  (let ((end (+ position (length prefix))))
    (and (<= end (length text)) (string= prefix text :start2 position :end2 end))))

(defun latex-formula-end (text from close)
  "Where the formula of TEXT that begins at FROM ends, after its CLOSE (a
delimiter, or \\end{NAME}), its escaped characters and comments stepped
over; NIL when it does not end in TEXT."
  (loop with i = from
        while (< i (length text))
        do (let ((c (char text i)))
             (cond ((and (char= c #\\) (< (1+ i) (length text)) (find (char text (1+ i)) "\\%$"))
                    (if (and (string= close "$") (char= (char text (1+ i)) #\$))
                        (incf i 2)
                        (if (text-at-p text i close)
                            (return (+ i (length close)))
                            (incf i 2))))
                   ((char= c #\%)
                    (setf i (or (position #\Newline text :start i) (length text))))
                   ((text-at-p text i close)
                    (return (+ i (length close))))
                   (t (incf i))))))

(defparameter *latex-opener-scanner*
  (cl-ppcre:create-scanner "\\\\[\\\\%$]|%|\\$\\$?|\\\\[\\[(]|\\\\begin\\{([^}]*)\\}")
  "Matches what the search for formulas stops at: an escaped backslash, percent
sign or dollar, a comment, $ or $$, \\[ or \\(, and \\begin{NAME} (the name as
group).")

(defun latex-opener (text start end name-start name-end)
  "What the match of *LATEX-OPENER-SCANNER* in TEXT from START to END begins, as
two values: how many characters its opener takes and what closes it, as
LATEX-FORMULA-END takes it; :COMMENT or :SKIP in place of the number for a
comment, or an environment whose text is not read (*LATEX-SKIPPED-ENVIRONMENTS*);
NIL for no formula, and the place to go on from as a third value."
  (let ((name (and name-start (subseq text name-start name-end))))
    (case (char text start)
      (#\% (values :comment nil))
      (#\$ (if (= 2 (- end start)) (values 2 "$$") (values 1 "$")))
      (t (case (char text (1+ start))
           (#\[ (values 2 "\\]"))
           (#\( (values 2 "\\)"))
           (#\b (cond ((member name *latex-math-environments* :test #'string=)
                       (values (- end start) (format nil "\\end{~a}" name)))
                      ((member name *latex-skipped-environments* :test #'string=)
                       (values :skip (format nil "\\end{~a}" name)))
                      (t (values nil nil (+ start 2)))))
           (t (values nil nil end)))))))

(defun latex-formulas (buffer start end)
  "The formulas BUFFER, a LaTeX file, shows as pictures among its lines (an
INLINE-PICTURE's source a FORMULA) that begin from octet START below END, in
order, and true as a second value: in its body (after \\begin{document},
LATEX-PREAMBLE), outside comments and the environments of
*LATEX-SKIPPED-ENVIRONMENTS*, those in $, $$, \\(, \\[ and the environments of
*LATEX-MATH-ENVIRONMENTS*; one alone on its lines centred on rows of its own,
any other in the columns it stands in. Drawn with the file's preamble, or
*FORMULA-PREAMBLE* when it has none. The text is read from the paragraph
START is in to the end of that of END, a formula never going past a blank
line."
  (multiple-value-bind (body preamble) (latex-preamble buffer)
    (let* ((size (tuatara.buffer:buffer-size buffer))
           (from (max body (tuatara.buffer:buffer-line-start buffer (min start size))))
           (preamble (or preamble *formula-preamble*))
           (found '()))
      (when (< from (min end size))
        (let* ((head-start (tuatara.buffer:buffer-line-start buffer (max body (- from *latex-paragraph-reach*))))
               (head (tuatara.buffer:buffer-string buffer :start head-start :end from))
               (blank (cl-ppcre:all-matches "\\n[ \\t]*\\n" head))
               (top (if blank
                        (- from (tuatara.text:string-octet-length head :start (second (last blank 2))))
                        head-start))
               ;; read to the end of END's line, then on to the end of its paragraph
               (middle (tuatara.buffer:buffer-line-end buffer (min end size)))
               (part (tuatara.buffer:buffer-string buffer :start top :end middle))
               (tail-end (tuatara.buffer:buffer-line-end buffer (min size (+ middle *latex-paragraph-reach*))))
               (text (concatenate 'string part (tuatara.buffer:buffer-string buffer :start middle :end tail-end)))
               (stop (or (cl-ppcre:scan "\\n[ \\t]*\\n" text :start (length part)) (length text)))
               (octet top) (char 0))
          (flet ((byte-at (i)
                   ;; octets counted on from the last place asked, in order
                   (incf octet (tuatara.text:string-octet-length text :start char :end i))
                   (setf char i)
                   octet))
            (loop with i = 0
                  while (< i stop)
                  do (multiple-value-bind (match-start match-end starts ends)
                         (cl-ppcre:scan *latex-opener-scanner* text :start i :end stop)
                       (unless match-start (return))
                       (multiple-value-bind (open close go-on)
                           (latex-opener text match-start match-end
                                         (and starts (aref starts 0)) (and ends (aref ends 0)))
                         (setf i (or go-on match-end))
                         (cond ((eq open :comment)
                                (setf i (or (position #\Newline text :start match-start) (length text))))
                               ((eq open :skip)
                                (setf i (or (let ((at (search close text :start2 match-start)))
                                              (and at (+ at (length close))))
                                            (length text))))
                               (open
                                (let* ((after (latex-formula-end text (+ match-start open) close))
                                       (blank (cl-ppcre:scan "\\n[ \\t]*\\n" text :start match-start
                                                                                  :end (or after (length text)))))
                                  (if (or (null after) blank)
                                      (setf i (+ match-start open))
                                      (let* ((line (1+ (or (position #\Newline text :end match-start :from-end t) -1)))
                                             (line-end (or (position #\Newline text :start after) (length text)))
                                             (display (and (not (string= close "$")) (not (string= close "\\)"))
                                                           (tuatara.text:blank-text-p text line match-start)
                                                           (tuatara.text:blank-text-p text after line-end)))
                                             (begin (if display line match-start))
                                             (begin-octet (byte-at begin)))
                                        (when (and (<= start begin-octet) (< begin-octet end))
                                          (push (make-inline-picture
                                                 begin-octet (byte-at (if display line-end after))
                                                 (make-formula (subseq text match-start after) preamble
                                                               :inline (not display))
                                                 :align (if display :center :left))
                                                found))
                                        (setf i after))))))))))))
      (values (nreverse found) t))))

(defmethod mode-enter ((mode latex-mode) buffer)
  "Have BUFFER show its formulas as pictures while \\[inline-pictures-mode]
is on there (LATEX-FORMULAS)."
  (call-next-method)
  (show-inline-pictures buffer 'latex-formulas))

(defmethod mode-exit ((mode latex-mode) buffer)
  "Let go of the formulas BUFFER showed as pictures."
  (forget-inline-pictures buffer)
  (call-next-method))
