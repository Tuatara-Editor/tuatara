;;;; lisp/modes/lisp-complete.lisp --- Completing Lisp's names: C-M-i in a Lisp buffer, TAB at M-:.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The names Lisp typed here can use, completed from a list beside the cursor
;;;; as Emacs completes Elisp's (completion-at-point, shown as Corfu shows it):
;;;; after an open bracket or #' the operators; elsewhere the variables,
;;;; operators and classes; after a colon the keywords; after a package's name
;;;; and a colon that package's names. Beside each name what it is, and beside
;;;; the list the chosen one's parameters and documentation.

(in-package #:tuatara.defaults)

(defun lisp-completion-context (text start)
  "What a name at index START of TEXT, Lisp text, stands for: :OPERATOR right
after an open bracket that opens no quoted list or vector, or after #'; else
:ANY."
  (let ((before (and (plusp start) (char text (1- start))))
        (two-before (and (> start 1) (char text (- start 2)))))
    (cond ((and (eql before #\') (eql two-before #\#)) :operator)
          ((and (eql before #\() (not (member two-before '(#\' #\` #\#)))) :operator)
          (t :any))))

(defun lisp-name-text (symbol)
  "SYMBOL's name as Lisp text writes it: in lower case, or between bars when
its name holds lower-case letters or characters a name cannot."
  (let ((name (symbol-name symbol)))
    (if (and (notany #'lower-case-p name) (every #'lisp-name-part-p name) (plusp (length name)))
        (string-downcase name)
        (format nil "|~a|" name))))

(defun lisp-name-wanted-p (symbol context)
  "True when SYMBOL is worth offering where a name of CONTEXT (see
LISP-COMPLETION-CONTEXT) goes: an operator after an open bracket; elsewhere
anything that names a function, a variable or a class."
  (if (eq context :operator)
      (fboundp symbol)
      (or (fboundp symbol) (boundp symbol) (find-class symbol nil))))

(defun lisp-name-candidates (word package context)
  "The completion candidates for WORD, the start of a Lisp name typed where a
name of CONTEXT goes, read in PACKAGE: after a leading colon the keywords;
after a package's name and one colon its external names, two colons all its
own, each with that prefix; otherwise the names PACKAGE sees (LISP-NAME-WANTED-P).
Each candidate's value is its symbol."
  (let ((colon (position #\: word)) (out '()))
    (cond ((and colon (zerop colon))
           (do-symbols (s :keyword)
             (push (make-candidate (concatenate 'string ":" (lisp-name-text s)) :value s) out)))
          (colon
           (let* ((double (and (< (1+ colon) (length word)) (char= (char word (1+ colon)) #\:)))
                  (prefix (subseq word 0 (+ colon (if double 2 1))))
                  (found (find-package (string-upcase (subseq word 0 colon)))))
             (when found
               (flet ((add (s)
                        (when (or (eq context :any) (fboundp s))
                          (push (make-candidate (concatenate 'string prefix (lisp-name-text s)) :value s) out))))
                 (if double
                     (do-symbols (s found) (when (eq (symbol-package s) found) (add s)))
                     (do-external-symbols (s found) (add s)))))))
          (t
           (let ((seen (make-hash-table :test 'eq)))
             (do-symbols (s package)
               (unless (gethash s seen)
                 (setf (gethash s seen) t)
                 (when (lisp-name-wanted-p s context)
                   (push (make-candidate (lisp-name-text s) :value s) out)))))))
    out))

(defun lisp-name-kind (symbol)
  "What SYMBOL names, in a word or two: a keyword, a command, a special
operator, a macro, a generic function, a function, a constant, a variable or a
class; an empty string for none of them."
  (cond ((keywordp symbol) "keyword")
        ((fboundp symbol) (function-kind symbol))
        ((and (boundp symbol) (constantp symbol)) "constant")
        ((boundp symbol) "variable")
        ((find-class symbol nil) "class")
        (t "")))

(defun value-preview (value)
  "VALUE printed readably and short: a string's first 60 characters, other
things cut to a line of 60."
  (let ((*print-length* 8) (*print-level* 3) (*print-circle* t))
    (tuatara.display:shorten-string (printed-value (if (stringp value) (tuatara.display:shorten-string value 60) value))
                                    60 :one-line t)))

(defun lisp-name-documentation (symbol)
  "What the box beside the list of completions shows of SYMBOL: an operator's
name and parameters, a variable's name and value, a class's name, then its
documentation."
  (flet ((doc (kind)
           (let ((doc (documentation symbol kind)))
             ;; the keys it names written in, for the box to colour
             (if doc (substitute-command-keys doc :marks t) "Not documented."))))
    (cond ((keywordp symbol) (format nil ":~a" (lisp-name-text symbol)))
          ((fboundp symbol)
           (format nil "~a~2%~a" (or (lisp-signature symbol) (lisp-name-text symbol)) (doc 'function)))
          ((boundp symbol)
           (format nil "~a = ~a~2%~a" (lisp-name-text symbol) (value-preview (symbol-value symbol)) (doc 'variable)))
          ((find-class symbol nil)
           (format nil "~a, a class~2%~a" (lisp-name-text symbol) (doc 'type)))
          (t (lisp-name-text symbol)))))

(defun lisp-candidate-kind (candidate)
  "What CANDIDATE's symbol names (LISP-NAME-KIND), said beside it in the list."
  (lisp-name-kind (candidate-value candidate)))

(defun lisp-candidate-documentation (candidate)
  "CANDIDATE's symbol's parameters and documentation (LISP-NAME-DOCUMENTATION)."
  (lisp-name-documentation (candidate-value candidate)))

(defun lisp-completion (text point package)
  "The completion of the Lisp name around index POINT of TEXT, read in
PACKAGE, as three values: the start and end of the name in TEXT and the
candidates for it; NIL where no name is being typed and none could begin (after
a blank), though right after an open bracket every operator is offered."
  (multiple-value-bind (start end) (text-word-bounds text point #'lisp-name-part-p)
    (let ((context (lisp-completion-context text start)))
      (unless (and (= start end) (not (eq context :operator)))
        (values start end (lisp-name-candidates (subseq text start point) package context))))))

(defmethod mode-completion-at-point ((mode lisp-mode) buffer position)
  "The Lisp names that complete the one at POSITION of BUFFER, read in its
package."
  (let* ((line-start (tuatara.buffer:buffer-line-start buffer position))
         (line-end (tuatara.buffer:buffer-line-end buffer position))
         ;; the name at point is on its line, and never long
         (from (max line-start (tuatara.buffer:buffer-forward-char buffer position (- *popup-reach*))))
         (to (min line-end (tuatara.buffer:buffer-forward-char buffer position *popup-reach*)))
         (text (tuatara.buffer:buffer-string buffer :start from :end to))
         (point (length (tuatara.buffer:buffer-string buffer :start from :end position))))
    (multiple-value-bind (start end candidates) (lisp-completion text point (lisp-package buffer))
      (when start
        (flet ((offset (index) (+ from (tuatara.text:string-octet-length text :end index))))
          (values (offset start) (offset end) candidates
                  #'lisp-candidate-kind #'lisp-candidate-documentation #'lisp-name-part-p))))))

(defun lisp-complete-in-minibuffer (package)
  "Complete the Lisp name at the minibuffer's cursor, read in PACKAGE, from a
list over the prompt (COMPLETE-IN-TEXT): \\<minibuffer>\\[minibuffer-complete] and
\\`C-M-i' at \\[eval-expression]."
  (complete-in-minibuffer (lambda (text point) (lisp-completion text point package))
                          :annotate #'lisp-candidate-kind :document #'lisp-candidate-documentation
                          :constituent #'lisp-name-part-p))
