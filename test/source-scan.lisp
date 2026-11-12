;;;; test/source-scan.lisp --- Reading the source text: top-level forms, symbol tokens and the package repositories.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The tests of where dangerous things happen (chokepoints.lisp), of code
;;;; nothing calls (dead-code.lisp) and of sizes (sizes.lisp) read the source
;;;; as text, in the one way these functions give.

(in-package #:tuatara.tests)

(defun skip-form (text start)
  "The index just after the form that begins with the open parenthesis at START in TEXT, or the end of TEXT when
it is not closed. Strings, comments, character names and symbols in bars hide their parentheses."
  (declare (type simple-string text) (type fixnum start) (optimize speed))
  (let ((depth 0) (i start) (end (length text)))
    (declare (type fixnum depth i end))
    (loop while (< i end)
          do (let ((c (schar text i)))
               (case c
                 (#\( (incf depth))
                 (#\) (decf depth) (when (zerop depth) (return-from skip-form (1+ i))))
                 (#\; (setf i (or (position #\Newline text :start i) end)))
                 ((#\" #\|) (loop do (incf i)
                                  while (and (< i end) (char/= (schar text i) c))
                                  do (when (char= (schar text i) #\\) (incf i))))
                 (#\# (cond ((and (< (1+ i) end) (char= (schar text (1+ i)) #\\)) (incf i 2))
                            ((and (< (1+ i) end) (char= (schar text (1+ i)) #\|))
                             (setf i (or (search "|#" text :start2 i) end)))))))
             (incf i))
    end))

(defun top-level-forms (text)
  "A list of (START END) for each form of TEXT that is not inside another form, wherever on its line it begins (a
form after #+ or #- included). Comments, strings and reader conditionals between the forms are passed over."
  (let ((forms '()) (i 0) (text (coerce text 'simple-string)))
    (let ((end (length text)))
      (loop
        (loop while (and (< i end) (member (schar text i) '(#\Space #\Tab #\Newline #\Return))) do (incf i))
        (when (>= i end) (return))
        (let ((c (schar text i)))
          (cond ((char= c #\()
                 (let ((after (skip-form text i)))
                   (push (list i after) forms)
                   (setf i after)))
                ((char= c #\;) (setf i (or (position #\Newline text :start i) end)))
                ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\|))
                 (setf i (+ 2 (or (search "|#" text :start2 i) (- end 2)))))
                (t (incf i)
                   (loop while (and (< i end) (not (member (schar text i) '(#\Space #\Tab #\Newline #\Return #\())))
                         do (incf i)))))))
    (nreverse forms)))

(defun text-tokens (text)
  "The symbols and numbers written in TEXT as downcased strings, with a package prefix still on: what is in
comments and strings is left out."
  (declare (type simple-string text) (optimize speed))
  (let ((tokens '()) (i 0) (end (length text)))
    (declare (type fixnum i end))
    (flet ((break-p (c) (or (member c '(#\Space #\Tab #\Newline #\Return #\( #\) #\' #\` #\, #\" #\;)))))
      (loop while (< i end)
            do (let ((c (schar text i)))
                 (cond ((char= c #\;) (setf i (or (position #\Newline text :start i) end)))
                       ((char= c #\")
                        (loop do (incf i)
                              while (and (< i end) (char/= (schar text i) #\"))
                              do (when (char= (schar text i) #\\) (incf i)))
                        (incf i))
                       ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\\)) (incf i 3)
                        (loop while (and (< i end) (alphanumericp (schar text i))) do (incf i)))
                       ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\|))
                        (setf i (+ 2 (or (search "|#" text :start2 i) (- end 2)))))
                       ((break-p c) (incf i))
                       (t (let ((from i))
                            (loop while (and (< i end) (not (break-p (schar text i)))) do (incf i))
                            (push (string-downcase (subseq text from i)) tokens)))))))
    (nreverse tokens)))

(defun text-strings (text)
  "The string literals written in TEXT, each as the text between its quotes, as written; comments are left out."
  (declare (type simple-string text))
  (let ((strings '()) (i 0) (end (length text)))
    (loop while (< i end)
          do (let ((c (schar text i)))
               (cond ((char= c #\;) (setf i (or (position #\Newline text :start i) end)))
                     ((char= c #\")
                      (let ((from (1+ i)))
                        (loop do (incf i)
                              while (and (< i end) (char/= (schar text i) #\"))
                              do (when (char= (schar text i) #\\) (incf i)))
                        (push (subseq text from (min i end)) strings)))
                     ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\\)) (incf i 2))
                     ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\|))
                      (setf i (+ 1 (or (search "|#" text :start2 i) (- end 2))))))
               (incf i)))
    (nreverse strings)))

(defun token-base (token)
  "TOKEN as the name it gives, without package prefix (any number of colons) or #: mark; a keyword keeps its colon.
NIL when TOKEN is not a string."
  (when (stringp token) (token-base-of token)))

(defun token-base-of (token)
  "The name TOKEN, a string, gives (TOKEN-BASE)."
  (let ((token (if (and (> (length token) 1) (string= "#:" token :end2 2)) (subseq token 2) token)))
    (if (and (plusp (length token)) (char= (char token 0) #\:))
        token
        (subseq token (1+ (or (position #\: token :from-end t) -1))))))

(defun token-name (token)
  "TOKEN without its package prefix and its #: mark, or NIL for a keyword."
  (let ((base (token-base token)))
    (and base (plusp (length base)) (char/= (char base 0) #\:) base)))

(defvar *packages-directory-candidates* '("../tuatara-packages/" "../../../../tuatara-packages/")
  "Where PACKAGES-DIRECTORY looks, relative to this tree: beside it, and four directories up for a worktree.")

(defun sibling-packages-directory (&optional (candidates *packages-directory-candidates*))
  "The directory with the package repositories as a full name with no \"..\" in it, looked for beside the editor's
directory and, for a worktree, four directories up; NIL when it is in neither."
  (let ((found (find-if #'uiop:directory-exists-p
                        (mapcar (lambda (candidate) (merge-pathnames candidate (asdf:system-source-directory :tuatara)))
                                candidates))))
    (and found (truename found))))

(defun package-repositories ()
  "The directories of the package repositories (PACKAGES-DIRECTORY), each a git repository of its own, the worktrees of
their branches left out; none when there is no such directory."
  (let ((root (sibling-packages-directory)))
    (and root (sort (remove-if-not (lambda (directory) (uiop:directory-exists-p (merge-pathnames ".git/" directory)))
                                   (uiop:subdirectories root))
                    #'string< :key #'namestring))))

(defun package-source-files ()
  "Every Lisp file of the package repositories."
  (loop for directory in (package-repositories)
        append (directory (merge-pathnames "**/*.lisp" directory))))

(defun tree-relative-name (file)
  "FILE as a name relative to this tree, such as \"lisp/theme.lisp\"; a file outside it by its full name."
  (namestring (uiop:enough-pathname file (asdf:system-source-directory :tuatara))))

(defun file-line-number (text index)
  "The line (counted from 1) of the character at INDEX in TEXT."
  (1+ (count #\Newline text :end index)))

(defun form-head (text start)
  "The operator and the name of the form that begins at START in TEXT, as downcased strings (the name of a
\(setf name) as that whole), or NIL when the form has no name after its operator."
  (multiple-value-bind (from to starts ends)
      (cl-ppcre:scan "^\\(([^\\s()]+)\\s+(\\(setf\\s+[^\\s()]+\\)|[^\\s()]+)" text :start start)
    (declare (ignore from))
    (when to
      (values (string-downcase (subseq text (aref starts 0) (aref ends 0)))
              (string-downcase (subseq text (aref starts 1) (aref ends 1)))))))

(defun read-tree (text start end)
  "The forms written in TEXT from START to END as a tree: a list for a list, a string for a symbol or number
\(downcased, a package prefix still on), \"quote\" and \"function\" lists for 'x and #'x. Strings, characters and
comments are left out."
  (declare (type simple-string text) (type fixnum start end))
  (let ((i start))
    (labels ((blank-p (c) (member c '(#\Space #\Tab #\Newline #\Return)))
             (stop-p (c) (or (blank-p c) (member c '(#\( #\) #\' #\` #\, #\" #\;))))
             (skip-blanks ()
               (loop while (< i end)
                     do (let ((c (schar text i)))
                          (cond ((blank-p c) (incf i))
                                ((char= c #\;) (setf i (or (position #\Newline text :start i :end end) end)))
                                ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\|))
                                 (setf i (+ 2 (or (search "|#" text :start2 i :end2 end) (- end 2)))))
                                (t (return))))))
             (read-one ()
               (skip-blanks)
               (when (>= i end) (return-from read-one :end))
               (let ((c (schar text i)))
                 (cond ((char= c #\() (incf i) (read-list))
                       ((char= c #\)) (incf i) :close)
                       ((char= c #\')
                        (incf i) (let ((x (read-one))) (list "quote" x)))
                       ((or (char= c #\`) (char= c #\,))
                        (incf i) (when (and (< i end) (char= (schar text i) #\@)) (incf i)) (read-one))
                       ((char= c #\")
                        (loop do (incf i)
                              while (and (< i end) (char/= (schar text i) #\"))
                              do (when (char= (schar text i) #\\) (incf i)))
                        (incf i) (read-one))
                       ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\\))
                        (incf i 3)
                        (loop while (and (< i end) (alphanumericp (schar text i))) do (incf i))
                        (read-one))
                       ((and (char= c #\#) (< (1+ i) end) (char= (schar text (1+ i)) #\'))
                        (incf i 2) (let ((x (read-one))) (list "function" x)))
                       (t (let ((from i))
                            (incf i)
                            (loop while (and (< i end) (not (stop-p (schar text i)))) do (incf i))
                            (string-downcase (subseq text from i)))))))
             (read-list ()
               (let ((items '()))
                 (loop (let ((x (read-one)))
                         (cond ((member x '(:end :close)) (return))
                               (t (push x items)))))
                 (nreverse items))))
      (let ((forms '()))
        (loop (let ((x (read-one)))
                (when (eq x :end) (return))
                (unless (eq x :close) (push x forms))))
        (nreverse forms)))))
