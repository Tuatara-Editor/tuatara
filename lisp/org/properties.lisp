;;;; lisp/org/properties.lisp --- An Org file's properties and settings, and a source block's code.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The settings written as :KEY VALUE (header arguments), the properties a
;;;; file gives by #+PROPERTY: lines and a headline by its property drawer,
;;;; inherited down the tree, and the code of a source block as it is tangled
;;;; and exported.

(in-package #:tuatara.defaults)

(defun org-keyword-arguments (text)
  "The settings TEXT gives as :KEY VALUE (:tangle ~/.bashrc :mkdirp yes), in
order, as an alist of (KEY . VALUE), KEY as written without its colon, VALUE
without the spaces around it; a key with no value has \"\"."
  (loop for part in (cl-ppcre:split "(?:^|[ \\t]+)(?=:[A-Za-z][-A-Za-z0-9_]*(?:[ \\t]|$))" (or text ""))
        for setting = (string-trim *org-blanks* part)
        when (and (plusp (length setting)) (char= (char setting 0) #\:))
          collect (let ((space (position-if #'space-or-tab-char-p setting)))
                    (cons (subseq setting 1 space) (if space (string-trim *org-blanks* (subseq setting space)) "")))))

(defun org-header-arguments (text)
  "The header arguments of TEXT (:tangle ~/.bashrc :mkdirp yes) as an alist of
(KEY . VALUE) (ORG-KEYWORD-ARGUMENTS), KEY in lower case, VALUE without the
double quotes around it."
  (loop for (key . value) in (org-keyword-arguments text)
        collect (cons (string-downcase key)
                      (if (and (>= (length value) 2)
                               (char= (char value 0) #\")
                               (char= (char value (1- (length value))) #\"))
                          (subseq value 1 (1- (length value)))
                          value))))

(defun org-property-with (value name key new)
  "VALUE, the property NAME's so far, after a property KEY of value NEW: NEW
when KEY is NAME, NEW added to VALUE after a space when KEY is NAME+, else
VALUE as it was."
  (cond ((string-equal key name) new)
        ((and (= (length key) (1+ (length name)))
              (char= (char key (length name)) #\+)
              (string-equal key name :end1 (length name)))
         (if value (format nil "~a ~a" value new) new))
        (t value)))

(defvar *org-file-properties-kept* nil
  "While the files a tree's blocks write are worked out (ORG-CODE-FILES),
\(TREE . TABLE): the values TREE's #+PROPERTY: lines give, by name, as found.")

(defun org-file-properties (tree name)
  "The value the file gives property NAME by #+PROPERTY: lines (NAME+ adding
to it), or NIL; found once for each name while *ORG-FILE-PROPERTIES-KEPT* is
of TREE."
  (let ((kept *org-file-properties-kept*))
    (if (and kept (eq (car kept) tree))
        (multiple-value-bind (value found) (gethash name (cdr kept))
          (if found value (setf (gethash name (cdr kept)) (org-file-properties-read tree name))))
        (org-file-properties-read tree name))))

(defun org-file-properties-read (tree name)
  "The value TREE's #+PROPERTY: lines give property NAME, read from them, as
ORG-FILE-PROPERTIES gives it."
  (let ((value nil))
    (labels ((walk (node)
               (when (and (eq (org-node-type node) 'keyword) (string= (org-property node :key) "PROPERTY"))
                 (let* ((text (string-trim *org-blanks* (org-property node :value)))
                        (space (position-if #'space-or-tab-char-p text))
                        (key (subseq text 0 space))
                        (rest (if space (string-trim *org-blanks* (subseq text space)) "")))
                   (setf value (org-property-with value name key rest))))
               (when (member (org-node-type node) '(org-data section))
                 (mapc #'walk (org-node-children node)))))
      (walk tree))
    value))

(defun org-headline-properties (headline)
  "The properties of HEADLINE's property drawer, as an alist of (KEY . VALUE), in order."
  (let ((section (find 'section (org-node-children headline) :key #'org-node-type)))
    (and section
         (let ((drawer (find 'property-drawer (org-node-children section) :key #'org-node-type)))
           (and drawer
                (loop for node in (org-node-children drawer)
                      when (eq (org-node-type node) 'node-property)
                        collect (cons (org-property node :key) (or (org-property node :value) ""))))))))

(defun org-headline-property (headline key)
  "The value of the property KEY in HEADLINE's property drawer, or NIL when it
has none or an empty one."
  (let ((value (cdr (assoc key (org-headline-properties headline) :test #'string-equal))))
    (and (plusp (length value)) value)))

(defun org-inherited-property (tree node name)
  "The value property NAME has at NODE, inherited: the file's (#+PROPERTY:),
then each headline above NODE's from the outermost, NAME replacing what is
there and NAME+ adding to it; NIL when none."
  (let ((value (org-file-properties tree name))
        (headlines (loop for n = (org-node-parent node) then (org-node-parent n)
                         while n when (eq (org-node-type n) 'headline) collect n)))
    (dolist (headline (reverse headlines) value)
      (loop for (key . v) in (org-headline-properties headline)
            do (setf value (org-property-with value name key v))))))

(defun org-remove-common-indentation (text)
  "TEXT with the white space all its lines that are not blank begin with taken
away (a tab counting to the next eighth column)."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (indent (loop for line in lines
                       unless (string= (string-trim *org-blanks* line) "")
                         minimize (loop for c across line
                                        with column = 0
                                        do (case c
                                             (#\Space (incf column))
                                             (#\Tab (setf column (tuatara.display:next-tab-stop column 8)))
                                             (t (return column)))
                                        finally (return column)))))
    (if (or (null indent) (zerop indent))
        text
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (line)
                          (let ((column 0) (i 0))
                            (loop while (and (< i (length line)) (< column indent))
                                  do (case (char line i)
                                       (#\Space (incf column))
                                       (#\Tab (setf column (tuatara.display:next-tab-stop column 8)))
                                       (t (return)))
                                     (incf i))
                            (subseq line i)))
                        lines)))))

(defun org-block-code (block)
  "The code of BLOCK, a source block, as it is tangled and exported: its lines
unescaped and their common indentation taken away (`org-remove-common-indentation')."
  (org-remove-common-indentation (or (org-property block :value) "")))

(defun org-trim-code (code)
  "CODE without the spaces, tabs and blank lines at its start and end, as a block's code is written to its file."
  (string-trim '(#\Space #\Tab #\Newline #\Return) code))
