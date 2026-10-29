;;;; lisp/help/faces.lisp --- Faces described: M-x list-faces and M-x describe-face.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every face a theme can colour, listed in sections by the first word of
;;;; its name, each with a sample drawn in it as the theme in force draws it and
;;;; what it colours; and one face described whole in *Help*: what it colours,
;;;; its definition, what the theme in force and the init file set, what it
;;;; inherits and how it is drawn now.

(in-package #:tuatara.defaults)

(defparameter *face-sample* "Aa Bb 0123"
  "The text the list of faces (\\[list-faces]) and \\[describe-face] draw in each face.")

(defun listed-faces ()
  "The names of the faces a theme can colour, sorted: every face defined with
a description, but the samples the list of fonts makes."
  (let ((names '()))
    (maphash (lambda (name face)
               (when (and (plusp (length (tuatara.display:face-doc face))) (not (font-sample-face-p name)))
                 (push name names)))
             tuatara.display:*faces*)
    (sort names #'string< :key #'symbol-name)))

(defun face-name-text (name)
  "Face NAME as it is written in an init file: :comment."
  (format nil ":~(~a~)" (symbol-name name)))

(defun face-candidates ()
  "Completion candidates for every face a theme can colour, each with the
first sentence of its description beside it."
  (mapcar (lambda (name)
            (make-candidate (face-name-text name)
                            :annotation (list (documentation-field (tuatara.display:face-doc (tuatara.display:find-face name)) 1/2))
                            :value name))
          (listed-faces)))

;;; One face described

(defun face-setting-words (attributes &optional theme)
  "ATTRIBUTES, a property list as SET-FACE-ATTRIBUTES takes it, in words: each
key and value, a role of THEME's palette followed by the colour it stands
for; none when there are none."
  (if (null attributes)
      "none"
      (format nil "~{~a~^ ~}"
              (loop for (key value) on attributes by #'cddr
                    collect (format nil "~(~s~) ~a" key
                                    (if (and theme (tuatara.display:role-p value))
                                        (format nil "~(~a~) (~a)" value
                                                (or (ignore-errors (tuatara.display:theme-role-colour theme value)) "no colour"))
                                        (string-downcase (prin1-to-string value))))))))

(defun theme-face-settings (theme name)
  "What THEME, and the themes it takes its faces from, set for face NAME, as
written, roles and all: a property list, each attribute as the last setting
of it leaves it."
  (let ((attributes '()))
    (loop repeat 16
          for source = theme then (tuatara.display:theme-source source)
          while source
          do (dolist (setting (reverse (tuatara.display:theme-settings source)))
               (when (eq (car setting) name)
                 (loop for (key value) on (cdr setting) by #'cddr
                       when (eq (getf attributes key :unset) :unset)
                         do (setf attributes (append attributes (list key value)))))))
    attributes))

(defparameter *face-attribute-words*
  '((:fg "Colour") (:bg "Ground") (:bold "Bold") (:italic "Italic") (:underline "Underlined")
    (:underline-colour "Underline colour") (:reverse "Reversed") (:dim "Dim") (:extend "To the edge")
    (:font "Font") (:height "Rows tall"))
  "The attributes of a face \\[describe-face] shows as it is drawn now, each with its name in words.")

(defun describe-face-pieces (name)
  "The pieces of *Help* describing face NAME (SHOW-HELP-PIECES): its sample,
its description, its definition, what the theme in force and the init file
set for it, the face it inherits from, a link, and each attribute as it is
drawn now, with the face it comes from when another gives it."
  (let* ((face (tuatara.display:find-face name))
         (theme (and tuatara.display:*current-theme* (tuatara.display:find-theme tuatara.display:*current-theme*))))
    (collecting-pieces (put)
      (flet ((link (other) (put (face-name-text other) :button (lambda () (show-help-pieces (lambda () (describe-face-pieces other)))))))
        (put (format nil "~a is a face.~%~%" (face-name-text name)))
        (put "Sample: ")
        (put *face-sample* name)
        (put (format nil "~%~%~a~%~%" (with-output-to-string (out)
                                        (write-documentation out (tuatara.display:face-doc face)))))
        (put (format nil "Its definition: ~a~%" (face-setting-words (tuatara.display:face-base face))))
        (when theme
          (put (format nil "The theme ~(~a~) sets: ~a~%" (tuatara.display:theme-name theme)
                       (face-setting-words (theme-face-settings theme name) theme))))
        (let ((yours (cdr (assoc name tuatara.display:*user-face-settings*))))
          (when yours (put (format nil "Your init file sets: ~a~%" (face-setting-words yours)))))
        (let ((parent (tuatara.display:face-inherit face)))
          (when parent
            (put "Inherits from ")
            (link parent)
            (put (format nil ": what this face leaves unset is drawn as that one draws it.~%"))))
        (put (format nil "~%As drawn now~@[, in the theme ~(~a~)~]:~%" (and theme (tuatara.display:theme-name theme))))
        (let ((any nil))
          (loop for (attribute words) in *face-attribute-words*
                do (multiple-value-bind (value from) (tuatara.display:face-attribute name attribute)
                     (when value
                       (setf any t)
                       (put (format nil "  ~a ~a" (column-text words 18 :elide nil) (cond ((eq value t) "yes")
                                                                ((stringp value) value)
                                                                (t (string-downcase (princ-to-string value))))))
                       (unless (eq from name)
                         (put ", from ")
                         (link from))
                       (put (string #\Newline)))))
          (unless any (put (format nil "  nothing set: drawn as the text around it~%"))))))))

(define-command describe-face ()
  "Show all that is known of a face: a sample drawn in it, what it colours,
its definition, what the theme in force and your init file set for it, the
face it inherits from and each of its attributes as it is drawn now."
  (let ((chosen (completing-read "Describe face: " (face-candidates) :require-match t)))
    (when (and chosen (symbolp chosen))
      (show-help-pieces (lambda () (describe-face-pieces chosen))))))

;;; Every face listed

(define-major-mode face-list-mode special-mode
  "The list of faces (\\[list-faces]): every face a theme can colour, in
sections by the first word of its name, each with its name, a sample drawn in
it as the theme in force draws it, and the first sentence of what it colours.
\\<face-list-mode>\\[list-visit] or \\`l' on a face shows all that is known of
it (\\[describe-face]), \\[list-visit-other] the list staying selected;
\\[list-next-entry] and \\[list-previous-entry] go from one face to the next;
\\[search-filter-regex] narrows the list to the faces whose lines match;
\\[revert-buffer] lists them again; \\`q' closes."
  :lighter "Faces" :interactive nil)

(defun face-section (name)
  "The first word of face NAME, up to its first - or ., or the whole of it:
the section of the list of faces it is in, with the others it shares that word with."
  (let* ((text (string-downcase (symbol-name name)))
         (end (position-if (lambda (c) (member c '(#\- #\.))) text)))
    (subseq text 0 end)))

(defparameter *other-faces* "Other faces"
  "The title of the section of the list of faces whose first word no other face shares.")

(defun face-list-pieces ()
  "The pieces of the list of faces, made as the faces are now: a section for
each first word two or more faces share, in the order of the words, then one
of the faces whose first word is their own; each section's title in
:heading, then a line for each face. The entries, a vector, as a second
value: (:SECTION TITLE) for a title, the face's name for its line."
  (let* ((names (listed-faces))
         (width (min 32 (reduce #'max names :key (lambda (n) (length (face-name-text n))) :initial-value 0)))
         (sections (group-sections names #'face-section :order #'string<))
         (shared (remove-if (lambda (section) (null (cddr section))) sections))
         (alone (loop for section in sections unless (cddr section) append (cdr section))))
    (entry-lines
     (lambda (line)
       (flet ((section (title faces)
                (funcall line "" nil nil)
                (funcall line (format nil "~a (~d)" title (length faces)) :heading (list :section title))
                (dolist (name faces)
                  (funcall line (list (format nil "  ~a  " (column-text (face-name-text name) width))
                                      (cons *face-sample* name)
                                      "  "
                                      (cons (first-sentence (substitute-command-keys (tuatara.display:face-doc (tuatara.display:find-face name)) :marks t))
                                            :list-note))
                           nil name))))
         (funcall line (format nil "Every face a theme can colour~@[, drawn as the theme ~(~a~) draws them~]: ~d"
                               tuatara.display:*current-theme* (length names))
                  :heading nil)
         (loop for (title . faces) in shared do (section title faces))
         (when alone (section *other-faces* alone)))))))

(defun face-list-render (list &optional (point :keep))
  "Fill LIST, the list of faces, with every face as it is now
\(FACE-LIST-PIECES), point put as RENDER-PIECES puts it."
  (multiple-value-bind (pieces entries) (face-list-pieces)
    (render-pieces list pieces :point point :entries entries :keys t
                               :refill (lambda (list) (face-list-render list :keep)))))

(define-command list-faces ()
  "List every face a theme can colour in *Faces*, in sections by the first
word of their names: each face's name, a sample drawn in it as the theme in
force draws it, and the first sentence of what it colours. \\<face-list-mode>\\[list-visit]
on a face shows all that is known of it, as \\[describe-face] does;
\\[search-filter-regex] narrows the list to the faces whose lines match;
\\[revert-buffer] lists them again, after a theme is loaded or a face set."
  (let ((list (set-search-filtering (buffer-in-mode "*Faces*" 'face-list-mode)
                                    :nouns "faces" :heading #'consp)))
    (face-list-render list :start)
    (show-buffer list)))

(defmethod list-entry-noun ((mode face-list-mode))
  "The list of faces lists faces."
  "face")

(defmethod visit-entry ((mode face-list-mode) list entry other)
  "Show all that is known of the face on the line in *Help*, selected, or
with OTHER the list kept selected."
  (declare (ignore list))
  (if (not (keywordp entry))
      (message "No face on this line")
      (let ((window (show-help-pieces (lambda () (describe-face-pieces entry)))))
        (when (and window (not other)) (select-window window)))))

(defun install-faces-help ()
  "Give the list of faces the keys of a list whose entries are opened."
  (entry-list-keys 'face-list-mode "the list of faces (\\[list-faces])"))
