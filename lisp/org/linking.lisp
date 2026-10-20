;;;; lisp/org/linking.lisp --- Org links made: stored where you are, inserted or edited where you write.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; org-store-link keeps a link to where point is: a headline of an Org
;;;; file (by its CUSTOM_ID, else its title), one of your notes (denote:ID),
;;;; a line of another file. org-insert-link (C-c C-l) asks for a link, the
;;;; stored ones and the kinds of link offered, file: and denote: chosen from
;;;; your files, and a label, and writes [[link][label]]; on a link, it edits
;;;; it. A stored link to the file it is put in becomes a link within it, as
;;;; Org makes it.

(in-package #:tuatara.defaults)

(defvar *org-stored-links* '()
  "The links org-store-link kept, the latest first, each (LINK LABEL), LABEL NIL for none.")

(defparameter *org-link-kinds*
  '("file:" "denote:" "https:" "http:" "mailto:" "id:" "doi:" "formatted-match:")
  "The kinds of link org-insert-link offers besides the stored links: file:
and denote: go on to ask for the file or the note.")

(defun org-link-path (path directory)
  "PATH (native) as a file: link writes it from a file in DIRECTORY (native, ending
in /): from DIRECTORY when it is in
it or under it, else with the home directory written ~."
  (if (and directory (uiop:string-prefix-p directory path))
      (subseq path (length directory))
      (abbreviated-path path)))

(defun org-note-title (path)
  "The title of the note at PATH: the title its front matter gives
\(NOTE-FRONT-MATTER), else the title its formatted name holds, hyphens as
spaces."
  (or (note-front-matter path)
      (let ((title (nth-value 2 (name-parts (tuatara.io:native-file-name path)))))
        (and title (substitute #\Space #\- title)))))

(defun org-link-to-here (buffer)
  "A link to where point is in BUFFER and its label, as two values: in one of
your notes, denote:ID and its title; under
an Org headline, file:PATH::#CUSTOM_ID or file:PATH::*Title and the title; in
a file not Org, file:PATH::the line point is on; NIL where there is no file."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (when file
      (let* ((path (tuatara.io:native file))
             (name (tuatara.io:native-file-name path))
             (identifier (and (identifier-at-start-p name) (name-parts name))))
        (cond
          ((typep (buffer-major-mode buffer) 'org-mode)
           (multiple-value-bind (tree text) (org-tree-now buffer)
             (declare (ignore text))
             (let* ((headline (and tree (org-current-headline tree (org-point-char buffer))))
                    (custom-id (and headline (org-headline-property headline "CUSTOM_ID")))
                    (title (and headline (org-property headline :raw-value))))
               (cond ((and identifier (null headline)) (values (format nil "denote:~a" identifier) (org-note-title path)))
                     (custom-id (values (format nil "file:~a::#~a" (abbreviated-path path) custom-id) title))
                     (headline (values (format nil "file:~a::*~a" (abbreviated-path path) title) title))
                     (t (values (format nil "file:~a" (abbreviated-path path)) nil))))))
          (identifier (values (format nil "denote:~a" identifier) (org-note-title path)))
          (t (let* ((point (tuatara.buffer:buffer-point buffer))
                    (line (string-trim " 	" (tuatara.buffer:buffer-string buffer :start (tuatara.buffer:buffer-line-start buffer point)
                                                                                 :end (tuatara.buffer:buffer-line-end buffer point)))))
               (values (if (plusp (length line))
                           (format nil "file:~a::~a" (abbreviated-path path) line)
                           (format nil "file:~a" (abbreviated-path path)))
                       nil))))))))

(define-command org-store-link ()
  "Keep a link to where point is, for org-insert-link (\\[org-insert-link]) to
offer: under an Org headline, to it (by its
CUSTOM_ID, else its title); in one of your notes, to the note (denote:ID); in
another file, to the line point is on."
  (multiple-value-bind (link label) (org-link-to-here (current-buffer))
    (if (null link)
        (message "No link to store here: this buffer has no file")
        (progn
          (setf *org-stored-links* (cons (list link label) (remove link *org-stored-links* :key #'first :test #'string=)))
          (message "Stored: ~a" (or label link))))))

(defun org-link-within (link buffer)
  "LINK, a stored link, as it is put in BUFFER: a file: link to a
headline of BUFFER's own file made a link within it (*Title, #id); another
file: link written from BUFFER's folder when the file is in it or under it."
  (let ((file (tuatara.buffer:buffer-file buffer))
        (directory (buffer-directory buffer)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^file:(.+?)(?:::(.*))?$" link)
      (if (not match)
          link
          (let ((path (tuatara.io:native (tuatara.io:expand-path (aref groups 0) :directory directory)))
                (search (aref groups 1)))
            (if (and file search (plusp (length search)) (find (char search 0) "*#")
                     (string= path (tuatara.io:native file)))
                search
                (format nil "file:~a~@[::~a~]" (org-link-path path directory) search)))))))

(defun read-note-link ()
  "Ask for one of your notes, as open-file-formatted lists them
(READ-FORMATTED-FILE), and return a denote: link to it and its title, as two
values; NIL when none was chosen."
  (let ((chosen (read-formatted-file "Link to note: ")))
    (when chosen
      (values (format nil "denote:~a" (name-parts (tuatara.io:native-file-name chosen)))
              (org-note-title (concatenate 'string (directory-name *formatted-file-directory*) chosen))))))

(defun read-file-link (buffer)
  "Ask for a file, as find-file asks, and return a file: link to it from BUFFER's folder."
  (let ((path (read-file-path "File link: ")))
    (and path
         (format nil "file:~a" (org-link-path (tuatara.io:native (tuatara.io:expand-path path :directory (buffer-directory buffer)))
                                              (buffer-directory buffer))))))

(defun read-link-target (buffer)
  "Ask for a link, offering the stored links and the kinds of link: file:
and denote: go on to ask for the file or note. RET with nothing typed takes
the link stored last. The link and a label to offer for it, as two values;
NIL when none was given."
  (let* ((stored (loop for (link label) in *org-stored-links*
                       for rank from 0
                       collect (make-candidate link :annotation label :group "Stored links" :rank rank)))
         (kinds (mapcar (lambda (kind) (make-candidate kind :group "Kinds of link")) *org-link-kinds*))
         (chosen (completing-read "Link: " (append stored kinds)
                                  :select (lambda (shown input)
                                            (or (and (string= (string-trim " " input) "") *org-stored-links*
                                                     (position (first (first *org-stored-links*)) shown
                                                               :key #'candidate-string :test #'string=))
                                                :input))
                                  :reselect t)))
    (cond ((string= chosen "file:") (values (read-file-link buffer) nil))
          ((string= chosen "denote:") (read-note-link))
          ((string= (string-trim " " chosen) "") nil)
          (t (let ((entry (find chosen *org-stored-links* :key #'first :test #'string=)))
               (values chosen (second entry)))))))

(defun org-link-text (link label)
  "The Org link to LINK labelled LABEL: [[LINK][LABEL]], [[LINK]] when LABEL is empty, LINK escaped (ORG-LINK-ESCAPE)."
  (if (plusp (length label))
      (format nil "[[~a][~a]]" (org-link-escape link) label)
      (format nil "[[~a]]" (org-link-escape link))))

(define-command org-insert-link ()
  "Insert an Org link (\\`C-c C-l'): it asks for the
link, offering those org-store-link (\\[org-store-link]) kept and the kinds of
link (file: asks for a file, denote: for one of your notes), then for its
label, one offered selected (the stored link's, the note's title) to take or
type over; the text selected becomes the label instead. A stored link to a
headline of this file becomes a link within it (*Title). On a link, edit it:
its link and label offered. With \\[universal-argument], a link to a file."
  (let* ((buffer (current-buffer))
         (on-link (and (typep (buffer-major-mode buffer) 'org-mode)
                       (multiple-value-bind (tree text) (org-tree-now buffer)
                         (let ((*org-current-text* (or text "")))
                           (let ((link (and tree (org-link-at tree (org-point-char buffer)))))
                             (and link (eq (org-property link :format) 'bracket)
                                  (list link text))))))))
    (if on-link
        (destructuring-bind (link text) on-link
          (let* ((target (read-string "Link: " :initial (org-property link :raw-link)))
                 (old-label (if (org-node-contents-begin link)
                                (subseq text (org-node-contents-begin link) (org-node-contents-end link))
                                ""))
                 (label (read-string "Label: " :initial old-label :select-initial t))
                 (problem (link-label-problem label)))
            (if problem
                (message "Link not changed: ~a" problem)
                (let ((*org-current-text* text))
                  (org-replace-chars buffer (org-node-begin link) (- (org-node-end link) (org-node-post-blank link))
                                     (org-link-text target label)
                                     :point (+ (org-node-begin link) (length (org-link-text target label))))))))
        (let ((region (selected-region buffer)))
          (multiple-value-bind (target suggested)
              (if (prefix-argument) (values (read-file-link buffer) nil) (read-link-target buffer))
            (if (null target)
                (message "No link inserted")
                (let* ((stored target)
                       (target (org-link-within target buffer))
                       (label (read-link-label region buffer (or suggested "")))
                       (problem (link-label-problem label)))
                  (if problem
                      (message "No link inserted: ~a" problem)
                      (progn
                        (setf *org-stored-links* (remove stored *org-stored-links* :key #'first :test #'string=))
                        (insert-link-text buffer region (org-link-text target label)))))))))))

(defun install-org-linking ()
  "Bind \\`C-c C-l' in Org buffers to org-insert-link."
  (define-key (mode-keymap 'org-mode) "C-c C-l" 'org-insert-link))
