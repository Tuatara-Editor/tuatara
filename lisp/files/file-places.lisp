;;;; lisp/files/file-places.lisp --- A place in a file or folder, found again after the text has changed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What bookmarks and registers keep of a place: the file or folder, the octet
;;;; in it, the text that followed, and whether the file was opened safely. Made
;;;; from a buffer, found again in a buffer, gone to by visiting the file.

(in-package #:tuatara.defaults)

(defparameter *file-place-context-length* 32
  "How many characters after a place it keeps, to find the place again once the
text before it has changed.")

(defstruct (file-place (:constructor make-file-place (path offset context safe)) (:copier nil))
  "A place in the file or folder at PATH: the octet OFFSET in it, the text that
followed it (CONTEXT), and SAFE when the file was opened safely."
  path offset context safe)

(setf (documentation 'make-file-place 'function) "Return a FILE-PLACE."
      (documentation 'file-place-p 'function) "True when OBJECT is a FILE-PLACE.")

(defun file-place-target (buffer)
  "What a place of BUFFER is in: its file, or the folder a directory list shows;
NIL for a buffer of neither."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (cond (file (tuatara.io:native file))
          ((buffer-mode-named-p buffer 'rimu-mode) (buffer-directory buffer)))))

(defun character-start (buffer octet)
  "OCTET of BUFFER, or the start of the character it falls inside, as far as
the text goes."
  (tuatara.text:rope-char-start (tuatara.buffer:buffer-text buffer) (min octet (tuatara.buffer:buffer-size buffer))))

(defun text-after-place (buffer position)
  "The text after POSITION of BUFFER a place keeps: *FILE-PLACE-CONTEXT-LENGTH*
characters at most, up to a tab."
  (let* ((start (character-start buffer position))
         (limit (* 4 *file-place-context-length*))
         (text (tuatara.buffer:buffer-string buffer :start start :end (character-start buffer (+ start limit))))
         (text (subseq text 0 (min (length text) *file-place-context-length*))))
    (subseq text 0 (or (position #\Tab text) (length text)))))

(defun file-place-here (buffer &optional (position (tuatara.buffer:buffer-point buffer)))
  "The FILE-PLACE of POSITION (point by default) in BUFFER, safe when the buffer
was opened so; NIL for a buffer with no file or folder."
  (let ((target (file-place-target buffer)))
    (and target
         (make-file-place target position (text-after-place buffer position) (tuatara.io:safe-visit-p buffer)))))

(defun file-place-found (buffer place)
  "Where in BUFFER PLACE is now: its offset (or the start of the character it
falls inside) when the text there still begins with its context, else the
nearest place within a megabyte that does, else its offset (within the text)."
  (let* ((context (file-place-context place))
         (size (tuatara.buffer:buffer-size buffer))
         (offset (character-start buffer (file-place-offset place)))
         (limit (* 4 (length context)))
         (here (tuatara.buffer:buffer-string buffer :start offset :end (character-start buffer (+ offset limit)))))
    (if (or (zerop (length context)) (uiop:string-prefix-p context here))
        offset
        (let* ((from (character-start buffer (max 0 (- offset (* 1024 1024)))))
               (to (character-start buffer (min size (+ offset (* 1024 1024)))))
               (text (tuatara.buffer:buffer-string buffer :start from :end to))
               (best nil))
          (loop with byte = from and counted = 0
                for at = (search context text) then (search context text :start2 (1+ at))
                while at
                do (incf byte (tuatara.text:string-octet-length text :start counted :end at))
                   (setf counted at)
                   (when (or (null best) (< (abs (- byte offset)) (abs (- best offset))))
                     (setf best byte)))
          (or best offset)))))

(defun file-place-kind (path)
  "What the place at PATH is in, as the bookmark manager says it: \"file\",
\"folder\", or \"gone\" when it is no longer there."
  (let ((kind (ignore-errors (tuatara.io:path-kind path))))
    (case kind
      ((nil) "gone")
      (:directory "folder")
      (t "file"))))

(defun go-to-file-place (name place &key other)
  "Visit the file or folder PLACE is in (in another window with OTHER), safely
when it was opened so, and go to the place, found again where the text has
moved it (FILE-PLACE-FOUND); a folder's in a rimu list of its project there
is, moved there (*RIMU-REUSE-LISTS*), else in one of its own. Returns the
buffer. A file or folder no longer there is said so with NAME, nothing made in
its place, and NIL is returned."
  (let* ((path (file-place-path place))
         (kind (file-place-kind path))
         (buffer (or (buffer-visiting path)
                     (and (string= kind "folder") (rimu-show-in-project path :show #'identity))
                     (and (string/= kind "gone")
                          (visit-path path :safe (file-place-safe place) :show #'identity)))))
    (cond ((not (typep buffer 'tuatara.buffer:buffer))
           (message "~a: ~a is no longer there" name (abbreviated-path path))
           nil)
          (t (if other (pop-to-buffer buffer :other t) (show-buffer buffer))
             (goto-shown buffer (file-place-found buffer place))
             buffer))))
