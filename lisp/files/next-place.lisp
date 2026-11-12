;;;; lisp/files/next-place.lisp --- Going from place to place of the list in force: M-g n and M-g p.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A compile's output, the lines a search of files found, what the language
;;;; servers found wrong in a buffer and the errors of a PDF made of LaTeX are
;;;; each a list of places. The one used last is the list in force, and M-g n
;;;; and M-g p go to the next of its places or the one before, whichever kind of
;;;; list it is: each kind says how it steps (STEP-PLACE). A list made by a
;;;; command whose entries name places is one as it is (ENTRY-PLACES): its mode
;;;; says which entry names which place (ENTRY-PLACE). A place in a file opens
;;;; as a line chosen from a search of files does (SHOW-FILE-LINE).

(in-package #:tuatara.defaults)

(defvar *places-in-force* nil
  "The list of places \\[next-place] and \\[previous-place] go through: the one
used last, a compile's output (\\[compile]), the lines a search of files found
\(\\[search-in-files]), what the language servers found wrong in a buffer, or
the errors of *LaTeX errors*; NIL before any has been used.")

(defgeneric step-place (places direction)
  (:documentation "Go to the place of PLACES, a list of places, after the one gone
to last (DIRECTION 1) or before it (-1), and say what is there; say so when
there is none that way."))

(defgeneric places-name (places)
  (:documentation "What PLACES, a list of places, is called where its ends are said: its
buffer's name (*compile*), or what it is (the search for kiwi)."))

(defun say-places-end (places direction)
  "Say that PLACES, a list of places, has no place after the one gone to last
\(DIRECTION 1) or before it (-1), naming it (PLACES-NAME): No more places in
*compile*, No place before this one in *compile*."
  (message (if (plusp direction) "No more places in ~a" "No place before this one in ~a")
           (places-name places)))

(defgeneric places-buffer (places)
  (:documentation "The buffer PLACES, a list of places, lives in, whose killing ends
it as the list in force (a compile's output); NIL for one of no buffer.")
  (:method (places) (declare (ignore places)) nil))

(defun forget-places-of (buffer)
  "Kill-buffer function: when the list of places in force lives in BUFFER,
there is none in force any more, its text not kept for it."
  (when (and *places-in-force* (eq buffer (places-buffer *places-in-force*)))
    (setf *places-in-force* nil)))

(defun use-places (places)
  "Make PLACES the list of places in force, the one \\[next-place] and
\\[previous-place] go through. Returns PLACES."
  (setf *places-in-force* places))

(defun line-place (buffer line &key (offset 0) column)
  "The octet in BUFFER's text where the line LINE (from 1; the last line for a
number past it) is entered: OFFSET octets in, or with COLUMN that many
characters in from 1, never past the line's end. Second value: true when LINE
is past the last line."
  (let* ((start (tuatara.buffer:buffer-goto-line buffer (max 0 (1- line))))
         (rope (tuatara.buffer:buffer-text buffer))
         (end (tuatara.text:rope-line-end rope start)))
    (values (if column
                (min end (cluster-forward buffer start (max 0 (1- column))))
                (tuatara.text:rope-char-start rope (min end (+ start offset))))
            (> line (tuatara.buffer:buffer-line-count buffer)))))

(defun show-file-line (path line &key (offset 0) column other)
  "Visit the file at PATH as \\[find-file] does and put point on its line LINE
\(from 1; its last line for one past it), OFFSET octets in, or with COLUMN
that many characters in from 1, never past the line's end. The line is put in
the middle of the window, in the selected window or with OTHER in another. A
buffer narrowed to a part not holding it is widened first, and that said. A
file shown otherwise than as text (a PDF's pages) is shown as it is. A file no
longer there is said, not made. Returns the buffer, or NIL then."
  (unless (tuatara.io:path-kind (tuatara.io:native (tuatara.io:expand-path path)))
    (message "~a is not there" (abbreviated-path path))
    (return-from show-file-line nil))
  (let ((buffer (visit-path path :show (if other
                                           (lambda (buffer) (pop-to-buffer buffer :other t))
                                           (lambda (buffer) (show-buffer buffer))))))
    (when (and (tuatara.buffer:buffer-file buffer)
               (eq buffer (tuatara.display:window-buffer (current-window))))
      (goto-shown buffer (line-place buffer line :offset offset :column column))
      (tuatara.display:window-recenter (current-window)))
    buffer))

(defun show-start-places (places)
  "Put point on the line asked for in each of PLACES, the files the editor was
started with, each (PATH LINE COLUMN) with COLUMN or NIL: a line past the end
goes to the last line, and that is said, as is a file not shown as text. The
file in the selected window is shown with the line in the middle."
  (let ((notes '()))
    (loop for (path line column) in places
          for buffer = (buffer-visiting path)
          do (cond ((null buffer))
                   ((not (and (tuatara.buffer:buffer-file buffer) (not (tuatara.display:hex-view-p buffer))))
                    (push (format nil "~a is not shown as text: line ~d not gone to" (abbreviated-path path) line) notes))
                   (t (multiple-value-bind (byte past) (line-place buffer line :column column)
                        (goto-shown buffer byte)
                        (when past
                          (push (format nil "~a has no line ~:d: at its last line" (abbreviated-path path) line) notes))
                        (when (eq buffer (tuatara.display:window-buffer (current-window)))
                          (tuatara.display:window-recenter (current-window)))))))
    (when notes (message "~{~a~^; ~}" (nreverse notes)))))

(defgeneric entry-place (mode entry)
  (:documentation "The file and line from 1 that ENTRY of a list in major MODE names, as
two values; NIL for an entry that names no place. A list whose entries name
places is a list of places for \\[next-place] and \\[previous-place] (ENTRY-PLACES).")
  (:method (mode entry)
    "A list's entries name no places."
    (declare (ignore mode entry))
    nil))

(defstruct (entry-places (:constructor entry-places (buffer)) (:copier nil))
  "A list made by a command (RENDER-PIECES) in BUFFER as a list of places
\(STEP-PLACE): its entries that name a file and a line (ENTRY-PLACE), in order,
the one gone to last that on the line of its point."
  (buffer nil :read-only t))

(setf (documentation 'entry-places-p 'function) "True when OBJECT is an ENTRY-PLACES."
      (documentation 'entry-places-buffer 'function) "The buffer of the list an ENTRY-PLACES goes through.")

(defmethod places-buffer ((places entry-places))
  "The buffer of the list: killed, its places are in force no more."
  (entry-places-buffer places))

(defmethod places-name ((places entry-places))
  "The name of the buffer of the list."
  (tuatara.buffer:buffer-name (entry-places-buffer places)))

(defmethod step-place ((places entry-places) direction)
  "Put the list's point on its next entry naming a place (DIRECTION 1), or the
one before (-1), and open its file at its line: in another window when the list
is in the selected one, else in the selected one; the list's line said."
  (let ((list (entry-places-buffer places)))
    (if (not (live-buffer-p list))
        (message "~a has been killed" (tuatara.buffer:buffer-name list))
        (let* ((mode (buffer-major-mode list))
               (entry (list-step-entry list direction :test (lambda (entry) (entry-place mode entry)))))
          (if (null entry)
              (say-places-end places direction)
              (let* ((start (tuatara.buffer:buffer-point list))
                     (said (tuatara.buffer:buffer-string
                            list :start start :end (tuatara.text:rope-line-end (tuatara.buffer:buffer-text list) start))))
                (dolist (window (buffer-windows list))
                  (setf (tuatara.display:window-point window) start))
                (multiple-value-bind (path line) (entry-place mode entry)
                  (when (show-file-line path line :other (eq list (tuatara.display:window-buffer (current-window))))
                    (message "~a" (tuatara.display:shorten-string (string-trim " " said) 200 :one-line t))))))))))

(define-command next-place ()
  "Go to the next place of the list of places used last: the next line naming
a file and line in a compile's output (\\[compile]), the next line a search of
files found (\\[search-in-files]), the next place the language servers found
something wrong in a buffer, or the next error of *LaTeX errors*. A place in a
file opens it at that line, in another window from the list itself, else in
this one."
  (if *places-in-force*
      (step-place *places-in-force* 1)
      (message "No list of places yet: \\[compile], \\[search-in-files] or a language server's diagnostics make one")))

(define-command previous-place ()
  "Go to the place before the one gone to last in the list of places used
last: a compile's output (\\[compile]), the lines a search of files found
\(\\[search-in-files]), the places the language servers found something
wrong in a buffer, or the errors of *LaTeX errors*, as \\[next-place] goes to
the next."
  (if *places-in-force*
      (step-place *places-in-force* -1)
      (message "No list of places yet: \\[compile], \\[search-in-files] or a language server's diagnostics make one")))

(defun install-next-place ()
  "Bind \\`M-g n' to \\[next-place] and \\`M-g p' to \\[previous-place], and let a
list of places go with the buffer it lives in."
  (define-keys *global-keymap* '(("M-g n" next-place) ("M-g p" previous-place)))
  (pushnew 'forget-places-of tuatara.buffer:*kill-buffer-functions*))
