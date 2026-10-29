;;;; lisp/files/places.lisp --- Where point was in each file, kept between sessions: save-place-mode, as Emacs's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's save-place-mode: where point is in a file is written to the
;;;; state directory when its buffer is killed, when it is saved and when the
;;;; editor stops, and a file visited again, in this session or a later one,
;;;; opens with point there, a fold around it opened. The places live in one
;;;; file of the state directory (state.lisp), so that sessions side by side
;;;; share them, as the list of recent files is shared.

(in-package #:tuatara.defaults)

(defvar *save-place-mode* t
  "True to remember where point was in each file you visit and put it back
there when you visit the file again, in this session or a later one
\(on as standard); NIL to open files at their top.
The places are the file places in $XDG_STATE_HOME/tuatara/
\(~/.local/state/tuatara/), readable by you alone.")

(defvar *save-place-limit* 400
  "How many files' places are kept, the latest first.")

(defun parse-saved-place-line (line)
  "The entry of the places LINE writes, (PATH . OFFSET): the octet point was
at, then the file's path (ESCAPE-PATH); NIL for a line that makes no sense."
  (let ((space (position #\Space line)))
    (when (and space (< 0 space 19) (every #'digit-char-p (subseq line 0 space)))
      (let ((path (unescape-path (subseq line (1+ space)))))
        (and (plusp (length path)) (cons path (parse-integer line :end space)))))))

(defun saved-place-line (entry)
  "The line of the places for ENTRY, (PATH . OFFSET)."
  (format nil "~d ~a" (cdr entry) (escape-path (car entry))))

(define-state-list *saved-places* "places" 'parse-saved-place-line 'saved-place-line '*save-place-limit*
  :header (format nil "# Where point was in the files Tuatara visited, the latest first: the octet,~%~
                       # then the file's path (\\\\, \\n and \\r stand for \\, a newline and a return).~%")
  :path-of #'car)

(defun read-saved-places ()
  "The places: a list of (PATH . OFFSET), the latest first, OFFSET the octet
point was at; empty when there are none (STATE-LIST-ENTRIES)."
  (state-list-entries *saved-places*))

(defun buffer-file-place (buffer)
  "The file BUFFER visits and where its point is, as (PATH . OFFSET); NIL for
a buffer with no file, or one whose mode keeps no place (MODE-KEEPS-PLACE-P)."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (and file (live-buffer-p buffer) (mode-keeps-place-p (buffer-major-mode buffer))
         (let ((path (tuatara.io:native file)))
           (and path (cons path (tuatara.buffer:buffer-point buffer)))))))

(defun save-places (buffers)
  "Put where point is in the files of BUFFERS first among the places, when
*SAVE-PLACE-MODE* is true: written in the background, with what sessions side
by side put there meanwhile (STATE-LIST-PUT)."
  (when *save-place-mode*
    (state-list-put *saved-places* (remove nil (mapcar #'buffer-file-place buffers)))))

(defun save-place-of (buffer)
  "Keep where point is in BUFFER's file: as it is killed, and after it is saved."
  (save-places (list buffer)))

(defun save-places-at-exit (editor)
  "Keep where point is in every file open, as the editor stops."
  (declare (ignore editor))
  (save-places (remove-if-not #'tuatara.buffer:buffer-file tuatara.buffer:*buffers*)))

(defun restore-place (buffer)
  "Put point in BUFFER, a file just visited, where it was when the file was
last left, when *SAVE-PLACE-MODE* is true, its mode keeps places
\(MODE-KEEPS-PLACE-P) and nothing has moved it yet: at that octet or the
file's end when it has shrunk, on the start of the character there, a fold
around it opened."
  (when (and *save-place-mode* (zerop (tuatara.buffer:buffer-point buffer))
             (mode-keeps-place-p (buffer-major-mode buffer)))
    (let* ((path (tuatara.io:native (tuatara.buffer:buffer-file buffer)))
           (entry (and path (state-list-find *saved-places* path))))
      (when (and entry (plusp (cdr entry)))
        (let* ((rope (tuatara.buffer:buffer-text buffer))
               (offset (min (cdr entry) (tuatara.buffer:buffer-size buffer)))
               (offset (tuatara.text:rope-char-start rope offset)))
          (goto-shown buffer offset))))))

(defun install-places ()
  "Remember where point is in files, and put it back when they are visited
again: after each visit, as the last thing done (once a mode has folded the
file); as a buffer is killed and after it is saved; as the editor stops."
  (setf tuatara.buffer:*find-file-functions*
        (append (remove 'restore-place tuatara.buffer:*find-file-functions*) (list 'restore-place)))
  (pushnew 'save-place-of tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'save-place-of *after-save-functions*)
  (pushnew 'save-places-at-exit *exit-functions*))
