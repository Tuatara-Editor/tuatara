;;;; test/places.lisp --- Where point was in each file, kept between sessions (save-place-mode).
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test places
  :description "Where point was in a file is remembered, as Emacs's save-place-mode remembers it: kept when its buffer goes, it is saved or the editor stops, and put back when the file is visited again.")

(defun place-after (path keys)
  "Visit PATH in a session typing KEYS, and return where point is in its buffer
then; every file buffer killed after, as leaving does."
  (let* ((editor (nth-value 1 (run-keys keys :file (namestring path) :width 80 :height 12)))
         (point (session-point editor)))
    (forget-file-buffers)
    point))

(define-test (places put-back)
  (with-test-directory (dir)
    (let ((tuatara-user:*save-place-mode* t)
          (path (merge-pathnames "notes.txt" dir)))
      (alexandria:write-string-into-file (format nil "one~%two~%three~%") path)
      ;; left on the second line's third character, back there next time, and the time after
      (is = 6 (place-after path "j l l"))
      (is = 6 (place-after path ""))
      (is = 6 (place-after path ""))
      ;; moved again: the new place
      (is = 10 (place-after path "j"))
      ;; the file shrank meanwhile: its end
      (alexandria:write-string-into-file "one" path :if-exists :supersede)
      (is = 3 (place-after path ""))
      ;; kept readable by you alone, a line a file
      (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (tuatara.defaults::state-list-path tuatara.defaults::*saved-places*))))))
      (true (assoc (tuatara.io:native path) (tuatara.defaults::read-saved-places) :test #'string=))
      ;; off: files open at their top
      (alexandria:write-string-into-file (format nil "one~%two~%three~%") path :if-exists :supersede)
      (place-after path "j j")
      (let ((tuatara-user:*save-place-mode* nil))
        (is = 0 (place-after path ""))))))

(define-test (places git-messages-at-their-top)
  ;; a commit's message is written afresh each time: its next one opens at the top, where it is typed
  (with-test-directory (dir)
    (let ((tuatara-user:*save-place-mode* t)
          (path (merge-pathnames "COMMIT_EDITMSG" dir)))
      (alexandria:write-string-into-file (format nil "~%# Please enter the commit message.~%# On branch main~%") path)
      (place-after path "j j l l")
      (is = 0 (place-after path ""))
      (false (assoc (tuatara.io:native path) (tuatara.defaults::read-saved-places) :test #'string=)))))

(define-test (places characters-and-folds)
  (with-test-directory (dir)
    (let ((tuatara-user:*save-place-mode* t))
      ;; a place inside a character that changed: the character's start
      (let ((path (merge-pathnames "words.txt" dir)))
        (alexandria:write-string-into-file "abcdef" path)
        (is = 2 (place-after path "l l"))
        ;; the octet 2 is now inside the first é, which begins at 1
        (alexandria:write-string-into-file "aéé" path :if-exists :supersede)
        (is = 1 (place-after path "")))
      ;; an Org file that opens folded: the fold around the place opened
      (let ((path (merge-pathnames "plan.org" dir)))
        (alexandria:write-string-into-file (format nil "#+STARTUP: overview~%* One~%deep text~%* Two~%") path)
        (let ((here (search "deep" (alexandria:read-file-into-string path))))
          ;; left on the hidden line (as another session left it, the file shown whole)
          (tuatara.defaults::state-list-put tuatara.defaults::*saved-places* (list (cons (tuatara.io:native path) here)))
          (let* ((editor (nth-value 1 (run-keys "" :file (namestring path) :width 80 :height 12)))
                 (buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            (is = here (tuatara.buffer:buffer-point buffer))
            (false (tuatara.display:fold-at buffer here))
            (forget-file-buffers)))))))

(define-test (places kept-few)
  (with-test-directory (dir)
    (let ((tuatara-user:*save-place-mode* t)
          (tuatara-user:*save-place-limit* 2))
      (dolist (name '("a.txt" "b.txt" "c.txt"))
        (alexandria:write-string-into-file (format nil "x~%y~%") (merge-pathnames name dir))
        (place-after (merge-pathnames name dir) "j"))
      (let ((kept (mapcar (lambda (entry) (file-namestring (car entry))) (tuatara.defaults::read-saved-places))))
        (is equal '("c.txt" "b.txt") kept)))))

(define-test (places typing-and-deleting-undo-together)
  ;; characters typed and deleted one after another are one undo step: the file as it was, unmodified
  (with-test-directory (dir)
    (let ((path (merge-pathnames "note.txt" dir)))
      (put-file path (format nil "one~%two~%"))
      (multiple-value-bind (f editor) (run-keys "i h e l l o DEL DEL DEL DEL DEL C-g u" :file (namestring path) :width 80 :height 8)
        (declare (ignore f))
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= (format nil "one~%two~%") (tuatara.buffer:buffer-string b))
          (false (tuatara.buffer:buffer-modified-p b))
          (tuatara.buffer:kill-buffer b)))
      ;; a deletion after moving away is a step of its own
      (multiple-value-bind (f editor) (run-keys "i h i C-g l i DEL C-g u" :file (namestring path) :width 80 :height 8)
        (declare (ignore f))
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= (format nil "hione~%two~%") (tuatara.buffer:buffer-string b))
          (tuatara.buffer:buffer-mark-saved b)
          (tuatara.buffer:kill-buffer b))))))
