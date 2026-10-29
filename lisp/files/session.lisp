;;;; lisp/files/session.lisp --- The session kept: the files open and the windows, as they were, at the next start.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; With *KEEP-SESSION* true, the editor writes what it shows to the file
;;;; session in its state folder (for you alone): the buffers visiting files
;;;; and the lists of directories, each with where point was, and how the
;;;; windows are split, each window's buffer, point and first line, and which
;;;; is selected. It is written as the editor stops, and now and then while
;;;; it runs (*KEEP-SESSION-INTERVAL*), so that a crash loses little. Started
;;;; again without files, the editor opens them again and splits its windows
;;;; as they were: a file opened safely is opened safely again, one no longer
;;;; there is left out, and windows that no longer fit are left out.
;;;;
;;;; The file is lines of words, read as text, never as Lisp: a line that is
;;;; not one of its own kinds ends the reading. What a frame shows is a
;;;; section of its own, so that a session of several frames would keep each.

(in-package #:tuatara.defaults)

(defvar *keep-session* nil
  "True for the editor to keep its session: the files and directories open and
how the windows show them, written as it stops (and now and then while it
runs), opened again as it was when it starts without files.")

(defvar *keep-session-interval* 30
  "Seconds between the writings of the session while the editor runs, when it
has changed (*KEEP-SESSION*): what a crash can lose.")

(defun session-path ()
  "The file the session is kept in: session in the state folder."
  (tuatara.io:path-join (tuatara.io:native (tuatara.io:state-directory)) "session"))

;;; What is kept

(defun session-entry (buffer)
  "What the session keeps of BUFFER, as (KIND POINT TARGET): KIND :FILE, :SAFE
\(a file opened safely), :DIRECTORY (a rimu list) or :SCRATCH (one of the
scratch buffers, by name); a view of a file (a PDF's pages, a picture, an
Office document) kept as its file, opened again as it opens; NIL for a buffer
not kept (help, lists, terminals, buffers with no file, an encrypted file's text)."
  (let* ((point (tuatara.buffer:buffer-point buffer))
         (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         ;; a view (a PDF's pages, a picture, an Office document's pages or text) kept by the file it shows
         (file (and (not directory)
                    (or (tuatara.buffer:buffer-file buffer)
                        (ignore-errors (shown-file buffer))
                        (tuatara.buffer:buffer-local buffer 'office-reading)))))
    (cond ((tuatara.buffer:buffer-local buffer :plaintext) nil)
          (directory (list :directory 0 directory))
          (file (list (if (tuatara.io:safe-visit-p buffer) :safe :file) point (tuatara.io:native file)))
          ((member (tuatara.buffer:buffer-name buffer) '("*lisp-scratch*" "*org-scratch*" "*scratch*") :test #'string=)
           (list :scratch point (tuatara.buffer:buffer-name buffer))))))

(defun session-lines (editor)
  "The lines of the session of EDITOR, as SESSION-PATH keeps them."
  (let ((lines (list "tuatara session 1")))
    (labels ((out (format &rest arguments) (push (apply #'format nil format arguments) lines))
             (entry-words (entry) (format nil "~(~a~) ~d ~a" (first entry) (second entry) (escape-path (third entry))))
             (area (node)
               (if (typep node 'tuatara.display:split)
                   (let ((children (tuatara.display:split-children node)))
                     (out "split ~(~a~) ~d~{ ~,6f~}" (tuatara.display:split-arrangement node) (length children)
                          (mapcar (lambda (c) (float (tuatara.display:area-share c) 1d0)) children))
                     (mapc #'area children))
                   (let ((entry (session-entry (tuatara.display:window-buffer node))))
                     (if entry
                         (out "window ~:[0~;1~] ~d ~a" (eq node (editor-window editor))
                              (tuatara.display:window-start node)
                              (entry-words (list (first entry) (tuatara.display:window-point node) (third entry))))
                         (out "window ~:[0~;1~] 0 none 0 -" (eq node (editor-window editor))))))))
      ;; the buffers not shown, the one used longest ago first (those never shown before them), so
      ;; that they come back in the same order
      (dolist (buffer (append (remove-if (lambda (b) (member b *buffer-order*)) tuatara.buffer:*buffers*)
                              (reverse (remove-if-not #'live-buffer-p *buffer-order*))))
        (let ((entry (session-entry buffer)))
          (when (and entry (not (buffer-window buffer editor)))
            (out "buffer ~a" (entry-words entry)))))
      (out "frame")
      (area (tuatara.display:area-root (editor-window editor)))
      (out "end"))
    (nreverse lines)))

(defvar *session-written* nil
  "The session's lines as written last, so that one not changed is not written again.")

(defvar *session-lock* (sb-thread:make-mutex :name "session")
  "Held while the session file is written, by the editor's thread or a thread of its own.")

(defvar *session-numbers* (cons 0 0)
  "The number given to the last writing of the session asked for, and that of the last written.")

(defun session-writer (lines)
  "A function writing LINES as the session file, for you alone, unless a writing
asked for after this one has been done already: so that a slow writing never
puts back an older session. It returns NIL when it could not write."
  (let ((number (sb-thread:with-mutex (*session-lock*) (incf (car *session-numbers*)))))
    (lambda ()
      (sb-thread:with-mutex (*session-lock*)
        (when (> number (cdr *session-numbers*))
          (setf (cdr *session-numbers*) number)
          (ignore-errors
           (tuatara.io:write-private-file (session-path) (format nil "~{~a~%~}" lines))
           t))))))

(defvar *session-looked* nil
  "When the session was last looked at to be written (an internal real time), or NIL.")

(defun session-tick (editor)
  "Write the session now and then while the editor runs, every
*KEEP-SESSION-INTERVAL* seconds when it has changed, on a thread of its own;
the seconds until the next look."
  (when *keep-session*
    (let* ((now (get-internal-real-time))
           (due (if *session-looked* (+ *session-looked* (* *keep-session-interval* internal-time-units-per-second)) now)))
      (if (< now due)
          (/ (- due now) internal-time-units-per-second)
          (let ((lines (ignore-errors (session-lines editor))))
            (setf *session-looked* now)
            (when (and lines (not (equal lines *session-written*)))
              (setf *session-written* lines)
              (tuatara.runtime:make-guarded-thread (session-writer lines) :name "tuatara session"))
            *keep-session-interval*)))))

(defun session-at-exit (editor)
  "Write the session as the editor stops (*KEEP-SESSION*)."
  (when *keep-session*
    (let ((lines (ignore-errors (session-lines editor))))
      (when lines (funcall (session-writer lines))))))

;;; Read again

(defun read-session ()
  "The session kept, as two values: the buffers not shown, each (KIND POINT
TARGET), and the windows' layout, a tree of (:SPLIT ARRANGEMENT SHARES .
CHILDREN) and (:WINDOW SELECTED START ENTRY), ENTRY NIL for a window whose
buffer is not kept; NIL when there is none or it is not one this editor
wrote. Read as text: nothing in it is evaluated."
  (let* ((text (ignore-errors (tuatara.io:file-text (session-path) :limit (* 4 1024 1024))))
         (lines (and text (uiop:split-string text :separator '(#\Newline)))))
    (when (and lines (string= (first lines) "tuatara session 1"))
      (let ((rest (rest lines)) (buffers '()))
        (labels ((entry (words)
                   ;; KIND POINT TARGET, the target the rest of the line
                   (destructuring-bind (kind point &rest target) words
                     (let ((kind (cdr (assoc kind '(("file" . :file) ("safe" . :safe) ("directory" . :directory) ("scratch" . :scratch)) :test #'string=)))
                           (point (parse-integer point :junk-allowed t)))
                       (and kind point (>= point 0) target
                            (list kind point (unescape-path (format nil "~{~a~^ ~}" target)))))))
                 (words (line) (uiop:split-string line :separator '(#\Space)))
                 (node (depth)
                   (let ((line (pop rest)))
                     (when (or (null line) (> depth 16)) (error "The session file ends too soon"))
                     (let ((words (words line)))
                       (cond ((string= (first words) "split")
                              (let* ((arrangement (cdr (assoc (second words) '(("stacked" . :stacked) ("side-by-side" . :side-by-side)) :test #'string=)))
                                     (count (parse-integer (third words) :junk-allowed t))
                                     (shares (mapcar #'session-number (nthcdr 3 words))))
                                (unless (and arrangement count (<= 2 count 64) (= count (length shares)) (every #'realp shares))
                                  (error "A split the session file cannot hold"))
                                (list* :split arrangement shares (loop repeat count collect (node (1+ depth))))))
                             ((string= (first words) "window")
                              (let ((start (parse-integer (third words) :junk-allowed t)))
                                (list :window (string= (second words) "1") (or start 0)
                                      (and (not (string= (fourth words) "none")) (entry (nthcdr 3 words))))))
                             (t (error "Not a line of the session file: ~a" line)))))))
          (handler-case
              (progn
                (loop while (and rest (uiop:string-prefix-p "buffer " (first rest)))
                      do (let ((entry (entry (rest (words (pop rest))))))
                           (when entry (push entry buffers))))
                (when (string= (pop rest) "frame")
                  (values (nreverse buffers) (node 0))))
            (error () nil)))))))

(defun session-number (text)
  "TEXT, a number as the session file writes it (0.500000, 12), as a number,
a whole one or a double float (TUATARA.TEXT:PARSE-DECIMAL); NIL for anything
else."
  (let ((n (tuatara.text:parse-decimal text :most-digits 18)))
    (if (or (null n) (integerp n)) n (coerce n 'double-float))))

(defun session-open (entry)
  "Open again the buffer ENTRY of the session (KIND POINT TARGET), point where it
was; NIL when it cannot be (a file no longer there)."
  (destructuring-bind (kind point target) entry
    (let ((buffer (ignore-errors
                   (case kind
                     ((:file :safe) (and (tuatara.io:path-stamp target)
                                         (nth-value 0 (visit-path target :safe (eq kind :safe) :show #'identity))))
                     (:directory (and (tuatara.io:path-stamp target) (nth-value 0 (visit-path target :show #'identity))))
                     (:scratch (tuatara.buffer:find-buffer target))))))
      (when (and buffer (not (eq kind :directory)))
        (setf (tuatara.buffer:buffer-point buffer) (min point (tuatara.buffer:buffer-size buffer))))
      buffer)))

(defvar *session-opened* nil
  "True once the session kept has been opened again in this run of the editor.")

(defvar *session-layout* nil
  "The windows' layout of the session being opened again, for SESSION-LAYOUT-TICK
to make once the editor's first window is there, with the buffers opened for
it: (TREE . BUFFERS), BUFFERS each window's buffer by its entry.")

(defun session-startup-buffer ()
  "The buffer the editor starts with when it starts without files: with
*KEEP-SESSION* the session's, its files and windows opened again; else
STARTUP-BUFFER's."
  (or (and *keep-session*
           (multiple-value-bind (buffers tree) (read-session)
             (when tree
               (let ((missing 0) (opened '()) (selected nil))
                 (dolist (entry buffers)
                   (unless (session-open entry) (incf missing)))
                 (labels ((walk (node)
                            (if (eq (first node) :split)
                                (mapc #'walk (cdddr node))
                                (destructuring-bind (chosen start entry) (rest node)
                                  (declare (ignore start))
                                  (let ((buffer (and entry (session-open entry))))
                                    (when (and entry (null buffer)) (incf missing))
                                    (push (cons node buffer) opened)
                                    (when (and chosen buffer) (setf selected buffer)))))))
                   (walk tree))
                 (setf *session-layout* (cons tree opened) *session-opened* t)
                 (message "Session opened again~[~:;: ~:*~d file~:p no longer there~]" missing)
                 (or selected (cdr (find-if #'cdr opened)))))))
      (startup-buffer)))

(defun session-buffers-behind (editor)
  "Open again, behind the files the editor was started with, the files and
directories of the session kept (*KEEP-SESSION*), its windows left as they
are; so that the session written as it stops still holds them."
  (declare (ignore editor))
  (setf *session-opened* t)
  (multiple-value-bind (buffers tree) (read-session)
    (let ((entries (append buffers
                           (labels ((walk (node) (if (eq (first node) :split)
                                                     (mapcan #'walk (cdddr node))
                                                     (and (fourth node) (list (fourth node))))))
                             (and tree (walk tree)))))
          (opened 0))
      (dolist (entry entries)
        (unless (and (member (first entry) '(:file :safe :directory))
                     (find-if (lambda (b) (equal (session-entry b) entry)) tuatara.buffer:*buffers*))
          (when (session-open entry) (incf opened))))
      (when (plusp opened)
        (message "~d file~:p of the session kept opened again, behind those named" opened)))))

(defun session-layout-tick (editor)
  "Split the editor's windows as the session opened again had them, once, its
first window there; windows that do not fit are left out. Started with files,
the session's files opened behind them instead (SESSION-BUFFERS-BEHIND)."
  (setf *tick-functions* (remove 'session-layout-tick *tick-functions*))
  (when (and *keep-session* (not *session-opened*))
    (ignore-errors (session-buffers-behind editor)))
  (let ((layout (shiftf *session-layout* nil)))
    (when layout
      (destructuring-bind (tree . opened) layout
        (let ((selected nil))
          (labels ((fill-window (window node)
                     (destructuring-bind (chosen start entry) (rest node)
                       (declare (ignore entry))
                       (let ((buffer (cdr (assoc node opened))))
                         (when buffer
                           (tuatara.display:set-window-buffer window buffer)
                           (setf (tuatara.display:window-start window) (min start (tuatara.buffer:buffer-size buffer))
                                 (tuatara.display:window-point window) (tuatara.buffer:buffer-point buffer)))
                         (when chosen (setf selected window)))))
                   (build (window node)
                     (if (eq (first node) :window)
                         (fill-window window node)
                         (destructuring-bind (arrangement shares &rest children) (rest node)
                           (let ((windows (list window)) (side (if (eq arrangement :stacked) :below :right)))
                             (dolist (child (rest children))
                               (declare (ignore child))
                               (let ((new (ignore-errors (split-window (car (last windows)) side))))
                                 (when new (setf windows (append windows (list new))))))
                             ;; the shares as they were, among the windows made
                             (let ((made (subseq shares 0 (length windows))))
                               (loop for w in windows for share in made
                                     do (setf (tuatara.display:area-share w) (/ share (reduce #'+ made)))))
                             (loop for w in windows for child in children do (build w child)))))))
            (ignore-errors (build (editor-window editor) tree)))
          (fit-windows editor)
          (when selected (select-window selected editor))))))
  nil)

(defun install-session ()
  "Have the session kept (*KEEP-SESSION*): written as the editor stops and now
and then, and opened again when it starts without files."
  (setf *session-opened* nil *session-layout* nil *session-looked* nil)
  (pushnew 'session-tick *tick-functions*)
  (pushnew 'session-layout-tick *tick-functions*)
  (pushnew 'session-at-exit *exit-functions*))
