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
;;;; section of its own, the frame worked in first: a session of several
;;;; frames opens them all again, where frames can open (a window).

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
                        (ignoring-errors-reported ("finding the file ~a shows" (tuatara.buffer:buffer-name buffer))
                          (shown-file buffer))
                        (tuatara.buffer:buffer-local buffer 'office-reading)))))
    (cond ((tuatara.io:buffer-plaintext-p buffer) nil)
          (directory (list :directory 0 directory))
          (file (list (if (tuatara.io:safe-visit-p buffer) :safe :file) point (tuatara.io:native file)))
          ((member (tuatara.buffer:buffer-name buffer) '("*lisp-scratch*" "*org-scratch*" "*scratch*") :test #'string=)
           (list :scratch point (tuatara.buffer:buffer-name buffer))))))

(defvar *session-sections-kept* '()
  "The sections of the session read whose frames were not opened again (in a
terminal, started with files, or past *MOST-FRAMES*), each a list of its lines
as written: kept in the session as they were, to be opened where frames can
open.")

(defvar *session-sections-read* 0
  "How many frame sections the session read had: with the frames open, the
session written keeps no more sections than these, or *MOST-FRAMES*.")

(defun session-lines (editor)
  "The lines of the session of EDITOR, as SESSION-PATH keeps them: the buffers
no frame shows, then a section for each frame, the one worked in first, then
the sections read that no frame was opened for (*SESSION-SECTIONS-KEPT*)."
  (let ((lines (list "tuatara session 1")))
    (labels ((out (format &rest arguments) (push (apply #'format nil format arguments) lines))
             (entry-words (entry) (format nil "~(~a~) ~d ~a" (first entry) (second entry) (escape-path (third entry))))
             (area (node selected)
               (if (typep node 'tuatara.display:split)
                   (let ((children (tuatara.display:split-children node)))
                     (out "split ~(~a~) ~d~{ ~,6f~}" (tuatara.display:split-arrangement node) (length children)
                          (mapcar (lambda (c) (float (tuatara.display:area-share c) 1d0)) children))
                     (dolist (child children) (area child selected)))
                   (let ((entry (session-entry (tuatara.display:window-buffer node))))
                     (if entry
                         (out "window ~:[0~;1~] ~d ~a" (eq node selected)
                              (tuatara.display:window-start node)
                              (entry-words (list (first entry) (tuatara.display:window-point node) (third entry))))
                         (out "window ~:[0~;1~] 0 none 0 -" (eq node selected)))))))
      ;; the buffers not shown, the one used longest ago first (those never shown before them), so
      ;; that they come back in the same order
      (dolist (buffer (append (remove-if (lambda (b) (member b *buffer-order*)) tuatara.buffer:*buffers*)
                              (reverse (remove-if-not #'live-buffer-p *buffer-order*))))
        (let ((entry (session-entry buffer)))
          (when (and entry (not (buffer-windows buffer editor)))
            (out "buffer ~a" (entry-words entry)))))
      (dolist (frame (cons (editor-frame editor) (remove (editor-frame editor) (editor-frames editor))))
        (let ((selected (frame-window frame)))
          (out "frame")
          (area (tuatara.display:area-root selected) selected)))
      ;; the frames of the session read that were not opened, as they were, as many as there is room for
      (loop for section in *session-sections-kept*
            repeat (- (max *most-frames* *session-sections-read*) (length (editor-frames editor)))
            do (out "frame")
               (dolist (line section) (out "~a" line)))
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
          (ignoring-errors-reported ("writing the session")
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
          (let ((lines (ignoring-errors-reported ("gathering the session") (session-lines editor))))
            (setf *session-looked* now)
            (when (and lines (not (equal lines *session-written*)))
              (setf *session-written* lines)
              (tuatara.runtime:make-guarded-thread (session-writer lines) :name "tuatara session"))
            *keep-session-interval*)))))

(defun session-at-exit (editor)
  "Write the session as the editor stops (*KEEP-SESSION*)."
  (when *keep-session*
    (let ((lines (ignoring-errors-reported ("gathering the session") (session-lines editor))))
      (when lines (funcall (session-writer lines))))))

;;; Read again

(defun read-session ()
  "The session kept, as four values: the buffers not shown, each (KIND POINT
TARGET); the windows' layout of the frame worked in, a tree of (:SPLIT
ARRANGEMENT SHARES . CHILDREN) and (:WINDOW SELECTED START ENTRY), ENTRY NIL
for a window whose buffer is not kept; the layouts of the other frames, a
list of such trees; and the lines of each of their sections as written, a
list of lists of strings. NIL when there is none or it is not one this editor
wrote. Read as text: nothing in it is evaluated."
  (let* ((text (ignoring-errors-reported ("reading the session")
                 (tuatara.io:file-text (session-path) :limit (* 4 1024 1024))))
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
                     (when (or (null line) (> depth 16)) (refuse "The session file ends too soon"))
                     (let ((words (words line)))
                       (cond ((string= (first words) "split")
                              (let* ((arrangement (cdr (assoc (second words) '(("stacked" . :stacked) ("side-by-side" . :side-by-side)) :test #'string=)))
                                     (count (parse-integer (third words) :junk-allowed t))
                                     (shares (mapcar #'session-number (nthcdr 3 words))))
                                (unless (and arrangement count (<= 2 count 64) (= count (length shares)) (every #'realp shares))
                                  (refuse "A split the session file cannot hold"))
                                (list* :split arrangement shares (loop repeat count collect (node (1+ depth))))))
                             ((string= (first words) "window")
                              (let ((start (parse-integer (third words) :junk-allowed t)))
                                (list :window (string= (second words) "1") (or start 0)
                                      (and (not (string= (fourth words) "none")) (entry (nthcdr 3 words))))))
                             (t (refuse "Not a line of the session file: ~a" line)))))))
          (handler-case
              (reporting-errors ("reading the session")
                (loop while (and rest (uiop:string-prefix-p "buffer " (first rest)))
                      do (let ((entry (entry (rest (words (pop rest))))))
                           (when entry (push entry buffers))))
                (when (string= (pop rest) "frame")
                  (let ((worked-in (node 0)) (others '()) (sections '()))
                    (loop while (equal (first rest) "frame")
                          do (pop rest)
                             (let ((from rest))
                               (push (node 0) others)
                               (push (ldiff from rest) sections)))
                    (values (nreverse buffers) worked-in (nreverse others) (nreverse sections)))))
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
    (let ((buffer (ignoring-errors-reported ("opening ~a again" target)
                    (case kind
                      ((:file :safe) (and (tuatara.io:path-stamp target)
                                          (nth-value 0 (visit-path target :safe (eq kind :safe) :show #'identity))))
                      (:directory (and (tuatara.io:path-stamp target)
                                       (nth-value 0 (visit-path target :show #'identity))))
                      (:scratch (tuatara.buffer:find-buffer target))))))
      (when (and buffer (not (eq kind :directory)))
        (setf (tuatara.buffer:buffer-point buffer) (min point (tuatara.buffer:buffer-size buffer))))
      buffer)))

(defvar *session-opened* nil
  "True once the session kept has been opened again in this run of the editor.")

(defvar *session-layout* nil
  "The windows' layouts of the session being opened again, for
SESSION-LAYOUT-TICK to make once the editor's first window is there, with the
buffers opened for them: a list of (TREE BUFFERS LINES), the first for the
frame worked in, then one for each frame more, BUFFERS each window's buffer by
its entry, LINES the section's lines as written (NIL for the first).")

(defun session-tree-entries (tree)
  "The entries of the windows of TREE, a layout of the session, that have one."
  (if (eq (first tree) :split)
      (mapcan #'session-tree-entries (cdddr tree))
      (and (fourth tree) (list (fourth tree)))))

(defmethod host-startup-buffer ((host defaults-host))
  "The buffer the editor starts with when it starts without files: with
*KEEP-SESSION* the session's, its files and frames' windows opened again, of
*MOST-FRAMES* frames at most, the sections of those past them kept as they
were (*SESSION-SECTIONS-KEPT*); else STARTUP-BUFFER's."
  (or (and *keep-session*
           (multiple-value-bind (buffers tree others sections) (read-session)
             (when tree
               (let ((missing 0) (selected nil) (layouts '())
                     (shown (min (length others) (1- *most-frames*))))
                 (dolist (entry buffers)
                   (unless (session-open entry) (incf missing)))
                 (loop for layout in (cons tree (subseq others 0 shown))
                       for lines in (cons nil sections)
                       do (let ((opened '()))
                            (labels ((walk (node)
                                       (if (eq (first node) :split)
                                           (mapc #'walk (cdddr node))
                                           (destructuring-bind (chosen start entry) (rest node)
                                             (declare (ignore start))
                                             (let ((buffer (and entry (session-open entry))))
                                               (when (and entry (null buffer)) (incf missing))
                                               (push (cons node buffer) opened)
                                               (when (and chosen buffer (eq layout tree)) (setf selected buffer)))))))
                              (walk layout))
                            (push (list layout opened lines) layouts)))
                 (setf layouts (nreverse layouts)
                       *session-layout* layouts
                       *session-sections-read* (1+ (length others))
                       *session-sections-kept* (nthcdr shown sections)
                       *session-opened* t)
                 (message "Session opened again~[~:;: ~:*~d file~:p no longer there~]" missing)
                 (or selected (cdr (find-if #'cdr (second (first layouts)))))))))
      (startup-buffer)))

(defun session-buffers-behind (editor)
  "Open again, behind the files the editor was started with, the files and
directories of the session kept (*KEEP-SESSION*), every frame's, its windows
left as they are; so that the session written as it stops still holds them,
the other frames' sections as they were (*SESSION-SECTIONS-KEPT*)."
  (declare (ignore editor))
  (setf *session-opened* t)
  (multiple-value-bind (buffers tree others sections) (read-session)
    ;; no frame opened for the other frames: their sections kept as they were
    (setf *session-sections-kept* sections
          *session-sections-read* (if tree (1+ (length others)) 0))
    (let ((entries (append buffers
                           (and tree (mapcan #'session-tree-entries
                                             (cons tree (subseq others 0 (min (length others) (1- *most-frames*))))))))
          (opened 0))
      (dolist (entry entries)
        (unless (and (member (first entry) '(:file :safe :directory))
                     (find-if (lambda (b) (equal (session-entry b) entry)) tuatara.buffer:*buffers*))
          (when (session-open entry) (incf opened))))
      (when (plusp opened)
        (message "~d file~:p of the session kept opened again, behind those named" opened)))))

(defun session-build-layout (editor tree opened)
  "Split the windows of EDITOR's frame worked in as TREE, a layout of the
session, has them, each window showing its buffer of OPENED as
*SESSION-LAYOUT* has them; windows that do not fit are left out. The window
that was selected is selected again."
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
                         (let ((new (ignoring-errors-reported ("splitting a window")
                                      (split-window (car (last windows)) side))))
                           (when new (setf windows (append windows (list new))))))
                       ;; the shares as they were, among the windows made
                       (let ((made (subseq shares 0 (length windows))))
                         (loop for w in windows for share in made
                               do (setf (tuatara.display:area-share w) (/ share (reduce #'+ made)))))
                       (loop for w in windows for child in children do (build w child)))))))
      (ignoring-errors-reported ("laying out the session's windows") (build (editor-window editor) tree)))
    (fit-windows editor)
    (when selected (select-window selected editor))))

(defun session-layout-tick (editor)
  "Split the windows of the editor's frames as the session opened again had
them, once, its first window there: the frame worked in first, then the
frames opened at the start, then frames opened for those left, where frames
can open (a window); in a terminal their files are open behind and their
sections kept as they were (*SESSION-SECTIONS-KEPT*). Started with files, the
session's files opened behind them instead (SESSION-BUFFERS-BEHIND)."
  (setf *tick-functions* (remove 'session-layout-tick *tick-functions*))
  (when (and *keep-session* (not *session-opened*))
    (ignoring-errors-reported ("opening the session's files") (session-buffers-behind editor)))
  (let ((layouts (shiftf *session-layout* nil))
        (worked-in (editor-frame editor)))
    (when layouts
      (destructuring-bind (tree opened lines) (first layouts)
        (declare (ignore lines))
        (session-build-layout editor tree opened))
      (let ((others (remove worked-in (editor-frames editor))))
        (loop for (layout . left) on (rest layouts)
              for frame = (or (pop others)
                              (and (< (length (editor-frames editor)) *most-frames*)
                                   (handler-case (open-frame editor :select nil)
                                     (tuatara.runtime:refusal () nil)))
                              ;; no frame for it (a terminal, or frames enough): the sections left kept as they were
                              (progn (setf *session-sections-kept* (append (mapcar #'third (cons layout left))
                                                                           *session-sections-kept*))
                                     (return)))
              do (destructuring-bind (tree opened lines) layout
                   (declare (ignore lines))
                   (call-in-frame editor frame (lambda () (session-build-layout editor tree opened))))))))
  nil)

(defun install-session ()
  "Have the session kept (*KEEP-SESSION*): written as the editor stops and now
and then, and opened again when it starts without files."
  (setf *session-opened* nil *session-layout* nil *session-looked* nil *session-sections-kept* '()
        *session-sections-read* 0)
  (pushnew 'session-tick *tick-functions*)
  (pushnew 'session-layout-tick *tick-functions*)
  (pushnew 'session-at-exit *exit-functions*))
