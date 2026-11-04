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
;;;; frames opens them all again, as windows or in the terminal.

(in-package #:tuatara.defaults)

(defvar *keep-session* nil
  "True for the editor to keep its session: the files and directories open and
how the windows show them, written as it stops (and now and then while it
runs), opened again as it was when it starts without files.")

(defvar *keep-session-interval* 30
  "Seconds between the writings of the session while the editor runs, when it
has changed (*KEEP-SESSION*): what a crash can lose.")

(defvar *session-name* "session"
  "The name of the file in the state folder the session is kept in: session,
or session-daemon-NAME for the daemon NAME, whose session is its own.")

(defun session-path ()
  "The file the session is kept in: *SESSION-NAME* in the state folder."
  (tuatara.io:path-join (tuatara.io:native (tuatara.io:state-directory)) *session-name*))

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

(defun session-entry-words (entry)
  "ENTRY of the session, (KIND POINT TARGET), as its line writes it."
  (format nil "~(~a~) ~d ~a" (first entry) (second entry) (escape-path (third entry))))

(defun frame-layout-lines (frame)
  "The lines of the session laying out FRAME's windows, as a frame section
holds them after its frame line: a split line for each split, before its
parts, and a window line for each window, with its buffer's entry
\(SESSION-ENTRY), or none."
  (let ((lines '()) (selected (frame-window frame)))
    (labels ((out (format &rest arguments) (push (apply #'format nil format arguments) lines))
             (area (node)
               (if (typep node 'tuatara.display:split)
                   (let ((children (tuatara.display:split-children node)))
                     (out "split ~(~a~) ~d~{ ~,6f~}" (tuatara.display:split-arrangement node) (length children)
                          (mapcar (lambda (c) (float (tuatara.display:area-share c) 1d0)) children))
                     (mapc #'area children))
                   (let ((entry (session-entry (tuatara.display:window-buffer node))))
                     (if entry
                         (out "window ~:[0~;1~] ~d ~a" (eq node selected) (tuatara.display:window-start node)
                              (session-entry-words (list (first entry) (tuatara.display:window-point node)
                                                         (third entry))))
                         (out "window ~:[0~;1~] 0 none 0 -" (eq node selected)))))))
      (area (tuatara.display:area-root selected)))
    (nreverse lines)))

(defvar *session-sections-kept* '()
  "The sections of the session read whose frames were not opened again
\(started with files, or past *MOST-FRAMES*), each a list of its lines as
written: kept in the session as they were, to be opened at a later start.")

(defvar *session-sections-read* 0
  "How many frame sections the session read had: with the frames open, the
session written keeps no more sections than these, or *MOST-FRAMES*.")

(defvar *client-layouts* '()
  "The layouts of the frames of the clients that ended, with *KEEP-SESSION*,
each (CLIENT . SECTIONS), SECTIONS the lines of each frame's section
\(FRAME-LAYOUT-LINES): kept in the session with the client's folder, and
taken up with it by a client that attaches with no files
\(TAKE-UP-ENDED-FOLDER).")

(defvar *evidence-waiting* '()
  "The files opened safely (evidence) that the session of an editor running
in the background lists and that were left closed as it started, each
\(OWNER . ENTRIES): OWNER a client that ended, whose folder they wait in, or
:EVERYONE for those no client had; ENTRIES as the session writes them. They
are opened, safely, when a client takes the folder up (TAKE-UP-ENDED-FOLDER),
and kept in the session until then.")

(defun waiting-evidence (owner)
  "The entries of the files opened safely that wait closed in the folder of
OWNER, a client or :EVERYONE (*EVIDENCE-WAITING*), but for those opened since."
  (remove-if #'session-entry-buffer (cdr (assoc owner *evidence-waiting*))))

(defun remote-client-p (client)
  "True when CLIENT showed, or shows, an editor running in the background from
a program of its own."
  (and client (client-remote client)))

(defun layout-shows-entry-p (sections)
  "True when one of SECTIONS, a frame's lines each (FRAME-LAYOUT-LINES), has a
window showing a buffer the session keeps."
  (some (lambda (lines)
          (some (lambda (line) (and (uiop:string-prefix-p "window " line) (not (search " 0 none 0 -" line))))
                lines))
        sections))

(defun session-lines (editor)
  "The lines of the session of EDITOR, as SESSION-PATH keeps them: the buffers
no frame shows and no client has, then a section for each frame of the
editor's own (no client's), the one worked in first, then the sections read
that no frame was opened for (*SESSION-SECTIONS-KEPT*). Then, in an editor
running in the background, a section for each client that has buffers kept:
a client line (its number, when it began and ended, its folder), the buffers
of its own its frames do not show, and a frame section for each of its
frames, or for those it had as it ended (*CLIENT-LAYOUTS*). The files opened
safely that wait closed (*EVIDENCE-WAITING*) are kept with the buffers of
their folder."
  (let* ((lines (list "tuatara session 1"))
         ;; the buffers, the one used longest ago first (those never shown before them), so that they come
         ;; back in the same order
         (ordered (append (remove-if (lambda (b) (member b *buffer-order*)) tuatara.buffer:*buffers*)
                          (reverse (remove-if-not #'live-buffer-p *buffer-order*))))
         (frames (cons (editor-frame editor) (remove (editor-frame editor) (editor-frames editor))))
         (own (remove-if (lambda (frame) (remote-client-p (frame-client frame))) frames)))
    (labels ((out (format &rest arguments) (push (apply #'format nil format arguments) lines))
             (section (layout) (out "frame") (dolist (line layout) (out "~a" line))))
      (dolist (buffer ordered)
        (let ((entry (session-entry buffer)))
          (when (and entry (null (buffer-clients buffer)) (not (buffer-windows buffer editor)))
            (out "buffer ~a" (session-entry-words entry)))))
      (dolist (entry (waiting-evidence :everyone))
        (out "buffer ~a" (session-entry-words entry)))
      (dolist (frame own)
        (section (frame-layout-lines frame)))
      ;; the frames of the session read that were not opened, as they were, as many as there is room for
      (loop for kept in *session-sections-kept*
            repeat (- (max *most-frames* *session-sections-read*) (length own))
            do (section kept))
      (dolist (client (editor-clients editor))
        (when (remote-client-p client)
          (let* ((frames (client-frames editor client))
                 (shown (loop for frame in frames
                              append (mapcar #'tuatara.display:window-buffer
                                             (tuatara.display:area-windows
                                              (tuatara.display:area-root (frame-window frame))))))
                 (layouts (if frames
                              (mapcar #'frame-layout-lines frames)
                              (cdr (assoc client *client-layouts*))))
                 (entries (append (loop for buffer in ordered
                                        for entry = (and (member client (buffer-clients buffer))
                                                         (not (member buffer shown))
                                                         (session-entry buffer))
                                        when entry collect entry)
                                  (waiting-evidence client))))
            (when (or entries (layout-shows-entry-p layouts))
              (out "client ~d ~d ~d ~a" (client-number client) (client-began client)
                   (or (client-ended client) (get-universal-time))
                   (if (client-directory client) (escape-path (client-directory client)) "-"))
              (dolist (entry entries) (out "buffer ~a" (session-entry-words entry)))
              (mapc #'section layouts)))))
      (out "end"))
    (nreverse lines)))

(defvar *session-written* nil
  "The session's lines as written last, so that one not changed is not written again.")

(defvar *session-lock* (sb-thread:make-mutex :name "session")
  "Held while the session file is written, by the editor's thread or a thread of its own.")

(defvar *session-numbers* (cons 0 0)
  "The number given to the last writing of the session asked for, and that of the last written.")

(defun session-writer (lines)
  "A function writing LINES as the session file (SESSION-PATH, as it is when
this is called), for you alone, unless a writing asked for after this one has
been done already: so that a slow writing never puts back an older session.
It returns NIL when it could not write."
  (let ((number (sb-thread:with-mutex (*session-lock*) (incf (car *session-numbers*))))
        (path (session-path)))
    (lambda ()
      (sb-thread:with-mutex (*session-lock*)
        (when (> number (cdr *session-numbers*))
          (setf (cdr *session-numbers*) number)
          (ignoring-errors-reported ("writing the session")
            (tuatara.io:write-private-file path (format nil "~{~a~%~}" lines))
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

(defun session-words (line)
  "The words of LINE of the session, split at each space."
  (uiop:split-string line :separator '(#\Space)))

(defun session-line-entry (words)
  "The entry of the session WORDS write, KIND POINT TARGET, the target the
rest of the line, as (KIND POINT TARGET); NIL when they write none."
  (destructuring-bind (kind point &rest target) words
    (let ((kind (cdr (assoc kind '(("file" . :file) ("safe" . :safe) ("directory" . :directory) ("scratch" . :scratch))
                            :test #'string=)))
          (point (parse-integer point :junk-allowed t)))
      (and kind point (>= point 0) target
           (list kind point (unescape-path (format nil "~{~a~^ ~}" target)))))))

(defun session-layout (lines)
  "The layout of a frame's windows that LINES of the session begin with, a
tree of (:SPLIT ARRANGEMENT SHARES . CHILDREN) and (:WINDOW SELECTED START
ENTRY), ENTRY NIL for a window whose buffer is not kept; the lines after it
as a second value. Refused when they hold no layout."
  (labels ((node (depth)
             (let ((line (pop lines)))
               (when (or (null line) (> depth 16)) (refuse "The session file ends too soon"))
               (let ((words (session-words line)))
                 (cond ((string= (first words) "split")
                        (let* ((arrangement (cdr (assoc (second words) '(("stacked" . :stacked)
                                                                         ("side-by-side" . :side-by-side))
                                                        :test #'string=)))
                               (count (parse-integer (third words) :junk-allowed t))
                               (shares (mapcar #'session-number (nthcdr 3 words))))
                          (unless (and arrangement count (<= 2 count 64) (= count (length shares))
                                       (every #'realp shares))
                            (refuse "A split the session file cannot hold"))
                          (list* :split arrangement shares (loop repeat count collect (node (1+ depth))))))
                       ((string= (first words) "window")
                        (let ((start (parse-integer (third words) :junk-allowed t)))
                          (list :window (string= (second words) "1") (or start 0)
                                (and (not (string= (fourth words) "none")) (session-line-entry (nthcdr 3 words))))))
                       (t (refuse "Not a line of the session file: ~a" line)))))))
    (let ((tree (node 0)))
      (values tree lines))))

(defun read-session ()
  "The session kept, as five values: the buffers not shown, each (KIND POINT
TARGET); the windows' layout of the frame worked in, as SESSION-LAYOUT reads
it, or NIL; the layouts of the other frames, a list of such trees; the lines
of each of their sections as written, a list of lists of strings; and the
clients of an editor running in the background, each a property list of
:NUMBER, :BEGAN, :ENDED (universal times), :DIRECTORY (or NIL), :ENTRIES,
the buffers of its own not shown, and :LAYOUTS, the lines of each of its
frames' sections. NIL when there is none or it is not one this editor wrote.
Read as text: nothing in it is evaluated."
  (let* ((text (ignoring-errors-reported ("reading the session")
                 (tuatara.io:file-text (session-path) :limit (* 4 1024 1024))))
         (lines (and text (uiop:split-string text :separator '(#\Newline)))))
    (when (and lines (string= (first lines) "tuatara session 1"))
      (let ((rest (rest lines)))
        (labels ((entries ()
                   (let ((found '()))
                     (loop while (and rest (uiop:string-prefix-p "buffer " (first rest)))
                           do (let ((entry (session-line-entry (rest (session-words (pop rest))))))
                                (when entry (push entry found))))
                     (nreverse found)))
                 (sections ()
                   ;; each frame section's layout, and its lines as written
                   (loop while (equal (first rest) "frame")
                         collect (let ((start (setf rest (rest rest))))
                                   (multiple-value-bind (tree after) (session-layout start)
                                     (setf rest after)
                                     (cons tree (ldiff start after)))))))
          (handler-case
              (reporting-errors ("reading the session")
                (let* ((buffers (entries))
                       (frames (sections))
                       (clients (loop while (and rest (uiop:string-prefix-p "client " (first rest)))
                                      collect (destructuring-bind (number began ended &rest directory)
                                                  (rest (session-words (pop rest)))
                                                (let ((number (parse-integer number :junk-allowed t))
                                                      (began (parse-integer began :junk-allowed t))
                                                      (ended (parse-integer ended :junk-allowed t))
                                                      (directory (format nil "~{~a~^ ~}" directory)))
                                                  (unless (and number began ended (plusp number))
                                                    (refuse "A client the session file cannot hold"))
                                                  (list :number number :began began :ended ended
                                                        :directory (and (string/= directory "-")
                                                                        (unescape-path directory))
                                                        :entries (entries)
                                                        :layouts (mapcar #'cdr (sections))))))))
                  (when (or frames clients)
                    (values buffers (car (first frames)) (mapcar #'car (rest frames)) (mapcar #'cdr (rest frames))
                            clients))))
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
were (*SESSION-SECTIONS-KEPT*); else STARTUP-BUFFER's. An editor running in
the background opens its clients' folders again instead, as ended
\(OPEN-ENDED-FOLDERS), and starts with STARTUP-BUFFER's."
  (or (and *keep-session*
           (if (editor-idle-frontend *editor*)
               (progn (open-ended-folders *editor*) nil)
               (open-session-frames)))
      (startup-buffer)))

(defun open-session-frames ()
  "Open again the session kept, as the editor starts without files
\(HOST-STARTUP-BUFFER): its files and frames' windows, of *MOST-FRAMES*
frames at most, laid out once the first window is there (*SESSION-LAYOUT*),
the sections of those past them kept as they were (*SESSION-SECTIONS-KEPT*).
The buffer the frame worked in shows, or NIL when there is no session."
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
frames opened at the start, then frames opened for those left, up to
*MOST-FRAMES*; the sections past them, or from one that would not open, kept
as they were (*SESSION-SECTIONS-KEPT*). Started with files, the session's files opened
behind them instead (SESSION-BUFFERS-BEHIND)."
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
                                   (ignoring-errors-reported ("opening a frame of the session")
                                     (open-frame editor :select nil)))
                              ;; no room for more frames, or one would not open: the sections left kept as they were
                              (progn (setf *session-sections-kept* (append (mapcar #'third (cons layout left))
                                                                           *session-sections-kept*))
                                     (return)))
              do (destructuring-bind (tree opened lines) layout
                   (declare (ignore lines))
                   (call-in-frame editor frame (lambda () (session-build-layout editor tree opened))))))))
  nil)

;;; The clients of an editor running in the background

(defun open-ended-folders (editor)
  "Open again the session of EDITOR, running in the background, kept in a
file of its own: the buffers no client had, and for each client a folder of
its own, as one that ended, with its buffers and those its frames showed, and
its frames' layouts kept (*CLIENT-LAYOUTS*) for a client that attaches with
no files to take up (TAKE-UP-ENDED-FOLDER). The files opened safely
(evidence) are not opened: they wait closed in their folder
(*EVIDENCE-WAITING*), said by how many in *Messages* and the daemon's log.
The clients are numbered from 1 again, in the order they began."
  (setf *session-opened* t)
  (multiple-value-bind (buffers tree others sections clients) (read-session)
    (declare (ignore tree others sections))
    (let ((missing 0) (folders 0) (waiting 0))
      (flet ((open-or-wait (entries owner)
               ;; the buffers opened; the evidence left closed, waiting in OWNER's folder
               (let ((opened '()) (evidence '()))
                 (dolist (entry (remove-duplicates entries :test (lambda (a b) (and (eq (first a) (first b))
                                                                                   (equal (third a) (third b))))))
                   (if (eq (first entry) :safe)
                       (push entry evidence)
                       (let ((buffer (session-open entry)))
                         (if buffer (push buffer opened) (incf missing)))))
                 (when evidence
                   (push (cons owner (nreverse evidence)) *evidence-waiting*)
                   (incf waiting (length evidence)))
                 opened)))
        (open-or-wait buffers :everyone)
        (loop for kept in (stable-sort (copy-list clients) #'< :key (lambda (kept) (getf kept :began)))
              for number from 1
              do (let ((client (make-client :remote t :number number :directory (getf kept :directory)))
                       (layouts (getf kept :layouts)))
                   (setf (client-began client) (getf kept :began)
                         (editor-clients editor) (append (editor-clients editor) (list client)))
                   (dolist (buffer (open-or-wait (append (getf kept :entries)
                                                         (loop for lines in layouts
                                                               append (session-tree-entries (session-layout lines))))
                                                 client))
                     (join-client buffer client))
                   (setf (client-ended client) (getf kept :ended))
                   (when layouts (push (cons client layouts) *client-layouts*))
                   (incf folders))))
      (message "Session opened again: ~d folder~:p of clients that ended~[~:;, ~:*~d file~:p no longer there~]"
               folders missing)
      (when (plusp waiting)
        (say-in-daemon-log "~d file~:p opened safely (evidence) wait~:[~;s~] closed in the folders of the session, ~
                            opened only as a client takes a folder up"
                           waiting (= waiting 1))))))

(defun keep-client-layout (client)
  "Client end function (*CLIENT-END-FUNCTIONS*): with *KEEP-SESSION*, keep the
layouts of the frames of CLIENT, one of an editor running in the background,
as it ends (*CLIENT-LAYOUTS*), for the session to keep with its folder and a
client that attaches with no files to take up."
  (let ((editor (or *editor* (running-editor))))
    (when (and *keep-session* editor (remote-client-p client))
      (let ((frames (client-frames editor client)))
        (when frames
          (setf *client-layouts* (acons client (mapcar #'frame-layout-lines frames)
                                        (remove client *client-layouts* :key #'car))))))))

(pushnew 'keep-client-layout *client-end-functions*)

(defun session-entry-buffer (entry)
  "The buffer open now that ENTRY of the session names, by its kind and
target, or NIL: none is opened for it."
  (find-if (lambda (buffer)
             (let ((kept (session-entry buffer)))
               (and kept (eq (first kept) (first entry)) (equal (third kept) (third entry)))))
           tuatara.buffer:*buffers*))

(defun ended-later-p (a b)
  "True when the client A ended after the client B; of two that ended in the
same second, when A began after B."
  (or (> (client-ended a) (client-ended b))
      (and (= (client-ended a) (client-ended b)) (> (client-number a) (client-number b)))))

(defun take-up-ended-folder (editor client &optional folder)
  "Have CLIENT, of EDITOR, take up FOLDER, the client of a folder that ended,
or with *KEEP-SESSION*, when CLIENT has just attached with no files and FOLDER
is not given, the folder of the client that ended last whose frames' layouts
are kept (*CLIENT-LAYOUTS*) or whose files opened safely wait closed
\(*EVIDENCE-WAITING*): those files are opened, safely, and said by how many;
its buffers become CLIENT's, its folder goes, and CLIENT's frame, and a frame
more for each other it had, up to *MOST-FRAMES*, are laid out as its were,
each window showing its buffer where that is still open; a client in windows
of its own is asked for those windows, laid out as they come (OPEN-FRAME).
True when one was taken up."
  (let ((ended (or (and folder (client-ended folder) (not (eq folder client)) folder)
                   (and (null folder) *keep-session*
                        (first (sort (remove-if-not #'client-ended
                                                    (remove-duplicates
                                                     (append (mapcar #'car *client-layouts*)
                                                             (remove :everyone (mapcar #'car *evidence-waiting*)))))
                                     #'ended-later-p))))))
    (when ended
      (let ((layouts (cdr (assoc ended *client-layouts*)))
            (evidence (waiting-evidence ended))
            (opened 0))
        (setf *client-layouts* (remove ended *client-layouts* :key #'car)
              *evidence-waiting* (remove ended *evidence-waiting* :key #'car)
              (editor-clients editor) (remove ended (editor-clients editor)))
        (dolist (buffer tuatara.buffer:*buffers*)
          (let ((clients (buffer-clients buffer)))
            (when (member ended clients)
              (setf (tuatara.buffer:buffer-local buffer :clients) (adjoin client (remove ended clients))))))
        (dolist (entry evidence)
          (let ((buffer (session-open entry)))
            (when buffer
              (join-client buffer client)
              (incf opened))))
        (when evidence
          (message "~d file~:p of the folder taken up opened safely (evidence)~[~:;, ~:*~d no longer there~]"
                   opened (- (length evidence) opened)))
        (flet ((lay-out (lines)
                 ;; a function of a frame laying it out as LINES, its section of the session, had it
                 (lambda (frame)
                   (let* ((tree (session-layout lines))
                          (opened (let ((pairs '()))
                                    (labels ((walk (node)
                                               (if (eq (first node) :split)
                                                   (mapc #'walk (cdddr node))
                                                   (push (cons node (and (fourth node)
                                                                         (session-entry-buffer (fourth node))))
                                                         pairs))))
                                      (walk tree))
                                    pairs)))
                     (call-in-frame editor frame (lambda () (session-build-layout editor tree opened)))))))
          (when layouts
            (funcall (lay-out (first layouts)) (editor-frame editor)))
          ;; the others in frames opened for them: in a window client's windows as they come
          (loop for lines in (rest layouts)
                for frames from (length (editor-frames editor))
                while (< frames *most-frames*)
                do (open-frame editor :select nil :then (lay-out lines))))
        t))))

(defun install-session ()
  "Have the session kept (*KEEP-SESSION*): written as the editor stops and now
and then, and opened again when it starts without files."
  (setf *session-opened* nil *session-layout* nil *session-looked* nil *session-sections-kept* '()
        *session-sections-read* 0 *client-layouts* '() *evidence-waiting* '())
  (pushnew 'session-tick *tick-functions*)
  (pushnew 'session-layout-tick *tick-functions*)
  (pushnew 'session-at-exit *exit-functions*))
