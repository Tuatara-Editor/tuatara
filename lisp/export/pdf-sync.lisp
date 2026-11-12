;;;; lisp/export/pdf-sync.lisp --- A line of a LaTeX or Org file shown in its PDF, and a place of the PDF in the file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A PDF made by export-latex-compile-pdf comes with a SyncTeX file among the
;;;; working files (LATEX-SYNCTEX-FILE). In a LaTeX buffer C-c C-v
;;;; (show-line-in-pdf) shows the line at point in the PDF's view, beside;
;;;; in the view, C-c C-v (pdf-view-show-source) shows the line of the file
;;;; that made the part of the page shown, and a double click on the page the
;;;; line of that point. The SyncTeX file is read once in the background, with
;;;; the PDF's page boxes, and kept until either changes. A PDF exported from an
;;;; Org file in this session goes through the Org export's map of its lines
;;;; too (ERROR-PLACE), so that both commands work from the Org file.

(in-package #:tuatara.export)

(defparameter *pdf-sync-kept* 4
  "How many PDFs' SyncTeX files are kept read, the last used first, within
*PDF-SYNC-KEPT-OCTETS*.")

(defparameter *pdf-sync-kept-octets* (* 256 1024 1024)
  "The most memory the SyncTeX files kept read may take together: past it
those used longest ago are let go of, the last one read always kept.")

(defparameter *pdf-sync-pdf-largest* (* 256 1024 1024)
  "The most octets of a PDF read for the boxes of its pages beside its SyncTeX
file: past them its first page's size, as its program reports it, is taken
for every page, from the page's top left, since that report does not say
where a crop box begins.")

(defvar *pdf-sync-read* '()
  "The SyncTeX files read, the last used first, each (PATH STAMP PDF-STAMP
SYNCTEX): its path and stamp, the stamp of the PDF beside the LaTeX file, and
what was read (TUATARA.DEFAULTS::READ-SYNCTEX), or why it could not be, in
words, until either file changes.")

(defvar *pdf-sync-reading* '()
  "The SyncTeX files being read in the background, each (PATH JOB . THEN):
the job reading it, and THEN, the function last asked for what it says
\(PDF-SYNC-DATA). An entry whose job ended other than done (stopped, failed,
its view killed) no longer counts.")

(defvar *org-pdf-exports* (make-hash-table :test 'equal)
  "The last export of each PDF made from an Org file in this session, by the
PDF's path, as much of the PDF-EXPORT as leads from the Org file to the LaTeX
and back: its source, LaTeX file, PDF, text, map and origins. Dropped when the
PDF is made another way or the Org file's buffer is killed.")

(defun note-pdf-export (job)
  "Keep what an Org file's export JOB that made a PDF needs for its lines to
be shown in the PDF and back; forget it for a PDF made another way."
  (let ((pdf (pdf-export-pdf job)))
    (when (and (eq (pdf-export-status job) :done) pdf)
      (if (eq (pdf-export-kind job) :org)
          (let ((kept (make-pdf-export nil (pdf-export-source job) :org nil nil nil nil)))
            (setf (pdf-export-tex kept) (pdf-export-tex job) (pdf-export-pdf kept) pdf
                  (pdf-export-text kept) (pdf-export-text job) (pdf-export-map kept) (pdf-export-map job)
                  (pdf-export-origins kept) (pdf-export-origins job)
                  (gethash pdf *org-pdf-exports*) kept))
          (remhash pdf *org-pdf-exports*)))))

(defmethod tuatara.runtime:job-take-in :after ((job pdf-export))
  "Keep an Org file's export that made a PDF, for its lines to be shown in the PDF and back (NOTE-PDF-EXPORT)."
  (note-pdf-export job))

(defun forget-org-pdf-exports (buffer)
  "Forget the exports of the Org file BUFFER visits, when it is killed."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (when file
      (let ((path (tuatara.io:native (tuatara.io:expand-path file))))
        (loop for pdf being the hash-keys of *org-pdf-exports* using (hash-value kept)
              when (equal (pdf-export-source kept) path) collect pdf into gone
              finally (dolist (pdf gone) (remhash pdf *org-pdf-exports*)))))))

(defun org-export-current-p (kept buffer)
  "True when BUFFER's text is still what KEPT, an Org file's export, was
made of: the whole text, or for a part exported alone each of its lines
where it came from."
  (let ((text (pdf-export-text kept)) (origins (pdf-export-origins kept)))
    (if (null origins)
        (string= text (tuatara.text:rope-string (tuatara.buffer:buffer-text buffer)))
        (let ((lines (uiop:split-string text :separator '(#\Newline)))
              (count (1+ (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-size buffer)))))
          (and (= (length lines) (length origins))
               (loop for line in lines
                     for origin across origins
                     always (and (<= 1 origin count)
                            (let ((start (tuatara.buffer:buffer-goto-line buffer (1- origin))))
                              (string= line (tuatara.buffer:buffer-string
                                             buffer :start start
                                                    :end (tuatara.buffer:buffer-line-end buffer start)))))))))))

(defun pdf-sync-take-in (synctex stamp pdf pdf-stamp result then)
  "Take in RESULT, what reading the SyncTeX file SYNCTEX beside PDF gave (a
SYNCTEX or why not, in words), read when they had STAMP and PDF-STAMP: kept
and handed to THEN as PDF-SYNC-DATA says; dropped, and that said, when
either file changed meanwhile (the PDF made again)."
  (setf *pdf-sync-reading* (remove synctex *pdf-sync-reading* :key #'car :test #'equal))
  (let ((now (tuatara.io:path-stamp synctex)) (pdf-now (tuatara.io:path-stamp pdf)))
    (if (not (and stamp pdf-stamp now pdf-now
                  (tuatara.io:same-content-p stamp now) (tuatara.io:same-content-p pdf-stamp pdf-now)))
        (message "~a changed while where its lines are was read: ask again" (tuatara.io:native-file-name pdf))
        (let ((kept (cons (list synctex stamp pdf-stamp result)
                          (remove synctex *pdf-sync-read* :key #'first :test #'equal))))
          (setf *pdf-sync-read*
                (loop for entry in kept
                      for count from 0
                      sum (if (stringp (fourth entry)) 0 (synctex-octets (fourth entry))) into octets
                      while (or (zerop count) (and (< count *pdf-sync-kept*) (<= octets *pdf-sync-kept-octets*)))
                      collect entry))
          (if (stringp result) (funcall then nil result) (funcall then result nil))))))

(defun pdf-sync-data (buffer synctex then)
  "Call THEN with what the SyncTeX file SYNCTEX says, read with the page boxes
of the PDF BUFFER views, and NIL; or with NIL and why it could not be read,
in words. At once when it was read, or failed to be, since it and the PDF
last changed; else once it is read, in the background (a job of BUFFER, said
in the echo area meanwhile), one read of a file at a time, THEN the last
asked. THEN runs on the editor's thread."
  (let* ((view (pdf-view-of buffer))
         (pdf (pdf-view-path view))
         (stamp (tuatara.io:path-stamp synctex))
         (pdf-stamp (tuatara.io:path-stamp pdf))
         (kept (find synctex *pdf-sync-read* :key #'first :test #'equal))
         (reading (let ((entry (assoc synctex *pdf-sync-reading* :test #'equal)))
                    ;; a read that ended without its answer (stopped, failed) is forgotten
                    (if (and entry (member (tuatara.runtime:job-status (second entry)) '(:running :done)))
                        entry
                        (progn (setf *pdf-sync-reading* (remove entry *pdf-sync-reading*)) nil))))
         (name (tuatara.io:native-file-name pdf)))
    (cond ((and kept stamp pdf-stamp
                (tuatara.io:same-content-p stamp (second kept)) (tuatara.io:same-content-p pdf-stamp (third kept)))
           (setf *pdf-sync-read* (cons kept (remove kept *pdf-sync-read*)))
           (if (stringp (fourth kept)) (funcall then nil (fourth kept)) (funcall then (fourth kept) nil)))
          (reading
           (setf (cddr reading) then)
           (message "Still reading where the lines are in ~a..." name))
          (t
           (let* ((size (page-size (pdf-view-info view)))
                  ;; a PDF too large to read for its pages' boxes: its first page's size for every page
                  (first-page (and size (let ((box (list 0 0 (first size) (second size)))) (vector (list box box 0))))))
             (push (list* synctex
                          (pdf-view-task
                           view buffer (format nil "Reading where the lines are in ~a" name)
                           (lambda (fd)
                             (handler-case
                                 (let ((octets (tuatara.io:fd-octets fd pdf
                                                                     (tuatara.io:stamp-size (tuatara.io:fd-stamp fd pdf))
                                                                     *pdf-sync-pdf-largest* (constantly nil))))
                                   (or (read-synctex (tuatara.io:file-octets synctex :limit *synctex-largest*)
                                                     :boxes (or (and octets (pdf-page-boxes octets)) first-page))
                                       "it is no SyncTeX file"))
                               (tuatara.io:file-system-error (e) (tuatara.io:file-system-error-reason e))))
                           (lambda (result)
                             (let ((then (cddr (assoc synctex *pdf-sync-reading* :test #'equal))))
                               (pdf-sync-take-in synctex stamp pdf pdf-stamp result (or then (constantly nil)))))
                           :quiet t)
                          then)
                   *pdf-sync-reading*))
           (message "Reading where the lines are in ~a..." name)))))

(defun source-to-show (file synctex tex source)
  "Where FILE, an input SYNCTEX names, is, when it may be opened: its name
taken from the directory of SYNCTEX when it is not a full one, a regular file
within the directory of TEX, or SOURCE, the Org file it was exported from,
by where it really is, every link followed, as it was checked.
NIL for anything else, which a document may name as it reads any file the
sandbox shows it."
  (let* ((path (synctex-path (if (uiop:string-prefix-p "/" file)
                                 file
                                 (tuatara.io:path-join (tuatara.io:parent-path synctex) file))))
         (real (tuatara.io:real-path-or-parent path)))
    (and (eq (tuatara.io:path-kind real) :regular)
         (or (tuatara.io:path-within-p real (tuatara.io:real-path-or-parent (tuatara.io:parent-path tex)))
             (and source (equal real (tuatara.io:real-path-or-parent source))))
         real)))

(defun org-latex-line (job line)
  "The line of the LaTeX JOB, an Org file's export, wrote for LINE (from 1)
of the Org file: by its map of where each element's LaTeX begins, counted on
within the element (the way back from what ERROR-PLACE does)."
  (let* ((text (pdf-export-text job))
         (origins (pdf-export-origins job))
         (at (if origins
                 (let ((index (or (position line origins) (position-if (lambda (l) (> l line)) origins))))
                   (if index (1+ index) (length origins)))
                 line))
         (position (loop with p = 0
                         repeat (1- at)
                         do (setf p (or (position #\Newline text :start p) (length text)))
                            (when (< p (length text)) (incf p))
                         finally (return p)))
         (map (pdf-export-map job))
         (entry (or (let ((inner nil))
                      (loop for e across map
                            when (and (<= (second e) position) (< position (third e))
                                      (or (null inner) (< (- (third e) (second e)) (- (third inner) (second inner)))))
                              do (setf inner e))
                      inner)
                    (find-if (lambda (e) (<= (second e) position)) map :from-end t))))
    (if (null entry)
        1
        (destructuring-bind (marked begin end) entry
          (declare (ignore end))
          (let ((next (find-if (lambda (e) (> (first e) marked)) map)))
            (min (+ marked (count #\Newline text :start (min begin position) :end position))
                 (if next (max marked (1- (first next))) most-positive-fixnum)))))))

(defun show-line-in-pdf-at (asked tex line pdf remake)
  "Show LINE of TEX in the PDF beside it, PDF, for ASKED, the words naming the
line asked for (notes.org line 6): the PDF's view shown beside when it is
not, turned to the line's page and the line marked
\(TUATARA.DEFAULTS::PDF-VIEW-SHOW-PLACE). REMAKE is the command
that makes the PDF again, named when there is no PDF or no SyncTeX file."
  (let ((synctex (and (tuatara.io:path-stamp pdf) (latex-synctex-file tex)))
        (pdf-name (tuatara.io:native-file-name pdf)))
    (cond ((not (tuatara.io:path-stamp pdf))
           (message "There is no ~a yet: ~a makes it" pdf-name
                    (substitute-command-keys (format nil "\\[~(~a~)]" remake))))
          ((null synctex)
           (message "~a has no SyncTeX file to say where its lines are: ~a makes it with one" pdf-name
                    (substitute-command-keys (format nil "\\[~(~a~)]" remake))))
          (t
           (let ((view (show-exported-pdf pdf)))
             (cond ((null view) (message "~a could not be shown" pdf-name))
                   (t (pdf-sync-data
                       view synctex
                       (lambda (read why)
                         (multiple-value-bind (page left top right bottom)
                             (and read (live-buffer-p view) (synctex-place read tex line))
                           (cond ((null read) (message "The SyncTeX file of ~a could not be read: ~a" pdf-name why))
                                 ((not (live-buffer-p view)))
                                 ((null page)
                                  (message "~a does not say where line ~d of ~a is" pdf-name line
                                           (tuatara.io:native-file-name tex)))
                                 (t (let ((marked (pdf-view-show-place view page left top right bottom)))
                                      (message "~a: page ~d~:[ (the page alone: pictures are not shown here)~;~]"
                                               asked page marked))))))))))))))

(define-command show-line-in-pdf ()
  "Show the line point is on in the PDF made of this LaTeX file: the PDF's
view shown beside when it is not, turned to the page the line is on, the
place scrolled into sight and marked for a moment. The PDF is made by
\\[export-latex-compile-pdf-buffer] (or \\[export-latex-compile-pdf]),
which leaves where each line went beside its working files; when there is no
such PDF, or the file has changed since, the file is saved and the PDF made
again first, in the background, the line shown once it is. In an Org file,
\\[execute-extended-command] show-line-in-pdf does the same for the PDF
\\[org-export-latex-pdf-buffer] made of it in this session, exported again
alike when the note has changed. In the PDF,
\\<pdf-view-mode>\\[pdf-view-show-source] goes the other way. Where pictures
are not shown, the PDF's text is turned to the page."
  (let* ((buffer (current-buffer))
         (file (let ((file (tuatara.buffer:buffer-file buffer)))
                 (and file (tuatara.io:native (tuatara.io:expand-path file)))))
         (line (1+ (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)))))
    (case (and file (export-kind buffer))
      (:latex
       (let* ((pdf (replaced-type file "pdf"))
              (pdf-stamp (tuatara.io:path-stamp pdf))
              (stamp (tuatara.io:path-stamp file))
              (asked (format nil "~a line ~d" (tuatara.io:native-file-name file) line)))
         (flet ((show (&optional job)
                  (declare (ignore job))
                  (show-line-in-pdf-at asked file line pdf 'export-latex-compile-pdf-buffer)))
           (if (or (null pdf-stamp) (tuatara.buffer:buffer-modified-p buffer)
                   (and stamp (> (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime pdf-stamp))))
               (start-latex-export :view :after #'show)
               (show)))))
      (:org
       (let ((job (loop for job being the hash-values of *org-pdf-exports*
                        when (equal (pdf-export-source job) file) return job)))
         (cond ((null job)
                (message "~a was not made into a PDF in this session: ~a makes it"
                         (tuatara.io:native-file-name file) (substitute-command-keys "\\[org-export-latex-pdf-buffer]")))
               (t (flet ((show (job)
                           (show-line-in-pdf-at (format nil "~a line ~d" (tuatara.io:native-file-name file) line)
                                                (pdf-export-tex job) (org-latex-line job line) (pdf-export-pdf job)
                                                'org-export-latex-pdf-buffer)))
                    (if (org-export-current-p job buffer)
                        (show job)
                        (start-latex-export :view :enhanced (pdf-export-enhanced job) :after #'show)))))))
      (t (message "Not a LaTeX or Org file")))))

(defun show-source-of (buffer page fx fy)
  "Show the line of the file that made the point FX, FY (fractions of PAGE
from its top left) of the PDF BUFFER views, in the other window: the LaTeX
file beside the PDF, or the Org file it was exported from in this session."
  (let* ((pdf (pdf-view-path (pdf-view-of buffer)))
         (tex (replaced-type pdf "tex"))
         (synctex (latex-synctex-file tex))
         (pdf-name (tuatara.io:native-file-name pdf)))
    (if (null synctex)
        (message "~a has no SyncTeX file to say where its lines come from: ~a in its LaTeX file makes it with one"
                 pdf-name (substitute-command-keys "\\[export-latex-compile-pdf-buffer]"))
        (pdf-sync-data
         buffer synctex
         (lambda (read why)
           (multiple-value-bind (file line) (and read (synctex-line read page fx fy))
             (cond ((null read) (message "The SyncTeX file of ~a could not be read: ~a" pdf-name why))
                   ((null file) (message "~a does not say what made page ~d" pdf-name page))
                   (t (let* ((job (gethash pdf *org-pdf-exports*))
                             (shown (source-to-show file synctex tex (and job (pdf-export-source job)))))
                        (multiple-value-bind (source at)
                            (if (and job shown (equal (synctex-path (pdf-export-tex job)) shown))
                                (error-place job (list :line line :file shown))
                                (values shown line))
                          (if source
                              (progn (show-file-line source at :other t)
                                     (message "Page ~d: ~a line ~d" page (tuatara.io:native-file-name source) at))
                              (message "Page ~d comes from ~a line ~d, not opened: only files in the directory of ~a are"
                                       page (escape-controls file) line (tuatara.io:native-file-name tex)))))))))))))

(define-command pdf-view-show-source ()
  "Show the line of the LaTeX file that made the part of the page shown, in
the other window: the spot \\[show-line-in-pdf] marked or a double click
chose when it is on the page in sight, else the middle of the window; in the
PDF's text, the place of point. The PDF must have been made by
\\[export-latex-compile-pdf-buffer], which leaves where each line went beside
its working files; for a PDF exported from an Org file in this session, the
line of the Org file."
  (let ((buffer (current-buffer)))
    (when (pdf-view-of buffer)
      (multiple-value-bind (page fx fy) (pdf-view-place-shown buffer)
        (show-source-of buffer page fx fy)))))

(define-command pdf-view-show-source-at-click ()
  "Show the line of the LaTeX file that made the point of the page double
clicked, in the other window, as \\[pdf-view-show-source] does for the part
shown. In the PDF's text, select the word clicked, as a double click does in
any text."
  (multiple-value-bind (column row) (mouse-key-cell)
    (let* ((window (and row (window-at-cell column row)))
           (buffer (and window (tuatara.display:window-buffer window))))
      (cond ((not (and buffer (pdf-view-of buffer))))
            ((pdf-view-text-p buffer) (mouse-select-word))
            (t (multiple-value-bind (page fx fy) (pdf-view-place-at buffer column row)
                 (if page
                     (show-source-of buffer page fx fy)
                     (message "Not on the page"))))))))

(defun install-pdf-sync ()
  "Bind \\`C-c C-v' in LaTeX buffers to show-line-in-pdf, and in a PDF's view to
pdf-view-show-source, a double click on its page to
pdf-view-show-source-at-click; offer show-line-in-pdf in Org buffers too;
forget an Org file's exports when its buffer is killed."
  (define-key (mode-keymap 'latex-mode) "C-c C-v" 'show-line-in-pdf)
  (define-key *pdf-view-keymap* "C-c C-v" 'pdf-view-show-source)
  (define-key *pdf-view-keymap* "<double-mouse-1>" 'pdf-view-show-source-at-click)
  (commands-for-mode 'latex-mode "LaTeX and Org files" 'show-line-in-pdf)
  (commands-for-mode 'org-mode "LaTeX and Org files" 'show-line-in-pdf)
  (commands-for-mode 'pdf-view-mode "a PDF's view" 'pdf-view-show-source 'pdf-view-show-source-at-click)
  (pushnew 'forget-org-pdf-exports tuatara.buffer:*kill-buffer-functions*))
