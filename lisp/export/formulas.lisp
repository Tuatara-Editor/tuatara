;;;; lisp/export/formulas.lisp --- Formulas drawn by LaTeX, to be shown among the lines of a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A FORMULA (lisp/viewers/inline-pictures.lisp) is drawn by the first LaTeX
;;;; program of *LATEX-ENGINES* found, shut in the sandbox as an export's is but
;;;; shown nothing beyond TeX's own files and fonts, never run without the
;;;; sandbox. The formulas asked for while none is drawn are drawn together, each
;;;; on a page of its own (the preview package's tight pages), and each page is
;;;; drawn into pixels by the PDF view's program, at the size of the text of the
;;;; rows it is shown among, then cut to its ink and coloured as the text is. A
;;;; formula that fails with others is tried alone; one that fails alone is
;;;; shown as written.

(in-package #:tuatara.export)

(defparameter *formula-row-points* 12
  "How many of LaTeX's points a row of text stands for: formulas, set at 10
points, are drawn as large as text of 10 points with lines of 12 is, a row
as tall as such a line.")

(defparameter *formula-seconds* 20
  "The most processor seconds the LaTeX program drawing formulas may take at
a time, in place of an export's *LATEX-SANDBOX-SECONDS*: a formula that never
ends holds the others up no longer than that.")

(defparameter *formula-largest* (* 4 1024 1024)
  "The most pixels a formula is drawn in; a page larger than that (a rule
drawn huge) is not drawn, and its formula is shown as written.")

(defparameter *formula-pdf-limit* (* 64 1024 1024)
  "The most octets of the PDF of formulas read.")

(defparameter *formula-memory* (* 64 1024 1024)
  "The most octets the formulas drawn for showing among lines of text are kept
in; past it, those shown least lately are let go of, and drawn again when
they come back into sight.")

(defvar *formula-images* (make-hash-table :test 'equal)
  "The formulas drawn, by the key FORMULA-LOOKUP-KEY gives: :PENDING while one is
asked for or drawn, :FAILED when it could not be, else (IMAGE . USED), USED
the count of *INLINE-PICTURE-USES* it was last shown at.")

(defvar *formula-waiting* '()
  "The keys of the formulas asked for and not yet begun, the newest first.")

(defvar *formula-job* nil
  "The job drawing formulas now, or NIL.")

(defvar *formula-colour* (cons -1 nil)
  "The colour formulas are drawn in, the text's (the :default face's), kept
with the *FACE-GENERATION* it was read at.")

(defun formula-colour ()
  "The colour formulas are drawn in, the text's, as #RRGGBB."
  (unless (eql (car *formula-colour*) tuatara.display:*face-generation*)
    (setf *formula-colour* (cons tuatara.display:*face-generation*
                                 (tuatara.display:colour-hex (tuatara.display:resolve-face-spec :default)))))
  (cdr *formula-colour*))

(defun formula-lookup-key (source)
  "The key SOURCE, a FORMULA, is drawn and kept by: its preamble and text,
whether it is inside a line, the height of a row in pixels and the colour it
is drawn in; NIL where the frontend shows no pictures or its rows' size is
not known. The same key while those are unchanged (FORMULA-KEY of SOURCE)."
  (let ((frontend (and *editor* (editor-frontend *editor*))))
    (when (and frontend (tuatara.frontend:frontend-images-p frontend))
      (let ((height (nth-value 1 (tuatara.frontend:frontend-cell-pixels frontend)))
            (colour (formula-colour))
            (kept (formula-key source)))
        (when height
          (let ((height (round height)))
            (if (and kept (eql (fourth kept) height) (equal (fifth kept) colour))
                kept
                (setf (formula-key source)
                      (list (formula-preamble source) (formula-text source) (formula-inline source)
                            height colour)))))))))

(defun formula-entry (buffer source)
  "What is kept of SOURCE, a FORMULA, drawn: (IMAGE . USED), marked used now,
or NIL while it is drawn (begun now in a job of BUFFER's, with the formulas
asked for meanwhile, when it is not yet) or could not be."
  (let* ((key (formula-lookup-key source))
         (entry (and key (gethash key *formula-images*))))
    (cond ((null key) nil)
          ((consp entry) (setf (cdr entry) (incf *inline-picture-uses*)) entry)
          (entry nil)
          (t (setf (gethash key *formula-images*) :pending)
             (push key *formula-waiting*)
             (start-formula-job buffer)
             nil))))

(defmethod picture-source-size (buffer (source formula))
  "A formula's size once drawn (FORMULA-ENTRY); NIL meanwhile."
  (let ((entry (formula-entry buffer source)))
    (when entry
      (values :formula (tuatara.frontend:image-width (car entry)) (tuatara.frontend:image-height (car entry))))))

(defmethod picture-source-image (buffer (source formula) width height)
  "A formula drawn, at its own size whatever WIDTH and HEIGHT ask: the
frontend fits it into the cells it is given."
  (declare (ignore width height))
  (car (formula-entry buffer source)))

(defmethod picture-source-words ((source formula))
  "Where pictures cannot be shown, a formula is shown as written."
  nil)

;;; Drawing them, in the background

(define-record (formula-job (:include decoding-job) (:constructor make-formula-job (buffer keys)) (:copier nil))
  "Formulas being drawn for BUFFER: KEYS, each as FORMULA-LOOKUP-KEY gives it, and
IMAGES, once done, the picture drawn for each, or NIL for one that could not
be."
  (keys nil :documentation "The keys of the formulas FORMULA-JOB draws, each as FORMULA-LOOKUP-KEY gives it.")
  (images nil
          :documentation "The picture drawn for each formula of FORMULA-JOB once done, NIL for one that could not be
drawn."))

(tuatara.runtime:define-job-words formula-job
    :doing "Drawing"
    :description (format nil "Drawing ~d formula~:p" (length (formula-job-keys job)))
    :verb "draw them"
    :left-words "nothing drawn")

(defun start-formula-job (buffer)
  "Draw the formulas waiting (*FORMULA-WAITING*) in a job of BUFFER's, unless
one is being drawn: those asked for meanwhile wait for the next."
  (when (and *formula-waiting* (null *formula-job*))
    (let ((job (make-formula-job buffer (reverse *formula-waiting*))))
      (setf *formula-waiting* '() *formula-job* job)
      (tuatara.runtime:start-job-thread
       job (tuatara.runtime:carry-settings (append *decoding-carried-settings* *latex-carried-settings*
                                                   '(*formula-preamble*))
                                           (lambda (job) (setf (formula-job-images job) (draw-formula-keys job))))
       :name "tuatara formulas"))))

(defmethod tuatara.runtime:job-take-in ((job formula-job))
  "Keep the formulas drawn, and that the others could not be; let go of
those shown least lately past *FORMULA-MEMORY*, and draw those asked for
meanwhile, or forget them when JOB's buffer is gone. The windows show them at
the next frame."
  (setf *formula-job* nil)
  (loop for key in (formula-job-keys job)
        for rest = (formula-job-images job) then (cdr rest)
        for image = (and (eq (tuatara.runtime:job-status job) :done) (car rest))
        do (setf (gethash key *formula-images*) (if image (cons image (incf *inline-picture-uses*)) :failed)))
  (forget-pictures-past *formula-memory* *formula-images*)
  (if (tuatara.buffer:live-buffer-p (tuatara.runtime:job-buffer job))
      (start-formula-job (tuatara.runtime:job-buffer job))
      ;; its buffer gone: those waiting are asked for again by whoever shows them next
      (progn (dolist (key *formula-waiting*) (remhash key *formula-images*))
             (setf *formula-waiting* '()))))

(defun draw-formula-keys (job)
  "The pictures of JOB's formulas, each drawn as its key says, in order, NIL
for one that could not be: those with the same preamble, row height and
colour drawn together."
  (let ((drawn (make-hash-table :test 'equal)))
    (loop for group in (let ((groups '()))
                         (dolist (key (formula-job-keys job) (nreverse groups))
                           (let ((group (assoc (list (first key) (fourth key) (fifth key)) groups :test #'equal)))
                             (if group (push key (cdr group))
                                 (push (list (list (first key) (fourth key) (fifth key)) key)
                                       groups)))))
          do (let ((keys (reverse (cdr group))))
               (destructuring-bind (preamble height colour) (car group)
                 (loop for key in keys
                       for image in (draw-formulas job preamble (mapcar #'second keys) height)
                       do (setf (gethash key drawn) (and image (formula-ink image colour (third key) height)))))))
    (mapcar (lambda (key) (gethash key drawn)) (formula-job-keys job))))

(defun formula-document (preamble texts)
  "The LaTeX document drawing TEXTS, formulas, each on a page of its own cut
to it, after PREAMBLE."
  (with-output-to-string (out)
    (format out "\\documentclass{article}~%~a~%\\usepackage[active,tightpage]{preview}~%~
                 \\setlength\\PreviewBorder{0pt}~%\\pagestyle{empty}~%\\begin{document}~%" preamble)
    (dolist (text texts)
      (format out "\\begin{preview}~a\\end{preview}~%" text))
    (format out "\\end{document}~%")))

(defvar *formula-preambles-tried* (make-hash-table :test 'equal)
  "Whether each preamble tried for formulas lets a formula be drawn after it,
by the preamble: true or NIL. Read and written by the job drawing formulas
alone, one at a time.")

(defun formula-preamble-works-p (job engine preamble height)
  "True when a formula can be drawn for JOB after PREAMBLE by ENGINE, (NAME
. PATH), in rows HEIGHT pixels tall: found once by drawing one, and kept
\(*FORMULA-PREAMBLES-TRIED*)."
  (multiple-value-bind (works known) (gethash preamble *formula-preambles-tried*)
    (if known
        works
        (setf (gethash preamble *formula-preambles-tried*)
              (and (first (formula-pages job (cdr engine) (car engine) preamble '("$x$") height)) t)))))

(defun draw-formulas (job preamble texts height)
  "TEXTS, formulas, drawn for JOB after PREAMBLE by the first LaTeX program
found, shut in, each as a picture as large as text in rows HEIGHT pixels tall
is (*FORMULA-ROW-POINTS*), black on white, in order, NIL for one that could
not be: those that fail together are tried each alone. After
*FORMULA-PREAMBLE* instead when nothing can be drawn after PREAMBLE (a file's
own, naming files the sandbox does not show; FORMULA-PREAMBLE-WORKS-P). All
NIL where there is no LaTeX program, no sandbox, or no program drawing PDF
pages."
  (let ((engine (first (latex-engines-found))))
    (when (and engine (eq (tuatara.io:sandbox-status) :ready) (find-pdf-decoder))
      (let ((preamble (if (or (string= preamble *formula-preamble*)
                              (formula-preamble-works-p job engine preamble height))
                          preamble
                          *formula-preamble*)))
        (labels ((try (texts)
                   (let ((pictures (formula-pages job (cdr engine) (car engine) preamble texts height)))
                     (cond (pictures pictures)
                           ((rest texts) (mapcan (lambda (text) (try (list text))) texts))
                           (t (list nil))))))
          (try texts))))))

(defun formula-pages (job path name preamble texts height)
  "TEXTS drawn by the LaTeX program NAME at PATH for JOB, shut in, as
DRAW-FORMULAS draws them, a picture for each, NIL for a page larger than
*FORMULA-LARGEST*; NIL when the program fails, takes longer than
*FORMULA-SECONDS*, or makes another number of pages."
  (let* ((directory (tuatara.io:ensure-private-directory
                     (tuatara.io:path-join (private-cache-directory "formulas/") (format nil "~a/" name))))
         (tex (tuatara.io:path-join directory "formulas.tex"))
         (pdf (tuatara.io:path-join directory "formulas.pdf")))
    (tuatara.io:remove-file-if-there pdf)
    (tuatara.io:replace-file tex (formula-document preamble texts))
    (when (let ((*latex-sandbox-seconds* *formula-seconds*))
            (run-shut-in job tex path (list "-interaction=nonstopmode" "-halt-on-error" "-no-shell-escape"
                                            (concatenate 'string "-output-directory=" directory) tex)
                         directory :places (tex-program-places path directory)))
      (let* ((octets (handler-case (tuatara.io:file-octets pdf :limit *formula-pdf-limit*)
                       (tuatara.io:file-system-error () nil)))
             (boxes (and octets (pdf-page-boxes octets)))
             (scale (/ height *formula-row-points*)))
        (when (and boxes (= (length boxes) (length texts)))
          (loop for (nil (left bottom right top)) across boxes
                for page from 1
                for across = (max 1 (ceiling (* (- right left) scale)))
                collect (and (<= (* across (max 1 (ceiling (* (- top bottom) scale)))) *formula-largest*)
                             (let ((fd (tuatara.io:memory-file "tuatara-formulas" octets :kept nil)))
                               (unwind-protect
                                    (decode-pdf-page fd page across -1 :cancel (tuatara.runtime:job-cancel job))
                                 (sb-posix:close fd))))))))))

(defun formula-ink (image colour inline height)
  "IMAGE, a formula drawn black on white, as its ink in COLOUR (#RRGGBB) on
nothing, the paper let through, cut to the ink with a pixel round it; one
INLINE, inside a line, shorter than HEIGHT pixels, as tall as that, the ink
in the middle."
  (multiple-value-bind (red green blue) (tuatara.display:parse-hex-colour colour)
    (let* ((width (tuatara.frontend:image-width image))
           (rows (tuatara.frontend:image-height image))
           (channels (tuatara.frontend:image-channels image))
           (pixels (tuatara.frontend:image-pixels image))
           (left width) (right -1) (top rows) (bottom -1))
      (declare (type (simple-array (unsigned-byte 8) (*)) pixels))
      (flet ((ink (x y)
               (let ((at (* channels (+ x (* y width)))))
                 (- 255 (floor (+ (aref pixels at) (aref pixels (+ at 1)) (aref pixels (+ at 2))) 3)))))
        (dotimes (y rows)
          (dotimes (x width)
            (when (plusp (ink x y))
              (setf left (min left x) right (max right x) top (min top y) bottom (max bottom y)))))
        (when (minusp right)
          (return-from formula-ink nil))
        (let* ((left (max 0 (1- left))) (top (max 0 (1- top)))
               (right (min (1- width) (1+ right))) (bottom (min (1- rows) (1+ bottom)))
               (across (1+ (- right left)))
               (down (1+ (- bottom top)))
               (tall (if (and inline (< down height)) height down))
               (above (floor (- tall down) 2))
               (out (make-array (* 4 across tall) :element-type '(unsigned-byte 8) :initial-element 0)))
          (loop for y from 0 below down
                do (loop for x from 0 below across
                         for to = (* 4 (+ x (* (+ y above) across)))
                         do (setf (aref out to) red (aref out (+ to 1)) green (aref out (+ to 2)) blue
                                  (aref out (+ to 3)) (ink (+ x left) (+ y top)))))
          (tuatara.frontend:make-image across tall 4 out))))))
