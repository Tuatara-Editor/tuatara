;;;; lisp/viewers/image-view.lisp --- An image file shown as its picture.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Visiting an image (PNG, JPEG, GIF, WebP, BMP, TIFF, ICO, HEIF, AVIF, told
;;;; by its octets) shows its picture, drawn by a program shut in by the
;;;; sandbox (decode.lisp) in the background, as large as the window allows,
;;;; never larger than it is: a buffer of its own, read only, with nothing to
;;;; save. hex-mode opens its octets, safely; O opens it in the desktop's
;;;; program. An SVG image is a text: it is visited as one, its picture in
;;;; rimu's preview. Where pictures are not shown, the buffer says what the
;;;; image is.

(in-package #:tuatara.defaults)

(define-major-mode image-view-mode special-mode
  "An image file shown as its picture, drawn in the sandbox: read only,
nothing to save. \\[hex-mode] opens its octets, safely; \\<image-view-mode>\\[open-externally] opens it in the
desktop's program; \\`q' closes."
  :lighter "Image" :interactive nil)

(defvar *image-view-keymap* (mode-keymap 'image-view-mode)
  "Keys of an image file shown as its picture (image-view-mode's keymap).")

(defparameter *image-view-largest* 4096
  "The most pixels across or down an image is drawn with: a larger one is drawn
this large, its proportions kept, and shown smaller to fit its window.")

(defvar *image-views* '()
  "The buffers showing an image file as its picture.")

(defstruct (image-view (:constructor make-image-view (path kind words)) (:copier nil))
  "What an image's buffer knows: the image file's PATH, its KIND (as
TUATARA.IO:OCTETS-KIND tells it) and that in WORDS."
  path kind words)

(setf (documentation 'image-view-p 'function) "True when OBJECT is an IMAGE-VIEW.")

(defun image-view-of (buffer)
  "The IMAGE-VIEW of BUFFER, or NIL when it shows no image file."
  (and (live-buffer-p buffer) (tuatara.buffer:buffer-local buffer 'image-view)))

(defstruct (image-draw-job (:include tuatara.runtime:job) (:constructor make-image-draw-job (buffer view)) (:copier nil))
  "VIEW's image being drawn for BUFFER, a job: RESULT, once done, (IMAGE . NOTES)
as DECODE-IMAGE gives them."
  view result)

(setf (documentation 'image-draw-job-p 'function) "True when OBJECT is an IMAGE-DRAW-JOB.")

(tuatara.runtime:define-job-words image-draw-job
  :doing "Drawing"
  :description (format nil "Drawing ~a" (tuatara.io:native-file-name (image-view-path (image-draw-job-view job))))
  :verb "draw it"
  :left-words "nothing drawn")

(defmethod tuatara.runtime:job-at-exit ((job image-draw-job))
  "Drawing a picture only shows something: stopped without asking."
  :quiet)

(defmethod tuatara.runtime:job-take-in ((job image-draw-job))
  "Give the image's buffer its picture (IMAGE-VIEW-SHOW)."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (image-view-of buffer)
      (image-view-show buffer (image-draw-job-view job)
                       (or (and (eq (tuatara.runtime:job-status job) :done) (image-draw-job-result job))
                           (cons nil (list (cons ", not shown: it could not be read" :comment))))))))

(defun image-file-kind (native)
  "What the file at NATIVE is by its octets, as two values, a keyword and
words, when it is an image this view draws (one of *IMAGE-FORMATS*, not
SVG); else NIL."
  (multiple-value-bind (words kind) (ignore-errors (file-kind native))
    (and kind (not (eq kind :svg)) (assoc kind *image-formats*)
         (values kind words))))

(defun image-view-start (buffer view)
  "Start drawing VIEW's image, BUFFER's, in the background."
  (tuatara.runtime:start-job-thread
   (make-image-draw-job buffer view)
   (tuatara.runtime:carry-settings
    *decoding-carried-settings*
    (lambda (job)
      (setf (image-draw-job-result job)
            (handler-case
                (tuatara.io:call-with-file
                 (image-view-path view)
                 (lambda (fd path size)
                   (declare (ignore path size))
                   (multiple-value-bind (image notes)
                       (decode-image fd (image-view-kind view) *image-view-largest* *image-view-largest*
                                     :cancel (tuatara.runtime:job-cancel job))
                     (cons image notes))))
              (storage-condition ()
                (cons nil (list (cons ", not shown: there was not memory enough to draw it" :comment))))))))
   :name "tuatara image view"))

(defun image-view-visit (path show)
  "Show the image file at PATH as its picture, with SHOW, a function of the
buffer, when it is an image by its octets and a program shut in by the
sandbox can draw it; the buffer made for it already, when there is one.
Returns the buffer, or NIL for a file visited as usual (not an image, or
nothing to draw it with, which is said)."
  (let ((native (file-view-path path)))
    (when native
      (multiple-value-bind (kind words) (image-file-kind native)
        (when (and kind
                   (file-view-can-draw-p native (find-image-decoder)
                                         (format nil "no program for pictures (install ~{~a~^ or ~})"
                                                 (mapcar (lambda (d) (getf d :name)) *image-decoders*))))
          (file-view-show native *image-views* (lambda (b) (let ((v (image-view-of b))) (and v (image-view-path v))))
                          (lambda () (image-view-make native kind words)) show))))))

(defun image-view-make (native kind words)
  "A new buffer showing the image file at NATIVE, of KIND (WORDS), its picture being drawn."
  (let ((buffer (make-file-view-buffer native 'image-view-mode
                                       (substitute-command-keys
                                        (format nil "~a: ~a~%~%Pictures are not shown here: \\[hex-mode] shows its octets.~%"
                                                (tuatara.io:native-file-name native) words))
                                       "its picture"))
        (view (make-image-view native kind words)))
    (setf (tuatara.buffer:buffer-local buffer :header-line)
          (let ((text (format nil " ~a" words))) (lambda (window) (declare (ignore window)) (values text nil)))
          (tuatara.buffer:buffer-local buffer 'image-view) view)
    (push buffer *image-views*)
    (image-view-start buffer view)
    buffer))

(defun image-view-show (buffer view result)
  "Give BUFFER, VIEW's, the picture in RESULT, (IMAGE . NOTES), or say in its
header and its text why there is none."
  (destructuring-bind (image . notes) result
    (let ((text (format nil " ~a~{~a~}" (image-view-words view) (mapcar #'car notes))))
      (setf (tuatara.buffer:buffer-local buffer :header-line)
            (lambda (window) (declare (ignore window)) (values text nil))))
    (if image
        (setf (tuatara.buffer:buffer-local buffer :image) image)
        ;; nothing to show: the words, and why
        (progn
          (set-made-text buffer (substitute-command-keys
                                 (format nil "~a: ~a~{~a~}~%~%The picture could not be drawn: \\[hex-mode] shows its octets.~%"
                                         (tuatara.io:native-file-name (image-view-path view)) (image-view-words view)
                                         (mapcar #'car notes))))
          (setf (tuatara.buffer:buffer-local buffer :picture-pending) nil
                (tuatara.buffer:buffer-local buffer :picture-view) nil)))))

(defun image-view-forget (buffer)
  "Forget BUFFER, killed, among the images shown; its drawing, a job of its, is stopped with it."
  (setf *image-views* (remove buffer *image-views*)))

(defun install-image-view ()
  "Have image files visited shown as their pictures, and bind O there."
  (define-key *image-view-keymap* "O" 'open-externally)
  (pushnew 'image-view-visit *file-views*)
  (pushnew 'image-view-forget tuatara.buffer:*kill-buffer-functions*))
