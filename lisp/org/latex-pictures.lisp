;;;; lisp/org/latex-pictures.lisp --- Pictures LaTeX cannot take, made into PNG for the enhanced export.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; xelatex and lualatex take PNG, JPEG and PDF pictures. The enhanced export
;;;; gives them any other picture Tuatara can show (SVG, WebP, GIF, BMP, TIFF,
;;;; HEIF, AVIF, ICO) as a PNG: the picture is turned into pixels by the
;;;; program that shows pictures, shut in the sandbox (DECODE-IMAGE), and the
;;;; pixels written here as a PNG in the cache, kept while the picture is
;;;; unchanged. The PNG holds its pixels uncompressed (zlib's stored blocks).

(in-package #:tuatara.defaults)

(defparameter *latex-picture-largest* 2400
  "The most pixels a side of a picture made into PNG for the enhanced export
has: a page's width at 300 dots an inch.")

(defparameter *latex-picture-kinds* '(:png :jpeg :pdf)
  "The kinds of picture (TUATARA.IO:OCTETS-KIND's keywords) the LaTeX programs
take as they are.")

(defun png-octets (image)
  "IMAGE, a TUATARA.FRONTEND:IMAGE of 3 or 4 channels, as the octets of a PNG
file: its rows, each with no filter, in zlib's stored blocks, uncompressed.
The file is written into one array of its size, made once."
  (let* ((width (tuatara.frontend:image-width image))
         (height (tuatara.frontend:image-height image))
         (channels (tuatara.frontend:image-channels image))
         (pixels (tuatara.frontend:image-pixels image))
         (row (* width channels))
         (raw-length (* height (1+ row)))
         (blocks (max 1 (ceiling raw-length 65535)))
         ;; zlib: its header, each block's five octets before its data, the Adler-32
         (z-length (+ 2 (* 5 blocks) raw-length 4))
         (out (make-array (+ 8 (+ 12 13) (+ 12 z-length) 12) :element-type '(unsigned-byte 8)))
         (at 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) out) (type fixnum at))
    (labels ((put (&rest octets) (dolist (o octets) (setf (aref out at) o) (incf at)))
             (put-integer (n count &key big-endian)
               (tuatara.io:octets-put-integer out n count :at at :big-endian big-endian)
               (incf at count))
             (chunk (type length writer)
               (put-integer length 4 :big-endian t)
               (let ((start at))
                 (loop for c across type do (put (char-code c)))
                 (funcall writer)
                 (put-integer (crc32 out start at) 4 :big-endian t))))
      (put 137 80 78 71 13 10 26 10)
      (chunk "IHDR" 13 (lambda ()
                         (put-integer width 4 :big-endian t)
                         (put-integer height 4 :big-endian t)
                         (put 8 (if (= channels 4) 6 2) 0 0 0)))
      (chunk "IDAT" z-length
             (lambda ()
               (put #x78 #x01)
               ;; the Adler-32 sums taken modulo 65521 only every 5552 octets, the most before B could pass 32 bits
               (let ((a 1) (b 0) (done 0) (pending 0) (x 0) (row-start 0))
                 (declare (type fixnum a b done pending x row-start))
                 (flet ((data (octet)
                          (setf (aref out at) octet)
                          (incf at)
                          (setf a (+ a octet) b (+ b a))
                          (when (= (incf pending) 5552)
                            (setf a (mod a 65521) b (mod b 65521) pending 0))))
                   (dotimes (k blocks)
                     (let ((length (min 65535 (- raw-length done))))
                       (put (if (= k (1- blocks)) 1 0))
                       (put-integer length 2)
                       (put-integer (logxor length #xFFFF) 2)
                       ;; the rows' octets of this block: each row its filter (none), then its pixels
                       (loop repeat length
                             do (data (if (zerop x) 0 (aref pixels (+ row-start x -1))))
                                (when (> (incf x) row)
                                  (setf x 0 row-start (+ row-start row))))
                       (incf done length))))
                 (put-integer (logior (ash (mod b 65521) 16) (mod a 65521)) 4 :big-endian t))))
      (chunk "IEND" 0 (lambda ())))
    out))

(defun latex-pictures-directory ()
  "Where the pictures made into PNG for the enhanced export are kept, for you
alone: latex/pictures/ in the cache directory. Those not used for a day are deleted."
  (let ((directory (private-cache-directory "latex/pictures/")))
    (forget-old-copies directory)
    directory))

(defun png-sixteen-bit-p (path)
  "True when the file at PATH, a PNG, has 16 bits for each colour (its
header's bit depth), which not every PDF reader draws once in a PDF."
  (let ((head (ignore-errors
               (tuatara.io:call-with-file path (lambda (fd name size)
                                                 (declare (ignore size))
                                                 (tuatara.io:read-fd-octets fd :path name :size-hint 32 :most 32))))))
    (and head (>= (length head) 25) (= 16 (aref head 24)))))

(defun latex-picture (path)
  "The picture at PATH as the LaTeX programs can take it: PATH itself for a
PNG of 8 bits a colour, a JPEG or a PDF; else a PNG of 8 bits a colour made
of it in the sandbox (kept in LATEX-PICTURES-DIRECTORY while PATH is
unchanged); as a second value why there is none (NIL then)."
  (multiple-value-bind (words kind why) (file-kind path)
    (declare (ignore words))
    (cond (why (values nil (format nil "~a cannot be read: ~a" (tuatara.io:native-file-name path) why)))
          ((and (member kind *latex-picture-kinds*) (not (and (eq kind :png) (png-sixteen-bit-p path)))) path)
          ((not (assoc kind *image-formats*))
           (values nil (format nil "~a is no picture Tuatara can show" (tuatara.io:native-file-name path))))
          (t
           (let* ((stamp (tuatara.io:path-stamp path))
                  (png (tuatara.io:path-join (latex-pictures-directory)
                                             (format nil "~36r.png" (sxhash (format nil "~a ~a ~a" path (tuatara.io:stamp-size stamp)
                                                                                     (tuatara.io:stamp-mtime stamp)))))))
             (if (tuatara.io:path-stamp png)
                 png
                 (multiple-value-bind (image notes)
                     (tuatara.io:call-with-file path (lambda (fd name size)
                                                       (declare (ignore name size))
                                                       ;; a drawing drawn for print, not at its own small size
                                                       (decode-image fd kind *latex-picture-largest* *latex-picture-largest*
                                                                     :density (and (eq kind :svg) 300))))
                   (if image
                       (progn (tuatara.io:replace-file png (png-octets image))
                              png)
                       (values nil (format nil "~a could not be made into a picture LaTeX takes~{~a~}"
                                           (tuatara.io:native-file-name path) (mapcar #'car notes)))))))))))
