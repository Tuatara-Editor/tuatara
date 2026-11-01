;;;; test/image-view.lisp --- An image file visited, shown as its picture.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test image-view
  :description "Visiting an image file (PNG, JPEG and the like, by its octets) shows its picture,
drawn in the sandbox, read only; M-x hex-mode opens its octets, safely; where pictures are not shown
the buffer says what it is. An SVG image is visited as its text; a safe visit opens the octets.")

(defun image-session (file keys &key cell-pixels (width 120) (height 30))
  "Visit FILE with C-x C-f in an editor session and, once its picture is drawn
in the background, type KEYS; CELL-PIXELS has the frontend show pictures.
Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels))
        (start (get-internal-real-time)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file))))
    (labels ((busy ()
               (some #'tuatara.defaults::image-draw-job-p tuatara.runtime:*jobs*))
             (check ()
               (if (and (busy) (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                   (when keys (tuatara.frontend.fake:fake-queue-keys f keys)))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(defun image-forget ()
  "Kill the image buffers earlier sessions left."
  (dolist (b (copy-list tuatara.defaults::*image-views*))
    (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b))))

(define-test (image-view shown)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      ;; its picture, read only, with what it is over it; no octets, nothing to save
      (image-forget)
      (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= "photo.png" (tuatara.buffer:buffer-name buffer))
          (true (tuatara.defaults::image-view-of buffer))
          (true (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f "PNG image, 2 × 2"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Image"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Sandboxed"))
          (false (tuatara.frontend.fake:fake-frame-showing f "Pictures are not shown here"))
          (false (tuatara.buffer:buffer-modified-p buffer))
          (false (tuatara.buffer:buffer-file buffer))))
      ;; M-x hex-mode: its own octets, opened safely
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "M-x h e x - m o d e RET" :cell-pixels '(10 20))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.display:hex-view-p buffer))
          (true (tuatara.io:safe-visit-p buffer))
          (true (tuatara.frontend.fake:fake-frame-showing f "00000000  89 50 4e 47"))
          (tuatara.buffer:kill-buffer buffer)))
      ;; where pictures are not shown: what it is, and how to see its octets
      (image-forget)
      (let ((f (image-session file nil)))
        (true (tuatara.frontend.fake:fake-frame-showing f "photo.png: PNG image, 2 × 2"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Pictures are not shown here")))
      ;; a safe visit: its octets
      (image-forget)
      (multiple-value-bind (f editor)
          (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file))) :width 120 :height 20 :cell-pixels '(10 20))
        (declare (ignore f))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (false (tuatara.defaults::image-view-of buffer))
          (tuatara.buffer:kill-buffer buffer)))
      ;; an SVG image is a text: visited as one
      (alexandria:write-string-into-file "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"4\"></svg>"
                                         (merge-pathnames "drawing.svg" dir))
      (image-forget)
      (multiple-value-bind (f editor) (image-session (merge-pathnames "drawing.svg" dir) nil :cell-pixels '(10 20))
        (declare (ignore f))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (false (tuatara.defaults::image-view-of buffer))
          (true (search "<svg" (tuatara.buffer:buffer-string buffer)))
          (tuatara.buffer:kill-buffer buffer)))
      (image-forget))))

(define-test (image-view large-photo)
  ;; a photo of 108 megapixels (12000 x 9000, a phone's at full size) drawn as any: decoded
  ;; at a fraction of its size from the start, never refused for want of memory in the sandbox
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "large.jpg" dir))))
      (uiop:run-program (list "magick" "-size" "12000x9000" "xc:#808080" "-quality" "70" file))
      (image-forget)
      (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-images f))
        (true (tuatara.frontend.fake:fake-frame-showing f "12000 × 9000"))
        (false (tuatara.frontend.fake:fake-frame-showing f "failed"))))
    ;; a PNG, a TIFF with its directory after its pixels, a WebP and an AVIF of 36 megapixels, which no
    ;; fraction shortens: given the memory their size calls for, read from the file
    (dolist (name '("large.png" "large.tif" "large.webp" "large.avif"))
      (let ((file (namestring (merge-pathnames name dir))))
        (uiop:run-program (list "magick" "-size" "6000x6000" "gradient:#203040-#c0d0e0" file))
        (image-forget)
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-images f) "~a" name)
          (true (tuatara.frontend.fake:fake-frame-showing f "6000 × 6000") "~a" name)
          (false (tuatara.frontend.fake:fake-frame-showing f "failed") "~a" name))))
    ;; one needing more than *picture-memory*: not drawn, and why said
    (let ((tuatara.defaults::*picture-memory* (* 512 1024 1024)))
      (image-forget)
      (let ((f (image-session (namestring (merge-pathnames "large.png" dir)) nil :cell-pixels '(10 20))))
        (true (tuatara.frontend.fake:fake-frame-showing f "more than *picture-memory* allows"))))))

(define-test (image-view camera-raw-preview)
  ;; a Fujifilm raw LibRaw cannot develop (here one with no raw data at all): the camera's JPEG
  ;; it carries, where its header says, drawn in its place
  (with-test-directory (dir)
    (let ((jpeg (namestring (merge-pathnames "preview.jpg" dir)))
          (raf (namestring (merge-pathnames "photo.raf" dir))))
      (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" jpeg))
      (let* ((octets (alexandria:read-file-into-byte-vector jpeg))
             (header (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
        (replace header (map 'vector #'char-code "FUJIFILMCCD-RAW 0201FF383501"))
        (loop for (at value) in (list (list 84 256) (list 88 (length octets)))
              do (loop for i below 4 do (setf (aref header (+ at i)) (ldb (byte 8 (* 8 (- 3 i))) value))))
        (with-open-file (out raf :direction :output :element-type '(unsigned-byte 8))
          (write-sequence header out)
          (write-sequence octets out)))
      (image-forget)
      (multiple-value-bind (f editor) (image-session raf nil :cell-pixels '(10 20))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-images f))
        (true (tuatara.frontend.fake:fake-frame-showing f "Camera raw, Fujifilm RAF"))))))

(define-test (image-view why-not)
  ;; nothing to draw it with: its octets, and why
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      (image-forget)
      (let ((tuatara-user:*sandbox-program* nil))
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "photo.png opens as octets: decoding is off"))
          (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            (false (tuatara.defaults::image-view-of buffer))
            (tuatara.buffer:kill-buffer buffer))))
      (image-forget)
      (let ((tuatara.defaults::*image-decoders* '((:name "NoSuchMagick" :program ("no-such-magick-tuatara")))))
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "photo.png opens as octets: no program for pictures (install NoSuchMagick)"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (tuatara.io:sandbox-status :again t)
      ;; a picture that cannot be drawn says so, and why, where pictures are shown
      (let ((fake (merge-pathnames "fake.png" dir)))
        (alexandria:write-byte-vector-into-file
         (concatenate '(vector (unsigned-byte 8)) #(#x89 80 78 71 13 10 26 10) (map 'vector #'char-code "garbage garbage garbage"))
         fake)
        (image-forget)
        (multiple-value-bind (f editor) (image-session fake nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "damaged PNG image"))
          (true (tuatara.frontend.fake:fake-frame-showing f "The picture could not be drawn"))
          (false (tuatara.frontend.fake:fake-frame-showing f "Pictures are not shown here"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (image-forget))
      ;; a JPEG cut short: what could be drawn is shown, and said to be damaged
      (let ((whole (namestring (merge-pathnames "whole.jpg" dir)))
            (cut (merge-pathnames "cut.jpg" dir)))
        (uiop:run-program (list "magick" "-size" "400x300" "gradient:red-blue" whole))
        (let ((octets (alexandria:read-file-into-byte-vector whole)))
          (alexandria:write-byte-vector-into-file (subseq octets 0 (floor (length octets) 2)) cut))
        (image-forget)
        (multiple-value-bind (f editor) (image-session cut nil :cell-pixels '(10 20))
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f ", damaged: Premature end of JPEG file")))
        (image-forget)))))

(define-test (image-view listed-with-its-file)
  ;; the buffer list names the picture's file, counted among the files
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "C-x C-b" :cell-pixels '(10 20) :width 200)
        (declare (ignore f))
        (let ((row (find-if (lambda (l) (search "photo.png" l :end2 (min 30 (length l))))
                            (uiop:split-string (session-text editor) :separator '(#\Newline)))))
          (true row)
          (true (search (tuatara.command::abbreviated-path (tuatara.io:native file)) row))))
      (image-forget)
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (image-view picture-octets-decoded)
  ;; a picture's octets, as a package finds them in another file, turned into pixels in the sandbox
  (with-test-directory (dir)
    (let ((jpeg (namestring (merge-pathnames "patch.jpg" dir))))
      (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" "-quality" "95" jpeg))
      (let ((octets (alexandria:read-file-into-byte-vector jpeg)))
        (let ((image (tuatara:decode-picture octets :jpeg 64 48)))
          (true image)
          (when image
            (is = 64 (tuatara:image-width image))
            (is = 48 (tuatara:image-height image))
            (let ((pixels (tuatara:image-pixels image)) (at (* 10 (tuatara:image-channels image))))
              (true (<= 48 (aref pixels at) 54) "red ~d" (aref pixels at))
              (true (<= 99 (aref pixels (+ at 1)) 105) "green ~d" (aref pixels (+ at 1)))
              (true (<= 150 (aref pixels (+ at 2)) 156) "blue ~d" (aref pixels (+ at 2))))))
        ;; nothing to decode it with: no picture, and why
        (unwind-protect
             (let ((tuatara-user:*sandbox-program* nil))
               (tuatara.io:sandbox-status :again t)
               (multiple-value-bind (image notes) (tuatara:decode-picture octets :jpeg 64 48)
                 (false image)
                 (true notes)))
          (tuatara.io:sandbox-status :again t))))))

(defun j2k-octets (width height &key (components 3) (tile-width width) (tile-height height) (precision 8))
  "The start of a JPEG 2000 codestream for a picture WIDTH by HEIGHT: its
start marker and size marker, with COMPONENTS of PRECISION bits, cut in tiles
TILE-WIDTH by TILE-HEIGHT, and nothing after them."
  (flet ((be (n k) (loop for i from (1- k) downto 0 collect (ldb (byte 8 (* 8 i)) n))))
    (kind-octets (list #xFF #x4F #xFF #x51) (be (+ 38 (* 3 components)) 2) (be 0 2)
                 (be width 4) (be height 4) (be 0 4) (be 0 4) (be tile-width 4) (be tile-height 4) (be 0 4) (be 0 4)
                 (be components 2)
                 (loop repeat components append (list (1- precision) 1 1)))))

(defun picture-box (type octets)
  "OCTETS in a box of TYPE, as JPEG 2000 and JPEG XL files keep their parts."
  (let ((size (+ 8 (length octets))))
    (kind-octets (loop for i from 3 downto 0 collect (ldb (byte 8 (* 8 i)) size)) type (coerce octets 'list))))

(defun jp2-octets (codestream &key (before #()))
  "CODESTREAM kept in a JP2 file: its signature, its file type box, BEFORE
\(octets of boxes) and the codestream's box."
  (concatenate '(vector (unsigned-byte 8))
               (kind-octets tuatara.io::*jp2-signature*) (picture-box "ftyp" (kind-octets "jp2 " (list 0 0 0 0) "jp2 "))
               before (picture-box "jp2c" codestream)))

(defun jxl-octets (width height)
  "The start of a bare JPEG XL codestream for a picture WIDTH by HEIGHT (each
at most 2^30), its size written in its longest form, all else as standard,
and nothing after."
  (let ((bits '()))
    (flet ((put (value count) (dotimes (k count) (push (ldb (byte 1 k) value) bits))))
      (put 0 1) (put 3 2) (put (1- height) 30) (put 0 3) (put 3 2) (put (1- width) 30) (put 1 1))
    (setf bits (reverse bits))
    (kind-octets (list #xFF #x0A)
                 (loop for rest on bits by (lambda (l) (nthcdr 8 l))
                       collect (loop for k below 8 for b in rest sum (ash b k))))))

(defun jxl-boxed (codestream &key parts)
  "CODESTREAM kept in a JPEG XL file of boxes: its signature, its file type
box and a jxlc box, or with PARTS, in that many jxlp boxes."
  (concatenate '(vector (unsigned-byte 8))
               (kind-octets tuatara.io::*jxl-signature*) (picture-box "ftyp" (kind-octets "jxl " (list 0 0 0 0) "jxl "))
               (if parts
                   (let ((step (ceiling (length codestream) parts)))
                     (apply #'concatenate '(vector (unsigned-byte 8))
                            (loop for i below parts
                                  for start = (* i step)
                                  collect (picture-box "jxlp" (concatenate '(vector (unsigned-byte 8))
                                                                           (vector (if (= i (1- parts)) #x80 0) 0 0 i)
                                                                           (subseq codestream (min start (length codestream))
                                                                                   (min (+ start step) (length codestream))))))))
                   (picture-box "jxlc" codestream))))

(define-test (image-view jpeg-2000-and-xl-told)
  ;; JPEG 2000 files and codestreams, JPEG XL codestreams and files of boxes: what they are, and their size
  (flet ((kind (octets) (multiple-value-list (tuatara.io:octets-kind octets))))
    (is equal '("JPEG 2000 codestream, 640 × 480" :j2k) (kind (j2k-octets 640 480)))
    (is equal '("JPEG 2000 image, 640 × 480" :jp2) (kind (jp2-octets (j2k-octets 640 480))))
    (is equal '(640 480) (multiple-value-list (tuatara.io:octets-picture-size (jp2-octets (j2k-octets 640 480)))))
    (is equal '("JPEG 2000 image" :jp2) (kind (kind-octets tuatara.io::*jp2-signature*)))
    (is equal '("JPEG XL image, 5000 × 3000" :jxl) (kind (jxl-octets 5000 3000)))
    (is equal '("JPEG XL image, 5000 × 3000" :jxl) (kind (jxl-boxed (jxl-octets 5000 3000))))
    (is equal '("JPEG XL image, 5000 × 3000" :jxl) (kind (jxl-boxed (jxl-octets 5000 3000) :parts 2)))
    (is equal '(:width 300 :height 200 :components 4 :precision 16 :tiles 247)
        (tuatara.io:j2k-header (j2k-octets 300 200 :components 4 :precision 16 :tile-width 16 :tile-height 16)))
    (false (tuatara.io:kind-name-disagrees-p :jp2 "scan.jpx"))
    (false (tuatara.io:kind-name-disagrees-p :j2k "scan.j2c"))
    (true (tuatara.io:kind-name-disagrees-p :jxl "photo.txt"))))

(defun coded-picture-files (dir)
  "Pictures made in DIR by ImageMagick, each 64 by 48 pixels of #336699, as
a list of (NAME KIND OCTETS): a JP2 file, a JPEG 2000 codestream, a JPEG XL
codestream, the same kept in boxes, a JP2 file and a JPEG XL one of 16 bits,
and a JP2 file with alpha at half."
  (let ((png (namestring (merge-pathnames "flat.png" dir)))
        (png16 (namestring (merge-pathnames "flat16.png" dir)))
        (alpha (namestring (merge-pathnames "alpha.png" dir))))
    (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" png))
    (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" "-depth" "16" png16))
    (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" "-alpha" "set" "-channel" "A" "-evaluate" "set" "50%" alpha))
    (flet ((made (from name &rest options)
             (let ((path (namestring (merge-pathnames name dir))))
               (uiop:run-program (append (list "magick" from) options (list path)))
               (alexandria:read-file-into-byte-vector path))))
      (let ((jxl (made png "flat.jxl" "-quality" "100")))
        (list (list "flat.jp2" :jp2 (made png "flat.jp2" "-depth" "8"))
              (list "flat.j2k" :j2k (made png "flat.j2k" "-depth" "8"))
              (list "flat.jxl" :jxl jxl)
              (list "boxed.jxl" :jxl (jxl-boxed jxl))
              (list "parts.jxl" :jxl (jxl-boxed jxl :parts 3))
              (list "flat16.jp2" :jp2 (made png16 "flat16.jp2" "-depth" "16"))
              (list "flat16.jxl" :jxl (made png16 "flat16.jxl" "-depth" "16" "-quality" "100"))
              (list "alpha.jp2" :jp2 (made alpha "alpha.jp2" "-depth" "8")))))))

(define-test (image-view jpeg-2000-and-xl-decoded)
  ;; each turned into pixels in the sandbox, as a package calls for them, and shown when visited
  (with-test-directory (dir)
    (let ((files (coded-picture-files dir)))
      (loop for (name kind octets) in files
            do (is eq kind (nth-value 1 (tuatara.io:octets-kind octets)) "~a told" name)
               (multiple-value-bind (image notes) (tuatara:decode-picture octets kind 64 48)
                 (true image "~a decoded: ~s" name notes)
                 (when image
                   (is = 64 (tuatara:image-width image))
                   (is = 48 (tuatara:image-height image))
                   (let ((pixels (tuatara:image-pixels image)) (at (* 100 (tuatara:image-channels image))))
                     (true (<= 48 (aref pixels at) 54) "~a red ~d" name (aref pixels at))
                     (true (<= 99 (aref pixels (+ at 1)) 105) "~a green ~d" name (aref pixels (+ at 1)))
                     (true (<= 150 (aref pixels (+ at 2)) 156) "~a blue ~d" name (aref pixels (+ at 2)))
                     (when (= 4 (tuatara:image-channels image))
                       (if (search "alpha" name)
                           (true (<= 120 (aref pixels (+ at 3)) 135) "~a alpha ~d" name (aref pixels (+ at 3)))
                           (is = 255 (aref pixels (+ at 3)))))))))
      ;; a PDF's /JPXDecode picture may be either: :jp2 and :j2k each read both
      (let ((jp2 (third (assoc "flat.jp2" files :test #'string=)))
            (j2k (third (assoc "flat.j2k" files :test #'string=))))
        (true (tuatara:decode-picture j2k :jp2 64 48))
        (true (tuatara:decode-picture jp2 :j2k 64 48)))
      ;; visited: its picture, with what it is over it
      (let ((file (merge-pathnames "flat.jp2" dir)))
        (image-forget)
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f "JPEG 2000 image, 64 × 48"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (image-forget)))))

(define-test (image-view huge-declared-picture-refused)
  ;; a small file declaring a huge picture is refused from its header, the decoding program never run
  ;; (here one that would say it gave no picture)
  (let ((tuatara.defaults::*image-decoders* '((:name "stand-in" :program ("true")))))
    (flet ((refusal (octets kind)
             (multiple-value-bind (image notes) (tuatara:decode-picture octets kind 640 480)
               (false image)
               (let ((said (format nil "~{~a~}" (mapcar #'car notes))))
                 (false (search "gave no picture" said) "~a" said)
                 said))))
      (let ((garbage (make-array 4096 :element-type '(unsigned-byte 8) :initial-element #x5A)))
        (dolist (case (list (list (concatenate '(vector (unsigned-byte 8)) (j2k-octets 100000 100000) garbage) :j2k)
                            (list (jp2-octets (concatenate '(vector (unsigned-byte 8)) (j2k-octets 100000 100000) garbage)) :jp2)
                            ;; its codestream past the first 64 KB, after a long box of XML
                            (list (jp2-octets (concatenate '(vector (unsigned-byte 8)) (j2k-octets 100000 100000) garbage)
                                              :before (picture-box "xml " (make-array 70000 :element-type '(unsigned-byte 8)
                                                                                            :initial-element 32)))
                                  :jp2)
                            (list (concatenate '(vector (unsigned-byte 8)) (jxl-octets 1000000000 1000000000) garbage) :jxl)
                            (list (jxl-boxed (concatenate '(vector (unsigned-byte 8)) (jxl-octets 200000 200000) garbage)) :jxl)))
          (true (cl-ppcre:scan "not shown: [0-9,.]+ megapixels would take .* more than \\*picture-memory\\* allows"
                               (refusal (first case) (second case)))
                "~a" (second case))))
      ;; no size in its header, or one no JPEG 2000 picture has
      (true (search "its header gives no size" (refusal (kind-octets tuatara.io::*jp2-signature*) :jp2)))
      (true (search "its header gives no size" (refusal (kind-octets (list #xFF #x0A)) :jxl)))
      (true (search "more than JPEG 2000 allows"
                    (refusal (j2k-octets 100000 100000 :tile-width 1 :tile-height 1 :components 1) :j2k))))))

(define-test (image-view corrupt-coded-picture-fails-cleanly)
  ;; a damaged codestream fails in the sandbox and says so, never an error here
  (with-test-directory (dir)
    (loop for (name kind octets) in (coded-picture-files dir)
          when (member name '("flat.jp2" "flat.j2k" "flat.jxl") :test #'string=)
            do (let ((damaged (copy-seq octets))
                     (start (get-internal-real-time)))
                 ;; the header kept, the coded pixels after it made noise, then cut short
                 (loop for i from (min (length damaged) (if (eq kind :jxl) 12 140)) below (length damaged)
                       do (setf (aref damaged i) (logxor (aref damaged i) (logand (* i 37) 255))))
                 (setf damaged (subseq damaged 0 (min (length damaged) (max 40 (floor (* 3 (length damaged)) 4)))))
                 (multiple-value-bind (image notes) (tuatara:decode-picture damaged kind 64 48)
                   (let ((said (format nil "~{~a~}" (mapcar #'car notes))))
                     (false image "~a drawn" name)
                     (true (search "not shown: " said) "~a: ~s" name said)))
                 (true (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))))))

(define-test (image-view format-reader-missing-said)
  ;; a decoding program built without a format's reader: said plainly, the library named
  (let ((tuatara.defaults::*image-decoders*
          '((:name "stand-in"
             :program ("sh" "-c" "echo \"magick: no decode delegate for this image format \\`JP2' @ error/constitute.c/ReadImage/746.\" >&2; exit 1")))))
    (multiple-value-bind (image notes)
        (tuatara:decode-picture (jp2-octets (concatenate '(vector (unsigned-byte 8)) (j2k-octets 64 48) (make-array 64 :element-type '(unsigned-byte 8))))
                                :jp2 64 48)
      (false image)
      (is equal ", not shown: sh here cannot read JP2 pictures (OpenJPEG, not installed with it)" (car (first notes))))))
