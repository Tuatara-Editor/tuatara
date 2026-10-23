;;;; lisp/viewers/decode.lisp --- Files decoded by programs of the system, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What a file holds that is not text, the text of a PDF or the pixels of an
;;;; image, is taken out by a program of the system run in the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN): the editor only ever gets text or plain pixels
;;;; back, which it checks before use. For each kind of file a short list of
;;;; the programs known to do it, each with the arguments it needs; the first
;;;; found where the sandbox sees programs is used, and M-x check-health says
;;;; which. What cannot be decoded, and why, is said in a few words.
;;;; ImageMagick runs under a policy of its own besides: it reads only the
;;;; image formats it is told, writes only the pixels, starts no other program
;;;; and refuses images larger than any screen could want.

(in-package #:tuatara.defaults)

(defparameter *decoding-carried-settings*
  '(tuatara-user:*sandbox-program* tuatara-user:*sandbox-seconds* tuatara-user:*sandbox-memory*
    tuatara-user:*sandbox-total-memory* *pdf-decoders* *pdf-text-pages* *image-decoders* *magick-largest-side*
    *picture-memory* *image-view-largest*)
  "The settings that decoding reads, carried to the thread that draws a PDF's
page, takes out its text, draws an image or makes a preview, as they are on
the editor's when that starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defparameter *pdf-decoders*
  '((:name "poppler"
     :info ("pdfinfo" "fd://0")
     :text ("pdftotext" "-layout" "-enc" "UTF-8" "-l" :pages "fd://0" "-")
     :page ("pdftoppm" "-f" :page "-l" :page "-scale-to-x" :width "-scale-to-y" :height "fd://0")))
  "The programs known to take the text out of a PDF, the first found used:
each a list of :NAME, the package they come in, :TEXT, the program printing
the text and its arguments (:PAGES standing for how many pages, the file read
on standard input), :INFO, the one printing the number of pages, their size
and the like, as lines of Name: value, and :PAGE, the one drawing page :PAGE
as a PPM or PAM image :WIDTH by :HEIGHT pixels (-1 for a height that keeps
the page's proportions).")

(defparameter *pdf-text-pages* 50
  "The pages of a PDF whose text is taken out for a preview, from the first.")

(defun find-pdf-decoder ()
  "The first of *PDF-DECODERS* whose program for the text the sandbox finds,
or NIL."
  (find-if (lambda (decoder) (tuatara.io:sandbox-find-program (first (getf decoder :text)))) *pdf-decoders*))

(defun decoder-words (decoder)
  "DECODER's programs as a sentence names them: pdftotext and pdfinfo (poppler)."
  (let ((text (first (getf decoder :text)))
        (info (and (getf decoder :info) (tuatara.io:sandbox-find-program (first (getf decoder :info)))
                   (first (getf decoder :info)))))
    (format nil "~a~@[ and ~a~] (~a)" text info (getf decoder :name))))

(defun pdf-decoder-words ()
  "What decodes PDFs here, as M-x check-health says it: the programs, or what
to install."
  (let ((decoder (find-pdf-decoder)))
    (if decoder
        (format nil "PDF text by ~a" (decoder-words decoder))
        (format nil "PDF text by none of ~{~a~^, ~}: ~a"
                (mapcar (lambda (d) (first (getf d :text))) *pdf-decoders*)
                (outside-program-install-words :pdf)))))

(defun sandbox-font-directories ()
  "Your own font directories, each (SOURCE . TARGET), TARGET where a program
in the sandbox, its home /tmp, finds them: $XDG_DATA_HOME/fonts (or
~/.local/share/fonts) and ~/.fonts, so that a document in a font you
installed shows in it."
  (mapcar #'cons (tuatara.io:user-font-directories) '("/tmp/.local/share/fonts" "/tmp/.fonts")))

(defparameter *page-drawing-seconds* 120
  "The most processor seconds a program drawing a document's pages as a PDF
\(SANDBOX-PDF), LibreOffice or WeasyPrint, may take.")

(defparameter *page-drawing-memory* (* 2 1024 1024 1024)
  "The most memory a program drawing a document's pages as a PDF (SANDBOX-PDF)
may take.")

(defun sandbox-pdf (program arguments fd &key (label program) files directories cancel)
  "The PDF PROGRAM, run shut in with ARGUMENTS (TUATARA.IO:SANDBOX-RUN),
prints of the document open at FD, given on its standard input, as a sealed
file in memory: it is shown your fonts (SANDBOX-FONT-DIRECTORIES), and FILES
and DIRECTORIES as SANDBOX-RUN takes them, and held to
*PAGE-DRAWING-SECONDS* and *PAGE-DRAWING-MEMORY*. An error saying why, LABEL
naming the program, when it prints no PDF. CANCEL, a cons, stops it when its
car turns true. Waits for it: call it from a thread working in the background."
  (multiple-value-bind (octets outcome said)
      (tuatara.io:sandbox-run program arguments :input fd :seconds *page-drawing-seconds* :memory *page-drawing-memory*
                                                :cancel cancel :files files
                                                :directories (append directories (sandbox-font-directories)))
    (unless (and (eq outcome :done) (tuatara.io:octets-at-p octets "%PDF"))
      (error "~a" (or (tuatara.io:sandbox-outcome-words outcome said :label label :seconds-setting '*page-drawing-seconds*)
                      (format nil "~a drew no pages" label))))
    (tuatara.io:memory-file "tuatara-pages" octets :kept nil)))

(defun no-sandbox-words (status)
  "Why nothing is decoded while the sandbox has STATUS (TUATARA.IO:SANDBOX-STATUS)."
  (case status
    (:missing "no sandbox, bwrap not found (M-x check-health)")
    (:broken "the sandbox cannot run (M-x check-health)")
    (t "decoding is off (*sandbox-program*)")))

(defun decoder-arguments (arguments values)
  "ARGUMENTS of a decoding program as it is run: each keyword among them
replaced by the arguments VALUES, a plist, gives it (a list of strings,
perhaps empty), the rest kept."
  (mapcan (lambda (a) (if (keywordp a) (copy-list (getf values a)) (list a))) arguments))

(defun picture-damage-words (said)
  "The first line of what a program drawing a picture SAID that tells of the
picture being damaged (cut short, its data corrupt), what it drew being only
part of it, without the program's name and where in its source it said it
(Premature end of JPEG file); NIL when it says nothing of the kind."
  (let ((line (find-if (lambda (line) (cl-ppcre:scan "(?i)premature end|corrupt|truncat|damaged|unexpected end|insufficient image data" line))
                       (uiop:split-string (or said "") :separator '(#\Newline)))))
    (and line (string-trim " " (cl-ppcre:regex-replace "(?: [`'(].*| @ .*)$" (cl-ppcre:regex-replace "^[a-z]+: " line "") "")))))

(defun info-field (info name)
  "The value of the line NAME: value in INFO, the text an :INFO program
printed, or NIL. The last such line is taken: the program prints what the file
says of itself (its title, its author) before what it found, so a title with
a line of its own in it cannot stand in for what was found."
  (let ((value nil))
    (dolist (line (uiop:split-string info :separator '(#\Newline)) value)
      (let ((colon (position #\: line)))
        (when (and colon (string-equal name (string-trim " " (subseq line 0 colon))))
          (setf value (string-trim " " (subseq line (1+ colon)))))))))

(defun pdf-pages-text (text)
  "TEXT, pages as the text program prints them, a form feed after each, with a
line saying where each page after the first begins."
  (let ((pages (uiop:split-string text :separator '(#\Page))))
    (when (and (rest pages) (string= "" (string-trim '(#\Newline #\Space) (first (last pages)))))
      (setf pages (butlast pages)))
    (with-output-to-string (out)
      (loop for page in pages
            for n from 1
            do (when (> n 1) (format out "~&~%── Page ~d ──~%~%" n))
               (write-string page out)))))

(defun page-pixels (info width height)
  "The width and height in pixels of the first page, as INFO (what the :INFO
program printed) gives its size and turn, as large as fits in WIDTH by HEIGHT;
NIL when INFO does not say its size."
  (let* ((size (info-field info "Page size"))
         (numbers (and size (ignore-errors (let ((words (uiop:split-string size)))
                                             (list (parse-decimal (first words)) (parse-decimal (third words)))))))
         (turn (ignore-errors (parse-integer (info-field info "Page rot")))))
    (when (and numbers (every (lambda (n) (and (realp n) (plusp n))) numbers))
      (destructuring-bind (across down) (if (member turn '(90 270)) (reverse numbers) numbers)
        (let ((scale (min (/ width across) (/ height down))))
          (list (max 1 (round (* across scale))) (max 1 (round (* down scale)))))))))

(defun decode-pdf (fd &key cancel (limit (expt 2 20)) pixels on-info)
  "The text of the PDF open as FD, taken out in the sandbox, as four values:
the text of its first *PDF-TEXT-PAGES* pages, up to LIMIT octets (1 MB unless
given; NIL when there is none); notes about it for a line over it, each (TEXT
. FACE): how many pages, JavaScript and encryption in :warning, or why there
is no text in :comment; and with PIXELS, a list of the most width and height
in pixels, its first page drawn as large as fits there (a
TUATARA.FRONTEND:IMAGE), or NIL; and what its :INFO program said of it (pages,
their size), as text. Waits for the programs: call it from a thread working in
the background. CANCEL, a cons, stops them when its car turns true; NIL, NIL,
NIL then. ON-INFO, when given, is called with what the :INFO program said as
soon as it has, before the text is taken out."
  (let ((status (tuatara.io:sandbox-status))
        (decoder (find-pdf-decoder))
        (notes '())
        (info "")
        (text nil))
    (flet ((note (text &optional face) (push (cons text face) notes))
           (stopped () (return-from decode-pdf (values nil nil nil)))
           (run (spec values output-limit)
             ;; SPEC's program, its arguments' keywords given by VALUES (DECODER-ARGUMENTS)
             (destructuring-bind (program &rest arguments) spec
               (multiple-value-list
                (tuatara.io:sandbox-run program (decoder-arguments arguments values)
                                        :input fd :cancel cancel :output-limit output-limit)))))
      (unless (eq status :ready)
        (note (format nil ", not decoded: ~a" (no-sandbox-words status)) :comment)
        (return-from decode-pdf (values nil (reverse notes) nil)))
      (unless decoder
        (note (format nil ", not decoded: no program for its text (install ~a: ~a)"
                      (getf (first *pdf-decoders*) :name) (first (getf (first *pdf-decoders*) :text)))
              :comment)
        (return-from decode-pdf (values nil (reverse notes) nil)))
      ;; how many pages, their size, JavaScript and encryption
      (let ((pages nil) (spec (getf decoder :info)))
        (when (and spec (tuatara.io:sandbox-find-program (first spec)))
          (destructuring-bind (output outcome &rest ignore) (run spec '() 65536)
            (declare (ignore ignore))
            (when (eq outcome :cancelled) (stopped))
            (when (eq outcome :done)
              (setf info (tuatara.text:lenient-string output)
                    pages (ignore-errors (parse-integer (info-field info "Pages"))))
              (when on-info (funcall on-info info))
              (when pages (note (format nil ", ~:d page~:p" pages)))
              (when (equal "yes" (info-field info "JavaScript")) (note ", with JavaScript" :warning))
              (when (uiop:string-prefix-p "yes" (info-field info "Encrypted")) (note ", encrypted" :warning)))))
        ;; the text
        (let ((program (first (getf decoder :text))))
          (destructuring-bind (output outcome said) (run (getf decoder :text) (list :pages (list (princ-to-string *pdf-text-pages*))) limit)
            (case outcome
              (:cancelled (stopped))
              ((:done :too-much)
               (let ((all (tuatara.text:lenient-string output)))
                 (cond ((string= "" (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) all))
                        (note ", no text in it (pages of pictures?)" :comment))
                       (t
                        (when (or (eq outcome :too-much) (and pages (> pages *pdf-text-pages*)))
                          (note ", the text of its start" :comment))
                        (setf text (pdf-pages-text all))))))
              (t (note (format nil ", not decoded: ~a" (tuatara.io:sandbox-outcome-words outcome said :label program)) :comment))))))
      ;; the first page drawn
      (values text (reverse notes)
              (and pixels (decode-pdf-page fd 1 (first pixels) (second pixels) :info info :cancel cancel
                                                                                  :decoder decoder))
              info))))

(defun decode-pdf-page (fd page width height &key info cancel (decoder (find-pdf-decoder)))
  "Page PAGE (from 1) of the PDF open as FD, drawn in the sandbox as large as
fits in WIDTH by HEIGHT pixels, as a TUATARA.FRONTEND:IMAGE; NIL when it could
not be drawn, or CANCEL, a cons, had a true car. INFO, what the :INFO program
of DECODER printed, gives the pages' size; without it the page is drawn WIDTH
wide. Waits for the program: call it from a thread working in the background."
  (let ((spec (getf decoder :page)))
    (when (and spec (eq (tuatara.io:sandbox-status) :ready) (tuatara.io:sandbox-find-program (first spec)))
      (destructuring-bind (across down) (or (page-pixels (or info "") width height) (list width -1))
        (destructuring-bind (program &rest arguments) spec
          (multiple-value-bind (output outcome)
              (tuatara.io:sandbox-run program
                                      (decoder-arguments arguments (list :page (list (princ-to-string page))
                                                                         :width (list (princ-to-string across))
                                                                         :height (list (princ-to-string down))))
                                      :input fd :cancel cancel
                                      ;; its height not known: up to 16 times its width, as a long receipt
                                      :output-limit (+ 4096 (* 3 across (if (plusp down) down (min 16384 (* 16 across))))))
            (and (eq outcome :done) (decoded-image output))))))))

;;; Pictures: the pixels of an image, made no larger than they are shown

(defparameter *image-decoders*
  '((:name "ImageMagick"
     :program ("magick" :density :jpeg-hint :input "-auto-orient" "-resize" :size "-alpha" "on" "-depth" "8" "PAM:-")
     :jpeg-hint ("-define" "jpeg:size=~dx~d")
     :sandbox magick-sandbox)
    (:name "GraphicsMagick"
     :program ("gm" "convert" :density :jpeg-hint :input "-auto-orient" "-resize" :size "-depth" "8" "PAM:-")
     :jpeg-hint ("-size" "~dx~d")))
  "The programs known to turn an image into pixels, the first found used: each
a list of :NAME, the package they come in, and :PROGRAM, the program and its
arguments, :INPUT standing for the image on standard input, its format named
(PNG:-[0], its first frame), :SIZE for the most it may be (640x480>, made
no larger than it is), :DENSITY for how finely a drawing (SVG) is drawn
before that, -density N, when asked (nothing otherwise), and :JPEG-HINT, for a
JPEG, the arguments of its :JPEG-HINT with that size: the JPEG is then
decoded at the smallest fraction of its size (an eighth, two eighths...) no
smaller than that, from the start, so that a photo of a hundred megapixels
needs little more memory than a small one. It prints the pixels as a PAM (or PPM) image. :SANDBOX
names a function giving what else the sandbox gives it, as TUATARA.IO:SANDBOX-RUN's
:FILES and :ENVIRONMENT.")

(defvar *magick-largest-side* "64KP"
  "The widest and tallest image ImageMagick reads for a preview, in its own
words (64KP, 65,536 pixels); a larger one is refused before its pixels are
made.")

(defvar *picture-memory* (* 4 1024 1024 1024)
  "The most memory the program drawing a large picture is given (4 GB): a
picture's own size, read from its file, tells how much it needs (about 32
octets a pixel, a hundred-megapixel PNG some 3.9 GB), never less than
*SANDBOX-MEMORY*; one needing more than this is not drawn, and says so. A
JPEG needs little whatever its size, decoded at a fraction of it.")

(defun picture-pixels (fd head)
  "How many pixels the picture open as FD, whose first octets are HEAD, has,
as their kind gives its size (TUATARA.IO:OCTETS-PICTURE-SIZE: 12000 × 9000);
NIL when it gives none."
  (multiple-value-bind (width height) (and head (ignore-errors (tuatara.io:octets-picture-size head)))
    (cond (width (* width height))
          ;; a TIFF whose first directory stands after its pixels, past HEAD
          ((and head (tuatara.io:tiff-octets-p head))
           (multiple-value-bind (width height) (tiff-file-size fd head)
             (and width (* width height)))))))

(defun raf-preview (fd head)
  "The JPEG a Fujifilm raw open as FD, whose first octets are HEAD, carries
of its picture (its header gives where, at octet 84, and how long, at 88),
read with pread; NIL when FD is no such raw or the JPEG is not where it says."
  (when (and head (tuatara.io:octets-at-p head "FUJIFILMCCD-RAW"))
    (let ((offset (tuatara.io:octets-be head 84 4))
          (length (tuatara.io:octets-be head 88 4)))
      (when (and offset length (< 0 length (* 64 1024 1024)))
        (let ((jpeg (tuatara.io:read-fd-exactly fd length offset)))
          (and jpeg (tuatara.io:octets-at-p jpeg '(#xFF #xD8 #xFF)) jpeg))))))

(defun tiff-file-size (fd head)
  "The width and height of the first image of the TIFF open as FD, whose
first octets are HEAD, its first directory read where it stands, as two
values; NIL when it cannot be."
  (let* ((little (= (aref head 0) 73))
         (ifd (tuatara.io:octets-integer head 4 4 :big-endian (not little)))
         (count-octets (and ifd (tuatara.io:read-fd-exactly fd 2 ifd)))
         (count (and count-octets (min 512 (tuatara.io:octets-integer count-octets 0 2 :big-endian (not little)))))
         (directory (and count (plusp count) (tuatara.io:read-fd-exactly fd (+ 2 (* 12 count)) ifd)))
         (entries (and directory (tuatara.io:tiff-entries directory :ifd 0 :little little)))
         (width (fourth (assoc 256 entries)))
         (height (fourth (assoc 257 entries))))
    (and width height (values width height))))

(defun octets-kind-of-file (fd octets)
  "What the file open as FD is by its first OCTETS, as two values, the words
and the keyword of TUATARA.IO:OCTETS-KIND, a TIFF's size read where its first
directory stands when that is past them (TIFF-FILE-SIZE)."
  (multiple-value-bind (words kind) (tuatara.io:octets-kind octets)
    (if (and (eq kind :tiff) (not (search "×" words)))
        (multiple-value-bind (width height) (ignore-errors (tiff-file-size fd octets))
          (values (if width (format nil "~a, ~a" words (tuatara.io:size-words width height)) words) kind))
        (values words kind))))

(defun picture-memory (kind pixels octets)
  "The memory the program drawing a picture of KIND of PIXELS pixels (NIL:
not known), its file OCTETS long, is given: *SANDBOX-MEMORY*, or what a large
one needs, its file read whole first; a JPEG's the first, decoded at a
fraction of its size; a camera's raw what its file's size calls for, developed
whole, up to *PICTURE-MEMORY*."
  (cond ((eq kind :raw)
         ;; its size not in its first octets (they hold a thumbnail's): reckoned from its file's
         (let ((need (+ (* 1024 1024 1024) (* 64 (or octets 0)))))
           (max (or tuatara-user:*sandbox-memory* 0) (if *picture-memory* (min need *picture-memory*) need))))
        ((and pixels (not (eq kind :jpeg)))
         (max (or tuatara-user:*sandbox-memory* 0) (+ (* 512 1024 1024) (* 32 pixels) (or octets 0))))
        (t tuatara-user:*sandbox-memory*)))

(defvar *large-picture-turn* (sb-thread:make-semaphore :count 1 :name "tuatara large picture")
  "Held while a large picture is drawn, one given more memory than
*SANDBOX-MEMORY*, so that one such picture is drawn at a time.")

(defun call-in-picture-turn (large cancel function)
  "Call FUNCTION and return what it returns; when LARGE, only once no other
large picture is being drawn (*LARGE-PICTURE-TURN*). NIL, :CANCELLED when
CANCEL, a cons, has its car turn true while it waits."
  (if large
      (loop (when (and cancel (car cancel)) (return (values nil :cancelled nil)))
            (when (sb-thread:wait-on-semaphore *large-picture-turn* :timeout 1/10)
              (return (unwind-protect (funcall function)
                        (sb-thread:signal-semaphore *large-picture-turn*)))))
      (funcall function)))

(defun magick-sandbox ()
  "What the sandbox gives ImageMagick: a policy of its own, through
MAGICK_CONFIGURE_PATH, reading only the formats of *IMAGE-FORMATS*, writing
only PAM pixels, no program started (delegates), no filters, no file named by
@, no image wider or taller than *MAGICK-LARGEST-SIDE*, two threads at most
\(each costs memory, which the sandbox holds it to)."
  (list :files (list (cons "/etc/tuatara/ImageMagick/policy.xml"
                           (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<policymap>
  <policy domain=\"resource\" name=\"width\" value=\"~a\"/>
  <policy domain=\"resource\" name=\"height\" value=\"~:*~a\"/>
  <policy domain=\"resource\" name=\"list-length\" value=\"128\"/>
  <policy domain=\"resource\" name=\"thread\" value=\"2\"/>
  <policy domain=\"delegate\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"filter\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"path\" rights=\"none\" pattern=\"@*\"/>
  <policy domain=\"coder\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"coder\" rights=\"read\" pattern=\"{~{~a~^,~}}\"/>
  <policy domain=\"coder\" rights=\"write\" pattern=\"PAM\"/>
</policymap>
" *magick-largest-side* (mapcar #'cdr *image-formats*))))
        :environment '(("MAGICK_CONFIGURE_PATH" "/etc/tuatara/ImageMagick"))))

(defparameter *image-formats*
  '((:png . "PNG") (:jpeg . "JPEG") (:gif . "GIF") (:webp . "WEBP") (:bmp . "BMP") (:tiff . "TIFF") (:raw . "DNG")
    (:ico . "ICO") (:heif . "HEIC") (:avif . "AVIF") (:svg . "SVG"))
  "The kinds of image (TUATARA.IO:OCTETS-KIND's keywords) turned into pixels
for a preview, each with the name of its format that the decoding program is
told, so that it reads the file as that and nothing else: a camera's raw is
developed by ImageMagick's LibRaw reader (DNG), whatever its maker.")

(defun find-image-decoder ()
  "The first of *IMAGE-DECODERS* whose program the sandbox finds, or NIL."
  (find-if (lambda (decoder) (tuatara.io:sandbox-find-program (first (getf decoder :program)))) *image-decoders*))

(defun image-decoder-words ()
  "What turns images into pixels here, as M-x check-health says it."
  (let ((decoder (find-image-decoder)))
    (if decoder
        (format nil "pictures by ~a (~a)" (first (getf decoder :program)) (getf decoder :name))
        (format nil "pictures by none of ~{~a~^, ~}: ~a, or ~{~a~^ or ~}"
                (mapcar (lambda (d) (first (getf d :program))) *image-decoders*)
                (outside-program-install-words :pictures)
                (remove (getf (outside-program :pictures) :name) (mapcar (lambda (d) (getf d :name)) *image-decoders*)
                        :test #'string=)))))

(defun decoded-image (octets)
  "The picture OCTETS hold as a PAM image (P7) of 3 or 4 channels, or of 1 or 2
\(grey, grey and alpha, made 3 and 4: a black-and-white photo), or a PPM (P6),
8 bits to a channel, as a TUATARA.FRONTEND:IMAGE; NIL when they hold anything
else, or fewer pixels than they say."
  (let ((at 0) (length (length octets)))
    (labels ((line ()
               (let ((end (position 10 octets :start at :end (min length (+ at 80)))))
                 (when end
                   (prog1 (tuatara.text:octets-latin-1 octets :start at :end end) (setf at (1+ end))))))
             (field (line name)
               (and line (uiop:string-prefix-p name line)
                    (ignore-errors (parse-integer line :start (length name)))))
             (image (width height channels)
               (let ((count (and width height channels (* width height channels))))
                 (when (and count (< 0 width 16385) (< 0 height 16385) (member channels '(1 2 3 4))
                            (<= (+ at count) length))
                   (if (<= channels 2)
                       ;; grey made the three colours alike, its alpha kept
                       (let ((pixels (make-array (* width height (+ 2 channels)) :element-type '(unsigned-byte 8))))
                         (loop for from from at below (+ at count) by channels
                               for to from 0 by (+ 2 channels)
                               do (let ((grey (aref octets from)))
                                    (setf (aref pixels to) grey (aref pixels (+ to 1)) grey (aref pixels (+ to 2)) grey)
                                    (when (= channels 2) (setf (aref pixels (+ to 3)) (aref octets (1+ from))))))
                         (tuatara.frontend:make-image width height (+ 2 channels) pixels))
                       (tuatara.frontend:make-image width height channels (subseq octets at (+ at count))))))))
      (let ((magic (line)))
        (cond ((equal magic "P7")
               (let (width height depth maxval)
                 (loop for line = (line)
                       while (and line (not (equal line "ENDHDR")))
                       do (setf width (or (field line "WIDTH ") width)
                                height (or (field line "HEIGHT ") height)
                                depth (or (field line "DEPTH ") depth)
                                maxval (or (field line "MAXVAL ") maxval))
                       finally (unless line (return-from decoded-image nil)))
                 (and (eql maxval 255) (image width height depth))))
              ((equal magic "P6")
               (let ((size (line)) (maxval (line)))
                 (destructuring-bind (&optional width height) (and size (ignore-errors (mapcar #'parse-integer (uiop:split-string size))))
                   (and (equal maxval "255") (image width height 3))))))))))

(defun decode-image (fd kind width height &key cancel density)
  "The picture in the image open as FD, of KIND (TUATARA.IO:OCTETS-KIND's
keyword), turned into pixels in the sandbox, no larger than WIDTH by HEIGHT
pixels, as two values: the picture (a TUATARA.FRONTEND:IMAGE), NIL when it
could not be made; and notes about it for a line over it, as DECODE-PDF
gives them. Waits for the program: call it from a thread working in the
background. CANCEL, a cons, stops it when its car turns true; NIL, NIL then.
DENSITY, dots an inch, draws a drawing (SVG) that finely before it is sized,
for print."
  (let ((status (tuatara.io:sandbox-status))
        (decoder (find-image-decoder))
        (format (cdr (assoc kind *image-formats*)))
        (notes '()))
    (flet ((note (text &optional face) (push (cons text face) notes))
           (done (image) (return-from decode-image (values image (reverse notes)))))
      (unless format (done nil))
      (unless (eq status :ready)
        (note (format nil ", not shown: ~a" (no-sandbox-words status)) :comment)
        (done nil))
      (unless decoder
        (note (format nil ", not shown: no program for pictures (install ~a: ~a)"
                      (getf (first *image-decoders*) :name) (first (getf (first *image-decoders*) :program)))
              :comment)
        (done nil))
      (let* ((octets (ignore-errors (sb-posix:stat-size (sb-posix:fstat fd))))
             (head (and octets (ignore-errors (tuatara.io:read-fd-start fd "the picture" octets 65536))))
             (pixels (picture-pixels fd head))
             (memory (picture-memory kind pixels octets))
             (large (and memory tuatara-user:*sandbox-memory* (> memory tuatara-user:*sandbox-memory*))))
        (when (and memory *picture-memory* (> memory *picture-memory*))
          (note (format nil ", not shown: ~:[it~;~:*~,1f megapixels~] would take ~a to draw, more than *picture-memory* allows (~a)"
                        (and pixels (/ pixels 1d6)) (octets-words memory) (octets-words *picture-memory*))
                :comment)
          (done nil))
      (destructuring-bind (program &rest arguments) (getf decoder :program)
        (multiple-value-bind (output outcome said)
            (let ((tuatara-user:*sandbox-memory* memory)
                  ;; all of one run's programs together given no less than it
                  (tuatara-user:*sandbox-total-memory* (and tuatara-user:*sandbox-total-memory*
                                                            (max tuatara-user:*sandbox-total-memory* (or memory 0)))))
            (call-in-picture-turn
             large cancel
             (lambda ()
               (apply #'tuatara.io:sandbox-run program
                      (decoder-arguments
                       arguments
                       (list :input (list (format nil "~a:-[0]" format))
                             :size (list (format nil "~dx~d>" (max 1 width) (max 1 height)))
                             :density (and density (list "-density" (princ-to-string density)))
                             :jpeg-hint (and (eq kind :jpeg) (getf decoder :jpeg-hint)
                                             (destructuring-bind (option value) (getf decoder :jpeg-hint)
                                               (list option (format nil value (max 1 width) (max 1 height)))))))
                      :input fd :cancel cancel :output-limit (+ 4096 (* 4 (max 1 width) (max 1 height)))
                      ;; a TIFF read from a pipe is copied to a file of the sandbox's first
                      :file-limit (max tuatara.io:*sandbox-file-limit* (+ (* 1024 1024) (or octets 0)))
                      (and (getf decoder :sandbox) (funcall (getf decoder :sandbox)))))))
          (case outcome
            (:cancelled (return-from decode-image (values nil nil)))
            (:done
             (let ((image (decoded-image output))
                   (damage (picture-damage-words said)))
               (unless image (note (format nil ", not shown: ~a gave no picture" program) :comment))
               (when (and image damage) (note (format nil ", damaged: ~a" damage) :warning))
               (done image)))
            (:timeout
             (note (format nil ", not shown: ~a" (tuatara.io:sandbox-outcome-words outcome said :label program)) :comment)
             (done nil))
            (t
             ;; a TIFF-shaped file LibRaw does not develop, a scanner's say: drawn as a TIFF
             (when (and (eq kind :raw) head (tuatara.io:tiff-octets-p head))
               (return-from decode-image (decode-image fd :tiff width height :cancel cancel :density density)))
             ;; a Fujifilm raw LibRaw does not develop (its compressed kind, on many cores): the camera's own JPEG of it
             (let ((preview (and (eq kind :raw) (raf-preview fd head))))
               (when preview
                 (let ((copy (tuatara.io:memory-file "tuatara-raw-preview" preview :kept nil)))
                   (unwind-protect
                        (return-from decode-image (decode-image copy :jpeg width height :cancel cancel))
                     (ignore-errors (sb-posix:close copy))))))
             (note (format nil ", not shown: ~a" (tuatara.io:sandbox-outcome-words outcome said :label program)) :comment)
             (done nil)))))))))

(defun sandboxed-mode-line (editor buffer)
  "What the mode line of a buffer adds when what it shows was made by a
program shut in by the sandbox (its :sandboxed local): Sandboxed, as the
text of a PDF, or the picture of an image or a page."
  (declare (ignore editor))
  (and (tuatara.buffer:buffer-local buffer :sandboxed) "Sandboxed"))
