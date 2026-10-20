;;;; src/io/kinds.lisp --- What a file is, told by its first octets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file is recognised by what it holds, as the file program recognises it,
;;;; never by its name: the octets its kind of file starts with (its magic),
;;;; and for some a few fields after them, such as an image's width and
;;;; height. Nothing is decoded beyond those fields; the octets are only read.
;;;; A name whose extension is not one its kind has is told apart
;;;; (KIND-NAME-DISAGREES-P): a picture named .txt, a program named .jpg.

(in-package #:tuatara.io)

(defun octet-at (octets i)
  "The octet at I of OCTETS, or NIL past their end."
  (and (< -1 i (length octets)) (aref octets i)))

(defun octets-integer (octets at count &key big-endian)
  "The unsigned integer in COUNT octets of OCTETS from AT, least significant
first unless BIG-ENDIAN; NIL when they are not all there."
  (and (<= 0 at) (<= (+ at count) (length octets))
       (loop for k below count
             sum (ash (aref octets (+ at k)) (* 8 (if big-endian (- count k 1) k))))))

(defun octets-signed (value bits)
  "VALUE, an unsigned integer of BITS bits, as the signed one it stands for; NIL for NIL."
  (and value (if (logbitp (1- bits) value) (- value (ash 1 bits)) value)))

(defun octets-le (octets start count)
  "The unsigned number in COUNT octets of OCTETS from START, least significant
first; NIL past their end (OCTETS-INTEGER)."
  (octets-integer octets start count))

(defun octets-be (octets start count)
  "The unsigned number in COUNT octets of OCTETS from START, most significant
first; NIL past their end (OCTETS-INTEGER)."
  (octets-integer octets start count :big-endian t))

(declaim (inline octets-put-integer))
(defun octets-put-integer (octets value count &key at big-endian)
  "Write the unsigned integer VALUE as COUNT octets, least significant first
unless BIG-ENDIAN: into OCTETS from AT, or without AT at their end, OCTETS
then a vector with a fill pointer that grows. Returns OCTETS."
  (dotimes (k count octets)
    (let ((octet (ldb (byte 8 (* 8 (if big-endian (- count k 1) k))) value)))
      (if at
          (setf (aref octets (+ at k)) octet)
          (vector-push-extend octet octets)))))

(defun octets-at-p (octets prefix &optional (at 0))
  "True when OCTETS hold PREFIX, a string of ASCII or a list of octets, at AT."
  (let ((prefix (if (stringp prefix) (map 'list #'char-code prefix) prefix)))
    (and (<= 0 at) (<= (+ at (length prefix)) (length octets))
         (loop for o in prefix for i from at always (= o (aref octets i))))))

(defun base64-value (octet)
  "The six bits OCTET stands for in base64, or NIL for an octet that is not one of its letters."
  (cond ((<= 65 octet 90) (- octet 65)) ((<= 97 octet 122) (- octet 71)) ((<= 48 octet 57) (+ octet 4))
        ((= octet 43) 62) ((= octet 47) 63) (t nil)))

(defun base64-octets (octets &key (start 0) (end (length octets)))
  "The octets base64 in OCTETS from START below END stands for: what is not
of its letters (line ends, spaces) passed over, and a = ending it."
  (let ((out (make-array (floor (* 3 (- end start)) 4) :element-type '(unsigned-byte 8)))
        (filled 0) (bits 0) (count 0))
    (loop for i from start below end
          for octet = (aref octets i)
          until (= octet 61)
          do (let ((value (base64-value octet)))
               (when value
                 (setf bits (logior (ash bits 6) value))
                 (incf count)
                 (when (= count 4)
                   (setf (aref out filled) (ldb (byte 8 16) bits)
                         (aref out (+ filled 1)) (ldb (byte 8 8) bits)
                         (aref out (+ filled 2)) (ldb (byte 8 0) bits))
                   (incf filled 3)
                   (setf bits 0 count 0)))))
    ;; what a last group cut short holds
    (case count
      (2 (setf (aref out filled) (ldb (byte 8 4) bits)) (incf filled 1))
      (3 (setf (aref out filled) (ldb (byte 8 10) bits) (aref out (1+ filled)) (ldb (byte 8 2) bits)) (incf filled 2)))
    (subseq out 0 filled)))

(defparameter *base64-digits*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The 64 letters of base64, each standing for six bits, in order.")

(defun base64-string (octets &key (start 0) (end (length octets)))
  "The octets of OCTETS from START below END in base64, = filling its last
group: what BASE64-OCTETS reads back."
  (let* ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (count (- end start))
         (out (make-string (* 4 (ceiling count 3))))
         (digits *base64-digits*))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type simple-string digits out) (fixnum start end count))
    (loop for i fixnum from start below end by 3
          for o fixnum from 0 by 4
          do (let* ((left (- end i))
                    (n (logior (ash (aref octets i) 16)
                               (if (> left 1) (ash (aref octets (+ i 1)) 8) 0)
                               (if (> left 2) (aref octets (+ i 2)) 0))))
               (setf (schar out o) (schar digits (ldb (byte 6 18) n))
                     (schar out (+ o 1)) (schar digits (ldb (byte 6 12) n))
                     (schar out (+ o 2)) (if (> left 1) (schar digits (ldb (byte 6 6) n)) #\=)
                     (schar out (+ o 3)) (if (> left 2) (schar digits (ldb (byte 6 0) n)) #\=))))
    out))

(defun octets-ascii (octets start end)
  "The octets of OCTETS from START below END (or their end) as ASCII, a
character standing for each; NIL when START is past their end."
  (let ((end (min end (length octets))))
    (and (< start end) (tuatara.text:octets-latin-1 octets :start start :end end))))

(defun octets-find (octets needle &key (start 0) (end (length octets)))
  "Where the ASCII string NEEDLE first stands in OCTETS from START below END, or NIL."
  (search (map 'vector #'char-code needle) octets :start2 start :end2 (min end (length octets))))

(defun pdf-header-at (octets)
  "Where %PDF- followed by a digit begins among the first 1,024 octets of
OCTETS, as a PDF reader looks for it (a PDF may have other octets before its
header); NIL when it is not there."
  (loop for at = (octets-find octets "%PDF-" :end 1024) then (octets-find octets "%PDF-" :start (1+ at) :end 1024)
        while at
        when (let ((digit (octet-at octets (+ at 5)))) (and digit (<= 48 digit 57)))
          return at))

(defparameter *compound-file-signature* '(#xD0 #xCF #x11 #xE0 #xA1 #xB1 #x1A #xE1)
  "The eight octets a compound file starts with: an Office document of before
2007, an Outlook item, an installer.")

(defun size-words (width height)
  "WIDTH and HEIGHT as a description writes them: 640 × 480; NIL when either is not known."
  (and width height (format nil "~d × ~d" width height)))

(defun joined-words (&rest parts)
  "PARTS that are not NIL, joined with commas."
  (format nil "~{~a~^, ~}" (remove nil parts)))

(defun png-size (o)
  "The width and height of the PNG image whose first octets are O, as two
values, from the image header that comes straight after the signature; NIL
when that header is not there, is cut short or holds values no PNG has."
  (let ((width (octets-be o 16 4)) (height (octets-be o 20 4)))
    (if (and (eql (octets-be o 8 4) 13) (octets-at-p o "IHDR" 12)
             width height (plusp width) (plusp height)
             (member (octet-at o 24) '(1 2 4 8 16)) (member (octet-at o 25) '(0 2 3 4 6)))
        (values width height)
        (values nil nil))))

(defun png-words (o)
  "A PNG image described: its size, depth and colours, from its image header
\(PNG-SIZE); a damaged PNG when that header is not there, is cut short or
holds values no PNG has."
  (multiple-value-bind (width height) (png-size o)
    (if width
        (joined-words "PNG image" (size-words width height)
                      (format nil "~d-bit ~a" (octet-at o 24)
                              (case (octet-at o 25) (0 "grey") (2 "RGB") (3 "with a palette") (4 "grey and alpha") (t "RGBA"))))
        "damaged PNG image: its image header is missing or wrong")))

(defun jpeg-segments (octets function)
  "Call FUNCTION with the marker, where the data begins and where it ends of
each segment of the JPEG in OCTETS before its picture data (its SOS or EOI
marker), passing over fill octets and the markers that stand alone (RSTn,
TEM); return the first true value FUNCTION returns, or NIL. At most 2,000
markers are looked at."
  (let ((at 2))
    (loop repeat 2000
          while (and (< (+ at 4) (length octets)) (= (aref octets at) #xFF))
          do (let ((marker (aref octets (1+ at))))
               (cond ((= marker #xFF) (incf at))
                     ((or (<= #xD0 marker #xD7) (= marker #x01)) (incf at 2))
                     ((member marker '(#xDA #xD9)) (return nil))
                     (t (let ((length (octets-be octets (+ at 2) 2)))
                          (unless (and length (>= length 2)) (return nil))
                          (let ((result (funcall function marker (+ at 4) (min (length octets) (+ at 2 length)))))
                            (when result (return result)))
                          (incf at (+ 2 length)))))))))

(defun jpeg-size (o)
  "The width and height of the JPEG image whose first octets are O, as two
values, from its first frame header; NIL when that is not among O."
  (let ((size (jpeg-segments o (lambda (marker start end)
                                 (and (member marker '(#xC0 #xC1 #xC2 #xC3 #xC5 #xC6 #xC7 #xC9 #xCA #xCB #xCD #xCE #xCF))
                                      (<= (+ start 5) end)
                                      (list (octets-be o (+ start 3) 2) (octets-be o (+ start 1) 2)))))))
    (values (first size) (second size))))

(defun jpeg-words (o)
  "A JPEG image described: its size, from its first frame header."
  (joined-words "JPEG image" (multiple-value-call #'size-words (jpeg-size o))
                (and (octets-find o "Exif" :end 64) "with EXIF data")))

(defun gif-size (o)
  "The width and height of the GIF image whose first octets are O, as two values."
  (values (octets-le o 6 2) (octets-le o 8 2)))

(defun bmp-size (o)
  "The width and height of the BMP image whose first octets are O, as two
values (a height written negative, for rows kept top first, made positive)."
  (values (octets-le o 18 4) (let ((h (octets-signed (octets-le o 22 4) 32))) (and h (abs h)))))

(defun psd-size (o)
  "The width and height of the Photoshop image whose first octets are O, as two values."
  (values (octets-be o 18 4) (octets-be o 14 4)))

(defun tiff-octets-p (octets)
  "True when OCTETS begin as a TIFF-shaped file does: II*0, its numbers least
significant first, or MM0*, most significant first."
  (or (octets-at-p octets '(73 73 42 0)) (octets-at-p octets '(77 77 0 42))))

(defun tiff-entries (octets &key (start 0) ifd (most 512) (little (eql (octet-at octets start) 73)))
  "The entries of a directory of the TIFF structure in OCTETS from START (a
TIFF-shaped file, or a picture's EXIF): the one at IFD, an offset from START,
or else the first. Each is (TAG TYPE COUNT VALUE WHERE): VALUE the number a
SHORT or LONG holds, or the offset of what does not fit in four octets, and
WHERE the offset of those four octets. At most MOST entries; numbers read
LITTLE-endian (as standard, when the structure begins II) or not. As a second
value a function of an offset from START and a length reading a number that
way, and as a third the offset of the next directory. NIL when the directory
is not among OCTETS."
  (let* ((int (lambda (at n) (if little (octets-le octets (+ start at) n) (octets-be octets (+ start at) n))))
         (ifd (or ifd (funcall int 4 4)))
         (count (and ifd (funcall int ifd 2))))
    (values (and count
                 (loop for i below (min count most)
                       for entry = (+ ifd 2 (* 12 i))
                       for tag = (funcall int entry 2)
                       for type = (funcall int (+ entry 2) 2)
                       for n = (funcall int (+ entry 4) 4)
                       for value = (case type (3 (funcall int (+ entry 8) 2)) (t (funcall int (+ entry 8) 4)))
                       while (and tag type n value)
                       collect (list tag type n value (+ entry 8))))
            int
            (and count (funcall int (+ ifd 2 (* 12 (min count most))) 4)))))

(defun tiff-size (o)
  "The width and height of the first image of the TIFF (or TIFF-shaped raw)
file whose first octets are O, as two values: its first directory's tags 256
and 257; NIL when they are not among O."
  (let ((entries (tiff-entries o)))
    (values (fourth (assoc 256 entries)) (fourth (assoc 257 entries)))))

(defparameter *raw-camera-makers*
  '(("NIKON" . "Nikon NEF") ("SONY" . "Sony ARW") ("PENTAX" . "Pentax PEF") ("RICOH" . "Pentax PEF")
    ("SAMSUNG" . "Samsung SRW") ("Hasselblad" . "Hasselblad") ("Phase One" . "Phase One IIQ") ("Leaf" . "Leaf MOS")
    ("Mamiya" . "Mamiya MEF") ("KODAK" . "Kodak") ("EASTMAN KODAK" . "Kodak") ("SEIKO EPSON" . "Epson ERF")
    ("LEICA" . "Leica") ("SIGMA" . "Sigma"))
  "The makers of cameras whose raw files are TIFF-shaped, each with what their
raw is called, as a TIFF file's Make tag begins: a TIFF from such a maker,
not one of its scanners (whose files have no camera's maker note), is its raw.")

(defun tiff-raw-words (o)
  "What the TIFF-shaped file whose first octets are O is when it is a
camera's raw rather than a TIFF image: Canon CR2 (CR at 8), a DNG (its
DNGVersion tag), or the raw of a maker of *RAW-CAMERA-MAKERS* (its Make tag,
with an EXIF directory); NIL for a TIFF image."
  (let ((entries (tiff-entries o)))
    (cond ((octets-at-p o "CR" 8) "Canon CR2")
          ((assoc 50706 entries) "DNG")
          (t (let* ((make (assoc 271 entries))
                    (count (and make (third make)))
                    (at (and make (if (<= count 4) nil (fourth make))))
                    (name (and at (<= (+ at count) (length o)) (octets-ascii o at (+ at (min count 64)))))
                    (maker (and name (find-if (lambda (m) (uiop:string-prefix-p (car m) (string-trim '(#\Nul #\Space) name)))
                                              *raw-camera-makers*))))
               (and maker (assoc 34665 entries) (cdr maker)))))))

(defun raw-words (o)
  "What camera's raw the file whose first octets are O is, when it is one of
a shape of its own (not TIFF's): Fujifilm RAF, Canon CR3 and CRW, Olympus
ORF, Panasonic or Leica RW2, Minolta MRW; NIL otherwise."
  (flet ((is (prefix &optional (at 0)) (octets-at-p o prefix at)))
    (cond ((is "FUJIFILMCCD-RAW") "Fujifilm RAF")
          ((and (is "ftyp" 4) (is "crx " 8)) "Canon CR3")
          ((and (or (is (list 73 73)) (is (list 77 77))) (is "HEAPCCDR" 6)) "Canon CRW")
          ((or (is (list 73 73 82 79)) (is (list 73 73 82 83)) (is (list 77 77 79 82))) "Olympus ORF")
          ((is (list 73 73 85 0)) "Panasonic or Leica RW2")
          ((is (list 0 77 82 77)) "Minolta MRW"))))

(defun webp-size (o)
  "The width and height of the WebP image whose first octets are O, as two
values, from whichever of its three headers it has; NIL when none is among O."
  (cond ((and (octets-at-p o "VP8X" 12) (octets-le o 27 3))
         (values (1+ (octets-le o 24 3)) (1+ (octets-le o 27 3))))
        ((and (octets-at-p o "VP8L" 12) (octets-le o 21 4))
         (let ((bits (octets-le o 21 4)))
           (values (1+ (logand bits #x3FFF)) (1+ (logand (ash bits -14) #x3FFF)))))
        ((and (octets-at-p o "VP8 " 12) (octets-le o 28 2))
         (values (logand (octets-le o 26 2) #x3FFF) (logand (octets-le o 28 2) #x3FFF)))
        (t (values nil nil))))

(defun webp-words (o)
  "A WebP image described: its size, from whichever of its three headers it has."
  (joined-words "WebP image" (multiple-value-call #'size-words (webp-size o))))

(defun elf-words (o)
  "An ELF file described: its width, what it is, the processor it is for."
  (let ((class (octet-at o 4)) (le (eql (octet-at o 5) 1)))
    (flet ((field (start count) (if le (octets-le o start count) (octets-be o start count))))
      (joined-words (format nil "ELF ~a-bit ~a" (case class (1 32) (2 64) (t "?"))
                     (case (field 16 2) (1 "relocatable object") (2 "executable") (3 "shared object or executable")
                       (4 "core dump") (t "file")))
             (case (field 18 2)
               (3 "x86") (62 "x86-64") (40 "ARM") (183 "AArch64") (243 "RISC-V") (8 "MIPS") (20 "PowerPC")
               (21 "PowerPC 64") (22 "IBM Z") (t nil))))))

(defun pe-words (o)
  "A Windows program described: executable or DLL, its format and processor."
  (let ((pe (octets-le o #x3C 4)))
    (if (and pe (octets-at-p o (list 80 69 0 0) pe))
        (let ((machine (octets-le o (+ pe 4) 2))
              (characteristics (octets-le o (+ pe 22) 2))
              (magic (octets-le o (+ pe 24) 2)))
          (joined-words (format nil "Windows ~:[program~;DLL~]~@[ (~a)~]"
                         (and characteristics (logtest characteristics #x2000))
                         (case magic (#x10B "PE32") (#x20B "PE32+") (t nil)))
                 (case machine (#x14C "x86") (#x8664 "x86-64") (#xAA64 "ARM64") (#x1C0 "ARM") (t nil))))
        "DOS program")))

(defun box-children (octets start end)
  "The boxes (atoms) in OCTETS from START below END, each (TYPE DATA-START
DATA-END), as MP4, QuickTime and HEIF files keep their parts; at most 10,000,
one whose size does not fit ending the list."
  (let ((boxes '()) (at start))
    (loop while (and (<= (+ at 8) end) (< (length boxes) 10000))
          do (let ((size (octets-be octets at 4))
                   (type (octets-ascii octets (+ at 4) (+ at 8)))
                   (header 8))
               (case size
                 (1 (setf size (octets-be octets (+ at 8) 8) header 16))
                 (0 (setf size (- end at))))
               (when (or (null size) (< size header) (> (+ at size) end)) (return))
               (push (list type (+ at header) (+ at size)) boxes)
               (incf at size)))
    (nreverse boxes)))

(defun box-find (octets start end &rest path)
  "The box at PATH (types, the first among those from START below END, then
within it) in OCTETS, as (TYPE DATA-START DATA-END), or NIL. A meta box's
version and flags are passed over when it has them."
  (let ((box (list nil start end)))
    (dolist (type path box)
      (let* ((inner (second box))
             ;; meta is a full box in MP4 and HEIF (four octets of version and flags), not in QuickTime
             (inner (if (and (equal (first box) "meta") (not (octets-at-p octets "hdlr" (+ inner 4)))) (+ inner 4) inner)))
        (setf box (find type (box-children octets inner (third box)) :key #'first :test #'equal))
        (unless box (return nil))))))

(defun ispe-size (o)
  "The width and height of the largest image a HEIF or AVIF file whose first
octets are O says it holds, as two values: the ispe boxes among its item
properties (meta, iprp, ipco), the picture's and a thumbnail's smaller; NIL
when none is among O."
  (let ((ipco (box-find o 0 (length o) "meta" "iprp" "ipco"))
        (best nil))
    (when ipco
      (loop for (type start end) in (box-children o (second ipco) (third ipco))
            when (and (equal type "ispe") (<= (+ start 12) end))
              do (let ((width (octets-be o (+ start 4) 4)) (height (octets-be o (+ start 8) 4)))
                   (when (or (null best) (> (* width height) (* (first best) (second best))))
                     (setf best (list width height))))))
    (values (first best) (second best))))

(defun ftyp-words (o)
  "An ISO media file described by its brand: MP4 video, a QuickTime movie,
MPEG-4 audio, 3GP, or a HEIF or AVIF image, with its size."
  (let ((brand (octets-ascii o 8 12)))
    (values (cond ((member brand '("heic" "heix" "heim" "heis" "mif1" "msf1") :test #'equal) (joined-words "HEIF image" (multiple-value-call #'size-words (ispe-size o))))
                  ((member brand '("avif" "avis") :test #'equal) (joined-words "AVIF image" (multiple-value-call #'size-words (ispe-size o))))
                  ((equal brand "qt  ") "QuickTime movie")
                  ((member brand '("M4A " "M4B " "M4P ") :test #'equal) "MPEG-4 audio")
                  ((and brand (uiop:string-prefix-p "3g" brand)) "3GP video")
                  (t "MP4 video"))
            (cond ((member brand '("heic" "heix" "heim" "heis" "mif1" "msf1") :test #'equal) :heif)
                  ((member brand '("avif" "avis") :test #'equal) :avif)
                  (t :mp4)))))

(defun ebml-doc-type (o)
  "The document type a Matroska or WebM file names in its header, or NIL."
  (let ((at (search (list #x42 #x82) o :end2 (min 64 (length o)))))
    (and at (octet-at o (+ at 2))
         (let ((size (logand (aref o (+ at 2)) #x7F)))
           (octets-ascii o (+ at 3) (+ at 3 size))))))

(defun zip-words (o)
  "A ZIP archive described by what it holds: a document of Office or
OpenDocument, an EPUB book, a Java or Android package, or else plain."
  (let ((mimetype (and (octets-at-p o "mimetype" 30)
                       (octets-ascii o 38 (min (length o) 120)))))
    (cond ((and mimetype (search "application/epub+zip" mimetype)) (values "EPUB book" :epub))
          ((and mimetype (search "opendocument.text" mimetype)) (values "OpenDocument text" :odt))
          ((and mimetype (search "opendocument.spreadsheet" mimetype)) (values "OpenDocument spreadsheet" :ods))
          ((and mimetype (search "opendocument.presentation" mimetype)) (values "OpenDocument presentation" :odp))
          ((octets-find o "word/") (values "Word document (OOXML)" :docx))
          ((octets-find o "xl/") (values "Excel workbook (OOXML)" :xlsx))
          ((octets-find o "ppt/") (values "PowerPoint presentation (OOXML)" :pptx))
          ((octets-find o "AndroidManifest.xml") (values "Android package" :apk))
          ((octets-find o "META-INF/MANIFEST.MF") (values "Java archive" :jar))
          (t (values "ZIP archive" :zip)))))

(defun text-words (o)
  "What a file of text is when its first octets tell: a script, a desktop
launcher, a key or certificate, SVG, HTML or XML, PostScript or RTF; NIL for
plain text, and as a second value its kind."
  (let* ((head (octets-ascii o 0 (min (length o) 1024)))
         (start (and head (string-left-trim '(#\Space #\Tab #\Return #\Newline #\Page) head))))
    (cond ((null head) nil)
          ((octets-at-p o "#!")
           (values (format nil "script, run by ~a"
                           (string-trim " " (subseq head 2 (or (position #\Newline head) (length head)))))
                   :script))
          ((octets-at-p o "[Desktop Entry]") (values "desktop launcher (a program to start)" :desktop))
          ((octets-at-p o "-----BEGIN ")
           (let ((end (search "-----" head :start2 11)))
             (values (format nil "PEM ~(~a~)" (subseq head 11 (or end 11))) :pem)))
          ((octets-at-p o "%!PS") (values "PostScript document" :postscript))
          ((octets-at-p o "{\\rtf") (values "RTF document" :rtf))
          ((or (and (>= (length start) 4) (string-equal "<svg" start :end2 4))
               (and (>= (length start) 5) (string-equal "<?xml" start :end2 5) (search "<svg" head)))
           (values "SVG image" :svg))
          ((or (and (>= (length start) 9) (string-equal "<!doctype" start :end2 9) (search "html" head :test #'char-equal))
               (and (>= (length start) 5) (string-equal "<html" start :end2 5)))
           (values "HTML document" :html))
          ((and (>= (length start) 5) (string-equal "<?xml" start :end2 5)) (values "XML document" :xml)))))

(defparameter *kind-extensions*
  '((:png "png") (:jpeg "jpg" "jpeg" "jpe" "jfif") (:gif "gif") (:webp "webp") (:bmp "bmp" "dib")
    (:tiff "tif" "tiff") (:raw "dng" "nef" "nrw" "cr2" "cr3" "crw" "arw" "orf" "rw2" "rwl" "pef" "srw" "raf" "mrw"
                                "3fr" "fff" "iiq" "mos" "mef" "erf" "kdc" "dcr" "x3f")
    (:ico "ico" "cur") (:heif "heic" "heif")
    (:avif "avif") (:psd "psd" "psb") (:xcf "xcf") (:svg "svg") (:pdf "pdf") (:postscript "ps" "eps" "ai")
    (:rtf "rtf") (:ole "doc" "xls" "ppt" "msg" "msi" "vsd" "pub" "dot" "xlt" "pot" "pps")
    (:docx "docx" "docm" "dotx" "dotm") (:xlsx "xlsx" "xlsm" "xltx" "xltm") (:pptx "pptx" "pptm" "potx" "potm" "ppsx" "ppsm")
    (:odt "odt" "ott") (:ods "ods" "ots") (:odp "odp" "otp") (:epub "epub") (:apk "apk") (:jar "jar" "war" "ear")
    (:zip "zip" "whl" "xpi" "crx" "nupkg" "cbz" "kmz" "ipa")
    (:gzip "gz" "tgz") (:bzip2 "bz2" "tbz" "tbz2") (:xz "xz" "txz") (:zstd "zst" "zstd" "tzst") (:lz4 "lz4")
    (:7z "7z") (:rar "rar" "cbr") (:tar "tar") (:cpio "cpio") (:ar "a" "deb" "lib") (:rpm "rpm")
    (:iso "iso") (:squashfs "squashfs" "sqsh" "sqfs" "snap") (:qcow "qcow" "qcow2") (:vmdk "vmdk") (:vhdx "vhdx")
    (:pe "exe" "dll" "sys" "efi" "scr" "ocx" "cpl" "drv" "com") (:java-class "class") (:wasm "wasm") (:fasl "fasl")
    (:mp4 "mp4" "m4v" "m4a" "m4b" "mov" "3gp" "3g2" "f4v" "qt") (:matroska "mkv" "mka" "mks" "mk3d" "webm")
    (:avi "avi") (:wav "wav") (:mp3 "mp3") (:ogg "ogg" "oga" "ogv" "opus" "spx") (:flac "flac") (:midi "mid" "midi")
    (:mpeg "mpg" "mpeg" "vob" "m2v") (:aiff "aif" "aiff" "aifc")
    (:sqlite "sqlite" "sqlite3" "db" "db3" "s3db" "sl3") (:pcap "pcap" "cap" "dmp") (:pcapng "pcapng")
    (:lnk "lnk") (:evtx "evtx") (:pst "pst" "ost") (:minidump "dmp" "mdmp")
    (:font "ttf" "otf" "ttc" "woff" "woff2") (:desktop "desktop") (:pem "pem" "crt" "cer" "key" "pub" "csr")
    (:html "html" "htm" "xhtml") (:xml "xml" "xsd" "xsl" "xslt" "rss" "atom" "plist" "svg" "kml" "gpx"))
  "For each kind of file OCTETS-KIND tells, the extensions its names usually
have; a kind not here, or a script or an executable, has no rule.")

(defun openpgp-packet-header (octets at)
  "The OpenPGP packet whose header is in OCTETS at AT: three values, its tag,
where its body begins and its length (NIL for one whose length is given in
parts, as data packets have it); NIL when no packet header is there."
  (let ((b (octet-at octets at)))
    (when (and b (logbitp 7 b))
      (if (logbitp 6 b)
          (let ((l (octet-at octets (+ at 1))))
            (cond ((null l) nil)
                  ((< l 192) (values (logand b #x3F) (+ at 2) l))
                  ((< l 224) (let ((l2 (octet-at octets (+ at 2))))
                               (and l2 (values (logand b #x3F) (+ at 3) (+ (ash (- l 192) 8) l2 192)))))
                  ((= l 255) (let ((n (octets-be octets (+ at 2) 4))) (and n (values (logand b #x3F) (+ at 6) n))))
                  (t (values (logand b #x3F) (+ at 2) nil))))
          (let ((tag (ldb (byte 4 2) b)))
            (case (logand b 3)
              (0 (let ((n (octet-at octets (+ at 1)))) (and n (values tag (+ at 2) n))))
              (1 (let ((n (octets-be octets (+ at 1) 2))) (and n (values tag (+ at 3) n))))
              (2 (let ((n (octets-be octets (+ at 1) 4))) (and n (values tag (+ at 5) n))))
              (t (values tag (+ at 1) nil))))))))

(defparameter *openpgp-packet-versions* '((1 3 6) (3 4 5 6) (18 1 2) (20 1))
  "The packets an OpenPGP message encrypted by gpg begins with, by tag, each with
the versions it may have: a session key encrypted to a public key (1) or with a
passphrase (3), or the encrypted data itself (18, 20).")

(defun openpgp-packet-p (octets &optional (tags '(1 3 18 20)))
  "True when OCTETS begin with an OpenPGP packet of one of TAGS (as standard,
any an encrypted message may begin with) of a version gpg writes
\(*OPENPGP-PACKET-VERSIONS*)."
  (multiple-value-bind (tag body) (openpgp-packet-header octets 0)
    (and (member tag tags)
         (member (octet-at octets body) (rest (assoc tag *openpgp-packet-versions*)))
         t)))

(defun openpgp-encrypted-p (octets)
  "True when OCTETS begin an OpenPGP encrypted message: armoured (-----BEGIN
PGP MESSAGE-----), or a session key's packet, to a key or a passphrase, as
gpg writes them first."
  (or (octets-at-p octets "-----BEGIN PGP MESSAGE-----")
      (openpgp-packet-p octets '(1 3))))

(defun pdf-words (octets header)
  "What the PDF whose first octets are OCTETS is, in words, its %PDF- header
at HEADER: PDF document, version 1.7."
  (joined-words "PDF document" (let ((v (octets-ascii octets (+ header 5) (+ header 8)))) (and v (format nil "version ~a" v)))))

(defun octets-kind (octets)
  "What the file whose first octets are OCTETS is, told by them alone: two
values, a description in words (\"PNG image, 640 × 480, 8-bit RGBA\") and a
keyword (:PNG); NIL for a file of plain text or of a kind not told. The more
octets, up to 64 KB, the more kinds are told (a disc image's mark is 32 KB in)."
  (let ((o octets))
    (flet ((is (prefix &optional (at 0)) (octets-at-p o prefix at)))
      (cond
        ;; images
        ((is (list #x89 80 78 71 13 10 26 10)) (values (png-words o) :png))
        ((is (list #xFF #xD8 #xFF)) (values (jpeg-words o) :jpeg))
        ((or (is "GIF87a") (is "GIF89a")) (values (joined-words "GIF image" (multiple-value-call #'size-words (gif-size o))) :gif))
        ((and (is "RIFF") (is "WEBP" 8)) (values (webp-words o) :webp))
        ((and (is "BM") (member (octets-le o 14 4) '(12 40 52 56 64 108 124)))
         (values (joined-words "BMP image" (multiple-value-call #'size-words (bmp-size o))) :bmp))
        ((raw-words o) (values (format nil "Camera raw, ~a" (raw-words o)) :raw))
        ((tiff-octets-p o)
         (let ((raw (tiff-raw-words o)))
           (if raw
               (values (format nil "Camera raw, ~a" raw) :raw)
               (values (joined-words "TIFF image" (multiple-value-call #'size-words (tiff-size o))) :tiff))))
        ((and (is (list 0 0 1 0)) (octets-le o 4 2) (< 0 (octets-le o 4 2) 256))
         (values (format nil "Windows icon, ~d image~:p" (octets-le o 4 2)) :ico))
        ((is "8BPS") (values (joined-words "Photoshop image" (multiple-value-call #'size-words (psd-size o))) :psd))
        ((is "gimp xcf") (values "GIMP image" :xcf))
        ;; documents
        ((is "%PDF-") (values (pdf-words o 0) :pdf))
        ((is *compound-file-signature*)
         (values "Microsoft compound file (an Office document of before 2007, a mail, an installer)" :ole))
        ((or (is (list 80 75 3 4)) (is (list 80 75 5 6)) (is (list 80 75 7 8)))
         (multiple-value-bind (words kind) (zip-words o) (values words kind)))
        ;; compressed data and archives
        ((is (list #x1F #x8B))
         (values (joined-words "gzip compressed data"
                        (and (logtest (or (octet-at o 3) 0) 8)
                             (let ((end (and (> (length o) 10) (position 0 o :start 10))))
                               (and end (format nil "was ~a" (octets-ascii o 10 end))))))
                 :gzip))
        ((and (is "BZh") (octet-at o 3) (<= 49 (octet-at o 3) 57)) (values "bzip2 compressed data" :bzip2))
        ((is (list #xFD 55 122 88 90 0)) (values "xz compressed data" :xz))
        ((is (list #x28 #xB5 #x2F #xFD)) (values "Zstandard compressed data" :zstd))
        ((is (list 4 34 77 24)) (values "LZ4 compressed data" :lz4))
        ((is (list 55 122 #xBC #xAF 39 28)) (values "7-Zip archive" :7z))
        ((is (list 82 97 114 33 26 7)) (values (if (eql (octet-at o 6) 1) "RAR archive, version 5" "RAR archive") :rar))
        ((is "ustar" 257) (values "tar archive" :tar))
        ((or (is "070701") (is "070702") (is "070707")) (values "cpio archive" :cpio))
        ((is (format nil "!<arch>~%")) (values (if (is "debian-binary" 8) "Debian package" "ar archive") :ar))
        ((is (list #xED #xAB #xEE #xDB)) (values "RPM package" :rpm))
        ((is "CD001" 32769) (values "ISO 9660 disc image" :iso))
        ((is "hsqs") (values "SquashFS file system" :squashfs))
        ((is (list 81 70 73 #xFB)) (values "QEMU disk image (qcow)" :qcow))
        ((is "KDMV") (values "VMware disk image" :vmdk))
        ((is "vhdxfile") (values "Hyper-V disk image" :vhdx))
        ;; programs
        ((is (list #x7F 69 76 70)) (values (elf-words o) :elf))
        ((is "MZ") (values (pe-words o) :pe))
        ((or (is (list #xFE #xED #xFA #xCE)) (is (list #xFE #xED #xFA #xCF))
             (is (list #xCE #xFA #xED #xFE)) (is (list #xCF #xFA #xED #xFE)))
         (values "Mach-O program (macOS)" :mach-o))
        ((is (list #xCA #xFE #xBA #xBE))
         (let ((major (octets-be o 6 2)))
           (if (and major (<= 45 major 99))
               (values (format nil "Java class, version ~d" major) :java-class)
               (values "Mach-O universal program (macOS)" :mach-o))))
        ((is (list 0 97 115 109)) (values "WebAssembly module" :wasm))
        ((is "# FASL") (values "compiled Lisp (SBCL)" :fasl))
        ;; sound and video
        ((is "ftyp" 4) (multiple-value-bind (words kind) (ftyp-words o) (values words kind)))
        ((is (list #x1A #x45 #xDF #xA3))
         (let ((type (ebml-doc-type o)))
           (values (if (equal type "webm") "WebM video" "Matroska video") :matroska)))
        ((and (is "RIFF") (is "AVI " 8)) (values "AVI video" :avi))
        ((and (is "RIFF") (is "WAVE" 8)) (values "WAV audio" :wav))
        ((and (is "FORM") (or (is "AIFF" 8) (is "AIFC" 8))) (values "AIFF audio" :aiff))
        ((or (is "ID3") (and (eql (octet-at o 0) #xFF) (member (octet-at o 1) '(#xFB #xF3 #xF2 #xFA))))
         (values "MP3 audio" :mp3))
        ((is "OggS")
         (values (cond ((octets-find o "OpusHead" :end 64) "Opus audio")
                       ((octets-find o "vorbis" :end 64) "Ogg Vorbis audio")
                       ((octets-find o "theora" :end 64) "Ogg Theora video")
                       (t "Ogg data"))
                 :ogg))
        ((is "fLaC") (values "FLAC audio" :flac))
        ((is "MThd") (values "MIDI music" :midi))
        ((or (is (list 0 0 1 #xBA)) (is (list 0 0 1 #xB3))) (values "MPEG video" :mpeg))
        ;; data kept by programs and systems
        ((is (list 83 81 76 105 116 101 32 102 111 114 109 97 116 32 51 0)) (values "SQLite database" :sqlite))
        ((or (is (list #xD4 #xC3 #xB2 #xA1)) (is (list #xA1 #xB2 #xC3 #xD4))) (values "network capture (pcap)" :pcap))
        ((is (list 10 13 13 10)) (values "network capture (pcapng)" :pcapng))
        ((is (list 76 0 0 0 1 20 2 0)) (values "Windows shortcut" :lnk))
        ((is "regf") (values "Windows registry hive" :registry))
        ((is "!BDN") (values (case (octet-at o 10) ((14 15) "Outlook mailbox (ANSI, before 2003)") (23 "Outlook mailbox (Unicode)")
                                   (36 "Outlook mailbox (Unicode, 4 KB pages)") (t "Outlook mailbox"))
                             :pst))
        ((is (list 69 108 102 70 105 108 101 0)) (values "Windows event log" :evtx))
        ((is "MDMP") (values "Windows memory dump" :minidump))
        ((or (is (list 0 1 0 0 0)) (is "OTTO") (is "true") (is "ttcf") (is "wOFF") (is "wOF2")) (values "font" :font))
        ((openpgp-encrypted-p o) (values (if (is "-----") "OpenPGP encrypted message, armoured" "OpenPGP encrypted message") :openpgp))
        ;; a PDF with other octets before its header, and binary data after it
        ((and (pdf-header-at o) (binary-head-p o)) (values (pdf-words o (pdf-header-at o)) :pdf))
        ;; text in another encoding, then kinds of text
        ((is (list #xFF #xFE)) (values "UTF-16 text, little-endian" :utf-16))
        ((is (list #xFE #xFF)) (values "UTF-16 text, big-endian" :utf-16))
        (t (multiple-value-bind (words kind) (text-words o)
             (if words
                 (values words kind)
                 (and (is (list #xEF #xBB #xBF)) (values "UTF-8 text, with a byte order mark" :utf-8)))))))))

(defparameter *picture-size-functions*
  '((:png . png-size) (:jpeg . jpeg-size) (:gif . gif-size) (:webp . webp-size) (:bmp . bmp-size)
    (:tiff . tiff-size) (:heif . ispe-size) (:avif . ispe-size) (:psd . psd-size))
  "For each kind of picture OCTETS-KIND gives the size of, the function of its
first octets giving its width and height.")

(defun octets-picture-size (octets)
  "The width and height of the picture whose file's first octets are OCTETS,
as two values, as OCTETS-KIND's words give them (640 × 480); NIL when they
give none: no picture, a camera's raw, or its size not among OCTETS."
  (let ((function (cdr (assoc (nth-value 1 (octets-kind octets)) *picture-size-functions*))))
    (multiple-value-bind (width height) (and function (funcall function octets))
      (if (and width height) (values width height) (values nil nil)))))

(defun kind-name-disagrees-p (kind name)
  "True when NAME, a file's name, has an extension its KIND (OCTETS-KIND's
keyword) is not known by: a picture named .txt. A name with no extension, a
name starting with its only dot, and a kind without a rule never disagree."
  (let* ((extension (let ((type (native-file-type name))) (and type (string-downcase type))))
         (known (cdr (assoc kind *kind-extensions*))))
    (and extension known (not (member extension known :test #'string=)) t)))
