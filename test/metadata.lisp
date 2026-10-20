;;;; test/metadata.lisp --- M-x file-metadata: what a file is and says of itself, read by Tuatara alone.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test metadata
  :description "M-x file-metadata shows what the system knows of a file (kind by contents, size,
permissions, owner, its four times, SHA-256, extended attributes) and what it says of itself, read
by Tuatara alone: a photograph's EXIF and GPS position and thumbnail in JPEG, PNG, WebP, TIFF and
HEIF; PNG text; XMP and its edit history; a PDF's information, saves and what could act in it, from
an object stream too; MP3 and FLAC tags; an MP4's times and place; an Office file's account of
itself. Files made to mislead or loop are read as far as they make sense, never hanging; the file's
access time is left alone.")

;;; Test files, built here

(defun octets-of (&rest parts)
  "PARTS joined as octets: strings as Latin-1, integers as one octet each, vectors as they are."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase part
        (string (loop for c across part do (vector-push-extend (char-code c) out)))
        (integer (vector-push-extend part out))
        (vector (loop for o across part do (vector-push-extend o out)))))))

(defun le-octets (n count) "N as COUNT octets, least significant first." (tuatara.io:octets-put-integer (make-array count :element-type '(unsigned-byte 8)) n count :at 0))
(defun be-octets (n count) "N as COUNT octets, most significant first." (tuatara.io:octets-put-integer (make-array count :element-type '(unsigned-byte 8)) n count :at 0 :big-endian t))

(defun tiff-test-octets (image exif gps thumbnail)
  "A little-endian TIFF structure (EXIF) of the IMAGE, EXIF, GPS and THUMBNAIL
parts, each a list of (TAG TYPE VALUE): VALUE a string (ASCII), a list of
integers (SHORT, LONG) or of fractions (N . D) (RATIONAL), or octets."
  (let* ((image (append image (and exif '((#x8769 4 (:exif)))) (and gps '((#x8825 4 (:gps))))))
         (parts (remove nil (list (cons :image image) (and exif (cons :exif exif)) (and gps (cons :gps gps)) (and thumbnail (cons :thumbnail thumbnail)))))
         (places (let ((at 8)) (loop for (name . entries) in parts collect (cons name at) do (incf at (+ 2 (* 12 (length entries)) 4)))))
         (data-at (+ 8 (loop for (nil . entries) in parts sum (+ 2 (* 12 (length entries)) 4))))
         (head (octets-of "II" 42 0 (le-octets 8 4)))
         (ifds (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((value-octets (type value)
             (case type
               (2 (octets-of value 0))
               (3 (apply #'octets-of (mapcar (lambda (v) (le-octets v 2)) value)))
               (4 (apply #'octets-of (mapcar (lambda (v) (le-octets (if (keywordp v) (cdr (assoc v places)) v) 4)) value)))
               (5 (apply #'octets-of (mapcar (lambda (r) (octets-of (le-octets (car r) 4) (le-octets (cdr r) 4))) value)))
               (t (coerce value '(vector (unsigned-byte 8)))))))
      (loop for (name . entries) in parts
            do (loop for o across (le-octets (length entries) 2) do (vector-push-extend o ifds))
               (dolist (entry entries)
                 (destructuring-bind (tag type value) entry
                   (let* ((octets (value-octets type value))
                          (number (case type (2 (length octets)) (3 (/ (length octets) 2)) (4 (/ (length octets) 4)) (5 (/ (length octets) 8)) (t (length octets)))))
                     (loop for o across (octets-of (le-octets tag 2) (le-octets type 2) (le-octets number 4)) do (vector-push-extend o ifds))
                     (if (<= (length octets) 4)
                         (loop for i below 4 do (vector-push-extend (if (< i (length octets)) (aref octets i) 0) ifds))
                         (progn (loop for o across (le-octets (+ data-at (length data)) 4) do (vector-push-extend o ifds))
                                (loop for o across octets do (vector-push-extend o data)))))))
               ;; the picture's own part leads to its thumbnail's
               (loop for o across (le-octets (if (and (eq name :image) (assoc :thumbnail places)) (cdr (assoc :thumbnail places)) 0) 4)
                     do (vector-push-extend o ifds))))
    (octets-of head ifds data)))

(defparameter *exif-test-tiff*
  (tiff-test-octets '((#x010F 2 "Kea Cameras") (#x0110 2 "K1") (#x0131 2 "Darkroom 4.2") (#x0132 2 "2026:03:02 10:00:00")
                      (#x9C9C 1 #(72 0 105 0 0 0)))
                    '((#x9003 2 "2026:03:01 09:30:00") (#x829A 5 ((1 . 125))) (#x829D 5 ((28 . 10))) (#x8827 3 (200))
                      (#x9286 7 #(65 83 67 73 73 0 0 0 79 110 32 116 104 101 32 114 105 100 103 101)) (#x927C 7 #(1 2 3 4 5 6 7 8 9)))
                    '((1 2 "S") (2 5 ((41 . 1) (17 . 1) (12 . 1))) (3 2 "E") (4 5 ((174 . 1) (46 . 1) (30 . 1))) (5 1 #(0)) (6 5 ((123 . 10))))
                    '((#x0201 4 (0)) (#x0202 4 (4321))))
  "The EXIF of a photograph made for the tests: its camera, times, settings, a GPS position in Wellington, a thumbnail.")

(defparameter *xmp-test-packet*
  "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description rdf:about='' xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:xmpMM='http://ns.adobe.com/xap/1.0/mm/' xmlns:stEvt='http://ns.adobe.com/xap/1.0/sType/ResourceEvent#' xmp:CreatorTool='Kea Hamilton 3'><dc:creator><rdf:Seq><rdf:li>Ana Timaru</rdf:li></rdf:Seq></dc:creator><xmpMM:History><rdf:Seq><rdf:li stEvt:action='saved' stEvt:when='2026-03-02T10:00:00' stEvt:softwareAgent='Kea Hamilton 3'/></rdf:Seq></xmpMM:History></rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end='w'?>"
  "An XMP packet made for the tests: the program that made the file, who, and an edit.")

(defun jpeg-test-octets ()
  "A JPEG made for the tests: its EXIF, an XMP packet, a comment."
  (let ((exif (octets-of "Exif" 0 0 *exif-test-tiff*))
        (xmp (octets-of "http://ns.adobe.com/xap/1.0/" 0 *xmp-test-packet*))
        (comment (octets-of "Shot from the ridge")))
    (octets-of #xFF #xD8
               #xFF #xE1 (be-octets (+ 2 (length exif)) 2) exif
               #xFF #xE1 (be-octets (+ 2 (length xmp)) 2) xmp
               #xFF #xFE (be-octets (+ 2 (length comment)) 2) comment
               #xFF #xDA 0 2 #xFF #xD9)))

(defun png-chunk (type data)
  "A PNG chunk of TYPE holding DATA (its check left 0)."
  (octets-of (be-octets (length data) 4) type data 0 0 0 0))

(defun box (type &rest contents)
  "An MP4 box of TYPE holding CONTENTS."
  (let ((data (apply #'octets-of contents)))
    (octets-of (be-octets (+ 8 (length data)) 4) type data)))

(defun write-test-octets-file (path octets)
  "Write OCTETS to PATH; return PATH as a string."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence octets out))
  (namestring path))

(defun metadata-of (path)
  "The note M-x file-metadata makes of the file at PATH."
  (tuatara.defaults::install-metadata)
  (tuatara.defaults::file-metadata-note path))

;;; Pictures

(define-test (metadata photograph-read)
  (with-test-directory (dir)
    (let* ((path (write-test-octets-file (merge-pathnames "kea.jpg" dir) (jpeg-test-octets)))
           (note (metadata-of path)))
      (note-has note "#+title: Metadata of kea.jpg")
      (note-has note "- Kind :: JPEG image")
      (note-has note "- MIME type :: image/jpeg")
      (note-has note "- Permissions :: -rw-")
      (note-has note "- SHA-256 :: ")
      (note-has note "* EXIF (in its APP1 segment)")
      (note-has note "- Position :: -41.286667, 174.775000 (12.3 m above sea level)")
      (note-has note "- Thumbnail :: 4,321 octets, a picture of its own kept inside it")
      (note-has note "- Make :: Kea Cameras")
      (note-has note "- Model :: K1")
      (note-has note "- Software :: Darkroom 4.2")
      (note-has note "- DateTime :: 2026:03:02 10:00:00")
      (note-has note "- DateTimeOriginal :: 2026:03:01 09:30:00")
      (note-has note "- ExposureTime :: 1/125")
      (note-has note "- FNumber :: 2.8")
      (note-has note "- ISOSpeedRatings :: 200")
      (note-has note "- UserComment :: On the ridge")
      (note-has note "- XPComment :: Hi")
      (note-has note "- MakerNote :: 9 octets of the maker's own notes (not read)")
      (note-has note "- GPSLatitudeRef :: S")
      (note-has note "- Comment :: Shot from the ridge")
      (note-has note "* XMP")
      (note-has note "- xmp:CreatorTool :: Kea Hamilton 3")
      (note-has note "- dc:creator :: Ana Timaru")
      (note-has note "- xmpMM:History :: stEvt:action saved, stEvt:when 2026-03-02T10:00:00, stEvt:softwareAgent Kea Hamilton 3"))))

(define-test (metadata other-pictures)
  (with-test-directory (dir)
    ;; PNG: its EXIF, its text (plain, packed, international) and when it says it was changed
    (let* ((png (write-test-octets-file
                 (merge-pathnames "kea.png" dir)
                 (octets-of #x89 "PNG" 13 10 26 10
                            (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                            (png-chunk "tEXt" (octets-of "Author" 0 "Ana Timaru"))
                            (png-chunk "iTXt" (octets-of "Description" 0 0 0 "en" 0 0 "Kea on a rock"))
                            (png-chunk "tIME" (octets-of (be-octets 2026 2) 3 2 10 0 0))
                            (png-chunk "eXIf" *exif-test-tiff*)
                            (png-chunk "IEND" #()))))
           (note (metadata-of png)))
      (note-has note "* EXIF (in its eXIf chunk)")
      (note-has note "- Make :: Kea Cameras")
      (note-has note "- Author :: Ana Timaru")
      (note-has note "- Description :: Kea on a rock")
      (note-has note "- Last changed (tIME) :: 2026-03-02 10:00:00 UTC"))
    ;; WebP and TIFF
    (let ((webp (write-test-octets-file (merge-pathnames "kea.webp" dir)
                                        (let ((exif (octets-of "Exif" 0 0 *exif-test-tiff*)))
                                          (octets-of "RIFF" (le-octets (+ 4 8 (length exif) (mod (length exif) 2)) 4) "WEBP"
                                                     "EXIF" (le-octets (length exif) 4) exif (if (oddp (length exif)) (octets-of 0) #())))))
          (tiff (write-test-octets-file (merge-pathnames "kea.tif" dir) *exif-test-tiff*)))
      (note-has (metadata-of webp) "- Model :: K1")
      (note-has (metadata-of tiff) "* EXIF (its own tags)")
      (note-has (metadata-of tiff) "- Position :: -41.286667, 174.775000"))
    ;; HEIF, as a phone keeps its photographs: the EXIF in an item the boxes point to
    (let* ((exif-data (octets-of (be-octets 0 4) *exif-test-tiff*))
           (ftyp (box "ftyp" "heic" (be-octets 0 4) "mif1heic"))
           (infe (box "infe" 2 0 0 0 (be-octets 2 2) (be-octets 0 2) "Exif"))
           (iinf (box "iinf" 0 0 0 0 (be-octets 1 2) infe))
           (hdlr (box "hdlr" 0 0 0 0 (be-octets 0 4) "pict" (be-octets 0 12) 0))
           (iloc-size (+ 8 4 2 2 (+ 2 2 2 4 4)))
           (meta-size (+ 8 4 (length hdlr) (length iinf) iloc-size))
           (data-at (+ (length ftyp) meta-size))
           (iloc (box "iloc" 0 0 0 0 #x44 #x00 (be-octets 1 2)
                      (be-octets 2 2) (be-octets 0 2) (be-octets 1 2) (be-octets data-at 4) (be-octets (length exif-data) 4)))
           (meta (box "meta" 0 0 0 0 hdlr iinf iloc))
           (heic (write-test-octets-file (merge-pathnames "kea.heic" dir) (octets-of ftyp meta exif-data))))
      (is = meta-size (length meta))
      (let ((note (metadata-of heic)))
        (note-has note "* EXIF (in its Exif item)")
        (note-has note "- Position :: -41.286667, 174.775000")))))

;;; PDF

(define-test (metadata pdf-read)
  (with-test-directory (dir)
    (let* ((pdf (write-test-octets-file
                 (merge-pathnames "report.pdf" dir)
                 (octets-of "%PDF-1.7" 10
                            "1 0 obj << /Type /Catalog /Pages 2 0 R /OpenAction << /S /JavaScript /JS (app.alert\\(1\\)) >> >> endobj" 10
                            "2 0 obj << /Type /Pages /Kids [] /Count 0 >> endobj" 10
                            "3 0 obj << /Title <FEFF004B00650061002000720065007000f60072007400> /Author (Ana \\(field\\) Writer) /Creator (Kea Writer)"
                            " /Producer (Kea PDF 1.0) /CreationDate (D:20260301093000+13'00') /ModDate (D:20260302100000Z) /Custom#20Field (x) >> endobj" 10
                            "trailer << /Root 1 0 R /Info 3 0 R >>" 10 "%%EOF" 10
                            ;; saved again: the information changed, the first kept beside it
                            "3 0 obj << /Title (Kea report, changed) /Author 4 0 R >> endobj" 10
                            "4 0 obj (Bo Westland) endobj" 10
                            "trailer << /Root 1 0 R /Info 3 0 R /Prev 9 >>" 10 "%%EOF" 10)))
           (note (metadata-of pdf)))
      (note-has note "* What the PDF says of itself")
      (note-has note "- PDF version :: 1.7")
      (note-has note "- Saved :: 2 times (changed after it was made")
      (note-has note "- Title :: Kea report, changed")
      (note-has note "- Author :: Bo Westland")
      (note-has note "- Holds :: JavaScript, an action on opening (none run here)")
      (false (search "Ana (field) Writer" note)))
    ;; the first save's information, and one kept in a packed object stream
    (let* ((first (write-test-octets-file
                   (merge-pathnames "first.pdf" dir)
                   (octets-of "%PDF-1.4" 10
                              "3 0 obj << /Title <FEFF004B00650061002000720065007000f60072007400> /Author (Ana \\(field\\) Writer)"
                              " /CreationDate (D:20260301093000+13'00') /Custom#20Field (x) >> endobj" 10
                              "trailer << /Info 3 0 R >>" 10 "%%EOF" 10)))
           (note (metadata-of first)))
      (note-has note "- Title :: Kea repört")
      (note-has note "- Author :: Ana (field) Writer")
      (note-has note "- CreationDate :: 2026-03-01 09:30:00 +13:00")
      (note-has note "- Custom Field :: x"))
    (let* ((packed (coerce (loop for i from 0 below (length "789c335530503057303750b0b151d00fc92cc94955d008484cce4e4d512801f13415f41d4b4b32f28b14341cf3123515f4038af2534a93538b1434bc531315025cdc3415ecec1460006c4a6541aa827e40627a6ab182be737e695e898281829d1d00214b1d42") by 2
                                 collect (parse-integer "789c335530503057303750b0b151d00fc92cc94955d008484cce4e4d512801f13415f41d4b4b32f28b14341cf3123515f4038af2534a93538b1434bc531315025cdc3415ecec1460006c4a6541aa827e40627a6ab182be737e695e898281829d1d00214b1d42"
                                                        :start i :end (+ i 2) :radix 16))
                           '(vector (unsigned-byte 8))))
           (pdf (write-test-octets-file
                 (merge-pathnames "packed.pdf" dir)
                 (octets-of "%PDF-1.5" 10
                            "6 0 obj << /Type /ObjStm /N 2 /First 9 /Filter /FlateDecode /Length " (princ-to-string (length packed)) " >>" 10
                            "stream" 13 10 packed 10 "endstream endobj" 10
                            "8 0 obj << /Type /XRef /Root 1 0 R /Info 5 0 R /Size 9 >> stream" 10 "endstream endobj" 10 "%%EOF" 10)))
           (note (metadata-of pdf)))
      (note-has note "- Title :: Packed title")
      (note-has note "- Producer :: Kea PDF"))))

(define-test (metadata pdf-naming-missing-objects)
  ;; a PDF whose information names a great many objects it does not have, among many object
  ;; streams that are none: read in a moment, every object looked for once, a bounded number of them
  (let* ((octets (octets-of "%PDF-1.7" 10
                            (format nil "3 0 obj << /Title (Kept) ~{/K~d ~:*~d 0 R ~}>> endobj" (loop for i from 100 below 1100 collect i)) 10
                            (with-output-to-string (out) (loop repeat 400 do (format out "9 0 obj << /Type /ObjStm /N 1 /First 4 >> endobj~%")))
                            (make-string (* 4 1024 1024) :initial-element #\Space)
                            "trailer << /Info 3 0 R >>" 10 "%%EOF" 10))
         (start (get-internal-real-time))
         (sections (tuatara.defaults::pdf-metadata octets))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (true (search "- Title :: Kept" (first sections)))
    (true (< seconds 10) "read in ~,1f seconds" seconds)))

(defun zlib-stored-octets (octets)
  "OCTETS as zlib data of one stored deflate block, as /FlateDecode takes them."
  (let ((n (length octets)))
    (octets-of #x78 #x01 1 (logand n 255) (ash n -8) (logand (lognot n) 255) (logand (ash (lognot n) -8) 255) octets 0 0 0 0)))

(define-test (metadata pdf-objects)
  ;; numbers, from their octets; N G R a reference, RG a word
  (is equal '(42 2) (multiple-value-list (tuatara.defaults::pdf-number-at (octets-of "42 ") 0)))
  (is = -1.5d0 (tuatara.defaults::pdf-number-at (octets-of "-1.5]") 0))
  (is = 0.25d0 (tuatara.defaults::pdf-number-at (octets-of ".25") 0))
  (is equal '((:ref 12 0) 6) (multiple-value-list (tuatara.defaults::pdf-number-at (octets-of "12 0 R>>") 0)))
  (is = 1 (tuatara.defaults::pdf-number-at (octets-of "1 0 RG") 0))
  (false (tuatara.defaults::pdf-number-at (octets-of "-") 0))
  (false (tuatara.defaults::pdf-number-at (octets-of (make-string 40 :initial-element #\7)) 0))
  (is equal '(12 4) (multiple-value-list (tuatara.defaults::pdf-integer-at (octets-of "  12x") 0)))
  (false (tuatara.defaults::pdf-integer-at (octets-of "1234567890123456") 0))
  ;; values: a dictionary's entries, a reference's number, an array's items
  (let ((dictionary (tuatara.defaults::pdf-object-at (octets-of "<< /Type /Page /Kids [1 0 R 2 0 R] /Parent 3 0 R >>") 0)))
    (true (tuatara.defaults::pdf-dictionary-p dictionary))
    (is string= "/Page" (tuatara.defaults::pdf-get dictionary "/Type"))
    (is = 3 (tuatara.defaults::pdf-ref-p (tuatara.defaults::pdf-get dictionary "/Parent")))
    (is equal '((:ref 1 0) (:ref 2 0)) (tuatara.defaults::pdf-array-items (tuatara.defaults::pdf-get dictionary "/Kids")))
    (is equal '("/Page") (tuatara.defaults::pdf-array-items "/Page"))
    (false (tuatara.defaults::pdf-get '(1 2) "/Type")))
  ;; an object as written: its stream as long as /Length says, else as far as endstream
  (let* ((octets (octets-of "%PDF-1.4" 10 "4 0 obj << /Length 5 >> stream" 13 10 "hello" 13 10 "endstream endobj" 10
                            "5 0 obj << /Length 99 >> stream" 10 "world" 13 10 "endstream" 10 "endobj"))
         (four (tuatara.defaults::pdf-indirect-object-at octets 9))
         (five-at (search (octets-of "5 0 obj") octets))
         (five (tuatara.defaults::pdf-indirect-object-at octets five-at)))
    (is = 4 (tuatara.defaults::pdf-indirect-number four))
    (is equalp (octets-of "hello") (subseq octets (tuatara.defaults::pdf-indirect-stream-start four) (tuatara.defaults::pdf-indirect-stream-end four)))
    (is equalp (octets-of "world") (subseq octets (tuatara.defaults::pdf-indirect-stream-start five) (tuatara.defaults::pdf-indirect-stream-end five)))
    (is = (length octets) (tuatara.defaults::pdf-indirect-end five))
    (is equal (list five-at 5 0) (multiple-value-list (tuatara.defaults::pdf-object-header-before octets (+ five-at 4))))
    (false (tuatara.defaults::pdf-indirect-object-at octets 0)))
  ;; streams decoded: each filter, filters in a row, PNG's predictor, a picture's data left as it is, the limit kept
  (is equalp (octets-of "Hello") (tuatara.defaults::pdf-ascii-hex-decoded (octets-of "48 65 6C6c6F>") 100))
  (is equalp (octets-of "Man ") (tuatara.defaults::pdf-ascii-85-decoded (octets-of "9jqo^~>") 100))
  (is equalp (octets-of "abcxxx") (tuatara.defaults::pdf-run-length-decoded (octets-of 2 "abc" 254 "x" 128) 100))
  (is equalp (octets-of 45 45 45 45 45 65 45 45 45 66)
      (tuatara.defaults::pdf-lzw-decoded (octets-of #x80 #x0B #x60 #x50 #x22 #x0C #x0C #x85 #x01) 100 1))
  (is equalp (octets-of "abcxxx") (tuatara.defaults::pdf-stream-octets '(("/Filter" "/AHx" "/RL")) (octets-of "02616263FE7880>")))
  (is equalp (octets-of 1 2 2 3)
      (tuatara.defaults::pdf-stream-octets '(("/Filter" . "/FlateDecode") ("/DecodeParms" ("/Predictor" . 12) ("/Columns" . 2)))
                                           (zlib-stored-octets (octets-of 2 1 2 2 1 1))))
  (is equalp '(#(255 216) "/DCTDecode")
      (multiple-value-list (tuatara.defaults::pdf-stream-octets '(("/Filter" . "/DCTDecode")) (octets-of 255 216))))
  (multiple-value-bind (data why) (tuatara.defaults::pdf-stream-octets '(("/Filter" . "/FlateDecode")) (zlib-stored-octets (make-array 2000 :element-type '(unsigned-byte 8) :initial-element 0)) :limit 1000)
    (false data)
    (true (search "more than" why)))
  (multiple-value-bind (data why) (tuatara.defaults::pdf-stream-octets '(("/F" . "elsewhere.dat")) (octets-of "x"))
    (false data)
    (true (search "another file" why)))
  ;; an object stream's objects
  (let* ((data (octets-of "7 0 8 8 (seven) (eight)"))
         (offsets (tuatara.defaults::pdf-object-stream-offsets '(("/N" . 2) ("/First" . 8)) data))
         (eight (tuatara.defaults::pdf-packed-object data offsets 8 :index 1 :in 6)))
    (is equalp #((7 . 8) (8 . 16)) offsets)
    (is string= "eight" (tuatara.defaults::pdf-indirect-value eight))
    (is = 6 (tuatara.defaults::pdf-indirect-packed-in eight))
    (is string= "seven" (tuatara.defaults::pdf-indirect-value (tuatara.defaults::pdf-packed-object data offsets 7 :index 1)))
    (false (tuatara.defaults::pdf-packed-object data offsets 9))
    (false (tuatara.defaults::pdf-object-stream-offsets '(("/N" . 2) ("/First" . 99)) data)))
  ;; a header after other octets, as a PDF may have; a text naming %PDF- is still text
  (is = 10 (tuatara.io:pdf-header-at (octets-of "junk junk" 10 "%PDF-1.4" 10)))
  (false (tuatara.io:pdf-header-at (octets-of "say %PDF- here")))
  (false (tuatara.io:pdf-header-at (octets-of (make-string 1100 :initial-element #\x) "%PDF-1.4")))
  (is eq :pdf (nth-value 1 (tuatara.io:octets-kind (octets-of "MIME junk" 10 "%PDF-1.4" 10 "%" 0 226 10))))
  (is string= "PDF document, version 1.4" (tuatara.io:octets-kind (octets-of "MIME junk" 10 "%PDF-1.4" 10 "%" 0 226 10)))
  (false (eq :pdf (nth-value 1 (tuatara.io:octets-kind (octets-of "Notes: a PDF begins %PDF-1.7 as a rule" 10))))))

;;; Sound and video

(define-test (metadata sound-and-video-read)
  (with-test-directory (dir)
    ;; MP3: ID3 version 2.3 at its start, version 1 at its end
    (flet ((frame (id data) (octets-of id (be-octets (length data) 4) 0 0 data)))
      (let* ((frames (octets-of (frame "TIT2" (octets-of 0 "Kea call"))
                                (frame "TPE1" (octets-of 1 #xFF #xFE "A" 0 "n" 0 "a" 0))
                                (frame "COMM" (octets-of 0 "eng" 0 "Recorded at dawn"))
                                (frame "TXXX" (octets-of 0 "Place" 0 "Arthur's Pass"))
                                (frame "APIC" (octets-of 0 "image/png" 0 3 0 1 2 3 4))))
             (size (length frames))
             (mp3 (write-test-octets-file
                   (merge-pathnames "kea.mp3" dir)
                   (octets-of "ID3" 3 0 0 (ldb (byte 7 21) size) (ldb (byte 7 14) size) (ldb (byte 7 7) size) (ldb (byte 7 0) size)
                              frames #xFF #xFB #x90 0 (make-array 200 :element-type '(unsigned-byte 8) :initial-element 0)
                              "TAG" (octets-of "Old title" (make-array 21 :element-type '(unsigned-byte 8) :initial-element 0))
                              (make-array 95 :element-type '(unsigned-byte 8) :initial-element 0))))
             (note (metadata-of mp3)))
        (note-has note "* ID3 tags")
        (note-has note "- Title :: Kea call")
        (note-has note "- Artist :: Ana")
        (note-has note "- Comment :: Recorded at dawn")
        (note-has note "- TXXX :: Place: Arthur's Pass")
        (note-has note "- Picture :: 17 octets of a picture kept inside it")
        (note-has note "- Title (ID3v1) :: Old title")))
    ;; FLAC: its length and comments
    (let* ((comments (octets-of (le-octets 7 4) "Kea rec" (le-octets 2 4) (le-octets 10 4) "TITLE=Dawn" (le-octets 13 4) "ARTIST=Ana W."))
           (info (octets-of (be-octets 0 4) (be-octets 0 3) (be-octets 0 3)
                            (be-octets (logior (ash 44100 44) (ash 1 41) (ash 15 36) 441000) 8) (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
           (flac (write-test-octets-file (merge-pathnames "kea.flac" dir)
                                         (octets-of "fLaC" 0 (be-octets (length info) 3) info #x84 (be-octets (length comments) 3) comments)))
           (note (metadata-of flac)))
      (note-has note "- Length :: 10.0 seconds, 44,100 samples a second")
      (note-has note "- Made by :: Kea rec")
      (note-has note "- TITLE :: Dawn")
      (note-has note "- ARTIST :: Ana W."))
    ;; a phone's video: its movie header's times and length, its place, maker and model as keyed items
    (let* ((mvhd (box "mvhd" 0 0 0 0 (be-octets 3855888000 4) (be-octets 3855888060 4) (be-octets 600 4) (be-octets 7200 4)
                      (make-array 80 :element-type '(unsigned-byte 8) :initial-element 0)))
           (udta (box "udta" (box "©xyz" (be-octets 18 2) (be-octets #x15c7 2) "-41.2865+174.7762/")))
           (keys (box "keys" 0 0 0 0 (be-octets 2 4)
                      (be-octets 44 4) "mdta" "com.apple.quicktime.location.ISO6709"
                      (be-octets 33 4) "mdta" "com.apple.quicktime.model"))
           (ilst (box "ilst" (box (be-octets 2 4) (box "data" (be-octets 1 4) (be-octets 0 4) "Kea Phone 9"))))
           (meta (box "meta" (box "hdlr" 0 0 0 0 (be-octets 0 4) "mdta" (be-octets 0 12) 0) keys ilst))
           (mov (write-test-octets-file (merge-pathnames "kea.mov" dir)
                                        (octets-of (box "ftyp" "qt  " (be-octets 0 4) "qt  ") (box "moov" mvhd udta meta))))
           (note (metadata-of mov)))
      (note-has note "* What the video or sound says of itself")
      (note-has note "- Brand :: qt")
      (note-has note "- Made (its movie header) :: 2026-03-09")
      (note-has note "- Length :: 12.0 seconds")
      (note-has note "- Position :: -41.2865+174.7762/")
      (note-has note "- com.apple.quicktime.model :: Kea Phone 9"))))

;;; Office files, the command, files made to mislead

(define-test (metadata office-and-command)
  (with-test-directory (dir)
    (let* ((xlsx (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*))
           (note (metadata-of xlsx)))
      (note-has note "* About this workbook")
      (note-has note "- Made by :: Bo Kaikoura")
      (false (search "* Sheet 1" note)))
    ;; M-x file-metadata in a buffer visiting the file: its own buffer, read in the background, the file's
    ;; access time as it was
    (let ((path (write-test-octets-file (merge-pathnames "kea.jpg" dir) (jpeg-test-octets))))
      (sb-posix:utimes path 946681200 946681200)
      (docx-session path (list (format nil "M-x ~a RET" (rimu-typed "file-metadata")) ""))
      (let ((visited (tuatara.buffer:find-buffer "kea.jpg")))
        (when visited (tuatara.buffer:kill-buffer visited)))
      (let ((shown (tuatara.buffer:find-buffer "*Metadata: kea.jpg*")))
        (true shown)
        (when shown
          (note-has (tuatara.buffer:buffer-string shown) "- Accessed :: 2000-01-01")
          (note-has (tuatara.buffer:buffer-string shown) "- Make :: Kea Cameras")
          (true (tuatara.buffer:buffer-local shown :read-only))
          (tuatara.buffer:kill-buffer shown)))
      (is = 946681200 (sb-posix:stat-atime (sb-posix:stat path))))))

(define-test (metadata misleading-files)
  ;; loops, sizes past the end, counts in the millions: read as far as they make sense, never hanging
  (with-test-directory (dir)
    (let ((start (get-internal-real-time)))
      (flet ((note-of (name octets) (metadata-of (write-test-octets-file (merge-pathnames name dir) octets))))
        ;; EXIF whose parts lead to themselves, entries in their millions, offsets past the end
        (let ((loop-tiff (octets-of "II" 42 0 (le-octets 8 4) (le-octets 2 2)
                                    (le-octets #x8769 2) (le-octets 4 2) (le-octets 1 4) (le-octets 8 4)
                                    (le-octets #x010F 2) (le-octets 2 2) (le-octets 4000000 4) (le-octets 99999 4)
                                    (le-octets 8 4))))
          (note-has (note-of "loop.tif" loop-tiff) "* The file")
          (note-has (note-of "many.jpg" (octets-of #xFF #xD8 #xFF #xE1 #xFF #xFF "Exif" 0 0 "II" 42 0 (le-octets 8 4) (le-octets 65535 2))) "* The file"))
        ;; a PDF whose information names itself, and whose strings never close
        (note-has (note-of "self.pdf" (octets-of "%PDF-1.4" 10 "3 0 obj 3 0 R endobj trailer << /Info 3 0 R >> %%EOF"))
             "- PDF version :: 1.4")
        (note-has (note-of "open.pdf" (octets-of "%PDF-1.4" 10 "3 0 obj << /Title (((((never closed" 10 "trailer << /Info 3 0 R >>"))
             "* What the PDF says of itself")
        ;; boxes of no size, of sizes past the end; an ID3 tag longer than the file
        (note-has (note-of "boxes.mp4" (octets-of (box "ftyp" "isom" (be-octets 0 4)) (be-octets 0 4) "moov" (be-octets 999999 4) "mvhd")) "- Brand :: isom")
        (note-has (note-of "tag.mp3" (octets-of "ID3" 4 0 0 127 127 127 127 "TIT2" (be-octets 999999 4) 0 0 0 "x")) "* The file")
        ;; a PNG whose chunk says it is larger than the file
        (note-has (note-of "big.png" (octets-of #x89 "PNG" 13 10 26 10 (be-octets #x7FFFFFFF 4) "tEXt" "x")) "* The file"))
      (true (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second))))))

(define-test (metadata exif-fields-bounded)
  ;; a thousand fields each claiming most of the file are kept to their first 4 KB, not copied whole each time
  (let* ((size (* 256 1024))
         (octets (make-array size :element-type '(unsigned-byte 8) :initial-element 65)))
    (replace octets (octets-of "II" 42 0 (le-octets 8 4) (le-octets 1000 2)))
    (dotimes (i 1000)
      (replace octets (octets-of (le-octets #x9286 2) (le-octets 7 2) (le-octets (- size 16) 4) (le-octets 0 4)) :start1 (+ 10 (* 12 i))))
    (let ((fields (tuatara.defaults::exif-read octets 0)))
      (is = 1000 (length fields))
      (true (every (lambda (field) (<= (length (third field)) 4096)) fields)))))

;;; OpenPGP

(defun openpgp-test-packet (tag body)
  "An OpenPGP packet of TAG holding BODY (octets), in the new form, its length in one octet."
  (octets-of (logior #xC0 tag) (length body) body))

(define-test (metadata openpgp-read)
  (with-test-directory (dir)
    (let* ((zeros (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
           (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7))
           (fingerprint (octets-of (be-octets #xA1B2C3D4E5F60718 8) (make-array 24 :element-type '(unsigned-byte 8) :initial-element 9)))
           (packets (octets-of
                     ;; to an RSA key, the old form of header; to a key its sender left out; to a version 6 key
                     (octets-of #x84 13 3 (be-octets #x0123456789ABCDEF 8) 1 0 8 #xAB)
                     (openpgp-test-packet 1 (octets-of 3 zeros 18 0 8 #xCD))
                     (openpgp-test-packet 1 (octets-of 6 33 6 fingerprint 25 1 2 3))
                     ;; passphrases: iterated and salted SHA-512; Argon2
                     (openpgp-test-packet 3 (octets-of 4 9 3 10 (subseq salt 0 8) #xFF))
                     (openpgp-test-packet 3 (octets-of 6 23 9 2 20 4 salt 1 4 21 1 2))
                     ;; the data, authenticated
                     (openpgp-test-packet 18 (octets-of 2 9 2 6 1 2 3))))
           (path (write-test-octets-file (merge-pathnames "secret.gpg" dir) packets))
           (note (metadata-of path)))
      (true (eq :openpgp (nth-value 1 (tuatara.io:octets-kind packets))))
      (note-has note "OpenPGP encrypted message")
      (note-has note "* Encryption (OpenPGP)")
      (note-has note "- Encrypted to :: 3 keys")
      (note-has note "- Passphrase :: yes, any of 2")
      (note-has note "- Key 1 :: 0123456789ABCDEF, RSA: owner not looked up (*metadata-look-up-key-owners*)")
      (note-has note "- Key 2 :: hidden: the sender left its ID out, ECDH")
      (note-has note "- Key 3 :: A1B2C3D4E5F60718 (fingerprint A1B2C3D4E5F60718")
      (note-has note ", X25519: owner not looked up")
      (note-has note "- Passphrase 1 :: AES-256, the passphrase made a key by SHA-512, salted and repeated, 65,011,712 octets hashed")
      (note-has note "- Passphrase 2 :: AES-256, the passphrase made a key by Argon2, 1 pass, 4 lanes, 2 GiB of memory")
      (note-has note "- Modification check :: authenticated encryption: AES-256, OCB")
      ;; armoured, with its headers; the same packets read
      (let* ((armoured (format nil "-----BEGIN PGP MESSAGE-----~%Comment: from the field~%~%~a~%=abcd~%-----END PGP MESSAGE-----~%"
                               (base64-of packets)))
             (note (metadata-of (write-test-octets-file (merge-pathnames "secret.asc" dir) (octets-of armoured)))))
        (note-has note "OpenPGP encrypted message, armoured")
        (note-has note "- Encrypted to :: 3 keys")
        (note-has note "- Armour header Comment :: from the field"))
      ;; cut short, or an old packet without a modification check
      (note-has (metadata-of (write-test-octets-file (merge-pathnames "cut.gpg" dir) (subseq packets 0 20)))
                "- Modification check :: not reached")
      (let ((old (write-test-octets-file (merge-pathnames "old.gpg" dir)
                                         (octets-of (openpgp-test-packet 3 (octets-of 4 3 0 1)) (openpgp-test-packet 9 (octets-of 1 2 3))))))
        (note-has (metadata-of old) "the passphrase made a key by MD5 of the passphrase alone, no salt: weak")
        (note-has (metadata-of old) "- Modification check :: none: an old form")
        ;; nothing asked of a file that names no key
        (let ((tuatara-user:*metadata-look-up-key-owners* :ask))
          (false (tuatara.defaults::look-up-key-owners-p old))))
      ;; armour with nothing after its first line
      (note-has (metadata-of (write-test-octets-file (merge-pathnames "bare.asc" dir) (octets-of "-----BEGIN PGP MESSAGE-----")))
                "- Modification check :: not reached")
      ;; other files are not taken for one
      (false (eq :openpgp (nth-value 1 (tuatara.io:octets-kind (octets-of #x84 2 9 9))))))))

(defun base64-of (octets)
  "OCTETS in base64, without line breaks."
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (s)
      (loop for i from 0 below (length octets) by 3
            for n = (min 3 (- (length octets) i))
            for bits = (loop for k below 3 sum (ash (if (< k n) (aref octets (+ i k)) 0) (* 8 (- 2 k))))
            do (loop for k below 4
                     do (write-char (if (<= k n) (char letters (ldb (byte 6 (* 6 (- 3 k))) bits)) #\=) s))))))

(define-test (metadata openpgp-key-owners)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (path (namestring (gpg-test-file (merge-pathnames "plans.gpg" dir) "plans" :key key :passphrase "kea"))))
          (tuatara.defaults::install-metadata)
          (let ((note (tuatara.defaults::file-metadata-note path :look-up-keys t)))
            (note-has note "- Passphrase :: yes")
            (note-has note ": Kea Taranaki <kea@example.org>"))
          (let ((tuatara-user:*metadata-look-up-key-owners* nil))
            (false (tuatara.defaults::look-up-key-owners-p path)))
          (let ((tuatara-user:*metadata-look-up-key-owners* t))
            (true (tuatara.defaults::look-up-key-owners-p path))))
        ;; asked by M-x file-metadata, the answer y: a key not in the keyring said to be so
        (let ((path (write-test-octets-file (merge-pathnames "message.dat" dir)
                                            (octets-of (openpgp-test-packet 1 (octets-of 3 (be-octets #x0123456789ABCDEF 8) 1 0 8 #xAB))
                                                       (openpgp-test-packet 18 (octets-of 1 2 3)))))
              (tuatara-user:*metadata-look-up-key-owners* :ask))
          (let ((f (docx-session path (list (format nil "M-x ~a RET" (rimu-typed "file-metadata")) "y RET" ""))))
            (true (tuatara.frontend.fake:fake-frame-showing f "look up whose in your keyring")))
          (let ((shown (tuatara.buffer:find-buffer "*Metadata: message.dat*")))
            (true shown)
            (when shown
              (note-has (tuatara.buffer:buffer-string shown) "- Key 1 :: 0123456789ABCDEF, RSA: not in your keyring")
              (tuatara.buffer:kill-buffer shown)))
          (let ((visited (tuatara.buffer:find-buffer "message.dat")))
            (when visited (tuatara.buffer:kill-buffer visited)))))))
