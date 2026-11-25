;;;; test/org-export-checks.lisp --- Word and LaTeX reading and writing: small fixes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (docx-view links-of-a-header-use-its-own-relations)
  ;; a header's link is found in the header's relations, not the main part's with the same id
  (let* ((w *docx-report-w*)
         (rels "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">~a</Relationships>")
         (link "<Relationship Id=\"rId1\" Type=\"hyperlink\" Target=\"~a\" TargetMode=\"External\"/>")
         (octets (tuatara.defaults::zip-octets
                  `(("word/document.xml"
                     . ,(format nil "<w:document ~a><w:body><w:p><w:hyperlink r:id=\"rId1\"><w:r><w:t>body</w:t></w:r></w:hyperlink></w:p></w:body></w:document>" w))
                    ("word/_rels/document.xml.rels"
                     . ,(format nil rels (concatenate 'string (format nil link "https://body.example/")
                                                      "<Relationship Id=\"rId9\" Type=\"http://x/header\" Target=\"header1.xml\"/>")))
                    ("word/header1.xml"
                     . ,(format nil "<w:hdr ~a><w:p><w:hyperlink r:id=\"rId1\"><w:r><w:t>head</w:t></w:r></w:hyperlink></w:p></w:hdr>" w))
                    ("word/_rels/header1.xml.rels" . ,(format nil rels (format nil link "https://header.example/"))))))
         (tuatara.defaults::*docx-reading* (tuatara.defaults::docx-reading-of "h.docx" octets)))
    (tuatara.defaults::docx-reading-setup)
    (let ((out (with-output-to-string (s) (tuatara.defaults::docx-read-headers s))))
      (true (search "https://header.example/" out) "~a" out)
      (false (search "https://body.example/" out) "~a" out))))

(defun picture-to-convert (dir)
  "A PNG of 16 bits a colour in DIR, which the export has to convert."
  (let* ((pixels (let ((p (make-array (* 4 2 4) :element-type '(unsigned-byte 8))))
                   (dotimes (i (length p) p) (setf (aref p i) (mod (* 37 i) 256)))))
         (png (copy-seq (tuatara.defaults::png-octets (tuatara.frontend:make-image 4 2 4 pixels))))
         (path (namestring (merge-pathnames "deep.png" dir))))
    (tuatara.text:octets-put-integer png 2 4 :at 16 :big-endian t)
    (setf (aref png 24) 16)
    (tuatara.text:octets-put-integer png (tuatara.defaults::crc32 png 12 29) 4 :at 29 :big-endian t)
    (alexandria:write-byte-vector-into-file png path)
    path))

(define-test (latex-export converted-picture-used-again-is-kept)
  ;; a converted picture taken again is marked as used, so cleaning up old copies leaves it
  (with-test-directory (dir)
    (let* ((deep (picture-to-convert dir))
           (made (tuatara.defaults::latex-picture deep))
           (old (- (get-universal-time) 2208988800 (* 3 86400))))
      (true (and made (string/= made deep)))
      (sb-posix:utimes made old old)
      (is string= made (tuatara.defaults::latex-picture deep))
      (tuatara.defaults::forget-old-copies (tuatara.defaults::latex-pictures-directory))
      (true (tuatara.io:path-stamp made)))))

(define-test (latex-export font-answers-asked-again-when-fontconfig-rewrites-its-cache)
  ;; a font installed in a directory below the font directories makes fontconfig rewrite its cache
  (with-test-directory (dir)
    (let ((was (sb-posix:getenv "XDG_CACHE_HOME"))
          (cache (namestring dir)))
      (ensure-directories-exist (concatenate 'string cache "fontconfig/"))
      (sb-posix:setenv "XDG_CACHE_HOME" cache 1)
      (unwind-protect
           (let ((before (tuatara.defaults::fonts-stamp)))
             (sb-posix:utimes (concatenate 'string cache "fontconfig") 1000 1000)
             (false (equal before (tuatara.defaults::fonts-stamp))))
        (if was (sb-posix:setenv "XDG_CACHE_HOME" was 1) (sb-posix:unsetenv "XDG_CACHE_HOME"))))))

(define-test (office-view sheet-listed-twice-written-once)
  ;; a sheet a workbook lists twice is written once, the repeat saying which it repeats
  (with-test-directory (dir)
    (let* ((workbook (cdr (assoc "xl/workbook.xml" *xlsx-test-parts* :test #'string=)))
           (parts (cons (cons "xl/workbook.xml"
                              (cl-ppcre:regex-replace "<sheet name='Deep'[^>]*/>" workbook
                                                      "<sheet name='Birds again' sheetId='4' r:id='rId1'/>"))
                        (remove "xl/workbook.xml" *xlsx-test-parts* :key #'car :test #'string=)))
           (note (tuatara.defaults::xlsx-read-note
                  (write-office-test-file (merge-pathnames "twice.xlsx" dir) parts))))
      (is = 1 (length (cl-ppcre:all-matches-as-strings "Kea \\\\vert\\{\\} alpine" note)) "~a" note)
      (true (search "Sheet 1 again" note) "~a" note))))

(defun latex-read-as-org (text)
  "TEXT, LaTeX, read as the template import reads a document, as Org on one line."
  (let ((tuatara.defaults::*latex-org-text* text)
        (tuatara.defaults::*latex-org-source* text)
        (tuatara.defaults::*latex-org-footnotes* '()))
    (tuatara.defaults::latex-org-string 0 (length text))))

(define-test (template-import accent-before-a-spaced-letter)
  ;; \c c has its cedilla on the c, not on the space between
  (is string= "ç" (latex-read-as-org "\\c c"))
  (is string= "ç" (latex-read-as-org "\\c{c}"))
  (is string= "é" (latex-read-as-org "\\'e")))

(define-test (template-import footnote-in-a-field-example)
  ;; a footnote met in a field's example is left out of it, and makes no footnote of the note
  (let ((tuatara.defaults::*latex-org-text* "Kea\\footnote{Alpine} parrot")
        (tuatara.defaults::*latex-org-source* "Kea\\footnote{Alpine} parrot")
        (tuatara.defaults::*latex-org-footnotes* '()))
    (is string= "Kea parrot"
        (tuatara.defaults::latex-import-plain 0 (length tuatara.defaults::*latex-org-text*)))
    (is equal '() tuatara.defaults::*latex-org-footnotes*)))

(define-test (latex-export labels-and-addresses-hold-no-characters-past-ascii)
  ;; a label or an address with letters past ASCII holds none, so that no font fallback is written in it
  (is string= "sec-x4e2dx6587" (tuatara.defaults::latex-label-name "sec-中文"))
  (is string= "a-b" (tuatara.defaults::latex-label-name "a b"))
  (is string= "http://x/\\%C3\\%A9" (tuatara.defaults::latex-url "http://x/é")))

(define-test (docx-view run-properties-kept-as-written)
  ;; a template's run properties holding backslashes are copied to each run as they are
  (let ((properties "<w:rPr><w:rFonts w:ascii=\"Kea\\&\\1\\\\\"/></w:rPr>"))
    (is string= (format nil "<w:r>~a<w:t>a</w:t></w:r><w:r><w:rPr><w:b/></w:rPr><w:t>b</w:t></w:r>" properties)
        (tuatara.defaults::docx-with-properties "<w:r><w:t>a</w:t></w:r><w:r><w:rPr><w:b/></w:rPr><w:t>b</w:t></w:r>"
                                                properties))))

(define-test (latex-export modification-time-of-the-file)
  ;; {{{modification-time}}} is when the note's file was last changed; {{{time}}} is the time now
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "note.org" dir)))
          (text (format nil "Changed {{{modification-time(%Y-%m-%d)}}}, made {{{time(%Y)}}}.~%")))
      (alexandria:write-string-into-file text file)
      ;; 2000-01-01 00:00 UTC; noon that day in any zone a person lives in
      (sb-posix:utimes file (+ 946684800 43200) (+ 946684800 43200))
      (let ((html (tuatara.defaults::org-html-document text :file file)))
        (true (search "Changed 2000-01-01" html) "~a" html)
        (true (search (format nil "made ~d" (nth-value 5 (decode-universal-time (get-universal-time)))) html))))))

(define-test (latex-export export-drawer-after-a-last-line-unended)
  ;; a heading on the last line, no newline after it: the drawer goes on a line of its own below it
  (let ((text (org-text-after "M-x o r g - i n s e r t - e x p o r t - d r a w e r RET" "* Title")))
    (true (search (format nil "* Title~%:EXPORT:~%") text) "~a" text)))
