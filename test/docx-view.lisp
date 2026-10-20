;;;; test/docx-view.lisp --- Word documents opened, read as text by Tuatara itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test docx-view
  :description "Zip archives are unpacked, stored and deflated, within limits, damaged ones refused; XML is
read with no entity of a DTD expanded and no nesting past the limit; a Word document is read as an Org
note: what it says of itself, macros, embedded files and outside links named, headings, lists, tables,
markup, links, footnotes, comments, tracked changes and hidden text; visited where pages are not drawn,
it is read as text, read only, in Org mode.")

(defun deflated-zip ()
  "The octets of test/docx-view/deflated.zip, made by another program: short.txt
and long.xml deflated (fixed and dynamic codes), stored.bin stored."
  (alexandria:read-file-into-byte-vector (asdf:system-relative-pathname :tuatara "test/docx-view/deflated.zip")))

(defun zip-entry-named (octets name)
  "The entry NAME of the zip archive OCTETS."
  (find name (tuatara.defaults::zip-entries octets) :key (lambda (e) (getf e :name)) :test #'string=))

(define-test (docx-view zip-read)
  (let ((octets (deflated-zip)))
    (is equal '("short.txt" "long.xml" "stored.bin")
        (mapcar (lambda (e) (getf e :name)) (tuatara.defaults::zip-entries octets)))
    (is string= (format nil "~{~a~}" (make-list 3 :initial-element "Kia ora, Tuatara! "))
        (sb-ext:octets-to-string (tuatara.defaults::zip-entry-octets octets (zip-entry-named octets "short.txt"))))
    (let ((long (sb-ext:octets-to-string (tuatara.defaults::zip-entry-octets octets (zip-entry-named octets "long.xml")))))
      (is = 5088 (length long))
      (true (search "kakapo" long)))
    (true (equalp (coerce (loop for i below 256 collect i) '(vector (unsigned-byte 8)))
                  (tuatara.defaults::zip-entry-octets octets (zip-entry-named octets "stored.bin"))))
    ;; a part larger unpacked than allowed, refused before it is unpacked
    (let ((tuatara.defaults::*zip-largest-entry* 1000))
      (fail (tuatara.defaults::zip-entry-octets octets (zip-entry-named octets "long.xml")) 'tuatara.defaults::zip-error))
    ;; damaged: an octet of the deflated data changed
    (let* ((entry (zip-entry-named octets "long.xml"))
           (broken (copy-seq octets))
           (at (+ (getf entry :data) 40)))
      (setf (aref broken at) (logxor (aref broken at) #x5A))
      (fail (tuatara.defaults::zip-entry-octets broken entry) 'tuatara.defaults::zip-error))
    (fail (tuatara.defaults::zip-entries (sb-ext:string-to-octets "no archive at all, only words")) 'tuatara.defaults::zip-error)))

(define-test (docx-view xml-read)
  (let ((tree (tuatara.defaults::xml-parse
               "<?xml version=\"1.0\"?><!DOCTYPE d [<!ENTITY boom \"EXPANDED\"><!ENTITY big SYSTEM \"file:///etc/passwd\">]><d a=\"x &amp; y\">&boom; &big; &#233;&#xE9; &lt;b&gt;<![CDATA[<raw>]]><!-- gone --></d>")))
    (let ((d (tuatara.defaults::xml-child tree "d")))
      (is string= "x & y" (tuatara.defaults::xml-attribute d "a"))
      (is string= "&boom; &big; éé <b><raw>" (tuatara.defaults::xml-all-text d))))
  (let ((tuatara.defaults::*xml-deepest* 20))
    (fail (tuatara.defaults::xml-parse (format nil "~{~a~}" (make-list 50 :initial-element "<a>")))))
  (true (tuatara.defaults::xml-parse (format nil "~{~a~}" (make-list 50 :initial-element "<a/>")))))

(defparameter *docx-test-parts*
  `(("[Content_Types].xml" . "<?xml version=\"1.0\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/><Override PartName=\"/word/footnotes.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml\"/><Override PartName=\"/word/comments.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml\"/><Override PartName=\"/word/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml\"/><Override PartName=\"/word/numbering.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml\"/></Types>")
    ("_rels/.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/></Relationships>")
    ("word/_rels/document.xml.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"hyperlink\" Target=\"https://example.org/\" TargetMode=\"External\"/><Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes\" Target=\"footnotes.xml\"/><Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments\" Target=\"comments.xml\"/><Relationship Id=\"rId4\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/><Relationship Id=\"rId5\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering\" Target=\"numbering.xml\"/></Relationships>")
    ("word/styles.xml" . "<w:styles xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:style w:styleId=\"H1\"><w:name w:val=\"heading 1\"/></w:style><w:style w:styleId=\"Sub\"><w:name w:val=\"Sub heading\"/><w:pPr><w:outlineLvl w:val=\"1\"/></w:pPr></w:style><w:style w:styleId=\"Title\"><w:name w:val=\"Title\"/></w:style></w:styles>")
    ("word/numbering.xml" . "<w:numbering xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:abstractNum w:abstractNumId=\"0\"><w:lvl w:ilvl=\"0\"><w:numFmt w:val=\"decimal\"/></w:lvl></w:abstractNum><w:abstractNum w:abstractNumId=\"1\"><w:lvl w:ilvl=\"0\"><w:numFmt w:val=\"bullet\"/></w:lvl><w:lvl w:ilvl=\"1\"><w:numFmt w:val=\"bullet\"/></w:lvl></w:abstractNum><w:num w:numId=\"1\"><w:abstractNumId w:val=\"0\"/></w:num><w:num w:numId=\"2\"><w:abstractNumId w:val=\"1\"/></w:num></w:numbering>")
    ("word/footnotes.xml" . "<w:footnotes xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:footnote w:type=\"separator\" w:id=\"0\"><w:p/></w:footnote><w:footnote w:id=\"1\"><w:p><w:r><w:t>Seen on the log.</w:t></w:r></w:p></w:footnote></w:footnotes>")
    ("word/comments.xml" . "<w:comments xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:comment w:id=\"0\" w:author=\"Ann Wellington\" w:date=\"2026-10-01T09:00:00Z\"><w:p><w:r><w:t>Check this date.</w:t></w:r></w:p></w:comment></w:comments>")
    ("docProps/core.xml" . "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\"><dc:title>Kea survey</dc:title><dc:creator>Bo Waikato</dc:creator><cp:lastModifiedBy>Cy Gisborne</cp:lastModifiedBy><dcterms:created>2026-09-30T10:00:00Z</dcterms:created></cp:coreProperties>")
    ("docProps/app.xml" . "<Properties><Application>Some Word Processor</Application><Template>Normal.dotm</Template></Properties>")
    ("word/vbaProject.bin" . "not run")
    ("word/document.xml" . ,(concatenate 'string
                             "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><w:body>"
                             "<w:p><w:pPr><w:pStyle w:val=\"Title\"/></w:pPr><w:r><w:t>Kea survey</w:t></w:r></w:p>"
                             "<w:p><w:pPr><w:pStyle w:val=\"H1\"/></w:pPr><w:r><w:t>Findings</w:t></w:r></w:p>"
                             "<w:p><w:r><w:t xml:space=\"preserve\">The birds were </w:t></w:r><w:r><w:rPr><w:b/></w:rPr><w:t>counted</w:t></w:r>"
                             "<w:r><w:t xml:space=\"preserve\"> twice, </w:t></w:r><w:r><w:rPr><w:i/></w:rPr><w:t>at dawn</w:t></w:r>"
                             "<w:r><w:footnoteReference w:id=\"1\"/></w:r><w:r><w:t xml:space=\"preserve\">; see </w:t></w:r>"
                             "<w:hyperlink r:id=\"rId1\"><w:r><w:t>the site</w:t></w:r></w:hyperlink><w:r><w:t>.</w:t></w:r></w:p>"
                             "<w:p><w:pPr><w:pStyle w:val=\"Sub\"/></w:pPr><w:r><w:t>Changes</w:t></w:r></w:p>"
                             "<w:p><w:r><w:t xml:space=\"preserve\">Counted on </w:t></w:r>"
                             "<w:del w:author=\"Cy Gisborne\" w:date=\"2026-10-02T08:00:00Z\"><w:r><w:delText>Monday</w:delText></w:r></w:del>"
                             "<w:ins w:author=\"Cy Gisborne\" w:date=\"2026-10-02T08:00:00Z\"><w:r><w:t>Tuesday</w:t></w:r></w:ins>"
                             "<w:r><w:commentReference w:id=\"0\"/></w:r><w:r><w:rPr><w:vanish/></w:rPr><w:t>secret aside</w:t></w:r></w:p>"
                             "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"1\"/></w:numPr></w:pPr><w:r><w:t>First</w:t></w:r></w:p>"
                             "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"1\"/></w:numPr></w:pPr><w:r><w:t>Second</w:t></w:r></w:p>"
                             "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"1\"/><w:numId w:val=\"2\"/></w:numPr></w:pPr><w:r><w:t>Aside</w:t></w:r></w:p>"
                             "<w:p><w:r><w:t>After the list.</w:t></w:r></w:p>"
                             "<w:p><w:r><w:t>* not a heading | nor a table</w:t></w:r></w:p>"
                             "<w:tbl><w:tr><w:tc><w:p><w:r><w:t>Bird</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>Count</w:t></w:r></w:p></w:tc></w:tr>"
                             "<w:tr><w:tc><w:p><w:r><w:t>Kea | alpine</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>12</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"
                             "</w:body></w:document>")))
  "The parts of a small Word document made for the tests, as a word processor writes them.")

(defun write-test-docx (path)
  "Write the Word document of *DOCX-TEST-PARTS* at PATH."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (tuatara.defaults::zip-octets *docx-test-parts*) out))
  path)

(define-test (docx-view document-read)
  (with-test-directory (dir)
    (let* ((path (namestring (write-test-docx (merge-pathnames "survey.docx" dir))))
           (note (tuatara.defaults::docx-read-note path)))
      (flet ((has (text) (true (search text note) "~s in~%~a" text note)))
        (has "#+title: survey.docx")
        (has "- Title :: Kea survey")
        (has "- Made by :: Bo Waikato")
        (has "- Last changed by :: Cy Gisborne")
        (has "- Template :: Normal.dotm")
        (has "- Macros :: yes (word/vbaProject.bin), never run")
        (has "- Links outside it :: https://example.org/ (none followed)")
        (has "- Tracked changes :: 2")
        (has "- Comments :: 1")
        (has "#+title: Kea survey")
        (has "** Findings")
        (has "*** Changes")
        (has "The birds were *counted* twice, /at dawn/[fn:1]; see [[https://example.org/][the site]].")
        (has "Counted on +Monday+_Tuesday_[fn:c0][hidden: secret aside]")
        (has (format nil "1. First~%2. Second~%  - Aside~%~%After the list."))
        (has (format nil "~c* not a heading | nor a table" (code-char #x200B)))
        (has (format nil "| Bird | Count |~%|-~%| Kea \\vert{} alpine | 12 |"))
        (has "- Deleted by Cy Gisborne, 2026-10-02T08:00:00Z :: Monday")
        (has "- Inserted by Cy Gisborne, 2026-10-02T08:00:00Z :: Tuesday")
        (has "[fn:1] Seen on the log.")
        (has "[fn:c0] Comment by Ann Wellington, 2026-10-01T09:00:00Z: Check this date.")))
    (alexandria:write-string-into-file "PK not really" (merge-pathnames "broken.docx" dir))
    (fail (tuatara.defaults::docx-read-note (namestring (merge-pathnames "broken.docx" dir))))))

(define-test (docx-view main-part-as-word-reads-it)
  ;; the text Word shows is the part _rels/.rels names, in any case: that one read, the other said
  (with-test-directory (dir)
    (let* ((w "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"")
           (parts (list (cons "_rels/.rels" "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"/WORD/Main.xml\"/></Relationships>")
                        (cons "word/main.xml" (format nil "<w:document ~a><w:body><w:p><w:r><w:t>What Word shows</w:t></w:r></w:p></w:body></w:document>" w))
                        (cons "word/document.xml" (format nil "<w:document ~a><w:body><w:p><w:r><w:t>Nothing to see</w:t></w:r></w:p></w:body></w:document>" w))))
           (path (namestring (merge-pathnames "decoy.docx" dir))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (tuatara.defaults::zip-octets parts) out))
      (let ((note (tuatara.defaults::docx-read-note path)))
        (true (search "What Word shows" note))
        (false (search "Nothing to see" note))
        (true (search "- Main part :: word/main.xml" note) "~a" note)
        (true (search "it also holds word/document.xml, which Word does not show" note))))))

(define-test (docx-view text-stays-text)
  ;; what a document says after a line break, in its title, its facts or a picture's words never
  ;; becomes a heading or keyword of the note made from it
  (with-test-directory (dir)
    (let* ((w "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"")
           (parts (list (cons "word/document.xml"
                              (format nil "<w:document ~a><w:body><w:p><w:r><w:t>first</w:t><w:br/><w:t>* Forged heading</w:t><w:br/><w:t>#+SETUPFILE: x.org</w:t><w:br/><w:t>[fn:9] forged</w:t></w:r></w:p></w:body></w:document>" w))
                        (cons "docProps/core.xml"
                              (format nil "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><dc:description>fine&#10;* Tracked changes&#10;- Inserted by nobody :: lies</dc:description></cp:coreProperties>"))))
           (path (namestring (merge-pathnames "forged.docx" dir))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (tuatara.defaults::zip-octets parts) out))
      (let ((note (tuatara.defaults::docx-read-note path)))
        (false (cl-ppcre:scan "(?m)^\\* Forged" note) "~a" note)
        (false (cl-ppcre:scan "(?m)^\\*+ Tracked changes" note))
        (false (cl-ppcre:scan "(?m)^#\\+SETUPFILE" note))
        (false (cl-ppcre:scan "(?m)^\\[fn:9\\]" note))
        (true (search "- Description :: fine * Tracked changes - Inserted by nobody :: lies" note))))))

(define-test (docx-view nested-changes)
  ;; changes nested hundreds deep around much text: one change noted, its text once, read at once
  (with-test-directory (dir)
    (let* ((depth 400)
           (words (make-string 20000 :initial-element #\k))
           (document (with-output-to-string (s)
                       (write-string "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body><w:p>" s)
                       (loop repeat depth do (write-string "<w:ins w:author=\"A\" w:date=\"2026-10-02T08:00:00Z\">" s))
                       (format s "<w:r><w:t>~a</w:t></w:r>" words)
                       (loop repeat depth do (write-string "</w:ins>" s))
                       (write-string "</w:p></w:body></w:document>" s)))
           (parts (cons (cons "word/document.xml" document) (remove "word/document.xml" *docx-test-parts* :key #'car :test #'string=)))
           (path (namestring (merge-pathnames "nested.docx" dir)))
           (start (get-internal-real-time)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (tuatara.defaults::zip-octets parts) out))
      (let ((note (tuatara.defaults::docx-read-note path)))
        (true (search "- Tracked changes :: 1" note))
        (is = 2 (count-matches words note))
        (true (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))))))

(define-test (docx-view visited-as-text)
  (with-test-directory (dir)
    (let ((path (write-test-docx (merge-pathnames "survey.docx" dir)))
          (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 30))
          (start (get-internal-real-time)))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
        (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text "")
      (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring path))))
      (labels ((check ()
                 (when (and (some #'tuatara.defaults::office-read-job-p tuatara.runtime:*jobs*)
                            (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                   (sleep 0.02)
                   (tuatara.frontend.fake:fake-queue-action f #'check))))
        (tuatara.frontend.fake:fake-queue-action f #'check))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
      (let ((buffer (tuatara.buffer:find-buffer "survey.docx (text)")))
        (true buffer)
        (when buffer
          (true (tuatara.buffer:buffer-local buffer :read-only))
          (true (tuatara.command::buffer-mode-named-p buffer 'tuatara.defaults::org-mode))
          (true (search "** Findings" (tuatara.buffer:buffer-string buffer)))
          (tuatara.buffer:kill-buffer buffer))))))

(defun docx-session (path steps &key cell-pixels)
  "Visit PATH with C-x C-f in an editor session, then type each of STEPS in
turn, each once what the views make in the background is done; CELL-PIXELS
has the frontend show pictures. Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 30 :cell-pixels cell-pixels))
        (start (get-internal-real-time)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring path))))
    (labels ((busy ()
               (or (some (lambda (job) (or (tuatara.defaults::office-read-job-p job) (tuatara.defaults::metadata-job-p job))) tuatara.runtime:*jobs*)
                   (some (lambda (b) (let ((view (tuatara.defaults::pdf-view-of b)))
                                       (and view (or (tuatara.defaults::pdf-view-job view) (tuatara.defaults::pdf-view-text-job view)))))
                         tuatara.defaults::*pdf-views*)))
             (check ()
               (if (and (busy) (< (- (get-internal-real-time) start) (* 60 internal-time-units-per-second)))
                   (progn (sleep 0.05) (tuatara.frontend.fake:fake-queue-action f #'check))
                   (when steps
                     (tuatara.frontend.fake:fake-queue-keys f (pop steps))
                     (tuatara.frontend.fake:fake-queue-action f #'check)))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(define-test (docx-view word-pages)
  (if (not (and (eq :ready (tuatara.io:sandbox-status))
                (tuatara.io:sandbox-find-program tuatara.defaults::*office-page-program*)
                (tuatara.defaults::find-pdf-decoder)))
      (skip "LibreOffice or the sandbox missing here" (true nil))
      (with-test-directory (dir)
        (let ((path (write-test-docx (merge-pathnames "survey.docx" dir))))
          (pdf-forget)
          ;; its pages, made by LibreOffice in the sandbox; T its text, T its pages again
          (multiple-value-bind (f editor) (docx-session path '("T" "T") :cell-pixels '(10 20))
            (true (tuatara.frontend.fake:fake-frame-showing f "Word document, its pages drawn by LibreOffice"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Page 1 of 1"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Sandboxed"))
            (true (last-images f))
            (true (tuatara.frontend.fake:fake-frame-showing f "About this document"))
            (is string= "survey.docx" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
          (let ((reading (tuatara.buffer:find-buffer "survey.docx (text)")))
            (true reading)
            (when reading (tuatara.buffer:kill-buffer reading)))
          (pdf-forget)
          ;; not wanted: read as text at once
          (let ((tuatara.defaults::*office-view-pages* nil))
            (multiple-value-bind (f editor) (docx-session path '() :cell-pixels '(10 20))
              (declare (ignore f))
              (is string= "survey.docx (text)" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
          (let ((reading (tuatara.buffer:find-buffer "survey.docx (text)")))
            (when reading (tuatara.buffer:kill-buffer reading)))))))

(define-test (docx-view in-rimu-preview)
  (with-rimu-tree (dir)
    (write-test-docx (merge-pathnames "survey.docx" dir))
    ;; read as text by Tuatara, an Org note, where pictures are not shown
    (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "survey.docx")) :preview t :width 250)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Word document (OOXML)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "survey.docx (preview, text)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "About this document"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Macros :: yes")))
    ;; its first page, drawn from the PDF LibreOffice makes in the sandbox
    (when (and (eq :ready (tuatara.io:sandbox-status))
               (tuatara.io:sandbox-find-program tuatara.defaults::*office-page-program*)
               (tuatara.defaults::find-pdf-decoder))
      (let ((f (rimu-session dir (format nil "g f ~a RET" (rimu-typed "survey.docx")) :preview t :width 250 :cell-pixels '(10 20))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Word document (OOXML), 1 page"))
        (true (last-images f))))
    (rimu-forget)))

(define-test (docx-view fonts-shown)
  ;; LibreOffice shown your font directories, read only, where its home finds them, after its scratch /tmp
  (let ((arguments (tuatara.io::sandbox-arguments "soffice" '() 10 nil :directories '(("/home/x/.fonts" . "/tmp/.fonts")))))
    (true (search '("--tmpfs" "/tmp" "--ro-bind-try" "/home/x/.fonts" "/tmp/.fonts") arguments :test #'equal)))
  (let ((directories (tuatara.defaults::sandbox-font-directories)))
    (is equal '("/tmp/.local/share/fonts" "/tmp/.fonts") (mapcar #'cdr directories))
    (true (every (lambda (d) (uiop:string-suffix-p (car d) "fonts")) directories))))

;;; A report someone gives you

(defparameter *docx-report-w*
  "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:m=\"http://schemas.openxmlformats.org/officeDocument/2006/math\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" xmlns:wps=\"http://schemas.microsoft.com/office/word/2010/wordprocessingShape\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\""
  "The namespaces of the test report's parts.")

(defun docx-report-parts ()
  "The parts of a small report as a word processor writes one: a cover with
its fields (a title, a table of labels and values, a date control, a
labelled line, placeholder text in the header), contents, then its text
(headings, a numbered list, a table in a table style, an endnote, a text
box, an equation, sub- and superscripts, a cell spanning two columns);
tracking on, an attached template, a comment and macros besides."
  (let ((w *docx-report-w*))
    (flet ((p (text &key style (rpr "")) (format nil "<w:p>~@[<w:pPr><w:pStyle w:val=\"~a\"/></w:pPr>~]<w:r><w:rPr>~a</w:rPr><w:t xml:space=\"preserve\">~a</w:t></w:r></w:p>" style rpr text))
           (cell (text) (format nil "<w:tc><w:tcPr><w:tcW w:w=\"2000\" w:type=\"dxa\"/></w:tcPr><w:p><w:r><w:t>~a</w:t></w:r></w:p></w:tc>" text)))
      `(("[Content_Types].xml" . "<?xml version=\"1.0\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Default Extension=\"bin\" ContentType=\"application/vnd.ms-office.vbaProject\"/><Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.ms-word.document.macroEnabled.main+xml\"/><Override PartName=\"/word/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml\"/><Override PartName=\"/word/numbering.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml\"/><Override PartName=\"/word/settings.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml\"/><Override PartName=\"/word/header1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml\"/><Override PartName=\"/word/comments.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml\"/><Override PartName=\"/word/endnotes.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml\"/></Types>")
        ("_rels/.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/></Relationships>")
        ("word/_rels/document.xml.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/><Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering\" Target=\"numbering.xml\"/><Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings\" Target=\"settings.xml\"/><Relationship Id=\"rId4\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/header\" Target=\"header1.xml\"/><Relationship Id=\"rId5\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments\" Target=\"comments.xml\"/><Relationship Id=\"rId6\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes\" Target=\"endnotes.xml\"/><Relationship Id=\"rId7\" Type=\"http://schemas.microsoft.com/office/2006/relationships/vbaProject\" Target=\"vbaProject.bin\"/></Relationships>")
        ("word/styles.xml" . ,(format nil "<w:styles ~a><w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii=\"No Such Font Tuatara\" w:hAnsi=\"No Such Font Tuatara\"/><w:sz w:val=\"24\"/></w:rPr></w:rPrDefault><w:pPrDefault><w:pPr><w:spacing w:after=\"160\" w:line=\"276\" w:lineRule=\"auto\"/></w:pPr></w:pPrDefault></w:docDefaults><w:style w:type=\"paragraph\" w:default=\"1\" w:styleId=\"Normal\"><w:name w:val=\"Normal\"/></w:style><w:style w:type=\"paragraph\" w:styleId=\"Title\"><w:name w:val=\"Title\"/><w:basedOn w:val=\"Normal\"/><w:rPr><w:sz w:val=\"56\"/><w:color w:val=\"123456\"/></w:rPr></w:style><w:style w:type=\"paragraph\" w:styleId=\"Heading1\"><w:name w:val=\"heading 1\"/><w:basedOn w:val=\"Normal\"/><w:pPr><w:spacing w:before=\"360\" w:after=\"80\"/><w:outlineLvl w:val=\"0\"/></w:pPr><w:rPr><w:b/><w:sz w:val=\"36\"/></w:rPr></w:style><w:style w:type=\"paragraph\" w:styleId=\"Caption\"><w:name w:val=\"caption\"/><w:basedOn w:val=\"Normal\"/><w:rPr><w:i/></w:rPr></w:style><w:style w:type=\"table\" w:default=\"1\" w:styleId=\"TableNormal\"><w:name w:val=\"Normal Table\"/><w:tblPr><w:tblCellMar><w:left w:w=\"108\" w:type=\"dxa\"/><w:right w:w=\"108\" w:type=\"dxa\"/></w:tblCellMar></w:tblPr></w:style><w:style w:type=\"table\" w:styleId=\"BlueGrid\"><w:name w:val=\"Blue Grid\"/><w:basedOn w:val=\"TableNormal\"/></w:style></w:styles>" w))
        ("word/numbering.xml" . ,(format nil "<w:numbering ~a><w:abstractNum w:abstractNumId=\"5\"><w:lvl w:ilvl=\"0\"><w:start w:val=\"1\"/><w:numFmt w:val=\"upperRoman\"/><w:lvlText w:val=\"%1.\"/></w:lvl><w:lvl w:ilvl=\"1\"><w:start w:val=\"1\"/><w:numFmt w:val=\"lowerLetter\"/><w:lvlText w:val=\"%2)\"/></w:lvl></w:abstractNum><w:num w:numId=\"7\"><w:abstractNumId w:val=\"5\"/></w:num></w:numbering>" w))
        ("word/settings.xml" . ,(format nil "<w:settings ~a><w:trackRevisions/><w:attachedTemplate r:id=\"rId1\"/></w:settings>" w))
        ("word/_rels/settings.xml.rels" . "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/attachedTemplate\" Target=\"file:///server/Report.dotm\" TargetMode=\"External\"/></Relationships>")
        ("word/header1.xml" . ,(format nil "<w:hdr ~a><w:p><w:pPr><w:jc w:val=\"right\"/></w:pPr><w:r><w:t>Survey [Plan code] page </w:t></w:r><w:fldSimple w:instr=\" PAGE \"><w:r><w:t>1</w:t></w:r></w:fldSimple></w:p></w:hdr>" w))
        ("word/comments.xml" . ,(format nil "<w:comments ~a><w:comment w:id=\"0\" w:author=\"Ann\"><w:p><w:r><w:t>Check</w:t></w:r></w:p></w:comment></w:comments>" w))
        ("word/endnotes.xml" . ,(format nil "<w:endnotes ~a><w:endnote w:type=\"separator\" w:id=\"-1\"><w:p><w:r><w:separator/></w:r></w:p></w:endnote><w:endnote w:id=\"1\"><w:p><w:r><w:t>Data kept by the office.</w:t></w:r></w:p></w:endnote></w:endnotes>" w))
        ("word/vbaProject.bin" . "not run")
        ("docProps/core.xml" . "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><dc:title>Bird Survey</dc:title></cp:coreProperties>")
        ("word/document.xml"
         . ,(concatenate
             'string
             (format nil "<w:document ~a><w:body>" w)
             (p "Bird Survey" :style "Title")
             "<w:tbl><w:tblPr><w:tblW w:w=\"0\" w:type=\"auto\"/></w:tblPr><w:tblGrid><w:gridCol w:w=\"2000\"/><w:gridCol w:w=\"2000\"/></w:tblGrid>"
             "<w:tr>" (cell "Version") (cell "0.3") "</w:tr><w:tr>" (cell "Reviewer") (cell "[Enter reviewer name]") "</w:tr></w:tbl>"
             "<w:p><w:r><w:t xml:space=\"preserve\">Date: </w:t></w:r><w:sdt><w:sdtPr><w:tag w:val=\"issue date\"/><w:showingPlcHdr/><w:date><w:dateFormat w:val=\"d MMMM yyyy\"/></w:date></w:sdtPr><w:sdtContent><w:r><w:rPr><w:rStyle w:val=\"PlaceholderText\"/></w:rPr><w:t>Click to enter a date.</w:t></w:r></w:sdtContent></w:sdt></w:p>"
             "<w:p><w:r><w:rPr><w:b/></w:rPr><w:t xml:space=\"preserve\">Made for: </w:t></w:r><w:r><w:t>The Office</w:t></w:r></w:p>"
             "<w:p><w:commentRangeStart w:id=\"0\"/><w:del w:author=\"A\"><w:r><w:delText>Struck words</w:delText></w:r></w:del><w:ins w:author=\"A\"><w:r><w:t>Kept words</w:t></w:r></w:ins><w:commentRangeEnd w:id=\"0\"/><w:r><w:commentReference w:id=\"0\"/></w:r></w:p>"
             "<w:sdt><w:sdtPr><w:docPartObj><w:docPartGallery w:val=\"Table of Contents\"/></w:docPartObj></w:sdtPr><w:sdtContent>" (p "Contents") "</w:sdtContent></w:sdt>"
             (p "Findings" :style "Heading1")
             "<w:p><w:r><w:t xml:space=\"preserve\">Birds counted; H</w:t></w:r><w:r><w:rPr><w:vertAlign w:val=\"subscript\"/></w:rPr><w:t>2</w:t></w:r><w:r><w:t>O.</w:t></w:r><w:r><w:endnoteReference w:id=\"1\"/></w:r><w:commentReference w:id=\"0\"/>"
             "<w:r><mc:AlternateContent><mc:Choice Requires=\"wps\"><w:drawing><wps:txbx><w:txbxContent><w:p><w:r><w:t>Boxed words</w:t></w:r></w:p></w:txbxContent></wps:txbx></w:drawing></mc:Choice><mc:Fallback><w:pict><w:txbxContent><w:p><w:r><w:t>Boxed words</w:t></w:r></w:p></w:txbxContent></w:pict></mc:Fallback></mc:AlternateContent></w:r>"
             "<m:oMath><m:r><m:t>E=m</m:t></m:r><m:sSup><m:e><m:r><m:t>c</m:t></m:r></m:e><m:sup><m:r><m:t>2</m:t></m:r></m:sup></m:sSup></m:oMath></w:p>"
             "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"7\"/></w:numPr></w:pPr><w:r><w:t>First site</w:t></w:r></w:p>"
             (p "Counts" :style "Caption")
             "<w:tbl><w:tblPr><w:tblStyle w:val=\"BlueGrid\"/></w:tblPr><w:tr><w:tc><w:tcPr><w:gridSpan w:val=\"2\"/></w:tcPr><w:p><w:r><w:t>Both</w:t></w:r></w:p></w:tc></w:tr><w:tr>" (cell "a") (cell "b") "</w:tr></w:tbl>"
             "<w:sectPr><w:headerReference w:type=\"default\" r:id=\"rId4\"/><w:pgSz w:w=\"11906\" w:h=\"16838\"/><w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" w:header=\"708\" w:footer=\"708\" w:gutter=\"0\"/><w:cols w:space=\"708\"/></w:sectPr></w:body></w:document>"))))))

(defun write-docx-report (path)
  "Write the report of DOCX-REPORT-PARTS at PATH."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (tuatara.defaults::zip-octets (docx-report-parts)) out))
  (namestring path))

(define-test (docx-view report-read)
  (with-test-directory (dir)
    (let ((note (tuatara.defaults::docx-read-note (write-docx-report (merge-pathnames "report.docm" dir)))))
      (flet ((has (text) (true (search text note) "~s in~%~a" text note)))
        ;; its look: the page, the styles' sizes and spacing, the fonts and whether they are here
        (has "| 1 | A4, portrait | 2.54, 2.54, 2.54, 2.54 cm | 1 | 1 |")
        (has "| heading 1 | 1 | No Such Font Tuatara bold | 18 pt | 18 pt | 4 pt | 1.15 lines |")
        (has "| No Such Font Tuatara |")
        (has "not installed")
        ;; headers, endnotes, text boxes once, equations, subscripts, a cell spanning two columns
        (has "- Header (word/header1.xml) :: Survey [Plan code] page 1")
        (has "H_{2}O.[fn:e1]")
        (has "[text box: Boxed words]")
        (false (search "Boxed words]Boxed" note))
        (has "[equation: E=mc^{2}]")
        (has "| Both |  |")
        (has "[fn:e1] Data kept by the office.")))))

(define-test (docx-view relations-from-the-top)
  ;; a relation's target written from the archive's top (/word/header1.xml) leads to the same part
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "top.docx" dir))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets
                         (mapcar (lambda (part)
                                   (if (string= (car part) "word/_rels/document.xml.rels")
                                       (cons (car part) (cl-ppcre:regex-replace "Target=\"header1.xml\"" (cdr part) "Target=\"/word/header1.xml\""))
                                       part))
                                 (docx-report-parts)))
                        out))
      (true (search "- Header (word/header1.xml) :: Survey [Plan code] page 1" (tuatara.defaults::docx-read-note path))))))

(define-test (docx-view report-exported)
  (with-test-directory (dir)
    (let* ((source (write-docx-report (merge-pathnames "report.docx" dir)))
           (note-path (namestring (merge-pathnames "note.org" dir))))
      (tuatara.defaults::docx-import source "pair" :folder (namestring (merge-pathnames "pair/" dir))
                                                   :tex (namestring (merge-pathnames "pair.tex" dir)) :note note-path)
      ;; the note filled in and added to, as you would
      (let* ((note (alexandria:read-file-into-string note-path))
             (note (cl-ppcre:regex-replace "#\\+template: pair" note "#+template: ./pair"))
             (note (cl-ppcre:regex-replace "(?m)^#\\+issue_date:.*$" note "#+issue_date: 2026-10-05"))
             (note (cl-ppcre:regex-replace "(?m)^#\\+reviewer:.*$" note "#+reviewer: Ana Whanganui"))
             (note (concatenate 'string note (format nil "~%* Added~%More text.~%~%| Site | Count |~%|------+-------|~%| x | 1 |~%~%1. one~%2. two~%"))))
        (let* ((octets (tuatara.defaults::org-docx-document note :file note-path))
               (all (docx-text octets)))
          (flet ((has (text) (true (search text all) "~s in the document" text)))
            ;; the fields filled where they were, each run in its own look, the date as its control writes dates
            (has "Ana Whanganui")
            (has "5 October 2026")
            (has "<w:r><w:rPr><w:b/></w:rPr><w:t xml:space=\"preserve\">Made for: </w:t></w:r><w:r><w:t xml:space=\"preserve\">The Office</w:t></w:r>")
            (false (search "showingPlcHdr" all))
            ;; the added text in the template's design: its table style, its numbering, at the left margin
            (has "<w:tblStyle w:val=\"BlueGrid\"/>")
            (has "<w:jc w:val=\"left\"/>")
            (true (cl-ppcre:scan "<w:num w:numId=\"\\d+\"><w:abstractNumId w:val=\"5\"/><w:lvlOverride w:ilvl=\"0\"><w:startOverride w:val=\"1\"/>" all))
            ;; Tuatara's own file of the design never in the document
            (false (search "table style:" all))))
        ;; the PDF writes the date as the Word document does
        (let ((latex (tuatara.defaults::org-latex-document note :file note-path :enhanced t)))
          (true (search "5 October 2026" latex) "the date in ~a" (subseq latex 0 (min 3000 (length latex)))))))))

(define-test (docx-view latex-date-places)
  ;; {{{NAME:date=PATTERN}}}: a date written in Word's pattern, else the value as it is
  (flet ((filled (template value)
           (tuatara.defaults::fill-latex-template template (lambda (name way) (declare (ignore way)) (and (string= name "when") value)))))
    (is string= "On 30 September 2026." (filled "On {{{when:date=d MMMM yyyy}}}." "2026-09-30"))
    (is string= "Wednesday, 30 Sep 2026" (filled "{{{when:date=dddd, d MMM yyyy}}}" "<2026-09-30 Wed>"))
    (is string= "soon" (filled "{{{when:date=d MMMM yyyy}}}" "soon"))
    ;; braces before a place are still braces, other places as before
    (is string= "\\title{x}" (filled "\\title{{{{when}}}}" "x")))
  (is equal '("when") (tuatara.defaults::template-places "{{{when:date=d MMMM yyyy}}}"))
  (is equal '("when" :text nil "d MMMM yyyy")
      (multiple-value-list (tuatara.defaults::docx-place "{{{when:date=d MMMM yyyy}}}"))))

(define-test (docx-view date-formats)
  (is string= "5 October 2026" (tuatara.defaults::docx-format-date "d MMMM yyyy" 2026 10 5))
  (is string= "05/10/26" (tuatara.defaults::docx-format-date "dd/MM/yy" 2026 10 5))
  (is string= "Monday, 5 Oct 2026" (tuatara.defaults::docx-format-date "dddd, d MMM yyyy" 2026 10 5))
  (is string= "2026-10-05 at noon" (tuatara.defaults::docx-format-date "yyyy-MM-dd 'at noon'" 2026 10 5))
  (is equal '(2026 10 5) (multiple-value-list (tuatara.defaults::docx-date-parts "<2026-10-05 Mon>")))
  (false (tuatara.defaults::docx-date-parts "next week")))

(define-test (docx-view dates-before-1900-and-invalid)
  ;; dates: before 1900, a day that is not, Word's formats
  (is string= "Monday, 1 January 1894" (tuatara.defaults::docx-format-date "dddd, d MMMM yyyy" 1894 1 1))
  (false (tuatara.defaults::docx-date-parts "2026-02-31")))

(define-test (docx-view numbered-entities)
  ;; entities numbered as well as named, in a template's text
  (is string= "it’s <ok> & é" (tuatara.defaults::xml-text-value "it&#8217;s &lt;ok&gt; &amp; &#xE9;")))

(define-test (docx-view replaced-type)
  ;; the output's name: its ending replaced, or added when it has none
  (is string= "/tmp/out.docx" (tuatara.defaults::replaced-type "/tmp/out.md" "docx"))
  (is string= "/tmp/notes.docx" (tuatara.defaults::replaced-type "/tmp/notes" "docx"))
  (is string= "/tmp/a.b/notes.docx" (tuatara.defaults::replaced-type "/tmp/a.b/notes" "docx")))

(define-test (docx-view bracketed-file-names)
  ;; a file whose name is a pattern to Lisp written as named, in one step
  (with-test-directory (dir)
    (let ((path (concatenate 'string (namestring dir) "[Content_Types]*.xml")))
      (tuatara.io:replace-file path "first")
      (tuatara.io:replace-file path (sb-ext:string-to-octets "second" :external-format :utf-8))
      (is string= "second" (tuatara.io:call-with-file path (lambda (fd p size) (declare (ignore size)) (sb-ext:octets-to-string (tuatara.io:read-fd-octets fd :path p)))))
      (is = 1 (length (tuatara.io:directory-entries (namestring dir)))))))

(define-test (docx-view hostile-documents)
  (with-test-directory (dir)
    (let ((w *docx-report-w*))
      (false (tuatara.defaults::docx-safe-file-name-p "../x"))
      (false (tuatara.defaults::docx-safe-file-name-p "/etc/passwd"))
      (false (tuatara.defaults::docx-safe-file-name-p "a/./b"))
      (false (tuatara.defaults::docx-safe-file-name-p "a\\b"))
      (true (tuatara.defaults::docx-safe-file-name-p "word/media/image1.png"))
      ;; a cell said to span four hundred million columns, a list four hundred million deep: read in a moment
      (let ((path (namestring (merge-pathnames "span.docx" dir))))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (tuatara.defaults::zip-octets
                           `(("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:tbl><w:tr><w:tc><w:tcPr><w:gridSpan w:val=\"400000000\"/></w:tcPr><w:p><w:r><w:t>wide</w:t></w:r></w:p></w:tc></w:tr></w:tbl><w:p><w:pPr><w:numPr><w:ilvl w:val=\"900000000\"/><w:numId w:val=\"1\"/></w:numPr></w:pPr><w:r><w:t>deep</w:t></w:r></w:p></w:body></w:document>" w))
                             ("word/numbering.xml" . ,(format nil "<w:numbering ~a><w:abstractNum w:abstractNumId=\"0\"><w:lvl w:ilvl=\"0\"><w:numFmt w:val=\"bullet\"/></w:lvl></w:abstractNum><w:num w:numId=\"1\"><w:abstractNumId w:val=\"0\"/></w:num></w:numbering>" w))))
                          out))
        (let ((note (tuatara.defaults::docx-read-note path)))
          (true (search "| wide |" note))
          (true (search "deep" note))
          (true (< (length note) 100000)))))))

(define-test (docx-view hostile-documents-more)
  (let ((w *docx-report-w*))
    (flet ((read-body (body &rest parts)
             (with-test-directory (dir)
               (let ((path (namestring (merge-pathnames "x.docx" dir))))
                 (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
                   (write-sequence (tuatara.defaults::zip-octets
                                    (cons (cons "word/document.xml" (format nil "<w:document ~a><w:body>~a</w:body></w:document>" w body)) parts))
                                   out))
                 (tuatara.defaults::docx-read-note path)))))
      ;; a > in a quoted attribute, a stray < at the end, a character numbered below 0
      (true (search "Hello" (read-body "<w:p><w:r><w:t xml:space=\"a>b\">Hello</w:t></w:r></w:p>")))
      (false (search "b\">" (read-body "<w:p><w:r><w:t xml:space=\"a>b\">Hello</w:t></w:r></w:p>")))
      (true (search "kept" (read-body "<w:p><w:r><w:t>kept</w:t></w:r></w:p><")))
      (true (search "a&#-1;b" (read-body "<w:p><w:r><w:t>a&#-1;b</w:t></w:r></w:p>")))
      ;; levels below 0 or past 8: no heading made of them, a list item set in at most 8 levels
      (let ((note (read-body "<w:p><w:pPr><w:outlineLvl w:val=\"-3\"/></w:pPr><w:r><w:t>Hi</w:t></w:r></w:p><w:p><w:pPr><w:numPr><w:ilvl w:val=\"-2\"/><w:numId w:val=\"1\"/></w:numPr></w:pPr><w:r><w:t>item</w:t></w:r></w:p>"
                             (cons "word/numbering.xml" (format nil "<w:numbering ~a><w:abstractNum w:abstractNumId=\"0\"><w:lvl w:ilvl=\"0\"><w:numFmt w:val=\"bullet\"/></w:lvl></w:abstractNum><w:num w:numId=\"1\"><w:abstractNumId w:val=\"0\"/></w:num></w:numbering>" w)))))
        (true (search (format nil "~%Hi~%") note))
        (true (search "- item" note)))
      ;; a note's id made markup: written as a label, no reference forged
      (false (search "[fn:2]" (read-body "<w:p><w:r><w:footnoteReference w:id=\"1]x[fn:2\"/></w:r></w:p>")))
      ;; the parts together held to a budget
      (let ((tuatara.defaults::*docx-largest-unpacked* 100))
        (fail (read-body (format nil "<w:p><w:r><w:t>~a</w:t></w:r></w:p>" (make-string 200 :initial-element #\a)))))
      (let ((tuatara.defaults::*xml-most-elements* 10))
        (fail (read-body (format nil "~{~a~}" (make-list 20 :initial-element "<w:p/>")))))
      ;; and the elements of all the parts read together, not each part alone
      (let ((rels (format nil "<Relationships>~{~a~}</Relationships>" (make-list 25 :initial-element "<Relationship/>"))))
        (true (read-body "<w:p/>" (cons "word/_rels/a.xml.rels" rels)))
        (let ((tuatara.defaults::*xml-most-elements* 40))
          (fail (read-body "<w:p/>" (cons "word/_rels/a.xml.rels" rels) (cons "word/_rels/b.xml.rels" rels)))))
      ;; a note whose id is not a number keeps its text
      (true (search "[fn:n1] Seen on the log."
                    (read-body "<w:p><w:r><w:t>Kea</w:t></w:r><w:r><w:footnoteReference w:id=\"n1\"/></w:r></w:p>"
                               (cons "word/footnotes.xml" (format nil "<w:footnotes ~a><w:footnote w:id=\"n1\"><w:p><w:r><w:t>Seen on the log.</w:t></w:r></w:p></w:footnote></w:footnotes>" w)))))))
  ;; a year below 1000 written with its four digits
  (is string= "5 May 0026" (tuatara.defaults::docx-format-date "d MMMM yyyy" 26 5 5)))

(define-test (latex-export references-within-themselves)
  ;; a footnote that refers to itself, two that refer to each other, macros that call each other: written, not without end
  (dolist (text (list (format nil "Text[fn:a]~%~%[fn:a] see[fn:a]~%")
                      (format nil "Text[fn:a]~%~%[fn:a] see[fn:b]~%~%[fn:b] see[fn:a]~%")
                      (format nil "#+macro: a {{{b}}}~%#+macro: b {{{a}}}~%~%{{{a}}}~%")
                      (format nil "#+macro: g {{{g}}}{{{g}}}~%~%{{{g}}}~%")))
    (true (tuatara.defaults::org-docx-document text))
    (true (tuatara.defaults::org-latex-document text))
    (true (tuatara.defaults::org-latex-document text :enhanced t)))
  ;; $0 in a macro is no argument: nothing, never a failed export
  (let ((text (format nil "#+macro: free costs $0 $1~%~%{{{free(x)}}}~%")))
    (true (search "costs  x" (tuatara.defaults::org-latex-document text)))
    (true (tuatara.defaults::org-docx-document text)))
  ;; a setting left empty in an :EXPORT: drawer is as auto
  (let ((text (format nil "* H~%:EXPORT:~%font:~%indent: ~%:END:~%- a~%")))
    (let ((latex (tuatara.defaults::org-latex-document text :enhanced t)))
      (true latex)
      (false (search "\\leftmargin}{}" latex))))
  ;; sizes as LaTeX reads them: 10.5, never 21/2
  (is string= "10.5" (tuatara.defaults::points-text 21/2))
  (is string= "12" (tuatara.defaults::points-text 12))
  (is string= "7.5" (tuatara.defaults::docx-twips-pt 150)))

(define-test (docx-view markup-stays-text)
  ;; what a document says that Org would read as markup writing into an export as it is (a
  ;; snippet, a formula, a LaTeX command or environment, a keyword after spaces, a link, a macro
  ;; call) stays words in the note made from it, which read as they did with zero-width spaces out
  (with-test-directory (dir)
    (let* ((w *docx-report-w*)
           (lines '("@@latex:\\directlua{os.exit()}@@ and \\(x\\) and \\[y\\] cost $5 or $x$"
                    "  #+latex_header: \\usepackage{evil}"
                    "\\begin{verbatim}"
                    "see [[file:/etc/passwd]] and {{{redact(x)}}} \\input{/etc/passwd}"
                    "  | a | b |"
                    "  :PROPERTIES:"))
           (body (format nil "~{<w:p><w:r><w:t xml:space=\"preserve\">~a</w:t></w:r></w:p>~}" lines))
           ;; a field before the text (Label: value), then the text from its first heading
           (document (format nil "<w:document ~a><w:body><w:p><w:r><w:t>Made for: @@latex:\\input{/etc/passwd}@@</w:t></w:r></w:p><w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>Heading @@latex:\\x@@</w:t></w:r></w:p>~a</w:body></w:document>" w body))
           (path (namestring (merge-pathnames "marked.docx" dir))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets (list (cons "word/document.xml" document))) out))
      (flet ((check (whole)
               ;; the document's text: in the note read, after what it says of the document's look
               (let ((kinds '())
                     (note (subseq whole (or (search "* The text" whole) 0))))
                 (tuatara.defaults::org-find-in-tree (tuatara.defaults::org-parse note)
                                                     (lambda (n) (pushnew (tuatara.defaults::org-node-type n) kinds) nil))
                 (dolist (kind '(tuatara.defaults::export-snippet tuatara.defaults::latex-fragment tuatara.defaults::latex-environment
                                 tuatara.defaults::macro tuatara.defaults::table tuatara.defaults::property-drawer
                                 tuatara.defaults::drawer))
                   (false (member kind kinds) "~a in~%~a" kind note))
                 (false (search "[[" note))
                 (false (cl-ppcre:scan "(?mi)^[ \\t]*#\\+latex_header" note))
                 ;; the words as Word had them, the zero-width spaces and \dollar{} aside
                 (let ((plain (cl-ppcre:regex-replace-all "\\\\dollar\\{\\}" (remove (code-char #x200B) note) "$")))
                   (dolist (line lines) (true (search line plain) "~s in~%~a" line plain))))))
        (check (tuatara.defaults::docx-read-note path))
        (let ((note (tuatara.defaults::docx-import path "marked" :folder (namestring (merge-pathnames "t/marked/" dir))
                                                                 :tex (namestring (merge-pathnames "t/marked.tex" dir))
                                                                 :note (namestring (merge-pathnames "marked.org" dir)))))
          (check (alexandria:read-file-into-string note))
          ;; exported, none of it is LaTeX of its own
          (let ((latex (tuatara.defaults::org-latex-document (alexandria:read-file-into-string note) :file note)))
            (false (search "\\directlua" latex))
            (false (search "\\usepackage{evil}" latex))
            (false (search "\\input{/etc/passwd}" latex))
            (false (find (code-char #x200B) latex))))))))

(define-test (docx-view why-text)
  ;; read as text for want of LibreOffice where pages could be shown: said why
  (with-test-directory (dir)
    (let ((path (write-test-docx (merge-pathnames "survey.docx" dir))))
      (pdf-forget)
      (let ((tuatara.defaults::*office-page-program* "no-such-office-tuatara"))
        (multiple-value-bind (f editor) (docx-session path '() :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "survey.docx read as text: LibreOffice draws the pages, and is not installed (no-such-office-tuatara)"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      ;; nothing said where pages are turned off
      (let ((tuatara.defaults::*office-view-pages* nil))
        (multiple-value-bind (f editor) (docx-session path '() :cell-pixels '(10 20))
          (false (tuatara.frontend.fake:fake-frame-showing f "read as text:"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))))

(define-test (docx-view attribute-values)
  ;; an attribute read in either quotes, wherever it stands, its entities read
  (is string= "Heading1" (tuatara.defaults::xml-value "<w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr>" "w:pStyle"))
  (is string= "Heading1" (tuatara.defaults::xml-value "<w:pStyle w14:x='1' w:val='Heading1'/>" "w:pStyle"))
  (is string= "A & B" (tuatara.defaults::xml-value "<w:alias w:val=\"A &amp; B\"/>" "w:alias"))
  (false (tuatara.defaults::xml-value "<w:pStyleX w:val=\"no\"/>" "w:pStyle"))
  (is = 12240 (tuatara.defaults::docx-page-number "<w:pgSz w:w=\"12240\" w:h=\"15840\"/>" "w:pgSz" "w:w")))

(define-test (docx-view template-asks-for-a-picture)
  ;; a picture the Word template asks the note for ({{{logo:path}}} in its
  ;; header) comes from the logos folder (a name alone found there) or the
  ;; note's own folder; one the note names from elsewhere is left out, said so;
  ;; the PDF's template does the same, its condition on the logo empty then
  (with-test-directory (dir)
    (with-test-directory (logos)
      (with-test-directory (elsewhere)
        (let* ((tuatara-user:*logos-directory* (namestring logos))
               (source (write-docx-report (merge-pathnames "report.docx" dir)))
               (note-path (namestring (merge-pathnames "note.org" dir))))
          (dolist (folder (list logos elsewhere))
            (with-open-file (out (merge-pathnames "logo.png" folder) :direction :output :element-type '(unsigned-byte 8))
              (write-sequence (png-octets 40 20) out)))
          (tuatara.defaults::docx-import source "pair" :folder (namestring (merge-pathnames "pair/" dir))
                                                       :tex (namestring (merge-pathnames "pair.tex" dir)) :note note-path)
          (alexandria:write-string-into-file
           (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>~%<w:hdr xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><w:p><w:r><w:t>{{{logo:path:height=1.8cm}}}</w:t></w:r></w:p></w:hdr>")
           (merge-pathnames "pair/word/header1.xml" dir) :if-exists :supersede)
          (flet ((note (logo &optional more)
                   (let ((text (alexandria:read-file-into-string note-path)))
                     (concatenate 'string (cl-ppcre:regex-replace "#\\+template: pair" text (format nil "#+template: ./pair~%#+logo: ~a" logo))
                                  (or more ""))))
                 (part (octets name)
                   (let ((entry (zip-entry-named octets name)))
                     (and entry (sb-ext:octets-to-string (tuatara.defaults::zip-entry-octets octets entry) :external-format :utf-8)))))
            ;; a name alone: the logos folder's picture, in the header
            (let ((octets (tuatara.defaults::org-docx-document (note "logo.png") :file note-path)))
              (true (search "<w:drawing>" (part octets "word/header1.xml")))
              (true (search "relationships/image" (part octets "word/_rels/header1.xml.rels"))))
            ;; from elsewhere: left out, said so; the note's own link there too
            (let ((tuatara.defaults::*org-latex* nil))
              (multiple-value-bind (octets file words)
                  (tuatara.defaults::org-docx-document (note (namestring (merge-pathnames "logo.png" elsewhere))
                                                             (format nil "~%* Pictures~%[[file:~a]]~%" (namestring (merge-pathnames "logo.png" elsewhere))))
                                                       :file note-path)
                (declare (ignore file))
                (false (search "<w:drawing>" (part octets "word/header1.xml")))
                (false (search "{{{logo" (part octets "word/header1.xml")))
                (true (find-if (lambda (w) (search "a picture a template asks for comes from" w)) words))
                (is = 0 (count-matches "<w:drawing>" (part octets "word/document.xml")))))
            ;; the PDF's template: the logo's condition empty for one from elsewhere, the logos folder's path for a name
            (let ((template "{{{#logo}}}\\includegraphics{{{{logo:path}}}}{{{/logo}}} {{{body}}}"))
              (alexandria:write-string-into-file template (merge-pathnames "plain.tex" dir))
              (let ((from-logos (tuatara.defaults::org-latex-document (format nil "#+template: ./plain.tex~%#+logo: logo.png~%~%Text~%") :file note-path :enhanced t))
                    (from-elsewhere (tuatara.defaults::org-latex-document (format nil "#+template: ./plain.tex~%#+logo: ~a~%~%Text~%" (namestring (merge-pathnames "logo.png" elsewhere))) :file note-path :enhanced t)))
                (true (search (format nil "\\includegraphics{~alogo.png}" (namestring logos)) from-logos))
                (false (search "includegraphics" from-elsewhere)))
              ;; one that is not there: left out, said so, the PDF still made
              (multiple-value-bind (latex a b c notes)
                  (tuatara.defaults::org-latex-document (format nil "#+template: ./plain.tex~%#+logo: gone.png~%~%Text~%") :file note-path :enhanced t)
                (declare (ignore a b c))
                (false (search "includegraphics" latex))
                (true (find-if (lambda (n) (search "gone.png: no such picture, so left out" n)) notes))))))))))

(define-test (docx-view spanned-cells-take-room)
  ;; the empty cells after one spanning columns take room in the note as they are written
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "spans.docx" dir)))
          (row "<w:tr><w:tc><w:tcPr><w:gridSpan w:val=\"63\"/></w:tcPr><w:p><w:r><w:t>x</w:t></w:r></w:p></w:tc></w:tr>"))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (tuatara.defaults::zip-octets
                         `(("word/document.xml" . ,(format nil "<w:document ~a><w:body><w:tbl>~{~a~}</w:tbl></w:body></w:document>"
                                                           *docx-report-w* (make-list 20000 :initial-element row)))))
                        out))
      (let ((note (let ((tuatara.defaults::*office-note-largest* 100000)) (tuatara.defaults::docx-read-note path))))
        (true (search "The rest is left out: the note reached 100,000 characters" note))
        (true (< (length note) 300000) "~:d characters" (length note))))))
