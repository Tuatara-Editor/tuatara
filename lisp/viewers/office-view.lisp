;;;; lisp/viewers/office-view.lisp --- Word, Excel and PowerPoint documents opened safely: as their pages, or read as text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Office document opened is shown as its pages: a Word document
;;;; (.docx, .docm, .dotx, .dotm), an Excel workbook (.xlsx, .xlsm, .xltx,
;;;; .xltm), a PowerPoint presentation (.pptx, .pptm, .potx, .potm, .ppsx,
;;;; .ppsm), one of before 2007 (.doc, .xls, .ppt) or an OpenDocument one
;;;; (.odt, .ods, .odp). LibreOffice, shut in by the sandbox as the programs
;;;; drawing PDFs are (no network, no files of yours but your fonts, read
;;;; only, nothing kept after), makes a PDF of it, which the PDF view shows
;;;; (pdf-view-mode, its keys all). T there switches a Word document, an
;;;; Excel workbook, a PowerPoint presentation or an OpenDocument one to it
;;;; read as text by Tuatara itself (lisp/org/docx-read.lisp,
;;;; office-read.lisp, odf-read.lisp), an Org note, read only, and T there
;;;; back to its pages; those of before 2007 to the text of their PDF.
;;;; Without LibreOffice, or with *OFFICE-VIEW-PAGES* NIL, or where pictures
;;;; are not shown, those Tuatara reads are read as text at once, the others
;;;; opened as any file. None runs its macros, fetches what it links
;;;; to, or changes the file. Opened safely, it is shown as its octets, as
;;;; any file is.

(in-package #:tuatara.defaults)

(defvar *office-view-pages* t
  "True to show a Word document, an Excel workbook, a PowerPoint
presentation (and the older and OpenDocument kinds of them) opened as its
pages, drawn by LibreOffice shut in by the sandbox, when it is installed;
\\<pdf-view-mode>\\[pdf-view-toggle-text] switches to it read as text. NIL to
show the first three read as text always, by Tuatara itself, and to open the
others as any file, LibreOffice never run.")

(defparameter *office-page-program* "soffice"
  "LibreOffice's program, which makes the PDF of an Office document's pages.")

(defparameter *office-formats*
  '((:kind :docx :words "Word document" :names ("docx" "docm" "dotx" "dotm") :container :zip :kinds (:docx) :read docx-read-note)
    (:kind :xlsx :words "Excel workbook" :names ("xlsx" "xlsm" "xltx" "xltm") :container :zip :kinds (:xlsx) :read xlsx-read-note)
    (:kind :pptx :words "PowerPoint presentation" :names ("pptx" "pptm" "potx" "potm" "ppsx" "ppsm") :container :zip :kinds (:pptx)
     :read pptx-read-note)
    (:kind :doc :words "Word document of before 2007" :names ("doc" "dot") :container :ole :kinds (:ole))
    (:kind :xls :words "Excel workbook of before 2007" :names ("xls" "xlt") :container :ole :kinds (:ole))
    (:kind :ppt :words "PowerPoint presentation of before 2007" :names ("ppt" "pot" "pps") :container :ole :kinds (:ole))
    (:kind :odt :words "OpenDocument text" :names ("odt" "ott") :container :zip :kinds (:odt) :read odf-read-note)
    (:kind :ods :words "OpenDocument spreadsheet" :names ("ods" "ots") :container :zip :kinds (:ods) :read odf-read-note)
    (:kind :odp :words "OpenDocument presentation" :names ("odp" "otp") :container :zip :kinds (:odp) :read odf-read-note))
  "The kinds of Office document shown by their views, each a plist: :KIND, a
keyword naming it; :WORDS, what it is called; :NAMES, the endings of its
files' names; :CONTAINER, :ZIP or :OLE, what its first octets must be;
:KINDS, what TUATARA.IO:OCTETS-KIND calls it by its contents; :READ, the
function reading it as an Org note (of its PATH, and :OCTETS), or none for a
kind shown only as its pages.")

(defvar *office-readings* '()
  "The buffers showing an Office document read as text.")

(defvar *office-reading-keymap* (make-keymap :name "Office document read as text, before the modal keys")
  "Keys of an Office document read as text that come before the modal keys:
\\`T', back to its pages.")

(defun office-file-format (native)
  "The kind of Office document (an entry of *OFFICE-FORMATS*) the file at
NATIVE is: by the ending of its name, its first octets those of a zip
archive or of a compound file as the kind's are; NIL for any other file."
  (let* ((type (tuatara.io:native-file-type native))
         (office (and type (find-if (lambda (f) (member type (getf f :names) :test #'string-equal)) *office-formats*)))
         (head (and office (tuatara.io:file-head native))))
    (and head
         (tuatara.io:octets-at-p head (if (eq (getf office :container) :ole) tuatara.io:*compound-file-signature* (list #x50 #x4B 3 4)))
         office)))

(defun office-preview-format (kind name)
  "The kind of Office document (an entry of *OFFICE-FORMATS*) a file whose
contents say KIND (TUATARA.IO:OCTETS-KIND) is, named NAME: by its contents,
and for a compound file (which may also be a mail or an installer) by its
name's ending too; NIL for any other."
  (let ((type (tuatara.io:native-file-type name)))
    (find-if (lambda (f) (and (member kind (getf f :kinds))
                              (or (not (eq (getf f :container) :ole))
                                  (member type (getf f :names) :test #'string-equal))))
             *office-formats*)))

(defun office-ending (office name)
  "The ending LibreOffice is given the document of the kind OFFICE as, NAME's when it
is one of the kind's, else the kind's first: one of *OFFICE-FORMATS*'s own
words, never the file's."
  (let ((type (tuatara.io:native-file-type name)))
    (or (find type (getf office :names) :test #'string-equal) (first (getf office :names)))))

;;; Its pages

(defun office-pdf-copy (fd ending &key cancel)
  "The PDF LibreOffice makes, shut in (SANDBOX-PDF), of the Office document
open at FD, given to it as a file ending in ENDING (OFFICE-ENDING), as a
sealed file in memory; an error saying why when it makes none. CANCEL, a
cons, stops it when its car turns true."
  (sandbox-pdf "sh" (list "-c" (concatenate 'string "cat > \"/tmp/in.$2\" && \"$1\" --headless --norestore --nolockcheck"
                                            " -env:UserInstallation=file:///tmp/profile --convert-to pdf --outdir /tmp"
                                            " \"/tmp/in.$2\" >&2; exec cat /tmp/in.pdf")
                          "sh" *office-page-program* ending)
               fd :label "LibreOffice" :cancel cancel))

(defun office-no-pages-words ()
  "Why an Office document cannot be shown as its pages for want of the
sandbox, a program for PDFs or LibreOffice, in words; NIL when it is not for
want of them (pages not wanted, or pictures not shown)."
  (when (and *office-view-pages* (tuatara.frontend:frontend-images-p (editor-frontend *editor*)))
    (let ((status (tuatara.io:sandbox-status-now)))
      (cond ((not (member status '(:ready :unknown))) (no-sandbox-words status))
            ((null (find-pdf-decoder)) (format nil "no program for PDFs (install ~a)" (getf (first *pdf-decoders*) :name)))
            ((not (tuatara.io:sandbox-find-program *office-page-program*))
             (format nil "LibreOffice draws the pages, and is not installed (~a)" *office-page-program*))))))

(defun office-pages-possible-p ()
  "True when an Office document can be shown as its pages here: wanted, pictures
shown, the sandbox ready (or not tried yet), and LibreOffice and a program
drawing PDFs in it."
  (and *office-view-pages*
       (tuatara.frontend:frontend-images-p (editor-frontend *editor*))
       (null (office-no-pages-words))))

(defun office-show-reading (pages)
  "Show, in PAGES' place, the Office document PAGES shows read as text."
  (let* ((native (pdf-view-path (pdf-view-of pages)))
         (reading (office-reading-buffer native (office-file-format native) pages)))
    (show-buffer reading)
    (message "Read as text; \\[office-view-pages] shows its pages")))

(defun office-page-words ()
  "What shows Office documents here, as M-x check-health says it."
  (if (tuatara.io:sandbox-find-program *office-page-program*)
      (format nil "Word, Excel, PowerPoint and OpenDocument files' pages (and those of before 2007) by ~a (LibreOffice: its Writer, Calc and Impress), their text by Tuatara"
              *office-page-program*)
      (format nil "Word, Excel, PowerPoint and OpenDocument files' text by Tuatara, their pages by none (~a): ~a"
              *office-page-program* (outside-program-install-words :office))))

;;; In rimu's preview

(defun office-preview (fd path size office &key pixels cancel)
  "The preview of the Office document of the kind OFFICE (*OFFICE-FORMATS*) open as
FD, PATH's, of SIZE octets, as three values: the document read as text by
its kind's reader, or NIL for a kind shown only as its pages; notes for the
line over it, each (TEXT . FACE): how many pages, or why they were not
drawn; and with PIXELS, the most width and height in pixels, its first page
drawn as large as fits there by LibreOffice shut in by the sandbox, when it
is installed and *OFFICE-VIEW-PAGES* is true, or NIL. CANCEL, a cons, stops
it when its car turns true. Waits for the programs: call it from a thread
working in the background."
  (let ((text (let ((read (getf office :read)))
                (and read (funcall read path :octets (fd-octets fd path size (symbol-value '*docx-largest-file*)) :cancel cancel))))
        (notes '()) (image nil))
    (when (and pixels *office-view-pages* (not (car cancel))
               (tuatara.io:sandbox-find-program *office-page-program*) (find-pdf-decoder))
      (handler-case
          (let ((pdf (office-pdf-copy fd (office-ending office (tuatara.io:native-file-name path)) :cancel cancel)))
            (unwind-protect
                 (multiple-value-bind (pdf-text pdf-notes pdf-image) (decode-pdf pdf :cancel cancel :pixels pixels :limit 4096)
                   (declare (ignore pdf-text))
                   ;; its pages counted; what the PDF's text was is not this text's
                   (setf notes (remove-if-not (lambda (note) (cl-ppcre:scan "^, [0-9,]+ pages?$" (car note))) pdf-notes)
                         image pdf-image))
              (ignore-errors (sb-posix:close pdf))))
        (error () (push (cons ", its pages not drawn (the debug log says why)" :comment) notes))))
    (values text notes image)))

;;; Read as text

(defstruct (office-read-job (:include tuatara.runtime:job) (:constructor make-office-read-job (buffer path office)) (:copier nil))
  "An Office document at PATH, of the kind OFFICE (*OFFICE-FORMATS*), being
read as text for BUFFER, a job; TEXT, the Org note made of it."
  path office text)

(setf (documentation 'office-read-job-p 'function) "True when OBJECT is an OFFICE-READ-JOB.")

(tuatara.runtime:define-job-words office-read-job
  :doing (format nil "Reading the ~a" (getf (office-read-job-office job) :words))
  :description (format nil "Reading ~a as text" (tuatara.io:native-file-name (office-read-job-path job)))
  :verb (format nil "read the ~a" (getf (office-read-job-office job) :words))
  :left-words "nothing read")

(defmethod tuatara.runtime:job-take-in ((job office-read-job))
  "Show the Office document read as text, or say why it could not be."
  (show-reading-result job (office-read-job-path job) (office-read-job-text job)))

(defun office-reading-buffer (native office &optional pages)
  "The buffer showing the Office document at NATIVE, of the kind OFFICE
\(*OFFICE-FORMATS*, a kind with a reader), read as text, read in the
background when there is none yet; PAGES, the buffer showing its pages, if
any."
  (let ((name (tuatara.io:native-file-name native)))
    (reading-buffer '*office-readings* (lambda (b) (tuatara.buffer:buffer-local b 'office-reading)) native
                    (format nil "~a (text)" name) (format nil "Reading ~a...~%" name) 'org-mode
                    (lambda (buffer)
                      (setf (tuatara.buffer:buffer-local buffer 'office-reading) native
                            (tuatara.buffer:buffer-local buffer 'office-pages) pages)
                      (when pages (setf (tuatara.buffer:buffer-local buffer :override-keymap) *office-reading-keymap*))
                      (tuatara.runtime:start-job-thread
                       (make-office-read-job buffer native office)
                       (lambda (job) (setf (office-read-job-text job) (funcall (getf (office-read-job-office job) :read) (office-read-job-path job))))
                       :name "tuatara office reading")))))

(define-command office-view-pages ()
  "Show the pages of the Office document read as text here, in its place."
  (let ((pages (tuatara.buffer:buffer-local (current-buffer) 'office-pages)))
    (if (live-buffer-p pages)
        (show-buffer pages)
        (message "Its pages are not shown: open the document again"))))

;;; Opening

(defun office-view-visit (path show)
  "Show the Office document at PATH (OFFICE-FILE-FORMAT), with SHOW, a
function of the buffer: as its pages when they can be drawn
\(OFFICE-PAGES-POSSIBLE-P); else one Tuatara reads (a Word document, an
Excel workbook, a PowerPoint presentation, an OpenDocument file) read as text, saying why when that is for want of
the sandbox, LibreOffice or a program for PDFs, and any other left to be
opened as any file, saying why. Returns the buffer, or NIL for a file this
view does not show."
  (let* ((native (file-view-path path))
         (office (and native (office-file-format native))))
    (when office
      (cond ((office-pages-possible-p)
             (file-view-show native *pdf-views* (lambda (b) (let ((v (pdf-view-of b))) (and v (pdf-view-path v))))
                             (lambda ()
                               (let ((ending (office-ending office (tuatara.io:native-file-name native))))
                                 (pdf-view-make native :make (lambda (fd) (office-pdf-copy fd ending))
                                                       :reading (and (getf office :read) #'office-show-reading)
                                                       :words (format nil "~a, its pages drawn by LibreOffice" (getf office :words)))))
                             show))
            ((getf office :read)
             ;; read as text for want of what draws pages, not by choice: said
             (let ((why (office-no-pages-words)))
               (when why (message "~a read as text: ~a" (tuatara.io:native-file-name native) why)))
             (file-view-show native '() #'identity (lambda () (office-reading-buffer native office)) show))
            (t (file-view-refuses "~a opens as octets: ~a" (tuatara.io:native-file-name native)
                                  (or (office-no-pages-words) "its pages are not shown here (*office-view-pages*)"))
               nil)))))

(defun install-office-view ()
  "Have Office documents visited shown as their pages or read as text, and bind \\`T' in their text."
  (define-key *office-reading-keymap* "T" 'office-view-pages)
  (pushnew 'office-view-visit *file-views*))
