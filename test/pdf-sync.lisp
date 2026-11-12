;;;; test/pdf-sync.lisp --- A line of a LaTeX or Org file shown in its PDF, and back.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test pdf-sync
  :description "A PDF made by export-latex-compile-pdf-buffer has a SyncTeX file among its working files.
In the LaTeX file C-c C-v (show-line-in-pdf) shows the line at point in the PDF's view beside, its page
turned and the place marked; in the view C-c C-v (pdf-view-show-source) shows the line of the part
shown, and a double click on the page the line of that point. Both work from an Org file exported in
the session. A PDF missing or older than the file is made again before the line is shown; one without
a SyncTeX file is said. The SyncTeX file and the PDF's page tree are read as data alone, every number
and count bounded.")

(defparameter *synctex-sample*
  "SyncTeX Version:1
Input:1:/tmp/sample/./a.tex
Input:2:/usr/share/texmf/article.cls
Output:pdf
Magnification:1000
Unit:1
X Offset:0
Y Offset:0
Content:
!699
{1
[1,9:4736287,28155905:14899938,23419618,0
[1,9:6578177,26189825:13058048,19611648,0
(1,6:6578177,7233537:13058048,655360,7208
(1,5:6578177,7233537:655360,655360,0
r1,9:7233537,7233537:655360,655360,0
)
g1,5:8831960,7233537
g1,5:10069935,7233537
k1,6:19636225,7233537:8180859
g1,6:19636225,7233537
)
(1,9:6578177,8019969:13058048,462029,135003
h1,7:6578177,8019969:983040,0,0
g1,7:9782232,8019969
g1,7:12918130,8019969
g1,8:13828425,8019969
g1,8:15065745,8019969
k1,9:19636225,8019969:3110338
g1,9:19636225,8019969
)
]
]
!755
}1
{2
[1,11:4736287,28155905:14899938,23419618,0
(1,11:6578177,7233537:13058048,447611,135003
h1,10:6578177,7233537:983040,0,0
g1,10:9153742,7233537
k1,11:19636225,7233537:7916093
)
]
}2
Postamble:
Count:30
"
  "A SyncTeX file as xelatex writes it for a page of 400 by 500 points with margins of 100: a rule and a line
from lines 5 and 6 of a.tex, a paragraph of lines 7 and 8 on page 1, line 10 on page 2.")

(defparameter *synctex-sample-boxes*
  (vector (list '(0 0 398.51 498.13) '(0 0 398.51 498.13) 0) (list '(0 0 398.51 498.13) '(0 0 398.51 498.13) 0))
  "The page boxes of the PDF *SYNCTEX-SAMPLE* describes.")

(defun synctex-of (text &optional (boxes *synctex-sample-boxes*))
  "TEXT, a SyncTeX file, read with BOXES as its PDF's page boxes."
  (tuatara.defaults::read-synctex (tuatara.text:string-octets text) :boxes boxes))

(define-test (pdf-sync read-as-data)
  (let ((synctex (synctex-of *synctex-sample*)))
    (true synctex)
    ;; line 7 of a.tex: page 1, its paragraph line's box, from where the line's text begins
    (multiple-value-bind (page left top right bottom) (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 7)
      (is = 1 page)
      (true (< (abs (- left (/ (/ 6578177 65781.76d0) 398.51))) 0.001))
      (true (< (abs (- top (/ (/ (- 8019969 462029) 65781.76d0) 498.13))) 0.001))
      (true (< (abs (- bottom (/ (/ (+ 8019969 135003) 65781.76d0) 498.13))) 0.001))
      (true (< left right))
      ;; and back: the middle of that place is line 7, the start of the line before it line 5
      (is equal '("/tmp/sample/a.tex" 7)
          (multiple-value-list (tuatara.defaults::synctex-line synctex 1 (/ (+ left right) 2) (/ (+ top bottom) 2))))
      (is equal '("/tmp/sample/a.tex" 5)
          (multiple-value-list (tuatara.defaults::synctex-line synctex 1 (+ left 0.04) (- top 0.02)))))
    ;; page 2, line 10; a line on no page goes to the nearest after it, then before it
    (is = 2 (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 10))
    (is = 2 (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 11))
    (is = 1 (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 1))
    (is = 2 (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 400))
    ;; a file it does not name, a page it does not have
    (false (tuatara.defaults::synctex-place synctex "/tmp/sample/b.tex" 7))
    (false (tuatara.defaults::synctex-line synctex 9 0.5 0.5))
    ;; a page turned a quarter: the place turned with it
    (let ((turned (synctex-of *synctex-sample* (vector (list '(0 0 398.51 498.13) '(0 0 398.51 498.13) 90)))))
      (multiple-value-bind (page left top) (tuatara.defaults::synctex-place turned "/tmp/sample/a.tex" 7)
        (is = 1 page)
        (true (> left 0.5))
        (true (< (abs (- top (/ (/ 6578177 65781.76d0) 398.51))) 0.001))))
    ;; damaged or hostile lines are passed over and the rest read, nothing signalled: numbers past 32
    ;; bits, lines of no shape, octets that are no text, boxes nested deeper than followed
    (let* ((noise (format nil "~a~%(1,7:99999999999999,1:1,1,1~%g1,7:12,~%k~%Input:99999999999999:/x~%Input:3~%~a~%~a~%"
                          "{3" (make-string 20000 :initial-element #\() (make-string 3000 :initial-element #\))))
           (text (concatenate 'string (subseq *synctex-sample* 0 (search "Postamble:" *synctex-sample*))
                              noise
                              (with-output-to-string (out) (loop repeat 2000 do (format out "(1,7:1,1:1,1,1~%")))
                              (format nil "}3~%Postamble:~%")))
           (octets (concatenate '(vector (unsigned-byte 8)) (tuatara.text:string-octets text) #(0 255 254 10 7 13 10)))
           (synctex (let ((tuatara.defaults::*synctex-most-depth* 50))
                      (tuatara.defaults::read-synctex (coerce octets '(simple-array (unsigned-byte 8) (*)))
                                                      :boxes *synctex-sample-boxes*))))
      (true synctex)
      (is = 7 (nth-value 1 (tuatara.defaults::synctex-line synctex 1 0.5 0.25)))
      (true (find 3 (tuatara.defaults::synctex-pages synctex) :key #'first)))
    ;; records held to their limit
    (let ((tuatara.defaults::*synctex-most-records* 5))
      (is = 5 (tuatara.defaults::synctex-count (synctex-of *synctex-sample*))))
    ;; not a SyncTeX file at all
    (false (synctex-of "%PDF-1.4 not synctex"))))

(define-test (pdf-sync page-boxes)
  ;; each page's media box, crop box and turn, from the page or the tree above it
  (let ((boxes (tuatara.defaults::pdf-page-boxes (coerce (pdf-octets '("one" "two" "three"))
                                                         '(simple-array (unsigned-byte 8) (*))))))
    (is = 3 (length boxes))
    (is equal '((0 0 300 200) (0 0 300 200) 0) (svref boxes 2)))
  (let ((boxes (tuatara.defaults::pdf-page-boxes
                (coerce (pdf-of-objects
                         (list "<< /Type /Catalog /Pages 2 0 R >>"
                               ;; inherited, and a page tree that names itself: read once, no loop
                               "<< /Type /Pages /Kids [3 0 R 2 0 R 4 0 R] /MediaBox [0 0 612 792] /Rotate 90 /Count 2 >>"
                               "<< /Type /Page /Parent 2 0 R /CropBox [10 20 600 780] >>"
                               "<< /Type /Page /Parent 2 0 R /Rotate 180 /CropBox [-5 -5 1000 1000] >>"))
                        '(simple-array (unsigned-byte 8) (*))))))
    (is = 2 (length boxes))
    (is equal '((0 0 612 792) (10 20 600 780) 90) (svref boxes 0))
    ;; a crop box past the media box: within it
    (is equal '((0 0 612 792) (0 0 612 792) 180) (svref boxes 1)))
  ;; no page tree
  (false (tuatara.defaults::pdf-page-boxes (tuatara.text:string-octets "%PDF-1.4 nothing"))))

(defun sync-ready-p ()
  "True when a real xelatex and the sandbox are here, for the tests that make PDFs."
  (let ((tuatara-user:*latex-engines* '("xelatex")))
    (and (tuatara.export::latex-engines-found) (eq :ready (tuatara.io:sandbox-status)))))

(defparameter *sync-tex*
  "\\documentclass{article}
\\pagestyle{empty}
\\begin{document}
First page words.
\\newpage
Second page words here.
\\newpage
Third page words at last.
\\end{document}
"
  "A LaTeX file of three pages, a line of words on each, at lines 4, 6 and 8.")

(defun goto-file-line (name line)
  "A step: point to LINE of the buffer called NAME, in the selected window."
  (lambda ()
    (let ((buffer (tuatara.buffer:find-buffer name)))
      (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-goto-line buffer (1- line))))))

(defun sync-view ()
  "The PDF view of the session."
  (tuatara.defaults::pdf-view-of (find-if #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*)))

(define-test (pdf-sync line-shown-and-back)
  (if (not (sync-ready-p))
      (skip "no xelatex or no sandbox here" (true nil))
      (with-test-directory (dir)
        (let ((tex (merge-pathnames "doc.tex" dir))
              (tuatara-user:*latex-engines* '("xelatex"))
              (marked nil) (page nil) (spot nil))
          (alexandria:write-string-into-file *sync-tex* tex)
          (pdf-forget)
          ;; C-c C-v on line 6: the view beside, turned to page 2, the place marked on its picture
          (multiple-value-bind (f editor)
              (export-session tex (list (export-command "export-latex-compile-pdf-buffer")
                                        (goto-file-line "doc.tex" 6)
                                        "C-c C-v"
                                        (lambda ()
                                          (let ((view (sync-view)))
                                            (setf page (tuatara.defaults::pdf-view-page view)
                                                  marked (tuatara.defaults::pdf-view-marked view)
                                                  spot (tuatara.defaults::pdf-view-spot view)))))
                              :width 200 :cell-pixels '(10 20))
            (true (tuatara.frontend.fake:fake-frame-showing f "doc.tex line 6: page 2"))
            (is = 2 page)
            (true marked)
            (is = 2 (first spot))
            (true (< 0 (third spot) 0.5))
            ;; the picture shown with the mark differs from the page drawn
            (true (tuatara.defaults::pdf-mark-image marked))
            (false (equalp (tuatara.frontend:image-pixels (tuatara.defaults::pdf-mark-image marked))
                           (tuatara.frontend:image-pixels (tuatara.defaults::pdf-mark-source marked))))
            (is string= "doc.tex" (current-name editor)))
          ;; in the view, C-c C-v: the line of the place marked, doc.tex in the other window
          (pdf-forget)
          (multiple-value-bind (f editor)
              (export-session tex (list (export-command "export-latex-compile-pdf-buffer")
                                        (goto-file-line "doc.tex" 8)
                                        "C-c C-v" "C-x o" (goto-file-line "doc.tex" 1) "C-c C-v")
                              :width 200 :cell-pixels '(10 20))
            (true (tuatara.frontend.fake:fake-frame-showing f "Page 3: doc.tex line 8"))
            (is string= "doc.tex" (current-name editor))
            (is = 8 (1+ (tuatara.buffer:buffer-line-number (tuatara.buffer:find-buffer "doc.tex") (session-point editor)))))
          ;; a double click on the page's line of words: that line
          (pdf-forget)
          (let ((cell nil))
            (multiple-value-bind (f editor)
                (export-session tex (list (export-command "export-latex-compile-pdf-buffer")
                                          (goto-file-line "doc.tex" 4)
                                          "C-c C-v"
                                          (lambda ()
                                            ;; the cell over the middle of the place marked on page 1
                                            (let* ((buffer (find-if #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*))
                                                   (view (tuatara.defaults::pdf-view-of buffer))
                                                   (window (tuatara.command:buffer-window buffer))
                                                   (image (tuatara.buffer:buffer-local buffer :image))
                                                   (box (tuatara.defaults::pdf-mark-box (tuatara.defaults::pdf-view-marked view))))
                                              (setf cell (list (+ (tuatara.display:area-left window)
                                                                  (floor (* (/ (+ (first box) (third box)) 2)
                                                                            (tuatara.frontend:image-width image))
                                                                         10)
                                                                  (- (tuatara.defaults::pdf-view-left view)))
                                                               (+ (tuatara.display:window-text-top window)
                                                                  (floor (* (/ (+ (second box) (fourth box)) 2)
                                                                            (tuatara.frontend:image-height image))
                                                                         20)
                                                                  (- (tuatara.defaults::pdf-view-top view)))))
                                              (setf (tuatara.buffer:buffer-point (tuatara.buffer:find-buffer "doc.tex")) 0)
                                              (dolist (base '(:down-mouse-1 :double-mouse-1 :up-mouse-1))
                                                (tuatara.frontend.fake:fake-queue-event
                                                 (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                 (tuatara.frontend:make-event
                                                    :key :key (tuatara.frontend:make-key base :column (first cell)
                                                                                              :row (second cell))))))))
                                :width 200 :cell-pixels '(10 20))
              (declare (ignore f))
              (true cell)
              (is string= "doc.tex" (current-name editor))
              (is = 4 (1+ (tuatara.buffer:buffer-line-number (tuatara.buffer:find-buffer "doc.tex")
                                                             (session-point editor))))))
          ;; where pictures are not shown: the PDF's text, turned to the page
          (pdf-forget)
          (let ((page nil))
            (let ((f (export-session tex (list (export-command "export-latex-compile-pdf-buffer")
                                               (goto-file-line "doc.tex" 8)
                                               "C-c C-v"
                                               (lambda ()
                                                 (let ((buffer (find-if #'tuatara.defaults::pdf-view-of
                                                                        tuatara.defaults::*pdf-views*)))
                                                   (setf page (tuatara.defaults::pdf-view-page-now buffer)))))
                                     :width 200)))
              (true (tuatara.frontend.fake:fake-frame-showing f "page 3 (the page alone"))
              (is = 3 page)))
          (pdf-forget)))))

(define-test (pdf-sync org-line-shown-and-back)
  (if (not (sync-ready-p))
      (skip "no xelatex or no sandbox here" (true nil))
      (with-test-directory (dir)
        (let ((org (merge-pathnames "notes.org" dir))
              (tuatara-user:*latex-engines* '("xelatex"))
              (page nil))
          (alexandria:write-string-into-file
           (format nil "#+title: Notes~%* One~%First words.~%* Two~%#+latex: \\newpage~%Second words here.~%") org)
          (pdf-forget)
          (multiple-value-bind (f editor)
              (export-session org (list (export-command "org-export-latex-pdf-buffer")
                                        (goto-file-line "notes.org" 6)
                                        (export-command "show-line-in-pdf")
                                        (lambda () (setf page (tuatara.defaults::pdf-view-page (sync-view))))
                                        "C-x o" "C-c C-v")
                              :width 200 :cell-pixels '(10 20))
            (true (tuatara.frontend.fake:fake-frame-showing f "notes.org line 6: page 2"))
            (is = 2 page)
            ;; back from the PDF: the Org file's line, not the LaTeX's
            (is string= "notes.org" (current-name editor))
            (is = 6 (1+ (tuatara.buffer:buffer-line-number (tuatara.buffer:find-buffer "notes.org")
                                                           (session-point editor)))))
          (pdf-forget)))))

(define-test (pdf-sync made-again-then-shown)
  ;; no PDF yet, or one older than the file or its changes: saved, made again, then the line shown in it
  (if (not (sync-ready-p))
      (skip "no xelatex or no sandbox here" (true nil))
      (with-test-directory (dir)
        (let ((tex (merge-pathnames "doc.tex" dir))
              (pdf (merge-pathnames "doc.pdf" dir))
              (tuatara-user:*latex-engines* '("xelatex"))
              (made nil))
          (alexandria:write-string-into-file *sync-tex* tex)
          (pdf-forget)
          (let ((f (export-session tex (list (goto-file-line "doc.tex" 8) "C-c C-v"
                                             (lambda ()
                                               (setf made (tuatara.io:stamp-mtime (tuatara.io:path-stamp pdf))))
                                             ;; a change not saved: a word typed on line 8
                                             (lambda ()
                                               (let ((buffer (tuatara.buffer:find-buffer "doc.tex")))
                                                 (tuatara.buffer:buffer-insert
                                                  buffer (tuatara.buffer:buffer-goto-line buffer 7) "More ")))
                                             (lambda () (sleep 1.1))
                                             (goto-file-line "doc.tex" 6) "C-c C-v")
                                   :width 200)))
            (true made)
            (true (tuatara.frontend.fake:fake-frame-showing f "doc.tex line 8: page 3"))
            (true (tuatara.frontend.fake:fake-frame-showing f "doc.tex line 6: page 2"))
            (false (tuatara.buffer:buffer-modified-p (tuatara.buffer:find-buffer "doc.tex")))
            (true (search "More Third" (alexandria:read-file-into-string tex)))
            (true (> (tuatara.io:stamp-mtime (tuatara.io:path-stamp pdf)) made)))
          (pdf-forget)))))

(define-test (pdf-sync older-or-without-said)
  (with-test-directory (dir)
    (let ((tex (merge-pathnames "doc.tex" dir))
          (pdf (merge-pathnames "doc.pdf" dir)))
      (alexandria:write-string-into-file *sync-tex* tex)
      ;; a PDF made elsewhere, with no SyncTeX file among the working files: said, the view not shown
      (alexandria:write-byte-vector-into-file (pdf-octets '("one")) pdf)
      (sb-posix:utimes (namestring tex) 1000 1000)
      (pdf-forget)
      (let ((f (export-session tex "C-c C-v" :width 200)))
        (true (tuatara.frontend.fake:fake-frame-showing f "doc.pdf has no SyncTeX file"))
        (false tuatara.defaults::*pdf-views*))
      ;; in an Org file not exported in this session: said
      (let ((org (merge-pathnames "notes.org" dir)))
        (alexandria:write-string-into-file (format nil "* One~%Text~%") org)
        (let ((f (export-session org (export-command "show-line-in-pdf") :width 200)))
          (true (tuatara.frontend.fake:fake-frame-showing f "notes.org was not made into a PDF in this session"))))
      (pdf-forget))))

(defun synctex-text (pages lines &key (nested 0) (pairs 0))
  "A SyncTeX file of PAGES pages of LINES lines of a.tex each, a line's box
with a glue in it for each, line numbers going on from page to page; with
NESTED boxes of lines opened on the first page and PAIRS of them closed and
opened again after."
  (with-output-to-string (out)
    (format out "SyncTeX Version:1~%Input:1:/tmp/sample/a.tex~%Content:~%")
    (loop for page from 1 to pages
          do (format out "{~d~%" page)
             (when (= page 1)
               (loop repeat nested do (format out "[1,1:0,0:10,10,0~%"))
               (loop repeat pairs do (format out "]~%[1,1:0,0:10,10,0~%")))
             (loop for line from 1 to lines
                   for number = (+ line (* lines (1- page)))
                   for y = (* line 1000000)
                   do (format out "(1,~d:6578177,~d:13058048,600000,100000~%g1,~d:9000000,~d~%)~%" number y number y))
             (format out "}~d~%" page))))

(defun run-seconds (function)
  "The processor time FUNCTION takes, in seconds."
  (let ((start (get-internal-run-time)))
    (funcall function)
    (/ (- (get-internal-run-time) start) internal-time-units-per-second)))

(define-test (pdf-sync crop-box-drawn-as-measured)
  ;; a page drawn is the part a PDF program shows, its crop box, as its size and the SyncTeX places are measured
  (with-test-directory (dir)
    (let ((file (merge-pathnames "cropped.pdf" dir)))
      (alexandria:write-byte-vector-into-file
       (pdf-of-objects (list "<< /Type /Catalog /Pages 2 0 R >>"
                             "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                             "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 800] /CropBox [0 0 300 200] >>"))
       file)
      (pdf-forget)
      (let* ((f (pdf-session file nil nil :cell-pixels '(10 20)))
             (put (first (last-images f))))
        (true put)
        (true (< (abs (- 3/2 (/ (tuatara.frontend:image-width (first put))
                                (tuatara.frontend:image-height (first put)))))
                 1/50)))
      (pdf-forget))))

(define-test (pdf-sync nesting-read-in-linear-time)
  ;; boxes nested as deep as followed, closed and opened again many times: each time costs the same, however deep
  (flet ((seconds (nested)
           (let ((octets (tuatara.text:string-octets (synctex-text 1 1 :nested nested :pairs 200000))))
             (run-seconds (lambda () (tuatara.defaults::read-synctex octets))))))
    (seconds 1)
    (let ((shallow (seconds 1)) (deep (seconds 999)))
      (true (< deep (+ 0.2 (* 3 shallow))) "~,3f s deep, ~,3f s shallow" deep shallow))))

(define-test (pdf-sync read-in-bounded-memory)
  ;; the records kept in one array made once, at most a record a line: no growing, no copy at the end
  (let* ((octets (tuatara.text:string-octets (synctex-text 100 1000)))
         (lines (1+ (count 10 octets))))
    (tuatara.defaults::read-synctex octets)
    (let* ((before (sb-ext:get-bytes-consed))
           (synctex (tuatara.defaults::read-synctex octets))
           (consed (- (sb-ext:get-bytes-consed) before))
           (kept (+ (* 4 (length (tuatara.defaults::synctex-records synctex)))
                    (* 4 (length (tuatara.defaults::synctex-index synctex))))))
      (is = (* 8 lines) (length (tuatara.defaults::synctex-records synctex)))
      (true (< consed (* 3/2 kept)) "~:d octets consed for ~:d kept" consed kept)))
  (true (<= tuatara.defaults::*synctex-largest* (* 256 1024 1024)))
  (true (<= tuatara.defaults::*synctex-most-records* 4000000)))

(define-test (pdf-sync found-by-search)
  ;; a line looked for is found in the index by halves, not by going through every record
  (let ((synctex (synctex-of (synctex-text 200 1000))))
    (is = 101 (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 100500))
    (is = 200 (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 999999))
    (let ((seconds (run-seconds (lambda ()
                                  (loop repeat 1000
                                        do (tuatara.defaults::synctex-place synctex "/tmp/sample/a.tex" 999999))))))
      (true (< seconds 0.3) "~,3f s for 1000 lookups" seconds))))

(define-test (pdf-sync page-tree-each-node-once)
  ;; a page tree naming one large page many times: the page read once, the names after it passed over unread
  (flet ((seconds (keys)
           (let ((octets (coerce (pdf-of-objects
                                  (list "<< /Type /Catalog /Pages 2 0 R >>"
                                        (format nil "<< /Type /Pages /Kids [~{~a~^ ~}] /Count 1 >>"
                                                (make-list 20000 :initial-element "3 0 R"))
                                        (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 200] ~{/K~d 1 ~}>>"
                                                (loop for i below keys collect i))))
                                 '(simple-array (unsigned-byte 8) (*)))))
             (let ((boxes nil))
               (values (run-seconds (lambda () (setf boxes (tuatara.defaults::pdf-page-boxes octets)))) boxes)))))
    (multiple-value-bind (small small-boxes) (seconds 1)
      (multiple-value-bind (large large-boxes) (seconds 5000)
        (is = 1 (length small-boxes))
        (is = 1 (length large-boxes))
        (true (< large (+ 0.3 (* 3 small))) "~,3f s large, ~,3f s small" large small)))))

(define-test (pdf-sync only-its-own-files-opened)
  (with-test-directory (dir)
    (let* ((top (namestring dir))
           (root (concatenate 'string top "doc/"))
           (tex (concatenate 'string root "doc.tex"))
           (build (concatenate 'string root "build/"))
           (synctex (concatenate 'string build "doc.synctex"))
           (org (concatenate 'string top "elsewhere/notes.org")))
      (ensure-directories-exist build)
      (ensure-directories-exist (concatenate 'string top "elsewhere/"))
      (ensure-directories-exist (concatenate 'string root "sub/"))
      (dolist (file (list tex (concatenate 'string root "sub/ch.tex") org))
        (alexandria:write-string-into-file "x" file))
      (sb-posix:symlink "/etc/passwd" (concatenate 'string root "link.tex"))
      (flet ((shown (file &optional source) (tuatara.export::source-to-show file synctex tex source)))
        ;; within the LaTeX file's directory, a full name or one taken from the build directory
        (is string= (concatenate 'string root "sub/ch.tex") (shown (concatenate 'string root "sub/ch.tex")))
        (true (shown "../sub/ch.tex"))
        ;; anything else a document read: named, not opened
        (false (shown "/etc/passwd"))
        (false (shown (concatenate 'string root "link.tex")))
        (false (shown (concatenate 'string root "sub")))
        (false (shown "ch.tex"))
        ;; the Org file it was exported from, wherever it is
        (false (shown org))
        (true (shown org org))))))

(define-test (pdf-sync synctex-of-the-copy-by-real-path)
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (real (concatenate 'string root "real/"))
           (link (concatenate 'string root "link"))
           (tex (concatenate 'string real "doc.tex")))
      (ensure-directories-exist real)
      (sb-posix:symlink real link)
      (alexandria:write-string-into-file "x" tex)
      ;; the working files of one file, by whichever name it is reached
      (is string= (tuatara.export::latex-build-place tex)
          (tuatara.export::latex-build-place (concatenate 'string link "/doc.tex")))
      (let* ((directory (concatenate 'string (tuatara.export::latex-build-place tex) "xelatex/"))
             (made (concatenate 'string directory "doc.pdf")))
        (ensure-directories-exist directory)
        (alexandria:write-string-into-file "same size" made)
        (alexandria:write-string-into-file "SyncTeX Version:1" (concatenate 'string directory "doc.synctex"))
        (alexandria:write-string-into-file "same size" (concatenate 'string real "doc.pdf"))
        (sb-posix:utimes made 1000000 1000000)
        (true (tuatara.export::latex-synctex-file (concatenate 'string link "/doc.tex")))
        ;; a PDF beside the file older than the one made, though as long: not its copy
        (sb-posix:utimes made 2000000000 2000000000)
        (false (tuatara.export::latex-synctex-file tex))
        (tuatara.io:delete-tree (tuatara.export::latex-build-place tex))))))

(define-test (pdf-sync org-exports-kept-lean)
  (with-test-directory (dir)
    (let* ((org (namestring (merge-pathnames "notes.org" dir)))
           (pdf (namestring (merge-pathnames "notes.pdf" dir)))
           (job (tuatara.export::make-pdf-export nil org :org nil nil "the text as asked" nil))
           (buffer (tuatara.buffer:make-buffer "notes.org" :text (format nil "a~%b~%c~%") :file org)))
      (setf (tuatara.export::pdf-export-status job) :done (tuatara.export::pdf-export-pdf job) pdf
            (tuatara.export::pdf-export-tex job) "notes.tex" (tuatara.export::pdf-export-text job) (format nil "b~%c")
            (tuatara.export::pdf-export-origins job) #(2 3) (tuatara.export::pdf-export-map job) #())
      (tuatara.export::install-pdf-sync)
      (unwind-protect
           (progn
             (tuatara.export::note-pdf-export job)
             (let ((kept (gethash pdf tuatara.export::*org-pdf-exports*)))
               (true kept)
               (false (eq kept job))
               (false (tuatara.export::pdf-export-rope kept))
               ;; a part exported alone: current while its lines are as they were where they came from
               (true (tuatara.export::org-export-current-p kept buffer))
               (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-goto-line buffer 2) "changed ")
               (false (tuatara.export::org-export-current-p kept buffer)))
             ;; the PDF made again from its LaTeX: forgotten
             (let ((latex (tuatara.export::make-pdf-export nil "notes.tex" :latex nil nil nil nil)))
               (setf (tuatara.export::pdf-export-status latex) :done (tuatara.export::pdf-export-pdf latex) pdf)
               (tuatara.export::note-pdf-export latex)
               (false (gethash pdf tuatara.export::*org-pdf-exports*)))
             ;; the Org file's buffer killed: forgotten
             (tuatara.export::note-pdf-export job)
             (tuatara.buffer:kill-buffer buffer)
             (false (gethash pdf tuatara.export::*org-pdf-exports*)))
        (remhash pdf tuatara.export::*org-pdf-exports*)
        (when (tuatara.buffer:live-buffer-p buffer) (tuatara.buffer:kill-buffer buffer))))))

(define-test (pdf-sync one-read-at-a-time)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "report.pdf" dir))
          (synctex (namestring (merge-pathnames "report.synctex" dir)))
          (started nil) (again nil) (said nil))
      (alexandria:write-byte-vector-into-file (pdf-octets '("one")) file)
      (alexandria:write-string-into-file "no SyncTeX here" synctex)
      (pdf-forget)
      (flet ((tasks () (count-if #'tuatara.defaults::pdf-view-task-p tuatara.runtime:*jobs*))
             (view () (find-if #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*)))
        (pdf-session file nil
                     (list (lambda ()
                             ;; asked twice while it is read: read once, the last asking answered
                             (tuatara.export::pdf-sync-data (view) synctex (lambda (read why) (declare (ignore read why))))
                             (tuatara.export::pdf-sync-data (view) synctex
                                                            (lambda (read why) (declare (ignore read)) (setf said why)))
                             (setf started (tasks)))
                           (after-pdf-views
                            (lambda ()
                              ;; a file that could not be read: remembered until it changes, not read again
                              (setf said nil)
                              (tuatara.export::pdf-sync-data (view) synctex
                                                             (lambda (read why) (declare (ignore read)) (setf said why)))
                              (setf again (tasks)))))
                     :cell-pixels '(10 20)))
      (is = 1 started)
      (is = 0 again)
      (is string= "it is no SyncTeX file" said)
      (pdf-forget))))

(define-test (pdf-sync stale-read-dropped)
  (with-test-directory (dir)
    (let* ((synctex (namestring (merge-pathnames "doc.synctex" dir)))
           (pdf (namestring (merge-pathnames "doc.pdf" dir)))
           (called nil))
      (alexandria:write-string-into-file "SyncTeX Version:1" synctex)
      (alexandria:write-string-into-file "old" pdf)
      (let ((stamp (tuatara.io:path-stamp synctex)) (pdf-stamp (tuatara.io:path-stamp pdf)))
        ;; the PDF made again while its SyncTeX file was read: what was read is not used
        (alexandria:write-string-into-file "made again" pdf :if-exists :supersede)
        (let ((f (run-keys (list (lambda ()
                                   (tuatara.export::pdf-sync-take-in synctex stamp pdf pdf-stamp
                                                                     (synctex-of *synctex-sample*)
                                                                     (lambda (read why) (declare (ignore why))
                                                                       (setf called read))))))))
          (false called)
          (true (tuatara.frontend.fake:fake-frame-showing f "doc.pdf changed"))
          (false (find synctex tuatara.export::*pdf-sync-read* :key #'first :test #'equal)))))))

(define-test (pdf-sync scrolled-by-the-page-drawn-for-the-window)
  ;; a place shown right after the zoom changed: scrolled to by the page drawn for the new zoom, not the old one
  (with-test-directory (dir)
    (let ((file (merge-pathnames "report.pdf" dir))
          (top nil) (rows nil) (height nil))
      (alexandria:write-byte-vector-into-file (pdf-octets '("one" "two")) file)
      (pdf-forget)
      (pdf-session file nil
                   (list (lambda ()
                           (let ((buffer (find-if #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*)))
                             (tuatara.defaults::pdf-view-fit-width)
                             (tuatara.defaults::pdf-view-show-place buffer 1 0.1 0.9 0.9 0.95)))
                         (after-pdf-views
                          (lambda ()
                            (let* ((buffer (find-if #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*))
                                   (view (tuatara.defaults::pdf-view-of buffer)))
                              (setf top (tuatara.defaults::pdf-view-top view)
                                    rows (nth-value 1 (tuatara.defaults::pdf-view-image-cells
                                                       (tuatara.buffer:buffer-local buffer :image)))
                                    height (tuatara.display:window-height (tuatara.command:buffer-window buffer)))))))
                   :cell-pixels '(10 20))
      (true (numberp top))
      (true (and rows (> rows height)) "the page fitted to the width is taller than the window")
      (true (and (numberp top) (<= top (floor (* 0.9 rows)) (+ top height))) "top ~a, ~a rows, window ~a" top rows height)
      (pdf-forget))))

(define-test (pdf-sync marked-without-garbage-for-each-pixel)
  (let* ((image (tuatara.frontend:make-image 500 500 3 (make-array (* 3 500 500) :element-type '(unsigned-byte 8)
                                                                                    :initial-element 255)))
         (octets (* 3 500 500)))
    ;; a theme giving the mark a colour, so that it is laid over the page
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'tuatara.defaults::ao-dark)
           (tuatara.defaults::marked-image image '(0 0 1 1))
           (let* ((before (sb-ext:get-bytes-consed))
                  (marked (tuatara.defaults::marked-image image '(0 0 1 1)))
                  (consed (- (sb-ext:get-bytes-consed) before)))
             (true (/= 255 (aref (tuatara.frontend:image-pixels marked) 2)))
             (true (< consed (* 2 octets)) "~:d octets consed for a picture of ~:d" consed octets)))
      (tuatara.display:apply-theme nil))))

(define-test (pdf-sync read-forgotten-when-ended-unanswered)
  ;; a read that was stopped or failed leaves no "still reading": asking again reads again
  (with-test-directory (dir)
    (let ((file (merge-pathnames "report.pdf" dir))
          (synctex (namestring (merge-pathnames "report.synctex" dir)))
          (started nil))
      (alexandria:write-byte-vector-into-file (pdf-octets '("one")) file)
      (alexandria:write-string-into-file "no SyncTeX here" synctex)
      (pdf-forget)
      (setf tuatara.export::*pdf-sync-read* '())
      (let ((ended (tuatara.defaults::make-pdf-view-task nil "reading" (constantly nil) t)))
        (setf (tuatara.runtime:job-status ended) :stopped)
        (push (list* synctex ended (constantly nil)) tuatara.export::*pdf-sync-reading*)
        (let ((f (pdf-session file nil
                              (list (lambda ()
                                      (tuatara.export::pdf-sync-data
                                       (find-if #'tuatara.defaults::pdf-view-of tuatara.defaults::*pdf-views*) synctex
                                       (lambda (read why) (declare (ignore read why))))
                                      (setf started (count-if #'tuatara.defaults::pdf-view-task-p
                                                              tuatara.runtime:*jobs*))))
                              :cell-pixels '(10 20))))
          (is = 1 started)
          (false (tuatara.frontend.fake:fake-frame-showing f "Still reading"))))
      (setf tuatara.export::*pdf-sync-reading* '())
      (pdf-forget))))

(define-test (pdf-sync pages-and-kept-reads-bounded)
  ;; pages held to their limit, as records are
  (let ((tuatara.defaults::*synctex-most-pages* 10))
    (is = 10 (length (tuatara.defaults::synctex-pages
                      (synctex-of (concatenate 'string "SyncTeX Version:1" (string #\Newline)
                                               (with-output-to-string (out)
                                                 (loop repeat 1000 do (format out "{1~%}1~%")))))))))
  ;; the reads kept held to a total size, the last always kept
  (with-test-directory (dir)
    (let* ((pdf (namestring (merge-pathnames "doc.pdf" dir)))
           (first (namestring (merge-pathnames "one.synctex" dir)))
           (second (namestring (merge-pathnames "two.synctex" dir)))
           (tuatara.export::*pdf-sync-read* '())
           (tuatara.export::*pdf-sync-kept-octets* 1))
      (dolist (file (list first second pdf)) (alexandria:write-string-into-file "x" file))
      (run-keys (list (lambda ()
                        (dolist (synctex (list first second))
                          (tuatara.export::pdf-sync-take-in synctex (tuatara.io:path-stamp synctex) pdf
                                                            (tuatara.io:path-stamp pdf) (synctex-of *synctex-sample*)
                                                            (constantly nil)))
                        (setf first (mapcar #'first tuatara.export::*pdf-sync-read*)))))
      (is equal (list second) first))))

(define-test (pdf-sync mark-ends-without-the-fitted-page)
  ;; a mark first shown on a page drawn for another width still ends; shown on the right one it begins again
  (let* ((view (tuatara.defaults::make-pdf-view "/tmp/none.pdf"))
         (mark (tuatara.defaults::make-pdf-mark 1 '(0.1 0.1 0.5 0.2)))
         (image (tuatara.frontend:make-image 10 10 3 (make-array 300 :element-type '(unsigned-byte 8)
                                                                   :initial-element 255))))
    (setf (tuatara.defaults::pdf-view-marked view) mark)
    (tuatara.defaults::pdf-view-marked-image view image nil)
    (let ((until (tuatara.defaults::pdf-mark-until mark)))
      (true until)
      (sleep 0.01)
      (tuatara.defaults::pdf-view-marked-image
       view (tuatara.frontend:make-image 10 10 3 (copy-seq (tuatara.frontend:image-pixels image))) t)
      (true (> (tuatara.defaults::pdf-mark-until mark) until)))))

(define-test (pdf-sync source-opened-where-it-really-is)
  ;; the file opened is the one checked: a link within the directory leads to where it really is
  (with-test-directory (dir)
    (let* ((root (concatenate 'string (namestring dir) "doc/"))
           (tex (concatenate 'string root "doc.tex"))
           (chapter (concatenate 'string root "sub/ch.tex"))
           (alias (concatenate 'string root "alias.tex")))
      (ensure-directories-exist (concatenate 'string root "sub/"))
      (dolist (file (list tex chapter)) (alexandria:write-string-into-file "x" file))
      (sb-posix:symlink chapter alias)
      (is string= (tuatara.io:real-path-or-parent chapter)
          (tuatara.export::source-to-show alias (concatenate 'string root "doc.synctex") tex nil)))))

(define-test (pdf-sync crop-box-outside-the-page)
  ;; a crop box wholly outside the media box: the media box, as PDF programs take it
  (let ((boxes (tuatara.defaults::pdf-page-boxes
                (coerce (pdf-of-objects
                         (list "<< /Type /Catalog /Pages 2 0 R >>"
                               "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                               "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /CropBox [700 900 800 1000] >>"))
                        '(simple-array (unsigned-byte 8) (*))))))
    (is equal '((0 0 612 792) (0 0 612 792) 0) (svref boxes 0))))

(define-test (pdf-sync part-of-other-length-changed)
  ;; a part exported alone whose lines are more than its places in the file: not current
  (let* ((kept (tuatara.export::make-pdf-export nil "/tmp/notes.org" :org nil nil nil nil))
         (buffer (tuatara.buffer:make-buffer "part-lengths.org" :text (format nil "a~%b~%c~%"))))
    (setf (tuatara.export::pdf-export-text kept) (format nil "b~%c~%d") (tuatara.export::pdf-export-origins kept) #(2 3))
    (unwind-protect (false (tuatara.export::org-export-current-p kept buffer))
      (tuatara.buffer:kill-buffer buffer))))
