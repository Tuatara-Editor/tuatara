;;;; test/outline.lisp --- Going to a heading or a definition: buffer-outline.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test outline
  :description "buffer-outline (M-g o, SPC ;) lists a buffer's headings in the order of the
text, each as its line after its number: Org's, Lisp's ;;; comments and top-level forms, and by
the syntax tree Markdown's headings and Rust's definitions, nested. It opens on the heading point
is under, shows each in the window as it is selected, goes there on RET leaving the mark behind,
and back on C-g; N SPC and a prefix argument keep only the headings N levels deep or less.")

(defun outline-run (keys name text &key (width 80) (height 20))
  "Run KEYS in a session visiting a new file called NAME holding TEXT; return
the frontend, the buffer's point, its mark and the window's first octet shown,
the buffer killed afterwards."
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames name dir) text)))
      (multiple-value-bind (f editor) (run-keys keys :file (namestring path) :width width :height height)
        (let* ((window (tuatara.command:editor-window editor))
               (buffer (tuatara.display:window-buffer window)))
          (unwind-protect
               (values f (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-mark buffer)
                       (tuatara.display:window-start window))
            (tuatara.buffer:kill-buffer buffer)))))))

(defparameter *outline-org*
  (format nil "#+title: Notes~%* One~%text~%** Two~%more~%*bold* is no heading~%* Three~%")
  "An Org text with three headings, one under another.")

(define-test (outline org-headings)
  (let ((text *outline-org*))
    (flet ((at (string) (search string text)))
      ;; each heading as its line after its number, the first selected
      (let ((f (outline-run "M-g o" "notes.org" text)))
        (true (tuatara.frontend.fake:fake-frame-showing f "1/3" "Go to heading:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "2  * One"))
        (true (tuatara.frontend.fake:fake-frame-showing f "4  ** Two"))
        (true (tuatara.frontend.fake:fake-frame-showing f "7  * Three")))
      ;; RET goes to the one selected, the mark left where point was
      (multiple-value-bind (f point mark) (outline-run "M-g o RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "* One") point)
        (is = 0 mark))
      (multiple-value-bind (f point) (outline-run "M-g o C-n RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "** Two") point))
      ;; typing narrows the list
      (multiple-value-bind (f point) (outline-run "M-g o t h r RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "* Three") point))
      ;; it opens on the heading point is under
      (multiple-value-bind (f point) (outline-run "M-> M-g o RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "* Three") point))
      ;; C-g: back where point was, the heading shown on the way given up
      (multiple-value-bind (f point) (outline-run "M-> M-g o C-p C-g" "notes.org" text)
        (declare (ignore f))
        (is = (length text) point))
      ;; 1 SPC: the outermost only; a prefix argument does the same
      (let ((f (outline-run "M-g o 1 SPC" "notes.org" text)))
        (true (tuatara.frontend.fake:fake-frame-showing f "1/2" "Go to heading: 1 ")))
      (multiple-value-bind (f point) (outline-run "M-g o 1 SPC C-n RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "* Three") point))
      (multiple-value-bind (f point) (outline-run "C-u 1 M-g o C-n RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "* Three") point))
      ;; the leader's key, and its jump back
      (multiple-value-bind (f point) (outline-run "SPC ; C-n RET" "notes.org" text)
        (declare (ignore f))
        (is = (at "** Two") point))
      (multiple-value-bind (f point) (outline-run "SPC ; C-n RET S" "notes.org" text)
        (declare (ignore f))
        (is = 0 point)))))

(define-test (outline shown-as-selected)
  ;; a heading out of sight is shown in the middle of the window as it is
  ;; selected, and stays shown after RET; C-g puts the window back
  (let* ((text (format nil "* Near~%~{~a~%~}* Far~%~{~a~%~}" (loop for i below 60 collect i) (loop for i below 60 collect i)))
         (far (search "* Far" text)))
    (multiple-value-bind (f point mark start) (outline-run "M-g o C-n RET" "long.org" text :height 12)
      (declare (ignore mark))
      (is = far point)
      (true (< 0 start far))
      ;; the window's rows showed the far heading while the list was open
      (true (tuatara.frontend.fake:fake-frame-showing f "2/2" "Go to heading:"))
      (true (loop for screen in (tuatara.frontend.fake::fake-history f)
                  thereis (some (lambda (row) (and (>= (length row) 5) (string= "◉ Far" row :end2 5))) screen))))
    (multiple-value-bind (f point mark start) (outline-run "M-g o C-n C-g" "long.org" text :height 12)
      (declare (ignore f mark))
      (is = 0 point)
      (is = 0 start))))

(define-test (outline lisp-headings)
  (let ((text (format nil ";;;; file.lisp --- A file.~%(in-package :x)~2%;;; Section~%(defun a ()~%  (b))~%;;;; Deeper~%(defvar *c* 1)~%")))
    ;; comments of three semicolons and more, and the top-level forms
    (let ((f (outline-run "M-g o" "file.lisp" text)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1/6"))
      (true (tuatara.frontend.fake:fake-frame-showing f "4  ;;; Section"))
      (true (tuatara.frontend.fake:fake-frame-showing f "5  (defun a ()")))
    ;; 1 SPC the ;;; comments, 2 SPC those of four semicolons too
    (true (tuatara.frontend.fake:fake-frame-showing (outline-run "M-g o 1 SPC" "file.lisp" text) "1/1"))
    (true (tuatara.frontend.fake:fake-frame-showing (outline-run "M-g o 2 SPC" "file.lisp" text) "1/3"))
    (multiple-value-bind (f point) (outline-run "M-g o 1 SPC RET" "file.lisp" text)
      (declare (ignore f))
      (is = (search ";;; Section" text) point))))

(define-test (outline headings-by-the-tree)
  ;; Markdown's headings, the second under the first
  (let ((text (format nil "# A~2%intro~2%## B~2%text~2%# C~%")))
    (true (tuatara.frontend.fake:fake-frame-showing (outline-run "M-g o" "doc.md" text) "1/3"))
    (true (tuatara.frontend.fake:fake-frame-showing (outline-run "M-g o 1 SPC" "doc.md" text) "1/2"))
    (multiple-value-bind (f point) (outline-run "M-g o C-n RET" "doc.md" text)
      (declare (ignore f))
      (is = (search "## B" text) point)))
  ;; and those underlined
  (true (tuatara.frontend.fake:fake-frame-showing
         (outline-run "M-g o" "setext.md" (format nil "Title~%=====~2%text~2%Part~%----~%")) "1/2"))
  ;; Rust's definitions, a function of an impl block under it
  (let ((text (format nil "struct S;~2%impl S {~%    fn new() -> S {~%        S~%    }~%}~2%fn main() {}~%")))
    (let ((f (outline-run "M-g o" "main.rs" text)))
      (true (tuatara.frontend.fake:fake-frame-showing f "1/4"))
      (true (tuatara.frontend.fake:fake-frame-showing f "4      fn new() -> S {")))
    (true (tuatara.frontend.fake:fake-frame-showing (outline-run "M-g o 1 SPC" "main.rs" text) "1/3"))
    (multiple-value-bind (f point) (outline-run "M-g o n e w RET" "main.rs" text)
      (declare (ignore f))
      (is = (search "fn new" text) point))))

(define-test (outline line-text)
  ;; a line as the list shows it: tabs as spaces, control characters and raw
  ;; octets written out, blanks at the end gone, colours moved with the text
  (let ((rope (tuatara.text:octets->rope
               (concatenate '(vector (unsigned-byte 8))
                            (tuatara.text:string-octets (format nil "~cx = é;" #\Tab))
                            #(27 255 32 32)))))
    (multiple-value-bind (text spans)
        (tuatara.defaults::outline-line-text rope 0 (tuatara.text:rope-bytes rope) 4
                                             '((0 10 :none) (1 2 :variable) (5 7 :string)))
      (is string= "    x = é;^[\\xFF" text)
      (is equal '((4 5 :variable) (8 9 :string)) spans)))
  ;; a C1 control character written out as the text shows it
  (let ((rope (tuatara.text:string->rope (format nil "a~cb" (code-char #x85)))))
    (is string= "a\\x85b" (tuatara.defaults::outline-line-text rope 0 (tuatara.text:rope-bytes rope) 4 '()))))

(define-test (outline no-headings)
  (multiple-value-bind (f editor) (run-keys "M-g o" :text "hello" :width 80)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No headings"))))
