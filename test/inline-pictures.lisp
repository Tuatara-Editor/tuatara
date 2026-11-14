;;;; test/inline-pictures.lisp --- Pictures shown among the lines of an Org buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test inline-pictures)

(defun picture-file (dir name width height)
  "Make a picture NAME of WIDTH by HEIGHT pixels in DIR; return its path."
  (let ((file (namestring (merge-pathnames name dir))))
    (uiop:run-program (list "magick" "-size" (format nil "~dx~d" width height) "xc:#808080" file))
    file))

(defun picture-session (dir text keys &key more cell-pixels (width 40) (height 16) (name "notes.org"))
  "Visit NAME, notes.org unless given, in DIR holding TEXT, type KEYS, wait for the pictures and
formulas drawn in the background, then type MORE (keys, or a list of keys, mouse keys, each
\(BASE COLUMN ROW), and functions called in their turn); CELL-PIXELS has the
frontend show pictures. Returns the frontend and the editor."
  (let ((file (namestring (merge-pathnames name dir)))
        (f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels))
        (start (get-internal-real-time)))
    (alexandria:write-string-into-file text file :if-exists :supersede)
    (let ((visiting (tuatara.buffer:find-buffer name)))
      (when visiting (tuatara.buffer:kill-buffer visiting)))
    (clrhash tuatara.defaults::*inline-picture-images*)
    (clrhash tuatara.defaults::*inline-picture-sizes*)
    (queue-test-keys f keys)
    (labels ((busy () (some (lambda (job) (or (tuatara.defaults::inline-picture-job-p job)
                                              (tuatara.export::formula-job-p job)))
                            tuatara.runtime:*jobs*))
             (check ()
               (if (and (busy) (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                   (dolist (item (if (listp more) more (list more)))
                     (cond ((functionp item) (tuatara.frontend.fake:fake-queue-action f item))
                           ((consp item)
                            (destructuring-bind (base column row) item
                              (tuatara.frontend.fake:fake-queue-event
                               f (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key base :column column
                                                                                                        :row row)))))
                           (t (queue-test-keys f item)))))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (without-grammar-questions
                (tuatara.command:run-editor f :files (list file) :setup #'tuatara.defaults:install-defaults)))))

(defun screen-lines (f)
  "The rows of F's last frame, trailing blanks off."
  (loop for row across (tuatara.frontend.fake:fake-screen f) collect (string-right-trim " " row)))

(defmacro with-plain-org (&body body)
  "Run BODY with Org buffers shown as written: not indented, no signs."
  `(let ((tuatara-user:*org-startup-indented* nil)
         (tuatara-user:*org-link-descriptive* nil)
         (tuatara-user:*org-bullets* nil)
         (tuatara-user:*org-list-bullets* nil)
         (tuatara-user:*org-checkbox-signs* nil)
         (tuatara-user:*inline-pictures-modes* '())
         (tuatara-user:*global-inline-pictures-mode* nil))
     ,@body))

(defparameter *picture-note* (format nil "Above~%[[file:pic.png]]~%Below~%")
  "An Org note with a picture 200 by 100 pixels alone on its second line.")

(define-test (inline-pictures off-as-standard)
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir *picture-note* "" :cell-pixels '(10 20))))
        (is equal '("Above" "[[file:pic.png]]" "Below") (subseq (screen-lines f) 0 3))
        (false (tuatara.frontend.fake:fake-images f))))))

(define-test (inline-pictures shown-in-a-terminal)
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      ;; C-x T p: the line held five rows (100 pixels in rows of 20), the picture over them, 20 columns wide
      (multiple-value-bind (f editor) (picture-session dir *picture-note* "C-x T p" :cell-pixels '(10 20))
        (let ((lines (screen-lines f)))
          (is string= "Above" (first lines))
          (true (every (lambda (line) (string= line "")) (subseq lines 1 6)))
          (is string= "Below" (nth 6 lines)))
        (let ((images (tuatara.frontend.fake:fake-images f)))
          (is = 1 (length images))
          (destructuring-bind (image row column columns rows offset) (first images)
            (is = 200 (tuatara.frontend:image-width image))
            (is equal '(1 0 20 5 nil) (list row column columns rows offset))))
        ;; the text is as it was: the link, not the picture, is saved and searched
        (is string= *picture-note* (session-text editor)))
      ;; the cursor below it stands on its screen row, and a click there finds its line
      (let ((f (picture-session dir *picture-note* "C-x T p" :more "g e" :cell-pixels '(10 20))))
        (is equal '(7 . 0) (tuatara.frontend.fake:fake-cursor f)))
      (multiple-value-bind (f editor)
          (picture-session dir *picture-note* "C-x T p" :more '((:down-mouse-1 2 6)) :cell-pixels '(10 20))
        (declare (ignore f))
        (is = 25 (session-point editor)))
      ;; point on its line: the link shown, to be edited
      (let ((f (picture-session dir *picture-note* "C-x T p" :more "j" :cell-pixels '(10 20))))
        (is equal '("Above" "[[file:pic.png]]" "Below") (subseq (screen-lines f) 0 3))
        (false (tuatara.frontend.fake:fake-images f))))))

(define-test (inline-pictures sized-and-placed)
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (flet ((placed (attributes)
               (let ((f (picture-session dir
                                         (format nil "Above~%~%#+ATTR_ORG: ~a~%[[file:pic.png]]~%~%Below~%" attributes)
                                         "C-x T p" :cell-pixels '(10 20))))
                 (destructuring-bind (&optional image row column columns rows offset)
                     (first (tuatara.frontend.fake:fake-images f))
                   (declare (ignore offset))
                   (and image (list (tuatara.frontend:image-width image) row column columns rows))))))
        ;; in pixels: 100 wide, 50 high, three rows of 20; the picture alone in its paragraph
        (is equal '(100 3 0 10 3) (placed ":width 100"))
        (is equal '(100 3 0 10 3) (placed ":width 100px"))
        ;; a share of the window's text, 40 columns of 10 pixels; larger than it is, made larger
        (is equal '(300 3 0 30 8) (placed ":width 75%"))
        (is equal '(200 3 0 20 5) (placed ":width 0.5"))
        ;; never wider than the window's text
        (is equal '(400 3 0 40 10) (placed ":width 900"))
        ;; centred and to the right, as :align and :center t say
        (is equal '(200 3 10 20 5) (placed ":align center"))
        (is equal '(200 3 10 20 5) (placed ":center t"))
        (is equal '(200 3 20 20 5) (placed ":align right"))
        ;; another export's width read when #+ATTR_ORG gives none
        (is equal '(100 4 0 10 3) (placed ":align left
#+ATTR_HTML: :width 100"))))))

(define-test (inline-pictures shown-in-a-window)
  ;; a window drawing rows large: the line exactly as tall as the picture, the rows under it moved down
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((tuatara.display:*row-scaling* t))
        (let ((f (picture-session dir (format nil "Above~%#+ATTR_ORG: :width 150~%[[file:pic.png]]~%Below~%")
                                  "C-x T p" :cell-pixels '(10 20))))
          (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
            (declare (ignore offset))
            (is equal '(150 2 0 15 15/4) (list (tuatara.frontend:image-width image) row column columns rows)))
          (is equal '((0 40 1 11/4 1)) (aref (tuatara.frontend.fake:fake-row-scales f) 3))
          (is string= "Below" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 3))))))))

(define-test (inline-pictures indented-across-a-stretch)
  ;; an indented link whose line begins in one stretch of the text looked through and the link in the next
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let* ((tuatara.defaults::*inline-picture-block* 8)
             (f (picture-session dir (format nil "Above~%  [[file:pic.png]]~%Below~%") "C-x T p"
                                 :cell-pixels '(10 20))))
        (is = 1 (length (tuatara.frontend.fake:fake-images f)))
        (is string= "Below" (nth 6 (screen-lines f)))))))

(define-test (inline-pictures stand-in-without-pictures)
  ;; a terminal that shows no pictures: a line naming it and its size
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir *picture-note* "C-x T p")))
        (is equal '("Above" "[picture: pic.png, 200×100]" "Below") (subseq (screen-lines f) 0 3)))
      ;; a link to a file that is not there: the link as written
      (let ((f (picture-session dir (format nil "Above~%[[file:gone.png]]~%Below~%") "C-x T p" :cell-pixels '(10 20))))
        (is equal '("Above" "[[file:gone.png]]" "Below") (subseq (screen-lines f) 0 3))))))

(define-test (inline-pictures startup-and-safe-visits)
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      ;; #+STARTUP: inlineimages shows them as the file opens only as *org-startup-shows-pictures* allows;
      ;; noinlineimages does not, whatever the setting
      (let ((f (picture-session dir (format nil "#+STARTUP: inlineimages~%~a" *picture-note*) ""
                                :cell-pixels '(10 20))))
        (false (tuatara.frontend.fake:fake-images f)))
      (let* ((tuatara.defaults::*org-startup-shows-pictures* t)
             (f (picture-session dir (format nil "#+STARTUP: inlineimages~%~a" *picture-note*) ""
                                 :cell-pixels '(10 20))))
        (is = 1 (length (tuatara.frontend.fake:fake-images f))))
      (let ((tuatara-user:*global-inline-pictures-mode* t))
        (let ((f (picture-session dir *picture-note* "" :cell-pixels '(10 20))))
          (is = 1 (length (tuatara.frontend.fake:fake-images f))))
        (let ((f (picture-session dir (format nil "#+STARTUP: noinlineimages~%~a" *picture-note*) ""
                                  :cell-pixels '(10 20))))
          (false (tuatara.frontend.fake:fake-images f))))
      ;; a file opened safely shows none, even asked
      (let ((tuatara-user:*global-inline-pictures-mode* t)
            (file (namestring (merge-pathnames "safe.org" dir))))
        (alexandria:write-string-into-file *picture-note* file)
        (let ((f (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET C-x T p" (rimu-typed file))
                           :width 40 :height 16 :cell-pixels '(10 20))))
          (false (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f "[[file:pic.png]]")))))))

(define-test (inline-pictures width-resized)
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (flet ((resized (text keys)
               (multiple-value-bind (f editor) (picture-session dir text "C-x T p" :more keys :cell-pixels '(10 20))
                 (values (session-text editor) f))))
        ;; a step wider writes the width, the picture shown with point on its line
        (multiple-value-bind (text f) (resized *picture-note* "j C-c C-x w +")
          (is string= (format nil "Above~%#+ATTR_ORG: :width 240~%[[file:pic.png]]~%Below~%") text)
          (true (tuatara.frontend.fake:fake-frame-showing f "Width 240")))
        ;; + and - go on by themselves; 0 takes the width out, and the line left empty
        (is string= (format nil "Above~%#+ATTR_ORG: :width 288~%[[file:pic.png]]~%Below~%")
            (resized *picture-note* "j C-c C-x w + + = -"))
        (is string= *picture-note* (resized *picture-note* "j C-c C-x w + 0"))
        ;; from the #+ATTR_ORG: line, keeping what else is on it; a share stays a share
        (is string= (format nil "#+ATTR_ORG: :align center :width 60%~%[[file:pic.png]]~%")
            (resized (format nil "#+ATTR_ORG: :width 50% :align center~%[[file:pic.png]]~%") "C-c C-x w +"))
        (is string= (format nil "#+ATTR_ORG: :align center~%[[file:pic.png]]~%")
            (resized (format nil "#+ATTR_ORG: :width 50% :align center~%[[file:pic.png]]~%") "C-c C-x w 0"))
        ;; where there is no picture, said
        (multiple-value-bind (text f) (resized *picture-note* "C-c C-x w")
          (is string= *picture-note* text)
          (true (tuatara.frontend.fake:fake-frame-showing f "No picture here")))))))

(define-test (inline-pictures nothing-looked-up-while-off)
  ;; off, typing and moving in an Org buffer with pictures looks for none of them
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((calls 0)
            (names '(tuatara.defaults::org-inline-pictures tuatara.defaults::inline-picture-floats
                     tuatara.defaults::inline-picture-float-before)))
        (dolist (name names)
          (sb-int:encapsulate name 'counting
                              (lambda (function &rest arguments) (incf calls) (apply function arguments))))
        (unwind-protect
             (picture-session dir (format nil "#+ATTR_ORG: :float left~%~a" *picture-note*) "j j a b c DEL DEL k k"
                              :cell-pixels '(10 20))
          (dolist (name names) (sb-int:unencapsulate name 'counting)))
        (is = 0 calls)))))

(defun floated-note (attributes)
  "An Org note whose picture, 200 by 100 pixels, has ATTRIBUTES on its
#+ATTR_ORG: line, five short lines after it."
  (format nil "#+ATTR_ORG: ~a~%[[file:pic.png]]~%one~%two~%three~%four~%five~%" attributes))

(defun scrolled-to (text)
  "A function setting the selected window to begin at the line TEXT in its
buffer, point there, as a test's typing goes on."
  (lambda ()
    (let* ((window (tuatara.command:editor-window tuatara.command:*editor*))
           (buffer (tuatara.display:window-buffer window))
           (at (search (format nil "~%~a~%" text) (tuatara.buffer:buffer-string buffer))))
      (setf (tuatara.buffer:buffer-point buffer) (1+ at)
            (tuatara.display:window-start window) (1+ at)))))

(defun words-line ()
  "A line of 24 words of four letters, aaaa to xxxx."
  (format nil "~{~a~^ ~}" (loop for c from (char-code #\a) to (char-code #\x)
                                collect (make-string 4 :initial-element (code-char c)))))

(defun beside (text &optional (columns 21))
  "TEXT after COLUMNS blank columns, as a row beside a picture floated left shows it."
  (concatenate 'string (make-string columns :initial-element #\Space) text))

(define-test (inline-pictures floated-left-and-right)
  ;; the text goes round the picture: the rows beside it, five of 20 pixels, narrower, from the line after its
  ;; own, which takes no row
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (floated-note ":float left") "C-x T p" :cell-pixels '(10 20))))
        (is equal (list "#+ATTR_ORG: :float left" (beside "one") (beside "two") (beside "three") (beside "four")
                        (beside "five") "")
            (subseq (screen-lines f) 0 7))
        (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
          (is equal '(200 1 0 20 5 (0 0)) (list (tuatara.frontend:image-width image) row column columns rows offset))))
      (let ((f (picture-session dir (floated-note ":float right") "C-x T p" :cell-pixels '(10 20))))
        (is equal '("#+ATTR_ORG: :float right" "one" "two" "three" "four" "five") (subseq (screen-lines f) 0 6))
        (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
          (declare (ignore image))
          (is equal '(1 20 20 5 (0 0)) (list row column columns rows offset))))
      ;; never wider than two thirds of the window's text, 40 columns
      (let ((f (picture-session dir (floated-note ":float left :width 400") "C-x T p" :cell-pixels '(10 20))))
        (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
          (declare (ignore offset))
          (is equal '(267 1 0 27 7) (list (tuatara.frontend:image-width image) row column columns rows)))
        (is string= (beside "one" 28) (tuatara.frontend.fake:fake-screen-line f 1)))
      ;; nor taller than the window, 14 rows of text
      (picture-file dir "tall.png" 100 1000)
      (let ((f (picture-session dir (format nil "#+ATTR_ORG: :float left~%[[file:tall.png]]~%one~%") "C-x T p"
                                :cell-pixels '(10 20))))
        (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
          (declare (ignore offset))
          (is equal '(28 1 0 3 13) (list (tuatara.frontend:image-width image) row column columns rows)))))))

(define-test (inline-pictures floated-as-other-exports-say)
  ;; :float wrap of #+ATTR_LATEX: on the left, or on the right as its :placement says
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (flet ((placed (attributes)
               (let ((f (picture-session dir (format nil "#+ATTR_LATEX: ~a~%[[file:pic.png]]~%one~%" attributes)
                                         "C-x T p" :cell-pixels '(10 20))))
                 (list (third (first (tuatara.frontend.fake:fake-images f)))
                       (tuatara.frontend.fake:fake-screen-line f 1)))))
        (is equal (list 0 (beside "one")) (placed ":float wrap"))
        (is equal '(20 "one") (placed ":float wrap :placement {r}{0.4\\textwidth}"))
        ;; any other float is a picture on rows of its own
        (is equal '(0 "") (placed ":float t"))))))

(defun wrapped-beside (text)
  "TEXT on a row beside a picture floated left where long lines wrap: after the
gutter (a row going on from the one above marked in it) and 21 blank columns."
  (concatenate 'string "↪ " (beside text)))

(define-test (inline-pictures floated-text-wrapped)
  ;; a long line wrapped at a word's edge, the window's text 38 columns after the gutter: its five rows beside
  ;; the picture hold 17 columns, the row below it all 38
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (format nil "#+ATTR_ORG: :float left~%[[file:pic.png]]~%~a~%" (words-line))
                                "C-x T p C-x T l" :cell-pixels '(10 20))))
        (is equal (list (beside "aaaa bbbb cccc" 23) (wrapped-beside "dddd eeee ffff") (wrapped-beside "gggg hhhh iiii")
                        (wrapped-beside "jjjj kkkk llll") (wrapped-beside "mmmm nnnn oooo")
                        "↪ pppp qqqq rrrr ssss tttt uuuu vvvv")
            (subseq (screen-lines f) 1 7))))))

(define-test (inline-pictures floated-from-above-the-window)
  ;; a window beginning beside the picture: its rows still narrower, the picture's lower rows drawn
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (format nil "Top~%~a" (floated-note ":float left")) "C-x T p"
                                :more (list (scrolled-to "two") "C-g") :cell-pixels '(10 20))))
        (is equal (list (beside "two") (beside "three") (beside "four") (beside "five") "")
            (subseq (screen-lines f) 0 5))
        (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
          (declare (ignore image))
          (is equal '(0 0 20 4 (1 0)) (list row column columns rows offset)))))))

(define-test (inline-pictures floated-cursor-and-clicks)
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((text (floated-note ":float left")))
        ;; the cursor beside the picture stands after the blank columns
        (let ((f (picture-session dir text "C-x T p" :more "j j j" :cell-pixels '(10 20))))
          (is equal '(2 . 21) (tuatara.frontend.fake:fake-cursor f)))
        ;; a click beside it finds its character; one over the picture the start of the row's text
        (multiple-value-bind (f editor)
            (picture-session dir text "C-x T p" :more '((:down-mouse-1 23 2)) :cell-pixels '(10 20))
          (declare (ignore f))
          (is = (+ 2 (search "two" text)) (session-point editor)))
        (multiple-value-bind (f editor)
            (picture-session dir text "C-x T p" :more '((:down-mouse-1 5 3)) :cell-pixels '(10 20))
          (declare (ignore f))
          (is = (search "three" text) (session-point editor)))
        ;; point on the picture's line: the link shown, nothing narrower
        (let ((f (picture-session dir text "C-x T p" :more "j" :cell-pixels '(10 20))))
          (is equal '("#+ATTR_ORG: :float left" "[[file:pic.png]]" "one") (subseq (screen-lines f) 0 3))
          (false (tuatara.frontend.fake:fake-images f)))))))

(define-test (inline-pictures floated-screen-lines)
  ;; moving by screen rows and scrolling over the rows beside the picture
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((text (format nil "#+ATTR_ORG: :float left~%[[file:pic.png]]~%~a~%end~%" (words-line))))
        ;; from the first row below the picture up a screen row: the row beside it, at the same column of the
        ;; window's text, or from its first character when that column is beside the picture
        (flet ((up-from (column)
                 (session-point (nth-value 1 (picture-session
                                              dir text "C-x T p C-x T l"
                                              :more (format nil "g e k k k~{ ~a~} k"
                                                            (make-list column :initial-element "l"))
                                              :cell-pixels '(10 20))))))
          (is = (search "mmmm" text) (up-from 3))
          (is = (+ 4 (search "mmmm" text)) (up-from 25)))
        ;; scrolled down three rows, the window beginning beside the picture, and back: the rows as at first
        (flet ((rows (keys) (subseq (screen-lines (picture-session dir text "C-x T p C-x T l" :more keys
                                                                                            :cell-pixels '(10 20)
                                                                                            :height 9))
                                    0 7)))
          (is equal (list (wrapped-beside "gggg hhhh iiii") (wrapped-beside "jjjj kkkk llll")
                          (wrapped-beside "mmmm nnnn oooo") "↪ pppp qqqq rrrr ssss tttt uuuu vvvv")
              (subseq (rows "C-u 3 C-v") 0 4))
          (is equal (rows "") (rows "C-u 3 C-v C-u 3 M-v")))))))

(define-test (inline-pictures picture-line-crossed-by-rows)
  ;; with long lines wrapped, moving a row at a time goes over a picture's line five rows tall, stopping at its
  ;; start, and scrolling a row at a time brings the line to the top
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((text (format nil "Top~%~%[[file:pic.png]]~%~%Below~%")))
        (labels ((session (keys) (picture-session dir text "C-x T p C-x T l" :more keys :cell-pixels '(10 20)))
                 (point-after (keys) (session-point (nth-value 1 (session keys))))
                 (top-after (keys) (string-left-trim " " (first (screen-lines (session keys))))))
          (is = (search "[[" text) (point-after "C-n C-n"))
          (is = (search "Below" text) (point-after "C-n C-n C-n C-n"))
          (is = (search "Below" text) (point-after "j j j j"))
          (is = (search "[[" text) (point-after "M-> C-p C-p C-p"))
          (is = 0 (point-after "M-> C-p C-p C-p C-p C-p"))
          (is = 0 (point-after "M-> k k k k k"))
          (is string= "[[file:pic.png]]" (top-after "C-u 1 C-v C-u 1 C-v")))))))

(define-test (inline-pictures floated-exact-in-a-window)
  ;; a window drawing rows large: the picture 15 columns by 3¾ rows, the fourth row beside it narrower too
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((tuatara.display:*row-scaling* t))
        (let ((f (picture-session dir (floated-note ":float left :width 150") "C-x T p" :cell-pixels '(10 20))))
          (destructuring-bind (image row column columns rows offset) (first (tuatara.frontend.fake:fake-images f))
            (is equal '(150 1 0 15 15/4 (0 0))
                (list (tuatara.frontend:image-width image) row column columns rows offset)))
          (is equal (list (beside "one" 16) (beside "two" 16) (beside "three" 16) (beside "four" 16) "five")
              (subseq (screen-lines f) 1 6)))))))

(define-test (inline-pictures floated-stand-in)
  ;; where pictures cannot be drawn: the line naming it, nothing narrower
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (floated-note ":float left") "C-x T p")))
        (is equal '("#+ATTR_ORG: :float left" "[picture: pic.png, 200×100]" "one" "two")
            (subseq (screen-lines f) 0 4))))))

(define-test (inline-pictures floated-own-line-kept-while-resized)
  ;; point on the picture's line while it is resized: the picture shown, its line keeping its row
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (floated-note ":float left") "C-x T p" :more "j C-c C-x w +"
                                                                             :cell-pixels '(10 20))))
        (is equal (list "#+ATTR_ORG: :float left :width 240" "" (beside "one" 25))
            (subseq (screen-lines f) 0 3))))))

(define-test (inline-pictures block-below-a-float)
  ;; a picture on rows of its own beside a floated one begins below it: its row as much taller, the picture at
  ;; its foot
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (picture-file dir "wide.png" 300 100)
      (let ((f (picture-session dir (format nil "#+ATTR_ORG: :float left~%[[file:pic.png]]~%one~%~%~
                                                 [[file:wide.png]]~%~%two~%")
                                "C-x T p" :cell-pixels '(10 20))))
        (is equal (append (list (beside "one")) (make-list 10 :initial-element "") (list "two"))
            (subseq (screen-lines f) 1 13))
        (is equal '((1 0 20 5 (0 0)) (6 0 30 5 nil))
            (sort (mapcar #'cdr (tuatara.frontend.fake:fake-images f)) #'< :key #'first))))))

(define-test (inline-pictures floated-above-a-fold)
  ;; a window beginning below a fold of many lines, beside a picture floated above it: found, its rows narrower
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (format nil "#+STARTUP: overview~%#+ATTR_ORG: :float left~%[[file:pic.png]]~%* B~%~
                                                 ~{~a~%~}* C~%* D~%" (make-list 3000 :initial-element "body"))
                                "C-x T p" :more (list (scrolled-to "* C") "C-g") :cell-pixels '(10 20))))
        (is equal (list (beside "* C") (beside "* D")) (subseq (screen-lines f) 0 2))
        (is equal '((0 0 20 4 (1 0))) (mapcar #'cdr (tuatara.frontend.fake:fake-images f)))))))

(define-test (inline-pictures floated-scrolled-sideways)
  ;; lines cut off at the edge: point at the end of a long line beside a picture floated left is shown, the
  ;; window scrolled sideways as far as the columns beside the picture need
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "pic.png" 200 100)
      (let ((f (picture-session dir (format nil "#+ATTR_ORG: :float left~%[[file:pic.png]]~%one~%~a~%" (words-line))
                                "C-x T p" :more "j j j C-e" :cell-pixels '(10 20))))
        (destructuring-bind (row . column) (tuatara.frontend.fake:fake-cursor f)
          (is = 2 row)
          (true (< 21 column 40)))
        (let ((line (tuatara.frontend.fake:fake-screen-line f 2)))
          (is string= (beside "") (subseq line 0 21))
          (true (search "xxxx" line)))))))

(define-test (inline-pictures inside-the-fill-column)
  ;; a window of 100 columns, lines filled to 60: a picture as wide as it can be stays inside 60 columns, and one
  ;; aligned right ends at the 60th
  (with-plain-org
    (with-test-directory (dir)
      (picture-file dir "wide.png" 1000 100)
      (picture-file dir "pic.png" 200 100)
      (flet ((placed (text)
               (let ((f (picture-session dir text "C-u 6 0 C-x f C-x T p" :cell-pixels '(10 20) :width 100)))
                 (destructuring-bind (row column columns &rest more)
                     (cdr (first (tuatara.frontend.fake:fake-images f)))
                   (declare (ignore row more))
                   (list column columns)))))
        (is equal '(0 60) (placed (format nil "Above~%[[file:wide.png]]~%")))
        (is equal '(40 20) (placed (format nil "#+ATTR_ORG: :align right~%[[file:pic.png]]~%")))
        (is equal '(40 20) (placed (format nil "#+ATTR_ORG: :float right~%[[file:pic.png]]~%one~%")))))))

(define-test (inline-pictures formula-ink)
  ;; a formula drawn black on white: cut to its ink with a pixel round it, the ink in the text's colour, the paper
  ;; let through; one inside a line as tall as a row, the ink in the middle
  (let* ((width 10) (height 6)
         (pixels (make-array (* 3 width height) :element-type '(unsigned-byte 8) :initial-element 255)))
    (loop for y from 2 to 3
          do (loop for x from 3 to 5 do (fill pixels 0 :start (* 3 (+ x (* y width))) :end (* 3 (+ 1 x (* y width))))))
    (let* ((image (tuatara.frontend:make-image width height 3 pixels))
           (ink (tuatara.export::formula-ink image "#102030" nil 20))
           (out (tuatara.frontend:image-pixels ink)))
      (is equal '(5 4 4) (list (tuatara.frontend:image-width ink) (tuatara.frontend:image-height ink)
                               (tuatara.frontend:image-channels ink)))
      ;; the margin clear, the ink its colour and whole
      (is = 0 (aref out 3))
      (is equal '(16 32 48 255) (coerce (subseq out (* 4 (+ 1 5)) (* 4 (+ 2 5))) 'list))
      (let ((inline (tuatara.export::formula-ink image "#102030" t 10)))
        (is = 10 (tuatara.frontend:image-height inline))
        ;; four rows of it, three above
        (is = 255 (aref (tuatara.frontend:image-pixels inline) (+ 3 (* 4 (+ 1 (* 4 5))))))
        (is = 0 (aref (tuatara.frontend:image-pixels inline) (+ 3 (* 4 (+ 1 (* 2 5))))))))
    ;; nothing drawn: none
    (false (tuatara.export::formula-ink (tuatara.frontend:make-image 2 2 3 (make-array 12 :element-type '(unsigned-byte 8)
                                                                                        :initial-element 255))
                                        "#000000" nil 20))))

(define-test (inline-pictures formula-document)
  ;; each formula on a page of its own, cut to it, after the preamble
  (let ((text (tuatara.export::formula-document "\\usepackage{amsmath}" '("$x$" "\\[ y \\]"))))
    (true (search "\\usepackage{amsmath}" text))
    (true (search "\\usepackage[active,tightpage]{preview}" text))
    (true (search "\\begin{preview}$x$\\end{preview}" text))
    (true (search "\\begin{preview}\\[ y \\]\\end{preview}" text))
    (is = 2 (count-matches "\\begin{preview}" text))))

(define-test (inline-pictures org-formulas-found)
  ;; which formulas an Org note shows as pictures: inside a line in its columns, alone on its lines or an
  ;; environment on rows of their own, the #+LATEX_HEADER: lines above the first heading in their preamble when
  ;; the note is in a project trusted, else left out
  (with-plain-org
    (with-test-directory (dir)
      (let* ((text (format nil "#+LATEX_HEADER: \\usepackage{bm}~%Then $x^2$ and \\(y\\) here.~%\\[ E = mc^2 \\]~%~
                                \\begin{align}~%a &= b~%\\end{align}~%A \\alpha and $5 and $6.~%* H~%"))
             (found '()))
        (picture-session dir text "" :more (list (lambda ()
                                                   (let ((buffer (tuatara.command:current-buffer)))
                                                     (setf found (tuatara.defaults::org-inline-pictures
                                                                  buffer 0 (tuatara.buffer:buffer-size buffer)))))))
        (flet ((at (string) (search string text)))
          (is equal (list (list (at "$x^2$") (+ 5 (at "$x^2$")) "$x^2$" t)
                          (list (at "\\(y") (+ 5 (at "\\(y")) "\\(y\\)" t)
                          (list (at "\\[ E") (+ 14 (at "\\[ E")) "\\[ E = mc^2 \\]" nil)
                          (list (at "\\begin") (+ (at "\\end{align}") 11) (format nil "\\begin{align}~%a &= b~%\\end{align}")
                                nil))
              (mapcar (lambda (picture)
                        (let ((formula (tuatara.defaults::inline-picture-source picture)))
                          (list (tuatara.defaults::inline-picture-start picture)
                                (tuatara.defaults::inline-picture-end picture)
                                (tuatara.defaults::formula-text formula) (tuatara.defaults::formula-inline formula))))
                      found)))
        (false (search "\\usepackage{bm}" (tuatara.defaults::formula-preamble
                                           (tuatara.defaults::inline-picture-source (first found)))))
        (true (search "\\usepackage{amsmath}" (tuatara.defaults::formula-preamble
                                               (tuatara.defaults::inline-picture-source (first found)))))
        (sb-int:encapsulate 'tuatara.defaults::buffer-in-trusted-project-p 'trusted (constantly t))
        (unwind-protect
             (picture-session dir text "" :more (list (lambda ()
                                                        (let ((buffer (tuatara.command:current-buffer)))
                                                          (setf found (tuatara.defaults::org-inline-pictures
                                                                       buffer 0 (tuatara.buffer:buffer-size buffer)))))))
          (sb-int:unencapsulate 'tuatara.defaults::buffer-in-trusted-project-p 'trusted))
        (true (search "\\usepackage{bm}" (tuatara.defaults::formula-preamble
                                          (tuatara.defaults::inline-picture-source (first found)))))))))

(define-test (inline-pictures formulas-drawn)
  ;; drawn by LaTeX, shut in: one alone on its line centred on rows of its own, one inside a line in the columns
  ;; it stands in, an environment of three lines on the rows of one; point on one shows it as written
  (if (not (and (tuatara.export::latex-engines-found) (eq :ready (tuatara.io:sandbox-status))))
      (skip "no LaTeX program or no sandbox here" (true nil))
      (with-plain-org
        (with-test-directory (dir)
          (let* ((text (format nil "Above~%\\[ E = mc^2 \\]~%Then $x^2$ here.~%\\begin{align}~%a &= b\\\\~%c &= d~%~
                                    \\end{align}~%After~%"))
                 (f (picture-session dir text "C-x T p" :cell-pixels '(10 20) :width 60 :height 30))
                 (lines (screen-lines f))
                 (images (sort (mapcar #'cdr (tuatara.frontend.fake:fake-images f)) #'< :key #'first)))
            (is = 3 (length images))
            (is string= "Above" (first lines))
            (destructuring-bind ((display-row display-column display-columns display-rows &rest more1)
                                 (inline-row inline-column inline-columns inline-rows &rest more2)
                                 (align-row align-column align-columns align-rows &rest more3))
                images
              (declare (ignore more1 more2 more3 display-columns align-column))
              ;; the display formula centred below Above, the rows it takes blank
              (is = 1 display-row)
              (true (plusp display-column))
              (true (every (lambda (line) (string= line "")) (subseq lines 1 (+ 1 display-rows))))
              ;; the inline one where $x^2$ began, its columns blank, one row tall
              (is = (+ 1 display-rows) inline-row)
              (is = 5 inline-column)
              (is = 1 inline-rows)
              (let ((line (nth inline-row lines)))
                (is string= "Then " (subseq line 0 5))
                (false (search "$" line))
                (is string= " here." (subseq line (+ 5 inline-columns))))
              ;; the environment's three lines on one picture's rows, After right below
              (is = (1+ inline-row) align-row)
              (is string= "After" (nth (+ align-row align-rows) lines))))
          ;; point on the inline formula: it is shown as written
          (let ((f (picture-session dir (format nil "Then $x^2$ here.~%") "C-x T p" :more "C-f C-f C-f C-f C-f C-f"
                                                                                    :cell-pixels '(10 20))))
            (is string= "Then $x^2$ here." (first (screen-lines f))))))))

(define-test (inline-pictures latex-formulas-found)
  ;; a LaTeX file's formulas: in its body, not in comments, escaped dollars or verbatim text, never past a blank
  ;; line; drawn with its own preamble
  (with-test-directory (dir)
    (let* ((text (format nil "\\documentclass{article}~%\\usepackage{amsmath}~%\\newcommand{\\R}{\\mathbb{R}}~%~
                              \\begin{document}~%Cost \\$5 and $x^2$ here % $not$~%\\[ a + b \\]~%~
                              \\begin{verbatim}~%$no$~%\\end{verbatim}~%\\begin{align*}~%x &= y~%\\end{align*}~%~
                              An $unclosed~%~%next paragraph $z$.~%\\end{document}~%"))
           (found '()))
      (picture-session dir text "" :name "doc.tex"
                                   :more (list (lambda ()
                                                 (let ((buffer (tuatara.command:current-buffer)))
                                                   (setf found (tuatara.defaults::latex-formulas
                                                                buffer 0 (tuatara.buffer:buffer-size buffer)))))))
      (flet ((at (string) (search string text)))
        (is equal (list (list (at "$x^2$") (+ 5 (at "$x^2$")) "$x^2$" t)
                        (list (at "\\[ a") (+ 11 (at "\\[ a")) "\\[ a + b \\]" nil)
                        (list (at "\\begin{align*}") (+ 12 (at "\\end{align*}"))
                              (format nil "\\begin{align*}~%x &= y~%\\end{align*}") nil)
                        (list (at "$z$") (+ 3 (at "$z$")) "$z$" t))
            (mapcar (lambda (picture)
                      (let ((formula (tuatara.defaults::inline-picture-source picture)))
                        (list (tuatara.defaults::inline-picture-start picture)
                              (tuatara.defaults::inline-picture-end picture)
                              (tuatara.defaults::formula-text formula) (tuatara.defaults::formula-inline formula))))
                    found)))
      (let ((preamble (tuatara.defaults::formula-preamble (tuatara.defaults::inline-picture-source (first found)))))
        (true (search "\\newcommand{\\R}" preamble))
        (false (search "documentclass" preamble))
        (false (search "begin{document}" preamble))))))

(define-test (inline-pictures latex-formulas-drawn)
  ;; a LaTeX file whose preamble takes in a package of its own the sandbox does not show: its formulas drawn all
  ;; the same, after *formula-preamble*
  (if (not (and (tuatara.export::latex-engines-found) (eq :ready (tuatara.io:sandbox-status))))
      (skip "no LaTeX program or no sandbox here" (true nil))
      (with-test-directory (dir)
        (alexandria:write-string-into-file "\\ProvidesPackage{mine}" (merge-pathnames "mine.sty" dir))
        (let ((f (picture-session dir (format nil "\\documentclass{article}~%\\usepackage{mine}~%\\begin{document}~%~
                                                   Then $x^2$ here.~%\\end{document}~%")
                                  "C-x T p" :name "doc.tex" :cell-pixels '(10 20))))
          (is = 1 (length (tuatara.frontend.fake:fake-images f)))
          (is string= "Then " (subseq (tuatara.frontend.fake:fake-screen-line f 3) 0 5))))))

(define-test (inline-pictures formulas-held-to-limits)
  ;; a formula that never ends is given up after *formula-seconds*, one drawn huge is not drawn: both shown as
  ;; written, the formula beside them drawn
  (if (not (and (tuatara.export::latex-engines-found) (eq :ready (tuatara.io:sandbox-status))))
      (skip "no LaTeX program or no sandbox here" (true nil))
      (with-plain-org
        (with-test-directory (dir)
          (let ((before tuatara.export::*formula-seconds*))
            (setf tuatara.export::*formula-seconds* 2)
            (unwind-protect
                 (let* ((f (picture-session dir (format nil "Loop $\\def\\a{\\a}\\a$~%Huge $\\rule{3000pt}{3000pt}$~%~
                                                            Fine $x$.~%")
                                            "C-x T p" :cell-pixels '(10 20) :width 60))
                        (lines (screen-lines f)))
                   (is = 1 (length (tuatara.frontend.fake:fake-images f)))
                   (true (search "$\\def" (first lines)))
                   (true (search "$\\rule" (second lines)))
                   (false (search "$x$" (third lines))))
              (setf tuatara.export::*formula-seconds* before)))))))

(define-test (inline-pictures stopped-picture-drawn-again)
  ;; a picture whose drawing was stopped is not kept as one that cannot be drawn: it is drawn when next in sight
  (let* ((tuatara.defaults::*inline-picture-images* (make-hash-table :test 'equal))
         (key (list "/nowhere/kea.png" nil 40 20))
         (job (tuatara.defaults::make-inline-picture-job nil key "/nowhere/kea.png" 40 20)))
    (setf (gethash key tuatara.defaults::*inline-picture-images*) :pending
          (tuatara.runtime:job-status job) :stopped)
    (tuatara.runtime:job-take-in job)
    (false (nth-value 1 (gethash key tuatara.defaults::*inline-picture-images*)))))

(define-test (inline-pictures failed-pictures-bounded)
  ;; pictures that could not be drawn, one for each width tried, are not kept without end
  (let ((table (make-hash-table :test 'equal)))
    (dotimes (width 300)
      (setf (gethash (list "/nowhere/kea.png" nil width 20) table) :failed))
    (tuatara.defaults::forget-pictures-past (* 1024 1024) table)
    (true (<= (hash-table-count table) tuatara.defaults::*inline-pictures-failed-kept*))))

(define-test (inline-pictures own-words-of-a-note-given-count-not)
  ;; a note's #+STARTUP: turns its pictures and formulas off, but on only as *org-startup-shows-pictures* allows
  ;; and never for a note opened safely or read-only; a note not trusted has none of its own LaTeX lines drawn
  (let ((b (tuatara.buffer:make-buffer "org pictures own words"
                                       :text (format nil "#+LATEX_HEADER: \\usepackage{mine}~%* A~%$x$~%"))))
    (unwind-protect
         (flet ((after (options)
                  (setf (tuatara.buffer:buffer-local b :inline-pictures) :unset)
                  (tuatara.defaults::org-startup-pictures b options)
                  (tuatara.buffer:buffer-local b :inline-pictures)))
           (is eq :unset (after '("latexpreview")))
           (is eq nil (after '("noinlineimages")))
           (false (search "usepackage{mine}" (tuatara.defaults::org-formula-preamble b)))
           (let ((tuatara.defaults::*org-startup-shows-pictures* t))
             (is eq t (after '("inlineimages")))
             (setf (tuatara.io:safe-visit-p b) t)
             (is eq :unset (after '("latexpreview")))
             (setf (tuatara.io:safe-visit-p b) nil
                   (tuatara.buffer:buffer-read-only-p b) t)
             (is eq :unset (after '("inlineimages")))))
      (tuatara.buffer:kill-buffer b))))

(define-test (inline-pictures picture-added-as-each-mode-writes-it)
  ;; C-c C-x p: only pictures offered, the one selected shown while choosing; its link written alone on new
  ;; lines with the properties a picture takes; the picture itself linked where it is and left unchanged
  (with-test-directory (dir)
    (let* ((picture (picture-file dir "my pic.png" 20 10))
           (before (alexandria:read-file-into-byte-vector picture))
           (written (file-write-date picture))
           (offered nil)
           (previewed nil)
           (pictures-shown :unset))
      (alexandria:write-string-into-file "x" (merge-pathnames "words.txt" dir))
      (flet ((added (name text)
               (let ((file (namestring (merge-pathnames name dir))))
                 (alexandria:write-string-into-file text file :if-exists :supersede)
                 (let ((editor (nth-value 1 (run-keys (list (format nil "M-> C-c C-x p C-a C-k ~a" (rimu-typed (namestring dir)))
                                                            :wait
                                                            (lambda () (setf offered (shown-candidates)))
                                                            (rimu-typed "my pic")
                                                            :wait
                                                            (lambda ()
                                                              (let ((shown (tuatara.display:window-buffer
                                                                            (tuatara.command:editor-window
                                                                             tuatara.command:*editor*))))
                                                                (setf previewed (and (tuatara.buffer:buffer-file shown)
                                                                                     (tuatara.io:native
                                                                                      (tuatara.buffer:buffer-file shown))))))
                                                            "RET")
                                                      :file file :width 100 :height 20))))
                   (setf pictures-shown (tuatara.defaults::inline-pictures-p
                                         (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (prog1 (session-text editor)
                     (forget-file-buffers))))))
        (is string= (format nil "Above~%#+CAPTION:~%#+NAME: fig:my-pic~%#+ATTR_ORG: :width t~%~
                                 #+ATTR_LATEX: :width .9\\linewidth~%[[file:my pic.png]]")
            (added "notes.org" "Above"))
        (is equal '("my pic.png") offered)
        (is string= (tuatara.io:native picture) previewed)
        (false pictures-shown)
        (is string= (format nil "Some text~%![](<my pic.png>){width=100%}")
            (added "notes.md" (format nil "Some text~%")))
        (let ((latex (added "notes.tex" (format nil "\\begin{document}~%"))))
          (true (search "\\begin{figure}[tbp]" latex) "~a" latex)
          (true (cl-ppcre:scan "(?m)^\\s*\\\\includegraphics\\[width=\\.9\\\\linewidth\\]\\{my pic\\.png\\}$" latex) "~a" latex)
          (true (cl-ppcre:scan "(?m)^\\s*\\\\caption\\{\\}$" latex) "~a" latex)
          (true (cl-ppcre:scan "(?m)^\\s*\\\\label\\{fig:my-pic\\}$" latex) "~a" latex))
        (false (find picture tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file
                                                       :test (lambda (a b) (and b (string= a (tuatara.io:native b))))))
        (is equalp before (alexandria:read-file-into-byte-vector picture))
        (is = written (file-write-date picture))
        (is equal '("my pic.png") (mapcar (lambda (p) (tuatara.io:native-file-name (namestring p)))
                                          (directory (merge-pathnames "*.png" dir))))))))

(define-test (inline-pictures one-picture-link-test)
  ;; the picture view and the exports ask one question, ORG-IMAGE-LINK-P, of a link: a file link with no
  ;; description; they differ only in which file names count as pictures
  (flet ((link (text)
           (first (tuatara.defaults::org-node-children (tuatara.defaults::org-parse-line text)))))
    (dolist (text '("[[file:a.png]]" "[[./a.png]]"))
      (true (tuatara.defaults::org-picture-link-p (link text)) "~a" text)
      (true (tuatara.defaults::org-html-image-link-p (link text)) "~a" text))
    (dolist (text '("[[file:a.png][shown]]" "[[https://example.org/a.png]]" "[[file:notes.txt]]" "[[file:a]]"))
      (false (tuatara.defaults::org-picture-link-p (link text)) "~a" text)
      (false (tuatara.defaults::org-html-image-link-p (link text)) "~a" text))
    ;; a type only the exports take, one only the view draws
    (false (tuatara.defaults::org-picture-link-p (link "[[file:a.eps]]")))
    (true (tuatara.defaults::org-image-link-p (link "[[file:a.eps]]") tuatara.defaults::*org-latex-image-types*))
    (is eq t (tuatara.defaults::org-image-link-p (link "[[file:a.png]]") (constantly :anything)))))
