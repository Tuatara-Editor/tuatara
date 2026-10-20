;;;; test/mouse.lisp --- The mouse: clicking to put point, dragging and clicking again to select, the wheel.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test mouse
  :description "The first mouse button puts point where it goes down, in the window there; dragging selects, scrolling when held past the window's text; twice selects a word, three times a line; the wheel scrolls the window under the mouse.")

(defparameter *mouse-text* (format nil "hello world~%second line~%")
  "Two short lines for the mouse to point at.")

(defun run-events (items &key (text *mouse-text*) (width 40) (height 8) primary)
  "RUN-KEYS with ITEMS in turn: a string is keys as RUN-KEYS takes them, a list
(BASE COLUMN ROW) a mouse key at that cell of the screen; PRIMARY the text
selected in another program."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (setf (tuatara.frontend.fake:fake-primary f) primary)
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (setf tuatara.defaults::*command-history* '())
    (dolist (item items)
      (if (stringp item)
          (tuatara.frontend.fake:fake-queue-keys f item)
          (destructuring-bind (base column row) item
            (tuatara.frontend.fake:fake-queue-event
             f (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key base :column column :row row))))))
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                :buffer (tuatara.buffer:find-buffer "*scratch*"))))
      (values f editor))))

(defun session-region (editor)
  "The selection in the editor's current buffer as (START END), or NIL."
  (let ((b (session-buffer editor)))
    (when (tuatara.buffer:buffer-mark-active-p b)
      (let ((point (tuatara.buffer:buffer-point b)) (mark (tuatara.buffer:buffer-mark b)))
        (list (min point mark) (max point mark))))))

(defun mouse-point (&rest items)
  "Point after RUN-EVENTS with ITEMS on *MOUSE-TEXT*."
  (session-point (nth-value 1 (run-events items))))

(define-test (mouse clicking)
  ;; the character under the mouse; past a line's text, its end; below the text, the end
  (is = 6 (mouse-point '(:down-mouse-1 6 0)))
  (is = 23 (mouse-point '(:down-mouse-1 30 1)))
  (is = 24 (mouse-point '(:down-mouse-1 3 4)))
  ;; the mode line and the echo row are no place in the text
  (is = 0 (mouse-point '(:down-mouse-1 3 6)))
  (is = 0 (mouse-point '(:down-mouse-1 3 7)))
  ;; the cursor is drawn there, and the selection let go of
  (multiple-value-bind (f editor) (run-events '("v l l" (:down-mouse-1 8 1)))
    (is = 20 (session-point editor))
    (false (session-region editor))
    (is equal '(1 . 8) (tuatara.frontend.fake:fake-cursor f)))
  ;; a click only moves about: a split waiting for its verb stays, as it
  ;; does for the keys that move; a drag, which selects, ends it
  (is string= "Xfoo bar Xfoo"
      (session-text (nth-value 1 (run-events '("f f o o RET" (:down-mouse-1 4 0) "i X C-g") :text "foo bar foo"))))
  (true (tuatara.frontend.fake:fake-frame-showing
         (run-events '("f f o o RET" (:down-mouse-1 4 0) (:drag-mouse-1 6 0)) :text "foo bar foo")
         "Split ended: 2 places left alone"))
  ;; in INSERT state too, which it stays in
  (multiple-value-bind (f editor) (run-events '("i" (:down-mouse-1 2 0) "x"))
    (declare (ignore f))
    (is string= (format nil "hexllo world~%second line~%") (session-text editor)))
  ;; the line numbers are no text: a click on them goes to the row's first character
  (is = 12 (session-point (nth-value 1 (run-events '("M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET"
                                                     (:down-mouse-1 0 1))))))
  (let* ((editor (nth-value 1 (run-events '("M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET"
                                            (:down-mouse-1 6 1)))))
         (margin (tuatara.display:window-margin (tuatara.command:editor-window editor))))
    (true (plusp margin))
    (is = (+ 12 (- 6 margin)) (session-point editor))))

(define-test (mouse clicking-in-windows)
  ;; C-x 2 on twelve rows: the upper window's text on rows 0 to 4, the lower's on 6 to 9
  (let ((editor (nth-value 1 (run-events '("C-x 2" (:down-mouse-1 2 7)) :text *twenty-lines* :height 12))))
    (is = 1 (selected-index editor))
    (is = 9 (session-point editor))
    (is = 0 (tuatara.display:window-point (first (tuatara.command:editor-windows editor)))))
  ;; beside each other: the divider is neither window's text
  (let ((editor (nth-value 1 (run-events '("C-x 3" (:down-mouse-1 19 1)) :text *twenty-lines*))))
    (is = 0 (selected-index editor))
    (is = 0 (session-point editor)))
  (let ((editor (nth-value 1 (run-events '("C-x 3" (:down-mouse-1 22 1)) :text *twenty-lines*))))
    (is = 1 (selected-index editor))
    (is = 9 (session-point editor))))

(define-test (mouse dragging)
  ;; down at one place, dragged to another: what lies between selected, point at the mouse
  (let ((editor (nth-value 1 (run-events '((:down-mouse-1 2 0) (:drag-mouse-1 4 0) (:drag-mouse-1 5 1))))))
    (is equal '(2 17) (session-region editor))
    (is = 17 (session-point editor)))
  (let ((editor (nth-value 1 (run-events '((:down-mouse-1 5 1) (:drag-mouse-1 2 0))))))
    (is equal '(2 17) (session-region editor))
    (is = 2 (session-point editor)))
  ;; the selection verbs take it as any selection
  (is string= (format nil " world~%second line~%")
      (session-text (nth-value 1 (run-events '((:down-mouse-1 0 0) (:drag-mouse-1 5 0) "d")))))
  ;; a drag from outside the text selects nothing
  (false (session-region (nth-value 1 (run-events '((:down-mouse-1 3 6) (:drag-mouse-1 5 1))))))
  (false (session-region (nth-value 1 (run-events '((:drag-mouse-1 5 1))))))
  ;; held below the window's text it scrolls a line each time, point on the last row shown
  (let* ((editor (nth-value 1 (run-events '((:down-mouse-1 0 0) (:drag-mouse-1 0 7) (:drag-mouse-1 0 7) (:drag-mouse-1 0 7))
                                          :text *twenty-lines*)))
         (window (tuatara.command:editor-window editor)))
    (is = 21 (tuatara.display:window-start window))
    (is equal '(0 56) (session-region editor)))
  ;; and above it, back: C-v shows line 5 at the top, the press is on line 7
  (let ((editor (nth-value 1 (run-events '("C-v" (:down-mouse-1 3 2) (:drag-mouse-1 3 -1) (:drag-mouse-1 3 -1))
                                         :text *twenty-lines*))))
    (is = 14 (tuatara.display:window-start (tuatara.command:editor-window editor)))
    (is equal '(17 45) (session-region editor))
    (is = 17 (session-point editor)))
  ;; no scrolling past the end when all of it is shown
  (let ((editor (nth-value 1 (run-events '((:down-mouse-1 0 0) (:drag-mouse-1 0 7))))))
    (is = 0 (tuatara.display:window-start (tuatara.command:editor-window editor)))
    (is equal '(0 24) (session-region editor))))

(define-test (mouse clicking-again)
  ;; twice: the word; three times: the line with its line break
  (let ((editor (nth-value 1 (run-events '((:down-mouse-1 8 0) (:double-mouse-1 8 0))))))
    (is equal '(6 11) (session-region editor))
    (is = 11 (session-point editor)))
  (is equal '(12 24) (session-region (nth-value 1 (run-events '((:down-mouse-1 8 1) (:double-mouse-1 8 1) (:triple-mouse-1 8 1))))))
  ;; after a rectangle, a word clicked twice is a plain selection
  (let ((editor (nth-value 1 (run-events '("C-x SPC C-f C-f" (:down-mouse-1 8 0) (:double-mouse-1 8 0))))))
    (false (tuatara.defaults::rectangle-live-p (session-buffer editor)))
    (is equal '(6 11) (session-region editor)))
  ;; spaces are a run of their own, as are other signs; a bracket gives its group
  (flet ((word (text column)
           (let ((editor (nth-value 1 (run-events `((:double-mouse-1 ,column 0)) :text text))))
             (let ((region (session-region editor)))
               (and region (subseq text (first region) (second region)))))))
    (is string= "   " (word "ab   cd" 3))
    (is string= "->" (word "a->b" 2))
    (is string= "snake_case" (word "x snake_case y" 5))
    (is string= "(b c)" (word "a (b c) d" 2))
    (is string= "(b c)" (word "a (b c) d" 6))
    ;; a bracket without a partner: itself
    (is string= "(" (word "a (b c d" 2))
    (is string= ")" (word "a b c) d" 5))
    ;; past a line's end: the run before it
    (is string= "cd" (word "ab cd" 9))
    (false (word "" 3)))
  ;; a drag after two clicks goes on from the word's start
  (is equal '(6 15) (session-region (nth-value 1 (run-events '((:down-mouse-1 8 0) (:double-mouse-1 8 0) (:drag-mouse-1 3 1)))))))

(define-test (mouse hex-view)
  ;; on an octet's digits: that octet, point at the start of its character
  (let ((editor (nth-value 1 (run-events '("M-x h e x - m o d e RET" (:down-mouse-1 16 1)) :text *hex-sample* :width 80))))
    (is = 18 (session-hex-point editor))
    (is = 17 (session-point editor)))
  ;; on the offsets: the row's first octet
  (is = 16 (session-hex-point (nth-value 1 (run-events '("M-x h e x - m o d e RET" (:down-mouse-1 2 1))
                                                       :text *hex-sample* :width 80)))))

(define-test (mouse wheel)
  ;; the window under the mouse scrolls; the selected one stays as it was
  (let* ((editor (nth-value 1 (run-events '("C-x 2" (:wheel-down 3 7)) :text *twenty-lines* :height 12)))
         (windows (tuatara.command:editor-windows editor)))
    (is = 0 (selected-index editor))
    (is = 0 (tuatara.display:window-start (first windows)))
    (is = 21 (tuatara.display:window-start (second windows))))
  ;; a split stays while the wheel turns
  (is string= "Xfoo bar Xfoo"
      (session-text (nth-value 1 (run-events '("f f o o RET" (:wheel-down 0 0) "i X C-g") :text "foo bar foo"))))
  ;; without a cell, the selected window; up goes back
  (is = 0 (tuatara.display:window-start
           (tuatara.command:editor-window (nth-value 1 (run-events '("<wheel-down>" "<wheel-up>") :text *twenty-lines*)))))
  (is = 21 (tuatara.display:window-start
            (tuatara.command:editor-window (nth-value 1 (run-events '("<wheel-down>") :text *twenty-lines*))))))

(define-test (mouse not-repeated)
  ;; . repeats the command before the click
  (is = 16 (mouse-point "l" '(:down-mouse-1 3 1) ".")))

(define-test (mouse dropping-files)
  (let* ((directory (uiop:ensure-directory-pathname
                     (format nil "~adrop-~d/" (namestring (uiop:temporary-directory)) (random 1000000))))
         (one (concatenate 'string (namestring directory) "one.txt"))
         (two (concatenate 'string (namestring directory) "two [2].txt")))
    (ensure-directories-exist directory)
    (with-open-file (o (sb-ext:parse-native-namestring one) :direction :output :if-exists :supersede)
      (write-string "first file" o))
    (with-open-file (o (sb-ext:parse-native-namestring two) :direction :output :if-exists :supersede)
      (write-string "second file" o))
    (unwind-protect
         (flet ((dropped (files column row &key keys (text *mouse-text*) (height 8))
                  (let ((f (tuatara.frontend.fake:make-fake-frontend :height height)))
                    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
                      (when scratch (tuatara.buffer:kill-buffer scratch)))
                    (dolist (name '("one.txt" "two [2].txt"))
                      (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))
                    (tuatara.buffer:make-buffer "*scratch*" :text text)
                    (when keys (tuatara.frontend.fake:fake-queue-keys f keys))
                    (tuatara.frontend.fake:fake-queue-event
                     f (tuatara.frontend:make-event :drop :files files :column column :row row))
                    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                            :buffer (tuatara.buffer:find-buffer "*scratch*"))))))
           ;; one file: visited and shown, as C-x C-f does, with its mode
           (multiple-value-bind (f editor) (dropped (list one) 3 1)
             (declare (ignore f))
             (is string= "first file" (session-text editor))
             (false (tuatara.io:safe-visit-p (session-buffer editor))))
           ;; two, one with [ ] in its name: both visited, the last shown, the count said
           (multiple-value-bind (f editor) (dropped (list one two) 3 1)
             (is string= "second file" (session-text editor))
             (true (tuatara.buffer:find-buffer "one.txt"))
             (true (search "Opened 2 files" (tuatara.frontend.fake:fake-screen-line f 7))))
           ;; in the window dropped on
           (multiple-value-bind (f editor) (dropped (list one) 2 7 :keys "C-x 2" :text *twenty-lines* :height 12)
             (declare (ignore f))
             (is = 1 (selected-index editor))
             (is string= "first file" (session-text editor)))
           ;; a directory shows as its rimu list, as C-x C-f shows it
           (multiple-value-bind (f editor) (dropped (list (namestring directory)) 3 1)
             (declare (ignore f))
             (true (search "two [2].txt" (session-text editor))))
           ;; safely, when asked for
           (let ((tuatara-user:*open-dropped-files-safely* t))
             (multiple-value-bind (f editor) (dropped (list one) 3 1)
               (declare (ignore f))
               (true (tuatara.io:safe-visit-p (session-buffer editor)))))
           ;; M-x without a drop says what it is for
           (true (search "Drop files on the window"
                         (tuatara.frontend.fake:fake-screen-line
                          (run-keys "M-x o p e n - d r o p p e d - f i l e s RET") 7))))
      (dolist (name '("one.txt" "two [2].txt"))
        (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))
      (uiop:delete-directory-tree directory :validate t))))

(define-test (mouse dropped-shown-as-visited)
  ;; a file dropped opens as C-x C-f opens it: a binary one in the hex view, and a
  ;; file that opens safely by itself says why
  (with-test-directory (dir)
    (let ((binary (namestring (merge-pathnames "drop.bin" dir)))
          (text (namestring (put-file (merge-pathnames "drop.txt" dir) "dropped text"))))
      (alexandria:write-byte-vector-into-file (coerce #(102 111 111 0 1 2) '(vector (unsigned-byte 8))) binary)
      (flet ((dropped (path)
               (let ((f (tuatara.frontend.fake:make-fake-frontend :height 8 :width 120)))
                 (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
                   (when scratch (tuatara.buffer:kill-buffer scratch)))
                 (dolist (name '("drop.bin" "drop.txt"))
                   (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))
                 (tuatara.buffer:make-buffer "*scratch*" :text *mouse-text*)
                 (tuatara.frontend.fake:fake-queue-event
                  f (tuatara.frontend:make-event :drop :files (list path) :column 3 :row 1))
                 (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                         :buffer (tuatara.buffer:find-buffer "*scratch*"))))))
        (unwind-protect
             (progn
               (multiple-value-bind (f editor) (dropped binary)
                 (declare (ignore f))
                 (is string= "drop.bin" (tuatara.buffer:buffer-name (session-buffer editor)))
                 (true (tuatara.display:hex-view-p (session-buffer editor))))
               (let ((tuatara-user:*safe-directories* (list (namestring dir))))
                 (multiple-value-bind (f editor) (dropped text)
                   (true (tuatara.io:safe-visit-p (session-buffer editor)))
                   (true (tuatara.frontend.fake:fake-frame-showing f "drop.txt opened safely: it is in")))))
          (dolist (name '("drop.bin" "drop.txt"))
            (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b)))))))))

(define-test (mouse window-edges)
  ;; C-x 2 on twelve rows: the upper window's mode line on row 5, the lower's on row 10
  (flet ((boxes (items &key (height 12) (keys "C-x 2"))
           (window-boxes (nth-value 1 (run-events (cons keys items) :text *twenty-lines* :height height)))))
    ;; a click on a mode line selects that window, point staying
    (let ((editor (nth-value 1 (run-events '("C-x 2" (:down-mouse-1 3 10)) :text *twenty-lines* :height 12))))
      (is = 1 (selected-index editor))
      (is = 0 (session-point editor)))
    ;; dragging the mode line between them sizes both, eleven rows shared
    (is equal '((0 0 40 4) (0 4 40 7)) (boxes '((:down-mouse-1 3 5) (:drag-mouse-1 3 4) (:drag-mouse-1 3 3))))
    (is equal '((0 0 40 7) (0 7 40 4)) (boxes '((:down-mouse-1 3 5) (:drag-mouse-1 3 6))))
    ;; no smaller than a window may be
    (let ((sizes (boxes '((:down-mouse-1 3 5) (:drag-mouse-1 3 0)))))
      (true (>= (fourth (first sizes)) tuatara-user:*window-min-height*)))
    ;; the lowest mode line has no window below it to give rows to
    (is equal '((0 0 40 6) (0 6 40 5)) (boxes '((:down-mouse-1 3 10) (:drag-mouse-1 3 8))))
    ;; the divider between windows side by side, dragged, sizes them
    (is equal '((0 0 26 7) (26 0 14 7)) (boxes '((:down-mouse-1 19 2) (:drag-mouse-1 25 2)) :height 8 :keys "C-x 3"))
    ;; clicked twice, a mode line selects no text
    (let ((editor (nth-value 1 (run-events '("C-x 2" (:down-mouse-1 3 5) (:double-mouse-1 3 5))
                                           :text *twenty-lines* :height 12))))
      (false (session-region editor)))))

(define-test (mouse middle-button-pastes-the-selection)
  ;; what is selected anywhere goes in where the middle button was pressed
  (multiple-value-bind (f editor) (run-events '((:mouse-2 6 0)) :primary "big ")
    (declare (ignore f))
    (is string= (format nil "hello big world~%second line~%") (session-text editor))))
