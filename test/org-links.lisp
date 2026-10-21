;;;; test/org-links.lisp --- Org's links: followed, shown as their labels, inserted, and ids in other notes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (org links-followed)
  (let ((text (format nil "See [[*Two]] and [[#here]] and [[target]] and [[id:x-1]].~%* One~%<<target>> is here.~%* Two~%:PROPERTIES:~%:CUSTOM_ID: here~%:ID: x-1~%:END:~%")))
    (flet ((after (keys) (org-line-of-point (nth-value 1 (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 80 :height 12)))))
      ;; RET in NORMAL state on a link follows it: a headline, a custom ID, a target, an ID
      (is string= "* Two" (after "C-u 5 l RET"))
      (is string= "* Two" (after "C-u 1 8 l RET"))
      (is string= "<<target>> is here." (after "C-u 3 2 l RET"))
      (is string= "* Two" (after "C-u 4 7 l RET"))
      ;; C-c C-o too
      (is string= "* Two" (after "C-u 5 l C-c C-o"))
      ;; RET in NORMAL state elsewhere does nothing; while typing, a new line
      (is string= (format nil "See [[*Two]] and [[#here]] and [[target]] and [[id:x-1]].")
          (after "RET"))
      (true (search (format nil "S~%ee") (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET l i RET ESC" :text text :width 80 :height 12)))))))
  ;; a file link, with a headline to go to in it; a note by its identifier
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "* First~%* Wanted~%") (merge-pathnames "other.org" dir))
    (alexandria:write-string-into-file (format nil "* A note~%") (merge-pathnames "20260101T101010--a-note.org" dir))
    (let ((text (format nil "[[file:other.org::*Wanted][there]] and [[denote:20260101T101010][a note]]~%"))
          (file (merge-pathnames "links.org" dir)))
      (alexandria:write-string-into-file text file)
      (multiple-value-bind (line name)
          (org-line-of-point (nth-value 1 (run-keys (format nil "C-x C-f C-a C-k ~a RET RET" (rimu-typed (namestring file))) :width 120 :height 12)))
        (is string= "other.org" name)
        (is string= "* Wanted" line))
      (let ((tuatara-user::*formatted-file-directory* (namestring dir)))
        (multiple-value-bind (line name)
            (org-line-of-point (nth-value 1 (run-keys (format nil "C-x C-f C-a C-k ~a RET C-u 4 0 l RET" (rimu-typed (namestring file))) :width 120 :height 12)))
          (declare (ignore line))
          (is string= "20260101T101010--a-note.org" name))))
    ;; the files visited go, for the tests after
    (dolist (name '("other.org" "links.org" "20260101T101010--a-note.org"))
      (let ((buffer (tuatara.buffer:find-buffer name)))
        (when buffer (tuatara.buffer:kill-buffer buffer)))))
  ;; denote:query-filenames: lists the notes whose names match, to visit one;
  ;; match-denote-files-link inserts one
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "* A note~%") (merge-pathnames "20260101T101010--a-note.org" dir))
    (alexandria:write-string-into-file (format nil "* Other~%") (merge-pathnames "20260202T101010--other-note__work.org" dir))
    (alexandria:write-string-into-file (format nil "* Plan~%") (merge-pathnames "20260303T101010--plan__work.org" dir))
    (let ((tuatara-user::*formatted-file-directory* (namestring dir)))
      (multiple-value-bind (f editor)
          (run-keys "M-x o r g - m o d e RET RET" :text (format nil "[[denote:query-filenames:note][note]]~%") :width 120 :height 16)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Files matching note:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "other-note"))
        (false (tuatara.frontend.fake:fake-frame-showing f "plan")))
      (multiple-value-bind (line name)
          (org-line-of-point (nth-value 1 (run-keys "M-x o r g - m o d e RET RET o t h e r RET"
                                                    :text (format nil "[[denote:query-filenames:note][note]]~%") :width 120 :height 16)))
        (declare (ignore line))
        (is string= "20260202T101010--other-note__work.org" name))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "M-x o r g - m o d e RET RET" :text (format nil "[[denote:query-filenames:nothing][x]]~%") :width 300 :height 16)
             "Files matching nothing (none in"))
      ;; the files the text typed matches listed as it is typed; RET takes the text, a file selected or not
      (let ((f (run-keys "M-x m a t c h - d e n o t e - f i l e s - l i n k RET _ w o r k" :text (format nil "~%") :width 120 :height 16)))
        (true (tuatara.frontend.fake:fake-frame-showing f "other-note"))
        (true (tuatara.frontend.fake:fake-frame-showing f "plan"))
        ;; before _work was typed every file was listed; now a-note is not
        (false (some (lambda (row) (search "a-note" row)) (first (tuatara.frontend.fake::fake-history f)))))
      (is string= (format nil "[[denote:query-filenames:plan][plan]]~%")
          (session-text (nth-value 1 (run-keys "M-x m a t c h - d e n o t e - f i l e s - l i n k RET p l a n C-n RET RET" :text (format nil "~%") :width 120 :height 16))))
      ;; the label asked for, what is matched offered selected: taken with RET, or typed over
      (is string= (format nil "[[denote:query-filenames:_work][Work notes]]~%")
          (session-text (nth-value 1 (run-keys "M-x m a t c h - d e n o t e - f i l e s - l i n k RET _ w o r k RET W o r k SPC n o t e s RET" :text (format nil "~%") :width 120 :height 16))))
      (is string= (format nil "[[denote:query-filenames:_work][_work]]~%")
          (session-text (nth-value 1 (run-keys "M-x m a t c h - d e n o t e - f i l e s - l i n k RET _ w o r k RET RET" :text (format nil "~%") :width 120 :height 16)))))
    ;; our own: formatted-match:, a regular expression, ignoring case unless it has a capital
    (let ((tuatara-user::*formatted-file-directory* (namestring dir))
          (ours (lambda (link keys)
                  (run-keys (format nil "M-x o r g - m o d e RET RET ~a" keys) :text (format nil "~a~%" link) :width 120 :height 16))))
      (let ((f (funcall ours "[[formatted-match:_work][_work]]" "")))
        (true (tuatara.frontend.fake:fake-frame-showing f "Files matching _work:"))
        (true (tuatara.frontend.fake:fake-frame-showing f "other-note"))
        (true (tuatara.frontend.fake:fake-frame-showing f "plan"))
        (false (tuatara.frontend.fake:fake-frame-showing f "a-note")))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "M-x o r g - m o d e RET RET" :text (format nil "[[formatted-match:_Work][x]]~%") :width 300 :height 16)
             "Files matching _Work (none in"))
      (true (tuatara.frontend.fake:fake-frame-showing (funcall ours "[[formatted-match:(][x]]" "") "( is no regular expression"))
      ;; brackets escaped in the link, as Org escapes them
      (multiple-value-bind (line name)
          (org-line-of-point (nth-value 1 (funcall ours "[[formatted-match:2026020\\[0-9\\]][x]]" "RET")))
        (declare (ignore line))
        (is string= "20260202T101010--other-note__work.org" name))
      (is string= (format nil "[[formatted-match:_work][_work]]~%")
          (session-text (nth-value 1 (run-keys "M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET _ w o r k RET RET" :text (format nil "~%") :width 120 :height 16))))
      (is string= (format nil "[[formatted-match:2026\\[0-9\\]][matching files]]~%")
          (session-text (nth-value 1 (run-keys "M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET 2 0 2 6 [ 0 - 9 ] RET RET" :text (format nil "~%") :width 120 :height 16))))
      ;; DEL clears the label offered: a link with none
      (is string= (format nil "[[formatted-match:_work]]~%")
          (session-text (nth-value 1 (run-keys "M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET _ w o r k RET DEL RET" :text (format nil "~%") :width 120 :height 16))))
      ;; the label offered is shown selected
      (let ((f (run-keys "M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET _ w o r k RET" :text (format nil "~%") :width 120 :height 16)))
        (multiple-value-bind (screen spans) (tuatara.frontend.fake::fake-frame-with f "Link LABEL: _work")
          (let* ((row (position-if (lambda (line) (search "Link LABEL: _work" line)) screen))
                 (start (+ (search "Link LABEL: " (aref screen row)) (length "Link LABEL: "))))
            (true (find-if (lambda (span) (and (= (first span) start) (eq (third span) :region))) (aref spans row))))))
      ;; the text selected becomes the link, its label; what is matched still asked for
      (is string= (format nil "See [[formatted-match:_work][my work notes]] here.~%")
          (session-text (nth-value 1 (run-keys "C-u 4 l C-SPC C-u 1 3 l M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET _ w o r k RET"
                                               :text (format nil "See my work notes here.~%") :width 120 :height 16))))
      ;; a label that would end the link early is refused
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET _ w o r k RET DEL a ] RET" :text (format nil "~%") :width 120 :height 16)
             "would end the link early"))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "M-x m a t c h - f o r m a t t e d - f i l e s - l i n k RET ( RET" :text (format nil "~%") :width 120 :height 16)
             "No link inserted: ( is no regular expression")))
    (dolist (name '("20260202T101010--other-note__work.org"))
      (let ((buffer (tuatara.buffer:find-buffer name)))
        (when buffer (tuatara.buffer:kill-buffer buffer)))))
  ;; a web address: the desktop's program, after asking
  (with-test-directory (dir)
    (with-stand-in-opener (record dir)
      (let ((f (run-keys "M-x o r g - m o d e RET RET y RET" :text (format nil "[[https://orgmode.org][Org]]~%") :width 80 :height 12)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Open https://orgmode.org in the desktop's program, outside Tuatara?"))
        (is equal '("https://orgmode.org") (opened-names record)))
      ;; or the program *open-externally-programs* gives its scheme
      (delete-file record)
      (let ((tuatara-user:*open-externally-programs*
              `(("x-scheme-handler/https" ,tuatara-user:*open-externally-program* "--new-tab")))
            (tuatara-user:*open-externally-program* "no-such-xdg-open"))
        (let ((f (run-keys "M-x o r g - m o d e RET RET y RET" :text (format nil "[[https://orgmode.org][Org]]~%") :width 80 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Open https://orgmode.org in stand-in-xdg-open, outside Tuatara?"))
          (is equal '("--new-tab" "https://orgmode.org") (opened-names record)))))))

(define-test (org link-targets-alike)
  ;; following a link and exporting it lead to the same place: a target before a
  ;; name before a headline of the same words; *Title a headline only; #id a custom id
  (is equal '((:target . "Kea") (:name . "Kea") (:title . "Kea")) (tuatara.defaults::org-link-target-kinds "Kea"))
  (is equal '((:title . "Kea")) (tuatara.defaults::org-link-target-kinds "*Kea"))
  (is equal '((:custom-id . "kea")) (tuatara.defaults::org-link-target-kinds "#kea"))
  (let ((text (format nil "Go [[Kea]] and [[*Kea]].~%* Kea~%:PROPERTIES:~%:CUSTOM_ID: kea~%:END:~%Text <<Kea>> here.~%#+NAME: Kea~%| a |~%* Tui~%")))
    (flet ((after (keys) (org-line-of-point (nth-value 1 (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 80 :height 12)))))
      (is string= "Text <<Kea>> here." (after "C-u 5 l RET"))
      (is string= "* Kea" (after "C-u 1 5 l RET")))
    (let* ((latex (tuatara.defaults::org-latex-document text :enhanced t))
           (to-target (cl-ppcre:scan-to-strings "(?<=\\\\hyperref\\[)[^]]+(?=\\]\\{Kea\\})" latex))
           (to-headline (cl-ppcre:scan-to-strings "(?<=\\\\autoref\\{)[^}]+" latex)))
      ;; [[Kea]] to the target's label, [[*Kea]] to the headline's
      (true (search (format nil "Text \\label{~a} here." to-target) latex))
      (true (search (format nil "\\section{Kea}~%\\label{~a}" to-headline) latex)))))

(define-test (org link-escapes)
  ;; brackets in a link's target are escaped with backslashes, as Org escapes them, and read back
  (is string= "2026\\[0-9\\]{2}" (tuatara.defaults::org-link-escape "2026[0-9]{2}"))
  (is string= "a\\\\\\]b" (tuatara.defaults::org-link-escape "a\\]b"))
  (is string= "end\\\\" (tuatara.defaults::org-link-escape "end\\"))
  (is string= "x\\d+" (tuatara.defaults::org-link-escape "x\\d+"))
  (dolist (s '("2026[0-9]{2}" "a\\]b" "end\\" "x\\d+" "_(work|home)"))
    (is string= s (tuatara.defaults::org-link-unescape (tuatara.defaults::org-link-escape s))))
  (let ((link (find 'tuatara.defaults::link (tuatara.defaults::org-node-children
                                              (first (tuatara.defaults::org-node-children
                                                      (first (tuatara.defaults::org-node-children
                                                              (tuatara.defaults::org-parse (format nil "[[m:20\\[0-9\\]+][x]]~%")))))))
                    :key #'tuatara.defaults::org-node-type)))
    (is string= "m:20[0-9]+" (tuatara.defaults::org-property link :path))))

(define-test (org link-display)
  (let ((text (format nil "See [[https://orgmode.org][Org]] and [[*Two]] here.~%* Two~%")))
    (flet ((first-line (keys &optional (text text))
             (tuatara.frontend.fake:fake-screen-line (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 80 :height 12) 0)))
      ;; links shown as their labels, as Org shows them; SPC t L as written, and back
      (is string= "See Org and *Two here." (string-right-trim " " (first-line "")))
      (is string= "See [[https://orgmode.org][Org]] and [[*Two]] here." (string-right-trim " " (first-line "SPC t L")))
      (is string= "See Org and *Two here." (string-right-trim " " (first-line "SPC t L SPC t L")))
      ;; typing keeps them shown as their labels
      (is string= "xySee Org and *Two here." (string-right-trim " " (first-line "i x y")))
      ;; #+STARTUP: literallinks shows them as written
      (true (search "[[*Two]]" (first-line "" (format nil "[[*Two]]~%#+STARTUP: literallinks~%* Two~%"))))))
  ;; a large text, parsed in the background: while its parse catches up with what is typed, the
  ;; links stay shown as their labels, where the typing has moved them
  (let ((tuatara.defaults::*org-parse-at-once* 10)
        (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 12))
        (start (get-internal-real-time))
        (typed-frame nil))
    (labels ((wait-for-labels ()
               (if (and (not (search "See Org" (tuatara.frontend.fake:fake-screen-line f 0)))
                        (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'wait-for-labels))
                   (progn (tuatara.frontend.fake:fake-queue-keys f "i x")
                          (tuatara.frontend.fake:fake-queue-action
                           f (lambda () (setf typed-frame (tuatara.frontend.fake:fake-screen-line f 0))))))))
      (tuatara.frontend.fake:fake-queue-action f #'wait-for-labels))
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                  :buffer (let ((b (tuatara.buffer:make-buffer "links.org" :text (format nil "See [[https://orgmode.org][Org]] here.~%"))))
                                            (tuatara.defaults::set-major-mode b 'tuatara.defaults::org-mode)
                                            b))
    (is string= "xSee Org here." (string-right-trim " " typed-frame)))
  ;; point moved into a hidden part goes past it the way it was going; the text is not changed
  (let ((text (format nil "@See [[https://orgmode.org][Org]] here.~%")))
    (is string= (format nil "See [[https://orgmode.org][@Org]] here.~%") (org-table-after "C-u 4 l" text))
    (is string= (format nil "See [[https://orgmode.org][Org]]@ here.~%") (org-table-after "C-u 4 l l l l" text))
    (is string= (format nil "See [[https://orgmode.org][Or@g]] here.~%") (org-table-after "C-u 4 l l l l h" text))
    (is string= (format nil "See@ [[https://orgmode.org][Org]] here.~%") (org-table-after "C-u 4 l h" text))))

(define-test (org entity-display)
  ;; an entity shown as its character, as a Word document's $ is written, with the links shown as
  ;; their labels; SPC t L shows both as written; the text is not changed
  (let ((text (format nil "Paid \\dollar{}5,950.00 to \\alpha{}, [[x][Org]].~%")))
    (flet ((shown (keys)
             (let ((f (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 80 :height 12)))
               (values (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0))
                       (tuatara.frontend.fake:fake-cursor f)))))
      (is string= "Paid $5,950.00 to α, Org." (shown ""))
      (is string= "Paid \\dollar{}5,950.00 to \\alpha{}, [[x][Org]]." (shown "SPC t L"))
      ;; point rests on the character, at the entity's start
      (is equal '(0 . 5) (nth-value 1 (shown "C-u 5 l")))
      (is equal '(0 . 6) (nth-value 1 (shown "C-u 5 l l")))))
  ;; point moved into the rest of an entity goes past it, or back onto its character
  (let ((text (format nil "@Paid \\dollar{}5 now.~%")))
    (is string= (format nil "Paid @\\dollar{}5 now.~%") (org-table-after "C-u 5 l" text))
    (is string= (format nil "Paid \\dollar{}@5 now.~%") (org-table-after "C-u 5 l l" text))
    (is string= (format nil "Paid @\\dollar{}5 now.~%") (org-table-after "C-u 5 l l h" text))
    (is string= (format nil "Paid \\dollar{}5 now.~%") (remove #\@ (org-table-after "C-u 5 l l h" text)))))

(define-test (org link-display-kept-while-wrapping)
  ;; the window's hidden parts are kept while soft wrap finds where rows start, one line at a time
  (let* ((text (format nil "A [[https://a.org][one]] line.~%B [[https://b.org][two]] line.~%C [[https://c.org][three]] line.~%"))
         (b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "wrap.org") :text text)))
    (tuatara.defaults::set-major-mode b 'tuatara.defaults::org-mode)
    (let* ((size (tuatara.buffer:buffer-size b))
           (second-line (1+ (position #\Newline text)))
           (second-end (position #\Newline text :start second-line))
           (all (tuatara.defaults::org-hidden-ranges b 0 size))
           (kept (first (tuatara.buffer:buffer-local b 'tuatara.defaults::org-hidden))))
      (is = 6 (length all))
      ;; a part of what is kept: those of it there, what is kept unchanged
      (let ((tuatara.display:*layout-probe* t))
        (is = 2 (length (tuatara.defaults::org-hidden-ranges b second-line second-end))))
      (is equal kept (first (tuatara.buffer:buffer-local b 'tuatara.defaults::org-hidden)))
      ;; a row's start looked for elsewhere: found, not kept in place of the window's
      (let ((tuatara.display:*layout-probe* t))
        (is = 4 (length (tuatara.defaults::org-hidden-ranges b second-line size))))
      (is equal kept (first (tuatara.buffer:buffer-local b 'tuatara.defaults::org-hidden)))
      (tuatara.buffer:kill-buffer b)))
  ;; soft wrap on: the labels shown as the cursor moves over a long line
  (let ((line (format nil "- see [[https://example.org][the page]] ~{~a~^ ~}~%" (loop repeat 20 collect "words that wrap"))))
    (multiple-value-bind (f editor)
        (run-keys "M-x o r g - m o d e RET M-x s o f t - w r a p - m o d e RET j j k" :text (format nil "~a~a~a" line line line) :width 60 :height 20)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "- see the page words")))))

(define-test (org ids-in-other-notes)
  ;; id: to a headline of another note: looked for in the background, a note open
  ;; as its buffer has it, one not open read from disk without changing its access time
  (with-test-directory (dir)
    (let ((tuatara-user::*formatted-file-directory* (namestring dir))
          (other (namestring (merge-pathnames "20260101T101010--other.org" dir)))
          (open-one (namestring (merge-pathnames "20260202T101010--open.org" dir))))
      (alexandria:write-string-into-file (format nil "* First~%* Wanted~%:PROPERTIES:~%:ID: far-1~%:END:~%") other)
      (alexandria:write-string-into-file (format nil "* Open~%") open-one)
      (unwind-protect
           (flet ((follow (link)
                    (run-keys "M-x o r g - m o d e RET RET" :text (format nil "[[~a]]~%" link) :width 120 :height 12)))
             (multiple-value-bind (line name) (org-line-of-point (nth-value 1 (follow "id:far-1")))
               (is string= "20260101T101010--other.org" name)
               (is string= "* Wanted" line))
             (forget-file-buffers)
             ;; an ID only in an open buffer, not saved yet: found there
             (let ((buffer (tuatara.io:visit-file open-one)))
               (tuatara.defaults::set-major-mode buffer 'tuatara.defaults::org-mode)
               (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer)
                                             (format nil "* Unsaved~%:PROPERTIES:~%:ID: near-2~%:END:~%")))
             (multiple-value-bind (line name) (org-line-of-point (nth-value 1 (follow "id:near-2")))
               (is string= "20260202T101010--open.org" name)
               (is string= "* Unsaved" line))
             ;; none has it: said, the notes read without changing when they were last read
             (forget-file-buffers)
             (sb-posix:utimes other 1000000000 1500000000)
             (true (tuatara.frontend.fake:fake-frame-showing (follow "id:nowhere") "No match for id:nowhere"))
             (is = 1000000000 (sb-posix:stat-atime (sb-posix:stat other))))
        (forget-file-buffers)))))

(define-test (org pickers-list-in-background)
  ;; the notes offered for a link, a query, a sequence, and a new file's
  ;; subdirectories and keywords are found away from the editor's thread
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "#+title: A Fine Note~%~%Body~%") (merge-pathnames "20260101T101010--a-fine-note__work.org" dir))
    (alexandria:write-string-into-file (format nil "* Seq~%") (merge-pathnames "20260102T101010==1--seq__work.org" dir))
    (ensure-directories-exist (merge-pathnames "sub/" dir))
    (let ((tuatara-user::*formatted-file-directory* (namestring dir))
          (tuatara.defaults::*formatted-file-subdirectory-history* '())
          (walks '())
          (here sb-thread:*current-thread*))
      (sb-int:encapsulate 'tuatara.defaults::files-under 'walk-spy
                          (lambda (function &rest arguments)
                            (push sb-thread:*current-thread* walks)
                            (apply function arguments)))
      (unwind-protect
           (progn
             ;; RET waits for the notes listed; what is typed meanwhile (the label's RET) follows it
             (true (search "[[denote:20260101T101010][A Fine Note]]"
                           (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET C-c C-l d e n o t e : RET f i n e RET RET" :text "" :width 120 :height 16)))))
             (is string= (format nil "[[denote:query-filenames:fine][fine]]~%")
                 (session-text (nth-value 1 (run-keys "M-x m a t c h - d e n o t e - f i l e s - l i n k RET f i n e RET RET" :text (format nil "~%") :width 120 :height 16))))
             (multiple-value-bind (line name)
                 (org-line-of-point (nth-value 1 (run-keys "M-x o r g - m o d e RET RET RET" :text (format nil "[[denote:query-filenames:fine][x]]~%") :width 120 :height 16)))
               (declare (ignore line))
               (is string= "20260101T101010--a-fine-note__work.org" name))
             (true (search "#+BEGIN: formatted-sequence :sequence \"1\" :depth 2"
                           (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET M-x i n s e r t - f o r m a t t e d - s e q u e n c e - b l o c k RET RET RET"
                                                                :text "" :width 120 :height 16)))))
             (let ((made (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window
                                                                                     (nth-value 1 (run-keys "SPC d n T i t l e RET RET w o r k RET RET RET" :width 120 :height 16)))))))
               (true (search "--title__work" made) "~a" made))
             (true walks)
             (false (member here walks)))
        (sb-int:unencapsulate 'tuatara.defaults::files-under 'walk-spy)
        (forget-file-buffers)))))

(define-test (org insert-link)
  (with-test-directory (dir)
    (let ((org (merge-pathnames "a.org" dir))
          (notes (merge-pathnames "sub/notes.txt" dir))
          (tuatara.defaults::*org-stored-links* '()))
      (ensure-directories-exist notes)
      (alexandria:write-string-into-file (format nil "* First~%Text~%* Second heading~%:PROPERTIES:~%:CUSTOM_ID: sec~%:END:~%more~%") org)
      (alexandria:write-string-into-file (format nil "line one~%the second line~%") notes)
      (flet ((in-file (path keys) (nth-value 1 (run-keys (format nil "C-x C-f C-a C-k ~a RET ~a" (rimu-typed (namestring path)) keys) :width 120 :height 20)))
             (forget () (dolist (name '("a.org" "notes.txt"))
                          (let ((b (tuatara.buffer:find-buffer name)))
                            (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b))))))
        ;; stored under a headline, put in its own file: a link within it, the title its label; used up;
        ;; RET with nothing typed takes the link stored last
        (let ((editor (in-file org "j M-x o r g - s t o r e - l i n k RET g e i C-c C-l RET RET")))
          (true (search "[[*First][First]]" (session-text editor)))
          (is eq nil tuatara.defaults::*org-stored-links*))
        (forget)
        ;; by its CUSTOM_ID
        (let ((editor (in-file org "C-u 6 j M-x o r g - s t o r e - l i n k RET g e i C-c C-l RET RET")))
          (true (search "[[#sec][Second heading]]" (session-text editor))))
        (forget)
        ;; a line of another file, from this file's folder
        (in-file notes "j M-x o r g - s t o r e - l i n k RET")
        (is equal "the second line" (subseq (first (first tuatara.defaults::*org-stored-links*))
                                            (- (length (first (first tuatara.defaults::*org-stored-links*))) 15)))
        (let ((editor (in-file org "g e i C-c C-l RET RET")))
          (true (search "[[file:sub/notes.txt::the second line]]" (session-text editor))))
        (forget)
        ;; C-u: a file, asked as find-file asks; an empty label, none
        (let ((editor (in-file org (format nil "g e i C-u C-c C-l C-a C-k ~a RET DEL RET" (rimu-typed (namestring notes))))))
          (true (search "[[file:sub/notes.txt]]" (session-text editor))))
        (forget)
        ;; a web address typed, the text selected its label
        (let ((editor (in-file org "j C-a C-SPC C-e C-c C-l h t t p s : / / o r g m o d e . o r g RET")))
          (true (search (format nil "[[https://orgmode.org][Text]]~%") (session-text editor))))
        (forget)
        ;; denote: one of your notes chosen, its #+title offered as the label
        (alexandria:write-string-into-file (format nil "#+title: A Fine Note~%~%Body~%") (merge-pathnames "20260101T101010--a-fine-note__work.org" dir))
        (let ((tuatara-user::*formatted-file-directory* (namestring dir)))
          (let ((editor (in-file org "g e i C-c C-l d e n o t e : RET RET RET")))
            (true (search "[[denote:20260101T101010][A Fine Note]]" (session-text editor)))))
        (forget)
        ;; on a link: its link and label offered, both changed
        (alexandria:write-string-into-file (format nil "X [[https://a.org][old]] Y~%") org :if-exists :supersede)
        (let ((editor (in-file org "C-u 4 l C-c C-l C-a C-k h t t p s : / / b . o r g RET n e w RET")))
          (is string= (format nil "X [[https://b.org][new]] Y~%") (session-text editor)))
        (forget)))))

(define-test (org links-to-programs-kept-safe)
  ;; a mail link never has the mail program attach a file; numbers in matches read without the Lisp reader
  (is string= "mailto:a@b" (tuatara.defaults::mail-address-without-attachments "mailto:a@b?attach=~/.ssh/id_rsa"))
  (is string= "mailto:a@b?subject=Hi&cc=c@d"
      (tuatara.defaults::mail-address-without-attachments "mailto:a@b?subject=Hi&Attachment=/etc/passwd&cc=c@d"))
  (is string= "mailto:a@b?subject=Hi" (tuatara.defaults::mail-address-without-attachments "mailto:a@b?subject=Hi"))
  (is string= "https://x.org/?attach=1" (tuatara.defaults::mail-address-without-attachments "https://x.org/?attach=1"))
  ;; a name written in %XX, in any case, is the name the mail program reads
  (is string= "mailto:a@b" (tuatara.defaults::mail-address-without-attachments "mailto:a@b?%61ttach=~/.ssh/id_rsa"))
  (is string= "mailto:a@b?subject=Hi"
      (tuatara.defaults::mail-address-without-attachments "mailto:a@b?subject=Hi&%41TTACH%4dent=/etc/passwd&att%61ch=x"))
  (is string= "mailto:a@b?%73ubject=Hi" (tuatara.defaults::mail-address-without-attachments "mailto:a@b?%73ubject=Hi"))
  (true (tuatara.defaults::org-match-compare ">" "10" "9"))
  (true (tuatara.defaults::org-match-compare "=" "1.5" "1.50"))
  (false (tuatara.defaults::org-match-compare ">" "abc" "9"))
  (false (tuatara.defaults::org-match-compare ">" "1e30000000" "9")))
