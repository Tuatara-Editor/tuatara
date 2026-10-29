;;;; test/org-view.lisp --- Org's view and outline: folding, the indented view, editing the outline, lists and checkboxes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (org folding)
  ;; the view as written, no signs, the indented one turned on by hand
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    ;; TAB on a headline folds its subtree: the headline alone, an ellipsis after it
    (let ((lines (org-screen "TAB")))
      (is equal '("* One..." "* Two" "Text of two." "") (subseq lines 0 4)))
    ;; again: its own text and its children's headlines, their text folded, as Org shows them
    (let ((lines (org-screen "TAB TAB")))
      (is equal '("* One" "Text of one." "** One A..." "** One B...") (subseq lines 0 4)))
    ;; again: all of it
    (let ((lines (org-screen "TAB TAB TAB")))
      (is equal '("* One" "Text of one." "** One A" "Text of one A.") (subseq lines 0 4)))
    ;; and folded again
    (is equal '("* One..." "* Two") (subseq (org-screen "TAB TAB TAB TAB") 0 2))
    ;; S-TAB: the overview, then the contents, then everything
    (is equal '("* One..." "* Two..." "") (subseq (org-screen "S-TAB") 0 3))
    (is equal '("* One..." "** One A..." "** One B..." "* Two...") (subseq (org-screen "S-TAB S-TAB") 0 4))
    (is equal '("* One" "Text of one.") (subseq (org-screen "S-TAB S-TAB S-TAB") 0 2))
    ;; j over a folded subtree goes to the next headline; k back to the folded one
    (flet ((line-at-point (editor)
             (let* ((buffer (session-buffer editor)) (rope (tuatara.buffer:buffer-text buffer))
                    (point (tuatara.buffer:buffer-point buffer)))
               (tuatara.text:octets-string (tuatara.text:rope-octets rope :start (tuatara.text:rope-line-start rope point)
                                                                           :end (tuatara.text:rope-line-end rope point))))))
      (is string= "* Two" (line-at-point (nth-value 1 (org-screen "TAB j"))))
      (is string= "* One" (line-at-point (nth-value 1 (org-screen "TAB j k"))))
      ;; a search ending in folded text opens what hides the match, the other folds kept
      (multiple-value-bind (lines editor) (org-screen "S-TAB C-s o f SPC o n e SPC b RET")
        (is string= "Text of one B." (line-at-point editor))
        (true (member "Text of one B." lines :test #'string=))
        (true (member "* Two..." lines :test #'string=))))
    ;; the text itself is never changed by folding
    (multiple-value-bind (lines editor) (org-screen "TAB TAB S-TAB")
      (declare (ignore lines))
      (is string= *org-folding* (session-text editor)))
    ;; #+STARTUP: overview opens folded
    (is equal '("#+STARTUP: overview" "* One..." "* Two...")
        (subseq (org-screen "" :text (format nil "#+STARTUP: overview~%~a" *org-folding*)) 0 3))))

(defparameter *org-two-folded* (format nil "* One~%body one~%* Two~%body two~%")
  "Two headlines with a line of text each, folded by S-TAB to the headlines alone.")

(define-test (org stale-fold-opens)
  ;; a folded heading whose stars are deleted is a heading no more: what it hid is shown again
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    (flet ((shown (keys) (subseq (org-screen (format nil "S-TAB ~a" keys) :text *org-two-folded* :height 8) 0 4)))
      (is equal '(" One" "body one" "* Two..." "") (shown "d"))
      (is equal '("One" "body one" "* Two..." "") (shown "d d"))
      (is equal '("*One" "body one" "* Two..." "") (shown "l d"))
      ;; TAB there then indents, as on any line
      (is equal '("     One" "body one" "* Two..." "") (shown "d TAB"))
      ;; a heading still: its fold stays
      (is equal '("* One!..." "* Two..." "" "") (shown "A ! C-g"))
      (is equal '("** One..." "* Two..." "" "") (shown "i * C-g"))
      ;; undone, the heading folds again with TAB
      (is equal '("* One..." "* Two..." "" "") (shown "d u TAB")))))

(define-test (org folded-text-edited-seen)
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    (flet ((after (keys &optional (text *org-two-folded*))
             (multiple-value-bind (lines editor) (org-screen (format nil "S-TAB ~a" keys) :text text :height 8)
               (values (session-text editor) lines editor))))
      ;; V and its J and K take a folded headline with all it hides, as one line
      (is string= (format nil "* Two~%body two~%") (after "V d"))
      (is string= "" (after "V J d"))
      (is string= "" (after "j V K d"))
      (is string= (format nil "* Three~%") (after "V J d" (format nil "* One~%body one~%* Two~%body two~%* Three~%")))
      (is string= (format nil "* Three~%") (after "C-u 2 V d" (format nil "* One~%body one~%* Two~%body two~%* Three~%")))
      (is string= (format nil "* One~%body one~%") (after "j V d"))
      ;; one undo brings it all back
      (is string= *org-two-folded* (after "C-u 2 V d u"))
      ;; j and k step over what a fold hides, a count the lines shown
      (is = 15 (tuatara.buffer:buffer-point (session-buffer (nth-value 2 (after "j")))))
      (is = 0 (tuatara.buffer:buffer-point (session-buffer (nth-value 2 (after "j k")))))
      (is = 30 (tuatara.buffer:buffer-point (session-buffer (nth-value 2 (after "j j" (format nil "* One~%body one~%* Two~%body two~%* Three~%"))))))
      ;; t and T put the new cursor on the next line shown, never on a hidden one
      (is string= (format nil "X* One~%body one~%X* Two~%body two~%") (after "t i X C-g"))
      (is string= (format nil "X* One~%body one~%X* Two~%body two~%") (after "j T i X C-g"))
      ;; a deletion that would take only part of the hidden text is refused, the fold shown
      (dolist (keys '("g l d" "l l l l C-u 3 d" "D" "g l i C-d C-g" "j i C-u 2 DEL C-g" "g l C-k"))
        (multiple-value-bind (text lines) (after keys)
          (is string= *org-two-folded* text "~a deletes nothing" keys)
          (is equal '("* One" "body one" "* Two...") (subseq lines 0 3) "~a shows the fold" keys)))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "M-x o r g - m o d e RET S-TAB g l d" :text *org-two-folded* :width 80)
             "That would delete hidden text"))
      ;; shown, the same key deletes what can now be seen
      (is string= (format nil "* Onebody one~%* Two~%body two~%") (after "g l d d"))
      (is string= (format nil "~%body one~%* Two~%body two~%") (after "D D"))
      ;; what does not reach into hidden text goes ahead, the folds kept
      (is string= (format nil "* On~%body one~%* Two~%body two~%") (after "g l h d"))
      (multiple-value-bind (text lines) (after "A X C-g")
        (is string= (format nil "* OneX~%body one~%* Two~%body two~%") text)
        (is equal '("* OneX..." "* Two...") (subseq lines 0 2))))))

(define-test (org folded-text-kept-at-many-places)
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    (flet ((after (keys)
             (multiple-value-bind (lines editor) (org-screen keys :text *org-two-folded* :height 8)
               (values (session-text editor) lines))))
      ;; f on the line shown holds the match seen, never the one the fold hides
      (multiple-value-bind (text lines) (after "S-TAB f o n e RET d")
        (is string= (format nil "* ~%body one~%* Two~%body two~%") text)
        (is equal '("* ..." "* Two...") (subseq lines 0 2)))
      ;; over the whole text too, the match the fold hides left alone (more in
      ;; matches-in-folds)
      (dolist (keys '("S-TAB % f o n e RET d" "S-TAB % f o n e RET c"))
        (multiple-value-bind (text lines) (after keys)
          (is string= (format nil "* ~%body one~%* Two~%body two~%") text "~a deletes the one shown" keys)
          (is equal '("* ..." "* Two...") (subseq lines 0 2) "~a keeps the fold" keys)))
      ;; m l on a folded headline banks it with all it hides
      (is string= (format nil "* Two~%body two~%") (after "S-TAB m l d"))
      (is string= "" (after "S-TAB m l j m l d"))
      ;; a line banked, then folded into its headline: the line shown is banked, all it hides with it
      (multiple-value-bind (text lines) (after "j m l S-TAB d")
        (is string= (format nil "* Two~%body two~%") text)
        (is equal '("* Two...") (subseq lines 0 1))))))

(define-test (org matches-in-folds)
  ;; f acts only on what is shown, whatever is searched, % too; r r and r q
  ;; act on matches hidden too, the folds kept; asking at
  ;; each, a match in folded text has its fold opened first
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    (flet ((after (keys &optional (text *org-two-folded*))
             (multiple-value-bind (f editor)
                 (run-keys (format nil "M-x o r g - m o d e RET S-TAB ~a" keys) :text text :width 100 :height 8)
               (values (session-text editor)
                       (loop for i below 2 collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f i)))
                       f)))
           (in-turns (turns)
             (multiple-value-bind (f editor)
                 (run-keys-in-turns (append (list (format nil "M-x o r g - m o d e RET S-TAB ~a" (first turns))) (rest turns) '(""))
                                    :text *org-two-folded* :width 100)
               (values (session-text editor) f
                       (loop for i below 2 collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f i)))))))
      ;; f, over the whole buffer or a selection of folded headlines: only the
      ;; match shown is a place, and every verb acts there alone
      (loop for (keys text shown)
              in '(("% f o n e RET d" "* ~%body one~%* Two~%body two~%" ("* ..." "* Two..."))
                   ("% f o n e RET c X C-g" "* X~%body one~%* Two~%body two~%" ("* X..." "* Two..."))
                   ("% f o n e RET i X C-g" "* XOne~%body one~%* Two~%body two~%" ("* XOne..." "* Two..."))
                   ("% f o n e RET a X C-g" "* OneX~%body one~%* Two~%body two~%" ("* OneX..." "* Two..."))
                   ;; a headline no more: its fold opens
                   ("% f o n e RET i DEL C-g" "*One~%body one~%* Two~%body two~%" ("*One" "body one"))
                   ("% f o n e RET w (" "* (One)~%body one~%* Two~%body two~%" ("* (One)..." "* Two..."))
                   ("f % o n e RET d" "* ~%body one~%* Two~%body two~%" ("* ..." "* Two..."))
                   ("V f o n e RET d" "* ~%body one~%* Two~%body two~%" ("* ..." "* Two..."))
                   ("V g l f o n e RET d" "* ~%body one~%* Two~%body two~%" ("* ..." "* Two..."))
                   ("V J f o RET d" "* ne~%body one~%* Tw~%body two~%" ("* ne..." "* Tw..."))
                   ("l v j l l l l f o RET d" "* ne~%body one~%* Tw~%body two~%" ("* ne..." "* Tw...")))
            do (multiple-value-bind (now lines f) (after keys)
                 (is string= (format nil text) now "~a acts at what is shown" keys)
                 (is equal shown lines "~a keeps the folds" keys)
                 (true (tuatara.frontend.fake:fake-frame-showing f "hidden in folds left alone") "~a says so" keys)))
      (multiple-value-bind (now lines f) (after "% f o n e RET d")
        (declare (ignore now lines))
        (true (tuatara.frontend.fake:fake-frame-showing f "Split: deleted 1 place; 1 hidden in folds left alone")))
      ;; V on the first headline, whose fold hides the rest of the text: the
      ;; selection is all of the text, yet only the line shown is acted on
      (let ((text (format nil "* One~%body one~%** One A~%one more")))
        (multiple-value-bind (now lines f) (after "V g l f o n e RET d" text)
          (is string= (format nil "* ~%body one~%** One A~%one more") now)
          (is equal '("* ..." "") lines)
          (true (tuatara.frontend.fake:fake-frame-showing f "3 hidden in folds left alone"))))
      ;; a match a fold hides is no place at all: nothing to hold
      (multiple-value-bind (now lines f) (after "% f b o d y RET")
        (is string= *org-two-folded* now)
        (is equal '("* One..." "* Two...") lines)
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing matched body; 2 hidden in folds left alone")))
      ;; r r and r q: hidden matches too, the folds kept folded
      (loop for (keys text shown)
              in '(("% r r o n e RET 1 RET" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("r r o n e RET 1 RET" "* 1~%body one~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("r r % o n e RET 1 RET" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("V r r o n e RET 1 RET" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("l v j l l l l r r % o RET 0 RET" "* 0ne~%b0dy 0ne~%* Tw0~%body two~%" ("* 0ne..." "* Tw0..."))
                   ("l v j l l l l r r o RET 0 RET" "* 0ne~%b0dy one~%* Tw0~%body two~%" ("* 0ne..." "* Tw0..."))
                   ("% r r o n e / 1 RET" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("% r r o n e / 1 RET" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("V r r o n e / 1 RET" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("r r o n e / 1 RET" "* 1~%body one~%* Two~%body two~%" ("* 1..." "* Two..."))
                   ("r q % o n e RET 1 RET !" "* 1~%body 1~%* Two~%body two~%" ("* 1..." "* Two...")))
            do (multiple-value-bind (now lines f) (after keys)
                 (is string= (format nil text) now "~a replaces the matches in its scope" keys)
                 (is equal shown lines "~a keeps the folds" keys)
                 (false (tuatara.frontend.fake:fake-frame-showing f "hidden in folds") "~a leaves none alone" keys)))
      ;; r q: asked at the hidden match, its fold
      ;; is shown first, the other left folded; y replaces it, n leaves it
      (loop for (keys answer text seen shown)
              in '(("r q % o n e RET 1 RET y" "y" "* 1~%body 1~%* Two~%body two~%" ("* 1" "body one" "* Two...") ("* 1" "body 1" "* Two..."))
                   ("r q % o n e RET 1 RET y" "n" "* 1~%body one~%* Two~%body two~%" ("* 1" "body one" "* Two...") ("* 1" "body one" "* Two..."))
                   ("r q % o n e RET 1 RET n" "y" "* One~%body 1~%* Two~%body two~%" ("* One" "body one" "* Two...") ("* One" "body 1" "* Two..."))
                   ("% r q o n e / 1 RET y" "y" "* 1~%body 1~%* Two~%body two~%" ("* 1" "body one" "* Two...") ("* 1" "body 1" "* Two...")))
            do (let ((asking nil))
                 (multiple-value-bind (f editor)
                     (run-keys (list (format nil "M-x o r g - m o d e RET S-TAB ~a" keys)
                                     (lambda ()
                                       (let ((frame (tuatara.command:editor-frontend tuatara.command::*editor*)))
                                         (setf asking (loop for i below 8
                                                            collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line frame i))))))
                                     answer)
                               :text *org-two-folded* :width 100 :height 8)
                   (is string= (format nil text) (session-text editor) "~a ~a" keys answer)
                   (is equal seen (subseq asking 0 3) "~a shows the hidden match" keys)
                   (true (find-if (lambda (line) (search "Query replacing" line)) asking) "~a asks" keys)
                   (is equal shown (loop for i below 3 collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f i)))
                       "~a ~a leaves the fold shown" keys answer))))
      ;; past the limit, out of sight in the background: the same
      (let ((tuatara-user:*split-limit* 1))
        (multiple-value-bind (now f lines) (in-turns '("% f o RET d"))
          (is string= (format nil "* ne~%body one~%* Tw~%body two~%") now)
          (is equal '("* ne..." "* Tw...") lines)
          (true (tuatara.frontend.fake:fake-frame-showing f "4 hidden in folds left alone")))
        (multiple-value-bind (now f lines) (in-turns '("l v j l l l l f o RET d"))
          (is string= (format nil "* ne~%body one~%* Tw~%body two~%") now)
          (is equal '("* ne..." "* Tw...") lines)
          (true (tuatara.frontend.fake:fake-frame-showing f "2 hidden in folds left alone"))))
      (let ((tuatara.defaults::*rewrite-threshold* 0))
        (multiple-value-bind (now f lines) (in-turns '("% f o n e RET" "d"))
          (is string= (format nil "* ~%body one~%* Two~%body two~%") now)
          (is equal '("* ..." "* Two...") lines)
          (true (tuatara.frontend.fake:fake-frame-showing f "1 hidden in folds left alone")))
        (loop for keys in '("% r r o n e RET 1 RET" "V r r o n e RET 1 RET" "V r r o n e / 1 RET")
              do (multiple-value-bind (now f lines) (in-turns (list keys))
                   (is string= (format nil "* 1~%body 1~%* Two~%body two~%") now "~a replaces both" keys)
                   (is equal '("* 1..." "* Two...") lines "~a keeps the folds" keys)
                   (false (tuatara.frontend.fake:fake-frame-showing f "hidden in folds") "~a leaves none alone" keys)))))))

(define-test (org indented-view-and-bullets)
  ;; the view as written, no signs, the indented one turned on by hand
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    ;; M-x org-indent-mode: each line indented by its headline's level, the stars but the last hidden
    (is equal '("* One" "  Text of one." " ** One A" "    Text of one A." " ** One B" "    Text of one B." "* Two" "  Text of two.")
        (subseq (org-screen "M-x o r g - i n d e n t - m o d e RET") 0 8))
    ;; the hidden stars in :org-hide, the text unchanged
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET M-x o r g - i n d e n t - m o d e RET j" :text *org-folding* :width 60 :height 12)
      (true (org-faces-include f 2 "*" :org-hide))
      (is string= *org-folding* (session-text editor))
      ;; the cursor after the indentation, on the line's first character
      (is equal '(1 . 2) (tuatara.frontend.fake:fake-cursor f)))
    ;; with bullets, a headline's last star shows as its level's sign
    (let ((tuatara-user:*org-bullets* '("◉" "○")))
      (is equal '("◉ One" "Text of one." "*○ One A") (subseq (org-screen "") 0 3))
      (is equal '("◉ One" "  Text of one." " *○ One A" "    Text of one A.")
          (subseq (org-screen "M-x o r g - i n d e n t - m o d e RET") 0 4)))
    ;; folded and indented together
    (is equal '("* One..." "* Two" "  Text of two.")
        (subseq (org-screen "M-x o r g - i n d e n t - m o d e RET TAB") 0 3))
    ;; #+STARTUP: indent opens indented
    (is equal '("#+STARTUP: indent" "* One" "  Text of one.")
        (subseq (org-screen "" :text (format nil "#+STARTUP: indent~%~a" *org-folding*)) 0 3))))

;; the fake frontend drawing rows large, as the window does
(defun call-with-large-level-one (function &optional (height 2))
  "Call FUNCTION with first-level headlines drawn HEIGHT rows tall, as the
window draws them with (set-face :org-level-1 :height 2)."
  (let ((face (tuatara.display:find-face :org-level-1))
        (tuatara.display:*row-scaling* t)
        (tuatara.display::*user-face-settings* (append tuatara.display::*user-face-settings* `((:org-level-1 :height ,height)))))
    (let ((old (tuatara.display::face-height face)))
      (unwind-protect (progn (tuatara.display:set-face-attributes :org-level-1 :height height)
                             (funcall function))
        (setf (tuatara.display::face-height face) old)))))

(define-test (org headlines-drawn-large)
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    (call-with-large-level-one
     (lambda ()
       ;; a first-level headline two rows tall, the rows under it moved down a row; a second-level one as it is
       (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET l l" :text *org-folding* :width 60 :height 12)
         (declare (ignore editor))
         (is equal '("* One" "Text of one." "** One A" "Text of one A.")
             (loop for i below 4 collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f i))))
         (is equal '((0 60 2 0 2)) (aref (tuatara.frontend.fake:fake-row-scales f) 0))
         (is equal '((0 60 1 1 1)) (aref (tuatara.frontend.fake:fake-row-scales f) 1))
         ;; the rows past those that fit left to the rows moved over them
         (is equal '((0 60 0 0 0)) (aref (tuatara.frontend.fake:fake-row-scales f) 9))
         ;; the cursor as wide as the characters
         (is equal '(0 . 4) (tuatara.frontend.fake:fake-cursor f)))
       (is equal '(1 . 0) (tuatara.frontend.fake:fake-cursor (run-keys "M-x o r g - m o d e RET j" :text *org-folding* :width 60 :height 12)))
       ;; the mouse on the large row, on the character drawn there
       (is = 2 (session-point (nth-value 1 (run-events '("M-x o r g - m o d e RET" (:down-mouse-1 4 0)) :text *org-folding* :width 60 :height 12))))
       (is = 3 (session-point (nth-value 1 (run-events '("M-x o r g - m o d e RET" (:down-mouse-1 7 0)) :text *org-folding* :width 60 :height 12))))
       (is = 12 (session-point (nth-value 1 (run-events '("M-x o r g - m o d e RET" (:down-mouse-1 6 1)) :text *org-folding* :width 60 :height 12))))
       ;; in truncate mode, point past the columns a large row holds scrolls the row sideways, never the window to the end
       (let ((f (run-keys "M-x o r g - m o d e RET C-e" :text (format nil "* ~a~%second~%" (make-string 50 :initial-element #\x))
                                                       :width 40 :height 8)))
         (is = 0 (car (tuatara.frontend.fake:fake-cursor f)))
         (true (< (cdr (tuatara.frontend.fake:fake-cursor f)) 40))
         ;; the window still at the top, the headline's end scrolled into sight
         (true (search "xxx" (tuatara.frontend.fake:fake-screen-line f 0))))
       ;; with line numbers, the gutter of a large row is a part of its own, as tall as the row's band
       (let ((f (run-keys "M-x o r g - m o d e RET M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET" :text *org-folding* :width 60 :height 12)))
         (let ((parts (aref (tuatara.frontend.fake:fake-row-scales f) 0)))
           (is = 2 (length parts))
           (is equal '(1 0 2) (cddr (first parts)))
           (is equal '(2 0 2) (cddr (second parts)))
           (is = (second (first parts)) (first (second parts)))))
       ;; recentring counts the heights of the large rows above point
       (let ((f (run-keys "M-x o r g - m o d e RET C-u 2 0 j C-l" :text (format nil "~{* Headline ~d~%~}end" (loop for i from 1 to 30 collect i))
                                                                  :width 60 :height 8)))
         (true (<= 1 (car (tuatara.frontend.fake:fake-cursor f)) 4)))
       ;; going down to the last of many large headlines: in sight, the window scrolled past the rest
       (let ((f (run-keys "M-x o r g - m o d e RET g e" :text (format nil "~{* Headline ~d~%~}end" (loop for i from 1 to 30 collect i))
                                                      :width 60 :height 8)))
         (is equal '(2 . 3) (tuatara.frontend.fake:fake-cursor f))
         (is string= "end" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 2))))))
    ;; a quarter as large again: the rows under it moved down a quarter of a row
    (call-with-large-level-one
     (lambda ()
       (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET l l l" :text *org-folding* :width 60 :height 12)
         (declare (ignore editor))
         (is equal '((0 60 5/4 0 5/4)) (aref (tuatara.frontend.fake:fake-row-scales f) 0))
         (is equal '((0 60 1 1/4 1)) (aref (tuatara.frontend.fake:fake-row-scales f) 1))
         (is equal '((0 60 5/4 1/4 5/4)) (aref (tuatara.frontend.fake:fake-row-scales f) 6))
         (is equal '((0 60 1 1/2 1)) (aref (tuatara.frontend.fake:fake-row-scales f) 7))
         (is equal '(0 . 4) (tuatara.frontend.fake:fake-cursor f))))
     1.25)
    ;; past the eighth level a headline's colours go round to the first's again, its size not
    (call-with-large-level-one
     (lambda ()
       (let ((f (run-keys "M-x o r g - m o d e RET" :text (format nil "* One~%********* Nine~%") :width 60 :height 12)))
         (is equal '((0 60 3/2 0 3/2)) (aref (tuatara.frontend.fake:fake-row-scales f) 0))
         (is equal '((0 60 1 1/2 1)) (aref (tuatara.frontend.fake:fake-row-scales f) 1))))
     1.5)
    ;; a terminal draws all rows alike
    (call-with-large-level-one
     (lambda ()
       (let ((tuatara.display:*row-scaling* nil))
         (is equal '("* One" "Text of one.")
             (subseq (org-screen "") 0 2)))))))

(define-test (org structure-editing)
  ;; the view as written, no signs, the indented one turned on by hand
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
    ;; M-j and M-k move the subtree point is in past its sibling, children with it
    (is string= (format nil "* Two~%Text of two.~%* One~%Text of one.~%** One A~%Text of one A.~%** One B~%Text of one B.~%")
        (org-text-after "M-j"))
    (is string= (format nil "* Two~%Text of two.~%* One~%Text of one.~%** One A~%Text of one A.~%** One B~%Text of one B.~%")
        (org-text-after "g e M-k"))
    (is string= (format nil "* One~%Text of one.~%** One B~%Text of one B.~%** One A~%Text of one A.~%* Two~%Text of two.~%")
        (org-text-after "j j M-j"))
    ;; not past the parent's end
    (is string= *org-folding* (org-text-after "j j j j M-j"))
    ;; M-h and M-l promote and demote the headline alone; M-H and M-L its subtree too
    (is string= (format nil "* One~%Text of one.~%* One A~%Text of one A.~%** One B~%Text of one B.~%* Two~%Text of two.~%")
        (org-text-after "j j M-h"))
    (is string= (format nil "** One~%Text of one.~%** One A~%Text of one A.~%** One B~%Text of one B.~%* Two~%Text of two.~%")
        (org-text-after "M-l"))
    (is string= (format nil "** One~%Text of one.~%*** One A~%Text of one A.~%*** One B~%Text of one B.~%* Two~%Text of two.~%")
        (org-text-after "M-L"))
    (is string= *org-folding* (org-text-after "M-h"))
    ;; C-c C-t: TODO, WAIT, then DONE noted with CLOSED: (with *org-log-done*), then none, the note gone
    (true (uiop:string-prefix-p (format nil "* TODO One~%Text of one.~%") (org-text-after "C-c C-t")))
    (true (uiop:string-prefix-p (format nil "* WAIT One~%Text of one.~%") (org-text-after "C-c C-t C-c C-t")))
    (let ((tuatara.defaults::*org-log-done* :time))
      (let ((done (org-text-after "C-c C-t C-c C-t C-c C-t")))
        (true (uiop:string-prefix-p (format nil "* DONE One~%CLOSED: [") done) "~a" done)
        (true (search (format nil "]~%Text of one.") done)))
      (is string= *org-folding* (org-text-after "C-c C-t C-c C-t C-c C-t C-c C-t")))
    ;; the document's own keywords, in their order
    (let ((text (format nil "#+TODO: TODO NEXT | DONE~%* Task~%")))
      (is string= (format nil "#+TODO: TODO NEXT | DONE~%* NEXT Task~%") (org-text-after "j C-c C-t C-c C-t" text)))
    ;; M-RET: a headline of this level after this subtree, point after its stars
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET j M-RET i N e w ESC" :text *org-folding* :width 60 :height 12)
      (declare (ignore f))
      (is string= (format nil "* One~%Text of one.~%** One A~%Text of one A.~%** One B~%Text of one B.~%* New~%* Two~%Text of two.~%")
          (session-text editor)))
    ;; M-S-RET: a task
    (true (search (format nil "~%* TODO ~%* Two") (org-text-after "M-S-RET")))
    ;; in a list, M-RET makes the next item, its number the next
    (is string= (format nil "- a~%- ~%- b~%") (org-text-after "M-RET" (format nil "- a~%- b~%")))
    (is string= (format nil "1. a~%2. ~%") (org-text-after "M-RET" (format nil "1. a~%")))
    ;; the numbered items after it numbered on, their ) kept
    (is string= (format nil "1) a~%2) ~%3) b~%4) c~%") (org-text-after "M-RET" (format nil "1) a~%2) b~%3) c~%")))
    ;; on the last item, at the very end of a text with no newline after it, still an item
    (is string= (format nil "* J~%- a~%- b~%- ") (org-text-after "g g j j g l M-RET" (format nil "* J~%- a~%- b")))
    ;; each one step to undo
    (is string= *org-folding* (org-text-after "M-j u"))
    ;; a folded subtree moved stays folded
    (is equal '("* Two" "Text of two." "* One...") (subseq (org-screen "TAB M-j") 0 3))))

(define-test (org checkboxes-and-templates)
  ;; ticking as such, nothing logged
  (let ((tuatara-user:*org-log-checkboxes* nil))
    (let ((text (format nil "* Tasks [0/2] [0%]~%- [ ] one~%- [ ] two~%  - [ ] sub a~%  - [ ] sub b~%")))
      ;; C-c C-c ticks the box of the item point is on, the headline's cookies brought up to date
      (is string= (format nil "* Tasks [1/2] [50%]~%- [X] one~%- [ ] two~%  - [ ] sub a~%  - [ ] sub b~%")
          (org-text-after "j C-c C-c" text))
      ;; and clears it again
      (is string= text (org-text-after "j C-c C-c C-c C-c" text))
      ;; an item's box with some of its items ticked shows [-], all of them [X]
      (is string= (format nil "* Tasks [0/2] [0%]~%- [ ] one~%- [-] two~%  - [X] sub a~%  - [ ] sub b~%")
          (org-text-after "j j j C-c C-c" text))
      (is string= (format nil "* Tasks [1/2] [50%]~%- [ ] one~%- [X] two~%  - [X] sub a~%  - [X] sub b~%")
          (org-text-after "j j j C-c C-c j C-c C-c" text)))
    ;; on a cookie, C-c C-c counts again
    (is string= (format nil "* T [1/2]~%- [X] a~%- [ ] b~%")
        (org-text-after "C-u 4 l C-c C-c" (format nil "* T [0/0]~%- [X] a~%- [ ] b~%")))
    ;; a headline with TODO sub-headlines counts them, on a cookie as when they change, its checkboxes left out
    (is string= (format nil "* T [1/3]~%- [X] a~%- [X] b~%** DONE x~%** TODO y~%** TODO z~%")
        (org-text-after "C-u 4 l C-c C-c" (format nil "* T [0/0]~%- [X] a~%- [X] b~%** DONE x~%** TODO y~%** TODO z~%")))
    (is string= (format nil "* T [0/1]~%- [X] a~%- [ ] b~%** TODO x~%")
        (org-text-after "j j C-c C-c" (format nil "* T [0/1]~%- [X] a~%- [X] b~%** TODO x~%")))
    ;; <s TAB: a source block, point where its language goes; <q TAB a quote, point inside
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET i < s TAB l i s p ESC" :width 60 :height 12)
      (declare (ignore f))
      (is string= (format nil "#+begin_src lisp~%~%#+end_src") (session-text editor)))
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET i < q TAB H i ESC" :width 60 :height 12)
      (declare (ignore f))
      (is string= (format nil "#+begin_quote~%Hi~%#+end_quote") (session-text editor)))
    ;; one's own snippet: <sh for a shell block
    (with-test-directory (dir)
      (ensure-directories-exist (merge-pathnames "org-mode/" dir))
      (alexandria:write-string-into-file (format nil "# name: Shell block~%# key: <sh~%# --~%#+begin_src shell~%$0~%#+end_src~%")
                                         (merge-pathnames "org-mode/shell" dir))
      (let ((tuatara-user:*snippet-directories* (list (namestring dir))))
        (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET i < s h TAB l s ESC" :width 60 :height 12)
          (declare (ignore f))
          (is string= (format nil "#+begin_src shell~%ls~%#+end_src") (session-text editor)))))))

(define-test (org leaving-org-mode)
  ;; the view Org gave the text goes with the mode: no indentation, signs, folds or large headlines in text mode
  (call-with-large-level-one
   (lambda ()
     (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET j j TAB g g M-x t e x t - m o d e RET" :text *org-folding* :width 60 :height 12)
       (let ((buffer (session-buffer editor)))
         (false (tuatara.buffer:buffer-local buffer :line-scale-function))
         (false (tuatara.buffer:buffer-local buffer :line-prefix-function))
         (false (tuatara.buffer:buffer-local buffer :display-replacements-function))
         (false (tuatara.buffer:buffer-local buffer :folds))
         (false (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-state))
         (false (aref (tuatara.frontend.fake:fake-row-scales f) 0))
         (is equal '("* One" "Text of one." "** One A" "Text of one A.")
             (loop for i below 4 collect (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f i)))))))))

(define-test (org cycle-without-parsing)
  ;; TAB on a headline of a large file, its parse not ready: the subtree found by its lines folds as the parse folds it
  (dolist (text (list (format nil "* A~%a text~%~%** A1~%one~%*** A1x~%deep~%** A2~%~%two~%~%* B~%b~%")
                      (format nil "intro~%* A~%*** skipped level~%x~%** then two~%y~%* B~%")
                      (format nil "* Empty~%* Only text~%  some~%~%~%* Last~%*bold* not a headline~%** child~%")
                      (format nil "* A~%** A1~%** A2~%")))
    (dolist (crlf '(nil t))
      (let* ((text (if crlf (cl-ppcre:regex-replace-all (string #\Newline) text (format nil "~c~%" #\Return)) text))
             (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 10))
             (b (progn (let ((old (tuatara.buffer:find-buffer "cycle.org"))) (when old (tuatara.buffer:kill-buffer old)))
                       (tuatara.buffer:make-buffer "cycle.org" :text text)))
             (differences '()) (compared 0))
        (when crlf (setf (tuatara.buffer:buffer-local b :line-ending) :crlf))
        (tuatara.defaults::set-major-mode b 'tuatara.defaults::org-mode)
        (tuatara.frontend.fake:fake-queue-action
         f (lambda ()
             (let ((rope (tuatara.buffer:buffer-text b)))
               (loop for line = 0 then (1+ (tuatara.text:rope-line-end rope line))
                     while (< line (tuatara.text:rope-bytes rope))
                     when (tuatara.defaults::org-stars rope line)
                       do (flet ((steps (cycle)
                                   (tuatara.display:unfold-all b)
                                   (loop repeat 4 do (funcall cycle) collect (tuatara.display:buffer-folds b))))
                            (let ((parsed (steps (lambda ()
                                                   (multiple-value-bind (tree text) (tuatara.defaults::org-tree-now b)
                                                     (let ((tuatara.defaults::*org-current-text* text))
                                                       (tuatara.defaults::org-cycle-headline
                                                        b (tuatara.defaults::org-headline-on-line tree text (tuatara.defaults::org-byte->char b line))
                                                        text))))))
                                  (read (steps (lambda () (tuatara.defaults::org-cycle-headline-at b line)))))
                              (when (some #'identity parsed) (incf compared))
                              (unless (equal parsed read) (push (list line parsed read) differences))))))))
        (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer b)
        (true (plusp compared) "~d headlines folded" compared)
        (is equal '() differences "~s~:[~; (CR LF)~]" text crlf)
        (tuatara.buffer:kill-buffer b)))))

(define-test (org opens-indented)
  ;; an Org buffer opens in the indented view, as org-indent-mode on every file; *org-startup-indented* nil, as written
  (let ((tuatara-user:*org-bullets* nil))
    (is equal '("* One" "  Text of one." " ** One A" "    Text of one A.") (subseq (org-screen "") 0 4)))
  (let ((tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil))
    (is equal '("* One" "Text of one." "** One A" "Text of one A.") (subseq (org-screen "") 0 4))))

(defparameter *org-signs*
  (format nil "* One~%** Two~%* TODO Task~%* DONE Done~%- a~%+ b~%  * c~%1. num~%- [ ] x~%- [X] y~%- [-] z~%#+begin_src text~%- not an item~%#+end_src~%")
  "An Org text with headlines, TODO keywords, list items of each bullet and checkboxes, and a list in a block.")

(define-test (org signs)
  ;; headlines' last stars, TODO keywords, list bullets and checkboxes shown as signs; the text as it was
  (let ((tuatara-user:*org-startup-indented* nil)
        (tuatara-user:*org-list-bullets* '(("-" . "•") ("+" . "◦") ("*" . "▸")))
        (tuatara-user:*org-checkbox-signs* '(("[ ]" . "☐") ("[-]" . "◐") ("[X]" . "☑"))))
    (multiple-value-bind (lines editor) (org-screen "" :text *org-signs* :height 18)
      (is equal '("◉ One" "*○ Two" "◉ TODO Task" "◉ DONE Done" "• a" "◦ b" "  ▸ c" "1. num" "• ☐  x" "• ☑  y" "• ◐  z"
                  "#+begin_src text" "- not an item" "#+end_src")
          (subseq lines 0 14))
      (is string= *org-signs* (session-text editor)))
    ;; each turned off: as written
    (let ((tuatara-user:*org-list-bullets* nil) (tuatara-user:*org-checkbox-signs* nil))
      (is equal '("◉ One" "*○ Two" "◉ TODO Task" "◉ DONE Done" "- a" "+ b" "  * c" "1. num" "- [ ] x")
          (subseq (org-screen "" :text *org-signs* :height 18) 0 9)))
    ;; leading stars hidden in the ground's colour, or shown faint
    (let ((f (run-keys "M-x o r g - m o d e RET" :text *org-signs* :width 60 :height 18)))
      (true (org-faces-include f 1 "*" :org-hide)))
    (let* ((tuatara-user:*org-hide-leading-stars* nil)
           (f (run-keys "M-x o r g - m o d e RET" :text *org-signs* :width 60 :height 18)))
      (true (org-faces-include f 1 "*" :org-leading-stars)))))

(define-test (org list-items-fold)
  ;; TAB on a list item with something under it folds it, as a headline: folded, its sub-items, all of it
  (let ((text (format nil "- one~%  more of one~%  - sub a~%    under a~%  - sub b~%- two~%"))
        (tuatara-user:*org-startup-indented* nil))
    (is equal '("- one..." "- two") (subseq (org-screen "TAB" :text text) 0 2))
    (is equal '("- one" "  more of one" "  - sub a..." "  - sub b") (subseq (org-screen "TAB TAB" :text text) 0 4))
    (is equal '("- one" "  more of one" "  - sub a" "    under a") (subseq (org-screen "TAB TAB TAB" :text text) 0 4))
    (is equal '("- one..." "- two") (subseq (org-screen "TAB TAB TAB TAB" :text text) 0 2))
    ;; an item with nothing under it is said to be empty, as Org says, and left as it is
    (multiple-value-bind (lines editor) (org-screen "g e k TAB" :text text)
      (is equal '("- two") (subseq lines 5 6))
      (is string= text (session-text editor)))
    ;; *org-cycle-include-plain-lists* nil: TAB never folds an item, but indents as in text
    (let ((tuatara-user:*org-cycle-include-plain-lists* nil))
      (is string= "  more of one" (second (org-screen "TAB" :text text))))))

(define-test (org checkbox-typed)
  ;; [ typed after a list item's bullet makes a checkbox at once, point after its space; checkboxes shown as written
  (dolist (pairs '(nil t))
    (let ((tuatara-user:*electric-pair-mode* pairs))
      (multiple-value-bind (lines editor) (org-screen "i - SPC [ x ESC" :text "")
        (is string= "- [ ] x" (session-text editor) "pairs ~a" pairs)
        (is string= "- [ ] x" (first lines)))
      (is string= "1. [ ] y" (session-text (nth-value 1 (org-screen "i 1 . SPC [ y ESC" :text ""))))
      (is string= (format nil "* A~%  * [ ] z") (session-text (nth-value 1 (org-screen "g e A RET SPC SPC * SPC [ z ESC" :text (format nil "* A")))))
      ;; elsewhere, [ as typed
      (is string= (if pairs "a []" "a [") (session-text (nth-value 1 (org-screen "i a SPC [ ESC" :text ""))))
      (is string= (if pairs "- x[]" "- x[") (session-text (nth-value 1 (org-screen "i - SPC x [ ESC" :text ""))))
      ;; and in a buffer that is not Org, never
      (is string= (if pairs "- []" "- [") (session-text (nth-value 1 (run-keys "i - SPC [ ESC" :width 60 :height 8))))
      (let ((tuatara-user:*org-complete-checkboxes* nil))
        (is string= (if pairs "- []" "- [") (session-text (nth-value 1 (org-screen "i - SPC [ ESC" :text ""))))))))

(define-test (org checkbox-return)
  ;; cycling as such, nothing logged
  (let ((tuatara-user:*org-log-checkboxes* nil))
    ;; RET in NORMAL state anywhere on a checkbox's line cycles it, [ ] [-] [X], the count above brought up to date
    (let ((text (format nil "* Tasks [0/2]~%- [ ] one~%- [ ] two [[*Tasks][a link]]~%")))
      (flet ((after (keys) (session-text (nth-value 1 (org-screen keys :text text)))))
        (is string= (format nil "* Tasks [0/2]~%- [-] one~%- [ ] two [[*Tasks][a link]]~%") (after "j RET"))
        (is string= (format nil "* Tasks [1/2]~%- [X] one~%- [ ] two [[*Tasks][a link]]~%") (after "j C-u 6 l RET RET"))
        (is string= (format nil "* Tasks [0/2]~%- [ ] one~%- [ ] two [[*Tasks][a link]]~%") (after "j RET RET RET"))
        ;; on a link on such a line, RET follows the link
        (multiple-value-bind (lines editor) (org-screen "j j C-u 2 1 l RET" :text text)
          (declare (ignore lines))
          (is string= text (session-text editor)))
        ;; turned off: RET does nothing there
        (let ((tuatara-user:*org-return-cycles-checkboxes* nil))
          (is string= text (after "j RET")))))))

(define-test (org todo-return)
  ;; RET in NORMAL state on a headline with a keyword: the next keyword, round to the first, never none
  (let ((tuatara-user:*org-log-done* nil))
    (flet ((after (keys text) (session-text (nth-value 1 (org-screen keys :text text)))))
      (is string= (format nil "* WAIT Mow the lawn~%") (after "RET" (format nil "* TODO Mow the lawn~%")))
      (is string= (format nil "* DONE Mow the lawn~%") (after "RET RET" (format nil "* TODO Mow the lawn~%")))
      (is string= (format nil "* TODO Mow the lawn~%") (after "RET RET RET" (format nil "* TODO Mow the lawn~%")))
      ;; anywhere on the line, and round the file's own keywords
      (is string= (format nil "#+TODO: TODO WAIT | DONE~%* WAIT Call back~%")
          (after "j C-u 8 l RET" (format nil "#+TODO: TODO WAIT | DONE~%* TODO Call back~%")))
      (is string= (format nil "#+TODO: TODO WAIT | DONE~%* TODO Call back~%")
          (after "j RET RET RET" (format nil "#+TODO: TODO WAIT | DONE~%* TODO Call back~%")))
      ;; the heading above counts it: [1/1]
      (is string= (format nil "* Jobs [1/1]~%** DONE Mow~%") (after "j RET RET" (format nil "* Jobs [0/1]~%** TODO Mow~%")))
      ;; with a checkbox under it not ticked: TODO to WAIT and back, DONE passed over
      (is string= (format nil "* WAIT Pack [1/2]~%- [X] tent~%- [ ] stove~%")
          (after "RET" (format nil "* TODO Pack [1/2]~%- [X] tent~%- [ ] stove~%")))
      (is string= (format nil "* TODO Pack [1/2]~%- [X] tent~%- [ ] stove~%")
          (after "RET RET" (format nil "* TODO Pack [1/2]~%- [X] tent~%- [ ] stove~%")))
      ;; with TODO and DONE alone, not done while a checkbox under it is not ticked, or a TODO under
      ;; it not done: said, nothing changed
      (let ((tuatara-user:*org-standard-todo-keywords* '(("TODO") . ("DONE"))))
        (multiple-value-bind (lines editor) (org-screen "RET" :text (format nil "* TODO Pack [1/3]~%- [X] tent~%- [ ] stove~%- [-] food~%"))
          (declare (ignore lines))
          (is string= (format nil "* TODO Pack [1/3]~%- [X] tent~%- [ ] stove~%- [-] food~%") (session-text editor))
          (true (search "Not DONE yet: 2 checkboxes still open" (messages-text))))
        (multiple-value-bind (lines editor) (org-screen "RET" :text (format nil "* TODO Move~%** DONE Boxes~%** TODO Van~%- [ ] keys~%"))
          (declare (ignore lines))
          (is string= (format nil "* TODO Move~%** DONE Boxes~%** TODO Van~%- [ ] keys~%") (session-text editor))
          (true (search "Not DONE yet: 1 task still open" (messages-text)))))
      ;; with work open and keywords of its own: round those not done, DONE and CANCELLED passed over
      (let ((head (format nil "#+TODO: TODO NEXT WAIT | DONE CANCELLED~%"))
            (body (format nil "- [ ] stove~%")))
        (flet ((task (keyword) (format nil "~a* ~a Pack~%~a" head keyword body)))
          (is string= (task "NEXT") (after "j RET" (task "TODO")))
          (is string= (task "WAIT") (after "j RET RET" (task "TODO")))
          (is string= (task "TODO") (after "j RET RET RET" (task "TODO")))
          ;; a done one with work open: to the first not done
          (is string= (task "TODO") (after "j RET" (task "CANCELLED")))))
      ;; a task whose date repeats: RET reaches DONE, done this time and moved on, back to TODO; with
      ;; boxes open, DONE passed over
      (let ((head (format nil "#+TODO: TODO NEXT WAIT | DONE~%")))
        (let ((done (after "j RET" (format nil "~a* WAIT Pack~%SCHEDULED: <2026-10-27 Tue +1w>~%" head))))
          (true (search "* TODO Pack" done) "~a" done)
          (true (search "<2026-11-03 Tue +1w>" done) "~a" done)
          (true (search "- State \"DONE\"" done) "~a" done))
        (is string= (format nil "~a* TODO Pack~%SCHEDULED: <%%(diary-date t 15 t)>~%- [ ] tent~%" head)
            (after "j RET" (format nil "~a* WAIT Pack~%SCHEDULED: <%%(diary-date t 15 t)>~%- [ ] tent~%" head))))
      ;; a repeating task done by its last box ticked: moved on, its boxes and counts cleared for next time
      (let ((tuatara-user:*org-log-repeat* nil))
        (is string= (format nil "* TODO Stand-up [0/2]~%SCHEDULED: <2026-11-02 Mon 09:00 +1w>~%- [ ] notes~%- [ ] board [0/1]~%  - [ ] cards~%")
            (let ((tuatara-user:*org-log-checkboxes* nil))
              (after "j j j RET"
                     (format nil "* TODO Stand-up [1/2]~%SCHEDULED: <2026-10-26 Mon 09:00 +1w>~%- [X] notes~%- [-] board [0/1]~%  - [ ] cards~%")))))
      ;; all ticked: done; and a done one goes back to TODO whatever is under it
      (is string= (format nil "* DONE Pack [2/2]~%- [X] tent~%- [X] stove~%")
          (after "RET RET" (format nil "* TODO Pack [2/2]~%- [X] tent~%- [X] stove~%")))
      (is string= (format nil "* TODO Pack [1/2]~%- [X] tent~%- [ ] stove~%")
          (after "RET" (format nil "* DONE Pack [1/2]~%- [X] tent~%- [ ] stove~%")))
      ;; C-c C-t makes it done all the same
      (is string= (format nil "* DONE Pack [1/2]~%- [X] tent~%- [ ] stove~%")
          (after "C-c C-t C-c C-t" (format nil "* TODO Pack [1/2]~%- [X] tent~%- [ ] stove~%")))
      ;; a headline with no keyword, or a line under one, is left alone
      (is string= (format nil "* Notes~%") (after "RET" (format nil "* Notes~%")))
      (is string= (format nil "* TODO Mow~%Some text.~%") (after "j RET" (format nil "* TODO Mow~%Some text.~%")))
      ;; turned off: nothing
      (let ((tuatara-user:*org-return-cycles-todo* nil))
        (is string= (format nil "* TODO Mow the lawn~%") (after "RET" (format nil "* TODO Mow the lawn~%")))))))

(define-test (org todo-rotation-passes-over-cancelled)
  ;; the standard keywords go TODO, WAIT, DONE and none: C-c C-t, S-<right>, S-<left> and RET pass over CANCELLED
  (let ((tuatara-user:*org-log-done* nil))
    (flet ((after (keys text) (session-text (nth-value 1 (org-screen keys :text text)))))
      (let ((todo (format nil "* TODO Gate~%")))
        (is string= (format nil "* WAIT Gate~%") (after "C-c C-t" todo))
        (is string= (format nil "* DONE Gate~%") (after "C-c C-t C-c C-t" todo))
        (is string= (format nil "* Gate~%") (after "C-c C-t C-c C-t C-c C-t" todo))
        (is string= (format nil "* Gate~%") (after "S-<right> S-<right> S-<right>" todo))
        (is string= (format nil "* DONE Gate~%") (after "S-<left>" (format nil "* Gate~%")))
        (is string= (format nil "* WAIT Gate~%") (after "RET" todo))
        (is string= todo (after "RET RET RET" todo)))
      ;; from CANCELLED: on to the first, back to the last
      (let ((cancelled (format nil "* CANCELLED Gate~%")))
        (is string= (format nil "* TODO Gate~%") (after "C-c C-t" cancelled))
        (is string= (format nil "* DONE Gate~%") (after "S-<left>" cancelled))
        (is string= (format nil "* TODO Gate~%") (after "RET" cancelled)))
      ;; a file's own CANCELLED passed over the same way, its own keywords still in force
      (let ((head (format nil "#+TODO: TODO NEXT | DONE CANCELLED~%")))
        (is string= (format nil "~a* DONE Gate~%" head) (after "j C-c C-t C-c C-t" (format nil "~a* TODO Gate~%" head)))
        (is string= (format nil "~a* Gate~%" head) (after "j C-c C-t C-c C-t C-c C-t" (format nil "~a* TODO Gate~%" head))))
      ;; with none passed over, CANCELLED takes its turn after DONE
      (let ((tuatara-user:*org-todo-keywords-not-cycled* nil))
        (is string= (format nil "* CANCELLED Gate~%") (after "C-c C-t C-c C-t C-c C-t" (format nil "* TODO Gate~%")))))))

(define-test (org cancel-task)
  ;; C-c C-x C cancels a task: CANCELLED noted with CLOSED: as DONE is; one that repeats asks, s ending it where it is
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        (tuatara-user:*org-log-done* :time))
    (is string= (format nil "* CANCELLED Gate~%CLOSED: [2026-10-01 Thu 18:00]~%Body~%")
        (org-text-after "C-c C-x C" (format nil "* TODO Gate~%Body~%")))
    (is string= (format nil "* CANCELLED Mow~%CLOSED: [2026-10-01 Thu 18:00] SCHEDULED: <2026-10-05 Mon +1w>~%")
        (org-text-after "C-c C-x C s" (format nil "* WAIT Mow~%SCHEDULED: <2026-10-05 Mon +1w>~%")))
    ;; a file's own keywords: its CANCELED; with neither, said and nothing changed
    (is string= (format nil "#+TODO: TODO | DONE CANCELED~%* CANCELED Gate~%CLOSED: [2026-10-01 Thu 18:00]~%")
        (org-text-after "j C-c C-x C" (format nil "#+TODO: TODO | DONE CANCELED~%* TODO Gate~%")))
    (let ((own (format nil "#+TODO: TODO | DONE~%* TODO Gate~%")))
      (multiple-value-bind (lines editor) (org-screen "j C-c C-x C" :text own)
        (declare (ignore lines))
        (is string= own (session-text editor))
        (true (search "No CANCELLED" (messages-text)))))
    ;; a checkbox's line is refused, its heading left as it is
    (let ((errands (format nil "* TODO Errands~%- [ ] call Anna~%")))
      (is string= errands (org-text-after "j C-c C-x C" errands)))))

(define-test (org new-items-and-bullets)
  ;; M-RET: an item of the same kind after the one point is in and all under it, a checkbox carried over
  (is string= (format nil "- [ ] a~%- [ ] ~%- b~%") (org-text-after "M-RET" (format nil "- [ ] a~%- b~%")))
  (is string= (format nil "- a~%  - sub~%- ~%- b~%") (org-text-after "M-RET" (format nil "- a~%  - sub~%- b~%")))
  (is string= (format nil "1. a~%   more~%2. ~%") (org-text-after "j M-RET" (format nil "1. a~%   more~%")))
  ;; M-RET carries the checkbox too
  (is string= (format nil "- [X] a~%- [ ] ~%") (org-text-after "M-RET" (format nil "- [X] a~%")))
  ;; past the list, in the section's text: a headline of the section's level after its subtree
  (is string= (format nil "* H~%- a~%~%text~%** Sub~%* ~%* Next~%")
      (org-text-after "j j j M-RET" (format nil "* H~%- a~%~%text~%** Sub~%* Next~%")))
  ;; C-RET is free in Org, for a key of your own
  (false (tuatara.command:lookup-key (tuatara.defaults::mode-keymap 'tuatara.defaults::org-mode) (tuatara.frontend:parse-key-sequence "C-RET")))
  ;; C-c -: the list's next kind of bullet, every item, numbered from 1; * passed over at the margin
  (let ((text (format nil "- a~%- [ ] b~%")))
    (is string= (format nil "+ a~%+ [ ] b~%") (org-text-after "C-c -" text))
    (is string= (format nil "1. a~%2. [ ] b~%") (org-text-after "C-c - C-c -" text))
    (is string= (format nil "1) a~%2) [ ] b~%") (org-text-after "C-c - C-c - C-c -" text))
    (is string= text (org-text-after "C-c - C-c - C-c - C-c -" text)))
  (is string= (format nil "* H~%  * a~%  * b~%") (org-text-after "j C-c - C-c -" (format nil "* H~%  - a~%  - b~%"))))

(define-test (org checkbox-tasks)
  ;; a list item with a checkbox is a task: ticked with C-c C-t, clocked, noted, each logged in a drawer under it
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 5 14 2 10 2026))
        (text (format nil "* Errands~%- [ ] call Anna~%- [ ] buy milk~%")))
    (is string= (format nil "* Errands~%- [X] call Anna~%  :LOGBOOK:~%  - State \"DONE\"       from \"TODO\"       [2026-10-02 Fri 14:05]~%  :END:~%- [ ] buy milk~%")
        (org-text-after "j C-c C-t" text))
    ;; cleared a minute later: logged on top
    (is string= (format nil "* Errands~%- [ ] call Anna~%  :LOGBOOK:~%  - State \"TODO\"       from \"DONE\"       [2026-10-02 Fri 14:06]~%  - State \"DONE\"       from \"TODO\"       [2026-10-02 Fri 14:05]~%  :END:~%- [ ] buy milk~%")
        (session-text (nth-value 1 (run-keys (list "M-x o r g - m o d e RET j C-c C-t"
                                                   (lambda () (setf tuatara.defaults::*org-now* (encode-universal-time 0 6 14 2 10 2026)))
                                                   "C-c C-t")
                                             :text text :width 60 :height 12))))
    (setf tuatara.defaults::*org-now* (encode-universal-time 0 5 14 2 10 2026))
    ;; taken back the same minute: its note goes, and the drawer with it when nothing else is there
    (is string= text (org-text-after "j C-c C-t C-c C-t" text))
    (is string= (format nil "* Errands~%- [ ] call Anna~%  :LOGBOOK:~%  - Note~%  :END:~%- [ ] buy milk~%")
        (org-text-after "j C-c C-t C-c C-t" (format nil "* Errands~%- [ ] call Anna~%  :LOGBOOK:~%  - Note~%  :END:~%- [ ] buy milk~%")))
    ;; RET in NORMAL state logs as it reaches [X], and cycled round leaves no note
    (true (search "- State \"DONE\"" (org-text-after "j RET RET" text)))
    (is string= text (org-text-after "j RET RET RET" text))
    (let ((tuatara-user:*org-log-checkboxes* nil))
      (is string= (format nil "* Errands~%- [X] call Anna~%- [ ] buy milk~%") (org-text-after "j C-c C-t" text)))
    ;; clocked in and out on the item, its line in its own drawer
    (is string= (format nil "* Errands~%- [ ] call Anna~%  :LOGBOOK:~%  CLOCK: [2026-10-02 Fri 14:05]--[2026-10-02 Fri 14:05] =>  0:00~%  :END:~%- [ ] buy milk~%")
        (org-text-after "j C-c C-x C-i C-c C-x C-o" text))
    (setf tuatara.defaults::*org-clock* nil)
    ;; a note
    (true (search (format nil "  :LOGBOOK:~%  - Note taken on [2026-10-02 Fri 14:05] \\\\~%    hi~%  :END:")
                  (org-text-after "j C-c C-z h i RET" text)))
    ;; the agenda reads each item as an entry of its own, its drawer its own, not the headline's
    (let* ((done (format nil "* Errands~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00~%:END:~%- [X] call Anna~%  :LOGBOOK:~%  - State \"DONE\"       from \"TODO\"       [2026-10-02 Fri 14:05]~%  CLOCK: [2026-10-02 Fri 13:40]--[2026-10-02 Fri 14:05] =>  0:25~%  :END:~%- [ ] buy milk~%  - [-] sub task~%"))
           (entries (tuatara.defaults::org-agenda-file-entries "/x/errands.org" done)))
      (is equal '((:headline nil "Errands") (:checkbox "DONE" "call Anna") (:checkbox "TODO" "buy milk") (:checkbox "TODO" "sub task"))
          (mapcar (lambda (e) (list (tuatara.defaults::agenda-entry-kind e) (tuatara.defaults::agenda-entry-todo e)
                                    (tuatara.defaults::agenda-entry-title e)))
                  entries))
      (is = 1 (count :clock (tuatara.defaults::agenda-entry-logs (first entries)) :key #'first))
      (is equal '(:clock :closed) (sort (mapcar #'first (tuatara.defaults::agenda-entry-logs (second entries))) #'string<)))))

(define-test (org todo-follows-checkboxes)
  ;; a TODO headline with a cookie is done once all it counts are, with CLOSED: whatever *org-log-done* says; reopened, CLOSED: goes
  ;; with Org's own TODO and DONE alone
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 17 14 2 10 2026))
        (tuatara-user:*org-standard-todo-keywords* '(("TODO") . ("DONE")))
        (tuatara-user:*org-log-checkboxes* nil)
        (tuatara-user:*org-log-done* nil)
        (text (format nil "** TODO Second test [1/2] [50%]                                 :gardening:~%- [X] a~%- [ ] b~%")))
    (is string= (format nil "** DONE Second test [2/2] [100%]                                 :gardening:~%CLOSED: [2026-10-02 Fri 14:17]~%- [X] a~%- [X] b~%")
        (org-text-after "j j C-c C-c" text))
    (is string= text (org-text-after "j j C-c C-c C-c C-c" text))
    ;; a headline with no TODO keyword stays as it is; turned off, so does a TODO one
    (is string= (format nil "** Plain [2/2]~%- [X] a~%- [X] b~%") (org-text-after "j j C-c C-c" (format nil "** Plain [1/2]~%- [X] a~%- [ ] b~%")))
    (let ((tuatara-user:*org-todo-follows-checkboxes* nil))
      (true (search "** TODO Second test [2/2] [100%]" (org-text-after "j j C-c C-c" text))))
    ;; TODO sub-headlines: their parent's cookie kept up, the parent done once all are, and so up the tree
    (let ((project (format nil "* TODO Goal [0/1]~%** TODO Project [0/2]~%*** TODO a~%*** TODO b~%")))
      (is string= (format nil "* TODO Goal [0/1]~%** TODO Project [1/2]~%*** DONE a~%*** TODO b~%")
          (org-text-after "j j C-c C-t" project))
      (is string= (format nil "* DONE Goal [1/1]~%CLOSED: [2026-10-02 Fri 14:17]~%** DONE Project [2/2]~%CLOSED: [2026-10-02 Fri 14:17]~%*** DONE a~%*** DONE b~%")
          (org-text-after "j j C-c C-t j C-c C-t" project))
      ;; b back to TODO: the project and the goal open again, their CLOSED: gone
      (is string= (format nil "* TODO Goal [0/1]~%** TODO Project [1/2]~%*** DONE a~%*** TODO b~%")
          (org-text-after "j j C-c C-t j C-c C-t S-<left>" project)))))

(define-test (org drawers-fold)
  ;; TAB on a drawer's line folds it and opens it; cycling a headline or an item shows its drawers folded
  (let ((text (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00~%:END:~%body~%* B~%- [ ] x~%  :LOGBOOK:~%  - State \"DONE\" [2026-10-02 Fri 14:05]~%  :END:~%  more~%"))
        (tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil))
    (is equal '("* A" ":LOGBOOK:..." "body" "* B") (subseq (org-screen "j TAB" :text text) 0 4))
    (is equal '("* A" ":LOGBOOK:" "CLOCK: [2026") (mapcar (lambda (l) (subseq l 0 (min 12 (length l)))) (subseq (org-screen "j TAB TAB" :text text) 0 3)))
    (is equal '("* A..." "* B") (subseq (org-screen "TAB" :text text) 0 2))
    (is equal '("* A" ":LOGBOOK:..." "body") (subseq (org-screen "TAB TAB" :text text) 0 3))
    (is equal '("- [ ] x" "  :LOGBOOK:..." "  more") (subseq (org-screen "C-u 6 j TAB TAB" :text text) 6 9))
    (let ((tuatara-user:*org-cycle-hide-drawers* nil))
      (is equal '("* A" ":LOGBOOK:" "CLOCK: [2026") (mapcar (lambda (l) (subseq l 0 (min 12 (length l)))) (subseq (org-screen "TAB TAB" :text text) 0 3))))))

(define-test (org metadata-folded-on-opening)
  ;; an Org file opens with each task's drawers and notes folded to one line, its planning shown;
  ;; TAB on the notes' line opens them
  (let ((text (format nil "* TODO Review [0/1]~%SCHEDULED: <2026-10-28 Wed +1d>~%:PROPERTIES:~%:LAST_REPEAT: [2026-10-25 Sun 14:02]~%:END:~%~
                           - State \"DONE\"       from \"NEXT\"       [2026-10-25 Sun 14:02]~%~
                           - Note taken on [2026-10-25 Sun 13:00] \\\\~%  Called back~%~
                           CLOCK: [2026-10-25 Sun 12:00]--[2026-10-25 Sun 12:30] =>  0:30~%~
                           - [ ] hello~%  :LOGBOOK:~%  - State \"DONE\"       from \"TODO\"       [2026-10-25 Sun 14:02]~%  :END:~%~
                           - State \"DONE\"       from \"TODO\"       [2026-10-24 Sat 09:00]~%* Next~%"))
        (tuatara-user:*org-startup-indented* nil) (tuatara-user:*org-bullets* nil) (tuatara-user:*org-fold-metadata* t))
    (flet ((head (lines) (mapcar (lambda (l) (subseq l 0 (min 24 (length l)))) lines)))
      ;; the notes' line, longer than the 60 columns, has its ... on the row after; a note alone is no fold
      (is equal '("* TODO Review [0/1]" "SCHEDULED: <2026-10-28 W" ":PROPERTIES:..." "- State \"DONE\"       fro" "..."
                  "- [ ] hello" "  :LOGBOOK:..." "- State \"DONE\"       fro" "* Next")
          (head (subseq (org-screen "" :text text) 0 9)))
      ;; TAB on the notes' first line opens them, again folds them
      (is equal '("- State \"DONE\"       fro" "- Note taken on [2026-10" "  Called back" "CLOCK: [2026-10-25 Sun 1")
          (head (subseq (org-screen "j j j TAB" :text text) 3 7)))
      (true (search "Notes shown" (messages-text)))
      ;; off: all shown; a file's #+STARTUP: says otherwise either way
      (let ((tuatara-user:*org-fold-metadata* nil))
        (is string= ":PROPERTIES:" (third (org-screen "" :text text)))
        (is string= ":PROPERTIES:..." (fourth (org-screen "" :text (format nil "#+STARTUP: hidedrawers~%~a" text)))))
      (is string= ":PROPERTIES:" (fourth (org-screen "" :text (format nil "#+STARTUP: showdrawers~%~a" text))))
      ;; a drawer of any name: everything in it folded, TAB on its line opens it
      (let ((own (format nil "* Trip~%:FOLD:~%Flights booked~%Hotel paid~%:END:~%Packing list~%")))
        (is equal '("* Trip" ":FOLD:..." "Packing list") (subseq (org-screen "" :text own) 0 3))
        (is equal '(":FOLD:" "Flights booked" "Hotel paid" ":END:") (subseq (org-screen "j TAB" :text own) 1 5))))))

(define-test (org tab-leaves-planning-and-drawers)
  ;; TAB on a planning line, on a drawer's :END: or on a line inside a drawer leaves the text as it is
  (let ((text (format nil "* DONE Task~%CLOSED: [2026-10-15 Thu 10:00]~%:LOGBOOK:~%- State \"DONE\"       from \"TODO\"       [2026-10-15 Thu 10:00]~%:END:~%:PROPERTIES:~%:ID: x~%:END:~%body~%")))
    (dolist (keys '("j TAB" "j l l l TAB" "C-u 3 j TAB" "C-u 3 j l l TAB" "C-u 4 j TAB" "C-u 6 j TAB" "C-u 7 j TAB"))
      (is string= text (org-text-after keys text) "~a" keys))))

(define-test (org fold-with-links-drawn)
  ;; a fold between links shown as their labels, signs on the headlines: drawn every time, frame after frame
  (let ((text (format nil "* A [[https://a.org][one]]~%** Sub~%text under sub~%more~%* B [[https://b.org][two]]~%** C [[https://c.org][three]]~%body~%")))
    (dolist (keys '("j TAB j k" "j TAB j k j k" "j TAB TAB TAB j k" "C-u 5 j TAB k k j j" "j TAB C-u 5 j TAB k j"))
      (let ((f (run-keys (format nil "M-x o r g - m o d e RET ~a" keys) :text text :width 80 :height 12)))
        (false (tuatara.frontend.fake:fake-frame-showing f "cannot be shown") "~a" keys)))))

(define-test (org kills-never-take-folded-text-unseen)
  ;; zap-up-to-char, kill-sexp and delete-blank-lines reaching into a folded body: the fold shown, nothing taken
  (let ((text (format nil "* A~%body x and more~%* B~%")))
    (is string= text (org-text-after "S-TAB M-z x" text))
    (is string= text (org-text-after "S-TAB C-M-k C-M-k C-M-k" text))))


(define-test (org folded-blocks-not-coloured)
  ;; the source blocks a fold hides between two headlines shown are not coloured at each frame
  (let ((path (records-file (with-output-to-string (o)
                              (format o "#+STARTUP: overview~%* A~%* B~%")
                              (dotimes (i 300) (format o "#+begin_src lisp~%(defun f~d (x) (+ x ~d))~%#+end_src~%" i i))
                              (format o "* C~%#+begin_src lisp~%(car x)~%#+end_src~%"))
                            :type "org"))
        (b nil) (lit nil))
    (unwind-protect
         (run-keys (list (lambda () (setf b (tuatara.command:current-buffer)))
                         :tick
                         (lambda ()
                           (loop repeat 200 until (tuatara.defaults::org-parse-current-p b) do (sleep 0.05))
                           (let* ((e tuatara.command:*editor*) (w (tuatara.command:editor-window e)))
                             (setf lit (length (tuatara.command::window-highlights-for
                                                e w (tuatara.display:window-rows w)))))))
                   :file path :width 60 :height 10)
      (kill-records b path))
    (true (< 0 lit 30) "~d ranges lit" lit)))
