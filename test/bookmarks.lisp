;;;; test/bookmarks.lisp --- Places in files kept by name, between sessions (bookmarks).
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test bookmarks
  :description "C-x m m keeps where point is in a file under a name, C-x m b goes back there in a later session, found again when the text above it changed, and C-x m l manages them as the buffer list does buffers: marks, deleting, renaming.")

(define-test (bookmarks set-and-jump)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (path (merge-pathnames "notes.txt" dir)))
      (alexandria:write-string-into-file (format nil "one~%two~%three~%") path)
      (unwind-protect
           (progn
             ;; on the second line's second character, kept under the file's name
             (run-keys "C-n C-f C-x m m RET RET" :file (namestring path) :width 80)
             (forget-file-buffers)
             (let ((kept (tuatara.defaults::find-bookmark "notes.txt")))
               (is equal (list "notes.txt" (tuatara.io:native path) 5) (subseq kept 0 3))
               (is string= (format nil "wo~%three~%") (fourth kept)))
             ;; lines put above it meanwhile: found again by the text after it
             (alexandria:write-string-into-file (format nil "zero~%one~%two~%three~%") path :if-exists :supersede)
             (let ((editor (nth-value 1 (run-keys "C-x m b n o t e s RET" :width 80))))
               (is = 10 (session-point editor)))
             (forget-file-buffers)
             ;; the list: renamed, then deleted
             (multiple-value-bind (f editor) (run-keys "C-x m l" :width 120 :height 12)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "notes.txt"))
               (true (tuatara.frontend.fake:fake-frame-showing f "File or folder")))
             (run-keys "C-x m l r C-a C-k t o d o RET" :width 120 :height 12)
             (false (tuatara.defaults::find-bookmark "notes.txt"))
             (true (tuatara.defaults::find-bookmark "todo"))
             (run-keys "C-x m l d x y e s RET" :width 120 :height 12)
             (false (tuatara.defaults::find-bookmark "todo")))
        (forget-file-buffers)
        (dolist (name (tuatara.defaults::bookmark-names))
          (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name)))))))

(define-test (bookmarks lines-read-back)
  ;; a bookmark's line and back, names and paths with tabs, newlines and backslashes kept apart
  (let ((bookmark (list (format nil "a\\b~%c") "/tmp/x y/z.txt" 42 (format nil "after~%it") '() nil)))
    (is equal bookmark (tuatara.defaults::parse-bookmark-line (tuatara.defaults::bookmark-line bookmark))))
  ;; with categories, and a line written before there were any
  (let ((bookmark (list "n" "/tmp/z.txt" 7 "x" '("work" "home") nil)))
    (is equal bookmark (tuatara.defaults::parse-bookmark-line (tuatara.defaults::bookmark-line bookmark))))
  (is equal '("n" "/p" 3 "x" () nil) (tuatara.defaults::parse-bookmark-line (format nil "3~cn~c/p~cx" #\Tab #\Tab #\Tab)))
  ;; set in a file opened safely: kept so, with or without categories
  (dolist (bookmark (list (list "s" "/tmp/e.txt" 1 "x" '() t) (list "s" "/tmp/e.txt" 1 "x" '("case") t)))
    (is equal bookmark (tuatara.defaults::parse-bookmark-line (tuatara.defaults::bookmark-line bookmark))))
  (false (tuatara.defaults::parse-bookmark-line "x	name	/path	text"))
  (false (tuatara.defaults::parse-bookmark-line "12	name")))

(define-test (bookmarks managed-as-buffers)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil))
      (flet ((keep (name path) (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks*
                                                                  (list (list name (namestring path) 0 "")))))
        (unwind-protect
             (progn
               (alexandria:write-string-into-file "a" (merge-pathnames "a.txt" dir))
               (keep "gone" (merge-pathnames "missing.txt" dir))
               (keep "folder" dir)
               (keep "alpha" (merge-pathnames "a.txt" dir))
               ;; the latest first, each with its kind, a summing-up line
               (multiple-value-bind (f editor) (run-keys "C-x m l" :width 120 :height 14)
                 (declare (ignore f))
                 (let ((text (session-text editor)))
                   (let ((rows (subseq text (search "alpha" text))))
                     (true (< (search "alpha" rows) (search "folder" rows) (search "gone " rows)) "~a" text))
                   (true (cl-ppcre:scan "alpha +file" text))
                   (true (cl-ppcre:scan "folder +folder" text))
                   (true (cl-ppcre:scan "gone +gone" text))
                   (true (search "3 bookmarks" text))
                   (true (search "1 gone" text))))
               ;; m marks (>), U takes the marks off, D deletes the marked after asking
               (multiple-value-bind (f editor) (run-keys "C-x m l m" :width 120 :height 14)
                 (declare (ignore f))
                 (true (cl-ppcre:scan "(?m)^> +alpha" (session-text editor))))
               (multiple-value-bind (f editor) (run-keys "C-x m l m U" :width 120 :height 14)
                 (declare (ignore f))
                 (false (cl-ppcre:scan "(?m)^> +alpha" (session-text editor))))
               (run-keys "C-x m l m m D y e s RET" :width 120 :height 14)
               (is equal '("gone") (tuatara.defaults::bookmark-names))
               ;; none marked: D deletes the one on the line
               (run-keys "C-x m l D y e s RET" :width 120 :height 14)
               (is equal '() (tuatara.defaults::bookmark-names)))
          (dolist (name (tuatara.defaults::bookmark-names))
            (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name))))))))

(define-test (bookmarks in-categories)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (file (namestring (merge-pathnames "a.txt" dir))))
      (alexandria:write-string-into-file "a" file)
      (flet ((keep (name &rest categories)
               (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list name file 0 "" categories))))
             (shown (keys)
               ;; each from a list with no section folded
               (let ((list (tuatara.buffer:find-buffer "*Bookmarks*")))
                 (when list (tuatara.buffer:kill-buffer list)))
               (session-text (nth-value 1 (run-keys keys :width 120 :height 20)))))
        (unwind-protect
             (progn
               (keep "plain")
               (keep "home-only" "home")
               (keep "both" "work" "home")
               ;; a section a category, in the order of their names, then those in none; one in two in both
               (let ((text (shown "C-x m l")))
                 (true (< (search "▾ home (2)" text) (search "▾ work (1)" text) (search "▾ No category (1)" text)) "~a" text)
                 (is = 2 (count-matches "both " text))
                 (true (search "2 categories" text))
                 ;; a line of air after a section's last bookmark
                 (true (cl-ppcre:scan "home-only [^\\n]*\\n\\n▾ work" text)))
               ;; TAB on a heading folds its section, again unfolds; S-TAB all
               (let ((text (shown "C-x m l C-p TAB")))
                 (true (search "▸ home (2)" text))
                 (false (search "home-only" text)))
               (true (search "home-only" (shown "C-x m l C-p TAB TAB")))
               (let ((text (shown "C-x m l S-TAB")))
                 (true (search "▸ work" text))
                 (false (search "plain " text)))
               ;; deleted from work, still in home; from home then, gone
               (shown "C-x m l C-n C-n C-n C-n d x y e s RET")
               (is equal '("home") (tuatara.defaults::bookmark-categories (tuatara.defaults::find-bookmark "both")))
               (shown "C-x m l d x y e s RET")
               (false (tuatara.defaults::find-bookmark "both"))
               ;; categories set where it stands: the order kept
               (let ((order (tuatara.defaults::bookmark-names)))
                 (tuatara.defaults::set-bookmark-categories (tuatara.defaults::find-bookmark "plain") '("new"))
                 (is equal order (tuatara.defaults::bookmark-names))
                 (is equal '("new") (tuatara.defaults::bookmark-categories (tuatara.defaults::find-bookmark "plain")))))
          (dolist (name (tuatara.defaults::bookmark-names))
            (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name))))))))


(define-test (bookmarks categories-asked)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (path (merge-pathnames "notes.txt" dir)))
      (alexandria:write-string-into-file (format nil "one~%") path)
      (unwind-protect
           (progn
             ;; set: asked after the name, a new one typed
             (run-keys "C-x m m RET w o r k C-j RET" :file (namestring path) :width 100)
             (forget-file-buffers)
             (is equal '("work") (tuatara.defaults::bookmark-categories (tuatara.defaults::find-bookmark "notes.txt")))
             ;; c in the manager: its own written in first, another added
             (let ((list (tuatara.buffer:find-buffer "*Bookmarks*")))
               (when list (tuatara.buffer:kill-buffer list)))
             (run-keys "C-x m l c h o m e C-j RET" :width 100 :height 16)
             (is equal '("work" "home") (tuatara.defaults::bookmark-categories (tuatara.defaults::find-bookmark "notes.txt")))
             ;; set again, none chosen: its categories kept
             (run-keys "C-x m m RET RET" :file (namestring path) :width 100)
             (forget-file-buffers)
             (is equal '("work" "home") (tuatara.defaults::bookmark-categories (tuatara.defaults::find-bookmark "notes.txt"))))
        (forget-file-buffers)
        (dolist (name (tuatara.defaults::bookmark-names))
          (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name)))))))

(define-test (bookmarks safe-stays-safe)
  ;; a bookmark set in a file opened safely opens it safely again
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (path (merge-pathnames "evidence.txt" dir)))
      (alexandria:write-string-into-file (format nil "one~%two~%") path)
      (unwind-protect
           (progn
             (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET C-n C-x m m RET RET" (rimu-typed (namestring path))) :width 160)
             (true (sixth (tuatara.defaults::find-bookmark "evidence.txt")))
             (forget-file-buffers)
             (let ((editor (nth-value 1 (run-keys "C-x m b e v i d e n c e RET" :width 160))))
               (true (tuatara.io:safe-visit-p (session-buffer editor)))
               (is = 4 (session-point editor))))
        (forget-file-buffers)
        (dolist (name (tuatara.defaults::bookmark-names))
          (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name)))))))

(define-test (bookmarks inside-a-character)
  ;; text of three-octet characters: the place and the text after it kept and
  ;; found again on the characters' starts, an octet inside one never read from
  (let ((b (tuatara.buffer:make-buffer "euros" :text (make-string 100 :initial-element (code-char #x20AC)))))
    (unwind-protect
         (progn
           (is string= (make-string 32 :initial-element (code-char #x20AC)) (tuatara.defaults::bookmark-context b 3))
           (is string= (make-string 32 :initial-element (code-char #x20AC)) (tuatara.defaults::bookmark-context b 4))
           (is = 3 (tuatara.defaults::bookmark-place b 4 (string (code-char #x20AC))))
           (is = 3 (tuatara.defaults::bookmark-place b 5 "not there"))
           (is = 300 (tuatara.defaults::bookmark-place b 1000 "")))
      (tuatara.buffer:kill-buffer b))))

(define-test (bookmarks gone-said-so)
  ;; a bookmark whose file is gone: said so, no new file made, the order kept
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (missing (namestring (merge-pathnames "missing.txt" dir)))
          (here (namestring (merge-pathnames "here.txt" dir))))
      (alexandria:write-string-into-file "x" here)
      (unwind-protect
           (progn
             (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "gone" missing 0 "")))
             (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "here" here 0 "")))
             (multiple-value-bind (f editor) (run-keys "C-x m b g o n e RET" :width 120)
               (true (tuatara.frontend.fake:fake-frame-showing f "is no longer there"))
               (false (equal "missing.txt" (tuatara.buffer:buffer-name (session-buffer editor)))))
             (false (find missing tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file :test #'equal))
             (false (probe-file missing))
             (is equal '("here" "gone") (tuatara.defaults::bookmark-names)))
        (forget-file-buffers)
        (dolist (name (tuatara.defaults::bookmark-names))
          (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name)))))))

(define-test (bookmarks no-category-named-so)
  ;; a category called as the section of those in none is a section of its own:
  ;; folded alone, and a bookmark deleted from it stays in its other categories
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (file (namestring (merge-pathnames "a.txt" dir))))
      (alexandria:write-string-into-file "a" file)
      (flet ((shown (keys)
               (let ((list (tuatara.buffer:find-buffer "*Bookmarks*")))
                 (when list (tuatara.buffer:kill-buffer list)))
               (session-text (nth-value 1 (run-keys keys :width 120 :height 20)))))
        (unwind-protect
             (progn
               (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "plain" file 0 "" '())))
               (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "named" file 0 "" '("No category" "work"))))
               (let ((text (shown "C-x m l C-p TAB")))
                 (true (search "▸ No category (1)" text) "~a" text)
                 (true (search "▾ No category (1)" text) "~a" text))
               ;; line 3: named, in the category called No category
               (shown "C-x m l d x y e s RET")
               (is equal '("work") (tuatara.defaults::bookmark-categories (tuatara.defaults::find-bookmark "named")))
               (true (tuatara.defaults::find-bookmark "plain")))
          (dolist (name (tuatara.defaults::bookmark-names))
            (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name))))))))

(define-test (bookmarks rename-asks-before-replacing)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (one (namestring (merge-pathnames "one.txt" dir)))
          (two (namestring (merge-pathnames "two.txt" dir))))
      (alexandria:write-string-into-file "1" one)
      (alexandria:write-string-into-file "2" two)
      (flet ((manage (keys)
               (let ((list (tuatara.buffer:find-buffer "*Bookmarks*")))
                 (when list (tuatara.buffer:kill-buffer list)))
               (run-keys keys :width 120 :height 20)))
        (unwind-protect
             (progn
               (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "one" one 0 "")))
               (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "two" two 0 "")))
               ;; two, the latest, renamed one: no keeps both
               (manage "C-x m l r C-a C-k o n e RET n o RET")
               (is equal '("one" "two") (sort (tuatara.defaults::bookmark-names) #'string<))
               (manage "C-x m l r C-a C-k o n e RET y e s RET")
               (is equal '("one") (tuatara.defaults::bookmark-names))
               (is string= two (second (tuatara.defaults::find-bookmark "one"))))
          (dolist (name (tuatara.defaults::bookmark-names))
            (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name))))))))

(define-test (bookmarks marks-counted-and-shown)
  ;; C-u 2 m marks two; a mark in a folded section is shown before it is acted on
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (file (namestring (merge-pathnames "a.txt" dir))))
      (alexandria:write-string-into-file "a" file)
      (flet ((manage (keys)
               (let ((list (tuatara.buffer:find-buffer "*Bookmarks*")))
                 (when list (tuatara.buffer:kill-buffer list)))
               (session-text (nth-value 1 (run-keys keys :width 120 :height 20)))))
        (unwind-protect
             (progn
               (dolist (name '("x" "y" "z"))
                 (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list name file 0 "" '("c1")))))
               (let ((text (manage "C-x m l C-u 2 m")))
                 (is = 2 (count-if (lambda (l) (uiop:string-prefix-p ">" l)) (uiop:split-string text :separator '(#\Newline)))))
               ;; z, the latest, marked for deletion; the section folded; x unfolds it and asks
               (let ((text (manage "C-x m l d C-p TAB x y e s RET")))
                 (false (tuatara.defaults::find-bookmark "z"))
                 (true (search "▾ c1" text) "~a" text)
                 (true (search "x " text))))
          (dolist (name (tuatara.defaults::bookmark-names))
            (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name))))))))

(define-test (bookmarks other-window-on-a-heading)
  ;; o on a section's heading folds the section, as RET does
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (file (namestring (merge-pathnames "a.txt" dir))))
      (alexandria:write-string-into-file "a" file)
      (unwind-protect
           (progn
             (tuatara.defaults::state-list-put tuatara.defaults::*bookmarks* (list (list "x" file 0 "" '("c1"))))
             (let ((list (tuatara.buffer:find-buffer "*Bookmarks*")))
               (when list (tuatara.buffer:kill-buffer list)))
             (let ((text (session-text (nth-value 1 (run-keys "C-x m l C-p o" :width 120 :height 20)))))
               (true (search "▸ c1" text) "~a" text)))
        (dolist (name (tuatara.defaults::bookmark-names))
          (tuatara.defaults::state-list-drop tuatara.defaults::*bookmarks* (list name)))))))

(define-test (bookmarks kinds-by-native-names)
  ;; what a bookmark keeps a place in, its name taken as it is: a [ or * in it only a character
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (folder (concatenate 'string root "case [1]*"))
           (file (concatenate 'string root "notes [old].txt")))
      (tuatara.io:ensure-directories (concatenate 'string folder "/"))
      (tuatara.io:write-private-file file "x")
      (is string= "folder" (tuatara.defaults::bookmark-kind (list "f" folder)))
      (is string= "file" (tuatara.defaults::bookmark-kind (list "n" file)))
      (is string= "gone" (tuatara.defaults::bookmark-kind (list "g" (concatenate 'string root "none")))))))
