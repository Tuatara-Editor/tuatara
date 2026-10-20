;;;; test/snippets.lisp --- Snippets: text with fields, put in by a key and TAB or by C-c y.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test snippets
  :description "A snippet, a file in a mode's folder, is put in by its key and TAB or chosen with C-c y; its fields ($1, ${1:text}, ${1|a,b|}, $0) are gone through with TAB and S-TAB, a number used again filled in as the field is, $NAME standing for a value; nothing in it is run.")

(defmacro with-snippets ((&rest files) &body body)
  "Run BODY with the snippets FILES, each (MODE NAME TEXT), in a folder of their own, modal editing off."
  (let ((dir (gensym)))
    `(with-test-directory (,dir)
       (loop for (mode name text) in (list ,@(mapcar (lambda (f) `(list ,@f)) files))
             do (let ((path (merge-pathnames (format nil "~a/~a" mode name) ,dir)))
                  (ensure-directories-exist path)
                  (alexandria:write-string-into-file text path)))
       (let ((tuatara-user:*snippet-directories* (list (namestring ,dir)))
             (tuatara.defaults::*modal-editing* nil))
         ,@body))))

(define-test (snippets body-read)
  (is equal '("a " (:field 1 ()) " " (:field 2 ("b")) " " (:choice 3 ("x" "y,z")) " " (:field 0 ())
              " $ " (:value "CURRENT_YEAR" ()) " " (:value "NOPE" ("none")))
      (tuatara.defaults::parse-snippet-body "a $1 ${2:b} ${3|x,y\\,z|} $0 \\$ $CURRENT_YEAR ${NOPE:none}"))
  ;; a field within a field's text
  (is equal '((:field 1 ("f(" (:field 2 ("x")) ")")))
      (tuatara.defaults::parse-snippet-body "${1:f(${2:x})}")))

(define-test (snippets fields-and-mirrors)
  (with-snippets (("fundamental-mode" "pair" (format nil "# key: pr~%# --~%(${1:name} . $1) ${2:value}$0")))
    ;; the key and TAB: the snippet, its first field's text selected; typing replaces it, the mirror follows
    (is string= "(abc . abc) 42" (session-text (nth-value 1 (run-keys "p r TAB a b c TAB 4 2" :text ""))))
    ;; untouched, a field keeps its text
    (is string= "(name . name) 7" (session-text (nth-value 1 (run-keys "p r TAB TAB 7" :text ""))))
    ;; S-TAB goes back to the field before
    (is string= "(x . x) value" (session-text (nth-value 1 (run-keys "p r TAB TAB S-TAB x" :text ""))))
    ;; no key before the cursor: TAB does what it does (on to the next tab stop)
    (is string= "q   " (session-text (nth-value 1 (run-keys "q TAB" :text ""))))))

(define-test (snippets values-and-choices)
  (with-snippets (("fundamental-mode" "dated" (format nil "# key: dd~%# --~%Year $CURRENT_YEAR by $UNKNOWN_NAME.$0"))
                  ("fundamental-mode" "pick" (format nil "# name: Pick one~%# key: pk~%# --~%${1|red,green,blue|}!")))
    (multiple-value-bind (s m h d mo year) (decode-universal-time (get-universal-time))
      (declare (ignore s m h d mo))
      (is string= (format nil "Year ~d by ." year) (session-text (nth-value 1 (run-keys "d d TAB" :text "")))))
    ;; a choice: its options offered beside the cursor
    (let ((f (run-keys "p k TAB" :text "" :width 60 :height 12)))
      (true (tuatara.frontend.fake:fake-frame-showing f "green"))
      (true (tuatara.frontend.fake:fake-frame-showing f "blue"))
      ;; in the order the snippet gives them
      (flet ((row (word) (loop for r below 12 when (search word (tuatara.frontend.fake:fake-screen-line f r)) return r)))
        (true (< (row "red") (row "green") (row "blue")))))
    ;; chosen by name with C-c y
    (is string= "green!" (session-text (nth-value 1 (run-keys "C-c y P i c k RET g r e e n RET" :text ""))))
    ;; not where nothing is typed: the buffer list, the bookmark manager, help
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x C-b C-c y" :text "" :width 100) "C-c y is undefined"))
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x m l C-c y" :text "" :width 100) "C-c y is undefined"))))

(define-test (snippets org-blocks)
  ;; Tuatara's own, for Org: <q a quote, the cursor inside, the line's indentation kept
  (let ((tuatara.defaults::*modal-editing* nil))
    (is string= (format nil "  #+begin_quote~%  Hi~%  #+end_quote")
        (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET SPC SPC < q TAB H i" :text ""))))))

(define-test (snippets whose-and-copied)
  ;; C-c y says whose each snippet is; copy-snippet makes one of Tuatara's yours, which then takes its place
  (with-snippets (("org-mode" "mine" (format nil "# name: Mine~%# key: <m~%# --~%mine")))
    (let ((f (run-keys "M-x o r g - m o d e RET C-c y" :text "" :width 100 :height 30)))
      (true (tuatara.frontend.fake:fake-frame-showing f "<m  yours (org-mode)"))
      (true (tuatara.frontend.fake:fake-frame-showing f "<q  Tuatara's (org-mode)")))
    (run-keys "M-x c o p y - s n i p p e t RET Q u o t e RET" :text "" :width 100)
    (let* ((dir (first tuatara-user:*snippet-directories*))
           (copy (merge-pathnames "org-mode/quote-block" dir)))
      (true (probe-file copy))
      (true (search "#+begin_quote" (alexandria:read-file-into-string copy)))
      ;; changed, the copy is what <q puts in
      (alexandria:write-string-into-file (format nil "# name: Quote~%# key: <q~%# --~%> $0") copy :if-exists :supersede)
      (is string= "> hi" (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET < q TAB h i" :text "")))))))

(define-test (snippets nested-too-deep)
  ;; fields within fields are read so deep, and no deeper: the rest is text, never a stack overflow
  (let* ((deep (with-output-to-string (out)
                 (dotimes (i 100000) (write-string "${1:" out))
                 (write-string "x" out)
                 (dotimes (i 100000) (write-string "}" out))))
         (parts (tuatara.defaults::parse-snippet-body deep)))
    (true (consp parts))))

(define-test (snippets fields-side-by-side-and-within)
  (with-snippets (("fundamental-mode" "adjacent" (format nil "# key: aj~%# --~%${1:a}$2 $1"))
                  ("fundamental-mode" "two" (format nil "# key: tw~%# --~%$1$2|"))
                  ("fundamental-mode" "nested" (format nil "# key: ns~%# --~%${1:f(${2:x})} $2"))
                  ("fundamental-mode" "itself" (format nil "# key: sf~%# --~%${1:x $1}")))
    ;; what is typed in a field beside another is its own, not its neighbour's
    (is string= "az a" (session-text (nth-value 1 (run-keys "a j TAB TAB z" :text ""))))
    (is string= "xy|" (session-text (nth-value 1 (run-keys "t w TAB x TAB y" :text ""))))
    ;; a field's text typed over: the fields within it are gone, their mirrors left as they were
    (is string= "z x" (session-text (nth-value 1 (run-keys "n s TAB z" :text ""))))
    (is string= "f(q) q" (session-text (nth-value 1 (run-keys "n s TAB TAB q" :text ""))))
    ;; a field's number within it is no mirror of it
    (is string= "x " (session-text (nth-value 1 (run-keys "s f TAB" :text ""))))))

(define-test (snippets undo-steps)
  (with-snippets (("fundamental-mode" "adjacent" (format nil "# key: aj~%# --~%${1:a}$2 $1")))
    ;; undone, the snippet gives its key back
    (is string= "aj" (session-text (nth-value 1 (run-keys "a j TAB C-/" :text ""))))
    ;; a field typed over is undone at once, its mirrors with it
    (is string= "a a" (session-text (nth-value 1 (run-keys "a j TAB q q C-/" :text ""))))
    (is string= "aj" (session-text (nth-value 1 (run-keys "a j TAB q q C-/ C-/" :text ""))))))

(define-test (snippets ends-choices-and-files)
  (with-snippets (("fundamental-mode" "zero" (format nil "# key: zp~%# --~%p${0:q}r${1:s}"))
                  ("fundamental-mode" "windows" (format nil "# key: cr~c~%# --~c~%line1~c~%line2~c~%" #\Return #\Return #\Return #\Return)))
    ;; $0's text is selected for typing to replace
    (is string= "pwrs" (session-text (nth-value 1 (run-keys "z p TAB TAB w" :text ""))))
    ;; a file whose lines end in carriage returns puts in lines without them
    (is string= (format nil "line1~%line2") (session-text (nth-value 1 (run-keys "c r TAB" :text ""))))
    ;; the snippets read are kept, and read again once their files change
    (let* ((place (tuatara.io:native (tuatara.defaults::own-folder (first tuatara-user:*snippet-directories*) "fundamental-mode")))
           (kept (cdr (gethash place tuatara.defaults::*snippet-folders*))))
      (true kept)
      (is eq (first kept) (first (tuatara.defaults::user-snippets-of "fundamental-mode")))
      (alexandria:write-string-into-file (format nil "# key: zp~%# --~%changed and longer") (concatenate 'string place "zero")
                                         :if-exists :supersede)
      (is string= "changed and longer" (session-text (nth-value 1 (run-keys "z p TAB" :text ""))))))
  ;; a choice never closed is text
  (is equal '("x ${1|a,b") (tuatara.defaults::parse-snippet-body "x ${1|a,b"))
  ;; the date's values as the dates write them
  (let ((b (tuatara.buffer:make-buffer "*snippet-values*" :text "")))
    (unwind-protect
         (progn (is string= (tuatara.defaults::format-time "%B" (get-universal-time))
                    (tuatara.defaults::snippet-value "CURRENT_MONTH_NAME" b nil))
                (is string= (tuatara.defaults::format-time "%a" (get-universal-time))
                    (tuatara.defaults::snippet-value "CURRENT_DAY_NAME_SHORT" b nil)))
      (tuatara.buffer:kill-buffer b))))

(define-test (snippets long-lines-and-many-fields)
  (with-snippets (("fundamental-mode" "adjacent" (format nil "# key: aj~%# --~%${1:a}$2 $1")))
    ;; a key after a long line of characters of several octets each
    (let ((text (session-text (nth-value 1 (run-keys "M-> TAB" :text (concatenate 'string (make-string 300 :initial-element #\€) " aj"))))))
      (true (alexandria:ends-with-subseq " a a" text))))
  ;; twenty thousand fields put in at once, without a wait
  (with-snippets (("fundamental-mode" "many" (with-output-to-string (out)
                                               (format out "# key: mf~%# --~%")
                                               (loop for n from 1 to 20000 do (format out "${~d:f} " n)))))
    (let* ((started (get-internal-real-time))
           (text (session-text (nth-value 1 (run-keys "m f TAB" :text "")))))
      (true (< (- (get-internal-real-time) started) (* 4 internal-time-units-per-second)))
      (is = 40000 (length text)))))

(define-test (snippets built-in-ones-read-once)
  ;; Tuatara's own snippets are read once, not again at each TAB
  (let ((first (tuatara.defaults::built-in-snippets-of "org-mode")))
    (true first)
    (is eq first (tuatara.defaults::built-in-snippets-of "org-mode"))
    (is equal (loop for (mode nil nil) in tuatara.defaults::*built-in-snippets* when (string= mode "org-mode") count t)
        (length first))
    (let ((consed (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring (tuatara.defaults::built-in-snippets-of "org-mode"))))))
      (true (< consed 256) "~:d octets consed" consed))))

(define-test (snippets folder-in-the-configuration-folder)
  ;; with none set, your snippets are in snippets/ of the configuration folder as the
  ;; environment names it when asked, never a place fixed when Tuatara was built
  (let ((old (uiop:getenv "XDG_CONFIG_HOME")))
    (unwind-protect
         (let ((tuatara-user:*snippet-directories* nil) (tuatara.defaults:*config-file* nil))
           (sb-posix:setenv "XDG_CONFIG_HOME" "/tmp/tuatara-config-elsewhere" 1)
           (is equal '("/tmp/tuatara-config-elsewhere/tuatara/snippets/") (tuatara.defaults::snippet-directories))
           (is string= "/tmp/tuatara-config-elsewhere/tuatara/init.lisp" (namestring (tuatara.defaults::config-file)))
           (let ((tuatara-user:*snippet-directories* (list "/tmp/mine/")))
             (is equal '("/tmp/mine/") (tuatara.defaults::snippet-directories))))
      (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME")))))
