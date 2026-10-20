;;;; test/popup.lisp --- Completing a word from a list beside the cursor: C-M-i, and TAB at M-:.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test in-text-completion
  :description "C-M-i lists the completions of the Lisp name at point beside it, narrowed as more is
typed; C-n, C-p, RET, TAB and C-g work the list; a lone completion goes in at once; M-: completes
the same way over its prompt; the boxes are laid over the screen's rows.")

(define-test (in-text-completion rows-laid-over)
  (flet ((spliced (string spans column box box-spans)
           (multiple-value-list (tuatara.command:splice-row string spans column box box-spans))))
    ;; the box's columns show it, the rest of the row stays, spans cut and moved along
    (is equal '("abXYef" ((0 2 :x) (2 4 :b) (4 6 :x))) (spliced "abcdef" '((0 6 :x)) 2 "XY" '((0 2 :b))))
    ;; past the end of the row's text: spaces up to it
    (is equal '("ab   X" ()) (spliced "ab" '() 5 "X" '()))
    ;; a wide character cut by an edge becomes spaces
    (is string= "a Xbc" (first (spliced (format nil "a~cbc" (code-char #x4E2D)) '() 2 "X" '())))
    (is string= (format nil "a Xb~cc" (code-char #x4E2D)) (first (spliced (format nil "a~cb~cc" (code-char #x4E2D) (code-char #x4E2D)) '() 2 "X" '())))
    (is string= (format nil "a~cb Xc" (code-char #x4E2D)) (first (spliced (format nil "a~cb~cc" (code-char #x4E2D) (code-char #x4E2D)) '() 5 "X" '()))))
  ;; a row under a face of its own keeps it outside the box
  (multiple-value-bind (string spans face)
      (tuatara.command:lay-boxes-over "ab" '() :mode-line 0 (list (list 0 1 (list (list "X" '((0 1 :popup)))))) 4)
    (is string= "aX  " string)
    (false face)
    (is equal '((0 1 :mode-line) (1 2 :popup) (2 4 :mode-line)) spans)))

(define-test (in-text-completion names-not-utf-8-written-out)
  ;; an octet that is not UTF-8 in a name the list shows is written \xNN, in the four
  ;; columns it takes, as in the mode line, and the rows stay lined up
  (let* ((names (list (format nil "bad~cname.txt" (code-char #xDCE9)) "good.txt"))
         (popup (tuatara.command::%make-popup :minibuffer 0 "" '() nil nil nil)))
    (setf (tuatara.command::popup-candidates popup)
          (map 'simple-vector #'tuatara.command:make-candidate names))
    (multiple-value-bind (rows width) (tuatara.command::popup-list-box popup 60)
      (true (search "bad\\xE9name.txt" (first (first rows))) "~s" rows)
      (false (find-if #'tuatara.text:raw-byte-code-p (first (first rows)) :key #'char-code))
      (is = width (tuatara.command:panel-columns (first (first rows))))
      (is = width (tuatara.command:panel-columns (first (second rows)))))))

(define-test (in-text-completion lisp-names)
  (let ((user (find-package :tuatara-user)))
    (flet ((names (word context) (mapcar #'tuatara.command:candidate-string
                                         (tuatara.defaults::lisp-name-candidates word user context))))
      ;; where the name goes
      (is eq :operator (tuatara.defaults::lisp-completion-context "(buf" 1))
      (is eq :operator (tuatara.defaults::lisp-completion-context "#'buf" 2))
      (is eq :any (tuatara.defaults::lisp-completion-context "'(buf" 2))
      (is eq :any (tuatara.defaults::lisp-completion-context "(f buf" 3))
      ;; the name around the cursor, whatever comes before and after it
      (is equal '(1 10) (multiple-value-list (tuatara.defaults::text-word-bounds "(buffer-na)" 5 #'tuatara.defaults::lisp-name-part-p)))
      (is equal '(2 5) (multiple-value-list (tuatara.defaults::text-word-bounds "#'car" 5 #'tuatara.defaults::lisp-name-part-p)))
      ;; operators after a bracket, the editor's own functions among them; variables elsewhere
      (true (member "buffer-substring" (names "buffer" :operator) :test #'string=))
      (true (member "mapcar" (names "map" :operator) :test #'string=))
      (false (member "*tab-width*" (names "tab" :operator) :test #'string=))
      (true (member "*tab-width*" (names "tab" :any) :test #'string=))
      ;; keywords, and a package's names with its prefix
      (true (member ":test-not" (names ":test" :any) :test #'string=))
      (true (member "cl:mapcar" (names "cl:map" :operator) :test #'string=))
      (true (member "tuatara.defaults::lisp-name-candidates" (names "tuatara.defaults::lisp-name" :operator) :test #'string=))
      (false (names "no-such-package:x" :any))
      ;; what each is, and what the box beside the list says
      (is string= "function" (tuatara.defaults::lisp-name-kind 'tuatara-user:buffer-substring))
      (is string= "macro" (tuatara.defaults::lisp-name-kind 'tuatara-user:save-excursion))
      (is string= "variable" (tuatara.defaults::lisp-name-kind 'tuatara-user:*tab-width*))
      (is string= "keyword" (tuatara.defaults::lisp-name-kind :test))
      (true (search "(buffer-substring start end)" (tuatara.defaults::lisp-name-documentation 'tuatara-user:buffer-substring)))
      (true (search "*tab-width* = 4" (tuatara.defaults::lisp-name-documentation 'tuatara-user:*tab-width*))))))

(define-test (in-text-completion in-a-buffer)
  (flet ((typed (keys) (session-text (nth-value 1 (fresh-start keys :width 120))))
         (lisp (string) (key-string string)))
    ;; one completion alone goes in at once
    (true (search "(buffer-substring" (typed (format nil "o ~a C-M-i" (lisp "(buffer-subs")))))
    ;; as it does from a terminal, where C-M-i is M-TAB
    (true (search "(buffer-substring" (typed (format nil "o ~a M-TAB" (lisp "(buffer-subs")))))
    ;; several (buffer-size, buffer-string, buffer-substring): listed, the first chosen; C-n the next, RET puts it in
    (let ((text (typed (format nil "o ~a C-M-i C-n RET" (lisp "(buffer-s")))))
      (true (search "(buffer-string" text))
      ;; RET went to the list, not into the text
      (false (search (format nil "(buffer-string~%") text)))
    ;; C-p from the first wraps to the last, C-n back to the first
    (let ((text (typed (format nil "o ~a C-M-i C-p C-n TAB" (lisp "(buffer-s")))))
      (true (search "(buffer-size" text)))
    ;; typing narrows the list
    (true (search "(buffer-string" (typed (format nil "o ~a C-M-i t r RET" (lisp "(buffer-s")))))
    ;; words in any order after M-SPC
    (true (search "(get-buffer-create" (typed (format nil "o ~a C-M-i M-SPC c r e RET" (lisp "(buffer")))))
    ;; C-g closes it, the word as typed
    (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-M-i C-g" (lisp "(buffer-s")) :width 120)
      (declare (ignore f))
      (true (search "(buffer-s" (session-text editor)))
      (false (search "(buffer-size" (session-text editor)))
      (false (tuatara.command:editor-popup editor)))
    ;; leaving INSERT state closes it too (C-g closes the list first: here a key of one's own)
    (multiple-value-bind (f editor)
        (fresh-start (format nil "M-: ~a RET o ~a C-M-i <f5>" (lisp "(bind-key \"<f5>\" 'tuatara-normal)") (lisp "(buffer-"))
                     :width 120)
      (declare (ignore f))
      (false (tuatara.command:editor-popup editor))
      (is eq :normal (tuatara.defaults:modal-state (session-buffer editor))))
    ;; the minibuffer asking something closes it, and its keys are the minibuffer's
    (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-M-i M-x C-n C-g" (lisp "(buffer-s")) :width 120)
      (declare (ignore f))
      (false (tuatara.command:editor-popup editor))
      (true (search "(buffer-s" (session-text editor)))
      (false (search "(buffer-size" (session-text editor))))
    ;; and so does typing what cannot be part of a name
    (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-M-i SPC" (lisp "(buffer-")) :width 120)
      (declare (ignore f))
      (false (tuatara.command:editor-popup editor)))
    ;; a package's names, keywords
    (true (search "(cl:mapcar" (typed (format nil "o ~a C-M-i RET" (lisp "(cl:mapcar")))))
    (true (search ":test-not" (typed (format nil "o ~a C-M-i RET" (lisp "(f :test-no")))))
    ;; nothing to offer
    (true (showing (fresh-start (format nil "o ~a C-M-i" (lisp "(zzqqxx")) :width 120) "No match"))
    (true (showing (fresh-start (format nil "o ~a C-M-i" (lisp "(buffer-substring")) :width 120) "Sole completion"))))

(define-test (in-text-completion list-shown)
  (let ((f (fresh-start (format nil "o ~a C-M-i" (key-string "(buffer-s")) :width 120)))
    ;; the list: each name and what it is, the chosen one first
    (true (showing f " buffer-size " "function"))
    (true (showing f " buffer-string " "function"))
    ;; beside it, the chosen one's parameters and documentation
    (true (showing f " buffer-size " "(buffer-size &optional buffer)"))
    (true (showing f "Return how many characters"))))

(define-test (in-text-completion at-m-colon)
  ;; TAB completes the name at the cursor over the prompt, RET puts it in, the next RET evaluates
  (let ((f (fresh-start (format nil "M-: ~a TAB RET ) RET" (key-string "(buffer-na")) :width 120)))
    (true (showing f "buffer-name" "function"))
    (true (showing f "=> \"*lisp-scratch*\"")))
  ;; a lone completion goes in at once, and C-M-i does as TAB
  (true (showing (fresh-start (format nil "M-: ~a C-M-i ) RET" (key-string "(buffer-subs")) :width 120)
                 "Eval: (buffer-substring)"))
  ;; C-g closes the list and leaves the prompt, still reading
  (true (showing (fresh-start (format nil "M-: ~a TAB C-g ~a RET" (key-string "(buffer-") (key-string "name)")) :width 120)
                 "=> \"*lisp-scratch*\"")))

(define-test (in-text-completion where-nothing-can-be-typed)
  ;; a read-only buffer refuses, and so does the hex view
  (true (showing (fresh-start (format nil "o ~a C-g C-x C-q C-M-i" (key-string "(buffer-")) :width 120)
                 "Buffer is read-only: *lisp-scratch*"))
  (true (showing (fresh-start (format nil "M-x h e x - m o d e RET C-M-i") :width 120)
                 "Nothing to complete in the hex view")))

(defun wait-for-path-listing ()
  "Wait, five seconds at most, until the directory of the path typed has been
read (TUATARA.DEFAULTS::*PATH-LISTING*), for the editor to list it next."
  (let ((until (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
    (loop for listing = tuatara.defaults::*path-listing*
          until (or (null listing) (tuatara.defaults::gathering-done (fourth listing))
                    (> (get-internal-real-time) until))
          do (sleep 0.001))))

(define-test (in-text-completion paths)
  ;; typing a slash in a path lists its directory's entries beside the cursor, none chosen
  (with-test-directory (dir)
    (put-file (merge-pathnames "alpha.txt" dir) "a")
    (put-file (merge-pathnames "almond.txt" dir) "a")
    (ensure-directories-exist (merge-pathnames "beta/" dir))
    (put-file (merge-pathnames "beta/gamma.org" dir) "g")
    (let ((native (namestring dir)) (sessions 0))
      (flet ((typed (&rest keys)
               ;; each session in a file of its own: a file's buffer outlives the session
               (run-keys (cons "i" (loop for k in keys
                                         append (if (eq k :listed) (list #'wait-for-path-listing) (list k))))
                         :file (namestring (put-file (merge-pathnames (format nil "z~d.txt" (incf sessions)) dir) ""))
                         :width 160 :height 12))
             (text (editor) (subseq (session-text editor) 0 (position #\Newline (session-text editor) :from-end t))))
        ;; the entries listed, directories first, none chosen
        (multiple-value-bind (f editor) (typed (key-string native) :listed)
          (true (showing f " beta/ "))
          (true (showing f " alpha.txt "))
          (true (tuatara.command:editor-popup editor))
          (is = -1 (tuatara.command:popup-index (tuatara.command:editor-popup editor))))
        ;; typing narrows the list, TAB puts the first in
        (is string= (format nil "~aalmond.txt" native)
            (text (nth-value 1 (typed (key-string native) :listed "a l m TAB"))))
        ;; RET with none chosen ends the line, the path as typed
        (multiple-value-bind (f editor) (typed (key-string native) :listed "a l RET")
          (declare (ignore f))
          (true (search (format nil "~aal~%" native) (session-text editor)))
          (false (tuatara.command:editor-popup editor)))
        ;; C-n chooses the first (the shorter of two as close) and RET then puts it in
        (is string= (format nil "~aalpha.txt" native)
            (text (nth-value 1 (typed (key-string native) :listed "a l C-n RET"))))
        ;; a directory put in lists its own entries in turn
        (multiple-value-bind (f editor) (typed (key-string native) :listed "b e TAB" :listed)
          (true (showing f " gamma.org "))
          (is string= (format nil "~abeta/" native) (text editor)))
        ;; a path not starting with / or ~ is taken from the buffer's directory
        (true (showing (typed (key-string "./") :listed) " almond.txt "))
        ;; a file: link's path
        (true (showing (typed (key-string (format nil "[[file:~a" native)) :listed) " beta/ "))
        ;; C-g closes the list and it stays closed while the name is typed
        (false (tuatara.command:editor-popup (nth-value 1 (typed (key-string native) :listed "C-g a l"))))
        ;; no directory there, no list, nothing said
        (multiple-value-bind (f editor) (typed (key-string "/no/such/dir/") :listed)
          (false (tuatara.command:editor-popup editor))
          (false (showing f "No match")))
        ;; typing without a slash reads nothing
        (is eq nil (progn (typed (key-string "plain words")) tuatara.defaults::*path-listing*))
        ;; *COMPLETE-PATHS* NIL lists nothing
        (let ((tuatara-user:*complete-paths* nil))
          (false (tuatara.command:editor-popup (nth-value 1 (typed (key-string native) :listed)))))
        ;; *COMPLETE-PATHS-MODES* says otherwise for a mode, and for the modes made from it
        (let ((tuatara-user:*complete-paths* nil) (tuatara-user:*complete-paths-modes* '((text . t))))
          (true (tuatara.command:editor-popup (nth-value 1 (typed (key-string native) :listed)))))
        (let ((tuatara-user:*complete-paths-modes* '((text-mode . nil))))
          (false (tuatara.command:editor-popup (nth-value 1 (typed (key-string native) :listed)))))
        (let ((tuatara-user:*complete-paths-modes* '((fundamental-mode . nil))))
          (false (tuatara.command:editor-popup (nth-value 1 (typed (key-string native) :listed)))))))))

(define-test (in-text-completion paths-folders-first)
  ;; the list of a folder's entries beside the cursor shows its folders first, then its files
  (with-test-directory (dir)
    (dolist (name '("b" "-x" "_y"))
      (put-file (merge-pathnames name dir) "a"))
    (dolist (name '("adir/" "zdir/"))
      (ensure-directories-exist (merge-pathnames name dir)))
    (multiple-value-bind (f editor)
        (run-keys (list "i" (key-string (namestring dir)) #'wait-for-path-listing)
                  :file (namestring (put-file (merge-pathnames "notes.txt" dir) ""))
                  :width 160 :height 12)
      (declare (ignore f))
      (let ((popup (tuatara.command:editor-popup editor)))
        (true popup)
        (when popup
          (is equal '("adir/" "zdir/")
              (subseq (map 'list #'tuatara.command:candidate-string (tuatara.command:popup-candidates popup)) 0 2)))))))

(define-test (in-text-completion opening-paths)
  ;; C-x C-p opens the path at the cursor: a folder in a list of it, a file in its buffer
  (with-test-directory (dir)
    (put-file (merge-pathnames "alpha.txt" dir) "alpha's text")
    (ensure-directories-exist (merge-pathnames "beta/" dir))
    (let ((native (namestring dir)) (sessions 0))
      (flet ((opened (typed &optional (keys "C-x C-p"))
               ;; each session in a file of its own: a file's buffer outlives the session
               (let ((tuatara-user:*complete-paths* nil))
                 (run-keys (list "i" (key-string typed) keys)
                           :file (namestring (put-file (merge-pathnames (format nil "z~d.txt" (incf sessions)) dir) ""))
                           :width 160 :height 12)))
             (shown (editor) (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        ;; a folder, with its slash or without, listed
        (dolist (path (list (format nil "~abeta/" native) (format nil "~abeta" native)))
          (let ((b (shown (nth-value 1 (opened path)))))
            (is string= (format nil "~abeta/" native) (tuatara.buffer:buffer-local b :directory))))
        ;; a file visited, the cursor anywhere in its path
        (let ((b (shown (nth-value 1 (opened (format nil "~aalpha.txt" native) "C-b C-b C-b C-x C-p")))))
          (is string= (format nil "~aalpha.txt" native) (namestring (tuatara.buffer:buffer-file b)))
          (false (tuatara.io:safe-visit-p b)))
        (tuatara.buffer:kill-buffer (find (format nil "~aalpha.txt" native) tuatara.buffer:*buffers*
                                          :key (lambda (b) (let ((file (tuatara.buffer:buffer-file b))) (and file (namestring file))))
                                          :test #'equal))
        ;; taken from the buffer's directory; a file: link's; the full stop of a sentence left out
        (dolist (typed (list "./alpha.txt" (format nil "[[file:~aalpha.txt][alpha]]" native) (format nil "see ~aalpha.txt." native)))
          (let ((b (shown (nth-value 1 (opened typed (if (char= #\] (char typed (1- (length typed))))
                                                         "C-b C-b C-b C-b C-b C-b C-b C-b C-b C-b C-x C-p"
                                                         "C-x C-p"))))))
            (is string= (format nil "~aalpha.txt" native) (namestring (tuatara.buffer:buffer-file b)) typed)
            (tuatara.buffer:kill-buffer b)))
        ;; C-u opens it safely
        (let ((b (shown (nth-value 1 (opened (format nil "~aalpha.txt" native) "C-u C-x C-p")))))
          (true (tuatara.io:safe-visit-p b))
          (tuatara.buffer:kill-buffer b))
        ;; nothing there, or no path: said, nothing made
        (multiple-value-bind (f editor) (opened (format nil "~anone.txt" native))
          (true (showing f (format nil "No file or folder: ~anone.txt" native)))
          (false (probe-file (merge-pathnames "none.txt" dir)))
          (is string= "z8.txt" (tuatara.buffer:buffer-name (shown editor))))
        (true (showing (opened "  " "C-x C-p") "No path at point"))))))

(define-test (in-text-completion documentation-filled-anew)
  ;; a docstring's own line breaks give way to the box's width; blank lines and indented lines stay
  (is equal '("The buffer called NAME, made" "when there is none." "" "  example" "Done.")
      (tuatara.command::wrap-text (format nil "The buffer called NAME,~%made when there is none.~%~%  example~%Done.") 30))
  (is equal '("one two three" "four") (tuatara.command::wrap-text (format nil "one~%two three four") 13)))
