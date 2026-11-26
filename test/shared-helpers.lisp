;;;; test/shared-helpers.lisp --- The small helpers several parts of the editor share, each the one way of its kind.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test shared-helpers
  :description "The helpers several parts of the editor call for one kind of thing (an entry's name, a
duration in words, a file's size from its descriptor, the homes of settings and caches) give one answer
to every part that asks.")

(define-test (shared-helpers entry-names-judged-alike)
  ;; one rule for a directory entry's name, whether a server, git or a person gave it: never empty, . or
  ;; .., nor holding / or NUL; and, for a name typed, no control character
  (dolist (name '("notes.txt" ".hidden" "a b" "ü" "..x"))
    (true (tuatara.io:entry-name-p name) "~s" name)
    (true (tuatara.io:entry-name-p name :controls t) "~s" name))
  (dolist (name (list "" "." ".." "sub/x" "/" (format nil "a~cb" (code-char 0))))
    (false (tuatara.io:entry-name-p name) "~s" name)
    (false (tuatara.io:entry-name-p name :controls t) "~s" name))
  (dolist (name (list (format nil "a~cb" #\Tab) (format nil "a~c" (code-char 127))))
    (true (tuatara.io:entry-name-p name) "~s" name)
    (false (tuatara.io:entry-name-p name :controls t) "~s" name)))

(define-test (shared-helpers c-called-one-way)
  ;; C is called through functions made with DEFINE-C-FUNCTION, which the runtime holds so that every
  ;; layer can use it: the runtime's own calls, the syntax helper's and the terminal's go through it
  (false (find-symbol "FOREIGN-CALL" "TUATARA.RUNTIME"))
  (is eq (find-symbol "DEFINE-C-FUNCTION" "TUATARA.RUNTIME") (find-symbol "DEFINE-C-FUNCTION" "TUATARA.IO"))
  (let ((top (asdf:system-source-directory "tuatara")))
    (dolist (name '("src/runtime/pool.lisp" "src/runtime/threads.lisp" "src/runtime/locks.lisp"
                    "src/syntax/helper.lisp" "lisp/terminal/emulator.lisp" "src/text/mapping.lisp"
                    "src/text/utf8.lisp" "lisp/gpg/encrypted.lisp"))
      (false (search "alien-funcall" (uiop:read-file-string (merge-pathnames name top))) "~a" name))))

(define-test (shared-helpers small-helpers-in-their-layers)
  ;; the homes of settings and caches as the XDG variables say, else under the home given
  (is equal (tuatara.io:own-directory "XDG_CONFIG_HOME") (tuatara.io:config-home))
  (is equal (tuatara.io:own-directory "XDG_CACHE_HOME") (tuatara.io:cache-home))
  (is equal #p"/home/kea/.config/" (tuatara.io::xdg-home "TUATARA_TEST_NO_SUCH_VARIABLE" ".config/" "/home/kea"))
  ;; a file's size from its descriptor, and whether two stamps name one file
  (with-test-directory (dir)
    (let ((one (namestring (merge-pathnames "one.txt" dir))) (two (namestring (merge-pathnames "two.txt" dir))))
      (alexandria:write-string-into-file "twelve bytes" one)
      (alexandria:write-string-into-file "other" two)
      (let ((fd (sb-posix:open one sb-posix:o-rdonly)))
        (unwind-protect (is = 12 (tuatara.io:fd-size fd))
          (sb-posix:close fd)))
      (false (tuatara.io:fd-size -1))
      (true (tuatara.io:same-stamped-file-p (tuatara.io:path-stamp one) (tuatara.io:path-stamp one)))
      (false (tuatara.io:same-stamped-file-p (tuatara.io:path-stamp one) (tuatara.io:path-stamp two)))
      (false (tuatara.io:same-stamped-file-p (tuatara.io:path-stamp one) nil))))
  ;; a word quoted for the shell reads back as itself
  (dolist (word '("plain" "it's" "a b" "$HOME" ""))
    (is equal (list word) (tuatara.text:command-words (tuatara.text:shell-quoted word)) "~s" word))
  ;; a raw byte's octet
  (is = #x9f (tuatara.text:raw-byte-octet (+ #xdc00 #x9f)))
  ;; the head of a long line, cut before a character
  (let ((buffer (tuatara.buffer:make-buffer "*line head test*"
                                            :text (format nil "short~%~a" (make-string 600 :initial-element #\é)))))
    (unwind-protect
         (progn (is string= "short" (tuatara.buffer:buffer-line-head buffer 2))
                (is = 256 (length (tuatara.buffer:buffer-line-head buffer 7))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (shared-helpers durations-said-alike)
  ;; a compile's report, a job's and a server's timeout say a length of time the same way
  (is string= "1 second" (tuatara.dates:duration-words 1))
  (is string= "2 seconds" (tuatara.dates:duration-words 2.0))
  (is string= "0.5 seconds" (tuatara.dates:duration-words 1/2))
  (is string= "0.25 seconds" (tuatara.dates:duration-words 0.25))
  (is string= "0.012 seconds" (tuatara.dates:duration-words 0.012))
  (is string= "2.4 seconds" (tuatara.dates:duration-words 2.43))
  (is string= "1 minute 0 seconds" (tuatara.dates:duration-words 59.96))
  (is string= "3 minutes 12 seconds" (tuatara.dates:duration-words 192.4))
  (is string= "2 hours 5 minutes" (tuatara.dates:duration-words 7500))
  (is string= "1 day 0 hours" (tuatara.dates:duration-words 86400))
  (is string= "3 days 12 hours" (tuatara.dates:duration-words (* 84 3600)))
  (is string= "in 0.5 seconds" (tuatara.defaults::job-done-words 0.5 0))
  (is string= "no answer within 2 seconds (*lsp-wait*)" (tuatara.lsp::lsp-error-words :timeout :seconds 2)))

(define-test (shared-helpers answers-trimmed-when-asked)
  ;; READ-STRING with :trim gives the answer without the spaces and tabs at either end; without, as typed
  (flet ((answer (&rest arguments)
           (let ((answer nil)
                 (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
             (tuatara.frontend.fake:fake-queue-action
              f (lambda () (setf answer (apply #'tuatara.command:read-string "Name: " arguments))))
             (tuatara.frontend.fake:fake-queue-keys f "SPC SPC k e a SPC SPC RET")
             (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
             answer)))
    (is string= "kea" (answer :trim t))
    (is string= "  kea  " (answer))))

(define-test (shared-helpers replacement-sources-kept-apart)
  ;; what a buffer shows in place of its text comes from sources kept apart by key: the display merges
  ;; them, and one taken away leaves the others, whichever came first
  (let ((buffer (tuatara.buffer:make-buffer "*replacements test*" :text "abcdefghij")))
    (unwind-protect
         (progn
           (tuatara.display:add-replacements buffer :mode (lambda (b s e) (declare (ignore b s e))
                                                            (list (list 4 5 "E" nil))))
           (tuatara.display:add-replacements buffer :hints (lambda (b s e) (declare (ignore b s e))
                                                             (list (list 1 1 ":hint" nil) (list 8 8 ":x" nil))))
           (is equal '(1 4 8) (mapcar #'first (tuatara.display:buffer-replacements buffer 0 10)))
           (true (tuatara.display:buffer-replacements-p buffer :hints))
           (tuatara.display:remove-replacements buffer :mode)
           (is equal '(1 8) (mapcar #'first (tuatara.display:buffer-replacements buffer 0 10)))
           (tuatara.display:remove-replacements buffer :hints)
           (false (tuatara.display:buffer-replacements-p buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (shared-helpers background-work-stopped-by-place)
  ;; work in the background kept as one record: its key, its value once there, stopped and forgotten at once
  (let ((work (tuatara.runtime:start-background-work :key (lambda (cancel) (declare (ignore cancel)) 42)
                                                     "tuatara test work" nil)))
    (is eq :key (tuatara.runtime:background-work-key work))
    (tuatara.runtime:wait-until (lambda () (tuatara.runtime:background-work-value work)) 10)
    (is eql 42 (tuatara.runtime:background-work-value work)))
  (let* ((stopped nil)
         (holder (list (tuatara.runtime:start-background-work
                        nil (lambda (cancel) (loop until (car cancel) do (sleep 0.01)) (setf stopped t) :late)
                        "tuatara test work" nil))))
    (tuatara.runtime:stop-background-work (first holder))
    (false (first holder))
    (tuatara.runtime:wait-until (lambda () stopped) 10)
    (true stopped))
  ;; the place's forms evaluated once
  (let ((holders (vector nil (tuatara.runtime:start-background-work nil (constantly 1) "tuatara test work" nil)))
        (asked 0))
    (tuatara.runtime:stop-background-work (aref holders (progn (incf asked) 1)))
    (is = 1 asked)
    (false (aref holders 1))))

(define-test (shared-helpers context-keys-take-every-letter)
  ;; a mode's keys of one character are under C-c too, x, s and t as much as any other: git status's s stages
  ;; and x reverses there as well
  (run-keys "")
  (flet ((key (text) (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'tuatara.git::git-status-mode)
                                                 (tuatara.frontend:parse-key-sequence text))))
    (is eq 'tuatara.git::git-stage (key "C-c s"))
    (is eq 'tuatara.git::git-reverse (key "C-c x"))
    (false (boundp 'tuatara.defaults::*context-keys-left-free*))))

(define-test (shared-helpers list-keys-bound-one-way)
  ;; h and C-b do what the list's kind says: fold a heading in a manager (the jobs among them), go back from
  ;; a list entered from another view, nothing of their own elsewhere; a list of entries to open has the same
  ;; keys for them
  (run-keys "")
  (flet ((key (mode text) (tuatara.command:lookup-key (tuatara.modes:mode-keymap mode)
                                                      (tuatara.frontend:parse-key-sequence text))))
    (dolist (text '("h" "C-b"))
      (is eq 'tuatara.defaults::list-fold-heading (key 'tuatara.defaults::bookmarks-mode text))
      (is eq 'tuatara.defaults::quit-window (key 'tuatara.git::git-log-mode text))
      (is eq 'tuatara.defaults::list-fold-heading (key 'tuatara.defaults::jobs-mode text))
      (false (key 'tuatara.git::git-blame-mode text)))
    (is eq 'tuatara.defaults::list-next-entry (key 'tuatara.defaults::bookmarks-mode "n"))
    (is eq 'tuatara.defaults::list-visit-other (key 'tuatara.defaults::bookmarks-mode "o"))
    (true (fboundp 'tuatara.defaults::declare-list-kind))))

(define-test (shared-helpers quit-questions-asked-one-way)
  ;; the questions before quitting: asked in order until one says stay; what they give to call, called once
  ;; going is certain; the editor's exit and the daemon's stop ask them through the same pair
  (let* ((called '())
         (tuatara.defaults::*quit-query-functions*
           (list (constantly t) (lambda () (lambda () (push :stopped called))))))
    (let ((answers (tuatara.defaults::ask-quit-queries "exit")))
      (is eql 2 (length answers))
      (false called)
      (tuatara.defaults::settle-quit answers)
      (is equal '(:stopped) called)))
  (let* ((asked 0)
         (tuatara.defaults::*quit-query-functions*
           (list (constantly nil) (lambda () (incf asked) t))))
    (is eq :stay (tuatara.defaults::ask-quit-queries "exit"))
    (is = 0 asked))
  (true (fboundp 'tuatara.defaults::install-mode-line-signs)))

(define-test (shared-helpers replace-previews-kept-apart)
  ;; two replacements previewed in one buffer (two clients asking): the one ended first leaves the other's
  (let ((buffer (tuatara.buffer:make-buffer "*previews test*" :text "abc")))
    (unwind-protect
         (let ((first (tuatara.defaults::show-replace-preview nil buffer))
               (second (tuatara.defaults::show-replace-preview nil buffer)))
           (funcall first)
           (true (tuatara.display:buffer-replacements-p buffer))
           (funcall second)
           (false (tuatara.display:buffer-replacements-p buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (shared-helpers errors-handled-and-reported)
  ;; HANDLING-ERRORS-REPORTED is HANDLER-CASE whose form reports its errors as REPORTING-ERRORS does
  (drain-debug-reports)
  (clrhash tuatara.runtime::*error-report-places*)
  (unwind-protect
       (progn
         (tuatara.runtime:set-debug t)
         (is = 3 (tuatara.runtime:handling-errors-reported ("adding in a test") (+ 1 2) (error () 0)))
         (is equal '(:handled "boom")
             (tuatara.runtime:handling-errors-reported ("failing in a test ~d" 7)
                 (error "boom")
               (error (condition) (list :handled (princ-to-string condition)))))
         (let ((reports (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                              while report collect (princ-to-string (fourth report)))))
           (true (some (lambda (text) (search "failing in a test 7" text)) reports) "~s" reports))
         (tuatara.runtime:set-debug nil)
         (is eq :other (tuatara.runtime:handling-errors-reported ("signalling in a test")
                           (signal 'tuatara.runtime:quit)
                         (tuatara.runtime:quit () :other))))
    (drain-debug-reports)))

(define-test (shared-helpers current-job-cancel-of-the-work-running)
  (false (tuatara.runtime:current-job-cancel))
  (let* ((job (tuatara.defaults::%make-hash-job :what "a sum"))
         (tuatara.runtime:*job* job))
    (is eq (tuatara.runtime:job-cancel job) (tuatara.runtime:current-job-cancel))))

(define-test (shared-helpers octet-counter-agrees-with-octet-length)
  ;; characters of one, two, three and four octets
  (let* ((text (format nil "a~cb~c~cc~cdd" (code-char #xE9) (code-char #x20AC) (code-char #x1F600) (code-char #x7FF)))
         (count (tuatara.text:make-octet-counter text 100)))
    (dotimes (i (1+ (length text)))
      (is = (+ 100 (tuatara.text:string-octet-length text :end i)) (funcall count i) "at ~d" i))
    (let ((again (tuatara.text:make-octet-counter text)))
      (is = 0 (funcall again 0))
      (is = (tuatara.text:string-octet-length text) (funcall again (length text))))))

(define-test (shared-helpers octets-read-as-text)
  (let ((utf-8 (tuatara.text:string-octets (format nil "caf~c" (code-char #xE9))))
        (latin-1 (coerce '(99 97 102 233) '(vector (unsigned-byte 8)))))
    (is string= (format nil "caf~c" (code-char #xE9)) (tuatara.text:octets-text utf-8 0 (length utf-8)))
    (is string= (format nil "caf~c" (code-char #xE9)) (tuatara.text:octets-text latin-1 0 4))
    (is string= "ab" (tuatara.text:octets-text (coerce '(97 98 0 99) '(vector (unsigned-byte 8))) 0 4))))

(define-test (shared-helpers colours-kept-latest)
  (let ((kept '()) (made 0))
    (flet ((ask (key) (tuatara.syntax:keep-lately key (progn (incf made) (list :made key)) kept :most 3)))
      (is equal '(:made (1)) (ask '(1)))
      (is equal '(:made (1)) (ask '(1)))
      (is = 1 made)
      (ask '(2))
      (ask '(3))
      (ask '(4))
      (is equal '((4) (3) (2)) (mapcar #'car kept))
      (is equal '(:made (1)) (ask '(1)))
      (is = 5 made)
      (is equal '((1) (4) (3)) (mapcar #'car kept)))))

(define-test (shared-helpers content-lines-folded-within-the-limit)
  ;; 200 octets, the characters at the edge of the first piece of three octets
  (let* ((line (concatenate 'string (make-string 74 :initial-element #\a)
                            (make-string 42 :initial-element (code-char #x20AC))))
         (folded (with-output-to-string (out) (tuatara.text:fold-content-line line out)))
         (crlf (coerce '(#\Return #\Newline) 'string))
         (pieces (loop with from = 0
                       for end = (search crlf folded :start2 from)
                       while end collect (subseq folded from end) do (setf from (+ end 2)))))
    (is = (+ 74 (* 3 42)) (tuatara.text:string-octet-length line))
    (true (every (lambda (piece) (<= (tuatara.text:string-octet-length piece) 75)) pieces) "~s" pieces)
    (true (every (lambda (piece) (char= #\Space (char piece 0))) (rest pieces)))
    (is string= line (apply #'concatenate 'string (first pieces)
                            (mapcar (lambda (piece) (subseq piece 1)) (rest pieces))))
    (is = 74 (length (first pieces)))))

(define-test (shared-helpers base64-in-lines-of-mail)
  (let* ((octets (make-array 100 :element-type '(unsigned-byte 8) :initial-element 255))
         (plain (tuatara.text:base64-string octets))
         (mail (tuatara.text:base64-string octets :line-length 76)))
    (is string= (format nil "~a~%~a~%" (subseq plain 0 76) (subseq plain 76)) mail)
    (is string= plain (remove #\Newline mail))))

(define-test (shared-helpers figure-checks-in-the-api)
  (dolist (name '("LUHN-P" "IBAN-CHECK-P" "IBAN-LENGTH" "FOLD-CONTENT-LINE" "CHAR-OCTET-LENGTH" "BASE64-STRING"
                  "*SHORT-WEEKDAY-NAMES*"))
    (is eq :external (nth-value 1 (find-symbol name "TUATARA")) "~a" name))
  (true (tuatara:luhn-p "4539578763621486"))
  (false (tuatara:luhn-p "4539578763621487"))
  ;; countries a shorter table of the registry once missed
  (dolist (iban '("RU0304452522540817810538091310419" "YE15CBYE0001018861234567891234"))
    (true (tuatara:iban-check-p iban) iban)
    (is eql (length iban) (tuatara:iban-length (subseq iban 0 2)) iban))
  (false (tuatara:iban-check-p "RU0304452522540817810538091310418"))
  (false (tuatara:iban-length "ZZ")))

(define-test (shared-helpers first-line-of-octets-without-carriage-return)
  ;; a Windows file's first line ends with CR LF: the mode rules see neither
  (flet ((first-line (text) (tuatara.text:octets-first-line (tuatara.text:string-octets text))))
    (is string= "a" (first-line (format nil "a~c~%b" #\Return)))
    (is string= "a" (first-line (format nil "a~%b")))
    (is string= "" (first-line (format nil "~c~%b" #\Return)))
    (is string= "ab" (first-line "ab"))
    (is string= "" (first-line ""))
    (is eq 'tuatara.defaults::bash-mode
        (tuatara.modes:mode-for-octets #p"/tmp/G0-script" (tuatara.text:string-octets
                                                             (format nil "#!/bin/sh~c~%echo~%" #\Return))))))

(define-test (shared-helpers content-line-escapes)
  ;; one way to write a text value and a parameter value of a vCard or iCalendar line
  (is string= (format nil "a\\\\b\\;c\\,d\\ne") (tuatara.text:content-text-escaped (format nil "a\\b;c,d~c~%e" #\Return)))
  (is string= "plain" (tuatara.text:content-text-escaped "plain"))
  (is string= "\"Smith, John\"" (tuatara.text:content-parameter-text "Smith, John"))
  (is string= "Smith" (tuatara.text:content-parameter-text "Smith"))
  (is string= "a^'b^^c^nd" (tuatara.text:content-parameter-text (format nil "a\"b^c~%d")))
  (is string= "\"x^'y;z\"" (tuatara.text:content-parameter-text "x\"y;z")))

(define-test (shared-helpers check-digits-of-bad-input)
  ;; the exported checks answer no, never an error, for what cannot be a number
  (true (tuatara.syntax:luhn-p "4111111111111111"))
  (false (tuatara.syntax:luhn-p ""))
  (false (tuatara.syntax:luhn-p "4111 1111"))
  (false (tuatara.syntax:luhn-p "41a1"))
  (true (tuatara.syntax:iban-check-p "GB82WEST12345698765432"))
  (false (tuatara.syntax:iban-check-p ""))
  (false (tuatara.syntax:iban-check-p "GB82 WEST"))
  (false (tuatara.syntax:iban-check-p "GB8"))
  (false (tuatara.syntax:iban-check-p "GB82-WEST12345698765432")))

(defun ask-which-key (prompt)
  "Ask PROMPT as a question of four keys, j, m, M and n, each with its words."
  (tuatara.command:read-key-choice prompt '(("j" . "one") ("m" . "two") ("M" . "three") ("n" . "none"))))

(define-test (shared-helpers one-key-choice-aliases-and-case)
  ;; a key may stand for another (SPC for y), and with fold-case a capital answers as its small letter
  (let ((ask (key-string (concatenate 'string "(setf tuatara.tests::*yes-or-no-answer* "
                                      "(tuatara.command:read-key-choice "
                                      "\"Go\" '((\"y\" . \"yes\") (\"n\" . \"no\")) :aliases '((\"SPC\" . \"y\"))"
                                      " :fold-case t))"))))
    (dolist (case '(("SPC" "y") ("Y" "y") ("x n" "n")))
      (setf *yes-or-no-answer* :none)
      (run-keys (list (format nil "M-: ~a RET" ask) (first case)) :width 200 :height 20)
      (is equal (second case) *yes-or-no-answer* "~s" (first case)))))

(define-test (shared-helpers one-key-choice)
  ;; a key and its words on the prompt, the rows beside it, the case of the key kept, C-g leaves
  (let ((ask (key-string "(setf tuatara.tests::*yes-or-no-answer* (tuatara.tests::ask-which-key \"Pick\"))"))
        during)
    (dolist (case '(("M" "M") ("m" "m") ("x M" "M")))
      (setf *yes-or-no-answer* :none)
      (run-keys (list (format nil "M-: ~a RET" ask)
                      (lambda () (setf during (question-details-seen)))
                      (first case))
                :width 120 :height 20)
      (is equal (second case) *yes-or-no-answer* "~s" (first case))
      (true (search (format nil "M  three~%n  none") (first during)) "~s" during))
    (setf *yes-or-no-answer* :none)
    (run-keys (list (format nil "M-: ~a RET" ask) "C-g") :width 120 :height 20)
    (is eq :none *yes-or-no-answer*)))
