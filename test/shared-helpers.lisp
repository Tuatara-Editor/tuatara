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

(define-test (shared-helpers list-keys-bound-one-way)
  ;; every list goes back with h and C-b, and a list of entries to open has the same keys for them
  (run-keys "")
  (flet ((key (mode text) (tuatara.command:lookup-key (tuatara.modes:mode-keymap mode)
                                                      (tuatara.frontend:parse-key-sequence text))))
    (dolist (mode '(tuatara.defaults::jobs-mode tuatara.defaults::bookmarks-mode tuatara.git::git-blame-mode))
      (is eq 'tuatara.defaults::quit-window (key mode "h") "~a" mode)
      (is eq 'tuatara.defaults::quit-window (key mode "C-b") "~a" mode))
    (is eq 'tuatara.defaults::list-next-entry (key 'tuatara.defaults::bookmarks-mode "n"))
    (is eq 'tuatara.defaults::list-visit-other (key 'tuatara.defaults::bookmarks-mode "o"))
    (true (fboundp 'tuatara.defaults::list-back-keys))))

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
