;;;; test/org-agenda-reading.lisp --- What the agenda, clock, blocks and tables read, and on which thread.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun write-agenda-files (directory count &optional (text (format nil "* TODO Task :work:~%")))
  "Write COUNT agenda files holding TEXT in DIRECTORY; their names as strings."
  (loop for i below count
        for path = (namestring (merge-pathnames (format nil "a~d.org" i) directory))
        do (alexandria:write-string-into-file text path)
        collect path))

(defun wait-for-thread-named (name)
  "Wait, at most ten seconds, until no thread called NAME runs."
  (tuatara.runtime:wait-until (lambda () (not (find name (sb-thread:list-all-threads)
                                                    :key #'sb-thread:thread-name :test #'equal)))
                              10))

(define-test (org agenda-file-cache-forgotten-while-filled)
  ;; files are let go of while other threads keep adding to the table: no entry skipped, nothing signalled
  (let* ((table (make-hash-table :test 'equal :synchronized t))
         (tuatara.defaults::*org-agenda-file-cache* table)
         (failed nil))
    (let ((adder (sb-thread:make-thread
                  (lambda ()
                    (dotimes (i 200000)
                      (setf (gethash (format nil "/tmp/f~d" i) table) t))))))
      (handler-case (loop while (sb-thread:thread-alive-p adder)
                          do (tuatara.defaults::org-agenda-forget-files '()))
        (error (e) (setf failed e)))
      (sb-thread:join-thread adder)
      (tuatara.defaults::org-agenda-forget-files '())
      (false failed "~a" failed)
      (is = 0 (hash-table-count tuatara.defaults::*org-agenda-file-cache*)))))

(define-test (org agenda-shown-looks-at-disk-once-a-second)
  ;; an agenda shown asks for its key after every key: its files on disk are looked at once a second, not each time
  (with-test-directory (dir)
    (let* ((tuatara-user:*org-agenda-files* (write-agenda-files dir 20))
           (tuatara.defaults::*org-agenda-shown-sources* nil)
           (key (second (assoc 'tuatara.defaults::org-agenda-mode tuatara.defaults::*shown-lists*))))
      (funcall key nil)
      (multiple-value-bind (again stats) (counting-calls (tuatara.io:path-stamp) (funcall key nil))
        (is = 0 stats)
        (is equal (tuatara.defaults::org-agenda-sources-key) again)))))

(define-test (org tags-offered-without-reading-large-agenda-files)
  ;; agenda files too large to read at once are read in the background: the question opens at once
  (with-test-directory (dir)
    (let ((tuatara-user:*org-agenda-files* (write-agenda-files dir 3))
          (tuatara.defaults::*org-agenda-at-once* 0))
      ;; looked at for their sizes, never read here
      (is = 0 (nth-value 1 (counting-calls (tuatara.defaults::org-agenda-source-text)
                             (tuatara.defaults::org-known-tags))))
      (wait-for-thread-named "tuatara agenda files read")
      (is equal '("work") (tuatara.defaults::org-known-tags)))))

(define-test (org clock-left-open-looked-for-in-the-background)
  ;; with no clock running and large agenda files, C-x a o looks in the background and says so; the clock
  ;; found is taken up once it is back
  (with-test-directory (dir)
    (let ((tuatara-user:*org-agenda-files*
            (write-agenda-files dir 1 (format nil "* Minutes~%:LOGBOOK:~%CLOCK: [2026-10-26 Mon 14:40]~%:END:~%")))
          (tuatara.defaults::*org-agenda-at-once* 0)
          (tuatara.defaults::*org-clocks-none-open* nil)
          (tuatara.defaults::*org-now* (encode-universal-time 0 55 14 26 10 2026)))
      (unwind-protect
           (progn
             (setf tuatara.defaults::*org-clock* nil)
             (run-keys (list "C-x a o"
                             (lambda ()
                               (true (search "Looking for a clock left running" (messages-text)))
                               (false tuatara.defaults::*org-clock*)
                               (wait-for-thread-named "tuatara clocks left open"))
                             :tick)
                       :width 80 :height 12)
             (true tuatara.defaults::*org-clock*))
        (setf tuatara.defaults::*org-clock* nil)))))

(define-test (org clock-look-ended-with-its-session-looked-for-again)
  ;; clocks found by a look whose session ended before they were taken up do not keep the next session
  ;; from looking
  (with-test-directory (dir)
    (let ((tuatara-user:*org-agenda-files*
            (write-agenda-files dir 1 (format nil "* Minutes~%:LOGBOOK:~%CLOCK: [2026-10-26 Mon 14:40]~%:END:~%")))
          (tuatara.defaults::*org-agenda-at-once* 0)
          (tuatara.defaults::*org-clocks-none-open* nil)
          (tuatara.defaults::*org-now* (encode-universal-time 0 55 14 26 10 2026)))
      (unwind-protect
           (progn
             (setf tuatara.defaults::*org-clock* nil)
             (tuatara.defaults::org-clocks-look-in-background)
             (wait-for-thread-named "tuatara clocks left open")
             (false tuatara.defaults::*org-clock*)
             (run-keys (list (lambda ()
                               (tuatara.defaults::org-clocks-look-in-background)
                               (wait-for-thread-named "tuatara clocks left open"))
                             :tick)
                       :width 80 :height 12)
             (true tuatara.defaults::*org-clock*))
        (setf tuatara.defaults::*org-clock* nil)))))

(define-test (org agenda-log-days-without-garbage)
  ;; the closing and clock lines of an entry are told to be of a day without a string made for each
  (let* ((text (with-output-to-string (s)
                 (format s "* DONE Task~%:LOGBOOK:~%")
                 (dotimes (i 2000) (format s "CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00~%"))
                 (format s ":END:~%")))
         (entries (tuatara.defaults::org-agenda-file-entries "/tmp/log.org" text))
         (day (tuatara.dates:day-number 2026 10 2)))
    (true (< (least-bytes (lambda () (tuatara.defaults::org-agenda-progress entries day)) 3) 8000))))

(define-test (org formatted-sort-keys-worked-out-once)
  ;; files sorted by the time they changed are looked at once each, not at each comparison
  (let ((files (loop for i below 64 collect (cons (format nil "/nonexistent/n~d.org" i) (format nil "n~d.org" i)))))
    (is = 64 (nth-value 1 (counting-calls (tuatara.io:path-stamp)
                            (tuatara.defaults::formatted-block-sorted files "last-modified" nil))))))

(define-test (org formatted-links-label-markdown-titles)
  ;; a Markdown note's title is read from its own front matter, not as Org's
  (with-test-directory (dir)
    (let* ((name "20260101T090000--kea-count.md")
           (path (namestring (merge-pathnames name dir))))
      (alexandria:write-string-into-file (format nil "---~%title: The Kea Count~%---~%~%Words.~%") path)
      (true (search "The Kea Count" (tuatara.defaults::formatted-link-lines (list (cons path name)) nil nil))))))

(define-test (org formatted-pieces-stop-at-the-limit)
  ;; past the most a block holds, no more files are read
  (let ((tuatara.defaults::*formatted-block-most-text* 10)
        (read 0))
    (tuatara.defaults::formatted-pieces '(a b c d e) (lambda (file) (declare (ignore file)) (incf read) "123456"))
    (is = 2 read)))

(define-test (org formatted-tags-aligned-in-one-pass)
  ;; the tags of many headlines are put in their column writing the text once
  (let ((text (with-output-to-string (s) (dotimes (i 2000) (format s "* Headline ~d          :tag:~%" i)))))
    (let ((before (sb-ext:get-bytes-consed)))
      (tuatara.defaults::formatted-tags-aligned text)
      (true (< (- (sb-ext:get-bytes-consed) before) (* 64 1024 1024))))))

(define-test (org table-key-reads-only-its-part)
  ;; TAB in a table of a large note, its parse behind, reads the part holding the table, not the whole file
  (let* ((filler (with-output-to-string (s) (dotimes (i 400) (format s "* Heading ~d~%Some words of text here.~%" i))))
         (text (format nil "~a* Table~%| a | bb |~%| ccc | d |~%" filler)))
    (let ((whole 0))
      (flet ((counted (function &rest arguments) (incf whole) (apply function arguments)))
        (let ((result (session-text
                       (nth-value 1 (run-keys (list "M-x o r g - m o d e RET M-< x M-> C-p"
                                                    (lambda () (sb-int:encapsulate 'tuatara.defaults::org-parse-now
                                                                                   'counted #'counted))
                                                    "TAB"
                                                    (lambda () (sb-int:unencapsulate 'tuatara.defaults::org-parse-now
                                                                                     'counted)))
                                              :text text :width 80 :height 10)))))
          (sb-int:unencapsulate 'tuatara.defaults::org-parse-now 'counted)
          (true (search (format nil "| a   | bb |~%| ccc | d  |~%") result) "~a" (subseq result (- (length result) 60)))
          (is = 0 whole))))))

(define-test (org killed-checkboxes-counted-once-per-list)
  ;; many checkbox items killed under one list: the list's cookie is counted again once
  (let ((text (with-output-to-string (s)
                (format s "* Tasks [0/40]~%")
                (dotimes (i 40) (format s "- [ ] item ~d~%" i)))))
    (multiple-value-bind (result counts)
        (counting-calls (tuatara.defaults::org-statistics-edits)
          (session-text (nth-value 1 (run-keys "M-x o r g - m o d e RET M-< C-n C-SPC M-> C-w"
                                               :text text :width 80 :height 10))))
      (true (search "[0/0]" result) "~a" result)
      (true (<= counts 2) "~d counts" counts))))

(define-test (org parse-short-of-memory-shown-plain)
  ;; a parse that runs out of memory gives the text shown plain, as a failed one does, never the condition
  (multiple-value-bind (parse failed)
      (progn
        (sb-int:encapsulate 'tuatara.defaults::org-parse 'short
                            (lambda (parse &rest arguments)
                              (declare (ignore parse arguments))
                              (error (make-condition 'storage-condition))))
        (unwind-protect
             (tuatara.defaults::org-worker-parse
              (list 1 "* A" nil nil nil))
          (sb-int:unencapsulate 'tuatara.defaults::org-parse 'short)))
    (true parse)
    (true failed)))

(define-test (org tangle-compares-sizes-first)
  ;; a code file of another size is changed, and is not read to say so
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "out.sh" dir))))
      (alexandria:write-string-into-file "echo old" path)
      (multiple-value-bind (file reads)
          (counting-calls (tuatara.io:file-octets)
            (tuatara.defaults::check-code-file (tuatara.defaults::make-code-file :path path :text "echo longer")))
        (is eq :changed (tuatara.defaults::code-file-state file))
        (is = 0 reads)))))

(define-test (org tangle-looks-in-the-background)
  ;; the code files are worked out and compared away from the editor's thread
  (with-test-directory (dir)
    (let* ((note (namestring (merge-pathnames "n.org" dir)))
           (main sb-thread:*current-thread*)
           (here 0))
      (alexandria:write-string-into-file (format nil "#+begin_src sh :tangle out.sh~%echo~%#+end_src~%") note)
      (sb-int:encapsulate 'tuatara.defaults::org-code-files 'where
                          (lambda (function &rest arguments)
                            (when (eq sb-thread:*current-thread* main) (incf here))
                            (apply function arguments)))
      (unwind-protect (run-keys "C-c C-v t y e s RET" :file note :width 80 :height 12)
        (sb-int:unencapsulate 'tuatara.defaults::org-code-files 'where))
      (is = 0 here)
      (true (probe-file (merge-pathnames "out.sh" dir))))))

(define-test (org link-opens-safely-from-a-safe-file)
  ;; a file link followed in an Org file opened safely opens its target safely too
  (with-test-directory (dir)
    (let ((a (namestring (merge-pathnames "a.org" dir)))
          (b (namestring (merge-pathnames "b.org" dir))))
      (alexandria:write-string-into-file (format nil "[[file:b.org][next]]~%") a)
      (alexandria:write-string-into-file (format nil "* B~%") b)
      (run-keys (list (lambda ()
                        (let* ((buffer (tuatara.defaults::visit-path a :safe t))
                               (parse (tuatara.defaults::org-parse-now buffer)))
                          (tuatara.defaults::org-follow-link
                           parse (tuatara.defaults::org-link-at (tuatara.defaults::org-parse-tree parse) 3)))))
                :width 80 :height 12)
      (let ((target (tuatara.buffer:find-buffer "b.org")))
        (true target)
        (true (and target (tuatara.io:safe-visit-p target)))))))

(define-test (org formula-ranges-and-ends-read-rightly)
  ;; a range may begin and end a number of rows or rules from the field; @<2 is no row formulas know
  (is eq :range (tuatara.defaults::org-formula-kind (tuatara.defaults::org-read-formula "@-1$2..@+1$3=1")))
  (is eq :range (tuatara.defaults::org-formula-kind (tuatara.defaults::org-read-formula "@-I$2..@+I$3=1")))
  (is eq :refused (odd-input-formula-outcome (list "$3=@<2$1") (list (list "1" "2" "") (list "3" "4" "")))))

(define-test (org blank-line-with-a-return)
  ;; a line of blanks ending in CR LF among lines ending in LF is a blank line, as the blanks skipped are
  (let ((tuatara.defaults::*org-text* (format nil " ~c~%Words~%" #\Return)))
    (true (tuatara.defaults::org-blank-line-p 0))
    (false (tuatara.defaults::org-blank-line-p 3))))

(define-test (org refile-leaves-out-changed-large-files)
  ;; a large agenda file changed since it was read offers none of its old headings
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "inbox.org" dir)))
           (tuatara-user:*org-agenda-files* (list path)))
      (alexandria:write-string-into-file (format nil "* One~%* Two~%") path)
      (let ((buffer (tuatara.io:visit-file path)))
        (tuatara.defaults::org-agenda-file-read path)
        (tuatara.buffer:buffer-insert buffer 0 (format nil "* Zero~%"))
        (let ((tuatara.defaults::*org-agenda-at-once* 0))
          (is eq nil (first (tuatara.defaults::org-files-headings (list path)))))
        (wait-for-thread-named "tuatara agenda files read")
        (tuatara.buffer:buffer-mark-saved buffer)))))
