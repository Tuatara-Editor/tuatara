;;;; test/core-edges.lisp --- The core's edge cases: files, programs, text, threads and buffers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test core-edges
  :description "Edge cases of the core below the editor's commands: a file that cannot be opened,
a program that will not end, data damaged or cut short, text that is not what it claims, and the
threads that do the work in the background.")

(defun octets-given (&rest octets)
  "OCTETS as a simple vector of octets."
  (coerce octets 'tuatara.text:octets))

(defun within-seconds (seconds function)
  "Call FUNCTION on a thread of its own: two values, what it returned and true,
or NIL and NIL when it has not returned within SECONDS (the thread is then
ended)."
  (let ((thread (sb-thread:make-thread function :name "core-edges bounded call")))
    (multiple-value-bind (result ended) (tuatara.runtime:thread-result thread seconds)
      (unless ended (sb-thread:terminate-thread thread))
      (values result ended))))

(define-test (core-edges appending-through-a-link-refused)
  ;; a link where the file should be is refused as a file error, not a type error
  (with-test-directory (dir)
    (let ((target (namestring (merge-pathnames "target" dir)))
          (link (namestring (merge-pathnames "journal" dir))))
      (put-file target "")
      (sb-posix:symlink target link)
      (fail (tuatara.io:append-to-file link "line") tuatara.io:file-system-error)
      (is string= "" (file-text target)))))

(define-test (core-edges program-output-ends-a-stubborn-program)
  ;; a program that will not end when asked, timed out, is killed with what it started
  (let ((mark (format nil "32.~d" (+ 100000 (random 900000)))))
    (multiple-value-bind (output ok)
        (tuatara.io:program-output (list (tuatara.io:find-program "sh") "-c"
                                         (format nil "trap '' TERM; sleep ~a & wait" mark))
                                   :seconds 1/2)
      (false output)
      (false ok))
    (true (gone-within mark 6))))

(defun core-edges-environment-host ()
  "A host whose programs are given one variable, CORE_EDGES=yes, and nothing else."
  (make-instance 'core-edges-host))

(defclass core-edges-host (tuatara.runtime:host) ()
  (:documentation "A host giving its programs the one variable CORE_EDGES=yes."))

(defmethod tuatara.io:host-program-environment ((host core-edges-host))
  "CORE_EDGES=yes alone."
  (list "CORE_EDGES=yes"))

(define-test (core-edges program-tied-to-editor-takes-callers-environment)
  ;; the variables of whom a program is for are taken where it is asked for, not on the starter thread
  (let* ((tuatara.runtime:*host* (core-edges-environment-host))
         (process (tuatara.io:start-program (list (tuatara.io:find-program "env")) :output :stream :with-editor t)))
    (unwind-protect
         (let ((text (tuatara.text:octets-string (tuatara.io::read-stream-octets (sb-ext:process-output process)))))
           (true (search "CORE_EDGES=yes" text) "~s" text))
      (tuatara.io:done-with-program process))))

(define-test (core-edges damaged-packed-data-refused)
  ;; a stored block whose length and its check differ is damaged
  (fail (tuatara.io:inflate (octets-given 1 3 0 0 0 97 98 99)) tuatara.io:zip-error)
  (is equalp (octets-given 97 98 99) (tuatara.io:inflate (octets-given 1 3 0 #xFC #xFF 97 98 99)))
  ;; a size of 0 said beforehand still grows to what the data holds, within the limit
  (is equalp (octets-given 97 98 99) (tuatara.io:inflate (octets-given 1 3 0 #xFC #xFF 97 98 99) :size 0 :limit 100))
  (fail (tuatara.io:inflate (octets-given 1 3 0 #xFC #xFF 97 98 99) :size 0 :limit 2) tuatara.io:zip-error))

(define-test (core-edges old-bmp-header-read)
  ;; a BMP with the 12-octet header holds its width and height in two octets each
  (let ((o (make-array 26 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace o (octets-given 66 77))
    (setf (aref o 14) 12 (aref o 18) #x2C (aref o 19) 1 (aref o 20) #xC8 (aref o 22) 1 (aref o 24) 24)
    (is equal '(300 200) (multiple-value-list (tuatara.io::bmp-size o)))))

(define-test (core-edges unfinished-control-sequence-ends-at-control)
  (let ((e (string #\Escape)))
    (flet ((end (text) (multiple-value-list (tuatara.text:escape-sequence-end text 0))))
      ;; the line end after an unfinished sequence is not taken into it, nor anything after
      (is equal '(3 nil 2 3) (end (format nil "~a[1~%bold" e)))
      (is equal '(2 nil 2 2) (end (format nil "~a[" e)))
      (is equal '(4 #\m 2 3) (end (format nil "~a[1mx" e))))))

(define-test (core-edges strings-and-counts)
  ;; a string is not an array of JSON
  (false (tuatara.text:json-get "abc" 1))
  (is = 2 (tuatara.text:json-get (vector 1 2) 1))
  ;; an empty needle stands nowhere, and the count ends
  (multiple-value-bind (count ended) (within-seconds 2 (lambda () (tuatara.text:search-count "" "abc")))
    (true ended)
    (is eql 0 count))
  (is = 2 (tuatara.text:search-count "ab" "abcab")))

(define-test (core-edges xml-read-exactly)
  ;; a character's number is digits alone, never 0
  (is string= "A" (tuatara.text:xml-entity "#65"))
  (is string= "A" (tuatara.text:xml-entity "#x41"))
  (dolist (name '("#+65" "# 65" "#x 41" "#0" "#x0" "#" "#x"))
    (false (tuatara.text:xml-entity name) "~s" name))
  ;; a tag's name ends at any blank, a return or a form feed too
  (let ((xml (format nil "<w:body><w:p~cw:x=\"1\">one</w:p><w:p~a>two</w:p></w:body>" #\Return #\Page)))
    (is = 2 (length (tuatara.text:xml-elements xml "w:p")))
    (is equal '("w:p" "w:p") (mapcar #'first (tuatara.text:xml-top-elements xml 8 (- (length xml) 9))))))

(define-test (core-edges read-only-splice-refused-before-counting)
  ;; a splice of a read-only buffer is refused before its lines are counted
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "big.txt" dir))))
      (put-file path (format nil "one~%two~%three~%"))
      (let* ((tuatara.runtime:*host* (make-instance 'tuatara.runtime:host))
             (b (tuatara.buffer:make-buffer "core-edges splice" :text (tuatara.text:open-file-rope path))))
        (unwind-protect
             (progn
               (false (tuatara.buffer:buffer-indexed-p b))
               (setf (tuatara.buffer:buffer-read-only-p b) t)
               (fail (tuatara.buffer:buffer-splice b 0 3 nil))
               (fail (tuatara.buffer:buffer-splice-spans b '((0 . 3)) nil))
               (false (tuatara.buffer:buffer-indexed-p b) "the lines not counted for a refusal"))
          (tuatara.buffer:kill-buffer b))))))

(define-test (core-edges refused-undo-keeps-held-count)
  ;; an undo or redo of the only group refused leaves what the history holds as it was
  (let ((b (tuatara.buffer:make-buffer "core-edges held" :text "abc")))
    (unwind-protect
         (flet ((refusing (buffer edits)
                  (declare (ignore buffer edits))
                  (error "Not changed: refused")))
           (tuatara.buffer:buffer-insert b 0 "x")
           (let ((held (tuatara.buffer::buffer-undo-held b)))
             (true (plusp held))
             (let ((tuatara.buffer:*before-edit-functions* (list #'refusing)))
               (fail (tuatara.buffer:buffer-undo b)))
             (is = held (tuatara.buffer::buffer-undo-held b))
             (tuatara.buffer:buffer-undo b)
             (let ((redo-held (tuatara.buffer::buffer-redo-held b)))
               (true (plusp redo-held))
               (let ((tuatara.buffer:*before-edit-functions* (list #'refusing)))
                 (fail (tuatara.buffer:buffer-redo b)))
               (is = redo-held (tuatara.buffer::buffer-redo-held b)))))
      (tuatara.buffer:kill-buffer b))))

(define-test (core-edges revert-asks-before-edit-once)
  ;; reverting asks those who may refuse a change once, before the file is read
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "asked.txt" dir))) (asked 0))
      (put-file path "one")
      (with-visited (b path)
        (let ((tuatara.buffer:*before-edit-functions*
                (list (lambda (buffer edits) (declare (ignore edits)) (when (eq buffer b) (incf asked))))))
          (tuatara.io:revert-buffer-file b))
        (is = 1 asked)))))

(define-test (core-edges program-path-read-as-written)
  ;; a program standing in a directory whose name holds [ or * is found where it stands
  (let ((kept sb-ext:*runtime-pathname*))
    (unwind-protect
         (let ((beside (progn (setf sb-ext:*runtime-pathname* "/tmp/a[b]*/bin/tuatara")
                              (first (tuatara.runtime:resource-directories "share/" "data/")))))
           (false (wild-pathname-p beside))
           (is string= "/tmp/a[b]*/bin/share/" (sb-ext:native-namestring beside)))
      (setf sb-ext:*runtime-pathname* kept))))

(define-test (core-edges long-lived-threads-hold-no-secret)
  ;; a thread kept for all work does not keep the secret of whoever made it
  (let ((tuatara.runtime:*secret-at-hand* t))
    (is eq nil (sb-thread:join-thread
                (tuatara.runtime:make-guarded-thread (lambda () tuatara.runtime:*secret-at-hand*) :secret nil)))
    (is eq t (sb-thread:join-thread
              (tuatara.runtime:make-guarded-thread (lambda () tuatara.runtime:*secret-at-hand*))))))

(define-test (core-edges thread-cut-short-has-ended)
  ;; a thread that ended without returning has ended, not still running
  (let ((thread (tuatara.runtime:make-guarded-thread (lambda () (sb-thread:abort-thread)))))
    (is equal '(nil t) (multiple-value-list (tuatara.runtime:thread-result thread 2))))
  (is equal '(7 t) (multiple-value-list
                    (tuatara.runtime:thread-result (tuatara.runtime:make-guarded-thread (lambda () 7)) 2))))

(define-test (core-edges zone-names-that-are-none-not-kept)
  ;; a name no zone has, as a hostile file may give any number of, is not kept
  (let ((name (format nil "Nowhere/Core_Edges_~d" (random 1000000))))
    (false (tuatara.dates:time-zone-named name))
    (false (nth-value 1 (gethash name tuatara.dates::*zones*)))
    ;; remembered as none among a few, so asked again it is not looked up again
    (true (gethash name tuatara.dates::*zone-misses*))
    (loop repeat 100 do (tuatara.dates:time-zone-named (format nil "Nowhere/Core_Edges_~d" (random 1000000))))
    (true (<= (hash-table-count tuatara.dates::*zone-misses*) tuatara.dates::*zone-misses-kept*)))
  (true (tuatara.dates:time-zone-named "UTC"))
  (true (nth-value 1 (gethash "UTC" tuatara.dates::*zones*))))

(define-test (core-edges sandbox-shows-places-where-they-really-are)
  ;; a link under /tmp leading to the directory of your programs' sockets is refused, as that directory is
  (let ((sockets (first (tuatara.io::session-socket-directories))))
    (when (and sockets (probe-file sockets))
      (with-file-in-tmp (link "-sockets")
        (sb-posix:symlink sockets link)
        (fail (tuatara.io::mount-arguments (list link) '() '()) tuatara.io:file-system-error)
        (fail (tuatara.io::mount-arguments (list (cons link "/data")) '() '()) tuatara.io:file-system-error))))
  ;; your home directory, and a directory holding it, are never shown whole; a directory in it may be
  (let ((home (string-right-trim "/" (sb-ext:native-namestring (user-homedir-pathname)))))
    (false (tuatara.io:shown-place-p home))
    (false (tuatara.io:shown-place-p (tuatara.io:real-place home)))
    (false (tuatara.io:shown-place-p (tuatara.io:parent-path home)))
    (fail (tuatara.io::mount-arguments (list home) '() '()) tuatara.io:file-system-error)
    (true (tuatara.io:shown-place-p (concatenate 'string home "/notes")))
    ;; nor is the root, and an export shows neither of them whole to LaTeX
    (false (tuatara.io:shown-place-p "/"))
    (false (tuatara.io:shown-place-p "//"))
    (false (tuatara.io:shown-place-p "/."))
    (with-file-in-tmp (link "-root")
      (sb-posix:symlink "/" link)
      (false (tuatara.io:shown-place-p link))
      (fail (tuatara.io::check-shown-place link) tuatara.io:file-system-error))
    (is equal '() (tuatara.io::mount-arguments (list "/") '() '()))))

(define-test (core-edges query-regexes-read-as-searches)
  ;; a rule's #match? that repeats a repeated part is refused, as a search from a file is
  (false (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "(a+)+b") "aab"))
  (false (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "(a|a)*b") "aab"))
  (false (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "(a") "(a"))
  (true (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "^[A-Z]") "Foo"))
  (false (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "^[A-Z]") "foo")))

(define-test (core-edges mapped-file-kept-from-programs)
  ;; the descriptor a mapping keeps open is not given to programs started
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "mapped.txt" dir))))
      (put-file path "some text")
      (let ((mapping (tuatara.text:map-file path)))
        (unwind-protect
             (true (logtest 1 (sb-posix:fcntl (tuatara.text:mapping-fd mapping) sb-posix:f-getfd)))
          (tuatara.text:close-mapping mapping))))))

(defun octets-per-call (function &optional (calls 200))
  "The octets FUNCTION, of no arguments, makes for each call, the least of
three runs of CALLS calls."
  (funcall function)
  (loop repeat 3
        minimize (let ((before (sb-ext:get-bytes-consed)))
                   (loop repeat calls do (funcall function))
                   (floor (- (sb-ext:get-bytes-consed) before) calls))))

(define-test (core-edges file-kind-told-without-garbage)
  ;; a plain text, which goes past every kind, makes no list or string for each test of a kind
  (let ((octets (tuatara.text:string-octets
                 (with-output-to-string (out) (dotimes (i 40) (format out "line ~d of a plain text~%" i))))))
    (let ((made (octets-per-call (lambda () (tuatara.io:octets-kind octets)))))
      (true (< made 6000) "~:d octets for each file" made))))

(define-test (core-edges org-colours-keep-the-code-colours-uncopied)
  ;; the colours kept for a note's source blocks are given as they are, not copied, for each paint
  (let* ((text (format nil "#+begin_src lisp~%(f x)~%#+end_src~%"))
         (buffer (org-parts-buffer text nil))
         (kept (loop for i below 4000 collect (list i (1+ i) :org-bold))))
    (unwind-protect
         (let* ((view (org-parts-whole-view buffer))
                (size (tuatara.buffer:buffer-size buffer)))
           (with-functions-replaced ((tuatara.defaults::org-src-cached-highlights
                                      (lambda (&rest arguments) (declare (ignore arguments)) kept)))
             (let ((colours (tuatara.defaults::org-view-colours buffer view 0 size nil)))
               (true (member kept colours :test #'eq) "the kept list is one of the lists given"))
             (let ((made (octets-per-call
                          (lambda () (tuatara.defaults::org-view-colours buffer view 0 size nil)))))
               (true (< made 40000) "~:d octets for each paint" made))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (core-edges note-link-patterns-are-compiled-once)
  ;; looking for the links of a note does not compile its pattern again for each file
  (let ((text (format nil "see [[denote:20260106T090000][a note]] and [b](denote:20260107T090000)~%")))
    (true (functionp tuatara.defaults::*formatted-org-link-re*))
    (true (functionp tuatara.defaults::*formatted-markdown-link-re*))
    (dolist (kind '(:org :markdown-yaml))
      (let ((made (octets-per-call (lambda () (tuatara.defaults::formatted-linked-identifiers text kind)))))
        (true (< made 6000) "~:d octets for each file of kind ~a" made kind)))))

(define-test (core-edges watcher-finds-a-file-by-table)
  ;; an event about one file of thousands is found without going through them all
  (let* ((items (loop for i below 20000
                      collect (tuatara.io::make-watch-item nil (format nil "/data/d~d/f~d" (mod i 100) i) nil
                                                           (list (cons (format nil "/data/d~d" (mod i 100))
                                                                       (tuatara.text:string-octets
                                                                        (format nil "f~d" i)))))))
         (places (tuatara.io::items-by-place items))
         (wds (make-hash-table))
         (event (list 7 2 (tuatara.text:string-octets "f4242"))))
    (setf (gethash 7 wds) (list "/data/d42"))
    (tuatara.io::note-watch-event items places event wds (make-hash-table :test 'equal) 1000)
    (is equal '("/data/d42/f4242") (loop for item in items
                                         when (tuatara.io::watch-item-due item)
                                           collect (tuatara.io::watch-item-path item)))
    (let ((start (get-internal-real-time)))
      (loop repeat 1000 do (tuatara.io::note-watch-event items places event wds (make-hash-table :test 'equal) 1000))
      (true (< (tuatara.runtime:seconds-since start) (* 1/10 (machine-slowdown)))
            "1000 events in ~,3f s" (tuatara.runtime:seconds-since start)))))

(define-test (core-edges edit-told-to-tree-waits-briefly)
  ;; the editor tells a tree of each edit as it types: a helper that does not answer is ended soon
  (let ((tuatara.syntax:*syntax-helper-wait* 2)
        (began (get-internal-real-time)))
    (call-with-fake-helper (lambda (fd how body)
                             (declare (ignore how body))
                             (let ((all (words 4 1 0 1)))
                               (dotimes (i (length all))
                                 (unless (fake-send-octet fd (aref all i)) (return))
                                 (sleep 0.3))))
                           (lambda (helper)
                             (false (nth-value 1 (tuatara.syntax:edit-tree (tuatara.syntax::%make-tree helper 1)
                                                                           (list (list 0 0 1 '(0 . 0) '(0 . 0)
                                                                                       '(0 . 1))))))
                             ;; late once, as under load: the helper goes on
                             (true (tuatara.syntax:helper-alive helper))))
    (true (< (tuatara.runtime:seconds-since began) 1) "~,2f s" (tuatara.runtime:seconds-since began))))

(define-test (core-edges splice-reads-only-its-seams)
  ;; neighbours of one text stay as they were: a splice of a file of many pieces, its lines ended by a
  ;; carriage return and a newline, looks only where the new text meets the old, not at every piece
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "crlf.txt" dir)))
          (octets (tuatara.text:string-octets
                   (with-output-to-string (out) (dotimes (i 200000) (format out "line ~d~c~%" i #\Return)))))
          (looks 0))
      (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (unwind-protect
             (let ((indexed (tuatara.text:index-rope rope :threads 1)))
               (true (> (getf (tuatara.text:rope-statistics indexed) :spans) 30))
               (sb-int:encapsulate 'tuatara.text::clean-junction-p 'counted
                                   (lambda (original left right) (incf looks) (funcall original left right)))
               (let ((spliced (unwind-protect (tuatara.text:splice-rope indexed 10 10 (tuatara.text:string->rope "y"))
                                (sb-int:unencapsulate 'tuatara.text::clean-junction-p 'counted))))
                 (true (<= looks 2) "~d junctions looked at" looks)
                 (is = (1+ (length octets)) (tuatara.text:rope-octet-length spliced))
                 (is = (tuatara.text:rope-lines indexed) (tuatara.text:rope-lines spliced))))
          (tuatara.text:close-mapping mapping))))))

(define-test (core-edges line-ending-read-without-a-lock)
  ;; how a buffer's lines end is read for each line looked at: as quickly as any other state of it
  (let ((b (tuatara.buffer:make-buffer "core-edges line ending" :text "abc")))
    (unwind-protect
         (flet ((seconds (function)
                  (loop repeat 3
                        minimize (let ((start (get-internal-run-time)))
                                   (loop repeat 2000000 do (funcall function b))
                                   (- (get-internal-run-time) start)))))
           (setf (tuatara.buffer:buffer-local b :line-ending) :crlf)
           (true (tuatara.buffer:buffer-crlf-p b))
           (let ((ending (seconds #'tuatara.buffer:buffer-line-ending))
                 (name (seconds #'tuatara.buffer:buffer-name)))
             (true (< ending (* 3 (max name 1))) "~d against ~d" ending name)))
      (tuatara.buffer:kill-buffer b))))

(define-test (core-edges small-things-packed-with-small-tables)
  ;; packing a few octets does not make tables for a quarter of a million tokens
  (let* ((octets (tuatara.text:string-octets "a small picture's few octets"))
         (made (octets-per-call (lambda () (tuatara.io:deflate octets)) 20)))
    (true (< made (* 400 1024)) "~:d octets" made)
    (is equalp octets (tuatara.io:inflate (tuatara.io:deflate octets)))))

(define-test (core-edges copy-looks-at-stop-and-progress-often)
  ;; a large copy says how far it has got, and looks at a stop, more than once in 64 MB
  (with-test-directory (dir)
    (let ((from (namestring (merge-pathnames "from" dir)))
          (to (namestring (merge-pathnames "to" dir)))
          (calls 0))
      (alexandria:write-byte-vector-into-file
       (make-array (* 33 1024 1024) :element-type '(unsigned-byte 8) :initial-element 7) from)
      (let ((in (sb-posix:open from sb-posix:o-rdonly))
            (out (sb-posix:open to (logior sb-posix:o-wronly sb-posix:o-creat) #o600)))
        (unwind-protect
             (true (tuatara.io:copy-fd-contents in out (* 33 1024 1024) :progress (lambda (done)
                                                                                   (declare (ignore done))
                                                                                   (incf calls))))
          (sb-posix:close in)
          (sb-posix:close out)))
      (true (>= calls 3) "~d" calls)
      (is = (* 33 1024 1024) (sb-posix:stat-size (sb-posix:stat to))))))

(define-test (core-edges names-and-wakes-without-garbage)
  ;; a random name draws its bits from the system, and waking a thread makes nothing
  (let ((made (octets-per-call #'tuatara.io:random-name)))
    (true (< made 512) "~:d octets for each name" made))
  (is = 2 (length (remove-duplicates (list (tuatara.io:random-name) (tuatara.io:random-name)) :test #'string=)))
  (let ((pipe (tuatara.io:make-wake-pipe)))
    (unwind-protect
         (let ((made (octets-per-call (lambda () (tuatara.io:wake pipe) (tuatara.io:drain-wake-pipe pipe)))))
           (true (< made 16) "~:d octets for each wake" made))
      (tuatara.io:close-wake-pipe pipe))))

(define-test (core-edges program-group-ended-without-reading-proc)
  ;; a program no longer wanted, leading no session, is ended by its group without a look at every process
  (let ((looks 0)
        (mark (format nil "33.~d" (+ 100000 (random 900000)))))
    (sb-int:encapsulate 'tuatara.io::processes-in 'counted
                        (lambda (original &rest arguments) (incf looks) (apply original arguments)))
    (unwind-protect
         (let ((process (tuatara.io:start-program (list (tuatara.io:find-program "sh") "-c"
                                                        (format nil "sleep ~a & wait" mark)))))
           (sleep 0.1)
           (sb-thread:join-thread (tuatara.io::end-unwanted-program process) :default nil :timeout 10)
           (true (gone-within mark 3)))
      (sb-int:unencapsulate 'tuatara.io::processes-in 'counted))
    (is = 0 looks)))

(define-test (core-edges one-sandbox-trial-at-a-time)
  ;; asked again and again before it is known, the sandbox is tried once, and the PATH not walked each time
  (let ((kept tuatara.io::**sandbox-state**) (finds 0) (here sb-thread:*current-thread*))
    (unwind-protect
         (progn
           (setf tuatara.io::**sandbox-state** nil)
           (sb-int:encapsulate 'tuatara.io:find-program 'counted
                               (lambda (original &rest arguments)
                                 (when (eq sb-thread:*current-thread* here) (incf finds))
                                 (apply original arguments)))
           (unwind-protect
                (let ((answers (loop repeat 50 collect (tuatara.io:sandbox-status-now))))
                  (true (every (lambda (answer) (member answer '(:unknown :ready :missing :broken))) answers)))
             (sb-int:unencapsulate 'tuatara.io:find-program 'counted))
           (true (<= (count "sandbox trial" (sb-thread:list-all-threads) :key #'sb-thread:thread-name
                                                                         :test #'equal)
                     1))
           (is = 0 finds)
           (tuatara.runtime:wait-until (lambda () (not (find "sandbox trial" (sb-thread:list-all-threads)
                                                             :key #'sb-thread:thread-name :test #'equal)))
                                       10))
      (setf tuatara.io::**sandbox-state** kept))))

(define-test (core-edges processors-counted-once)
  ;; each batch of work run in parallel asks how many processors there are: the system is asked once
  (tuatara.runtime:cpu-count)
  (let ((start (get-internal-real-time)))
    (loop repeat 100000 do (tuatara.runtime:cpu-count))
    (true (< (tuatara.runtime:seconds-since start) (* 1/50 (machine-slowdown)))
          "~,3f s" (tuatara.runtime:seconds-since start))))

(define-test (core-edges lease-keeper-rests)
  ;; with a large file open and nothing writing it, the keeper of its lease does not wake every few seconds
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (tuatara.io:*memory-read-limit* 1024)
          (looks 0))
      (with-visited (b path)
        (is eq :leased (tuatara.io:file-isolation b))
        (sleep 0.2)
        (sb-int:encapsulate 'tuatara.io:lease-breaking-p 'counted
                            (lambda (original fd) (incf looks) (funcall original fd)))
        (unwind-protect (sleep 6)
          (sb-int:unencapsulate 'tuatara.io:lease-breaking-p 'counted))
        (is = 0 looks)
        (tuatara.io:finish-leases)))))

(define-test (core-edges carry-grows-with-the-edits)
  ;; a line count carried over many edits at once takes time growing with their number, not its square
  (flet ((carry-time (count)
           (multiple-value-bind (b carried)
               (counted-carried (format nil "~{~6,'0d~%~}" (loop for i below (* 2 count) collect i)))
             (unwind-protect
                  (let ((tuatara.text:*carried-count-octets* most-positive-fixnum))
                    (tuatara.buffer:buffer-edit b (coerce (loop for i below count
                                                                collect (tuatara.text:make-edit
                                                                         (* i 14) (* i 14)
                                                                         (tuatara.text:string-octets "x")))
                                                          'simple-vector))
                    (let ((start (thread-cpu-time)))
                      (true (tuatara.text:carried-line-number carried (tuatara.buffer:buffer-version b) 0))
                      (- (thread-cpu-time) start)))
               (forget-counted-carried b)))))
    (carry-time 1000)
    (let ((few (max 1 (carry-time 4000))) (many (carry-time 16000)))
      (true (< many (* 8 few)) "~:d for 16,000 edits against ~:d for 4,000 (internal time units)" many few))))

(define-test (core-edges long-names-cut-one-way)
  ;; a name in the trash is cut as every name is, its ending kept, its .trashinfo within 255 octets
  (let ((name (tuatara.io::trash-name (concatenate 'string (make-string 300 :initial-element #\a) ".txt") 2)))
    (true (uiop:string-suffix-p name ".txt.2") "~a" name)
    (true (<= (+ (tuatara.text:string-octet-length name) (length ".trashinfo")) 255)))
  ;; a file whose name is near the longest a name may be is written whole beside itself, then renamed
  (with-test-directory (dir)
    (let ((path (tuatara.io:path-join (namestring dir) (make-string 240 :initial-element #\b))))
      (tuatara.io:replace-file path "written")
      (is string= "written" (file-text path)))))

(define-test (core-edges zip64-archive-said-as-such)
  ;; an archive whose directory is told by a zip64 record is said to be one, not damaged
  (let ((octets (make-array (+ 56 20 22) :element-type '(unsigned-byte 8) :initial-element 0)))
    (tuatara.text:octets-put-integer octets #x06064b50 4 :at 0)
    (tuatara.text:octets-put-integer octets #x07064b50 4 :at 56)
    (tuatara.text:octets-put-integer octets 0 8 :at 64)
    (tuatara.text:octets-put-integer octets 1 4 :at 72)
    (tuatara.text:octets-put-integer octets #x06054b50 4 :at 76)
    (tuatara.text:octets-put-integer octets #xFFFF 2 :at 86)
    (tuatara.text:octets-put-integer octets #xFFFFFFFF 4 :at 88)
    (tuatara.text:octets-put-integer octets #xFFFFFFFF 4 :at 92)
    (let ((said (handler-case (progn (tuatara.io:zip-entries octets) nil)
                  (tuatara.io:zip-error (condition) (princ-to-string condition)))))
      (true (and said (search "zip64" said)) "~a" said))))

(define-test (core-edges shown-place-bound-as-checked)
  ;; a place shown to a program shut in is given to bwrap as a descriptor of what was checked: a part
  ;; of its path swapped for a link after the check is refused, and a missing place is left out
  (with-test-directory (dir)
    (let* ((root (string-right-trim "/" (namestring dir)))
           (part (format nil "~a/part" root))
           (shown (format nil "~a/shown" part))
           (elsewhere (format nil "~a/elsewhere" root))
           (opened (list nil)))
      (ensure-directories-exist (format nil "~a/" shown))
      (ensure-directories-exist (format nil "~a/shown/" elsewhere))
      (let ((arguments (tuatara.io::mount-arguments (list shown) '() '() :opened opened)))
        (unwind-protect (true (member "--ro-bind-fd" arguments :test #'equal) "~s" arguments)
          (dolist (fd (car opened)) (sb-posix:close fd))))
      (setf (car opened) nil)
      (is equal '() (remove-if-not (lambda (word) (search "missing" word))
                                   (tuatara.io::mount-arguments (list (format nil "~a/missing" root)) '() '()
                                                                :opened opened)))
      (sb-int:encapsulate 'tuatara.io::check-shown-place 'swapped-after
                          (lambda (original place)
                            (prog1 (funcall original place)
                              (when (search "/part/shown" place)
                                (rename-file (format nil "~a/" part) (format nil "~a/kept/" root))
                                (sb-posix:symlink elsewhere part)))))
      (unwind-protect
           (fail (tuatara.io::mount-arguments (list shown) '() '() :opened opened) tuatara.io:file-system-error)
        (sb-int:unencapsulate 'tuatara.io::check-shown-place 'swapped-after)
        (dolist (fd (car opened)) (sb-posix:close fd))))))

(define-test (core-edges home-refused-in-its-own-words)
  ;; the home directory, reached through a link too, is refused saying why
  (with-test-directory (dir)
    (let ((home (string-right-trim "/" (sb-ext:native-namestring (user-homedir-pathname))))
          (link (format nil "~ahome-link" (namestring dir))))
      (sb-posix:symlink home link)
      (false (tuatara.io:shown-place-p link))
      (let ((said (handler-case (progn (tuatara.io::check-shown-place link) nil)
                    (tuatara.io:file-system-error (condition) (princ-to-string condition)))))
        (true (and said (search "home directory" said)) "~a" said)))))

(define-test (core-edges empty-group-not-signalled)
  ;; once a program's group is found empty, no signal is sent to its number, which another may now have
  (let ((group-kills 0))
    (sb-int:encapsulate 'tuatara.io::signal-group 'counted
                        (lambda (original group signal)
                          (let ((there (funcall original group signal)))
                            (when (and (= signal sb-unix:sigkill) (not there)) (incf group-kills))
                            there)))
    (unwind-protect
         (let ((process (tuatara.io:start-program (list (tuatara.io:find-program "sh") "-c" "sleep 0.2"))))
           (sleep 0.05)
           (sb-thread:join-thread (tuatara.io::end-unwanted-program process) :default nil :timeout 10))
      (sb-int:unencapsulate 'tuatara.io::signal-group 'counted))
    (is = 0 group-kills)))

(define-test (core-edges xml-comments-hold-no-elements)
  ;; a tag within a comment or CDATA is no element, as the XML reader has it
  (let ((xml "<w:body><!-- <w:p>old</w:p> --><w:p>one</w:p><![CDATA[<w:p>]]><w:p>two</w:p></w:body>"))
    (is equal '("<w:p>one</w:p>" "<w:p>two</w:p>")
        (mapcar (lambda (element) (subseq xml (car element) (cdr element))) (tuatara.text:xml-elements xml "w:p")))
    (is equal '("w:p" "w:p") (mapcar #'first (tuatara.text:xml-top-elements xml 8 (- (length xml) 9))))))

(define-test (core-edges backup-of-a-long-name)
  ;; a file whose name is near the longest a name may be has a backup beside it all the same
  (with-test-directory (dir)
    (let ((path (tuatara.io:path-join (namestring dir) (make-string 240 :initial-element #\c))))
      (put-file path "kept")
      (multiple-value-bind (fd backup) (tuatara.io::backup-copy path (tuatara.io:path-stamp path))
        (unwind-protect (is string= "kept" (file-text backup))
          (sb-posix:close fd)
          (ignore-errors (sb-posix:unlink backup)))))))

(define-test (core-edges sandbox-needs-descriptor-binds)
  ;; a bwrap that cannot be given the places it shows as descriptors is not used: nothing is shown by name
  (with-test-directory (dir)
    (let ((stand-in (put-program (merge-pathnames "bwrap" dir)
                                 (format nil "#!/bin/sh~%case \"$1\" in~%--version) echo 'bubblewrap 0.7.0' ;;~%~
                                              --help) echo 'usage: bwrap [OPTIONS...] [--] COMMAND' ;;~%esac~%"))))
      (unwind-protect
           (let ((tuatara.io::*bwrap-name* stand-in))
             (multiple-value-bind (status detail) (tuatara.io:sandbox-status :again t)
               (is eq :broken status)
               (true (search "--ro-bind-fd" detail) "~a" detail)))
        (tuatara.io:sandbox-status :again t)))))

(define-test (core-edges late-edit-copy-let-go)
  ;; an edit asking a copy of the tree, answered late, leaves the helper running: the copy its late
  ;; answer names is let go over the same connection, which is used again after
  (let ((deleted '()))
    (call-with-fake-helper (lambda (fd how body)
                             (case how
                               (23 (sleep 0.6) (fake-send fd 1 0 (words 42)))
                               (8 (push (octets-u32 body 0) deleted) (fake-send fd 1 0 (words)))
                               (16 (fake-send fd 1 0 (words 0)))))
                           (lambda (helper)
                             (false (nth-value 1 (tuatara.syntax:edit-tree (tuatara.syntax::%make-tree helper 1)
                                                                           (list (list 0 0 1 '(0 . 0) '(0 . 0)
                                                                                       '(0 . 1)))
                                                                           :copy t)))
                             (true (tuatara.runtime:wait-until (lambda () deleted) 5))
                             (is equal '(42) deleted)
                             (true (tuatara.syntax:helper-alive helper))
                             (true (tuatara.runtime:wait-until
                                    (lambda () (tuatara.syntax::helper-idle helper)) 5))
                             (is = 1 (length (tuatara.syntax::helper-connections helper)))
                             (false (tree-holds-error-p (tuatara.syntax::%make-tree helper 1)))
                             (true (tuatara.syntax:helper-alive helper))))))

;;; Late replies of a helper

(defparameter *late-copy-reply* (concatenate '(vector (unsigned-byte 8)) (words 4 1 0 42))
  "A whole reply to an edit asking a copy: the copy's number 42.")

(defun fake-send-in-parts (fd octets &rest cuts)
  "Write OCTETS to FD in parts, cut at each of CUTS (octet offsets), the
parts 0.6 seconds apart; NIL when a part cannot be written."
  (sb-sys:with-pinned-objects (octets)
    (loop for (from to) on (append (list 0) cuts (list (length octets)))
          while to
          do (unless (= from 0) (sleep 0.6))
             (unless (eq t (tuatara.syntax::transfer fd (sb-sys:sap+ (sb-sys:vector-sap octets) from) (- to from)
                                                     :write nil))
               (return nil))
          finally (return t))))

(defun late-edit (helper &key copy)
  "Tell the tree numbered 1 of HELPER of an edit, asking a copy when COPY:
true when the helper answered in time."
  (nth-value 1 (tuatara.syntax:edit-tree (tuatara.syntax::%make-tree helper 1)
                                         (list (list 0 0 1 '(0 . 0) '(0 . 0) '(0 . 1)))
                                         :copy copy)))

(defun late-readers-gone-p ()
  "True when no thread reads a late reply of a helper."
  (notany (lambda (thread) (equal (sb-thread:thread-name thread) "syntax helper's late reply"))
          (sb-thread:list-all-threads)))

(define-test (core-edges late-reply-read-in-parts)
  ;; a late reply cut in its header, or in its body, is read on from where it was cut
  (dolist (cut '(6 14))
    (let ((deleted '()))
      (call-with-fake-helper (lambda (fd how body)
                               (case how
                                 (23 (fake-send-in-parts fd *late-copy-reply* cut))
                                 (8 (push (octets-u32 body 0) deleted) (fake-send fd 1 0 (words)))))
                             (lambda (helper)
                               (false (late-edit helper :copy t))
                               (true (tuatara.runtime:wait-until (lambda () deleted) 5) "cut at ~d" cut)
                               (is equal '(42) deleted)
                               (true (tuatara.syntax:helper-alive helper)))))))

(define-test (core-edges late-reader-ends-helper-as-hung)
  ;; a late reply that never comes whole ends the helper as hung, its buffer's text no more parsed
  (let ((tuatara.syntax:*syntax-helper-wait* 1)
        (buffer (tuatara.buffer:make-buffer "late.rs" :text "fn main() {}")))
    (unwind-protect
         (call-with-fake-helper (lambda (fd how body)
                                  (declare (ignore body))
                                  (when (= how 23) (fake-send-in-parts fd (subseq *late-copy-reply* 0 6))))
                                (lambda (helper)
                                  (setf (tuatara.syntax::helper-buffer helper) buffer)
                                  (false (late-edit helper :copy t))
                                  (true (tuatara.runtime:wait-until (lambda () (not (tuatara.syntax:helper-alive helper)))
                                                                    5))
                                  (is eq :hung (tuatara.syntax:helper-ended helper))
                                  (true (member :all (tuatara.syntax::faulted-names buffer)))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (core-edges late-edit-without-copy-closes)
  ;; an edit asking no copy, answered late, closes its connection at once; one asking a copy whose
  ;; late answer names none deletes nothing, and keeps its connection
  (let ((deleted '()))
    (call-with-fake-helper (lambda (fd how body)
                             (case how
                               (23 (sleep 0.6) (fake-send fd 1 0 (words 0)))
                               (8 (push (octets-u32 body 0) deleted) (fake-send fd 1 0 (words)))))
                           (lambda (helper)
                             (false (late-edit helper))
                             (true (tuatara.runtime:wait-until
                                    (lambda () (null (tuatara.syntax::helper-connections helper))) 0.2))
                             (true (tuatara.syntax:helper-alive helper))))
    (call-with-fake-helper (lambda (fd how body)
                             (case how
                               (23 (sleep 0.6) (fake-send fd 1 0 (words 0)))
                               (8 (push (octets-u32 body 0) deleted) (fake-send fd 1 0 (words)))))
                           (lambda (helper)
                             (false (late-edit helper :copy t))
                             (true (tuatara.runtime:wait-until (lambda () (tuatara.syntax::helper-idle helper)) 5))
                             (true (tuatara.syntax:helper-alive helper))))
    (is eq nil deleted)))

(define-test (core-edges late-readers-held-to-a-few)
  ;; late replies awaited at once are few: past them, the helper is ended as hung
  (let ((tuatara.syntax:*syntax-helper-wait* 5))
    (call-with-fake-helper (lambda (fd how body) (declare (ignore fd how body)))
                           (lambda (helper)
                             (dotimes (i 2)
                               (false (late-edit helper :copy t))
                               (true (tuatara.syntax:helper-alive helper) "after ~d" (1+ i)))
                             (false (late-edit helper :copy t))
                             (false (tuatara.syntax:helper-alive helper))
                             (is eq :hung (tuatara.syntax:helper-ended helper))))
    (true (tuatara.runtime:wait-until #'late-readers-gone-p 5))))

(define-test (core-edges late-reader-ended-with-helper)
  ;; a helper ended while its late reply is awaited leaves no reader and no connection behind
  (let ((deleted '()))
    (call-with-fake-helper (lambda (fd how body)
                             (case how
                               (23 (sleep 1) (fake-send fd 1 0 (words 42)))
                               (8 (push (octets-u32 body 0) deleted) (fake-send fd 1 0 (words)))))
                           (lambda (helper)
                             (false (late-edit helper :copy t))
                             (tuatara.syntax::end-helper helper :stopped)
                             (true (tuatara.runtime:wait-until
                                    (lambda () (null (tuatara.syntax::helper-connections helper))) 3))
                             (true (tuatara.runtime:wait-until #'late-readers-gone-p 3))))
    (is eq nil deleted)))

(define-test (core-edges comment-ends-read-alike)
  ;; "<!-->" begins a comment, and does not end it, wherever elements are found by tag
  (let ((xml "<w:body><!--> <w:p>x</w:p> --><w:p>y</w:p></w:body>"))
    (is equal '("<w:p>y</w:p>")
        (mapcar (lambda (element) (subseq xml (car element) (cdr element))) (tuatara.text:xml-elements xml "w:p")))
    (is = 1 (length (tuatara.text:xml-top-elements xml 8 (- (length xml) 9))))))

(define-test (core-edges large-bounded-repeats-refused)
  ;; a repeat bounded at many times counts as one without bound; a few times does not
  (false (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "(a+){2,25}b") "aab"))
  (false (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "(a|a){1,30}b") "aab"))
  (true (cl-ppcre:scan (tuatara.syntax::compile-regex-for-query "(a+){2,3}b") "aab")))

(define-test (core-edges xml-elements-read-once)
  ;; elements found by tag read the text once: many comments before them, many elements after the one asked
  ;; for, many top elements each found in turn
  (let ((comments (format nil "<w:body>~{~a~}<w:p>x</w:p></w:body>" (make-list 30000 :initial-element "<!---->")))
        (paragraphs (format nil "<w:body>~{~a~}</w:body>" (make-list 20000 :initial-element "<w:p>x</w:p>"))))
    (multiple-value-bind (found ended) (within-seconds 1 (lambda () (tuatara.text:xml-elements comments "w:p")))
      (true ended "not found within a second")
      (is = 1 (length found)))
    (multiple-value-bind (found ended)
        (within-seconds 1 (lambda () (tuatara.text:xml-top-elements paragraphs 8 (- (length paragraphs) 9))))
      (true ended "not found within a second")
      (is = 20000 (length found)))))

(define-test (core-edges zip-entry-held-to-its-size)
  ;; a deflated entry that unpacks to more than its size says is refused, never unpacked past it
  (let* ((octets (deflated-zip))
         (entry (copy-list (zip-entry-named octets "long.xml"))))
    (setf (getf entry :size) 100)
    (fail (tuatara.defaults::zip-entry-octets octets entry) 'tuatara.defaults::zip-error)))
