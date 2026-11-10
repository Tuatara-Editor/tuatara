;;;; test/syntax-helpers.lisp --- Tree-sitter: helpers kept, shared and ended, and what they are told.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (syntax lying-matches-cost-little)
  ;; a reply of more matches than the text asked about could have ends its
  ;; helper at once; matches whose tests would read the whole text over and
  ;; over fail those tests past what they may read, at once
  (let* ((grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar "((identifier) @name (#match? @name \"^x\"))"))
         (rope (tuatara.text:string->rope (make-string (* 4 1024 1024) :initial-element #\x)))
         (bytes (tuatara.text:rope-octet-length rope)))
    (dolist (case (list (list 5000 10) (list 100 bytes)))
      (destructuring-bind (matches width) case
        (let ((reply (apply #'concatenate '(vector (unsigned-byte 8))
                            (loop repeat matches collect (words 0 1 0 0 width 0))))
              (seen 0) ended (began (get-internal-real-time)))
          (call-with-fake-helper
           (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 reply))
           (lambda (helper)
             (setf (gethash (tuatara.syntax::query-number query) (tuatara.syntax::helper-queries helper)) t)
             (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                                 (declare (ignore pattern captures count))
                                                 (incf seen))
                                               query (tuatara.syntax::%make-tree helper 1) rope 0 10)
             (setf ended (tuatara.syntax:helper-ended helper))))
          (if (= matches 5000)
              (is eq :broken ended "~s" case)
              (progn (false ended) (is = 0 seen)))
          (true (< seen matches))
          (true (< (tuatara.runtime:seconds-since began) 2)))))))

(define-test (syntax one-helper-for-a-buffer)
  ;; threads asking at once for a buffer's helper all get the one helper
  (let ((buffer (tuatara.buffer:make-buffer "*one-helper*" :text "fn main() {}"))
        (got '()) (lock (sb-thread:make-mutex)))
    (unwind-protect
         (let ((threads (loop repeat 4
                              collect (sb-thread:make-thread
                                       (lambda ()
                                         (let ((helper (tuatara.syntax:buffer-helper buffer)))
                                           (sb-thread:with-mutex (lock) (push helper got))))))))
           (mapc #'sb-thread:join-thread threads)
           (is = 4 (length got))
           (is = 1 (length (remove-duplicates got)))
           (is = 1 (count buffer tuatara.syntax::**helpers** :key #'tuatara.syntax::helper-buffer)))
      (tuatara.buffer:kill-buffer buffer))
    ;; killed, its helper ends, and none is started for it after
    (is = 0 (count buffer tuatara.syntax::**helpers** :key #'tuatara.syntax::helper-buffer))
    (false (tuatara.syntax:buffer-helper buffer))))

(define-test (syntax a-file-counts-once-however-often-opened)
  ;; a grammar going wrong on one file opened again and again is still one
  ;; text, not one more each time: no language is turned off for all by it
  (let ((grammar (tuatara.syntax:find-grammar "rust"))
        (buffers '()))
    (clrhash tuatara.syntax::**grammar-faults**)
    (unwind-protect
         (progn
           (dotimes (i 4)
             (let ((buffer (tuatara.buffer:make-buffer (format nil "*same-file-~d*" i) :text "fn main() {}")))
               (setf (tuatara.buffer:buffer-file buffer) "/nonexistent/evidence/same.rs")
               (push buffer buffers)
               (tuatara.syntax::grammar-went-wrong grammar buffer :crashed)))
           (false (tuatara.syntax:grammar-faulted grammar))
           (true (every (lambda (buffer) (tuatara.syntax::grammar-off-p grammar buffer)) buffers)))
      (mapc #'tuatara.buffer:kill-buffer buffers)
      (clrhash tuatara.syntax::**grammar-faults**)
      (setf (tuatara.syntax:grammar-faulted grammar) nil))))

(define-test (syntax one-kept-helper-of-a-kind)
  ;; threads asking at once for the helper kept for all start one of it
  (with-test-directory (directory)
    (let* ((directories (cons (namestring directory) (tuatara.syntax:grammar-directories)))
           (got '()) (lock (sb-thread:make-mutex)))
      (unwind-protect
           (let ((threads (loop repeat 4
                                collect (sb-thread:make-thread
                                         (lambda ()
                                           (let* ((tuatara.syntax:*grammar-directories* directories)
                                                  (helper (tuatara.syntax:kept-helper :shared)))
                                             (sb-thread:with-mutex (lock) (push helper got))))))))
             (mapc #'sb-thread:join-thread threads)
             (is = 1 (length (remove-duplicates got)))
             (is = 1 (count-if (lambda (helper) (and (eq :shared (tuatara.syntax:helper-kind helper))
                                                     (equal directories (tuatara.syntax::helper-directories helper))))
                               tuatara.syntax::**helpers**)))
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (equal directories (tuatara.syntax::helper-directories helper))
            (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax editor-thread-never-waits-for-a-start)
  ;; on a thread that must not wait, a helper kept for all that is not
  ;; running is started in the background, NIL given at once meanwhile
  (with-test-directory (directory)
    (let ((directories (cons (namestring directory) (tuatara.syntax:grammar-directories))))
      (unwind-protect
           (let ((tuatara.syntax:*grammar-directories* directories))
             (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-must-not-wait-p (this-thread-only)))
                   (asking sb-thread:*current-thread*)
                   (started-by '()))
               ;; where each start is made: never on the thread that asked
               (sb-int:encapsulate 'tuatara.syntax::start-helper 'note-thread
                                   (lambda (function &rest arguments)
                                     (push sb-thread:*current-thread* started-by)
                                     (apply function arguments)))
               (unwind-protect
                    (progn
                      (false (tuatara.syntax:kept-helper :rules))
                      (true (loop repeat 500
                                  thereis (tuatara.syntax::known-kept-helper (cons :rules directories))
                                  do (sleep 0.01)))
                      (true started-by)
                      (false (member asking started-by)))
                 (sb-int:unencapsulate 'tuatara.syntax::start-helper 'note-thread))))
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (equal directories (tuatara.syntax::helper-directories helper))
            (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax a-link-lends-no-trust)
  ;; a file reached through a link inside a project you trust is parsed as
  ;; the file it really is, outside it
  (with-test-directory (trusted)
    (with-test-directory (elsewhere)
      (let ((tuatara-user:*lsp-trusted-projects* (list (namestring trusted)))
            (inside (namestring (merge-pathnames "inside.rs" trusted)))
            (outside (namestring (merge-pathnames "evidence.rs" elsewhere)))
            (link (namestring (merge-pathnames "link.rs" trusted))))
        (ensure-directories-exist (merge-pathnames ".git/" trusted))
        (dolist (path (list inside outside)) (with-open-file (o path :direction :output) (write-line "fn f() {}" o)))
        (sb-posix:symlink outside link)
        (flet ((trusted-p (path)
                 (let ((buffer (tuatara.buffer:make-buffer (format nil "*trust ~a*" path) :text "")))
                   (unwind-protect
                        (progn (setf (tuatara.buffer:buffer-file buffer) path)
                               (tuatara.defaults::buffer-in-trusted-project-p buffer))
                     (tuatara.buffer:kill-buffer buffer)))))
          (true (trusted-p inside))
          (false (trusted-p link)))))))

(define-test (syntax counts-checked-before-anything-made)
  ;; a reply to compiling rules naming more captures than the rules have,
  ;; or a grammar of more node types than tree-sitter can number, is
  ;; refused before anything is made of it
  (let ((grammar (tuatara.syntax:find-grammar "rust")))
    (dolist (case (list (list :rules (words 0 #x7ffffff)) (list :grammar (words 1 #x7ffffff))))
      (destructuring-bind (kind reply) case
        (let (ended (before (sb-ext:get-bytes-consed)))
          (call-with-fake-helper
           (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 reply))
           (lambda (helper)
             (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-counts/")))
               (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                 (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper)
                       tuatara.syntax::**kept-helpers**))
               (unwind-protect
                    (handler-case (if (eq kind :rules)
                                      (tuatara.syntax::compile-query-text grammar "(identifier) @name")
                                      (tuatara.syntax::load-grammar "fake"))
                      (tuatara.syntax:query-error () nil))
                 (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                   (setf tuatara.syntax::**kept-helpers** (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))
             (setf ended (tuatara.syntax:helper-ended helper))))
          (is eq :broken ended "~s" kind)
          (true (< (- (sb-ext:get-bytes-consed) before) (* 64 1024 1024)) "~s" kind))))))

(define-test (syntax reset-refused-while-parsing)
  ;; a parser is not reset, nor deleted under it, while it parses on
  ;; another connection
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((tuatara.syntax:*syntax-helper-wait* 1))
       (call-with-own-helper
        (lambda (helper)
          (let* ((parser (tuatara.syntax:make-parser grammar helper))
                 (parsing (sb-thread:make-thread
                           (lambda ()
                             (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !l def") :limit 2)))))
            (sleep 0.3)
            (tuatara.syntax::with-call (call helper tuatara.syntax::+op-reset+)
              (tuatara.syntax::call-u32 call (tuatara.syntax::parser-number parser))
              (is eql 1 (tuatara.syntax::call-send call)))
            (sb-thread:join-thread parsing :default nil))))))))

(define-test (syntax helpers-in-use-not-ended-for-room)
  ;; past *syntax-helpers-most*, a helper whose buffer is shown or parsing
  ;; is left running, the helpers going past the most
  (let ((buffers (loop for i below 3
                       collect (tuatara.buffer:make-buffer (format nil "*in-use-~d*" i) :text "fn f() {}"))))
    (unwind-protect
         (let ((tuatara.syntax::*syntax-helpers-most* 1)
               (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-buffer-shown-p
                                                            (lambda (buffer) (member buffer buffers)))))
           (let ((helpers (mapcar #'tuatara.syntax:buffer-helper buffers)))
             (true (every #'tuatara.syntax:helper-alive helpers))))
      (mapc #'tuatara.buffer:kill-buffer buffers))))

(define-test (syntax no-sandbox-nothing-pending)
  ;; without the sandbox a grammar not yet loaded is not waited for: no
  ;; helper is started again and again, no thread made for each redraw
  (let ((starts 0) (kept (gethash "toml" tuatara.syntax::*grammars*)))
    (call-without-sandbox
     (lambda ()
       (tuatara.io:sandbox-status :again t)
       (sb-int:encapsulate 'tuatara.syntax::start-helper 'counted
                           (lambda (function &rest arguments) (incf starts) (apply function arguments)))
       (remhash "toml" tuatara.syntax::*grammars*)
       (let ((buffer (tuatara.buffer:make-buffer "*pending.toml*" :text "a = 1"))
             (tuatara.syntax::*syntax-helper-unconfined* nil)
             (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-must-not-wait-p (this-thread-only))))
         (unwind-protect
              (let ((threads (length (sb-thread:list-all-threads))))
                (tuatara.modes:set-major-mode buffer 'tuatara.defaults::toml-mode)
                (dotimes (i 20) (tuatara.defaults::syntax-refresh-tick nil))
                (false (tuatara.syntax:grammar-pending-p "toml"))
                (false (tuatara.buffer:buffer-local buffer :syntax-pending))
                (is = 0 starts)
                (true (<= (length (sb-thread:list-all-threads)) (+ threads 1))))
           (sb-int:unencapsulate 'tuatara.syntax::start-helper 'counted)
           (tuatara.buffer:kill-buffer buffer)))))
    (if kept (setf (gethash "toml" tuatara.syntax::*grammars*) kept) (remhash "toml" tuatara.syntax::*grammars*))))

(define-test (syntax grammar-ending-its-loader-given-up)
  ;; a grammar whose loading ends the program loading it is tried a few
  ;; times, then given up on as missing: no helper started without end
  (with-test-directory (directory)
    (let ((source (merge-pathnames "crashy.c" directory))
          (library (merge-pathnames "crashy.so" directory))
          (rules 0))
      (with-open-file (o source :direction :output)
        (format o "#include <stdlib.h>~%const void *tree_sitter_crashy(void) { abort(); }~%"))
      (multiple-value-bind (output error code)
          (uiop:run-program (list "cc" "-shared" "-fPIC" "-o" (namestring library) (namestring source))
                            :output :string :error-output :output :ignore-error-status t)
        (declare (ignore error))
        (is = 0 code "~a" output))
      (sb-int:encapsulate 'tuatara.syntax::start-helper 'counted
                          (lambda (function directories kind)
                            (when (eq kind :rules) (incf rules))
                            (funcall function directories kind)))
      (unwind-protect
           (let ((tuatara.syntax:*grammar-directories* (cons (namestring directory)
                                                             (tuatara.syntax:grammar-directories))))
             (remhash "crashy" tuatara.syntax::**grammar-load-deaths**)
             (dotimes (i 10) (false (tuatara.syntax:find-grammar "crashy")))
             (true (<= rules (1+ tuatara.syntax::*grammar-load-deaths-most*)) "~d starts" rules)
             (false (tuatara.syntax:grammar-pending-p "crashy")))
        (sb-int:unencapsulate 'tuatara.syntax::start-helper 'counted)
        (remhash "crashy" tuatara.syntax::*grammars*)
        (remhash "crashy" tuatara.syntax::**grammar-load-deaths**)
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (member (namestring directory) (tuatara.syntax::helper-directories helper) :test #'equal)
            (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax reply-too-large-refused-cleanly)
  ;; a helper whose reply would be past its most refuses it whole, writes
  ;; nothing past it, and goes on; built here with a small most
  (with-test-directory (directory)
    (let* ((ts (namestring (asdf:system-relative-pathname :tuatara "build/grammar-sources/tree-sitter/")))
           (root (namestring (asdf:system-relative-pathname :tuatara "")))
           (program (namestring (merge-pathnames "tuatara-syntax-helper" directory))))
      (multiple-value-bind (output error code)
          (uiop:run-program (list "cc" "-O1" "-std=c11" "-DNDEBUG" "-D_POSIX_C_SOURCE=200112L" "-D_DEFAULT_SOURCE"
                                  "-DREPLY_MOST=16384u"
                                  (format nil "-I~alib/include" ts) (format nil "-I~alib/src" ts)
                                  (format nil "~alib/src/lib.c" ts) (format nil "~asyntax/syntax.c" root)
                                  (format nil "~asyntax/helper.c" root)
                                  "-rdynamic" "-lpthread" "-ldl" "-o" program)
                            :output :string :error-output :output :ignore-error-status t)
        (declare (ignore error))
        (is = 0 code "~a" output))
      (let* ((tuatara.syntax:*grammar-directories* (cons (namestring directory) (tuatara.syntax:grammar-directories)))
             (helper (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
             (grammar (tuatara.syntax:find-grammar "rust"))
             (query (tuatara.syntax:compile-query grammar "(identifier) @name"))
             (rope (tuatara.text:string->rope (format nil
                                                      "~{fn f~d(a: i32, b: i32) -> i32 { a + b }~%~}"
                                                      (loop for i below 400 collect i)))))
        (unwind-protect
             (let* ((parser (tuatara.syntax:make-parser grammar helper))
                    (tree (tuatara.syntax:parse-rope parser rope))
                    (seen 0) (began (get-internal-real-time)))
               (true tree)
               (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                                   (declare (ignore pattern captures count))
                                                   (incf seen))
                                                 query tree rope 0 (tuatara.text:rope-octet-length rope))
               (is = 0 seen)
               (true (< (tuatara.runtime:seconds-since began) 1))
               (true (tuatara.syntax:helper-alive helper))
               ;; and a small one is answered as before
               (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                                   (declare (ignore pattern captures count))
                                                   (incf seen))
                                                 query tree rope 0 40)
               (true (plusp seen))
               (true (tuatara.syntax:helper-alive helper))
               ;; text in another language whose matches are too many: its trees let go
               (let ((held (tuatara.syntax:helper-memory helper)))
                 (dotimes (i 5)
                   (tuatara.syntax::parse-texts rope (list (tuatara.syntax::make-text-parse
                                                            grammar
                                                            (list (cons 0 (tuatara.text:rope-octet-length rope))) nil
                                                            (list (list query 0 (tuatara.text:rope-octet-length rope))) 2 nil))
                                                (lambda (trees) (declare (ignore trees)))
                                                :helper helper))
                 (true (< (tuatara.syntax:helper-memory helper) (+ held (* 64 1024))) "~d held, ~d before"
                       (tuatara.syntax:helper-memory helper) held)))
          (when helper (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax showing-text-never-waits-for-its-helper)
  ;; on the editor's own thread, a buffer's helper is set going at once when
  ;; nothing holds it up; while another start does, none is given at once,
  ;; its helper started in the background
  (tuatara.io:sandbox-status)
  (let ((buffer (tuatara.buffer:make-buffer "*soon*" :text "fn main() {}"))
        (holding (sb-thread:make-semaphore)))
    (unwind-protect
         (let* ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-must-not-wait-p (this-thread-only)))
               (letting-go (sb-thread:make-semaphore))
               (given-up nil)
               (holder (sb-thread:make-thread
                        (lambda ()
                          (sb-thread:with-mutex (tuatara.syntax::**room-lock**)
                            (sb-thread:signal-semaphore holding)
                            (unless (sb-thread:wait-on-semaphore letting-go :timeout 10)
                              (setf given-up t)))))))
           (sb-thread:wait-on-semaphore holding)
           (false (tuatara.syntax:buffer-helper-soon buffer))
           ;; given up on while the start was still held up, not after waiting for it
           (false given-up)
           (sb-thread:signal-semaphore letting-go)
           (sb-thread:join-thread holder)
           (true (loop repeat 500
                       thereis (let ((own (tuatara.buffer:buffer-local buffer :syntax-helper)))
                                 (and own (tuatara.syntax:helper-alive own)))
                       do (sleep 0.01))))
      (tuatara.buffer:kill-buffer buffer)))
  ;; nothing holding it up: set going at once
  (let ((buffer (tuatara.buffer:make-buffer "*soon-now*" :text "fn main() {}")))
    (unwind-protect
         (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-must-not-wait-p (this-thread-only)))
               (tuatara.io::*at-once-wait* 30))
           (true (tuatara.syntax:buffer-helper-soon buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax buffer-killed-while-its-helper-starts)
  ;; a buffer killed while its helper starts leaves none behind
  (let ((buffer (tuatara.buffer:make-buffer "*killed-starting*" :text "fn main() {}")))
    (let ((killer nil))
      ;; killed from another thread while the start is under way
      (sb-int:encapsulate 'tuatara.syntax::start-helper 'kill-meanwhile
                          (lambda (function &rest arguments)
                            (setf killer (sb-thread:make-thread (lambda () (tuatara.buffer:kill-buffer buffer))))
                            (sleep 0.2)
                            (apply function arguments)))
      (unwind-protect
           (tuatara.syntax:buffer-helper buffer)
        (sb-int:unencapsulate 'tuatara.syntax::start-helper 'kill-meanwhile)
        (when killer (sb-thread:join-thread killer :default nil :timeout 10))))
    (false (find-if (lambda (helper) (and (eq buffer (tuatara.syntax::helper-buffer helper))
                                          (tuatara.syntax:helper-alive helper)))
                    tuatara.syntax::**helpers**))))

(define-test (syntax killing-never-waits-for-helper-starts)
  ;; on the editor's own thread, a buffer with a helper is killed at once while another helper's
  ;; start holds the room for helpers; its helper is ended all the same, its process with it
  (let* ((buffer (tuatara.buffer:make-buffer "*killed-while-busy*" :text "fn main() {}"))
         (helper (tuatara.syntax:buffer-helper buffer))
         (process (tuatara.syntax::helper-process helper))
         (holding (sb-thread:make-semaphore))
         (letting-go (sb-thread:make-semaphore))
         (holder (sb-thread:make-thread (lambda ()
                                          (sb-thread:with-mutex (tuatara.syntax::**room-lock**)
                                            (sb-thread:signal-semaphore holding)
                                            (sb-thread:wait-on-semaphore letting-go :timeout 10))))))
    (true (tuatara.syntax:helper-alive helper))
    (sb-thread:wait-on-semaphore holding)
    (unwind-protect
         (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-must-not-wait-p (this-thread-only)))
               (began (get-internal-real-time)))
           (tuatara.buffer:kill-buffer buffer)
           (true (< (tuatara.runtime:seconds-since began) 0.05)))
      (sb-thread:signal-semaphore letting-go)
      (sb-thread:join-thread holder :default nil))
    (false (tuatara.syntax:helper-alive helper))
    (true (loop repeat 500
                thereis (not (sb-ext:process-alive-p process))
                do (sleep 0.01)))))

(define-test (syntax start-finishing-for-killed-buffer-ends-its-helper)
  ;; a buffer killed while a start for it holds the room for helpers, before it stops being live:
  ;; the helper that start then makes is ended, not kept for it
  (let ((buffer (tuatara.buffer:make-buffer "*killed-before-start-ends*" :text "fn main() {}")))
    (unwind-protect
         (sb-thread:with-mutex (tuatara.syntax::**room-lock**)
           (tuatara.syntax::forget-killed-buffer-helper buffer)
           (true (tuatara.buffer:live-buffer-p buffer))
           (false (tuatara.syntax::start-own-helper-now nil buffer nil))
           (false (tuatara.buffer:buffer-local buffer :syntax-helper)))
      (tuatara.buffer:kill-buffer buffer))
    (false (find-if (lambda (helper) (and (eq buffer (tuatara.syntax::helper-buffer helper))
                                          (tuatara.syntax:helper-alive helper)))
                    tuatara.syntax::**helpers**))))

(define-test (syntax trusted-text-ends-its-own-helper)
  ;; a file's project trusted after its own helper started: on the editor's own thread its text is
  ;; given to the shared helper, and its own one ended
  (let ((buffer (tuatara.buffer:make-buffer "*trusted-later*" :text "fn main() {}")))
    (unwind-protect
         (let ((own (tuatara.syntax:buffer-helper buffer)))
           (true (tuatara.syntax:helper-alive own))
           (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-trusted-text-p
                                                              (lambda (other) (eq other buffer))
                                                              'tuatara.syntax:host-must-not-wait-p (this-thread-only))))
             (tuatara.syntax:buffer-helper-soon buffer))
           (false (tuatara.syntax:helper-alive own))
           (false (tuatara.buffer:buffer-local buffer :syntax-helper)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax helper-connections-closed-when-ending-fails)
  ;; ending a helper's process failing (no thread to be had to wait for its end): the helper is
  ;; ended and its connection closed all the same
  (let* ((helper (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
         (fd (tuatara.syntax::helper-fd helper))
         (process (tuatara.syntax::helper-process helper)))
    (sb-int:encapsulate 'tuatara.io:end-sandbox 'failing (lambda (function &rest arguments)
                                                           (declare (ignore function arguments))
                                                           (error "No thread to wait for the end")))
    (unwind-protect
         (finish (tuatara.syntax::stop-helper helper :wait nil))
      (sb-int:unencapsulate 'tuatara.io:end-sandbox 'failing))
    (false (tuatara.syntax:helper-alive helper))
    (fail (sb-posix:fcntl fd sb-posix:f-getfd))
    (tuatara.io:end-sandbox process)))

(define-test (syntax honest-helpers-not-blamed)
  ;; tests of a long captured text past what they may read do not pass, the
  ;; helper left alone; a whole text's matches taking long are slow, not a
  ;; fault of the buffer's text; reading it again tries its grammars again
  (let* ((grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar "((string_literal) @s (#match? @s \"x\"))"))
         (text (format nil "fn f() { let s = \"~a\"; }~%" (make-string (* 3 1024 1024) :initial-element #\x)))
         (rope (tuatara.text:string->rope text)))
    (call-with-own-helper
     (lambda (helper)
       (let* ((parser (tuatara.syntax:make-parser grammar helper))
              (tree (tuatara.syntax:parse-rope parser rope))
              (seen 0))
         (true tree)
         (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                             (declare (ignore pattern captures count))
                                             (incf seen))
                                           query tree rope 0 20)
         (is = 0 seen)
         (true (tuatara.syntax:helper-alive helper))))))
  (let ((buffer (tuatara.buffer:make-buffer "*slow-whole*" :text "fn main() {}"))
        (tuatara.syntax:*syntax-helper-wait* 0.5))
    (unwind-protect
         (progn
           (call-with-fake-helper
            :never
            (lambda (helper)
              (setf (tuatara.syntax::helper-buffer helper) buffer)
              (let ((query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "rust") "(identifier) @x")))
                (setf (gethash (tuatara.syntax::query-number query) (tuatara.syntax::helper-queries helper)) t)
                (tuatara.syntax:map-query-matches (lambda (&rest r) (declare (ignore r)))
                                                  query (tuatara.syntax::%make-tree helper 1)
                                                  (tuatara.buffer:buffer-text buffer) 0 12))
              (is eq :slow (tuatara.syntax:helper-ended helper))))
           (false (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer))
           (tuatara.syntax::note-faulted buffer :all)
           (true (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer))
           (tuatara.defaults::parse-afresh buffer)
           (false (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax a-text-counted-once-whatever-it-is-called)
  ;; one file reached by a link and by its own path, one text pasted twice,
  ;; one note exported again and again: each one text
  (with-test-directory (directory)
    (let* ((real (namestring (merge-pathnames "real.rs" directory)))
           (link (namestring (merge-pathnames "link.rs" directory))))
      (with-open-file (o real :direction :output) (write-line "fn f() {}" o))
      (sb-posix:symlink real link)
      (is equal (tuatara.syntax:text-key :file real) (tuatara.syntax:text-key :file link))
      (let ((a (tuatara.buffer:make-buffer "*pasted-a*" :text "fn g() {}"))
            (b (tuatara.buffer:make-buffer "*pasted-b*" :text "fn g() {}"))
            (c (tuatara.buffer:make-buffer "*pasted-c*" :text "fn h() {}")))
        (unwind-protect
             (progn
               (is equal (tuatara.syntax::text-identity a) (tuatara.syntax::text-identity b))
               (false (equal (tuatara.syntax::text-identity a) (tuatara.syntax::text-identity c))))
          (mapc #'tuatara.buffer:kill-buffer (list a b c))))
      (is equal (tuatara.syntax:text-key :file nil :text "* a note") (tuatara.syntax:text-key :text "* a note")))))

(define-test (syntax replies-held-to-what-was-asked)
  ;; a reply to a parse longer than a parse's reply can be ends its helper
  (let (ended)
    (call-with-fake-helper
     (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 (apply #'words (loop repeat 25 collect 1))))
     (lambda (helper)
       (tuatara.syntax:parse-rope (tuatara.syntax::%make-parser helper 5 (tuatara.syntax:find-grammar "rust"))
                                  (tuatara.text:string->rope "fn f() {}"))
       (setf ended (tuatara.syntax:helper-ended helper))))
    (is eq :broken ended)))

(define-test (syntax grammar-unanswered-asked-again)
  ;; a grammar asked for while no connection to the helper loading grammars
  ;; can be had is asked for again later, not taken as missing
  (let ((kept (gethash "toml" tuatara.syntax::*grammars*)))
    (remhash "toml" tuatara.syntax::*grammars*)
    (unwind-protect
         (progn
           (tuatara.syntax:kept-helper :rules)
           (sb-int:encapsulate 'tuatara.syntax::take-call 'none
                               (lambda (function helper) (declare (ignore function helper)) nil))
           (unwind-protect (false (tuatara.syntax:find-grammar "toml"))
             (sb-int:unencapsulate 'tuatara.syntax::take-call 'none))
           (false (gethash "toml" tuatara.syntax::*grammars*))
           (true (tuatara.syntax:find-grammar "toml")))
      (if kept (setf (gethash "toml" tuatara.syntax::*grammars*) kept) (remhash "toml" tuatara.syntax::*grammars*)))))

(define-test (syntax slow-grammar-load-not-counted)
  ;; a grammar slow to load is given up on this time, not counted against it
  ;; nor taken as missing
  (remhash "slowly" tuatara.syntax::**grammar-load-deaths**)
  (call-with-fake-helper
   :never
   (lambda (helper)
     (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-slow/"))
           (tuatara.syntax::*grammar-load-wait* 0.2))
       (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
         (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper) tuatara.syntax::**kept-helpers**))
       (unwind-protect
            (is eq :unknown (tuatara.syntax::load-grammar "slowly"))
         (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
           (setf tuatara.syntax::**kept-helpers** (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))
     (is eq :slow (tuatara.syntax:helper-ended helper))))
  (false (gethash "slowly" tuatara.syntax::**grammar-load-deaths**)))

(define-test (syntax start-at-once-never-waits)
  ;; on the editor's own thread a start that would wait (another under way,
  ;; or the program slow to set going) is left to the background at once,
  ;; no helper ended for room by it
  (tuatara.io:sandbox-status)
  (let ((other (tuatara.buffer:make-buffer "*room-other*" :text "fn f() {}"))
        (buffer (tuatara.buffer:make-buffer "*at-once*" :text "fn main() {}")))
    (unwind-protect
         (let ((tuatara.syntax::*syntax-helpers-most* 1)
               (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-must-not-wait-p (this-thread-only)))
               (tuatara.io::*at-once-wait* 0.05)
               (holding (sb-thread:make-semaphore)))
           (let ((first (tuatara.syntax:buffer-helper other))
                 (holder (sb-thread:make-thread
                          (lambda ()
                            (sb-thread:with-mutex (tuatara.io::**starting**)
                              (sb-thread:signal-semaphore holding)
                              (sleep 0.5))))))
             (sb-thread:wait-on-semaphore holding)
             ;; the start busy: given up on at once, nothing ended for room
             (let ((began (get-internal-real-time)))
               (is eq :busy (tuatara.syntax::start-own-helper :buffer buffer :at-once t))
               (true (< (tuatara.runtime:seconds-since began) 0.05)))
             (true (tuatara.syntax:helper-alive first))
             (sb-thread:join-thread holder))
           ;; set going slowly: given up on at once, started in the background
           (tuatara.syntax:forget-buffer-helper buffer)
           (sb-int:encapsulate 'tuatara.io:sandbox-start 'slow (lambda (function &rest arguments)
                                                                (sleep 0.3)
                                                                (apply function arguments)))
           (unwind-protect
                (let ((began (get-internal-real-time)))
                  (false (tuatara.syntax:buffer-helper-soon buffer))
                  (true (< (tuatara.runtime:seconds-since began) 0.2)))
             (sb-int:unencapsulate 'tuatara.io:sandbox-start 'slow))
           (true (loop repeat 500
                       thereis (let ((own (tuatara.buffer:buffer-local buffer :syntax-helper)))
                                 (and own (tuatara.syntax:helper-alive own)))
                       do (sleep 0.01)))
           (sleep 0.5)
           (is = 1 (count-if (lambda (helper) (and (eq buffer (tuatara.syntax::helper-buffer helper))
                                                   (tuatara.syntax:helper-alive helper)))
                             tuatara.syntax::**helpers**)))
      (tuatara.buffer:kill-buffer buffer)
      (tuatara.buffer:kill-buffer other))))

(define-test (syntax start-given-up-closes-what-it-held)
  ;; a start given up on before it was ever taken closes the helper's end of
  ;; its socket: no descriptor left open by it
  (tuatara.io:sandbox-status)
  (flet ((open-descriptors () (length (directory "/proc/self/fd/*" :resolve-symlinks nil))))
    (let ((before (open-descriptors)))
      (let ((tuatara.io::*at-once-wait* 0))
        (dotimes (i 5)
          (let ((helper (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own :at-once t)))
            (when (tuatara.syntax:helper-p helper) (tuatara.syntax::stop-helper helper)))))
      (sleep 1)
      (true (<= (open-descriptors) before) "~d open, ~d before" (open-descriptors) before))))

(define-test (syntax revert-forgets-faults-once-it-reads-again)
  ;; reading a text again forgets what went wrong in it, with the export's
  ;; count of its file too; answering no to reading it again forgets nothing
  (with-test-directory (directory)
    (let ((path (namestring (merge-pathnames "note.rs" directory)))
          (answer nil) (buffer nil) (kept-after-no nil))
      (with-open-file (o path :direction :output) (write-line "fn f() {}" o))
      (sb-int:encapsulate 'tuatara.command:yes-or-no-p* 'answer
                          (lambda (function prompt) (declare (ignore function prompt)) answer))
      (unwind-protect
           (run-keys (list (lambda ()
                             (setf buffer (tuatara.command:current-buffer))
                             (tuatara.syntax::note-faulted buffer :all)
                             (tuatara.syntax::note-faulted (tuatara.syntax:text-key :file path) "rust")
                             (tuatara.buffer:buffer-insert buffer 0 " "))
                           "M-x r e v e r t - b u f f e r RET"
                           (lambda ()
                             (setf kept-after-no (and (tuatara.syntax::faulted-names buffer) t)
                                   answer t))
                           "M-x r e v e r t - b u f f e r RET")
                     :file path)
        (sb-int:unencapsulate 'tuatara.command:yes-or-no-p* 'answer))
      (true kept-after-no)
      (false (tuatara.syntax::faulted-names buffer))
      (false (tuatara.syntax::faulted-names (tuatara.syntax:text-key :file path))))))

(define-test (syntax spent-text-budget-keeps-tests-that-read-nothing)
  ;; once the text matches' tests may read is spent, a test that reads none
  ;; still passes as it would: a not-equal of texts of other lengths
  (let ((rope (tuatara.text:string->rope "abc"))
        (captures (make-array 4 :element-type 'fixnum :initial-contents '(0 0 3 0)))
        (tuatara.syntax::*capture-text-left* -1))
    (true (tuatara.syntax::text-test-passes-p :eq 0 (tuatara.text:string-octets "longer text") t rope captures 1))))

(define-test (syntax looping-scanner-ended-at-its-deadline)
  ;; a scanner that never returns ("!l") cannot be stopped from inside: the
  ;; program parsing is ended when its time is up, and the editor carries on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((tuatara.syntax:*syntax-helper-wait* 1))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper))
                (began (get-internal-real-time)))
            (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !l def")
                                                                       :limit 0.5)
              (false tree)
              (is eq :hung why)
              (is eq :hung (tuatara.syntax:helper-ended helper))
              (true (< (tuatara.runtime:seconds-since began) 5))
              (false (loop repeat 200
                           while (sb-ext:process-alive-p (tuatara.syntax::helper-process helper))
                           do (sleep 0.01)
                           finally (return (sb-ext:process-alive-p (tuatara.syntax::helper-process helper)))))
              (true (parses-elsewhere))))))))))

(define-test (syntax runaway-scanner-held-to-its-memory)
  ;; a scanner taking address space without end ("!r") gets no more than
  ;; the program parsing is held to, and the editor carries on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((tuatara.syntax:*syntax-helper-memory* (* 512 1024 1024))
           (editor-before (sb-kernel:dynamic-usage)))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper)))
            (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !r def"))
              (declare (ignore why))
              (when (eq :ready (tuatara.io:sandbox-status-now))
                ;; held to its memory: what it may still take is less than one more step
                (multiple-value-bind (held back left) (tuatara.syntax:helper-memory helper)
                  (declare (ignore held back))
                  (true (and left (<= 0 left (* 64 1024 1024))) "~s left" left)))
              (when tree (tuatara.syntax:delete-tree tree))))))
       (true (< (- (sb-kernel:dynamic-usage) editor-before) (* 256 1024 1024)))
       (true (parses-elsewhere))))))

(define-test (syntax long-read-back-given-up-as-slow)
  ;; a parse reading its text many times over is given no more and given up
  ;; as too slow: no fault of the grammar, its helper left running
  (let ((tuatara.syntax::*read-back-times* 0)
        (tuatara.syntax::*read-back-more* 1000)
        (tuatara.syntax::*given-text-most* 0)
        (grammar (tuatara.syntax:find-grammar "rust")))
    (call-with-own-helper
     (lambda (helper)
       (let ((parser (tuatara.syntax:make-parser grammar helper)))
         (multiple-value-bind (tree why)
             (tuatara.syntax:parse-rope parser (tuatara.text:string->rope
                                                (format nil
                                                        "~{fn f~d() { (\"[<'\") }~%~}"
                                                        (loop for i below 3000 collect i))))
           (false tree)
           (is eq :slow why)
           (true (tuatara.syntax:helper-alive helper))
           (false (tuatara.syntax::grammar-off-p grammar helper))))
       ;; text in another language read many times over (asked for, not sent with the request): the same
       (multiple-value-bind (tree why)
           (tuatara.syntax::parse-rope-ranges grammar (tuatara.text:string->rope
                                                       (format nil "~{fn g~d() {}~%~}"
                                                               (loop for i below 3000 collect i)))
                                              (list (cons 0 30000)) :helper helper)
         (false tree)
         (is eq :slow why (format nil "~s" (tuatara.syntax:helper-ended helper)))
         (true (tuatara.syntax:helper-alive helper)))))))

(define-test (syntax scanner-cannot-fill-memory-files)
  ;; a scanner writing into /dev/shm, memory no limit counts, is ended by
  ;; the limit on files it may write (none): the editor carries on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (when (eq :ready (tuatara.io:sandbox-status))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper)))
            (multiple-value-bind (tree why)
                (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !w/dev/shm/tuatara-shm-probe def"))
              (false tree)
              (is eq :crashed why)
              (false (tuatara.syntax:helper-alive helper))))))
       (true (parses-elsewhere))))))

(define-test (syntax no-helper-unless-named)
  ;; text parsed with no helper named is not parsed, never sent to the
  ;; shared one; Org's source blocks are parsed in their buffer's own
  (let ((tuatara.syntax:*helper* :none))
    (false (tuatara.syntax::current-helper))
    (false (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "rust")))
    (false (tuatara.syntax:language-highlights (tuatara.defaults::injection-resolver :rich 3) "rust"
                                               (list (cons 0 12)) (tuatara.text:string->rope "fn main() {}") 0 12)))
  (is eq :none (symbol-value 'tuatara.syntax:*helper*))
  (let* ((text (format nil "#+begin_src rust~%fn main() {}~%#+end_src~%"))
         (buffer (tuatara.buffer:make-buffer "*org-blocks*" :text text))
         (helpers '()))
    (sb-int:encapsulate 'tuatara.syntax::parse-texts 'which
                        (lambda (function &rest arguments)
                          (push (tuatara.syntax::current-helper) helpers)
                          (apply function arguments)))
    (unwind-protect
         (progn
           ;; its own helper started, then the blocks coloured
           (true (tuatara.syntax:buffer-helper buffer))
           (true (tuatara.defaults::org-src-highlights buffer (tuatara.buffer:buffer-text buffer)
                                                       (list (list "rust" 17 30)) 0 (length text)))
           (true helpers)
           (true (every (lambda (helper) (and helper (eq :own (tuatara.syntax:helper-kind helper))
                                              (eq helper (tuatara.syntax:buffer-helper buffer))))
                        helpers)))
      (sb-int:unencapsulate 'tuatara.syntax::parse-texts 'which)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax scanner-cannot-write-files)
  ;; a scanner writing a file ("!w" and a path) is refused by the sandbox:
  ;; the file is not made, and the parse goes on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (let ((path (uiop:native-namestring (merge-pathnames "written-by-scanner" directory))))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper)))
            (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope (format nil "abc !w~a def" path)))))
              (true tree)
              (when tree (tuatara.syntax:delete-tree tree))))))
       ;; without the sandbox the program is only a process of its own, and may write
       (is eq (not (eq :ready (tuatara.io:sandbox-status-now))) (and (probe-file path) t))))))

(define-test (syntax scanner-writing-past-its-buffer-faults)
  ;; a scanner writing past the buffer for its state, a little or much,
  ;; faults on the page after it before it reaches anything else, ending the
  ;; program parsing; in a buffer the grammar is turned off, and the editor
  ;; carries on, other text parsed by another
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (dolist (text '("abc !o def" "abc !O def"))
       (let ((parser (tuatara.syntax:make-parser grammar (tuatara.syntax:kept-helper :shared))))
         (multiple-value-bind (tree why unusable) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text))
           (false tree)
           (is eq :crashed why "~s" text)
           (true unusable)
           (false (tuatara.syntax:helper-alive (tuatara.syntax::parser-helper parser)))))
       (let ((said nil))
         (unwind-protect
              (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-fault
                                                                 (lambda (syntax) (setf said syntax)))))
                (multiple-value-bind (buffer syntax) (parse-in-buffer grammar text)
                  (unwind-protect
                       (progn
                         (is eq syntax said)
                         (true (tuatara.syntax::grammar-off-p grammar (tuatara.syntax::syntax-helper syntax)) "~s" text)
                         (true (tuatara.syntax:syntax-broken syntax))
                         ;; editing goes on, and another language still parses
                         (tuatara.buffer:buffer-insert buffer 0 "more ")
                         (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json")
                                                                   (tuatara.syntax:kept-helper :shared))))
                           (unwind-protect
                                (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "[1, 2]"))))
                                  (true tree)
                                  (when tree (tuatara.syntax:delete-tree tree)))
                             (tuatara.syntax:delete-parser parser))))
                    (tuatara.syntax:detach-syntax buffer)
                    (tuatara.buffer:kill-buffer buffer))))
           (setf (tuatara.syntax:grammar-faulted grammar) nil)))))))
