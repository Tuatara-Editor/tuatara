;;;; test/cache.lisp --- What the cache keeps to be used again: kept, pinned, cleared once unused, listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test cache
  :description "Entries of the cache are kept after use, found again by their keys, cleared *cache-keep* after
their last use but never while in use or pinned, never outside the cache directory, and listed by M-x cache-manager,
where they are opened, pinned and deleted.")

(defmacro with-cache-kind ((kind directory) &body body)
  "Run BODY with a state directory of its own and KIND, a kind of entry of the
cache made in DIRECTORY of the cache directory, defined; the kind, its
entries and their directory gone after."
  `(with-fresh-state
     (tuatara.defaults::define-cache-kind ,kind ,directory "Things of the tests")
     (unwind-protect (progn ,@body)
       (remhash ,kind tuatara.defaults::*cache-kinds*)
       (ignore-errors (tuatara.io:delete-tree (tuatara.defaults::private-cache-directory ,directory))))))

(defun cache-entry-used-time (directory)
  "When the entry of the cache DIRECTORY was last used, as kept."
  (tuatara.defaults::cache-entry-used (tuatara.defaults::cache-entry-at directory)))

(define-test (cache cleared-after-keep-from-last-use)
  ;; *cache-keep* counts from the moment an entry stops being in use: used past half the time, then let go,
  ;; it lives a whole *cache-keep* from then; in use it is never cleared, however old
  (with-cache-kind ("test" "cache-test/")
    (let* ((tuatara-user:*cache-keep* 100)
           (directory (tuatara.defaults::make-cache-entry "test" "one" "The first" :user :someone))
           (begun (cache-entry-used-time directory)))
      (is equal (list directory "") (multiple-value-list (tuatara.defaults::find-cache-entry "test" "one")))
      (tuatara.defaults::clear-old-cache :now (+ begun 1000))
      (is eq :directory (tuatara.io:path-kind directory :follow nil) "in use: kept, however old")
      (tuatara.defaults::stop-using-cache-directory directory :someone :now (+ begun 60))
      (is = (+ begun 60) (cache-entry-used-time directory) "last used when let go")
      (tuatara.defaults::clear-old-cache :now (+ begun 159))
      (is eq :directory (tuatara.io:path-kind directory :follow nil) "kept a whole *cache-keep* from the close")
      (tuatara.defaults::clear-old-cache :now (+ begun 161))
      (false (tuatara.io:path-kind directory :follow nil) "cleared once unused for *cache-keep*")
      (false (tuatara.defaults::find-cache-entry "test" "one") "and forgotten"))))

(define-test (cache last-use-touched-when-closed)
  ;; a buffer using an entry, killed: the entry is no longer in use, and its last use is the kill
  (with-cache-kind ("test" "cache-test/")
    (let* ((buffer (tuatara.buffer:make-buffer "uses-the-cache"))
           (directory (tuatara.defaults::make-cache-entry "test" "two" "The second" :user buffer)))
      (true (tuatara.defaults::cache-directory-in-use-p directory))
      (let ((before (get-universal-time)))
        (auto-save-session (list (list :until (lambda () (tuatara.buffer:kill-buffer buffer) t))))
        (false (tuatara.defaults::cache-directory-in-use-p directory) "let go with its buffer")
        (true (>= (cache-entry-used-time directory) before) "last used as the buffer was killed")))))

(define-test (cache pinned-entries-kept)
  ;; a pin keeps an entry however long unused, between sessions too; unpinned, it is cleared again
  (with-cache-kind ("test" "cache-test/")
    (let* ((tuatara-user:*cache-keep* 100)
           (directory (tuatara.defaults::make-cache-entry "test" "three" "The third"))
           (later (+ (get-universal-time) 100000)))
      (true (tuatara.defaults::pin-cache-directory directory t))
      (tuatara.defaults::finish-state-writes)
      (true (search (format nil "~cpinned~c" #\Tab #\Tab) (file-text (tuatara.defaults::state-list-path
                                                       tuatara.defaults::*cache-entries*)))
            "the pin kept in the state directory")
      (tuatara.defaults::clear-old-cache :now later)
      (is eq :directory (tuatara.io:path-kind directory :follow nil) "pinned: kept")
      (tuatara.defaults::pin-cache-directory directory nil)
      (tuatara.defaults::clear-old-cache :now later)
      (false (tuatara.io:path-kind directory :follow nil) "unpinned: cleared"))))

(define-test (cache in-use-elsewhere-kept)
  ;; another session using an entry holds its in-use.lock shared: neither clearing nor deleting takes it
  (with-cache-kind ("test" "cache-test/")
    (let* ((tuatara-user:*cache-keep* 0)
           (directory (tuatara.defaults::make-cache-entry "test" "four" "The fourth"))
           (other (tuatara.io:open-lock-file (tuatara.io:path-join directory "in-use.lock") :shared t)))
      (unwind-protect
           (progn
             (tuatara.defaults::clear-old-cache :now (+ (get-universal-time) 1000))
             (is eq :directory (tuatara.io:path-kind directory :follow nil) "in use elsewhere: kept")
             (is equal '(nil "it is in use") (multiple-value-list (tuatara.defaults::clear-cache-path directory))))
        (sb-posix:close other))
      (true (tuatara.defaults::clear-cache-path directory) "let go there, it is deleted")
      (false (tuatara.io:path-kind directory :follow nil)))))

(define-test (cache nothing-outside-the-cache)
  ;; no place kept may lead out of the cache directory, no pin keeps anything outside it, and clearing
  ;; takes a link away, never what it leads to
  (with-cache-kind ("test" "cache-test/")
    (dolist (place '("../outside/" "/etc/" "remote/../../x/" "remote//x/" "remote/x" ""))
      (false (tuatara.defaults::cache-place-p place) "~s refused" place)
      (false (tuatara.defaults::parse-cache-entry-line
              (format nil (format nil "~~{~~a~~^~c~~}" #\Tab) (list place "test" "k" "n" "0" "0" "pinned" "")))
             "~s read as no entry" place))
    (false (tuatara.defaults::pin-cache-directory "/etc/" t))
    (false (tuatara.defaults::clear-cache-path "/etc/"))
    (let* ((directory (tuatara.defaults::private-cache-directory "cache-test/"))
           (target (tuatara.defaults::private-cache-directory "cache-test/target/"))
           (link (concatenate 'string directory "link")))
      (alexandria:write-string-into-file "kept" (concatenate 'string target "file"))
      (sb-posix:symlink (string-right-trim "/" target) link)
      (true (tuatara.defaults::clear-cache-path (concatenate 'string link "/")) "the link taken away")
      (false (tuatara.io:path-kind link :follow nil))
      (is string= "kept" (file-text (concatenate 'string target "file")) "what it led to left"))))

(define-test (cache listed-pinned-and-deleted)
  ;; M-x cache-manager lists an entry under its kind's title; P pins it; d then x deletes it after a yes, pinned
  ;; or not, and one in use is not deleted
  (with-cache-kind ("test" "cache-test/")
    (let* ((tuatara-user:*modal-editing* nil)
           (free (tuatara.defaults::make-cache-entry "test" "free" "A free entry"))
           (busy (tuatara.defaults::make-cache-entry "test" "busy" "A busy entry" :user :someone)))
      (flet ((list-buffer () (tuatara.buffer:find-buffer "*Cache*"))
             (on (name)
               (lambda ()
                 (let* ((list (tuatara.buffer:find-buffer "*Cache*"))
                        (at (and list (search name (tuatara.buffer:buffer-string list)))))
                   (when at
                     (let ((octet (tuatara.text:string-octet-length (subseq (tuatara.buffer:buffer-string list) 0 at))))
                       (setf (tuatara.buffer:buffer-point list) octet
                             (tuatara.display:window-point (tuatara.command::current-window)) octet))
                     t)))))
        (unwind-protect
             (progn
               (auto-save-session (list (format nil "M-x ~a RET" (typed-keys "cache-manager"))
                                        (list :until (on "A free entry"))
                                        "P"
                                        (list :until (lambda () (tuatara.defaults::cache-entry-pinned
                                                                 (tuatara.defaults::cache-entry-at free)))))
                                  :width 160)
               (true (search "Things of the tests" (tuatara.buffer:buffer-string (list-buffer))))
               ;; a session ending lets go of what it used: the busy entry used again in the next
               (auto-save-session (list (list :until (lambda () (tuatara.defaults::use-cache-directory busy :someone)))
                                        (format nil "C-x b ~a RET" (typed-keys "*Cache*")) "r"
                                        (list :until (lambda () (search "in use" (tuatara.buffer:buffer-string
                                                                                  (list-buffer)))))
                                        (list :until (on "A free entry")) "d"
                                        (list :until (on "A busy entry")) "d" "x" (typed-keys "yes") " RET"
                                        (list :until (lambda () (null (tuatara.io:path-kind free :follow nil)))))
                                  :width 160)
               (false (tuatara.io:path-kind free :follow nil) "deleted from the list")
               (is eq :directory (tuatara.io:path-kind busy :follow nil) "the one in use left"))
          (tuatara.defaults::stop-using-cache-directory busy :someone)
          (when (list-buffer) (tuatara.buffer:kill-buffer (list-buffer))))))))

(define-test (cache cleared-entry-not-taken-up)
  ;; a session taking up an entry just as another clears it, which moves it aside under its lock, lets go of
  ;; the lock it took and does not use the entry
  (with-cache-kind ("test" "cache-test/")
    (let* ((directory (tuatara.defaults::make-cache-entry "test" "race" "Raced"))
           (gone (concatenate 'string (string-right-trim "/" directory) "-gone")))
      (sb-int:encapsulate 'tuatara.io:open-lock-file 'cleared-meanwhile
                          (lambda (function path &rest keys)
                            (prog1 (apply function path keys)
                              (rename-file (string-right-trim "/" directory) gone))))
      (unwind-protect
           (false (tuatara.defaults::use-cache-directory directory :someone) "not taken up")
        (sb-int:unencapsulate 'tuatara.io:open-lock-file 'cleared-meanwhile)
        (ignore-errors (tuatara.io:delete-tree gone)))
      (false (gethash directory tuatara.defaults::*cache-uses*)))))

(define-test (cache sweep-carries-the-keep)
  ;; the sweep of old copies on its own thread keeps to the *cache-keep* in force where it was asked
  (with-cache-kind ("test" "cache-test/")
    (let* ((directory (tuatara.defaults::private-cache-directory (format nil "cache-sweep-~36r/" (random (expt 36 6)))))
           (old (concatenate 'string directory "old/"))
           (hour-ago (- (sb-posix:time) 3600)))
      (unwind-protect
           (progn
             (ensure-directories-exist old)
             (sb-posix:utimes (string-right-trim "/" old) hour-ago hour-ago)
             (let ((tuatara-user:*cache-keep* 60))
               (tuatara.defaults::sweep-old-copies directory))
             (true (wait-until (lambda () (null (tuatara.io:path-kind old :follow nil))) 10)
                   "cleared by the keep in force"))
        (remhash directory tuatara.defaults::*old-copies-swept*)
        (ignore-errors (tuatara.io:delete-tree directory))))))

(define-test (cache uses-touched-while-running)
  ;; the entries in use are noted as used now and then, so that a session ending badly leaves a late last use
  (with-cache-kind ("test" "cache-test/")
    (let ((directory (tuatara.defaults::make-cache-entry "test" "long" "Long in use" :user :someone)))
      (unwind-protect
           (progn
             (tuatara.defaults::put-cache-entry
              (tuatara.defaults::changed-cache-entry (tuatara.defaults::cache-entry-at directory) :used 1000))
             (let ((tuatara.defaults::*private-directories-touched* 0))
               (tuatara.defaults::keep-private-directories-fresh nil))
             (true (> (cache-entry-used-time directory) (- (get-universal-time) 60)) "touched"))
        (tuatara.defaults::stop-using-cache-directory directory :someone)))))

(defmacro with-kept-index ((index kept root) &body body)
  "Run BODY with a state directory of its own and INDEX, a directory index of
ROOT, a new directory of five notes, read and kept on disk in KEPT, its
directory of the cache, held in use by nothing; *CACHE-KEEP* 100 seconds and
the entry's last use 1000 seconds before now. Its copy on disk gone after."
  (let ((dir (gensym)))
    `(with-fresh-state
       (with-test-directory (,dir)
         (let* ((tuatara-user:*cache-keep* 100)
                (,root (tuatara.defaults::directory-name ,dir))
                (,index (tuatara.defaults::make-directory-index ,root)))
           (dotimes (i 5) (put-file (merge-pathnames (format nil "n~d.txt" i) ,dir) (format nil "note ~d~%" i)))
           (index-now ,index (tuatara.defaults::read-directory-index ,index :all (* 1024 1024) 64 t))
           (let ((,kept (tuatara.defaults::directory-index-kept-in ,index)))
             (unwind-protect
                  (progn
                    (tuatara.defaults::put-cache-entry
                     (tuatara.defaults::changed-cache-entry (tuatara.defaults::cache-entry-at ,kept)
                                                            :used (- (get-universal-time) 1000)))
                    ,@body)
               (ignore-errors (tuatara.io:delete-tree ,kept)))))))))

(define-test (cache index-held-while-searched)
  ;; a search or a reading holds the index's entry in use: a clear meanwhile leaves it, however old; the
  ;; search notes its last use, and *cache-keep* after that it is cleared like any other entry
  (with-kept-index (index kept root)
    (let ((now (get-universal-time)))
      (true kept "the index kept in the cache, as an entry")
      (is equal root (tuatara.defaults::cache-entry-key (tuatara.defaults::cache-entry-at kept)))
      (tuatara.defaults::call-searching-index
       index (lambda ()
               (tuatara.defaults::clear-old-cache :now (+ now 1000))
               (is eq :directory (tuatara.io:path-kind kept :follow nil) "kept while searched")))
      (false (tuatara.defaults::directory-index-lost index))
      (true (>= (cache-entry-used-time kept) now) "last used as the search ended")
      (sb-int:encapsulate 'tuatara.defaults::save-index 'cleared-meanwhile
                          (lambda (function &rest arguments)
                            (tuatara.defaults::clear-old-cache :now (+ now 1000))
                            (apply function arguments)))
      (unwind-protect
           (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64 t))
        (sb-int:unencapsulate 'tuatara.defaults::save-index 'cleared-meanwhile))
      (is eq :regular (tuatara.io:path-kind (concatenate 'string kept "index") :follow nil) "kept while read")
      (tuatara.defaults::clear-old-cache :now (+ (cache-entry-used-time kept) 99))
      (is eq :directory (tuatara.io:path-kind kept :follow nil) "kept a whole *cache-keep* from its last use")
      (tuatara.defaults::clear-old-cache :now (+ (cache-entry-used-time kept) 101))
      (false (tuatara.io:path-kind kept :follow nil) "cleared once unused for *cache-keep*"))))

(define-test (cache index-cleared-first-written-again)
  ;; cleared before a search takes it: the search reads the pieces mapped still, whole, and the copy on disk is
  ;; written again from them at the next tick, no file read again, one the next session takes up
  (with-kept-index (index kept root)
    (tuatara.defaults::clear-old-cache :now (+ (get-universal-time) 1000))
    (false (tuatara.io:path-kind kept :follow nil) "cleared, unused for long")
    (let ((text (tuatara.defaults::call-searching-index
                 index (lambda ()
                         (format nil "~{~a~}" (map 'list (lambda (corpus)
                                                           (tuatara.text:rope-string
                                                            (tuatara.defaults::file-corpus-rope corpus)))
                                                   (tuatara.defaults::directory-index-corpora index)))))))
      (dotimes (i 5) (true (search (format nil "note ~d" i) text) "searched whole")))
    (true (tuatara.defaults::directory-index-lost index))
    (let ((reads 0)
          (tuatara.defaults::*directory-indexes* (list index))
          (tuatara.defaults::*indexes-made-for* (list tuatara-user:*indexed-directories*))
          (tuatara-user:*indexes-kept-on-disk* t))
      (sb-int:encapsulate 'tuatara.defaults::read-text-file 'count-reads
                          (lambda (f &rest arguments) (incf reads) (apply f arguments)))
      (unwind-protect
           (progn
             (tuatara.defaults::indexes-tick nil)
             (true (wait-until (lambda () (tuatara.defaults::directory-index-result index)) 10))
             (tuatara.defaults::indexes-tick nil))
        (sb-int:unencapsulate 'tuatara.defaults::read-text-file 'count-reads))
      (false (tuatara.defaults::directory-index-lost index))
      (is = 0 reads "nothing read again")
      (true (tuatara.defaults::cache-entry-at kept) "an entry again")
      (let ((next (tuatara.defaults::make-directory-index root)))
        (true (tuatara.defaults::load-index next kept))
        (is = (length (tuatara.defaults::directory-index-pieces index))
            (length (tuatara.defaults::directory-index-pieces next)))))))

(define-test (cache index-held-at-little-cost)
  ;; holding an index for a search looks nothing up again that only changes with the settings: the cache
  ;; directory is not worked out anew, nor the list of the cache written, at each search
  (with-kept-index (index kept root)
    (tuatara.defaults::call-searching-index index (lambda () nil))
    (let ((worked-out 0) (written 0))
      (sb-int:encapsulate 'tuatara.io:cache-directory 'counted
                          (lambda (function &rest arguments) (incf worked-out) (apply function arguments)))
      (sb-int:encapsulate 'tuatara.defaults::put-cache-entry 'counted
                          (lambda (function &rest arguments) (incf written) (apply function arguments)))
      (unwind-protect
           (dotimes (i 10) (tuatara.defaults::call-searching-index index (lambda () nil)))
        (sb-int:unencapsulate 'tuatara.io:cache-directory 'counted)
        (sb-int:unencapsulate 'tuatara.defaults::put-cache-entry 'counted))
      (is = 0 worked-out "the cache directory known")
      (is = 0 written "last use noted once a minute at most"))))

(define-test (cache root-follows-its-setting)
  ;; the cache directory, known between asks, is worked out again once *cache-directory* names another, and
  ;; looked for each time the cache is looked through, so that a drive lost or found is noticed
  (with-test-directory (dir)
    (let ((before (tuatara.defaults::cache-root)) (looked 0))
      (let ((tuatara-user:*cache-directory* (namestring dir)))
        (is string= (tuatara.defaults::directory-name dir) (tuatara.defaults::cache-root)))
      (is string= before (tuatara.defaults::cache-root))
      (sb-int:encapsulate 'tuatara.io:cache-directory 'counted
                          (lambda (function &rest arguments) (incf looked) (apply function arguments)))
      (unwind-protect (with-fresh-state (tuatara.defaults::clear-old-cache))
        (sb-int:unencapsulate 'tuatara.io:cache-directory 'counted))
      (true (plusp looked) "looked for as the cache is looked through"))))

(define-test (cache use-read-with-table-free)
  ;; taking up an entry reads the disk and the list of the cache with the table of uses free: another thread,
  ;; the editor's, never waits on it
  (with-cache-kind ("test" "cache-test/")
    (let ((directory (tuatara.defaults::make-cache-entry "test" "free" "Free")) (free :not-asked))
      (sb-int:encapsulate 'tuatara.defaults::cache-entry-at 'table-free
                          (lambda (function &rest arguments)
                            (let ((other (sb-thread:make-thread
                                          (lambda ()
                                            (sb-ext:with-locked-hash-table (tuatara.defaults::*cache-uses*) t)))))
                              (setf free (sb-thread:join-thread other :timeout 2 :default nil)))
                            (apply function arguments)))
      (unwind-protect (true (tuatara.defaults::use-cache-directory directory :someone))
        (sb-int:unencapsulate 'tuatara.defaults::cache-entry-at 'table-free))
      (is eq t free "the table taken by another thread meanwhile")
      (tuatara.defaults::stop-using-cache-directory directory :someone))))

(define-test (cache index-written-again-once-for-two)
  ;; two sessions keeping the same directory read, its index cleared: the second to write it again takes the
  ;; first one's pieces rather than writing its own and taking the first one's away, and so on by turns
  (with-kept-index (index kept root)
    (let ((other (tuatara.defaults::make-directory-index root)) (written 0))
      (index-now other (tuatara.defaults::read-directory-index other :all (* 1024 1024) 64 t))
      (tuatara.defaults::clear-old-cache :now (+ (get-universal-time) 1000))
      (false (tuatara.io:path-kind kept :follow nil) "cleared")
      (index-now index (tuatara.defaults::read-directory-index index '() (* 1024 1024) 64 t))
      (index-now other (tuatara.defaults::read-directory-index other '() (* 1024 1024) 64 t))
      (true (every (lambda (p) (tuatara.defaults::index-piece-kept-p p kept))
                   (tuatara.defaults::directory-index-pieces index))
            "the first one's pieces left")
      (is equal (map 'list #'tuatara.defaults::index-piece-file (tuatara.defaults::directory-index-pieces index))
          (map 'list #'tuatara.defaults::index-piece-file (tuatara.defaults::directory-index-pieces other))
          "and taken by the second")
      (sb-int:encapsulate 'tuatara.io:write-new-file 'counted
                          (lambda (function &rest arguments) (incf written) (apply function arguments)))
      (unwind-protect
           (index-now index (tuatara.defaults::read-directory-index index '() (* 1024 1024) 64 t))
        (sb-int:unencapsulate 'tuatara.io:write-new-file 'counted))
      (is = 0 written "nothing written again by turns"))))

(define-test (cache kinds-untrusted-unless-said)
  ;; what the entries of a kind hold is untrusted, as files from outside are, unless the kind is defined trusted
  (with-cache-kind ("test" "cache-test/")
    (let* ((tuatara-user:*untrusted-directories* '())
           (directory (tuatara.defaults::make-cache-entry "test" "one" "The first" :user :someone))
           (file (tuatara.io:path-join directory "message.html")))
      (alexandria:write-string-into-file "<p>words</p>" (sb-ext:parse-native-namestring file))
      (true (tuatara:untrusted-path-p file))
      (tuatara:define-cache-kind "test" "cache-test/" "Things of the tests" :untrusted nil)
      (false (tuatara:untrusted-path-p file))
      (tuatara.defaults::stop-using-cache-directory directory :someone))))

(define-test (cache walked-under-their-lock)
  ;; the uses are walked with their table locked: a walk begun while another thread holds the table sees what that
  ;; thread added before letting go, and walks while uses come and go neither fail nor miss one held all along
  (let ((held "/nonexistent-cache-walk/held/")
        (added "/nonexistent-cache-walk/added/")
        (locked (sb-thread:make-semaphore)))
    (setf (gethash held tuatara.defaults::*cache-uses*) (list -1 :walker))
    (unwind-protect
         (let ((holder (sb-thread:make-thread
                        (lambda ()
                          (sb-ext:with-locked-hash-table (tuatara.defaults::*cache-uses*)
                            (sb-thread:signal-semaphore locked)
                            (sleep 0.3)
                            (setf (gethash added tuatara.defaults::*cache-uses*) (list -1 :walker))))
                        :name "cache test holder")))
           (sb-thread:wait-on-semaphore locked)
           (true (member added (tuatara.defaults::cache-directories-used-by :walker) :test #'string=)
                 "the walk waited for the table")
           (sb-thread:join-thread holder)
           (let* ((stop (list nil))
                  (churn (sb-thread:make-thread
                          (lambda ()
                            (loop until (car stop)
                                  do (dotimes (i 50)
                                       (setf (gethash (format nil "/nonexistent-cache-walk/~d/" i)
                                                      tuatara.defaults::*cache-uses*)
                                             (list -1 :churn)))
                                     (dotimes (i 50)
                                       (remhash (format nil "/nonexistent-cache-walk/~d/" i)
                                                tuatara.defaults::*cache-uses*))))
                          :name "cache test churn")))
             (unwind-protect
                  (true (loop repeat 2000
                              always (member held (tuatara.defaults::cache-directories-used-by :walker)
                                             :test #'string=))
                        "the use held is seen by every walk")
               (setf (car stop) t)
               (sb-thread:join-thread churn))))
      (remhash held tuatara.defaults::*cache-uses*)
      (remhash added tuatara.defaults::*cache-uses*))))

(define-test (cache visiting-within-alike)
  ;; whether a buffer visits a file in a directory is asked one way everywhere, the directory written with its
  ;; last slash or without: a file in /x/bc/ is not in /x/b
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (near (tuatara.buffer:make-buffer "visiting-within-near" :file (concatenate 'string root "bc/f.txt")))
           (in (concatenate 'string root "b")))
      (unwind-protect
           (progn
             (false (tuatara.defaults::private-directory-heir nil in))
             (false (tuatara.defaults::cache-directory-in-use-p in))
             (false (tuatara.defaults::buffer-visiting-within in))
             (let ((inside (tuatara.buffer:make-buffer "visiting-within-in" :file (concatenate 'string in "/f.txt"))))
               (unwind-protect
                    (progn
                      (is eq inside (tuatara.defaults::private-directory-heir nil in))
                      (is eq inside (tuatara.defaults::private-directory-heir nil (concatenate 'string in "/")))
                      (true (tuatara.defaults::cache-directory-in-use-p (concatenate 'string in "/")))
                      (false (tuatara.defaults::private-directory-heir inside in) "the buffer given aside"))
                 (tuatara.buffer:kill-buffer inside))))
        (tuatara.buffer:kill-buffer near)))))
