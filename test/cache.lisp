;;;; test/cache.lisp --- What the cache keeps to be used again: kept, pinned, cleared once unused, listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test cache
  :description "Entries of the cache are kept after use, found again by their keys, cleared *cache-keep* after
their last use but never while in use or pinned, never outside the cache directory, and listed by M-x list-cache,
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
  ;; M-x list-cache lists an entry under its kind's title; p pins it; d then x deletes it after a yes, pinned
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
               (auto-save-session (list (format nil "M-x ~a RET" (typed-keys "list-cache"))
                                        (list :until (on "A free entry"))
                                        "p"
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
