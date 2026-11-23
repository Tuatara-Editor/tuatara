;;;; test/language-servers-brought.lisp --- A language server taken to a trusted server, and found there.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The server is the stand-in for ssh of remote-files: OpenSSH's sftp-server
;;;; for files, and for a command the shell in the test's directory, the home
;;;; directory there; FAKES put a program of the same name first on the PATH of
;;;; those commands.

(in-package #:tuatara.tests)

(defun little-integer (value bytes)
  "The BYTES octets of VALUE in the order a little-endian program holds it."
  (loop for i below bytes collect (ldb (byte 8 (* 8 i)) value)))

(defun made-program (&key (machine 62) loader libraries glibc)
  "The octets of a 64-bit little-endian program for MACHINE, as its header
says it: started by LOADER, when given, needing LIBRARIES, and wanting the
glibc version GLIBC, text such as \"2.38\". One piece loaded at address 0."
  (let* ((strings (with-output-to-string (s)
                    (write-char #\Nul s)
                    (dolist (name libraries) (write-string name s) (write-char #\Nul s))
                    (when glibc (format s "GLIBC_~a~c" glibc #\Nul))))
         (headers (+ 64 (* 3 56)))
         (loader-octets (and loader (concatenate 'list (tuatara.text:string-octets loader) '(0))))
         (loader-at headers)
         (strings-at (+ loader-at (length loader-octets)))
         (dynamic-at (+ strings-at (length strings)))
         (needs (append (loop with at = 1
                              for name in libraries
                              append (list 1 at) do (incf at (1+ (length name))))
                        (list 5 strings-at 10 (length strings))
                        (and glibc (list #x6FFFFFFE 0 #x6FFFFFFF 1))))
         (dynamic-size (* 8 (+ (length needs) 2)))
         (verneed-at (+ dynamic-at dynamic-size))
         (needs (if glibc
                    (let ((at (position #x6FFFFFFE needs)))
                      (setf (nth (1+ at) needs) verneed-at)
                      needs)
                    needs))
         (glibc-name (and glibc (1+ (reduce #'+ libraries :key (lambda (n) (1+ (length n)))))))
         (verneed (and glibc (append (little-integer 1 2) (little-integer 1 2) (little-integer 0 4)
                                     (little-integer 16 4) (little-integer 0 4)
                                     (little-integer 0 4) (little-integer 0 2) (little-integer 0 2)
                                     (little-integer glibc-name 4) (little-integer 0 4))))
         (total (+ verneed-at (length verneed))))
    (flet ((segment (type offset size)
             (append (little-integer type 4) (little-integer 0 4) (little-integer offset 8)
                     (little-integer offset 8) (little-integer offset 8) (little-integer size 8)
                     (little-integer size 8) (little-integer 0 8))))
      (coerce (append '(#x7F 69 76 70 2 1 1) (make-list 9 :initial-element 0)
                      (little-integer 3 2) (little-integer machine 2) (little-integer 1 4) (little-integer 0 8)
                      (little-integer 64 8) (little-integer 0 8) (little-integer 0 4)
                      (little-integer 64 2) (little-integer 56 2) (little-integer 3 2) (little-integer 64 2)
                      (little-integer 0 2) (little-integer 0 2)
                      (segment 1 0 total)
                      (segment 2 dynamic-at dynamic-size)
                      (segment (if loader 3 0) loader-at (length loader-octets))
                      loader-octets
                      (coerce (tuatara.text:string-octets strings) 'list)
                      (loop for value in needs append (little-integer value 8))
                      (make-list 16 :initial-element 0)
                      verneed)
              'tuatara.text:octets))))

(defun with-program-open (octets function)
  "Call FUNCTION with a descriptor of a new file holding OCTETS and their length."
  (let ((fd (tuatara.io:memory-file "test program" octets :kept nil)))
    (unwind-protect (funcall function fd (length octets))
      (sb-posix:close fd))))

(defun program-needs (octets)
  "What ELF-PROGRAM-NEEDS reads of OCTETS."
  (with-program-open octets (lambda (fd size) (tuatara.defaults::elf-program-needs fd size "rust-analyzer"))))

(define-test (remote-files language-server-program-read-not-run)
  ;; the header and table of needs are read in Lisp; a script, something else, or a table not to be believed is refused
  (let ((needs (program-needs (made-program :loader "/lib64/ld-linux-x86-64.so.2"
                                            :libraries '("libc.so.6" "libgcc_s.so.1") :glibc "2.34"))))
    (is = 62 (getf needs :machine))
    (is string= "/lib64/ld-linux-x86-64.so.2" (getf needs :loader))
    (is equal '("libc.so.6" "libgcc_s.so.1") (getf needs :libraries))
    (is equal '(2 34) (getf needs :glibc)))
  (let ((needs (program-needs (made-program))))
    (false (getf needs :loader))
    (false (getf needs :libraries))
    (false (getf needs :glibc)))
  (fail (program-needs (tuatara.text:string-octets (format nil "#!/bin/sh~%exec node \"$@\"~%")))
      'tuatara.runtime:refusal)
  (fail (program-needs (tuatara.text:string-octets "plain words, long enough to be a header, are not a program at all really"))
      'tuatara.runtime:refusal)
  ;; a table said to be past the end of the file, or too long, reads nothing and is refused
  (let ((whole (made-program :loader "/lib64/ld-linux-x86-64.so.2" :libraries '("libc.so.6"))))
    (let ((bad (copy-seq whole)))
      (setf (subseq bad 32 40) (coerce (little-integer most-positive-fixnum 8) 'vector))
      (fail (program-needs bad) 'tuatara.runtime:refusal))
    (let ((bad (copy-seq whole)))
      (setf (subseq bad 56 58) (coerce (little-integer 60000 2) 'vector))
      (fail (program-needs bad) 'tuatara.runtime:refusal))))

(defun fits-refusal (needs &key (machine '("x86_64" 62 "/lib64/ld-linux-x86-64.so.2")) (glibc '(2 36)))
  "What REFUSE-UNLESS-IT-FITS says of NEEDS, or NIL when it refuses nothing."
  (handler-case (progn (tuatara.defaults::refuse-unless-it-fits needs "gopls" "build.example" machine glibc) nil)
    (tuatara.runtime:refusal (condition) (princ-to-string condition))))

(define-test (remote-files language-server-fits-the-server)
  ;; a program needing what a server may lack is refused, saying what; one needing only glibc's own is sent
  (false (fits-refusal '(:machine 62)))
  (false (fits-refusal '(:machine 62 :loader "/lib64/ld-linux-x86-64.so.2"
                         :libraries ("libc.so.6" "libgcc_s.so.1") :glibc (2 34))))
  (true (search "may not have" (fits-refusal '(:machine 62 :loader "/lib64/ld-linux-x86-64.so.2"
                                               :libraries ("libc.so.6" "libLLVM.so.20"))))
        "a library a server may lack is refused")
  (true (search "libLLVM.so.20" (fits-refusal '(:machine 62 :loader "/lib64/ld-linux-x86-64.so.2"
                                                :libraries ("libc.so.6" "libLLVM.so.20")))))
  (true (search "may not have" (fits-refusal '(:machine 62 :loader "/nix/store/x/ld.so" :libraries ("libc.so.6"))))
        "a loader not the system's is refused")
  (true (search "aarch64" (fits-refusal '(:machine 183))) "another kind of machine is refused")
  (true (search "glibc 2.38" (fits-refusal '(:machine 62 :loader "/lib64/ld-linux-x86-64.so.2" :libraries ("libc.so.6")
                                             :glibc (2 38))))
        "a glibc too old is refused")
  (true (search "none" (fits-refusal '(:machine 62 :loader "/lib64/ld-linux-x86-64.so.2" :libraries ("libc.so.6")
                                       :glibc (2 2))
                                     :glibc nil))
        "a server saying no glibc cannot run one that wants it"))

(defmacro with-server-for-language-servers ((dir log &key fakes) &body body)
  "Run BODY as WITH-STAND-IN-SFTP does, DIR the home directory there and the
commands ssh runs there run by the shell in DIR, the name and text of each of
FAKES put first on their PATH; skipped when there is no sftp-server here."
  `(if (null (sftp-server-program))
       (skip "no sftp-server here" (true nil))
       (with-stand-in-sftp (,dir ,log
                                 :before (format nil "PATH='~afake':\"$PATH\"; cd '~a'; for a do last=\"$a\"; done; ~
                                                           case \"$last\" in sftp) ;; *) exec sh -c \"$last\";; esac"
                                                 (namestring ,dir) (namestring ,dir)))
         (ensure-directories-exist (merge-pathnames "fake/" ,dir))
         (loop for (name text) in ,fakes
               do (put-program (merge-pathnames (concatenate 'string "fake/" name) ,dir) text))
         ,@body)))

(defun brought-file (dir name)
  "The native name of NAME in the directory of brought language servers in the home directory DIR."
  (namestring (merge-pathnames (concatenate 'string tuatara.lsp:*brought-language-servers-directory* name) dir)))

(defun bring-program (host name path)
  "Do the work of bringing PATH, the program NAME, to HOST, on this thread."
  (tuatara.defaults::bring-language-server
   (tuatara.defaults::make-server-program-job nil host name path)))

(defun put-here-program (dir name octets)
  "Put OCTETS as the program NAME in the directory here/ of DIR; its native name."
  (let ((path (merge-pathnames (concatenate 'string "here/" name) dir)))
    (ensure-directories-exist path)
    (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
    (sb-posix:chmod (namestring path) #o755)
    (namestring path)))

(defun bring-refusal (host name path)
  "What bringing PATH, the program NAME, to HOST is refused with, or NIL when it is not."
  (handler-case (progn (bring-program host name path) nil)
    (tuatara.runtime:refusal (c) (princ-to-string c))))

(defun leftover-names (dir)
  "The names of the files in the directory of brought language servers in the home directory DIR."
  (mapcar #'file-namestring (uiop:directory-files (brought-file dir ""))))

(define-test (remote-files language-server-sum-mismatch-never-takes-the-name)
  ;; sha256sum there saying another sum of the copy: nothing takes the name, the copy is gone, the list as it was
  (with-server-for-language-servers (dir log :fakes (list (list "sha256sum" (format nil "#!/bin/sh~%echo ~a'  x'~%"
                                                                                    (make-string 64 :initial-element #\f)))))
    (with-fresh-state
      (let* ((path (put-here-program dir "gopls" (made-program)))
             (words (bring-refusal "build.example" "gopls" path)))
        (true (search "was not put in place on build.example" words))
        (true (search "does not have the sum sent" words))
        (false (probe-file (brought-file dir "gopls")))
        (false (remove ".manifest" (leftover-names dir) :test #'string=) "no copy is left beside it")
        (is equal '() (tuatara.defaults::server-manifest "build.example"))))))

(define-test (remote-files language-server-older-copy-stays-when-the-new-is-bad)
  ;; a listed good copy: a new one whose sum is wrong, or that cannot be summed, leaves the old in place and listed
  (dolist (answer (list (format nil "echo ~a'  x'" (make-string 64 :initial-element #\f)) "exit 1" "exit 255"))
    (with-server-for-language-servers (dir log :fakes (list (list "sha256sum"
                                                                  (format nil "#!/bin/sh~%case \"$2\" in *gopls) exec /usr/bin/sha256sum \"$@\";; esac~%~a~%" answer))))
      (with-fresh-state
        (let* ((old (made-program))
               (new (made-program :machine 62 :libraries '("libc.so.6") :loader "/lib64/ld-linux-x86-64.so.2"))
               (path (put-here-program dir "gopls" old))
               (there (brought-file dir "gopls")))
          (ensure-directories-exist there)
          (sb-posix:chmod (string-right-trim "/" (brought-file dir "")) #o700)
          (alexandria:write-byte-vector-into-file old there)
          (sb-posix:chmod there #o755)
          (put-file (brought-file dir ".manifest")
                    (format nil "gopls ~a~%" (tuatara.defaults::octets-sha256 old)))
          (alexandria:write-byte-vector-into-file new path :if-exists :supersede)
          (true (search "what was there stays" (bring-refusal "build.example" "gopls" path)))
          (is equalp old (alexandria:read-file-into-byte-vector there))
          (is string= (format nil "gopls ~a~%" (tuatara.defaults::octets-sha256 old))
              (uiop:read-file-string (brought-file dir ".manifest")))
          (is equal '("gopls" ".manifest")
              (sort (leftover-names dir) #'string>) "no copy is left beside it"))))))

(define-test (remote-files language-server-sum-answer-malformed)
  ;; sha256sum's answer of exactly 64 or 65 characters is no sum, said in words, nothing signalled
  (dolist (answer (list (make-string 64 :initial-element #\a) (format nil "~a " (make-string 64 :initial-element #\a))))
    (with-server-for-language-servers (dir log
                                           :fakes (list (list "sha256sum"
                                                              (format nil "#!/bin/sh~%printf '%s' '~a'~%" answer))))
      (with-fresh-state
        (multiple-value-bind (sum why) (tuatara.defaults::server-sum "build.example" "x")
          (false sum)
          (true (search "not a sum" why)))))))

(define-test (remote-files language-server-kept-from-evidence)
  ;; a directory of evidence named from the root is told by the first bring, when nothing is there yet, and by removal
  (with-server-for-language-servers (dir log)
    (with-fresh-state
      (let ((path (put-here-program dir "gopls" (made-program))))
        (with-evidence-directories ((list (format nil "/ssh:build.example:~a" (namestring dir))))
          (true (search "evidence" (bring-refusal "build.example" "gopls" path)))
          (false (probe-file (brought-file dir "gopls")))
          (fail (tuatara.defaults::remove-language-server
                 (tuatara.defaults::make-server-program-job nil "build.example" "a language server" nil))
              'tuatara.runtime:refusal))))))

(define-test (remote-files language-server-held-to-what-the-daemon-uses)
  ;; a directory or an "already there" file the daemon would not use is refused and said, never reported brought
  (with-server-for-language-servers (dir log)
    (with-fresh-state
      (let* ((octets (made-program))
             (path (put-here-program dir "gopls" octets))
             (directory (string-right-trim "/" (brought-file dir "")))
             (there (brought-file dir "gopls")))
        (ensure-directories-exist there)
        (sb-posix:chmod directory #o775)
        (true (search "others may write in it" (bring-refusal "build.example" "gopls" path)))
        (sb-posix:chmod directory #o700)
        (alexandria:write-byte-vector-into-file octets there :if-exists :supersede)
        (put-file (brought-file dir ".manifest") (format nil "gopls ~a~%" (tuatara.defaults::octets-sha256 octets)))
        (sb-posix:chmod there #o644)
        (true (search "would not use it" (bring-refusal "build.example" "gopls" path)))
        (sb-posix:chmod there #o755)
        (false (bring-refusal "build.example" "gopls" path)))))
  ;; the same for an account the directory is not owned by
  (with-server-for-language-servers (dir log :fakes (list (list "id" (format nil "#!/bin/sh~%echo 1~%"))))
    (with-fresh-state
      (let ((path (put-here-program dir "gopls" (made-program))))
        (ensure-directories-exist (brought-file dir ""))
        (sb-posix:chmod (string-right-trim "/" (brought-file dir "")) #o700)
        (true (search "another's" (bring-refusal "build.example" "gopls" path)))))))

(define-test (remote-files language-server-name-strings-bounded)
  ;; each name in a crafted table costs a few octets of looking, however long the text without an end
  (let ((octets (make-array (* 4 1024 1024) :element-type '(unsigned-byte 8) :initial-element 65))
        (began (get-internal-run-time)))
    (loop repeat 2000 do (tuatara.defaults::elf-string octets 0))
    (true (< (- (get-internal-run-time) began) (* 2 internal-time-units-per-second)))))

(define-test (remote-files language-server-what-is-not-ours-left-alone)
  ;; a file of that name not listed is refused and untouched; junk in the list counts for nothing; a list saying a
  ;; name with shell characters never names anything
  (with-server-for-language-servers (dir log)
    (with-fresh-state
      (let ((path (put-here-program dir "gopls" (made-program)))
            (theirs (brought-file dir "gopls")))
        (ensure-directories-exist theirs)
        (put-file theirs (format nil "a program of someone else~%"))
        (is string= "gopls on build.example is not what this command put there: it is left alone"
            (handler-case (progn (bring-program "build.example" "gopls" path) "no refusal")
              (tuatara.runtime:refusal (c) (princ-to-string c))))
        (is string= (format nil "a program of someone else~%") (uiop:read-file-string theirs))
        (put-file (brought-file dir ".manifest")
                  (format nil "gopls ~a~%x;rm -rf ~a~%../x ~a~%good ~a~%" (make-string 64 :initial-element #\a)
                          (make-string 64 :initial-element #\b) (make-string 64 :initial-element #\c)
                          (make-string 64 :initial-element #\d)))
        (is equal (list (list "gopls" (make-string 64 :initial-element #\a))
                        (list "good" (make-string 64 :initial-element #\d)))
            (tuatara.defaults::server-manifest "build.example"))))))

(define-test (remote-files language-server-hostile-answers-cleaned)
  ;; a server saying anything for uname is refused without showing what it said raw, and its name goes to no shell
  (with-server-for-language-servers (dir log
                                         :fakes (list (list "uname"
                                                            (format nil
                                                                    "#!/bin/sh~%printf 'x86~c$(reboot)\\n'~%"
                                                                    (code-char 27)))))
    (with-fresh-state
      (let* ((path (put-here-program dir "gopls" (made-program)))
             (words (handler-case (progn (bring-program "build.example" "gopls" path) "no refusal")
                      (tuatara.runtime:refusal (c) (princ-to-string c)))))
        (true (search "a kind of machine nothing can be brought to" words))
        (false (find (code-char 27) words) "what the server said is shown cleaned")
        (true (search "BatchMode=yes" (uiop:read-file-string log)))
        (true (search "ControlPath=none" (uiop:read-file-string log)))
        (false (probe-file (brought-file dir "gopls")))))))

(define-test (remote-files language-server-brought-and-taken-away)
  ;; the whole way: from the list of servers, a language server here is copied there, 0755, listed with its sum and
  ;; found matching; then taken away, the rest left and the directory with it once empty
  (with-server-for-language-servers (dir log)
    (with-fresh-state
      (let* ((octets (made-program :loader "/lib64/ld-linux-x86-64.so.2" :libraries '("libc.so.6") :glibc "2.17"))
             (here (put-here-program dir "gopls" octets))
             (sum (tuatara.defaults::octets-sha256 octets))
             (there (brought-file dir "gopls"))
             (table tuatara.lsp::*known-language-servers*))
        (declare (ignorable here))
        (setf tuatara.lsp::*known-language-servers* (make-hash-table :test 'equal)
              (gethash "gopls" tuatara.lsp::*known-language-servers*) '(:command ("gopls")))
        (unwind-protect
             (with-environment-variable ("PATH" (format nil "~ahere:~a" (namestring dir) (uiop:getenv "PATH")))
               (servers-session (typed-command "server-trust") (typed-keys "build.example") "RET" (list :wait 0.1))
               (servers-session (typed-command "server-list") (list :wait 0.1)
                                (typed-command "server-bring-language-server") (typed-keys "gopls") "RET"
                                (list :until (lambda () (probe-file there)))
                                (list :until (lambda ()
                                               (search (format nil "gopls ~a" sum)
                                                       (handler-case (uiop:read-file-string (brought-file dir
                                                                                                          ".manifest"))
                                                         (error () ""))))))
               (is = #o755 (logand #o777 (sb-posix:stat-mode (sb-posix:stat there))))
               (is = #o700
                   (logand #o777 (sb-posix:stat-mode (sb-posix:stat (string-right-trim "/" (brought-file dir ""))))))
               (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (brought-file dir ".manifest")))))
               (is equalp octets (alexandria:read-file-into-byte-vector there))
               (is string= (format nil "gopls ~a~%" sum) (uiop:read-file-string (brought-file dir ".manifest")))
               ;; another's file in the directory stays when ours is taken away
               (put-file (brought-file dir "taplo") (format nil "not ours~%"))
               (servers-session (typed-command "server-list") (list :wait 0.1)
                                (typed-command "server-remove-language-server")
                                (list :until (asking "Take away"))
                                (typed-keys "gopls") "RET"
                                (list :until (lambda () (not (probe-file there)))))
               (false (probe-file there) "what was brought is taken away")
               (true (probe-file (brought-file dir "taplo")) "what was not brought stays")
               (false (probe-file (brought-file dir ".manifest"))))
          (setf tuatara.lsp::*known-language-servers* table))))))

(define-test (lsp brought-language-server-found-after-the-path)
  ;; a language server brought to this machine is found where the daemon there looks: after the PATH, only
  ;; a plain file of yours in a directory of yours; check-health says brought
  (with-test-directory (dir)
    (with-environment-variable ("HOME" (namestring dir))
      (with-environment-variable ("XDG_DATA_HOME" (namestring (merge-pathnames ".local/share/" dir)))
        (with-environment-variable ("PATH" (format nil "~apath" (namestring dir)))
          (let ((directory (merge-pathnames tuatara.lsp:*brought-language-servers-directory* dir)))
            (ensure-directories-exist directory)
            (sb-posix:chmod (namestring directory) #o700)
            (false (tuatara.lsp::installed-program '("gopls")))
            (let ((there (put-program (merge-pathnames "gopls" directory) (format nil "#!/bin/sh~%"))))
              (sb-posix:chmod there #o755)
              (is string= there (tuatara.lsp::installed-program '("gopls")))
              (is string= there (tuatara.lsp::brought-language-server "gopls"))
              (false (tuatara.lsp::brought-language-server "../gopls") "a name with a slash is no name there")
              (false (tuatara.lsp::brought-language-server "-x"))
              ;; only the first found on the PATH counts: nothing of yours is hidden
              (ensure-directories-exist (merge-pathnames "path/" dir))
              (let ((own (put-program (merge-pathnames "path/gopls" dir) (format nil "#!/bin/sh~%"))))
                (sb-posix:chmod own #o755)
                (is string= own (tuatara.lsp::installed-program '("gopls")))
                (delete-file own))
              ;; writable by others, or a link, it is not used
              (sb-posix:chmod there #o775)
              (false (tuatara.lsp::brought-language-server "gopls"))
              (sb-posix:chmod there #o755)
              (true (tuatara.lsp::brought-language-server "gopls"))
              (let ((tuatara.lsp::*language-servers* '((go ("gopls")))))
                (true (search "gopls ✓ brought" (nth-value 1 (tuatara.lsp::language-tools-words 'go))))))))))))

(define-test (lsp brought-language-servers-in-the-data-home)
  ;; on this machine the servers brought are looked for in the data home, wherever XDG_DATA_HOME puts it
  (with-test-directory (dir)
    (with-environment-variable ("HOME" (namestring (merge-pathnames "home/" dir)))
      (with-environment-variable ("XDG_DATA_HOME" (namestring (merge-pathnames "data/" dir)))
        (with-environment-variable ("PATH" (format nil "~apath" (namestring dir)))
          (let ((directory (merge-pathnames "data/tuatara/language-servers/" dir)))
            (ensure-directories-exist directory)
            (sb-posix:chmod (namestring directory) #o700)
            (let ((there (put-program (merge-pathnames "gopls" directory) (format nil "#!/bin/sh~%"))))
              (sb-posix:chmod there #o755)
              (is string= there (tuatara.lsp::installed-program '("gopls"))))))))))
