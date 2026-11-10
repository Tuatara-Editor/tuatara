;;;; lisp/remote/language-servers.lisp --- A language server of this machine, taken to a trusted server.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x server-bring-language-server copies a language server installed here
;;;; to a trusted server, into a folder of the editor's own there
;;;; (*BROUGHT-LANGUAGE-SERVERS-FOLDER*), nothing fetched from anywhere and
;;;; nothing done as root. Only a program that can run there is sent: read here
;;;; in Lisp, never run, it must be a machine program of the server's kind
;;;; needing no library a server may lack. The file is written beside its name
;;;; and renamed by the SFTP client, its sum compared with one taken there, and
;;;; a list kept beside it, a name and sum each, says what was brought, so that
;;;; M-x server-remove-language-server takes away that and nothing else. All
;;;; that comes back from the server is cleaned, cut short and matched.

(in-package #:tuatara.defaults)

(defparameter *language-server-most-octets* (* 1024 1024 1024)
  "The largest program \\[server-bring-language-server] sends, in octets.")

(defparameter *program-machines*
  '(("x86_64" 62 "/lib64/ld-linux-x86-64.so.2")
    ("aarch64" 183 "/lib/ld-linux-aarch64.so.1")
    ("riscv64" 243 "/lib/ld-linux-riscv64-lp64d.so.1")
    ("ppc64le" 21 "/lib64/ld64.so.2"))
  "The kinds of machine a language server can be brought to: each (NAME CODE
LOADER), NAME as uname -m says it, CODE the machine a program's header names,
LOADER the program a glibc system starts such a program with.")

(defparameter *libraries-every-server-has*
  '("libc.so.6" "libm.so.6" "libdl.so.2" "libpthread.so.0" "librt.so.1" "libutil.so.1" "libgcc_s.so.1")
  "The libraries a program may need and still be brought: glibc's own and
libgcc's, which are on every system with a glibc.")

(defun manifest-path ()
  "The list of what was brought, from the home folder of the server."
  (concatenate 'string tuatara.lsp:*brought-language-servers-folder* ".manifest"))

(defun server-program-path (name)
  "Where the language server NAME, already checked, is kept on the server, from its home folder."
  (concatenate 'string tuatara.lsp:*brought-language-servers-folder* name))

;;; What a program needs

(defun elf-octets (fd count offset size name)
  "COUNT octets of the program NAME, open as FD and SIZE octets long, from
OFFSET; refused when they are not all in it."
  (or (and (<= 0 offset) (<= (+ offset count) size) (tuatara.io:read-fd-exactly fd count offset))
      (refuse "~a is cut short, not a whole program" name)))

(defun elf-string (octets index)
  "The text at INDEX of OCTETS up to its NUL, at most 256 characters, cleaned to be shown; NIL past their end."
  (when (< -1 index (length octets))
    (let* ((limit (min (length octets) (+ index 256)))
           (end (or (position 0 octets :start index :end limit) limit)))
      (escape-controls (tuatara.text:lenient-string (subseq octets index end))))))

(defun elf-versions-wanted (fd start count size name strings)
  "The highest version of glibc wanted by the COUNT lists of versions at
START of the program NAME, open as FD and SIZE octets long, their names in
STRINGS, as a list of numbers, NIL when it wants none."
  (let* ((window (max 0 (min (* 64 1024) (- size start))))
         (needs (elf-octets fd window start size name))
         (highest nil))
    (flet ((field (at bytes)
             (if (<= 0 at (- window bytes)) (tuatara.io:octets-integer needs at bytes) 0)))
      (let ((at 0))
        (loop repeat (min count 256)
              do (tuatara.runtime:job-check-stop)
                 (let ((auxiliary (+ at (field (+ at 8) 4))))
                   (loop repeat (min (field (+ at 2) 2) 256)
                         do (let ((text (elf-string strings (field (+ auxiliary 8) 4))))
                              (when text
                                (let ((version (if (string= text "GLIBC_ABI_DT_RELR")
                                                   '(2 36)
                                                   (tuatara.io:version-numbers text "^GLIBC_"))))
                                  (when (and version (or (null highest) (tuatara.io:version-at-least-p version highest)))
                                    (setf highest version)))))
                            (setf auxiliary (+ auxiliary (field (+ auxiliary 12) 4)))))
                 (let ((next (field (+ at 12) 4)))
                   (when (zerop next) (return))
                   (incf at next)))))
    highest))

(defun elf-program-needs (fd size name)
  "What the machine program NAME, open as FD and SIZE octets long, needs to
run, read from its header and never run, as a plist. :MACHINE is its code in
*PROGRAM-MACHINES*. :LOADER is the program that starts it, or NIL for one that
starts by itself. :LIBRARIES are the names of those it needs. :GLIBC is the
version of glibc it wants as a list of numbers, or NIL. It is refused with
words saying why for anything but a 64-bit little-endian program, and for a
script."
  (let ((head (tuatara.io:read-fd-exactly fd (min size 64) 0)))
    (flet ((u (at bytes) (tuatara.io:octets-integer head at bytes)))
      (cond ((or (null head) (< (length head) 64))
             (refuse "~a is too short to be a program" name))
            ((and (= (aref head 0) 35) (= (aref head 1) 33))
             (refuse "~a is a script that starts another program, not a program that can be copied by itself" name))
            ((not (and (= (aref head 0) #x7F) (= (aref head 1) 69) (= (aref head 2) 76) (= (aref head 3) 70)))
             (refuse "~a is not a machine program" name))
            ((not (and (= (aref head 4) 2) (= (aref head 5) 1) (member (u 16 2) '(2 3))))
             (refuse "~a is not a 64-bit little-endian program" name)))
      (let* ((table (u 32 8)) (entry (u 54 2)) (count (u 56 2))
             (segments
               (progn
                 (unless (and (= entry 56) (<= count 256)) (refuse "~a has a program table not understood" name))
                 (let ((octets (elf-octets fd (* 56 count) table size name)))
                   (loop for at from 0 below (* 56 count) by 56
                         collect (list (tuatara.io:octets-integer octets at 4)
                                       (tuatara.io:octets-integer octets (+ at 8) 8)
                                       (tuatara.io:octets-integer octets (+ at 16) 8)
                                       (tuatara.io:octets-integer octets (+ at 32) 8)))))))
        (flet ((segment (type) (find type segments :key #'first))
               (file-at (address)
                 (loop for (type offset start length) in segments
                       when (and (= type 1) (<= start address) (< address (+ start length)))
                         return (+ offset (- address start)))))
          (let ((loader (let ((interp (segment 3)))
                          (and interp
                               (elf-string (elf-octets fd (min (fourth interp) 256) (second interp) size name) 0))))
                (dynamic (segment 2))
                (needed '()) strtab strsz verneed verneednum)
            (when dynamic
              (when (> (fourth dynamic) (* 16 4096)) (refuse "~a has too large a table of what it needs" name))
              (let ((octets (elf-octets fd (fourth dynamic) (second dynamic) size name)))
                (loop for at from 0 to (- (length octets) 16) by 16
                      for tag = (tuatara.io:octets-integer octets at 8)
                      for value = (tuatara.io:octets-integer octets (+ at 8) 8)
                      until (zerop tag)
                      do (case tag
                           (1 (when (< (length needed) 256) (push value needed)))
                           (5 (setf strtab value))
                           (10 (setf strsz value))
                           (#x6FFFFFFE (setf verneed value))
                           (#x6FFFFFFF (setf verneednum value))))))
            (let* ((strings (and strtab strsz (file-at strtab)
                                 (elf-octets fd (min strsz (* 4 1024 1024)) (file-at strtab) size name)))
                   (libraries (and strings (loop for index in (reverse needed)
                                                 collect (or (elf-string strings index) "?")))))
              (when (and needed (null strings)) (refuse "~a does not say what it needs" name))
              (list :machine (u 18 2) :loader loader :libraries libraries
                    :glibc (and strings verneed verneednum (file-at verneed)
                                (elf-versions-wanted fd (file-at verneed) verneednum size name strings))))))))))

;;; What the server says

(defun server-facts (host)
  "What HOST says it is, asked over ssh: the kind of machine as a known entry
of *PROGRAM-MACHINES*, the number of the account logged in, and its glibc's
version as a list of numbers, NIL when it says none; refused when it cannot
be reached, is a kind not known or says no number for the account."
  (multiple-value-bind (code output said)
      (run-on-server host '("uname -m; id -u; getconf GNU_LIBC_VERSION 2>/dev/null; true") :seconds 30)
    (let ((why (server-unreached-words host code said 30)))
      (when why (refuse "~a" why)))
    (let* ((lines (uiop:split-string output :separator '(#\Newline)))
           (name (first lines))
           (machine (find name *program-machines* :key #'first :test #'string=)))
      (unless machine
        (refuse "~a says it is a ~a, a kind of machine nothing can be brought to" host (server-said-shown (or name ""))))
      (unless (and (second lines) (cl-ppcre:scan "^\\d{1,10}$" (second lines)))
        (refuse "~a says no number for the account logged in" host))
      (values machine
              (parse-integer (second lines))
              (let ((version (third lines)))
                (and version
                     (cl-ppcre:scan "^glibc \\d+\\.\\d+$" version)
                     (tuatara.io:version-numbers version "^glibc ")))))))

(defun refuse-unless-it-fits (needs name host machine glibc)
  "Refuse, saying why, when the program NAME, needing NEEDS (ELF-PROGRAM-NEEDS),
would not run on HOST, a server of the kind MACHINE with the glibc GLIBC."
  (unless (= (getf needs :machine) (second machine))
    (refuse "~a is a program for ~a, and ~a is a ~a machine: it needs a build for ~a" name
            (or (first (find (getf needs :machine) *program-machines* :key #'second)) "another kind of machine")
            host (first machine) (first machine)))
  (let ((loader (getf needs :loader))
        (strange (set-difference (getf needs :libraries) *libraries-every-server-has* :test #'string=)))
    (when strange
      (refuse "~a needs ~{~a~^, ~}, which ~a may not have: it needs a self-contained build" name strange host))
    (when (and (or loader (getf needs :libraries)) (not (equal loader (third machine))))
      (refuse "~a is started by ~a, which ~a may not have: it needs a self-contained build" name
              (or loader "a loader of its own") host))
    (let ((wanted (getf needs :glibc)))
      (when (and loader wanted (not (and glibc (tuatara.io:version-at-least-p glibc wanted))))
        (refuse "~a wants glibc ~{~a~^.~} or later and ~a has ~:[none that says so~;~:*~{~a~^.~}~]" name wanted host glibc)))))

(defun server-manifest (host)
  "What the list kept on HOST says was brought: each (NAME SUM), the lines
that are not a plain name and a sum of 64 hex digits left out; empty when
there is no list."
  (let ((path (manifest-path)))
    (multiple-value-bind (code output said)
        (run-on-server host (list (format nil "cat -- ~a 2>/dev/null || [ ! -e ~a ]" path path)) :seconds 30)
      (let ((why (or (server-unreached-words host code said 30)
                     (and (not (eql code 0)) (format nil "the list of what was brought to ~a cannot be read" host)))))
        (when why (refuse "~a" why)))
      (loop for line in (uiop:split-string output :separator '(#\Newline))
            for count from 1 to 256
            for parts = (uiop:split-string line :separator " ")
            when (and (= (length parts) 2) (tuatara.lsp:language-server-program-name-p (first parts))
                      (= 64 (length (second parts)))
                      (every (lambda (c) (find c "0123456789abcdef")) (second parts)))
              collect parts))))

(defun write-server-manifest (host entries)
  "Keep ENTRIES, (NAME SUM) each, as the list of what was brought to HOST,
read and written by that account alone, its file replaced in one step; the
list taken away when ENTRIES is empty."
  (if (null entries)
      (when (remote-attributes host (manifest-path) :follow nil)
        (remote-change host :remove (manifest-path)))
      (let* ((octets (tuatara.text:string-octets (format nil "~:{~a ~a~%~}" entries)))
             (fd (tuatara.io:memory-file "language-servers manifest" octets :kept nil)))
        (unwind-protect
             (remote-upload host (manifest-path) fd (length octets)
                            :permissions #o600 :like '(:permissions #o600) :expected (constantly nil)
                            :linked-refusal "the list of what was brought is kept under one name")
          (sb-posix:close fd)))))

(defun server-sum (host path)
  "The SHA-256 sum of the file at PATH on HOST as sha256sum there says it, in
lower-case hex, and NIL; else NIL and words saying why there is none: no
answer in time, sha256sum failing, or an answer that is not a sum."
  (unless (plain-path-p path :relative t) (refuse "~a is no plain path" path))
  (multiple-value-bind (code output said) (run-on-server host (list (format nil "sha256sum -- ~a" path)) :seconds 300)
    (let ((why (or (server-unreached-words host code said 300)
                   (and (not (eql code 0)) (format nil "sha256sum on ~a says: ~a" host (server-said-shown said)))
                   (and (not (and (<= 66 (length output))
                                  (every (lambda (c) (find c "0123456789abcdef")) (subseq output 0 64))
                                  (string= output "  " :start1 64 :end1 66)))
                        (format nil "~a answered with something that is not a sum" host)))))
      (if why (values nil why) (values (subseq output 0 64) nil)))))

(defun server-entry-usable-p (attributes uid)
  "True when ATTRIBUTES, of a file or folder there, are as the daemon's
lookup of a brought language server (BROUGHT-LANGUAGE-SERVER) wants them: the
account numbered UID's, and written by no one else."
  (and (eql (getf attributes :uid) uid) (not (logtest (or (getf attributes :permissions) 0) #o022))))

(defun make-server-folder (host uid)
  "Make the folder language servers are kept in on HOST, and those on the way
that are not there, it alone private; refused when something else has its
name, or it is not the account UID's or others may write in it, as the daemon
there would not use what is in it."
  (let ((so-far "") (folder (string-right-trim "/" tuatara.lsp:*brought-language-servers-folder*)))
    (dolist (part (uiop:split-string folder :separator "/"))
      (setf so-far (concatenate 'string so-far part))
      (let ((last (string= so-far folder)))
        (unless (remote-attributes host so-far)
          (remote-change host :mkdir so-far (and last #o700)))
        (when last
          (let ((attributes (remote-attributes host so-far :follow nil)))
            (unless (eq :directory (remote-kind attributes))
              (refuse "~a is not a folder there" (remote-name host so-far)))
            (unless (server-entry-usable-p attributes uid)
              (refuse "~a is another's, or others may write in it: the daemon there would not use what is in it"
                      (remote-name host so-far))))))
      (setf so-far (concatenate 'string so-far "/")))))

(defun refuse-on-evidence (host)
  "Refuse when the folder language servers are kept in on HOST is evidence
(REMOTE-EVIDENCE-WORDS), named from the root there, as it may not exist yet."
  (when (evidence-rules)
    (let ((why (remote-evidence-words host (concatenate 'string (string-right-trim "/" (remote-real-path host "")) "/"
                                                        tuatara.lsp:*brought-language-servers-folder*))))
      (when why (refuse "~a" why)))))

;;; The job

(defstruct (server-program-job (:include tuatara.runtime:job)
                               (:constructor make-server-program-job (buffer host program path))
                               (:copier nil))
  "A language server taken to a trusted server or away from it: HOST, PROGRAM
its name, PATH its full name here, NIL when it is being taken away; DOING
says the step, SIZE the octets to send, SENT, a cons, those sent; UID the
number of the account there; WORDS what is said at the end."
  host program path (doing "Starting") (size 0) (sent (list 0)) uid words)

(setf (documentation 'server-program-job-p 'function) "True when OBJECT is a SERVER-PROGRAM-JOB.")

(tuatara.runtime:define-job-words server-program-job
  :doing (server-program-job-doing job)
  :description (format nil "~:[Taking away~;Bringing~] ~a on ~a" (server-program-job-path job)
                       (server-program-job-program job) (server-program-job-host job))
  :verb "bring it"
  :left-words (format nil "~a stays listed there, what was copied can be taken away" (server-program-job-program job)))

(defmethod tuatara.runtime:job-percent ((job server-program-job))
  "How much of the program has been sent."
  (let ((size (server-program-job-size job)))
    (and (plusp size) (string= (server-program-job-doing job) "Copying")
         (min 99 (floor (* 100 (car (server-program-job-sent job))) size)))))

(defmethod tuatara.runtime:job-take-in ((job server-program-job))
  "Say how the work went."
  (message "~a" (if (eq (tuatara.runtime:job-status job) :done)
                    (server-program-job-words job)
                    (tuatara.runtime:job-ending-words job))))

(defun check-server-file (host path name entries)
  "The sum of the file NAME at PATH on HOST, as SERVER-SUM takes it, when it
is a plain file listed in ENTRIES under that sum; NIL when there is no such
file; refused, saying it is left alone, when it is anything else: what was
not brought is not ours."
  (let ((attributes (remote-attributes host path :follow nil)))
    (when attributes
      (unless (eq :file (remote-kind attributes))
        (refuse "~a is not a plain file on ~a: it is left alone" name host))
      (multiple-value-bind (sum why) (server-sum host path)
        (unless sum (refuse "~a on ~a could not be checked, so it is left alone: ~a" name host why))
        (unless (member sum (remove name entries :key #'first :test-not #'string=) :key #'second :test #'string=)
          (refuse "~a on ~a is not what this command put there: it is left alone" name host))
        sum))))

(defun bring-language-server (job)
  "The work of JOB: send its program, when it fits the server and nothing of
the same name but what was brought is there. It is written in the server's
folder, listed, and renamed into place by the SFTP client. Before that, its
sum is taken there and compared. What does not match, or whose sum cannot be
taken, never takes the name. The list of what was brought is put back."
  (let* ((host (server-program-job-host job))
         (name (server-program-job-program job))
         (local (server-program-job-path job))
         (remote (server-program-path name))
         (*sftp-octets-sent* (server-program-job-sent job))
         (fd (tuatara.io:open-for-reading local nil :accept '(:regular))))
    (unwind-protect
         (let ((size (tuatara.io:stamp-size (tuatara.io:fd-stamp fd local))))
           (when (> size *language-server-most-octets*) (refuse "~a is too large to bring" name))
           (setf (server-program-job-doing job) "Checking")
           (let ((real (tuatara.io:native-file-name (tuatara.io:real-path local))))
             (unless (uiop:string-prefix-p name real)
               (refuse "~a is a link to ~a, a program that starts it: it needs a self-contained build" name real)))
           (let ((needs (elf-program-needs fd size name))
                 (ours (or (fd-sha256 fd local) (refuse "~a could not be read" name))))
             (multiple-value-bind (machine uid glibc) (server-facts host)
               (refuse-unless-it-fits needs name host machine glibc)
               (setf (server-program-job-uid job) uid))
             (refuse-on-evidence host)
             (make-server-folder host (server-program-job-uid job))
             (let* ((entries (server-manifest host))
                    (found (remote-attributes host remote :follow nil))
                    (there (check-server-file host remote name entries))
                    (others (remove name entries :key #'first :test #'string=)))
               (if (equal there ours)
                   (progn
                     (unless (and (server-entry-usable-p found (server-program-job-uid job))
                                  (logtest (or (getf found :permissions) 0) #o100))
                       (refuse "~a is on ~a already, but the daemon there would not use it: another's, not runnable, ~
                                or others may write it" name host))
                     (write-server-manifest host (append others (list (list name ours))))
                     (setf (server-program-job-words job) (format nil "~a is on ~a already" name host)))
                   (let ((placed nil))
                     (setf (server-program-job-doing job) "Copying" (server-program-job-size job) size)
                     (write-server-manifest host (append entries (list (list name ours))))
                     (unwind-protect
                          (progn
                            (remote-upload host remote fd size
                                           :permissions #o755 :like '(:permissions #o755)
                                           :linked-refusal "a brought program is kept under one name"
                                           :expected (lambda (attributes)
                                                       (unless (and (eq (null attributes) (null found))
                                                                    (eql (getf attributes :size) (getf found :size))
                                                                    (eql (getf attributes :mtime) (getf found :mtime)))
                                                         "it changed on the server while it was being brought"))
                                           :vet (lambda (temporary)
                                                  (setf (server-program-job-doing job) "Verifying")
                                                  (multiple-value-bind (sum why) (server-sum host temporary)
                                                    (unless sum
                                                      (refuse "~a was not put in place on ~a, what was there stays: ~a"
                                                              name host why))
                                                    (unless (string= sum ours)
                                                      (refuse "~a was not put in place on ~a, what was there stays: ~
                                                               the copy does not have the sum sent" name host)))))
                            (setf placed t))
                       (unless placed
                         (ignoring-errors-reported ("putting back the list of what was brought to ~a" host)
                           (write-server-manifest host entries))))
                     (write-server-manifest host (append others (list (list name ours))))
                     (setf (server-program-job-words job)
                           (format nil "~a brought to ~a, in ~a" name host tuatara.lsp:*brought-language-servers-folder*)))))))
      (sb-posix:close fd))))

(defun remove-language-server (job)
  "The work of JOB: take away from its server the program it asks for among
those listed there, only when it is the file that was brought, and the
folder too once nothing is left in it."
  (let* ((host (server-program-job-host job))
         (entries (progn (refuse-on-evidence host) (server-manifest host)))
         (names (remove-duplicates (mapcar #'first entries) :test #'string=)))
    (setf (server-program-job-words job) (format nil "Nothing taken away from ~a" host))
    (cond ((null names)
           (setf (server-program-job-words job) (format nil "Nothing was brought to ~a" host)))
          (t
           (let ((name (ask-on-editor (lambda () (completing-read (format nil "Take away from ~a: " host) names
                                                                  :require-match t)))))
             (setf (server-program-job-program job) (or name "nothing"))
             (when (member name names :test #'string=)
               (setf (server-program-job-doing job) "Taking away")
               (let ((path (server-program-path name)))
                 (when (check-server-file host path name entries)
                   (remote-change host :remove path))
                 (let ((left (remove name entries :key #'first :test #'string=)))
                   (write-server-manifest host left)
                   (unless left
                     (handler-case (remote-change host :rmdir (string-right-trim "/" tuatara.lsp:*brought-language-servers-folder*))
                       (sftp-refusal () nil)))))
               (setf (server-program-job-words job) (format nil "~a taken away from ~a" name host))))))))

(defun start-server-program-job (host program path work)
  "Start a job doing WORK for HOST, a trusted server, unless one is going there."
  (cond ((not (state-list-find *trusted-servers* host))
         (message "~a is not a trusted server" host))
        ((find-if (lambda (job) (and (server-program-job-p job) (tuatara.runtime:job-running-p job)
                                     (string= host (server-program-job-host job))))
                  tuatara.runtime:*jobs*)
         (message "A language server is being brought to ~a or taken away already" host))
        (t (tuatara.runtime:start-job-thread (make-server-program-job (current-buffer) host program path) work
                                             :name "tuatara language server to server"))))

(define-command server-bring-language-server ()
  "Copy a language server installed on this machine to the server on the
line of the list of trusted servers, or those marked, chosen in the
minibuffer among those the editor knows that are here: nothing is fetched
from anywhere. It goes in a folder of the editor's own there,
~/.local/share/tuatara/language-servers/, where the daemon there looks after
the PATH, so nothing of yours is hidden and no shell setting is needed. Only
a program for that kind of machine needing no library a server may lack is
sent; a script or a program that only starts another is refused, saying
so. Its SHA-256 sum is checked there, in the background; a file that does
not match is taken away. Nothing is done as root.
\\[server-remove-language-server] takes it away; \\[check-health] on the
daemon shows what was brought."
  (act-on-list (current-buffer)
               (lambda (list hosts)
                 (declare (ignore list))
                 (let ((programs (tuatara.lsp:language-server-programs-here)))
                   (if (null programs)
                       (message "No language server the editor knows is installed on this machine")
                       (let* ((name (completing-read "Bring the language server: " (mapcar #'car programs)
                                                     :require-match t))
                              (found (assoc name programs :test #'string=)))
                         (when found
                           (dolist (host hosts)
                             (start-server-program-job host name (cdr found) #'bring-language-server)))))))
               "server"))

(define-command server-remove-language-server ()
  "Take away a language server \\[server-bring-language-server] brought to the
server on the line of the list of trusted servers, or those marked, chosen
in the minibuffer among those it lists there. Only what is listed there, and
is still the file that was brought, is taken away; anything else of that
name is left, saying so. Over ssh, in the background."
  (act-on-list (current-buffer)
               (lambda (list hosts)
                 (declare (ignore list))
                 (dolist (host hosts)
                   (start-server-program-job host "a language server" nil #'remove-language-server)))
               "server"))
