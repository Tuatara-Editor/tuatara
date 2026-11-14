;;;; lisp/gpg/encrypted.lisp --- Files encrypted with GnuPG (.gpg): opened decrypted, saved encrypted, their text
;;;; never on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file whose name ends in .gpg opens decrypted: gpg (GnuPG) reads it from
;;;; a pipe and gives the text back through one, straight into memory, in the
;;;; background. A save sends the text through gpg first, encrypted to the keys
;;;; the file was encrypted to (or with a passphrase, as it was), so that only
;;;; the encrypted octets touch the disk; a new .gpg file asks whom to encrypt
;;;; it to in the minibuffer, keys or a passphrase, several at once, commas
;;;; between them. The passphrase is asked for by gpg-agent's own
;;;; pinentry, which keeps it out of the editor: its window in a desktop
;;;; session; in a terminal without one, its prompt in the terminal, lent to
;;;; it while gpg runs. Only where neither can ask, or when you say so, the
;;;; minibuffer asks, shown as stars and kept no longer than it takes to hand
;;;; it to gpg (*GPG-PASSPHRASE-FROM*).
;;;;
;;;; The decrypted text is kept from the disk and from other programs: no
;;;; auto-saved copy, no session file, no scratch file of a large sort or
;;;; replacement (refused past memory), no git gutter, no system clipboard but
;;;; on request (M-x gpg-copy-to-clipboard), no core dump of the editor while one is
;;;; open; an export asks first, since what it writes is not encrypted. A file
;;;; opened safely is never decrypted: evidence stays its encrypted octets.

(in-package #:tuatara.defaults)

(define-setting *gpg-program* "gpg"
  "The GnuPG program that decrypts and encrypts .gpg files.
A name is looked for only in the system's own directories of programs
(/usr/local/bin, /usr/bin, /bin), never in a directory of yours your PATH may
name, nor in the directory the editor is in.
- a full path from the root: /opt/gnupg/bin/gpg for example.
- a name: gpg as standard."
  :values file)

(define-setting *gpg-passphrase-from* :auto
  "Who asks for a passphrase gpg needs.
Pinentry keeps the passphrase out of the editor.
- :agent: gpg-agent's pinentry in its own window.
- :terminal: gpg-agent's pinentry in the terminal the editor shows in, lent to
  it while gpg runs.
- :minibuffer: the minibuffer, what is typed shown as stars (gpg's loopback,
  the passphrase then passes through the editor).
- :auto: the agent's window in a desktop session, else the terminal, else the
  minibuffer."
  :values (member :agent :terminal :minibuffer :auto))

(define-setting *gpg-loopback-warning* t
  "Whether the minibuffer asking for a passphrase for gpg's loopback shows a
window above it, while it asks.
The window says why it asks, how the passphrase goes and what that risks (the
garbage collector's copies, memory not locked, no keyboard grab, a look-alike).
- t: the window is shown.
- nil: the question alone, once you know them."
  :values boolean)

(defun gpg-path ()
  "The gpg program found (*GPG-PROGRAM*), as TUATARA.IO:SYSTEM-PROGRAM finds
it; NIL when there is none."
  (tuatara.io:system-program *gpg-program*))

(defun gpg-terminal ()
  "The terminal device the editor shows in, which gpg-agent's pinentry can be lent; NIL in a window of its own."
  (and *editor* (tuatara.frontend:frontend-terminal-device (editor-frontend *editor*))))

(defun gpg-asker ()
  "Who asks for passphrases, as *GPG-PASSPHRASE-FROM* says: :AGENT, :TERMINAL or :MINIBUFFER.
In the frame of a client on another machine, gpg-agent alone, whatever it
says: a passphrase is never typed into an editor on a server."
  (let ((from (let ((client (current-client)))
                (if (and client (client-far-p client)) :agent *gpg-passphrase-from*))))
    (case from
      ((:agent :minibuffer) from)
      (:terminal (if (gpg-terminal) :terminal :minibuffer))
      (t (cond ((tuatara.io:desktop-p) :agent)
               ((gpg-terminal) :terminal)
               (t :minibuffer))))))

(defun gpg-minibuffer-p ()
  "True when the minibuffer, not gpg-agent's pinentry, asks for passphrases (GPG-ASKER)."
  (eq (gpg-asker) :minibuffer))

(defun loopback-warning ()
  "The warning shown above the minibuffer while it asks for a passphrase for
gpg's loopback: why it asks, how the passphrase goes, and what that risks."
  (format nil "Warning: this passphrase goes through Tuatara, not gpg-agent's pinentry.
Why: ~a.
How: gpg's loopback mode. Tuatara holds what you type as text, hands it to gpg
through a pipe of its own (never a file or an argument), then overwrites it;
it is kept nowhere and goes to no log. The risks pinentry does not have:
 - the garbage collector may have copied it in memory before it is overwritten,
   and a copy stays until that memory is used again;
 - that memory is not locked: it can reach swap on disk, or a capture of memory;
 - typing has no keyboard grab: a program watching the keyboard could see it;
 - a program that only looks like Tuatara could ask the same way.
Pinentry keeps it out of Tuatara: *gpg-passphrase-from* :auto uses gpg-agent's
window on a desktop, or its prompt in the terminal Tuatara runs in. C-g: nothing.
Off: (setf *gpg-loopback-warning* nil).~%"
          (if (eq *gpg-passphrase-from* :minibuffer)
              "you chose it (*gpg-passphrase-from* :minibuffer)"
              "no desktop for gpg-agent's window, and no terminal Tuatara can lend its prompt")))

(defun gpg-read-passphrase (prompt &key confirm)
  "Ask for a passphrase gpg needs in the minibuffer (READ-PASSWORD, PROMPT and
CONFIRM as it takes them), the risks of gpg's loopback said in a window above
it (LOOPBACK-WARNING) while it asks, unless *GPG-LOOPBACK-WARNING* is NIL."
  (if (not *gpg-loopback-warning*)
      (read-password prompt :confirm confirm)
      (let* ((text (loopback-warning))
             (end (position #\Newline text)))
        (call-with-details-shown (list (cons (subseq text 0 end) :warning) (subseq text end))
                                 (lambda () (read-password prompt :confirm confirm))
                                 :name "*Passphrase warning*"))))

(defun with-gpg-terminal (words function)
  "Call FUNCTION, gpg run in it, with the terminal lent to gpg-agent's pinentry
when it asks there (GPG-ASKER :TERMINAL), WORDS shown on the terminal
meanwhile; else just call it."
  (if (eq (gpg-asker) :terminal)
      (tuatara.frontend:frontend-lend-terminal
       (editor-frontend *editor*) function
       :words (format nil "Tuatara: ~a; gpg-agent asks here for a passphrase it needs." words))
      (funcall function)))

;;; Running gpg

(defparameter *gpg-variables*
  '("GNUPGHOME" "TERM" "GPG_TTY" "DISPLAY" "WAYLAND_DISPLAY" "XAUTHORITY" "DBUS_SESSION_BUS_ADDRESS"
    "XDG_RUNTIME_DIR" "PINENTRY_USER_DATA")
  "The environment variables gpg is given, of those programs started from the
editor have, besides TUATARA.IO:*RESTRICTED-VARIABLES*: where your keyring
is, and where gpg-agent and its pinentry find your terminal or desktop. Its
PATH is the system's own directories of programs, and nothing else reaches it.")

(defparameter *gpg-staying-home-options*
  '("--disable-dirmngr" "--no-auto-key-retrieve" "--no-auto-key-locate" "--no-auto-key-import"
    "--list-options" "no-show-photos" "--verify-options" "no-show-photos")
  "The options every gpg run is given, after those of your gpg.conf, so that
whatever that or a file asks, gpg fetches no key over the network, takes
none into your keyring from a signed file, and starts no program to show the
photo in a key. Those of *GPG-NEWER-OPTIONS* only a gpg that knows them is
given.")

(defparameter *gpg-newer-options* '("--no-auto-key-import")
  "The options of *GPG-STAYING-HOME-OPTIONS* that older gpg versions (2.2.27
and before) do not know and refuse to run with: a gpg is given them only
when it lists them (GPG-OPTIONS).")

(sb-ext:defglobal **gpg-options** nil
  "The options the gpg last asked knows (GPG-OPTIONS): (PATH . OPTIONS), or NIL before.")

(defun gpg-options (program environment)
  "The options the gpg program at PROGRAM knows, as its --dump-options lists
them, run with ENVIRONMENT (GPG-ENVIRONMENT); asked of it once for each
program, in a few milliseconds, and kept. :UNKNOWN when it gives no answer
within 5 seconds, asked again the next time."
  (let ((known **gpg-options**))
    (if (equal program (car known))
        (cdr known)
        (multiple-value-bind (text ended-well)
            (tuatara.io:program-output (list program "--dump-options") :seconds 5 :environment environment)
          (if ended-well
              (cdr (setf **gpg-options** (cons program (uiop:split-string text :separator '(#\Newline)))))
              :unknown)))))

(defun forget-gpg-options ()
  "Forget the options gpg was found to know (GPG-OPTIONS), as a saved image is
made: the gpg where the image runs may be another."
  (setf **gpg-options** nil))

(pushnew 'forget-gpg-options sb-ext:*save-hooks*)

(defun gpg-staying-home-options (program environment)
  "The options of *GPG-STAYING-HOME-OPTIONS* the gpg program at PROGRAM is
given, run with ENVIRONMENT: those of *GPG-NEWER-OPTIONS* left out when it
says it does not know them (GPG-OPTIONS), kept when it gives no answer."
  (let ((known (gpg-options program environment)))
    (if (eq known :unknown)
        *gpg-staying-home-options*
        (remove-if (lambda (option)
                     (and (member option *gpg-newer-options* :test #'string=)
                          (not (member option known :test #'string=))))
                   *gpg-staying-home-options*))))

(defun gpg-home-refusal ()
  "Why the directory gpg takes its keys and options from is not safe, in words, or
NIL when it is. GNUPGHOME, or with none ~/.gnupg (your home directory while that is
not made yet), must be a directory of yours that only you may write in. It must be
no link, named from the root, every directory on the way to it root's or yours and
writable by others only with the sticky bit (DIRECTORY-REFUSAL). gpg reads its
options there, and options can have it run programs."
  (let ((uid (sb-posix:geteuid))
        (home (tuatara.io:program-variable "GNUPGHOME")))
    (if (and home (plusp (length home)))
        (directory-refusal home (format nil "GNUPGHOME (~a)" home) uid :private nil)
        (let* ((yours (or (tuatara.io:program-variable "HOME") (sb-posix:passwd-dir (sb-posix:getpwuid uid))))
               (default (tuatara.io:path-join yours ".gnupg")))
          (if (tuatara.io:path-stamp default :follow nil :failure nil)
              (directory-refusal default default uid :private nil)
              (directory-refusal yours (format nil "your home directory (~a)" yours) uid :private nil))))))

(defun gpg-environment (asker)
  "The environment gpg runs with, as NAME=VALUE strings: the variables of
*GPG-VARIABLES* programs started from the editor have, PATH the system's own
directories of programs (TUATARA.IO:*SANDBOX-PATH*), and with ASKER :TERMINAL,
GPG_TTY the terminal lent to gpg-agent's pinentry. Refused, saying why, when
the directory gpg takes its keys and options from is not safe (GPG-HOME-REFUSAL)."
  (let ((device (and (eq asker :terminal) (gpg-terminal)))
        (why (gpg-home-refusal)))
    (when why
      (refuse "gpg is not run: the directory gpg takes its keys and options from is not safe: ~a" why))
    (apply #'tuatara.io:restricted-environment *gpg-variables* (format nil "PATH=~a" tuatara.io:*sandbox-path*)
           (and device (list (format nil "GPG_TTY=~a" device))))))

(defparameter *gpg-least-output-limit* (* 64 1024 1024)
  "The most octets gpg may write however little it is given (GPG-MOST-OUTPUT).")

(defun gpg-most-output (size)
  "The most octets gpg may write for an input of SIZE octets: 32 times SIZE,
at least *GPG-LEAST-OUTPUT-LIMIT*; a file that would decompress to more is
refused."
  (max *gpg-least-output-limit* (* 32 size)))

(defun gpg-feed (fd octets path)
  "Write to file descriptor (FD), a pipe gpg reads, OCTETS, or the file at
PATH a block at a time (FILE-SOURCE), never read whole into memory."
  (if path
      (tuatara.io:call-with-file path
                                 (lambda (in name size)
                                   (declare (ignore size))
                                   (let ((source (file-source in name))
                                         (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
                                     (unwind-protect
                                          (loop for n = (funcall source buffer)
                                                while (plusp n)
                                                do (tuatara.io:write-octets fd buffer :end n :path "gpg"))
                                       (close-source source)))))
      (tuatara.io:write-octets fd octets :path "gpg")))

(defun gpg-passphrase-pipe (passphrase)
  "A pipe holding PASSPHRASE and a line end, for gpg to read whole: its end to
read from, as a list (FD NIL) as GPG-RUN keeps its pipes. The passphrase is
written as octets on the pipe itself, and they are wiped."
  (multiple-value-bind (in out) (sb-posix:pipe)
    (let ((octets (tuatara.text:string-octets passphrase)))
      (unwind-protect
           (progn (tuatara.io:write-octets out octets :path "gpg")
                  (tuatara.io:write-octets out (load-time-value (tuatara.text:string-octets (string #\Newline)))
                                           :path "gpg"))
        (fill octets 0)
        (sb-posix:close out)))
    (list in nil)))

(defun gpg-secret-thread (name function)
  "A thread called NAME calling FUNCTION, nothing of a secret it handles
written to the debug log should it fail (*SECRET-AT-HAND*): its value is
FUNCTION's."
  (tuatara.runtime:make-guarded-thread (lambda () (let ((tuatara.runtime:*secret-at-hand* t)) (funcall function)))
                                       :name name))

(defun gpg-line-reader (name stream most count keep)
  "A thread called NAME reading the lines of STREAM to its end, each cut to
MOST characters, its value those of the first COUNT that KEEP, a function of
a line, gives back (NIL leaving it out), as it gives them, in order. STREAM is
closed at its end."
  (gpg-secret-thread name
                     (lambda ()
                       (let ((lines '()))
                         (unwind-protect
                              (ignoring-errors-reported ("reading what gpg gives")
                                (loop for line = (tuatara.io:read-line-bounded stream most)
                                      for n from 0
                                      while line
                                      do (let ((kept (and (< n count) (funcall keep line))))
                                           (when kept (push kept lines)))))
                           (ignoring-errors-reported ("closing a pipe from gpg") (close stream)))
                         (nreverse lines)))))

(defun gpg-command (program environment arguments asker &key status most passphrase second)
  "The command running gpg, PROGRAM, with ARGUMENTS as GPG-RUN takes them and
ENVIRONMENT, ASKER (GPG-ASKER) asking for passphrases: its status lines to
the descriptor STATUS, no more output than MOST, its passphrase read from
the descriptor PASSPHRASE, and a second input from the descriptor SECOND,
when given."
  (list* program
         (append (gpg-staying-home-options program environment)
                 (list "--yes" "--status-fd" (princ-to-string status) "--max-output" (princ-to-string most))
                 (and (not (eq asker :terminal)) (list "--no-tty"))
                 (and (or (eq asker :minibuffer) passphrase) (list "--pinentry-mode" "loopback"))
                 (and passphrase (list "--batch" "--passphrase-fd" (princ-to-string passphrase)))
                 (and (eq asker :minibuffer) (not passphrase) (list "--batch"))
                 (and second (list "--enable-special-filenames"))
                 (substitute (and second (format nil "-&~d" second)) :second-input arguments))))

(defun gpg-run (arguments &key input input-path passphrase cancel seconds (asker (gpg-asker)) second-input second-path)
  "Run gpg with ARGUMENTS, never through a shell, giving it INPUT (octets),
or the file at INPUT-PATH a block at a time, through a pipe and taking what
it writes the same way, so that nothing it reads or writes touches the disk;
PASSPHRASE, when given, through a pipe of its own, which gpg's loopback then
takes in place of any pinentry. CANCEL, a cons, stops it when its car turns
true; SECONDS, when given, is the most it may take, then it is stopped and
the run refused (a keyring locked, an agent that does not answer). ASKER
\(GPG-ASKER) says how a passphrase gpg needs is asked for: by the minibuffer
\(loopback, PASSPHRASE given), by gpg-agent in the terminal lent it (call it
within WITH-GPG-TERMINAL), or by its window.
SECOND-INPUT, more octets (a detached signature), or the file at
SECOND-PATH, reach gpg through a pipe of their own, named in ARGUMENTS by
:SECOND-INPUT (made -&N, gpg's name for a descriptor). Its status lines come
through a pipe of their own too, apart from what it says, which a file could
put words in. More output than GPG-MOST-OUTPUT stops it. Four values: the
octets it wrote, its status lines (\"ENC_TO ...\"), true when it ended well,
and what it said besides. An error meanwhile writes no passphrase or text
to the debug log (*SECRET-AT-HAND*). It runs with an environment of its own
\(GPG-ENVIRONMENT) and fetches nothing over the network
\(*GPG-STAYING-HOME-OPTIONS*)."
  (let* ((tuatara.runtime:*secret-at-hand* t)
         (program (or (gpg-path) (tuatara.io:program-missing "gpg" :why (outside-program-install-words :gpg))))
         (environment (gpg-environment asker))
         (most (gpg-most-output (if input-path (tuatara.io:path-size input-path) (length input))))
         (pipe (and passphrase (gpg-passphrase-pipe passphrase)))
         (second (and (or second-input second-path) (multiple-value-list (sb-posix:pipe))))
         (status (multiple-value-list (sb-posix:pipe)))
         (process nil) (statuses '()) (said '()) (output nil) (too-much nil) (timed-out nil)
         (timer (and seconds
                     (sb-ext:make-timer (lambda ()
                                          (when (and process (sb-ext:process-alive-p process))
                                            (setf timed-out t)
                                            (ignoring-errors-reported ("ending gpg") (sb-ext:process-kill process 9))))
                                        :name "tuatara gpg deadline" :thread t))))
    (unwind-protect
         (tuatara.io:call-with-program
          (gpg-command program environment arguments asker :status (second status) :most most
                                                           :passphrase (first pipe) :second (first second))
          (lambda (started)
            (setf process started)
            (when timer (sb-ext:schedule-timer timer seconds))
            (sb-posix:close (shiftf (second status) nil))
            (when pipe (sb-posix:close (shiftf (first pipe) nil)))
            (when second
              ;; the second input written on a thread of its own, gpg reading it as it goes
              (sb-posix:close (shiftf (first second) nil))
              (let ((fd (shiftf (second second) nil)))
                (gpg-secret-thread "tuatara gpg second input"
                                   (lambda ()
                                     (unwind-protect (ignoring-errors-reported ("feeding gpg its second input")
                                                       (gpg-feed fd second-input second-path))
                                       (ignoring-errors-reported ("closing gpg's second input") (sb-posix:close fd)))))))
            (let ((writer (and (or input input-path)
                               (gpg-secret-thread "tuatara gpg input"
                                                  (lambda ()
                                                    (ignoring-errors-reported ("feeding gpg its input")
                                                      (gpg-feed (sb-sys:fd-stream-fd (sb-ext:process-input process))
                                                                input input-path))
                                                    (ignoring-errors-reported ("closing gpg's input")
                                                      (close (sb-ext:process-input process)))))))
                  (reader (gpg-line-reader "tuatara gpg status"
                                           (sb-sys:make-fd-stream (shiftf (first status) nil)
                                                                  :input t :external-format :latin-1 :buffering :full)
                                           4096 10000
                                           (lambda (line)
                                             (and (uiop:string-prefix-p "[GNUPG:] " line) (subseq line 9)))))
                  (listener (gpg-line-reader "tuatara gpg messages" (sb-ext:process-error process) 1024 200
                                             #'identity))
                  (cancelled nil))
              ;; what was decrypted is kept only in OUTPUT
              (setf (values output too-much cancelled)
                    (tuatara.io:read-stream-octets (sb-ext:process-output process) :limit most :cancel cancel :wipe t))
              (when (or too-much cancelled) (sb-ext:process-kill process 15))
              (unless output (setf output (make-array 0 :element-type '(unsigned-byte 8))))
              (when writer (sb-thread:join-thread writer :default nil))
              (setf statuses (sb-thread:join-thread reader :default nil)
                    said (sb-thread:join-thread listener :default nil))
              (sb-ext:process-wait process)
              (when too-much
                (setf said (append said (list (format nil "it gave more than ~a, far more than this holds: stopped"
                                                      (octets-words most))))))))
          :input (if (or input input-path) :stream nil) :output :stream :error :stream :external-format :latin-1
          :preserve-fds (append (list (second status)) (and pipe (list (first pipe))) (and second (list (first second))))
          :environment environment)
      (when timer (sb-ext:unschedule-timer timer))
      (dolist (fd (append pipe second status))
        (when fd (ignoring-errors-reported ("closing a pipe to gpg") (sb-posix:close fd)))))
    (when timed-out (refuse "gpg gave no answer within ~d second~:p: stopped" seconds))
    (values output statuses
            (and process (not too-much) (eql 0 (sb-ext:process-exit-code process)) (not (and cancel (car cancel))))
            (format nil "~{~a~^; ~}" said))))

(defun gpg-status-p (statuses name)
  "True when STATUSES, gpg's status lines, have one called NAME."
  (find-if (lambda (s) (or (string= s name) (uiop:string-prefix-p (concatenate 'string name " ") s))) statuses))

(defun gpg-recipient-words (status)
  "Why gpg would not encrypt to a key, as words, from STATUS, its INV_RECP
line (a reason code and the key): with the gpg command that puts it right
where there is one."
  (destructuring-bind (&optional reason key &rest more)
      (rest (uiop:split-string status :separator '(#\Space)))
    (declare (ignore more))
    (let ((code (and reason (parse-integer reason :junk-allowed t))))
      (case code
        (1 (format nil "there is no key ~a among your keys" key))
        (4 (format nil "key ~a is revoked" key))
        (5 (format nil "key ~a has expired" key))
        (10 (format nil "gpg does not count key ~a as valid yet; gpg --check-trustdb counts its trust again, ~
and gpg --edit-key ~a trust sets it" key key))
        (t (format nil "gpg would not encrypt to key ~a (reason ~a)" key reason))))))

(defun gpg-failure-words (statuses said)
  "Why gpg failed, as words, from its STATUSES and what it SAID."
  (cond ((some (lambda (s) (search "BAD_PASSPHRASE" s)) statuses) "the passphrase was not right")
        ((gpg-status-p statuses "INV_RECP") (gpg-recipient-words (gpg-status-p statuses "INV_RECP")))
        ((or (gpg-status-p statuses "MISSING_PASSPHRASE") (gpg-status-p statuses "NO_PASSPHRASE")) "no passphrase was given")
        ((gpg-status-p statuses "NO_SECKEY") "none of the keys it is encrypted to is yours")
        ((gpg-status-p statuses "NODATA") "it holds no encrypted data")
        ((plusp (length said)) said)
        (t "gpg failed")))

;;; Decrypting

(defun gpg-decrypt (octets &key passphrase cancel)
  "OCTETS, an encrypted file's, decrypted by gpg, as octets; as a second value
a plist of what is known of how: :KEYS, the IDs of the keys it is encrypted
to; :SYMMETRIC, true when a passphrase opens it; :NEEDS-PASSPHRASE, true when
it failed for want of one; :ERROR, why it failed (NIL when it did not)."
  (let ((tuatara.runtime:*secret-at-hand* t))
    (multiple-value-bind (output statuses ok said)
        (gpg-run (list "--decrypt" "--output" "-") :input octets :passphrase passphrase :cancel cancel)
      (let ((keys (loop for s in statuses
                        when (uiop:string-prefix-p "ENC_TO " s)
                          collect (second (uiop:split-string s :separator '(#\Space)))))
            (symmetric (and (gpg-status-p statuses "NEED_PASSPHRASE_SYM") t))
            (decrypted (and ok (gpg-status-p statuses "DECRYPTION_OKAY"))))
        (values (and decrypted output)
                (list :keys (remove-duplicates keys :test #'string=) :symmetric symmetric
                      :needs-passphrase (and (not decrypted) (not passphrase)
                                             (or (gpg-status-p statuses "MISSING_PASSPHRASE")
                                                 (gpg-status-p statuses "NO_PASSPHRASE")
                                                 (gpg-status-p statuses "NEED_PASSPHRASE_SYM")
                                                 (gpg-status-p statuses "NEED_PASSPHRASE"))
                                             t)
                      :error (and (not decrypted) (gpg-failure-words statuses said))))))))

(defun decrypt-naming-keys (octets &key passphrase cancel)
  "OCTETS decrypted as GPG-DECRYPT does, its plist of how with :KEY-WORDS
besides, the keys it is encrypted to as words (ENCRYPTION-KEY-WORDS), looked
up once it is decrypted."
  (let ((tuatara.runtime:*secret-at-hand* t))
    (multiple-value-bind (text info) (gpg-decrypt octets :passphrase passphrase :cancel cancel)
      (values text (if (and text (getf info :keys))
                       (list* :key-words (encryption-key-words (getf info :keys)) info)
                       info)))))

(defun openpgp-symmetric-p (path)
  "True when the encrypted file at PATH begins with a passphrase's packet (a
symmetric-key encrypted session key), as gpg --symmetric writes them; only
its first octets are read."
  (eql 3 (tuatara.io:openpgp-packet-header (tuatara.io:file-head path 16) 0)))

;;; Keeping the text from the disk

(defvar *encrypted-buffers* '()
  "The buffers holding text that was encrypted (KEEP-FROM-DISK).")

(defun encrypted-buffer-p (buffer)
  "True when BUFFER holds the decrypted text of an encrypted file."
  (and buffer (tuatara.buffer:buffer-local buffer :encrypted) t))

(defun set-dumpable (dumpable)
  "Allow the system to keep a core dump of the editor should it crash, or not
\(Linux's PR_SET_DUMPABLE): not while an encrypted file's text is in memory."
  (ignoring-errors-reported ("telling the system whether it may keep a core dump")
    (sb-alien:alien-funcall (sb-alien:extern-alien "prctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                                     sb-alien:unsigned-long sb-alien:unsigned-long
                                                                     sb-alien:unsigned-long))
                            4 (if dumpable 1 0) 0 0 0)))

(sb-ext:defglobal **secrets-held** 0
  "How many secrets other than buffers' text are held at once (HOLD-SECRET):
no core dump of the editor while any is. Changed under **DUMPABLE-LOCK**.")

(sb-ext:defglobal **dumpable-lock** (sb-thread:make-mutex :name "core dumps")
  "Held while whether the editor may be dumped is worked out and told the
system, so that two threads never tell it in the wrong order.")

(defun core-dumps-allowed-again ()
  "Allow core dumps of the editor again, when no encrypted file's text is open
and no secret is held (HOLD-SECRET)."
  (sb-thread:with-recursive-lock (**dumpable-lock**)
    (when (and (zerop **secrets-held**) (notany #'live-buffer-p *encrypted-buffers*))
      (set-dumpable t))))

(defun hold-secret ()
  "Say that a secret, a password asked for, is now held in memory: no core
dump of the editor until it is let go (LET-GO-OF-SECRET), from any thread."
  (sb-thread:with-recursive-lock (**dumpable-lock**)
    (incf **secrets-held**)
    (set-dumpable nil)))

(defun let-go-of-secret ()
  "Say that a secret held (HOLD-SECRET) has been wiped: core dumps are allowed
again once nothing else keeps them away."
  (sb-thread:with-recursive-lock (**dumpable-lock**)
    (decf **secrets-held**)
    (core-dumps-allowed-again)))

(defun keep-from-disk (buffer)
  "Keep BUFFER's text, which was encrypted, from the disk and from other
programs (TUATARA.IO:BUFFER-PLAINTEXT-P, which auto-save, the session, scratch
files, the git gutter, the clipboard, exports and code files heed); no core
dump of the editor while it is open. A save writes it encrypted, or asks
first where it would write it as it is (TUATARA.IO:HOST-SAVE-TRANSFORM)."
  (setf (tuatara.io:buffer-plaintext-p buffer) t)
  (sb-thread:with-recursive-lock (**dumpable-lock**)
    (pushnew buffer *encrypted-buffers*)
    (set-dumpable nil)))

(defun note-encrypted (buffer encryption)
  "Mark BUFFER as holding an encrypted file's text, ENCRYPTION a plist of
:KEYS and :SYMMETRIC, kept from the disk (KEEP-FROM-DISK)."
  (setf (tuatara.buffer:buffer-local buffer :encrypted) encryption)
  (keep-from-disk buffer))

(defun encrypted-killed (buffer)
  "Kill-buffer function: forget BUFFER among the encrypted ones; core dumps allowed again once none is open."
  (when (member buffer *encrypted-buffers*)
    (setf *encrypted-buffers* (remove buffer *encrypted-buffers*))
    (core-dumps-allowed-again)))

;;; Opening

(defvar *encrypted-waiting-keymap* (make-keymap :name "Encrypted file waiting for its passphrase")
  "Keys of an encrypted file's buffer waiting for its passphrase: \\`RET' asks for it.")

(defun encrypted-placeholder (buffer text)
  "Show TEXT, what is happening to it, in BUFFER, an encrypted file's buffer, read only."
  (tuatara.buffer:widen-buffer buffer)
  (tuatara.buffer:with-read-only-lifted (buffer :after t)
    (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text))
  (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '())
  (tuatara.buffer:buffer-mark-saved buffer))

(define-record (decrypt-job (:include tuatara.runtime:job)
                           (:constructor make-decrypt-job (buffer path passphrase))
                           (:copier nil))
  "The encrypted file at PATH being decrypted for BUFFER, a job: PASSPHRASE,
the one typed, if any; TEXT, the octets decrypted; INFO, what GPG-DECRYPT says
of how; STAMP, the file's as read."
  (path nil :documentation "The path of the encrypted file DECRYPT-JOB decrypts.")
  (passphrase nil :documentation "The passphrase typed for DECRYPT-JOB, or NIL.")
  (text nil :documentation "The octets DECRYPT-JOB decrypted.")
  (info nil :documentation "What GPG-DECRYPT says of how DECRYPT-JOB decrypted the file.")
  (stamp nil :documentation "The stamp of the file as DECRYPT-JOB read it."))

(defmethod print-object ((job decrypt-job) stream)
  "Print JOB by the name of the file it decrypts, never its passphrase or text."
  (print-unreadable-object (job stream :type t :identity t)
    (format stream "~a" (tuatara.io:native-file-name (decrypt-job-path job)))))

(tuatara.runtime:define-job-words decrypt-job
  :doing "Decrypting"
  :description (format nil "Decrypting ~a with gpg" (tuatara.io:native-file-name (decrypt-job-path job)))
  :verb "decrypt it"
  :left-words "nothing decrypted")

(defmethod tuatara.runtime:job-take-in ((job decrypt-job))
  "Show what the decryption gave (DECRYPTED-IN)."
  (decrypted-in (tuatara.runtime:job-buffer job) (decrypt-job-path job) (decrypt-job-text job) (decrypt-job-info job)
                (decrypt-job-stamp job)
                (and (not (eq (tuatara.runtime:job-status job) :done)) (tuatara.runtime:job-ending-words job))))

(defun decrypted-in (buffer path text info stamp &optional ending)
  "Show TEXT, the file at PATH decrypted (its STAMP as read), in BUFFER, in the
mode its name without .gpg calls for; or, with no TEXT, say why it could not
be decrypted (INFO's :ERROR, else ENDING), and how to give the passphrase
when it wants one. TEXT is wiped once in BUFFER, and at once when BUFFER is
gone."
  (let ((name (tuatara.io:native-file-name path)))
    (if (not (live-buffer-p buffer))
        (when text (fill text 0))
        (cond
          (text
           (setf (buffer-read-only-p buffer) nil)
           (tuatara.buffer:widen-buffer buffer)
           (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text)
           (fill text 0)
           (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '()
                 (tuatara.buffer:buffer-point buffer) 0
                 (tuatara.io:buffer-file-stamp buffer) stamp
                 (tuatara.buffer:buffer-local buffer :disk-state) nil
                 (tuatara.buffer:buffer-local buffer :disk-state-said) nil
                 (tuatara.buffer:buffer-line-ending buffer)
                 (tuatara.buffer:detect-line-ending (tuatara.buffer:buffer-text buffer))
                 (tuatara.buffer:buffer-local buffer :encrypted)
                 (list :keys (getf info :keys) :symmetric (getf info :symmetric))
                 (buffer-override-keymap buffer) nil)
           (tuatara.buffer:buffer-mark-saved buffer)
           (set-major-mode buffer (tuatara.modes:mode-for-file path))
           (unless (tuatara.io:writable-p path)
             (setf (buffer-read-only-p buffer) t))
           (message "~a decrypted; a save encrypts it ~{~a~^ and ~} (~a chooses again)"
                    name (remove nil (list (and (getf info :keys) (format nil "to ~{~a~^, ~}" (getf info :key-words)))
                                           (and (getf info :symmetric) "with a passphrase")))
                    (substitute-command-keys "\\[gpg-choose-encryption-keys]")))
          ((getf info :needs-passphrase)
           (encrypted-placeholder buffer (format nil "~a needs its passphrase: ~a asks for it.~%" name
                                                 (substitute-command-keys "\\[gpg-decrypt-with-passphrase]")))
           (setf (buffer-override-keymap buffer) *encrypted-waiting-keymap*)
           (message "~a needs its passphrase: RET asks for it" name))
          (t
           (encrypted-placeholder buffer (format nil "~a could not be decrypted: ~a~%"
                                         name (or (getf info :error) ending "gpg failed"))))))))

(defun start-decrypting (buffer path &optional passphrase)
  "Decrypt the file at PATH into BUFFER in the background, with PASSPHRASE when
given (wiped once handed to gpg); at once, the terminal lent to gpg-agent's
pinentry, when it asks there (WITH-GPG-TERMINAL)."
  (encrypted-placeholder buffer (format nil "Decrypting ~a with gpg...~%" (tuatara.io:native-file-name path)))
  (if (eq (gpg-asker) :terminal)
      (tuatara.io:call-with-file
       path
       (lambda (fd native size)
         (let ((tuatara.runtime:*secret-at-hand* t)
               (octets (tuatara.io:read-fd-octets fd :path native :size-hint (1+ size)))
               (stamp (tuatara.io:fd-stamp fd native)))
           (multiple-value-bind (text info)
               (unwind-protect (with-gpg-terminal (format nil "decrypting ~a" (tuatara.io:native-file-name native))
                                                  (lambda () (decrypt-naming-keys octets :passphrase passphrase)))
                 (wipe-string passphrase))
             (fill octets 0)
             (decrypted-in buffer native text info stamp)))))
      (start-decrypting-job buffer path passphrase)))

(defun start-decrypting-job (buffer path passphrase)
  "Decrypt the file at PATH into BUFFER on a job, with PASSPHRASE when given (wiped once handed to gpg)."
  (tuatara.runtime:start-job-thread
   (make-decrypt-job buffer path passphrase)
   (lambda (job)
     (let ((tuatara.runtime:*secret-at-hand* t))
       (tuatara.io:call-with-file
        (decrypt-job-path job)
        (lambda (fd native size)
          (let ((octets (tuatara.io:read-fd-octets fd :path native :size-hint (1+ size))))
            (setf (decrypt-job-stamp job) (tuatara.io:fd-stamp fd native))
            (multiple-value-bind (text info)
                (unwind-protect (decrypt-naming-keys octets :passphrase (decrypt-job-passphrase job)
                                                            :cancel (tuatara.runtime:job-cancel job))
                  (wipe-string (decrypt-job-passphrase job)))
              (setf (decrypt-job-text job) text (decrypt-job-info job) info)))))))
   :name "tuatara decrypt"))

(define-command gpg-decrypt-with-passphrase ()
  "Decrypt again the encrypted file this buffer waits for, its passphrase
asked as *GPG-PASSPHRASE-FROM* says: in the minibuffer, shown as stars, or by
gpg-agent's pinentry."
  (let* ((buffer (current-buffer))
         (path (tuatara.buffer:buffer-file buffer)))
    (if (not (and path (encrypted-buffer-p buffer)))
        (message "This buffer waits for no passphrase")
        (start-decrypting buffer (tuatara.io:native path)
                          (and (gpg-minibuffer-p)
                               (gpg-read-passphrase (format nil "Passphrase for ~a: "
                                                            (tuatara.io:native-file-name path))))))))

(defun encrypted-visit (path show)
  "Show the encrypted file at PATH (a name ending in .gpg), decrypted in the
background into a buffer of its own, with SHOW, a function of the buffer;
NIL for any other file. With the minibuffer asking for passphrases, one
encrypted with a passphrase asks for it first."
  (let ((native (file-view-path path)))
    (when (and native (gpg-name-p native))
      (file-view-show native *encrypted-buffers* (lambda (b) (and (live-buffer-p b) (tuatara.buffer:buffer-file b)
                                                                    (tuatara.io:native (tuatara.buffer:buffer-file b))))
                      (lambda ()
                        (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name (tuatara.io:native-file-name native)))))
                          (setf (tuatara.buffer:buffer-file buffer) native
                                (tuatara.io:buffer-file-stamp buffer) (tuatara.io:path-stamp native)
                                (tuatara.buffer:buffer-local buffer :reread)
                                (lambda (b) (start-decrypting b (tuatara.io:native (tuatara.buffer:buffer-file b)))))
                          (note-encrypted buffer (list :keys '() :symmetric nil))
                          (start-decrypting buffer native
                                            (and (gpg-minibuffer-p)
                                                 (openpgp-symmetric-p native)
                                                 (gpg-read-passphrase (format nil "Passphrase for ~a: " (tuatara.io:native-file-name native)))))
                          buffer))
                      show))))

;;; Saving

(defun gpg-encrypt-arguments (encryption)
  "gpg's arguments encrypting to ENCRYPTION's :KEYS and with a passphrase when :SYMMETRIC."
  (append (and (getf encryption :symmetric) (list "--symmetric"))
          (and (getf encryption :keys)
               (cons "--encrypt" (loop for key in (getf encryption :keys) append (list "--recipient" key))))
          (list "--output" "-")))

(defmethod tuatara.io:host-save-transform ((host defaults-host) buffer path)
  "The transform of BUFFER's text as its save as PATH has it: for an encrypted
buffer, or one saved under a name ending in .gpg, its text encrypted by gpg
to the keys and passphrase it has; with none yet, the
question how to encrypt it asked first (READ-ENCRYPTION), the save refused
when nothing is chosen. A buffer holding text decrypted in place in a file
that is not encrypted (\\[gpg-decrypt-region]) is saved as it is, the decrypted
text in clear, only when the question asked first is answered yes; else the
save is refused."
  (when (and (tuatara.io:buffer-plaintext-p buffer)
             (not (encrypted-buffer-p buffer))
             (not (gpg-name-p (tuatara.io:native path)))
             (not (yes-or-no-p* (format nil "~a holds text decrypted in place; save it unencrypted?"
                                        (tuatara.buffer:buffer-name buffer)))))
    (refuse "~a is not saved: it holds text decrypted in place" (tuatara.buffer:buffer-name buffer)))
  (when (or (encrypted-buffer-p buffer) (gpg-name-p (tuatara.io:native path)))
    (let ((encryption (tuatara.buffer:buffer-local buffer :encrypted)))
      (unless (or (getf encryption :keys) (getf encryption :symmetric))
        (setf encryption (or (read-encryption (tuatara.buffer:buffer-name buffer))
                             (refuse "~a is not saved: nothing chosen to encrypt it to"
                                     (tuatara.buffer:buffer-name buffer)))))
      (note-encrypted buffer encryption)
      (let ((passphrase (and (getf encryption :symmetric) (gpg-minibuffer-p)
                             (gpg-read-passphrase (format nil
                                                          "Passphrase to encrypt ~a with: "
                                                          (tuatara.buffer:buffer-name buffer))
                                                  :confirm t)))
            (name (tuatara.buffer:buffer-name buffer)))
        (if (eq (gpg-asker) :terminal)
            ;; encrypted now, the terminal lent to the pinentry: the save writes what it gave
            (let ((output (with-gpg-terminal (format nil "encrypting ~a" name)
                                             (lambda ()
                                               (encrypt-octets (tuatara.text:version-rope
                                                                (tuatara.buffer:buffer-version buffer))
                                                               encryption name)))))
              (lambda (rope) (declare (ignore rope)) output))
            (lambda (rope) (encrypt-octets rope encryption name :passphrase passphrase)))))))

(defun encrypt-octets (rope encryption name &key passphrase)
  "The octets of ROPE, NAME's text, encrypted by gpg as ENCRYPTION (a plist of
:KEYS and :SYMMETRIC) says, with PASSPHRASE for the minibuffer's loopback;
the plain octets and the passphrase are wiped once gpg has them. Refused,
saying why, when gpg gives nothing."
  (let ((tuatara.runtime:*secret-at-hand* t)
        (plain (tuatara.text:rope-octets rope)))
    (multiple-value-bind (output statuses ok said)
        (unwind-protect (gpg-run (gpg-encrypt-arguments encryption) :input plain :passphrase passphrase)
          (fill plain 0)
          (wipe-string passphrase))
      (unless (and ok output (plusp (length output)))
        (refuse "gpg could not encrypt ~a: ~a" name (gpg-failure-words statuses said)))
      output)))

;;; Choosing keys

(defparameter *gpg-listing-seconds* 10
  "The most seconds gpg may take to list the keys (GPG-KEYS): past them it is
stopped and the listing refused, rather than the editor kept waiting on a
locked keyring or an agent that does not answer.")

(defun gpg-keys (&key all)
  "The keys gpg has that can encrypt, each a plist of :ID, :NAME, :VALIDITY (words),
:EXPIRES (a universal time or NIL), :SECRET (true for one of yours) and
:SUBKEYS (the IDs of its subkeys, which a file names it by), :FINGERPRINT and
:CAPABILITIES (gpg's letters: e encrypts, s signs, c certifies); with ALL, the
keys that cannot encrypt too. Refused, saying why, when gpg gives no answer
within *GPG-LISTING-SECONDS*."
  (flet ((lines (arguments)
           (uiop:split-string (tuatara.text:lenient-string
                               (or (gpg-run arguments :asker :agent :seconds *gpg-listing-seconds*) #()))
                              :separator '(#\Newline)))
         (fields (line) (uiop:split-string line :separator '(#\:))))
    (let ((secret (loop for line in (lines (list "--list-secret-keys" "--with-colons"))
                        for f = (fields line)
                        when (string= (first f) "sec") collect (fifth f)))
          (keys '()) (current nil))
      (dolist (line (lines (list "--list-keys" "--with-colons")))
        (let ((f (fields line)))
          (cond ((string= (first f) "pub")
                 (setf current (and (>= (length f) 12) (or all (find #\E (nth 11 f)))
                                    (list :id (fifth f)
                                          :name nil
                                          :capabilities (remove-duplicates (string-downcase (nth 11 f)))
                                          :validity (case (char (concatenate 'string (second f) "-") 0)
                                                      (#\u "ultimate") (#\f "full") (#\m "marginal") (#\n "never")
                                                      (#\e "expired") (#\r "revoked") (t "unknown"))
                                          :expires (let ((n (parse-integer (seventh f) :junk-allowed t)))
                                                     (and n (+ n +unix-epoch+)))
                                          :secret (and (member (fifth f) secret :test #'string=) t))))
                 (when current (push current keys)))
                ((and current (string= (first f) "fpr") (null (getf current :fingerprint)))
                 (setf (getf current :fingerprint) (tenth f))
                 (setf (first keys) current))
                ((and current (string= (first f) "sub"))
                 (push (fifth f) (getf current :subkeys))
                 (setf (first keys) current))
                ((and current (string= (first f) "uid") (null (getf current :name)))
                 (setf (getf current :name) (tenth f))
                 (setf (first keys) current)))))
      (nreverse keys))))

(defun key-owners (ids)
  "IDS, keys' or subkeys' IDs a file is encrypted to, each with its owner's
name as your keyring has it, as (ID . NAME); those you do not have left out."
  (let ((keys (ignoring-errors-reported ("listing your keys") (gpg-keys))))
    (loop for id in ids
          for key = (find-if (lambda (k) (member id (cons (getf k :id) (getf k :subkeys)) :test #'string-equal)) keys)
          when (and key (getf key :name)) collect (cons id (getf key :name)))))

(defun encryption-key-words (ids)
  "IDS, keys' or subkeys' IDs a file is encrypted to, as words: each its
owner's name as your keyring has it, else the ID (a key you do not have)."
  (let ((owners (key-owners ids)))
    (mapcar (lambda (id) (or (cdr (assoc id owners :test #'string-equal)) (format nil "~a (a key you do not have)" id)))
            ids)))

(defun encryption-choice (entry)
  "ENTRY, :PASSPHRASE or one of GPG-KEYS, as the encryption question offers
it: a passphrase, or the key's owner and ID (a comma in a name left out, the
choices being separated by commas), with what more to say of it, as a
cons."
  (if (eq entry :passphrase)
      (cons "a passphrase" "asked for as it is saved (symmetric)")
      (cons (format nil
                    "~@[~a ~]~a"
                    (and (getf entry :name) (substitute #\Space #\, (getf entry :name)))
                    (getf entry :id))
            (format nil "~a~@[, expires ~a~]~:[~;, yours~]" (getf entry :validity)
                    (let ((e (getf entry :expires))) (and e (format-time "%F" e))) (getf entry :secret)))))

(defun read-encryption (what &optional encryption)
  "Ask in the minibuffer how WHAT (words, a file's name) is encrypted, to keys,
with a passphrase, or both, several at once with commas between them
\(READ-SEVERAL), those of ENCRYPTION (a plist of :KEYS and :SYMMETRIC) there
already. Returns the encryption chosen, a plist as ENCRYPTION is, or NIL
when nothing is chosen."
  (let* ((entries (cons :passphrase (gpg-keys)))
         (choices (mapcar (lambda (entry) (cons (car (encryption-choice entry)) entry)) entries))
         (initial (loop for entry in entries
                        when (if (eq entry :passphrase)
                                 (getf encryption :symmetric)
                                 (some (lambda (id) (member id (getf encryption :keys) :test #'string-equal))
                                       (cons (getf entry :id) (getf entry :subkeys))))
                          collect (car (encryption-choice entry))))
         (chosen (loop for text in (read-several (format nil "Encrypt ~a to (keys, a passphrase; commas between): " what)
                                                 (lambda () (mapcar #'encryption-choice entries))
                                                 :initial (format nil "~{~a, ~}" initial)
                                                 :none "nothing: no encryption" :more "these, no more")
                       for entry = (cdr (assoc text choices :test #'string=))
                       when entry collect entry)))
    (and chosen
         (list :keys (loop for e in chosen unless (eq e :passphrase) collect (getf e :id))
               :symmetric (and (member :passphrase chosen) t)))))

(defun choose-encryption (what encryption then)
  "Ask how WHAT is encrypted (READ-ENCRYPTION), those of ENCRYPTION offered,
and call THEN with the encryption chosen; with nothing chosen, say so and do
nothing."
  (let ((chosen (read-encryption what encryption)))
    (if chosen
        (funcall then chosen)
        (message "Nothing chosen: nothing done"))))

(define-command gpg-choose-encryption-keys ()
  "Choose again whom the encrypted file shown is encrypted to, or a
passphrase, in the minibuffer: from its next save it is encrypted so."
  (let ((buffer (current-buffer)))
    (if (encrypted-buffer-p buffer)
        (choose-encryption (tuatara.buffer:buffer-name buffer) (tuatara.buffer:buffer-local buffer :encrypted)
                           (lambda (encryption)
                             (note-encrypted buffer encryption)
                             (message "~a is encrypted so from its next save" (tuatara.buffer:buffer-name buffer))))
        (message "~a is no encrypted file" (tuatara.buffer:buffer-name buffer)))))

;;; The rest of the editor

(define-command gpg-copy-to-clipboard ()
  "Put the text copied last on the system clipboard, even text of an encrypted
file, which is otherwise kept within Tuatara: other programs (clipboard
managers among them) can then read it, and keep it."
  (let ((kill (first (editor-kill-ring *editor*))))
    (if (and (stringp kill) (tuatara.frontend:frontend-set-clipboard (editor-frontend *editor*) kill))
        (progn (setf *clipboard-sent* kill) (message "On the system clipboard"))
        (message "Nothing to put on the system clipboard"))))

(defun encrypted-health-words ()
  "What decrypts .gpg files here, as M-x check-health says it."
  (let ((program (gpg-path)))
    (if program
        (format nil "Encrypted files (.gpg) by ~a, passphrases asked by ~a" program
                (ecase (gpg-asker)
                  (:agent "gpg-agent's pinentry, in its window")
                  (:terminal "gpg-agent's pinentry, in the terminal lent it")
                  (:minibuffer "the minibuffer (gpg's loopback: the passphrase passes through the editor)")))
        (format nil "Encrypted files (.gpg) by none: ~a" (outside-program-install-words :gpg)))))

(defun encrypted-new-file (buffer)
  "Find-file function: a new file whose name ends in .gpg is encrypted from the
start, its text kept from the disk before its first save encrypts it."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (when (and file (gpg-name-p (tuatara.io:native file))
               (null (tuatara.io:path-stamp (tuatara.io:native file))))
      (note-encrypted buffer (list :keys '() :symmetric nil)))))

(defun install-encrypted ()
  "Have files ending in .gpg opened decrypted and saved encrypted, their text kept from the disk."
  (pushnew 'encrypted-visit *file-views*)
  (pushnew 'encrypted-killed tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'encrypted-new-file tuatara.buffer:*find-file-functions*)
  (define-key *encrypted-waiting-keymap* "RET" 'gpg-decrypt-with-passphrase))
