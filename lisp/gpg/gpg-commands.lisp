;;;; lisp/gpg/gpg-commands.lisp --- GnuPG on files, regions and keys: encrypt, decrypt, sign, verify, and the keys
;;;; listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; GnuPG's work on files, regions and keys, under plain names. A file is
;;;; encrypted (FILE.gpg), decrypted (asked where, the text then on disk as it
;;;; is), signed (a detached signature FILE.sig, clear-signed text FILE.asc, or
;;;; a signed file FILE.gpg) or its signature verified, saying who signed it,
;;;; when, and how far that key is trusted; the same on the region, the armoured
;;;; blocks in it decrypted or verified in place; and the keys listed (M-x
;;;; gpg-keys-manager), their details shown, imported from a file or the region,
;;;; exported, inserted, deleted. gpg reads and writes through pipes as for
;;;; encrypted files (encrypted.lisp): a file is read by Tuatara without
;;;; changing its access time, and what gpg gives back is written by Tuatara, a
;;;; decrypted file for you alone. Work on a file runs in the background, or at
;;;; once with the terminal lent to gpg-agent's pinentry when it asks there.

(in-package #:tuatara.defaults)

;;; Running gpg on work that takes time

(define-record (gpg-job (:include tuatara.runtime:job) (:constructor make-gpg-job (buffer words then quiet after))
                        (:copier nil))
  "gpg at work on WORDS (decrypting report.pdf.gpg), a job; THEN, called on the
editor's thread with the RESULTS of its work, a list of values (what gpg gave:
its output, status lines, whether it ended well and what it said; or the keys
listed). QUIET, true for work that only shows something (the keys listed),
stopped without asking when its buffer is killed. AFTER, when given, is
called once the job is taken in, however it ended."
  (words nil :documentation "The words describing the work of GPG-JOB, as in \"decrypting report.pdf.gpg\".")
  (then nil :documentation "The function called on the editor's thread with the results of GPG-JOB.")
  (after nil
         :documentation "NIL, or a function called on the editor's thread once GPG-JOB is taken in, with true
when its work was done, NIL when it was stopped or failed.")
  (quiet nil
         :documentation "True when GPG-JOB only shows something, so it is stopped without asking when its buffer is
killed.")
  (results nil
           :documentation "The list of values gpg gave for GPG-JOB: its output, status lines, whether it ended well
and what it said, or the keys listed."))

(tuatara.runtime:define-job-words gpg-job
  :doing (gpg-job-words job)
  :description (format nil "gpg: ~a" (gpg-job-words job))
  :verb (gpg-job-words job)
  :left-words "nothing done")

(defmethod tuatara.runtime:job-at-exit ((job gpg-job))
  "Stop work that only shows something without asking; ask before stopping any other."
  (if (gpg-job-quiet job) :quiet :stop))

(defmethod tuatara.runtime:job-take-in ((job gpg-job))
  "Hand the results of the job's work to its THEN; when it was stopped or
failed, wipe the octets it gave and say why. Then call its AFTER."
  (let ((done (eq (tuatara.runtime:job-status job) :done)))
    (unwind-protect
         (if done
             (apply (gpg-job-then job) (gpg-job-results job))
             (progn
               (dolist (result (gpg-job-results job))
                 (when (typep result '(vector (unsigned-byte 8))) (fill result 0)))
               (message "gpg: ~a: ~a" (gpg-job-words job) (tuatara.runtime:job-ending-words job))))
      (when (gpg-job-after job) (funcall (gpg-job-after job) done)))))

(defun gpg-in-background (words work then &key (asker (gpg-asker)) (buffer (current-buffer)) quiet after)
  "Call WORK, a function of a cancel cell running gpg, on WORDS's behalf (words
for the job, as decrypting FILE), and THEN on the editor's thread with the values
it returns, in the background, a job of BUFFER's (QUIET and AFTER as GPG-JOB
has them), or at once with the terminal lent to gpg-agent's pinentry when
ASKER is :TERMINAL (GPG-ASKER)."
  (if (eq asker :terminal)
      (progn (multiple-value-call then (with-gpg-terminal words (lambda () (funcall work nil))))
             (when after (funcall after t)))
      (tuatara.runtime:start-job-thread
       (make-gpg-job buffer words then quiet after)
       (lambda (job)
         (setf (gpg-job-results job) (multiple-value-list (funcall work (tuatara.runtime:job-cancel job)))))
       :name "tuatara gpg")))

(defun run-gpg-work (words arguments then &key input-path input passphrase second-path (asker (gpg-asker))
                                               (buffer (current-buffer)) quiet after)
  "Run gpg with ARGUMENTS on WORDS's behalf (words for the job, decrypting
FILE), its input the file at INPUT-PATH or the octets INPUT, a detached
signature at SECOND-PATH given as its second input; THEN called with what it
gave (its output, status lines, whether it ended well and what it said). In
the background, or at once with the terminal lent to gpg-agent's pinentry
when ASKER (GPG-ASKER) says it asks there (GPG-IN-BACKGROUND, BUFFER, QUIET
and AFTER as it takes them). INPUT, a copy made for gpg, and PASSPHRASE, for
the minibuffer's loopback, are wiped once gpg has them, and in any case once
the job is taken in, as when no thread could be had for it."
  (flet ((wipe ()
           (when input (fill input 0))
           (wipe-string passphrase)))
    (gpg-in-background words
                       (lambda (cancel)
                         ;; the files read by gpg's pipes a block at a time, never whole in memory
                         (unwind-protect (gpg-run arguments :input input :input-path (and (null input) input-path)
                                                            :passphrase passphrase :cancel cancel
                                                            :second-path second-path :asker asker)
                           (wipe)))
                       then :asker asker :buffer buffer :quiet quiet
                            :after (lambda (done)
                                     (wipe)
                                     (when after (funcall after done))))))

(defun gpg-files-in-turn (paths start &optional finally)
  "Have gpg work on each of PATHS in turn, one at a time: START, a function of
a path and of AFTER, starts the work on one (RUN-GPG-WORK and the commands
on files take AFTER), and the next is started once that is taken in, each a
job of the buffer current when this was called, whichever is current then.
Stopping one stops the rest; leaving START without starting the work, as
\\[keyboard-quit] at a question it asks does, stops them too, the files left
said. FINALLY, when given, a function of no arguments, is called once, when
none is left or the rest was stopped."
  (let ((buffer (current-buffer)) (ended nil))
    (labels ((end ()
               (unless ended
                 (setf ended t)
                 (when finally (funcall finally))))
             (from (rest)
               (if (null rest)
                   (end)
                   (let ((started nil))
                     (unwind-protect
                          (let ((*current-buffer* (if (live-buffer-p buffer) buffer (current-buffer))))
                            (funcall start (first rest)
                                     (lambda (done) (if done (from (rest rest)) (end))))
                            (setf started t))
                       (unless (or started ended)
                         (message "Not done: ~{~a~^, ~}" (mapcar #'tuatara.io:native-file-name rest))
                         (end)))))))
      (from paths))))

(defun gpg-free-targets (paths target)
  "Those of PATHS whose file made by gpg, TARGET, a function of a path, gives
its name, is free (GPG-TARGET-FREE-P): each one there asked about in turn."
  (remove-if-not (lambda (path) (gpg-target-free-p (funcall target path))) paths))

(defun gpg-own-passphrase (what)
  "With the minibuffer asking for passphrases (GPG-MINIBUFFER-P), the passphrase
of your key for WHAT, asked for: NIL for RET alone (gpg-agent has it). NIL
when gpg-agent's pinentry asks."
  (when (gpg-minibuffer-p)
    (let ((typed (gpg-read-passphrase (format nil "Passphrase of your key for ~a (RET: gpg-agent has it): " what))))
      (if (zerop (length typed)) nil typed))))

(defun write-gpg-output (path octets &key private from)
  "Write OCTETS, what gpg gave, as the file at PATH (never through a link),
for you alone with PRIVATE; and show it in the lists of its directory. Made
FROM a file come from outside, it is marked as come from outside too
\(KEEPING-UNTRUSTED)."
  (flet ((write-it ()
           (tuatara.io:write-text-file (tuatara.text:octets->rope (coerce octets 'tuatara.text:octets)) path
                                       :expected :any :follow-links nil :mode (and private (constantly #o600)))))
    (if from
        (keeping-untrusted (from path) (write-it))
        (write-it)))
  (paths-changed (list path)))

(defun gpg-target-free-p (path)
  "True when nothing is at PATH, or you say to write over what is."
  (or (null (tuatara.io:path-stamp path :follow nil))
      (yes-or-no-p* (format nil "Write over ~a?" (tuatara.io:native-file-name path))
                    :details (format nil "~a exists." (abbreviated-path path)))))

(defun confirm-decrypting-to-disk (what &optional details)
  "True when, asked, you say to decrypt WHAT (words naming a file, or files) to
disk, where its text can then be read; DETAILS, when given, are shown before
what the answer does."
  (yes-or-no-p* (format nil "Decrypt ~a to disk?" what)
                :details (format nil "~@[~a~%~%~]Its text is then on disk, where it can be read." details)))

(defun gpg-stripped-name (path endings)
  "PATH without the first of ENDINGS it ends in (.gpg), or NIL."
  (let ((ending (find-if (lambda (e) (uiop:string-suffix-p (string-downcase path) e)) endings)))
    (and ending (subseq path 0 (- (length path) (length ending))))))

(defun decrypted-name (path)
  "The name a decrypted copy of the file at PATH is given: PATH without its
.gpg, .pgp or .asc, else PATH with .decrypted after it."
  (or (gpg-stripped-name path '(".gpg" ".pgp" ".asc")) (concatenate 'string path ".decrypted")))

;;; Files

(defun gpg-file-to (path target verb arguments said &key status private passphrase then after)
  "Run gpg with ARGUMENTS to VERB (encrypt, decrypt, sign) the file at PATH,
PASSPHRASE, for the minibuffer's loopback, wiped once gpg has it (RUN-GPG-WORK,
AFTER as it takes it). When gpg ends well, and has said STATUS when given
\(else given back anything), write what it gave as the file TARGET, for you
alone and then wiped from memory when PRIVATE; say SAID (words) and call
THEN, a function of no arguments, when given. Else say that the file was not
VERBed, and why, what gpg gave wiped."
  (let ((name (tuatara.io:native-file-name path)))
    (run-gpg-work (format nil "~aing ~a" verb name) arguments
                  (lambda (output statuses ok words)
                    (if (and ok (if status (gpg-status-p statuses status) (plusp (length output))))
                        (progn
                          ;; what was decrypted wiped whether or not it could be written; it is the
                          ;; octets the file is written from, never copied (TUATARA.TEXT:OCTETS->ROPE)
                          (unwind-protect (progn (write-gpg-output target output :private private :from path)
                                                 (message "~a" said))
                            (when private (fill output 0)))
                          (when then (funcall then)))
                        (progn (fill output 0)
                               (message "~a was not ~aed: ~a" name verb (gpg-failure-words statuses words)))))
                  :input-path path :passphrase passphrase :after after)))

(defun encrypt-file-to (path encryption &key then after (passphrase nil passphrase-p))
  "Encrypt the file at PATH as ENCRYPTION says (:KEYS, :SYMMETRIC, and the keys'
:KEY-WORDS to name them by, as READ-ENCRYPTION chooses) into PATH.gpg, written
over when it exists; THEN, a function of no arguments, called once it is
written, AFTER as RUN-GPG-WORK takes it. PASSPHRASE, for the minibuffer's
loopback, is asked for when not given, and wiped once gpg has it."
  (let ((target (concatenate 'string path ".gpg")))
    (gpg-file-to path target "encrypt" (gpg-encrypt-arguments encryption)
                 (format nil "Encrypted as ~a~@[ to ~{~a~^, ~}~]~:[~; with a passphrase~]" (abbreviated-path target)
                         (or (getf encryption :key-words) (getf encryption :keys)) (getf encryption :symmetric))
                 :then then :after after
                 :passphrase (if passphrase-p
                                 passphrase
                                 (gpg-passphrase-for (tuatara.io:native-file-name path) :encrypting encryption)))))

(defun gpg-files-argument (prompt verb)
  "The files a gpg command on files works on, words naming them, and details
about them to show beside its questions (NIL for none), as three values. In a
list of a directory: the files marked, or the one on the line, the details
telling where they are when that is elsewhere, and how many the search filters
hide (RIMU-QUESTION);
an error saying why when there is none to VERB. Elsewhere, or with
\\[universal-argument]: the file shown or asked for with PROMPT
\(SHOWN-FILE-ARGUMENT)."
  (let ((buffer (current-buffer)))
    (if (and (not (prefix nil)) (typep (buffer-major-mode buffer) 'rimu-mode))
        (let ((paths (or (rimu-chosen-paths buffer :files-only t)
                         (refuse "~a" (rimu-nothing-words
                                       buffer verb :files-only t
                                       :none "No file here: mark files, or put the cursor on one")))))
          (multiple-value-bind (words details)
              (rimu-question nil paths :here (tuatara.buffer:buffer-local buffer 'rimu-directory))
            (values paths words details)))
        (let ((path (shown-file-argument prompt)))
          (values (list path) (tuatara.io:native-file-name path))))))

(define-command gpg-encrypt-file ()
  "Encrypt the file shown (in a list of a directory, the files marked, or the
one on the line; with \\[universal-argument], or where there is none, one asked
for) each into FILE.gpg, to the keys or with a passphrase chosen in the
minibuffer, commas between several, asked once. When you say so, asked once
too, wipe each file once it is encrypted (\\[wipe-file]): with a passphrase,
only after it, asked again, opens what was made."
  (multiple-value-bind (paths what details) (gpg-files-argument "Encrypt file: " "encrypt")
    (choose-encryption
     what '()
     (lambda (encryption)
       (let ((paths (gpg-free-targets paths (lambda (path) (concatenate 'string path ".gpg")))))
         (when paths
           (let* ((symmetric (getf encryption :symmetric))
                  (passphrase (gpg-passphrase-for what :encrypting encryption))
                  (wipe (wipe-original-question "the originals once encrypted" encryption (wipe-caveat paths)))
                  (check (and wipe symmetric (gpg-minibuffer-p)
                              (gpg-read-passphrase "The passphrase again, to check it opens what is made: "))))
             (gpg-files-in-turn
              paths
              (lambda (path after)
                (let ((check (and check (copy-seq check))))
                  (encrypt-file-to path encryption
                                   :passphrase (and passphrase (copy-seq passphrase))
                                   :after after
                                   :then (and wipe (lambda ()
                                                     (wipe-original path (concatenate 'string path ".gpg")
                                                                    encryption :passphrase check))))))
              (lambda ()
                (wipe-string passphrase)
                (wipe-string check)))))))
     :details details)))

(defun decrypt-file-to (path target &key then after)
  "Decrypt the file at PATH into the file TARGET, for you alone; THEN, a
function of no arguments, called once it is written, AFTER as RUN-GPG-WORK
takes it."
  (gpg-file-to path target "decrypt" (list "--decrypt" "--output" "-")
               (format nil "Decrypted as ~a, for you alone" (abbreviated-path target))
               :status "DECRYPTION_OKAY" :private t :then then :after after
               :passphrase (gpg-passphrase-for (tuatara.io:native-file-name path) :path path)))

(defun gpgconf-path ()
  "gpgconf, GnuPG's program that runs its parts: the one beside gpg
\(*GPG-PROGRAM*), else the one in the system's own directories of programs;
NIL when there is none."
  (let ((gpg (gpg-path)))
    (or (and gpg (tuatara.io:find-program (tuatara.io:path-join (tuatara.io:parent-path gpg) "gpgconf")))
        (tuatara.io:sandbox-find-program "gpgconf"))))

(define-command gpg-lock ()
  "Make gpg-agent forget every passphrase it keeps for a while (ten minutes,
as it comes, its default-cache-ttl), of encrypted files and of your keys: the
next to open one asks again. Text already open stays open: kill those
buffers too to lock it away."
  (let ((gpgconf (or (gpgconf-path)
                     (tuatara.io:program-missing "gpgconf" :why (outside-program-install-words :gpg))))
        (environment (gpg-environment :agent)))
    ;; reloading gpg-agent flushes its cache; one not running keeps nothing
    (gpg-in-background "making gpg-agent forget"
                       (lambda (cancel)
                         (declare (ignore cancel))
                         (nth-value 1 (tuatara.io:program-output (list gpgconf "--reload" "gpg-agent")
                                                                 :capture nil :seconds 5 :environment environment)))
                       (lambda (done)
                         (message (if done
                                      "gpg-agent has forgotten the passphrases it kept"
                                      "gpg-agent could not be told to forget its passphrases: gpgconf failed or took ~
                                       over 5 seconds")))
                       :asker :agent :quiet t)))

(define-command gpg-decrypt-file ()
  "Decrypt the file shown (in a list of a directory, the files marked, or the
one on the line; with \\[universal-argument], or where there is none, one asked
for) into its name without .gpg, for you alone, after asking once: the text is
then on disk where it can be read. One file is decrypted to a name asked for,
the one without .gpg offered. To read it, visit the encrypted file instead:
its text stays in memory."
  (multiple-value-bind (paths what details) (gpg-files-argument "Decrypt file: " "decrypt")
    (let ((named (and (null (rest paths))
                      (read-native-path "Decrypt to: " :initial (abbreviated-path (decrypted-name (first paths)))
                                                       :kind :any :writing t))))
      (flet ((target (path) (or named (decrypted-name path))))
        (when (or named (rest paths))
          (let ((paths (gpg-free-targets paths #'target)))
            (when (and paths (confirm-decrypting-to-disk what details))
              (gpg-files-in-turn paths (lambda (path after) (decrypt-file-to path (target path) :after after))))))))))

(defparameter *signature-kinds*
  '(("detached signature (FILE.sig)" ".sig" "--detach-sign")
    ("clear-signed text (FILE.asc)" ".asc" "--clearsign")
    ("signed file (FILE.gpg)" ".gpg" "--sign"))
  "The kinds of signature gpg-sign-file makes: the words asked with, the ending of the file made, gpg's option.")

(defun read-signature-kind (&optional details)
  "The kind of signature to make, asked for, with DETAILS shown beside the
question when given: an entry of *SIGNATURE-KINDS*."
  (assoc (completing-read "Signature: "
                          (mapcar #'first *signature-kinds*)
                          :require-match t
                          :default (first (first *signature-kinds*))
                          :details details)
         *signature-kinds* :test #'string=))

(defun signature-name (path kind)
  "The name of the signature of the file at PATH that KIND, an entry of
*SIGNATURE-KINDS*, makes: PATH and the kind's ending."
  (concatenate 'string path (second kind)))

(defun sign-file-as (path kind passphrase &key after)
  "Sign the file at PATH as KIND (an entry of *SIGNATURE-KINDS*) says, into
its SIGNATURE-NAME, written over when it exists; PASSPHRASE, for the
minibuffer's loopback, wiped once gpg has it; AFTER as RUN-GPG-WORK takes it."
  (let ((target (signature-name path kind)))
    (gpg-file-to path target "sign" (list (third kind) "--output" "-")
                 (format nil "Signed: ~a" (abbreviated-path target))
                 :status "SIG_CREATED" :passphrase passphrase :after after)))

(define-command gpg-sign-file ()
  "Sign the file shown (in a list of a directory, the files marked, or the one
on the line; with \\[universal-argument], or where there is none, one asked for)
with your key: a detached signature (FILE.sig, the file left as it is),
clear-signed text (FILE.asc) or a signed file (FILE.gpg), asked which, once."
  (multiple-value-bind (paths what details) (gpg-files-argument "Sign file: " "sign")
    (let* ((kind (read-signature-kind details))
           (paths (gpg-free-targets paths (lambda (path) (signature-name path kind))))
           (passphrase (and paths (gpg-own-passphrase (format nil "signing ~a" what)))))
      (gpg-files-in-turn paths
                         (lambda (path after)
                           (sign-file-as path kind (and passphrase (copy-seq passphrase)) :after after))
                         (lambda () (wipe-string passphrase))))))

(defun signature-words (statuses said ok)
  "What gpg's STATUSES of a verification say, in words, of every signature in
turn: who signed, when and how far the key is trusted, or why the signature
does not hold. OK, true when gpg ended well, and SAID, what it said besides,
tell of a failure the signatures do not account for. It begins \"Good
signature\" only for a single good signature on which gpg ended well."
  (flet ((words (signature)
           (destructuring-bind (&key status key-id name made trust &allow-other-keys) signature
             (let ((name (or name key-id)))
               (ecase status
                 (:good (format nil "good signature by ~a (key ~a)~@[, made ~a~], by ~a" name key-id
                                (and made (time-words made))
                                (case trust
                                  (:ultimate "a key you trust ultimately (yours)")
                                  (:fully "a key trusted fully")
                                  (:marginal "a key trusted marginally")
                                  (:never "a key you never trust")
                                  (t "a key whose owner nothing confirms (not certified)"))))
                 (:bad (format nil "BAD signature by ~a: the file was changed after it was signed, or the ~
                                    signature is not for it" name))
                 (:expired (format nil "signature by ~a, which has expired" name))
                 (:expired-key (format nil "signature by ~a, whose key has expired" name))
                 (:revoked (format nil "signature by ~a, whose key is revoked" name))
                 (:missing (format nil "signed by key ~a, which you do not have: import it (M-x gpg-keys-import) ~
                                        to check the signature" key-id))
                 (:error (format nil "a signature~@[ by key ~a~] that gpg could not check" key-id)))))))
    (let* ((signatures (gpg-signatures statuses))
           (text (format nil "~:[~*~;~d signatures: ~]~{~a~^; ~}"
                         (rest signatures) (length signatures) (mapcar #'words signatures))))
      (cond ((null signatures) (format nil "No signature checked: ~a" (gpg-failure-words statuses said)))
            ((and (not ok) (every (lambda (signature) (eq (getf signature :status) :good)) signatures))
             (format nil "Not verified, gpg failed (~a); of the signatures it said: ~a"
                     (gpg-failure-words statuses said) text))
            ((rest signatures) text)
            (t (concatenate 'string (string-upcase (subseq text 0 1)) (subseq text 1)))))))

(define-command gpg-verify-file ()
  "Check the signature of the file shown (in a list of a directory, the files
marked, or the one on the line; with \\[universal-argument], or where there is
none, one asked for): a detached signature (FILE.sig, FILE.asc) against its
file, else the signature within the file; say, in turn, who signed it, when,
and how far that key is trusted. Neither file's access time changes."
  (gpg-files-in-turn (gpg-files-argument "Verify file: " "verify")
                     (lambda (path after) (verify-file-at path :after after))))

(defun verify-file-at (path &key after)
  "Check the signature of the file at PATH, as \\[gpg-verify-file] does;
AFTER as RUN-GPG-WORK takes it."
  (let* ((data (gpg-stripped-name path '(".sig" ".asc")))
         (detached (and data (tuatara.io:path-stamp data)
                        (not (search "-----BEGIN PGP SIGNED MESSAGE-----"
                                     (tuatara.text:lenient-string (or (tuatara.io:file-head path 200) #())))))))
    (run-gpg-work (format nil "verifying ~a" (tuatara.io:native-file-name path))
                  (if detached (list "--verify" "--" :second-input "-") (list "--verify" "--" "-"))
                  (lambda (output statuses ok said)
                    (declare (ignore output))
                    (message "~a: ~a" (tuatara.io:native-file-name path) (signature-words statuses said ok)))
                  :input-path (if detached data path) :second-path (and detached path) :after after)))

;;; The region

(defun region-or-error (buffer)
  "BUFFER's selection (SELECTED-REGION), as two values, its start and end; an
error when no text is selected."
  (values-list (or (selected-region buffer) (refuse "Select the text first"))))

(defun replace-region-when-unchanged (buffer start end version text words &key before)
  "Replace BUFFER's text from START below END with TEXT (octets) as one edit, when
the buffer is still at VERSION, the selection ending with the text it held,
BEFORE, a function of no arguments, called first when given; else say that
WORDS was not put in, the text having changed. True when it was put in."
  (if (and (live-buffer-p buffer) (eq (tuatara.buffer:buffer-version buffer) version))
      (progn (when before (funcall before))
             (tuatara.buffer:buffer-replace buffer start end text)
             (drop-selection buffer)
             t)
      (progn (message "~a: the text changed meanwhile, nothing put in" words) nil)))

(define-command gpg-encrypt-region ()
  "Encrypt the text selected in place, as an armoured block (-----BEGIN PGP
MESSAGE-----), to the keys or with a passphrase chosen in the minibuffer."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (region-or-error buffer)
      (let ((version (tuatara.buffer:buffer-version buffer)))
        (choose-encryption "the selection" '()
                           (lambda (encryption)
                             (let* ((passphrase (gpg-passphrase-for "the selection" :encrypting encryption))
                                    (plain (tuatara.text:rope-octets (tuatara.text:version-rope version)
                                                                     :start start
                                                                     :end end)))
                               (run-gpg-work "encrypting the selection"
                                             (cons "--armor" (gpg-encrypt-arguments encryption))
                                             (lambda (output statuses ok said)
                                               (if ok
                                                   (when (replace-region-when-unchanged buffer start end version
                                                                                        output "Encrypting")
                                                     (message "The selection is encrypted in place"))
                                                   (message "Not encrypted: ~a" (gpg-failure-words statuses said))))
                                             :input plain :passphrase passphrase))))))))

(defun armoured-block-at (rope kind begin limit)
  "The armoured block of KIND (MESSAGE, SIGNED MESSAGE) whose opening line is
at BEGIN in ROPE, as (START . END), the line end after it with it, its closing
line ending at or before LIMIT; NIL when there is none, or the search for it
was stopped (OCTETS-STOPPER)."
  (multiple-value-bind (stop after)
      (tuatara.syntax:search-forward rope (format nil "-----END PGP ~a-----"
                                                  (if (string= kind "SIGNED MESSAGE") "SIGNATURE" kind))
                                     :start begin :end limit :case-fold nil :stop (octets-stopper))
    (when (and stop (integerp after))
      (cons begin (if (and (< after (tuatara.text:rope-octet-length rope))
                           (= 10 (tuatara.text:rope-octet rope after)))
                      (1+ after)
                      after)))))

(defun armoured-block (buffer kind)
  "The armoured block of KIND (MESSAGE, SIGNED MESSAGE) to work on in BUFFER,
as two values, where it starts and ends; NIL when there is none. With text
selected, the first in the selection; else the one point is in, or else the
one nearest to point, before or after it. Looked for in BUFFER's text from
point outward, a search \\[keyboard-quit] stops, only the block found taken."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-octet-length rope))
         (opening (format nil "-----BEGIN PGP ~a-----" kind))
         (stop (octets-stopper)))
    (flet ((opening-after (from end) (tuatara.syntax:search-forward rope opening :start from :end end :case-fold nil
                                                                               :stop stop)))
      (let ((found
              (if (region-active-p buffer)
                  (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
                    (let ((begin (opening-after start end)))
                      (and (integerp begin) (armoured-block-at rope kind begin end))))
                  (let* ((point (tuatara.buffer:buffer-point buffer))
                         (before (tuatara.syntax:search-backward rope opening :start (min size (1+ point))
                                                                              :case-fold nil :stop stop))
                         (behind (and (integerp before) (armoured-block-at rope kind before size)))
                         (after (and (not (and behind (<= point (cdr behind))))
                                     (opening-after (if behind (cdr behind) point) size)))
                         (ahead (and (integerp after) (armoured-block-at rope kind after size))))
                    (cond ((and behind (<= point (cdr behind))) behind)
                          ((and behind ahead) (if (<= (- point (cdr behind)) (- (car ahead) point)) behind ahead))
                          (t (or behind ahead)))))))
        (and found (values (car found) (cdr found)))))))

(define-command gpg-decrypt-region ()
  "Decrypt in place the armoured block (-----BEGIN PGP MESSAGE-----) in the text
selected, or without a selection the one point is in, else the one nearest to
point. The buffer's text is then kept from the disk as an encrypted file's is
\(no auto-save, no clipboard but on request), and a save asks before writing the
decrypted text to the file unencrypted."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (armoured-block buffer "MESSAGE")
      (if (null start)
          (message "No encrypted block (-----BEGIN PGP MESSAGE-----) here")
          (let ((version (tuatara.buffer:buffer-version buffer))
                (cipher (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end)))
            (run-gpg-work "decrypting the block" (list "--decrypt" "--output" "-")
                          (lambda (output statuses ok said)
                            (if (and ok (gpg-status-p statuses "DECRYPTION_OKAY"))
                                ;; kept from the disk before the decrypted text goes in
                                (when (replace-region-when-unchanged buffer start end version output "Decrypting"
                                                                     :before (lambda () (keep-from-disk buffer)))
                                  (message "Decrypted in place: not auto-saved, and a save asks before ~
                                            writing it unencrypted"))
                                (message "Not decrypted: ~a" (gpg-failure-words statuses said)))
                            (fill output 0))
                          :input cipher
                          :passphrase (block-passphrase cipher)))))))

(defun block-passphrase (cipher)
  "With the minibuffer asking for passphrases (GPG-MINIBUFFER-P), the
passphrase for CIPHER, the octets of an armoured encrypted block, asked for:
its own when a passphrase opens it (OPENPGP-PASSPHRASE-OPENS-P),
else your key's (GPG-OWN-PASSPHRASE). NIL when gpg-agent's pinentry asks."
  (when (gpg-minibuffer-p)
    (if (openpgp-passphrase-opens-p cipher)
        (gpg-passphrase-for "the block")
        (gpg-own-passphrase "the block"))))

(define-command gpg-sign-region ()
  "Clear-sign the text selected in place (-----BEGIN PGP SIGNED MESSAGE-----), with your key."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (region-or-error buffer)
      (let ((plain (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end))
            (version (tuatara.buffer:buffer-version buffer)))
        (run-gpg-work "signing the selection" (list "--clearsign" "--output" "-")
                      (lambda (output statuses ok said)
                        (if (and ok (gpg-status-p statuses "SIG_CREATED"))
                            (when (replace-region-when-unchanged buffer start end version output "Signing")
                              (message "The selection is signed in place"))
                            (message "Not signed: ~a" (gpg-failure-words statuses said))))
                      :input plain :passphrase (gpg-own-passphrase "signing the selection"))))))

(define-command gpg-verify-region ()
  "Check the signature of the clear-signed block (-----BEGIN PGP SIGNED
MESSAGE-----) in the text selected, or without a selection the one point is
in, else the one nearest to point: who signed it, when, and how far that key
is trusted."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (armoured-block buffer "SIGNED MESSAGE")
      (if (null start)
          (message "No signed block (-----BEGIN PGP SIGNED MESSAGE-----) here")
          (run-gpg-work "verifying the block" (list "--verify" "-")
                        (lambda (output statuses ok said)
                          (declare (ignore output))
                          (message "~a" (signature-words statuses said ok)))
                        :input (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end))))))

;;; The keys

(define-major-mode gpg-keys-mode sectioned-list-mode
  "Your GnuPG keys and those you have of others, in two sections, one a line:
ID, validity, what it can do (e encrypt, s sign, c certify), expiry, name.
\\<gpg-keys-mode>\\[list-visit] shows the details of the key at the cursor, on
a heading folds or unfolds its section, \\[list-fold-heading] folds it.
\\[list-mark-entry] marks the key at the cursor and moves down,
\\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks them all,
\\[list-toggle-marks] swaps marked and unmarked. \\[gpg-keys-import] imports keys from a file, \\[gpg-keys-export]
exports the keys marked (or the one at the cursor) to a file,
\\[gpg-keys-insert] puts them in the buffer you came from,
\\[list-delete-marked] deletes them, after asking. \\[revert-buffer] lists them
again."
  :lighter "Keys" :interactive nil)

(defvar *gpg-keys-keymap* (make-keymap :name "The list of GnuPG keys, before the modal keys")
  "Keys of the list of GnuPG keys (\\[gpg-keys-manager]), before the modal
keys.")

(defparameter *gpg-keys-columns* '((" M" :width 2) ("ID" :width 16) ("Validity" :width 9) ("Can" :width 4)
                                   ("Expires" :width 10) ("Name"))
  "The columns of the list of keys, as TABLE-HEADING-PIECES takes them: the
mark, the key's ID, how far it is trusted, what it can do (e encrypt, s sign,
c certify), when it expires, and its name.")

(defun gpg-keys-show (buffer &optional (point :keep))
  "Fill BUFFER, the list of keys, as gpg has them now, listed in the
background, point on line POINT or kept."
  (gpg-in-background "listing keys" (lambda (cancel) (gpg-keys :all t :cancel cancel))
                     (lambda (keys) (when (live-buffer-p buffer) (gpg-keys-draw buffer keys point)))
                     :asker :agent :buffer buffer :quiet t))

(defun gpg-keys-draw (buffer keys &optional (point :keep))
  "Make BUFFER, the list of keys, show KEYS (GPG-KEYS), yours in a section
before those of others, point on line POINT or kept."
  (render-table-list
   buffer *gpg-keys-columns*
   (group-sections keys (lambda (key) (if (getf key :secret) :yours :others)) :first :yours)
   (lambda (key mark section)
     (declare (ignore section))
     (list (list-mark-sign mark)
           (cons (getf key :id) (list-mark-face mark))
           (getf key :validity)
           (or (getf key :capabilities) "")
           (let ((e (getf key :expires))) (if e (format-time "%F" e) ""))
           (escape-controls (or (getf key :name) ""))))
   :sections t :section-title (lambda (section) (if (eq section :yours) "Yours" "Others'"))
   :marked-by (lambda (key section) (declare (ignore section)) (gpg-key-id key))
   :after (list (format nil "~%   ~[No keys~;1 key~:;~:*~d keys~], ~d yours~%" (length keys)
                        (count-if (lambda (key) (getf key :secret)) keys)))
   :point point
   :refill (lambda (b) (gpg-keys-show b :keep))
   :redraw (lambda (b) (gpg-keys-draw b keys :keep))))

(defun gpg-key-id (key)
  "The ID of KEY, one of GPG-KEYS: what a mark on it in the list of keys is kept by."
  (getf key :id))

(define-command gpg-keys-manager ()
  "List your GnuPG keys and those you have of others, in a section each: there
\\<gpg-keys-mode>\\[list-mark-entry] marks one, \\[list-visit] shows its
details, \\[gpg-keys-import] imports keys from a file, \\[gpg-keys-export]
exports, \\[gpg-keys-insert] inserts and \\[list-delete-marked] deletes those
marked."
  (let ((from (current-buffer))
        (buffer (buffer-in-mode "*GnuPG keys*" 'gpg-keys-mode)))
    (unless (eq from buffer) (setf (tuatara.buffer:buffer-local buffer 'gpg-keys-from) from))
    (setf (buffer-override-keymap buffer) *gpg-keys-keymap*)
    (gpg-keys-show buffer 3)
    (show-buffer buffer)))

(defun gpg-keys-chosen ()
  "The keys the list's command works on: those marked, else the one at the cursor."
  (list-entries-of (current-buffer) (list-acted-on (current-buffer))))

(defun key-details-text (octets)
  "gpg's details of a key, OCTETS, as text to show, the control characters in
each line written as the editor shows them (ESCAPE-CONTROLS)."
  (format nil "~{~a~%~}" (mapcar #'escape-controls
                                 (uiop:split-string (string-right-trim '(#\Newline)
                                                                       (tuatara.text:lenient-string octets))
                                                    :separator '(#\Newline)))))

(defmethod visit-entry ((mode gpg-keys-mode) list key other)
  "Show the details of KEY (GPG-SHOW-KEY-DETAILS)."
  (declare (ignore list other))
  (gpg-show-key-details key))

(defun gpg-show-key-details (key)
  "Show the details of KEY, one of GPG-KEYS, as help: its fingerprint, its
names, its subkeys and what they can do, when they expire; gpg asked in the
background."
  (run-gpg-work (format nil "showing the details of ~a" (getf key :id))
                (list "--list-keys" "--with-subkey-fingerprints" "--fingerprint" (getf key :fingerprint))
                (lambda (output statuses ok said)
                  (declare (ignore ok))
                  (if (plusp (length output))
                      (show-help (format nil "Key ~a~%~%~a" (getf key :id) (key-details-text output)))
                      (message "No details of ~a: ~a" (getf key :id) (gpg-failure-words statuses said))))
                :asker :agent :quiet t))

(define-command gpg-keys-import ()
  "Import the keys in a file asked for (armoured or not) into your keyring; say what was imported."
  (let ((path (read-native-path "Import keys from: " :kind :file)))
    (when path
      (run-gpg-work (format nil "importing keys from ~a" (tuatara.io:native-file-name path)) (list "--import")
                    #'after-key-import :input-path path))))

(define-command gpg-keys-import-region ()
  "Import the keys in the text selected (an armoured -----BEGIN PGP PUBLIC KEY BLOCK-----) into your keyring."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (region-or-error buffer)
      (run-gpg-work "importing keys from the selection" (list "--import")
                    #'after-key-import
                    :input (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end)))))

(defun after-key-import (output statuses ok said)
  "Say what an import of keys did (IMPORT-WORDS), from what gpg gave, OUTPUT,
STATUSES, OK and SAID, and list the keys again where they are shown."
  (declare (ignore output ok))
  (message "~a" (import-words statuses said))
  (refill-open-list "*GnuPG keys*" 'gpg-keys-mode))

(defun import-words (statuses said)
  "What an import did, as words, from gpg's STATUSES."
  (let ((result (gpg-status-p statuses "IMPORT_RES")))
    (if result
        (let ((n (mapcar (lambda (w) (or (whole-number w) 0)) (rest (uiop:split-string result :separator '(#\Space))))))
          ;; IMPORT_RES count no-user-id imported imported-rsa unchanged uids subkeys signatures revocations
          ;; secrets-read secrets-imported ...
          (format nil "Keys imported: ~d new, ~d unchanged~@[, ~d secret~]" (nth 2 n) (nth 4 n)
                  (and (plusp (nth 10 n)) (nth 10 n))))
        (format nil "Nothing imported: ~a" (gpg-failure-words statuses said)))))

(defun with-exported-keys (keys then)
  "Call THEN with the public KEYS armoured, as octets, gpg run in the background."
  (run-gpg-work (format nil "exporting ~d key~:p" (length keys))
                (append (list "--armor" "--export") (mapcar (lambda (k) (getf k :fingerprint)) keys))
                (lambda (output statuses ok said)
                  (if (and ok (plusp (length output)))
                      (funcall then output)
                      (message "No keys exported: ~a" (gpg-failure-words statuses said))))
                :asker :agent))

(define-command gpg-keys-export ()
  "Export the public part of the keys marked (or the one at the cursor), armoured, to a file asked for."
  (let ((keys (gpg-keys-chosen)))
    (if (null keys)
        (message "No key on this line")
        (let ((path (read-native-path "Export the keys to: " :kind :any :writing t)))
          (when (and path (gpg-target-free-p path))
            (with-exported-keys keys
              (lambda (octets)
                (write-gpg-output path octets)
                (message "~d key~:p exported to ~a" (length keys) (abbreviated-path path)))))))))

(define-command gpg-keys-insert ()
  "Put the public part of the keys marked (or the one at the cursor), armoured,
at point in the buffer the list was opened from."
  (let ((keys (gpg-keys-chosen))
        (target (tuatara.buffer:buffer-local (current-buffer) 'gpg-keys-from)))
    (cond ((null keys) (message "No key on this line"))
          ((not (live-buffer-p target)) (message "The buffer the list was opened from is gone"))
          (t (with-exported-keys keys
               (lambda (octets)
                 (if (not (live-buffer-p target))
                     (message "The buffer the list was opened from is gone")
                     (progn (tuatara.buffer:buffer-insert target (tuatara.buffer:buffer-point target) octets)
                            (show-buffer target)
                            (message "~d key~:p inserted" (length keys))))))))))

(defmethod list-entry-noun ((mode gpg-keys-mode))
  "A key."
  "key")

(defmethod delete-entries ((mode gpg-keys-mode) list ids mark)
  "Delete the keys whose IDs are IDS from your keyring, after asking; a key of
yours with its secret part, asked twice."
  (declare (ignore mark))
  (let ((keys (list-entries-of list ids)))
    (cond ((null keys) (message "No key on this line"))
          ((not (yes-or-no-p* (format nil "Delete ~d key~:p from your keyring?" (length keys))
                              :details (format nil "~{~a~^~%~}"
                                               (mapcar (lambda (k) (escape-controls (or (getf k :name) (getf k :id))))
                                                       keys))))
           (message "Nothing deleted"))
          ((and (some (lambda (k) (getf k :secret)) keys)
                (not (yes-or-no-p* "Delete your own keys too?"
                                   :details (format nil "Some are your own keys: their secret parts go too, and ~
                                                         what was encrypted to them cannot be read again."))))
           (message "Nothing deleted"))
          (t (gpg-in-background (format nil "deleting ~d key~:p" (length keys))
                                (lambda (cancel) (delete-gpg-keys keys cancel))
                                (lambda (deleted failure)
                                  (when (live-buffer-p list) (gpg-keys-show list))
                                  (if failure
                                      (message "~d of ~d keys deleted: ~a" deleted (length keys) failure)
                                      (message "~d key~:p deleted" deleted)))
                                :asker :agent :buffer list)))))

(defun delete-gpg-keys (keys cancel)
  "Delete KEYS, of GPG-KEYS, from your keyring one after another, a key of
yours with its secret part, until gpg fails or CANCEL, a cons, has a true car.
Two values: how many were deleted, and NIL or why the first not deleted was
not, in words."
  (let ((deleted 0))
    (dolist (key keys (values deleted nil))
      (when (car cancel) (return (values deleted "stopped")))
      (multiple-value-bind (output statuses ok said)
          (gpg-run (list "--batch" "--yes" (if (getf key :secret) "--delete-secret-and-public-key" "--delete-keys")
                         (getf key :fingerprint))
                   :asker :agent :cancel cancel)
        (declare (ignore output))
        (unless ok
          (return (values deleted (format nil "~a: ~a" (or (getf key :name) (getf key :id))
                                          (gpg-failure-words statuses said)))))
        (incf deleted)))))

(defun install-gpg-commands ()
  "Bind the keys of the list of GnuPG keys; its keys have no mark for deletion."
  (entry-list-keys 'gpg-keys-mode "the list of GnuPG keys (\\[gpg-keys-manager])")
  (define-key (mode-keymap 'gpg-keys-mode) "d" :undefined)
  (define-keys *gpg-keys-keymap* '(("i" gpg-keys-import) ("x" gpg-keys-export)
                                   ("I" gpg-keys-insert) ("D" list-delete-marked))))
