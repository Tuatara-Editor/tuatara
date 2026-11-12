;;;; test/encrypted.lisp --- Files encrypted with GnuPG: opened decrypted, saved encrypted, their text never on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test encrypted
  :description "A .gpg file opens decrypted (to a key, or with a passphrase asked in the minibuffer and
shown as stars) in its text's mode, and saves encrypted to the same, only the encrypted octets on disk.
A new one asks whom to encrypt it to in the list of keys (m marks, RET saves, nothing marked does
nothing). A safe visit never decrypts. Its text is never auto-saved, kept in the session, written to a
scratch file, given to the clipboard unasked, nor exported unasked. No core dump while one is open.")

(defun gpg-test-key ()
  "The ID of the tests' own key, made the first time in the tests' own keyring (GNUPGHOME), with no passphrase."
  (let ((keys (tuatara.defaults::gpg-keys)))
    (unless keys
      (let ((tuatara-user:*gpg-passphrase-from* :minibuffer))
        (tuatara.defaults::gpg-run (list "--quick-gen-key" "Kea Taranaki <kea@example.org>" "default" "default" "never")
                                   :passphrase ""))
      (setf keys (tuatara.defaults::gpg-keys)))
    (getf (first keys) :id)))

(defun gpg-test-file (path text &key key passphrase)
  "Write TEXT encrypted to KEY, or with PASSPHRASE, as the file at PATH; return PATH as a string."
  (let ((tuatara-user:*gpg-passphrase-from* :minibuffer))
    (let ((octets (tuatara.defaults::gpg-run (tuatara.defaults::gpg-encrypt-arguments (list :keys (and key (list key)) :symmetric (and passphrase t)))
                                             :input (tuatara.text:string-octets text) :passphrase passphrase)))
      (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)))
  (namestring path))

(defun gpg-test-plain (path &key passphrase)
  "The text of the encrypted file at PATH, decrypted with the tests' key or PASSPHRASE."
  (let ((tuatara-user:*gpg-passphrase-from* :minibuffer))
    (let ((octets (tuatara.defaults::gpg-decrypt (alexandria:read-file-into-byte-vector path)
                                                 :passphrase (and passphrase (copy-seq passphrase)))))
      (and octets (sb-ext:octets-to-string octets :external-format :utf-8)))))

(defun dumpable-p ()
  "True when the system would keep a core dump of this process (PR_GET_DUMPABLE)."
  (= 1 (sb-alien:alien-funcall (sb-alien:extern-alien "prctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                                        sb-alien:unsigned-long
                                                                        sb-alien:unsigned-long
                                                                        sb-alien:unsigned-long))
                               3 0 0 0 0)))

(defun decrypting-p ()
  "True while a file is being decrypted or a buffer saved."
  (some (lambda (job) (or (tuatara.defaults::decrypt-job-p job) (tuatara.io::save-job-p job))) tuatara.runtime:*jobs*))

(define-test (encrypted to-a-key)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (file (gpg-test-file (merge-pathnames "notes.org.gpg" dir)
                                    (format nil "* Secret plans~%secret words~%")
                                    :key key))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*auto-save* t)
               (tuatara-user:*auto-save-idle* 0.1)
               (tuatara-user:*auto-save-directory* (namestring (merge-pathnames "copies/" dir)))
               (tuatara-user:*gpg-passphrase-from* :minibuffer))
          (true key)
          (false (search "secret words" (alexandria:read-file-into-string file :external-format :latin-1)))
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                       (list :until (lambda () (not (decrypting-p))))
                                       "M-> m o r e SPC w o r d s" (list :wait 0.4)
                                       "C-x C-s" (list :until (lambda () (not (decrypting-p))))))
            (declare (ignore editor))
            (let ((buffer (tuatara.buffer:find-buffer "notes.org.gpg")))
              (true buffer)
              (when buffer
                (true (search "secret words" (tuatara.buffer:buffer-string buffer)))
                (true (tuatara.command::buffer-mode-named-p buffer 'tuatara.defaults::org-mode))
                ;; encrypted to the tests' key, by its encryption subkey
                (true (getf (tuatara.buffer:buffer-local buffer :encrypted) :keys))
                (false (tuatara.buffer:buffer-modified-p buffer))
                ;; kept from the disk: no auto-saved copy, no session entry, no scratch file, no core dump
                (false (and (probe-file (merge-pathnames "copies/" dir))
                            (uiop:directory-files (merge-pathnames "copies/" dir))))
                (false (tuatara.defaults::session-entry buffer))
                (false (tuatara.io:buffer-scratch-directory buffer))
                (fail (tuatara.io:open-scratch-file nil))
                (false (dumpable-p))
                (tuatara.buffer:kill-buffer buffer)))
            (true (tuatara.frontend.fake:fake-frame-showing f "notes.org.gpg decrypted; a save encrypts it to Kea Taranaki <kea@example.org>")))
          ;; saved encrypted to the same key: only encrypted octets on disk
          (let ((raw (alexandria:read-file-into-string file :external-format :latin-1)))
            (false (search "words" raw)))
          (is string= (format nil "* Secret plans~%secret words~%more words") (gpg-test-plain file))
          (true (dumpable-p))))))

(define-test (encrypted with-a-passphrase)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((file (gpg-test-file (merge-pathnames "diary.txt.gpg" dir) "dear diary" :passphrase "kea pass"))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*gpg-passphrase-from* :minibuffer))
          ;; asked before it is opened, shown as stars; saved asks twice
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "k e a SPC p a s s"
                                       (list :wait 0.2) "RET" (list :until (lambda () (not (decrypting-p))))
                                       "M-> SPC t w o" "C-x C-s n e w SPC p a s s RET n e w SPC p a s s RET"
                                       (list :until (lambda () (not (decrypting-p))))))
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "Passphrase for diary.txt.gpg: ********"))
            (false (tuatara.frontend.fake:fake-frame-showing f "kea pass"))
            (true (tuatara.frontend.fake:fake-frame-showing f "diary.txt.gpg decrypted; a save encrypts it with a passphrase"))
            (false (tuatara.frontend.fake:fake-frame-showing f "Changed on disk"))
            (let ((buffer (tuatara.buffer:find-buffer "diary.txt.gpg")))
              (true buffer)
              (when buffer
                (true (getf (tuatara.buffer:buffer-local buffer :encrypted) :symmetric))
                (tuatara.buffer:kill-buffer buffer))))
          (is string= "dear diary two" (gpg-test-plain file :passphrase "new pass"))
          (false (gpg-test-plain file :passphrase "kea pass"))
          ;; a wrong passphrase: said, nothing shown
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "w r o n g RET"
                                       (list :until (lambda () (not (decrypting-p))))))
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "the passphrase was not right"))
            (let ((buffer (tuatara.buffer:find-buffer "diary.txt.gpg")))
              (when buffer (tuatara.buffer:kill-buffer buffer))))))))

(define-test (encrypted new-file-and-safe-visit)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (file (namestring (merge-pathnames "fresh.md.gpg" dir)))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*gpg-passphrase-from* :minibuffer))
          ;; a new one: asked in the minibuffer how to encrypt it; nothing chosen does not save it, a key chosen does
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                       "f i r s t SPC n o t e"
                                       "C-x C-s"
                                       (list :wait 0.2)
                                       "RET"
                                       "C-x C-s"
                                       "C-n C-n RET RET"
                                       (list :until (lambda () (not (decrypting-p)))))
                                 :width 140)
            (declare (ignore editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "Encrypt fresh.md.gpg to (keys, a passphrase; commas between):"))
            (true (tuatara.frontend.fake:fake-frame-showing f "asked for as it is saved"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Kea Taranaki <kea@example.org>"))
            (true (tuatara.frontend.fake:fake-frame-showing f "nothing chosen to encrypt it to"))
            (false (tuatara.buffer:find-buffer "*Encryption keys*"))
            (let ((buffer (tuatara.buffer:find-buffer "fresh.md.gpg")))
              (when buffer (tuatara.buffer:kill-buffer buffer))))
          (true (probe-file file))
          (is string= "first note" (gpg-test-plain file))
          (true key)
          ;; opened safely: never decrypted, its octets shown
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed file))))
            (declare (ignore f editor))
            (let ((buffer (tuatara.buffer:find-buffer "fresh.md.gpg")))
              (true buffer)
              (when buffer
                (true (tuatara.io:safe-visit-p buffer))
                (false (search "first note" (tuatara.buffer:buffer-string buffer)))
                (false (tuatara.buffer:buffer-local buffer :encrypted))
                (tuatara.buffer:kill-buffer buffer))))))))

(define-test (encrypted minibuffer-secret)
  ;; a secret read in the minibuffer shows as stars and its keys go to no log
  (is string= "***" (let ((tuatara.command::*minibuffer-hidden* t))
                      (let ((editor (tuatara.command::make-editor (tuatara.frontend.fake:make-fake-frontend))))
                        (setf (tuatara.command:editor-minibuffer-text editor) "abc")
                        (tuatara.command::minibuffer-shown-text editor))))
  (let ((secret (copy-seq "secret")))
    (tuatara.command:wipe-string secret)
    (false (find #\s secret)))
  ;; each key typed or deleted wipes the text it replaces: no part of the passphrase stays behind
  (let* ((tuatara.command::*minibuffer-hidden* t)
         (tuatara.command:*editor* (tuatara.command::make-editor (tuatara.frontend.fake:make-fake-frontend)))
         (editor tuatara.command:*editor*))
    (setf (tuatara.command:editor-minibuffer-text editor) (copy-seq "")
          (tuatara.command::editor-minibuffer-point editor) 0)
    (let ((earlier '()))
      (loop for c across "hunter"
            do (push (tuatara.command:editor-minibuffer-text editor) earlier)
               (tuatara.command:minibuffer-insert (string c)))
      (push (tuatara.command:editor-minibuffer-text editor) earlier)
      (tuatara.command::minibuffer-backward-delete-char)
      (push (tuatara.command:editor-minibuffer-text editor) earlier)
      (tuatara.command::minibuffer-backward-kill-word)
      (is string= "" (tuatara.command:editor-minibuffer-text editor))
      (true (every (lambda (text) (every (lambda (c) (char= c #\Nul)) text)) earlier)))))

(define-test (encrypted who-asks)
  ;; gpg-agent's pinentry where it can ask (its window, the terminal lent it), the minibuffer only else
  (let ((tuatara.command:*editor* (tuatara.command::make-editor (tuatara.frontend.fake:make-fake-frontend))))
    (let ((tuatara-user:*gpg-passphrase-from* :agent)) (is eq :agent (tuatara.defaults::gpg-asker)))
    (let ((tuatara-user:*gpg-passphrase-from* :minibuffer)) (is eq :minibuffer (tuatara.defaults::gpg-asker)))
    ;; a frontend that is no terminal has none to lend
    (let ((tuatara-user:*gpg-passphrase-from* :terminal)) (is eq :minibuffer (tuatara.defaults::gpg-asker)))
    (let ((tuatara-user:*gpg-passphrase-from* :auto))
      (with-environment-variable ("DISPLAY" nil)
        (with-environment-variable ("WAYLAND_DISPLAY" nil)
          (is eq :minibuffer (tuatara.defaults::gpg-asker))
          (sb-posix:setenv "WAYLAND_DISPLAY" "wayland-0" 1)
          (is eq :agent (tuatara.defaults::gpg-asker)))))))

(define-test (encrypted files-regions-and-keys)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (plain (namestring (merge-pathnames "report.txt" dir)))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*gpg-passphrase-from* :minibuffer))
          (true key)
          (alexandria:write-string-into-file "the findings" plain)
          (flet ((typed (command) (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string command)))
                 (settle ()
                   (list :until (lambda () (not (some #'tuatara.defaults::gpg-job-p tuatara.runtime:*jobs*))))))
            ;; a file encrypted (to the key marked), signed (detached) and verified; the original untouched
            (multiple-value-bind (f editor)
                (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed plain))
                                         (typed "gpg-encrypt-file") "C-n C-n RET RET" (settle) "n RET"
                                         (typed "gpg-sign-file") "RET RET" (settle)))
              (declare (ignore editor))
              (true (tuatara.frontend.fake:fake-frame-showing f "Encrypted as")))
            (is string= "the findings" (gpg-test-plain (concatenate 'string plain ".gpg")))
            (true (probe-file (concatenate 'string plain ".sig")))
            (let ((buffer (tuatara.buffer:find-buffer "report.txt"))) (when buffer (tuatara.buffer:kill-buffer buffer)))
            (let ((sig (namestring (concatenate 'string plain ".sig"))))
              (multiple-value-bind (f editor)
                  (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed sig))
                                           (typed "gpg-verify-file")
                                           (settle)))
                (declare (ignore editor))
                (true (tuatara.frontend.fake:fake-frame-showing f "Good signature by Kea Taranaki"))
                (let ((buffer (tuatara.buffer:find-buffer "report.txt.sig")))
                  (when buffer (tuatara.buffer:kill-buffer buffer))))
              ;; the file changed after it was signed: said
              (alexandria:write-string-into-file "the findings, changed" plain :if-exists :supersede)
              (multiple-value-bind (f editor)
                  (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed sig))
                                           (typed "gpg-verify-file")
                                           (settle)))
                (declare (ignore editor))
                (true (tuatara.frontend.fake:fake-frame-showing f "BAD signature"))
                (let ((buffer (tuatara.buffer:find-buffer "report.txt.sig")))
                  (when buffer (tuatara.buffer:kill-buffer buffer)))))
            ;; decrypted to a file of its own, for you alone, after asking
            (let ((back (namestring (merge-pathnames "back.txt" dir))))
              (auto-save-session (list (format nil
                                               "C-x C-f C-a C-k ~a RET"
                                               (rimu-typed (concatenate 'string plain ".gpg")))
                                       (list :until (lambda () (not (decrypting-p))))
                                       (typed "gpg-decrypt-file")
                                       (format nil "C-a C-k ~a RET y RET" (rimu-typed back))
                                       (settle)))
              (is string= "the findings" (alexandria:read-file-into-string back))
              (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat back))))
              (let ((buffer (tuatara.buffer:find-buffer "report.txt.gpg")))
                (when buffer (tuatara.buffer:kill-buffer buffer))))
            ;; the region: encrypted in place as an armoured block, decrypted back, the buffer then kept from the disk
            (multiple-value-bind (f editor)
                (auto-save-session (list "C-x b s c r a t c h - r e g i o n RET" "s e c r e t SPC l i n e" "C-x h"
                                         (typed "gpg-encrypt-region") "C-n C-n RET RET" (settle)))
              (declare (ignore f editor))
              (let ((buffer (tuatara.buffer:find-buffer "scratch-region")))
                (true (and buffer (search "-----BEGIN PGP MESSAGE-----" (tuatara.buffer:buffer-string buffer))))
                (false (and buffer (search "secret line" (tuatara.buffer:buffer-string buffer))))))
            (auto-save-session (list "C-x b s c r a t c h - r e g i o n RET"
                                 (typed "gpg-decrypt-region")
                                 "RET"
                                 (settle)))
            (let ((buffer (tuatara.buffer:find-buffer "scratch-region")))
              (is string= "secret line" (and buffer (tuatara.buffer:buffer-string buffer)))
              (true (and buffer (tuatara.buffer:buffer-local buffer :plaintext)))
              (when buffer (tuatara.buffer:kill-buffer buffer)))
            ;; a line among others: the selection ends with the text it held, and the
            ;; block is decrypted from a line away from it
            (let ((tuatara.defaults::*modal-editing* t))
              (tuatara.buffer:make-buffer "scratch-region" :text (format nil "before~%secret~%after~%"))
              (auto-save-session (list "C-x b s c r a t c h - r e g i o n RET"
                                       "g e k V" (typed "gpg-encrypt-region") "C-n C-n RET RET" (settle)))
              (let ((buffer (tuatara.buffer:find-buffer "scratch-region")))
                (false (tuatara.buffer:buffer-mark-active-p buffer))
                (false (search "after" (tuatara.buffer:buffer-string buffer))))
              (auto-save-session (list "C-x b s c r a t c h - r e g i o n RET"
                                       "g g"
                                       (typed "gpg-decrypt-region")
                                       "RET"
                                       (settle)))
              (let ((buffer (tuatara.buffer:find-buffer "scratch-region")))
                (is string= (format nil "before~%secret~%after~%") (and buffer (tuatara.buffer:buffer-string buffer)))
                (when buffer (tuatara.buffer:kill-buffer buffer))))
            ;; the keys listed, yours marked as such; exported to a file
            (let ((exported (namestring (merge-pathnames "kea.asc" dir))))
              (multiple-value-bind (f editor)
                  (auto-save-session (list (typed "gpg-list-keys")
                                           (settle)
                                           "x"
                                           (format nil "C-a C-k ~a C-j" (rimu-typed exported))
                                           (settle)) :width 140)
                (declare (ignore editor))
                (true (tuatara.frontend.fake:fake-frame-showing f "Kea Taranaki <kea@example.org>"))
                (true (tuatara.frontend.fake:fake-frame-showing f "yours")))
              (true (search "-----BEGIN PGP PUBLIC KEY BLOCK-----" (alexandria:read-file-into-string exported)))
              ;; m marks the key on the line, u takes the mark off
              (true (find-if (lambda (line) (uiop:string-prefix-p ">" line))
                                             (uiop:split-string (tuatara.buffer:buffer-string
                                                                 (progn
                                                                   (auto-save-session (list (typed "gpg-list-keys")
                                                                                            (settle)
                                                                                            "m") :width 140)
                                                                   (tuatara.buffer:find-buffer "*GnuPG keys*")))
                                                                :separator '(#\Newline))))
              (auto-save-session (list (typed "gpg-list-keys") (settle) "u") :width 140)
              (false (find-if (lambda (line) (uiop:string-prefix-p ">" line))
                                              (uiop:split-string (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*GnuPG keys*"))
                                                                 :separator '(#\Newline))))
              ;; RET shows the key's details as help
              (auto-save-session (list (typed "gpg-list-keys") (settle) "RET" (settle)) :width 140)
              (let ((help (tuatara.buffer:find-buffer "*Help*")))
                (true (and help (search (format nil "Key ~a" key) (tuatara.buffer:buffer-string help))))
                (true (and help (search "Kea Taranaki" (tuatara.buffer:buffer-string help)))))
              (let ((buffer (tuatara.buffer:find-buffer "*GnuPG keys*")))
                (when buffer (tuatara.buffer:kill-buffer buffer)))))))))

(define-test (encrypted decrypted-in-place-saved-after-asking)
  ;; a file holding text decrypted in place is written in clear only when the question is
  ;; answered yes; no keeps the file as it was
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "mail.txt" dir)))
          (tuatara.defaults::*modal-editing* nil))
      (alexandria:write-string-into-file "on disk" file)
      (flet ((save (answer)
               (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                        (list :until (lambda () (tuatara.buffer:find-buffer "mail.txt")))
                                        "C-e SPC c l e a r"
                                        (list :until (lambda () (tuatara.defaults::keep-from-disk (tuatara.buffer:find-buffer "mail.txt")) t))
                                        "C-x C-s" (list :wait 0.2) (format nil "~a RET" answer)
                                        (list :until
                                              (lambda () (notany #'tuatara.io::save-job-p tuatara.runtime:*jobs*))))
                                  :width 140)))
        (let ((f (save "n")))
          (true (tuatara.frontend.fake:fake-frame-showing f "holds text decrypted in place; save it unencrypted?"))
          (true (tuatara.frontend.fake:fake-frame-showing f "not saved: it holds text decrypted in place"))
          (is string= "on disk" (alexandria:read-file-into-string file)))
        (save "y")
        (is string= "on disk clear clear" (alexandria:read-file-into-string file))
        (let ((buffer (tuatara.buffer:find-buffer "mail.txt"))) (when buffer (tuatara.buffer:kill-buffer buffer)))))))

(define-test (encrypted block-near-point)
  ;; with nothing selected, the armoured block point is in, else the nearest one
  (let* ((one (format nil "-----BEGIN PGP MESSAGE-----~%x~%-----END PGP MESSAGE-----~%"))
         (two (format nil "-----BEGIN PGP MESSAGE-----~%y~%-----END PGP MESSAGE-----~%"))
         (text (format nil "A~%~aB~%C~%D~%~aE" one two))
         (buffer (tuatara.buffer:make-buffer "armoured-blocks" :text text)))
    (unwind-protect
         (flet ((found-at (point)
                  (setf (tuatara.buffer:buffer-point buffer) point)
                  (multiple-value-bind (start end) (tuatara.defaults::armoured-block buffer "MESSAGE")
                    (and start (subseq text start end)))))
           (is string= one (found-at 0))
           (is string= two (found-at (+ (search two text) 5)))
           (is string= two (found-at (1+ (search (format nil "~%D~%") text))))
           (is string= one (found-at (1+ (search (format nil "~%B~%") text))))
           (is string= two (found-at (length text))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (encrypted in-rimu)
  ;; : e on a file of a list asks how to encrypt it; : v on one says what its signature is
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-rimu-tree (dir)
        (gpg-test-key)
        (let ((f (rimu-session dir (format nil "g f ~a RET : e" (rimu-typed "text.txt")) :width 120)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Encrypt text.txt to")))
        (let ((f (rimu-session dir (format nil "g f ~a RET : v" (rimu-typed "text.txt")) :width 160)))
          (true (tuatara.frontend.fake:fake-frame-showing f "text.txt: No signature checked")))
        (rimu-forget))))

(define-test (encrypted loopback-warned)
  ;; the minibuffer asking for gpg's loopback: the risks said in a window above it, gone once it has asked
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((file (gpg-test-file (merge-pathnames "w.txt.gpg" dir) "warned" :passphrase "w pass"))
              (tuatara.defaults::*modal-editing* nil)
              (tuatara-user:*gpg-passphrase-from* :minibuffer))
          (multiple-value-bind (f editor)
              (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "w SPC p a s s"
                                       (list :wait 0.2) "RET" (list :until (lambda () (not (decrypting-p)))))
                                 :height 40)
            (true (tuatara.frontend.fake:fake-frame-showing f "Warning: this passphrase goes through Tuatara"))
            (true (tuatara.frontend.fake:fake-frame-showing f "the garbage collector may have copied it"))
            (true (tuatara.frontend.fake:fake-frame-showing f "you chose it (*gpg-passphrase-from* :minibuffer)"))
            (false (tuatara.buffer:find-buffer "*Passphrase warning*"))
            (is = 1 (length (tuatara.command:editor-windows editor)))
            (let ((buffer (tuatara.buffer:find-buffer "w.txt.gpg")))
              (true (and buffer (search "warned" (tuatara.buffer:buffer-string buffer))))
              (when buffer (tuatara.buffer:kill-buffer buffer))))
          ;; turned off: the question alone
          (let ((tuatara-user:*gpg-loopback-warning* nil))
            (multiple-value-bind (f editor)
                (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "w SPC p a s s"
                                         (list :wait 0.2) "RET" (list :until (lambda () (not (decrypting-p)))))
                                   :height 40)
              (declare (ignore editor))
              (true (tuatara.frontend.fake:fake-frame-showing f "Passphrase for w.txt.gpg: "))
              (false (tuatara.frontend.fake:fake-frame-showing f "Warning: this passphrase goes through Tuatara"))
              (let ((buffer (tuatara.buffer:find-buffer "w.txt.gpg")))
                (true (and buffer (search "warned" (tuatara.buffer:buffer-string buffer))))
                (when buffer (tuatara.buffer:kill-buffer buffer)))))))))

(define-test (encrypted bounded-output)
  ;; a small file that decompresses to far more than it could hold is stopped, not read whole
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (let* ((stored (tuatara.defaults::gpg-run (list "--store" "--compress-algo" "zlib" "--output" "-")
                                                :input (make-array (* 4 1024 1024)
                                                                   :element-type '(unsigned-byte 8)
                                                                   :initial-element 0)
                                                :asker :agent))
             (tuatara.defaults::*gpg-least-output-limit* (* 1024 1024)))
        (true (< (length stored) (* 64 1024)))
        (multiple-value-bind (output statuses ok said)
            (tuatara.defaults::gpg-run (list "--decrypt" "--output" "-") :input stored :asker :agent)
          (declare (ignore statuses said))
          (false ok)
          (true (< (length output) (* 2 1024 1024)))))))

(define-test (encrypted safe-directories-never-decrypt)
  ;; a .gpg file in one of *safe-directories*, opened however, shows its encrypted octets: nothing is decrypted
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((file (gpg-test-file (merge-pathnames "evidence.txt.gpg" dir) "kept closed" :passphrase "e pass"))
              (tuatara.defaults::*modal-editing* nil)
              (tuatara-user:*safe-directories* (list (namestring dir)))
              (tuatara-user:*gpg-passphrase-from* :minibuffer))
          (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) (list :wait 0.3)))
          (let ((buffer (tuatara.buffer:find-buffer "evidence.txt.gpg")))
            (true buffer)
            (when buffer
              (true (tuatara.io:safe-visit-p buffer))
              (false (search "kept closed" (tuatara.buffer:buffer-string buffer)))
              (tuatara.buffer:kill-buffer buffer)))))))

(define-test (encrypted keys-asked-in-the-minibuffer)
  ;; how to encrypt is asked in the minibuffer, several at once: no buffer is left behind, C-g does nothing
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((plain (namestring (merge-pathnames "list.txt" dir)))
              (tuatara.defaults::*modal-editing* nil)
              (tuatara-user:*gpg-passphrase-from* :minibuffer))
          (gpg-test-key)
          (alexandria:write-string-into-file "words" plain)
          (auto-save-session (list (format nil
                                           "M-x g p g - e n c r y p t - f i l e RET C-a C-k ~a C-j"
                                           (rimu-typed plain)) (list :wait 0.2) "C-g"))
          (false (tuatara.buffer:find-buffer "*Encryption keys*"))
          (false (probe-file (concatenate 'string plain ".gpg")))
          ;; a passphrase and the key together
          (auto-save-session (list (format nil
                                           "M-x g p g - e n c r y p t - f i l e RET C-a C-k ~a C-j"
                                           (rimu-typed plain)) (list :wait 0.2)
                                   "C-n TAB , SPC C-n RET RET" "b o t h RET b o t h RET"
                                   (list :until
                                         (lambda () (not (some #'tuatara.defaults::gpg-job-p tuatara.runtime:*jobs*))))
                                   "n RET")
                             :width 140)
          (false (tuatara.buffer:find-buffer "*Encryption keys*"))
          (is string= "words" (gpg-test-plain (concatenate 'string plain ".gpg") :passphrase "both"))
          ;; to the key too
          (let ((tuatara-user:*gpg-passphrase-from* :minibuffer))
            (true (getf (nth-value 1 (tuatara.defaults::gpg-decrypt (alexandria:read-file-into-byte-vector (concatenate 'string plain ".gpg"))
                                                                     :passphrase (copy-seq "both")))
                        :keys)))))))

(define-test (encrypted octets-and-wiping)
  ;; every octet value goes through gpg and back as it was; text decrypted for a buffer killed meanwhile is wiped
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (let* ((all (coerce (loop for i below 512 collect (mod i 256)) '(simple-array (unsigned-byte 8) (*))))
             (stored (tuatara.defaults::gpg-run (list "--store" "--output" "-") :input all :asker :agent)))
        (true (plusp (length stored)))
        (is equalp all (tuatara.defaults::gpg-run (list "--decrypt" "--output" "-") :input stored :asker :agent))
        (let ((buffer (tuatara.buffer:make-buffer "gone.txt.gpg"))
              (text (tuatara.text:string-octets "secret words")))
          (tuatara.buffer:kill-buffer buffer)
          (tuatara.defaults::decrypted-in buffer "/nowhere/gone.txt.gpg" text (list :keys '()) nil)
          (true (every #'zerop text))))))

(define-test (encrypted files-given-in-blocks)
  ;; a file given to gpg goes through its pipe a block at a time, never read whole into memory
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((file (namestring (merge-pathnames "large.bin" dir)))
              (size (* 32 1024 1024)))
          (with-open-file (out file :direction :output :element-type '(unsigned-byte 8))
            (let ((block (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 7)))
              (dotimes (i (/ size 65536)) (write-sequence block out))))
          (let ((consed (loop repeat 3
                              minimize (let ((before (sb-ext:get-bytes-consed)))
                                         (multiple-value-bind (output statuses ok)
                                             (tuatara.defaults::gpg-run (list "--print-md" "SHA256")
                                                                        :input-path file
                                                                        :asker :agent)
                                           (declare (ignore statuses))
                                           (true ok)
                                           (true (plusp (length output))))
                                         (- (sb-ext:get-bytes-consed) before)))))
            (true (< consed (/ size 4)) "~:d octets made for a file of ~:d" consed size))))))

(define-test (encrypted secrets-kept-from-the-debug-log)
  ;; an error inside a decrypt with a passphrase: the debug log says that it failed, never the
  ;; passphrase or the text, in its words or in the calls under way; nor does a job on such a text
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((file (gpg-test-file (merge-pathnames "secret.txt.gpg" dir)
                                    "plain kiwi words"
                                    :passphrase "kea secret pass"))
               (tuatara-user:*gpg-passphrase-from* :minibuffer)
               (octets (alexandria:read-file-into-byte-vector file)))
          (flet ((reported ()
                   (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                         while report collect (princ-to-string (fourth report)))))
            (drain-debug-reports)
            (tuatara.runtime:set-debug t)
            (sb-int:encapsulate 'tuatara.defaults::gpg-status-p 'test-failing
                                (lambda (function &rest arguments)
                                  (declare (ignore function arguments))
                                  (error 'type-error
                                         :datum "kea secret pass plain kiwi words"
                                         :expected-type 'integer)))
            (unwind-protect
                 (progn
                   (false (ignore-errors
                           (tuatara.runtime:reporting-errors ("decrypting in a test")
                             (tuatara.defaults::decrypt-naming-keys octets :passphrase (copy-seq "kea secret pass")))))
                   (let ((texts (reported)))
                     (true (find-if (lambda (text) (search "decrypting in a test failed: a type-error" text)) texts)
                           "~s" texts)
                     (false (find-if (lambda (text) (or (search "kea secret" text) (search "kiwi" text))) texts)
                            "~s" texts)))
              (sb-int:unencapsulate 'tuatara.defaults::gpg-status-p 'test-failing))
            ;; a job on a buffer whose text is secret
            (let ((buffer (tuatara.buffer:make-buffer "*secret test*")))
              (setf (tuatara.io:buffer-plaintext-p buffer) t)
              (unwind-protect
                   (let ((job (tuatara.defaults::make-decrypt-job buffer "/tmp/x.txt.gpg" nil)))
                     (tuatara.runtime:start-job-thread job (lambda (job) (declare (ignore job))
                                                             (error "plain kiwi words")))
                     (loop repeat 500 while (tuatara.runtime:job-running-p job) do (sleep 0.01))
                     (let ((texts (reported)))
                       (true (find-if (lambda (text) (search "failed" text)) texts) "~s" texts)
                       (false (find-if (lambda (text) (search "kiwi" text)) texts) "~s" texts)))
                (tuatara.buffer:kill-buffer buffer)))
            ;; a decrypt job prints by its file's name alone
            (let ((job (tuatara.defaults::make-decrypt-job nil "/tmp/x.txt.gpg" "kea secret pass")))
              (false (search "kea" (prin1-to-string job))))
            (drain-debug-reports))))))

(define-test (encrypted decrypted-buffer-failures-kept-secret)
  ;; failures while a decrypted Org text is worked on away from the editor's thread (its outline),
  ;; taken in, and drawn (a header line, in a window not selected): said in the debug log by their
  ;; kind alone, never with the text or the calls under way
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (file (gpg-test-file (merge-pathnames "kiwi.org.gpg" dir)
                                    (format nil "* Heading~%plain kiwi words~%")
                                    :key key))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*gpg-passphrase-from* :minibuffer)
               (kiwi (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error 'type-error :datum "plain kiwi words" :expected-type 'integer))))
          (drain-debug-reports)
          (clrhash tuatara.runtime::*error-report-places*)
          ;; debug mode on in the editors the test starts
          (setf tuatara-user:*debug-mode* t)
          (tuatara.runtime:set-debug t)
          (sb-int:encapsulate 'tuatara.defaults::outline-entries 'test-failing (lambda (f &rest a) (apply kiwi f a)))
          (sb-int:encapsulate 'tuatara.defaults::decrypted-in 'test-failing
                              (lambda (f &rest arguments) (apply f arguments) (funcall kiwi)))
          (unwind-protect
               (progn
                 (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                          (list :until (lambda () (not (decrypting-p))))
                                          (list :wait 0.2)
                                          "M-g o" (list :wait 0.5) "C-g"
                                          (list :wait 0.2)))
                 (let ((buffer (tuatara.buffer:find-buffer "kiwi.org.gpg")))
                   (true buffer)
                   (when buffer
                     (setf (tuatara.display:buffer-header-line buffer) (lambda (window) (declare (ignore window))
                                                                          (funcall kiwi)))
                     ;; drawn in the window not selected
                     (auto-save-session (list "C-x 2 C-x o" (format nil "C-x b ~a RET" (rimu-typed "kiwi.org.gpg"))
                                              "C-x o" (list :wait 0.3) "C-n" (list :wait 0.2)))
                     (tuatara.buffer:kill-buffer buffer)))
                 ;; the editor shows its reports in *Debug* as they come: read from the debug log
                 (let ((texts (list (if (probe-file *test-debug-log*)
                                        (alexandria:read-file-into-string *test-debug-log*)
                                        ""))))
                   (true (find-if (lambda (text) (search "finding the headings failed: a type-error" text)) texts)
                         "~s" texts)
                   (true (find-if (lambda (text) (search "drawing a header line failed: a type-error" text)) texts)
                         "~s" texts)
                   (false (find-if (lambda (text) (search "kiwi words" text)) texts) "~s" texts)
                   ;; no calls under way after either
                   (dolist (said '("finding the headings failed" "drawing a header line failed"))
                     (let* ((text (first texts)) (at (search said text))
                            (next (and at (position #\Newline text :start at))))
                       (false (and next
                                   (search (format nil
                                                   "~%    (") text :start2 next :end2 (min (length text) (+ next 6))))
                              "~a: ~s" said text)))))
            (sb-int:unencapsulate 'tuatara.defaults::outline-entries 'test-failing)
            (sb-int:unencapsulate 'tuatara.defaults::decrypted-in 'test-failing)
            (drain-debug-reports))))))

(define-test (encrypted secret-kept-from-what-is-said-and-kept)
  ;; with a decrypted Org text: a window that cannot be drawn, a command failing with
  ;; *Backtrace* asked for, its parse failing and a job on it failing say the kind of failure
  ;; alone, on the screen, in *Backtrace*, the debug log and the job's words; and what is said
  ;; meanwhile is not kept in *Messages*
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (file (gpg-test-file (merge-pathnames "kea.org.gpg" dir)
                                    (format nil "* Heading~%plain kiwi words~%")
                                    :key key))
               (tuatara.defaults::*modal-editing* nil)
               (tuatara-user:*gpg-passphrase-from* :minibuffer)
               (tuatara.command:*debug-on-error* t)
               (kiwi (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error 'type-error :datum "plain kiwi words" :expected-type 'integer)))
               (drawn-wrong nil))
          (drain-debug-reports)
          (clrhash tuatara.runtime::*error-report-places*)
          (setf tuatara-user:*debug-mode* t)
          (tuatara.runtime:set-debug t)
          (setf (gethash 'secret-test-failing tuatara.command::*commands*) (lambda () (funcall kiwi)))
          (sb-int:encapsulate 'tuatara.defaults::org-parse 'test-failing (lambda (f &rest a) (apply kiwi f a)))
          (sb-int:encapsulate 'tuatara.command::window-pieces 'test-failing
                              (lambda (f editor window &rest more)
                                (if (and drawn-wrong
                                         (tuatara.io:buffer-plaintext-p (tuatara.display:window-buffer window)))
                                    (funcall kiwi)
                                    (apply f editor window more))))
          (unwind-protect
               (multiple-value-bind (f editor)
                   (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                            (list :until (lambda () (not (decrypting-p))))
                                            (list :wait 0.3)
                                            "M-x s e c r e t - t e s t - f a i l i n g RET" (list :wait 0.2)
                                            "C-x o" (list :wait 0.1)))
                 (declare (ignore editor))
                 (let ((buffer (tuatara.buffer:find-buffer "kea.org.gpg"))
                       (backtrace (tuatara.buffer:find-buffer "*Backtrace*")))
                   (true buffer)
                   (true backtrace)
                   (when backtrace
                     (let ((text (tuatara.buffer:buffer-string backtrace)))
                       (true (search "a type-error" text) "~a" text)
                       (false (search "kiwi words" text) "~a" text)))
                   (true (tuatara.frontend.fake:fake-frame-showing f "a type-error"))
                   (false (tuatara.frontend.fake:fake-frame-showing f "kiwi words"))
                   ;; a job on it failing: its words for the person say the kind alone
                   (when buffer
                     (let ((job (tuatara.defaults::make-decrypt-job buffer "/tmp/x.txt.gpg" nil)))
                       (tuatara.runtime:start-job-thread job (lambda (job) (declare (ignore job)) (funcall kiwi)))
                       (loop repeat 500 while (tuatara.runtime:job-running-p job) do (sleep 0.01))
                       (false (search "kiwi" (tuatara.runtime:job-ending-words job)))
                       (true (search "a type-error" (tuatara.runtime:job-ending-words job)))))
                   ;; drawn: the window that cannot be shown says the kind alone
                   (setf drawn-wrong t)
                   (when buffer
                     (multiple-value-bind (f2 editor2)
                         (auto-save-session (list (format nil "C-x b ~a RET" (rimu-typed "kea.org.gpg"))
                                                  (list :wait 0.2)))
                       (declare (ignore editor2))
                       (true (tuatara.frontend.fake:fake-frame-showing f2 "This window cannot be shown: a type-error"))
                       (false (tuatara.frontend.fake:fake-frame-showing f2 "kiwi words"))))
                   (setf drawn-wrong nil)
                   (let ((messages (tuatara.buffer:find-buffer "*Messages*")))
                     (when messages
                       (false (search "kiwi words" (tuatara.buffer:buffer-string messages)))))
                   (let ((log (if (probe-file *test-debug-log*) (alexandria:read-file-into-string *test-debug-log*)
                                "")))
                     (true (search "a type-error" log))
                     (false (search "kiwi words" log) "~a" log))
                   (when buffer (tuatara.buffer:kill-buffer buffer))))
            (sb-int:unencapsulate 'tuatara.defaults::org-parse 'test-failing)
            (sb-int:unencapsulate 'tuatara.command::window-pieces 'test-failing)
            (remhash 'secret-test-failing tuatara.command::*commands*)
            (drain-debug-reports))))))

(defun fake-gpg (dir &key (name "gpg") knows)
  "A program in DIR standing in for gpg, or for another program of GnuPG's by
NAME, by its full path: it writes the arguments it was given, a line each,
to DIR's arguments, and its environment to DIR's environment; asked for the
options it knows (--dump-options), it lists KNOWS, a line each, and does
nothing more."
  (let ((program (namestring (merge-pathnames name dir))))
    (put-program program (format nil "#!/bin/sh~%if [ \"$1\" = --dump-options ]; then~%~{  echo '~a'~%~}  exit 0~%fi~%~
                                      printf '%s\\n' \"$@\" > '~aarguments'~%env > '~aenvironment'~%"
                                 knows (namestring dir) (namestring dir)))
    program))

(define-test (encrypted gpg-found-only-in-system-directories)
  ;; a gpg in a directory of the PATH that is no system directory of programs is not the one run
  (with-test-directory (dir)
    (fake-gpg dir)
    (with-environment-variable ("PATH" (format nil "~a:~a" (string-right-trim "/" (namestring dir))
                                               (or (sb-posix:getenv "PATH") "/usr/bin")))
      (let ((tuatara.defaults::*gpg-program* "gpg"))
        (false (search (namestring dir) (or (tuatara.defaults::gpg-path) "")))))))

(define-test (encrypted gpg-run-kept-to-itself)
  ;; gpg runs with the environment it needs alone, the system's PATH, and nothing fetched or shown by a program
  (with-test-directory (dir)
    (let ((tuatara.defaults::*gpg-program* (fake-gpg dir)))
      (with-environment-variable ("TUATARA_TEST_CANARY" "kea")
        (tuatara.defaults::gpg-run (list "--list-keys") :asker :agent))
      (let ((arguments (uiop:read-file-lines (merge-pathnames "arguments" dir)))
            (environment (uiop:read-file-lines (merge-pathnames "environment" dir))))
        (dolist (option '("--disable-dirmngr" "--no-auto-key-retrieve" "--no-auto-key-locate"))
          (true (member option arguments :test #'string=) "~a" option))
        (is = 2 (count "no-show-photos" arguments :test #'string=))
        (true (member (format nil "PATH=~a" tuatara.io:*sandbox-path*) environment :test #'string=))
        (true (find-if (lambda (line) (uiop:string-prefix-p "GNUPGHOME=" line)) environment))
        (false (find-if (lambda (line) (search "TUATARA_TEST_CANARY" line)) environment))))))

(define-test (encrypted gpg-home-of-yours-alone)
  ;; GnuPG's directory, where gpg reads options that can run programs, is used only when no one else may write in it
  (with-test-directory (dir)
    (let ((tuatara.defaults::*gpg-program* (fake-gpg dir))
          (open (merge-pathnames "open/" dir)))
      (ensure-directories-exist open)
      (sb-posix:chmod (namestring open) #o777)
      (dolist (home (list (namestring open) "gnupg"))
        (with-environment-variable ("GNUPGHOME" home)
          (let ((words (handler-case (progn (tuatara.defaults::gpg-run (list "--list-keys") :asker :agent) nil)
                         (tuatara.runtime:refusal (condition) (princ-to-string condition)))))
            (true (search "GNUPGHOME" (or words "")) "~a: ~a" home words))))
      (false (probe-file (merge-pathnames "arguments" dir)) "gpg never ran"))))

(defun gpg-refusal ()
  "What gpg's refusal to run says, or NIL when it runs."
  (handler-case (progn (tuatara.defaults::gpg-run (list "--list-keys") :asker :agent) nil)
    (tuatara.runtime:refusal (condition) (princ-to-string condition))))

(define-test (encrypted gpg-home-directories-on-the-way)
  ;; GnuPG's directory is no link, and no directory on the way to it may be written in by others, but for a sticky one
  (with-test-directory (dir)
    (let ((tuatara.defaults::*gpg-program* (fake-gpg dir)))
      (flet ((make-dir (name &optional (mode #o700))
               (let ((path (namestring (merge-pathnames name dir))))
                 (ensure-directories-exist path)
                 (sb-posix:chmod path mode)
                 path))
             (link (name target)
               (sb-posix:symlink (string-right-trim "/" target) (namestring (merge-pathnames name dir)))))
        (let ((home (make-dir "home/")))
          (make-dir "open/" #o777) (make-dir "open/home/")
          (make-dir "group/" #o770) (make-dir "group/home/")
          (make-dir "sticky/" #o1777) (make-dir "sticky/home/")
          (link "home-link" home)
          (link "way" (namestring dir))
          (link "open-way" (namestring (merge-pathnames "open/" dir)))
          (dolist (case `((,home nil) (,(namestring (merge-pathnames "home-link" dir)) "a link")
                          (,(make-dir "open/home/") "may be used by others")
                          (,(make-dir "group/home/") "may be used by others")
                          (,(make-dir "sticky/home/") nil) (,(format nil "~away/home" dir) nil)
                          (,(format nil "~aopen-way/home" dir) "may be used by others")))
            (destructuring-bind (gnupg-home words) case
              (with-environment-variable ("GNUPGHOME" gnupg-home)
                (let ((said (gpg-refusal)))
                  (if words
                      (true (search words (or said "")) "~a: ~a" gnupg-home said)
                      (false said "~a" gnupg-home))))))
          ;; with no GNUPGHOME, ~/.gnupg is held to the same, and your home while it is not made yet
          (make-dir "open/me/.gnupg/") (make-dir "linked/") (link "linked/.gnupg" home) (make-dir "bare/")
          (make-dir "mine/.gnupg/")
          (with-environment-variable ("GNUPGHOME" nil)
            (dolist (case '(("open/me/" "may be used by others") ("linked/" "a link") ("bare/" nil) ("mine/" nil)))
              (destructuring-bind (name words) case
                (with-environment-variable ("HOME" (namestring (merge-pathnames name dir)))
                  (let ((said (gpg-refusal)))
                    (if words
                        (true (search words (or said "")) "~a: ~a" name said)
                        (false said "~a" name))))))))))))

(define-test (encrypted gpg-newer-options-where-known)
  ;; --no-auto-key-import is given to a gpg that lists it, and not to an older one, which would refuse to run
  (dolist (knows '(t nil))
    (with-test-directory (dir)
      (let ((tuatara.defaults::*gpg-program* (fake-gpg dir :knows (and knows '("--no-auto-key-import")))))
        (tuatara.defaults::gpg-run (list "--list-keys") :asker :agent)
        (is eq knows (and (member "--no-auto-key-import" (uiop:read-file-lines (merge-pathnames "arguments" dir))
                                  :test #'string=)
                          t))))))

(define-test (encrypted keys-never-taken-from-signed-files)
  ;; with auto-key-import in gpg.conf, verifying a file signed with its key in it leaves the keyring as it was
  (with-test-directory (dir)
    (let ((signer (namestring (merge-pathnames "signer/" dir)))
          (home (namestring (merge-pathnames "home/" dir)))
          (signed nil))
      (dolist (directory (list signer home))
        (ensure-directories-exist directory)
        (sb-posix:chmod directory #o700))
      (put-file (merge-pathnames "gpg.conf" home) (format nil "auto-key-import~%"))
      (unwind-protect
           (progn
             (with-environment-variable ("GNUPGHOME" signer)
               (let ((tuatara-user:*gpg-passphrase-from* :minibuffer))
                 (tuatara.defaults::gpg-run (list "--quick-gen-key" "Moa Kaikoura <moa@example.org>" "default" "default"
                                                  "never")
                                            :passphrase "")
                 (setf signed (tuatara.defaults::gpg-run (list "--sign" "--include-key-block" "--output" "-")
                                                         :input (tuatara.text:string-octets "kea") :passphrase ""))))
             (true (plusp (length signed)))
             (with-environment-variable ("GNUPGHOME" home)
               (let ((statuses (nth-value 1 (tuatara.defaults::gpg-run (list "--verify" "--" "-") :input signed
                                                                                                    :asker :agent))))
                 (false (find-if (lambda (status) (uiop:string-prefix-p "GOODSIG" status)) statuses) "~a" statuses))
               (false (tuatara.defaults::gpg-keys :all t))))
        (dolist (directory (list signer home))
          (with-environment-variable ("GNUPGHOME" directory)
            (uiop:run-program (list (tuatara.defaults::gpgconf-path) "--kill" "all") :ignore-error-status t)))))))

(define-test (encrypted gpg-path-from-the-root)
  ;; a gpg named by a path not from the root is not looked for in the directory the editor is in
  (with-test-directory (dir)
    (fake-gpg dir)
    (let ((before (sb-posix:getcwd)))
      (sb-posix:chdir (namestring dir))
      (unwind-protect
           (dolist (name '("./gpg" "../gpg"))
             (let ((tuatara.defaults::*gpg-program* name))
               (false (tuatara.defaults::gpg-path) "~a" name)))
        (sb-posix:chdir before)))))

(define-test (encrypted keys-gpg-will-not-encrypt-to)
  (let ((said "gpg: C008B5A496AD5646: There is no assurance this key belongs to the named user"))
    (true (search "gpg --check-trustdb"
                  (tuatara.defaults::gpg-failure-words '("INV_RECP 10 C008B5A496AD5646" "FAILURE encrypt 53") said)))
    (true (search "C008B5A496AD5646 has expired"
                  (tuatara.defaults::gpg-failure-words '("INV_RECP 5 C008B5A496AD5646") said)))
    (true (search "no key ABCD"
                  (tuatara.defaults::gpg-failure-words '("INV_RECP 1 ABCD") "")))
    (is string= said (tuatara.defaults::gpg-failure-words '("FAILURE encrypt 53") said))))

(define-test (encrypted gpg-lock-kept-to-itself)
  ;; gpgconf is found beside gpg or in the system's directories only, and runs with gpg's environment
  (with-test-directory (dir)
    (let ((gpgconf (fake-gpg dir :name "gpgconf")))
      (with-environment-variable ("PATH" (format nil "~a:~a" (string-right-trim "/" (namestring dir))
                                                 (or (sb-posix:getenv "PATH") "/usr/bin")))
        (let ((tuatara.defaults::*gpg-program* "gpg"))
          (false (search (namestring dir) (or (tuatara.defaults::gpgconf-path) "")))))
      (let ((tuatara.defaults::*gpg-program* (fake-gpg dir)))
        (is string= gpgconf (tuatara.defaults::gpgconf-path))
        (with-environment-variable ("TUATARA_TEST_CANARY" "kea")
          (let ((f (run-keys "M-x g p g - l o c k RET")))
            (true (tuatara.frontend.fake:fake-frame-showing f "gpg-agent has forgotten the passphrases it kept"))))
        (let ((environment (uiop:read-file-lines (merge-pathnames "environment" dir))))
          (is equal '("--reload" "gpg-agent") (uiop:read-file-lines (merge-pathnames "arguments" dir)))
          (true (member (format nil "PATH=~a" tuatara.io:*sandbox-path*) environment :test #'string=))
          (true (find-if (lambda (line) (uiop:string-prefix-p "GNUPGHOME=" line)) environment))
          (false (find-if (lambda (line) (search "TUATARA_TEST_CANARY" line)) environment)))
        ;; nor at all with a GNUPGHOME others may write in
        (delete-file (merge-pathnames "arguments" dir))
        (let ((open (namestring (merge-pathnames "open/" dir))))
          (ensure-directories-exist open)
          (sb-posix:chmod open #o777)
          (with-environment-variable ("GNUPGHOME" open)
            (run-keys "M-x g p g - l o c k RET"))
          (false (probe-file (merge-pathnames "arguments" dir)) "gpgconf never ran"))))))
