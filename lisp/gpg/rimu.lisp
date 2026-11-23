;;;; lisp/gpg/rimu.lisp --- GnuPG on the files marked in a list of a directory: : e encrypts, : d decrypts, : s signs,
;;;; : v verifies.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands on files of gpg-commands.lisp, on the files marked in a
;;;; rimu list (or the one on the line): each encrypted, decrypted, signed or
;;;; verified in turn, one gpg at work at a time, the question of how asked
;;;; once for all of them.

(in-package #:tuatara.defaults)

(defun rimu-gpg-files (verb)
  "The files marked in the current list, or the one on the line, and words
naming them, where they are too when that is elsewhere (RIMU-QUESTION); an
error saying why when there is none to VERB."
  (let* ((buffer (current-buffer))
         (paths (or (rimu-chosen-paths buffer :files-only t)
                    (refuse "~a" (rimu-nothing-words buffer verb
                                                     :files-only t
                                                     :none "No file here: mark files, or put the cursor on one")))))
    (values paths (rimu-question nil paths :here (tuatara.buffer:buffer-local buffer 'rimu-directory)))))

(define-command rimu-encrypt-files ()
  "Encrypt the files marked (or the one on the line), each into FILE.gpg, to
the keys or with a passphrase chosen in the minibuffer, asked once; and,
when you say so, asked once too, wipe each file once it is encrypted
\(\\[wipe-file]): with a passphrase, only after it, asked again, opens what was
made."
  (multiple-value-bind (paths what) (rimu-gpg-files "encrypt")
    (choose-encryption
     what '()
     (lambda (encryption)
       (let ((paths (gpg-free-targets paths (lambda (path) (concatenate 'string path ".gpg")))))
         (when paths
           (let* ((symmetric (getf encryption :symmetric))
                  (passphrase (gpg-passphrase-for "them" :encrypting encryption))
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
                (wipe-string check))))))))))

(define-command rimu-decrypt-files ()
  "Decrypt the files marked (or the one on the line), each into its name
without .gpg, for you alone, after asking once: their text is then on disk."
  (multiple-value-bind (paths what) (rimu-gpg-files "decrypt")
    (when (yes-or-no-p* (format nil "This writes the text of ~a to disk, where it can be read; go on?" what))
      (gpg-files-in-turn (gpg-free-targets paths #'decrypted-name)
                         (lambda (path after) (decrypt-file-to path (decrypted-name path) :after after))))))

(define-command rimu-sign-files ()
  "Sign the files marked (or the one on the line) with your key, the kind of signature asked once."
  (multiple-value-bind (paths what) (rimu-gpg-files "sign")
    (let* ((kind (read-signature-kind))
           (paths (gpg-free-targets paths (lambda (path) (signature-name path kind))))
           (passphrase (and paths (gpg-own-passphrase (format nil "signing ~a" what)))))
      (gpg-files-in-turn paths
                         (lambda (path after)
                           (sign-file-as path kind (and passphrase (copy-seq passphrase)) :after after))
                         (lambda () (wipe-string passphrase))))))

(define-command rimu-verify-files ()
  "Check the signatures of the files marked (or the one on the line), each said
in turn and kept in *Messages*."
  (gpg-files-in-turn (rimu-gpg-files "verify") (lambda (path after) (verify-file-at path :after after))))

(defun install-rimu-gpg ()
  "Bind : e, : d, : s and : v in lists of directories."
  (define-keys *rimu-keymap*
      '((": e" rimu-encrypt-files) (": d" rimu-decrypt-files) (": s" rimu-sign-files) (": v" rimu-verify-files))))
