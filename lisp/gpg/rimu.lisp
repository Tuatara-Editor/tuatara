;;;; lisp/gpg/rimu.lisp --- GnuPG on the files marked in a list of a directory: : e encrypts, : d decrypts, : s signs, : v verifies.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's dired has them, on the files marked in a rimu list (or the one
;;;; on the line): each encrypted, decrypted, signed or verified as the
;;;; commands of the same names do it to one file (gpg-commands.lisp), the
;;;; question of how asked once for all of them.

(in-package #:tuatara.defaults)

(defun rimu-gpg-files (verb)
  "The files marked in the current list, or the one on the line, and words
naming them, where they are too when that is elsewhere (RIMU-QUESTION); an
error saying why when there is none to VERB."
  (let* ((buffer (current-buffer))
         (paths (or (rimu-chosen-paths buffer :files-only t)
                    (error "~a" (rimu-nothing-words buffer verb :files-only t :none "No file here: mark files, or put the cursor on one")))))
    (values paths (rimu-question nil paths :here (tuatara.buffer:buffer-local buffer 'rimu-directory)))))

(define-command rimu-encrypt-files ()
  "Encrypt the files marked (or the one on the line), each into FILE.gpg, to
the keys or with a passphrase chosen in the minibuffer, asked once; and,
when you say so, asked once too, wipe each file once it is encrypted
\(\\[wipe-file]): with a passphrase, only after it, asked again, opens what was
made."
  (multiple-value-bind (paths what) (rimu-gpg-files "encrypt")
    (choose-encryption what '()
                       (lambda (encryption)
                         (let* ((symmetric (getf encryption :symmetric))
                                (passphrase (and symmetric (gpg-minibuffer-p)
                                                 (gpg-read-passphrase "Passphrase to encrypt them with: " :confirm t)))
                                (wipe (yes-or-no-p* (wipe-original-question "the originals once encrypted" encryption (wipe-caveat paths))))
                                (check (and wipe symmetric (gpg-minibuffer-p)
                                            (gpg-read-passphrase "The passphrase again, to check it opens what is made: "))))
                           (unwind-protect
                                (dolist (path paths)
                                  (let ((check (and check (copy-seq check))))
                                    (encrypt-file-to path encryption :passphrase (and passphrase (copy-seq passphrase))
                                                     :then (and wipe (lambda ()
                                                                       (wipe-original path (concatenate 'string path ".gpg") encryption
                                                                                      :passphrase check))))))
                             (wipe-string passphrase)
                             (wipe-string check)))))))

(define-command rimu-decrypt-files ()
  "Decrypt the files marked (or the one on the line), each into its name
without .gpg, for you alone, after asking once: their text is then on disk."
  (multiple-value-bind (paths what) (rimu-gpg-files "decrypt")
    (when (yes-or-no-p* (format nil "This writes the text of ~a to disk, where it can be read; go on?" what))
      (dolist (path paths)
        (let ((target (decrypted-name path)))
          (when (gpg-target-free-p target)
            (decrypt-file-to path target)))))))

(define-command rimu-sign-files ()
  "Sign the files marked (or the one on the line) with your key, the kind of signature asked once."
  (multiple-value-bind (paths what) (rimu-gpg-files "sign")
    (let* ((kind (read-signature-kind))
           (passphrase (gpg-own-passphrase (format nil "signing ~a" what))))
      (unwind-protect
         (dolist (path paths)
           (sign-file-as path kind (and passphrase (copy-seq passphrase))))
        (wipe-string passphrase)))))

(define-command rimu-verify-files ()
  "Check the signatures of the files marked (or the one on the line), each said
in turn and kept in *Messages*."
  (dolist (path (rimu-gpg-files "verify"))
    (verify-file-at path)))

(defun install-rimu-gpg ()
  "Bind : e, : d, : s and : v in lists of directories."
  (define-keys *rimu-keymap* '((": e" rimu-encrypt-files) (": d" rimu-decrypt-files) (": s" rimu-sign-files) (": v" rimu-verify-files))))
