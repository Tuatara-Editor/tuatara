;;;; lisp/gpg/rimu.lisp --- GnuPG on the files marked in a list of a directory: : e encrypts, : d decrypts, : s signs,
;;;; : v verifies.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands on files of gpg-commands.lisp take the files marked in a
;;;; rimu list (or the one on the line); here they have their keys there.

(in-package #:tuatara.defaults)

(defun install-rimu-gpg ()
  "Bind : e, : d, : s and : v in lists of directories."
  (define-keys *rimu-keymap*
      '((": e" gpg-encrypt-file) (": d" gpg-decrypt-file) (": s" gpg-sign-file) (": v" gpg-verify-file))))
