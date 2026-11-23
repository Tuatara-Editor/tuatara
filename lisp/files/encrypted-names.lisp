;;;; lisp/files/encrypted-names.lisp --- Encrypted files told by their names and their first octets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Names ending .gpg or .age, in any case, say a file is encrypted; so do
;;;; the first octets of one written by gpg, age, openssl, Ansible's vault or
;;;; a LUKS or BitLocker volume. Searches, directories kept read and saving
;;;; ask these before they read or write such a file.

(in-package #:tuatara.defaults)

(defun encrypted-name-p (name)
  "True when file NAME, or a path, ends as an encrypted file does: .gpg or
.age, in any case (.GPG too)."
  (or (gpg-name-p name) (and (> (length name) 4) (string-equal name ".age" :start1 (- (length name) 4)))))

(defun gpg-name-p (name)
  "True when file NAME, or a path, ends as a file gpg encrypted does: .gpg, in
any case."
  (let ((end (- (length name) 4)))
    (and (plusp end) (string-equal name ".gpg" :start1 end))))

(defun unencryptable-name-p (name)
  "True when file NAME ends as an encrypted file of a kind Tuatara cannot
encrypt (.age): such a file is not made, its text would be there to read."
  (and (encrypted-name-p name) (not (gpg-name-p name))))

(defparameter *encrypted-text-headers*
  '("-----BEGIN PGP MESSAGE-----" "-----BEGIN AGE ENCRYPTED FILE-----" "-----BEGIN ENCRYPTED PRIVATE KEY-----")
  "The lines an encrypted file written as text begins with: gpg --armor, age
--armor, a private key kept under a passphrase (PKCS #8).")

(defparameter *encrypted-signatures*
  '((0 . "age-encryption.org/v1") (0 . "$ANSIBLE_VAULT;") (0 . "Salted__")
    (0 . (#x4c #x55 #x4b #x53 #xba #xbe)) (3 . "-FVE-FS-"))
  "Where in a file, and as what octets, encrypted files of other kinds say what
they are: age's own header, Ansible's vault, openssl enc, a LUKS volume (its
header, LUKS and BA BE), a BitLocker volume (-FVE-FS- after the jump). A
VeraCrypt or TrueCrypt volume says nothing, its header looking like chance
octets, as it is made to; its zero octets keep its text from being read.")

(defun encrypted-head-p (head)
  "True when HEAD, the first octets of a file (or its text, as characters),
begins as an encrypted file does, namely a line of *ENCRYPTED-TEXT-HEADERS*
\(after blank space), or a key written as text with Proc-Type: 4,ENCRYPTED; one
of *ENCRYPTED-SIGNATURES* where it belongs. It is also true when HEAD is the
first packet of a binary OpenPGP message, its tag and version as gpg writes them
\(TUATARA.IO:OPENPGP-PACKET-P)."
  (flet ((octet (i) (and (< i (length head))
                         (let ((x (elt head i))) (if (characterp x) (char-code x) x)))))
    (let* ((start (position-if-not (lambda (x) (member (if (characterp x) (char-code x) x) tuatara.text:*blank-octets*)) head))
           (text (map 'string (lambda (x) (if (characterp x) x (code-char x)))
                      (subseq head (or start 0) (min (length head) (+ (or start 0) 40))))))
      (or (some (lambda (header) (uiop:string-prefix-p header text)) *encrypted-text-headers*)
          (and (uiop:string-prefix-p "-----BEGIN " text)
               (search "Proc-Type: 4,ENCRYPTED"
                       (map 'string
                            (lambda (x) (if (characterp x) x (code-char x)))
                            (subseq head 0 (min (length head) 200)))))
          (loop for (at . signature) in *encrypted-signatures*
                thereis (let ((codes (if (stringp signature) (map 'list #'char-code signature) signature)))
                          (loop for code in codes
                                for i from at
                                always (eql (octet i) code))))
          (tuatara.io:openpgp-packet-p
           (if (stringp head)
               ;; text: its characters taken as octets, those past one octet as none
               (map '(vector (unsigned-byte 8)) (lambda (c) (if (< (char-code c) 256) (char-code c) 0))
                    (subseq head 0 (min (length head) 8)))
               head))))))

(defun encrypted-file-p (path &optional text)
  "True when the file at PATH is encrypted: as its name says (ENCRYPTED-NAME-P for
.gpg, .age), as its header on disk shows (ENCRYPTED-HEAD-P), or as TEXT, the
text a buffer shows of it, when given, begins. The text of such a file is
never read for searching, for a directory kept read or for a dynamic block,
even while a buffer shows it decrypted."
  (or (encrypted-name-p (tuatara.io:native-file-name path))
      (let ((head (tuatara.io:file-head path 4096))) (and head (encrypted-head-p head)))
      (and text (encrypted-head-p (subseq text 0 (min (length text) 200))))))
