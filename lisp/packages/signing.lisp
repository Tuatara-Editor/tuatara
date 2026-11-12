;;;; lisp/packages/signing.lisp --- Who signed a version fetched, and whom a source is trusted by.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Who signed the commit or tag a package was fetched at is read from git's own
;;;; check; the signer of a source's first version taken is remembered, and a later
;;;; version signed by another, or unsigned when it was signed, is refused until
;;;; the source is trusted again.

(in-package #:tuatara.packages)

(define-setting *package-check-signatures* t
  "Whether a package's versions are taken only when signed by a key you trust.
- t: who signed each version is checked, and only those that M-x
  package-trust-again and the signers of a source (:signers) allow are taken.
- nil: nothing is checked. Every version of every source is taken, signed by
  any key, a key your keyring lacks or no one, and nothing is remembered of
  who signed it. Only for a machine where the sources are trusted by other
  means."
  :values boolean)

(defun trust-line (entry)
  "The line of the trust file for ENTRY, (WHERE SIGNER NAME KEY)."
  (destructuring-bind (where signer name key) entry
    (format nil "~a~c~a~c~a~c~a" (escape-path where) #\Tab signer #\Tab (escape-path (or name "")) #\Tab (or key ""))))

(defun parse-trust-line (line)
  "The entry LINE of the trust file writes, (WHERE SIGNER NAME KEY), or NIL
for a line that makes no sense. SIGNER a key's fingerprint or \"unsigned\",
NAME the signer's name or NIL, KEY a secure shell (SSH) signer's key or NIL;
SIGNER is key: and a GNU Privacy Guard (GPG) key's fingerprint for a source
whose first version was signed by a key your keyring lacks. Fields after those
are passed over \(STATE-LINE-FIELDS)."
  (let ((fields (state-line-fields line)))
    (when (<= 4 (length fields))
      (destructuring-bind (where signer name key &rest later) fields
        (declare (ignore later))
        (and (plusp (length where))
             (or (string= signer "unsigned") (cl-ppcre:scan "^(?:SHA256:[A-Za-z0-9+/=]+|[0-9A-F]{40,64}|key:(?:[0-9A-F]{40}|[0-9A-F]{64}))$" signer))
             (or (zerop (length key)) (cl-ppcre:scan "^[a-z0-9@.-]{1,64} [A-Za-z0-9+/]+={0,2}$" key))
             (list (unescape-path where) signer (let ((name (unescape-path name))) (and (plusp (length name)) name))
                   (and (plusp (length key)) key)))))))

(define-state-list *package-trust* "package-trust" 'parse-trust-line 'trust-line '*packages-kept*
  :header (format nil "# The sources of packages trusted, the latest first: the address; who signed~%~
                       # the versions taken from it, a key's fingerprint, or unsigned; the signer's~%~
                       # name; an SSH signer's key. M-x package-trust-again forgets a source's.~%")
  :path-of #'first :fields 4 :trust t :line-key (field-line-key 4 0))

(defun package-signers-file (where)
  "The allowed signers file git checks the secure shell (SSH) signatures of
the source WHERE against, kept beside its packages."
  (format nil "~a~a/.allowed-signers" (packages-directory) (tuatara.io:path-file-name where)))

(defun write-package-signers-file (where)
  "Write the allowed signers file of the source WHERE (PACKAGE-SIGNERS-FILE)
anew: the secure shell (SSH) key trusted for it (*PACKAGE-TRUST*), or nothing.
Returns its name."
  (let ((file (package-signers-file where))
        (trusted (state-list-find *package-trust* where)))
    (tuatara.io:ensure-private-directory (directory-namestring file))
    (tuatara.io:write-private-file file (if (fourth trusted) (format nil "trusted namespaces=\"git\" ~a~%" (fourth trusted)) ""))
    file))

(defun signature-block (text kind)
  "Where the signature of the git object TEXT, of KIND :commit or :tag, lies
in it, as two values, its start and end: a commit's on its header's line
gpgsig (or gpgsig-sha256) and those after it, before the blank line that
ends the header, never another header's (mergetag); a tag's the last block of its
text, where git appends it. NIL when it has none there."
  (let ((header-end (or (search (format nil "~%~%") text) (length text))))
    (multiple-value-bind (begin end)
        (if (eq kind :commit)
            (let* ((line (cl-ppcre:scan "(?m)^gpgsig(?:-sha256)? -----BEGIN " text :end header-end))
                   (begin (and line (search "-----BEGIN " text :start2 line :end2 header-end))))
              (values begin (and begin (search "-----END " text :start2 begin :end2 header-end))))
            (let ((begin (search "-----BEGIN " text :from-end t)))
              (values begin (and begin (search "-----END " text :start2 begin)))))
      (when end
        (values begin (or (position #\Newline text :start end) (length text)))))))

(defun ssh-signature-key (text kind)
  "The public key the secure shell (SSH) signature of the git object TEXT, of
KIND :commit or :tag (SIGNATURE-BLOCK), was made with, as an allowed
signers file writes it (TYPE BASE64), read from the signature's own octets;
NIL when it has none."
  (multiple-value-bind (begin end) (signature-block text kind)
    (let ((mark "-----BEGIN SSH SIGNATURE-----"))
      (when (and begin (string= mark text :start2 begin :end2 (min (length text) (+ begin (length mark)))))
        (let* ((octets (tuatara.io:base64-octets (tuatara.text:string-octets (subseq text (+ begin (length mark)) (search "-----END" text :start2 begin :end2 end)))))
               (size (tuatara.io:octets-integer octets 10 4 :big-endian t)))
          (when (and size (tuatara.io:octets-at-p octets "SSHSIG") (<= (+ 14 size) (length octets)))
            (let* ((blob (subseq octets 14 (+ 14 size)))
                   (type-size (tuatara.io:octets-integer blob 0 4 :big-endian t))
                   (type (and type-size
                              (<= (+ 4 type-size) (length blob))
                              (tuatara.io:octets-ascii blob 4 (+ 4 type-size)))))
              (and type (cl-ppcre:scan "^[a-z0-9@.-]{1,64}$" type)
                   (format nil "~a ~a" type (tuatara.io:base64-string blob))))))))))

(defun ssh-signature-p (text kind)
  "True when the signature of the git object TEXT, of KIND :commit or :tag, is
a secure shell (SSH) one."
  (let ((begin (signature-block text kind)))
    (and begin (string= "-----BEGIN SSH SIGNATURE-----" text :start2 begin :end2 (min (length text) (+ begin 29))))))

(defun plain-name (text)
  "TEXT, a signer's name as a key gives it, on one line and without the
characters that change how text is laid out (right to left, hidden ones), so
that it shows as it is; NIL for NIL or nothing left."
  (let ((name (tuatara.text:one-line (and text (remove-if (lambda (c) (eq (sb-unicode:general-category c) :cf))
                                                          text)))))
    (and name (plusp (length name)) (tuatara.display:shorten-string name 80))))

(defun commit-signature (fields object)
  "The signature of a commit, as PACKAGE-SIGNATURE gives it, from FIELDS,
what git's log said of it (%G?, %GF, %GP, %GS and %GK), and OBJECT, the
commit's object; :ERROR when it is signed but git could not say by whom. A
GNU Privacy Guard (GPG) signature whose key your keyring lacks is :UNCHECKED,
its key's fingerprint left to MISSING-KEY."
  (destructuring-bind (&optional (grade "") subkey primary signer key-id &rest more) fields
    (declare (ignore more))
    (let ((ssh (ssh-signature-p object :commit))
          (signed (signature-block object :commit)))
      (cond ((member grade '("G" "U" "X" "Y") :test #'string=)
             (list :status :good
                   :fingerprint (signer-fingerprint (if (plusp (length primary)) primary subkey))
                   :subkey (and (plusp (length subkey)) (signer-fingerprint subkey))
                   :name (and (not ssh) (plain-name signer))
                   :key (and ssh (ssh-signature-key object :commit))))
            ((member grade '("B" "R") :test #'string=) (list :status :bad))
            ((and (string= grade "E") (not ssh) (cl-ppcre:scan "^[0-9A-Fa-f]{16,64}$" key-id))
             (list :status :unchecked))
            ((and (string= grade "N") (not signed)) (list :status :none))
            (t (list :status :error))))))

(defun gnupg-status (said name)
  "The fields of gpg's status line NAME in SAID, what it wrote, each at the start of a line; NIL when there is none."
  (let ((found (nth-value 1 (cl-ppcre:scan-to-strings (format nil "(?m)^\\[GNUPG:\\] ~a(?: ([^\\n]*))?$" name) said))))
    (and found (or (tuatara.text:split-words (aref found 0)) (list "")))))

(defun tag-signature (said object)
  "The signature of a tag, as PACKAGE-SIGNATURE gives it, from SAID, what git
verify-tag --raw said of it, and OBJECT, the tag's object."
  (if (ssh-signature-p object :tag)
      (let ((good (nth-value 1 (cl-ppcre:scan-to-strings "(?m)^Good \"git\" signature (?:for \\S+ )?with \\S+ key (SHA256:[A-Za-z0-9+/=]+)$" said))))
        (if good
            (list :status :good :fingerprint (aref good 0) :key (ssh-signature-key object :tag))
            (list :status :bad)))
      (let ((valid (gnupg-status said "VALIDSIG"))
            (good (or (gnupg-status said "GOODSIG") (gnupg-status said "EXPSIG") (gnupg-status said "EXPKEYSIG")))
            (missing (or (gnupg-status said "ERRSIG") (gnupg-status said "NO_PUBKEY"))))
        (cond ((or (gnupg-status said "BADSIG") (gnupg-status said "REVKEYSIG")) (list :status :bad))
              ((and valid good)
               (list :status :good :fingerprint (signer-fingerprint (or (nth 9 valid) (first valid)))
                     :subkey (signer-fingerprint (first valid))
                     :name (plain-name (format nil "~{~a~^ ~}" (rest good)))))
              ((and missing (missing-key said)) (list :status :unchecked :key-id (missing-key said)))
              (t (list :status :error))))))

(defun missing-key (said)
  "The fingerprint of the key that made a GNU Privacy Guard (GPG) signature your
keyring lacks, from SAID, gpg's status lines. It is ERRSIG's seventh field, the
issuer's fingerprint, 40 or 64 hexadecimal digits: the signing key itself, a
subkey or the primary key. It is NIL when gpg did not give it, as for an old
signature naming only a 64-bit ID, which is too short to hold to."
  (let ((fingerprint (seventh (gnupg-status said "ERRSIG"))))
    (and fingerprint (cl-ppcre:scan "^(?:[0-9A-Fa-f]{40}|[0-9A-Fa-f]{64})$" fingerprint)
         (signer-fingerprint fingerprint))))

(defun tag-object-names-p (object tag)
  "True when OBJECT, a tag's object, names itself TAG in its header (its tag
line, before the blank line that ends the header), whatever its message says."
  (and (cl-ppcre:scan (format nil "(?m)^tag ~a$" (cl-ppcre:quote-meta-chars tag)) object
                      :end (or (search (format nil "~%~%") object) (length object)))
       t))

(defparameter *package-check-seconds* 120
  "The most seconds checking who signed a package's version may take, git
with gpg or ssh-keygen, before it is ended and the version not taken.")

(defun package-signature (folder commit tag where)
  "Who signed COMMIT of the repository at WHERE, kept in FOLDER: the tag TAG it
was fetched by now when that is signed (its object, FETCH_HEAD, naming TAG and
COMMIT), else the commit. Git does the check, with gpg and your keyring for a
GNU Privacy Guard (GPG) signature, and with ssh-keygen and the allowed signers
file of the key trusted for WHERE for a secure shell (SSH) one
\(WRITE-PACKAGE-SIGNERS-FILE). Nothing of the repository is run. Returns a list.
:STATUS is :GOOD, :BAD (it does not hold, or its key is revoked), :UNCHECKED (by
a GPG key your keyring lacks, its fingerprint :KEY-ID), :NONE (unsigned) or
:ERROR (signed, but git could not say by whom). The list also has :FINGERPRINT
\(the primary key's), :SUBKEY and :NAME. For an SSH key it has :KEY of a good
one, the key seen to have made the signature."
  (labels ((git (allowed &rest arguments)
             (multiple-value-bind (octets ok too-large said)
                 (run-git-program (git-for-packages) folder arguments
                                  :environment (git-network-environment (list "gpg.ssh.allowedSignersFile" allowed))
                                  :errors t :seconds *package-check-seconds*)
               (declare (ignore ok too-large))
               (values (if octets (tuatara.text:octets-string octets) "") (or said ""))))
           (trimmed (text) (string-trim '(#\Newline #\Space) text))
           (check (allowed tag-object)
             (if tag-object
                 (tag-signature (nth-value 1 (git allowed "verify-tag" "--raw" "FETCH_HEAD")) tag-object)
                 (commit-signature (uiop:split-string (trimmed (git allowed "log" "-1" "--format=%G?%x00%GF%x00%GP%x00%GS%x00%GK" commit))
                                                      :separator (list (code-char 0)))
                                   (git allowed "cat-file" "commit" commit)))))
    (let* ((allowed (write-package-signers-file where))
           (object (and tag (string= "tag" (trimmed (git allowed "cat-file" "-t" "FETCH_HEAD")))
                        (git allowed "cat-file" "tag" "FETCH_HEAD")))
           (tag-object (and object (signature-block object :tag) object)))
      (when (and tag-object
                 (not (and (tag-object-names-p tag-object tag)
                           (string= commit
                                    (trimmed (git allowed "rev-parse" "--verify" "--quiet" "FETCH_HEAD^{commit}"))))))
        (return-from package-signature (list :status :error :why "its tag object names another tag or commit")))
      (let ((signature (check allowed tag-object)))
        (when (and (eq (getf signature :status) :unchecked) (not (getf signature :key-id)))
          (let ((key (missing-key (nth-value 1 (git allowed "verify-commit" "--raw" commit)))))
            (setf signature (if key (list :status :unchecked :key-id key) (list :status :error)))))
        ;; the key read from the signature is kept only once git has seen it make the signature
        (when (getf signature :key)
          (let ((alone (format nil "~a-check-~a" (package-signers-file where) (tuatara.io:random-name))))
            (unwind-protect
                 (progn
                   (tuatara.io:write-private-file alone
                                                  (format nil "trusted namespaces=\"git\" ~a~%" (getf signature :key)))
                   (let ((again (check alone tag-object)))
                     (unless (and (eq (getf again :status) :good)
                                  (equal (getf again :fingerprint) (getf signature :fingerprint))
                                  (if tag-object
                                      (search "signature for trusted with"
                                              (nth-value 1 (git alone "verify-tag" "--raw" "FETCH_HEAD")))
                                      (string= "G" (trimmed (git alone "log" "-1" "--format=%G?" commit)))))
                       (setf (getf signature :key) nil))))
              (handler-case (delete-file alone) (file-error () nil)))))
        signature))))

(defun signer-words (signature)
  "Who signed a version, as SIGNATURE (PACKAGE-SIGNATURE) says, in words."
  (let ((fingerprint (getf signature :fingerprint)))
    (ecase (getf signature :status)
      (:good (if (getf signature :name)
                 (format nil "signed by ~a (key ~a)" (getf signature :name) fingerprint)
                 (format nil "signed by the key ~a" fingerprint)))
      (:unchecked (format nil "signed by the GPG key ~a, which your keyring lacks, so the signature cannot be checked: ~
                               import that key (M-x gpg-import-keys), then fetch it again (M-x package-update)"
                          (getf signature :key-id)))
      (:bad "signed, but its signature does not hold: it was changed after it was signed, or its key is revoked")
      (:error (format nil "signed, but who signed it could not be checked~@[ (~a)~]" (getf signature :why)))
      (:none "unsigned"))))

(defun trusted-signer-words (trusted)
  "Who TRUSTED, an entry of *PACKAGE-TRUST*, says signed the versions taken, in words."
  (cond ((string= (second trusted) "unsigned") "no one (unsigned)")
        ((uiop:string-prefix-p "key:" (second trusted))
         (format nil "the GPG key ~a, not yet in your keyring" (subseq (second trusted) 4)))
        ((third trusted) (format nil "~a (key ~a)" (third trusted) (second trusted)))
        (t (format nil "the key ~a" (second trusted)))))

(defun package-trust-verdict (where signature)
  "What becomes of a version of the source WHERE that SIGNATURE
\(PACKAGE-SIGNATURE) says who signed, as three values. The first is :TAKE, :ASK
\(its first version, whoever signed it is asked about, as the version is) or
:REFUSE. The second is who signed it in words, or why it is refused. The third
is the entry of *PACKAGE-TRUST* to remember once it is taken (for one refused,
to remember all the same), or NIL. With the signers the init file names
\(:signers), only a version one of them signed is taken. Else the signer of the
first version taken is remembered. Later ones signed by that key are taken, and
others refused, unsigned ones too. A source whose first version was unsigned has
its unsigned versions taken, and the first signed one taken makes its signer the
one remembered. A GNU Privacy Guard (GPG) signature whose key your keyring lacks
is refused until the key is imported. Its fingerprint is remembered for a
source's first version, so that only that key (or the key it is a subkey of) is
taken then. A signature that does not hold, or one git could not check, is
refused always. With *PACKAGE-CHECK-SIGNATURES* NIL every version is taken,
whoever signed it, and nothing is remembered."
  (when (not *package-check-signatures*)
    (return-from package-trust-verdict (values :take (signer-words signature) nil)))
  (let* ((status (getf signature :status))
         (fingerprint (getf signature :fingerprint))
         (good (eq status :good))
         (words (signer-words signature))
         (remember (and good (list where fingerprint (getf signature :name) (getf signature :key))))
         (named (package-signers-named where))
         (trusted (state-list-find *package-trust* where))
         (pinned-id (and trusted (uiop:string-prefix-p "key:" (second trusted)) (subseq (second trusted) 4))))
    (cond ((member status '(:bad :error)) (values :refuse words nil))
          (named (cond ((and good (member fingerprint named :test #'string=)) (values :take words remember))
                       ((and good (member (getf signature :subkey) named :test #'equal))
                        (values :refuse (format nil "~a; its :signers name the key's signing subkey, ~a: name the key's own fingerprint, ~a"
                                                words (getf signature :subkey) fingerprint)
                                nil))
                       (t (values :refuse (format nil "~a, by none of the signers its :signers name" words) nil))))
          ((eq status :unchecked)
           (values :refuse words (and (null trusted) (list where (format nil "key:~a" (getf signature :key-id)) nil nil))))
          (pinned-id (if (and good (or (string= fingerprint pinned-id) (equal (getf signature :subkey) pinned-id)))
                         (values :take words remember)
                         (values :refuse (format nil "~a, and its first version was signed by ~a" words (trusted-signer-words trusted)) nil)))
          ((null trusted) (values :ask words (or remember (list where "unsigned" nil nil))))
          ((string= (second trusted) "unsigned") (values :take words remember))
          ((and good (string= fingerprint (second trusted))) (values :take words nil))
          (t (values :refuse (format nil "~a, and the versions taken before were signed by ~a" words (trusted-signer-words trusted))
                     nil)))))
