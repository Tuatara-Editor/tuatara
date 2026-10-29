;;;; test/wipe.lisp --- Files wiped: overwritten, then deleted; offered once a file is encrypted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test wipe
  :description "A file wiped is overwritten, emptied and deleted, a link, a folder and a file with
other names refused, a stop leaving it there; M-x wipe-file asks first, saying what it cannot promise;
encrypting a file offers to wipe it, at once with keys, with a passphrase only once it, asked again,
opens what was made; gpg-lock makes gpg-agent forget its passphrases.")

(defun wiping-p ()
  "True while a file is being wiped or gpg works."
  (some (lambda (job) (or (tuatara.defaults::wipe-job-p job) (tuatara.defaults::gpg-job-p job))) tuatara.runtime:*jobs*))

(define-test (wipe the-file)
  (with-test-directory (dir)
    (let ((plain (namestring (merge-pathnames "secret.txt" dir)))
          (other (namestring (merge-pathnames "other-name.txt" dir)))
          (link (namestring (merge-pathnames "link.txt" dir))))
      (alexandria:write-string-into-file (make-string 200000 :initial-element #\k) plain)
      ;; a stop leaves it there
      (is eq :stopped (tuatara.io:wipe-file plain :cancel (list t)))
      (true (probe-file plain))
      ;; links and other names refused, the file untouched
      (sb-posix:symlink plain link)
      (fail (tuatara.io:wipe-file link) tuatara.io:file-system-error)
      (sb-posix:link plain other)
      (fail (tuatara.io:wipe-file plain) tuatara.io:file-system-error)
      (sb-posix:unlink other)
      (fail (tuatara.io:wipe-file (namestring dir)) tuatara.io:file-system-error)
      (fail (tuatara.io:wipe-file (namestring (merge-pathnames "none.txt" dir))) tuatara.io:file-system-error)
      ;; wiped: gone, nothing hidden left behind, progress told
      (let ((done 0))
        (true (tuatara.io:wipe-file plain :passes 2 :progress (lambda (n) (setf done n))))
        (is = 400000 done))
      (false (probe-file plain))
      (sb-posix:unlink link)
      (false (uiop:directory-files dir))
      (false (uiop:subdirectories dir)))))

(define-test (wipe the-command)
  (with-test-directory (dir)
    (let ((plain (namestring (merge-pathnames "notes.txt" dir)))
          (tuatara.defaults::*modal-editing* nil))
      (alexandria:write-string-into-file "notes to wipe" plain)
      ;; no: kept
      (multiple-value-bind (f editor)
          (auto-save-session (list (format nil "M-x w i p e - f i l e RET C-a C-k ~a C-j" (rimu-typed plain)) (list :wait 0.2) "n RET"))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Wipe notes.txt for good? (~a)" (tuatara.defaults::wipe-caveat (list plain))))))
      (true (probe-file plain))
      ;; yes: gone
      (multiple-value-bind (f editor)
          (auto-save-session (list (format nil "M-x w i p e - f i l e RET C-a C-k ~a C-j" (rimu-typed plain)) "y RET"
                                   (list :until (lambda () (not (wiping-p))))))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Wiped: notes.txt")))
      (false (probe-file plain)))))

(define-test (wipe wiping-in-rimu)
  ;; g w on a file of a list: asked, then wiped
  (with-rimu-tree (dir)
    (let ((file (namestring (merge-pathnames "text.txt" dir))))
      (true (probe-file file))
      (let ((f (rimu-session dir (format nil "g f ~a RET g w y RET" (rimu-typed "text.txt")) :width 120)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Wipe text.txt for good?")))
      (loop repeat 100 while (probe-file file) do (sleep 0.05))
      (false (probe-file file))
      (rimu-forget))))

(define-test (wipe after-encrypting)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((tuatara.defaults::*modal-editing* nil)
              (tuatara-user:*gpg-passphrase-from* :minibuffer))
          (gpg-test-key)
          (flet ((encrypting (path keys)
                   (auto-save-session (append (cons (format nil "M-x e n c r y p t - f i l e RET C-a C-k ~a C-j" (rimu-typed path)) keys)
                                              (list (list :until (lambda () (not (wiping-p)))) (list :wait 0.3)
                                                    (list :until (lambda () (not (wiping-p)))))))))
            ;; to a key: wiped once said, no passphrase asked
            (let ((plain (namestring (merge-pathnames "keyed.txt" dir))))
              (alexandria:write-string-into-file "keyed words" plain)
              (let ((f (encrypting plain (list "C-n C-n RET RET" (list :until (lambda () (not (wiping-p)))) "y RET"))))
                (true (tuatara.frontend.fake:fake-frame-showing f "Wiped: keyed.txt")))
              (false (probe-file plain))
              (is string= "keyed words" (gpg-test-plain (concatenate 'string plain ".gpg"))))
            ;; with a passphrase: a wrong one asked again keeps the file
            (let ((plain (namestring (merge-pathnames "pass.txt" dir))))
              (alexandria:write-string-into-file "pass words" plain)
              (let ((f (encrypting plain (list "C-n RET RET" "p w RET p w RET" (list :until (lambda () (not (wiping-p))))
                                               "y RET" "w r o n g RET"))))
                (true (tuatara.frontend.fake:fake-frame-showing f "pass.txt is not wiped: the passphrase given does not open pass.txt.gpg")))
              (true (probe-file plain))
              (is string= "pass words" (gpg-test-plain (concatenate 'string plain ".gpg") :passphrase "pw"))
              ;; the right one: wiped
              (sb-posix:unlink (concatenate 'string plain ".gpg"))
              (let ((f (encrypting plain (list "C-n RET RET" "p w RET p w RET" (list :until (lambda () (not (wiping-p))))
                                               "y RET" "p w RET"))))
                (true (tuatara.frontend.fake:fake-frame-showing f "The passphrase again, to check it opens pass.txt.gpg"))
                (true (tuatara.frontend.fake:fake-frame-showing f "Wiped: pass.txt")))
              (false (probe-file plain)))
            ;; the list of keys gone once used, nothing left waiting in it
            (false (tuatara.buffer:find-buffer "*Encryption keys*")))))))

(define-test (wipe gpg-lock)
  (if (null (tuatara.defaults::gpgconf-path))
      (skip "gpgconf is not installed here" (true nil))
      (multiple-value-bind (f editor) (auto-save-session (list "M-x g p g - l o c k RET"))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "gpg-agent has forgotten the passphrases it kept")))))
