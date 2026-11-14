;;;; lisp/files/listing.lisp --- A directory listed as the walks of files see it, and a text file read whole.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every walk of the files under a directory (searching them, choosing one,
;;;; keeping a directory read) lists each directory one way: its files and
;;;; directories by name, hidden names, backups and links to directories left
;;;; out, its .gitignore read. A directory kept read gives its listing from
;;;; memory instead. The text of a file is read for searching only when it is
;;;; text: never a picture, a program or an encrypted file.

(in-package #:tuatara.defaults)

(defun compact-name (string)
  "STRING, a file's name or path, as a string of one octet a character when
every character of it is ASCII, as most names are, else as it is: an index
keeps hundreds of thousands of them, and a string of any characters takes
four octets for each."
  (if (and (not (typep string 'simple-base-string)) (every (lambda (c) (< (char-code c) 128)) string))
      (coerce string 'simple-base-string)
      string))

(defun compact-join (&rest parts)
  "PARTS, names or paths, joined as COMPACT-NAME keeps one: made at once at
one octet a character when every part is such a string already."
  (if (every (lambda (part) (typep part 'simple-base-string)) parts)
      (apply #'concatenate 'simple-base-string parts)
      (compact-name (apply #'concatenate 'string parts))))

(define-record (index-directory (:constructor make-index-directory (path entries rules)) (:copier nil))
  "A directory as an index last listed it: PATH, its native name ending in /;
ENTRIES, its files and directories in the order listed, each (NAME KIND
STAMP), KIND :REGULAR or :DIRECTORY, hidden names, backups and links to
directories left out; RULES, those of its own .gitignore (GITIGNORE-RULES)."
  (path nil :documentation "The native name of the directory of INDEX-DIRECTORY, ending in /.")
  (entries nil
           :documentation "The files and directories of INDEX-DIRECTORY in the order listed, each (NAME KIND STAMP).")
  (rules nil :documentation "The rules of the .gitignore file of INDEX-DIRECTORY, or NIL."))

(defun list-index-directory (path &key (rules t) hidden)
  "The directory PATH (a native name ending in /) listed as an index lists it
(INDEX-DIRECTORY): its files and directories by name, hidden names (but with
HIDDEN), backups and links to directories left out, each entry examined once
\(TUATARA.IO:DIRECTORY-ENTRIES); with RULES, those of its .gitignore read."
  (let* ((all (ignoring-errors-reported ("listing ~a" path) (tuatara.io:directory-entries path :stamps t)))
         (entries (loop for (name kind nil own stamp) in all
                        unless (or (and (not hidden) (char= (char name 0) #\.)) (backup-name-p name))
                          if (and (eq kind :directory) (eq own :directory))
                            collect (list (compact-name name) :directory stamp)
                          else if (eq kind :regular)
                                 collect (list (compact-name name) :regular stamp)))
         (rules (and rules
                     (find-if (lambda (e) (and (string= (first e) ".gitignore") (eq (second e) :regular))) all)
                     (gitignore-rules (concatenate 'string path ".gitignore") path))))
    (make-index-directory (compact-name path) entries rules)))

(defvar *directory-listing-functions* '()
  "Functions of a directory's native name ending in /, each giving that
directory as what keeps it read has it listed (INDEX-DIRECTORY), or NIL:
DIRECTORY-LISTING asks them before it lists the directory from the disk.")

(defun directory-listing (path &key (rules t) hidden)
  "The directory PATH (a native name ending in /) listed (INDEX-DIRECTORY): as
what keeps it read has it (*DIRECTORY-LISTING-FUNCTIONS*, a directory of
*INDEXED-DIRECTORIES*), nothing asked of the disk, else listed now
\(LIST-INDEX-DIRECTORY, its .gitignore read with RULES); with HIDDEN, names
starting with a dot too, always from the disk, as an index keeps none. Every
walk of files under a directory lists them so."
  (or (and (not hidden) (some (lambda (function) (funcall function path)) *directory-listing-functions*))
      (list-index-directory path :rules rules :hidden hidden)))

(defun read-text-file (path &key most)
  "The octets of the file at PATH, or NIL when it cannot be read or is no
regular file. NIL too when it looks binary (a zero octet among its first 64 KB,
as pictures, video and programs have: TUATARA.IO:BINARY-HEAD-P, as opening a
file tells it). NIL also when it is of a kind its first octets tell
\(TUATARA.IO:OCTETS-KIND: a PDF, a picture, an archive, a disk image; a drawing
written as text, SVG, is text, and so is UTF-8 with a byte order mark). NIL as
well when it is encrypted, as its name or its header says (ENCRYPTED-FILE-P).
So only text is read for searching, and the text of an encrypted file never is.
With MOST, a file larger than MOST octets is not read beyond its first 64 KB,
its size known from the system: NIL, with :TOO-LARGE as a second value, so that
a gigantic file never passes through memory for a search that would not keep
it. Its access time is left alone where the system allows, and a named pipe
cannot hold it up."
  (unless (encrypted-name-p (tuatara.io:native-file-name path))
    (ignoring-errors-reported ("reading ~a to search" path)
      (handler-case
          (tuatara.io:call-with-file
           path
           (lambda (fd path size)
             ;; a small file read whole at once; of a larger one, the part that tells it binary
             (let ((head (tuatara.io:read-fd-start fd path size tuatara.io:+binary-sample+ :past-size t)))
               (unless (or (tuatara.io:binary-head-p head)
                           (encrypted-head-p (subseq head 0 (min (length head) 200)))
                           ;; a kind of file told by its first octets: a picture, a PDF, an archive, a disk image
                           (not (member (nth-value 1 (tuatara.io:octets-kind head)) '(nil :svg :utf-8))))
                 (cond ((and most (> (max size (length head)) most)) (values nil :too-large))
                       ((< (length head) (min tuatara.io:+binary-sample+ (max 8192 (1+ size)))) head)
                       ;; larger than the system said (/proc, /sys) or than the head: read whole
                       (t (let ((octets (tuatara.io:read-fd-octets fd :path path :size-hint (1+ size))))
                            (and (not (tuatara.io:binary-head-p octets)) octets))))))))
        (tuatara.io:file-system-error (condition) (values nil condition))))))
