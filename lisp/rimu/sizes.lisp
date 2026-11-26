;;;; lisp/rimu/sizes.lisp --- Rimu's sizes column: each entry's size, a directory's with all it holds.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; S in a rimu list shows before each name how much it takes: a file its
;;;; size, a directory the size of everything in it and under it, as Emacs's
;;;; dired-du shows it. The directories are walked on a thread of their own,
;;;; every entry counted, hidden ones too (unlike the walks that look for files
;;;; to read, which leave them out), a file with several names once, links
;;;; never followed; the column fills in when the walk is done. In a list of a
;;;; directory on another machine the walk reads each directory below through
;;;; the list's SFTP connection (REMOTE-TREE-SIZE), nothing run there.

(in-package #:tuatara.defaults)

(defun rimu-sizes-p (buffer)
  "True when the rimu list BUFFER shows the sizes column
\(\\<rimu-mode>\\[rimu-toggle-sizes]): never in a list opened safely, as summing
them reads every directory below, nor in one of a directory as a commit left
it, whose directories are not on the disk."
  (rimu-column-allowed-p buffer 'rimu-sizes 'rimu-toggle-sizes))

(defvar *rimu-size-walks* (make-hash-table :test 'equal :synchronized t)
  "By directory, which walk of its sizes column is the latest, a number: an
older walk of the same directory ends (RIMU-SIZE-CELLS).")

(defun rimu-size-listing-p (buffer directory)
  "True when BUFFER, a rimu list, lists DIRECTORY, or is about to: one of the
home directory of another machine (/ssh:HOST:) takes the name its server gives
that directory once it is read."
  (let ((listed (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (or (equal directory listed)
        (and (remote-name-p listed) (remote-name-p directory)
             (= (remote-prefix-length listed) (length listed))
             (uiop:string-prefix-p listed directory)))))

(defun rimu-size-cells (directory entries)
  "The sizes column's texts for ENTRIES of DIRECTORY: :PENDING, worked out on
a thread of its own (TUATARA.IO:TREE-SIZE for a directory, REMOTE-TREE-SIZE
for one on another machine) and given to the lists by RIMU-COLUMN-READY. The
walk ends once no list of DIRECTORY shows the column, or a newer walk of
DIRECTORY begins, as when it is read again after a change; on another machine
also when the connection is lost or does not answer in time, the sizes not
yet found left empty."
  (let ((walk (sb-ext:with-locked-hash-table (*rimu-size-walks*)
                (incf (gethash directory *rimu-size-walks* 0)))))
    (flet ((wanted ()
             (and (eql walk (gethash directory *rimu-size-walks*))
                  (some (lambda (buffer)
                          (and (rimu-list-p buffer) (rimu-sizes-p buffer) (rimu-size-listing-p buffer directory)))
                        tuatara.buffer:*buffers*))))
      (rimu-column-in-background
       :sizes entries "tuatara rimu sizes column"
       (if (remote-name-p directory)
           (lambda (names)
             (declare (ignore names))
             (multiple-value-bind (host path) (remote-name-parts directory)
               (let ((lost nil))
                 (map 'vector
                      (lambda (entry)
                        (let* ((name (tuatara.io:listed-name entry))
                               (stamp (tuatara.io:listed-stamp entry))
                               (size (and (not lost) stamp (tuatara.io:entry-name-p name) (wanted)
                                          (case (tuatara.io:stamp-kind stamp)
                                            (:directory
                                             (handler-case (remote-tree-size host (concatenate 'string path name "/")
                                                                             :wanted #'wanted)
                                               (tuatara.io:file-system-error () (setf lost t) nil)))
                                            (:regular (tuatara.io:stamp-size stamp))))))
                          (and size (cons (rimu-size-words size) :list-size))))
                      entries))))
           (lambda (names)
             (let ((seen (make-hash-table :test 'equal)))
               (map 'vector
                    (lambda (name)
                      (when (and (tuatara.io:entry-name-p name) (wanted))
                        (let* ((path (concatenate 'string directory name))
                               (stamp (tuatara.io:path-stamp path :follow nil :failure nil))
                               (size (case (and stamp (tuatara.io:stamp-kind stamp))
                                       (:directory (tuatara.io:tree-size (concatenate 'string path "/")
                                                                         :seen seen :wanted #'wanted))
                                       (:regular (tuatara.io:stamp-size stamp)))))
                          (and size (cons (rimu-size-words size) :list-size)))))
                    names))))))))

(define-command rimu-toggle-sizes ()
  "Show or hide, before each name in this list, how much it takes: a file
its size, a directory the size of everything in it and under it, hidden
files too, directories of another drive mounted in it left out, worked out in
the background while the list shows it. *RIMU-DIRECTORY-SIZES* says
whether new lists show it. In a list of another machine each directory is
read through its connection, and as SFTP tells of no file's identity nor
drive, a file with several names is counted once for each and a drive mounted
below is counted in."
  (rimu-toggle 'rimu-sizes "Sizes shown" "Sizes hidden"))

(defun install-rimu-sizes ()
  "Make the sizes column, and bind S to show or hide it; refuse it in a list
of a directory not on the disk, unless its source works it out itself."
  (rimu-set-column :sizes 'rimu-size-cells :place :before :align :right :shown 'rimu-sizes-p :interim t
                   :documentation "How much the entry takes: a directory with everything under it; on another
machine a file of several names counted once for each, a drive mounted below counted in.")
  (define-key *rimu-keymap* "S" 'rimu-toggle-sizes)
  (pushnew 'rimu-toggle-sizes *rimu-disk-commands*))
