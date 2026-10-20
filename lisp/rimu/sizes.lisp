;;;; lisp/rimu/sizes.lisp --- Rimu's sizes column: each entry's size, a directory's with all it holds.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; S in a rimu list shows before each name how much it takes: a file its
;;;; size, a directory the size of everything in it and under it, as Emacs's
;;;; dired-du shows it. The directories are walked on a thread of their own,
;;;; every entry counted, hidden ones too (unlike the walks that look for files
;;;; to read, which leave them out), a file with several names once, links
;;;; never followed; the column fills in when the walk is done.

(in-package #:tuatara.defaults)

(defun rimu-sizes-p (buffer)
  "True when the rimu list BUFFER shows the sizes column (\\<rimu-mode>\\[rimu-toggle-sizes]):
never in a list opened safely, as summing them reads every folder below, nor
in one of a folder as a commit left it, whose folders are not on the disk."
  (rimu-column-allowed-p buffer 'rimu-sizes))

(defun rimu-tree-size (directory &key (seen (make-hash-table :test 'equal)) wanted)
  "The octets the files in DIRECTORY (a native name ending in /) and under it
take, hidden ones too: each file once however many names it has (SEEN, a
table of those counted), links not followed, and folders of another file
system (a drive mounted below it, /proc) left out. WANTED, a function of no
arguments, is asked every 64 folders whether the sum is still wanted: NIL is
given when it says no."
  (let ((total 0) (folders 0))
    (walk-directory directory
                    (lambda (folder relative entry)
                      (declare (ignore folder relative))
                      (if (null entry)
                          (when (and wanted (zerop (mod (incf folders) 64)) (not (funcall wanted)))
                            (return-from rimu-tree-size nil))
                          (destructuring-bind (name kind size own &optional stamp) entry
                            (declare (ignore name))
                            (when (and (eq kind :regular) (eq own kind))
                              (let ((key (and stamp (cons (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)))))
                                (unless (and key (gethash key seen))
                                  (when key (setf (gethash key seen) t))
                                  (incf total (or size 0)))))
                            t)))
                    :same-device t)
    total))

(defun rimu-size-cells (directory entries)
  "The sizes column's texts for ENTRIES of DIRECTORY: :PENDING, worked out on
a thread of its own (RIMU-TREE-SIZE for a directory) and given to the lists by
RIMU-COLUMN-READY; the walk ends once no list of DIRECTORY shows the column."
  (flet ((wanted ()
           (some (lambda (buffer)
                   (and (rimu-list-p buffer) (rimu-sizes-p buffer)
                        (equal directory (tuatara.buffer:buffer-local buffer 'rimu-directory))))
                 tuatara.buffer:*buffers*)))
    (rimu-column-in-background
     :sizes entries "tuatara rimu sizes column"
     (lambda (names)
       (let ((seen (make-hash-table :test 'equal)))
         (map 'vector
              (lambda (name)
                (unless (member name '("." "..") :test #'string=)
                  (let* ((path (concatenate 'string directory name))
                         (stamp (ignore-errors (tuatara.io:path-stamp path :follow nil)))
                         (size (case (and stamp (tuatara.io:stamp-kind stamp))
                                 (:directory (rimu-tree-size (concatenate 'string path "/") :seen seen :wanted #'wanted))
                                 (:regular (tuatara.io:stamp-size stamp)))))
                    (and size (cons (octets-words size) :list-size)))))
              names))))))

(define-command rimu-toggle-sizes ()
  "Show or hide, before each name in this list, how much it takes: a file
its size, a directory the size of everything in it and under it, hidden
files too, folders of another drive mounted in it left out, worked out in
the background while the list shows it. *RIMU-DIRECTORY-SIZES* says
whether new lists show it."
  (rimu-toggle 'rimu-sizes "Sizes shown" "Sizes hidden"))

(defun install-rimu-sizes ()
  "Make the sizes column, and bind S to show or hide it."
  (set-rimu-column :sizes 'rimu-size-cells :place :before :align :right :shown 'rimu-sizes-p
                   :documentation "How much the entry takes: a directory with everything under it.")
  (define-key *rimu-keymap* "S" 'rimu-toggle-sizes))
