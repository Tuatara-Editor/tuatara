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
never in a list opened safely, as summing them reads every directory below, nor
in one of a directory as a commit left it, whose directories are not on the disk."
  (rimu-column-allowed-p buffer 'rimu-sizes))

(defvar *rimu-size-walks* (make-hash-table :test 'equal :synchronized t)
  "By directory, which walk of its sizes column is the latest, a number: an
older walk of the same directory ends (RIMU-SIZE-CELLS).")

(defun rimu-size-cells (directory entries)
  "The sizes column's texts for ENTRIES of DIRECTORY: :PENDING, worked out on
a thread of its own (TUATARA.IO:TREE-SIZE for a directory) and given to the
lists by RIMU-COLUMN-READY. The walk ends once no list of DIRECTORY shows the
column, or a newer walk of DIRECTORY begins, as when it is read again after a
change."
  (let ((walk (sb-ext:with-locked-hash-table (*rimu-size-walks*)
                (incf (gethash directory *rimu-size-walks* 0)))))
    (flet ((wanted ()
             (and (eql walk (gethash directory *rimu-size-walks*))
                  (some (lambda (buffer)
                          (and (rimu-list-p buffer) (rimu-sizes-p buffer)
                               (equal directory (tuatara.buffer:buffer-local buffer 'rimu-directory))))
                        tuatara.buffer:*buffers*))))
      (rimu-column-in-background
       :sizes entries "tuatara rimu sizes column"
       (lambda (names)
         (let ((seen (make-hash-table :test 'equal)))
           (map 'vector
                (lambda (name)
                  (unless (or (member name '("." "..") :test #'string=) (not (wanted)))
                    (let* ((path (concatenate 'string directory name))
                           (stamp (tuatara.io:path-stamp path :follow nil :failure nil))
                           (size (case (and stamp (tuatara.io:stamp-kind stamp))
                                   (:directory (tuatara.io:tree-size (concatenate 'string path "/")
                                                               :seen seen :wanted #'wanted))
                                   (:regular (tuatara.io:stamp-size stamp)))))
                      (and size (cons (rimu-size-words size) :list-size)))))
                names)))))))

(define-command rimu-toggle-sizes ()
  "Show or hide, before each name in this list, how much it takes: a file
its size, a directory the size of everything in it and under it, hidden
files too, directories of another drive mounted in it left out, worked out in
the background while the list shows it. *RIMU-DIRECTORY-SIZES* says
whether new lists show it."
  (rimu-toggle 'rimu-sizes "Sizes shown" "Sizes hidden"))

(defun install-rimu-sizes ()
  "Make the sizes column, and bind S to show or hide it; refuse it in a list
of a directory not on the disk."
  (rimu-set-column :sizes 'rimu-size-cells :place :before :align :right :shown 'rimu-sizes-p :interim t
                   :documentation "How much the entry takes: a directory with everything under it.")
  (define-key *rimu-keymap* "S" 'rimu-toggle-sizes)
  (pushnew 'rimu-toggle-sizes *rimu-disk-commands*))
