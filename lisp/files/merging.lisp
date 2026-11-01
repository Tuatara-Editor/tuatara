;;;; lisp/files/merging.lisp --- three files merged into one, conflicts marked, by git merge-file shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; For a program that runs the editor to merge (git mergetool): the file to
;;;; merge into shows the merge of the local, base and remote files with the
;;;; conflicts marked, to be resolved as any marked conflicts are
;;;; (lisp/editing/conflicts.lisp) and saved. merge-file runs in the sandbox
;;;; (TUATARA.IO:SANDBOX-RUN), seeing only the folders of the three, read only.

(in-package #:tuatara.defaults)

(defun three-way-merge (local base remote &key cancel)
  "The text of the merge of LOCAL and REMOTE, files, from their common
ancestor BASE (a file, or /dev/null for none), as an octet vector: the lines
changed in one only taken as changed, those changed in both ways between
<<<<<<<, |||||||, ======= and >>>>>>> lines. merge-file, shut in, sees only
the folders of the three, read only. Stopped when CANCEL's car turns true; an
error saying why when it cannot be done."
  (multiple-value-bind (output outcome said)
      (tuatara.io:sandbox-run "git" (list "merge-file" "-p" "--diff3" "-L" "local" "-L" "base" "-L" "remote"
                                          (tuatara.io:real-place local) (tuatara.io:real-place base) (tuatara.io:real-place remote))
                              :output-limit *differences-limit* :cancel cancel :ended-well (loop for n from 0 to 127 collect n)
                              :directories (remove-duplicates
                                            (mapcar #'sandbox-seen-directory (remove "/dev/null" (list local base remote) :test #'equal))
                                            :test #'string=))
    (unless (eq outcome :done)
      (error "~a" (tuatara.io:sandbox-outcome-words outcome said :label "git merge-file")))
    output))

(defstruct (merge-job (:include tuatara.runtime:job) (:constructor make-merge-job (buffer local base remote version))
                      (:copier nil))
  "The merge of the files LOCAL, BASE and REMOTE being made for BUFFER, a job, begun on
its text of version number VERSION; OCTETS, the merge."
  local base remote version octets)

(setf (documentation 'merge-job-p 'function) "True when OBJECT is a MERGE-JOB.")

(tuatara.runtime:define-job-words merge-job
  :doing "Merging"
  :description (format nil "Merging ~a and ~a" (tuatara.io:native-file-name (merge-job-local job))
                       (tuatara.io:native-file-name (merge-job-remote job)))
  :verb "merge"
  :left-words "nothing merged"
  :at-exit :quiet)

(defun merge-resolved-p (buffer path)
  "True when the merge into BUFFER, visiting PATH, is done: the buffer holds
nothing unsaved and the file PATH has no conflict marked in it."
  (and (not (and (live-buffer-p buffer) (tuatara.buffer:buffer-modified-p buffer)))
       (let ((octets (ignore-errors (tuatara.io:file-octets path))))
         (and octets (null (conflict-find-all (tuatara.text:octets->rope (coerce octets 'tuatara.text:octets))))))))

(defmethod tuatara.runtime:job-take-in ((job merge-job))
  "Put the merge in the buffer, its conflicts marked, and say how many there
are, going to the first; or say why it could not be made, or that the file was changed meanwhile."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (live-buffer-p buffer)
      (cond ((not (eq (tuatara.runtime:job-status job) :done))
             (message "~a could not be merged: ~a" (tuatara.buffer:buffer-name buffer) (tuatara.runtime:job-ending-words job)))
            ((/= (merge-job-version job) (tuatara.text:version-number (tuatara.buffer:buffer-version buffer)))
             (message "~a was changed meanwhile: the merge is not put in it" (tuatara.buffer:buffer-name buffer)))
            (t
             (let ((rope (tuatara.text:octets->rope (coerce (merge-job-octets job) 'tuatara.text:octets))))
               ;; the conflicts of the file as it was are no more
               (setf (tuatara.buffer:buffer-local buffer 'conflict-list) nil)
               (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) rope)
               (setf (tuatara.buffer:buffer-point buffer) 0)
               (if (conflict-find-all rope)
                   ;; the conflicts are looked for, said, and gone to, at once
                   (conflict-look-through buffer :tell :found :go t)
                   (message "Merged without conflicts: \\[save-buffer] to finish"))))))))

(defun merge-files-into (local base remote merged)
  "Merge the files LOCAL, BASE and REMOTE in the background into the buffer
visiting MERGED, its conflicts marked and left to resolve; saving it writes
MERGED. The editor ends with the code 0 when MERGED is saved with no conflict
left in it, else 1. A buffer that is read only (a file of evidence) is left as
it is, and that is said."
  (let ((buffer (buffer-visiting merged)))
    (cond ((null buffer) (message "~a could not be opened: nothing merged" (abbreviated-path merged)))
          ((tuatara.buffer:buffer-local buffer :read-only)
           (message "~a is read only: the merge is not put in it" (abbreviated-path merged)))
          (t
           (let ((base (if (tuatara.io:path-kind base) base "/dev/null")))
             (tuatara.runtime:start-job-thread
              (make-merge-job buffer local base remote (tuatara.text:version-number (tuatara.buffer:buffer-version buffer)))
              (lambda (job)
                (setf (merge-job-octets job)
                      (three-way-merge (merge-job-local job) (merge-job-base job) (merge-job-remote job)
                                       :cancel (tuatara.runtime:job-cancel job))))
              :name "tuatara merge"))))
    (setf tuatara.command:*exit-functions*
          (append tuatara.command:*exit-functions*
                  (list (lambda (editor)
                          (declare (ignore editor))
                          (setf tuatara.command:*exit-code* (if (merge-resolved-p buffer merged) 0 1))))))))
