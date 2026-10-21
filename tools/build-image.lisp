;;;; tools/build-image.lisp --- Save the whole editor as one executable, build/tuatara.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;; Run by tools/build-image.sh, inside the toolbox, with the heap size to keep.
(load (merge-pathnames "quicklisp.lisp" *load-truename*))
;; the Tuatara of this folder, whatever other copies ASDF knows of
(let ((tree (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*))))
  (push tree asdf:*central-registry*)
  (ql:quickload :tuatara/terminal :silent t)
  (unless (uiop:pathname-equal tree (asdf:system-source-directory :tuatara))
    (format *error-output* "Tuatara was loaded from ~a, not ~a~%" (asdf:system-source-directory :tuatara) tree)
    (sb-ext:exit :code 1)))
(let ((threads (remove sb-thread:*current-thread* (sb-thread:list-all-threads))))
  (when threads
    (format *error-output* "Threads still running, the image cannot be saved: ~{~a~^, ~}~%"
            (mapcar #'sb-thread:thread-name threads))
    (sb-ext:exit :code 1)))
(ensure-directories-exist "build/")
;; every Lisp system the program carries and the folder of its sources (none
;; for those SBCL carries compiled), for tools/release.sh to give their licences
(with-open-file (out "build/lisp-systems.tsv" :direction :output :if-exists :supersede)
  (dolist (name (sort (remove-duplicates (mapcar #'asdf:primary-system-name (asdf:already-loaded-systems))
                                         :test #'string=)
                      #'string<))
    (let ((directory (asdf:system-source-directory (asdf:find-system name))))
      (format out "~a~c~@[~a~]~%" name #\Tab (and directory (uiop:native-namestring directory))))))
(sb-ext:gc :full t)
;; the heap and stack sizes this process was started with are kept, as the
;; defaults; the executable still takes --dynamic-space-size and
;; --control-stack-size, and every other argument goes to the editor, which
;; starts again with another heap where the machine wants one; not compressed,
;; so that it starts at once
(setf tuatara.terminal:*built-heap* (sb-ext:dynamic-space-size))
;; what it was built from (tools/build-image.sh): commit, changes not committed, day
(let ((build (uiop:split-string (or (uiop:getenv "TUATARA_BUILD") "") :separator " ")))
  (when (= (length build) 3)
    (setf tuatara:*build* (list (first build) (string= (second build) "t") (third build)))))
(setf tuatara:*built-at* (get-universal-time))
(sb-ext:save-lisp-and-die "build/tuatara" :executable t :save-runtime-options :accept-runtime-options
                                          :toplevel #'tuatara.terminal:image-toplevel)
