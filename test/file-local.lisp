;;;; test/file-local.lisp --- Settings a file gives itself (-*- ... -*-, Local Variables:).
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test file-local
  :description "A file's own settings, on its first line between -*- and -*- or in a Local Variables: block near its end: its mode, tab width, tabs or spaces and fill column taken, nothing run, nothing for a file opened safely.")

(defun opened-settings (dir name text &key safe)
  "Open a file NAME holding TEXT in DIR (safely with SAFE); its major mode's name,
tab width, whether tabs indent, fill column, and what the echo area said."
  (let ((path (merge-pathnames name dir)))
    (alexandria:write-string-into-file text path)
    (multiple-value-bind (f editor)
        (run-keys (format nil "~:[~;C-u ~]C-x C-f C-a C-k ~a RET" safe (rimu-typed (namestring path))) :width 160)
      (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (prog1 (list (string-downcase (symbol-name (type-of (tuatara.modes:buffer-major-mode buffer))))
                     (tuatara.defaults::buffer-tab-width buffer)
                     (tuatara.defaults::buffer-indent-tabs-p buffer)
                     (tuatara.defaults::buffer-fill-column buffer)
                     f)
          (forget-file-buffers))))))

(define-test (file-local first-line-and-end-block)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil))
      ;; the first line names the mode, its width and fill
      (destructuring-bind (mode tab tabs fill f) (opened-settings dir "notes.txt" (format nil "# -*- mode: org; tab-width: 8; fill-column: 72 -*-~%* A~%"))
        (declare (ignore tabs f))
        (is string= "org-mode" mode)
        (is = 8 tab)
        (is = 72 fill))
      ;; a lone word: the mode; after #!, the second line
      (is string= "org-mode" (first (opened-settings dir "a.txt" (format nil "-*- org -*-~%text~%"))))
      (is string= "org-mode" (first (opened-settings dir "run" (format nil "#!/bin/sh~%# -*- mode: org -*-~%"))))
      ;; a block at the end, its lines with the same text before and after
      (destructuring-bind (mode tab tabs fill f)
          (opened-settings dir "b.c" (format nil "int x;~%/* Local Variables: */~%/* indent-tabs-mode: t */~%/* tab-width: 3 */~%/* End: */~%"))
        (declare (ignore mode fill f))
        (is = 3 tab)
        (true tabs))
      ;; nothing run, nothing out of range, said so; the rest taken
      (destructuring-bind (mode tab tabs fill f)
          (opened-settings dir "c.txt" (format nil "x~%;; Local Variables:~%;; eval: (delete-file \"x\")~%;; tab-width: 900~%;; fill-column: 60~%;; End:~%"))
        (declare (ignore mode tabs))
        (is = 4 tab)
        (is = 60 fill)
        (true (tuatara.frontend.fake:fake-frame-showing f "left alone: eval, tab-width")))
      ;; a list of Tuatara's own is no mode for a file
      (is string/= "buffers-mode" (first (opened-settings dir "d.txt" (format nil "-*- mode: buffers -*-~%"))))
      ;; nor a mode that runs what is typed: the Lisp prompt, *scratch*'s Lisp
      (is string= "text-mode" (first (opened-settings dir "g.txt" (format nil ";; -*- mode: ielm -*-~%X> (+ 1 2)~%"))))
      (is string= "text-mode" (first (opened-settings dir "h.txt" (format nil ";; -*- mode: lisp-interaction -*-~%(+ 1 2)~%"))))
      ;; a mode some files are opened in is taken, fundamental-mode too
      (is string= "lisp-mode" (first (opened-settings dir "i.txt" (format nil ";; -*- mode: lisp -*-~%"))))
      (is string= "fundamental-mode" (first (opened-settings dir "j.txt" (format nil "-*- mode: fundamental -*-~%"))))
      ;; opened safely: never read for them
      (is string/= "org-mode" (first (opened-settings dir "e.txt" (format nil "-*- mode: org -*-~%") :safe t)))
      ;; off: not read
      (let ((tuatara-user:*file-local-variables* nil))
        (is string/= "org-mode" (first (opened-settings dir "f.txt" (format nil "-*- mode: org -*-~%"))))))))

(define-test (file-local spaces-between-sentences)
  (with-test-directory (dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (path (merge-pathnames "two.txt" dir)))
      ;; the file says its sentences are two spaces apart: taken, and known as said by the file
      (alexandria:write-string-into-file (format nil "One.  Two.~%~%# Local Variables:~%# sentence-end-double-space: t~%# End:~%") path)
      (let* ((editor (nth-value 1 (run-keys (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring path))) :width 160)))
             (buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (true (tuatara.defaults::sentence-end-double-space-p buffer))
        (is equal '(t t) (multiple-value-list (tuatara.defaults::file-declares buffer "sentence-end-double-space")))
        (is equal '(nil nil) (multiple-value-list (tuatara.defaults::file-declares buffer "fill-column")))
        (forget-file-buffers))
      ;; a value that is no yes or no is left alone
      (alexandria:write-string-into-file (format nil "x~%# Local Variables:~%# sentence-end-double-space: 3~%# End:~%") path :if-exists :supersede)
      (let* ((editor (nth-value 1 (run-keys (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring path))) :width 160)))
             (buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (false (tuatara.defaults::sentence-end-double-space-p buffer))
        (false (nth-value 1 (tuatara.defaults::file-declares buffer "sentence-end-double-space")))
        (forget-file-buffers)))))

(define-test (file-local settings-written)
  (flet ((written (text mode name value)
           (let ((b (tuatara.buffer:make-buffer "*file-local-written*" :text text)))
             (unwind-protect
                  (progn
                    (when mode
                      (tuatara.defaults::set-major-mode b (tuatara.defaults::mode-named mode)))
                    (tuatara.defaults::set-file-local-setting b name value)
                    (true (nth-value 1 (tuatara.defaults::file-declares b name)))
                    (is eq value (tuatara.defaults::file-declares b name))
                    (tuatara.buffer:buffer-string b))
               (tuatara.buffer:kill-buffer b)))))
    ;; a block added at the end, in the language's comments
    (is string= (format nil "* A~%~%# Local Variables:~%# sentence-end-double-space: t~%# End:~%")
        (written (format nil "* A~%") 'tuatara.defaults::org-mode "sentence-end-double-space" t))
    (is string= (format nil "x~%~%;; Local Variables:~%;; sentence-end-double-space: nil~%;; End:~%")
        (written "x" 'tuatara.defaults::lisp-mode "sentence-end-double-space" nil))
    (is string= (format nil "x~%~%<!-- Local Variables: -->~%<!-- sentence-end-double-space: t -->~%<!-- End: -->~%")
        (written (format nil "x~%") 'tuatara.defaults::markdown-mode "sentence-end-double-space" t))
    (is string= (format nil "x~%~%Local Variables:~%sentence-end-double-space: t~%End:~%")
        (written (format nil "x~%") nil "sentence-end-double-space" t))
    ;; a line of the block changed, the rest kept
    (is string= (format nil "x~%# Local Variables:~%# fill-column: 60~%# sentence-end-double-space: nil~%# End:~%")
        (written (format nil "x~%# Local Variables:~%# fill-column: 60~%# sentence-end-double-space: t~%# End:~%")
                 'tuatara.defaults::org-mode "sentence-end-double-space" nil))
    ;; one added before End:
    (is string= (format nil "x~%# Local Variables:~%# fill-column: 60~%# sentence-end-double-space: t~%# End:~%")
        (written (format nil "x~%# Local Variables:~%# fill-column: 60~%# End:~%")
                 'tuatara.defaults::org-mode "sentence-end-double-space" t))))

(define-test (file-local settings-written-into-any-block)
  (flet ((written (text name value &key crlf)
           (let ((b (tuatara.buffer:make-buffer "*file-local-written*" :text text)))
             (unwind-protect
                  (progn
                    (tuatara.defaults::set-major-mode b (tuatara.defaults::mode-named 'tuatara.defaults::org-mode))
                    (when crlf (setf (tuatara.buffer:buffer-local b :line-ending) :crlf))
                    (tuatara.defaults::set-file-local-setting b name value)
                    (tuatara.buffer:buffer-string b))
               (tuatara.buffer:kill-buffer b))))
         (crlf (&rest lines) (format nil "~{~a~c~%~}" (loop for line in lines append (list line #\Return)))))
    ;; a block with no End: and no last line end: the setting on a line of its own
    (is string= (format nil "x~%# Local Variables:~%# tab-width: 7~%# fill-column: 20~%# End:~%")
        (written (format nil "x~%# Local Variables:~%# tab-width: 7") "fill-column" 20))
    ;; lines ending in a carriage return and a newline keep doing so
    (is string= (crlf "x" "# Local Variables:" "# tab-width: 3" "# End:")
        (written (crlf "x" "# Local Variables:" "# tab-width: 7" "# End:") "tab-width" 3 :crlf t))
    (is string= (crlf "x" "# Local Variables:" "# tab-width: 7" "# fill-column: 3" "# End:")
        (written (crlf "x" "# Local Variables:" "# tab-width: 7" "# End:") "fill-column" 3 :crlf t))
    (is string= (crlf "x" "" "# Local Variables:" "# fill-column: 3" "# End:")
        (written (crlf "x") "fill-column" 3 :crlf t))
    ;; the words in prose are no block: one is added after it, the prose left alone
    (is string= (format nil "See Local Variables: in the manual~%~%# Local Variables:~%# tab-width: 3~%# End:~%")
        (written (format nil "See Local Variables: in the manual~%") "tab-width" 3)))
  (is equal '() (tuatara.defaults::end-block-settings (format nil "See Local Variables: in the manual~%tab-width: 4~%")))
  (is equal '(("tab-width" . 4)) (tuatara.defaults::end-block-settings (format nil "<!-- Local Variables: -->~%<!-- tab-width: 4 -->~%")))
  ;; a number too long for any setting is kept as text
  (is equal '(("tab-width" . "1234567890")) (tuatara.defaults::end-block-settings (format nil "Local Variables:~%tab-width: 1234567890~%End:~%"))))

(define-test (file-local first-line-of-wide-text)
  ;; a file whose first kilobyte ends, or whose end read begins, inside a character is read all the same
  (let ((b (tuatara.buffer:make-buffer "*wide-head*" :text (format nil "-*- fill-column: 60 -*-~%~a" (make-string 9001 :initial-element #\€)))))
    (unwind-protect
         (is equal '(("fill-column" . 60)) (tuatara.defaults::file-local-settings b))
      (tuatara.buffer:kill-buffer b))))
