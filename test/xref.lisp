;;;; test/xref.lisp --- Going to definitions: M-. M-, C-M-, and the *xref* list.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test xref
  :description "M-. goes to the definition of the Lisp name at point, from the files the editor was
built from and the buffers definitions were evaluated from; several are listed in *xref*; M-, comes
back and C-M-, goes forward again.")

(defun session-point-chars (editor)
  "Point of EDITOR's current buffer, in characters from the start of its text."
  (let ((b (session-buffer editor)))
    (length (tuatara.buffer:buffer-string b :start 0 :end (tuatara.buffer:buffer-point b)))))

(define-test (xref finding-definitions-in-text)
  (flet ((starts (text name) (tuatara.defaults::definitions-in-text text (intern (string-upcase name) :keyword))))
    (is equal '(0) (starts "(defun foo (x) x)" "foo"))
    (is equal '(0 20) (starts "(defvar foo 1)      (defmethod foo ((x t)) x)" "foo"))
    (is equal '(0) (starts "(define-command foo () 1)" "foo"))
    (is equal '(0) (starts "(DEFUN (setf foo) (v) v)" "foo"))
    (is equal '(0) (starts "(defun tuatara-user:foo () 1)" "foo"))
    ;; calls, and longer names, are no definitions
    (is equal '() (starts "(default-foo foo)" "foo"))
    (is equal '() (starts "(defun foobar () 1)" "foo"))))

(define-test (xref lisp-definitions)
  ;; a function of the editor: in the file it was built from, point at its definition
  (let ((found (tuatara.defaults::lisp-definitions 'tuatara-user:insert)))
    (is = 1 (length found))
    (true (search "user-lisp.lisp" (tuatara.defaults::definition-group (first found))))
    (multiple-value-bind (b position) (funcall (tuatara.defaults::definition-locate (first found)))
      (true (typep b 'tuatara.buffer:buffer))
      (true (eql 0 (search "(defun tuatara-user:insert" (tuatara.buffer:buffer-string b :start position
                                                                                        :end (min (tuatara.buffer:buffer-size b) (+ position 40))))))))
  ;; a generic function: its defgeneric and each method, a method with what it is for
  (let* ((found (tuatara.defaults::lisp-definitions 'tuatara.defaults::mode-completion-at-point))
         (summaries (mapcar #'tuatara.defaults::definition-summary found)))
    (true (member "(defgeneric mode-completion-at-point)" summaries :test #'string=))
    (true (member "(defmethod mode-completion-at-point (lisp-mode t t))" summaries :test #'string=))
    (false (member "(defun mode-completion-at-point)" summaries :test #'string=)))
  ;; a command says so
  (true (member "(define-command goto-line)" (mapcar #'tuatara.defaults::definition-summary
                                                     (tuatara.defaults::lisp-definitions 'tuatara.defaults::goto-line))
                :test #'string=))
  ;; SBCL's own functions come from sources not installed: said, not visited
  (let ((found (tuatara.defaults::lisp-definitions 'mapcar)))
    (when found
      (multiple-value-bind (b why) (funcall (tuatara.defaults::definition-locate (first found)))
        (false b)
        (true (search "The source of mapcar is not here" why))))))

(define-test (xref going-and-coming-back)
  ;; M-. on a name goes to its definition, M-, comes back, C-M-, goes there again
  (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-g M-." (key-string "(insert")) :width 120)
    (declare (ignore f))
    (is string= "user-lisp.lisp" (current-name editor))
    (is eql (session-point-chars editor) (search "(defun tuatara-user:insert" (session-text editor) :start2 (session-point-chars editor))))
  (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-g M-. M-," (key-string "(insert")) :width 120)
    (declare (ignore f))
    (is string= "*lisp-scratch*" (current-name editor)))
  (multiple-value-bind (f editor) (fresh-start (format nil "o ~a C-g M-. M-, C-M-," (key-string "(insert")) :width 120)
    (declare (ignore f))
    (is string= "user-lisp.lisp" (current-name editor)))
  ;; the ends of the history, and a name defined nowhere
  (true (showing (fresh-start "M-," :width 120) "At start of xref history"))
  (true (showing (fresh-start "C-M-," :width 120) "At end of xref history"))
  (true (showing (fresh-start (format nil "o ~a C-g M-." (key-string "(no-such-name-anywhere")) :width 120)
                 "No definitions found for: no-such-name-anywhere")))

(define-test (xref evaluated-in-a-buffer)
  ;; a definition evaluated in *lisp-scratch* is found there
  (unwind-protect
       (multiple-value-bind (f editor)
           (fresh-start (format nil "o ~a C-M-x o ~a C-g M-." (key-string "(defun xref-test-scratch-fn () 1)")
                                (key-string "(xref-test-scratch-fn"))
                        :width 120)
         (declare (ignore f))
         (is string= "*lisp-scratch*" (current-name editor))
         (is eql (session-point-chars editor) (search "(defun xref-test-scratch-fn" (session-text editor) :start2 (session-point-chars editor))))
    (let ((symbol (find-symbol "XREF-TEST-SCRATCH-FN" :tuatara-user)))
      (when symbol
        (fmakunbound symbol)
        (remhash symbol tuatara.defaults::*evaluated-definitions*)
        (unintern symbol :tuatara-user)))))

(define-test (xref the-list)
  ;; several definitions: listed in *xref*, file by file; RET goes to the one on the line
  (let ((keys (format nil "o ~a C-g M-." (key-string "(tuatara.defaults::mode-completion-at-point"))))
    (multiple-value-bind (f editor) (fresh-start keys :width 120 :height 20)
      (is string= "*xref*" (current-name editor))
      (true (showing f "(defgeneric mode-completion-at-point)"))
      (true (showing f "(defmethod mode-completion-at-point (lisp-mode t t))"))
      (true (search "extended-command.lisp" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*xref*")))))
    (multiple-value-bind (f editor) (fresh-start (format nil "~a RET" keys) :width 120 :height 20)
      (declare (ignore f))
      (true (member (current-name editor) '("extended-command.lisp" "lisp-complete.lisp") :test #'string=)))
    ;; n shows the next beside the list, which stays selected
    (multiple-value-bind (f editor) (fresh-start (format nil "~a n" keys) :width 120 :height 20)
      (declare (ignore f))
      (is string= "*xref*" (current-name editor)))
    ;; q closes it
    (multiple-value-bind (f editor) (fresh-start (format nil "~a q" keys) :width 120 :height 20)
      (declare (ignore f))
      (is string= "*lisp-scratch*" (current-name editor)))))

(define-test (xref killed-buffers-forgotten)
  ;; a killed buffer is kept neither by the history nor by the definitions evaluated from it
  (let ((b (tuatara.buffer:make-buffer "*xref-killed-test*" :text "(defun xref-killed-test-fn () 1)"))
        (tuatara.defaults::*xref-back* '())
        (tuatara.defaults::*xref-forward* '()))
    (setf (gethash 'xref-killed-test-name tuatara.defaults::*evaluated-definitions*) (list b))
    (push (cons b (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) 0)) tuatara.defaults::*xref-back*)
    (push (cons b (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) 0)) tuatara.defaults::*xref-forward*)
    (tuatara.defaults::forget-killed-buffer-definitions b)
    (tuatara.buffer:kill-buffer b)
    (false (nth-value 1 (gethash 'xref-killed-test-name tuatara.defaults::*evaluated-definitions*)))
    (false tuatara.defaults::*xref-back*)
    (false tuatara.defaults::*xref-forward*))
  ;; and the editor does so whenever a buffer is killed
  (fresh-start "")
  (true (member 'tuatara.defaults::forget-killed-buffer-definitions tuatara.buffer:*kill-buffer-functions*)))

(define-test (xref where-there-are-no-definitions)
  ;; a mode that finds none says so at once, asking nothing
  (multiple-value-bind (f editor) (fresh-start "M-x t e x t - m o d e RET M-." :width 120)
    (true (showing f "No way to find definitions in Text buffers"))
    (false (tuatara.command:editor-minibuffer-active editor))))

(define-test (xref chosen-by-name)
  ;; a name asked for keeps its package
  (multiple-value-bind (f editor) (fresh-start (format nil "C-u M-. C-a C-k ~a RET" (key-string "tuatara.display:layout")) :width 160)
    (declare (ignore f))
    (is string= "layout.lisp" (current-name editor))))

(define-test (xref buffer-definitions-followed)
  ;; a definition evaluated from a buffer is found where it is after edits, and a
  ;; killed buffer is said to be gone, not shown again
  (let ((b (tuatara.buffer:make-buffer "*xref-follow-test*" :text (format nil "~%(defun xref-follow-test-fn () 1)~%")))
        (symbol (intern "XREF-FOLLOW-TEST-FN" :tuatara-user)))
    (unwind-protect
         (progn
           (setf (gethash symbol tuatara.defaults::*evaluated-definitions*) (list b))
           (let ((d (find "*xref-follow-test*" (tuatara.defaults::lisp-definitions symbol)
                          :key #'tuatara.defaults::definition-group :test #'string=)))
             (true d)
             (tuatara.buffer:buffer-insert b 0 ";; pushed down")
             (multiple-value-bind (found position) (funcall (tuatara.defaults::definition-locate d))
               (is eq b found)
               (is eql 0 (search "(defun xref-follow-test-fn" (tuatara.buffer:buffer-string b :start position))))
             (tuatara.buffer:kill-buffer b)
             (multiple-value-bind (found why) (funcall (tuatara.defaults::definition-locate d))
               (false found)
               (true (search "has been killed" why)))))
      (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b))
      (remhash symbol tuatara.defaults::*evaluated-definitions*)
      (unintern symbol :tuatara-user))))

(define-test (xref definition-heads-one-list)
  ;; the forms that define a name, told by their heads and found in text alike
  (dolist (head '("DEFUN" "DEFSTRUCT" "DEFSETF" "DEFINE-COMMAND"))
    (true (tuatara.defaults::definition-head-p head) "~a" head))
  (dolist (head '("LET" "DEFINE-" "DEFAULT"))
    (false (tuatara.defaults::definition-head-p head) "~a" head))
  (let ((text (format nil "(defstruct foo)~%(let ((foo 1)))~%(define-command foo ()~%(DefSetf foo bar)~%(defun foobar ())")))
    (is equal (list 0 (search "(define-command" text) (search "(DefSetf" text))
        (tuatara.defaults::definitions-in-text text :foo))))

(define-test (xref places-kept-as-every-history)
  ;; going back and forward keeps the newest *HISTORY-KEPT* places, both ways
  (let ((tuatara.defaults::*history-kept* 2)
        (tuatara.defaults::*xref-back* '())
        (tuatara.defaults::*xref-forward* '()))
    (fresh-start (format nil "o ~a C-g M-. M-, M-. M-, M-. M-, C-M-, M-, C-M-," (key-string "(insert")) :width 120)
    (true (<= (length tuatara.defaults::*xref-back*) 2))
    (true (<= (length tuatara.defaults::*xref-forward*) 2))))

