;;;; test/prose.lisp --- Where prose is written: Org's parse, text with no syntax tree, lines half typed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test prose
  :description "Prose told from what is not: in an Org note its paragraphs and headlines, not its source blocks, tables, links or code; in plain text everywhere; never in a link or code span not closed yet; asked one place at a time or many in order alike.")

(defun prose-at (mode text mark)
  "Whether the place of MARK, the first character of it in TEXT, is prose in a
buffer in MODE holding TEXT, asked by PROSE-P and by PROSE-TEST, as two values."
  (let ((b (tuatara.buffer:make-buffer "*prose*" :text text)))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b mode)
           (let ((at (tuatara.text:string-octet-length text :end (search mark text))))
             (values (tuatara.defaults::prose-p b at)
                     (funcall (tuatara.defaults::prose-test b) at))))
      (tuatara.buffer:kill-buffer b))))

(define-test (prose org-notes)
  (let ((note (format nil "* Héading one~%Prose here.~%#+begin_src sh~%echo code~%#+end_src~%| table cell |~%See [[https://example.org][a link]] and ~~code~~ then more.~%")))
    (dolist (place '(("Héading" t) ("Prose" t) ("echo" nil) ("table" nil) ("https" nil) ("code~" nil) ("then" t)))
      (multiple-value-bind (one many) (prose-at 'tuatara.defaults::org-mode note (first place))
        (is eq (second place) (and one t) "~a" (first place))
        (is eq (second place) (and many t) "~a, in order" (first place))))))

(define-test (prose plain-text)
  ;; text with no syntax tree is prose throughout
  (true (prose-at 'tuatara.defaults::text-mode (format nil "Some words.~%More words.") "More"))
  (true (prose-at 'tuatara.defaults::fundamental-mode "Run `ls a." "a.")))

(define-test (prose half-typed)
  (true (tuatara.defaults::half-typed-p "Run ~ls -l."))
  (true (tuatara.defaults::half-typed-p "=a.b."))
  (true (tuatara.defaults::half-typed-p "See [[https://a.b/c."))
  (false (tuatara.defaults::half-typed-p "So a = b."))
  (false (tuatara.defaults::half-typed-p "Run ~ls~ now.")))

(define-test (prose special-blocks-are-prose)
  ;; a block of a name of your own (#+begin_note) holds prose to fill and to check the spelling of,
  ;; an example block neither
  (let* ((note (format nil "#+begin_note~%Words in a note.~%#+end_note~%#+begin_example~%kept as it is~%#+end_example~%#+begin_quote~%Quoted words.~%#+end_quote~%"))
         (b (tuatara.buffer:make-buffer "*special-blocks*" :text note)))
    (dolist (place '(("Words" t) ("kept" nil) ("Quoted" t)))
      (is eq (second place) (and (prose-at 'tuatara.defaults::org-mode note (first place)) t) "~a" (first place)))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::org-mode)
           (dolist (place '(("Words" nil) ("kept" t) ("Quoted" nil)))
             (is eq (second place)
                 (and (tuatara.defaults::mode-verbatim-line-p (tuatara.defaults::buffer-major-mode b) b (search (first place) note)) t)
                 "~a filled" (first place))))
      (tuatara.buffer:kill-buffer b))))
