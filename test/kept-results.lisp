;;;; test/kept-results.lisp --- Lists a function keeps and gives again are never changed by those it gives them to.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A function that keeps its answer and gives the same list again lends it:
;;;; a caller joining it into another list with NCONC, or sorting or reversing
;;;; it in place, changes what the next caller is given, and a list joined to
;;;; itself that way makes the next LAST or LENGTH spin for ever. While the
;;;; tests run, the lists such caches give out are fingerprinted as they are
;;;; kept and checked each time they are given again; every mode's colouring
;;;; is asked for twice over sample files, under a deadline; and the source is
;;;; searched for the destructive operators applied to what they give.

(in-package #:tuatara.tests)

(define-test kept-results
  :description "Lists kept and given again stay as they were kept: every mode's colouring asked for twice over
samples gives the same proper lists in time, a kept list changed in place or made a loop fails the test that did
it, and no code joins, sorts, reverses or deletes from such a list in place.")

;;; Fingerprints of the lists the caches give out

(defun kept-entries-of (accessor)
  "A function of a buffer, and any arguments after it, that gives the entries
of the cache ACCESSOR, a function of the buffer, holds: each (KEY . LIST)."
  (lambda (buffer &rest arguments)
    (declare (ignore arguments))
    (funcall accessor buffer)))

(defparameter *watched-caches*
  (list (list 'tuatara.syntax::kept-colours "the colours kept for a syntax tree (KEPT-COLOURS)"
              (kept-entries-of #'tuatara.syntax::syntax-cache))
        (list 'tuatara.syntax::slice-highlights "the colours kept for a slice of text (SLICE-HIGHLIGHTS)"
              (kept-entries-of #'tuatara.syntax::syntax-cache))
        (list 'tuatara.defaults::org-src-cached-highlights
              "the colours kept for an Org note's source blocks (ORG-SRC-CACHED-HIGHLIGHTS)"
              (kept-entries-of (lambda (buffer)
                                 (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-src-colours))))
        (list 'tuatara.defaults::org-buffer-laid-out-ranges
              "the laid-out ranges kept for an Org parse (ORG-BUFFER-LAID-OUT-RANGES)"
              (kept-entries-of (lambda (buffer)
                                 (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-laid-out-ranges))))
        (list 'tuatara.display:buffer-folds "the folds kept merged for a buffer (BUFFER-FOLDS)"
              (kept-entries-of (lambda (buffer)
                                 (let ((kept (tuatara.buffer:buffer-local buffer 'tuatara.display::folds-merged)))
                                   (and kept (list (cons (list (first kept) (second kept)) (cddr kept))))))))
        (list 'tuatara.lsp::org-block-doc-specs "the source blocks kept for language servers (ORG-BLOCK-DOC-SPECS)"
              (kept-entries-of (lambda (buffer)
                                 (let ((kept (tuatara.buffer:buffer-local buffer 'tuatara.lsp::org-block-specs)))
                                   (and kept (list kept)))))))
  "The caches whose lists are checked while the tests run, each (FUNCTION
WORDS ENTRIES): the function giving out a kept list, words naming the cache,
and a function of the same arguments giving the cache's entries, each (KEY
. LIST).")

(defvar *kept-fingerprints* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "For each list a watched cache gave out, (KEY . FINGERPRINT): the key it was
kept under and its KEPT-FINGERPRINT then.")

(defun kept-fingerprint (tree)
  "A fixnum made of everything in TREE, a proper list of conses, numbers and
symbols, in order, so that a change to any of it changes it but by rare
chance; nothing is made in working it out, so that the tests measuring the
garbage made at each key are not put out."
  (let ((hash 17))
    (declare (type fixnum hash))
    (labels ((mix (value)
               (setf hash (logand most-positive-fixnum (+ (* hash 31) value))))
             (walk (item)
               (if (consp item)
                   (progn (mix 1) (walk (car item)) (walk (cdr item)))
                   (mix (sxhash item)))))
      (walk tree))
    hash))

(defun check-kept-result (words result entries)
  "Check RESULT, a list a cache named by WORDS gave out, against ENTRIES, the
cache's entries now: a list that is not proper, or one given again under the
key it was kept with but changed since, is noted as a fault of the test
running (NOTE-TEST-FAULT), and an error, so that the caller does not spin on
a loop it would make of it; a list not met before is fingerprinted."
  (when (consp result)
    (unless (list-length result)
      (error "~a" (note-test-fault "~a gave out a kept list joined into a loop" words)))
    (let ((entry (find result entries :key #'cdr :test #'eq)))
      (when entry
        (let ((seen (gethash result *kept-fingerprints*)))
          (if (and seen (equal (car seen) (car entry)))
              (unless (= (cdr seen) (kept-fingerprint result))
                (error "~a" (note-test-fault "~a gave again a list changed in place since it was kept" words)))
              (setf (gethash result *kept-fingerprints*) (cons (car entry) (kept-fingerprint result)))))))))

(defun watch-kept-results ()
  "Have each cache of *WATCHED-CACHES* check the lists it gives out
(CHECK-KEPT-RESULT), for the tests alone: the functions are wrapped, their
code is not changed."
  (loop for (name words entries) in *watched-caches*
        do (let ((words words) (entries entries))
             (sb-int:unencapsulate name 'kept-results)
             (sb-int:encapsulate name 'kept-results
                                 (lambda (function &rest arguments)
                                   (let ((result (apply function arguments)))
                                     (check-kept-result words result (apply entries arguments))
                                     result))))))

(watch-kept-results)

(define-test (kept-results changed-in-place-fails-the-test)
  ;; a kept list given again after a caller joined another to it fails the test running, naming the cache;
  ;; one joined to itself is an error at once, not a loop
  (let ((kept (list (list 0 1 :org-block))))
    (flet ((give ()
             (check-kept-result "the colours kept" kept (list (cons 'key kept)))
             kept))
      (take-test-faults)
      (give)
      (give)
      (is equal '() (take-test-faults))
      (nconc (give) (list (list 1 2 :org-block)))
      (fail (give))
      (let ((faults (take-test-faults)))
        (is = 1 (length faults))
        (true (search "the colours kept gave again a list changed in place" (first faults))))
      (nconc kept kept)
      (fail (give))
      (true (search "joined into a loop" (first (take-test-faults)))))))

(define-test (kept-results replaced-functions-come-back-whole)
  ;; a function a test replaces for a while is itself again afterwards, its check of kept lists kept,
  ;; never the stand-in
  (let ((name 'tuatara.defaults::org-src-cached-highlights))
    (with-functions-replaced ((tuatara.defaults::org-src-cached-highlights (lambda (&rest arguments)
                                                                             (declare (ignore arguments))
                                                                             :stand-in)))
      (is eq :stand-in (funcall name nil nil 0 0)))
    (true (sb-int:encapsulated-p name 'kept-results))
    (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*kept-results*") :text "abc")))
      (unwind-protect
           (isnt eq :stand-in (let ((tuatara.defaults::*org-src-blocks* '()))
                                (funcall name buffer (tuatara.buffer:buffer-text buffer) 0 3)))
        (tuatara.buffer:kill-buffer buffer)))))

;;; Every mode's colouring, twice, over samples

(defparameter *colouring-deadline* 5
  "The seconds one call for a window's colours may take before the test says it hung.")

(defun colouring-org-note (seed headings)
  "A made-up Org note of HEADINGS headlines, drawn from SEED: each with
drawers, lists and source blocks in several languages, some nested under
others."
  (let ((*random-state* (sb-ext:seed-random-state seed))
        (blocks '(("sh" "echo \"$HOME\" | wc -c") ("lisp" "(defun f (x) (* x 2))")
                  ("rust" "fn main() { let x: u8 = 1; }") ("c" "int main(void) { return 0; }")
                  ("python" "def f(x):~%    return [x for x in range(3)]") ("emacs-lisp" "(setq a '(1 2))"))))
    (with-output-to-string (out)
      (format out "#+title: Made-up note ~d~%~%" seed)
      (dotimes (i headings)
        (format out "~a ~:[~;TODO ~]Heading ~d~%"
                (make-string (1+ (random 3)) :initial-element #\*) (zerop (random 3)) i)
        (when (zerop (random 2))
          (format out ":PROPERTIES:~%:ID: id-~d~%:END:~%" i))
        (format out "Some *bold* text and =code= with a [[https://example.invalid/~d][link]].~%" i)
        (when (zerop (random 3))
          (format out ":LOGBOOK:~%CLOCK: [2026-11-17 Tue 10:00]--[2026-11-17 Tue 11:00] =>  1:00~%:END:~%"))
        (when (zerop (random 3))
          (format out "- [ ] one~%- [X] two :: done~%"))
        (loop repeat (1+ (random 3))
              do (destructuring-bind (language code) (nth (random (length blocks)) blocks)
                   (format out "#+begin_src ~a~%~?~%#+end_src~%" language code '())))))))

(defun colouring-samples ()
  "The samples every mode's colouring is tried on, each (NAME KEYS TEXT FILE):
the language samples in test/syntax/, the Org notes in test/org/, made-up
notes with many source blocks, folds and drawers, and a CSV table; FILE is
visited, else TEXT put in the mode KEYS choose."
  (let ((org "M-x o r g - m o d e RET"))
    (append (loop for file in (remove-duplicates
                               (loop for files in '("test/syntax/*.*" "test/syntax/Containerfile"
                                                    "test/syntax/indentation/*.*" "test/org/*.org")
                                     append (directory (asdf:system-relative-pathname "tuatara" files)))
                               :test #'equal :key #'namestring)
                  unless (equal (pathname-name file) "LICENSE")
                    collect (list (tree-relative-name file) "" "" file))
            (loop for (seed headings) in '((1 12) (2 30) (3 60))
                  collect (list (format nil "made-up note ~d" seed) org (colouring-org-note seed headings) nil))
            (list (list "a CSV table" "M-x c s v - m o d e RET"
                        (with-output-to-string (out)
                          (dotimes (i 200) (format out "~d,\"name ~d\",~,2f~%" i i (/ i 7))))
                        nil)))))

(defun proper-colours-p (colours)
  "True when COLOURS, as a mode gives a window's, is a proper list of proper lists."
  (and (listp colours) (list-length colours)
       (every (lambda (layer) (and (listp layer) (list-length layer))) colours)))

(defun colours-in-time (name thunk)
  "What THUNK, asking for colours, gives, checked to be proper lists
\(PROPER-COLOURS-P) given within *COLOURING-DEADLINE*; :FAILED, a check failed
naming NAME, when not."
  (let ((colours (handler-case (sb-ext:with-timeout *colouring-deadline* (funcall thunk))
                   (sb-ext:timeout () :timeout)
                   (error (condition) (list :error condition)))))
    (cond ((eq colours :timeout)
           (true nil "~a: no answer within ~d s" name *colouring-deadline*)
           :failed)
          ((not (proper-colours-p colours))
           (true nil "~a: ~a" name (if (eq (first colours) :error) (second colours) "not proper lists"))
           :failed)
          (t colours))))

(defun colours-settled (name thunk)
  "The colours THUNK gives once two calls in a row agree and colour
something, the parses and rules made in the background done, or agree after
a few seconds: each call checked as COLOURS-IN-TIME does. :FAILED when one
fails or they never agree."
  (let ((before (colours-in-time name thunk)))
    (loop repeat 100
          for tries from 1
          for now = (if (eq before :failed) :failed (colours-in-time name thunk))
          do (cond ((eq now :failed) (return :failed))
                   ((and (equal now before) (or (some #'identity now) (> tries 40))) (return now))
                   (t (setf before now) (sleep 0.05)))
          finally (true nil "~a: the colours never settled" name) (return :failed))))

(defun parsed-once-grammar-loaded (buffer)
  "Have BUFFER parsed once its grammar is loaded, when the session it was
visited in ended before the program loading grammars had it: waited for at
most five seconds, as the editor's tick would."
  (loop repeat 50
        for language = (tuatara.buffer:buffer-local buffer :syntax-pending)
        while (and language (null (tuatara.syntax:buffer-syntax buffer)))
        do (sleep 0.1)
           (tuatara.defaults::start-parsing buffer language)))

(defun code-parser-awaited (buffer)
  "Wait, for at most 20 seconds, until the program parsing BUFFER's code runs
\(TUATARA.SYNTAX:BUFFER-HELPER-SOON starts it in the background), so that
the colours of an Org note's code are kept, as they are only once it runs."
  (loop repeat 400
        until (tuatara.syntax:buffer-helper-soon buffer)
        do (sleep 0.05)))

(defun check-colouring-twice (name editor window ranges)
  "Show WINDOW of EDITOR at each of RANGES, (START . FOLD) with a part of FOLD
octets folded after START when FOLD is not NIL, and ask for its colours, the
major mode's (MODE-SYNTAX-HIGHLIGHTS) and those of all
*SYNTAX-HIGHLIGHT-FUNCTIONS*, until settled and then twice more: the same
proper lists each time, given in time, the places after a failure passed
over. NAME names the sample in failures. Returns how many of the answers
coloured something."
  (let* ((coloured 0)
         (failed nil)
         (buffer (tuatara.display:window-buffer window))
         (mode (tuatara.modes:buffer-major-mode buffer))
         (rope (tuatara.buffer:buffer-text buffer)))
    (loop for (start . fold) in ranges
          for line = (tuatara.text:rope-line-start rope start)
          until failed
          do (tuatara.display:unfold-all buffer)
             (when fold
               (let ((end (min (tuatara.buffer:buffer-size buffer) (+ line fold))))
                 (tuatara.display:fold-region buffer (tuatara.text:rope-line-end rope line) end)))
             (setf (tuatara.display:window-start window) line)
             (let ((rows (tuatara.display:window-layout window))
                   (where (format nil "~a at ~d~@[ folded ~d~]" name line fold)))
               (loop for (what thunk)
                       in (list (list "the mode's colours"
                                      (lambda () (tuatara.modes:mode-syntax-highlights mode buffer rows)))
                                (list "the window's colours"
                                      (lambda () (tuatara.command:window-syntax editor window rows))))
                     for label = (format nil "~a: ~a" where what)
                     for first = (colours-settled label thunk)
                     for second = (if (eq first :failed) :failed (colours-in-time label thunk))
                     for third = (if (eq second :failed) :failed (colours-in-time label thunk))
                     do (if (eq third :failed)
                            (return (setf failed t))
                            (progn (when (some #'identity second) (incf coloured))
                                   (is equal second third "~a: given twice" label))))))
    (tuatara.display:unfold-all buffer)
    coloured))

(define-test (kept-results every-colouring-twice)
  :time-limit 120
  ;; every sample at a few places, some with a part folded: its colours asked for twice agree, as proper lists,
  ;; each in time, so that a kept list joined into a loop fails here instead of hanging the editor
  (let ((*random-state* (sb-ext:seed-random-state 1117))
        (tuatara.syntax:*syntax-wait* 10))
    (loop for (name keys text file) in (colouring-samples)
          do (multiple-value-bind (f editor) (run-keys keys :text text :file file :width 80 :height 24)
               (declare (ignore f))
               (let* ((window (tuatara.command:editor-window editor))
                      (buffer (tuatara.display:window-buffer window))
                      (size (tuatara.buffer:buffer-size buffer)))
                 (true (plusp size) "~a opened" name)
                 (parsed-once-grammar-loaded buffer)
                 (when (search "#+begin_src" text)
                   (code-parser-awaited buffer))
                 (unwind-protect
                      (progn
                        (true (plusp (check-colouring-twice
                                      name editor window
                                      (list* (cons 0 nil)
                                             (loop repeat 4
                                                   collect (cons (random (max 1 size))
                                                                 (and (zerop (random 2)) (+ 40 (random 400))))))))
                              "~a coloured, in ~(~a~)" name (type-of (tuatara.modes:buffer-major-mode buffer)))
                        ;; the colours of the made-up notes' code were kept, and given again
                        (unless file
                          (when (search "#+begin_src" text)
                            (true (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-src-colours)
                                  "~a: its code's colours kept" name))))
                   (tuatara.buffer:kill-buffer buffer)))))))

;;; Destructive operators applied to what caches give

(defparameter *lent-list-functions*
  '("mode-syntax-highlights" "syntax-highlights" "window-syntax" "part-highlights" "language-highlights"
    "kept-entry")
  "Functions documented to lend the lists they give (read, never changed or
kept), beside those of *WATCHED-CACHES* and those named \"cached\".")

(defparameter *destructive-operators*
  '("nconc" "nreverse" "nreconc" "sort" "stable-sort" "delete" "delete-if" "delete-if-not" "delete-duplicates"
    "rplaca" "rplacd" "fill" "replace" "map-into")
  "The operators that change a list given them, the last argument of NCONC
included: a kept list there is joined into one its caller may join more to.")

(defun kept-list-function-names ()
  "The names, downcased and without package, of the functions whose lists are
kept and given again: those of *WATCHED-CACHES*, *LENT-LIST-FUNCTIONS*, and
every function of the code named with \"cached\"."
  (remove-duplicates
   (append (loop for (name) in *watched-caches* collect (string-downcase (symbol-name name)))
           *lent-list-functions*
           (loop for file in (core-source-files)
                 append (let ((text (coerce (uiop:read-file-string file :external-format :utf-8) 'simple-string)))
                          (loop for (start) in (top-level-forms text)
                                for name = (defined-function-name text start)
                                when (and name (search "cached" name)) collect name))))
   :test #'string=))

(defun kept-list-changes (tree names)
  "Where TREE, a form as READ-TREE gives it, changes in place a list one of
the functions NAMES gave: a destructive operator given a call to one or a
variable bound to one by LET or LET*, MAPCAN given one as its function or a
lambda ending in a call to one, LOOP's NCONC clause of one. A list of
\(OPERATOR FUNCTION)."
  (let ((found '()))
    (labels ((kept-name (form vars)
               (cond ((stringp form) (cdr (assoc form vars :test #'string=)))
                     ((and (consp form) (stringp (first form)))
                      (find (token-name (first form)) names :test #'equal))))
             (note (operator form vars)
               (let ((name (kept-name form vars)))
                 (when name (push (list operator name) found))))
             (walk (form vars)
               (when (consp form)
                 (let ((head (and (stringp (first form)) (token-name (first form)))))
                   (when (and (member head '("let" "let*") :test #'equal) (listp (second form)))
                     (dolist (binding (second form))
                       (when (and (consp binding) (stringp (first binding)))
                         (let ((name (kept-name (second binding) vars)))
                           (when name (push (cons (first binding) name) vars))))))
                   (cond ((member head *destructive-operators* :test #'equal)
                          (dolist (argument (rest form)) (note head argument vars)))
                         ((equal head "mapcan")
                          (let ((function (second form)))
                            (when (consp function)
                              (cond ((equal (first function) "function") (note head (list (second function)) vars))
                                    ((equal (first function) "lambda") (note head (car (last function)) vars))))))
                         ((equal head "loop")
                          (loop for (word form) on (rest form)
                                when (and (stringp word) (member (token-name word) '("nconc" "nconcing") :test #'equal))
                                  do (note "loop nconc" form vars))))
                   (dolist (part form) (walk part vars))))))
      (walk tree '()))
    (nreverse found)))

(defun kept-list-changes-in (files)
  "Each place in FILES where a list a cache gave is changed in place
\(KEPT-LIST-CHANGES), as (FILE LINE DEFINITION OPERATOR FUNCTION)."
  (let ((names (kept-list-function-names)))
    (loop for file in files
          append (let ((text (coerce (uiop:read-file-string file :external-format :utf-8) 'simple-string)))
                   (loop for (start end) in (top-level-forms text)
                         append (loop for (operator function) in (loop for tree in (read-tree text start end)
                                                                       append (kept-list-changes tree names))
                                      collect (list (tree-relative-name file) (file-line-number text start)
                                                    (nth-value 1 (form-head text start)) operator function)))))))

(define-test (kept-results changes-in-place-found)
  ;; the joins that made a loop of an Org note's kept colours are found; a copy of the kept list is not one
  (let ((names '("org-src-cached-highlights" "org-view-colours")))
    (flet ((changes (text)
             (let ((text (coerce text 'simple-string)))
               (kept-list-changes (first (read-tree text 0 (length text))) names))))
      (is equal '(("nconc" "org-src-cached-highlights"))
          (changes "(if blocks (nconc ranges (org-src-cached-highlights b r f l)) ranges)"))
      (is equal '()
          (changes "(if blocks (nconc ranges (copy-list (org-src-cached-highlights b r f l))) ranges)"))
      (is equal '(("loop nconc" "org-view-colours"))
          (changes "(loop for view in views nconc (org-view-colours b view f l s))"))
      (is equal '(("sort" "org-src-cached-highlights"))
          (changes "(let ((kept (tuatara.defaults::org-src-cached-highlights b r f l))) (sort kept #'< :key #'first))"))
      (is equal '(("mapcan" "org-src-cached-highlights"))
          (changes "(mapcan (lambda (b) (org-src-cached-highlights b r f l)) buffers)")))))

(define-test (kept-results no-kept-list-changed-in-place)
  ;; no code of the editor or its packages changes in place a list a cache keeps and gives again
  (let ((found (kept-list-changes-in (append (core-source-files) (package-source-files)))))
    (false found
           "~d kept lists changed in place (file line definition operator function):~{~%  ~{~a:~a ~a: ~a of ~a~}~}"
           (length found) found)))

(defun kept-results-report (&optional (stream *standard-output*))
  "Print to STREAM each place the code of the editor or its packages changes
in place a list a cache keeps and gives again (KEPT-LIST-CHANGES-IN), after
a line that counts them."
  (let ((found (kept-list-changes-in (append (core-source-files) (package-source-files)))))
    (format stream "~d kept lists changed in place by those given them~%" (length found))
    (loop for (file line definition operator function) in found
          do (format stream "  ~a:~a  ~a: ~a of ~a~%" file line definition operator function))))

