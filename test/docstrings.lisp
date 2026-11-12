;;;; test/docstrings.lisp --- Every public symbol is documented.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun tuatara-packages ()
  "Return every package whose name starts with TUATARA, excluding the test packages."
  (remove-if-not (lambda (p)
                   (let ((n (package-name p)))
                     (and (>= (length n) 7) (string= "TUATARA" n :end2 7)
                          (not (member n '("TUATARA.TESTS" "TUATARA.BENCH") :test #'string=)))))
                 (list-all-packages)))

(defun docstring-with-sentence-p (doc)
  "True when DOC is a string whose first sentence says something: it holds a
letter or digit before its first full stop."
  (and (stringp doc)
       (find-if #'alphanumericp doc :end (or (position #\. doc) (length doc)))
       t))

(defun undocumented-symbols (package)
  "Return a list of strings naming external symbols of PACKAGE that lack a
docstring in any role they fill: function or macro, special variable (or a
name standing for one), class, type. The docstring needs a first sentence. A symbol that fills
no role at all (exported but never defined) is reported too, since exporting it was a promise."
  (let ((missing '()))
    (do-external-symbols (s package)
      (let ((roles 0))
        (when (fboundp s)
          (incf roles)
          (unless (docstring-with-sentence-p (documentation s 'function))
            (push (format nil "~a (function)" s) missing)))
        (when (member (sb-int:info :variable :kind s) '(:special :constant :macro))
          (incf roles)
          (unless (docstring-with-sentence-p (documentation s 'variable))
            (push (format nil "~a (variable)" s) missing)))
        (when (find-class s nil)
          (incf roles)
          (unless (docstring-with-sentence-p (documentation (find-class s) t))
            (push (format nil "~a (class)" s) missing)))
        (when (and (not (find-class s nil)) (sb-int:info :type :kind s))
          (incf roles)
          (unless (docstring-with-sentence-p (documentation s 'type)) (push (format nil "~a (type)" s) missing)))
        (when (zerop roles) (push (format nil "~a (exported but undefined)" s) missing))))
    (sort missing #'string<)))

(define-test documentation
  :description "Self-documentation gate: every public symbol of every TUATARA package has a docstring.")

(define-test (documentation docstring-first-sentences-are-checked)
  (true (docstring-with-sentence-p "Return the size. More."))
  (true (docstring-with-sentence-p "Return the size"))
  (false (docstring-with-sentence-p nil))
  (false (docstring-with-sentence-p ""))
  (false (docstring-with-sentence-p "  "))
  (false (docstring-with-sentence-p ". Text after a stop.")))

(define-test (documentation packages-have-docstrings)
  (let ((undocumented (remove-if (lambda (p) (documentation p t)) (tuatara-packages))))
    (true (null undocumented) "Packages without a docstring: ~{~a~^, ~}"
          (mapcar #'package-name undocumented))))

(define-test (documentation public-symbols-have-docstrings)
  (dolist (p (tuatara-packages))
    (let ((missing (undocumented-symbols p)))
      (true (null missing) "~a: undocumented public symbols: ~{~a~^, ~}" (package-name p) missing))))

(defparameter *borrowed-phrases*
  '("as emacs" "emacs's" "your emacs" "as helix" "helix's" "as kitty does" "as vim" "vim's")
  "Phrases that explain a thing by how another program behaves, which no
docstring may hold: a docstring says what a thing does on its own.")

(defun phrase-docstrings (phrases)
  "Return a list of strings naming the definitions of the TUATARA packages
whose docstring holds one of PHRASES, any case, each with the phrase found."
  (let ((found '()))
    (dolist (p (tuatara-packages))
      (let ((doc (documentation p t)))
        (dolist (phrase phrases)
          (when (and doc (search phrase doc :test #'char-equal))
            (push (format nil "package ~a (~a)" (package-name p) phrase) found))))
      (do-symbols (s p)
        (when (eq (symbol-package s) p)
          (dolist (doc (list (and (fboundp s) (documentation s 'function))
                             (documentation s 'variable)
                             (let ((class (find-class s nil))) (and class (documentation class t)))))
            (dolist (phrase phrases)
              (when (and doc (search phrase doc :test #'char-equal))
                (push (format nil "~s (~a)" s phrase) found)))))))
    (sort (remove-duplicates found :test #'string=) #'string<)))

(define-test (documentation docstrings-stand-on-their-own)
  (let ((found (phrase-docstrings *borrowed-phrases*)))
    (true (null found) "Docstrings that explain by another program: ~{~a~^, ~}" found)))

(define-test (documentation docstrings-speak-to-you)
  ;; a docstring speaks to the person reading it, or names the account a thing belongs to
  (let ((found (phrase-docstrings '("the user"))))
    (true (null found) "Docstrings that say the user: ~{~a~^, ~}" found))
  (false (search "the user" (tuatara.defaults::peer-refusal (1+ (sb-posix:geteuid)) nil) :test #'char-equal)))

(define-test (documentation csv-column-faces-count-from-zero)
  (dotimes (n 8)
    (let* ((face (tuatara.display:find-face (intern (format nil "CSV-COLUMN-~d" n) :keyword)))
           (doc (tuatara.display:face-doc face)))
      (true (search (format nil "columns ~d, ~d, ~d" n (+ n 8) (+ n 16)) doc)
            "The face for column ~d says ~s" n doc))))

(defun own-settings ()
  "Every setting of Tuatara's own: the variables TUATARA-USER exports, a
package's settings left out."
  (let ((settings '()))
    (do-external-symbols (symbol :tuatara-user)
      (when (and (boundp symbol) (eq :special (sb-int:info :variable :kind symbol))
                 (not (get symbol 'tuatara.packages::package-setting)))
        (push symbol settings)))
    (sort settings #'string< :key #'symbol-name)))

(define-test (documentation settings-declare-values)
  ;; every setting says the values it takes where it is defined, and its value now is one of them
  (dolist (symbol (own-settings))
    (let ((name (string-downcase symbol)))
      (true (tuatara.runtime::setting-values symbol) "~a declares no values (define-setting :values)" name)
      (false (tuatara.runtime::setting-value-fault symbol)))))

(define-test (documentation values-in-words)
  ;; a description of the values a setting takes, in words, and whether a value is one of them
  (flet ((words (description) (tuatara.runtime::values-words description))
         (fits (value description) (tuatara.runtime::value-fits value description)))
    (is string= "t, nil" (words 'boolean))
    (is string= ":absolute, :relative, nil" (words '(member :absolute :relative nil)))
    (is string= "\"gh\", \"org\"" (words '(member "gh" "org")))
    (is string= "a whole number, 0 or more" (words '(integer 0 *)))
    (is string= "a whole number from 1 to 7" (words '(integer 1 7)))
    (is string= "a number, 2.5 or less" (words '(number * 2.5)))
    (is string= "a whole number" (words 'integer))
    (is string= "text" (words 'string))
    (is string= "nil, or text" (words '(or (member nil) string)))
    (is string= "a list, each a directory's name" (words '(list-of directory)))
    (is string= "a list of (NAME . PATTERN)" (words "a list of (NAME . PATTERN)"))
    (is eq :yes (fits nil 'boolean))
    (is eq :no (fits :yes 'boolean))
    (is eq :yes (fits 0 '(integer 0 *)))
    (is eq :no (fits -1 '(integer 0 *)))
    (is eq :no (fits 1.5 '(integer 0 *)))
    (is eq :yes (fits 1.5 '(number 0 *)))
    (is eq :yes (fits "GH" '(member "gh" "org")))
    (is eq :yes (fits '("a" "b") '(list-of string)))
    (is eq :no (fits '("a" b) '(list-of string)))
    (is eq :no (fits "a" '(list-of string)))
    (is eq :yes (fits nil '(or (member nil) string)))
    (is eq :unknown (fits 'anything "words"))
    (is eq :unknown (fits '((a . b)) '(list-of "(NAME . PATTERN)")))
    (is eq :no (fits 3 '(list-of "(NAME . PATTERN)")))
    ;; a description no one understands is refused where the setting is defined
    (fail (macroexpand-1 '(tuatara.runtime::define-setting *no-such-setting* 1 "A setting." :values (intger 0 *))))
    (fail (macroexpand-1 '(tuatara.runtime::define-setting *no-such-setting* 1 "A setting." :values (integer 0))))
    (fail (macroexpand-1 '(tuatara.runtime::define-setting *no-such-setting* 1 nil :values boolean)))))
