;;;; lisp/org/foreign-text.lisp --- Words from a file given to you, written into an Org note as words.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A note made from a document someone else wrote (a Word, OpenDocument,
;;;; Excel or PowerPoint file read, a LaTeX or Word template imported) holds
;;;; that document's words. Org gives meaning to some of them: a line that
;;;; begins with * is a heading, [[...]] a link, @@latex:...@@ is written into
;;;; an export as it is, {{{...}}} a macro call. Every reader and importer
;;;; writes the words it takes from such a file through the functions here,
;;;; so that what the file says stays text in the note, however it is
;;;; written, and the names of its parts never reach outside the directory
;;;; they are saved in.

(in-package #:tuatara.defaults)

(defparameter *org-zero-width-space* (string (code-char #x200B))
  "The zero-width space set into words from a file where Org would otherwise
read them as markup: it shows as nothing, and Org reads the words beside it as
words.")

(defun org-escaped-words (text)
  "TEXT, words a file given to you holds, written so that the note made from
it reads them as those words and never as markup that writes into an export as
it is. A zero-width space goes after each backslash (no LaTeX command, \\=\\(
\\=\\) or \\=\\[ \\=\\] formula, \\begin{...} environment) and between the @s of
@@ (no @@latex:...@@ snippet), the [s of [[ (no link) and the first two {s of
{{{ (no macro call), and after the [ of [fn: (no footnote reference). Each $
as \\dollar{}, which shows as $ (no $...$ formula)."
  (cl-ppcre:regex-replace-all "\\\\|\\$|@(?=@)|\\[(?=\\[|fn:)|\\{(?=\\{\\{)" text
                              (lambda (match)
                                (if (string= match "$")
                                    "\\dollar{}"
                                    (concatenate 'string match *org-zero-width-space*)))
                              :simple-calls t))

(defun org-safe-line (text &key escaped)
  "TEXT, words a document holds, on one line: its line ends (and any run of
them) a space, so that what a document says can never begin a line of the
note made from it, and its words kept from being read as markup
\(ORG-ESCAPED-WORDS). With ESCAPED true, TEXT is Org text already kept from
markup, and its line breaks become a space as its line ends do."
  (if escaped
      (cl-ppcre:regex-replace-all "(?:(?:\\\\\\\\)?[\\r\\n])+" text " ")
      (org-escaped-words (cl-ppcre:regex-replace-all "[\\r\\n]+" text " "))))

(defun org-safe-line-words (text)
  "The words TEXT, a line ORG-SAFE-LINE made, said before they were kept from
markup: the zero-width space after each backslash, between @@, [[ and {{{ and
after the [ of [fn: taken out, and each \\dollar{} a $ again."
  (cl-ppcre:regex-replace-all (format nil "\\\\dollar\\{\\}|(?<=[\\\\@\\[{])~a" *org-zero-width-space*) text
                              (lambda (match) (if (string= match "\\dollar{}") "$" ""))
                              :simple-calls t))

(defun org-guarded-lines (text)
  "TEXT with a zero-width space set before what begins each of its lines when
Org would read the line as a heading, a keyword (#+, after spaces too), a
comment (# and a space), a table, a drawer's line, a footnote or a diary entry.
Text a file holds stays text in the note, whatever it says, and reads as it
did."
  (cl-ppcre:regex-replace-all "(?m)^([ \\t]*)(\\*+\\s|#\\+|#(?=[ \\t]|$)|\\||:[-\\w]*:|\\[fn:|%%\\()" text
                              (format nil "\\1~a\\2" *org-zero-width-space*)))

(defun org-safe-link (target words)
  "A link to TARGET, an address a file given to you holds, showing WORDS, Org
text, as a note writes it, on one line: the brackets and control characters of
TARGET written as %5B, %5D, %0A and so on, and the brackets of WORDS left out,
so that neither can end the link or the line."
  (format nil "[[~a][~a]]"
          (cl-ppcre:regex-replace-all "[\\[\\]\\x00-\\x1F\\x7F]" target
                                      (lambda (match) (format nil "%~2,'0X" (char-code (char match 0))))
                                      :simple-calls t)
          (org-safe-line (remove #\] (remove #\[ words)) :escaped t)))

(defun safe-part-name-p (name &key link)
  "True when NAME, the name of a part of a document or of a file in it, may be
written under a directory of ours as it is: a relative path of plain names,
none of them . or .., with no backslash and no control character; with LINK,
one named in a link of a note, no square bracket either. A document given to
you may name its parts ../../init.lisp; such a one is never written."
  (and (plusp (length name))
       (not (char= (char name 0) #\/))
       (notany (lambda (c) (or (char= c #\\) (< (char-code c) 32) (= (char-code c) 127) (and link (find c "[]"))))
               name)
       (every (lambda (piece) (and (plusp (length piece)) (not (member piece '("." "..") :test #'string=))))
              (uiop:split-string name :separator '(#\/)))))

(defun import-picture-file (source directory name pictures)
  "Where the picture SOURCE is saved when a document is imported: DIRECTORY/NAME,
NAME numbered (TUATARA.IO:NUMBERED-NAME) when another picture among
PICTURES, each (SOURCE . FILE), has that file already; the file SOURCE was
given before when it has one."
  (or (cdr (assoc source pictures :test #'equal))
      (loop for n from 1
            for file = (format nil "~a/~a" directory (tuatara.io:numbered-name name n))
            unless (rassoc file pictures :test #'string=) return file)))
