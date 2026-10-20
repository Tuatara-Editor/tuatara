;;;; lisp/editing/sentences.lisp --- Where sentences end: the marks that end them, and the abbreviations that do not.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defparameter *sentence-enders* ".?!…‽"
  "The characters that can end a sentence.")

(defparameter *sentence-closers* ")]}\"'”’»›"
  "What may follow a sentence's end before the space after it, and belongs to
the sentence: closing quote marks and brackets.")

(defun sentence-end-char-p (char)
  "True for the characters that can end a sentence (*SENTENCE-ENDERS*): . ? ! … and ‽."
  (find char *sentence-enders*))

(defun sentence-closing-char-p (char)
  "True for what may follow a sentence's end before the space after it
\(*SENTENCE-CLOSERS*): closing quote marks and brackets."
  (find char *sentence-closers*))

(defparameter *sentence-abbreviations*
  '("e.g." "i.e." "cf." "viz." "vs." "etc." "al." "et seq." "ibid." "q.v."
    "ca." "approx." "esp." "incl." "excl." "resp." "misc." "a.k.a." "ff."
    "p." "pp." "ed." "eds." "edn." "trans." "repr." "suppl."
    "No." "Nos." "Fig." "Figs." "Ch." "Chap." "Vol." "Vols." "Sec." "Secs."
    "Eq." "Eqs." "Ref." "Refs." "App."
    "Mr." "Mrs." "Ms." "Mx." "Messrs." "Dr." "Prof." "Rev." "Fr." "Msgr."
    "Sr." "Jr." "St." "Hon." "Pres." "Gov." "Sen." "Rep." "Amb."
    "Adm." "Capt." "Cmdr." "Col." "Cpl." "Det." "Gen." "Insp." "Lt."
    "Maj." "Pvt." "Sgt." "Supt."
    "Co." "Corp." "Bros." "Dept." "Univ." "Inst." "Mt." "Ft." "Ave."
    "Blvd." "Rd." "U.S." "U.K." "U.N." "D.C."
    "Jan." "Feb." "Mar." "Apr." "Jun." "Jul." "Aug." "Sep." "Sept."
    "Oct." "Nov." "Dec."
    "Mon." "Tue." "Tues." "Wed." "Weds." "Thu." "Thur." "Thurs." "Fri."
    "Sat." "Sun."
    "a.m." "p.m." "A.M." "P.M." "N.B." "P.S.")
  "Tuatara's abbreviations whose full stop ends no sentence: the sentence
commands, the sentence selections and filling pass over them. Latin and
editorial shorthand, bibliographic marks, forms of address and rank,
organisations and places, the months and days, the clock. Degrees, company
names and units (Ph.D., Inc., lb.) are not among them, as they end sentences
as often as not: *EXTRA-SENTENCE-ABBREVIATIONS* adds them,
*REMOVED-SENTENCE-ABBREVIATIONS* takes these out. Letter case counts, but one
beginning with a small letter counts capitalised too (e.g. and E.g.). Of an
abbreviation of several words, only the last may have a full stop.")

(defvar *extra-sentence-abbreviations* '()
  "Abbreviations of your own whose full stop ends no sentence, added to
Tuatara's (*SENTENCE-ABBREVIATIONS*), each written as typed: '(\"Ph.D.\"
\"et al.\" \"Inc.\").")

(defvar *removed-sentence-abbreviations* '()
  "Abbreviations of Tuatara's (*SENTENCE-ABBREVIATIONS*) that should end a
sentence after all, written as they are there: '(\"St.\" \"No.\").")

(defun sentence-abbreviations-in-force ()
  "The abbreviations whose full stop ends no sentence: Tuatara's
\(*SENTENCE-ABBREVIATIONS*) less *REMOVED-SENTENCE-ABBREVIATIONS*, then
*EXTRA-SENTENCE-ABBREVIATIONS*; only text that is not empty."
  (remove-if-not (lambda (a) (and (stringp a) (plusp (length a))))
                 (append (remove-if (lambda (a) (member a *removed-sentence-abbreviations* :test #'equal))
                                    *sentence-abbreviations*)
                         *extra-sentence-abbreviations*)))

(defvar *abbreviation-scanner* nil
  "The abbreviations in force as one pattern, with the three lists it was made
of and how many characters the longest reaches back, (SCANNER LISTS REACH);
made again when one of the lists is another.")

(defun abbreviation-scanner ()
  "Two values: a pattern matching text that ends in an abbreviation in force
\(SENTENCE-ABBREVIATIONS-IN-FORCE), after a character that is not part of a
word, and how many characters back the longest reaches, that character
included."
  (let ((lists (list *sentence-abbreviations* *extra-sentence-abbreviations* *removed-sentence-abbreviations*))
        (made *abbreviation-scanner*))
    (unless (and made (every #'eq lists (second made)))
      (let* ((in-force (sentence-abbreviations-in-force))
             (all (append in-force
                          (loop for a in in-force
                                when (lower-case-p (char a 0))
                                  collect (concatenate 'string (string (char-upcase (char a 0))) (subseq a 1))))))
        (setf made (list (and all (cl-ppcre:create-scanner
                                   (format nil "\\W(?:~{~a~^|~})$" (mapcar #'cl-ppcre:quote-meta-chars all))))
                         lists
                         (1+ (reduce #'max all :key #'length :initial-value 0)))
              *abbreviation-scanner* made)))
    (values (first made) (third made))))

(defun abbreviation-ending-p (text)
  "True when TEXT, a word or a line's text up to a full stop, ends in an
abbreviation in force (SENTENCE-ABBREVIATIONS-IN-FORCE)."
  (multiple-value-bind (scanner reach) (abbreviation-scanner)
    (and scanner
         (let ((from (max 0 (- (length text) reach))))
           (cl-ppcre:scan scanner (concatenate 'string " " (subseq text from))))
         t)))

(defun abbreviation-before-p (buffer position)
  "True when the text of BUFFER's line before POSITION, just after a full stop,
ends in an abbreviation in force (SENTENCE-ABBREVIATIONS-IN-FORCE)."
  (let* ((line (tuatara.buffer:buffer-line-start buffer position))
         (from (loop with p = position
                     repeat (nth-value 1 (abbreviation-scanner))
                     while (> p line)
                     do (setf p (tuatara.buffer:buffer-forward-char buffer p -1))
                     finally (return p))))
    (abbreviation-ending-p (tuatara.buffer:buffer-string buffer :start from :end position))))
