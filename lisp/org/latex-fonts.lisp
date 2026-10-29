;;;; lisp/org/latex-fonts.lisp --- Fonts for the enhanced export: chosen from those installed, and a font for every character.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The enhanced export sets a document in fonts found on this system when
;;;; its style asks for them ({{{fonts}}}): a text font, a sans and a
;;;; fixed-width one for code, each the first installed of a list, or the
;;;; one *LATEX-MAIN-FONT*, *LATEX-SANS-FONT* or *LATEX-MONO-FONT* names.
;;;; Whatever the fonts, each character of the document they do not have is
;;;; set in an installed font that has it ({{{font_fallbacks}}}), and a
;;;; character no font here has is named. What is installed and what each
;;;; font holds is asked of fontconfig (fc-list, fc-match), the list
;;;; xelatex and lualatex themselves load fonts from.

(in-package #:tuatara.defaults)

(defvar *latex-main-font* nil
  "The font the enhanced export sets a document's text in, by its family's
name as fontconfig knows it (\"Noto Serif\"); NIL for the first installed of
Libertinus Serif, Noto Serif, Source Serif 4, Liberation Serif, DejaVu Serif.
Used by the styles that ask for chosen fonts (the standard one, article).")

(defvar *latex-sans-font* nil
  "The sans-serif font of the enhanced export, as *LATEX-MAIN-FONT*; NIL for
the first installed of Libertinus Sans, Noto Sans, Source Sans 3, Liberation
Sans, DejaVu Sans.")

(defvar *latex-mono-font* nil
  "The fixed-width font the enhanced export sets code in, as
*LATEX-MAIN-FONT*; NIL for your window's font (*gui-font*) when it is
installed, else the first of JetBrains Mono, Cascadia Code, Noto Sans Mono,
Source Code Pro, Liberation Mono, DejaVu Sans Mono.")

(defparameter *latex-font-choices*
  '((:main "Libertinus Serif" "Noto Serif" "Source Serif 4" "Liberation Serif" "DejaVu Serif")
    (:sans "Libertinus Sans" "Noto Sans" "Source Sans 3" "Liberation Sans" "DejaVu Sans")
    (:mono "JetBrains Mono" "Cascadia Code" "Noto Sans Mono" "Source Code Pro" "Liberation Mono" "DejaVu Sans Mono"))
  "The fonts tried for each part of a document, in order, when none is set.")

(defparameter *latex-assumed-characters* '((#x20 . #x7e) (#xa0 . #x24f) (#x2000 . #x206f))
  "The characters taken to be in a document's fonts when fontconfig cannot
say (the fonts LaTeX has by standard, which fontconfig may not list): Latin
and its punctuation, as ranges of code points.")

(defun fontconfig-output (program &rest arguments)
  "What fontconfig's PROGRAM (fc-list, fc-match) prints given ARGUMENTS, or NIL
when it is not here or fails; never run through a shell."
  (let ((path (tuatara.io:find-program program)))
    (when path
      (multiple-value-bind (output ok) (tuatara.io:program-output (cons path arguments) :seconds 10)
        (and ok output (plusp (length output)) output)))))

(defun fontconfig-match (family format)
  "What fontconfig prints of the font it shows FAMILY in, FORMAT its
fc-match --format (%{family}|%{file}), or NIL; FAMILY is taken as a name
however it begins."
  (fontconfig-output "fc-match" (concatenate 'string "--format=" format) "--" family))

(defun variable-font-file-p (file)
  "True when FILE, a font's file, is a variable font by its name (-VF., [wght],
Variable), which xelatex and lualatex cannot write into a PDF."
  (and (cl-ppcre:scan "(?i)-VF\\.|\\[[a-z,]+\\]|Variable" file) t))

(defun own-family (families languages)
  "The first of FAMILIES, a font's family names as fontconfig writes them
(\"STIX,STIX Two Text\"), that the font itself gives, with a language in
LANGUAGES (\"en\"): the names fontconfig's own rules put before them, such
as \"STIX\", are not ones xelatex can find the font by."
  (let ((names (uiop:split-string families :separator '(#\,)))
        (given (count-if (lambda (language) (plusp (length language)))
                         (uiop:split-string languages :separator '(#\,)))))
    (string-trim " " (nth (max 0 (- (length names) (max given 1))) names))))

(defun font-listing (charsets &rest pattern)
  "The fonts fontconfig lists for PATTERN (\":charset=2713\") that the LaTeX
programs can load (OpenType or TrueType files, not variable ones,
VARIABLE-FONT-FILE-P), each (FAMILY FILE), a font in several files once for
each, FAMILY the first of its own names (OWN-FAMILY); with CHARSETS, each
\(FAMILY FILE RANGES), RANGES the characters it has as FONT-CHARACTERS gives
them."
  (loop for line in (uiop:split-string (or (apply #'fontconfig-output "fc-list"
                                                  (append pattern (list (if charsets "%{family}|%{familylang}|%{file}|%{charset}\\n" "%{family}|%{familylang}|%{file}\\n"))))
                                           "")
                                       :separator '(#\Newline))
        for bar = (position #\| line)
        for languages-end = (and bar (position #\| line :start (1+ bar)))
        for file-end = (and languages-end (if charsets (position #\| line :start (1+ languages-end)) (length line)))
        for file = (and file-end (subseq line (1+ languages-end) file-end))
        when (and file (plusp bar) (not (variable-font-file-p file))
                  ;; OpenType and TrueType alone: fontspec loads no other kind (Type 1, bitmaps)
                  (cl-ppcre:scan "(?i)\\.(otf|ttf|ttc|otc)$" file))
          collect (list* (own-family (subseq line 0 bar) (subseq line (1+ bar) languages-end)) file
                         (and charsets (list (charset-ranges (subseq line (1+ file-end))))))))

(defun charset-ranges (text)
  "TEXT, a charset as fontconfig writes it (20-7e a0-17f), as a list of
\(FIRST . LAST) code points; what is not a range of hexadecimal numbers
passed over."
  (loop for range in (uiop:split-string text :separator '(#\Space #\Newline))
        for dash = (position #\- range)
        for first = (ignore-errors (parse-integer range :end dash :radix 16))
        for last = (if dash (ignore-errors (parse-integer range :start (1+ dash) :radix 16)) first)
        when (and first last) collect (cons first last)))

(defun installed-font-families (&key every-kind)
  "The families of the fonts installed that the LaTeX programs can use (not
variable ones), each by the first of its own names (OWN-FAMILY), in a table
by name (any case); with EVERY-KIND, of every font installed, by every name
fontconfig finds it by, or NIL when fontconfig is not here."
  (let ((table (make-hash-table :test 'equalp)))
    (if every-kind
        (let ((output (fontconfig-output "fc-list" ":" "-f" "%{family}\\n")))
          (and output
               (dolist (line (uiop:split-string output :separator '(#\Newline)) table)
                 (dolist (name (uiop:split-string line :separator '(#\,)))
                   (let ((name (string-trim " " name)))
                     (when (plusp (length name)) (setf (gethash name table) name)))))))
        (dolist (font (font-listing nil ":" "-f") table)
          (setf (gethash (car font) table) (car font))))))

(defun font-characters (family)
  "The characters FAMILY has, as fontconfig says, a list of (FIRST . LAST) code
points; NIL when it does not know the family."
  (let ((output (fontconfig-match family "%{family}|%{familylang}|%{charset}")))
    (when output
      (let* ((bar (position #\| output))
             (languages-end (and bar (position #\| output :start (1+ bar))))
             (matched (and languages-end (own-family (subseq output 0 bar) (subseq output (1+ bar) languages-end)))))
        ;; fontconfig answers with another font when it has not this one
        (when (and matched (search (string-downcase family) (string-downcase matched)))
          (charset-ranges (subseq output (1+ languages-end))))))))

(defun characters-have-p (ranges code)
  "True when the character with CODE is in RANGES, as FONT-CHARACTERS gives them."
  (some (lambda (r) (<= (car r) code (cdr r))) ranges))

(defun fallback-fonts ()
  "The fonts installed that the LaTeX programs can set a character in, not
of colour pictures nor variable (FONT-LISTING): each (FAMILY FILE RANGES), in
the order fontconfig lists them; asked of it once."
  (font-listing t ":color=false" "-f"))

(defun font-for-character (code fonts)
  "A font among FONTS (FALLBACK-FONTS) that has the character with CODE: a
Noto one when there is one; NIL for none."
  (let ((having (remove-if-not (lambda (font) (characters-have-p (third font) code)) fonts)))
    (first (or (find-if (lambda (font) (search "Noto" (first font))) having) (first having)))))

(defun font-name-family (name installed)
  "The longest of the families INSTALLED, a table by name, that NAME, a
font's name with any style and size after it (JetBrains Mono Medium 12),
begins with; NIL when none does."
  (let ((found nil))
    (maphash (lambda (key family)
               (declare (ignore key))
               (when (and (> (length family) (length found)) (fixed-width-family-p name (list family)))
                 (setf found family)))
             installed)
    found))

(defun chosen-font (part &optional (installed (installed-font-families)))
  "The family the enhanced export sets PART of a document in, :MAIN :SANS or
:MONO: as set, else the first of *LATEX-FONT-CHOICES* installed (for :MONO
your window's font first); NIL when none is. INSTALLED is the table of the
families installed (INSTALLED-FONT-FAMILIES)."
  (let ((set (ecase part (:main *latex-main-font*) (:sans *latex-sans-font*) (:mono *latex-mono-font*)))
        (window (and (eq part :mono) (boundp 'tuatara-user:*gui-font*) (stringp tuatara-user:*gui-font*)
                     (font-name-family tuatara-user:*gui-font* installed))))
    (or (and set (plusp (length set)) set)
        window
        (find-if (lambda (name) (gethash name installed)) (cdr (assoc part *latex-font-choices*))))))

(defun chosen-fonts ()
  "The families the enhanced export sets a document's main text, its sans
serif parts and its code in, each as CHOSEN-FONT gives it: a list of three,
NIL for one not found; the fonts installed looked up once."
  (let ((installed (installed-font-families)))
    (mapcar (lambda (part) (ignore-errors (chosen-font part installed))) '(:main :sans :mono))))
