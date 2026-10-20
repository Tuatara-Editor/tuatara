;;;; lisp/editing/toggles.lisp --- Display toggles: ways of showing text, switched on and off one way.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Soft wrap, the line at the fill column, centred text, the highlighted line,
;;;; line numbers and the characters that draw nothing shown as their codes
;;;; (defined here, on in a file opened safely) are each made by
;;;; define-display-toggle, and so behave
;;;; alike: on in a buffer as it chose for itself, else as the list for its
;;;; major mode says, else as the setting for every buffer says; a command for
;;;; the buffer and global- the same for every buffer, which say "Soft wrap on"
;;;; and "Soft wrap off in every buffer"; and a word in the mode line for those
;;;; that move the text about (Wrap, Centred). Each way of showing is read
;;;; afresh whenever a window is drawn, so a change shows at once.

(in-package #:tuatara.defaults)

(defstruct (display-toggle (:constructor make-display-toggle (name label key lookup global modes values word))
                           (:copier nil) (:predicate nil))
  "A way of showing text made by define-display-toggle: its NAME, the LABEL
its messages begin with, the buffer-local KEY a buffer's own choice is kept
under, its LOOKUP function of a buffer, the names of its GLOBAL setting and of
its MODES list, the VALUES it takes when on, each (VALUE WORD), WORD words or
a function of the buffer, or NIL for every buffer, that returns them, and the
WORD the mode line shows while it is on, or NIL for none."
  name label key lookup global modes values word)

(defvar *display-toggles* '()
  "Every way of showing text define-display-toggle has made, as
DISPLAY-TOGGLEs, the last made first.")

(defun find-display-toggle (name)
  "The display toggle called NAME (a DISPLAY-TOGGLE); an error when there is none."
  (or (find name *display-toggles* :key #'display-toggle-name)
      (error "No display toggle called ~(~a~)." name)))

(defun switched-by-count (on)
  "Whether a command that switches something on or off leaves it on, ON
saying whether it is on now: with no count, the other way round; with a
count, on when the count is positive, off when it is zero or less."
  (let ((argument (prefix-argument)))
    (cond ((null argument) (not on))
          ((and (integerp argument) (<= argument 0)) nil)
          (t t))))

(defun choose-display (buffer name value)
  "Make VALUE (NIL for off) BUFFER's own choice for the display toggle called
NAME, which counts before what its major mode and every buffer are set to."
  (setf (tuatara.buffer:buffer-local buffer (display-toggle-key (find-display-toggle name))) value))

(defun set-display (name value &key everywhere)
  "Set the display toggle called NAME to VALUE (NIL for off) for the current
buffer, or with EVERYWHERE for every buffer (its global setting, which buffers
that chose for themselves and the modes its list names keep out of), and say
so in one form for every toggle: Soft wrap on, Line numbers relative, Soft
wrap off in every buffer."
  (let ((toggle (find-display-toggle name)))
    (if everywhere
        (setf (symbol-value (display-toggle-global toggle)) value)
        (choose-display (current-buffer) name value))
    (message "~a ~a~:[~; in every buffer~]" (display-toggle-label toggle)
             (display-toggle-words toggle value (and (not everywhere) (current-buffer)))
             everywhere)))

(defun display-toggle-words (toggle value buffer)
  "What TOGGLE, a DISPLAY-TOGGLE, set to VALUE is said to do in BUFFER, or in
every buffer when BUFFER is NIL: off for NIL, else the word its VALUES give
VALUE, or what their function returns for BUFFER; on when they give none."
  (let ((word (and value (second (assoc value (display-toggle-values toggle))))))
    (cond ((null value) "off")
          ((null word) "on")
          ((stringp word) word)
          (t (funcall word buffer)))))

(defun switch-display (name &key value everywhere)
  "Switch the display toggle called NAME for the current buffer, or with
EVERYWHERE for every buffer, and say so (SET-DISPLAY): to VALUE, or the
toggle's first value, when it is not that now, else off; with a count, to it
when the count is positive, off when it is zero or less."
  (let* ((toggle (find-display-toggle name))
         (value (or value (first (first (display-toggle-values toggle)))))
         (now (if everywhere
                  (symbol-value (display-toggle-global toggle))
                  (funcall (display-toggle-lookup toggle) (current-buffer)))))
    (set-display name (and (switched-by-count (if (eq value t) now (eq now value))) value)
                 :everywhere everywhere)))

(defun display-toggles-mode-line (editor buffer)
  "Mode line function: the words of the display toggles on in BUFFER that
have one, as Wrap while its long lines wrap and Centred while its text is
centred."
  (declare (ignore editor))
  (let ((words (loop for toggle in *display-toggles*
                     when (and (display-toggle-word toggle) (funcall (display-toggle-lookup toggle) buffer))
                       collect (display-toggle-word toggle))))
    (and words (format nil "~{~a~^  ~}" words))))

(defun install-display-toggles ()
  "Show the words of the display toggles on in a buffer in its mode line, and
the characters that draw nothing as their codes in the buffers that want it
\(\\[invisible-characters-mode])."
  (pushnew 'display-toggles-mode-line *mode-line-functions*)
  (setf tuatara.display:*invisible-characters-function* 'invisible-characters-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun filled-docstring (control &rest arguments)
    "CONTROL formatted with ARGUMENTS, as FORMAT does, its words filled into
lines of at most 78 columns, as a docstring is written."
    (let ((words (tuatara.text:split-words (apply #'format nil control arguments) '(#\Space #\Newline)))
          (column 0))
      (with-output-to-string (out)
        (dolist (word words)
          (cond ((zerop column))
                ((> (+ column 1 (length word)) 78) (terpri out) (setf column 0))
                (t (write-char #\Space out) (incf column)))
          (write-string word out)
          (incf column (length word)))))))

(defmacro define-display-toggle (name label doc &key command lookup global default global-doc
                                                     what example (values '((t "on"))) mode-line unless where)
  "Define NAME, a way of showing a buffer's text that is switched on and off:
LOOKUP, a function of a buffer (the current one unless given) true where it
is on, or the value it takes there of VALUES: the buffer's own choice, made
with COMMAND; else what *NAME-MODES*, a list of (MODE . VALUE), says of its
major mode or the nearest mode that mode is made from (MODE-SETTING); else
GLOBAL, the setting for every buffer, DEFAULT unless the init file sets it
\(its docstring GLOBAL-DOC, or one made from WHAT). Never where UNLESS, a list
\(FUNCTION WHERE), says so of the buffer, WHERE saying where in words; on, at
its first value, where WHERE, a list of the same kind, says so of a buffer
that made no choice of its own, whatever its major mode and GLOBAL say. COMMAND,
documented by DOC, switches it for the current buffer, and global-COMMAND for
every buffer (SWITCH-DISPLAY); each says LABEL and what it did. WHAT says in
a few words what it does, as \"wrap long lines at a word's edge\", and
EXAMPLE gives an example of the list of modes, for the documentation. VALUES,
each (VALUE WORD), are what it is when on, the first the one COMMAND switches
to; one, T and on, unless given. WORD is said after LABEL once it is set: words,
or the name of a function of the buffer (NIL when set for every buffer) that
returns them, where what it does differs from buffer to buffer. MODE-LINE is the word the mode line shows
while it is on, or NIL for none. Defined by a package (its Lisp package uses
TUATARA), GLOBAL and *NAME-MODES* are settings of the package, as
DEFINE-PACKAGE-SETTING makes them, so NAME and GLOBAL begin with the
package's name."
  (check-type doc string "a docstring for the command")
  (let* ((package-p (member (find-package '#:tuatara) (package-use-list *package*)))
         (modes (intern (format nil "*~a-MODES*" (symbol-name name))))
         (modes (if package-p (claim-package-setting (symbol-name modes) *package*) modes))
         (global (if package-p (claim-package-setting (symbol-name global) *package*) global))
         (setting (if package-p 'define-package-setting 'defvar))
         (global-command (intern (format nil "GLOBAL-~a" (symbol-name command))))
         (key (intern (symbol-name name) :keyword))
         (valued (not (equal values '((t "on")))))
         (what-capital (concatenate 'string (string-upcase (subseq what 0 1)) (subseq what 1)))
         (count-doc "with a count, on when it is positive, off when it is zero or less."))
    `(progn
       (,setting ,global ,default
         ,(or global-doc
              (filled-docstring "True to ~a, in every buffer (~:[NIL~;T~] unless set); a buffer's own \\[~(~a~)], ~
                           and the list for its major mode in ~a, say otherwise for it. Set it in the init file; ~
                           \\[~(~a~)] switches it while the editor runs."
                      what default command modes global-command)))
       (,setting ,modes '()
         ,(filled-docstring "Major modes whose buffers ~a, or not: a list of (MODE . ~:[ON), ON true or NIL~;~:*VALUE), ~
                       VALUE NIL or one of ~{~(~s~)~^, ~}~], MODE a major mode's name, with or without -mode, for ~
                       example ~a. A mode counts for the modes made from it, the nearest one listed deciding; ~
                       it says more than ~a, and a buffer's own \\[~(~a~)] more than either."
                  what (and valued (mapcar #'first values)) example global command))
       (defun ,lookup (&optional (buffer (current-buffer)))
         ,(filled-docstring "True when BUFFER (the current buffer unless given) is set to ~a, as it chose for ~
                             itself with \\[~(~a~)], ~@[else always ~a, ~]else as ~a says of its major mode, else as ~a says~
                             ~@[; the value is then ~{~(~s~)~^ or ~}~].~@[ Never ~a.~]"
                  what command (second where) modes global (and valued (mapcar #'first values)) (second unless))
         (and ,@(when unless `((not (,(first unless) buffer))))
              ,(if where
                   `(if (and (eq (tuatara.buffer:buffer-local buffer ,key '%unset) '%unset) (,(first where) buffer))
                        ',(first (first values))
                        (buffer-setting buffer ,key (setting-list ',modes ,modes :pairs t) ,global))
                   `(buffer-setting buffer ,key (setting-list ',modes ,modes :pairs t) ,global))))
       (define-command ,command ()
         ,(filled-docstring "~a The buffer keeps its choice whatever ~a and ~a say; ~a" doc modes global count-doc)
         (switch-display ',name))
       (define-command ,global-command ()
         ,(filled-docstring "~a, in every buffer, or stop, while the editor runs (~a in the init file keeps it); a buffer ~
                       that chose with \\[~(~a~)], and the modes ~a names, keep theirs; ~a"
                  what-capital global command modes count-doc)
         (switch-display ',name :everywhere t))
       (setf *display-toggles*
             (cons (make-display-toggle ',name ,label ,key ',lookup ',global ',modes ',values ,mode-line)
                   (remove ',name *display-toggles* :key #'display-toggle-name)))
       ',name)))

;;; The characters that draw nothing, shown as their codes

(defvar *invisible-characters-when-safe* t
  "True to show the characters that draw nothing as their codes (<FEFF>,
<200B>) in every file opened safely (\\[find-file-safely]) that has not
chosen otherwise with \\[invisible-characters-mode]; NIL to leave those
buffers to *INVISIBLE-CHARACTERS-MODES* and *GLOBAL-INVISIBLE-CHARACTERS-MODE*
as any other. T unless set.")

(defun invisible-characters-safe-p (buffer)
  "True when BUFFER shows the characters that draw nothing as their codes for
being opened safely: it is, and *INVISIBLE-CHARACTERS-WHEN-SAFE* says so."
  (and *invisible-characters-when-safe* (tuatara.io:safe-visit-p buffer)))

(define-display-toggle invisible-characters "Invisible characters"
  "Show the characters that draw nothing in the current buffer as their
codes, or stop: the byte-order mark as <FEFF>, a zero-width space as <200B>,
the soft hyphen, the joiners, the word joiner, the marks that set the
direction text is drawn in and the tags the same way, in the
:invisible-character face, each taking the columns its code takes. The
cursor stands on the code's first column while point is on the character,
and the text itself is not changed: selecting, copying and deleting work on
the character as ever. The mode line says Invisible while it is on."
  :command invisible-characters-mode :lookup invisible-characters-p
  :global *global-invisible-characters-mode*
  :what "show the characters that draw nothing as their codes, <FEFF> for the byte-order mark"
  :example "'((text-mode . t) (csv-mode . t))"
  :mode-line "Invisible"
  :where (invisible-characters-safe-p "in a file opened safely (*invisible-characters-when-safe*)"))
