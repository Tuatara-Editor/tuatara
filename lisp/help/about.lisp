;;;; lisp/help/about.lisp --- About Tuatara: what it is, whose, under which license, and the ways in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x about-tuatara (C-h C-a, as Emacs's about-emacs) shows *About Tuatara*:
;;;; the name in large letters, the version, the copyright and license, where
;;;; its sources are, and the ways in for a newcomer, each a button. C-h C-c shows the license, C-h C-w its
;;;; part on warranty; M-x credits says where Tuatara's ideas come from. A
;;;; button is text that does something when RET is pressed on it or it is
;;;; clicked; TAB and S-TAB go from one to the next.

(in-package #:tuatara.defaults)

(define-face :button "A button: text that does something when \\`RET' is pressed on it or it is clicked."
  :underline t)

(define-face :about-name "Tuatara's name in large letters in *About Tuatara*." :inherit :function :bold t)

(define-face :about-heading "The headings of *About Tuatara*." :bold t)

;;; Buttons

(defun button-at (buffer position)
  "The button of BUFFER holding octet POSITION, as (START END ACTION), or NIL."
  (find-if (lambda (button) (and (<= (first button) position) (< position (second button))))
           (tuatara.buffer:buffer-local buffer :buttons)))

(define-command push-button ()
  "Do what the button at point does: open what it names."
  (let ((button (button-at (current-buffer) (point))))
    (if button
        (funcall (third button))
        (message "No button at point"))))

(define-command forward-button ()
  "Move point to the next button, from the last round to the first."
  (let ((buttons (tuatara.buffer:buffer-local (current-buffer) :buttons)))
    (if buttons
        (goto (first (or (find-if (lambda (button) (> (first button) (point))) buttons) (first buttons))))
        (message "No buttons here"))))

(define-command backward-button ()
  "Move point to the button before, from the first round to the last."
  (let ((buttons (tuatara.buffer:buffer-local (current-buffer) :buttons)))
    (if buttons
        (goto (first (or (find-if (lambda (button) (< (second button) (point))) buttons :from-end t)
                         (car (last buttons)))))
        (message "No buttons here"))))

(defun buttons-in-mode (mode where)
  "Give the buffers of major MODE, WHERE in words for those elsewhere, the keys
of buffers with buttons: \\`RET' and a click do what the button does, \\`TAB'
and \\`S-TAB' go to the next and the one before; the commands run there, and
in the other modes given them so (COMMANDS-FOR-MODE)."
  (define-keys (mode-keymap mode) '(("RET" push-button) ("TAB" forward-button) ("S-TAB" backward-button)
                                    ("<down-mouse-1>" push-button-at-click)))
  (commands-for-mode mode where 'push-button 'forward-button 'backward-button 'push-button-at-click))

(define-command push-button-at-click ()
  "Put point where the first mouse button went down, as \\[mouse-set-point] does,
and do what a button there does."
  (mouse-set-point)
  (let ((button (button-at (current-buffer) (point))))
    (when button (funcall (third button)))))

;;; *About Tuatara*

(define-major-mode about-mode special-mode
  "*About Tuatara*: what Tuatara is, whose, under which license, and the ways
in, each a button: \\<about-mode>\\[push-button] or a click on one opens what it
names, \\[forward-button] and \\[backward-button] go from one to the next,
\\`q' closes; the modal keys that move, select and copy work as in any support
buffer."
  :lighter "About" :interactive nil)

(defparameter *about-name-rows*
  '("▀█▀ █ █ ▄▀▄ ▀█▀ ▄▀▄ █▀▄ ▄▀▄"
    " █  █ █ █▀█  █  █▀█ █▀▄ █▀█"
    " ▀  ▀▀▀ ▀ ▀  ▀  ▀ ▀ ▀ ▀ ▀ ▀")
  "The name in large letters, as *About Tuatara* shows it.")

(defparameter *project-address* "https://github.com/Tuatara-Editor/tuatara"
  "Where Tuatara's sources are, and its faults are reported.")

(defun about-pieces ()
  "The text of *About Tuatara*, as pieces for RENDER-PIECES."
  (collecting-pieces (put)
    (flet ((line (format-string &rest arguments) (put (apply #'format nil format-string arguments)))
           (keys (command) (substitute-command-keys (format nil "\\[~(~a~)]" command))))
      (put (format nil "~%~{  ~a~%~}~%" *about-name-rows*) :about-name)
      (line "  ~a~%~%" (tuatara:version-line (tuatara.frontend:frontend-description (editor-frontend *editor*))))
      (line "  An editor in the manner of Emacs, with modal keys after donkey.el, for text~%")
      (line "  of any size: what takes time is done in the background.~%~%")
      (line "  Copyright (C) 2026 Michael A Jones.~%")
      (line "  Tuatara is free software under the ")
      (put "GNU General Public License" :button #'describe-copying)
      (line ", version 3 or any later~%  version, which you may share and change on its terms (~a).~%" (keys 'describe-copying))
      (line "  It comes with ")
      (put "absolutely no warranty" :button #'describe-no-warranty)
      (line " (~a).~%" (keys 'describe-no-warranty))
      (line "  Where its ideas come from: the ")
      (put "credits" :button #'credits)
      (line " (~a).~%" (keys 'credits))
      (line "  Its sources: ")
      (put *project-address* :button (lambda () (open-address *project-address*)))
      (line "; ")
      (put "report a fault" :button (lambda () (open-address (format nil "~a/issues/new/choose" *project-address*))))
      (line " there.~%~%")
      (put (format nil "  Ways in~%") :about-heading)
      (loop for (label command words) in
            '(("The tutor" tuatara-tutor "learn the modal keys by editing a text with them")
              ("What a key does" describe-key "and the command it runs")
              ("A command" describe-command "what it does, and its keys")
              ("Every key here" describe-bindings "those of the buffer shown and its mode")
              ("Your init file" nil "every setting, commented out, each with what it does")
              ("Checking health" check-health "what works here: grammars, fonts, the clipboard"))
            do (put "  ")
               (put label :button
                    (if command
                        (let ((command command)) (lambda () (funcall command)))
                        (lambda () (visit-path (tuatara.io:native (config-file))))))
               (line "~va ~a~%" (max 1 (- 22 (length label)))
                     "" (format nil "~22a ~a" (if command (keys command) (abbreviated-path (config-file))) words))))))

(define-command about-tuatara ()
  "Show *About Tuatara*, the page that introduces Tuatara.
It gives the version, the copyright and license, where its ideas come from,
where its sources are and faults are reported, and the ways in for a newcomer (the tutor, the help commands, your init
file), each a button."
  (let ((buffer (buffer-in-mode "*About Tuatara*" 'about-mode)))
    (render-pieces buffer #'about-pieces :point :button)
    (show-buffer buffer)))

;;; The license

(defun license-file ()
  "The file of Tuatara's license, the GNU GPL's text: beside the program, above
it (build/ in the source tree), or in the source tree; NIL when there is none."
  (let ((runtime (and sb-ext:*runtime-pathname* (uiop:pathname-directory-pathname sb-ext:*runtime-pathname*))))
    (find-if #'probe-file
             (remove nil (list (and runtime (merge-pathnames "LICENSE" runtime))
                               (and runtime (merge-pathnames "../LICENSE" runtime))
                               (and tuatara.runtime:*source-tree* (merge-pathnames "LICENSE" tuatara.runtime:*source-tree*)))))))

(defun show-license (name start-marker end-marker)
  "Show in a read-only buffer called NAME the text of the license (LICENSE-FILE)
from the line holding START-MARKER to the one before the line holding
END-MARKER, all of it when they are NIL."
  (let ((file (license-file)))
    (if (null file)
        (message "The license's text is not here: it is the GNU GPL, version 3 or later, https://www.gnu.org/licenses/gpl-3.0.html")
        (let* ((text (tuatara.io:file-text file))
               (start (and start-marker (search start-marker text)))
               (from (if start (string-line-start text start) 0))
               (end (and end-marker (search end-marker text :start2 from)))
               (to (if end (string-line-start text end) (length text)))
               (buffer (buffer-in-mode name 'about-mode)))
          (render-pieces buffer (lambda () (list (subseq text from to))) :point :start)
          (show-buffer buffer)))))

(define-command describe-copying ()
  "Show the license Tuatara comes under.
It is the GNU General Public License, version 3 (or, as you choose, any later
version)."
  (show-license "*GNU General Public License*" nil nil))

(define-command describe-no-warranty ()
  "Show the part of the license that says Tuatara comes with no warranty."
  (show-license "*No Warranty*" "15. Disclaimer of Warranty." "END OF TERMS AND CONDITIONS"))

(defparameter *credits*
  '(("GNU Emacs" "the design: buffers for everything, text properties, every key"
                 "a named command, self-documentation; the standard keys, the"
                 "behaviour of most base commands, and many of their test cases")
    ("DONKEY" "the modal keys: donkey.el, by the same author"
              "(github.com/YardQuit/donkey)")
    ("Helix" "each language's rules for colours, indentation, selections and"
             "outlines (under the MPL 2.0), the keys that select by the syntax,"
             "the health report, and each language's servers and formatters")
    ("tree-sitter" "parsing, with the grammars of their many authors")
    ("Org mode" "the Org format and how Org files behave: folding, links,"
                "tables, the agenda, clocking, source blocks, dynamic blocks,"
                "and Org documents written as LaTeX for a PDF")
    ("toc-org" "tables of contents of Org and Markdown files, by Sergei Nosov")
    ("markdown-mode" "how a Markdown link is read, by Jason Blevins")
    ("org-bullets" "signs in place of headlines' stars and lists' bullets,"
                   "with org-superstar")
    ("Denote" "the names of files with formatted names, by Protesilaos Stavrou")
    ("denote-org" "the dynamic blocks of files with formatted names, with"
                  "denote-sequence, also by Protesilaos Stavrou")
    ("vertico, orderless" "the list of completions, matched by words in any order")
    ("marginalia" "what is shown beside each completion")
    ("consult" "the outline of a buffer, and its preview")
    ("Corfu" "completion beside the cursor")
    ("eglot" "how language servers look and feel, with Flymake and ElDoc:"
             "problems marked and shown under the cursor, a call's"
             "parameters in the echo area")
    ("Magit" "the state of a git repository, staging, commits and the log")
    ("EasyPG" "the GnuPG commands, and .gpg files opened and saved")
    ("jinx" "checking the spelling of only the words shown, with enchant"
            "called directly, by Daniel Mendler")
    ("olivetti" "text centred in its window, by Paul W. Rankin; soft wrap at"
                "the fill column, as visual-fill-column wraps")
    ("use-package" "the keywords of load-package in the init file")
    ("ibuffer" "the buffer list")
    ("Vim" "replacing typed at the search prompt, digraphs, the line"
           "selection and the cursor column")
    ("RFC 1345" "the digraphs, as Emacs's rfc1345 input method has them")
    ("Howard Hinnant" "days as numbers and back, for any date (chrono's algorithms)")
    ("Eugene W. Myers" "the difference algorithm the gutter's changes come from")
    ("Unicode UAX #29" "a letter and its accents, or an emoji, taken as one character")
    ("freedesktop.org" "the trash Rimu puts files in and lists")
    ("file" "a file's kind told by its first octets, never by its name")
    ("Ghostty" "the terminal library M-x terminal keeps its screen with"
               "(libghostty-vt), by Mitchell Hashimoto and its contributors")
    ("kitty" "the graphics and keyboard protocols, by Kovid Goyal")
    ("Ao" "the colour theme, by the same author (github.com/YardQuit/ao)")
    ("SBCL" "the Common Lisp Tuatara is written in and runs on"))
  "Where Tuatara's ideas come from, as \\[credits] shows them: each a name and
the lines saying what came from it.")

(define-command credits ()
  "Show where Tuatara's ideas come from: the projects it owes, and what it took
from each."
  (let ((buffer (buffer-in-mode "*Credits*" 'about-mode)))
    (flet ((credits-pieces ()
             (append (list (list (format nil "~%  Credits~2%") :about-heading)
                           (list (format nil "  Tuatara says where its ideas come from.~2%")))
                     (loop for (name . lines) in *credits*
                           collect (list (format nil "  ~22a" name) :about-heading)
                           collect (list (format nil "~{~a~^~%                        ~}~%" lines))))))
      (render-pieces buffer #'credits-pieces :point :button))
    (show-buffer buffer)))

(defun install-about ()
  "Bind the keys of *About Tuatara* and the license's buffers, and the help keys
that show them."
  (buttons-in-mode 'about-mode "*About Tuatara* (\\[about-tuatara])")
  (define-key *global-keymap* "C-h C-a" 'about-tuatara)
  (define-key *global-keymap* "C-h C-c" 'describe-copying)
  (define-key *global-keymap* "C-h C-w" 'describe-no-warranty))
