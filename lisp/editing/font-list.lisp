;;;; lisp/editing/font-list.lisp --- Every font installed, each shown in itself, with the line that chooses it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(define-major-mode font-list-mode special-mode
  "The list of fonts: each family with its variants, the line of the init file
that chooses each, and a sample written in it. \\<font-list-mode>\\[font-list-use]
shows the window in the font on the line, \\[font-list-copy] copies that line
for the init file, \\[search-filter-regex] shows only the fonts whose lines
match what is typed, each under its family, \\[revert-buffer] or \\`g r' lists
them again, \\[quit-window] returns. With the modal keys,
\\<support>\\[beginning-of-buffer], \\`g e', \\[end-of-buffer],
\\[beginning-of-line], \\[end-of-line], \\[start-selection],
\\[select-lines], \\[select-next-line],
\\[select-previous-line], \\[select-whole-buffer] and the \\`m'
selections move and select as in NORMAL state, \\[copy-selection-or-char] copying what is
selected, as in every support buffer."
  :lighter "Fonts" :interactive nil)

(defvar *font-list-keymap* (mode-keymap 'font-list-mode)
  "Keys of the list of fonts (font-list-mode's keymap).")

(defparameter *font-sample* "The quick brown fox jumps over the lazy dog 0O 1lI {}[] -> != =>"
  "The text each font in the list of fonts is shown writing.")

(defun font-init-line (font)
  "The init-file line that chooses FONT for the window."
  (format nil "(setf *gui-font* ~s)" font))

(defun font-sample-face (index font)
  "The face that shows text in FONT, the INDEXth sample of the list."
  (let ((name (intern (format nil "FONT-SAMPLE-~d" index) :keyword)))
    ;; the font is the face's base, so that a theme changed meanwhile keeps it
    (tuatara.display:register-face name "A sample in the list of fonts, written in its own font." (list :font font))
    name))

(defun font-sample-face-p (name)
  "True when NAME is a face the list of fonts made for a sample
\(FONT-SAMPLE-FACE), which a theme does not colour."
  (uiop:string-prefix-p "FONT-SAMPLE-" (symbol-name name)))

(defun font-list-render (buffer faces &key (point :start))
  "Fill BUFFER with FACES (FRONTEND-FONT-FACES's answer), point put as
RENDER-PIECES puts it: each family's line is a heading over its fonts', for
search filters (\\[search-filter-regex])."
  (let* ((lines (make-array 0 :adjustable t :fill-pointer 0)) (sample 0)
         ;; the samples start together, two columns after the longest init-file line
         (column (+ 4 (reduce #'max (loop for entry in faces
                                          append (loop for variant in (third entry)
                                                       collect (length (font-init-line (second variant)))))
                              :initial-value 0)))
         (pieces
           (collecting-pieces (put)
             (labels ((end-line (entry)
                        (put (string #\Newline))
                        (vector-push-extend entry lines))
                      (group (title entries)
                        (when entries
                          (put (format nil "~a: ~d famil~:@p" title (length entries)) :heading)
                          (end-line nil)
                          (end-line nil)
                          (dolist (entry entries)
                            (destructuring-bind (family fixed variants) entry
                              (declare (ignore fixed))
                              (put family :list-name)
                              (end-line (list :family family))
                              (dolist (variant variants)
                                (let ((line (font-init-line (second variant))))
                                  (put "  ")
                                  (put line :font-list-line)
                                  (put (make-string (- column 2 (length line)) :initial-element #\Space))
                                  (put *font-sample* (font-sample-face (incf sample) (second variant)))
                                  (end-line (second variant))))))
                          (end-line nil))))
               (group "Fonts with every character the same width, for the window" (remove-if-not #'second faces))
               (group "Other fonts (letters of their own widths, which leave the window's grid)" (remove-if #'second faces))))))
    (set-search-filtering buffer :nouns "fonts" :heading #'consp)
    (render-pieces buffer pieces :point point :entries lines :refill 'font-list-refill)))

(define-command list-fonts ()
  "List every font installed in *Fonts*, the fixed-width ones first: each family
with its variants, the line of the init file that chooses each, and a sample
written in it. \\<font-list-mode>\\[font-list-use] on a variant shows the window
in it now, \\[font-list-copy] copies its line for the init file,
\\[search-filter-regex] shows only the fonts whose lines match what is typed,
\\[revert-buffer] or \\`g r' lists them again; the modal keys' motions, selections
and \\<support>\\[copy-selection-or-char] work there too. In a terminal the font is
the terminal's."
  (let ((faces (tuatara.frontend:frontend-font-faces (editor-frontend *editor*))))
    (if (null faces)
        (message "Fonts are listed in a window of its own (tuatara --gui): a terminal's font is its own")
        (let ((list (buffer-in-mode "*Fonts*" 'font-list-mode)))
          (font-list-render list faces)
          (show-buffer list)))))

(define-command font-list-use ()
  "Show the window in the font on the current line of the list of fonts, for
this session; \\<font-list-mode>\\[font-list-copy] copies the line that chooses
it for good."
  (let ((font (list-entry-at)))
    (if (not (stringp font))
        (message "No font on this line")
        (let ((full (tuatara.frontend:frontend-set-font (editor-frontend *editor*) font)))
          (message (if full "Font ~a; \\<font-list-mode>\\[font-list-copy] copies its line for the init file" "Font ~a is not to be had")
                   font)))))

(define-command font-list-copy ()
  "Copy the init-file line that chooses the font on the current line, to put
in the init file (\\[find-file] ~/.config/tuatara/init.lisp)."
  (let ((font (list-entry-at)))
    (if (not (stringp font))
        (message "No font on this line")
        (let ((line (font-init-line font)))
          (kill-text line :append nil)
          (message "Copied ~a" line)))))

(defun font-list-refill (buffer)
  "List the fonts again in BUFFER, the list of fonts, fonts installed since
included, narrowed again by its search filters, point on the same line
\(\\[revert-buffer])."
  (let ((faces (tuatara.frontend:frontend-font-faces (editor-frontend *editor*))))
    (when faces
      (font-list-render buffer faces :point :keep))))

(defun install-font-list ()
  "Bind the keys of the list of fonts."
  (define-keys *font-list-keymap* '(("RET" font-list-use) ("w" font-list-copy))))

(commands-for-mode 'font-list-mode "the list of fonts (\\[list-fonts])"
                   'font-list-use 'font-list-copy)
