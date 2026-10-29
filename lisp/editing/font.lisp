;;;; lisp/editing/font.lisp --- The window's font (larger, smaller, another), and filling the screen.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defparameter *font-steps-limits* '(-5 . 15)
  "The fewest and most steps the window's font can be scaled by, each 1.2
times the last: from 0.4 to 15 times the size it started with.")

(defun scale-font (steps)
  "Scale the window's font to STEPS steps from the size it started with, kept
within *FONT-STEPS-LIMITS*, and say so; in a terminal, say that the terminal
sets the size. Returns the steps, or NIL in a terminal."
  (let ((frontend (editor-frontend *editor*)))
    (if (null (tuatara.frontend:frontend-font-steps frontend))
        (progn (message "In a terminal the font size is the terminal's to change") nil)
        (let ((steps (setf (tuatara.frontend:frontend-font-steps frontend)
                           (max (car *font-steps-limits*) (min (cdr *font-steps-limits*) steps)))))
          (message "Font size ~:[~@d~;as it started~]; \\`+', \\`-' and \\`0' go on changing it" (zerop steps) steps)
          steps))))

(defun font-steps ()
  "The steps the window's font is scaled by now; 0 in a terminal."
  (or (tuatara.frontend:frontend-font-steps (editor-frontend *editor*)) 0))

(define-command text-scale-adjust ()
  "Make the window's font larger or smaller, by the last key of the keys that
ran it: \\`-' a step smaller, \\`0' the size it started with, any other
(\\`+', \\`=') a step larger, 1.2 times; with a prefix argument, that many
steps. Right after it, \\`+', \\`=', \\`-' and \\`0' alone go on. The whole
window's font changes, and the window shows more or fewer rows and columns; in
a terminal the font is the terminal's."
  (let* ((key (editor-this-key *editor*))
         (base (and key (tuatara.frontend:key-base key))))
    (case base
      (#\- (scale-font (- (font-steps) (prefix))))
      (#\0 (scale-font 0))
      (t (scale-font (+ (font-steps) (prefix)))))))

(define-command text-scale-increase ()
  "Make the window's font a step larger (1.2 times), or the prefix argument's
worth of steps."
  (scale-font (+ (font-steps) (prefix))))

(define-command text-scale-decrease ()
  "Make the window's font a step smaller, or the prefix argument's worth of steps."
  (scale-font (- (font-steps) (prefix))))

(define-command mouse-wheel-text-scale ()
  "Make the window's font a step larger as the mouse wheel turns up with Ctrl
held (\\`C-<wheel-up>'), a step smaller as it turns down."
  (let ((key (editor-this-key *editor*)))
    (scale-font (+ (font-steps) (if (and key (eq (tuatara.frontend:key-base key) :wheel-up)) 1 -1)))))

(defun fixed-width-family-p (name families)
  "True when NAME, a font family with any style and size after it, begins with
one of FAMILIES, whole words matched in any case."
  (some (lambda (family)
          (and (<= (length family) (length name))
               (string-equal family name :end2 (length family))
               (or (= (length family) (length name)) (char= (char name (length family)) #\Space))))
        families))

(define-command set-frame-font ()
  "Choose the window's font among the fixed-width fonts installed: a family,
with any style and size typed after it (DejaVu Sans Mono Bold 12); without a
size, the size in use stays, and so does any scaling by \\[text-scale-adjust].
A family whose letters are not all as wide, typed by hand, is refused, as the
window's text stands in columns. It lasts for this session; *gui-font* in the
init file sets it for good. In a terminal the font is the terminal's."
  (let* ((frontend (editor-frontend *editor*))
         (families (tuatara.frontend:frontend-fonts frontend)))
    (if (null families)
        (message "In a terminal the font is the terminal's to change")
        (let* ((name (string-trim " " (completing-read "Font: " families)))
               (family (and (plusp (length name)) (fixed-width-family-p name families)))
               (full (and family (tuatara.frontend:frontend-set-font frontend name))))
          (cond (full (message "Font ~a" full))
                ((and (not family)
                      (fixed-width-family-p name (mapcar #'first (tuatara.frontend:frontend-font-faces frontend))))
                 (message "~a is no fixed-width font, and the window's text stands in columns: the font is as it was"
                          (shown-font-family name)))
                (t (message "No font family ~a is installed" (shown-font-family name))))))))

(defun shown-font-family (name)
  "NAME, a font as typed, without the size at its end."
  (cl-ppcre:regex-replace "\\s+[0-9]+(?:\\.[0-9]+)?$" name ""))

(defun toggle-window-state (state)
  "Turn the window to STATE, :fullscreen or :maximised, or back; in a
terminal, say that its window is the terminal's."
  (unless (tuatara.frontend:frontend-toggle-state (editor-frontend *editor*) state)
    (message "In a terminal the window is the terminal's to change")))

(define-command toggle-frame-fullscreen ()
  "Make the window fill the screen, without its title bar, or go back to how it
was."
  (toggle-window-state :fullscreen))

(define-command toggle-frame-maximized ()
  "Make the window as large as the screen allows, or go back to its size before."
  (toggle-window-state :maximised))

(defparameter *font-size-bindings*
  '(("C-x C-+" text-scale-adjust) ("C-x C-=" text-scale-adjust) ("C-x C--" text-scale-adjust)
    ("C-x C-0" text-scale-adjust) ("C-<wheel-up>" mouse-wheel-text-scale) ("C-<wheel-down>" mouse-wheel-text-scale)
    ("<f11>" toggle-frame-fullscreen) ("M-<f10>" toggle-frame-maximized))
  "The keys of the global map that change the window's font size, and
\\[toggle-frame-fullscreen] and \\[toggle-frame-maximized] for filling the
screen and maximising.")

(defparameter *font-size-repeat-keys*
  '(("+" text-scale-adjust) ("=" text-scale-adjust) ("-" text-scale-adjust) ("0" text-scale-adjust))
  "The keys that, right after \\[text-scale-adjust], \\`C-x C-=', \\`C-x C--' or
\\`C-x C-0', change the font size again by themselves.")

(defun install-font ()
  "Bind the keys that change the window's font size, and the keys that repeat
them, and \\[toggle-frame-fullscreen] and \\[toggle-frame-maximized]."
  (define-keys *global-keymap* *font-size-bindings*)
  (define-repeat-keys *font-size-repeat-keys*))

(pushnew 'mouse-wheel-text-scale *not-repeatable*)
