;;;; lisp/editing/windows.lisp --- Window commands: split, close, move between, swap, rotate, resize.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; Splitting and closing

(defun split-size (total)
  "The rows or columns of TOTAL a new window gets from the prefix argument N:
TOTAL less N when N is positive (the window split keeps N), -N when N is
negative, half when there is none."
  (let ((n (prefix-argument)))
    (cond ((not (integerp n)) nil)
          ((plusp n) (- total n))
          (t (- n)))))

(define-command split-window-below ()
  "Split the selected window into two, one above the other, both showing its
buffer from the same line; the upper one stays selected. With a prefix
argument N, the upper window keeps N rows (the lower one gets -N when N is
negative)."
  (fit-windows *editor*)
  (let ((window (current-window)))
    (split-window window :below :size (split-size (tuatara.display:area-height window)))))

(define-command split-window-right ()
  "Split the selected window into two side by side, both showing its buffer
from the same line; the left one stays selected. With a prefix argument N,
the left window keeps N columns (the right one gets -N when N is negative)."
  (fit-windows *editor*)
  (let ((window (current-window)))
    (split-window window :right :size (split-size (tuatara.display:area-width window)))))

(define-command delete-window ()
  "Close the selected window: its room goes to the window beside it, and the
window used before it is selected. Its buffer stays."
  (remove-window (current-window)))

(define-command delete-other-windows ()
  "Make the selected window fill the screen, closing the others. Their buffers stay."
  (only-window (current-window)))

(define-command quit-window ()
  "Put the selected window back as it was before its buffer was shown: close
it when it was made to show the buffer (a help window, *Messages*), else show
the buffer it showed before. It is \\`q' in every buffer a command makes (lists,
help, reports), which it leaves alike, ending first what such a buffer has of
its own (a rimu list's preview and quick edit)."
  (mode-before-quit (buffer-major-mode (current-buffer)) (current-buffer))
  (leave-window (current-window)))

(define-command kill-buffer-and-window ()
  "Kill the selected window's buffer, asking first when it has changes not
saved, and close the window when there are others."
  (let ((window (current-window)))
    (kill-buffer-asking (current-buffer)
                        :before (lambda () (unless (one-window-p) (remove-window window))))))

;;; Moving between windows

(defun window-on (count)
  "The window COUNT windows on from the selected one, from the top left, the
first again after the last; back when COUNT is negative."
  (let* ((windows (editor-windows))
         (here (position (current-window) windows)))
    (nth (mod (+ here count) (length windows)) windows)))

(define-command other-window ()
  "Select the next window, from the top left, the first again after the last.
With a prefix argument N, the Nth window on, or back when N is negative."
  (select-window (window-on (prefix))))

(defun select-window-towards (direction name)
  "Select the window next to the selected one in DIRECTION, or say there is
none, calling that side NAME."
  (let ((window (window-towards direction)))
    (if window
        (select-window window)
        (message "No window ~a" name))))

(define-command select-window-left ()
  "Select the window to the left of the selected one: the one beside point's line."
  (select-window-towards :left "to the left"))

(define-command select-window-right ()
  "Select the window to the right of the selected one: the one beside point's line."
  (select-window-towards :right "to the right"))

(define-command select-window-above ()
  "Select the window above the selected one: the one over point's column."
  (select-window-towards :above "above"))

(define-command select-window-below ()
  "Select the window below the selected one: the one under point's column."
  (select-window-towards :below "below"))

;;; Swapping and rotating

(defun swap-window-towards (direction name)
  "Swap what the selected window shows with the window next to it in
DIRECTION, or say there is none, calling that side NAME."
  (let ((window (window-towards direction)))
    (if window
        (swap-windows (current-window) window)
        (message "No window ~a" name))))

(define-command swap-window-left ()
  "Swap the selected window's buffer with the window to the left; you go with
your buffer, so it moves left."
  (swap-window-towards :left "to the left"))

(define-command swap-window-right ()
  "Swap the selected window's buffer with the window to the right; you go
with your buffer, so it moves right."
  (swap-window-towards :right "to the right"))

(define-command swap-window-above ()
  "Swap the selected window's buffer with the window above; you go with your
buffer, so it moves up."
  (swap-window-towards :above "above"))

(define-command swap-window-below ()
  "Swap the selected window's buffer with the window below; you go with your
buffer, so it moves down."
  (swap-window-towards :below "below"))

(define-command window-swap-states ()
  "Swap what the selected window shows with the next window (as
\\[other-window] would select it); you go with your buffer."
  (if (one-window-p)
      (message "There is no other window")
      (swap-windows (current-window) (window-on 1))))

(define-command rotate-windows ()
  "Move each window's buffer to the next window, from the top left, and the
last window's to the first; you go with your buffer."
  (if (one-window-p)
      (message "There is no other window")
      (rotate-window-states (editor-windows))))

(define-command rotate-windows-back ()
  "Move each window's buffer to the window before it, from the top left, and
the first window's to the last; you go with your buffer."
  (if (one-window-p)
      (message "There is no other window")
      (rotate-window-states (reverse (editor-windows)))))

(defun turn-windows (how)
  "Rearrange the layout of windows as TURN-LAYOUT does for HOW."
  (tuatara.display:turn-layout (tuatara.display:area-root (current-window)) how)
  (fit-windows *editor*))

(define-command rotate-window-layout-clockwise ()
  "Turn the layout of windows a quarter turn clockwise: windows one above the
other come to stand side by side, the top one on the right, and windows side
by side come to stand one above the other, the left one on top."
  (turn-windows :clockwise))

(define-command rotate-window-layout-anticlockwise ()
  "Turn the layout of windows a quarter turn anticlockwise: windows one above
the other come to stand side by side, the top one on the left, and windows
side by side come to stand one above the other, the right one on top."
  (turn-windows :anticlockwise))

(define-command transpose-window-layout ()
  "Make windows one above the other stand side by side, and windows side by
side one above the other, each keeping its place in the order; again turns
them back."
  (turn-windows :transpose))

(define-command flip-window-layout-horizontally ()
  "Mirror the layout of windows left to right."
  (turn-windows :flip-horizontally))

(define-command flip-window-layout-vertically ()
  "Mirror the layout of windows top to bottom."
  (turn-windows :flip-vertically))

;;; Sizes

(defun resize-selected-window (delta arrangement)
  "Make the selected window DELTA rows (ARRANGEMENT :STACKED) or columns
larger, or smaller when negative, and say so when there is no room."
  (fit-windows *editor*)
  (when (zerop (tuatara.display:resize-area (current-window) delta arrangement))
    (message "The window cannot be made ~:[smaller~;larger~]" (plusp delta)))
  (fit-windows *editor*))

(define-command enlarge-window ()
  "Make the selected window a row taller, or the prefix argument's rows; they
come from the windows below it, then above."
  (resize-selected-window (prefix) :stacked))

(define-command shrink-window ()
  "Make the selected window a row shorter, or the prefix argument's rows; they
go to the window below it, or above."
  (resize-selected-window (- (prefix)) :stacked))

(define-command enlarge-window-horizontally ()
  "Make the selected window a column wider, or the prefix argument's columns;
they come from the windows to its right, then its left."
  (resize-selected-window (prefix) :side-by-side))

(define-command shrink-window-horizontally ()
  "Make the selected window a column narrower, or the prefix argument's
columns; they go to the window to its right, or its left."
  (resize-selected-window (- (prefix)) :side-by-side))

(define-command balance-windows ()
  "Make the windows the same size: each split shares its room equally."
  (tuatara.display:balance-area (tuatara.display:area-root (current-window)))
  (fit-windows *editor*))

(define-command shrink-window-if-larger-than-buffer ()
  "Make the selected window as short as its buffer's lines need, when it is taller."
  (fit-windows *editor*)
  (let* ((window (current-window))
         (need (max tuatara.display:*window-min-height*
                    ;; lines still being counted: a large text, as tall as the window at least
                    (if (tuatara.buffer:buffer-indexed-p (current-buffer))
                        (+ 1 (tuatara.display:window-header-rows window) (tuatara.buffer:buffer-line-count (current-buffer)))
                        (tuatara.display:area-height window))))
         (extra (- (tuatara.display:area-height window) need)))
    (when (plusp extra)
      (tuatara.display:resize-area window (- extra) :stacked)
      (fit-windows *editor*))))

;;; Buffers in other windows

(define-command switch-to-buffer-other-window ()
  "Show a buffer in another window and select it, completing on the live
buffers as \\[switch-to-buffer] does, the ones used lately first; a new name
makes one. With *preview-buffers*, the buffer selected is shown meanwhile."
  (let ((name (read-buffer "Switch to buffer in other window: ")))
    (unless (zerop (length name))
      (pop-to-buffer (find-or-make-buffer name) :other t))))

(define-command find-file-other-window ()
  "Visit a file in another window and select it, completing its name as
find-file does."
  (find-file-reading nil :prompt "Find file in other window: "
                         :show (lambda (buffer) (pop-to-buffer buffer :other t))))

(defun scroll-next-window (direction)
  "Scroll the next window (SCROLL-BY-COUNT, DIRECTION 1 forward or -1 back),
leaving the selected one as it is, or say there is none."
  (let ((next (window-on 1)))
    (if (eq next (current-window))
        (message "There is no other window")
        (scroll-by-count next direction))))

(define-command scroll-other-window ()
  "Scroll the next window forward by nearly a screenful, leaving the selected
one as it is. With a count, that many rows; back when it is negative."
  (scroll-next-window 1))

(define-command scroll-other-window-down ()
  "Scroll the next window backward by nearly a screenful, leaving the
selected one as it is. With a count, that many rows; forward when it is negative."
  (scroll-next-window -1))

;;; Where buffers are shown

(defun describe-which (which)
  "WHICH, a rule's buffers, as a line of text."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (item)
                    (cond ((stringp item) (format nil "~s" item))
                          ((keywordp item) (format nil "~(~s~)" item))
                          (t (format nil "'~(~a~)" item))))
                  which)))

(define-command describe-buffer-places ()
  "Show where buffers are shown, the rules place-buffers makes, the one made
last first: which buffers, the place, the size, and whether the window is
kept for them and selected."
  (show-help
   (with-output-to-string (out)
     (format out "Where buffers are shown~2%")
     (format out "Of the rules that fit a buffer the first counts; a buffer no rule fits~%")
     (format out "is shown in another window (the place :other). Rules are made in the~%")
     (format out (substitute-command-keys "init file with (place-buffers WHICH PLACE ...); \\[describe-function] place-buffers~%"
                                          :marks t :for-format t))
     (format out "has the details.~2%")
     (format out "  ~30a ~10a ~6a ~5a ~a~%" "Buffers" "Place" "Size" "Kept" "Selected")
     (dolist (rule *buffer-places*)
       (format out "  ~30a ~10a ~6a ~5a ~a~%"
               (describe-which (buffer-place-which rule))
               (format nil "~(~s~)" (buffer-place-place rule))
               (let ((size (buffer-place-size rule)))
                 (cond ((null size) "half") ((eq size :fit) "fit") (t size)))
               (if (buffer-place-keep rule) "yes" "no")
               (if (buffer-place-select rule) "yes" "no"))))))

(defparameter *window-bindings*
  '(("C-x 0" delete-window) ("C-x 1" delete-other-windows) ("C-x 2" split-window-below)
    ("C-x 3" split-window-right) ("C-x o" other-window)
    ("C-x ^" enlarge-window) ("C-x }" enlarge-window-horizontally) ("C-x {" shrink-window-horizontally)
    ("C-x -" shrink-window-if-larger-than-buffer) ("C-x +" balance-windows)
    ("C-x 4 b" switch-to-buffer-other-window) ("C-x 4 f" find-file-other-window)
    ("C-x 4 C-f" find-file-other-window) ("C-x 4 0" kill-buffer-and-window)
    ("C-M-v" scroll-other-window) ("C-M-S-v" scroll-other-window-down)
    ("C-M-h" select-window-left) ("C-M-j" select-window-below)
    ("C-M-k" select-window-above) ("C-M-l" select-window-right))
  "The window keys of the global map: the \\`C-x' and \\`C-x 4' keys that
split, close, select and size windows and show a buffer or file in another
window, \\[scroll-other-window] and \\[scroll-other-window-down] to scroll the
next window on and back, and \\[select-window-left], \\[select-window-below],
\\[select-window-above] and \\[select-window-right] to move between windows.")

(defparameter *window-size-repeat-keys*
  '(("{" shrink-window-horizontally) ("}" enlarge-window-horizontally) ("^" enlarge-window) ("v" shrink-window)
    ("<" shrink-window-horizontally) (">" enlarge-window-horizontally) ("+" enlarge-window) ("-" shrink-window))
  "The keys that, right after a command sizing a window, size it again by
themselves: \\`{' \\`}' \\`^' \\`v' as after \\`C-x {' \\`C-x }' \\`C-x ^',
and \\`<' \\`>' \\`+' \\`-' too.")

(defun install-windows ()
  "Bind the window keys, the keys that repeat sizing a window by themselves
(*WINDOW-SIZE-REPEAT-KEYS*), and make the standard rules of where
buffers are shown: *Messages* in a window of its own across the bottom, 30% of
the height, kept for it and selected; help in another window, and *Apropos*
there too, selected."
  (define-keys *global-keymap* *window-bindings*)
  (define-repeat-keys *window-size-repeat-keys*)
  (setf *buffer-places* '())
  (place-buffers :help :other)
  (place-buffers "*Apropos*" :other :select t)
  (place-buffers :messages :bottom :size 0.3 :keep t :select t))
