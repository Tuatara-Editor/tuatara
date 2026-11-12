;;;; test/gtk/window-client.lisp --- A client of a daemon in real windows, for the test daemon window-client-in-windows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway):
;;;; NAME RUNTIME FILE DROPPED PICTURE OUT. Starts tuatara --client=NAME --gui
;;;; FILE from these sources, the daemon's sockets in the directory RUNTIME, then
;;;; types in the window, pastes what another program put on the clipboard,
;;;; makes the font a step larger, opens another window with C-x 5 2 (both
;;;; drawn by the renderer the daemon's settings choose), types
;;;; there and closes it with its close button's signal, drops the file
;;;; DROPPED and then the picture PICTURE on the first, and ends the client
;;;; with C-x C-c; it writes to OUT what it saw at each step, and the exit code
;;;; the client ended with, as a property list.

(load (merge-pathnames "steps.lisp" *load-truename*))

(defvar *arguments* (uiop:command-line-arguments)
  "NAME RUNTIME FILE DROPPED PICTURE OUT, as the test gives them.")

(defun font-steps (f)
  "The steps F's window's font is scaled by."
  (tuatara.frontend:frontend-font-steps f))

(defun steps ()
  "Go through the steps, noting what is seen."
  (let ((first (first-window-shown 30)))
    (see :window (and first t))
    (when first
      (see :shows-file (and (wait-until (lambda () (search "kiwi window" (shown-text first))) 30) t))
      (type-keys first "i h e l l o SPC <escape>")
      (see :typed (and (wait-until (lambda () (search "hello kiwi window" (shown-text first))) 30) t))
      ;; another program's copy, taken from the client's clipboard as the daemon pastes
      (tuatara.frontend:frontend-set-clipboard first "from the desktop ")
      (sleep 1)
      (type-keys first "i C-y <escape>")
      (see :pasted (and (wait-until (lambda () (search "from the desktop" (shown-text first))) 30) t))
      ;; the font a step larger in this window
      (type-keys first "C-x C-+")
      (see :font-larger (and (wait-until (lambda () (eql 1 (font-steps first))) 10) t))
      (type-keys first "C-g")
      ;; another window of the client, its font as it started
      (type-keys first "C-x 5 2")
      (see :another-window (and (wait-until (lambda () (= 2 (length (windows)))) 30) t))
      (let ((second (second (windows))))
        (when second
          (see :another-shows (and (wait-until (lambda () (search "hello" (shown-text second))) 30) t))
          (see :another-font-steps (font-steps second))
          ;; both windows drawn as the daemon's init file asks, GSK_RENDERER kept from the programs started
          (see :renderers (list (tuatara.frontend.gtk::gtk-drawn-by first) (tuatara.frontend.gtk::gtk-drawn-by second)))
          (see :renderer-kept (uiop:getenv "GSK_RENDERER"))
          (see :renderer-given (some (lambda (setting) (uiop:string-prefix-p "GSK_RENDERER=" setting))
                                     (tuatara.io:program-environment)))
          (type-keys second "i z z <escape>")
          (see :typed-in-another (and (wait-until (lambda () (search "zz" (shown-text first))) 30) t))
          (setf *to-close* second)
          (tuatara.gtk:g-idle-add (tuatara.gtk:callback on-close-pressed) (tuatara.io:null-pointer))
          (see :closed-another (and (wait-until (lambda () (= 1 (length (windows)))) 30) t))))
      ;; files dropped on the window: a text, then a picture, sent as a file in memory
      (tuatara.frontend.gtk::push-event first :drop :files (list (fourth *arguments*)) :column 1 :row 1)
      (see :dropped (and (wait-until (lambda () (search "dropped kea" (shown-text first))) 30) t))
      (tuatara.frontend.gtk::push-event first :drop :files (list (fifth *arguments*)) :column 1 :row 1)
      (see :picture (and (wait-until (lambda () (plusp (shown-pictures first))) 30) t))
      (type-keys first "C-x C-c"))))

;; the daemon's sockets where the test's daemon listens
(setf tuatara.defaults::*runtime-directory* (second *arguments*))

;; given up on after four minutes, what was seen written all the same
(defvar *steps* (run-steps #'steps (sixth *arguments*) :exit nil)
  "The thread going through the steps.")

(let ((code (tuatara.terminal:main (list (format nil "--client=~a" (first *arguments*)) "--gui" (third *arguments*)))))
  (sb-thread:join-thread *steps* :timeout 30 :default nil)
  (see :exit-code code)
  (see :windows-left (length (windows)))
  (write-seen-and-exit))
