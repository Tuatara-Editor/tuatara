;;;; test/gtk/desktop-font.lisp --- The desktop's font followed, for the test gtk desktop-font-followed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway),
;;;; the desktop's settings in memory: FILE OUT. Starts Tuatara from these
;;;; sources with --gui on FILE, no font named, changes the desktop's monospace
;;;; font, and writes to OUT what it saw, as a property list: whether the
;;;; desktop has the setting, the font in use before and after.

(load (merge-pathnames "steps.lisp" *load-truename*))

(tuatara.runtime:define-c-function ("g_settings_set_string" g-settings-set-string) :boolean
  (settings :pointer) (key :string) (value :string))

(tuatara.frontend.gtk::define-callback on-change-desktop-font :boolean ((data :pointer))
  (declare (ignore data))
  ;; as the desktop's own settings program does: a value given to the setting, from elsewhere
  (let ((settings (tuatara.gtk:g-settings-of "org.gnome.desktop.interface" "monospace-font-name")))
    (when settings
      (g-settings-set-string settings "monospace-font-name" "Kiwi Mono 15")
      (tuatara.gtk:g-object-unref settings)))
  nil)

(defun font-in-use (f)
  "The name of the font F's window is written in."
  (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock f)) (tuatara.frontend.gtk::gtk-font-in-use f)))

(defun steps ()
  "Go through the steps, noting what is seen."
  (let ((first (first-window-shown 30)))
    (see :window (and first t))
    (when first
      (see :desktop-has-it (and (tuatara.frontend.gtk::gtk-desktop-settings first) t))
      (see :font-before (font-in-use first))
      (type-keys first "i a b c <escape>")
      (wait-until (lambda () (search "abc" (shown-text first))) 30)
      (tuatara.gtk:g-idle-add (tuatara.gtk:callback on-change-desktop-font) (tuatara.runtime:null-pointer))
      (wait-until (lambda () (equal "Kiwi Mono 15" (font-in-use first))) 10)
      (see :font-after (font-in-use first)))))

(run-steps #'steps (second (uiop:command-line-arguments)))

(tuatara.terminal:main (list "--gui" (first (uiop:command-line-arguments))))
