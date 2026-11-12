;;;; test/prefix-keys.lisp --- NORMAL state's commands under C-z, for use without modal editing.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test prefix-keys
  :description "C-z gives, in every state, the commands NORMAL state's keys run, under the same keys
(C-z m f selects the function around point); the commands that have a key of their own in every
state are not repeated there, and the keys M-o M-i M-n M-p work in both modes.")

(defun global-binding (editor keys)
  "The name, in lower case, of the command the global keymap of EDITOR binds KEYS, a key sequence
written as a string, to; :PREFIX when KEYS begin longer ones, NIL when unbound."
  (let ((binding (tuatara.command:lookup-key (tuatara.command:editor-global-keymap editor)
                                             (tuatara.frontend:parse-key-sequence keys))))
    (cond ((null binding) nil)
          ((symbolp binding) (string-downcase (symbol-name binding)))
          (t :prefix))))

(defun command-name (command)
  "COMMAND, a symbol, as a lower case string."
  (string-downcase (symbol-name command)))

(defun modal-off-text (keys text)
  "The text of the buffer of a session that types KEYS on a scratch buffer holding TEXT, with modal
editing off."
  (let ((tuatara.defaults::*modal-editing* nil))
    (session-text (nth-value 1 (run-keys keys :text text :width 60)))))

(define-test (prefix-keys generated-from-normal-state)
  (let ((editor (nth-value 1 (run-keys ""))))
    ;; a sample of each family, under the keys NORMAL state has them on
    (loop for (keys command) in '(("m f" select-function-inside) ("m W" select-symbol) ("m w" select-word)
                                  ("m l" bank-lines) ("m DEL" bank-clear) ("m i" select-inside-pair)
                                  ("f" split-selection) ("t" add-cursor-below) ("T" add-cursor-above)
                                  ("J" select-next-line) ("K" select-previous-line) ("V" select-lines)
                                  ("M" mark-run-toggle) ("S" jump-back) ("g q" fill-region)
                                  (">" indent-selection-or-line)
                                  ("z n" spellcheck-next) ("z p" spellcheck-previous) ("z g" spellcheck-add-word)
                                  ("z f" spellcheck-add-word-to-file) ("r r" replace-regexp)
                                  ("\"" wrap-selection-with-key) ("(" wrap-selection-with-key))
          do (is equal (command-name command) (global-binding editor (format nil "C-z ~a" keys)) "C-z ~a" keys))
    ;; every key of NORMAL state's tables is there, but for the commands left out
    (loop for (keys command) in (append tuatara.defaults::*tuatara-normal-bindings*
                                        tuatara.defaults::*tuatara-syntax-bindings*)
          unless (member command tuatara.defaults::*non-modal-keys-left-out*)
            do (is equal (command-name command) (global-binding editor (format nil "C-z ~a" keys)) "C-z ~a" keys))))

(define-test (prefix-keys left-out-have-their-own-key)
  (let ((editor (nth-value 1 (run-keys ""))))
    ;; one key for a command: C-z w does not exist, M-f is that key
    (loop for keys in '("w" "b" "h" "j" "k" "l" "v" "d" "D" "y" "p" "P" "u" "U" "%" "F" "g g" "g e" "g h" "g l" "g j"
                        "g Q" "z z" "." ":" "?" "m v" "o" "O" "i" "a" "I" "A" "c" "RET" "DEL" "<delete>")
          do (false (global-binding editor (format nil "C-z ~a" keys)) "C-z ~a" keys))
    (is equal "forward-word" (global-binding editor "M-f"))
    (is equal "set-mark-command" (global-binding editor "C-SPC"))
    (is equal "kill-ring-save" (global-binding editor "M-w"))
    (is equal "mark-whole-buffer" (global-binding editor "C-x h"))
    ;; the table names real commands
    (loop for command in tuatara.defaults::*non-modal-keys-left-out*
          do (true (tuatara.command:find-command command) "~a" command))))

(define-test (prefix-keys four-keys-moved)
  (let ((editor (nth-value 1 (run-keys ""))))
    (loop for (old new command) in '(("C-x C-;" "C-z C" toggle-comment-lines)
                                     ("C-M-%" "C-z r q" query-replace-regexp)
                                     ("M-$" "C-z z =" spellcheck-correct)
                                     ("M-i" "C-z TAB" tab-to-tab-stop))
          do (isnt equal (command-name command) (global-binding editor old) "~a" old)
             (is equal (command-name command) (global-binding editor new) "~a" new))
    ;; the old keys are bound to nothing now, but for M-i, which shrinks the selection
    (false (global-binding editor "C-x C-;"))
    (false (global-binding editor "C-M-%"))
    (false (global-binding editor "M-$"))
    (is equal "shrink-selection" (global-binding editor "M-i"))))

(define-test (prefix-keys wrapped-by-pair-keys)
  ;; C-z " puts quotes round the selection, whatever the state
  (is string= "a \"bc\" d" (modal-off-text "C-f C-f C-SPC C-f C-f C-z \"" "a bc d")))

(define-test (prefix-keys syntax-keys-in-both-modes)
  ;; M-o grows the selection to the syntax around it, M-i goes back, M-n and M-p step to the piece
  ;; beside it, with modal editing off as on; C-z m f selects the function
  (let ((rust (format nil "fn main() {~%    let x = foo(a, (b, c), d);~%}~%")))
    (flet ((region (keys)
             (let (result)
               (with-file-session ((f editor) keys rust "rs")
                 (setf result (session-region-text editor)))
               result))
           (to (line column modal)
             (format nil "~{~a~^ ~}" (append (make-list line :initial-element (if modal "j" "C-n"))
                                             (make-list column :initial-element (if modal "l" "C-f"))))))
      (dolist (modal '(nil t))
        (let ((tuatara.defaults::*modal-editing* modal)
              (to (to 1 20 modal)))
          (is string= "(b, c)" (region (format nil "~a M-o" to)) "modal ~a" modal)
          (is string= "(a, (b, c), d)" (region (format nil "~a M-o M-o" to)) "modal ~a" modal)
          (is string= "(b, c)" (region (format nil "~a M-o M-o M-o M-i M-i" to)) "modal ~a" modal)
          (is string= "d" (region (format nil "~a M-o M-n" to)) "modal ~a" modal)
          (is string= "a" (region (format nil "~a M-o M-p" to)) "modal ~a" modal)))
      (let ((tuatara.defaults::*modal-editing* nil))
        (is string= (format nil "{~%    let x = foo(a, (b, c), d);~%}")
            (region (format nil "~a C-z m f" (to 1 20 nil))))))))

(defparameter *prefix-key-cases*
  '(("" "m w") ("C-f C-f" "m W") ("C-e" "m b") ("C-e" "m B") ("" "m s") ("C-e" "m S") ("" "m p") ("C-n C-n" "m P")
    ("C-n C-f C-f C-f C-f C-f C-f C-f C-f C-f C-f C-f C-f" "m i") ("C-n C-f C-f C-f C-f C-f C-f C-f" "m a")
    ("C-n C-f C-f C-f C-f C-f C-f C-f" "m I") ("C-n C-f C-f C-f C-f C-f C-f C-f" "m A")
    ("" "V") ("" "V" "V") ("" "J") ("" "J" "J") ("C-n C-n" "K") ("C-n C-n" "K" "K") ("C-n" "V" "J" "K")
    ("" "m w" (:plain "C-x z")) ("" "M") ("" "M" (:plain "w")) ("" "M" (:plain "w") (:plain "w")) ("" "S") ("C-n C-n" "S")
    ("" "m l") ("C-n" "m l" "m u") ("" "m l" "m DEL")
    ("C-SPC C-f C-f C-f" "m U") ("C-SPC C-n" ">") ("" ">") ("C-SPC C-n" "C") ("" "C") ("" "g q")
    ("C-SPC C-n C-n" "g q")
    ("" "m f") ("" "m F") ("" "m t") ("" "m c") ("" "m y") ("" "m e") ("" "m x") ("" "m r")
    ("" "t") ("" "t" "t") ("" "T") ("C-n C-n" "T" "T") ("" "f") ("C-SPC C-e" "f")
    ("" "z n") ("" "z p") ("" "z =") ("" "z g") ("" "z f") ("" "r q") ("" "r r")
    ("C-SPC C-f C-f C-f" "\"") ("C-SPC C-f C-f C-f" "(") ("" (:plain "C-x z")))
  "Cases of (SETUP COMMAND ...): keys typed first, in both modes, and then each COMMAND, the keys of
NORMAL state with modal editing on, and under C-z with it off. A COMMAND (:PLAIN KEYS) is typed as
it is in both.")

(defun prefix-case-result (setup commands modal)
  "What a session of the case SETUP and COMMANDS leaves with MODAL editing on or off: the text, point,
the selection, and the last row of the screen."
  (let ((tuatara.defaults::*modal-editing* modal)
        (text (format nil "alpha beta. gamma~%delta (eps zeta) eta~%last line here~%")))
    (multiple-value-bind (f editor)
        (run-keys (format nil "~a~{ ~a~}" setup
                          (mapcar (lambda (command)
                                    (cond ((consp command) (second command))
                                          (modal command)
                                          (t (format nil "C-z ~a" command))))
                                  commands))
                  :text text :width 60 :height 8)
      (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (list (tuatara.buffer:buffer-string b) (tuatara.buffer:buffer-point b)
              (and (tuatara.buffer:buffer-mark-active-p b)
                   (multiple-value-list (tuatara.buffer:buffer-region b)))
              (tuatara.frontend.fake:fake-screen-line f 7))))))

(define-test (prefix-keys work-as-in-normal-state)
  ;; each command under C-z does what its key does in NORMAL state: text, point, selection and what
  ;; the echo area says alike
  (dolist (case *prefix-key-cases*)
    (destructuring-bind (setup &rest commands) case
      (let ((normal (prefix-case-result setup commands t))
            (prefixed (prefix-case-result setup commands nil)))
        ;; the last row names the keys, which differ (C-z z n for z n): not compared
        (is equal (butlast normal) (butlast prefixed) "~s" case)))))

(define-test (prefix-keys act-without-modal-editing)
  ;; what a few of the commands do, as they are run with modal editing off
  (let ((tuatara.defaults::*modal-editing* nil))
    (flet ((region (keys)
             (let ((editor (nth-value 1 (run-keys keys :text (format nil "alpha beta. gamma~%delta~%") :width 60))))
               (session-region-text editor))))
      (is equal "alpha" (region "C-z m w"))
      (is equal "alpha beta." (region "C-z m s"))
      (is equal "alpha beta. gamma" (region "C-z V"))
      (is equal (format nil "alpha beta. gamma~%delta~%") (region "C-z m p"))))
  (is string= "    x" (modal-off-text "C-z TAB" "x"))
  (with-file-session ((f editor) "C-z C" (format nil "fn main() {}~%") "rs")
    (is string= (format nil "// fn main() {}~%") (session-text editor)))
  ;; the prompts they open
  (let ((quiet (prefix-case-result "" '() nil)))
    (dolist (keys '("r q" "r r"))
      (isnt equal (fourth quiet) (fourth (prefix-case-result "" (list keys) nil)) "~a" keys))))

(define-test (prefix-keys go-on-without-the-prefix)
  ;; after C-z t, t and T add cursors by themselves; after C-z J, J and K go on; z repeats after C-x z
  (let ((lines (format nil "a~%b~%c~%d~%e~%f~%g~%h~%")))
    (is string= (format nil "Xa~%Xb~%Xc~%d~%e~%f~%g~%h~%") (modal-off-text "C-z t t i X C-g" lines))
    (is string= (format nil "Xa~%Xb~%c~%d~%e~%f~%g~%h~%") (modal-off-text "C-z t i X C-g" lines))
    (is string= (format nil "Xa~%Xb~%Xc~%d~%e~%f~%g~%h~%") (modal-off-text "C-n C-n C-z T T i X C-g" lines))
    (let ((tuatara.defaults::*modal-editing* nil))
      (flet ((point-after (keys)
               (session-point (nth-value 1 (run-keys keys :text lines)))))
        (is = 4 (point-after "C-z J J"))
        (is = 0 (point-after "C-n C-n C-z K K"))
        ;; after C-x z, the z repeats the command before it again: four lines down
        (is = 8 (point-after "C-z J C-x z z z"))))
    ;; a key that is not one of them does what it always does, the run over
    (is string= (format nil "a~%b~%Tc~%d~%e~%f~%g~%h~%") (modal-off-text "C-z J J T" lines))
    ;; the keys that go on are the commands' own, in pairs
    (loop for (command . keys) in '(("add-cursor-below" "t" "T") ("add-cursor-above" "t" "T")
                                    ("select-next-line" "J" "K") ("select-previous-line" "J" "K")
                                    ("spellcheck-next" "n" "p") ("spellcheck-previous" "n" "p"))
          do (let ((entry (loop for name being the hash-keys of tuatara.command::*repeat-maps* using (hash-value entry)
                                when (string-equal (symbol-name name) command) return entry)))
               (true entry "~a" command)
               (when entry
                 (is equal keys (cdr entry) "~a" command))))))

(define-modal-test (prefix-keys plain-keys-go-on-only-after-the-prefix)
  ;; in NORMAL state z n is no run of C-z keys: p after it still pastes
  (flet ((text-after (keys) (session-text (nth-value 1 (run-keys keys :text (format nil "ab cd~%ef~%") :width 60)))))
    (is = 11 (length (text-after "m w y p")))
    (is = 11 (length (text-after "m w y z n p"))))
  ;; the plain keys still go on after C-z with modal editing off
  (let ((tuatara.defaults::*modal-editing* nil))
    (is = 4 (session-point (nth-value 1 (run-keys "C-z J J" :text (format nil "a~%b~%c~%d~%")))))))

(define-test (prefix-keys repeat-on-c-x-z)
  ;; with modal editing off, C-x z runs the last command again and each z after it does too, until
  ;; another key; C-x z is the only key of repeat
  (let ((tuatara.defaults::*modal-editing* nil)
        (lines (format nil "a~%b~%c~%d~%e~%f~%g~%h~%")))
    (flet ((point-after (keys)
             (session-point (nth-value 1 (run-keys keys :text lines)))))
      (is = 6 (point-after "C-n C-n C-x z"))
      (is = 10 (point-after "C-n C-n C-x z z z"))
      ;; a count given before repeat's command is kept
      (is = 12 (point-after "C-u 3 C-n C-x z"))
      ;; another key ends the run: z then types itself
      (is string= (format nil "a~%zb~%c~%d~%e~%f~%g~%h~%")
          (session-text (nth-value 1 (run-keys "C-n C-x z C-p z" :text lines))))))
  (is equal '("C-x z") (loop for (keys command) in tuatara.defaults::*global-bindings*
                             when (eq command 'tuatara.defaults::repeat) collect keys)))
