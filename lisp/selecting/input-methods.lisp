;;;; lisp/selecting/input-methods.lisp --- Input methods: keys typed in turn making the characters a keyboard lacks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's input methods (its Quail): a buffer can have one on, and the
;;;; keys typed into it go through the method's rules before they are text.
;;;; Swedish's aa gives å, the digraphs' &e' gives é. A key that starts a
;;;; longer rule shows as typed, underlined, until the next key either goes
;;;; on with the rule or does not; the longest rule at the start is used and
;;;; the rest typed again, as Quail does. The methods and their rules are
;;;; Emacs's own (input-method-table.lisp); the title each shows in the mode
;;;; line is Emacs's too. In NORMAL state keys are commands, so a method
;;;; types only where text is typed.

(in-package #:tuatara.defaults)

(defstruct (input-method (:constructor %make-input-method (name title description rules prefixes)) (:copier nil))
  "An input method: its NAME, the TITLE the mode line shows while it is on, a
one-line DESCRIPTION, its RULES (a hash table of the keys typed in turn to
the text they give) and PREFIXES (every start of a rule's keys that a longer
rule goes on from, as a hash table)."
  name title description rules prefixes)

(setf (documentation 'input-method-p 'function) "True when OBJECT is an INPUT-METHOD.")

(defun make-input-method-of (name title description rules)
  "An input method NAME from RULES, a list of (KEYS . TEXT), its TITLE and DESCRIPTION."
  (let ((table (make-hash-table :test 'equal))
        (prefixes (make-hash-table :test 'equal)))
    (loop for (keys . text) in rules
          do (setf (gethash keys table) text)
             (loop for i from 1 below (length keys) do (setf (gethash (subseq keys 0 i) prefixes) t)))
    (%make-input-method name title description table prefixes)))

(defvar *input-methods*
  (let ((methods (make-hash-table :test 'equal)))
    (loop for (name title description rules) in *input-method-table*
          do (setf (gethash name methods)
                   (make-input-method-of name title description
                                         (if (eq rules :digraphs)
                                             (loop for (keys . code) in *digraph-table*
                                                   collect (cons (concatenate 'string "&" keys) (string (code-char code))))
                                             rules))))
    methods)
  "The input methods, by name: rfc1345 (the digraphs of RFC 1345, after an
ampersand) and those for Swedish, Norwegian, Danish, Finnish, Polish, German,
French, Spanish, Italian and Latin letters.")

(defvar *default-input-method* nil
  "The input method \\[toggle-input-method] turns on in a buffer that has had
none on yet, such as \"swedish-postfix\"; NIL
for it to ask. \\[set-input-method] makes the one chosen the default.")

(defun find-input-method (name)
  "The input method called NAME, or NIL."
  (and name (gethash name *input-methods*)))

(defun buffer-input-method (&optional (buffer (current-buffer)))
  "The input method on in BUFFER, or NIL."
  (find-input-method (tuatara.buffer:buffer-local buffer :input-method)))

;;; Typing through a method

(defun input-method-step (method keys)
  "What KEYS, typed in turn, give under METHOD, as two values: the text settled,
and the keys still waiting, the start of a longer rule (an empty string when
none wait). Keys a longer rule may go on from
wait; else a whole rule gives its text; else the longest whole rule at their
start gives its text, or the first key itself, and the rest is taken again."
  (let ((rules (input-method-rules method))
        (prefixes (input-method-prefixes method))
        (done (make-string-output-stream)))
    (loop
      (cond ((or (zerop (length keys)) (gethash keys prefixes))
             (return))
            ((gethash keys rules)
             (write-string (gethash keys rules) done)
             (setf keys "")
             (return))
            (t (let ((whole (loop for i from (1- (length keys)) downto 1
                                  when (gethash (subseq keys 0 i) rules) return i)))
                 (if whole
                     (progn (write-string (gethash (subseq keys 0 whole) rules) done)
                            (setf keys (subseq keys whole)))
                     (progn (write-char (char keys 0) done)
                            (setf keys (subseq keys 1))))))))
    (values (get-output-stream-string done) keys)))

(defun input-method-shown (method keys)
  "How KEYS waiting under METHOD are shown while they wait: the text of the
rule they make when they make one already, else as typed."
  (or (gethash keys (input-method-rules method)) keys))

(defun input-method-pending (buffer)
  "The keys waiting under BUFFER's input method, as three values: where they
are shown (an octet), the keys, and the text shown; NIL when none wait, or
when what was shown is no longer just before point."
  (let ((pending (tuatara.buffer:buffer-local buffer 'input-method-pending)))
    (when pending
      (destructuring-bind (anchor keys shown) pending
        (let* ((start (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer)))
               (end (+ start (tuatara.text:string-octet-length shown))))
          (when (and (= end (tuatara.buffer:buffer-point buffer))
                     (<= end (tuatara.buffer:buffer-size buffer))
                     (string= shown (tuatara.buffer:buffer-string buffer :start start :end end)))
            (values start keys shown)))))))

(defun input-method-type (buffer method char)
  "Type CHAR into BUFFER at point through METHOD: with the keys waiting before
it, the text they settle on put in place of what they showed, the keys still
waiting shown after it, underlined; one undo step with the typing around it.
True when CHAR was taken as it is, starting and settling nothing."
  (multiple-value-bind (start waiting shown) (input-method-pending buffer)
    (declare (ignore shown))
    (let* ((keys (concatenate 'string (if start waiting "") (string char))))
      (multiple-value-bind (done left) (input-method-step method keys)
        (let* ((show (if (string= left "") "" (input-method-shown method left)))
               (new (concatenate 'string done show))
               (from (or start (tuatara.buffer:buffer-point buffer)))
               (to (tuatara.buffer:buffer-point buffer))
               (octets (tuatara.text:string-octets new)))
          (tuatara.buffer:buffer-edit buffer
                                      (vector (tuatara.text:make-edit from to (if (plusp (length octets)) octets nil)))
                                      :group (eq (editor-last-command *editor*) 'self-insert-command))
          (let ((settled (+ from (tuatara.text:string-octet-length done))))
            (setf (tuatara.buffer:buffer-point buffer) (+ from (length octets))
                  (tuatara.buffer:buffer-local buffer 'input-method-pending)
                  (and (plusp (length left))
                       (list (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) settled) left show))))
          (and (null start) (string= left "") (string= done (string char))))))))

(defun input-method-settle (editor)
  "After each command: keys left waiting under an input method stay as they
are shown, unless the command typed (self-insert-command), which goes on
with them."
  (unless (eq (editor-this-command editor) 'self-insert-command)
    (dolist (buffer tuatara.buffer:*buffers*)
      (when (tuatara.buffer:buffer-local buffer 'input-method-pending)
        (setf (tuatara.buffer:buffer-local buffer 'input-method-pending) nil)))))

(defun input-method-highlights (editor window rows)
  "Highlight function: the keys waiting under WINDOW's buffer's input method,
underlined (:input-method-pending)."
  (declare (ignore editor rows))
  (let ((buffer (tuatara.display:window-buffer window)))
    (multiple-value-bind (start keys shown) (input-method-pending buffer)
      (declare (ignore keys))
      (when start
        (list (list start (+ start (tuatara.text:string-octet-length shown)) :input-method-pending))))))

(defun input-method-mode-line (editor buffer)
  "Mode line function: the title of BUFFER's input method while one is on
\(SV< for Swedish, m for the digraphs)."
  (declare (ignore editor))
  (let ((method (buffer-input-method buffer)))
    (and method (input-method-title method))))

;;; Turning methods on and off

(defun input-method-state-words ()
  "What turning an input method on in the current buffer is said to do: on
now, or, in NORMAL state, once INSERT state is entered."
  (if (and *modal-editing* (eq (modal-state) :normal)) " when you enter INSERT state" ""))

(defun activate-input-method (name &optional (buffer (current-buffer)))
  "Turn on the input method NAME in BUFFER, the one before it off."
  (setf (tuatara.buffer:buffer-local buffer :input-method) name
        (tuatara.buffer:buffer-local buffer 'input-method-last) name
        (tuatara.buffer:buffer-local buffer 'input-method-pending) nil))

(defun deactivate-input-method (&optional (buffer (current-buffer)))
  "Turn off BUFFER's input method; the keys waiting stay as they are shown."
  (setf (tuatara.buffer:buffer-local buffer :input-method) nil
        (tuatara.buffer:buffer-local buffer 'input-method-pending) nil))

(defun input-method-candidates ()
  "The input methods as completion candidates: each name, its title and its description."
  (loop for name in (sort (alexandria:hash-table-keys *input-methods*) #'string<)
        for method = (find-input-method name)
        collect (make-candidate name :annotation (format nil "~a  ~a" (input-method-title method)
                                                          (input-method-description method)))))

(defun read-input-method (prompt)
  "Ask PROMPT for an input method, the default one (*DEFAULT-INPUT-METHOD*)
offered first, taken by \\<minibuffer>\\[minibuffer-done] alone; its name, or
NIL when none was chosen."
  (let ((chosen (completing-read prompt (input-method-candidates) :require-match t :default *default-input-method*)))
    (and (find-input-method chosen) chosen)))

(defun turn-on-input-method (name &optional label)
  "Make NAME the default input method and turn it on in the current buffer,
saying so (by LABEL, when given), as \\[set-input-method] does."
  (activate-input-method name)
  (setf *default-input-method* name)
  (message "~a on~a" (if label (format nil "~a (~a)" label name) name) (input-method-state-words)))

(define-command set-input-method ()
  "Choose an input method and turn it on in this buffer (\\[set-input-method]).
The keys typed then go through its
rules, swedish-postfix's aa typing å, rfc1345's &e' typing é. It becomes the
default, which \\[toggle-input-method] turns on and off from then on;
\\[describe-input-method] lists its rules."
  (let ((name (read-input-method "Input method: ")))
    (when name (turn-on-input-method name))))

(define-command toggle-input-method ()
  "Turn this buffer's input method off, or on again (\\[toggle-input-method]).
On again means the one it had last, else the
default (*default-input-method*), else one asked for (\\[set-input-method])."
  (let* ((buffer (current-buffer))
         (method (buffer-input-method buffer)))
    (if method
        (progn (deactivate-input-method buffer)
               (message "~a off" (input-method-name method)))
        (let ((name (or (and (find-input-method (tuatara.buffer:buffer-local buffer 'input-method-last))
                             (tuatara.buffer:buffer-local buffer 'input-method-last))
                        (and (find-input-method *default-input-method*) *default-input-method*))))
          (if name
              (progn (activate-input-method name buffer)
                     (message "~a on~a" name (input-method-state-words)))
              (set-input-method))))))

(define-command input-method-off ()
  "Turn off this buffer's input method, whichever it is."
  (let ((method (buffer-input-method)))
    (if method
        (progn (deactivate-input-method)
               (message "~a off" (input-method-name method)))
        (message "No input method is on"))))

(define-command describe-input-method ()
  "Show an input method's rules, the keys typed in turn and what they give
\(\\[describe-input-method]): this buffer's, else one asked for."
  (let* ((name (or (tuatara.buffer:buffer-local (current-buffer) :input-method)
                   (read-input-method "Describe input method: ")))
         (method (find-input-method name)))
    (when method
      (let ((rules (sort (loop for keys being the hash-keys of (input-method-rules method) using (hash-value text)
                               collect (cons keys text))
                         #'string< :key #'car)))
        (show-help
         (with-output-to-string (out)
           (format out "Input method: ~a (~a in the mode line)~%~%~a~%~%" name (input-method-title method)
                   (input-method-description method))
           (format out (substitute-command-keys
                        "\\[set-input-method] turns it on, \\[toggle-input-method] off and on again. Keys that start a~%~
longer rule are shown underlined until the next key settles them.~%~%"
                        :marks t :for-format t))
           (format out "  ~12a~a~%  ~12a~a~%" "KEYS" "TYPE" "----" "----")
           (loop for (keys . text) in rules
                 do (format out "  ~12a~a~%" keys text))))))))

;;; The digraphs as an input method

(define-command digraph-input-mode ()
  "Type digraphs as you go: turn on the rfc1345 input method, so that typed
text takes & and a digraph's keys for its character, &e' for e with an
acute; & and keys that make none stay as typed. \\[input-method-off] or
\\[toggle-input-method] turns it off; \\[list-digraphs] lists them."
  (turn-on-input-method "rfc1345" "Digraphs"))

(defun install-input-methods (editor)
  "Bind the input method keys: \\[toggle-input-method],
\\[set-input-method], \\[describe-input-method]; underline the keys that wait,
show the method's title in the mode line, and let the keys typed settle."
  (let ((global (editor-global-keymap editor)))
    (define-key global "C-\\" 'toggle-input-method)
    (define-key global "C-x RET C-\\" 'set-input-method)
    (define-key global "C-h I" 'describe-input-method))
  (pushnew 'input-method-settle (editor-post-command-functions editor))
  (pushnew 'input-method-highlights *highlight-functions*)
  (pushnew 'input-method-mode-line *mode-line-functions*))
