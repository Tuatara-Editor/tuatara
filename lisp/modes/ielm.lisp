;;;; lisp/modes/ielm.lisp --- IELM: typing Lisp to the running editor, a line at a time.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's ielm: M-x ielm shows *ielm*, where Lisp typed after the prompt
;;;; is evaluated in the running editor when RET ends a whole expression, what
;;;; it printed and its values put after it, then a new prompt. The prompt
;;;; names the package the input is read in, which IN-PACKAGE changes. RET on
;;;; an earlier input evaluates it again; C-j evaluates the input as it is;
;;;; M-p and M-n bring back earlier inputs; C-c C-u takes the input away;
;;;; C-c C-b chooses the working buffer, the one the Lisp typed sees as
;;;; current. *, ** and *** hold the last three values, as a Lisp REPL has them.
;;;; Modal editing leaves the buffer alone, as donkey.el does every REPL.

(in-package #:tuatara.defaults)

(define-major-mode ielm-mode lisp-mode
  "Lisp typed to the running editor and evaluated there.
\\<ielm-mode>\\[ielm-return] after a whole expression evaluates it and puts what
it printed and its values after it, then a new prompt; \\[ielm-return] on an
earlier input evaluates it again; \\[ielm-send-input] evaluates the input as it
is; \\[ielm-previous-input] and \\[ielm-next-input] bring back earlier inputs;
\\[ielm-kill-input] takes the input away; \\[ielm-change-working-buffer] chooses
the working buffer, the one the Lisp typed sees as current. \\[ielm-tab] and
\\[completion-at-point] complete names."
  :lighter "IELM" :modal-class :excluded :interactive nil)

(defvar *ielm-keymap* (mode-keymap 'ielm-mode)
  "Keys of *ielm* (ielm-mode's keymap).")

(defparameter *ielm-banner*
  "*** Welcome to IELM ***  \\<ielm-mode>\\[ielm-return] evaluates, \\[ielm-previous-input] and \\[ielm-next-input] bring back earlier input, \\[ielm-change-working-buffer] chooses the working buffer.
"
  "What *ielm* begins with, the keys it names written in when it is put there
(SUBSTITUTE-COMMAND-KEYS).")

(defun ielm-prompt (buffer)
  "The prompt of *ielm* BUFFER: the name of the package input is read in, then \"> \"."
  (format nil "~a> " (package-name (lisp-package buffer))))

(defun ielm-insert-prompt (buffer)
  "Put a new prompt at the end of BUFFER, the input starting after it, point there."
  (let ((end (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer) (ielm-prompt buffer))))
    (setf (tuatara.buffer:buffer-local buffer 'ielm-input-start)
          (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) end)
          (tuatara.buffer:buffer-point buffer) end)))

(defun ielm-input-start (buffer)
  "Where the input after BUFFER's last prompt starts."
  (let ((anchor (tuatara.buffer:buffer-local buffer 'ielm-input-start)))
    (min (tuatara.buffer:buffer-size buffer)
         (or (and anchor (ignore-errors (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer))))
             (tuatara.buffer:buffer-size buffer)))))

(defun prompt-length-at (buffer line)
  "The octets of the prompt that begins the line starting at LINE of BUFFER:
a package's name and \"> \"; NIL when no prompt begins it. Read as octets, the
prompt being ASCII, so that no cut falls inside a character."
  (let* ((end (tuatara.buffer:buffer-line-end buffer line))
         (octets (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start line :end (min end (+ line 128))))
         (gt (position 62 octets)))
    (and gt (plusp gt) (< (1+ gt) (length octets)) (= (aref octets (1+ gt)) 32)
         (loop for i below gt never (tuatara.text:blank-line-octet-p (aref octets i)))
         (+ gt 2))))

(defun status-line-p (buffer line)
  "True when the line starting at LINE of BUFFER is one of *ielm*'s own, the
banner or a \"*** ...\" line, which are no Lisp."
  (let ((size (tuatara.buffer:buffer-size buffer)))
    (and (<= (+ line 4) size)
         (equalp (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start line :end (+ line 4))
                 #(42 42 42 32)))))

(defun ielm-input (buffer)
  "The input typed after BUFFER's last prompt."
  (tuatara.buffer:buffer-string buffer :start (ielm-input-start buffer)))

(define-command ielm ()
  "Show *ielm*, where Lisp typed after the prompt is evaluated in the running
editor: made with a prompt the first time, as it was after."
  (let* ((working (current-buffer))
         (b (find-or-make-buffer "*ielm*" :mode 'ielm-mode
                                          :made (lambda (b)
                                                  (tuatara.buffer:buffer-insert b 0 (substitute-command-keys *ielm-banner*))
                                                  (ielm-insert-prompt b)
                                                  (setf (tuatara.buffer:buffer-undo-stack b) '())
                                                  (tuatara.buffer:buffer-mark-saved b)))))
    (unless (or (eq working b) (live-buffer-p (tuatara.buffer:buffer-local b 'ielm-working-buffer)))
      (setf (tuatara.buffer:buffer-local b 'ielm-working-buffer) working))
    (show-buffer b)))

(defun ielm-complete-p (text package)
  "True when TEXT, read in PACKAGE, holds whole expressions only, none left
open: what \\<ielm-mode>\\[ielm-return] evaluates. Nothing is evaluated reading it."
  (let ((*package* package) (*read-eval* nil) (eof '#:eof))
    (handler-case (with-input-from-string (in text)
                    (loop until (eq (read in nil eof) eof))
                    t)
      (end-of-file () nil)
      ;; a reader error is shown by the evaluation
      (error () t))))

(defun ielm-results (result)
  "What *ielm* puts after an input from RESULT, an evaluation's (RUN-EVALUATION):
what was printed, then each value readably on a line of its own, or the error."
  (let* ((outcome (getf result :outcome))
         (values (and (consp outcome) (eq (first outcome) :values) (second outcome)))
         (*package* (getf result :package *package*)))
    (with-output-to-string (out)
      (write-output-and-values out (getf result :output "") values)
      (cond ((eq outcome :stopped) (format out "*** Evaluation stopped~%"))
            ((eq (first outcome) :error) (format out "*** Eval error ***  ~a~%" (tuatara.runtime:condition-words (second outcome))))
            ((null values) (format out "; No values~%"))))))

(defun ielm-evaluate (buffer)
  "Evaluate the input after BUFFER's last prompt: what it printed and its
values go after it, then a new prompt; the input is remembered for
\\<ielm-mode>\\[ielm-previous-input]."
  (let* ((input (ielm-input buffer))
         (text (string-right-trim '(#\Space #\Tab #\Newline) input)))
    (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer) (string #\Newline))
    (if (zerop (length (string-trim '(#\Space #\Tab #\Newline) text)))
        (ielm-insert-prompt buffer)
        (let* ((working (tuatara.buffer:buffer-local buffer 'ielm-working-buffer))
               (result (run-evaluation text buffer
                                       :current (and (live-buffer-p working) working)))
               (outcome (getf result :outcome)))
          (let ((history (tuatara.buffer:buffer-local buffer 'ielm-history '())))
            (unless (equal text (first history))
              (setf (tuatara.buffer:buffer-local buffer 'ielm-history) (pushed-and-kept text history))))
          (setf (tuatara.buffer:buffer-local buffer 'ielm-history-index) nil
                (tuatara.buffer:buffer-local buffer :lisp-package) (package-name (getf result :package)))
          ;; the first value of each of the last three inputs in *, ** and ***,
          ;; all their values in /, // and ///
          (when (and (consp outcome) (eq (first outcome) :values))
            (let ((values (second outcome)))
              (setf *** ** ** * * (first values)
                    /// // // / / values)))
          (unwind-protect
               (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer) (ielm-results result))
            ;; a new prompt whatever became of the results
            (ielm-insert-prompt buffer))
          (when (and (getf result :backtrace) *backtrace-function*)
            (funcall *backtrace-function* (second outcome) (getf result :backtrace)))))
    ;; point in *ielm* itself: the Lisp typed, or *Backtrace*, may have selected another window
    (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-size buffer))))

(defun ielm-old-input (buffer position)
  "The earlier input around POSITION of BUFFER: from the prompt that begins
POSITION's line, or the nearest line before it that a prompt begins, the first
whole expression after that prompt, over as many lines as it takes; NIL when
no prompt is found, or no expression follows."
  (let ((line (tuatara.buffer:buffer-line-start buffer position)))
    (loop
      (let ((prompt (prompt-length-at buffer line)))
        (when prompt
          (let* ((from (+ line prompt))
                 (to (min (tuatara.buffer:buffer-size buffer) (+ from (* 1024 1024))))
                 (first (find 0 (lisp-elements (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start from :end to))
                              :key #'third)))
            (return (and first (< (+ from (second first)) to)
                         (tuatara.buffer:buffer-string buffer :start (+ from (first first)) :end (+ from (second first))))))))
      (when (zerop line) (return nil))
      (setf line (tuatara.buffer:buffer-line-start buffer (1- line))))))

(defun ielm-replace-input (buffer text)
  "Put TEXT in place of the input after BUFFER's last prompt, point at its end."
  (let ((start (ielm-input-start buffer)))
    (goto (tuatara.buffer:buffer-replace buffer start (tuatara.buffer:buffer-size buffer) text))))

(define-command ielm-return ()
  "\\<ielm-mode>\\[ielm-return] in *ielm*: after the last prompt, evaluate the
input when it is whole expressions, else begin a new line of it; on an earlier
input, evaluate that again."
  (let ((b (current-buffer)))
    (if (< (point) (ielm-input-start b))
        (let ((old (ielm-old-input b (point))))
          (if (null old)
              (message "Not at command line")
              (progn (ielm-replace-input b old) (ielm-evaluate b))))
        (if (ielm-complete-p (ielm-input b) (lisp-package b))
            (ielm-evaluate b)
            (newline)))))

(define-command ielm-send-input ()
  "Evaluate the input after *ielm*'s last prompt as it is, whole or not."
  (ielm-evaluate (current-buffer)))

(defun ielm-history-step (direction)
  "Put the input DIRECTION steps back in *ielm*'s history (1 back, -1 on) in
place of the input typed."
  (let* ((b (current-buffer))
         (history (tuatara.buffer:buffer-local b 'ielm-history '()))
         (index (tuatara.buffer:buffer-local b 'ielm-history-index))
         (next (if index (+ index direction) (if (plusp direction) 0 -1))))
    (cond ((null history) (message "No history"))
          ((>= next (length history)) (message "Beginning of history; no preceding item"))
          ((< next 0)
           (setf (tuatara.buffer:buffer-local b 'ielm-history-index) nil)
           (if index (ielm-replace-input b "") (message "End of history; no next item")))
          (t (setf (tuatara.buffer:buffer-local b 'ielm-history-index) next)
             (ielm-replace-input b (nth next history))))))

(define-command ielm-previous-input ()
  "Put the input before the one shown in *ielm*'s history in place of the input."
  (ielm-history-step 1))

(define-command ielm-next-input ()
  "Put the input after the one shown in *ielm*'s history in place of the input."
  (ielm-history-step -1))

(define-command ielm-tab ()
  "\\<ielm-mode>\\[ielm-tab] in *ielm*: where only blanks come before point on
its line of the input, indent the line as Lisp (never the prompt's line); else
complete the name at point from a list beside it, as \\[completion-at-point]
does."
  (let* ((b (current-buffer))
         (p (point))
         (line (tuatara.buffer:buffer-line-start b p))
         (input (ielm-input-start b))
         (before (tuatara.buffer:buffer-string b :start (max line input) :end (max p (max line input)))))
    (cond ((notevery (lambda (c) (member c '(#\Space #\Tab))) before) (completion-at-point))
          ((< line input) nil)
          (t (unless (eq (indent-lines b line line :blank-lines t) :none)
               (multiple-value-bind (width) (line-indentation b (tuatara.buffer:buffer-line-start b (point)))
                 (let ((line (tuatara.buffer:buffer-line-start b (point))))
                   (when (< (point) (+ line width)) (goto (+ line width))))))))))

(define-command ielm-kill-input ()
  "Take away the input after *ielm*'s last prompt."
  (ielm-replace-input (current-buffer) ""))

(define-command ielm-change-working-buffer ()
  "Choose the working buffer of *ielm*: the one the Lisp typed there sees as
current, (insert \"x\") putting text in it."
  (let* ((b (current-buffer))
         (name (read-buffer "Set working buffer to: " :require-match t))
         (working (tuatara.buffer:find-buffer name)))
    (when working
      (setf (tuatara.buffer:buffer-local b 'ielm-working-buffer) working)
      (message "Working buffer is now: ~a" name))))

(commands-for-mode 'ielm-mode "*ielm* (\\[ielm])" 'ielm-return 'ielm-send-input 'ielm-previous-input
                   'ielm-next-input 'ielm-kill-input 'ielm-change-working-buffer 'ielm-tab)

(defmethod mode-syntax-highlights ((mode ielm-mode) buffer rows)
  "Colour the Lisp typed in BUFFER as Lisp, not its banner and *** lines,
which are no Lisp, and colour its prompts over them."
  (append (let ((plain (loop for row across rows
                             for start = (tuatara.display:row-start row)
                             when (and (not (tuatara.display:row-continuation-p row)) (status-line-p buffer start))
                               collect (cons start (tuatara.display:row-end row)))))
            (remove-if (lambda (span) (find-if (lambda (line) (and (<= (car line) (first span)) (< (first span) (cdr line)))) plain))
                       (call-next-method)))
          (loop for row across rows
                for start = (tuatara.display:row-start row)
                for prompt = (and (not (tuatara.display:row-continuation-p row)) (prompt-length-at buffer start))
                when prompt
                  collect (list start (+ start prompt) :minibuffer-prompt))))

(defun forget-killed-working-buffer (buffer)
  "When BUFFER, killed, is *ielm*'s working buffer, forget it: the next
\\[ielm] takes the buffer it is typed in."
  (let ((ielm (tuatara.buffer:find-buffer "*ielm*")))
    (when (and ielm (eq (tuatara.buffer:buffer-local ielm 'ielm-working-buffer) buffer))
      (setf (tuatara.buffer:buffer-local ielm 'ielm-working-buffer) nil))))

(defun install-ielm ()
  "Bind *ielm*'s keys, and forget its working buffer when that is killed."
  (pushnew 'forget-killed-working-buffer tuatara.buffer:*kill-buffer-functions*)
  (define-keys *ielm-keymap* '(("RET" ielm-return) ("C-j" ielm-send-input) ("M-p" ielm-previous-input)
                               ("M-n" ielm-next-input) ("C-c C-u" ielm-kill-input)
                               ("C-c C-b" ielm-change-working-buffer) ("TAB" ielm-tab))))
