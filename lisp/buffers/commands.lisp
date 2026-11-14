;;;; lisp/buffers/commands.lisp --- Buffers chosen, switched to, killed and saved.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-x b switches to a buffer chosen with completion, C-x k kills one after
;;;; the questions its features ask (*KILL-BUFFER-QUERY-FUNCTIONS*), and
;;;; C-x s and C-x C-c save the buffers changed, asking about each. READ-BUFFER
;;;; and READ-SEVERAL are the questions every command choosing buffers asks.

(in-package #:tuatara.defaults)

(defun buffer-listed-file (buffer)
  "The file the lists of buffers name for BUFFER: the file it visits, or the
picture, PDF or document it shows; NIL for none."
  (or (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer :viewed-file)))

(defun buffer-candidates ()
  "Completion candidates for the live buffers other than the current one, of
those offered to the client working in the frame (CLIENT-BUFFERS), each with
its size, its major mode and its file (BUFFER-LISTED-FILE) beside it, the
modes in a column as wide as the longest, and a + when it has changes not
saved."
  (let* ((buffers (remove (current-buffer) (client-buffers)))
         (mode-width (loop for b in buffers
                           maximize (length (or (buffer-mode-name b) "")) into widest
                           finally (return (max 12 widest)))))
    (mapcar (lambda (b)
              (let ((file (buffer-listed-file b)))
                (make-candidate (tuatara.buffer:buffer-name b)
                                :annotation (list (list (if (tuatara.buffer:buffer-modified-p b) "+" "")
                                                        :completion-modified 1)
                                                  (list (octets-words (tuatara.buffer:buffer-size b)) :list-size -8)
                                                  (list (or (buffer-mode-name b) "") :completion-major-mode mode-width)
                                                  (list (if file (abbreviated-path file) "")
                                                        :completion-annotation 2/5 :middle)))))
            buffers)))

(defun recent-buffer-names ()
  "The names of the buffers used before the current one, the latest first:
the history that puts them first among the candidates, so that
\\<minibuffer>\\[minibuffer-finish] alone goes back to the buffer used last."
  (loop with client = (current-client)
        for b in *buffer-order*
        unless (or (eq b (current-buffer)) (not (live-buffer-p b)) (not (client-sees-buffer-p client b)))
          collect (tuatara.buffer:buffer-name b)))

(defun split-choices (text)
  "The choices in TEXT, separated by commas, spaces around them dropped."
  (remove "" (mapcar (lambda (k) (string-trim " " k)) (uiop:split-string text :separator ","))
          :test #'string=))

(defun read-several (prompt offered &key (initial "") source (none "none") (more "no more"))
  "Ask with PROMPT for several of the choices OFFERED gives, separated by
commas, completing each in turn: OFFERED, a function of no arguments, gives
them as they are now, each a string or (STRING . ANNOTATION), none chosen
twice. \\<minibuffer>\\[minibuffer-finish] adds the selected one and asks for the
next, \\[minibuffer-complete] puts it in, \\[minibuffer-submit-input] adds what
is typed. While no choice is begun, an empty line comes first, selected,
saying NONE (nothing chosen yet) or MORE: \\[minibuffer-finish] there ends the
question. INITIAL is the text it begins with; SOURCE, for choices found in
the background meanwhile, as COMPLETING-READ takes it. Returns the strings
chosen, in order."
  (let ((text initial))
    (loop
      (let ((got (read-several-once prompt offered text source none more)))
        ;; the empty line taken, or nothing added: the question ends
        (when (equal (split-choices got) (split-choices text))
          (return (split-choices got)))
        (setf text (format nil "~{~a, ~}" (split-choices got)))))))

(defun read-several-once (prompt offered initial source none more)
  "One round of READ-SEVERAL: the line as it is when one choice is taken or the question ends."
  (completing-read prompt
                    (lambda (input)
                      (let* ((comma (position #\, input :from-end t))
                             (before (if comma (subseq input 0 (1+ comma)) ""))
                             (taken (split-choices before)))
                        (append
                         (when (zerop (length (string-trim " " (subseq input (length before)))))
                           (list (make-candidate "" :value before :annotation (if taken more none))))
                         (loop for choice in (funcall offered)
                               for string = (if (consp choice) (car choice) choice)
                               unless (member string taken :test #'string=)
                                 collect (make-candidate string :value (concatenate 'string before string)
                                                                :annotation (and (consp choice) (cdr choice)))))))
                    :separator #\, :initial initial :source source))

(defun read-buffer (prompt &key require-match)
  "Ask PROMPT for a buffer, completing on the live buffers other than the
current one (BUFFER-CANDIDATES), the ones used lately first, so that
\\<minibuffer>\\[minibuffer-finish] alone takes the one used last. With
*PREVIEW-BUFFERS*, the buffer selected is shown meanwhile in the window you
are in, what it showed coming back when the question ends. With
REQUIRE-MATCH, one of them must be chosen; else a new name is taken as typed.
The current buffer's name typed whole is taken, though it is not offered.
Returns the name."
  (call-with-window-preview
   (lambda (candidate made)
     (declare (ignore made))
     (let ((buffer (tuatara.buffer:find-buffer (candidate-string candidate))))
       (and buffer (live-buffer-p buffer) buffer)))
   (lambda (show)
     (completing-read prompt (buffer-candidates) :history (recent-buffer-names) :require-match require-match
                                                 :taken (and (not require-match)
                                                             (list (tuatara.buffer:buffer-name (current-buffer))))
                                                 :preview (and *preview-buffers* show)))))

(define-command switch-to-buffer ()
  "Show another buffer, completing on the live buffers, the ones used lately
first (\\<minibuffer>\\[minibuffer-finish] alone goes back to the one used
last); a new name creates one. With *preview-buffers*, the buffer selected is
shown meanwhile."
  (let ((name (read-buffer "Switch to buffer: ")))
    (unless (zerop (length name))
      (show-buffer (find-or-make-buffer name)))))

(define-command switch-other-buffer ()
  "Switch to the buffer used before this one (one no other window shows,
when there is one), so that doing it again comes back."
  (show-buffer (other-buffer (current-buffer))))

(defvar *kill-buffer-query-functions* '()
  "Functions of a buffer asked before it is killed; each returns true to go
ahead. A replacement running in the background asks here whether to stop.")

(defvar *quit-query-functions* '()
  "Functions of no arguments asked before the editor exits; each returns NIL
to stay, true to go ahead, or a function of no arguments to call once the
exit is certain (stopping its work), so that staying after all changes
nothing.")

(defun query-answer (function going &rest arguments)
  "What FUNCTION, one of a hook's questions before GOING (\"kill\",
\"exit\"), answers when called with ARGUMENTS. When it fails, you are asked
whether to go ahead all the same, with its failure said, so that a broken
question neither lets work go unasked nor keeps you from going: T then, or
NIL to stay."
  (multiple-value-bind (answer failure)
      (let ((tuatara.runtime:*hook-failures-said* nil)) (apply #'call-hook-function function arguments))
    (if (typep failure 'error)
        (yes-or-no-p* (format nil "~a failed (~a); ~a anyway?" (hook-function-name function)
                              (tuatara.runtime:failure-words failure :said t) going))
        answer)))

(defun kill-buffer-allowed-p (buffer)
  "True when every one of *KILL-BUFFER-QUERY-FUNCTIONS* lets BUFFER be killed
\(QUERY-ANSWER)."
  (every (lambda (function) (query-answer function "kill" buffer)) *kill-buffer-query-functions*))

(defun kill-buffer-asking (buffer &key before (ask t) (client (current-client)) every-client)
  "Kill BUFFER, asking first when it has changes not saved unless ASK is NIL,
once every one of *KILL-BUFFER-QUERY-FUNCTIONS* lets it; BEFORE, a function
of none, is called just before it goes. In a daemon, a buffer CLIENT shares
with another running client is left by CLIENT alone (LEAVE-SHARED-BUFFER),
nothing asked, and killed only by the last to have it; with EVERY-CLIENT it
is killed whoever has it. True when it was killed or left."
  (cond ((and (not every-client) (shares-buffer-p client buffer))
         (when before (funcall before))
         (leave-shared-buffer buffer client))
        ((and (kill-buffer-allowed-p buffer)
              (or (not ask)
                  (not (tuatara.buffer:buffer-modified-p buffer))
                  (yes-or-no-p* (format nil "Buffer ~a modified; kill anyway?" (tuatara.buffer:buffer-name buffer)))))
         (when before (funcall before))
         (tuatara.buffer:kill-buffer buffer)
         t)))

(define-command kill-buffer ()
  "Kill the current buffer, asking first when it has changes not saved. Each
window showing it shows the buffer it showed before, or another; a window made
or kept for it closes. In a client of an editor running in the background, a
buffer another client has too is left to that client and only taken from this
one, which shows another of its own in its place."
  (kill-buffer-asking (current-buffer)))

(define-command new-empty-buffer ()
  "Show a new empty buffer, untitled (untitled<2> beside one already there),
for text that has no file yet: \\[save-buffer] asks where to save it. Leaving
the editor asks first while it holds text not saved, as for a file."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "untitled"))))
    (setf (tuatara.buffer:buffer-local buffer 'unsaved-asked) t)
    (show-buffer buffer)
    buffer))

(defun unsaved-p (buffer)
  "True when BUFFER has changes that leaving the editor would lose and asks
about: one visiting a file, or one made by \\[new-empty-buffer], modified."
  (and (or (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer 'unsaved-asked))
       (tuatara.buffer:buffer-modified-p buffer)))

(defun save-buffers-saying (buffers words)
  "Save each of BUFFERS as \\[save-buffer] does (SAVE-BUFFER-WITH-CARE) and, once
those saved are all on disk, say WORDS, a format string given how many: at
once when none is, else when the last save in the background is done. Returns
how many are saved or being saved."
  (let ((started 0) (done 0) (all nil))
    (flet ((say () (when (and all words (= done started)) (message words started))))
      (dolist (buffer buffers)
        (when (save-buffer-with-care buffer :then (lambda (buffer) (declare (ignore buffer)) (incf done) (say)))
          (incf started)))
      (setf all t)
      (say)
      started)))

(defun save-buffers-where (test words)
  "Save each buffer visiting a file with changes not saved that TEST, a
function of the buffer, is true of, and say WORDS of them as
SAVE-BUFFERS-SAYING does; how many are saved or being saved."
  (save-buffers-saying (remove-if-not (lambda (buffer) (and (tuatara.buffer:buffer-file buffer)
                                                            (tuatara.buffer:buffer-modified-p buffer)
                                                            (funcall test buffer)))
                                      tuatara.buffer:*buffers*)
                       words))

(define-command save-some-buffers ()
  "Save every buffer visiting a file with changes not saved, after asking once,
the files named: the files a rename across a project changed, say. In a
client of an editor running in the background, the client's own buffers."
  (let ((changed (remove-if-not (lambda (b) (and (tuatara.buffer:buffer-file b) (tuatara.buffer:buffer-modified-p b)))
                                (client-buffers))))
    (cond ((null changed) (message "(No files need saving)"))
          ((let ((*buffers-asked-about* changed))
             (y-or-n-p* (format nil "Save ~d changed file~:p: ~{~a~^, ~}?"
                                (length changed) (mapcar #'tuatara.buffer:buffer-name changed))))
           (save-buffers-where (lambda (b) (member b changed)) "Saved ~d file~:p"))
          (t (message "Nothing saved")))))

(define-command save-buffers-kill-editor ()
  "Exit the editor, asking first if a buffer visiting a file, or one made by
\\[new-empty-buffer], has unsaved changes. Other buffers without a file, such as the
scratch buffers, are not asked about. In a client of an editor running in the
background, close that client alone, asking nothing but about its encrypted
buffers, which close with it (CLOSE-CLIENT); its other buffers stay in the
editor, for another client to take up, and the editor goes on."
  (when (client-remote (current-client))
    (return-from save-buffers-kill-editor (close-client)))
  (let ((answers (loop for query in *quit-query-functions*
                       for answer = (query-answer query "exit")
                       unless answer return :stay
                       collect answer)))
    (when (and (not (eq answers :stay))
               (or (notany #'unsaved-p tuatara.buffer:*buffers*)
                   (yes-or-no-p* "Modified buffers exist; exit anyway?")))
      ;; what the questions said to stop, now that the exit is certain
      (dolist (answer answers) (when (functionp answer) (call-hook-function answer)))
      (setf (editor-running *editor*) nil))))
