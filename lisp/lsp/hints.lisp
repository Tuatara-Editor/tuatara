;;;; lisp/lsp/hints.lisp --- Inlay hints: the types and parameter names a language server shows in code.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A server that gives inlay hints shows, faint within a line, what the code
;;;; leaves unsaid, a binding's type (`let total: i32 = ...') and the name of
;;;; the parameter an argument goes to (`add(a: 1, b: 2)'). They are no text of
;;;; the buffer: the cursor passes them by, a copy leaves them out, a save never
;;;; writes them. Those of the lines shown are asked for, and again as other
;;;; lines come into view and each time the text is sent to the server;
;;;; meanwhile they move with the text typed, so that the line never jumps. M-x
;;;; lsp-inlay-hints-mode turns them off and on. Code lenses, what a server says
;;;; of a line (3 references, Run test), are shown so at the line's end when M-x
;;;; lsp-code-lenses-mode is on, M-x lsp-code-lens running one.

(in-package #:tuatara.lsp)

(define-setting *lsp-inlay-hints* t
  "Whether language servers' inlay hints are shown within the code: the types
of bindings, the names of the parameters arguments go to.
- t: they are shown.
- nil: they are not."
  :values boolean)

(define-setting *lsp-code-lenses* nil
  "Whether what language servers say of lines (3 references, Run test) is shown
faint at each line's end.
- t: it is shown.
- nil: it is not, as standard."
  :values boolean)

(defvar *lsp-most-hints* 5000
  "The most inlay hints kept for one buffer.")

(defun hint-label (hint)
  "The text HINT, an inlay hint a server sent, shows: its label, with the
spaces it asks for either side."
  (format nil "~:[~; ~]~a~:[~; ~]"
          (tuatara.text:json-true-p (tuatara.text:json-get hint "paddingLeft"))
          (or (tuatara.text:json-text hint "label")
              (format nil "~{~a~}" (map 'list
                                        (lambda (part) (or (tuatara.text:json-text part "value") ""))
                                        (tuatara.text:json-items hint "label"))))
          (tuatara.text:json-true-p (tuatara.text:json-get hint "paddingRight"))))

(defvar *lsp-hint-margin* 100
  "How many lines above and below those the windows show have their inlay
hints asked for with them, so that a scroll within that has them at once.")

(defun hint-range (server doc lines)
  "The range of DOC's text as its servers have it holding LINES, (FIRST . LAST),
as SERVER counts it: from the start of FIRST to the start of LAST, or to the
end of the text when LAST is past it."
  (let ((shown (lsp-doc-shown-rope doc)))
    (flet ((line-start (line) (tuatara.text:json-object "line" line "character" 0)))
      (tuatara.text:json-object "start" (line-start (min (car lines) (tuatara.text:rope-lines shown)))
                                "end" (if (<= (cdr lines) (tuatara.text:rope-lines shown))
                                          (line-start (cdr lines))
                                          (lsp-position server shown (tuatara.text:rope-octet-length shown)))))))

(defun lsp-ask-hints (doc &optional lines)
  "Ask DOC's server that gives inlay hints for those of LINES, (FIRST . LAST),
or else of the lines last asked of (the lines shown and a margin, as
NOTE-HINT-VIEW asks), kept in place of those before to be shown once they
come while its buffer's text is still as asked of."
  (let ((server (and *lsp-inlay-hints* (doc-server-offers doc "inlayHintProvider" "inlay-hints")))
        (lines (or lines (lsp-doc-hint-lines doc)))
        (buffer (lsp-doc-buffer doc)))
    (when (and server lines)
      (setf (lsp-doc-hint-lines doc) lines)
      (let ((version (lsp-doc-version doc)))
        (lsp-ask-anew doc :hints server "textDocument/inlayHint"
                      (tuatara.text:json-object "textDocument" (doc-identifier doc)
                                                "range" (hint-range server doc lines))
                      (lambda (result error)
                        (when (and (null error) (doc-unchanged-p doc version))
                          (let ((hints (loop for hint across (tuatara.text:json-items result)
                                             for i below *lsp-most-hints*
                                             for at = (doc-byte doc server (tuatara.text:json-get hint "position"))
                                             when at
                                             collect (list at at (hint-label hint)))))
                            (keep-placed buffer 'lsp-hints doc (and hints
                                                                    (coerce (stable-sort hints #'< :key #'car)
                                                                            'simple-vector)))
                            (show-hints-in buffer)))))))))

(defun shown-lines (editor buffer window rows)
  "The lines of BUFFER the windows of EDITOR showing it show, as (FIRST .
LAST), LAST the line after the last: those of WINDOW from ROWS, its rows now,
and of the others from their last layout; NIL when none shows any."
  (let ((rope (tuatara.buffer:buffer-text buffer)) (first nil) (last nil))
    (dolist (w (cons window (remove window (editor-windows editor))))
      (let ((rows (if (eq w window) rows (tuatara.display:window-rows w))))
        (when (and (eq (tuatara.display:window-buffer w) buffer) (plusp (length rows)))
          (let ((from (tuatara.text:byte->line rope (tuatara.display:row-start (aref rows 0))))
                (to (1+ (tuatara.text:byte->line rope (tuatara.display:row-end (aref rows (1- (length rows))))))))
            (setf first (if first (min first from) from)
                  last (if last (max last to) to))))))
    (and first (cons first last))))

(defun note-hint-view (editor window rows)
  "As WINDOW of EDITOR is drawn with ROWS: when its buffer's documents have
servers giving inlay hints and the lines its windows show are not all among
those asked of, ask for those lines and *LSP-HINT-MARGIN* more either side;
the same for what its code lenses say (RESOLVE-SHOWN-LENSES). A function of
*VIEW-CHANGE-FUNCTIONS*. Complexity: O(log lines) a window, unless it asks."
  (let ((buffer (tuatara.display:window-buffer window))
        (shown nil))
    (when (and *lsp-code-lenses* (plusp (length rows)) (tuatara.buffer:buffer-local buffer 'lsp-lenses))
      (setf shown (shown-lines editor buffer window rows))
      (resolve-shown-lenses buffer shown))
    (when (and *lsp-inlay-hints* (plusp (length rows)))
      (dolist (doc (buffer-docs buffer))
        (when (doc-server-offers doc "inlayHintProvider" "inlay-hints")
          (unless shown
            (setf shown (shown-lines editor buffer window rows)))
          (let ((asked (lsp-doc-hint-lines doc)))
            (unless (and asked (<= (car asked) (car shown)) (<= (cdr shown) (cdr asked)))
              (lsp-ask-hints doc (cons (max 0 (- (car shown) *lsp-hint-margin*))
                                       (+ (cdr shown) *lsp-hint-margin*)))))))))
  nil)

(defun hint-replacements (buffer start end)
  "The inlay hints and code lenses of BUFFER from START below END as the
display shows text standing where nothing is hidden: each (AT AT TEXT FACE),
in order of place. Complexity: O(log hints + those there)."
  (let ((found '()))
    (loop for (nil . placed) in (tuatara.buffer:buffer-local buffer 'lsp-hints)
          do (let ((these (loop for i from (placed-first placed buffer (lambda (hint) (>= (first hint) start)))
                                below (length (placed-items placed))
                                for (at nil text face) = (placed-item placed i buffer)
                                while (< at end)
                                collect (list at at text (or face :inlay-hint)))))
               (setf found (if found (merge 'list found these #'< :key #'car) these))))
    found))

(defun show-hints-in (buffer)
  "Have BUFFER's display show its inlay hints, beside what its mode shows
otherwise (TUATARA.DISPLAY:ADD-REPLACEMENTS)."
  (tuatara.display:add-replacements buffer :hints 'hint-replacements))

(defun hints-doc (key)
  "The document a key of a buffer's hints is for: the key, or for code lenses its first."
  (if (consp key) (car key) key))

(defun forget-hints (buffer &optional doc)
  "BUFFER's inlay hints and code lenses let go, those of DOC alone when given;
its display as its mode has it once none are left."
  (setf (tuatara.buffer:buffer-local buffer 'lsp-hints)
        (and doc (remove doc (tuatara.buffer:buffer-local buffer 'lsp-hints) :key #'hints-doc)))
  (unless (tuatara.buffer:buffer-local buffer 'lsp-hints)
    (tuatara.display:remove-replacements buffer :hints)))

(defun toggle-server-showing (setting ask forget words)
  "Turn SETTING (a variable's symbol), what servers show in buffers, on or
off: on, each buffer's documents asked for it by ASK, a function of a
document; off, each buffer's let go by FORGET, a function of a buffer. Say
it, WORDS naming what it is."
  (let ((on (tuatara.runtime:set-setting setting (not (symbol-value setting)))))
    (dolist (buffer tuatara.buffer:*buffers*)
      (if on
          (mapc ask (buffer-docs buffer))
          (funcall forget buffer)))
    (message "~a ~:[off~;on~]" words on)))

(define-command lsp-inlay-hints-mode ()
  "Show the language servers' inlay hints within the code, or no longer:
the types of bindings, the names of the parameters arguments go to."
  (toggle-server-showing '*lsp-inlay-hints* #'lsp-ask-hints #'forget-hints "Inlay hints"))

;;; Code lenses

(defun lens-title (lens)
  "What LENS, a code lens, says: its command's title."
  (tuatara.text:json-text lens "command" "title"))

(defun show-lenses (buffer doc server lenses)
  "Show LENSES, SERVER's code lenses for DOC, at the ends of their lines in
BUFFER, those of a line joined, those with no title yet left out."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (by-line (make-hash-table)))
    (dolist (lens lenses)
      (let ((line (lens-line doc server lens)))
        (when (and line (lens-title lens))
          (push lens (gethash line by-line)))))
    (let ((items (loop for line being the hash-keys of by-line using (hash-value here)
                       collect (let ((end (line-end-byte rope line)))
                                 (list end end (format nil "  ~{~a~^ | ~}" (mapcar #'lens-title (reverse here)))
                                       :code-lens)))))
      (setf (tuatara.buffer:buffer-local buffer 'lsp-lenses) (cons server lenses))
      (keep-placed buffer 'lsp-hints (cons doc :lenses) (and items (coerce (sort items #'< :key #'car) 'simple-vector)))
      (show-hints-in buffer))))

(defun lens-line (doc server lens)
  "The 0-based line of DOC's buffer LENS, SERVER's code lens, is of, or NIL when it says none."
  (let ((at (doc-byte doc server (tuatara.text:json-get lens "range" "start"))))
    (and at (tuatara.text:byte->line (tuatara.buffer:buffer-text (lsp-doc-buffer doc)) at))))

(defun lenses-resolved-p (server)
  "True when SERVER gives what its code lenses say only when asked (codeLens/resolve)."
  (tuatara.text:json-true-p
   (tuatara.text:json-get (lsp-server-capabilities server) "codeLensProvider" "resolveProvider")))

(defvar *lsp-lens-resolve-tries* 3
  "How many times what a code lens says is asked for (codeLens/resolve) when
the server fails to answer or gives no answer in time.")

(defun resolve-lenses (buffer doc server lenses lines)
  "Ask SERVER what those of LENSES, its code lenses of DOC, on LINES (FIRST
. LAST, LAST the line after) say that it gave without a title
\(codeLens/resolve), each while it is not being asked already; shown all
together in BUFFER once the last answer comes while the text is still as
asked of. One the server failed to answer is asked again when the lines are
next shown, *LSP-LENS-RESOLVE-TRIES* times in all."
  (let ((asked (or (tuatara.buffer:buffer-local buffer 'lsp-lenses-asked)
                   (setf (tuatara.buffer:buffer-local buffer 'lsp-lenses-asked) (make-hash-table :test 'eq))))
        (version (lsp-doc-version doc))
        (resolved nil))
    ;; each lens's tries so far, negative while one is under way
    (let* ((wanted (remove-if-not (lambda (lens)
                                    (let ((tries (gethash lens asked 0)))
                                      (unless (or (lens-title lens) (minusp tries)
                                                  (>= tries *lsp-lens-resolve-tries*))
                                        (let ((line (lens-line doc server lens)))
                                          (and line (<= (car lines) line) (< line (cdr lines)))))))
                                  lenses))
           (waiting (length wanted)))
      (dolist (lens wanted)
        (setf (gethash lens asked) (- (1+ (gethash lens asked 0))))
        (lsp-request server "codeLens/resolve" lens
                     (lambda (answer error)
                       (setf (gethash lens asked) (abs (gethash lens asked)))
                       (if (and (null error) (hash-table-p answer) (doc-unchanged-p doc version))
                           (setf (gethash "command" lens) (tuatara.text:json-get answer "command")
                                 resolved t)
                           ;; asked again once the lines are shown anew
                           (setf (tuatara.buffer:buffer-local buffer 'lsp-lens-lines) nil))
                       (when (and (zerop (decf waiting)) resolved (doc-unchanged-p doc version))
                         (show-lenses buffer doc server lenses)))
                     :seconds *lsp-wait*)))))

(defun resolve-shown-lenses (buffer shown)
  "Have what the code lenses of BUFFER on the lines SHOWN, (FIRST . LAST),
and *LSP-HINT-MARGIN* lines either side say asked for (RESOLVE-LENSES), when
their server gives it only when asked and those lines were not asked of
already."
  (let ((lenses (tuatara.buffer:buffer-local buffer 'lsp-lenses))
        (asked (tuatara.buffer:buffer-local buffer 'lsp-lens-lines))
        (doc (find-if-not #'lsp-doc-shadow (buffer-docs buffer))))
    (when (and shown doc lenses (lenses-resolved-p (car lenses))
               (not (and asked (<= (car asked) (car shown)) (<= (cdr shown) (cdr asked)))))
      (let ((lines (cons (max 0 (- (car shown) *lsp-hint-margin*)) (+ (cdr shown) *lsp-hint-margin*))))
        (setf (tuatara.buffer:buffer-local buffer 'lsp-lens-lines) lines)
        (resolve-lenses buffer doc (car lenses) (cdr lenses) lines)))))

(defun lsp-ask-lenses (doc)
  "Ask DOC's server that gives code lenses for those of its whole text, shown
once they come while the text is still as asked of; for those it gives
without their title on the lines the windows show, what they say asked for
\(RESOLVE-SHOWN-LENSES)."
  (let ((server (and *lsp-code-lenses* (null (lsp-doc-shadow doc))
                     (doc-server-offers doc "codeLensProvider" "code-lens"))))
    (when server
      (let ((buffer (lsp-doc-buffer doc))
            (version (lsp-doc-version doc)))
        (lsp-ask-anew doc :lenses server "textDocument/codeLens"
                      (tuatara.text:json-object "textDocument" (doc-identifier doc))
                      (lambda (result error)
                        (when (and (null error) (doc-unchanged-p doc version))
                          (let* ((all (remove-if-not #'hash-table-p (tuatara.text:json-items result)))
                                 (lenses (coerce (subseq all 0 (min (length all) *lsp-most-hints*)) 'list))
                                 (window (and *editor* (find buffer (editor-windows *editor*)
                                                            :key #'tuatara.display:window-buffer))))
                            (setf (tuatara.buffer:buffer-local buffer 'lsp-lens-lines) nil
                                  (tuatara.buffer:buffer-local buffer 'lsp-lenses-asked) nil)
                            (show-lenses buffer doc server lenses)
                            (when window
                              (resolve-shown-lenses buffer (shown-lines *editor* buffer window
                                                                        (tuatara.display:window-rows window))))))))))))

(defun forget-lenses (buffer)
  "BUFFER's code lenses let go."
  (setf (tuatara.buffer:buffer-local buffer 'lsp-hints)
        (remove-if #'consp (tuatara.buffer:buffer-local buffer 'lsp-hints) :key #'car)
        (tuatara.buffer:buffer-local buffer 'lsp-lenses) nil
        (tuatara.buffer:buffer-local buffer 'lsp-lens-lines) nil
        (tuatara.buffer:buffer-local buffer 'lsp-lenses-asked) nil))

(define-command lsp-code-lenses-mode ()
  "Show what language servers say of lines (3 references, Run test) faint at
each line's end, or no longer; \\[lsp-code-lens] runs one."
  (toggle-server-showing '*lsp-code-lenses* #'lsp-ask-lenses #'forget-lenses "Code lenses"))

(define-command lsp-code-lens ()
  "Run what a language server says of the current line (a code lens: Run
test, 3 references), chosen when there are several, as the server's own
command."
  (let* ((buffer (current-buffer))
         (shown (tuatara.buffer:buffer-local buffer 'lsp-lenses))
         (server (car shown))
         (rope (tuatara.buffer:buffer-text buffer))
         (line (tuatara.text:byte->line rope (point)))
         (doc (find-if-not #'lsp-doc-shadow (buffer-docs buffer)))
         (here (and doc server
                    (remove-if-not (lambda (lens) (and (lens-title lens) (eql line (lens-line doc server lens))))
                                   (cdr shown)))))
    (cond ((null shown) (message "No code lenses shown here (~a)" (substitute-command-keys "\\[lsp-code-lenses-mode]")))
          ((null here) (message "No code lens on this line"))
          (t (let ((lens (if (rest here)
                             (completing-read-value "Run: "
                                                    (mapcar (lambda (l) (make-candidate (lens-title l) :value l)) here)
                                                    :require-match t :keep-order t)
                             (first here))))
               (when (hash-table-p lens)
                 (lsp-do-action server (tuatara.text:json-get lens "command"))))))))
