;;;; src/command/editor.lisp --- The editor's state, the current buffer, and messages in the echo area.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The command loop is in parts: the panel under the prompt (panel.lisp),
;;;; reading keys and running commands (dispatch.lisp), painting frames through
;;;; the frontend (redisplay.lisp), the minibuffer (minibuffer.lisp), and
;;;; making and running the editor (session.lisp); windows side by side and one
;;;; above another are in windows.lisp.

(in-package #:tuatara.command)

(define-record (client (:constructor make-client (&key frontend directory environment (number 1)
                                                       (remote (and frontend t))))
                      (:copier nil))
  "Someone working with the editor on screens of their own (clients.lisp):
in an editor of its own, the one who started it, on its own screen
\(FRONTEND NIL, REMOTE false); in an editor running in the background, each
program that attached to show it (REMOTE true), painting what the remote
frontend FRONTEND sends, let go of once it ends.
NUMBER tells it apart from the others; DIRECTORY is the directory it was
started in, and ENVIRONMENT its variables laid over the editor's for the
programs started from its frames, each (NAME . VALUE); BEGAN, when it
began, and ENDED, when it ended or NIL, in universal time. STARTUP-BUFFERS
are the buffers of the files it was started with, in order: those a program
running it as its editor waits on. EXIT-CODE and EXIT-WORDS are the code it
ends with and what it says then; ENDINGS, functions of it called as it ends,
before those are given. An ended client keeps only what names its directory:
its number, directory and times."
  (frontend nil
            :documentation "The remote frontend CLIENT paints, or NIL for the
editor's own screen or once CLIENT has ended.")
  (remote nil :read-only t :documentation "True when CLIENT shows an editor running in the
background, from a program of its own.")
  (number 1 :type fixnum :read-only t :documentation "The number telling CLIENT apart from the editor's other clients.")
  (directory nil :documentation "The directory CLIENT was started in, or NIL.")
  (environment '()
               :documentation "CLIENT's variables laid over the editor's for the programs
started from its frames, each (NAME . VALUE).")
  (began (get-universal-time) :documentation "When CLIENT began, in universal time.")
  (ended nil :documentation "When CLIENT ended, in universal time, or NIL while it works.")
  (startup-buffers '()
                   :documentation "The buffers of the files CLIENT was started with: those
a program running it as its editor waits on.")
  (exit-code 0 :documentation "The code CLIENT ends with: 0, or 1 for a program
running it as its editor to give up.")
  (exit-words nil :documentation "What CLIENT says as it ends, or NIL.")
  (endings '() :documentation "Functions of CLIENT called as it ends, newest first."))

(setf (documentation 'make-client 'function)
      "A CLIENT on FRONTEND (NIL: the editor's own screen), started in DIRECTORY; REMOTE for one that showed an
editor running in the background, by default when it has a FRONTEND.")

(define-record (frame (:constructor make-frame (frontend)) (:copier nil))
  "One screen of the editor. FRONTEND paints it and reads its events. WINDOW
is its selected window, the one commands act on, in a layout of one or more.
MINIBUFFER-PROMPT, MINIBUFFER-TEXT and MINIBUFFER-ACTIVE describe its echo
area. MESSAGE is transient text shown there. PENDING are the key specs of an
unfinished key sequence typed in it. UNREAD holds events read while the
editor waited on work of its own, to be handled next, in order. DEFERRED
holds the window closed or files dropped while a command was asking for
something, to be handled once it is done. LAST-COMMAND, REPEATABLE and
REPEAT-MAP are what the next command typed in it follows on from.
KEYS-RECORDING and KEYS-PLAYING are the keys being recorded in it and played
into it. PAINTED is what it showed when it was last painted while another
frame was worked in, for PAINT-OTHER-FRAMES, or NIL to be painted again.
HELD is, while its question waits for what is done in another frame to run
inside it (RUN-INSIDE-QUESTION), the variables of that question, their
values there and the buffer it is about, as (VARIABLES VALUES BUFFER). It is
NIL otherwise. DELETED is true once the frame has left the editor. NAME is
what it is called, F1 for the first: the mode line shows it while frames take
turns on one display, as in a terminal. CLIENT is the one who works in it
\(CLIENT), whose buffers it shows first."
  (frontend nil :read-only t :documentation "The frontend that paints FRAME and reads its events.")
  (name "F1" :type string :documentation "What FRAME is called: F1, F2, the smallest number no other frame has.")
  (window nil :documentation "The selected window of FRAME, the one commands act on.")
  (minibuffer-prompt nil :documentation "The prompt shown while the minibuffer of FRAME is active.")
  (minibuffer-text nil :documentation "The text typed so far in the minibuffer of FRAME.")
  (minibuffer-point 0 :type fixnum :documentation "The cursor position within the minibuffer text of FRAME.")
  (minibuffer-count nil
                    :documentation "While a question that is no completing read counts something, what FRAME shows
before its prompt (3/12, or \"\" for its place kept empty); else NIL.")
  (minibuffer-active nil :documentation "True while the minibuffer of FRAME reads input.")
  (completion nil :documentation "The COMPLETION state while a completing read is active in FRAME, or NIL.")
  (popup nil :documentation "The list of completions shown beside the cursor in FRAME (COMPLETE-IN-TEXT), or NIL.")
  (note nil
        :documentation "The box of words shown under the cursor in FRAME (SHOW-NOTE), as (BUFFER POSITION PARAGRAPHS),
or NIL.")
  (minibuffer-completer nil
                        :documentation "While the minibuffer of FRAME reads, the function that completes the word at its
cursor, or NIL.")
  (height 24 :type fixnum :documentation "Rows of the whole display of FRAME.")
  (width 80 :type fixnum :documentation "Columns of the whole display of FRAME.")
  (message nil :documentation "Transient text shown in the echo area of FRAME, or NIL.")
  (pending '() :documentation "Key specs of an unfinished key sequence typed in FRAME.")
  (prefix-argument nil :documentation "The numeric prefix argument for the current command, given in FRAME, or NIL.")
  (unread '()
          :documentation "Events of FRAME read while the editor waited on work of its own, to be handled next, in
order.")
  (collecting-prefix nil
                     :documentation "True while \\[universal-argument] collects digits for the prefix argument in FRAME:
:DIGITS once a digit has come since the last \\[universal-argument].")
  (this-key nil :documentation "The key that completed the current command's key sequence in FRAME.")
  (transient-map nil
                 :documentation "A keymap consulted before all others in FRAME while its commands keep it
\(SET-TRANSIENT-MAP), or NIL.")
  (transient-keep nil
                  :documentation "The function of a command that says whether the transient map of FRAME stays after
it.")
  (transient-exit nil :documentation "The function called when the transient map of FRAME goes, or NIL.")
  (transient-fresh nil
                   :documentation "True while the command that set the transient map of FRAME runs: that one is not
asked whether it keeps it.")
  (menu nil
        :documentation "The menu open now in FRAME, the arguments it has on and those it was opened with, as (MENU
ARGUMENTS OPENED), or NIL (OPEN-MENU).")
  (title-shown nil :documentation "The buffer name and *GUI-TITLE* the title of FRAME was last made from, as a list.")
  (deferred '()
            :documentation "Events that ended a command's question in FRAME, the window closed or files dropped, to be
handled once it is done, oldest first.")
  (last-command nil :documentation "The command run before the current one in FRAME.")
  (repeatable nil
              :documentation "The last command run in FRAME other than repeat, the prefix argument it had and the key
that ran it, as (COMMAND ARGUMENT . KEY).")
  (repeat-map nil
              :documentation "The keymap of keys that repeat the command just run in FRAME by themselves
\(DEFINE-REPEAT-KEYS), while it is in force, or NIL.")
  (keys-recording nil :documentation "The keys being recorded in FRAME, a RECORDING, or NIL while none are.")
  (keys-playing nil
                :documentation "While recorded keys are played into FRAME, the PLAYING they are read from: READ-EVENT
gives its keys before any typed, and nothing is drawn; NIL otherwise.")
  (painted nil
           :documentation "What FRAME showed when it was last painted while another
frame was worked in, or NIL (PAINT-OTHER-FRAMES).")
  (held nil :documentation "While FRAME's question waits for another frame, its variables, their values and its buffer,
as (VARIABLES VALUES BUFFER).")
  (deleted nil :documentation "True once FRAME has left its editor (REMOVE-FRAME).")
  (client nil :documentation "The client who works in FRAME (CLIENT)."))

(setf (documentation 'make-frame 'function) "A FRAME painted through FRONTEND, with no window yet.")

(define-record (editor (:constructor %make-editor (frontend &aux (frame (make-frame frontend)) (frames (list frame))))
                      (:copier nil))
  "The editor state. FRAMES are its screens, a list replaced and never changed
in place, as other threads read it. FRAME is the one being worked in, between
commands the one you last typed in. EDITOR-WINDOW, EDITOR-MESSAGE,
EDITOR-LAST-COMMAND and the other names of a screen's state read and set that
frame's. SELECTIONS counts the times a window was selected. KILL-RING holds
killed text, newest first. STATE-KEYMAP is the modal state's map consulted
before the others (the modal layer's). OTHERS-PAINTED is when the frames not
worked in were last painted (PAINT-OTHER-FRAMES), and SETTINGS-LOOKED when the
settings were last looked at for them (LOOK-AT-SETTINGS), in internal time.
CLIENTS are those who work with it (CLIENT), the one who started it first, a
list replaced and never changed in place. IDLE-FRONTEND is, in an editor
running in the background, the frontend with no screen its frame shows on
while no client shows it, else NIL. RUNNING is false once the editor should
exit."
  (frames '() :type list :documentation "The frames of EDITOR, its screens.")
  (frame nil :type frame :documentation "The current frame of EDITOR, the screen being worked in.")
  (clients '() :type list :documentation "Those who work with EDITOR (CLIENT), the one who started it first.")
  (idle-frontend nil
                 :documentation "The frontend with no screen EDITOR's frame shows on while no client shows it, or NIL.")
  (selections 0 :type fixnum :documentation "How many times a window of EDITOR has been
selected: the clock of WINDOW-USE-TIME.")
  (others-painted 0 :documentation "When the frames of EDITOR not worked in were
last painted, in internal time.")
  (settings-looked 0 :documentation "When the settings were last looked at for
the frames of EDITOR not worked in, in internal time.")
  (global-keymap (make-keymap :name "global") :documentation "EDITOR's global keymap, the last one consulted.")
  (state-keymap nil :type (or null keymap) :documentation "The modal state keymap consulted first, or NIL.")
  (fallback-keymap nil :type (or null keymap) :documentation "A keymap consulted after the buffer's own
and before the global one (the modal support keys), or NIL.")
  (minibuffer-keymap (make-keymap :name "minibuffer")
                     :documentation "The keymap active while the minibuffer reads input.")
  (key-hints t :documentation "True to show the bindings under a pending prefix key in the panel.")
  (kill-ring '() :documentation "Kills, newest first: strings, or ropes for kills
too large for the system clipboard.")
  (this-command nil :documentation "The command being run.")
  (running t :documentation "False once the editor should exit.")
  (self-insert-command nil :documentation "Command run for an unbound plain
character, or NIL to treat it as undefined.")
  (mode-line-function nil :documentation "Function of (EDITOR WINDOW) returning
the text of WINDOW's mode line and the spans of it in faces of their own, or NIL
for the default.")
  (cursor-shape-function nil :documentation "Function of (EDITOR) returning the
cursor's shape and colour, as *CURSOR-SHAPE* and *CURSOR-COLOUR* take them, NIL
for those; and as a third value, true while text is typed. NIL for
*CURSOR-SHAPE* and *CURSOR-COLOUR* always.")
  (post-command-functions '() :documentation "Functions called with (EDITOR) after every command.")
  (pre-command-functions '() :documentation "Functions called with (EDITOR
COMMAND) before every command runs."))

(defmacro define-frame-accessor (name slot documentation &key read-only)
  "Define NAME, a function of an editor, as SLOT of the editor's current frame,
documented by DOCUMENTATION; SETF, INCF and PUSH on it change that frame's,
unless READ-ONLY."
  (let ((reader (symbolicate 'frame- slot)))
    `(progn
       (declaim (inline ,name ,@(unless read-only `((setf ,name)))))
       (defun ,name (editor)
         ,documentation
         (,reader (editor-frame editor)))
       ,@(unless read-only
           `((defun (setf ,name) (value editor)
               ,documentation
               (setf (,reader (editor-frame editor)) value)))))))

(define-frame-accessor editor-frontend frontend
  "The frontend of EDITOR's current frame." :read-only t)
(define-frame-accessor editor-window window
  "The selected window of EDITOR's current frame, the one commands act on.")
(define-frame-accessor editor-minibuffer-prompt minibuffer-prompt
  "The prompt shown while the minibuffer of EDITOR's current frame is active.")
(define-frame-accessor editor-minibuffer-text minibuffer-text
  "The text typed so far in the minibuffer of EDITOR's current frame.")
(define-frame-accessor editor-minibuffer-point minibuffer-point
  "The cursor position within the minibuffer text of EDITOR's current frame.")
(define-frame-accessor editor-minibuffer-count minibuffer-count
  "While a question that is no completing read counts something, what it shows
before its prompt in EDITOR's current frame (3/12, or \"\" for its place kept
empty); else NIL.")
(define-frame-accessor editor-minibuffer-active minibuffer-active
  "True while the minibuffer of EDITOR's current frame reads input.")
(define-frame-accessor editor-completion completion
  "The COMPLETION state while a completing read is active in EDITOR's current frame, or NIL.")
(define-frame-accessor editor-popup popup
  "The list of completions shown beside the cursor in EDITOR's current frame (COMPLETE-IN-TEXT), or NIL.")
(define-frame-accessor editor-note note
  "The box of words shown under the cursor in EDITOR's current frame (SHOW-NOTE),
as (BUFFER POSITION PARAGRAPHS), or NIL.")
(define-frame-accessor editor-minibuffer-completer minibuffer-completer
  "While the minibuffer of EDITOR's current frame reads, the function
\\<minibuffer>\\[minibuffer-complete] calls to complete the word at its cursor, or NIL.")
(define-frame-accessor editor-height height
  "Rows of the whole display of EDITOR's current frame.")
(define-frame-accessor editor-width width
  "Columns of the whole display of EDITOR's current frame.")
(define-frame-accessor editor-message message
  "Transient text shown in the echo area of EDITOR's current frame, or NIL.")
(define-frame-accessor editor-pending pending
  "Key specs of an unfinished key sequence typed in EDITOR's current frame.")
(define-frame-accessor editor-prefix-argument prefix-argument
  "The numeric prefix argument for the current command, given in EDITOR's current frame, or NIL.")
(define-frame-accessor editor-unread unread
  "Events of EDITOR's current frame read while the editor waited on work of its
own, to be handled next, in order.")
(define-frame-accessor editor-collecting-prefix collecting-prefix
  "True while \\[universal-argument] collects digits for the prefix argument in EDITOR's current frame:
:DIGITS once a digit has come since the last \\[universal-argument].")
(define-frame-accessor editor-this-key this-key
  "The key that completed the current command's key sequence in EDITOR's current frame.")
(define-frame-accessor editor-transient-map transient-map
  "A keymap consulted before all others in EDITOR's current frame while its
commands keep it (SET-TRANSIENT-MAP), or NIL.")
(define-frame-accessor editor-transient-keep transient-keep
  "The function of a command that says whether the transient map of EDITOR's current frame stays after it.")
(define-frame-accessor editor-transient-exit transient-exit
  "The function called when the transient map of EDITOR's current frame goes, or NIL.")
(define-frame-accessor editor-transient-fresh transient-fresh
  "True while the command that set the transient map of EDITOR's current frame
runs: that one is not asked whether it keeps it.")
(define-frame-accessor editor-menu menu
  "The menu open now in EDITOR's current frame, the arguments it has on and
those it was opened with, as (MENU ARGUMENTS OPENED), or NIL (OPEN-MENU).")
(define-frame-accessor editor-title-shown title-shown
  "The buffer name and *GUI-TITLE* the title of EDITOR's current frame was last made from, as a list.")
(define-frame-accessor editor-deferred deferred
  "Events that ended a command's question in EDITOR's current frame, the window
closed or files dropped, to be handled once it is done, oldest first.")
(define-frame-accessor editor-last-command last-command
  "The command run before the current one in EDITOR's current frame.")
(define-frame-accessor editor-repeatable repeatable
  "The last command run in EDITOR's current frame other than repeat, the prefix
argument it had and the key that ran it, as (COMMAND ARGUMENT . KEY).")
(define-frame-accessor editor-repeat-map repeat-map
  "The keymap of keys that repeat the command just run in EDITOR's current
frame by themselves (DEFINE-REPEAT-KEYS), while it is in force, or NIL.")
(define-frame-accessor editor-keys-recording keys-recording
  "The keys being recorded in EDITOR's current frame, a RECORDING, or NIL while none are.")
(define-frame-accessor editor-keys-playing keys-playing
  "While recorded keys are played into EDITOR's current frame, the PLAYING they
are read from: READ-EVENT gives its keys before any typed, and nothing is
drawn; NIL otherwise.")

(define-record (later (:constructor later (start)) (:copier nil))
  "An answer that is to come, as a feature that asks a program gives it: START,
a function of one argument, THEN, asks for it, and calls THEN with the
answer's values on the editor's thread once it comes, or never."
  (start nil :type function :read-only t :documentation "The function that asks for a LATER answer."))

(setf (documentation 'later 'function) "An answer to come that START asks for (see the structure LATER)."
      (documentation 'later-p 'function) "True when OBJECT is an answer to come, a LATER.")

(defun take-answer (buffer then answer &rest more)
  "Call THEN with ANSWER and MORE, the values of an answer given now; or when
ANSWER is a LATER, with the values it gives once they come, but only while
BUFFER is still the current buffer, its text and point as they were when it
was asked: an answer to a question no longer standing is let go."
  (if (later-p answer)
      (let ((version (tuatara.buffer:buffer-version buffer))
            (point (tuatara.buffer:buffer-point buffer)))
        (funcall (later-start answer)
                 (lambda (&rest values)
                   (when (and (eq buffer (current-buffer))
                              (eq version (tuatara.buffer:buffer-version buffer))
                              (eql point (tuatara.buffer:buffer-point buffer)))
                     (apply then values)))))
      (apply then answer more)))

(defun wake-editor (editor)
  "Wake EDITOR, from any thread, to show what work in the background has for
it: its frames share one source of events, which is woken once, through the
frame being worked in; an error waking it is ignored."
  (tuatara.runtime:ignoring-errors-reported ("waking the editor")
    (tuatara.frontend:frontend-wake (frame-frontend (editor-frame editor)))))

(defvar *editor* nil
  "The editor whose command is running.")

(defvar *display-generation* 0
  "A count moved on whenever what every frame may show changes beside the text
of its buffers: work taken in from the background, a way of showing text
switched, a setting changed (SETTINGS-MOVED-P), a job's work or a message
taken in. A frame not worked in is painted again when it moved since it was
\(PAINT-OTHER-FRAMES).")

(defvar *background-work* nil
  "True when a tick function said at the last tick that work of its own runs
in the background. A frontend for tests ends its session only once nothing
runs.")

(defun prefix-argument ()
  "The prefix argument given the command running now, a number: 4 for
\\[universal-argument] alone, 16 for it twice, the digits typed after it, -1 for a
lone minus; NIL for none, or with no editor."
  (and *editor* (editor-prefix-argument *editor*)))

(define-record (completion (:constructor make-completion
                                        (table &key annotate require-match history (separator #\/) (filter t)
                                                    select reselect preview source default keep-order none taken
                                                    (prompt "")))
                          (:copier nil))
  "State of a completing read. TABLE is a list of CANDIDATE or a function of
the input returning one. ANNOTATE is a function of a candidate returning an
annotation string or NIL. REQUIRE-MATCH true means
\\<minibuffer>\\[minibuffer-finish] must pick a candidate. HISTORY is a list of
previous inputs, most recent first (or a function of no arguments returning
one, for a list still growing). SEPARATOR, for a TABLE that is a function, is
the character after whose last occurrence the input is matched and completed
\(/ for file names, a comma for a list of words). FILTER false shows a function
TABLE's candidates as it gives them, matched and ordered already (a search of
files as you type).
SELECT, when given, is a function of the candidates first shown (a vector) and
the input. It returns the index of the one selected at first, or :INPUT for
the line typed itself. With RESELECT, it is called for those shown after each
change of the input too. Where the read takes what is typed (REQUIRE-MATCH
false), the line typed can be selected as a candidate is (INDEX -1). It is
when nothing matches, or SELECT says so, and one step above the first
candidate. PREVIEW is a function called with the selected candidate, or NIL
when none is, each time the selection changes. PREVIEWED is the candidate it
was last called with.
SOURCE, for candidates that arrive from work in the background, is a function
the minibuffer calls on the editor's thread whenever it is woken while the
read is open. It returns three values. The first is true when the candidates
may have changed (STALE is set then, and they are worked out again, the
selected one kept). The second is true while more may come (WORKING). The
third is how many things it has listed so far, or NIL for nothing to tell
\(LISTED), which the prompt tells while more come (SHOWN-PROMPT).
DEFAULT, when not NIL, is what \\[minibuffer-finish] takes while nothing is
typed, its candidate offered first then, and named in the prompt. KEEP-ORDER
true keeps the candidates matching in the order TABLE gives them, not sorted
best first. NONE is words the prompt says once all have come and there are
none. TAKEN is a list of names the read takes when they are typed whole,
though they are not among its candidates. PROMPT is the question as asked,
before those words are added to it (or a function of no arguments giving it).
DONE-PENDING is true when \\[minibuffer-finish] came while nothing could be
chosen yet and more was coming, to be done once it has come. MOVED is true
once a candidate was selected by hand for the input typed, which stays
selected as more come (else the best match is). TALLEST is the most rows of
candidates shown so far, the panel keeping that height while the question is
open, so that the windows above do not jump as typing narrows the list.
CANDIDATES holds the matches for the current input and INDEX the selected
one, -1 for the line typed. NARROWED is the last filtering, (ALL QUERY
MATCHES), so that a query typed on from that one is matched against MATCHES
alone."
  (table nil :read-only t :documentation "The candidate list or function of a COMPLETION.")
  (annotate nil :read-only t :documentation "The annotation function of a COMPLETION, or NIL.")
  (require-match nil :read-only t :documentation "True when the completing read only accepts a candidate.")
  (history '() :read-only t :documentation "Previous inputs of a COMPLETION, most recent first.")
  (separator #\/ :read-only t :documentation "The character after which a COMPLETION with a
function table matches and completes the input.")
  (filter t :read-only t :documentation "False when a COMPLETION shows its function
table's candidates as they come.")
  (select nil :read-only t :documentation "The function naming the candidate a COMPLETION
selects at first, or NIL.")
  (reselect nil :read-only t :documentation "True when a COMPLETION's SELECT names the
candidate selected after each change of the input too.")
  (preview nil :read-only t :documentation "The function a COMPLETION calls with the
selected candidate as the selection changes, or NIL.")
  (source nil :read-only t :documentation "The function a COMPLETION calls to take in
candidates found in the background, or NIL.")
  (default nil :read-only t :documentation "What a COMPLETION takes while nothing is typed, or NIL.")
  (keep-order nil :read-only t :documentation "True when a COMPLETION keeps its candidates in the order given.")
  (none nil :read-only t :documentation "The words a COMPLETION's prompt says when all have
come and there are none, or NIL.")
  (taken '() :read-only t :documentation "The names a COMPLETION takes when typed whole,
though they are not among its candidates.")
  (prompt "" :type (or string function) :read-only t :documentation "The question a COMPLETION asks, as asked.")
  (working nil :documentation "True while a COMPLETION's SOURCE said more candidates may come.")
  (listed nil :documentation "How many things a COMPLETION's SOURCE said it
has listed so far, or NIL.")
  (stale nil :documentation "True when a COMPLETION's candidates are to be
worked out again, its input unchanged.")
  (done-pending nil :documentation "True when a choice was asked of a
COMPLETION before anything could be chosen, more coming.")
  (moved nil :documentation "True when a COMPLETION's selection was moved by
hand since the input last changed.")
  (tallest 0 :documentation "The most rows of candidates a COMPLETION's
panel has shown, which it keeps while the question is open.")
  (tall nil :documentation "True while a COMPLETION's candidates take as many
rows as the frame has room for (\\[minibuffer-toggle-tall]).")
  (previewed :none :documentation "The candidate a COMPLETION's preview function
was last called with, or :NONE.")
  (candidates #() :type simple-vector :documentation "The candidates matching the current input.")
  (index 0 :type fixnum :documentation "The index of the selected candidate, -1 when it is the line typed.")
  (input-seen nil :documentation "The input the candidates were computed for.")
  (narrowed nil :documentation "The last filtering of a COMPLETION: (ALL QUERY MATCHES)."))

(setf (documentation 'make-completion 'function) "Return the state of a completing read over TABLE.")

(define-record (popup (:constructor %make-popup
                                   (target start end table annotate document constituent &optional keep-order auto))
                     (:copier nil))
  "Completion in the text (COMPLETE-IN-TEXT). TARGET is the buffer the word is
in, or :MINIBUFFER. In a buffer, START and END are anchors at the word's ends,
END moving on with what is typed at it. In the minibuffer, START is the index
of the word's first character and END the text after the word, which stays as
it is. TABLE is the candidates, a list of CANDIDATE. ANNOTATE is a function of
a candidate returning what is said beside it, or NIL. DOCUMENT is one
returning its documentation, shown in the box beside the list, or NIL.
CONSTITUENT is a function of a character, true for those a word is made of.
KEEP-ORDER true lists the matches in TABLE's order, not the best first. AUTO
is true for a list that opened by itself as the word was typed, with none
chosen at first. CANDIDATES holds the matches of the word, INDEX the chosen
one (-1 for none) and SCROLL the first shown. SEPARATOR is true once
\\<popup>\\[popup-insert-separator] put a space in the word, which then goes on
past spaces. INPUT-SEEN is the word the candidates were matched against. SHOWN
is the documentation last shown, as (CANDIDATE ROOM LINES WIDTH), and
LIST-KEPT the list last drawn, with what it was drawn from."
  (target nil :read-only t :documentation "The buffer the completed word is in, or :MINIBUFFER.")
  (start nil :read-only t :documentation "Where the completed word starts: an anchor in a
buffer, an index in the minibuffer.")
  (end nil :read-only t :documentation "Where the completed word ends: an anchor in a buffer,
the text after it in the minibuffer.")
  (table '() :type list :read-only t :documentation "Every candidate of a completion in the text.")
  (annotate nil :read-only t :documentation "The function giving what is said beside a candidate, or NIL.")
  (document nil :read-only t :documentation "The function giving a candidate's documentation, or NIL.")
  (constituent nil :read-only t :documentation "The function saying which characters a word is made of.")
  (keep-order nil :read-only t :documentation "True when the matches are listed in the table's order.")
  (auto nil :read-only t :documentation "True for a list that opened by itself as the word was typed.")
  (candidates #() :type simple-vector :documentation "The candidates matching the word, best first.")
  (index 0 :type fixnum :documentation "The index of the chosen candidate, -1 while none is.")
  (scroll 0 :type fixnum :documentation "The index of the first candidate shown.")
  (separator nil :documentation "True once \\<popup>\\[popup-insert-separator] put
a space in the word.")
  (input-seen nil :documentation "The word the candidates were matched against.")
  (shown nil :documentation "The documentation last shown, as (CANDIDATE ROOM LINES
WIDTH): its lines broken for ROOM columns, the box WIDTH wide
(DOCUMENTATION-LINES).")
  (list-kept nil :documentation "The list last drawn and what it was drawn from, as
\(FROM ROWS WIDTH) (POPUP-LIST-BOX), or NIL."))

(defun resize (editor width height)
  "Record the display size and lay the windows out on the rows the panel leaves."
  (setf (editor-width editor) (max 1 width) (editor-height editor) (max 3 height))
  (fit-windows editor))

(defvar *current-buffer* nil
  "The buffer commands and the Lisp one types act on when it is not the selected
window's: made so by SET-BUFFER until the command ends, or by
WITH-CURRENT-BUFFER for its body.")

(defun current-buffer ()
  "Return the current buffer: the one commands act on, the selected window's
unless SET-BUFFER or WITH-CURRENT-BUFFER made another current."
  (or *current-buffer* (selected-window-buffer *editor*)))

(defun abbreviated-path (path)
  "PATH, a file's or a directory's (a pathname or a native name), as a
string with the home directory written ~, as messages and the mode line show
it, whichever name of the home directory it is by
\(TUATARA.IO:HOME-DIRECTORY-NAMES): ~/notes.org, ~/ for the home itself."
  (let ((name (tuatara.io:native path)))
    (dolist (home (tuatara.io:home-directory-names) name)
      (cond ((string= name home) (return "~/"))
            ((and (> (length name) (length home)) (string= home name :end2 (length home)))
             (return (concatenate 'string "~/" (subseq name (length home)))))))))

(defun buffer-given-directory (buffer)
  "The directory BUFFER was given to stand in, whatever file it visits: the
one a list of a directory lists (\\[rimu]), where a compile or a terminal's
program runs; NIL for a buffer that stands in its file's (BUFFER-DIRECTORY)."
  (tuatara.buffer:buffer-local buffer :directory))

(defun (setf buffer-given-directory) (directory buffer)
  "Have BUFFER stand in DIRECTORY, a native name (BUFFER-GIVEN-DIRECTORY); NIL
for its file's."
  (setf (tuatara.buffer:buffer-local buffer :directory) directory))

(defun buffer-directory (buffer)
  "The native name, ending in /, of the directory BUFFER stands in: the one it
lists, for a list of a directory (\\[rimu]); else that of its file; else the one
*DEFAULT-DIRECTORY* names. Files asked for from BUFFER are looked for there."
  (let ((listed (buffer-given-directory buffer))
        (file (tuatara.buffer:buffer-file buffer)))
    (cond (listed)
          (file (tuatara.io:parent-path file))
          (t (tuatara.io:native (tuatara.io:default-directory))))))

(defun selected-window-buffer (&optional (editor *editor*))
  "The buffer EDITOR's selected window shows, whatever buffer is current; NIL
when there is no editor or it has no window yet."
  (let ((window (and editor (editor-window editor))))
    (and window (tuatara.display:window-buffer window))))

(defun current-window ()
  "The selected window, the one commands act on."
  (editor-window *editor*))

(define-setting tuatara-user:*use-system-clipboard* t
  "Whether what is killed or copied goes on the system clipboard as well.
Other programs paste it from there, and what another program copied there is
pasted since.
- t: the system clipboard is used (the standard).
- nil: killed text stays with the editor."
  :values boolean)

(define-setting *message-log-max* 1000
  "The lines the *Messages* buffer keeps of the messages shown.
The oldest go first. Raise it to look back further, at the cost of the
memory of those lines.
- a whole number, 0 or more: that many lines; 1000 as standard.
- nil: none are kept."
  :values (or (member nil) (integer 0 *)))

(defun message-repeats (line)
  "LINE, a line of *Messages*, as the message it shows and how many times:
\"Saved [3 times]\" is \"Saved\" three times."
  (let ((open (search " [" line :from-end t)))
    (if (and open (> (length line) (+ open 8)) (string= " times]" line :start2 (- (length line) 7)))
        (let ((count (parse-integer line :start (+ open 2) :end (- (length line) 7) :junk-allowed t)))
          (if count (values (subseq line 0 open) count) (values line 1)))
        (values line 1))))

(defgeneric host-messages-made (host buffer)
  (:documentation "Give BUFFER, a log just made (*Messages*, *Debug*, LOG-BUFFER), what more it
has (its major mode); on the core's own host, nothing.")
  (:method ((host tuatara.runtime:host) buffer)
    (declare (ignore buffer))
    nil))

(defun log-buffer (name)
  "The log called NAME (*Messages*, *Debug*), made when there is none:
read-only, shown where *Messages* is, given what HOST-MESSAGES-MADE gives it
\(its mode), and added to with ADD-TO-LOG."
  (or (tuatara.buffer:find-buffer name)
      (let ((b (tuatara.buffer:make-buffer name)))
        (setf (tuatara.buffer:buffer-read-only-p b) t
              (buffer-kind b) :messages)
        (host-messages-made tuatara.runtime:*host* b)
        b)))

(defun messages-buffer ()
  "The *Messages* buffer, made when there is none (LOG-BUFFER)."
  (log-buffer "*Messages*"))

(defun add-to-log (buffer max change)
  "Call CHANGE with BUFFER, a log such as *Messages*, its read-only lifted for
it, then keep BUFFER's last MAX lines, the oldest going first (all of them
when MAX is NIL). A log never counts as modified and has no undo; a window
showing its end goes on showing it."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (following (and *editor*
                         (remove-if-not (lambda (window) (= (tuatara.display:window-point window) size))
                                        (buffer-windows buffer)))))
    (unwind-protect
         (tuatara.buffer:with-read-only-lifted (buffer :after t)
           (funcall change buffer)
           (when max
             (let ((lines (1- (tuatara.buffer:buffer-line-count buffer))))
               (when (> lines max)
                 (tuatara.buffer:buffer-delete buffer 0 (tuatara.buffer:buffer-goto-line buffer (- lines max)))))))
      (tuatara.buffer:forget-history buffer)
      (tuatara.buffer:buffer-mark-saved buffer))
    (dolist (window following)
      (setf (tuatara.display:window-point window) (tuatara.buffer:buffer-size buffer)))))

(defvar *secret-messages* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The messages said while a secret was at hand (TUATARA.RUNTIME:*SECRET-AT-HAND*),
shown but never kept in *Messages* (LOG-MESSAGE).")

(defun log-message (text)
  "Add TEXT, a message just shown, to the *Messages* buffer (made when there is
none): a line of its own, or the last line counted up when it says the same
(\"Saved [2 times]\"). The buffer keeps *MESSAGE-LOG-MAX* lines (ADD-TO-LOG).
A message said while a secret is, or was, at hand is not kept: its words may
hold the secret."
  (when (and *message-log-max* (plusp (length text))
             (not tuatara.runtime:*secret-at-hand*) (not (gethash text *secret-messages*)))
    (add-to-log (messages-buffer) *message-log-max*
                (lambda (buffer)
                  (let* ((size (tuatara.buffer:buffer-size buffer))
                         (last-start (if (zerop size) 0 (tuatara.buffer:buffer-line-start buffer (1- size))))
                         (last (and (plusp size)
                                    (tuatara.buffer:buffer-string buffer :start last-start :end (1- size)))))
                    (multiple-value-bind (said count) (if last (message-repeats last) (values nil 0))
                      (if (and last (string= said text))
                          (tuatara.buffer:buffer-replace buffer last-start (1- size)
                                                         (format nil "~a [~d times]" text (1+ count)))
                          (tuatara.buffer:buffer-insert buffer size (format nil "~a~%" text)))))))))

(defvar *messages-kept* nil
  "While work runs on a thread away from the editor's: a list cell whose first
element gathers the messages it says, the newest first, for the editor to
show once the work is taken in; NIL, as on the editor's own thread, for
messages to be shown at once.")

(defvar *running-editor* nil
  "The editor whose session runs, and the thread it runs on, as a cons; NIL
when none does. Work on other threads finds it here.")

(defvar *messages-from-threads* '()
  "The messages said on other threads while an editor runs, the newest first,
each with the frame it is for (THREAD-FRAME), as (FRAME . TEXT), for the
editor's thread to show and keep (TAKE-IN-THREAD-MESSAGES).")

(defvar *actions-from-threads* '()
  "The functions given to DEFER-ACTION on other threads while an editor runs,
the newest first, each with the frame it is for (THREAD-FRAME), as (FRAME .
FUNCTION), for the editor's thread to run (TAKE-IN-THREAD-MESSAGES).")

(defvar *messages-from-threads-lock* (sb-thread:make-mutex :name "messages from threads")
  "Held while *MESSAGES-FROM-THREADS* or *ACTIONS-FROM-THREADS* is changed.")

(defun away-from-editor-p ()
  "True on a thread other than the one the running editor's session runs on."
  (let ((running *running-editor*))
    (and running (not (eq (cdr running) sb-thread:*current-thread*)))))

(defmethod tuatara.runtime:host-wake ((host editor-host))
  "Wake the running editor (WAKE-EDITOR) from another thread."
  (let ((running *running-editor*))
    (when (and running (not (eq (cdr running) sb-thread:*current-thread*)))
      (wake-editor (car running)))))

(defun thread-frame ()
  "The frame what is said or handed on from this thread is for: on the thread
of a job, the frame the job was begun in (TUATARA.RUNTIME:JOB-FRAME); NIL
elsewhere, for the frame you are in."
  (let ((job tuatara.runtime:*job*))
    (and job (tuatara.runtime:job-frame job))))

(defmacro hand-to-editor-thread (item place)
  "Push ITEM onto PLACE, *MESSAGES-FROM-THREADS* or *ACTIONS-FROM-THREADS*,
with the frame it is for (THREAD-FRAME), under *MESSAGES-FROM-THREADS-LOCK*,
and wake the running editor to take it in."
  `(let ((entry (cons (thread-frame) ,item)))
     (sb-thread:with-mutex (*messages-from-threads-lock*)
       (push entry ,place))
     (let ((running *running-editor*))
       (when running
         (wake-editor (car running))))))

(defun frame-or-current (editor frame)
  "FRAME while it is one of EDITOR's frames, else the frame EDITOR works in:
where what was meant for FRAME goes once FRAME has gone."
  (if (and frame (not (frame-deleted frame))) frame (editor-frame editor)))

(defun take-in-thread-messages (editor)
  "Show and keep in *Messages*, on EDITOR's thread, the messages said on other
threads since last time, in the order they were said, and have the functions
given to DEFER-ACTION there run as EDITOR's next commands, in that order too.
Each message shows in the frame it is for while that frame is open, else in
the frame you are in, but only in *Messages* while a question of one key
waits in that frame, the question staying in view; each function waits among the things to be done in
that frame likewise (FRAME-DEFERRED), to run with it the one worked in. True
when there was anything to take in."
  (when (or *messages-from-threads* *actions-from-threads*)
    (let (texts actions)
      (sb-thread:with-mutex (*messages-from-threads-lock*)
        (setf texts (shiftf *messages-from-threads* '())
              actions (shiftf *actions-from-threads* '())))
      (loop for (frame . text) in (reverse texts)
            do (let ((shown (frame-or-current editor frame)))
                 (unless (frame-key-question-p shown)
                   (setf (frame-message shown) text
                         (frame-painted shown) nil)))
               (tuatara.runtime:ignoring-errors-reported ("keeping a message in *Messages*") (log-message text)))
      (loop for (frame . action) in (reverse actions)
            do (let ((done-in (frame-or-current editor frame)))
                 (setf (frame-deferred done-in) (append (frame-deferred done-in) (list action)))))
      t)))

(defvar *command-notes* nil
  "While a command run from a key runs (RUN-COMMAND), a list cell whose car is
a property list of what is noted for that command alone: an answer given once
for all its edits, words to say with each of its later messages
\(SAY-WITH-COMMAND-MESSAGES). NIL when no command runs: a tick, work in the background,
a buffer edited from Lisp outside the command loop.")

(defun say-with-command-messages (control &rest arguments)
  "Say a formatted message, as MESSAGE does, and while a command runs have it
said too with each message the command says after it, joined to that one:
something the command did on the way that the person must see (a buffer
widened to show where it went), whatever the command goes on to say."
  (let ((text (apply #'message control arguments)))
    (when *command-notes*
      (setf (getf (car *command-notes*) :said-too) text))
    text))

(defun with-command-notes-said (text)
  "TEXT, a message, as the echo area shows it: followed by what the command
running had said too (SAY-WITH-COMMAND-MESSAGES), its first letter lower
case, when TEXT does not hold it already."
  (let ((also (and *command-notes* (getf (car *command-notes*) :said-too))))
    (if (and also (not (search also text)))
        (format nil "~a; ~a~a" text (string-downcase (subseq also 0 1)) (subseq also 1))
        text)))

(defun message (control &rest arguments)
  "Show a formatted message in the echo area until the next command, and keep
it in the *Messages* buffer (LOG-MESSAGE). The keys CONTROL names as
\\=\\[COMMAND] and the rest (SUBSTITUTE-COMMAND-KEYS) are those bound now, shown
in :help-key-binding. Said by work away from the editor's thread, while
*MESSAGES-KEPT* holds a cell, it is kept there to be shown later; said there
otherwise, it is handed to the editor's thread, which shows it as soon as it
is woken. What the command running said too (SAY-WITH-COMMAND-MESSAGES) is
shown after it. Said while a secret is at hand, it is shown but not kept.
Control and direction characters in it, from a name or text from elsewhere,
are written out (ESCAPE-CONTROLS), so that it takes one line and hides
nothing."
  (let ((text (unmark-keys (escape-controls (apply #'format nil (substitute-command-keys control :marks t :for-format t)
                                                   arguments)))))
    (when tuatara.runtime:*secret-at-hand*
      (setf (gethash text *secret-messages*) t))
    (cond (*messages-kept*
           (push text (car *messages-kept*)))
          ((away-from-editor-p)
           (hand-to-editor-thread text *messages-from-threads*))
          (t
           (when *editor*
             (setf (editor-message *editor*) (with-command-notes-said text)))
           (tuatara.runtime:ignoring-errors-reported ("keeping a message in *Messages*") (log-message text))))
    text))

(defun echo (control &rest arguments)
  "Show a formatted text in the echo area, as MESSAGE does, without keeping it
in *Messages*: the keys of a sequence being typed, prompts. Control and
direction characters in it are written out (ESCAPE-CONTROLS), as MESSAGE
writes them."
  (setf (editor-message *editor*)
        (unmark-keys (escape-controls (apply #'format nil (substitute-command-keys control :marks t :for-format t)
                                             arguments)))))

(defun keyboard-quit-signal (&optional words)
  "Signal QUIT: abandon the current command or minibuffer read, saying WORDS,
when given, of why in place of Quit. Keys being recorded are given up
\(GIVE-UP-KEYS-RECORDING)."
  (give-up-keys-recording)
  (error 'quit :words words))

(defun quit-asking (fresh)
  "Signal QUIT for a question given up with \\`C-g': with FRESH, one that came
unasked (READ-KEY's FRESH), leaving any recording of keys going; else as
KEYBOARD-QUIT-SIGNAL does."
  (if fresh (error 'quit) (keyboard-quit-signal)))

(defvar *key-question* nil
  "True while a question in the echo area waits for one key (READ-KEY): the
cursor stands after the question, the window's point drawn as a window's
the typing does not go to.")

(defvar *minibuffer-question-keymap* nil
  "The keys of the question the minibuffer asks now, consulted before the
minibuffer's own (READ-STRING's KEYMAP); NIL when it has none.")

(defvar *minibuffer-hidden* nil
  "True while the minibuffer reads a secret (READ-PASSWORD): what is typed is
shown as stars, and the keys of the commands run meanwhile are not noted in
debug mode's log.")

(defun minibuffer-shown-text (editor)
  "The text typed in EDITOR's minibuffer as it is shown: as stars while it reads a secret."
  (let ((text (editor-minibuffer-text editor)))
    (if *minibuffer-hidden* (make-string (length text) :initial-element #\*) text)))
