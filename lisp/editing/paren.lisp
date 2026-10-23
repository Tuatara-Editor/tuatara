;;;; lisp/editing/paren.lisp --- The bracket that pairs with the one at point, shown: show-paren-mode.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's show-paren-mode, on as standard: when the character before
;;;; point closes a bracket, or else the one after it opens one, that bracket
;;;; and its partner are shown in :paren-match; a partner of another kind, or
;;;; none before the text ends, in :paren-mismatch. Where the language has a
;;;; grammar, brackets in strings, comments and character literals count for
;;;; nothing: the tree says where the partner is, however far when the two
;;;; brackets begin and end a node of it, else the brackets are counted within
;;;; that node. Without a grammar they are counted within *show-paren-reach*
;;;; octets either side. What was found is kept until point or the text
;;;; changes. Not in the lists and help buffers, as Emacs leaves out
;;;; special-mode's.

(in-package #:tuatara.defaults)

(defvar *show-paren-mode* t
  "True to show the bracket that pairs with the one at point: the one before
point when it closes, else the one after it when it opens. T unless set;
\\[show-paren-mode] switches it.")

(defparameter *show-paren-reach* (* 64 1024)
  "The most octets either side of a bracket looked through for its partner;
a partner further away is not shown.")

(defun counted-partner (rope spans at forward from to)
  "The partner of the bracket at octet AT of ROPE found by counting brackets
from octet FROM below TO, FORWARD or back (ROPE-BRACKET-MATCH): those in SPANS
\(strings and comments, a simple-vector of ranges in order) and those escaped
by a backslash count for nothing, brackets of any kind count in between. Two
values: the partner's octet, or NIL when the brackets do not come back to AT's
level there; and true when the partner is of AT's kind, ( with ), [ with ], {
with }."
  (multiple-value-bind (found same)
      (tuatara.text:rope-bracket-match rope (if forward at (1+ at)) :backward (not forward) :start from :end to
                                                                   :spans spans :escapes t)
    (if found
        (values (if forward (1- found) found) same)
        (values nil nil))))

(defun bracket-token-parent (syntax position)
  "The bracket token at octet POSITION of SYNTAX's text and the node it is a
child of, as two values; NIL when no bracket token is there."
  (let* ((chain (tuatara.syntax:node-ancestry syntax position (1+ position)))
         (leaf (first chain)))
    (when (and leaf
               (= (tuatara.syntax:tree-node-start leaf) position) (= (tuatara.syntax:tree-node-end leaf) (1+ position))
               (or (tuatara.syntax:opening-bracket-node-p leaf) (tuatara.syntax:closing-bracket-node-p leaf)))
      (values leaf (second chain)))))

(defun same-node-p (a b)
  "True when nodes A and B are the same node: the same octets."
  (and a b
       (= (tuatara.syntax:tree-node-start a) (tuatara.syntax:tree-node-start b))
       (= (tuatara.syntax:tree-node-end a) (tuatara.syntax:tree-node-end b))))

(defun counted-within (syntax at low high forward)
  "The partner of the bracket at octet AT found by counting brackets towards
the end of the node around it, LOW or HIGH, FORWARD or back, SYNTAX's
strings and comments not counting; at most *SHOW-PAREN-REACH* octets away,
looked through a few kilobytes first. Two values as COUNTED-PARTNER gives
them; :NONE when there is none within reach."
  (let ((rope (tuatara.syntax:syntax-rope syntax)))
    (dolist (reach (list 4096 *show-paren-reach*) :none)
      (let ((from (if forward at (max low (- at reach))))
            (to (if forward (min high (+ at 1 reach)) (1+ at))))
        (multiple-value-bind (partner same)
            (counted-partner rope (coerce (tuatara.syntax:opaque-spans syntax from to) 'simple-vector) at forward from to)
          (cond (partner (return (values partner same)))
                ;; the whole node looked through: none there
                ((if forward (= to high) (= from low)) (return :none))))))))

(defun tree-bracket-partner (syntax at)
  "The partner of the bracket at octet AT as SYNTAX's tree has it. When the
bracket's token and its partner's begin and end the node around them, the
tree says it at once, however far; else the brackets are counted within that
node (COUNTED-WITHIN), as in C's for (...) and a[i]. Two values as
COUNTED-PARTNER gives them; or :TEXT when AT is in a
string, comment or character literal; :NONE when the node around it has no
partner for it (a shell case pattern's parenthesis); or :UNKNOWN when the tree
cannot tell: no bracket token at AT, or an error in the node around it."
  (let* ((chain (tuatara.syntax:node-ancestry syntax at (1+ at)))
         (leaf (first chain))
         (parent (second chain))
         (rope (tuatara.syntax:syntax-rope syntax)))
    (flet ((octet (i) (tuatara.text:rope-octet rope i))
           (kind-of (i) (or (position (tuatara.text:rope-octet rope i) tuatara.text:*opening-octets*)
                            (position (tuatara.text:rope-octet rope i) tuatara.text:*closing-octets*))))
      (cond ((null leaf) :unknown)
            ((some (lambda (node) (or (tuatara.syntax:string-node-p node) (tuatara.syntax:comment-node-p node))) chain)
             :text)
            ;; inside a token that is no bracket, a name holding \) say: only text
            ((not (or (tuatara.syntax:opening-bracket-node-p leaf) (tuatara.syntax:closing-bracket-node-p leaf)))
             (if (tuatara.syntax:tree-node-broken leaf) :unknown :text))
            ((or (/= (tuatara.syntax:tree-node-start leaf) at) (/= (tuatara.syntax:tree-node-end leaf) (1+ at))
                 (null parent) (tuatara.syntax:tree-node-broken parent))
             :unknown)
            ((tuatara.syntax:opening-bracket-node-p leaf)
             (let ((close (1- (tuatara.syntax:tree-node-end parent))))
               (if (and (= at (tuatara.syntax:tree-node-start parent)) (> close at)
                        (multiple-value-bind (token up) (bracket-token-parent syntax close)
                          (and token (tuatara.syntax:closing-bracket-node-p token) (same-node-p up parent))))
                   (values close (= (kind-of close) (kind-of at)))
                   (counted-within syntax at (tuatara.syntax:tree-node-start parent) (tuatara.syntax:tree-node-end parent) t))))
            ((tuatara.syntax:closing-bracket-node-p leaf)
             ;; the node may begin with marks before its bracket (#( in Lisp)
             (let ((open (loop for i from (tuatara.syntax:tree-node-start parent)
                                 below (min at (+ 4 (tuatara.syntax:tree-node-start parent)))
                               when (member (octet i) tuatara.text:*opening-octets*) return i)))
               (if (and open (= at (1- (tuatara.syntax:tree-node-end parent)))
                        (multiple-value-bind (token up) (bracket-token-parent syntax open)
                          (and token (tuatara.syntax:opening-bracket-node-p token) (same-node-p up parent))))
                   (values open (= (kind-of open) (kind-of at)))
                   (counted-within syntax at (tuatara.syntax:tree-node-start parent) (tuatara.syntax:tree-node-end parent) nil))))
            (t :unknown)))))

(defun counted-bracket-partner (buffer syntax at forward)
  "The partner of the bracket at octet AT of BUFFER found by counting brackets
within *SHOW-PAREN-REACH* octets of it, FORWARD or back: in SYNTAX's strings
and comments, when SYNTAX is given, they count for nothing (asked of the tree
for the side looked through only). Two values: the partner's octet and whether
it is of AT's kind; or NIL and :END when the text ended first, NIL and NIL
when the reach did."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (from (if forward at (max 0 (- at *show-paren-reach*))))
         (to (if forward (min size (+ at 1 *show-paren-reach*)) (1+ at)))
         (spans (if syntax (coerce (tuatara.syntax:opaque-spans syntax from to) 'simple-vector) #())))
    (multiple-value-bind (partner same) (counted-partner rope spans at forward from to)
      (cond (partner (values partner same))
            ((if forward (= to size) (zerop from)) (values nil :end))
            (t (values nil nil))))))

(defun paren-ranges (buffer point)
  "The ranges (START END FACE) that show the bracket at POINT of BUFFER and
its partner (see *SHOW-PAREN-MODE*): the bracket before POINT when it closes,
else the one after it when it opens; NIL when neither is a bracket, when it is
in a string or comment, or when its partner cannot be found near enough. Where
BUFFER's language has a grammar the tree says where the partner is
(TREE-BRACKET-PARTNER); else, and where the tree has an error around the
bracket, the brackets are counted within *SHOW-PAREN-REACH* octets."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (before (and (plusp point) (tuatara.text:rope-octet rope (1- point))))
         (after (and (< point size) (tuatara.text:rope-octet rope point)))
         (at (cond ((and (member before tuatara.text:*closing-octets*) (not (tuatara.text:rope-escaped-p rope (1- point)))) (1- point))
                   ((and (member after tuatara.text:*opening-octets*) (not (tuatara.text:rope-escaped-p rope point))) point))))
    (when at
      (let* ((syntax (ready-syntax buffer))
             (answer (if syntax (multiple-value-list (tree-bracket-partner syntax at)) '(:unknown))))
        (destructuring-bind (partner &optional same)
            (if (eq (first answer) :unknown)
                (multiple-value-list (counted-bracket-partner buffer syntax at (= at point)))
                answer)
          (cond ((member partner '(:text :none)) nil)
                ((integerp partner)
                 (let ((face (if same :paren-match :paren-mismatch)))
                   (list (list at (1+ at) face) (list partner (1+ partner) face))))
                ;; the text ends before the brackets come back: there is no partner
                ((eq same :end) (list (list at (1+ at) :paren-mismatch)))))))))

(defvar *paren-shown* nil
  "What PAREN-HIGHLIGHTS found last, kept until point or the text changes: the
buffer, its version, point and the ranges, as a list. One variable, so that
redisplay while an evaluation runs writes no table the evaluation may write.")

(defun paren-highlights (editor window rows)
  "Highlight function: in the selected window, the bracket at point and its
partner (PAREN-RANGES), while *SHOW-PAREN-MODE* is on; not in the hex view or
a special-mode buffer (lists, help). What was found is kept (*PAREN-SHOWN*)
until point or the text changes."
  (declare (ignore rows))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and *show-paren-mode*
               (eq window (editor-window editor))
               (not (tuatara.display:hex-view-p buffer))
               (not (typep (buffer-major-mode buffer) 'special-mode)))
      (let ((version (tuatara.buffer:buffer-version buffer))
            (point (tuatara.buffer:buffer-point buffer))
            (kept *paren-shown*))
        (if (and kept (eq (first kept) buffer) (eq (second kept) version) (= (third kept) point))
            (fourth kept)
            (let ((ranges (paren-ranges buffer point)))
              (setf *paren-shown* (list buffer version point ranges))
              ranges))))))

(define-command show-paren-mode ()
  "Switch on or off showing the bracket that pairs with the one at point, in
every buffer: when on, the bracket before point when it closes, else the one
after it when it opens, is shown with its partner in :paren-match, or in
:paren-mismatch when the partner is of another kind or there is none. With a
count, on when it is positive, off when it is zero or less."
  (setf *show-paren-mode* (switched-by-count *show-paren-mode*))
  (message "Show-Paren mode ~:[disabled~;enabled~]" *show-paren-mode*))

(defvar *blink-matching-paren* t
  "True to say, when a closing bracket is typed, what its partner is when it
is out of sight (\"Matches (defun foo ()\"), or that it has none.")

(defun open-bracket-line-text (buffer position)
  "What shows the opening bracket at POSITION of BUFFER: its line up to it
when text comes before it there, else its line from it when text comes after
it, else the line before with text, \"...\" and the bracket, else the bracket
alone."
  (let* ((line (tuatara.buffer:buffer-line-start buffer position))
         (end (tuatara.buffer:buffer-line-end buffer position))
         (next (tuatara.buffer:buffer-forward-char buffer position))
         (blank (lambda (text) (every (lambda (c) (member c '(#\Space #\Tab #\Return))) text))))
    (cond ((not (funcall blank (tuatara.buffer:buffer-string buffer :start line :end position)))
           (tuatara.buffer:buffer-string buffer :start line :end next))
          ((not (funcall blank (tuatara.buffer:buffer-string buffer :start next :end end)))
           (tuatara.buffer:buffer-string buffer :start position :end end))
          (t
           (let ((before (loop for l = line then (tuatara.buffer:buffer-line-start buffer (1- l))
                               while (plusp l)
                               do (let* ((prev (tuatara.buffer:buffer-line-start buffer (1- l)))
                                         (text (tuatara.buffer:buffer-string buffer :start prev
                                                                                    :end (tuatara.buffer:buffer-line-end buffer prev))))
                                    (unless (funcall blank text)
                                      (return (string-right-trim '(#\Space #\Tab #\Return) text)))))))
             (if before
                 (concatenate 'string before "..." (tuatara.buffer:buffer-string buffer :start position :end next))
                 (tuatara.buffer:buffer-string buffer :start position :end next)))))))

(defun blink-matching-open (buffer char)
  "After CHAR was typed into BUFFER: when it is a closing bracket that counts,
say what its partner is if that is out of sight above the window, or that it
has none or one of another kind."
  (let ((point (tuatara.buffer:buffer-point buffer)))
    (when (and *blink-matching-paren* (find char tuatara.text:*closers*) (plusp point)
               (eql (tuatara.buffer:buffer-char-at buffer (1- point)) char)
               (not (tuatara.text:rope-escaped-p (tuatara.buffer:buffer-text buffer) (1- point)))
               (not (tuatara.display:hex-view-p buffer)))
      (let* ((at (1- point))
             (syntax (ready-syntax buffer))
             (answer (if syntax (multiple-value-list (tree-bracket-partner syntax at)) '(:unknown))))
        (destructuring-bind (partner &optional same)
            (if (eq (first answer) :unknown)
                (multiple-value-list (counted-bracket-partner buffer syntax at nil))
                answer)
          ;; a passing note on the key just typed: shown, not kept in *Messages*
          (cond ((member partner '(:text :none)))
                ((integerp partner)
                 (cond ((not same) (echo "Mismatched parentheses"))
                       ((< partner (tuatara.display:window-start (current-window)))
                        (echo "Matches ~a" (open-bracket-line-text buffer partner)))))
                ((eq same :end) (echo "No matching parenthesis found"))))))))

(defun install-show-paren ()
  "Show the bracket that pairs with the one at point (*SHOW-PAREN-MODE*), and
say what a closing bracket typed pairs with (*BLINK-MATCHING-PAREN*)."
  (pushnew 'paren-highlights *highlight-functions*)
  (pushnew 'blink-matching-open *post-self-insert-functions*))
