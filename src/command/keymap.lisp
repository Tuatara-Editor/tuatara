;;;; src/command/keymap.lisp --- Keymaps with parents and prefix keys.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defstruct (keymap (:constructor %make-keymap (name up)) (:copier nil))
  "A keymap: bindings from key specs to commands (symbols) or nested keymaps
for prefix keys, with an optional parent, UP, consulted for keys not bound
here (KEYMAP-PARENT)."
  (name nil :read-only t)
  (up nil :type (or null keymap))
  (table (make-hash-table :test 'equal) :type hash-table :read-only t))

(setf (documentation 'keymap-p 'function) "True when OBJECT is a KEYMAP."
      (documentation 'keymap-name 'function) "The name of KEYMAP, for help."
      (documentation 'keymap-up 'function) "KEYMAP's parent, as KEYMAP-PARENT gives it."
      (documentation 'keymap-table 'function) "The hash table of KEYMAP's own bindings.")

(defconstant +key-open+ (code-char #xFDD0)
  "The mark before a key written into a text (a Unicode noncharacter, found in
no real text).")

(defconstant +key-close+ (code-char #xFDD1)
  "The mark after a key written into a text.")

(defun key-mark-p (char)
  "True for the marks round a key written into a text (+KEY-OPEN+, +KEY-CLOSE+)."
  (or (char= char +key-open+) (char= char +key-close+)))

(defvar *keymaps-generation* 0
  "A count that goes up each time a key is bound or a parent given, so that what is worked out from the keymaps (WHERE-IS,
SUBSTITUTE-COMMAND-KEYS) is kept until they change.")

(defvar *keymaps-lock* (sb-thread:make-mutex :name "keymaps")
  "Held while a keymap's bindings change or are walked (MAP-KEYMAP-BINDINGS),
so that work on another thread reads them whole.")

(defmacro with-keymaps-held (&body body)
  "Run BODY holding *KEYMAPS-LOCK*: no binding changes meanwhile."
  `(sb-thread:with-recursive-lock (*keymaps-lock*) ,@body))

(declaim (inline keymap-parent))
(defun keymap-parent (keymap)
  "The keymap consulted when a key is not bound in KEYMAP."
  (keymap-up keymap))

(defun (setf keymap-parent) (parent keymap)
  "Make PARENT the keymap consulted when a key is not bound in KEYMAP."
  (with-keymaps-held
    (incf *keymaps-generation*)
    (setf (keymap-up keymap) parent)))

(defun make-keymap (&key name parent)
  "Return an empty keymap called NAME whose unbound keys fall through to PARENT."
  (%make-keymap name parent))

(defvar *key-labels* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The labels keys are given (SET-KEY-LABEL), by keymap: for each, a table of
key spec to the label the key hints show for that key.")

(defun set-key-label (keymap keys label)
  "Give KEYS, a key sequence in KEYMAP, the LABEL (a string) the key hints
show for it instead of the name of the command it runs, or, for a prefix key,
of its keymap. NIL takes the label away. A prefix before the last key is made
when KEYS has none yet; a key bound to a command is no prefix, which is an
error."
  (let ((specs (if (stringp keys) (tuatara.frontend:parse-key-sequence keys) keys)))
    (when (null specs) (error "Empty key sequence."))
    (with-keymaps-held
     (loop for (spec . rest) on specs
          do (if rest
                 (let ((next (gethash spec (keymap-table keymap))))
                   (cond ((keymap-p next))
                         (next (error "~a is bound to ~(~a~), not a prefix key."
                                      (tuatara.frontend:key-description spec) next))
                         (t (incf *keymaps-generation*)
                            (setf next (make-keymap :name (tuatara.frontend:key-description spec))
                                  (gethash spec (keymap-table keymap)) next)))
                   (setf keymap next))
                 (let ((table (or (gethash keymap *key-labels*)
                                  (setf (gethash keymap *key-labels*) (make-hash-table :test 'equal)))))
                   (if label (setf (gethash spec table) label) (remhash spec table))))))
    label))

(defun key-label (keymap spec)
  "The label of the key SPEC in KEYMAP (SET-KEY-LABEL): the one given where
SPEC is bound, KEYMAP or the first of its parents that binds it, a binding
shadowing its parents' labels, but for a prefix key with none of its own,
which takes the label of the same prefix in the parents; else KEYMAP's own;
NIL when there is none."
  (flet ((label-in (map) (let ((table (gethash map *key-labels*))) (and table (values (gethash spec table))))))
    (loop for map = keymap then (keymap-parent map)
          while map
          do (multiple-value-bind (binding found) (gethash spec (keymap-table map))
               (when found
                 (let ((label (label-in map)))
                   (when (or label (not (keymap-p binding)))
                     (return label)))))
          finally (return (label-in keymap)))))

(defun define-key (keymap keys command)
  "Bind KEYS, a key sequence as a string (\"C-x C-f\") or a list of key specs,
to COMMAND (a command symbol, a keymap, NIL to unbind, or :UNDEFINED to take
the key away from the keymaps consulted after KEYMAP too, as a major mode takes
the modal keys away in its buffers) in KEYMAP. Prefix keymaps are created as
needed."
  (let ((specs (if (stringp keys) (tuatara.frontend:parse-key-sequence keys) keys)))
    (when (null specs) (error "Empty key sequence."))
    (with-keymaps-held
     (incf *keymaps-generation*)
     (loop for (spec . rest) on specs
          do (if (null rest)
                 (if command
                     (setf (gethash spec (keymap-table keymap)) command)
                     (progn (remhash spec (keymap-table keymap))
                            ;; an unbound key keeps no label
                            (let ((labels (gethash keymap *key-labels*)))
                              (when labels (remhash spec labels)))))
                 (let ((next (gethash spec (keymap-table keymap))))
                   (unless (keymap-p next)
                     (setf next (make-keymap :name (tuatara.frontend:key-description spec))
                           (gethash spec (keymap-table keymap)) next))
                   (setf keymap next)))))
    command))

(defun define-keys (keymap bindings)
  "Bind in KEYMAP each of BINDINGS, a list of (KEYS COMMAND), as DEFINE-KEY
does. Returns KEYMAP."
  (loop for (keys command) in bindings do (define-key keymap keys command))
  keymap)

(defun unbind-commands-if (test keymap)
  "Take out of KEYMAP, and of the keymaps of its prefix keys, each binding to
a command (a symbol) for which TEST is true."
  (with-keymaps-held
    (incf *keymaps-generation*)
    (labels ((walk (map)
               (loop for spec being the hash-keys of (keymap-table map) using (hash-value binding)
                     do (cond ((keymap-p binding) (walk binding))
                              ((and binding (symbolp binding) (not (keywordp binding)) (funcall test binding))
                               (remhash spec (keymap-table map)))))))
      (walk keymap))))

(defun label-keys (keymap labels)
  "Give in KEYMAP each of LABELS, a list of (KEYS LABEL), its label, as
SET-KEY-LABEL does. Returns KEYMAP."
  (loop for (keys label) in labels do (set-key-label keymap keys label))
  keymap)

(defun lookup-key (keymap specs &key (parents t))
  "Look up the key specs SPECS in KEYMAP and its parents. Returns a command
symbol, a keymap when SPECS is a prefix, :UNDEFINED where the key was taken
away (DEFINE-KEY), or NIL when unbound. A binding in
KEYMAP shadows the same key in its parents, including prefix keymaps. With
PARENTS NIL, only KEYMAP's own bindings count."
  (labels ((look (map specs)
             (when map
               (let ((binding (gethash (first specs) (keymap-table map))))
                 (cond ((null binding) (and parents (look (keymap-parent map) specs)))
                       ((null (rest specs)) binding)
                       ((keymap-p binding) (or (look binding (rest specs))
                                               ;; a prefix here but the full sequence unbound: try the parent chain
                                               (and parents (look (keymap-parent map) specs))))
                       (t nil))))))
    (look keymap specs)))

(defun keymaps-lookup (keymaps specs)
  "The binding of the key specs SPECS in KEYMAPS, a list consulted in order as
typed keys are: a command, a keymap (SPECS is a prefix), or NIL when unbound,
a key taken away (:UNDEFINED) in one keymap being unbound there and in the
keymaps after it."
  (dolist (map keymaps nil)
    (let ((binding (lookup-key map specs)))
      (cond ((eq binding :undefined) (return nil))
            (binding (return binding))))))

(defun map-keymap-bindings (function keymaps)
  "Call FUNCTION with each key sequence bound to a command in KEYMAPS, a list
consulted in order, and the command it runs: the sequence as a list of key
specs, keys reached through prefix keymaps and parents included. A sequence
is given once, with the command typing it runs (KEYMAPS-LOOKUP): a binding
shadowed by an earlier keymap, or by the keymap's own over its parent's, is
left out, and so is a key taken away. The bindings do not change meanwhile.
Complexity: O(bindings × depth of the keymaps)."
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((walk (map path)
               (loop for map* = map then (keymap-parent map*)
                     while map*
                     do (loop for spec being the hash-keys of (keymap-table map*) using (hash-value binding)
                              do (let ((keys (append path (list spec))))
                                   (if (keymap-p binding)
                                       (walk binding keys)
                                       (unless (gethash keys seen)
                                         (setf (gethash keys seen) t)
                                         (let ((runs (keymaps-lookup keymaps keys)))
                                           (when (and runs (not (keymap-p runs)))
                                             (funcall function keys runs))))))))))
      (with-keymaps-held
        (dolist (map keymaps) (walk map '()))))))

(defun keymap-bindings (keymap)
  "A list of (KEY-SPEC . BINDING) for KEYMAP's own bindings, for help."
  (loop for spec being the hash-keys of (keymap-table keymap) using (hash-value binding)
        collect (cons spec binding)))
