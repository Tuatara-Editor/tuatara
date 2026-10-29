;;;; lisp/selecting/digraphs.lisp --- Digraphs: two keys for a character the keyboard lacks, as Vim's and RFC 1345's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defparameter *common-digraphs*
  '(("'6" . "‘") ("'9" . "’") ("\"6" . "“") ("\"9" . "”") ("<<" . "«") (">>" . "»")
    ("-N" . "–") ("-M" . "—") ("sb" . "•") ("e'" . "é") ("Pd" . "£") ("Rg" . "®") ("1'" . "′") ("Eu" . "€"))
  "The digraphs the chart shows first: the characters English text uses most
that a keyboard lacks, quotation marks in pairs, and the guillemets, which
\\<normal>\\[select-inside-pair], \\[select-pair] and the wrap keys know as
a pair.")

(defun digraph-rule (keys)
  "The digraph KEYS as a rule of the rfc1345 input method: an ampersand before them."
  (concatenate 'string "&" keys))

(defun digraph-char (keys)
  "The character the digraph KEYS stands for, or NIL: the rfc1345 input
method's rule for them."
  (let ((text (gethash (digraph-rule keys) (input-method-rules (find-input-method "rfc1345")))))
    (and text (char text 0))))

(defun digraph-prefix-p (keys)
  "True when a longer digraph goes on from KEYS."
  (gethash (digraph-rule keys) (input-method-prefixes (find-input-method "rfc1345"))))

(defun read-digraph ()
  "Ask for a digraph's keys one at a time; two values: the keys, and the
character they make or NIL. Another key is asked for while the keys so far
start a longer digraph; \\`RET' takes a shorter one that is whole, and so does
a key that goes on with none. \\`C-g' gives up."
  (let ((keys ""))
    (loop
      (let* ((whole (and (plusp (length keys)) (digraph-char keys)))
             (key (read-key (if whole
                                (format nil (substitute-command-keys "Digraph: ~a (~a), \\`RET' or more: "
                                                                     :marks t :for-format t)
                                        keys whole)
                                (format nil "Digraph: ~a" keys))))
             (base (tuatara.frontend:key-base key))
             (plain (and (characterp base) (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key)))))
        (cond ((quit-key-p key) (keyboard-quit-signal))
              ((eq base :enter) (return (values keys whole)))
              ((not plain) (return (values keys nil)))
              (t (let ((next (concatenate 'string keys (string base))))
                   (cond ((digraph-prefix-p next) (setf keys next))
                         ((digraph-char next) (return (values next (digraph-char next))))
                         (whole (return (values keys whole)))
                         (t (return (values next nil)))))))))))

(define-command insert-digraph ()
  "Type the character a digraph stands for, the count's times: asks for its
keys, e' for e with an acute, -> for an arrow, << for a guillemet;
\\[list-digraphs] lists them all. With a selection, the character wraps it, with
its partner when it has one (<< wraps in guillemets, \"6 in curly quotes), or
takes the pair off. The state does not change, so it types from NORMAL state
too."
  (let ((b (current-buffer)))
    (check-writable b)
    (multiple-value-bind (keys char) (read-digraph)
      (cond ((null char) (message "No digraph ~a" keys))
            ((multiple-value-bind (start end) (selection-range) (and start (< start end)))
             (wrap-selection char)
             (message "~a: ~a" keys char))
            (t (goto (tuatara.buffer:buffer-insert b (point) (make-string (max 1 (prefix)) :initial-element char)))
               (message "~a: ~a" keys char))))))

;;; The chart

(defun char-script-group (char)
  "The heading CHAR comes under in the chart of every digraph."
  (let ((code (char-code char)))
    (cond ((or (<= code #x24F) (<= #x1E00 code #x1EFF) (<= #x250 code #x2AF)) "Latin")
          ((or (<= #x370 code #x3FF) (<= #x1F00 code #x1FFF)) "Greek")
          ((<= #x400 code #x52F) "Cyrillic")
          ((<= #x590 code #x5FF) "Hebrew")
          ((<= #x600 code #x6FF) "Arabic")
          ((<= #x3040 code #x30FF) "Kana")
          ((<= #x3100 code #x312F) "Bopomofo")
          ((or (<= #x4E00 code #x9FFF) (<= #x3400 code #x4DBF)) "Han")
          ((or (<= #x3000 code #x303F) (<= #xFF00 code #xFFEF)) "CJK, other")
          ((<= #xE000 code #xF8FF) "Private use")
          (t "Symbols"))))

(defparameter *digraph-groups*
  '("Latin" "Greek" "Cyrillic" "Hebrew" "Arabic" "Symbols" "Kana" "Bopomofo" "Han" "CJK, other" "Private use")
  "The headings of the chart of every digraph, in order.")

(defun unicode-name (char)
  "CHAR's Unicode name, as the standard writes it (LATIN SMALL LETTER E WITH
ACUTE), or an empty string for one that has none: a control character, or a
code with no character given it yet."
  (let ((name (char-name char)))
    (if (and name (> (length name) 1)
             (not (eq (ignore-errors (sb-unicode:general-category char)) :cc))
             (not (cl-ppcre:scan "^U[0-9A-F]+$" name)))
        (substitute #\Space #\_ (string-upcase name))
        "")))

(define-command list-digraphs ()
  "Show every digraph: the keys that type a character a keyboard lacks,
those English text wants most first, then all of them by script, with
the code point and name of each, and how to type them."
  (let ((rule (make-string 50 :initial-element #\-)))
    (show-help
     (with-output-to-string (out)
       (format out "Digraphs~%~a~%~%" (make-string 50 :initial-element #\=))
       (format out "A digraph is two characters that stand for one you cannot type directly.~%~
These are the codes of RFC 1345, a standard list, each typed with an~%~
ampersand in front of it while digraphs are on.~%~%")
       (format out "  Common digraphs~%~a~%" rule)
       (format out (substitute-command-keys
                    "  The characters English text uses most that a keyboard lacks, quotation~%~
  marks in pairs, and the guillemets, which \\<normal>\\[select-inside-pair], \\[select-pair] and a wrap know as a pair.~%~%"
                    :marks t :for-format t))
       (format out "  ~12a~22a~a~%" "DIGRAPH" "TYPE" "RESULT")
       (loop for (keys . result) in *common-digraphs*
             do (format out "  ~12a~22a~a~%" keys (concatenate 'string "&" keys) result))
       (format out "~%  How to type one~%~a~%" rule)
       (format out (substitute-command-keys
                    "  1. Press \\[digraph-input-mode] (or \\[set-input-method] and rfc1345): digraphs are on~%~
     where you type text, from the next INSERT state in NORMAL state.~%~
  2. Type an ampersand and the digraph: &e' gives é, &e shown underlined until the '.~%~
  \\[toggle-input-method] turns them off and on again, \\[input-method-off] off.~%~%~
  For just one character, \\[insert-digraph] asks for the keys and types it, in either state;~%~
  with a selection, it wraps it. Or copy the character from the table below.~%~%"
                    :marks t :for-format t))
       (let ((groups (make-hash-table :test 'equal)))
         (loop for (keys . code) in *digraph-table*
               do (push (cons keys (code-char code)) (gethash (char-script-group (code-char code)) groups)))
         (format out "  All ~d digraphs, by script~%~a~%" (length *digraph-table*) rule)
         (format out "  ~11a~8a~11a~a~%" "DIGRAPH" "RESULT" "CODE" "NAME")
         (dolist (group *digraph-groups*)
           (let ((rows (reverse (gethash group groups))))
             (when rows
               (format out "~%  ~a~%" group)
               (loop for (keys . char) in rows
                     do (let ((shown (if (graphic-char-p char) (string char) " ")))
                          ;; padded by the columns the character takes, two for a wide one
                          (format out "  ~11a~a~a~11a~a~%" keys shown
                                  (make-string (max 1 (- 8 (tuatara.display:char-columns (char shown 0))))
                                               :initial-element #\Space)
                                  (format nil "U+~4,'0X" (char-code char))
                                  (unicode-name char))))))))
       (format out (substitute-command-keys "~%~a~%\\<help-mode>\\[quit-window]: quit  |  \\[regex-search-forward]: search"
                                            :marks t :for-format t)
               (make-string 50 :initial-element #\=))))))
