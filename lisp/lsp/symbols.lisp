;;;; lisp/lsp/symbols.lisp --- Who calls a function, what it calls, and the names of a whole project.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; On a function, M-x lsp-incoming-calls lists the places that call it and
;;;; M-x lsp-outgoing-calls the functions it calls, as its language server
;;;; knows them, in *xref*. M-x lsp-find-symbol asks for a name and goes to
;;;; what of the project's functions, types and the rest the server finds by
;;;; it, wherever in the project it is. M-x buffer-outline lists the file's
;;;; functions, types and the rest as its server knows them, nested as they are.

(in-package #:tuatara.defaults)

(defparameter *symbol-kinds*
  #("file" "module" "namespace" "package" "class" "method" "property" "field" "constructor" "enum"
    "interface" "function" "variable" "constant" "string" "number" "boolean" "array" "object" "key"
    "null" "enum member" "struct" "event" "operator" "type parameter")
  "What a language server's symbol kinds, numbered from 1, are called.")

(defun symbol-kind-words (kind)
  "KIND, a symbol kind a server sent, as words."
  (if (and (integerp kind) (<= 1 kind (length *symbol-kinds*))) (aref *symbol-kinds* (1- kind)) "symbol"))

;;; Calls

(defun lsp-calls (buffer position direction)
  "The calls of the function at POSITION of BUFFER as its server knows them,
as DEFINITIONs: with DIRECTION :INCOMING the places that call it, with
:OUTGOING the functions it calls. Two values: those, and the function's name;
:NONE when no server knows calls there, NIL when it knows no function there."
  (lsp-in-step buffer)
  (multiple-value-bind (doc server) (lsp-doc-at buffer position "callHierarchyProvider" "goto-reference")
    (if (null server)
        :none
        (let ((item (find-if #'hash-table-p (tuatara.text:json-items (lsp-request-now server "textDocument/prepareCallHierarchy"
                                                                          (doc-params doc server position)))))
              (*location-files* (make-hash-table :test 'equal))
              (*location-octets-left* *lsp-location-files-total*))
          (when item
            (let ((calls (lsp-request-now server (if (eq direction :incoming) "callHierarchy/incomingCalls" "callHierarchy/outgoingCalls")
                                          (tuatara.text:json-object "item" item))))
              (values
               (remove nil
                       (loop for call across (tuatara.text:json-items calls)
                             for other = (tuatara.text:json-get call (if (eq direction :incoming) "from" "to"))
                             append (map 'list (lambda (range)
                                                 (lsp-location-definition server (tuatara.text:json-object "uri" (tuatara.text:json-text other "uri")
                                                                                                           "range" range)))
                                         (if (eq direction :incoming)
                                             ;; each place it is called from
                                             (tuatara.text:json-items call "fromRanges")
                                             (vector (tuatara.text:json-get other "selectionRange"))))))
               (or (tuatara.text:json-text item "name") "this"))))))))

(defun show-calls (direction)
  "List the calls of the function at point in DIRECTION (LSP-CALLS) in *xref*."
  (let* ((buffer (current-buffer))
         (position (point)))
    (multiple-value-bind (found name) (lsp-calls buffer position direction)
      (if (and (null found) (null name))
          (message "No function here the server knows")
          (show-server-places found (format nil "~:[calls made by~;callers of~] ~a" (eq direction :incoming) name)
                              (lambda () (lsp-calls buffer (min position (tuatara.buffer:buffer-size buffer)) direction))
                              :none "No language server here knows calls"
                              :nothing (format nil "~a ~:[calls nothing~;is called from nowhere~] the server knows of"
                                               name (eq direction :incoming)))))))

(define-command lsp-incoming-calls ()
  "List the places that call the function at point, as its language server
knows them, in *xref*: RET goes to one."
  (show-calls :incoming))

(define-command lsp-outgoing-calls ()
  "List the functions the function at point calls, as its language server
knows them, in *xref*: RET goes to one."
  (show-calls :outgoing))

;;; The file's names, for the outline

(defvar *lsp-outline* t
  "True for \\[buffer-outline] to list a file's names as its language server
knows them (functions, types, their fields and methods, nested), where it has
one that does; else as the mode finds its headings.")

(defun document-symbol-headings (doc server symbols)
  "SYMBOLS, the names SERVER gives for DOC (DocumentSymbols, nested, or
SymbolInformation, flat), as MODE-OUTLINE's headings: each (START LEVEL), START
where its name is, in the order of the text."
  (let ((headings '()))
    (labels ((walk (symbols level)
               (loop for s across symbols
                     for range = (or (tuatara.text:json-get s "selectionRange") (tuatara.text:json-get s "range")
                                     (tuatara.text:json-get s "location" "range"))
                     for at = (and range (doc-byte doc server (tuatara.text:json-get range "start")))
                     when at
                       do (push (list at level) headings)
                          (walk (tuatara.text:json-items s "children") (1+ level)))))
      (walk symbols 1))
    (stable-sort (nreverse headings) #'< :key #'first)))

(defmethod mode-outline :around (mode buffer)
  "In a buffer whose language server lists a file's names, those names, nested
as they are (*LSP-OUTLINE*); else the mode's headings."
  (multiple-value-bind (doc server) (and *lsp-outline* (buffer-docs buffer)
                                         (lsp-server-offers buffer "documentSymbolProvider" "document-symbols"))
    (let ((symbols (if server
                       (progn (lsp-in-step buffer)
                              (tuatara.text:json-items (lsp-request-now server "textDocument/documentSymbol"
                                                           (tuatara.text:json-object "textDocument" (doc-identifier doc)))))
                       #())))
      (if (plusp (length symbols))
          (let ((headings (document-symbol-headings doc server symbols)))
            (lambda (rope tree halt)
              (declare (ignore tree halt))
              ;; one a line, the outer first, as the list shows lines
              (let ((size (tuatara.text:rope-bytes rope)) (seen (make-hash-table)))
                (loop for (start level) in headings
                      for line = (and (<= start size) (tuatara.text:rope-line-start rope start))
                      when (and line (not (gethash line seen)))
                        do (setf (gethash line seen) t)
                        and collect (list start level)))))
          (call-next-method)))))

;;; The project's names

(define-command lsp-find-symbol ()
  "Go to a function, type or other name of the current buffer's project, as
its language server finds them by what you type (the name at point offered):
each is listed with what it is and where, and the one chosen is gone to."
  (let* ((buffer (current-buffer))
         (server (find-if (lambda (s) (server-offers-p s "workspaceSymbolProvider" "workspace-symbols")) (buffer-servers buffer))))
    (if (null server)
        (message "No language server here finds a project's names")
        (let* ((query (read-string "Project symbol: " :initial (or (lsp-word-at buffer (point)) "") :select-initial t))
               (symbols (remove-if-not (lambda (s) (tuatara.text:json-text s "name"))
                                       (coerce (tuatara.text:json-items (lsp-request-now server "workspace/symbol" (tuatara.text:json-object "query" query)))
                                               'list))))
          (if (null symbols)
              (message "~a finds no name like ~a" (lsp-server-name server) query)
              (let ((chosen (completing-read-value "Go to: " (mapcar #'symbol-candidate symbols) :require-match t :keep-order t)))
                (when (hash-table-p chosen)
                  (let ((definition (lsp-location-definition
                                     server (tuatara.text:json-object
                                             "uri" (tuatara.text:json-text chosen "location" "uri")
                                             ;; a name the server gives with no place in its file: the file's start
                                             "range" (or (tuatara.text:json-get chosen "location" "range")
                                                         (tuatara.text:json-object "start" (tuatara.text:json-object "line" 0 "character" 0)
                                                                                   "end" (tuatara.text:json-object "line" 0 "character" 0)))))))
                    (if definition
                        (progn (push-xref-marker) (go-to-definition definition))
                        (message "~a gives no place for ~a" (lsp-server-name server) (tuatara.text:json-text chosen "name")))))))))))

(defun symbol-candidate (symbol)
  "SYMBOL, a name of a project a server found, as a candidate to choose: its
name, with what it is and where beside it."
  (let ((path (uri-path (tuatara.text:json-get symbol "location" "uri"))))
    (make-candidate (tuatara.text:json-text symbol "name")
                    :value symbol
                    :annotation (format nil "~a~@[ in ~a~]  ~a"
                                        (symbol-kind-words (tuatara.text:json-get symbol "kind"))
                                        (tuatara.text:json-text symbol "containerName")
                                        (if path (abbreviated-path path) "")))))
