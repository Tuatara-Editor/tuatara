;;;; lisp/lsp/symbols.lisp --- Who calls a function, what it calls, and the names of a whole project.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; On a function, M-x lsp-incoming-calls lists the places that call it and
;;;; M-x lsp-outgoing-calls the functions it calls, as its language server
;;;; knows them, in *xref*. M-x lsp-find-symbol asks for a name and goes to
;;;; what of the project's functions, types and the rest the server finds by
;;;; it, wherever in the project it is. M-x buffer-outline lists the file's
;;;; functions, types and the rest as its server knows them, nested as they are.

(in-package #:tuatara.lsp)

(defparameter *symbol-kinds*
  #("file" "module" "namespace" "package" "class" "method" "property" "field" "constructor" "enum"
    "interface" "function" "variable" "constant" "string" "number" "boolean" "array" "object" "key"
    "null" "enum member" "struct" "event" "operator" "type parameter")
  "What a language server's symbol kinds, numbered from 1, are called.")

(defun symbol-kind-words (kind)
  "KIND, a symbol kind a server sent, as words."
  (if (and (integerp kind) (<= 1 kind (length *symbol-kinds*))) (aref *symbol-kinds* (1- kind)) "symbol"))

;;; Calls

(defun call-definitions (server calls direction)
  "CALLS, SERVER's answer of the calls of a function in DIRECTION, as
DEFINITIONs: with :INCOMING each place it is called from, with :OUTGOING each
function it calls."
  (location-definitions
   server
   (coerce (loop for call across (tuatara.text:json-items calls)
                 for other = (tuatara.text:json-get call (if (eq direction :incoming) "from" "to"))
                 append (map 'list (lambda (range)
                                     (tuatara.text:json-object "uri" (tuatara.text:json-text other "uri")
                                                               "range" range))
                             (if (eq direction :incoming)
                                 (tuatara.text:json-items call "fromRanges")
                                 (vector (tuatara.text:json-get other "selectionRange")))))
           'vector)))

(defun lsp-ask-calls (buffer position direction then)
  "Ask the server of BUFFER for the calls of the function at POSITION, and
call THEN once they come, while BUFFER is as it was asked of: with DIRECTION
:INCOMING the places that call it, with :OUTGOING the functions it calls. THEN
takes those as DEFINITIONs and the function's name, both NIL when the server
knows no function there. The editor goes on meanwhile. :NONE when no server
knows calls there."
  (with-lsp-server (doc server) (buffer "callHierarchyProvider" "goto-reference" :position position :else :none)
    (lsp-ask-doc server doc "textDocument/prepareCallHierarchy" (doc-params doc server position)
                 "find the function here"
                 (lambda (prepared)
                   (let ((item (find-if #'hash-table-p (tuatara.text:json-items prepared))))
                     (if (null item)
                         (funcall then nil nil)
                         (lsp-ask-doc server doc
                                      (if (eq direction :incoming)
                                          "callHierarchy/incomingCalls"
                                          "callHierarchy/outgoingCalls")
                                      (tuatara.text:json-object "item" item)
                                      "find the calls"
                                      (lambda (calls)
                                        (funcall then (call-definitions server calls direction)
                                                 (or (tuatara.text:json-text item "name") "this"))))))))))

(defun show-calls (direction)
  "List the calls of the function at point in DIRECTION (LSP-ASK-CALLS) in
*xref* once the server answers."
  (let* ((buffer (current-buffer))
         (position (point)))
    (labels ((title (name) (format nil "~:[calls made by~;callers of~] ~a" (eq direction :incoming) name))
             (again ()
               ;; the list kept until the server answers, then shown anew
               (lsp-ask-calls buffer (min position (tuatara.buffer:buffer-size buffer)) direction
                              (lambda (found name) (when found (show-definitions (title name) found :again #'again))))
               nil))
      (when (eq (lsp-ask-calls buffer position direction
                               (lambda (found name)
                                 (if (null name)
                                     (message "No function here the server knows")
                                     (show-server-places found (title name) #'again
                                                         :nothing (format nil "~a ~:[calls nothing~;is called ~
                                                                               from nowhere~] the server knows of"
                                                                          name (eq direction :incoming))))))
                :none)
        (message "No language server here knows calls")))))

(define-command lsp-incoming-calls ()
  "List the places that call the function at point, as its language server
knows them, in *xref*: RET goes to one."
  (show-calls :incoming))

(define-command lsp-outgoing-calls ()
  "List the functions the function at point calls, as its language server
knows them, in *xref*: RET goes to one."
  (show-calls :outgoing))

;;; The file's names, for the outline

(define-setting *lsp-outline* t
  "Whether \\[buffer-outline] lists a file's names as its language server knows
them (functions, types, their fields and methods, nested), where it has one
that does.
- t: it does.
- nil: it lists the headings the mode finds."
  :values boolean)

(defun document-symbol-headings (symbols byte)
  "SYMBOLS, the names a server gives for a document (DocumentSymbols, nested,
or SymbolInformation, flat), as MODE-OUTLINE's headings: each (START LEVEL),
START where its name is, by BYTE, a function of a position the server sent
giving the byte there (DOC-BYTE-FUNCTION), in the order of the text."
  (let ((headings '()))
    (labels ((walk (symbols level)
               (loop for s across symbols
                     for range = (or (tuatara.text:json-get s "selectionRange") (tuatara.text:json-get s "range")
                                     (tuatara.text:json-get s "location" "range"))
                     for at = (and range (funcall byte (tuatara.text:json-get range "start")))
                     when at
                     do (push (list at level) headings)
                     (walk (tuatara.text:json-items s "children") (1+ level)))))
      (walk symbols 1))
    (stable-sort (nreverse headings) #'< :key #'first)))

(defun line-headings (headings rope)
  "HEADINGS, each (START LEVEL) in order, one a line of ROPE, the first of a
line kept, as the list shows lines."
  (let ((size (tuatara.text:rope-octet-length rope)) (seen (make-hash-table)))
    (loop for (start level) in headings
          for line = (and (<= start size) (tuatara.text:rope-line-start rope start))
          when (and line (not (gethash line seen)))
          do (setf (gethash line seen) t)
          and collect (list start level))))

(defmethod mode-outline :around (mode buffer)
  "In a buffer whose language server lists a file's names, those names, nested
as they are (*LSP-OUTLINE*): asked of the server here, waited for in the
search's own thread; the mode's headings when it lists none. Else the mode's
headings."
  (multiple-value-bind (doc server) (and *lsp-outline* (buffer-docs buffer)
                                         (lsp-server-offers buffer "documentSymbolProvider" "document-symbols"))
    (if (null server)
        (call-next-method)
        (let ((own (call-next-method))
              (name (lsp-server-name server))
              (promise (progn (lsp-in-step buffer)
                              (lsp-promise server "textDocument/documentSymbol"
                                           (tuatara.text:json-object "textDocument" (doc-identifier doc)))))
              (byte (doc-byte-function doc server)))
          (lambda (rope tree halt)
            (multiple-value-bind (result error) (lsp-promise-wait promise :halt halt)
              (let ((symbols (and (null error) (tuatara.text:json-items result))))
                (cond (error (error "~a could not list the names: ~a" name (lsp-error-words error)))
                      ((zerop (length symbols)) (funcall own rope tree halt))
                      (t (line-headings (document-symbol-headings symbols byte) rope))))))))))

;;; The project's names

(define-command lsp-find-symbol ()
  "Go to a function, type or other name of the current buffer's project, as
its language server finds them by what you type (the name at point offered):
each is listed with what it is and where, and the one chosen is gone to."
  (let* ((buffer (current-buffer))
         (server (find-if (lambda (s) (server-offers-p s "workspaceSymbolProvider" "workspace-symbols"))
                          (buffer-servers buffer))))
    (if (null server)
        (message "No language server here finds a project's names")
        (let ((query (read-string "Project symbol: " :initial (or (lsp-word-at buffer (point)) "") :select-initial t)))
          (lsp-request server "workspace/symbol" (tuatara.text:json-object "query" query)
                       (lambda (result error)
                         (let ((symbols (remove-if-not (lambda (s) (tuatara.text:json-text s "name"))
                                                       (coerce (tuatara.text:json-items result) 'list))))
                           (cond ((lsp-failure-said server "find names" error))
                                 ((null symbols) (message "~a finds no name like ~a" (lsp-server-name server) query))
                                 ;; chosen as a command of its own, which asks
                                 (t (defer-action (lambda () (go-to-found-symbol server symbols)))))))
                       :seconds *lsp-wait*)))))

(defun go-to-found-symbol (server symbols)
  "Ask which of SYMBOLS, names of a project SERVER found, to go to, and go there."
  (let ((chosen (completing-read-value "Go to: " (mapcar #'symbol-candidate symbols) :require-match t :keep-order t)))
    (when (hash-table-p chosen)
      (let ((definition (lsp-location-definition
                         server (tuatara.text:json-object
                                 "uri" (tuatara.text:json-text chosen "location" "uri")
                                 ;; a name the server gives with no place in its file: the file's start
                                 "range" (or (tuatara.text:json-get chosen "location" "range")
                                             (let ((start (tuatara.text:json-object "line" 0 "character" 0)))
                                               (tuatara.text:json-object "start" start "end" start)))))))
        (if definition
            (progn (push-xref-marker) (go-to-definition definition))
            (message "~a gives no place for ~a" (lsp-server-name server) (tuatara.text:json-text chosen "name")))))))

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
